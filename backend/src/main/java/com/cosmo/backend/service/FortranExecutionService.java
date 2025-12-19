package com.cosmo.backend.service;

import com.cosmo.backend.dto.InitialConditionsDTO;
import org.springframework.stereotype.Service;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.*;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.TimeUnit;
import java.util.regex.Pattern;
import java.util.regex.Matcher;

/**
 * Service to execute Fortran program with initial conditions
 * 
 * WORKFLOW:
 * 1. Frontend sends potential expression and initial conditions
 * 2. Backend transforms potential to Fortran 77 format
 * 3. Backend writes potential.inc and initial_conditions.inc files
 * 4. Backend compiles multifix.f
 * 5. Backend runs the compiled executable
 * 6. Backend returns results
 */
@Service
public class FortranExecutionService {
    
    private static final Logger logger = LoggerFactory.getLogger(FortranExecutionService.class);
    
    /**
     * Maximum execution time in seconds (10 minutes)
     */
    private static final long MAX_EXECUTION_TIME = 600;
    
    /**
     * Map to track running processes by execution ID
     * Used for cancellation support
     */
    private final Map<String, Process> runningProcesses = new ConcurrentHashMap<>();
    
    /**
     * Get the absolute path to the fortran directory
     * This is where multifix.f, potential.inc, and initial_conditions.inc are located
     */
    private Path getFortranDirectory() {
        // Get current working directory (usually backend/ when running Spring Boot)
        Path currentDir = Paths.get("").toAbsolutePath();
        logger.info("Current working directory: {}", currentDir);
        
        // Try multiple possible locations for the fortran directory
        List<Path> possiblePaths = new ArrayList<>();
        
        // 1. Relative to current dir: ../fortran (if running from backend/)
        possiblePaths.add(currentDir.resolve("../fortran").normalize());
        
        // 2. Relative to current dir: ./fortran (if fortran is in backend/)
        possiblePaths.add(currentDir.resolve("fortran"));
        
        // 3. Absolute path from project root (if we can detect it)
        // If current dir ends with "backend", go up one level
        if (currentDir.toString().endsWith("backend")) {
            possiblePaths.add(currentDir.getParent().resolve("fortran"));
        }
        
        // Find the first path that exists
        for (Path path : possiblePaths) {
            logger.info("Checking fortran directory path: {}", path);
            if (Files.exists(path) && Files.isDirectory(path)) {
                logger.info("Found fortran directory at: {}", path);
                return path;
            }
        }
        
        // Return the first path even if it doesn't exist (for error message)
        logger.warn("Fortran directory not found, using: {}", possiblePaths.get(0));
        return possiblePaths.get(0);
    }
    
    /**
     * Replace parameter symbols in expression with their values
     * Handles case-insensitive matching (e.g., "A" in map matches "a" in expression)
     * 
     * @param expression The potential expression with parameter symbols
     * @param parameterValues Map of parameter names to values
     * @return Expression with parameters replaced by values
     */
    private String replaceParameters(String expression, Map<String, Double> parameterValues) {
        if (expression == null || expression.trim().isEmpty()) {
            return expression;
        }
        
        if (parameterValues == null || parameterValues.isEmpty()) {
            return expression;
        }
        
        String result = expression;
        
        // Replace each parameter with its value
        // Sort by length (longest first) to avoid partial replacements (e.g., "lambda" before "lam")
        List<String> sortedParams = new ArrayList<>(parameterValues.keySet());
        sortedParams.sort((a, b) -> Integer.compare(b.length(), a.length()));
        
        for (String paramName : sortedParams) {
            Double value = parameterValues.get(paramName);
            if (value != null) {
                // Use word boundaries to match whole parameter names only
                // Case-insensitive replacement - matches "A", "a", "Aa", etc.
                String pattern = "\\b" + Pattern.quote(paramName) + "\\b";
                String replacement = String.format("%.15f", value);
                result = result.replaceAll("(?i)" + pattern, replacement);
                logger.info("Replaced parameter '{}' (case-insensitive) with value {}", paramName, value);
            }
        }
        
        logger.info("Expression after parameter replacement: {}", result);
        return result;
    }
    
    /**
     * Transform a potential expression from frontend format to Fortran 77 format
     * 
     * TRANSFORMATIONS:
     * 1. Add .d0 suffix to all numeric literals (e.g., 0.1 -> 0.1d0, 6 -> 6.d0)
     * 2. Break long lines with continuation characters (Fortran 77 format)
     * 3. Ensure proper Fortran 77 formatting with 6-space indentation
     * 
     * @param expression The potential expression from frontend (e.g., "(0.1*Tanh(x(1)/Sqrt(6)))**2")
     * @return Fortran 77 formatted expression
     */
    private String transformPotentialToFortran77(String expression) {
        if (expression == null || expression.trim().isEmpty()) {
            // Default potential if none provided
            return "      VV=(0.1d0*Tanh(x(1)/Sqrt(6.d0)))**2.d0";
        }
        
        logger.info("Transforming potential expression to Fortran 77 format...");
        logger.debug("Original expression: {}", expression);
        
        // Step 1: Add .d0 suffix to numeric literals
        // IMPORTANT: Do NOT transform array indices like x(1), x(2), etc.
        // Pattern matches: numbers like 0.1, 6, 100000, -0.981, etc.
        // But NOT already formatted numbers like 6.d0
        // And NOT numbers inside x(...) which are array indices
        String transformed = expression;
        
        // First, protect array indices by temporarily replacing them
        // Pattern to find x(number) - these are array indices and should NOT be changed
        Pattern arrayIndexPattern = Pattern.compile("x\\((\\d+)\\)");
        List<String> arrayIndices = new ArrayList<>();
        StringBuffer protectedBuffer = new StringBuffer();
        Matcher arrayMatcher = arrayIndexPattern.matcher(transformed);
        int index = 0;
        
        while (arrayMatcher.find()) {
            arrayIndices.add(arrayMatcher.group(0)); // Store the full match like "x(1)"
            arrayMatcher.appendReplacement(protectedBuffer, "___ARRAY_INDEX_" + index + "___");
            index++;
        }
        arrayMatcher.appendTail(protectedBuffer);
        transformed = protectedBuffer.toString();
        
        // Now transform numbers (array indices are protected)
        // Pattern to match numbers that need .d0 suffix
        // Matches: optional negative sign, digits, optional decimal point and digits
        // Excludes: numbers already ending with d0, d1, etc. (like 6.d0)
        // Also excludes numbers that are part of function names or other identifiers
        Pattern numberPattern = Pattern.compile("(?<![a-zA-Z_])(-?\\d+\\.?\\d*)(?![a-zA-Z_0-9])");
        Matcher matcher = numberPattern.matcher(transformed);
        StringBuffer sb = new StringBuffer();
        
        while (matcher.find()) {
            String number = matcher.group(1);
            // Add .d0 suffix for double precision
            // If number already has decimal point, use it; otherwise add .d0
            String replacement;
            if (number.contains(".")) {
                replacement = number + "d0";
            } else {
                replacement = number + ".d0";
            }
            matcher.appendReplacement(sb, Matcher.quoteReplacement(replacement));
        }
        matcher.appendTail(sb);
        transformed = sb.toString();
        
        // Restore array indices (they should remain as x(1), x(2), etc.)
        for (int i = 0; i < arrayIndices.size(); i++) {
            transformed = transformed.replace("___ARRAY_INDEX_" + i + "___", arrayIndices.get(i));
        }
        
        // Step 2: Add VV= prefix if not present (with proper spacing)
        transformed = transformed.trim();
        // Remove ANY existing VV= prefix (case-insensitive, with optional spaces)
        // This ensures we never have double VV=
        transformed = transformed.replaceFirst("(?i)^\\s*VV\\s*=\\s*", "");
        // Now add VV= prefix (guaranteed to be only one)
        transformed = "VV=" + transformed;
        
        // Step 3: Break long lines with Fortran 77 continuation
        // Fortran 77: lines can be up to 72 characters (columns 1-72)
        // First 6 columns reserved: first line uses 6 spaces, continuation uses 5 spaces + '-' in column 6
        transformed = breakLongLinesFortran77(transformed);
        
        logger.debug("Transformed expression:\n{}", transformed);
        return transformed;
    }
    
    /**
     * Break a long line into multiple lines with Fortran 77 continuation
     * 
     * FORTRAN 77 CONTINUATION RULES:
     * - Lines can be up to 72 characters (columns 1-72)
     * - Columns 1-6 are reserved for special purposes
     * - First line: 6 spaces, then "VV=       " (with spacing), then expression
     * - Continuation: 5 spaces, then '-' in column 6, then rest of expression
     * - Break at logical points (after operators, before function calls)
     * 
     * @param line The line to break (without leading spaces, should start with "VV=")
     * @return Multi-line string with proper Fortran 77 continuation
     */
    private String breakLongLinesFortran77(String line) {
        // Remove any existing leading spaces
        line = line.trim();
        
        // Remove any existing VV= prefix to avoid duplication
        // The method will add the properly formatted VV= prefix
        line = line.replaceFirst("(?i)^\\s*VV\\s*=\\s*", "");
        
        // Format first line: "      VV=       " (6 spaces + VV= + 7 spaces for alignment)
        String firstLinePrefix = "      VV=       ";
        int firstLineMaxContent = 72 - firstLinePrefix.length(); // Max content on first line
        
        // Continuation line prefix: "     -" (5 spaces + '-' in column 6)
        String continuationPrefix = "     -";
        int continuationMaxContent = 72 - continuationPrefix.length(); // Max content on continuation lines
        
        // If entire expression fits on first line, return it
        if (line.length() <= firstLineMaxContent) {
            return firstLinePrefix + line;
        }
        
        List<String> lines = new ArrayList<>();
        int pos = 0;
        boolean isFirstLine = true;
        
        while (pos < line.length()) {
            int maxContent;
            String prefix;
            
            if (isFirstLine) {
                maxContent = firstLineMaxContent;
                prefix = firstLinePrefix;
                isFirstLine = false;
            } else {
                maxContent = continuationMaxContent;
                prefix = continuationPrefix;
            }
            
            // Find the best break point (prefer breaking after operators or before functions)
            int endPos = findBestBreakPoint(line, pos, Math.min(pos + maxContent, line.length()));
            
            String segment = line.substring(pos, endPos);
            String formattedLine = prefix + segment;
            
            // Ensure line doesn't exceed 72 characters
            if (formattedLine.length() > 72) {
                // If too long, break at maxContent
                endPos = pos + maxContent;
                segment = line.substring(pos, endPos);
                formattedLine = prefix + segment;
            }
            
            lines.add(formattedLine);
            pos = endPos;
            
            // Skip whitespace at the start of next line
            while (pos < line.length() && Character.isWhitespace(line.charAt(pos))) {
                pos++;
            }
        }
        
        return String.join("\n", lines);
    }
    
    /**
     * Find the best break point in a Fortran expression
     * Prefers breaking after operators (+, -, *, /) or before function calls
     * 
     * @param line The line to break
     * @param start Start position
     * @param maxEnd Maximum end position (72 characters limit)
     * @return Best break position
     */
    private int findBestBreakPoint(String line, int start, int maxEnd) {
        if (maxEnd >= line.length()) {
            return line.length();
        }
        
        // Look for good break points (after operators, before functions)
        // Search backwards from maxEnd to find a good break point
        int bestPos = maxEnd;
        
        // Preferred break characters (after these)
        char[] breakAfter = {'+', '-', '*', '/', '(', ','};
        
        // Search backwards from maxEnd
        for (int i = maxEnd - 1; i > start + 10; i--) { // Don't break too early
            char c = line.charAt(i);
            for (char breakChar : breakAfter) {
                if (c == breakChar) {
                    // Found a good break point
                    bestPos = i + 1;
                    return bestPos;
                }
            }
        }
        
        // If no good break point found, break at maxEnd
        return maxEnd;
    }
    
    /**
     * Write the potential expression to potential.inc file
     * This file is included by multifix.f at compile time
     * 
     * @param fortranDir The directory containing the Fortran files
     * @param potentialExpression The potential expression from frontend
     * @throws IOException If file writing fails
     */
    private void writePotentialInc(Path fortranDir, String potentialExpression) throws IOException {
        Path potentialIncFile = fortranDir.resolve("potential.inc");
        logger.info("Writing potential to: {}", potentialIncFile);
        
        // Transform the expression to Fortran 77 format
        String fortranExpression = transformPotentialToFortran77(potentialExpression);
        
        try (BufferedWriter writer = Files.newBufferedWriter(potentialIncFile)) {
            writer.write(fortranExpression);
            writer.newLine();
            writer.newLine(); // Add blank line at end
        }
        
        logger.info("Successfully wrote potential.inc");
    }
    
    /**
     * Write initial conditions to initial_conditions.inc file
     * Format: x(1)=value, x(2)=value, etc.
     * 
     * Note: The existing format doesn't use .d0 suffix, but Fortran will interpret
     * the numbers correctly. We match the existing format for compatibility.
     * 
     * @param fortranDir The directory containing the Fortran files
     * @param conditions The initial conditions from frontend
     * @throws IOException If file writing fails
     */
    private void writeInitialConditionsInc(Path fortranDir, InitialConditionsDTO conditions) throws IOException {
        Path initialConditionsIncFile = fortranDir.resolve("initial_conditions.inc");
        logger.info("Writing initial conditions to: {}", initialConditionsIncFile);
        
        try (BufferedWriter writer = Files.newBufferedWriter(initialConditionsIncFile)) {
            List<Double> fieldValues = conditions.getFieldValues();
            
            // The include is inside a do i=1,nf loop
            // We write all field assignments: x(1)=value1, x(2)=value2, etc.
            // Even though the include is in a loop, all lines in the include file execute,
            // so all assignments will be set correctly
            
            if (fieldValues == null || fieldValues.isEmpty()) {
                logger.warn("No field values provided, using default");
                // Fortran 77: columns 1-5 for labels, column 6 for continuation, column 7+ for code
                // Write with 6 spaces to start in column 7 (proper Fortran 77 format)
                writer.write("      x(1)=6.33");
                writer.newLine();
            } else {
                // Write all field assignments: x(1)=value1, x(2)=value2, etc.
                // Format: 6 spaces + x(i)=value (starts in column 7, proper Fortran 77 format)
                for (int i = 0; i < fieldValues.size(); i++) {
                    Double value = fieldValues.get(i);
                    // Fortran arrays are 1-indexed, so use i+1
                    writer.write("      x(" + (i + 1) + ")=" + String.format("%.15f", value));
                    writer.newLine();
                }
            }
            
            writer.newLine(); // Add blank line at end
        }
        
        logger.info("✅Successfully wrote initial_conditions.inc");
    }
    
    /**
     * Prepare multifix.f with correct number of fields (nf)
     * Modifies the source code to set nf parameter based on number of fields
     * 
     * Uses sed command to preserve exact file format (line endings, encoding, etc.)
     * This is critical for Fortran 77 which is very sensitive to file format
     * 
     * @param fortranDir The directory containing multifix.f
     * @param numFields Number of fields from frontend
     * @return Path to the prepared source file (may be original or modified copy)
     * @throws IOException If file operations fail
     */
    private Path prepareMultifixSource(Path fortranDir, int numFields) throws IOException {
        Path originalSource = fortranDir.resolve("multifix.f");
        Path preparedSource = fortranDir.resolve("multifix_prepared.f");
        
        logger.info("Preparing multifix.f with nf={}", numFields);
        
        // Check if source file exists
        if (!Files.exists(originalSource)) {
            throw new IOException("Fortran source file not found: " + originalSource);
        }
        
        // Try using sed to preserve exact file format
        // sed preserves line endings and encoding exactly
        try {
            // sed command: replace nf=<number> with nf=<numFields>
            // Escape special characters for sed regex
            String sedPattern = String.format("s/\\(integer,\\s*parameter\\s*::\\s*nf\\s*=\\s*\\)[0-9]*/\\1%d/", numFields);
            
            ProcessBuilder sedBuilder = new ProcessBuilder(
                "sed",
                sedPattern,
                originalSource.toString()
            );
            
            sedBuilder.directory(fortranDir.toFile());
            sedBuilder.redirectOutput(preparedSource.toFile());
            sedBuilder.redirectErrorStream(true);
            
            logger.info("Running sed to modify nf parameter: {}", sedPattern);
            Process sedProcess = sedBuilder.start();
            
            // Read any error output
            StringBuilder sedOutput = new StringBuilder();
            try (BufferedReader reader = new BufferedReader(
                    new InputStreamReader(sedProcess.getInputStream()))) {
                String line;
                while ((line = reader.readLine()) != null) {
                    sedOutput.append(line).append("\n");
                }
            }
            
            int sedExitCode = sedProcess.waitFor();
            
            if (sedExitCode == 0 && Files.exists(preparedSource) && Files.size(preparedSource) > 0) {
                logger.info("Successfully used sed to prepare source file");
                return preparedSource;
            } else {
                logger.warn("sed command failed or produced empty file, falling back to Java method. Exit code: {}, Output: {}", 
                    sedExitCode, sedOutput.toString());
                // Fall through to Java method
            }
        } catch (Exception e) {
            logger.warn("sed command not available or failed: {}, falling back to Java method", e.getMessage());
            // Fall through to Java method
        }
        
        // Fallback: Use Java method (may have line ending issues, but better than nothing)
        // Read the original source file as bytes to preserve EXACT format
        byte[] fileBytes = Files.readAllBytes(originalSource);
        
        // Convert to string using ISO-8859-1 (1-to-1 byte mapping) for pattern matching
        String content = new String(fileBytes, java.nio.charset.StandardCharsets.ISO_8859_1);
        
        // Find and replace the nf parameter definition
        String pattern = "(integer,\\s*parameter\\s*::\\s*nf\\s*=\\s*)\\d+";
        java.util.regex.Pattern nfPattern = java.util.regex.Pattern.compile(pattern);
        java.util.regex.Matcher matcher = nfPattern.matcher(content);
        
        if (!matcher.find()) {
            logger.warn("Could not find nf parameter definition, using original file");
            Files.copy(originalSource, preparedSource, java.nio.file.StandardCopyOption.REPLACE_EXISTING);
            return preparedSource;
        }
        
        // Replace only the number, preserving everything else
        String modifiedContent = matcher.replaceFirst("$1" + numFields);
        
        // Convert back to bytes using ISO-8859-1
        byte[] modifiedBytes = modifiedContent.getBytes(java.nio.charset.StandardCharsets.ISO_8859_1);
        
        // Write back
        Files.write(preparedSource, modifiedBytes);
        
        logger.info("Prepared source file using Java method: {} (nf={})", preparedSource, numFields);
        return preparedSource;
    }
    
    /**
     * Compile the Fortran program multifix.f
     * 
     * @param fortranDir The directory containing multifix.f
     * @param numFields Number of fields (used to prepare source with correct nf)
     * @return Path to the compiled executable
     * @throws IOException If compilation fails
     * @throws InterruptedException If compilation is interrupted
     */
    private Path compileFortranProgram(Path fortranDir, int numFields) throws IOException, InterruptedException {
        // First, prepare the source file with correct nf value
        Path sourceFile = prepareMultifixSource(fortranDir, numFields);
        Path executable = fortranDir.resolve("multifix.exe");
        
        logger.info("Compiling Fortran program: {} -> {}", sourceFile, executable);
        
        // Build compilation command
        // gfortran is the GNU Fortran compiler
        // -o specifies output executable name
        ProcessBuilder compileBuilder = new ProcessBuilder(
            "gfortran",
            "-o", executable.toString(),
            sourceFile.toString()
        );
        
        compileBuilder.directory(fortranDir.toFile());
        compileBuilder.redirectErrorStream(true);
        
        logger.info("Running: gfortran -o {} {}", executable, sourceFile);
        Process compileProcess = compileBuilder.start();
        
        // Read compilation output
        StringBuilder compileOutput = new StringBuilder();
        try (BufferedReader reader = new BufferedReader(
                new InputStreamReader(compileProcess.getInputStream()))) {
            String line;
            while ((line = reader.readLine()) != null) {
                compileOutput.append(line).append("\n");
                logger.debug("Compilation output: {}", line);
            }
        }
        
        // Wait for compilation to finish
        int exitCode = compileProcess.waitFor();
        
        if (exitCode != 0) {
            String errorMsg = "Fortran compilation failed with exit code " + exitCode + 
                "\nCompilation output:\n" + compileOutput.toString();
            logger.error(errorMsg);
            throw new IOException(errorMsg);
        }
        
        // Check if executable was created
        if (!Files.exists(executable)) {
            throw new IOException("Compilation succeeded but executable not found: " + executable);
        }
        
        logger.info("Fortran compilation successful! Executable: {}", executable);
        return executable;
    }
    
    /**
     * Execute Fortran program with given initial conditions
     * 
     * WORKFLOW:
     * 1. Get fortran directory
     * 2. Write potential.inc (transformed to Fortran 77 format)
     * 3. Write initial_conditions.inc
     * 4. Compile multifix.f
     * 5. Run the compiled executable
     * 6. Collect output files
     * 7. Return results
     * 
     * @param initialConditions Initial conditions from frontend
     * @return Execution result containing output files and status
     */
    public FortranExecutionResult executeFortran(InitialConditionsDTO initialConditions) {
        String executionId = UUID.randomUUID().toString();
        
        logger.info("========================================");
        logger.info("Starting Fortran execution with ID: {}", executionId);
        logger.info("========================================");
        
        try {
            // Step 1: Get fortran directory
            Path fortranDir = getFortranDirectory();
            
            if (!Files.exists(fortranDir) || !Files.isDirectory(fortranDir)) {
                String errorMsg = "Fortran directory does not exist: " + fortranDir;
                logger.error(errorMsg);
                return new FortranExecutionResult(
                    executionId,
                    false,
                    errorMsg,
                    null,
                    null
                );
            }
            
            logger.info("Using fortran directory: {}", fortranDir);
            
            // Step 1.5: Clean up previous plot files
            cleanupOldPlots(fortranDir);
            
            // Step 2: Replace parameters in potential expression
            String potentialExpression = initialConditions.getPotentialExpression();
            Map<String, Double> parameterValues = initialConditions.getParameterValues();
            
            if (parameterValues != null && !parameterValues.isEmpty()) {
                logger.info("Replacing parameters in potential expression...");
                potentialExpression = replaceParameters(potentialExpression, parameterValues);
                logger.info("Potential expression after parameter replacement: {}", potentialExpression);
            }
            
            // Step 3: Write potential.inc file
            writePotentialInc(fortranDir, potentialExpression);
            
            // Step 4: Write initial_conditions.inc file
            writeInitialConditionsInc(fortranDir, initialConditions);
            
            // Step 5: Determine number of fields from initial conditions
            int numFields = initialConditions.getFieldValues() != null ? 
                initialConditions.getFieldValues().size() : 1;
            logger.info("Number of fields (nf): {}", numFields);
            
            // Step 6: Compile multifix.f with correct nf value
            Path executable;
            try {
                executable = compileFortranProgram(fortranDir, numFields);
            } catch (IOException | InterruptedException e) {
                logger.error("Compilation failed: ", e);
                return new FortranExecutionResult(
                    executionId,
                    false,
                    "Compilation failed: " + e.getMessage(),
                    e.getClass().getSimpleName() + ": " + e.getMessage(),
                    null
                );
            }
            
            // Step 7: Run the compiled executable
            // The Fortran program runs without command-line arguments
            // It reads from potential.inc and initial_conditions.inc at compile time
            ProcessBuilder processBuilder = new ProcessBuilder(
                executable.toString()
            );
            
            processBuilder.directory(fortranDir.toFile());
            processBuilder.redirectErrorStream(true);
            
            logger.info("Executing: {}", executable);
            Process process = processBuilder.start();
            
            // Store process for cancellation support
            runningProcesses.put(executionId, process);
            
            try {
                // Read output from the Fortran program
                logger.info("========================================");
                logger.info("Fortran program output (multifix.exe):");
                logger.info("========================================");
                StringBuilder output = new StringBuilder();
                try (BufferedReader reader = new BufferedReader(
                        new InputStreamReader(process.getInputStream()))) {
                    String line;
                    while ((line = reader.readLine()) != null) {
                        // Check if process was cancelled
                        if (!process.isAlive()) {
                            break;
                        }
                        output.append(line).append("\n");
                        // Log at INFO level so developers can see the output in terminal
                        logger.info("multifix.exe: {}", line);
                    }
                }
                logger.info("========================================");
                logger.info("End of Fortran program output");
                logger.info("========================================");
                
                // Wait for process with timeout
                boolean finished = process.waitFor(MAX_EXECUTION_TIME, TimeUnit.SECONDS);
                
                if (!finished) {
                    process.destroyForcibly();
                    return new FortranExecutionResult(
                        executionId,
                        false,
                        "Execution timeout after " + MAX_EXECUTION_TIME + " seconds",
                        output.toString(),
                        null
                    );
                }
                
                int exitCode = process.exitValue();
                
                // Step 8: Collect output files
                // The Fortran program writes various output files to the fortran directory
                List<String> outputFiles = collectOutputFiles(fortranDir);
                
                // Step 9: Generate plot if calculation was successful
                if (exitCode == 0) {
                    try {
                        String plotFile = generatePlot(fortranDir, executionId);
                        if (plotFile != null) {
                            outputFiles.add(plotFile);
                            logger.info("✅ Generated plot: {}", plotFile);
                        }
                    } catch (Exception e) {
                        logger.warn("Failed to generate plot: {}", e.getMessage());
                        // Don't fail the entire execution if plotting fails
                    }
                }
                
                if (exitCode == 0) {
                    String successMessage = String.format(
                        "Calculation completed successfully! Generated %d output file(s). Execution ID: %s",
                        outputFiles.size(),
                        executionId.substring(0, 8) + "..."
                    );
                    logger.info("✅ {}", successMessage);
                    return new FortranExecutionResult(
                        executionId,
                        true,
                        successMessage,
                        output.toString(),
                        outputFiles
                    );
                } else {
                    String errorMsg = "Fortran program exited with error code " + exitCode + ". Check output for details.";
                    logger.error("❌ {}", errorMsg);
                    return new FortranExecutionResult(
                        executionId,
                        false,
                        errorMsg,
                        output.toString(),
                        outputFiles
                    );
                }
            } finally {
                // Always remove process from tracking map when done
                runningProcesses.remove(executionId);
                logger.debug("Removed execution {} from tracking map", executionId);
            }
            
        } catch (IOException e) {
            logger.error("IO Error executing Fortran: ", e);
            return new FortranExecutionResult(
                executionId,
                false,
                "IO Error: " + e.getMessage() + ". Check if files exist and have permissions.",
                e.getClass().getSimpleName() + ": " + e.getMessage(),
                null
            );
        } catch (InterruptedException e) {
            logger.error("Interrupted while executing Fortran: ", e);
            Thread.currentThread().interrupt();
            // Clean up process if still running
            Process process = runningProcesses.remove(executionId);
            if (process != null && process.isAlive()) {
                process.destroyForcibly();
            }
            return new FortranExecutionResult(
                executionId,
                false,
                "Execution interrupted: " + e.getMessage(),
                null,
                null
            );
        } catch (Exception e) {
            // Clean up process if still running
            Process process = runningProcesses.remove(executionId);
            if (process != null && process.isAlive()) {
                process.destroyForcibly();
            }
            logger.error("Unexpected error executing Fortran: ", e);
            String errorDetails = e.getClass().getSimpleName() + ": " + e.getMessage();
            if (e.getCause() != null) {
                errorDetails += " (Caused by: " + e.getCause().getMessage() + ")";
            }
            return new FortranExecutionResult(
                executionId,
                false,
                "Error: " + e.getMessage(),
                errorDetails,
                null
            );
        }
    }
    
    /**
     * Cancel a running Fortran execution by execution ID
     * 
     * @param executionId The execution ID to cancel
     * @return true if the execution was found and cancelled, false otherwise
     */
    public boolean cancelExecution(String executionId) {
        if (executionId == null || executionId.trim().isEmpty()) {
            logger.warn("Attempted to cancel execution with null or empty execution ID");
            return false;
        }
        
        Process process = runningProcesses.get(executionId);
        if (process == null) {
            logger.warn("Execution {} not found in running processes", executionId);
            return false;
        }
        
        if (!process.isAlive()) {
            logger.info("Execution {} already finished", executionId);
            runningProcesses.remove(executionId);
            return false;
        }
        
        logger.info("Cancelling execution {}", executionId);
        process.destroyForcibly();
        runningProcesses.remove(executionId);
        logger.info("Successfully cancelled execution {}", executionId);
        return true;
    }
    
    /**
     * Check if an execution is currently running
     * 
     * @param executionId The execution ID to check
     * @return true if the execution is running, false otherwise
     */
    public boolean isExecutionRunning(String executionId) {
        if (executionId == null || executionId.trim().isEmpty()) {
            return false;
        }
        
        Process process = runningProcesses.get(executionId);
        return process != null && process.isAlive();
    }
    
    /**
     * Clean up previous plot files from the fortran directory
     * Deletes all files matching the pattern n_prz_kmode_plot_*.png
     * 
     * @param fortranDir The directory containing the plot files
     */
    private void cleanupOldPlots(Path fortranDir) {
        try {
            // Find all plot files matching the pattern
            try (java.util.stream.Stream<Path> stream = Files.list(fortranDir)) {
                long deletedCount = stream
                    .filter(Files::isRegularFile)
                    .filter(path -> {
                        String filename = path.getFileName().toString();
                        return filename.startsWith("n_prz_kmode_plot_") && filename.endsWith(".png");
                    })
                    .peek(path -> {
                        try {
                            Files.delete(path);
                            logger.info("🗑️  Deleted old plot file: {}", path.getFileName());
                        } catch (IOException e) {
                            logger.warn("Failed to delete old plot file {}: {}", path.getFileName(), e.getMessage());
                        }
                    })
                    .count();
                
                if (deletedCount > 0) {
                    logger.info("Cleaned up {} old plot file(s)", deletedCount);
                }
            }
        } catch (IOException e) {
            logger.warn("Error cleaning up old plots: {}", e.getMessage());
            // Don't fail the execution if cleanup fails
        }
    }
    
    /**
     * Generate plot from n_prz_kmode.txt using Python script
     * Creates a log-scale plot and saves it as PNG
     * 
     * @param fortranDir The directory containing the data file and plot script
     * @param executionId Execution ID for unique plot filename
     * @return Name of the generated plot file, or null if generation failed
     * @throws IOException If plot generation fails
     * @throws InterruptedException If plot generation is interrupted
     */
    private String generatePlot(Path fortranDir, String executionId) throws IOException, InterruptedException {
        Path dataFile = fortranDir.resolve("n_prz_kmode.txt");
        Path plotScript = fortranDir.resolve("plot_results.py");
        String plotFileName = "n_prz_kmode_plot_" + executionId.substring(0, 8) + ".png";
        Path plotFile = fortranDir.resolve(plotFileName);
        
        // Check if data file exists
        if (!Files.exists(dataFile)) {
            logger.warn("Data file not found for plotting: {}", dataFile);
            return null;
        }
        
        // Check if plot script exists
        if (!Files.exists(plotScript)) {
            logger.warn("Plot script not found: {}", plotScript);
            return null;
        }
        
        logger.info("Generating plot from {} -> {}", dataFile, plotFile);
        
        // Run Python script to generate plot
        ProcessBuilder plotBuilder = new ProcessBuilder(
            "python3",
            plotScript.toString(),
            dataFile.toString(),
            plotFile.toString()
        );
        
        // Try python if python3 doesn't work
        plotBuilder.directory(fortranDir.toFile());
        plotBuilder.redirectErrorStream(true);
        
        try {
            Process plotProcess = plotBuilder.start();
            
            // Read output
            StringBuilder plotOutput = new StringBuilder();
            try (BufferedReader reader = new BufferedReader(
                    new InputStreamReader(plotProcess.getInputStream()))) {
                String line;
                while ((line = reader.readLine()) != null) {
                    plotOutput.append(line).append("\n");
                    logger.debug("Plot output: {}", line);
                }
            }
            
            int plotExitCode = plotProcess.waitFor();
            
            if (plotExitCode == 0 && Files.exists(plotFile) && Files.size(plotFile) > 0) {
                logger.info("✅ Plot generated successfully: {}", plotFileName);
                return plotFileName;
            } else {
                // Try with 'python' instead of 'python3'
                logger.info("Trying with 'python' instead of 'python3'");
                plotBuilder = new ProcessBuilder(
                    "python",
                    plotScript.toString(),
                    dataFile.toString(),
                    plotFile.toString()
                );
                plotBuilder.directory(fortranDir.toFile());
                plotBuilder.redirectErrorStream(true);
                
                plotProcess = plotBuilder.start();
                plotOutput = new StringBuilder();
                try (BufferedReader reader = new BufferedReader(
                        new InputStreamReader(plotProcess.getInputStream()))) {
                    String line;
                    while ((line = reader.readLine()) != null) {
                        plotOutput.append(line).append("\n");
                    }
                }
                
                plotExitCode = plotProcess.waitFor();
                
                if (plotExitCode == 0 && Files.exists(plotFile) && Files.size(plotFile) > 0) {
                    logger.info("✅ Plot generated successfully: {}", plotFileName);
                    return plotFileName;
                } else {
                    logger.warn("Plot generation failed. Exit code: {}, Output: {}", 
                        plotExitCode, plotOutput.toString());
                    return null;
                }
            }
        } catch (Exception e) {
            logger.error("Error generating plot: ", e);
            return null;
        }
    }
    
    /**
     * Collect output files from the fortran directory
     * The Fortran program generates various .txt files with results
     * 
     * @param fortranDir The directory containing output files
     * @return List of output file names
     * @throws IOException If file collection fails
     */
    private List<String> collectOutputFiles(Path fortranDir) throws IOException {
        if (!Files.exists(fortranDir)) {
            logger.warn("Fortran directory does not exist: {}", fortranDir);
            return new ArrayList<>();
        }
        
        List<String> files = new ArrayList<>();
        
        // List of expected output files from multifix.f
        // These are the files the Fortran program writes
        String[] expectedOutputFiles = {
            "information.txt",
            "fields.txt",
            "n_epsilon_hubble.txt",
            "kmode.txt",
            "n_prz_kmode.txt",
            "prslow.txt",
            "gw2.txt"
        };
        
        // Check which expected files exist
        for (String fileName : expectedOutputFiles) {
            Path filePath = fortranDir.resolve(fileName);
            if (Files.exists(filePath) && Files.isRegularFile(filePath)) {
                files.add(fileName);
                logger.debug("Found output file: {}", fileName);
            }
        }
        
        logger.info("Collected {} output file(s)", files.size());
        return files;
    }
    
    /**
     * Result of Fortran execution
     * This is returned to the frontend as JSON
     */
    public static class FortranExecutionResult {
        private String executionId;
        private boolean success;
        private String message;
        private String output;
        private List<String> outputFiles;
        
        public FortranExecutionResult(String executionId, boolean success, 
                                     String message, String output, 
                                     List<String> outputFiles) {
            this.executionId = executionId;
            this.success = success;
            this.message = message;
            this.output = output;
            this.outputFiles = outputFiles;
        }
        
        // Getters (used by Spring to convert to JSON)
        public String getExecutionId() { return executionId; }
        public boolean isSuccess() { return success; }
        public String getMessage() { return message; }
        public String getOutput() { return output; }
        public List<String> getOutputFiles() { return outputFiles; }
    }
}
