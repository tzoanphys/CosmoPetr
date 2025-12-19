package com.cosmo.backend.controller;

import com.cosmo.backend.dto.InitialConditionsDTO;
import com.cosmo.backend.service.FortranExecutionService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.core.io.FileSystemResource;
import org.springframework.core.io.Resource;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.HashSet;
import java.util.regex.Pattern;
import java.util.regex.Matcher;

//Logging API (prints to conslole/logs in a structures way)
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

//Rest Controller for Cosmological Perturbation Calculations

@RestController // this class contains HTTP endpoints
@RequestMapping("/api/cosmo-perturbations")


public class CosmoController {

    private static final Logger logger = LoggerFactory.getLogger(CosmoController.class);  // Changed here
    //creates a logger named after this class | static =one logger shared with all instances |
    //                                         final =cannot be reassigned

    @Autowired //
    private FortranExecutionService fortranExecutionService;
    //Spring will autoamtically inject an instance of  FortranExecutionService
    // --> no new private FortranExecutionService will created

    //Execute cosmological perturbations calculation
    //POST /api/cosmo-perturbations/calculate

    /**
     * @param initialConditions Initial conditions from frontend
     * @return Execution result
     */

    @PostMapping("/calculate")
    public ResponseEntity<FortranExecutionService.FortranExecutionResult> calculate(
            @RequestBody InitialConditionsDTO initialConditions) {

        //ResponseEntity <..> --> return both a body and a HTTP status code
        //@RequestBody ---> take the JSON body from the frontend and convert it to InitialConditionsDTO

        logger.info("Received calculation request"); // print an information log message
        
        //try/catch = protect the endpoint so the backend doesnt crash if something goes wrong
        try {
            // Validate input --> check if the input is valid
            if (initialConditions.getFieldValues() == null || 
                initialConditions.getFieldValues().isEmpty()) {
                logger.warn("Validation failed: fieldValues is null or empty");
                return ResponseEntity.badRequest().build();
            }
            
            if (initialConditions.getFieldVelocities() == null || 
                initialConditions.getFieldVelocities().isEmpty()) {
                logger.warn("Validation failed: fieldVelocities is null or empty");
                return ResponseEntity.badRequest().build();
            }
            
            if (initialConditions.getFieldValues().size() != 
                initialConditions.getFieldVelocities().size()) {
                logger.warn("Validation failed: fieldValues size ({}) != fieldVelocities size ({})",
                    initialConditions.getFieldValues().size(),
                    initialConditions.getFieldVelocities().size());
                return ResponseEntity.badRequest().build();
            }
            
            // Validate parameters in potential expression
            String potentialExpression = initialConditions.getPotentialExpression();
            if (potentialExpression != null && !potentialExpression.trim().isEmpty()) {
                List<String> missingParams = validatePotentialParameters(
                    potentialExpression, 
                    initialConditions.getParameterValues()
                );
                
                if (!missingParams.isEmpty()) {
                    String errorMsg = "Missing parameter values for: " + String.join(", ", missingParams);
                    logger.warn("Validation failed: {}", errorMsg);
                    return ResponseEntity.badRequest().build();
                }
            }
            
            // We passed validation, so now we try running Fortran.
            logger.info("Input validated. 🚀 Executing Fortran program...");
            
            // Execute Fortran program
            FortranExecutionService.FortranExecutionResult result = 
                fortranExecutionService.executeFortran(initialConditions);
            
            logger.info("🏺Fortran execution completed. Success: {}, Message: {}", 
                result.isSuccess(), result.getMessage());
            
            if (result.isSuccess()) {
                return ResponseEntity.ok(result);
            } else {
                // Return 500 with error details
                logger.error("Fortran execution failed: {}", result.getMessage());
                return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).body(result);
            }
        } catch (Exception e) {
            logger.error("Unexpected error in calculate endpoint: ", e);
            FortranExecutionService.FortranExecutionResult errorResult = 
                new FortranExecutionService.FortranExecutionResult(
                    "error",
                    false,
                    "Unexpected error: " + e.getMessage(),
                    e.getClass().getSimpleName() + ": " + e.getMessage(),
                    null
                );
            return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).body(errorResult);
        }
    }
    
    /**
     * Cancel a running Fortran execution
     * POST /api/cosmo-perturbations/cancel/{executionId}
     * 
     * @param executionId The execution ID to cancel
     * @return Response indicating success or failure
     */
    @PostMapping("/cancel/{executionId}")
    public ResponseEntity<Map<String, Object>> cancelExecution(@PathVariable String executionId) {
        logger.info("Received cancel request for execution: {}", executionId);
        
        try {
            boolean cancelled = fortranExecutionService.cancelExecution(executionId);
            
            Map<String, Object> response = new java.util.HashMap<>();
            if (cancelled) {
                response.put("success", true);
                response.put("message", "Execution cancelled successfully");
                logger.info("Successfully cancelled execution: {}", executionId);
                return ResponseEntity.ok(response);
            } else {
                response.put("success", false);
                response.put("message", "Execution not found or already finished");
                logger.warn("Failed to cancel execution: {} (not found or already finished)", executionId);
                return ResponseEntity.status(HttpStatus.NOT_FOUND).body(response);
            }
        } catch (Exception e) {
            logger.error("Error cancelling execution {}: ", executionId, e);
            Map<String, Object> response = new java.util.HashMap<>();
            response.put("success", false);
            response.put("message", "Error cancelling execution: " + e.getMessage());
            return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).body(response);
        }
    }
    
    // Health check endpoint
    // GET /api/cosmo-perturbations/health --> this is a simple endpoint to check if the backend is runnin
     
    @GetMapping("/health")
    public ResponseEntity<String> health() {
        return ResponseEntity.ok("Cosmological Perturbations API is running");  // Updated message
    }
    
    /**
     * Test endpoint to check Fortran executable
     * GET /api/cosmo-perturbations/check-executable
     */
    @GetMapping("/check-executable")
    public ResponseEntity<String> checkExecutable() {
        try {
            java.nio.file.Path workDir = java.nio.file.Paths.get("./spare").toAbsolutePath();
            java.nio.file.Path executable = workDir.resolve("m.exe");
            
            StringBuilder response = new StringBuilder();
            response.append("Checking Fortran executable...\n");
            response.append("Work directory: ").append(workDir).append("\n");
            response.append("Executable path: ").append(executable).append("\n");
            response.append("Exists: ").append(java.nio.file.Files.exists(executable)).append("\n");
            
            if (java.nio.file.Files.exists(executable)) {
                response.append("Is executable: ").append(java.nio.file.Files.isExecutable(executable)).append("\n");
                response.append("Size: ").append(java.nio.file.Files.size(executable)).append(" bytes\n");
            } else {
                response.append("ERROR: Executable not found!\n");
                response.append("Please compile: cd spare && gfortran gravitationalwaves.f -o m.exe\n");
            }
            
            return ResponseEntity.ok(response.toString());
        } catch (Exception e) {
            return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR)
                .body("Error checking executable: " + e.getMessage());
        }
    }
    
    /**
     * Serve output files (plots, data files) from the fortran directory
     * GET /api/cosmo-perturbations/files/{filename}
     * 
     * @param filename Name of the file to serve
     * @return File resource or 404 if not found
     */
    @GetMapping("/files/{filename:.+}")
    public ResponseEntity<Resource> getFile(@PathVariable String filename) {
        try {
            // Get fortran directory (same logic as in FortranExecutionService)
            Path currentDir = Paths.get("").toAbsolutePath();
            Path fortranDir = null;
            
            // Try multiple possible locations
            List<Path> possiblePaths = new ArrayList<>();
            possiblePaths.add(currentDir.resolve("../fortran").normalize());
            possiblePaths.add(currentDir.resolve("fortran"));
            if (currentDir.toString().endsWith("backend")) {
                possiblePaths.add(currentDir.getParent().resolve("fortran"));
            }
            
            for (Path path : possiblePaths) {
                if (Files.exists(path) && Files.isDirectory(path)) {
                    fortranDir = path;
                    break;
                }
            }
            
            if (fortranDir == null) {
                fortranDir = possiblePaths.get(0);
            }
            
            Path filePath = fortranDir.resolve(filename).normalize();
            
            // Security check: ensure file is within fortran directory
            if (!filePath.startsWith(fortranDir.normalize())) {
                logger.warn("Attempted path traversal attack: {}", filename);
                return ResponseEntity.status(HttpStatus.FORBIDDEN).build();
            }
            
            if (!Files.exists(filePath) || !Files.isRegularFile(filePath)) {
                logger.warn("File not found: {}", filePath);
                return ResponseEntity.notFound().build();
            }
            
            Resource resource = new FileSystemResource(filePath.toFile());
            
            // Determine content type
            String contentType = Files.probeContentType(filePath);
            if (contentType == null) {
                if (filename.toLowerCase().endsWith(".png")) {
                    contentType = MediaType.IMAGE_PNG_VALUE;
                } else if (filename.toLowerCase().endsWith(".txt")) {
                    contentType = MediaType.TEXT_PLAIN_VALUE;
                } else {
                    contentType = MediaType.APPLICATION_OCTET_STREAM_VALUE;
                }
            }
            
            return ResponseEntity.ok()
                .contentType(MediaType.parseMediaType(contentType))
                .header(HttpHeaders.CONTENT_DISPOSITION, "inline; filename=\"" + filename + "\"")
                .body(resource);
                
        } catch (Exception e) {
            logger.error("Error serving file {}: ", filename, e);
            return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).build();
        }
    }
    
    /**
     * Validate that all parameters in potential expression have values
     * Similar to initial conditions validation
     * 
     * @param expression The potential expression
     * @param parameterValues Map of parameter names to values
     * @return List of missing parameter names, empty if all parameters are provided
     */
    private List<String> validatePotentialParameters(String expression, Map<String, Double> parameterValues) {
        List<String> missingParams = new ArrayList<>();
        
        if (expression == null || expression.trim().isEmpty()) {
            return missingParams;
        }
        
        Set<String> requiredParams = extractParameterSymbols(expression);
        
        if (requiredParams.isEmpty()) {
            // No parameters in expression, validation passes
            return missingParams;
        }
        
        if (parameterValues == null || parameterValues.isEmpty()) {
            // Expression has parameters but no values provided
            missingParams.addAll(requiredParams);
            return missingParams;
        }
        
        // Check each required parameter (case-insensitive)
        for (String param : requiredParams) {
            boolean found = false;
            for (String providedParam : parameterValues.keySet()) {
                if (param.equalsIgnoreCase(providedParam)) {
                    found = true;
                    break;
                }
            }
            if (!found) {
                missingParams.add(param);
            }
        }
        
        return missingParams;
    }
    
    /**
     * Extract parameter symbols from a potential expression
     * Finds all Latin letter identifiers (like m, lambda, i, etc.) that are not function names
     * 
     * @param expression The potential expression
     * @return Set of parameter symbols found in the expression
     */
    private Set<String> extractParameterSymbols(String expression) {
        Set<String> parameters = new HashSet<>();
        
        if (expression == null || expression.trim().isEmpty()) {
            return parameters;
        }
        
        // Common function names that should NOT be treated as parameters
        Set<String> functionNames = new HashSet<>();
        functionNames.add("sin"); functionNames.add("cos"); functionNames.add("tan");
        functionNames.add("sinh"); functionNames.add("cosh"); functionNames.add("tanh");
        functionNames.add("asin"); functionNames.add("acos"); functionNames.add("atan");
        functionNames.add("exp"); functionNames.add("log"); functionNames.add("ln");
        functionNames.add("sqrt"); functionNames.add("abs");
        functionNames.add("x"); // x is the field array, not a parameter
        
        // Pattern to match identifiers: letters followed by optional letters/digits
        Pattern identifierPattern = Pattern.compile("\\b([a-zA-Z][a-zA-Z0-9]*)\\b");
        Matcher matcher = identifierPattern.matcher(expression);
        
        while (matcher.find()) {
            String identifier = matcher.group(1).toLowerCase();
            
            // Skip if it's a function name
            if (functionNames.contains(identifier)) {
                continue;
            }
            
            // Skip if it's part of x(...) array access
            int start = matcher.start();
            if (start > 0 && expression.charAt(start - 1) == '(') {
                // Check if preceded by 'x'
                int checkStart = Math.max(0, start - 2);
                String before = expression.substring(checkStart, start);
                if (before.endsWith("x")) {
                    continue; // This is x(...), not a parameter
                }
            }
            
            parameters.add(identifier);
        }
        
        return parameters;
    }
}
