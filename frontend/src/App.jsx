// src/App.jsx
// We import React's useState so we can store changing values.
import { useState } from "react";
import "./App.css";

// API base URL - use environment variable in production, or relative path in development
const API_BASE_URL = import.meta.env.VITE_API_URL || '';

function App() {
  // Which page is currently visible: "model", "initial", "summary", "about", or "instructions"
  const [activePage, setActivePage] = useState("about");
  // Number of scalar fields
  const [numFields, setNumFields] = useState(1);
  // Display value for number of fields (allows typing)
  const [numFieldsInput, setNumFieldsInput] = useState("1");
  // Potential V(phi) - Starobinsky potential
  const [potential, setPotential] = useState(" (1 - exp(-sqrt(2/3) * x(1)))**2");
  // Arrays for initial values and velocities of each field
  const [initialValues, setInitialValues] = useState([""]);
  const [initialVelocities, setInitialVelocities] = useState([""]);
  // Initial time (for example N = 0 or t = 0)
  const [initialTime, setInitialTime] = useState("0.0");
  // Number of parameters
  const [numParameters, setNumParameters] = useState(0);
  // Display value for number of parameters (allows typing)
  const [numParametersInput, setNumParametersInput] = useState("0");
  // Array of parameter objects: [{name: "m", value: "1.0"}, ...]
  const [parameters, setParameters] = useState([]);

  // ----------------------------------------------------
  //  update number of fields AND keep arrays in sync
  // ----------------------------------------------------
  function handleNumFieldsChange(event) {
    // event.target.value is a string from the input
    const valueAsString = event.target.value;

    // Update the input display value to allow free typing
    setNumFieldsInput(valueAsString);

    // Convert to integer
    let n = parseInt(valueAsString, 10);

    // If conversion fails or n < 1, don't update the actual number or arrays
    if (isNaN(n) || n < 1) {
      return;
    }

    // Update state for number of fields
    setNumFields(n);

    // Update initialValues array to have length n
    setInitialValues((previousArray) => {
      const newArray = [...previousArray]; // copy old values

      if (n > newArray.length) {
        // If we need more fields, add empty strings
        while (newArray.length < n) {
          newArray.push("");
        }
      } else if (n < newArray.length) {
        // If fewer fields, cut the extra ones
        newArray.length = n;
      }

      return newArray;
    });

    // Update initialVelocities array to have length n
    setInitialVelocities((previousArray) => {
      const newArray = [...previousArray];

      if (n > newArray.length) {
        while (newArray.length < n) {
          newArray.push("");
        }
      } else if (n < newArray.length) {
        newArray.length = n;
      }

      return newArray;
    });
  }

  // ----------------------------------------------------
  // Update value of field i
  // ----------------------------------------------------
  function handleFieldValueChange(index, event) {
    const newValue = event.target.value;
    const copy = [...initialValues]; // copy the array
    copy[index] = newValue;          // change only one position
    setInitialValues(copy);          // save new array
  }

  // ----------------------------------------------------
  // Update velocity of field i
  // ----------------------------------------------------
  function handleFieldVelocityChange(index, event) {
    const newValue = event.target.value;
    const copy = [...initialVelocities];
    copy[index] = newValue;
    setInitialVelocities(copy);
  }

  // ----------------------------------------------------
  // Update number of parameters AND keep array in sync
  // ----------------------------------------------------
  function handleNumParametersChange(event) {
    const valueAsString = event.target.value;
    
    // Update the input display value to allow free typing
    setNumParametersInput(valueAsString);
    
    let n = parseInt(valueAsString, 10);
    
    // If conversion fails or n < 0, don't update the actual number or arrays
    if (isNaN(n) || n < 0) {
      return;
    }
    
    setNumParameters(n);
    
    // Update parameters array to have length n
    setParameters((previousArray) => {
      const newArray = [...previousArray];
      
      if (n > newArray.length) {
        // If we need more parameters, add empty ones with empty value (user can enter their own)
        while (newArray.length < n) {
          newArray.push({ name: "", value: "" });
        }
      } else if (n < newArray.length) {
        // If fewer parameters, cut the extra ones
        newArray.length = n;
      }
      
      return newArray;
    });
  }

  // ----------------------------------------------------
  // Update parameter name at index i
  // ----------------------------------------------------
  function handleParameterNameChange(index, event) {
    const newName = event.target.value;
    const copy = [...parameters];
    copy[index] = { ...copy[index], name: newName };
    setParameters(copy);
  }

  // ----------------------------------------------------
  // Update parameter value at index i
  // ----------------------------------------------------
  function handleParameterValueChange(index, event) {
    const newValue = event.target.value;
    const copy = [...parameters];
    // Allow empty values - don't force "0"
    copy[index] = { ...copy[index], value: newValue };
    setParameters(copy);
  }

  // ----------------------------------------------------
  // State for calculation results
  // ----------------------------------------------------
  const [calculationResult, setCalculationResult] = useState(null);
  const [isCalculating, setIsCalculating] = useState(false);
  const [error, setError] = useState(null);
  const [currentExecutionId, setCurrentExecutionId] = useState(null);
  const [abortController, setAbortController] = useState(null);

  // ----------------------------------------------------
  // When user clicks the "Run Calculation" button
  // ----------------------------------------------------
  async function handleRunCalculation() {
    // Validate inputs
    const fieldValues = initialValues
      .map(v => parseFloat(v))
      .filter(v => !isNaN(v));
    
    const fieldVelocities = initialVelocities
      .map(v => parseFloat(v))
      .filter(v => !isNaN(v));
    
    if (fieldValues.length === 0 || fieldVelocities.length === 0) {
      alert("Please enter valid field values and velocities!");
      return;
    }
    
    if (fieldValues.length !== fieldVelocities.length) {
      alert("Number of field values must match number of velocities!");
      return;
    }
    
    setIsCalculating(true);
    setError(null);
    // Clear previous result immediately to prevent showing old plot
    setCalculationResult(null);
    setCurrentExecutionId(null);
    
    // Force a small delay to ensure React re-renders and clears the old image
    // This helps prevent showing cached images
    await new Promise(resolve => setTimeout(resolve, 100));
    
    // Create AbortController for cancellation
    const controller = new AbortController();
    setAbortController(controller);
    
    // Prepare parameters object from the parameters array
    // Only include parameters with valid names and numeric values
    const parametersObj = {};
    parameters.forEach(param => {
      if (param.name && param.name.trim() !== "") {
        const valueStr = param.value ? param.value.trim() : "";
        if (valueStr !== "") {
          const value = parseFloat(valueStr);
          if (!isNaN(value)) {
            parametersObj[param.name] = value;
          }
        }
      }
    });

    // Prepare request data
    const requestData = {
      fieldValues: fieldValues,
      fieldVelocities: fieldVelocities,
      initialTime: parseFloat(initialTime) || 0.0,
      timeStep: 0.05,
      kstar: 0.05,
      cq: 100.0,
      potentialType: "tanh",
      potentialParameters: [
        0.1,                    // a0 (base coefficient)
        0.0007,                 // a1
        0.0007,                 // a2
        0.0007,                 // a3
        0.0007,                 // a4
        -0.9811333867328498,    // b1
        -0.9787014277530225,    // b2
        -0.9759597865125814,    // b3
        -0.972870068569409      // b4
      ],
      potentialExpression: potential || "",
      parameterValues: parametersObj,
      numParameters: numParameters
    };
    
    try {
      const response = await fetch(`${API_BASE_URL}/api/cosmo-perturbations/calculate`, {
        method: 'POST',
        headers: {
          'Content-Type': 'application/json',
        },
        body: JSON.stringify(requestData),
        signal: controller.signal
      });
      
      if (!response.ok) {
        throw new Error(`HTTP error! status: ${response.status}`);
      }
      
      const result = await response.json();
      setCalculationResult(result);
      setCurrentExecutionId(result.executionId);
      
      if (result.success) {
        const fileCount = result.outputFiles ? result.outputFiles.length : 0;
        alert(`✅ Calculation completed successfully!\n\nGenerated ${fileCount} output file(s).\n\nCheck the results below for details.`);
      } else {
        alert("❌ Calculation failed: " + result.message + "\n\nCheck the error details below.");
      }
    } catch (err) {
      if (err.name === 'AbortError') {
        setError("Calculation was cancelled by user");
        setCalculationResult({
          success: false,
          message: "Calculation was cancelled",
          executionId: currentExecutionId
        });
      } else {
        setError(err.message);
        alert("Error: " + err.message);
        console.error("Error:", err);
      }
    } finally {
      setIsCalculating(false);
      setAbortController(null);
    }
  }

  // ----------------------------------------------------
  // Cancel running calculation
  // ----------------------------------------------------
  async function handleCancelCalculation() {
    if (!abortController) {
      return;
    }
    
    try {
      // Abort the fetch request first (this will interrupt the HTTP request)
      abortController.abort();
      
      // If we have an executionId, also call backend cancel endpoint to kill the process
      if (currentExecutionId) {
        try {
          const response = await fetch(`${API_BASE_URL}/api/cosmo-perturbations/cancel/${currentExecutionId}`, {
            method: 'POST'
          });
          
          if (response.ok) {
            const result = await response.json();
            if (result.success) {
              console.log("Backend process cancelled successfully");
            }
          }
        } catch (err) {
          console.error("Error calling cancel endpoint:", err);
          // Continue anyway - the abort should have stopped the request
        }
      }
      
      // Update UI
      setIsCalculating(false);
      setError("Calculation was cancelled by user");
      setCalculationResult({
        success: false,
        message: "Calculation was cancelled by user",
        executionId: currentExecutionId || "unknown"
      });
      alert("Calculation cancelled");
    } catch (err) {
      console.error("Error cancelling calculation:", err);
      setIsCalculating(false);
      setError("Calculation cancellation requested");
    } finally {
      setAbortController(null);
      setCurrentExecutionId(null);
    }
  }

  // ----------------------------------------------------
  // When user clicks the "Preview configuration" button
  // ----------------------------------------------------
  //function handlePreviewConfig() {
   // const config = {
   //   numFields: numFields,
   //   potential: potential,
   //   numParameters: numParameters,
   //   parameters: parameters,
   //   initialTime: initialTime,
   //   initialValues: initialValues,
   //   initialVelocities: initialVelocities,
  //  };

    // Show the configuration in the browser console
  // console.log("Cosmological perturbation config:", config);

    // Show a simple message on the screen
    //alert("Parameters collected! Click 'Run Calculation' to execute.");
 // }

  // ----------------------------------------------------
  // JSX: what appears on the screen
  // ----------------------------------------------------
  return (
    <div className="app">
      {/* ================== LEFT SIDEBAR ================== */}
      <aside className="sidebar">
        <div style={{ textAlign: "center", marginBottom: "20px" }}>
          <img 
            src="/unnamed1.jpg" 
            alt="Eagle Logo" 
            style={{
              width: "100px",
              height: "100px",
              objectFit: "contain"
            }}
          />
        </div>
        <div className="menu-title">Menu</div>

        <nav className="menu-list">

           {/* Button: About */}
          <button
            className={`menu-item ${activePage === "about" ? "active" : ""}`}
            onClick={() => setActivePage("about")}
          >
            About
          </button>

          {/* Button: Model setup */}
          <button
            className={`menu-item ${activePage === "model" ? "active" : ""}`}
            onClick={() => setActivePage("model")}
          >
            Model setup
          </button>

          {/* Button: Initial conditions */}
          <button
            className={`menu-item ${activePage === "initial" ? "active" : ""}`}
            onClick={() => setActivePage("initial")}
          >
            Initial conditions
          </button>

          {/* Button: Summary / Export */}
          <button
            className={`menu-item ${activePage === "summary" ? "active" : ""}`}
            onClick={() => setActivePage("summary")}
          >
            Summary / Export
          </button>


        </nav>
      </aside>

      {/* ================== MAIN CONTENT ================== */}
      <main className="content">
        {/* Top banner */}
        <section className="header-banner">
          <h1 className="name-title">Cosmological Perturbations</h1>
          <p className="header-subtitle">
            Define your multi-field inflation model and initial conditions. This app 
            will give you the infaltionary dynamics and the power spectrum.
          </p>
        </section>

        {/* ================== PAGE: ABOUT ================== */}
        {activePage === "about" && (
          <section className="section">
            <h2 className="section-title">About this app</h2>
            <p className="section-text">
              This app provides the cosmological perturbations 
              for a given inflationary potential and initial condition. The idea is:
            </p>
            <ol className="section-list">
              <li>Define the inflationary potential and model parameters.</li>
              <li>Specify initial conditions for the background fields.</li>
              <li>
                Send the configuration to a Fortran backend that integrates the
                background and perturbation equations.
              </li>
              <li>
                Visualize power spectra and other observables directly in the
                browser.
              </li>
            </ol>

            {/* Notes of cosmological fluctuations */}
            <div style={{ 
              marginTop: '30px', 
              padding: '20px', 
              backgroundColor: '#0a0a0a', 
              borderRadius: '4px', 
              border: '1px solid rgba(0, 191, 166, 0.3)', 
              maxWidth: '95%' 
            }}>
              <strong style={{ 
                color: '#00ffc3', 
                display: 'block', 
                marginBottom: '12px', 
                fontSize: '16px' 
              }}>
                Notes of cosmological fluctuations
              </strong>
              <p style={{ 
                color: '#e8fff7', 
                margin: '0 0 12px 0',
                fontSize: '14px'
              }}>
                Access the detailed notes on cosmological fluctuations:
              </p>
              <a 
                href="/fluctuatiions.pdf" 
                target="_blank" 
                rel="noopener noreferrer"
                style={{ 
                  color: '#00ffc3', 
                  textDecoration: 'underline',
                  fontSize: '14px',
                  display: 'inline-block',
                  marginTop: '8px'
                }}
              >
                Open PDF →
              </a>
            </div>

            {/* Instructions button */}
            <div style={{ 
              marginTop: '30px', 
              padding: '20px', 
              backgroundColor: '#0a0a0a', 
              borderRadius: '4px', 
              border: '1px solid rgba(0, 191, 166, 0.3)', 
              maxWidth: '95%' 
            }}>
              <strong style={{ 
                color: '#00ffc3', 
                display: 'block', 
                marginBottom: '12px', 
                fontSize: '16px' 
              }}>
                How to use this app
              </strong>
              <p style={{ 
                color: '#e8fff7', 
                margin: '0 0 16px 0',
                fontSize: '14px'
              }}>
                How to write potential expressions and use the app correctly. 
              </p>
              <button 
                className="primary-button" 
                onClick={() => setActivePage("instructions")}
                style={{ 
                  backgroundColor: '#00ffc3',
                  color: '#0a0a0a',
                  fontWeight: 'bold',
                  padding: '12px 24px',
                  fontSize: '16px',
                  border: 'none',
                  cursor: 'pointer'
                }}
              >
                View Instructions →
              </button>
            </div>
          </section>
        )}

        {/* ================== PAGE: INSTRUCTIONS ================== */}
        {activePage === "instructions" && (
          <section className="section">
            <h2 className="section-title">Instructions</h2>
            
            <div style={{ 
              marginTop: '20px', 
              padding: '20px', 
              backgroundColor: '#0a0a0a', 
              borderRadius: '4px', 
              border: '1px solid rgba(0, 191, 166, 0.3)', 
              maxWidth: '95%' 
            }}>
              <h3 style={{ 
                color: '#00ffc3', 
                fontSize: '20px', 
                marginBottom: '16px',
                marginTop: '0'
              }}>
                Writing Potential Expressions
              </h3>
              
              <div style={{ marginBottom: '24px' }}>
                <h4 style={{ 
                  color: '#00ffc3', 
                  fontSize: '16px', 
                  marginBottom: '12px',
                  marginTop: '0'
                }}>
                  Field Notation
                </h4>
                <ul style={{ 
                  color: '#e8fff7', 
                  fontSize: '14px',
                  lineHeight: '1.8',
                  paddingLeft: '20px',
                  margin: '0'
                }}>
                  <li><code style={{ color: '#91fff3' }}>x(1)</code> represents field 1</li>
                  <li><code style={{ color: '#91fff3' }}>x(2)</code> represents field 2</li>
                  <li><code style={{ color: '#91fff3' }}>x(3)</code> represents field 3</li>
                  <li>And so on for additional fields...</li>
                </ul>
              </div>

              <div style={{ marginBottom: '24px' }}>
                <h4 style={{ 
                  color: '#00ffc3', 
                  fontSize: '16px', 
                  marginBottom: '12px',
                  marginTop: '0'
                }}>
                  Mathematical Operators
                </h4>
                <ul style={{ 
                  color: '#e8fff7', 
                  fontSize: '14px',
                  lineHeight: '1.8',
                  paddingLeft: '20px',
                  margin: '0'
                }}>
                  <li>Use <code style={{ color: '#91fff3' }}>**</code> for exponentiation, not <code style={{ color: '#91fff3' }}>^</code></li>
                  <li>Example: <code style={{ color: '#91fff3' }}>10**2</code> = 100 (not <code style={{ color: '#91fff3' }}>10^2</code>)</li>
                  <li>Use <code style={{ color: '#91fff3' }}>*</code> for multiplication</li>
                  <li>Use <code style={{ color: '#91fff3' }}>/</code> for division</li>
                  <li>Use <code style={{ color: '#91fff3' }}>+</code> and <code style={{ color: '#91fff3' }}>-</code> for addition and subtraction</li>
                </ul>
              </div>

              <div style={{ marginBottom: '24px' }}>
                <h4 style={{ 
                  color: '#00ffc3', 
                  fontSize: '16px', 
                  marginBottom: '12px',
                  marginTop: '0'
                }}>
                  Common Functions
                </h4>
                <ul style={{ 
                  color: '#e8fff7', 
                  fontSize: '14px',
                  lineHeight: '1.8',
                  paddingLeft: '20px',
                  margin: '0'
                }}>
                  <li><code style={{ color: '#91fff3' }}>Sin(x)</code>, <code style={{ color: '#91fff3' }}>Cos(x)</code>, <code style={{ color: '#91fff3' }}>Tan(x)</code></li>
                  <li><code style={{ color: '#91fff3' }}>Sinh(x)</code>, <code style={{ color: '#91fff3' }}>Cosh(x)</code>, <code style={{ color: '#91fff3' }}>Tanh(x)</code></li>
                  <li><code style={{ color: '#91fff3' }}>Sqrt(x)</code> for square root</li>
                  <li><code style={{ color: '#91fff3' }}>Exp(x)</code> for exponential</li>
                  <li><code style={{ color: '#91fff3' }}>Log(x)</code> for natural logarithm</li>
                </ul>
              </div>

              <div style={{ marginBottom: '24px' }}>
                <h4 style={{ 
                  color: '#00ffc3', 
                  fontSize: '16px', 
                  marginBottom: '12px',
                  marginTop: '0'
                }}>
                  Examples
                </h4>
                <div style={{ 
                  backgroundColor: '#050505', 
                  padding: '16px', 
                  borderRadius: '4px',
                  border: '1px solid rgba(0, 191, 166, 0.2)',
                  marginTop: '12px'
                }}>
                  <p style={{ 
                    color: '#e8fff7', 
                    fontSize: '14px',
                    margin: '0 0 8px 0'
                  }}>
                    <strong style={{ color: '#00ffc3' }}>Single field:</strong>
                  </p>
                  <code style={{ 
                    color: '#91fff3', 
                    fontSize: '13px',
                    display: 'block',
                    marginBottom: '12px'
                  }}>
                    0.5 * m**2 * x(1)**2
                  </code>
                  
                  <p style={{ 
                    color: '#e8fff7', 
                    fontSize: '14px',
                    margin: '0 0 8px 0'
                  }}>
                    <strong style={{ color: '#00ffc3' }}>Two fields with parameters:</strong>
                  </p>
                  <code style={{ 
                    color: '#91fff3', 
                    fontSize: '13px',
                    display: 'block',
                    marginBottom: '12px'
                  }}>
                    A + G * x(1) - B * Tanh(c * x(1))
                  </code>
                  
                  <p style={{ 
                    color: '#e8fff7', 
                    fontSize: '14px',
                    margin: '0 0 8px 0'
                  }}>
                    <strong style={{ color: '#00ffc3' }}>With trigonometric functions:</strong>
                  </p>
                  <code style={{ 
                    color: '#91fff3', 
                    fontSize: '13px',
                    display: 'block'
                  }}>
                    lambda * (1 - Cos(x(1) / f))
                  </code>
                </div>
              </div>

              <div style={{ marginBottom: '24px' }}>
                <h4 style={{ 
                  color: '#00ffc3', 
                  fontSize: '16px', 
                  marginBottom: '12px',
                  marginTop: '0'
                }}>
                  Parameters
                </h4>
                <p style={{ 
                  color: '#e8fff7', 
                  fontSize: '14px',
                  lineHeight: '1.8',
                  margin: '0'
                }}>
                  You can use parameters (like <code style={{ color: '#91fff3' }}>m</code>, <code style={{ color: '#91fff3' }}>lambda</code>, <code style={{ color: '#91fff3' }}>A</code>, etc.) in your potential expression. 
                  Define these parameters in the "Model setup" page by specifying their names and values. 
                  The app will automatically replace the parameter symbols with their numeric values before execution.
                </p>
              </div>
            </div>

            {/* Navigation button */}
            <div style={{ display: 'flex', justifyContent: 'flex-start', marginTop: '30px' }}>
              <button 
                className="primary-button" 
                onClick={() => setActivePage("about")}
                style={{ 
                  backgroundColor: 'transparent',
                  color: '#00ffc3',
                  fontWeight: 'bold',
                  padding: '12px 24px',
                  fontSize: '16px',
                  border: '1px solid rgba(0, 191, 166, 0.5)',
                  cursor: 'pointer'
                }}
              >
                ← Back to About
              </button>
            </div>
          </section>
        )}

        {/* ================== PAGE: MODEL SETUP ================== */}
        {activePage === "model" && (
          <section className="section">
            <h2 className="section-title">1. Model setup</h2>
            <p className="section-text">
              Choose the number of scalar fields and specify the potential
              V(φᵢ). You can use a simple math-like syntax (for example{" "}
              <code>0.5*m^2*x(1)**2 + lambda*x(1)**4</code>).
            </p>

            <div className="form-grid">
              {/* Number of fields input */}
              <label className="form-field">
                <span className="field-label">Number of fields</span>
                <input
                  type="text"
                  value={numFieldsInput}
                  onChange={handleNumFieldsChange}
                  onBlur={(e) => {
                    // On blur, validate and fix if needed
                    const n = parseInt(e.target.value, 10);
                    if (isNaN(n) || n < 1) {
                      setNumFieldsInput("1");
                      setNumFields(1);
                    } else {
                      setNumFieldsInput(String(n));
                      setNumFields(n);
                    }
                  }}
                  placeholder="e.g. 1, 2, 3"
                />
              </label>

              {/* Number of parameters input */}
              <label className="form-field">
                <span className="field-label">Number of parameters</span>
                <input
                  type="text"
                  value={numParametersInput}
                  onChange={handleNumParametersChange}
                  onBlur={(e) => {
                    // On blur, validate and fix if needed
                    const n = parseInt(e.target.value, 10);
                    if (isNaN(n) || n < 0) {
                      setNumParametersInput("0");
                      setNumParameters(0);
                    } else {
                      setNumParametersInput(String(n));
                      setNumParameters(n);
                    }
                  }}
                  placeholder="e.g. 0, 1, 2"
                />
              </label>

              {/* Potential input */}
              <label className="form-field form-field-full">
                <span className="field-label">Potential V(φ)</span>
                <textarea
                  rows={4}
                  value={potential}
                  onChange={(event) => setPotential(event.target.value)}
                  placeholder="Example: 0.5*m^2*phi1^2 + lambda*phi1^4"
                />
              </label>
            </div>

            {/* Parameters table */}
            {numParameters > 0 && (
              <div style={{ marginTop: "20px" }}>
                <h3 style={{ 
                  color: "#00ffc3", 
                  fontSize: "18px", 
                  marginBottom: "12px" 
                }}>
                  Parameters
                </h3>
                <p className="section-text" style={{ marginBottom: "12px" }}>
                  Define the parameter names and their values. Default value is 0.
                </p>
                <div className="fields-table">
                  {/* Header row */}
                  <div className="fields-header">
                    <span>Parameter</span>
                    <span>Name</span>
                    <span>Value</span>
                  </div>

                  {/* Rows for each parameter */}
                  {Array.from({ length: numParameters }).map((_, index) => (
                    <div className="fields-row" key={index}>
                      {/* Parameter label */}
                      <span className="field-name">Param {index + 1}</span>

                      {/* Parameter name input */}
                      <input
                        type="text"
                        value={parameters[index]?.name || ""}
                        onChange={(event) =>
                          handleParameterNameChange(index, event)
                        }
                        placeholder="e.g. m, lambda"
                      />

                      {/* Parameter value input */}
                      <input
                        type="text"
                        value={parameters[index]?.value || ""}
                        onChange={(event) =>
                          handleParameterValueChange(index, event)
                        }
                        placeholder="0"
                      />
                    </div>
                  ))}
                </div>
              </div>
            )}

            {/* Navigation buttons */}
            <div style={{ display: 'flex', justifyContent: 'space-between', marginTop: '30px' }}>
              <button 
                className="primary-button" 
                onClick={() => setActivePage("about")}
                style={{ 
                  backgroundColor: 'transparent',
                  color: '#00ffc3',
                  fontWeight: 'bold',
                  padding: '12px 24px',
                  fontSize: '16px',
                  border: '1px solid rgba(0, 191, 166, 0.5)',
                  cursor: 'pointer'
                }}
              >
                ← Back
              </button>
              <button 
                className="primary-button" 
                onClick={() => setActivePage("initial")}
                style={{ 
                  backgroundColor: '#00ffc3',
                  color: '#0a0a0a',
                  fontWeight: 'bold',
                  padding: '12px 24px',
                  fontSize: '16px'
                }}
              >
                Next →
              </button>
            </div>
          </section>
        )}

        {/* ================== PAGE: INITIAL CONDITIONS ================== */}
        {activePage === "initial" && (
          <section className="section">
            <h2 className="section-title">2. Initial conditions</h2>
            <p className="section-text">
              Set the background initial conditions for each field and its
              conjugate momentum / time derivative.
            </p>

            {/* Initial time */}
            <div className="form-grid">
              <label className="form-field">
                <span className="field-label">Initial time</span>
                <input
                  type="text"
                  value={initialTime}
                  onChange={(event) => setInitialTime(event.target.value)}
                  placeholder="e.g. N = 0 or t = 0"
                />
              </label>
            </div>

            {/* Table for fields */}
            <div className="fields-table">
              {/* Header row */}
              <div className="fields-header">
                <span>Field</span>
                <span>ϕᵢ (initial value)</span>
                <span>ϕ̇ᵢ or πᵢ (initial velocity)</span>
              </div>

              {/* Rows for each field (from 1 to numFields) */}
              {Array.from({ length: numFields }).map((_, index) => (
                <div className="fields-row" key={index}>
                  {/* Field label φ1, φ2, ... */}
                  <span className="field-name">φ{index + 1}</span>

                  {/* Initial value input */}
                  <input
                    type="text"
                    value={initialValues[index] || ""}
                    onChange={(event) =>
                      handleFieldValueChange(index, event)
                    }
                    placeholder="e.g. 15.0"
                  />

                  {/* Initial velocity input */}
                  <input
                    type="text"
                    value={initialVelocities[index] || ""}
                    onChange={(event) =>
                      handleFieldVelocityChange(index, event)
                    }
                    placeholder="e.g. 0.0"
                  />
                </div>
              ))}
            </div>

            {/* Navigation buttons */}
            <div style={{ display: 'flex', justifyContent: 'space-between', marginTop: '30px' }}>
              <button 
                className="primary-button" 
                onClick={() => setActivePage("model")}
                style={{ 
                  backgroundColor: 'transparent',
                  color: '#00ffc3',
                  fontWeight: 'bold',
                  padding: '12px 24px',
                  fontSize: '16px',
                  border: '1px solid rgba(0, 191, 166, 0.5)',
                  cursor: 'pointer'
                }}
              >
                ← Back
              </button>
              <button 
                className="primary-button" 
                onClick={() => setActivePage("summary")}
                style={{ 
                  backgroundColor: '#00ffc3',
                  color: '#0a0a0a',
                  fontWeight: 'bold',
                  padding: '12px 24px',
                  fontSize: '16px'
                }}
              >
                Next →
              </button>
            </div>
          </section>
        )}

        {/* ================== PAGE: SUMMARY / EXPORT ================== */}
        {activePage === "summary" && (
          <section className="section">
            <h2 className="section-title">3. Summary / Export</h2>
            <p className="section-text">
              Here is a quick preview of the configuration that will be sent to
               run.
            </p>

            {/* Show all data in JSON format (potential excluded to avoid duplication) */}
            <div style={{ marginTop: '16px', padding: '20px', backgroundColor: '#0a0a0a', borderRadius: '4px', border: '1px solid rgba(0, 191, 166, 0.3)', maxWidth: '95%' }}>
              <strong style={{ color: '#00ffc3', display: 'block', marginBottom: '12px', fontSize: '16px' }}>Configuration Summary:</strong>
              <pre style={{ 
                color: '#91fff3', 
                margin: 0, 
                fontSize: '12px',
                fontFamily: 'monospace',
                whiteSpace: 'pre-wrap',
                wordBreak: 'break-word',
                maxHeight: '200px',
                overflow: 'auto'
              }}>
{JSON.stringify(
  {
    numFields: numFields,
    numParameters: numParameters,
    parameters: parameters,
    initialTime: initialTime,
    initialValues: initialValues,
    initialVelocities: initialVelocities,
  },
  null,
  2
)}
              </pre>
            </div>
            
            {/* Show potential separately if it exists */}
            {potential && potential.trim() !== "" && (
              <div style={{ marginTop: '20px', padding: '20px', backgroundColor: '#0a0a0a', borderRadius: '4px', border: '1px solid rgba(0, 191, 166, 0.3)', maxWidth: '95%' }}>
                <strong style={{ color: '#00ffc3', display: 'block', marginBottom: '12px', fontSize: '16px' }}>Potential Expression:</strong>
                <pre style={{ 
                  color: '#91fff3', 
                  margin: 0, 
                  fontSize: '12px',
                  fontFamily: 'monospace',
                  whiteSpace: 'pre-wrap',
                  wordBreak: 'break-word'
                }}>
                  {potential}
                </pre>
              </div>
            )}

            <div style={{ display: 'flex', justifyContent: 'space-between', marginTop: '20px', gap: '10px' }}>
              <button 
                className="primary-button" 
                onClick={() => setActivePage("initial")}
                disabled={isCalculating}
                style={{ 
                  backgroundColor: 'transparent',
                  color: '#00ffc3',
                  fontWeight: 'bold',
                  padding: '12px 24px',
                  fontSize: '16px',
                  border: '1px solid rgba(0, 191, 166, 0.5)',
                  cursor: isCalculating ? 'not-allowed' : 'pointer',
                  opacity: isCalculating ? 0.5 : 1
                }}
              >
                ← Back
              </button>
              <div style={{ display: 'flex', gap: '10px' }}>
                {isCalculating && (
                  <button 
                    className="primary-button" 
                    onClick={handleCancelCalculation}
                    style={{
                      backgroundColor: '#ff4444',
                      color: '#fff',
                      fontWeight: 'bold',
                      padding: '12px 24px',
                      fontSize: '16px',
                      border: 'none',
                      cursor: 'pointer'
                    }}
                  >
                    Cancel
                  </button>
                )}
                <button 
                  className="primary-button" 
                  onClick={handleRunCalculation}
                  disabled={isCalculating}
                  style={{
                    backgroundColor: isCalculating ? '#ccc' : '#4CAF50',
                    cursor: isCalculating ? 'not-allowed' : 'pointer'
                  }}
                >
                  {isCalculating ? 'Calculating...' : 'Run Calculation'}
                </button>
              </div>
            </div>
            
            {/* Display calculation results */}
            {error && (
              <div className="summary-block" style={{ 
                marginTop: '20px', 
                borderColor: '#ff4444',
                color: '#ff4444'
              }}>
                <strong>Error:</strong> {error}
              </div>
            )}
            
            {calculationResult && (
              <div className="summary-block" style={{ 
                marginTop: '30px',
                width: '100%',
                maxWidth: '98%',
                padding: '40px',
                minHeight: '600px'
              }}>
                <h3 style={{ 
                  marginTop: '0', 
                  marginBottom: '20px',
                  color: calculationResult.success ? '#00ffc3' : '#ff4444',
                  fontSize: '28px'
                }}>
                  {calculationResult.success ? '✓' : '✗'} Calculation Result
                </h3>
                
                <p style={{ marginBottom: '12px', color: '#e8fff7', fontSize: '15px' }}>
                  <strong>Status:</strong> <span style={{ color: calculationResult.success ? '#00ffc3' : '#ff4444' }}>
                    {calculationResult.success ? 'Success' : 'Failed'}
                  </span>
                </p>
                
                <p style={{ marginBottom: '12px', color: '#e8fff7', fontSize: '15px' }}>
                  <strong>Message:</strong> {calculationResult.message}
                </p>
                
                {calculationResult.executionId && (
                  <p style={{ marginBottom: '12px', fontSize: '13px', color: '#91fff3', opacity: 0.7 }}>
                    <strong>Execution ID:</strong> {calculationResult.executionId.substring(0, 8)}...
                  </p>
                )}
                
                {calculationResult.success && calculationResult.outputFiles && calculationResult.outputFiles.length > 0 && (
                  <div style={{ marginTop: '16px', paddingTop: '16px', borderTop: '1px solid rgba(0, 191, 166, 0.3)' }}>
                    <strong style={{ color: '#00ffc3', fontSize: '16px' }}>Output Files ({calculationResult.outputFiles.length}):</strong>
                    <ul style={{ marginTop: '10px', marginBottom: '0', paddingLeft: '20px', color: '#e8fff7', fontSize: '14px' }}>
                      {calculationResult.outputFiles.map((file, idx) => {
                        const isImage = file.toLowerCase().endsWith('.png') || file.toLowerCase().endsWith('.jpg') || file.toLowerCase().endsWith('.jpeg');
                        // Add cache-busting query parameter to force browser to reload the image
                        // Use executionId and timestamp to ensure unique URL for each calculation
                        // For plot files, use a more aggressive cache-busting approach
                        const isPlotFile = file.toLowerCase().includes('plot');
                        const cacheBuster = calculationResult.executionId 
                          ? `?t=${Date.now()}&id=${calculationResult.executionId.substring(0, 8)}&v=${isPlotFile ? 'plot' : 'file'}`
                          : `?t=${Date.now()}&v=${isPlotFile ? 'plot' : 'file'}`;
                        const fileUrl = `${API_BASE_URL}/api/cosmo-perturbations/files/${encodeURIComponent(file)}${cacheBuster}`;
                        
                        return (
                          <li key={idx} style={{ marginBottom: '8px' }}>
                            {isImage ? (
                              <div style={{ 
                                width: '100%', 
                                maxWidth: '600px',
                                marginTop: '20px',
                                marginBottom: '20px'
                              }}>
                                <img 
                                  src={fileUrl} 
                                  alt={file}
                                  key={`${file}-${calculationResult.executionId || Date.now()}`}
                                  style={{
                                    width: '100%',
                                    height: 'auto',
                                    maxHeight: '400px',
                                    border: '1px solid rgba(0, 191, 166, 0.3)',
                                    borderRadius: '4px',
                                    backgroundColor: '#ffffff',
                                    boxShadow: '0 2px 8px rgba(0, 191, 166, 0.2)',
                                    display: 'block',
                                    cursor: 'pointer'
                                  }}
                                  onClick={() => window.open(fileUrl, '_blank')}
                                  onError={(e) => {
                                    e.target.style.display = 'none';
                                    console.error('Failed to load image:', fileUrl);
                                  }}
                                  onLoad={() => {
                                    // Force image refresh by removing and re-adding cache-busting parameter
                                    console.log('Plot image loaded:', file);
                                  }}
                                  loading="eager"
                                />
                              </div>
                            ) : (
                              <a 
                                href={fileUrl} 
                                target="_blank" 
                                rel="noopener noreferrer"
                                style={{ color: '#00ffc3', textDecoration: 'underline' }}
                              >
                                {file}
                              </a>
                            )}
                          </li>
                        );
                      })}
                    </ul>
                  </div>
                )}
                
                {calculationResult.output && (
                  <details style={{ marginTop: '12px', paddingTop: '12px', borderTop: '1px solid rgba(0, 191, 166, 0.3)' }}>
                    <summary style={{ 
                      cursor: 'pointer', 
                      color: '#00ffc3',
                      fontSize: '13px'
                    }}>
                      View Output
                    </summary>
                    <pre style={{ 
                      backgroundColor: '#0a0a0a', 
                      color: '#91fff3',
                      padding: '12px', 
                      overflow: 'auto', 
                      maxHeight: '400px',
                      borderRadius: '4px',
                      marginTop: '8px',
                      fontSize: '11px',
                      fontFamily: 'monospace',
                      border: '1px solid rgba(0, 191, 166, 0.3)'
                    }}>
                      {calculationResult.output}
                    </pre>
                  </details>
                )}
              </div>
            )}
            <p className="hint-text">
              Right now it works  without change of metric. For more fields, the code maybe be crash because of server limitations.
              Later these issues will be fixed. 
            </p>
          </section>
        )}

        {/* Footer */}
        <footer style={{ 
          textAlign: "center", 
          marginTop: "auto", 
          paddingTop: "40px",
          fontSize: "18px"
        }}>
          <span style={{ color: "#00F5C4" }}>Created By{" "}</span>
          <a 
            href="https://ioanna-stamou.com/" 
            target="_blank" 
            rel="noopener noreferrer"
            style={{
              color: "#00F5C4",
              textDecoration: "none",
              transition: "opacity 0.2s ease"
            }}
            onMouseEnter={(e) => e.target.style.opacity = "0.7"}
            onMouseLeave={(e) => e.target.style.opacity = "1"}
          >
            Ioanna Stamou
          </a>
        </footer>

      </main>
    </div>
  );
}

// We export the App component so main.jsx can render it.
export default App;
