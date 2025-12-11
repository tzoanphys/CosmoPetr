// src/App.jsx
// We import React's useState so we can store changing values.
import { useState } from "react";
import "./App.css";

function App() {
  // Which page is currently visible: "model", "initial", "summary", or "about"
  const [activePage, setActivePage] = useState("about");
  // Number of scalar fields
  const [numFields, setNumFields] = useState(1);
  // Potential V(phi)
  const [potential, setPotential] = useState("0.5 * m^2 * phi1^2");
  // Arrays for initial values and velocities of each field
  const [initialValues, setInitialValues] = useState([""]);
  const [initialVelocities, setInitialVelocities] = useState([""]);
  // Initial time (for example N = 0 or t = 0)
  const [initialTime, setInitialTime] = useState("0.0");

  // ----------------------------------------------------
  //  update number of fields AND keep arrays in sync
  // ----------------------------------------------------
  function handleNumFieldsChange(event) {
    // event.target.value is a string from the input
    const valueAsString = event.target.value;

    // Convert to integer
    let n = parseInt(valueAsString, 10);

    // If conversion fails or n < 1, set n = 1
    if (isNaN(n) || n < 1) {
      n = 1;
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
  // When user clicks the "Preview configuration" button
  // ----------------------------------------------------
  function handlePreviewConfig() {
    const config = {
      numFields: numFields,
      potential: potential,
      initialTime: initialTime,
      initialValues: initialValues,
      initialVelocities: initialVelocities,
    };

    // Show the configuration in the browser console
    console.log("Cosmological perturbation config:", config);

    // Show a simple message on the screen
    alert("Parameters collected! (Next step: connect the Fortran backend.)");
  }

  // ----------------------------------------------------
  // JSX: what appears on the screen
  // ----------------------------------------------------
  return (
    <div className="app">
      {/* ================== LEFT SIDEBAR ================== */}
      <aside className="sidebar">
        <div style={{ textAlign: "center", marginBottom: "20px" }}>
          <img 
            src="/unnamed.jpg" 
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
            wiil give you the power spectra and observables.
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
          </section>
        )}

        {/* ================== PAGE: MODEL SETUP ================== */}
        {activePage === "model" && (
          <section className="section">
            <h2 className="section-title">1. Model setup</h2>
            <p className="section-text">
              Choose the number of scalar fields and specify the potential
              V(φᵢ). You can use a simple math-like syntax (for example{" "}
              <code>0.5*m^2*phi1^2 + lambda*phi1^4</code>).
            </p>

            <div className="form-grid">
              {/* Number of fields input */}
              <label className="form-field">
                <span className="field-label">Number of fields</span>
                <input
                  type="number"
                  min="1"
                  value={numFields}
                  onChange={handleNumFieldsChange}
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
          </section>
        )}

        {/* ================== PAGE: SUMMARY / EXPORT ================== */}
        {activePage === "summary" && (
          <section className="section">
            <h2 className="section-title">3. Summary / Export</h2>
            <p className="section-text">
              Here is a quick preview of the configuration that will be sent to
              the backend solver.
            </p>

            {/* Show all data in JSON format */}
            <pre className="summary-block">
{JSON.stringify(
  {
    numFields: numFields,
    potential: potential,
    initialTime: initialTime,
    initialValues: initialValues,
    initialVelocities: initialVelocities,
  },
  null,
  2
)}
            </pre>

            <button className="primary-button" onClick={handlePreviewConfig}>
              Preview configuration (console log)
            </button>
            <p className="hint-text">
              Later, this button will send the data to your Fortran backend and
              show power spectra / observables here.
            </p>
          </section>
        )}

        {/* Footer */}
        <footer style={{ 
          textAlign: "center", 
          marginTop: "auto", 
          paddingTop: "40px",
          color: "#8af9dd",
          fontSize: "14px",
          opacity: 0.8
        }}>
          Created By{" "}
          <a 
            href="https://ioanna-stamou.com/" 
            target="_blank" 
            rel="noopener noreferrer"
            style={{
              color: "#00ffc3",
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
