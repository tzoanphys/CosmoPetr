// src/App.jsx
import './App.css'

function App() {
  return (
    <div className="app">
      {/* LEFT MENU */}
      <aside className="sidebar">
        <div className="menu-title">Menu</div>

        <nav className="menu-list">
          <button className="menu-item active">About &amp; Expertise</button>
          <button className="menu-item">Contact</button>
          <button className="menu-item">Working Experience</button>
          <button className="menu-item">Education</button>
          <button className="menu-item">Projects</button>
        </nav>
      </aside>

      {/* MAIN CONTENT */}
      <main className="content">
        {/* TOP BANNER */}
        <section className="header-banner">
          <h1 className="name-title">Cosmological Perturbations Analysis</h1>
        </section>

        {/* ABOUT ME */}
        <section className="section">
          <h2 className="section-title">Stamou Ioanna</h2>
          <p className="section-text">
           This is an application that allows you to analyze cosmological perturbations. It is now under construction.
          </p>
        </section>


      </main>
    </div>
  )
}

export default App
