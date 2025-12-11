// we import react usestate sp we can store changing values

import {useState} from "react";
import "./App.css";

//this is the main component of my app
function App(){
// which page is currently vissible "model", "initial", "summary" or "about"

const [activatePage, setActivePage] = useState ("model");

//Number of scalar fields
const [numFields, setNumFields]= useState(1);
//Potential V
const [potential, setPotential] = useState("0.5 * m^2 * phi1^2");

//Array for intial values and velocities
const[initialValues, setInitialValues] = useState ([""]);
const[initialVelocities, setInitialVelocities]= useState ([""]);

function handleNumFieldChange(event){
  const valueAsString = event.target.value;
  
  // Convert to integer 
  let n = parseInt(valueAsString, 10);
  //if incersion fails send n=1
  if (isNan(n) || n<1){
    n=1;
  }
 // update initial values to have lenght
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
}












 