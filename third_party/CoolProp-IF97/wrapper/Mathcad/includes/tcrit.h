// if97_tcrit stub - Interfaces CoolProp IF97 function to Mathcad
//

// this code executes the user function if97_tcrit(n), which returns
// the triple point temperature.  There is one dummy parameter, which
// is not used to return the triple point value.
LRESULT  if97_Tcrit(
    LPCOMPLEXSCALAR c,  // pointer to the result
    LPCCOMPLEXSCALAR a) // pointer to the parameter received from Mathcad
{  
    // stuff result into return scalar structure
    c->real = IF97::Tcrit;

    // normal return
    return 0;
}

FUNCTIONINFO    if97_tcrit = 
{
    "if97_tcrit",                    // name by which Mathcad will recognize the function
    "d",                             // if97_tcrit will be called as if97_tcrit(d)
    // description of if97_tcrit(d)
    "Obtains saturation temperature in K at the critical point. There is one dummy parameter, d, which is not used and can be zero.",
    (LPCFUNCTION)if97_Tcrit,         // pointer to executable code
    COMPLEX_SCALAR,                  // the return type is a complex scalar
    1,                               // there is only one input parameter
    { COMPLEX_SCALAR }               //    it is a complex scalar
};