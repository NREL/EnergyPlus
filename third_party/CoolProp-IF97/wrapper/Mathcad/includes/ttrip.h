// if97_ttrip stub - Interfaces CoolProp IF97 function to Mathcad
//

// this code executes the user function if97_ttrip(n), which returns
// the triple point temperature.  There is one dummy parameter, which
// is not used to return the triple point value.
LRESULT  if97_Ttrip(
    LPCOMPLEXSCALAR c,  // pointer to the result
    LPCCOMPLEXSCALAR a) // pointer to the parameter received from Mathcad
{  
    // stuff result into return scalar structure
    c->real = IF97::Ttrip;

    // normal return
    return 0;
}

FUNCTIONINFO    if97_ttrip = 
{
    "if97_ttrip",                    // name by which Mathcad will recognize the function
    "d",                             // if97_ttrip will be called as if97_ttrip(d)
    // description of if97_ttrip(d)
    "Obtains saturation temperature in K at the triple point. There is one dummy parameter, d, which is not used and can be zero.",
    (LPCFUNCTION)if97_Ttrip,         // pointer to executable code
    COMPLEX_SCALAR,                  // the return type is a complex scalar
    1,                               // there is only one input parameter
    { COMPLEX_SCALAR }               //    it is a complex scalar
};