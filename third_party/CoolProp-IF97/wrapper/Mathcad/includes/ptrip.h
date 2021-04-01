// if97_ptrip stub - Interfaces CoolProp IF97 function to Mathcad
//

// this code executes the user function if97_ptrip(n), which returns
// the triple point pressure.  There is one dummy parameter, which
// is not used to return the triple point value.
LRESULT  if97_Ptrip(
    LPCOMPLEXSCALAR c,  // pointer to the result
    LPCCOMPLEXSCALAR a) // pointer to the parameter received from Mathcad
{  
    // stuff result into return scalar structure
    c->real = IF97::Ptrip;

    // normal return
    return 0;
}

FUNCTIONINFO    if97_ptrip = 
{
    "if97_ptrip",                    // name by which Mathcad will recognize the function
    "d",                             // if97_ptrip will be called as if97_ptrip(d)
    // description of if97_ptrip(d)
    "Obtains saturation pressure in K at the triple point. There is one dummy parameter, d, which is not used and can be zero.",
    (LPCFUNCTION)if97_Ptrip,         // pointer to executable code
    COMPLEX_SCALAR,                  // the return type is a complex scalar
    1,                               // there is only one input parameter
    { COMPLEX_SCALAR }               //    it is a complex scalar
};