// if97_psatt stub - Interfaces CoolProp IF97 function to Mathcad
//

// this code executes the user function if97_psatt(P), which is a wrapper for
// the CoolProp-IF97 function, Tsat97(P), used to calculate the saturation
// temperature along the saturation curve in terms of pressure
LRESULT  if97_PsatT(
    LPCOMPLEXSCALAR c,  // pointer to the result
    LPCCOMPLEXSCALAR a) // pointer to the parameter received from Mathcad
{  
    // first check to make sure "a" has no imaginary component
    if ( a->imag != 0.0 )
        return MAKELRESULT(MUST_BE_REAL,1);

    //otherwise, all is well, evaluate function
    try { 
        c->real = IF97::psat97(a->real);
    }
    catch ( const std::out_of_range& ) {
        return MAKELRESULT(T_OUT_OF_RANGE,1);
    }

    // normal return
    return 0;
}

FUNCTIONINFO    if97_psatt = 
{
    "if97_psatt",                    // name by which Mathcad will recognize the function
    "T",                             // if97_psatt will be called as if97_psatt(p)
    // description of if97_psatt(p)
    "Obtains saturation pressure in Pa as a function of temperature, T, in K.",
    (LPCFUNCTION)if97_PsatT,         // pointer to executable code
    COMPLEX_SCALAR,                  // the return type is a complex scalar
    1,                               // there is only one input parameter
    { COMPLEX_SCALAR }               //    it is a complex scalar
};