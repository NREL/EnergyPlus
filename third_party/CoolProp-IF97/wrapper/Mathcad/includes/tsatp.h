// if97_tsatp stub - Interfaces CoolProp IF97 function to Mathcad
//

// this code executes the user function if97_tsatp(P), which is a wrapper for
// the CoolProp-IF97 function, Tsat97(P), used to calculate the saturation
// temperature along the saturation curve in terms of pressure
LRESULT  if97_TsatP(
    LPCOMPLEXSCALAR c,  // pointer to the result
    LPCCOMPLEXSCALAR a) // pointer to the parameter received from Mathcad
{  
    // first check to make sure "a" has no imaginary component
    if ( a->imag != 0.0 )
        return MAKELRESULT(MUST_BE_REAL,1);

    try { 
        c->real = IF97::Tsat97(a->real);
    }
    catch ( const std::out_of_range& ) {
        return MAKELRESULT(P_OUT_OF_RANGE,1);
    }

    // normal return
    return 0;
}

FUNCTIONINFO    if97_tsatp = 
{
    "if97_tsatp",                    // name by which Mathcad will recognize the function
    "p",                             // if97_tsatp will be called as if97_tsatp(p)
    // description of if97_tsatp(p)
    "Obtains saturation temperature in K as a function of pressure, P, in Pa.",
    (LPCFUNCTION)if97_TsatP,         // pointer to executable code
    COMPLEX_SCALAR,                  // the return type is a complex scalar
    1,                               // there is only one input parameter
    { COMPLEX_SCALAR }               //    it is a complex scalar
};