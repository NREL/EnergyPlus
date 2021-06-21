// if97_sigma stub - Interfaces CoolProp IF97 function to Mathcad
//

// this code executes the user function if97_sigma(T), which is a wrapper for
// the CoolProp-IF97 function, sigma97(P), used to calculate the surface
// tension along the saturation curve in terms of Temperature
LRESULT  if97_SIGMAT(
    LPCOMPLEXSCALAR c,  // pointer to the result
    LPCCOMPLEXSCALAR a) // pointer to the parameter received from Mathcad
{  
    // first check to make sure "a" has no imaginary component
    if ( a->imag != 0.0 )
        return MAKELRESULT(MUST_BE_REAL,1);

    //otherwise, all is well, evaluate function
    try { 
        c->real = IF97::sigma97(a->real);
    }
    catch ( const std::out_of_range& ) {
        return MAKELRESULT(T_OUT_OF_RANGE,1);
    }

    // normal return
    return 0;
}

FUNCTIONINFO    if97_sigma = 
{
    "if97_sigma",                    // name by which Mathcad will recognize the function
    "T",                             // if97_sigma will be called as if97_sigma(p)
    // description of if97_sigma(T)
    "Obtains the surface tension [N/m] as a function of temperature, T [K].",
    (LPCFUNCTION)if97_SIGMAT,        // pointer to executable code
    COMPLEX_SCALAR,                  // the return type is a complex scalar
    1,                               // there is only one input parameter
    { COMPLEX_SCALAR }               //    it is a complex scalar
};