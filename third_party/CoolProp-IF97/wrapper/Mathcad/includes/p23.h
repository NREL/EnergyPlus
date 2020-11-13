// if97_p23 stub - Interfaces CoolProp IF97 function to Mathcad
//

// this code executes the user function if97_p23(P), which is a wrapper for
// the CoolProp-IF97 function, Region23_T(T), used to calculate the 
// pressure along the Region23 curve in terms of temperature
LRESULT  if97_P23(
    LPCOMPLEXSCALAR c,  // pointer to the result
    LPCCOMPLEXSCALAR a) // pointer to the parameter received from Mathcad
{  
    // first check to make sure "a" has no imaginary component
    if ( a->imag != 0.0 )
        return MAKELRESULT(MUST_BE_REAL,1);

    if ( (a->real < 623.15) || (a->real > 863.15) )
        return MAKELRESULT(T_OUT_OF_RANGE,1);

    //otherwise, all is well, evaluate function
    c->real = IF97::Region23_T(a->real);

    // normal return
    return 0;
}

FUNCTIONINFO    if97_p23 = 
{
    "if97_p23",                    // name by which Mathcad will recognize the function
    "T",                             // if97_p23 will be called as if97_p23(p)
    // description of if97_p23(p)
    "Obtains pressure [Pa] along the Region2/3 boundary as a function of temperature [K].",
    (LPCFUNCTION)if97_P23,         // pointer to executable code
    COMPLEX_SCALAR,                  // the return type is a complex scalar
    1,                               // there is only one input parameter
    { COMPLEX_SCALAR }               //    it is a complex scalar
};