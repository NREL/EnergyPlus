// if97_hmins stub - Interfaces CoolProp IF97 function to Mathcad
//

// this code executes the user function if97_hmins(S), which is a wrapper for
// the CoolProp-IF97 function, Hmin(s), used to calculate the 
// enthalpy along the Pmin boundary as a function of entropy
LRESULT  if97_HMINS(
    LPCOMPLEXSCALAR c,  // pointer to the result
    LPCCOMPLEXSCALAR a) // pointer to the parameter received from Mathcad
{  
    // first check to make sure "a" has no imaginary component
    if ( a->imag != 0.0 )
        return MAKELRESULT(MUST_BE_REAL,1);

    if ( (a->real < IF97::Smin) || (a->real > IF97::Smax) )
        return MAKELRESULT(S_OUT_OF_RANGE,1);

    //otherwise, all is well, evaluate function
    c->real = IF97::Hmin(a->real);

    // normal return
    return 0;
}

FUNCTIONINFO    if97_hmins = 
{
    "if97_hmins",                     // name by which Mathcad will recognize the function
    "s",                             // if97_hmins will be called as if97_hmins(s)
    // description of if97_t23(p)
    "Obtains Enthalpy [J/kg] along the Pmin boundary as a function of Entropy [J/kg-K].",
    (LPCFUNCTION)if97_HMINS,          // pointer to executable code
    COMPLEX_SCALAR,                  // the return type is a complex scalar
    1,                               // there is only one input parameter
    { COMPLEX_SCALAR }               //    it is a complex scalar
};