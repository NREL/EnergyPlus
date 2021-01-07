// if97_h2ab stub - Interfaces CoolProp IF97 function to Mathcad
//
// this code executes the user function if97_h2ab(S), which is a wrapper for
// the CoolProp-IF97 function, H2ab_s(s), used to calculate the 
// enthalpy along the Sub-region 2a/2b boundary as a function of entropy
LRESULT  if97_H2AB(
    LPCOMPLEXSCALAR c,  // pointer to the result
    LPCCOMPLEXSCALAR a) // pointer to the parameter received from Mathcad
{  
    // first check to make sure "a" has no imaginary component
    if ( a->imag != 0.0 )
        return MAKELRESULT(MUST_BE_REAL,1);

    // No range checking!

    //otherwise, all is well, evaluate function
    c->real = IF97::Backwards::H2ab_s(a->real);

    // normal return
    return 0;
}

FUNCTIONINFO    if97_h2ab = 
{
    "if97_h2ab",                     // name by which Mathcad will recognize the function
    "s",                             // if97_h2ab will be called as if97_h2ab(s)
    // description of if97_h2ab(p)
    "Obtains Enthalpy [J/kg] along the Sub-region 2a/2b boundary as a function of Entropy [J/kg-K].",
    (LPCFUNCTION)if97_H2AB,          // pointer to executable code
    COMPLEX_SCALAR,                  // the return type is a complex scalar
    1,                               // there is only one input parameter
    { COMPLEX_SCALAR }               //    it is a complex scalar
};