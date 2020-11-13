// if97_h3ab stub - Interfaces CoolProp IF97 function to Mathcad
//

// this code executes the user function if97_h3ab(P), which is a wrapper for
// the CoolProp-IF97 function, h3ab(P), used to calculate the 
// enthalpy along the Region 3a/3b boundary as a function of pressure
LRESULT  if97_H3AB(
    LPCOMPLEXSCALAR c,  // pointer to the result
    LPCCOMPLEXSCALAR a) // pointer to the parameter received from Mathcad
{  
    // first check to make sure "a" has no imaginary component
    if ( a->imag != 0.0 )
        return MAKELRESULT(MUST_BE_REAL,1);

    if ( (a->real < IF97::Pcrit) || (a->real > IF97::Pmax) )
        return MAKELRESULT(P_OUT_OF_RANGE,1);

    //otherwise, all is well, evaluate function
    c->real = IF97::Backwards::H3ab_p(a->real);

    // normal return
    return 0;
}

FUNCTIONINFO    if97_h3ab = 
{
    "if97_h3ab",                     // name by which Mathcad will recognize the function
    "p",                             // if97_h3ab will be called as if97_h3ab(p)
    // description of if97_t23(p)
    "Obtains Enthalpy [J/kg] along the Region 3a/3b boundary as a function of pressure [Pa].",
    (LPCFUNCTION)if97_H3AB,          // pointer to executable code
    COMPLEX_SCALAR,                  // the return type is a complex scalar
    1,                               // there is only one input parameter
    { COMPLEX_SCALAR }               //    it is a complex scalar
};