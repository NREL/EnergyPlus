// if97_h13s stub - Interfaces CoolProp IF97 function to Mathcad
//
// this code executes the user function if97_h13s(S), which is a wrapper for
// the CoolProp-IF97 function, H13_s(s), used to calculate the 
// enthalpy along the Region 1/3 boundary as a function of entropy
LRESULT  if97_H13S(
    LPCOMPLEXSCALAR c,  // pointer to the result
    LPCCOMPLEXSCALAR a) // pointer to the parameter received from Mathcad
{  
    // first check to make sure "a" has no imaginary component
    if ( a->imag != 0.0 )
        return MAKELRESULT(MUST_BE_REAL,1);


if ( ( (IF97::S13min - a->real) > (IF97::R_fact*1.0E-9) ) || ( (a->real-IF97::SfT23) > (IF97::R_fact*1.0E-9) ) )
        return MAKELRESULT(S_OUT_OF_RANGE,1);

    //otherwise, all is well, evaluate function
    c->real = IF97::Backwards::H13_s(a->real);

    // normal return
    return 0;
}

FUNCTIONINFO    if97_h13s = 
{
    "if97_h13s",                     // name by which Mathcad will recognize the function
    "s",                             // if97_h13s will be called as if97_h13s(s)
    // description of if97_t23(p)
    "Obtains Enthalpy [J/kg] along the Region 1/3 boundary as a function of Entropy [J/kg-K].",
    (LPCFUNCTION)if97_H13S,          // pointer to executable code
    COMPLEX_SCALAR,                  // the return type is a complex scalar
    1,                               // there is only one input parameter
    { COMPLEX_SCALAR }               //    it is a complex scalar
};