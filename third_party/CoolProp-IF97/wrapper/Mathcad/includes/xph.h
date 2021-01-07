// if97_xph stub - Interfaces CoolProp IF97 function to Mathcad
//

// this code executes the user function if97_xph(p,h), which is a wrapper for
// the CoolProp-IF97 function, Q_phmass(p,h).
LRESULT  if97_XPH(
    LPCOMPLEXSCALAR x,  // pointer to the result
    LPCCOMPLEXSCALAR p,
    LPCCOMPLEXSCALAR h) // pointer to the parameter received from Mathcad
{  
    // first check to make sure "p" and "h" have no imaginary component
    if ( p->imag != 0.0 )
        return MAKELRESULT(MUST_BE_REAL,1);
    if ( h->imag != 0.0 )
        return MAKELRESULT(MUST_BE_REAL,2);

    //otherwise, all is well, evaluate function
    try {
        x->real = IF97::Q_phmass(p->real,h->real);
    }
    catch (const std::out_of_range& e) { 
        if (e.what()[0] == 'P')
            return MAKELRESULT(P_OUT_OF_RANGE,1);
        else if ((e.what()[0] == 'E') && (e.what()[3] == 'h'))
            return MAKELRESULT(H_OUT_OF_RANGE,2);
        else
            return MAKELRESULT(UNKNOWN,1);
    }
    // normal return
    return 0;
}

FUNCTIONINFO    if97_xph = 
{
    "if97_xph",                         // name by which Mathcad will recognize the function
    "p,h",                              // if97_xph will be called as if97_xph(p,h)
    // description of if97_xph(p,h)
    "Obtains the steam quality, x, as a function of pressure, p [Pa], and mass enthalpy [J/kg].",
    (LPCFUNCTION)if97_XPH,              // pointer to executable code
    COMPLEX_SCALAR,                     // the return type is a complex scalar
    2,                                  // there are two input parameters
    { COMPLEX_SCALAR, COMPLEX_SCALAR }  //    that are both complex scalars
};