// if97_xprho stub - Interfaces CoolProp IF97 function to Mathcad
//

// this code executes the user function if97_xprho(p,rho), which is a wrapper for
// the CoolProp-IF97 function, Q_prhomass(p,rho).
LRESULT  if97_XPRHO(
    LPCOMPLEXSCALAR x,  // pointer to the result
    LPCCOMPLEXSCALAR p,
    LPCCOMPLEXSCALAR rho) // pointer to the parameter received from Mathcad
{  
    // first check to make sure "p" and "rho" have no imaginary component
    if ( p->imag != 0.0 )
        return MAKELRESULT(MUST_BE_REAL,1);
    if ( rho->imag != 0.0 )
        return MAKELRESULT(MUST_BE_REAL,2);

    //otherwise, all is well, evaluate function
    try {
        x->real = IF97::Q_prhomass(p->real,rho->real);
    }
    catch (const std::out_of_range& e) { 
        if (e.what()[0] == 'P')
            return MAKELRESULT(P_OUT_OF_RANGE,1);
        else if (e.what()[0] == 'D')
            return MAKELRESULT(D_OUT_OF_RANGE,2);
        else
            return MAKELRESULT(UNKNOWN,1);
    }
    // normal return
    return 0;
}

FUNCTIONINFO    if97_xprho = 
{
    "if97_xprho",                         // name by which Mathcad will recognize the function
    "p,rho",                              // if97_xprho will be called as if97_xprho(p,rho)
    // description of if97_xprho(p,rho)
    "Obtains the steam quality, x, as a function of pressure, p [Pa], and mass density [kg/m³].",
    (LPCFUNCTION)if97_XPRHO,              // pointer to executable code
    COMPLEX_SCALAR,                       // the return type is a complex scalar
    2,                                    // there are two input parameters
    { COMPLEX_SCALAR, COMPLEX_SCALAR }    //    that are both complex scalars
};