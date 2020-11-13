// if97_xpu stub - Interfaces CoolProp IF97 function to Mathcad
//

// this code executes the user function if97_xpu(p,u), which is a wrapper for
// the CoolProp-IF97 function, Q_pumass(p,u).
LRESULT  if97_XPU(
    LPCOMPLEXSCALAR x,  // pointer to the result
    LPCCOMPLEXSCALAR p,
    LPCCOMPLEXSCALAR u) // pointer to the parameter received from Mathcad
{  
    // first check to make sure "p" and "x" have no imaginary component
    if ( p->imag != 0.0 )
        return MAKELRESULT(MUST_BE_REAL,1);
    if ( u->imag != 0.0 )
        return MAKELRESULT(MUST_BE_REAL,2);

    //otherwise, all is well, evaluate function
    try {
        x->real = IF97::Q_pumass(p->real,u->real);
    }
    catch (const std::out_of_range& e) { 
        if (e.what()[0] == 'P')
            return MAKELRESULT(P_OUT_OF_RANGE,1);
        else
            return MAKELRESULT(UNKNOWN,1);
    }
    // normal return
    return 0;
}

FUNCTIONINFO    if97_xpu = 
{
    "if97_xpu",                         // name by which Mathcad will recognize the function
    "p,u",                              // if97_xpu will be called as if97_xpu(p,u)
    // description of if97_xpu(p,u)
    "Obtains the steam quality, x, as a function of pressure, p [Pa], and mass internal energy [J/kg].",
    (LPCFUNCTION)if97_XPU,              // pointer to executable code
    COMPLEX_SCALAR,                     // the return type is a complex scalar
    2,                                  // there are two input parameters
    { COMPLEX_SCALAR, COMPLEX_SCALAR }  //    that are both complex scalars
};