// if97_xpv stub - Interfaces CoolProp IF97 function to Mathcad
//

// this code executes the user function if97_xpv(p,v), which is a wrapper for
// the CoolProp-IF97 function, Q_prhomass(p,v).
LRESULT  if97_XPV(
    LPCOMPLEXSCALAR x,  // pointer to the result
    LPCCOMPLEXSCALAR p,
    LPCCOMPLEXSCALAR v) // pointer to the parameter received from Mathcad
{  
    // first check to make sure "p" and "rho" have no imaginary component
    if ( p->imag != 0.0 )
        return MAKELRESULT(MUST_BE_REAL,1);
    if ( v->imag != 0.0 )
        return MAKELRESULT(MUST_BE_REAL,2);

    //otherwise, all is well, evaluate function
    try {
        x->real = IF97::Q_pv(p->real,v->real);
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

FUNCTIONINFO    if97_xpv = 
{
    "if97_xpv",                           // name by which Mathcad will recognize the function
    "p,v",                                // if97_xpv will be called as if97_xpv(p,v)
    // description of if97_xpv(p,v)
    "Obtains the steam quality, x, as a function of pressure, p [Pa], and specific volume [m³/kg].",
    (LPCFUNCTION)if97_XPV,                // pointer to executable code
    COMPLEX_SCALAR,                       // the return type is a complex scalar
    2,                                    // there are two input parameters
    { COMPLEX_SCALAR, COMPLEX_SCALAR }    //    that are both complex scalars
};