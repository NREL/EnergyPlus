// if97_xps stub - Interfaces CoolProp IF97 function to Mathcad
//

// this code executes the user function if97_xps(p,s), which is a wrapper for
// the CoolProp-IF97 function, Q_psmass(p,s).
LRESULT  if97_XPS(
    LPCOMPLEXSCALAR x,  // pointer to the result
    LPCCOMPLEXSCALAR p,
    LPCCOMPLEXSCALAR s) // pointer to the parameter received from Mathcad
{  
    // first check to make sure "p" and "s" have no imaginary component
    if ( p->imag != 0.0 )
        return MAKELRESULT(MUST_BE_REAL,1);
    if ( s->imag != 0.0 )
        return MAKELRESULT(MUST_BE_REAL,2);

    //otherwise, all is well, evaluate function
    try {
        x->real = IF97::Q_psmass(p->real,s->real);
    }
    catch (const std::out_of_range& e) { 
        if (e.what()[0] == 'P')
            return MAKELRESULT(P_OUT_OF_RANGE,1);
        else if ((e.what()[0] == 'E') && (e.what()[3] == 'r'))
            return MAKELRESULT(S_OUT_OF_RANGE,2);
        else
            return MAKELRESULT(UNKNOWN,1);
    }
    // normal return
    return 0;
}

FUNCTIONINFO    if97_xps = 
{
    "if97_xps",                         // name by which Mathcad will recognize the function
    "p,s",                              // if97_xps will be called as if97_xps(p,s)
    // description of if97_xps(p,s)
    "Obtains the steam quality, x, as a function of pressure, p [Pa], and mass entropy [J/kg-K].",
    (LPCFUNCTION)if97_XPS,              // pointer to executable code
    COMPLEX_SCALAR,                     // the return type is a complex scalar
    2,                                  // there are two input parameters
    { COMPLEX_SCALAR, COMPLEX_SCALAR }  //    that are both complex scalars
};