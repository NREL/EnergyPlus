// if97_upx stub - Interfaces CoolProp IF97 function to Mathcad
//

// this code executes the user function if97_upx(p,x), which is a wrapper for
// the CoolProp-IF97 function, umass_pQ(p,Q).
LRESULT  if97_UPX(
    LPCOMPLEXSCALAR u,  // pointer to the result
    LPCCOMPLEXSCALAR p,
    LPCCOMPLEXSCALAR x) // pointer to the parameter received from Mathcad
{  
    // first check to make sure "p" and "x" have no imaginary component
    if ( p->imag != 0.0 )
        return MAKELRESULT(MUST_BE_REAL,1);
    if ( x->imag != 0.0 )
        return MAKELRESULT(MUST_BE_REAL,2);

    //otherwise, all is well, evaluate function
    try {
        u->real = IF97::umass_pQ(p->real,x->real);
    }
    catch (const std::out_of_range& e) { 
        if (e.what()[0] == 'P')
            return MAKELRESULT(P_OUT_OF_RANGE,1);
        else if (e.what()[0] == 'Q')
            return MAKELRESULT(Q_OUT_OF_RANGE,2);
        else
            return MAKELRESULT(UNKNOWN,1);
    }
    // normal return
    return 0;
}

FUNCTIONINFO    if97_upx = 
{
    "if97_upx",                         // name by which Mathcad will recognize the function
    "p,x",                              // if97_upx will be called as if97_upx(p,x)
    // description of if97_upx(p,x)
    "Obtains the mass internal energy [J/kg] as a function of pressure, p [Pa], and steam quality, x.",
    (LPCFUNCTION)if97_UPX,              // pointer to executable code
    COMPLEX_SCALAR,                     // the return type is a complex scalar
    2,                                  // there are two input parameters
    { COMPLEX_SCALAR, COMPLEX_SCALAR }  //    that are both complex scalars
};