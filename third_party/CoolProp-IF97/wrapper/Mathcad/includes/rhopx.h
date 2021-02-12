// if97_rhopx stub - Interfaces CoolProp IF97 function to Mathcad
//

// this code executes the user function if97_rhopx(p,x), which is a wrapper for
// the CoolProp-IF97 function, rhomass_pQ(p,Q).
LRESULT  if97_RHOPX(
    LPCOMPLEXSCALAR rho,  // pointer to the result
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
        rho->real = IF97::rhomass_pQ(p->real,x->real);
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

FUNCTIONINFO    if97_rhopx = 
{
    "if97_rhopx",                         // name by which Mathcad will recognize the function
    "p,x",                                // if97_rhopx will be called as if97_rhopx(p,x)
    // description of if97_rhopx(p,x)
    "Obtains the mixture density [kg/m³] as a function of pressure, p [Pa], and steam quality, x.",
    (LPCFUNCTION)if97_RHOPX,              // pointer to executable code
    COMPLEX_SCALAR,                       // the return type is a complex scalar
    2,                                    // there are two input parameters
    { COMPLEX_SCALAR, COMPLEX_SCALAR }    //    that are both complex scalars
};