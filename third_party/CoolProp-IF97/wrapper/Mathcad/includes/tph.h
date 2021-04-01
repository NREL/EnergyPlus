// if97_tph stub - Interfaces CoolProp IF97 function to Mathcad
//

// this code executes the user function if97_tph(P,h), which is a wrapper for
// the CoolProp-IF97 function, T_phmass(p,h).
LRESULT  if97_TPH(
    LPCOMPLEXSCALAR c,  // pointer to the result
    LPCCOMPLEXSCALAR a,
    LPCCOMPLEXSCALAR b) // pointer to the parameter received from Mathcad
{  
    // first check to make sure "a" and "b" have no imaginary component
    if ( a->imag != 0.0 )
        return MAKELRESULT(MUST_BE_REAL,1);
    if ( b->imag != 0.0 )
        return MAKELRESULT(MUST_BE_REAL,2);

    //otherwise, all is well, evaluate function
    try {
        c->real = IF97::T_phmass(a->real,b->real);
    }
    catch (const std::out_of_range& e) { 
        if (e.what()[0] == 'P') 
            return MAKELRESULT(P_OUT_OF_RANGE,1);
        else // (e.what == "H")
            return MAKELRESULT(H_OUT_OF_RANGE,2);
    }
    catch (const std::logic_error&) {
        return MAKELRESULT(NO_SOLUTION_FOUND,1);
    }
    // normal return
    return 0;
}

FUNCTIONINFO    if97_tph = 
{
    "if97_tph",                      // name by which Mathcad will recognize the function
    "p,h",                           // if97_tph will be called as if97_tph(p,h)
    // description of if97_tph(p,h)
    "Obtains the temperature, T [K], as a function of p [Pa] and mass enthalpy, h, [J/kg].",
    (LPCFUNCTION)if97_TPH,           // pointer to executable code
    COMPLEX_SCALAR,                  // the return type is a complex scalar
    2,                               // there are two input parameters
    { COMPLEX_SCALAR, COMPLEX_SCALAR }               //    that are both complex scalars
};