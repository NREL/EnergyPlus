// if97_rhoph stub - Interfaces CoolProp IF97 function to Mathcad
//

// this code executes the user function if97_rhoph(P), which is a wrapper for
// the CoolProp-IF97 function, rhomass_phmass(P,H), used to calculate the density
// in terms of pressure and enthalpy
LRESULT  if97_RhoPH(
    LPCOMPLEXSCALAR c,  // pointer to the result (density)
    LPCCOMPLEXSCALAR a, // pointer to the pressure parameter received from Mathcad
    LPCCOMPLEXSCALAR b) // pointer to the enthalpy parameter received from Mathcad
{  
    // first check to make sure "a" and "b" have no imaginary component
    if ( a->imag != 0.0 )
        return MAKELRESULT(MUST_BE_REAL,1);
    if ( b->imag != 0.0 )
        return MAKELRESULT(MUST_BE_REAL,2);

    //otherwise, all is well, evaluate function
    try {
        c->real = IF97::rhomass_phmass(a->real,b->real);
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

FUNCTIONINFO    if97_rhoph = 
{
    "if97_rhoph",                        // name by which Mathcad will recognize the function
    "p,h",                               // if97_RhoPH will be called as if97_rhoph(p,h)
    // description of if97_rhoph(p)
    "Obtains the mass density [kg/m^3] as a function of p [Pa] and h [J/kg].",
    (LPCFUNCTION)if97_RhoPH,             // pointer to executable code
    COMPLEX_SCALAR,                      // the return type is a complex scalar
    2,                                   // there are two input parameters
    { COMPLEX_SCALAR, COMPLEX_SCALAR }   //    that are both complex scalars
};