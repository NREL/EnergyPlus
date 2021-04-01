// if97_vtp stub - Interfaces CoolProp IF97 function to Mathcad
//

// this code executes the user function if97_vtp(P), which is a wrapper for
// the CoolProp-IF97 function, Tsat97(P), used to calculate the specific
// volume at the temperature and pressure state point
LRESULT  if97_VTP(
    LPCOMPLEXSCALAR c,  // pointer to the result
    LPCCOMPLEXSCALAR a, // pointer to the temperature parameter received from Mathcad
    LPCCOMPLEXSCALAR b) // pointer to the pressure parameter received from Mathcad
{  
    // first check to make sure "a" and "b" have no imaginary component
    if ( a->imag != 0.0 )
        return MAKELRESULT(MUST_BE_REAL,1);
    if ( b->imag != 0.0 )
        return MAKELRESULT(MUST_BE_REAL,2);

    //otherwise, all is well, evaluate function
    try {
        c->real = 1.0/IF97::rhomass_Tp(a->real,b->real);
    }
    catch (const std::out_of_range &e) { 
        if (e.what()[0] == 'T') 
            return MAKELRESULT(T_OUT_OF_RANGE,1);
        else if (e.what()[0] == 'P')
            return MAKELRESULT(P_OUT_OF_RANGE,2);
        else if (e.what()[0] == 'C')
            return MAKELRESULT(SATURATED,1);
        else
            return MAKELRESULT(UNKNOWN,1);
    }
    catch (const std::logic_error& ) {
        return MAKELRESULT(NO_SOLUTION_FOUND,1);
    }
    // normal return
    return 0;
}

FUNCTIONINFO    if97_vtp = 
{
    "if97_vtp",                    // name by which Mathcad will recognize the function
    "T,p",                             // if97_vtp will be called as if97_vtp(T,p)
    // description of if97_vtp(p)
    "Obtains the specific volume [m³/kg] as a function of T [K] and p [Pa]",
    (LPCFUNCTION)if97_VTP,         // pointer to executable code
    COMPLEX_SCALAR,                  // the return type is a complex scalar
    2,                               // there are two input parameters
    { COMPLEX_SCALAR, COMPLEX_SCALAR }               //    that are both complex scalars
};