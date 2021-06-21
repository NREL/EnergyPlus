// if97_regionps stub - Interfaces CoolProp IF97 function to Mathcad
//

// this code executes the user function if97_regionps(P,h), which is a wrapper for
// the CoolProp-IF97 test function, Region_ps(p,h).
LRESULT  if97_REGIONPS(
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
        c->real = IF97::Region_ps(a->real,b->real);
    }
    catch (const std::out_of_range& e) { 
        if (e.what()[0] == 'P') 
            return MAKELRESULT(P_OUT_OF_RANGE,1);
        else // (e.what == "S")
            return MAKELRESULT(S_OUT_OF_RANGE,2);
    }
    catch (const std::logic_error&) {
        return MAKELRESULT(NO_SOLUTION_FOUND,1);
    }
    // normal return
    return 0;
}

FUNCTIONINFO    if97_regionps = 
{
    "if97_regionps",                 // name by which Mathcad will recognize the function
    "p,s",                           // if97_regionps will be called as if97_regionps(p,h)
    // description of if97_regionps(p)
    "Obtains the Region Number as a function of p [Pa] and mass entropy, s, [J/kg-K].",
    (LPCFUNCTION)if97_REGIONPS,           // pointer to executable code
    COMPLEX_SCALAR,                  // the return type is a complex scalar
    2,                               // there are two input parameters
    { COMPLEX_SCALAR, COMPLEX_SCALAR }               //    that are both complex scalars
};