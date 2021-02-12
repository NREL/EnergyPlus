// if97_sg stub - Interfaces CoolProp IF97 function to Mathcad
//

// this code executes the user function if97_sg(P), which is a wrapper for
// the CoolProp-IF97 function, svap_p(P), used to calculate the saturated
// vapor entropy along the saturation curve in terms of pressure
LRESULT  if97_Sg(
    LPCOMPLEXSCALAR c,  // pointer to the result
    LPCCOMPLEXSCALAR a) // pointer to the parameter received from Mathcad
{  
    // first check to make sure "a" has no imaginary component
    if ( a->imag != 0.0 )
        return MAKELRESULT(MUST_BE_REAL,1);

    // otherwise, all is well, evaluate function
    try {
        c->real = IF97::svap_p(a->real);
    }
    catch (const std::out_of_range &e) { 
        if (e.what()[0] == 'P') 
            return MAKELRESULT(P_OUT_OF_RANGE,1);
        else //
            return MAKELRESULT(NO_SOLUTION_FOUND,1);
    }

    // normal return
    return 0;
}

FUNCTIONINFO    if97_sg = 
{
    "if97_sg",                     // name by which Mathcad will recognize the function
    "p",                             // if97_sg will be called as if97_sg(p)
    // description of if97_sg(p)
    "Obtains saturated vapor entropy [J/kg/K] as a function of pressure, p [Pa].",
    (LPCFUNCTION)if97_Sg,			// pointer to executable code
    COMPLEX_SCALAR,					// the return type is a complex scalar
    1,								// there is only one input parameter
    { COMPLEX_SCALAR }				//    it is a complex scalar
};