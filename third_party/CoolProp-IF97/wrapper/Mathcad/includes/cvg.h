// if97_cvg stub - Interfaces CoolProp IF97 function to Mathcad
//

// this code executes the user function if97_cvg(P), which is a wrapper for
// the CoolProp-IF97 function, cvvap_p(P), used to calculate the saturated
// vapor isochoric specific heat along the saturation curve in terms of pressure
LRESULT  if97_Cvg(
    LPCOMPLEXSCALAR c,  // pointer to the result
    LPCCOMPLEXSCALAR a) // pointer to the parameter received from Mathcad
{  
    // first check to make sure "a" has no imaginary component
    if ( a->imag != 0.0 )
        return MAKELRESULT(MUST_BE_REAL,1);

    // otherwise, all is well, evaluate function
    try {
        c->real = IF97::cvvap_p(a->real);
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

FUNCTIONINFO    if97_cvg = 
{
    "if97_cvg",                     // name by which Mathcad will recognize the function
    "p",                             // if97_cvg will be called as if97_cvg(p)
    // description of if97_cvg(p)
    "Obtains saturated vapor isochoric specific heat [J/kg/K] as a function of pressure, p [Pa].",
    (LPCFUNCTION)if97_Cvg,			// pointer to executable code
    COMPLEX_SCALAR,					// the return type is a complex scalar
    1,								// there is only one input parameter
    { COMPLEX_SCALAR }				//    it is a complex scalar
};