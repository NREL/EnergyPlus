// if97_cpf stub - Interfaces CoolProp IF97 function to Mathcad
//

// this code executes the user function if97_cpf(P), which is a wrapper for
// the CoolProp-IF97 function, cpliq_p(p), used to calculate the saturated
// liquid isobaric specific heat along the saturation curve in terms of pressure
LRESULT  if97_Cpf(
    LPCOMPLEXSCALAR c,  // pointer to the result
    LPCCOMPLEXSCALAR a) // pointer to the parameter received from Mathcad
{  
    // first check to make sure "a" has no imaginary component
    if ( a->imag != 0.0 )
        return MAKELRESULT(MUST_BE_REAL,1);

    // otherwise, all is well, evaluate function
    try {
        c->real = IF97::cpliq_p(a->real);
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

FUNCTIONINFO    if97_cpf = 
{
    "if97_cpf",                     // name by which Mathcad will recognize the function
    "p",                             // if97_cpf will be called as if97_cpf(p)
    // description of if97_cpf(p)
    "Obtains saturated liquid isobaric specific heat [J/kg/K] as a function of pressure, p [Pa].",
    (LPCFUNCTION)if97_Cpf,			// pointer to executable code
    COMPLEX_SCALAR,					// the return type is a complex scalar
    1,								// there is only one input parameter
    { COMPLEX_SCALAR }				//    it is a complex scalar
};