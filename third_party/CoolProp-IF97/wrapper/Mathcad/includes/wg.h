// if97_wg stub - Interfaces CoolProp IF97 function to Mathcad
//

// this code executes the user function if97_wg(P), which is a wrapper for
// the CoolProp-IF97 function, speed_soundvap_p(P), used to calculate the saturated
// vapor speed of sound along the saturation curve in terms of pressure
LRESULT  if97_Wg(
    LPCOMPLEXSCALAR c,  // pointer to the result
    LPCCOMPLEXSCALAR a) // pointer to the parameter received from Mathcad
{  
    // first check to make sure "a" has no imaginary component
    if ( a->imag != 0.0 )
        return MAKELRESULT(MUST_BE_REAL,1);

    // otherwise, all is well, evaluate function
    try {
        c->real = IF97::speed_soundvap_p(a->real);
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

FUNCTIONINFO    if97_wg = 
{
    "if97_wg",                     // name by which Mathcad will recognize the function
    "p",                             // if97_wg will be called as if97_wg(p)
    // description of if97_wg(p)
    "Obtains saturated vapor speed of sound [m/s] as a function of pressure, p [Pa].",
    (LPCFUNCTION)if97_Wg,			// pointer to executable code
    COMPLEX_SCALAR,					// the return type is a complex scalar
    1,								// there is only one input parameter
    { COMPLEX_SCALAR }				//    it is a complex scalar
};