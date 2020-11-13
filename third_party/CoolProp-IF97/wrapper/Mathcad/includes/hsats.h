// if97_hsats stub - Interfaces CoolProp IF97 function to Mathcad
//

// this code executes the user function if97_hsats(S), which is a wrapper for
// the CoolProp-IF97 function, Hsat_s(s), used to calculate the 
// enthalpy along the saturation boundary as a function of entropy
LRESULT  if97_HsatS(
    LPCOMPLEXSCALAR c,  // pointer to the result
    LPCCOMPLEXSCALAR a) // pointer to the parameter received from Mathcad
{  
    // first check to make sure "a" has no imaginary component
    if ( a->imag != 0.0 )
        return MAKELRESULT(MUST_BE_REAL,1);

    //otherwise, all is well, evaluate function
    try{
        c->real = IF97::Backwards::Hsat_s(a->real);
    }
    catch (const std::out_of_range& e) { 
        if (e.what()[0] == 'E')                        // Entropy out of range
            return MAKELRESULT(S_OUT_OF_RANGE,1);
        else                                           // some other error
            return MAKELRESULT(NO_SOLUTION_FOUND,2);
    }
    // normal return
    return 0;
}

FUNCTIONINFO    if97_hsats = 
{
    "if97_hsats",                    // name by which Mathcad will recognize the function
    "s",                             // if97_hsats will be called as if97_hsats(s)
    // description of if97_t23(p)
    "Obtains Enthalpy [J/kg] along the Liquid/Vapor saturation boundary as a function of Entropy [J/kg-K].",
    (LPCFUNCTION)if97_HsatS,         // pointer to executable code
    COMPLEX_SCALAR,                  // the return type is a complex scalar
    1,                               // there is only one input parameter
    { COMPLEX_SCALAR }               //    it is a complex scalar
};