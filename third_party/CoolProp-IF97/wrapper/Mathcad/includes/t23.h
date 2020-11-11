// if97_t23 stub - Interfaces CoolProp IF97 function to Mathcad
//

// this code executes the user function if97_t23(P), which is a wrapper for
// the CoolProp-IF97 function, Region23_p(P), used to calculate the 
// temperature along the Region 2/3 boundary in terms of pressure
LRESULT  if97_T23(
    LPCOMPLEXSCALAR c,  // pointer to the result
    LPCCOMPLEXSCALAR a) // pointer to the parameter received from Mathcad
{  
    // first check to make sure "a" has no imaginary component
    if ( a->imag != 0.0 )
        return MAKELRESULT(MUST_BE_REAL,1);

    if ( (a->real < IF97::P23min) || (a->real > IF97::Pmax) )
        return MAKELRESULT(T_OUT_OF_RANGE,1);

    //otherwise, all is well, evaluate function
    c->real = IF97::Region23_p(a->real);

    // normal return
    return 0;
}

FUNCTIONINFO    if97_t23 = 
{
    "if97_t23",                    // name by which Mathcad will recognize the function
    "p",                             // if97_t23 will be called as if97_t23(p)
    // description of if97_t23(p)
    "Obtains Temperature [K] along the Region 2/3 boundary as a function of pressure [Pa].",
    (LPCFUNCTION)if97_T23,         // pointer to executable code
    COMPLEX_SCALAR,                  // the return type is a complex scalar
    1,                               // there is only one input parameter
    { COMPLEX_SCALAR }               //    it is a complex scalar
};