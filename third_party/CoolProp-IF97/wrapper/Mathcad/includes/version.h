// if97_getvers stub - Interfaces CoolProp IF97 function to Mathcad
//

// this code executes the user function if97_getvers(), which is a wrapper for
// the CoolProp-IF97 function, version(void), used to get the IF97 code version.
LRESULT  IF97_Version(
    LPMCSTRING hstr,        // pointer to the results string
    LPCCOMPLEXSCALAR dummy) // pointer to the dummy parameter received from Mathcad
                            // Unfortunately Mathcad functions have to pass at least
                            // one parameter and it cannot be void.
{

    // Note: we don't care what gets passed in.
    // call IF97's get_if97_version() routine;
    std::string s = IF97::get_if97_version();

    char * c = MathcadAllocate(static_cast<int>(s.size())+1); //create a c-string (pointer) with same size as s
    // copy s into c, this process avoids the const-cast type which would result
    // from instead converting the string using s.c_str()
    std::copy(s.begin(), s.end(), c);
    c[s.size()] = '\0';                 // tag on the null character
    hstr->str = c;                      // assign the string to the output parameter
    // normal return
    return 0;
}

FUNCTIONINFO    if97_getvers = 
{
    "if97_getvers",                                 // name of the function in Mathcad
    "dummy",                                        // description of input parameter
    "Obtains the CoolProp-IF97 Version string.",    // description of this function
    (LPCFUNCTION)IF97_Version,       // pointer to the function code
    MC_STRING,                       // Returns a Mathcad string, MC_STRING
    1,                               // there is only one argument, dummy
    { COMPLEX_SCALAR }               //    dummy is a Mathcad complex scalar
};