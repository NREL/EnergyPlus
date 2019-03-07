#ifndef EPFMI_I
#define EPFMI_I

%module epfmi

%{
  #include "EPFMI.hpp"
%}

#define EPFMI_API

//%include "EPFMI.hpp"

EPFMI_API unsigned int setupExperiment(double tStart,
                             bool stopTimeDefined,
                             const char *log);

                             
#endif //EPFMI_I