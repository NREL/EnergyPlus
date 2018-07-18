#ifndef PsychState_hh_INCLUDED
#define PsychState_hh_INCLUDED

// TODO: Eventually move into Psychrometrics.hh

#include <EnergyPlus.hh>

namespace EnergyPlus {

namespace Psychrometrics {

    struct PsychState
    {
        Real64 tdb;          // Dry-bulb temperature {C}
        Real64 w;            // Humidity Ratio {kg/kg}
        Real64 p;            // Barometric Pressure {Pa}
        Real64 h;            // Enthalpy {J/kg}
        Real64 twb;          // Wet-bulb temperature {C}
        Real64 rh;           // Relative Humidity
        Real64 massFlowRate; // Mass Flow Rate {kg/s}
    };

} // namespace Psychrometrics
} // namespace EnergyPlus

#endif // PsychState_hh_INCLUDED
