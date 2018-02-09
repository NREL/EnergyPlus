#ifndef PsychState_hh_INCLUDED
#define PsychState_hh_INCLUDED

// TODO: Eventually move into Psychrometrics.hh

#include <EnergyPlus.hh>

namespace EnergyPlus {

namespace Psychrometrics {

struct PsychState {
  Real64 tdb; // Dry-bulb temperature
  Real64 w; // Humidity Ratio
  Real64 p;  // Barometric Pressure
  Real64 h; // Enthalpy
  Real64 twb; // Wet-bulb temperature
  Real64 rh; // Relative Humidity
};

}
}

#endif // PsychState_hh_INCLUDED
