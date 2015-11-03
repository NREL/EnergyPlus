#ifndef EcoRoofManager_hh_INCLUDED
#define EcoRoofManager_hh_INCLUDED

// EnergyPlus Headers
#include <EnergyPlus.hh>

namespace EnergyPlus {

namespace EcoRoofManager {

	// Data
	// MODULE PARAMETER DEFINITIONS
	// na

	// DERIVED TYPE DEFINITIONS
	// na

	// MODULE VARIABLE DECLARATIONS:

	extern Real64 CumRunoff; // Cumulative runoff, updated each time step (m) mult by roof area to get volume
	extern Real64 CumET; // Cumulative evapotranspiration from soil and plants (m)
	extern Real64 CumPrecip;
	extern Real64 CumIrrigation; // Cumulative irrigation, updated each time step (m) mult by roof area to get volume
	extern Real64 CurrentRunoff;
	extern Real64 CurrentET;
	extern Real64 CurrentPrecipitation; // units of (m) per timestep
	extern Real64 CurrentIrrigation; // units of (m) per timestep

	extern Real64 Tfold; // leaf temperature from the previous time step
	extern Real64 Tgold; // ground temperature from the previous time step
	extern bool EcoRoofbeginFlag;

	// Functions

	void
	CalcEcoRoof(
		int const SurfNum, // Indicator of Surface Number for the current surface
		int const ZoneNum, // Indicator for zone number where the current surface
		int & ConstrNum, // Indicator for contruction index for the current surface
		Real64 & TempExt // Exterior temperature boundary condidtion
	);

	void
	UpdateSoilProps(
		Real64 & Moisture,
		Real64 & MeanRootMoisture,
		Real64 const MoistureMax,
		Real64 const MoistureResidual,
		Real64 const SoilThickness,
		Real64 const Vfluxf, // Water mass flux from vegetation [m/s]
		Real64 const Vfluxg, // Water mass flux from soil surface [m/s]
		int & ConstrNum, // Indicator for contruction index for the current surface
		Real64 & Alphag,
		int const unit, // unused1208
		Real64 const Tg, // unused1208
		Real64 const Tf, // unused1208
		Real64 const Qsoil // unused1208
	);

} // EcoRoofManager

} // EnergyPlus

#endif
