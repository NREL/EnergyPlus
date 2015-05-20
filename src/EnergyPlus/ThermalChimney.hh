#ifndef ThermalChimney_hh_INCLUDED
#define ThermalChimney_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1A.hh>
#include <ObjexxFCL/Array2A.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>
#include <DataGlobals.hh>

namespace EnergyPlus {

namespace ThermalChimney {

	// Using/Aliasing

	// Data
	// DERIVED TYPE DEFINITIONS

	extern int TotThermalChimney; // Total ThermalChimney Statements in input

	// Subroutine Specifications for the Heat Balance Module
	// Driver Routines
	// Get Input routines for module
	// Algorithms for the module
	// Reporting routines for module
	// Utility routines for module

	// Types

	struct ThermalChimneyData
	{
		// Members
		std::string Name;
		int RealZonePtr;
		std::string RealZoneName;
		int SchedPtr;
		std::string SchedName;
		Real64 AbsorberWallWidth;
		Real64 AirOutletCrossArea;
		Real64 DischargeCoeff;
		int TotZoneToDistrib;
		Array1D_int ZonePtr;
		Array1D_string ZoneName;
		Array1D< Real64 > DistanceThermChimInlet;
		Array1D< Real64 > RatioThermChimAirFlow;
		Array1D< Real64 > EachAirInletCrossArea;

		// Default Constructor
		ThermalChimneyData() :
			RealZonePtr( 0 ),
			SchedPtr( 0 ),
			AbsorberWallWidth( 0.0 ),
			AirOutletCrossArea( 0.0 ),
			DischargeCoeff( 0.0 ),
			TotZoneToDistrib( 0 )
		{}

		// Member Constructor
		ThermalChimneyData(
			std::string const & Name,
			int const RealZonePtr,
			std::string const & RealZoneName,
			int const SchedPtr,
			std::string const & SchedName,
			Real64 const AbsorberWallWidth,
			Real64 const AirOutletCrossArea,
			Real64 const DischargeCoeff,
			int const TotZoneToDistrib,
			Array1_int const & ZonePtr,
			Array1_string const & ZoneName,
			Array1< Real64 > const & DistanceThermChimInlet,
			Array1< Real64 > const & RatioThermChimAirFlow,
			Array1< Real64 > const & EachAirInletCrossArea
		) :
			Name( Name ),
			RealZonePtr( RealZonePtr ),
			RealZoneName( RealZoneName ),
			SchedPtr( SchedPtr ),
			SchedName( SchedName ),
			AbsorberWallWidth( AbsorberWallWidth ),
			AirOutletCrossArea( AirOutletCrossArea ),
			DischargeCoeff( DischargeCoeff ),
			TotZoneToDistrib( TotZoneToDistrib ),
			ZonePtr( ZonePtr ),
			ZoneName( ZoneName ),
			DistanceThermChimInlet( DistanceThermChimInlet ),
			RatioThermChimAirFlow( RatioThermChimAirFlow ),
			EachAirInletCrossArea( EachAirInletCrossArea )
		{}

	};

	struct ThermChimZnReportVars
	{
		// Members
		Real64 ThermalChimneyHeatLoss; // Heat Gain {Joules} due to ThermalChimney
		Real64 ThermalChimneyHeatGain; // Heat Loss {Joules} due to ThermalChimney
		Real64 ThermalChimneyVolume; // Volume of Air {m3} due to ThermalChimney
		Real64 ThermalChimneyMass; // Mass of Air {kg} due to ThermalChimney

		// Default Constructor
		ThermChimZnReportVars() :
			ThermalChimneyHeatLoss( 0.0 ),
			ThermalChimneyHeatGain( 0.0 ),
			ThermalChimneyVolume( 0.0 ),
			ThermalChimneyMass( 0.0 )
		{}

		// Member Constructor
		ThermChimZnReportVars(
			Real64 const ThermalChimneyHeatLoss, // Heat Gain {Joules} due to ThermalChimney
			Real64 const ThermalChimneyHeatGain, // Heat Loss {Joules} due to ThermalChimney
			Real64 const ThermalChimneyVolume, // Volume of Air {m3} due to ThermalChimney
			Real64 const ThermalChimneyMass // Mass of Air {kg} due to ThermalChimney
		) :
			ThermalChimneyHeatLoss( ThermalChimneyHeatLoss ),
			ThermalChimneyHeatGain( ThermalChimneyHeatGain ),
			ThermalChimneyVolume( ThermalChimneyVolume ),
			ThermalChimneyMass( ThermalChimneyMass )
		{}

	};

	struct ThermChimReportVars
	{
		// Members
		Real64 OverallTCVolumeFlow; // Volume of Air {m3/s} due to ThermalChimney
		Real64 OverallTCVolumeFlowStd; // Volume of Air {m3/s} due to ThermalChimney at standard conditions
		Real64 OverallTCMassFlow; // Mass of Air {kg/s} due to ThermalChimney
		Real64 OutletAirTempThermalChim; // Air Temp {C} of ThermalChimney

		// Default Constructor
		ThermChimReportVars() :
			OverallTCVolumeFlow( 0.0 ),
			OverallTCVolumeFlowStd( 0.0 ),
			OverallTCMassFlow( 0.0 ),
			OutletAirTempThermalChim( 0.0 )
		{}

		// Member Constructor
		ThermChimReportVars(
			Real64 const OverallTCVolumeFlow, // Volume of Air {m3/s} due to ThermalChimney
			Real64 const OverallTCVolumeFlowStd, // Volume of Air {m3/s} due to ThermalChimney at standard conditions
			Real64 const OverallTCMassFlow, // Mass of Air {kg/s} due to ThermalChimney
			Real64 const OutletAirTempThermalChim // Air Temp {C} of ThermalChimney
		) :
			OverallTCVolumeFlow( OverallTCVolumeFlow ),
			OverallTCVolumeFlowStd( OverallTCVolumeFlowStd ),
			OverallTCMassFlow( OverallTCMassFlow ),
			OutletAirTempThermalChim( OutletAirTempThermalChim )
		{}

	};

	// Object Data
	extern Array1D< ThermalChimneyData > ThermalChimneySys;
	extern Array1D< ThermChimZnReportVars > ZnRptThermChim;
	extern Array1D< ThermChimReportVars > ThermalChimneyReport;

	// Functions

	void
	ManageThermalChimney();

	void
	GetThermalChimney( bool & ErrorsFound ); // If errors found in input

	void
	CalcThermalChimney();

	void
	ReportThermalChimney();

	void
	GaussElimination(
		Array2A< Real64 > EquaCoef,
		Array1A< Real64 > EquaConst,
		Array1A< Real64 > ThermChimSubTemp,
		int const NTC
	);

	//        End of Module Subroutines for ThermalChimney

	//*****************************************************************************************
	//     NOTICE

	//     Copyright © 1996-2014 The Board of Trustees of the University of Illinois
	//     and The Regents of the University of California through Ernest Orlando Lawrence
	//     Berkeley National Laboratory.  All rights reserved.

	//     Portions of the EnergyPlus software package have been developed and copyrighted
	//     by other individuals, companies and institutions.  These portions have been
	//     incorporated into the EnergyPlus software package under license.   For a complete
	//     list of contributors, see "Notice" located in main.cc.

	//     NOTICE: The U.S. Government is granted for itself and others acting on its
	//     behalf a paid-up, nonexclusive, irrevocable, worldwide license in this data to
	//     reproduce, prepare derivative works, and perform publicly and display publicly.
	//     Beginning five (5) years after permission to assert copyright is granted,
	//     subject to two possible five year renewals, the U.S. Government is granted for
	//     itself and others acting on its behalf a paid-up, non-exclusive, irrevocable
	//     worldwide license in this data to reproduce, prepare derivative works,
	//     distribute copies to the public, perform publicly and display publicly, and to
	//     permit others to do so.

	//     TRADEMARKS: EnergyPlus is a trademark of the US Department of Energy.

} // ThermalChimney

} // EnergyPlus

#endif
