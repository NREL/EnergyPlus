#ifndef EarthTube_hh_INCLUDED
#define EarthTube_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>
#include <DataGlobals.hh>

namespace EnergyPlus {

namespace EarthTube {

	// Using/Aliasing

	// Data
	// DERIVED TYPE DEFINITIONS

	// MODULE VARIABLES DECLARATIONS:
	extern int TotEarthTube; // Total EarthTube Statements in input
	// Parameters for Ventilation
	extern int const NaturalEarthTube;
	extern int const IntakeEarthTube;
	extern int const ExhaustEarthTube;

	//         Subroutine Specifications for the Heat Balance Module
	// Driver Routines

	// Get Input routines for module

	// Algorithms for the module

	// Reporting routines for module

	// Types

	struct EarthTubeData
	{
		// Members
		int ZonePtr;
		int SchedPtr;
		std::string SchedName;
		Real64 DesignLevel;
		Real64 MinTemperature;
		Real64 MaxTemperature;
		Real64 DelTemperature;
		int FanType;
		Real64 FanPressure;
		Real64 FanEfficiency;
		Real64 FanPower;
		Real64 GroundTempz1z2t; // ground temp between z1 and z2 at time t
		Real64 InsideAirTemp;
		Real64 AirTemp;
		Real64 r1; // Inner Pipe Radius (m)
		Real64 r2; // Pipe Thickness (m)
		Real64 r3; // Distance between Pipe Outer Surface and Undistubed Soil (m)
		Real64 PipeLength; // Entire Pipe Length
		Real64 PipeThermCond; // Pipe Thermal Conductivity
		Real64 z; // Depth under the Ground Surface (m)
		Real64 SoilThermDiff; // Soil Thermal Diffusivity
		Real64 SoilThermCond; // Soil Thermal Conductivity
		Real64 AverSoilSurTemp; // Average Soil Surface Temperature
		Real64 ApmlSoilSurTemp; // Amplitude of Soil Surface Temperature
		int SoilSurPhaseConst; // Phase constant of Soil Surface
		Real64 ConstantTermCoef;
		Real64 TemperatureTermCoef;
		Real64 VelocityTermCoef;
		Real64 VelocitySQTermCoef;

		// Default Constructor
		EarthTubeData() :
			ZonePtr( 0 ),
			SchedPtr( 0 ),
			DesignLevel( 0.0 ),
			MinTemperature( 0.0 ),
			MaxTemperature( 0.0 ),
			DelTemperature( 0.0 ),
			FanType( 0 ),
			FanPressure( 0.0 ),
			FanEfficiency( 0.0 ),
			FanPower( 0.0 ),
			GroundTempz1z2t( 0.0 ),
			InsideAirTemp( 0.0 ),
			AirTemp( 0.0 ),
			r1( 0.0 ),
			r2( 0.0 ),
			r3( 0.0 ),
			PipeLength( 0.0 ),
			PipeThermCond( 0.0 ),
			z( 0.0 ),
			SoilThermDiff( 0.0 ),
			SoilThermCond( 0.0 ),
			ConstantTermCoef( 0.0 ),
			TemperatureTermCoef( 0.0 ),
			VelocityTermCoef( 0.0 ),
			VelocitySQTermCoef( 0.0 )
		{}

		// Member Constructor
		EarthTubeData(
			int const ZonePtr,
			int const SchedPtr,
			std::string const & SchedName,
			Real64 const DesignLevel,
			Real64 const MinTemperature,
			Real64 const MaxTemperature,
			Real64 const DelTemperature,
			int const FanType,
			Real64 const FanPressure,
			Real64 const FanEfficiency,
			Real64 const FanPower,
			Real64 const GroundTempz1z2t, // ground temp between z1 and z2 at time t
			Real64 const InsideAirTemp,
			Real64 const AirTemp,
			Real64 const r1, // Inner Pipe Radius (m)
			Real64 const r2, // Pipe Thickness (m)
			Real64 const r3, // Distance between Pipe Outer Surface and Undistubed Soil (m)
			Real64 const PipeLength, // Entire Pipe Length
			Real64 const PipeThermCond, // Pipe Thermal Conductivity
			Real64 const z, // Depth under the Ground Surface (m)
			Real64 const SoilThermDiff, // Soil Thermal Diffusivity
			Real64 const SoilThermCond, // Soil Thermal Conductivity
			Real64 const AverSoilSurTemp, // Average Soil Surface Temperature
			Real64 const ApmlSoilSurTemp, // Amplitude of Soil Surface Temperature
			int const SoilSurPhaseConst, // Phase constant of Soil Surface
			Real64 const ConstantTermCoef,
			Real64 const TemperatureTermCoef,
			Real64 const VelocityTermCoef,
			Real64 const VelocitySQTermCoef
		) :
			ZonePtr( ZonePtr ),
			SchedPtr( SchedPtr ),
			SchedName( SchedName ),
			DesignLevel( DesignLevel ),
			MinTemperature( MinTemperature ),
			MaxTemperature( MaxTemperature ),
			DelTemperature( DelTemperature ),
			FanType( FanType ),
			FanPressure( FanPressure ),
			FanEfficiency( FanEfficiency ),
			FanPower( FanPower ),
			GroundTempz1z2t( GroundTempz1z2t ),
			InsideAirTemp( InsideAirTemp ),
			AirTemp( AirTemp ),
			r1( r1 ),
			r2( r2 ),
			r3( r3 ),
			PipeLength( PipeLength ),
			PipeThermCond( PipeThermCond ),
			z( z ),
			SoilThermDiff( SoilThermDiff ),
			SoilThermCond( SoilThermCond ),
			AverSoilSurTemp( AverSoilSurTemp ),
			ApmlSoilSurTemp( ApmlSoilSurTemp ),
			SoilSurPhaseConst( SoilSurPhaseConst ),
			ConstantTermCoef( ConstantTermCoef ),
			TemperatureTermCoef( TemperatureTermCoef ),
			VelocityTermCoef( VelocityTermCoef ),
			VelocitySQTermCoef( VelocitySQTermCoef )
		{}

	};

	struct EarthTubeZoneReportVars
	{
		// Members
		Real64 EarthTubeHeatLoss; // [J] Heat loss or cooling to zone from air delivered by earth tube
		Real64 EarthTubeHeatLossRate; // [W] Heat loss or cooling rate to zone from air delivered by earth tube
		Real64 EarthTubeHeatGain; // [J] Heat Gain to zone from air delivered by earth tube
		Real64 EarthTubeHeatGainRate; // [W] Heat Gain rate to zone from air delivered by earth tube
		Real64 EarthTubeOATreatmentPower; // [W] rate of heat transfer to/from air.  positive is heating OA to higher temp
		Real64 EarthTubeVolume; // Volume of Air {m3} due to EarthTube
		Real64 EarthTubeVolFlowRate; // Volume flow rate of air (m3/s) due to EarthTube
		Real64 EarthTubeVolFlowRateStd; // Volume flow rate of air (m3/s) due to EarthTube at standard air conditions
		Real64 EarthTubeMass; // Mass of Air {kg} due to EarthTube
		Real64 EarthTubeMassFlowRate; // Mass flow rate of air (kg/s) due to EarthTube
		Real64 EarthTubeFanElec; // [J] Fan Electricity consumed by EarthTube
		Real64 EarthTubeFanElecPower; // [W] Fan Electric power for EarthTube
		Real64 EarthTubeAirTemp; // Air Temp {C} of EarthTube, air leaving tube and entering zone

		// Default Constructor
		EarthTubeZoneReportVars() :
			EarthTubeHeatLoss( 0.0 ),
			EarthTubeHeatLossRate( 0.0 ),
			EarthTubeHeatGain( 0.0 ),
			EarthTubeHeatGainRate( 0.0 ),
			EarthTubeOATreatmentPower( 0.0 ),
			EarthTubeVolume( 0.0 ),
			EarthTubeVolFlowRate( 0.0 ),
			EarthTubeVolFlowRateStd( 0.0 ),
			EarthTubeMass( 0.0 ),
			EarthTubeMassFlowRate( 0.0 ),
			EarthTubeFanElec( 0.0 ),
			EarthTubeFanElecPower( 0.0 ),
			EarthTubeAirTemp( 0.0 )
		{}

		// Member Constructor
		EarthTubeZoneReportVars(
			Real64 const EarthTubeHeatLoss, // [J] Heat loss or cooling to zone from air delivered by earth tube
			Real64 const EarthTubeHeatLossRate, // [W] Heat loss or cooling rate to zone from air delivered by earth tube
			Real64 const EarthTubeHeatGain, // [J] Heat Gain to zone from air delivered by earth tube
			Real64 const EarthTubeHeatGainRate, // [W] Heat Gain rate to zone from air delivered by earth tube
			Real64 const EarthTubeOATreatmentPower, // [W] rate of heat transfer to/from air.  positive is heating OA to higher temp
			Real64 const EarthTubeVolume, // Volume of Air {m3} due to EarthTube
			Real64 const EarthTubeVolFlowRate, // Volume flow rate of air (m3/s) due to EarthTube
			Real64 const EarthTubeVolFlowRateStd, // Volume flow rate of air (m3/s) due to EarthTube at standard air conditions
			Real64 const EarthTubeMass, // Mass of Air {kg} due to EarthTube
			Real64 const EarthTubeMassFlowRate, // Mass flow rate of air (kg/s) due to EarthTube
			Real64 const EarthTubeFanElec, // [J] Fan Electricity consumed by EarthTube
			Real64 const EarthTubeFanElecPower, // [W] Fan Electric power for EarthTube
			Real64 const EarthTubeAirTemp // Air Temp {C} of EarthTube, air leaving tube and entering zone
		) :
			EarthTubeHeatLoss( EarthTubeHeatLoss ),
			EarthTubeHeatLossRate( EarthTubeHeatLossRate ),
			EarthTubeHeatGain( EarthTubeHeatGain ),
			EarthTubeHeatGainRate( EarthTubeHeatGainRate ),
			EarthTubeOATreatmentPower( EarthTubeOATreatmentPower ),
			EarthTubeVolume( EarthTubeVolume ),
			EarthTubeVolFlowRate( EarthTubeVolFlowRate ),
			EarthTubeVolFlowRateStd( EarthTubeVolFlowRateStd ),
			EarthTubeMass( EarthTubeMass ),
			EarthTubeMassFlowRate( EarthTubeMassFlowRate ),
			EarthTubeFanElec( EarthTubeFanElec ),
			EarthTubeFanElecPower( EarthTubeFanElecPower ),
			EarthTubeAirTemp( EarthTubeAirTemp )
		{}

	};

	// Object Data
	extern Array1D< EarthTubeData > EarthTubeSys;
	extern Array1D< EarthTubeZoneReportVars > ZnRptET;

	// Functions

	void
	ManageEarthTube();

	void
	GetEarthTube( bool & ErrorsFound ); // If errors found in input

	void
	CalcEarthTube();

	void
	ReportEarthTube();

	//        End of Module Subroutines for EarthTube

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

} // EarthTube

} // EnergyPlus

#endif
