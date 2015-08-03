#ifndef ElectricBaseboardRadiator_hh_INCLUDED
#define ElectricBaseboardRadiator_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>
#include <DataGlobals.hh>

namespace EnergyPlus {

namespace ElectricBaseboardRadiator {

	// Using/Aliasing

	// Data
	//MODULE PARAMETER DEFINITIONS
	extern int const BaseboardRadiator_Electric;
	extern std::string const cCMO_BBRadiator_Electric;

	// DERIVED TYPE DEFINITIONS

	//MODULE VARIABLE DECLARATIONS:
	extern int NumElecBaseboards;
	extern Array1D< Real64 > QBBElecRadSource; // Need to keep the last value in case we are still iterating
	extern Array1D< Real64 > QBBElecRadSrcAvg; // Need to keep the last value in case we are still iterating
	extern Array1D< Real64 > ZeroSourceSumHATsurf; // Equal to the SumHATsurf for all the walls in a zone with no source
	// Record keeping variables used to calculate QBBRadSrcAvg locally
	extern Array1D< Real64 > LastQBBElecRadSrc; // Need to keep the last value in case we are still iterating
	extern Array1D< Real64 > LastSysTimeElapsed; // Need to keep the last value in case we are still iterating
	extern Array1D< Real64 > LastTimeStepSys; // Need to keep the last value in case we are still iterating
	extern Array1D_bool MySizeFlag;
	extern Array1D_bool CheckEquipName;
	//SUBROUTINE SPECIFICATIONS FOR MODULE BaseboardRadiator

	// Types

	struct ElecBaseboardParams
	{
		// Members
		std::string EquipName;
		int EquipType;
		std::string Schedule;
		Array1D_string SurfaceName;
		Array1D_int SurfacePtr;
		int ZonePtr;
		int SchedPtr;
		int TotSurfToDistrib;
		Real64 NominalCapacity;
		Real64 BaseboardEfficiency;
		Real64 AirInletTemp;
		Real64 AirInletHumRat;
		Real64 AirOutletTemp;
		Real64 ElecUseLoad;
		Real64 ElecUseRate;
		Real64 FracRadiant;
		Real64 FracConvect;
		Real64 FracDistribPerson;
		Real64 TotPower;
		Real64 Power;
		Real64 ConvPower;
		Real64 RadPower;
		Real64 TotEnergy;
		Real64 Energy;
		Real64 ConvEnergy;
		Real64 RadEnergy;
		Array1D< Real64 > FracDistribToSurf;
		int HeatingCapMethod; // - Method for electric baseboard heating capacity scalable sizing calculation
		Real64 ScaledHeatingCapacity; // - electric baseboard scaled maximum heating capacity {W} or scalable variable for sizing in {-}, or {W/m2}

		// Default Constructor
		ElecBaseboardParams() :
			EquipType( 0 ),
			ZonePtr( 0 ),
			SchedPtr( 0 ),
			TotSurfToDistrib( 0 ),
			NominalCapacity( 0.0 ),
			BaseboardEfficiency( 0.0 ),
			AirInletTemp( 0.0 ),
			AirInletHumRat( 0.0 ),
			AirOutletTemp( 0.0 ),
			ElecUseLoad( 0.0 ),
			ElecUseRate( 0.0 ),
			FracRadiant( 0.0 ),
			FracConvect( 0.0 ),
			FracDistribPerson( 0.0 ),
			TotPower( 0.0 ),
			Power( 0.0 ),
			ConvPower( 0.0 ),
			RadPower( 0.0 ),
			TotEnergy( 0.0 ),
			Energy( 0.0 ),
			ConvEnergy( 0.0 ),
			RadEnergy( 0.0 ),
			HeatingCapMethod( 0 ),
			ScaledHeatingCapacity( 0.0 )
		{}

		// Member Constructor
		ElecBaseboardParams(
			std::string const & EquipName,
			int const EquipType,
			std::string const & Schedule,
			Array1_string const & SurfaceName,
			Array1_int const & SurfacePtr,
			int const ZonePtr,
			int const SchedPtr,
			int const TotSurfToDistrib,
			Real64 const NominalCapacity,
			Real64 const BaseboardEfficiency,
			Real64 const AirInletTemp,
			Real64 const AirInletHumRat,
			Real64 const AirOutletTemp,
			Real64 const ElecUseLoad,
			Real64 const ElecUseRate,
			Real64 const FracRadiant,
			Real64 const FracConvect,
			Real64 const FracDistribPerson,
			Real64 const TotPower,
			Real64 const Power,
			Real64 const ConvPower,
			Real64 const RadPower,
			Real64 const TotEnergy,
			Real64 const Energy,
			Real64 const ConvEnergy,
			Real64 const RadEnergy,
			Array1< Real64 > const & FracDistribToSurf,
			int const HeatingCapMethod,
			Real64 const ScaledHeatingCapacity
		) :
			EquipName( EquipName ),
			EquipType( EquipType ),
			Schedule( Schedule ),
			SurfaceName( SurfaceName ),
			SurfacePtr( SurfacePtr ),
			ZonePtr( ZonePtr ),
			SchedPtr( SchedPtr ),
			TotSurfToDistrib( TotSurfToDistrib ),
			NominalCapacity( NominalCapacity ),
			BaseboardEfficiency( BaseboardEfficiency ),
			AirInletTemp( AirInletTemp ),
			AirInletHumRat( AirInletHumRat ),
			AirOutletTemp( AirOutletTemp ),
			ElecUseLoad( ElecUseLoad ),
			ElecUseRate( ElecUseRate ),
			FracRadiant( FracRadiant ),
			FracConvect( FracConvect ),
			FracDistribPerson( FracDistribPerson ),
			TotPower( TotPower ),
			Power( Power ),
			ConvPower( ConvPower ),
			RadPower( RadPower ),
			TotEnergy( TotEnergy ),
			Energy( Energy ),
			ConvEnergy( ConvEnergy ),
			RadEnergy( RadEnergy ),
			FracDistribToSurf( FracDistribToSurf ),
			HeatingCapMethod( HeatingCapMethod ),
			ScaledHeatingCapacity( ScaledHeatingCapacity )
		{}

	};

	struct ElecBaseboardNumericFieldData
	{
		// Members
		Array1D_string FieldNames;

		// Default Constructor
		ElecBaseboardNumericFieldData()
		{}

		// Member Constructor
		ElecBaseboardNumericFieldData(
			Array1_string const & FieldNames // Name of the HeatingCoil numeric field descriptions
			) :
			FieldNames(FieldNames)
		{}
	};

	// Object Data
	extern Array1D< ElecBaseboardParams > ElecBaseboard;
	extern Array1D< ElecBaseboardNumericFieldData > ElecBaseboardNumericFields;

	// Functions

	void
	SimElecBaseboard(
		std::string const & EquipName,
		int const ActualZoneNum,
		int const ControlledZoneNum,
		bool const FirstHVACIteration,
		Real64 & PowerMet,
		int & CompIndex
	);

	void
	GetElectricBaseboardInput();

	void
	InitElectricBaseboard(
		int const BaseboardNum,
		int const ControlledZoneNumSub,
		bool const FirstHVACIteration
	);

	void
	SizeElectricBaseboard( int const BaseboardNum );

	void
	CalcElectricBaseboard(
		int const BaseboardNum,
		int const ControlledZoneNum
	);

	void
	UpdateElectricBaseboard( int const BaseboardNum );

	void
	UpdateBBElecRadSourceValAvg( bool & ElecBaseboardSysOn ); // .TRUE. if the radiant system has run this zone time step

	void
	DistributeBBElecRadGains();

	void
	ReportElectricBaseboard( int const BaseboardNum );

	Real64
	SumHATsurf( int const ZoneNum ); // Zone number

	//     NOTICE

	//     Copyright (c) 1996-2014 The Board of Trustees of the University of Illinois
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

} // ElectricBaseboardRadiator

} // EnergyPlus

#endif
