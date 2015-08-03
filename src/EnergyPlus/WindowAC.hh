#ifndef WindowAC_hh_INCLUDED
#define WindowAC_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>
#include <DataGlobals.hh>

namespace EnergyPlus {

namespace WindowAC {

	// Using/Aliasing

	// Data
	// MODULE PARAMETER DEFINITIONS
	extern int const WindowAC_UnitType;
	extern std::string const cWindowAC_UnitType;
	extern Array1D_string const cWindowAC_UnitTypes;

	// Compressor operation
	extern int const On; // normal compressor operation
	extern int const Off; // signal DXCoil that compressor shouldn't run

	// DERIVED TYPE DEFINITIONS

	// MODULE VARIABLE DECLARATIONS:

	extern int NumWindAC;
	extern int NumWindACCyc;
	extern Array1D_bool MySizeFlag;
	extern bool GetWindowACInputFlag; // First time, input is "gotten"
	extern bool CoolingLoad; // defines a cooling load
	extern Array1D_bool CheckEquipName;

	// SUBROUTINE SPECIFICATIONS FOR MODULE

	// Types

	struct WindACData
	{
		// Members
		// input data
		std::string Name; // name of unit
		//  CHARACTER(len=MaxNameLength) :: UnitType         =' '  ! type of unit
		int UnitType; // type of unit
		std::string Sched; // availability schedule
		int SchedPtr; // index to schedule
		int FanSchedPtr; // index to fan operating mode schedule
		int FanAvailSchedPtr; // index to fan availability schedule
		Real64 MaxAirVolFlow; // m3/s
		Real64 MaxAirMassFlow; // kg/s
		Real64 OutAirVolFlow; // m3/s
		Real64 OutAirMassFlow; // kg/s
		int AirInNode; // inlet air node number
		int AirOutNode; // outlet air node number
		int OutsideAirNode; // outside air node number
		int AirReliefNode; // relief air node number
		int MixedAirNode; // Mixed Air Node number
		std::string OAMixName; // name of outdoor air mixer
		std::string OAMixType; // type of outdoor air mixer
		int OAMixIndex;
		std::string FanName; // name of fan
		std::string FanType; // type of fan
		int FanType_Num; // index to fan type
		int FanIndex;
		std::string DXCoilName; // name of cooling coil
		std::string DXCoilType; // type of cooling coil,Coil:DX:CoolingBypassFactorEmpirical or
		// 'CoilSystem:Cooling:DX:HeatExchangerAssisted'
		int DXCoilType_Num; // Numeric Equivalent for DXCoil Type
		int DXCoilIndex; // Index to DX cooling coil
		int CoilOutletNodeNum; // Outlet node number of DX cooling coil
		int OpMode; // mode of operation; 1=cycling fan, cycling compressor,
		// 2=continuous fan, cycling compresor
		int FanPlace; // fan placement; 1=blow through, 2=draw through
		int MaxIterIndex1;
		int MaxIterIndex2;
		Real64 ConvergenceTol; // Convergence tolerance, fraction (ZoneLoad - Equip Output)/ZoneLoad
		// Calc data
		Real64 PartLoadFrac; // part load fraction for the unit
		bool EMSOverridePartLoadFrac;
		Real64 EMSValueForPartLoadFrac;
		// Report data
		Real64 TotCoolEnergyRate; // total cooling output [W]
		Real64 TotCoolEnergy; // total cooling output [J]
		Real64 SensCoolEnergyRate; // sensible cooling output [W]
		Real64 SensCoolEnergy; // sensible cooling output [J]
		Real64 LatCoolEnergyRate; // sensible cooling output [W]
		Real64 LatCoolEnergy; // sensible cooling output [J]
		Real64 ElecPower; // electricity consumed [W]
		Real64 ElecConsumption; // electricity consumed [J]
		Real64 FanPartLoadRatio; // fan part-load ratio for time step
		Real64 CompPartLoadRatio; // compressor part-load ratio for time step
		std::string AvailManagerListName; // Name of an availability manager list object
		int AvailStatus;
		int ZonePtr; // pointer to a zone served by a Window AC unit
		int HVACSizingIndex; // index of a HVACSizing object for a Window AC unit

		// Default Constructor
		WindACData() :
			UnitType( 0 ),
			SchedPtr( 0 ),
			FanSchedPtr( 0 ),
			FanAvailSchedPtr( 0 ),
			MaxAirVolFlow( 0.0 ),
			MaxAirMassFlow( 0.0 ),
			OutAirVolFlow( 0.0 ),
			OutAirMassFlow( 0.0 ),
			AirInNode( 0 ),
			AirOutNode( 0 ),
			OutsideAirNode( 0 ),
			AirReliefNode( 0 ),
			MixedAirNode( 0 ),
			OAMixIndex( 0 ),
			FanType_Num( 0 ),
			FanIndex( 0 ),
			DXCoilType_Num( 0 ),
			DXCoilIndex( 0 ),
			CoilOutletNodeNum( 0 ),
			OpMode( 0 ),
			FanPlace( 0 ),
			MaxIterIndex1( 0 ),
			MaxIterIndex2( 0 ),
			ConvergenceTol( 0.0 ),
			PartLoadFrac( 0.0 ),
			EMSOverridePartLoadFrac( false ),
			EMSValueForPartLoadFrac( 0.0 ),
			TotCoolEnergyRate( 0.0 ),
			TotCoolEnergy( 0.0 ),
			SensCoolEnergyRate( 0.0 ),
			SensCoolEnergy( 0.0 ),
			LatCoolEnergyRate( 0.0 ),
			LatCoolEnergy( 0.0 ),
			ElecPower( 0.0 ),
			ElecConsumption( 0.0 ),
			FanPartLoadRatio( 0.0 ),
			CompPartLoadRatio( 0.0 ),
			AvailStatus( 0 ),
			ZonePtr( 0 ),
			HVACSizingIndex( 0 )
		{}

		// Member Constructor
		WindACData(
			std::string const & Name, // name of unit
			int const UnitType, // type of unit
			std::string const & Sched, // availability schedule
			int const SchedPtr, // index to schedule
			int const FanSchedPtr, // index to fan operating mode schedule
			int const FanAvailSchedPtr, // index to fan availability schedule
			Real64 const MaxAirVolFlow, // m3/s
			Real64 const MaxAirMassFlow, // kg/s
			Real64 const OutAirVolFlow, // m3/s
			Real64 const OutAirMassFlow, // kg/s
			int const AirInNode, // inlet air node number
			int const AirOutNode, // outlet air node number
			int const OutsideAirNode, // outside air node number
			int const AirReliefNode, // relief air node number
			int const MixedAirNode, // Mixed Air Node number
			std::string const & OAMixName, // name of outdoor air mixer
			std::string const & OAMixType, // type of outdoor air mixer
			int const OAMixIndex,
			std::string const & FanName, // name of fan
			std::string const & FanType, // type of fan
			int const FanType_Num, // index to fan type
			int const FanIndex,
			std::string const & DXCoilName, // name of cooling coil
			std::string const & DXCoilType, // type of cooling coil,Coil:DX:CoolingBypassFactorEmpirical or
			int const DXCoilType_Num, // Numeric Equivalent for DXCoil Type
			int const DXCoilIndex, // Index to DX cooling coil
			int const CoilOutletNodeNum, // Outlet node number of DX cooling coil
			int const OpMode, // mode of operation; 1=cycling fan, cycling compressor,
			int const FanPlace, // fan placement; 1=blow through, 2=draw through
			int const MaxIterIndex1,
			int const MaxIterIndex2,
			Real64 const ConvergenceTol, // Convergence tolerance, fraction (ZoneLoad - Equip Output)/ZoneLoad
			Real64 const PartLoadFrac, // part load fraction for the unit
			bool const EMSOverridePartLoadFrac,
			Real64 const EMSValueForPartLoadFrac,
			Real64 const TotCoolEnergyRate, // total cooling output [W]
			Real64 const TotCoolEnergy, // total cooling output [J]
			Real64 const SensCoolEnergyRate, // sensible cooling output [W]
			Real64 const SensCoolEnergy, // sensible cooling output [J]
			Real64 const LatCoolEnergyRate, // sensible cooling output [W]
			Real64 const LatCoolEnergy, // sensible cooling output [J]
			Real64 const ElecPower, // electricity consumed [W]
			Real64 const ElecConsumption, // electricity consumed [J]
			Real64 const FanPartLoadRatio, // fan part-load ratio for time step
			Real64 const CompPartLoadRatio, // compressor part-load ratio for time step
			std::string const & AvailManagerListName, // Name of an availability manager list object
			int const AvailStatus,
			int const ZonePtr, // pointer to a zone served by a Window AC unit
			int const HVACSizingIndex // index of a HVACSizing object for a Window AC unit

		) :
			Name( Name ),
			UnitType( UnitType ),
			Sched( Sched ),
			SchedPtr( SchedPtr ),
			FanSchedPtr( FanSchedPtr ),
			FanAvailSchedPtr( FanAvailSchedPtr ),
			MaxAirVolFlow( MaxAirVolFlow ),
			MaxAirMassFlow( MaxAirMassFlow ),
			OutAirVolFlow( OutAirVolFlow ),
			OutAirMassFlow( OutAirMassFlow ),
			AirInNode( AirInNode ),
			AirOutNode( AirOutNode ),
			OutsideAirNode( OutsideAirNode ),
			AirReliefNode( AirReliefNode ),
			MixedAirNode( MixedAirNode ),
			OAMixName( OAMixName ),
			OAMixType( OAMixType ),
			OAMixIndex( OAMixIndex ),
			FanName( FanName ),
			FanType( FanType ),
			FanType_Num( FanType_Num ),
			FanIndex( FanIndex ),
			DXCoilName( DXCoilName ),
			DXCoilType( DXCoilType ),
			DXCoilType_Num( DXCoilType_Num ),
			DXCoilIndex( DXCoilIndex ),
			CoilOutletNodeNum( CoilOutletNodeNum ),
			OpMode( OpMode ),
			FanPlace( FanPlace ),
			MaxIterIndex1( MaxIterIndex1 ),
			MaxIterIndex2( MaxIterIndex2 ),
			ConvergenceTol( ConvergenceTol ),
			PartLoadFrac( PartLoadFrac ),
			EMSOverridePartLoadFrac( EMSOverridePartLoadFrac ),
			EMSValueForPartLoadFrac( EMSValueForPartLoadFrac ),
			TotCoolEnergyRate( TotCoolEnergyRate ),
			TotCoolEnergy( TotCoolEnergy ),
			SensCoolEnergyRate( SensCoolEnergyRate ),
			SensCoolEnergy( SensCoolEnergy ),
			LatCoolEnergyRate( LatCoolEnergyRate ),
			LatCoolEnergy( LatCoolEnergy ),
			ElecPower( ElecPower ),
			ElecConsumption( ElecConsumption ),
			FanPartLoadRatio( FanPartLoadRatio ),
			CompPartLoadRatio( CompPartLoadRatio ),
			AvailManagerListName( AvailManagerListName ),
			AvailStatus( AvailStatus ),
			ZonePtr( ZonePtr ),
			HVACSizingIndex( HVACSizingIndex )
		{}

	};

	struct WindACNumericFieldData
	{
		// Members
		Array1D_string FieldNames;

		// Default Constructor
		WindACNumericFieldData()
		{}

		// Member Constructor
		WindACNumericFieldData(
			Array1_string const & FieldNames // Name of the HeatingCoil numeric field descriptions
			) :
			FieldNames(FieldNames)
		{}
	};

	// Object Data
	extern Array1D< WindACData > WindAC;
	extern Array1D< WindACNumericFieldData > WindACNumericFields; // holds window AC numeric input fields character field name

	// Functions

	void
	SimWindowAC(
		std::string const & CompName, // name of the window AC unit
		int const ZoneNum, // number of zone being served
		bool const FirstHVACIteration, // TRUE if 1st HVAC simulation of system timestep
		Real64 & PowerMet, // Sensible power supplied by window AC (W)
		Real64 & LatOutputProvided, // Latent add/removal supplied by window AC (kg/s), dehumid = negative
		int & CompIndex // component index
	);

	void
	GetWindowAC();

	void
	InitWindowAC(
		int const WindACNum, // number of the current window AC unit being simulated
		Real64 & QZnReq, // zone load (modified as needed) (W)
		int const ZoneNum, // index to zone
		bool const FirstHVACIteration // TRUE when first HVAC iteration
	);

	void
	SizeWindowAC( int const WindACNum );

	void
	SimCyclingWindowAC(
		int const WindACNum, // number of the current window AC unit being simulated
		int const ZoneNum, // number of zone being served !unused1208
		bool const FirstHVACIteration, // TRUE if 1st HVAC simulation of system timestep
		Real64 & PowerMet, // Sensible power supplied (W)
		Real64 const QZnReq, // Sensible load to be met (W)
		Real64 & LatOutputProvided // Latent power supplied (kg/s), negative = dehumidification
	);

	void
	ReportWindowAC( int const WindACNum ); // number of the current AC unit being simulated

	void
	CalcWindowACOutput(
		int const WindACNum, // Unit index in fan coil array
		bool const FirstHVACIteration, // flag for 1st HVAV iteration in the time step
		int const OpMode, // operating mode: CycFanCycCoil | ContFanCycCoil
		Real64 const PartLoadFrac, // unit part load fraction
		bool const HXUnitOn, // Flag to toggle HX heat recovery as needed
		Real64 & LoadMet // load met by unit (watts)
	);

	void
	ControlCycWindACOutput(
		int const WindACNum, // Unit index in fan coil array
		bool const FirstHVACIteration, // flag for 1st HVAV iteration in the time step
		int const OpMode, // operating mode: CycFanCycCoil | ContFanCycCoil
		Real64 const QZnReq, // cooling output needed by zone [W]
		Real64 & PartLoadFrac, // unit part load fraction
		bool & HXUnitOn // Used to control HX heat recovery as needed
	);

	int
	GetWindowACZoneInletAirNode( int const WindACNum );

	int
	GetWindowACOutAirNode( int const WindACNum );

	int
	GetWindowACReturnAirNode( int const WindACNum );

	int
	GetWindowACMixedAirNode( int const WindACNum );

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

} // WindowAC

} // EnergyPlus

#endif
