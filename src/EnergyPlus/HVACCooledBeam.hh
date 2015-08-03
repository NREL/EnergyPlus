#ifndef HVACCooledBeam_hh_INCLUDED
#define HVACCooledBeam_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>
#include <DataGlobals.hh>

namespace EnergyPlus {

namespace HVACCooledBeam {

	// Using/Aliasing

	// Data
	// MODULE PARAMETER DEFINITIONS:
	extern int const Passive_Cooled_Beam;
	extern int const Active_Cooled_Beam;
	extern Real64 const NomMassFlowPerBeam; // nominal water mass flow rate per beam [kg/s]
	extern Real64 const MinWaterVel; // minimum water velocity [m/s]
	extern Real64 const Coeff2;
	// DERIVED TYPE DEFINITIONS:

	// MODULE VARIABLE DECLARATIONS:
	extern Array1D_bool CheckEquipName;

	// INTEGER :: NumPassiveCB = 0
	// INTEGER :: NumActiveCB = 0
	extern int NumCB;

	// SUBROUTINE SPECIFICATIONS FOR MODULE HVACCooledBeam:

	// Types

	struct CoolBeamData
	{
		// Members
		// input data
		std::string Name; // name of unit
		std::string UnitType; // type of unit = AirTerminal:SingleDuct:ConstantVolume:CooledBeam
		int UnitType_Num; // index to type of unit = 1 (there's only 1 type so far)
		std::string CBType; // type of cooled beam: active | passive
		int CBType_Num; // index to type of cooled beam: passive=1; active=2
		std::string Sched; // availability schedule
		int SchedPtr; // index to schedule
		Real64 MaxAirVolFlow; // m3/s (autosizable)
		Real64 MaxAirMassFlow; // kg/s
		Real64 MaxCoolWaterVolFlow; // m3/s
		Real64 MaxCoolWaterMassFlow; // kg/s
		int AirInNode; // unit air inlet node number
		int AirOutNode; // unit air outlet node number
		int CWInNode; // chilled water inlet node
		int CWOutNode; // chilled water outlet node
		int ADUNum; // index of corresponding air distribution unit
		Real64 NumBeams; // number of beams in the zone
		Real64 BeamLength; // length of individual beam [m]
		Real64 DesInletWaterTemp; // design inlet water temperature [C]
		Real64 DesOutletWaterTemp; // design outlet water Temperature [c]
		Real64 CoilArea; // coil surface area per coil length [m2/m]
		Real64 a; // model parameter a
		Real64 n1; // model parameter n0
		Real64 n2; // model parameter n1
		Real64 n3; // model parameter n2
		Real64 a0; // model parameter a0
		Real64 K1; // model parameter K1
		Real64 n; // model parameter n
		Real64 Kin; // Coefficient of Induction Kin
		Real64 InDiam; // Leaving Pipe Inside Diameter
		// time step variables
		Real64 TWIn; // current inlet water temperature [C]
		Real64 TWOut; // current outlet water temperature [C]
		Real64 EnthWaterOut; // current outlet water enthalpy [J/kg]
		Real64 BeamFlow; // supply air flow per beam [m3/s]
		Real64 CoolWaterMassFlow; // chilled water mass flow rate [kg/s]
		Real64 BeamCoolingEnergy; // Cooled beam cooling energy of all beams in the zone [J]
		Real64 BeamCoolingRate; // Cooled beam cooling rate of all beams in the zone [W]
		Real64 SupAirCoolingEnergy; // Total cooling energy from supply air [J]
		Real64 SupAirCoolingRate; // Total cooling rate from supply air [W]
		Real64 SupAirHeatingEnergy; // Total cooling energy from supply air [J]
		Real64 SupAirHeatingRate; // Total cooling rate from supply air [W]
		int CWLoopNum; // cooling water plant loop index number
		int CWLoopSideNum; // cooling water plant loop side index
		int CWBranchNum; // cooling water plant loop branch index
		int CWCompNum; // cooling water plant loop component index
		int CBLoadReSimIndex;
		int CBMassFlowReSimIndex;
		int CBWaterOutletTempReSimIndex;

		// Default Constructor
		CoolBeamData() :
			UnitType_Num( 0 ),
			CBType_Num( 0 ),
			SchedPtr( 0 ),
			MaxAirVolFlow( 0.0 ),
			MaxAirMassFlow( 0.0 ),
			MaxCoolWaterVolFlow( 0.0 ),
			MaxCoolWaterMassFlow( 0.0 ),
			AirInNode( 0 ),
			AirOutNode( 0 ),
			CWInNode( 0 ),
			CWOutNode( 0 ),
			ADUNum( 0 ),
			NumBeams( 0.0 ),
			BeamLength( 0.0 ),
			DesInletWaterTemp( 0.0 ),
			DesOutletWaterTemp( 0.0 ),
			CoilArea( 0.0 ),
			a( 0.0 ),
			n1( 0.0 ),
			n2( 0.0 ),
			n3( 0.0 ),
			a0( 0.0 ),
			K1( 0.0 ),
			n( 0.0 ),
			Kin( 0.0 ),
			InDiam( 0.0 ),
			TWIn( 0.0 ),
			TWOut( 0.0 ),
			EnthWaterOut( 0.0 ),
			BeamFlow( 0.0 ),
			CoolWaterMassFlow( 0.0 ),
			BeamCoolingEnergy( 0.0 ),
			BeamCoolingRate( 0.0 ),
			SupAirCoolingEnergy( 0.0 ),
			SupAirCoolingRate( 0.0 ),
			SupAirHeatingEnergy( 0.0 ),
			SupAirHeatingRate( 0.0 ),
			CWLoopNum( 0 ),
			CWLoopSideNum( 0 ),
			CWBranchNum( 0 ),
			CWCompNum( 0 ),
			CBLoadReSimIndex( 0 ),
			CBMassFlowReSimIndex( 0 ),
			CBWaterOutletTempReSimIndex( 0 )
		{}

		// Member Constructor
		CoolBeamData(
			std::string const & Name, // name of unit
			std::string const & UnitType, // type of unit = AirTerminal:SingleDuct:ConstantVolume:CooledBeam
			int const UnitType_Num, // index to type of unit = 1 (there's only 1 type so far)
			std::string const & CBType, // type of cooled beam: active | passive
			int const CBType_Num, // index to type of cooled beam: passive=1; active=2
			std::string const & Sched, // availability schedule
			int const SchedPtr, // index to schedule
			Real64 const MaxAirVolFlow, // m3/s (autosizable)
			Real64 const MaxAirMassFlow, // kg/s
			Real64 const MaxCoolWaterVolFlow, // m3/s
			Real64 const MaxCoolWaterMassFlow, // kg/s
			int const AirInNode, // unit air inlet node number
			int const AirOutNode, // unit air outlet node number
			int const CWInNode, // chilled water inlet node
			int const CWOutNode, // chilled water outlet node
			int const ADUNum, // index of corresponding air distribution unit
			Real64 const NumBeams, // number of beams in the zone
			Real64 const BeamLength, // length of individual beam [m]
			Real64 const DesInletWaterTemp, // design inlet water temperature [C]
			Real64 const DesOutletWaterTemp, // design outlet water Temperature [c]
			Real64 const CoilArea, // coil surface area per coil length [m2/m]
			Real64 const a, // model parameter a
			Real64 const n1, // model parameter n0
			Real64 const n2, // model parameter n1
			Real64 const n3, // model parameter n2
			Real64 const a0, // model parameter a0
			Real64 const K1, // model parameter K1
			Real64 const n, // model parameter n
			Real64 const Kin, // Coefficient of Induction Kin
			Real64 const InDiam, // Leaving Pipe Inside Diameter
			Real64 const TWIn, // current inlet water temperature [C]
			Real64 const TWOut, // current outlet water temperature [C]
			Real64 const EnthWaterOut, // current outlet water enthalpy [J/kg]
			Real64 const BeamFlow, // supply air flow per beam [m3/s]
			Real64 const CoolWaterMassFlow, // chilled water mass flow rate [kg/s]
			Real64 const BeamCoolingEnergy, // Cooled beam cooling energy of all beams in the zone [J]
			Real64 const BeamCoolingRate, // Cooled beam cooling rate of all beams in the zone [W]
			Real64 const SupAirCoolingEnergy, // Total cooling energy from supply air [J]
			Real64 const SupAirCoolingRate, // Total cooling rate from supply air [W]
			Real64 const SupAirHeatingEnergy, // Total cooling energy from supply air [J]
			Real64 const SupAirHeatingRate, // Total cooling rate from supply air [W]
			int const CWLoopNum, // cooling water plant loop index number
			int const CWLoopSideNum, // cooling water plant loop side index
			int const CWBranchNum, // cooling water plant loop branch index
			int const CWCompNum, // cooling water plant loop component index
			int const CBLoadReSimIndex,
			int const CBMassFlowReSimIndex,
			int const CBWaterOutletTempReSimIndex
		) :
			Name( Name ),
			UnitType( UnitType ),
			UnitType_Num( UnitType_Num ),
			CBType( CBType ),
			CBType_Num( CBType_Num ),
			Sched( Sched ),
			SchedPtr( SchedPtr ),
			MaxAirVolFlow( MaxAirVolFlow ),
			MaxAirMassFlow( MaxAirMassFlow ),
			MaxCoolWaterVolFlow( MaxCoolWaterVolFlow ),
			MaxCoolWaterMassFlow( MaxCoolWaterMassFlow ),
			AirInNode( AirInNode ),
			AirOutNode( AirOutNode ),
			CWInNode( CWInNode ),
			CWOutNode( CWOutNode ),
			ADUNum( ADUNum ),
			NumBeams( NumBeams ),
			BeamLength( BeamLength ),
			DesInletWaterTemp( DesInletWaterTemp ),
			DesOutletWaterTemp( DesOutletWaterTemp ),
			CoilArea( CoilArea ),
			a( a ),
			n1( n1 ),
			n2( n2 ),
			n3( n3 ),
			a0( a0 ),
			K1( K1 ),
			n( n ),
			Kin( Kin ),
			InDiam( InDiam ),
			TWIn( TWIn ),
			TWOut( TWOut ),
			EnthWaterOut( EnthWaterOut ),
			BeamFlow( BeamFlow ),
			CoolWaterMassFlow( CoolWaterMassFlow ),
			BeamCoolingEnergy( BeamCoolingEnergy ),
			BeamCoolingRate( BeamCoolingRate ),
			SupAirCoolingEnergy( SupAirCoolingEnergy ),
			SupAirCoolingRate( SupAirCoolingRate ),
			SupAirHeatingEnergy( SupAirHeatingEnergy ),
			SupAirHeatingRate( SupAirHeatingRate ),
			CWLoopNum( CWLoopNum ),
			CWLoopSideNum( CWLoopSideNum ),
			CWBranchNum( CWBranchNum ),
			CWCompNum( CWCompNum ),
			CBLoadReSimIndex( CBLoadReSimIndex ),
			CBMassFlowReSimIndex( CBMassFlowReSimIndex ),
			CBWaterOutletTempReSimIndex( CBWaterOutletTempReSimIndex )
		{}

	};

	// Object Data
	extern Array1D< CoolBeamData > CoolBeam;

	// Functions

	void
	SimCoolBeam(
		std::string const & CompName, // name of the cooled beam unit
		bool const FirstHVACIteration, // TRUE if first HVAC iteration in time step
		int const ZoneNum, // index of zone served by the unit
		int const ZoneNodeNum, // zone node number of zone served by the unit
		int & CompIndex, // which cooled beam unit in data structure
		Real64 & NonAirSysOutput // convective cooling by the beam system [W]
	);

	void
	GetCoolBeams();

	void
	InitCoolBeam(
		int const CBNum, // number of the current cooled beam unit being simulated
		bool const FirstHVACIteration // TRUE if first air loop solution this HVAC step
	);

	void
	SizeCoolBeam( int const CBNum );

	void
	ControlCoolBeam(
		int const CBNum, // number of the current unit being simulated
		int const ZoneNum, // number of zone being served
		int const ZoneNodeNum, // zone node number
		bool const FirstHVACIteration, // TRUE if 1st HVAC simulation of system timestep
		Real64 & NonAirSysOutput // convective cooling by the beam system [W]
	);

	void
	CalcCoolBeam(
		int const CBNum, // Unit index
		int const ZoneNode, // zone node number
		Real64 const CWFlow, // cold water flow [kg/s]
		Real64 & LoadMet, // load met by unit [W]
		Real64 & TWOut // chilled water outlet temperature [C]
	);

	Real64
	CoolBeamResidual(
		Real64 const CWFlow, // cold water flow rate in kg/s
		Array1< Real64 > const & Par
	);

	void
	UpdateCoolBeam( int const CBNum );

	void
	ReportCoolBeam( int const CBNum );

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

} // HVACCooledBeam

} // EnergyPlus

#endif
