#ifndef OutsideEnergySources_hh_INCLUDED
#define OutsideEnergySources_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>
#include <DataGlobals.hh>

namespace EnergyPlus {

namespace OutsideEnergySources {

	// Using/Aliasing

	// Data
	//MODULE PARAMETER DEFINITIONS
	extern int const EnergyType_DistrictHeating;
	extern int const EnergyType_DistrictCooling;

	// DERIVED TYPE DEFINITIONS

	//MODULE VARIABLE DECLARATIONS:
	extern int NumDistrictUnits;

	// SUBROUTINE SPECIFICATIONS FOR MODULE OutsideEnergySources

	// Types

	struct OutsideEnergySourceSpecs
	{
		// Members
		std::string PlantLoopID; // main plant loop ID
		std::string SecndryLoopID; // secondary chiller loop (cond loop) ID
		std::string ScheduleID; // equipment availability schedule
		std::string Name; // user identifier
		Real64 NomCap; // design nominal capacity of district service
		bool NomCapWasAutoSized; // ture if Nominal Capacity was autosize on input
		int CapFractionSchedNum; // capacity modifier schedule number
		int InletNodeNum; // Node number on the inlet side of the plant
		int OutletNodeNum; // Node number on the inlet side of the plant
		Real64 EnergyTransfer; // cooling energy provided in time step
		Real64 EnergyRate; // cooling power
		int EnergyType; // flag for district heating OR cooling
		int MassFlowReSimIndex;
		//loop topology variables
		int LoopNum;
		int LoopSideNum;
		int BranchNum;
		int CompNum;
		//flags
		bool OneTimeInitFlag;
		bool BeginEnvrnInitFlag;
		bool CheckEquipName;

		// Default Constructor
		OutsideEnergySourceSpecs() :
			NomCap( 0.0 ),
			NomCapWasAutoSized( false ),
			CapFractionSchedNum( 0 ),
			InletNodeNum( 0 ),
			OutletNodeNum( 0 ),
			EnergyTransfer( 0.0 ),
			EnergyRate( 0.0 ),
			EnergyType( 0 ),
			MassFlowReSimIndex( 0 ),
			LoopNum( 0 ),
			LoopSideNum( 0 ),
			BranchNum( 0 ),
			CompNum( 0 ),
			OneTimeInitFlag( true ),
			BeginEnvrnInitFlag( true ),
			CheckEquipName( true )
		{}

	};

	struct ReportVars
	{
		// Members
		Real64 MassFlowRate;
		Real64 InletTemp;
		Real64 OutletTemp;
		Real64 EnergyTransfer;

		// Default Constructor
		ReportVars() :
			MassFlowRate( 0.0 ),
			InletTemp( 0.0 ),
			OutletTemp( 0.0 ),
			EnergyTransfer( 0.0 )
		{}
	};

	// Object Data
	extern Array1D< OutsideEnergySourceSpecs > EnergySource;
	extern Array1D< ReportVars > EnergySourceReport;

	// Functions

	void
	SimOutsideEnergy(
		std::string const & EnergyType,
		std::string const & EquipName,
		int const EquipFlowCtrl, // Flow control mode for the equipment
		int & CompIndex,
		bool const RunFlag,
		bool const InitLoopEquip,
		Real64 & MyLoad,
		Real64 & MaxCap,
		Real64 & MinCap,
		Real64 & OptCap,
		bool const FirstHVACIteration
	);

	// End OutsideEnergySources Module Driver Subroutines
	//******************************************************************************

	// Beginning of OutsideEnergySources Module Get Input subroutines
	//******************************************************************************

	void
	GetOutsideEnergySourcesInput();

	// End of Get Input subroutines for the OutsideEnergySources Module
	//******************************************************************************

	// Beginning Initialization Section of the OutsideEnergySources Module
	//******************************************************************************

	void
	InitSimVars(
		int const EnergySourceNum, // Which item being initialized
		Real64 & MassFlowRate,
		Real64 & InletTemp,
		Real64 & OutletTemp,
		Real64 const MyLoad
	);

	// End Initialization Section of the OutsideEnergySources Module
	//******************************************************************************

	// Beginning of OutsideEnergySources Module Utility Subroutines
	// *****************************************************************************

	void
	SizeDistrictEnergy(
		int const EnergySourceNum
	);

	void
	SimDistrictEnergy(
		bool const RunFlag,
		int const DistrictEqNum,
		Real64 & MyLoad,
		Real64 const MassFlowRate,
		Real64 const InletTemp,
		Real64 & OutletTemp
	);

	// End of OutsideEnergySources Module Utility Subroutines
	// *****************************************************************************

	// Beginning of Record Keeping subroutines for the OutsideEnergySources Module
	// *****************************************************************************

	void
	UpdateRecords(
		Real64 const MyLoad,
		int const EqNum,
		Real64 const MassFlowRate,
		Real64 const OutletTemp
	);

	// End of Record Keeping subroutines for the OutsideEnergySources Module
	// *****************************************************************************

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

} // OutsideEnergySources

} // EnergyPlus

#endif
