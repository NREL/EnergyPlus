#ifndef PlantManager_hh_INCLUDED
#define PlantManager_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/FArray1D.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>
#include <DataGlobals.hh>
#include <DataPlant.hh>

namespace EnergyPlus {

namespace PlantManager {

	// Using/Aliasing
	using DataPlant::BranchData;
	using DataPlant::MixerData;
	using DataPlant::PipeData;
	using DataPlant::SplitterData;

	// Data
	// MODULE PARAMETER DEFINITIONS
	extern int const MaxBranchLevel;
	extern int const Plant;
	extern int const Condenser;
	extern int const SupplyLoopPumpSingleSplitMix;
	extern int const DemandSingleSplitterMixer;
	extern int const TempSetPt;
	extern int const FlowSetPt;
	extern bool InitLoopEquip;
	extern bool GetCompSizFac;

	//MODULE DERIVED TYPE DEFINITIONS

	// MODULE VARIABLE DEFINITIONS
	extern int PlantSupplyLoopCase;
	extern int PlantDemandLoopCase;

	extern FArray1D_int SupplySideInletNode; // Node number for the supply side inlet
	extern FArray1D_int SupplySideOutletNode; // Node number for the supply side outlet
	extern FArray1D_int DemandSideInletNode; // Inlet node on the demand side

	// SUBROUTINE SPECIFICATIONS:
	//The following public routines are called from HVAC Manager
	//PUBLIC  CheckPlantLoopData      !called from SimHVAC

	// Types

	struct TempLoopData
	{
		// Members
		std::string Name; // Name of the component list
		// Loop connections
		std::string BranchList; // Branch list name for the half loop
		std::string ConnectList; // Connector list name for the half loop
		int TotalBranches; // Total number of branches on the loop
		FArray1D< BranchData > Branch; // Branch data
		FArray1D< SplitterData > Splitter; // Data for splitter on branch (if any)
		FArray1D< MixerData > Mixer; // Data for mixer on branch (if any)
		bool SplitterExists; // Logical Flag indication splitter exists in the half loop
		bool MixerExists; // Logical Flag indication mixer exists in the half loop
		bool BypassExists;
		bool LoopHasConnectionComp;

		// Default Constructor
		TempLoopData() :
			TotalBranches( 0 ),
			SplitterExists( false ),
			MixerExists( false ),
			BypassExists( false ),
			LoopHasConnectionComp( false )
		{}

		// Member Constructor
		TempLoopData(
			std::string const & Name, // Name of the component list
			std::string const & BranchList, // Branch list name for the half loop
			std::string const & ConnectList, // Connector list name for the half loop
			int const TotalBranches, // Total number of branches on the loop
			FArray1< BranchData > const & Branch, // Branch data
			FArray1< SplitterData > const & Splitter, // Data for splitter on branch (if any)
			FArray1< MixerData > const & Mixer, // Data for mixer on branch (if any)
			bool const SplitterExists, // Logical Flag indication splitter exists in the half loop
			bool const MixerExists, // Logical Flag indication mixer exists in the half loop
			bool const BypassExists,
			bool const LoopHasConnectionComp
		) :
			Name( Name ),
			BranchList( BranchList ),
			ConnectList( ConnectList ),
			TotalBranches( TotalBranches ),
			Branch( Branch ),
			Splitter( Splitter ),
			Mixer( Mixer ),
			SplitterExists( SplitterExists ),
			MixerExists( MixerExists ),
			BypassExists( BypassExists ),
			LoopHasConnectionComp( LoopHasConnectionComp )
		{}

	};

	struct LoopPipeData
	{
		// Members
		int NumPipes; // Total number of pipes
		FArray1D< PipeData > Pipe; // Pipe data, using definition from DataPlant

		// Default Constructor
		LoopPipeData() :
			NumPipes( 0 )
		{}

		// Member Constructor
		LoopPipeData(
			int const NumPipes, // Total number of pipes
			FArray1< PipeData > const & Pipe // Pipe data, using definition from DataPlant
		) :
			NumPipes( NumPipes ),
			Pipe( Pipe )
		{}

	};

	// Object Data
	extern FArray1D< LoopPipeData > LoopPipe;
	extern TempLoopData TempLoop; // =(' ',' ',' ',0, , , ,.FALSE.,.FALSE.,.FALSE.,.FALSE.,.FALSE.)

	// Functions

	void
	ManagePlantLoops(
		bool const FirstHVACIteration,
		bool & SimAirLoops, // True when the air loops need to be (re)simulated
		bool & SimZoneEquipment, // True when zone equipment components need to be (re)simulated
		bool & SimNonZoneEquipment, // True when non-zone equipment components need to be (re)simulated
		bool & SimPlantLoops, // True when some part of Plant needs to be (re)simulated
		bool & SimElecCircuits // True when electic circuits need to be (re)simulated
	);

	void
	GetPlantLoopData();

	void
	GetPlantInput();

	void
	SetupReports();

	void
	InitializeLoops( bool const FirstHVACIteration ); // true if first iteration of the simulation

	void
	ReInitPlantLoopsAtFirstHVACIteration();

	void
	UpdateNodeThermalHistory();

	void
	CheckPlantOnAbort();

	//SUBROUTINE CheckPlantLoopData

	//          ! SUBROUTINE INFORMATION:
	//          !       AUTHOR         B. Griffith
	//          !       DATE WRITTEN   May 2008
	//          !       MODIFIED       na
	//          !       RE-ENGINEERED  na

	//          ! PURPOSE OF THIS SUBROUTINE:
	//          ! This routine checks plant loop for input problems early in the simulation
	//          ! Some of the same checks also occur in CheckPlantOnAbort but those only execute if aborted
	//          ! Additional plant loop input checks can be added here.

	//          ! METHODOLOGY EMPLOYED:
	//          ! Test plant loop data for know issues.
	//          !  1. CR 7431.  detect presence of water coils and check for "ACTIVE" branch control.

	//          ! REFERENCES:
	//          ! na

	//          ! USE STATEMENTS:
	//          ! na

	//  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

	//          ! SUBROUTINE ARGUMENT DEFINITIONS:
	//          ! na

	//          ! SUBROUTINE PARAMETER DEFINITIONS:
	//          ! na

	//          ! INTERFACE BLOCK SPECIFICATIONS:
	//          ! na

	//          ! DERIVED TYPE DEFINITIONS:
	//          ! na

	//          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
	//  LOGICAL :: ShouldBeACTIVE
	//  INTEGER :: SideNum
	//  INTEGER :: numLoopSides
	//unused-1208  INTEGER :: SplitNum
	//  INTEGER :: BranchNum  ! DO loop counter for branches
	//  INTEGER :: CompNum    ! do loop for multiple components on a branch
	//  INTEGER :: LoopNum    ! DO loop counter for loops

	//  IF (.not. (TotNumLoops  > 0)) RETURN
	//  IF (.not.(ALLOCATED(PlantLoop))) RETURN

	//  DO LoopNum = 1, TotNumLoops
	//    numLoopSides = 2
	//    DO SideNum = 1, numLoopSides
	//      DO BranchNum =1, PlantLoop(LoopNum)%LoopSide(SideNum)%TotalBranches
	//        DO CompNum= 1,  PlantLoop(LoopNum)%LoopSide(SideNum)%Branch(BranchNum)%TotalComponents
	//          ShouldBeACTIVE = .FALSE.

	//          SELECT CASE (PlantLoop(LoopNum)%LoopSide(SideNum)%Branch(BranchNum)%Comp(CompNum)%TypeOf_Num)
	//          ! for now, check that all water coils are on "active" branch.
	//          CASE (TypeOf_WaterUseConnection)
	//            ShouldBeACTIVE = .TRUE.
	//          CASE (TypeOf_CoilWaterCooling)
	//            ShouldBeACTIVE = .TRUE.
	//          CASE (TypeOf_CoilWaterDetailedFlatCooling)
	//            ShouldBeACTIVE = .TRUE.
	//          CASE (TypeOf_CoilWaterSimpleHeating)
	//            ShouldBeACTIVE = .TRUE.
	//          CASE (TypeOf_CoilSteamAirHeating)
	//            ShouldBeACTIVE = .TRUE.

	//          CASE DEFAULT

	//          END SELECT

	//          If (ShouldBeACTIVE) THEN
	//            SELECT CASE (PlantLoop(LoopNum)%LoopSide(SideNum)%Branch(BranchNum)%Comp(CompNum)%FlowCtrl)

	//            CASE (ControlType_Unknown)
	//               CALL ShowWarningError('Found potential problem with Control Type for Branch named: '&
	//                             //TRIM(PlantLoop(LoopNum)%LoopSide(SideNum)%Branch(BranchNum)%Name) )
	//                             !DSU3 note, this confuses branch and components, should have reported out comp name as well.
	//               CALL ShowContinueError('This branch should (probably) be ACTIVE but has control type unknown')
	//            CASE (ControlType_Active)
	//              ! do nothing, this is correct control type.
	//            CASE (ControlType_Passive)
	//               CALL ShowSevereError('Found problem with Control Type for Branch named: '&
	//                             //TRIM(PlantLoop(LoopNum)%LoopSide(SideNum)%Branch(BranchNum)%Name) )
	//               CALL ShowContinueError('This branch should be ACTIVE but has control type PASSIVE')
	//            CASE (ControlType_SeriesActive)
	//              ! do nothing, should be okay. (? don't really understand SeriesActive though)
	//            CASE (ControlType_Bypass)
	//               CALL ShowSevereError('Found problem with Control Type for Branch named: '&
	//                             //TRIM(PlantLoop(LoopNum)%LoopSide(SideNum)%Branch(BranchNum)%Name) )
	//               CALL ShowContinueError('This branch should be ACTIVE but has control type Bypass')
	//            END SELECT
	//          ENDIF ! should be active
	//        ENDDO !comp num loop
	//      ENDDO ! branches
	//    ENDDO ! loop sides
	//  ENDDO ! plant loops

	//  RETURN

	//END SUBROUTINE CheckPlantLoopData

	void
	InitOneTimePlantSizingInfo( int const LoopNum ); // loop being initialized for sizing

	void
	SizePlantLoop(
		int const LoopNum, // Supply side loop being simulated
		bool const OkayToFinish
	);

	void
	SetupInitialPlantCallingOrder();

	void
	RevisePlantCallingOrder();

	int
	FindLoopSideInCallingOrder(
		int const LoopNum,
		int const LoopSide
	);

	void
	StoreAPumpOnCurrentTempLoop(
		int const LoopNum,
		int const LoopSideNum,
		int const BranchNum,
		int const CompNum,
		std::string const & PumpName,
		int const PumpOutletNode,
		bool const HasBranchPumps
	);

	void
	SetupBranchControlTypes();

	void
	CheckIfAnyPlant();

	//     NOTICE

	//     Copyright � 1996-2014 The Board of Trustees of the University of Illinois
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

} // PlantManager

} // EnergyPlus

#endif
