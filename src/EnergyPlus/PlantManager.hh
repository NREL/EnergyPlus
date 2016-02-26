// EnergyPlus, Copyright (c) 1996-2016, The Board of Trustees of the University of Illinois and
// The Regents of the University of California, through Lawrence Berkeley National Laboratory
// (subject to receipt of any required approvals from the U.S. Dept. of Energy). All rights
// reserved.
//
// If you have questions about your rights to use or distribute this software, please contact
// Berkeley Lab's Innovation & Partnerships Office at IPO@lbl.gov.
//
// NOTICE: This Software was developed under funding from the U.S. Department of Energy and the
// U.S. Government consequently retains certain rights. As such, the U.S. Government has been
// granted for itself and others acting on its behalf a paid-up, nonexclusive, irrevocable,
// worldwide license in the Software to reproduce, distribute copies to the public, prepare
// derivative works, and perform publicly and display publicly, and to permit others to do so.
//
// Redistribution and use in source and binary forms, with or without modification, are permitted
// provided that the following conditions are met:
//
// (1) Redistributions of source code must retain the above copyright notice, this list of
//     conditions and the following disclaimer.
//
// (2) Redistributions in binary form must reproduce the above copyright notice, this list of
//     conditions and the following disclaimer in the documentation and/or other materials
//     provided with the distribution.
//
// (3) Neither the name of the University of California, Lawrence Berkeley National Laboratory,
//     the University of Illinois, U.S. Dept. of Energy nor the names of its contributors may be
//     used to endorse or promote products derived from this software without specific prior
//     written permission.
//
// (4) Use of EnergyPlus(TM) Name. If Licensee (i) distributes the software in stand-alone form
//     without changes from the version obtained under this License, or (ii) Licensee makes a
//     reference solely to the software portion of its product, Licensee must refer to the
//     software as "EnergyPlus version X" software, where "X" is the version number Licensee
//     obtained under this License and may not use a different name for the software. Except as
//     specifically required in this Section (4), Licensee shall not use in a company name, a
//     product name, in advertising, publicity, or other promotional activities any name, trade
//     name, trademark, logo, or other designation of "EnergyPlus", "E+", "e+" or confusingly
//     similar designation, without Lawrence Berkeley National Laboratory's prior written consent.
//
// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR
// IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY
// AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR
// CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
// CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
// SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
// THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
// OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
// POSSIBILITY OF SUCH DAMAGE.
//
// You are under no obligation whatsoever to provide any bug fixes, patches, or upgrades to the
// features, functionality or performance of the source code ("Enhancements") to anyone; however,
// if you choose to make your Enhancements available either publicly, or directly to Lawrence
// Berkeley National Laboratory, without imposing a separate written license agreement for such
// Enhancements, then you hereby grant the following license: a non-exclusive, royalty-free
// perpetual license to install, use, modify, prepare derivative works, incorporate into other
// computer software, distribute, and sublicense such enhancements or derivative works thereof,
// in binary and source code form.

#ifndef PlantManager_hh_INCLUDED
#define PlantManager_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>

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

	extern Array1D_int SupplySideInletNode; // Node number for the supply side inlet
	extern Array1D_int SupplySideOutletNode; // Node number for the supply side outlet
	extern Array1D_int DemandSideInletNode; // Inlet node on the demand side

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
		Array1D< BranchData > Branch; // Branch data
		Array1D< SplitterData > Splitter; // Data for splitter on branch (if any)
		Array1D< MixerData > Mixer; // Data for mixer on branch (if any)
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

	};

	struct LoopPipeData
	{
		// Members
		int NumPipes; // Total number of pipes
		Array1D< PipeData > Pipe; // Pipe data, using definition from DataPlant

		// Default Constructor
		LoopPipeData() :
			NumPipes( 0 )
		{}

	};

	// Object Data
	extern Array1D< LoopPipeData > LoopPipe;
	extern TempLoopData TempLoop; // =(' ',' ',' ',0, , , ,.FALSE.,.FALSE.,.FALSE.,.FALSE.,.FALSE.)

	// Functions

	void
	clear_state();

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
	ResizePlantLoopLevelSizes(
		int const LoopNum
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

} // PlantManager

} // EnergyPlus

#endif
