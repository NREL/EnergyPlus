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

#ifndef PlantCondLoopOperation_hh_INCLUDED
#define PlantCondLoopOperation_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Optional.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>

namespace EnergyPlus {

namespace PlantCondLoopOperation {

	// Data
	//MODULE PARAMETER DEFINITIONS
	extern int const HeatingOp; // Constant for Heating Operation
	extern int const CoolingOp; // Constant for Cooling Operation
	extern int const DualOp; // Constant for Cooling or Heating Operation

	extern bool const TurnItemOn; // Convenient for calling TurnPlantItemOnOff instead of hardwired true/false
	extern bool const TurnItemOff; // Convenient for calling TurnPlantItemOnOff instead of hardwired true/false

	//MODULE VARIABLE DECLARATIONS:

	//SUBROUTINE SPECIFICATIONS FOR MODULE  !SUBROUTINE SPECIFICATIONS FOR MODULE
	//Driver Routines
	//Get Input Routines
	//Initialization Routines
	//Load Distribution/Calculation Routines

	//ON/OFF Utility Routines

	//PLANT EMS Utility Routines

	// Functions
	void
	clear_state();

	void
	ManagePlantLoadDistribution(
		int const LoopNum, // PlantLoop data structure loop counter
		int const LoopSideNum, // PlantLoop data structure LoopSide counter
		int const BranchNum, // PlantLoop data structure branch counter
		int const CompNum, // PlantLoop data structure component counter
		Real64 & LoopDemand,
		Real64 & RemLoopDemand,
		bool const FirstHVACIteration,
		bool & LoopShutDownFlag, // EMS flag to tell loop solver to shut down pumps
		Optional_bool LoadDistributionWasPerformed = _
	);

	// Beginning of GetInput subroutines for the Module
	//******************************************************************************

	void
	GetPlantOperationInput( bool & GetInputOK );

	void
	GetOperationSchemeInput();

	void
	FindRangeBasedOrUncontrolledInput(
		std::string & CurrentModuleObject, // for ease in renaming
		int const NumSchemes, // May be set here and passed on
		int const LoopNum, // May be set here and passed on
		int const SchemeNum, // May be set here and passed on
		bool & ErrorsFound // May be set here and passed on
	);

	void
	FindDeltaTempRangeInput(
		std::string & CurrentModuleObject, // for ease in renaming
		int const NumSchemes, // May be set here and passed on
		int const LoopNum, // May be set here and passed on
		int const SchemeNum, // May be set here and passed on
		bool & ErrorsFound // May be set here and passed on
	);

	void
	LoadEquipList(
		int const LoopNum, // May be set here and passed on
		int const SchemeNum, // May be set here and passed on
		int const ListNum, // May be set here and passed on
		bool & ErrorsFound // May be set here and passed on
	);

	void
	FindCompSPInput(
		std::string & CurrentModuleObject, // for ease in renaming
		int const NumSchemes, // May be set here and passed on
		int const LoopNum, // May be set here and passed on
		int const SchemeNum, // May be set here and passed on
		bool & ErrorsFound // May be set here and passed on
	);

	void
	GetUserDefinedOpSchemeInput(
		std::string & CurrentModuleObject, // for ease in renaming
		int const NumSchemes, // May be set here and passed on
		int const LoopNum, // May be set here and passed on
		int const SchemeNum, // May be set here and passed on
		bool & ErrorsFound // May be set here and passed on
	);

	// End of GetInput subroutines for the Module
	//******************************************************************************

	// Beginning Initialization Section of the Plant Loop Module
	//******************************************************************************

	void
	InitLoadDistribution( bool const FirstHVACIteration );

	// End Initialization Section of the Plant Loop Module
	//******************************************************************************

	// Begin Load Calculation/Distribution Section of the Plant Loop Module
	//******************************************************************************

	void
	DistributePlantLoad(
		int const LoopNum,
		int const LoopSideNum,
		int const CurSchemePtr, // use as index in PlantLoop()OpScheme() data structure
		int const ListPtr, // use as index in PlantLoop()OpScheme() data structure
		Real64 const LoopDemand,
		Real64 & RemLoopDemand
	);

	void
	AdjustChangeInLoadForLastStageUpperRangeLimit(
		int const LoopNum, // component topology
		int const CurOpSchemePtr, // currect active operation scheme
		int const CurEquipListPtr, // current equipment list
		Real64 & ChangeInLoad // positive magnitude of load change
	);

	void
	AdjustChangeInLoadByHowServed(
		int const LoopNum, // component topology
		int const LoopSideNum, // component topology
		int const BranchNum, // component topology
		int const CompNum, // component topology
		Real64 & ChangeInLoad // positive magnitude of load change
	);

	void
	FindCompSPLoad(
		int const LoopNum,
		int const LoopSideNum,
		int const BranchNum,
		int const CompNum,
		int const OpNum // index for Plant()%LoopSide()%Branch()%Comp()%OpScheme()
	);

	void
	DistributeUserDefinedPlantLoad(
		int const LoopNum,
		int const LoopSideNum,
		int const BranchNum,
		int const CompNum,
		int const CurCompLevelOpNum, // index for Plant()%LoopSide()%Branch()%Comp()%OpScheme()
		int const CurSchemePtr,
		Real64 const LoopDemand,
		Real64 & RemLoopDemand
	);

	// End Load Calculation/Distribution Section of the Plant Loop Module
	//******************************************************************************

	//********************************

	Real64
	FindRangeVariable(
		int const LoopNum, // PlantLoop data structure loop counter
		int const CurSchemePtr, // set by PL()%LoopSide()%Branch()%Comp()%OpScheme()%OpSchemePtr
		int const CurSchemeType // identifier set in PlantData
	);

	//********************************

	// Begin Plant Loop ON/OFF Utility Subroutines
	//******************************************************************************

	void
	TurnOnPlantLoopPipes(
		int const LoopNum,
		int const LoopSideNum
	);

	void
	TurnOffLoopEquipment( int const LoopNum );

	void
	TurnOffLoopSideEquipment(
		int const LoopNum,
		int const LoopSideNum
	);

	// End Plant Loop ON/OFF Utility Subroutines
	//******************************************************************************

	// Begin Plant EMS Control Routines
	//******************************************************************************

	void
	SetupPlantEMSActuators();

	void
	ActivateEMSControls(
		int const LoopNum,
		int const LoopSideNum,
		int const BranchNum,
		int const CompNum,
		bool & LoopShutDownFlag
	);

	void
	AdjustChangeInLoadByEMSControls(
		int const LoopNum,
		int const LoopSideNum,
		int const BranchNum,
		int const CompNum,
		Real64 & ChangeInLoad // positive magnitude of load change
	);

	//*END PLANT EMS CONTROL ROUTINES!
	//******************************************************************************

} // PlantCondLoopOperation

} // EnergyPlus

#endif
