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

// C++ Headers
#include <cmath>

// ObjexxFCL Headers
#include <ObjexxFCL/Array.functions.hh>
#include <ObjexxFCL/Fmath.hh>
#include <ObjexxFCL/gio.hh>
#include <ObjexxFCL/string.functions.hh>

// EnergyPlus Headers
#include <PlantChillers.hh>
#include <BranchNodeConnections.hh>
#include <CurveManager.hh>
#include <DataBranchAirLoopPlant.hh>
#include <DataEnvironment.hh>
#include <DataHVACGlobals.hh>
#include <DataIPShortCuts.hh>
#include <DataLoopNode.hh>
#include <DataPlant.hh>
#include <DataPrecisionGlobals.hh>
#include <DataSizing.hh>
#include <EMSManager.hh>
#include <FluidProperties.hh>
#include <General.hh>
#include <GeneralRoutines.hh>
#include <GlobalNames.hh>
#include <InputProcessor.hh>
#include <NodeInputManager.hh>
#include <OutAirNodeManager.hh>
#include <OutputProcessor.hh>
#include <OutputReportPredefined.hh>
#include <PlantLocation.hh>
#include <PlantUtilities.hh>
#include <Psychrometrics.hh>
#include <ReportSizingManager.hh>
#include <ScheduleManager.hh>
#include <UtilityRoutines.hh>

namespace EnergyPlus {

namespace PlantChillers {

	// MODULE INFORMATION:
	//       AUTHOR         Dan Fisher / Brandon Anderson
	//       DATE WRITTEN   September 2000
	//       MODIFIED       Richard Liesen Nov-Dec 2001; Jan 2002
	//                      Chandan Sharma, FSEC, February 2010, Added basin heater
	//       RE-ENGINEERED  Edwin: Merged Four Chiller Modules Into One

	// PURPOSE OF THIS MODULE:
	// This module simulates the performance of the Electric vapor
	// compression Chillers, Gas Turbine Chillers, Engine Drivent chillers, and
	// Constant COP chillers

	// METHODOLOGY EMPLOYED:
	// Called by plantloopequipment, model accepts inputs, and calculates a
	// thermal response using new plant routines such as SetComponentFlowRate

	// REFERENCES:
	// 1. BLAST Users Manual

	// Using/Aliasing
	using namespace DataPrecisionGlobals;
	using namespace DataLoopNode;
	using DataGlobals::MaxNameLength;
	using DataGlobals::NumOfTimeStepInHour;
	using DataGlobals::InitConvTemp;
	using DataGlobals::WarmupFlag;
	using DataGlobals::DisplayExtraWarnings;
	using DataHVACGlobals::SmallWaterVolFlow;
	using DataPlant::DeltaTempTol;
	using DataPlant::TypeOf_Chiller_EngineDriven;
	using DataPlant::TypeOf_Chiller_Electric;
	using DataPlant::TypeOf_Chiller_CombTurbine;
	using DataPlant::TypeOf_Chiller_ConstCOP;
	using General::TrimSigDigits;
	using General::RoundSigDigits;

	// Data
	//MODULE PARAMETER DEFINITIONS:
	// Parameters for use in Chillers
	int const AirCooled( 1 );
	int const WaterCooled( 2 );
	int const EvapCooled( 3 );
	Real64 const KJtoJ( 1000.0 ); // convert Kjoules to joules

	//chiller flow modes
	int const FlowModeNotSet( 200 );
	int const ConstantFlow( 201 );
	int const NotModulated( 202 );
	int const LeavingSetPointModulated( 203 );

	static std::string const BlankString;

	// MODULE VARIABLE DECLARATIONS:
	int NumElectricChillers( 0 ); // number of Electric chillers specified in input
	int NumEngineDrivenChillers( 0 ); // number of EngineDriven chillers specified in input
	int NumGTChillers( 0 ); // number of GT chillers specified in input
	int NumConstCOPChillers( 0 );
	bool GetInputFlag( true );

	// Object Data
	Array1D< ElectricChillerSpecs > ElectricChiller; // dimension to number of machines
	Array1D< EngineDrivenChillerSpecs > EngineDrivenChiller; // dimension to number of machines
	Array1D< GTChillerSpecs > GTChiller; // dimension to number of machines
	Array1D< ConstCOPChillerSpecs > ConstCOPChiller; // dimension to number of machines

	// MODULE SUBROUTINES:

	// Beginning of Electric Chiller Module Driver Subroutines
	//*************************************************************************

	// Functions

	void
	clear_state()
	{
		NumElectricChillers = 0;
		NumEngineDrivenChillers = 0;
		NumGTChillers = 0;
		NumConstCOPChillers = 0;
		GetInputFlag = true;
		ElectricChiller.deallocate();
		EngineDrivenChiller.deallocate();
		GTChiller.deallocate();
		ConstCOPChiller.deallocate();
	}

	PlantComponent * BaseChillerSpecs::factory( int objectType, std::string objectName )
	{
		if ( GetInputFlag ) {
			GetElectricChillerInput();
			GetEngineDrivenChillerInput();
			GetGTChillerInput();
			GetConstCOPChillerInput();
			GetInputFlag = false;
		}
		if ( objectType == TypeOf_Chiller_Electric ) {
			for ( auto & chiller : ElectricChiller ) {
				if ( chiller.Name == objectName ) {
					return &chiller;
				}
			}
		} else if ( objectType == TypeOf_Chiller_EngineDriven ) {
			for ( auto & chiller : EngineDrivenChiller ) {
				if ( chiller.Name == objectName ) {
					return &chiller;
				}
			}
		} else if ( objectType == TypeOf_Chiller_CombTurbine ) {
			for ( auto & chiller : GTChiller ) {
				if ( chiller.Name == objectName ) {
					return &chiller;
				}
			}
		} else if ( objectType == TypeOf_Chiller_ConstCOP ) {
			for ( auto & chiller : ConstCOPChiller ) {
				if ( chiller.Name == objectName ) {
					return &chiller;
				}
			}
		}
		ShowFatalError( "Chiller factory, couldn't find chiller named: " + objectName );
		// shut up the compiler
		return nullptr;
	}

	void ElectricChillerSpecs::simulate( const PlantLocation & calledFromLocation, bool const FirstHVACIteration, Real64 & CurLoad )
	{
		bool RunFlag = DataPlant::PlantLoop( calledFromLocation.loopNum ).LoopSide( calledFromLocation.loopSideNum ).Branch( calledFromLocation.branchNum ).Comp( calledFromLocation.compNum ).ON;
		int EquipFlowCtrl = DataPlant::PlantLoop( calledFromLocation.loopNum ).LoopSide( calledFromLocation.loopSideNum ).Branch( calledFromLocation.branchNum ).Comp( calledFromLocation.compNum ).FlowCtrl;
		if ( calledFromLocation.loopNum == this->CWLoopNum ) { // chilled water loop

			this->InitElectricChiller( RunFlag, CurLoad );
			this->CalcElectricChillerModel( CurLoad, EquipFlowCtrl, RunFlag );
			this->UpdateElectricChillerRecords( CurLoad, RunFlag );

		} else if ( calledFromLocation.loopNum == this->CDLoopNum ) { // condenser loop
			PlantUtilities::UpdateChillerComponentCondenserSide( this->CDLoopNum, this->CDLoopSideNum, TypeOf_Chiller_Electric, this->CondInletNodeNum, this->CondOutletNodeNum, this->reports.QCond, this->reports.CondInletTemp, this->reports.CondOutletTemp, this->reports.Condmdot, FirstHVACIteration );
		} else if ( calledFromLocation.loopNum == this->HRLoopNum ) { // heat recovery loop
			PlantUtilities::UpdateComponentHeatRecoverySide( this->HRLoopNum, this->HRLoopSideNum, TypeOf_Chiller_Electric, this->HeatRecInletNodeNum, this->HeatRecOutletNodeNum, this->reports.QHeatRecovery, this->reports.HeatRecInletTemp, this->reports.HeatRecOutletTemp, this->reports.HeatRecMassFlow, FirstHVACIteration );
		}

	}

	void ElectricChillerSpecs::onInitLoopEquip( const PlantLocation & calledFromLocation )
	{
		bool RunFlag = DataPlant::PlantLoop( calledFromLocation.loopNum ).LoopSide( calledFromLocation.loopSideNum ).Branch( calledFromLocation.branchNum ).Comp( calledFromLocation.compNum ).ON;
		Real64 initLoad = 0.0;
		this->InitElectricChiller( RunFlag, initLoad );
		if ( calledFromLocation.loopNum == this->CWLoopNum ) { // chilled water loop
			this->SizeElectricChiller();
		}
	}

	void ElectricChillerSpecs::getSizingFactor( Real64 & SizFac )
	{
		SizFac = this->SizFac;
	}

	void ElectricChillerSpecs::getDesignTemperatures( Real64 & TempDesCondIn, Real64 & TempDesEvapOut )
	{
		TempDesEvapOut = this->TempDesEvapOut;
		TempDesCondIn = this->TempDesCondIn;
	}

	void ElectricChillerSpecs::getDesignCapacities( const PlantLocation & calledFromLocation, Real64 & MaxLoad, Real64 & MinLoad, Real64 & OptLoad )
	{
		if ( calledFromLocation.loopNum == this->CWLoopNum ) { // chilled water loop
			MinLoad = this->NomCap * this->MinPartLoadRat;
			MaxLoad = this->NomCap * this->MaxPartLoadRat;
			OptLoad = this->NomCap * this->OptPartLoadRat;
		} else {
			MinLoad = 0.0;
			MaxLoad = 0.0;
			OptLoad = 0.0;
		}
	}

	void EngineDrivenChillerSpecs::simulate( const PlantLocation & calledFromLocation, bool const FirstHVACIteration, Real64 & CurLoad )
	{
		bool RunFlag = DataPlant::PlantLoop( calledFromLocation.loopNum ).LoopSide( calledFromLocation.loopSideNum ).Branch( calledFromLocation.branchNum ).Comp( calledFromLocation.compNum ).ON;
		int EquipFlowCtrl = DataPlant::PlantLoop( calledFromLocation.loopNum ).LoopSide( calledFromLocation.loopSideNum ).Branch( calledFromLocation.branchNum ).Comp( calledFromLocation.compNum ).FlowCtrl;
		if ( calledFromLocation.loopNum == this->CWLoopNum ) { // chilled water loop
			this->InitEngineDrivenChiller( RunFlag, CurLoad );
			this->CalcEngineDrivenChillerModel( CurLoad, RunFlag, EquipFlowCtrl );
			this->UpdateEngineDrivenChiller( CurLoad, RunFlag );
		} else if ( calledFromLocation.loopNum == this->CDLoopNum ) { // condenser loop
			PlantUtilities::UpdateChillerComponentCondenserSide( this->CDLoopNum, this->CDLoopSideNum, TypeOf_Chiller_EngineDriven, this->CondInletNodeNum, this->CondOutletNodeNum, this->reports.QCond, this->reports.CondInletTemp, this->reports.CondOutletTemp, this->reports.Condmdot, FirstHVACIteration );
		} else if ( calledFromLocation.loopNum == this->HRLoopNum ) { // heat recovery loop
			PlantUtilities::UpdateComponentHeatRecoverySide( this->HRLoopNum, this->HRLoopSideNum, TypeOf_Chiller_EngineDriven, this->HeatRecInletNodeNum, this->HeatRecOutletNodeNum, this->reports.QTotalHeatRecovered, this->reports.HeatRecInletTemp, this->reports.HeatRecOutletTemp, this->reports.HeatRecMdot, FirstHVACIteration );
		}

	}

	void EngineDrivenChillerSpecs::onInitLoopEquip( const PlantLocation & calledFromLocation )
	{

		bool RunFlag = DataPlant::PlantLoop( calledFromLocation.loopNum ).LoopSide( calledFromLocation.loopSideNum ).Branch( calledFromLocation.branchNum ).Comp( calledFromLocation.compNum ).ON;
		Real64 initLoad = 0.0;
		this->InitEngineDrivenChiller( RunFlag, initLoad );

		if ( calledFromLocation.loopNum == this->CWLoopNum ) {
			this->SizeEngineDrivenChiller();
		}
	}

	void EngineDrivenChillerSpecs::getSizingFactor( Real64 & SizFac )
	{
		SizFac = this->SizFac;
	}

	void EngineDrivenChillerSpecs::getDesignTemperatures( Real64 & TempDesCondIn, Real64 & TempDesEvapOut )
	{
		TempDesEvapOut = this->TempDesEvapOut;
		TempDesCondIn = this->TempDesCondIn;
	}

	void EngineDrivenChillerSpecs::getDesignCapacities( const PlantLocation & calledFromLocation, Real64 & MaxLoad, Real64 & MinLoad, Real64 & OptLoad )
	{
		if ( calledFromLocation.loopNum == this->CWLoopNum ) { // chilled water loop
			MinLoad = this->NomCap * this->MinPartLoadRat;
			MaxLoad = this->NomCap * this->MaxPartLoadRat;
			OptLoad = this->NomCap * this->OptPartLoadRat;
		} else {
			MinLoad = 0.0;
			MaxLoad = 0.0;
			OptLoad = 0.0;
		}
	}

	void GTChillerSpecs::simulate( const PlantLocation & calledFromLocation, bool const FirstHVACIteration, Real64 & CurLoad )
	{
		bool RunFlag = DataPlant::PlantLoop( calledFromLocation.loopNum ).LoopSide( calledFromLocation.loopSideNum ).Branch( calledFromLocation.branchNum ).Comp( calledFromLocation.compNum ).ON;
		int EquipFlowCtrl = DataPlant::PlantLoop( calledFromLocation.loopNum ).LoopSide( calledFromLocation.loopSideNum ).Branch( calledFromLocation.branchNum ).Comp( calledFromLocation.compNum ).FlowCtrl;
		if ( calledFromLocation.loopNum == this->CWLoopNum ) { // chilled water loop
			this->InitGTChiller(  RunFlag, CurLoad );
			this->CalcGTChillerModel(  CurLoad, RunFlag, EquipFlowCtrl );
			this->UpdateGTChillerRecords( CurLoad, RunFlag );

		} else if ( calledFromLocation.loopNum == this->CDLoopNum ) { // condenser loop
			PlantUtilities::UpdateChillerComponentCondenserSide( this->CDLoopNum, this->CDLoopSideNum, TypeOf_Chiller_CombTurbine, this->CondInletNodeNum, this->CondOutletNodeNum, this->reports.QCond, this->reports.CondInletTemp, this->reports.CondOutletTemp, this->reports.Condmdot, FirstHVACIteration );
		} else if ( calledFromLocation.loopNum == this->HRLoopNum ) { // heat recovery loop
			PlantUtilities::UpdateComponentHeatRecoverySide( this->HRLoopNum, this->HRLoopSideNum, TypeOf_Chiller_CombTurbine, this->HeatRecInletNodeNum, this->HeatRecOutletNodeNum, this->reports.HeatRecLubeRate, this->reports.HeatRecInletTemp, this->reports.HeatRecOutletTemp, this->reports.HeatRecMdot, FirstHVACIteration );
		}

	}

	void GTChillerSpecs::onInitLoopEquip( const PlantLocation & calledFromLocation )
	{

		bool RunFlag = DataPlant::PlantLoop( calledFromLocation.loopNum ).LoopSide( calledFromLocation.loopSideNum ).Branch( calledFromLocation.branchNum ).Comp( calledFromLocation.compNum ).ON;
		Real64 initLoad = 0.0;
		this->InitGTChiller( RunFlag, initLoad );

		if ( calledFromLocation.loopNum == this->CWLoopNum ) {
			this->SizeGTChiller();
		}
	}

	void GTChillerSpecs::getSizingFactor( Real64 & SizFac )
	{
		SizFac = this->SizFac;
	}

	void GTChillerSpecs::getDesignTemperatures( Real64 & TempDesCondIn, Real64 & TempDesEvapOut )
	{
		TempDesEvapOut = this->TempDesEvapOut;
		TempDesCondIn = this->TempDesCondIn;
	}

	void GTChillerSpecs::getDesignCapacities( const PlantLocation & calledFromLocation, Real64 & MaxLoad, Real64 & MinLoad, Real64 & OptLoad )
	{
		if ( calledFromLocation.loopNum == this->CWLoopNum ) { // chilled water loop
			MinLoad = this->NomCap * this->MinPartLoadRat;
			MaxLoad = this->NomCap * this->MaxPartLoadRat;
			OptLoad = this->NomCap * this->OptPartLoadRat;
		} else {
			MinLoad = 0.0;
			MaxLoad = 0.0;
			OptLoad = 0.0;
		}
	}

	void ConstCOPChillerSpecs::simulate( const PlantLocation & calledFromLocation, bool const FirstHVACIteration, Real64 & CurLoad )
	{
		bool RunFlag = DataPlant::PlantLoop( calledFromLocation.loopNum ).LoopSide( calledFromLocation.loopSideNum ).Branch( calledFromLocation.branchNum ).Comp( calledFromLocation.compNum ).ON;
		int EquipFlowCtrl = DataPlant::PlantLoop( calledFromLocation.loopNum ).LoopSide( calledFromLocation.loopSideNum ).Branch( calledFromLocation.branchNum ).Comp( calledFromLocation.compNum ).FlowCtrl;
		if ( calledFromLocation.loopNum == this->CWLoopNum ) {
			// Calculate Load
			// IF MinPlr, MaxPlr, OptPlr are not defined, assume min = 0, max=opt=Nomcap
			this->InitConstCOPChiller( RunFlag, CurLoad );
			this->CalcConstCOPChillerModel( CurLoad, RunFlag, EquipFlowCtrl );
			this->UpdateConstCOPChillerRecords( CurLoad, RunFlag );
		} else if ( calledFromLocation.loopNum == this->CDLoopNum ) {
			PlantUtilities::UpdateChillerComponentCondenserSide( this->CDLoopNum, this->CDLoopSideNum, TypeOf_Chiller_ConstCOP, this->CondInletNodeNum, this->CondOutletNodeNum, this->reports.QCond, this->reports.CondInletTemp, this->reports.CondOutletTemp, this->reports.Condmdot, FirstHVACIteration );
		}

	}

	void ConstCOPChillerSpecs::onInitLoopEquip( const PlantLocation & calledFromLocation )
	{

		bool RunFlag = DataPlant::PlantLoop( calledFromLocation.loopNum ).LoopSide( calledFromLocation.loopSideNum ).Branch( calledFromLocation.branchNum ).Comp( calledFromLocation.compNum ).ON;
		Real64 initLoad = 0.0;
		this->InitConstCOPChiller( RunFlag, initLoad );

		if ( calledFromLocation.loopNum == this->CWLoopNum ) {
			this->SizeConstCOPChiller();
		}
	}

	void ConstCOPChillerSpecs::getSizingFactor( Real64 & SizFac )
	{
		SizFac = this->SizFac;
	}

	void ConstCOPChillerSpecs::getDesignTemperatures( Real64 & TempDesCondIn, Real64 & TempDesEvapOut )
	{
		TempDesEvapOut = 0.0;
		TempDesCondIn = 0.0;

	}

	void ConstCOPChillerSpecs::getDesignCapacities( const PlantLocation & calledFromLocation, Real64 & MaxLoad, Real64 & MinLoad, Real64 & OptLoad )
	{
		if ( calledFromLocation.loopNum == this->CWLoopNum ) {
			MinLoad = 0.0;
			MaxLoad = this->NomCap;
			OptLoad = this->NomCap;
		} else {
			MinLoad = 0.0;
			MaxLoad = 0.0;
			OptLoad = 0.0;
		}
	}

	void
	GetElectricChillerInput()
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR:          Dan Fisher / Brandon Anderson
		//       DATE WRITTEN:    September 2000

		// PURPOSE OF THIS SUBROUTINE:
		// This routine will get the input
		// required by the Electric Chiller model.

		// METHODOLOGY EMPLOYED:
		// EnergyPlus input processor

		// REFERENCES: na

		// Using/Aliasing
		using InputProcessor::GetNumObjectsFound;
		using InputProcessor::GetObjectItem;
		using InputProcessor::VerifyName;
		using namespace DataIPShortCuts; // Data for field names, blank numerics
		using BranchNodeConnections::TestCompSet;
		using NodeInputManager::GetOnlySingleNode;
		using GlobalNames::VerifyUniqueChillerName;
		using OutAirNodeManager::CheckAndAddAirNodeNumber;
		using General::RoundSigDigits;
		using PlantUtilities::RegisterPlantCompDesignFlow;
		using ScheduleManager::GetScheduleIndex;
		using DataSizing::AutoSize;
		using DataGlobals::AnyEnergyManagementSystemInModel;

		// Locals
		// PARAMETERS
		static std::string const RoutineName( "GetElectricChillerInput: " ); // include trailing blank space
		//LOCAL VARIABLES
		int ChillerNum; // chiller counter
		int NumAlphas; // Number of elements in the alpha array
		int NumNums; // Number of elements in the numeric array
		int IOStat; // IO Status when calling get input subroutine
		//  CHARACTER(len=MaxNameLength),DIMENSION(9)   :: AlphArray !character string data
		//  REAL(r64),                        DIMENSION(22)  :: NumArray  !numeric data
		static bool ErrorsFound( false );
		bool IsNotOK; // Flag to verify name
		bool IsBlank; // Flag for blank name
		bool errFlag;
		bool Okay;
		//  CHARACTER(len=MaxNameLength) :: cCurrentModuleObject  ! for ease in renaming.

		//FLOW
		cCurrentModuleObject = "Chiller:Electric";
		NumElectricChillers = GetNumObjectsFound( cCurrentModuleObject );

		if ( NumElectricChillers <= 0 ) {
			return;
		}

		//See if load distribution manager has already gotten the input
		if ( allocated( ElectricChiller ) ) return;

		//ALLOCATE ARRAYS
		ElectricChiller.allocate( NumElectricChillers );

		//LOAD ARRAYS WITH Electric CURVE FIT CHILLER DATA
		for ( ChillerNum = 1; ChillerNum <= NumElectricChillers; ++ChillerNum ) {
			GetObjectItem( cCurrentModuleObject, ChillerNum, cAlphaArgs, NumAlphas, rNumericArgs, NumNums, IOStat, lNumericFieldBlanks, lAlphaFieldBlanks,  cAlphaFieldNames, cNumericFieldNames );

			IsNotOK = false;
			IsBlank = false;
			VerifyName( cAlphaArgs( 1 ), ElectricChiller, ChillerNum - 1, IsNotOK, IsBlank, cCurrentModuleObject + " Name" );
			if ( IsNotOK ) {
				ErrorsFound = true;
				if ( IsBlank ) cAlphaArgs( 1 ) = "xxxxx";
			}
			VerifyUniqueChillerName( cCurrentModuleObject, cAlphaArgs( 1 ), errFlag, cCurrentModuleObject + " Name" );
			if ( errFlag ) {
				ErrorsFound = true;
			}
			ElectricChiller( ChillerNum ).Name = cAlphaArgs( 1 );

			if ( cAlphaArgs( 2 ) == "AIRCOOLED" ) {
				ElectricChiller( ChillerNum ).CondenserType = AirCooled;
			} else if ( cAlphaArgs( 2 ) == "WATERCOOLED" ) {
				ElectricChiller( ChillerNum ).CondenserType = WaterCooled;
			} else if ( cAlphaArgs( 2 ) == "EVAPORATIVELYCOOLED" ) {
				ElectricChiller( ChillerNum ).CondenserType = EvapCooled;
			} else {
				ShowSevereError( "Invalid " + cAlphaFieldNames( 2 ) + '=' + cAlphaArgs( 2 ) );
				ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + cAlphaArgs( 1 ) );
				ErrorsFound = true;
			}

			ElectricChiller( ChillerNum ).NomCap = rNumericArgs( 1 );
			if ( ElectricChiller( ChillerNum ).NomCap == AutoSize ) {
				ElectricChiller( ChillerNum ).NomCapWasAutoSized = true;
			}
			if ( rNumericArgs( 1 ) == 0.0 ) {
				ShowSevereError( "Invalid " + cNumericFieldNames( 1 ) + '=' + RoundSigDigits( rNumericArgs( 1 ), 2 ) );
				ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + cAlphaArgs( 1 ) );
				ErrorsFound = true;
			}
			ElectricChiller( ChillerNum ).COP = rNumericArgs( 2 );
			if ( rNumericArgs( 2 ) == 0.0 ) {
				ShowSevereError( "Invalid " + cNumericFieldNames( 2 ) + '=' + RoundSigDigits( rNumericArgs( 2 ), 3 ) );
				ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + cAlphaArgs( 1 ) );
				ErrorsFound = true;
			}
			ElectricChiller( ChillerNum ).EvapInletNodeNum = GetOnlySingleNode( cAlphaArgs( 3 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Water, NodeConnectionType_Inlet, 1, ObjectIsNotParent );
			ElectricChiller( ChillerNum ).EvapOutletNodeNum = GetOnlySingleNode( cAlphaArgs( 4 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Water, NodeConnectionType_Outlet, 1, ObjectIsNotParent );
			TestCompSet( cCurrentModuleObject, cAlphaArgs( 1 ), cAlphaArgs( 3 ), cAlphaArgs( 4 ), "Chilled Water Nodes" );

			if ( ElectricChiller( ChillerNum ).CondenserType == AirCooled || ElectricChiller( ChillerNum ).CondenserType == EvapCooled ) {
				// Connection not required for air or evap cooled condenser
				//If the condenser inlet is blank for air cooled and evap cooled condensers then supply a generic name
				//  since it is not used elsewhere for connection
				// for transition purposes, add this node if not there.
				if ( lAlphaFieldBlanks( 5 ) ) {
					if ( len( cAlphaArgs( 1 ) ) < MaxNameLength - 21 ) { // protect against long name leading to > 100 chars
						cAlphaArgs( 5 ) = cAlphaArgs( 1 ) + " CONDENSER INLET NODE";
					} else {
						cAlphaArgs( 5 ) = cAlphaArgs( 1 ).substr( 0, 79 ) + " CONDENSER INLET NODE";
					}
				}
				if ( lAlphaFieldBlanks( 6 ) ) {
					if ( len( cAlphaArgs( 1 ) ) < MaxNameLength - 22 ) { // protect against long name leading to > 100 chars
						cAlphaArgs( 6 ) = cAlphaArgs( 1 ) + " CONDENSER OUTLET NODE";
					} else {
						cAlphaArgs( 6 ) = cAlphaArgs( 1 ).substr( 0, 78 ) + " CONDENSER OUTLET NODE";
					}
				}

				ElectricChiller( ChillerNum ).CondInletNodeNum = GetOnlySingleNode( cAlphaArgs( 5 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Air, NodeConnectionType_OutsideAirReference, 2, ObjectIsNotParent );
				CheckAndAddAirNodeNumber( ElectricChiller( ChillerNum ).CondInletNodeNum, Okay );
				if ( ! Okay ) {
					ShowWarningError( cCurrentModuleObject + ", Adding OutdoorAir:Node=" + cAlphaArgs( 5 ) );
				}

				ElectricChiller( ChillerNum ).CondOutletNodeNum = GetOnlySingleNode( cAlphaArgs( 6 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Air, NodeConnectionType_Outlet, 2, ObjectIsNotParent );
			} else if ( ElectricChiller( ChillerNum ).CondenserType == WaterCooled ) {
				ElectricChiller( ChillerNum ).CondInletNodeNum = GetOnlySingleNode( cAlphaArgs( 5 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Water, NodeConnectionType_Inlet, 2, ObjectIsNotParent );
				ElectricChiller( ChillerNum ).CondOutletNodeNum = GetOnlySingleNode( cAlphaArgs( 6 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Water, NodeConnectionType_Outlet, 2, ObjectIsNotParent );
				TestCompSet( cCurrentModuleObject, cAlphaArgs( 1 ), cAlphaArgs( 5 ), cAlphaArgs( 6 ), "Condenser Water Nodes" );
				//Condenser Inlet node name is necessary for Water Cooled
				if ( lAlphaFieldBlanks( 5 ) ) {
					ShowSevereError( "Invalid, " + cAlphaFieldNames( 5 ) + "is blank " );
					ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + cAlphaArgs( 1 ) );
					ErrorsFound = true;
				} else if ( lAlphaFieldBlanks( 6 ) ) {
					ShowSevereError( "Invalid, " + cAlphaFieldNames( 6 ) + "is blank " );
					ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + cAlphaArgs( 1 ) );
					ErrorsFound = true;
				}
			} else {
				ElectricChiller( ChillerNum ).CondInletNodeNum = GetOnlySingleNode( cAlphaArgs( 5 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Unknown, NodeConnectionType_Inlet, 2, ObjectIsNotParent );
				ElectricChiller( ChillerNum ).CondOutletNodeNum = GetOnlySingleNode( cAlphaArgs( 6 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Unknown, NodeConnectionType_Outlet, 2, ObjectIsNotParent );
				TestCompSet( cCurrentModuleObject, cAlphaArgs( 1 ), cAlphaArgs( 5 ), cAlphaArgs( 6 ), "Condenser (unknown?) Nodes" );
				//Condenser Inlet node name is necessary
				if ( lAlphaFieldBlanks( 5 ) ) {
					ShowSevereError( "Invalid, " + cAlphaFieldNames( 5 ) + "is blank " );
					ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + cAlphaArgs( 1 ) );
					ErrorsFound = true;
				} else if ( lAlphaFieldBlanks( 6 ) ) {
					ShowSevereError( "Invalid, " + cAlphaFieldNames( 6 ) + "is blank " );
					ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + cAlphaArgs( 1 ) );
					ErrorsFound = true;
				}
			}

			ElectricChiller( ChillerNum ).MinPartLoadRat = rNumericArgs( 3 );
			ElectricChiller( ChillerNum ).MaxPartLoadRat = rNumericArgs( 4 );
			ElectricChiller( ChillerNum ).OptPartLoadRat = rNumericArgs( 5 );
			ElectricChiller( ChillerNum ).TempDesCondIn = rNumericArgs( 6 );
			ElectricChiller( ChillerNum ).TempRiseCoef = rNumericArgs( 7 );
			ElectricChiller( ChillerNum ).TempDesEvapOut = rNumericArgs( 8 );
			ElectricChiller( ChillerNum ).EvapVolFlowRate = rNumericArgs( 9 );
			if ( ElectricChiller( ChillerNum ).EvapVolFlowRate == AutoSize ) {
				ElectricChiller( ChillerNum ).EvapVolFlowRateWasAutoSized = true;
			}
			ElectricChiller( ChillerNum ).CondVolFlowRate = rNumericArgs( 10 );
			if ( ElectricChiller( ChillerNum ).CondVolFlowRate == AutoSize ) {
				if ( ElectricChiller( ChillerNum ).CondenserType == WaterCooled ) {
					ElectricChiller( ChillerNum ).CondVolFlowRateWasAutoSized = true;
				}
			}
			ElectricChiller( ChillerNum ).CapRatCoef( 1 ) = rNumericArgs( 11 );
			ElectricChiller( ChillerNum ).CapRatCoef( 2 ) = rNumericArgs( 12 );
			ElectricChiller( ChillerNum ).CapRatCoef( 3 ) = rNumericArgs( 13 );
			if ( ( rNumericArgs( 11 ) + rNumericArgs( 12 ) + rNumericArgs( 13 ) ) == 0.0 ) {
				ShowSevereError( cCurrentModuleObject + ": Sum of Capacity Ratio Coef = 0.0, chiller=" + cAlphaArgs( 1 ) );
				ErrorsFound = true;
			}
			ElectricChiller( ChillerNum ).PowerRatCoef( 1 ) = rNumericArgs( 14 );
			ElectricChiller( ChillerNum ).PowerRatCoef( 2 ) = rNumericArgs( 15 );
			ElectricChiller( ChillerNum ).PowerRatCoef( 3 ) = rNumericArgs( 16 );
			ElectricChiller( ChillerNum ).FullLoadCoef( 1 ) = rNumericArgs( 17 );
			ElectricChiller( ChillerNum ).FullLoadCoef( 2 ) = rNumericArgs( 18 );
			ElectricChiller( ChillerNum ).FullLoadCoef( 3 ) = rNumericArgs( 19 );
			ElectricChiller( ChillerNum ).TempLowLimitEvapOut = rNumericArgs( 20 );
			ElectricChiller( ChillerNum ).SizFac = rNumericArgs( 22 );
			if ( ElectricChiller( ChillerNum ).SizFac <= 0.0 ) ElectricChiller( ChillerNum ).SizFac = 1.0;

			{ auto const SELECT_CASE_var( cAlphaArgs( 7 ) );
			if ( SELECT_CASE_var == "CONSTANTFLOW" ) {
				ElectricChiller( ChillerNum ).FlowMode = ConstantFlow;
			} else if ( SELECT_CASE_var == "VARIABLEFLOW" ) {
				ElectricChiller( ChillerNum ).FlowMode = LeavingSetPointModulated;
				ShowWarningError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\"," );
				ShowContinueError( "Invalid " + cAlphaFieldNames( 7 ) + '=' + cAlphaArgs( 7 ) );
				ShowContinueError( "Key choice is now called \"LeavingSetpointModulated\" and the simulation continues" );
			} else if ( SELECT_CASE_var == "LEAVINGSETPOINTMODULATED" ) {
				ElectricChiller( ChillerNum ).FlowMode = LeavingSetPointModulated;
			} else if ( SELECT_CASE_var == "NOTMODULATED" ) {
				ElectricChiller( ChillerNum ).FlowMode = NotModulated;
			} else {
				ShowSevereError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\"," );
				ShowContinueError( "Invalid " + cAlphaFieldNames( 7 ) + '=' + cAlphaArgs( 7 ) );
				ShowContinueError( "Available choices are ConstantFlow, NotModulated, or LeavingSetpointModulated" );
				ShowContinueError( "Flow mode NotModulated is assumed and the simulation continues." );
				ElectricChiller( ChillerNum ).FlowMode = NotModulated;
			}}

			// These are the Heat Recovery Inputs
			ElectricChiller( ChillerNum ).DesignHeatRecVolFlowRate = rNumericArgs( 21 );
			if ( ElectricChiller( ChillerNum ).DesignHeatRecVolFlowRate == AutoSize ) {
				ElectricChiller( ChillerNum ).DesignHeatRecVolFlowRateWasAutoSized = true;
			}

			if ( ( ElectricChiller( ChillerNum ).DesignHeatRecVolFlowRate > 0.0 ) || ( ElectricChiller( ChillerNum ).DesignHeatRecVolFlowRate == AutoSize ) ) {
				ElectricChiller( ChillerNum ).HeatRecActive = true;
				ElectricChiller( ChillerNum ).HeatRecInletNodeNum = GetOnlySingleNode( cAlphaArgs( 8 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Water, NodeConnectionType_Inlet, 3, ObjectIsNotParent );
				if ( ElectricChiller( ChillerNum ).HeatRecInletNodeNum == 0 ) {
					ShowSevereError( "Invalid " + cAlphaFieldNames( 8 ) + '=' + cAlphaArgs( 8 ) );
					ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + cAlphaArgs( 1 ) );
					ErrorsFound = true;
				}
				ElectricChiller( ChillerNum ).HeatRecOutletNodeNum = GetOnlySingleNode( cAlphaArgs( 9 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Water, NodeConnectionType_Outlet, 3, ObjectIsNotParent );
				if ( ElectricChiller( ChillerNum ).HeatRecOutletNodeNum == 0 ) {
					ShowSevereError( "Invalid " + cAlphaFieldNames( 9 ) + '=' + cAlphaArgs( 9 ) );
					ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + cAlphaArgs( 1 ) );
					ErrorsFound = true;
				}

				TestCompSet( cCurrentModuleObject, cAlphaArgs( 1 ), cAlphaArgs( 8 ), cAlphaArgs( 9 ), "Heat Recovery Nodes" );
				if ( ElectricChiller( ChillerNum ).DesignHeatRecVolFlowRate > 0.0 ) {
					RegisterPlantCompDesignFlow( ElectricChiller( ChillerNum ).HeatRecInletNodeNum, ElectricChiller( ChillerNum ).DesignHeatRecVolFlowRate );
				}
				// Condenser flow rate must be specified for heat reclaim
				if ( ElectricChiller( ChillerNum ).CondenserType == AirCooled || ElectricChiller( ChillerNum ).CondenserType == EvapCooled ) {
					if ( ElectricChiller( ChillerNum ).CondVolFlowRate <= 0.0 ) {
						ShowSevereError( "Invalid " + cNumericFieldNames( 10 ) + '=' + RoundSigDigits( rNumericArgs( 10 ), 6 ) );
						ShowSevereError( "Condenser fluid flow rate must be specified for Heat Reclaim applications." );
						ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + cAlphaArgs( 1 ) );
						ErrorsFound = true;
					}
				}

				if ( NumNums > 24 ) {
					if ( ! lNumericFieldBlanks( 25 ) ) {
						ElectricChiller( ChillerNum ).HeatRecCapacityFraction = rNumericArgs( 25 );
					} else {
						ElectricChiller( ChillerNum ).HeatRecCapacityFraction = 1.0;
					}
				} else {
					ElectricChiller( ChillerNum ).HeatRecCapacityFraction = 1.0;
				}

				if ( NumAlphas > 10 ) {
					if ( ! lAlphaFieldBlanks( 11 ) ) {
						ElectricChiller( ChillerNum ).HeatRecInletLimitSchedNum = GetScheduleIndex( cAlphaArgs( 11 ) );
						if ( ElectricChiller( ChillerNum ).HeatRecInletLimitSchedNum == 0 ) {
							ShowSevereError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\"" );
							ShowContinueError( "Invalid " + cAlphaFieldNames( 11 ) + '=' + cAlphaArgs( 11 ) );
							ErrorsFound = true;
						}
					} else {
						ElectricChiller( ChillerNum ).HeatRecInletLimitSchedNum = 0;
					}
				} else {
					ElectricChiller( ChillerNum ).HeatRecInletLimitSchedNum = 0;
				}

				if ( NumAlphas > 11 ) {
					if ( ! lAlphaFieldBlanks( 12 ) ) {
						ElectricChiller( ChillerNum ).HeatRecSetPointNodeNum = GetOnlySingleNode( cAlphaArgs( 12 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Water, NodeConnectionType_Sensor, 1, ObjectIsNotParent );
					} else {
						ElectricChiller( ChillerNum ).HeatRecSetPointNodeNum = 0;
					}
				} else {
					ElectricChiller( ChillerNum ).HeatRecSetPointNodeNum = 0;
				}

			} else {
				ElectricChiller( ChillerNum ).HeatRecActive = false;
				ElectricChiller( ChillerNum ).DesignHeatRecMassFlowRate = 0.0;
				ElectricChiller( ChillerNum ).HeatRecInletNodeNum = 0;
				ElectricChiller( ChillerNum ).HeatRecOutletNodeNum = 0;
				// if heat recovery is not used, don't care about condenser flow rate for air/evap-cooled equip.
				if ( ElectricChiller( ChillerNum ).CondenserType == AirCooled || ElectricChiller( ChillerNum ).CondenserType == EvapCooled ) {
					ElectricChiller( ChillerNum ).CondVolFlowRate = 0.0011; // set to avoid errors in calc routine
				}
				if ( ( ! lAlphaFieldBlanks( 8 ) ) || ( ! lAlphaFieldBlanks( 9 ) ) ) {
					ShowWarningError( "Since Design Heat Flow Rate = 0.0, Heat Recovery inactive for " + cCurrentModuleObject + '=' + cAlphaArgs( 1 ) );
					ShowContinueError( "However, Node names were specified for Heat Recovery inlet or outlet nodes" );
				}

			}
			//   Basin heater power as a function of temperature must be greater than or equal to 0
			ElectricChiller( ChillerNum ).BasinHeaterPowerFTempDiff = rNumericArgs( 23 );
			if ( rNumericArgs( 23 ) < 0.0 ) {
				ShowSevereError( cCurrentModuleObject + ", \"" + ElectricChiller( ChillerNum ).Name + "\" TRIM(cNumericFieldNames(23)) must be >= 0" );
				ErrorsFound = true;
			}

			ElectricChiller( ChillerNum ).BasinHeaterSetPointTemp = rNumericArgs( 24 );

			if ( ElectricChiller( ChillerNum ).BasinHeaterPowerFTempDiff > 0.0 ) {
				if ( NumNums < 24 ) {
					ElectricChiller( ChillerNum ).BasinHeaterSetPointTemp = 2.0;
				}
				if ( ElectricChiller( ChillerNum ).BasinHeaterSetPointTemp < 2.0 ) {
					ShowWarningError( cCurrentModuleObject + ":\"" + ElectricChiller( ChillerNum ).Name + "\", " + cNumericFieldNames( 24 ) + " is less than 2 deg C. Freezing could occur." );
				}
			}

			if ( ! lAlphaFieldBlanks( 10 ) ) {
				ElectricChiller( ChillerNum ).BasinHeaterSchedulePtr = GetScheduleIndex( cAlphaArgs( 10 ) );
				if ( ElectricChiller( ChillerNum ).BasinHeaterSchedulePtr == 0 ) {
					ShowWarningError( cCurrentModuleObject + ", \"" + ElectricChiller( ChillerNum ).Name + "\" TRIM(cAlphaFieldNames(10)) \"" + cAlphaArgs( 10 ) + "\" was not found. Basin heater operation will not be modeled and the simulation continues" );
				}
			}

		}

		if ( ErrorsFound ) {
			ShowFatalError( "Errors found in processing input for " + cCurrentModuleObject );
		}

		for ( ChillerNum = 1; ChillerNum <= NumElectricChillers; ++ChillerNum ) {
			SetupOutputVariable( "Chiller Electric Power [W]", ElectricChiller( ChillerNum ).reports.Power, "System", "Average", ElectricChiller( ChillerNum ).Name );
			SetupOutputVariable( "Chiller Electric Energy [J]", ElectricChiller( ChillerNum ).reports.Energy, "System", "Sum", ElectricChiller( ChillerNum ).Name, _, "ELECTRICITY", "Cooling", _, "Plant" );

			SetupOutputVariable( "Chiller Evaporator Cooling Rate [W]", ElectricChiller( ChillerNum ).reports.QEvap, "System", "Average", ElectricChiller( ChillerNum ).Name );
			SetupOutputVariable( "Chiller Evaporator Cooling Energy [J]", ElectricChiller( ChillerNum ).reports.EvapEnergy, "System", "Sum", ElectricChiller( ChillerNum ).Name, _, "ENERGYTRANSFER", "CHILLERS", _, "Plant" );
			SetupOutputVariable( "Chiller Evaporator Inlet Temperature [C]", ElectricChiller( ChillerNum ).reports.EvapInletTemp, "System", "Average", ElectricChiller( ChillerNum ).Name );
			SetupOutputVariable( "Chiller Evaporator Outlet Temperature [C]", ElectricChiller( ChillerNum ).reports.EvapOutletTemp, "System", "Average", ElectricChiller( ChillerNum ).Name );
			SetupOutputVariable( "Chiller Evaporator Mass Flow Rate [kg/s]", ElectricChiller( ChillerNum ).reports.Evapmdot, "System", "Average", ElectricChiller( ChillerNum ).Name );

			SetupOutputVariable( "Chiller Condenser Heat Transfer Rate [W]", ElectricChiller( ChillerNum ).reports.QCond, "System", "Average", ElectricChiller( ChillerNum ).Name );
			SetupOutputVariable( "Chiller Condenser Heat Transfer Energy [J]", ElectricChiller( ChillerNum ).reports.CondEnergy, "System", "Sum", ElectricChiller( ChillerNum ).Name, _, "ENERGYTRANSFER", "HEATREJECTION", _, "Plant" );
			SetupOutputVariable( "Chiller COP [W/W]", ElectricChiller( ChillerNum ).reports.ActualCOP, "System", "Average", ElectricChiller( ChillerNum ).Name );

			//Condenser mass flow and outlet temp are valid for water cooled
			if ( ElectricChiller( ChillerNum ).CondenserType == WaterCooled ) {
				SetupOutputVariable( "Chiller Condenser Inlet Temperature [C]", ElectricChiller( ChillerNum ).reports.CondInletTemp, "System", "Average", ElectricChiller( ChillerNum ).Name );
				SetupOutputVariable( "Chiller Condenser Outlet Temperature [C]", ElectricChiller( ChillerNum ).reports.CondOutletTemp, "System", "Average", ElectricChiller( ChillerNum ).Name );
				SetupOutputVariable( "Chiller Condenser Mass Flow Rate [kg/s]", ElectricChiller( ChillerNum ).reports.Condmdot, "System", "Average", ElectricChiller( ChillerNum ).Name );
			} else if ( ElectricChiller( ChillerNum ).CondenserType == AirCooled ) {
				SetupOutputVariable( "Chiller Condenser Inlet Temperature [C]", ElectricChiller( ChillerNum ).reports.CondInletTemp, "System", "Average", ElectricChiller( ChillerNum ).Name );
			} else if ( ElectricChiller( ChillerNum ).CondenserType == EvapCooled ) {
				SetupOutputVariable( "Chiller Condenser Inlet Temperature [C]", ElectricChiller( ChillerNum ).reports.CondInletTemp, "System", "Average", ElectricChiller( ChillerNum ).Name );
				if ( ElectricChiller( ChillerNum ).BasinHeaterPowerFTempDiff > 0.0 ) {
					SetupOutputVariable( "Chiller Basin Heater Electric Power [W]", ElectricChiller( ChillerNum ).reports.BasinHeaterPower, "System", "Average", ElectricChiller( ChillerNum ).Name );
					SetupOutputVariable( "Chiller Basin Heater Electric Energy [J]", ElectricChiller( ChillerNum ).reports.BasinHeaterConsumption, "System", "Sum", ElectricChiller( ChillerNum ).Name, _, "Electric", "CHILLERS", _, "Plant" );
				}
			}

			//If heat recovery is active then setup report variables
			if ( ElectricChiller( ChillerNum ).HeatRecActive ) {
				SetupOutputVariable( "Chiller Total Recovered Heat Rate [W]", ElectricChiller( ChillerNum ).reports.QHeatRecovery, "System", "Average", ElectricChiller( ChillerNum ).Name );
				SetupOutputVariable( "Chiller Total Recovered Heat Energy [J]", ElectricChiller( ChillerNum ).reports.EnergyHeatRecovery, "System", "Sum", ElectricChiller( ChillerNum ).Name, _, "ENERGYTRANSFER", "HEATRECOVERY", _, "Plant" );
				SetupOutputVariable( "Chiller Heat Recovery Inlet Temperature [C]", ElectricChiller( ChillerNum ).reports.HeatRecInletTemp, "System", "Average", ElectricChiller( ChillerNum ).Name );

				SetupOutputVariable( "Chiller Heat Recovery Outlet Temperature [C]", ElectricChiller( ChillerNum ).reports.HeatRecOutletTemp, "System", "Average", ElectricChiller( ChillerNum ).Name );

				SetupOutputVariable( "Chiller Heat Recovery Mass Flow Rate [kg/s]", ElectricChiller( ChillerNum ).reports.HeatRecMassFlow, "System", "Average", ElectricChiller( ChillerNum ).Name );

				SetupOutputVariable( "Chiller Effective Heat Rejection Temperature [C]", ElectricChiller( ChillerNum ).reports.ChillerCondAvgTemp, "System", "Average", ElectricChiller( ChillerNum ).Name );

			}
			if ( AnyEnergyManagementSystemInModel ) {
				SetupEMSInternalVariable( "Chiller Nominal Capacity", ElectricChiller( ChillerNum ).Name, "[W]", ElectricChiller( ChillerNum ).NomCap );
			}
		}

	}

	void
	GetEngineDrivenChillerInput()
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR:          Dan Fisher / Brandon Anderson
		//       DATE WRITTEN:    September 2000
		// PURPOSE OF THIS SUBROUTINE:
		// This routine will get the input
		// required by the EngineDriven Chiller model.

		// METHODOLOGY EMPLOYED:

		// REFERENCES: na

		// Using/Aliasing
		using InputProcessor::GetNumObjectsFound;
		using InputProcessor::GetObjectItem;
		using InputProcessor::VerifyName;
		using namespace DataIPShortCuts; // Data for field names, blank numerics
		using CurveManager::GetCurveIndex;
		using BranchNodeConnections::TestCompSet;
		using NodeInputManager::GetOnlySingleNode;
		using GlobalNames::VerifyUniqueChillerName;
		using OutAirNodeManager::CheckAndAddAirNodeNumber;
		using General::RoundSigDigits;
		using PlantUtilities::RegisterPlantCompDesignFlow;
		using ScheduleManager::GetScheduleIndex;
		using DataGlobals::AnyEnergyManagementSystemInModel;
		using DataSizing::AutoSize;

		// Locals
		// PARAMETERS
		static std::string const RoutineName( "GetEngineDrivenChillerInput: " ); // include trailing blank space
		//LOCAL VARIABLES
		int ChillerNum; // chiller counter
		int NumAlphas; // Number of elements in the alpha array
		int NumNums; // Number of elements in the numeric array
		int IOStat; // IO Status when calling get input subroutine
		static bool ErrorsFound( false );
		bool IsNotOK; // Flag to verify name
		bool IsBlank; // Flag for blank name
		bool errFlag;
		bool Okay;

		//FLOW
		cCurrentModuleObject = "Chiller:EngineDriven";
		NumEngineDrivenChillers = GetNumObjectsFound( cCurrentModuleObject );

		if ( NumEngineDrivenChillers <= 0 ) {
			return;
		}
		//See if load distribution manager has already gotten the input
		if ( allocated( EngineDrivenChiller ) ) return;

		//ALLOCATE ARRAYS
		EngineDrivenChiller.allocate( NumEngineDrivenChillers );

		//LOAD ARRAYS WITH EngineDriven CURVE FIT CHILLER DATA
		for ( ChillerNum = 1; ChillerNum <= NumEngineDrivenChillers; ++ChillerNum ) {
			GetObjectItem( cCurrentModuleObject, ChillerNum, cAlphaArgs, NumAlphas, rNumericArgs, NumNums, IOStat, _, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );

			IsNotOK = false;
			IsBlank = false;
			VerifyName( cAlphaArgs( 1 ), EngineDrivenChiller, ChillerNum - 1, IsNotOK, IsBlank, cCurrentModuleObject + " Name" );
			if ( IsNotOK ) {
				ErrorsFound = true;
				if ( IsBlank ) cAlphaArgs( 1 ) = "xxxxx";
			}
			VerifyUniqueChillerName( cCurrentModuleObject, cAlphaArgs( 1 ), errFlag, cCurrentModuleObject + " Name" );
			if ( errFlag ) {
				ErrorsFound = true;
			}
			EngineDrivenChiller( ChillerNum ).Name = cAlphaArgs( 1 );

			EngineDrivenChiller( ChillerNum ).NomCap = rNumericArgs( 1 );
			if ( EngineDrivenChiller( ChillerNum ).NomCap == AutoSize ) {
				EngineDrivenChiller( ChillerNum ).NomCapWasAutoSized = true;
			}
			if ( rNumericArgs( 1 ) == 0.0 ) {
				ShowSevereError( "Invalid " + cNumericFieldNames( 1 ) + '=' + RoundSigDigits( rNumericArgs( 1 ), 2 ) );
				ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + cAlphaArgs( 1 ) );
				ErrorsFound = true;
			}

			EngineDrivenChiller( ChillerNum ).COP = rNumericArgs( 2 );
			if ( rNumericArgs( 2 ) == 0.0 ) {
				ShowSevereError( "Invalid " + cNumericFieldNames( 2 ) + '=' + RoundSigDigits( rNumericArgs( 2 ), 2 ) );
				ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + cAlphaArgs( 1 ) );
				ErrorsFound = true;
			}

			if ( cAlphaArgs( 2 ) == "AIRCOOLED" ) {
				EngineDrivenChiller( ChillerNum ).CondenserType = AirCooled;
			} else if ( cAlphaArgs( 2 ) == "WATERCOOLED" ) {
				EngineDrivenChiller( ChillerNum ).CondenserType = WaterCooled;
			} else if ( cAlphaArgs( 2 ) == "EVAPORATIVELYCOOLED" ) {
				EngineDrivenChiller( ChillerNum ).CondenserType = EvapCooled;
			} else {
				ShowSevereError( "Invalid " + cAlphaFieldNames( 2 ) + '=' + cAlphaArgs( 2 ) );
				ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + cAlphaArgs( 1 ) );
				ErrorsFound = true;
			}

			EngineDrivenChiller( ChillerNum ).EvapInletNodeNum = GetOnlySingleNode( cAlphaArgs( 3 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Water, NodeConnectionType_Inlet, 1, ObjectIsNotParent );
			EngineDrivenChiller( ChillerNum ).EvapOutletNodeNum = GetOnlySingleNode( cAlphaArgs( 4 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Water, NodeConnectionType_Outlet, 1, ObjectIsNotParent );
			TestCompSet( cCurrentModuleObject, cAlphaArgs( 1 ), cAlphaArgs( 3 ), cAlphaArgs( 4 ), "Chilled Water Nodes" );

			if ( EngineDrivenChiller( ChillerNum ).CondenserType == AirCooled || EngineDrivenChiller( ChillerNum ).CondenserType == EvapCooled ) {
				// Connection not required for air or evap cooled condenser
				//If the condenser inlet is blank for air cooled and evap cooled condensers then supply a generic name
				//  since it is not used elsewhere for connection
				if ( lAlphaFieldBlanks( 5 ) ) {
					if ( len( cAlphaArgs( 1 ) ) < MaxNameLength - 21 ) { // protect against long name leading to > 100 chars
						cAlphaArgs( 5 ) = cAlphaArgs( 1 ) + " CONDENSER INLET NODE";
					} else {
						cAlphaArgs( 5 ) = cAlphaArgs( 1 ).substr( 0, 79 ) + " CONDENSER INLET NODE";
					}
				}
				if ( lAlphaFieldBlanks( 6 ) ) {
					if ( len( cAlphaArgs( 1 ) ) < MaxNameLength - 22 ) { // protect against long name leading to > 100 chars
						cAlphaArgs( 6 ) = cAlphaArgs( 1 ) + " CONDENSER OUTLET NODE";
					} else {
						cAlphaArgs( 6 ) = cAlphaArgs( 1 ).substr( 0, 78 ) + " CONDENSER OUTLET NODE";
					}
				}

				EngineDrivenChiller( ChillerNum ).CondInletNodeNum = GetOnlySingleNode( cAlphaArgs( 5 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Air, NodeConnectionType_OutsideAirReference, 2, ObjectIsNotParent );
				CheckAndAddAirNodeNumber( EngineDrivenChiller( ChillerNum ).CondInletNodeNum, Okay );
				if ( ! Okay ) {
					ShowWarningError( cCurrentModuleObject + ", Adding OutdoorAir:Node=" + cAlphaArgs( 5 ) );
				}

				EngineDrivenChiller( ChillerNum ).CondOutletNodeNum = GetOnlySingleNode( cAlphaArgs( 6 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Air, NodeConnectionType_Outlet, 2, ObjectIsNotParent );
				//CALL TestCompSet(TRIM(cCurrentModuleObject),cAlphaArgs(1),cAlphaArgs(5),cAlphaArgs(6),'Condenser (Air) Nodes')
			} else if ( EngineDrivenChiller( ChillerNum ).CondenserType == WaterCooled ) {
				EngineDrivenChiller( ChillerNum ).CondInletNodeNum = GetOnlySingleNode( cAlphaArgs( 5 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Water, NodeConnectionType_Inlet, 2, ObjectIsNotParent );
				EngineDrivenChiller( ChillerNum ).CondOutletNodeNum = GetOnlySingleNode( cAlphaArgs( 6 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Water, NodeConnectionType_Outlet, 2, ObjectIsNotParent );
				TestCompSet( cCurrentModuleObject, cAlphaArgs( 1 ), cAlphaArgs( 5 ), cAlphaArgs( 6 ), "Condenser Water Nodes" );
				//Condenser Inlet node name is necessary for Water Cooled
				if ( lAlphaFieldBlanks( 5 ) ) {
					ShowSevereError( "Invalid, " + cAlphaFieldNames( 5 ) + " is blank " );
					ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + cAlphaArgs( 1 ) );
					ErrorsFound = true;
				} else if ( lAlphaFieldBlanks( 6 ) ) {
					ShowSevereError( "Invalid, " + cAlphaFieldNames( 6 ) + " is blank " );
					ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + cAlphaArgs( 1 ) );
					ErrorsFound = true;
				}
			} else {
				EngineDrivenChiller( ChillerNum ).CondInletNodeNum = GetOnlySingleNode( cAlphaArgs( 5 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Unknown, NodeConnectionType_Inlet, 2, ObjectIsNotParent );
				EngineDrivenChiller( ChillerNum ).CondOutletNodeNum = GetOnlySingleNode( cAlphaArgs( 6 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Unknown, NodeConnectionType_Outlet, 2, ObjectIsNotParent );
				TestCompSet( cCurrentModuleObject, cAlphaArgs( 1 ), cAlphaArgs( 5 ), cAlphaArgs( 6 ), "Condenser (unknown?) Nodes" );
				//Condenser Inlet node name is necessary for Water Cooled
				if ( lAlphaFieldBlanks( 5 ) ) {
					ShowSevereError( "Invalid, " + cAlphaFieldNames( 5 ) + " is blank " );
					ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + cAlphaArgs( 1 ) );
					ErrorsFound = true;
				} else if ( lAlphaFieldBlanks( 6 ) ) {
					ShowSevereError( "Invalid, " + cAlphaFieldNames( 6 ) + " is blank " );
					ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + cAlphaArgs( 1 ) );
					ErrorsFound = true;
				}
			}

			EngineDrivenChiller( ChillerNum ).MinPartLoadRat = rNumericArgs( 3 );
			EngineDrivenChiller( ChillerNum ).MaxPartLoadRat = rNumericArgs( 4 );
			EngineDrivenChiller( ChillerNum ).OptPartLoadRat = rNumericArgs( 5 );
			EngineDrivenChiller( ChillerNum ).TempDesCondIn = rNumericArgs( 6 );
			EngineDrivenChiller( ChillerNum ).TempRiseCoef = rNumericArgs( 7 );
			EngineDrivenChiller( ChillerNum ).TempDesEvapOut = rNumericArgs( 8 );
			EngineDrivenChiller( ChillerNum ).EvapVolFlowRate = rNumericArgs( 9 );
			if ( EngineDrivenChiller( ChillerNum ).EvapVolFlowRate == AutoSize ) {
				EngineDrivenChiller( ChillerNum ).EvapVolFlowRateWasAutoSized = true;
			}
			EngineDrivenChiller( ChillerNum ).CondVolFlowRate = rNumericArgs( 10 );
			if ( EngineDrivenChiller( ChillerNum ).CondVolFlowRate == AutoSize ) {
				EngineDrivenChiller( ChillerNum ).CondVolFlowRateWasAutoSized = true;
			}
			EngineDrivenChiller( ChillerNum ).CapRatCoef( 1 ) = rNumericArgs( 11 );
			EngineDrivenChiller( ChillerNum ).CapRatCoef( 2 ) = rNumericArgs( 12 );
			EngineDrivenChiller( ChillerNum ).CapRatCoef( 3 ) = rNumericArgs( 13 );
			if ( ( rNumericArgs( 11 ) + rNumericArgs( 12 ) + rNumericArgs( 13 ) ) == 0.0 ) {
				ShowSevereError( cCurrentModuleObject + ": Sum of Capacity Ratio Coef = 0.0, chiller=" + cAlphaArgs( 1 ) );
				ErrorsFound = true;
			}
			EngineDrivenChiller( ChillerNum ).PowerRatCoef( 1 ) = rNumericArgs( 14 );
			EngineDrivenChiller( ChillerNum ).PowerRatCoef( 2 ) = rNumericArgs( 15 );
			EngineDrivenChiller( ChillerNum ).PowerRatCoef( 3 ) = rNumericArgs( 16 );
			EngineDrivenChiller( ChillerNum ).FullLoadCoef( 1 ) = rNumericArgs( 17 );
			EngineDrivenChiller( ChillerNum ).FullLoadCoef( 2 ) = rNumericArgs( 18 );
			EngineDrivenChiller( ChillerNum ).FullLoadCoef( 3 ) = rNumericArgs( 19 );
			EngineDrivenChiller( ChillerNum ).TempLowLimitEvapOut = rNumericArgs( 20 );

			//Load Special EngineDriven Chiller Curve Fit Inputs
			EngineDrivenChiller( ChillerNum ).ClngLoadtoFuelCurve = GetCurveIndex( cAlphaArgs( 7 ) ); // convert curve name to number
			if ( EngineDrivenChiller( ChillerNum ).ClngLoadtoFuelCurve == 0 ) {
				ShowSevereError( "Invalid " + cAlphaFieldNames( 7 ) + '=' + cAlphaArgs( 7 ) );
				ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + cAlphaArgs( 1 ) );
				ErrorsFound = true;
			}

			EngineDrivenChiller( ChillerNum ).RecJacHeattoFuelCurve = GetCurveIndex( cAlphaArgs( 8 ) ); // convert curve name to number
			if ( EngineDrivenChiller( ChillerNum ).RecJacHeattoFuelCurve == 0 ) {
				ShowSevereError( "Invalid " + cAlphaFieldNames( 8 ) + '=' + cAlphaArgs( 8 ) );
				ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + cAlphaArgs( 1 ) );
				ErrorsFound = true;
			}

			EngineDrivenChiller( ChillerNum ).RecLubeHeattoFuelCurve = GetCurveIndex( cAlphaArgs( 9 ) ); // convert curve name to number
			if ( EngineDrivenChiller( ChillerNum ).RecLubeHeattoFuelCurve == 0 ) {
				ShowSevereError( "Invalid " + cAlphaFieldNames( 9 ) + '=' + cAlphaArgs( 9 ) );
				ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + cAlphaArgs( 1 ) );
				ErrorsFound = true;
			}

			EngineDrivenChiller( ChillerNum ).TotExhausttoFuelCurve = GetCurveIndex( cAlphaArgs( 10 ) ); // convert curve name to number
			if ( EngineDrivenChiller( ChillerNum ).TotExhausttoFuelCurve == 0 ) {
				ShowSevereError( "Invalid " + cAlphaFieldNames( 10 ) + '=' + cAlphaArgs( 10 ) );
				ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + cAlphaArgs( 1 ) );
				ErrorsFound = true;
			}

			EngineDrivenChiller( ChillerNum ).ExhaustTempCurve = GetCurveIndex( cAlphaArgs( 11 ) ); // convert curve name to number
			if ( EngineDrivenChiller( ChillerNum ).ExhaustTempCurve == 0 ) {
				ShowSevereError( "Invalid " + cAlphaFieldNames( 11 ) + '=' + cAlphaArgs( 11 ) );
				ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + cAlphaArgs( 1 ) );
				ErrorsFound = true;
			}

			EngineDrivenChiller( ChillerNum ).UACoef( 1 ) = rNumericArgs( 21 );
			EngineDrivenChiller( ChillerNum ).UACoef( 2 ) = rNumericArgs( 22 );

			EngineDrivenChiller( ChillerNum ).MaxExhaustperPowerOutput = rNumericArgs( 23 );
			EngineDrivenChiller( ChillerNum ).DesignMinExitGasTemp = rNumericArgs( 24 );

			EngineDrivenChiller( ChillerNum ).FuelType = cAlphaArgs( 12 );

			{ auto const SELECT_CASE_var( cAlphaArgs( 12 ) );

			if ( ( SELECT_CASE_var == "Gas" ) || ( SELECT_CASE_var == "NATURALGAS" ) || ( SELECT_CASE_var == "NATURAL GAS" ) ) {
				EngineDrivenChiller( ChillerNum ).FuelType = "Gas";

			} else if ( SELECT_CASE_var == "DIESEL" ) {
				EngineDrivenChiller( ChillerNum ).FuelType = "Diesel";

			} else if ( SELECT_CASE_var == "GASOLINE" ) {
				EngineDrivenChiller( ChillerNum ).FuelType = "Gasoline";

			} else if ( ( SELECT_CASE_var == "FUEL OIL #1" ) || ( SELECT_CASE_var == "FUELOIL#1" ) || ( SELECT_CASE_var == "FUEL OIL" ) || ( SELECT_CASE_var == "DISTILLATE OIL" ) ) {
				EngineDrivenChiller( ChillerNum ).FuelType = "FuelOil#1";

			} else if ( ( SELECT_CASE_var == "FUEL OIL #2" ) || ( SELECT_CASE_var == "FUELOIL#2" ) || ( SELECT_CASE_var == "RESIDUAL OIL" ) ) {
				EngineDrivenChiller( ChillerNum ).FuelType = "FuelOil#2";

			} else if ( ( SELECT_CASE_var == "Propane" ) || ( SELECT_CASE_var == "LPG" ) || ( SELECT_CASE_var == "PROPANEGAS" ) || ( SELECT_CASE_var == "PROPANE GAS" ) ) {
				EngineDrivenChiller( ChillerNum ).FuelType = "Propane";

			} else if ( SELECT_CASE_var == "OTHERFUEL1" ) {
				EngineDrivenChiller( ChillerNum ).FuelType = "OtherFuel1";

			} else if ( SELECT_CASE_var == "OTHERFUEL2" ) {
				EngineDrivenChiller( ChillerNum ).FuelType = "OtherFuel2";

			} else {
				ShowSevereError( "Invalid " + cAlphaFieldNames( 12 ) + '=' + cAlphaArgs( 12 ) );
				ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + cAlphaArgs( 1 ) );
				ShowContinueError( "Valid choices are Electricity, NaturalGas, PropaneGas, Diesel, Gasoline, FuelOil#1, FuelOil#2,OtherFuel1 or OtherFuel2" );
				ErrorsFound = true;
			}}

			EngineDrivenChiller( ChillerNum ).FuelHeatingValue = rNumericArgs( 25 );
			EngineDrivenChiller( ChillerNum ).DesignHeatRecVolFlowRate = rNumericArgs( 26 );
			if ( EngineDrivenChiller( ChillerNum ).DesignHeatRecVolFlowRate > 0.0 ) {
				EngineDrivenChiller( ChillerNum ).HeatRecActive = true;
				EngineDrivenChiller( ChillerNum ).HeatRecInletNodeNum = GetOnlySingleNode( cAlphaArgs( 13 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Water, NodeConnectionType_Inlet, 3, ObjectIsNotParent );
				if ( EngineDrivenChiller( ChillerNum ).HeatRecInletNodeNum == 0 ) {
					ShowSevereError( "Invalid " + cAlphaFieldNames( 13 ) + '=' + cAlphaArgs( 13 ) );
					ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + cAlphaArgs( 1 ) );
					ErrorsFound = true;
				}
				EngineDrivenChiller( ChillerNum ).HeatRecOutletNodeNum = GetOnlySingleNode( cAlphaArgs( 14 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Water, NodeConnectionType_Outlet, 3, ObjectIsNotParent );
				if ( EngineDrivenChiller( ChillerNum ).HeatRecOutletNodeNum == 0 ) {
					ShowSevereError( "Invalid " + cAlphaFieldNames( 14 ) + '=' + cAlphaArgs( 14 ) );
					ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + cAlphaArgs( 1 ) );
					ErrorsFound = true;
				}
				TestCompSet( cCurrentModuleObject, cAlphaArgs( 1 ), cAlphaArgs( 13 ), cAlphaArgs( 14 ), "Heat Recovery Nodes" );
				RegisterPlantCompDesignFlow( EngineDrivenChiller( ChillerNum ).HeatRecInletNodeNum, EngineDrivenChiller( ChillerNum ).DesignHeatRecVolFlowRate );
				// Condenser flow rate must be specified for heat reclaim
				if ( EngineDrivenChiller( ChillerNum ).CondenserType == AirCooled || EngineDrivenChiller( ChillerNum ).CondenserType == EvapCooled ) {
					if ( EngineDrivenChiller( ChillerNum ).CondVolFlowRate <= 0.0 ) {
						ShowSevereError( "Invalid " + cNumericFieldNames( 10 ) + '=' + RoundSigDigits( rNumericArgs( 10 ), 6 ) );
						ShowSevereError( "Condenser fluid flow rate must be specified for Heat Reclaim applications." );
						ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + cAlphaArgs( 1 ) );
						ErrorsFound = true;
					}
				}

			} else {

				EngineDrivenChiller( ChillerNum ).HeatRecActive = false;
				EngineDrivenChiller( ChillerNum ).DesignHeatRecMassFlowRate = 0.0;
				EngineDrivenChiller( ChillerNum ).HeatRecInletNodeNum = 0;
				EngineDrivenChiller( ChillerNum ).HeatRecOutletNodeNum = 0;
				// if heat recovery is not used, don't care about condenser flow rate for air/evap-cooled equip.
				if ( EngineDrivenChiller( ChillerNum ).CondenserType == AirCooled || EngineDrivenChiller( ChillerNum ).CondenserType == EvapCooled ) {
					EngineDrivenChiller( ChillerNum ).CondVolFlowRate = 0.0011; // set to avoid errors in calc routine
				}
				if ( ( ! lAlphaFieldBlanks( 13 ) ) || ( ! lAlphaFieldBlanks( 14 ) ) ) {
					ShowWarningError( "Since Design Heat Flow Rate = 0.0, Heat Recovery inactive for " + cCurrentModuleObject + '=' + cAlphaArgs( 1 ) );
					ShowContinueError( "However, Node names were specified for Heat Recovery inlet or outlet nodes" );
				}
			}

			{ auto const SELECT_CASE_var( cAlphaArgs( 15 ) );
			if ( SELECT_CASE_var == "CONSTANTFLOW" ) {
				EngineDrivenChiller( ChillerNum ).FlowMode = ConstantFlow;
			} else if ( SELECT_CASE_var == "VARIABLEFLOW" ) {
				EngineDrivenChiller( ChillerNum ).FlowMode = LeavingSetPointModulated;
				ShowWarningError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\"," );
				ShowContinueError( "Invalid " + cAlphaFieldNames( 15 ) + '=' + cAlphaArgs( 15 ) );
				ShowContinueError( "Key choice is now called \"LeavingSetpointModulated\" and the simulation continues" );
			} else if ( SELECT_CASE_var == "LEAVINGSETPOINTMODULATED" ) {
				EngineDrivenChiller( ChillerNum ).FlowMode = LeavingSetPointModulated;
			} else if ( SELECT_CASE_var == "NOTMODULATED" ) {
				EngineDrivenChiller( ChillerNum ).FlowMode = NotModulated;
			} else {
				ShowSevereError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\"," );
				ShowContinueError( "Invalid " + cAlphaFieldNames( 15 ) + '=' + cAlphaArgs( 15 ) );
				ShowContinueError( "Available choices are ConstantFlow, NotModulated, or LeavingSetpointModulated" );
				ShowContinueError( "Flow mode NotModulated is assumed and the simulation continues." );
				EngineDrivenChiller( ChillerNum ).FlowMode = NotModulated;
			}}

			EngineDrivenChiller( ChillerNum ).HeatRecMaxTemp = rNumericArgs( 27 );
			EngineDrivenChiller( ChillerNum ).SizFac = rNumericArgs( 28 );
			if ( EngineDrivenChiller( ChillerNum ).SizFac <= 0.0 ) EngineDrivenChiller( ChillerNum ).SizFac = 1.0;

			//   Basin heater power as a function of temperature must be greater than or equal to 0
			EngineDrivenChiller( ChillerNum ).BasinHeaterPowerFTempDiff = rNumericArgs( 29 );
			if ( rNumericArgs( 29 ) < 0.0 ) {
				ShowSevereError( cCurrentModuleObject + ", \"" + EngineDrivenChiller( ChillerNum ).Name + "\" TRIM(cNumericFieldNames(29)) must be >= 0" );
				ErrorsFound = true;
			}

			EngineDrivenChiller( ChillerNum ).BasinHeaterSetPointTemp = rNumericArgs( 30 );

			if ( EngineDrivenChiller( ChillerNum ).BasinHeaterPowerFTempDiff > 0.0 ) {
				if ( NumNums < 30 ) {
					EngineDrivenChiller( ChillerNum ).BasinHeaterSetPointTemp = 2.0;
				}
				if ( EngineDrivenChiller( ChillerNum ).BasinHeaterSetPointTemp < 2.0 ) {
					ShowWarningError( cCurrentModuleObject + ":\"" + EngineDrivenChiller( ChillerNum ).Name + "\", " + cNumericFieldNames( 30 ) + " is less than 2 deg C. Freezing could occur." );
				}
			}

			if ( ! lAlphaFieldBlanks( 16 ) ) {
				EngineDrivenChiller( ChillerNum ).BasinHeaterSchedulePtr = GetScheduleIndex( cAlphaArgs( 16 ) );
				if ( EngineDrivenChiller( ChillerNum ).BasinHeaterSchedulePtr == 0 ) {
					ShowWarningError( cCurrentModuleObject + ", \"" + EngineDrivenChiller( ChillerNum ).Name + "\" TRIM(cAlphaFieldNames(16)) \"" + cAlphaArgs( 16 ) + "\" was not found. Basin heater operation will not be modeled and the simulation continues" );
				}
			}

		}

		if ( ErrorsFound ) {
			ShowFatalError( "Errors found in processing input for " + cCurrentModuleObject );
		}

		for ( ChillerNum = 1; ChillerNum <= NumEngineDrivenChillers; ++ChillerNum ) {
			SetupOutputVariable( "Chiller Drive Shaft Power [W]", EngineDrivenChiller( ChillerNum ).reports.Power, "System", "Average", EngineDrivenChiller( ChillerNum ).Name );
			SetupOutputVariable( "Chiller Drive Shaft Energy [J]", EngineDrivenChiller( ChillerNum ).reports.Energy, "System", "Sum", EngineDrivenChiller( ChillerNum ).Name );

			SetupOutputVariable( "Chiller Evaporator Cooling Rate [W]", EngineDrivenChiller( ChillerNum ).reports.QEvap, "System", "Average", EngineDrivenChiller( ChillerNum ).Name );
			SetupOutputVariable( "Chiller Evaporator Cooling Energy [J]", EngineDrivenChiller( ChillerNum ).reports.EvapEnergy, "System", "Sum", EngineDrivenChiller( ChillerNum ).Name, _, "ENERGYTRANSFER", "CHILLERS", _, "Plant" );
			SetupOutputVariable( "Chiller Evaporator Inlet Temperature [C]", EngineDrivenChiller( ChillerNum ).reports.EvapInletTemp, "System", "Average", EngineDrivenChiller( ChillerNum ).Name );
			SetupOutputVariable( "Chiller Evaporator Outlet Temperature [C]", EngineDrivenChiller( ChillerNum ).reports.EvapOutletTemp, "System", "Average", EngineDrivenChiller( ChillerNum ).Name );
			SetupOutputVariable( "Chiller Evaporator Mass Flow Rate [kg/s]", EngineDrivenChiller( ChillerNum ).reports.Evapmdot, "System", "Average", EngineDrivenChiller( ChillerNum ).Name );
			SetupOutputVariable( "Chiller Condenser Heat Transfer Rate [W]", EngineDrivenChiller( ChillerNum ).reports.QCond, "System", "Average", EngineDrivenChiller( ChillerNum ).Name );
			SetupOutputVariable( "Chiller Condenser Heat Transfer Energy [J]", EngineDrivenChiller( ChillerNum ).reports.CondEnergy, "System", "Sum", EngineDrivenChiller( ChillerNum ).Name, _, "ENERGYTRANSFER", "HEATREJECTION", _, "Plant" );

			//Condenser mass flow and outlet temp are valid for Water Cooled
			if ( EngineDrivenChiller( ChillerNum ).CondenserType == WaterCooled ) {
				SetupOutputVariable( "Chiller Condenser Inlet Temperature [C]", EngineDrivenChiller( ChillerNum ).reports.CondInletTemp, "System", "Average", EngineDrivenChiller( ChillerNum ).Name );
				SetupOutputVariable( "Chiller Condenser Outlet Temperature [C]", EngineDrivenChiller( ChillerNum ).reports.CondOutletTemp, "System", "Average", EngineDrivenChiller( ChillerNum ).Name );
				SetupOutputVariable( "Chiller Condenser Mass Flow Rate [kg/s]", EngineDrivenChiller( ChillerNum ).reports.Condmdot, "System", "Average", EngineDrivenChiller( ChillerNum ).Name );
			} else if ( EngineDrivenChiller( ChillerNum ).CondenserType == AirCooled ) {
				SetupOutputVariable( "Chiller Condenser Inlet Temperature [C]", EngineDrivenChiller( ChillerNum ).reports.CondInletTemp, "System", "Average", EngineDrivenChiller( ChillerNum ).Name );
			} else if ( EngineDrivenChiller( ChillerNum ).CondenserType == EvapCooled ) {
				SetupOutputVariable( "Chiller Condenser Inlet Temperature [C]", EngineDrivenChiller( ChillerNum ).reports.CondInletTemp, "System", "Average", EngineDrivenChiller( ChillerNum ).Name );
				if ( EngineDrivenChiller( ChillerNum ).BasinHeaterPowerFTempDiff > 0.0 ) {
					SetupOutputVariable( "Chiller Basin Heater Electric Power [W]", EngineDrivenChiller( ChillerNum ).reports.BasinHeaterPower, "System", "Average", EngineDrivenChiller( ChillerNum ).Name );
					SetupOutputVariable( "Chiller Basin Heater Electric Energy [J]", EngineDrivenChiller( ChillerNum ).reports.BasinHeaterConsumption, "System", "Sum", EngineDrivenChiller( ChillerNum ).Name, _, "Electric", "CHILLERS", _, "Plant" );
				}
			}

			SetupOutputVariable( "Chiller " + EngineDrivenChiller( ChillerNum ).FuelType + " Rate [W]", EngineDrivenChiller( ChillerNum ).reports.FuelEnergyUseRate, "System", "Average", EngineDrivenChiller( ChillerNum ).Name );
			SetupOutputVariable( "Chiller " + EngineDrivenChiller( ChillerNum ).FuelType + " Energy [J]", EngineDrivenChiller( ChillerNum ).reports.FuelEnergy, "System", "Sum", EngineDrivenChiller( ChillerNum ).Name, _, EngineDrivenChiller( ChillerNum ).FuelType, "Cooling", _, "Plant" );

			SetupOutputVariable( "Chiller COP [W/W]", EngineDrivenChiller( ChillerNum ).reports.FuelCOP, "System", "Average", EngineDrivenChiller( ChillerNum ).Name );

			SetupOutputVariable( "Chiller " + EngineDrivenChiller( ChillerNum ).FuelType + " Mass Flow Rate [kg/s]", EngineDrivenChiller( ChillerNum ).reports.FuelMdot, "System", "Average", EngineDrivenChiller( ChillerNum ).Name );

			SetupOutputVariable( "Chiller Exhaust Temperature [C]", EngineDrivenChiller( ChillerNum ).reports.ExhaustStackTemp, "System", "Average", EngineDrivenChiller( ChillerNum ).Name );

			SetupOutputVariable( "Chiller Heat Recovery Mass Flow Rate [kg/s]", EngineDrivenChiller( ChillerNum ).reports.HeatRecMdot, "System", "Average", EngineDrivenChiller( ChillerNum ).Name );

			if ( EngineDrivenChiller( ChillerNum ).HeatRecActive ) {
				// need to only report if heat recovery active
				SetupOutputVariable( "Chiller Jacket Recovered Heat Rate [W]", EngineDrivenChiller( ChillerNum ).reports.QJacketRecovered, "System", "Average", EngineDrivenChiller( ChillerNum ).Name );
				SetupOutputVariable( "Chiller Jacket Recovered Heat Energy [J]", EngineDrivenChiller( ChillerNum ).reports.JacketEnergyRec, "System", "Sum", EngineDrivenChiller( ChillerNum ).Name, _, "ENERGYTRANSFER", "HEATRECOVERY", _, "Plant" );

				SetupOutputVariable( "Chiller Lube Recovered Heat Rate [W]", EngineDrivenChiller( ChillerNum ).reports.QLubeOilRecovered, "System", "Average", EngineDrivenChiller( ChillerNum ).Name );
				SetupOutputVariable( "Chiller Lube Recovered Heat Energy [J]", EngineDrivenChiller( ChillerNum ).reports.LubeOilEnergyRec, "System", "Sum", EngineDrivenChiller( ChillerNum ).Name, _, "ENERGYTRANSFER", "HEATRECOVERY", _, "Plant" );

				SetupOutputVariable( "Chiller Exhaust Recovered Heat Rate [W]", EngineDrivenChiller( ChillerNum ).reports.QExhaustRecovered, "System", "Average", EngineDrivenChiller( ChillerNum ).Name );
				SetupOutputVariable( "Chiller Exhaust Recovered Heat Energy [J]", EngineDrivenChiller( ChillerNum ).reports.ExhaustEnergyRec, "System", "Sum", EngineDrivenChiller( ChillerNum ).Name, _, "ENERGYTRANSFER", "HEATRECOVERY", _, "Plant" );

				SetupOutputVariable( "Chiller Total Recovered Heat Rate [W]", EngineDrivenChiller( ChillerNum ).reports.QTotalHeatRecovered, "System", "Average", EngineDrivenChiller( ChillerNum ).Name );
				SetupOutputVariable( "Chiller Total Recovered Heat Energy [J]", EngineDrivenChiller( ChillerNum ).reports.TotalHeatEnergyRec, "System", "Sum", EngineDrivenChiller( ChillerNum ).Name );

				SetupOutputVariable( "Chiller Heat Recovery Inlet Temperature [C]", EngineDrivenChiller( ChillerNum ).reports.HeatRecInletTemp, "System", "Average", EngineDrivenChiller( ChillerNum ).Name );

				SetupOutputVariable( "Chiller Heat Recovery Outlet Temperature [C]", EngineDrivenChiller( ChillerNum ).reports.HeatRecOutletTemp, "System", "Average", EngineDrivenChiller( ChillerNum ).Name );

			}
			if ( AnyEnergyManagementSystemInModel ) {
				SetupEMSInternalVariable( "Chiller Nominal Capacity", EngineDrivenChiller( ChillerNum ).Name, "[W]", EngineDrivenChiller( ChillerNum ).NomCap );
			}
		}

	}

	void
	GetGTChillerInput()
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR:          Dan Fisher / Brandon Anderson
		//       DATE WRITTEN:    September 2000

		// PURPOSE OF THIS SUBROUTINE:
		// This routine will get the input
		// required by the GT Chiller model.

		// METHODOLOGY EMPLOYED:
		// EnergyPlus input processor

		// REFERENCES: na

		// Using/Aliasing
		using InputProcessor::GetNumObjectsFound;
		using InputProcessor::GetObjectItem;
		using InputProcessor::VerifyName;
		using namespace DataIPShortCuts; // Data for field names, blank numerics
		using BranchNodeConnections::TestCompSet;
		using NodeInputManager::GetOnlySingleNode;
		using GlobalNames::VerifyUniqueChillerName;
		using OutAirNodeManager::CheckAndAddAirNodeNumber;
		using General::RoundSigDigits;
		using PlantUtilities::RegisterPlantCompDesignFlow;
		using ScheduleManager::GetScheduleIndex;
		using DataGlobals::AnyEnergyManagementSystemInModel;
		using DataSizing::AutoSize;

		// Locals
		// PARAMETERS
		static std::string const RoutineName( "GetGTChillerInput: " ); // include trailing blank space
		//LOCAL VARIABLES
		int ChillerNum; // chiller counter
		int NumAlphas; // Number of elements in the alpha array
		int NumNums; // Number of elements in the numeric array
		int IOStat; // IO Status when calling get input subroutine
		static bool ErrorsFound( false );
		bool IsNotOK; // Flag to verify name
		bool IsBlank; // Flag for blank name
		bool errFlag;
		bool Okay;

		//FLOW
		cCurrentModuleObject = "Chiller:CombustionTurbine";
		NumGTChillers = GetNumObjectsFound( cCurrentModuleObject );

		if ( NumGTChillers <= 0 ) {
			return;
		}
		//See if load distribution manager has already gotten the input
		if ( allocated( GTChiller ) ) return;

		//ALLOCATE ARRAYS
		GTChiller.allocate( NumGTChillers );

		for ( ChillerNum = 1; ChillerNum <= NumGTChillers; ++ChillerNum ) {
			GetObjectItem( cCurrentModuleObject, ChillerNum, cAlphaArgs, NumAlphas, rNumericArgs, NumNums, IOStat, _, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );

			IsNotOK = false;
			IsBlank = false;
			VerifyName( cAlphaArgs( 1 ), GTChiller, ChillerNum - 1, IsNotOK, IsBlank, cCurrentModuleObject + " Name" );
			if ( IsNotOK ) {
				ErrorsFound = true;
				if ( IsBlank ) cAlphaArgs( 1 ) = "xxxxx";
			}
			VerifyUniqueChillerName( cCurrentModuleObject, cAlphaArgs( 1 ), errFlag, cCurrentModuleObject + " Name" );
			if ( errFlag ) {
				ErrorsFound = true;
			}
			GTChiller( ChillerNum ).Name = cAlphaArgs( 1 );

			GTChiller( ChillerNum ).NomCap = rNumericArgs( 1 );
			if ( rNumericArgs( 1 ) == 0.0 ) {
				ShowSevereError( "Invalid " + cNumericFieldNames( 1 ) + '=' + RoundSigDigits( rNumericArgs( 1 ), 2 ) );
				ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + cAlphaArgs( 1 ) );
				ErrorsFound = true;
			}

			GTChiller( ChillerNum ).COP = rNumericArgs( 2 );
			if ( rNumericArgs( 2 ) == 0.0 ) {
				ShowSevereError( "Invalid " + cNumericFieldNames( 2 ) + '=' + RoundSigDigits( rNumericArgs( 2 ), 2 ) );
				ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + cAlphaArgs( 1 ) );
				ErrorsFound = true;
			}

			if ( cAlphaArgs( 2 ) == "AIRCOOLED" ) {
				GTChiller( ChillerNum ).CondenserType = AirCooled;
			} else if ( cAlphaArgs( 2 ) == "WATERCOOLED" ) {
				GTChiller( ChillerNum ).CondenserType = WaterCooled;
			} else if ( cAlphaArgs( 2 ) == "EVAPORATIVELYCOOLED" ) {
				GTChiller( ChillerNum ).CondenserType = EvapCooled;
			} else {
				ShowSevereError( "Invalid " + cAlphaFieldNames( 2 ) + '=' + cAlphaArgs( 2 ) );
				ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + cAlphaArgs( 1 ) );
				ErrorsFound = true;
			}

			GTChiller( ChillerNum ).EvapInletNodeNum = GetOnlySingleNode( cAlphaArgs( 3 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Water, NodeConnectionType_Inlet, 1, ObjectIsNotParent );
			GTChiller( ChillerNum ).EvapOutletNodeNum = GetOnlySingleNode( cAlphaArgs( 4 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Water, NodeConnectionType_Outlet, 1, ObjectIsNotParent );
			TestCompSet( cCurrentModuleObject, cAlphaArgs( 1 ), cAlphaArgs( 3 ), cAlphaArgs( 4 ), "Chilled Water Nodes" );

			if ( GTChiller( ChillerNum ).CondenserType == AirCooled || GTChiller( ChillerNum ).CondenserType == EvapCooled ) {
				// Connection not required for air or evap cooled condenser
				// If the condenser inlet is blank for air cooled and evap cooled condensers then supply a generic name
				// since it is not used elsewhere for connection
				if ( lAlphaFieldBlanks( 5 ) ) {
					if ( len( cAlphaArgs( 1 ) ) < MaxNameLength - 21 ) { // protect against long name leading to > 100 chars
						cAlphaArgs( 5 ) = cAlphaArgs( 1 ) + " CONDENSER INLET NODE";
					} else {
						cAlphaArgs( 5 ) = cAlphaArgs( 1 ).substr( 0, 79 ) + " CONDENSER INLET NODE";
					}
				}
				if ( lAlphaFieldBlanks( 6 ) ) {
					if ( len( cAlphaArgs( 1 ) ) < MaxNameLength - 22 ) { // protect against long name leading to > 100 chars
						cAlphaArgs( 6 ) = cAlphaArgs( 1 ) + " CONDENSER OUTLET NODE";
					} else {
						cAlphaArgs( 6 ) = cAlphaArgs( 1 ).substr( 0, 78 ) + " CONDENSER OUTLET NODE";
					}
				}

				GTChiller( ChillerNum ).CondInletNodeNum = GetOnlySingleNode( cAlphaArgs( 5 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Air, NodeConnectionType_OutsideAirReference, 2, ObjectIsNotParent );
				CheckAndAddAirNodeNumber( GTChiller( ChillerNum ).CondInletNodeNum, Okay );
				if ( ! Okay ) {
					ShowWarningError( cCurrentModuleObject + ", Adding OutdoorAir:Node=" + cAlphaArgs( 5 ) );
				}

				GTChiller( ChillerNum ).CondOutletNodeNum = GetOnlySingleNode( cAlphaArgs( 6 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Air, NodeConnectionType_Outlet, 2, ObjectIsNotParent );
			} else { // WaterCooled CondenserType
				GTChiller( ChillerNum ).CondInletNodeNum = GetOnlySingleNode( cAlphaArgs( 5 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Unknown, NodeConnectionType_Inlet, 2, ObjectIsNotParent );
				GTChiller( ChillerNum ).CondOutletNodeNum = GetOnlySingleNode( cAlphaArgs( 6 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Unknown, NodeConnectionType_Outlet, 2, ObjectIsNotParent );
				TestCompSet( cCurrentModuleObject, cAlphaArgs( 1 ), cAlphaArgs( 5 ), cAlphaArgs( 6 ), "Condenser (unknown?) Nodes" );
				//Condenser Inlet node name is necessary for Water Cooled
				if ( lAlphaFieldBlanks( 5 ) ) {
					ShowSevereError( "Invalid, " + cAlphaFieldNames( 5 ) + " is blank " );
					ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + cAlphaArgs( 1 ) );
					ErrorsFound = true;
				} else if ( lAlphaFieldBlanks( 6 ) ) {
					ShowSevereError( "Invalid, " + cAlphaFieldNames( 6 ) + " is blank " );
					ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + cAlphaArgs( 1 ) );
					ErrorsFound = true;
				}
			}

			GTChiller( ChillerNum ).MinPartLoadRat = rNumericArgs( 3 );
			GTChiller( ChillerNum ).MaxPartLoadRat = rNumericArgs( 4 );
			GTChiller( ChillerNum ).OptPartLoadRat = rNumericArgs( 5 );
			GTChiller( ChillerNum ).TempDesCondIn = rNumericArgs( 6 );
			GTChiller( ChillerNum ).TempRiseCoef = rNumericArgs( 7 );
			GTChiller( ChillerNum ).TempDesEvapOut = rNumericArgs( 8 );
			GTChiller( ChillerNum ).EvapVolFlowRate = rNumericArgs( 9 );
			GTChiller( ChillerNum ).CondVolFlowRate = rNumericArgs( 10 );
			GTChiller( ChillerNum ).CapRatCoef( 1 ) = rNumericArgs( 11 );
			GTChiller( ChillerNum ).CapRatCoef( 2 ) = rNumericArgs( 12 );
			GTChiller( ChillerNum ).CapRatCoef( 3 ) = rNumericArgs( 13 );
			if ( ( rNumericArgs( 11 ) + rNumericArgs( 12 ) + rNumericArgs( 13 ) ) == 0.0 ) {
				ShowSevereError( cCurrentModuleObject + ": Sum of Capacity Ratio Coef = 0.0, chiller=" + cAlphaArgs( 1 ) );
				ErrorsFound = true;
			}
			GTChiller( ChillerNum ).PowerRatCoef( 1 ) = rNumericArgs( 14 );
			GTChiller( ChillerNum ).PowerRatCoef( 2 ) = rNumericArgs( 15 );
			GTChiller( ChillerNum ).PowerRatCoef( 3 ) = rNumericArgs( 16 );
			GTChiller( ChillerNum ).FullLoadCoef( 1 ) = rNumericArgs( 17 );
			GTChiller( ChillerNum ).FullLoadCoef( 2 ) = rNumericArgs( 18 );
			GTChiller( ChillerNum ).FullLoadCoef( 3 ) = rNumericArgs( 19 );
			GTChiller( ChillerNum ).TempLowLimitEvapOut = rNumericArgs( 20 );

			//Load Special GT Chiller Input

			GTChiller( ChillerNum ).PLBasedFuelInputCoef( 1 ) = rNumericArgs( 21 );
			GTChiller( ChillerNum ).PLBasedFuelInputCoef( 2 ) = rNumericArgs( 22 );
			GTChiller( ChillerNum ).PLBasedFuelInputCoef( 3 ) = rNumericArgs( 23 );

			GTChiller( ChillerNum ).TempBasedFuelInputCoef( 1 ) = rNumericArgs( 24 );
			GTChiller( ChillerNum ).TempBasedFuelInputCoef( 2 ) = rNumericArgs( 25 );
			GTChiller( ChillerNum ).TempBasedFuelInputCoef( 3 ) = rNumericArgs( 26 );

			GTChiller( ChillerNum ).ExhaustFlowCoef( 1 ) = rNumericArgs( 27 );
			GTChiller( ChillerNum ).ExhaustFlowCoef( 2 ) = rNumericArgs( 28 );
			GTChiller( ChillerNum ).ExhaustFlowCoef( 3 ) = rNumericArgs( 29 );

			GTChiller( ChillerNum ).PLBasedExhaustTempCoef( 1 ) = rNumericArgs( 30 );
			GTChiller( ChillerNum ).PLBasedExhaustTempCoef( 2 ) = rNumericArgs( 31 );
			GTChiller( ChillerNum ).PLBasedExhaustTempCoef( 3 ) = rNumericArgs( 32 );

			GTChiller( ChillerNum ).TempBasedExhaustTempCoef( 1 ) = rNumericArgs( 33 );
			GTChiller( ChillerNum ).TempBasedExhaustTempCoef( 2 ) = rNumericArgs( 34 );
			GTChiller( ChillerNum ).TempBasedExhaustTempCoef( 3 ) = rNumericArgs( 35 );

			GTChiller( ChillerNum ).HeatRecLubeEnergyCoef( 1 ) = rNumericArgs( 36 );
			GTChiller( ChillerNum ).HeatRecLubeEnergyCoef( 2 ) = rNumericArgs( 37 );
			GTChiller( ChillerNum ).HeatRecLubeEnergyCoef( 3 ) = rNumericArgs( 38 );

			GTChiller( ChillerNum ).UAtoCapCoef( 1 ) = rNumericArgs( 39 );
			GTChiller( ChillerNum ).UAtoCapCoef( 2 ) = rNumericArgs( 40 );

			GTChiller( ChillerNum ).GTEngineCapacity = rNumericArgs( 41 );
			if ( GTChiller( ChillerNum ).GTEngineCapacity == AutoSize ) {
				GTChiller( ChillerNum ).GTEngineCapacityWasAutoSized = true;
			}
			GTChiller( ChillerNum ).MaxExhaustperGTPower = rNumericArgs( 42 );
			GTChiller( ChillerNum ).DesignSteamSatTemp = rNumericArgs( 43 );
			GTChiller( ChillerNum ).FuelHeatingValue = rNumericArgs( 44 );

			//Get the Heat Recovery information
			GTChiller( ChillerNum ).DesignHeatRecVolFlowRate = rNumericArgs( 45 );
			if ( GTChiller( ChillerNum ).DesignHeatRecVolFlowRate > 0.0 ) {
				GTChiller( ChillerNum ).HeatRecActive = true;
				GTChiller( ChillerNum ).HeatRecInletNodeNum = GetOnlySingleNode( cAlphaArgs( 7 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Water, NodeConnectionType_Inlet, 3, ObjectIsNotParent );
				if ( GTChiller( ChillerNum ).HeatRecInletNodeNum == 0 ) {
					ShowSevereError( "Invalid " + cAlphaFieldNames( 7 ) + '=' + cAlphaArgs( 7 ) );
					ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + cAlphaArgs( 1 ) );
					ErrorsFound = true;
				}
				GTChiller( ChillerNum ).HeatRecOutletNodeNum = GetOnlySingleNode( cAlphaArgs( 8 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Water, NodeConnectionType_Outlet, 3, ObjectIsNotParent );
				if ( GTChiller( ChillerNum ).HeatRecOutletNodeNum == 0 ) {
					ShowSevereError( "Invalid " + cAlphaFieldNames( 8 ) + '=' + cAlphaArgs( 8 ) );
					ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + cAlphaArgs( 1 ) );
					ErrorsFound = true;
				}
				TestCompSet( cCurrentModuleObject, cAlphaArgs( 1 ), cAlphaArgs( 7 ), cAlphaArgs( 8 ), "Heat Recovery Nodes" );

				RegisterPlantCompDesignFlow( GTChiller( ChillerNum ).HeatRecInletNodeNum, GTChiller( ChillerNum ).DesignHeatRecVolFlowRate );
				// Condenser flow rate must be specified for heat reclaim
				if ( GTChiller( ChillerNum ).CondenserType == AirCooled || GTChiller( ChillerNum ).CondenserType == EvapCooled ) {
					if ( GTChiller( ChillerNum ).CondVolFlowRate <= 0.0 ) {
						ShowSevereError( "Invalid " + cNumericFieldNames( 10 ) + '=' + RoundSigDigits( rNumericArgs( 10 ), 6 ) );
						ShowSevereError( "Condenser fluid flow rate must be specified for Heat Reclaim applications." );
						ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + cAlphaArgs( 1 ) );
						ErrorsFound = true;
					}

				}

			} else {
				GTChiller( ChillerNum ).HeatRecActive = false;
				GTChiller( ChillerNum ).DesignHeatRecMassFlowRate = 0.0;
				GTChiller( ChillerNum ).HeatRecInletNodeNum = 0;
				GTChiller( ChillerNum ).HeatRecOutletNodeNum = 0;
				if ( ( ! lAlphaFieldBlanks( 7 ) ) || ( ! lAlphaFieldBlanks( 8 ) ) ) {
					ShowWarningError( "Since Design Heat Flow Rate = 0.0, Heat Recovery inactive for " + cCurrentModuleObject + '=' + cAlphaArgs( 1 ) );
					ShowContinueError( "However, Node names were specified for heat recovery inlet or outlet nodes" );
				}
				if ( GTChiller( ChillerNum ).CondenserType == AirCooled || GTChiller( ChillerNum ).CondenserType == EvapCooled ) {
					GTChiller( ChillerNum ).CondVolFlowRate = 0.0011; // set to avoid errors in calc routine
				}
			}

			{ auto const SELECT_CASE_var( cAlphaArgs( 9 ) );
			if ( SELECT_CASE_var == "CONSTANTFLOW" ) {
				GTChiller( ChillerNum ).FlowMode = ConstantFlow;
			} else if ( SELECT_CASE_var == "VARIABLEFLOW" ) {
				GTChiller( ChillerNum ).FlowMode = LeavingSetPointModulated;
				ShowWarningError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\"," );
				ShowContinueError( "Invalid " + cAlphaFieldNames( 9 ) + '=' + cAlphaArgs( 9 ) );
				ShowContinueError( "Key choice is now called \"LeavingSetpointModulated\" and the simulation continues" );
			} else if ( SELECT_CASE_var == "LEAVINGSETPOINTMODULATED" ) {
				GTChiller( ChillerNum ).FlowMode = LeavingSetPointModulated;
			} else if ( SELECT_CASE_var == "NOTMODULATED" ) {
				GTChiller( ChillerNum ).FlowMode = NotModulated;
			} else {
				ShowSevereError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\"," );
				ShowContinueError( "Invalid " + cAlphaFieldNames( 9 ) + '=' + cAlphaArgs( 9 ) );
				ShowContinueError( "Available choices are ConstantFlow, NotModulated, or LeavingSetpointModulated" );
				ShowContinueError( "Flow mode NotModulated is assumed and the simulation continues." );
				GTChiller( ChillerNum ).FlowMode = NotModulated;
			}}

			//Fuel Type Case Statement
			{ auto const SELECT_CASE_var( cAlphaArgs( 10 ) );
			if ( ( SELECT_CASE_var == "GAS" ) || ( SELECT_CASE_var == "NATURALGAS" ) || ( SELECT_CASE_var == "NATURAL GAS" ) ) {
				GTChiller( ChillerNum ).FuelType = "Gas";

			} else if ( SELECT_CASE_var == "DIESEL" ) {
				GTChiller( ChillerNum ).FuelType = "Diesel";

			} else if ( SELECT_CASE_var == "GASOLINE" ) {
				GTChiller( ChillerNum ).FuelType = "Gasoline";

			} else if ( ( SELECT_CASE_var == "FUEL OIL #1" ) || ( SELECT_CASE_var == "FUELOIL#1" ) || ( SELECT_CASE_var == "FUEL OIL" ) || ( SELECT_CASE_var == "DISTILLATE OIL" ) ) {
				GTChiller( ChillerNum ).FuelType = "FuelOil#1";

			} else if ( ( SELECT_CASE_var == "FUEL OIL #2" ) || ( SELECT_CASE_var == "FUELOIL#2" ) || ( SELECT_CASE_var == "RESIDUAL OIL" ) ) {
				GTChiller( ChillerNum ).FuelType = "FuelOil#2";

			} else if ( ( SELECT_CASE_var == "PROPANE" ) || ( SELECT_CASE_var == "LPG" ) || ( SELECT_CASE_var == "PROPANEGAS" ) || ( SELECT_CASE_var == "PROPANE GAS" ) ) {
				GTChiller( ChillerNum ).FuelType = "Propane";

			} else if ( SELECT_CASE_var == "OTHERFUEL1" ) {
				GTChiller( ChillerNum ).FuelType = "OtherFuel1";

			} else if ( SELECT_CASE_var == "OTHERFUEL2" ) {
				GTChiller( ChillerNum ).FuelType = "OtherFuel2";

			} else {
				ShowSevereError( "Invalid " + cAlphaFieldNames( 10 ) + '=' + cAlphaArgs( 10 ) );
				ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + cAlphaArgs( 1 ) );
				ShowContinueError( "Valid choices are Electricity, NaturalGas, PropaneGas, Diesel, Gasoline, FuelOil#1, FuelOil#2,OtherFuel1 or OtherFuel2" );
				ErrorsFound = true;
			}}

			GTChiller( ChillerNum ).HeatRecMaxTemp = rNumericArgs( 46 );
			GTChiller( ChillerNum ).SizFac = rNumericArgs( 47 );
			if ( GTChiller( ChillerNum ).SizFac <= 0.0 ) GTChiller( ChillerNum ).SizFac = 1.0;

			//   Basin heater power as a function of temperature must be greater than or equal to 0
			GTChiller( ChillerNum ).BasinHeaterPowerFTempDiff = rNumericArgs( 48 );
			if ( rNumericArgs( 48 ) < 0.0 ) {
				ShowSevereError( cCurrentModuleObject + "=\"" + GTChiller( ChillerNum ).Name + "\"" + cNumericFieldNames( 48 ) + " must be >= 0" );
				ErrorsFound = true;
			}

			GTChiller( ChillerNum ).BasinHeaterSetPointTemp = rNumericArgs( 49 );

			if ( GTChiller( ChillerNum ).BasinHeaterPowerFTempDiff > 0.0 ) {
				if ( NumNums < 49 ) {
					GTChiller( ChillerNum ).BasinHeaterSetPointTemp = 2.0;
				}
				if ( GTChiller( ChillerNum ).BasinHeaterSetPointTemp < 2.0 ) {
					ShowWarningError( cCurrentModuleObject + ":\"" + GTChiller( ChillerNum ).Name + "\", " + cNumericFieldNames( 49 ) + " is less than 2 deg C. Freezing could occur." );
				}
			}

			if ( ! lAlphaFieldBlanks( 11 ) ) {
				GTChiller( ChillerNum ).BasinHeaterSchedulePtr = GetScheduleIndex( cAlphaArgs( 11 ) );
				if ( GTChiller( ChillerNum ).BasinHeaterSchedulePtr == 0 ) {
					ShowWarningError( cCurrentModuleObject + ", \"" + GTChiller( ChillerNum ).Name + "\" TRIM(cAlphaFieldNames(11)) \"" + cAlphaArgs( 11 ) + "\" was not found. Basin heater operation will not be modeled and the simulation continues" );
				}
			}

		}

		if ( ErrorsFound ) {
			ShowFatalError( "Errors found in processing input for " + cCurrentModuleObject );
		}

		for ( ChillerNum = 1; ChillerNum <= NumGTChillers; ++ChillerNum ) {
			SetupOutputVariable( "Chiller Drive Shaft Power [W]", GTChiller( ChillerNum ).reports.Power, "System", "Average", GTChiller( ChillerNum ).Name );
			SetupOutputVariable( "Chiller Drive Shaft Energy [J]", GTChiller( ChillerNum ).reports.Energy, "System", "Sum", GTChiller( ChillerNum ).Name );

			SetupOutputVariable( "Chiller Evaporator Cooling Rate [W]", GTChiller( ChillerNum ).reports.QEvap, "System", "Average", GTChiller( ChillerNum ).Name );
			SetupOutputVariable( "Chiller Evaporator Cooling Energy [J]", GTChiller( ChillerNum ).reports.EvapEnergy, "System", "Sum", GTChiller( ChillerNum ).Name, _, "ENERGYTRANSFER", "CHILLERS", _, "Plant" );
			SetupOutputVariable( "Chiller Evaporator Inlet Temperature [C]", GTChiller( ChillerNum ).reports.EvapInletTemp, "System", "Average", GTChiller( ChillerNum ).Name );
			SetupOutputVariable( "Chiller Evaporator Outlet Temperature [C]", GTChiller( ChillerNum ).reports.EvapOutletTemp, "System", "Average", GTChiller( ChillerNum ).Name );
			SetupOutputVariable( "Chiller Evaporator Mass Flow Rate [kg/s]", GTChiller( ChillerNum ).reports.Evapmdot, "System", "Average", GTChiller( ChillerNum ).Name );

			SetupOutputVariable( "Chiller Condenser Heat Transfer Rate [W]", GTChiller( ChillerNum ).reports.QCond, "System", "Average", GTChiller( ChillerNum ).Name );
			SetupOutputVariable( "Chiller Condenser Heat Transfer Energy [J]", GTChiller( ChillerNum ).reports.CondEnergy, "System", "Sum", GTChiller( ChillerNum ).Name, _, "ENERGYTRANSFER", "HEATREJECTION", _, "Plant" );

			//Condenser mass flow and outlet temp are valid for water cooled
			if ( GTChiller( ChillerNum ).CondenserType == WaterCooled ) {
				SetupOutputVariable( "Chiller Condenser Inlet Temperature [C]", GTChiller( ChillerNum ).reports.CondInletTemp, "System", "Average", GTChiller( ChillerNum ).Name );
				SetupOutputVariable( "Chiller Condenser Outlet Temperature [C]", GTChiller( ChillerNum ).reports.CondOutletTemp, "System", "Average", GTChiller( ChillerNum ).Name );
				SetupOutputVariable( "Chiller Condenser Mass Flow Rate [kg/s]", GTChiller( ChillerNum ).reports.Condmdot, "System", "Average", GTChiller( ChillerNum ).Name );
			} else if ( GTChiller( ChillerNum ).CondenserType == AirCooled ) {
				SetupOutputVariable( "Chiller Condenser Inlet Temperature [C]", GTChiller( ChillerNum ).reports.CondInletTemp, "System", "Average", GTChiller( ChillerNum ).Name );
			} else if ( GTChiller( ChillerNum ).CondenserType == EvapCooled ) {
				SetupOutputVariable( "Chiller Condenser Inlet Temperature [C]", GTChiller( ChillerNum ).reports.CondInletTemp, "System", "Average", GTChiller( ChillerNum ).Name );
				if ( GTChiller( ChillerNum ).BasinHeaterPowerFTempDiff > 0.0 ) {
					SetupOutputVariable( "Chiller Basin Heater Electric Power [W]", GTChiller( ChillerNum ).reports.BasinHeaterPower, "System", "Average", GTChiller( ChillerNum ).Name );
					SetupOutputVariable( "Chiller Basin Heater Electric Energy [J]", GTChiller( ChillerNum ).reports.BasinHeaterConsumption, "System", "Sum", GTChiller( ChillerNum ).Name, _, "Electric", "CHILLERS", _, "Plant" );
				}
			}

			SetupOutputVariable( "Chiller Lube Recovered Heat Rate [W]", GTChiller( ChillerNum ).reports.HeatRecLubeRate, "System", "Average", GTChiller( ChillerNum ).Name );
			SetupOutputVariable( "Chiller Lube Recovered Heat Energy [J]", GTChiller( ChillerNum ).reports.HeatRecLubeEnergy, "System", "Sum", GTChiller( ChillerNum ).Name, _, "ENERGYTRANSFER", "HeatRecovery", _, "Plant" );

			SetupOutputVariable( "Chiller " + GTChiller( ChillerNum ).FuelType + " Rate [W]", GTChiller( ChillerNum ).reports.FuelEnergyUsedRate, "System", "Average", GTChiller( ChillerNum ).Name );

			SetupOutputVariable( "Chiller " + GTChiller( ChillerNum ).FuelType + " Energy [J]", GTChiller( ChillerNum ).reports.FuelEnergyUsed, "System", "Sum", GTChiller( ChillerNum ).Name, _, GTChiller( ChillerNum ).FuelType, "Cooling", _, "Plant" );

			SetupOutputVariable( "Chiller " + GTChiller( ChillerNum ).FuelType + " Mass Flow Rate [kg/s]", GTChiller( ChillerNum ).reports.FuelMassUsedRate, "System", "Average", GTChiller( ChillerNum ).Name );

			SetupOutputVariable( "Chiller " + GTChiller( ChillerNum ).FuelType + " Mass [kg]", GTChiller( ChillerNum ).reports.FuelMassUsed, "System", "Sum", GTChiller( ChillerNum ).Name );

			SetupOutputVariable( "Chiller Exhaust Temperature [C]", GTChiller( ChillerNum ).reports.ExhaustStackTemp, "System", "Average", GTChiller( ChillerNum ).Name );

			SetupOutputVariable( "Chiller Heat Recovery Inlet Temperature [C]", GTChiller( ChillerNum ).reports.HeatRecInletTemp, "System", "Average", GTChiller( ChillerNum ).Name );

			SetupOutputVariable( "Chiller Heat Recovery Outlet Temperature [C]", GTChiller( ChillerNum ).reports.HeatRecOutletTemp, "System", "Average", GTChiller( ChillerNum ).Name );

			SetupOutputVariable( "Chiller Heat Recovery Mass Flow Rate [kg/s]", GTChiller( ChillerNum ).reports.HeatRecMdot, "System", "Average", GTChiller( ChillerNum ).Name );

			SetupOutputVariable( "Chiller COP [W/W]", GTChiller( ChillerNum ).reports.FuelCOP, "System", "Average", GTChiller( ChillerNum ).Name );

			if ( AnyEnergyManagementSystemInModel ) {
				SetupEMSInternalVariable( "Chiller Nominal Capacity", GTChiller( ChillerNum ).Name, "[W]", GTChiller( ChillerNum ).NomCap );
			}

		}

	}

	void
	GetConstCOPChillerInput()
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR:          Dan Fisher
		//       DATE WRITTEN:    April 1998

		// PURPOSE OF THIS SUBROUTINE:!This routine will get the input
		//required by the PrimaryPlantLoopManager.  As such
		//it will interact with the Input Scanner to retrieve
		//information from the input file, count the number of
		//heating and cooling loops and begin to fill the
		//arrays associated with the type PlantLoopProps.

		// METHODOLOGY EMPLOYED: to be determined...
		// REFERENCES:

		// Using/Aliasing
		using InputProcessor::GetNumObjectsFound;
		using InputProcessor::GetObjectItem;
		using InputProcessor::VerifyName;
		using namespace DataIPShortCuts; // Data for field names, blank numerics
		using BranchNodeConnections::TestCompSet;
		using NodeInputManager::GetOnlySingleNode;
		using GlobalNames::VerifyUniqueChillerName;
		using namespace OutputReportPredefined;
		using OutAirNodeManager::CheckAndAddAirNodeNumber;
		using General::RoundSigDigits;
		using ScheduleManager::GetScheduleIndex;
		using DataGlobals::AnyEnergyManagementSystemInModel;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "GetConstCOPChillerInput: " ); // include trailing blank space

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int ChillerNum;
		int NumAlphas; // Number of elements in the alpha array
		int NumNums; // Number of elements in the numeric array
		int IOStat; // IO Status when calling get input subroutine
		static bool ErrorsFound( false );
		bool IsNotOK; // Flag to verify name
		bool IsBlank; // Flag for blank name
		bool errFlag;
		bool Okay;

		//GET NUMBER OF ALL EQUIPMENT TYPES
		cCurrentModuleObject = "Chiller:ConstantCOP";
		NumConstCOPChillers = GetNumObjectsFound( cCurrentModuleObject );

		if ( NumConstCOPChillers <= 0 ) {
			return;
		}

		//See if load distribution manager has already gotten the input
		if ( allocated( ConstCOPChiller ) ) return;

		ConstCOPChiller.allocate( NumConstCOPChillers );

		//LOAD ARRAYS WITH BLAST ConstCOP CHILLER DATA
		for ( ChillerNum = 1; ChillerNum <= NumConstCOPChillers; ++ChillerNum ) {
			GetObjectItem( cCurrentModuleObject, ChillerNum, cAlphaArgs, NumAlphas, rNumericArgs, NumNums, IOStat, _, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );

			IsNotOK = false;
			IsBlank = false;
			VerifyName( cAlphaArgs( 1 ), ConstCOPChiller, ChillerNum - 1, IsNotOK, IsBlank, cCurrentModuleObject + " Name" );
			if ( IsNotOK ) {
				ErrorsFound = true;
				if ( IsBlank ) cAlphaArgs( 1 ) = "xxxxx";
			}
			VerifyUniqueChillerName( cCurrentModuleObject, cAlphaArgs( 1 ), errFlag, cCurrentModuleObject + " Name" );
			if ( errFlag ) {
				ErrorsFound = true;
			}
			ConstCOPChiller( ChillerNum ).Name = cAlphaArgs( 1 );
			ConstCOPChiller( ChillerNum ).NomCap = rNumericArgs( 1 );
			if ( rNumericArgs( 1 ) == 0.0 ) {
				ShowSevereError( "Invalid " + cNumericFieldNames( 1 ) + '=' + RoundSigDigits( rNumericArgs( 1 ), 2 ) );
				ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + cAlphaArgs( 1 ) );
				ErrorsFound = true;
			}
			ConstCOPChiller( ChillerNum ).COP = rNumericArgs( 2 );
			if ( rNumericArgs( 2 ) == 0.0 ) {
				ShowSevereError( "Invalid " + cNumericFieldNames( 2 ) + '=' + RoundSigDigits( rNumericArgs( 2 ), 2 ) );
				ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + cAlphaArgs( 1 ) );
				ErrorsFound = true;
			}

			//Set the Condenser Type from input
			if ( cAlphaArgs( 6 ) == "AIRCOOLED" ) {
				ConstCOPChiller( ChillerNum ).CondenserType = AirCooled;
			} else if ( cAlphaArgs( 6 ) == "EVAPORATIVELYCOOLED" ) {
				ConstCOPChiller( ChillerNum ).CondenserType = EvapCooled;
			} else if ( cAlphaArgs( 6 ) == "WATERCOOLED" ) {
				ConstCOPChiller( ChillerNum ).CondenserType = WaterCooled;
			} else {
				ShowSevereError( "Invalid " + cAlphaFieldNames( 6 ) + '=' + cAlphaArgs( 6 ) );
				ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + cAlphaArgs( 1 ) );
				ErrorsFound = true;
			}

			ConstCOPChiller( ChillerNum ).EvapVolFlowRate = rNumericArgs( 3 );
			if ( ConstCOPChiller( ChillerNum ).CondenserType == AirCooled || ConstCOPChiller( ChillerNum ).CondenserType == EvapCooled ) { // Condenser flow rate not used for these cond types
				ConstCOPChiller( ChillerNum ).CondVolFlowRate = 0.0011;
			} else {
				ConstCOPChiller( ChillerNum ).CondVolFlowRate = rNumericArgs( 4 );
			}
			ConstCOPChiller( ChillerNum ).SizFac = rNumericArgs( 5 );

			ConstCOPChiller( ChillerNum ).EvapInletNodeNum = GetOnlySingleNode( cAlphaArgs( 2 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Water, NodeConnectionType_Inlet, 1, ObjectIsNotParent );
			ConstCOPChiller( ChillerNum ).EvapOutletNodeNum = GetOnlySingleNode( cAlphaArgs( 3 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Water, NodeConnectionType_Outlet, 1, ObjectIsNotParent );
			TestCompSet( cCurrentModuleObject, cAlphaArgs( 1 ), cAlphaArgs( 2 ), cAlphaArgs( 3 ), "Chilled Water Nodes" );

			if ( ConstCOPChiller( ChillerNum ).CondenserType == AirCooled || ConstCOPChiller( ChillerNum ).CondenserType == EvapCooled ) {
				// Connection not required for air or evap cooled condenser
				//If the condenser inlet is blank for air cooled and evap cooled condensers then supply a generic name
				//  since it is not used elsewhere for connection
				if ( lAlphaFieldBlanks( 4 ) ) {
					if ( len( cAlphaArgs( 1 ) ) < MaxNameLength - 21 ) { // protect against long name leading to > 100 chars
						cAlphaArgs( 4 ) = cAlphaArgs( 1 ) + " CONDENSER INLET NODE";
					} else {
						cAlphaArgs( 4 ) = cAlphaArgs( 1 ).substr( 0, 79 ) + " CONDENSER INLET NODE";
					}
				}
				if ( lAlphaFieldBlanks( 5 ) ) {
					if ( len( cAlphaArgs( 1 ) ) < MaxNameLength - 22 ) { // protect against long name leading to > 100 chars
						cAlphaArgs( 5 ) = cAlphaArgs( 1 ) + " CONDENSER OUTLET NODE";
					} else {
						cAlphaArgs( 5 ) = cAlphaArgs( 1 ).substr( 0, 78 ) + " CONDENSER OUTLET NODE";
					}
				}

				ConstCOPChiller( ChillerNum ).CondInletNodeNum = GetOnlySingleNode( cAlphaArgs( 4 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Air, NodeConnectionType_OutsideAirReference, 2, ObjectIsNotParent );
				CheckAndAddAirNodeNumber( ConstCOPChiller( ChillerNum ).CondInletNodeNum, Okay );
				if ( ! Okay ) {
					ShowWarningError( cCurrentModuleObject + ", Adding OutdoorAir:Node=" + cAlphaArgs( 4 ) );
				}

				ConstCOPChiller( ChillerNum ).CondOutletNodeNum = GetOnlySingleNode( cAlphaArgs( 5 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Air, NodeConnectionType_Outlet, 2, ObjectIsNotParent );
			} else if ( ConstCOPChiller( ChillerNum ).CondenserType == WaterCooled ) {
				ConstCOPChiller( ChillerNum ).CondInletNodeNum = GetOnlySingleNode( cAlphaArgs( 4 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Water, NodeConnectionType_Inlet, 2, ObjectIsNotParent );
				ConstCOPChiller( ChillerNum ).CondOutletNodeNum = GetOnlySingleNode( cAlphaArgs( 5 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Water, NodeConnectionType_Outlet, 2, ObjectIsNotParent );
				TestCompSet( cCurrentModuleObject, cAlphaArgs( 1 ), cAlphaArgs( 4 ), cAlphaArgs( 5 ), "Condenser Water Nodes" );
				//Condenser Inlet node name is necessary for Water Cooled
				if ( lAlphaFieldBlanks( 4 ) ) {
					ShowSevereError( "Invalid, " + cAlphaFieldNames( 4 ) + "is blank " );
					ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + cAlphaArgs( 1 ) );
					ErrorsFound = true;
				} else if ( lAlphaFieldBlanks( 5 ) ) {
					ShowSevereError( "Invalid, " + cAlphaFieldNames( 5 ) + "is blank " );
					ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + cAlphaArgs( 1 ) );
					ErrorsFound = true;
				}
			} else {
				ConstCOPChiller( ChillerNum ).CondInletNodeNum = GetOnlySingleNode( cAlphaArgs( 4 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Unknown, NodeConnectionType_Inlet, 2, ObjectIsNotParent );
				ConstCOPChiller( ChillerNum ).CondOutletNodeNum = GetOnlySingleNode( cAlphaArgs( 5 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Unknown, NodeConnectionType_Outlet, 2, ObjectIsNotParent );
				TestCompSet( cCurrentModuleObject, cAlphaArgs( 1 ), cAlphaArgs( 4 ), cAlphaArgs( 5 ), "Condenser (unknown?) Nodes" );
				//Condenser Inlet node name is necessary for Water Cooled
				if ( lAlphaFieldBlanks( 4 ) ) {
					ShowSevereError( "Invalid, " + cAlphaFieldNames( 4 ) + "is blank " );
					ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + cAlphaArgs( 1 ) );
					ErrorsFound = true;
				} else if ( lAlphaFieldBlanks( 5 ) ) {
					ShowSevereError( "Invalid, " + cAlphaFieldNames( 5 ) + "is blank " );
					ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + cAlphaArgs( 1 ) );
					ErrorsFound = true;
				}
			}

			{ auto const SELECT_CASE_var( cAlphaArgs( 7 ) );
			if ( SELECT_CASE_var == "CONSTANTFLOW" ) {
				ConstCOPChiller( ChillerNum ).FlowMode = ConstantFlow;
			} else if ( SELECT_CASE_var == "VARIABLEFLOW" ) {
				ConstCOPChiller( ChillerNum ).FlowMode = LeavingSetPointModulated;
				ShowWarningError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\"," );
				ShowContinueError( "Invalid " + cAlphaFieldNames( 7 ) + '=' + cAlphaArgs( 7 ) );
				ShowContinueError( "Key choice is now called \"LeavingSetpointModulated\" and the simulation continues" );
			} else if ( SELECT_CASE_var == "LEAVINGSETPOINTMODULATED" ) {
				ConstCOPChiller( ChillerNum ).FlowMode = LeavingSetPointModulated;
			} else if ( SELECT_CASE_var == "NOTMODULATED" ) {
				ConstCOPChiller( ChillerNum ).FlowMode = NotModulated;
			} else {
				ShowSevereError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\"," );
				ShowContinueError( "Invalid " + cAlphaFieldNames( 7 ) + '=' + cAlphaArgs( 7 ) );
				ShowContinueError( "Available choices are ConstantFlow, NotModulated, or LeavingSetpointModulated" );
				ShowContinueError( "Flow mode NotModulated is assumed and the simulation continues." );
				ConstCOPChiller( ChillerNum ).FlowMode = NotModulated;
			}}

			//   Basin heater power as a function of temperature must be greater than or equal to 0
			ConstCOPChiller( ChillerNum ).BasinHeaterPowerFTempDiff = rNumericArgs( 6 );
			if ( rNumericArgs( 6 ) < 0.0 ) {
				ShowSevereError( cCurrentModuleObject + ", \"" + ConstCOPChiller( ChillerNum ).Name + "\" TRIM(cNumericFieldNames(6)) must be >= 0" );
				ErrorsFound = true;
			}

			ConstCOPChiller( ChillerNum ).BasinHeaterSetPointTemp = rNumericArgs( 7 );

			if ( ConstCOPChiller( ChillerNum ).BasinHeaterPowerFTempDiff > 0.0 ) {
				if ( NumNums < 7 ) {
					ConstCOPChiller( ChillerNum ).BasinHeaterSetPointTemp = 2.0;
				}
				if ( ConstCOPChiller( ChillerNum ).BasinHeaterSetPointTemp < 2.0 ) {
					ShowWarningError( cCurrentModuleObject + ":\"" + ConstCOPChiller( ChillerNum ).Name + "\", " + cNumericFieldNames( 7 ) + " is less than 2 deg C. Freezing could occur." );
				}
			}

			if ( ! lAlphaFieldBlanks( 8 ) ) {
				ConstCOPChiller( ChillerNum ).BasinHeaterSchedulePtr = GetScheduleIndex( cAlphaArgs( 8 ) );
				if ( ConstCOPChiller( ChillerNum ).BasinHeaterSchedulePtr == 0 ) {
					ShowWarningError( cCurrentModuleObject + ", \"" + ConstCOPChiller( ChillerNum ).Name + "\" TRIM(cAlphaFieldNames(8)) \"" + cAlphaArgs( 8 ) + "\" was not found. Basin heater operation will not be modeled and the simulation continues" );
				}
			}

		}

		if ( ErrorsFound ) {
			ShowFatalError( "Errors found in processing input for " + cCurrentModuleObject );
		}

		for ( ChillerNum = 1; ChillerNum <= NumConstCOPChillers; ++ChillerNum ) {
			SetupOutputVariable( "Chiller Electric Power [W]", ConstCOPChiller( ChillerNum ).reports.Power, "System", "Average", ConstCOPChiller( ChillerNum ).Name );
			SetupOutputVariable( "Chiller Electric Energy [J]", ConstCOPChiller( ChillerNum ).reports.Energy, "System", "Sum", ConstCOPChiller( ChillerNum ).Name, _, "ELECTRICITY", "Cooling", _, "Plant" );

			SetupOutputVariable( "Chiller Evaporator Cooling Rate [W]", ConstCOPChiller( ChillerNum ).reports.QEvap, "System", "Average", ConstCOPChiller( ChillerNum ).Name );
			SetupOutputVariable( "Chiller Evaporator Cooling Energy [J]", ConstCOPChiller( ChillerNum ).reports.EvapEnergy, "System", "Sum", ConstCOPChiller( ChillerNum ).Name, _, "ENERGYTRANSFER", "CHILLERS", _, "Plant" );
			SetupOutputVariable( "Chiller Evaporator Inlet Temperature [C]", ConstCOPChiller( ChillerNum ).reports.EvapInletTemp, "System", "Average", ConstCOPChiller( ChillerNum ).Name );
			SetupOutputVariable( "Chiller Evaporator Outlet Temperature [C]", ConstCOPChiller( ChillerNum ).reports.EvapOutletTemp, "System", "Average", ConstCOPChiller( ChillerNum ).Name );
			SetupOutputVariable( "Chiller Evaporator Mass Flow Rate [kg/s]", ConstCOPChiller( ChillerNum ).reports.Evapmdot, "System", "Average", ConstCOPChiller( ChillerNum ).Name );
			SetupOutputVariable( "Chiller COP [W/W]", ConstCOPChiller( ChillerNum ).reports.ActualCOP, "System", "Average", ConstCOPChiller( ChillerNum ).Name );

			SetupOutputVariable( "Chiller Condenser Heat Transfer Rate [W]", ConstCOPChiller( ChillerNum ).reports.QCond, "System", "Average", ConstCOPChiller( ChillerNum ).Name );
			SetupOutputVariable( "Chiller Condenser Heat Transfer Energy [J]", ConstCOPChiller( ChillerNum ).reports.CondEnergy, "System", "Sum", ConstCOPChiller( ChillerNum ).Name, _, "ENERGYTRANSFER", "HEATREJECTION", _, "Plant" );

			//Condenser mass flow and outlet temp are valid for water cooled
			if ( ConstCOPChiller( ChillerNum ).CondenserType == WaterCooled ) {
				SetupOutputVariable( "Chiller Condenser Inlet Temperature [C]", ConstCOPChiller( ChillerNum ).reports.CondInletTemp, "System", "Average", ConstCOPChiller( ChillerNum ).Name );
				SetupOutputVariable( "Chiller Condenser Outlet Temperature [C]", ConstCOPChiller( ChillerNum ).reports.CondOutletTemp, "System", "Average", ConstCOPChiller( ChillerNum ).Name );
				SetupOutputVariable( "Chiller Condenser Mass Flow Rate [kg/s]", ConstCOPChiller( ChillerNum ).reports.Condmdot, "System", "Average", ConstCOPChiller( ChillerNum ).Name );
			} else if ( ConstCOPChiller( ChillerNum ).CondenserType == AirCooled ) {
				SetupOutputVariable( "Chiller Condenser Inlet Temperature [C]", ConstCOPChiller( ChillerNum ).reports.CondInletTemp, "System", "Average", ConstCOPChiller( ChillerNum ).Name );
			} else if ( ConstCOPChiller( ChillerNum ).CondenserType == EvapCooled ) {
				SetupOutputVariable( "Chiller Condenser Inlet Temperature [C]", ConstCOPChiller( ChillerNum ).reports.CondInletTemp, "System", "Average", ConstCOPChiller( ChillerNum ).Name );
				if ( ConstCOPChiller( ChillerNum ).BasinHeaterPowerFTempDiff > 0.0 ) {
					SetupOutputVariable( "Chiller Basin Heater Electric Power [W]", ConstCOPChiller( ChillerNum ).reports.BasinHeaterPower, "System", "Average", ConstCOPChiller( ChillerNum ).Name );
					SetupOutputVariable( "Chiller Basin Heater Electric Energy [J]", ConstCOPChiller( ChillerNum ).reports.BasinHeaterConsumption, "System", "Sum", ConstCOPChiller( ChillerNum ).Name, _, "Electric", "CHILLERS", _, "Plant" );
				}
			}
			if ( AnyEnergyManagementSystemInModel ) {
				SetupEMSInternalVariable( "Chiller Nominal Capacity", ConstCOPChiller( ChillerNum ).Name, "[W]", ConstCOPChiller( ChillerNum ).NomCap );
			}
		}

	}

	void
	ElectricChillerSpecs::InitElectricChiller(
		bool const RunFlag, // TRUE when chiller operating
		Real64 const MyLoad
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Fred Buhl
		//       DATE WRITTEN   April 2002
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine is for initializations of the Electric Chiller components

		// METHODOLOGY EMPLOYED:
		// Uses the status flags to trigger initializations.

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataGlobals::BeginEnvrnFlag;
		using DataGlobals::AnyEnergyManagementSystemInModel;
		using DataPlant::PlantLoop;
		using DataPlant::TypeOf_Chiller_Electric;
		using DataPlant::ScanPlantLoopsForObject;
		using DataPlant::PlantFirstSizesOkayToFinalize;
		using DataPlant::LoopFlowStatus_NeedyIfLoopOn;
		using DataPlant::SingleSetPoint;
		using DataPlant::DualSetPointDeadBand;
		using DataEnvironment::StdBaroPress;
		using Psychrometrics::PsyRhoAirFnPbTdbW;
		using PlantUtilities::InterConnectTwoPlantLoopSides;
		using PlantUtilities::InitComponentNodes;
		using PlantUtilities::SetComponentFlowRate;
		using FluidProperties::GetDensityGlycol;
		using EMSManager::iTemperatureSetPoint;
		using EMSManager::CheckIfNodeSetPointManagedByEMS;
		// na

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "InitElectricChiller" );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int CondInletNode; // node number of water inlet node to the condenser
		int CondOutletNode; // node number of water outlet node from the condenser
		int EvapInletNode;
		int EvapOutletNode;
		int HeatRecInNode;
		int HeatRecOutNode;
		bool errFlag;
		Real64 rho; // local fluid density
		Real64 mdot; // local mass flow rate
		Real64 mdotCond; // local mass flow rate for condenser
		Real64 THeatRecSetPoint( 0.0 ); // tests set point node for proper set point value

		int InletNode;
		int OutletNode;
		int LoopNum;
		int LoopSideNum;
		int BranchIndex;
		int CompIndex;
		bool FatalError;

		CondInletNode = this->CondInletNodeNum;
		CondOutletNode = this->CondOutletNodeNum;
		EvapInletNode = this->EvapInletNodeNum;
		EvapOutletNode = this->EvapOutletNodeNum;

		if ( this->HeatRecActive ) {
			HeatRecInNode = this->HeatRecInletNodeNum;
			HeatRecOutNode = this->HeatRecOutletNodeNum;
		}

		// Init more variables
		if ( this->MyFlag ) {
			// Locate the chillers on the plant loops for later usage
			errFlag = false;
			ScanPlantLoopsForObject( this->Name, TypeOf_Chiller_Electric, this->CWLoopNum, this->CWLoopSideNum, this->CWBranchNum, this->CWCompNum, this->TempLowLimitEvapOut, _, _, this->EvapInletNodeNum, _, errFlag );
			if ( this->CondenserType != AirCooled && this->CondenserType != EvapCooled ) {
				ScanPlantLoopsForObject( this->Name, TypeOf_Chiller_Electric, this->CDLoopNum, this->CDLoopSideNum, this->CDBranchNum, this->CDCompNum, _, _, _, this->CondInletNodeNum, _, errFlag );
				InterConnectTwoPlantLoopSides( this->CWLoopNum, this->CWLoopSideNum, this->CDLoopNum, this->CDLoopSideNum, TypeOf_Chiller_Electric, true );
			}
			if ( this->HeatRecActive ) {
				ScanPlantLoopsForObject( this->Name, TypeOf_Chiller_Electric, this->HRLoopNum, this->HRLoopSideNum, this->HRBranchNum, this->HRCompNum, _, _, _, this->HeatRecInletNodeNum, _, errFlag );
				InterConnectTwoPlantLoopSides( this->CWLoopNum, this->CWLoopSideNum, this->HRLoopNum, this->HRLoopSideNum, TypeOf_Chiller_Electric, true );
			}

			if ( this->CondenserType != AirCooled && this->CondenserType != EvapCooled && this->HeatRecActive ) {
				InterConnectTwoPlantLoopSides( this->CDLoopNum, this->CDLoopSideNum, this->HRLoopNum, this->HRLoopSideNum, TypeOf_Chiller_Electric, false );
			}

			if ( errFlag ) {
				ShowFatalError( "InitElectricChiller: Program terminated due to previous condition(s)." );
			}

			if ( this->FlowMode == ConstantFlow ) {
				// reset flow priority
				PlantLoop( this->CWLoopNum ).LoopSide( this->CWLoopSideNum ).Branch( this->CWBranchNum ).Comp( this->CWCompNum ).FlowPriority = LoopFlowStatus_NeedyIfLoopOn;
			}

			if ( this->FlowMode == LeavingSetPointModulated ) {
				// reset flow priority
				PlantLoop( this->CWLoopNum ).LoopSide( this->CWLoopSideNum ).Branch( this->CWBranchNum ).Comp( this->CWCompNum ).FlowPriority = LoopFlowStatus_NeedyIfLoopOn;

				// check if setpoint on outlet node
				if ( ( Node( this->EvapOutletNodeNum ).TempSetPoint == SensedNodeFlagValue ) && ( Node( this->EvapOutletNodeNum ).TempSetPointHi == SensedNodeFlagValue ) ) {
					if ( ! AnyEnergyManagementSystemInModel ) {
						if ( ! this->ModulatedFlowErrDone ) {
							ShowWarningError( "Missing temperature setpoint for LeavingSetpointModulated mode chiller named " + this->Name );
							ShowContinueError( "  A temperature setpoint is needed at the outlet node of a chiller in variable flow mode, use a SetpointManager" );
							ShowContinueError( "  The overall loop setpoint will be assumed for chiller. The simulation continues ... " );
							this->ModulatedFlowErrDone = true;
						}
					} else {
						// need call to EMS to check node
						FatalError = false; // but not really fatal yet, but should be.
						CheckIfNodeSetPointManagedByEMS( this->EvapOutletNodeNum, iTemperatureSetPoint, FatalError );
						if ( FatalError ) {
							if ( ! this->ModulatedFlowErrDone ) {
								ShowWarningError( "Missing temperature setpoint for LeavingSetpointModulated mode chiller named " + this->Name );
								ShowContinueError( "  A temperature setpoint is needed at the outlet node of a chiller evaporator in variable flow mode" );
								ShowContinueError( "  use a Setpoint Manager to establish a setpoint at the chiller evaporator outlet node " );
								ShowContinueError( "  or use an EMS actuator to establish a setpoint at the outlet node " );
								ShowContinueError( "  The overall loop setpoint will be assumed for chiller. The simulation continues ... " );
								this->ModulatedFlowErrDone = true;
							}
						}

					}
					this->ModulatedFlowSetToLoop = true;
					{ auto const SELECT_CASE_var( PlantLoop( this->CWLoopNum ).LoopDemandCalcScheme );
					if ( SELECT_CASE_var == SingleSetPoint ) {
						Node( this->EvapOutletNodeNum ).TempSetPoint = Node( PlantLoop( this->CWLoopNum ).TempSetPointNodeNum ).TempSetPoint;
					} else if ( SELECT_CASE_var == DualSetPointDeadBand ) {
						Node( this->EvapOutletNodeNum ).TempSetPointHi = Node( PlantLoop( this->CWLoopNum ).TempSetPointNodeNum ).TempSetPointHi;
					}}
				}
			}
			this->MyFlag = false;
		}

		if ( this->MyEnvrnFlag && BeginEnvrnFlag && ( PlantFirstSizesOkayToFinalize ) ) {

			rho = GetDensityGlycol( PlantLoop( this->CWLoopNum ).FluidName, InitConvTemp, PlantLoop( this->CWLoopNum ).FluidIndex, RoutineName );

			this->EvapMassFlowRateMax = rho * this->EvapVolFlowRate;
			InitComponentNodes( 0.0, this->EvapMassFlowRateMax, EvapInletNode, EvapOutletNode, this->CWLoopNum, this->CWLoopSideNum, this->CWBranchNum, this->CWCompNum );

			//init maximum available condenser flow rate
			if ( this->CondenserType == WaterCooled ) {

				Node( CondInletNode ).Temp = this->TempDesCondIn; //DSU? old behavior, still want?

				rho = GetDensityGlycol( PlantLoop( this->CDLoopNum ).FluidName, InitConvTemp, PlantLoop( this->CDLoopNum ).FluidIndex, RoutineName );

				this->CondMassFlowRateMax = rho * this->CondVolFlowRate;

				InitComponentNodes( 0.0, this->CondMassFlowRateMax, CondInletNode, CondOutletNode, this->CDLoopNum, this->CDLoopSideNum, this->CDBranchNum, this->CDCompNum );
			} else { // air or evap-air

				rho = PsyRhoAirFnPbTdbW( StdBaroPress, this->TempDesCondIn, 0.0, RoutineName );
				this->CondMassFlowRateMax = rho * this->CondVolFlowRate;

				Node( CondInletNode ).MassFlowRate = this->CondMassFlowRateMax;
				Node( CondOutletNode ).MassFlowRate = Node( CondInletNode ).MassFlowRate;
				Node( CondInletNode ).MassFlowRateMaxAvail = Node( CondInletNode ).MassFlowRate;
				Node( CondInletNode ).MassFlowRateMax = Node( CondInletNode ).MassFlowRate;
				Node( CondOutletNode ).MassFlowRateMax = Node( CondInletNode ).MassFlowRate;
				Node( CondInletNode ).MassFlowRateMinAvail = 0.0;
				Node( CondInletNode ).MassFlowRateMin = 0.0;
				Node( CondOutletNode ).MassFlowRateMinAvail = 0.0;
				Node( CondOutletNode ).MassFlowRateMin = 0.0;
			}

			if ( this->HeatRecActive ) {
				rho = GetDensityGlycol( PlantLoop( this->HRLoopNum ).FluidName, InitConvTemp, PlantLoop( this->HRLoopNum ).FluidIndex, RoutineName );
				this->DesignHeatRecMassFlowRate = rho * this->DesignHeatRecVolFlowRate;

				InitComponentNodes( 0.0, this->DesignHeatRecMassFlowRate, this->HeatRecInletNodeNum, this->HeatRecOutletNodeNum, this->HRLoopNum, this->HRLoopSideNum, this->HRBranchNum, this->HRCompNum );
				this->HeatRecMaxCapacityLimit = this->HeatRecCapacityFraction * ( this->NomCap + this->NomCap / this->COP );

				if ( this->HeatRecSetPointNodeNum > 0 ) {
					{ auto const SELECT_CASE_var( PlantLoop( this->HRLoopNum ).LoopDemandCalcScheme );
					if ( SELECT_CASE_var == SingleSetPoint ) {
						THeatRecSetPoint = Node( this->HeatRecSetPointNodeNum ).TempSetPoint;
					} else if ( SELECT_CASE_var == DualSetPointDeadBand ) {
						THeatRecSetPoint = Node( this->HeatRecSetPointNodeNum ).TempSetPointHi;
					}}
					if ( THeatRecSetPoint == SensedNodeFlagValue ) {
						if ( ! AnyEnergyManagementSystemInModel ) {
							if ( ! this->HRSPErrDone ) {
								ShowWarningError( "Missing heat recovery temperature setpoint for chiller named " + this->Name );
								ShowContinueError( "  A temperature setpoint is needed at the heat recovery leaving temperature setpoint node specified, use a SetpointManager" );
								ShowContinueError( "  The overall loop setpoint will be assumed for heat recovery. The simulation continues ..." );
								this->HeatRecSetPointNodeNum = PlantLoop( this->HRLoopNum ).TempSetPointNodeNum;
								this->HRSPErrDone = true;
							}
						} else {
							// need call to EMS to check node
							FatalError = false; // but not really fatal yet, but should be.
							CheckIfNodeSetPointManagedByEMS( this->EvapOutletNodeNum, iTemperatureSetPoint, FatalError );
							if ( FatalError ) {
								if ( ! this->HRSPErrDone ) {
									ShowWarningError( "Missing heat recovery temperature setpoint for chiller named " + this->Name );
									ShowContinueError( "  A temperature setpoint is needed at the heat recovery leaving temperature setpoint node specified, use a SetpointManager to establish a setpoint" );
									ShowContinueError( "  or use an EMS actuator to establish a setpoint at this node " );
									ShowContinueError( "  The overall loop setpoint will be assumed for heat recovery. The simulation continues ..." );
									this->HeatRecSetPointNodeNum = PlantLoop( this->HRLoopNum ).TempSetPointNodeNum;
									this->HRSPErrDone = true;
								}
							}
						} // IF (.NOT. AnyEnergyManagementSystemInModel) THEN
					} // IF(THeatRecSetpoint == SensedNodeFlagValue)THEN
				} // IF(ElectricChiller(ChillNum)%HeatRecSetpointNodeNum > 0)THEN
			} // IF (ElectricChiller(ChillNum)%HeatRecActive) THEN

			this->MyEnvrnFlag = false;
		}
		if ( ! BeginEnvrnFlag ) {
			this->MyEnvrnFlag = true;
		}

		if ( ( this->FlowMode == LeavingSetPointModulated ) && ( this->ModulatedFlowSetToLoop ) ) {
			// fix for clumsy old input that worked because loop setpoint was spread.
			//  could be removed with transition, testing , model change, period of being obsolete.
			{ auto const SELECT_CASE_var( PlantLoop( this->CWLoopNum ).LoopDemandCalcScheme );
			if ( SELECT_CASE_var == SingleSetPoint ) {
				Node( this->EvapOutletNodeNum ).TempSetPoint = Node( PlantLoop( this->CWLoopNum ).TempSetPointNodeNum ).TempSetPoint;
			} else if ( SELECT_CASE_var == DualSetPointDeadBand ) {
				Node( this->EvapOutletNodeNum ).TempSetPointHi = Node( PlantLoop( this->CWLoopNum ).TempSetPointNodeNum ).TempSetPointHi;
			}}

		}

		if ( ( MyLoad < 0.0 ) && RunFlag ) {
			// request full then take what can get
			mdot = this->EvapMassFlowRateMax;
			mdotCond = this->CondMassFlowRateMax;
		} else {
			mdot = 0.0;
			mdotCond = 0.0;
		}

		SetComponentFlowRate( mdot, EvapInletNode, EvapOutletNode, this->CWLoopNum, this->CWLoopSideNum, this->CWBranchNum, this->CWCompNum );
		if ( this->CondenserType == WaterCooled ) {
			SetComponentFlowRate( mdotCond, CondInletNode, CondOutletNode, this->CDLoopNum, this->CDLoopSideNum, this->CDBranchNum, this->CDCompNum );
		}

		// Initialize heat recovery flow rates at node
		if ( this->HeatRecActive ) {

			InletNode = this->HeatRecInletNodeNum;
			OutletNode = this->HeatRecOutletNodeNum;
			LoopNum = this->HRLoopNum;
			LoopSideNum = this->HRLoopSideNum;
			BranchIndex = this->HRBranchNum;
			CompIndex = this->HRCompNum;

			if ( RunFlag ) {
				mdot = this->DesignHeatRecMassFlowRate;
			} else {
				mdot = 0.0;
			}

			SetComponentFlowRate( mdot, InletNode, OutletNode, LoopNum, LoopSideNum, BranchIndex, CompIndex );

		}

		if ( this->CondenserType == EvapCooled ) {
			this->BasinHeaterPower = 0.0;
		}

	}

	void
	EngineDrivenChillerSpecs::InitEngineDrivenChiller(
		bool const RunFlag, // TRUE when chiller operating
		Real64 const MyLoad
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Fred Buhl
		//       DATE WRITTEN   June 2002
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine is for initializations of the Engine Driven Chiller components

		// METHODOLOGY EMPLOYED:
		// Uses the status flags to trigger initializations.

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataGlobals::BeginEnvrnFlag;
		using DataGlobals::AnyEnergyManagementSystemInModel;
		using DataPlant::PlantLoop;
		using DataPlant::TypeOf_Chiller_EngineDriven;
		using DataPlant::ScanPlantLoopsForObject;
		using DataPlant::PlantFirstSizesOkayToFinalize;
		using DataPlant::LoopFlowStatus_NeedyIfLoopOn;
		using DataEnvironment::StdBaroPress;
		using Psychrometrics::PsyRhoAirFnPbTdbW;
		using PlantUtilities::InterConnectTwoPlantLoopSides;
		using PlantUtilities::InitComponentNodes;
		using PlantUtilities::SetComponentFlowRate;
		using FluidProperties::GetDensityGlycol;
		using EMSManager::iTemperatureSetPoint;
		using EMSManager::CheckIfNodeSetPointManagedByEMS;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "InitEngineDrivenChiller" );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int CondInletNode; // node number of water inlet node to the condenser
		int CondOutletNode;
		int EvapInletNode;
		int EvapOutletNode;
		int HeatRecInNode;
		int HeatRecOutNode;
		Real64 rho; // local fluid density
		Real64 mdot; // local mass flow rate
		Real64 mdotCond; // local mass flow rate for condenser
		bool errFlag;
		int InletNode;
		int OutletNode;
		int LoopNum;
		int LoopSideNum;
		int BranchIndex;
		int CompIndex;
		bool FatalError;

		//Load inputs to local structure
		CondInletNode = this->CondInletNodeNum;
		CondOutletNode = this->CondOutletNodeNum;
		EvapInletNode = this->EvapInletNodeNum;
		EvapOutletNode = this->EvapOutletNodeNum;

		if ( this->HeatRecActive ) {
			HeatRecInNode = this->HeatRecInletNodeNum;
			HeatRecOutNode = this->HeatRecOutletNodeNum;
		}

		// Init more variables
		if ( this->MyFlag ) {
			// Locate the chillers on the plant loops for later usage
			errFlag = false;
			ScanPlantLoopsForObject( this->Name, TypeOf_Chiller_EngineDriven, this->CWLoopNum, this->CWLoopSideNum, this->CWBranchNum, this->CWCompNum, this->TempLowLimitEvapOut, _, _, this->EvapInletNodeNum, _, errFlag );
			if ( this->CondenserType != AirCooled && this->CondenserType != EvapCooled ) {
				ScanPlantLoopsForObject( this->Name, TypeOf_Chiller_EngineDriven, this->CDLoopNum, this->CDLoopSideNum, this->CDBranchNum, this->CDCompNum, _, _, _, this->CondInletNodeNum, _, errFlag );
				InterConnectTwoPlantLoopSides( this->CWLoopNum, this->CWLoopSideNum, this->CDLoopNum, this->CDLoopSideNum, TypeOf_Chiller_EngineDriven, true );
			}
			if ( this->HeatRecActive ) {
				ScanPlantLoopsForObject( this->Name, TypeOf_Chiller_EngineDriven, this->HRLoopNum, this->HRLoopSideNum, this->HRBranchNum, this->HRCompNum, _, _, _, this->HeatRecInletNodeNum, _, errFlag );
				InterConnectTwoPlantLoopSides( this->CWLoopNum, this->CWLoopSideNum, this->HRLoopNum, this->HRLoopSideNum, TypeOf_Chiller_EngineDriven, true );
			}
			if ( this->CondenserType != AirCooled && this->CondenserType != EvapCooled && this->HeatRecActive ) {
				InterConnectTwoPlantLoopSides( this->CDLoopNum, this->CDLoopSideNum, this->HRLoopNum, this->HRLoopSideNum, TypeOf_Chiller_EngineDriven, false );
			}
			if ( errFlag ) {
				ShowFatalError( "InitEngineDrivenChiller: Program terminated due to previous condition(s)." );
			}

			if ( this->FlowMode == ConstantFlow ) {
				// reset flow priority
				PlantLoop( this->CWLoopNum ).LoopSide( this->CWLoopSideNum ).Branch( this->CWBranchNum ).Comp( this->CWCompNum ).FlowPriority = LoopFlowStatus_NeedyIfLoopOn;
			}

			if ( this->FlowMode == LeavingSetPointModulated ) {
				// reset flow priority
				PlantLoop( this->CWLoopNum ).LoopSide( this->CWLoopSideNum ).Branch( this->CWBranchNum ).Comp( this->CWCompNum ).FlowPriority = LoopFlowStatus_NeedyIfLoopOn;
				// check if setpoint on outlet node
				if ( ( Node( this->EvapOutletNodeNum ).TempSetPoint == SensedNodeFlagValue ) && ( Node( this->EvapOutletNodeNum ).TempSetPointHi == SensedNodeFlagValue ) ) {
					if ( ! AnyEnergyManagementSystemInModel ) {
						if ( ! this->ModulatedFlowErrDone ) {
							ShowWarningError( "Missing temperature setpoint for LeavingSetpointModulated mode chiller named " + this->Name );
							ShowContinueError( "  A temperature setpoint is needed at the outlet node of a chiller in variable flow mode, use a SetpointManager" );
							ShowContinueError( "  The overall loop setpoint will be assumed for chiller. The simulation continues ... " );
							this->ModulatedFlowErrDone = true;
						}
					} else {
						// need call to EMS to check node
						FatalError = false; // but not really fatal yet, but should be.
						CheckIfNodeSetPointManagedByEMS( this->EvapOutletNodeNum, iTemperatureSetPoint, FatalError );
						if ( FatalError ) {
							if ( ! this->ModulatedFlowErrDone ) {
								ShowWarningError( "Missing temperature setpoint for LeavingSetpointModulated mode chiller named " + this->Name );
								ShowContinueError( "  A temperature setpoint is needed at the outlet node of a chiller evaporator in variable flow mode" );
								ShowContinueError( "  use a Setpoint Manager to establish a setpoint at the chiller evaporator outlet node " );
								ShowContinueError( "  or use an EMS actuator to establish a setpoint at the outlet node " );
								ShowContinueError( "  The overall loop setpoint will be assumed for chiller. The simulation continues ... " );
								this->ModulatedFlowErrDone = true;
							}
						}

					}
					this->ModulatedFlowSetToLoop = true;
					Node( this->EvapOutletNodeNum ).TempSetPoint = Node( PlantLoop( this->CWLoopNum ).TempSetPointNodeNum ).TempSetPoint;
					Node( this->EvapOutletNodeNum ).TempSetPointHi = Node( PlantLoop( this->CWLoopNum ).TempSetPointNodeNum ).TempSetPointHi;
				}
			}

			this->MyFlag = false;
		}

		//Initialize critical Demand Side Variables
		//  IF((MyEnvrnFlag(ChillNum) .and. BeginEnvrnFlag) &
		//     .OR. (Node(CondInletNode)%MassFlowrate <= 0.0 .AND. RunFlag)) THEN
		if ( this->MyEnvrnFlag && BeginEnvrnFlag && ( PlantFirstSizesOkayToFinalize ) ) {

			rho = GetDensityGlycol( PlantLoop( this->CWLoopNum ).FluidName, InitConvTemp, PlantLoop( this->CWLoopNum ).FluidIndex, RoutineName );

			this->EvapMassFlowRateMax = rho * this->EvapVolFlowRate;
			InitComponentNodes( 0.0, this->EvapMassFlowRateMax, EvapInletNode, EvapOutletNode, this->CWLoopNum, this->CWLoopSideNum, this->CWBranchNum, this->CWCompNum );

			//init maximum available condenser flow rate

			if ( this->CondenserType == WaterCooled ) {

				Node( CondInletNode ).Temp = this->TempDesCondIn;

				rho = GetDensityGlycol( PlantLoop( this->CDLoopNum ).FluidName, InitConvTemp, PlantLoop( this->CDLoopNum ).FluidIndex, RoutineName );

				this->CondMassFlowRateMax = rho * this->CondVolFlowRate;

				InitComponentNodes( 0.0, this->CondMassFlowRateMax, CondInletNode, CondOutletNode, this->CDLoopNum, this->CDLoopSideNum, this->CDBranchNum, this->CDCompNum );
			} else { // air or evap-air
				Node( CondInletNode ).MassFlowRate = this->CondVolFlowRate * PsyRhoAirFnPbTdbW( StdBaroPress, this->TempDesCondIn, 0.0, RoutineName );

				Node( CondOutletNode ).MassFlowRate = Node( CondInletNode ).MassFlowRate;
				Node( CondInletNode ).MassFlowRateMaxAvail = Node( CondInletNode ).MassFlowRate;
				Node( CondInletNode ).MassFlowRateMax = Node( CondInletNode ).MassFlowRate;
				Node( CondOutletNode ).MassFlowRateMax = Node( CondInletNode ).MassFlowRate;
				Node( CondInletNode ).MassFlowRateMinAvail = 0.0;
				Node( CondInletNode ).MassFlowRateMin = 0.0;
				Node( CondOutletNode ).MassFlowRateMinAvail = 0.0;
				Node( CondOutletNode ).MassFlowRateMin = 0.0;
			}

			if ( this->HeatRecActive ) {
				rho = GetDensityGlycol( PlantLoop( this->HRLoopNum ).FluidName, InitConvTemp, PlantLoop( this->HRLoopNum ).FluidIndex, RoutineName );
				this->DesignHeatRecMassFlowRate = rho * this->DesignHeatRecVolFlowRate;

				InitComponentNodes( 0.0, this->DesignHeatRecMassFlowRate, this->HeatRecInletNodeNum, this->HeatRecOutletNodeNum, this->HRLoopNum, this->HRLoopSideNum, this->HRBranchNum, this->HRCompNum );
			}

			this->MyEnvrnFlag = false;
		}

		if ( ! BeginEnvrnFlag ) {
			this->MyEnvrnFlag = true;
		}

		if ( ( this->FlowMode == LeavingSetPointModulated ) && ( this->ModulatedFlowSetToLoop ) ) {
			// fix for clumsy old input that worked because loop setpoint was spread.
			//  could be removed with transition, testing , model change, period of being obsolete.
			Node( this->EvapOutletNodeNum ).TempSetPoint = Node( PlantLoop( this->CWLoopNum ).TempSetPointNodeNum ).TempSetPoint;
			Node( this->EvapOutletNodeNum ).TempSetPointHi = Node( PlantLoop( this->CWLoopNum ).TempSetPointNodeNum ).TempSetPointHi;
		}

		if ( ( std::abs( MyLoad ) > 0.0 ) && RunFlag ) {
			mdot = this->EvapMassFlowRateMax;
			mdotCond = this->CondMassFlowRateMax;
		} else {
			mdot = 0.0;
			mdotCond = 0.0;
		}

		SetComponentFlowRate( mdot, EvapInletNode, EvapOutletNode, this->CWLoopNum, this->CWLoopSideNum, this->CWBranchNum, this->CWCompNum );
		if ( this->CondenserType == WaterCooled ) {
			SetComponentFlowRate( mdotCond, CondInletNode, CondOutletNode, this->CDLoopNum, this->CDLoopSideNum, this->CDBranchNum, this->CDCompNum );
		}

		// Initialize heat recovery flow rates at node
		if ( this->HeatRecActive ) {
			InletNode = this->HeatRecInletNodeNum;
			OutletNode = this->HeatRecOutletNodeNum;
			LoopNum = this->HRLoopNum;
			LoopSideNum = this->HRLoopSideNum;
			BranchIndex = this->HRBranchNum;
			CompIndex = this->HRCompNum;

			if ( RunFlag ) {
				mdot = this->DesignHeatRecMassFlowRate;
			} else {
				mdot = 0.0;
			}

			SetComponentFlowRate( mdot, InletNode, OutletNode, LoopNum, LoopSideNum, BranchIndex, CompIndex );

		}
		if ( this->CondenserType == EvapCooled ) {
			this->BasinHeaterPower = 0.0;
		}

	}

	void
	GTChillerSpecs::InitGTChiller(
		bool const RunFlag, // TRUE when chiller operating
		Real64 const MyLoad
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Fred Buhl
		//       DATE WRITTEN   November 2003
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine is for initializations of the Gas Turbine Chiller components

		// METHODOLOGY EMPLOYED:
		// Uses the status flags to trigger initializations.

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataGlobals::BeginEnvrnFlag;
		using DataGlobals::AnyEnergyManagementSystemInModel;
		using DataPlant::PlantLoop;
		using DataPlant::TypeOf_Chiller_CombTurbine;
		using DataPlant::ScanPlantLoopsForObject;
		using DataPlant::PlantFirstSizesOkayToFinalize;
		using DataPlant::LoopFlowStatus_NeedyIfLoopOn;
		using DataEnvironment::StdBaroPress;
		using Psychrometrics::PsyRhoAirFnPbTdbW;
		using PlantUtilities::InterConnectTwoPlantLoopSides;
		using PlantUtilities::InitComponentNodes;
		using PlantUtilities::SetComponentFlowRate;
		using FluidProperties::GetDensityGlycol;
		using EMSManager::iTemperatureSetPoint;
		using EMSManager::CheckIfNodeSetPointManagedByEMS;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "InitGTChiller" );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int CondInletNode; // node number of water inlet node to the condenser
		int CondOutletNode; // node number of water outlet node from the condenser
		int EvapInletNode;
		int EvapOutletNode;
		int HeatRecInNode;
		int HeatRecOutNode;
		Real64 rho; // local fluid density
		Real64 mdot; // local mass flow rate
		Real64 mdotCond; // local mass flow rate for condenser
		int InletNode;
		int OutletNode;
		int LoopNum;
		int LoopSideNum;
		int BranchIndex;
		int CompIndex;
		bool FatalError;
		bool errFlag;

		CondInletNode = this->CondInletNodeNum;
		CondOutletNode = this->CondOutletNodeNum;
		EvapInletNode = this->EvapInletNodeNum;
		EvapOutletNode = this->EvapOutletNodeNum;

		if ( this->HeatRecActive ) {
			HeatRecInNode = this->HeatRecInletNodeNum;
			HeatRecOutNode = this->HeatRecOutletNodeNum;
		}

		// Init more variables
		if ( this->MyFlag ) {
			// Locate the chillers on the plant loops for later usage
			errFlag = false;
			ScanPlantLoopsForObject( this->Name, TypeOf_Chiller_CombTurbine, this->CWLoopNum, this->CWLoopSideNum, this->CWBranchNum, this->CWCompNum, this->TempLowLimitEvapOut, _, _, this->EvapInletNodeNum, _, errFlag );
			if ( this->CondenserType != AirCooled && this->CondenserType != EvapCooled ) {
				ScanPlantLoopsForObject( this->Name, TypeOf_Chiller_CombTurbine, this->CDLoopNum, this->CDLoopSideNum, this->CDBranchNum, this->CDCompNum, _, _, _, this->CondInletNodeNum, _, errFlag );
				InterConnectTwoPlantLoopSides( this->CWLoopNum, this->CWLoopSideNum, this->CDLoopNum, this->CDLoopSideNum, TypeOf_Chiller_CombTurbine, true );
			}
			if ( this->HeatRecActive ) {
				ScanPlantLoopsForObject( this->Name, TypeOf_Chiller_CombTurbine, this->HRLoopNum, this->HRLoopSideNum, this->HRBranchNum, this->HRCompNum, _, _, _, this->HeatRecInletNodeNum, _, errFlag );
				InterConnectTwoPlantLoopSides( this->CWLoopNum, this->CWLoopSideNum, this->HRLoopNum, this->HRLoopSideNum, TypeOf_Chiller_CombTurbine, true );
			}

			if ( this->CondenserType != AirCooled && this->CondenserType != EvapCooled && this->HeatRecActive ) {
				InterConnectTwoPlantLoopSides( this->CDLoopNum, this->CDLoopSideNum, this->HRLoopNum, this->HRLoopSideNum, TypeOf_Chiller_CombTurbine, false );
			}
			if ( errFlag ) {
				ShowFatalError( "InitGTChiller: Program terminated due to previous condition(s)." );
			}

			if ( this->FlowMode == ConstantFlow ) {
				// reset flow priority
				PlantLoop( this->CWLoopNum ).LoopSide( this->CWLoopSideNum ).Branch( this->CWBranchNum ).Comp( this->CWCompNum ).FlowPriority = LoopFlowStatus_NeedyIfLoopOn;
			}

			if ( this->FlowMode == LeavingSetPointModulated ) {
				// reset flow priority
				PlantLoop( this->CWLoopNum ).LoopSide( this->CWLoopSideNum ).Branch( this->CWBranchNum ).Comp( this->CWCompNum ).FlowPriority = LoopFlowStatus_NeedyIfLoopOn;

				// check if setpoint on outlet node
				if ( ( Node( this->EvapOutletNodeNum ).TempSetPoint == SensedNodeFlagValue ) && ( Node( this->EvapOutletNodeNum ).TempSetPointHi == SensedNodeFlagValue ) ) {
					if ( ! AnyEnergyManagementSystemInModel ) {
						if ( ! this->ModulatedFlowErrDone ) {
							ShowWarningError( "Missing temperature setpoint for LeavingSetpointModulated mode chiller named " + this->Name );
							ShowContinueError( "  A temperature setpoint is needed at the outlet node of a chiller in variable flow mode, use a SetpointManager" );
							ShowContinueError( "  The overall loop setpoint will be assumed for chiller. The simulation continues ... " );
							this->ModulatedFlowErrDone = true;
						}
					} else {
						// need call to EMS to check node
						FatalError = false; // but not really fatal yet, but should be.
						CheckIfNodeSetPointManagedByEMS( this->EvapOutletNodeNum, iTemperatureSetPoint, FatalError );
						if ( FatalError ) {
							if ( ! this->ModulatedFlowErrDone ) {
								ShowWarningError( "Missing temperature setpoint for LeavingSetpointModulated mode chiller named " + this->Name );
								ShowContinueError( "  A temperature setpoint is needed at the outlet node of a chiller evaporator in variable flow mode" );
								ShowContinueError( "  use a Setpoint Manager to establish a setpoint at the chiller evaporator outlet node " );
								ShowContinueError( "  or use an EMS actuator to establish a setpoint at the outlet node " );
								ShowContinueError( "  The overall loop setpoint will be assumed for chiller. The simulation continues ... " );
								this->ModulatedFlowErrDone = true;
							}
						}

					}
					this->ModulatedFlowSetToLoop = true;
					Node( this->EvapOutletNodeNum ).TempSetPoint = Node( PlantLoop( this->CWLoopNum ).TempSetPointNodeNum ).TempSetPoint;
					Node( this->EvapOutletNodeNum ).TempSetPointHi = Node( PlantLoop( this->CWLoopNum ).TempSetPointNodeNum ).TempSetPointHi;
				}
			}
			this->MyFlag = false;
		}

		if ( this->MyEnvrnFlag && BeginEnvrnFlag && ( PlantFirstSizesOkayToFinalize ) ) {

			rho = GetDensityGlycol( PlantLoop( this->CWLoopNum ).FluidName, InitConvTemp, PlantLoop( this->CWLoopNum ).FluidIndex, RoutineName );

			this->EvapMassFlowRateMax = rho * this->EvapVolFlowRate;
			InitComponentNodes( 0.0, this->EvapMassFlowRateMax, EvapInletNode, EvapOutletNode, this->CWLoopNum, this->CWLoopSideNum, this->CWBranchNum, this->CWCompNum );

			//init maximum available condenser flow rate
			if ( this->CondenserType == WaterCooled ) {

				Node( CondInletNode ).Temp = this->TempDesCondIn;

				rho = GetDensityGlycol( PlantLoop( this->CDLoopNum ).FluidName, InitConvTemp, PlantLoop( this->CDLoopNum ).FluidIndex, RoutineName );

				this->CondMassFlowRateMax = rho * this->CondVolFlowRate;

				InitComponentNodes( 0.0, this->CondMassFlowRateMax, CondInletNode, CondOutletNode, this->CDLoopNum, this->CDLoopSideNum, this->CDBranchNum, this->CDCompNum );
			} else { // air or evap-air
				Node( CondInletNode ).MassFlowRate = this->CondVolFlowRate * PsyRhoAirFnPbTdbW( StdBaroPress, this->TempDesCondIn, 0.0, RoutineName );

				Node( CondOutletNode ).MassFlowRate = Node( CondInletNode ).MassFlowRate;
				Node( CondInletNode ).MassFlowRateMaxAvail = Node( CondInletNode ).MassFlowRate;
				Node( CondInletNode ).MassFlowRateMax = Node( CondInletNode ).MassFlowRate;
				Node( CondOutletNode ).MassFlowRateMax = Node( CondInletNode ).MassFlowRate;
				Node( CondInletNode ).MassFlowRateMinAvail = 0.0;
				Node( CondInletNode ).MassFlowRateMin = 0.0;
				Node( CondOutletNode ).MassFlowRateMinAvail = 0.0;
				Node( CondOutletNode ).MassFlowRateMin = 0.0;
			}

			if ( this->HeatRecActive ) {
				rho = GetDensityGlycol( PlantLoop( this->HRLoopNum ).FluidName, InitConvTemp, PlantLoop( this->HRLoopNum ).FluidIndex, RoutineName );
				this->DesignHeatRecMassFlowRate = rho * this->DesignHeatRecVolFlowRate;

				InitComponentNodes( 0.0, this->DesignHeatRecMassFlowRate, this->HeatRecInletNodeNum, this->HeatRecOutletNodeNum, this->HRLoopNum, this->HRLoopSideNum, this->HRBranchNum, this->HRCompNum );

			}

			this->MyEnvrnFlag = false;
		}

		if ( ! BeginEnvrnFlag ) {
			this->MyEnvrnFlag = true;
		}

		if ( ( this->FlowMode == LeavingSetPointModulated ) && ( this->ModulatedFlowSetToLoop ) ) {
			// fix for clumsy old input that worked because loop setpoint was spread.
			//  could be removed with transition, testing , model change, period of being obsolete.
			Node( this->EvapOutletNodeNum ).TempSetPoint = Node( PlantLoop( this->CWLoopNum ).TempSetPointNodeNum ).TempSetPoint;
			Node( this->EvapOutletNodeNum ).TempSetPointHi = Node( PlantLoop( this->CWLoopNum ).TempSetPointNodeNum ).TempSetPointHi;
		}

		if ( ( std::abs( MyLoad ) > 0.0 ) && RunFlag ) {
			mdot = this->EvapMassFlowRateMax;
			mdotCond = this->CondMassFlowRateMax;
		} else {
			mdot = 0.0;
			mdotCond = 0.0;
		}

		SetComponentFlowRate( mdot, EvapInletNode, EvapOutletNode, this->CWLoopNum, this->CWLoopSideNum, this->CWBranchNum, this->CWCompNum );
		if ( this->CondenserType == WaterCooled ) {
			SetComponentFlowRate( mdotCond, CondInletNode, CondOutletNode, this->CDLoopNum, this->CDLoopSideNum, this->CDBranchNum, this->CDCompNum );
		}

		// Initialize heat recovery flow rates at node
		if ( this->HeatRecActive ) {

			InletNode = this->HeatRecInletNodeNum;
			OutletNode = this->HeatRecOutletNodeNum;
			LoopNum = this->HRLoopNum;
			LoopSideNum = this->HRLoopSideNum;
			BranchIndex = this->HRBranchNum;
			CompIndex = this->HRCompNum;

			if ( RunFlag ) {
				mdot = this->DesignHeatRecMassFlowRate;
			} else {
				mdot = 0.0;
			}

			SetComponentFlowRate( mdot, InletNode, OutletNode, LoopNum, LoopSideNum, BranchIndex, CompIndex );

		}
		if ( this->CondenserType == EvapCooled ) {
			this->BasinHeaterPower = 0.0;
		}

	}

	void
	ConstCOPChillerSpecs::InitConstCOPChiller(
		bool const RunFlag, // TRUE when chiller operating
		Real64 const MyLoad
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Chandan Sharma
		//       DATE WRITTEN   September 2010
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine is for initializations of the Electric Chiller components

		// METHODOLOGY EMPLOYED:
		// Uses the status flags to trigger initializations.

		// REFERENCES:
		// Based on InitElectricChiller from Fred Buhl

		// Using/Aliasing
		using DataGlobals::BeginEnvrnFlag;
		using DataGlobals::AnyEnergyManagementSystemInModel;
		using DataPlant::PlantLoop;
		using DataPlant::ScanPlantLoopsForObject;
		using DataPlant::PlantFirstSizesOkayToFinalize;
		using DataPlant::LoopFlowStatus_NeedyIfLoopOn;
		using DataEnvironment::StdBaroPress;
		using Psychrometrics::PsyRhoAirFnPbTdbW;
		using PlantUtilities::InterConnectTwoPlantLoopSides;
		using PlantUtilities::InitComponentNodes;
		using PlantUtilities::SetComponentFlowRate;
		using FluidProperties::GetDensityGlycol;
		using EMSManager::iTemperatureSetPoint;
		using EMSManager::CheckIfNodeSetPointManagedByEMS;
		// na

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "InitConstCOPChiller" );
		Real64 const TempDesCondIn( 25.0 ); // Design condenser inlet temp. C

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int CondInletNode; // node number of water inlet node to the condenser
		int CondOutletNode; // node number of water outlet node from the condenser
		int EvapInletNode;
		int EvapOutletNode;
		Real64 rho; // local fluid density
		Real64 mdot; // local mass flow rate
		Real64 mdotCond; // local mass flow rate for condenser
		bool FatalError;
		bool errFlag;

		EvapInletNode = this->EvapInletNodeNum;
		EvapOutletNode = this->EvapOutletNodeNum;
		CondInletNode = this->CondInletNodeNum;
		CondOutletNode = this->CondOutletNodeNum;

		// Init more variables
		if ( this->MyFlag ) {
			// Locate the chillers on the plant loops for later usage
			errFlag = false;
			ScanPlantLoopsForObject( this->Name, TypeOf_Chiller_ConstCOP, this->CWLoopNum, this->CWLoopSideNum, this->CWBranchNum, this->CWCompNum, _, _, _, this->EvapInletNodeNum, _, errFlag );
			if ( this->CondenserType != AirCooled && this->CondenserType != EvapCooled ) {
				ScanPlantLoopsForObject( this->Name, TypeOf_Chiller_ConstCOP, this->CDLoopNum, this->CDLoopSideNum, this->CDBranchNum, this->CDCompNum, _, _, _, this->CondInletNodeNum, _, errFlag );
				InterConnectTwoPlantLoopSides( this->CWLoopNum, this->CWLoopSideNum, this->CDLoopNum, this->CDLoopSideNum, TypeOf_Chiller_ConstCOP, true );
			}

			if ( errFlag ) {
				ShowFatalError( "CalcConstCOPChillerModel: Program terminated due to previous condition(s)." );
			}
			if ( this->FlowMode == ConstantFlow ) {
				// reset flow priority
				PlantLoop( this->CWLoopNum ).LoopSide( this->CWLoopSideNum ).Branch( this->CWBranchNum ).Comp( this->CWCompNum ).FlowPriority = LoopFlowStatus_NeedyIfLoopOn;
			}

			if ( this->FlowMode == LeavingSetPointModulated ) {
				// reset flow priority
				PlantLoop( this->CWLoopNum ).LoopSide( this->CWLoopSideNum ).Branch( this->CWBranchNum ).Comp( this->CWCompNum ).FlowPriority = LoopFlowStatus_NeedyIfLoopOn;

				// check if setpoint on outlet node
				if ( ( Node( this->EvapOutletNodeNum ).TempSetPoint == SensedNodeFlagValue ) && ( Node( this->EvapOutletNodeNum ).TempSetPointHi == SensedNodeFlagValue ) ) {
					if ( ! AnyEnergyManagementSystemInModel ) {
						if ( ! this->ModulatedFlowErrDone ) {
							ShowWarningError( "Missing temperature setpoint for LeavingSetpointModulated mode chiller named " + this->Name );
							ShowContinueError( "  A temperature setpoint is needed at the outlet node of a chiller in variable flow mode, use a SetpointManager" );
							ShowContinueError( "  The overall loop setpoint will be assumed for chiller. The simulation continues ... " );
							this->ModulatedFlowErrDone = true;
						}
					} else {
						// need call to EMS to check node
						FatalError = false; // but not really fatal yet, but should be.
						CheckIfNodeSetPointManagedByEMS( this->EvapOutletNodeNum, iTemperatureSetPoint, FatalError );
						if ( FatalError ) {
							if ( ! this->ModulatedFlowErrDone ) {
								ShowWarningError( "Missing temperature setpoint for LeavingSetpointModulated mode chiller named " + this->Name );
								ShowContinueError( "  A temperature setpoint is needed at the outlet node of a chiller evaporator in variable flow mode" );
								ShowContinueError( "  use a Setpoint Manager to establish a setpoint at the chiller evaporator outlet node " );
								ShowContinueError( "  or use an EMS actuator to establish a setpoint at the outlet node " );
								ShowContinueError( "  The overall loop setpoint will be assumed for chiller. The simulation continues ... " );
								this->ModulatedFlowErrDone = true;
							}
						}
					}
					this->ModulatedFlowSetToLoop = true;
					Node( this->EvapOutletNodeNum ).TempSetPoint = Node( PlantLoop( this->CWLoopNum ).TempSetPointNodeNum ).TempSetPoint;
					Node( this->EvapOutletNodeNum ).TempSetPointHi = Node( PlantLoop( this->CWLoopNum ).TempSetPointNodeNum ).TempSetPointHi;
				}
			}
			this->MyFlag = false;
		}

		//Initialize critical Demand Side Variables at the beginning of each environment
		if ( this->MyEnvrnFlag && BeginEnvrnFlag && ( PlantFirstSizesOkayToFinalize ) ) {

			rho = GetDensityGlycol( PlantLoop( this->CWLoopNum ).FluidName, InitConvTemp, PlantLoop( this->CWLoopNum ).FluidIndex, RoutineName );
			this->EvapMassFlowRateMax = this->EvapVolFlowRate * rho;
			InitComponentNodes( 0.0, this->EvapMassFlowRateMax, EvapInletNode, EvapOutletNode, this->CWLoopNum, this->CWLoopSideNum, this->CWBranchNum, this->CWCompNum );

			//init maximum available condenser flow rate
			if ( this->CondenserType == WaterCooled ) {

				Node( CondInletNode ).Temp = TempDesCondIn;

				rho = GetDensityGlycol( PlantLoop( this->CDLoopNum ).FluidName, InitConvTemp, PlantLoop( this->CDLoopNum ).FluidIndex, RoutineName );

				this->CondMassFlowRateMax = rho * this->CondVolFlowRate;

				InitComponentNodes( 0.0, this->CondMassFlowRateMax, CondInletNode, CondOutletNode, this->CDLoopNum, this->CDLoopSideNum, this->CDBranchNum, this->CDCompNum );
			} else { // air or evap-air
				Node( CondInletNode ).MassFlowRate = this->CondVolFlowRate * PsyRhoAirFnPbTdbW( StdBaroPress, TempDesCondIn, 0.0, RoutineName );

				Node( CondOutletNode ).MassFlowRate = Node( CondInletNode ).MassFlowRate;
				Node( CondInletNode ).MassFlowRateMaxAvail = Node( CondInletNode ).MassFlowRate;
				Node( CondInletNode ).MassFlowRateMax = Node( CondInletNode ).MassFlowRate;
				Node( CondOutletNode ).MassFlowRateMax = Node( CondInletNode ).MassFlowRate;
				Node( CondInletNode ).MassFlowRateMinAvail = 0.0;
				Node( CondInletNode ).MassFlowRateMin = 0.0;
				Node( CondOutletNode ).MassFlowRateMinAvail = 0.0;
				Node( CondOutletNode ).MassFlowRateMin = 0.0;
			}
			this->MyEnvrnFlag = false;
		}

		if ( ! BeginEnvrnFlag ) {
			this->MyEnvrnFlag = true;
		}
		if ( ( this->FlowMode == LeavingSetPointModulated ) && ( this->ModulatedFlowSetToLoop ) ) {
			// fix for clumsy old input that worked because loop setpoint was spread.
			//  could be removed with transition, testing , model change, period of being obsolete.
			Node( this->EvapOutletNodeNum ).TempSetPoint = Node( PlantLoop( this->CWLoopNum ).TempSetPointNodeNum ).TempSetPoint;
			Node( this->EvapOutletNodeNum ).TempSetPointHi = Node( PlantLoop( this->CWLoopNum ).TempSetPointNodeNum ).TempSetPointHi;
		}

		if ( ( MyLoad < 0.0 ) && RunFlag ) {
			mdot = this->EvapMassFlowRateMax;
			mdotCond = this->CondMassFlowRateMax;
		} else {
			mdot = 0.0;
			mdotCond = 0.0;
		}

		SetComponentFlowRate( mdot, EvapInletNode, EvapOutletNode, this->CWLoopNum, this->CWLoopSideNum, this->CWBranchNum, this->CWCompNum );
		if ( this->CondenserType == WaterCooled ) {
			SetComponentFlowRate( mdotCond, CondInletNode, CondOutletNode, this->CDLoopNum, this->CDLoopSideNum, this->CDBranchNum, this->CDCompNum );
		}

		if ( this->CondenserType == EvapCooled ) {
			this->BasinHeaterPower = 0.0;
		}

	}

	void
	ElectricChillerSpecs::SizeElectricChiller()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Fred Buhl
		//       DATE WRITTEN   April 2002
		//       MODIFIED       November 2013 Daeho Kang, add component sizing table entries
		//       RE-ENGINEERED  B. Griffith, April 2011, allow repeated sizing calls, finish when ready to do so

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine is for sizing Electric Chiller Components for which capacities and flow rates
		// have not been specified in the input.

		// METHODOLOGY EMPLOYED:
		// Obtains evaporator flow rate from the plant sizing array. Calculates nominal capacity from
		// the evaporator flow rate and the chilled water loop design delta T. The condenser flow rate
		// is calculated from the nominal capacity, the COP, and the condenser loop design delta T.

		// REFERENCES:
		// na

		// Using/Aliasing
		using namespace DataSizing;
		using DataPlant::PlantLoop;
		using DataPlant::PlantFirstSizesOkayToFinalize;
		using DataPlant::PlantFirstSizesOkayToReport;
		using DataPlant::PlantFinalSizesOkayToReport;
		using PlantUtilities::RegisterPlantCompDesignFlow;
		using ReportSizingManager::ReportSizingOutput;
		using namespace OutputReportPredefined;

		using FluidProperties::GetDensityGlycol;
		using FluidProperties::GetSpecificHeatGlycol;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "SizeElectricChiller" );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		//unused1208  INTEGER             :: PltSizIndex   ! Plant Sizing Do loop index
		int PltSizNum( 0 ); // Plant Sizing index corresponding to CurLoopNum
		int PltSizCondNum( 0 ); // Plant Sizing index for condenser loop
		bool ErrorsFound( false ); // If errors detected in input
		std::string equipName;
		Real64 rho; // local fluid density
		Real64 Cp; // local fluid specific heat
		Real64 tmpNomCap; // local nominal capacity cooling power
		Real64 tmpEvapVolFlowRate; // local evaporator design volume flow rate
		Real64 tmpCondVolFlowRate; // local condenser design volume flow rate
		Real64 tmpHeatRecVolFlowRate( 0.0 ); // local heat recovery design volume flow rate
		Real64 EvapVolFlowRateUser( 0.0 ); // Hardsized evaporator flow rate for reporting
		Real64 NomCapUser( 0.0 ); // Hardsized reference capacity for reporting
		Real64 CondVolFlowRateUser( 0.0 ); // Hardsized condenser flow rate for reporting
		Real64 DesignHeatRecVolFlowRateUser( 0.0 ); // Hardsized heat recovery flow rate for reporting

		// init local temporary version in case of partial/mixed autosizing
		tmpEvapVolFlowRate = this->EvapVolFlowRate;
		tmpNomCap = this->NomCap;
		tmpCondVolFlowRate = this->CondVolFlowRate;

		if ( this->CondenserType == WaterCooled ) {
			PltSizCondNum = PlantLoop( this->CDLoopNum ).PlantSizNum;
		}

		PltSizNum = PlantLoop( this->CWLoopNum ).PlantSizNum;

		if ( PltSizNum > 0 ) {
			if ( PlantSizData( PltSizNum ).DesVolFlowRate >= SmallWaterVolFlow ) {
				rho = GetDensityGlycol( PlantLoop( this->CWLoopNum ).FluidName, InitConvTemp, PlantLoop( this->CWLoopNum ).FluidIndex, RoutineName );
				Cp = GetSpecificHeatGlycol( PlantLoop( this->CWLoopNum ).FluidName, InitConvTemp, PlantLoop( this->CWLoopNum ).FluidIndex, RoutineName );
				tmpNomCap = Cp * rho * PlantSizData( PltSizNum ).DeltaT * PlantSizData( PltSizNum ).DesVolFlowRate * this->SizFac;
				if ( ! this->NomCapWasAutoSized ) tmpNomCap = this->NomCap;
			} else {
				if ( this->NomCapWasAutoSized ) tmpNomCap = 0.0;
			}
			if ( PlantFirstSizesOkayToFinalize ) {
				if ( this->NomCapWasAutoSized ) {
					this->NomCap = tmpNomCap;
					if ( PlantFinalSizesOkayToReport ) {
						ReportSizingOutput( "Chiller:Electric", this->Name,
							"Design Size Nominal Capacity [W]", tmpNomCap );
					}
					if ( PlantFirstSizesOkayToReport ) {
						ReportSizingOutput( "Chiller:Electric", this->Name,
							"Initial Design Size Nominal Capacity [W]", tmpNomCap );
					}
				} else {
					if ( this->NomCap > 0.0 && tmpNomCap > 0.0 ) {
						NomCapUser = this->NomCap;
						if ( PlantFinalSizesOkayToReport ) {
							ReportSizingOutput( "Chiller:Electric", this->Name, "Design Size Nominal Capacity [W]", tmpNomCap, "User-Specified Nominal Capacity [W]", NomCapUser );
							if ( DisplayExtraWarnings ) {
								if ( ( std::abs( tmpNomCap - NomCapUser ) / NomCapUser ) > AutoVsHardSizingThreshold ) {
									ShowMessage( "SizeChillerElectric: Potential issue with equipment sizing for " + this->Name );
									ShowContinueError( "User-Specified Nominal Capacity of " + RoundSigDigits( NomCapUser, 2 ) + " [W]" );
									ShowContinueError( "differs from Design Size Nominal Capacity of " + RoundSigDigits( tmpNomCap, 2 ) + " [W]" );
									ShowContinueError( "This may, or may not, indicate mismatched component sizes." );
									ShowContinueError( "Verify that the value entered is intended and is consistent with other components." );
								}
							}
						}
						tmpNomCap = NomCapUser;
					}
				}
			}
		} else {
			if ( this->NomCapWasAutoSized && PlantFirstSizesOkayToFinalize ) {
				ShowSevereError( "Autosizing of Electric Chiller nominal capacity requires a loop Sizing:Plant object" );
				ShowContinueError( "Occurs in Electric Chiller object=" + this->Name );
				ErrorsFound = true;
			}
			if ( ! this->NomCapWasAutoSized && PlantFinalSizesOkayToReport
					&& ( this->NomCap > 0.0 ) ) {
					ReportSizingOutput( "Chiller:Electric", this->Name,
						"User-Specified Nominal Capacity [W]", this->NomCap );
			}
		}

		if ( PltSizNum > 0 ) {
			if ( PlantSizData( PltSizNum ).DesVolFlowRate >= SmallWaterVolFlow ) {
				tmpEvapVolFlowRate = PlantSizData( PltSizNum ).DesVolFlowRate * this->SizFac;
				if ( !  this->EvapVolFlowRateWasAutoSized ) tmpEvapVolFlowRate = this->EvapVolFlowRate;
			} else {
				if ( this->EvapVolFlowRateWasAutoSized ) tmpEvapVolFlowRate = 0.0;
			}
			if ( PlantFirstSizesOkayToFinalize ) {
				if ( this->EvapVolFlowRateWasAutoSized ) {
					this->EvapVolFlowRate = tmpEvapVolFlowRate;
					if ( PlantFinalSizesOkayToReport) {
						ReportSizingOutput( "Chiller:Electric", this->Name,
							"Design Size Design Chilled Water Flow Rate [m3/s]", tmpEvapVolFlowRate );
					}
					if ( PlantFirstSizesOkayToReport) {
						ReportSizingOutput( "Chiller:Electric", this->Name,
							"Initial Design Size Design Chilled Water Flow Rate [m3/s]", tmpEvapVolFlowRate );
					}
				} else {
					if ( this->EvapVolFlowRate > 0.0 && tmpEvapVolFlowRate > 0.0 ) {
						EvapVolFlowRateUser = this->EvapVolFlowRate;
						if ( PlantFinalSizesOkayToReport ) {
							ReportSizingOutput( "Chiller:Electric", this->Name,
								"Design Size Design Chilled Water Flow Rate [m3/s]", tmpEvapVolFlowRate,
								"User-Specified Design Chilled Water Flow Rate [m3/s]", EvapVolFlowRateUser );
							if ( DisplayExtraWarnings ) {
								if ( ( std::abs( tmpEvapVolFlowRate - EvapVolFlowRateUser ) / EvapVolFlowRateUser ) > AutoVsHardSizingThreshold ) {
									ShowMessage( "SizeChillerElectric: Potential issue with equipment sizing for " + this->Name );
									ShowContinueError( "User-Specified Design Chilled Water Flow Rate of " + RoundSigDigits( EvapVolFlowRateUser, 5 ) + " [m3/s]" );
									ShowContinueError( "differs from Design Size Design Chilled Water Flow Rate of " + RoundSigDigits( tmpEvapVolFlowRate, 5 ) + " [m3/s]" );
									ShowContinueError( "This may, or may not, indicate mismatched component sizes." );
									ShowContinueError( "Verify that the value entered is intended and is consistent with other components." );
								}
							}
						}
						tmpEvapVolFlowRate = EvapVolFlowRateUser;
					}
				}
			}
		} else {
			if ( this->EvapVolFlowRateWasAutoSized && PlantFirstSizesOkayToFinalize) {
				ShowSevereError( "Autosizing of Electric Chiller evap flow rate requires a loop Sizing:Plant object" );
				ShowContinueError( "Occurs in Electric Chiller object=" + this->Name );
				ErrorsFound = true;
			}
			if ( ! this->EvapVolFlowRateWasAutoSized && PlantFinalSizesOkayToReport
					&& ( this->EvapVolFlowRate > 0.0 ) ) {
					ReportSizingOutput( "Chiller:Electric", this->Name,
						"User-Specified Design Chilled Water Flow Rate [m3/s]", this->EvapVolFlowRate );
			}
		}

		RegisterPlantCompDesignFlow( this->EvapInletNodeNum, tmpEvapVolFlowRate );

		if ( PltSizCondNum > 0 && PltSizNum > 0 ) {
			if ( PlantSizData( PltSizNum ).DesVolFlowRate >= SmallWaterVolFlow && tmpNomCap > 0.0 ) {
				rho = GetDensityGlycol( PlantLoop( this->CDLoopNum ).FluidName, this->TempDesCondIn, PlantLoop( this->CDLoopNum ).FluidIndex, RoutineName );

				Cp = GetSpecificHeatGlycol( PlantLoop( this->CDLoopNum ).FluidName, this->TempDesCondIn, PlantLoop( this->CDLoopNum ).FluidIndex, RoutineName );
				tmpCondVolFlowRate = tmpNomCap * ( 1.0 + 1.0 / this->COP ) / ( PlantSizData( PltSizCondNum ).DeltaT * Cp * rho );
				if ( ! this->CondVolFlowRateWasAutoSized ) tmpCondVolFlowRate = this->CondVolFlowRate;
			} else {
				if ( this->CondVolFlowRateWasAutoSized ) tmpCondVolFlowRate = 0.0;
			}
			if ( PlantFirstSizesOkayToFinalize ) {
				if ( this->CondVolFlowRateWasAutoSized ) {
					this->CondVolFlowRate = tmpCondVolFlowRate;
					if (PlantFinalSizesOkayToReport) {
						ReportSizingOutput( "Chiller:Electric", this->Name,
							"Design Size Design Condenser Water Flow Rate [m3/s]", tmpCondVolFlowRate );
					}
					if ( PlantFirstSizesOkayToReport ) {
						ReportSizingOutput( "Chiller:Electric", this->Name,
							"Initial Design Size Design Condenser Water Flow Rate [m3/s]", tmpCondVolFlowRate );
					}
				} else {
					if ( this->CondVolFlowRate > 0.0 && tmpCondVolFlowRate > 0.0 ) {
						CondVolFlowRateUser = this->CondVolFlowRate;
						if ( PlantFinalSizesOkayToReport ) {
							ReportSizingOutput( "Chiller:Electric", this->Name, "Design Size Design Condenser Water Flow Rate [m3/s]", tmpCondVolFlowRate, "User-Specified Design Condenser Water Flow Rate [m3/s]", CondVolFlowRateUser );
							if ( DisplayExtraWarnings ) {
								if ( ( std::abs( tmpCondVolFlowRate - CondVolFlowRateUser ) / CondVolFlowRateUser ) > AutoVsHardSizingThreshold ) {
									ShowMessage( "SizeChillerElectric: Potential issue with equipment sizing for " + this->Name );
									ShowContinueError( "User-Specified Design Condenser Water Flow Rate of " + RoundSigDigits( CondVolFlowRateUser, 5 ) + " [m3/s]" );
									ShowContinueError( "differs from Design Size Design Condenser Water Flow Rate of " + RoundSigDigits( tmpCondVolFlowRate, 5 ) + " [m3/s]" );
									ShowContinueError( "This may, or may not, indicate mismatched component sizes." );
									ShowContinueError( "Verify that the value entered is intended and is consistent with other components." );
								}
							}
						}
						tmpCondVolFlowRate = CondVolFlowRateUser;
					}
				}
			}
		} else {
			if ( this->CondVolFlowRateWasAutoSized && PlantFirstSizesOkayToFinalize ) {
				ShowSevereError( "Autosizing of Electric Chiller condenser flow rate requires a condenser" );
				ShowContinueError( "loop Sizing:Plant object" );
				ShowContinueError( "Occurs in Electric Chiller object=" + this->Name );
				ErrorsFound = true;
			}
			if ( ! this->CondVolFlowRateWasAutoSized && PlantFinalSizesOkayToReport
					&& ( this->CondVolFlowRate > 0.0 ) ) {
					ReportSizingOutput( "Chiller:Electric", this->Name,
						"User-Specified Design Condenser Water Flow Rate [m3/s]", this->CondVolFlowRate );
			}
		}

		// save the design condenser water volumetric flow rate for use by the condenser water loop sizing algorithms
		if ( this->CondenserType == WaterCooled ) {
			RegisterPlantCompDesignFlow( this->CondInletNodeNum, tmpCondVolFlowRate );
		}
		if ( ErrorsFound ) {
			ShowFatalError( "Preceding sizing errors cause program termination" );
		}

		if ( this->HeatRecActive ) {
			tmpHeatRecVolFlowRate = this->CondVolFlowRate * this->HeatRecCapacityFraction;
			if ( ! this->DesignHeatRecVolFlowRateWasAutoSized ) tmpHeatRecVolFlowRate = this->DesignHeatRecVolFlowRate;
			if ( PlantFirstSizesOkayToFinalize ) {
				if ( this->DesignHeatRecVolFlowRateWasAutoSized ) {
					this->DesignHeatRecVolFlowRate = tmpHeatRecVolFlowRate;
					if ( PlantFinalSizesOkayToReport ) {
						ReportSizingOutput( "Chiller:Electric", this->Name,
							"Design Size Design Heat Recovery Fluid Flow Rate [m3/s]", tmpHeatRecVolFlowRate );
					}
					if ( PlantFirstSizesOkayToReport ) {
						ReportSizingOutput( "Chiller:Electric", this->Name,
							"Initial Design Size Design Heat Recovery Fluid Flow Rate [m3/s]", tmpHeatRecVolFlowRate );
					}
				} else {
					if ( this->DesignHeatRecVolFlowRate > 0.0 && tmpHeatRecVolFlowRate > 0.0 ) {
						DesignHeatRecVolFlowRateUser = this->DesignHeatRecVolFlowRate;
						if ( PlantFinalSizesOkayToReport ) {
							ReportSizingOutput( "Chiller:Electric", this->Name,
								"Design Size Design Heat Recovery Fluid Flow Rate [m3/s]", tmpHeatRecVolFlowRate,
								"User-Specified Design Heat Recovery Fluid Flow Rate [m3/s]", DesignHeatRecVolFlowRateUser );
							if ( DisplayExtraWarnings ) {
								if ( ( std::abs( tmpHeatRecVolFlowRate - DesignHeatRecVolFlowRateUser ) / DesignHeatRecVolFlowRateUser ) > AutoVsHardSizingThreshold ) {
									ShowMessage( "SizeChillerElectric: Potential issue with equipment sizing for " + this->Name );
									ShowContinueError( "User-Specified Design Heat Recovery Fluid Flow Rate of " + RoundSigDigits( DesignHeatRecVolFlowRateUser, 5 ) + " [m3/s]" );
									ShowContinueError( "differs from Design Size Design Heat Recovery Fluid Flow Rate of " + RoundSigDigits( tmpHeatRecVolFlowRate, 5 ) + " [m3/s]" );
									ShowContinueError( "This may, or may not, indicate mismatched component sizes." );
									ShowContinueError( "Verify that the value entered is intended and is consistent with other components." );
								}
							}
						}
						tmpHeatRecVolFlowRate = DesignHeatRecVolFlowRateUser;
					}
				}
			}
			// save the reference heat recovery fluid volumetric flow rate
			RegisterPlantCompDesignFlow( this->HeatRecInletNodeNum, tmpHeatRecVolFlowRate );
		}

		if ( PlantFinalSizesOkayToReport ) {
			//create predefined report
			equipName = this->Name;
			PreDefTableEntry( pdchMechType, equipName, "Chiller:Electric" );
			PreDefTableEntry( pdchMechNomEff, equipName, this->COP );
			PreDefTableEntry( pdchMechNomCap, equipName, this->NomCap );
		}

	}

	void
	EngineDrivenChillerSpecs::SizeEngineDrivenChiller()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Fred Buhl
		//       DATE WRITTEN   June 2002
		//       MODIFIED       November 2013 Daeho Kang, add component sizing table entries
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine is for sizing Engine Driven Chiller Components for which capacities and flow rates
		// have not been specified in the input.

		// METHODOLOGY EMPLOYED:
		// Obtains evaporator flow rate from the plant sizing array. Calculates nominal capacity from
		// the evaporator flow rate and the chilled water loop design delta T. The condenser flow rate
		// is calculated from the nominal capacity, the COP, and the condenser loop design delta T.

		// REFERENCES:
		// na

		// Using/Aliasing
		using namespace DataSizing;
		using DataPlant::PlantLoop;
		using DataPlant::PlantFirstSizesOkayToFinalize;
		using DataPlant::PlantFirstSizesOkayToReport;
		using DataPlant::PlantFinalSizesOkayToReport;
		using PlantUtilities::RegisterPlantCompDesignFlow;
		using ReportSizingManager::ReportSizingOutput;
		using namespace OutputReportPredefined;
		using FluidProperties::GetDensityGlycol;
		using FluidProperties::GetSpecificHeatGlycol;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "SizeEngineDrivenChiller" );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		//unused1208  INTEGER             :: PltSizIndex   ! Plant Sizing Do loop index
		int PltSizNum; // Plant Sizing index corresponding to CurLoopNum
		int PltSizCondNum; // Plant Sizing index for condenser loop
		bool ErrorsFound; // If errors detected in input
		std::string equipName;
		Real64 rho; // local fluid density
		Real64 Cp; // local fluid specific heat
		Real64 tmpNomCap; // local nominal capacity cooling power
		Real64 tmpEvapVolFlowRate; // local evaporator design volume flow rate
		Real64 tmpCondVolFlowRate; // local condenser design volume flow rate
		Real64 EvapVolFlowRateUser; // Hardsized evaporator flow rate for reporting
		Real64 NomCapUser; // Hardsized reference capacity for reporting
		Real64 CondVolFlowRateUser; // Hardsized condenser flow rate for reporting

		PltSizNum = 0;
		PltSizCondNum = 0;
		ErrorsFound = false;
		tmpNomCap = this->NomCap;
		tmpEvapVolFlowRate = this->EvapVolFlowRate;
		tmpCondVolFlowRate = this->CondVolFlowRate;
		EvapVolFlowRateUser = 0.0;
		NomCapUser = 0.0;
		CondVolFlowRateUser = 0.0;

		if ( this->CondenserType == WaterCooled ) {
			PltSizCondNum = PlantLoop( this->CDLoopNum ).PlantSizNum;
		}

		PltSizNum = PlantLoop( this->CWLoopNum ).PlantSizNum;

		if ( PltSizNum > 0 ) {
			if ( PlantSizData( PltSizNum ).DesVolFlowRate >= SmallWaterVolFlow ) {
				rho = GetDensityGlycol( PlantLoop( this->CWLoopNum ).FluidName, InitConvTemp, PlantLoop( this->CWLoopNum ).FluidIndex, RoutineName );
				Cp = GetSpecificHeatGlycol( PlantLoop( this->CWLoopNum ).FluidName, InitConvTemp, PlantLoop( this->CWLoopNum ).FluidIndex, RoutineName );
				tmpNomCap = Cp * rho * PlantSizData( PltSizNum ).DeltaT * PlantSizData( PltSizNum ).DesVolFlowRate * this->SizFac;
				if ( ! this->NomCapWasAutoSized ) tmpNomCap = this->NomCap;

			} else {
				if ( this->NomCapWasAutoSized ) tmpNomCap = 0.0;

			}
			if ( PlantFirstSizesOkayToFinalize ) {
				if ( this->NomCapWasAutoSized ) {
					this->NomCap = tmpNomCap;
					if (PlantFinalSizesOkayToReport ) {
						ReportSizingOutput( "Chiller:EngineDriven", this->Name,
							"Design Size Nominal Capacity [W]", tmpNomCap );
					}
					if (PlantFirstSizesOkayToReport ) {
						ReportSizingOutput( "Chiller:EngineDriven", this->Name,
							"Initial Design Size Nominal Capacity [W]", tmpNomCap );
					}
				} else {
					if ( this->NomCap > 0.0 && tmpNomCap > 0.0 ) {
						NomCapUser = this->NomCap;
						if ( PlantFinalSizesOkayToReport ) {
							ReportSizingOutput( "Chiller:EngineDriven", this->Name, "Design Size Nominal Capacity [W]", tmpNomCap, "User-Specified Nominal Capacity [W]", NomCapUser );
							if ( DisplayExtraWarnings ) {
								if ( ( std::abs( tmpNomCap - NomCapUser ) / NomCapUser ) > AutoVsHardSizingThreshold ) {
									ShowMessage( "SizeChillerEngineDriven: Potential issue with equipment sizing for " + this->Name );
									ShowContinueError( "User-Specified Nominal Capacity of " + RoundSigDigits( NomCapUser, 2 ) + " [W]" );
									ShowContinueError( "differs from Design Size Nominal Capacity of " + RoundSigDigits( tmpNomCap, 2 ) + " [W]" );
									ShowContinueError( "This may, or may not, indicate mismatched component sizes." );
									ShowContinueError( "Verify that the value entered is intended and is consistent with other components." );
								}
							}
						}
						tmpNomCap = NomCapUser;
					}
				}
			}
		} else {
			if ( this->NomCapWasAutoSized && PlantFirstSizesOkayToFinalize ) {
				ShowSevereError( "Autosizing of Engine Driven Chiller nominal capacity requires a loop Sizing:Plant object" );
				ShowContinueError( "Occurs in Engine Driven Chiller object=" + this->Name );
				ErrorsFound = true;
			}
			if ( ! this->NomCapWasAutoSized && PlantFinalSizesOkayToReport
					&& ( this->NomCap > 0.0 ) ) {
					ReportSizingOutput( "Chiller:EngineDriven", this->Name,
						"User-Specified Nominal Capacity [W]", this->NomCap );
			}
		}

		if ( PltSizNum > 0 ) {
			if ( PlantSizData( PltSizNum ).DesVolFlowRate >= SmallWaterVolFlow ) {
				tmpEvapVolFlowRate = PlantSizData( PltSizNum ).DesVolFlowRate * this->SizFac;
				if ( ! this->EvapVolFlowRateWasAutoSized ) tmpEvapVolFlowRate = this->EvapVolFlowRate;

			} else {
				if ( this->EvapVolFlowRateWasAutoSized ) tmpEvapVolFlowRate = 0.0;

			}
			if ( PlantFirstSizesOkayToFinalize ) {
				if ( this->EvapVolFlowRateWasAutoSized ) {
					this->EvapVolFlowRate = tmpEvapVolFlowRate;
					if ( PlantFinalSizesOkayToReport ) {
						ReportSizingOutput( "Chiller:EngineDriven", this->Name,
							"Design Chilled Water Flow Rate [m3/s]", tmpEvapVolFlowRate );
					}
					if ( PlantFirstSizesOkayToReport ) {
						ReportSizingOutput( "Chiller:EngineDriven", this->Name,
							"Initial Design Chilled Water Flow Rate [m3/s]", tmpEvapVolFlowRate );
					}
				} else {
					if ( this->EvapVolFlowRate > 0.0 && tmpEvapVolFlowRate > 0.0 ) {
						EvapVolFlowRateUser = this->EvapVolFlowRate;
						if ( PlantFinalSizesOkayToReport) {
							ReportSizingOutput( "Chiller:EngineDriven", this->Name,
								"Design Size Design Chilled Water Flow Rate [m3/s]", tmpEvapVolFlowRate,
								"User-Specified Design Chilled Water Flow Rate [m3/s]", EvapVolFlowRateUser );
							if ( DisplayExtraWarnings ) {
								if ( ( std::abs( tmpEvapVolFlowRate - EvapVolFlowRateUser ) / EvapVolFlowRateUser ) > AutoVsHardSizingThreshold ) {
									ShowMessage( "SizeChillerEngineDriven: Potential issue with equipment sizing for " + this->Name );
									ShowContinueError( "User-Specified Design Chilled Water Flow Rate of " + RoundSigDigits( EvapVolFlowRateUser, 5 ) + " [m3/s]" );
									ShowContinueError( "differs from Design Size Design Chilled Water Flow Rate of " + RoundSigDigits( tmpEvapVolFlowRate, 5 ) + " [m3/s]" );
									ShowContinueError( "This may, or may not, indicate mismatched component sizes." );
									ShowContinueError( "Verify that the value entered is intended and is consistent with other components." );
								}
							}
						}
						tmpEvapVolFlowRate = EvapVolFlowRateUser;
					}
				}
			}
		} else {
			if ( this->EvapVolFlowRateWasAutoSized && PlantFirstSizesOkayToFinalize ) {
				ShowSevereError( "Autosizing of Engine Driven Chiller evap flow rate requires a loop Sizing:Plant object" );
				ShowContinueError( "Occurs in Engine Driven Chiller object=" + this->Name );
				ErrorsFound = true;
			}
			if ( ! this->EvapVolFlowRateWasAutoSized && PlantFinalSizesOkayToReport
					&& ( this->EvapVolFlowRate > 0.0 ) ) {
						ReportSizingOutput( "Chiller:EngineDriven", this->Name,
							"User-Specified Design Chilled Water Flow Rate [m3/s]", this->EvapVolFlowRate );

			}
		}

		RegisterPlantCompDesignFlow( this->EvapInletNodeNum, tmpEvapVolFlowRate );

		if ( PltSizCondNum > 0 && PltSizNum > 0 ) {
			if ( PlantSizData( PltSizNum ).DesVolFlowRate >= SmallWaterVolFlow && tmpNomCap > 0.0 ) {
				rho = GetDensityGlycol( PlantLoop( this->CDLoopNum ).FluidName, this->TempDesCondIn, PlantLoop( this->CDLoopNum ).FluidIndex, RoutineName );

				Cp = GetSpecificHeatGlycol( PlantLoop( this->CDLoopNum ).FluidName, this->TempDesCondIn, PlantLoop( this->CDLoopNum ).FluidIndex, RoutineName );
				tmpCondVolFlowRate = tmpNomCap * ( 1.0 + 1.0 / this->COP ) / ( PlantSizData( PltSizCondNum ).DeltaT * Cp * rho );
				if ( ! this->CondVolFlowRateWasAutoSized ) tmpCondVolFlowRate = this->CondVolFlowRate;

			} else {
				if ( this->CondVolFlowRateWasAutoSized ) tmpCondVolFlowRate = 0.0;
			}
			if ( PlantFirstSizesOkayToFinalize ) {
				if ( this->CondVolFlowRateWasAutoSized ) {
					this->CondVolFlowRate = tmpCondVolFlowRate;
					if ( PlantFinalSizesOkayToReport ) {
						ReportSizingOutput( "Chiller:EngineDriven", this->Name,
							"Design Size Design Condenser Water Flow Rate [m3/s]", tmpCondVolFlowRate );
					}
					if ( PlantFirstSizesOkayToReport ) {
						ReportSizingOutput( "Chiller:EngineDriven", this->Name,
							"Initial Design Size Design Condenser Water Flow Rate [m3/s]", tmpCondVolFlowRate );
					}
				} else {
					if ( this->CondVolFlowRate > 0.0 && tmpCondVolFlowRate > 0.0 ) {
						CondVolFlowRateUser = this->CondVolFlowRate;
						if ( PlantFinalSizesOkayToReport ) {
							ReportSizingOutput( "Chiller:EngineDriven", this->Name,
								"Design Size Design Condenser Water Flow Rate [m3/s]", tmpCondVolFlowRate,
								"User-Specified Design Condenser Water Flow Rate [m3/s]", CondVolFlowRateUser );
							if ( DisplayExtraWarnings ) {
								if ( ( std::abs( tmpCondVolFlowRate - CondVolFlowRateUser ) / CondVolFlowRateUser ) > AutoVsHardSizingThreshold ) {
									ShowMessage( "SizeChillerEngineDriven: Potential issue with equipment sizing for " + this->Name );
									ShowContinueError( "User-Specified Design Condenser Water Flow Rate of " + RoundSigDigits( CondVolFlowRateUser, 5 ) + " [m3/s]" );
									ShowContinueError( "differs from Design Size Design Condenser Water Flow Rate of " + RoundSigDigits( tmpCondVolFlowRate, 5 ) + " [m3/s]" );
									ShowContinueError( "This may, or may not, indicate mismatched component sizes." );
									ShowContinueError( "Verify that the value entered is intended and is consistent with other components." );
								}
							}
						}
						tmpCondVolFlowRate = CondVolFlowRateUser;
					}
				}
			}
		} else {
			if ( this->CondVolFlowRateWasAutoSized && PlantFirstSizesOkayToFinalize ) {
				ShowSevereError( "Autosizing of EngineDriven Chiller condenser flow rate requires a condenser" );
				ShowContinueError( "loop Sizing:Plant object" );
				ShowContinueError( "Occurs in EngineDriven Chiller object=" + this->Name );
				ErrorsFound = true;

			}
			if ( ! this->CondVolFlowRateWasAutoSized && PlantFinalSizesOkayToReport
					&& ( this->CondVolFlowRate > 0.0 ) ) {
					ReportSizingOutput( "Chiller:EngineDriven", this->Name,
						"User-Specified Design Condenser Water Flow Rate [m3/s]", this->CondVolFlowRate );
			}
		}

		// save the design condenser water volumetric flow rate for use by the condenser water loop sizing algorithms
		if ( this->CondenserType == WaterCooled ) {
			RegisterPlantCompDesignFlow( this->CondInletNodeNum, tmpCondVolFlowRate );
		}

		if ( PlantFinalSizesOkayToReport ) {
			//create predefined report
			equipName = this->Name;
			PreDefTableEntry( pdchMechType, equipName, "Chiller:EngineDriven" );
			PreDefTableEntry( pdchMechNomEff, equipName, this->COP );
			PreDefTableEntry( pdchMechNomCap, equipName, this->NomCap );
		}

		if ( ErrorsFound ) {
			ShowFatalError( "Preceding sizing errors cause program termination" );
		}

	}

	void
	GTChillerSpecs::SizeGTChiller()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Fred Buhl
		//       DATE WRITTEN   June 2002
		//       MODIFIED       November 2013 Daeho Kang, add component sizing table entries
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine is for sizing Gas Turbine Chiller Components for which capacities and flow rates
		// have not been specified in the input.

		// METHODOLOGY EMPLOYED:
		// Obtains evaporator flow rate from the plant sizing array. Calculates nominal capacity from
		// the evaporator flow rate and the chilled water loop design delta T. The condenser flow rate
		// is calculated from the nominal capacity, the COP, and the condenser loop design delta T.

		// REFERENCES:
		// na

		// Using/Aliasing
		using namespace DataSizing;
		using DataPlant::PlantLoop;
		using DataPlant::PlantFirstSizesOkayToFinalize;
		using DataPlant::PlantFirstSizesOkayToReport;
		using DataPlant::PlantFinalSizesOkayToReport;
		using PlantUtilities::RegisterPlantCompDesignFlow;
		using ReportSizingManager::ReportSizingOutput;
		using namespace OutputReportPredefined;
		using FluidProperties::GetDensityGlycol;
		using FluidProperties::GetSpecificHeatGlycol;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "SizeGTChiller" );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		//unused1208  INTEGER             :: PltSizIndex   ! Plant Sizing Do loop index
		int PltSizNum; // Plant Sizing index corresponding to CurLoopNum
		int PltSizCondNum; // Plant Sizing index for condenser loop
		bool ErrorsFound; // If errors detected in input
		Real64 EngineEff; // this should be an input! needed to autosize the engine capacity.
		std::string equipName;
		Real64 rho; // local fluid density
		Real64 Cp; // local fluid specific heat
		Real64 tmpNomCap; // local nominal capacity cooling power
		Real64 tmpEvapVolFlowRate; // local evaporator design volume flow rate
		Real64 tmpCondVolFlowRate; // local condenser design volume flow rate
		Real64 EvapVolFlowRateUser; // Hardsized evaporator flow rate for reporting
		Real64 NomCapUser; // Hardsized reference capacity for reporting
		Real64 CondVolFlowRateUser; // Hardsized condenser flow rate for reporting
		Real64 GTEngineCapacityDes; // Autosized GT engine capacity for reporting
		Real64 GTEngineCapacityUser; // Hardsized GT engine capacity for reporting

		PltSizNum = 0;
		PltSizCondNum = 0;
		EngineEff = 0.35;
		ErrorsFound = false;
		tmpNomCap = this->NomCap;
		tmpEvapVolFlowRate = this->EvapVolFlowRate;
		tmpCondVolFlowRate = this->CondVolFlowRate;
		EvapVolFlowRateUser = 0.0;
		NomCapUser = 0.0;
		CondVolFlowRateUser = 0.0;
		GTEngineCapacityDes = 0.0;
		GTEngineCapacityUser = 0.0;

		if ( this->CondenserType == WaterCooled ) {
			//if ( this->CondVolFlowRate == AutoSize ) {
			PltSizCondNum = PlantLoop( this->CDLoopNum ).PlantSizNum;
			//}
		}

		PltSizNum = PlantLoop( this->CWLoopNum ).PlantSizNum;

		if ( PltSizNum > 0 ) {
			if ( PlantSizData( PltSizNum ).DesVolFlowRate >= SmallWaterVolFlow ) {
				rho = GetDensityGlycol( PlantLoop( this->CWLoopNum ).FluidName, InitConvTemp, PlantLoop( this->CWLoopNum ).FluidIndex, RoutineName );
				Cp = GetSpecificHeatGlycol( PlantLoop( this->CWLoopNum ).FluidName, InitConvTemp, PlantLoop( this->CWLoopNum ).FluidIndex, RoutineName );
				tmpNomCap = Cp * rho * PlantSizData( PltSizNum ).DeltaT * PlantSizData( PltSizNum ).DesVolFlowRate * this->SizFac;
				if ( ! this->NomCapWasAutoSized ) tmpNomCap = this->NomCap;
				//IF (PlantFirstSizesOkayToFinalize)  GTChiller(ChillNum)%Base%NomCap = tmpNomCap
			} else {
				if ( this->NomCapWasAutoSized ) tmpNomCap = 0.0;
				//IF (PlantFirstSizesOkayToFinalize) GTChiller(ChillNum)%Base%NomCap = tmpNomCap
			}
			if ( PlantFirstSizesOkayToFinalize ) {
				if ( this->NomCapWasAutoSized ) {
					this->NomCap = tmpNomCap;
					if ( PlantFinalSizesOkayToReport ) {
						ReportSizingOutput( "Chiller:CombustionTurbine", this->Name,
							"Design Size Nominal Capacity [W]", tmpNomCap );
					}
					if ( PlantFirstSizesOkayToReport ) {
						ReportSizingOutput( "Chiller:CombustionTurbine", this->Name,
							"Initial Design Size Nominal Capacity [W]", tmpNomCap );
					}
				} else {
					if ( this->NomCap > 0.0 && tmpNomCap > 0.0 ) {
						NomCapUser = this->NomCap;
						if ( PlantFinalSizesOkayToReport ) {
							ReportSizingOutput( "Chiller:CombustionTurbine", this->Name, "Design Size Nominal Capacity [W]", tmpNomCap, "User-Specified Nominal Capacity [W]", NomCapUser );
							if ( DisplayExtraWarnings ) {
								if ( ( std::abs( tmpNomCap - NomCapUser ) / NomCapUser ) > AutoVsHardSizingThreshold ) {
									ShowMessage( "SizeChillerElectricEIR: Potential issue with equipment sizing for " + this->Name );
									ShowContinueError( "User-Specified Nominal Capacity of " + RoundSigDigits( NomCapUser, 2 ) + " [W]" );
									ShowContinueError( "differs from Design Size Nominal Capacity of " + RoundSigDigits( tmpNomCap, 2 ) + " [W]" );
									ShowContinueError( "This may, or may not, indicate mismatched component sizes." );
									ShowContinueError( "Verify that the value entered is intended and is consistent with other components." );
								}
							}
						}
						tmpNomCap = NomCapUser;
					}
				}
			}
		} else {
			if ( this->NomCapWasAutoSized && PlantFirstSizesOkayToFinalize ) {
				ShowSevereError( "Autosizing of Gas Turbine Chiller nominal capacity requires a loop Sizing:Plant object" );
				ShowContinueError( "Occurs in Gas Turbine Chiller object=" + this->Name );
				ErrorsFound = true;
			}
			if ( ! this->NomCapWasAutoSized && PlantFinalSizesOkayToReport
					&& ( this->NomCap > 0.0 ) ) {
					ReportSizingOutput( "Chiller:CombustionTurbine", this->Name,
						"User-Specified Design Size Nominal Capacity [W]", this->NomCap );
			}
		}

		if ( PltSizNum > 0 ) {
			if ( PlantSizData( PltSizNum ).DesVolFlowRate >= SmallWaterVolFlow ) {
				tmpEvapVolFlowRate = PlantSizData( PltSizNum ).DesVolFlowRate * this->SizFac;
				if ( ! this->EvapVolFlowRateWasAutoSized ) tmpEvapVolFlowRate = this->EvapVolFlowRate;

			} else {
				if ( this->EvapVolFlowRateWasAutoSized ) tmpEvapVolFlowRate = 0.0;

			}
			if ( PlantFirstSizesOkayToFinalize ) {
				if ( this->EvapVolFlowRateWasAutoSized ) {
					this->EvapVolFlowRate = tmpEvapVolFlowRate;
					if ( PlantFinalSizesOkayToReport ) {
						ReportSizingOutput( "Chiller:CombustionTurbine", this->Name,
							"Design size Design Chilled Water Flow Rate [m3/s]", tmpEvapVolFlowRate );
					}
					if ( PlantFirstSizesOkayToReport ) {
						ReportSizingOutput( "Chiller:CombustionTurbine", this->Name,
							"Initial Design size Design Chilled Water Flow Rate [m3/s]", tmpEvapVolFlowRate );
					}
				} else {
					if ( this->EvapVolFlowRate > 0.0 && tmpEvapVolFlowRate > 0.0 ) {
						EvapVolFlowRateUser = this->EvapVolFlowRate;
						if ( PlantFinalSizesOkayToReport ) {
							ReportSizingOutput( "Chiller:CombustionTurbine", this->Name, "Design size Design Chilled Water Flow Rate [m3/s]", tmpEvapVolFlowRate, "User-Specified Design Chilled Water Flow Rate [m3/s]", EvapVolFlowRateUser );
							if ( DisplayExtraWarnings ) {
								if ( ( std::abs( tmpEvapVolFlowRate - EvapVolFlowRateUser ) / EvapVolFlowRateUser ) > AutoVsHardSizingThreshold ) {
									ShowMessage( "SizeChillerElectricEIR: Potential issue with equipment sizing for " + this->Name );
									ShowContinueError( "User-Specified Design Chilled Water Flow Rate of " + RoundSigDigits( EvapVolFlowRateUser, 5 ) + " [m3/s]" );
									ShowContinueError( "differs from Design Size Design Chilled Water Flow Rate of " + RoundSigDigits( tmpEvapVolFlowRate, 5 ) + " [m3/s]" );
									ShowContinueError( "This may, or may not, indicate mismatched component sizes." );
									ShowContinueError( "Verify that the value entered is intended and is consistent with other components." );
								}
							}
						}
						tmpEvapVolFlowRate = EvapVolFlowRateUser;
					}
				}
			}
		} else {
			if ( this->EvapVolFlowRateWasAutoSized && PlantFirstSizesOkayToFinalize ) {
				ShowSevereError( "Autosizing of Gas Turbine Chiller evap flow rate requires a loop Sizing:Plant object" );
				ShowContinueError( "Occurs in Gas Turbine Chiller object=" + this->Name );
				ErrorsFound = true;
			}
			if ( ! this->EvapVolFlowRateWasAutoSized && PlantFinalSizesOkayToReport
					&& ( this->EvapVolFlowRate > 0.0 ) ) {
					ReportSizingOutput( "Chiller:CombustionTurbine", this->Name,
						"User-Specified Design Chilled Water Flow Rate [m3/s]", this->EvapVolFlowRate );
			}
		}

		RegisterPlantCompDesignFlow( this->EvapInletNodeNum, tmpEvapVolFlowRate );

		if ( PltSizCondNum > 0 && PltSizNum > 0 ) {
			if ( PlantSizData( PltSizNum ).DesVolFlowRate >= SmallWaterVolFlow && tmpNomCap > 0.0 ) {
				rho = GetDensityGlycol( PlantLoop( this->CDLoopNum ).FluidName, this->TempDesCondIn, PlantLoop( this->CDLoopNum ).FluidIndex, RoutineName );

				Cp = GetSpecificHeatGlycol( PlantLoop( this->CDLoopNum ).FluidName, this->TempDesCondIn, PlantLoop( this->CDLoopNum ).FluidIndex, RoutineName );
				tmpCondVolFlowRate = tmpNomCap * ( 1.0 + 1.0 / this->COP ) / ( PlantSizData( PltSizCondNum ).DeltaT * Cp * rho );
				if ( ! this->CondVolFlowRateWasAutoSized ) tmpCondVolFlowRate = this->CondVolFlowRate;
			} else {
				if ( this->CondVolFlowRateWasAutoSized ) tmpCondVolFlowRate = 0.0;
			}
			if ( PlantFirstSizesOkayToFinalize ) {
				if ( this->CondVolFlowRateWasAutoSized ) {
					this->CondVolFlowRate = tmpCondVolFlowRate;
					if ( PlantFinalSizesOkayToReport ) {
						ReportSizingOutput( "Chiller:CombustionTurbine", this->Name,
							"Design Size Design Condenser Water Flow Rate [m3/s]", tmpCondVolFlowRate );
					}
					if ( PlantFirstSizesOkayToReport ) {
						ReportSizingOutput( "Chiller:CombustionTurbine", this->Name,
							"Initial Design Size Design Condenser Water Flow Rate [m3/s]", tmpCondVolFlowRate );
					}
				} else {
					if ( this->CondVolFlowRate > 0.0 && tmpCondVolFlowRate > 0.0 ) {
						CondVolFlowRateUser = this->CondVolFlowRate;
						if ( PlantFinalSizesOkayToReport ) {
							ReportSizingOutput( "Chiller:CombustionTurbine", this->Name, "Design Size Design Condenser Water Flow Rate [m3/s]", tmpCondVolFlowRate, "User-Specified Design Condenser Water Flow Rate [m3/s]", CondVolFlowRateUser );
							if ( DisplayExtraWarnings ) {
								if ( ( std::abs( tmpCondVolFlowRate - CondVolFlowRateUser ) / CondVolFlowRateUser ) > AutoVsHardSizingThreshold ) {
									ShowMessage( "SizeChillerElectricEIR: Potential issue with equipment sizing for " + this->Name );
									ShowContinueError( "User-Specified Design Condenser Water Flow Rate of " + RoundSigDigits( CondVolFlowRateUser, 5 ) + " [m3/s]" );
									ShowContinueError( "differs from Design Size Design Condenser Water Flow Rate of " + RoundSigDigits( tmpCondVolFlowRate, 5 ) + " [m3/s]" );
									ShowContinueError( "This may, or may not, indicate mismatched component sizes." );
									ShowContinueError( "Verify that the value entered is intended and is consistent with other components." );
								}
							}
						}
						tmpCondVolFlowRate = CondVolFlowRateUser;
					}
				}
			}
		} else {
			if ( this->CondVolFlowRateWasAutoSized && PlantFirstSizesOkayToFinalize ) {
				ShowSevereError( "Autosizing of Gas Turbine Chiller condenser flow rate requires a condenser" );
				ShowContinueError( "loop Sizing:Plant object" );
				ShowContinueError( "Occurs in Gas Turbine Chiller object=" + this->Name );
				ErrorsFound = true;
			}
			if ( ! this->CondVolFlowRateWasAutoSized  && PlantFinalSizesOkayToReport
					&& ( this->CondVolFlowRate > 0.0 ) ) {
					ReportSizingOutput( "Chiller:Electric", this->Name,
						"User-Specified Design Condenser Water Flow Rate [m3/s]", this->CondVolFlowRate );

			}
		}
		// save the design condenser water volumetric flow rate for use by the condenser water loop sizing algorithms
		if ( this->CondenserType == WaterCooled ) RegisterPlantCompDesignFlow( this->CondInletNodeNum, tmpCondVolFlowRate );


		GTEngineCapacityDes = this->NomCap * EngineEff / this->COP;
		if ( PlantFirstSizesOkayToFinalize ) {
			if ( this->GTEngineCapacityWasAutoSized ) {
				this->GTEngineCapacity = GTEngineCapacityDes;
				if ( PlantFinalSizesOkayToReport ) {
					ReportSizingOutput( "Chiller:CombustionTurbine", this->Name,
						"Design Size Gas Turbine Engine Capacity [W]", GTEngineCapacityDes );
				}
				if ( PlantFirstSizesOkayToReport ) {
					ReportSizingOutput( "Chiller:CombustionTurbine", this->Name,
						"Initial Design Size Gas Turbine Engine Capacity [W]", GTEngineCapacityDes );
				}
			} else {
				if ( this->GTEngineCapacity > 0.0 && GTEngineCapacityDes > 0.0 ) {
					GTEngineCapacityUser = this->GTEngineCapacity;
					if ( PlantFinalSizesOkayToReport ) {
						ReportSizingOutput( "Chiller:CombustionTurbine", this->Name, "Design Size Gas Turbine Engine Capacity [W]", GTEngineCapacityDes, "User-Specified Gas Turbine Engine Capacity [W]", GTEngineCapacityUser );
					}
					if ( DisplayExtraWarnings ) {
						if ( ( std::abs( GTEngineCapacityDes - GTEngineCapacityUser ) / GTEngineCapacityUser ) > AutoVsHardSizingThreshold ) {
							ShowMessage( "SizeChillerElectricEIR: Potential issue with equipment sizing for " + this->Name );
							ShowContinueError( "User-Specified Gas Turbine Engine Capacity of " + RoundSigDigits( GTEngineCapacityUser, 2 ) + " [W]" );
							ShowContinueError( "differs from Design Size Gas Turbine Engine Capacity of " + RoundSigDigits( GTEngineCapacityDes, 2 ) + " [W]" );
							ShowContinueError( "This may, or may not, indicate mismatched component sizes." );
							ShowContinueError( "Verify that the value entered is intended and is consistent with other components." );
						}
					}
				}
			}
		}

		if ( PlantFinalSizesOkayToReport ) {
			//create predefined report
			equipName = this->Name;
			PreDefTableEntry( pdchMechType, equipName, "Chiller:CombustionTurbine" );
			PreDefTableEntry( pdchMechNomEff, equipName, this->COP );
			PreDefTableEntry( pdchMechNomCap, equipName, this->NomCap );
		}

		if ( ErrorsFound ) {
			ShowFatalError( "Preceding sizing errors cause program termination" );
		}

	}

	void
	ConstCOPChillerSpecs::SizeConstCOPChiller()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Fred Buhl
		//       DATE WRITTEN   March 2008
		//       MODIFIED       November 2013 Daeho Kang, add component sizing table entries
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine is for sizing Constabt COP Chiller Components for which capacities and flow rates
		// have not been specified in the input.

		// METHODOLOGY EMPLOYED:
		// Obtains evaporator flow rate from the plant sizing array. Calculates nominal capacity from
		// the evaporator flow rate and the chilled water loop design delta T. The condenser flow rate
		// is calculated from the nominal capacity, the COP, and the condenser loop design delta T.

		// REFERENCES:
		// na

		// Using/Aliasing
		using namespace DataSizing;
		using DataPlant::PlantLoop;
		using DataPlant::PlantFirstSizesOkayToFinalize;
		using DataPlant::PlantFirstSizesOkayToReport;
		using DataPlant::PlantFinalSizesOkayToReport;
		using PlantUtilities::RegisterPlantCompDesignFlow;
		using ReportSizingManager::ReportSizingOutput;
		using namespace OutputReportPredefined;
		using FluidProperties::GetDensityGlycol;
		using FluidProperties::GetSpecificHeatGlycol;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "SizeConstCOPChiller" );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		//unused1208  INTEGER             :: PltSizIndex   ! Plant Sizing Do loop index
		int PltSizNum; // Plant Sizing index corresponding to CurLoopNum
		int PltSizCondNum; // Plant Sizing index for condenser loop
		bool ErrorsFound; // If errors detected in input
		std::string equipName;
		Real64 rho; // local fluid density
		Real64 Cp; // local fluid specific heat
		Real64 tmpNomCap; // local nominal capacity cooling power
		Real64 tmpEvapVolFlowRate; // local evaporator design volume flow rate
		Real64 tmpCondVolFlowRate; // local condenser design volume flow rate
		Real64 EvapVolFlowRateUser; // Hardsized evaporator flow for reporting
		Real64 NomCapUser; // Hardsized reference capacity for reporting
		Real64 CondVolFlowRateUser; // Hardsized condenser flow for reporting

		PltSizNum = 0;
		PltSizCondNum = 0;
		ErrorsFound = false;
		tmpNomCap = this->NomCap;
		tmpEvapVolFlowRate = this->EvapVolFlowRate;
		tmpCondVolFlowRate = this->CondVolFlowRate;

		EvapVolFlowRateUser = 0.0;
		NomCapUser = 0.0;
		CondVolFlowRateUser = 0.0;

		if ( this->CondenserType == WaterCooled ) {
			//IF (ConstCOPChiller(ChillNum)%Base%CondVolFlowRate == AutoSize) THEN
			PltSizCondNum = PlantLoop( this->CDLoopNum ).PlantSizNum;
			//END IF
		}

		PltSizNum = PlantLoop( this->CWLoopNum ).PlantSizNum;

		if ( PltSizNum > 0 ) {
			if ( PlantSizData( PltSizNum ).DesVolFlowRate >= SmallWaterVolFlow ) {
				rho = GetDensityGlycol( PlantLoop( this->CWLoopNum ).FluidName, InitConvTemp, PlantLoop( this->CWLoopNum ).FluidIndex, RoutineName );
				Cp = GetSpecificHeatGlycol( PlantLoop( this->CWLoopNum ).FluidName, InitConvTemp, PlantLoop( this->CWLoopNum ).FluidIndex, RoutineName );
				tmpNomCap = Cp * rho * PlantSizData( PltSizNum ).DeltaT * PlantSizData( PltSizNum ).DesVolFlowRate * this->SizFac;
				if ( ! this->NomCapWasAutoSized ) tmpNomCap = this->NomCap;
			} else {
				if ( this->NomCapWasAutoSized ) tmpNomCap = 0.0;
			}
			if ( PlantFirstSizesOkayToFinalize ) {
				if ( this->NomCapWasAutoSized ) {
					this->NomCap = tmpNomCap;
					if ( PlantFinalSizesOkayToReport ) {
						ReportSizingOutput( "Chiller:ConstantCOP", this->Name,
							"Design Size Nominal Capacity [W]", tmpNomCap );
					}
					if ( PlantFirstSizesOkayToReport ) {
						ReportSizingOutput( "Chiller:ConstantCOP", this->Name,
							"Initial Design Size Nominal Capacity [W]", tmpNomCap );
					}
				} else { // Hard-size with sizing data
					if ( this->NomCap > 0.0 && tmpNomCap > 0.0 ) {
						NomCapUser = this->NomCap;
						if ( PlantFinalSizesOkayToReport ) {
							ReportSizingOutput( "Chiller:ConstantCOP", this->Name, "Design Size Nominal Capacity [W]", tmpNomCap, "User-Specified Nominal Capacity [W]", NomCapUser );
							if ( DisplayExtraWarnings ) {
								if ( ( std::abs( tmpNomCap - NomCapUser ) / NomCapUser ) > AutoVsHardSizingThreshold ) {
									ShowMessage( "SizeChillerConstantCOP: Potential issue with equipment sizing for " + this->Name );
									ShowContinueError( "User-Specified Nominal Capacity of " + RoundSigDigits( NomCapUser, 2 ) + " [W]" );
									ShowContinueError( "differs from Design Size Nominal Capacity of " + RoundSigDigits( tmpNomCap, 2 ) + " [W]" );
									ShowContinueError( "This may, or may not, indicate mismatched component sizes." );
									ShowContinueError( "Verify that the value entered is intended and is consistent with other components." );
								}
							}
						}
						tmpNomCap = NomCapUser;
					}
				}
			}
		} else {
			if ( this->NomCapWasAutoSized && PlantFirstSizesOkayToFinalize ) {
				ShowSevereError( "Autosizing of Constant COP Chiller nominal capacity requires a loop Sizing:Plant object" );
				ShowContinueError( "Occurs in Chiller:ConstantCOP object=" + this->Name );
				ErrorsFound = true;
			}
			if ( ! this->NomCapWasAutoSized && PlantFinalSizesOkayToReport
					&& ( this->NomCap > 0.0 ) ) {
					ReportSizingOutput( "Chiller:ConstantCOP", this->Name,
						"User-Specified Nominal Capacity [W]", this->NomCap );
			}
		}

		if ( PltSizNum > 0 ) {
			if ( PlantSizData( PltSizNum ).DesVolFlowRate >= SmallWaterVolFlow ) {
				tmpEvapVolFlowRate = PlantSizData( PltSizNum ).DesVolFlowRate * this->SizFac;
				if ( ! this->EvapVolFlowRateWasAutoSized ) tmpEvapVolFlowRate = this->EvapVolFlowRate;
			} else {
				if ( this->EvapVolFlowRateWasAutoSized ) tmpEvapVolFlowRate = 0.0;
			}
			if ( PlantFirstSizesOkayToFinalize ) {
				if ( this->EvapVolFlowRateWasAutoSized ) {
					this->EvapVolFlowRate = tmpEvapVolFlowRate;
					if ( PlantFinalSizesOkayToReport ) {
						ReportSizingOutput( "Chiller:ConstantCOP", this->Name,
							"Design Size Design Chilled Water Flow Rate [m3/s]", tmpEvapVolFlowRate );
					}
					if ( PlantFirstSizesOkayToReport ) {
						ReportSizingOutput( "Chiller:ConstantCOP", this->Name,
							"Initial Design Size Design Chilled Water Flow Rate [m3/s]", tmpEvapVolFlowRate );
					}
				} else {
					if ( this->EvapVolFlowRate > 0.0 && tmpEvapVolFlowRate > 0.0 ) {
						EvapVolFlowRateUser = this->EvapVolFlowRate;
						if ( PlantFinalSizesOkayToReport ) {
							ReportSizingOutput( "Chiller:ConstantCOP", this->Name, "Design Size Design Chilled Water Flow Rate [m3/s]", tmpEvapVolFlowRate, "User-Specified Design Chilled Water Flow Rate [m3/s]", EvapVolFlowRateUser );
							if ( DisplayExtraWarnings ) {
								if ( ( std::abs( tmpEvapVolFlowRate - EvapVolFlowRateUser ) / EvapVolFlowRateUser ) > AutoVsHardSizingThreshold ) {
									ShowMessage( "SizeChillerConstantCOP: Potential issue with equipment sizing for " + this->Name );
									ShowContinueError( "User-Specified Design Chilled Water Flow Rate of " + RoundSigDigits( EvapVolFlowRateUser, 5 ) + " [m3/s]" );
									ShowContinueError( "differs from Design Size Design Chilled Water Flow Rate of " + RoundSigDigits( tmpEvapVolFlowRate, 5 ) + " [m3/s]" );
									ShowContinueError( "This may, or may not, indicate mismatched component sizes." );
									ShowContinueError( "Verify that the value entered is intended and is consistent with other components." );
								}
							}
						}
						tmpEvapVolFlowRate = EvapVolFlowRateUser;
					}
				}
			}
		} else {
			if ( this->EvapVolFlowRateWasAutoSized && PlantFirstSizesOkayToFinalize ) {
				ShowSevereError( "Autosizing of Constant COP Chiller evap flow rate requires a loop Sizing:Plant object" );
				ShowContinueError( "Occurs in Chiller:ConstantCOP object=" + this->Name );
				ErrorsFound = true;
			}
			if ( ! this->EvapVolFlowRateWasAutoSized && PlantFinalSizesOkayToReport
					&& ( this->EvapVolFlowRate > 0.0 ) ) {
					ReportSizingOutput( "Chiller:ConstantCOP", this->Name,
						"User-Specified Design Chilled Water Flow Rate [m3/s]", this->EvapVolFlowRate );
			}
		}

		RegisterPlantCompDesignFlow( this->EvapInletNodeNum, tmpEvapVolFlowRate );

		if ( this->CondenserType == WaterCooled ) {
			if ( PltSizCondNum > 0 && PltSizNum > 0 ) {
				if ( PlantSizData( PltSizNum ).DesVolFlowRate >= SmallWaterVolFlow && tmpNomCap > 0.0 ) {
					rho = GetDensityGlycol( PlantLoop( this->CDLoopNum ).FluidName, 29.44, PlantLoop( this->CDLoopNum ).FluidIndex, RoutineName );

					Cp = GetSpecificHeatGlycol( PlantLoop( this->CDLoopNum ).FluidName, 29.44, PlantLoop( this->CDLoopNum ).FluidIndex, RoutineName );
					tmpCondVolFlowRate = tmpNomCap * ( 1.0 + 1.0 / this->COP ) / ( PlantSizData( PltSizCondNum ).DeltaT * Cp * rho );
					if ( ! this->CondVolFlowRateWasAutoSized ) tmpCondVolFlowRate = this->CondVolFlowRate;
				} else {
					if ( this->CondVolFlowRateWasAutoSized ) tmpCondVolFlowRate = 0.0;
				}
				if ( PlantFirstSizesOkayToFinalize ) {
					if ( this->CondVolFlowRateWasAutoSized ) {
						this->CondVolFlowRate = tmpCondVolFlowRate;
						if ( PlantFinalSizesOkayToReport ) {
							ReportSizingOutput( "Chiller:ConstantCOP", this->Name,
								"Design Size Design Condenser Water Flow Rate [m3/s]", tmpCondVolFlowRate );
						}
						if ( PlantFirstSizesOkayToReport ) {
							ReportSizingOutput( "Chiller:ConstantCOP", this->Name,
								"Initial Design Size Design Condenser Water Flow Rate [m3/s]", tmpCondVolFlowRate );
						}
					} else {
						if ( this->CondVolFlowRate > 0.0 && tmpCondVolFlowRate > 0.0 ) {
							CondVolFlowRateUser = this->CondVolFlowRate;
							if ( PlantFinalSizesOkayToReport ) {
								ReportSizingOutput( "Chiller:ConstantCOP", this->Name, "Design Size Design Condenser Water Flow Rate [m3/s]", tmpCondVolFlowRate, "User-Specified Design Condenser Water Flow Rate [m3/s]", CondVolFlowRateUser );
								if ( DisplayExtraWarnings ) {
									if ( ( std::abs( tmpCondVolFlowRate - CondVolFlowRateUser ) / CondVolFlowRateUser ) > AutoVsHardSizingThreshold ) {
										ShowMessage( "SizeChillerConstantCOP: Potential issue with equipment sizing for " + this->Name );
										ShowContinueError( "User-Specified Design Condenser Water Flow Rate of " + RoundSigDigits( CondVolFlowRateUser, 5 ) + " [m3/s]" );
										ShowContinueError( "differs from Design Size Design Condenser Water Flow Rate of " + RoundSigDigits( tmpCondVolFlowRate, 5 ) + " [m3/s]" );
										ShowContinueError( "This may, or may not, indicate mismatched component sizes." );
										ShowContinueError( "Verify that the value entered is intended and is consistent with other components." );
									}
								}
							}
							tmpCondVolFlowRate = CondVolFlowRateUser;
						}
					}
				}
			} else {
				if ( this->CondVolFlowRateWasAutoSized && PlantFirstSizesOkayToFinalize ) {
					ShowSevereError( "Autosizing of Constant COP Chiller condenser flow rate requires a condenser" );
					ShowContinueError( "loop Sizing:Plant object" );
					ShowContinueError( "Occurs in Chiller:ConstantCOP object=" + this->Name );
					ErrorsFound = true;
				}
				if ( ! this->CondVolFlowRateWasAutoSized && PlantFinalSizesOkayToReport
						&& ( this->CondVolFlowRate > 0.0 ) ) {
						ReportSizingOutput( "Chiller:ConstantCOP", this->Name,
							"User-Specified Design Condenser Water Flow Rate [m3/s]", this->CondVolFlowRate );
				}
			}
		}

		// save the design condenser water volumetric flow rate for use by the condenser water loop sizing algorithms
		if ( this->CondenserType == WaterCooled ) RegisterPlantCompDesignFlow( this->CondInletNodeNum, tmpCondVolFlowRate );

		if ( ErrorsFound ) {
			ShowFatalError( "Preceding sizing errors cause program termination" );
		}

		//create predefined report
		if ( PlantFinalSizesOkayToReport ) {
			equipName = this->Name;
			PreDefTableEntry( pdchMechType, equipName, "Chiller:ConstantCOP" );
			PreDefTableEntry( pdchMechNomEff, equipName, this->COP );
			PreDefTableEntry( pdchMechNomCap, equipName, this->NomCap );
		}

	}

	void
	ElectricChillerSpecs::CalcElectricChillerModel(
		Real64 & MyLoad, // operating load
		int const EquipFlowCtrl, // Flow control mode for the equipment
		bool const RunFlag // TRUE when chiller operating
	)
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         Dan Fisher / Brandon Anderson
		//       DATE WRITTEN   Sept. 2000
		//       MODIFIED       Chandan Sharma, FSEC, February 2010, Added basin heater
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// simulate a vapor compression chiller using the Electric model

		// METHODOLOGY EMPLOYED:
		// curve fit of performance data:

		// REFERENCES:
		// 1. BLAST Users Manual
		// 2. CHILLER User Manual

		// Using/Aliasing
		using namespace DataPrecisionGlobals;
		using DataGlobals::BeginEnvrnFlag;
		using DataGlobals::SecInHour;
		using DataGlobals::CurrentTime;
		using DataHVACGlobals::TimeStepSys;
		using DataHVACGlobals::SysTimeElapsed;
		using General::RoundSigDigits;
		using General::CreateSysTimeIntervalString;
		using DataPlant::PlantLoop;
		using DataPlant::TypeOf_Chiller_Electric;
		using DataPlant::CompSetPtBasedSchemeType;
		using DataPlant::CriteriaType_MassFlowRate;
		using DataPlant::SingleSetPoint;
		using DataPlant::DualSetPointDeadBand;
		using DataBranchAirLoopPlant::ControlType_SeriesActive;
		using DataBranchAirLoopPlant::MassFlowTolerance;
		using DataEnvironment::EnvironmentName;
		using DataEnvironment::CurMnDy;
		using FluidProperties::GetSpecificHeatGlycol;
		using PlantUtilities::SetComponentFlowRate;
		using PlantUtilities::PullCompInterconnectTrigger;
		using Psychrometrics::PsyCpAirFnWTdb;
		using Psychrometrics::PsyWFnTdbTwbPb;

		// Locals
		Real64 EvapInletTemp; // C - evaporator inlet temperature, water side
		Real64 CondInletTemp; // C - condenser inlet temperature, water side

		// SUBROUTINE ARGUMENT DEFINITIONS:
		//INTEGER, INTENT(IN)    :: FlowLock        ! TRUE when flow resolver has calculated branch flow

		// SUBROUTINE PARAMETER DEFINITIONS:

		static gio::Fmt OutputFormat( "(F6.2)" );
		static std::string const RoutineName( "CalcElectricChillerModel" );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 MinPartLoadRat; // min allowed operating frac full load
		Real64 MaxPartLoadRat; // max allowed operating frac full load
		Real64 TempCondInDesign; // C - (Electric ADJTC(1)The design secondary loop fluid
		Real64 TempRiseRat; // intermediate result:  temperature rise ratio
		Real64 TempEvapOut; // C - evaporator outlet temperature, water side
		Real64 TempEvapOutSetPoint( 0.0 ); // C - evaporator outlet temperature setpoint
		Real64 TempEvapOutDesign; // design evaporator outlet temperature, water side
		Real64 ChillerNomCap; // chiller nominal capacity
		Real64 AvailChillerCap; // chiller available capacity
		Real64 RatedCOP; // rated coefficient of performance, from user input
		Real64 FracFullLoadPower; // fraction of full load power
		Real64 EvapDeltaTemp( 0.0 ); // C - evaporator temperature difference, water side
		Real64 DeltaTemp; // C - intermediate result: condenser/evaporator temp diff
		Real64 AvailNomCapRat; // intermediate result: available nominal capacity ratio
		Real64 FullLoadPowerRat; // intermediate result: full load power ratio
		Real64 PartLoadRat; // part load ratio for efficiency calculation
		Real64 OperPartLoadRat; // Actual Operating PLR
		Real64 TempLowLimitEout; // C - Evaporator low temp. limit cut off
		Real64 EvapMassFlowRateMax; // Max Design Evaporator Mass Flow Rate converted from Volume Flow Rate
		int EvapInletNode; // evaporator inlet node number, water side
		int EvapOutletNode; // evaporator outlet node number, water side
		int CondInletNode; // condenser inlet node number, water side
		int CondOutletNode; // condenser outlet node number, water side
		Real64 FRAC;
		//  LOGICAL,SAVE           :: PossibleSubcooling=.FALSE.
		int PlantLoopNum;
		int LoopNum;
		int LoopSideNum;
		int BranchNum;
		int CompNum;
		static Real64 TimeStepSysLast( 0.0 ); // last system time step (used to check for downshifting)
		Real64 CurrentEndTime; // end time of time step for current simulation time step
		static Real64 CurrentEndTimeLast( 0.0 ); // end time of time step for last simulation time step
		static std::string OutputChar; // character string for warning messages
		Real64 Cp; // local for fluid specif heat, for evaporator
		Real64 CpCond; // local for fluid specif heat, for condenser

		//set module level inlet and outlet nodes
		this->EvapMassFlowRate = 0.0;
		this->CondMassFlowRate = 0.0;
		this->Power = 0.0;
		this->Energy = 0.0;
		this->QCondenser = 0.0;
		this->QEvaporator = 0.0;
		this->CondenserEnergy = 0.0;
		this->EvaporatorEnergy = 0.0;
		this->QHeatRecovered = 0.0;
		EvapInletNode = this->EvapInletNodeNum;
		EvapOutletNode = this->EvapOutletNodeNum;
		CondInletNode = this->CondInletNodeNum;
		CondOutletNode = this->CondOutletNodeNum;
		FRAC = 1.0;
		LoopNum = this->CWLoopNum;
		LoopSideNum = this->CWLoopSideNum;
		BranchNum = this->CWBranchNum;
		CompNum = this->CWCompNum;
		EvapInletTemp = Node( EvapInletNode ).Temp;

		//   calculate end time of current time step
		CurrentEndTime = CurrentTime + SysTimeElapsed;

		//   Print warning messages only when valid and only for the first ocurrance. Let summary provide statistics.
		//   Wait for next time step to print warnings. If simulation iterates, print out
		//   the warning for the last iteration only. Must wait for next time step to accomplish this.
		//   If a warning occurs and the simulation down shifts, the warning is not valid.
		if ( CurrentEndTime > CurrentEndTimeLast && TimeStepSys >= TimeStepSysLast ) {
			if ( this->PrintMessage ) {
				++this->MsgErrorCount;
				//       Show single warning and pass additional info to ShowRecurringWarningErrorAtEnd
				if ( this->MsgErrorCount < 2 ) {
					ShowWarningError( this->MsgBuffer1 + '.' );
					ShowContinueError( this->MsgBuffer2 );
				} else {
					ShowRecurringWarningErrorAtEnd( this->MsgBuffer1 + " error continues.", this->ErrCount1, this->MsgDataLast, this->MsgDataLast, _, "[C]", "[C]" );
				}
			}
		}

		//   save last system time step and last end time of current time step (used to determine if warning is valid)
		TimeStepSysLast = TimeStepSys;
		CurrentEndTimeLast = CurrentEndTime;

		//If no loop demand or chiller OFF, return
		//If Chiller load is 0 or chiller is not running then leave the subroutine.
		if ( MyLoad >= 0.0 || ! RunFlag ) {
			// call for zero flow before leaving
			if ( EquipFlowCtrl == ControlType_SeriesActive || PlantLoop( LoopNum ).LoopSide( LoopSideNum ).FlowLock == 1 ) {
				this->EvapMassFlowRate = Node( EvapInletNode ).MassFlowRate;
			} else {
				this->EvapMassFlowRate = 0.0;
				SetComponentFlowRate( this->EvapMassFlowRate, EvapInletNode, EvapOutletNode, this->CWLoopNum, this->CWLoopSideNum, this->CWBranchNum, this->CWCompNum );
			}
			if ( this->CondenserType == WaterCooled ) {
				if ( PlantLoop( this->CDLoopNum ).LoopSide( this->CDLoopSideNum ).Branch( this->CDBranchNum ).Comp( this->CDCompNum ).FlowCtrl == ControlType_SeriesActive ) {
					this->CondMassFlowRate = Node( CondInletNode ).MassFlowRate;
				} else {
					this->CondMassFlowRate = 0.0;
					SetComponentFlowRate( this->CondMassFlowRate, CondInletNode, CondOutletNode, this->CDLoopNum, this->CDLoopSideNum, this->CDBranchNum, this->CDCompNum );
				}
			}

			if ( this->CondenserType == EvapCooled ) {
				CalcBasinHeaterPower( this->BasinHeaterPowerFTempDiff, this->BasinHeaterSchedulePtr, this->BasinHeaterSetPointTemp, this->BasinHeaterPower );
			}
			this->PrintMessage = false;
			return;
		}

		// If not air or evap cooled then set to the condenser node that is attached to a cooling tower
		CondInletTemp = Node( CondInletNode ).Temp;

		//Set mass flow rates
		if ( this->CondenserType == WaterCooled ) {
			this->CondMassFlowRate = this->CondMassFlowRateMax;
			SetComponentFlowRate( this->CondMassFlowRate, CondInletNode, CondOutletNode, this->CDLoopNum, this->CDLoopSideNum, this->CDBranchNum, this->CDCompNum );
			PullCompInterconnectTrigger( this->CWLoopNum, this->CWLoopSideNum, this->CWBranchNum, this->CWCompNum, this->CondMassFlowIndex, this->CDLoopNum, this->CDLoopSideNum, CriteriaType_MassFlowRate, this->CondMassFlowRate );
			if ( this->CondMassFlowRate < MassFlowTolerance ) return;
		}

		//  LOAD LOCAL VARIABLES FROM DATA STRUCTURE (for code readability)
		auto const & CapacityRat( this->CapRatCoef );
		auto const & PowerRat( this->PowerRatCoef );
		auto const & FullLoadFactor( this->FullLoadCoef );
		MinPartLoadRat = this->MinPartLoadRat;
		PartLoadRat = MinPartLoadRat;
		MaxPartLoadRat = this->MaxPartLoadRat;
		TempCondInDesign = this->TempDesCondIn;
		TempRiseRat = this->TempRiseCoef;
		TempEvapOutDesign = this->TempDesEvapOut;
		ChillerNomCap = this->NomCap;
		RatedCOP = this->COP;
		TempEvapOut = Node( this->EvapOutletNodeNum ).Temp;
		TempLowLimitEout = this->TempLowLimitEvapOut;
		EvapMassFlowRateMax = this->EvapMassFlowRateMax;
		PlantLoopNum = this->CWLoopNum;

		LoopNum = this->CWLoopNum;
		LoopSideNum = this->CWLoopSideNum;

		// initialize outlet air humidity ratio of air or evap cooled chillers
		this->CondOutletHumRat = Node( CondInletNode ).HumRat;

		if ( this->CondenserType == AirCooled ) { //Condenser inlet temp = outdoor temp
			Node( CondInletNode ).Temp = Node( CondInletNode ).OutAirDryBulb;
			//  Warn user if entering condenser temperature falls below 0C
			if ( Node( CondInletNode ).Temp < 0.0 && ! WarmupFlag ) {
				this->PrintMessage = true;
				gio::write( OutputChar, OutputFormat ) << Node( CondInletNode ).Temp;
				this->MsgBuffer1 = "CalcElectricChillerModel - Chiller:Electric \"" + this->Name + "\" - Air Cooled Condenser Inlet Temperature below 0C";
				this->MsgBuffer2 = "... Outdoor Dry-bulb Condition = " + OutputChar + " C. Occurrence info = " + EnvironmentName + ", " + CurMnDy + ' ' + CreateSysTimeIntervalString();
				this->MsgDataLast = Node( CondInletNode ).Temp;
			} else {
				this->PrintMessage = false;
			}
		} else if ( this->CondenserType == EvapCooled ) { //Condenser inlet temp = (outdoor wet bulb)
			Node( CondInletNode ).Temp = Node( CondInletNode ).OutAirWetBulb;
			//  line above assumes evaporation pushes condenser inlet air humidity ratio to saturation
			this->CondOutletHumRat = PsyWFnTdbTwbPb( Node( CondInletNode ).Temp, Node( CondInletNode ).Temp, Node( CondInletNode ).Press );
			//  Warn user if evap condenser wet bulb temperature falls below 10C
			if ( Node( CondInletNode ).Temp < 10.0 && ! WarmupFlag ) {
				this->PrintMessage = true;
				gio::write( OutputChar, OutputFormat ) << Node( CondInletNode ).Temp;
				this->MsgBuffer1 = "CalcElectricChillerModel - Chiller:Electric \"" + this->Name + "\" - Evap Cooled Condenser Inlet Temperature below 10C";
				this->MsgBuffer2 = "... Outdoor Wet-bulb Condition = " + OutputChar + " C. Occurrence info = " + EnvironmentName + ", " + CurMnDy + ' ' + CreateSysTimeIntervalString();
				this->MsgDataLast = Node( CondInletNode ).Temp;
			} else {
				this->PrintMessage = false;
			}
		} // End of the Air Cooled/Evap Cooled Logic block

		CondInletTemp = Node( CondInletNode ).Temp;

		// correct inlet temperature if using heat recovery
		if ( this->HeatRecActive ) {
			if ( ( this->reports.QHeatRecovery + this->reports.QCond ) > 0.0 ) {
				this->AvgCondSinkTemp = ( this->reports.QHeatRecovery * this->reports.HeatRecInletTemp + this->reports.QCond * this->reports.CondInletTemp ) / ( this->reports.QHeatRecovery + this->reports.QCond );
			} else {
				this->AvgCondSinkTemp = CondInletTemp;
			}
		} else {
			this->AvgCondSinkTemp = CondInletTemp;
		}

		//Calculate chiller performance from this set of performance equations.
		//  from BLAST...Z=(TECONDW-ADJTC(1))/ADJTC(2)-(TLCHLRW-ADJTC(3))

		DeltaTemp = ( this->AvgCondSinkTemp - TempCondInDesign ) / TempRiseRat - ( TempEvapOut - TempEvapOutDesign );

		// model should have bounds on DeltaTemp and check them (also needs engineering ref content)
		//  from BLAST...RCAV=RCAVC(1)+RCAVC(2)*Z+RCAVC(3)*Z**2
		AvailNomCapRat = CapacityRat( 1 ) + CapacityRat( 2 ) * DeltaTemp + CapacityRat( 3 ) * pow_2( DeltaTemp );

		AvailChillerCap = ChillerNomCap * AvailNomCapRat;

		// from BLAST...G=ADJEC(1)+ADJEC(2)*RCAV+ADJEC(3)*RCAV**2.
		FullLoadPowerRat = PowerRat( 1 ) + PowerRat( 2 ) * AvailNomCapRat + PowerRat( 3 ) * pow_2( AvailNomCapRat );

		//  from BLAST...RCLOAD=AMAX1(MINCHFR(I,IPLCTR),AMIN1(CHLRLOAD(I)/CHLROCAP(I) &
		//         /RCAV,MAXCHFR(I,IPLCTR)))

		//Calculate the PLR. When there is Min PLR and the load is less than Min PLR then the Frac Full load Power
		//is calculated at Min PLR, while all other calculations are based on the actual PLR. So in that case once
		//FracFullLoadPower is calculated the PLR should be recalculated
		if ( AvailChillerCap > 0.0 ) {
			PartLoadRat = max( MinPartLoadRat, min( std::abs( MyLoad ) / AvailChillerCap, MaxPartLoadRat ) );
		}

		// from BLAST...RPOWER=RPWRC(1)+RPWRC(2)*RCLOAD+RPWRC(3)*RCLOAD**2
		FracFullLoadPower = FullLoadFactor( 1 ) + FullLoadFactor( 2 ) * PartLoadRat + FullLoadFactor( 3 ) * pow_2( PartLoadRat );

		//If the PLR is less than Min PLR calculate the actual PLR for calculations. The power will then adjust for
		//the cycling.
		if ( AvailChillerCap > 0.0 ) {
			if ( std::abs( MyLoad ) / AvailChillerCap < MinPartLoadRat ) {
				OperPartLoadRat = std::abs( MyLoad ) / AvailChillerCap;
			} else {
				OperPartLoadRat = PartLoadRat;
			}
		} else {
			OperPartLoadRat = 0.0;
		}

		Cp = GetSpecificHeatGlycol( PlantLoop( this->CWLoopNum ).FluidName, Node( EvapInletNode ).Temp, PlantLoop( this->CWLoopNum ).FluidIndex, RoutineName );

		// If FlowLock is True, the new resolved mdot is used to update Power, QEvap, Qcond, and
		// condenser side outlet temperature.
		if ( PlantLoop( LoopNum ).LoopSide( LoopSideNum ).FlowLock == 0 ) {

			//ElectricChiller(ChillNum)%PossibleSubcooling = .FALSE.
			//PossibleSubcooling = .NOT. PlantLoop(PlantLoopNum)%TempSetPtCtrl
			if ( PlantLoop( PlantLoopNum ).LoopSide( LoopSideNum ).Branch( BranchNum ).Comp( CompNum ).CurOpSchemeType == CompSetPtBasedSchemeType ) {
				this->PossibleSubcooling = false;
			} else {
				this->PossibleSubcooling = true;
			}
			this->QEvaporator = AvailChillerCap * OperPartLoadRat;
			if ( OperPartLoadRat < MinPartLoadRat ) {
				FRAC = min( 1.0, ( OperPartLoadRat / MinPartLoadRat ) );
			} else {
				FRAC = 1.0;
			}
			this->Power = FracFullLoadPower * FullLoadPowerRat * AvailChillerCap / RatedCOP * FRAC;

			// Either set the flow to the Constant value or caluclate the flow for the variable volume
			if ( ( this->FlowMode == ConstantFlow ) || ( this->FlowMode == NotModulated ) ) {

				// Start by assuming max (design) flow
				this->EvapMassFlowRate = EvapMassFlowRateMax;
				// Use SetComponentFlowRate to decide actual flow
				SetComponentFlowRate( this->EvapMassFlowRate, EvapInletNode, EvapOutletNode, this->CWLoopNum, this->CWLoopSideNum, this->CWBranchNum, this->CWCompNum );
				// Evaluate delta temp based on actual flow rate
				if ( this->EvapMassFlowRate != 0.0 ) {
					EvapDeltaTemp = this->QEvaporator / this->EvapMassFlowRate / Cp;
				} else {
					EvapDeltaTemp = 0.0;
				}
				// Evaluate outlet temp based on delta
				this->EvapOutletTemp = Node( EvapInletNode ).Temp - EvapDeltaTemp;

			} else if ( this->FlowMode == LeavingSetPointModulated ) {

				// Calculate the Delta Temp from the inlet temp to the chiller outlet setpoint
				{ auto const SELECT_CASE_var( PlantLoop( this->CWLoopNum ).LoopDemandCalcScheme );
				if ( SELECT_CASE_var == SingleSetPoint ) {
					EvapDeltaTemp = Node( EvapInletNode ).Temp - Node( EvapOutletNode ).TempSetPoint;
				} else if ( SELECT_CASE_var == DualSetPointDeadBand ) {
					EvapDeltaTemp = Node( EvapInletNode ).Temp - Node( EvapOutletNode ).TempSetPointHi;
				}}

				if ( EvapDeltaTemp != 0.0 ) {

					// Calculate desired flow to request based on load
					this->EvapMassFlowRate = std::abs( this->QEvaporator / Cp / EvapDeltaTemp );
					//Check to see if the Maximum is exceeded, if so set to maximum
					if ( ( this->EvapMassFlowRate - EvapMassFlowRateMax ) > MassFlowTolerance ) this->PossibleSubcooling = true;
					this->EvapMassFlowRate = min( EvapMassFlowRateMax, this->EvapMassFlowRate );
					// Use SetComponentFlowRate to decide actual flow
					SetComponentFlowRate( this->EvapMassFlowRate, EvapInletNode, EvapOutletNode, this->CWLoopNum, this->CWLoopSideNum, this->CWBranchNum, this->CWCompNum );
					{ auto const SELECT_CASE_var( PlantLoop( this->CWLoopNum ).LoopDemandCalcScheme );
					if ( SELECT_CASE_var == SingleSetPoint ) {
						this->EvapOutletTemp = Node( EvapOutletNode ).TempSetPoint;
					} else if ( SELECT_CASE_var == DualSetPointDeadBand ) {
						this->EvapOutletTemp = Node( EvapOutletNode ).TempSetPointHi;
					}}

				} else {

					// Try to request zero flow
					this->EvapMassFlowRate = 0.0;
					// Use SetComponentFlowRate to decide actual flow
					SetComponentFlowRate( this->EvapMassFlowRate, EvapInletNode, EvapOutletNode, this->CWLoopNum, this->CWLoopSideNum, this->CWBranchNum, this->CWCompNum );
					// No deltaT since component is not running
					this->EvapOutletTemp = Node( EvapInletNode ).Temp;

				}

			} //End of Constant Variable Flow If Block

		} else { // If FlowLock is True

			this->EvapMassFlowRate = Node( EvapInletNode ).MassFlowRate;
			SetComponentFlowRate( this->EvapMassFlowRate, EvapInletNode, EvapOutletNode, this->CWLoopNum, this->CWLoopSideNum, this->CWBranchNum, this->CWCompNum );

			//       Some other component set the flow to 0. No reason to continue with calculations.
			if ( this->EvapMassFlowRate == 0.0 ) {
				MyLoad = 0.0;
				if ( this->CondenserType == EvapCooled ) {
					CalcBasinHeaterPower( this->BasinHeaterPowerFTempDiff, this->BasinHeaterSchedulePtr, this->BasinHeaterSetPointTemp, this->BasinHeaterPower );
				}
				this->PrintMessage = false;
				return;
			}
			//Flow resolver might have given less flow or control scheme have provided more load, which may
			//result in subcooling.
			if ( this->PossibleSubcooling ) {
				this->QEvaporator = std::abs( MyLoad );
				EvapDeltaTemp = this->QEvaporator / this->EvapMassFlowRate / Cp;
				this->EvapOutletTemp = Node( EvapInletNode ).Temp - EvapDeltaTemp;
			} else { //No subcooling in this case.No recalculation required.Still need to check chiller low temp limit

				{ auto const SELECT_CASE_var( PlantLoop( LoopNum ).LoopDemandCalcScheme );
				if ( SELECT_CASE_var == SingleSetPoint ) {
					if ( ( this->FlowMode == LeavingSetPointModulated ) || ( PlantLoop( LoopNum ).LoopSide( LoopSideNum ).Branch( BranchNum ).Comp( CompNum ).CurOpSchemeType == CompSetPtBasedSchemeType ) || ( Node( EvapOutletNode ).TempSetPoint != SensedNodeFlagValue ) ) {
						TempEvapOutSetPoint = Node( EvapOutletNode ).TempSetPoint;
					} else {
						TempEvapOutSetPoint = Node( PlantLoop( LoopNum ).TempSetPointNodeNum ).TempSetPoint;
					}
				} else if ( SELECT_CASE_var == DualSetPointDeadBand ) {
					if ( ( this->FlowMode == LeavingSetPointModulated ) || ( PlantLoop( LoopNum ).LoopSide( LoopSideNum ).Branch( BranchNum ).Comp( CompNum ).CurOpSchemeType == CompSetPtBasedSchemeType ) || ( Node( EvapOutletNode ).TempSetPointHi != SensedNodeFlagValue ) ) {
						TempEvapOutSetPoint = Node( EvapOutletNode ).TempSetPointHi;
					} else {
						TempEvapOutSetPoint = Node( PlantLoop( LoopNum ).TempSetPointNodeNum ).TempSetPointHi;
					}
				}}
				EvapDeltaTemp = Node( EvapInletNode ).Temp - TempEvapOutSetPoint;
				this->QEvaporator = std::abs( this->EvapMassFlowRate * Cp * EvapDeltaTemp );
				this->EvapOutletTemp = TempEvapOutSetPoint;
			}
			//Check that the Evap outlet temp honors both plant loop temp low limit and also the chiller low limit
			if ( this->EvapOutletTemp < TempLowLimitEout ) {
				if ( ( Node( EvapInletNode ).Temp - TempLowLimitEout ) > DeltaTempTol ) {
					this->EvapOutletTemp = TempLowLimitEout;
					EvapDeltaTemp = Node( EvapInletNode ).Temp - this->EvapOutletTemp;
					this->QEvaporator = this->EvapMassFlowRate * Cp * EvapDeltaTemp;
				} else {
					this->EvapOutletTemp = Node( EvapInletNode ).Temp;
					EvapDeltaTemp = Node( EvapInletNode ).Temp - this->EvapOutletTemp;
					this->QEvaporator = this->EvapMassFlowRate * Cp * EvapDeltaTemp;
				}
			}
			if ( this->EvapOutletTemp < Node( EvapOutletNode ).TempMin ) {
				if ( ( Node( EvapInletNode ).Temp - Node( EvapOutletNode ).TempMin ) > DeltaTempTol ) {
					this->EvapOutletTemp = Node( EvapOutletNode ).TempMin;
					EvapDeltaTemp = Node( EvapInletNode ).Temp - this->EvapOutletTemp;
					this->QEvaporator = this->EvapMassFlowRate * Cp * EvapDeltaTemp;
				} else {
					this->EvapOutletTemp = Node( EvapInletNode ).Temp;
					EvapDeltaTemp = Node( EvapInletNode ).Temp - this->EvapOutletTemp;
					this->QEvaporator = this->EvapMassFlowRate * Cp * EvapDeltaTemp;
				}
			}

			// If load exceeds the distributed load set to the distributed load
			if ( this->QEvaporator > std::abs( MyLoad ) ) {
				if ( this->EvapMassFlowRate > MassFlowTolerance ) {
					this->QEvaporator = std::abs( MyLoad );
					EvapDeltaTemp = this->QEvaporator / this->EvapMassFlowRate / Cp;
					this->EvapOutletTemp = Node( EvapInletNode ).Temp - EvapDeltaTemp;
				} else {
					this->QEvaporator = 0.0;
					this->EvapOutletTemp = Node( EvapInletNode ).Temp;
				}
			}

			// Checks QEvaporator on the basis of the machine limits.
			if ( this->QEvaporator > ( AvailChillerCap * MaxPartLoadRat ) ) {
				if ( this->EvapMassFlowRate > MassFlowTolerance ) {
					this->QEvaporator = AvailChillerCap * OperPartLoadRat;
					EvapDeltaTemp = this->QEvaporator / this->EvapMassFlowRate / Cp;
					this->EvapOutletTemp = Node( EvapInletNode ).Temp - EvapDeltaTemp;
				} else {
					this->QEvaporator = 0.0;
					this->EvapOutletTemp = Node( EvapInletNode ).Temp;
				}
			}

			if ( OperPartLoadRat < MinPartLoadRat ) {
				FRAC = min( 1.0, ( OperPartLoadRat / MinPartLoadRat ) );
			} else {
				FRAC = 1.0;
			}

			// set the module level variable used for reporting FRAC
			this->ChillerCyclingRatio = FRAC;

			// Chiller is false loading below PLR = minimum unloading ratio, find PLR used for energy calculation
			this->Power = FracFullLoadPower * FullLoadPowerRat * AvailChillerCap / RatedCOP * FRAC;

			if ( this->EvapMassFlowRate == 0.0 ) {
				this->QEvaporator = 0.0;
				this->EvapOutletTemp = Node( EvapInletNode ).Temp;
				this->Power = 0.0;
				this->PrintMessage = false;
			}
			if ( this->QEvaporator == 0.0 && this->CondenserType == EvapCooled ) {
				CalcBasinHeaterPower( this->BasinHeaterPowerFTempDiff, this->BasinHeaterSchedulePtr, this->BasinHeaterSetPointTemp, this->BasinHeaterPower );
			}
		} //This is the end of the FlowLock Block

		//QCondenser is calculated the same for each type, but the power consumption should be different
		//  depending on the performance coefficients used for the chiller model.
		this->QCondenser = this->Power + this->QEvaporator;

		if ( this->CondenserType == WaterCooled ) {
			if ( this->CondMassFlowRate > MassFlowTolerance ) {
				// If Heat Recovery specified for this vapor compression chiller, then Qcondenser will be adjusted by this subroutine
				if ( this->HeatRecActive ) this->CalcElectricChillerHeatRecovery( this->QCondenser, this->CondMassFlowRate, CondInletTemp, this->QHeatRecovered );
				CpCond = GetSpecificHeatGlycol( PlantLoop( this->CDLoopNum ).FluidName, CondInletTemp, PlantLoop( this->CDLoopNum ).FluidIndex, RoutineName );
				this->CondOutletTemp = this->QCondenser / this->CondMassFlowRate / CpCond + CondInletTemp;
			} else {
				ShowSevereError( "CalcElectricChillerModel: Condenser flow = 0, for ElectricChiller=" + this->Name );
				ShowContinueErrorTimeStamp( "" );

			}
		} else { //Air Cooled or Evap Cooled

			if ( this->QCondenser > 0.0 ) {
				this->CondMassFlowRate = this->CondMassFlowRateMax * OperPartLoadRat;
			} else {
				this->CondMassFlowRate = 0.0;
			}

			// If Heat Recovery specified for this vapor compression chiller, then Qcondenser will be adjusted by this subroutine
			if ( this->HeatRecActive ) CalcElectricChillerHeatRecovery( this->QCondenser, this->CondMassFlowRate, CondInletTemp, this->QHeatRecovered );
			if ( this->CondMassFlowRate > 0.0 ) {
				CpCond = PsyCpAirFnWTdb( Node( CondInletNode ).HumRat, CondInletTemp );
				this->CondOutletTemp = CondInletTemp + this->QCondenser / this->CondMassFlowRate / CpCond;
			} else {
				this->CondOutletTemp = CondInletTemp;
			}
		}

		//Calculate Energy
		this->CondenserEnergy = this->QCondenser * TimeStepSys * SecInHour;
		this->Energy = this->Power * TimeStepSys * SecInHour;
		this->EvaporatorEnergy = this->QEvaporator * TimeStepSys * SecInHour;

		//check for problems BG 9/12/06 (deal with observed negative energy results)
		if ( this->Energy < 0.0 ) { // there is a serious problem

			if ( this->CondenserType == WaterCooled ) {
				// first check for run away condenser loop temps (only reason yet to be observed for this?)
				if ( CondInletTemp > 70.0 ) {
					ShowSevereError( "CalcElectricChillerModel: Condenser loop inlet temperatures over 70.0 C for ElectricChiller=" + this->Name );
					ShowContinueErrorTimeStamp( "" );
					ShowContinueError( "Condenser loop water temperatures are too high at" + RoundSigDigits( CondInletTemp, 2 ) );
					ShowContinueError( "Check input for condenser plant loop, especially cooling tower" );
					ShowContinueError( "Evaporator inlet temperature: " + RoundSigDigits( Node( EvapInletNode ).Temp, 2 ) );

					ShowFatalError( "Program Terminates due to previous error condition" );
				}
			}
			if ( ! WarmupFlag ) {
				if ( AvailNomCapRat < 0.0 ) { // apparently the real reason energy goes negative
					ShowSevereError( "CalcElectricChillerModel: Capacity ratio below zero for ElectricChiller=" + this->Name );
					ShowContinueErrorTimeStamp( "" );
					ShowContinueError( "Check input for Capacity Ratio Curve" );
					ShowContinueError( "Condenser inlet temperature: " + RoundSigDigits( CondInletTemp, 2 ) );
					ShowContinueError( "Evaporator inlet temperature: " + RoundSigDigits( Node( EvapInletNode ).Temp, 2 ) );
					ShowFatalError( "Program Terminates due to previous error condition" );
				}
			}
			// If makes it here, set limits, chiller can't have negative energy/power
			// proceeding silently for now but may want to throw error here
			this->Power = 0.0;
			this->Energy = 0.0;
		}
	}

	void
	EngineDrivenChillerSpecs::CalcEngineDrivenChillerModel(
		Real64 & MyLoad, // operating load
		bool const RunFlag, // TRUE when chiller operating
		int const EquipFlowCtrl // Flow control mode for the equipment
	)
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         Dan Fisher / Brandon Anderson
		//       DATE WRITTEN   Sept. 2000
		//       MODIFIED       Chandan Sharma, FSEC, February 2010, Added basin heater
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// simulate a vapor compression chiller using the EngineDriven model

		// METHODOLOGY EMPLOYED:
		// curve fit of performance data:

		// REFERENCES:
		// 1. BLAST Users Manual
		// 2. CHILLER User Manual

		// Using/Aliasing
		using DataGlobals::BeginEnvrnFlag;
		using DataGlobals::SecInHour;
		using DataGlobals::CurrentTime;
		using DataHVACGlobals::TimeStepSys;
		using DataHVACGlobals::SysTimeElapsed;
		using CurveManager::CurveValue;
		using General::RoundSigDigits;
		using General::CreateSysTimeIntervalString;
		using DataPlant::PlantLoop;
		using DataPlant::TypeOf_Chiller_EngineDriven;
		using DataPlant::CompSetPtBasedSchemeType;
		using DataPlant::CriteriaType_MassFlowRate;
		using DataPlant::SingleSetPoint;
		using DataPlant::DualSetPointDeadBand;
		using DataBranchAirLoopPlant::ControlType_SeriesActive;
		using DataBranchAirLoopPlant::MassFlowTolerance;
		using DataEnvironment::EnvironmentName;
		using DataEnvironment::CurMnDy;
		using FluidProperties::GetSpecificHeatGlycol;
		using PlantUtilities::SetComponentFlowRate;
		using PlantUtilities::PullCompInterconnectTrigger;

		// Locals
		Real64 EvapInletTemp; // C - evaporator inlet temperature, water side
		Real64 CondInletTemp; // C - condenser inlet temperature, water side

		// SUBROUTINE ARGUMENT DEFINITIONS:
		// INTEGER, INTENT(IN)    :: FlowLock        ! TRUE when flow resolver has calculated branch flow

		// SUBROUTINE PARAMETER DEFINITIONS:

		Real64 const ExhaustCP( 1.047 ); // Exhaust Gas Specific Heat (J/kg-K)
		Real64 const ReferenceTemp( 25.0 ); // Reference temperature by which lower heating
		// value is reported.  This should be subtracted
		// off of when calculated exhaust energies.
		static gio::Fmt OutputFormat( "(F6.2)" );
		static std::string const RoutineName( "CalcEngineDrivenChillerModel" );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 MinPartLoadRat; // min allowed operating frac full load
		Real64 MaxPartLoadRat; // max allowed operating frac full load
		Real64 TempCondIn; // C - (EngineDriven ADJTC(1)The design secondary loop fluid
		Real64 TempCondInDesign; // C - (EngineDriven ADJTC(1)The design secondary loop fluid
		Real64 TempRiseRat; // intermediate result:  temperature rise ratio
		Real64 TempEvapOut; // C - evaporator outlet temperature, water side
		Real64 TempEvapOutSetPoint( 0.0 ); // C - evaporator outlet temperature setpoint
		Real64 TempEvapOutDesign; // design evaporator outlet temperature, water side
		Real64 ChillerNomCap; // chiller nominal capacity
		Real64 AvailChillerCap; // chiller available capacity
		Real64 COP; // coefficient of performance
		Real64 FracFullLoadPower; // fraction of full load power
		Real64 EvapDeltaTemp( 0.0 ); // C - evaporator temperature difference, water side
		Real64 DeltaTemp; // C - intermediate result: condenser/evaporator temp diff
		Real64 AvailNomCapRat; // intermediate result: available nominal capacity ratio
		Real64 FullLoadPowerRat; // intermediate result: full load power ratio
		Real64 PartLoadRat( 0.0 ); // part load ratio for efficiency
		Real64 OperPartLoadRat; // Actual operating PLR
		int EvapInletNode; // evaporator inlet node number, water side
		int EvapOutletNode; // evaporator outlet node number, water side
		int CondInletNode; // condenser inlet node number, water side
		int CondOutletNode; // condenser outlet node number, water side
		Real64 EvapMassFlowRateMax; // Max Design Evaporator Mass Flow Rate converted from Volume Flow Rate
		Real64 TempLowLimitEout; // C - Evaporator low temp. limit cut off
		Real64 FRAC;
		int LoopNum;
		int LoopSideNum;
		static Real64 TimeStepSysLast( 0.0 ); // last system time step (used to check for downshifting)
		Real64 CurrentEndTime; // end time of time step for current simulation time step
		static Real64 CurrentEndTimeLast( 0.0 ); // end time of time step for last simulation time step
		static std::string OutputChar; // character string for warning messages
		Real64 Cp; // local for fluid specif heat, for evaporator
		Real64 CpCond; // local for fluid specif heat, for condenser

		// Special variables for EngineDriven Chiller
		Real64 MaxExhaustperPowerOutput; // curve fit parameter
		Real64 ClngLoadFuelRat; // (RELDC) Ratio of Shaft Power to Fuel Energy Input
		Real64 RecJacHeattoFuelRat; // (RJACDC) Ratio of Recoverable Jacket Heat to Fuel Energy Input
		Real64 RecLubeHeattoFuelRat; // (RLUBDC) Ratio of Recoverable Lube Oil Heat to Fuel Energy Input
		Real64 TotExhausttoFuelRat; // (REXDC) Total Exhaust Energy Input to Fuel Energy Input
		Real64 TotalExhaustEnergy;
		Real64 ExhaustTemp; // (TEX) Exhaust Gas Temp
		Real64 ExhaustGasFlow; // exhaust gas mass flow rate
		Real64 DesignMinExitGasTemp;
		Real64 UA; // (UACDC) exhaust gas Heat Exchanger UA
		Real64 HeatRecCp; // Specific Heat of the Heat Recovery Fluid (J/kg-K)
		Real64 EngineDrivenFuelEnergy;
		Real64 HeatRecRatio; // When Max Temp is reached the amount of recovered heat has to be reduced.
		//  LOGICAL,SAVE :: PossibleSubcooling=.FALSE.

		//set module level inlet and outlet nodes
		this->EvapMassFlowRate = 0.0;
		this->CondMassFlowRate = 0.0;
		this->Power = 0.0;
		this->QCondenser = 0.0;
		this->QEvaporator = 0.0;
		this->Energy = 0.0;
		this->CondenserEnergy = 0.0;
		this->EvaporatorEnergy = 0.0;
		HeatRecCp = 0.0;
		this->HeatRecMdotActual = 0.0;
		this->QTotalHeatRecovered = 0.0;
		this->QJacketRecovered = 0.0;
		this->QLubeOilRecovered = 0.0;
		this->QExhaustRecovered = 0.0;
		EngineDrivenFuelEnergy = 0.0;
		this->FuelEnergyUseRate = 0.0;
		this->TotalHeatEnergyRec = 0.0;
		this->JacketEnergyRec = 0.0;
		this->LubeOilEnergyRec = 0.0;
		this->ExhaustEnergyRec = 0.0;
		this->FuelEnergy = 0.0;
		this->FuelMdot = 0.0;
		this->ExhaustStackTemp = 0.0;
		FRAC = 1.0;

		if ( this->HeatRecActive ) {
			this->HeatRecInletTemp = Node( this->HeatRecInletNodeNum ).Temp;
			this->HeatRecOutletTemp = Node( this->HeatRecInletNodeNum ).Temp;
			this->HeatRecMdotDesign = this->DesignHeatRecMassFlowRate;
		}

		EvapInletNode = this->EvapInletNodeNum;
		EvapOutletNode = this->EvapOutletNodeNum;
		CondInletNode = this->CondInletNodeNum;
		CondOutletNode = this->CondOutletNodeNum;
		LoopNum = this->CWLoopNum;
		LoopSideNum = this->CWLoopSideNum;
		EvapInletTemp = Node( EvapInletNode ).Temp;

		//   calculate end time of current time step
		CurrentEndTime = CurrentTime + SysTimeElapsed;

		//   Print warning messages only when valid and only for the first ocurrance. Let summary provide statistics.
		//   Wait for next time step to print warnings. If simulation iterates, print out
		//   the warning for the last iteration only. Must wait for next time step to accomplish this.
		//   If a warning occurs and the simulation down shifts, the warning is not valid.
		if ( CurrentEndTime > CurrentEndTimeLast && TimeStepSys >= TimeStepSysLast ) {
			if ( this->PrintMessage ) {
				++this->MsgErrorCount;
				//     Show single warning and pass additional info to ShowRecurringWarningErrorAtEnd
				if ( this->MsgErrorCount < 2 ) {
					ShowWarningError( this->MsgBuffer1 + '.' );
					ShowContinueError( this->MsgBuffer2 );
				} else {
					ShowRecurringWarningErrorAtEnd( this->MsgBuffer1 + " error continues.", this->ErrCount1, this->MsgDataLast, this->MsgDataLast, _, "[C]", "[C]" );
				}
			}
		}

		// save last system time step and last end time of current time step (used to determine if warning is valid)
		TimeStepSysLast = TimeStepSys;
		CurrentEndTimeLast = CurrentEndTime;

		//If Chiller load is 0 or chiller is not running then leave the subroutine.
		if ( MyLoad >= 0.0 || ! RunFlag ) {
			if ( EquipFlowCtrl == ControlType_SeriesActive || PlantLoop( LoopNum ).LoopSide( LoopSideNum ).FlowLock == 1 ) {
				this->EvapMassFlowRate = Node( EvapInletNode ).MassFlowRate;
			} else {
				this->EvapMassFlowRate = 0.0;

				SetComponentFlowRate( this->EvapMassFlowRate, EvapInletNode, EvapOutletNode, this->CWLoopNum, this->CWLoopSideNum, this->CWBranchNum, this->CWCompNum );
			}

			if ( this->CondenserType == WaterCooled ) {
				if ( PlantLoop( this->CDLoopNum ).LoopSide( this->CDLoopSideNum ).Branch( this->CDBranchNum ).Comp( this->CDCompNum ).FlowCtrl == ControlType_SeriesActive ) {
					this->CondMassFlowRate = Node( CondInletNode ).MassFlowRate;
				} else {
					this->CondMassFlowRate = 0.0;
					SetComponentFlowRate( this->CondMassFlowRate, CondInletNode, CondOutletNode, this->CDLoopNum, this->CDLoopSideNum, this->CDBranchNum, this->CDCompNum );
				}
			}

			if ( this->CondenserType == EvapCooled ) {
				CalcBasinHeaterPower( this->BasinHeaterPowerFTempDiff, this->BasinHeaterSchedulePtr, this->BasinHeaterSetPointTemp, this->BasinHeaterPower );
			}
			this->PrintMessage = false;
			return;
		}

		if ( this->CondenserType == AirCooled ) { //Condenser inlet temp = outdoor temp
			Node( CondInletNode ).Temp = Node( CondInletNode ).OutAirDryBulb;
			//  Warn user if entering condenser temperature falls below 0C
			if ( Node( CondInletNode ).Temp < 0.0 && ! WarmupFlag ) {
				this->PrintMessage = true;
				gio::write( OutputChar, OutputFormat ) << Node( CondInletNode ).Temp;
				this->MsgBuffer1 = "CalcEngineDrivenChillerModel - Chiller:EngineDriven \"" + this->Name + "\" - Air Cooled Condenser Inlet Temperature below 0C";
				this->MsgBuffer2 = "... Outdoor Dry-bulb Condition = " + OutputChar + " C. Occurrence info = " + EnvironmentName + ", " + CurMnDy + ' ' + CreateSysTimeIntervalString();
				this->MsgDataLast = Node( CondInletNode ).Temp;
			} else {
				this->PrintMessage = false;
			}
		} else if ( this->CondenserType == EvapCooled ) { //Condenser inlet temp = (outdoor wet bulb)
			Node( CondInletNode ).Temp = Node( CondInletNode ).OutAirWetBulb;
			//  Warn user if evap condenser wet bulb temperature falls below 10C
			if ( Node( CondInletNode ).Temp < 10.0 && ! WarmupFlag ) {
				this->PrintMessage = true;
				gio::write( OutputChar, OutputFormat ) << Node( CondInletNode ).Temp;
				this->MsgBuffer1 = "CalcEngineDrivenChillerModel - Chiller:EngineDriven \"" + this->Name + "\" - Evap Cooled Condenser Inlet Temperature below 10C";
				this->MsgBuffer2 = "... Outdoor Wet-bulb Condition = " + OutputChar + " C. Occurrence info = " + EnvironmentName + ", " + CurMnDy + ' ' + CreateSysTimeIntervalString();
				this->MsgDataLast = Node( CondInletNode ).Temp;
			} else {
				this->PrintMessage = false;
			}
		} // End of the Air Cooled/Evap Cooled Logic block

		// If not air or evap cooled then set to the condenser node that is attached to a cooling tower
		CondInletTemp = Node( CondInletNode ).Temp;

		//Set mass flow rates
		if ( this->CondenserType == WaterCooled ) {
			this->CondMassFlowRate = this->CondMassFlowRateMax;
			SetComponentFlowRate( this->CondMassFlowRate, CondInletNode, CondOutletNode, this->CDLoopNum, this->CDLoopSideNum, this->CDBranchNum, this->CDCompNum );
			PullCompInterconnectTrigger( this->CWLoopNum, this->CWLoopSideNum, this->CWBranchNum, this->CWCompNum, this->CondMassFlowIndex, this->CDLoopNum, this->CDLoopSideNum, CriteriaType_MassFlowRate, this->CondMassFlowRate );
			if ( this->CondMassFlowRate < MassFlowTolerance ) return;

		}

		//  LOAD LOCAL VARIABLES FROM DATA STRUCTURE (for code readability)
		auto const & CapacityRat( this->CapRatCoef );
		auto const & PowerRat( this->PowerRatCoef );
		auto const & FullLoadFactor( this->FullLoadCoef );
		MinPartLoadRat = this->MinPartLoadRat;
		MaxPartLoadRat = this->MaxPartLoadRat;
		TempCondInDesign = this->TempDesCondIn;
		TempRiseRat = this->TempRiseCoef;
		TempEvapOutDesign = this->TempDesEvapOut;
		ChillerNomCap = this->NomCap;
		COP = this->COP;
		TempCondIn = Node( this->CondInletNodeNum ).Temp;
		TempEvapOut = Node( this->EvapOutletNodeNum ).Temp;
		TempLowLimitEout = this->TempLowLimitEvapOut;
		MaxExhaustperPowerOutput = this->MaxExhaustperPowerOutput;
		LoopNum = this->CWLoopNum;
		LoopSideNum = this->CWLoopSideNum;
		EvapMassFlowRateMax = this->EvapMassFlowRateMax;

		//*********************************

		//Calculate chiller performance from this set of performance equations.
		//  from BLAST...Z=(TECONDW-ADJTC(1))/ADJTC(2)-(TLCHLRW-ADJTC(3))
		DeltaTemp = ( TempCondIn - TempCondInDesign ) / TempRiseRat - ( TempEvapOut - TempEvapOutDesign );

		//  from BLAST...RCAV=RCAVC(1)+RCAVC(2)*Z+RCAVC(3)*Z**2
		AvailNomCapRat = CapacityRat( 1 ) + CapacityRat( 2 ) * DeltaTemp + CapacityRat( 3 ) * pow_2( DeltaTemp );

		AvailChillerCap = ChillerNomCap * AvailNomCapRat;

		// from BLAST...G=ADJEC(1)+ADJEC(2)*RCAV+ADJEC(3)*RCAV**2.
		FullLoadPowerRat = PowerRat( 1 ) + PowerRat( 2 ) * AvailNomCapRat + PowerRat( 3 ) * pow_2( AvailNomCapRat );

		//  from BLAST...RCLOAD=AMAX1(MINCHFR(I,IPLCTR),AMIN1(CHLRLOAD(I)/CHLROCAP(I) &
		//         /RCAV,MAXCHFR(I,IPLCTR)))
		if ( AvailChillerCap > 0.0 ) {
			PartLoadRat = max( MinPartLoadRat, min( std::abs( MyLoad ) / AvailChillerCap, MaxPartLoadRat ) );
		}
		// from BLAST...RPOWER=RPWRC(1)+RPWRC(2)*RCLOAD+RPWRC(3)*RCLOAD**2
		FracFullLoadPower = FullLoadFactor( 1 ) + FullLoadFactor( 2 ) * PartLoadRat + FullLoadFactor( 3 ) * pow_2( PartLoadRat );

		if ( AvailChillerCap > 0.0 ) {
			if ( std::abs( MyLoad ) / AvailChillerCap < MinPartLoadRat ) {
				OperPartLoadRat = std::abs( MyLoad ) / AvailChillerCap;
			} else {
				OperPartLoadRat = PartLoadRat;
			}
		} else {
			OperPartLoadRat = 0.0;
		}
		//*********************************
		Cp = GetSpecificHeatGlycol( PlantLoop( this->CWLoopNum ).FluidName, Node( EvapInletNode ).Temp, PlantLoop( this->CWLoopNum ).FluidIndex, RoutineName );

		// If FlowLock is True, the new resolved mdot is used to update Power, QEvap, Qcond, and
		// condenser side outlet temperature.
		if ( PlantLoop( LoopNum ).LoopSide( LoopSideNum ).FlowLock == 0 ) {
			this->PossibleSubcooling = false;
			this->QEvaporator = AvailChillerCap * OperPartLoadRat;
			if ( OperPartLoadRat < MinPartLoadRat ) {
				FRAC = min( 1.0, ( OperPartLoadRat / MinPartLoadRat ) );
			} else {
				FRAC = 1.0;
			}
			this->Power = FracFullLoadPower * FullLoadPowerRat * AvailChillerCap / COP * FRAC;

			// Either set the flow to the Constant value or caluclate the flow for the variable volume
			if ( ( this->FlowMode == ConstantFlow ) || ( this->FlowMode == NotModulated ) ) {
				// Start by assuming max (design) flow
				this->EvapMassFlowRate = EvapMassFlowRateMax;
				// Use SetComponentFlowRate to decide actual flow
				SetComponentFlowRate( this->EvapMassFlowRate, EvapInletNode, EvapOutletNode, this->CWLoopNum, this->CWLoopSideNum, this->CWBranchNum, this->CWCompNum );
				// Evaluate delta temp based on actual flow rate
				if ( this->EvapMassFlowRate != 0.0 ) {
					EvapDeltaTemp = this->QEvaporator / this->EvapMassFlowRate / Cp;
				} else {
					EvapDeltaTemp = 0.0;
				}
				// Evaluate outlet temp based on delta
				this->EvapOutletTemp = Node( EvapInletNode ).Temp - EvapDeltaTemp;

			} else if ( this->FlowMode == LeavingSetPointModulated ) {

				// Calculate the Delta Temp from the inlet temp to the chiller outlet setpoint
				{ auto const SELECT_CASE_var( PlantLoop( this->CWLoopNum ).LoopDemandCalcScheme );
				if ( SELECT_CASE_var == SingleSetPoint ) {
					EvapDeltaTemp = Node( EvapInletNode ).Temp - Node( EvapOutletNode ).TempSetPoint;
				} else if ( SELECT_CASE_var == DualSetPointDeadBand ) {
					EvapDeltaTemp = Node( EvapInletNode ).Temp - Node( EvapOutletNode ).TempSetPointHi;
				}}

				if ( EvapDeltaTemp != 0.0 ) {
					this->EvapMassFlowRate = std::abs( this->QEvaporator / Cp / EvapDeltaTemp );
					if ( ( this->EvapMassFlowRate - EvapMassFlowRateMax ) > MassFlowTolerance ) this->PossibleSubcooling = true;
					//Check to see if the Maximum is exceeded, if so set to maximum
					this->EvapMassFlowRate = min( EvapMassFlowRateMax, this->EvapMassFlowRate );
					// Use SetComponentFlowRate to decide actual flow
					SetComponentFlowRate( this->EvapMassFlowRate, EvapInletNode, EvapOutletNode, this->CWLoopNum, this->CWLoopSideNum, this->CWBranchNum, this->CWCompNum );
					{ auto const SELECT_CASE_var( PlantLoop( this->CWLoopNum ).LoopDemandCalcScheme );
					if ( SELECT_CASE_var == SingleSetPoint ) {
						this->EvapOutletTemp = Node( EvapOutletNode ).TempSetPoint;
					} else if ( SELECT_CASE_var == DualSetPointDeadBand ) {
						this->EvapOutletTemp = Node( EvapOutletNode ).TempSetPointHi;
					}}
				} else {
					// Try to request zero flow
					this->EvapMassFlowRate = 0.0;
					// Use SetComponentFlowRate to decide actual flow
					SetComponentFlowRate( this->EvapMassFlowRate, EvapInletNode, EvapOutletNode, this->CWLoopNum, this->CWLoopSideNum, this->CWBranchNum, this->CWCompNum );
					// No deltaT since component is not running
					this->EvapOutletTemp = Node( EvapInletNode ).Temp;

				}
			} //End of Constant Variable Flow If Block
		} else { // If FlowLock is True

			this->EvapMassFlowRate = Node( EvapInletNode ).MassFlowRate;
			SetComponentFlowRate( this->EvapMassFlowRate, EvapInletNode, EvapOutletNode, this->CWLoopNum, this->CWLoopSideNum, this->CWBranchNum, this->CWCompNum );
			// Some other component set the flow to 0. No reason to continue with calculations.
			if ( this->EvapMassFlowRate == 0.0 ) {
				MyLoad = 0.0;
				if ( this->CondenserType == EvapCooled ) {
					CalcBasinHeaterPower( this->BasinHeaterPowerFTempDiff, this->BasinHeaterSchedulePtr, this->BasinHeaterSetPointTemp, this->BasinHeaterPower );
				}
				this->PrintMessage = false;
				return;
			}

			if ( this->PossibleSubcooling ) {
				this->QEvaporator = std::abs( MyLoad );
				EvapDeltaTemp = this->QEvaporator / this->EvapMassFlowRate / Cp;
				this->EvapOutletTemp = Node( EvapInletNode ).Temp - EvapDeltaTemp;
				if ( this->EvapOutletTemp < Node( EvapOutletNode ).TempMin ) {
					this->EvapOutletTemp = Node( EvapOutletNode ).TempMin;
					EvapDeltaTemp = Node( EvapInletNode ).Temp - this->EvapOutletTemp;
					this->QEvaporator = std::abs( this->EvapMassFlowRate * Cp * EvapDeltaTemp );
				}
			} else { //No subcooling in this case.No recalculation required.Still need to check chiller low temp limit

				{ auto const SELECT_CASE_var( PlantLoop( LoopNum ).LoopDemandCalcScheme );
				if ( SELECT_CASE_var == SingleSetPoint ) {
					if ( ( this->FlowMode == LeavingSetPointModulated ) || ( PlantLoop( LoopNum ).LoopSide( LoopSideNum ).Branch( this->CWBranchNum ).Comp( this->CWCompNum ).CurOpSchemeType == CompSetPtBasedSchemeType ) || ( Node( EvapOutletNode ).TempSetPoint != SensedNodeFlagValue ) ) {
						TempEvapOutSetPoint = Node( EvapOutletNode ).TempSetPoint;
					} else {
						TempEvapOutSetPoint = Node( PlantLoop( LoopNum ).TempSetPointNodeNum ).TempSetPoint;
					}
				} else if ( SELECT_CASE_var == DualSetPointDeadBand ) {
					if ( ( this->FlowMode == LeavingSetPointModulated ) || ( PlantLoop( LoopNum ).LoopSide( LoopSideNum ).Branch( this->CWBranchNum ).Comp( this->CWCompNum ).CurOpSchemeType == CompSetPtBasedSchemeType ) || ( Node( EvapOutletNode ).TempSetPointHi != SensedNodeFlagValue ) ) {
						TempEvapOutSetPoint = Node( EvapOutletNode ).TempSetPointHi;
					} else {
						TempEvapOutSetPoint = Node( PlantLoop( LoopNum ).TempSetPointNodeNum ).TempSetPointHi;
					}
				}}
				EvapDeltaTemp = Node( EvapInletNode ).Temp - TempEvapOutSetPoint;
				this->QEvaporator = std::abs( this->EvapMassFlowRate * Cp * EvapDeltaTemp );
				this->EvapOutletTemp = TempEvapOutSetPoint;
			}
			//Check that the Evap outlet temp honors both plant loop temp low limit and also the chiller low limit
			if ( this->EvapOutletTemp < TempLowLimitEout ) {
				if ( ( Node( EvapInletNode ).Temp - TempLowLimitEout ) > DeltaTempTol ) {
					this->EvapOutletTemp = TempLowLimitEout;
					EvapDeltaTemp = Node( EvapInletNode ).Temp - this->EvapOutletTemp;
					this->QEvaporator = this->EvapMassFlowRate * Cp * EvapDeltaTemp;
				} else {
					this->EvapOutletTemp = Node( EvapInletNode ).Temp;
					EvapDeltaTemp = Node( EvapInletNode ).Temp - this->EvapOutletTemp;
					this->QEvaporator = this->EvapMassFlowRate * Cp * EvapDeltaTemp;
				}
			}
			if ( this->EvapOutletTemp < Node( EvapOutletNode ).TempMin ) {
				if ( ( Node( EvapInletNode ).Temp - Node( EvapOutletNode ).TempMin ) > DeltaTempTol ) {
					this->EvapOutletTemp = Node( EvapOutletNode ).TempMin;
					EvapDeltaTemp = Node( EvapInletNode ).Temp - this->EvapOutletTemp;
					this->QEvaporator = this->EvapMassFlowRate * Cp * EvapDeltaTemp;
				} else {
					this->EvapOutletTemp = Node( EvapInletNode ).Temp;
					EvapDeltaTemp = Node( EvapInletNode ).Temp - this->EvapOutletTemp;
					this->QEvaporator = this->EvapMassFlowRate * Cp * EvapDeltaTemp;
				}
			}
			// If load exceeds the distributed load set to the distributed load
			if ( this->QEvaporator > std::abs( MyLoad ) ) {
				if ( this->EvapMassFlowRate > MassFlowTolerance ) {
					this->QEvaporator = std::abs( MyLoad );
					EvapDeltaTemp = this->QEvaporator / this->EvapMassFlowRate / Cp;
					this->EvapOutletTemp = Node( EvapInletNode ).Temp - EvapDeltaTemp;
				} else {
					this->QEvaporator = 0.0;
					this->EvapOutletTemp = Node( EvapInletNode ).Temp;
				}
			}

			// Checks QEvaporator on the basis of the machine limits.
			if ( this->QEvaporator > ( AvailChillerCap * MaxPartLoadRat ) ) {
				if ( this->EvapMassFlowRate > MassFlowTolerance ) {
					this->QEvaporator = AvailChillerCap * OperPartLoadRat;
					EvapDeltaTemp = this->QEvaporator / this->EvapMassFlowRate / Cp;
					this->EvapOutletTemp = Node( EvapInletNode ).Temp - EvapDeltaTemp;
				} else {
					this->QEvaporator = 0.0;
					this->EvapOutletTemp = Node( EvapInletNode ).Temp;
				}
			}

			if ( OperPartLoadRat < MinPartLoadRat ) {
				FRAC = min( 1.0, ( OperPartLoadRat / MinPartLoadRat ) );
			} else {
				FRAC = 1.0;
			}

			// set the module level variable used for reporting FRAC
			this->ChillerCyclingRatio = FRAC;

			// Chiller is false loading below PLR = minimum unloading ratio, find PLR used for energy calculation
			this->Power = FracFullLoadPower * FullLoadPowerRat * AvailChillerCap / COP * FRAC;

			if ( this->EvapMassFlowRate == 0.0 ) {
				this->QEvaporator = 0.0;
				this->EvapOutletTemp = Node( EvapInletNode ).Temp;
				this->Power = 0.0;
				this->PrintMessage = false;
			}
			if ( this->QEvaporator == 0.0 && this->CondenserType == EvapCooled ) {
				CalcBasinHeaterPower( this->BasinHeaterPowerFTempDiff, this->BasinHeaterSchedulePtr, this->BasinHeaterSetPointTemp, this->BasinHeaterPower );
			}
		} //This is the end of the FlowLock Block

		//Now determine Cooling
		//QCondenser is calculated the same for each type, but the power consumption should be different
		//  depending on the performance coefficients used for the chiller model.
		this->QCondenser = this->Power + this->QEvaporator;

		if ( this->CondenserType == WaterCooled ) {

			if ( this->CondMassFlowRate > MassFlowTolerance ) {
				CpCond = GetSpecificHeatGlycol( PlantLoop( this->CDLoopNum ).FluidName, CondInletTemp, PlantLoop( this->CDLoopNum ).FluidIndex, RoutineName );
				this->CondOutletTemp = this->QCondenser / this->CondMassFlowRate / CpCond + CondInletTemp;
			} else {
				ShowSevereError( "CalcEngineDrivenChillerModel: Condenser flow = 0, for EngineDrivenChiller=" + this->Name );
				ShowContinueErrorTimeStamp( "" );
			}

		} else { //Air Cooled or Evap Cooled

			//don't care about outlet temp for Air-Cooled or Evap Cooled
			this->CondOutletTemp = CondInletTemp;
		}

		// EngineDriven Portion of the Engine Driven Chiller:

		//DETERMINE FUEL CONSUMED AND AVAILABLE WASTE HEAT

		//Use Curve fit to determine Fuel Energy Input.  For electric power generated in Watts, the fuel
		//energy input is calculated in J/s.  The PLBasedFuelInputCurve selects ratio of fuel flow (J/s)/cooling load (J/s).
		if ( PartLoadRat == 0 ) {
			EngineDrivenFuelEnergy = 0.0;
		} else {
			PartLoadRat = max( MinPartLoadRat, PartLoadRat );
			ClngLoadFuelRat = CurveValue( this->ClngLoadtoFuelCurve, PartLoadRat );
			EngineDrivenFuelEnergy = this->QEvaporator / ClngLoadFuelRat;
		}
		//Use Curve fit to determine energy recovered in the water jacket.  This curve calculates the water jacket energy recovered (J/s) by
		//multiplying the total fuel input (J/s) by the fraction of that power that could be recovered in the water jacket at that
		//particular part load.

		RecJacHeattoFuelRat = CurveValue( this->RecJacHeattoFuelCurve, PartLoadRat );
		this->QJacketRecovered = EngineDrivenFuelEnergy * RecJacHeattoFuelRat;

		//Use Curve fit to determine Heat Recovered Lubricant Energy.  This curve calculates the lube energy recovered (J/s) by
		//multiplying the total fuel input (J/s) by the fraction of that power that could be recovered in the lube oil at that
		//particular part load.
		RecLubeHeattoFuelRat = CurveValue( this->RecLubeHeattoFuelCurve, PartLoadRat );
		this->QLubeOilRecovered = EngineDrivenFuelEnergy * RecLubeHeattoFuelRat;

		//Use Curve fit to determine Heat Recovered from the exhaust.  This curve calculates the  energy recovered (J/s) by
		//multiplying the total fuel input (J/s) by the fraction of that power that could be recovered in the exhaust at that
		//particular part load.
		TotExhausttoFuelRat = CurveValue( this->TotExhausttoFuelCurve, PartLoadRat );
		TotalExhaustEnergy = EngineDrivenFuelEnergy * TotExhausttoFuelRat;

		//Use Curve fit to determine Exhaust Temperature in C.  The temperature is simply a curve fit
		//of the exhaust temperature in C to the part load ratio.
		if ( PartLoadRat != 0 ) {
			ExhaustTemp = CurveValue( this->ExhaustTempCurve, PartLoadRat );
			ExhaustGasFlow = TotalExhaustEnergy / ( ExhaustCP * ( ExhaustTemp - ReferenceTemp ) );

			//Use Curve fit to determine stack temp after heat recovery
			UA = this->UACoef( 1 ) * std::pow( ChillerNomCap, this->UACoef( 2 ) );

			DesignMinExitGasTemp = this->DesignMinExitGasTemp;
			this->ExhaustStackTemp = DesignMinExitGasTemp + ( ExhaustTemp - DesignMinExitGasTemp ) / std::exp( UA / ( max( ExhaustGasFlow, MaxExhaustperPowerOutput * ChillerNomCap ) * ExhaustCP ) );

			this->QExhaustRecovered = max( ExhaustGasFlow * ExhaustCP * ( ExhaustTemp - this->ExhaustStackTemp ), 0.0 );
		} else {
			this->QExhaustRecovered = 0.0;
		}

		this->QTotalHeatRecovered = this->QExhaustRecovered + this->QLubeOilRecovered + this->QJacketRecovered;

		//Update Heat Recovery temperatures
		if ( this->HeatRecActive ) {
			CalcEngineChillerHeatRec( this->QTotalHeatRecovered, HeatRecRatio );
			this->QExhaustRecovered *= HeatRecRatio;
			this->QLubeOilRecovered *= HeatRecRatio;
			this->QJacketRecovered *= HeatRecRatio;

		}

		//Calculate Energy
		this->CondenserEnergy = this->QCondenser * TimeStepSys * SecInHour;
		this->Energy = this->Power * TimeStepSys * SecInHour;
		this->EvaporatorEnergy = this->QEvaporator * TimeStepSys * SecInHour;
		this->FuelEnergyUseRate = EngineDrivenFuelEnergy;
		this->FuelEnergy = this->FuelEnergyUseRate * TimeStepSys * SecInHour;
		this->JacketEnergyRec = this->QJacketRecovered * TimeStepSys * SecInHour;
		this->LubeOilEnergyRec = this->QLubeOilRecovered * TimeStepSys * SecInHour;
		this->ExhaustEnergyRec = this->QExhaustRecovered * TimeStepSys * SecInHour;
		this->QTotalHeatRecovered = this->QExhaustRecovered + this->QLubeOilRecovered + this->QJacketRecovered;
		this->TotalHeatEnergyRec = this->ExhaustEnergyRec + this->LubeOilEnergyRec + this->JacketEnergyRec;
		this->FuelEnergyUseRate = std::abs( this->FuelEnergyUseRate );
		this->FuelEnergy = std::abs( this->FuelEnergy );
		this->FuelMdot = std::abs( this->FuelEnergyUseRate ) / ( this->FuelHeatingValue * KJtoJ );

		//check for problems BG 9/12/06 (deal with observed negative energy results)
		if ( this->Energy < 0.0 ) { // there is a serious problem
			if ( this->CondenserType == WaterCooled ) {
				// first check for run away condenser loop temps (only reason yet to be observed for this?)
				if ( CondInletTemp > 70.0 ) {
					ShowSevereError( "CalcEngineDrivenChillerModel: Condenser loop inlet temperatures > 70.0 C for EngineDrivenChiller=" + this->Name );
					ShowContinueErrorTimeStamp( "" );
					ShowContinueError( "Condenser loop water temperatures are too high at" + RoundSigDigits( CondInletTemp, 2 ) );
					ShowContinueError( "Check input for condenser plant loop, especially cooling tower" );
					ShowContinueError( "Evaporator inlet temperature: " + RoundSigDigits( Node( EvapInletNode ).Temp, 2 ) );

					ShowFatalError( "Program Terminates due to previous error condition" );
				}
			}
			if ( ! WarmupFlag ) {
				if ( AvailNomCapRat < 0.0 ) { // apparently the real reason energy goes negative
					ShowSevereError( "CalcEngineDrivenChillerModel: Capacity ratio below zero for EngineDrivenChiller=" + this->Name );
					ShowContinueErrorTimeStamp( "" );
					ShowContinueError( "Check input for Capacity Ratio Curve" );
					ShowContinueError( "Condenser inlet temperature: " + RoundSigDigits( CondInletTemp, 2 ) );
					ShowContinueError( "Evaporator inlet temperature: " + RoundSigDigits( Node( EvapInletNode ).Temp, 2 ) );
					ShowFatalError( "Program Terminates due to previous error condition" );
				}
			}
			// If makes it here, set limits, chiller can't have negative energy/power
			// proceeding silently for now but may want to throw error here
			this->Power = 0.0;
			this->Energy = 0.0;
		}

	}

	void
	GTChillerSpecs::CalcGTChillerModel(
		Real64 & MyLoad, // operating load
		bool const RunFlag, // TRUE when chiller operating
		int const EquipFlowCtrl // Flow control mode for the equipment
	)
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         Dan Fisher / Brandon Anderson
		//       DATE WRITTEN   Sept. 2000
		//       MODIFIED       Chandan Sharma, FSEC, February 2010, Added basin heater
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// simulate a vapor compression chiller using the GT model

		// METHODOLOGY EMPLOYED:
		// curve fit of performance data:

		// REFERENCES:
		// 1. BLAST Users Manual
		// 2. CHILLER User Manual

		// Using/Aliasing
		using DataGlobals::BeginEnvrnFlag;
		using DataGlobals::SecInHour;
		using DataGlobals::CurrentTime;
		using DataHVACGlobals::TimeStepSys;
		using DataHVACGlobals::SysTimeElapsed;
		using General::RoundSigDigits;
		using General::CreateSysTimeIntervalString;
		using DataPlant::PlantLoop;
		using DataPlant::TypeOf_Chiller_CombTurbine;
		using DataPlant::CompSetPtBasedSchemeType;
		using DataPlant::CriteriaType_MassFlowRate;
		using DataPlant::SingleSetPoint;
		using DataPlant::DualSetPointDeadBand;
		using DataBranchAirLoopPlant::ControlType_SeriesActive;
		using DataBranchAirLoopPlant::MassFlowTolerance;
		using DataEnvironment::OutDryBulbTemp;
		using DataEnvironment::EnvironmentName;
		using DataEnvironment::CurMnDy;
		using FluidProperties::GetSpecificHeatGlycol;
		using PlantUtilities::SetComponentFlowRate;
		using PlantUtilities::PullCompInterconnectTrigger;

		// Locals
		Real64 ExhaustStackTemp( 0.0 ); // Temperature of Exhaust Gases
		Real64 EvapInletTemp; // C - evaporator inlet temperature, water side
		Real64 CondInletTemp; // C - condenser inlet temperature, water side

		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:

		Real64 const ExhaustCP( 1.047 ); // Exhaust Gas Specific Heat
		static gio::Fmt OutputFormat( "(F6.2)" );
		static std::string const RoutineName( "CalcGTChillerModel" );
		static std::string const RoutineNameHeatRecovery( "ChillerHeatRecovery" );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 MinPartLoadRat; // min allowed operating frac full load
		Real64 MaxPartLoadRat; // max allowed operating frac full load
		Real64 TempCondIn; // C - (GT ADJTC(1)The design secondary loop fluid
		Real64 TempCondInDesign; // C - (GT ADJTC(1)The design secondary loop fluid
		Real64 TempRiseRat; // intermediate result:  temperature rise ratio
		Real64 TempEvapOut; // C - evaporator outlet temperature, water side
		Real64 TempEvapOutSetPoint( 0.0 ); // C - evaporator outlet temperature setpoint
		Real64 TempEvapOutDesign; // design evaporator outlet temperature, water side
		Real64 ChillerNomCap; // chiller nominal capacity
		Real64 AvailChillerCap; // chiller available capacity
		Real64 COP; // coefficient of performance
		Real64 FracFullLoadPower; // fraction of full load power
		Real64 EvapDeltaTemp( 0.0 ); // C - evaporator temperature difference, water side
		Real64 DeltaTemp; // C - intermediate result: condenser/evaporator temp diff
		Real64 AvailNomCapRat; // intermediate result: available nominal capacity ratio
		Real64 FullLoadPowerRat; // intermediate result: full load power ratio
		Real64 PartLoadRat( 0.0 ); // part load ratio for efficiency calculations
		Real64 OperPartLoadRat; // Actual Operating PLR
		int EvapInletNode; // evaporator inlet node number, water side
		int EvapOutletNode; // evaporator outlet node number, water side
		int CondInletNode; // condenser inlet node number, water side
		int CondOutletNode; // condenser outlet node number, water side
		static Real64 EvapMassFlowRateMax( 0.0 ); // Max Design Evaporator Mass Flow Rate converted from Volume Flow Rate
		Real64 TempLowLimitEout; // C - Evaporator low temp. limit cut off
		// Special variables for GT Chiller
		Real64 RPLoad;
		Real64 PLoad;
		Real64 GTEngineCapacity; // Capacity of GT Unit attached to Chiller
		Real64 MaxExhaustperGTPower; // Maximum Exhaust Flow per KW Power Out
		Real64 RL;
		Real64 RL2;

		Real64 FuelEnergyIn( 0.0 ); // (EFUEL) Amount of Fuel Energy Required to run gas turbine
		Real64 ExhaustFlow( 0.0 ); // (FEX) Exhaust Gas Flow Rate cubic meters per second
		Real64 ExhaustTemp( 0.0 ); // (TEX) Exhaust Gas Temperature in C
		Real64 QHeatRecLube; // (ELUBE) Recoverable Lube Oil Energy (W)
		Real64 UAtoCapRat; // (UACGC) Heat Exchanger UA to Capacity
		Real64 AmbientDeltaT; // (ATAIR) Difference between ambient actual and ambient design temperatures
		Real64 DesignSteamSatTemp; // Saturization Temperature of Steam in Stack
		static Real64 TimeStepSysLast( 0.0 ); // last system time step (used to check for downshifting)
		Real64 CurrentEndTime; // end time of time step for current simulation time step
		static Real64 CurrentEndTimeLast( 0.0 ); // end time of time step for last simulation time step
		static std::string OutputChar; // character string for warning messages

		int HeatRecInNode; // Heat Recovery Fluid Inlet Node Num
		int HeatRecOutNode; // Heat Recovery Fluid Outlet Node Num
		Real64 HeatRecInTemp( 0.0 ); // Heat Recovery Fluid Inlet Temperature
		Real64 HeatRecOutTemp( 0.0 ); // Heat Recovery Fluid Outlet Temperature
		Real64 HeatRecMdot( 0.0 ); // Heat Recovery Fluid Mass FlowRate
		Real64 HeatRecCp; // Specific Heat of the Heat Recovery Fluid
		Real64 FuelHeatingValue; // Heating Value of Fuel in kJ/kg
		Real64 MinHeatRecMdot( 0.0 ); // Mass Flow rate that keeps from exceeding max temp
		Real64 HeatRecRatio; // Reduced ratio to multiply recovered heat terms by
		Real64 FRAC;
		//  LOGICAL,SAVE      :: PossibleSubcooling=.FALSE.

		int LoopNum;
		int LoopSideNum;
		Real64 Cp; // local for fluid specif heat, for evaporator
		Real64 CpCond; // local for fluid specif heat, for condenser

		//set module level inlet and outlet nodes
		this->EvapMassFlowRate = 0.0;
		this->CondMassFlowRate = 0.0;
		this->Power = 0.0;
		this->QCondenser = 0.0;
		this->QEvaporator = 0.0;
		this->Energy = 0.0;
		this->CondenserEnergy = 0.0;
		this->EvaporatorEnergy = 0.0;
		EvapInletNode = this->EvapInletNodeNum;
		EvapOutletNode = this->EvapOutletNodeNum;
		CondInletNode = this->CondInletNodeNum;
		CondOutletNode = this->CondOutletNodeNum;
		HeatRecInNode = this->HeatRecInletNodeNum;
		HeatRecOutNode = this->HeatRecOutletNodeNum;
		QHeatRecLube = 0.0;
		FRAC = 1.0;
		LoopNum = this->CWLoopNum;
		LoopSideNum = this->CWLoopSideNum;
		EvapInletTemp = Node( EvapInletNode ).Temp;

		// calculate end time of current time step
		CurrentEndTime = CurrentTime + SysTimeElapsed;

		// Print warning messages only when valid and only for the first ocurrance. Let summary provide statistics.
		// Wait for next time step to print warnings. If simulation iterates, print out
		// the warning for the last iteration only. Must wait for next time step to accomplish this.
		// If a warning occurs and the simulation down shifts, the warning is not valid.
		if ( CurrentEndTime > CurrentEndTimeLast && TimeStepSys >= TimeStepSysLast ) {
			if ( this->PrintMessage ) {
				++this->MsgErrorCount;
				// Show single warning and pass additional info to ShowRecurringWarningErrorAtEnd
				if ( this->MsgErrorCount < 2 ) {
					ShowWarningError( this->MsgBuffer1 + '.' );
					ShowContinueError( this->MsgBuffer2 );
				} else {
					ShowRecurringWarningErrorAtEnd( this->MsgBuffer1 + " error continues.", this->ErrCount1, this->MsgDataLast, this->MsgDataLast, _, "[C]", "[C]" );
				}
			}
		}

		// save last system time step and last end time of current time step (used to determine if warning is valid)
		TimeStepSysLast = TimeStepSys;
		CurrentEndTimeLast = CurrentEndTime;

		// If Chiller load is 0 or chiller is not running then leave the subroutine.Before leaving
		// if the component control is SERIESACTIVE we set the component flow to inlet flow so that
		// flow resolver will not shut down the branch
		if ( MyLoad >= 0.0 || ! RunFlag ) {
			if ( EquipFlowCtrl == ControlType_SeriesActive || PlantLoop( LoopNum ).LoopSide( LoopSideNum ).FlowLock == 1 ) {
				this->EvapMassFlowRate = Node( EvapInletNode ).MassFlowRate;
			} else {
				this->EvapMassFlowRate = 0.0;

				SetComponentFlowRate( this->EvapMassFlowRate, EvapInletNode, EvapOutletNode, this->CWLoopNum, this->CWLoopSideNum, this->CWBranchNum, this->CWCompNum );
			}
			if ( this->CondenserType == WaterCooled ) {
				if ( PlantLoop( this->CDLoopNum ).LoopSide( this->CDLoopSideNum ).Branch( this->CDBranchNum ).Comp( this->CDCompNum ).FlowCtrl == ControlType_SeriesActive ) {
					this->CondMassFlowRate = Node( CondInletNode ).MassFlowRate;
				} else {
					this->CondMassFlowRate = 0.0;
					SetComponentFlowRate( this->CondMassFlowRate, CondInletNode, CondOutletNode, this->CDLoopNum, this->CDLoopSideNum, this->CDBranchNum, this->CDCompNum );
				}
			}

			if ( this->CondenserType == EvapCooled ) {
				CalcBasinHeaterPower( this->BasinHeaterPowerFTempDiff, this->BasinHeaterSchedulePtr, this->BasinHeaterSetPointTemp, this->BasinHeaterPower );
			}
			this->PrintMessage = false;
			return;
		}

		if ( this->CondenserType == AirCooled ) { //Condenser inlet temp = outdoor temp
			Node( CondInletNode ).Temp = Node( CondInletNode ).OutAirDryBulb;
			//  Warn user if entering condenser temperature falls below 0C
			if ( Node( CondInletNode ).Temp < 0.0 && ! WarmupFlag ) {
				this->PrintMessage = true;
				gio::write( OutputChar, OutputFormat ) << Node( CondInletNode ).Temp;
				this->MsgBuffer1 = "CalcGasTurbineChillerModel - Chiller:CombustionTurbine \"" + this->Name + "\" - Air Cooled Condenser Inlet Temperature below 0C";
				this->MsgBuffer2 = "... Outdoor Dry-bulb Condition = " + OutputChar + " C. Occurrence info = " + EnvironmentName + ", " + CurMnDy + ' ' + CreateSysTimeIntervalString();
				this->MsgDataLast = Node( CondInletNode ).Temp;
			} else {
				this->PrintMessage = false;
			}
		} else if ( this->CondenserType == EvapCooled ) { //Condenser inlet temp = (outdoor wet bulb)
			Node( CondInletNode ).Temp = Node( CondInletNode ).OutAirWetBulb;
			//  Warn user if evap condenser wet bulb temperature falls below 10C
			if ( Node( CondInletNode ).Temp < 10.0 && ! WarmupFlag ) {
				this->PrintMessage = true;
				gio::write( OutputChar, OutputFormat ) << Node( CondInletNode ).Temp;
				this->MsgBuffer1 = "CalcGasTurbineChillerModel - Chiller:CombustionTurbine \"" + this->Name + "\" - Evap Cooled Condenser Inlet Temperature below 10C";
				this->MsgBuffer2 = "... Outdoor Wet-bulb Condition = " + OutputChar + " C. Occurrence info = " + EnvironmentName + ", " + CurMnDy + ' ' + CreateSysTimeIntervalString();
				this->MsgDataLast = Node( CondInletNode ).Temp;
			} else {
				this->PrintMessage = false;
			}
		} // End of the Air Cooled/Evap Cooled Logic block

		// If not air or evap cooled then set to the condenser node that is attached to a cooling tower
		CondInletTemp = Node( CondInletNode ).Temp;

		//Set mass flow rates
		if ( this->CondenserType == WaterCooled ) {
			this->CondMassFlowRate = this->CondMassFlowRateMax;
			SetComponentFlowRate( this->CondMassFlowRate, CondInletNode, CondOutletNode, this->CDLoopNum, this->CDLoopSideNum, this->CDBranchNum, this->CDCompNum );
			PullCompInterconnectTrigger( this->CWLoopNum, this->CWLoopSideNum, this->CWBranchNum, this->CWCompNum, this->CondMassFlowIndex, this->CDLoopNum, this->CDLoopSideNum, CriteriaType_MassFlowRate, this->CondMassFlowRate );

			if ( this->CondMassFlowRate < MassFlowTolerance ) return;

		}

		//  LOAD LOCAL VARIABLES FROM DATA STRUCTURE (for code readability)
		auto const & CapacityRat( this->CapRatCoef );
		auto const & PowerRat( this->PowerRatCoef );
		auto const & FullLoadFactor( this->FullLoadCoef );
		MinPartLoadRat = this->MinPartLoadRat;
		MaxPartLoadRat = this->MaxPartLoadRat;
		TempCondInDesign = this->TempDesCondIn;
		TempRiseRat = this->TempRiseCoef;
		TempEvapOutDesign = this->TempDesEvapOut;
		ChillerNomCap = this->NomCap;
		COP = this->COP;
		TempCondIn = Node( this->CondInletNodeNum ).Temp;
		TempEvapOut = Node( this->EvapOutletNodeNum ).Temp;
		TempLowLimitEout = this->TempLowLimitEvapOut;
		EvapMassFlowRateMax = this->EvapMassFlowRateMax;
		LoopNum = this->CWLoopNum;
		LoopSideNum = this->CWLoopSideNum;

		//*********************************

		//Calculate chiller performance from this set of performance equations.
		//  from BLAST...Z=(TECONDW-ADJTC(1))/ADJTC(2)-(TLCHLRW-ADJTC(3))
		DeltaTemp = ( TempCondIn - TempCondInDesign ) / TempRiseRat - ( TempEvapOut - TempEvapOutDesign );

		//  from BLAST...RCAV=RCAVC(1)+RCAVC(2)*Z+RCAVC(3)*Z**2
		AvailNomCapRat = CapacityRat( 1 ) + CapacityRat( 2 ) * DeltaTemp + CapacityRat( 3 ) * pow_2( DeltaTemp );

		AvailChillerCap = ChillerNomCap * AvailNomCapRat;

		// from BLAST...G=ADJEC(1)+ADJEC(2)*RCAV+ADJEC(3)*RCAV**2.
		FullLoadPowerRat = PowerRat( 1 ) + PowerRat( 2 ) * AvailNomCapRat + PowerRat( 3 ) * pow_2( AvailNomCapRat );

		//  from BLAST...RCLOAD=AMAX1(MINCHFR(I,IPLCTR),AMIN1(CHLRLOAD(I)/CHLROCAP(I) &
		//         /RCAV,MAXCHFR(I,IPLCTR)))
		if ( AvailChillerCap > 0.0 ) {
			PartLoadRat = max( MinPartLoadRat, min( std::abs( MyLoad ) / AvailChillerCap, MaxPartLoadRat ) );
		}

		// from BLAST...RPOWER=RPWRC(1)+RPWRC(2)*RCLOAD+RPWRC(3)*RCLOAD**2
		FracFullLoadPower = FullLoadFactor( 1 ) + FullLoadFactor( 2 ) * PartLoadRat + FullLoadFactor( 3 ) * pow_2( PartLoadRat );

		if ( AvailChillerCap > 0.0 ) {
			if ( std::abs( MyLoad ) / AvailChillerCap < MinPartLoadRat ) {
				OperPartLoadRat = std::abs( MyLoad ) / AvailChillerCap;
			} else {
				OperPartLoadRat = PartLoadRat;
			}
		} else {
			OperPartLoadRat = 0.0;
		}
		//*********************************
		Cp = GetSpecificHeatGlycol( PlantLoop( this->CWLoopNum ).FluidName, Node( EvapInletNode ).Temp, PlantLoop( this->CWLoopNum ).FluidIndex, RoutineName );
		// If FlowLock is True, the new resolved mdot is used to update Power, QEvap, Qcond, and
		// condenser side outlet temperature.
		if ( PlantLoop( LoopNum ).LoopSide( LoopSideNum ).FlowLock == 0 ) {
			this->PossibleSubcooling = false;
			this->QEvaporator = AvailChillerCap * OperPartLoadRat;
			if ( OperPartLoadRat < MinPartLoadRat ) {
				FRAC = min( 1.0, ( OperPartLoadRat / MinPartLoadRat ) );
			} else {
				FRAC = 1.0;
			}
			this->Power = FracFullLoadPower * FullLoadPowerRat * AvailChillerCap / COP * FRAC;

			// Either set the flow to the Constant value or caluclate the flow for the variable volume
			if ( ( this->FlowMode == ConstantFlow ) || ( this->FlowMode == NotModulated ) ) {
				// Start by assuming max (design) flow
				this->EvapMassFlowRate = EvapMassFlowRateMax;
				// Use SetComponentFlowRate to decide actual flow
				SetComponentFlowRate( this->EvapMassFlowRate, EvapInletNode, EvapOutletNode, this->CWLoopNum, this->CWLoopSideNum, this->CWBranchNum, this->CWCompNum );
				// Evaluate delta temp based on actual flow rate
				if ( this->EvapMassFlowRate != 0.0 ) {
					EvapDeltaTemp = this->QEvaporator / this->EvapMassFlowRate / Cp;
				} else {
					EvapDeltaTemp = 0.0;
				}
				// Evaluate outlet temp based on delta
				this->EvapOutletTemp = Node( EvapInletNode ).Temp - EvapDeltaTemp;
			} else if ( this->FlowMode == LeavingSetPointModulated ) {
				// Calculate the Delta Temp from the inlet temp to the chiller outlet setpoint
				{ auto const SELECT_CASE_var( PlantLoop( this->CWLoopNum ).LoopDemandCalcScheme );
				if ( SELECT_CASE_var == SingleSetPoint ) {
					EvapDeltaTemp = Node( EvapInletNode ).Temp - Node( EvapOutletNode ).TempSetPoint;
				} else if ( SELECT_CASE_var == DualSetPointDeadBand ) {
					EvapDeltaTemp = Node( EvapInletNode ).Temp - Node( EvapOutletNode ).TempSetPointHi;
				}}
				if ( EvapDeltaTemp != 0.0 ) {
					// Calculate desired flow to request based on load
					this->EvapMassFlowRate = std::abs( this->QEvaporator / Cp / EvapDeltaTemp );
					if ( ( this->EvapMassFlowRate - EvapMassFlowRateMax ) > MassFlowTolerance ) this->PossibleSubcooling = true;
					//Check to see if the Maximum is exceeded, if so set to maximum
					this->EvapMassFlowRate = min( EvapMassFlowRateMax, this->EvapMassFlowRate );
					// Use SetComponentFlowRate to decide actual flow
					SetComponentFlowRate( this->EvapMassFlowRate, EvapInletNode, EvapOutletNode, this->CWLoopNum, this->CWLoopSideNum, this->CWBranchNum, this->CWCompNum );
					{ auto const SELECT_CASE_var( PlantLoop( this->CWLoopNum ).LoopDemandCalcScheme );
					if ( SELECT_CASE_var == SingleSetPoint ) {
						this->EvapOutletTemp = Node( EvapOutletNode ).TempSetPoint;
					} else if ( SELECT_CASE_var == DualSetPointDeadBand ) {
						this->EvapOutletTemp = Node( EvapOutletNode ).TempSetPointHi;
					}}
				} else {
					// Try to request zero flow
					this->EvapMassFlowRate = 0.0;
					// Use SetComponentFlowRate to decide actual flow
					SetComponentFlowRate( this->EvapMassFlowRate, EvapInletNode, EvapOutletNode, this->CWLoopNum, this->CWLoopSideNum, this->CWBranchNum, this->CWCompNum );
					// No deltaT since component is not running
					this->EvapOutletTemp = Node( EvapInletNode ).Temp;

				}
			} //End of Constant Variable Flow If Block
		} else { // If FlowLock is True

			this->EvapMassFlowRate = Node( EvapInletNode ).MassFlowRate;
			SetComponentFlowRate( this->EvapMassFlowRate, EvapInletNode, EvapOutletNode, this->CWLoopNum, this->CWLoopSideNum, this->CWBranchNum, this->CWCompNum );
			//       Some other component set the flow to 0. No reason to continue with calculations.
			if ( this->EvapMassFlowRate == 0.0 ) {
				MyLoad = 0.0;
				if ( this->CondenserType == EvapCooled ) {
					CalcBasinHeaterPower( this->BasinHeaterPowerFTempDiff, this->BasinHeaterSchedulePtr, this->BasinHeaterSetPointTemp, this->BasinHeaterPower );
				}
				this->PrintMessage = false;
				return;
			}

			if ( this->PossibleSubcooling ) {
				this->QEvaporator = std::abs( MyLoad );
				EvapDeltaTemp = this->QEvaporator / this->EvapMassFlowRate / Cp;
				this->EvapOutletTemp = Node( EvapInletNode ).Temp - EvapDeltaTemp;
			} else { //No subcooling in this case.No recalculation required.Still need to check chiller low temp limit
				{ auto const SELECT_CASE_var( PlantLoop( LoopNum ).LoopDemandCalcScheme );
				if ( SELECT_CASE_var == SingleSetPoint ) {
					if ( ( this->FlowMode == LeavingSetPointModulated ) || ( PlantLoop( LoopNum ).LoopSide( LoopSideNum ).Branch( this->CWBranchNum ).Comp( this->CWCompNum ).CurOpSchemeType == CompSetPtBasedSchemeType ) || ( Node( EvapOutletNode ).TempSetPoint != SensedNodeFlagValue ) ) {
						TempEvapOutSetPoint = Node( EvapOutletNode ).TempSetPoint;
					} else {
						TempEvapOutSetPoint = Node( PlantLoop( LoopNum ).TempSetPointNodeNum ).TempSetPoint;
					}
				} else if ( SELECT_CASE_var == DualSetPointDeadBand ) {
					if ( ( this->FlowMode == LeavingSetPointModulated ) || ( PlantLoop( LoopNum ).LoopSide( LoopSideNum ).Branch( this->CWBranchNum ).Comp( this->CWCompNum ).CurOpSchemeType == CompSetPtBasedSchemeType ) || ( Node( EvapOutletNode ).TempSetPointHi != SensedNodeFlagValue ) ) {
						TempEvapOutSetPoint = Node( EvapOutletNode ).TempSetPointHi;
					} else {
						TempEvapOutSetPoint = Node( PlantLoop( LoopNum ).TempSetPointNodeNum ).TempSetPointHi;
					}
				}}
				EvapDeltaTemp = Node( EvapInletNode ).Temp - TempEvapOutSetPoint;
				this->QEvaporator = std::abs( this->EvapMassFlowRate * Cp * EvapDeltaTemp );
				this->EvapOutletTemp = TempEvapOutSetPoint;
			}
			//Check that the Evap outlet temp honors both plant loop temp low limit and also the chiller low limit
			if ( this->EvapOutletTemp < TempLowLimitEout ) {
				if ( ( Node( EvapInletNode ).Temp - TempLowLimitEout ) > DeltaTempTol ) {
					this->EvapOutletTemp = TempLowLimitEout;
					EvapDeltaTemp = Node( EvapInletNode ).Temp - this->EvapOutletTemp;
					this->QEvaporator = this->EvapMassFlowRate * Cp * EvapDeltaTemp;
				} else {
					this->EvapOutletTemp = Node( EvapInletNode ).Temp;
					EvapDeltaTemp = Node( EvapInletNode ).Temp - this->EvapOutletTemp;
					this->QEvaporator = this->EvapMassFlowRate * Cp * EvapDeltaTemp;
				}
			}
			if ( this->EvapOutletTemp < Node( EvapOutletNode ).TempMin ) {
				if ( ( Node( EvapInletNode ).Temp - Node( EvapOutletNode ).TempMin ) > DeltaTempTol ) {
					this->EvapOutletTemp = Node( EvapOutletNode ).TempMin;
					EvapDeltaTemp = Node( EvapInletNode ).Temp - this->EvapOutletTemp;
					this->QEvaporator = this->EvapMassFlowRate * Cp * EvapDeltaTemp;
				} else {
					this->EvapOutletTemp = Node( EvapInletNode ).Temp;
					EvapDeltaTemp = Node( EvapInletNode ).Temp - this->EvapOutletTemp;
					this->QEvaporator = this->EvapMassFlowRate * Cp * EvapDeltaTemp;
				}
			}
			// If load exceeds the distributed load set to the distributed load
			if ( this->QEvaporator > std::abs( MyLoad ) ) {
				if ( this->EvapMassFlowRate > MassFlowTolerance ) {
					this->QEvaporator = std::abs( MyLoad );
					EvapDeltaTemp = this->QEvaporator / this->EvapMassFlowRate / Cp;
					this->EvapOutletTemp = Node( EvapInletNode ).Temp - EvapDeltaTemp;
				} else {
					this->QEvaporator = 0.0;
					this->EvapOutletTemp = Node( EvapInletNode ).Temp;
				}
			}

			// Checks QEvaporator on the basis of the machine limits.
			if ( this->QEvaporator > ( AvailChillerCap * MaxPartLoadRat ) ) {
				if ( this->EvapMassFlowRate > MassFlowTolerance ) {
					this->QEvaporator = AvailChillerCap * PartLoadRat;
					EvapDeltaTemp = this->QEvaporator / this->EvapMassFlowRate / Cp;
					this->EvapOutletTemp = Node( EvapInletNode ).Temp - EvapDeltaTemp;
				} else {
					this->QEvaporator = 0.0;
					this->EvapOutletTemp = Node( EvapInletNode ).Temp;
				}
			}

			if ( OperPartLoadRat < MinPartLoadRat ) {
				FRAC = min( 1.0, ( OperPartLoadRat / MinPartLoadRat ) );
			} else {
				FRAC = 1.0;
			}

			// set the module level variable used for reporting FRAC
			this->ChillerCyclingRatio = FRAC;

			// Chiller is false loading below PLR = minimum unloading ratio, find PLR used for energy calculation
			this->Power = FracFullLoadPower * FullLoadPowerRat * AvailChillerCap / COP * FRAC;

			if ( this->EvapMassFlowRate == 0.0 ) {
				this->QEvaporator = 0.0;
				this->EvapOutletTemp = Node( EvapInletNode ).Temp;
				this->Power = 0.0;
				this->PrintMessage = false;
			}
			if ( this->QEvaporator == 0.0 && this->CondenserType == EvapCooled ) {
				CalcBasinHeaterPower( this->BasinHeaterPowerFTempDiff, this->BasinHeaterSchedulePtr, this->BasinHeaterSetPointTemp, this->BasinHeaterPower );
			}

		} //This is the end of the FlowLock Block

		//Now determine Cooling
		//QCondenser is calculated the same for each type, but the power consumption should be different
		//  depending on the performance coefficients used for the chiller model.
		this->QCondenser = this->Power + this->QEvaporator;

		if ( this->CondenserType == WaterCooled ) {

			if ( this->CondMassFlowRate > MassFlowTolerance ) {
				CpCond = GetSpecificHeatGlycol( PlantLoop( this->CDLoopNum ).FluidName, CondInletTemp, PlantLoop( this->CDLoopNum ).FluidIndex, RoutineName );
				this->CondOutletTemp = this->QCondenser / this->CondMassFlowRate / CpCond + CondInletTemp;
			} else {
				ShowSevereError( "CalcGasTurbineChillerModel: Condenser flow = 0, for GasTurbineChiller=" + this->Name );
				ShowContinueErrorTimeStamp( "" );

			}

		} else { //Air Cooled or Evap Cooled

			//don't care about outlet temp for Air-Cooled or Evap Cooled and there is no CondMassFlowRate and would divide by zero
			this->CondOutletTemp = CondInletTemp;
		}

		//Special GT Chiller Variables
		// Gas Turbine Driven Portion of the Chiller:

		GTEngineCapacity = this->GTEngineCapacity;
		MaxExhaustperGTPower = this->MaxExhaustperGTPower;

		//Note: All Old Blast Code comments begin at left.

		//D                                   COMPUTE TOWER CLOAD
		//               ETOWER(TypeIndex) = PREQD + CHLRLOAD(TypeIndex)
		//               RPLOAD = PREQD/CHLROCAP(TypeIndex)
		//               IF (RFLAGS(81)) WRITE (OUTPUT,703) PREQD,ETOWER(TypeIndex),RPLOAD
		//               IF (PREQD .GT. 0.0d0) THEN
		if ( AvailChillerCap > 0 ) {
			RPLoad = this->Power / AvailChillerCap;
		} else {
			RPLoad = 0.0;
		}

		if ( this->Power > 0 ) {
			//D$                               FOR EACH CHILLER OPERATING
			//                  MAXSZ = NUMCHSIZ(TypeIndex,IPLCTR)
			//                  DO IS = 1,MAXSZ
			//                     NUMOPR = CHLRIOPR(IS,TypeIndex)
			//                     IF (NUMOPR.GT.0) THEN
			//                        PLOAD = CHNOMCAP(IS,TypeIndex,IPLCTR) * RPLOAD

			PLoad = ChillerNomCap * RPLoad;

			//D$                                COMPUTE FUEL AND WASTE HEAT
			//     TEX IS CALCULATED USING COEFFICIENTS TEX2GC( ) TO RESULT IN TEMP.
			//     DEGREES ACTUAL, HENCE THE NECESSARY CONVERSION ?-273.?
			//                        RLOAD=AMAX1(PLOAD/CHLROCAP(TypeIndex),MINCHFR(TypeIndex,IPLCTR))
			//                        RLD2 = RLOAD**2

			// RL = MAX(PLoad/GTEngineCapacity, MinPartLoadRat * ChillerNomCap)
			RL = max( PLoad / ChillerNomCap, MinPartLoadRat );
			RL2 = pow_2( RL );

			//     ATAIR = DELTA TEMPERATURE. ACTUAL - 25 DEG.C (77 DEG.F)
			//                                RATING POINT
			//                        ATAIR = ODB - 25.
			//                        TAR2=ATAIR**2

			// ??? Not sure about this Ambient Actual Temp - also do we need to have design ambient as input?

			if ( this->CondenserType == WaterCooled ) {
				AmbientDeltaT = OutDryBulbTemp - 25.0;
			} else { // air or evap cooled
				AmbientDeltaT = Node( CondInletNode ).OutAirDryBulb - 25.0;
			}

			//                        EFUEL=PLOAD*(FUL1GC(1,IPLCTR)+FUL1GC(2,IPLCTR)*  &
			//                              RLOAD+FUL1GC(3,IPLCTR)*RLD2)*              &
			//                              (FUL2GC(1,IPLCTR)+FUL2GC(2,IPLCTR)*ATAIR+  &
			//                              FUL2GC(3,IPLCTR)*TAR2)

			FuelEnergyIn = PLoad * ( this->PLBasedFuelInputCoef( 1 ) + this->PLBasedFuelInputCoef( 2 ) * RL + this->PLBasedFuelInputCoef( 3 ) * RL2 ) * ( this->TempBasedFuelInputCoef( 1 ) + this->TempBasedFuelInputCoef( 2 ) * AmbientDeltaT + this->TempBasedFuelInputCoef( 3 ) * pow_2( AmbientDeltaT ) );

			//                        FEX=GTDSLCAP(IS,TypeIndex,IPLCTR)*(FEXGC(1,IPLCTR)+      &
			//                            FEXGC(2,IPLCTR)*ATAIR+FEXGC(3,IPLCTR)*TAR2)

			ExhaustFlow = GTEngineCapacity * ( this->ExhaustFlowCoef( 1 ) + this->ExhaustFlowCoef( 2 ) * AmbientDeltaT + this->ExhaustFlowCoef( 3 ) * pow_2( AmbientDeltaT ) );

			//                        TEX=(TEX1GC(1,IPLCTR)+TEX1GC(2,IPLCTR)*RLOAD+    &
			//                            TEX1GC(3,IPLCTR)*RLD2)*(TEX2GC(1,IPLCTR)+    &
			//                            TEX2GC(2,IPLCTR)*ATAIR+TEX2GC(3,IPLCTR)*     &
			//                            TAR2)-273.

			ExhaustTemp = ( this->PLBasedExhaustTempCoef( 1 ) + this->PLBasedExhaustTempCoef( 2 ) * RL + this->PLBasedExhaustTempCoef( 3 ) * RL2 ) * ( this->TempBasedExhaustTempCoef( 1 ) + this->TempBasedExhaustTempCoef( 2 ) * AmbientDeltaT + this->TempBasedExhaustTempCoef( 3 ) * pow_2( AmbientDeltaT ) ) - 273;

			//                        UAG=UACGC(1,IPLCTR)*GTDSLCAP(IS,TypeIndex,IPLCTR)**      &
			//                            UACGC(2,IPLCTR)
			if ( PLoad != 0.0 ) {
				UAtoCapRat = this->UAtoCapCoef( 1 ) * std::pow( GTEngineCapacity, this->UAtoCapCoef( 2 ) );

				//     TSTACK = EXHAUST STACK TEMPERATURE, C.
				//                        TSTACK=TSATUR(IPLCTR)+(TEX-TSATUR(IPLCTR))/      &
				//                               EXP(UAG/(AMAX1(FEX,RMXKGC(IPLCTR)*        &
				//                               GTDSLCAP(IS,TypeIndex,IPLCTR)) * 1.047))

				DesignSteamSatTemp = this->DesignSteamSatTemp;
				ExhaustStackTemp = DesignSteamSatTemp + ( ExhaustTemp - DesignSteamSatTemp ) / std::exp( UAtoCapRat / ( max( ExhaustFlow, MaxExhaustperGTPower * GTEngineCapacity ) * ExhaustCP ) );

				//                        EEX = AMAX1 ( FEX*1.047*(TEX-TSTACK),0.0d0)
				//                        ELUBE=PLOAD*(ELUBEGC(1,IPLCTR)+ELUBEGC(2,IPLCTR) &
				//                              *RLOAD+ELUBEGC(3,IPLCTR)*RLD2 )
			}

			if ( this->HeatRecActive ) {
				QHeatRecLube = PLoad * ( this->HeatRecLubeEnergyCoef( 1 ) + this->HeatRecLubeEnergyCoef( 2 ) * RL + this->HeatRecLubeEnergyCoef( 3 ) * RL2 );

			} else {
				QHeatRecLube = 0.0;
			}

			//                        CHLRFUEL(TypeIndex) = CHLRFUEL(TypeIndex) + EFUEL * NUMOPR
			//                        EEXGC = EEXGC + EEX * NUMOPR
			//                        ELBEGC = ELBEGC + ELUBE * NUMOPR

			//Heat Recovery Loop -  lube recovered heat
			//   If lube is not present, then the energy should be 0 at this point
			// Thigh = Energy / (Mdot*Cp) + Tlow

			//Need to set the HeatRecRatio to 1.0 if it is not modified
			HeatRecRatio = 1.0;

			if ( this->HeatRecActive ) {
				//This mdot is input specified mdot "Desired Flowrate", already set at node in init routine
				HeatRecMdot = Node( HeatRecInNode ).MassFlowRate;
				HeatRecInTemp = Node( HeatRecInNode ).Temp;
				HeatRecCp = GetSpecificHeatGlycol( PlantLoop( this->HRLoopNum ).FluidName, HeatRecInTemp, PlantLoop( this->HRLoopNum ).FluidIndex, RoutineNameHeatRecovery );

				//Don't divide by zero
				if ( ( HeatRecMdot > 0.0 ) && ( HeatRecCp > 0.0 ) ) {
					HeatRecOutTemp = ( QHeatRecLube ) / ( HeatRecMdot * HeatRecCp ) + HeatRecInTemp;
				} else {
					HeatRecOutTemp = HeatRecInTemp;
				}

				//Now verify that the design flowrate was large enough to prevent phase change
				if ( HeatRecOutTemp > this->HeatRecMaxTemp ) {
					if ( this->HeatRecMaxTemp != HeatRecInTemp ) {
						MinHeatRecMdot = ( QHeatRecLube ) / ( HeatRecCp * ( this->HeatRecMaxTemp - HeatRecInTemp ) );
						if ( MinHeatRecMdot < 0.0 ) MinHeatRecMdot = 0.0;
					}

					//Recalculate Outlet Temperature, with adjusted flowrate
					if ( ( MinHeatRecMdot > 0.0 ) && ( HeatRecCp > 0.0 ) ) {
						HeatRecOutTemp = ( QHeatRecLube ) / ( MinHeatRecMdot * HeatRecCp ) + HeatRecInTemp;
						HeatRecRatio = HeatRecMdot / MinHeatRecMdot;
					} else {
						HeatRecOutTemp = HeatRecInTemp;
						HeatRecRatio = 0.0;
					}
				}

				QHeatRecLube *= HeatRecRatio;
			} else {
				HeatRecInTemp = 0.0;
				HeatRecMdot = 0.0;
				HeatRecCp = 0.0;
				HeatRecOutTemp = 0.0;
			}

		}

		this->HeatRecInletTemp = HeatRecInTemp;
		this->HeatRecOutletTemp = HeatRecOutTemp;
		this->HeatRecMdot = HeatRecMdot;
		this->HeatRecLubeEnergy = QHeatRecLube * ( TimeStepSys * SecInHour );
		this->HeatRecLubeRate = QHeatRecLube;
		this->FuelEnergyIn = std::abs( FuelEnergyIn );

		FuelHeatingValue = this->FuelHeatingValue;

		this->reports.FuelMassUsedRate = std::abs( FuelEnergyIn ) / ( FuelHeatingValue * KJtoJ );

		this->ExhaustStackTemp = ExhaustStackTemp;

		//Calculate Energy
		this->CondenserEnergy = this->QCondenser * TimeStepSys * SecInHour;
		this->Energy = this->Power * TimeStepSys * SecInHour;
		this->EvaporatorEnergy = this->QEvaporator * TimeStepSys * SecInHour;

		//check for problems BG 9/12/06 (deal with observed negative energy results)
		if ( this->Energy < 0.0 ) { // there is a serious problem

			if ( this->CondenserType == WaterCooled ) {
				// first check for run away condenser loop temps (only reason yet to be observed for this?)
				if ( CondInletTemp > 70.0 ) {
					ShowSevereError( "CalcGTChillerModel: Condenser loop inlet temperatures over 70.0 C for GTChiller=" + this->Name );
					ShowContinueErrorTimeStamp( "" );
					ShowContinueError( "Condenser loop water temperatures are too high at" + RoundSigDigits( CondInletTemp, 2 ) );
					ShowContinueError( "Check input for condenser plant loop, especially cooling tower" );
					ShowContinueError( "Evaporator inlet temperature: " + RoundSigDigits( Node( EvapInletNode ).Temp, 2 ) );

					ShowFatalError( "Program Terminates due to previous error condition" );
				}
			}
			if ( ! WarmupFlag ) {
				if ( AvailNomCapRat < 0.0 ) { // apparently the real reason energy goes negative
					ShowSevereError( "CalcGTChillerModel: Capacity ratio below zero for GTChiller=" + this->Name );
					ShowContinueErrorTimeStamp( "" );
					ShowContinueError( "Check input for Capacity Ratio Curve" );
					ShowContinueError( "Condenser inlet temperature: " + RoundSigDigits( CondInletTemp, 2 ) );
					ShowContinueError( "Evaporator inlet temperature: " + RoundSigDigits( Node( EvapInletNode ).Temp, 2 ) );
					ShowFatalError( "Program Terminates due to previous error condition" );
				}
			}
			// If makes it here, set limits, chiller can't have negative energy/power
			// proceeding silently for now but may want to throw error here
			this->Power = 0.0;
			this->Energy = 0.0;
		}
	}

	void
	ConstCOPChillerSpecs::CalcConstCOPChillerModel(
		Real64 & MyLoad,
		bool const RunFlag,
		int const EquipFlowCtrl // Flow control mode for the equipment
	)
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         Dan Fisher
		//       DATE WRITTEN   Sept. 1998
		//       MODIFIED       Richard Liesen Nov-Dec 2001; Jan 2002,
		//                      Chandan Sharma, FSEC, February 2010, Added basin heater
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:

		// METHODOLOGY EMPLOYED:

		// REFERENCES:

		// Using/Aliasing
		using DataGlobals::SecInHour;
		using DataGlobals::CurrentTime;
		using DataHVACGlobals::TimeStepSys;
		using DataHVACGlobals::SysTimeElapsed;
		using General::RoundSigDigits;
		using General::CreateSysTimeIntervalString;
		using DataPlant::PlantLoop;
		using DataPlant::SimPlantEquipTypes;
		using DataPlant::CompSetPtBasedSchemeType;
		using DataPlant::CriteriaType_MassFlowRate;
		using DataPlant::SingleSetPoint;
		using DataPlant::DualSetPointDeadBand;
		using DataBranchAirLoopPlant::ControlType_SeriesActive;
		using DataBranchAirLoopPlant::MassFlowTolerance;
		using DataEnvironment::EnvironmentName;
		using DataEnvironment::CurMnDy;
		using FluidProperties::GetSpecificHeatGlycol;
		using PlantUtilities::SetComponentFlowRate;
		using PlantUtilities::PullCompInterconnectTrigger;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:

		Real64 const DeltaTempTol( 0.0001 ); // C - minimum significant mass flow rate
		static gio::Fmt OutputFormat( "(F6.2)" );
		static std::string const RoutineName( "CalcConstCOPChillerModel" );

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 EvapDeltaTemp;
		Real64 TempEvapOutSetPoint( 0.0 ); // C - evaporator outlet temperature setpoint
		int EvapInletNode;
		int EvapOutletNode;
		int CondInletNode;
		int CondOutletNode;
		//  LOGICAL,SAVE           :: PossibleSubcooling=.FALSE.
		int LoopNum;
		int LoopSideNum;
		static Real64 TimeStepSysLast( 0.0 ); // last system time step (used to check for downshifting)
		Real64 CurrentEndTime; // end time of time step for current simulation time step
		static Real64 CurrentEndTimeLast( 0.0 ); // end time of time step for last simulation time step
		static std::string OutputChar; // character string for warning messages
		Real64 Cp; // local for fluid specif heat, for evaporator
		Real64 CpCond; // local for fluid specif heat, for condenser

		EvapInletNode = this->EvapInletNodeNum;
		EvapOutletNode = this->EvapOutletNodeNum;
		CondInletNode = this->CondInletNodeNum;
		CondOutletNode = this->CondOutletNodeNum;

		//set module level chiller inlet and temperature variables
		LoopNum = this->CWLoopNum;
		LoopSideNum = this->CWLoopSideNum;
		{ auto const SELECT_CASE_var( PlantLoop( LoopNum ).LoopDemandCalcScheme );
		if ( SELECT_CASE_var == SingleSetPoint ) {
			if ( ( this->FlowMode == LeavingSetPointModulated ) || ( PlantLoop( LoopNum ).LoopSide( LoopSideNum ).Branch( this->CWBranchNum ).Comp( this->CWCompNum ).CurOpSchemeType == CompSetPtBasedSchemeType ) || ( Node( EvapOutletNode ).TempSetPoint != SensedNodeFlagValue ) ) {
				TempEvapOutSetPoint = Node( EvapOutletNode ).TempSetPoint;
			} else {
				TempEvapOutSetPoint = Node( PlantLoop( LoopNum ).TempSetPointNodeNum ).TempSetPoint;
			}
		} else if ( SELECT_CASE_var == DualSetPointDeadBand ) {
			if ( ( this->FlowMode == LeavingSetPointModulated ) || ( PlantLoop( LoopNum ).LoopSide( LoopSideNum ).Branch( this->CWBranchNum ).Comp( this->CWCompNum ).CurOpSchemeType == CompSetPtBasedSchemeType ) || ( Node( EvapOutletNode ).TempSetPointHi != SensedNodeFlagValue ) ) {
				TempEvapOutSetPoint = Node( EvapOutletNode ).TempSetPointHi;
			} else {
				TempEvapOutSetPoint = Node( PlantLoop( LoopNum ).TempSetPointNodeNum ).TempSetPointHi;
			}
		}}
		EvapDeltaTemp = std::abs( Node( EvapInletNode ).Temp - TempEvapOutSetPoint );
		this->EvapInletTemp = Node( EvapInletNode ).Temp;

		//If no component demand, or chiller OFF, or Chiller type set to 'Passive' by free
		//cooling heat exchanger, then set condenser side flow and heat transfer rates set to zero
		if ( MyLoad >= 0.0 || ! RunFlag ) {

			//If Chiller load is 0 or greater or chiller is not running then leave the subroutine.Before leaving
			//if the component control is SERIESACTIVE we set the component flow to inlet flow so that
			//flow resolver will not shut down the branch
			if ( EquipFlowCtrl == ControlType_SeriesActive || PlantLoop( LoopNum ).LoopSide( LoopSideNum ).FlowLock == 1 ) {
				this->EvapMassFlowRate = Node( EvapInletNode ).MassFlowRate;
			} else {
				this->EvapMassFlowRate = 0.0;
				SetComponentFlowRate( this->EvapMassFlowRate, EvapInletNode, EvapOutletNode, this->CWLoopNum, this->CWLoopSideNum, this->CWBranchNum, this->CWCompNum );
			}
			if ( this->CondenserType == WaterCooled ) {
				if ( PlantLoop( this->CDLoopNum ).LoopSide( this->CDLoopSideNum ).Branch( this->CDBranchNum ).Comp( this->CDCompNum ).FlowCtrl == ControlType_SeriesActive ) {
					this->CondMassFlowRate = Node( CondInletNode ).MassFlowRate;
				} else {
					this->CondMassFlowRate = 0.0;
					SetComponentFlowRate( this->CondMassFlowRate, CondInletNode, CondOutletNode, this->CDLoopNum, this->CDLoopSideNum, this->CDBranchNum, this->CDCompNum );
				}
			}

			this->EvapOutletTemp = Node( EvapInletNode ).Temp;
			this->CondOutletTemp = Node( CondInletNode ).Temp;

			this->Power = 0.0;
			this->QEvaporator = 0.0;
			this->QCondenser = 0.0;
			this->Energy = 0.0;
			this->EvaporatorEnergy = 0.0;
			this->CondenserEnergy = 0.0;

			if ( this->CondenserType == EvapCooled ) {
				CalcBasinHeaterPower( this->BasinHeaterPowerFTempDiff, this->BasinHeaterSchedulePtr, this->BasinHeaterSetPointTemp, this->BasinHeaterPower );
			}
			this->PrintMessage = false;
			return;
		}

		//   calculate end time of current time step
		CurrentEndTime = CurrentTime + SysTimeElapsed;

		//   Print warning messages only when valid and only for the first ocurrance. Let summary provide statistics.
		//   Wait for next time step to print warnings. If simulation iterates, print out
		//   the warning for the last iteration only. Must wait for next time step to accomplish this.
		//   If a warning occurs and the simulation down shifts, the warning is not valid.
		if ( CurrentEndTime > CurrentEndTimeLast && TimeStepSys >= TimeStepSysLast ) {
			if ( this->PrintMessage ) {
				++this->MsgErrorCount;
				//       Show single warning and pass additional info to ShowRecurringWarningErrorAtEnd
				if ( this->MsgErrorCount < 2 ) {
					ShowWarningError( this->MsgBuffer1 + '.' );
					ShowContinueError( this->MsgBuffer2 );
				} else {
					ShowRecurringWarningErrorAtEnd( this->MsgBuffer1 + " error continues.", this->ErrCount1, this->MsgDataLast, this->MsgDataLast, _, "[C]", "[C]" );
				}
			}
		}

		//   save last system time step and last end time of current time step (used to determine if warning is valid)
		TimeStepSysLast = TimeStepSys;
		CurrentEndTimeLast = CurrentEndTime;

		//otherwise the chiller is running...

		if ( this->CondenserType == AirCooled ) { //Condenser inlet temp = outdoor temp
			Node( CondInletNode ).Temp = Node( CondInletNode ).OutAirDryBulb;
			//  Warn user if entering condenser temperature falls below 0C
			if ( Node( CondInletNode ).Temp < 0.0 && ! WarmupFlag ) {
				this->PrintMessage = true;
				gio::write( OutputChar, OutputFormat ) << Node( CondInletNode ).Temp;
				this->MsgBuffer1 = "CalcConstCOPChillerModel - Chiller:ConstantCOP \"" + this->Name + "\" - Air Cooled Condenser Inlet Temperature below 0C";
				this->MsgBuffer2 = "... Outdoor Dry-bulb Condition = " + OutputChar + " C. Occurrence info = " + EnvironmentName + ", " + CurMnDy + ' ' + CreateSysTimeIntervalString();
				this->MsgDataLast = Node( CondInletNode ).Temp;
			} else {
				this->PrintMessage = false;
			}
		} else if ( this->CondenserType == EvapCooled ) { //Condenser inlet temp = (outdoor wet bulb)
			Node( CondInletNode ).Temp = Node( CondInletNode ).OutAirWetBulb;
			//  Warn user if evap condenser wet bulb temperature falls below 10C
			if ( Node( CondInletNode ).Temp < 10.0 && ! WarmupFlag ) {
				this->PrintMessage = true;
				gio::write( OutputChar, OutputFormat ) << Node( CondInletNode ).Temp;
				this->MsgBuffer1 = "CalcConstCOPChillerModel - Chiller:ConstantCOP \"" + this->Name + "\" - Evap Cooled Condenser Inlet Temperature below 10C";
				this->MsgBuffer2 = "... Outdoor Wet-bulb Condition = " + OutputChar + " C. Occurrence info = " + EnvironmentName + ", " + CurMnDy + ' ' + CreateSysTimeIntervalString();
				this->MsgDataLast = Node( CondInletNode ).Temp;
			} else {
				this->PrintMessage = false;
			}
		} // End of the Air Cooled/Evap Cooled Logic block

		// If not air or evap cooled then set to the condenser node that is attached to a cooling tower
		this->CondInletTemp = Node( CondInletNode ).Temp;

		//Set condenser flow rate
		if ( this->CondenserType == WaterCooled ) {
			this->CondMassFlowRate = this->CondMassFlowRateMax;
			SetComponentFlowRate( this->CondMassFlowRate, CondInletNode, CondOutletNode, this->CDLoopNum, this->CDLoopSideNum, this->CDBranchNum, this->CDCompNum );
			PullCompInterconnectTrigger( this->CWLoopNum, this->CWLoopSideNum, this->CWBranchNum, this->CWCompNum, this->CondMassFlowIndex, this->CDLoopNum, this->CDLoopSideNum, CriteriaType_MassFlowRate, this->CondMassFlowRate );

			if ( this->CondMassFlowRate < MassFlowTolerance ) return;

		}

		// If FlowLock is True, the new resolved mdot is used to update Power, QEvap, Qcond, and
		// condenser side outlet temperature.

		Cp = GetSpecificHeatGlycol( PlantLoop( this->CWLoopNum ).FluidName, Node( EvapInletNode ).Temp, PlantLoop( this->CWLoopNum ).FluidIndex, RoutineName );

		if ( PlantLoop( LoopNum ).LoopSide( LoopSideNum ).FlowLock == 0 ) {
			this->PossibleSubcooling = false;
			this->QEvaporator = std::abs( MyLoad );
			this->Power = std::abs( MyLoad ) / this->COP;

			// Either set the flow to the Constant value or caluclate the flow for the variable volume
			if ( ( this->FlowMode == ConstantFlow ) || ( this->FlowMode == NotModulated ) ) {

				// Start by assuming max (design) flow
				this->EvapMassFlowRate = this->EvapMassFlowRateMax;
				// Use SetComponentFlowRate to decide actual flow
				SetComponentFlowRate( this->EvapMassFlowRate, EvapInletNode, EvapOutletNode, this->CWLoopNum, this->CWLoopSideNum, this->CWBranchNum, this->CWCompNum );
				// Evaluate delta temp based on actual flow rate
				if ( this->EvapMassFlowRate != 0.0 ) {
					EvapDeltaTemp = this->QEvaporator / this->EvapMassFlowRate / Cp;
				} else {
					EvapDeltaTemp = 0.0;
				}
				// Evaluate outlet temp based on delta
				this->EvapOutletTemp = Node( EvapInletNode ).Temp - EvapDeltaTemp;

			} else if ( this->FlowMode == LeavingSetPointModulated ) {

				// Calculate the Delta Temp from the inlet temp to the chiller outlet setpoint
				{ auto const SELECT_CASE_var( PlantLoop( this->CWLoopNum ).LoopDemandCalcScheme );
				if ( SELECT_CASE_var == SingleSetPoint ) {
					EvapDeltaTemp = std::abs( Node( EvapInletNode ).Temp - Node( EvapOutletNode ).TempSetPoint );
				} else if ( SELECT_CASE_var == DualSetPointDeadBand ) {
					EvapDeltaTemp = std::abs( Node( EvapInletNode ).Temp - Node( EvapOutletNode ).TempSetPointHi );
				}}

				if ( EvapDeltaTemp > DeltaTempTol ) {
					this->EvapMassFlowRate = std::abs( this->QEvaporator / Cp / EvapDeltaTemp );
					if ( ( this->EvapMassFlowRate - this->EvapMassFlowRateMax ) > MassFlowTolerance ) this->PossibleSubcooling = true;
					//Check to see if the Maximum is exceeded, if so set to maximum
					this->EvapMassFlowRate = min( this->EvapMassFlowRateMax, this->EvapMassFlowRate );
					// Use SetComponentFlowRate to decide actual flow
					SetComponentFlowRate( this->EvapMassFlowRate, EvapInletNode, EvapOutletNode, this->CWLoopNum, this->CWLoopSideNum, this->CWBranchNum, this->CWCompNum );
					{ auto const SELECT_CASE_var( PlantLoop( this->CWLoopNum ).LoopDemandCalcScheme );
					if ( SELECT_CASE_var == SingleSetPoint ) {
						this->EvapOutletTemp = Node( EvapOutletNode ).TempSetPoint;
					} else if ( SELECT_CASE_var == DualSetPointDeadBand ) {
						this->EvapOutletTemp = Node( EvapOutletNode ).TempSetPointHi;
					}}
				} else {
					// Try to request zero flow
					this->EvapMassFlowRate = 0.0;
					// Use SetComponentFlowRate to decide actual flow
					SetComponentFlowRate( this->EvapMassFlowRate, EvapInletNode, EvapOutletNode, this->CWLoopNum, this->CWLoopSideNum, this->CWBranchNum, this->CWCompNum );
					// No deltaT since component is not running
					this->EvapOutletTemp = Node( EvapInletNode ).Temp;

				}
			} //End of Constant or Variable Flow If Block for FlowLock = 0 (or making a flow request)
		} else { // If FlowLock is True

			this->EvapMassFlowRate = Node( EvapInletNode ).MassFlowRate;
			SetComponentFlowRate( this->EvapMassFlowRate, EvapInletNode, EvapOutletNode, this->CWLoopNum, this->CWLoopSideNum, this->CWBranchNum, this->CWCompNum );
			//   Some other component set the flow to 0. No reason to continue with calculations.
			if ( this->EvapMassFlowRate == 0.0 ) {
				MyLoad = 0.0;
				if ( this->CondenserType == EvapCooled ) {
					CalcBasinHeaterPower( this->BasinHeaterPowerFTempDiff, this->BasinHeaterSchedulePtr, this->BasinHeaterSetPointTemp, this->BasinHeaterPower );
				}
				this->PrintMessage = false;
				return;
			}

			//Recalculate the Delts Temp
			if ( this->PossibleSubcooling ) {
				this->QEvaporator = std::abs( MyLoad );
				EvapDeltaTemp = this->QEvaporator / this->EvapMassFlowRate / Cp;
				this->EvapOutletTemp = Node( EvapInletNode ).Temp - EvapDeltaTemp;
				if ( this->EvapOutletTemp < Node( EvapOutletNode ).TempMin ) {
					this->EvapOutletTemp = Node( EvapOutletNode ).TempMin;
					EvapDeltaTemp = Node( EvapInletNode ).Temp - this->EvapOutletTemp;
					this->QEvaporator = this->EvapMassFlowRate * Cp * EvapDeltaTemp;
				}
			} else {
				EvapDeltaTemp = Node( EvapInletNode ).Temp - TempEvapOutSetPoint;
				//Calculate the evaporator heat transfer at the specified flow which could have changed
				//  in the Flow Resolution step.
				this->QEvaporator = std::abs( this->EvapMassFlowRate * Cp * EvapDeltaTemp );
				this->EvapOutletTemp = TempEvapOutSetPoint;
			}
			//Check that the Evap outlet temp honors both plant loop temp low limit and also the chiller low limit
			if ( this->EvapOutletTemp < Node( EvapOutletNode ).TempMin ) {
				if ( ( Node( EvapInletNode ).Temp - Node( EvapOutletNode ).TempMin ) > DeltaTempTol ) {
					this->EvapOutletTemp = Node( EvapOutletNode ).TempMin;
					EvapDeltaTemp = Node( EvapInletNode ).Temp - this->EvapOutletTemp;
					this->QEvaporator = this->EvapMassFlowRate * Cp * EvapDeltaTemp;
				} else {
					this->EvapOutletTemp = Node( EvapInletNode ).Temp;
					EvapDeltaTemp = Node( EvapInletNode ).Temp - this->EvapOutletTemp;
					this->QEvaporator = this->EvapMassFlowRate * Cp * EvapDeltaTemp;
				}
			}
			// If load exceeds the distributed load set to the distributed load
			if ( this->QEvaporator > std::abs( MyLoad ) ) {
				if ( this->EvapMassFlowRate > MassFlowTolerance ) {
					this->QEvaporator = std::abs( MyLoad );
					EvapDeltaTemp = this->QEvaporator / this->EvapMassFlowRate / Cp;
					this->EvapOutletTemp = Node( EvapInletNode ).Temp - EvapDeltaTemp;
				} else {
					this->QEvaporator = 0.0;
					this->EvapOutletTemp = Node( EvapInletNode ).Temp;
				}
			}

			// Checks QEvaporator on the basis of the machine limits.
			if ( this->QEvaporator > this->NomCap ) {
				if ( this->EvapMassFlowRate > MassFlowTolerance ) {
					this->QEvaporator = this->NomCap;
					EvapDeltaTemp = this->QEvaporator / this->EvapMassFlowRate / Cp;
					this->EvapOutletTemp = Node( EvapInletNode ).Temp - EvapDeltaTemp;
				} else {
					this->QEvaporator = 0.0;
					this->EvapOutletTemp = Node( EvapInletNode ).Temp;
				}
			}
			//Calculate the Power consumption of the Const COP chiller which is a simplified calculation
			this->Power = this->QEvaporator / this->COP;
			if ( this->EvapMassFlowRate == 0.0 ) {
				this->QEvaporator = 0.0;
				this->EvapOutletTemp = Node( EvapInletNode ).Temp;
				this->Power = 0.0;
				this->PrintMessage = false;
			}
			if ( this->QEvaporator == 0.0 && this->CondenserType == EvapCooled ) {
				CalcBasinHeaterPower( this->BasinHeaterPowerFTempDiff, this->BasinHeaterSchedulePtr, this->BasinHeaterSetPointTemp, this->BasinHeaterPower );
			}

		} //This is the end of the FlowLock Block

		//QCondenser is calculated the same for each type, but the power consumption should be different
		//  depending on the performance coefficients used for the chiller model.
		this->QCondenser = this->Power + this->QEvaporator;

		if ( this->CondenserType == WaterCooled ) {
			CpCond = GetSpecificHeatGlycol( PlantLoop( this->CDLoopNum ).FluidName, this->CondInletTemp, PlantLoop( this->CDLoopNum ).FluidIndex, RoutineName );
			if ( this->CondMassFlowRate > MassFlowTolerance ) {
				this->CondOutletTemp = this->QCondenser / this->CondMassFlowRate / CpCond + this->CondInletTemp;
			} else {
				ShowSevereError( "CalcConstCOPChillerModel: Condenser flow = 0, for CONST COP Chiller=" + this->Name );
				ShowContinueErrorTimeStamp( "" );

			}
		} else { // Air Cooled or Evap Cooled
			//  Set condenser outlet temp to condenser inlet temp for Air Cooled or Evap Cooled
			//  since there is no CondMassFlowRate and would divide by zero
			this->CondOutletTemp = this->CondInletTemp;
		}

		//Calculate Energy
		this->CondenserEnergy = this->QCondenser * TimeStepSys * SecInHour;
		this->Energy = this->Power * TimeStepSys * SecInHour;
		this->EvaporatorEnergy = this->QEvaporator * TimeStepSys * SecInHour;

		//check for problems BG 9/12/06 (deal with observed negative energy results)
		if ( this->Energy < 0.0 ) { // there is a serious problem

			if ( this->CondenserType == WaterCooled ) {
				// first check for run away condenser loop temps (only reason yet to be observed for this?)
				if ( this->CondInletTemp > 70.0 ) {
					ShowSevereError( "CalcConstCOPChillerModel: Condenser loop inlet temperatures over 70.0 C for ConstCOPChiller=" + this->Name );
					ShowContinueErrorTimeStamp( "" );
					ShowContinueError( "Condenser loop water temperatures are too high at" + RoundSigDigits( this->CondInletTemp, 2 ) );
					ShowContinueError( "Check input for condenser plant loop, especially cooling tower" );
					ShowContinueError( "Evaporator inlet temperature: " + RoundSigDigits( Node( EvapInletNode ).Temp, 2 ) );

					ShowFatalError( "Program Terminates due to previous error condition" );
				}
			}
			// If makes it here, set limits, chiller can't have negative energy/power
			// proceeding silently for now but may want to throw error here
			this->Power = 0.0;
			this->Energy = 0.0;

		}
	}

	void
	ElectricChillerSpecs::CalcElectricChillerHeatRecovery(
		Real64 & QCond, // current condenser load
		Real64 const CondMassFlow, // current condenser Mass Flow
		Real64 const CondInletTemp, // current condenser Inlet Temp
		Real64 & QHeatRec // amount of heat recovered
	)
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR:          Richard Liesen
		//       DATE WRITTEN:    January 2004

		// PURPOSE OF THIS SUBROUTINE:
		// Calculate the heat recovered from the chiller condenser

		// METHODOLOGY EMPLOYED: na

		// REFERENCES: na

		// Using/Aliasing
		using Psychrometrics::PsyCpAirFnWTdb;
		using FluidProperties::GetSpecificHeatGlycol;
		using DataPlant::PlantLoop;
		using DataPlant::SingleSetPoint;
		using DataPlant::DualSetPointDeadBand;
		using ScheduleManager::GetCurrentScheduleValue;

		// Locals
		Real64 HeatRecInletTemp;

		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "ChillerHeatRecovery" );

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int CondInletNode; // condenser inlet node number, water side
		int CondOutletNode; // condenser outlet node number, water side
		int HeatRecInNode;
		int HeatRecOutNode;
		Real64 QTotal;
		Real64 HeatRecMassFlowRate;
		Real64 TAvgIn;
		Real64 TAvgOut;
		Real64 CpHeatRec;
		Real64 CpCond;
		Real64 THeatRecSetPoint( 0.0 );
		Real64 QHeatRecToSetPoint;
		Real64 HeatRecHighInletLimit;

		// Begin routine
		HeatRecInNode = this->HeatRecInletNodeNum;
		HeatRecOutNode = this->HeatRecOutletNodeNum;
		CondInletNode = this->CondInletNodeNum;
		CondOutletNode = this->CondOutletNodeNum;
		HeatRecInletTemp = Node( HeatRecInNode ).Temp;
		HeatRecMassFlowRate = Node( HeatRecInNode ).MassFlowRate;

		CpHeatRec = GetSpecificHeatGlycol( PlantLoop( this->HRLoopNum ).FluidName, HeatRecInletTemp, PlantLoop( this->HRLoopNum ).FluidIndex, RoutineName );

		if ( this->CondenserType == WaterCooled ) {
			CpCond = GetSpecificHeatGlycol( PlantLoop( this->CDLoopNum ).FluidName, CondInletTemp, PlantLoop( this->CDLoopNum ).FluidIndex, RoutineName );
		} else {
			CpCond = PsyCpAirFnWTdb( Node( CondInletNode ).HumRat, CondInletTemp );
		}

		// Before we modify the QCondenser, the total or original value is transferred to QTot
		QTotal = QCond;

		if ( this->HeatRecSetPointNodeNum == 0 ) { // use original algorithm that blends temps
			TAvgIn = ( HeatRecMassFlowRate * CpHeatRec * HeatRecInletTemp + CondMassFlow * CpCond * CondInletTemp ) / ( HeatRecMassFlowRate * CpHeatRec + CondMassFlow * CpCond );

			TAvgOut = QTotal / ( HeatRecMassFlowRate * CpHeatRec + CondMassFlow * CpCond ) + TAvgIn;

			QHeatRec = HeatRecMassFlowRate * CpHeatRec * ( TAvgOut - HeatRecInletTemp );
			QHeatRec = max( QHeatRec, 0.0 ); // ensure non negative
			//check if heat flow too large for physical size of bundle
			QHeatRec = min( QHeatRec, this->HeatRecMaxCapacityLimit );
		} else { // use new algorithm to meet setpoint
			{ auto const SELECT_CASE_var( PlantLoop( this->HRLoopNum ).LoopDemandCalcScheme );

			if ( SELECT_CASE_var == SingleSetPoint ) {
				THeatRecSetPoint = Node( this->HeatRecSetPointNodeNum ).TempSetPoint;
			} else if ( SELECT_CASE_var == DualSetPointDeadBand ) {
				THeatRecSetPoint = Node( this->HeatRecSetPointNodeNum ).TempSetPointHi;
			}}

			QHeatRecToSetPoint = HeatRecMassFlowRate * CpHeatRec * ( THeatRecSetPoint - HeatRecInletTemp );
			QHeatRecToSetPoint = max( QHeatRecToSetPoint, 0.0 );
			QHeatRec = min( QTotal, QHeatRecToSetPoint );
			//check if heat flow too large for physical size of bundle
			QHeatRec = min( QHeatRec, this->HeatRecMaxCapacityLimit );

		}
		// check if limit on inlet is present and exceeded.
		if ( this->HeatRecInletLimitSchedNum > 0 ) {
			HeatRecHighInletLimit = GetCurrentScheduleValue( this->HeatRecInletLimitSchedNum );
			if ( HeatRecInletTemp > HeatRecHighInletLimit ) { // shut down heat recovery
				QHeatRec = 0.0;
			}
		}

		QCond = QTotal - QHeatRec;

		// Calculate a new Heat Recovery Coil Outlet Temp
		if ( HeatRecMassFlowRate > 0.0 ) {
			this->HeatRecOutletTemp = QHeatRec / ( HeatRecMassFlowRate * CpHeatRec ) + HeatRecInletTemp;
		} else {
			this->HeatRecOutletTemp = HeatRecInletTemp;
		}

	}

	void
	EngineDrivenChillerSpecs::CalcEngineChillerHeatRec(
		Real64 const EnergyRecovered, // Amount of heat recovered
		Real64 & HeatRecRatio // Max Heat recovery ratio
	)
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR:          Brandon Anderson
		//       DATE WRITTEN:    November 2000

		// PURPOSE OF THIS SUBROUTINE:
		// To perform heat recovery calculations and node updates

		// METHODOLOGY EMPLOYED: This routine is required for the heat recovery loop.
		// It works in conjunction with the Heat Recovery Manager, and the PlantWaterHeater.
		// The chiller sets the flow on the loop first by the input design flowrate and then
		// performs a check to verify that

		// REFERENCES: na

		// USE STATEMENTS: na
		// Using/Aliasing
		using Psychrometrics::PsyCpAirFnWTdb;
		using FluidProperties::GetSpecificHeatGlycol;
		using DataPlant::PlantLoop;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "ChillerHeatRecovery" );

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int HeatRecInNode;
		int HeatRecOutNode;
		Real64 HeatRecMdot;
		Real64 MinHeatRecMdot( 0.0 );
		Real64 HeatRecInTemp;
		Real64 HeatRecOutTemp;
		Real64 HeatRecCp;

		//Load inputs to local structure
		HeatRecInNode = this->HeatRecInletNodeNum;
		HeatRecOutNode = this->HeatRecOutletNodeNum;

		//Need to set the HeatRecRatio to 1.0 if it is not modified
		HeatRecRatio = 1.0;

		//  !This mdot is input specified mdot "Desired Flowrate", already set in init routine
		HeatRecMdot = Node( HeatRecInNode ).MassFlowRate;

		HeatRecInTemp = Node( HeatRecInNode ).Temp;
		HeatRecCp = GetSpecificHeatGlycol( PlantLoop( this->HRLoopNum ).FluidName, this->HeatRecInletTemp, PlantLoop( this->HRLoopNum ).FluidIndex, RoutineName );

		//Don't divide by zero - Note This also results in no heat recovery when
		//  design Mdot for Heat Recovery - Specified on Chiller Input - is zero
		//  In order to see what minimum heat recovery flow rate is for the design temperature
		//  The design heat recovery flow rate can be set very small, but greater than zero.
		if ( ( HeatRecMdot > 0 ) && ( HeatRecCp > 0 ) ) {
			HeatRecOutTemp = ( EnergyRecovered ) / ( HeatRecMdot * HeatRecCp ) + HeatRecInTemp;
		} else {
			HeatRecOutTemp = HeatRecInTemp;
		}

		//Now verify that the design flowrate was large enough to prevent phase change
		if ( HeatRecOutTemp > this->HeatRecMaxTemp ) {
			if ( this->HeatRecMaxTemp != HeatRecInTemp ) {
				MinHeatRecMdot = ( EnergyRecovered ) / ( HeatRecCp * ( this->HeatRecMaxTemp - HeatRecInTemp ) );
				if ( MinHeatRecMdot < 0.0 ) MinHeatRecMdot = 0.0;
			}

			//Recalculate Outlet Temperature, with adjusted flowrate
			if ( ( MinHeatRecMdot > 0.0 ) && ( HeatRecCp > 0.0 ) ) {
				HeatRecOutTemp = ( EnergyRecovered ) / ( MinHeatRecMdot * HeatRecCp ) + HeatRecInTemp;
				HeatRecRatio = HeatRecMdot / MinHeatRecMdot;
			} else {
				HeatRecOutTemp = HeatRecInTemp;
				HeatRecRatio = 0.0;
			}

		}

		//Update global variables for reporting later
		this->HeatRecInletTemp = HeatRecInTemp;
		this->HeatRecOutletTemp = HeatRecOutTemp;
		this->HeatRecMdotActual = HeatRecMdot;

	}

	void
	ElectricChillerSpecs::UpdateElectricChillerRecords(
		Real64 const MyLoad, // current load
		bool const RunFlag // TRUE if chiller operating
	)
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR:          Dan Fisher / Brandon Anderson
		//       DATE WRITTEN:    September 2000

		// PURPOSE OF THIS SUBROUTINE:
		// reporting

		// METHODOLOGY EMPLOYED: na

		// REFERENCES: na

		// USE STATEMENTS: na
		// Using/Aliasing
		using DataGlobals::SecInHour;
		using DataHVACGlobals::TimeStepSys;
		using PlantUtilities::SetComponentFlowRate;
		using PlantUtilities::SafeCopyPlantNode;
		using Psychrometrics::PsyHFnTdbW;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int EvapInletNode; // evaporator inlet node number, water side
		int EvapOutletNode; // evaporator outlet node number, water side
		int CondInletNode; // condenser inlet node number, water side
		int CondOutletNode; // condenser outlet node number, water side
		int HeatRecInNode;
		int HeatRecOutNode;
		Real64 ReportingConstant; // Number of seconds per HVAC system time step, to convert from W (J/s) to J

		ReportingConstant = TimeStepSys * SecInHour;

		EvapInletNode = this->EvapInletNodeNum;
		EvapOutletNode = this->EvapOutletNodeNum;
		CondInletNode = this->CondInletNodeNum;
		CondOutletNode = this->CondOutletNodeNum;
		HeatRecInNode = this->HeatRecInletNodeNum;
		HeatRecOutNode = this->HeatRecOutletNodeNum;

		if ( MyLoad >= 0.0 || ! RunFlag ) { //Chiller not running so pass inlet states to outlet states
			//set node temperatures
			Node( EvapOutletNode ).Temp = Node( EvapInletNode ).Temp;
			Node( CondOutletNode ).Temp = Node( CondInletNode ).Temp;
			if ( this->CondenserType != WaterCooled ) {
				Node( CondOutletNode ).HumRat = Node( CondInletNode ).HumRat;
				Node( CondOutletNode ).Enthalpy = Node( CondInletNode ).Enthalpy;
			}

			this->reports.Power = 0.0;
			this->reports.QEvap = 0.0;
			this->reports.QCond = 0.0;
			this->reports.Energy = 0.0;
			this->reports.EvapEnergy = 0.0;
			this->reports.CondEnergy = 0.0;
			this->reports.EvapInletTemp = Node( EvapInletNode ).Temp;
			this->reports.CondInletTemp = Node( CondInletNode ).Temp;
			this->reports.CondOutletTemp = Node( CondOutletNode ).Temp;
			this->reports.EvapOutletTemp = Node( EvapOutletNode ).Temp;
			this->reports.Evapmdot = this->EvapMassFlowRate;
			this->reports.Condmdot = this->CondMassFlowRate;
			this->reports.ActualCOP = 0.0;
			if ( this->CondenserType == EvapCooled ) {
				this->reports.BasinHeaterPower = this->BasinHeaterPower;
				this->reports.BasinHeaterConsumption = this->BasinHeaterPower * ReportingConstant;
			}

			if ( this->HeatRecActive ) {

				SafeCopyPlantNode( HeatRecInNode, HeatRecOutNode );

				this->reports.QHeatRecovery = 0.0;
				this->reports.EnergyHeatRecovery = 0.0;
				this->reports.HeatRecInletTemp = Node( HeatRecInNode ).Temp;
				this->reports.HeatRecOutletTemp = Node( HeatRecOutNode ).Temp;
				this->reports.HeatRecMassFlow = Node( HeatRecInNode ).MassFlowRate;

				this->reports.ChillerCondAvgTemp = this->AvgCondSinkTemp;
			}

		} else { //Chiller is running, so pass calculated values
			//set node temperatures
			Node( EvapOutletNode ).Temp = this->EvapOutletTemp;
			Node( CondOutletNode ).Temp = this->CondOutletTemp;
			if ( this->CondenserType != WaterCooled ) {
				Node( CondOutletNode ).HumRat = this->CondOutletHumRat;
				Node( CondOutletNode ).Enthalpy = PsyHFnTdbW( this->CondOutletTemp, this->CondOutletHumRat );
			}
			//set node flow rates;  for these load based models
			//assume that the sufficient evaporator flow rate available
			this->reports.Power = this->Power;
			this->reports.QEvap = this->QEvaporator;
			this->reports.QCond = this->QCondenser;
			this->reports.Energy = this->Energy;
			this->reports.EvapEnergy = this->EvaporatorEnergy;
			this->reports.CondEnergy = this->CondenserEnergy;
			this->reports.EvapInletTemp = Node( EvapInletNode ).Temp;
			this->reports.CondInletTemp = Node( CondInletNode ).Temp;
			this->reports.CondOutletTemp = Node( CondOutletNode ).Temp;
			this->reports.EvapOutletTemp = Node( EvapOutletNode ).Temp;
			this->reports.Evapmdot = this->EvapMassFlowRate;
			this->reports.Condmdot = this->CondMassFlowRate;
			if ( this->CondenserType == EvapCooled ) {
				this->reports.BasinHeaterPower = this->BasinHeaterPower;
				this->reports.BasinHeaterConsumption = this->BasinHeaterPower * ReportingConstant;
			}
			if ( this->Power != 0.0 ) {
				this->reports.ActualCOP = this->QEvaporator / this->Power;
			} else {
				this->reports.ActualCOP = 0.0;
			}

			if ( this->HeatRecActive ) {

				SafeCopyPlantNode( HeatRecInNode, HeatRecOutNode );
				this->reports.QHeatRecovery = this->QHeatRecovered;
				this->reports.EnergyHeatRecovery = this->QHeatRecovered * TimeStepSys * SecInHour;
				Node( HeatRecOutNode ).Temp = this->HeatRecOutletTemp;
				this->reports.HeatRecInletTemp = Node( HeatRecInNode ).Temp;
				this->reports.HeatRecOutletTemp = Node( HeatRecOutNode ).Temp;
				this->reports.HeatRecMassFlow = Node( HeatRecInNode ).MassFlowRate;
				this->reports.ChillerCondAvgTemp = this->AvgCondSinkTemp;
			}

		}
	}

	void
	EngineDrivenChillerSpecs::UpdateEngineDrivenChiller(
		Real64 const MyLoad, // current load
		bool const RunFlag // TRUE if chiller operating
	)
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR:          Dan Fisher / Brandon Anderson
		//       DATE WRITTEN:    September 2000

		// PURPOSE OF THIS SUBROUTINE:
		// reporting

		// METHODOLOGY EMPLOYED: na

		// REFERENCES: na

		// USE STATEMENTS: na

		// Using/Aliasing
		using DataGlobals::SecInHour;
		using DataHVACGlobals::TimeStepSys;
		using PlantUtilities::SafeCopyPlantNode;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int EvapInletNode; // evaporator inlet node number, water side
		int EvapOutletNode; // evaporator outlet node number, water side
		int CondInletNode; // condenser inlet node number, water side
		int CondOutletNode; // condenser outlet node number, water side
		int HeatRecInletNode;
		int HeatRecOutletNode;
		Real64 ReportingConstant; // Number of seconds per HVAC system time step, to convert from W (J/s) to J

		ReportingConstant = TimeStepSys * SecInHour;

		EvapInletNode = this->EvapInletNodeNum;
		EvapOutletNode = this->EvapOutletNodeNum;
		CondInletNode = this->CondInletNodeNum;
		CondOutletNode = this->CondOutletNodeNum;

		HeatRecInletNode = this->HeatRecInletNodeNum;
		HeatRecOutletNode = this->HeatRecOutletNodeNum;

		if ( MyLoad >= 0.0 || ! RunFlag ) { //Chiller not running
			//set node temperatures
			Node( EvapOutletNode ).Temp = Node( EvapInletNode ).Temp;
			Node( CondOutletNode ).Temp = Node( CondInletNode ).Temp;

			this->reports.Power = 0.0;
			this->reports.QEvap = 0.0;
			this->reports.QCond = 0.0;
			this->reports.Energy = 0.0;
			this->reports.EvapEnergy = 0.0;
			this->reports.CondEnergy = 0.0;
			this->reports.EvapInletTemp = Node( EvapInletNode ).Temp;
			this->reports.CondInletTemp = Node( CondInletNode ).Temp;
			this->reports.CondOutletTemp = Node( CondOutletNode ).Temp;
			this->reports.EvapOutletTemp = Node( EvapOutletNode ).Temp;
			this->reports.Evapmdot = this->EvapMassFlowRate;
			this->reports.Condmdot = this->CondMassFlowRate;
			this->reports.FuelCOP = 0.0;
			if ( this->CondenserType == EvapCooled ) {
				this->reports.BasinHeaterPower = this->BasinHeaterPower;
				this->reports.BasinHeaterConsumption = this->BasinHeaterPower * ReportingConstant;
			}
		} else { //Chiller is running
			//set node temperatures
			Node( EvapOutletNode ).Temp = this->EvapOutletTemp;
			Node( CondOutletNode ).Temp = this->CondOutletTemp;

			this->reports.Power = this->Power;
			this->reports.QEvap = this->QEvaporator;
			this->reports.QCond = this->QCondenser;
			this->reports.Energy = this->Energy;
			this->reports.EvapEnergy = this->EvaporatorEnergy;
			this->reports.CondEnergy = this->CondenserEnergy;
			this->reports.EvapInletTemp = Node( EvapInletNode ).Temp;
			this->reports.CondInletTemp = Node( CondInletNode ).Temp;
			this->reports.CondOutletTemp = Node( CondOutletNode ).Temp;
			this->reports.EvapOutletTemp = Node( EvapOutletNode ).Temp;
			this->reports.Evapmdot = this->EvapMassFlowRate;
			this->reports.Condmdot = this->CondMassFlowRate;
			if ( this->FuelEnergyUseRate != 0.0 ) {
				this->reports.FuelCOP = this->QEvaporator / this->FuelEnergyUseRate;
			} else {
				this->reports.FuelCOP = 0.0;
			}
			if ( this->CondenserType == EvapCooled ) {
				this->reports.BasinHeaterPower = this->BasinHeaterPower;
				this->reports.BasinHeaterConsumption = this->BasinHeaterPower * ReportingConstant;
			}
		}

		// Update Heat Recovery Stuff whether running or not, variables should be set correctly
		this->reports.FuelEnergyUseRate = this->FuelEnergyUseRate;
		this->reports.JacketEnergyRec = this->JacketEnergyRec;
		this->reports.LubeOilEnergyRec = this->LubeOilEnergyRec;
		this->reports.ExhaustEnergyRec = this->ExhaustEnergyRec;
		this->reports.TotalHeatEnergyRec = this->TotalHeatEnergyRec;
		this->reports.FuelEnergy = this->FuelEnergy;
		this->reports.FuelMdot = this->FuelMdot;
		this->reports.ExhaustStackTemp = this->ExhaustStackTemp;
		this->reports.HeatRecInletTemp = this->HeatRecInletTemp;
		this->reports.HeatRecOutletTemp = this->HeatRecOutletTemp;
		this->reports.HeatRecMdot = this->HeatRecMdotActual;

		//Update the Heat Recovery outlet
		if ( this->HeatRecActive ) {
			SafeCopyPlantNode( HeatRecInletNode, HeatRecOutletNode );
			Node( HeatRecOutletNode ).Temp = this->HeatRecOutletTemp;
		}

	}

	void
	GTChillerSpecs::UpdateGTChillerRecords(
		Real64 const MyLoad, // current load
		bool const RunFlag // TRUE if chiller operating
	)
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR:          Dan Fisher / Brandon Anderson
		//       DATE WRITTEN:    September 2000

		// PURPOSE OF THIS SUBROUTINE:
		// reporting

		// METHODOLOGY EMPLOYED: na

		// REFERENCES: na

		// USE STATEMENTS: na
		// Using/Aliasing
		using DataGlobals::SecInHour;
		using DataHVACGlobals::TimeStepSys;
		using PlantUtilities::SafeCopyPlantNode;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int EvapInletNode; // evaporator inlet node number, water side
		int EvapOutletNode; // evaporator outlet node number, water side
		int CondInletNode; // condenser inlet node number, water side
		int CondOutletNode; // condenser outlet node number, water side

		int HeatRecInletNode;
		int HeatRecOutletNode;
		Real64 ReportingConstant; // Number of seconds per HVAC system time step, to convert from W (J/s) to J

		ReportingConstant = TimeStepSys * SecInHour;

		EvapInletNode = this->EvapInletNodeNum;
		EvapOutletNode = this->EvapOutletNodeNum;
		CondInletNode = this->CondInletNodeNum;
		CondOutletNode = this->CondOutletNodeNum;
		if ( this->HeatRecActive ) {
			HeatRecInletNode = this->HeatRecInletNodeNum;
			HeatRecOutletNode = this->HeatRecOutletNodeNum;
		}

		if ( MyLoad >= 0.0 || ! RunFlag ) { //Chiller not running so pass inlet states to outlet states
			//set node temperatures
			Node( EvapOutletNode ).Temp = Node( EvapInletNode ).Temp;
			Node( CondOutletNode ).Temp = Node( CondInletNode ).Temp;

			if ( this->HeatRecActive ) {
				SafeCopyPlantNode( HeatRecOutletNode, HeatRecInletNode );
				this->reports.HeatRecInletTemp = Node( HeatRecInletNode ).Temp;
				this->reports.HeatRecOutletTemp = Node( HeatRecOutletNode ).Temp;
			}

			this->reports.Power = 0.0;
			this->reports.QEvap = 0.0;
			this->reports.QCond = 0.0;
			this->reports.Energy = 0.0;
			this->reports.EvapEnergy = 0.0;
			this->reports.CondEnergy = 0.0;
			this->reports.EvapInletTemp = Node( EvapInletNode ).Temp;
			this->reports.CondInletTemp = Node( CondInletNode ).Temp;
			this->reports.CondOutletTemp = Node( CondOutletNode ).Temp;
			this->reports.EvapOutletTemp = Node( EvapOutletNode ).Temp;
			this->reports.Evapmdot = this->EvapMassFlowRate;
			this->reports.Condmdot = this->CondMassFlowRate;
			this->reports.FuelEnergyUsedRate = 0.0;
			this->reports.FuelMassUsedRate = 0.0;
			this->reports.FuelEnergyUsed = 0.0;
			this->reports.FuelMassUsed = 0.0;

			this->reports.HeatRecLubeEnergy = 0.0;
			this->reports.HeatRecLubeRate = 0.0;
			this->reports.ExhaustStackTemp = 0.0;
			this->reports.HeatRecMdot = this->HeatRecMdot;
			this->reports.FuelCOP = 0.0;
			if ( this->CondenserType == EvapCooled ) {
				this->reports.BasinHeaterPower = this->BasinHeaterPower;
				this->reports.BasinHeaterConsumption = this->BasinHeaterPower * ReportingConstant;
			}

		} else { //Chiller is running so report calculated values
			//set node temperatures
			Node( EvapOutletNode ).Temp = this->EvapOutletTemp;
			Node( CondOutletNode ).Temp = this->CondOutletTemp;

			if ( this->HeatRecActive ) {
				SafeCopyPlantNode( HeatRecOutletNode, HeatRecInletNode );
				Node( HeatRecOutletNode ).Temp = this->HeatRecOutletTemp;
			}

			this->reports.Power = this->Power;
			this->reports.QEvap = this->QEvaporator;
			this->reports.QCond = this->QCondenser;
			this->reports.Energy = this->Energy;
			this->reports.EvapEnergy = this->EvaporatorEnergy;
			this->reports.CondEnergy = this->CondenserEnergy;
			this->reports.EvapInletTemp = Node( EvapInletNode ).Temp;
			this->reports.CondInletTemp = Node( CondInletNode ).Temp;
			this->reports.CondOutletTemp = Node( CondOutletNode ).Temp;
			this->reports.EvapOutletTemp = Node( EvapOutletNode ).Temp;
			this->reports.Evapmdot = this->EvapMassFlowRate;
			this->reports.Condmdot = this->CondMassFlowRate;

			this->reports.HeatRecLubeEnergy = this->HeatRecLubeEnergy;
			this->reports.HeatRecLubeRate = this->HeatRecLubeRate;
			this->reports.FuelEnergyUsedRate = this->FuelEnergyIn;
			this->reports.FuelMassUsedRate = this->reports.FuelMassUsedRate;
			this->reports.FuelEnergyUsed = this->reports.FuelEnergyUsedRate * TimeStepSys * SecInHour;
			this->reports.FuelMassUsed = this->reports.FuelMassUsedRate * TimeStepSys * SecInHour;
			this->reports.ExhaustStackTemp = this->ExhaustStackTemp;
			this->reports.HeatRecInletTemp = this->HeatRecInletTemp;
			this->reports.HeatRecOutletTemp = this->HeatRecOutletTemp;
			this->reports.HeatRecMdot = this->HeatRecMdot;
			if ( this->reports.FuelEnergyUsedRate != 0.0 ) {
				this->reports.FuelCOP = this->reports.QEvap / this->reports.FuelEnergyUsedRate;
			} else {
				this->reports.FuelCOP = 0.0;
			}
			if ( this->CondenserType == EvapCooled ) {
				this->reports.BasinHeaterPower = this->BasinHeaterPower;
				this->reports.BasinHeaterConsumption = this->BasinHeaterPower * ReportingConstant;
			}
		}
	}

	void
	ConstCOPChillerSpecs::UpdateConstCOPChillerRecords(
		Real64 const MyLoad, // unused1208
		bool const RunFlag // unused1208
	)
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR:          Dan Fisher
		//       DATE WRITTEN:    October 1998

		// PURPOSE OF THIS SUBROUTINE:

		// METHODOLOGY EMPLOYED:
		// REFERENCES:

		// Using/Aliasing
		using DataGlobals::SecInHour;
		using DataHVACGlobals::TimeStepSys;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int EvapInletNode;
		int EvapOutletNode;
		int CondInletNode;
		int CondOutletNode;
		Real64 ReportingConstant; // Number of seconds per HVAC system time step, to convert from W (J/s) to J

		ReportingConstant = TimeStepSys * SecInHour;

		EvapInletNode = this->EvapInletNodeNum;
		EvapOutletNode = this->EvapOutletNodeNum;
		CondInletNode = this->CondInletNodeNum;
		CondOutletNode = this->CondOutletNodeNum;

		if ( MyLoad >= 0.0 || ! RunFlag ) { //Chiller not running so pass inlet states to outlet states
			this->reports.Power = 0.0;
			this->reports.QEvap = 0.0;
			this->reports.QCond = 0.0;
			this->reports.Energy = 0.0;
			this->reports.EvapEnergy = 0.0;
			this->reports.CondEnergy = 0.0;
			this->reports.CondInletTemp = Node( CondInletNode ).Temp;
			this->reports.EvapInletTemp = Node( EvapInletNode ).Temp;
			this->reports.CondOutletTemp = Node( CondInletNode ).Temp;
			this->reports.EvapOutletTemp = Node( EvapInletNode ).Temp;
			this->reports.Evapmdot = this->EvapMassFlowRate;
			this->reports.Condmdot = this->CondMassFlowRate;
			this->reports.ActualCOP = 0.0;
			if ( this->CondenserType == EvapCooled ) {
				this->reports.BasinHeaterPower = this->BasinHeaterPower;
				this->reports.BasinHeaterConsumption = this->BasinHeaterPower * ReportingConstant;
			}

			//set outlet node temperatures
			Node( EvapOutletNode ).Temp = Node( EvapInletNode ).Temp;
			Node( CondOutletNode ).Temp = Node( CondInletNode ).Temp;

		} else {
			this->reports.Power = this->Power;
			this->reports.QEvap = this->QEvaporator;
			this->reports.QCond = this->QCondenser;
			this->reports.Energy = this->Energy;
			this->reports.EvapEnergy = this->EvaporatorEnergy;
			this->reports.CondEnergy = this->CondenserEnergy;
			this->reports.CondInletTemp = Node( CondInletNode ).Temp;
			this->reports.EvapInletTemp = Node( EvapInletNode ).Temp;
			this->reports.CondOutletTemp = this->CondOutletTemp;
			this->reports.EvapOutletTemp = this->EvapOutletTemp;
			this->reports.Evapmdot = this->EvapMassFlowRate;
			this->reports.Condmdot = this->CondMassFlowRate;
			if ( this->Power != 0.0 ) {
				this->reports.ActualCOP = this->QEvaporator / this->Power;
			} else {
				this->reports.ActualCOP = 0.0;
			}
			if ( this->CondenserType == EvapCooled ) {
				this->reports.BasinHeaterPower = this->BasinHeaterPower;
				this->reports.BasinHeaterConsumption = this->BasinHeaterPower * ReportingConstant;
			}

			//set outlet node temperatures
			Node( EvapOutletNode ).Temp = this->EvapOutletTemp;
			Node( CondOutletNode ).Temp = this->CondOutletTemp;
		}

	}

} // PlantChillers

} // EnergyPlus
