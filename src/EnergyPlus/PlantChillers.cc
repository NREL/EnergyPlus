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
	bool GetEngineDrivenInput( true ); // then TRUE, calls subroutine to read input file.
	bool GetElectricInput( true ); // then TRUE, calls subroutine to read input file.
	bool GetGasTurbineInput( true ); // then TRUE, calls subroutine to read input file.
	bool GetConstCOPInput( true );

	// Object Data
	Array1D< ElectricChillerSpecs > ElectricChiller; // dimension to number of machines
	Array1D< ElectricReportVars > ElectricChillerReport;
	Array1D< EngineDrivenChillerSpecs > EngineDrivenChiller; // dimension to number of machines
	Array1D< EngineDrivenReportVars > EngineDrivenChillerReport;
	Array1D< GTChillerSpecs > GTChiller; // dimension to number of machines
	Array1D< GasTurbineReportVars > GTChillerReport;
	Array1D< ConstCOPChillerSpecs > ConstCOPChiller; // dimension to number of machines
	Array1D< ConstCOPReportVars > ConstCOPChillerReport;

	// MODULE SUBROUTINES:

	// Beginning of Electric Chiller Module Driver Subroutines
	//*************************************************************************

	// Functions

	void
	clear_state()
	{
		NumElectricChillers = 0;
		Power = 0.0;
		QEvaporator = 0.0;
		QCondenser = 0.0;
		Energy = 0.0;
		EvaporatorEnergy = 0.0;
		CondenserEnergy = 0.0;
		QHeatRecovered = 0.0;
		HeatRecOutletTemp = 0.0;
		AvgCondSinkTemp = 0.0;
		ChillerCyclingRatio = 0.0;
		BasinHeaterPower = 0.0;
		NumEngineDrivenChillers = 0;
		NumGTChillers = 0;
		NumConstCOPChillers = 0;
		GetEngineDrivenInput = true;
		GetElectricInput = true;
		GetGasTurbineInput = true;
		GetConstCOPInput = true;
		ElectricChiller.deallocate();
		ElectricChillerReport.deallocate();
		EngineDrivenChiller.deallocate();
		EngineDrivenChillerReport.deallocate();
		GTChiller.deallocate();
		GTChillerReport.deallocate();
		ConstCOPChiller.deallocate();
		ConstCOPChillerReport.deallocate();
	}

	void
	SimChiller(
		int const LoopNum, // Flow control mode for the equipment
		int const EP_UNUSED( LoopSide ), // chiller number pointer
		int const ChillerType, // type of chiller
		std::string const & EP_UNUSED(ChillerName), // user specified name of chiller
		int const EquipFlowCtrl, // Flow control mode for the equipment
		int & EP_UNUSED(CompIndex), // chiller number pointer
		bool const RunFlag, // simulate chiller when TRUE
		bool const FirstHVACIteration, // initialize variables when TRUE
		bool & InitLoopEquip, // If not zero, calculate the max load for operating conditions
		Real64 & MyLoad, // loop demand component will meet
		Real64 & MaxCap, // W - maximum operating capacity of chiller
		Real64 & MinCap, // W - minimum operating capacity of chiller
		Real64 & OptCap, // W - optimal operating capacity of chiller
		bool const GetSizingFactor, // TRUE when just the sizing factor is requested
		Real64 & SizingFactor, // sizing factor
		Real64 & TempCondInDesign, // design condenser inlet temperature, water side
		Real64 & TempEvapOutDesign // design evaporator outlet temperature, water side
	)
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         Dan Fisher
		//       DATE WRITTEN   Sept. 1998
		//       MODIFIED       April 1999, May 200-Taecheol Kim
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE: This is the Electric chiller model driver.  It
		// gets the input for the models, initializes simulation variables, call
		// the appropriate model and sets up reporting variables.

		// METHODOLOGY EMPLOYED: na

		// REFERENCES: na

		// Using/Aliasing
		using InputProcessor::FindItemInList;
		using PlantUtilities::UpdateChillerComponentCondenserSide;
		using PlantUtilities::UpdateComponentHeatRecoverySide;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int ChillNum = 1; // chiller number pointer

		{ auto const SELECT_CASE_var( ChillerType );

		if ( SELECT_CASE_var == TypeOf_Chiller_Electric ) {

			//Get chiller data from input file
			if ( GetElectricInput ) {
				GetElectricChillerInput();
				GetElectricInput = false;
			}

			if ( InitLoopEquip ) {
				TempEvapOutDesign = ElectricChiller( ChillNum ).TempDesEvapOut;
				TempCondInDesign = ElectricChiller( ChillNum ).TempDesCondIn;

				InitElectricChiller( ChillNum, RunFlag, MyLoad );

				if ( LoopNum == ElectricChiller( ChillNum ).CWLoopNum ) { // chilled water loop
					SizeElectricChiller( ChillNum );
					MinCap = ElectricChiller( ChillNum ).NomCap * ElectricChiller( ChillNum ).MinPartLoadRat;
					MaxCap = ElectricChiller( ChillNum ).NomCap * ElectricChiller( ChillNum ).MaxPartLoadRat;
					OptCap = ElectricChiller( ChillNum ).NomCap * ElectricChiller( ChillNum ).OptPartLoadRat;
				} else {
					MinCap = 0.0;
					MaxCap = 0.0;
					OptCap = 0.0;
				}
				if ( GetSizingFactor ) {
					SizingFactor = ElectricChiller( ChillNum ).SizFac;
				}
				return;
			}

			// calculate model depending on where called from
			if ( LoopNum == ElectricChiller( ChillNum ).CWLoopNum ) { // chilled water loop

				InitElectricChiller( ChillNum, RunFlag, MyLoad );
				CalcElectricChillerModel( ChillNum, MyLoad, EquipFlowCtrl, RunFlag );
				UpdateElectricChillerRecords( MyLoad, RunFlag, ChillNum );

			} else if ( LoopNum == ElectricChiller( ChillNum ).CDLoopNum ) { // condenser loop
				UpdateChillerComponentCondenserSide( ElectricChiller( ChillNum ).CDLoopNum, ElectricChiller( ChillNum ).CDLoopSideNum, TypeOf_Chiller_Electric, ElectricChiller( ChillNum ).CondInletNodeNum, ElectricChiller( ChillNum ).CondOutletNodeNum, ElectricChillerReport( ChillNum ).QCond, ElectricChillerReport( ChillNum ).CondInletTemp, ElectricChillerReport( ChillNum ).CondOutletTemp, ElectricChillerReport( ChillNum ).Condmdot, FirstHVACIteration );
			} else if ( LoopNum == ElectricChiller( ChillNum ).HRLoopNum ) { // heat recovery loop
				UpdateComponentHeatRecoverySide( ElectricChiller( ChillNum ).HRLoopNum, ElectricChiller( ChillNum ).HRLoopSideNum, TypeOf_Chiller_Electric, ElectricChiller( ChillNum ).HeatRecInletNodeNum, ElectricChiller( ChillNum ).HeatRecOutletNodeNum, ElectricChillerReport( ChillNum ).QHeatRecovery, ElectricChillerReport( ChillNum ).HeatRecInletTemp, ElectricChillerReport( ChillNum ).HeatRecOutletTemp, ElectricChillerReport( ChillNum ).HeatRecMassFlow, FirstHVACIteration );
			}

		} else if ( SELECT_CASE_var == TypeOf_Chiller_EngineDriven ) {

			if ( GetEngineDrivenInput ) {
				GetEngineDrivenChillerInput();
				GetEngineDrivenInput = false;
			}

			if ( InitLoopEquip ) {
				TempEvapOutDesign = EngineDrivenChiller( ChillNum ).TempDesEvapOut;
				TempCondInDesign = EngineDrivenChiller( ChillNum ).TempDesCondIn;

				InitEngineDrivenChiller( ChillNum, RunFlag, MyLoad );

				if ( LoopNum == EngineDrivenChiller( ChillNum ).CWLoopNum ) {
					SizeEngineDrivenChiller( ChillNum );
					MinCap = EngineDrivenChiller( ChillNum ).NomCap * EngineDrivenChiller( ChillNum ).MinPartLoadRat;
					MaxCap = EngineDrivenChiller( ChillNum ).NomCap * EngineDrivenChiller( ChillNum ).MaxPartLoadRat;
					OptCap = EngineDrivenChiller( ChillNum ).NomCap * EngineDrivenChiller( ChillNum ).OptPartLoadRat;
				} else {
					MinCap = 0.0;
					MaxCap = 0.0;
					OptCap = 0.0;
				}
				if ( GetSizingFactor ) {
					SizingFactor = EngineDrivenChiller( ChillNum ).SizFac;
				}
				return;
			}

			// calculate model depending on where called from
			if ( LoopNum == EngineDrivenChiller( ChillNum ).CWLoopNum ) { // chilled water loop
				InitEngineDrivenChiller( ChillNum, RunFlag, MyLoad );
				CalcEngineDrivenChillerModel( ChillNum, MyLoad, RunFlag, EquipFlowCtrl );
				UpdateEngineDrivenChiller( MyLoad, RunFlag, ChillNum );
			} else if ( LoopNum == EngineDrivenChiller( ChillNum ).CDLoopNum ) { // condenser loop
				UpdateChillerComponentCondenserSide( EngineDrivenChiller( ChillNum ).CDLoopNum, EngineDrivenChiller( ChillNum ).CDLoopSideNum, TypeOf_Chiller_EngineDriven, EngineDrivenChiller( ChillNum ).CondInletNodeNum, EngineDrivenChiller( ChillNum ).CondOutletNodeNum, EngineDrivenChillerReport( ChillNum ).QCond, EngineDrivenChillerReport( ChillNum ).CondInletTemp, EngineDrivenChillerReport( ChillNum ).CondOutletTemp, EngineDrivenChillerReport( ChillNum ).Condmdot, FirstHVACIteration );
			} else if ( LoopNum == EngineDrivenChiller( ChillNum ).HRLoopNum ) { // heat recovery loop
				UpdateComponentHeatRecoverySide( EngineDrivenChiller( ChillNum ).HRLoopNum, EngineDrivenChiller( ChillNum ).HRLoopSideNum, TypeOf_Chiller_EngineDriven, EngineDrivenChiller( ChillNum ).HeatRecInletNodeNum, EngineDrivenChiller( ChillNum ).HeatRecOutletNodeNum, EngineDrivenChillerReport( ChillNum ).QTotalHeatRecovered, EngineDrivenChillerReport( ChillNum ).HeatRecInletTemp, EngineDrivenChillerReport( ChillNum ).HeatRecOutletTemp, EngineDrivenChillerReport( ChillNum ).HeatRecMdot, FirstHVACIteration );
			}

		} else if ( SELECT_CASE_var == TypeOf_Chiller_CombTurbine ) {

			if ( GetGasTurbineInput ) {
				GetGTChillerInput();
				GetGasTurbineInput = false;
			}

			if ( InitLoopEquip ) {
				TempEvapOutDesign = GTChiller( ChillNum ).TempDesEvapOut;
				TempCondInDesign = GTChiller( ChillNum ).TempDesCondIn;

				InitGTChiller( ChillNum, RunFlag, MyLoad );

				if ( LoopNum == GTChiller( ChillNum ).CWLoopNum ) {
					SizeGTChiller( ChillNum );
					MinCap = GTChiller( ChillNum ).NomCap * GTChiller( ChillNum ).MinPartLoadRat;
					MaxCap = GTChiller( ChillNum ).NomCap * GTChiller( ChillNum ).MaxPartLoadRat;
					OptCap = GTChiller( ChillNum ).NomCap * GTChiller( ChillNum ).OptPartLoadRat;
				} else {
					MinCap = 0.0;
					MaxCap = 0.0;
					OptCap = 0.0;
				}
				if ( GetSizingFactor ) {
					SizingFactor = GTChiller( ChillNum ).SizFac;
				}
				return;
			}

			// calculate model depending on where called from
			if ( LoopNum == GTChiller( ChillNum ).CWLoopNum ) { // chilled water loop
				InitGTChiller( ChillNum, RunFlag, MyLoad );
				CalcGTChillerModel( ChillNum, MyLoad, RunFlag, EquipFlowCtrl );
				UpdateGTChillerRecords( MyLoad, RunFlag, ChillNum );

			} else if ( LoopNum == GTChiller( ChillNum ).CDLoopNum ) { // condenser loop
				UpdateChillerComponentCondenserSide( GTChiller( ChillNum ).CDLoopNum, GTChiller( ChillNum ).CDLoopSideNum, TypeOf_Chiller_CombTurbine, GTChiller( ChillNum ).CondInletNodeNum, GTChiller( ChillNum ).CondOutletNodeNum, GTChillerReport( ChillNum ).QCond, GTChillerReport( ChillNum ).CondInletTemp, GTChillerReport( ChillNum ).CondOutletTemp, GTChillerReport( ChillNum ).Condmdot, FirstHVACIteration );
			} else if ( LoopNum == GTChiller( ChillNum ).HRLoopNum ) { // heat recovery loop
				UpdateComponentHeatRecoverySide( GTChiller( ChillNum ).HRLoopNum, GTChiller( ChillNum ).HRLoopSideNum, TypeOf_Chiller_CombTurbine, GTChiller( ChillNum ).HeatRecInletNodeNum, GTChiller( ChillNum ).HeatRecOutletNodeNum, GTChillerReport( ChillNum ).HeatRecLubeRate, GTChillerReport( ChillNum ).HeatRecInletTemp, GTChillerReport( ChillNum ).HeatRecOutletTemp, GTChillerReport( ChillNum ).HeatRecMdot, FirstHVACIteration );
			}

		} else if ( SELECT_CASE_var == TypeOf_Chiller_ConstCOP ) {

			//GET INPUT
			if ( GetConstCOPInput ) {
				GetConstCOPChillerInput();
				GetConstCOPInput = false;
			}

			if ( InitLoopEquip ) {
				TempEvapOutDesign = 0.0;
				TempCondInDesign = 0.0;

				InitConstCOPChiller( ChillNum, RunFlag, MyLoad );

				if ( LoopNum == ConstCOPChiller( ChillNum ).CWLoopNum ) {
					SizeConstCOPChiller( ChillNum );
					MinCap = 0.0;
					MaxCap = ConstCOPChiller( ChillNum ).NomCap;
					OptCap = ConstCOPChiller( ChillNum ).NomCap;
				} else {
					MinCap = 0.0;
					MaxCap = 0.0;
					OptCap = 0.0;
				}
				if ( GetSizingFactor ) {
					SizingFactor = ConstCOPChiller( ChillNum ).SizFac;
				}
				return;
			}

			if ( LoopNum == ConstCOPChiller( ChillNum ).CWLoopNum ) {
				// Calculate Load
				// IF MinPlr, MaxPlr, OptPlr are not defined, assume min = 0, max=opt=Nomcap
				InitConstCOPChiller( ChillNum, RunFlag, MyLoad );
				CalcConstCOPChillerModel( ChillNum, MyLoad, RunFlag, EquipFlowCtrl );
				UpdateConstCOPChillerRecords( MyLoad, RunFlag, ChillNum );
			} else if ( LoopNum == ConstCOPChiller( ChillNum ).CDLoopNum ) {
				UpdateChillerComponentCondenserSide( ConstCOPChiller( ChillNum ).CDLoopNum, ConstCOPChiller( ChillNum ).CDLoopSideNum, TypeOf_Chiller_ConstCOP, ConstCOPChiller( ChillNum ).CondInletNodeNum, ConstCOPChiller( ChillNum ).CondOutletNodeNum, ConstCOPChillerReport( ChillNum ).QCond, ConstCOPChillerReport( ChillNum ).CondInletTemp, ConstCOPChillerReport( ChillNum ).CondOutletTemp, ConstCOPChillerReport( ChillNum ).Condmdot, FirstHVACIteration );
			}

		}}

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
			ShowSevereError( "No " + cCurrentModuleObject + " Equipment specified in input file" );
			ErrorsFound = true;
		}

		//See if load distribution manager has already gotten the input
		if ( allocated( ElectricChiller ) ) return;

		//ALLOCATE ARRAYS
		ElectricChiller.allocate( NumElectricChillers );
		ElectricChillerReport.allocate( NumElectricChillers );

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
			SetupOutputVariable( "Chiller Electric Power [W]", ElectricChillerReport( ChillerNum ).Power, "System", "Average", ElectricChiller( ChillerNum ).Name );
			SetupOutputVariable( "Chiller Electric Energy [J]", ElectricChillerReport( ChillerNum ).Energy, "System", "Sum", ElectricChiller( ChillerNum ).Name, _, "ELECTRICITY", "Cooling", _, "Plant" );

			SetupOutputVariable( "Chiller Evaporator Cooling Rate [W]", ElectricChillerReport( ChillerNum ).QEvap, "System", "Average", ElectricChiller( ChillerNum ).Name );
			SetupOutputVariable( "Chiller Evaporator Cooling Energy [J]", ElectricChillerReport( ChillerNum ).EvapEnergy, "System", "Sum", ElectricChiller( ChillerNum ).Name, _, "ENERGYTRANSFER", "CHILLERS", _, "Plant" );
			SetupOutputVariable( "Chiller Evaporator Inlet Temperature [C]", ElectricChillerReport( ChillerNum ).EvapInletTemp, "System", "Average", ElectricChiller( ChillerNum ).Name );
			SetupOutputVariable( "Chiller Evaporator Outlet Temperature [C]", ElectricChillerReport( ChillerNum ).EvapOutletTemp, "System", "Average", ElectricChiller( ChillerNum ).Name );
			SetupOutputVariable( "Chiller Evaporator Mass Flow Rate [kg/s]", ElectricChillerReport( ChillerNum ).Evapmdot, "System", "Average", ElectricChiller( ChillerNum ).Name );

			SetupOutputVariable( "Chiller Condenser Heat Transfer Rate [W]", ElectricChillerReport( ChillerNum ).QCond, "System", "Average", ElectricChiller( ChillerNum ).Name );
			SetupOutputVariable( "Chiller Condenser Heat Transfer Energy [J]", ElectricChillerReport( ChillerNum ).CondEnergy, "System", "Sum", ElectricChiller( ChillerNum ).Name, _, "ENERGYTRANSFER", "HEATREJECTION", _, "Plant" );
			SetupOutputVariable( "Chiller COP [W/W]", ElectricChillerReport( ChillerNum ).ActualCOP, "System", "Average", ElectricChiller( ChillerNum ).Name );

			//Condenser mass flow and outlet temp are valid for water cooled
			if ( ElectricChiller( ChillerNum ).CondenserType == WaterCooled ) {
				SetupOutputVariable( "Chiller Condenser Inlet Temperature [C]", ElectricChillerReport( ChillerNum ).CondInletTemp, "System", "Average", ElectricChiller( ChillerNum ).Name );
				SetupOutputVariable( "Chiller Condenser Outlet Temperature [C]", ElectricChillerReport( ChillerNum ).CondOutletTemp, "System", "Average", ElectricChiller( ChillerNum ).Name );
				SetupOutputVariable( "Chiller Condenser Mass Flow Rate [kg/s]", ElectricChillerReport( ChillerNum ).Condmdot, "System", "Average", ElectricChiller( ChillerNum ).Name );
			} else if ( ElectricChiller( ChillerNum ).CondenserType == AirCooled ) {
				SetupOutputVariable( "Chiller Condenser Inlet Temperature [C]", ElectricChillerReport( ChillerNum ).CondInletTemp, "System", "Average", ElectricChiller( ChillerNum ).Name );
			} else if ( ElectricChiller( ChillerNum ).CondenserType == EvapCooled ) {
				SetupOutputVariable( "Chiller Condenser Inlet Temperature [C]", ElectricChillerReport( ChillerNum ).CondInletTemp, "System", "Average", ElectricChiller( ChillerNum ).Name );
				if ( ElectricChiller( ChillerNum ).BasinHeaterPowerFTempDiff > 0.0 ) {
					SetupOutputVariable( "Chiller Basin Heater Electric Power [W]", ElectricChillerReport( ChillerNum ).BasinHeaterPower, "System", "Average", ElectricChiller( ChillerNum ).Name );
					SetupOutputVariable( "Chiller Basin Heater Electric Energy [J]", ElectricChillerReport( ChillerNum ).BasinHeaterConsumption, "System", "Sum", ElectricChiller( ChillerNum ).Name, _, "Electric", "CHILLERS", _, "Plant" );
				}
			}

			//If heat recovery is active then setup report variables
			if ( ElectricChiller( ChillerNum ).HeatRecActive ) {
				SetupOutputVariable( "Chiller Total Recovered Heat Rate [W]", ElectricChillerReport( ChillerNum ).QHeatRecovery, "System", "Average", ElectricChiller( ChillerNum ).Name );
				SetupOutputVariable( "Chiller Total Recovered Heat Energy [J]", ElectricChillerReport( ChillerNum ).EnergyHeatRecovery, "System", "Sum", ElectricChiller( ChillerNum ).Name, _, "ENERGYTRANSFER", "HEATRECOVERY", _, "Plant" );
				SetupOutputVariable( "Chiller Heat Recovery Inlet Temperature [C]", ElectricChillerReport( ChillerNum ).HeatRecInletTemp, "System", "Average", ElectricChiller( ChillerNum ).Name );

				SetupOutputVariable( "Chiller Heat Recovery Outlet Temperature [C]", ElectricChillerReport( ChillerNum ).HeatRecOutletTemp, "System", "Average", ElectricChiller( ChillerNum ).Name );

				SetupOutputVariable( "Chiller Heat Recovery Mass Flow Rate [kg/s]", ElectricChillerReport( ChillerNum ).HeatRecMassFlow, "System", "Average", ElectricChiller( ChillerNum ).Name );

				SetupOutputVariable( "Chiller Effective Heat Rejection Temperature [C]", ElectricChillerReport( ChillerNum ).ChillerCondAvgTemp, "System", "Average", ElectricChiller( ChillerNum ).Name );

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
			ShowSevereError( "No " + cCurrentModuleObject + " equipment specified in input file" );
			ErrorsFound = true;
		}
		//See if load distribution manager has already gotten the input
		if ( allocated( EngineDrivenChiller ) ) return;

		//ALLOCATE ARRAYS
		EngineDrivenChiller.allocate( NumEngineDrivenChillers );
		EngineDrivenChillerReport.allocate( NumEngineDrivenChillers );

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
			SetupOutputVariable( "Chiller Drive Shaft Power [W]", EngineDrivenChillerReport( ChillerNum ).Power, "System", "Average", EngineDrivenChiller( ChillerNum ).Name );
			SetupOutputVariable( "Chiller Drive Shaft Energy [J]", EngineDrivenChillerReport( ChillerNum ).Energy, "System", "Sum", EngineDrivenChiller( ChillerNum ).Name );

			SetupOutputVariable( "Chiller Evaporator Cooling Rate [W]", EngineDrivenChillerReport( ChillerNum ).QEvap, "System", "Average", EngineDrivenChiller( ChillerNum ).Name );
			SetupOutputVariable( "Chiller Evaporator Cooling Energy [J]", EngineDrivenChillerReport( ChillerNum ).EvapEnergy, "System", "Sum", EngineDrivenChiller( ChillerNum ).Name, _, "ENERGYTRANSFER", "CHILLERS", _, "Plant" );
			SetupOutputVariable( "Chiller Evaporator Inlet Temperature [C]", EngineDrivenChillerReport( ChillerNum ).EvapInletTemp, "System", "Average", EngineDrivenChiller( ChillerNum ).Name );
			SetupOutputVariable( "Chiller Evaporator Outlet Temperature [C]", EngineDrivenChillerReport( ChillerNum ).EvapOutletTemp, "System", "Average", EngineDrivenChiller( ChillerNum ).Name );
			SetupOutputVariable( "Chiller Evaporator Mass Flow Rate [kg/s]", EngineDrivenChillerReport( ChillerNum ).Evapmdot, "System", "Average", EngineDrivenChiller( ChillerNum ).Name );
			SetupOutputVariable( "Chiller Condenser Heat Transfer Rate [W]", EngineDrivenChillerReport( ChillerNum ).QCond, "System", "Average", EngineDrivenChiller( ChillerNum ).Name );
			SetupOutputVariable( "Chiller Condenser Heat Transfer Energy [J]", EngineDrivenChillerReport( ChillerNum ).CondEnergy, "System", "Sum", EngineDrivenChiller( ChillerNum ).Name, _, "ENERGYTRANSFER", "HEATREJECTION", _, "Plant" );

			//Condenser mass flow and outlet temp are valid for Water Cooled
			if ( EngineDrivenChiller( ChillerNum ).CondenserType == WaterCooled ) {
				SetupOutputVariable( "Chiller Condenser Inlet Temperature [C]", EngineDrivenChillerReport( ChillerNum ).CondInletTemp, "System", "Average", EngineDrivenChiller( ChillerNum ).Name );
				SetupOutputVariable( "Chiller Condenser Outlet Temperature [C]", EngineDrivenChillerReport( ChillerNum ).CondOutletTemp, "System", "Average", EngineDrivenChiller( ChillerNum ).Name );
				SetupOutputVariable( "Chiller Condenser Mass Flow Rate [kg/s]", EngineDrivenChillerReport( ChillerNum ).Condmdot, "System", "Average", EngineDrivenChiller( ChillerNum ).Name );
			} else if ( EngineDrivenChiller( ChillerNum ).CondenserType == AirCooled ) {
				SetupOutputVariable( "Chiller Condenser Inlet Temperature [C]", EngineDrivenChillerReport( ChillerNum ).CondInletTemp, "System", "Average", EngineDrivenChiller( ChillerNum ).Name );
			} else if ( EngineDrivenChiller( ChillerNum ).CondenserType == EvapCooled ) {
				SetupOutputVariable( "Chiller Condenser Inlet Temperature [C]", EngineDrivenChillerReport( ChillerNum ).CondInletTemp, "System", "Average", EngineDrivenChiller( ChillerNum ).Name );
				if ( EngineDrivenChiller( ChillerNum ).BasinHeaterPowerFTempDiff > 0.0 ) {
					SetupOutputVariable( "Chiller Basin Heater Electric Power [W]", EngineDrivenChillerReport( ChillerNum ).BasinHeaterPower, "System", "Average", EngineDrivenChiller( ChillerNum ).Name );
					SetupOutputVariable( "Chiller Basin Heater Electric Energy [J]", EngineDrivenChillerReport( ChillerNum ).BasinHeaterConsumption, "System", "Sum", EngineDrivenChiller( ChillerNum ).Name, _, "Electric", "CHILLERS", _, "Plant" );
				}
			}

			SetupOutputVariable( "Chiller " + EngineDrivenChiller( ChillerNum ).FuelType + " Rate [W]", EngineDrivenChillerReport( ChillerNum ).FuelEnergyUseRate, "System", "Average", EngineDrivenChiller( ChillerNum ).Name );
			SetupOutputVariable( "Chiller " + EngineDrivenChiller( ChillerNum ).FuelType + " Energy [J]", EngineDrivenChillerReport( ChillerNum ).FuelEnergy, "System", "Sum", EngineDrivenChiller( ChillerNum ).Name, _, EngineDrivenChiller( ChillerNum ).FuelType, "Cooling", _, "Plant" );

			SetupOutputVariable( "Chiller COP [W/W]", EngineDrivenChillerReport( ChillerNum ).FuelCOP, "System", "Average", EngineDrivenChiller( ChillerNum ).Name );

			SetupOutputVariable( "Chiller " + EngineDrivenChiller( ChillerNum ).FuelType + " Mass Flow Rate [kg/s]", EngineDrivenChillerReport( ChillerNum ).FuelMdot, "System", "Average", EngineDrivenChiller( ChillerNum ).Name );

			SetupOutputVariable( "Chiller Exhaust Temperature [C]", EngineDrivenChillerReport( ChillerNum ).ExhaustStackTemp, "System", "Average", EngineDrivenChiller( ChillerNum ).Name );

			SetupOutputVariable( "Chiller Heat Recovery Mass Flow Rate [kg/s]", EngineDrivenChillerReport( ChillerNum ).HeatRecMdot, "System", "Average", EngineDrivenChiller( ChillerNum ).Name );

			if ( EngineDrivenChiller( ChillerNum ).HeatRecActive ) {
				// need to only report if heat recovery active
				SetupOutputVariable( "Chiller Jacket Recovered Heat Rate [W]", EngineDrivenChillerReport( ChillerNum ).QJacketRecovered, "System", "Average", EngineDrivenChiller( ChillerNum ).Name );
				SetupOutputVariable( "Chiller Jacket Recovered Heat Energy [J]", EngineDrivenChillerReport( ChillerNum ).JacketEnergyRec, "System", "Sum", EngineDrivenChiller( ChillerNum ).Name, _, "ENERGYTRANSFER", "HEATRECOVERY", _, "Plant" );

				SetupOutputVariable( "Chiller Lube Recovered Heat Rate [W]", EngineDrivenChillerReport( ChillerNum ).QLubeOilRecovered, "System", "Average", EngineDrivenChiller( ChillerNum ).Name );
				SetupOutputVariable( "Chiller Lube Recovered Heat Energy [J]", EngineDrivenChillerReport( ChillerNum ).LubeOilEnergyRec, "System", "Sum", EngineDrivenChiller( ChillerNum ).Name, _, "ENERGYTRANSFER", "HEATRECOVERY", _, "Plant" );

				SetupOutputVariable( "Chiller Exhaust Recovered Heat Rate [W]", EngineDrivenChillerReport( ChillerNum ).QExhaustRecovered, "System", "Average", EngineDrivenChiller( ChillerNum ).Name );
				SetupOutputVariable( "Chiller Exhaust Recovered Heat Energy [J]", EngineDrivenChillerReport( ChillerNum ).ExhaustEnergyRec, "System", "Sum", EngineDrivenChiller( ChillerNum ).Name, _, "ENERGYTRANSFER", "HEATRECOVERY", _, "Plant" );

				SetupOutputVariable( "Chiller Total Recovered Heat Rate [W]", EngineDrivenChillerReport( ChillerNum ).QTotalHeatRecovered, "System", "Average", EngineDrivenChiller( ChillerNum ).Name );
				SetupOutputVariable( "Chiller Total Recovered Heat Energy [J]", EngineDrivenChillerReport( ChillerNum ).TotalHeatEnergyRec, "System", "Sum", EngineDrivenChiller( ChillerNum ).Name );

				SetupOutputVariable( "Chiller Heat Recovery Inlet Temperature [C]", EngineDrivenChillerReport( ChillerNum ).HeatRecInletTemp, "System", "Average", EngineDrivenChiller( ChillerNum ).Name );

				SetupOutputVariable( "Chiller Heat Recovery Outlet Temperature [C]", EngineDrivenChillerReport( ChillerNum ).HeatRecOutletTemp, "System", "Average", EngineDrivenChiller( ChillerNum ).Name );

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
			ShowSevereError( "No " + cCurrentModuleObject + " equipment specified in input file" );
			ErrorsFound = true;
		}
		//See if load distribution manager has already gotten the input
		if ( allocated( GTChiller ) ) return;

		//ALLOCATE ARRAYS
		GTChiller.allocate( NumGTChillers );
		GTChillerReport.allocate( NumGTChillers );

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
			SetupOutputVariable( "Chiller Drive Shaft Power [W]", GTChillerReport( ChillerNum ).Power, "System", "Average", GTChiller( ChillerNum ).Name );
			SetupOutputVariable( "Chiller Drive Shaft Energy [J]", GTChillerReport( ChillerNum ).Energy, "System", "Sum", GTChiller( ChillerNum ).Name );

			SetupOutputVariable( "Chiller Evaporator Cooling Rate [W]", GTChillerReport( ChillerNum ).QEvap, "System", "Average", GTChiller( ChillerNum ).Name );
			SetupOutputVariable( "Chiller Evaporator Cooling Energy [J]", GTChillerReport( ChillerNum ).EvapEnergy, "System", "Sum", GTChiller( ChillerNum ).Name, _, "ENERGYTRANSFER", "CHILLERS", _, "Plant" );
			SetupOutputVariable( "Chiller Evaporator Inlet Temperature [C]", GTChillerReport( ChillerNum ).EvapInletTemp, "System", "Average", GTChiller( ChillerNum ).Name );
			SetupOutputVariable( "Chiller Evaporator Outlet Temperature [C]", GTChillerReport( ChillerNum ).EvapOutletTemp, "System", "Average", GTChiller( ChillerNum ).Name );
			SetupOutputVariable( "Chiller Evaporator Mass Flow Rate [kg/s]", GTChillerReport( ChillerNum ).Evapmdot, "System", "Average", GTChiller( ChillerNum ).Name );

			SetupOutputVariable( "Chiller Condenser Heat Transfer Rate [W]", GTChillerReport( ChillerNum ).QCond, "System", "Average", GTChiller( ChillerNum ).Name );
			SetupOutputVariable( "Chiller Condenser Heat Transfer Energy [J]", GTChillerReport( ChillerNum ).CondEnergy, "System", "Sum", GTChiller( ChillerNum ).Name, _, "ENERGYTRANSFER", "HEATREJECTION", _, "Plant" );

			//Condenser mass flow and outlet temp are valid for water cooled
			if ( GTChiller( ChillerNum ).CondenserType == WaterCooled ) {
				SetupOutputVariable( "Chiller Condenser Inlet Temperature [C]", GTChillerReport( ChillerNum ).CondInletTemp, "System", "Average", GTChiller( ChillerNum ).Name );
				SetupOutputVariable( "Chiller Condenser Outlet Temperature [C]", GTChillerReport( ChillerNum ).CondOutletTemp, "System", "Average", GTChiller( ChillerNum ).Name );
				SetupOutputVariable( "Chiller Condenser Mass Flow Rate [kg/s]", GTChillerReport( ChillerNum ).Condmdot, "System", "Average", GTChiller( ChillerNum ).Name );
			} else if ( GTChiller( ChillerNum ).CondenserType == AirCooled ) {
				SetupOutputVariable( "Chiller Condenser Inlet Temperature [C]", GTChillerReport( ChillerNum ).CondInletTemp, "System", "Average", GTChiller( ChillerNum ).Name );
			} else if ( GTChiller( ChillerNum ).CondenserType == EvapCooled ) {
				SetupOutputVariable( "Chiller Condenser Inlet Temperature [C]", GTChillerReport( ChillerNum ).CondInletTemp, "System", "Average", GTChiller( ChillerNum ).Name );
				if ( GTChiller( ChillerNum ).BasinHeaterPowerFTempDiff > 0.0 ) {
					SetupOutputVariable( "Chiller Basin Heater Electric Power [W]", GTChillerReport( ChillerNum ).BasinHeaterPower, "System", "Average", GTChiller( ChillerNum ).Name );
					SetupOutputVariable( "Chiller Basin Heater Electric Energy [J]", GTChillerReport( ChillerNum ).BasinHeaterConsumption, "System", "Sum", GTChiller( ChillerNum ).Name, _, "Electric", "CHILLERS", _, "Plant" );
				}
			}

			SetupOutputVariable( "Chiller Lube Recovered Heat Rate [W]", GTChillerReport( ChillerNum ).HeatRecLubeRate, "System", "Average", GTChiller( ChillerNum ).Name );
			SetupOutputVariable( "Chiller Lube Recovered Heat Energy [J]", GTChillerReport( ChillerNum ).HeatRecLubeEnergy, "System", "Sum", GTChiller( ChillerNum ).Name, _, "ENERGYTRANSFER", "HeatRecovery", _, "Plant" );

			SetupOutputVariable( "Chiller " + GTChiller( ChillerNum ).FuelType + " Rate [W]", GTChillerReport( ChillerNum ).FuelEnergyUsedRate, "System", "Average", GTChiller( ChillerNum ).Name );

			SetupOutputVariable( "Chiller " + GTChiller( ChillerNum ).FuelType + " Energy [J]", GTChillerReport( ChillerNum ).FuelEnergyUsed, "System", "Sum", GTChiller( ChillerNum ).Name, _, GTChiller( ChillerNum ).FuelType, "Cooling", _, "Plant" );

			SetupOutputVariable( "Chiller " + GTChiller( ChillerNum ).FuelType + " Mass Flow Rate [kg/s]", GTChillerReport( ChillerNum ).FuelMassUsedRate, "System", "Average", GTChiller( ChillerNum ).Name );

			SetupOutputVariable( "Chiller " + GTChiller( ChillerNum ).FuelType + " Mass [kg]", GTChillerReport( ChillerNum ).FuelMassUsed, "System", "Sum", GTChiller( ChillerNum ).Name );

			SetupOutputVariable( "Chiller Exhaust Temperature [C]", GTChillerReport( ChillerNum ).ExhaustStackTemp, "System", "Average", GTChiller( ChillerNum ).Name );

			SetupOutputVariable( "Chiller Heat Recovery Inlet Temperature [C]", GTChillerReport( ChillerNum ).HeatRecInletTemp, "System", "Average", GTChiller( ChillerNum ).Name );

			SetupOutputVariable( "Chiller Heat Recovery Outlet Temperature [C]", GTChillerReport( ChillerNum ).HeatRecOutletTemp, "System", "Average", GTChiller( ChillerNum ).Name );

			SetupOutputVariable( "Chiller Heat Recovery Mass Flow Rate [kg/s]", GTChillerReport( ChillerNum ).HeatRecMdot, "System", "Average", GTChiller( ChillerNum ).Name );

			SetupOutputVariable( "Chiller COP [W/W]", GTChillerReport( ChillerNum ).FuelCOP, "System", "Average", GTChiller( ChillerNum ).Name );

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
			ShowSevereError( "No " + cCurrentModuleObject + " equipment specified in input file" );
			ErrorsFound = true;
		}

		//See if load distribution manager has already gotten the input
		if ( allocated( ConstCOPChiller ) ) return;

		ConstCOPChiller.allocate( NumConstCOPChillers );
		ConstCOPChillerReport.allocate( NumConstCOPChillers );

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
			SetupOutputVariable( "Chiller Electric Power [W]", ConstCOPChillerReport( ChillerNum ).Power, "System", "Average", ConstCOPChiller( ChillerNum ).Name );
			SetupOutputVariable( "Chiller Electric Energy [J]", ConstCOPChillerReport( ChillerNum ).Energy, "System", "Sum", ConstCOPChiller( ChillerNum ).Name, _, "ELECTRICITY", "Cooling", _, "Plant" );

			SetupOutputVariable( "Chiller Evaporator Cooling Rate [W]", ConstCOPChillerReport( ChillerNum ).QEvap, "System", "Average", ConstCOPChiller( ChillerNum ).Name );
			SetupOutputVariable( "Chiller Evaporator Cooling Energy [J]", ConstCOPChillerReport( ChillerNum ).EvapEnergy, "System", "Sum", ConstCOPChiller( ChillerNum ).Name, _, "ENERGYTRANSFER", "CHILLERS", _, "Plant" );
			SetupOutputVariable( "Chiller Evaporator Inlet Temperature [C]", ConstCOPChillerReport( ChillerNum ).EvapInletTemp, "System", "Average", ConstCOPChiller( ChillerNum ).Name );
			SetupOutputVariable( "Chiller Evaporator Outlet Temperature [C]", ConstCOPChillerReport( ChillerNum ).EvapOutletTemp, "System", "Average", ConstCOPChiller( ChillerNum ).Name );
			SetupOutputVariable( "Chiller Evaporator Mass Flow Rate [kg/s]", ConstCOPChillerReport( ChillerNum ).Evapmdot, "System", "Average", ConstCOPChiller( ChillerNum ).Name );
			SetupOutputVariable( "Chiller COP [W/W]", ConstCOPChillerReport( ChillerNum ).ActualCOP, "System", "Average", ConstCOPChiller( ChillerNum ).Name );

			SetupOutputVariable( "Chiller Condenser Heat Transfer Rate [W]", ConstCOPChillerReport( ChillerNum ).QCond, "System", "Average", ConstCOPChiller( ChillerNum ).Name );
			SetupOutputVariable( "Chiller Condenser Heat Transfer Energy [J]", ConstCOPChillerReport( ChillerNum ).CondEnergy, "System", "Sum", ConstCOPChiller( ChillerNum ).Name, _, "ENERGYTRANSFER", "HEATREJECTION", _, "Plant" );

			//Condenser mass flow and outlet temp are valid for water cooled
			if ( ConstCOPChiller( ChillerNum ).CondenserType == WaterCooled ) {
				SetupOutputVariable( "Chiller Condenser Inlet Temperature [C]", ConstCOPChillerReport( ChillerNum ).CondInletTemp, "System", "Average", ConstCOPChiller( ChillerNum ).Name );
				SetupOutputVariable( "Chiller Condenser Outlet Temperature [C]", ConstCOPChillerReport( ChillerNum ).CondOutletTemp, "System", "Average", ConstCOPChiller( ChillerNum ).Name );
				SetupOutputVariable( "Chiller Condenser Mass Flow Rate [kg/s]", ConstCOPChillerReport( ChillerNum ).Condmdot, "System", "Average", ConstCOPChiller( ChillerNum ).Name );
			} else if ( ConstCOPChiller( ChillerNum ).CondenserType == AirCooled ) {
				SetupOutputVariable( "Chiller Condenser Inlet Temperature [C]", ConstCOPChillerReport( ChillerNum ).CondInletTemp, "System", "Average", ConstCOPChiller( ChillerNum ).Name );
			} else if ( ConstCOPChiller( ChillerNum ).CondenserType == EvapCooled ) {
				SetupOutputVariable( "Chiller Condenser Inlet Temperature [C]", ConstCOPChillerReport( ChillerNum ).CondInletTemp, "System", "Average", ConstCOPChiller( ChillerNum ).Name );
				if ( ConstCOPChiller( ChillerNum ).BasinHeaterPowerFTempDiff > 0.0 ) {
					SetupOutputVariable( "Chiller Basin Heater Electric Power [W]", ConstCOPChillerReport( ChillerNum ).BasinHeaterPower, "System", "Average", ConstCOPChiller( ChillerNum ).Name );
					SetupOutputVariable( "Chiller Basin Heater Electric Energy [J]", ConstCOPChillerReport( ChillerNum ).BasinHeaterConsumption, "System", "Sum", ConstCOPChiller( ChillerNum ).Name, _, "Electric", "CHILLERS", _, "Plant" );
				}
			}
			if ( AnyEnergyManagementSystemInModel ) {
				SetupEMSInternalVariable( "Chiller Nominal Capacity", ConstCOPChiller( ChillerNum ).Name, "[W]", ConstCOPChiller( ChillerNum ).NomCap );
			}
		}

	}

	void
	InitElectricChiller(
		int const ChillNum, // number of the current electric chiller being simulated
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
		static bool MyOneTimeFlag( true );
		static Array1D_bool MyFlag;
		static Array1D_bool MyEnvrnFlag;
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
		// FLOW:

		// Do the one time initializations
		if ( MyOneTimeFlag ) {
			MyFlag.allocate( NumElectricChillers );
			MyEnvrnFlag.allocate( NumElectricChillers );
			MyFlag = true;
			MyEnvrnFlag = true;
			MyOneTimeFlag = false;
		}

		CondInletNode = ElectricChiller( ChillNum ).CondInletNodeNum;
		CondOutletNode = ElectricChiller( ChillNum ).CondOutletNodeNum;
		EvapInletNode = ElectricChiller( ChillNum ).EvapInletNodeNum;
		EvapOutletNode = ElectricChiller( ChillNum ).EvapOutletNodeNum;

		if ( ElectricChiller( ChillNum ).HeatRecActive ) {
			HeatRecInNode = ElectricChiller( ChillNum ).HeatRecInletNodeNum;
			HeatRecOutNode = ElectricChiller( ChillNum ).HeatRecOutletNodeNum;
		}

		// Init more variables
		if ( MyFlag( ChillNum ) ) {
			// Locate the chillers on the plant loops for later usage
			errFlag = false;
			ScanPlantLoopsForObject( ElectricChiller( ChillNum ).Name, TypeOf_Chiller_Electric, ElectricChiller( ChillNum ).CWLoopNum, ElectricChiller( ChillNum ).CWLoopSideNum, ElectricChiller( ChillNum ).CWBranchNum, ElectricChiller( ChillNum ).CWCompNum, ElectricChiller( ChillNum ).TempLowLimitEvapOut, _, _, ElectricChiller( ChillNum ).EvapInletNodeNum, _, errFlag );
			if ( ElectricChiller( ChillNum ).CondenserType != AirCooled && ElectricChiller( ChillNum ).CondenserType != EvapCooled ) {
				ScanPlantLoopsForObject( ElectricChiller( ChillNum ).Name, TypeOf_Chiller_Electric, ElectricChiller( ChillNum ).CDLoopNum, ElectricChiller( ChillNum ).CDLoopSideNum, ElectricChiller( ChillNum ).CDBranchNum, ElectricChiller( ChillNum ).CDCompNum, _, _, _, ElectricChiller( ChillNum ).CondInletNodeNum, _, errFlag );
				InterConnectTwoPlantLoopSides( ElectricChiller( ChillNum ).CWLoopNum, ElectricChiller( ChillNum ).CWLoopSideNum, ElectricChiller( ChillNum ).CDLoopNum, ElectricChiller( ChillNum ).CDLoopSideNum, TypeOf_Chiller_Electric, true );
			}
			if ( ElectricChiller( ChillNum ).HeatRecActive ) {
				ScanPlantLoopsForObject( ElectricChiller( ChillNum ).Name, TypeOf_Chiller_Electric, ElectricChiller( ChillNum ).HRLoopNum, ElectricChiller( ChillNum ).HRLoopSideNum, ElectricChiller( ChillNum ).HRBranchNum, ElectricChiller( ChillNum ).HRCompNum, _, _, _, ElectricChiller( ChillNum ).HeatRecInletNodeNum, _, errFlag );
				InterConnectTwoPlantLoopSides( ElectricChiller( ChillNum ).CWLoopNum, ElectricChiller( ChillNum ).CWLoopSideNum, ElectricChiller( ChillNum ).HRLoopNum, ElectricChiller( ChillNum ).HRLoopSideNum, TypeOf_Chiller_Electric, true );
			}

			if ( ElectricChiller( ChillNum ).CondenserType != AirCooled && ElectricChiller( ChillNum ).CondenserType != EvapCooled && ElectricChiller( ChillNum ).HeatRecActive ) {
				InterConnectTwoPlantLoopSides( ElectricChiller( ChillNum ).CDLoopNum, ElectricChiller( ChillNum ).CDLoopSideNum, ElectricChiller( ChillNum ).HRLoopNum, ElectricChiller( ChillNum ).HRLoopSideNum, TypeOf_Chiller_Electric, false );
			}

			if ( errFlag ) {
				ShowFatalError( "InitElectricChiller: Program terminated due to previous condition(s)." );
			}

			if ( ElectricChiller( ChillNum ).FlowMode == ConstantFlow ) {
				// reset flow priority
				PlantLoop( ElectricChiller( ChillNum ).CWLoopNum ).LoopSide( ElectricChiller( ChillNum ).CWLoopSideNum ).Branch( ElectricChiller( ChillNum ).CWBranchNum ).Comp( ElectricChiller( ChillNum ).CWCompNum ).FlowPriority = LoopFlowStatus_NeedyIfLoopOn;
			}

			if ( ElectricChiller( ChillNum ).FlowMode == LeavingSetPointModulated ) {
				// reset flow priority
				PlantLoop( ElectricChiller( ChillNum ).CWLoopNum ).LoopSide( ElectricChiller( ChillNum ).CWLoopSideNum ).Branch( ElectricChiller( ChillNum ).CWBranchNum ).Comp( ElectricChiller( ChillNum ).CWCompNum ).FlowPriority = LoopFlowStatus_NeedyIfLoopOn;

				// check if setpoint on outlet node
				if ( ( Node( ElectricChiller( ChillNum ).EvapOutletNodeNum ).TempSetPoint == SensedNodeFlagValue ) && ( Node( ElectricChiller( ChillNum ).EvapOutletNodeNum ).TempSetPointHi == SensedNodeFlagValue ) ) {
					if ( ! AnyEnergyManagementSystemInModel ) {
						if ( ! ElectricChiller( ChillNum ).ModulatedFlowErrDone ) {
							ShowWarningError( "Missing temperature setpoint for LeavingSetpointModulated mode chiller named " + ElectricChiller( ChillNum ).Name );
							ShowContinueError( "  A temperature setpoint is needed at the outlet node of a chiller in variable flow mode, use a SetpointManager" );
							ShowContinueError( "  The overall loop setpoint will be assumed for chiller. The simulation continues ... " );
							ElectricChiller( ChillNum ).ModulatedFlowErrDone = true;
						}
					} else {
						// need call to EMS to check node
						FatalError = false; // but not really fatal yet, but should be.
						CheckIfNodeSetPointManagedByEMS( ElectricChiller( ChillNum ).EvapOutletNodeNum, iTemperatureSetPoint, FatalError );
						if ( FatalError ) {
							if ( ! ElectricChiller( ChillNum ).ModulatedFlowErrDone ) {
								ShowWarningError( "Missing temperature setpoint for LeavingSetpointModulated mode chiller named " + ElectricChiller( ChillNum ).Name );
								ShowContinueError( "  A temperature setpoint is needed at the outlet node of a chiller evaporator in variable flow mode" );
								ShowContinueError( "  use a Setpoint Manager to establish a setpoint at the chiller evaporator outlet node " );
								ShowContinueError( "  or use an EMS actuator to establish a setpoint at the outlet node " );
								ShowContinueError( "  The overall loop setpoint will be assumed for chiller. The simulation continues ... " );
								ElectricChiller( ChillNum ).ModulatedFlowErrDone = true;
							}
						}

					}
					ElectricChiller( ChillNum ).ModulatedFlowSetToLoop = true;
					{ auto const SELECT_CASE_var( PlantLoop( ElectricChiller( ChillNum ).CWLoopNum ).LoopDemandCalcScheme );
					if ( SELECT_CASE_var == SingleSetPoint ) {
						Node( ElectricChiller( ChillNum ).EvapOutletNodeNum ).TempSetPoint = Node( PlantLoop( ElectricChiller( ChillNum ).CWLoopNum ).TempSetPointNodeNum ).TempSetPoint;
					} else if ( SELECT_CASE_var == DualSetPointDeadBand ) {
						Node( ElectricChiller( ChillNum ).EvapOutletNodeNum ).TempSetPointHi = Node( PlantLoop( ElectricChiller( ChillNum ).CWLoopNum ).TempSetPointNodeNum ).TempSetPointHi;
					}}
				}
			}
			MyFlag( ChillNum ) = false;
		}

		if ( MyEnvrnFlag( ChillNum ) && BeginEnvrnFlag && ( PlantFirstSizesOkayToFinalize ) ) {

			rho = GetDensityGlycol( PlantLoop( ElectricChiller( ChillNum ).CWLoopNum ).FluidName, InitConvTemp, PlantLoop( ElectricChiller( ChillNum ).CWLoopNum ).FluidIndex, RoutineName );

			ElectricChiller( ChillNum ).EvapMassFlowRateMax = rho * ElectricChiller( ChillNum ).EvapVolFlowRate;
			InitComponentNodes( 0.0, ElectricChiller( ChillNum ).EvapMassFlowRateMax, EvapInletNode, EvapOutletNode, ElectricChiller( ChillNum ).CWLoopNum, ElectricChiller( ChillNum ).CWLoopSideNum, ElectricChiller( ChillNum ).CWBranchNum, ElectricChiller( ChillNum ).CWCompNum );

			//init maximum available condenser flow rate
			if ( ElectricChiller( ChillNum ).CondenserType == WaterCooled ) {

				Node( CondInletNode ).Temp = ElectricChiller( ChillNum ).TempDesCondIn; //DSU? old behavior, still want?

				rho = GetDensityGlycol( PlantLoop( ElectricChiller( ChillNum ).CDLoopNum ).FluidName, InitConvTemp, PlantLoop( ElectricChiller( ChillNum ).CDLoopNum ).FluidIndex, RoutineName );

				ElectricChiller( ChillNum ).CondMassFlowRateMax = rho * ElectricChiller( ChillNum ).CondVolFlowRate;

				InitComponentNodes( 0.0, ElectricChiller( ChillNum ).CondMassFlowRateMax, CondInletNode, CondOutletNode, ElectricChiller( ChillNum ).CDLoopNum, ElectricChiller( ChillNum ).CDLoopSideNum, ElectricChiller( ChillNum ).CDBranchNum, ElectricChiller( ChillNum ).CDCompNum );
			} else { // air or evap-air

				rho = PsyRhoAirFnPbTdbW( StdBaroPress, ElectricChiller( ChillNum ).TempDesCondIn, 0.0, RoutineName );
				ElectricChiller( ChillNum ).CondMassFlowRateMax = rho * ElectricChiller( ChillNum ).CondVolFlowRate;

				Node( CondInletNode ).MassFlowRate = ElectricChiller( ChillNum ).CondMassFlowRateMax;
				Node( CondOutletNode ).MassFlowRate = Node( CondInletNode ).MassFlowRate;
				Node( CondInletNode ).MassFlowRateMaxAvail = Node( CondInletNode ).MassFlowRate;
				Node( CondInletNode ).MassFlowRateMax = Node( CondInletNode ).MassFlowRate;
				Node( CondOutletNode ).MassFlowRateMax = Node( CondInletNode ).MassFlowRate;
				Node( CondInletNode ).MassFlowRateMinAvail = 0.0;
				Node( CondInletNode ).MassFlowRateMin = 0.0;
				Node( CondOutletNode ).MassFlowRateMinAvail = 0.0;
				Node( CondOutletNode ).MassFlowRateMin = 0.0;
			}

			if ( ElectricChiller( ChillNum ).HeatRecActive ) {
				rho = GetDensityGlycol( PlantLoop( ElectricChiller( ChillNum ).HRLoopNum ).FluidName, InitConvTemp, PlantLoop( ElectricChiller( ChillNum ).HRLoopNum ).FluidIndex, RoutineName );
				ElectricChiller( ChillNum ).DesignHeatRecMassFlowRate = rho * ElectricChiller( ChillNum ).DesignHeatRecVolFlowRate;

				InitComponentNodes( 0.0, ElectricChiller( ChillNum ).DesignHeatRecMassFlowRate, ElectricChiller( ChillNum ).HeatRecInletNodeNum, ElectricChiller( ChillNum ).HeatRecOutletNodeNum, ElectricChiller( ChillNum ).HRLoopNum, ElectricChiller( ChillNum ).HRLoopSideNum, ElectricChiller( ChillNum ).HRBranchNum, ElectricChiller( ChillNum ).HRCompNum );
				ElectricChiller( ChillNum ).HeatRecMaxCapacityLimit = ElectricChiller( ChillNum ).HeatRecCapacityFraction * ( ElectricChiller( ChillNum ).NomCap + ElectricChiller( ChillNum ).NomCap / ElectricChiller( ChillNum ).COP );

				if ( ElectricChiller( ChillNum ).HeatRecSetPointNodeNum > 0 ) {
					{ auto const SELECT_CASE_var( PlantLoop( ElectricChiller( ChillNum ).HRLoopNum ).LoopDemandCalcScheme );
					if ( SELECT_CASE_var == SingleSetPoint ) {
						THeatRecSetPoint = Node( ElectricChiller( ChillNum ).HeatRecSetPointNodeNum ).TempSetPoint;
					} else if ( SELECT_CASE_var == DualSetPointDeadBand ) {
						THeatRecSetPoint = Node( ElectricChiller( ChillNum ).HeatRecSetPointNodeNum ).TempSetPointHi;
					}}
					if ( THeatRecSetPoint == SensedNodeFlagValue ) {
						if ( ! AnyEnergyManagementSystemInModel ) {
							if ( ! ElectricChiller( ChillNum ).HRSPErrDone ) {
								ShowWarningError( "Missing heat recovery temperature setpoint for chiller named " + ElectricChiller( ChillNum ).Name );
								ShowContinueError( "  A temperature setpoint is needed at the heat recovery leaving temperature setpoint node specified, use a SetpointManager" );
								ShowContinueError( "  The overall loop setpoint will be assumed for heat recovery. The simulation continues ..." );
								ElectricChiller( ChillNum ).HeatRecSetPointNodeNum = PlantLoop( ElectricChiller( ChillNum ).HRLoopNum ).TempSetPointNodeNum;
								ElectricChiller( ChillNum ).HRSPErrDone = true;
							}
						} else {
							// need call to EMS to check node
							FatalError = false; // but not really fatal yet, but should be.
							CheckIfNodeSetPointManagedByEMS( ElectricChiller( ChillNum ).EvapOutletNodeNum, iTemperatureSetPoint, FatalError );
							if ( FatalError ) {
								if ( ! ElectricChiller( ChillNum ).HRSPErrDone ) {
									ShowWarningError( "Missing heat recovery temperature setpoint for chiller named " + ElectricChiller( ChillNum ).Name );
									ShowContinueError( "  A temperature setpoint is needed at the heat recovery leaving temperature setpoint node specified, use a SetpointManager to establish a setpoint" );
									ShowContinueError( "  or use an EMS actuator to establish a setpoint at this node " );
									ShowContinueError( "  The overall loop setpoint will be assumed for heat recovery. The simulation continues ..." );
									ElectricChiller( ChillNum ).HeatRecSetPointNodeNum = PlantLoop( ElectricChiller( ChillNum ).HRLoopNum ).TempSetPointNodeNum;
									ElectricChiller( ChillNum ).HRSPErrDone = true;
								}
							}
						} // IF (.NOT. AnyEnergyManagementSystemInModel) THEN
					} // IF(THeatRecSetpoint == SensedNodeFlagValue)THEN
				} // IF(ElectricChiller(ChillNum)%HeatRecSetpointNodeNum > 0)THEN
			} // IF (ElectricChiller(ChillNum)%HeatRecActive) THEN

			MyEnvrnFlag( ChillNum ) = false;
		}
		if ( ! BeginEnvrnFlag ) {
			MyEnvrnFlag( ChillNum ) = true;
		}

		if ( ( ElectricChiller( ChillNum ).FlowMode == LeavingSetPointModulated ) && ( ElectricChiller( ChillNum ).ModulatedFlowSetToLoop ) ) {
			// fix for clumsy old input that worked because loop setpoint was spread.
			//  could be removed with transition, testing , model change, period of being obsolete.
			{ auto const SELECT_CASE_var( PlantLoop( ElectricChiller( ChillNum ).CWLoopNum ).LoopDemandCalcScheme );
			if ( SELECT_CASE_var == SingleSetPoint ) {
				Node( ElectricChiller( ChillNum ).EvapOutletNodeNum ).TempSetPoint = Node( PlantLoop( ElectricChiller( ChillNum ).CWLoopNum ).TempSetPointNodeNum ).TempSetPoint;
			} else if ( SELECT_CASE_var == DualSetPointDeadBand ) {
				Node( ElectricChiller( ChillNum ).EvapOutletNodeNum ).TempSetPointHi = Node( PlantLoop( ElectricChiller( ChillNum ).CWLoopNum ).TempSetPointNodeNum ).TempSetPointHi;
			}}

		}

		if ( ( MyLoad < 0.0 ) && RunFlag ) {
			// request full then take what can get
			mdot = ElectricChiller( ChillNum ).EvapMassFlowRateMax;
			mdotCond = ElectricChiller( ChillNum ).CondMassFlowRateMax;
		} else {
			mdot = 0.0;
			mdotCond = 0.0;
		}

		SetComponentFlowRate( mdot, EvapInletNode, EvapOutletNode, ElectricChiller( ChillNum ).CWLoopNum, ElectricChiller( ChillNum ).CWLoopSideNum, ElectricChiller( ChillNum ).CWBranchNum, ElectricChiller( ChillNum ).CWCompNum );
		if ( ElectricChiller( ChillNum ).CondenserType == WaterCooled ) {
			SetComponentFlowRate( mdotCond, CondInletNode, CondOutletNode, ElectricChiller( ChillNum ).CDLoopNum, ElectricChiller( ChillNum ).CDLoopSideNum, ElectricChiller( ChillNum ).CDBranchNum, ElectricChiller( ChillNum ).CDCompNum );
		}

		// Initialize heat recovery flow rates at node
		if ( ElectricChiller( ChillNum ).HeatRecActive ) {

			InletNode = ElectricChiller( ChillNum ).HeatRecInletNodeNum;
			OutletNode = ElectricChiller( ChillNum ).HeatRecOutletNodeNum;
			LoopNum = ElectricChiller( ChillNum ).HRLoopNum;
			LoopSideNum = ElectricChiller( ChillNum ).HRLoopSideNum;
			BranchIndex = ElectricChiller( ChillNum ).HRBranchNum;
			CompIndex = ElectricChiller( ChillNum ).HRCompNum;

			if ( RunFlag ) {
				mdot = ElectricChiller( ChillNum ).DesignHeatRecMassFlowRate;
			} else {
				mdot = 0.0;
			}

			SetComponentFlowRate( mdot, InletNode, OutletNode, LoopNum, LoopSideNum, BranchIndex, CompIndex );

		}

		if ( ElectricChiller( ChillNum ).CondenserType == EvapCooled ) {
			BasinHeaterPower = 0.0;
		}

	}

	void
	InitEngineDrivenChiller(
		int const ChillNum, // number of the current engine driven chiller being simulated
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
		static bool MyOneTimeFlag( true );
		static Array1D_bool MyEnvrnFlag;
		static Array1D_bool MyFlag;
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

		// FLOW:

		// Do the one time initializations
		if ( MyOneTimeFlag ) {
			MyFlag.allocate( NumEngineDrivenChillers );
			MyEnvrnFlag.allocate( NumEngineDrivenChillers );
			MyFlag = true;
			MyEnvrnFlag = true;
			MyOneTimeFlag = false;
		}

		//Load inputs to local structure
		CondInletNode = EngineDrivenChiller( ChillNum ).CondInletNodeNum;
		CondOutletNode = EngineDrivenChiller( ChillNum ).CondOutletNodeNum;
		EvapInletNode = EngineDrivenChiller( ChillNum ).EvapInletNodeNum;
		EvapOutletNode = EngineDrivenChiller( ChillNum ).EvapOutletNodeNum;

		if ( EngineDrivenChiller( ChillNum ).HeatRecActive ) {
			HeatRecInNode = EngineDrivenChiller( ChillNum ).HeatRecInletNodeNum;
			HeatRecOutNode = EngineDrivenChiller( ChillNum ).HeatRecOutletNodeNum;
		}

		// Init more variables
		if ( MyFlag( ChillNum ) ) {
			// Locate the chillers on the plant loops for later usage
			errFlag = false;
			ScanPlantLoopsForObject( EngineDrivenChiller( ChillNum ).Name, TypeOf_Chiller_EngineDriven, EngineDrivenChiller( ChillNum ).CWLoopNum, EngineDrivenChiller( ChillNum ).CWLoopSideNum, EngineDrivenChiller( ChillNum ).CWBranchNum, EngineDrivenChiller( ChillNum ).CWCompNum, EngineDrivenChiller( ChillNum ).TempLowLimitEvapOut, _, _, EngineDrivenChiller( ChillNum ).EvapInletNodeNum, _, errFlag );
			if ( EngineDrivenChiller( ChillNum ).CondenserType != AirCooled && EngineDrivenChiller( ChillNum ).CondenserType != EvapCooled ) {
				ScanPlantLoopsForObject( EngineDrivenChiller( ChillNum ).Name, TypeOf_Chiller_EngineDriven, EngineDrivenChiller( ChillNum ).CDLoopNum, EngineDrivenChiller( ChillNum ).CDLoopSideNum, EngineDrivenChiller( ChillNum ).CDBranchNum, EngineDrivenChiller( ChillNum ).CDCompNum, _, _, _, EngineDrivenChiller( ChillNum ).CondInletNodeNum, _, errFlag );
				InterConnectTwoPlantLoopSides( EngineDrivenChiller( ChillNum ).CWLoopNum, EngineDrivenChiller( ChillNum ).CWLoopSideNum, EngineDrivenChiller( ChillNum ).CDLoopNum, EngineDrivenChiller( ChillNum ).CDLoopSideNum, TypeOf_Chiller_EngineDriven, true );
			}
			if ( EngineDrivenChiller( ChillNum ).HeatRecActive ) {
				ScanPlantLoopsForObject( EngineDrivenChiller( ChillNum ).Name, TypeOf_Chiller_EngineDriven, EngineDrivenChiller( ChillNum ).HRLoopNum, EngineDrivenChiller( ChillNum ).HRLoopSideNum, EngineDrivenChiller( ChillNum ).HRBranchNum, EngineDrivenChiller( ChillNum ).HRCompNum, _, _, _, EngineDrivenChiller( ChillNum ).HeatRecInletNodeNum, _, errFlag );
				InterConnectTwoPlantLoopSides( EngineDrivenChiller( ChillNum ).CWLoopNum, EngineDrivenChiller( ChillNum ).CWLoopSideNum, EngineDrivenChiller( ChillNum ).HRLoopNum, EngineDrivenChiller( ChillNum ).HRLoopSideNum, TypeOf_Chiller_EngineDriven, true );
			}
			MyFlag( ChillNum ) = false;
			if ( EngineDrivenChiller( ChillNum ).CondenserType != AirCooled && EngineDrivenChiller( ChillNum ).CondenserType != EvapCooled && EngineDrivenChiller( ChillNum ).HeatRecActive ) {
				InterConnectTwoPlantLoopSides( EngineDrivenChiller( ChillNum ).CDLoopNum, EngineDrivenChiller( ChillNum ).CDLoopSideNum, EngineDrivenChiller( ChillNum ).HRLoopNum, EngineDrivenChiller( ChillNum ).HRLoopSideNum, TypeOf_Chiller_EngineDriven, false );
			}
			if ( errFlag ) {
				ShowFatalError( "InitEngineDrivenChiller: Program terminated due to previous condition(s)." );
			}

			if ( EngineDrivenChiller( ChillNum ).FlowMode == ConstantFlow ) {
				// reset flow priority
				PlantLoop( EngineDrivenChiller( ChillNum ).CWLoopNum ).LoopSide( EngineDrivenChiller( ChillNum ).CWLoopSideNum ).Branch( EngineDrivenChiller( ChillNum ).CWBranchNum ).Comp( EngineDrivenChiller( ChillNum ).CWCompNum ).FlowPriority = LoopFlowStatus_NeedyIfLoopOn;
			}

			if ( EngineDrivenChiller( ChillNum ).FlowMode == LeavingSetPointModulated ) {
				// reset flow priority
				PlantLoop( EngineDrivenChiller( ChillNum ).CWLoopNum ).LoopSide( EngineDrivenChiller( ChillNum ).CWLoopSideNum ).Branch( EngineDrivenChiller( ChillNum ).CWBranchNum ).Comp( EngineDrivenChiller( ChillNum ).CWCompNum ).FlowPriority = LoopFlowStatus_NeedyIfLoopOn;
				// check if setpoint on outlet node
				if ( ( Node( EngineDrivenChiller( ChillNum ).EvapOutletNodeNum ).TempSetPoint == SensedNodeFlagValue ) && ( Node( EngineDrivenChiller( ChillNum ).EvapOutletNodeNum ).TempSetPointHi == SensedNodeFlagValue ) ) {
					if ( ! AnyEnergyManagementSystemInModel ) {
						if ( ! EngineDrivenChiller( ChillNum ).ModulatedFlowErrDone ) {
							ShowWarningError( "Missing temperature setpoint for LeavingSetpointModulated mode chiller named " + EngineDrivenChiller( ChillNum ).Name );
							ShowContinueError( "  A temperature setpoint is needed at the outlet node of a chiller in variable flow mode, use a SetpointManager" );
							ShowContinueError( "  The overall loop setpoint will be assumed for chiller. The simulation continues ... " );
							EngineDrivenChiller( ChillNum ).ModulatedFlowErrDone = true;
						}
					} else {
						// need call to EMS to check node
						FatalError = false; // but not really fatal yet, but should be.
						CheckIfNodeSetPointManagedByEMS( EngineDrivenChiller( ChillNum ).EvapOutletNodeNum, iTemperatureSetPoint, FatalError );
						if ( FatalError ) {
							if ( ! EngineDrivenChiller( ChillNum ).ModulatedFlowErrDone ) {
								ShowWarningError( "Missing temperature setpoint for LeavingSetpointModulated mode chiller named " + EngineDrivenChiller( ChillNum ).Name );
								ShowContinueError( "  A temperature setpoint is needed at the outlet node of a chiller evaporator in variable flow mode" );
								ShowContinueError( "  use a Setpoint Manager to establish a setpoint at the chiller evaporator outlet node " );
								ShowContinueError( "  or use an EMS actuator to establish a setpoint at the outlet node " );
								ShowContinueError( "  The overall loop setpoint will be assumed for chiller. The simulation continues ... " );
								EngineDrivenChiller( ChillNum ).ModulatedFlowErrDone = true;
							}
						}

					}
					EngineDrivenChiller( ChillNum ).ModulatedFlowSetToLoop = true;
					Node( EngineDrivenChiller( ChillNum ).EvapOutletNodeNum ).TempSetPoint = Node( PlantLoop( EngineDrivenChiller( ChillNum ).CWLoopNum ).TempSetPointNodeNum ).TempSetPoint;
					Node( EngineDrivenChiller( ChillNum ).EvapOutletNodeNum ).TempSetPointHi = Node( PlantLoop( EngineDrivenChiller( ChillNum ).CWLoopNum ).TempSetPointNodeNum ).TempSetPointHi;
				}
			}

			MyFlag( ChillNum ) = false;
		}

		//Initialize critical Demand Side Variables
		//  IF((MyEnvrnFlag(ChillNum) .and. BeginEnvrnFlag) &
		//     .OR. (Node(CondInletNode)%MassFlowrate <= 0.0 .AND. RunFlag)) THEN
		if ( MyEnvrnFlag( ChillNum ) && BeginEnvrnFlag && ( PlantFirstSizesOkayToFinalize ) ) {

			rho = GetDensityGlycol( PlantLoop( EngineDrivenChiller( ChillNum ).CWLoopNum ).FluidName, InitConvTemp, PlantLoop( EngineDrivenChiller( ChillNum ).CWLoopNum ).FluidIndex, RoutineName );

			EngineDrivenChiller( ChillNum ).EvapMassFlowRateMax = rho * EngineDrivenChiller( ChillNum ).EvapVolFlowRate;
			InitComponentNodes( 0.0, EngineDrivenChiller( ChillNum ).EvapMassFlowRateMax, EvapInletNode, EvapOutletNode, EngineDrivenChiller( ChillNum ).CWLoopNum, EngineDrivenChiller( ChillNum ).CWLoopSideNum, EngineDrivenChiller( ChillNum ).CWBranchNum, EngineDrivenChiller( ChillNum ).CWCompNum );

			//init maximum available condenser flow rate

			if ( EngineDrivenChiller( ChillNum ).CondenserType == WaterCooled ) {

				Node( CondInletNode ).Temp = EngineDrivenChiller( ChillNum ).TempDesCondIn;

				rho = GetDensityGlycol( PlantLoop( EngineDrivenChiller( ChillNum ).CDLoopNum ).FluidName, InitConvTemp, PlantLoop( EngineDrivenChiller( ChillNum ).CDLoopNum ).FluidIndex, RoutineName );

				EngineDrivenChiller( ChillNum ).CondMassFlowRateMax = rho * EngineDrivenChiller( ChillNum ).CondVolFlowRate;

				InitComponentNodes( 0.0, EngineDrivenChiller( ChillNum ).CondMassFlowRateMax, CondInletNode, CondOutletNode, EngineDrivenChiller( ChillNum ).CDLoopNum, EngineDrivenChiller( ChillNum ).CDLoopSideNum, EngineDrivenChiller( ChillNum ).CDBranchNum, EngineDrivenChiller( ChillNum ).CDCompNum );
			} else { // air or evap-air
				Node( CondInletNode ).MassFlowRate = EngineDrivenChiller( ChillNum ).CondVolFlowRate * PsyRhoAirFnPbTdbW( StdBaroPress, EngineDrivenChiller( ChillNum ).TempDesCondIn, 0.0, RoutineName );

				Node( CondOutletNode ).MassFlowRate = Node( CondInletNode ).MassFlowRate;
				Node( CondInletNode ).MassFlowRateMaxAvail = Node( CondInletNode ).MassFlowRate;
				Node( CondInletNode ).MassFlowRateMax = Node( CondInletNode ).MassFlowRate;
				Node( CondOutletNode ).MassFlowRateMax = Node( CondInletNode ).MassFlowRate;
				Node( CondInletNode ).MassFlowRateMinAvail = 0.0;
				Node( CondInletNode ).MassFlowRateMin = 0.0;
				Node( CondOutletNode ).MassFlowRateMinAvail = 0.0;
				Node( CondOutletNode ).MassFlowRateMin = 0.0;
			}

			if ( EngineDrivenChiller( ChillNum ).HeatRecActive ) {
				rho = GetDensityGlycol( PlantLoop( EngineDrivenChiller( ChillNum ).HRLoopNum ).FluidName, InitConvTemp, PlantLoop( EngineDrivenChiller( ChillNum ).HRLoopNum ).FluidIndex, RoutineName );
				EngineDrivenChiller( ChillNum ).DesignHeatRecMassFlowRate = rho * EngineDrivenChiller( ChillNum ).DesignHeatRecVolFlowRate;

				InitComponentNodes( 0.0, EngineDrivenChiller( ChillNum ).DesignHeatRecMassFlowRate, EngineDrivenChiller( ChillNum ).HeatRecInletNodeNum, EngineDrivenChiller( ChillNum ).HeatRecOutletNodeNum, EngineDrivenChiller( ChillNum ).HRLoopNum, EngineDrivenChiller( ChillNum ).HRLoopSideNum, EngineDrivenChiller( ChillNum ).HRBranchNum, EngineDrivenChiller( ChillNum ).HRCompNum );
			}

			MyEnvrnFlag( ChillNum ) = false;
		}

		if ( ! BeginEnvrnFlag ) {
			MyEnvrnFlag( ChillNum ) = true;
		}

		if ( ( EngineDrivenChiller( ChillNum ).FlowMode == LeavingSetPointModulated ) && ( EngineDrivenChiller( ChillNum ).ModulatedFlowSetToLoop ) ) {
			// fix for clumsy old input that worked because loop setpoint was spread.
			//  could be removed with transition, testing , model change, period of being obsolete.
			Node( EngineDrivenChiller( ChillNum ).EvapOutletNodeNum ).TempSetPoint = Node( PlantLoop( EngineDrivenChiller( ChillNum ).CWLoopNum ).TempSetPointNodeNum ).TempSetPoint;
			Node( EngineDrivenChiller( ChillNum ).EvapOutletNodeNum ).TempSetPointHi = Node( PlantLoop( EngineDrivenChiller( ChillNum ).CWLoopNum ).TempSetPointNodeNum ).TempSetPointHi;
		}

		if ( ( std::abs( MyLoad ) > 0.0 ) && RunFlag ) {
			mdot = EngineDrivenChiller( ChillNum ).EvapMassFlowRateMax;
			mdotCond = EngineDrivenChiller( ChillNum ).CondMassFlowRateMax;
		} else {
			mdot = 0.0;
			mdotCond = 0.0;
		}

		SetComponentFlowRate( mdot, EvapInletNode, EvapOutletNode, EngineDrivenChiller( ChillNum ).CWLoopNum, EngineDrivenChiller( ChillNum ).CWLoopSideNum, EngineDrivenChiller( ChillNum ).CWBranchNum, EngineDrivenChiller( ChillNum ).CWCompNum );
		if ( EngineDrivenChiller( ChillNum ).CondenserType == WaterCooled ) {
			SetComponentFlowRate( mdotCond, CondInletNode, CondOutletNode, EngineDrivenChiller( ChillNum ).CDLoopNum, EngineDrivenChiller( ChillNum ).CDLoopSideNum, EngineDrivenChiller( ChillNum ).CDBranchNum, EngineDrivenChiller( ChillNum ).CDCompNum );
		}

		// Initialize heat recovery flow rates at node
		if ( EngineDrivenChiller( ChillNum ).HeatRecActive ) {
			InletNode = EngineDrivenChiller( ChillNum ).HeatRecInletNodeNum;
			OutletNode = EngineDrivenChiller( ChillNum ).HeatRecOutletNodeNum;
			LoopNum = EngineDrivenChiller( ChillNum ).HRLoopNum;
			LoopSideNum = EngineDrivenChiller( ChillNum ).HRLoopSideNum;
			BranchIndex = EngineDrivenChiller( ChillNum ).HRBranchNum;
			CompIndex = EngineDrivenChiller( ChillNum ).HRCompNum;

			if ( RunFlag ) {
				mdot = EngineDrivenChiller( ChillNum ).DesignHeatRecMassFlowRate;
			} else {
				mdot = 0.0;
			}

			SetComponentFlowRate( mdot, InletNode, OutletNode, LoopNum, LoopSideNum, BranchIndex, CompIndex );

		}
		if ( EngineDrivenChiller( ChillNum ).CondenserType == EvapCooled ) {
			BasinHeaterPower = 0.0;
		}

	}

	void
	InitGTChiller(
		int const ChillNum, // number of the current engine driven chiller being simulated
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
		static bool MyOneTimeFlag( true );
		static Array1D_bool MyEnvrnFlag;
		static Array1D_bool MyFlag;
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
		// FLOW:

		// Do the one time initializations
		if ( MyOneTimeFlag ) {
			MyFlag.allocate( NumGTChillers );
			MyEnvrnFlag.allocate( NumGTChillers );
			MyFlag = true;
			MyEnvrnFlag = true;
			MyOneTimeFlag = false;
		}

		CondInletNode = GTChiller( ChillNum ).CondInletNodeNum;
		CondOutletNode = GTChiller( ChillNum ).CondOutletNodeNum;
		EvapInletNode = GTChiller( ChillNum ).EvapInletNodeNum;
		EvapOutletNode = GTChiller( ChillNum ).EvapOutletNodeNum;

		if ( GTChiller( ChillNum ).HeatRecActive ) {
			HeatRecInNode = GTChiller( ChillNum ).HeatRecInletNodeNum;
			HeatRecOutNode = GTChiller( ChillNum ).HeatRecOutletNodeNum;
		}

		// Init more variables
		if ( MyFlag( ChillNum ) ) {
			// Locate the chillers on the plant loops for later usage
			errFlag = false;
			ScanPlantLoopsForObject( GTChiller( ChillNum ).Name, TypeOf_Chiller_CombTurbine, GTChiller( ChillNum ).CWLoopNum, GTChiller( ChillNum ).CWLoopSideNum, GTChiller( ChillNum ).CWBranchNum, GTChiller( ChillNum ).CWCompNum, GTChiller( ChillNum ).TempLowLimitEvapOut, _, _, GTChiller( ChillNum ).EvapInletNodeNum, _, errFlag );
			if ( GTChiller( ChillNum ).CondenserType != AirCooled && GTChiller( ChillNum ).CondenserType != EvapCooled ) {
				ScanPlantLoopsForObject( GTChiller( ChillNum ).Name, TypeOf_Chiller_CombTurbine, GTChiller( ChillNum ).CDLoopNum, GTChiller( ChillNum ).CDLoopSideNum, GTChiller( ChillNum ).CDBranchNum, GTChiller( ChillNum ).CDCompNum, _, _, _, GTChiller( ChillNum ).CondInletNodeNum, _, errFlag );
				InterConnectTwoPlantLoopSides( GTChiller( ChillNum ).CWLoopNum, GTChiller( ChillNum ).CWLoopSideNum, GTChiller( ChillNum ).CDLoopNum, GTChiller( ChillNum ).CDLoopSideNum, TypeOf_Chiller_CombTurbine, true );
			}
			if ( GTChiller( ChillNum ).HeatRecActive ) {
				ScanPlantLoopsForObject( GTChiller( ChillNum ).Name, TypeOf_Chiller_CombTurbine, GTChiller( ChillNum ).HRLoopNum, GTChiller( ChillNum ).HRLoopSideNum, GTChiller( ChillNum ).HRBranchNum, GTChiller( ChillNum ).HRCompNum, _, _, _, GTChiller( ChillNum ).HeatRecInletNodeNum, _, errFlag );
				InterConnectTwoPlantLoopSides( GTChiller( ChillNum ).CWLoopNum, GTChiller( ChillNum ).CWLoopSideNum, GTChiller( ChillNum ).HRLoopNum, GTChiller( ChillNum ).HRLoopSideNum, TypeOf_Chiller_CombTurbine, true );
			}

			if ( GTChiller( ChillNum ).CondenserType != AirCooled && GTChiller( ChillNum ).CondenserType != EvapCooled && GTChiller( ChillNum ).HeatRecActive ) {
				InterConnectTwoPlantLoopSides( GTChiller( ChillNum ).CDLoopNum, GTChiller( ChillNum ).CDLoopSideNum, GTChiller( ChillNum ).HRLoopNum, GTChiller( ChillNum ).HRLoopSideNum, TypeOf_Chiller_CombTurbine, false );
			}
			if ( errFlag ) {
				ShowFatalError( "InitGTChiller: Program terminated due to previous condition(s)." );
			}

			if ( GTChiller( ChillNum ).FlowMode == ConstantFlow ) {
				// reset flow priority
				PlantLoop( GTChiller( ChillNum ).CWLoopNum ).LoopSide( GTChiller( ChillNum ).CWLoopSideNum ).Branch( GTChiller( ChillNum ).CWBranchNum ).Comp( GTChiller( ChillNum ).CWCompNum ).FlowPriority = LoopFlowStatus_NeedyIfLoopOn;
			}

			if ( GTChiller( ChillNum ).FlowMode == LeavingSetPointModulated ) {
				// reset flow priority
				PlantLoop( GTChiller( ChillNum ).CWLoopNum ).LoopSide( GTChiller( ChillNum ).CWLoopSideNum ).Branch( GTChiller( ChillNum ).CWBranchNum ).Comp( GTChiller( ChillNum ).CWCompNum ).FlowPriority = LoopFlowStatus_NeedyIfLoopOn;

				// check if setpoint on outlet node
				if ( ( Node( GTChiller( ChillNum ).EvapOutletNodeNum ).TempSetPoint == SensedNodeFlagValue ) && ( Node( GTChiller( ChillNum ).EvapOutletNodeNum ).TempSetPointHi == SensedNodeFlagValue ) ) {
					if ( ! AnyEnergyManagementSystemInModel ) {
						if ( ! GTChiller( ChillNum ).ModulatedFlowErrDone ) {
							ShowWarningError( "Missing temperature setpoint for LeavingSetpointModulated mode chiller named " + GTChiller( ChillNum ).Name );
							ShowContinueError( "  A temperature setpoint is needed at the outlet node of a chiller in variable flow mode, use a SetpointManager" );
							ShowContinueError( "  The overall loop setpoint will be assumed for chiller. The simulation continues ... " );
							GTChiller( ChillNum ).ModulatedFlowErrDone = true;
						}
					} else {
						// need call to EMS to check node
						FatalError = false; // but not really fatal yet, but should be.
						CheckIfNodeSetPointManagedByEMS( GTChiller( ChillNum ).EvapOutletNodeNum, iTemperatureSetPoint, FatalError );
						if ( FatalError ) {
							if ( ! GTChiller( ChillNum ).ModulatedFlowErrDone ) {
								ShowWarningError( "Missing temperature setpoint for LeavingSetpointModulated mode chiller named " + GTChiller( ChillNum ).Name );
								ShowContinueError( "  A temperature setpoint is needed at the outlet node of a chiller evaporator in variable flow mode" );
								ShowContinueError( "  use a Setpoint Manager to establish a setpoint at the chiller evaporator outlet node " );
								ShowContinueError( "  or use an EMS actuator to establish a setpoint at the outlet node " );
								ShowContinueError( "  The overall loop setpoint will be assumed for chiller. The simulation continues ... " );
								GTChiller( ChillNum ).ModulatedFlowErrDone = true;
							}
						}

					}
					GTChiller( ChillNum ).ModulatedFlowSetToLoop = true;
					Node( GTChiller( ChillNum ).EvapOutletNodeNum ).TempSetPoint = Node( PlantLoop( GTChiller( ChillNum ).CWLoopNum ).TempSetPointNodeNum ).TempSetPoint;
					Node( GTChiller( ChillNum ).EvapOutletNodeNum ).TempSetPointHi = Node( PlantLoop( GTChiller( ChillNum ).CWLoopNum ).TempSetPointNodeNum ).TempSetPointHi;
				}
			}
			MyFlag( ChillNum ) = false;
		}

		if ( MyEnvrnFlag( ChillNum ) && BeginEnvrnFlag && ( PlantFirstSizesOkayToFinalize ) ) {

			rho = GetDensityGlycol( PlantLoop( GTChiller( ChillNum ).CWLoopNum ).FluidName, InitConvTemp, PlantLoop( GTChiller( ChillNum ).CWLoopNum ).FluidIndex, RoutineName );

			GTChiller( ChillNum ).EvapMassFlowRateMax = rho * GTChiller( ChillNum ).EvapVolFlowRate;
			InitComponentNodes( 0.0, GTChiller( ChillNum ).EvapMassFlowRateMax, EvapInletNode, EvapOutletNode, GTChiller( ChillNum ).CWLoopNum, GTChiller( ChillNum ).CWLoopSideNum, GTChiller( ChillNum ).CWBranchNum, GTChiller( ChillNum ).CWCompNum );

			//init maximum available condenser flow rate
			if ( GTChiller( ChillNum ).CondenserType == WaterCooled ) {

				Node( CondInletNode ).Temp = GTChiller( ChillNum ).TempDesCondIn;

				rho = GetDensityGlycol( PlantLoop( GTChiller( ChillNum ).CDLoopNum ).FluidName, InitConvTemp, PlantLoop( GTChiller( ChillNum ).CDLoopNum ).FluidIndex, RoutineName );

				GTChiller( ChillNum ).CondMassFlowRateMax = rho * GTChiller( ChillNum ).CondVolFlowRate;

				InitComponentNodes( 0.0, GTChiller( ChillNum ).CondMassFlowRateMax, CondInletNode, CondOutletNode, GTChiller( ChillNum ).CDLoopNum, GTChiller( ChillNum ).CDLoopSideNum, GTChiller( ChillNum ).CDBranchNum, GTChiller( ChillNum ).CDCompNum );
			} else { // air or evap-air
				Node( CondInletNode ).MassFlowRate = GTChiller( ChillNum ).CondVolFlowRate * PsyRhoAirFnPbTdbW( StdBaroPress, GTChiller( ChillNum ).TempDesCondIn, 0.0, RoutineName );

				Node( CondOutletNode ).MassFlowRate = Node( CondInletNode ).MassFlowRate;
				Node( CondInletNode ).MassFlowRateMaxAvail = Node( CondInletNode ).MassFlowRate;
				Node( CondInletNode ).MassFlowRateMax = Node( CondInletNode ).MassFlowRate;
				Node( CondOutletNode ).MassFlowRateMax = Node( CondInletNode ).MassFlowRate;
				Node( CondInletNode ).MassFlowRateMinAvail = 0.0;
				Node( CondInletNode ).MassFlowRateMin = 0.0;
				Node( CondOutletNode ).MassFlowRateMinAvail = 0.0;
				Node( CondOutletNode ).MassFlowRateMin = 0.0;
			}

			if ( GTChiller( ChillNum ).HeatRecActive ) {
				rho = GetDensityGlycol( PlantLoop( GTChiller( ChillNum ).HRLoopNum ).FluidName, InitConvTemp, PlantLoop( GTChiller( ChillNum ).HRLoopNum ).FluidIndex, RoutineName );
				GTChiller( ChillNum ).DesignHeatRecMassFlowRate = rho * GTChiller( ChillNum ).DesignHeatRecVolFlowRate;

				InitComponentNodes( 0.0, GTChiller( ChillNum ).DesignHeatRecMassFlowRate, GTChiller( ChillNum ).HeatRecInletNodeNum, GTChiller( ChillNum ).HeatRecOutletNodeNum, GTChiller( ChillNum ).HRLoopNum, GTChiller( ChillNum ).HRLoopSideNum, GTChiller( ChillNum ).HRBranchNum, GTChiller( ChillNum ).HRCompNum );

			}

			MyEnvrnFlag( ChillNum ) = false;
		}

		if ( ! BeginEnvrnFlag ) {
			MyEnvrnFlag( ChillNum ) = true;
		}

		if ( ( GTChiller( ChillNum ).FlowMode == LeavingSetPointModulated ) && ( GTChiller( ChillNum ).ModulatedFlowSetToLoop ) ) {
			// fix for clumsy old input that worked because loop setpoint was spread.
			//  could be removed with transition, testing , model change, period of being obsolete.
			Node( GTChiller( ChillNum ).EvapOutletNodeNum ).TempSetPoint = Node( PlantLoop( GTChiller( ChillNum ).CWLoopNum ).TempSetPointNodeNum ).TempSetPoint;
			Node( GTChiller( ChillNum ).EvapOutletNodeNum ).TempSetPointHi = Node( PlantLoop( GTChiller( ChillNum ).CWLoopNum ).TempSetPointNodeNum ).TempSetPointHi;
		}

		if ( ( std::abs( MyLoad ) > 0.0 ) && RunFlag ) {
			mdot = GTChiller( ChillNum ).EvapMassFlowRateMax;
			mdotCond = GTChiller( ChillNum ).CondMassFlowRateMax;
		} else {
			mdot = 0.0;
			mdotCond = 0.0;
		}

		SetComponentFlowRate( mdot, EvapInletNode, EvapOutletNode, GTChiller( ChillNum ).CWLoopNum, GTChiller( ChillNum ).CWLoopSideNum, GTChiller( ChillNum ).CWBranchNum, GTChiller( ChillNum ).CWCompNum );
		if ( GTChiller( ChillNum ).CondenserType == WaterCooled ) {
			SetComponentFlowRate( mdotCond, CondInletNode, CondOutletNode, GTChiller( ChillNum ).CDLoopNum, GTChiller( ChillNum ).CDLoopSideNum, GTChiller( ChillNum ).CDBranchNum, GTChiller( ChillNum ).CDCompNum );
		}

		// Initialize heat recovery flow rates at node
		if ( GTChiller( ChillNum ).HeatRecActive ) {

			InletNode = GTChiller( ChillNum ).HeatRecInletNodeNum;
			OutletNode = GTChiller( ChillNum ).HeatRecOutletNodeNum;
			LoopNum = GTChiller( ChillNum ).HRLoopNum;
			LoopSideNum = GTChiller( ChillNum ).HRLoopSideNum;
			BranchIndex = GTChiller( ChillNum ).HRBranchNum;
			CompIndex = GTChiller( ChillNum ).HRCompNum;

			if ( RunFlag ) {
				mdot = GTChiller( ChillNum ).DesignHeatRecMassFlowRate;
			} else {
				mdot = 0.0;
			}

			SetComponentFlowRate( mdot, InletNode, OutletNode, LoopNum, LoopSideNum, BranchIndex, CompIndex );

		}
		if ( GTChiller( ChillNum ).CondenserType == EvapCooled ) {
			BasinHeaterPower = 0.0;
		}

	}

	void
	InitConstCOPChiller(
		int const ChillNum, // number of the current electric chiller being simulated
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
		static bool OneTimeFlag( true );
		static Array1D_bool MyFlag;
		static Array1D_bool MyEnvironFlag;
		int CondInletNode; // node number of water inlet node to the condenser
		int CondOutletNode; // node number of water outlet node from the condenser
		int EvapInletNode;
		int EvapOutletNode;
		Real64 rho; // local fluid density
		Real64 mdot; // local mass flow rate
		Real64 mdotCond; // local mass flow rate for condenser
		bool FatalError;
		bool errFlag;
		//FLOW
		// Do the one time initializations

		if ( OneTimeFlag ) {
			MyFlag.allocate( NumConstCOPChillers );
			MyEnvironFlag.allocate( NumConstCOPChillers );
			MyFlag = true;
			MyEnvironFlag = true;
			OneTimeFlag = false;
		}

		EvapInletNode = ConstCOPChiller( ChillNum ).EvapInletNodeNum;
		EvapOutletNode = ConstCOPChiller( ChillNum ).EvapOutletNodeNum;
		CondInletNode = ConstCOPChiller( ChillNum ).CondInletNodeNum;
		CondOutletNode = ConstCOPChiller( ChillNum ).CondOutletNodeNum;

		// Init more variables
		if ( MyFlag( ChillNum ) ) {
			// Locate the chillers on the plant loops for later usage
			errFlag = false;
			ScanPlantLoopsForObject( ConstCOPChiller( ChillNum ).Name, TypeOf_Chiller_ConstCOP, ConstCOPChiller( ChillNum ).CWLoopNum, ConstCOPChiller( ChillNum ).CWLoopSideNum, ConstCOPChiller( ChillNum ).CWBranchNum, ConstCOPChiller( ChillNum ).CWCompNum, _, _, _, ConstCOPChiller( ChillNum ).EvapInletNodeNum, _, errFlag );
			if ( ConstCOPChiller( ChillNum ).CondenserType != AirCooled && ConstCOPChiller( ChillNum ).CondenserType != EvapCooled ) {
				ScanPlantLoopsForObject( ConstCOPChiller( ChillNum ).Name, TypeOf_Chiller_ConstCOP, ConstCOPChiller( ChillNum ).CDLoopNum, ConstCOPChiller( ChillNum ).CDLoopSideNum, ConstCOPChiller( ChillNum ).CDBranchNum, ConstCOPChiller( ChillNum ).CDCompNum, _, _, _, ConstCOPChiller( ChillNum ).CondInletNodeNum, _, errFlag );
				InterConnectTwoPlantLoopSides( ConstCOPChiller( ChillNum ).CWLoopNum, ConstCOPChiller( ChillNum ).CWLoopSideNum, ConstCOPChiller( ChillNum ).CDLoopNum, ConstCOPChiller( ChillNum ).CDLoopSideNum, TypeOf_Chiller_ConstCOP, true );
			}

			if ( errFlag ) {
				ShowFatalError( "CalcConstCOPChillerModel: Program terminated due to previous condition(s)." );
			}
			if ( ConstCOPChiller( ChillNum ).FlowMode == ConstantFlow ) {
				// reset flow priority
				PlantLoop( ConstCOPChiller( ChillNum ).CWLoopNum ).LoopSide( ConstCOPChiller( ChillNum ).CWLoopSideNum ).Branch( ConstCOPChiller( ChillNum ).CWBranchNum ).Comp( ConstCOPChiller( ChillNum ).CWCompNum ).FlowPriority = LoopFlowStatus_NeedyIfLoopOn;
			}

			if ( ConstCOPChiller( ChillNum ).FlowMode == LeavingSetPointModulated ) {
				// reset flow priority
				PlantLoop( ConstCOPChiller( ChillNum ).CWLoopNum ).LoopSide( ConstCOPChiller( ChillNum ).CWLoopSideNum ).Branch( ConstCOPChiller( ChillNum ).CWBranchNum ).Comp( ConstCOPChiller( ChillNum ).CWCompNum ).FlowPriority = LoopFlowStatus_NeedyIfLoopOn;

				// check if setpoint on outlet node
				if ( ( Node( ConstCOPChiller( ChillNum ).EvapOutletNodeNum ).TempSetPoint == SensedNodeFlagValue ) && ( Node( ConstCOPChiller( ChillNum ).EvapOutletNodeNum ).TempSetPointHi == SensedNodeFlagValue ) ) {
					if ( ! AnyEnergyManagementSystemInModel ) {
						if ( ! ConstCOPChiller( ChillNum ).ModulatedFlowErrDone ) {
							ShowWarningError( "Missing temperature setpoint for LeavingSetpointModulated mode chiller named " + ConstCOPChiller( ChillNum ).Name );
							ShowContinueError( "  A temperature setpoint is needed at the outlet node of a chiller in variable flow mode, use a SetpointManager" );
							ShowContinueError( "  The overall loop setpoint will be assumed for chiller. The simulation continues ... " );
							ConstCOPChiller( ChillNum ).ModulatedFlowErrDone = true;
						}
					} else {
						// need call to EMS to check node
						FatalError = false; // but not really fatal yet, but should be.
						CheckIfNodeSetPointManagedByEMS( ConstCOPChiller( ChillNum ).EvapOutletNodeNum, iTemperatureSetPoint, FatalError );
						if ( FatalError ) {
							if ( ! ConstCOPChiller( ChillNum ).ModulatedFlowErrDone ) {
								ShowWarningError( "Missing temperature setpoint for LeavingSetpointModulated mode chiller named " + ConstCOPChiller( ChillNum ).Name );
								ShowContinueError( "  A temperature setpoint is needed at the outlet node of a chiller evaporator in variable flow mode" );
								ShowContinueError( "  use a Setpoint Manager to establish a setpoint at the chiller evaporator outlet node " );
								ShowContinueError( "  or use an EMS actuator to establish a setpoint at the outlet node " );
								ShowContinueError( "  The overall loop setpoint will be assumed for chiller. The simulation continues ... " );
								ConstCOPChiller( ChillNum ).ModulatedFlowErrDone = true;
							}
						}
					}
					ConstCOPChiller( ChillNum ).ModulatedFlowSetToLoop = true;
					Node( ConstCOPChiller( ChillNum ).EvapOutletNodeNum ).TempSetPoint = Node( PlantLoop( ConstCOPChiller( ChillNum ).CWLoopNum ).TempSetPointNodeNum ).TempSetPoint;
					Node( ConstCOPChiller( ChillNum ).EvapOutletNodeNum ).TempSetPointHi = Node( PlantLoop( ConstCOPChiller( ChillNum ).CWLoopNum ).TempSetPointNodeNum ).TempSetPointHi;
				}
			}
			MyFlag( ChillNum ) = false;
		}

		//Initialize critical Demand Side Variables at the beginning of each environment
		if ( MyEnvironFlag( ChillNum ) && BeginEnvrnFlag && ( PlantFirstSizesOkayToFinalize ) ) {

			rho = GetDensityGlycol( PlantLoop( ConstCOPChiller( ChillNum ).CWLoopNum ).FluidName, InitConvTemp, PlantLoop( ConstCOPChiller( ChillNum ).CWLoopNum ).FluidIndex, RoutineName );
			ConstCOPChiller( ChillNum ).EvapMassFlowRateMax = ConstCOPChiller( ChillNum ).EvapVolFlowRate * rho;
			InitComponentNodes( 0.0, ConstCOPChiller( ChillNum ).EvapMassFlowRateMax, EvapInletNode, EvapOutletNode, ConstCOPChiller( ChillNum ).CWLoopNum, ConstCOPChiller( ChillNum ).CWLoopSideNum, ConstCOPChiller( ChillNum ).CWBranchNum, ConstCOPChiller( ChillNum ).CWCompNum );

			//init maximum available condenser flow rate
			if ( ConstCOPChiller( ChillNum ).CondenserType == WaterCooled ) {

				Node( CondInletNode ).Temp = TempDesCondIn;

				rho = GetDensityGlycol( PlantLoop( ConstCOPChiller( ChillNum ).CDLoopNum ).FluidName, InitConvTemp, PlantLoop( ConstCOPChiller( ChillNum ).CDLoopNum ).FluidIndex, RoutineName );

				ConstCOPChiller( ChillNum ).CondMassFlowRateMax = rho * ConstCOPChiller( ChillNum ).CondVolFlowRate;

				InitComponentNodes( 0.0, ConstCOPChiller( ChillNum ).CondMassFlowRateMax, CondInletNode, CondOutletNode, ConstCOPChiller( ChillNum ).CDLoopNum, ConstCOPChiller( ChillNum ).CDLoopSideNum, ConstCOPChiller( ChillNum ).CDBranchNum, ConstCOPChiller( ChillNum ).CDCompNum );
			} else { // air or evap-air
				Node( CondInletNode ).MassFlowRate = ConstCOPChiller( ChillNum ).CondVolFlowRate * PsyRhoAirFnPbTdbW( StdBaroPress, TempDesCondIn, 0.0, RoutineName );

				Node( CondOutletNode ).MassFlowRate = Node( CondInletNode ).MassFlowRate;
				Node( CondInletNode ).MassFlowRateMaxAvail = Node( CondInletNode ).MassFlowRate;
				Node( CondInletNode ).MassFlowRateMax = Node( CondInletNode ).MassFlowRate;
				Node( CondOutletNode ).MassFlowRateMax = Node( CondInletNode ).MassFlowRate;
				Node( CondInletNode ).MassFlowRateMinAvail = 0.0;
				Node( CondInletNode ).MassFlowRateMin = 0.0;
				Node( CondOutletNode ).MassFlowRateMinAvail = 0.0;
				Node( CondOutletNode ).MassFlowRateMin = 0.0;
			}
			MyEnvironFlag( ChillNum ) = false;
		}

		if ( ! BeginEnvrnFlag ) {
			MyEnvironFlag( ChillNum ) = true;
		}
		if ( ( ConstCOPChiller( ChillNum ).FlowMode == LeavingSetPointModulated ) && ( ConstCOPChiller( ChillNum ).ModulatedFlowSetToLoop ) ) {
			// fix for clumsy old input that worked because loop setpoint was spread.
			//  could be removed with transition, testing , model change, period of being obsolete.
			Node( ConstCOPChiller( ChillNum ).EvapOutletNodeNum ).TempSetPoint = Node( PlantLoop( ConstCOPChiller( ChillNum ).CWLoopNum ).TempSetPointNodeNum ).TempSetPoint;
			Node( ConstCOPChiller( ChillNum ).EvapOutletNodeNum ).TempSetPointHi = Node( PlantLoop( ConstCOPChiller( ChillNum ).CWLoopNum ).TempSetPointNodeNum ).TempSetPointHi;
		}

		if ( ( MyLoad < 0.0 ) && RunFlag ) {
			mdot = ConstCOPChiller( ChillNum ).EvapMassFlowRateMax;
			mdotCond = ConstCOPChiller( ChillNum ).CondMassFlowRateMax;
		} else {
			mdot = 0.0;
			mdotCond = 0.0;
		}

		SetComponentFlowRate( mdot, EvapInletNode, EvapOutletNode, ConstCOPChiller( ChillNum ).CWLoopNum, ConstCOPChiller( ChillNum ).CWLoopSideNum, ConstCOPChiller( ChillNum ).CWBranchNum, ConstCOPChiller( ChillNum ).CWCompNum );
		if ( ConstCOPChiller( ChillNum ).CondenserType == WaterCooled ) {
			SetComponentFlowRate( mdotCond, CondInletNode, CondOutletNode, ConstCOPChiller( ChillNum ).CDLoopNum, ConstCOPChiller( ChillNum ).CDLoopSideNum, ConstCOPChiller( ChillNum ).CDBranchNum, ConstCOPChiller( ChillNum ).CDCompNum );
		}

		if ( ConstCOPChiller( ChillNum ).CondenserType == EvapCooled ) {
			BasinHeaterPower = 0.0;
		}

	}

	void
	SizeElectricChiller( int const ChillNum )
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
		tmpEvapVolFlowRate = ElectricChiller( ChillNum ).EvapVolFlowRate;
		tmpNomCap = ElectricChiller( ChillNum ).NomCap;
		tmpCondVolFlowRate = ElectricChiller( ChillNum ).CondVolFlowRate;

		if ( ElectricChiller( ChillNum ).CondenserType == WaterCooled ) {
			PltSizCondNum = PlantLoop( ElectricChiller( ChillNum ).CDLoopNum ).PlantSizNum;
		}

		PltSizNum = PlantLoop( ElectricChiller( ChillNum ).CWLoopNum ).PlantSizNum;

		if ( PltSizNum > 0 ) {
			if ( PlantSizData( PltSizNum ).DesVolFlowRate >= SmallWaterVolFlow ) {
				rho = GetDensityGlycol( PlantLoop( ElectricChiller( ChillNum ).CWLoopNum ).FluidName, InitConvTemp, PlantLoop( ElectricChiller( ChillNum ).CWLoopNum ).FluidIndex, RoutineName );
				Cp = GetSpecificHeatGlycol( PlantLoop( ElectricChiller( ChillNum ).CWLoopNum ).FluidName, InitConvTemp, PlantLoop( ElectricChiller( ChillNum ).CWLoopNum ).FluidIndex, RoutineName );
				tmpNomCap = Cp * rho * PlantSizData( PltSizNum ).DeltaT * PlantSizData( PltSizNum ).DesVolFlowRate * ElectricChiller( ChillNum ).SizFac;
				if ( ! ElectricChiller( ChillNum ).NomCapWasAutoSized ) tmpNomCap = ElectricChiller( ChillNum ).NomCap;
			} else {
				if ( ElectricChiller( ChillNum ).NomCapWasAutoSized ) tmpNomCap = 0.0;
			}
			if ( PlantFirstSizesOkayToFinalize ) {
				if ( ElectricChiller( ChillNum ).NomCapWasAutoSized ) {
					ElectricChiller( ChillNum ).NomCap = tmpNomCap;
					if ( PlantFinalSizesOkayToReport ) {
						ReportSizingOutput( "Chiller:Electric", ElectricChiller( ChillNum ).Name,
							"Design Size Nominal Capacity [W]", tmpNomCap );
					}
					if ( PlantFirstSizesOkayToReport ) {
						ReportSizingOutput( "Chiller:Electric", ElectricChiller( ChillNum ).Name,
							"Initial Design Size Nominal Capacity [W]", tmpNomCap );
					}
				} else {
					if ( ElectricChiller( ChillNum ).NomCap > 0.0 && tmpNomCap > 0.0 ) {
						NomCapUser = ElectricChiller( ChillNum ).NomCap;
						if ( PlantFinalSizesOkayToReport ) {
							ReportSizingOutput( "Chiller:Electric", ElectricChiller( ChillNum ).Name, "Design Size Nominal Capacity [W]", tmpNomCap, "User-Specified Nominal Capacity [W]", NomCapUser );
							if ( DisplayExtraWarnings ) {
								if ( ( std::abs( tmpNomCap - NomCapUser ) / NomCapUser ) > AutoVsHardSizingThreshold ) {
									ShowMessage( "SizeChillerElectric: Potential issue with equipment sizing for " + ElectricChiller( ChillNum ).Name );
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
			if ( ElectricChiller( ChillNum ).NomCapWasAutoSized && PlantFirstSizesOkayToFinalize ) {
				ShowSevereError( "Autosizing of Electric Chiller nominal capacity requires a loop Sizing:Plant object" );
				ShowContinueError( "Occurs in Electric Chiller object=" + ElectricChiller( ChillNum ).Name );
				ErrorsFound = true;
			}
			if ( ! ElectricChiller( ChillNum ).NomCapWasAutoSized && PlantFinalSizesOkayToReport
					&& ( ElectricChiller( ChillNum ).NomCap > 0.0 ) ) {
					ReportSizingOutput( "Chiller:Electric", ElectricChiller( ChillNum ).Name,
						"User-Specified Nominal Capacity [W]", ElectricChiller( ChillNum ).NomCap );
			}
		}

		if ( PltSizNum > 0 ) {
			if ( PlantSizData( PltSizNum ).DesVolFlowRate >= SmallWaterVolFlow ) {
				tmpEvapVolFlowRate = PlantSizData( PltSizNum ).DesVolFlowRate * ElectricChiller( ChillNum ).SizFac;
				if ( !  ElectricChiller( ChillNum ).EvapVolFlowRateWasAutoSized ) tmpEvapVolFlowRate = ElectricChiller( ChillNum ).EvapVolFlowRate;
			} else {
				if ( ElectricChiller( ChillNum ).EvapVolFlowRateWasAutoSized ) tmpEvapVolFlowRate = 0.0;
			}
			if ( PlantFirstSizesOkayToFinalize ) {
				if ( ElectricChiller( ChillNum ).EvapVolFlowRateWasAutoSized ) {
					ElectricChiller( ChillNum ).EvapVolFlowRate = tmpEvapVolFlowRate;
					if ( PlantFinalSizesOkayToReport) {
						ReportSizingOutput( "Chiller:Electric", ElectricChiller( ChillNum ).Name,
							"Design Size Design Chilled Water Flow Rate [m3/s]", tmpEvapVolFlowRate );
					}
					if ( PlantFirstSizesOkayToReport) {
						ReportSizingOutput( "Chiller:Electric", ElectricChiller( ChillNum ).Name,
							"Initial Design Size Design Chilled Water Flow Rate [m3/s]", tmpEvapVolFlowRate );
					}
				} else {
					if ( ElectricChiller( ChillNum ).EvapVolFlowRate > 0.0 && tmpEvapVolFlowRate > 0.0 ) {
						EvapVolFlowRateUser = ElectricChiller( ChillNum ).EvapVolFlowRate;
						if ( PlantFinalSizesOkayToReport ) {
							ReportSizingOutput( "Chiller:Electric", ElectricChiller( ChillNum ).Name,
								"Design Size Design Chilled Water Flow Rate [m3/s]", tmpEvapVolFlowRate,
								"User-Specified Design Chilled Water Flow Rate [m3/s]", EvapVolFlowRateUser );
							if ( DisplayExtraWarnings ) {
								if ( ( std::abs( tmpEvapVolFlowRate - EvapVolFlowRateUser ) / EvapVolFlowRateUser ) > AutoVsHardSizingThreshold ) {
									ShowMessage( "SizeChillerElectric: Potential issue with equipment sizing for " + ElectricChiller( ChillNum ).Name );
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
			if ( ElectricChiller( ChillNum ).EvapVolFlowRateWasAutoSized && PlantFirstSizesOkayToFinalize) {
				ShowSevereError( "Autosizing of Electric Chiller evap flow rate requires a loop Sizing:Plant object" );
				ShowContinueError( "Occurs in Electric Chiller object=" + ElectricChiller( ChillNum ).Name );
				ErrorsFound = true;
			}
			if ( ! ElectricChiller( ChillNum ).EvapVolFlowRateWasAutoSized && PlantFinalSizesOkayToReport
					&& ( ElectricChiller( ChillNum ).EvapVolFlowRate > 0.0 ) ) {
					ReportSizingOutput( "Chiller:Electric", ElectricChiller( ChillNum ).Name,
						"User-Specified Design Chilled Water Flow Rate [m3/s]", ElectricChiller( ChillNum ).EvapVolFlowRate );
			}
		}

		RegisterPlantCompDesignFlow( ElectricChiller( ChillNum ).EvapInletNodeNum, tmpEvapVolFlowRate );

		if ( PltSizCondNum > 0 && PltSizNum > 0 ) {
			if ( PlantSizData( PltSizNum ).DesVolFlowRate >= SmallWaterVolFlow && tmpNomCap > 0.0 ) {
				rho = GetDensityGlycol( PlantLoop( ElectricChiller( ChillNum ).CDLoopNum ).FluidName, ElectricChiller( ChillNum ).TempDesCondIn, PlantLoop( ElectricChiller( ChillNum ).CDLoopNum ).FluidIndex, RoutineName );

				Cp = GetSpecificHeatGlycol( PlantLoop( ElectricChiller( ChillNum ).CDLoopNum ).FluidName, ElectricChiller( ChillNum ).TempDesCondIn, PlantLoop( ElectricChiller( ChillNum ).CDLoopNum ).FluidIndex, RoutineName );
				tmpCondVolFlowRate = tmpNomCap * ( 1.0 + 1.0 / ElectricChiller( ChillNum ).COP ) / ( PlantSizData( PltSizCondNum ).DeltaT * Cp * rho );
				if ( ! ElectricChiller( ChillNum ).CondVolFlowRateWasAutoSized ) tmpCondVolFlowRate = ElectricChiller( ChillNum ).CondVolFlowRate;
			} else {
				if ( ElectricChiller( ChillNum ).CondVolFlowRateWasAutoSized ) tmpCondVolFlowRate = 0.0;
			}
			if ( PlantFirstSizesOkayToFinalize ) {
				if ( ElectricChiller( ChillNum ).CondVolFlowRateWasAutoSized ) {
					ElectricChiller( ChillNum ).CondVolFlowRate = tmpCondVolFlowRate;
					if (PlantFinalSizesOkayToReport) {
						ReportSizingOutput( "Chiller:Electric", ElectricChiller( ChillNum ).Name,
							"Design Size Design Condenser Water Flow Rate [m3/s]", tmpCondVolFlowRate );
					}
					if ( PlantFirstSizesOkayToReport ) {
						ReportSizingOutput( "Chiller:Electric", ElectricChiller( ChillNum ).Name,
							"Initial Design Size Design Condenser Water Flow Rate [m3/s]", tmpCondVolFlowRate );
					}
				} else {
					if ( ElectricChiller( ChillNum ).CondVolFlowRate > 0.0 && tmpCondVolFlowRate > 0.0 ) {
						CondVolFlowRateUser = ElectricChiller( ChillNum ).CondVolFlowRate;
						if ( PlantFinalSizesOkayToReport ) {
							ReportSizingOutput( "Chiller:Electric", ElectricChiller( ChillNum ).Name, "Design Size Design Condenser Water Flow Rate [m3/s]", tmpCondVolFlowRate, "User-Specified Design Condenser Water Flow Rate [m3/s]", CondVolFlowRateUser );
							if ( DisplayExtraWarnings ) {
								if ( ( std::abs( tmpCondVolFlowRate - CondVolFlowRateUser ) / CondVolFlowRateUser ) > AutoVsHardSizingThreshold ) {
									ShowMessage( "SizeChillerElectric: Potential issue with equipment sizing for " + ElectricChiller( ChillNum ).Name );
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
			if ( ElectricChiller( ChillNum ).CondVolFlowRateWasAutoSized && PlantFirstSizesOkayToFinalize ) {
				ShowSevereError( "Autosizing of Electric Chiller condenser flow rate requires a condenser" );
				ShowContinueError( "loop Sizing:Plant object" );
				ShowContinueError( "Occurs in Electric Chiller object=" + ElectricChiller( ChillNum ).Name );
				ErrorsFound = true;
			}
			if ( ! ElectricChiller( ChillNum ).CondVolFlowRateWasAutoSized && PlantFinalSizesOkayToReport
					&& ( ElectricChiller( ChillNum ).CondVolFlowRate > 0.0 ) ) {
					ReportSizingOutput( "Chiller:Electric", ElectricChiller( ChillNum ).Name,
						"User-Specified Design Condenser Water Flow Rate [m3/s]", ElectricChiller( ChillNum ).CondVolFlowRate );
			}
		}

		// save the design condenser water volumetric flow rate for use by the condenser water loop sizing algorithms
		if ( ElectricChiller( ChillNum ).CondenserType == WaterCooled ) {
			RegisterPlantCompDesignFlow( ElectricChiller( ChillNum ).CondInletNodeNum, tmpCondVolFlowRate );
		}
		if ( ErrorsFound ) {
			ShowFatalError( "Preceding sizing errors cause program termination" );
		}

		if ( ElectricChiller( ChillNum ).HeatRecActive ) {
			tmpHeatRecVolFlowRate = ElectricChiller( ChillNum ).CondVolFlowRate * ElectricChiller( ChillNum ).HeatRecCapacityFraction;
			if ( ! ElectricChiller( ChillNum ).DesignHeatRecVolFlowRateWasAutoSized ) tmpHeatRecVolFlowRate = ElectricChiller( ChillNum ).DesignHeatRecVolFlowRate;
			if ( PlantFirstSizesOkayToFinalize ) {
				if ( ElectricChiller( ChillNum ).DesignHeatRecVolFlowRateWasAutoSized ) {
					ElectricChiller( ChillNum ).DesignHeatRecVolFlowRate = tmpHeatRecVolFlowRate;
					if ( PlantFinalSizesOkayToReport ) {
						ReportSizingOutput( "Chiller:Electric", ElectricChiller( ChillNum ).Name,
							"Design Size Design Heat Recovery Fluid Flow Rate [m3/s]", tmpHeatRecVolFlowRate );
					}
					if ( PlantFirstSizesOkayToReport ) {
						ReportSizingOutput( "Chiller:Electric", ElectricChiller( ChillNum ).Name,
							"Initial Design Size Design Heat Recovery Fluid Flow Rate [m3/s]", tmpHeatRecVolFlowRate );
					}
				} else {
					if ( ElectricChiller( ChillNum ).DesignHeatRecVolFlowRate > 0.0 && tmpHeatRecVolFlowRate > 0.0 ) {
						DesignHeatRecVolFlowRateUser = ElectricChiller( ChillNum ).DesignHeatRecVolFlowRate;
						if ( PlantFinalSizesOkayToReport ) {
							ReportSizingOutput( "Chiller:Electric", ElectricChiller( ChillNum ).Name,
								"Design Size Design Heat Recovery Fluid Flow Rate [m3/s]", tmpHeatRecVolFlowRate,
								"User-Specified Design Heat Recovery Fluid Flow Rate [m3/s]", DesignHeatRecVolFlowRateUser );
							if ( DisplayExtraWarnings ) {
								if ( ( std::abs( tmpHeatRecVolFlowRate - DesignHeatRecVolFlowRateUser ) / DesignHeatRecVolFlowRateUser ) > AutoVsHardSizingThreshold ) {
									ShowMessage( "SizeChillerElectric: Potential issue with equipment sizing for " + ElectricChiller( ChillNum ).Name );
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
			RegisterPlantCompDesignFlow( ElectricChiller( ChillNum ).HeatRecInletNodeNum, tmpHeatRecVolFlowRate );
		}

		if ( PlantFinalSizesOkayToReport ) {
			//create predefined report
			equipName = ElectricChiller( ChillNum ).Name;
			PreDefTableEntry( pdchMechType, equipName, "Chiller:Electric" );
			PreDefTableEntry( pdchMechNomEff, equipName, ElectricChiller( ChillNum ).COP );
			PreDefTableEntry( pdchMechNomCap, equipName, ElectricChiller( ChillNum ).NomCap );
		}

	}

	void
	SizeEngineDrivenChiller( int const ChillNum )
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
		tmpNomCap = EngineDrivenChiller( ChillNum ).NomCap;
		tmpEvapVolFlowRate = EngineDrivenChiller( ChillNum ).EvapVolFlowRate;
		tmpCondVolFlowRate = EngineDrivenChiller( ChillNum ).CondVolFlowRate;
		EvapVolFlowRateUser = 0.0;
		NomCapUser = 0.0;
		CondVolFlowRateUser = 0.0;

		if ( EngineDrivenChiller( ChillNum ).CondenserType == WaterCooled ) {
			PltSizCondNum = PlantLoop( EngineDrivenChiller( ChillNum ).CDLoopNum ).PlantSizNum;
		}

		PltSizNum = PlantLoop( EngineDrivenChiller( ChillNum ).CWLoopNum ).PlantSizNum;

		if ( PltSizNum > 0 ) {
			if ( PlantSizData( PltSizNum ).DesVolFlowRate >= SmallWaterVolFlow ) {
				rho = GetDensityGlycol( PlantLoop( EngineDrivenChiller( ChillNum ).CWLoopNum ).FluidName, InitConvTemp, PlantLoop( EngineDrivenChiller( ChillNum ).CWLoopNum ).FluidIndex, RoutineName );
				Cp = GetSpecificHeatGlycol( PlantLoop( EngineDrivenChiller( ChillNum ).CWLoopNum ).FluidName, InitConvTemp, PlantLoop( EngineDrivenChiller( ChillNum ).CWLoopNum ).FluidIndex, RoutineName );
				tmpNomCap = Cp * rho * PlantSizData( PltSizNum ).DeltaT * PlantSizData( PltSizNum ).DesVolFlowRate * EngineDrivenChiller( ChillNum ).SizFac;
				if ( ! EngineDrivenChiller( ChillNum ).NomCapWasAutoSized ) tmpNomCap = EngineDrivenChiller( ChillNum ).NomCap;

			} else {
				if ( EngineDrivenChiller( ChillNum ).NomCapWasAutoSized ) tmpNomCap = 0.0;

			}
			if ( PlantFirstSizesOkayToFinalize ) {
				if ( EngineDrivenChiller( ChillNum ).NomCapWasAutoSized ) {
					EngineDrivenChiller( ChillNum ).NomCap = tmpNomCap;
					if (PlantFinalSizesOkayToReport ) {
						ReportSizingOutput( "Chiller:EngineDriven", EngineDrivenChiller( ChillNum ).Name,
							"Design Size Nominal Capacity [W]", tmpNomCap );
					}
					if (PlantFirstSizesOkayToReport ) {
						ReportSizingOutput( "Chiller:EngineDriven", EngineDrivenChiller( ChillNum ).Name,
							"Initial Design Size Nominal Capacity [W]", tmpNomCap );
					}
				} else {
					if ( EngineDrivenChiller( ChillNum ).NomCap > 0.0 && tmpNomCap > 0.0 ) {
						NomCapUser = EngineDrivenChiller( ChillNum ).NomCap;
						if ( PlantFinalSizesOkayToReport ) {
							ReportSizingOutput( "Chiller:EngineDriven", EngineDrivenChiller( ChillNum ).Name, "Design Size Nominal Capacity [W]", tmpNomCap, "User-Specified Nominal Capacity [W]", NomCapUser );
							if ( DisplayExtraWarnings ) {
								if ( ( std::abs( tmpNomCap - NomCapUser ) / NomCapUser ) > AutoVsHardSizingThreshold ) {
									ShowMessage( "SizeChillerEngineDriven: Potential issue with equipment sizing for " + EngineDrivenChiller( ChillNum ).Name );
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
			if ( EngineDrivenChiller( ChillNum ).NomCapWasAutoSized && PlantFirstSizesOkayToFinalize ) {
				ShowSevereError( "Autosizing of Engine Driven Chiller nominal capacity requires a loop Sizing:Plant object" );
				ShowContinueError( "Occurs in Engine Driven Chiller object=" + EngineDrivenChiller( ChillNum ).Name );
				ErrorsFound = true;
			}
			if ( ! EngineDrivenChiller( ChillNum ).NomCapWasAutoSized && PlantFinalSizesOkayToReport
					&& ( EngineDrivenChiller( ChillNum ).NomCap > 0.0 ) ) {
					ReportSizingOutput( "Chiller:EngineDriven", EngineDrivenChiller( ChillNum ).Name,
						"User-Specified Nominal Capacity [W]", EngineDrivenChiller( ChillNum ).NomCap );
			}
		}

		if ( PltSizNum > 0 ) {
			if ( PlantSizData( PltSizNum ).DesVolFlowRate >= SmallWaterVolFlow ) {
				tmpEvapVolFlowRate = PlantSizData( PltSizNum ).DesVolFlowRate * EngineDrivenChiller( ChillNum ).SizFac;
				if ( ! EngineDrivenChiller( ChillNum ).EvapVolFlowRateWasAutoSized ) tmpEvapVolFlowRate = EngineDrivenChiller( ChillNum ).EvapVolFlowRate;

			} else {
				if ( EngineDrivenChiller( ChillNum ).EvapVolFlowRateWasAutoSized ) tmpEvapVolFlowRate = 0.0;

			}
			if ( PlantFirstSizesOkayToFinalize ) {
				if ( EngineDrivenChiller( ChillNum ).EvapVolFlowRateWasAutoSized ) {
					EngineDrivenChiller( ChillNum ).EvapVolFlowRate = tmpEvapVolFlowRate;
					if ( PlantFinalSizesOkayToReport ) {
						ReportSizingOutput( "Chiller:EngineDriven", EngineDrivenChiller( ChillNum ).Name,
							"Design Chilled Water Flow Rate [m3/s]", tmpEvapVolFlowRate );
					}
					if ( PlantFirstSizesOkayToReport ) {
						ReportSizingOutput( "Chiller:EngineDriven", EngineDrivenChiller( ChillNum ).Name,
							"Initial Design Chilled Water Flow Rate [m3/s]", tmpEvapVolFlowRate );
					}
				} else {
					if ( EngineDrivenChiller( ChillNum ).EvapVolFlowRate > 0.0 && tmpEvapVolFlowRate > 0.0 ) {
						EvapVolFlowRateUser = EngineDrivenChiller( ChillNum ).EvapVolFlowRate;
						if ( PlantFinalSizesOkayToReport) {
							ReportSizingOutput( "Chiller:EngineDriven", EngineDrivenChiller( ChillNum ).Name,
								"Design Size Design Chilled Water Flow Rate [m3/s]", tmpEvapVolFlowRate,
								"User-Specified Design Chilled Water Flow Rate [m3/s]", EvapVolFlowRateUser );
							if ( DisplayExtraWarnings ) {
								if ( ( std::abs( tmpEvapVolFlowRate - EvapVolFlowRateUser ) / EvapVolFlowRateUser ) > AutoVsHardSizingThreshold ) {
									ShowMessage( "SizeChillerEngineDriven: Potential issue with equipment sizing for " + EngineDrivenChiller( ChillNum ).Name );
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
			if ( EngineDrivenChiller( ChillNum ).EvapVolFlowRateWasAutoSized && PlantFirstSizesOkayToFinalize ) {
				ShowSevereError( "Autosizing of Engine Driven Chiller evap flow rate requires a loop Sizing:Plant object" );
				ShowContinueError( "Occurs in Engine Driven Chiller object=" + EngineDrivenChiller( ChillNum ).Name );
				ErrorsFound = true;
			}
			if ( ! EngineDrivenChiller( ChillNum ).EvapVolFlowRateWasAutoSized && PlantFinalSizesOkayToReport
					&& ( EngineDrivenChiller( ChillNum ).EvapVolFlowRate > 0.0 ) ) {
						ReportSizingOutput( "Chiller:EngineDriven", EngineDrivenChiller( ChillNum ).Name,
							"User-Specified Design Chilled Water Flow Rate [m3/s]", EngineDrivenChiller( ChillNum ).EvapVolFlowRate );

			}
		}

		RegisterPlantCompDesignFlow( EngineDrivenChiller( ChillNum ).EvapInletNodeNum, tmpEvapVolFlowRate );

		if ( PltSizCondNum > 0 && PltSizNum > 0 ) {
			if ( PlantSizData( PltSizNum ).DesVolFlowRate >= SmallWaterVolFlow && tmpNomCap > 0.0 ) {
				rho = GetDensityGlycol( PlantLoop( EngineDrivenChiller( ChillNum ).CDLoopNum ).FluidName, EngineDrivenChiller( ChillNum ).TempDesCondIn, PlantLoop( EngineDrivenChiller( ChillNum ).CDLoopNum ).FluidIndex, RoutineName );

				Cp = GetSpecificHeatGlycol( PlantLoop( EngineDrivenChiller( ChillNum ).CDLoopNum ).FluidName, EngineDrivenChiller( ChillNum ).TempDesCondIn, PlantLoop( EngineDrivenChiller( ChillNum ).CDLoopNum ).FluidIndex, RoutineName );
				tmpCondVolFlowRate = tmpNomCap * ( 1.0 + 1.0 / EngineDrivenChiller( ChillNum ).COP ) / ( PlantSizData( PltSizCondNum ).DeltaT * Cp * rho );
				if ( ! EngineDrivenChiller( ChillNum ).CondVolFlowRateWasAutoSized ) tmpCondVolFlowRate = EngineDrivenChiller( ChillNum ).CondVolFlowRate;

			} else {
				if ( EngineDrivenChiller( ChillNum ).CondVolFlowRateWasAutoSized ) tmpCondVolFlowRate = 0.0;
			}
			if ( PlantFirstSizesOkayToFinalize ) {
				if ( EngineDrivenChiller( ChillNum ).CondVolFlowRateWasAutoSized ) {
					EngineDrivenChiller( ChillNum ).CondVolFlowRate = tmpCondVolFlowRate;
					if ( PlantFinalSizesOkayToReport ) {
						ReportSizingOutput( "Chiller:EngineDriven", EngineDrivenChiller( ChillNum ).Name,
							"Design Size Design Condenser Water Flow Rate [m3/s]", tmpCondVolFlowRate );
					}
					if ( PlantFirstSizesOkayToReport ) {
						ReportSizingOutput( "Chiller:EngineDriven", EngineDrivenChiller( ChillNum ).Name,
							"Initial Design Size Design Condenser Water Flow Rate [m3/s]", tmpCondVolFlowRate );
					}
				} else {
					if ( EngineDrivenChiller( ChillNum ).CondVolFlowRate > 0.0 && tmpCondVolFlowRate > 0.0 ) {
						CondVolFlowRateUser = EngineDrivenChiller( ChillNum ).CondVolFlowRate;
						if ( PlantFinalSizesOkayToReport ) {
							ReportSizingOutput( "Chiller:EngineDriven", EngineDrivenChiller( ChillNum ).Name,
								"Design Size Design Condenser Water Flow Rate [m3/s]", tmpCondVolFlowRate,
								"User-Specified Design Condenser Water Flow Rate [m3/s]", CondVolFlowRateUser );
							if ( DisplayExtraWarnings ) {
								if ( ( std::abs( tmpCondVolFlowRate - CondVolFlowRateUser ) / CondVolFlowRateUser ) > AutoVsHardSizingThreshold ) {
									ShowMessage( "SizeChillerEngineDriven: Potential issue with equipment sizing for " + EngineDrivenChiller( ChillNum ).Name );
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
			if ( EngineDrivenChiller( ChillNum ).CondVolFlowRateWasAutoSized && PlantFirstSizesOkayToFinalize ) {
				ShowSevereError( "Autosizing of EngineDriven Chiller condenser flow rate requires a condenser" );
				ShowContinueError( "loop Sizing:Plant object" );
				ShowContinueError( "Occurs in EngineDriven Chiller object=" + EngineDrivenChiller( ChillNum ).Name );
				ErrorsFound = true;

			}
			if ( ! EngineDrivenChiller( ChillNum ).CondVolFlowRateWasAutoSized && PlantFinalSizesOkayToReport
					&& ( EngineDrivenChiller( ChillNum ).CondVolFlowRate > 0.0 ) ) {
					ReportSizingOutput( "Chiller:EngineDriven", EngineDrivenChiller( ChillNum ).Name,
						"User-Specified Design Condenser Water Flow Rate [m3/s]", EngineDrivenChiller( ChillNum ).CondVolFlowRate );
			}
		}

		// save the design condenser water volumetric flow rate for use by the condenser water loop sizing algorithms
		if ( EngineDrivenChiller( ChillNum ).CondenserType == WaterCooled ) {
			RegisterPlantCompDesignFlow( EngineDrivenChiller( ChillNum ).CondInletNodeNum, tmpCondVolFlowRate );
		}

		if ( PlantFinalSizesOkayToReport ) {
			//create predefined report
			equipName = EngineDrivenChiller( ChillNum ).Name;
			PreDefTableEntry( pdchMechType, equipName, "Chiller:EngineDriven" );
			PreDefTableEntry( pdchMechNomEff, equipName, EngineDrivenChiller( ChillNum ).COP );
			PreDefTableEntry( pdchMechNomCap, equipName, EngineDrivenChiller( ChillNum ).NomCap );
		}

		if ( ErrorsFound ) {
			ShowFatalError( "Preceding sizing errors cause program termination" );
		}

	}

	void
	SizeGTChiller( int const ChillNum )
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
		tmpNomCap = GTChiller( ChillNum ).NomCap;
		tmpEvapVolFlowRate = GTChiller( ChillNum ).EvapVolFlowRate;
		tmpCondVolFlowRate = GTChiller( ChillNum ).CondVolFlowRate;
		EvapVolFlowRateUser = 0.0;
		NomCapUser = 0.0;
		CondVolFlowRateUser = 0.0;
		GTEngineCapacityDes = 0.0;
		GTEngineCapacityUser = 0.0;

		if ( GTChiller( ChillNum ).CondenserType == WaterCooled ) {
			//if ( GTChiller( ChillNum ).CondVolFlowRate == AutoSize ) {
			PltSizCondNum = PlantLoop( GTChiller( ChillNum ).CDLoopNum ).PlantSizNum;
			//}
		}

		PltSizNum = PlantLoop( GTChiller( ChillNum ).CWLoopNum ).PlantSizNum;

		if ( PltSizNum > 0 ) {
			if ( PlantSizData( PltSizNum ).DesVolFlowRate >= SmallWaterVolFlow ) {
				rho = GetDensityGlycol( PlantLoop( GTChiller( ChillNum ).CWLoopNum ).FluidName, InitConvTemp, PlantLoop( GTChiller( ChillNum ).CWLoopNum ).FluidIndex, RoutineName );
				Cp = GetSpecificHeatGlycol( PlantLoop( GTChiller( ChillNum ).CWLoopNum ).FluidName, InitConvTemp, PlantLoop( GTChiller( ChillNum ).CWLoopNum ).FluidIndex, RoutineName );
				tmpNomCap = Cp * rho * PlantSizData( PltSizNum ).DeltaT * PlantSizData( PltSizNum ).DesVolFlowRate * GTChiller( ChillNum ).SizFac;
				if ( ! GTChiller( ChillNum ).NomCapWasAutoSized ) tmpNomCap = GTChiller( ChillNum ).NomCap;
				//IF (PlantFirstSizesOkayToFinalize)  GTChiller(ChillNum)%Base%NomCap = tmpNomCap
			} else {
				if ( GTChiller( ChillNum ).NomCapWasAutoSized ) tmpNomCap = 0.0;
				//IF (PlantFirstSizesOkayToFinalize) GTChiller(ChillNum)%Base%NomCap = tmpNomCap
			}
			if ( PlantFirstSizesOkayToFinalize ) {
				if ( GTChiller( ChillNum ).NomCapWasAutoSized ) {
					GTChiller( ChillNum ).NomCap = tmpNomCap;
					if ( PlantFinalSizesOkayToReport ) {
						ReportSizingOutput( "Chiller:CombustionTurbine", GTChiller( ChillNum ).Name,
							"Design Size Nominal Capacity [W]", tmpNomCap );
					}
					if ( PlantFirstSizesOkayToReport ) {
						ReportSizingOutput( "Chiller:CombustionTurbine", GTChiller( ChillNum ).Name,
							"Initial Design Size Nominal Capacity [W]", tmpNomCap );
					}
				} else {
					if ( GTChiller( ChillNum ).NomCap > 0.0 && tmpNomCap > 0.0 ) {
						NomCapUser = GTChiller( ChillNum ).NomCap;
						if ( PlantFinalSizesOkayToReport ) {
							ReportSizingOutput( "Chiller:CombustionTurbine", GTChiller( ChillNum ).Name, "Design Size Nominal Capacity [W]", tmpNomCap, "User-Specified Nominal Capacity [W]", NomCapUser );
							if ( DisplayExtraWarnings ) {
								if ( ( std::abs( tmpNomCap - NomCapUser ) / NomCapUser ) > AutoVsHardSizingThreshold ) {
									ShowMessage( "SizeChillerElectricEIR: Potential issue with equipment sizing for " + GTChiller( ChillNum ).Name );
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
			if ( GTChiller( ChillNum ).NomCapWasAutoSized && PlantFirstSizesOkayToFinalize ) {
				ShowSevereError( "Autosizing of Gas Turbine Chiller nominal capacity requires a loop Sizing:Plant object" );
				ShowContinueError( "Occurs in Gas Turbine Chiller object=" + GTChiller( ChillNum ).Name );
				ErrorsFound = true;
			}
			if ( ! GTChiller( ChillNum ).NomCapWasAutoSized && PlantFinalSizesOkayToReport
					&& ( GTChiller( ChillNum ).NomCap > 0.0 ) ) {
					ReportSizingOutput( "Chiller:CombustionTurbine", GTChiller( ChillNum ).Name,
						"User-Specified Design Size Nominal Capacity [W]", GTChiller( ChillNum ).NomCap );
			}
		}

		if ( PltSizNum > 0 ) {
			if ( PlantSizData( PltSizNum ).DesVolFlowRate >= SmallWaterVolFlow ) {
				tmpEvapVolFlowRate = PlantSizData( PltSizNum ).DesVolFlowRate * GTChiller( ChillNum ).SizFac;
				if ( ! GTChiller( ChillNum ).EvapVolFlowRateWasAutoSized ) tmpEvapVolFlowRate = GTChiller( ChillNum ).EvapVolFlowRate;

			} else {
				if ( GTChiller( ChillNum ).EvapVolFlowRateWasAutoSized ) tmpEvapVolFlowRate = 0.0;

			}
			if ( PlantFirstSizesOkayToFinalize ) {
				if ( GTChiller( ChillNum ).EvapVolFlowRateWasAutoSized ) {
					GTChiller( ChillNum ).EvapVolFlowRate = tmpEvapVolFlowRate;
					if ( PlantFinalSizesOkayToReport ) {
						ReportSizingOutput( "Chiller:CombustionTurbine", GTChiller( ChillNum ).Name,
							"Design size Design Chilled Water Flow Rate [m3/s]", tmpEvapVolFlowRate );
					}
					if ( PlantFirstSizesOkayToReport ) {
						ReportSizingOutput( "Chiller:CombustionTurbine", GTChiller( ChillNum ).Name,
							"Initial Design size Design Chilled Water Flow Rate [m3/s]", tmpEvapVolFlowRate );
					}
				} else {
					if ( GTChiller( ChillNum ).EvapVolFlowRate > 0.0 && tmpEvapVolFlowRate > 0.0 ) {
						EvapVolFlowRateUser = GTChiller( ChillNum ).EvapVolFlowRate;
						if ( PlantFinalSizesOkayToReport ) {
							ReportSizingOutput( "Chiller:CombustionTurbine", GTChiller( ChillNum ).Name, "Design size Design Chilled Water Flow Rate [m3/s]", tmpEvapVolFlowRate, "User-Specified Design Chilled Water Flow Rate [m3/s]", EvapVolFlowRateUser );
							if ( DisplayExtraWarnings ) {
								if ( ( std::abs( tmpEvapVolFlowRate - EvapVolFlowRateUser ) / EvapVolFlowRateUser ) > AutoVsHardSizingThreshold ) {
									ShowMessage( "SizeChillerElectricEIR: Potential issue with equipment sizing for " + GTChiller( ChillNum ).Name );
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
			if ( GTChiller( ChillNum ).EvapVolFlowRateWasAutoSized && PlantFirstSizesOkayToFinalize ) {
				ShowSevereError( "Autosizing of Gas Turbine Chiller evap flow rate requires a loop Sizing:Plant object" );
				ShowContinueError( "Occurs in Gas Turbine Chiller object=" + GTChiller( ChillNum ).Name );
				ErrorsFound = true;
			}
			if ( ! GTChiller( ChillNum ).EvapVolFlowRateWasAutoSized && PlantFinalSizesOkayToReport
					&& ( GTChiller( ChillNum ).EvapVolFlowRate > 0.0 ) ) {
					ReportSizingOutput( "Chiller:CombustionTurbine", GTChiller( ChillNum ).Name,
						"User-Specified Design Chilled Water Flow Rate [m3/s]", GTChiller( ChillNum ).EvapVolFlowRate );
			}
		}

		RegisterPlantCompDesignFlow( GTChiller( ChillNum ).EvapInletNodeNum, tmpEvapVolFlowRate );

		if ( PltSizCondNum > 0 && PltSizNum > 0 ) {
			if ( PlantSizData( PltSizNum ).DesVolFlowRate >= SmallWaterVolFlow && tmpNomCap > 0.0 ) {
				rho = GetDensityGlycol( PlantLoop( GTChiller( ChillNum ).CDLoopNum ).FluidName, GTChiller( ChillNum ).TempDesCondIn, PlantLoop( GTChiller( ChillNum ).CDLoopNum ).FluidIndex, RoutineName );

				Cp = GetSpecificHeatGlycol( PlantLoop( GTChiller( ChillNum ).CDLoopNum ).FluidName, GTChiller( ChillNum ).TempDesCondIn, PlantLoop( GTChiller( ChillNum ).CDLoopNum ).FluidIndex, RoutineName );
				tmpCondVolFlowRate = tmpNomCap * ( 1.0 + 1.0 / GTChiller( ChillNum ).COP ) / ( PlantSizData( PltSizCondNum ).DeltaT * Cp * rho );
				if ( ! GTChiller( ChillNum ).CondVolFlowRateWasAutoSized ) tmpCondVolFlowRate = GTChiller( ChillNum ).CondVolFlowRate;
			} else {
				if ( GTChiller( ChillNum ).CondVolFlowRateWasAutoSized ) tmpCondVolFlowRate = 0.0;
			}
			if ( PlantFirstSizesOkayToFinalize ) {
				if ( GTChiller( ChillNum ).CondVolFlowRateWasAutoSized ) {
					GTChiller( ChillNum ).CondVolFlowRate = tmpCondVolFlowRate;
					if ( PlantFinalSizesOkayToReport ) {
						ReportSizingOutput( "Chiller:CombustionTurbine", GTChiller( ChillNum ).Name,
							"Design Size Design Condenser Water Flow Rate [m3/s]", tmpCondVolFlowRate );
					}
					if ( PlantFirstSizesOkayToReport ) {
						ReportSizingOutput( "Chiller:CombustionTurbine", GTChiller( ChillNum ).Name,
							"Initial Design Size Design Condenser Water Flow Rate [m3/s]", tmpCondVolFlowRate );
					}
				} else {
					if ( GTChiller( ChillNum ).CondVolFlowRate > 0.0 && tmpCondVolFlowRate > 0.0 ) {
						CondVolFlowRateUser = GTChiller( ChillNum ).CondVolFlowRate;
						if ( PlantFinalSizesOkayToReport ) {
							ReportSizingOutput( "Chiller:CombustionTurbine", GTChiller( ChillNum ).Name, "Design Size Design Condenser Water Flow Rate [m3/s]", tmpCondVolFlowRate, "User-Specified Design Condenser Water Flow Rate [m3/s]", CondVolFlowRateUser );
							if ( DisplayExtraWarnings ) {
								if ( ( std::abs( tmpCondVolFlowRate - CondVolFlowRateUser ) / CondVolFlowRateUser ) > AutoVsHardSizingThreshold ) {
									ShowMessage( "SizeChillerElectricEIR: Potential issue with equipment sizing for " + GTChiller( ChillNum ).Name );
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
			if ( GTChiller( ChillNum ).CondVolFlowRateWasAutoSized && PlantFirstSizesOkayToFinalize ) {
				ShowSevereError( "Autosizing of Gas Turbine Chiller condenser flow rate requires a condenser" );
				ShowContinueError( "loop Sizing:Plant object" );
				ShowContinueError( "Occurs in Gas Turbine Chiller object=" + GTChiller( ChillNum ).Name );
				ErrorsFound = true;
			}
			if ( ! GTChiller( ChillNum ).CondVolFlowRateWasAutoSized  && PlantFinalSizesOkayToReport
					&& ( GTChiller( ChillNum ).CondVolFlowRate > 0.0 ) ) {
					ReportSizingOutput( "Chiller:Electric", GTChiller( ChillNum ).Name,
						"User-Specified Design Condenser Water Flow Rate [m3/s]", GTChiller( ChillNum ).CondVolFlowRate );

			}
		}
		// save the design condenser water volumetric flow rate for use by the condenser water loop sizing algorithms
		if ( GTChiller( ChillNum ).CondenserType == WaterCooled ) RegisterPlantCompDesignFlow( GTChiller( ChillNum ).CondInletNodeNum, tmpCondVolFlowRate );


		GTEngineCapacityDes = GTChiller( ChillNum ).NomCap * EngineEff / GTChiller( ChillNum ).COP;
		if ( PlantFirstSizesOkayToFinalize ) {
			if ( GTChiller( ChillNum ).GTEngineCapacityWasAutoSized ) {
				GTChiller( ChillNum ).GTEngineCapacity = GTEngineCapacityDes;
				if ( PlantFinalSizesOkayToReport ) {
					ReportSizingOutput( "Chiller:CombustionTurbine", GTChiller( ChillNum ).Name,
						"Design Size Gas Turbine Engine Capacity [W]", GTEngineCapacityDes );
				}
				if ( PlantFirstSizesOkayToReport ) {
					ReportSizingOutput( "Chiller:CombustionTurbine", GTChiller( ChillNum ).Name,
						"Initial Design Size Gas Turbine Engine Capacity [W]", GTEngineCapacityDes );
				}
			} else {
				if ( GTChiller( ChillNum ).GTEngineCapacity > 0.0 && GTEngineCapacityDes > 0.0 ) {
					GTEngineCapacityUser = GTChiller( ChillNum ).GTEngineCapacity;
					if ( PlantFinalSizesOkayToReport ) {
						ReportSizingOutput( "Chiller:CombustionTurbine", GTChiller( ChillNum ).Name, "Design Size Gas Turbine Engine Capacity [W]", GTEngineCapacityDes, "User-Specified Gas Turbine Engine Capacity [W]", GTEngineCapacityUser );
					}
					if ( DisplayExtraWarnings ) {
						if ( ( std::abs( GTEngineCapacityDes - GTEngineCapacityUser ) / GTEngineCapacityUser ) > AutoVsHardSizingThreshold ) {
							ShowMessage( "SizeChillerElectricEIR: Potential issue with equipment sizing for " + GTChiller( ChillNum ).Name );
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
			equipName = GTChiller( ChillNum ).Name;
			PreDefTableEntry( pdchMechType, equipName, "Chiller:CombustionTurbine" );
			PreDefTableEntry( pdchMechNomEff, equipName, GTChiller( ChillNum ).COP );
			PreDefTableEntry( pdchMechNomCap, equipName, GTChiller( ChillNum ).NomCap );
		}

		if ( ErrorsFound ) {
			ShowFatalError( "Preceding sizing errors cause program termination" );
		}

	}

	void
	SizeConstCOPChiller( int const ChillNum )
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
		tmpNomCap = ConstCOPChiller( ChillNum ).NomCap;
		tmpEvapVolFlowRate = ConstCOPChiller( ChillNum ).EvapVolFlowRate;
		tmpCondVolFlowRate = ConstCOPChiller( ChillNum ).CondVolFlowRate;

		EvapVolFlowRateUser = 0.0;
		NomCapUser = 0.0;
		CondVolFlowRateUser = 0.0;

		if ( ConstCOPChiller( ChillNum ).CondenserType == WaterCooled ) {
			//IF (ConstCOPChiller(ChillNum)%Base%CondVolFlowRate == AutoSize) THEN
			PltSizCondNum = PlantLoop( ConstCOPChiller( ChillNum ).CDLoopNum ).PlantSizNum;
			//END IF
		}

		PltSizNum = PlantLoop( ConstCOPChiller( ChillNum ).CWLoopNum ).PlantSizNum;

		if ( PltSizNum > 0 ) {
			if ( PlantSizData( PltSizNum ).DesVolFlowRate >= SmallWaterVolFlow ) {
				rho = GetDensityGlycol( PlantLoop( ConstCOPChiller( ChillNum ).CWLoopNum ).FluidName, InitConvTemp, PlantLoop( ConstCOPChiller( ChillNum ).CWLoopNum ).FluidIndex, RoutineName );
				Cp = GetSpecificHeatGlycol( PlantLoop( ConstCOPChiller( ChillNum ).CWLoopNum ).FluidName, InitConvTemp, PlantLoop( ConstCOPChiller( ChillNum ).CWLoopNum ).FluidIndex, RoutineName );
				tmpNomCap = Cp * rho * PlantSizData( PltSizNum ).DeltaT * PlantSizData( PltSizNum ).DesVolFlowRate * ConstCOPChiller( ChillNum ).SizFac;
				if ( ! ConstCOPChiller( ChillNum ).NomCapWasAutoSized ) tmpNomCap = ConstCOPChiller( ChillNum ).NomCap;
			} else {
				if ( ConstCOPChiller( ChillNum ).NomCapWasAutoSized ) tmpNomCap = 0.0;
			}
			if ( PlantFirstSizesOkayToFinalize ) {
				if ( ConstCOPChiller( ChillNum ).NomCapWasAutoSized ) {
					ConstCOPChiller( ChillNum ).NomCap = tmpNomCap;
					if ( PlantFinalSizesOkayToReport ) {
						ReportSizingOutput( "Chiller:ConstantCOP", ConstCOPChiller( ChillNum ).Name,
							"Design Size Nominal Capacity [W]", tmpNomCap );
					}
					if ( PlantFirstSizesOkayToReport ) {
						ReportSizingOutput( "Chiller:ConstantCOP", ConstCOPChiller( ChillNum ).Name,
							"Initial Design Size Nominal Capacity [W]", tmpNomCap );
					}
				} else { // Hard-size with sizing data
					if ( ConstCOPChiller( ChillNum ).NomCap > 0.0 && tmpNomCap > 0.0 ) {
						NomCapUser = ConstCOPChiller( ChillNum ).NomCap;
						if ( PlantFinalSizesOkayToReport ) {
							ReportSizingOutput( "Chiller:ConstantCOP", ConstCOPChiller( ChillNum ).Name, "Design Size Nominal Capacity [W]", tmpNomCap, "User-Specified Nominal Capacity [W]", NomCapUser );
							if ( DisplayExtraWarnings ) {
								if ( ( std::abs( tmpNomCap - NomCapUser ) / NomCapUser ) > AutoVsHardSizingThreshold ) {
									ShowMessage( "SizeChillerConstantCOP: Potential issue with equipment sizing for " + ConstCOPChiller( ChillNum ).Name );
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
			if ( ConstCOPChiller( ChillNum ).NomCapWasAutoSized && PlantFirstSizesOkayToFinalize ) {
				ShowSevereError( "Autosizing of Constant COP Chiller nominal capacity requires a loop Sizing:Plant object" );
				ShowContinueError( "Occurs in Chiller:ConstantCOP object=" + ConstCOPChiller( ChillNum ).Name );
				ErrorsFound = true;
			}
			if ( ! ConstCOPChiller( ChillNum ).NomCapWasAutoSized && PlantFinalSizesOkayToReport
					&& ( ConstCOPChiller( ChillNum ).NomCap > 0.0 ) ) {
					ReportSizingOutput( "Chiller:ConstantCOP", ConstCOPChiller( ChillNum ).Name,
						"User-Specified Nominal Capacity [W]", ConstCOPChiller( ChillNum ).NomCap );
			}
		}

		if ( PltSizNum > 0 ) {
			if ( PlantSizData( PltSizNum ).DesVolFlowRate >= SmallWaterVolFlow ) {
				tmpEvapVolFlowRate = PlantSizData( PltSizNum ).DesVolFlowRate * ConstCOPChiller( ChillNum ).SizFac;
				if ( ! ConstCOPChiller( ChillNum ).EvapVolFlowRateWasAutoSized ) tmpEvapVolFlowRate = ConstCOPChiller( ChillNum ).EvapVolFlowRate;
			} else {
				if ( ConstCOPChiller( ChillNum ).EvapVolFlowRateWasAutoSized ) tmpEvapVolFlowRate = 0.0;
			}
			if ( PlantFirstSizesOkayToFinalize ) {
				if ( ConstCOPChiller( ChillNum ).EvapVolFlowRateWasAutoSized ) {
					ConstCOPChiller( ChillNum ).EvapVolFlowRate = tmpEvapVolFlowRate;
					if ( PlantFinalSizesOkayToReport ) {
						ReportSizingOutput( "Chiller:ConstantCOP", ConstCOPChiller( ChillNum ).Name,
							"Design Size Design Chilled Water Flow Rate [m3/s]", tmpEvapVolFlowRate );
					}
					if ( PlantFirstSizesOkayToReport ) {
						ReportSizingOutput( "Chiller:ConstantCOP", ConstCOPChiller( ChillNum ).Name,
							"Initial Design Size Design Chilled Water Flow Rate [m3/s]", tmpEvapVolFlowRate );
					}
				} else {
					if ( ConstCOPChiller( ChillNum ).EvapVolFlowRate > 0.0 && tmpEvapVolFlowRate > 0.0 ) {
						EvapVolFlowRateUser = ConstCOPChiller( ChillNum ).EvapVolFlowRate;
						if ( PlantFinalSizesOkayToReport ) {
							ReportSizingOutput( "Chiller:ConstantCOP", ConstCOPChiller( ChillNum ).Name, "Design Size Design Chilled Water Flow Rate [m3/s]", tmpEvapVolFlowRate, "User-Specified Design Chilled Water Flow Rate [m3/s]", EvapVolFlowRateUser );
							if ( DisplayExtraWarnings ) {
								if ( ( std::abs( tmpEvapVolFlowRate - EvapVolFlowRateUser ) / EvapVolFlowRateUser ) > AutoVsHardSizingThreshold ) {
									ShowMessage( "SizeChillerConstantCOP: Potential issue with equipment sizing for " + ConstCOPChiller( ChillNum ).Name );
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
			if ( ConstCOPChiller( ChillNum ).EvapVolFlowRateWasAutoSized && PlantFirstSizesOkayToFinalize ) {
				ShowSevereError( "Autosizing of Constant COP Chiller evap flow rate requires a loop Sizing:Plant object" );
				ShowContinueError( "Occurs in Chiller:ConstantCOP object=" + ConstCOPChiller( ChillNum ).Name );
				ErrorsFound = true;
			}
			if ( ! ConstCOPChiller( ChillNum ).EvapVolFlowRateWasAutoSized && PlantFinalSizesOkayToReport
					&& ( ConstCOPChiller( ChillNum ).EvapVolFlowRate > 0.0 ) ) {
					ReportSizingOutput( "Chiller:ConstantCOP", ConstCOPChiller( ChillNum ).Name,
						"User-Specified Design Chilled Water Flow Rate [m3/s]", ConstCOPChiller( ChillNum ).EvapVolFlowRate );
			}
		}

		RegisterPlantCompDesignFlow( ConstCOPChiller( ChillNum ).EvapInletNodeNum, tmpEvapVolFlowRate );

		if ( ConstCOPChiller( ChillNum ).CondenserType == WaterCooled ) {
			if ( PltSizCondNum > 0 && PltSizNum > 0 ) {
				if ( PlantSizData( PltSizNum ).DesVolFlowRate >= SmallWaterVolFlow && tmpNomCap > 0.0 ) {
					rho = GetDensityGlycol( PlantLoop( ConstCOPChiller( ChillNum ).CDLoopNum ).FluidName, 29.44, PlantLoop( ConstCOPChiller( ChillNum ).CDLoopNum ).FluidIndex, RoutineName );

					Cp = GetSpecificHeatGlycol( PlantLoop( ConstCOPChiller( ChillNum ).CDLoopNum ).FluidName, 29.44, PlantLoop( ConstCOPChiller( ChillNum ).CDLoopNum ).FluidIndex, RoutineName );
					tmpCondVolFlowRate = tmpNomCap * ( 1.0 + 1.0 / ConstCOPChiller( ChillNum ).COP ) / ( PlantSizData( PltSizCondNum ).DeltaT * Cp * rho );
					if ( ! ConstCOPChiller( ChillNum ).CondVolFlowRateWasAutoSized ) tmpCondVolFlowRate = ConstCOPChiller( ChillNum ).CondVolFlowRate;
				} else {
					if ( ConstCOPChiller( ChillNum ).CondVolFlowRateWasAutoSized ) tmpCondVolFlowRate = 0.0;
				}
				if ( PlantFirstSizesOkayToFinalize ) {
					if ( ConstCOPChiller( ChillNum ).CondVolFlowRateWasAutoSized ) {
						ConstCOPChiller( ChillNum ).CondVolFlowRate = tmpCondVolFlowRate;
						if ( PlantFinalSizesOkayToReport ) {
							ReportSizingOutput( "Chiller:ConstantCOP", ConstCOPChiller( ChillNum ).Name,
								"Design Size Design Condenser Water Flow Rate [m3/s]", tmpCondVolFlowRate );
						}
						if ( PlantFirstSizesOkayToReport ) {
							ReportSizingOutput( "Chiller:ConstantCOP", ConstCOPChiller( ChillNum ).Name,
								"Initial Design Size Design Condenser Water Flow Rate [m3/s]", tmpCondVolFlowRate );
						}
					} else {
						if ( ConstCOPChiller( ChillNum ).CondVolFlowRate > 0.0 && tmpCondVolFlowRate > 0.0 ) {
							CondVolFlowRateUser = ConstCOPChiller( ChillNum ).CondVolFlowRate;
							if ( PlantFinalSizesOkayToReport ) {
								ReportSizingOutput( "Chiller:ConstantCOP", ConstCOPChiller( ChillNum ).Name, "Design Size Design Condenser Water Flow Rate [m3/s]", tmpCondVolFlowRate, "User-Specified Design Condenser Water Flow Rate [m3/s]", CondVolFlowRateUser );
								if ( DisplayExtraWarnings ) {
									if ( ( std::abs( tmpCondVolFlowRate - CondVolFlowRateUser ) / CondVolFlowRateUser ) > AutoVsHardSizingThreshold ) {
										ShowMessage( "SizeChillerConstantCOP: Potential issue with equipment sizing for " + ConstCOPChiller( ChillNum ).Name );
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
				if ( ConstCOPChiller( ChillNum ).CondVolFlowRateWasAutoSized && PlantFirstSizesOkayToFinalize ) {
					ShowSevereError( "Autosizing of Constant COP Chiller condenser flow rate requires a condenser" );
					ShowContinueError( "loop Sizing:Plant object" );
					ShowContinueError( "Occurs in Chiller:ConstantCOP object=" + ConstCOPChiller( ChillNum ).Name );
					ErrorsFound = true;
				}
				if ( ! ConstCOPChiller( ChillNum ).CondVolFlowRateWasAutoSized && PlantFinalSizesOkayToReport
						&& ( ConstCOPChiller( ChillNum ).CondVolFlowRate > 0.0 ) ) {
						ReportSizingOutput( "Chiller:ConstantCOP", ConstCOPChiller( ChillNum ).Name,
							"User-Specified Design Condenser Water Flow Rate [m3/s]", ConstCOPChiller( ChillNum ).CondVolFlowRate );
				}
			}
		}

		// save the design condenser water volumetric flow rate for use by the condenser water loop sizing algorithms
		if ( ConstCOPChiller( ChillNum ).CondenserType == WaterCooled ) RegisterPlantCompDesignFlow( ConstCOPChiller( ChillNum ).CondInletNodeNum, tmpCondVolFlowRate );

		if ( ErrorsFound ) {
			ShowFatalError( "Preceding sizing errors cause program termination" );
		}

		//create predefined report
		if ( PlantFinalSizesOkayToReport ) {
			equipName = ConstCOPChiller( ChillNum ).Name;
			PreDefTableEntry( pdchMechType, equipName, "Chiller:ConstantCOP" );
			PreDefTableEntry( pdchMechNomEff, equipName, ConstCOPChiller( ChillNum ).COP );
			PreDefTableEntry( pdchMechNomCap, equipName, ConstCOPChiller( ChillNum ).NomCap );
		}

	}

	void
	CalcElectricChillerModel(
		int & ChillNum, // chiller number
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
		ElectricChiller( ChillNum ).EvapMassFlowRate = 0.0;
		ElectricChiller( ChillNum ).CondMassFlowRate = 0.0;
		Power = 0.0;
		Energy = 0.0;
		QCondenser = 0.0;
		QEvaporator = 0.0;
		CondenserEnergy = 0.0;
		EvaporatorEnergy = 0.0;
		QHeatRecovered = 0.0;
		EvapInletNode = ElectricChiller( ChillNum ).EvapInletNodeNum;
		EvapOutletNode = ElectricChiller( ChillNum ).EvapOutletNodeNum;
		CondInletNode = ElectricChiller( ChillNum ).CondInletNodeNum;
		CondOutletNode = ElectricChiller( ChillNum ).CondOutletNodeNum;
		FRAC = 1.0;
		LoopNum = ElectricChiller( ChillNum ).CWLoopNum;
		LoopSideNum = ElectricChiller( ChillNum ).CWLoopSideNum;
		BranchNum = ElectricChiller( ChillNum ).CWBranchNum;
		CompNum = ElectricChiller( ChillNum ).CWCompNum;
		EvapInletTemp = Node( EvapInletNode ).Temp;

		//   calculate end time of current time step
		CurrentEndTime = CurrentTime + SysTimeElapsed;

		//   Print warning messages only when valid and only for the first ocurrance. Let summary provide statistics.
		//   Wait for next time step to print warnings. If simulation iterates, print out
		//   the warning for the last iteration only. Must wait for next time step to accomplish this.
		//   If a warning occurs and the simulation down shifts, the warning is not valid.
		if ( CurrentEndTime > CurrentEndTimeLast && TimeStepSys >= TimeStepSysLast ) {
			if ( ElectricChiller( ChillNum ).PrintMessage ) {
				++ElectricChiller( ChillNum ).MsgErrorCount;
				//       Show single warning and pass additional info to ShowRecurringWarningErrorAtEnd
				if ( ElectricChiller( ChillNum ).MsgErrorCount < 2 ) {
					ShowWarningError( ElectricChiller( ChillNum ).MsgBuffer1 + '.' );
					ShowContinueError( ElectricChiller( ChillNum ).MsgBuffer2 );
				} else {
					ShowRecurringWarningErrorAtEnd( ElectricChiller( ChillNum ).MsgBuffer1 + " error continues.", ElectricChiller( ChillNum ).ErrCount1, ElectricChiller( ChillNum ).MsgDataLast, ElectricChiller( ChillNum ).MsgDataLast, _, "[C]", "[C]" );
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
				ElectricChiller( ChillNum ).EvapMassFlowRate = Node( EvapInletNode ).MassFlowRate;
			} else {
				ElectricChiller( ChillNum ).EvapMassFlowRate = 0.0;
				SetComponentFlowRate( ElectricChiller( ChillNum ).EvapMassFlowRate, EvapInletNode, EvapOutletNode, ElectricChiller( ChillNum ).CWLoopNum, ElectricChiller( ChillNum ).CWLoopSideNum, ElectricChiller( ChillNum ).CWBranchNum, ElectricChiller( ChillNum ).CWCompNum );
			}
			if ( ElectricChiller( ChillNum ).CondenserType == WaterCooled ) {
				if ( PlantLoop( ElectricChiller( ChillNum ).CDLoopNum ).LoopSide( ElectricChiller( ChillNum ).CDLoopSideNum ).Branch( ElectricChiller( ChillNum ).CDBranchNum ).Comp( ElectricChiller( ChillNum ).CDCompNum ).FlowCtrl == ControlType_SeriesActive ) {
					ElectricChiller( ChillNum ).CondMassFlowRate = Node( CondInletNode ).MassFlowRate;
				} else {
					ElectricChiller( ChillNum ).CondMassFlowRate = 0.0;
					SetComponentFlowRate( ElectricChiller( ChillNum ).CondMassFlowRate, CondInletNode, CondOutletNode, ElectricChiller( ChillNum ).CDLoopNum, ElectricChiller( ChillNum ).CDLoopSideNum, ElectricChiller( ChillNum ).CDBranchNum, ElectricChiller( ChillNum ).CDCompNum );
				}
			}

			if ( ElectricChiller( ChillNum ).CondenserType == EvapCooled ) {
				CalcBasinHeaterPower( ElectricChiller( ChillNum ).BasinHeaterPowerFTempDiff, ElectricChiller( ChillNum ).BasinHeaterSchedulePtr, ElectricChiller( ChillNum ).BasinHeaterSetPointTemp, BasinHeaterPower );
			}
			ElectricChiller( ChillNum ).PrintMessage = false;
			return;
		}

		// If not air or evap cooled then set to the condenser node that is attached to a cooling tower
		CondInletTemp = Node( CondInletNode ).Temp;

		//Set mass flow rates
		if ( ElectricChiller( ChillNum ).CondenserType == WaterCooled ) {
			ElectricChiller( ChillNum ).CondMassFlowRate = ElectricChiller( ChillNum ).CondMassFlowRateMax;
			SetComponentFlowRate( ElectricChiller( ChillNum ).CondMassFlowRate, CondInletNode, CondOutletNode, ElectricChiller( ChillNum ).CDLoopNum, ElectricChiller( ChillNum ).CDLoopSideNum, ElectricChiller( ChillNum ).CDBranchNum, ElectricChiller( ChillNum ).CDCompNum );
			PullCompInterconnectTrigger( ElectricChiller( ChillNum ).CWLoopNum, ElectricChiller( ChillNum ).CWLoopSideNum, ElectricChiller( ChillNum ).CWBranchNum, ElectricChiller( ChillNum ).CWCompNum, ElectricChiller( ChillNum ).CondMassFlowIndex, ElectricChiller( ChillNum ).CDLoopNum, ElectricChiller( ChillNum ).CDLoopSideNum, CriteriaType_MassFlowRate, ElectricChiller( ChillNum ).CondMassFlowRate );
			if ( ElectricChiller( ChillNum ).CondMassFlowRate < MassFlowTolerance ) return;
		}

		//  LOAD LOCAL VARIABLES FROM DATA STRUCTURE (for code readability)
		auto const & CapacityRat( ElectricChiller( ChillNum ).CapRatCoef );
		auto const & PowerRat( ElectricChiller( ChillNum ).PowerRatCoef );
		auto const & FullLoadFactor( ElectricChiller( ChillNum ).FullLoadCoef );
		MinPartLoadRat = ElectricChiller( ChillNum ).MinPartLoadRat;
		PartLoadRat = MinPartLoadRat;
		MaxPartLoadRat = ElectricChiller( ChillNum ).MaxPartLoadRat;
		TempCondInDesign = ElectricChiller( ChillNum ).TempDesCondIn;
		TempRiseRat = ElectricChiller( ChillNum ).TempRiseCoef;
		TempEvapOutDesign = ElectricChiller( ChillNum ).TempDesEvapOut;
		ChillerNomCap = ElectricChiller( ChillNum ).NomCap;
		RatedCOP = ElectricChiller( ChillNum ).COP;
		TempEvapOut = Node( ElectricChiller( ChillNum ).EvapOutletNodeNum ).Temp;
		TempLowLimitEout = ElectricChiller( ChillNum ).TempLowLimitEvapOut;
		EvapMassFlowRateMax = ElectricChiller( ChillNum ).EvapMassFlowRateMax;
		PlantLoopNum = ElectricChiller( ChillNum ).CWLoopNum;

		LoopNum = ElectricChiller( ChillNum ).CWLoopNum;
		LoopSideNum = ElectricChiller( ChillNum ).CWLoopSideNum;

		// initialize outlet air humidity ratio of air or evap cooled chillers
		ElectricChiller( ChillNum ).CondOutletHumRat = Node( CondInletNode ).HumRat;

		if ( ElectricChiller( ChillNum ).CondenserType == AirCooled ) { //Condenser inlet temp = outdoor temp
			Node( CondInletNode ).Temp = Node( CondInletNode ).OutAirDryBulb;
			//  Warn user if entering condenser temperature falls below 0C
			if ( Node( CondInletNode ).Temp < 0.0 && ! WarmupFlag ) {
				ElectricChiller( ChillNum ).PrintMessage = true;
				gio::write( OutputChar, OutputFormat ) << Node( CondInletNode ).Temp;
				ElectricChiller( ChillNum ).MsgBuffer1 = "CalcElectricChillerModel - Chiller:Electric \"" + ElectricChiller( ChillNum ).Name + "\" - Air Cooled Condenser Inlet Temperature below 0C";
				ElectricChiller( ChillNum ).MsgBuffer2 = "... Outdoor Dry-bulb Condition = " + OutputChar + " C. Occurrence info = " + EnvironmentName + ", " + CurMnDy + ' ' + CreateSysTimeIntervalString();
				ElectricChiller( ChillNum ).MsgDataLast = Node( CondInletNode ).Temp;
			} else {
				ElectricChiller( ChillNum ).PrintMessage = false;
			}
		} else if ( ElectricChiller( ChillNum ).CondenserType == EvapCooled ) { //Condenser inlet temp = (outdoor wet bulb)
			Node( CondInletNode ).Temp = Node( CondInletNode ).OutAirWetBulb;
			//  line above assumes evaporation pushes condenser inlet air humidity ratio to saturation
			ElectricChiller( ChillNum ).CondOutletHumRat = PsyWFnTdbTwbPb( Node( CondInletNode ).Temp, Node( CondInletNode ).Temp, Node( CondInletNode ).Press );
			//  Warn user if evap condenser wet bulb temperature falls below 10C
			if ( Node( CondInletNode ).Temp < 10.0 && ! WarmupFlag ) {
				ElectricChiller( ChillNum ).PrintMessage = true;
				gio::write( OutputChar, OutputFormat ) << Node( CondInletNode ).Temp;
				ElectricChiller( ChillNum ).MsgBuffer1 = "CalcElectricChillerModel - Chiller:Electric \"" + ElectricChiller( ChillNum ).Name + "\" - Evap Cooled Condenser Inlet Temperature below 10C";
				ElectricChiller( ChillNum ).MsgBuffer2 = "... Outdoor Wet-bulb Condition = " + OutputChar + " C. Occurrence info = " + EnvironmentName + ", " + CurMnDy + ' ' + CreateSysTimeIntervalString();
				ElectricChiller( ChillNum ).MsgDataLast = Node( CondInletNode ).Temp;
			} else {
				ElectricChiller( ChillNum ).PrintMessage = false;
			}
		} // End of the Air Cooled/Evap Cooled Logic block

		CondInletTemp = Node( CondInletNode ).Temp;

		// correct inlet temperature if using heat recovery
		if ( ElectricChiller( ChillNum ).HeatRecActive ) {
			if ( ( ElectricChillerReport( ChillNum ).QHeatRecovery + ElectricChillerReport( ChillNum ).QCond ) > 0.0 ) {
				AvgCondSinkTemp = ( ElectricChillerReport( ChillNum ).QHeatRecovery * ElectricChillerReport( ChillNum ).HeatRecInletTemp + ElectricChillerReport( ChillNum ).QCond * ElectricChillerReport( ChillNum ).CondInletTemp ) / ( ElectricChillerReport( ChillNum ).QHeatRecovery + ElectricChillerReport( ChillNum ).QCond );
			} else {
				AvgCondSinkTemp = CondInletTemp;
			}
		} else {
			AvgCondSinkTemp = CondInletTemp;
		}

		//Calculate chiller performance from this set of performance equations.
		//  from BLAST...Z=(TECONDW-ADJTC(1))/ADJTC(2)-(TLCHLRW-ADJTC(3))

		DeltaTemp = ( AvgCondSinkTemp - TempCondInDesign ) / TempRiseRat - ( TempEvapOut - TempEvapOutDesign );

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

		Cp = GetSpecificHeatGlycol( PlantLoop( ElectricChiller( ChillNum ).CWLoopNum ).FluidName, Node( EvapInletNode ).Temp, PlantLoop( ElectricChiller( ChillNum ).CWLoopNum ).FluidIndex, RoutineName );

		// If FlowLock is True, the new resolved mdot is used to update Power, QEvap, Qcond, and
		// condenser side outlet temperature.
		if ( PlantLoop( LoopNum ).LoopSide( LoopSideNum ).FlowLock == 0 ) {

			//ElectricChiller(ChillNum)%PossibleSubcooling = .FALSE.
			//PossibleSubcooling = .NOT. PlantLoop(PlantLoopNum)%TempSetPtCtrl
			if ( PlantLoop( PlantLoopNum ).LoopSide( LoopSideNum ).Branch( BranchNum ).Comp( CompNum ).CurOpSchemeType == CompSetPtBasedSchemeType ) {
				ElectricChiller( ChillNum ).PossibleSubcooling = false;
			} else {
				ElectricChiller( ChillNum ).PossibleSubcooling = true;
			}
			QEvaporator = AvailChillerCap * OperPartLoadRat;
			if ( OperPartLoadRat < MinPartLoadRat ) {
				FRAC = min( 1.0, ( OperPartLoadRat / MinPartLoadRat ) );
			} else {
				FRAC = 1.0;
			}
			Power = FracFullLoadPower * FullLoadPowerRat * AvailChillerCap / RatedCOP * FRAC;

			// Either set the flow to the Constant value or caluclate the flow for the variable volume
			if ( ( ElectricChiller( ChillNum ).FlowMode == ConstantFlow ) || ( ElectricChiller( ChillNum ).FlowMode == NotModulated ) ) {

				// Start by assuming max (design) flow
				ElectricChiller( ChillNum ).EvapMassFlowRate = EvapMassFlowRateMax;
				// Use SetComponentFlowRate to decide actual flow
				SetComponentFlowRate( ElectricChiller( ChillNum ).EvapMassFlowRate, EvapInletNode, EvapOutletNode, ElectricChiller( ChillNum ).CWLoopNum, ElectricChiller( ChillNum ).CWLoopSideNum, ElectricChiller( ChillNum ).CWBranchNum, ElectricChiller( ChillNum ).CWCompNum );
				// Evaluate delta temp based on actual flow rate
				if ( ElectricChiller( ChillNum ).EvapMassFlowRate != 0.0 ) {
					EvapDeltaTemp = QEvaporator / ElectricChiller( ChillNum ).EvapMassFlowRate / Cp;
				} else {
					EvapDeltaTemp = 0.0;
				}
				// Evaluate outlet temp based on delta
				ElectricChiller( ChillNum ).EvapOutletTemp = Node( EvapInletNode ).Temp - EvapDeltaTemp;

			} else if ( ElectricChiller( ChillNum ).FlowMode == LeavingSetPointModulated ) {

				// Calculate the Delta Temp from the inlet temp to the chiller outlet setpoint
				{ auto const SELECT_CASE_var( PlantLoop( ElectricChiller( ChillNum ).CWLoopNum ).LoopDemandCalcScheme );
				if ( SELECT_CASE_var == SingleSetPoint ) {
					EvapDeltaTemp = Node( EvapInletNode ).Temp - Node( EvapOutletNode ).TempSetPoint;
				} else if ( SELECT_CASE_var == DualSetPointDeadBand ) {
					EvapDeltaTemp = Node( EvapInletNode ).Temp - Node( EvapOutletNode ).TempSetPointHi;
				}}

				if ( EvapDeltaTemp != 0.0 ) {

					// Calculate desired flow to request based on load
					ElectricChiller( ChillNum ).EvapMassFlowRate = std::abs( QEvaporator / Cp / EvapDeltaTemp );
					//Check to see if the Maximum is exceeded, if so set to maximum
					if ( ( ElectricChiller( ChillNum ).EvapMassFlowRate - EvapMassFlowRateMax ) > MassFlowTolerance ) ElectricChiller( ChillNum ).PossibleSubcooling = true;
					ElectricChiller( ChillNum ).EvapMassFlowRate = min( EvapMassFlowRateMax, ElectricChiller( ChillNum ).EvapMassFlowRate );
					// Use SetComponentFlowRate to decide actual flow
					SetComponentFlowRate( ElectricChiller( ChillNum ).EvapMassFlowRate, EvapInletNode, EvapOutletNode, ElectricChiller( ChillNum ).CWLoopNum, ElectricChiller( ChillNum ).CWLoopSideNum, ElectricChiller( ChillNum ).CWBranchNum, ElectricChiller( ChillNum ).CWCompNum );
					{ auto const SELECT_CASE_var( PlantLoop( ElectricChiller( ChillNum ).CWLoopNum ).LoopDemandCalcScheme );
					if ( SELECT_CASE_var == SingleSetPoint ) {
						ElectricChiller( ChillNum ).EvapOutletTemp = Node( EvapOutletNode ).TempSetPoint;
					} else if ( SELECT_CASE_var == DualSetPointDeadBand ) {
						ElectricChiller( ChillNum ).EvapOutletTemp = Node( EvapOutletNode ).TempSetPointHi;
					}}

				} else {

					// Try to request zero flow
					ElectricChiller( ChillNum ).EvapMassFlowRate = 0.0;
					// Use SetComponentFlowRate to decide actual flow
					SetComponentFlowRate( ElectricChiller( ChillNum ).EvapMassFlowRate, EvapInletNode, EvapOutletNode, ElectricChiller( ChillNum ).CWLoopNum, ElectricChiller( ChillNum ).CWLoopSideNum, ElectricChiller( ChillNum ).CWBranchNum, ElectricChiller( ChillNum ).CWCompNum );
					// No deltaT since component is not running
					ElectricChiller( ChillNum ).EvapOutletTemp = Node( EvapInletNode ).Temp;

				}

			} //End of Constant Variable Flow If Block

		} else { // If FlowLock is True

			ElectricChiller( ChillNum ).EvapMassFlowRate = Node( EvapInletNode ).MassFlowRate;
			SetComponentFlowRate( ElectricChiller( ChillNum ).EvapMassFlowRate, EvapInletNode, EvapOutletNode, ElectricChiller( ChillNum ).CWLoopNum, ElectricChiller( ChillNum ).CWLoopSideNum, ElectricChiller( ChillNum ).CWBranchNum, ElectricChiller( ChillNum ).CWCompNum );

			//       Some other component set the flow to 0. No reason to continue with calculations.
			if ( ElectricChiller( ChillNum ).EvapMassFlowRate == 0.0 ) {
				MyLoad = 0.0;
				if ( ElectricChiller( ChillNum ).CondenserType == EvapCooled ) {
					CalcBasinHeaterPower( ElectricChiller( ChillNum ).BasinHeaterPowerFTempDiff, ElectricChiller( ChillNum ).BasinHeaterSchedulePtr, ElectricChiller( ChillNum ).BasinHeaterSetPointTemp, BasinHeaterPower );
				}
				ElectricChiller( ChillNum ).PrintMessage = false;
				return;
			}
			//Flow resolver might have given less flow or control scheme have provided more load, which may
			//result in subcooling.
			if ( ElectricChiller( ChillNum ).PossibleSubcooling ) {
				QEvaporator = std::abs( MyLoad );
				EvapDeltaTemp = QEvaporator / ElectricChiller( ChillNum ).EvapMassFlowRate / Cp;
				ElectricChiller( ChillNum ).EvapOutletTemp = Node( EvapInletNode ).Temp - EvapDeltaTemp;
			} else { //No subcooling in this case.No recalculation required.Still need to check chiller low temp limit

				{ auto const SELECT_CASE_var( PlantLoop( LoopNum ).LoopDemandCalcScheme );
				if ( SELECT_CASE_var == SingleSetPoint ) {
					if ( ( ElectricChiller( ChillNum ).FlowMode == LeavingSetPointModulated ) || ( PlantLoop( LoopNum ).LoopSide( LoopSideNum ).Branch( BranchNum ).Comp( CompNum ).CurOpSchemeType == CompSetPtBasedSchemeType ) || ( Node( EvapOutletNode ).TempSetPoint != SensedNodeFlagValue ) ) {
						TempEvapOutSetPoint = Node( EvapOutletNode ).TempSetPoint;
					} else {
						TempEvapOutSetPoint = Node( PlantLoop( LoopNum ).TempSetPointNodeNum ).TempSetPoint;
					}
				} else if ( SELECT_CASE_var == DualSetPointDeadBand ) {
					if ( ( ElectricChiller( ChillNum ).FlowMode == LeavingSetPointModulated ) || ( PlantLoop( LoopNum ).LoopSide( LoopSideNum ).Branch( BranchNum ).Comp( CompNum ).CurOpSchemeType == CompSetPtBasedSchemeType ) || ( Node( EvapOutletNode ).TempSetPointHi != SensedNodeFlagValue ) ) {
						TempEvapOutSetPoint = Node( EvapOutletNode ).TempSetPointHi;
					} else {
						TempEvapOutSetPoint = Node( PlantLoop( LoopNum ).TempSetPointNodeNum ).TempSetPointHi;
					}
				}}
				EvapDeltaTemp = Node( EvapInletNode ).Temp - TempEvapOutSetPoint;
				QEvaporator = std::abs( ElectricChiller( ChillNum ).EvapMassFlowRate * Cp * EvapDeltaTemp );
				ElectricChiller( ChillNum ).EvapOutletTemp = TempEvapOutSetPoint;
			}
			//Check that the Evap outlet temp honors both plant loop temp low limit and also the chiller low limit
			if ( ElectricChiller( ChillNum ).EvapOutletTemp < TempLowLimitEout ) {
				if ( ( Node( EvapInletNode ).Temp - TempLowLimitEout ) > DeltaTempTol ) {
					ElectricChiller( ChillNum ).EvapOutletTemp = TempLowLimitEout;
					EvapDeltaTemp = Node( EvapInletNode ).Temp - ElectricChiller( ChillNum ).EvapOutletTemp;
					QEvaporator = ElectricChiller( ChillNum ).EvapMassFlowRate * Cp * EvapDeltaTemp;
				} else {
					ElectricChiller( ChillNum ).EvapOutletTemp = Node( EvapInletNode ).Temp;
					EvapDeltaTemp = Node( EvapInletNode ).Temp - ElectricChiller( ChillNum ).EvapOutletTemp;
					QEvaporator = ElectricChiller( ChillNum ).EvapMassFlowRate * Cp * EvapDeltaTemp;
				}
			}
			if ( ElectricChiller( ChillNum ).EvapOutletTemp < Node( EvapOutletNode ).TempMin ) {
				if ( ( Node( EvapInletNode ).Temp - Node( EvapOutletNode ).TempMin ) > DeltaTempTol ) {
					ElectricChiller( ChillNum ).EvapOutletTemp = Node( EvapOutletNode ).TempMin;
					EvapDeltaTemp = Node( EvapInletNode ).Temp - ElectricChiller( ChillNum ).EvapOutletTemp;
					QEvaporator = ElectricChiller( ChillNum ).EvapMassFlowRate * Cp * EvapDeltaTemp;
				} else {
					ElectricChiller( ChillNum ).EvapOutletTemp = Node( EvapInletNode ).Temp;
					EvapDeltaTemp = Node( EvapInletNode ).Temp - ElectricChiller( ChillNum ).EvapOutletTemp;
					QEvaporator = ElectricChiller( ChillNum ).EvapMassFlowRate * Cp * EvapDeltaTemp;
				}
			}

			// If load exceeds the distributed load set to the distributed load
			if ( QEvaporator > std::abs( MyLoad ) ) {
				if ( ElectricChiller( ChillNum ).EvapMassFlowRate > MassFlowTolerance ) {
					QEvaporator = std::abs( MyLoad );
					EvapDeltaTemp = QEvaporator / ElectricChiller( ChillNum ).EvapMassFlowRate / Cp;
					ElectricChiller( ChillNum ).EvapOutletTemp = Node( EvapInletNode ).Temp - EvapDeltaTemp;
				} else {
					QEvaporator = 0.0;
					ElectricChiller( ChillNum ).EvapOutletTemp = Node( EvapInletNode ).Temp;
				}
			}

			// Checks QEvaporator on the basis of the machine limits.
			if ( QEvaporator > ( AvailChillerCap * MaxPartLoadRat ) ) {
				if ( ElectricChiller( ChillNum ).EvapMassFlowRate > MassFlowTolerance ) {
					QEvaporator = AvailChillerCap * OperPartLoadRat;
					EvapDeltaTemp = QEvaporator / ElectricChiller( ChillNum ).EvapMassFlowRate / Cp;
					ElectricChiller( ChillNum ).EvapOutletTemp = Node( EvapInletNode ).Temp - EvapDeltaTemp;
				} else {
					QEvaporator = 0.0;
					ElectricChiller( ChillNum ).EvapOutletTemp = Node( EvapInletNode ).Temp;
				}
			}

			if ( OperPartLoadRat < MinPartLoadRat ) {
				FRAC = min( 1.0, ( OperPartLoadRat / MinPartLoadRat ) );
			} else {
				FRAC = 1.0;
			}

			// set the module level variable used for reporting FRAC
			ChillerCyclingRatio = FRAC;

			// Chiller is false loading below PLR = minimum unloading ratio, find PLR used for energy calculation
			Power = FracFullLoadPower * FullLoadPowerRat * AvailChillerCap / RatedCOP * FRAC;

			if ( ElectricChiller( ChillNum ).EvapMassFlowRate == 0.0 ) {
				QEvaporator = 0.0;
				ElectricChiller( ChillNum ).EvapOutletTemp = Node( EvapInletNode ).Temp;
				Power = 0.0;
				ElectricChiller( ChillNum ).PrintMessage = false;
			}
			if ( QEvaporator == 0.0 && ElectricChiller( ChillNum ).CondenserType == EvapCooled ) {
				CalcBasinHeaterPower( ElectricChiller( ChillNum ).BasinHeaterPowerFTempDiff, ElectricChiller( ChillNum ).BasinHeaterSchedulePtr, ElectricChiller( ChillNum ).BasinHeaterSetPointTemp, BasinHeaterPower );
			}
		} //This is the end of the FlowLock Block

		//QCondenser is calculated the same for each type, but the power consumption should be different
		//  depending on the performance coefficients used for the chiller model.
		QCondenser = Power + QEvaporator;

		if ( ElectricChiller( ChillNum ).CondenserType == WaterCooled ) {
			if ( ElectricChiller( ChillNum ).CondMassFlowRate > MassFlowTolerance ) {
				// If Heat Recovery specified for this vapor compression chiller, then Qcondenser will be adjusted by this subroutine
				if ( ElectricChiller( ChillNum ).HeatRecActive ) CalcElectricChillerHeatRecovery( ChillNum, QCondenser, ElectricChiller( ChillNum ).CondMassFlowRate, CondInletTemp, QHeatRecovered );
				CpCond = GetSpecificHeatGlycol( PlantLoop( ElectricChiller( ChillNum ).CDLoopNum ).FluidName, CondInletTemp, PlantLoop( ElectricChiller( ChillNum ).CDLoopNum ).FluidIndex, RoutineName );
				ElectricChiller( ChillNum ).CondOutletTemp = QCondenser / ElectricChiller( ChillNum ).CondMassFlowRate / CpCond + CondInletTemp;
			} else {
				ShowSevereError( "CalcElectricChillerModel: Condenser flow = 0, for ElectricChiller=" + ElectricChiller( ChillNum ).Name );
				ShowContinueErrorTimeStamp( "" );

			}
		} else { //Air Cooled or Evap Cooled

			if ( QCondenser > 0.0 ) {
				ElectricChiller( ChillNum ).CondMassFlowRate = ElectricChiller( ChillNum ).CondMassFlowRateMax * OperPartLoadRat;
			} else {
				ElectricChiller( ChillNum ).CondMassFlowRate = 0.0;
			}

			// If Heat Recovery specified for this vapor compression chiller, then Qcondenser will be adjusted by this subroutine
			if ( ElectricChiller( ChillNum ).HeatRecActive ) CalcElectricChillerHeatRecovery( ChillNum, QCondenser, ElectricChiller( ChillNum ).CondMassFlowRate, CondInletTemp, QHeatRecovered );
			if ( ElectricChiller( ChillNum ).CondMassFlowRate > 0.0 ) {
				CpCond = PsyCpAirFnWTdb( Node( CondInletNode ).HumRat, CondInletTemp );
				ElectricChiller( ChillNum ).CondOutletTemp = CondInletTemp + QCondenser / ElectricChiller( ChillNum ).CondMassFlowRate / CpCond;
			} else {
				ElectricChiller( ChillNum ).CondOutletTemp = CondInletTemp;
			}
		}

		//Calculate Energy
		CondenserEnergy = QCondenser * TimeStepSys * SecInHour;
		Energy = Power * TimeStepSys * SecInHour;
		EvaporatorEnergy = QEvaporator * TimeStepSys * SecInHour;

		//check for problems BG 9/12/06 (deal with observed negative energy results)
		if ( Energy < 0.0 ) { // there is a serious problem

			if ( ElectricChiller( ChillNum ).CondenserType == WaterCooled ) {
				// first check for run away condenser loop temps (only reason yet to be observed for this?)
				if ( CondInletTemp > 70.0 ) {
					ShowSevereError( "CalcElectricChillerModel: Condenser loop inlet temperatures over 70.0 C for ElectricChiller=" + ElectricChiller( ChillNum ).Name );
					ShowContinueErrorTimeStamp( "" );
					ShowContinueError( "Condenser loop water temperatures are too high at" + RoundSigDigits( CondInletTemp, 2 ) );
					ShowContinueError( "Check input for condenser plant loop, especially cooling tower" );
					ShowContinueError( "Evaporator inlet temperature: " + RoundSigDigits( Node( EvapInletNode ).Temp, 2 ) );

					ShowFatalError( "Program Terminates due to previous error condition" );
				}
			}
			if ( ! WarmupFlag ) {
				if ( AvailNomCapRat < 0.0 ) { // apparently the real reason energy goes negative
					ShowSevereError( "CalcElectricChillerModel: Capacity ratio below zero for ElectricChiller=" + ElectricChiller( ChillNum ).Name );
					ShowContinueErrorTimeStamp( "" );
					ShowContinueError( "Check input for Capacity Ratio Curve" );
					ShowContinueError( "Condenser inlet temperature: " + RoundSigDigits( CondInletTemp, 2 ) );
					ShowContinueError( "Evaporator inlet temperature: " + RoundSigDigits( Node( EvapInletNode ).Temp, 2 ) );
					ShowFatalError( "Program Terminates due to previous error condition" );
				}
			}
			// If makes it here, set limits, chiller can't have negative energy/power
			// proceeding silently for now but may want to throw error here
			Power = 0.0;
			Energy = 0.0;
		}
	}

	void
	CalcEngineDrivenChillerModel(
		int & ChillerNum, // chiller number
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
		ElectricChiller( ChillerNum ).EvapMassFlowRate = 0.0;
		ElectricChiller( ChillerNum ).CondMassFlowRate = 0.0;
		Power = 0.0;
		QCondenser = 0.0;
		QEvaporator = 0.0;
		Energy = 0.0;
		CondenserEnergy = 0.0;
		EvaporatorEnergy = 0.0;
		HeatRecCp = 0.0;
		EngineDrivenChiller( ChillerNum ).HeatRecMdotActual = 0.0;
		EngineDrivenChiller( ChillerNum ).QTotalHeatRecovered = 0.0;
		EngineDrivenChiller( ChillerNum ).QJacketRecovered = 0.0;
		EngineDrivenChiller( ChillerNum ).QLubeOilRecovered = 0.0;
		EngineDrivenChiller( ChillerNum ).QExhaustRecovered = 0.0;
		EngineDrivenFuelEnergy = 0.0;
		EngineDrivenChiller( ChillerNum ).FuelEnergyUseRate = 0.0;
		EngineDrivenChiller( ChillerNum ).TotalHeatEnergyRec = 0.0;
		EngineDrivenChiller( ChillerNum ).JacketEnergyRec = 0.0;
		EngineDrivenChiller( ChillerNum ).LubeOilEnergyRec = 0.0;
		EngineDrivenChiller( ChillerNum ).ExhaustEnergyRec = 0.0;
		EngineDrivenChiller( ChillerNum ).FuelEnergy = 0.0;
		EngineDrivenChiller( ChillerNum ).FuelMdot = 0.0;
		EngineDrivenChiller( ChillerNum ).ExhaustStackTemp = 0.0;
		FRAC = 1.0;

		if ( EngineDrivenChiller( ChillerNum ).HeatRecActive ) {
			EngineDrivenChiller( ChillerNum ).HeatRecInletTemp = Node( EngineDrivenChiller( ChillerNum ).HeatRecInletNodeNum ).Temp;
			HeatRecOutletTemp = Node( EngineDrivenChiller( ChillerNum ).HeatRecInletNodeNum ).Temp;
			EngineDrivenChiller( ChillerNum ).HeatRecMdotDesign = EngineDrivenChiller( ChillerNum ).DesignHeatRecMassFlowRate;
		}

		EvapInletNode = EngineDrivenChiller( ChillerNum ).EvapInletNodeNum;
		EvapOutletNode = EngineDrivenChiller( ChillerNum ).EvapOutletNodeNum;
		CondInletNode = EngineDrivenChiller( ChillerNum ).CondInletNodeNum;
		CondOutletNode = EngineDrivenChiller( ChillerNum ).CondOutletNodeNum;
		LoopNum = EngineDrivenChiller( ChillerNum ).CWLoopNum;
		LoopSideNum = EngineDrivenChiller( ChillerNum ).CWLoopSideNum;
		EvapInletTemp = Node( EvapInletNode ).Temp;

		//   calculate end time of current time step
		CurrentEndTime = CurrentTime + SysTimeElapsed;

		//   Print warning messages only when valid and only for the first ocurrance. Let summary provide statistics.
		//   Wait for next time step to print warnings. If simulation iterates, print out
		//   the warning for the last iteration only. Must wait for next time step to accomplish this.
		//   If a warning occurs and the simulation down shifts, the warning is not valid.
		if ( CurrentEndTime > CurrentEndTimeLast && TimeStepSys >= TimeStepSysLast ) {
			if ( EngineDrivenChiller( ChillerNum ).PrintMessage ) {
				++EngineDrivenChiller( ChillerNum ).MsgErrorCount;
				//     Show single warning and pass additional info to ShowRecurringWarningErrorAtEnd
				if ( EngineDrivenChiller( ChillerNum ).MsgErrorCount < 2 ) {
					ShowWarningError( EngineDrivenChiller( ChillerNum ).MsgBuffer1 + '.' );
					ShowContinueError( EngineDrivenChiller( ChillerNum ).MsgBuffer2 );
				} else {
					ShowRecurringWarningErrorAtEnd( EngineDrivenChiller( ChillerNum ).MsgBuffer1 + " error continues.", EngineDrivenChiller( ChillerNum ).ErrCount1, EngineDrivenChiller( ChillerNum ).MsgDataLast, EngineDrivenChiller( ChillerNum ).MsgDataLast, _, "[C]", "[C]" );
				}
			}
		}

		// save last system time step and last end time of current time step (used to determine if warning is valid)
		TimeStepSysLast = TimeStepSys;
		CurrentEndTimeLast = CurrentEndTime;

		//If Chiller load is 0 or chiller is not running then leave the subroutine.
		if ( MyLoad >= 0.0 || ! RunFlag ) {
			if ( EquipFlowCtrl == ControlType_SeriesActive || PlantLoop( LoopNum ).LoopSide( LoopSideNum ).FlowLock == 1 ) {
				ElectricChiller( ChillerNum ).EvapMassFlowRate = Node( EvapInletNode ).MassFlowRate;
			} else {
				ElectricChiller( ChillerNum ).EvapMassFlowRate = 0.0;

				SetComponentFlowRate( ElectricChiller( ChillerNum ).EvapMassFlowRate, EvapInletNode, EvapOutletNode, EngineDrivenChiller( ChillerNum ).CWLoopNum, EngineDrivenChiller( ChillerNum ).CWLoopSideNum, EngineDrivenChiller( ChillerNum ).CWBranchNum, EngineDrivenChiller( ChillerNum ).CWCompNum );
			}

			if ( EngineDrivenChiller( ChillerNum ).CondenserType == WaterCooled ) {
				if ( PlantLoop( EngineDrivenChiller( ChillerNum ).CDLoopNum ).LoopSide( EngineDrivenChiller( ChillerNum ).CDLoopSideNum ).Branch( EngineDrivenChiller( ChillerNum ).CDBranchNum ).Comp( EngineDrivenChiller( ChillerNum ).CDCompNum ).FlowCtrl == ControlType_SeriesActive ) {
					ElectricChiller( ChillerNum ).CondMassFlowRate = Node( CondInletNode ).MassFlowRate;
				} else {
					ElectricChiller( ChillerNum ).CondMassFlowRate = 0.0;
					SetComponentFlowRate( ElectricChiller( ChillerNum ).CondMassFlowRate, CondInletNode, CondOutletNode, EngineDrivenChiller( ChillerNum ).CDLoopNum, EngineDrivenChiller( ChillerNum ).CDLoopSideNum, EngineDrivenChiller( ChillerNum ).CDBranchNum, EngineDrivenChiller( ChillerNum ).CDCompNum );
				}
			}

			if ( EngineDrivenChiller( ChillerNum ).CondenserType == EvapCooled ) {
				CalcBasinHeaterPower( EngineDrivenChiller( ChillerNum ).BasinHeaterPowerFTempDiff, EngineDrivenChiller( ChillerNum ).BasinHeaterSchedulePtr, EngineDrivenChiller( ChillerNum ).BasinHeaterSetPointTemp, BasinHeaterPower );
			}
			EngineDrivenChiller( ChillerNum ).PrintMessage = false;
			return;
		}

		if ( EngineDrivenChiller( ChillerNum ).CondenserType == AirCooled ) { //Condenser inlet temp = outdoor temp
			Node( CondInletNode ).Temp = Node( CondInletNode ).OutAirDryBulb;
			//  Warn user if entering condenser temperature falls below 0C
			if ( Node( CondInletNode ).Temp < 0.0 && ! WarmupFlag ) {
				EngineDrivenChiller( ChillerNum ).PrintMessage = true;
				gio::write( OutputChar, OutputFormat ) << Node( CondInletNode ).Temp;
				EngineDrivenChiller( ChillerNum ).MsgBuffer1 = "CalcEngineDrivenChillerModel - Chiller:EngineDriven \"" + EngineDrivenChiller( ChillerNum ).Name + "\" - Air Cooled Condenser Inlet Temperature below 0C";
				EngineDrivenChiller( ChillerNum ).MsgBuffer2 = "... Outdoor Dry-bulb Condition = " + OutputChar + " C. Occurrence info = " + EnvironmentName + ", " + CurMnDy + ' ' + CreateSysTimeIntervalString();
				EngineDrivenChiller( ChillerNum ).MsgDataLast = Node( CondInletNode ).Temp;
			} else {
				EngineDrivenChiller( ChillerNum ).PrintMessage = false;
			}
		} else if ( EngineDrivenChiller( ChillerNum ).CondenserType == EvapCooled ) { //Condenser inlet temp = (outdoor wet bulb)
			Node( CondInletNode ).Temp = Node( CondInletNode ).OutAirWetBulb;
			//  Warn user if evap condenser wet bulb temperature falls below 10C
			if ( Node( CondInletNode ).Temp < 10.0 && ! WarmupFlag ) {
				EngineDrivenChiller( ChillerNum ).PrintMessage = true;
				gio::write( OutputChar, OutputFormat ) << Node( CondInletNode ).Temp;
				EngineDrivenChiller( ChillerNum ).MsgBuffer1 = "CalcEngineDrivenChillerModel - Chiller:EngineDriven \"" + EngineDrivenChiller( ChillerNum ).Name + "\" - Evap Cooled Condenser Inlet Temperature below 10C";
				EngineDrivenChiller( ChillerNum ).MsgBuffer2 = "... Outdoor Wet-bulb Condition = " + OutputChar + " C. Occurrence info = " + EnvironmentName + ", " + CurMnDy + ' ' + CreateSysTimeIntervalString();
				EngineDrivenChiller( ChillerNum ).MsgDataLast = Node( CondInletNode ).Temp;
			} else {
				EngineDrivenChiller( ChillerNum ).PrintMessage = false;
			}
		} // End of the Air Cooled/Evap Cooled Logic block

		// If not air or evap cooled then set to the condenser node that is attached to a cooling tower
		CondInletTemp = Node( CondInletNode ).Temp;

		//Set mass flow rates
		if ( EngineDrivenChiller( ChillerNum ).CondenserType == WaterCooled ) {
			ElectricChiller( ChillerNum ).CondMassFlowRate = EngineDrivenChiller( ChillerNum ).CondMassFlowRateMax;
			SetComponentFlowRate( ElectricChiller( ChillerNum ).CondMassFlowRate, CondInletNode, CondOutletNode, EngineDrivenChiller( ChillerNum ).CDLoopNum, EngineDrivenChiller( ChillerNum ).CDLoopSideNum, EngineDrivenChiller( ChillerNum ).CDBranchNum, EngineDrivenChiller( ChillerNum ).CDCompNum );
			PullCompInterconnectTrigger( EngineDrivenChiller( ChillerNum ).CWLoopNum, EngineDrivenChiller( ChillerNum ).CWLoopSideNum, EngineDrivenChiller( ChillerNum ).CWBranchNum, EngineDrivenChiller( ChillerNum ).CWCompNum, EngineDrivenChiller( ChillerNum ).CondMassFlowIndex, EngineDrivenChiller( ChillerNum ).CDLoopNum, EngineDrivenChiller( ChillerNum ).CDLoopSideNum, CriteriaType_MassFlowRate, ElectricChiller( ChillerNum ).CondMassFlowRate );
			if ( ElectricChiller( ChillerNum ).CondMassFlowRate < MassFlowTolerance ) return;

		}

		//  LOAD LOCAL VARIABLES FROM DATA STRUCTURE (for code readability)
		auto const & CapacityRat( EngineDrivenChiller( ChillerNum ).CapRatCoef );
		auto const & PowerRat( EngineDrivenChiller( ChillerNum ).PowerRatCoef );
		auto const & FullLoadFactor( EngineDrivenChiller( ChillerNum ).FullLoadCoef );
		MinPartLoadRat = EngineDrivenChiller( ChillerNum ).MinPartLoadRat;
		MaxPartLoadRat = EngineDrivenChiller( ChillerNum ).MaxPartLoadRat;
		TempCondInDesign = EngineDrivenChiller( ChillerNum ).TempDesCondIn;
		TempRiseRat = EngineDrivenChiller( ChillerNum ).TempRiseCoef;
		TempEvapOutDesign = EngineDrivenChiller( ChillerNum ).TempDesEvapOut;
		ChillerNomCap = EngineDrivenChiller( ChillerNum ).NomCap;
		COP = EngineDrivenChiller( ChillerNum ).COP;
		TempCondIn = Node( EngineDrivenChiller( ChillerNum ).CondInletNodeNum ).Temp;
		TempEvapOut = Node( EngineDrivenChiller( ChillerNum ).EvapOutletNodeNum ).Temp;
		TempLowLimitEout = EngineDrivenChiller( ChillerNum ).TempLowLimitEvapOut;
		MaxExhaustperPowerOutput = EngineDrivenChiller( ChillerNum ).MaxExhaustperPowerOutput;
		LoopNum = EngineDrivenChiller( ChillerNum ).CWLoopNum;
		LoopSideNum = EngineDrivenChiller( ChillerNum ).CWLoopSideNum;
		EvapMassFlowRateMax = EngineDrivenChiller( ChillerNum ).EvapMassFlowRateMax;

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
		Cp = GetSpecificHeatGlycol( PlantLoop( EngineDrivenChiller( ChillerNum ).CWLoopNum ).FluidName, Node( EvapInletNode ).Temp, PlantLoop( EngineDrivenChiller( ChillerNum ).CWLoopNum ).FluidIndex, RoutineName );

		// If FlowLock is True, the new resolved mdot is used to update Power, QEvap, Qcond, and
		// condenser side outlet temperature.
		if ( PlantLoop( LoopNum ).LoopSide( LoopSideNum ).FlowLock == 0 ) {
			EngineDrivenChiller( ChillerNum ).PossibleSubcooling = false;
			QEvaporator = AvailChillerCap * OperPartLoadRat;
			if ( OperPartLoadRat < MinPartLoadRat ) {
				FRAC = min( 1.0, ( OperPartLoadRat / MinPartLoadRat ) );
			} else {
				FRAC = 1.0;
			}
			Power = FracFullLoadPower * FullLoadPowerRat * AvailChillerCap / COP * FRAC;

			// Either set the flow to the Constant value or caluclate the flow for the variable volume
			if ( ( EngineDrivenChiller( ChillerNum ).FlowMode == ConstantFlow ) || ( EngineDrivenChiller( ChillerNum ).FlowMode == NotModulated ) ) {
				// Start by assuming max (design) flow
				ElectricChiller( ChillerNum ).EvapMassFlowRate = EvapMassFlowRateMax;
				// Use SetComponentFlowRate to decide actual flow
				SetComponentFlowRate( ElectricChiller( ChillerNum ).EvapMassFlowRate, EvapInletNode, EvapOutletNode, EngineDrivenChiller( ChillerNum ).CWLoopNum, EngineDrivenChiller( ChillerNum ).CWLoopSideNum, EngineDrivenChiller( ChillerNum ).CWBranchNum, EngineDrivenChiller( ChillerNum ).CWCompNum );
				// Evaluate delta temp based on actual flow rate
				if ( ElectricChiller( ChillerNum ).EvapMassFlowRate != 0.0 ) {
					EvapDeltaTemp = QEvaporator / ElectricChiller( ChillerNum ).EvapMassFlowRate / Cp;
				} else {
					EvapDeltaTemp = 0.0;
				}
				// Evaluate outlet temp based on delta
				ElectricChiller( ChillerNum ).EvapOutletTemp = Node( EvapInletNode ).Temp - EvapDeltaTemp;

			} else if ( EngineDrivenChiller( ChillerNum ).FlowMode == LeavingSetPointModulated ) {

				// Calculate the Delta Temp from the inlet temp to the chiller outlet setpoint
				{ auto const SELECT_CASE_var( PlantLoop( EngineDrivenChiller( ChillerNum ).CWLoopNum ).LoopDemandCalcScheme );
				if ( SELECT_CASE_var == SingleSetPoint ) {
					EvapDeltaTemp = Node( EvapInletNode ).Temp - Node( EvapOutletNode ).TempSetPoint;
				} else if ( SELECT_CASE_var == DualSetPointDeadBand ) {
					EvapDeltaTemp = Node( EvapInletNode ).Temp - Node( EvapOutletNode ).TempSetPointHi;
				}}

				if ( EvapDeltaTemp != 0.0 ) {
					ElectricChiller( ChillerNum ).EvapMassFlowRate = std::abs( QEvaporator / Cp / EvapDeltaTemp );
					if ( ( ElectricChiller( ChillerNum ).EvapMassFlowRate - EvapMassFlowRateMax ) > MassFlowTolerance ) EngineDrivenChiller( ChillerNum ).PossibleSubcooling = true;
					//Check to see if the Maximum is exceeded, if so set to maximum
					ElectricChiller( ChillerNum ).EvapMassFlowRate = min( EvapMassFlowRateMax, ElectricChiller( ChillerNum ).EvapMassFlowRate );
					// Use SetComponentFlowRate to decide actual flow
					SetComponentFlowRate( ElectricChiller( ChillerNum ).EvapMassFlowRate, EvapInletNode, EvapOutletNode, EngineDrivenChiller( ChillerNum ).CWLoopNum, EngineDrivenChiller( ChillerNum ).CWLoopSideNum, EngineDrivenChiller( ChillerNum ).CWBranchNum, EngineDrivenChiller( ChillerNum ).CWCompNum );
					{ auto const SELECT_CASE_var( PlantLoop( EngineDrivenChiller( ChillerNum ).CWLoopNum ).LoopDemandCalcScheme );
					if ( SELECT_CASE_var == SingleSetPoint ) {
						ElectricChiller( ChillerNum ).EvapOutletTemp = Node( EvapOutletNode ).TempSetPoint;
					} else if ( SELECT_CASE_var == DualSetPointDeadBand ) {
						ElectricChiller( ChillerNum ).EvapOutletTemp = Node( EvapOutletNode ).TempSetPointHi;
					}}
				} else {
					// Try to request zero flow
					ElectricChiller( ChillerNum ).EvapMassFlowRate = 0.0;
					// Use SetComponentFlowRate to decide actual flow
					SetComponentFlowRate( ElectricChiller( ChillerNum ).EvapMassFlowRate, EvapInletNode, EvapOutletNode, EngineDrivenChiller( ChillerNum ).CWLoopNum, EngineDrivenChiller( ChillerNum ).CWLoopSideNum, EngineDrivenChiller( ChillerNum ).CWBranchNum, EngineDrivenChiller( ChillerNum ).CWCompNum );
					// No deltaT since component is not running
					ElectricChiller( ChillerNum ).EvapOutletTemp = Node( EvapInletNode ).Temp;

				}
			} //End of Constant Variable Flow If Block
		} else { // If FlowLock is True

			ElectricChiller( ChillerNum ).EvapMassFlowRate = Node( EvapInletNode ).MassFlowRate;
			SetComponentFlowRate( ElectricChiller( ChillerNum ).EvapMassFlowRate, EvapInletNode, EvapOutletNode, EngineDrivenChiller( ChillerNum ).CWLoopNum, EngineDrivenChiller( ChillerNum ).CWLoopSideNum, EngineDrivenChiller( ChillerNum ).CWBranchNum, EngineDrivenChiller( ChillerNum ).CWCompNum );
			// Some other component set the flow to 0. No reason to continue with calculations.
			if ( ElectricChiller( ChillerNum ).EvapMassFlowRate == 0.0 ) {
				MyLoad = 0.0;
				if ( EngineDrivenChiller( ChillerNum ).CondenserType == EvapCooled ) {
					CalcBasinHeaterPower( EngineDrivenChiller( ChillerNum ).BasinHeaterPowerFTempDiff, EngineDrivenChiller( ChillerNum ).BasinHeaterSchedulePtr, EngineDrivenChiller( ChillerNum ).BasinHeaterSetPointTemp, BasinHeaterPower );
				}
				EngineDrivenChiller( ChillerNum ).PrintMessage = false;
				return;
			}

			if ( EngineDrivenChiller( ChillerNum ).PossibleSubcooling ) {
				QEvaporator = std::abs( MyLoad );
				EvapDeltaTemp = QEvaporator / ElectricChiller( ChillerNum ).EvapMassFlowRate / Cp;
				ElectricChiller( ChillerNum ).EvapOutletTemp = Node( EvapInletNode ).Temp - EvapDeltaTemp;
				if ( ElectricChiller( ChillerNum ).EvapOutletTemp < Node( EvapOutletNode ).TempMin ) {
					ElectricChiller( ChillerNum ).EvapOutletTemp = Node( EvapOutletNode ).TempMin;
					EvapDeltaTemp = Node( EvapInletNode ).Temp - ElectricChiller( ChillerNum ).EvapOutletTemp;
					QEvaporator = std::abs( ElectricChiller( ChillerNum ).EvapMassFlowRate * Cp * EvapDeltaTemp );
				}
			} else { //No subcooling in this case.No recalculation required.Still need to check chiller low temp limit

				{ auto const SELECT_CASE_var( PlantLoop( LoopNum ).LoopDemandCalcScheme );
				if ( SELECT_CASE_var == SingleSetPoint ) {
					if ( ( EngineDrivenChiller( ChillerNum ).FlowMode == LeavingSetPointModulated ) || ( PlantLoop( LoopNum ).LoopSide( LoopSideNum ).Branch( EngineDrivenChiller( ChillerNum ).CWBranchNum ).Comp( EngineDrivenChiller( ChillerNum ).CWCompNum ).CurOpSchemeType == CompSetPtBasedSchemeType ) || ( Node( EvapOutletNode ).TempSetPoint != SensedNodeFlagValue ) ) {
						TempEvapOutSetPoint = Node( EvapOutletNode ).TempSetPoint;
					} else {
						TempEvapOutSetPoint = Node( PlantLoop( LoopNum ).TempSetPointNodeNum ).TempSetPoint;
					}
				} else if ( SELECT_CASE_var == DualSetPointDeadBand ) {
					if ( ( EngineDrivenChiller( ChillerNum ).FlowMode == LeavingSetPointModulated ) || ( PlantLoop( LoopNum ).LoopSide( LoopSideNum ).Branch( EngineDrivenChiller( ChillerNum ).CWBranchNum ).Comp( EngineDrivenChiller( ChillerNum ).CWCompNum ).CurOpSchemeType == CompSetPtBasedSchemeType ) || ( Node( EvapOutletNode ).TempSetPointHi != SensedNodeFlagValue ) ) {
						TempEvapOutSetPoint = Node( EvapOutletNode ).TempSetPointHi;
					} else {
						TempEvapOutSetPoint = Node( PlantLoop( LoopNum ).TempSetPointNodeNum ).TempSetPointHi;
					}
				}}
				EvapDeltaTemp = Node( EvapInletNode ).Temp - TempEvapOutSetPoint;
				QEvaporator = std::abs( ElectricChiller( ChillerNum ).EvapMassFlowRate * Cp * EvapDeltaTemp );
				ElectricChiller( ChillerNum ).EvapOutletTemp = TempEvapOutSetPoint;
			}
			//Check that the Evap outlet temp honors both plant loop temp low limit and also the chiller low limit
			if ( ElectricChiller( ChillerNum ).EvapOutletTemp < TempLowLimitEout ) {
				if ( ( Node( EvapInletNode ).Temp - TempLowLimitEout ) > DeltaTempTol ) {
					ElectricChiller( ChillerNum ).EvapOutletTemp = TempLowLimitEout;
					EvapDeltaTemp = Node( EvapInletNode ).Temp - ElectricChiller( ChillerNum ).EvapOutletTemp;
					QEvaporator = ElectricChiller( ChillerNum ).EvapMassFlowRate * Cp * EvapDeltaTemp;
				} else {
					ElectricChiller( ChillerNum ).EvapOutletTemp = Node( EvapInletNode ).Temp;
					EvapDeltaTemp = Node( EvapInletNode ).Temp - ElectricChiller( ChillerNum ).EvapOutletTemp;
					QEvaporator = ElectricChiller( ChillerNum ).EvapMassFlowRate * Cp * EvapDeltaTemp;
				}
			}
			if ( ElectricChiller( ChillerNum ).EvapOutletTemp < Node( EvapOutletNode ).TempMin ) {
				if ( ( Node( EvapInletNode ).Temp - Node( EvapOutletNode ).TempMin ) > DeltaTempTol ) {
					ElectricChiller( ChillerNum ).EvapOutletTemp = Node( EvapOutletNode ).TempMin;
					EvapDeltaTemp = Node( EvapInletNode ).Temp - ElectricChiller( ChillerNum ).EvapOutletTemp;
					QEvaporator = ElectricChiller( ChillerNum ).EvapMassFlowRate * Cp * EvapDeltaTemp;
				} else {
					ElectricChiller( ChillerNum ).EvapOutletTemp = Node( EvapInletNode ).Temp;
					EvapDeltaTemp = Node( EvapInletNode ).Temp - ElectricChiller( ChillerNum ).EvapOutletTemp;
					QEvaporator = ElectricChiller( ChillerNum ).EvapMassFlowRate * Cp * EvapDeltaTemp;
				}
			}
			// If load exceeds the distributed load set to the distributed load
			if ( QEvaporator > std::abs( MyLoad ) ) {
				if ( ElectricChiller( ChillerNum ).EvapMassFlowRate > MassFlowTolerance ) {
					QEvaporator = std::abs( MyLoad );
					EvapDeltaTemp = QEvaporator / ElectricChiller( ChillerNum ).EvapMassFlowRate / Cp;
					ElectricChiller( ChillerNum ).EvapOutletTemp = Node( EvapInletNode ).Temp - EvapDeltaTemp;
				} else {
					QEvaporator = 0.0;
					ElectricChiller( ChillerNum ).EvapOutletTemp = Node( EvapInletNode ).Temp;
				}
			}

			// Checks QEvaporator on the basis of the machine limits.
			if ( QEvaporator > ( AvailChillerCap * MaxPartLoadRat ) ) {
				if ( ElectricChiller( ChillerNum ).EvapMassFlowRate > MassFlowTolerance ) {
					QEvaporator = AvailChillerCap * OperPartLoadRat;
					EvapDeltaTemp = QEvaporator / ElectricChiller( ChillerNum ).EvapMassFlowRate / Cp;
					ElectricChiller( ChillerNum ).EvapOutletTemp = Node( EvapInletNode ).Temp - EvapDeltaTemp;
				} else {
					QEvaporator = 0.0;
					ElectricChiller( ChillerNum ).EvapOutletTemp = Node( EvapInletNode ).Temp;
				}
			}

			if ( OperPartLoadRat < MinPartLoadRat ) {
				FRAC = min( 1.0, ( OperPartLoadRat / MinPartLoadRat ) );
			} else {
				FRAC = 1.0;
			}

			// set the module level variable used for reporting FRAC
			ChillerCyclingRatio = FRAC;

			// Chiller is false loading below PLR = minimum unloading ratio, find PLR used for energy calculation
			Power = FracFullLoadPower * FullLoadPowerRat * AvailChillerCap / COP * FRAC;

			if ( ElectricChiller( ChillerNum ).EvapMassFlowRate == 0.0 ) {
				QEvaporator = 0.0;
				ElectricChiller( ChillerNum ).EvapOutletTemp = Node( EvapInletNode ).Temp;
				Power = 0.0;
				EngineDrivenChiller( ChillerNum ).PrintMessage = false;
			}
			if ( QEvaporator == 0.0 && EngineDrivenChiller( ChillerNum ).CondenserType == EvapCooled ) {
				CalcBasinHeaterPower( EngineDrivenChiller( ChillerNum ).BasinHeaterPowerFTempDiff, EngineDrivenChiller( ChillerNum ).BasinHeaterSchedulePtr, EngineDrivenChiller( ChillerNum ).BasinHeaterSetPointTemp, BasinHeaterPower );
			}
		} //This is the end of the FlowLock Block

		//Now determine Cooling
		//QCondenser is calculated the same for each type, but the power consumption should be different
		//  depending on the performance coefficients used for the chiller model.
		QCondenser = Power + QEvaporator;

		if ( EngineDrivenChiller( ChillerNum ).CondenserType == WaterCooled ) {

			if ( ElectricChiller( ChillerNum ).CondMassFlowRate > MassFlowTolerance ) {
				CpCond = GetSpecificHeatGlycol( PlantLoop( EngineDrivenChiller( ChillerNum ).CDLoopNum ).FluidName, CondInletTemp, PlantLoop( EngineDrivenChiller( ChillerNum ).CDLoopNum ).FluidIndex, RoutineName );
				ElectricChiller( ChillerNum ).CondOutletTemp = QCondenser / ElectricChiller( ChillerNum ).CondMassFlowRate / CpCond + CondInletTemp;
			} else {
				ShowSevereError( "CalcEngineDrivenChillerModel: Condenser flow = 0, for EngineDrivenChiller=" + EngineDrivenChiller( ChillerNum ).Name );
				ShowContinueErrorTimeStamp( "" );
			}

		} else { //Air Cooled or Evap Cooled

			//don't care about outlet temp for Air-Cooled or Evap Cooled
			ElectricChiller( ChillerNum ).CondOutletTemp = CondInletTemp;
		}

		// EngineDriven Portion of the Engine Driven Chiller:

		//DETERMINE FUEL CONSUMED AND AVAILABLE WASTE HEAT

		//Use Curve fit to determine Fuel Energy Input.  For electric power generated in Watts, the fuel
		//energy input is calculated in J/s.  The PLBasedFuelInputCurve selects ratio of fuel flow (J/s)/cooling load (J/s).
		if ( PartLoadRat == 0 ) {
			EngineDrivenFuelEnergy = 0.0;
		} else {
			PartLoadRat = max( MinPartLoadRat, PartLoadRat );
			ClngLoadFuelRat = CurveValue( EngineDrivenChiller( ChillerNum ).ClngLoadtoFuelCurve, PartLoadRat );
			EngineDrivenFuelEnergy = QEvaporator / ClngLoadFuelRat;
		}
		//Use Curve fit to determine energy recovered in the water jacket.  This curve calculates the water jacket energy recovered (J/s) by
		//multiplying the total fuel input (J/s) by the fraction of that power that could be recovered in the water jacket at that
		//particular part load.

		RecJacHeattoFuelRat = CurveValue( EngineDrivenChiller( ChillerNum ).RecJacHeattoFuelCurve, PartLoadRat );
		EngineDrivenChiller( ChillerNum ).QJacketRecovered = EngineDrivenFuelEnergy * RecJacHeattoFuelRat;

		//Use Curve fit to determine Heat Recovered Lubricant Energy.  This curve calculates the lube energy recovered (J/s) by
		//multiplying the total fuel input (J/s) by the fraction of that power that could be recovered in the lube oil at that
		//particular part load.
		RecLubeHeattoFuelRat = CurveValue( EngineDrivenChiller( ChillerNum ).RecLubeHeattoFuelCurve, PartLoadRat );
		EngineDrivenChiller( ChillerNum ).QLubeOilRecovered = EngineDrivenFuelEnergy * RecLubeHeattoFuelRat;

		//Use Curve fit to determine Heat Recovered from the exhaust.  This curve calculates the  energy recovered (J/s) by
		//multiplying the total fuel input (J/s) by the fraction of that power that could be recovered in the exhaust at that
		//particular part load.
		TotExhausttoFuelRat = CurveValue( EngineDrivenChiller( ChillerNum ).TotExhausttoFuelCurve, PartLoadRat );
		TotalExhaustEnergy = EngineDrivenFuelEnergy * TotExhausttoFuelRat;

		//Use Curve fit to determine Exhaust Temperature in C.  The temperature is simply a curve fit
		//of the exhaust temperature in C to the part load ratio.
		if ( PartLoadRat != 0 ) {
			ExhaustTemp = CurveValue( EngineDrivenChiller( ChillerNum ).ExhaustTempCurve, PartLoadRat );
			ExhaustGasFlow = TotalExhaustEnergy / ( ExhaustCP * ( ExhaustTemp - ReferenceTemp ) );

			//Use Curve fit to determine stack temp after heat recovery
			UA = EngineDrivenChiller( ChillerNum ).UACoef( 1 ) * std::pow( ChillerNomCap, EngineDrivenChiller( ChillerNum ).UACoef( 2 ) );

			DesignMinExitGasTemp = EngineDrivenChiller( ChillerNum ).DesignMinExitGasTemp;
			EngineDrivenChiller( ChillerNum ).ExhaustStackTemp = DesignMinExitGasTemp + ( ExhaustTemp - DesignMinExitGasTemp ) / std::exp( UA / ( max( ExhaustGasFlow, MaxExhaustperPowerOutput * ChillerNomCap ) * ExhaustCP ) );

			EngineDrivenChiller( ChillerNum ).QExhaustRecovered = max( ExhaustGasFlow * ExhaustCP * ( ExhaustTemp - EngineDrivenChiller( ChillerNum ).ExhaustStackTemp ), 0.0 );
		} else {
			EngineDrivenChiller( ChillerNum ).QExhaustRecovered = 0.0;
		}

		EngineDrivenChiller( ChillerNum ).QTotalHeatRecovered = EngineDrivenChiller( ChillerNum ).QExhaustRecovered + EngineDrivenChiller( ChillerNum ).QLubeOilRecovered + EngineDrivenChiller( ChillerNum ).QJacketRecovered;

		//Update Heat Recovery temperatures
		if ( EngineDrivenChiller( ChillerNum ).HeatRecActive ) {
			CalcEngineChillerHeatRec( ChillerNum, EngineDrivenChiller( ChillerNum ).QTotalHeatRecovered, HeatRecRatio );
			EngineDrivenChiller( ChillerNum ).QExhaustRecovered *= HeatRecRatio;
			EngineDrivenChiller( ChillerNum ).QLubeOilRecovered *= HeatRecRatio;
			EngineDrivenChiller( ChillerNum ).QJacketRecovered *= HeatRecRatio;

		}

		//Calculate Energy
		CondenserEnergy = QCondenser * TimeStepSys * SecInHour;
		Energy = Power * TimeStepSys * SecInHour;
		EvaporatorEnergy = QEvaporator * TimeStepSys * SecInHour;
		EngineDrivenChiller( ChillerNum ).FuelEnergyUseRate = EngineDrivenFuelEnergy;
		EngineDrivenChiller( ChillerNum ).FuelEnergy = EngineDrivenChiller( ChillerNum ).FuelEnergyUseRate * TimeStepSys * SecInHour;
		EngineDrivenChiller( ChillerNum ).JacketEnergyRec = EngineDrivenChiller( ChillerNum ).QJacketRecovered * TimeStepSys * SecInHour;
		EngineDrivenChiller( ChillerNum ).LubeOilEnergyRec = EngineDrivenChiller( ChillerNum ).QLubeOilRecovered * TimeStepSys * SecInHour;
		EngineDrivenChiller( ChillerNum ).ExhaustEnergyRec = EngineDrivenChiller( ChillerNum ).QExhaustRecovered * TimeStepSys * SecInHour;
		EngineDrivenChiller( ChillerNum ).QTotalHeatRecovered = EngineDrivenChiller( ChillerNum ).QExhaustRecovered + EngineDrivenChiller( ChillerNum ).QLubeOilRecovered + EngineDrivenChiller( ChillerNum ).QJacketRecovered;
		EngineDrivenChiller( ChillerNum ).TotalHeatEnergyRec = EngineDrivenChiller( ChillerNum ).ExhaustEnergyRec + EngineDrivenChiller( ChillerNum ).LubeOilEnergyRec + EngineDrivenChiller( ChillerNum ).JacketEnergyRec;
		EngineDrivenChiller( ChillerNum ).FuelEnergyUseRate = std::abs( EngineDrivenChiller( ChillerNum ).FuelEnergyUseRate );
		EngineDrivenChiller( ChillerNum ).FuelEnergy = std::abs( EngineDrivenChiller( ChillerNum ).FuelEnergy );
		EngineDrivenChiller( ChillerNum ).FuelMdot = std::abs( EngineDrivenChiller( ChillerNum ).FuelEnergyUseRate ) / ( EngineDrivenChiller( ChillerNum ).FuelHeatingValue * KJtoJ );

		//check for problems BG 9/12/06 (deal with observed negative energy results)
		if ( Energy < 0.0 ) { // there is a serious problem
			if ( EngineDrivenChiller( ChillerNum ).CondenserType == WaterCooled ) {
				// first check for run away condenser loop temps (only reason yet to be observed for this?)
				if ( CondInletTemp > 70.0 ) {
					ShowSevereError( "CalcEngineDrivenChillerModel: Condenser loop inlet temperatures > 70.0 C for EngineDrivenChiller=" + EngineDrivenChiller( ChillerNum ).Name );
					ShowContinueErrorTimeStamp( "" );
					ShowContinueError( "Condenser loop water temperatures are too high at" + RoundSigDigits( CondInletTemp, 2 ) );
					ShowContinueError( "Check input for condenser plant loop, especially cooling tower" );
					ShowContinueError( "Evaporator inlet temperature: " + RoundSigDigits( Node( EvapInletNode ).Temp, 2 ) );

					ShowFatalError( "Program Terminates due to previous error condition" );
				}
			}
			if ( ! WarmupFlag ) {
				if ( AvailNomCapRat < 0.0 ) { // apparently the real reason energy goes negative
					ShowSevereError( "CalcEngineDrivenChillerModel: Capacity ratio below zero for EngineDrivenChiller=" + EngineDrivenChiller( ChillerNum ).Name );
					ShowContinueErrorTimeStamp( "" );
					ShowContinueError( "Check input for Capacity Ratio Curve" );
					ShowContinueError( "Condenser inlet temperature: " + RoundSigDigits( CondInletTemp, 2 ) );
					ShowContinueError( "Evaporator inlet temperature: " + RoundSigDigits( Node( EvapInletNode ).Temp, 2 ) );
					ShowFatalError( "Program Terminates due to previous error condition" );
				}
			}
			// If makes it here, set limits, chiller can't have negative energy/power
			// proceeding silently for now but may want to throw error here
			Power = 0.0;
			Energy = 0.0;
		}

	}

	void
	CalcGTChillerModel(
		int & ChillerNum, // chiller number
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
		GTChiller( ChillerNum ).EvapMassFlowRate = 0.0;
		GTChiller( ChillerNum ).CondMassFlowRate = 0.0;
		Power = 0.0;
		QCondenser = 0.0;
		QEvaporator = 0.0;
		Energy = 0.0;
		CondenserEnergy = 0.0;
		EvaporatorEnergy = 0.0;
		EvapInletNode = GTChiller( ChillerNum ).EvapInletNodeNum;
		EvapOutletNode = GTChiller( ChillerNum ).EvapOutletNodeNum;
		CondInletNode = GTChiller( ChillerNum ).CondInletNodeNum;
		CondOutletNode = GTChiller( ChillerNum ).CondOutletNodeNum;
		HeatRecInNode = GTChiller( ChillerNum ).HeatRecInletNodeNum;
		HeatRecOutNode = GTChiller( ChillerNum ).HeatRecOutletNodeNum;
		QHeatRecLube = 0.0;
		FRAC = 1.0;
		LoopNum = GTChiller( ChillerNum ).CWLoopNum;
		LoopSideNum = GTChiller( ChillerNum ).CWLoopSideNum;
		EvapInletTemp = Node( EvapInletNode ).Temp;

		// calculate end time of current time step
		CurrentEndTime = CurrentTime + SysTimeElapsed;

		// Print warning messages only when valid and only for the first ocurrance. Let summary provide statistics.
		// Wait for next time step to print warnings. If simulation iterates, print out
		// the warning for the last iteration only. Must wait for next time step to accomplish this.
		// If a warning occurs and the simulation down shifts, the warning is not valid.
		if ( CurrentEndTime > CurrentEndTimeLast && TimeStepSys >= TimeStepSysLast ) {
			if ( GTChiller( ChillerNum ).PrintMessage ) {
				++GTChiller( ChillerNum ).MsgErrorCount;
				// Show single warning and pass additional info to ShowRecurringWarningErrorAtEnd
				if ( GTChiller( ChillerNum ).MsgErrorCount < 2 ) {
					ShowWarningError( GTChiller( ChillerNum ).MsgBuffer1 + '.' );
					ShowContinueError( GTChiller( ChillerNum ).MsgBuffer2 );
				} else {
					ShowRecurringWarningErrorAtEnd( GTChiller( ChillerNum ).MsgBuffer1 + " error continues.", GTChiller( ChillerNum ).ErrCount1, GTChiller( ChillerNum ).MsgDataLast, GTChiller( ChillerNum ).MsgDataLast, _, "[C]", "[C]" );
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
				GTChiller( ChillerNum ).EvapMassFlowRate = Node( EvapInletNode ).MassFlowRate;
			} else {
				GTChiller( ChillerNum ).EvapMassFlowRate = 0.0;

				SetComponentFlowRate( GTChiller( ChillerNum ).EvapMassFlowRate, EvapInletNode, EvapOutletNode, GTChiller( ChillerNum ).CWLoopNum, GTChiller( ChillerNum ).CWLoopSideNum, GTChiller( ChillerNum ).CWBranchNum, GTChiller( ChillerNum ).CWCompNum );
			}
			if ( GTChiller( ChillerNum ).CondenserType == WaterCooled ) {
				if ( PlantLoop( GTChiller( ChillerNum ).CDLoopNum ).LoopSide( GTChiller( ChillerNum ).CDLoopSideNum ).Branch( GTChiller( ChillerNum ).CDBranchNum ).Comp( GTChiller( ChillerNum ).CDCompNum ).FlowCtrl == ControlType_SeriesActive ) {
					GTChiller( ChillerNum ).CondMassFlowRate = Node( CondInletNode ).MassFlowRate;
				} else {
					GTChiller( ChillerNum ).CondMassFlowRate = 0.0;
					SetComponentFlowRate( GTChiller( ChillerNum ).CondMassFlowRate, CondInletNode, CondOutletNode, GTChiller( ChillerNum ).CDLoopNum, GTChiller( ChillerNum ).CDLoopSideNum, GTChiller( ChillerNum ).CDBranchNum, GTChiller( ChillerNum ).CDCompNum );
				}
			}

			if ( GTChiller( ChillerNum ).CondenserType == EvapCooled ) {
				CalcBasinHeaterPower( GTChiller( ChillerNum ).BasinHeaterPowerFTempDiff, GTChiller( ChillerNum ).BasinHeaterSchedulePtr, GTChiller( ChillerNum ).BasinHeaterSetPointTemp, BasinHeaterPower );
			}
			GTChiller( ChillerNum ).PrintMessage = false;
			return;
		}

		if ( GTChiller( ChillerNum ).CondenserType == AirCooled ) { //Condenser inlet temp = outdoor temp
			Node( CondInletNode ).Temp = Node( CondInletNode ).OutAirDryBulb;
			//  Warn user if entering condenser temperature falls below 0C
			if ( Node( CondInletNode ).Temp < 0.0 && ! WarmupFlag ) {
				GTChiller( ChillerNum ).PrintMessage = true;
				gio::write( OutputChar, OutputFormat ) << Node( CondInletNode ).Temp;
				GTChiller( ChillerNum ).MsgBuffer1 = "CalcGasTurbineChillerModel - Chiller:CombustionTurbine \"" + GTChiller( ChillerNum ).Name + "\" - Air Cooled Condenser Inlet Temperature below 0C";
				GTChiller( ChillerNum ).MsgBuffer2 = "... Outdoor Dry-bulb Condition = " + OutputChar + " C. Occurrence info = " + EnvironmentName + ", " + CurMnDy + ' ' + CreateSysTimeIntervalString();
				GTChiller( ChillerNum ).MsgDataLast = Node( CondInletNode ).Temp;
			} else {
				GTChiller( ChillerNum ).PrintMessage = false;
			}
		} else if ( GTChiller( ChillerNum ).CondenserType == EvapCooled ) { //Condenser inlet temp = (outdoor wet bulb)
			Node( CondInletNode ).Temp = Node( CondInletNode ).OutAirWetBulb;
			//  Warn user if evap condenser wet bulb temperature falls below 10C
			if ( Node( CondInletNode ).Temp < 10.0 && ! WarmupFlag ) {
				GTChiller( ChillerNum ).PrintMessage = true;
				gio::write( OutputChar, OutputFormat ) << Node( CondInletNode ).Temp;
				GTChiller( ChillerNum ).MsgBuffer1 = "CalcGasTurbineChillerModel - Chiller:CombustionTurbine \"" + GTChiller( ChillerNum ).Name + "\" - Evap Cooled Condenser Inlet Temperature below 10C";
				GTChiller( ChillerNum ).MsgBuffer2 = "... Outdoor Wet-bulb Condition = " + OutputChar + " C. Occurrence info = " + EnvironmentName + ", " + CurMnDy + ' ' + CreateSysTimeIntervalString();
				GTChiller( ChillerNum ).MsgDataLast = Node( CondInletNode ).Temp;
			} else {
				GTChiller( ChillerNum ).PrintMessage = false;
			}
		} // End of the Air Cooled/Evap Cooled Logic block

		// If not air or evap cooled then set to the condenser node that is attached to a cooling tower
		CondInletTemp = Node( CondInletNode ).Temp;

		//Set mass flow rates
		if ( GTChiller( ChillerNum ).CondenserType == WaterCooled ) {
			GTChiller( ChillerNum ).CondMassFlowRate = GTChiller( ChillerNum ).CondMassFlowRateMax;
			SetComponentFlowRate( GTChiller( ChillerNum ).CondMassFlowRate, CondInletNode, CondOutletNode, GTChiller( ChillerNum ).CDLoopNum, GTChiller( ChillerNum ).CDLoopSideNum, GTChiller( ChillerNum ).CDBranchNum, GTChiller( ChillerNum ).CDCompNum );
			PullCompInterconnectTrigger( GTChiller( ChillerNum ).CWLoopNum, GTChiller( ChillerNum ).CWLoopSideNum, GTChiller( ChillerNum ).CWBranchNum, GTChiller( ChillerNum ).CWCompNum, GTChiller( ChillerNum ).CondMassFlowIndex, GTChiller( ChillerNum ).CDLoopNum, GTChiller( ChillerNum ).CDLoopSideNum, CriteriaType_MassFlowRate, GTChiller( ChillerNum ).CondMassFlowRate );

			if ( GTChiller( ChillerNum ).CondMassFlowRate < MassFlowTolerance ) return;

		}

		//  LOAD LOCAL VARIABLES FROM DATA STRUCTURE (for code readability)
		auto const & CapacityRat( GTChiller( ChillerNum ).CapRatCoef );
		auto const & PowerRat( GTChiller( ChillerNum ).PowerRatCoef );
		auto const & FullLoadFactor( GTChiller( ChillerNum ).FullLoadCoef );
		MinPartLoadRat = GTChiller( ChillerNum ).MinPartLoadRat;
		MaxPartLoadRat = GTChiller( ChillerNum ).MaxPartLoadRat;
		TempCondInDesign = GTChiller( ChillerNum ).TempDesCondIn;
		TempRiseRat = GTChiller( ChillerNum ).TempRiseCoef;
		TempEvapOutDesign = GTChiller( ChillerNum ).TempDesEvapOut;
		ChillerNomCap = GTChiller( ChillerNum ).NomCap;
		COP = GTChiller( ChillerNum ).COP;
		TempCondIn = Node( GTChiller( ChillerNum ).CondInletNodeNum ).Temp;
		TempEvapOut = Node( GTChiller( ChillerNum ).EvapOutletNodeNum ).Temp;
		TempLowLimitEout = GTChiller( ChillerNum ).TempLowLimitEvapOut;
		EvapMassFlowRateMax = GTChiller( ChillerNum ).EvapMassFlowRateMax;
		LoopNum = GTChiller( ChillerNum ).CWLoopNum;
		LoopSideNum = GTChiller( ChillerNum ).CWLoopSideNum;

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
		Cp = GetSpecificHeatGlycol( PlantLoop( GTChiller( ChillerNum ).CWLoopNum ).FluidName, Node( EvapInletNode ).Temp, PlantLoop( GTChiller( ChillerNum ).CWLoopNum ).FluidIndex, RoutineName );
		// If FlowLock is True, the new resolved mdot is used to update Power, QEvap, Qcond, and
		// condenser side outlet temperature.
		if ( PlantLoop( LoopNum ).LoopSide( LoopSideNum ).FlowLock == 0 ) {
			GTChiller( ChillerNum ).PossibleSubcooling = false;
			QEvaporator = AvailChillerCap * OperPartLoadRat;
			if ( OperPartLoadRat < MinPartLoadRat ) {
				FRAC = min( 1.0, ( OperPartLoadRat / MinPartLoadRat ) );
			} else {
				FRAC = 1.0;
			}
			Power = FracFullLoadPower * FullLoadPowerRat * AvailChillerCap / COP * FRAC;

			// Either set the flow to the Constant value or caluclate the flow for the variable volume
			if ( ( GTChiller( ChillerNum ).FlowMode == ConstantFlow ) || ( GTChiller( ChillerNum ).FlowMode == NotModulated ) ) {
				// Start by assuming max (design) flow
				GTChiller( ChillerNum ).EvapMassFlowRate = EvapMassFlowRateMax;
				// Use SetComponentFlowRate to decide actual flow
				SetComponentFlowRate( GTChiller( ChillerNum ).EvapMassFlowRate, EvapInletNode, EvapOutletNode, GTChiller( ChillerNum ).CWLoopNum, GTChiller( ChillerNum ).CWLoopSideNum, GTChiller( ChillerNum ).CWBranchNum, GTChiller( ChillerNum ).CWCompNum );
				// Evaluate delta temp based on actual flow rate
				if ( GTChiller( ChillerNum ).EvapMassFlowRate != 0.0 ) {
					EvapDeltaTemp = QEvaporator / GTChiller( ChillerNum ).EvapMassFlowRate / Cp;
				} else {
					EvapDeltaTemp = 0.0;
				}
				// Evaluate outlet temp based on delta
				GTChiller( ChillerNum ).EvapOutletTemp = Node( EvapInletNode ).Temp - EvapDeltaTemp;
			} else if ( GTChiller( ChillerNum ).FlowMode == LeavingSetPointModulated ) {
				// Calculate the Delta Temp from the inlet temp to the chiller outlet setpoint
				{ auto const SELECT_CASE_var( PlantLoop( GTChiller( ChillerNum ).CWLoopNum ).LoopDemandCalcScheme );
				if ( SELECT_CASE_var == SingleSetPoint ) {
					EvapDeltaTemp = Node( EvapInletNode ).Temp - Node( EvapOutletNode ).TempSetPoint;
				} else if ( SELECT_CASE_var == DualSetPointDeadBand ) {
					EvapDeltaTemp = Node( EvapInletNode ).Temp - Node( EvapOutletNode ).TempSetPointHi;
				}}
				if ( EvapDeltaTemp != 0.0 ) {
					// Calculate desired flow to request based on load
					GTChiller( ChillerNum ).EvapMassFlowRate = std::abs( QEvaporator / Cp / EvapDeltaTemp );
					if ( ( GTChiller( ChillerNum ).EvapMassFlowRate - EvapMassFlowRateMax ) > MassFlowTolerance ) GTChiller( ChillerNum ).PossibleSubcooling = true;
					//Check to see if the Maximum is exceeded, if so set to maximum
					GTChiller( ChillerNum ).EvapMassFlowRate = min( EvapMassFlowRateMax, GTChiller( ChillerNum ).EvapMassFlowRate );
					// Use SetComponentFlowRate to decide actual flow
					SetComponentFlowRate( GTChiller( ChillerNum ).EvapMassFlowRate, EvapInletNode, EvapOutletNode, GTChiller( ChillerNum ).CWLoopNum, GTChiller( ChillerNum ).CWLoopSideNum, GTChiller( ChillerNum ).CWBranchNum, GTChiller( ChillerNum ).CWCompNum );
					{ auto const SELECT_CASE_var( PlantLoop( GTChiller( ChillerNum ).CWLoopNum ).LoopDemandCalcScheme );
					if ( SELECT_CASE_var == SingleSetPoint ) {
						GTChiller( ChillerNum ).EvapOutletTemp = Node( EvapOutletNode ).TempSetPoint;
					} else if ( SELECT_CASE_var == DualSetPointDeadBand ) {
						GTChiller( ChillerNum ).EvapOutletTemp = Node( EvapOutletNode ).TempSetPointHi;
					}}
				} else {
					// Try to request zero flow
					GTChiller( ChillerNum ).EvapMassFlowRate = 0.0;
					// Use SetComponentFlowRate to decide actual flow
					SetComponentFlowRate( GTChiller( ChillerNum ).EvapMassFlowRate, EvapInletNode, EvapOutletNode, GTChiller( ChillerNum ).CWLoopNum, GTChiller( ChillerNum ).CWLoopSideNum, GTChiller( ChillerNum ).CWBranchNum, GTChiller( ChillerNum ).CWCompNum );
					// No deltaT since component is not running
					GTChiller( ChillerNum ).EvapOutletTemp = Node( EvapInletNode ).Temp;

				}
			} //End of Constant Variable Flow If Block
		} else { // If FlowLock is True

			GTChiller( ChillerNum ).EvapMassFlowRate = Node( EvapInletNode ).MassFlowRate;
			SetComponentFlowRate( GTChiller( ChillerNum ).EvapMassFlowRate, EvapInletNode, EvapOutletNode, GTChiller( ChillerNum ).CWLoopNum, GTChiller( ChillerNum ).CWLoopSideNum, GTChiller( ChillerNum ).CWBranchNum, GTChiller( ChillerNum ).CWCompNum );
			//       Some other component set the flow to 0. No reason to continue with calculations.
			if ( GTChiller( ChillerNum ).EvapMassFlowRate == 0.0 ) {
				MyLoad = 0.0;
				if ( GTChiller( ChillerNum ).CondenserType == EvapCooled ) {
					CalcBasinHeaterPower( GTChiller( ChillerNum ).BasinHeaterPowerFTempDiff, GTChiller( ChillerNum ).BasinHeaterSchedulePtr, GTChiller( ChillerNum ).BasinHeaterSetPointTemp, BasinHeaterPower );
				}
				GTChiller( ChillerNum ).PrintMessage = false;
				return;
			}

			if ( GTChiller( ChillerNum ).PossibleSubcooling ) {
				QEvaporator = std::abs( MyLoad );
				EvapDeltaTemp = QEvaporator / GTChiller( ChillerNum ).EvapMassFlowRate / Cp;
				GTChiller( ChillerNum ).EvapOutletTemp = Node( EvapInletNode ).Temp - EvapDeltaTemp;
			} else { //No subcooling in this case.No recalculation required.Still need to check chiller low temp limit
				{ auto const SELECT_CASE_var( PlantLoop( LoopNum ).LoopDemandCalcScheme );
				if ( SELECT_CASE_var == SingleSetPoint ) {
					if ( ( GTChiller( ChillerNum ).FlowMode == LeavingSetPointModulated ) || ( PlantLoop( LoopNum ).LoopSide( LoopSideNum ).Branch( GTChiller( ChillerNum ).CWBranchNum ).Comp( GTChiller( ChillerNum ).CWCompNum ).CurOpSchemeType == CompSetPtBasedSchemeType ) || ( Node( EvapOutletNode ).TempSetPoint != SensedNodeFlagValue ) ) {
						TempEvapOutSetPoint = Node( EvapOutletNode ).TempSetPoint;
					} else {
						TempEvapOutSetPoint = Node( PlantLoop( LoopNum ).TempSetPointNodeNum ).TempSetPoint;
					}
				} else if ( SELECT_CASE_var == DualSetPointDeadBand ) {
					if ( ( GTChiller( ChillerNum ).FlowMode == LeavingSetPointModulated ) || ( PlantLoop( LoopNum ).LoopSide( LoopSideNum ).Branch( GTChiller( ChillerNum ).CWBranchNum ).Comp( GTChiller( ChillerNum ).CWCompNum ).CurOpSchemeType == CompSetPtBasedSchemeType ) || ( Node( EvapOutletNode ).TempSetPointHi != SensedNodeFlagValue ) ) {
						TempEvapOutSetPoint = Node( EvapOutletNode ).TempSetPointHi;
					} else {
						TempEvapOutSetPoint = Node( PlantLoop( LoopNum ).TempSetPointNodeNum ).TempSetPointHi;
					}
				}}
				EvapDeltaTemp = Node( EvapInletNode ).Temp - TempEvapOutSetPoint;
				QEvaporator = std::abs( GTChiller( ChillerNum ).EvapMassFlowRate * Cp * EvapDeltaTemp );
				GTChiller( ChillerNum ).EvapOutletTemp = TempEvapOutSetPoint;
			}
			//Check that the Evap outlet temp honors both plant loop temp low limit and also the chiller low limit
			if ( GTChiller( ChillerNum ).EvapOutletTemp < TempLowLimitEout ) {
				if ( ( Node( EvapInletNode ).Temp - TempLowLimitEout ) > DeltaTempTol ) {
					GTChiller( ChillerNum ).EvapOutletTemp = TempLowLimitEout;
					EvapDeltaTemp = Node( EvapInletNode ).Temp - GTChiller( ChillerNum ).EvapOutletTemp;
					QEvaporator = GTChiller( ChillerNum ).EvapMassFlowRate * Cp * EvapDeltaTemp;
				} else {
					GTChiller( ChillerNum ).EvapOutletTemp = Node( EvapInletNode ).Temp;
					EvapDeltaTemp = Node( EvapInletNode ).Temp - GTChiller( ChillerNum ).EvapOutletTemp;
					QEvaporator = GTChiller( ChillerNum ).EvapMassFlowRate * Cp * EvapDeltaTemp;
				}
			}
			if ( GTChiller( ChillerNum ).EvapOutletTemp < Node( EvapOutletNode ).TempMin ) {
				if ( ( Node( EvapInletNode ).Temp - Node( EvapOutletNode ).TempMin ) > DeltaTempTol ) {
					GTChiller( ChillerNum ).EvapOutletTemp = Node( EvapOutletNode ).TempMin;
					EvapDeltaTemp = Node( EvapInletNode ).Temp - GTChiller( ChillerNum ).EvapOutletTemp;
					QEvaporator = GTChiller( ChillerNum ).EvapMassFlowRate * Cp * EvapDeltaTemp;
				} else {
					GTChiller( ChillerNum ).EvapOutletTemp = Node( EvapInletNode ).Temp;
					EvapDeltaTemp = Node( EvapInletNode ).Temp - GTChiller( ChillerNum ).EvapOutletTemp;
					QEvaporator = GTChiller( ChillerNum ).EvapMassFlowRate * Cp * EvapDeltaTemp;
				}
			}
			// If load exceeds the distributed load set to the distributed load
			if ( QEvaporator > std::abs( MyLoad ) ) {
				if ( GTChiller( ChillerNum ).EvapMassFlowRate > MassFlowTolerance ) {
					QEvaporator = std::abs( MyLoad );
					EvapDeltaTemp = QEvaporator / GTChiller( ChillerNum ).EvapMassFlowRate / Cp;
					GTChiller( ChillerNum ).EvapOutletTemp = Node( EvapInletNode ).Temp - EvapDeltaTemp;
				} else {
					QEvaporator = 0.0;
					GTChiller( ChillerNum ).EvapOutletTemp = Node( EvapInletNode ).Temp;
				}
			}

			// Checks QEvaporator on the basis of the machine limits.
			if ( QEvaporator > ( AvailChillerCap * MaxPartLoadRat ) ) {
				if ( GTChiller( ChillerNum ).EvapMassFlowRate > MassFlowTolerance ) {
					QEvaporator = AvailChillerCap * PartLoadRat;
					EvapDeltaTemp = QEvaporator / GTChiller( ChillerNum ).EvapMassFlowRate / Cp;
					GTChiller( ChillerNum ).EvapOutletTemp = Node( EvapInletNode ).Temp - EvapDeltaTemp;
				} else {
					QEvaporator = 0.0;
					GTChiller( ChillerNum ).EvapOutletTemp = Node( EvapInletNode ).Temp;
				}
			}

			if ( OperPartLoadRat < MinPartLoadRat ) {
				FRAC = min( 1.0, ( OperPartLoadRat / MinPartLoadRat ) );
			} else {
				FRAC = 1.0;
			}

			// set the module level variable used for reporting FRAC
			ChillerCyclingRatio = FRAC;

			// Chiller is false loading below PLR = minimum unloading ratio, find PLR used for energy calculation
			Power = FracFullLoadPower * FullLoadPowerRat * AvailChillerCap / COP * FRAC;

			if ( GTChiller( ChillerNum ).EvapMassFlowRate == 0.0 ) {
				QEvaporator = 0.0;
				GTChiller( ChillerNum ).EvapOutletTemp = Node( EvapInletNode ).Temp;
				Power = 0.0;
				GTChiller( ChillerNum ).PrintMessage = false;
			}
			if ( QEvaporator == 0.0 && GTChiller( ChillerNum ).CondenserType == EvapCooled ) {
				CalcBasinHeaterPower( GTChiller( ChillerNum ).BasinHeaterPowerFTempDiff, GTChiller( ChillerNum ).BasinHeaterSchedulePtr, GTChiller( ChillerNum ).BasinHeaterSetPointTemp, BasinHeaterPower );
			}

		} //This is the end of the FlowLock Block

		//Now determine Cooling
		//QCondenser is calculated the same for each type, but the power consumption should be different
		//  depending on the performance coefficients used for the chiller model.
		QCondenser = Power + QEvaporator;

		if ( GTChiller( ChillerNum ).CondenserType == WaterCooled ) {

			if ( GTChiller( ChillerNum ).CondMassFlowRate > MassFlowTolerance ) {
				CpCond = GetSpecificHeatGlycol( PlantLoop( GTChiller( ChillerNum ).CDLoopNum ).FluidName, CondInletTemp, PlantLoop( GTChiller( ChillerNum ).CDLoopNum ).FluidIndex, RoutineName );
				GTChiller( ChillerNum ).CondOutletTemp = QCondenser / GTChiller( ChillerNum ).CondMassFlowRate / CpCond + CondInletTemp;
			} else {
				ShowSevereError( "CalcGasTurbineChillerModel: Condenser flow = 0, for GasTurbineChiller=" + GTChiller( ChillerNum ).Name );
				ShowContinueErrorTimeStamp( "" );

			}

		} else { //Air Cooled or Evap Cooled

			//don't care about outlet temp for Air-Cooled or Evap Cooled and there is no CondMassFlowRate and would divide by zero
			GTChiller( ChillerNum ).CondOutletTemp = CondInletTemp;
		}

		//Special GT Chiller Variables
		// Gas Turbine Driven Portion of the Chiller:

		GTEngineCapacity = GTChiller( ChillerNum ).GTEngineCapacity;
		MaxExhaustperGTPower = GTChiller( ChillerNum ).MaxExhaustperGTPower;

		//Note: All Old Blast Code comments begin at left.

		//D                                   COMPUTE TOWER CLOAD
		//               ETOWER(TypeIndex) = PREQD + CHLRLOAD(TypeIndex)
		//               RPLOAD = PREQD/CHLROCAP(TypeIndex)
		//               IF (RFLAGS(81)) WRITE (OUTPUT,703) PREQD,ETOWER(TypeIndex),RPLOAD
		//               IF (PREQD .GT. 0.0d0) THEN
		if ( AvailChillerCap > 0 ) {
			RPLoad = Power / AvailChillerCap;
		} else {
			RPLoad = 0.0;
		}

		if ( Power > 0 ) {
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

			if ( GTChiller( ChillerNum ).CondenserType == WaterCooled ) {
				AmbientDeltaT = OutDryBulbTemp - 25.0;
			} else { // air or evap cooled
				AmbientDeltaT = Node( CondInletNode ).OutAirDryBulb - 25.0;
			}

			//                        EFUEL=PLOAD*(FUL1GC(1,IPLCTR)+FUL1GC(2,IPLCTR)*  &
			//                              RLOAD+FUL1GC(3,IPLCTR)*RLD2)*              &
			//                              (FUL2GC(1,IPLCTR)+FUL2GC(2,IPLCTR)*ATAIR+  &
			//                              FUL2GC(3,IPLCTR)*TAR2)

			FuelEnergyIn = PLoad * ( GTChiller( ChillerNum ).PLBasedFuelInputCoef( 1 ) + GTChiller( ChillerNum ).PLBasedFuelInputCoef( 2 ) * RL + GTChiller( ChillerNum ).PLBasedFuelInputCoef( 3 ) * RL2 ) * ( GTChiller( ChillerNum ).TempBasedFuelInputCoef( 1 ) + GTChiller( ChillerNum ).TempBasedFuelInputCoef( 2 ) * AmbientDeltaT + GTChiller( ChillerNum ).TempBasedFuelInputCoef( 3 ) * pow_2( AmbientDeltaT ) );

			//                        FEX=GTDSLCAP(IS,TypeIndex,IPLCTR)*(FEXGC(1,IPLCTR)+      &
			//                            FEXGC(2,IPLCTR)*ATAIR+FEXGC(3,IPLCTR)*TAR2)

			ExhaustFlow = GTEngineCapacity * ( GTChiller( ChillerNum ).ExhaustFlowCoef( 1 ) + GTChiller( ChillerNum ).ExhaustFlowCoef( 2 ) * AmbientDeltaT + GTChiller( ChillerNum ).ExhaustFlowCoef( 3 ) * pow_2( AmbientDeltaT ) );

			//                        TEX=(TEX1GC(1,IPLCTR)+TEX1GC(2,IPLCTR)*RLOAD+    &
			//                            TEX1GC(3,IPLCTR)*RLD2)*(TEX2GC(1,IPLCTR)+    &
			//                            TEX2GC(2,IPLCTR)*ATAIR+TEX2GC(3,IPLCTR)*     &
			//                            TAR2)-273.

			ExhaustTemp = ( GTChiller( ChillerNum ).PLBasedExhaustTempCoef( 1 ) + GTChiller( ChillerNum ).PLBasedExhaustTempCoef( 2 ) * RL + GTChiller( ChillerNum ).PLBasedExhaustTempCoef( 3 ) * RL2 ) * ( GTChiller( ChillerNum ).TempBasedExhaustTempCoef( 1 ) + GTChiller( ChillerNum ).TempBasedExhaustTempCoef( 2 ) * AmbientDeltaT + GTChiller( ChillerNum ).TempBasedExhaustTempCoef( 3 ) * pow_2( AmbientDeltaT ) ) - 273;

			//                        UAG=UACGC(1,IPLCTR)*GTDSLCAP(IS,TypeIndex,IPLCTR)**      &
			//                            UACGC(2,IPLCTR)
			if ( PLoad != 0.0 ) {
				UAtoCapRat = GTChiller( ChillerNum ).UAtoCapCoef( 1 ) * std::pow( GTEngineCapacity, GTChiller( ChillerNum ).UAtoCapCoef( 2 ) );

				//     TSTACK = EXHAUST STACK TEMPERATURE, C.
				//                        TSTACK=TSATUR(IPLCTR)+(TEX-TSATUR(IPLCTR))/      &
				//                               EXP(UAG/(AMAX1(FEX,RMXKGC(IPLCTR)*        &
				//                               GTDSLCAP(IS,TypeIndex,IPLCTR)) * 1.047))

				DesignSteamSatTemp = GTChiller( ChillerNum ).DesignSteamSatTemp;
				ExhaustStackTemp = DesignSteamSatTemp + ( ExhaustTemp - DesignSteamSatTemp ) / std::exp( UAtoCapRat / ( max( ExhaustFlow, MaxExhaustperGTPower * GTEngineCapacity ) * ExhaustCP ) );

				//                        EEX = AMAX1 ( FEX*1.047*(TEX-TSTACK),0.0d0)
				//                        ELUBE=PLOAD*(ELUBEGC(1,IPLCTR)+ELUBEGC(2,IPLCTR) &
				//                              *RLOAD+ELUBEGC(3,IPLCTR)*RLD2 )
			}

			if ( GTChiller( ChillerNum ).HeatRecActive ) {
				QHeatRecLube = PLoad * ( GTChiller( ChillerNum ).HeatRecLubeEnergyCoef( 1 ) + GTChiller( ChillerNum ).HeatRecLubeEnergyCoef( 2 ) * RL + GTChiller( ChillerNum ).HeatRecLubeEnergyCoef( 3 ) * RL2 );

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

			if ( GTChiller( ChillerNum ).HeatRecActive ) {
				//This mdot is input specified mdot "Desired Flowrate", already set at node in init routine
				HeatRecMdot = Node( HeatRecInNode ).MassFlowRate;
				HeatRecInTemp = Node( HeatRecInNode ).Temp;
				HeatRecCp = GetSpecificHeatGlycol( PlantLoop( GTChiller( ChillerNum ).HRLoopNum ).FluidName, HeatRecInTemp, PlantLoop( GTChiller( ChillerNum ).HRLoopNum ).FluidIndex, RoutineNameHeatRecovery );

				//Don't divide by zero
				if ( ( HeatRecMdot > 0.0 ) && ( HeatRecCp > 0.0 ) ) {
					HeatRecOutTemp = ( QHeatRecLube ) / ( HeatRecMdot * HeatRecCp ) + HeatRecInTemp;
				} else {
					HeatRecOutTemp = HeatRecInTemp;
				}

				//Now verify that the design flowrate was large enough to prevent phase change
				if ( HeatRecOutTemp > GTChiller( ChillerNum ).HeatRecMaxTemp ) {
					if ( GTChiller( ChillerNum ).HeatRecMaxTemp != HeatRecInTemp ) {
						MinHeatRecMdot = ( QHeatRecLube ) / ( HeatRecCp * ( GTChiller( ChillerNum ).HeatRecMaxTemp - HeatRecInTemp ) );
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

		GTChiller( ChillerNum ).HeatRecInletTemp = HeatRecInTemp;
		GTChiller( ChillerNum ).HeatRecOutletTemp = HeatRecOutTemp;
		GTChiller( ChillerNum ).HeatRecMdot = HeatRecMdot;
		GTChiller( ChillerNum ).HeatRecLubeEnergy = QHeatRecLube * ( TimeStepSys * SecInHour );
		GTChiller( ChillerNum ).HeatRecLubeRate = QHeatRecLube;
		GTChiller( ChillerNum ).FuelEnergyIn = std::abs( FuelEnergyIn );

		FuelHeatingValue = GTChiller( ChillerNum ).FuelHeatingValue;

		GTChillerReport( ChillerNum ).FuelMassUsedRate = std::abs( FuelEnergyIn ) / ( FuelHeatingValue * KJtoJ );

		GTChiller( ChillerNum ).ExhaustStackTemp = ExhaustStackTemp;

		//Calculate Energy
		CondenserEnergy = QCondenser * TimeStepSys * SecInHour;
		Energy = Power * TimeStepSys * SecInHour;
		EvaporatorEnergy = QEvaporator * TimeStepSys * SecInHour;

		//check for problems BG 9/12/06 (deal with observed negative energy results)
		if ( Energy < 0.0 ) { // there is a serious problem

			if ( GTChiller( ChillerNum ).CondenserType == WaterCooled ) {
				// first check for run away condenser loop temps (only reason yet to be observed for this?)
				if ( CondInletTemp > 70.0 ) {
					ShowSevereError( "CalcGTChillerModel: Condenser loop inlet temperatures over 70.0 C for GTChiller=" + GTChiller( ChillerNum ).Name );
					ShowContinueErrorTimeStamp( "" );
					ShowContinueError( "Condenser loop water temperatures are too high at" + RoundSigDigits( CondInletTemp, 2 ) );
					ShowContinueError( "Check input for condenser plant loop, especially cooling tower" );
					ShowContinueError( "Evaporator inlet temperature: " + RoundSigDigits( Node( EvapInletNode ).Temp, 2 ) );

					ShowFatalError( "Program Terminates due to previous error condition" );
				}
			}
			if ( ! WarmupFlag ) {
				if ( AvailNomCapRat < 0.0 ) { // apparently the real reason energy goes negative
					ShowSevereError( "CalcGTChillerModel: Capacity ratio below zero for GTChiller=" + GTChiller( ChillerNum ).Name );
					ShowContinueErrorTimeStamp( "" );
					ShowContinueError( "Check input for Capacity Ratio Curve" );
					ShowContinueError( "Condenser inlet temperature: " + RoundSigDigits( CondInletTemp, 2 ) );
					ShowContinueError( "Evaporator inlet temperature: " + RoundSigDigits( Node( EvapInletNode ).Temp, 2 ) );
					ShowFatalError( "Program Terminates due to previous error condition" );
				}
			}
			// If makes it here, set limits, chiller can't have negative energy/power
			// proceeding silently for now but may want to throw error here
			Power = 0.0;
			Energy = 0.0;
		}
	}

	void
	CalcConstCOPChillerModel(
		int const ChillNum,
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

		EvapInletNode = ConstCOPChiller( ChillNum ).EvapInletNodeNum;
		EvapOutletNode = ConstCOPChiller( ChillNum ).EvapOutletNodeNum;
		CondInletNode = ConstCOPChiller( ChillNum ).CondInletNodeNum;
		CondOutletNode = ConstCOPChiller( ChillNum ).CondOutletNodeNum;

		//set module level chiller inlet and temperature variables
		LoopNum = ConstCOPChiller( ChillNum ).CWLoopNum;
		LoopSideNum = ConstCOPChiller( ChillNum ).CWLoopSideNum;
		{ auto const SELECT_CASE_var( PlantLoop( LoopNum ).LoopDemandCalcScheme );
		if ( SELECT_CASE_var == SingleSetPoint ) {
			if ( ( ConstCOPChiller( ChillNum ).FlowMode == LeavingSetPointModulated ) || ( PlantLoop( LoopNum ).LoopSide( LoopSideNum ).Branch( ConstCOPChiller( ChillNum ).CWBranchNum ).Comp( ConstCOPChiller( ChillNum ).CWCompNum ).CurOpSchemeType == CompSetPtBasedSchemeType ) || ( Node( EvapOutletNode ).TempSetPoint != SensedNodeFlagValue ) ) {
				TempEvapOutSetPoint = Node( EvapOutletNode ).TempSetPoint;
			} else {
				TempEvapOutSetPoint = Node( PlantLoop( LoopNum ).TempSetPointNodeNum ).TempSetPoint;
			}
		} else if ( SELECT_CASE_var == DualSetPointDeadBand ) {
			if ( ( ConstCOPChiller( ChillNum ).FlowMode == LeavingSetPointModulated ) || ( PlantLoop( LoopNum ).LoopSide( LoopSideNum ).Branch( ConstCOPChiller( ChillNum ).CWBranchNum ).Comp( ConstCOPChiller( ChillNum ).CWCompNum ).CurOpSchemeType == CompSetPtBasedSchemeType ) || ( Node( EvapOutletNode ).TempSetPointHi != SensedNodeFlagValue ) ) {
				TempEvapOutSetPoint = Node( EvapOutletNode ).TempSetPointHi;
			} else {
				TempEvapOutSetPoint = Node( PlantLoop( LoopNum ).TempSetPointNodeNum ).TempSetPointHi;
			}
		}}
		EvapDeltaTemp = std::abs( Node( EvapInletNode ).Temp - TempEvapOutSetPoint );
		ConstCOPChiller( ChillNum ).EvapInletTemp = Node( EvapInletNode ).Temp;

		//If no component demand, or chiller OFF, or Chiller type set to 'Passive' by free
		//cooling heat exchanger, then set condenser side flow and heat transfer rates set to zero
		if ( MyLoad >= 0.0 || ! RunFlag ) {

			//If Chiller load is 0 or greater or chiller is not running then leave the subroutine.Before leaving
			//if the component control is SERIESACTIVE we set the component flow to inlet flow so that
			//flow resolver will not shut down the branch
			if ( EquipFlowCtrl == ControlType_SeriesActive || PlantLoop( LoopNum ).LoopSide( LoopSideNum ).FlowLock == 1 ) {
				ConstCOPChiller( ChillNum ).EvapMassFlowRate = Node( EvapInletNode ).MassFlowRate;
			} else {
				ConstCOPChiller( ChillNum ).EvapMassFlowRate = 0.0;
				SetComponentFlowRate( ConstCOPChiller( ChillNum ).EvapMassFlowRate, EvapInletNode, EvapOutletNode, ConstCOPChiller( ChillNum ).CWLoopNum, ConstCOPChiller( ChillNum ).CWLoopSideNum, ConstCOPChiller( ChillNum ).CWBranchNum, ConstCOPChiller( ChillNum ).CWCompNum );
			}
			if ( ConstCOPChiller( ChillNum ).CondenserType == WaterCooled ) {
				if ( PlantLoop( ConstCOPChiller( ChillNum ).CDLoopNum ).LoopSide( ConstCOPChiller( ChillNum ).CDLoopSideNum ).Branch( ConstCOPChiller( ChillNum ).CDBranchNum ).Comp( ConstCOPChiller( ChillNum ).CDCompNum ).FlowCtrl == ControlType_SeriesActive ) {
					ConstCOPChiller( ChillNum ).CondMassFlowRate = Node( CondInletNode ).MassFlowRate;
				} else {
					ConstCOPChiller( ChillNum ).CondMassFlowRate = 0.0;
					SetComponentFlowRate( ConstCOPChiller( ChillNum ).CondMassFlowRate, CondInletNode, CondOutletNode, ConstCOPChiller( ChillNum ).CDLoopNum, ConstCOPChiller( ChillNum ).CDLoopSideNum, ConstCOPChiller( ChillNum ).CDBranchNum, ConstCOPChiller( ChillNum ).CDCompNum );
				}
			}

			ConstCOPChiller( ChillNum ).EvapOutletTemp = Node( EvapInletNode ).Temp;
			ConstCOPChiller( ChillNum ).CondOutletTemp = Node( CondInletNode ).Temp;

			Power = 0.0;
			QEvaporator = 0.0;
			QCondenser = 0.0;
			Energy = 0.0;
			EvaporatorEnergy = 0.0;
			CondenserEnergy = 0.0;

			if ( ConstCOPChiller( ChillNum ).CondenserType == EvapCooled ) {
				CalcBasinHeaterPower( ConstCOPChiller( ChillNum ).BasinHeaterPowerFTempDiff, ConstCOPChiller( ChillNum ).BasinHeaterSchedulePtr, ConstCOPChiller( ChillNum ).BasinHeaterSetPointTemp, BasinHeaterPower );
			}
			ConstCOPChiller( ChillNum ).PrintMessage = false;
			return;
		}

		//   calculate end time of current time step
		CurrentEndTime = CurrentTime + SysTimeElapsed;

		//   Print warning messages only when valid and only for the first ocurrance. Let summary provide statistics.
		//   Wait for next time step to print warnings. If simulation iterates, print out
		//   the warning for the last iteration only. Must wait for next time step to accomplish this.
		//   If a warning occurs and the simulation down shifts, the warning is not valid.
		if ( CurrentEndTime > CurrentEndTimeLast && TimeStepSys >= TimeStepSysLast ) {
			if ( ConstCOPChiller( ChillNum ).PrintMessage ) {
				++ConstCOPChiller( ChillNum ).MsgErrorCount;
				//       Show single warning and pass additional info to ShowRecurringWarningErrorAtEnd
				if ( ConstCOPChiller( ChillNum ).MsgErrorCount < 2 ) {
					ShowWarningError( ConstCOPChiller( ChillNum ).MsgBuffer1 + '.' );
					ShowContinueError( ConstCOPChiller( ChillNum ).MsgBuffer2 );
				} else {
					ShowRecurringWarningErrorAtEnd( ConstCOPChiller( ChillNum ).MsgBuffer1 + " error continues.", ConstCOPChiller( ChillNum ).ErrCount1, ConstCOPChiller( ChillNum ).MsgDataLast, ConstCOPChiller( ChillNum ).MsgDataLast, _, "[C]", "[C]" );
				}
			}
		}

		//   save last system time step and last end time of current time step (used to determine if warning is valid)
		TimeStepSysLast = TimeStepSys;
		CurrentEndTimeLast = CurrentEndTime;

		//otherwise the chiller is running...

		if ( ConstCOPChiller( ChillNum ).CondenserType == AirCooled ) { //Condenser inlet temp = outdoor temp
			Node( CondInletNode ).Temp = Node( CondInletNode ).OutAirDryBulb;
			//  Warn user if entering condenser temperature falls below 0C
			if ( Node( CondInletNode ).Temp < 0.0 && ! WarmupFlag ) {
				ConstCOPChiller( ChillNum ).PrintMessage = true;
				gio::write( OutputChar, OutputFormat ) << Node( CondInletNode ).Temp;
				ConstCOPChiller( ChillNum ).MsgBuffer1 = "CalcConstCOPChillerModel - Chiller:ConstantCOP \"" + ConstCOPChiller( ChillNum ).Name + "\" - Air Cooled Condenser Inlet Temperature below 0C";
				ConstCOPChiller( ChillNum ).MsgBuffer2 = "... Outdoor Dry-bulb Condition = " + OutputChar + " C. Occurrence info = " + EnvironmentName + ", " + CurMnDy + ' ' + CreateSysTimeIntervalString();
				ConstCOPChiller( ChillNum ).MsgDataLast = Node( CondInletNode ).Temp;
			} else {
				ConstCOPChiller( ChillNum ).PrintMessage = false;
			}
		} else if ( ConstCOPChiller( ChillNum ).CondenserType == EvapCooled ) { //Condenser inlet temp = (outdoor wet bulb)
			Node( CondInletNode ).Temp = Node( CondInletNode ).OutAirWetBulb;
			//  Warn user if evap condenser wet bulb temperature falls below 10C
			if ( Node( CondInletNode ).Temp < 10.0 && ! WarmupFlag ) {
				ConstCOPChiller( ChillNum ).PrintMessage = true;
				gio::write( OutputChar, OutputFormat ) << Node( CondInletNode ).Temp;
				ConstCOPChiller( ChillNum ).MsgBuffer1 = "CalcConstCOPChillerModel - Chiller:ConstantCOP \"" + ConstCOPChiller( ChillNum ).Name + "\" - Evap Cooled Condenser Inlet Temperature below 10C";
				ConstCOPChiller( ChillNum ).MsgBuffer2 = "... Outdoor Wet-bulb Condition = " + OutputChar + " C. Occurrence info = " + EnvironmentName + ", " + CurMnDy + ' ' + CreateSysTimeIntervalString();
				ConstCOPChiller( ChillNum ).MsgDataLast = Node( CondInletNode ).Temp;
			} else {
				ConstCOPChiller( ChillNum ).PrintMessage = false;
			}
		} // End of the Air Cooled/Evap Cooled Logic block

		// If not air or evap cooled then set to the condenser node that is attached to a cooling tower
		ConstCOPChiller( ChillNum ).CondInletTemp = Node( CondInletNode ).Temp;

		//Set condenser flow rate
		if ( ConstCOPChiller( ChillNum ).CondenserType == WaterCooled ) {
			ConstCOPChiller( ChillNum ).CondMassFlowRate = ConstCOPChiller( ChillNum ).CondMassFlowRateMax;
			SetComponentFlowRate( ConstCOPChiller( ChillNum ).CondMassFlowRate, CondInletNode, CondOutletNode, ConstCOPChiller( ChillNum ).CDLoopNum, ConstCOPChiller( ChillNum ).CDLoopSideNum, ConstCOPChiller( ChillNum ).CDBranchNum, ConstCOPChiller( ChillNum ).CDCompNum );
			PullCompInterconnectTrigger( ConstCOPChiller( ChillNum ).CWLoopNum, ConstCOPChiller( ChillNum ).CWLoopSideNum, ConstCOPChiller( ChillNum ).CWBranchNum, ConstCOPChiller( ChillNum ).CWCompNum, ConstCOPChiller( ChillNum ).CondMassFlowIndex, ConstCOPChiller( ChillNum ).CDLoopNum, ConstCOPChiller( ChillNum ).CDLoopSideNum, CriteriaType_MassFlowRate, ConstCOPChiller( ChillNum ).CondMassFlowRate );

			if ( ConstCOPChiller( ChillNum ).CondMassFlowRate < MassFlowTolerance ) return;

		}

		// If FlowLock is True, the new resolved mdot is used to update Power, QEvap, Qcond, and
		// condenser side outlet temperature.

		Cp = GetSpecificHeatGlycol( PlantLoop( ConstCOPChiller( ChillNum ).CWLoopNum ).FluidName, Node( EvapInletNode ).Temp, PlantLoop( ConstCOPChiller( ChillNum ).CWLoopNum ).FluidIndex, RoutineName );

		if ( PlantLoop( LoopNum ).LoopSide( LoopSideNum ).FlowLock == 0 ) {
			ConstCOPChiller( ChillNum ).PossibleSubcooling = false;
			QEvaporator = std::abs( MyLoad );
			Power = std::abs( MyLoad ) / ConstCOPChiller( ChillNum ).COP;

			// Either set the flow to the Constant value or caluclate the flow for the variable volume
			if ( ( ConstCOPChiller( ChillNum ).FlowMode == ConstantFlow ) || ( ConstCOPChiller( ChillNum ).FlowMode == NotModulated ) ) {

				// Start by assuming max (design) flow
				ConstCOPChiller( ChillNum ).EvapMassFlowRate = ConstCOPChiller( ChillNum ).EvapMassFlowRateMax;
				// Use SetComponentFlowRate to decide actual flow
				SetComponentFlowRate( ConstCOPChiller( ChillNum ).EvapMassFlowRate, EvapInletNode, EvapOutletNode, ConstCOPChiller( ChillNum ).CWLoopNum, ConstCOPChiller( ChillNum ).CWLoopSideNum, ConstCOPChiller( ChillNum ).CWBranchNum, ConstCOPChiller( ChillNum ).CWCompNum );
				// Evaluate delta temp based on actual flow rate
				if ( ConstCOPChiller( ChillNum ).EvapMassFlowRate != 0.0 ) {
					EvapDeltaTemp = QEvaporator / ConstCOPChiller( ChillNum ).EvapMassFlowRate / Cp;
				} else {
					EvapDeltaTemp = 0.0;
				}
				// Evaluate outlet temp based on delta
				ConstCOPChiller( ChillNum ).EvapOutletTemp = Node( EvapInletNode ).Temp - EvapDeltaTemp;

			} else if ( ConstCOPChiller( ChillNum ).FlowMode == LeavingSetPointModulated ) {

				// Calculate the Delta Temp from the inlet temp to the chiller outlet setpoint
				{ auto const SELECT_CASE_var( PlantLoop( ConstCOPChiller( ChillNum ).CWLoopNum ).LoopDemandCalcScheme );
				if ( SELECT_CASE_var == SingleSetPoint ) {
					EvapDeltaTemp = std::abs( Node( EvapInletNode ).Temp - Node( EvapOutletNode ).TempSetPoint );
				} else if ( SELECT_CASE_var == DualSetPointDeadBand ) {
					EvapDeltaTemp = std::abs( Node( EvapInletNode ).Temp - Node( EvapOutletNode ).TempSetPointHi );
				}}

				if ( EvapDeltaTemp > DeltaTempTol ) {
					ConstCOPChiller( ChillNum ).EvapMassFlowRate = std::abs( QEvaporator / Cp / EvapDeltaTemp );
					if ( ( ConstCOPChiller( ChillNum ).EvapMassFlowRate - ConstCOPChiller( ChillNum ).EvapMassFlowRateMax ) > MassFlowTolerance ) ConstCOPChiller( ChillNum ).PossibleSubcooling = true;
					//Check to see if the Maximum is exceeded, if so set to maximum
					ConstCOPChiller( ChillNum ).EvapMassFlowRate = min( ConstCOPChiller( ChillNum ).EvapMassFlowRateMax, ConstCOPChiller( ChillNum ).EvapMassFlowRate );
					// Use SetComponentFlowRate to decide actual flow
					SetComponentFlowRate( ConstCOPChiller( ChillNum ).EvapMassFlowRate, EvapInletNode, EvapOutletNode, ConstCOPChiller( ChillNum ).CWLoopNum, ConstCOPChiller( ChillNum ).CWLoopSideNum, ConstCOPChiller( ChillNum ).CWBranchNum, ConstCOPChiller( ChillNum ).CWCompNum );
					{ auto const SELECT_CASE_var( PlantLoop( ConstCOPChiller( ChillNum ).CWLoopNum ).LoopDemandCalcScheme );
					if ( SELECT_CASE_var == SingleSetPoint ) {
						ConstCOPChiller( ChillNum ).EvapOutletTemp = Node( EvapOutletNode ).TempSetPoint;
					} else if ( SELECT_CASE_var == DualSetPointDeadBand ) {
						ConstCOPChiller( ChillNum ).EvapOutletTemp = Node( EvapOutletNode ).TempSetPointHi;
					}}
				} else {
					// Try to request zero flow
					ConstCOPChiller( ChillNum ).EvapMassFlowRate = 0.0;
					// Use SetComponentFlowRate to decide actual flow
					SetComponentFlowRate( ConstCOPChiller( ChillNum ).EvapMassFlowRate, EvapInletNode, EvapOutletNode, ConstCOPChiller( ChillNum ).CWLoopNum, ConstCOPChiller( ChillNum ).CWLoopSideNum, ConstCOPChiller( ChillNum ).CWBranchNum, ConstCOPChiller( ChillNum ).CWCompNum );
					// No deltaT since component is not running
					ConstCOPChiller( ChillNum ).EvapOutletTemp = Node( EvapInletNode ).Temp;

				}
			} //End of Constant or Variable Flow If Block for FlowLock = 0 (or making a flow request)
		} else { // If FlowLock is True

			ConstCOPChiller( ChillNum ).EvapMassFlowRate = Node( EvapInletNode ).MassFlowRate;
			SetComponentFlowRate( ConstCOPChiller( ChillNum ).EvapMassFlowRate, EvapInletNode, EvapOutletNode, ConstCOPChiller( ChillNum ).CWLoopNum, ConstCOPChiller( ChillNum ).CWLoopSideNum, ConstCOPChiller( ChillNum ).CWBranchNum, ConstCOPChiller( ChillNum ).CWCompNum );
			//   Some other component set the flow to 0. No reason to continue with calculations.
			if ( ConstCOPChiller( ChillNum ).EvapMassFlowRate == 0.0 ) {
				MyLoad = 0.0;
				if ( ConstCOPChiller( ChillNum ).CondenserType == EvapCooled ) {
					CalcBasinHeaterPower( ConstCOPChiller( ChillNum ).BasinHeaterPowerFTempDiff, ConstCOPChiller( ChillNum ).BasinHeaterSchedulePtr, ConstCOPChiller( ChillNum ).BasinHeaterSetPointTemp, BasinHeaterPower );
				}
				ConstCOPChiller( ChillNum ).PrintMessage = false;
				return;
			}

			//Recalculate the Delts Temp
			if ( ConstCOPChiller( ChillNum ).PossibleSubcooling ) {
				QEvaporator = std::abs( MyLoad );
				EvapDeltaTemp = QEvaporator / ConstCOPChiller( ChillNum ).EvapMassFlowRate / Cp;
				ConstCOPChiller( ChillNum ).EvapOutletTemp = Node( EvapInletNode ).Temp - EvapDeltaTemp;
				if ( ConstCOPChiller( ChillNum ).EvapOutletTemp < Node( EvapOutletNode ).TempMin ) {
					ConstCOPChiller( ChillNum ).EvapOutletTemp = Node( EvapOutletNode ).TempMin;
					EvapDeltaTemp = Node( EvapInletNode ).Temp - ConstCOPChiller( ChillNum ).EvapOutletTemp;
					QEvaporator = ConstCOPChiller( ChillNum ).EvapMassFlowRate * Cp * EvapDeltaTemp;
				}
			} else {
				EvapDeltaTemp = Node( EvapInletNode ).Temp - TempEvapOutSetPoint;
				//Calculate the evaporator heat transfer at the specified flow which could have changed
				//  in the Flow Resolution step.
				QEvaporator = std::abs( ConstCOPChiller( ChillNum ).EvapMassFlowRate * Cp * EvapDeltaTemp );
				ConstCOPChiller( ChillNum ).EvapOutletTemp = TempEvapOutSetPoint;
			}
			//Check that the Evap outlet temp honors both plant loop temp low limit and also the chiller low limit
			if ( ConstCOPChiller( ChillNum ).EvapOutletTemp < Node( EvapOutletNode ).TempMin ) {
				if ( ( Node( EvapInletNode ).Temp - Node( EvapOutletNode ).TempMin ) > DeltaTempTol ) {
					ConstCOPChiller( ChillNum ).EvapOutletTemp = Node( EvapOutletNode ).TempMin;
					EvapDeltaTemp = Node( EvapInletNode ).Temp - ConstCOPChiller( ChillNum ).EvapOutletTemp;
					QEvaporator = ConstCOPChiller( ChillNum ).EvapMassFlowRate * Cp * EvapDeltaTemp;
				} else {
					ConstCOPChiller( ChillNum ).EvapOutletTemp = Node( EvapInletNode ).Temp;
					EvapDeltaTemp = Node( EvapInletNode ).Temp - ConstCOPChiller( ChillNum ).EvapOutletTemp;
					QEvaporator = ConstCOPChiller( ChillNum ).EvapMassFlowRate * Cp * EvapDeltaTemp;
				}
			}
			// If load exceeds the distributed load set to the distributed load
			if ( QEvaporator > std::abs( MyLoad ) ) {
				if ( ConstCOPChiller( ChillNum ).EvapMassFlowRate > MassFlowTolerance ) {
					QEvaporator = std::abs( MyLoad );
					EvapDeltaTemp = QEvaporator / ConstCOPChiller( ChillNum ).EvapMassFlowRate / Cp;
					ConstCOPChiller( ChillNum ).EvapOutletTemp = Node( EvapInletNode ).Temp - EvapDeltaTemp;
				} else {
					QEvaporator = 0.0;
					ConstCOPChiller( ChillNum ).EvapOutletTemp = Node( EvapInletNode ).Temp;
				}
			}

			// Checks QEvaporator on the basis of the machine limits.
			if ( QEvaporator > ConstCOPChiller( ChillNum ).NomCap ) {
				if ( ConstCOPChiller( ChillNum ).EvapMassFlowRate > MassFlowTolerance ) {
					QEvaporator = ConstCOPChiller( ChillNum ).NomCap;
					EvapDeltaTemp = QEvaporator / ConstCOPChiller( ChillNum ).EvapMassFlowRate / Cp;
					ConstCOPChiller( ChillNum ).EvapOutletTemp = Node( EvapInletNode ).Temp - EvapDeltaTemp;
				} else {
					QEvaporator = 0.0;
					ConstCOPChiller( ChillNum ).EvapOutletTemp = Node( EvapInletNode ).Temp;
				}
			}
			//Calculate the Power consumption of the Const COP chiller which is a simplified calculation
			Power = QEvaporator / ConstCOPChiller( ChillNum ).COP;
			if ( ConstCOPChiller( ChillNum ).EvapMassFlowRate == 0.0 ) {
				QEvaporator = 0.0;
				ConstCOPChiller( ChillNum ).EvapOutletTemp = Node( EvapInletNode ).Temp;
				Power = 0.0;
				ConstCOPChiller( ChillNum ).PrintMessage = false;
			}
			if ( QEvaporator == 0.0 && ConstCOPChiller( ChillNum ).CondenserType == EvapCooled ) {
				CalcBasinHeaterPower( ConstCOPChiller( ChillNum ).BasinHeaterPowerFTempDiff, ConstCOPChiller( ChillNum ).BasinHeaterSchedulePtr, ConstCOPChiller( ChillNum ).BasinHeaterSetPointTemp, BasinHeaterPower );
			}

		} //This is the end of the FlowLock Block

		//QCondenser is calculated the same for each type, but the power consumption should be different
		//  depending on the performance coefficients used for the chiller model.
		QCondenser = Power + QEvaporator;

		if ( ConstCOPChiller( ChillNum ).CondenserType == WaterCooled ) {
			CpCond = GetSpecificHeatGlycol( PlantLoop( ConstCOPChiller( ChillNum ).CDLoopNum ).FluidName, ConstCOPChiller( ChillNum ).CondInletTemp, PlantLoop( ConstCOPChiller( ChillNum ).CDLoopNum ).FluidIndex, RoutineName );
			if ( ConstCOPChiller( ChillNum ).CondMassFlowRate > MassFlowTolerance ) {
				ConstCOPChiller( ChillNum ).CondOutletTemp = QCondenser / ConstCOPChiller( ChillNum ).CondMassFlowRate / CpCond + ConstCOPChiller( ChillNum ).CondInletTemp;
			} else {
				ShowSevereError( "CalcConstCOPChillerModel: Condenser flow = 0, for CONST COP Chiller=" + ConstCOPChiller( ChillNum ).Name );
				ShowContinueErrorTimeStamp( "" );

			}
		} else { // Air Cooled or Evap Cooled
			//  Set condenser outlet temp to condenser inlet temp for Air Cooled or Evap Cooled
			//  since there is no CondMassFlowRate and would divide by zero
			ConstCOPChiller( ChillNum ).CondOutletTemp = ConstCOPChiller( ChillNum ).CondInletTemp;
		}

		//Calculate Energy
		CondenserEnergy = QCondenser * TimeStepSys * SecInHour;
		Energy = Power * TimeStepSys * SecInHour;
		EvaporatorEnergy = QEvaporator * TimeStepSys * SecInHour;

		//check for problems BG 9/12/06 (deal with observed negative energy results)
		if ( Energy < 0.0 ) { // there is a serious problem

			if ( ConstCOPChiller( ChillNum ).CondenserType == WaterCooled ) {
				// first check for run away condenser loop temps (only reason yet to be observed for this?)
				if ( ConstCOPChiller( ChillNum ).CondInletTemp > 70.0 ) {
					ShowSevereError( "CalcConstCOPChillerModel: Condenser loop inlet temperatures over 70.0 C for ConstCOPChiller=" + ConstCOPChiller( ChillNum ).Name );
					ShowContinueErrorTimeStamp( "" );
					ShowContinueError( "Condenser loop water temperatures are too high at" + RoundSigDigits( ConstCOPChiller( ChillNum ).CondInletTemp, 2 ) );
					ShowContinueError( "Check input for condenser plant loop, especially cooling tower" );
					ShowContinueError( "Evaporator inlet temperature: " + RoundSigDigits( Node( EvapInletNode ).Temp, 2 ) );

					ShowFatalError( "Program Terminates due to previous error condition" );
				}
			}
			// If makes it here, set limits, chiller can't have negative energy/power
			// proceeding silently for now but may want to throw error here
			Power = 0.0;
			Energy = 0.0;

		}
	}

	void
	CalcElectricChillerHeatRecovery(
		int const ChillNum, // number of the current electric chiller being simulated
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
		HeatRecInNode = ElectricChiller( ChillNum ).HeatRecInletNodeNum;
		HeatRecOutNode = ElectricChiller( ChillNum ).HeatRecOutletNodeNum;
		CondInletNode = ElectricChiller( ChillNum ).CondInletNodeNum;
		CondOutletNode = ElectricChiller( ChillNum ).CondOutletNodeNum;
		HeatRecInletTemp = Node( HeatRecInNode ).Temp;
		HeatRecMassFlowRate = Node( HeatRecInNode ).MassFlowRate;

		CpHeatRec = GetSpecificHeatGlycol( PlantLoop( ElectricChiller( ChillNum ).HRLoopNum ).FluidName, HeatRecInletTemp, PlantLoop( ElectricChiller( ChillNum ).HRLoopNum ).FluidIndex, RoutineName );

		if ( ElectricChiller( ChillNum ).CondenserType == WaterCooled ) {
			CpCond = GetSpecificHeatGlycol( PlantLoop( ElectricChiller( ChillNum ).CDLoopNum ).FluidName, CondInletTemp, PlantLoop( ElectricChiller( ChillNum ).CDLoopNum ).FluidIndex, RoutineName );
		} else {
			CpCond = PsyCpAirFnWTdb( Node( CondInletNode ).HumRat, CondInletTemp );
		}

		// Before we modify the QCondenser, the total or original value is transferred to QTot
		QTotal = QCond;

		if ( ElectricChiller( ChillNum ).HeatRecSetPointNodeNum == 0 ) { // use original algorithm that blends temps
			TAvgIn = ( HeatRecMassFlowRate * CpHeatRec * HeatRecInletTemp + CondMassFlow * CpCond * CondInletTemp ) / ( HeatRecMassFlowRate * CpHeatRec + CondMassFlow * CpCond );

			TAvgOut = QTotal / ( HeatRecMassFlowRate * CpHeatRec + CondMassFlow * CpCond ) + TAvgIn;

			QHeatRec = HeatRecMassFlowRate * CpHeatRec * ( TAvgOut - HeatRecInletTemp );
			QHeatRec = max( QHeatRec, 0.0 ); // ensure non negative
			//check if heat flow too large for physical size of bundle
			QHeatRec = min( QHeatRec, ElectricChiller( ChillNum ).HeatRecMaxCapacityLimit );
		} else { // use new algorithm to meet setpoint
			{ auto const SELECT_CASE_var( PlantLoop( ElectricChiller( ChillNum ).HRLoopNum ).LoopDemandCalcScheme );

			if ( SELECT_CASE_var == SingleSetPoint ) {
				THeatRecSetPoint = Node( ElectricChiller( ChillNum ).HeatRecSetPointNodeNum ).TempSetPoint;
			} else if ( SELECT_CASE_var == DualSetPointDeadBand ) {
				THeatRecSetPoint = Node( ElectricChiller( ChillNum ).HeatRecSetPointNodeNum ).TempSetPointHi;
			}}

			QHeatRecToSetPoint = HeatRecMassFlowRate * CpHeatRec * ( THeatRecSetPoint - HeatRecInletTemp );
			QHeatRecToSetPoint = max( QHeatRecToSetPoint, 0.0 );
			QHeatRec = min( QTotal, QHeatRecToSetPoint );
			//check if heat flow too large for physical size of bundle
			QHeatRec = min( QHeatRec, ElectricChiller( ChillNum ).HeatRecMaxCapacityLimit );

		}
		// check if limit on inlet is present and exceeded.
		if ( ElectricChiller( ChillNum ).HeatRecInletLimitSchedNum > 0 ) {
			HeatRecHighInletLimit = GetCurrentScheduleValue( ElectricChiller( ChillNum ).HeatRecInletLimitSchedNum );
			if ( HeatRecInletTemp > HeatRecHighInletLimit ) { // shut down heat recovery
				QHeatRec = 0.0;
			}
		}

		QCond = QTotal - QHeatRec;

		// Calculate a new Heat Recovery Coil Outlet Temp
		if ( HeatRecMassFlowRate > 0.0 ) {
			HeatRecOutletTemp = QHeatRec / ( HeatRecMassFlowRate * CpHeatRec ) + HeatRecInletTemp;
		} else {
			HeatRecOutletTemp = HeatRecInletTemp;
		}

	}

	void
	CalcEngineChillerHeatRec(
		int const ChillerNum, // Chiller number
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
		HeatRecInNode = EngineDrivenChiller( ChillerNum ).HeatRecInletNodeNum;
		HeatRecOutNode = EngineDrivenChiller( ChillerNum ).HeatRecOutletNodeNum;

		//Need to set the HeatRecRatio to 1.0 if it is not modified
		HeatRecRatio = 1.0;

		//  !This mdot is input specified mdot "Desired Flowrate", already set in init routine
		HeatRecMdot = Node( HeatRecInNode ).MassFlowRate;

		HeatRecInTemp = Node( HeatRecInNode ).Temp;
		HeatRecCp = GetSpecificHeatGlycol( PlantLoop( EngineDrivenChiller( ChillerNum ).HRLoopNum ).FluidName, EngineDrivenChiller( ChillerNum ).HeatRecInletTemp, PlantLoop( EngineDrivenChiller( ChillerNum ).HRLoopNum ).FluidIndex, RoutineName );

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
		if ( HeatRecOutTemp > EngineDrivenChiller( ChillerNum ).HeatRecMaxTemp ) {
			if ( EngineDrivenChiller( ChillerNum ).HeatRecMaxTemp != HeatRecInTemp ) {
				MinHeatRecMdot = ( EnergyRecovered ) / ( HeatRecCp * ( EngineDrivenChiller( ChillerNum ).HeatRecMaxTemp - HeatRecInTemp ) );
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
		EngineDrivenChiller( ChillerNum ).HeatRecInletTemp = HeatRecInTemp;
		HeatRecOutletTemp = HeatRecOutTemp;
		EngineDrivenChiller( ChillerNum ).HeatRecMdotActual = HeatRecMdot;

	}

	void
	UpdateElectricChillerRecords(
		Real64 const MyLoad, // current load
		bool const RunFlag, // TRUE if chiller operating
		int const Num // chiller number
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

		EvapInletNode = ElectricChiller( Num ).EvapInletNodeNum;
		EvapOutletNode = ElectricChiller( Num ).EvapOutletNodeNum;
		CondInletNode = ElectricChiller( Num ).CondInletNodeNum;
		CondOutletNode = ElectricChiller( Num ).CondOutletNodeNum;
		HeatRecInNode = ElectricChiller( Num ).HeatRecInletNodeNum;
		HeatRecOutNode = ElectricChiller( Num ).HeatRecOutletNodeNum;

		if ( MyLoad >= 0.0 || ! RunFlag ) { //Chiller not running so pass inlet states to outlet states
			//set node temperatures
			Node( EvapOutletNode ).Temp = Node( EvapInletNode ).Temp;
			Node( CondOutletNode ).Temp = Node( CondInletNode ).Temp;
			if ( ElectricChiller( Num ).CondenserType != WaterCooled ) {
				Node( CondOutletNode ).HumRat = Node( CondInletNode ).HumRat;
				Node( CondOutletNode ).Enthalpy = Node( CondInletNode ).Enthalpy;
			}

			ElectricChillerReport( Num ).Power = 0.0;
			ElectricChillerReport( Num ).QEvap = 0.0;
			ElectricChillerReport( Num ).QCond = 0.0;
			ElectricChillerReport( Num ).Energy = 0.0;
			ElectricChillerReport( Num ).EvapEnergy = 0.0;
			ElectricChillerReport( Num ).CondEnergy = 0.0;
			ElectricChillerReport( Num ).EvapInletTemp = Node( EvapInletNode ).Temp;
			ElectricChillerReport( Num ).CondInletTemp = Node( CondInletNode ).Temp;
			ElectricChillerReport( Num ).CondOutletTemp = Node( CondOutletNode ).Temp;
			ElectricChillerReport( Num ).EvapOutletTemp = Node( EvapOutletNode ).Temp;
			ElectricChillerReport( Num ).Evapmdot = ElectricChiller( Num ).EvapMassFlowRate;
			ElectricChillerReport( Num ).Condmdot = ElectricChiller( Num ).CondMassFlowRate;
			ElectricChillerReport( Num ).ActualCOP = 0.0;
			if ( ElectricChiller( Num ).CondenserType == EvapCooled ) {
				ElectricChillerReport( Num ).BasinHeaterPower = BasinHeaterPower;
				ElectricChillerReport( Num ).BasinHeaterConsumption = BasinHeaterPower * ReportingConstant;
			}

			if ( ElectricChiller( Num ).HeatRecActive ) {

				SafeCopyPlantNode( HeatRecInNode, HeatRecOutNode );

				ElectricChillerReport( Num ).QHeatRecovery = 0.0;
				ElectricChillerReport( Num ).EnergyHeatRecovery = 0.0;
				ElectricChillerReport( Num ).HeatRecInletTemp = Node( HeatRecInNode ).Temp;
				ElectricChillerReport( Num ).HeatRecOutletTemp = Node( HeatRecOutNode ).Temp;
				ElectricChillerReport( Num ).HeatRecMassFlow = Node( HeatRecInNode ).MassFlowRate;

				ElectricChillerReport( Num ).ChillerCondAvgTemp = AvgCondSinkTemp;
			}

		} else { //Chiller is running, so pass calculated values
			//set node temperatures
			Node( EvapOutletNode ).Temp = ElectricChiller( Num ).EvapOutletTemp;
			Node( CondOutletNode ).Temp = ElectricChiller( Num ).CondOutletTemp;
			if ( ElectricChiller( Num ).CondenserType != WaterCooled ) {
				Node( CondOutletNode ).HumRat = ElectricChiller( Num ).CondOutletHumRat;
				Node( CondOutletNode ).Enthalpy = PsyHFnTdbW( ElectricChiller( Num ).CondOutletTemp, ElectricChiller( Num ).CondOutletHumRat );
			}
			//set node flow rates;  for these load based models
			//assume that the sufficient evaporator flow rate available
			ElectricChillerReport( Num ).Power = Power;
			ElectricChillerReport( Num ).QEvap = QEvaporator;
			ElectricChillerReport( Num ).QCond = QCondenser;
			ElectricChillerReport( Num ).Energy = Energy;
			ElectricChillerReport( Num ).EvapEnergy = EvaporatorEnergy;
			ElectricChillerReport( Num ).CondEnergy = CondenserEnergy;
			ElectricChillerReport( Num ).EvapInletTemp = Node( EvapInletNode ).Temp;
			ElectricChillerReport( Num ).CondInletTemp = Node( CondInletNode ).Temp;
			ElectricChillerReport( Num ).CondOutletTemp = Node( CondOutletNode ).Temp;
			ElectricChillerReport( Num ).EvapOutletTemp = Node( EvapOutletNode ).Temp;
			ElectricChillerReport( Num ).Evapmdot = ElectricChiller( Num ).EvapMassFlowRate;
			ElectricChillerReport( Num ).Condmdot = ElectricChiller( Num ).CondMassFlowRate;
			if ( ElectricChiller( Num ).CondenserType == EvapCooled ) {
				ElectricChillerReport( Num ).BasinHeaterPower = BasinHeaterPower;
				ElectricChillerReport( Num ).BasinHeaterConsumption = BasinHeaterPower * ReportingConstant;
			}
			if ( Power != 0.0 ) {
				ElectricChillerReport( Num ).ActualCOP = QEvaporator / Power;
			} else {
				ElectricChillerReport( Num ).ActualCOP = 0.0;
			}

			if ( ElectricChiller( Num ).HeatRecActive ) {

				SafeCopyPlantNode( HeatRecInNode, HeatRecOutNode );
				ElectricChillerReport( Num ).QHeatRecovery = QHeatRecovered;
				ElectricChillerReport( Num ).EnergyHeatRecovery = QHeatRecovered * TimeStepSys * SecInHour;
				Node( HeatRecOutNode ).Temp = HeatRecOutletTemp;
				ElectricChillerReport( Num ).HeatRecInletTemp = Node( HeatRecInNode ).Temp;
				ElectricChillerReport( Num ).HeatRecOutletTemp = Node( HeatRecOutNode ).Temp;
				ElectricChillerReport( Num ).HeatRecMassFlow = Node( HeatRecInNode ).MassFlowRate;
				ElectricChillerReport( Num ).ChillerCondAvgTemp = AvgCondSinkTemp;
			}

		}
	}

	// End of EngineDriven Chiller Module Utility Subroutines
	// *****************************************************************************

	// Beginning of Record Keeping subroutines for the EngineDriven Chiller Module
	// *****************************************************************************

	void
	UpdateEngineDrivenChiller(
		Real64 const MyLoad, // current load
		bool const RunFlag, // TRUE if chiller operating
		int const Num // chiller number
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

		EvapInletNode = EngineDrivenChiller( Num ).EvapInletNodeNum;
		EvapOutletNode = EngineDrivenChiller( Num ).EvapOutletNodeNum;
		CondInletNode = EngineDrivenChiller( Num ).CondInletNodeNum;
		CondOutletNode = EngineDrivenChiller( Num ).CondOutletNodeNum;

		HeatRecInletNode = EngineDrivenChiller( Num ).HeatRecInletNodeNum;
		HeatRecOutletNode = EngineDrivenChiller( Num ).HeatRecOutletNodeNum;

		if ( MyLoad >= 0.0 || ! RunFlag ) { //Chiller not running
			//set node temperatures
			Node( EvapOutletNode ).Temp = Node( EvapInletNode ).Temp;
			Node( CondOutletNode ).Temp = Node( CondInletNode ).Temp;

			EngineDrivenChillerReport( Num ).Power = 0.0;
			EngineDrivenChillerReport( Num ).QEvap = 0.0;
			EngineDrivenChillerReport( Num ).QCond = 0.0;
			EngineDrivenChillerReport( Num ).Energy = 0.0;
			EngineDrivenChillerReport( Num ).EvapEnergy = 0.0;
			EngineDrivenChillerReport( Num ).CondEnergy = 0.0;
			EngineDrivenChillerReport( Num ).EvapInletTemp = Node( EvapInletNode ).Temp;
			EngineDrivenChillerReport( Num ).CondInletTemp = Node( CondInletNode ).Temp;
			EngineDrivenChillerReport( Num ).CondOutletTemp = Node( CondOutletNode ).Temp;
			EngineDrivenChillerReport( Num ).EvapOutletTemp = Node( EvapOutletNode ).Temp;
			EngineDrivenChillerReport( Num ).Evapmdot = EngineDrivenChiller( Num ).EvapMassFlowRate;
			EngineDrivenChillerReport( Num ).Condmdot = EngineDrivenChiller( Num ).CondMassFlowRate;
			EngineDrivenChillerReport( Num ).FuelCOP = 0.0;
			if ( EngineDrivenChiller( Num ).CondenserType == EvapCooled ) {
				EngineDrivenChillerReport( Num ).BasinHeaterPower = BasinHeaterPower;
				EngineDrivenChillerReport( Num ).BasinHeaterConsumption = BasinHeaterPower * ReportingConstant;
			}
		} else { //Chiller is running
			//set node temperatures
			Node( EvapOutletNode ).Temp = EngineDrivenChiller( Num ).EvapOutletTemp;
			Node( CondOutletNode ).Temp = EngineDrivenChiller( Num ).CondOutletTemp;

			EngineDrivenChillerReport( Num ).Power = Power;
			EngineDrivenChillerReport( Num ).QEvap = QEvaporator;
			EngineDrivenChillerReport( Num ).QCond = QCondenser;
			EngineDrivenChillerReport( Num ).Energy = Energy;
			EngineDrivenChillerReport( Num ).EvapEnergy = EvaporatorEnergy;
			EngineDrivenChillerReport( Num ).CondEnergy = CondenserEnergy;
			EngineDrivenChillerReport( Num ).EvapInletTemp = Node( EvapInletNode ).Temp;
			EngineDrivenChillerReport( Num ).CondInletTemp = Node( CondInletNode ).Temp;
			EngineDrivenChillerReport( Num ).CondOutletTemp = Node( CondOutletNode ).Temp;
			EngineDrivenChillerReport( Num ).EvapOutletTemp = Node( EvapOutletNode ).Temp;
			EngineDrivenChillerReport( Num ).Evapmdot = EngineDrivenChiller( Num ).EvapMassFlowRate;
			EngineDrivenChillerReport( Num ).Condmdot = EngineDrivenChiller( Num ).CondMassFlowRate;
			if ( EngineDrivenChiller( Num ).FuelEnergyUseRate != 0.0 ) {
				EngineDrivenChillerReport( Num ).FuelCOP = QEvaporator / EngineDrivenChiller( Num ).FuelEnergyUseRate;
			} else {
				EngineDrivenChillerReport( Num ).FuelCOP = 0.0;
			}
			if ( EngineDrivenChiller( Num ).CondenserType == EvapCooled ) {
				EngineDrivenChillerReport( Num ).BasinHeaterPower = BasinHeaterPower;
				EngineDrivenChillerReport( Num ).BasinHeaterConsumption = BasinHeaterPower * ReportingConstant;
			}
		}

		// Update Heat Recovery Stuff whether running or not, variables should be set correctly
		EngineDrivenChillerReport( Num ).FuelEnergyUseRate = EngineDrivenChiller( Num ).FuelEnergyUseRate;
		EngineDrivenChillerReport( Num ).JacketEnergyRec = EngineDrivenChiller( Num ).JacketEnergyRec;
		EngineDrivenChillerReport( Num ).LubeOilEnergyRec = EngineDrivenChiller( Num ).LubeOilEnergyRec;
		EngineDrivenChillerReport( Num ).ExhaustEnergyRec = EngineDrivenChiller( Num ).ExhaustEnergyRec;
		EngineDrivenChillerReport( Num ).TotalHeatEnergyRec = EngineDrivenChiller( Num ).TotalHeatEnergyRec;
		EngineDrivenChillerReport( Num ).FuelEnergy = EngineDrivenChiller( Num ).FuelEnergy;
		EngineDrivenChillerReport( Num ).FuelMdot = EngineDrivenChiller( Num ).FuelMdot;
		EngineDrivenChillerReport( Num ).ExhaustStackTemp = EngineDrivenChiller( Num ).ExhaustStackTemp;
		EngineDrivenChillerReport( Num ).HeatRecInletTemp = EngineDrivenChiller( Num ).HeatRecInletTemp;
		EngineDrivenChillerReport( Num ).HeatRecOutletTemp = HeatRecOutletTemp;
		EngineDrivenChillerReport( Num ).HeatRecMdot = EngineDrivenChiller( Num ).HeatRecMdotActual;

		//Update the Heat Recovery outlet
		if ( EngineDrivenChiller( Num ).HeatRecActive ) {
			SafeCopyPlantNode( HeatRecInletNode, HeatRecOutletNode );
			Node( HeatRecOutletNode ).Temp = HeatRecOutletTemp;
		}

	}

	void
	UpdateGTChillerRecords(
		Real64 const MyLoad, // current load
		bool const RunFlag, // TRUE if chiller operating
		int const Num // chiller number
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

		EvapInletNode = GTChiller( Num ).EvapInletNodeNum;
		EvapOutletNode = GTChiller( Num ).EvapOutletNodeNum;
		CondInletNode = GTChiller( Num ).CondInletNodeNum;
		CondOutletNode = GTChiller( Num ).CondOutletNodeNum;
		if ( GTChiller( Num ).HeatRecActive ) {
			HeatRecInletNode = GTChiller( Num ).HeatRecInletNodeNum;
			HeatRecOutletNode = GTChiller( Num ).HeatRecOutletNodeNum;
		}

		if ( MyLoad >= 0.0 || ! RunFlag ) { //Chiller not running so pass inlet states to outlet states
			//set node temperatures
			Node( EvapOutletNode ).Temp = Node( EvapInletNode ).Temp;
			Node( CondOutletNode ).Temp = Node( CondInletNode ).Temp;

			if ( GTChiller( Num ).HeatRecActive ) {
				SafeCopyPlantNode( HeatRecOutletNode, HeatRecInletNode );
				GTChillerReport( Num ).HeatRecInletTemp = Node( HeatRecInletNode ).Temp;
				GTChillerReport( Num ).HeatRecOutletTemp = Node( HeatRecOutletNode ).Temp;
			}

			GTChillerReport( Num ).Power = 0.0;
			GTChillerReport( Num ).QEvap = 0.0;
			GTChillerReport( Num ).QCond = 0.0;
			GTChillerReport( Num ).Energy = 0.0;
			GTChillerReport( Num ).EvapEnergy = 0.0;
			GTChillerReport( Num ).CondEnergy = 0.0;
			GTChillerReport( Num ).EvapInletTemp = Node( EvapInletNode ).Temp;
			GTChillerReport( Num ).CondInletTemp = Node( CondInletNode ).Temp;
			GTChillerReport( Num ).CondOutletTemp = Node( CondOutletNode ).Temp;
			GTChillerReport( Num ).EvapOutletTemp = Node( EvapOutletNode ).Temp;
			GTChillerReport( Num ).Evapmdot = GTChiller( Num ).EvapMassFlowRate;
			GTChillerReport( Num ).Condmdot = GTChiller( Num ).CondMassFlowRate;
			GTChillerReport( Num ).FuelEnergyUsedRate = 0.0;
			GTChillerReport( Num ).FuelMassUsedRate = 0.0;
			GTChillerReport( Num ).FuelEnergyUsed = 0.0;
			GTChillerReport( Num ).FuelMassUsed = 0.0;

			GTChillerReport( Num ).HeatRecLubeEnergy = 0.0;
			GTChillerReport( Num ).HeatRecLubeRate = 0.0;
			GTChillerReport( Num ).ExhaustStackTemp = 0.0;
			GTChillerReport( Num ).HeatRecMdot = GTChiller( Num ).HeatRecMdot;
			GTChillerReport( Num ).FuelCOP = 0.0;
			if ( GTChiller( Num ).CondenserType == EvapCooled ) {
				GTChillerReport( Num ).BasinHeaterPower = BasinHeaterPower;
				GTChillerReport( Num ).BasinHeaterConsumption = BasinHeaterPower * ReportingConstant;
			}

		} else { //Chiller is running so report calculated values
			//set node temperatures
			Node( EvapOutletNode ).Temp = GTChiller( Num ).EvapOutletTemp;
			Node( CondOutletNode ).Temp = GTChiller( Num ).CondOutletTemp;

			if ( GTChiller( Num ).HeatRecActive ) {
				SafeCopyPlantNode( HeatRecOutletNode, HeatRecInletNode );
				Node( HeatRecOutletNode ).Temp = GTChiller( Num ).HeatRecOutletTemp;
			}

			GTChillerReport( Num ).Power = Power;
			GTChillerReport( Num ).QEvap = QEvaporator;
			GTChillerReport( Num ).QCond = QCondenser;
			GTChillerReport( Num ).Energy = Energy;
			GTChillerReport( Num ).EvapEnergy = EvaporatorEnergy;
			GTChillerReport( Num ).CondEnergy = CondenserEnergy;
			GTChillerReport( Num ).EvapInletTemp = Node( EvapInletNode ).Temp;
			GTChillerReport( Num ).CondInletTemp = Node( CondInletNode ).Temp;
			GTChillerReport( Num ).CondOutletTemp = Node( CondOutletNode ).Temp;
			GTChillerReport( Num ).EvapOutletTemp = Node( EvapOutletNode ).Temp;
			GTChillerReport( Num ).Evapmdot = GTChiller( Num ).EvapMassFlowRate;
			GTChillerReport( Num ).Condmdot = GTChiller( Num ).CondMassFlowRate;

			GTChillerReport( Num ).HeatRecLubeEnergy = GTChiller( Num ).HeatRecLubeEnergy;
			GTChillerReport( Num ).HeatRecLubeRate = GTChiller( Num ).HeatRecLubeRate;
			GTChillerReport( Num ).FuelEnergyUsedRate = GTChiller( Num ).FuelEnergyIn;
			GTChillerReport( Num ).FuelMassUsedRate = GTChillerReport( Num ).FuelMassUsedRate;
			GTChillerReport( Num ).FuelEnergyUsed = GTChillerReport( Num ).FuelEnergyUsedRate * TimeStepSys * SecInHour;
			GTChillerReport( Num ).FuelMassUsed = GTChillerReport( Num ).FuelMassUsedRate * TimeStepSys * SecInHour;
			GTChillerReport( Num ).ExhaustStackTemp = GTChiller( Num ).ExhaustStackTemp;
			GTChillerReport( Num ).HeatRecInletTemp = GTChiller( Num ).HeatRecInletTemp;
			GTChillerReport( Num ).HeatRecOutletTemp = GTChiller( Num ).HeatRecOutletTemp;
			GTChillerReport( Num ).HeatRecMdot = GTChiller( Num ).HeatRecMdot;
			if ( GTChillerReport( Num ).FuelEnergyUsedRate != 0.0 ) {
				GTChillerReport( Num ).FuelCOP = GTChillerReport( Num ).QEvap / GTChillerReport( Num ).FuelEnergyUsedRate;
			} else {
				GTChillerReport( Num ).FuelCOP = 0.0;
			}
			if ( GTChiller( Num ).CondenserType == EvapCooled ) {
				GTChillerReport( Num ).BasinHeaterPower = BasinHeaterPower;
				GTChillerReport( Num ).BasinHeaterConsumption = BasinHeaterPower * ReportingConstant;
			}
		}
	}

	void
	UpdateConstCOPChillerRecords(
		Real64 const MyLoad, // unused1208
		bool const RunFlag, // unused1208
		int const Num
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

		EvapInletNode = ConstCOPChiller( Num ).EvapInletNodeNum;
		EvapOutletNode = ConstCOPChiller( Num ).EvapOutletNodeNum;
		CondInletNode = ConstCOPChiller( Num ).CondInletNodeNum;
		CondOutletNode = ConstCOPChiller( Num ).CondOutletNodeNum;

		if ( MyLoad >= 0.0 || ! RunFlag ) { //Chiller not running so pass inlet states to outlet states
			ConstCOPChillerReport( Num ).Power = 0.0;
			ConstCOPChillerReport( Num ).QEvap = 0.0;
			ConstCOPChillerReport( Num ).QCond = 0.0;
			ConstCOPChillerReport( Num ).Energy = 0.0;
			ConstCOPChillerReport( Num ).EvapEnergy = 0.0;
			ConstCOPChillerReport( Num ).CondEnergy = 0.0;
			ConstCOPChillerReport( Num ).CondInletTemp = Node( CondInletNode ).Temp;
			ConstCOPChillerReport( Num ).EvapInletTemp = Node( EvapInletNode ).Temp;
			ConstCOPChillerReport( Num ).CondOutletTemp = Node( CondInletNode ).Temp;
			ConstCOPChillerReport( Num ).EvapOutletTemp = Node( EvapInletNode ).Temp;
			ConstCOPChillerReport( Num ).Evapmdot = ConstCOPChiller( Num ).EvapMassFlowRate;
			ConstCOPChillerReport( Num ).Condmdot = ConstCOPChiller( Num ).CondMassFlowRate;
			ConstCOPChillerReport( Num ).ActualCOP = 0.0;
			if ( ConstCOPChiller( Num ).CondenserType == EvapCooled ) {
				ConstCOPChillerReport( Num ).BasinHeaterPower = BasinHeaterPower;
				ConstCOPChillerReport( Num ).BasinHeaterConsumption = BasinHeaterPower * ReportingConstant;
			}

			//set outlet node temperatures
			Node( EvapOutletNode ).Temp = Node( EvapInletNode ).Temp;
			Node( CondOutletNode ).Temp = Node( CondInletNode ).Temp;

		} else {
			ConstCOPChillerReport( Num ).Power = Power;
			ConstCOPChillerReport( Num ).QEvap = QEvaporator;
			ConstCOPChillerReport( Num ).QCond = QCondenser;
			ConstCOPChillerReport( Num ).Energy = Energy;
			ConstCOPChillerReport( Num ).EvapEnergy = EvaporatorEnergy;
			ConstCOPChillerReport( Num ).CondEnergy = CondenserEnergy;
			ConstCOPChillerReport( Num ).CondInletTemp = Node( CondInletNode ).Temp;
			ConstCOPChillerReport( Num ).EvapInletTemp = Node( EvapInletNode ).Temp;
			ConstCOPChillerReport( Num ).CondOutletTemp = ConstCOPChiller( Num ).CondOutletTemp;
			ConstCOPChillerReport( Num ).EvapOutletTemp = ConstCOPChiller( Num ).EvapOutletTemp;
			ConstCOPChillerReport( Num ).Evapmdot = ConstCOPChiller( Num ).EvapMassFlowRate;
			ConstCOPChillerReport( Num ).Condmdot = ConstCOPChiller( Num ).CondMassFlowRate;
			if ( Power != 0.0 ) {
				ConstCOPChillerReport( Num ).ActualCOP = QEvaporator / Power;
			} else {
				ConstCOPChillerReport( Num ).ActualCOP = 0.0;
			}
			if ( ConstCOPChiller( Num ).CondenserType == EvapCooled ) {
				ConstCOPChillerReport( Num ).BasinHeaterPower = BasinHeaterPower;
				ConstCOPChillerReport( Num ).BasinHeaterConsumption = BasinHeaterPower * ReportingConstant;
			}

			//set outlet node temperatures
			Node( EvapOutletNode ).Temp = ConstCOPChiller( Num ).EvapOutletTemp;
			Node( CondOutletNode ).Temp = ConstCOPChiller( Num ).CondOutletTemp;
		}

	}

	// End of Record Keeping subroutines for the Const COP Chiller Module
	// *****************************************************************************

} // PlantChillers

} // EnergyPlus
