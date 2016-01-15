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
#include <cassert>
#include <cmath>
#include <string>

// ObjexxFCL Headers
#include <ObjexxFCL/Fmath.hh>
#include <ObjexxFCL/gio.hh>

// EnergyPlus Headers
#include <ChillerReformulatedEIR.hh>
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
#include <GlobalNames.hh>
#include <InputProcessor.hh>
#include <NodeInputManager.hh>
#include <OutputProcessor.hh>
#include <OutputReportPredefined.hh>
#include <PlantUtilities.hh>
#include <Psychrometrics.hh>
#include <ReportSizingManager.hh>
#include <ScheduleManager.hh>
#include <StandardRatings.hh>
#include <UtilityRoutines.hh>

namespace EnergyPlus {

namespace ChillerReformulatedEIR {

	// The Electric EIR and Reformulated EIR chiller models are similar.
	// They only differ in the independent variable used to evaluate the performance curves.
	// Since the Reformulated EIR chiller uses outlet condenser water temperature as an
	// independent variable, iteration is required to converge on a solution.

	// MODULE INFORMATION:
	//       AUTHOR         Lixing Gu
	//       DATE WRITTEN   August 2006
	//       MODIFIED       na
	//       RE-ENGINEERED  na

	//       MODIFIED
	//			Aug.  2014, Rongpeng Zhang, added An additional part-load performance curve type

	// PURPOSE OF THIS MODULE:
	//  This module simulates the performance of the electric vapor compression
	//  chiller using a reformulated model based on the DOE-2 EIR chiller.

	// METHODOLOGY EMPLOYED:
	//  Once the PlantLoopManager determines that the Reformulated EIR chiller
	//  is available to meet a loop cooling demand, it calls SimReformulatedEIRChiller
	//  which in turn calls the reformulated EIR chiller model.
	//  The ReformulatedEIR chiller model is based on polynomial fits of chiller
	//  performance data.

	// REFERENCES:
	// 1. Hydeman, M., P. Sreedharan, N. Webb, and S. Blanc. 2002. "Development and Testing of a Reformulated
	//    Regression-Based Electric Chiller Model". ASHRAE Transactions, HI-02-18-2, Vol 108, Part 2, pp. 1118-1127.

	// Using/Aliasing
	using namespace DataPrecisionGlobals;
	using namespace DataLoopNode;
	using DataGlobals::InitConvTemp;
	using DataGlobals::DisplayExtraWarnings;
	using DataHVACGlobals::SmallWaterVolFlow;
	using Psychrometrics::PsyCpAirFnWTdb;
	using Psychrometrics::PsyRhoAirFnPbTdbW;
	using General::TrimSigDigits;
	using General::RoundSigDigits;
	using FluidProperties::GetDensityGlycol;
	using FluidProperties::GetSpecificHeatGlycol;
	using DataPlant::PlantLoop;

	// Data
	// MODULE PARAMETER DEFINITIONS:
	// Chiller type parameters
	int const AirCooled( 1 ); // Air-cooled condenser currently not allowed
	int const WaterCooled( 2 ); // Only water-cooled condensers are currently allowed
	int const EvapCooled( 3 ); // Evap-cooled condenser currently not allowed
	// Performance curve variable parameters
	int const LeavingCondenser( 5 );

	//chiller flow modes
	int const FlowModeNotSet( 200 );
	int const ConstantFlow( 201 );
	int const NotModulated( 202 );
	int const LeavingSetPointModulated( 203 );

	//chiller part load curve types
	int const PLR_LeavingCondenserWaterTemperature( 1 ); //Type 1_LeavingCondenserWaterTemperature
	int const PLR_Lift( 2 ); //Type 2_Lift

	// MODULE VARIABLE DECLARATIONS:
	int NumElecReformEIRChillers( 0 ); // Number of electric reformulated EIR chillers specified in input
	Real64 CondMassFlowRate( 0.0 ); // Condenser mass flow rate [kg/s]
	Real64 EvapMassFlowRate( 0.0 ); // Evaporator mass flow rate [kg/s]
	Real64 CondOutletTemp( 0.0 ); // Condenser outlet temperature [C]
	Real64 EvapOutletTemp( 0.0 ); // Evaporator outlet temperature [C]
	Real64 Power( 0.0 ); // Rate of chiller electric energy use [W]
	Real64 QEvaporator( 0.0 ); // Rate of heat transfer to the evaporator coil [W]
	Real64 QCondenser( 0.0 ); // Rate of heat transfer to the condenser coil [W]
	Real64 QHeatRecovered( 0.0 ); // Rate of heat transfer to the heat recovery coil [W]
	Real64 HeatRecOutletTemp( 0.0 ); // Heat recovery outlet temperature [C]
	//REAL(r64)      :: CondenserFanPower       =0.0d0 ! Condenser Fan Power (fan cycles with compressor) [W]
	Real64 ChillerCapFT( 0.0 ); // Chiller capacity fraction (evaluated as a function of temperature)
	Real64 ChillerEIRFT( 0.0 ); // Chiller electric input ratio (EIR = 1 / COP) as a function of temperature
	Real64 ChillerEIRFPLR( 0.0 ); // Chiller EIR as a function of part-load ratio (PLR)
	Real64 ChillerPartLoadRatio( 0.0 ); // Chiller part-load ratio (PLR)
	Real64 ChillerCyclingRatio( 0.0 ); // Chiller cycling ratio
	Real64 ChillerFalseLoadRate( 0.0 ); // Chiller false load over and above the water-side load [W]
	Real64 AvgCondSinkTemp( 0.0 ); // condenser temperature value for use in curves [C]

	bool GetInputREIR( true ); // When TRUE, calls subroutine to read input file

	// SUBROUTINE SPECIFICATIONS FOR MODULE ChillerReformulatedEIR

	// Object Data
	Array1D< ReformulatedEIRChillerSpecs > ElecReformEIRChiller; // dimension to number of machines
	Array1D< ReportVars > ElecReformEIRChillerReport;

	// MODULE SUBROUTINES:

	// Beginning of Reformulated EIR Chiller Module Driver Subroutine
	//*************************************************************************

	// Functions

	void
	SimReformulatedEIRChiller(
		std::string const & EP_UNUSED( EIRChillerType ), // Type of chiller !unused1208
		std::string const & EIRChillerName, // User specified name of chiller
		int const EquipFlowCtrl, // Flow control mode for the equipment
		int & CompIndex, // Chiller number pointer
		int const LoopNum, // plant loop index pointer
		bool const RunFlag, // Simulate chiller when TRUE
		bool const FirstIteration, // Initialize variables when TRUE
		bool & InitLoopEquip, // If not zero, calculate the max load for operating conditions
		Real64 & MyLoad, // Loop demand component will meet [W]
		Real64 & MaxCap, // Maximum operating capacity of chiller [W]
		Real64 & MinCap, // Minimum operating capacity of chiller [W]
		Real64 & OptCap, // Optimal operating capacity of chiller [W]
		bool const GetSizingFactor, // TRUE when just the sizing factor is requested
		Real64 & SizingFactor, // sizing factor
		Real64 & TempCondInDesign,
		Real64 & TempEvapOutDesign
	)
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         Lixing Gu
		//       DATE WRITTEN   July 2004
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		//  This is the reformulated EIR chiller model driver. It gets the input for the
		//  models, initializes simulation variables, calls the appropriate model and sets
		//  up reporting variables.

		// METHODOLOGY EMPLOYED: na

		// REFERENCES: na

		// Using/Aliasing
		using InputProcessor::FindItemInList;
		using PlantUtilities::UpdateChillerComponentCondenserSide;
		using PlantUtilities::UpdateComponentHeatRecoverySide;
		using DataPlant::TypeOf_Chiller_ElectricReformEIR;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int EIRChillNum;
		int LoopSide;

		if ( GetInputREIR ) {
			GetElecReformEIRChillerInput();
			GetInputREIR = false;
		}

		// Find the correct Chiller
		if ( CompIndex == 0 ) {
			EIRChillNum = FindItemInList( EIRChillerName, ElecReformEIRChiller );
			if ( EIRChillNum == 0 ) {
				ShowFatalError( "SimReformulatedEIRChiller: Specified Chiller not one of Valid Reformulated EIR Electric Chillers=" + EIRChillerName );
			}
			CompIndex = EIRChillNum;
		} else {
			EIRChillNum = CompIndex;
			if ( EIRChillNum > NumElecReformEIRChillers || EIRChillNum < 1 ) {
				ShowFatalError( "SimReformulatedEIRChiller:  Invalid CompIndex passed=" + TrimSigDigits( EIRChillNum ) + ", Number of Units=" + TrimSigDigits( NumElecReformEIRChillers ) + ", Entered Unit name=" + EIRChillerName );
			}
			if ( EIRChillerName != ElecReformEIRChiller( EIRChillNum ).Name ) {
				ShowFatalError( "SimReformulatedEIRChiller: Invalid CompIndex passed=" + TrimSigDigits( EIRChillNum ) + ", Unit name=" + EIRChillerName + ", stored Unit Name for that index=" + ElecReformEIRChiller( EIRChillNum ).Name );
			}
		}

		if ( InitLoopEquip ) {
			TempEvapOutDesign = ElecReformEIRChiller( EIRChillNum ).TempRefEvapOut;
			TempCondInDesign = ElecReformEIRChiller( EIRChillNum ).TempRefCondIn;
			InitElecReformEIRChiller( EIRChillNum, RunFlag, MyLoad );

			if ( LoopNum == ElecReformEIRChiller( EIRChillNum ).CWLoopNum ) {
				SizeElecReformEIRChiller( EIRChillNum );
				MinCap = ElecReformEIRChiller( EIRChillNum ).RefCap * ElecReformEIRChiller( EIRChillNum ).MinPartLoadRat;
				MaxCap = ElecReformEIRChiller( EIRChillNum ).RefCap * ElecReformEIRChiller( EIRChillNum ).MaxPartLoadRat;
				OptCap = ElecReformEIRChiller( EIRChillNum ).RefCap * ElecReformEIRChiller( EIRChillNum ).OptPartLoadRat;
			} else {
				MinCap = 0.0;
				MaxCap = 0.0;
				OptCap = 0.0;
			}
			if ( GetSizingFactor ) {
				SizingFactor = ElecReformEIRChiller( EIRChillNum ).SizFac;
			}
			return;
		}

		if ( LoopNum == ElecReformEIRChiller( EIRChillNum ).CWLoopNum ) {
			InitElecReformEIRChiller( EIRChillNum, RunFlag, MyLoad );
			ControlReformEIRChillerModel( EIRChillNum, MyLoad, RunFlag, FirstIteration, EquipFlowCtrl );
			UpdateReformEIRChillerRecords( MyLoad, RunFlag, EIRChillNum );
		} else if ( LoopNum == ElecReformEIRChiller( EIRChillNum ).CDLoopNum ) {
			LoopSide = ElecReformEIRChiller( EIRChillNum ).CDLoopSideNum;
			UpdateChillerComponentCondenserSide( LoopNum, LoopSide, TypeOf_Chiller_ElectricReformEIR, ElecReformEIRChiller( EIRChillNum ).CondInletNodeNum, ElecReformEIRChiller( EIRChillNum ).CondOutletNodeNum, ElecReformEIRChillerReport( EIRChillNum ).QCond, ElecReformEIRChillerReport( EIRChillNum ).CondInletTemp, ElecReformEIRChillerReport( EIRChillNum ).CondOutletTemp, ElecReformEIRChillerReport( EIRChillNum ).Condmdot, FirstIteration );
		} else if ( LoopNum == ElecReformEIRChiller( EIRChillNum ).HRLoopNum ) {
			UpdateComponentHeatRecoverySide( ElecReformEIRChiller( EIRChillNum ).HRLoopNum, ElecReformEIRChiller( EIRChillNum ).HRLoopSideNum, TypeOf_Chiller_ElectricReformEIR, ElecReformEIRChiller( EIRChillNum ).HeatRecInletNodeNum, ElecReformEIRChiller( EIRChillNum ).HeatRecOutletNodeNum, ElecReformEIRChillerReport( EIRChillNum ).QHeatRecovery, ElecReformEIRChillerReport( EIRChillNum ).HeatRecInletTemp, ElecReformEIRChillerReport( EIRChillNum ).HeatRecOutletTemp, ElecReformEIRChillerReport( EIRChillNum ).HeatRecMassFlow, FirstIteration );
		}

	}

	// End Reformulated EIR Chiller Module Driver Subroutine
	//******************************************************************************

	void
	GetElecReformEIRChillerInput()
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR:          Lixing Gu, FSEC
		//       DATE WRITTEN:    July 2006

		//       MODIFIED
		//			Aug.  2014, Rongpeng Zhang, added an additional part-load performance curve type

		// PURPOSE OF THIS SUBROUTINE:
		//  This routine will get the input required by the Reformulated Electric EIR Chiller model

		// METHODOLOGY EMPLOYED:

		// REFERENCES: na

		// Using/Aliasing
		using InputProcessor::GetNumObjectsFound;
		using InputProcessor::GetObjectItem;
		using InputProcessor::VerifyName;
		using InputProcessor::SameString;
		using namespace DataIPShortCuts; // Data for field names, blank numerics
		using BranchNodeConnections::TestCompSet;
		using NodeInputManager::GetOnlySingleNode;
		using CurveManager::GetCurveIndex;
		using CurveManager::GetCurveType;
		using FluidProperties::FindGlycol;
		using General::TrimSigDigits;
		using General::RoundSigDigits;
		using PlantUtilities::RegisterPlantCompDesignFlow;
		using GlobalNames::VerifyUniqueChillerName;
		using DataSizing::AutoSize;
		using DataGlobals::AnyEnergyManagementSystemInModel;
		using ScheduleManager::GetScheduleIndex;

		// Locals
		// PARAMETERS
		static std::string const RoutineName( "GetElecReformEIRChillerInput: " ); // include trailing blank space

		// LOCAL VARIABLES
		int EIRChillerNum; // Chiller counter
		int NumAlphas; // Number of elements in the alpha array
		int NumNums; // Number of elements in the numeric array
		int IOStat; // IO Status when calling get input subroutine
		static bool ErrorsFound( false ); // True when input errors found
		bool IsNotOK; // Flag to verify name
		bool IsBlank; // Flag for blank name
		bool errFlag; // Error flag, used to tell if a unique chiller name has been specified
		static bool AllocatedFlag( false ); // True when arrays are allocated
		std::string PartLoadCurveType; // Part load curve type

		// FLOW

		if ( AllocatedFlag ) return;

		cCurrentModuleObject = "Chiller:Electric:ReformulatedEIR";
		NumElecReformEIRChillers = GetNumObjectsFound( cCurrentModuleObject );

		if ( NumElecReformEIRChillers <= 0 ) {
			ShowSevereError( "No " + cCurrentModuleObject + " equipment specified in input file" );
			ErrorsFound = true;
		}

		// ALLOCATE ARRAYS
		ElecReformEIRChiller.allocate( NumElecReformEIRChillers );
		ElecReformEIRChillerReport.allocate( NumElecReformEIRChillers );
		AllocatedFlag = true;

		// Load arrays with reformulated electric EIR chiller data
		for ( EIRChillerNum = 1; EIRChillerNum <= NumElecReformEIRChillers; ++EIRChillerNum ) {
			GetObjectItem( cCurrentModuleObject, EIRChillerNum, cAlphaArgs, NumAlphas, rNumericArgs, NumNums, IOStat, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );
			IsNotOK = false;
			IsBlank = false;
			VerifyName( cAlphaArgs( 1 ), ElecReformEIRChiller, EIRChillerNum - 1, IsNotOK, IsBlank, cCurrentModuleObject + " Name" );
			if ( IsNotOK ) {
				ErrorsFound = true;
				if ( IsBlank ) cAlphaArgs( 1 ) = "xxxxx";
			}
			VerifyUniqueChillerName( cCurrentModuleObject, cAlphaArgs( 1 ), errFlag, cCurrentModuleObject + " Name" );
			if ( errFlag ) {
				ErrorsFound = true;
			}
			ElecReformEIRChiller( EIRChillerNum ).Name = cAlphaArgs( 1 );
			// Performance curves
			ElecReformEIRChiller( EIRChillerNum ).ChillerCapFT = GetCurveIndex( cAlphaArgs( 2 ) );
			ElecReformEIRChiller( EIRChillerNum ).CAPFTName = cAlphaArgs( 2 );
			if ( ElecReformEIRChiller( EIRChillerNum ).ChillerCapFT == 0 ) {
				ShowSevereError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\"" );
				ShowContinueError( "Invalid " + cAlphaFieldNames( 2 ) + '=' + cAlphaArgs( 2 ) );
				ErrorsFound = true;
			}

			ElecReformEIRChiller( EIRChillerNum ).ChillerEIRFT = GetCurveIndex( cAlphaArgs( 3 ) );
			ElecReformEIRChiller( EIRChillerNum ).EIRFTName = cAlphaArgs( 3 );
			if ( ElecReformEIRChiller( EIRChillerNum ).ChillerEIRFT == 0 ) {
				ShowSevereError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\"" );
				ShowContinueError( "Invalid " + cAlphaFieldNames( 3 ) + '=' + cAlphaArgs( 3 ) );
				ErrorsFound = true;
			}

			//The default type of part-load curve is: LeavingCondenserWaterTemperature
			if ( lAlphaFieldBlanks( 4 ) ) {
				PartLoadCurveType = "LeavingCondenserWaterTemperature";
			} else {
				PartLoadCurveType = cAlphaArgs( 4 );
			}

			ElecReformEIRChiller( EIRChillerNum ).EIRFPLRName = cAlphaArgs( 5 );
			ElecReformEIRChiller( EIRChillerNum ).ChillerEIRFPLR = GetCurveIndex( cAlphaArgs( 5 ) );
			if ( ElecReformEIRChiller( EIRChillerNum ).ChillerEIRFPLR == 0 ) {
				ShowSevereError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\"" );
				ShowContinueError( "Invalid " + cAlphaFieldNames( 5 ) + '=' + cAlphaArgs( 5 ) );
				ErrorsFound = true;
			}

			//Check the type of part-load curves implemented: 1_LeavingCondenserWaterTemperature, 2_Lift    zrp_Aug2014
			if ( SameString( PartLoadCurveType, "LeavingCondenserWaterTemperature" ) && SameString( GetCurveType( ElecReformEIRChiller( EIRChillerNum ).ChillerEIRFPLR ), "BICUBIC" ) ) {
				ElecReformEIRChiller( EIRChillerNum ).PartLoadCurveType = PLR_LeavingCondenserWaterTemperature;
			} else if ( SameString( PartLoadCurveType, "Lift" ) && SameString( GetCurveType( ElecReformEIRChiller( EIRChillerNum ).ChillerEIRFPLR ), "CHILLERPARTLOADWITHLIFT" ) ) {
				ElecReformEIRChiller( EIRChillerNum ).PartLoadCurveType = PLR_Lift;
			} else {
				ShowSevereError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\"" );
				ShowContinueError( "Invalid " + cAlphaFieldNames( 5 ) + '=' + cAlphaArgs( 5 ) + " for " + cAlphaFieldNames( 4 ) + '=' + cAlphaArgs( 4 ) );
				ErrorsFound = true;
			}

			// Chilled water inlet/outlet node names are necessary
			if ( lAlphaFieldBlanks( 6 ) ) {
				ShowSevereError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\"" );
				ShowContinueError( cAlphaFieldNames( 6 ) + " is blank." );
				ErrorsFound = true;
			}
			if ( lAlphaFieldBlanks( 7 ) ) {
				ShowSevereError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\"" );
				ShowContinueError( cAlphaFieldNames( 7 ) + " is blank." );
				ErrorsFound = true;
			}

			ElecReformEIRChiller( EIRChillerNum ).EvapInletNodeNum = GetOnlySingleNode( cAlphaArgs( 6 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Water, NodeConnectionType_Inlet, 1, ObjectIsNotParent );
			ElecReformEIRChiller( EIRChillerNum ).EvapOutletNodeNum = GetOnlySingleNode( cAlphaArgs( 7 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Water, NodeConnectionType_Outlet, 1, ObjectIsNotParent );
			TestCompSet( cCurrentModuleObject, cAlphaArgs( 1 ), cAlphaArgs( 6 ), cAlphaArgs( 7 ), "Chilled Water Nodes" );

			ElecReformEIRChiller( EIRChillerNum ).CondenserType = WaterCooled;

			// Condenser inlet/outlet node names are necessary
			if ( lAlphaFieldBlanks( 8 ) ) {
				ShowSevereError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\"" );
				ShowContinueError( cAlphaFieldNames( 8 ) + " is blank." );
				ErrorsFound = true;
			}
			if ( lAlphaFieldBlanks( 9 ) ) {
				ShowSevereError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\"" );
				ShowContinueError( cAlphaFieldNames( 9 ) + " is blank." );
				ErrorsFound = true;
			}

			ElecReformEIRChiller( EIRChillerNum ).CondInletNodeNum = GetOnlySingleNode( cAlphaArgs( 8 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Water, NodeConnectionType_Inlet, 2, ObjectIsNotParent );
			ElecReformEIRChiller( EIRChillerNum ).CondOutletNodeNum = GetOnlySingleNode( cAlphaArgs( 9 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Water, NodeConnectionType_Outlet, 2, ObjectIsNotParent );

			TestCompSet( cCurrentModuleObject, cAlphaArgs( 1 ), cAlphaArgs( 8 ), cAlphaArgs( 9 ), "Condenser Water Nodes" );

			{ auto const SELECT_CASE_var( cAlphaArgs( 10 ) );
			if ( SELECT_CASE_var == "CONSTANTFLOW" ) {
				ElecReformEIRChiller( EIRChillerNum ).FlowMode = ConstantFlow;
			} else if ( SELECT_CASE_var == "VARIABLEFLOW" ) {
				ElecReformEIRChiller( EIRChillerNum ).FlowMode = LeavingSetPointModulated;
				ShowWarningError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\"," );
				ShowContinueError( "Invalid " + cAlphaFieldNames( 10 ) + '=' + cAlphaArgs( 10 ) );
				ShowContinueError( "Key choice is now called \"LeavingSetpointModulated\" and the simulation continues" );
			} else if ( SELECT_CASE_var == "LEAVINGSETPOINTMODULATED" ) {
				ElecReformEIRChiller( EIRChillerNum ).FlowMode = LeavingSetPointModulated;
			} else if ( SELECT_CASE_var == "NOTMODULATED" ) {
				ElecReformEIRChiller( EIRChillerNum ).FlowMode = NotModulated;
			} else {
				ShowSevereError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\"," );
				ShowContinueError( "Invalid " + cAlphaFieldNames( 10 ) + '=' + cAlphaArgs( 10 ) );
				ShowContinueError( "Available choices are ConstantFlow, NotModulated, or LeavingSetpointModulated" );
				ShowContinueError( "Flow mode NotModulated is assumed and the simulation continues." );
				ElecReformEIRChiller( EIRChillerNum ).FlowMode = NotModulated;
			}}

			//   Chiller rated performance data
			ElecReformEIRChiller( EIRChillerNum ).RefCap = rNumericArgs( 1 );
			if ( ElecReformEIRChiller( EIRChillerNum ).RefCap == AutoSize ) {
				ElecReformEIRChiller( EIRChillerNum ).RefCapWasAutoSized = true;
			}
			if ( rNumericArgs( 1 ) == 0.0 ) {
				ShowSevereError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\"" );
				ShowContinueError( "Invalid " + cNumericFieldNames( 1 ) + '=' + RoundSigDigits( rNumericArgs( 1 ), 2 ) );
				ErrorsFound = true;
			}

			ElecReformEIRChiller( EIRChillerNum ).RefCOP = rNumericArgs( 2 );
			if ( rNumericArgs( 2 ) == 0.0 ) {
				ShowSevereError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\"" );
				ShowContinueError( "Invalid " + cNumericFieldNames( 2 ) + '=' + RoundSigDigits( rNumericArgs( 2 ), 2 ) );
				ErrorsFound = true;
			}

			ElecReformEIRChiller( EIRChillerNum ).TempRefEvapOut = rNumericArgs( 3 );
			ElecReformEIRChiller( EIRChillerNum ).TempRefCondOut = rNumericArgs( 4 );
			if ( ElecReformEIRChiller( EIRChillerNum ).TempRefEvapOut >= ElecReformEIRChiller( EIRChillerNum ).TempRefCondOut ) {
				ShowSevereError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\"" );
				ShowContinueError( cNumericFieldNames( 3 ) + " [" + RoundSigDigits( rNumericArgs( 3 ), 2 ) + "] >= " + cNumericFieldNames( 4 ) + " [" + RoundSigDigits( rNumericArgs( 4 ), 2 ) + ']' );
				ShowContinueError( "Reference Leaving Chilled Water Temperature must be less than Reference Leaving Condenser Water Temperature " );
				ErrorsFound = true;
			}

			ElecReformEIRChiller( EIRChillerNum ).EvapVolFlowRate = rNumericArgs( 5 );
			if ( ElecReformEIRChiller( EIRChillerNum ).EvapVolFlowRate == AutoSize ) {
				ElecReformEIRChiller( EIRChillerNum ).EvapVolFlowRateWasAutoSized = true;
			}
			ElecReformEIRChiller( EIRChillerNum ).CondVolFlowRate = rNumericArgs( 6 );
			if ( ElecReformEIRChiller( EIRChillerNum ).CondVolFlowRate == AutoSize ) {
				ElecReformEIRChiller( EIRChillerNum ).CondVolFlowRateWasAutoSized = true;
			}
			ElecReformEIRChiller( EIRChillerNum ).MinPartLoadRat = rNumericArgs( 7 );
			ElecReformEIRChiller( EIRChillerNum ).MaxPartLoadRat = rNumericArgs( 8 );
			ElecReformEIRChiller( EIRChillerNum ).OptPartLoadRat = rNumericArgs( 9 );
			ElecReformEIRChiller( EIRChillerNum ).MinUnloadRat = rNumericArgs( 10 );
			ElecReformEIRChiller( EIRChillerNum ).SizFac = rNumericArgs( 14 );
			if ( ElecReformEIRChiller( EIRChillerNum ).SizFac <= 0.0 ) ElecReformEIRChiller( EIRChillerNum ).SizFac = 1.0;

			if ( ElecReformEIRChiller( EIRChillerNum ).MinPartLoadRat > ElecReformEIRChiller( EIRChillerNum ).MaxPartLoadRat ) {
				ShowSevereError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\"" );
				ShowContinueError( cNumericFieldNames( 7 ) + " [" + RoundSigDigits( rNumericArgs( 7 ), 3 ) + "] > " + cNumericFieldNames( 8 ) + " [" + RoundSigDigits( rNumericArgs( 8 ), 3 ) + ']' );
				ShowContinueError( "Minimum part load ratio must be less than or equal to the maximum part load ratio " );
				ErrorsFound = true;
			}

			if ( ElecReformEIRChiller( EIRChillerNum ).MinUnloadRat < ElecReformEIRChiller( EIRChillerNum ).MinPartLoadRat || ElecReformEIRChiller( EIRChillerNum ).MinUnloadRat > ElecReformEIRChiller( EIRChillerNum ).MaxPartLoadRat ) {
				ShowSevereError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\"" );
				ShowContinueError( cNumericFieldNames( 10 ) + " = " + RoundSigDigits( rNumericArgs( 10 ), 3 ) );
				ShowContinueError( cNumericFieldNames( 10 ) + " must be greater than or equal to the " + cNumericFieldNames( 7 ) );
				ShowContinueError( cNumericFieldNames( 10 ) + " must be less than or equal to the " + cNumericFieldNames( 8 ) );
				ErrorsFound = true;
			}

			if ( ElecReformEIRChiller( EIRChillerNum ).OptPartLoadRat < ElecReformEIRChiller( EIRChillerNum ).MinPartLoadRat || ElecReformEIRChiller( EIRChillerNum ).OptPartLoadRat > ElecReformEIRChiller( EIRChillerNum ).MaxPartLoadRat ) {
				ShowSevereError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\"" );
				ShowContinueError( cNumericFieldNames( 9 ) + " = " + RoundSigDigits( rNumericArgs( 9 ), 3 ) );
				ShowContinueError( cNumericFieldNames( 9 ) + " must be greater than or equal to the " + cNumericFieldNames( 7 ) );
				ShowContinueError( cNumericFieldNames( 9 ) + " must be less than or equal to the " + cNumericFieldNames( 8 ) );
				ErrorsFound = true;
			}

			ElecReformEIRChiller( EIRChillerNum ).CompPowerToCondenserFrac = rNumericArgs( 11 );

			if ( ElecReformEIRChiller( EIRChillerNum ).CompPowerToCondenserFrac < 0.0 || ElecReformEIRChiller( EIRChillerNum ).CompPowerToCondenserFrac > 1.0 ) {
				ShowSevereError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\"" );
				ShowContinueError( cNumericFieldNames( 11 ) + " = " + RoundSigDigits( rNumericArgs( 11 ), 3 ) );
				ShowContinueError( cNumericFieldNames( 11 ) + " must be greater than or equal to zero" );
				ShowContinueError( cNumericFieldNames( 11 ) + " must be less than or equal to one" );
				ErrorsFound = true;
			}

			ElecReformEIRChiller( EIRChillerNum ).TempLowLimitEvapOut = rNumericArgs( 12 );

			// These are the optional heat recovery inputs
			ElecReformEIRChiller( EIRChillerNum ).DesignHeatRecVolFlowRate = rNumericArgs( 13 );
			if ( ElecReformEIRChiller( EIRChillerNum ).DesignHeatRecVolFlowRate == AutoSize ) {
				ElecReformEIRChiller( EIRChillerNum ).DesignHeatRecVolFlowRateWasAutoSized = true;
			}
			if ( ( ElecReformEIRChiller( EIRChillerNum ).DesignHeatRecVolFlowRate > 0.0 ) || ( ElecReformEIRChiller( EIRChillerNum ).DesignHeatRecVolFlowRate == AutoSize ) ) {
				ElecReformEIRChiller( EIRChillerNum ).HeatRecActive = true;
				ElecReformEIRChiller( EIRChillerNum ).HeatRecInletNodeNum = GetOnlySingleNode( cAlphaArgs( 11 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Water, NodeConnectionType_Inlet, 3, ObjectIsNotParent );
				if ( ElecReformEIRChiller( EIRChillerNum ).HeatRecInletNodeNum == 0 ) {
					ShowSevereError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\"" );
					ShowContinueError( "Invalid " + cAlphaFieldNames( 11 ) + '=' + cAlphaArgs( 11 ) );
					ErrorsFound = true;
				}
				ElecReformEIRChiller( EIRChillerNum ).HeatRecOutletNodeNum = GetOnlySingleNode( cAlphaArgs( 12 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Water, NodeConnectionType_Outlet, 3, ObjectIsNotParent );
				if ( ElecReformEIRChiller( EIRChillerNum ).HeatRecOutletNodeNum == 0 ) {
					ShowSevereError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\"" );
					ShowContinueError( "Invalid " + cAlphaFieldNames( 12 ) + '=' + cAlphaArgs( 12 ) );
					ErrorsFound = true;
				}
				if ( ElecReformEIRChiller( EIRChillerNum ).CondenserType != WaterCooled ) {
					ShowSevereError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\"" );
					ShowContinueError( "Heat Recovery requires a Water Cooled Condenser." );
					ErrorsFound = true;
				}

				TestCompSet( cCurrentModuleObject, cAlphaArgs( 1 ), cAlphaArgs( 11 ), cAlphaArgs( 12 ), "Heat Recovery Nodes" );

				if ( ElecReformEIRChiller( EIRChillerNum ).DesignHeatRecVolFlowRate > 0.0 ) {
					RegisterPlantCompDesignFlow( ElecReformEIRChiller( EIRChillerNum ).HeatRecInletNodeNum, ElecReformEIRChiller( EIRChillerNum ).DesignHeatRecVolFlowRate );
				}
				if ( NumNums > 14 ) {
					if ( ! lNumericFieldBlanks( 15 ) ) {
						ElecReformEIRChiller( EIRChillerNum ).HeatRecCapacityFraction = rNumericArgs( 15 );
					} else {
						ElecReformEIRChiller( EIRChillerNum ).HeatRecCapacityFraction = 1.0;
					}
				} else {
					ElecReformEIRChiller( EIRChillerNum ).HeatRecCapacityFraction = 1.0;
				}

				if ( NumAlphas > 12 ) {
					if ( ! lAlphaFieldBlanks( 13 ) ) {
						ElecReformEIRChiller( EIRChillerNum ).HeatRecInletLimitSchedNum = GetScheduleIndex( cAlphaArgs( 13 ) );
						if ( ElecReformEIRChiller( EIRChillerNum ).HeatRecInletLimitSchedNum == 0 ) {
							ShowSevereError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\"" );
							ShowContinueError( "Invalid " + cAlphaFieldNames( 13 ) + '=' + cAlphaArgs( 13 ) );
							ErrorsFound = true;
						}
					} else {
						ElecReformEIRChiller( EIRChillerNum ).HeatRecInletLimitSchedNum = 0;
					}
				} else {
					ElecReformEIRChiller( EIRChillerNum ).HeatRecInletLimitSchedNum = 0;
				}

				if ( NumAlphas > 13 ) {
					if ( ! lAlphaFieldBlanks( 14 ) ) {
						ElecReformEIRChiller( EIRChillerNum ).HeatRecSetPointNodeNum = GetOnlySingleNode( cAlphaArgs( 14 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Water, NodeConnectionType_Sensor, 1, ObjectIsNotParent );
					} else {
						ElecReformEIRChiller( EIRChillerNum ).HeatRecSetPointNodeNum = 0;
					}
				} else {
					ElecReformEIRChiller( EIRChillerNum ).HeatRecSetPointNodeNum = 0;
				}

			} else {
				ElecReformEIRChiller( EIRChillerNum ).HeatRecActive = false;
				ElecReformEIRChiller( EIRChillerNum ).DesignHeatRecMassFlowRate = 0.0;
				ElecReformEIRChiller( EIRChillerNum ).HeatRecInletNodeNum = 0;
				ElecReformEIRChiller( EIRChillerNum ).HeatRecOutletNodeNum = 0;
				if ( ( ! lAlphaFieldBlanks( 11 ) ) || ( ! lAlphaFieldBlanks( 12 ) ) ) {
					ShowWarningError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\"" );
					ShowWarningError( "Since Reference Heat Reclaim Volume Flow Rate = 0.0, heat recovery is inactive." );
					ShowContinueError( "However, node names were specified for heat recovery inlet or outlet nodes." );
				}
			}

		}

		if ( ErrorsFound ) {
			ShowFatalError( "Errors found in processing input for " + cCurrentModuleObject );
		}

		for ( EIRChillerNum = 1; EIRChillerNum <= NumElecReformEIRChillers; ++EIRChillerNum ) {
			SetupOutputVariable( "Chiller Part Load Ratio []", ElecReformEIRChillerReport( EIRChillerNum ).ChillerPartLoadRatio, "System", "Average", ElecReformEIRChiller( EIRChillerNum ).Name );
			SetupOutputVariable( "Chiller Cycling Ratio []", ElecReformEIRChillerReport( EIRChillerNum ).ChillerCyclingRatio, "System", "Average", ElecReformEIRChiller( EIRChillerNum ).Name );
			SetupOutputVariable( "Chiller Electric Power [W]", ElecReformEIRChillerReport( EIRChillerNum ).Power, "System", "Average", ElecReformEIRChiller( EIRChillerNum ).Name );
			SetupOutputVariable( "Chiller Electric Energy [J]", ElecReformEIRChillerReport( EIRChillerNum ).Energy, "System", "Sum", ElecReformEIRChiller( EIRChillerNum ).Name, _, "ELECTRICITY", "Cooling", _, "Plant" );

			SetupOutputVariable( "Chiller Evaporator Cooling Rate [W]", ElecReformEIRChillerReport( EIRChillerNum ).QEvap, "System", "Average", ElecReformEIRChiller( EIRChillerNum ).Name );
			SetupOutputVariable( "Chiller Evaporator Cooling Energy [J]", ElecReformEIRChillerReport( EIRChillerNum ).EvapEnergy, "System", "Sum", ElecReformEIRChiller( EIRChillerNum ).Name, _, "ENERGYTRANSFER", "CHILLERS", _, "Plant" );
			SetupOutputVariable( "Chiller False Load Heat Transfer Rate [W]", ElecReformEIRChillerReport( EIRChillerNum ).ChillerFalseLoadRate, "System", "Average", ElecReformEIRChiller( EIRChillerNum ).Name );
			SetupOutputVariable( "Chiller False Load Heat Transfer Energy [J]", ElecReformEIRChillerReport( EIRChillerNum ).ChillerFalseLoad, "System", "Sum", ElecReformEIRChiller( EIRChillerNum ).Name );
			SetupOutputVariable( "Chiller Evaporator Inlet Temperature [C]", ElecReformEIRChillerReport( EIRChillerNum ).EvapInletTemp, "System", "Average", ElecReformEIRChiller( EIRChillerNum ).Name );
			SetupOutputVariable( "Chiller Evaporator Outlet Temperature [C]", ElecReformEIRChillerReport( EIRChillerNum ).EvapOutletTemp, "System", "Average", ElecReformEIRChiller( EIRChillerNum ).Name );
			SetupOutputVariable( "Chiller Evaporator Mass Flow Rate [kg/s]", ElecReformEIRChillerReport( EIRChillerNum ).Evapmdot, "System", "Average", ElecReformEIRChiller( EIRChillerNum ).Name );

			SetupOutputVariable( "Chiller Condenser Heat Transfer Rate [W]", ElecReformEIRChillerReport( EIRChillerNum ).QCond, "System", "Average", ElecReformEIRChiller( EIRChillerNum ).Name );
			SetupOutputVariable( "Chiller Condenser Heat Transfer Energy [J]", ElecReformEIRChillerReport( EIRChillerNum ).CondEnergy, "System", "Sum", ElecReformEIRChiller( EIRChillerNum ).Name, _, "ENERGYTRANSFER", "HEATREJECTION", _, "Plant" );
			SetupOutputVariable( "Chiller COP [W/W]", ElecReformEIRChillerReport( EIRChillerNum ).ActualCOP, "System", "Average", ElecReformEIRChiller( EIRChillerNum ).Name );

			SetupOutputVariable( "Chiller Capacity Temperature Modifier Multiplier []", ElecReformEIRChillerReport( EIRChillerNum ).ChillerCapFT, "System", "Average", ElecReformEIRChiller( EIRChillerNum ).Name );
			SetupOutputVariable( "Chiller EIR Temperature Modifier Multiplier []", ElecReformEIRChillerReport( EIRChillerNum ).ChillerEIRFT, "System", "Average", ElecReformEIRChiller( EIRChillerNum ).Name );
			SetupOutputVariable( "Chiller EIR Part Load Modifier Multiplier []", ElecReformEIRChillerReport( EIRChillerNum ).ChillerEIRFPLR, "System", "Average", ElecReformEIRChiller( EIRChillerNum ).Name );

			SetupOutputVariable( "Chiller Condenser Inlet Temperature [C]", ElecReformEIRChillerReport( EIRChillerNum ).CondInletTemp, "System", "Average", ElecReformEIRChiller( EIRChillerNum ).Name );
			SetupOutputVariable( "Chiller Condenser Outlet Temperature [C]", ElecReformEIRChillerReport( EIRChillerNum ).CondOutletTemp, "System", "Average", ElecReformEIRChiller( EIRChillerNum ).Name );
			SetupOutputVariable( "Chiller Condenser Mass Flow Rate [kg/s]", ElecReformEIRChillerReport( EIRChillerNum ).Condmdot, "System", "Average", ElecReformEIRChiller( EIRChillerNum ).Name );

			// If heat recovery is active then setup report variables
			if ( ElecReformEIRChiller( EIRChillerNum ).HeatRecActive ) {
				SetupOutputVariable( "Chiller Total Recovered Heat Rate [W]", ElecReformEIRChillerReport( EIRChillerNum ).QHeatRecovery, "System", "Average", ElecReformEIRChiller( EIRChillerNum ).Name );
				SetupOutputVariable( "Chiller Total Recovered Heat Energy [J]", ElecReformEIRChillerReport( EIRChillerNum ).EnergyHeatRecovery, "System", "Sum", ElecReformEIRChiller( EIRChillerNum ).Name, _, "ENERGYTRANSFER", "HEATRECOVERY", _, "Plant" );
				SetupOutputVariable( "Chiller Heat Recovery Inlet Temperature [C]", ElecReformEIRChillerReport( EIRChillerNum ).HeatRecInletTemp, "System", "Average", ElecReformEIRChiller( EIRChillerNum ).Name );
				SetupOutputVariable( "Chiller Heat Recovery Outlet Temperature [C]", ElecReformEIRChillerReport( EIRChillerNum ).HeatRecOutletTemp, "System", "Average", ElecReformEIRChiller( EIRChillerNum ).Name );
				SetupOutputVariable( "Chiller Heat Recovery Mass Flow Rate [kg/s]", ElecReformEIRChillerReport( EIRChillerNum ).HeatRecMassFlow, "System", "Average", ElecReformEIRChiller( EIRChillerNum ).Name );
				SetupOutputVariable( "Chiller Effective Heat Rejection Temperature [C]", ElecReformEIRChillerReport( EIRChillerNum ).ChillerCondAvgTemp, "System", "Average", ElecReformEIRChiller( EIRChillerNum ).Name );
			}

			if ( AnyEnergyManagementSystemInModel ) {
				SetupEMSInternalVariable( "Chiller Nominal Capacity", ElecReformEIRChiller( EIRChillerNum ).Name, "[W]", ElecReformEIRChiller( EIRChillerNum ).RefCap );
			}

		}

	}

	void
	InitElecReformEIRChiller(
		int const EIRChillNum, // Number of the current electric EIR chiller being simulated
		bool const RunFlag, // TRUE when chiller operating
		Real64 const MyLoad // Current load put on chiller
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Lixing Gu, FSEC
		//       DATE WRITTEN   July 2006
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		//  This subroutine is for initializations of the Reformulated Electric EIR Chiller variables

		// METHODOLOGY EMPLOYED:
		//  Uses the status flags to trigger initializations.

		// REFERENCES:
		//  na

		// Using/Aliasing
		using DataGlobals::BeginEnvrnFlag;
		using DataGlobals::AnyEnergyManagementSystemInModel;
		using DataPlant::PlantLoop;
		using DataPlant::TypeOf_Chiller_ElectricReformEIR;
		using DataPlant::ScanPlantLoopsForObject;
		using DataPlant::PlantFirstSizesOkayToFinalize;
		using DataPlant::LoopFlowStatus_NeedyIfLoopOn;
		using PlantUtilities::InterConnectTwoPlantLoopSides;
		using PlantUtilities::InitComponentNodes;
		using PlantUtilities::SetComponentFlowRate;
		using DataEnvironment::StdBaroPress;
		using EMSManager::iTemperatureSetPoint;
		using EMSManager::CheckIfNodeSetPointManagedByEMS;
		using ScheduleManager::GetCurrentScheduleValue;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		//  na

		// DERIVED TYPE DEFINITIONS:
		//  na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		static std::string const RoutineName( "InitElecReformEIRChiller" );
		static bool MyOneTimeFlag( true ); // One time logic flag for allocating MyEnvrnFlag array
		static Array1D_bool MyFlag;
		static Array1D_bool MyEnvrnFlag; // Logical array to initialize when appropriate
		int EvapInletNode; // Node number for evaporator water inlet node
		int EvapOutletNode; // Node number for evaporator water outlet node
		int CondInletNode; // Node number for condenser water inlet node
		int CondOutletNode; // Node number for condenser water outlet node
		int HeatRecInNode; // Node number for heat recovery water inlet node
		int HeatRecOutNode; // Node number for heat recovery water outlet node
		Real64 rho; // local fluid density
		Real64 mdot; // local fluid mass flow rate
		Real64 mdotCond; // local fluid mass flow rate for condenser
		int LoopNum;
		int LoopSideNum;
		int BranchIndex;
		int CompIndex;
		bool FatalError;
		bool errFlag;
		bool HeatRecRunFlag;
		Real64 HeatRecHighInletLimit;
		// FLOW:

		// Do the one time initializations
		if ( MyOneTimeFlag ) {
			MyEnvrnFlag.allocate( NumElecReformEIRChillers );
			MyFlag.allocate( NumElecReformEIRChillers );
			MyEnvrnFlag = true;
			MyFlag = true;
			MyOneTimeFlag = false;
		}

		// Initialize condenser nodes
		EvapInletNode = ElecReformEIRChiller( EIRChillNum ).EvapInletNodeNum;
		EvapOutletNode = ElecReformEIRChiller( EIRChillNum ).EvapOutletNodeNum;
		CondInletNode = ElecReformEIRChiller( EIRChillNum ).CondInletNodeNum;
		CondOutletNode = ElecReformEIRChiller( EIRChillNum ).CondOutletNodeNum;

		if ( ElecReformEIRChiller( EIRChillNum ).HeatRecActive ) {
			HeatRecInNode = ElecReformEIRChiller( EIRChillNum ).HeatRecInletNodeNum;
			HeatRecOutNode = ElecReformEIRChiller( EIRChillNum ).HeatRecOutletNodeNum;
		}

		// Init more variables
		if ( MyFlag( EIRChillNum ) ) {
			// Locate the chillers on the plant loops for later usage
			errFlag = false;
			ScanPlantLoopsForObject( ElecReformEIRChiller( EIRChillNum ).Name, TypeOf_Chiller_ElectricReformEIR, ElecReformEIRChiller( EIRChillNum ).CWLoopNum, ElecReformEIRChiller( EIRChillNum ).CWLoopSideNum, ElecReformEIRChiller( EIRChillNum ).CWBranchNum, ElecReformEIRChiller( EIRChillNum ).CWCompNum, ElecReformEIRChiller( EIRChillNum ).TempLowLimitEvapOut, _, _, ElecReformEIRChiller( EIRChillNum ).EvapInletNodeNum, _, errFlag );
			if ( ElecReformEIRChiller( EIRChillNum ).CondenserType != AirCooled ) {
				ScanPlantLoopsForObject( ElecReformEIRChiller( EIRChillNum ).Name, TypeOf_Chiller_ElectricReformEIR, ElecReformEIRChiller( EIRChillNum ).CDLoopNum, ElecReformEIRChiller( EIRChillNum ).CDLoopSideNum, ElecReformEIRChiller( EIRChillNum ).CDBranchNum, ElecReformEIRChiller( EIRChillNum ).CDCompNum, _, _, _, ElecReformEIRChiller( EIRChillNum ).CondInletNodeNum, _, errFlag );
				InterConnectTwoPlantLoopSides( ElecReformEIRChiller( EIRChillNum ).CWLoopNum, ElecReformEIRChiller( EIRChillNum ).CWLoopSideNum, ElecReformEIRChiller( EIRChillNum ).CDLoopNum, ElecReformEIRChiller( EIRChillNum ).CDLoopSideNum, TypeOf_Chiller_ElectricReformEIR, true );
			}
			if ( ElecReformEIRChiller( EIRChillNum ).HeatRecActive ) {
				ScanPlantLoopsForObject( ElecReformEIRChiller( EIRChillNum ).Name, TypeOf_Chiller_ElectricReformEIR, ElecReformEIRChiller( EIRChillNum ).HRLoopNum, ElecReformEIRChiller( EIRChillNum ).HRLoopSideNum, ElecReformEIRChiller( EIRChillNum ).HRBranchNum, ElecReformEIRChiller( EIRChillNum ).HRCompNum, _, _, _, ElecReformEIRChiller( EIRChillNum ).HeatRecInletNodeNum, _, errFlag );
				InterConnectTwoPlantLoopSides( ElecReformEIRChiller( EIRChillNum ).CWLoopNum, ElecReformEIRChiller( EIRChillNum ).CWLoopSideNum, ElecReformEIRChiller( EIRChillNum ).HRLoopNum, ElecReformEIRChiller( EIRChillNum ).HRLoopSideNum, TypeOf_Chiller_ElectricReformEIR, true );
			}

			if ( ( ElecReformEIRChiller( EIRChillNum ).CondenserType != AirCooled ) && ( ElecReformEIRChiller( EIRChillNum ).HeatRecActive ) ) {
				InterConnectTwoPlantLoopSides( ElecReformEIRChiller( EIRChillNum ).CDLoopNum, ElecReformEIRChiller( EIRChillNum ).CDLoopSideNum, ElecReformEIRChiller( EIRChillNum ).HRLoopNum, ElecReformEIRChiller( EIRChillNum ).HRLoopSideNum, TypeOf_Chiller_ElectricReformEIR, false );
			}

			if ( errFlag ) {
				ShowFatalError( "InitElecReformEIRChiller: Program terminated due to previous condition(s)." );
			}

			if ( ElecReformEIRChiller( EIRChillNum ).FlowMode == ConstantFlow ) {
				// reset flow priority
				PlantLoop( ElecReformEIRChiller( EIRChillNum ).CWLoopNum ).LoopSide( ElecReformEIRChiller( EIRChillNum ).CWLoopSideNum ).Branch( ElecReformEIRChiller( EIRChillNum ).CWBranchNum ).Comp( ElecReformEIRChiller( EIRChillNum ).CWCompNum ).FlowPriority = LoopFlowStatus_NeedyIfLoopOn;
			}

			if ( ElecReformEIRChiller( EIRChillNum ).FlowMode == LeavingSetPointModulated ) {
				// reset flow priority
				PlantLoop( ElecReformEIRChiller( EIRChillNum ).CWLoopNum ).LoopSide( ElecReformEIRChiller( EIRChillNum ).CWLoopSideNum ).Branch( ElecReformEIRChiller( EIRChillNum ).CWBranchNum ).Comp( ElecReformEIRChiller( EIRChillNum ).CWCompNum ).FlowPriority = LoopFlowStatus_NeedyIfLoopOn;
				// check if setpoint on outlet node
				if ( ( Node( ElecReformEIRChiller( EIRChillNum ).EvapOutletNodeNum ).TempSetPoint == SensedNodeFlagValue ) && ( Node( ElecReformEIRChiller( EIRChillNum ).EvapOutletNodeNum ).TempSetPointHi == SensedNodeFlagValue ) ) {
					if ( ! AnyEnergyManagementSystemInModel ) {
						if ( ! ElecReformEIRChiller( EIRChillNum ).ModulatedFlowErrDone ) {
							ShowWarningError( "Missing temperature setpoint for LeavingSetpointModulated mode chiller named " + ElecReformEIRChiller( EIRChillNum ).Name );
							ShowContinueError( "  A temperature setpoint is needed at the outlet node of a chiller in variable flow mode, use a SetpointManager" );
							ShowContinueError( "  The overall loop setpoint will be assumed for chiller. The simulation continues ... " );
							ElecReformEIRChiller( EIRChillNum ).ModulatedFlowErrDone = true;
						}
					} else {
						// need call to EMS to check node
						FatalError = false; // but not really fatal yet, but should be.
						CheckIfNodeSetPointManagedByEMS( ElecReformEIRChiller( EIRChillNum ).EvapOutletNodeNum, iTemperatureSetPoint, FatalError );
						if ( FatalError ) {
							if ( ! ElecReformEIRChiller( EIRChillNum ).ModulatedFlowErrDone ) {
								ShowWarningError( "Missing temperature setpoint for LeavingSetpointModulated mode chiller named " + ElecReformEIRChiller( EIRChillNum ).Name );
								ShowContinueError( "  A temperature setpoint is needed at the outlet node of a chiller evaporator in variable flow mode" );
								ShowContinueError( "  use a Setpoint Manager to establish a setpoint at the chiller evaporator outlet node " );
								ShowContinueError( "  or use an EMS actuator to establish a setpoint at the outlet node " );
								ShowContinueError( "  The overall loop setpoint will be assumed for chiller. The simulation continues ... " );
								ElecReformEIRChiller( EIRChillNum ).ModulatedFlowErrDone = true;
							}
						}

					}
					ElecReformEIRChiller( EIRChillNum ).ModulatedFlowSetToLoop = true;
					Node( ElecReformEIRChiller( EIRChillNum ).EvapOutletNodeNum ).TempSetPoint = Node( PlantLoop( ElecReformEIRChiller( EIRChillNum ).CWLoopNum ).TempSetPointNodeNum ).TempSetPoint;
					Node( ElecReformEIRChiller( EIRChillNum ).EvapOutletNodeNum ).TempSetPointHi = Node( PlantLoop( ElecReformEIRChiller( EIRChillNum ).CWLoopNum ).TempSetPointNodeNum ).TempSetPointHi;
				}
			}
			MyFlag( EIRChillNum ) = false;
		}

		// Initialize Demand Side Variables
		//  IF((MyEnvrnFlag(EIRChillNum) .and. BeginEnvrnFlag) &
		//     .OR. (Node(CondInletNode)%MassFlowrate <= 0.0 .AND. RunFlag)) THEN

		if ( MyEnvrnFlag( EIRChillNum ) && BeginEnvrnFlag && ( PlantFirstSizesOkayToFinalize ) ) {

			rho = GetDensityGlycol( PlantLoop( ElecReformEIRChiller( EIRChillNum ).CWLoopNum ).FluidName, InitConvTemp, PlantLoop( ElecReformEIRChiller( EIRChillNum ).CWLoopNum ).FluidIndex, RoutineName );

			ElecReformEIRChiller( EIRChillNum ).EvapMassFlowRateMax = ElecReformEIRChiller( EIRChillNum ).EvapVolFlowRate * rho;

			InitComponentNodes( 0.0, ElecReformEIRChiller( EIRChillNum ).EvapMassFlowRateMax, EvapInletNode, EvapOutletNode, ElecReformEIRChiller( EIRChillNum ).CWLoopNum, ElecReformEIRChiller( EIRChillNum ).CWLoopSideNum, ElecReformEIRChiller( EIRChillNum ).CWBranchNum, ElecReformEIRChiller( EIRChillNum ).CWCompNum );

			if ( ElecReformEIRChiller( EIRChillNum ).CondenserType == WaterCooled ) {

				rho = GetDensityGlycol( PlantLoop( ElecReformEIRChiller( EIRChillNum ).CDLoopNum ).FluidName, ElecReformEIRChiller( EIRChillNum ).TempRefCondIn, PlantLoop( ElecReformEIRChiller( EIRChillNum ).CDLoopNum ).FluidIndex, RoutineName );
				ElecReformEIRChiller( EIRChillNum ).CondMassFlowRateMax = rho * ElecReformEIRChiller( EIRChillNum ).CondVolFlowRate;
				InitComponentNodes( 0.0, ElecReformEIRChiller( EIRChillNum ).CondMassFlowRateMax, CondInletNode, CondOutletNode, ElecReformEIRChiller( EIRChillNum ).CDLoopNum, ElecReformEIRChiller( EIRChillNum ).CDLoopSideNum, ElecReformEIRChiller( EIRChillNum ).CDBranchNum, ElecReformEIRChiller( EIRChillNum ).CDCompNum );
				Node( CondInletNode ).Temp = ElecReformEIRChiller( EIRChillNum ).TempRefCondIn;
			} else { // air or evap air condenser
				// Initialize maximum available condenser flow rate
				Node( CondInletNode ).MassFlowRate = ElecReformEIRChiller( EIRChillNum ).CondVolFlowRate * PsyRhoAirFnPbTdbW( StdBaroPress, ElecReformEIRChiller( EIRChillNum ).TempRefCondIn, 0.0, RoutineName );
				Node( CondOutletNode ).MassFlowRate = Node( CondInletNode ).MassFlowRate;
				Node( CondInletNode ).MassFlowRateMaxAvail = Node( CondInletNode ).MassFlowRate;
				Node( CondInletNode ).MassFlowRateMax = Node( CondInletNode ).MassFlowRate;
				Node( CondOutletNode ).MassFlowRateMax = Node( CondInletNode ).MassFlowRate;
				Node( CondInletNode ).MassFlowRateMinAvail = 0.0;
				Node( CondInletNode ).MassFlowRateMin = 0.0;
				Node( CondOutletNode ).MassFlowRateMinAvail = 0.0;
				Node( CondOutletNode ).MassFlowRateMin = 0.0;
				Node( CondInletNode ).Temp = ElecReformEIRChiller( EIRChillNum ).TempRefCondIn;

			}

			if ( ElecReformEIRChiller( EIRChillNum ).HeatRecActive ) {
				rho = GetDensityGlycol( PlantLoop( ElecReformEIRChiller( EIRChillNum ).HRLoopNum ).FluidName, InitConvTemp, PlantLoop( ElecReformEIRChiller( EIRChillNum ).HRLoopNum ).FluidIndex, RoutineName );
				ElecReformEIRChiller( EIRChillNum ).DesignHeatRecMassFlowRate = rho * ElecReformEIRChiller( EIRChillNum ).DesignHeatRecVolFlowRate;
				InitComponentNodes( 0.0, ElecReformEIRChiller( EIRChillNum ).DesignHeatRecMassFlowRate, ElecReformEIRChiller( EIRChillNum ).HeatRecInletNodeNum, ElecReformEIRChiller( EIRChillNum ).HeatRecOutletNodeNum, ElecReformEIRChiller( EIRChillNum ).HRLoopNum, ElecReformEIRChiller( EIRChillNum ).HRLoopSideNum, ElecReformEIRChiller( EIRChillNum ).HRBranchNum, ElecReformEIRChiller( EIRChillNum ).HRCompNum );
				// overall capacity limit
				ElecReformEIRChiller( EIRChillNum ).HeatRecMaxCapacityLimit = ElecReformEIRChiller( EIRChillNum ).HeatRecCapacityFraction * ( ElecReformEIRChiller( EIRChillNum ).RefCap + ElecReformEIRChiller( EIRChillNum ).RefCap / ElecReformEIRChiller( EIRChillNum ).RefCOP );
			}

			MyEnvrnFlag( EIRChillNum ) = false;
		}
		if ( ! BeginEnvrnFlag ) {
			MyEnvrnFlag( EIRChillNum ) = true;
		}

		if ( ( ElecReformEIRChiller( EIRChillNum ).FlowMode == LeavingSetPointModulated ) && ElecReformEIRChiller( EIRChillNum ).ModulatedFlowSetToLoop ) {
			// fix for clumsy old input that worked because loop setpoint was spread.
			//  could be removed with transition, testing , model change, period of being obsolete.
			Node( ElecReformEIRChiller( EIRChillNum ).EvapOutletNodeNum ).TempSetPoint = Node( PlantLoop( ElecReformEIRChiller( EIRChillNum ).CWLoopNum ).TempSetPointNodeNum ).TempSetPoint;
			Node( ElecReformEIRChiller( EIRChillNum ).EvapOutletNodeNum ).TempSetPointHi = Node( PlantLoop( ElecReformEIRChiller( EIRChillNum ).CWLoopNum ).TempSetPointNodeNum ).TempSetPointHi;
		}

		if ( ( std::abs( MyLoad ) > 0.0 ) && RunFlag ) {
			mdot = ElecReformEIRChiller( EIRChillNum ).EvapMassFlowRateMax;
			mdotCond = ElecReformEIRChiller( EIRChillNum ).CondMassFlowRateMax;
		} else {
			mdot = 0.0;
			mdotCond = 0.0;
		}

		SetComponentFlowRate( mdot, EvapInletNode, EvapOutletNode, ElecReformEIRChiller( EIRChillNum ).CWLoopNum, ElecReformEIRChiller( EIRChillNum ).CWLoopSideNum, ElecReformEIRChiller( EIRChillNum ).CWBranchNum, ElecReformEIRChiller( EIRChillNum ).CWCompNum );

		if ( ElecReformEIRChiller( EIRChillNum ).CondenserType == WaterCooled ) {
			SetComponentFlowRate( mdotCond, CondInletNode, CondOutletNode, ElecReformEIRChiller( EIRChillNum ).CDLoopNum, ElecReformEIRChiller( EIRChillNum ).CDLoopSideNum, ElecReformEIRChiller( EIRChillNum ).CDBranchNum, ElecReformEIRChiller( EIRChillNum ).CDCompNum );
		}
		// Initialize heat recovery flow rates at node
		if ( ElecReformEIRChiller( EIRChillNum ).HeatRecActive ) {
			LoopNum = ElecReformEIRChiller( EIRChillNum ).HRLoopNum;
			LoopSideNum = ElecReformEIRChiller( EIRChillNum ).HRLoopSideNum;
			BranchIndex = ElecReformEIRChiller( EIRChillNum ).HRBranchNum;
			CompIndex = ElecReformEIRChiller( EIRChillNum ).HRCompNum;

			// check if inlet limit active and if exceeded.
			if ( ElecReformEIRChiller( EIRChillNum ).HeatRecInletLimitSchedNum > 0 ) {
				HeatRecHighInletLimit = GetCurrentScheduleValue( ElecReformEIRChiller( EIRChillNum ).HeatRecInletLimitSchedNum );
				if ( Node( HeatRecInNode ).Temp > HeatRecHighInletLimit ) { // shut down heat recovery
					HeatRecRunFlag = false;
				} else {
					HeatRecRunFlag = RunFlag;
				}
			} else {
				HeatRecRunFlag = RunFlag;
			}

			if ( HeatRecRunFlag ) {
				mdot = ElecReformEIRChiller( EIRChillNum ).DesignHeatRecMassFlowRate;
			} else {
				mdot = 0.0;
			}

			SetComponentFlowRate( mdot, HeatRecInNode, HeatRecOutNode, LoopNum, LoopSideNum, BranchIndex, CompIndex );

		}

	}

	void
	SizeElecReformEIRChiller( int const EIRChillNum )
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Richard Raustad, FSEC
		//       DATE WRITTEN   June 2004
		//       MODIFIED       July 2006, L. Gu, modified for reformulated EIR chiller
		//                      November 2013 Daeho Kang, add component sizing table entries
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		//  This subroutine is for sizing Reformulated Electric EIR Chiller Components for which capacities and flow rates
		//  have not been specified in the input.

		// METHODOLOGY EMPLOYED:
		//  Obtains evaporator flow rate from the plant sizing array. Calculates reference capacity from
		//  the evaporator flow rate and the chilled water loop design delta T. The condenser flow rate
		//  is calculated from the reference capacity, the COP, and the condenser loop design delta T.

		// REFERENCES:
		//  na

		// Using/Aliasing
		using namespace DataSizing;
		using DataPlant::PlantLoop;
		using DataPlant::PlantFirstSizesOkayToFinalize;
		using DataPlant::PlantFirstSizesOkayToReport;
		using DataPlant::PlantFinalSizesOkayToReport;
		using DataPlant::TypeOf_Chiller_ElectricReformEIR;
		using PlantUtilities::RegisterPlantCompDesignFlow;
		using ReportSizingManager::ReportSizingOutput;
		using CurveManager::CurveValue;
		using CurveManager::GetCurveMinMaxValues;
		using namespace OutputReportPredefined;
		using StandardRatings::CalcChillerIPLV;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "SizeElecReformEIRChiller" );

		// INTERFACE BLOCK SPECIFICATIONS:
		//  na

		// DERIVED TYPE DEFINITIONS:
		//  na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int PltSizNum( 0 ); // Plant Sizing index corresponding to CurLoopNum
		int PltSizCondNum( 0 ); // Plant Sizing index for condenser loop
		bool ErrorsFound( false ); // If errors detected in input
		Real64 SizingEvapOutletTemp; // Plant Sizing outlet temperature for CurLoopNum [C]
		Real64 SizingCondOutletTemp; // Plant Sizing outlet temperature for condenser loop [C]
		Real64 RefCapFT; // Capacity as a function of temperature curve output used for sizing
		std::string equipName; // Name of chiller
		Real64 CurveVal; // Used to verify EIR-FT/CAP-FT curves = 1 at reference conditions
		Real64 CondTemp; // Used to verify EIRFPLR curve is > than 0 at reference conditions
		static bool FoundNegValue( false ); // Used to evaluate EIRFPLR curve objects
		static int CurveCheck( 0 ); // Used to evaluate EIRFPLR curve objects
		Array1D< Real64 > CurveValArray( 11 ); // Used to evaluate EIRFPLR curve objects
		Array1D< Real64 > CondTempArray( 11 ); // Used to evaluate EIRFPLR curve objects
		Real64 CurveValTmp; // Used to evaluate EIRFPLR curve objects
		Real64 Density; // Density of condenser water used in warning messages
		Real64 SpecificHeat; // Specific heat of condenser water used in warning messages
		Real64 CondenserCapacity; // Full load (reference) condenser capacity used in warning messages
		std::string StringVar; // Used for EIRFPLR warning messages
		int CurveValPtr; // Index to EIRFPLR curve output
		Real64 DeltaTCond; // Full load delta T at condenser, used for checking curve objects
		Real64 PLRTemp; // Temporary variable used for warning messages
		Real64 rho;
		Real64 Cp;
		Real64 tmpNomCap; // local nominal capacity cooling power
		Real64 tmpEvapVolFlowRate; // local evaporator design volume flow rate
		Real64 tmpCondVolFlowRate; // local condenser design volume flow rate
		Real64 tmpHeatRecVolFlowRate; // local heat recovery design volume flow rate
		static bool MyOneTimeFlag( true );
		static Array1D_bool MyFlag; // TRUE in order to calculate IPLV
		Real64 EvapVolFlowRateUser( 0.0 ); // Hardsized evaporator flow for reporting
		Real64 RefCapUser( 0.0 ); // Hardsized reference capacity for reporting
		Real64 CondVolFlowRateUser( 0.0 ); // Hardsized condenser flow for reporting
		Real64 DesignHeatRecVolFlowRateUser( 0.0 ); // Hardsized design heat recovery flow for reporting

		// Formats
		static gio::Fmt Format_530( "('Cond Temp (C) = ',11(F7.2))" );
		static gio::Fmt Format_531( "('Curve Output  = ',11(F7.2))" );

		if ( MyOneTimeFlag ) {
			MyFlag.dimension( NumElecReformEIRChillers, true );
			MyOneTimeFlag = false;
		}

		tmpNomCap = ElecReformEIRChiller( EIRChillNum ).RefCap;
		tmpEvapVolFlowRate = ElecReformEIRChiller( EIRChillNum ).EvapVolFlowRate;
		tmpCondVolFlowRate = ElecReformEIRChiller( EIRChillNum ).CondVolFlowRate;
		PltSizCondNum = PlantLoop( ElecReformEIRChiller( EIRChillNum ).CDLoopNum ).PlantSizNum;

		// find the appropriate Plant Sizing object
		PltSizNum = PlantLoop( ElecReformEIRChiller( EIRChillNum ).CWLoopNum ).PlantSizNum;

		if ( PltSizNum > 0 ) {
			if ( PlantSizData( PltSizNum ).DesVolFlowRate >= SmallWaterVolFlow ) {
				tmpEvapVolFlowRate = PlantSizData( PltSizNum ).DesVolFlowRate * ElecReformEIRChiller( EIRChillNum ).SizFac;
				if ( ! ElecReformEIRChiller( EIRChillNum ).EvapVolFlowRateWasAutoSized ) tmpEvapVolFlowRate = ElecReformEIRChiller( EIRChillNum ).EvapVolFlowRate;

			} else {
				if ( ElecReformEIRChiller( EIRChillNum ).EvapVolFlowRateWasAutoSized ) tmpEvapVolFlowRate = 0.0;

			}
			if ( PlantFirstSizesOkayToFinalize ) {
				if ( ElecReformEIRChiller( EIRChillNum ).EvapVolFlowRateWasAutoSized ) {
					ElecReformEIRChiller( EIRChillNum ).EvapVolFlowRate = tmpEvapVolFlowRate;
					if ( PlantFinalSizesOkayToReport ) {
						ReportSizingOutput( "Chiller:Electric:ReformulatedEIR", ElecReformEIRChiller( EIRChillNum ).Name,
						"Design Size Reference Chilled Water Flow Rate [m3/s]", tmpEvapVolFlowRate );
					}
					if ( PlantFirstSizesOkayToReport ) {
						ReportSizingOutput( "Chiller:Electric:ReformulatedEIR", ElecReformEIRChiller( EIRChillNum ).Name,
						"Initial Design Size Reference Chilled Water Flow Rate [m3/s]", tmpEvapVolFlowRate );
					}
				} else { // Hard-size with sizing data
					if ( ElecReformEIRChiller( EIRChillNum ).EvapVolFlowRate > 0.0 && tmpEvapVolFlowRate > 0.0 ) {
						EvapVolFlowRateUser = ElecReformEIRChiller( EIRChillNum ).EvapVolFlowRate;
						if ( PlantFinalSizesOkayToReport ) {
							ReportSizingOutput( "Chiller:Electric:ReformulatedEIR", ElecReformEIRChiller( EIRChillNum ).Name,
								"Design Size Reference Chilled Water Flow Rate [m3/s]", tmpEvapVolFlowRate,
								"User-Specified Reference Chilled Water Flow Rate [m3/s]", EvapVolFlowRateUser );
							if ( DisplayExtraWarnings ) {
								if ( ( std::abs( tmpEvapVolFlowRate - EvapVolFlowRateUser ) / EvapVolFlowRateUser ) > AutoVsHardSizingThreshold ) {
									ShowMessage( "SizeChillerElectricReformulatedEIR: Potential issue with equipment sizing for " + ElecReformEIRChiller( EIRChillNum ).Name );
									ShowContinueError( "User-Specified Reference Chilled Water Flow Rate of " + RoundSigDigits( EvapVolFlowRateUser, 5 ) + " [m3/s]" );
									ShowContinueError( "differs from Design Size Reference Chilled Water Flow Rate of " + RoundSigDigits( tmpEvapVolFlowRate, 5 ) + " [m3/s]" );
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
			if ( ElecReformEIRChiller( EIRChillNum ).EvapVolFlowRateWasAutoSized && PlantFirstSizesOkayToFinalize ) {
				ShowSevereError( "Autosizing of Reformulated Electric Chiller evap flow rate requires a loop Sizing:Plant object" );
				ShowContinueError( "Occurs in Reformulated Electric Chiller object=" + ElecReformEIRChiller( EIRChillNum ).Name );
				ErrorsFound = true;
			}
			if ( ! ElecReformEIRChiller( EIRChillNum ).EvapVolFlowRateWasAutoSized && PlantFinalSizesOkayToReport
					&&  ( ElecReformEIRChiller( EIRChillNum ).EvapVolFlowRate > 0.0 ) ) { // Hard-size with sizing data
				ReportSizingOutput( "Chiller:Electric:ReformulatedEIR", ElecReformEIRChiller( EIRChillNum ).Name,
					"User-Specified Reference Chilled Water Flow Rate [m3/s]", ElecReformEIRChiller( EIRChillNum ).EvapVolFlowRate );
			}
		}

		RegisterPlantCompDesignFlow( ElecReformEIRChiller( EIRChillNum ).EvapInletNodeNum, tmpEvapVolFlowRate );

		if ( PltSizNum > 0 ) {
			if ( PlantSizData( PltSizNum ).DesVolFlowRate >= SmallWaterVolFlow ) {
				if ( PltSizCondNum > 0 && PltSizNum > 0 ) {
					SizingEvapOutletTemp = PlantSizData( PltSizNum ).ExitTemp;
					SizingCondOutletTemp = PlantSizData( PltSizCondNum ).ExitTemp + PlantSizData( PltSizCondNum ).DeltaT;
				} else {
					SizingEvapOutletTemp = ElecReformEIRChiller( EIRChillNum ).TempRefEvapOut;
					SizingCondOutletTemp = ElecReformEIRChiller( EIRChillNum ).TempRefCondOut;
				}
				Cp = GetSpecificHeatGlycol( PlantLoop( ElecReformEIRChiller( EIRChillNum ).CWLoopNum ).FluidName, InitConvTemp, PlantLoop( ElecReformEIRChiller( EIRChillNum ).CWLoopNum ).FluidIndex, RoutineName );

				rho = GetDensityGlycol( PlantLoop( ElecReformEIRChiller( EIRChillNum ).CWLoopNum ).FluidName, InitConvTemp, PlantLoop( ElecReformEIRChiller( EIRChillNum ).CWLoopNum ).FluidIndex, RoutineName );

				RefCapFT = CurveValue( ElecReformEIRChiller( EIRChillNum ).ChillerCapFT, SizingEvapOutletTemp, SizingCondOutletTemp );
				tmpNomCap = ( Cp * rho * PlantSizData( PltSizNum ).DeltaT * tmpEvapVolFlowRate ) / RefCapFT;
				if ( ! ElecReformEIRChiller( EIRChillNum ).RefCapWasAutoSized ) tmpNomCap = ElecReformEIRChiller( EIRChillNum ).RefCap;

			} else {
				if ( ElecReformEIRChiller( EIRChillNum ).RefCapWasAutoSized ) tmpNomCap = 0.0;

			}
			if ( PlantFirstSizesOkayToFinalize ) {
				if ( ElecReformEIRChiller( EIRChillNum ).RefCapWasAutoSized ) {
					ElecReformEIRChiller( EIRChillNum ).RefCap = tmpNomCap;
					if ( PlantFinalSizesOkayToReport ) {
						ReportSizingOutput( "Chiller:Electric:ReformulatedEIR", ElecReformEIRChiller( EIRChillNum ).Name,
						"Design Size Reference Capacity [W]", tmpNomCap );
					}
					if ( PlantFirstSizesOkayToReport ) {
						ReportSizingOutput( "Chiller:Electric:ReformulatedEIR", ElecReformEIRChiller( EIRChillNum ).Name,
						"Initial Design Size Reference Capacity [W]", tmpNomCap );
					}
				} else {
					if ( ElecReformEIRChiller( EIRChillNum ).RefCap > 0.0 && tmpNomCap > 0.0 ) {
						RefCapUser = ElecReformEIRChiller( EIRChillNum ).RefCap;
						if ( PlantFinalSizesOkayToReport ) {
							ReportSizingOutput( "Chiller:Electric:ReformulatedEIR", ElecReformEIRChiller( EIRChillNum ).Name, "Design Size Reference Capacity [W]", tmpNomCap, "User-Specified Reference Capacity [W]", RefCapUser );
							if ( DisplayExtraWarnings ) {
								if ( ( std::abs( tmpNomCap - RefCapUser ) / RefCapUser ) > AutoVsHardSizingThreshold ) {
									ShowMessage( "Size:ChillerElectricReformulatedEIR: Potential issue with equipment sizing for " + ElecReformEIRChiller( EIRChillNum ).Name );
									ShowContinueError( "User-Specified Reference Capacity of " + RoundSigDigits( RefCapUser, 2 ) + " [W]" );
									ShowContinueError( "differs from Design Size Reference Capacity of " + RoundSigDigits( tmpNomCap, 2 ) + " [W]" );
									ShowContinueError( "This may, or may not, indicate mismatched component sizes." );
									ShowContinueError( "Verify that the value entered is intended and is consistent with other components." );
								}
							}
						}
						tmpNomCap = RefCapUser;
					}
				}
			}
		} else {
			if ( ElecReformEIRChiller( EIRChillNum ).RefCapWasAutoSized && PlantFirstSizesOkayToFinalize ) {
				ShowSevereError( "Autosizing of Reformulated Electric Chiller reference capacity requires a loop Sizing:Plant object" );
				ShowContinueError( "Occurs in Reformulated Electric Chiller object=" + ElecReformEIRChiller( EIRChillNum ).Name );
				ErrorsFound = true;
			}
			if ( ! ElecReformEIRChiller( EIRChillNum ).RefCapWasAutoSized && PlantFinalSizesOkayToReport
					&& ( ElecReformEIRChiller( EIRChillNum ).RefCap > 0.0 ) ) {
						ReportSizingOutput( "Chiller:Electric:ReformulatedEIR", ElecReformEIRChiller( EIRChillNum ).Name,
							"User-Specified Reference Capacity [W]", ElecReformEIRChiller( EIRChillNum ).RefCap );
			}
		}

		if ( PltSizCondNum > 0 && PltSizNum > 0 ) {
			if ( PlantSizData( PltSizNum ).DesVolFlowRate >= SmallWaterVolFlow && tmpNomCap > 0.0 ) {
				rho = GetDensityGlycol( PlantLoop( ElecReformEIRChiller( EIRChillNum ).CDLoopNum ).FluidName, InitConvTemp, PlantLoop( ElecReformEIRChiller( EIRChillNum ).CDLoopNum ).FluidIndex, RoutineName );

				Cp = GetSpecificHeatGlycol( PlantLoop( ElecReformEIRChiller( EIRChillNum ).CDLoopNum ).FluidName, ElecReformEIRChiller( EIRChillNum ).TempRefCondIn, PlantLoop( ElecReformEIRChiller( EIRChillNum ).CDLoopNum ).FluidIndex, RoutineName );
				tmpCondVolFlowRate = tmpNomCap * ( 1.0 + ( 1.0 / ElecReformEIRChiller( EIRChillNum ).RefCOP ) * ElecReformEIRChiller( EIRChillNum ).CompPowerToCondenserFrac ) / ( PlantSizData( PltSizCondNum ).DeltaT * Cp * rho );
				if ( ! ElecReformEIRChiller( EIRChillNum ).CondVolFlowRateWasAutoSized ) tmpCondVolFlowRate = ElecReformEIRChiller( EIRChillNum ).CondVolFlowRate;
				//IF (PlantFirstSizesOkayToFinalize) ElecReformEIRChiller(EIRChillNum)%CondVolFlowRate = tmpCondVolFlowRate
			} else {
				if ( ElecReformEIRChiller( EIRChillNum ).CondVolFlowRateWasAutoSized ) tmpCondVolFlowRate = 0.0;
				//IF (PlantFirstSizesOkayToFinalize) ElecReformEIRChiller(EIRChillNum)%CondVolFlowRate = tmpCondVolFlowRate
			}
			if ( PlantFirstSizesOkayToFinalize ) {
				if ( ElecReformEIRChiller( EIRChillNum ).CondVolFlowRateWasAutoSized ) {
					ElecReformEIRChiller( EIRChillNum ).CondVolFlowRate = tmpCondVolFlowRate;
					if ( PlantFinalSizesOkayToReport ) {
						ReportSizingOutput( "Chiller:Electric:ReformulatedEIR", ElecReformEIRChiller( EIRChillNum ).Name,
							"Design Size Reference Condenser Water Flow Rate [m3/s]", tmpCondVolFlowRate );
					}
					if ( PlantFirstSizesOkayToReport ) {
						ReportSizingOutput( "Chiller:Electric:ReformulatedEIR", ElecReformEIRChiller( EIRChillNum ).Name,
							"Initial Design Size Reference Condenser Water Flow Rate [m3/s]", tmpCondVolFlowRate );
					}
				} else {
					if ( ElecReformEIRChiller( EIRChillNum ).CondVolFlowRate > 0.0 && tmpCondVolFlowRate > 0.0 ) {
						CondVolFlowRateUser = ElecReformEIRChiller( EIRChillNum ).CondVolFlowRate;
						if ( PlantFinalSizesOkayToReport ) {
							ReportSizingOutput( "Chiller:Electric:ReformulatedEIR", ElecReformEIRChiller( EIRChillNum ).Name,
								"Design Size Reference Condenser Water Flow Rate [m3/s]", tmpCondVolFlowRate,
								"User-Specified Reference Condenser Water Flow Rate [m3/s]", CondVolFlowRateUser );
							if ( DisplayExtraWarnings ) {
								if ( ( std::abs( tmpCondVolFlowRate - CondVolFlowRateUser ) / CondVolFlowRateUser ) > AutoVsHardSizingThreshold ) {
									ShowMessage( "Size:ChillerElectricReformulatedEIR: Potential issue with equipment sizing for " + ElecReformEIRChiller( EIRChillNum ).Name );
									ShowContinueError( "User-Specified Reference Condenser Water Flow Rate of " + RoundSigDigits( CondVolFlowRateUser, 5 ) + " [m3/s]" );
									ShowContinueError( "differs from Design Size Reference Condenser Water Flow Rate of " + RoundSigDigits( tmpCondVolFlowRate, 5 ) + " [m3/s]" );
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
			if ( ElecReformEIRChiller( EIRChillNum ).CondVolFlowRateWasAutoSized && PlantFirstSizesOkayToFinalize ) {
				ShowSevereError( "Autosizing of Reformulated Electric EIR Chiller condenser flow rate requires a condenser" );
				ShowContinueError( "loop Sizing:Plant object" );
				ShowContinueError( "Occurs in Reformulated Electric EIR Chiller object=" + ElecReformEIRChiller( EIRChillNum ).Name );
				ErrorsFound = true;
			}
			if ( ! ElecReformEIRChiller( EIRChillNum ).CondVolFlowRateWasAutoSized && PlantFinalSizesOkayToReport
					&&  ( ElecReformEIRChiller( EIRChillNum ).CondVolFlowRate > 0.0 ) ) {
						ReportSizingOutput( "Chiller:Electric:ReformulatedEIR", ElecReformEIRChiller( EIRChillNum ).Name,
							"User-Specified Reference Condenser Water Flow Rate [m3/s]", ElecReformEIRChiller( EIRChillNum ).CondVolFlowRate );
			}
		}

		// save the reference condenser water volumetric flow rate for use by the condenser water loop sizing algorithms
		RegisterPlantCompDesignFlow( ElecReformEIRChiller( EIRChillNum ).CondInletNodeNum, tmpCondVolFlowRate );

		if ( ElecReformEIRChiller( EIRChillNum ).HeatRecActive ) {
			tmpHeatRecVolFlowRate = tmpCondVolFlowRate * ElecReformEIRChiller( EIRChillNum ).HeatRecCapacityFraction;
			if ( ! ElecReformEIRChiller( EIRChillNum ).DesignHeatRecVolFlowRateWasAutoSized ) tmpHeatRecVolFlowRate = ElecReformEIRChiller( EIRChillNum ).DesignHeatRecVolFlowRate;
			if ( PlantFirstSizesOkayToFinalize ) {
				if ( ElecReformEIRChiller( EIRChillNum ).DesignHeatRecVolFlowRateWasAutoSized ) {
					ElecReformEIRChiller( EIRChillNum ).DesignHeatRecVolFlowRate = tmpHeatRecVolFlowRate;
					if ( PlantFinalSizesOkayToReport ) {
						ReportSizingOutput( "Chiller:Electric:ReformulatedEIR", ElecReformEIRChiller( EIRChillNum ).Name,
							"Design Size Design Heat Recovery Fluid Flow Rate [m3/s]", tmpHeatRecVolFlowRate );
					}
					if ( PlantFirstSizesOkayToReport ) {
						ReportSizingOutput( "Chiller:Electric:ReformulatedEIR", ElecReformEIRChiller( EIRChillNum ).Name,
							"Initial Design Size Design Heat Recovery Fluid Flow Rate [m3/s]", tmpHeatRecVolFlowRate );
					}
				} else {
					if ( ElecReformEIRChiller( EIRChillNum ).DesignHeatRecVolFlowRate > 0.0 && tmpHeatRecVolFlowRate > 0.0 ) {
						DesignHeatRecVolFlowRateUser = ElecReformEIRChiller( EIRChillNum ).DesignHeatRecVolFlowRate;
						if ( PlantFinalSizesOkayToReport ) {
							ReportSizingOutput( "Chiller:Electric:ReformulatedEIR", ElecReformEIRChiller( EIRChillNum ).Name, "Design Size Design Heat Recovery Fluid Flow Rate [m3/s]", tmpHeatRecVolFlowRate, "User-Specified Design Heat Recovery Fluid Flow Rate [m3/s]", DesignHeatRecVolFlowRateUser );
							if ( DisplayExtraWarnings ) {
								if ( ( std::abs( tmpHeatRecVolFlowRate - DesignHeatRecVolFlowRateUser ) / DesignHeatRecVolFlowRateUser ) > AutoVsHardSizingThreshold ) {
									ShowMessage( "Size:ChillerElectricReformulatedEIR: Potential issue with equipment sizing for " + ElecReformEIRChiller( EIRChillNum ).Name );
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
			RegisterPlantCompDesignFlow( ElecReformEIRChiller( EIRChillNum ).HeatRecInletNodeNum, tmpHeatRecVolFlowRate );
		}

		if ( PlantFinalSizesOkayToReport ) {
			if ( MyFlag( EIRChillNum ) ) {
				CalcChillerIPLV( ElecReformEIRChiller( EIRChillNum ).Name, TypeOf_Chiller_ElectricReformEIR, ElecReformEIRChiller( EIRChillNum ).RefCap, ElecReformEIRChiller( EIRChillNum ).RefCOP, ElecReformEIRChiller( EIRChillNum ).CondenserType, ElecReformEIRChiller( EIRChillNum ).ChillerCapFT, ElecReformEIRChiller( EIRChillNum ).ChillerEIRFT, ElecReformEIRChiller( EIRChillNum ).ChillerEIRFPLR, ElecReformEIRChiller( EIRChillNum ).MinUnloadRat, ElecReformEIRChiller( EIRChillNum ).EvapVolFlowRate, ElecReformEIRChiller( EIRChillNum ).CDLoopNum, ElecReformEIRChiller( EIRChillNum ).CompPowerToCondenserFrac );
				MyFlag( EIRChillNum ) = false;
			}
			//create predefined report
			equipName = ElecReformEIRChiller( EIRChillNum ).Name;
			PreDefTableEntry( pdchMechType, equipName, "Chiller:Electric:ReformulatedEIR" );
			PreDefTableEntry( pdchMechNomEff, equipName, ElecReformEIRChiller( EIRChillNum ).RefCOP );
			PreDefTableEntry( pdchMechNomCap, equipName, ElecReformEIRChiller( EIRChillNum ).RefCap );
		}

			// Only check performance curves if Capacity and volumetric flow rate are greater than 0
			if ( ElecReformEIRChiller( EIRChillNum ).RefCap > 0.0 && ElecReformEIRChiller( EIRChillNum ).CondVolFlowRate > 0.0 ) {
				//   Check the CAP-FT, EIR-FT, and PLR curves at reference conditions and warn user if different from 1.0 by more than +-10%
				if ( ElecReformEIRChiller( EIRChillNum ).ChillerCapFT > 0 ) {
					CurveVal = CurveValue( ElecReformEIRChiller( EIRChillNum ).ChillerCapFT, ElecReformEIRChiller( EIRChillNum ).TempRefEvapOut, ElecReformEIRChiller( EIRChillNum ).TempRefCondOut );
					if ( CurveVal > 1.10 || CurveVal < 0.90 ) {
						ShowWarningError( "Capacity ratio as a function of temperature curve output is not equal to 1.0" );
						ShowContinueError( "(+ or - 10%) at reference conditions for Chiller:Electric:ReformulatedEIR = " + equipName );
						ShowContinueError( "Curve output at reference conditions = " + TrimSigDigits( CurveVal, 3 ) );
					}
					GetCurveMinMaxValues( ElecReformEIRChiller( EIRChillNum ).ChillerCapFT, ElecReformEIRChiller( EIRChillNum ).ChillerCAPFTXTempMin, ElecReformEIRChiller( EIRChillNum ).ChillerCAPFTXTempMax, ElecReformEIRChiller( EIRChillNum ).ChillerCAPFTYTempMin, ElecReformEIRChiller( EIRChillNum ).ChillerCAPFTYTempMax );
				}

				if ( ElecReformEIRChiller( EIRChillNum ).ChillerEIRFT > 0 ) {
					CurveVal = CurveValue( ElecReformEIRChiller( EIRChillNum ).ChillerEIRFT, ElecReformEIRChiller( EIRChillNum ).TempRefEvapOut, ElecReformEIRChiller( EIRChillNum ).TempRefCondOut );
					if ( CurveVal > 1.10 || CurveVal < 0.90 ) {
						ShowWarningError( "Energy input ratio as a function of temperature curve output is not equal to 1.0" );
						ShowContinueError( "(+ or - 10%) at reference conditions for Chiller:Electric:ReformulatedEIR = " + equipName );
						ShowContinueError( "Curve output at reference conditions = " + TrimSigDigits( CurveVal, 3 ) );
					}
					GetCurveMinMaxValues( ElecReformEIRChiller( EIRChillNum ).ChillerEIRFT, ElecReformEIRChiller( EIRChillNum ).ChillerEIRFTXTempMin, ElecReformEIRChiller( EIRChillNum ).ChillerEIRFTXTempMax, ElecReformEIRChiller( EIRChillNum ).ChillerEIRFTYTempMin, ElecReformEIRChiller( EIRChillNum ).ChillerEIRFTYTempMax );
				}

			if ( ElecReformEIRChiller( EIRChillNum ).ChillerEIRFPLR > 0 ) {
				if ( ElecReformEIRChiller( EIRChillNum ).PartLoadCurveType == PLR_LeavingCondenserWaterTemperature ) {
					CurveVal = CurveValue( ElecReformEIRChiller( EIRChillNum ).ChillerEIRFPLR, ElecReformEIRChiller( EIRChillNum ).TempRefCondOut, 1.0 );
				} else if ( ElecReformEIRChiller( EIRChillNum ).PartLoadCurveType == PLR_Lift ) {
					CurveVal = CurveValue( ElecReformEIRChiller( EIRChillNum ).ChillerEIRFPLR, 1.0, 1.0, 0.0 ); // zrp_Aug2014
				}
				if ( CurveVal > 1.10 || CurveVal < 0.90 ) {
					ShowWarningError( "Energy input ratio as a function of part-load ratio curve output is not equal to 1.0" );
					ShowContinueError( "(+ or - 10%) at reference conditions for Chiller:Electric:ReformulatedEIR = " + equipName );
					ShowContinueError( "Curve output at reference conditions = " + TrimSigDigits( CurveVal, 3 ) );
				}

				if ( ElecReformEIRChiller( EIRChillNum ).PartLoadCurveType == PLR_LeavingCondenserWaterTemperature ) {
					GetCurveMinMaxValues( ElecReformEIRChiller( EIRChillNum ).ChillerEIRFPLR, ElecReformEIRChiller( EIRChillNum ).ChillerEIRFPLRTempMin, ElecReformEIRChiller( EIRChillNum ).ChillerEIRFPLRTempMax, ElecReformEIRChiller( EIRChillNum ).ChillerEIRFPLRPLRMin, ElecReformEIRChiller( EIRChillNum ).ChillerEIRFPLRPLRMax );
				} else if ( ElecReformEIRChiller( EIRChillNum ).PartLoadCurveType == PLR_Lift ) { // zrp_Aug2014
					GetCurveMinMaxValues( ElecReformEIRChiller( EIRChillNum ).ChillerEIRFPLR, ElecReformEIRChiller( EIRChillNum ).ChillerLiftNomMin, ElecReformEIRChiller( EIRChillNum ).ChillerLiftNomMax, ElecReformEIRChiller( EIRChillNum ).ChillerEIRFPLRPLRMin, ElecReformEIRChiller( EIRChillNum ).ChillerEIRFPLRPLRMax, ElecReformEIRChiller( EIRChillNum ).ChillerTdevNomMin, ElecReformEIRChiller( EIRChillNum ).ChillerTdevNomMax );
				}

				if ( ElecReformEIRChiller( EIRChillNum ).ChillerEIRFPLRPLRMin < 0 || ElecReformEIRChiller( EIRChillNum ).ChillerEIRFPLRPLRMin >= ElecReformEIRChiller( EIRChillNum ).ChillerEIRFPLRPLRMax || ElecReformEIRChiller( EIRChillNum ).ChillerEIRFPLRPLRMin > 1 ) {
					ShowSevereError( "Invalid minimum value of PLR = " + TrimSigDigits( ElecReformEIRChiller( EIRChillNum ).ChillerEIRFPLRPLRMin, 3 ) + " in bicubic curve = " + ElecReformEIRChiller( EIRChillNum ).EIRFPLRName + " which is used" );
					ShowContinueError( "by Chiller:Electric:ReformulatedEIR = " + equipName + '.' );
					ShowContinueError( "The minimum value of PLR [y] must be from zero to 1, and less than the maximum value of PLR." );
					ErrorsFound = true;
				}
				if ( ElecReformEIRChiller( EIRChillNum ).ChillerEIRFPLRPLRMax > 1.1 || ElecReformEIRChiller( EIRChillNum ).ChillerEIRFPLRPLRMax <= ElecReformEIRChiller( EIRChillNum ).ChillerEIRFPLRPLRMin || ElecReformEIRChiller( EIRChillNum ).ChillerEIRFPLRPLRMax < 0 ) {
					ShowSevereError( "Invalid maximum value of PLR = " + TrimSigDigits( ElecReformEIRChiller( EIRChillNum ).ChillerEIRFPLRPLRMax, 3 ) + " in bicubic curve = " + ElecReformEIRChiller( EIRChillNum ).EIRFPLRName + " which is used" );
					ShowContinueError( "by Chiller:Electric:ReformulatedEIR = " + equipName + '.' );
					ShowContinueError( "The maximum value of PLR [y] must be from zero to 1.1, and greater than the minimum value of PLR." );
					ErrorsFound = true;
				}
				//     calculate the condenser outlet temp proportional to PLR and test the EIRFPLR curve output for negative numbers.
			}

			//  Initialize condenser reference inlet temperature (not a user input)
			Density = GetDensityGlycol( PlantLoop( ElecReformEIRChiller( EIRChillNum ).CDLoopNum ).FluidName, ElecReformEIRChiller( EIRChillNum ).TempRefCondOut, PlantLoop( ElecReformEIRChiller( EIRChillNum ).CDLoopNum ).FluidIndex, RoutineName );

			SpecificHeat = GetSpecificHeatGlycol( PlantLoop( ElecReformEIRChiller( EIRChillNum ).CDLoopNum ).FluidName, ElecReformEIRChiller( EIRChillNum ).TempRefCondOut, PlantLoop( ElecReformEIRChiller( EIRChillNum ).CDLoopNum ).FluidIndex, RoutineName );
			CondenserCapacity = ElecReformEIRChiller( EIRChillNum ).RefCap * ( 1.0 + ( 1.0 / ElecReformEIRChiller( EIRChillNum ).RefCOP ) * ElecReformEIRChiller( EIRChillNum ).CompPowerToCondenserFrac );
			DeltaTCond = ( CondenserCapacity ) / ( ElecReformEIRChiller( EIRChillNum ).CondVolFlowRate * Density * SpecificHeat );
			ElecReformEIRChiller( EIRChillNum ).TempRefCondIn = ElecReformEIRChiller( EIRChillNum ).TempRefCondOut - DeltaTCond;

			if ( ElecReformEIRChiller( EIRChillNum ).PartLoadCurveType == PLR_LeavingCondenserWaterTemperature ) {
			//     Check EIRFPLR curve output. Calculate condenser inlet temp based on reference condenser outlet temp,
			//     chiller capacity, and mass flow rate. Starting with the calculated condenser inlet temp and PLR = 0,
			//     calculate the condenser outlet temp proportional to PLR and test the EIRFPLR curve output for negative numbers.
				FoundNegValue = false;
				if ( ElecReformEIRChiller( EIRChillNum ).ChillerEIRFPLR > 0 ) {
					CurveValArray = 0.0;
					CondTempArray = 0.0;
					for ( CurveCheck = 0; CurveCheck <= 10; ++CurveCheck ) {
						PLRTemp = CurveCheck / 10.0;
						CondTemp = ElecReformEIRChiller( EIRChillNum ).TempRefCondIn + ( DeltaTCond * PLRTemp );
						CondTemp = min( CondTemp, ElecReformEIRChiller( EIRChillNum ).ChillerEIRFPLRTempMax );
						CondTemp = max( CondTemp, ElecReformEIRChiller( EIRChillNum ).ChillerEIRFPLRTempMin );
						if ( PLRTemp < ElecReformEIRChiller( EIRChillNum ).ChillerEIRFPLRPLRMin ) {
							CurveValTmp = CurveValue( ElecReformEIRChiller( EIRChillNum ).ChillerEIRFPLR, CondTemp, ElecReformEIRChiller( EIRChillNum ).ChillerEIRFPLRPLRMin );
						} else {
							CurveValTmp = CurveValue( ElecReformEIRChiller( EIRChillNum ).ChillerEIRFPLR, CondTemp, PLRTemp );
						}
						if ( CurveValTmp < 0.0 ) FoundNegValue = true;
						CurveValArray( CurveCheck + 1 ) = int( CurveValTmp * 100.0 ) / 100.0;
						CondTempArray( CurveCheck + 1 ) = int( CondTemp * 100.0 ) / 100.0;
					}

					}

					//     Output warning message if negative values are found in the EIRFPLR curve output. Results in Fatal error.
					if ( FoundNegValue ) {
						ShowWarningError( "Energy input to cooing output ratio function of part-load ratio curve shows negative values " );
						ShowContinueError( "for  Chiller:Electric:ReformulatedEIR = " + equipName + '.' );
						ShowContinueError( "EIR as a function of PLR curve output at various part-load ratios and condenser water temperatures shown below:" );
						ShowContinueError( "PLR           =    0.00   0.10   0.20   0.30   0.40   0.50   0.60   0.70   0.80   0.90   1.00" );
						gio::write( StringVar, "'Cond Temp(C) = '" );
						for ( CurveValPtr = 1; CurveValPtr <= 11; ++CurveValPtr ) {
							gio::write( StringVar, "(F7.2,$)" ) << CondTempArray( CurveValPtr );
						}
						gio::write( StringVar );
						ShowContinueError( StringVar );
						gio::write( StringVar, "'Curve Output = '" );
						for ( CurveValPtr = 1; CurveValPtr <= 11; ++CurveValPtr ) {
							gio::write( StringVar, "(F7.2,$)" ) << CurveValArray( CurveValPtr );
						}
						gio::write( StringVar );
						ShowContinueError( StringVar );
						ErrorsFound = true;
					}
			}
				} else { // just get curve min/max values if capacity or cond volume flow rate = 0
					GetCurveMinMaxValues( ElecReformEIRChiller( EIRChillNum ).ChillerCapFT, ElecReformEIRChiller( EIRChillNum ).ChillerCAPFTXTempMin, ElecReformEIRChiller( EIRChillNum ).ChillerCAPFTXTempMax, ElecReformEIRChiller( EIRChillNum ).ChillerCAPFTYTempMin, ElecReformEIRChiller( EIRChillNum ).ChillerCAPFTYTempMax );
					GetCurveMinMaxValues( ElecReformEIRChiller( EIRChillNum ).ChillerEIRFT, ElecReformEIRChiller( EIRChillNum ).ChillerEIRFTXTempMin, ElecReformEIRChiller( EIRChillNum ).ChillerEIRFTXTempMax, ElecReformEIRChiller( EIRChillNum ).ChillerEIRFTYTempMin, ElecReformEIRChiller( EIRChillNum ).ChillerEIRFTYTempMax );
					if ( ElecReformEIRChiller( EIRChillNum ).PartLoadCurveType == PLR_LeavingCondenserWaterTemperature ) {
						GetCurveMinMaxValues( ElecReformEIRChiller( EIRChillNum ).ChillerEIRFPLR, ElecReformEIRChiller( EIRChillNum ).ChillerEIRFPLRTempMin, ElecReformEIRChiller( EIRChillNum ).ChillerEIRFPLRTempMax, ElecReformEIRChiller( EIRChillNum ).ChillerEIRFPLRPLRMin, ElecReformEIRChiller( EIRChillNum ).ChillerEIRFPLRPLRMax );
					} else if ( ElecReformEIRChiller( EIRChillNum ).PartLoadCurveType == PLR_Lift ) { // zrp_Aug2014
						GetCurveMinMaxValues( ElecReformEIRChiller( EIRChillNum ).ChillerEIRFPLR, ElecReformEIRChiller( EIRChillNum ).ChillerLiftNomMin, ElecReformEIRChiller( EIRChillNum ).ChillerLiftNomMax, ElecReformEIRChiller( EIRChillNum ).ChillerEIRFPLRPLRMin, ElecReformEIRChiller( EIRChillNum ).ChillerEIRFPLRPLRMax, ElecReformEIRChiller( EIRChillNum ).ChillerTdevNomMin, ElecReformEIRChiller( EIRChillNum ).ChillerTdevNomMax );
					}
				}

		if ( ErrorsFound ) {
			ShowFatalError( "Preceding sizing errors cause program termination" );
		}

	}

	void
	ControlReformEIRChillerModel(
		int & EIRChillNum, // Chiller number
		Real64 & MyLoad, // Operating load [W]
		bool const RunFlag, // TRUE when chiller operating
		bool const FirstIteration, // TRUE when first iteration of timestep
		int const EquipFlowCtrl // Flow control mode for the equipment
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Lixing Gu, FSEC
		//       DATE WRITTEN   July 2006
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Simulate a vapor compression chiller using the reformulated model developed by Mark Hydeman

		// METHODOLOGY EMPLOYED:
		// Use empirical curve fits to model performance at off-design conditions. This subroutine
		// calls Subroutines CalcReformEIRChillerModel and SolveRegulaFalsi to obtain solution.
		// The actual chiller performance calculations are in Subroutine CalcReformEIRChillerModel.

		// REFERENCES:
		// 1. Hydeman, M., P. Sreedharan, N. Webb, and S. Blanc. 2002. "Development and Testing of a Reformulated
		//    Regression-Based Electric Chiller Model". ASHRAE Transactions, HI-02-18-2, Vol 108, Part 2, pp. 1118-1127.

		// USE STATEMENTS:

		// Using/Aliasing
		using DataGlobals::WarmupFlag;
		using CurveManager::GetCurveMinMaxValues;
		using General::SolveRegulaFalsi;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:

		Real64 const Acc( 0.0001 ); // Accuracy control for SolveRegulaFalsi
		int const MaxIter( 500 ); // Iteration control for SolveRegulaFalsi

		// INTERFACE BLOCK SPECIFICATIONS:
		//  na

		// DERIVED TYPE DEFINITIONS:
		//  na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		Real64 CAPFTYTmin; // Minimum condenser leaving temperature allowed by CAPFT curve [C]
		Real64 CAPFTYTmax; // Maximum condenser leaving temperature allowed by CAPFT curve [C]
		Real64 EIRFTYTmin; // Minimum condenser leaving temperature allowed by EIRFT curve [C]
		Real64 EIRFTYTmax; // Maximum condenser leaving temperature allowed by EIRFT curve [C]
		Real64 EIRFPLRTmin; // Minimum condenser leaving temperature allowed by EIRFPLR curve [C]
		Real64 EIRFPLRTmax; // Maximum condenser leaving temperature allowed by EIRFPLR curve [C]
		Real64 Tmin( -99 ); // Minimum condenser leaving temperature allowed by curve objects [C]
		Real64 Tmax( -99 ); // Maximum condenser leaving temperature allowed by curve objects [C]
		Array1D< Real64 > Par( 6 ); // Pass parameters for RegulaFalsi solver
		Real64 FalsiCondOutTemp; // RegulaFalsi condenser outlet temperature result [C]
		int SolFla; // Feedback flag from SolveRegulaFalsi
		Real64 CondTempMin; // Condenser outlet temperature when using Tmin as input to CalcReformEIRChillerModel [C]
		Real64 CondTempMax; // Condenser outlet temperature when using Tmax as input to CalcReformEIRChillerModel [C]

		if ( MyLoad >= 0.0 || ! RunFlag ) {
			CalcReformEIRChillerModel( EIRChillNum, MyLoad, RunFlag, FirstIteration, EquipFlowCtrl, Node( ElecReformEIRChiller( EIRChillNum ).CondInletNodeNum ).Temp );
		} else {

			//  Find min/max condenser outlet temperature used by curve objects
			CAPFTYTmin = ElecReformEIRChiller( EIRChillNum ).ChillerCAPFTYTempMin;
			EIRFTYTmin = ElecReformEIRChiller( EIRChillNum ).ChillerEIRFTYTempMin;
			if ( ElecReformEIRChiller( EIRChillNum ).PartLoadCurveType == PLR_LeavingCondenserWaterTemperature ) {
				EIRFPLRTmin = ElecReformEIRChiller( EIRChillNum ).ChillerEIRFPLRTempMin;
				Tmin = min( CAPFTYTmin, EIRFTYTmin, EIRFPLRTmin );
			} else if ( ElecReformEIRChiller( EIRChillNum ).PartLoadCurveType == PLR_Lift ) { // zrp_Aug2014
				Tmin = min( CAPFTYTmin, EIRFTYTmin );
			}


			CAPFTYTmax = ElecReformEIRChiller( EIRChillNum ).ChillerCAPFTYTempMax;
			EIRFTYTmax = ElecReformEIRChiller( EIRChillNum ).ChillerEIRFTYTempMax;
			if ( ElecReformEIRChiller( EIRChillNum ).PartLoadCurveType == PLR_LeavingCondenserWaterTemperature ) {
				EIRFPLRTmax = ElecReformEIRChiller( EIRChillNum ).ChillerEIRFPLRTempMax;
				Tmax = max( CAPFTYTmax, EIRFTYTmax, EIRFPLRTmax );
			} else if ( ElecReformEIRChiller( EIRChillNum ).PartLoadCurveType == PLR_Lift ) { // zrp_Aug2014
				Tmax = max( CAPFTYTmax, EIRFTYTmax );
			}

			//  Check that condenser outlet temperature is within curve object limits prior to calling RegulaFalsi
			CalcReformEIRChillerModel( EIRChillNum, MyLoad, RunFlag, FirstIteration, EquipFlowCtrl, Tmin );
			CondTempMin = CondOutletTemp;
			CalcReformEIRChillerModel( EIRChillNum, MyLoad, RunFlag, FirstIteration, EquipFlowCtrl, Tmax );
			CondTempMax = CondOutletTemp;

			if ( CondTempMin > Tmin && CondTempMax < Tmax ) {

				//    Initialize iteration parameters for RegulaFalsi function
				Par( 1 ) = EIRChillNum;
				Par( 2 ) = MyLoad;
				if ( RunFlag ) {
					Par( 3 ) = 1.0;
				} else {
					Par( 3 ) = 0.0;
				}
				if ( FirstIteration ) {
					Par( 4 ) = 1.0;
				} else {
					Par( 4 ) = 0.0;
				}
				//Par(5) = FlowLock !DSU
				Par( 6 ) = EquipFlowCtrl;

				SolveRegulaFalsi( Acc, MaxIter, SolFla, FalsiCondOutTemp, CondOutTempResidual, Tmin, Tmax, Par );

				if ( SolFla == -1 ) {
					if ( ! WarmupFlag ) {
						++ElecReformEIRChiller( EIRChillNum ).IterLimitExceededNum;
						if ( ElecReformEIRChiller( EIRChillNum ).IterLimitExceededNum == 1 ) {
							ShowWarningError( ElecReformEIRChiller( EIRChillNum ).Name + ": Iteration limit exceeded calculating condenser outlet temperature and non-converged temperature is used" );
						} else {
							ShowRecurringWarningErrorAtEnd( ElecReformEIRChiller( EIRChillNum ).Name + ": Iteration limit exceeded calculating condenser outlet temperature.", ElecReformEIRChiller( EIRChillNum ).IterLimitErrIndex, CondOutletTemp, CondOutletTemp );
						}
					}
				} else if ( SolFla == -2 ) {
					if ( ! WarmupFlag ) {
						++ElecReformEIRChiller( EIRChillNum ).IterFailed;
						if ( ElecReformEIRChiller( EIRChillNum ).IterFailed == 1 ) {
							ShowWarningError( ElecReformEIRChiller( EIRChillNum ).Name + ": Solution found when calculating condenser outlet temperature. The inlet temperature will used and the simulation continues..." );
							ShowContinueError( "Please check minimum and maximum values of x in EIRFPLR Curve " + ElecReformEIRChiller( EIRChillNum ).EIRFPLRName );
						} else {
							ShowRecurringWarningErrorAtEnd( ElecReformEIRChiller( EIRChillNum ).Name + ": Solution is not found in calculating condenser outlet temperature.", ElecReformEIRChiller( EIRChillNum ).IterFailedIndex, CondOutletTemp, CondOutletTemp );
						}
					}
					CalcReformEIRChillerModel( EIRChillNum, MyLoad, RunFlag, FirstIteration, EquipFlowCtrl, Node( ElecReformEIRChiller( EIRChillNum ).CondInletNodeNum ).Temp );
				}
			} else {
				//    If iteration is not possible, average the min/max condenser outlet temperature and manually determine solution
				CalcReformEIRChillerModel( EIRChillNum, MyLoad, RunFlag, FirstIteration, EquipFlowCtrl, ( CondTempMin + CondTempMax ) / 2.0 );
				CalcReformEIRChillerModel( EIRChillNum, MyLoad, RunFlag, FirstIteration, EquipFlowCtrl, CondOutletTemp );
			}

			//  Call subroutine to evaluate all performance curve min/max values against evaporator/condenser outlet temps and PLR
			CheckMinMaxCurveBoundaries( EIRChillNum, FirstIteration );

		}

	}

	void
	ReformEIRChillerHeatRecovery(
		int const EIRChillNum, // Number of the current electric EIR chiller being simulated
		Real64 & QCond, // Current condenser load [W]
		Real64 const CondMassFlow, // Current condenser mass flow [kg/s]
		Real64 const CondInletTemp, // Current condenser inlet temp [C]
		Real64 & QHeatRec // Amount of heat recovered [W]
	)
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR:          Lixing Gu, FSEC
		//       DATE WRITTEN:    July 2006
		//       MODIFIED:        na

		// PURPOSE OF THIS SUBROUTINE:
		//  Calculate the heat recovered from the chiller condenser

		// METHODOLOGY EMPLOYED:
		//  na

		// REFERENCES:
		//  na

		// Using/Aliasing
		using DataPlant::SingleSetPoint;
		using DataPlant::DualSetPointDeadBand;
		using DataPlant::PlantLoop;
		using ScheduleManager::GetCurrentScheduleValue;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "EIRChillerHeatRecovery" );

		// DERIVED TYPE DEFINITIONS:
		//  na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int CondInletNode; // Condenser inlet node number
		int CondOutletNode; // Condenser outlet node number
		int HeatRecInNode; // Node number for heat recovery water inlet node
		int HeatRecOutNode; // Node number for heat recovery water outlet node
		Real64 QTotal; // Total condenser heat [W]
		//  REAL(r64)    :: QCondTmp            ! Total condenser heat based on average temperatures [W]
		Real64 HeatRecInletTemp; // Heat reclaim inlet temp [C]
		Real64 HeatRecMassFlowRate; // Heat reclaim mass flow rate [m3/s]
		Real64 TAvgIn; // Average inlet temperature of heat reclaim inlet and condenser inlet [C]
		Real64 TAvgOut; // Average outlet temperature [C]
		Real64 CpHeatRec; // Heat reclaim water inlet specific heat [J/kg-K]
		Real64 CpCond; // Condenser water inlet specific heat [J/kg-K]
		Real64 QHeatRecToSetPoint;
		Real64 THeatRecSetPoint( 0.0 );
		Real64 HeatRecHighInletLimit;

		// Begin routine
		HeatRecInNode = ElecReformEIRChiller( EIRChillNum ).HeatRecInletNodeNum;
		HeatRecOutNode = ElecReformEIRChiller( EIRChillNum ).HeatRecOutletNodeNum;
		CondInletNode = ElecReformEIRChiller( EIRChillNum ).CondInletNodeNum;
		CondOutletNode = ElecReformEIRChiller( EIRChillNum ).CondOutletNodeNum;

		// inlet node to the heat recovery heat exchanger
		HeatRecInletTemp = Node( HeatRecInNode ).Temp;
		HeatRecMassFlowRate = Node( HeatRecInNode ).MassFlowRate;

		CpHeatRec = GetSpecificHeatGlycol( PlantLoop( ElecReformEIRChiller( EIRChillNum ).HRLoopNum ).FluidName, HeatRecInletTemp, PlantLoop( ElecReformEIRChiller( EIRChillNum ).HRLoopNum ).FluidIndex, RoutineName );
		CpCond = GetSpecificHeatGlycol( PlantLoop( ElecReformEIRChiller( EIRChillNum ).CDLoopNum ).FluidName, CondInletTemp, PlantLoop( ElecReformEIRChiller( EIRChillNum ).CDLoopNum ).FluidIndex, RoutineName );

		// Before we modify the QCondenser, the total or original value is transferred to QTot
		QTotal = QCond;

		if ( ElecReformEIRChiller( EIRChillNum ).HeatRecSetPointNodeNum == 0 ) { // use original algorithm that blends temps
			TAvgIn = ( HeatRecMassFlowRate * CpHeatRec * HeatRecInletTemp + CondMassFlow * CpCond * CondInletTemp ) / ( HeatRecMassFlowRate * CpHeatRec + CondMassFlow * CpCond );

			TAvgOut = QTotal / ( HeatRecMassFlowRate * CpHeatRec + CondMassFlow * CpCond ) + TAvgIn;

			QHeatRec = HeatRecMassFlowRate * CpHeatRec * ( TAvgOut - HeatRecInletTemp );
			QHeatRec = max( QHeatRec, 0.0 ); // ensure non negative
			//check if heat flow too large for physical size of bundle
			QHeatRec = min( QHeatRec, ElecReformEIRChiller( EIRChillNum ).HeatRecMaxCapacityLimit );
		} else { // use new algorithm to meet setpoint
			{ auto const SELECT_CASE_var( PlantLoop( ElecReformEIRChiller( EIRChillNum ).HRLoopNum ).LoopDemandCalcScheme );

			if ( SELECT_CASE_var == SingleSetPoint ) {
				THeatRecSetPoint = Node( ElecReformEIRChiller( EIRChillNum ).HeatRecSetPointNodeNum ).TempSetPoint;
			} else if ( SELECT_CASE_var == DualSetPointDeadBand ) {
				THeatRecSetPoint = Node( ElecReformEIRChiller( EIRChillNum ).HeatRecSetPointNodeNum ).TempSetPointHi;
			} else {
				assert( false );
			}}

			QHeatRecToSetPoint = HeatRecMassFlowRate * CpHeatRec * ( THeatRecSetPoint - HeatRecInletTemp );
			QHeatRecToSetPoint = max( QHeatRecToSetPoint, 0.0 );
			QHeatRec = min( QTotal, QHeatRecToSetPoint );
			//check if heat flow too large for physical size of bundle
			QHeatRec = min( QHeatRec, ElecReformEIRChiller( EIRChillNum ).HeatRecMaxCapacityLimit );
		}

		// check if limit on inlet is present and exceeded.
		if ( ElecReformEIRChiller( EIRChillNum ).HeatRecInletLimitSchedNum > 0 ) {
			HeatRecHighInletLimit = GetCurrentScheduleValue( ElecReformEIRChiller( EIRChillNum ).HeatRecInletLimitSchedNum );
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
	UpdateReformEIRChillerRecords(
		Real64 const MyLoad, // Current load [W]
		bool const RunFlag, // TRUE if chiller operating
		int const Num // Chiller number
	)
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR:          Lixing Gu, FSEC
		//       DATE WRITTEN:    July 2006

		// PURPOSE OF THIS SUBROUTINE:
		//  Reporting

		// METHODOLOGY EMPLOYED:
		//  na

		// REFERENCES:
		//  na

		// USE STATEMENTS:

		// Using/Aliasing
		using DataGlobals::SecInHour;
		using DataHVACGlobals::TimeStepSys;
		using PlantUtilities::SafeCopyPlantNode;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		//  na

		// DERIVED TYPE DEFINITIONS
		//  na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int EvapInletNode; // Evaporator inlet node number
		int EvapOutletNode; // Evaporator outlet node number
		int CondInletNode; // Condenser inlet node number
		int CondOutletNode; // Condenser outlet node number
		int HeatRecInNode; // Node number for the heat recovery water inlet node
		int HeatRecOutNode; // Node number for the heat recovery water outlet node

		EvapInletNode = ElecReformEIRChiller( Num ).EvapInletNodeNum;
		EvapOutletNode = ElecReformEIRChiller( Num ).EvapOutletNodeNum;
		CondInletNode = ElecReformEIRChiller( Num ).CondInletNodeNum;
		CondOutletNode = ElecReformEIRChiller( Num ).CondOutletNodeNum;
		HeatRecInNode = ElecReformEIRChiller( Num ).HeatRecInletNodeNum;
		HeatRecOutNode = ElecReformEIRChiller( Num ).HeatRecOutletNodeNum;

		if ( MyLoad >= 0.0 || ! RunFlag ) { // Chiller not running so pass inlet states to outlet states
			// Set node temperatures
			Node( EvapOutletNode ).Temp = Node( EvapInletNode ).Temp;
			Node( CondOutletNode ).Temp = Node( CondInletNode ).Temp;

			ElecReformEIRChillerReport( Num ).ChillerPartLoadRatio = 0.0;
			ElecReformEIRChillerReport( Num ).ChillerCyclingRatio = 0.0;
			ElecReformEIRChillerReport( Num ).ChillerFalseLoadRate = 0.0;
			ElecReformEIRChillerReport( Num ).ChillerFalseLoad = 0.0;
			ElecReformEIRChillerReport( Num ).Power = 0.0;
			ElecReformEIRChillerReport( Num ).QEvap = 0.0;
			ElecReformEIRChillerReport( Num ).QCond = 0.0;
			ElecReformEIRChillerReport( Num ).Energy = 0.0;
			ElecReformEIRChillerReport( Num ).EvapEnergy = 0.0;
			ElecReformEIRChillerReport( Num ).CondEnergy = 0.0;
			ElecReformEIRChillerReport( Num ).EvapInletTemp = Node( EvapInletNode ).Temp;
			ElecReformEIRChillerReport( Num ).CondInletTemp = Node( CondInletNode ).Temp;
			ElecReformEIRChillerReport( Num ).CondOutletTemp = Node( CondOutletNode ).Temp;
			ElecReformEIRChillerReport( Num ).EvapOutletTemp = Node( EvapOutletNode ).Temp;
			ElecReformEIRChillerReport( Num ).Evapmdot = EvapMassFlowRate; // could still be flow if in series
			ElecReformEIRChillerReport( Num ).Condmdot = CondMassFlowRate; // could still be flow if in series
			ElecReformEIRChillerReport( Num ).ActualCOP = 0.0;

			if ( ElecReformEIRChiller( Num ).HeatRecActive ) {

				SafeCopyPlantNode( HeatRecInNode, HeatRecOutNode );
				ElecReformEIRChillerReport( Num ).QHeatRecovery = 0.0;
				ElecReformEIRChillerReport( Num ).EnergyHeatRecovery = 0.0;
				ElecReformEIRChillerReport( Num ).HeatRecInletTemp = Node( HeatRecInNode ).Temp;
				ElecReformEIRChillerReport( Num ).HeatRecOutletTemp = Node( HeatRecOutNode ).Temp;
				ElecReformEIRChillerReport( Num ).HeatRecMassFlow = Node( HeatRecInNode ).MassFlowRate;

				ElecReformEIRChillerReport( Num ).ChillerCondAvgTemp = AvgCondSinkTemp;
			}

		} else { // Chiller is running, so pass calculated values
			// Set node temperatures
			Node( EvapOutletNode ).Temp = EvapOutletTemp;
			Node( CondOutletNode ).Temp = CondOutletTemp;
			// Set node flow rates;  for these load based models
			// assume that sufficient evaporator flow rate is available
			ElecReformEIRChillerReport( Num ).ChillerPartLoadRatio = ChillerPartLoadRatio;
			ElecReformEIRChillerReport( Num ).ChillerCyclingRatio = ChillerCyclingRatio;
			ElecReformEIRChillerReport( Num ).ChillerFalseLoadRate = ChillerFalseLoadRate;
			ElecReformEIRChillerReport( Num ).ChillerFalseLoad = ChillerFalseLoadRate * TimeStepSys * SecInHour;
			ElecReformEIRChillerReport( Num ).Power = Power;
			ElecReformEIRChillerReport( Num ).QEvap = QEvaporator;
			ElecReformEIRChillerReport( Num ).QCond = QCondenser;
			ElecReformEIRChillerReport( Num ).Energy = Power * TimeStepSys * SecInHour;
			ElecReformEIRChillerReport( Num ).EvapEnergy = QEvaporator * TimeStepSys * SecInHour;
			ElecReformEIRChillerReport( Num ).CondEnergy = QCondenser * TimeStepSys * SecInHour;
			ElecReformEIRChillerReport( Num ).EvapInletTemp = Node( EvapInletNode ).Temp;
			ElecReformEIRChillerReport( Num ).CondInletTemp = Node( CondInletNode ).Temp;
			ElecReformEIRChillerReport( Num ).CondOutletTemp = Node( CondOutletNode ).Temp;
			ElecReformEIRChillerReport( Num ).EvapOutletTemp = Node( EvapOutletNode ).Temp;
			ElecReformEIRChillerReport( Num ).Evapmdot = EvapMassFlowRate;
			ElecReformEIRChillerReport( Num ).Condmdot = CondMassFlowRate;
			if ( Power != 0.0 ) {
				ElecReformEIRChillerReport( Num ).ActualCOP = ( QEvaporator + ChillerFalseLoadRate ) / Power;
			} else {
				ElecReformEIRChillerReport( Num ).ActualCOP = 0.0;
			}

			if ( ElecReformEIRChiller( Num ).HeatRecActive ) {

				SafeCopyPlantNode( HeatRecInNode, HeatRecOutNode );
				ElecReformEIRChillerReport( Num ).QHeatRecovery = QHeatRecovered;
				ElecReformEIRChillerReport( Num ).EnergyHeatRecovery = QHeatRecovered * TimeStepSys * SecInHour;
				Node( HeatRecOutNode ).Temp = HeatRecOutletTemp;
				ElecReformEIRChillerReport( Num ).HeatRecInletTemp = Node( HeatRecInNode ).Temp;
				ElecReformEIRChillerReport( Num ).HeatRecOutletTemp = Node( HeatRecOutNode ).Temp;
				ElecReformEIRChillerReport( Num ).HeatRecMassFlow = Node( HeatRecInNode ).MassFlowRate;

				ElecReformEIRChillerReport( Num ).ChillerCondAvgTemp = AvgCondSinkTemp;
			}

		}

		ElecReformEIRChillerReport( Num ).ChillerCapFT = ChillerCapFT;
		ElecReformEIRChillerReport( Num ).ChillerEIRFT = ChillerEIRFT;
		ElecReformEIRChillerReport( Num ).ChillerEIRFPLR = ChillerEIRFPLR;

	}

	Real64
	CondOutTempResidual(
		Real64 const FalsiCondOutTemp, // RegulaFalsi condenser outlet temperature result [C]
		Array1< Real64 > const & Par // Parameter array used to interface with RegulaFalsi solver
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Richard Raustad
		//       DATE WRITTEN   May 2006
		//       MODIFIED       L.Gu, May 2006
		//       RE-ENGINEERED

		// PURPOSE OF THIS FUNCTION:
		//  Calculates residual function (desired condenser outlet temperature)
		//  Reformulated EIR chiller requires condenser outlet temperature to calculate capacity and power.

		// METHODOLOGY EMPLOYED:
		//  Regula Falsi solver is used to calculate condenser outlet temperature.

		// REFERENCES:
		//  na

		// USE STATEMENTS:
		//  na

		// Return value
		Real64 CondOutTempResidual;

		// Argument array dimensioning

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		//  na

		// INTERFACE BLOCK SPECIFICATIONS:
		//  na

		// DERIVED TYPE DEFINITIONS:
		//  na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		int EIRChillNum; // Chiller number
		Real64 MyLoad; // Operating load [W]
		bool FirstIteration; // TRUE when first iteration of timestep
		bool RunFlag; // TRUE when chiller operating
		int EquipFlowCtrl; // Flow control mode for the equipment

		// FalsiCondOutTemp = Value used by RegulaFalsi during iteration (used to evaluate CAPFT, EIRFT, and EIRFPLR curves)
		// CondOutletTemp = Value calculated by CalcReformEIRChillerModel subroutine as shown below
		// CondOutletTemp = QCondenser/CondMassFlowRate/CPCW(CondInletTemp) + CondInletTemp

		EIRChillNum = int( Par( 1 ) );
		MyLoad = Par( 2 );
		RunFlag = ( int( Par( 3 ) ) == 1 );
		FirstIteration = ( int( Par( 4 ) ) == 1 );
		//FlowLock = INT(Par(5))   !DSU
		EquipFlowCtrl = int( Par( 6 ) );

		CalcReformEIRChillerModel( EIRChillNum, MyLoad, RunFlag, FirstIteration, EquipFlowCtrl, FalsiCondOutTemp );
		CondOutTempResidual = FalsiCondOutTemp - CondOutletTemp; // CondOutletTemp is module level variable, final value used for reporting

		return CondOutTempResidual;

	}

	void
	CalcReformEIRChillerModel(
		int const EIRChillNum, // Chiller number
		Real64 & MyLoad, // Operating load [W]
		bool const RunFlag, // TRUE when chiller operating
		bool const EP_UNUSED( FirstIteration ), // TRUE when first iteration of timestep !unused1208
		int const EquipFlowCtrl, // Flow control mode for the equipment
		Real64 const FalsiCondOutTemp // RegulaFalsi condenser outlet temperature result [C]
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Lixing Gu, FSEC
		//       DATE WRITTEN   July 2006
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		//       MODIFIED
		//			Aug.  2014, Rongpeng Zhang, added an additional part-load performance curve type

		// PURPOSE OF THIS SUBROUTINE:
		//  Simulate a vapor compression chiller using the reformulated model developed by Mark Hydeman

		// METHODOLOGY EMPLOYED:
		//  Use empirical curve fits to model performance at off-design conditions

		// REFERENCES:
		// 1. Hydeman, M., P. Sreedharan, N. Webb, and S. Blanc. 2002. "Development and Testing of a Reformulated
		//    Regression-Based Electric Chiller Model". ASHRAE Transactions, HI-02-18-2, Vol 108, Part 2, pp. 1118-1127.

		// Using/Aliasing
		using DataGlobals::WarmupFlag;
		using DataHVACGlobals::SmallLoad;
		using DataHVACGlobals::TimeStepSys;
		using General::RoundSigDigits;
		using General::CreateSysTimeIntervalString;
		using CurveManager::CurveValue;
		using DataPlant::DeltaTempTol;
		using DataPlant::PlantLoop;
		using DataPlant::SimPlantEquipTypes;
		using DataPlant::TypeOf_Chiller_ElectricReformEIR;
		using DataPlant::CompSetPtBasedSchemeType;
		using DataPlant::CriteriaType_MassFlowRate;
		using DataPlant::SingleSetPoint;
		using DataPlant::DualSetPointDeadBand;
		using DataBranchAirLoopPlant::ControlType_SeriesActive;
		using DataBranchAirLoopPlant::MassFlowTolerance;
		using DataEnvironment::EnvironmentName;
		using DataEnvironment::CurMnDy;
		using PlantUtilities::SetComponentFlowRate;
		using PlantUtilities::PullCompInterconnectTrigger;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:

		static gio::Fmt OutputFormat( "(F6.2)" );
		static std::string const RoutineName( "CalcElecReformEIRChillerModel" );

		// INTERFACE BLOCK SPECIFICATIONS
		//  na

		// DERIVED TYPE DEFINITIONS
		//  na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 FRAC; // Chiller cycling ratio
		Real64 MinPartLoadRat; // Minimum allowed operating fraction of full load
		Real64 MinUnloadRat; // Minimum allowed unloading fraction of full load
		Real64 MaxPartLoadRat; // Maximum allowed operating fraction of full load
		Real64 EvapInletTemp; // Evaporator inlet temperature [C]
		Real64 CondInletTemp; // Condenser inlet temperature [C]
		Real64 EvapOutletTempSetPoint( 0.0 ); // Evaporator outlet temperature setpoint [C]
		Real64 AvailChillerCap; // Chiller available capacity [W]
		Real64 ChillerRefCap; // Chiller reference capacity [W]
		Real64 EvapDeltaTemp( 0.0 ); // Evaporator temperature difference [C]
		Real64 ReferenceCOP; // Reference coefficient of performance, from user input
		Real64 PartLoadRat; // Operating part load ratio
		Real64 TempLowLimitEout; // Evaporator low temp. limit cut off [C]
		Real64 EvapMassFlowRateMax; // Maximum evaporator mass flow rate converted from volume flow rate [kg/s]

		Real64 ChillerLift;		//Chiller lift
		Real64 ChillerLiftRef; 	//Chiller lift under the reference condition
		Real64 ChillerLiftNom; 	//Normalized chiller lift
		Real64 ChillerTdev; 	//Deviation of leaving chilled water temperature from the reference condition
		Real64 ChillerTdevNom; 	//Normalized ChillerTdev
		int PartLoadCurveType; 	//Part Load Ratio Curve Type: 1_LeavingCondenserWaterTemperature; 2_Lift

		int EvapInletNode; // evaporator inlet node number
		int EvapOutletNode; // evaporator outlet node number
		int CondInletNode; // condenser inlet node number
		int CondOutletNode; // condenser outlet node number
		//  LOGICAL, SAVE          :: PossibleSubcooling
		Real64 TempLoad; // actual load to be met by chiller. This value is compared to MyLoad
		// and reset when necessary since this chiller can cycle, the load passed
		// should be the actual load.  Instead the minimum PLR * RefCap is
		// passed in.
		int PlantLoopNum; // Plant loop which contains the current chiller
		int LoopSideNum; // Plant loop side which contains the current chiller (usually supply side)
		int BranchNum;
		int CompNum;
		Real64 Cp; // Local fluid specific heat

		//  REAL(r64),SAVE         :: TimeStepSysLast=0.0     ! last system time step (used to check for downshifting)
		//  REAL(r64)              :: CurrentEndTime          ! end time of time step for current simulation time step
		//  REAL(r64),SAVE         :: CurrentEndTimeLast=0.0  ! end time of time step for last simulation time step
		//  CHARACTER(len=6)       :: OutputChar = ' '        ! character string for warning messages

		// Set module level inlet and outlet nodes and initialize other local variables
		ChillerPartLoadRatio = 0.0;
		ChillerCyclingRatio = 0.0;
		ChillerFalseLoadRate = 0.0;
		EvapMassFlowRate = 0.0;
		CondMassFlowRate = 0.0;
		Power = 0.0;
		QCondenser = 0.0;
		QEvaporator = 0.0;
		QHeatRecovered = 0.0;
		//  CondenserFanPower          = 0.0
		EvapInletNode = ElecReformEIRChiller( EIRChillNum ).EvapInletNodeNum;
		EvapOutletNode = ElecReformEIRChiller( EIRChillNum ).EvapOutletNodeNum;
		CondInletNode = ElecReformEIRChiller( EIRChillNum ).CondInletNodeNum;
		CondOutletNode = ElecReformEIRChiller( EIRChillNum ).CondOutletNodeNum;
		PlantLoopNum = ElecReformEIRChiller( EIRChillNum ).CWLoopNum;
		LoopSideNum = ElecReformEIRChiller( EIRChillNum ).CWLoopSideNum;
		BranchNum = ElecReformEIRChiller( EIRChillNum ).CWBranchNum;
		CompNum = ElecReformEIRChiller( EIRChillNum ).CWCompNum;

		// Set performance curve outputs to 0.0 when chiller is off
		ChillerCapFT = 0.0;
		ChillerEIRFT = 0.0;
		ChillerEIRFPLR = 0.0;

		// Set module-level chiller evap and condenser inlet temperature variables
		EvapInletTemp = Node( EvapInletNode ).Temp;
		CondInletTemp = Node( CondInletNode ).Temp;

		// This chiller is currenlty has only a water-cooled condenser
		//! calculate end time of current time step
		//  CurrentEndTime = CurrentTime + SysTimeElapsed
		//! Print warning messages only when valid and only for the first ocurrance. Let summary provide statistics.
		//! Wait for next time step to print warnings. If simulation iterates, print out
		//! the warning for the last iteration only. Must wait for next time step to accomplish this.
		//! If a warning occurs and the simulation down shifts, the warning is not valid.
		//  IF(CurrentEndTime .GT. CurrentEndTimeLast .AND. TimeStepSys .GE. TimeStepSysLast)THEN
		//    IF(ElecReformEIRChiller(EIRChillNum)%PrintMessage)THEN
		//          ElecReformEIRChiller(EIRChillNum)%MsgErrorCount = &
		//                         ElecReformEIRChiller(EIRChillNum)%MsgErrorCount + 1
		//!     Show single warning and pass additional info to ShowRecurringWarningErrorAtEnd
		//      IF (ElecReformEIRChiller(EIRChillNum)%MsgErrorCount < 2) THEN
		//         CALL ShowWarningError(TRIM(ElecReformEIRChiller(EIRChillNum)%MsgBuffer1)//'.')
		//         CALL ShowContinueError(TRIM(ElecReformEIRChiller(EIRChillNum)%MsgBuffer2))
		//      ELSE
		//        CALL ShowRecurringWarningErrorAtEnd(TRIM(ElecReformEIRChiller(EIRChillNum)%MsgBuffer1)//' error continues.', &
		//           ElecReformEIRChiller(EIRChillNum)%ErrCount1,ReportMaxOf=ElecReformEIRChiller(EIRChillNum)%MsgDataLast,  &
		//           ReportMinOf=ElecReformEIRChiller(EIRChillNum)%MsgDataLast,ReportMaxUnits='[C]',ReportMinUnits='[C]')
		//      END IF
		//    END IF
		//  END IF
		//! save last system time step and last end time of current time step (used to determine if warning is valid)
		//  TimeStepSysLast    = TimeStepSys
		//  CurrentEndTimeLast = CurrentEndTime

		// If no loop demand or chiller OFF, return
		// If chiller load is 0 or chiller is not running then leave the subroutine. Before leaving
		//  if the component control is SERIESACTIVE we set the component flow to inlet flow so that
		//  flow resolver will not shut down the branch
		if ( MyLoad >= 0 || ! RunFlag ) {
			if ( EquipFlowCtrl == ControlType_SeriesActive || PlantLoop( PlantLoopNum ).LoopSide( LoopSideNum ).FlowLock == 1 ) {
				EvapMassFlowRate = Node( EvapInletNode ).MassFlowRate;
			}
			if ( ElecReformEIRChiller( EIRChillNum ).CondenserType == WaterCooled ) {
				if ( PlantLoop( ElecReformEIRChiller( EIRChillNum ).CDLoopNum ).LoopSide( ElecReformEIRChiller( EIRChillNum ).CDLoopSideNum ).Branch( ElecReformEIRChiller( EIRChillNum ).CDBranchNum ).Comp( ElecReformEIRChiller( EIRChillNum ).CDCompNum ).FlowCtrl == ControlType_SeriesActive ) {
					CondMassFlowRate = Node( CondInletNode ).MassFlowRate;
				}
			}

			return;
		}

		// LOAD LOCAL VARIABLES FROM DATA STRUCTURE (for code readability)
		MinPartLoadRat = ElecReformEIRChiller( EIRChillNum ).MinPartLoadRat;
		MaxPartLoadRat = ElecReformEIRChiller( EIRChillNum ).MaxPartLoadRat;
		MinUnloadRat = ElecReformEIRChiller( EIRChillNum ).MinUnloadRat;
		ChillerRefCap = ElecReformEIRChiller( EIRChillNum ).RefCap;
		ReferenceCOP = ElecReformEIRChiller( EIRChillNum ).RefCOP;
		EvapOutletTemp = Node( ElecReformEIRChiller( EIRChillNum ).EvapOutletNodeNum ).Temp;
		TempLowLimitEout = ElecReformEIRChiller( EIRChillNum ).TempLowLimitEvapOut;
		EvapMassFlowRateMax = ElecReformEIRChiller( EIRChillNum ).EvapMassFlowRateMax;
		PartLoadCurveType = ElecReformEIRChiller( EIRChillNum ).PartLoadCurveType; //zrp_Aug2014

		// Set mass flow rates

		if ( ElecReformEIRChiller( EIRChillNum ).CondenserType == WaterCooled ) {
			CondMassFlowRate = ElecReformEIRChiller( EIRChillNum ).CondMassFlowRateMax;
			SetComponentFlowRate( CondMassFlowRate, CondInletNode, CondOutletNode, ElecReformEIRChiller( EIRChillNum ).CDLoopNum, ElecReformEIRChiller( EIRChillNum ).CDLoopSideNum, ElecReformEIRChiller( EIRChillNum ).CDBranchNum, ElecReformEIRChiller( EIRChillNum ).CDCompNum );
			PullCompInterconnectTrigger( ElecReformEIRChiller( EIRChillNum ).CWLoopNum, ElecReformEIRChiller( EIRChillNum ).CWLoopSideNum, ElecReformEIRChiller( EIRChillNum ).CWBranchNum, ElecReformEIRChiller( EIRChillNum ).CWCompNum, ElecReformEIRChiller( EIRChillNum ).CondMassFlowIndex, ElecReformEIRChiller( EIRChillNum ).CDLoopNum, ElecReformEIRChiller( EIRChillNum ).CDLoopSideNum, CriteriaType_MassFlowRate, CondMassFlowRate );

			if ( CondMassFlowRate < MassFlowTolerance ) return;

		}
		FRAC = 1.0;

		{ auto const SELECT_CASE_var( PlantLoop( PlantLoopNum ).LoopDemandCalcScheme );
		if ( SELECT_CASE_var == SingleSetPoint ) {
			if ( ( ElecReformEIRChiller( EIRChillNum ).FlowMode == LeavingSetPointModulated ) || ( PlantLoop( PlantLoopNum ).LoopSide( LoopSideNum ).Branch( BranchNum ).Comp( CompNum ).CurOpSchemeType == CompSetPtBasedSchemeType ) || ( Node( ElecReformEIRChiller( EIRChillNum ).EvapOutletNodeNum ).TempSetPoint != SensedNodeFlagValue ) ) {
				// there will be a valid setpoint on outlet
				EvapOutletTempSetPoint = Node( EvapOutletNode ).TempSetPoint;
			} else { // use plant loop overall setpoint
				EvapOutletTempSetPoint = Node( PlantLoop( PlantLoopNum ).TempSetPointNodeNum ).TempSetPoint;
			}
		} else if ( SELECT_CASE_var == DualSetPointDeadBand ) {
			if ( ( ElecReformEIRChiller( EIRChillNum ).FlowMode == LeavingSetPointModulated ) || ( PlantLoop( PlantLoopNum ).LoopSide( LoopSideNum ).Branch( BranchNum ).Comp( CompNum ).CurOpSchemeType == CompSetPtBasedSchemeType ) || ( Node( ElecReformEIRChiller( EIRChillNum ).EvapOutletNodeNum ).TempSetPointHi != SensedNodeFlagValue ) ) {
				// there will be a valid setpoint on outlet
				EvapOutletTempSetPoint = Node( EvapOutletNode ).TempSetPointHi;
			} else { // use plant loop overall setpoint
				EvapOutletTempSetPoint = Node( PlantLoop( PlantLoopNum ).TempSetPointNodeNum ).TempSetPointHi;
			}
		} else {
			assert( false );
		}}

		// correct temperature if using heat recovery
		// use report values for latest valid calculation, lagged somewhat
		if ( ElecReformEIRChiller( EIRChillNum ).HeatRecActive ) {
			if ( ( ElecReformEIRChillerReport( EIRChillNum ).QHeatRecovery + ElecReformEIRChillerReport( EIRChillNum ).QCond ) > 0.0 ) { // protect div by zero
				AvgCondSinkTemp = ( ElecReformEIRChillerReport( EIRChillNum ).QHeatRecovery * ElecReformEIRChillerReport( EIRChillNum ).HeatRecOutletTemp + ElecReformEIRChillerReport( EIRChillNum ).QCond * ElecReformEIRChillerReport( EIRChillNum ).CondOutletTemp ) / ( ElecReformEIRChillerReport( EIRChillNum ).QHeatRecovery + ElecReformEIRChillerReport( EIRChillNum ).QCond );
			} else {
				AvgCondSinkTemp = FalsiCondOutTemp;
			}
		} else {
			AvgCondSinkTemp = FalsiCondOutTemp;
		}

		// Get capacity curve info with respect to CW setpoint and leaving condenser water temps
		ChillerCapFT = max( 0.0, CurveValue( ElecReformEIRChiller( EIRChillNum ).ChillerCapFT, EvapOutletTempSetPoint, AvgCondSinkTemp ) );

		// Available chiller capacity as a function of temperature
		AvailChillerCap = ChillerRefCap * ChillerCapFT;

		//  IF (PlantLoop(PlantLoopNum)%LoopSide(LoopSideNum)%FlowLock .EQ. 0) THEN
		//    EvapMassFlowRate = MIN(EvapMassFlowRateMax,Node(EvapInletNode)%MassFlowRateMaxAvail)    !CRBranchPump
		//    EvapMassFlowRate = MAX(EvapMassFlowRate,Node(EvapInletNode)%MassFlowRateMinAvail)       !CRBranchPump
		//!   Some other component set the flow to 0. No reason to continue with calculations.
		//    IF(EvapMassFlowRate == 0.0d0)THEN
		//      MyLoad = 0.0d0
		//!      ElecReformEIRChiller(EIRChillNum)%PrintMessage = .FALSE.
		//      RETURN
		//    END IF
		//  ELSE
		EvapMassFlowRate = Node( EvapInletNode ).MassFlowRate;
		//   Some other component set the flow to 0. No reason to continue with calculations.
		if ( EvapMassFlowRate == 0.0 ) {
			MyLoad = 0.0;
			//      ElecReformEIRChiller(EIRChillNum)%PrintMessage = .FALSE.
			return;
		}
		//  END IF

		// This chiller is currenlty has only a water-cooled condenser

		// Calculate water side load
		Cp = GetSpecificHeatGlycol( PlantLoop( ElecReformEIRChiller( EIRChillNum ).CWLoopNum ).FluidName, Node( EvapInletNode ).Temp, PlantLoop( ElecReformEIRChiller( EIRChillNum ).CWLoopNum ).FluidIndex, RoutineName );
		// problem here if no setpoint on outlet
		// CR 9132 changed from actual node flow rate to maximum available to avoid issue of limiting capacity

		TempLoad = Node( EvapInletNode ).MassFlowRateMaxAvail * Cp * ( Node( EvapInletNode ).Temp - EvapOutletTempSetPoint );

		TempLoad = max( 0.0, TempLoad );

		// MyLoad is capped at minimum PLR * RefCap, adjust load to actual water side load because this chiller can cycle
		if ( std::abs( MyLoad ) > TempLoad ) {
			MyLoad = sign( TempLoad, MyLoad );
		}

		// Part load ratio based on load and available chiller capacity, cap at max part load ratio
		if ( AvailChillerCap > 0 ) {
			PartLoadRat = max( 0.0, min( std::abs( MyLoad ) / AvailChillerCap, MaxPartLoadRat ) );
		} else {
			PartLoadRat = 0.0;
		}

		// Set evaporator heat transfer rate
		QEvaporator = AvailChillerCap * PartLoadRat;
		ChillerPartLoadRatio = PartLoadRat;
		// If FlowLock is False (0), the chiller sets the plant loop mdot
		// If FlowLock is True (1),  the new resolved plant loop mdot is used
		if ( PlantLoop( PlantLoopNum ).LoopSide( LoopSideNum ).FlowLock == 0 ) {
			if ( PlantLoop( PlantLoopNum ).LoopSide( LoopSideNum ).Branch( BranchNum ).Comp( CompNum ).CurOpSchemeType == CompSetPtBasedSchemeType ) {
				ElecReformEIRChiller( EIRChillNum ).PossibleSubcooling = false;
			} else {
				ElecReformEIRChiller( EIRChillNum ).PossibleSubcooling = true;
			}
			// Either set the flow to the Constant value or caluclate the flow for the variable volume case
			if ( ( ElecReformEIRChiller( EIRChillNum ).FlowMode == ConstantFlow ) || ( ElecReformEIRChiller( EIRChillNum ).FlowMode == NotModulated ) ) {
				// Set the evaporator mass flow rate to design
				// Start by assuming max (design) flow
				EvapMassFlowRate = EvapMassFlowRateMax;
				// Use SetComponentFlowRate to decide actual flow
				SetComponentFlowRate( EvapMassFlowRate, EvapInletNode, EvapOutletNode, ElecReformEIRChiller( EIRChillNum ).CWLoopNum, ElecReformEIRChiller( EIRChillNum ).CWLoopSideNum, ElecReformEIRChiller( EIRChillNum ).CWBranchNum, ElecReformEIRChiller( EIRChillNum ).CWCompNum );
				if ( EvapMassFlowRate != 0.0 ) {
					EvapDeltaTemp = QEvaporator / EvapMassFlowRate / Cp;
				} else {
					EvapDeltaTemp = 0.0;
				}
				EvapOutletTemp = Node( EvapInletNode ).Temp - EvapDeltaTemp;
			} else if ( ElecReformEIRChiller( EIRChillNum ).FlowMode == LeavingSetPointModulated ) {
				{ auto const SELECT_CASE_var( PlantLoop( PlantLoopNum ).LoopDemandCalcScheme );
				if ( SELECT_CASE_var == SingleSetPoint ) {
					// Calculate the Delta Temp from the inlet temp to the chiller outlet setpoint
					EvapDeltaTemp = Node( EvapInletNode ).Temp - Node( EvapOutletNode ).TempSetPoint;
				} else if ( SELECT_CASE_var == DualSetPointDeadBand ) {
					EvapDeltaTemp = Node( EvapInletNode ).Temp - Node( EvapOutletNode ).TempSetPointHi;
				} else {
					assert( false );
				}}

				if ( EvapDeltaTemp != 0 ) {
					EvapMassFlowRate = max( 0.0, ( QEvaporator / Cp / EvapDeltaTemp ) );
					if ( ( EvapMassFlowRate - EvapMassFlowRateMax ) > MassFlowTolerance ) ElecReformEIRChiller( EIRChillNum ).PossibleSubcooling = true;
					//Check to see if the Maximum is exceeded, if so set to maximum
					EvapMassFlowRate = min( EvapMassFlowRateMax, EvapMassFlowRate );
					// Use SetComponentFlowRate to decide actual flow
					SetComponentFlowRate( EvapMassFlowRate, EvapInletNode, EvapOutletNode, ElecReformEIRChiller( EIRChillNum ).CWLoopNum, ElecReformEIRChiller( EIRChillNum ).CWLoopSideNum, ElecReformEIRChiller( EIRChillNum ).CWBranchNum, ElecReformEIRChiller( EIRChillNum ).CWCompNum );
					// Should we recalculate this with the corrected setpoint?
					{ auto const SELECT_CASE_var( PlantLoop( PlantLoopNum ).LoopDemandCalcScheme );
					if ( SELECT_CASE_var == SingleSetPoint ) {
						EvapOutletTemp = Node( EvapOutletNode ).TempSetPoint;
					} else if ( SELECT_CASE_var == DualSetPointDeadBand ) {
						EvapOutletTemp = Node( EvapOutletNode ).TempSetPointHi;
					}}
					QEvaporator = max( 0.0, ( EvapMassFlowRate * Cp * EvapDeltaTemp ) );
				} else {
					// Try to request zero flow
					EvapMassFlowRate = 0.0;
					// Use SetComponentFlowRate to decide actual flow
					SetComponentFlowRate( EvapMassFlowRate, EvapInletNode, EvapOutletNode, ElecReformEIRChiller( EIRChillNum ).CWLoopNum, ElecReformEIRChiller( EIRChillNum ).CWLoopSideNum, ElecReformEIRChiller( EIRChillNum ).CWBranchNum, ElecReformEIRChiller( EIRChillNum ).CWCompNum );
					// No deltaT since component is not running
					EvapOutletTemp = Node( EvapInletNode ).Temp;
					QEvaporator = 0.0;
					PartLoadRat = 0.0;
					ChillerPartLoadRatio = PartLoadRat;

					if ( ElecReformEIRChiller( EIRChillNum ).DeltaTErrCount < 1 && ! WarmupFlag ) {
						++ElecReformEIRChiller( EIRChillNum ).DeltaTErrCount;
						ShowWarningError( "Evaporator DeltaTemp = 0 in mass flow calculation (Tevapin = Tevapout setpoint temp)." );
						ShowContinueErrorTimeStamp( "" );
					} else if ( ! WarmupFlag ) {
						++ElecReformEIRChiller( EIRChillNum ).ChillerCapFTError;
						ShowRecurringWarningErrorAtEnd( "CHILLER:ELECTRIC:REFORMULATEDEIR \"" + ElecReformEIRChiller( EIRChillNum ).Name + "\": Evaporator DeltaTemp = 0 in mass flow calculation warning continues...", ElecReformEIRChiller( EIRChillNum ).DeltaTErrCountIndex, EvapDeltaTemp, EvapDeltaTemp );
					}

				}
			} //End of Constant Variable Flow If Block

		} else { // If FlowLock is True
			EvapMassFlowRate = Node( EvapInletNode ).MassFlowRate;
			SetComponentFlowRate( EvapMassFlowRate, EvapInletNode, EvapOutletNode, ElecReformEIRChiller( EIRChillNum ).CWLoopNum, ElecReformEIRChiller( EIRChillNum ).CWLoopSideNum, ElecReformEIRChiller( EIRChillNum ).CWBranchNum, ElecReformEIRChiller( EIRChillNum ).CWCompNum );
			//       Some other component set the flow to 0. No reason to continue with calculations.
			if ( EvapMassFlowRate == 0.0 ) {
				MyLoad = 0.0;
				//        ElecReformEIRChiller(EIRChillNum)%PrintMessage = .FALSE.
				return;
			}
			if ( ElecReformEIRChiller( EIRChillNum ).PossibleSubcooling ) {
				QEvaporator = std::abs( MyLoad );
				EvapDeltaTemp = QEvaporator / EvapMassFlowRate / Cp;
				EvapOutletTemp = Node( EvapInletNode ).Temp - EvapDeltaTemp;
			} else {
				EvapDeltaTemp = Node( EvapInletNode ).Temp - EvapOutletTempSetPoint;
				QEvaporator = max( 0.0, ( EvapMassFlowRate * Cp * EvapDeltaTemp ) );
				EvapOutletTemp = EvapOutletTempSetPoint;
			}
			if ( EvapOutletTemp < TempLowLimitEout ) {
				if ( ( Node( EvapInletNode ).Temp - TempLowLimitEout ) > DeltaTempTol ) {
					EvapOutletTemp = TempLowLimitEout;
					EvapDeltaTemp = Node( EvapInletNode ).Temp - EvapOutletTemp;
					QEvaporator = EvapMassFlowRate * Cp * EvapDeltaTemp;
				} else {
					EvapOutletTemp = Node( EvapInletNode ).Temp;
					EvapDeltaTemp = Node( EvapInletNode ).Temp - EvapOutletTemp;
					QEvaporator = EvapMassFlowRate * Cp * EvapDeltaTemp;
				}
			}
			if ( EvapOutletTemp < Node( EvapOutletNode ).TempMin ) {
				if ( ( Node( EvapInletNode ).Temp - Node( EvapOutletNode ).TempMin ) > DeltaTempTol ) {
					EvapOutletTemp = Node( EvapOutletNode ).TempMin;
					EvapDeltaTemp = Node( EvapInletNode ).Temp - EvapOutletTemp;
					QEvaporator = EvapMassFlowRate * Cp * EvapDeltaTemp;
				} else {
					EvapOutletTemp = Node( EvapInletNode ).Temp;
					EvapDeltaTemp = Node( EvapInletNode ).Temp - EvapOutletTemp;
					QEvaporator = EvapMassFlowRate * Cp * EvapDeltaTemp;
				}
			}
			// If load exceeds the distributed load set to the distributed load
			if ( QEvaporator > std::abs( MyLoad ) ) {
				if ( EvapMassFlowRate > MassFlowTolerance ) {
					QEvaporator = std::abs( MyLoad );
					EvapDeltaTemp = QEvaporator / EvapMassFlowRate / Cp;
					EvapOutletTemp = Node( EvapInletNode ).Temp - EvapDeltaTemp;
				} else {
					QEvaporator = 0.0;
					EvapOutletTemp = Node( EvapInletNode ).Temp;
				}
			}

			// Checks QEvaporator on the basis of the machine limits.
			if ( QEvaporator > ( AvailChillerCap * MaxPartLoadRat ) ) {
				if ( EvapMassFlowRate > MassFlowTolerance ) {
					QEvaporator = AvailChillerCap * MaxPartLoadRat;
					EvapDeltaTemp = QEvaporator / EvapMassFlowRate / Cp;
					// evaporator outlet temperature is allowed to float upwards (recalculate AvailChillerCap? iterate?)
					EvapOutletTemp = Node( EvapInletNode ).Temp - EvapDeltaTemp;
				} else {
					QEvaporator = 0.0;
					EvapOutletTemp = Node( EvapInletNode ).Temp;
					//           ElecReformEIRChiller(EIRChillNum)%PrintMessage = .FALSE.
				}

			}

			if ( AvailChillerCap > 0.0 ) {
				PartLoadRat = max( 0.0, min( ( QEvaporator / AvailChillerCap ), MaxPartLoadRat ) );
			} else {
				PartLoadRat = 0.0;
			}

			// Chiller cycles below minimum part load ratio, FRAC = amount of time chiller is ON during this time step
			if ( PartLoadRat < MinPartLoadRat ) FRAC = min( 1.0, ( PartLoadRat / MinPartLoadRat ) );

			// set the module level variable used for reporting FRAC
			ChillerCyclingRatio = FRAC;

			// Chiller is false loading below PLR = minimum unloading ratio, find PLR used for energy calculation
			if ( AvailChillerCap > 0.0 ) {
				PartLoadRat = max( PartLoadRat, MinUnloadRat );
			} else {
				PartLoadRat = 0.0;
			}

			// set the module level variable used for reporting PLR
			ChillerPartLoadRatio = PartLoadRat;

			// calculate the load due to false loading on chiller over and above water side load
			ChillerFalseLoadRate = ( AvailChillerCap * PartLoadRat * FRAC ) - QEvaporator;
			if ( ChillerFalseLoadRate < SmallLoad ) {
				ChillerFalseLoadRate = 0.0;
			}

		} //This is the end of the FlowLock Block

		ChillerEIRFT = max( 0.0, CurveValue( ElecReformEIRChiller( EIRChillNum ).ChillerEIRFT, EvapOutletTemp, AvgCondSinkTemp ) );

		// Part Load Ratio Curve Type: 1_LeavingCondenserWaterTemperature; 2_Lift  zrp_Aug2014
		if ( PartLoadCurveType == PLR_LeavingCondenserWaterTemperature ) {
			ChillerEIRFPLR = max( 0.0, CurveValue( ElecReformEIRChiller( EIRChillNum ).ChillerEIRFPLR, AvgCondSinkTemp, PartLoadRat ) );
		} else if ( PartLoadCurveType == PLR_Lift) {
			ChillerLift = AvgCondSinkTemp - EvapOutletTemp;
			ChillerTdev = std::abs(EvapOutletTemp - ElecReformEIRChiller(EIRChillNum).TempRefEvapOut);
			ChillerLiftRef = ElecReformEIRChiller( EIRChillNum ).TempRefCondOut - ElecReformEIRChiller( EIRChillNum ).TempRefEvapOut;

			if ( ChillerLiftRef <= 0 )  ChillerLiftRef = 35 - 6.67;
			ChillerLiftNom = ChillerLift / ChillerLiftRef;
			ChillerTdevNom = ChillerTdev / ChillerLiftRef;

			ChillerEIRFPLR = max(0.0, CurveValue( ElecReformEIRChiller(EIRChillNum).ChillerEIRFPLR, ChillerLiftNom, PartLoadRat, ChillerTdevNom));
		}

		if ( ReferenceCOP <= 0 ) ReferenceCOP = 5.5;
		Power = ( AvailChillerCap / ReferenceCOP ) * ChillerEIRFPLR * ChillerEIRFT * FRAC;

		QCondenser = Power * ElecReformEIRChiller( EIRChillNum ).CompPowerToCondenserFrac + QEvaporator + ChillerFalseLoadRate;

		//  Currently only water cooled chillers are allowed for the reformulated EIR chiller model
		if ( CondMassFlowRate > MassFlowTolerance ) {
			// If Heat Recovery specified for this vapor compression chiller, then Qcondenser will be adjusted by this subroutine
			if ( ElecReformEIRChiller( EIRChillNum ).HeatRecActive ) ReformEIRChillerHeatRecovery( EIRChillNum, QCondenser, CondMassFlowRate, CondInletTemp, QHeatRecovered );
			Cp = GetSpecificHeatGlycol( PlantLoop( ElecReformEIRChiller( EIRChillNum ).CDLoopNum ).FluidName, CondInletTemp, PlantLoop( ElecReformEIRChiller( EIRChillNum ).CDLoopNum ).FluidIndex, RoutineName );
			CondOutletTemp = QCondenser / CondMassFlowRate / Cp + CondInletTemp;
		} else {
			ShowSevereError( "ControlReformEIRChillerModel: Condenser flow = 0, for ElecReformEIRChiller=" + ElecReformEIRChiller( EIRChillNum ).Name );
			ShowContinueErrorTimeStamp( "" );

		}

	}

	void
	CheckMinMaxCurveBoundaries(
		int const EIRChillNum, // Number of the current electric EIR chiller being simulated
		bool const FirstIteration // TRUE when first iteration of timestep
	)
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR:          R Raustad, FSEC
		//       DATE WRITTEN:    August 2006

		// PURPOSE OF THIS SUBROUTINE:
		//  To compare the evaporator/condenser outlet temperatures to curve object min/max values

		// METHODOLOGY EMPLOYED:
		//  na

		// REFERENCES:
		//  na

		// Using/Aliasing
		using General::TrimSigDigits;
		using General::RoundSigDigits;
		using DataGlobals::WarmupFlag;
		using CurveManager::CurveValue;
		using DataPlant::PlantLoop;
		using DataPlant::CompSetPtBasedSchemeType;
		using DataPlant::SingleSetPoint;
		using DataPlant::DualSetPointDeadBand;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int EvapOutletNode; // Chiller evaporator outlet node number
		Real64 EvapOutletTempSetPoint( 0.0 ); // Evaporator outlet temperature setpoint [C]
		Real64 CAPFTXTmin; // Minimum evaporator leaving temperature allowed by CAPFT curve [C]
		Real64 CAPFTXTmax; // Maximum evaporator leaving temperature allowed by CAPFT curve [C]
		Real64 CAPFTYTmin; // Minimum condenser  leaving temperature allowed by CAPFT curve [C]
		Real64 CAPFTYTmax; // Maximum condenser  leaving temperature allowed by CAPFT curve [C]
		Real64 EIRFTXTmin; // Minimum evaporator leaving temperature allowed by EIRFT curve [C]
		Real64 EIRFTXTmax; // Maximum evaporator leaving temperature allowed by EIRFT curve [C]
		Real64 EIRFTYTmin; // Minimum condenser  leaving temperature allowed by EIRFT curve [C]
		Real64 EIRFTYTmax; // Maximum condenser  leaving temperature allowed by EIRFT curve [C]
		Real64 EIRFPLRTmin; // Minimum condenser  leaving temperature allowed by EIRFPLR curve [C]
		Real64 EIRFPLRTmax; // Maximum condenser  leaving temperature allowed by EIRFPLR curve [C]
		Real64 EIRFPLRPLRmin; // Minimum PLR allowed by EIRFPLR curve
		Real64 EIRFPLRPLRmax; // Maximum PLR allowed by EIRFPLR curve
		Real64 ChillerLift;	  //Chiller lift  [C]
		Real64 ChillerLiftRef; //Chiller lift under the reference condition  [C]
		Real64 ChillerLiftNom; //Normalized chiller lift
		Real64 ChillerTdev;    //Deviation of leaving chilled water temperature from the reference condition
		Real64 ChillerTdevNom; //Normalized ChillerTdev
		int PlantLoopNum; // Plant loop which contains the current chiller
		int LoopSideNum; // Plant loop side which contains the current chiller (usually supply side)
		int BranchNum;
		int CompNum;

		// Do not print out warnings if chiller not operating or FirstIteration/WarmupFlag/FlowLock
		PlantLoopNum = ElecReformEIRChiller( EIRChillNum ).CWLoopNum;
		LoopSideNum = ElecReformEIRChiller( EIRChillNum ).CWLoopSideNum;
		BranchNum = ElecReformEIRChiller( EIRChillNum ).CWBranchNum;
		CompNum = ElecReformEIRChiller( EIRChillNum ).CWCompNum;

		if ( FirstIteration || WarmupFlag || PlantLoop( PlantLoopNum ).LoopSide( LoopSideNum ).FlowLock == 0 ) return;

		EvapOutletNode = ElecReformEIRChiller( EIRChillNum ).EvapOutletNodeNum;

		// Move CAPFT and EIRFT min/max values for evaporator outlet temperature to local variables
		CAPFTXTmin = ElecReformEIRChiller( EIRChillNum ).ChillerCAPFTXTempMin;
		CAPFTXTmax = ElecReformEIRChiller( EIRChillNum ).ChillerCAPFTXTempMax;

		EIRFTXTmin = ElecReformEIRChiller( EIRChillNum ).ChillerEIRFTXTempMin;
		EIRFTXTmax = ElecReformEIRChiller( EIRChillNum ).ChillerEIRFTXTempMax;

		// Check bounds for curves, lump min/max into same check since min/max values are reported in recurring warning messages
		if ( EvapOutletTemp < CAPFTXTmin || EvapOutletTemp > CAPFTXTmax ) {
			++ElecReformEIRChiller( EIRChillNum ).CAPFTXIter;
			if ( ElecReformEIRChiller( EIRChillNum ).CAPFTXIter == 1 ) {
				ShowWarningError( "CHILLER:ELECTRIC:REFORMULATEDEIR \"" + ElecReformEIRChiller( EIRChillNum ).Name + "\": The evaporator outlet temperature (" + TrimSigDigits( EvapOutletTemp, 2 ) + " C) is outside the range of evaporator outlet temperatures (X var) given in Cooling Capacity Function of Temperature biquadratic curve = " + ElecReformEIRChiller( EIRChillNum ).CAPFTName );
				ShowContinueErrorTimeStamp( "The range specified = " + TrimSigDigits( CAPFTXTmin, 2 ) + " C to " + TrimSigDigits( CAPFTXTmax, 2 ) + " C." );
				ShowRecurringWarningErrorAtEnd( "CHILLER:ELECTRIC:REFORMULATEDEIR \"" + ElecReformEIRChiller( EIRChillNum ).Name + "\": The evap outlet temp range in Cooling Capacity Function of Temp curve error continues.", ElecReformEIRChiller( EIRChillNum ).CAPFTXIterIndex, EvapOutletTemp, EvapOutletTemp );
			} else {
				ShowRecurringWarningErrorAtEnd( "CHILLER:ELECTRIC:REFORMULATEDEIR \"" + ElecReformEIRChiller( EIRChillNum ).Name + "\": The evap outlet temp range in Cooling Capacity Function of Temp curve error continues.", ElecReformEIRChiller( EIRChillNum ).CAPFTXIterIndex, EvapOutletTemp, EvapOutletTemp );
			}
		}

		if ( EvapOutletTemp < EIRFTXTmin || EvapOutletTemp > EIRFTXTmax ) {
			++ElecReformEIRChiller( EIRChillNum ).EIRFTXIter;
			if ( ElecReformEIRChiller( EIRChillNum ).EIRFTXIter == 1 ) {
				ShowWarningError( "CHILLER:ELECTRIC:REFORMULATEDEIR \"" + ElecReformEIRChiller( EIRChillNum ).Name + "\": The evaporator outlet temperature (" + TrimSigDigits( EvapOutletTemp, 2 ) + " C) is outside the range of evaporator outlet temperatures (X var) given in Electric Input to Cooling Output Ratio Function of Temperature biquadratic curve = " + ElecReformEIRChiller( EIRChillNum ).EIRFTName );
				ShowContinueErrorTimeStamp( "The range specified = " + TrimSigDigits( EIRFTXTmin, 2 ) + " C to " + TrimSigDigits( EIRFTXTmax, 2 ) + " C." );
				ShowRecurringWarningErrorAtEnd( "CHILLER:ELECTRIC:REFORMULATEDEIR \"" + ElecReformEIRChiller( EIRChillNum ).Name + "\": The evap outlet temp range in Electric Input to Cooling Output Ratio Function of Temp curve error continues.", ElecReformEIRChiller( EIRChillNum ).EIRFTXIterIndex, EvapOutletTemp, EvapOutletTemp );
			} else {
				ShowRecurringWarningErrorAtEnd( "CHILLER:ELECTRIC:REFORMULATEDEIR \"" + ElecReformEIRChiller( EIRChillNum ).Name + "\": The evap outlet temp range in Electric Input to Cooling Output Ratio Function of Temp curve error continues.", ElecReformEIRChiller( EIRChillNum ).EIRFTXIterIndex, EvapOutletTemp, EvapOutletTemp );
			}
		}

		// Move CAPFT, EIRFT, and EIRFPLR min/max condenser outlet temperature values to local variables
		CAPFTYTmin = ElecReformEIRChiller( EIRChillNum ).ChillerCAPFTYTempMin;
		CAPFTYTmax = ElecReformEIRChiller( EIRChillNum ).ChillerCAPFTYTempMax;

		EIRFTYTmin = ElecReformEIRChiller( EIRChillNum ).ChillerEIRFTYTempMin;
		EIRFTYTmax = ElecReformEIRChiller( EIRChillNum ).ChillerEIRFTYTempMax;

		if ( ElecReformEIRChiller( EIRChillNum ).PartLoadCurveType == PLR_LeavingCondenserWaterTemperature ) {
			EIRFPLRTmin = ElecReformEIRChiller( EIRChillNum ).ChillerEIRFPLRTempMin;
			EIRFPLRTmax = ElecReformEIRChiller( EIRChillNum ).ChillerEIRFPLRTempMax;
		}

		// Move EIRFPLR min/max part-load ratio values to local variables
		EIRFPLRPLRmin = ElecReformEIRChiller( EIRChillNum ).ChillerEIRFPLRPLRMin;
		EIRFPLRPLRmax = ElecReformEIRChiller( EIRChillNum ).ChillerEIRFPLRPLRMax;

		// Check bounds for curves, lump min/max into same check since min/max values are reported in recurring warning messages
		if ( CondOutletTemp < CAPFTYTmin || CondOutletTemp > CAPFTYTmax ) {
			++ElecReformEIRChiller( EIRChillNum ).CAPFTYIter;
			if ( ElecReformEIRChiller( EIRChillNum ).CAPFTYIter == 1 ) {
				ShowWarningError( "CHILLER:ELECTRIC:REFORMULATEDEIR \"" + ElecReformEIRChiller( EIRChillNum ).Name + "\": The condenser outlet temperature (" + TrimSigDigits( CondOutletTemp, 2 ) + " C) is outside the range of condenser outlet temperatures (Y var) given in Cooling Capacity Function of Temperature biquadratic curve = " + ElecReformEIRChiller( EIRChillNum ).CAPFTName );
				ShowContinueErrorTimeStamp( "The range specified = " + TrimSigDigits( CAPFTYTmin, 2 ) + " C to " + TrimSigDigits( CAPFTYTmax, 2 ) + " C." );
				ShowRecurringWarningErrorAtEnd( "CHILLER:ELECTRIC:REFORMULATEDEIR \"" + ElecReformEIRChiller( EIRChillNum ).Name + "\": The cond outlet temp range in Cooling Capacity Function of Temp curve error continues.", ElecReformEIRChiller( EIRChillNum ).CAPFTYIterIndex, CondOutletTemp, CondOutletTemp );
			} else {
				ShowRecurringWarningErrorAtEnd( "CHILLER:ELECTRIC:REFORMULATEDEIR \"" + ElecReformEIRChiller( EIRChillNum ).Name + "\": The cond outlet temp range in Cooling Capacity Function of Temp curve error continues.", ElecReformEIRChiller( EIRChillNum ).CAPFTYIterIndex, CondOutletTemp, CondOutletTemp );
			}
		}

		if ( CondOutletTemp < EIRFTYTmin || CondOutletTemp > EIRFTYTmax ) {
			++ElecReformEIRChiller( EIRChillNum ).EIRFTYIter;
			if ( ElecReformEIRChiller( EIRChillNum ).EIRFTYIter == 1 ) {
				ShowWarningError( "CHILLER:ELECTRIC:REFORMULATEDEIR \"" + ElecReformEIRChiller( EIRChillNum ).Name + "\": The condenser outlet temperature (" + TrimSigDigits( CondOutletTemp, 2 ) + " C) is outside the range of condenser outlet temperatures (Y var) given in Electric Input to Cooling Output Ratio Function of Temperature biquadratic curve = " + ElecReformEIRChiller( EIRChillNum ).EIRFTName );
				ShowContinueErrorTimeStamp( "The range specified = " + TrimSigDigits( EIRFTYTmin, 2 ) + " C to " + TrimSigDigits( EIRFTYTmax, 2 ) + " C." );
				ShowRecurringWarningErrorAtEnd( "CHILLER:ELECTRIC:REFORMULATEDEIR \"" + ElecReformEIRChiller( EIRChillNum ).Name + "\": The cond outlet temp range in Electric Input to Cooling Output Ratio as a Function of Temp curve error continues.", ElecReformEIRChiller( EIRChillNum ).EIRFTYIterIndex, CondOutletTemp, CondOutletTemp );
			} else {
				ShowRecurringWarningErrorAtEnd( "CHILLER:ELECTRIC:REFORMULATEDEIR \"" + ElecReformEIRChiller( EIRChillNum ).Name + "\": The cond outlet temp range in Electric Input to Cooling Output Ratio as a Function of Temp curve error continues.", ElecReformEIRChiller( EIRChillNum ).EIRFTYIterIndex, CondOutletTemp, CondOutletTemp );
			}
		}

		if ( ElecReformEIRChiller( EIRChillNum ).PartLoadCurveType == PLR_LeavingCondenserWaterTemperature ) {
			if ( CondOutletTemp < EIRFPLRTmin || CondOutletTemp > EIRFPLRTmax ) {
				++ElecReformEIRChiller( EIRChillNum ).EIRFPLRTIter;
				if ( ElecReformEIRChiller( EIRChillNum ).EIRFPLRTIter == 1 ) {
					ShowWarningError( "CHILLER:ELECTRIC:REFORMULATEDEIR \"" + ElecReformEIRChiller( EIRChillNum ).Name + "\": The condenser outlet temperature (" + TrimSigDigits( CondOutletTemp, 2 ) + " C) is outside the range of condenser outlet temperatures (X var) given in Electric Input to Cooling Output Ratio Function of Part-load Ratio bicubic curve = " + ElecReformEIRChiller( EIRChillNum ).EIRFPLRName );
					ShowContinueErrorTimeStamp( "The range specified = " + TrimSigDigits( EIRFPLRTmin, 2 ) + " C to " + TrimSigDigits( EIRFPLRTmax, 2 ) + " C." );
					ShowRecurringWarningErrorAtEnd( "CHILLER:ELECTRIC:REFORMULATEDEIR \"" + ElecReformEIRChiller( EIRChillNum ).Name + "\": The cond outlet temp range in Electric Input to Cooling Output Ratio Function of PLR curve error continues.", ElecReformEIRChiller( EIRChillNum ).EIRFPLRTIterIndex, CondOutletTemp, CondOutletTemp );
				} else {
					ShowRecurringWarningErrorAtEnd( "CHILLER:ELECTRIC:REFORMULATEDEIR \"" + ElecReformEIRChiller( EIRChillNum ).Name + "\": The cond outlet temp range in Electric Input to Cooling Output Ratio Function of PLR curve error continues.", ElecReformEIRChiller( EIRChillNum ).EIRFPLRTIterIndex, CondOutletTemp, CondOutletTemp );
				}
			}
		}

		if ( ChillerPartLoadRatio < EIRFPLRPLRmin || ChillerPartLoadRatio > EIRFPLRPLRmax ) {
			++ElecReformEIRChiller( EIRChillNum ).EIRFPLRPLRIter;
			if ( ElecReformEIRChiller( EIRChillNum ).EIRFPLRPLRIter == 1 ) {
				ShowWarningError( "CHILLER:ELECTRIC:REFORMULATEDEIR \"" + ElecReformEIRChiller( EIRChillNum ).Name + "\": The part-load ratio (" + TrimSigDigits( ChillerPartLoadRatio, 3 ) + ") is outside the range of part-load ratios (Y var) given in Electric Input to Cooling Output Ratio Function of Part-load Ratio bicubic curve = " + ElecReformEIRChiller( EIRChillNum ).EIRFPLRName );
				ShowContinueErrorTimeStamp( "The range specified = " + TrimSigDigits( EIRFPLRPLRmin, 3 ) + " to " + TrimSigDigits( EIRFPLRPLRmax, 3 ) + '.' );
				ShowRecurringWarningErrorAtEnd( "CHILLER:ELECTRIC:REFORMULATEDEIR \"" + ElecReformEIRChiller( EIRChillNum ).Name + "\": The part-load ratio range in Electric Input to Cooling Output Ratio Function of PLRatio curve error continues.", ElecReformEIRChiller( EIRChillNum ).EIRFPLRPLRIterIndex, ChillerPartLoadRatio, ChillerPartLoadRatio );
			} else {
				ShowRecurringWarningErrorAtEnd( "CHILLER:ELECTRIC:REFORMULATEDEIR \"" + ElecReformEIRChiller( EIRChillNum ).Name + "\": The part-load ratio range in Electric Input to Cooling Output Ratio Function of PLRatio curve error continues.", ElecReformEIRChiller( EIRChillNum ).EIRFPLRPLRIterIndex, ChillerPartLoadRatio, ChillerPartLoadRatio );
			}
		}

		{ auto const SELECT_CASE_var( PlantLoop( PlantLoopNum ).LoopDemandCalcScheme );
		if ( SELECT_CASE_var == SingleSetPoint ) {
			if ( ( ElecReformEIRChiller( EIRChillNum ).FlowMode == LeavingSetPointModulated ) || ( PlantLoop( PlantLoopNum ).LoopSide( LoopSideNum ).Branch( BranchNum ).Comp( CompNum ).CurOpSchemeType == CompSetPtBasedSchemeType ) || ( Node( ElecReformEIRChiller( EIRChillNum ).EvapOutletNodeNum ).TempSetPoint != SensedNodeFlagValue ) ) {
				// there will be a valid setpoint on outlet
				EvapOutletTempSetPoint = Node( EvapOutletNode ).TempSetPoint;
			} else { // use plant loop overall setpoint
				EvapOutletTempSetPoint = Node( PlantLoop( PlantLoopNum ).TempSetPointNodeNum ).TempSetPoint;
			}
		} else if ( SELECT_CASE_var == DualSetPointDeadBand ) {
			if ( ( ElecReformEIRChiller( EIRChillNum ).FlowMode == LeavingSetPointModulated ) || ( PlantLoop( PlantLoopNum ).LoopSide( LoopSideNum ).Branch( BranchNum ).Comp( CompNum ).CurOpSchemeType == CompSetPtBasedSchemeType ) || ( Node( ElecReformEIRChiller( EIRChillNum ).EvapOutletNodeNum ).TempSetPointHi != SensedNodeFlagValue ) ) {
				// there will be a valid setpoint on outlet
				EvapOutletTempSetPoint = Node( EvapOutletNode ).TempSetPointHi;
			} else { // use plant loop overall setpoint
				EvapOutletTempSetPoint = Node( PlantLoop( PlantLoopNum ).TempSetPointNodeNum ).TempSetPointHi;
			}
		} else {
			assert( false );
		}}

		ChillerCapFT = CurveValue( ElecReformEIRChiller( EIRChillNum ).ChillerCapFT, EvapOutletTempSetPoint, CondOutletTemp );

		if ( ChillerCapFT < 0 ) {
			if ( ElecReformEIRChiller( EIRChillNum ).ChillerCapFTError < 1 && PlantLoop( PlantLoopNum ).LoopSide( LoopSideNum ).FlowLock != 0 && ! WarmupFlag ) {
				++ElecReformEIRChiller( EIRChillNum ).ChillerCapFTError;
				ShowWarningError( "CHILLER:ELECTRIC:REFORMULATEDEIR \"" + ElecReformEIRChiller( EIRChillNum ).Name + "\":" );
				ShowContinueError( " Chiller Capacity as a Function of Temperature curve output is negative (" + RoundSigDigits( ChillerCapFT, 3 ) + ")." );
				ShowContinueError( " Negative value occurs using an Evaporator Leaving Temp of " + RoundSigDigits( EvapOutletTempSetPoint, 1 ) + " and a Condenser Leaving Temp of " + RoundSigDigits( CondOutletTemp, 1 ) + '.' );
				ShowContinueErrorTimeStamp( " Resetting curve output to zero and continuing simulation." );
			} else if ( PlantLoop( PlantLoopNum ).LoopSide( LoopSideNum ).FlowLock != 0 && ! WarmupFlag ) {
				++ElecReformEIRChiller( EIRChillNum ).ChillerCapFTError;
				ShowRecurringWarningErrorAtEnd( "CHILLER:ELECTRIC:REFORMULATEDEIR \"" + ElecReformEIRChiller( EIRChillNum ).Name + "\": Chiller Capacity as a Function of Temperature curve output is negative warning continues...", ElecReformEIRChiller( EIRChillNum ).ChillerCapFTErrorIndex, ChillerCapFT, ChillerCapFT );
			}
		}

		ChillerEIRFT = CurveValue( ElecReformEIRChiller( EIRChillNum ).ChillerEIRFT, EvapOutletTemp, CondOutletTemp );

		if ( ChillerEIRFT < 0.0 ) {
			if ( ElecReformEIRChiller( EIRChillNum ).ChillerEIRFTError < 1 && PlantLoop( PlantLoopNum ).LoopSide( LoopSideNum ).FlowLock != 0 && ! WarmupFlag ) {
				++ElecReformEIRChiller( EIRChillNum ).ChillerEIRFTError;
				ShowWarningError( "CHILLER:ELECTRIC:REFORMULATEDEIR \"" + ElecReformEIRChiller( EIRChillNum ).Name + "\":" );
				ShowContinueError( " Reformulated Chiller EIR as a Function of Temperature curve output is negative (" + RoundSigDigits( ChillerEIRFT, 3 ) + ")." );
				ShowContinueError( " Negative value occurs using an Evaporator Leaving Temp of " + RoundSigDigits( EvapOutletTemp, 1 ) + " and a Condenser Leaving Temp of " + RoundSigDigits( CondOutletTemp, 1 ) + '.' );
				ShowContinueErrorTimeStamp( " Resetting curve output to zero and continuing simulation." );
			} else if ( PlantLoop( PlantLoopNum ).LoopSide( LoopSideNum ).FlowLock != 0 && ! WarmupFlag ) {
				++ElecReformEIRChiller( EIRChillNum ).ChillerEIRFTError;
				ShowRecurringWarningErrorAtEnd( "CHILLER:ELECTRIC:REFORMULATEDEIR \"" + ElecReformEIRChiller( EIRChillNum ).Name + "\": Chiller EIR as a Function of Temperature curve output is negative warning continues...", ElecReformEIRChiller( EIRChillNum ).ChillerEIRFTErrorIndex, ChillerEIRFT, ChillerEIRFT );
			}
		}

		if ( ElecReformEIRChiller( EIRChillNum ).PartLoadCurveType == PLR_LeavingCondenserWaterTemperature )  {
			ChillerEIRFPLR = CurveValue( ElecReformEIRChiller( EIRChillNum ).ChillerEIRFPLR, CondOutletTemp, ChillerPartLoadRatio );
		} else if ( ElecReformEIRChiller( EIRChillNum ).PartLoadCurveType == PLR_Lift ) {
			ChillerLift = CondOutletTemp - EvapOutletTemp;
			ChillerTdev = std::abs( EvapOutletTemp - ElecReformEIRChiller( EIRChillNum ).TempRefEvapOut );
			ChillerLiftRef = ElecReformEIRChiller( EIRChillNum ).TempRefCondOut - ElecReformEIRChiller( EIRChillNum ).TempRefEvapOut;

			if ( ChillerLiftRef <= 0 )  ChillerLiftRef = 35 - 6.67;
			ChillerLiftNom = ChillerLift / ChillerLiftRef;
			ChillerTdevNom = ChillerTdev / ChillerLiftRef;

			ChillerEIRFPLR = CurveValue( ElecReformEIRChiller( EIRChillNum ).ChillerEIRFPLR, ChillerLiftNom, ChillerPartLoadRatio, ChillerTdevNom );
		}

		if ( ChillerEIRFPLR < 0.0 ) {
			if ( ElecReformEIRChiller( EIRChillNum ).ChillerEIRFPLRError < 1 && PlantLoop( PlantLoopNum ).LoopSide( LoopSideNum ).FlowLock != 0 && ! WarmupFlag ) {
				++ElecReformEIRChiller( EIRChillNum ).ChillerEIRFPLRError;
				ShowWarningError( "CHILLER:ELECTRIC:REFORMULATEDEIR \"" + ElecReformEIRChiller( EIRChillNum ).Name + "\":" );
				ShowContinueError( " Chiller EIR as a function of PLR and condenser water temperature curve output is negative (" + RoundSigDigits( ChillerEIRFPLR, 3 ) + ")." );
				ShowContinueError( " Negative value occurs using a part-load ratio of " + RoundSigDigits( ChillerPartLoadRatio, 3 ) + " and a Condenser Leaving Temp of " + RoundSigDigits( CondOutletTemp, 1 ) + " C." );
				ShowContinueErrorTimeStamp( " Resetting curve output to zero and continuing simulation." );
			} else if ( PlantLoop( PlantLoopNum ).LoopSide( LoopSideNum ).FlowLock != 0 && ! WarmupFlag ) {
				++ElecReformEIRChiller( EIRChillNum ).ChillerEIRFPLRError;
				ShowRecurringWarningErrorAtEnd( "CHILLER:ELECTRIC:REFORMULATEDEIR \"" + ElecReformEIRChiller( EIRChillNum ).Name + "\": Chiller EIR as a function of PLR curve output is negative warning continues...", ElecReformEIRChiller( EIRChillNum ).ChillerEIRFPLRErrorIndex, ChillerEIRFPLR, ChillerEIRFPLR );
			}
		}

	}

} // ChillerReformulatedEIR

} // EnergyPlus
