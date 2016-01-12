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
#include <ObjexxFCL/string.functions.hh>

// EnergyPlus Headers
#include <ChillerElectricEIR.hh>
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
#include <StandardRatings.hh>
#include <UtilityRoutines.hh>

namespace EnergyPlus {

namespace ChillerElectricEIR {

	// NOTES:
	// The Electric EIR and Reformulated EIR chiller models are similar.
	// They only differ in the independent variable used to evaluate the performance curves.

	// MODULE INFORMATION:
	//       AUTHOR         Richard Raustad
	//       DATE WRITTEN   June 2004
	//       MODIFIED       Chandan Sharma, FSEC, February 2010, Added basin heater
	//                      Brent Griffith, NREL, Sept 2010, revised for plant changes
	//                      generalized fluid properties
	//       RE-ENGINEERED  na

	// PURPOSE OF THIS MODULE:
	//  This module simulates the performance of the electric vapor
	//  compression chiller used in DOE-2.

	// METHODOLOGY EMPLOYED:
	//  Once the PlantLoopManager determines that the Electric EIR chiller
	//  is available to meet a loop cooling demand, it calls SimElectricEIRChiller
	//  which in turn calls the electric EIR model. The EIR chiller model is based on
	//  polynomial fits of chiller performance data.

	// REFERENCES:
	// 1. DOE-2 Engineers Manual, Version 2.1A, November 1982, LBL-11353

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
	int const AirCooled( 1 );
	int const WaterCooled( 2 );
	int const EvapCooled( 3 );

	//chiller flow modes
	int const FlowModeNotSet( 200 );
	int const ConstantFlow( 201 );
	int const NotModulated( 202 );
	int const LeavingSetPointModulated( 203 );

	static std::string const BlankString;

	// MODULE VARIABLE DECLARATIONS:
	int NumElectricEIRChillers( 0 ); // Number of electric EIR chillers specified in input
	Real64 CondMassFlowRate( 0.0 ); // Condenser mass flow rate [kg/s]
	Real64 EvapMassFlowRate( 0.0 ); // Evaporator mass flow rate [kg/s]
	Real64 CondOutletTemp( 0.0 ); // Condenser outlet temperature [C]
	Real64 CondOutletHumRat( 0.0 ); // Condenser outlet humidity ratio [kg/kg]
	Real64 EvapOutletTemp( 0.0 ); // Evaporator outlet temperature [C]
	Real64 Power( 0.0 ); // Rate of chiller electric energy use [W]
	Real64 QEvaporator( 0.0 ); // Rate of heat transfer to the evaporator coil [W]
	Real64 QCondenser( 0.0 ); // Rate of heat transfer to the condenser coil [W]
	Real64 QHeatRecovered( 0.0 ); // Rate of heat transfer to the heat recovery coil [W]
	Real64 HeatRecOutletTemp( 0.0 ); // Heat recovery outlet temperature [C]
	Real64 CondenserFanPower( 0.0 ); // Condenser Fan Power (fan cycles with compressor) [W]
	Real64 ChillerCapFT( 0.0 ); // Chiller capacity fraction (evaluated as a function of temperature)
	Real64 ChillerEIRFT( 0.0 ); // Chiller electric input ratio (EIR = 1 / COP) as a function of temperature
	Real64 ChillerEIRFPLR( 0.0 ); // Chiller EIR as a function of part-load ratio (PLR)
	Real64 ChillerPartLoadRatio( 0.0 ); // Chiller part-load ratio (PLR)
	Real64 ChillerCyclingRatio( 0.0 ); // Chiller cycling ratio
	Real64 BasinHeaterPower( 0.0 ); // Basin heater power (W)
	Real64 ChillerFalseLoadRate( 0.0 ); // Chiller false load over and above the water-side load [W]
	Real64 AvgCondSinkTemp( 0.0 ); // condenser temperature value for use in curves [C]

	Array1D_bool CheckEquipName;

	bool GetInputEIR( true ); // When TRUE, calls subroutine to read input file.

	// SUBROUTINE SPECIFICATIONS FOR MODULE ChillerElectricEIR
	//PUBLIC     SimEIRChillerHeatRecovery

	// Object Data
	Array1D< ElectricEIRChillerSpecs > ElectricEIRChiller; // Dimension to number of machines
	Array1D< ReportEIRVars > ElectricEIRChillerReport;

	// MODULE SUBROUTINES:

	// Beginning of Electric EIR Chiller Module Driver Subroutine
	//*************************************************************************

	// Functions

	void
	SimElectricEIRChiller(
		std::string const & EP_UNUSED( EIRChillerType ), // Type of chiller
		std::string const & EIRChillerName, // User specified name of chiller
		int const EquipFlowCtrl, // Flow control mode for the equipment
		int & CompIndex, // Chiller number pointer
		int const LoopNum, // plant loop index pointer
		bool const RunFlag, // Simulate chiller when TRUE
		bool const FirstIteration, // Initialize variables when TRUE
		bool & InitLoopEquip, // If not zero, calculate the max load for operating conditions
		Real64 & MyLoad, // Loop demand component will meet
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
		//       AUTHOR         Richard Raustad
		//       DATE WRITTEN   June 2004
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		//  This is the electric EIR chiller model driver. It gets the input for the
		//  model, initializes simulation variables, calls the appropriate model and sets
		//  up reporting variables.

		// METHODOLOGY EMPLOYED: na

		// REFERENCES: na

		// Using/Aliasing
		using InputProcessor::FindItemInList;
		using PlantUtilities::UpdateChillerComponentCondenserSide;
		using PlantUtilities::UpdateComponentHeatRecoverySide;
		using DataPlant::TypeOf_Chiller_ElectricEIR;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int EIRChillNum; // Chiller number pointer
		int LoopSide;

		if ( GetInputEIR ) {
			GetElectricEIRChillerInput();
			GetInputEIR = false;
		}

		// Find the correct Chiller
		if ( CompIndex == 0 ) {
			EIRChillNum = FindItemInList( EIRChillerName, ElectricEIRChiller );
			if ( EIRChillNum == 0 ) {
				ShowFatalError( "SimElectricEIRChiller: Specified Chiller not one of Valid EIR Electric Chillers=" + EIRChillerName );
			}
			CompIndex = EIRChillNum;
		} else {
			EIRChillNum = CompIndex;
			if ( EIRChillNum > NumElectricEIRChillers || EIRChillNum < 1 ) {
				ShowFatalError( "SimElectricEIRChiller:  Invalid CompIndex passed=" + TrimSigDigits( EIRChillNum ) + ", Number of Units=" + TrimSigDigits( NumElectricEIRChillers ) + ", Entered Unit name=" + EIRChillerName );
			}
			if ( CheckEquipName( EIRChillNum ) ) {
				if ( EIRChillerName != ElectricEIRChiller( EIRChillNum ).Name ) {
					ShowFatalError( "SimElectricEIRChiller: Invalid CompIndex passed=" + TrimSigDigits( EIRChillNum ) + ", Unit name=" + EIRChillerName + ", stored Unit Name for that index=" + ElectricEIRChiller( EIRChillNum ).Name );
				}
				CheckEquipName( EIRChillNum ) = false;
			}
		}

		if ( InitLoopEquip ) {
			TempEvapOutDesign = ElectricEIRChiller( EIRChillNum ).TempRefEvapOut;
			TempCondInDesign = ElectricEIRChiller( EIRChillNum ).TempRefCondIn;

			InitElectricEIRChiller( EIRChillNum, RunFlag, MyLoad );

			if ( LoopNum == ElectricEIRChiller( EIRChillNum ).CWLoopNum ) {
				SizeElectricEIRChiller( EIRChillNum );
				MinCap = ElectricEIRChiller( EIRChillNum ).RefCap * ElectricEIRChiller( EIRChillNum ).MinPartLoadRat;
				MaxCap = ElectricEIRChiller( EIRChillNum ).RefCap * ElectricEIRChiller( EIRChillNum ).MaxPartLoadRat;
				OptCap = ElectricEIRChiller( EIRChillNum ).RefCap * ElectricEIRChiller( EIRChillNum ).OptPartLoadRat;
			} else {
				MinCap = 0.0;
				MaxCap = 0.0;
				OptCap = 0.0;
			}
			if ( GetSizingFactor ) {
				SizingFactor = ElectricEIRChiller( EIRChillNum ).SizFac;
			}
			return;
		}

		if ( LoopNum == ElectricEIRChiller( EIRChillNum ).CWLoopNum ) {
			InitElectricEIRChiller( EIRChillNum, RunFlag, MyLoad );
			CalcElectricEIRChillerModel( EIRChillNum, MyLoad, RunFlag, FirstIteration, EquipFlowCtrl );
			UpdateElectricEIRChillerRecords( MyLoad, RunFlag, EIRChillNum );

		} else if ( LoopNum == ElectricEIRChiller( EIRChillNum ).CDLoopNum ) {
			LoopSide = ElectricEIRChiller( EIRChillNum ).CDLoopSideNum;
			UpdateChillerComponentCondenserSide( LoopNum, LoopSide, TypeOf_Chiller_ElectricEIR, ElectricEIRChiller( EIRChillNum ).CondInletNodeNum, ElectricEIRChiller( EIRChillNum ).CondOutletNodeNum, ElectricEIRChillerReport( EIRChillNum ).QCond, ElectricEIRChillerReport( EIRChillNum ).CondInletTemp, ElectricEIRChillerReport( EIRChillNum ).CondOutletTemp, ElectricEIRChillerReport( EIRChillNum ).Condmdot, FirstIteration );

		} else if ( LoopNum == ElectricEIRChiller( EIRChillNum ).HRLoopNum ) {
			UpdateComponentHeatRecoverySide( ElectricEIRChiller( EIRChillNum ).HRLoopNum, ElectricEIRChiller( EIRChillNum ).HRLoopSideNum, TypeOf_Chiller_ElectricEIR, ElectricEIRChiller( EIRChillNum ).HeatRecInletNodeNum, ElectricEIRChiller( EIRChillNum ).HeatRecOutletNodeNum, ElectricEIRChillerReport( EIRChillNum ).QHeatRecovery, ElectricEIRChillerReport( EIRChillNum ).HeatRecInletTemp, ElectricEIRChillerReport( EIRChillNum ).HeatRecOutletTemp, ElectricEIRChillerReport( EIRChillNum ).HeatRecMassFlow, FirstIteration );
		}

	}

	// End Electric EIR Chiller Module Driver Subroutine
	//******************************************************************************

	void
	GetElectricEIRChillerInput()
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR:          Richard Raustad, FSEC
		//       DATE WRITTEN:    June 2004

		// PURPOSE OF THIS SUBROUTINE:
		//  This routine will get the input required by the Electric EIR Chiller model.

		// METHODOLOGY EMPLOYED:

		// REFERENCES: na

		// Using/Aliasing
		using DataGlobals::MaxNameLength;
		using InputProcessor::GetNumObjectsFound;
		using InputProcessor::GetObjectItem;
		using InputProcessor::VerifyName;
		using InputProcessor::SameString;
		using namespace DataIPShortCuts; // Data for field names, blank numerics
		using BranchNodeConnections::TestCompSet;
		using NodeInputManager::GetOnlySingleNode;
		using CurveManager::GetCurveIndex;
		using FluidProperties::FindGlycol;
		using CurveManager::CurveValue;
		using General::TrimSigDigits;
		using General::RoundSigDigits;
		using GlobalNames::VerifyUniqueChillerName;
		using OutAirNodeManager::CheckAndAddAirNodeNumber;
		using PlantUtilities::RegisterPlantCompDesignFlow;
		using ScheduleManager::GetScheduleIndex;
		using DataSizing::AutoSize;
		using DataGlobals::AnyEnergyManagementSystemInModel;

		// Locals
		// PARAMETERS
		static std::string const RoutineName( "GetElectricEIRChillerInput: " ); // include trailing blank space

		// LOCAL VARIABLES
		int EIRChillerNum; // Chiller counter
		int NumAlphas; // Number of elements in the alpha array
		int NumNums; // Number of elements in the numeric array
		int IOStat; // IO Status when calling get input subroutine
		static bool ErrorsFound( false ); // True when input errors are found
		bool IsNotOK; // Flag to verify name
		bool IsBlank; // Flag for blank name
		Real64 CurveVal; // Used to verify EIR-FT and CAP-FT curves equal 1 at reference conditions
		static bool FoundNegValue( false ); // Used to evaluate PLFFPLR curve objects
		static int CurveCheck( 0 ); // Used to evaluate PLFFPLR curve objects
		Array1D< Real64 > CurveValArray( 11 ); // Used to evaluate PLFFPLR curve objects
		Real64 CurveValTmp; // Used to evaluate PLFFPLR curve objects
		bool errFlag; // Used to tell if a unique chiller name has been specified
		std::string StringVar; // Used for EIRFPLR warning messages
		int CurveValPtr; // Index to EIRFPLR curve output
		static bool AllocatedFlag( false ); // True when arrays are allocated
		bool Okay;

		// Formats
		static gio::Fmt Format_530( "('Curve Output = ',11(F7.2))" );

		// FLOW

		if ( AllocatedFlag ) return;
		cCurrentModuleObject = "Chiller:Electric:EIR";
		NumElectricEIRChillers = GetNumObjectsFound( cCurrentModuleObject );

		if ( NumElectricEIRChillers <= 0 ) {
			ShowSevereError( "No " + cCurrentModuleObject + " equipment specified in input file" );
			ErrorsFound = true;
		}

		// ALLOCATE ARRAYS
		ElectricEIRChiller.allocate( NumElectricEIRChillers );
		ElectricEIRChillerReport.allocate( NumElectricEIRChillers );
		CheckEquipName.dimension( NumElectricEIRChillers, true );
		AllocatedFlag = true;

		// Load arrays with electric EIR chiller data
		for ( EIRChillerNum = 1; EIRChillerNum <= NumElectricEIRChillers; ++EIRChillerNum ) {
			GetObjectItem( cCurrentModuleObject, EIRChillerNum, cAlphaArgs, NumAlphas, rNumericArgs, NumNums, IOStat, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );

			IsNotOK = false;
			IsBlank = false;
			VerifyName( cAlphaArgs( 1 ), ElectricEIRChiller, EIRChillerNum - 1, IsNotOK, IsBlank, cCurrentModuleObject + " Name" );
			if ( IsNotOK ) {
				ErrorsFound = true;
				if ( IsBlank ) cAlphaArgs( 1 ) = "xxxxx";
			}
			VerifyUniqueChillerName( cCurrentModuleObject, cAlphaArgs( 1 ), errFlag, cCurrentModuleObject + " Name" );
			if ( errFlag ) {
				ErrorsFound = true;
			}
			ElectricEIRChiller( EIRChillerNum ).Name = cAlphaArgs( 1 );

			//   Performance curves
			ElectricEIRChiller( EIRChillerNum ).ChillerCapFT = GetCurveIndex( cAlphaArgs( 2 ) );
			if ( ElectricEIRChiller( EIRChillerNum ).ChillerCapFT == 0 ) {
				ShowSevereError( RoutineName + cCurrentModuleObject + " \"" + cAlphaArgs( 1 ) + "\"" );
				ShowContinueError( "Invalid " + cAlphaFieldNames( 2 ) + '=' + cAlphaArgs( 2 ) );
				ErrorsFound = true;
			}

			ElectricEIRChiller( EIRChillerNum ).ChillerEIRFT = GetCurveIndex( cAlphaArgs( 3 ) );
			if ( ElectricEIRChiller( EIRChillerNum ).ChillerEIRFT == 0 ) {
				ShowSevereError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\"" );
				ShowContinueError( "Invalid " + cAlphaFieldNames( 3 ) + '=' + cAlphaArgs( 3 ) );
				ErrorsFound = true;
			}

			ElectricEIRChiller( EIRChillerNum ).ChillerEIRFPLR = GetCurveIndex( cAlphaArgs( 4 ) );
			if ( ElectricEIRChiller( EIRChillerNum ).ChillerEIRFPLR == 0 ) {
				ShowSevereError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\"" );
				ShowContinueError( "Invalid " + cAlphaFieldNames( 4 ) + '=' + cAlphaArgs( 4 ) );
				ErrorsFound = true;
			}

			ElectricEIRChiller( EIRChillerNum ).EvapInletNodeNum = GetOnlySingleNode( cAlphaArgs( 5 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Water, NodeConnectionType_Inlet, 1, ObjectIsNotParent );
			ElectricEIRChiller( EIRChillerNum ).EvapOutletNodeNum = GetOnlySingleNode( cAlphaArgs( 6 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Water, NodeConnectionType_Outlet, 1, ObjectIsNotParent );
			TestCompSet( cCurrentModuleObject, cAlphaArgs( 1 ), cAlphaArgs( 5 ), cAlphaArgs( 6 ), "Chilled Water Nodes" );

			if ( SameString( cAlphaArgs( 9 ), "WaterCooled" ) ) {
				ElectricEIRChiller( EIRChillerNum ).CondenserType = WaterCooled;
			} else if ( SameString( cAlphaArgs( 9 ), "AirCooled" ) ) {
				ElectricEIRChiller( EIRChillerNum ).CondenserType = AirCooled;
			} else if ( SameString( cAlphaArgs( 9 ), "EvaporativelyCooled" ) ) {
				ElectricEIRChiller( EIRChillerNum ).CondenserType = EvapCooled;
			} else {
				ShowSevereError( RoutineName + cCurrentModuleObject + ": " + cAlphaArgs( 1 ) );
				ShowContinueError( "Invalid " + cAlphaFieldNames( 9 ) + '=' + cAlphaArgs( 9 ) );
				ShowContinueError( "Valid entries are AirCooled, WaterCooled, or EvaporativelyCooled" );
				ErrorsFound = true;
			}

			if ( ElectricEIRChiller( EIRChillerNum ).CondenserType == AirCooled || ElectricEIRChiller( EIRChillerNum ).CondenserType == EvapCooled ) {
				// Connection not required for air or evap cooled condenser
				// If the condenser inlet is blank for air cooled and evap cooled condensers then supply a generic name
				// since it is not used elsewhere for connection
				if ( lAlphaFieldBlanks( 7 ) ) {
					if ( len( cAlphaArgs( 1 ) ) < MaxNameLength - 25 ) { // protect against long name leading to > 100 chars
						cAlphaArgs( 7 ) = cAlphaArgs( 1 ) + " INLET NODE FOR CONDENSER";
					} else {
						cAlphaArgs( 7 ) = cAlphaArgs( 1 ).substr( 0, 75 ) + " INLET NODE FOR CONDENSER";
					}
				}
				if ( lAlphaFieldBlanks( 8 ) ) {
					if ( len( cAlphaArgs( 1 ) ) < MaxNameLength - 26 ) { // protect against long name leading to > 100 chars
						cAlphaArgs( 8 ) = cAlphaArgs( 1 ) + " OUTLET NODE FOR CONDENSER";
					} else {
						cAlphaArgs( 8 ) = cAlphaArgs( 1 ).substr( 0, 74 ) + " OUTLET NODE FOR CONDENSER";
					}
				}

				ElectricEIRChiller( EIRChillerNum ).CondInletNodeNum = GetOnlySingleNode( cAlphaArgs( 7 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Air, NodeConnectionType_OutsideAirReference, 2, ObjectIsNotParent );
				CheckAndAddAirNodeNumber( ElectricEIRChiller( EIRChillerNum ).CondInletNodeNum, Okay );
				if ( ! Okay ) {
					ShowWarningError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\"" );
					ShowContinueError( "Adding OutdoorAir:Node=" + cAlphaArgs( 7 ) );
				}

				ElectricEIRChiller( EIRChillerNum ).CondOutletNodeNum = GetOnlySingleNode( cAlphaArgs( 8 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Air, NodeConnectionType_Outlet, 2, ObjectIsNotParent );

			} else if ( ElectricEIRChiller( EIRChillerNum ).CondenserType == WaterCooled ) {
				// Condenser inlet node name is necessary for water-cooled condenser
				if ( lAlphaFieldBlanks( 7 ) || lAlphaFieldBlanks( 8 ) ) {
					ShowSevereError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\"" );
					ShowContinueError( "Condenser Inlet or Outlet Node Name is blank." );
					ErrorsFound = true;
				}

				ElectricEIRChiller( EIRChillerNum ).CondInletNodeNum = GetOnlySingleNode( cAlphaArgs( 7 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Water, NodeConnectionType_Inlet, 2, ObjectIsNotParent );

				ElectricEIRChiller( EIRChillerNum ).CondOutletNodeNum = GetOnlySingleNode( cAlphaArgs( 8 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Water, NodeConnectionType_Outlet, 2, ObjectIsNotParent );

				TestCompSet( cCurrentModuleObject, cAlphaArgs( 1 ), cAlphaArgs( 7 ), cAlphaArgs( 8 ), "Condenser Water Nodes" );

			} else {
				// Condenser inlet node name is necessary (never should reach this part of code)
				if ( lAlphaFieldBlanks( 7 ) || lAlphaFieldBlanks( 8 ) ) {
					ShowSevereError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\"" );
					ShowContinueError( "Condenser Inlet or Outlet Node Name is blank." );
					ErrorsFound = true;
				}
				ElectricEIRChiller( EIRChillerNum ).CondInletNodeNum = GetOnlySingleNode( cAlphaArgs( 7 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Unknown, NodeConnectionType_Inlet, 2, ObjectIsNotParent );

				ElectricEIRChiller( EIRChillerNum ).CondOutletNodeNum = GetOnlySingleNode( cAlphaArgs( 8 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Unknown, NodeConnectionType_Outlet, 2, ObjectIsNotParent );

				TestCompSet( cCurrentModuleObject, cAlphaArgs( 1 ), cAlphaArgs( 7 ), cAlphaArgs( 8 ), "Condenser (unknown?) Nodes" );

			}

			{ auto const SELECT_CASE_var( cAlphaArgs( 10 ) );
			if ( SELECT_CASE_var == "CONSTANTFLOW" ) {
				ElectricEIRChiller( EIRChillerNum ).FlowMode = ConstantFlow;
			} else if ( SELECT_CASE_var == "VARIABLEFLOW" ) {
				ElectricEIRChiller( EIRChillerNum ).FlowMode = LeavingSetPointModulated;
				ShowWarningError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\"," );
				ShowContinueError( "Invalid " + cAlphaFieldNames( 10 ) + '=' + cAlphaArgs( 10 ) );
				ShowContinueError( "Key choice is now called \"LeavingSetpointModulated\" and the simulation continues" );
			} else if ( SELECT_CASE_var == "LEAVINGSETPOINTMODULATED" ) {
				ElectricEIRChiller( EIRChillerNum ).FlowMode = LeavingSetPointModulated;
			} else if ( SELECT_CASE_var == "NOTMODULATED" ) {
				ElectricEIRChiller( EIRChillerNum ).FlowMode = NotModulated;
			} else {
				ShowSevereError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\"," );
				ShowContinueError( "Invalid " + cAlphaFieldNames( 10 ) + '=' + cAlphaArgs( 10 ) );
				ShowContinueError( "Available choices are ConstantFlow, NotModulated, or LeavingSetpointModulated" );
				ShowContinueError( "Flow mode NotModulated is assumed and the simulation continues." );
				ElectricEIRChiller( EIRChillerNum ).FlowMode = NotModulated;
			}}

			//   Chiller rated performance data
			ElectricEIRChiller( EIRChillerNum ).RefCap = rNumericArgs( 1 );
			if ( ElectricEIRChiller( EIRChillerNum ).RefCap == AutoSize ) {
				ElectricEIRChiller( EIRChillerNum ).RefCapWasAutoSized = true;
			}
			if ( rNumericArgs( 1 ) == 0.0 ) {
				ShowSevereError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\"" );
				ShowContinueError( "Invalid " + cNumericFieldNames( 1 ) + '=' + RoundSigDigits( rNumericArgs( 1 ), 2 ) );
				ErrorsFound = true;
			}
			ElectricEIRChiller( EIRChillerNum ).RefCOP = rNumericArgs( 2 );
			if ( rNumericArgs( 2 ) == 0.0 ) {
				ShowSevereError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\"" );
				ShowContinueError( "Invalid " + cNumericFieldNames( 2 ) + '=' + RoundSigDigits( rNumericArgs( 2 ), 2 ) );
				ErrorsFound = true;
			}
			ElectricEIRChiller( EIRChillerNum ).TempRefEvapOut = rNumericArgs( 3 );
			ElectricEIRChiller( EIRChillerNum ).TempRefCondIn = rNumericArgs( 4 );
			ElectricEIRChiller( EIRChillerNum ).EvapVolFlowRate = rNumericArgs( 5 );
			if ( ElectricEIRChiller( EIRChillerNum ).EvapVolFlowRate == AutoSize ) {
				ElectricEIRChiller( EIRChillerNum ).EvapVolFlowRateWasAutoSized = true;
			}
			if ( ElectricEIRChiller( EIRChillerNum ).CondenserType == AirCooled || ElectricEIRChiller( EIRChillerNum ).CondenserType == EvapCooled ) { // Condenser flow rate not used for these cond types
				ElectricEIRChiller( EIRChillerNum ).CondVolFlowRate = 0.0011;
			} else {
				ElectricEIRChiller( EIRChillerNum ).CondVolFlowRate = rNumericArgs( 6 );
			}
			if ( ElectricEIRChiller( EIRChillerNum ).CondVolFlowRate == AutoSize ) {
				ElectricEIRChiller( EIRChillerNum ).CondVolFlowRateWasAutoSized = true;
			}

			ElectricEIRChiller( EIRChillerNum ).MinPartLoadRat = rNumericArgs( 7 );
			ElectricEIRChiller( EIRChillerNum ).MaxPartLoadRat = rNumericArgs( 8 );
			ElectricEIRChiller( EIRChillerNum ).OptPartLoadRat = rNumericArgs( 9 );
			ElectricEIRChiller( EIRChillerNum ).MinUnloadRat = rNumericArgs( 10 );
			ElectricEIRChiller( EIRChillerNum ).SizFac = rNumericArgs( 15 );
			if ( ElectricEIRChiller( EIRChillerNum ).SizFac <= 0.0 ) ElectricEIRChiller( EIRChillerNum ).SizFac = 1.0;

			if ( ElectricEIRChiller( EIRChillerNum ).MinPartLoadRat > ElectricEIRChiller( EIRChillerNum ).MaxPartLoadRat ) {
				ShowSevereError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\"" );
				ShowContinueError( cNumericFieldNames( 7 ) + " [" + RoundSigDigits( rNumericArgs( 7 ), 3 ) + "] > " + cNumericFieldNames( 8 ) + " [" + RoundSigDigits( rNumericArgs( 8 ), 3 ) + ']' );
				ShowContinueError( "Minimum part load ratio must be less than or equal to the maximum part load ratio " );
				ErrorsFound = true;
			}

			if ( ElectricEIRChiller( EIRChillerNum ).MinUnloadRat < ElectricEIRChiller( EIRChillerNum ).MinPartLoadRat || ElectricEIRChiller( EIRChillerNum ).MinUnloadRat > ElectricEIRChiller( EIRChillerNum ).MaxPartLoadRat ) {
				ShowSevereError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\"" );
				ShowContinueError( cNumericFieldNames( 10 ) + " = " + RoundSigDigits( rNumericArgs( 10 ), 3 ) );
				ShowContinueError( cNumericFieldNames( 10 ) + " must be greater than or equal to the " + cNumericFieldNames( 7 ) );
				ShowContinueError( cNumericFieldNames( 10 ) + " must be less than or equal to the " + cNumericFieldNames( 8 ) );
				ErrorsFound = true;
			}

			if ( ElectricEIRChiller( EIRChillerNum ).OptPartLoadRat < ElectricEIRChiller( EIRChillerNum ).MinPartLoadRat || ElectricEIRChiller( EIRChillerNum ).OptPartLoadRat > ElectricEIRChiller( EIRChillerNum ).MaxPartLoadRat ) {
				ShowSevereError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\"" );
				ShowContinueError( cNumericFieldNames( 9 ) + " = " + RoundSigDigits( rNumericArgs( 9 ), 3 ) );
				ShowContinueError( cNumericFieldNames( 9 ) + " must be greater than or equal to the " + cNumericFieldNames( 7 ) );
				ShowContinueError( cNumericFieldNames( 9 ) + " must be less than or equal to the " + cNumericFieldNames( 8 ) );
				ErrorsFound = true;
			}

			ElectricEIRChiller( EIRChillerNum ).CondenserFanPowerRatio = rNumericArgs( 11 );
			ElectricEIRChiller( EIRChillerNum ).CompPowerToCondenserFrac = rNumericArgs( 12 );

			if ( ElectricEIRChiller( EIRChillerNum ).CompPowerToCondenserFrac < 0.0 || ElectricEIRChiller( EIRChillerNum ).CompPowerToCondenserFrac > 1.0 ) {
				ShowSevereError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\"" );
				ShowContinueError( cNumericFieldNames( 12 ) + " = " + RoundSigDigits( rNumericArgs( 12 ), 3 ) );
				ShowContinueError( cNumericFieldNames( 12 ) + " must be greater than or equal to zero" );
				ShowContinueError( cNumericFieldNames( 12 ) + " must be less than or equal to one" );
				ErrorsFound = true;
			}

			ElectricEIRChiller( EIRChillerNum ).TempLowLimitEvapOut = rNumericArgs( 13 );

			// These are the heat recovery inputs
			ElectricEIRChiller( EIRChillerNum ).DesignHeatRecVolFlowRate = rNumericArgs( 14 );
			if ( ElectricEIRChiller( EIRChillerNum ).DesignHeatRecVolFlowRate == AutoSize ) {
				ElectricEIRChiller( EIRChillerNum ).DesignHeatRecVolFlowRateWasAutoSized = true;
			}
			if ( ( ElectricEIRChiller( EIRChillerNum ).DesignHeatRecVolFlowRate > 0.0 ) || ( ElectricEIRChiller( EIRChillerNum ).DesignHeatRecVolFlowRate == AutoSize ) ) {
				ElectricEIRChiller( EIRChillerNum ).HeatRecActive = true;
				ElectricEIRChiller( EIRChillerNum ).HeatRecInletNodeNum = GetOnlySingleNode( cAlphaArgs( 11 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Water, NodeConnectionType_Inlet, 3, ObjectIsNotParent );
				if ( ElectricEIRChiller( EIRChillerNum ).HeatRecInletNodeNum == 0 ) {
					ShowSevereError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\"" );
					ShowContinueError( "Invalid " + cAlphaFieldNames( 11 ) + '=' + cAlphaArgs( 11 ) );
					ErrorsFound = true;
				}
				ElectricEIRChiller( EIRChillerNum ).HeatRecOutletNodeNum = GetOnlySingleNode( cAlphaArgs( 12 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Water, NodeConnectionType_Outlet, 3, ObjectIsNotParent );
				if ( ElectricEIRChiller( EIRChillerNum ).HeatRecOutletNodeNum == 0 ) {
					ShowSevereError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\"" );
					ShowContinueError( "Invalid " + cAlphaFieldNames( 12 ) + '=' + cAlphaArgs( 12 ) );
					ErrorsFound = true;
				}
				if ( ElectricEIRChiller( EIRChillerNum ).CondenserType != WaterCooled ) {
					ShowSevereError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\"" );
					ShowContinueError( "Heat Recovery requires a Water Cooled Condenser." );
					ErrorsFound = true;
				}

				TestCompSet( cCurrentModuleObject, cAlphaArgs( 1 ), cAlphaArgs( 11 ), cAlphaArgs( 12 ), "Heat Recovery Nodes" );
				//store heat recovery volume flow for plant sizing
				if ( ElectricEIRChiller( EIRChillerNum ).DesignHeatRecVolFlowRate > 0.0 ) {
					RegisterPlantCompDesignFlow( ElectricEIRChiller( EIRChillerNum ).HeatRecInletNodeNum, ElectricEIRChiller( EIRChillerNum ).DesignHeatRecVolFlowRate ); //CR 6953
				}
				if ( NumNums > 17 ) {
					if ( ! lNumericFieldBlanks( 18 ) ) {
						ElectricEIRChiller( EIRChillerNum ).HeatRecCapacityFraction = rNumericArgs( 18 );
					} else {
						ElectricEIRChiller( EIRChillerNum ).HeatRecCapacityFraction = 1.0;
					}
				} else {
					ElectricEIRChiller( EIRChillerNum ).HeatRecCapacityFraction = 1.0;
				}

				if ( NumAlphas > 13 ) {
					if ( ! lAlphaFieldBlanks( 14 ) ) {
						ElectricEIRChiller( EIRChillerNum ).HeatRecInletLimitSchedNum = GetScheduleIndex( cAlphaArgs( 14 ) );
						if ( ElectricEIRChiller( EIRChillerNum ).HeatRecInletLimitSchedNum == 0 ) {
							ShowSevereError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\"" );
							ShowContinueError( "Invalid " + cAlphaFieldNames( 14 ) + '=' + cAlphaArgs( 14 ) );
							ErrorsFound = true;
						}
					} else {
						ElectricEIRChiller( EIRChillerNum ).HeatRecInletLimitSchedNum = 0;
					}
				} else {
					ElectricEIRChiller( EIRChillerNum ).HeatRecInletLimitSchedNum = 0;
				}

				if ( NumAlphas > 14 ) {
					if ( ! lAlphaFieldBlanks( 15 ) ) {
						ElectricEIRChiller( EIRChillerNum ).HeatRecSetPointNodeNum = GetOnlySingleNode( cAlphaArgs( 15 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Water, NodeConnectionType_Sensor, 1, ObjectIsNotParent );
					} else {
						ElectricEIRChiller( EIRChillerNum ).HeatRecSetPointNodeNum = 0;
					}
				} else {
					ElectricEIRChiller( EIRChillerNum ).HeatRecSetPointNodeNum = 0;
				}

			} else {
				ElectricEIRChiller( EIRChillerNum ).HeatRecActive = false;
				ElectricEIRChiller( EIRChillerNum ).DesignHeatRecMassFlowRate = 0.0;
				ElectricEIRChiller( EIRChillerNum ).HeatRecInletNodeNum = 0;
				ElectricEIRChiller( EIRChillerNum ).HeatRecOutletNodeNum = 0;
				if ( ! lAlphaFieldBlanks( 11 ) || ! lAlphaFieldBlanks( 12 ) ) {
					//  IF (cAlphaArgs(11) /= ' ' .or. cAlphaArgs(12) /= ' ') THEN
					ShowWarningError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\"" );
					ShowContinueError( "Since Reference Heat Reclaim Volume Flow Rate = 0.0, heat recovery is inactive." );
					ShowContinueError( "However, node names were specified for heat recovery inlet or outlet nodes." );
				}
			}

			//   Check the CAP-FT, EIR-FT, and PLR curves and warn user if different from 1.0 by more than +-10%
			if ( ElectricEIRChiller( EIRChillerNum ).ChillerCapFT > 0 ) {
				CurveVal = CurveValue( ElectricEIRChiller( EIRChillerNum ).ChillerCapFT, ElectricEIRChiller( EIRChillerNum ).TempRefEvapOut, ElectricEIRChiller( EIRChillerNum ).TempRefCondIn );
				if ( CurveVal > 1.10 || CurveVal < 0.90 ) {
					ShowWarningError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\"" );
					ShowContinueError( "Capacity ratio as a function of temperature curve output is not equal to 1.0 (+ or - 10%) at reference conditions." );
					ShowContinueError( "Curve output at reference conditions = " + TrimSigDigits( CurveVal, 3 ) );
				}
			}

			if ( ElectricEIRChiller( EIRChillerNum ).ChillerEIRFT > 0 ) {
				CurveVal = CurveValue( ElectricEIRChiller( EIRChillerNum ).ChillerEIRFT, ElectricEIRChiller( EIRChillerNum ).TempRefEvapOut, ElectricEIRChiller( EIRChillerNum ).TempRefCondIn );
				if ( CurveVal > 1.10 || CurveVal < 0.90 ) {
					ShowWarningError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\"" );
					ShowContinueError( "Energy input ratio as a function of temperature curve output is not equal to 1.0 (+ or - 10%) at reference conditions." );
					ShowContinueError( "Curve output at reference conditions = " + TrimSigDigits( CurveVal, 3 ) );
				}
			}

			if ( ElectricEIRChiller( EIRChillerNum ).ChillerEIRFPLR > 0 ) {
				CurveVal = CurveValue( ElectricEIRChiller( EIRChillerNum ).ChillerEIRFPLR, 1.0 );

				if ( CurveVal > 1.10 || CurveVal < 0.90 ) {
					ShowWarningError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\"" );
					ShowContinueError( "Energy input ratio as a function of part-load ratio curve output is not equal to 1.0 (+ or - 10%) at reference conditions." );
					ShowContinueError( "Curve output at reference conditions = " + TrimSigDigits( CurveVal, 3 ) );
				}
			}

			if ( ElectricEIRChiller( EIRChillerNum ).ChillerEIRFPLR > 0 ) {
				FoundNegValue = false;
				for ( CurveCheck = 0; CurveCheck <= 10; ++CurveCheck ) {
					CurveValTmp = CurveValue( ElectricEIRChiller( EIRChillerNum ).ChillerEIRFPLR, double( CurveCheck / 10.0 ) );
					if ( CurveValTmp < 0.0 ) FoundNegValue = true;
					CurveValArray( CurveCheck + 1 ) = int( CurveValTmp * 100.0 ) / 100.0;
				}
				if ( FoundNegValue ) {
					ShowSevereError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\"" );
					ShowContinueError( "Energy input ratio as a function of part-load ratio curve shows negative values." );
					ShowContinueError( "EIR as a function of PLR curve output at various part-load ratios shown below:" );
					ShowContinueError( "PLR          =    0.00   0.10   0.20   0.30   0.40   0.50   0.60   0.70   0.80   0.90   1.00" );
					gio::write( StringVar, "'Curve Output = '" );
					static gio::Fmt fmtF72( "((F7.2),$)" );
					for ( CurveValPtr = 1; CurveValPtr <= 11; ++CurveValPtr ) {
						gio::write( StringVar, fmtF72 ) << CurveValArray( CurveValPtr );
					}
					gio::write( StringVar );
					ShowContinueError( StringVar );
					ErrorsFound = true;
				}
			}
			//   Basin heater power as a function of temperature must be greater than or equal to 0
			ElectricEIRChiller( EIRChillerNum ).BasinHeaterPowerFTempDiff = rNumericArgs( 16 );
			if ( rNumericArgs( 16 ) < 0.0 ) {
				ShowSevereError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\"" );
				ShowContinueError( cNumericFieldNames( 16 ) + " must be >= 0" );
				ErrorsFound = true;
			}

			ElectricEIRChiller( EIRChillerNum ).BasinHeaterSetPointTemp = rNumericArgs( 17 );

			if ( ElectricEIRChiller( EIRChillerNum ).BasinHeaterPowerFTempDiff > 0.0 ) {
				if ( NumNums < 17 ) {
					ElectricEIRChiller( EIRChillerNum ).BasinHeaterSetPointTemp = 2.0;
				}
				if ( ElectricEIRChiller( EIRChillerNum ).BasinHeaterSetPointTemp < 2.0 ) {
					ShowWarningError( RoutineName + cCurrentModuleObject + " \"" + cAlphaArgs( 1 ) + "\"" );
					ShowContinueError( cNumericFieldNames( 17 ) + " is less than 2 deg C. Freezing could occur." );
				}
			}

			if ( ! lAlphaFieldBlanks( 13 ) ) {
				ElectricEIRChiller( EIRChillerNum ).BasinHeaterSchedulePtr = GetScheduleIndex( cAlphaArgs( 13 ) );
				if ( ElectricEIRChiller( EIRChillerNum ).BasinHeaterSchedulePtr == 0 ) {
					ShowWarningError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\"" );
					ShowWarningError( cAlphaFieldNames( 13 ) + " \"" + cAlphaArgs( 13 ) + "\" was not found. Basin heater operation will not be modeled and the simulation continues" );
				}
			}

		}

		if ( ErrorsFound ) {
			ShowFatalError( "Errors found in processing input for " + cCurrentModuleObject );
		}

		for ( EIRChillerNum = 1; EIRChillerNum <= NumElectricEIRChillers; ++EIRChillerNum ) {
			SetupOutputVariable( "Chiller Part Load Ratio []", ElectricEIRChillerReport( EIRChillerNum ).ChillerPartLoadRatio, "System", "Average", ElectricEIRChiller( EIRChillerNum ).Name );
			SetupOutputVariable( "Chiller Cycling Ratio []", ElectricEIRChillerReport( EIRChillerNum ).ChillerCyclingRatio, "System", "Average", ElectricEIRChiller( EIRChillerNum ).Name );
			SetupOutputVariable( "Chiller Electric Power [W]", ElectricEIRChillerReport( EIRChillerNum ).Power, "System", "Average", ElectricEIRChiller( EIRChillerNum ).Name );
			SetupOutputVariable( "Chiller Electric Energy [J]", ElectricEIRChillerReport( EIRChillerNum ).Energy, "System", "Sum", ElectricEIRChiller( EIRChillerNum ).Name, _, "ELECTRICITY", "Cooling", _, "Plant" );

			SetupOutputVariable( "Chiller Evaporator Cooling Rate [W]", ElectricEIRChillerReport( EIRChillerNum ).QEvap, "System", "Average", ElectricEIRChiller( EIRChillerNum ).Name );
			SetupOutputVariable( "Chiller Evaporator Cooling Energy [J]", ElectricEIRChillerReport( EIRChillerNum ).EvapEnergy, "System", "Sum", ElectricEIRChiller( EIRChillerNum ).Name, _, "ENERGYTRANSFER", "CHILLERS", _, "Plant" );
			SetupOutputVariable( "Chiller False Load Heat Transfer Rate [W]", ElectricEIRChillerReport( EIRChillerNum ).ChillerFalseLoadRate, "System", "Average", ElectricEIRChiller( EIRChillerNum ).Name );
			SetupOutputVariable( "Chiller False Load Heat Transfer Energy [J]", ElectricEIRChillerReport( EIRChillerNum ).ChillerFalseLoad, "System", "Sum", ElectricEIRChiller( EIRChillerNum ).Name );
			SetupOutputVariable( "Chiller Evaporator Inlet Temperature [C]", ElectricEIRChillerReport( EIRChillerNum ).EvapInletTemp, "System", "Average", ElectricEIRChiller( EIRChillerNum ).Name );
			SetupOutputVariable( "Chiller Evaporator Outlet Temperature [C]", ElectricEIRChillerReport( EIRChillerNum ).EvapOutletTemp, "System", "Average", ElectricEIRChiller( EIRChillerNum ).Name );
			SetupOutputVariable( "Chiller Evaporator Mass Flow Rate [kg/s]", ElectricEIRChillerReport( EIRChillerNum ).Evapmdot, "System", "Average", ElectricEIRChiller( EIRChillerNum ).Name );

			SetupOutputVariable( "Chiller Condenser Heat Transfer Rate [W]", ElectricEIRChillerReport( EIRChillerNum ).QCond, "System", "Average", ElectricEIRChiller( EIRChillerNum ).Name );
			SetupOutputVariable( "Chiller Condenser Heat Transfer Energy [J]", ElectricEIRChillerReport( EIRChillerNum ).CondEnergy, "System", "Sum", ElectricEIRChiller( EIRChillerNum ).Name, _, "ENERGYTRANSFER", "HEATREJECTION", _, "Plant" );
			SetupOutputVariable( "Chiller COP [W/W]", ElectricEIRChillerReport( EIRChillerNum ).ActualCOP, "System", "Average", ElectricEIRChiller( EIRChillerNum ).Name );

			SetupOutputVariable( "Chiller Capacity Temperature Modifier Multiplier []", ElectricEIRChillerReport( EIRChillerNum ).ChillerCapFT, "System", "Average", ElectricEIRChiller( EIRChillerNum ).Name );
			SetupOutputVariable( "Chiller EIR Temperature Modifier Multiplier []", ElectricEIRChillerReport( EIRChillerNum ).ChillerEIRFT, "System", "Average", ElectricEIRChiller( EIRChillerNum ).Name );
			SetupOutputVariable( "Chiller EIR Part Load Modifier Multiplier []", ElectricEIRChillerReport( EIRChillerNum ).ChillerEIRFPLR, "System", "Average", ElectricEIRChiller( EIRChillerNum ).Name );

			// Condenser mass flow and outlet temp are valid for water cooled
			if ( ElectricEIRChiller( EIRChillerNum ).CondenserType == WaterCooled ) {
				SetupOutputVariable( "Chiller Condenser Inlet Temperature [C]", ElectricEIRChillerReport( EIRChillerNum ).CondInletTemp, "System", "Average", ElectricEIRChiller( EIRChillerNum ).Name );
				SetupOutputVariable( "Chiller Condenser Outlet Temperature [C]", ElectricEIRChillerReport( EIRChillerNum ).CondOutletTemp, "System", "Average", ElectricEIRChiller( EIRChillerNum ).Name );
				SetupOutputVariable( "Chiller Condenser Mass Flow Rate [kg/s]", ElectricEIRChillerReport( EIRChillerNum ).Condmdot, "System", "Average", ElectricEIRChiller( EIRChillerNum ).Name );

				// If heat recovery is active then setup report variables
				if ( ElectricEIRChiller( EIRChillerNum ).HeatRecActive ) {
					SetupOutputVariable( "Chiller Total Recovered Heat Rate [W]", ElectricEIRChillerReport( EIRChillerNum ).QHeatRecovery, "System", "Average", ElectricEIRChiller( EIRChillerNum ).Name );
					SetupOutputVariable( "Chiller Total Recovered Heat Energy [J]", ElectricEIRChillerReport( EIRChillerNum ).EnergyHeatRecovery, "System", "Sum", ElectricEIRChiller( EIRChillerNum ).Name, _, "ENERGYTRANSFER", "HEATRECOVERY", _, "Plant" );
					SetupOutputVariable( "Chiller Heat Recovery Inlet Temperature [C]", ElectricEIRChillerReport( EIRChillerNum ).HeatRecInletTemp, "System", "Average", ElectricEIRChiller( EIRChillerNum ).Name );
					SetupOutputVariable( "Chiller Heat Recovery Outlet Temperature [C]", ElectricEIRChillerReport( EIRChillerNum ).HeatRecOutletTemp, "System", "Average", ElectricEIRChiller( EIRChillerNum ).Name );
					SetupOutputVariable( "Chiller Heat Recovery Mass Flow Rate [kg/s]", ElectricEIRChillerReport( EIRChillerNum ).HeatRecMassFlow, "System", "Average", ElectricEIRChiller( EIRChillerNum ).Name );
					SetupOutputVariable( "Chiller Effective Heat Rejection Temperature [C]", ElectricEIRChillerReport( EIRChillerNum ).ChillerCondAvgTemp, "System", "Average", ElectricEIRChiller( EIRChillerNum ).Name );
				}

			} else {
				SetupOutputVariable( "Chiller Condenser Inlet Temperature [C]", ElectricEIRChillerReport( EIRChillerNum ).CondInletTemp, "System", "Average", ElectricEIRChiller( EIRChillerNum ).Name );
				if ( ElectricEIRChiller( EIRChillerNum ).CondenserFanPowerRatio > 0 ) {
					SetupOutputVariable( "Chiller Condenser Fan Electric Power [W]", ElectricEIRChillerReport( EIRChillerNum ).CondenserFanPowerUse, "System", "Average", ElectricEIRChiller( EIRChillerNum ).Name );
					SetupOutputVariable( "Chiller Condenser Fan Electric Energy [J]", ElectricEIRChillerReport( EIRChillerNum ).CondenserFanEnergyConsumption, "System", "Sum", ElectricEIRChiller( EIRChillerNum ).Name, _, "ELECTRICITY", "Cooling", _, "Plant" );
				}
				if ( ElectricEIRChiller( EIRChillerNum ).CondenserType == EvapCooled ) {
					if ( ElectricEIRChiller( EIRChillerNum ).BasinHeaterPowerFTempDiff > 0.0 ) {
						SetupOutputVariable( "Chiller Basin Heater Electric Power [W]", ElectricEIRChillerReport( EIRChillerNum ).BasinHeaterPower, "System", "Average", ElectricEIRChiller( EIRChillerNum ).Name );
						SetupOutputVariable( "Chiller Basin Heater Electric Energy [J]", ElectricEIRChillerReport( EIRChillerNum ).BasinHeaterConsumption, "System", "Sum", ElectricEIRChiller( EIRChillerNum ).Name, _, "Electric", "CHILLERS", _, "Plant" );
					}
				}
			}
			if ( AnyEnergyManagementSystemInModel ) {
				SetupEMSInternalVariable( "Chiller Nominal Capacity", ElectricEIRChiller( EIRChillerNum ).Name, "[W]", ElectricEIRChiller( EIRChillerNum ).RefCap );
			}

		}

	}

	void
	InitElectricEIRChiller(
		int const EIRChillNum, // Number of the current electric EIR chiller being simulated
		bool const RunFlag, // TRUE when chiller operating
		Real64 const MyLoad // current load put on chiller
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Richard Raustad, FSEC
		//       DATE WRITTEN   June 2004
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		//  This subroutine is for initializations of the Electric EIR Chiller variables

		// METHODOLOGY EMPLOYED:
		//  Uses the status flags to trigger initializations.

		// REFERENCES:
		//  na

		// Using/Aliasing
		using DataGlobals::BeginEnvrnFlag;
		using DataGlobals::AnyEnergyManagementSystemInModel;
		using DataPlant::PlantLoop;
		using DataPlant::TypeOf_Chiller_ElectricEIR;
		using DataPlant::ScanPlantLoopsForObject;
		using DataPlant::PlantFirstSizesOkayToFinalize;
		using DataPlant::LoopFlowStatus_NeedyIfLoopOn;
		using DataPlant::SingleSetPoint;
		using DataPlant::DualSetPointDeadBand;
		using PlantUtilities::InterConnectTwoPlantLoopSides;
		using PlantUtilities::InitComponentNodes;
		using PlantUtilities::SetComponentFlowRate;
		using DataEnvironment::StdBaroPress;
		using EMSManager::iTemperatureSetPoint;
		using EMSManager::CheckIfNodeSetPointManagedByEMS;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		//  na

		// DERIVED TYPE DEFINITIONS:
		//  na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		static std::string const RoutineName( "InitElectricEIRChiller" );
		static bool MyOneTimeFlag( true ); // Flag used to execute code only once
		static Array1D_bool MyFlag; // TRUE in order to set component location
		static Array1D_bool MyEnvrnFlag; // TRUE when new environment is started
		int EvapInletNode; // Node number for evaporator water inlet node
		int EvapOutletNode; // Node number for evaporator water outlet node
		int CondInletNode; // Node number for condenser water inlet node
		int CondOutletNode; // Node number for condenser water outlet node
		int HeatRecInNode; // Node number for heat recovery water inlet node
		int HeatRecOutNode; // Node number for heat recovery water outlet node
		Real64 rho; // local fluid density
		Real64 mdot; // local fluid mass flow rate
		Real64 mdotCond; // local fluid mass flow rate for condenser
		Real64 THeatRecSetPoint( 0.0 ); // tests set point node for proper set point value
		int LoopNum;
		int LoopSideNum;
		int BranchIndex;
		int CompIndex;
		bool FatalError;
		bool errFlag;
		// FLOW:

		// Do the one time initializations
		if ( MyOneTimeFlag ) {
			MyEnvrnFlag.allocate( NumElectricEIRChillers );
			MyFlag.allocate( NumElectricEIRChillers );
			MyEnvrnFlag = true;
			MyFlag = true;
			MyOneTimeFlag = false;
		}

		EvapInletNode = ElectricEIRChiller( EIRChillNum ).EvapInletNodeNum;
		EvapOutletNode = ElectricEIRChiller( EIRChillNum ).EvapOutletNodeNum;
		CondInletNode = ElectricEIRChiller( EIRChillNum ).CondInletNodeNum;
		CondOutletNode = ElectricEIRChiller( EIRChillNum ).CondOutletNodeNum;

		if ( ElectricEIRChiller( EIRChillNum ).HeatRecActive ) {
			HeatRecInNode = ElectricEIRChiller( EIRChillNum ).HeatRecInletNodeNum;
			HeatRecOutNode = ElectricEIRChiller( EIRChillNum ).HeatRecOutletNodeNum;
		}

		// Init more variables
		if ( MyFlag( EIRChillNum ) ) {
			// Locate the chillers on the plant loops for later usage
			errFlag = false;
			ScanPlantLoopsForObject( ElectricEIRChiller( EIRChillNum ).Name, TypeOf_Chiller_ElectricEIR, ElectricEIRChiller( EIRChillNum ).CWLoopNum, ElectricEIRChiller( EIRChillNum ).CWLoopSideNum, ElectricEIRChiller( EIRChillNum ).CWBranchNum, ElectricEIRChiller( EIRChillNum ).CWCompNum, ElectricEIRChiller( EIRChillNum ).TempLowLimitEvapOut, _, _, ElectricEIRChiller( EIRChillNum ).EvapInletNodeNum, _, errFlag );
			if ( ElectricEIRChiller( EIRChillNum ).CondenserType != AirCooled && ElectricEIRChiller( EIRChillNum ).CondenserType != EvapCooled ) {
				ScanPlantLoopsForObject( ElectricEIRChiller( EIRChillNum ).Name, TypeOf_Chiller_ElectricEIR, ElectricEIRChiller( EIRChillNum ).CDLoopNum, ElectricEIRChiller( EIRChillNum ).CDLoopSideNum, ElectricEIRChiller( EIRChillNum ).CDBranchNum, ElectricEIRChiller( EIRChillNum ).CDCompNum, _, _, _, ElectricEIRChiller( EIRChillNum ).CondInletNodeNum, _, errFlag );
				InterConnectTwoPlantLoopSides( ElectricEIRChiller( EIRChillNum ).CWLoopNum, ElectricEIRChiller( EIRChillNum ).CWLoopSideNum, ElectricEIRChiller( EIRChillNum ).CDLoopNum, ElectricEIRChiller( EIRChillNum ).CDLoopSideNum, TypeOf_Chiller_ElectricEIR, true );
			}
			if ( ElectricEIRChiller( EIRChillNum ).HeatRecActive ) {
				ScanPlantLoopsForObject( ElectricEIRChiller( EIRChillNum ).Name, TypeOf_Chiller_ElectricEIR, ElectricEIRChiller( EIRChillNum ).HRLoopNum, ElectricEIRChiller( EIRChillNum ).HRLoopSideNum, ElectricEIRChiller( EIRChillNum ).HRBranchNum, ElectricEIRChiller( EIRChillNum ).HRCompNum, _, _, _, ElectricEIRChiller( EIRChillNum ).HeatRecInletNodeNum, _, errFlag );
				InterConnectTwoPlantLoopSides( ElectricEIRChiller( EIRChillNum ).CWLoopNum, ElectricEIRChiller( EIRChillNum ).CWLoopSideNum, ElectricEIRChiller( EIRChillNum ).HRLoopNum, ElectricEIRChiller( EIRChillNum ).HRLoopSideNum, TypeOf_Chiller_ElectricEIR, true );
			}

			if ( ElectricEIRChiller( EIRChillNum ).CondenserType != AirCooled && ElectricEIRChiller( EIRChillNum ).CondenserType != EvapCooled && ElectricEIRChiller( EIRChillNum ).HeatRecActive ) {
				InterConnectTwoPlantLoopSides( ElectricEIRChiller( EIRChillNum ).CDLoopNum, ElectricEIRChiller( EIRChillNum ).CDLoopSideNum, ElectricEIRChiller( EIRChillNum ).HRLoopNum, ElectricEIRChiller( EIRChillNum ).HRLoopSideNum, TypeOf_Chiller_ElectricEIR, false );
			}

			if ( errFlag ) {
				ShowFatalError( "InitElectricEIRChiller: Program terminated due to previous condition(s)." );
			}

			if ( ElectricEIRChiller( EIRChillNum ).FlowMode == ConstantFlow ) {
				// reset flow priority
				PlantLoop( ElectricEIRChiller( EIRChillNum ).CWLoopNum ).LoopSide( ElectricEIRChiller( EIRChillNum ).CWLoopSideNum ).Branch( ElectricEIRChiller( EIRChillNum ).CWBranchNum ).Comp( ElectricEIRChiller( EIRChillNum ).CWCompNum ).FlowPriority = LoopFlowStatus_NeedyIfLoopOn;
			}

			if ( ElectricEIRChiller( EIRChillNum ).FlowMode == LeavingSetPointModulated ) {
				// reset flow priority
				PlantLoop( ElectricEIRChiller( EIRChillNum ).CWLoopNum ).LoopSide( ElectricEIRChiller( EIRChillNum ).CWLoopSideNum ).Branch( ElectricEIRChiller( EIRChillNum ).CWBranchNum ).Comp( ElectricEIRChiller( EIRChillNum ).CWCompNum ).FlowPriority = LoopFlowStatus_NeedyIfLoopOn;
				// check if setpoint on outlet node
				if ( ( Node( ElectricEIRChiller( EIRChillNum ).EvapOutletNodeNum ).TempSetPoint == SensedNodeFlagValue ) && ( Node( ElectricEIRChiller( EIRChillNum ).EvapOutletNodeNum ).TempSetPointHi == SensedNodeFlagValue ) ) {
					if ( ! AnyEnergyManagementSystemInModel ) {
						if ( ! ElectricEIRChiller( EIRChillNum ).ModulatedFlowErrDone ) {
							ShowWarningError( "Missing temperature setpoint for LeavingSetpointModulated mode chiller named " + ElectricEIRChiller( EIRChillNum ).Name );
							ShowContinueError( "  A temperature setpoint is needed at the outlet node of a chiller in variable flow mode, use a SetpointManager" );
							ShowContinueError( "  The overall loop setpoint will be assumed for chiller. The simulation continues ... " );
							ElectricEIRChiller( EIRChillNum ).ModulatedFlowErrDone = true;
						}
					} else {
						// need call to EMS to check node
						FatalError = false; // but not really fatal yet, but should be.
						CheckIfNodeSetPointManagedByEMS( ElectricEIRChiller( EIRChillNum ).EvapOutletNodeNum, iTemperatureSetPoint, FatalError );
						if ( FatalError ) {
							if ( ! ElectricEIRChiller( EIRChillNum ).ModulatedFlowErrDone ) {
								ShowWarningError( "Missing temperature setpoint for LeavingSetpointModulated mode chiller named " + ElectricEIRChiller( EIRChillNum ).Name );
								ShowContinueError( "  A temperature setpoint is needed at the outlet node of a chiller evaporator in variable flow mode" );
								ShowContinueError( "  use a Setpoint Manager to establish a setpoint at the chiller evaporator outlet node " );
								ShowContinueError( "  or use an EMS actuator to establish a setpoint at the outlet node " );
								ShowContinueError( "  The overall loop setpoint will be assumed for chiller. The simulation continues ... " );
								ElectricEIRChiller( EIRChillNum ).ModulatedFlowErrDone = true;
							}
						}

					}
					ElectricEIRChiller( EIRChillNum ).ModulatedFlowSetToLoop = true;
					Node( ElectricEIRChiller( EIRChillNum ).EvapOutletNodeNum ).TempSetPoint = Node( PlantLoop( ElectricEIRChiller( EIRChillNum ).CWLoopNum ).TempSetPointNodeNum ).TempSetPoint;
					Node( ElectricEIRChiller( EIRChillNum ).EvapOutletNodeNum ).TempSetPointHi = Node( PlantLoop( ElectricEIRChiller( EIRChillNum ).CWLoopNum ).TempSetPointNodeNum ).TempSetPointHi;
				}
			}
			MyFlag( EIRChillNum ) = false;
		}

		if ( MyEnvrnFlag( EIRChillNum ) && BeginEnvrnFlag && ( PlantFirstSizesOkayToFinalize ) ) {

			rho = GetDensityGlycol( PlantLoop( ElectricEIRChiller( EIRChillNum ).CWLoopNum ).FluidName, InitConvTemp, PlantLoop( ElectricEIRChiller( EIRChillNum ).CWLoopNum ).FluidIndex, RoutineName );

			ElectricEIRChiller( EIRChillNum ).EvapMassFlowRateMax = ElectricEIRChiller( EIRChillNum ).EvapVolFlowRate * rho;

			InitComponentNodes( 0.0, ElectricEIRChiller( EIRChillNum ).EvapMassFlowRateMax, EvapInletNode, EvapOutletNode, ElectricEIRChiller( EIRChillNum ).CWLoopNum, ElectricEIRChiller( EIRChillNum ).CWLoopSideNum, ElectricEIRChiller( EIRChillNum ).CWBranchNum, ElectricEIRChiller( EIRChillNum ).CWCompNum );

			if ( ElectricEIRChiller( EIRChillNum ).CondenserType == WaterCooled ) {

				rho = GetDensityGlycol( PlantLoop( ElectricEIRChiller( EIRChillNum ).CDLoopNum ).FluidName, ElectricEIRChiller( EIRChillNum ).TempRefCondIn, PlantLoop( ElectricEIRChiller( EIRChillNum ).CDLoopNum ).FluidIndex, RoutineName );
				ElectricEIRChiller( EIRChillNum ).CondMassFlowRateMax = rho * ElectricEIRChiller( EIRChillNum ).CondVolFlowRate;
				InitComponentNodes( 0.0, ElectricEIRChiller( EIRChillNum ).CondMassFlowRateMax, CondInletNode, CondOutletNode, ElectricEIRChiller( EIRChillNum ).CDLoopNum, ElectricEIRChiller( EIRChillNum ).CDLoopSideNum, ElectricEIRChiller( EIRChillNum ).CDBranchNum, ElectricEIRChiller( EIRChillNum ).CDCompNum );
				Node( CondInletNode ).Temp = ElectricEIRChiller( EIRChillNum ).TempRefCondIn;
			} else { // air or evap air condenser
				// Initialize maximum available condenser flow rate
				rho = PsyRhoAirFnPbTdbW( StdBaroPress, ElectricEIRChiller( EIRChillNum ).TempRefCondIn, 0.0, RoutineName );
				ElectricEIRChiller( EIRChillNum ).CondMassFlowRateMax = rho * ElectricEIRChiller( EIRChillNum ).CondVolFlowRate;

				Node( CondInletNode ).MassFlowRate = ElectricEIRChiller( EIRChillNum ).CondMassFlowRateMax;
				Node( CondOutletNode ).MassFlowRate = Node( CondInletNode ).MassFlowRate;
				Node( CondInletNode ).MassFlowRateMaxAvail = Node( CondInletNode ).MassFlowRate;
				Node( CondInletNode ).MassFlowRateMax = Node( CondInletNode ).MassFlowRate;
				Node( CondOutletNode ).MassFlowRateMax = Node( CondInletNode ).MassFlowRate;
				Node( CondInletNode ).MassFlowRateMinAvail = 0.0;
				Node( CondInletNode ).MassFlowRateMin = 0.0;
				Node( CondOutletNode ).MassFlowRateMinAvail = 0.0;
				Node( CondOutletNode ).MassFlowRateMin = 0.0;
				Node( CondInletNode ).Temp = ElectricEIRChiller( EIRChillNum ).TempRefCondIn;

			}

			if ( ElectricEIRChiller( EIRChillNum ).HeatRecActive ) {
				rho = GetDensityGlycol( PlantLoop( ElectricEIRChiller( EIRChillNum ).HRLoopNum ).FluidName, InitConvTemp, PlantLoop( ElectricEIRChiller( EIRChillNum ).HRLoopNum ).FluidIndex, RoutineName );
				ElectricEIRChiller( EIRChillNum ).DesignHeatRecMassFlowRate = rho * ElectricEIRChiller( EIRChillNum ).DesignHeatRecVolFlowRate;

				InitComponentNodes( 0.0, ElectricEIRChiller( EIRChillNum ).DesignHeatRecMassFlowRate, ElectricEIRChiller( EIRChillNum ).HeatRecInletNodeNum, ElectricEIRChiller( EIRChillNum ).HeatRecOutletNodeNum, ElectricEIRChiller( EIRChillNum ).HRLoopNum, ElectricEIRChiller( EIRChillNum ).HRLoopSideNum, ElectricEIRChiller( EIRChillNum ).HRBranchNum, ElectricEIRChiller( EIRChillNum ).HRCompNum );
				// overall capacity limit
				ElectricEIRChiller( EIRChillNum ).HeatRecMaxCapacityLimit = ElectricEIRChiller( EIRChillNum ).HeatRecCapacityFraction * ( ElectricEIRChiller( EIRChillNum ).RefCap + ElectricEIRChiller( EIRChillNum ).RefCap / ElectricEIRChiller( EIRChillNum ).RefCOP );

				if ( ElectricEIRChiller( EIRChillNum ).HeatRecSetPointNodeNum > 0 ) {
					{ auto const SELECT_CASE_var( PlantLoop( ElectricEIRChiller( EIRChillNum ).HRLoopNum ).LoopDemandCalcScheme );
					if ( SELECT_CASE_var == SingleSetPoint ) {
						THeatRecSetPoint = Node( ElectricEIRChiller( EIRChillNum ).HeatRecSetPointNodeNum ).TempSetPoint;
					} else if ( SELECT_CASE_var == DualSetPointDeadBand ) {
						THeatRecSetPoint = Node( ElectricEIRChiller( EIRChillNum ).HeatRecSetPointNodeNum ).TempSetPointHi;
					} else {
						assert( false );
					}}
					if ( THeatRecSetPoint == SensedNodeFlagValue ) {
						if ( ! AnyEnergyManagementSystemInModel ) {
							if ( ! ElectricEIRChiller( EIRChillNum ).HRSPErrDone ) {
								ShowWarningError( "Missing heat recovery temperature setpoint for chiller named " + ElectricEIRChiller( EIRChillNum ).Name );
								ShowContinueError( "  A temperature setpoint is needed at the heat recovery leaving temperature setpoint node specified, use a SetpointManager" );
								ShowContinueError( "  The overall loop setpoint will be assumed for heat recovery. The simulation continues ..." );
								ElectricEIRChiller( EIRChillNum ).HeatRecSetPointNodeNum = PlantLoop( ElectricEIRChiller( EIRChillNum ).HRLoopNum ).TempSetPointNodeNum;
								ElectricEIRChiller( EIRChillNum ).HRSPErrDone = true;
							}
						} else {
							// need call to EMS to check node
							FatalError = false; // but not really fatal yet, but should be.
							CheckIfNodeSetPointManagedByEMS( ElectricEIRChiller( EIRChillNum ).EvapOutletNodeNum, iTemperatureSetPoint, FatalError );
							if ( FatalError ) {
								if ( ! ElectricEIRChiller( EIRChillNum ).HRSPErrDone ) {
									ShowWarningError( "Missing heat recovery temperature setpoint for chiller named " + ElectricEIRChiller( EIRChillNum ).Name );
									ShowContinueError( "  A temperature setpoint is needed at the heat recovery leaving temperature setpoint node specified, use a SetpointManager to establish a setpoint" );
									ShowContinueError( "  or use an EMS actuator to establish a setpoint at this node " );
									ShowContinueError( "  The overall loop setpoint will be assumed for heat recovery. The simulation continues ..." );
									ElectricEIRChiller( EIRChillNum ).HeatRecSetPointNodeNum = PlantLoop( ElectricEIRChiller( EIRChillNum ).HRLoopNum ).TempSetPointNodeNum;
									ElectricEIRChiller( EIRChillNum ).HRSPErrDone = true;
								}
							}
						} // IF (.NOT. AnyEnergyManagementSystemInModel) THEN
					} // IF(THeatRecSetPoint == SensedNodeFlagValue)THEN
				} // IF(ElectricEIRChiller(EIRChillNum)%HeatRecSetPointNodeNum > 0)THEN
			} // IF (ElectricEIRChiller(EIRChillNum)%HeatRecActive) THEN

			MyEnvrnFlag( EIRChillNum ) = false;
		}
		if ( ! BeginEnvrnFlag ) {
			MyEnvrnFlag( EIRChillNum ) = true;
		}

		if ( ( ElectricEIRChiller( EIRChillNum ).FlowMode == LeavingSetPointModulated ) && ElectricEIRChiller( EIRChillNum ).ModulatedFlowSetToLoop ) {
			// fix for clumsy old input that worked because loop setpoint was spread.
			//  could be removed with transition, testing , model change, period of being obsolete.
			Node( ElectricEIRChiller( EIRChillNum ).EvapOutletNodeNum ).TempSetPoint = Node( PlantLoop( ElectricEIRChiller( EIRChillNum ).CWLoopNum ).TempSetPointNodeNum ).TempSetPoint;
			Node( ElectricEIRChiller( EIRChillNum ).EvapOutletNodeNum ).TempSetPointHi = Node( PlantLoop( ElectricEIRChiller( EIRChillNum ).CWLoopNum ).TempSetPointNodeNum ).TempSetPointHi;
		}

		if ( ( std::abs( MyLoad ) > 0.0 ) && RunFlag ) {
			mdot = ElectricEIRChiller( EIRChillNum ).EvapMassFlowRateMax;
			mdotCond = ElectricEIRChiller( EIRChillNum ).CondMassFlowRateMax;
		} else {
			mdot = 0.0;
			mdotCond = 0.0;
		}

		SetComponentFlowRate( mdot, EvapInletNode, EvapOutletNode, ElectricEIRChiller( EIRChillNum ).CWLoopNum, ElectricEIRChiller( EIRChillNum ).CWLoopSideNum, ElectricEIRChiller( EIRChillNum ).CWBranchNum, ElectricEIRChiller( EIRChillNum ).CWCompNum );

		if ( ElectricEIRChiller( EIRChillNum ).CondenserType == WaterCooled ) {
			SetComponentFlowRate( mdotCond, CondInletNode, CondOutletNode, ElectricEIRChiller( EIRChillNum ).CDLoopNum, ElectricEIRChiller( EIRChillNum ).CDLoopSideNum, ElectricEIRChiller( EIRChillNum ).CDBranchNum, ElectricEIRChiller( EIRChillNum ).CDCompNum );
		}
		// Initialize heat recovery flow rates at node
		if ( ElectricEIRChiller( EIRChillNum ).HeatRecActive ) {
			LoopNum = ElectricEIRChiller( EIRChillNum ).HRLoopNum;
			LoopSideNum = ElectricEIRChiller( EIRChillNum ).HRLoopSideNum;
			BranchIndex = ElectricEIRChiller( EIRChillNum ).HRBranchNum;
			CompIndex = ElectricEIRChiller( EIRChillNum ).HRCompNum;
			if ( RunFlag ) {
				mdot = ElectricEIRChiller( EIRChillNum ).DesignHeatRecMassFlowRate;
			} else {
				mdot = 0.0;
			}

			SetComponentFlowRate( mdot, HeatRecInNode, HeatRecOutNode, LoopNum, LoopSideNum, BranchIndex, CompIndex );

		}

		if ( ElectricEIRChiller( EIRChillNum ).CondenserType == EvapCooled ) {
			BasinHeaterPower = 0.0;
		}

	}

	void
	SizeElectricEIRChiller( int const EIRChillNum )
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Richard Raustad, FSEC
		//       DATE WRITTEN   June 2004
		//       MODIFIED       October 2013 Daeho Kang, add component sizing table entries
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		//  This subroutine is for sizing Electric EIR Chiller Components for which capacities and flow rates
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
		using DataPlant::TypeOf_Chiller_ElectricEIR;
		using PlantUtilities::RegisterPlantCompDesignFlow;
		using ReportSizingManager::ReportSizingOutput;
		using namespace OutputReportPredefined;
		using StandardRatings::CalcChillerIPLV;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "SizeElectricEIRChiller" );

		// INTERFACE BLOCK SPECIFICATIONS:
		//  na

		// DERIVED TYPE DEFINITIONS:
		//  na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		//unused1208  INTEGER             :: PltSizIndex   ! Plant Sizing Do loop index
		int PltSizNum; // Plant Sizing index corresponding to CurLoopNum
		int PltSizCondNum; // Plant Sizing index for condenser loop
		bool ErrorsFound; // If errors detected in input
		std::string equipName;
		Real64 rho;
		Real64 Cp;
		Real64 tmpNomCap; // local nominal capacity cooling power
		Real64 tmpEvapVolFlowRate; // local evaporator design volume flow rate
		Real64 tmpCondVolFlowRate; // local condenser design volume flow rate
		static bool MyOneTimeFlag( true );
		static Array1D_bool MyFlag; // TRUE in order to calculate IPLV
		Real64 EvapVolFlowRateUser; // Hardsized evaporator flow for reporting
		Real64 RefCapUser; // Hardsized reference capacity for reporting
		Real64 CondVolFlowRateUser; // Hardsized condenser flow for reporting

		if ( MyOneTimeFlag ) {
			MyFlag.dimension( NumElectricEIRChillers, true );
			MyOneTimeFlag = false;
		}

		PltSizNum = 0;
		PltSizCondNum = 0;
		ErrorsFound = false;
		tmpNomCap = ElectricEIRChiller( EIRChillNum ).RefCap;
		tmpEvapVolFlowRate = ElectricEIRChiller( EIRChillNum ).EvapVolFlowRate;
		tmpCondVolFlowRate = ElectricEIRChiller( EIRChillNum ).CondVolFlowRate;
		EvapVolFlowRateUser = 0.0;
		RefCapUser = 0.0;
		CondVolFlowRateUser = 0.0;

		if ( ElectricEIRChiller( EIRChillNum ).CondenserType == WaterCooled ) {
			PltSizCondNum = PlantLoop( ElectricEIRChiller( EIRChillNum ).CDLoopNum ).PlantSizNum;
		}

		// find the appropriate Plant Sizing object
		PltSizNum = PlantLoop( ElectricEIRChiller( EIRChillNum ).CWLoopNum ).PlantSizNum;

		if ( PltSizNum > 0 ) {
			if ( PlantSizData( PltSizNum ).DesVolFlowRate >= SmallWaterVolFlow ) {
				tmpEvapVolFlowRate = PlantSizData( PltSizNum ).DesVolFlowRate * ElectricEIRChiller( EIRChillNum ).SizFac;
				if ( ! ElectricEIRChiller( EIRChillNum ).EvapVolFlowRateWasAutoSized ) tmpEvapVolFlowRate = ElectricEIRChiller( EIRChillNum ).EvapVolFlowRate;

			} else {
				if ( ElectricEIRChiller( EIRChillNum ).EvapVolFlowRateWasAutoSized ) tmpEvapVolFlowRate = 0.0;

			}
			if ( PlantFirstSizesOkayToFinalize ) {
				if ( ElectricEIRChiller( EIRChillNum ).EvapVolFlowRateWasAutoSized ) {
					ElectricEIRChiller( EIRChillNum ).EvapVolFlowRate = tmpEvapVolFlowRate;
					if ( PlantFinalSizesOkayToReport ) {
						ReportSizingOutput( "Chiller:Electric:EIR", ElectricEIRChiller( EIRChillNum ).Name, "Design Size Reference Chilled Water Flow Rate [m3/s]", tmpEvapVolFlowRate );
					}
					if ( PlantFirstSizesOkayToReport ) {
						ReportSizingOutput( "Chiller:Electric:EIR", ElectricEIRChiller( EIRChillNum ).Name, "Initial Design Size Reference Chilled Water Flow Rate [m3/s]", tmpEvapVolFlowRate );
					}
				} else { // Hard-size with sizing data
					if ( ElectricEIRChiller( EIRChillNum ).EvapVolFlowRate > 0.0 && tmpEvapVolFlowRate > 0.0 ) {
						EvapVolFlowRateUser = ElectricEIRChiller( EIRChillNum ).EvapVolFlowRate;
						if ( PlantFinalSizesOkayToReport ) {
							ReportSizingOutput( "Chiller:Electric:EIR", ElectricEIRChiller( EIRChillNum ).Name,
								"Design Size Reference Chilled Water Flow Rate [m3/s]", tmpEvapVolFlowRate,
								"User-Specified Reference Chilled Water Flow Rate [m3/s]", EvapVolFlowRateUser );
							if ( DisplayExtraWarnings ) {
								if ( ( std::abs( tmpEvapVolFlowRate - EvapVolFlowRateUser ) / EvapVolFlowRateUser ) > AutoVsHardSizingThreshold ) {
									ShowMessage( "SizeChillerElectricEIR: Potential issue with equipment sizing for " + ElectricEIRChiller( EIRChillNum ).Name );
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
			if ( ElectricEIRChiller( EIRChillNum ).EvapVolFlowRateWasAutoSized && PlantFirstSizesOkayToFinalize ) {
				ShowSevereError( "Autosizing of Electric Chiller evap flow rate requires a loop Sizing:Plant object" );
				ShowContinueError( "Occurs in Electric Chiller object=" + ElectricEIRChiller( EIRChillNum ).Name );
				ErrorsFound = true;
			}
			if ( ! ElectricEIRChiller( EIRChillNum ).EvapVolFlowRateWasAutoSized && PlantFinalSizesOkayToReport
					&& ( ElectricEIRChiller( EIRChillNum ).EvapVolFlowRate > 0.0 ) ) {
					ReportSizingOutput( "Chiller:Electric:EIR", ElectricEIRChiller( EIRChillNum ).Name,
						"User-Specified Reference Chilled Water Flow Rate [m3/s]", ElectricEIRChiller( EIRChillNum ).EvapVolFlowRate );
			}
		}

		RegisterPlantCompDesignFlow( ElectricEIRChiller( EIRChillNum ).EvapInletNodeNum, tmpEvapVolFlowRate );

		if ( PltSizNum > 0 ) {
			if ( PlantSizData( PltSizNum ).DesVolFlowRate >= SmallWaterVolFlow ) {
				Cp = GetSpecificHeatGlycol( PlantLoop( ElectricEIRChiller( EIRChillNum ).CWLoopNum ).FluidName, InitConvTemp, PlantLoop( ElectricEIRChiller( EIRChillNum ).CWLoopNum ).FluidIndex, RoutineName );

				rho = GetDensityGlycol( PlantLoop( ElectricEIRChiller( EIRChillNum ).CWLoopNum ).FluidName, InitConvTemp, PlantLoop( ElectricEIRChiller( EIRChillNum ).CWLoopNum ).FluidIndex, RoutineName );
				tmpNomCap = Cp * rho * PlantSizData( PltSizNum ).DeltaT * tmpEvapVolFlowRate;
				if ( ! ElectricEIRChiller( EIRChillNum ).RefCapWasAutoSized ) tmpNomCap = ElectricEIRChiller( EIRChillNum ).RefCap;
			} else {
				tmpNomCap = 0.0;
			}
			if ( PlantFirstSizesOkayToFinalize ) {
				if ( ElectricEIRChiller( EIRChillNum ).RefCapWasAutoSized ) {
					ElectricEIRChiller( EIRChillNum ).RefCap = tmpNomCap;
					if ( PlantFinalSizesOkayToReport ) {
						ReportSizingOutput( "Chiller:Electric:EIR", ElectricEIRChiller( EIRChillNum ).Name,
							"Design Size Reference Capacity [W]", tmpNomCap );
					}
					if ( PlantFirstSizesOkayToReport ) {
						ReportSizingOutput( "Chiller:Electric:EIR", ElectricEIRChiller( EIRChillNum ).Name,
							"Initial Design Size Reference Capacity [W]", tmpNomCap );
					}
				} else { // Hard-sized with sizing data
					if ( ElectricEIRChiller( EIRChillNum ).RefCap > 0.0 && tmpNomCap > 0.0 ) {
						RefCapUser = ElectricEIRChiller( EIRChillNum ).RefCap;
						if ( PlantFinalSizesOkayToReport ) {
							ReportSizingOutput( "Chiller:Electric:EIR", ElectricEIRChiller( EIRChillNum ).Name,
								"Design Size Reference Capacity [W]", tmpNomCap, "User-Specified Reference Capacity [W]", RefCapUser );
							if ( DisplayExtraWarnings ) {
								if ( ( std::abs( tmpNomCap - RefCapUser ) / RefCapUser ) > AutoVsHardSizingThreshold ) {
									ShowMessage( "SizeChillerElectricEIR: Potential issue with equipment sizing for " + ElectricEIRChiller( EIRChillNum ).Name );
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
			if ( ElectricEIRChiller( EIRChillNum ).RefCapWasAutoSized && PlantFirstSizesOkayToFinalize ) {
				ShowSevereError( "Autosizing of Electric Chiller reference capacity requires a loop Sizing:Plant object" );
				ShowContinueError( "Occurs in Electric Chiller object=" + ElectricEIRChiller( EIRChillNum ).Name );
				ErrorsFound = true;
			}
			if ( ! ElectricEIRChiller( EIRChillNum ).RefCapWasAutoSized && PlantFinalSizesOkayToReport
					&& ( ElectricEIRChiller( EIRChillNum ).RefCap > 0.0 )) { // Hard-sized with no sizing data
						ReportSizingOutput( "Chiller:Electric:EIR", ElectricEIRChiller( EIRChillNum ).Name,
							"User-Specified Reference Capacity [W]", ElectricEIRChiller( EIRChillNum ).RefCap );

			}
		}

		if ( PltSizCondNum > 0 && PltSizNum > 0 ) {
			if ( PlantSizData( PltSizNum ).DesVolFlowRate >= SmallWaterVolFlow && tmpNomCap > 0.0 ) {

				rho = GetDensityGlycol( PlantLoop( ElectricEIRChiller( EIRChillNum ).CDLoopNum ).FluidName, InitConvTemp, PlantLoop( ElectricEIRChiller( EIRChillNum ).CDLoopNum ).FluidIndex, RoutineName );

				Cp = GetSpecificHeatGlycol( PlantLoop( ElectricEIRChiller( EIRChillNum ).CDLoopNum ).FluidName, ElectricEIRChiller( EIRChillNum ).TempRefCondIn, PlantLoop( ElectricEIRChiller( EIRChillNum ).CDLoopNum ).FluidIndex, RoutineName );
				tmpCondVolFlowRate = tmpNomCap * ( 1.0 + ( 1.0 / ElectricEIRChiller( EIRChillNum ).RefCOP ) * ElectricEIRChiller( EIRChillNum ).CompPowerToCondenserFrac ) / ( PlantSizData( PltSizCondNum ).DeltaT * Cp * rho );
				if ( ! ElectricEIRChiller( EIRChillNum ).CondVolFlowRateWasAutoSized ) tmpCondVolFlowRate = ElectricEIRChiller( EIRChillNum ).CondVolFlowRate;

			} else {
				if ( ElectricEIRChiller( EIRChillNum ).CondVolFlowRateWasAutoSized ) tmpCondVolFlowRate = 0.0;

			}
			if ( PlantFirstSizesOkayToFinalize ) {
				if ( ElectricEIRChiller( EIRChillNum ).CondVolFlowRateWasAutoSized ) {
					ElectricEIRChiller( EIRChillNum ).CondVolFlowRate = tmpCondVolFlowRate;
					if ( PlantFinalSizesOkayToReport ) {
						ReportSizingOutput( "Chiller:Electric:EIR", ElectricEIRChiller( EIRChillNum ).Name,
							"Design Size Reference Condenser Water Flow Rate [m3/s]", tmpCondVolFlowRate );
					}
					if ( PlantFirstSizesOkayToReport ) {
						ReportSizingOutput( "Chiller:Electric:EIR", ElectricEIRChiller( EIRChillNum ).Name,
							"Initial Design Size Reference Condenser Water Flow Rate [m3/s]", tmpCondVolFlowRate );
					}
				} else {
					if ( ElectricEIRChiller( EIRChillNum ).CondVolFlowRate > 0.0 && tmpCondVolFlowRate > 0.0 ) {
						CondVolFlowRateUser = ElectricEIRChiller( EIRChillNum ).CondVolFlowRate;
						if ( PlantFinalSizesOkayToReport ) {
							ReportSizingOutput( "Chiller:Electric:EIR", ElectricEIRChiller( EIRChillNum ).Name,
								"Design Size Reference Condenser Water Flow Rate [m3/s]", tmpCondVolFlowRate,
								"User-Specified Reference Condenser Water Flow Rate [m3/s]", CondVolFlowRateUser );
							if ( DisplayExtraWarnings ) {
								if ( ( std::abs( tmpCondVolFlowRate - CondVolFlowRateUser ) / CondVolFlowRateUser ) > AutoVsHardSizingThreshold ) {
									ShowMessage( "SizeChillerElectricEIR: Potential issue with equipment sizing for " + ElectricEIRChiller( EIRChillNum ).Name );
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
			if ( ElectricEIRChiller( EIRChillNum ).CondVolFlowRateWasAutoSized && PlantFirstSizesOkayToFinalize ) {
				ShowSevereError( "Autosizing of Electric EIR Chiller condenser flow rate requires a condenser" );
				ShowContinueError( "loop Sizing:Plant object" );
				ShowContinueError( "Occurs in Electric EIR Chiller object=" + ElectricEIRChiller( EIRChillNum ).Name );
				ErrorsFound = true;
			}
			if ( ! ElectricEIRChiller( EIRChillNum ).CondVolFlowRateWasAutoSized && PlantFinalSizesOkayToReport
					&& ( ElectricEIRChiller( EIRChillNum ).CondVolFlowRate > 0.0 )) {
					ReportSizingOutput( "Chiller:Electric:EIR", ElectricEIRChiller( EIRChillNum ).Name,
						"User-Specified Reference Condenser Water Flow Rate [m3/s]", ElectricEIRChiller( EIRChillNum ).CondVolFlowRate );
			}
		}

		// save the reference condenser water volumetric flow rate for use by the condenser water loop sizing algorithms
		RegisterPlantCompDesignFlow( ElectricEIRChiller( EIRChillNum ).CondInletNodeNum, tmpCondVolFlowRate );

		// where is the heat recovery sizing? should be here but it is missing???

		if ( PlantFinalSizesOkayToReport ) {
			if ( MyFlag( EIRChillNum ) ) {
				CalcChillerIPLV( ElectricEIRChiller( EIRChillNum ).Name, TypeOf_Chiller_ElectricEIR, ElectricEIRChiller( EIRChillNum ).RefCap, ElectricEIRChiller( EIRChillNum ).RefCOP, ElectricEIRChiller( EIRChillNum ).CondenserType, ElectricEIRChiller( EIRChillNum ).ChillerCapFT, ElectricEIRChiller( EIRChillNum ).ChillerEIRFT, ElectricEIRChiller( EIRChillNum ).ChillerEIRFPLR, ElectricEIRChiller( EIRChillNum ).MinUnloadRat );
				MyFlag( EIRChillNum ) = false;
			}
			//create predefined report
			equipName = ElectricEIRChiller( EIRChillNum ).Name;
			PreDefTableEntry( pdchMechType, equipName, "Chiller:Electric:EIR" );
			PreDefTableEntry( pdchMechNomEff, equipName, ElectricEIRChiller( EIRChillNum ).RefCOP );
			PreDefTableEntry( pdchMechNomCap, equipName, ElectricEIRChiller( EIRChillNum ).RefCap );
		}

		if ( ErrorsFound ) {
			ShowFatalError( "Preceding sizing errors cause program termination" );
		}

	}

	void
	CalcElectricEIRChillerModel(
		int & EIRChillNum, // Chiller number
		Real64 & MyLoad, // Operating load
		bool const RunFlag, // TRUE when chiller operating
		bool const EP_UNUSED( FirstIteration ), // TRUE when first iteration of timestep
		int const EquipFlowCtrl // Flow control mode for the equipment
	)
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         Richard Raustad, FSEC
		//       DATE WRITTEN   July 2004
		//       MODIFIED       Chandan Sharma, FSEC, February 2010, Added basin heater
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		//  Simulate a vapor compression chiller using the DOE-2 model

		// METHODOLOGY EMPLOYED:
		//  Use empirical curve fits to model performance at off-reference conditions

		// REFERENCES:
		// 1. DOE-2 Engineers Manual, Version 2.1A, November 1982, LBL-11353

		// Using/Aliasing
		using DataGlobals::WarmupFlag;
		using DataGlobals::CurrentTime;
		using DataHVACGlobals::SmallLoad;
		using DataHVACGlobals::SysTimeElapsed;
		using DataHVACGlobals::TimeStepSys;
		using General::RoundSigDigits;
		using General::CreateSysTimeIntervalString;
		using CurveManager::CurveValue;
		using DataPlant::DeltaTempTol;
		using DataPlant::PlantLoop;
		using DataPlant::SimPlantEquipTypes;
		using DataPlant::TypeOf_Chiller_ElectricEIR;
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
		using Psychrometrics::PsyCpAirFnWTdb;
		using Psychrometrics::PsyWFnTdbTwbPb;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:

		static gio::Fmt OutputFormat( "(F6.2)" );
		static std::string const RoutineName( "CalcElectricEIRChillerModel" );

		// INTERFACE BLOCK SPECIFICATIONS
		//  na

		// DERIVED TYPE DEFINITIONS
		//  na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 FRAC; // Chiller cycling ratio
		Real64 MinPartLoadRat; // Min allowed operating fraction of full load
		Real64 MinUnloadRat; // Min allowed unloading fraction of full load
		Real64 MaxPartLoadRat; // Max allowed operating fraction of full load
		Real64 EvapInletTemp; // Evaporator inlet temperature [C]
		Real64 CondInletTemp; // Condenser inlet temperature [C]
		Real64 EvapOutletTempSetPoint( 0.0 ); // Evaporator outlet temperature setpoint [C]
		Real64 AvailChillerCap; // Chiller available capacity at current operating conditions [W]
		Real64 ChillerRefCap; // Chiller reference capacity
		Real64 EvapDeltaTemp( 0.0 ); // Evaporator temperature difference [C]
		Real64 ReferenceCOP; // Reference coefficient of performance, from user input
		Real64 PartLoadRat; // Operating part load ratio
		Real64 TempLowLimitEout; // Evaporator low temp. limit cut off [C]
		Real64 EvapMassFlowRateMax; // Max reference evaporator mass flow rate converted from volume flow rate [kg/s]
		int EvapInletNode; // Evaporator inlet node number
		int EvapOutletNode; // Evaporator outlet node number
		int CondInletNode; // Condenser inlet node number
		int CondOutletNode; // Condenser outlet node number
		//  LOGICAL,SAVE           :: PossibleSubcooling
		Real64 TempLoad( 0.0 ); // Actual load to be met by chiller. This value is compared to MyLoad
		// and reset when necessary since this chiller can cycle, the load passed
		// should be the actual load. Instead the minimum PLR * RefCap is
		// passed in. [W]
		int PlantLoopNum; // Plant loop which contains the current chiller
		int LoopSideNum; // Plant loop side which contains the current chiller (usually supply side)
		int BranchNum;
		int CompNum;
		static Real64 TimeStepSysLast( 0.0 ); // last system time step (used to check for downshifting)
		Real64 CurrentEndTime; // end time of time step for current simulation time step
		static Real64 CurrentEndTimeLast( 0.0 ); // end time of time step for last simulation time step
		static std::string OutputChar; // character string for warning messages
		Real64 Cp; // local fluid specific heat

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
		CondenserFanPower = 0.0;
		EvapInletNode = ElectricEIRChiller( EIRChillNum ).EvapInletNodeNum;
		EvapOutletNode = ElectricEIRChiller( EIRChillNum ).EvapOutletNodeNum;
		CondInletNode = ElectricEIRChiller( EIRChillNum ).CondInletNodeNum;
		CondOutletNode = ElectricEIRChiller( EIRChillNum ).CondOutletNodeNum;
		PlantLoopNum = ElectricEIRChiller( EIRChillNum ).CWLoopNum;
		LoopSideNum = ElectricEIRChiller( EIRChillNum ).CWLoopSideNum;
		BranchNum = ElectricEIRChiller( EIRChillNum ).CWBranchNum;
		CompNum = ElectricEIRChiller( EIRChillNum ).CWCompNum;
		EvapInletTemp = Node( EvapInletNode ).Temp;
		FRAC = 1.0;

		// Set performance curve outputs to 0.0 when chiller is off
		ChillerCapFT = 0.0;
		ChillerEIRFT = 0.0;
		ChillerEIRFPLR = 0.0;

		// calculate end time of current time step
		CurrentEndTime = CurrentTime + SysTimeElapsed;

		// Print warning messages only when valid and only for the first ocurrance. Let summary provide statistics.
		// Wait for next time step to print warnings. If simulation iterates, print out
		// the warning for the last iteration only. Must wait for next time step to accomplish this.
		// If a warning occurs and the simulation down shifts, the warning is not valid.
		if ( CurrentEndTime > CurrentEndTimeLast && TimeStepSys >= TimeStepSysLast ) {
			if ( ElectricEIRChiller( EIRChillNum ).PrintMessage ) {
				++ElectricEIRChiller( EIRChillNum ).MsgErrorCount;
				//     Show single warning and pass additional info to ShowRecurringWarningErrorAtEnd
				if ( ElectricEIRChiller( EIRChillNum ).MsgErrorCount < 2 ) {
					ShowWarningError( ElectricEIRChiller( EIRChillNum ).MsgBuffer1 + '.' );
					ShowContinueError( ElectricEIRChiller( EIRChillNum ).MsgBuffer2 );
				} else {
					ShowRecurringWarningErrorAtEnd( ElectricEIRChiller( EIRChillNum ).MsgBuffer1 + " error continues.", ElectricEIRChiller( EIRChillNum ).ErrCount1, ElectricEIRChiller( EIRChillNum ).MsgDataLast, ElectricEIRChiller( EIRChillNum ).MsgDataLast, _, "[C]", "[C]" );
				}
			}
		}

		// save last system time step and last end time of current time step (used to determine if warning is valid)
		TimeStepSysLast = TimeStepSys;
		CurrentEndTimeLast = CurrentEndTime;

		// If no loop demand or chiller OFF, return
		//If Chiller load is 0 or chiller is not running then leave the subroutine.Before leaving
		//if the component control is SERIESACTIVE we set the component flow to inlet flow so that
		//flow resolver will not shut down the branch
		if ( MyLoad >= 0 || ! RunFlag ) {
			if ( EquipFlowCtrl == ControlType_SeriesActive || PlantLoop( PlantLoopNum ).LoopSide( LoopSideNum ).FlowLock == 1 ) {
				EvapMassFlowRate = Node( EvapInletNode ).MassFlowRate;
			}
			if ( ElectricEIRChiller( EIRChillNum ).CondenserType == WaterCooled ) {
				if ( PlantLoop( ElectricEIRChiller( EIRChillNum ).CDLoopNum ).LoopSide( ElectricEIRChiller( EIRChillNum ).CDLoopSideNum ).Branch( ElectricEIRChiller( EIRChillNum ).CDBranchNum ).Comp( ElectricEIRChiller( EIRChillNum ).CDCompNum ).FlowCtrl == ControlType_SeriesActive ) {
					CondMassFlowRate = Node( CondInletNode ).MassFlowRate;
				}
			}
			if ( ElectricEIRChiller( EIRChillNum ).CondenserType == EvapCooled ) {
				CalcBasinHeaterPower( ElectricEIRChiller( EIRChillNum ).BasinHeaterPowerFTempDiff, ElectricEIRChiller( EIRChillNum ).BasinHeaterSchedulePtr, ElectricEIRChiller( EIRChillNum ).BasinHeaterSetPointTemp, BasinHeaterPower );
			}
			ElectricEIRChiller( EIRChillNum ).PrintMessage = false;
			return;
		}

		// initialize outlet air humidity ratio of air or evap cooled chillers
		CondOutletHumRat = Node( CondInletNode ).HumRat;

		if ( ElectricEIRChiller( EIRChillNum ).CondenserType == AirCooled ) { // Condenser inlet temp = outdoor temp
			//    Node(CondInletNode)%Temp = OutDryBulbTemp
			Node( CondInletNode ).Temp = Node( CondInletNode ).OutAirDryBulb;

			// Warn user if entering condenser dry-bulb temperature falls below 0 C
			if ( Node( CondInletNode ).Temp < 0.0 && std::abs( MyLoad ) > 0 && RunFlag && ! WarmupFlag ) {
				ElectricEIRChiller( EIRChillNum ).PrintMessage = true;
				gio::write( OutputChar, OutputFormat ) << Node( CondInletNode ).Temp;
				ElectricEIRChiller( EIRChillNum ).MsgBuffer1 = "ElectricEIRChillerModel - CHILLER:ELECTRIC:EIR \"" + ElectricEIRChiller( EIRChillNum ).Name + "\" - Air Cooled Condenser Inlet Temperature below 0C";
				ElectricEIRChiller( EIRChillNum ).MsgBuffer2 = "... Outdoor Dry-bulb Condition = " + OutputChar + " C. Occurrence info = " + EnvironmentName + ", " + CurMnDy + ' ' + CreateSysTimeIntervalString();
				ElectricEIRChiller( EIRChillNum ).MsgDataLast = Node( CondInletNode ).Temp;
			} else {
				ElectricEIRChiller( EIRChillNum ).PrintMessage = false;
			}
		} else if ( ElectricEIRChiller( EIRChillNum ).CondenserType == EvapCooled ) { // Condenser inlet temp = (outdoor wet bulb)
			//    Node(CondInletNode)%Temp = OutWetBulbTemp
			Node( CondInletNode ).Temp = Node( CondInletNode ).OutAirWetBulb;
			//  line above assumes evaporation pushes condenser inlet air humidity ratio to saturation
			CondOutletHumRat = PsyWFnTdbTwbPb( Node( CondInletNode ).Temp, Node( CondInletNode ).Temp, Node( CondInletNode ).Press );

			// Warn user if evap condenser wet-bulb temperature falls below 10 C
			if ( Node( CondInletNode ).Temp < 10.0 && std::abs( MyLoad ) > 0 && RunFlag && ! WarmupFlag ) {
				ElectricEIRChiller( EIRChillNum ).PrintMessage = true;
				gio::write( OutputChar, OutputFormat ) << Node( CondInletNode ).Temp;
				ElectricEIRChiller( EIRChillNum ).MsgBuffer1 = "ElectricEIRChillerModel - CHILLER:ELECTRIC:EIR \"" + ElectricEIRChiller( EIRChillNum ).Name + "\" - Air Cooled Condenser Inlet Temperature below 10C";
				ElectricEIRChiller( EIRChillNum ).MsgBuffer2 = "... Outdoor Wet-bulb Condition = " + OutputChar + " C. Occurrence info = " + EnvironmentName + ", " + CurMnDy + ' ' + CreateSysTimeIntervalString();
				ElectricEIRChiller( EIRChillNum ).MsgDataLast = Node( CondInletNode ).Temp;
			} else {
				ElectricEIRChiller( EIRChillNum ).PrintMessage = false;
			}
		} // End of the Air Cooled/Evap Cooled Logic block

		// If not air or evap cooled then set to the condenser node that is attached to a cooling tower
		CondInletTemp = Node( CondInletNode ).Temp;

		// LOAD LOCAL VARIABLES FROM DATA STRUCTURE (for code readability)
		MinPartLoadRat = ElectricEIRChiller( EIRChillNum ).MinPartLoadRat;
		MaxPartLoadRat = ElectricEIRChiller( EIRChillNum ).MaxPartLoadRat;
		MinUnloadRat = ElectricEIRChiller( EIRChillNum ).MinUnloadRat;
		ChillerRefCap = ElectricEIRChiller( EIRChillNum ).RefCap;
		ReferenceCOP = ElectricEIRChiller( EIRChillNum ).RefCOP;
		EvapOutletTemp = Node( ElectricEIRChiller( EIRChillNum ).EvapOutletNodeNum ).Temp;
		TempLowLimitEout = ElectricEIRChiller( EIRChillNum ).TempLowLimitEvapOut;
		EvapMassFlowRateMax = ElectricEIRChiller( EIRChillNum ).EvapMassFlowRateMax;

		// Set mass flow rates
		if ( ElectricEIRChiller( EIRChillNum ).CondenserType == WaterCooled ) {
			CondMassFlowRate = ElectricEIRChiller( EIRChillNum ).CondMassFlowRateMax;
			SetComponentFlowRate( CondMassFlowRate, CondInletNode, CondOutletNode, ElectricEIRChiller( EIRChillNum ).CDLoopNum, ElectricEIRChiller( EIRChillNum ).CDLoopSideNum, ElectricEIRChiller( EIRChillNum ).CDBranchNum, ElectricEIRChiller( EIRChillNum ).CDCompNum );
			PullCompInterconnectTrigger( ElectricEIRChiller( EIRChillNum ).CWLoopNum, ElectricEIRChiller( EIRChillNum ).CWLoopSideNum, ElectricEIRChiller( EIRChillNum ).CWBranchNum, ElectricEIRChiller( EIRChillNum ).CWCompNum, ElectricEIRChiller( EIRChillNum ).CondMassFlowIndex, ElectricEIRChiller( EIRChillNum ).CDLoopNum, ElectricEIRChiller( EIRChillNum ).CDLoopSideNum, CriteriaType_MassFlowRate, CondMassFlowRate );

			if ( CondMassFlowRate < MassFlowTolerance ) {
				if ( EvapMassFlowRate < MassFlowTolerance ) {
					// Use SetComponentFlowRate to decide actual flow
					SetComponentFlowRate( EvapMassFlowRate, EvapInletNode, EvapOutletNode, ElectricEIRChiller( EIRChillNum ).CWLoopNum, ElectricEIRChiller( EIRChillNum ).CWLoopSideNum, ElectricEIRChiller( EIRChillNum ).CWBranchNum, ElectricEIRChiller( EIRChillNum ).CWCompNum );
				}
				return;
			}

		}

		{ auto const SELECT_CASE_var( PlantLoop( ElectricEIRChiller( EIRChillNum ).CWLoopNum ).LoopDemandCalcScheme );
		if ( SELECT_CASE_var == SingleSetPoint ) {
			if ( ( ElectricEIRChiller( EIRChillNum ).FlowMode == LeavingSetPointModulated ) || ( PlantLoop( PlantLoopNum ).LoopSide( LoopSideNum ).Branch( BranchNum ).Comp( CompNum ).CurOpSchemeType == CompSetPtBasedSchemeType ) || ( Node( ElectricEIRChiller( EIRChillNum ).EvapOutletNodeNum ).TempSetPoint != SensedNodeFlagValue ) ) {
				// there will be a valid setpoint on outlet
				EvapOutletTempSetPoint = Node( EvapOutletNode ).TempSetPoint;
			} else { // use plant loop overall setpoint
				EvapOutletTempSetPoint = Node( PlantLoop( PlantLoopNum ).TempSetPointNodeNum ).TempSetPoint;
			}
		} else if ( SELECT_CASE_var == DualSetPointDeadBand ) {
			if ( ( ElectricEIRChiller( EIRChillNum ).FlowMode == LeavingSetPointModulated ) || ( PlantLoop( PlantLoopNum ).LoopSide( LoopSideNum ).Branch( BranchNum ).Comp( CompNum ).CurOpSchemeType == CompSetPtBasedSchemeType ) || ( Node( ElectricEIRChiller( EIRChillNum ).EvapOutletNodeNum ).TempSetPointHi != SensedNodeFlagValue ) ) {
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
		if ( ElectricEIRChiller( EIRChillNum ).HeatRecActive ) {
			if ( ( ElectricEIRChillerReport( EIRChillNum ).QHeatRecovery + ElectricEIRChillerReport( EIRChillNum ).QCond ) > 0.0 ) { // protect div by zero
				AvgCondSinkTemp = ( ElectricEIRChillerReport( EIRChillNum ).QHeatRecovery * ElectricEIRChillerReport( EIRChillNum ).HeatRecInletTemp + ElectricEIRChillerReport( EIRChillNum ).QCond * ElectricEIRChillerReport( EIRChillNum ).CondInletTemp ) / ( ElectricEIRChillerReport( EIRChillNum ).QHeatRecovery + ElectricEIRChillerReport( EIRChillNum ).QCond );
			} else {
				AvgCondSinkTemp = CondInletTemp;
			}
		} else {
			AvgCondSinkTemp = CondInletTemp;
		}

		// Get capacity curve info with respect to CW setpoint and entering condenser water temps
		ChillerCapFT = CurveValue( ElectricEIRChiller( EIRChillNum ).ChillerCapFT, EvapOutletTempSetPoint, AvgCondSinkTemp );

		if ( ChillerCapFT < 0 ) {
			if ( ElectricEIRChiller( EIRChillNum ).ChillerCapFTError < 1 && PlantLoop( PlantLoopNum ).LoopSide( LoopSideNum ).FlowLock != 0 && ! WarmupFlag ) {
				++ElectricEIRChiller( EIRChillNum ).ChillerCapFTError;
				ShowWarningError( "CHILLER:ELECTRIC:EIR \"" + ElectricEIRChiller( EIRChillNum ).Name + "\":" );
				ShowContinueError( " Chiller Capacity as a Function of Temperature curve output is negative (" + RoundSigDigits( ChillerCapFT, 3 ) + ")." );
				ShowContinueError( " Negative value occurs using an Evaporator Outlet Temp of " + RoundSigDigits( EvapOutletTempSetPoint, 1 ) + " and a Condenser Inlet Temp of " + RoundSigDigits( CondInletTemp, 1 ) + '.' );
				ShowContinueErrorTimeStamp( " Resetting curve output to zero and continuing simulation." );
			} else if ( PlantLoop( PlantLoopNum ).LoopSide( LoopSideNum ).FlowLock != 0 && ! WarmupFlag ) {
				++ElectricEIRChiller( EIRChillNum ).ChillerCapFTError;
				ShowRecurringWarningErrorAtEnd( "CHILLER:ELECTRIC:EIR \"" + ElectricEIRChiller( EIRChillNum ).Name + "\": Chiller Capacity as a Function of Temperature curve output is negative warning continues...", ElectricEIRChiller( EIRChillNum ).ChillerCapFTErrorIndex, ChillerCapFT, ChillerCapFT );
			}
			ChillerCapFT = 0.0;
		}

		// Available chiller capacity as a function of temperature
		AvailChillerCap = ChillerRefCap * ChillerCapFT;

		//Only perform this check for temperature setpoint control
		if ( PlantLoop( PlantLoopNum ).LoopSide( LoopSideNum ).Branch( BranchNum ).Comp( CompNum ).CurOpSchemeType == CompSetPtBasedSchemeType ) {
			// Calculate water side load

			Cp = GetSpecificHeatGlycol( PlantLoop( ElectricEIRChiller( EIRChillNum ).CWLoopNum ).FluidName, Node( EvapInletNode ).Temp, PlantLoop( ElectricEIRChiller( EIRChillNum ).CWLoopNum ).FluidIndex, RoutineName );
			EvapMassFlowRate = Node( EvapInletNode ).MassFlowRate;
			{ auto const SELECT_CASE_var( PlantLoop( ElectricEIRChiller( EIRChillNum ).CWLoopNum ).LoopDemandCalcScheme );
			if ( SELECT_CASE_var == SingleSetPoint ) {
				TempLoad = EvapMassFlowRate * Cp * ( Node( EvapInletNode ).Temp - Node( EvapOutletNode ).TempSetPoint );
			} else if ( SELECT_CASE_var == DualSetPointDeadBand ) {
				TempLoad = EvapMassFlowRate * Cp * ( Node( EvapInletNode ).Temp - Node( EvapOutletNode ).TempSetPointHi );
			} else {
				assert( false );
			}}
			TempLoad = max( 0.0, TempLoad );

			// MyLoad is capped at minimum PLR * RefCap, adjust load to actual water side load because this chiller can cycle
			if ( std::abs( MyLoad ) > TempLoad ) {
				MyLoad = sign( TempLoad, MyLoad );
			}
		}

		// Part load ratio based on load and available chiller capacity, cap at max part load ratio
		if ( AvailChillerCap > 0 ) {
			PartLoadRat = max( 0.0, min( std::abs( MyLoad ) / AvailChillerCap, MaxPartLoadRat ) );
		} else {
			PartLoadRat = 0.0;
		}

		Cp = GetSpecificHeatGlycol( PlantLoop( ElectricEIRChiller( EIRChillNum ).CWLoopNum ).FluidName, Node( EvapInletNode ).Temp, PlantLoop( ElectricEIRChiller( EIRChillNum ).CWLoopNum ).FluidIndex, RoutineName );

		if ( PlantLoop( PlantLoopNum ).LoopSide( LoopSideNum ).Branch( BranchNum ).Comp( CompNum ).CurOpSchemeType == CompSetPtBasedSchemeType ) {
			ElectricEIRChiller( EIRChillNum ).PossibleSubcooling = false;
		} else {
			ElectricEIRChiller( EIRChillNum ).PossibleSubcooling = true;
		}
		// Set evaporator heat transfer rate
		QEvaporator = AvailChillerCap * PartLoadRat;

		// Either set the flow to the Constant value or caluclate the flow for the variable volume
		if ( ( ElectricEIRChiller( EIRChillNum ).FlowMode == ConstantFlow ) || ( ElectricEIRChiller( EIRChillNum ).FlowMode == NotModulated ) ) {
			// Set the evaporator mass flow rate to design
			// Start by assuming max (design) flow
			EvapMassFlowRate = EvapMassFlowRateMax;
			// Use SetComponentFlowRate to decide actual flow
			SetComponentFlowRate( EvapMassFlowRate, EvapInletNode, EvapOutletNode, ElectricEIRChiller( EIRChillNum ).CWLoopNum, ElectricEIRChiller( EIRChillNum ).CWLoopSideNum, ElectricEIRChiller( EIRChillNum ).CWBranchNum, ElectricEIRChiller( EIRChillNum ).CWCompNum );
			if ( EvapMassFlowRate != 0.0 ) {
				EvapDeltaTemp = QEvaporator / EvapMassFlowRate / Cp;
			} else {
				EvapDeltaTemp = 0.0;
			}
			// Evaluate outlet temp based on delta
			EvapOutletTemp = Node( EvapInletNode ).Temp - EvapDeltaTemp;

		} else if ( ElectricEIRChiller( EIRChillNum ).FlowMode == LeavingSetPointModulated ) {

			// Calculate the Delta Temp from the inlet temp to the chiller outlet setpoint
			{ auto const SELECT_CASE_var( PlantLoop( ElectricEIRChiller( EIRChillNum ).CWLoopNum ).LoopDemandCalcScheme );
			if ( SELECT_CASE_var == SingleSetPoint ) {
				EvapDeltaTemp = Node( EvapInletNode ).Temp - Node( EvapOutletNode ).TempSetPoint;
			} else if ( SELECT_CASE_var == DualSetPointDeadBand ) {
				EvapDeltaTemp = Node( EvapInletNode ).Temp - Node( EvapOutletNode ).TempSetPointHi;
			} else {
				assert( false );
			}}

			if ( EvapDeltaTemp != 0 ) {
				// Calculate desired flow to request based on load
				EvapMassFlowRate = std::abs( QEvaporator / Cp / EvapDeltaTemp );
				if ( ( EvapMassFlowRate - EvapMassFlowRateMax ) > MassFlowTolerance ) ElectricEIRChiller( EIRChillNum ).PossibleSubcooling = true;
				//Check to see if the Maximum is exceeded, if so set to maximum
				EvapMassFlowRate = min( EvapMassFlowRateMax, EvapMassFlowRate );
				// Use SetComponentFlowRate to decide actual flow
				SetComponentFlowRate( EvapMassFlowRate, EvapInletNode, EvapOutletNode, ElectricEIRChiller( EIRChillNum ).CWLoopNum, ElectricEIRChiller( EIRChillNum ).CWLoopSideNum, ElectricEIRChiller( EIRChillNum ).CWBranchNum, ElectricEIRChiller( EIRChillNum ).CWCompNum );
				// Should we recalculate this with the corrected setpoint?
				{ auto const SELECT_CASE_var( PlantLoop( ElectricEIRChiller( EIRChillNum ).CWLoopNum ).LoopDemandCalcScheme );
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
				SetComponentFlowRate( EvapMassFlowRate, EvapInletNode, EvapOutletNode, ElectricEIRChiller( EIRChillNum ).CWLoopNum, ElectricEIRChiller( EIRChillNum ).CWLoopSideNum, ElectricEIRChiller( EIRChillNum ).CWBranchNum, ElectricEIRChiller( EIRChillNum ).CWCompNum );
				// No deltaT since component is not running
				EvapOutletTemp = Node( EvapInletNode ).Temp;
				QEvaporator = 0.0;
				PartLoadRat = 0.0;
				ChillerPartLoadRatio = PartLoadRat;

				// DSU? so what if the delta T is zero?  On FlowLock==0, the inlet temp could = setpoint, right?
				if ( ElectricEIRChiller( EIRChillNum ).DeltaTErrCount < 1 && ! WarmupFlag ) {
					++ElectricEIRChiller( EIRChillNum ).DeltaTErrCount;
					ShowWarningError( "Evaporator DeltaTemp = 0 in mass flow calculation (Tevapin = Tsetpoint)." );
					ShowContinueErrorTimeStamp( "" );
				} else if ( ! WarmupFlag ) {
					++ElectricEIRChiller( EIRChillNum ).ChillerCapFTError;
					ShowRecurringWarningErrorAtEnd( "CHILLER:ELECTRIC:EIR \"" + ElectricEIRChiller( EIRChillNum ).Name + "\": Evaporator DeltaTemp = 0 in mass flow calculation warning continues...", ElectricEIRChiller( EIRChillNum ).DeltaTErrCountIndex, EvapDeltaTemp, EvapDeltaTemp );
				}

			}
		} //End of Constant Variable Flow If Block

		if ( EvapMassFlowRate == 0.0 ) {
			MyLoad = 0.0;
			if ( ElectricEIRChiller( EIRChillNum ).CondenserType == EvapCooled ) {
				CalcBasinHeaterPower( ElectricEIRChiller( EIRChillNum ).BasinHeaterPowerFTempDiff, ElectricEIRChiller( EIRChillNum ).BasinHeaterSchedulePtr, ElectricEIRChiller( EIRChillNum ).BasinHeaterSetPointTemp, BasinHeaterPower );
			}
			ElectricEIRChiller( EIRChillNum ).PrintMessage = false;
			return;
		}
		if ( ElectricEIRChiller( EIRChillNum ).PossibleSubcooling ) {
			QEvaporator = std::abs( MyLoad );
			EvapDeltaTemp = QEvaporator / EvapMassFlowRate / Cp;
			EvapOutletTemp = Node( EvapInletNode ).Temp - EvapDeltaTemp;
		} else {
			EvapDeltaTemp = Node( EvapInletNode ).Temp - EvapOutletTempSetPoint;
			QEvaporator = max( 0.0, ( EvapMassFlowRate * Cp * EvapDeltaTemp ) );
			EvapOutletTemp = EvapOutletTempSetPoint;
		}
		//Check that the Evap outlet temp honors both plant loop temp low limit and also the chiller low limit
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
		if ( QEvaporator == 0.0 && ElectricEIRChiller( EIRChillNum ).CondenserType == EvapCooled ) {
			CalcBasinHeaterPower( ElectricEIRChiller( EIRChillNum ).BasinHeaterPowerFTempDiff, ElectricEIRChiller( EIRChillNum ).BasinHeaterSchedulePtr, ElectricEIRChiller( EIRChillNum ).BasinHeaterSetPointTemp, BasinHeaterPower );
		}

		ChillerEIRFT = CurveValue( ElectricEIRChiller( EIRChillNum ).ChillerEIRFT, EvapOutletTemp, AvgCondSinkTemp );
		if ( ChillerEIRFT < 0.0 ) {
			if ( ElectricEIRChiller( EIRChillNum ).ChillerEIRFTError < 1 && PlantLoop( PlantLoopNum ).LoopSide( LoopSideNum ).FlowLock != 0 && ! WarmupFlag ) {
				++ElectricEIRChiller( EIRChillNum ).ChillerEIRFTError;
				ShowWarningError( "CHILLER:ELECTRIC:EIR \"" + ElectricEIRChiller( EIRChillNum ).Name + "\":" );
				ShowContinueError( " Chiller EIR as a Function of Temperature curve output is negative (" + RoundSigDigits( ChillerEIRFT, 3 ) + ")." );
				ShowContinueError( " Negative value occurs using an Evaporator Outlet Temp of " + RoundSigDigits( EvapOutletTemp, 1 ) + " and a Condenser Inlet Temp of " + RoundSigDigits( CondInletTemp, 1 ) + '.' );
				ShowContinueErrorTimeStamp( " Resetting curve output to zero and continuing simulation." );
			} else if ( PlantLoop( PlantLoopNum ).LoopSide( LoopSideNum ).FlowLock != 0 && ! WarmupFlag ) {
				++ElectricEIRChiller( EIRChillNum ).ChillerEIRFTError;
				ShowRecurringWarningErrorAtEnd( "CHILLER:ELECTRIC:EIR \"" + ElectricEIRChiller( EIRChillNum ).Name + "\": Chiller EIR as a Function of Temperature curve output is negative warning continues...", ElectricEIRChiller( EIRChillNum ).ChillerEIRFTErrorIndex, ChillerEIRFT, ChillerEIRFT );
			}
			ChillerEIRFT = 0.0;
		}

		ChillerEIRFPLR = CurveValue( ElectricEIRChiller( EIRChillNum ).ChillerEIRFPLR, PartLoadRat );
		if ( ChillerEIRFPLR < 0.0 ) {
			if ( ElectricEIRChiller( EIRChillNum ).ChillerEIRFPLRError < 1 && PlantLoop( PlantLoopNum ).LoopSide( LoopSideNum ).FlowLock != 0 && ! WarmupFlag ) {
				++ElectricEIRChiller( EIRChillNum ).ChillerEIRFPLRError;
				ShowWarningError( "CHILLER:ELECTRIC:EIR \"" + ElectricEIRChiller( EIRChillNum ).Name + "\":" );
				ShowContinueError( " Chiller EIR as a function of PLR curve output is negative (" + RoundSigDigits( ChillerEIRFPLR, 3 ) + ")." );
				ShowContinueError( " Negative value occurs using a part-load ratio of " + RoundSigDigits( PartLoadRat, 3 ) + '.' );
				ShowContinueErrorTimeStamp( " Resetting curve output to zero and continuing simulation." );
			} else if ( PlantLoop( PlantLoopNum ).LoopSide( LoopSideNum ).FlowLock != 0 && ! WarmupFlag ) {
				++ElectricEIRChiller( EIRChillNum ).ChillerEIRFPLRError;
				ShowRecurringWarningErrorAtEnd( "CHILLER:ELECTRIC:EIR \"" + ElectricEIRChiller( EIRChillNum ).Name + "\": Chiller EIR as a function of PLR curve output is negative warning continues...", ElectricEIRChiller( EIRChillNum ).ChillerEIRFPLRErrorIndex, ChillerEIRFPLR, ChillerEIRFPLR );
			}
			ChillerEIRFPLR = 0.0;
		}

		Power = ( AvailChillerCap / ReferenceCOP ) * ChillerEIRFPLR * ChillerEIRFT * FRAC;

		QCondenser = Power * ElectricEIRChiller( EIRChillNum ).CompPowerToCondenserFrac + QEvaporator + ChillerFalseLoadRate;

		if ( ElectricEIRChiller( EIRChillNum ).CondenserType == WaterCooled ) {
			if ( CondMassFlowRate > MassFlowTolerance ) {
				// If Heat Recovery specified for this vapor compression chiller, then Qcondenser will be adjusted by this subroutine
				if ( ElectricEIRChiller( EIRChillNum ).HeatRecActive ) EIRChillerHeatRecovery( EIRChillNum, QCondenser, CondMassFlowRate, CondInletTemp, QHeatRecovered );
				Cp = GetSpecificHeatGlycol( PlantLoop( ElectricEIRChiller( EIRChillNum ).CDLoopNum ).FluidName, CondInletTemp, PlantLoop( ElectricEIRChiller( EIRChillNum ).CDLoopNum ).FluidIndex, RoutineName );

				CondOutletTemp = QCondenser / CondMassFlowRate / Cp + CondInletTemp;
			} else {
				ShowSevereError( "CalcElectricEIRChillerModel: Condenser flow = 0, for ElectricEIRChiller=" + ElectricEIRChiller( EIRChillNum ).Name );
				ShowContinueErrorTimeStamp( "" );
				//DSU? maybe this could be handled earlier, check if this component has a load and an evap flow rate
				// then if cond flow is zero, just make a request to the condenser,
				// then just say it couldn't run until condenser loop wakes up.
				//CALL ShowFatalError('Program Terminates due to previous error condition.')
			}
		} else { //Air Cooled or Evap Cooled

			if ( QCondenser > 0.0 ) {
				CondMassFlowRate = ElectricEIRChiller( EIRChillNum ).CondMassFlowRateMax * PartLoadRat;
			} else {
				CondMassFlowRate = 0.0;
			}

			// If Heat Recovery specified for this vapor compression chiller, then Qcondenser will be adjusted by this subroutine
			if ( ElectricEIRChiller( EIRChillNum ).HeatRecActive ) EIRChillerHeatRecovery( EIRChillNum, QCondenser, CondMassFlowRate, CondInletTemp, QHeatRecovered );

			if ( CondMassFlowRate > 0.0 ) {
				Cp = PsyCpAirFnWTdb( Node( CondInletNode ).HumRat, CondInletTemp );
				CondOutletTemp = CondInletTemp + QCondenser / CondMassFlowRate / Cp;
			} else {
				CondOutletTemp = CondInletTemp;
			}
		}

		// Calculate condenser fan power
		if ( ChillerCapFT > 0.0 ) {
			CondenserFanPower = ChillerRefCap * ElectricEIRChiller( EIRChillNum ).CondenserFanPowerRatio * FRAC;
		} else {
			CondenserFanPower = 0.0;
		}

	}

	void
	EIRChillerHeatRecovery(
		int const EIRChillNum, // Number of the current electric EIR chiller being simulated
		Real64 & QCond, // Current condenser load [W]
		Real64 const CondMassFlow, // Current condenser mass flow [kg/s]
		Real64 const CondInletTemp, // Current condenser inlet temp [C]
		Real64 & QHeatRec // Amount of heat recovered [W]
	)
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR:          Richard Liesen
		//       DATE WRITTEN:    January 2004
		//       MODIFIED:        Richard Raustad, FSEC (occurrences of EIR only, calcs are identical to electric chiller)

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
		int HeatRecInNode; // Node number of heat recovery water inlet node
		int HeatRecOutNode; // Node number of heat recovery water outlet node
		Real64 QTotal; // Total condenser heat [W]
		Real64 HeatRecInletTemp; // Heat reclaim inlet temp [C]
		Real64 HeatRecMassFlowRate; // Heat reclaim mass flow rate [m3/s]
		Real64 TAvgIn; // Average inlet temperature of heat reclaim inlet and condenser inlet [C]
		Real64 TAvgOut; // Average outlet temperature [C]
		Real64 CpHeatRec; // Heat reclaim water inlet specific heat [J/kg-K]
		Real64 CpCond; // Condenser water inlet specific heat [J/kg-K]
		Real64 THeatRecSetPoint( 0.0 ); // local value for heat recovery leaving setpoint [C]
		Real64 QHeatRecToSetPoint; // load to heat recovery setpoint
		Real64 HeatRecHighInletLimit; // local value for inlet limit for heat recovery [C]

		// Begin routine
		HeatRecInNode = ElectricEIRChiller( EIRChillNum ).HeatRecInletNodeNum;
		HeatRecOutNode = ElectricEIRChiller( EIRChillNum ).HeatRecOutletNodeNum;
		CondInletNode = ElectricEIRChiller( EIRChillNum ).CondInletNodeNum;
		CondOutletNode = ElectricEIRChiller( EIRChillNum ).CondOutletNodeNum;

		// Inlet node to the heat recovery heat exchanger
		HeatRecInletTemp = Node( HeatRecInNode ).Temp;
		HeatRecMassFlowRate = Node( HeatRecInNode ).MassFlowRate;

		CpHeatRec = GetSpecificHeatGlycol( PlantLoop( ElectricEIRChiller( EIRChillNum ).HRLoopNum ).FluidName, HeatRecInletTemp, PlantLoop( ElectricEIRChiller( EIRChillNum ).HRLoopNum ).FluidIndex, RoutineName );
		CpCond = GetSpecificHeatGlycol( PlantLoop( ElectricEIRChiller( EIRChillNum ).CDLoopNum ).FluidName, CondInletTemp, PlantLoop( ElectricEIRChiller( EIRChillNum ).CDLoopNum ).FluidIndex, RoutineName );

		// Before we modify the QCondenser, the total or original value is transferred to QTot
		QTotal = QCond;

		if ( ElectricEIRChiller( EIRChillNum ).HeatRecSetPointNodeNum == 0 ) { // use original algorithm that blends temps
			TAvgIn = ( HeatRecMassFlowRate * CpHeatRec * HeatRecInletTemp + CondMassFlow * CpCond * CondInletTemp ) / ( HeatRecMassFlowRate * CpHeatRec + CondMassFlow * CpCond );

			TAvgOut = QTotal / ( HeatRecMassFlowRate * CpHeatRec + CondMassFlow * CpCond ) + TAvgIn;

			QHeatRec = HeatRecMassFlowRate * CpHeatRec * ( TAvgOut - HeatRecInletTemp );
			QHeatRec = max( QHeatRec, 0.0 ); // ensure non negative
			//check if heat flow too large for physical size of bundle
			QHeatRec = min( QHeatRec, ElectricEIRChiller( EIRChillNum ).HeatRecMaxCapacityLimit );
		} else { // use new algorithm to meet setpoint
			{ auto const SELECT_CASE_var( PlantLoop( ElectricEIRChiller( EIRChillNum ).HRLoopNum ).LoopDemandCalcScheme );

			if ( SELECT_CASE_var == SingleSetPoint ) {
				THeatRecSetPoint = Node( ElectricEIRChiller( EIRChillNum ).HeatRecSetPointNodeNum ).TempSetPoint;
			} else if ( SELECT_CASE_var == DualSetPointDeadBand ) {
				THeatRecSetPoint = Node( ElectricEIRChiller( EIRChillNum ).HeatRecSetPointNodeNum ).TempSetPointHi;
			} else {
				assert( false );
			}}

			QHeatRecToSetPoint = HeatRecMassFlowRate * CpHeatRec * ( THeatRecSetPoint - HeatRecInletTemp );
			QHeatRecToSetPoint = max( QHeatRecToSetPoint, 0.0 );
			QHeatRec = min( QTotal, QHeatRecToSetPoint );
			//check if heat flow too large for physical size of bundle
			QHeatRec = min( QHeatRec, ElectricEIRChiller( EIRChillNum ).HeatRecMaxCapacityLimit );
		}

		// check if limit on inlet is present and exceeded.
		if ( ElectricEIRChiller( EIRChillNum ).HeatRecInletLimitSchedNum > 0 ) {
			HeatRecHighInletLimit = GetCurrentScheduleValue( ElectricEIRChiller( EIRChillNum ).HeatRecInletLimitSchedNum );
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
	UpdateElectricEIRChillerRecords(
		Real64 const MyLoad, // Current load [W]
		bool const RunFlag, // TRUE if chiller operating
		int const Num // Chiller number
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR:          Richard Raustad, FSEC
		//       DATE WRITTEN:    June 2004

		// PURPOSE OF THIS SUBROUTINE:
		//  Reporting

		// METHODOLOGY EMPLOYED:
		//  na

		// REFERENCES:
		//  na

		// Using/Aliasing
		using DataBranchAirLoopPlant::MassFlowTolerance;
		using DataGlobals::SecInHour;
		using DataHVACGlobals::TimeStepSys;
		using PlantUtilities::SafeCopyPlantNode;
		using Psychrometrics::PsyHFnTdbW;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int EvapInletNode; // Evaporator inlet node number
		int EvapOutletNode; // Evaporator outlet node number
		int CondInletNode; // Condenser inlet node number
		int CondOutletNode; // Condenser outlet node number
		int HeatRecInNode; // Node number of heat recovery water inlet node
		int HeatRecOutNode; // Node number of heat recovery water outlet node
		Real64 ReportingConstant; // Number of seconds per HVAC system time step, to convert from W (J/s) to J

		ReportingConstant = TimeStepSys * SecInHour;

		EvapInletNode = ElectricEIRChiller( Num ).EvapInletNodeNum;
		EvapOutletNode = ElectricEIRChiller( Num ).EvapOutletNodeNum;
		CondInletNode = ElectricEIRChiller( Num ).CondInletNodeNum;
		CondOutletNode = ElectricEIRChiller( Num ).CondOutletNodeNum;
		HeatRecInNode = ElectricEIRChiller( Num ).HeatRecInletNodeNum;
		HeatRecOutNode = ElectricEIRChiller( Num ).HeatRecOutletNodeNum;

		if ( MyLoad >= 0 || ! RunFlag ) { // Chiller not running so pass inlet states to outlet states
			// Set node conditions
			Node( EvapOutletNode ).Temp = Node( EvapInletNode ).Temp;
			Node( CondOutletNode ).Temp = Node( CondInletNode ).Temp;
			if ( ElectricEIRChiller( Num ).CondenserType != WaterCooled ) {
				Node( CondOutletNode ).HumRat = Node( CondInletNode ).HumRat;
				Node( CondOutletNode ).Enthalpy = Node( CondInletNode ).Enthalpy;
			}

			ElectricEIRChillerReport( Num ).ChillerPartLoadRatio = 0.0;
			ElectricEIRChillerReport( Num ).ChillerCyclingRatio = 0.0;
			ElectricEIRChillerReport( Num ).ChillerFalseLoadRate = 0.0;
			ElectricEIRChillerReport( Num ).ChillerFalseLoad = 0.0;
			ElectricEIRChillerReport( Num ).Power = 0.0;
			ElectricEIRChillerReport( Num ).QEvap = 0.0;
			ElectricEIRChillerReport( Num ).QCond = 0.0;
			ElectricEIRChillerReport( Num ).Energy = 0.0;
			ElectricEIRChillerReport( Num ).EvapEnergy = 0.0;
			ElectricEIRChillerReport( Num ).CondEnergy = 0.0;
			ElectricEIRChillerReport( Num ).EvapInletTemp = Node( EvapInletNode ).Temp;
			ElectricEIRChillerReport( Num ).CondInletTemp = Node( CondInletNode ).Temp;
			ElectricEIRChillerReport( Num ).CondOutletTemp = Node( CondOutletNode ).Temp;
			ElectricEIRChillerReport( Num ).EvapOutletTemp = Node( EvapOutletNode ).Temp;
			ElectricEIRChillerReport( Num ).Evapmdot = EvapMassFlowRate; // could still be flow if in series
			ElectricEIRChillerReport( Num ).Condmdot = CondMassFlowRate; // could still be flow if in series
			ElectricEIRChillerReport( Num ).ActualCOP = 0.0;
			ElectricEIRChillerReport( Num ).CondenserFanPowerUse = 0.0;
			ElectricEIRChillerReport( Num ).CondenserFanEnergyConsumption = 0.0;
			if ( ElectricEIRChiller( Num ).CondenserType == EvapCooled ) {
				ElectricEIRChillerReport( Num ).BasinHeaterPower = BasinHeaterPower;
				ElectricEIRChillerReport( Num ).BasinHeaterConsumption = BasinHeaterPower * ReportingConstant;
			}

			if ( ElectricEIRChiller( Num ).HeatRecActive ) {

				SafeCopyPlantNode( HeatRecInNode, HeatRecOutNode );

				ElectricEIRChillerReport( Num ).QHeatRecovery = 0.0;
				ElectricEIRChillerReport( Num ).EnergyHeatRecovery = 0.0;
				ElectricEIRChillerReport( Num ).HeatRecInletTemp = Node( HeatRecInNode ).Temp;
				ElectricEIRChillerReport( Num ).HeatRecOutletTemp = Node( HeatRecOutNode ).Temp;
				ElectricEIRChillerReport( Num ).HeatRecMassFlow = Node( HeatRecInNode ).MassFlowRate;
			}

		} else { // Chiller is running, so pass calculated values
			// Set node temperatures
			if ( CondMassFlowRate < MassFlowTolerance && EvapMassFlowRate < MassFlowTolerance ) {
				Node( EvapOutletNode ).Temp = Node( EvapInletNode ).Temp;
				Node( CondOutletNode ).Temp = Node( CondInletNode ).Temp;
				if ( ElectricEIRChiller( Num ).CondenserType != WaterCooled ) {
					Node( CondOutletNode ).HumRat = Node( CondInletNode ).HumRat;
					Node( CondOutletNode ).Enthalpy = Node( CondInletNode ).Enthalpy;
				}
			} else {
				Node( EvapOutletNode ).Temp = EvapOutletTemp;
				Node( CondOutletNode ).Temp = CondOutletTemp;
				if ( ElectricEIRChiller( Num ).CondenserType != WaterCooled ) {
					Node( CondOutletNode ).HumRat = CondOutletHumRat;
					Node( CondOutletNode ).Enthalpy = PsyHFnTdbW( CondOutletTemp, CondOutletHumRat );
				}
			}

			// Set node flow rates;  for these load based models
			// assume that sufficient evaporator flow rate is available
			ElectricEIRChillerReport( Num ).ChillerPartLoadRatio = ChillerPartLoadRatio;
			ElectricEIRChillerReport( Num ).ChillerCyclingRatio = ChillerCyclingRatio;
			ElectricEIRChillerReport( Num ).ChillerFalseLoadRate = ChillerFalseLoadRate;
			ElectricEIRChillerReport( Num ).ChillerFalseLoad = ChillerFalseLoadRate * TimeStepSys * SecInHour;
			ElectricEIRChillerReport( Num ).Power = Power;
			ElectricEIRChillerReport( Num ).QEvap = QEvaporator;
			ElectricEIRChillerReport( Num ).QCond = QCondenser;
			ElectricEIRChillerReport( Num ).Energy = Power * TimeStepSys * SecInHour;
			ElectricEIRChillerReport( Num ).EvapEnergy = QEvaporator * TimeStepSys * SecInHour;
			ElectricEIRChillerReport( Num ).CondEnergy = QCondenser * TimeStepSys * SecInHour;
			ElectricEIRChillerReport( Num ).EvapInletTemp = Node( EvapInletNode ).Temp;
			ElectricEIRChillerReport( Num ).CondInletTemp = Node( CondInletNode ).Temp;
			ElectricEIRChillerReport( Num ).CondOutletTemp = Node( CondOutletNode ).Temp;
			ElectricEIRChillerReport( Num ).EvapOutletTemp = Node( EvapOutletNode ).Temp;
			ElectricEIRChillerReport( Num ).Evapmdot = EvapMassFlowRate;
			ElectricEIRChillerReport( Num ).Condmdot = CondMassFlowRate;
			ElectricEIRChillerReport( Num ).CondenserFanPowerUse = CondenserFanPower;
			ElectricEIRChillerReport( Num ).CondenserFanEnergyConsumption = CondenserFanPower * TimeStepSys * SecInHour;
			if ( Power != 0.0 ) {
				ElectricEIRChillerReport( Num ).ActualCOP = ( QEvaporator + ChillerFalseLoadRate ) / Power;
			} else {
				ElectricEIRChillerReport( Num ).ActualCOP = 0.0;
			}
			if ( ElectricEIRChiller( Num ).CondenserType == EvapCooled ) {
				ElectricEIRChillerReport( Num ).BasinHeaterPower = BasinHeaterPower;
				ElectricEIRChillerReport( Num ).BasinHeaterConsumption = BasinHeaterPower * ReportingConstant;
			}

			if ( ElectricEIRChiller( Num ).HeatRecActive ) {

				SafeCopyPlantNode( HeatRecInNode, HeatRecOutNode );
				ElectricEIRChillerReport( Num ).QHeatRecovery = QHeatRecovered;
				ElectricEIRChillerReport( Num ).EnergyHeatRecovery = QHeatRecovered * TimeStepSys * SecInHour;
				Node( HeatRecOutNode ).Temp = HeatRecOutletTemp;
				ElectricEIRChillerReport( Num ).HeatRecInletTemp = Node( HeatRecInNode ).Temp;
				ElectricEIRChillerReport( Num ).HeatRecOutletTemp = Node( HeatRecOutNode ).Temp;
				ElectricEIRChillerReport( Num ).HeatRecMassFlow = Node( HeatRecInNode ).MassFlowRate;
			}

		}

		ElectricEIRChillerReport( Num ).ChillerCapFT = ChillerCapFT;
		ElectricEIRChillerReport( Num ).ChillerEIRFT = ChillerEIRFT;
		ElectricEIRChillerReport( Num ).ChillerEIRFPLR = ChillerEIRFPLR;

	}

} // ChillerElectricEIR

} // EnergyPlus
