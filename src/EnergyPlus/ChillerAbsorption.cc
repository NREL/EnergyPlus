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

// ObjexxFCL Headers
#include <ObjexxFCL/Array.functions.hh>
#include <ObjexxFCL/Fmath.hh>

// EnergyPlus Headers
#include <ChillerAbsorption.hh>
#include <BranchNodeConnections.hh>
#include <DataBranchAirLoopPlant.hh>
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
#include <ReportSizingManager.hh>
#include <UtilityRoutines.hh>

namespace EnergyPlus {

namespace ChillerAbsorption {

	// MODULE INFORMATION:
	//       AUTHOR         Dan Fisher
	//       DATE WRITTEN   Nov. 2000
	//       MODIFIED       na
	//       RE-ENGINEERED  na

	// PURPOSE OF THIS MODULE:
	// This module simulates the performance of the BLAST
	// absorbers.

	// METHODOLOGY EMPLOYED:
	// Once the PlantLoopManager determines that the BLAST absorber
	// is available to meet a loop cooling demand, it calls SimBLAST
	// absorber which in turn calls the appropriate Absorption Chiller model.
	// All Absorption Chiller models are based on a polynomial fit of Absorber
	// performance data.

	// REFERENCES:
	// 1. BLAST Users Manual

	// OTHER NOTES:
	// The Absorber program from the BLAST family of software can be used
	// to generate the coefficients for the model.

	// Using/Aliasing
	using namespace DataPrecisionGlobals;
	using namespace DataLoopNode;
	using DataGlobals::InitConvTemp;
	using DataGlobals::DisplayExtraWarnings;
	using DataHVACGlobals::SmallWaterVolFlow;
	using General::TrimSigDigits;
	using General::RoundSigDigits;

	// Data
	// MODULE PARAMETER DEFINITIONS:
	//chiller flow modes
	int const FlowModeNotSet( 200 );
	int const ConstantFlow( 201 );
	int const NotModulated( 202 );
	int const LeavingSetPointModulated( 203 );

	// DERIVED TYPE DEFINITIONS:

	// MODULE VARIABLE DECLARATIONS:
	int NumBLASTAbsorbers( 0 ); // number of Absorption Chillers specified in input

	Real64 CondMassFlowRate( 0.0 ); // Kg/s - condenser mass flow rate, water side
	Real64 EvapMassFlowRate( 0.0 ); // Kg/s - evaporator mass flow rate, water side
	Real64 SteamMassFlowRate( 0.0 ); // Kg/s - steam mass flow rate, water side
	Real64 CondOutletTemp( 0.0 ); // C - condenser outlet temperature, water side
	Real64 EvapOutletTemp( 0.0 ); // C - evaporator outlet temperature, water side
	Real64 GenOutletTemp( 0.0 ); // C - generator fluid outlet temperature
	Real64 SteamOutletEnthalpy( 0.0 ); // J/kg - generator fluid outlet enthalpy
	Real64 PumpingPower( 0.0 ); // W - rate of Absorber energy use
	Real64 PumpingEnergy( 0.0 ); // J - Absorber energy use
	Real64 QGenerator( 0.0 ); // W - rate of Absorber steam use
	Real64 GeneratorEnergy( 0.0 ); // J - Absorber steam use
	Real64 QEvaporator( 0.0 ); // W - rate of heat transfer to the evaporator coil
	Real64 EvaporatorEnergy( 0.0 ); // J - heat transfer to the evaporator coil
	Real64 QCondenser( 0.0 ); // W - rate of heat transfer to the condenser coil
	Real64 CondenserEnergy( 0.0 ); // J - heat transfer to the condenser coil

	static std::string const BlankString;
	static std::string const fluidNameSteam( "STEAM" );
	static std::string const fluidNameWater( "WATER" );
	static std::string const moduleObjectType( "Chiller:Absorption" );
	static std::string const calcChillerAbsorption( "CALC Chiller:Absorption " );

	Array1D_bool CheckEquipName;

	// SUBROUTINE SPECIFICATIONS FOR MODULE:

	// Object Data
	Array1D< BLASTAbsorberSpecs > BLASTAbsorber; // dimension to number of machines
	Array1D< ReportVars > BLASTAbsorberReport;

	// MODULE SUBROUTINES:

	// Beginning of Absorption Chiller Module Driver Subroutines
	//*************************************************************************

	// Functions

	void
	SimBLASTAbsorber(
		std::string const & EP_UNUSED( AbsorberType ), // type of Absorber
		std::string const & AbsorberName, // user specified name of Absorber
		int const EquipFlowCtrl, // Flow control mode for the equipment
		int const LoopNum, // Plant loop index for where called from
		int const LoopSide, // Plant loop side index for where called from
		int & CompIndex, // Chiller number pointer
		bool const RunFlag, // simulate Absorber when TRUE
		bool const FirstIteration, // initialize variables when TRUE
		bool & InitLoopEquip, // If not zero, calculate the max load for operating conditions
		Real64 & MyLoad, // loop demand component will meet
		Real64 & MaxCap, // Maximum operating capacity of chiller [W]
		Real64 & MinCap, // Minimum operating capacity of chiller [W]
		Real64 & OptCap, // Optimal operating capacity of chiller [W]
		bool const GetSizingFactor, // TRUE when just the sizing factor is requested
		Real64 & SizingFactor, // sizing factor
		Real64 & TempCondInDesign
	)
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         Dan Fisher
		//       DATE WRITTEN   Nov. 2000
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE: This is the Absorption Chiller model driver.  It
		// gets the input for the models, initializes simulation variables, call
		// the appropriate model and sets up reporting variables.

		// METHODOLOGY EMPLOYED: na

		// REFERENCES: na

		// Using/Aliasing
		using InputProcessor::FindItemInList;
		using PlantUtilities::UpdateChillerComponentCondenserSide;
		using PlantUtilities::UpdateAbsorberChillerComponentGeneratorSide;
		using DataPlant::TypeOf_Chiller_Absorption;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		static bool GetInput( true ); // when TRUE, calls subroutine to read input file.
		int ChillNum; // Chiller number pointer

		//Get Absorber data from input file
		if ( GetInput ) {
			GetBLASTAbsorberInput();
			GetInput = false;
		}

		// Find the correct Chiller
		if ( CompIndex == 0 ) {
			ChillNum = FindItemInList( AbsorberName, BLASTAbsorber );
			if ( ChillNum == 0 ) {
				ShowFatalError( "SimBLASTAbsorber: Specified Absorber not one of Valid Absorption Chillers=" + AbsorberName );
			}
			CompIndex = ChillNum;
		} else {
			ChillNum = CompIndex;
			if ( ChillNum > NumBLASTAbsorbers || ChillNum < 1 ) {
				ShowFatalError( "SimBLASTAbsorber:  Invalid CompIndex passed=" + TrimSigDigits( ChillNum ) + ", Number of Units=" + TrimSigDigits( NumBLASTAbsorbers ) + ", Entered Unit name=" + AbsorberName );
			}
			if ( CheckEquipName( ChillNum ) ) {
				if ( AbsorberName != BLASTAbsorber( ChillNum ).Name ) {
					ShowFatalError( "SimBLASTAbsorber: Invalid CompIndex passed=" + TrimSigDigits( ChillNum ) + ", Unit name=" + AbsorberName + ", stored Unit Name for that index=" + BLASTAbsorber( ChillNum ).Name );
				}
				CheckEquipName( ChillNum ) = false;
			}
		}

		// Initialize Loop Equipment
		if ( InitLoopEquip ) {
			TempCondInDesign = BLASTAbsorber( ChillNum ).TempDesCondIn;
			InitBLASTAbsorberModel( ChillNum, RunFlag, MyLoad );

			if ( LoopNum == BLASTAbsorber( ChillNum ).CWLoopNum ) {
				SizeAbsorpChiller( ChillNum );
				MinCap = BLASTAbsorber( ChillNum ).NomCap * BLASTAbsorber( ChillNum ).MinPartLoadRat;
				MaxCap = BLASTAbsorber( ChillNum ).NomCap * BLASTAbsorber( ChillNum ).MaxPartLoadRat;
				OptCap = BLASTAbsorber( ChillNum ).NomCap * BLASTAbsorber( ChillNum ).OptPartLoadRat;
			} else {
				MinCap = 0.0;
				MaxCap = 0.0;
				OptCap = 0.0;
			}
			if ( GetSizingFactor ) {
				SizingFactor = BLASTAbsorber( ChillNum ).SizFac;
			}
			return;
		}

		// different actions depending on which loop the component was called from

		if ( LoopNum == BLASTAbsorber( ChillNum ).CWLoopNum ) {
			// called from dominant chilled water connection loop side

			//Calculate Load
			InitBLASTAbsorberModel( ChillNum, RunFlag, MyLoad );
			CalcBLASTAbsorberModel( ChillNum, MyLoad, RunFlag, FirstIteration, EquipFlowCtrl );
			UpdateBLASTAbsorberRecords( MyLoad, RunFlag, ChillNum );

		} else if ( LoopNum == BLASTAbsorber( ChillNum ).CDLoopNum ) {
			// Called from non-dominant condenser water connection loop side
			UpdateChillerComponentCondenserSide( LoopNum, LoopSide, TypeOf_Chiller_Absorption, BLASTAbsorber( ChillNum ).CondInletNodeNum, BLASTAbsorber( ChillNum ).CondOutletNodeNum, BLASTAbsorberReport( ChillNum ).QCond, BLASTAbsorberReport( ChillNum ).CondInletTemp, BLASTAbsorberReport( ChillNum ).CondOutletTemp, BLASTAbsorberReport( ChillNum ).Condmdot, FirstIteration );

		} else if ( LoopNum == BLASTAbsorber( ChillNum ).GenLoopNum ) {
			// Called from non-dominant generator hot water or steam connection loop side
			UpdateAbsorberChillerComponentGeneratorSide( LoopNum, LoopSide, TypeOf_Chiller_Absorption, BLASTAbsorber( ChillNum ).GeneratorInletNodeNum, BLASTAbsorber( ChillNum ).GeneratorOutletNodeNum, BLASTAbsorber( ChillNum ).GenHeatSourceType, BLASTAbsorberReport( ChillNum ).QGenerator, BLASTAbsorberReport( ChillNum ).SteamMdot, FirstIteration );

		} else {
			ShowFatalError( "SimBLASTAbsorber: Invalid LoopNum passed=" + TrimSigDigits( LoopNum ) + ", Unit name=" + AbsorberName + ", stored chilled water loop=" + TrimSigDigits( BLASTAbsorber( ChillNum ).CWLoopNum ) + ", stored condenser water loop=" + TrimSigDigits( BLASTAbsorber( ChillNum ).CDLoopNum ) + ", stored generator loop=" + TrimSigDigits( BLASTAbsorber( ChillNum ).GenLoopNum ) );
		}

	}

	// End Absorption Chiller Module Driver Subroutines
	//******************************************************************************

	// Beginning of Absorption Chiller Module Get Input subroutines
	//******************************************************************************

	void
	GetBLASTAbsorberInput()
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR:          Dan Fisher
		//       DATE WRITTEN:    April 1998
		//       MODIFIED:        R. Raustad May 2008 - added generator nodes

		// PURPOSE OF THIS SUBROUTINE:
		// This routine will get the input
		// required by the BLAST Absorption chiller models as shown below:

		// METHODOLOGY EMPLOYED:
		// EnergyPlus input processor

		// REFERENCES: na

		// Using/Aliasing
		using InputProcessor::GetNumObjectsFound;
		using InputProcessor::GetObjectItem;
		using InputProcessor::VerifyName;
		using InputProcessor::SameString;
		using InputProcessor::GetObjectDefMaxArgs;
		using namespace DataIPShortCuts; // Data for field names, blank numerics
		using BranchNodeConnections::TestCompSet;
		using NodeInputManager::GetOnlySingleNode;
		using GlobalNames::VerifyUniqueChillerName;
		using namespace OutputReportPredefined;
		using FluidProperties::FindRefrigerant;
		using General::RoundSigDigits;
		using DataGlobals::AnyEnergyManagementSystemInModel;
		using DataSizing::AutoSize;

		// Locals
		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "GetBLASTAbsorberInput: " ); // include trailing blank space

		//LOCAL VARIABLES
		int AbsorberNum; // Absorber counter
		int NumAlphas; // Number of elements in the alpha array
		int NumNums; // Number of elements in the numeric array
		int IOStat; // IO Status when calling get input subroutine
		Array1D_bool GenInputOutputNodesUsed; // Used for SetupOutputVariable
		static bool ErrorsFound( false );
		bool IsNotOK; // Flag to verify name
		bool IsBlank; // Flag for blank name
		bool errFlag;
		//  CHARACTER(len=MaxNameLength) :: CurrentModuleObject  ! for ease in renaming.

		//FLOW
		cCurrentModuleObject = moduleObjectType;

		NumBLASTAbsorbers = GetNumObjectsFound( cCurrentModuleObject );

		if ( NumBLASTAbsorbers <= 0 ) {
			ShowSevereError( "No " + cCurrentModuleObject + " equipment specified in input file" );
			//See if load distribution manager has already gotten the input
			ErrorsFound = true;
		}

		if ( allocated( BLASTAbsorber ) ) return;
		//ALLOCATE ARRAYS
		BLASTAbsorber.allocate( NumBLASTAbsorbers );
		CheckEquipName.dimension( NumBLASTAbsorbers, true );
		GenInputOutputNodesUsed.dimension( NumBLASTAbsorbers, false );

		BLASTAbsorberReport.allocate( NumBLASTAbsorbers );

		//LOAD ARRAYS WITH BLAST CURVE FIT Absorber DATA
		for ( AbsorberNum = 1; AbsorberNum <= NumBLASTAbsorbers; ++AbsorberNum ) {
			GetObjectItem( cCurrentModuleObject, AbsorberNum, cAlphaArgs, NumAlphas, rNumericArgs, NumNums, IOStat, _, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );

			IsNotOK = false;
			IsBlank = false;
			VerifyName( cAlphaArgs( 1 ), BLASTAbsorber, AbsorberNum - 1, IsNotOK, IsBlank, cCurrentModuleObject + " Name" );
			if ( IsNotOK ) {
				ErrorsFound = true;
				if ( IsBlank ) cAlphaArgs( 1 ) = "xxxxx";
			}
			VerifyUniqueChillerName( cCurrentModuleObject, cAlphaArgs( 1 ), errFlag, cCurrentModuleObject + " Name" );
			if ( errFlag ) {
				ErrorsFound = true;
			}
			BLASTAbsorber( AbsorberNum ).Name = cAlphaArgs( 1 );
			BLASTAbsorber( AbsorberNum ).NomCap = rNumericArgs( 1 );
			if ( BLASTAbsorber( AbsorberNum ).NomCap == AutoSize ) {
				BLASTAbsorber( AbsorberNum ).NomCapWasAutoSized = true;
			}
			BLASTAbsorber( AbsorberNum ).NomPumpPower = rNumericArgs( 2 );
			if ( BLASTAbsorber( AbsorberNum ).NomPumpPower == AutoSize ) {
				BLASTAbsorber( AbsorberNum ).NomPumpPowerWasAutoSized = true;
			}
			if ( rNumericArgs( 1 ) == 0.0 ) {
				ShowSevereError( "Invalid " + cNumericFieldNames( 1 ) + '=' + RoundSigDigits( rNumericArgs( 1 ), 2 ) );
				ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + cAlphaArgs( 1 ) );
				ErrorsFound = true;
			}
			// Assign Node Numbers to specified nodes
			BLASTAbsorber( AbsorberNum ).EvapInletNodeNum = GetOnlySingleNode( cAlphaArgs( 2 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Water, NodeConnectionType_Inlet, 1, ObjectIsNotParent );
			BLASTAbsorber( AbsorberNum ).EvapOutletNodeNum = GetOnlySingleNode( cAlphaArgs( 3 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Water, NodeConnectionType_Outlet, 1, ObjectIsNotParent );
			TestCompSet( cCurrentModuleObject, cAlphaArgs( 1 ), cAlphaArgs( 2 ), cAlphaArgs( 3 ), "Chilled Water Nodes" );

			BLASTAbsorber( AbsorberNum ).CondInletNodeNum = GetOnlySingleNode( cAlphaArgs( 4 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Water, NodeConnectionType_Inlet, 2, ObjectIsNotParent );
			BLASTAbsorber( AbsorberNum ).CondOutletNodeNum = GetOnlySingleNode( cAlphaArgs( 5 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Water, NodeConnectionType_Outlet, 2, ObjectIsNotParent );
			TestCompSet( cCurrentModuleObject, cAlphaArgs( 1 ), cAlphaArgs( 4 ), cAlphaArgs( 5 ), "Condenser (not tested) Nodes" );

			if ( NumAlphas > 8 ) {
				if ( SameString( cAlphaArgs( 9 ), "HotWater" ) || SameString( cAlphaArgs( 9 ), "HotWater" ) ) {
					BLASTAbsorber( AbsorberNum ).GenHeatSourceType = NodeType_Water;
				} else if ( SameString( cAlphaArgs( 9 ), "Steam" ) || cAlphaArgs( 9 ).empty() ) {
					BLASTAbsorber( AbsorberNum ).GenHeatSourceType = NodeType_Steam;
				} else {
					ShowSevereError( "Invalid " + cAlphaFieldNames( 9 ) + '=' + cAlphaArgs( 9 ) );
					ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + cAlphaArgs( 1 ) );
					ShowContinueError( "...Generator heat source type must be Steam or Hot Water." );
					ErrorsFound = true;
				}
			} else {
				BLASTAbsorber( AbsorberNum ).GenHeatSourceType = NodeType_Steam;
			}

			if ( ! lAlphaFieldBlanks( 6 ) && ! lAlphaFieldBlanks( 7 ) ) {
				GenInputOutputNodesUsed( AbsorberNum ) = true;
				if ( BLASTAbsorber( AbsorberNum ).GenHeatSourceType == NodeType_Water ) {
					BLASTAbsorber( AbsorberNum ).GeneratorInletNodeNum = GetOnlySingleNode( cAlphaArgs( 6 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Water, NodeConnectionType_Inlet, 3, ObjectIsNotParent );
					BLASTAbsorber( AbsorberNum ).GeneratorOutletNodeNum = GetOnlySingleNode( cAlphaArgs( 7 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Water, NodeConnectionType_Outlet, 3, ObjectIsNotParent );
					TestCompSet( cCurrentModuleObject, cAlphaArgs( 1 ), cAlphaArgs( 6 ), cAlphaArgs( 7 ), "Hot Water Nodes" );
				} else {
					BLASTAbsorber( AbsorberNum ).SteamFluidIndex = FindRefrigerant( "STEAM" );
					BLASTAbsorber( AbsorberNum ).GeneratorInletNodeNum = GetOnlySingleNode( cAlphaArgs( 6 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Steam, NodeConnectionType_Inlet, 3, ObjectIsNotParent );
					BLASTAbsorber( AbsorberNum ).GeneratorOutletNodeNum = GetOnlySingleNode( cAlphaArgs( 7 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Steam, NodeConnectionType_Outlet, 3, ObjectIsNotParent );
					TestCompSet( cCurrentModuleObject, cAlphaArgs( 1 ), cAlphaArgs( 6 ), cAlphaArgs( 7 ), "Steam Nodes" );
				}
			} else if ( ( lAlphaFieldBlanks( 6 ) && ! lAlphaFieldBlanks( 7 ) ) || ( ! lAlphaFieldBlanks( 6 ) && lAlphaFieldBlanks( 7 ) ) ) {
				ShowSevereError( cCurrentModuleObject + ", Name=" + cAlphaArgs( 1 ) );
				ShowContinueError( "...Generator fluid nodes must both be entered (or both left blank)." );
				ShowContinueError( "..." + cAlphaFieldNames( 6 ) + " = " + cAlphaArgs( 6 ) );
				ShowContinueError( "..." + cAlphaFieldNames( 7 ) + " = " + cAlphaArgs( 7 ) );
				ErrorsFound = true;
			} else {
				if ( BLASTAbsorber( AbsorberNum ).GenHeatSourceType == NodeType_Water ) {
					ShowWarningError( cCurrentModuleObject + ", Name=" + cAlphaArgs( 1 ) );
					ShowContinueError( "...Generator fluid type must be Steam if generator inlet/outlet nodes are blank." );
					ShowContinueError( "...Generator fluid type is set to Steam and the simulation continues." );
					BLASTAbsorber( AbsorberNum ).GenHeatSourceType = NodeType_Steam;
				}
			}

			// Get remaining data
			BLASTAbsorber( AbsorberNum ).MinPartLoadRat = rNumericArgs( 3 );
			BLASTAbsorber( AbsorberNum ).MaxPartLoadRat = rNumericArgs( 4 );
			BLASTAbsorber( AbsorberNum ).OptPartLoadRat = rNumericArgs( 5 );
			BLASTAbsorber( AbsorberNum ).TempDesCondIn = rNumericArgs( 6 );
			BLASTAbsorber( AbsorberNum ).EvapVolFlowRate = rNumericArgs( 7 );
			if ( BLASTAbsorber( AbsorberNum ).EvapVolFlowRate == AutoSize ) {
				BLASTAbsorber( AbsorberNum ).EvapVolFlowRateWasAutoSized = true;
			}
			BLASTAbsorber( AbsorberNum ).CondVolFlowRate = rNumericArgs( 8 );
			if ( BLASTAbsorber( AbsorberNum ).CondVolFlowRate == AutoSize ) {
				BLASTAbsorber( AbsorberNum ).CondVolFlowRateWasAutoSized = true;
			}
			BLASTAbsorber( AbsorberNum ).SteamLoadCoef( 1 ) = rNumericArgs( 9 );
			BLASTAbsorber( AbsorberNum ).SteamLoadCoef( 2 ) = rNumericArgs( 10 );
			BLASTAbsorber( AbsorberNum ).SteamLoadCoef( 3 ) = rNumericArgs( 11 );
			BLASTAbsorber( AbsorberNum ).PumpPowerCoef( 1 ) = rNumericArgs( 12 );
			BLASTAbsorber( AbsorberNum ).PumpPowerCoef( 2 ) = rNumericArgs( 13 );
			BLASTAbsorber( AbsorberNum ).PumpPowerCoef( 3 ) = rNumericArgs( 14 );
			BLASTAbsorber( AbsorberNum ).TempLowLimitEvapOut = rNumericArgs( 15 );

			{ auto const SELECT_CASE_var( cAlphaArgs( 8 ) );
			if ( SELECT_CASE_var == "CONSTANTFLOW" ) {
				BLASTAbsorber( AbsorberNum ).FlowMode = ConstantFlow;
			} else if ( SELECT_CASE_var == "VARIABLEFLOW" ) {
				BLASTAbsorber( AbsorberNum ).FlowMode = LeavingSetPointModulated;
				ShowWarningError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\"," );
				ShowContinueError( "Invalid " + cAlphaFieldNames( 8 ) + '=' + cAlphaArgs( 8 ) );
				ShowContinueError( "Key choice is now called \"LeavingSetpointModulated\" and the simulation continues" );
			} else if ( SELECT_CASE_var == "LEAVINGSETPOINTMODULATED" ) {
				BLASTAbsorber( AbsorberNum ).FlowMode = LeavingSetPointModulated;
			} else if ( SELECT_CASE_var == "NOTMODULATED" ) {
				BLASTAbsorber( AbsorberNum ).FlowMode = NotModulated;
			} else {
				ShowSevereError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\"," );
				ShowContinueError( "Invalid " + cAlphaFieldNames( 8 ) + '=' + cAlphaArgs( 8 ) );
				ShowContinueError( "Available choices are ConstantFlow, NotModulated, or LeavingSetpointModulated" );
				ShowContinueError( "Flow mode NotModulated is assumed and the simulation continues." );
				BLASTAbsorber( AbsorberNum ).FlowMode = NotModulated;
			}}

			if ( NumNums > 15 ) {
				BLASTAbsorber( AbsorberNum ).GeneratorVolFlowRate = rNumericArgs( 16 );
				if ( BLASTAbsorber( AbsorberNum ).GeneratorVolFlowRate == AutoSize ) {
					BLASTAbsorber( AbsorberNum ).GeneratorVolFlowRateWasAutoSized = true;
				}
			}

			if ( BLASTAbsorber( AbsorberNum ).GeneratorVolFlowRate == 0.0 && BLASTAbsorber( AbsorberNum ).GenHeatSourceType == NodeType_Water ) {
				ShowSevereError( "Invalid " + cNumericFieldNames( 16 ) + '=' + RoundSigDigits( rNumericArgs( 16 ), 2 ) );
				ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + cAlphaArgs( 1 ) );
				ShowContinueError( "...Generator water flow rate must be greater than 0 when absorber generator fluid type is hot water." );
				ErrorsFound = true;
			}

			if ( NumNums > 16 ) {
				BLASTAbsorber( AbsorberNum ).GeneratorSubcool = rNumericArgs( 17 );
			} else {
				BLASTAbsorber( AbsorberNum ).GeneratorSubcool = 1.0;
			}

			if ( NumNums > 17 ) {
				BLASTAbsorber( AbsorberNum ).SizFac = rNumericArgs( 18 );
			} else {
				BLASTAbsorber( AbsorberNum ).SizFac = 1.0;
			}

		}

		if ( ErrorsFound ) {
			ShowFatalError( "Errors found in processing input for " + cCurrentModuleObject );
		}

		for ( AbsorberNum = 1; AbsorberNum <= NumBLASTAbsorbers; ++AbsorberNum ) {
			SetupOutputVariable( "Chiller Electric Power [W]", BLASTAbsorberReport( AbsorberNum ).PumpingPower, "System", "Average", BLASTAbsorber( AbsorberNum ).Name );
			SetupOutputVariable( "Chiller Electric Energy [J]", BLASTAbsorberReport( AbsorberNum ).PumpingEnergy, "System", "Sum", BLASTAbsorber( AbsorberNum ).Name, _, "ELECTRICITY", "Cooling", _, "Plant" );
			SetupOutputVariable( "Chiller Evaporator Cooling Rate [W]", BLASTAbsorberReport( AbsorberNum ).QEvap, "System", "Average", BLASTAbsorber( AbsorberNum ).Name );
			SetupOutputVariable( "Chiller Evaporator Cooling Energy [J]", BLASTAbsorberReport( AbsorberNum ).EvapEnergy, "System", "Sum", BLASTAbsorber( AbsorberNum ).Name, _, "ENERGYTRANSFER", "CHILLERS", _, "Plant" );
			SetupOutputVariable( "Chiller Evaporator Inlet Temperature [C]", BLASTAbsorberReport( AbsorberNum ).EvapInletTemp, "System", "Average", BLASTAbsorber( AbsorberNum ).Name );
			SetupOutputVariable( "Chiller Evaporator Outlet Temperature [C]", BLASTAbsorberReport( AbsorberNum ).EvapOutletTemp, "System", "Average", BLASTAbsorber( AbsorberNum ).Name );
			SetupOutputVariable( "Chiller Evaporator Mass Flow Rate [kg/s]", BLASTAbsorberReport( AbsorberNum ).Evapmdot, "System", "Average", BLASTAbsorber( AbsorberNum ).Name );

			SetupOutputVariable( "Chiller Condenser Heat Transfer Rate [W]", BLASTAbsorberReport( AbsorberNum ).QCond, "System", "Average", BLASTAbsorber( AbsorberNum ).Name );
			SetupOutputVariable( "Chiller Condenser Heat Transfer Energy [J]", BLASTAbsorberReport( AbsorberNum ).CondEnergy, "System", "Sum", BLASTAbsorber( AbsorberNum ).Name, _, "ENERGYTRANSFER", "HEATREJECTION", _, "Plant" );
			SetupOutputVariable( "Chiller Condenser Inlet Temperature [C]", BLASTAbsorberReport( AbsorberNum ).CondInletTemp, "System", "Average", BLASTAbsorber( AbsorberNum ).Name );
			SetupOutputVariable( "Chiller Condenser Outlet Temperature [C]", BLASTAbsorberReport( AbsorberNum ).CondOutletTemp, "System", "Average", BLASTAbsorber( AbsorberNum ).Name );
			SetupOutputVariable( "Chiller Condenser Mass Flow Rate [kg/s]", BLASTAbsorberReport( AbsorberNum ).Condmdot, "System", "Average", BLASTAbsorber( AbsorberNum ).Name );

			if ( BLASTAbsorber( AbsorberNum ).GenHeatSourceType == NodeType_Water ) {
				SetupOutputVariable( "Chiller Hot Water Consumption Rate [W]", BLASTAbsorberReport( AbsorberNum ).QGenerator, "System", "Average", BLASTAbsorber( AbsorberNum ).Name );
				SetupOutputVariable( "Chiller Source Hot Water Energy [J]", BLASTAbsorberReport( AbsorberNum ).GeneratorEnergy, "System", "Sum", BLASTAbsorber( AbsorberNum ).Name, _, "PLANTLOOPHEATINGDEMAND", "CHILLERS", _, "Plant" );
			} else {
				if ( GenInputOutputNodesUsed( AbsorberNum ) ) {
					SetupOutputVariable( "Chiller Source Steam Rate [W]", BLASTAbsorberReport( AbsorberNum ).QGenerator, "System", "Average", BLASTAbsorber( AbsorberNum ).Name );
					SetupOutputVariable( "Chiller Source Steam Energy [J]", BLASTAbsorberReport( AbsorberNum ).GeneratorEnergy, "System", "Sum", BLASTAbsorber( AbsorberNum ).Name, _, "PLANTLOOPHEATINGDEMAND", "CHILLERS", _, "Plant" );
				} else {
					SetupOutputVariable( "Chiller Source Steam Rate [W]", BLASTAbsorberReport( AbsorberNum ).QGenerator, "System", "Average", BLASTAbsorber( AbsorberNum ).Name );
					SetupOutputVariable( "Chiller Source Steam Energy [J]", BLASTAbsorberReport( AbsorberNum ).GeneratorEnergy, "System", "Sum", BLASTAbsorber( AbsorberNum ).Name, _, "Steam", "Cooling", _, "Plant" );
				}
			}

			SetupOutputVariable( "Chiller COP [W/W]", BLASTAbsorberReport( AbsorberNum ).ActualCOP, "System", "Average", BLASTAbsorber( AbsorberNum ).Name );

			if ( AnyEnergyManagementSystemInModel ) {
				SetupEMSInternalVariable( "Chiller Nominal Capacity", BLASTAbsorber( AbsorberNum ).Name, "[W]", BLASTAbsorber( AbsorberNum ).NomCap );
			}

		}

		if ( allocated( GenInputOutputNodesUsed ) ) GenInputOutputNodesUsed.deallocate();

	}

	// End of Get Input subroutines for the Absorption Chiller Module
	//******************************************************************************

	void
	InitBLASTAbsorberModel(
		int const ChillNum, // number of the current electric chiller being simulated
		bool const RunFlag, // TRUE when chiller operating
		Real64 const MyLoad
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Richard Raustad
		//       DATE WRITTEN   September 2009
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
		using DataPlant::TypeOf_Chiller_Absorption;
		using DataPlant::ScanPlantLoopsForObject;
		using DataPlant::PlantFirstSizesOkayToFinalize;
		using DataPlant::LoopFlowStatus_NeedyIfLoopOn;
		using InputProcessor::SameString;
		using PlantUtilities::InterConnectTwoPlantLoopSides;
		using PlantUtilities::InitComponentNodes;
		using PlantUtilities::SetComponentFlowRate;
		using FluidProperties::GetDensityGlycol;
		using FluidProperties::GetSatEnthalpyRefrig;
		using FluidProperties::GetSatDensityRefrig;
		using EMSManager::iTemperatureSetPoint;
		using EMSManager::CheckIfNodeSetPointManagedByEMS;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "InitBLASTAbsorberModel" );

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
		bool errFlag;
		bool FatalError;
		Real64 rho; // local fluid density
		Real64 CpWater; // local specific heat
		Real64 SteamDensity; // density of generator steam (when connected to a steam loop)
		Real64 EnthSteamOutDry; // dry enthalpy of steam (quality = 1)
		Real64 EnthSteamOutWet; // wet enthalpy of steam (quality = 0)
		Real64 HfgSteam; // latent heat of steam at constant pressure
		Real64 SteamDeltaT; // amount of sub-cooling of steam condensate
		int GeneratorInletNode; // generator inlet node number, steam/water side
		Real64 SteamOutletTemp;
		static int DummyWaterIndex( 1 );
		Real64 mdotEvap; // local fluid mass flow rate thru evaporator
		Real64 mdotCond; // local fluid mass flow rate thru condenser
		Real64 mdotGen; // local fluid mass flow rate thru generator

		// FLOW:

		// Do the one time initializations
		if ( MyOneTimeFlag ) {
			MyFlag.allocate( NumBLASTAbsorbers );
			MyEnvrnFlag.allocate( NumBLASTAbsorbers );
			MyFlag = true;
			MyEnvrnFlag = true;
			MyOneTimeFlag = false;
		}

		// Init more variables
		if ( MyFlag( ChillNum ) ) {
			// Locate the chillers on the plant loops for later usage
			errFlag = false;
			ScanPlantLoopsForObject( BLASTAbsorber( ChillNum ).Name, TypeOf_Chiller_Absorption, BLASTAbsorber( ChillNum ).CWLoopNum, BLASTAbsorber( ChillNum ).CWLoopSideNum, BLASTAbsorber( ChillNum ).CWBranchNum, BLASTAbsorber( ChillNum ).CWCompNum, BLASTAbsorber( ChillNum ).TempLowLimitEvapOut, _, _, BLASTAbsorber( ChillNum ).EvapInletNodeNum, _, errFlag );
			if ( BLASTAbsorber( ChillNum ).CondInletNodeNum > 0 ) {
				ScanPlantLoopsForObject( BLASTAbsorber( ChillNum ).Name, TypeOf_Chiller_Absorption, BLASTAbsorber( ChillNum ).CDLoopNum, BLASTAbsorber( ChillNum ).CDLoopSideNum, BLASTAbsorber( ChillNum ).CDBranchNum, BLASTAbsorber( ChillNum ).CDCompNum, _, _, _, BLASTAbsorber( ChillNum ).CondInletNodeNum, _, errFlag );
				InterConnectTwoPlantLoopSides( BLASTAbsorber( ChillNum ).CWLoopNum, BLASTAbsorber( ChillNum ).CWLoopSideNum, BLASTAbsorber( ChillNum ).CDLoopNum, BLASTAbsorber( ChillNum ).CDLoopSideNum, TypeOf_Chiller_Absorption, true );
			}
			if ( BLASTAbsorber( ChillNum ).GeneratorInletNodeNum > 0 ) {
				ScanPlantLoopsForObject( BLASTAbsorber( ChillNum ).Name, TypeOf_Chiller_Absorption, BLASTAbsorber( ChillNum ).GenLoopNum, BLASTAbsorber( ChillNum ).GenLoopSideNum, BLASTAbsorber( ChillNum ).GenBranchNum, BLASTAbsorber( ChillNum ).GenCompNum, _, _, _, BLASTAbsorber( ChillNum ).GeneratorInletNodeNum, _, errFlag );
				InterConnectTwoPlantLoopSides( BLASTAbsorber( ChillNum ).CWLoopNum, BLASTAbsorber( ChillNum ).CWLoopSideNum, BLASTAbsorber( ChillNum ).GenLoopNum, BLASTAbsorber( ChillNum ).GenCompNum, TypeOf_Chiller_Absorption, true );
			}

			//Fill in connection data
			if ( ( BLASTAbsorber( ChillNum ).CondInletNodeNum > 0 ) && ( BLASTAbsorber( ChillNum ).GeneratorInletNodeNum > 0 ) ) {
				InterConnectTwoPlantLoopSides( BLASTAbsorber( ChillNum ).CDLoopNum, BLASTAbsorber( ChillNum ).CDLoopSideNum, BLASTAbsorber( ChillNum ).GenLoopNum, BLASTAbsorber( ChillNum ).GenCompNum, TypeOf_Chiller_Absorption, false );
			}
			if ( errFlag ) {
				ShowFatalError( "InitBLASTAbsorberModel: Program terminated due to previous condition(s)." );
			}

			if ( BLASTAbsorber( ChillNum ).FlowMode == ConstantFlow ) {
				PlantLoop( BLASTAbsorber( ChillNum ).CWLoopNum ).LoopSide( BLASTAbsorber( ChillNum ).CWLoopSideNum ).Branch( BLASTAbsorber( ChillNum ).CWBranchNum ).Comp( BLASTAbsorber( ChillNum ).CWCompNum ).FlowPriority = LoopFlowStatus_NeedyIfLoopOn;
			}

			if ( BLASTAbsorber( ChillNum ).FlowMode == LeavingSetPointModulated ) {
				PlantLoop( BLASTAbsorber( ChillNum ).CWLoopNum ).LoopSide( BLASTAbsorber( ChillNum ).CWLoopSideNum ).Branch( BLASTAbsorber( ChillNum ).CWBranchNum ).Comp( BLASTAbsorber( ChillNum ).CWCompNum ).FlowPriority = LoopFlowStatus_NeedyIfLoopOn;

				if ( ( Node( BLASTAbsorber( ChillNum ).EvapOutletNodeNum ).TempSetPoint == SensedNodeFlagValue ) && ( Node( BLASTAbsorber( ChillNum ).EvapOutletNodeNum ).TempSetPointHi == SensedNodeFlagValue ) ) {
					if ( ! AnyEnergyManagementSystemInModel ) {
						if ( ! BLASTAbsorber( ChillNum ).ModulatedFlowErrDone ) {
							ShowWarningError( "Missing temperature setpoint for LeavingSetpointModulated mode chiller named " + BLASTAbsorber( ChillNum ).Name );
							ShowContinueError( "  A temperature setpoint is needed at the outlet node of a chiller in variable flow mode, use a SetpointManager" );
							ShowContinueError( "  The overall loop setpoint will be assumed for chiller. The simulation continues ... " );
							BLASTAbsorber( ChillNum ).ModulatedFlowErrDone = true;
						}
					} else {
						// need call to EMS to check node
						FatalError = false; // but not really fatal yet, but should be.
						CheckIfNodeSetPointManagedByEMS( BLASTAbsorber( ChillNum ).EvapOutletNodeNum, iTemperatureSetPoint, FatalError );
						if ( FatalError ) {
							if ( ! BLASTAbsorber( ChillNum ).ModulatedFlowErrDone ) {
								ShowWarningError( "Missing temperature setpoint for LeavingSetpointModulated mode chiller named " + BLASTAbsorber( ChillNum ).Name );
								ShowContinueError( "  A temperature setpoint is needed at the outlet node of a chiller evaporator in variable flow mode" );
								ShowContinueError( "  use a Setpoint Manager to establish a setpoint at the chiller evaporator outlet node " );
								ShowContinueError( "  or use an EMS actuator to establish a setpoint at the outlet node " );
								ShowContinueError( "  The overall loop setpoint will be assumed for chiller. The simulation continues ... " );
								BLASTAbsorber( ChillNum ).ModulatedFlowErrDone = true;
							}
						}
					}

					BLASTAbsorber( ChillNum ).ModulatedFlowSetToLoop = true;
					Node( BLASTAbsorber( ChillNum ).EvapOutletNodeNum ).TempSetPoint = Node( PlantLoop( BLASTAbsorber( ChillNum ).CWLoopNum ).TempSetPointNodeNum ).TempSetPoint;
					Node( BLASTAbsorber( ChillNum ).EvapOutletNodeNum ).TempSetPointHi = Node( PlantLoop( BLASTAbsorber( ChillNum ).CWLoopNum ).TempSetPointNodeNum ).TempSetPointHi;
				}
			}

			MyFlag( ChillNum ) = false;
		}

		CondInletNode = BLASTAbsorber( ChillNum ).CondInletNodeNum;
		CondOutletNode = BLASTAbsorber( ChillNum ).CondOutletNodeNum;

		//Initialize critical Demand Side Variables
		//  IF((MyEnvrnFlag(ChillNum) .and. BeginEnvrnFlag) &
		//     .OR. (Node(CondInletNode)%MassFlowrate <= 0.0 .AND. RunFlag)) THEN

		if ( MyEnvrnFlag( ChillNum ) && BeginEnvrnFlag && ( PlantFirstSizesOkayToFinalize ) ) {

			rho = GetDensityGlycol( PlantLoop( BLASTAbsorber( ChillNum ).CWLoopNum ).FluidName, InitConvTemp, PlantLoop( BLASTAbsorber( ChillNum ).CWLoopNum ).FluidIndex, RoutineName );

			BLASTAbsorber( ChillNum ).EvapMassFlowRateMax = BLASTAbsorber( ChillNum ).EvapVolFlowRate * rho;

			InitComponentNodes( 0.0, BLASTAbsorber( ChillNum ).EvapMassFlowRateMax, BLASTAbsorber( ChillNum ).EvapInletNodeNum, BLASTAbsorber( ChillNum ).EvapOutletNodeNum, BLASTAbsorber( ChillNum ).CWLoopNum, BLASTAbsorber( ChillNum ).CWLoopSideNum, BLASTAbsorber( ChillNum ).CWBranchNum, BLASTAbsorber( ChillNum ).CWCompNum );

			rho = GetDensityGlycol( PlantLoop( BLASTAbsorber( ChillNum ).CDLoopNum ).FluidName, InitConvTemp, PlantLoop( BLASTAbsorber( ChillNum ).CDLoopNum ).FluidIndex, RoutineName );

			BLASTAbsorber( ChillNum ).CondMassFlowRateMax = rho * BLASTAbsorber( ChillNum ).CondVolFlowRate;

			InitComponentNodes( 0.0, BLASTAbsorber( ChillNum ).CondMassFlowRateMax, CondInletNode, CondOutletNode, BLASTAbsorber( ChillNum ).CDLoopNum, BLASTAbsorber( ChillNum ).CDLoopSideNum, BLASTAbsorber( ChillNum ).CDBranchNum, BLASTAbsorber( ChillNum ).CDCompNum );
			Node( CondInletNode ).Temp = BLASTAbsorber( ChillNum ).TempDesCondIn;

			if ( BLASTAbsorber( ChillNum ).GeneratorInletNodeNum > 0 ) {

				if ( BLASTAbsorber( ChillNum ).GenHeatSourceType == NodeType_Water ) {
					rho = GetDensityGlycol( PlantLoop( BLASTAbsorber( ChillNum ).GenLoopNum ).FluidName, InitConvTemp, PlantLoop( BLASTAbsorber( ChillNum ).GenLoopNum ).FluidIndex, RoutineName );

					BLASTAbsorber( ChillNum ).GenMassFlowRateMax = rho * BLASTAbsorber( ChillNum ).GeneratorVolFlowRate;
				} else if ( BLASTAbsorber( ChillNum ).GenHeatSourceType == NodeType_Steam ) {

					QGenerator = ( BLASTAbsorber( ChillNum ).SteamLoadCoef( 1 ) + BLASTAbsorber( ChillNum ).SteamLoadCoef( 2 ) + BLASTAbsorber( ChillNum ).SteamLoadCoef( 3 ) ) * BLASTAbsorber( ChillNum ).NomCap;
					GeneratorInletNode = BLASTAbsorber( ChillNum ).GeneratorInletNodeNum;
					EnthSteamOutDry = GetSatEnthalpyRefrig( fluidNameSteam, Node( GeneratorInletNode ).Temp, 1.0, BLASTAbsorber( ChillNum ).SteamFluidIndex, calcChillerAbsorption + BLASTAbsorber( ChillNum ).Name );
					EnthSteamOutWet = GetSatEnthalpyRefrig( fluidNameSteam, Node( GeneratorInletNode ).Temp, 0.0, BLASTAbsorber( ChillNum ).SteamFluidIndex, calcChillerAbsorption + BLASTAbsorber( ChillNum ).Name );
					SteamDeltaT = BLASTAbsorber( ChillNum ).GeneratorSubcool;
					SteamOutletTemp = Node( GeneratorInletNode ).Temp - SteamDeltaT;
					HfgSteam = EnthSteamOutDry - EnthSteamOutWet;
					SteamDensity = GetSatDensityRefrig( fluidNameSteam, Node( GeneratorInletNode ).Temp, 1.0, BLASTAbsorber( ChillNum ).SteamFluidIndex, calcChillerAbsorption + BLASTAbsorber( ChillNum ).Name );
					CpWater = GetDensityGlycol( fluidNameWater, SteamOutletTemp, DummyWaterIndex, calcChillerAbsorption + BLASTAbsorber( ChillNum ).Name );
					BLASTAbsorber( ChillNum ).GenMassFlowRateMax = QGenerator / ( HfgSteam + CpWater * SteamDeltaT );
				}

				InitComponentNodes( 0.0, BLASTAbsorber( ChillNum ).GenMassFlowRateMax, BLASTAbsorber( ChillNum ).GeneratorInletNodeNum, BLASTAbsorber( ChillNum ).GeneratorOutletNodeNum, BLASTAbsorber( ChillNum ).GenLoopNum, BLASTAbsorber( ChillNum ).GenLoopSideNum, BLASTAbsorber( ChillNum ).GenBranchNum, BLASTAbsorber( ChillNum ).GenCompNum );
			}

			MyEnvrnFlag( ChillNum ) = false;
		}
		if ( ! BeginEnvrnFlag ) {
			MyEnvrnFlag( ChillNum ) = true;
		}

		// every time inits

		if ( ( BLASTAbsorber( ChillNum ).FlowMode == LeavingSetPointModulated ) && BLASTAbsorber( ChillNum ).ModulatedFlowSetToLoop ) {
			// fix for clumsy old input that worked because loop setpoint was spread.
			//  could be removed with transition, testing , model change, period of being obsolete.
			Node( BLASTAbsorber( ChillNum ).EvapOutletNodeNum ).TempSetPoint = Node( PlantLoop( BLASTAbsorber( ChillNum ).CWLoopNum ).TempSetPointNodeNum ).TempSetPoint;
			Node( BLASTAbsorber( ChillNum ).EvapOutletNodeNum ).TempSetPointHi = Node( PlantLoop( BLASTAbsorber( ChillNum ).CWLoopNum ).TempSetPointNodeNum ).TempSetPointHi;
		}

		if ( ( MyLoad < 0.0 ) && RunFlag ) {
			mdotEvap = BLASTAbsorber( ChillNum ).EvapMassFlowRateMax;
			mdotCond = BLASTAbsorber( ChillNum ).CondMassFlowRateMax;
			mdotGen = BLASTAbsorber( ChillNum ).GenMassFlowRateMax;
		} else {
			mdotEvap = 0.0;
			mdotCond = 0.0;
			mdotGen = 0.0;
		}

		SetComponentFlowRate( mdotEvap, BLASTAbsorber( ChillNum ).EvapInletNodeNum, BLASTAbsorber( ChillNum ).EvapOutletNodeNum, BLASTAbsorber( ChillNum ).CWLoopNum, BLASTAbsorber( ChillNum ).CWLoopSideNum, BLASTAbsorber( ChillNum ).CWBranchNum, BLASTAbsorber( ChillNum ).CWCompNum );

		SetComponentFlowRate( mdotCond, CondInletNode, CondOutletNode, BLASTAbsorber( ChillNum ).CDLoopNum, BLASTAbsorber( ChillNum ).CDLoopSideNum, BLASTAbsorber( ChillNum ).CDBranchNum, BLASTAbsorber( ChillNum ).CDCompNum );

		if ( BLASTAbsorber( ChillNum ).GeneratorInletNodeNum > 0 ) {

			SetComponentFlowRate( mdotGen, BLASTAbsorber( ChillNum ).GeneratorInletNodeNum, BLASTAbsorber( ChillNum ).GeneratorOutletNodeNum, BLASTAbsorber( ChillNum ).GenLoopNum, BLASTAbsorber( ChillNum ).GenLoopSideNum, BLASTAbsorber( ChillNum ).GenBranchNum, BLASTAbsorber( ChillNum ).GenCompNum );

		}

	}

	void
	SizeAbsorpChiller( int const ChillNum )
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Fred Buhl
		//       DATE WRITTEN   March 2008
		//       MODIFIED:      R. Raustad May 2008 - added generator node sizing
		//                      November 2013 Daeho Kang, add component sizing table entries
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
		using DataPlant::MyPlantSizingIndex;
		using PlantUtilities::RegisterPlantCompDesignFlow;
		using ReportSizingManager::ReportSizingOutput;
		using namespace OutputReportPredefined;
		using namespace FluidProperties;
		//  USE BranchInputManager, ONLY: MyPlantSizingIndex

		// Locals
		Real64 SteamMassFlowRate; // steam mass flow rate through generator

		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "SizeAbsorpChiller" );
		static std::string const RoutineNameLong( "SizeAbsorptionChiller" );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int PltSizIndex; // Plant Sizing Do loop index
		int PltSizNum( 0 ); // Plant Sizing index corresponding to CurLoopNum
		int PltSizCondNum( 0 ); // Plant Sizing index for condenser loop
		int PltSizSteamNum( 0 ); // Plant Sizing index for steam heating loop
		int PltSizHeatingNum( 0 ); // Plant Sizing index for how water heating loop
		Real64 SteamInputRatNom; // nominal energy input ratio (steam or hot water)
		Real64 SteamDensity; // density of generator steam (when connected to a steam loop)
		Real64 EnthSteamOutDry; // dry enthalpy of steam (quality = 1)
		Real64 EnthSteamOutWet; // wet enthalpy of steam (quality = 0)
		Real64 HfgSteam; // latent heat of steam at constant pressure
		Real64 SteamDeltaT; // amount of sub-cooling of steam condensate
		Real64 CpWater; // specific heat of generator fluid (when connected to a hot water loop)
		Real64 RhoWater; // density of water
		Real64 GeneratorOutletTemp; // outlet temperature of generator
		bool ErrorsFound( false ); // If errors detected in input
		bool LoopErrorsFound;
		std::string equipName;
		Real64 rho; // local fluid density
		Real64 Cp; // local specific heat
		Real64 tmpNomCap; // local nominal capacity cooling power
		Real64 tmpNomPumpPower; // local nominal pump power
		Real64 tmpEvapVolFlowRate; // local evaporator design volume flow rate
		Real64 tmpCondVolFlowRate; // local condenser design volume flow rate
		Real64 tmpGeneratorVolFlowRate; // local generator design volume flow rate
		static int DummWaterIndex( 1 );

		Real64 NomCapUser( 0.0 ); // Hardsized nominal capacity for reporting
		Real64 NomPumpPowerUser( 0.0 ); // Hardsized nominal pump power for reporting
		Real64 EvapVolFlowRateUser( 0.0 ); // Hardsized evaporator volume flow rate for reporting
		Real64 CondVolFlowRateUser( 0.0 ); // Hardsized condenser flow rate for reporting
		Real64 GeneratorVolFlowRateUser( 0.0 ); // Hardsized generator flow rate for reporting

		SteamInputRatNom = BLASTAbsorber( ChillNum ).SteamLoadCoef( 1 ) + BLASTAbsorber( ChillNum ).SteamLoadCoef( 2 ) + BLASTAbsorber( ChillNum ).SteamLoadCoef( 3 );
		// init local temporary version in case of partial/mixed autosizing
		tmpNomCap = BLASTAbsorber( ChillNum ).NomCap;
		tmpNomPumpPower = BLASTAbsorber( ChillNum ).NomPumpPower;
		tmpEvapVolFlowRate = BLASTAbsorber( ChillNum ).EvapVolFlowRate;
		tmpCondVolFlowRate = BLASTAbsorber( ChillNum ).CondVolFlowRate;
		tmpGeneratorVolFlowRate = BLASTAbsorber( ChillNum ).GeneratorVolFlowRate;

		// find the appropriate Plant Sizing object
		PltSizNum = PlantLoop( BLASTAbsorber( ChillNum ).CWLoopNum ).PlantSizNum;
		PltSizCondNum = PlantLoop( BLASTAbsorber( ChillNum ).CDLoopNum ).PlantSizNum;

		if ( BLASTAbsorber( ChillNum ).GenHeatSourceType == NodeType_Steam ) {
			if ( BLASTAbsorber( ChillNum ).GeneratorInletNodeNum > 0 && BLASTAbsorber( ChillNum ).GeneratorOutletNodeNum > 0 ) {
				PltSizSteamNum = MyPlantSizingIndex( moduleObjectType, BLASTAbsorber( ChillNum ).Name, BLASTAbsorber( ChillNum ).GeneratorInletNodeNum, BLASTAbsorber( ChillNum ).GeneratorOutletNodeNum, LoopErrorsFound );
			} else {
				for ( PltSizIndex = 1; PltSizIndex <= NumPltSizInput; ++PltSizIndex ) {
					if ( PlantSizData( PltSizIndex ).LoopType == SteamLoop ) {
						PltSizSteamNum = PltSizIndex;
					}
				}
			}
		} else {
			if ( BLASTAbsorber( ChillNum ).GeneratorInletNodeNum > 0 && BLASTAbsorber( ChillNum ).GeneratorOutletNodeNum > 0 ) {
				PltSizHeatingNum = MyPlantSizingIndex( moduleObjectType, BLASTAbsorber( ChillNum ).Name, BLASTAbsorber( ChillNum ).GeneratorInletNodeNum, BLASTAbsorber( ChillNum ).GeneratorOutletNodeNum, LoopErrorsFound );
			} else {
				for ( PltSizIndex = 1; PltSizIndex <= NumPltSizInput; ++PltSizIndex ) {
					if ( PlantSizData( PltSizIndex ).LoopType == HeatingLoop ) {
						PltSizHeatingNum = PltSizIndex;
					}
				}
			}
		}

		if ( PltSizNum > 0 ) {
			if ( PlantSizData( PltSizNum ).DesVolFlowRate >= SmallWaterVolFlow ) {

				Cp = GetSpecificHeatGlycol( PlantLoop( BLASTAbsorber( ChillNum ).CWLoopNum ).FluidName, InitConvTemp, PlantLoop( BLASTAbsorber( ChillNum ).CWLoopNum ).FluidIndex, RoutineName );

				rho = GetDensityGlycol( PlantLoop( BLASTAbsorber( ChillNum ).CWLoopNum ).FluidName, InitConvTemp, PlantLoop( BLASTAbsorber( ChillNum ).CWLoopNum ).FluidIndex, RoutineName );
				tmpNomCap = Cp * rho * PlantSizData( PltSizNum ).DeltaT * PlantSizData( PltSizNum ).DesVolFlowRate * BLASTAbsorber( ChillNum ).SizFac;
				if ( ! BLASTAbsorber( ChillNum ).NomCapWasAutoSized ) tmpNomCap = BLASTAbsorber( ChillNum ).NomCap;
			} else {
				if ( BLASTAbsorber( ChillNum ).NomCapWasAutoSized ) tmpNomCap = 0.0;
			}
			if ( PlantFirstSizesOkayToFinalize ) {
				if ( BLASTAbsorber( ChillNum ).NomCapWasAutoSized ) {
					BLASTAbsorber( ChillNum ).NomCap = tmpNomCap;
					if ( PlantFinalSizesOkayToReport ) {
						ReportSizingOutput( moduleObjectType, BLASTAbsorber( ChillNum ).Name,
							"Design Size Nominal Capacity [W]", tmpNomCap );
					}
					if ( PlantFirstSizesOkayToReport ) {
						ReportSizingOutput( moduleObjectType, BLASTAbsorber( ChillNum ).Name,
							"Initial Design Size Nominal Capacity [W]", tmpNomCap );
					}
				} else {
					if ( BLASTAbsorber( ChillNum ).NomCap > 0.0 && tmpNomCap > 0.0 ) {
						NomCapUser = BLASTAbsorber( ChillNum ).NomCap;
						if ( PlantFinalSizesOkayToReport ) {
							ReportSizingOutput( moduleObjectType, BLASTAbsorber( ChillNum ).Name,
								"Design Size Nominal Capacity [W]", tmpNomCap,
								"User-Specified Nominal Capacity [W]", NomCapUser );
							if ( DisplayExtraWarnings ) {
								if ( ( std::abs( tmpNomCap - NomCapUser ) / NomCapUser ) > AutoVsHardSizingThreshold ) {
									ShowMessage( "SizeChillerAbsorption: Potential issue with equipment sizing for " + BLASTAbsorber( ChillNum ).Name );
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
			if ( BLASTAbsorber( ChillNum ).NomCapWasAutoSized && PlantFirstSizesOkayToFinalize ) {
				ShowSevereError( "Autosizing of Absorption Chiller nominal capacity requires a loop Sizing:Plant object" );
				ShowContinueError( "Occurs in Chiller:Absorption object=" + BLASTAbsorber( ChillNum ).Name );
				ErrorsFound = true;
			}
			if ( ! BLASTAbsorber( ChillNum ).NomCapWasAutoSized && PlantFinalSizesOkayToReport
					&& BLASTAbsorber( ChillNum ).NomCap > 0.0 ) {
				ReportSizingOutput( moduleObjectType, BLASTAbsorber( ChillNum ).Name,
					"User-Specified Nominal Capacity [W]", BLASTAbsorber( ChillNum ).NomCap );
			}

		}

		tmpNomPumpPower = 0.0045 * BLASTAbsorber( ChillNum ).NomCap;

		if ( PlantFirstSizesOkayToFinalize ) {
			// the DOE-2 EIR for single stage absorption chiller
			if ( BLASTAbsorber( ChillNum ).NomPumpPowerWasAutoSized ) {
				BLASTAbsorber( ChillNum ).NomPumpPower = tmpNomPumpPower;
				if ( PlantFinalSizesOkayToReport ) {
					ReportSizingOutput( moduleObjectType, BLASTAbsorber( ChillNum ).Name,
						"Design Size Nominal Pumping Power [W]", tmpNomPumpPower );
				}
				if ( PlantFirstSizesOkayToReport ) {
					ReportSizingOutput( moduleObjectType, BLASTAbsorber( ChillNum ).Name,
						"Initial Design Size Nominal Pumping Power [W]", tmpNomPumpPower );
				}
			} else {
				if ( BLASTAbsorber( ChillNum ).NomPumpPower > 0.0 && tmpNomPumpPower > 0.0 ) {
					NomPumpPowerUser = BLASTAbsorber( ChillNum ).NomPumpPower;
					if ( PlantFinalSizesOkayToReport ) {
						ReportSizingOutput( moduleObjectType, BLASTAbsorber( ChillNum ).Name,
							"Design Size Nominal Pumping Power [W]", tmpNomPumpPower,
							"User-Specified Nominal Pumping Power [W]", NomPumpPowerUser );
						if ( DisplayExtraWarnings ) {
							if ( ( std::abs( tmpNomPumpPower - NomPumpPowerUser ) / NomPumpPowerUser ) > AutoVsHardSizingThreshold ) {
								ShowMessage( "SizeChillerAbsorption: Potential issue with equipment sizing for " + BLASTAbsorber( ChillNum ).Name );
								ShowContinueError( "User-Specified Nominal Pumping Power of " + RoundSigDigits( NomPumpPowerUser, 2 ) + " [W]" );
								ShowContinueError( "differs from Design Size Nominal Pumping Power of " + RoundSigDigits( tmpNomPumpPower, 2 ) + " [W]" );
								ShowContinueError( "This may, or may not, indicate mismatched component sizes." );
								ShowContinueError( "Verify that the value entered is intended and is consistent with other components." );
							}
						}
					}
					tmpNomPumpPower = NomPumpPowerUser;
				}
			}
		}

		if ( PltSizNum > 0 ) {
			if ( PlantSizData( PltSizNum ).DesVolFlowRate >= SmallWaterVolFlow ) {
				tmpEvapVolFlowRate = PlantSizData( PltSizNum ).DesVolFlowRate * BLASTAbsorber( ChillNum ).SizFac;
				if ( ! BLASTAbsorber( ChillNum ).EvapVolFlowRateWasAutoSized ) tmpEvapVolFlowRate = BLASTAbsorber( ChillNum ).EvapVolFlowRate;
			} else {
				if ( BLASTAbsorber( ChillNum ).EvapVolFlowRateWasAutoSized ) tmpEvapVolFlowRate = 0.0;
			}
			if ( PlantFirstSizesOkayToFinalize ) {
				if ( BLASTAbsorber( ChillNum ).EvapVolFlowRateWasAutoSized ) {
					BLASTAbsorber( ChillNum ).EvapVolFlowRate = tmpEvapVolFlowRate;
					if ( PlantFinalSizesOkayToReport ) {
						ReportSizingOutput( moduleObjectType, BLASTAbsorber( ChillNum ).Name,
							"Design Size Design Chilled Water Flow Rate [m3/s]", tmpEvapVolFlowRate );
					}
					if ( PlantFirstSizesOkayToReport ) {
						ReportSizingOutput( moduleObjectType, BLASTAbsorber( ChillNum ).Name,
							"Initial Design Size Design Chilled Water Flow Rate [m3/s]", tmpEvapVolFlowRate );
					}
				} else {
					if ( BLASTAbsorber( ChillNum ).EvapVolFlowRate > 0.0 && tmpEvapVolFlowRate > 0.0 ) {
						EvapVolFlowRateUser = BLASTAbsorber( ChillNum ).EvapVolFlowRate;
						if ( PlantFinalSizesOkayToReport ) {
							ReportSizingOutput( moduleObjectType, BLASTAbsorber( ChillNum ).Name,
								"Design Size Design Chilled Water Flow Rate [m3/s]", tmpEvapVolFlowRate,
								"User-Specified Design Chilled Water Flow Rate [m3/s]", EvapVolFlowRateUser );
							if ( DisplayExtraWarnings ) {
								if ( ( std::abs( tmpEvapVolFlowRate - EvapVolFlowRateUser ) / EvapVolFlowRateUser ) > AutoVsHardSizingThreshold ) {
									ShowMessage( "SizeChillerAbsorption: Potential issue with equipment sizing for " + BLASTAbsorber( ChillNum ).Name );
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
			if ( BLASTAbsorber( ChillNum ).EvapVolFlowRateWasAutoSized && PlantFirstSizesOkayToFinalize ) {
				ShowSevereError( "Autosizing of Absorption Chiller evap flow rate requires a loop Sizing:Plant object" );
				ShowContinueError( "Occurs in CHILLER:ABSORPTION object=" + BLASTAbsorber( ChillNum ).Name );
				ErrorsFound = true;
			}
			if ( ! BLASTAbsorber( ChillNum ).EvapVolFlowRateWasAutoSized && PlantFinalSizesOkayToReport
					&& BLASTAbsorber( ChillNum ).EvapVolFlowRate > 0.0 ) {
					ReportSizingOutput( moduleObjectType, BLASTAbsorber( ChillNum ).Name,
						"User-Specified Design Chilled Water Flow Rate [m3/s]", BLASTAbsorber( ChillNum ).EvapVolFlowRate );
			}
		}

		RegisterPlantCompDesignFlow( BLASTAbsorber( ChillNum ).EvapInletNodeNum, tmpEvapVolFlowRate );

		if ( PltSizCondNum > 0 && PltSizNum > 0 ) {
			if ( BLASTAbsorber( ChillNum ).EvapVolFlowRate >= SmallWaterVolFlow && tmpNomCap > 0.0 ) {
				//       QCondenser = QEvaporator + QGenerator + PumpingPower

				Cp = GetSpecificHeatGlycol( PlantLoop( BLASTAbsorber( ChillNum ).CDLoopNum ).FluidName, BLASTAbsorber( ChillNum ).TempDesCondIn, PlantLoop( BLASTAbsorber( ChillNum ).CDLoopNum ).FluidIndex, RoutineName );

				rho = GetDensityGlycol( PlantLoop( BLASTAbsorber( ChillNum ).CDLoopNum ).FluidName, InitConvTemp, PlantLoop( BLASTAbsorber( ChillNum ).CDLoopNum ).FluidIndex, RoutineName );
				tmpCondVolFlowRate = tmpNomCap * ( 1.0 + SteamInputRatNom + tmpNomPumpPower / tmpNomCap ) / ( PlantSizData( PltSizCondNum ).DeltaT * Cp * rho );
				if ( ! BLASTAbsorber( ChillNum ).CondVolFlowRateWasAutoSized ) tmpCondVolFlowRate = BLASTAbsorber( ChillNum ).CondVolFlowRate;

			} else {
				if ( BLASTAbsorber( ChillNum ).CondVolFlowRateWasAutoSized ) tmpCondVolFlowRate = 0.0;
			}
			if ( PlantFirstSizesOkayToFinalize ) {
				if ( BLASTAbsorber( ChillNum ).CondVolFlowRateWasAutoSized ) {
					BLASTAbsorber( ChillNum ).CondVolFlowRate = tmpCondVolFlowRate;
					if ( PlantFinalSizesOkayToReport ) {
						ReportSizingOutput( moduleObjectType, BLASTAbsorber( ChillNum ).Name,
							"Design Size Design Condenser Water Flow Rate [m3/s]", tmpCondVolFlowRate );
					}
					if ( PlantFirstSizesOkayToReport ) {
						ReportSizingOutput( moduleObjectType, BLASTAbsorber( ChillNum ).Name,
							"Initial Design Size Design Condenser Water Flow Rate [m3/s]", tmpCondVolFlowRate );
					}
				} else {
					if ( BLASTAbsorber( ChillNum ).CondVolFlowRate > 0.0 && tmpCondVolFlowRate > 0.0 ) {
						CondVolFlowRateUser = BLASTAbsorber( ChillNum ).CondVolFlowRate;
						if ( PlantFinalSizesOkayToReport ) {
							ReportSizingOutput( moduleObjectType, BLASTAbsorber( ChillNum ).Name, "Design Size Design Condenser Water Flow Rate [m3/s]", tmpCondVolFlowRate, "User-Specified Design Condenser Water Flow Rate [m3/s]", CondVolFlowRateUser );
							if ( DisplayExtraWarnings ) {
								if ( ( std::abs( tmpCondVolFlowRate - CondVolFlowRateUser ) / CondVolFlowRateUser ) > AutoVsHardSizingThreshold ) {
									ShowMessage( "SizeChillerAbsorption: Potential issue with equipment sizing for " + BLASTAbsorber( ChillNum ).Name );
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
			if ( BLASTAbsorber( ChillNum ).CondVolFlowRateWasAutoSized && PlantFirstSizesOkayToFinalize ) {
				ShowSevereError( "Autosizing of Absorption Chiller condenser flow rate requires a condenser" );
				ShowContinueError( "loop Sizing:Plant object" );
				ShowContinueError( "Occurs in CHILLER:ABSORPTION object=" + BLASTAbsorber( ChillNum ).Name );
				ErrorsFound = true;
			}
			if ( ! BLASTAbsorber( ChillNum ).CondVolFlowRateWasAutoSized && PlantFirstSizesOkayToFinalize
					&&  ( BLASTAbsorber( ChillNum ).CondVolFlowRate > 0.0 ) ) {
					ReportSizingOutput( moduleObjectType, BLASTAbsorber( ChillNum ).Name,
						"User-Specified Design Condenser Water Flow Rate [m3/s]", BLASTAbsorber( ChillNum ).CondVolFlowRate );
			}
		}

		// save the design condenser water volumetric flow rate for use by the condenser water loop sizing algorithms
		RegisterPlantCompDesignFlow( BLASTAbsorber( ChillNum ).CondInletNodeNum, tmpCondVolFlowRate );

		if ( (PltSizSteamNum > 0 && BLASTAbsorber( ChillNum ).GenHeatSourceType == NodeType_Steam)
				|| (PltSizHeatingNum > 0 && BLASTAbsorber( ChillNum ).GenHeatSourceType == NodeType_Water) ) {
			if ( BLASTAbsorber( ChillNum ).EvapVolFlowRate >= SmallWaterVolFlow && tmpNomCap > 0.0 ) {
				if ( BLASTAbsorber( ChillNum ).GenHeatSourceType == NodeType_Water ) {
					CpWater = GetSpecificHeatGlycol( PlantLoop( BLASTAbsorber( ChillNum ).GenLoopNum ).FluidName, PlantSizData( PltSizHeatingNum ).ExitTemp, PlantLoop( BLASTAbsorber( ChillNum ).GenLoopNum ).FluidIndex, RoutineName );
					SteamDeltaT = max( 0.5, PlantSizData( PltSizHeatingNum ).DeltaT );
					RhoWater = GetDensityGlycol( PlantLoop( BLASTAbsorber( ChillNum ).GenLoopNum ).FluidName, ( PlantSizData( PltSizHeatingNum ).ExitTemp - SteamDeltaT ), PlantLoop( BLASTAbsorber( ChillNum ).GenLoopNum ).FluidIndex, RoutineName );
					tmpGeneratorVolFlowRate = ( BLASTAbsorber( ChillNum ).NomCap * SteamInputRatNom ) / ( CpWater * SteamDeltaT * RhoWater );
					if ( ! BLASTAbsorber( ChillNum ).GeneratorVolFlowRateWasAutoSized ) tmpGeneratorVolFlowRate = BLASTAbsorber( ChillNum ).GeneratorVolFlowRate;
					if ( PlantFirstSizesOkayToFinalize ) {
						if ( BLASTAbsorber( ChillNum ).GeneratorVolFlowRateWasAutoSized ) {
							BLASTAbsorber( ChillNum ).GeneratorVolFlowRate = tmpGeneratorVolFlowRate;
							if ( PlantFinalSizesOkayToReport ) {
								ReportSizingOutput( moduleObjectType, BLASTAbsorber( ChillNum ).Name,
									"Design Size Design Generator Fluid Flow Rate [m3/s]", tmpGeneratorVolFlowRate );
							}
							if ( PlantFirstSizesOkayToReport ) {
								ReportSizingOutput( moduleObjectType, BLASTAbsorber( ChillNum ).Name,
									"Iniital Design Size Design Generator Fluid Flow Rate [m3/s]", tmpGeneratorVolFlowRate );
							}
						} else {
							if ( BLASTAbsorber( ChillNum ).GeneratorVolFlowRate > 0.0 && tmpGeneratorVolFlowRate > 0.0 ) {
								GeneratorVolFlowRateUser = BLASTAbsorber( ChillNum ).GeneratorVolFlowRate;
								if ( PlantFinalSizesOkayToReport ) {
									ReportSizingOutput( moduleObjectType, BLASTAbsorber( ChillNum ).Name, "Design Size Design Generator Fluid Flow Rate [m3/s]", tmpGeneratorVolFlowRate, "User-Specified Design Generator Fluid Flow Rate [m3/s]", GeneratorVolFlowRateUser );
									if ( DisplayExtraWarnings ) {
										if ( ( std::abs( tmpGeneratorVolFlowRate - GeneratorVolFlowRateUser ) / GeneratorVolFlowRateUser ) > AutoVsHardSizingThreshold ) {
											ShowMessage( "SizeChillerAbsorption: Potential issue with equipment sizing for " + BLASTAbsorber( ChillNum ).Name );
											ShowContinueError( "User-Specified Design Generator Fluid Flow Rate of " + RoundSigDigits( GeneratorVolFlowRateUser, 5 ) + " [m3/s]" );
											ShowContinueError( "differs from Design Size Design Generator Fluid Flow Rate of " + RoundSigDigits( tmpGeneratorVolFlowRate, 5 ) + " [m3/s]" );
											ShowContinueError( "This may, or may not, indicate mismatched component sizes." );
											ShowContinueError( "Verify that the value entered is intended and is consistent with other components." );
										}
									}
								}
								tmpGeneratorVolFlowRate = GeneratorVolFlowRateUser;
							}
						}
					}
				} else {
					SteamDensity = GetSatDensityRefrig( fluidNameSteam, PlantSizData( PltSizSteamNum ).ExitTemp, 1.0, BLASTAbsorber( ChillNum ).SteamFluidIndex, RoutineNameLong );
					SteamDeltaT = PlantSizData( PltSizSteamNum ).DeltaT;
					GeneratorOutletTemp = PlantSizData( PltSizSteamNum ).ExitTemp - SteamDeltaT;

					EnthSteamOutDry = GetSatEnthalpyRefrig( fluidNameSteam, PlantSizData( PltSizSteamNum ).ExitTemp, 1.0, BLASTAbsorber( ChillNum ).SteamFluidIndex, moduleObjectType + BLASTAbsorber( ChillNum ).Name );
					EnthSteamOutWet = GetSatEnthalpyRefrig( fluidNameSteam, PlantSizData( PltSizSteamNum ).ExitTemp, 0.0, BLASTAbsorber( ChillNum ).SteamFluidIndex, moduleObjectType + BLASTAbsorber( ChillNum ).Name );
					CpWater = GetSpecificHeatGlycol( fluidNameWater, GeneratorOutletTemp, DummWaterIndex, RoutineName );
					HfgSteam = EnthSteamOutDry - EnthSteamOutWet;
					SteamMassFlowRate = ( BLASTAbsorber( ChillNum ).NomCap * SteamInputRatNom ) / ( ( HfgSteam ) + ( SteamDeltaT * CpWater ) );
					tmpGeneratorVolFlowRate = SteamMassFlowRate / SteamDensity;

					if ( ! BLASTAbsorber( ChillNum ).GeneratorVolFlowRateWasAutoSized ) tmpGeneratorVolFlowRate = BLASTAbsorber( ChillNum ).GeneratorVolFlowRate;
					if ( PlantFirstSizesOkayToFinalize ) {

						if ( BLASTAbsorber( ChillNum ).GeneratorVolFlowRateWasAutoSized ) {
							BLASTAbsorber( ChillNum ).GeneratorVolFlowRate = tmpGeneratorVolFlowRate;
							if ( PlantFinalSizesOkayToReport ) {
								ReportSizingOutput( moduleObjectType, BLASTAbsorber( ChillNum ).Name,
									"Design Size Design Generator Fluid Flow Rate [m3/s]", tmpGeneratorVolFlowRate );
							}
							if ( PlantFirstSizesOkayToReport ) {
								ReportSizingOutput( moduleObjectType, BLASTAbsorber( ChillNum ).Name,
									"Initial Design Size Design Generator Fluid Flow Rate [m3/s]", tmpGeneratorVolFlowRate );
							}
						} else {
							if ( BLASTAbsorber( ChillNum ).GeneratorVolFlowRate > 0.0 && tmpGeneratorVolFlowRate > 0.0 ) {
								GeneratorVolFlowRateUser = BLASTAbsorber( ChillNum ).GeneratorVolFlowRate;
								if ( PlantFinalSizesOkayToReport ) {
									ReportSizingOutput( moduleObjectType, BLASTAbsorber( ChillNum ).Name,
										"Design Size Design Generator Fluid Flow Rate [m3/s]", tmpGeneratorVolFlowRate,
										"User-Specified Design Generator Fluid Flow Rate [m3/s]", GeneratorVolFlowRateUser );
									if ( DisplayExtraWarnings ) {
										if ( ( std::abs( tmpGeneratorVolFlowRate - GeneratorVolFlowRateUser ) / GeneratorVolFlowRateUser ) > AutoVsHardSizingThreshold ) {
											ShowMessage( "SizeChillerAbsorption: Potential issue with equipment sizing for " + BLASTAbsorber( ChillNum ).Name );
											ShowContinueError( "User-Specified Design Generator Fluid Flow Rate of " + RoundSigDigits( GeneratorVolFlowRateUser, 5 ) + " [m3/s]" );
											ShowContinueError( "differs from Design Size Design Generator Fluid Flow Rate of " + RoundSigDigits( tmpGeneratorVolFlowRate, 5 ) + " [m3/s]" );
											ShowContinueError( "This may, or may not, indicate mismatched component sizes." );
											ShowContinueError( "Verify that the value entered is intended and is consistent with other components." );
										}
									}
								}
								tmpGeneratorVolFlowRate = GeneratorVolFlowRateUser;
							}
						}
					}
				}
			} else {
				if ( BLASTAbsorber( ChillNum ).GeneratorVolFlowRateWasAutoSized ) {
					if ( PlantFirstSizesOkayToFinalize ) {
						BLASTAbsorber( ChillNum ).GeneratorVolFlowRate = 0.0;
					} else {
						tmpGeneratorVolFlowRate = 0.0;
					}
				}
			}
		} else {
			if ( BLASTAbsorber( ChillNum ).GeneratorVolFlowRateWasAutoSized && PlantFirstSizesOkayToFinalize ) {
				ShowSevereError( "Autosizing of Absorption Chiller generator flow rate requires a loop Sizing:Plant object." );
				ShowContinueError( " For steam loops, use a steam Sizing:Plant object." );
				ShowContinueError( " For hot water loops, use a heating Sizing:Plant object." );
				ShowContinueError( "Occurs in Chiller:Absorption object=" + BLASTAbsorber( ChillNum ).Name );
				ErrorsFound = true;
			}
			if ( ! BLASTAbsorber( ChillNum ).GeneratorVolFlowRateWasAutoSized && PlantFinalSizesOkayToReport
					&& ( BLASTAbsorber( ChillNum ).GeneratorVolFlowRate > 0.0 ) ) {
					ReportSizingOutput( moduleObjectType, BLASTAbsorber( ChillNum ).Name,
						"User-Specified Design Generator Fluid Flow Rate [m3/s]", BLASTAbsorber( ChillNum ).GeneratorVolFlowRate );
			}
		}

		// save the design steam or hot water volumetric flow rate for use by the steam or hot water loop sizing algorithms
		if ( PlantFirstSizesOkayToFinalize ) {
			RegisterPlantCompDesignFlow( BLASTAbsorber( ChillNum ).GeneratorInletNodeNum, BLASTAbsorber( ChillNum ).GeneratorVolFlowRate );
		} else {
			RegisterPlantCompDesignFlow( BLASTAbsorber( ChillNum ).GeneratorInletNodeNum, tmpGeneratorVolFlowRate );
		}

		if ( BLASTAbsorber( ChillNum ).GeneratorDeltaTempWasAutoSized ) {
			if ( PltSizHeatingNum > 0 && BLASTAbsorber( ChillNum ).GenHeatSourceType == NodeType_Water ) {
				BLASTAbsorber( ChillNum ).GeneratorDeltaTemp = max( 0.5, PlantSizData( PltSizHeatingNum ).DeltaT );
			} else if ( BLASTAbsorber( ChillNum ).GenHeatSourceType == NodeType_Water ) {
				if ( PlantFirstSizesOkayToFinalize ) {
					Cp = GetSpecificHeatGlycol( PlantLoop( BLASTAbsorber( ChillNum ).GenLoopNum ).FluidName, InitConvTemp, PlantLoop( BLASTAbsorber( ChillNum ).GenLoopNum ).FluidIndex, RoutineName );
					rho = GetDensityGlycol( PlantLoop( BLASTAbsorber( ChillNum ).GenLoopNum ).FluidName, InitConvTemp, PlantLoop( BLASTAbsorber( ChillNum ).GenLoopNum ).FluidIndex, RoutineName );

					BLASTAbsorber( ChillNum ).GeneratorDeltaTemp = ( SteamInputRatNom * BLASTAbsorber( ChillNum ).NomCap ) / ( Cp * rho * BLASTAbsorber( ChillNum ).GeneratorVolFlowRate );
				}
			}
		}

		if ( ErrorsFound ) {
			ShowFatalError( "Preceding sizing errors cause program termination" );
		}

		if ( PlantFinalSizesOkayToReport ) {
			//create predefined report
			equipName = BLASTAbsorber( ChillNum ).Name;
			PreDefTableEntry( pdchMechType, equipName, moduleObjectType );
			PreDefTableEntry( pdchMechNomEff, equipName, "n/a" );
			PreDefTableEntry( pdchMechNomCap, equipName, BLASTAbsorber( ChillNum ).NomCap );
		}

	}

	// Beginning of Absorber model Subroutines
	// *****************************************************************************

	void
	CalcBLASTAbsorberModel(
		int & ChillNum, // Absorber number
		Real64 & MyLoad, // operating load
		bool const RunFlag, // TRUE when Absorber operating
		bool const EP_UNUSED( FirstIteration ), // TRUE when first iteration of timestep !unused1208
		int const EquipFlowCtrl // Flow control mode for the equipment
	)
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         Dan Fisher
		//       DATE WRITTEN   Sept. 1998
		//       MODIFIED       April 1999, May 2000- Taecheol Kim
		//                      May   2008 - R. Raustad, added generator nodes
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// simulate a vapor compression Absorber using the BLAST model

		// METHODOLOGY EMPLOYED:
		// curve fit of performance data:

		// REFERENCES:
		// 1.  BLAST User Manual
		// 2.  Absorber User Manual

		// Using/Aliasing
		using DataGlobals::BeginEnvrnFlag;
		using DataGlobals::SecInHour;
		using DataHVACGlobals::TimeStepSys;
		using DataPlant::DeltaTempTol;
		using DataPlant::PlantLoop;
		using DataPlant::CompSetPtBasedSchemeType;
		using DataPlant::SingleSetPoint;
		using DataPlant::DualSetPointDeadBand;
		using DataBranchAirLoopPlant::ControlType_SeriesActive;
		using DataBranchAirLoopPlant::MassFlowTolerance;
		using namespace FluidProperties;
		using General::TrimSigDigits;
		using PlantUtilities::SetComponentFlowRate;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "CalcBLASTAbsorberModel" );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Array1D< Real64 > SteamLoadFactor( 3 ); // coefficients to poly curve fit
		Array1D< Real64 > ElectricLoadFactor( 3 ); // coefficients to poly curve fit
		Real64 MinPartLoadRat; // min allowed operating frac full load
		Real64 MaxPartLoadRat; // max allowed operating frac full load
		Real64 TempCondIn; // C - (BLAST ADJTC(1)The design secondary loop fluid
		Real64 TempCondInDesign; // C - (BLAST ADJTC(1)The design secondary loop fluid
		Real64 EvapInletTemp; // C - evaporator inlet temperature, water side
		Real64 CondInletTemp; // C - condenser inlet temperature, water side
		Real64 TempEvapOut; // C - evaporator outlet temperature, water side
		Real64 TempEvapOutSetPoint( 0.0 ); // C - evaporator outlet temperature setpoint
		Real64 AbsorberNomCap; // Absorber nominal capacity
		Real64 NomPumpPower; // Absorber nominal pumping power
		Real64 PartLoadRat; // part load ratio for efficiency calc
		Real64 OperPartLoadRat; // Operating part load ratio
		Real64 EvapDeltaTemp( 0.0 ); // C - evaporator temperature difference, water side
		Real64 TempLowLimitEout; // C - Evaporator low temp. limit cut off
		Real64 SteamInputRat; // energy input ratio
		Real64 ElectricInputRat; // energy input ratio
		int EvapInletNode; // evaporator inlet node number, water side
		int EvapOutletNode; // evaporator outlet node number, water side
		int CondInletNode; // condenser inlet node number, water side
		int CondOutletNode; // condenser outlet node number, water side
		int GeneratorInletNode; // generator inlet node number, steam/water side
		int GeneratorOutletNode; // generator outlet node number, steam/water side
		Real64 EnthSteamOutDry; // enthalpy of dry steam at generator inlet
		Real64 EnthSteamOutWet; // enthalpy of wet steam at generator inlet
		Real64 HfgSteam; // heat of vaporization of steam
		static Array1D_bool MyEnvironFlag;
		static Array1D_bool MyEnvironSteamFlag;
		Real64 FRAC;
		//  LOGICAL,SAVE           :: PossibleSubcooling
		Real64 CpFluid; // local specific heat of fluid
		Real64 SteamDeltaT;
		Real64 SteamOutletTemp;
		int LoopNum;
		int LoopSideNum;
		static int DummyWaterIndex( 1 );

		//set module level inlet and outlet nodes
		EvapMassFlowRate = 0.0;
		CondMassFlowRate = 0.0;
		SteamMassFlowRate = 0.0;
		QCondenser = 0.0;
		QEvaporator = 0.0;
		QGenerator = 0.0;
		PumpingEnergy = 0.0;
		CondenserEnergy = 0.0;
		EvaporatorEnergy = 0.0;
		GeneratorEnergy = 0.0;
		PumpingPower = 0.0;
		FRAC = 1.0;
		EvapInletNode = BLASTAbsorber( ChillNum ).EvapInletNodeNum;
		EvapOutletNode = BLASTAbsorber( ChillNum ).EvapOutletNodeNum;
		CondInletNode = BLASTAbsorber( ChillNum ).CondInletNodeNum;
		CondOutletNode = BLASTAbsorber( ChillNum ).CondOutletNodeNum;
		GeneratorInletNode = BLASTAbsorber( ChillNum ).GeneratorInletNodeNum;
		GeneratorOutletNode = BLASTAbsorber( ChillNum ).GeneratorOutletNodeNum;

		//If no loop demand or Absorber OFF, return
		if ( MyLoad >= 0.0 || ! RunFlag ) { //off or heating
			if ( EquipFlowCtrl == ControlType_SeriesActive ) EvapMassFlowRate = Node( EvapInletNode ).MassFlowRate;
			return;
		}

		//set module level Absorber inlet and temperature variables
		EvapInletTemp = Node( EvapInletNode ).Temp;
		CondInletTemp = Node( CondInletNode ).Temp;

		//Set the condenser mass flow rates
		CondMassFlowRate = Node( CondInletNode ).MassFlowRate;

		//  LOAD LOCAL VARIABLES FROM DATA STRUCTURE (for code readability)
		SteamLoadFactor = BLASTAbsorber( ChillNum ).SteamLoadCoef;
		ElectricLoadFactor = BLASTAbsorber( ChillNum ).PumpPowerCoef;
		MinPartLoadRat = BLASTAbsorber( ChillNum ).MinPartLoadRat;
		MaxPartLoadRat = BLASTAbsorber( ChillNum ).MaxPartLoadRat;
		TempCondInDesign = BLASTAbsorber( ChillNum ).TempDesCondIn;
		AbsorberNomCap = BLASTAbsorber( ChillNum ).NomCap;
		NomPumpPower = BLASTAbsorber( ChillNum ).NomPumpPower;
		TempCondIn = Node( BLASTAbsorber( ChillNum ).CondInletNodeNum ).Temp;
		TempEvapOut = Node( BLASTAbsorber( ChillNum ).EvapOutletNodeNum ).Temp;
		TempLowLimitEout = BLASTAbsorber( ChillNum ).TempLowLimitEvapOut;
		LoopNum = BLASTAbsorber( ChillNum ).CWLoopNum;
		LoopSideNum = BLASTAbsorber( ChillNum ).CWLoopSideNum;

		CpFluid = GetSpecificHeatGlycol( PlantLoop( BLASTAbsorber( ChillNum ).CWLoopNum ).FluidName, EvapInletTemp, PlantLoop( BLASTAbsorber( ChillNum ).CWLoopNum ).FluidIndex, RoutineName );

		// If FlowLock is True, the new resolved mdot is used to update Power, QEvap, Qcond, and
		// condenser side outlet temperature.
		if ( PlantLoop( LoopNum ).LoopSide( LoopSideNum ).FlowLock == 0 ) {
			BLASTAbsorber( ChillNum ).PossibleSubcooling = false;
			QEvaporator = std::abs( MyLoad );
			// limit by max capacity
			QEvaporator = min( QEvaporator, ( BLASTAbsorber( ChillNum ).MaxPartLoadRat * BLASTAbsorber( ChillNum ).NomCap ) );

			// Either set the flow to the Constant value or caluclate the flow for the variable volume
			if ( ( BLASTAbsorber( ChillNum ).FlowMode == ConstantFlow ) || ( BLASTAbsorber( ChillNum ).FlowMode == NotModulated ) ) {
				EvapMassFlowRate = Node( EvapInletNode ).MassFlowRate;

				if ( EvapMassFlowRate != 0.0 ) {

					EvapDeltaTemp = QEvaporator / EvapMassFlowRate / CpFluid;
				} else {
					EvapDeltaTemp = 0.0;
				}
				EvapOutletTemp = EvapInletTemp - EvapDeltaTemp;

			} else if ( BLASTAbsorber( ChillNum ).FlowMode == LeavingSetPointModulated ) {
				// Calculate the Delta Temp from the inlet temp to the chiller outlet setpoint
				{ auto const SELECT_CASE_var( PlantLoop( BLASTAbsorber( ChillNum ).CWLoopNum ).LoopDemandCalcScheme );
				if ( SELECT_CASE_var == SingleSetPoint ) {
					EvapDeltaTemp = Node( EvapInletNode ).Temp - Node( EvapOutletNode ).TempSetPoint;
				} else if ( SELECT_CASE_var == DualSetPointDeadBand ) {
					EvapDeltaTemp = Node( EvapInletNode ).Temp - Node( EvapOutletNode ).TempSetPointHi;
				} else {
					assert( false );
				}}
				if ( EvapDeltaTemp != 0 ) {

					EvapMassFlowRate = std::abs( QEvaporator / CpFluid / EvapDeltaTemp );
					if ( ( EvapMassFlowRate - BLASTAbsorber( ChillNum ).EvapMassFlowRateMax ) > MassFlowTolerance ) BLASTAbsorber( ChillNum ).PossibleSubcooling = true;
					//Check to see if the Maximum is exceeded, if so set to maximum
					EvapMassFlowRate = min( BLASTAbsorber( ChillNum ).EvapMassFlowRateMax, EvapMassFlowRate );
					SetComponentFlowRate( EvapMassFlowRate, BLASTAbsorber( ChillNum ).EvapInletNodeNum, BLASTAbsorber( ChillNum ).EvapOutletNodeNum, BLASTAbsorber( ChillNum ).CWLoopNum, BLASTAbsorber( ChillNum ).CWLoopSideNum, BLASTAbsorber( ChillNum ).CWBranchNum, BLASTAbsorber( ChillNum ).CWCompNum );
					{ auto const SELECT_CASE_var( PlantLoop( BLASTAbsorber( ChillNum ).CWLoopNum ).LoopDemandCalcScheme );
					if ( SELECT_CASE_var == SingleSetPoint ) {
						EvapOutletTemp = Node( EvapOutletNode ).TempSetPoint;
					} else if ( SELECT_CASE_var == DualSetPointDeadBand ) {
						EvapOutletTemp = Node( EvapOutletNode ).TempSetPointHi;
					}}
				} else {
					EvapMassFlowRate = 0.0;

					EvapOutletTemp = Node( EvapInletNode ).Temp;

					ShowRecurringWarningErrorAtEnd( "CalcBLASTAbsorberModel: Name=\"" + BLASTAbsorber( ChillNum ).Name + "\" Evaporative Condenser Delta Temperature = 0 in mass flow calculation.", BLASTAbsorber( ChillNum ).ErrCount2 );
				}
			} //End of Constant Variable Flow If Block
		} else { // If FlowLock is True

			EvapMassFlowRate = Node( EvapInletNode ).MassFlowRate;
			if ( BLASTAbsorber( ChillNum ).PossibleSubcooling ) {
				QEvaporator = std::abs( MyLoad );
				EvapDeltaTemp = QEvaporator / EvapMassFlowRate / CpFluid;
				EvapOutletTemp = Node( EvapInletNode ).Temp - EvapDeltaTemp;
			} else {
				{ auto const SELECT_CASE_var( PlantLoop( LoopNum ).LoopDemandCalcScheme );
				if ( SELECT_CASE_var == SingleSetPoint ) {
					if ( ( BLASTAbsorber( ChillNum ).FlowMode == LeavingSetPointModulated ) || ( PlantLoop( LoopNum ).LoopSide( LoopSideNum ).Branch( BLASTAbsorber( ChillNum ).CWBranchNum ).Comp( BLASTAbsorber( ChillNum ).CWCompNum ).CurOpSchemeType == CompSetPtBasedSchemeType ) || ( Node( EvapOutletNode ).TempSetPoint != SensedNodeFlagValue ) ) {
						TempEvapOutSetPoint = Node( EvapOutletNode ).TempSetPoint;
					} else {
						TempEvapOutSetPoint = Node( PlantLoop( LoopNum ).TempSetPointNodeNum ).TempSetPoint;
					}
				} else if ( SELECT_CASE_var == DualSetPointDeadBand ) {
					if ( ( BLASTAbsorber( ChillNum ).FlowMode == LeavingSetPointModulated ) || ( PlantLoop( LoopNum ).LoopSide( LoopSideNum ).Branch( BLASTAbsorber( ChillNum ).CWBranchNum ).Comp( BLASTAbsorber( ChillNum ).CWCompNum ).CurOpSchemeType == CompSetPtBasedSchemeType ) || ( Node( EvapOutletNode ).TempSetPointHi != SensedNodeFlagValue ) ) {
						TempEvapOutSetPoint = Node( EvapOutletNode ).TempSetPointHi;
					} else {
						TempEvapOutSetPoint = Node( PlantLoop( LoopNum ).TempSetPointNodeNum ).TempSetPointHi;
					}
				} else {
					assert( false );
				}}
				EvapDeltaTemp = Node( EvapInletNode ).Temp - TempEvapOutSetPoint;
				QEvaporator = std::abs( EvapMassFlowRate * CpFluid * EvapDeltaTemp );
				EvapOutletTemp = TempEvapOutSetPoint;
			}
			//Check that the Evap outlet temp honors both plant loop temp low limit and also the chiller low limit
			if ( EvapOutletTemp < TempLowLimitEout ) {
				if ( ( Node( EvapInletNode ).Temp - TempLowLimitEout ) > DeltaTempTol ) {
					EvapOutletTemp = TempLowLimitEout;
					EvapDeltaTemp = Node( EvapInletNode ).Temp - EvapOutletTemp;
					QEvaporator = EvapMassFlowRate * CpFluid * EvapDeltaTemp;
				} else {
					EvapOutletTemp = Node( EvapInletNode ).Temp;
					EvapDeltaTemp = Node( EvapInletNode ).Temp - EvapOutletTemp;
					QEvaporator = EvapMassFlowRate * CpFluid * EvapDeltaTemp;
				}
			}
			if ( EvapOutletTemp < Node( EvapOutletNode ).TempMin ) {
				if ( ( Node( EvapInletNode ).Temp - Node( EvapOutletNode ).TempMin ) > DeltaTempTol ) {
					EvapOutletTemp = Node( EvapOutletNode ).TempMin;
					EvapDeltaTemp = Node( EvapInletNode ).Temp - EvapOutletTemp;
					QEvaporator = EvapMassFlowRate * CpFluid * EvapDeltaTemp;
				} else {
					EvapOutletTemp = Node( EvapInletNode ).Temp;
					EvapDeltaTemp = Node( EvapInletNode ).Temp - EvapOutletTemp;
					QEvaporator = EvapMassFlowRate * CpFluid * EvapDeltaTemp;
				}
			}
			// Checks QEvaporator on the basis of the machine limits.
			if ( QEvaporator > std::abs( MyLoad ) ) {
				if ( EvapMassFlowRate > MassFlowTolerance ) {
					QEvaporator = std::abs( MyLoad );
					EvapDeltaTemp = QEvaporator / EvapMassFlowRate / CpFluid;
					EvapOutletTemp = Node( EvapInletNode ).Temp - EvapDeltaTemp;
				} else {
					QEvaporator = 0.0;
					EvapOutletTemp = Node( EvapInletNode ).Temp;
				}
			}

		} //This is the end of the FlowLock Block

		//Calculate part load ratio for efficiency calcs. If this part load ratio is greater than
		//Min PLR it will be used for calculations too.
		PartLoadRat = max( MinPartLoadRat, min( QEvaporator / AbsorberNomCap, MaxPartLoadRat ) );

		//In case MyLoad is less than the Min PLR load, the power and steam input should be adjusted
		//for cycling. The ratios used however are based on MinPLR.
		OperPartLoadRat = QEvaporator / AbsorberNomCap;

		if ( OperPartLoadRat < PartLoadRat ) {
			FRAC = min( 1.0, OperPartLoadRat / MinPartLoadRat );
		} else {
			FRAC = 1.0;
		}

		//Calculate steam input ratio
		SteamInputRat = SteamLoadFactor( 1 ) / PartLoadRat + SteamLoadFactor( 2 ) + SteamLoadFactor( 3 ) * PartLoadRat;

		//Calculate electric input ratio
		ElectricInputRat = ElectricLoadFactor( 1 ) + ElectricLoadFactor( 2 ) * PartLoadRat + ElectricLoadFactor( 3 ) * pow_2( PartLoadRat );

		//Calculate electric energy input
		PumpingPower = ElectricInputRat * NomPumpPower * FRAC;

		//Calculate steam load
		QGenerator = SteamInputRat * QEvaporator * FRAC;

		if ( EvapMassFlowRate == 0.0 ) {
			QGenerator = 0.0;
			EvapOutletTemp = Node( EvapInletNode ).Temp;
			PumpingPower = 0.0;
		}

		QCondenser = QEvaporator + QGenerator + PumpingPower;

		CpFluid = GetSpecificHeatGlycol( PlantLoop( BLASTAbsorber( ChillNum ).CDLoopNum ).FluidName, CondInletTemp, PlantLoop( BLASTAbsorber( ChillNum ).CDLoopNum ).FluidIndex, RoutineName );

		if ( CondMassFlowRate > MassFlowTolerance ) {
			CondOutletTemp = QCondenser / CondMassFlowRate / CpFluid + CondInletTemp;
		} else {

			CondOutletTemp = CondInletTemp;
			CondMassFlowRate = 0.0;
			QCondenser = 0.0;
			return;
			// V7 plant upgrade, no longer fatal here anymore, set some things and return
		}

		if ( GeneratorInletNode > 0 ) {
			//         Hot water plant is used for the generator
			if ( BLASTAbsorber( ChillNum ).GenHeatSourceType == NodeType_Water ) {
				CpFluid = GetSpecificHeatGlycol( PlantLoop( BLASTAbsorber( ChillNum ).GenLoopNum ).FluidName, Node( GeneratorInletNode ).Temp, PlantLoop( BLASTAbsorber( ChillNum ).GenLoopNum ).FluidIndex, RoutineName );
				if ( ( BLASTAbsorber( ChillNum ).FlowMode == ConstantFlow ) || ( BLASTAbsorber( ChillNum ).FlowMode == NotModulated ) ) {
					SteamMassFlowRate = BLASTAbsorber( ChillNum ).GenMassFlowRateMax;
				} else {
					SteamMassFlowRate = QGenerator / CpFluid / BLASTAbsorber( ChillNum ).GeneratorDeltaTemp;
				}

				SetComponentFlowRate( SteamMassFlowRate, GeneratorInletNode, GeneratorOutletNode, BLASTAbsorber( ChillNum ).GenLoopNum, BLASTAbsorber( ChillNum ).GenLoopSideNum, BLASTAbsorber( ChillNum ).GenBranchNum, BLASTAbsorber( ChillNum ).GenCompNum );

				if ( SteamMassFlowRate <= 0.0 ) {
					GenOutletTemp = Node( GeneratorInletNode ).Temp;
					SteamOutletEnthalpy = Node( GeneratorInletNode ).Enthalpy;
				} else {
					GenOutletTemp = Node( GeneratorInletNode ).Temp - QGenerator / ( CpFluid * SteamMassFlowRate );
					SteamOutletEnthalpy = Node( GeneratorInletNode ).Enthalpy - QGenerator / SteamMassFlowRate;
				}

			} else { // using a steam plant for the generator

				EnthSteamOutDry = GetSatEnthalpyRefrig( fluidNameSteam, Node( GeneratorInletNode ).Temp, 1.0, BLASTAbsorber( ChillNum ).SteamFluidIndex, calcChillerAbsorption + BLASTAbsorber( ChillNum ).Name );
				EnthSteamOutWet = GetSatEnthalpyRefrig( fluidNameSteam, Node( GeneratorInletNode ).Temp, 0.0, BLASTAbsorber( ChillNum ).SteamFluidIndex, calcChillerAbsorption + BLASTAbsorber( ChillNum ).Name );
				SteamDeltaT = BLASTAbsorber( ChillNum ).GeneratorSubcool;
				SteamOutletTemp = Node( GeneratorInletNode ).Temp - SteamDeltaT;
				HfgSteam = EnthSteamOutDry - EnthSteamOutWet;
				CpFluid = GetSpecificHeatGlycol( fluidNameWater, SteamOutletTemp, DummyWaterIndex, calcChillerAbsorption + BLASTAbsorber( ChillNum ).Name );
				SteamMassFlowRate = QGenerator / ( HfgSteam + CpFluid * SteamDeltaT );
				SetComponentFlowRate( SteamMassFlowRate, GeneratorInletNode, GeneratorOutletNode, BLASTAbsorber( ChillNum ).GenLoopNum, BLASTAbsorber( ChillNum ).GenLoopSideNum, BLASTAbsorber( ChillNum ).GenBranchNum, BLASTAbsorber( ChillNum ).GenCompNum );

				if ( SteamMassFlowRate <= 0.0 ) {
					GenOutletTemp = Node( GeneratorInletNode ).Temp;
					SteamOutletEnthalpy = Node( GeneratorInletNode ).Enthalpy;
				} else {
					GenOutletTemp = Node( GeneratorInletNode ).Temp - SteamDeltaT;
					SteamOutletEnthalpy = GetSatEnthalpyRefrig( fluidNameSteam, GenOutletTemp, 0.0, BLASTAbsorber( ChillNum ).SteamFluidIndex, moduleObjectType + BLASTAbsorber( ChillNum ).Name );
					SteamOutletEnthalpy -= CpFluid * SteamDeltaT;
				}

			}
		} // IF(GeneratorInletNode .GT. 0)THEN

		//convert power to energy
		GeneratorEnergy = QGenerator * TimeStepSys * SecInHour;
		EvaporatorEnergy = QEvaporator * TimeStepSys * SecInHour;
		CondenserEnergy = QCondenser * TimeStepSys * SecInHour;
		PumpingEnergy = PumpingPower * TimeStepSys * SecInHour;

	}

	// End of Absorption Chiller Module Utility Subroutines
	// *****************************************************************************

	// Beginning of Record Keeping subroutines for the Absorption Chiller Module
	// *****************************************************************************

	void
	UpdateBLASTAbsorberRecords(
		Real64 const MyLoad, // current load
		bool const RunFlag, // TRUE if Absorber operating
		int const Num // Absorber number
	)
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR:          Dan Fisher
		//       DATE WRITTEN:    October 1998

		// PURPOSE OF THIS SUBROUTINE:
		// reporting

		// METHODOLOGY EMPLOYED: na

		// REFERENCES: na

		// USE STATEMENTS: na
		// Using/Aliasing
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
		int GeneratorInletNode; // generator inlet node number, steam/water side
		int GeneratorOutletNode; // generator outlet node number, steam/water side

		EvapInletNode = BLASTAbsorber( Num ).EvapInletNodeNum;
		EvapOutletNode = BLASTAbsorber( Num ).EvapOutletNodeNum;
		CondInletNode = BLASTAbsorber( Num ).CondInletNodeNum;
		CondOutletNode = BLASTAbsorber( Num ).CondOutletNodeNum;
		GeneratorInletNode = BLASTAbsorber( Num ).GeneratorInletNodeNum;
		GeneratorOutletNode = BLASTAbsorber( Num ).GeneratorOutletNodeNum;

		if ( MyLoad >= 0 || ! RunFlag ) {
			//set node conditions
			SafeCopyPlantNode( EvapInletNode, EvapOutletNode );
			SafeCopyPlantNode( CondInletNode, CondOutletNode );

			BLASTAbsorberReport( Num ).PumpingPower = 0.0;
			BLASTAbsorberReport( Num ).QEvap = 0.0;
			BLASTAbsorberReport( Num ).QCond = 0.0;
			BLASTAbsorberReport( Num ).QGenerator = 0.0;
			BLASTAbsorberReport( Num ).PumpingEnergy = 0.0;
			BLASTAbsorberReport( Num ).EvapEnergy = 0.0;
			BLASTAbsorberReport( Num ).CondEnergy = 0.0;
			BLASTAbsorberReport( Num ).GeneratorEnergy = 0.0;
			BLASTAbsorberReport( Num ).EvapInletTemp = Node( EvapInletNode ).Temp;
			BLASTAbsorberReport( Num ).CondInletTemp = Node( CondInletNode ).Temp;
			BLASTAbsorberReport( Num ).CondOutletTemp = Node( CondOutletNode ).Temp;
			BLASTAbsorberReport( Num ).EvapOutletTemp = Node( EvapOutletNode ).Temp;
			BLASTAbsorberReport( Num ).Evapmdot = 0.0;
			BLASTAbsorberReport( Num ).Condmdot = 0.0;
			BLASTAbsorberReport( Num ).Genmdot = 0.0;
			BLASTAbsorberReport( Num ).ActualCOP = 0.0;

			if ( GeneratorInletNode > 0 ) {
				SafeCopyPlantNode( GeneratorInletNode, GeneratorOutletNode );
			}

		} else {
			//set node conditions
			SafeCopyPlantNode( EvapInletNode, EvapOutletNode );
			SafeCopyPlantNode( CondInletNode, CondOutletNode );
			Node( EvapOutletNode ).Temp = EvapOutletTemp;
			Node( CondOutletNode ).Temp = CondOutletTemp;

			BLASTAbsorberReport( Num ).PumpingPower = PumpingPower;
			BLASTAbsorberReport( Num ).QEvap = QEvaporator;
			BLASTAbsorberReport( Num ).QCond = QCondenser;
			BLASTAbsorberReport( Num ).QGenerator = QGenerator;
			BLASTAbsorberReport( Num ).PumpingEnergy = PumpingEnergy;
			BLASTAbsorberReport( Num ).EvapEnergy = EvaporatorEnergy;
			BLASTAbsorberReport( Num ).CondEnergy = CondenserEnergy;
			BLASTAbsorberReport( Num ).GeneratorEnergy = GeneratorEnergy;
			BLASTAbsorberReport( Num ).EvapInletTemp = Node( EvapInletNode ).Temp;
			BLASTAbsorberReport( Num ).CondInletTemp = Node( CondInletNode ).Temp;
			BLASTAbsorberReport( Num ).CondOutletTemp = Node( CondOutletNode ).Temp;
			BLASTAbsorberReport( Num ).EvapOutletTemp = Node( EvapOutletNode ).Temp;
			BLASTAbsorberReport( Num ).Evapmdot = EvapMassFlowRate;
			BLASTAbsorberReport( Num ).Condmdot = CondMassFlowRate;
			BLASTAbsorberReport( Num ).Genmdot = SteamMassFlowRate;
			if ( QGenerator != 0.0 ) {
				BLASTAbsorberReport( Num ).ActualCOP = QEvaporator / QGenerator;
			} else {
				BLASTAbsorberReport( Num ).ActualCOP = 0.0;
			}

			if ( GeneratorInletNode > 0 ) {
				SafeCopyPlantNode( GeneratorInletNode, GeneratorOutletNode );
				Node( GeneratorOutletNode ).Temp = GenOutletTemp;
			}

		}

	}

	// End of Record Keeping subroutines for the Absorption Chiller Module
	// *****************************************************************************

} // ChillerAbsorption

} // EnergyPlus
