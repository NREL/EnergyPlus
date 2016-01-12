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
#include <ChillerIndirectAbsorption.hh>
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
#include <ReportSizingManager.hh>
#include <UtilityRoutines.hh>

namespace EnergyPlus {

namespace ChillerIndirectAbsorption {

	// MODULE INFORMATION:
	//       AUTHOR         R. Raustad (FSEC)
	//       DATE WRITTEN   May 2008
	//       MODIFIED       na
	//       RE-ENGINEERED  na

	// PURPOSE OF THIS MODULE:
	// This module simulates the performance of the revised BLAST
	// absorbers. New curve objects are included.

	// METHODOLOGY EMPLOYED:
	// Once the PlantLoopManager determines that the revised BLAST absorber
	// is available to meet a loop cooling demand, it calls SimIndirectAbsorber
	// which in turn calls the appropriate Indirect Absorption Chiller model.
	// All Absorption Chiller models are based on a polynomial fit of Absorber
	// performance data.

	// REFERENCES:
	// 1. BLAST Users Manual

	// OTHER NOTES:
	// Manufacturers performance data can be used to generate the coefficients for the model.

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
	static std::string const BlankString;
	static std::string const fluidNameSteam( "STEAM" );
	static std::string const fluidNameWater( "WATER" );
	static std::string const calcChillerAbsorptionIndirect( "CALC Chiller:Absorption:Indirect " );

	// DERIVED TYPE DEFINITIONS:

	// MODULE VARIABLE DECLARATIONS:
	int NumIndirectAbsorbers( 0 ); // number of Absorption Chillers specified in input
	Real64 CondMassFlowRate( 0.0 ); // Kg/s - condenser mass flow rate, water side
	Real64 EvapMassFlowRate( 0.0 ); // Kg/s - evaporator mass flow rate, water side
	Real64 GenMassFlowRate( 0.0 ); // Kg/s - steam mass flow rate, water side
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
	Real64 EnergyLossToEnvironment( 0.0 ); // J - piping energy loss from generator outlet to pump inlet
	Real64 ChillerONOFFCyclingFrac( 0.0 ); // fraction of time chiller is on

	// SUBROUTINE SPECIFICATIONS FOR MODULE:

	// Object Data
	Array1D< IndirectAbsorberSpecs > IndirectAbsorber; // dimension to number of machines
	Array1D< ReportVars > IndirectAbsorberReport;

	// MODULE SUBROUTINES:

	// Beginning of Absorption Chiller Module Driver Subroutines
	//*************************************************************************

	// Functions

	void
	SimIndirectAbsorber(
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
		Real64 & MaxCap, // W - maximum operating capacity of Absorber
		Real64 & MinCap, // W - minimum operating capacity of Absorber
		Real64 & OptCap, // W - optimal operating capacity of Absorber
		bool const GetSizingFactor, // TRUE when just the sizing factor is requested
		Real64 & SizingFactor, // sizing factor
		Real64 & TempCondInDesign
	)
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         R. Raustad (FSEC)
		//       DATE WRITTEN   May 2008
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE: This is the Indirect Absorption Chiller model driver.  It
		// gets the input for the models, initializes simulation variables, call
		// the appropriate model and sets up reporting variables.

		// METHODOLOGY EMPLOYED: na

		// REFERENCES: na

		// Using/Aliasing
		using InputProcessor::FindItemInList;
		using PlantUtilities::UpdateChillerComponentCondenserSide;
		using PlantUtilities::UpdateAbsorberChillerComponentGeneratorSide;
		using DataPlant::TypeOf_Chiller_Indirect_Absorption;

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

		if ( CompIndex != 0 ) {
			TempCondInDesign = IndirectAbsorber( CompIndex ).TempDesCondIn;
		}

		//Get Absorber data from input file
		if ( GetInput ) {
			GetIndirectAbsorberInput();
			GetInput = false;
		}

		// Find the correct Chiller
		if ( CompIndex == 0 ) {
			ChillNum = FindItemInList( AbsorberName, IndirectAbsorber );
			if ( ChillNum == 0 ) {
				ShowFatalError( "SimIndirectAbsorber: Specified chiller not one of Valid Absorption Chillers=" + AbsorberName );
			}
			CompIndex = ChillNum;
		} else {
			ChillNum = CompIndex;
			if ( ChillNum > NumIndirectAbsorbers || ChillNum < 1 ) {
				ShowFatalError( "SimIndirectAbsorber:  Invalid CompIndex passed=" + TrimSigDigits( ChillNum ) + ", Number of Units=" + TrimSigDigits( NumIndirectAbsorbers ) + ", Entered Unit name=" + AbsorberName );
			}
			if ( AbsorberName != IndirectAbsorber( ChillNum ).Name ) {
				ShowFatalError( "SimIndirectAbsorber: Invalid CompIndex passed=" + TrimSigDigits( ChillNum ) + ", Unit name=" + AbsorberName + ", stored Unit Name for that index=" + IndirectAbsorber( ChillNum ).Name );
			}
		}

		// Initialize Loop Equipment
		if ( InitLoopEquip ) {
			InitIndirectAbsorpChiller( ChillNum, RunFlag, MyLoad );

			if ( LoopNum == IndirectAbsorber( ChillNum ).CWLoopNum ) {
				SizeIndirectAbsorpChiller( ChillNum ); // only size when called from chilled water loop
				MinCap = IndirectAbsorber( ChillNum ).NomCap * IndirectAbsorber( ChillNum ).MinPartLoadRat;
				MaxCap = IndirectAbsorber( ChillNum ).NomCap * IndirectAbsorber( ChillNum ).MaxPartLoadRat;
				OptCap = IndirectAbsorber( ChillNum ).NomCap * IndirectAbsorber( ChillNum ).OptPartLoadRat;
			} else {
				MinCap = 0.0;
				MaxCap = 0.0;
				OptCap = 0.0;
			}
			if ( GetSizingFactor ) {
				ChillNum = FindItemInList( AbsorberName, IndirectAbsorber );
				if ( ChillNum != 0 ) {
					SizingFactor = IndirectAbsorber( ChillNum ).SizFac;
				}
			}
			return;
		}

		if ( LoopNum == IndirectAbsorber( ChillNum ).CWLoopNum ) {

			InitIndirectAbsorpChiller( ChillNum, RunFlag, MyLoad );
			CalcIndirectAbsorberModel( ChillNum, MyLoad, RunFlag, FirstIteration, EquipFlowCtrl );
			UpdateIndirectAbsorberRecords( MyLoad, RunFlag, ChillNum );

		} else if ( LoopNum == IndirectAbsorber( ChillNum ).CDLoopNum ) {
			// Called from non-dominant condenser water connection loop side
			UpdateChillerComponentCondenserSide( LoopNum, LoopSide, TypeOf_Chiller_Indirect_Absorption, IndirectAbsorber( ChillNum ).CondInletNodeNum, IndirectAbsorber( ChillNum ).CondOutletNodeNum, IndirectAbsorberReport( ChillNum ).QCond, IndirectAbsorberReport( ChillNum ).CondInletTemp, IndirectAbsorberReport( ChillNum ).CondOutletTemp, IndirectAbsorberReport( ChillNum ).Condmdot, FirstIteration );

		} else if ( LoopNum == IndirectAbsorber( ChillNum ).GenLoopNum ) {
			// Called from non-dominant generator hot water or steam connection loop side
			UpdateAbsorberChillerComponentGeneratorSide( LoopNum, LoopSide, TypeOf_Chiller_Indirect_Absorption, IndirectAbsorber( ChillNum ).GeneratorInletNodeNum, IndirectAbsorber( ChillNum ).GeneratorOutletNodeNum, IndirectAbsorber( ChillNum ).GenHeatSourceType, IndirectAbsorberReport( ChillNum ).QGenerator, IndirectAbsorberReport( ChillNum ).SteamMdot, FirstIteration );

		} else {
			ShowFatalError( "SimIndirectAbsorber: Invalid LoopNum passed=" + TrimSigDigits( LoopNum ) + ", Unit name=" + AbsorberName + ", stored chilled water loop=" + TrimSigDigits( IndirectAbsorber( ChillNum ).CWLoopNum ) + ", stored condenser water loop=" + TrimSigDigits( IndirectAbsorber( ChillNum ).CDLoopNum ) + ", stored generator loop=" + TrimSigDigits( IndirectAbsorber( ChillNum ).GenLoopNum ) );
		}

	}

	// End Absorption Chiller Module Driver Subroutines
	//******************************************************************************

	// Beginning of Absorption Chiller Module Get Input subroutines
	//******************************************************************************

	void
	GetIndirectAbsorberInput()
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR:          R. Raustad (FSEC)
		//       DATE WRITTEN:    May 2008

		// PURPOSE OF THIS SUBROUTINE:
		// This routine will get the input
		// required by the Indirect Absorption chiller models as shown below:

		// METHODOLOGY EMPLOYED:
		// EnergyPlus input processor

		// REFERENCES: na

		// Using/Aliasing
		using InputProcessor::GetNumObjectsFound;
		using InputProcessor::GetObjectItem;
		using InputProcessor::VerifyName;
		using InputProcessor::SameString;
		using InputProcessor::GetObjectDefMaxArgs;
		using namespace DataIPShortCuts;
		using BranchNodeConnections::TestCompSet;
		using NodeInputManager::GetOnlySingleNode;
		using GlobalNames::VerifyUniqueChillerName;
		using namespace OutputReportPredefined;
		using FluidProperties::FindRefrigerant;
		using CurveManager::GetCurveIndex;
		using CurveManager::GetCurveType;
		using CurveManager::CurveValue;
		using General::TrimSigDigits;
		using General::RoundSigDigits;
		using DataGlobals::AnyEnergyManagementSystemInModel;
		using DataSizing::AutoSize;

		// Locals
		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "GetIndirectAbsorberInput: " ); // include trailing blank space

		//LOCAL VARIABLES
		int AbsorberNum; // Absorber counter
		int NumAlphas; // Number of elements in the alpha array
		int NumNums; // Number of elements in the numeric array
		int IOStat; // IO Status when calling get input subroutine
		static bool ErrorsFound( false );
		bool IsNotOK; // Flag to verify name
		bool IsBlank; // Flag for blank name
		bool errFlag; // GetInput error flag
		Array1D_bool GenInputOutputNodesUsed; // Used for SetupOutputVariable

		//FLOW
		cCurrentModuleObject = "Chiller:Absorption:Indirect";
		NumIndirectAbsorbers = GetNumObjectsFound( cCurrentModuleObject );

		if ( NumIndirectAbsorbers <= 0 ) {
			ShowSevereError( "No " + cCurrentModuleObject + " equipment specified in input file" );
			//See if load distribution manager has already gotten the input
			ErrorsFound = true;
		}

		if ( allocated( IndirectAbsorber ) ) return;
		//ALLOCATE ARRAYS
		IndirectAbsorber.allocate( NumIndirectAbsorbers );

		IndirectAbsorberReport.allocate( NumIndirectAbsorbers );

		GenInputOutputNodesUsed.dimension( NumIndirectAbsorbers, false );

		//LOAD ARRAYS WITH BLAST CURVE FIT Absorber DATA
		for ( AbsorberNum = 1; AbsorberNum <= NumIndirectAbsorbers; ++AbsorberNum ) {
			GetObjectItem( cCurrentModuleObject, AbsorberNum, cAlphaArgs, NumAlphas, rNumericArgs, NumNums, IOStat, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );
			IsNotOK = false;
			IsBlank = false;
			VerifyName( cAlphaArgs( 1 ), IndirectAbsorber, AbsorberNum - 1, IsNotOK, IsBlank, cCurrentModuleObject + " Name" );
			if ( IsNotOK ) {
				ErrorsFound = true;
				if ( IsBlank ) cAlphaArgs( 1 ) = "xxxxx";
			}
			VerifyUniqueChillerName( cCurrentModuleObject, cAlphaArgs( 1 ), errFlag, cCurrentModuleObject + " Name" );
			if ( errFlag ) {
				ErrorsFound = true;
			}
			IndirectAbsorber( AbsorberNum ).Name = cAlphaArgs( 1 );
			IndirectAbsorber( AbsorberNum ).NomCap = rNumericArgs( 1 );
			if ( IndirectAbsorber( AbsorberNum ).NomCap == AutoSize ) {
				IndirectAbsorber( AbsorberNum ).NomCapWasAutoSized = true;
			}
			IndirectAbsorber( AbsorberNum ).NomPumpPower = rNumericArgs( 2 );
			if ( IndirectAbsorber( AbsorberNum ).NomPumpPower == AutoSize ) {
				IndirectAbsorber( AbsorberNum ).NomPumpPowerWasAutoSized = true;
			}
			if ( rNumericArgs( 1 ) == 0.0 ) {
				ShowSevereError( "Invalid " + cNumericFieldNames( 1 ) + '=' + RoundSigDigits( rNumericArgs( 1 ), 2 ) );
				ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + cAlphaArgs( 1 ) );
				ErrorsFound = true;
			}
			// Assign Node Numbers to specified nodes
			IndirectAbsorber( AbsorberNum ).EvapInletNodeNum = GetOnlySingleNode( cAlphaArgs( 2 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Water, NodeConnectionType_Inlet, 1, ObjectIsNotParent );
			IndirectAbsorber( AbsorberNum ).EvapOutletNodeNum = GetOnlySingleNode( cAlphaArgs( 3 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Water, NodeConnectionType_Outlet, 1, ObjectIsNotParent );
			TestCompSet( cCurrentModuleObject, cAlphaArgs( 1 ), cAlphaArgs( 2 ), cAlphaArgs( 3 ), "Chilled Water Nodes" );

			IndirectAbsorber( AbsorberNum ).CondInletNodeNum = GetOnlySingleNode( cAlphaArgs( 4 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Water, NodeConnectionType_Inlet, 2, ObjectIsNotParent );
			IndirectAbsorber( AbsorberNum ).CondOutletNodeNum = GetOnlySingleNode( cAlphaArgs( 5 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Water, NodeConnectionType_Outlet, 2, ObjectIsNotParent );
			TestCompSet( cCurrentModuleObject, cAlphaArgs( 1 ), cAlphaArgs( 4 ), cAlphaArgs( 5 ), "Condenser (not tested) Nodes" );

			IndirectAbsorber( AbsorberNum ).GeneratorInputCurvePtr = GetCurveIndex( cAlphaArgs( 7 ) );
			if ( IndirectAbsorber( AbsorberNum ).GeneratorInputCurvePtr > 0 ) {
				// Verify Curve Object, only legal types are Quadratic or Cubic
				{ auto const SELECT_CASE_var( GetCurveType( IndirectAbsorber( AbsorberNum ).GeneratorInputCurvePtr ) );
				if ( ( SELECT_CASE_var == "QUADRATIC" ) || ( SELECT_CASE_var == "CUBIC" ) ) {
				} else {
					ShowSevereError( cCurrentModuleObject + " \"" + IndirectAbsorber( AbsorberNum ).Name + "\"" );
					ShowContinueError( "...illegal Generator Heat Input function of part-load ratio curve type for this object." );
					ShowContinueError( "...Curve type = " + GetCurveType( IndirectAbsorber( AbsorberNum ).GeneratorInputCurvePtr ) );
					ErrorsFound = true;
				}}
			}

			IndirectAbsorber( AbsorberNum ).PumpPowerCurvePtr = GetCurveIndex( cAlphaArgs( 8 ) );
			if ( IndirectAbsorber( AbsorberNum ).PumpPowerCurvePtr > 0 ) {
				// Verify Curve Object, only legal types are Quadratic or Cubic
				{ auto const SELECT_CASE_var( GetCurveType( IndirectAbsorber( AbsorberNum ).PumpPowerCurvePtr ) );
				if ( ( SELECT_CASE_var == "QUADRATIC" ) || ( SELECT_CASE_var == "CUBIC" ) ) {
				} else {
					ShowSevereError( cCurrentModuleObject + " \"" + IndirectAbsorber( AbsorberNum ).Name + "\"" );
					ShowContinueError( "...illegal Pump Electric Input function of part-load ratio curve type for this object." );
					ShowContinueError( "...Curve type = " + GetCurveType( IndirectAbsorber( AbsorberNum ).PumpPowerCurvePtr ) );
					ErrorsFound = true;
				}}
			}

			if ( NumAlphas > 15 ) {
				if ( SameString( cAlphaArgs( 16 ), "HotWater" ) || SameString( cAlphaArgs( 16 ), "HotWater" ) ) {
					IndirectAbsorber( AbsorberNum ).GenHeatSourceType = NodeType_Water;
					//       Default to Steam if left blank
				} else if ( SameString( cAlphaArgs( 16 ), "Steam" ) || cAlphaArgs( 16 ).empty() ) {
					IndirectAbsorber( AbsorberNum ).GenHeatSourceType = NodeType_Steam;
				} else {
					ShowWarningError( cCurrentModuleObject + ", Name=" + cAlphaArgs( 1 ) );
					ShowContinueError( "...Generator heat source type must be Steam or Hot Water." );
					ShowContinueError( "...Entered generator heat source type = " + cAlphaArgs( 16 ) );
					ErrorsFound = true;
				}
			} else {
				//     Default to Steam if not entered as input
				IndirectAbsorber( AbsorberNum ).GenHeatSourceType = NodeType_Steam;
			}

			if ( ( ! cAlphaArgs( 9 ).empty() ) && ( ! cAlphaArgs( 10 ).empty() ) ) {
				GenInputOutputNodesUsed( AbsorberNum ) = true;
				if ( IndirectAbsorber( AbsorberNum ).GenHeatSourceType == NodeType_Water ) {
					IndirectAbsorber( AbsorberNum ).GeneratorInletNodeNum = GetOnlySingleNode( cAlphaArgs( 9 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Water, NodeConnectionType_Inlet, 3, ObjectIsNotParent );
					IndirectAbsorber( AbsorberNum ).GeneratorOutletNodeNum = GetOnlySingleNode( cAlphaArgs( 10 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Water, NodeConnectionType_Outlet, 3, ObjectIsNotParent );
					TestCompSet( cCurrentModuleObject, cAlphaArgs( 1 ), cAlphaArgs( 9 ), cAlphaArgs( 10 ), "Hot Water Nodes" );
				} else {
					IndirectAbsorber( AbsorberNum ).SteamFluidIndex = FindRefrigerant( "Steam" );
					IndirectAbsorber( AbsorberNum ).GeneratorInletNodeNum = GetOnlySingleNode( cAlphaArgs( 9 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Steam, NodeConnectionType_Inlet, 3, ObjectIsNotParent );
					IndirectAbsorber( AbsorberNum ).GeneratorOutletNodeNum = GetOnlySingleNode( cAlphaArgs( 10 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Steam, NodeConnectionType_Outlet, 3, ObjectIsNotParent );
					TestCompSet( cCurrentModuleObject, cAlphaArgs( 1 ), cAlphaArgs( 9 ), cAlphaArgs( 10 ), "Steam Nodes" );
				}
			} else if ( ! ( cAlphaArgs( 9 ).empty() == cAlphaArgs( 10 ).empty() ) ) {
				ShowWarningError( cCurrentModuleObject + ", Name=" + cAlphaArgs( 1 ) );
				ShowContinueError( "...Generator fluid nodes must both be entered (or both left blank)." );
				ShowContinueError( "...Generator fluid inlet node  = " + cAlphaArgs( 9 ) );
				ShowContinueError( "...Generator fluid outlet node = " + cAlphaArgs( 10 ) );
				ErrorsFound = true;
			} else {
				//     Generator fluid type must be steam if generator inlet/outlet nodes are not used
				if ( IndirectAbsorber( AbsorberNum ).GenHeatSourceType == NodeType_Water ) {
					ShowWarningError( cCurrentModuleObject + ", Name=" + cAlphaArgs( 1 ) );
					ShowContinueError( "...Generator fluid type must be Steam if generator inlet/outlet nodes are blank." );
					ShowContinueError( "...Generator fluid type is set to Steam and the simulation continues." );
					IndirectAbsorber( AbsorberNum ).GenHeatSourceType = NodeType_Steam;
				}
			}

			{ auto const SELECT_CASE_var( cAlphaArgs( 6 ) );
			if ( SELECT_CASE_var == "CONSTANTFLOW" ) {
				IndirectAbsorber( AbsorberNum ).FlowMode = ConstantFlow;
			} else if ( SELECT_CASE_var == "VARIABLEFLOW" ) {
				IndirectAbsorber( AbsorberNum ).FlowMode = LeavingSetPointModulated;
				ShowWarningError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\"," );
				ShowContinueError( "Invalid " + cAlphaFieldNames( 6 ) + '=' + cAlphaArgs( 6 ) );
				ShowContinueError( "Key choice is now called \"LeavingSetpointModulated\" and the simulation continues" );
			} else if ( SELECT_CASE_var == "LEAVINGSETPOINTMODULATED" ) {
				IndirectAbsorber( AbsorberNum ).FlowMode = LeavingSetPointModulated;
			} else if ( SELECT_CASE_var == "NOTMODULATED" ) {
				IndirectAbsorber( AbsorberNum ).FlowMode = NotModulated;
			} else {
				ShowSevereError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\"," );
				ShowContinueError( "Invalid " + cAlphaFieldNames( 6 ) + '=' + cAlphaArgs( 6 ) );
				ShowContinueError( "Available choices are ConstantFlow, NotModulated, or LeavingSetpointModulated" );
				ShowContinueError( "Flow mode NotModulated is assumed and the simulation continues." );
				IndirectAbsorber( AbsorberNum ).FlowMode = NotModulated;
			}}

			IndirectAbsorber( AbsorberNum ).CapFCondenserTempPtr = GetCurveIndex( cAlphaArgs( 11 ) );
			if ( IndirectAbsorber( AbsorberNum ).CapFCondenserTempPtr > 0 ) {
				// Verify Curve Object, only legal types are Quadratic or Cubic
				{ auto const SELECT_CASE_var( GetCurveType( IndirectAbsorber( AbsorberNum ).CapFCondenserTempPtr ) );
				if ( ( SELECT_CASE_var == "QUADRATIC" ) || ( SELECT_CASE_var == "CUBIC" ) ) {
				} else {
					ShowSevereError( cCurrentModuleObject + " \"" + IndirectAbsorber( AbsorberNum ).Name + "\"" );
					ShowContinueError( "...illegal Capacity Correction function of condenser temperature curve type for this object." );
					ShowContinueError( "...Curve type = " + GetCurveType( IndirectAbsorber( AbsorberNum ).CapFCondenserTempPtr ) );
					ErrorsFound = true;
				}}
			}

			IndirectAbsorber( AbsorberNum ).CapFEvaporatorTempPtr = GetCurveIndex( cAlphaArgs( 12 ) );
			if ( IndirectAbsorber( AbsorberNum ).CapFEvaporatorTempPtr > 0 ) {
				// Verify Curve Object, only legal types are Quadratic or Cubic
				{ auto const SELECT_CASE_var( GetCurveType( IndirectAbsorber( AbsorberNum ).CapFEvaporatorTempPtr ) );
				if ( ( SELECT_CASE_var == "QUADRATIC" ) || ( SELECT_CASE_var == "CUBIC" ) ) {
				} else {
					ShowSevereError( cCurrentModuleObject + " \"" + IndirectAbsorber( AbsorberNum ).Name + "\"" );
					ShowContinueError( "...illegal Capacity Correction function of evaporator temperature curve type for this object." );
					ShowContinueError( "...Curve type = " + GetCurveType( IndirectAbsorber( AbsorberNum ).CapFCondenserTempPtr ) );
					ErrorsFound = true;
				}}
			}

			IndirectAbsorber( AbsorberNum ).CapFGeneratorTempPtr = GetCurveIndex( cAlphaArgs( 13 ) );
			if ( IndirectAbsorber( AbsorberNum ).CapFGeneratorTempPtr > 0 ) {
				// Verify Curve Object, only legal types are Quadratic or Cubic
				{ auto const SELECT_CASE_var( GetCurveType( IndirectAbsorber( AbsorberNum ).CapFGeneratorTempPtr ) );
				if ( ( SELECT_CASE_var == "QUADRATIC" ) || ( SELECT_CASE_var == "CUBIC" ) ) {
				} else {
					if ( IndirectAbsorber( AbsorberNum ).GenHeatSourceType == NodeType_Water ) {
						ShowSevereError( cCurrentModuleObject + " \"" + IndirectAbsorber( AbsorberNum ).Name + "\"" );
						ShowContinueError( "...illegal Capacity Correction function of generator temperature curve type for this object." );
						ShowContinueError( "...Curve type = " + GetCurveType( IndirectAbsorber( AbsorberNum ).CapFGeneratorTempPtr ) );
						ErrorsFound = true;
					}
				}}
			}

			IndirectAbsorber( AbsorberNum ).HeatInputFCondTempPtr = GetCurveIndex( cAlphaArgs( 14 ) );
			if ( IndirectAbsorber( AbsorberNum ).HeatInputFCondTempPtr > 0 ) {
				// Verify Curve Object, only legal types are Quadratic or Cubic
				{ auto const SELECT_CASE_var( GetCurveType( IndirectAbsorber( AbsorberNum ).HeatInputFCondTempPtr ) );
				if ( ( SELECT_CASE_var == "QUADRATIC" ) || ( SELECT_CASE_var == "CUBIC" ) ) {
				} else {
					ShowSevereError( cCurrentModuleObject + " \"" + IndirectAbsorber( AbsorberNum ).Name + "\"" );
					ShowContinueError( "...illegal Generator Heat Input Correction function of condenser temperature curve type for this object." );
					ShowContinueError( "...Curve type = " + GetCurveType( IndirectAbsorber( AbsorberNum ).HeatInputFCondTempPtr ) );
					ErrorsFound = true;
				}}
			}

			IndirectAbsorber( AbsorberNum ).HeatInputFEvapTempPtr = GetCurveIndex( cAlphaArgs( 15 ) );
			if ( IndirectAbsorber( AbsorberNum ).HeatInputFEvapTempPtr > 0 ) {
				// Verify Curve Object, only legal types are Quadratic or Cubic
				{ auto const SELECT_CASE_var( GetCurveType( IndirectAbsorber( AbsorberNum ).HeatInputFEvapTempPtr ) );
				if ( ( SELECT_CASE_var == "QUADRATIC" ) || ( SELECT_CASE_var == "CUBIC" ) ) {
				} else {
					ShowSevereError( cCurrentModuleObject + " \"" + IndirectAbsorber( AbsorberNum ).Name + "\"" );
					ShowContinueError( "...illegal Generator Heat Input Correction function of evaporator temperature curve type for this object." );
					ShowContinueError( "...Curve type = " + GetCurveType( IndirectAbsorber( AbsorberNum ).HeatInputFEvapTempPtr ) );
					ErrorsFound = true;
				}}
			}

			// Get remaining data
			IndirectAbsorber( AbsorberNum ).MinPartLoadRat = rNumericArgs( 3 );
			IndirectAbsorber( AbsorberNum ).MaxPartLoadRat = rNumericArgs( 4 );
			IndirectAbsorber( AbsorberNum ).OptPartLoadRat = rNumericArgs( 5 );
			IndirectAbsorber( AbsorberNum ).TempDesCondIn = rNumericArgs( 6 );
			IndirectAbsorber( AbsorberNum ).MinCondInletTemp = rNumericArgs( 7 );
			IndirectAbsorber( AbsorberNum ).TempLowLimitEvapOut = rNumericArgs( 8 );
			IndirectAbsorber( AbsorberNum ).EvapVolFlowRate = rNumericArgs( 9 );
			if ( IndirectAbsorber( AbsorberNum ).EvapVolFlowRate == AutoSize ) {
				IndirectAbsorber( AbsorberNum ).EvapVolFlowRateWasAutoSized = true;
			}
			IndirectAbsorber( AbsorberNum ).CondVolFlowRate = rNumericArgs( 10 );
			if ( IndirectAbsorber( AbsorberNum ).CondVolFlowRate == AutoSize ) {
				IndirectAbsorber( AbsorberNum ).CondVolFlowRateWasAutoSized = true;
			}
			if ( NumNums > 10 ) {
				IndirectAbsorber( AbsorberNum ).GeneratorVolFlowRate = rNumericArgs( 11 );
				if ( IndirectAbsorber( AbsorberNum ).GeneratorVolFlowRate == AutoSize ) {
					IndirectAbsorber( AbsorberNum ).GeneratorVolFlowRateWasAutoSized = true;
				}
			}

			if ( IndirectAbsorber( AbsorberNum ).GeneratorVolFlowRate == 0.0 && IndirectAbsorber( AbsorberNum ).GenHeatSourceType == NodeType_Water ) {
				ShowWarningError( cCurrentModuleObject + ", Name=" + cAlphaArgs( 1 ) );
				ShowContinueError( "...Generator water flow rate must be greater than 0 when absorber generator fluid type is hot water." );
				ErrorsFound = true;
			}

			if ( NumNums > 11 ) {
				IndirectAbsorber( AbsorberNum ).MinGeneratorInletTemp = rNumericArgs( 12 );
			} else {
				IndirectAbsorber( AbsorberNum ).MinGeneratorInletTemp = 0.0;
			}

			if ( NumNums > 12 ) {
				IndirectAbsorber( AbsorberNum ).GeneratorSubcool = rNumericArgs( 13 );
			} else {
				IndirectAbsorber( AbsorberNum ).GeneratorSubcool = 0.0;
			}

			if ( NumNums > 13 ) {
				IndirectAbsorber( AbsorberNum ).LoopSubcool = rNumericArgs( 14 );
			} else {
				IndirectAbsorber( AbsorberNum ).LoopSubcool = 0.0;
			}

			if ( NumNums > 14 ) {
				IndirectAbsorber( AbsorberNum ).SizFac = rNumericArgs( 15 );
			} else {
				IndirectAbsorber( AbsorberNum ).SizFac = 1.0;
			}

		}

		if ( ErrorsFound ) {
			ShowFatalError( "Errors found in getting Chiller:Absorption:Indirect" );
		}

		for ( AbsorberNum = 1; AbsorberNum <= NumIndirectAbsorbers; ++AbsorberNum ) {
			SetupOutputVariable( "Chiller Electric Power [W]", IndirectAbsorberReport( AbsorberNum ).PumpingPower, "System", "Average", IndirectAbsorber( AbsorberNum ).Name );
			SetupOutputVariable( "Chiller Electric Energy [J]", IndirectAbsorberReport( AbsorberNum ).PumpingEnergy, "System", "Sum", IndirectAbsorber( AbsorberNum ).Name, _, "ELECTRICITY", "Cooling", _, "Plant" );
			SetupOutputVariable( "Chiller Evaporator Cooling Rate [W]", IndirectAbsorberReport( AbsorberNum ).QEvap, "System", "Average", IndirectAbsorber( AbsorberNum ).Name );
			SetupOutputVariable( "Chiller Evaporator Cooling Energy [J]", IndirectAbsorberReport( AbsorberNum ).EvapEnergy, "System", "Sum", IndirectAbsorber( AbsorberNum ).Name, _, "ENERGYTRANSFER", "CHILLERS", _, "Plant" );
			SetupOutputVariable( "Chiller Evaporator Inlet Temperature [C]", IndirectAbsorberReport( AbsorberNum ).EvapInletTemp, "System", "Average", IndirectAbsorber( AbsorberNum ).Name );
			SetupOutputVariable( "Chiller Evaporator Outlet Temperature [C]", IndirectAbsorberReport( AbsorberNum ).EvapOutletTemp, "System", "Average", IndirectAbsorber( AbsorberNum ).Name );
			SetupOutputVariable( "Chiller Evaporator Mass Flow Rate [kg/s]", IndirectAbsorberReport( AbsorberNum ).Evapmdot, "System", "Average", IndirectAbsorber( AbsorberNum ).Name );

			SetupOutputVariable( "Chiller Condenser Heat Transfer Rate [W]", IndirectAbsorberReport( AbsorberNum ).QCond, "System", "Average", IndirectAbsorber( AbsorberNum ).Name );
			SetupOutputVariable( "Chiller Condenser Heat Transfer Energy [J]", IndirectAbsorberReport( AbsorberNum ).CondEnergy, "System", "Sum", IndirectAbsorber( AbsorberNum ).Name, _, "ENERGYTRANSFER", "HEATREJECTION", _, "Plant" );
			SetupOutputVariable( "Chiller Condenser Inlet Temperature [C]", IndirectAbsorberReport( AbsorberNum ).CondInletTemp, "System", "Average", IndirectAbsorber( AbsorberNum ).Name );
			SetupOutputVariable( "Chiller Condenser Outlet Temperature [C]", IndirectAbsorberReport( AbsorberNum ).CondOutletTemp, "System", "Average", IndirectAbsorber( AbsorberNum ).Name );
			SetupOutputVariable( "Chiller Condenser Mass Flow Rate [kg/s]", IndirectAbsorberReport( AbsorberNum ).Condmdot, "System", "Average", IndirectAbsorber( AbsorberNum ).Name );

			if ( IndirectAbsorber( AbsorberNum ).GenHeatSourceType == NodeType_Water ) {
				SetupOutputVariable( "Chiller Hot Water Consumption Rate [W]", IndirectAbsorberReport( AbsorberNum ).QGenerator, "System", "Average", IndirectAbsorber( AbsorberNum ).Name );
				SetupOutputVariable( "Chiller Source Hot Water Energy [J]", IndirectAbsorberReport( AbsorberNum ).GeneratorEnergy, "System", "Sum", IndirectAbsorber( AbsorberNum ).Name, _, "EnergyTransfer", "Cooling", _, "Plant" );
			} else {
				if ( GenInputOutputNodesUsed( AbsorberNum ) ) {
					SetupOutputVariable( "Chiller Source Steam Rate [W]", IndirectAbsorberReport( AbsorberNum ).QGenerator, "System", "Average", IndirectAbsorber( AbsorberNum ).Name );
					SetupOutputVariable( "Chiller Source Steam Energy [J]", IndirectAbsorberReport( AbsorberNum ).GeneratorEnergy, "System", "Sum", IndirectAbsorber( AbsorberNum ).Name, _, "PLANTLOOPHEATINGDEMAND", "CHILLERS", _, "Plant" );
				} else {
					SetupOutputVariable( "Chiller Source Steam Rate [W]", IndirectAbsorberReport( AbsorberNum ).QGenerator, "System", "Average", IndirectAbsorber( AbsorberNum ).Name );
					SetupOutputVariable( "Chiller Source Steam Energy [J]", IndirectAbsorberReport( AbsorberNum ).GeneratorEnergy, "System", "Sum", IndirectAbsorber( AbsorberNum ).Name, _, "Steam", "Cooling", _, "Plant" );
				}
			}

			SetupOutputVariable( "Chiller COP [W/W]", IndirectAbsorberReport( AbsorberNum ).ActualCOP, "System", "Average", IndirectAbsorber( AbsorberNum ).Name );
			SetupOutputVariable( "Chiller Part Load Ratio []", IndirectAbsorberReport( AbsorberNum ).ChillerPartLoadRatio, "System", "Average", IndirectAbsorber( AbsorberNum ).Name );
			SetupOutputVariable( "Chiller Cycling Ratio []", IndirectAbsorberReport( AbsorberNum ).ChillerCyclingFrac, "System", "Average", IndirectAbsorber( AbsorberNum ).Name );

			SetupOutputVariable( "Chiller Steam Heat Loss Rate [W]", IndirectAbsorberReport( AbsorberNum ).LoopLoss, "System", "Average", IndirectAbsorber( AbsorberNum ).Name );

			if ( AnyEnergyManagementSystemInModel ) {
				SetupEMSInternalVariable( "Chiller Nominal Capacity", IndirectAbsorber( AbsorberNum ).Name, "[W]", IndirectAbsorber( AbsorberNum ).NomCap );
			}
		}

		if ( allocated( GenInputOutputNodesUsed ) ) GenInputOutputNodesUsed.deallocate();

	}

	// End of Get Input subroutines for the Absorption Chiller Module
	//******************************************************************************

	void
	InitIndirectAbsorpChiller(
		int const ChillNum, // number of the current electric chiller being simulated
		bool const RunFlag, // TRUE when chiller operating
		Real64 const MyLoad // requested load
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Richard Raustad
		//       DATE WRITTEN   September 2009
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine is for initializations of the Indirect Absorption Chiller components

		// METHODOLOGY EMPLOYED:
		// Uses the status flags to trigger initializations.

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataGlobals::BeginEnvrnFlag;
		using DataGlobals::AnyEnergyManagementSystemInModel;
		using DataPlant::PlantLoop;
		using DataPlant::TypeOf_Chiller_Indirect_Absorption;
		using DataPlant::ScanPlantLoopsForObject;
		using DataPlant::PlantFirstSizesOkayToFinalize;
		using DataPlant::LoopFlowStatus_NeedyIfLoopOn;
		using InputProcessor::SameString;
		using PlantUtilities::InterConnectTwoPlantLoopSides;
		using PlantUtilities::InitComponentNodes;
		using PlantUtilities::SetComponentFlowRate;
		using EMSManager::iTemperatureSetPoint;
		using EMSManager::CheckIfNodeSetPointManagedByEMS;
		using FluidProperties::GetDensityGlycol;
		using FluidProperties::GetSatDensityRefrig;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "InitIndirectAbsorpChiller" );

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
		Real64 SteamDensity; // density of generator steam (when connected to a steam loop)
		Real64 mdotEvap; // local fluid mass flow rate thru evaporator
		Real64 mdotCond; // local fluid mass flow rate thru condenser
		Real64 mdotGen; // local fluid mass flow rate thru generator

		// FLOW:

		// Do the one time initializations
		if ( MyOneTimeFlag ) {
			MyFlag.allocate( NumIndirectAbsorbers );
			MyEnvrnFlag.allocate( NumIndirectAbsorbers );
			MyFlag = true;
			MyEnvrnFlag = true;
			MyOneTimeFlag = false;
		}
		// Init more variables
		if ( MyFlag( ChillNum ) ) {
			// Locate the chillers on the plant loops for later usage
			errFlag = false;
			ScanPlantLoopsForObject( IndirectAbsorber( ChillNum ).Name, TypeOf_Chiller_Indirect_Absorption, IndirectAbsorber( ChillNum ).CWLoopNum, IndirectAbsorber( ChillNum ).CWLoopSideNum, IndirectAbsorber( ChillNum ).CWBranchNum, IndirectAbsorber( ChillNum ).CWCompNum, IndirectAbsorber( ChillNum ).TempLowLimitEvapOut, _, _, IndirectAbsorber( ChillNum ).EvapInletNodeNum, _, errFlag );

			ScanPlantLoopsForObject( IndirectAbsorber( ChillNum ).Name, TypeOf_Chiller_Indirect_Absorption, IndirectAbsorber( ChillNum ).CDLoopNum, IndirectAbsorber( ChillNum ).CDLoopSideNum, IndirectAbsorber( ChillNum ).CDBranchNum, IndirectAbsorber( ChillNum ).CDCompNum, _, _, _, IndirectAbsorber( ChillNum ).CondInletNodeNum, _, errFlag );
			InterConnectTwoPlantLoopSides( IndirectAbsorber( ChillNum ).CWLoopNum, IndirectAbsorber( ChillNum ).CWLoopSideNum, IndirectAbsorber( ChillNum ).CDLoopNum, IndirectAbsorber( ChillNum ).CDLoopSideNum, TypeOf_Chiller_Indirect_Absorption, true );

			if ( IndirectAbsorber( ChillNum ).GeneratorInletNodeNum > 0 ) {
				ScanPlantLoopsForObject( IndirectAbsorber( ChillNum ).Name, TypeOf_Chiller_Indirect_Absorption, IndirectAbsorber( ChillNum ).GenLoopNum, IndirectAbsorber( ChillNum ).GenLoopSideNum, IndirectAbsorber( ChillNum ).GenBranchNum, IndirectAbsorber( ChillNum ).GenCompNum, _, _, _, IndirectAbsorber( ChillNum ).GeneratorInletNodeNum, _, errFlag );
				InterConnectTwoPlantLoopSides( IndirectAbsorber( ChillNum ).CWLoopNum, IndirectAbsorber( ChillNum ).CWLoopSideNum, IndirectAbsorber( ChillNum ).GenLoopNum, IndirectAbsorber( ChillNum ).GenCompNum, TypeOf_Chiller_Indirect_Absorption, true );

			}

			if ( ( IndirectAbsorber( ChillNum ).CondInletNodeNum > 0 ) && ( IndirectAbsorber( ChillNum ).GeneratorInletNodeNum > 0 ) ) {
				InterConnectTwoPlantLoopSides( IndirectAbsorber( ChillNum ).CDLoopNum, IndirectAbsorber( ChillNum ).CDLoopSideNum, IndirectAbsorber( ChillNum ).GenLoopNum, IndirectAbsorber( ChillNum ).GenCompNum, TypeOf_Chiller_Indirect_Absorption, false );

			}
			if ( errFlag ) {
				ShowFatalError( "InitIndirectAbsorpChiller: Program terminated due to previous condition(s)." );
			}

			if ( IndirectAbsorber( ChillNum ).FlowMode == ConstantFlow ) {
				// reset flow priority
				PlantLoop( IndirectAbsorber( ChillNum ).CWLoopNum ).LoopSide( IndirectAbsorber( ChillNum ).CWLoopSideNum ).Branch( IndirectAbsorber( ChillNum ).CWBranchNum ).Comp( IndirectAbsorber( ChillNum ).CWCompNum ).FlowPriority = LoopFlowStatus_NeedyIfLoopOn;
			}

			if ( IndirectAbsorber( ChillNum ).FlowMode == LeavingSetPointModulated ) {
				// reset flow priority
				PlantLoop( IndirectAbsorber( ChillNum ).CWLoopNum ).LoopSide( IndirectAbsorber( ChillNum ).CWLoopSideNum ).Branch( IndirectAbsorber( ChillNum ).CWBranchNum ).Comp( IndirectAbsorber( ChillNum ).CWCompNum ).FlowPriority = LoopFlowStatus_NeedyIfLoopOn;

				if ( ( Node( IndirectAbsorber( ChillNum ).EvapOutletNodeNum ).TempSetPoint == SensedNodeFlagValue ) && ( Node( IndirectAbsorber( ChillNum ).EvapOutletNodeNum ).TempSetPointHi == SensedNodeFlagValue ) ) {
					if ( ! AnyEnergyManagementSystemInModel ) {
						if ( ! IndirectAbsorber( ChillNum ).ModulatedFlowErrDone ) {
							ShowWarningError( "Missing temperature setpoint for LeavingSetpointModulated mode chiller named " + IndirectAbsorber( ChillNum ).Name );
							ShowContinueError( "  A temperature setpoint is needed at the outlet node of a chiller in variable flow mode, use a SetpointManager" );
							ShowContinueError( "  The overall loop setpoint will be assumed for chiller. The simulation continues ... " );
							IndirectAbsorber( ChillNum ).ModulatedFlowErrDone = true;
						}
					} else {
						// need call to EMS to check node
						FatalError = false; // but not really fatal yet, but should be.
						CheckIfNodeSetPointManagedByEMS( IndirectAbsorber( ChillNum ).EvapOutletNodeNum, iTemperatureSetPoint, FatalError );
						if ( FatalError ) {
							if ( ! IndirectAbsorber( ChillNum ).ModulatedFlowErrDone ) {
								ShowWarningError( "Missing temperature setpoint for LeavingSetpointModulated mode chiller named " + IndirectAbsorber( ChillNum ).Name );
								ShowContinueError( "  A temperature setpoint is needed at the outlet node of a chiller evaporator in variable flow mode" );
								ShowContinueError( "  use a Setpoint Manager to establish a setpoint at the chiller evaporator outlet node " );
								ShowContinueError( "  or use an EMS actuator to establish a setpoint at the outlet node " );
								ShowContinueError( "  The overall loop setpoint will be assumed for chiller. The simulation continues ... " );
								IndirectAbsorber( ChillNum ).ModulatedFlowErrDone = true;
							}
						}
					}

					IndirectAbsorber( ChillNum ).ModulatedFlowSetToLoop = true;
					Node( IndirectAbsorber( ChillNum ).EvapOutletNodeNum ).TempSetPoint = Node( PlantLoop( IndirectAbsorber( ChillNum ).CWLoopNum ).TempSetPointNodeNum ).TempSetPoint;
					Node( IndirectAbsorber( ChillNum ).EvapOutletNodeNum ).TempSetPointHi = Node( PlantLoop( IndirectAbsorber( ChillNum ).CWLoopNum ).TempSetPointNodeNum ).TempSetPointHi;
				}
			}

			MyFlag( ChillNum ) = false;
		}

		CondInletNode = IndirectAbsorber( ChillNum ).CondInletNodeNum;
		CondOutletNode = IndirectAbsorber( ChillNum ).CondOutletNodeNum;

		//Initialize Supply Side Variables
		if ( MyEnvrnFlag( ChillNum ) && BeginEnvrnFlag && ( PlantFirstSizesOkayToFinalize ) ) {

			rho = GetDensityGlycol( PlantLoop( IndirectAbsorber( ChillNum ).CWLoopNum ).FluidName, InitConvTemp, PlantLoop( IndirectAbsorber( ChillNum ).CWLoopNum ).FluidIndex, RoutineName );

			IndirectAbsorber( ChillNum ).EvapMassFlowRateMax = IndirectAbsorber( ChillNum ).EvapVolFlowRate * rho;

			InitComponentNodes( 0.0, IndirectAbsorber( ChillNum ).EvapMassFlowRateMax, IndirectAbsorber( ChillNum ).EvapInletNodeNum, IndirectAbsorber( ChillNum ).EvapOutletNodeNum, IndirectAbsorber( ChillNum ).CWLoopNum, IndirectAbsorber( ChillNum ).CWLoopSideNum, IndirectAbsorber( ChillNum ).CWBranchNum, IndirectAbsorber( ChillNum ).CWCompNum );

			rho = GetDensityGlycol( PlantLoop( IndirectAbsorber( ChillNum ).CDLoopNum ).FluidName, InitConvTemp, PlantLoop( IndirectAbsorber( ChillNum ).CDLoopNum ).FluidIndex, RoutineName );

			IndirectAbsorber( ChillNum ).CondMassFlowRateMax = rho * IndirectAbsorber( ChillNum ).CondVolFlowRate;

			InitComponentNodes( 0.0, IndirectAbsorber( ChillNum ).CondMassFlowRateMax, CondInletNode, CondOutletNode, IndirectAbsorber( ChillNum ).CDLoopNum, IndirectAbsorber( ChillNum ).CDLoopSideNum, IndirectAbsorber( ChillNum ).CDBranchNum, IndirectAbsorber( ChillNum ).CDCompNum );

			Node( CondInletNode ).Temp = IndirectAbsorber( ChillNum ).TempDesCondIn;

			if ( IndirectAbsorber( ChillNum ).GeneratorInletNodeNum > 0 ) {

				if ( IndirectAbsorber( ChillNum ).GenHeatSourceType == NodeType_Water ) {

					rho = GetDensityGlycol( PlantLoop( IndirectAbsorber( ChillNum ).GenLoopNum ).FluidName, InitConvTemp, PlantLoop( IndirectAbsorber( ChillNum ).GenLoopNum ).FluidIndex, RoutineName );
					IndirectAbsorber( ChillNum ).GenMassFlowRateMax = rho * IndirectAbsorber( ChillNum ).GeneratorVolFlowRate;

				} else {
					SteamDensity = GetSatDensityRefrig( fluidNameSteam, Node( IndirectAbsorber( ChillNum ).GeneratorInletNodeNum ).Temp, 1.0, IndirectAbsorber( ChillNum ).SteamFluidIndex, calcChillerAbsorptionIndirect + IndirectAbsorber( ChillNum ).Name );
					IndirectAbsorber( ChillNum ).GenMassFlowRateMax = SteamDensity * IndirectAbsorber( ChillNum ).GeneratorVolFlowRate;
				}

				InitComponentNodes( 0.0, IndirectAbsorber( ChillNum ).GenMassFlowRateMax, IndirectAbsorber( ChillNum ).GeneratorInletNodeNum, IndirectAbsorber( ChillNum ).GeneratorOutletNodeNum, IndirectAbsorber( ChillNum ).GenLoopNum, IndirectAbsorber( ChillNum ).GenLoopSideNum, IndirectAbsorber( ChillNum ).GenBranchNum, IndirectAbsorber( ChillNum ).GenCompNum );
			}
			MyEnvrnFlag( ChillNum ) = false;
		}
		if ( ! BeginEnvrnFlag ) {
			MyEnvrnFlag( ChillNum ) = true;
		}

		if ( ( IndirectAbsorber( ChillNum ).FlowMode == LeavingSetPointModulated ) && IndirectAbsorber( ChillNum ).ModulatedFlowSetToLoop ) {
			// fix for clumsy old input that worked because loop setpoint was spread.
			//  could be removed with transition, testing , model change, period of being obsolete.
			Node( IndirectAbsorber( ChillNum ).EvapOutletNodeNum ).TempSetPoint = Node( PlantLoop( IndirectAbsorber( ChillNum ).CWLoopNum ).TempSetPointNodeNum ).TempSetPoint;
			Node( IndirectAbsorber( ChillNum ).EvapOutletNodeNum ).TempSetPointHi = Node( PlantLoop( IndirectAbsorber( ChillNum ).CWLoopNum ).TempSetPointNodeNum ).TempSetPointHi;
		}

		if ( ( MyLoad < 0.0 ) && RunFlag ) {
			mdotEvap = IndirectAbsorber( ChillNum ).EvapMassFlowRateMax;
			mdotCond = IndirectAbsorber( ChillNum ).CondMassFlowRateMax;
			mdotGen = IndirectAbsorber( ChillNum ).GenMassFlowRateMax;
		} else {
			mdotEvap = 0.0;
			mdotCond = 0.0;
			mdotGen = 0.0;
		}

		SetComponentFlowRate( mdotEvap, IndirectAbsorber( ChillNum ).EvapInletNodeNum, IndirectAbsorber( ChillNum ).EvapOutletNodeNum, IndirectAbsorber( ChillNum ).CWLoopNum, IndirectAbsorber( ChillNum ).CWLoopSideNum, IndirectAbsorber( ChillNum ).CWBranchNum, IndirectAbsorber( ChillNum ).CWCompNum );

		SetComponentFlowRate( mdotCond, CondInletNode, CondOutletNode, IndirectAbsorber( ChillNum ).CDLoopNum, IndirectAbsorber( ChillNum ).CDLoopSideNum, IndirectAbsorber( ChillNum ).CDBranchNum, IndirectAbsorber( ChillNum ).CDCompNum );

		if ( IndirectAbsorber( ChillNum ).GeneratorInletNodeNum > 0 ) {

			SetComponentFlowRate( mdotGen, IndirectAbsorber( ChillNum ).GeneratorInletNodeNum, IndirectAbsorber( ChillNum ).GeneratorOutletNodeNum, IndirectAbsorber( ChillNum ).GenLoopNum, IndirectAbsorber( ChillNum ).GenLoopSideNum, IndirectAbsorber( ChillNum ).GenBranchNum, IndirectAbsorber( ChillNum ).GenCompNum );

		}

	}

	void
	SizeIndirectAbsorpChiller( int const ChillNum )
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         R. Raustad (FSEC)
		//       DATE WRITTEN   May 2008
		//       MODIFIED       November 2013 Daeho Kang, add component sizing table entries
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine is for sizing Indirect Absorption Chiller Components for which capacities and flow rates
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
		using CurveManager::CurveValue;
		using namespace OutputReportPredefined;
		using namespace FluidProperties;
		//  USE BranchInputManager, ONLY: MyPlantSizingIndex
		using ReportSizingManager::ReportSizingOutput;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "SizeIndirectAbsorpChiller" );
		static std::string const SizeChillerAbsorptionIndirect( "SIZE Chiller:Absorption:Indirect" );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int PltSizIndex; // Plant Sizing Do loop index
		int PltSizNum; // Plant Sizing index corresponding to CurLoopNum
		int PltSizCondNum; // Plant Sizing index for condenser loop
		int PltSizSteamNum; // Plant Sizing index for steam heating loop
		int PltSizHeatingNum; // Plant Sizing index for how water heating loop
		Real64 SteamInputRatNom; // nominal energy input ratio (steam or hot water)
		Real64 SteamDensity; // density of generator steam (when connected to a steam loop)
		Real64 EnthSteamOutDry; // dry enthalpy of steam (quality = 1)
		Real64 EnthSteamOutWet; // wet enthalpy of steam (quality = 0)
		Real64 HfgSteam; // latent heat of steam at constant pressure
		Real64 SteamDeltaT; // amount of sub-cooling of steam condensate
		Real64 SteamMassFlowRate; // steam mass flow rate through generator
		Real64 CpWater; // specific heat of generator fluid (when connected to a hot water loop)
		Real64 RhoWater; // density of water (kg/m3)
		Real64 GeneratorOutletTemp; // outlet temperature of generator
		bool ErrorsFound; // If errors detected in input
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
		Real64 NomCapUser; // Hardsized nominal capacity cooling power for reporting
		Real64 NomPumpPowerUser; // Hardsized local nominal pump power for reporting
		Real64 EvapVolFlowRateUser; // Hardsized local evaporator design volume flow rate for reporting
		Real64 CondVolFlowRateUser; // Hardsized local condenser design volume flow rate for reporting
		Real64 GeneratorVolFlowRateUser; // Hardsized local generator design volume flow rate for reporting

		PltSizNum = 0;
		PltSizCondNum = 0;
		PltSizHeatingNum = 0;
		PltSizSteamNum = 0;
		ErrorsFound = false;
		// init local temporary version in case of partial/mixed autosizing
		tmpNomCap = IndirectAbsorber( ChillNum ).NomCap;
		tmpNomPumpPower = IndirectAbsorber( ChillNum ).NomPumpPower;
		tmpEvapVolFlowRate = IndirectAbsorber( ChillNum ).EvapVolFlowRate;
		tmpCondVolFlowRate = IndirectAbsorber( ChillNum ).CondVolFlowRate;
		tmpGeneratorVolFlowRate = IndirectAbsorber( ChillNum ).GeneratorVolFlowRate;
		NomCapUser = 0.0;
		NomPumpPowerUser = 0.0;
		EvapVolFlowRateUser = 0.0;
		CondVolFlowRateUser = 0.0;
		GeneratorVolFlowRateUser = 0.0;

		if ( IndirectAbsorber( ChillNum ).GeneratorInputCurvePtr > 0 ) {
			SteamInputRatNom = CurveValue( IndirectAbsorber( ChillNum ).GeneratorInputCurvePtr, 1.0 );
		} else {
			SteamInputRatNom = 1.0;
		}

		// find the appropriate Plant Sizing object
		//IF (CurLoopNum > 0) THEN
		PltSizNum = PlantLoop( IndirectAbsorber( ChillNum ).CWLoopNum ).PlantSizNum;
		//END IF

		//IF (IndirectAbsorber(ChillNum)%CondVolFlowRate == AutoSize) THEN
		if ( PltSizNum > 0 ) { //Autodesk:Std An integer can't be used in a boolean context (most compilers will allow this non-standard usage): Added > 0 patch
			PltSizCondNum = MyPlantSizingIndex( "Chiller:Absorption:Indirect", IndirectAbsorber( ChillNum ).Name,
				IndirectAbsorber( ChillNum ).CondInletNodeNum, IndirectAbsorber( ChillNum ).CondOutletNodeNum, LoopErrorsFound );
		}

		if ( IndirectAbsorber( ChillNum ).GenHeatSourceType == NodeType_Steam ) {
			if ( IndirectAbsorber( ChillNum ).GeneratorInletNodeNum > 0 && IndirectAbsorber( ChillNum ).GeneratorOutletNodeNum > 0 ) {
				PltSizSteamNum = MyPlantSizingIndex( "Chiller:Absorption:Indirect", IndirectAbsorber( ChillNum ).Name,
					IndirectAbsorber( ChillNum ).GeneratorInletNodeNum, IndirectAbsorber( ChillNum ).GeneratorOutletNodeNum, LoopErrorsFound );
			} else {
				for ( PltSizIndex = 1; PltSizIndex <= NumPltSizInput; ++PltSizIndex ) {
					if ( PlantSizData( PltSizIndex ).LoopType == SteamLoop ) {
						PltSizSteamNum = PltSizIndex;
					}
				}
			}
		} else {
			if ( IndirectAbsorber( ChillNum ).GeneratorInletNodeNum > 0 && IndirectAbsorber( ChillNum ).GeneratorOutletNodeNum > 0 ) {
				PltSizHeatingNum = MyPlantSizingIndex( "Chiller:Absorption:Indirect", IndirectAbsorber( ChillNum ).Name,
					IndirectAbsorber( ChillNum ).GeneratorInletNodeNum, IndirectAbsorber( ChillNum ).GeneratorOutletNodeNum, LoopErrorsFound );
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

				Cp = GetSpecificHeatGlycol( PlantLoop( IndirectAbsorber( ChillNum ).CWLoopNum ).FluidName, InitConvTemp, PlantLoop( IndirectAbsorber( ChillNum ).CWLoopNum ).FluidIndex, RoutineName );

				rho = GetDensityGlycol( PlantLoop( IndirectAbsorber( ChillNum ).CWLoopNum ).FluidName, InitConvTemp, PlantLoop( IndirectAbsorber( ChillNum ).CWLoopNum ).FluidIndex, RoutineName );
				tmpNomCap = Cp * rho * PlantSizData( PltSizNum ).DeltaT * PlantSizData( PltSizNum ).DesVolFlowRate * IndirectAbsorber( ChillNum ).SizFac;
				if ( ! IndirectAbsorber( ChillNum ).NomCapWasAutoSized ) tmpNomCap = IndirectAbsorber( ChillNum ).NomCap;
			} else {
				if ( IndirectAbsorber( ChillNum ).NomCapWasAutoSized ) tmpNomCap = 0.0;
			}
			if ( PlantFirstSizesOkayToFinalize ) {
				if ( IndirectAbsorber( ChillNum ).NomCapWasAutoSized ) {
					IndirectAbsorber( ChillNum ).NomCap = tmpNomCap;
					if ( PlantFinalSizesOkayToReport ) {
						ReportSizingOutput( "Chiller:Absorption:Indirect", IndirectAbsorber( ChillNum ).Name,
							"Design Size Nominal Capacity [W]", tmpNomCap );
					}
					if ( PlantFirstSizesOkayToReport ) {
						ReportSizingOutput( "Chiller:Absorption:Indirect", IndirectAbsorber( ChillNum ).Name,
							"Initial Design Size Nominal Capacity [W]", tmpNomCap );
					}
				} else {
					if ( IndirectAbsorber( ChillNum ).NomCap > 0.0 && tmpNomCap > 0.0 ) {
						NomCapUser = IndirectAbsorber( ChillNum ).NomCap;
						if ( PlantFinalSizesOkayToReport ) {
							ReportSizingOutput( "Chiller:Absorption:Indirect", IndirectAbsorber( ChillNum ).Name,
								"Design Size Nominal Capacity [W]", tmpNomCap, "User-Specified Nominal Capacity [W]", NomCapUser );
							if ( DisplayExtraWarnings ) {
								if ( ( std::abs( tmpNomCap - NomCapUser ) / NomCapUser ) > AutoVsHardSizingThreshold ) {
									ShowMessage( "SizeChillerAbsorptionIndirect: Potential issue with equipment sizing for " + IndirectAbsorber( ChillNum ).Name );
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
			if ( IndirectAbsorber( ChillNum ).NomCapWasAutoSized ) {
				if ( PlantFirstSizesOkayToFinalize ) {
					ShowSevereError( "Autosizing of Absorption Chiller nominal capacity requires a loop Sizing:Plant object" );
					ShowContinueError( "Occurs in Chiller:Absorption:Indirect object=" + IndirectAbsorber( ChillNum ).Name );
					ErrorsFound = true;
				}
			} else {
				if ( PlantFinalSizesOkayToReport ) {
					if ( IndirectAbsorber( ChillNum ).NomCap > 0.0 ) {
						ReportSizingOutput( "Chiller:Absorption:Indirect", IndirectAbsorber( ChillNum ).Name,
							"User-Specified Nominal Capacity [W]", IndirectAbsorber( ChillNum ).NomCap );
					}
				}
			}
		}

		tmpNomPumpPower = 0.0045 * tmpNomCap;
		if ( PlantFirstSizesOkayToFinalize ) {
			// the DOE-2 EIR for single stage absorption chiller
			if ( IndirectAbsorber( ChillNum ).NomPumpPowerWasAutoSized ) {
				IndirectAbsorber( ChillNum ).NomPumpPower = tmpNomPumpPower; //0.0045d0 * IndirectAbsorber(ChillNum)%NomCap
				if ( PlantFinalSizesOkayToReport ) {
					ReportSizingOutput( "Chiller:Absorption:Indirect", IndirectAbsorber( ChillNum ).Name,
						"Design Size Nominal Pumping Power [W]", tmpNomPumpPower );
				}
				if ( PlantFirstSizesOkayToReport ) {
					ReportSizingOutput( "Chiller:Absorption:Indirect", IndirectAbsorber( ChillNum ).Name,
						"Initial Design Size Nominal Pumping Power [W]", tmpNomPumpPower );
				}
			} else {
				if ( IndirectAbsorber( ChillNum ).NomPumpPower > 0.0 && tmpNomPumpPower > 0.0 ) {
					NomPumpPowerUser = IndirectAbsorber( ChillNum ).NomPumpPower;
					if ( PlantFinalSizesOkayToReport ) {
						ReportSizingOutput( "Chiller:Absorption:Indirect", IndirectAbsorber( ChillNum ).Name,
							"Design Size Nominal Pumping Power [W]", tmpNomPumpPower,
							"User-Specified Nominal Pumping Power [W]", NomPumpPowerUser );
						if ( DisplayExtraWarnings ) {
							if ( ( std::abs( tmpNomPumpPower - NomPumpPowerUser ) / NomPumpPowerUser ) > AutoVsHardSizingThreshold ) {
								ShowMessage( "SizeChillerAbsorptionIndirect: Potential issue with equipment sizing for " + IndirectAbsorber( ChillNum ).Name );
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
				tmpEvapVolFlowRate = PlantSizData( PltSizNum ).DesVolFlowRate * IndirectAbsorber( ChillNum ).SizFac;
				if ( ! IndirectAbsorber( ChillNum ).EvapVolFlowRateWasAutoSized ) tmpEvapVolFlowRate = IndirectAbsorber( ChillNum ).EvapVolFlowRate;
			} else {
				if ( IndirectAbsorber( ChillNum ).EvapVolFlowRateWasAutoSized ) tmpEvapVolFlowRate = 0.0;
			}
			if ( PlantFirstSizesOkayToFinalize ) {
				if ( IndirectAbsorber( ChillNum ).EvapVolFlowRateWasAutoSized ) {
					IndirectAbsorber( ChillNum ).EvapVolFlowRate = tmpEvapVolFlowRate;
					if ( PlantFinalSizesOkayToReport ) {
						ReportSizingOutput( "Chiller:Absorption:Indirect", IndirectAbsorber( ChillNum ).Name,
							"Design Size Design Chilled Water Flow Rate [m3/s]", tmpEvapVolFlowRate );
					}
					if ( PlantFirstSizesOkayToReport ) {
						ReportSizingOutput( "Chiller:Absorption:Indirect", IndirectAbsorber( ChillNum ).Name,
							"Initial Design Size Design Chilled Water Flow Rate [m3/s]", tmpEvapVolFlowRate );
					}
				} else {
					if ( IndirectAbsorber( ChillNum ).EvapVolFlowRate > 0.0 && tmpEvapVolFlowRate > 0.0 ) {
						EvapVolFlowRateUser = IndirectAbsorber( ChillNum ).EvapVolFlowRate;
						if ( PlantFinalSizesOkayToReport ) {
							ReportSizingOutput( "Chiller:Absorption:Indirect", IndirectAbsorber( ChillNum ).Name,
								"Design Size Design Chilled Water Flow Rate [m3/s]", tmpEvapVolFlowRate,
								"User-Specified Design Chilled Water Flow Rate [m3/s]", EvapVolFlowRateUser );
							if ( DisplayExtraWarnings ) {
								if ( ( std::abs( tmpEvapVolFlowRate - EvapVolFlowRateUser ) / EvapVolFlowRateUser ) > AutoVsHardSizingThreshold ) {
									ShowMessage( "SizeChillerElectricIndirect: Potential issue with equipment sizing for " + IndirectAbsorber( ChillNum ).Name );
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
			if ( IndirectAbsorber( ChillNum ).EvapVolFlowRateWasAutoSized ) {
				if ( PlantFirstSizesOkayToFinalize ) {
					ShowSevereError( "Autosizing of Absorption Chiller evap flow rate requires a loop Sizing:Plant object" );
					ShowContinueError( "Occurs in Chiller:Absorption:Indirect object=" + IndirectAbsorber( ChillNum ).Name );
					ErrorsFound = true;
				}
			} else {
				if ( PlantFinalSizesOkayToReport ) {
					if ( IndirectAbsorber( ChillNum ).EvapVolFlowRate > 0.0 ) {
						ReportSizingOutput( "Chiller:Absorption:Indirect", IndirectAbsorber( ChillNum ).Name,
							"User-Specified Design Chilled Water Flow Rate [m3/s]", IndirectAbsorber( ChillNum ).EvapVolFlowRate );
					}
				}
			}
		}

		if ( PlantFirstSizesOkayToFinalize ) {
			RegisterPlantCompDesignFlow( IndirectAbsorber( ChillNum ).EvapInletNodeNum, IndirectAbsorber( ChillNum ).EvapVolFlowRate );
		} else {
			RegisterPlantCompDesignFlow( IndirectAbsorber( ChillNum ).EvapInletNodeNum, tmpEvapVolFlowRate );
		}

		if ( PltSizCondNum > 0 && PltSizNum > 0 ) {
			if ( IndirectAbsorber( ChillNum ).EvapVolFlowRate >= SmallWaterVolFlow && tmpNomCap > 0.0 ) {
				//       QCondenser = QEvaporator + QGenerator + PumpingPower

				Cp = GetSpecificHeatGlycol( PlantLoop( IndirectAbsorber( ChillNum ).CDLoopNum ).FluidName, InitConvTemp, PlantLoop( IndirectAbsorber( ChillNum ).CDLoopNum ).FluidIndex, RoutineName );

				rho = GetDensityGlycol( PlantLoop( IndirectAbsorber( ChillNum ).CDLoopNum ).FluidName, InitConvTemp, PlantLoop( IndirectAbsorber( ChillNum ).CDLoopNum ).FluidIndex, RoutineName );
				tmpCondVolFlowRate = tmpNomCap * ( 1.0 + SteamInputRatNom + tmpNomPumpPower / tmpNomCap ) / ( PlantSizData( PltSizCondNum ).DeltaT * Cp * rho );
				if ( ! IndirectAbsorber( ChillNum ).CondVolFlowRateWasAutoSized ) tmpCondVolFlowRate = IndirectAbsorber( ChillNum ).CondVolFlowRate;
			} else {
				if ( IndirectAbsorber( ChillNum ).CondVolFlowRateWasAutoSized ) tmpCondVolFlowRate = 0.0;
			}
			if ( PlantFirstSizesOkayToFinalize ) {
				if ( IndirectAbsorber( ChillNum ).CondVolFlowRateWasAutoSized ) {
					IndirectAbsorber( ChillNum ).CondVolFlowRate = tmpCondVolFlowRate;
					if ( PlantFinalSizesOkayToReport ) {
						ReportSizingOutput( "Chiller:Absorption:Indirect", IndirectAbsorber( ChillNum ).Name,
							"Design Size Design Condenser Water Flow Rate [m3/s]", tmpCondVolFlowRate );
					}
					if ( PlantFirstSizesOkayToReport ) {
						ReportSizingOutput( "Chiller:Absorption:Indirect", IndirectAbsorber( ChillNum ).Name,
							"Initial Design Size Design Condenser Water Flow Rate [m3/s]", tmpCondVolFlowRate );
					}
				} else {
					if ( IndirectAbsorber( ChillNum ).CondVolFlowRate > 0.0 && tmpCondVolFlowRate > 0.0 ) {
						CondVolFlowRateUser = IndirectAbsorber( ChillNum ).CondVolFlowRate;
						if ( PlantFinalSizesOkayToReport ) {
							ReportSizingOutput( "Chiller:Absorption:Indirect", IndirectAbsorber( ChillNum ).Name,
								"Design Size Design Condenser Water Flow Rate [m3/s]", tmpCondVolFlowRate,
								"User-Specified Design Condenser Water Flow Rate [m3/s]", CondVolFlowRateUser );
							if ( DisplayExtraWarnings ) {
								if ( ( std::abs( tmpCondVolFlowRate - CondVolFlowRateUser ) / CondVolFlowRateUser ) > AutoVsHardSizingThreshold ) {
									ShowMessage( "SizeChillerAbsorptionIndirect: Potential issue with equipment sizing for " + IndirectAbsorber( ChillNum ).Name );
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
			if ( IndirectAbsorber( ChillNum ).CondVolFlowRateWasAutoSized ) {
				if ( PlantFirstSizesOkayToFinalize ) {
					ShowSevereError( "Autosizing of Absorption Chiller condenser flow rate requires a condenser" );
					ShowContinueError( "loop Sizing:Plant object" );
					ShowContinueError( "Occurs in Chiller:Absorption:Indirect object=" + IndirectAbsorber( ChillNum ).Name );
					ErrorsFound = true;
				}
			} else {
				if ( PlantFinalSizesOkayToReport ) {
					if ( IndirectAbsorber( ChillNum ).CondVolFlowRate > 0.0 ) {
						ReportSizingOutput( "Chiller:Absorption:Indirect", IndirectAbsorber( ChillNum ).Name, "User-Specified Design Condenser Water Flow Rate [m3/s]", IndirectAbsorber( ChillNum ).CondVolFlowRate );
					}
				}
			}
		}

		// save the design condenser water volumetric flow rate for use by the condenser water loop sizing algorithms
		if ( PlantFirstSizesOkayToFinalize ) {
			RegisterPlantCompDesignFlow( IndirectAbsorber( ChillNum ).CondInletNodeNum, IndirectAbsorber( ChillNum ).CondVolFlowRate );
		} else {
			RegisterPlantCompDesignFlow( IndirectAbsorber( ChillNum ).CondInletNodeNum, tmpCondVolFlowRate );
		}

		if ( (PltSizSteamNum > 0 && IndirectAbsorber( ChillNum ).GenHeatSourceType == NodeType_Steam) || (PltSizHeatingNum > 0 && IndirectAbsorber( ChillNum ).GenHeatSourceType == NodeType_Water) ) {
			if ( IndirectAbsorber( ChillNum ).EvapVolFlowRate >= SmallWaterVolFlow && tmpNomCap > 0.0 ) {
				if ( IndirectAbsorber( ChillNum ).GenHeatSourceType == NodeType_Water ) {
					CpWater = GetSpecificHeatGlycol( PlantLoop( IndirectAbsorber( ChillNum ).GenLoopNum ).FluidName, PlantSizData( PltSizHeatingNum ).ExitTemp, PlantLoop( IndirectAbsorber( ChillNum ).GenLoopNum ).FluidIndex, RoutineName );
					SteamDeltaT = max( 0.5, PlantSizData( PltSizHeatingNum ).DeltaT );

					RhoWater = GetDensityGlycol( PlantLoop( IndirectAbsorber( ChillNum ).GenLoopNum ).FluidName, ( PlantSizData( PltSizHeatingNum ).ExitTemp - SteamDeltaT ), PlantLoop( IndirectAbsorber( ChillNum ).GenLoopNum ).FluidIndex, RoutineName );
					tmpGeneratorVolFlowRate = ( tmpNomCap * SteamInputRatNom ) / ( CpWater * SteamDeltaT * RhoWater );
					if ( ! IndirectAbsorber( ChillNum ).GeneratorVolFlowRateWasAutoSized ) tmpGeneratorVolFlowRate = IndirectAbsorber( ChillNum ).GeneratorVolFlowRate;
					if ( PlantFirstSizesOkayToFinalize ) {
						if ( IndirectAbsorber( ChillNum ).GeneratorVolFlowRateWasAutoSized ) {
							IndirectAbsorber( ChillNum ).GeneratorVolFlowRate = tmpGeneratorVolFlowRate;
							if ( PlantFinalSizesOkayToReport ) {
								ReportSizingOutput( "Chiller:Absorption:Indirect", IndirectAbsorber( ChillNum ).Name,
									"Design Size Design Generator Fluid Flow Rate [m3/s]", tmpGeneratorVolFlowRate );
							}
							if ( PlantFirstSizesOkayToReport ) {
								ReportSizingOutput( "Chiller:Absorption:Indirect", IndirectAbsorber( ChillNum ).Name,
									"Initial Design Size Design Generator Fluid Flow Rate [m3/s]", tmpGeneratorVolFlowRate );
							}
						} else {
							if ( IndirectAbsorber( ChillNum ).GeneratorVolFlowRate > 0.0 && tmpGeneratorVolFlowRate > 0.0 ) {
								GeneratorVolFlowRateUser = IndirectAbsorber( ChillNum ).GeneratorVolFlowRate;
								if ( PlantFinalSizesOkayToReport ) {
									ReportSizingOutput( "Chiller:Absorption:Indirect", IndirectAbsorber( ChillNum ).Name,
										"Design Size Design Generator Fluid Flow Rate [m3/s]", tmpGeneratorVolFlowRate,
										"User-Specified Design Generator Fluid Flow Rate [m3/s]", GeneratorVolFlowRateUser );
									if ( DisplayExtraWarnings ) {
										if ( ( std::abs( tmpGeneratorVolFlowRate - GeneratorVolFlowRateUser ) / GeneratorVolFlowRateUser ) > AutoVsHardSizingThreshold ) {
											ShowMessage( "SizeChillerAbsorptionIndirect: Potential issue with equipment sizing for " + IndirectAbsorber( ChillNum ).Name );
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
					SteamDensity = GetSatDensityRefrig( fluidNameSteam, PlantSizData( PltSizSteamNum ).ExitTemp, 1.0, IndirectAbsorber( ChillNum ).SteamFluidIndex, SizeChillerAbsorptionIndirect + IndirectAbsorber( ChillNum ).Name );
					SteamDeltaT = PlantSizData( PltSizSteamNum ).DeltaT;
					GeneratorOutletTemp = PlantSizData( PltSizSteamNum ).ExitTemp - SteamDeltaT;

					EnthSteamOutDry = GetSatEnthalpyRefrig( fluidNameSteam, PlantSizData( PltSizSteamNum ).ExitTemp, 1.0, IndirectAbsorber( ChillNum ).SteamFluidIndex, SizeChillerAbsorptionIndirect + IndirectAbsorber( ChillNum ).Name );
					EnthSteamOutWet = GetSatEnthalpyRefrig( fluidNameSteam, PlantSizData( PltSizSteamNum ).ExitTemp, 0.0, IndirectAbsorber( ChillNum ).SteamFluidIndex, SizeChillerAbsorptionIndirect + IndirectAbsorber( ChillNum ).Name );
					CpWater = GetSpecificHeatGlycol( fluidNameWater, GeneratorOutletTemp, DummWaterIndex, RoutineName );
					HfgSteam = EnthSteamOutDry - EnthSteamOutWet;
					//         calculate the mass flow rate through the generator
					SteamMassFlowRate = ( tmpNomCap * SteamInputRatNom ) / ( ( HfgSteam ) + ( SteamDeltaT * CpWater ) );
					//         calculate the steam volumetric flow rate
					tmpGeneratorVolFlowRate = SteamMassFlowRate / SteamDensity;
					if ( ! IndirectAbsorber( ChillNum ).GeneratorVolFlowRateWasAutoSized ) tmpGeneratorVolFlowRate = IndirectAbsorber( ChillNum ).GeneratorVolFlowRate;
					if ( PlantFirstSizesOkayToFinalize ) {
						if ( IndirectAbsorber( ChillNum ).GeneratorVolFlowRateWasAutoSized ) {
							IndirectAbsorber( ChillNum ).GeneratorVolFlowRate = tmpGeneratorVolFlowRate;
							if ( PlantFinalSizesOkayToReport ) {
								ReportSizingOutput( "Chiller:Absorption:Indirect", IndirectAbsorber( ChillNum ).Name,
									"Design Size Design Generator Fluid Flow Rate [m3/s]", tmpGeneratorVolFlowRate );
							}
							if ( PlantFirstSizesOkayToReport ) {
								ReportSizingOutput( "Chiller:Absorption:Indirect", IndirectAbsorber( ChillNum ).Name,
									"Initial Design Size Design Generator Fluid Flow Rate [m3/s]", tmpGeneratorVolFlowRate );
							}
						} else {
							if ( IndirectAbsorber( ChillNum ).GeneratorVolFlowRate > 0.0 && tmpGeneratorVolFlowRate > 0.0 ) {
								GeneratorVolFlowRateUser = IndirectAbsorber( ChillNum ).GeneratorVolFlowRate;
								if ( PlantFinalSizesOkayToReport ) {
									ReportSizingOutput( "Chiller:Absorption:Indirect", IndirectAbsorber( ChillNum ).Name,
										"Design Size Design Generator Fluid Flow Rate [m3/s]", tmpGeneratorVolFlowRate,
										"User-Specified Design Generator Fluid Flow Rate [m3/s]", GeneratorVolFlowRateUser );
									if ( DisplayExtraWarnings ) {
										if ( ( std::abs( tmpGeneratorVolFlowRate - GeneratorVolFlowRateUser ) / GeneratorVolFlowRateUser ) > AutoVsHardSizingThreshold ) {
											ShowMessage( "SizeChillerAbsorptionIndirect: Potential issue with equipment sizing for " + IndirectAbsorber( ChillNum ).Name );
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
				if ( IndirectAbsorber( ChillNum ).GeneratorVolFlowRateWasAutoSized ) {
					if ( PlantFirstSizesOkayToFinalize ) {
						IndirectAbsorber( ChillNum ).GeneratorVolFlowRate = 0.0;
					} else {
						tmpGeneratorVolFlowRate = 0.0;
					}
				}
			}
		} else {
			if ( IndirectAbsorber( ChillNum ).GeneratorVolFlowRateWasAutoSized ) {
				if ( PlantFirstSizesOkayToFinalize ) {
					ShowSevereError( "Autosizing of Absorption Chiller generator flow rate requires a loop Sizing:Plant object." );
					ShowContinueError( " For steam loops, use a steam Sizing:Plant object." );
					ShowContinueError( " For hot water loops, use a heating Sizing:Plant object." );
					ShowContinueError( "Occurs in Chiller:Absorption:Indirect object=" + IndirectAbsorber( ChillNum ).Name );
					ErrorsFound = true;
				}
			} else {
				if ( PlantFinalSizesOkayToReport ) {
					if ( IndirectAbsorber( ChillNum ).GeneratorVolFlowRate > 0.0 ) {
						ReportSizingOutput( "Chiller:Absorption:Indirect", IndirectAbsorber( ChillNum ).Name,
						"User-Specified Design Generator Fluid Flow Rate [m3/s]", IndirectAbsorber( ChillNum ).GeneratorVolFlowRate );
					}
				}
			}
		}

		// save the design steam or hot water volumetric flow rate for use by the steam or hot water loop sizing algorithms
		if ( PlantFirstSizesOkayToFinalize ) {
			RegisterPlantCompDesignFlow( IndirectAbsorber( ChillNum ).GeneratorInletNodeNum, IndirectAbsorber( ChillNum ).GeneratorVolFlowRate );
		} else {
			RegisterPlantCompDesignFlow( IndirectAbsorber( ChillNum ).GeneratorInletNodeNum, tmpGeneratorVolFlowRate );
		}

		if ( IndirectAbsorber( ChillNum ).GeneratorDeltaTempWasAutoSized ) {
			if ( PltSizHeatingNum > 0 && IndirectAbsorber( ChillNum ).GenHeatSourceType == NodeType_Water ) {
				IndirectAbsorber( ChillNum ).GeneratorDeltaTemp = max( 0.5, PlantSizData( PltSizHeatingNum ).DeltaT );
			} else if ( IndirectAbsorber( ChillNum ).GenHeatSourceType == NodeType_Water ) {
				rho = GetDensityGlycol( PlantLoop( IndirectAbsorber( ChillNum ).GenLoopNum ).FluidName, InitConvTemp, PlantLoop( IndirectAbsorber( ChillNum ).GenLoopNum ).FluidIndex, RoutineName );
				CpWater = GetSpecificHeatGlycol( PlantLoop( IndirectAbsorber( ChillNum ).GenLoopNum ).FluidName, PlantSizData( PltSizHeatingNum ).ExitTemp, PlantLoop( IndirectAbsorber( ChillNum ).GenLoopNum ).FluidIndex, RoutineName );
				if ( PlantFirstSizesOkayToFinalize ) {
					IndirectAbsorber( ChillNum ).GeneratorDeltaTemp = ( SteamInputRatNom * IndirectAbsorber( ChillNum ).NomCap ) / ( CpWater * rho * IndirectAbsorber( ChillNum ).GeneratorVolFlowRate );
				}
			}
		}

		if ( ErrorsFound ) {
			ShowFatalError( "Preceding sizing errors cause program termination" );
		}

		if ( PlantFinalSizesOkayToReport ) {
			//create predefined report
			equipName = IndirectAbsorber( ChillNum ).Name;
			PreDefTableEntry( pdchMechType, equipName, "Chiller:Absorption:Indirect" );
			PreDefTableEntry( pdchMechNomEff, equipName, "n/a" );
			PreDefTableEntry( pdchMechNomCap, equipName, IndirectAbsorber( ChillNum ).NomCap );
		}

	}

	// Beginning of Absorber model Subroutines
	// *****************************************************************************

	void
	CalcIndirectAbsorberModel(
		int const ChillNum, // Absorber number
		Real64 const MyLoad, // operating load
		bool const RunFlag, // TRUE when Absorber operating
		bool const EP_UNUSED( FirstIteration ), // TRUE when first iteration of timestep !unused1208
		int const EquipFlowCtrl // Flow control mode for the equipment
	)
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         R. Raustad (FSEC)
		//       DATE WRITTEN   May 2008
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// simulate a vapor compression Absorber using a revised BLAST model

		// METHODOLOGY EMPLOYED:
		// curve fit of performance data:

		// REFERENCES:
		// 1.  BLAST User Manual
		// 2.  Absorber User Manual

		// Using/Aliasing
		using namespace FluidProperties;
		using General::TrimSigDigits;
		using General::RoundSigDigits;
		using DataPlant::DeltaTempTol;
		using DataPlant::PlantLoop;
		using DataPlant::CompSetPtBasedSchemeType;
		using DataPlant::SingleSetPoint;
		using DataPlant::DualSetPointDeadBand;
		using DataBranchAirLoopPlant::ControlType_SeriesActive;
		using DataBranchAirLoopPlant::MassFlowTolerance;
		using DataGlobals::BeginEnvrnFlag;
		using DataGlobals::SecInHour;
		using DataGlobals::WarmupFlag;
		using CurveManager::CurveValue;
		using DataHVACGlobals::TimeStepSys;
		using DataEnvironment::OutBaroPress;
		using PlantUtilities::SetComponentFlowRate;
		using PlantUtilities::RegisterPlantCompDesignFlow;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "CalcIndirectAbsorberModel" );
		static std::string const LoopLossesChillerAbsorptionIndirect( "Loop Losses: Chiller:Absorption:Indirect" );
		static std::string const LoopLossesChillerAbsorptionIndirectSpace( "Loop Losses: Chiller:Absorption:Indirect " );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 MinPartLoadRat; // min allowed operating frac full load
		Real64 MaxPartLoadRat; // max allowed operating frac full load
		Real64 TempCondIn; // C - (BLAST ADJTC(1)The design secondary loop fluid
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
		Real64 HeatInputRat; // genertaor heat input ratio
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
		static bool OneTimeFlag( true );
		Real64 FRAC; // fraction of time step chiller cycles
		static bool PossibleSubcooling; // flag to determine if supply water temperature is below setpoint
		Real64 CpFluid; // specific heat of generator fluid
		Real64 SteamDeltaT; // temperature difference of fluid through generator
		Real64 SteamOutletTemp; // generator outlet temperature
		Real64 CapacityfAbsorberTemp; // performance curve output
		Real64 CapacityfEvaporatorTemp; // performance curve output
		Real64 CapacityfGeneratorTemp; // performance curve output
		Real64 HeatInputfCondTemp; // performance curve output
		Real64 HeatInputfEvapTemp; // performance curve output
		Real64 TempWaterAtmPress; // temperature of condensed steam leaving generator (after condensate trap)
		Real64 TempLoopOutToPump; // temperature of condensed steam entering pump (includes loop losses)
		Real64 EnthAtAtmPress; // enthalpy  of condensed steam leaving generator (after condensate trap)
		Real64 EnthPumpInlet; // enthalpy of condensed steam entering pump (includes loop losses)
		int LoopSideNum;
		int LoopNum;
		static int DummyWaterIndex( 1 );

		if ( OneTimeFlag ) {
			MyEnvironFlag.allocate( NumIndirectAbsorbers );
			MyEnvironSteamFlag.allocate( NumIndirectAbsorbers );
			MyEnvironFlag = true;
			MyEnvironSteamFlag = true;
			OneTimeFlag = false;
		}

		//set module level inlet and outlet nodes
		EvapMassFlowRate = 0.0;
		CondMassFlowRate = 0.0;
		GenMassFlowRate = 0.0;
		QCondenser = 0.0;
		QEvaporator = 0.0;
		QGenerator = 0.0;
		PumpingEnergy = 0.0;
		CondenserEnergy = 0.0;
		EvaporatorEnergy = 0.0;
		GeneratorEnergy = 0.0;
		PumpingPower = 0.0;
		FRAC = 1.0;
		ChillerONOFFCyclingFrac = 0.0;
		EvapInletNode = IndirectAbsorber( ChillNum ).EvapInletNodeNum;
		EvapOutletNode = IndirectAbsorber( ChillNum ).EvapOutletNodeNum;
		CondInletNode = IndirectAbsorber( ChillNum ).CondInletNodeNum;
		CondOutletNode = IndirectAbsorber( ChillNum ).CondOutletNodeNum;
		GeneratorInletNode = IndirectAbsorber( ChillNum ).GeneratorInletNodeNum;
		GeneratorOutletNode = IndirectAbsorber( ChillNum ).GeneratorOutletNodeNum;

		//  If no loop demand or Absorber OFF, return
		if ( MyLoad >= 0.0 || ! RunFlag ) {
			if ( EquipFlowCtrl == ControlType_SeriesActive ) EvapMassFlowRate = Node( EvapInletNode ).MassFlowRate;
			return;
		}

		// Warn if entering condenser water temperature is below minimum
		if ( Node( CondInletNode ).Temp < IndirectAbsorber( ChillNum ).MinCondInletTemp ) {
			if ( ! WarmupFlag ) {
				if ( IndirectAbsorber( ChillNum ).MinCondInletTempCtr < 1 ) {
					++IndirectAbsorber( ChillNum ).MinCondInletTempCtr;
					ShowWarningError( "Chiller:Absorption:Indirect \"" + IndirectAbsorber( ChillNum ).Name + "\"" );
					ShowContinueError( "...Entering condenser water temperature below specified minimum (" + RoundSigDigits( IndirectAbsorber( ChillNum ).MinCondInletTemp, 3 ) + " C)." );
					ShowContinueError( "...Entering condenser water temperature = " + RoundSigDigits( Node( CondInletNode ).Temp, 3 ) + " C." );
					ShowContinueErrorTimeStamp( "...simulation continues." );
				} else {
					ShowRecurringWarningErrorAtEnd( "Entering condenser water temperature below specified minimum error continues.", IndirectAbsorber( ChillNum ).MinCondInletTempIndex, Node( CondInletNode ).Temp, Node( CondInletNode ).Temp );
				}
			}
		}

		// Warn if entering generator fluid temperature is below minimum
		if ( GeneratorInletNode > 0 ) {
			if ( Node( GeneratorInletNode ).Temp < IndirectAbsorber( ChillNum ).MinGeneratorInletTemp ) {
				if ( ! WarmupFlag ) {
					if ( IndirectAbsorber( ChillNum ).MinGenInletTempCtr < 1 ) {
						++IndirectAbsorber( ChillNum ).MinGenInletTempCtr;
						ShowWarningError( "Chiller:Absorption:Indirect \"" + IndirectAbsorber( ChillNum ).Name + "\"" );
						ShowContinueError( "...Entering generator fluid temperature below specified minimum (" + RoundSigDigits( IndirectAbsorber( ChillNum ).MinGeneratorInletTemp, 3 ) + " C)." );
						ShowContinueError( "...Entering generator fluid temperature = " + RoundSigDigits( Node( GeneratorInletNode ).Temp, 3 ) + " C." );
						ShowContinueErrorTimeStamp( "...simulation continues." );
					} else {
						ShowRecurringWarningErrorAtEnd( "Entering generator fluid temperature below specified minimum error continues.", IndirectAbsorber( ChillNum ).MinGenInletTempIndex, Node( GeneratorInletNode ).Temp, Node( GeneratorInletNode ).Temp );
					}
				}
			}
		}

		// Set module level Absorber inlet and temperature variables
		EvapInletTemp = Node( EvapInletNode ).Temp;
		CondInletTemp = Node( CondInletNode ).Temp;

		// Set the condenser mass flow rates
		CondMassFlowRate = Node( CondInletNode ).MassFlowRate;

		// LOAD LOCAL VARIABLES FROM DATA STRUCTURE (for code readability)
		MinPartLoadRat = IndirectAbsorber( ChillNum ).MinPartLoadRat;
		MaxPartLoadRat = IndirectAbsorber( ChillNum ).MaxPartLoadRat;
		AbsorberNomCap = IndirectAbsorber( ChillNum ).NomCap;
		NomPumpPower = IndirectAbsorber( ChillNum ).NomPumpPower;
		TempCondIn = Node( IndirectAbsorber( ChillNum ).CondInletNodeNum ).Temp;
		TempEvapOut = Node( IndirectAbsorber( ChillNum ).EvapOutletNodeNum ).Temp;
		TempLowLimitEout = IndirectAbsorber( ChillNum ).TempLowLimitEvapOut;
		LoopNum = IndirectAbsorber( ChillNum ).CWLoopNum;
		LoopSideNum = IndirectAbsorber( ChillNum ).CWLoopSideNum;

		CpFluid = GetSpecificHeatGlycol( PlantLoop( IndirectAbsorber( ChillNum ).CWLoopNum ).FluidName, EvapInletTemp, PlantLoop( IndirectAbsorber( ChillNum ).CWLoopNum ).FluidIndex, RoutineName );

		if ( IndirectAbsorber( ChillNum ).CapFCondenserTempPtr > 0 ) {
			CapacityfAbsorberTemp = CurveValue( IndirectAbsorber( ChillNum ).CapFCondenserTempPtr, TempCondIn );
		} else {
			CapacityfAbsorberTemp = 1.0;
		}
		if ( IndirectAbsorber( ChillNum ).CapFEvaporatorTempPtr > 0 ) {
			CapacityfEvaporatorTemp = CurveValue( IndirectAbsorber( ChillNum ).CapFEvaporatorTempPtr, TempEvapOut );
		} else {
			CapacityfEvaporatorTemp = 1.0;
		}
		if ( IndirectAbsorber( ChillNum ).CapFGeneratorTempPtr > 0 ) {
			if ( GeneratorInletNode > 0 ) {
				if ( IndirectAbsorber( ChillNum ).GenHeatSourceType == NodeType_Water ) {
					CapacityfGeneratorTemp = CurveValue( IndirectAbsorber( ChillNum ).CapFGeneratorTempPtr, Node( GeneratorInletNode ).Temp );
				} else {
					CapacityfGeneratorTemp = 1.0;
				}
			} else {
				CapacityfGeneratorTemp = 1.0;
			}
		} else {
			CapacityfGeneratorTemp = 1.0;
		}

		AbsorberNomCap *= CapacityfAbsorberTemp * CapacityfEvaporatorTemp * CapacityfGeneratorTemp;

		// If FlowLock is True, the new resolved mdot is used to update Power, QEvap, Qcond, and
		// condenser side outlet temperature.
		if ( PlantLoop( LoopNum ).LoopSide( LoopSideNum ).FlowLock == 0 ) {
			IndirectAbsorber( ChillNum ).PossibleSubcooling = false;
			QEvaporator = std::abs( MyLoad );

			// Either set the flow to the Constant value or caluclate the flow for the variable volume
			if ( ( IndirectAbsorber( ChillNum ).FlowMode == ConstantFlow ) || ( IndirectAbsorber( ChillNum ).FlowMode == NotModulated ) ) {
				EvapMassFlowRate = Node( EvapInletNode ).MassFlowRate;

				if ( EvapMassFlowRate != 0.0 ) {
					EvapDeltaTemp = QEvaporator / EvapMassFlowRate / CpFluid;
				} else {
					EvapDeltaTemp = 0.0;
				}
				EvapOutletTemp = Node( EvapInletNode ).Temp - EvapDeltaTemp;

			} else if ( IndirectAbsorber( ChillNum ).FlowMode == LeavingSetPointModulated ) {
				// Calculate the Delta Temp from the inlet temp to the chiller outlet setpoint
				{ auto const SELECT_CASE_var( PlantLoop( IndirectAbsorber( ChillNum ).CWLoopNum ).LoopDemandCalcScheme );
				if ( SELECT_CASE_var == SingleSetPoint ) {
					EvapDeltaTemp = Node( EvapInletNode ).Temp - Node( EvapOutletNode ).TempSetPoint;
				} else if ( SELECT_CASE_var == DualSetPointDeadBand ) {
					EvapDeltaTemp = Node( EvapInletNode ).Temp - Node( EvapOutletNode ).TempSetPointHi;
				} else {
					assert( false );
				}}

				if ( EvapDeltaTemp != 0 ) {
					EvapMassFlowRate = std::abs( QEvaporator / CpFluid / EvapDeltaTemp );
					if ( ( EvapMassFlowRate - IndirectAbsorber( ChillNum ).EvapMassFlowRateMax ) > MassFlowTolerance ) PossibleSubcooling = true;
					//Check to see if the Maximum is exceeded, if so set to maximum
					EvapMassFlowRate = min( IndirectAbsorber( ChillNum ).EvapMassFlowRateMax, EvapMassFlowRate );
					SetComponentFlowRate( EvapMassFlowRate, IndirectAbsorber( ChillNum ).EvapInletNodeNum, IndirectAbsorber( ChillNum ).EvapOutletNodeNum, IndirectAbsorber( ChillNum ).CWLoopNum, IndirectAbsorber( ChillNum ).CWLoopSideNum, IndirectAbsorber( ChillNum ).CWBranchNum, IndirectAbsorber( ChillNum ).CWCompNum );
					{ auto const SELECT_CASE_var( PlantLoop( IndirectAbsorber( ChillNum ).CWLoopNum ).LoopDemandCalcScheme );
					if ( SELECT_CASE_var == SingleSetPoint ) {
						EvapOutletTemp = Node( EvapOutletNode ).TempSetPoint;
					} else if ( SELECT_CASE_var == DualSetPointDeadBand ) {
						EvapOutletTemp = Node( EvapOutletNode ).TempSetPointHi;
					}}
				} else {
					EvapMassFlowRate = 0.0;
					EvapOutletTemp = Node( EvapInletNode ).Temp;

					ShowRecurringWarningErrorAtEnd( "CalcIndirectAbsorberModel: Name=\"" + IndirectAbsorber( ChillNum ).Name + "\" Evaporative Condenser Delta Temperature = 0 in mass flow calculation.", IndirectAbsorber( ChillNum ).ErrCount2 );
				}
			} //End of Constant Variable Flow If Block
		} else { // If FlowLock is True

			EvapMassFlowRate = Node( EvapInletNode ).MassFlowRate;
			if ( PossibleSubcooling ) {
				QEvaporator = std::abs( MyLoad );
				EvapDeltaTemp = QEvaporator / EvapMassFlowRate / CpFluid;
				EvapOutletTemp = Node( EvapInletNode ).Temp - EvapDeltaTemp;
			} else {
				{ auto const SELECT_CASE_var( PlantLoop( LoopNum ).LoopDemandCalcScheme );
				if ( SELECT_CASE_var == SingleSetPoint ) {
					if ( ( IndirectAbsorber( ChillNum ).FlowMode == LeavingSetPointModulated ) || ( PlantLoop( LoopNum ).LoopSide( LoopSideNum ).Branch( IndirectAbsorber( ChillNum ).CWBranchNum ).Comp( IndirectAbsorber( ChillNum ).CWCompNum ).CurOpSchemeType == CompSetPtBasedSchemeType ) || ( Node( EvapOutletNode ).TempSetPoint != SensedNodeFlagValue ) ) {
						TempEvapOutSetPoint = Node( EvapOutletNode ).TempSetPoint;
					} else {
						TempEvapOutSetPoint = Node( PlantLoop( LoopNum ).TempSetPointNodeNum ).TempSetPoint;
					}
				} else if ( SELECT_CASE_var == DualSetPointDeadBand ) {
					if ( ( IndirectAbsorber( ChillNum ).FlowMode == LeavingSetPointModulated ) || ( PlantLoop( LoopNum ).LoopSide( LoopSideNum ).Branch( IndirectAbsorber( ChillNum ).CWBranchNum ).Comp( IndirectAbsorber( ChillNum ).CWCompNum ).CurOpSchemeType == CompSetPtBasedSchemeType ) || ( Node( EvapOutletNode ).TempSetPointHi != SensedNodeFlagValue ) ) {
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

		OperPartLoadRat = QEvaporator / AbsorberNomCap;
		PartLoadRat = max( MinPartLoadRat, OperPartLoadRat );
		IndirectAbsorberReport( ChillNum ).ChillerPartLoadRatio = OperPartLoadRat;

		if ( OperPartLoadRat < PartLoadRat ) {
			FRAC = min( 1.0, OperPartLoadRat / MinPartLoadRat );
		} else {
			FRAC = 1.0;
		}

		ChillerONOFFCyclingFrac = FRAC;

		if ( GeneratorInletNode > 0 ) {
			if ( IndirectAbsorber( ChillNum ).HeatInputFCondTempPtr > 0 ) {
				HeatInputfCondTemp = CurveValue( IndirectAbsorber( ChillNum ).HeatInputFCondTempPtr, Node( GeneratorInletNode ).Temp );
			} else {
				HeatInputfCondTemp = 1.0;
			}
		} else {
			HeatInputfCondTemp = 1.0;
		}
		if ( IndirectAbsorber( ChillNum ).HeatInputFEvapTempPtr > 0 ) {
			HeatInputfEvapTemp = CurveValue( IndirectAbsorber( ChillNum ).HeatInputFEvapTempPtr, Node( EvapOutletNode ).Temp );
		} else {
			HeatInputfEvapTemp = 1.0;
		}

		//Calculate steam input ratio. Inlcude impact of generator and evaporator temperatures
		if ( IndirectAbsorber( ChillNum ).GeneratorInputCurvePtr > 0 ) {
			HeatInputRat = CurveValue( IndirectAbsorber( ChillNum ).GeneratorInputCurvePtr, PartLoadRat ) * HeatInputfCondTemp * HeatInputfEvapTemp;
		} else {
			HeatInputRat = HeatInputfCondTemp * HeatInputfEvapTemp;
		}

		//Calculate electric input ratio
		if ( IndirectAbsorber( ChillNum ).PumpPowerCurvePtr > 0 ) {
			ElectricInputRat = CurveValue( IndirectAbsorber( ChillNum ).PumpPowerCurvePtr, PartLoadRat );
		} else {
			ElectricInputRat = 1.0;
		}

		QGenerator = HeatInputRat * AbsorberNomCap * FRAC;
		PumpingPower = ElectricInputRat * NomPumpPower * FRAC;

		if ( EvapMassFlowRate == 0.0 ) {
			QGenerator = 0.0;
			EvapOutletTemp = Node( EvapInletNode ).Temp;
			PumpingPower = 0.0;
		}

		QCondenser = QEvaporator + QGenerator + PumpingPower;

		CpFluid = GetSpecificHeatGlycol( PlantLoop( IndirectAbsorber( ChillNum ).CDLoopNum ).FluidName, CondInletTemp, PlantLoop( IndirectAbsorber( ChillNum ).CDLoopNum ).FluidIndex, RoutineName );

		if ( CondMassFlowRate > MassFlowTolerance ) {
			CondOutletTemp = QCondenser / CondMassFlowRate / CpFluid + CondInletTemp;
		} else {
			CondOutletTemp = CondInletTemp;
			CondMassFlowRate = 0.0;
			QCondenser = 0.0;
			return;
			//V7 plant upgrade, no longer fatal here anymore... set some things and return
		}

		if ( GeneratorInletNode > 0 ) {
			//   Hot water plant is used for the generator
			if ( IndirectAbsorber( ChillNum ).GenHeatSourceType == NodeType_Water ) {

				CpFluid = GetSpecificHeatGlycol( PlantLoop( IndirectAbsorber( ChillNum ).GenLoopNum ).FluidName, Node( GeneratorInletNode ).Temp, PlantLoop( IndirectAbsorber( ChillNum ).GenLoopNum ).FluidIndex, RoutineName );
				if ( ( IndirectAbsorber( ChillNum ).FlowMode == ConstantFlow ) || ( IndirectAbsorber( ChillNum ).FlowMode == NotModulated ) ) {
					GenMassFlowRate = IndirectAbsorber( ChillNum ).GenMassFlowRateMax;
				} else {
					GenMassFlowRate = QGenerator / CpFluid / IndirectAbsorber( ChillNum ).GeneratorDeltaTemp;
				}

				SetComponentFlowRate( GenMassFlowRate, GeneratorInletNode, GeneratorOutletNode, IndirectAbsorber( ChillNum ).GenLoopNum, IndirectAbsorber( ChillNum ).GenLoopSideNum, IndirectAbsorber( ChillNum ).GenBranchNum, IndirectAbsorber( ChillNum ).GenCompNum );

				if ( GenMassFlowRate <= 0.0 ) {
					GenOutletTemp = Node( GeneratorInletNode ).Temp;
					SteamOutletEnthalpy = Node( GeneratorInletNode ).Enthalpy;
				} else {
					GenOutletTemp = Node( GeneratorInletNode ).Temp - QGenerator / ( CpFluid * GenMassFlowRate );
					SteamOutletEnthalpy = Node( GeneratorInletNode ).Enthalpy - QGenerator / GenMassFlowRate;
				}

			} else { // using a steam plant for the generator

				EnthSteamOutDry = GetSatEnthalpyRefrig( fluidNameSteam, Node( GeneratorInletNode ).Temp, 1.0, IndirectAbsorber( ChillNum ).SteamFluidIndex, calcChillerAbsorptionIndirect + IndirectAbsorber( ChillNum ).Name );
				EnthSteamOutWet = GetSatEnthalpyRefrig( fluidNameSteam, Node( GeneratorInletNode ).Temp, 0.0, IndirectAbsorber( ChillNum ).SteamFluidIndex, calcChillerAbsorptionIndirect + IndirectAbsorber( ChillNum ).Name );
				SteamDeltaT = IndirectAbsorber( ChillNum ).GeneratorSubcool;
				SteamOutletTemp = Node( GeneratorInletNode ).Temp - SteamDeltaT;
				HfgSteam = EnthSteamOutDry - EnthSteamOutWet;
				CpFluid = GetSpecificHeatGlycol( fluidNameWater, SteamOutletTemp, DummyWaterIndex, calcChillerAbsorptionIndirect + IndirectAbsorber( ChillNum ).Name );
				GenMassFlowRate = QGenerator / ( HfgSteam + CpFluid * SteamDeltaT );
				SetComponentFlowRate( GenMassFlowRate, GeneratorInletNode, GeneratorOutletNode, IndirectAbsorber( ChillNum ).GenLoopNum, IndirectAbsorber( ChillNum ).GenLoopSideNum, IndirectAbsorber( ChillNum ).GenBranchNum, IndirectAbsorber( ChillNum ).GenCompNum );

				if ( GenMassFlowRate <= 0.0 ) {
					GenOutletTemp = Node( GeneratorInletNode ).Temp;
					SteamOutletEnthalpy = Node( GeneratorInletNode ).Enthalpy;
				} else {
					GenOutletTemp = Node( GeneratorInletNode ).Temp - SteamDeltaT;
					SteamOutletEnthalpy = GetSatEnthalpyRefrig( fluidNameSteam, Node( GeneratorInletNode ).Temp, 0.0, IndirectAbsorber( ChillNum ).SteamFluidIndex, LoopLossesChillerAbsorptionIndirect + IndirectAbsorber( ChillNum ).Name );
					CpFluid = GetSpecificHeatGlycol( fluidNameWater, Node( GeneratorInletNode ).Temp, DummyWaterIndex, calcChillerAbsorptionIndirect + IndirectAbsorber( ChillNum ).Name );

					SteamOutletEnthalpy -= CpFluid * SteamDeltaT;

					//************************* Loop Losses *****************************
					TempWaterAtmPress = GetSatTemperatureRefrig( fluidNameSteam, OutBaroPress, IndirectAbsorber( ChillNum ).SteamFluidIndex, LoopLossesChillerAbsorptionIndirect + IndirectAbsorber( ChillNum ).Name );

					EnthAtAtmPress = GetSatEnthalpyRefrig( fluidNameSteam, TempWaterAtmPress, 0.0, IndirectAbsorber( ChillNum ).SteamFluidIndex, LoopLossesChillerAbsorptionIndirectSpace + IndirectAbsorber( ChillNum ).Name );

					// Point 4 at atm - loop delta subcool during return journery back to pump
					TempLoopOutToPump = TempWaterAtmPress - IndirectAbsorber( ChillNum ).LoopSubcool;

					// Reported value of coil outlet enthalpy at the node to match the node outlet temperature
					EnthPumpInlet = EnthAtAtmPress - CpFluid * IndirectAbsorber( ChillNum ).LoopSubcool;

					// Point 3-Point 5,
					EnergyLossToEnvironment = GenMassFlowRate * ( SteamOutletEnthalpy - EnthPumpInlet );

					//************************* Loop Losses *****************************

					GenOutletTemp = TempLoopOutToPump;
					SteamOutletEnthalpy = EnthPumpInlet;

				} // IF(GenMassFlowRate .LE. 0.0d0)THEN

			} // IF(IndirectAbsorber(ChillNum)%GenHeatSourceType == NodeType_Water)THEN

		} // IF(GeneratorInletNode .GT. 0)THEN

		//convert power to energy
		GeneratorEnergy = QGenerator * TimeStepSys * SecInHour;
		EvaporatorEnergy = QEvaporator * TimeStepSys * SecInHour;
		CondenserEnergy = QCondenser * TimeStepSys * SecInHour;
		PumpingEnergy = PumpingPower * TimeStepSys * SecInHour;

		return;

		//                              ------
		//                            /        \.
		//                          /           |
		//                       6/-------------1 - Boiler Outlet Temp/Enthalpy/Pressure
		//                    /  /             /.
		//                 /    /             / . \_
		//               /    /              /  .  _pressure drop (PD) across steam pressure regulator
		// P           /     /              /   . /
		// r         5      /              /    .
		// e        /    3-2'-------------2------ - Generator Inlet Temp/Enthalpy/Pressure
		// s       /     |/              /
		// s      /      |  PD across   /      2-2' latent heat of vaporization (neglecting amount of superheat due to PD)
		// u     /      /| condensate  /       1-3  delta H in generator
		// r    /      / |   trap     /        2'-3 subcooling of hot water in generator
		// e   4------/--3'          /         3-3' pressure drop at generator hot-water condensate trap
		//           /              /          3-4  loop subcooling back to loop pump
		//          /              /           4-5  pressure/temp/enthalpy increase due to loop condensate pump
		//         /              /            5-6  heat addition in boiler to return condensate
		//        /              /             6-1  heat of vaporization in boiler of return condensate to steam
		//____________________________________
		//         Enthalpy (H)

	}

	// End of Absorption Chiller Module Utility Subroutines
	// *****************************************************************************

	// Beginning of Record Keeping subroutines for the Absorption Chiller Module
	// *****************************************************************************

	void
	UpdateIndirectAbsorberRecords(
		Real64 const MyLoad, // current load
		bool const RunFlag, // TRUE if Absorber operating
		int const Num // Absorber number
	)
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR:          R. Raustad (FSEC)
		//       DATE WRITTEN:    May 2008

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

		EvapInletNode = IndirectAbsorber( Num ).EvapInletNodeNum;
		EvapOutletNode = IndirectAbsorber( Num ).EvapOutletNodeNum;
		CondInletNode = IndirectAbsorber( Num ).CondInletNodeNum;
		CondOutletNode = IndirectAbsorber( Num ).CondOutletNodeNum;
		GeneratorInletNode = IndirectAbsorber( Num ).GeneratorInletNodeNum;
		GeneratorOutletNode = IndirectAbsorber( Num ).GeneratorOutletNodeNum;

		if ( MyLoad >= 0 || ! RunFlag ) {
			//set node temperature
			SafeCopyPlantNode( EvapInletNode, EvapOutletNode );
			SafeCopyPlantNode( CondInletNode, CondOutletNode );

			IndirectAbsorberReport( Num ).PumpingPower = 0.0;
			IndirectAbsorberReport( Num ).QEvap = 0.0;
			IndirectAbsorberReport( Num ).QCond = 0.0;
			IndirectAbsorberReport( Num ).QGenerator = 0.0;
			IndirectAbsorberReport( Num ).PumpingEnergy = 0.0;
			IndirectAbsorberReport( Num ).EvapEnergy = 0.0;
			IndirectAbsorberReport( Num ).CondEnergy = 0.0;
			IndirectAbsorberReport( Num ).GeneratorEnergy = 0.0;
			IndirectAbsorberReport( Num ).EvapInletTemp = Node( EvapInletNode ).Temp;
			IndirectAbsorberReport( Num ).CondInletTemp = Node( CondInletNode ).Temp;
			IndirectAbsorberReport( Num ).CondOutletTemp = Node( CondOutletNode ).Temp;
			IndirectAbsorberReport( Num ).EvapOutletTemp = Node( EvapOutletNode ).Temp;
			IndirectAbsorberReport( Num ).Evapmdot = 0.0;
			IndirectAbsorberReport( Num ).Condmdot = 0.0;
			IndirectAbsorberReport( Num ).Genmdot = 0.0;
			IndirectAbsorberReport( Num ).ActualCOP = 0.0;
			IndirectAbsorberReport( Num ).ChillerPartLoadRatio = 0.0;
			IndirectAbsorberReport( Num ).LoopLoss = 0.0;
			IndirectAbsorberReport( Num ).ChillerCyclingFrac = 0.0;

			if ( GeneratorInletNode > 0 ) {
				SafeCopyPlantNode( GeneratorInletNode, GeneratorOutletNode );
			}

		} else {
			//set node temperatures
			SafeCopyPlantNode( EvapInletNode, EvapOutletNode );
			SafeCopyPlantNode( CondInletNode, CondOutletNode );
			Node( EvapOutletNode ).Temp = EvapOutletTemp;
			Node( CondOutletNode ).Temp = CondOutletTemp;

			IndirectAbsorberReport( Num ).PumpingPower = PumpingPower;
			IndirectAbsorberReport( Num ).QEvap = QEvaporator;
			IndirectAbsorberReport( Num ).QCond = QCondenser;
			IndirectAbsorberReport( Num ).QGenerator = QGenerator;
			IndirectAbsorberReport( Num ).PumpingEnergy = PumpingEnergy;
			IndirectAbsorberReport( Num ).EvapEnergy = EvaporatorEnergy;
			IndirectAbsorberReport( Num ).CondEnergy = CondenserEnergy;
			IndirectAbsorberReport( Num ).GeneratorEnergy = GeneratorEnergy;
			IndirectAbsorberReport( Num ).EvapInletTemp = Node( EvapInletNode ).Temp;
			IndirectAbsorberReport( Num ).CondInletTemp = Node( CondInletNode ).Temp;
			IndirectAbsorberReport( Num ).CondOutletTemp = Node( CondOutletNode ).Temp;
			IndirectAbsorberReport( Num ).EvapOutletTemp = Node( EvapOutletNode ).Temp;
			IndirectAbsorberReport( Num ).Evapmdot = EvapMassFlowRate;
			IndirectAbsorberReport( Num ).Condmdot = CondMassFlowRate;
			IndirectAbsorberReport( Num ).Genmdot = GenMassFlowRate;
			IndirectAbsorberReport( Num ).LoopLoss = EnergyLossToEnvironment;
			IndirectAbsorberReport( Num ).ChillerCyclingFrac = ChillerONOFFCyclingFrac;

			if ( QGenerator != 0.0 ) {
				IndirectAbsorberReport( Num ).ActualCOP = QEvaporator / QGenerator;
			} else {
				IndirectAbsorberReport( Num ).ActualCOP = 0.0;
			}

			if ( GeneratorInletNode > 0 ) {
				SafeCopyPlantNode( GeneratorInletNode, GeneratorOutletNode );
				Node( GeneratorOutletNode ).Temp = GenOutletTemp;
			}

		}

	}

	// End of Record Keeping subroutines for the Absorption Chiller Module
	// *****************************************************************************

	// Clears the global data. Needed for unit tests, should not be normally called.
	void
	clear_state()
	{
		NumIndirectAbsorbers =  0 ;
		CondMassFlowRate = 0.0 ;
		EvapMassFlowRate = 0.0 ;
		GenMassFlowRate = 0.0 ;
		CondOutletTemp = 0.0 ;
		EvapOutletTemp = 0.0 ;
		GenOutletTemp = 0.0 ;
		SteamOutletEnthalpy = 0.0 ;
		PumpingPower = 0.0 ;
		PumpingEnergy = 0.0 ;
		QGenerator = 0.0 ;
		GeneratorEnergy = 0.0 ;
		QEvaporator = 0.0 ;
		EvaporatorEnergy = 0.0 ;
		QCondenser = 0.0 ;
		CondenserEnergy = 0.0 ;
		EnergyLossToEnvironment = 0.0 ;
		ChillerONOFFCyclingFrac = 0.0 ;
		IndirectAbsorber.deallocate();
		IndirectAbsorberReport.deallocate();
	}

} // ChillerIndirectAbsorption

} // EnergyPlus
