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
#include <ObjexxFCL/Fmath.hh>

// EnergyPlus Headers
#include <HVACHXAssistedCoolingCoil.hh>
#include <BranchNodeConnections.hh>
#include <DataHeatBalance.hh>
#include <DataHVACGlobals.hh>
#include <DataLoopNode.hh>
#include <DataPrecisionGlobals.hh>
#include <DXCoils.hh>
#include <General.hh>
#include <HeatRecovery.hh>
#include <InputProcessor.hh>
#include <NodeInputManager.hh>
#include <Psychrometrics.hh>
#include <UtilityRoutines.hh>
#include <WaterCoils.hh>

namespace EnergyPlus {

namespace HVACHXAssistedCoolingCoil {
	// Module containing the simulation routines for a heat exchanger-
	// assisted cooling coil

	// MODULE INFORMATION:
	//       AUTHOR         Richard Raustad, FSEC
	//       DATE WRITTEN   Sept 2003
	//       MODIFIED       na
	//       RE-ENGINEERED  na

	// PURPOSE OF THIS MODULE:
	//  To encapsulate the data and algorithms required to
	//  manage the heat exchanger-assisted cooling coil compound component

	// METHODOLOGY EMPLOYED:
	//  Call the air-to-air heat exchanger and cooling coil repeatedly to converge
	//  on the solution instead of relying on the air loop manager for iterations

	// REFERENCES:
	// Kosar, D. 2006. Dehumidification Enhancements, ASHRAE Journal, Vol. 48, No. 2, February 2006.
	// Kosar, D. et al. 2006. Dehumidification Enhancement of Direct Expansion Systems Through Component
	//   Augmentation of the Cooling Coil. 15th Symposium on Improving Building Systems in Hot and Humid
	//   Climates, July 24-26, 2006.

	// OTHER NOTES:

	// USE STATEMENTS:
	//  Use statements for data only modules
	// Using/Aliasing
	using namespace DataPrecisionGlobals;
	using namespace DataLoopNode;
	using namespace DataGlobals;
	using namespace DataHVACGlobals;
	//unused0909USE DataEnvironment, ONLY: CurMnDy, EnvironmentName

	// Data
	// MODULE PARAMETER DEFINITIONS
	// Compressor operation
	int const On( 1 ); // normal compressor operation
	int const Off( 0 ); // signal DXCoil that compressor shouldn't run

	static std::string const BlankString;

	// DERIVED TYPE DEFINITIONS

	// MODULE VARIABLE DECLARATIONS:
	int TotalNumHXAssistedCoils( 0 ); // The total number of HXAssistedCoolingCoil compound objects
	Array1D< Real64 > HXAssistedCoilOutletTemp; // Outlet temperature from this compound object
	Array1D< Real64 > HXAssistedCoilOutletHumRat; // Outlet humidity ratio from this compound object
	// PUBLIC so others can access this information
	bool GetCoilsInputFlag( true ); // Flag to allow input data to be retrieved from idf on first call to this subroutine
	Array1D_bool CheckEquipName;

	// Subroutine Specifications for the Module
	// Driver/Manager Routines

	// Get Input routines for module

	// Initialization routines for module

	// Calculation algorithms for the module
	// Update routine to update output node information
	//PRIVATE UpdateHXAssistedCoolingCoil
	// Not required.  All updates done by the individual components
	// (cooling coil and air-to-air heat exchanger)

	// Reporting routines for module
	//PRIVATE ReportHXAssistedCoolingCoil
	// No reporting variables for this compound component

	// Utility routines for module

	// Object Data
	Array1D< HXAssistedCoilParameters > HXAssistedCoil;

	// MODULE SUBROUTINES:
	//*************************************************************************

	// Functions

	void
	SimHXAssistedCoolingCoil(
		std::string const & HXAssistedCoilName, // Name of HXAssistedCoolingCoil
		bool const FirstHVACIteration, // FirstHVACIteration flag
		int const CompOp, // compressor operation; 1=on, 0=off
		Real64 const PartLoadRatio, // Part load ratio of Coil:DX:CoolingBypassFactorEmpirical
		int & CompIndex,
		int const FanOpMode, // Allows the parent object to control fan operation
		Optional_bool_const HXUnitEnable, // flag to enable heat exchanger heat recovery
		Optional< Real64 const > OnOffAFR, // Ratio of compressor ON air mass flow rate to AVERAGE over time step
		Optional_bool_const EconomizerFlag, // OA sys or air loop economizer status
		Optional< Real64 > QTotOut // the total cooling output of unit
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Richard Raustad, FSEC
		//       DATE WRITTEN   Sept 2003
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		//  This subroutine manages the simulation of the
		//  cooling coil/heat exchanger combination.

		// METHODOLOGY EMPLOYED:
		//  na

		// REFERENCES:
		//  na

		// Using/Aliasing
		using InputProcessor::FindItemInList;
		using General::TrimSigDigits;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// (not used for Coil:Water:DetailedFlatCooling)

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		//  na

		// DERIVED TYPE DEFINITIONS
		//  na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int HXAssistedCoilNum; // Index for HXAssistedCoolingCoil
		Real64 AirFlowRatio; // Ratio of compressor ON air mass flow rate to AVEARAGE over time step
		bool HXUnitOn; // flag to enable heat exchanger
		Real64 AirMassFlow; // HX System air mass flow rate
		int InletNodeNum; // HX System inlet node number
		int OutletNodeNum; // HX System outlet node number

		// Obtains and allocates HXAssistedCoolingCoil related parameters from input file
		if ( GetCoilsInputFlag ) { // First time subroutine has been called, get input data
			// Get the HXAssistedCoolingCoil input
			GetHXAssistedCoolingCoilInput();
			GetCoilsInputFlag = false; // Set logic flag to disallow getting the input data on future calls to this subroutine
		}

		// Find the correct HXAssistedCoolingCoil number
		if ( CompIndex == 0 ) {
			HXAssistedCoilNum = FindItemInList( HXAssistedCoilName, HXAssistedCoil );
			if ( HXAssistedCoilNum == 0 ) {
				ShowFatalError( "HX Assisted Coil not found=" + HXAssistedCoilName );
			}
			CompIndex = HXAssistedCoilNum;
		} else {
			HXAssistedCoilNum = CompIndex;
			if ( HXAssistedCoilNum > TotalNumHXAssistedCoils || HXAssistedCoilNum < 1 ) {
				ShowFatalError( "SimHXAssistedCoolingCoil: Invalid CompIndex passed=" + TrimSigDigits( HXAssistedCoilNum ) + ", Number of HX Assisted Cooling Coils=" + TrimSigDigits( TotalNumHXAssistedCoils ) + ", Coil name=" + HXAssistedCoilName );
			}
			if ( CheckEquipName( HXAssistedCoilNum ) ) {
				if ( ! HXAssistedCoilName.empty() && HXAssistedCoilName != HXAssistedCoil( HXAssistedCoilNum ).Name ) {
					ShowFatalError( "SimHXAssistedCoolingCoil: Invalid CompIndex passed=" + TrimSigDigits( HXAssistedCoilNum ) + ", Coil name=" + HXAssistedCoilName + ", stored Coil Name for that index=" + HXAssistedCoil( HXAssistedCoilNum ).Name );
				}
				CheckEquipName( HXAssistedCoilNum ) = false;
			}
		}

		// Initialize HXAssistedCoolingCoil Flows
		InitHXAssistedCoolingCoil( HXAssistedCoilNum );

		if ( present( HXUnitEnable ) ) {
			HXUnitOn = HXUnitEnable;
		} else {
			HXUnitOn = true;
		}

		if ( CompOp == Off ) {
			HXUnitOn = false;
		}

		// Calculate the HXAssistedCoolingCoil performance and the coil outlet conditions
		if ( present( OnOffAFR ) ) {
			AirFlowRatio = OnOffAFR;
		} else {
			AirFlowRatio = 1.0;
		}
		CalcHXAssistedCoolingCoil( HXAssistedCoilNum, FirstHVACIteration, CompOp, PartLoadRatio, HXUnitOn, FanOpMode, AirFlowRatio, EconomizerFlag );

		// Update the current HXAssistedCoil output
		//  Call UpdateHXAssistedCoolingCoil(HXAssistedCoilNum), not required. Updates done by the HX and cooling coil components.

		// Report the current HXAssistedCoil output
		//  Call ReportHXAssistedCoolingCoil(HXAssistedCoilNum), not required. No reporting variables for this compound component.

		if ( present( QTotOut ) ) {
			InletNodeNum = HXAssistedCoil( HXAssistedCoilNum ).HXAssistedCoilInletNodeNum;
			OutletNodeNum = HXAssistedCoil( HXAssistedCoilNum ).HXAssistedCoilOutletNodeNum;
			AirMassFlow = Node( OutletNodeNum ).MassFlowRate;
			QTotOut = AirMassFlow * ( Node( InletNodeNum ).Enthalpy - Node( OutletNodeNum ).Enthalpy );
		}

	}

	// Get Input Section of the Module
	//******************************************************************************

	void
	GetHXAssistedCoolingCoilInput()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Richard Raustad, FSEC
		//       DATE WRITTEN   Sept 2003
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		//  Obtains input data for this compount object and stores it in data structure

		// METHODOLOGY EMPLOYED:
		//  Uses "Get" routines to read in data.

		// REFERENCES:

		// Using/Aliasing
		using InputProcessor::GetNumObjectsFound;
		using InputProcessor::GetObjectItem;
		using InputProcessor::VerifyName;
		using InputProcessor::SameString;
		using InputProcessor::GetObjectDefMaxArgs;
		using NodeInputManager::GetOnlySingleNode;
		using BranchNodeConnections::SetUpCompSets;
		using BranchNodeConnections::TestCompSet;
		auto & GetDXCoilInletNode( DXCoils::GetCoilInletNode );
		auto & GetDXCoilOutletNode( DXCoils::GetCoilOutletNode );
		using DXCoils::GetDXCoilIndex;
		auto & GetWaterCoilInletNode( WaterCoils::GetCoilInletNode );
		auto & GetWaterCoilOutletNode( WaterCoils::GetCoilOutletNode );
		using HeatRecovery::GetSupplyInletNode;
		using HeatRecovery::GetSupplyOutletNode;
		using HeatRecovery::GetSecondaryInletNode;
		using HeatRecovery::GetSecondaryOutletNode;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "GetHXAssistedCoolingCoilInput: " ); // include trailing blank space

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int HXAssistedCoilNum; // Index number of the HXAssistedCoolingCoil for which input data is being read from the idf
		int NumAlphas; // Number of alpha inputs
		int NumNums; // Number of number inputs
		int IOStat; // Return status from GetObjectItem call
		static bool ErrorsFound( false ); // set TRUE if errors detected in input
		bool IsNotOK; // Flag to verify name
		bool IsBlank; // Flag for blank name
		int NumHXAssistedDXCoils; // Number of HXAssistedCoolingCoil objects using a DX coil
		int NumHXAssistedWaterCoils; // Number of HXAssistedCoolingCoil objects using a chilled water coil
		//    LOGICAL :: FanErrFlag              ! Error flag for fan operating mode mining call
		bool HXErrFlag; // Error flag for HX node numbers mining call
		bool CoolingCoilErrFlag; // Error flag for cooling coil node numbers mining call
		int SupplyAirInletNode; // supply air inlet node number mined from heat exchanger object (ExchCond structure)
		int SupplyAirOutletNode; // supply air outlet node number mined from heat exchanger object (ExchCond structure)
		int SecondaryAirInletNode; // secondary air inlet node number mined from heat exchanger object (ExchCond structure)
		int SecondaryAirOutletNode; // secondary air outlet node number mined from heat exchanger object (ExchCond structure)
		int CoolingCoilInletNodeNum; // outlet node number of cooling coil, used for warning messages
		int CoolingCoilOutletNodeNum; // outlet node number of cooling coil, used for warning messages
		std::string CurrentModuleObject; // Object type for getting and error messages
		Array1D_string AlphArray; // Alpha input items for object
		Array1D_string cAlphaFields; // Alpha field names
		Array1D_string cNumericFields; // Numeric field names
		Array1D< Real64 > NumArray; // Numeric input items for object
		Array1D_bool lAlphaBlanks; // Logical array, alpha field input BLANK = .TRUE.
		Array1D_bool lNumericBlanks; // Logical array, numeric field input BLANK = .TRUE.
		static int MaxNums( 0 ); // Maximum number of numeric input fields
		static int MaxAlphas( 0 ); // Maximum number of alpha input fields
		static int TotalArgs( 0 ); // Total number of alpha and numeric arguments (max) for a

		NumHXAssistedDXCoils = GetNumObjectsFound( "CoilSystem:Cooling:DX:HeatExchangerAssisted" );
		NumHXAssistedWaterCoils = GetNumObjectsFound( "CoilSystem:Cooling:Water:HeatExchangerAssisted" );
		TotalNumHXAssistedCoils = NumHXAssistedDXCoils + NumHXAssistedWaterCoils;
		if ( TotalNumHXAssistedCoils > 0 ) {
			HXAssistedCoil.allocate( TotalNumHXAssistedCoils );
			HXAssistedCoilOutletTemp.allocate( TotalNumHXAssistedCoils );
			HXAssistedCoilOutletHumRat.allocate( TotalNumHXAssistedCoils );
			CheckEquipName.dimension( TotalNumHXAssistedCoils, true );
		}

		GetObjectDefMaxArgs( "CoilSystem:Cooling:DX:HeatExchangerAssisted", TotalArgs, NumAlphas, NumNums );
		MaxNums = max( MaxNums, NumNums );
		MaxAlphas = max( MaxAlphas, NumAlphas );
		GetObjectDefMaxArgs( "CoilSystem:Cooling:Water:HeatExchangerAssisted", TotalArgs, NumAlphas, NumNums );
		MaxNums = max( MaxNums, NumNums );
		MaxAlphas = max( MaxAlphas, NumAlphas );

		AlphArray.allocate( MaxAlphas );
		cAlphaFields.allocate( MaxAlphas );
		cNumericFields.allocate( MaxNums );
		NumArray.dimension( MaxNums, 0.0 );
		lAlphaBlanks.dimension( MaxAlphas, true );
		lNumericBlanks.dimension( MaxNums, true );

		// Get the data for the Coil:DX:CoolingHeatExchangerAssisted objects
		CurrentModuleObject = "CoilSystem:Cooling:DX:HeatExchangerAssisted";

		for ( HXAssistedCoilNum = 1; HXAssistedCoilNum <= NumHXAssistedDXCoils; ++HXAssistedCoilNum ) {
			GetObjectItem( CurrentModuleObject, HXAssistedCoilNum, AlphArray, NumAlphas, NumArray, NumNums, IOStat, lNumericBlanks, lAlphaBlanks, cAlphaFields, cNumericFields );
			IsNotOK = false;
			IsBlank = false;
			VerifyName( AlphArray( 1 ), HXAssistedCoil, HXAssistedCoilNum - 1, IsNotOK, IsBlank, CurrentModuleObject + " Name" );
			if ( IsNotOK ) {
				ErrorsFound = true;
				if ( IsBlank ) AlphArray( 1 ) = "xxxxx";
			}
			HXAssistedCoil( HXAssistedCoilNum ).Name = AlphArray( 1 );

			HXAssistedCoil( HXAssistedCoilNum ).HeatExchangerType = AlphArray( 2 );
			HXAssistedCoil( HXAssistedCoilNum ).HeatExchangerName = AlphArray( 3 );

			if ( SameString( HXAssistedCoil( HXAssistedCoilNum ).HeatExchangerType, "HeatExchanger:AirToAir:SensibleAndLatent" ) ) {
				HXAssistedCoil( HXAssistedCoilNum ).HeatExchangerType_Num = HX_AIRTOAIR_GENERIC;
			} else if ( SameString( HXAssistedCoil( HXAssistedCoilNum ).HeatExchangerType, "HeatExchanger:AirToAir:FlatPlate" ) ) {
				HXAssistedCoil( HXAssistedCoilNum ).HeatExchangerType_Num = HX_AIRTOAIR_FLATPLATE;
			} else if ( SameString( HXAssistedCoil( HXAssistedCoilNum ).HeatExchangerType, "HeatExchanger:Desiccant:BalancedFlow" ) ) {
				HXAssistedCoil( HXAssistedCoilNum ).HeatExchangerType_Num = HX_DESICCANT_BALANCED;
			} else {
				ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + HXAssistedCoil( HXAssistedCoilNum ).Name + "\"" );
				ShowContinueError( "Invalid " + cAlphaFields( 2 ) + "=\"" + HXAssistedCoil( HXAssistedCoilNum ).HeatExchangerType + "\"" );
				ErrorsFound = true;
			}

			HXAssistedCoil( HXAssistedCoilNum ).CoolingCoilType = AlphArray( 4 );
			HXAssistedCoil( HXAssistedCoilNum ).CoolingCoilName = AlphArray( 5 );

			if ( SameString( HXAssistedCoil( HXAssistedCoilNum ).CoolingCoilType, "Coil:Cooling:DX:SingleSpeed" ) ) {
				HXAssistedCoil( HXAssistedCoilNum ).CoolingCoilType_Num = CoilDX_CoolingSingleSpeed;
				HXAssistedCoil( HXAssistedCoilNum ).HXAssistedCoilType = CurrentModuleObject;
				HXAssistedCoil( HXAssistedCoilNum ).HXAssistedCoilType_Num = CoilDX_CoolingHXAssisted;
				CoolingCoilErrFlag = false;
				GetDXCoilIndex( HXAssistedCoil( HXAssistedCoilNum ).CoolingCoilName, HXAssistedCoil( HXAssistedCoilNum ).CoolingCoilIndex, CoolingCoilErrFlag, HXAssistedCoil( HXAssistedCoilNum ).CoolingCoilType );
				if ( CoolingCoilErrFlag ) {
					ShowContinueError( "...occurs in " + CurrentModuleObject + "=\"" + HXAssistedCoil( HXAssistedCoilNum ).Name + "\"" );
					ErrorsFound = true;
				}
			} else {
				ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + HXAssistedCoil( HXAssistedCoilNum ).Name + "\"" );
				ShowContinueError( "Invalid " + cAlphaFields( 4 ) + "=\"" + HXAssistedCoil( HXAssistedCoilNum ).CoolingCoilType + "\"" );
				ErrorsFound = true;
			}

			HXErrFlag = false;
			SupplyAirInletNode = GetSupplyInletNode( HXAssistedCoil( HXAssistedCoilNum ).HeatExchangerName, HXErrFlag );
			if ( HXErrFlag ) {
				ShowContinueError( "...Occurs in " + CurrentModuleObject + "=\"" + HXAssistedCoil( HXAssistedCoilNum ).Name + "\"" );
			}

			HXErrFlag = false;
			SupplyAirOutletNode = GetSupplyOutletNode( HXAssistedCoil( HXAssistedCoilNum ).HeatExchangerName, HXErrFlag );
			if ( HXErrFlag ) {
				ShowContinueError( "...Occurs in " + CurrentModuleObject + "=\"" + HXAssistedCoil( HXAssistedCoilNum ).Name + "\"" );
			}

			HXErrFlag = false;
			SecondaryAirInletNode = GetSecondaryInletNode( HXAssistedCoil( HXAssistedCoilNum ).HeatExchangerName, HXErrFlag );
			if ( HXErrFlag ) {
				ShowContinueError( "...Occurs in " + CurrentModuleObject + "=\"" + HXAssistedCoil( HXAssistedCoilNum ).Name + "\"" );
			}

			HXErrFlag = false;
			SecondaryAirOutletNode = GetSecondaryOutletNode( HXAssistedCoil( HXAssistedCoilNum ).HeatExchangerName, HXErrFlag );
			if ( HXErrFlag ) {
				ShowContinueError( "...Occurs in " + CurrentModuleObject + "=\"" + HXAssistedCoil( HXAssistedCoilNum ).Name + "\"" );
			}

			if ( SameString( HXAssistedCoil( HXAssistedCoilNum ).CoolingCoilType, "Coil:Cooling:DX:SingleSpeed" ) ) {
				//         Check node names in heat exchanger and coil objects for consistency
				CoolingCoilErrFlag = false;
				CoolingCoilInletNodeNum = GetDXCoilInletNode( HXAssistedCoil( HXAssistedCoilNum ).CoolingCoilType, HXAssistedCoil( HXAssistedCoilNum ).CoolingCoilName, CoolingCoilErrFlag );
				if ( CoolingCoilErrFlag ) {
					ShowContinueError( "...Occurs in " + CurrentModuleObject + "=\"" + HXAssistedCoil( HXAssistedCoilNum ).Name + "\"" );
				}
				if ( SupplyAirOutletNode != CoolingCoilInletNodeNum ) {
					ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + HXAssistedCoil( HXAssistedCoilNum ).Name + "\"" );
					ShowContinueError( "Node names are inconsistent in heat exchanger and cooling coil object." );
					ShowContinueError( "The supply air outlet node name in heat exchanger = " + HXAssistedCoil( HXAssistedCoilNum ).HeatExchangerType + "=\"" + HXAssistedCoil( HXAssistedCoilNum ).HeatExchangerName + "\"" );
					ShowContinueError( "must match the cooling coil inlet node name in = " + HXAssistedCoil( HXAssistedCoilNum ).CoolingCoilType + "=\"" + HXAssistedCoil( HXAssistedCoilNum ).CoolingCoilName + "\"" );
					ShowContinueError( "Heat exchanger supply air outlet node name=\"" + NodeID( SupplyAirOutletNode ) + "\"" );
					ShowContinueError( "Cooling coil air inlet node name=\"" + NodeID( CoolingCoilInletNodeNum ) + "\"" );
					ErrorsFound = true;
				}
				CoolingCoilErrFlag = false;
				CoolingCoilOutletNodeNum = GetDXCoilOutletNode( HXAssistedCoil( HXAssistedCoilNum ).CoolingCoilType, HXAssistedCoil( HXAssistedCoilNum ).CoolingCoilName, CoolingCoilErrFlag );
				if ( CoolingCoilErrFlag ) {
					ShowContinueError( "...Occurs in " + CurrentModuleObject + "=\"" + HXAssistedCoil( HXAssistedCoilNum ).Name + "\"" );
				}
				if ( SecondaryAirInletNode != CoolingCoilOutletNodeNum ) {
					ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + HXAssistedCoil( HXAssistedCoilNum ).Name + "\"" );
					ShowContinueError( "Node names are inconsistent in heat exchanger and cooling coil object." );
					ShowContinueError( "The secondary air inlet node name in heat exchanger =" + HXAssistedCoil( HXAssistedCoilNum ).HeatExchangerType + "=\"" + HXAssistedCoil( HXAssistedCoilNum ).HeatExchangerName + "\"" );
					ShowContinueError( "must match the cooling coil air outlet node name in = " + HXAssistedCoil( HXAssistedCoilNum ).CoolingCoilType + "=\"" + HXAssistedCoil( HXAssistedCoilNum ).CoolingCoilName + "\"." );
					ShowContinueError( "Heat exchanger secondary air inlet node name =\"" + NodeID( SecondaryAirInletNode ) + "\"." );
					ShowContinueError( "Cooling coil air outlet node name =\"" + NodeID( CoolingCoilOutletNodeNum ) + "\"." );
					ErrorsFound = true;
				}

			}

			TestCompSet( HXAssistedCoil( HXAssistedCoilNum ).HXAssistedCoilType, HXAssistedCoil( HXAssistedCoilNum ).Name, NodeID( SupplyAirInletNode ), NodeID( SecondaryAirOutletNode ), "Air Nodes" );

			HXAssistedCoil( HXAssistedCoilNum ).HXAssistedCoilInletNodeNum = GetOnlySingleNode( NodeID( SupplyAirInletNode ), ErrorsFound, CurrentModuleObject, HXAssistedCoil( HXAssistedCoilNum ).Name, NodeType_Air, NodeConnectionType_Inlet, 1, ObjectIsParent );
			CoolingCoilInletNodeNum = GetOnlySingleNode( NodeID( SupplyAirOutletNode ), ErrorsFound, CurrentModuleObject, HXAssistedCoil( HXAssistedCoilNum ).Name, NodeType_Air, NodeConnectionType_Internal, 1, ObjectIsParent );
			HXAssistedCoil( HXAssistedCoilNum ).HXExhaustAirInletNodeNum = GetOnlySingleNode( NodeID( SecondaryAirInletNode ), ErrorsFound, CurrentModuleObject, HXAssistedCoil( HXAssistedCoilNum ).Name, NodeType_Air, NodeConnectionType_Internal, 1, ObjectIsParent );
			HXAssistedCoil( HXAssistedCoilNum ).HXAssistedCoilOutletNodeNum = GetOnlySingleNode( NodeID( SecondaryAirOutletNode ), ErrorsFound, CurrentModuleObject, HXAssistedCoil( HXAssistedCoilNum ).Name, NodeType_Air, NodeConnectionType_Outlet, 1, ObjectIsParent );

			// Add cooling coil to component sets array
			SetUpCompSets( HXAssistedCoil( HXAssistedCoilNum ).HXAssistedCoilType, HXAssistedCoil( HXAssistedCoilNum ).Name, HXAssistedCoil( HXAssistedCoilNum ).CoolingCoilType, HXAssistedCoil( HXAssistedCoilNum ).CoolingCoilName, NodeID( SupplyAirOutletNode ), NodeID( SecondaryAirInletNode ), "Air Nodes" );
			// Add heat exchanger to component sets array
			SetUpCompSets( HXAssistedCoil( HXAssistedCoilNum ).HXAssistedCoilType, HXAssistedCoil( HXAssistedCoilNum ).Name, HXAssistedCoil( HXAssistedCoilNum ).HeatExchangerType, HXAssistedCoil( HXAssistedCoilNum ).HeatExchangerName, NodeID( SupplyAirInletNode ), NodeID( SupplyAirOutletNode ), "Process Air Nodes" );
			SetUpCompSets( HXAssistedCoil( HXAssistedCoilNum ).HXAssistedCoilType, HXAssistedCoil( HXAssistedCoilNum ).Name, HXAssistedCoil( HXAssistedCoilNum ).HeatExchangerType, HXAssistedCoil( HXAssistedCoilNum ).HeatExchangerName, NodeID( SecondaryAirInletNode ), NodeID( SecondaryAirOutletNode ), "Secondary Air Nodes" );

		} // End of the Coil:DX:CoolingHXAssisted Loop

		// Get the data for the Coil:Water:CoolingHeatExchangerAssisted objects
		CurrentModuleObject = "CoilSystem:Cooling:Water:HeatExchangerAssisted";

		for ( HXAssistedCoilNum = NumHXAssistedDXCoils + 1; HXAssistedCoilNum <= NumHXAssistedWaterCoils; ++HXAssistedCoilNum ) {

			GetObjectItem( CurrentModuleObject, HXAssistedCoilNum, AlphArray, NumAlphas, NumArray, NumNums, IOStat, lNumericBlanks, lAlphaBlanks, cAlphaFields, cNumericFields );
			IsNotOK = false;
			IsBlank = false;
			VerifyName( AlphArray( 1 ), HXAssistedCoil, HXAssistedCoilNum - 1, IsNotOK, IsBlank, CurrentModuleObject + " Name" );
			if ( IsNotOK ) {
				ErrorsFound = true;
				if ( IsBlank ) AlphArray( 1 ) = "xxxxx";
			}
			HXAssistedCoil( HXAssistedCoilNum ).Name = AlphArray( 1 );

			HXAssistedCoil( HXAssistedCoilNum ).HeatExchangerType = AlphArray( 2 );
			HXAssistedCoil( HXAssistedCoilNum ).HeatExchangerName = AlphArray( 3 );

			if ( SameString( HXAssistedCoil( HXAssistedCoilNum ).HeatExchangerType, "HeatExchanger:AirToAir:SensibleAndLatent" ) ) {
				HXAssistedCoil( HXAssistedCoilNum ).HeatExchangerType_Num = HX_AIRTOAIR_GENERIC;
			} else if ( SameString( HXAssistedCoil( HXAssistedCoilNum ).HeatExchangerType, "HeatExchanger:AirToAir:FlatPlate" ) ) {
				HXAssistedCoil( HXAssistedCoilNum ).HeatExchangerType_Num = HX_AIRTOAIR_FLATPLATE;
				//       balanced desiccant HX not allowed with water coils at this time
				//       ELSEIF(SameString(HXAssistedCoil(HXAssistedCoilNum)%HeatExchangerType,'HeatExchanger:Desiccant:BalancedFlow')) THEN
				//         HXAssistedCoil(HXAssistedCoilNum)%HeatExchangerType_Num = HX_DESICCANT_BALANCED
			} else {
				ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + HXAssistedCoil( HXAssistedCoilNum ).Name + "\"" );
				ShowContinueError( "Invalid " + cAlphaFields( 2 ) + "=\"" + HXAssistedCoil( HXAssistedCoilNum ).HeatExchangerType + "\"" );
				ErrorsFound = true;
			}

			HXAssistedCoil( HXAssistedCoilNum ).CoolingCoilType = AlphArray( 4 );
			HXAssistedCoil( HXAssistedCoilNum ).CoolingCoilName = AlphArray( 5 );

			HXErrFlag = false;
			SupplyAirInletNode = GetSupplyInletNode( HXAssistedCoil( HXAssistedCoilNum ).HeatExchangerName, HXErrFlag );
			if ( HXErrFlag ) {
				ShowContinueError( "...Occurs in " + CurrentModuleObject + "=\"" + HXAssistedCoil( HXAssistedCoilNum ).Name + "\"" );
			}

			HXErrFlag = false;
			SupplyAirOutletNode = GetSupplyOutletNode( HXAssistedCoil( HXAssistedCoilNum ).HeatExchangerName, HXErrFlag );
			if ( HXErrFlag ) {
				ShowContinueError( "...Occurs in " + CurrentModuleObject + "=\"" + HXAssistedCoil( HXAssistedCoilNum ).Name );
			}

			HXErrFlag = false;
			SecondaryAirInletNode = GetSecondaryInletNode( HXAssistedCoil( HXAssistedCoilNum ).HeatExchangerName, HXErrFlag );
			if ( HXErrFlag ) {
				ShowContinueError( "...Occurs in " + CurrentModuleObject + "=\"" + HXAssistedCoil( HXAssistedCoilNum ).Name + "\"" );
			}

			HXErrFlag = false;
			SecondaryAirOutletNode = GetSecondaryOutletNode( HXAssistedCoil( HXAssistedCoilNum ).HeatExchangerName, HXErrFlag );
			if ( HXErrFlag ) {
				ShowContinueError( "...Occurs in " + CurrentModuleObject + "=\"" + HXAssistedCoil( HXAssistedCoilNum ).Name + "\"" );
			}

			if ( SameString( HXAssistedCoil( HXAssistedCoilNum ).CoolingCoilType, "Coil:Cooling:Water" ) || SameString( HXAssistedCoil( HXAssistedCoilNum ).CoolingCoilType, "Coil:Cooling:Water:DetailedGeometry" ) ) {
				if ( SameString( HXAssistedCoil( HXAssistedCoilNum ).CoolingCoilType, "Coil:Cooling:Water:DetailedGeometry" ) ) {
					HXAssistedCoil( HXAssistedCoilNum ).CoolingCoilType_Num = Coil_CoolingWaterDetailed;
				} else if ( SameString( HXAssistedCoil( HXAssistedCoilNum ).CoolingCoilType, "Coil:Cooling:Water" ) ) {
					HXAssistedCoil( HXAssistedCoilNum ).CoolingCoilType_Num = Coil_CoolingWater;
				}

				HXAssistedCoil( HXAssistedCoilNum ).HXAssistedCoilType = CurrentModuleObject;
				HXAssistedCoil( HXAssistedCoilNum ).HXAssistedCoilType_Num = CoilWater_CoolingHXAssisted;

				//         Check node names in heat exchanger and coil objects for consistency
				CoolingCoilErrFlag = false;
				CoolingCoilInletNodeNum = GetWaterCoilInletNode( HXAssistedCoil( HXAssistedCoilNum ).CoolingCoilType, HXAssistedCoil( HXAssistedCoilNum ).CoolingCoilName, CoolingCoilErrFlag );
				if ( CoolingCoilErrFlag ) ShowContinueError( "...occurs in " + CurrentModuleObject + " \"" + HXAssistedCoil( HXAssistedCoilNum ).Name + "\"" );
				if ( SupplyAirOutletNode != CoolingCoilInletNodeNum ) {
					ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + HXAssistedCoil( HXAssistedCoilNum ).Name + "\"" );
					ShowContinueError( "Node names are inconsistent in heat exchanger and cooling coil object." );
					ShowContinueError( "The supply air outlet node name in heat exchanger = " + HXAssistedCoil( HXAssistedCoilNum ).HeatExchangerType + "=\"" + HXAssistedCoil( HXAssistedCoilNum ).HeatExchangerName + "\"" );
					ShowContinueError( "must match the cooling coil inlet node name in = " + HXAssistedCoil( HXAssistedCoilNum ).CoolingCoilType + "=\"" + HXAssistedCoil( HXAssistedCoilNum ).CoolingCoilName + "\"" );
					ShowContinueError( "Heat exchanger supply air outlet node name =\"" + NodeID( SupplyAirOutletNode ) + "\"" );
					ShowContinueError( "Cooling coil air inlet node name = \"" + NodeID( CoolingCoilInletNodeNum ) + "\"" );
					ErrorsFound = true;
				}
				CoolingCoilErrFlag = false;
				CoolingCoilOutletNodeNum = GetWaterCoilOutletNode( HXAssistedCoil( HXAssistedCoilNum ).CoolingCoilType, HXAssistedCoil( HXAssistedCoilNum ).CoolingCoilName, CoolingCoilErrFlag );
				if ( CoolingCoilErrFlag ) ShowContinueError( "...occurs in " + CurrentModuleObject + " \"" + HXAssistedCoil( HXAssistedCoilNum ).Name + "\"" );
				if ( SecondaryAirInletNode != CoolingCoilOutletNodeNum ) {
					ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + HXAssistedCoil( HXAssistedCoilNum ).Name + "\"" );
					ShowContinueError( "Node names are inconsistent in heat exchanger and cooling coil object." );
					ShowContinueError( "The secondary air inlet node name in heat exchanger = " + HXAssistedCoil( HXAssistedCoilNum ).HeatExchangerType + "=\"" + HXAssistedCoil( HXAssistedCoilNum ).HeatExchangerName + "\"" );
					ShowContinueError( "must match the cooling coil air outlet node name in = " + HXAssistedCoil( HXAssistedCoilNum ).CoolingCoilType + "=\"" + HXAssistedCoil( HXAssistedCoilNum ).CoolingCoilName + "\"." );
					ShowContinueError( "Heat exchanger secondary air inlet node name = \"" + NodeID( SecondaryAirInletNode ) + "\"." );
					ShowContinueError( "Cooling coil air outlet node name = \"" + NodeID( CoolingCoilOutletNodeNum ) + "\"." );
					ErrorsFound = true;
				}

			} else {
				ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + HXAssistedCoil( HXAssistedCoilNum ).Name + "\"" );
				ShowContinueError( "Invalid " + cAlphaFields( 4 ) + "=\"" + HXAssistedCoil( HXAssistedCoilNum ).CoolingCoilType + "\"" );
				ErrorsFound = true;
			}

			TestCompSet( HXAssistedCoil( HXAssistedCoilNum ).HXAssistedCoilType, HXAssistedCoil( HXAssistedCoilNum ).Name, NodeID( SupplyAirInletNode ), NodeID( SecondaryAirOutletNode ), "Air Nodes" );

			HXAssistedCoil( HXAssistedCoilNum ).HXAssistedCoilInletNodeNum = GetOnlySingleNode( NodeID( SupplyAirInletNode ), ErrorsFound, CurrentModuleObject, HXAssistedCoil( HXAssistedCoilNum ).Name, NodeType_Air, NodeConnectionType_Inlet, 1, ObjectIsParent );
			CoolingCoilInletNodeNum = GetOnlySingleNode( NodeID( SupplyAirOutletNode ), ErrorsFound, CurrentModuleObject, HXAssistedCoil( HXAssistedCoilNum ).Name, NodeType_Air, NodeConnectionType_Internal, 1, ObjectIsParent );
			HXAssistedCoil( HXAssistedCoilNum ).HXExhaustAirInletNodeNum = GetOnlySingleNode( NodeID( SecondaryAirInletNode ), ErrorsFound, CurrentModuleObject, HXAssistedCoil( HXAssistedCoilNum ).Name, NodeType_Air, NodeConnectionType_Internal, 1, ObjectIsParent );
			HXAssistedCoil( HXAssistedCoilNum ).HXAssistedCoilOutletNodeNum = GetOnlySingleNode( NodeID( SecondaryAirOutletNode ), ErrorsFound, CurrentModuleObject, HXAssistedCoil( HXAssistedCoilNum ).Name, NodeType_Air, NodeConnectionType_Outlet, 1, ObjectIsParent );

			// Add cooling coil to component sets array
			SetUpCompSets( HXAssistedCoil( HXAssistedCoilNum ).HXAssistedCoilType, HXAssistedCoil( HXAssistedCoilNum ).Name, HXAssistedCoil( HXAssistedCoilNum ).CoolingCoilType, HXAssistedCoil( HXAssistedCoilNum ).CoolingCoilName, NodeID( SupplyAirOutletNode ), NodeID( SecondaryAirInletNode ), "Air Nodes" );
			// Add heat exchanger to component sets array
			SetUpCompSets( HXAssistedCoil( HXAssistedCoilNum ).HXAssistedCoilType, HXAssistedCoil( HXAssistedCoilNum ).Name, HXAssistedCoil( HXAssistedCoilNum ).HeatExchangerType, HXAssistedCoil( HXAssistedCoilNum ).HeatExchangerName, NodeID( SupplyAirInletNode ), NodeID( SupplyAirOutletNode ), "Process Air Nodes" );
			SetUpCompSets( HXAssistedCoil( HXAssistedCoilNum ).HXAssistedCoilType, HXAssistedCoil( HXAssistedCoilNum ).Name, HXAssistedCoil( HXAssistedCoilNum ).HeatExchangerType, HXAssistedCoil( HXAssistedCoilNum ).HeatExchangerName, NodeID( SecondaryAirInletNode ), NodeID( SecondaryAirOutletNode ), "Secondary Air Nodes" );

		} //End of the Coil:Water:CoolingHXAssisted Loop

		AlphArray.deallocate();
		cAlphaFields.deallocate();
		cNumericFields.deallocate();
		NumArray.deallocate();
		lAlphaBlanks.deallocate();
		lNumericBlanks.deallocate();

		if ( ErrorsFound ) {
			ShowFatalError( RoutineName + "Previous error condition causes termination." );
		}

	}

	// End of Get Input subroutines for this Module
	//******************************************************************************

	// Beginning Initialization Section of the Module
	//******************************************************************************

	void
	InitHXAssistedCoolingCoil( int const HXAssistedCoilNum ) // index for HXAssistedCoolingCoil
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Richard Raustad, FSEC
		//       DATE WRITTEN   Sep 2003
		//       MODIFIED       R. Raustad, June 2007 now using FullLoadOutletConditions from DX Coil data structure
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		//  This subroutine is for initializations of the HXAssistedCoolingCoil components

		// METHODOLOGY EMPLOYED:
		//  Uses the status flags to trigger initializations.

		// REFERENCES:

		// Using/Aliasing
		using DXCoils::DXCoilFullLoadOutAirTemp;
		using DXCoils::DXCoilFullLoadOutAirHumRat;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		//  na

		// INTERFACE BLOCK SPECIFICATIONS
		//  na

		// DERIVED TYPE DEFINITIONS
		//  na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		//  na

		// Do these initializations every time
		HXAssistedCoil( HXAssistedCoilNum ).MassFlowRate = Node( HXAssistedCoil( HXAssistedCoilNum ).HXAssistedCoilInletNodeNum ).MassFlowRate;

		if ( HXAssistedCoil( HXAssistedCoilNum ).CoolingCoilType_Num == CoilDX_CoolingSingleSpeed ) {
			DXCoilFullLoadOutAirTemp( HXAssistedCoil( HXAssistedCoilNum ).CoolingCoilIndex ) = 0.0;
			DXCoilFullLoadOutAirHumRat( HXAssistedCoil( HXAssistedCoilNum ).CoolingCoilIndex ) = 0.0;
		}

	}

	// End Initialization Section of the Module
	//******************************************************************************

	void
	CalcHXAssistedCoolingCoil(
		int const HXAssistedCoilNum, // Index number for HXAssistedCoolingCoil
		bool const FirstHVACIteration, // FirstHVACIteration flag
		int const CompOp, // compressor operation; 1=on, 0=off
		Real64 const PartLoadRatio, // Cooling coil part load ratio
		bool const HXUnitOn, // Flag to enable heat exchanger
		int const FanOpMode, // Allows parent object to control fan operation
		Optional< Real64 const > OnOffAirFlow, // Ratio of compressor ON air mass flow to AVERAGE over time step
		Optional_bool_const EconomizerFlag // OA (or airloop) econommizer status
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Richard Raustad, FSEC
		//       DATE WRITTEN   Sept 2003
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		//  This subroutine models the cooling coil/air-to-air heat exchanger
		//  combination. The cooling coil exiting air temperature is used as
		//  an indicator of convergence.

		// METHODOLOGY EMPLOYED:
		//  na

		// REFERENCES:
		//  na

		// Using/Aliasing
		using HeatRecovery::SimHeatRecovery;
		using DXCoils::SimDXCoil;
		using WaterCoils::SimulateWaterCoilComponents;
		using Psychrometrics::PsyHFnTdbW;
		using General::TrimSigDigits;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// (used only for Coil:DX:CoolingBypassFactorEmpirical)

		// SUBROUTINE PARAMETER DEFINITIONS:
		int const MaxIter( 50 ); // Maximum number of iterations

		// INTERFACE BLOCK SPECIFICATIONS
		//  na

		// DERIVED TYPE DEFINITIONS
		//  na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		static Real64 CoilOutputTempLast; // Exiting cooling coil temperature from last iteration
		Real64 AirMassFlow; // Inlet air mass flow rate
		Real64 Error; // Error (exiting coil temp from last iteration minus current coil exiting temp)
		int Iter; // Number of iterations
		int CompanionCoilIndexNum; // Index to DX coil

		AirMassFlow = HXAssistedCoil( HXAssistedCoilNum ).MassFlowRate;
		Error = 1.0; // Initialize error (CoilOutputTemp last iteration minus current CoilOutputTemp)
		Iter = 0; // Initialize iteration counter to zero

		if ( FirstHVACIteration ) CoilOutputTempLast = -99.0; // Initialize coil output temp

		// Set mass flow rate at inlet of exhaust side of heat exchanger to supply side air mass flow rate entering this compound object
		Node( HXAssistedCoil( HXAssistedCoilNum ).HXExhaustAirInletNodeNum ).MassFlowRate = AirMassFlow;

		if ( HXAssistedCoil( HXAssistedCoilNum ).CoolingCoilType_Num == CoilDX_CoolingSingleSpeed ) {
			CompanionCoilIndexNum = HXAssistedCoil( HXAssistedCoilNum ).CoolingCoilIndex;
		} else {
			CompanionCoilIndexNum = 0;
		}

		// First call to RegulaFalsi uses PLR=0. Nodes are typically setup at full output on this call.
		// A large number of iterations are required to get to result (~36 iterations to get to PLR=0 node conditions).
		// Reset node data to minimize iteration. This initialization reduces the number of iterations by 50%.
		// CAUTION: Do not use Node(x) = Node(y) here, this can overwrite the coil outlet node setpoint.
		if ( PartLoadRatio == 0.0 ) {
			Node( HXAssistedCoil( HXAssistedCoilNum ).HXExhaustAirInletNodeNum ).Temp = Node( HXAssistedCoil( HXAssistedCoilNum ).HXAssistedCoilInletNodeNum ).Temp;
			Node( HXAssistedCoil( HXAssistedCoilNum ).HXExhaustAirInletNodeNum ).HumRat = Node( HXAssistedCoil( HXAssistedCoilNum ).HXAssistedCoilInletNodeNum ).HumRat;
			Node( HXAssistedCoil( HXAssistedCoilNum ).HXExhaustAirInletNodeNum ).Enthalpy = Node( HXAssistedCoil( HXAssistedCoilNum ).HXAssistedCoilInletNodeNum ).Enthalpy;
			Node( HXAssistedCoil( HXAssistedCoilNum ).HXExhaustAirInletNodeNum ).MassFlowRate = Node( HXAssistedCoil( HXAssistedCoilNum ).HXAssistedCoilInletNodeNum ).MassFlowRate;
		}

		// Force at least 2 iterations to pass outlet node information
		while ( ( std::abs( Error ) > 0.0005 && Iter <= MaxIter ) || Iter < 2 ) {

			SimHeatRecovery( HXAssistedCoil( HXAssistedCoilNum ).HeatExchangerName, FirstHVACIteration, HXAssistedCoil( HXAssistedCoilNum ).HeatExchangerIndex, FanOpMode, PartLoadRatio, HXUnitOn, CompanionCoilIndexNum, _, EconomizerFlag );

			if ( HXAssistedCoil( HXAssistedCoilNum ).CoolingCoilType_Num == CoilDX_CoolingSingleSpeed ) {
				SimDXCoil( HXAssistedCoil( HXAssistedCoilNum ).CoolingCoilName, CompOp, FirstHVACIteration, HXAssistedCoil( HXAssistedCoilNum ).CoolingCoilIndex, FanOpMode, PartLoadRatio, OnOffAirFlow );
			} else {
				SimulateWaterCoilComponents( HXAssistedCoil( HXAssistedCoilNum ).CoolingCoilName, FirstHVACIteration, HXAssistedCoil( HXAssistedCoilNum ).CoolingCoilIndex );
			}

			Error = CoilOutputTempLast - Node( HXAssistedCoil( HXAssistedCoilNum ).HXExhaustAirInletNodeNum ).Temp;
			CoilOutputTempLast = Node( HXAssistedCoil( HXAssistedCoilNum ).HXExhaustAirInletNodeNum ).Temp;
			++Iter;

		}

		// Write excessive iteration warning messages
		if ( Iter > MaxIter ) {
			if ( HXAssistedCoil( HXAssistedCoilNum ).MaxIterCounter < 1 ) {
				++HXAssistedCoil( HXAssistedCoilNum ).MaxIterCounter;
				ShowWarningError( HXAssistedCoil( HXAssistedCoilNum ).HXAssistedCoilType + " \"" + HXAssistedCoil( HXAssistedCoilNum ).Name + "\" -- Exceeded max iterations (" + TrimSigDigits( MaxIter ) + ") while calculating operating conditions." );
				ShowContinueErrorTimeStamp( "" );
			} else {
				ShowRecurringWarningErrorAtEnd( HXAssistedCoil( HXAssistedCoilNum ).HXAssistedCoilType + " \"" + HXAssistedCoil( HXAssistedCoilNum ).Name + "\" -- Exceeded max iterations error continues...", HXAssistedCoil( HXAssistedCoilNum ).MaxIterIndex );
			}
		}

		HXAssistedCoilOutletTemp( HXAssistedCoilNum ) = Node( HXAssistedCoil( HXAssistedCoilNum ).HXAssistedCoilOutletNodeNum ).Temp;
		HXAssistedCoilOutletHumRat( HXAssistedCoilNum ) = Node( HXAssistedCoil( HXAssistedCoilNum ).HXAssistedCoilOutletNodeNum ).HumRat;

	}

	//        End of Reporting subroutines for the HXAssistedCoil Module
	// *****************************************************************************

	void
	GetHXDXCoilIndex(
		std::string const & HXDXCoilName,
		int & HXDXCoilIndex,
		bool & ErrorsFound,
		Optional_string_const CurrentModuleObject
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Richard Raustad
		//       DATE WRITTEN   August 2007
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine sets an index for a given HX Assisted Cooling Coil -- issues error message if that
		// HX is not a legal HX Assisted Cooling Coil.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using InputProcessor::FindItem;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		// na

		// Obtains and allocates HXAssistedCoolingCoil related parameters from input file
		if ( GetCoilsInputFlag ) { // First time subroutine has been called, get input data
			// Get the HXAssistedCoolingCoil input
			GetHXAssistedCoolingCoilInput();
			GetCoilsInputFlag = false; // Set logic flag to disallow getting the input data on future calls to this subroutine
		}

		if ( TotalNumHXAssistedCoils > 0 ) {
			HXDXCoilIndex = FindItem( HXDXCoilName, HXAssistedCoil );
		} else {
			HXDXCoilIndex = 0;
		}

		if ( HXDXCoilIndex == 0 ) {
			if ( present( CurrentModuleObject ) ) {
				ShowSevereError( CurrentModuleObject() + ", GetHXDXCoilIndex: HX Assisted Cooling Coil not found=" + HXDXCoilName );
			} else {
				ShowSevereError( "GetHXDXCoilIndex: HX Assisted Cooling Coil not found=" + HXDXCoilName );
			}
			ErrorsFound = true;
		}

	}

	void
	CheckHXAssistedCoolingCoilSchedule(
		std::string const & EP_UNUSED( CompType ), // unused1208
		std::string const & CompName,
		Real64 & Value,
		int & CompIndex
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Linda Lawrie
		//       DATE WRITTEN   October 2005
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This routine provides a method for outside routines to check if
		// the hx assisted cooling coil is scheduled to be on.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using InputProcessor::FindItem;
		using General::TrimSigDigits;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int HXAssistedCoilNum;

		// Obtains and allocates HXAssistedCoolingCoil related parameters from input file
		if ( GetCoilsInputFlag ) { // First time subroutine has been called, get input data
			// Get the HXAssistedCoolingCoil input
			GetHXAssistedCoolingCoilInput();
			GetCoilsInputFlag = false; // Set logic flag to disallow getting the input data on future calls to this subroutine
		}

		// Find the correct Coil number
		if ( CompIndex == 0 ) {
			if ( TotalNumHXAssistedCoils > 0 ) {
				HXAssistedCoilNum = FindItem( CompName, HXAssistedCoil );
			} else {
				HXAssistedCoilNum = 0;
			}

			if ( HXAssistedCoilNum == 0 ) {
				ShowFatalError( "CheckHXAssistedCoolingCoilSchedule: HX Assisted Coil not found=" + CompName );
			}
			CompIndex = HXAssistedCoilNum;
			Value = 1.0; // not scheduled?
		} else {
			HXAssistedCoilNum = CompIndex;
			if ( HXAssistedCoilNum > TotalNumHXAssistedCoils || HXAssistedCoilNum < 1 ) {
				ShowFatalError( "CheckHXAssistedCoolingCoilSchedule: Invalid CompIndex passed=" + TrimSigDigits( HXAssistedCoilNum ) + ", Number of Heating Coils=" + TrimSigDigits( TotalNumHXAssistedCoils ) + ", Coil name=" + CompName );
			}
			if ( CompName != HXAssistedCoil( HXAssistedCoilNum ).Name ) {
				ShowFatalError( "CheckHXAssistedCoolingCoilSchedule: Invalid CompIndex passed=" + TrimSigDigits( HXAssistedCoilNum ) + ", Coil name=" + CompName + ", stored Coil Name for that index=" + HXAssistedCoil( HXAssistedCoilNum ).Name );
			}

			Value = 1.0; // not scheduled?
		}

	}

	Real64
	GetCoilCapacity(
		std::string const & CoilType, // must match coil types in this module
		std::string const & CoilName, // must match coil names for the coil type
		bool & ErrorsFound // set to true if problem
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Linda Lawrie
		//       DATE WRITTEN   February 2006
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// This function looks up the coil capacity for the given coil and returns it.  If
		// incorrect coil type or name is given, ErrorsFound is returned as true and capacity is returned
		// as negative.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using InputProcessor::FindItem;
		using InputProcessor::SameString;
		auto & GetDXCoilCapacity( DXCoils::GetCoilCapacity );
		using WaterCoils::GetWaterCoilCapacity;

		// Return value
		Real64 CoilCapacity; // returned capacity of matched coil

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		int WhichCoil;
		static int ErrCount( 0 );
		bool errFlag;

		// Obtains and allocates HXAssistedCoolingCoil related parameters from input file
		if ( GetCoilsInputFlag ) { // First time subroutine has been called, get input data
			// Get the HXAssistedCoolingCoil input
			GetHXAssistedCoolingCoilInput();
			GetCoilsInputFlag = false; // Set logic flag to disallow getting the input data on future calls to this subroutine
		}

		errFlag = false;

		if ( TotalNumHXAssistedCoils > 0 ) {
			WhichCoil = FindItem( CoilName, HXAssistedCoil );
		} else {
			WhichCoil = 0;
		}

		if ( SameString( CoilType, "CoilSystem:Cooling:DX:HeatExchangerAssisted" ) ) {
			if ( WhichCoil != 0 ) {
				// coil does not have capacity in input so mine information from DX cooling coil
				CoilCapacity = GetDXCoilCapacity( HXAssistedCoil( WhichCoil ).CoolingCoilType, HXAssistedCoil( WhichCoil ).CoolingCoilName, errFlag );
				if ( errFlag ) {
					ShowRecurringWarningErrorAtEnd( "Requested DX Coil from CoilSystem:Cooling:DX:HeatExchangerAssisted not found", ErrCount );
				}
			}
		} else if ( SameString( CoilType, "CoilSystem:Cooling:Water:HeatExchangerAssisted" ) ) {
			if ( WhichCoil != 0 ) {
				// coil does not have capacity in input so mine information from DX cooling coil
				CoilCapacity = GetWaterCoilCapacity( HXAssistedCoil( WhichCoil ).CoolingCoilType, HXAssistedCoil( WhichCoil ).CoolingCoilName, errFlag );
				if ( errFlag ) {
					ShowRecurringWarningErrorAtEnd( "Requested DX Coil from CoilSystem:Cooling:DX:HeatExchangerAssisted not found", ErrCount );
				}
			}
		} else {
			WhichCoil = 0;
		}

		if ( WhichCoil == 0 ) {
			ShowSevereError( "GetCoilCapacity: Could not find Coil, Type=\"" + CoilType + "\" Name=\"" + CoilName + "\"" );
			ShowContinueError( "... Coil Capacity returned as -1000." );
			ErrorsFound = true;
			CoilCapacity = -1000.0;
		}

		if ( errFlag ) ErrorsFound = true;

		return CoilCapacity;

	}

	int
	GetCoilGroupTypeNum(
		std::string const & CoilType, // must match coil types in this module
		std::string const & CoilName, // must match coil names for the coil type
		bool & ErrorsFound, // set to true if problem
		Optional_bool_const PrintWarning // prints warning message if true
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         R. Raustad - FSEC
		//       DATE WRITTEN   August 2008
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// This function looks up the HX coil type and returns it (CoilDX_CoolingHXAssisted, CoilWater_CoolingHXAssisted)
		// If incorrect coil type or name is given, ErrorsFound is returned as true.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using InputProcessor::FindItem;

		// Return value
		int TypeNum; // returned integerized type of matched coil

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		int WhichCoil;
		bool PrintMessage;

		// Obtains and allocates HXAssistedCoolingCoil related parameters from input file
		if ( GetCoilsInputFlag ) { // First time subroutine has been called, get input data
			// Get the HXAssistedCoolingCoil input
			GetHXAssistedCoolingCoilInput();
			GetCoilsInputFlag = false; // Set logic flag to disallow getting the input data on future calls to this subroutine
		}

		if ( present( PrintWarning ) ) {
			PrintMessage = PrintWarning;
		} else {
			PrintMessage = true;
		}

		if ( TotalNumHXAssistedCoils > 0 ) {
			WhichCoil = FindItem( CoilName, HXAssistedCoil );
		} else {
			WhichCoil = 0;
		}

		if ( WhichCoil != 0 ) {
			// coil does not have capacity in input so mine information from DX cooling coil
			TypeNum = HXAssistedCoil( WhichCoil ).HXAssistedCoilType_Num;
		} else {
			if ( PrintMessage ) {
				ShowSevereError( "GetCoilGroupTypeNum: Could not find Coil, Type=\"" + CoilType + "\" Name=\"" + CoilName + "\"" );
			}
			ErrorsFound = true;
			TypeNum = 0;
		}

		return TypeNum;

	}

	int
	GetCoilObjectTypeNum(
		std::string const & CoilType, // must match coil types in this module
		std::string const & CoilName, // must match coil names for the coil type
		bool & ErrorsFound, // set to true if problem
		Optional_bool_const PrintWarning // prints warning message if true
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         R. Raustad - FSEC
		//       DATE WRITTEN   April 2009
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// This function looks up the coil object type for the given coil and returns it.  If
		// incorrect coil type or name is given, ErrorsFound is returned as true and capacity is returned
		// as negative.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using InputProcessor::FindItem;

		// Return value
		int TypeNum; // returned integerized type of matched coil

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		int WhichCoil;
		bool PrintMessage;

		// Obtains and allocates HXAssistedCoolingCoil related parameters from input file
		if ( GetCoilsInputFlag ) { // First time subroutine has been called, get input data
			// Get the HXAssistedCoolingCoil input
			GetHXAssistedCoolingCoilInput();
			GetCoilsInputFlag = false; // Set logic flag to disallow getting the input data on future calls to this subroutine
		}

		if ( present( PrintWarning ) ) {
			PrintMessage = PrintWarning;
		} else {
			PrintMessage = true;
		}

		if ( TotalNumHXAssistedCoils > 0 ) {
			WhichCoil = FindItem( CoilName, HXAssistedCoil );
		} else {
			WhichCoil = 0;
		}

		if ( WhichCoil != 0 ) {
			TypeNum = HXAssistedCoil( WhichCoil ).CoolingCoilType_Num;
		} else {
			if ( PrintMessage ) {
				ShowSevereError( "GetCoilObjectTypeNum: Could not find Coil, Type=\"" + CoilType + "\" Name=\"" + CoilName + "\"" );
			}
			ErrorsFound = true;
			TypeNum = 0;
		}

		return TypeNum;

	}

	int
	GetCoilInletNode(
		std::string const & CoilType, // must match coil types in this module
		std::string const & CoilName, // must match coil names for the coil type
		bool & ErrorsFound // set to true if problem
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Linda Lawrie
		//       DATE WRITTEN   February 2006
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// This function looks up the given coil and returns the inlet node number.  If
		// incorrect coil type or name is given, ErrorsFound is returned as true and node number is returned
		// as zero.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using InputProcessor::FindItem;

		// Return value
		int NodeNumber; // returned node number of matched coil

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		int WhichCoil;

		// Obtains and allocates HXAssistedCoolingCoil related parameters from input file
		if ( GetCoilsInputFlag ) { // First time subroutine has been called, get input data
			// Get the HXAssistedCoolingCoil input
			GetHXAssistedCoolingCoilInput();
			GetCoilsInputFlag = false; // Set logic flag to disallow getting the input data on future calls to this subroutine
		}

		if ( TotalNumHXAssistedCoils > 0 ) {
			WhichCoil = FindItem( CoilName, HXAssistedCoil );
		} else {
			WhichCoil = 0;
		}

		if ( WhichCoil != 0 ) {
			NodeNumber = HXAssistedCoil( WhichCoil ).HXAssistedCoilInletNodeNum;
		} else {
			ShowSevereError( "GetCoilInletNode: Could not find Coil, Type=\"" + CoilType + "\" Name=\"" + CoilName + "\"" );
			ErrorsFound = true;
			NodeNumber = 0;
		}

		return NodeNumber;

	}

	int
	GetCoilWaterInletNode(
		std::string const & CoilType, // must match coil types in this module
		std::string const & CoilName, // must match coil names for the coil type
		bool & ErrorsFound // set to true if problem
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Linda Lawrie
		//       DATE WRITTEN   April 2011
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// This function looks up the given coil and returns the inlet node number.  If
		// incorrect coil type or name is given, ErrorsFound is returned as true and node number is returned
		// as zero.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using InputProcessor::FindItem;
		auto & GetWaterCoilWaterInletNode( WaterCoils::GetCoilWaterInletNode );

		// Return value
		int NodeNumber; // returned node number of matched coil

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		int WhichCoil;

		// Obtains and allocates HXAssistedCoolingCoil related parameters from input file
		if ( GetCoilsInputFlag ) { // First time subroutine has been called, get input data
			// Get the HXAssistedCoolingCoil input
			GetHXAssistedCoolingCoilInput();
			GetCoilsInputFlag = false; // Set logic flag to disallow getting the input data on future calls to this subroutine
		}

		if ( TotalNumHXAssistedCoils > 0 ) {
			WhichCoil = FindItem( CoilName, HXAssistedCoil );
		} else {
			WhichCoil = 0;
		}

		if ( WhichCoil != 0 ) {
			if ( HXAssistedCoil( WhichCoil ).CoolingCoilType_Num == Coil_CoolingWater ) {
				NodeNumber = GetWaterCoilWaterInletNode( HXAssistedCoil( WhichCoil ).CoolingCoilType, HXAssistedCoil( WhichCoil ).CoolingCoilName, ErrorsFound );
			} else if ( HXAssistedCoil( WhichCoil ).CoolingCoilType_Num == Coil_CoolingWaterDetailed ) {
				NodeNumber = GetWaterCoilWaterInletNode( HXAssistedCoil( WhichCoil ).CoolingCoilType, HXAssistedCoil( WhichCoil ).CoolingCoilName, ErrorsFound );
			} else { // even though validated in Get, still check.
				ShowSevereError( "GetCoilWaterInletNode: Invalid Cooling Coil for HX Assisted Coil, Type=\"" + HXAssistedCoil( WhichCoil ).CoolingCoilType + "\" Name=\"" + CoilName + "\"" );
				ErrorsFound = true;
				NodeNumber = 0; //Autodesk:Return Added line to set return value
			}
		} else {
			ShowSevereError( "GetCoilInletNode: Could not find Coil, Type=\"" + CoilType + "\" Name=\"" + CoilName + "\"" );
			ErrorsFound = true;
			NodeNumber = 0;
		}

		return NodeNumber;

	}

	int
	GetCoilOutletNode(
		std::string const & CoilType, // must match coil types in this module
		std::string const & CoilName, // must match coil names for the coil type
		bool & ErrorsFound // set to true if problem
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         R. Raustad
		//       DATE WRITTEN   August 2006
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// This function looks up the given coil and returns the outlet node number.  If
		// incorrect coil type or name is given, ErrorsFound is returned as true and node number is returned
		// as zero.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using InputProcessor::FindItem;

		// Return value
		int NodeNumber; // returned node number of matched coil

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		int WhichCoil;

		// Obtains and allocates HXAssistedCoolingCoil related parameters from input file
		if ( GetCoilsInputFlag ) { // First time subroutine has been called, get input data
			// Get the HXAssistedCoolingCoil input
			GetHXAssistedCoolingCoilInput();
			GetCoilsInputFlag = false; // Set logic flag to disallow getting the input data on future calls to this subroutine
		}

		if ( TotalNumHXAssistedCoils > 0 ) {
			WhichCoil = FindItem( CoilName, HXAssistedCoil );
		} else {
			WhichCoil = 0;
		}

		if ( WhichCoil != 0 ) {
			NodeNumber = HXAssistedCoil( WhichCoil ).HXAssistedCoilOutletNodeNum;
		} else {
			ShowSevereError( "GetCoilOutletNode: Could not find Coil, Type=\"" + CoilType + "\" Name=\"" + CoilName + "\"" );
			ErrorsFound = true;
			NodeNumber = 0;
		}

		return NodeNumber;

	}

	std::string
	GetHXDXCoilType(
		std::string const & CoilType, // must match coil types in this module
		std::string const & CoilName, // must match coil names for the coil type
		bool & ErrorsFound // set to true if problem
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         R. Raustad, FSEC
		//       DATE WRITTEN   September 2015
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// This function looks up the given coil and returns the cooling coil type.  If
		// incorrect coil type or name is given, ErrorsFound is returned as true and the name
		// is returned as blank

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using InputProcessor::FindItem;

		// Return value
		std::string DXCoilType; // returned type of cooling coil

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		int WhichCoil;

		// Obtains and allocates HXAssistedCoolingCoil related parameters from input file
		if ( GetCoilsInputFlag ) { // First time subroutine has been called, get input data
			// Get the HXAssistedCoolingCoil input
			GetHXAssistedCoolingCoilInput();
			GetCoilsInputFlag = false; // Set logic flag to disallow getting the input data on future calls to this subroutine
		}

		if ( TotalNumHXAssistedCoils > 0 ) {
			WhichCoil = FindItem( CoilName, HXAssistedCoil );
		} else {
			WhichCoil = 0;
		}

		if ( WhichCoil != 0 ) {
			DXCoilType = HXAssistedCoil( WhichCoil ).CoolingCoilType;
		} else {
			ShowSevereError( "Could not find Coil, Type=\"" + CoilType + "\" Name=\"" + CoilName + "\"" );
			ErrorsFound = true;
			DXCoilType = "";
		}

		return DXCoilType;

	}

	std::string
	GetHXDXCoilName(
		std::string const & CoilType, // must match coil types in this module
		std::string const & CoilName, // must match coil names for the coil type
		bool & ErrorsFound // set to true if problem
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Linda Lawrie
		//       DATE WRITTEN   February 2006
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// This function looks up the given coil and returns the cooling coil name.  If
		// incorrect coil type or name is given, ErrorsFound is returned as true and the name
		// is returned as blank

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using InputProcessor::FindItem;

		// Return value
		std::string DXCoilName; // returned name of cooling coil

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		int WhichCoil;

		// Obtains and allocates HXAssistedCoolingCoil related parameters from input file
		if ( GetCoilsInputFlag ) { // First time subroutine has been called, get input data
			// Get the HXAssistedCoolingCoil input
			GetHXAssistedCoolingCoilInput();
			GetCoilsInputFlag = false; // Set logic flag to disallow getting the input data on future calls to this subroutine
		}

		//  HXAssistedCoil(HXAssistedCoilNum)%CoolingCoilName            = AlphArray(7)
		if ( TotalNumHXAssistedCoils > 0 ) {
			WhichCoil = FindItem( CoilName, HXAssistedCoil );
		} else {
			WhichCoil = 0;
		}

		if ( WhichCoil != 0 ) {
			DXCoilName = HXAssistedCoil( WhichCoil ).CoolingCoilName;
		} else {
			ShowSevereError( "Could not find Coil, Type=\"" + CoilType + "\" Name=\"" + CoilName + "\"" );
			ErrorsFound = true;
			DXCoilName = "";
		}

		return DXCoilName;

	}

	int
	GetActualDXCoilIndex(
		std::string const & CoilType, // must match coil types in this module
		std::string const & CoilName, // must match coil names for the coil type
		bool & ErrorsFound // set to true if problem
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Linda Lawrie
		//       DATE WRITTEN   February 2006
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// This function looks up the given coil and returns the cooling coil name.  If
		// incorrect coil type or name is given, ErrorsFound is returned as true and the name
		// is returned as blank

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using InputProcessor::FindItem;

		// Return value
		int DXCoilIndex; // returned index of DX cooling coil

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		int WhichCoil;

		// Obtains and allocates HXAssistedCoolingCoil related parameters from input file
		if ( GetCoilsInputFlag ) { // First time subroutine has been called, get input data
			// Get the HXAssistedCoolingCoil input
			GetHXAssistedCoolingCoilInput();
			GetCoilsInputFlag = false; // Set logic flag to disallow getting the input data on future calls to this subroutine
		}

		if ( TotalNumHXAssistedCoils > 0 ) {
			WhichCoil = FindItem( CoilName, HXAssistedCoil );
		} else {
			WhichCoil = 0;
		}

		if ( WhichCoil != 0 ) {
			// this should be the index to the DX cooling coil object, not the HXAssisted object
			DXCoilIndex = HXAssistedCoil( WhichCoil ).CoolingCoilIndex;
		} else {
			ShowSevereError( "Could not find Coil, Type=\"" + CoilType + "\" Name=\"" + CoilName + "\"" );
			ErrorsFound = true;
			DXCoilIndex = 0;
		}

		return DXCoilIndex;

	}

	std::string
	GetHXCoilType(
		std::string const & CoilType, // must match coil types in this module
		std::string const & CoilName, // must match coil names for the coil type
		bool & ErrorsFound // set to true if problem
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Fred Buhl
		//       DATE WRITTEN   June 2009
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// This function looks up the given coil and returns the cooling coil type.  If
		// incorrect coil type or name is given, ErrorsFound is returned as true and the cooling
		// coil type is returned as blank.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using InputProcessor::FindItem;

		// Return value
		std::string CoolingCoilType; // returned type of cooling coil

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		int WhichCoil;

		// Obtains and allocates HXAssistedCoolingCoil related parameters from input file
		if ( GetCoilsInputFlag ) { // First time subroutine has been called, get input data
			// Get the HXAssistedCoolingCoil input
			GetHXAssistedCoolingCoilInput();
			GetCoilsInputFlag = false; // Set logic flag to disallow getting the input data on future calls to this subroutine
		}

		if ( TotalNumHXAssistedCoils > 0 ) {
			WhichCoil = FindItem( CoilName, HXAssistedCoil );
		} else {
			WhichCoil = 0;
		}

		if ( WhichCoil != 0 ) {
			CoolingCoilType = HXAssistedCoil( WhichCoil ).CoolingCoilType;
		} else {
			ShowSevereError( "Could not find Coil, Type=\"" + CoilType + "\" Name=\"" + CoilName + "\"" );
			ErrorsFound = true;
			CoolingCoilType = "";
		}

		return CoolingCoilType;

	}

	void
	GetHXCoilTypeAndName(
		std::string const & CoilType, // must match coil types in this module
		std::string const & CoilName, // must match coil names for the coil type
		bool & ErrorsFound, // set to true if problem
		std::string & CoolingCoilType, // returned type of cooling coil
		std::string & CoolingCoilName // returned name of cooling coil
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Linda Lawrie
		//       DATE WRITTEN   Oct 2011
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Need to get child coil type and name.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using InputProcessor::FindItem;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int WhichCoil;

		// Obtains and allocates HXAssistedCoolingCoil related parameters from input file
		if ( GetCoilsInputFlag ) { // First time subroutine has been called, get input data
			// Get the HXAssistedCoolingCoil input
			GetHXAssistedCoolingCoilInput();
			GetCoilsInputFlag = false; // Set logic flag to disallow getting the input data on future calls to this subroutine
		}

		if ( TotalNumHXAssistedCoils > 0 ) {
			WhichCoil = FindItem( CoilName, HXAssistedCoil );
		} else {
			WhichCoil = 0;
		}

		if ( WhichCoil != 0 ) {
			CoolingCoilType = HXAssistedCoil( WhichCoil ).CoolingCoilType;
			CoolingCoilName = HXAssistedCoil( WhichCoil ).CoolingCoilName;
		} else {
			ShowSevereError( "Could not find Coil, Type=\"" + CoilType + "\" Name=\"" + CoilName + "\"" );
			ErrorsFound = true;
			CoolingCoilType = "";
			CoolingCoilName = "";
		}

	}

	Real64
	GetCoilMaxWaterFlowRate(
		std::string const & CoilType, // must match coil types in this module
		std::string const & CoilName, // must match coil names for the coil type
		bool & ErrorsFound // set to true if problem
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Linda Lawrie
		//       DATE WRITTEN   November 2006
		//       MODIFIED       R. Raustad, April 2009 - added water coil ELSE IF
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// This function looks up the max water flow rate for the given coil and returns it.  If
		// incorrect coil type or name is given, ErrorsFound is returned as true and capacity is returned
		// as negative.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using InputProcessor::FindItem;
		using InputProcessor::SameString;
		auto & GetWaterCoilMaxFlowRate( WaterCoils::GetCoilMaxWaterFlowRate );

		// Return value
		Real64 MaxWaterFlowRate; // returned max water flow rate of matched coil

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		int WhichCoil;
		static int ErrCount( 0 );

		// Obtains and allocates HXAssistedCoolingCoil related parameters from input file
		if ( GetCoilsInputFlag ) { // First time subroutine has been called, get input data
			// Get the HXAssistedCoolingCoil input
			GetHXAssistedCoolingCoilInput();
			GetCoilsInputFlag = false; // Set logic flag to disallow getting the input data on future calls to this subroutine
		}

		if ( TotalNumHXAssistedCoils > 0 ) {

			WhichCoil = FindItem( CoilName, HXAssistedCoil );

			if ( SameString( CoilType, "CoilSystem:Cooling:DX:HeatExchangerAssisted" ) ) {
				if ( WhichCoil != 0 ) {
					// coil does not specify MaxWaterFlowRate
					MaxWaterFlowRate = 0.0;
					ShowRecurringWarningErrorAtEnd( "Requested Max Water Flow Rate from CoilSystem:Cooling:DX:HeatExchangerAssisted N/A", ErrCount );
				}
			} else if ( SameString( CoilType, "CoilSystem:Cooling:Water:HeatExchangerAssisted" ) ) {
				if ( WhichCoil != 0 ) {
					MaxWaterFlowRate = GetWaterCoilMaxFlowRate( cAllCoilTypes( GetCoilObjectTypeNum( CoilType, CoilName, ErrorsFound ) ), GetHXDXCoilName( CoilType, CoilName, ErrorsFound ), ErrorsFound );
				}
			} else {
				WhichCoil = 0;
			}

			if ( WhichCoil == 0 ) {
				ShowSevereError( "GetCoilMaxWaterFlowRate: Could not find Coil, Type=\"" + CoilType + "\" Name=\"" + CoilName + "\"" );
				ErrorsFound = true;
				MaxWaterFlowRate = -1000.0;
			}
		} else {
			ShowSevereError( "GetCoilMaxWaterFlowRate: Could not find Coil, Type=\"" + CoilType + "\" Name=\"" + CoilName + "\"" );
			ErrorsFound = true;
			MaxWaterFlowRate = -1000.0;
		}

		return MaxWaterFlowRate;

	}

	Real64
	GetHXCoilAirFlowRate(
		std::string const & CoilType, // must match coil types in this module
		std::string const & CoilName, // must match coil names for the coil type
		bool & ErrorsFound // set to true if problem
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Richard Raustad
		//       DATE WRITTEN   September 2013
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// This function looks up the max air flow rate for the given HX and returns it.  If
		// incorrect coil type or name is given, ErrorsFound is returned as true and capacity is returned
		// as negative.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using InputProcessor::FindItem;
		using InputProcessor::SameString;
		using HeatRecovery::GetSupplyAirFlowRate;

		// Return value
		Real64 MaxAirFlowRate; // returned max air flow rate of matched HX

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		int WhichCoil;

		// Obtains and allocates HXAssistedCoolingCoil related parameters from input file
		if ( GetCoilsInputFlag ) { // First time subroutine has been called, get input data
			// Get the HXAssistedCoolingCoil input
			GetHXAssistedCoolingCoilInput();
			GetCoilsInputFlag = false; // Set logic flag to disallow getting the input data on future calls to this subroutine
		}

		if ( TotalNumHXAssistedCoils > 0 ) {

			WhichCoil = FindItem( CoilName, HXAssistedCoil );

			if ( SameString( CoilType, "CoilSystem:Cooling:DX:HeatExchangerAssisted" ) || SameString( CoilType, "CoilSystem:Cooling:Water:HeatExchangerAssisted" ) ) {
				if ( WhichCoil != 0 ) {
					MaxAirFlowRate = GetSupplyAirFlowRate( HXAssistedCoil( WhichCoil ).HeatExchangerName, ErrorsFound );
				}
			} else {
				WhichCoil = 0;
			}

			if ( WhichCoil == 0 ) {
				ShowSevereError( "GetHXCoilAirFlowRate: Could not find HX, Type=\"" + CoilType + "\" Name=\"" + CoilName + "\"" );
				ErrorsFound = true;
				MaxAirFlowRate = -1000.0;
			}
		} else {
			ShowSevereError( "GetHXCoilAirFlowRate: Could not find HX, Type=\"" + CoilType + "\" Name=\"" + CoilName + "\"" );
			ErrorsFound = true;
			MaxAirFlowRate = -1000.0;
		}

		return MaxAirFlowRate;

	}

	bool
	VerifyHeatExchangerParent(
		std::string const & HXType, // must match coil types in this module
		std::string const & HXName // must match coil names for the coil type
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Lixing Gu
		//       DATE WRITTEN   January 2009
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// This function looks up the given heat exchanger name and type and returns true or false.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using InputProcessor::FindItem;
		using InputProcessor::SameString;

		// Return value
		bool Found; // set to true if found

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		int WhichCoil;

		// Obtains and allocates HXAssistedCoolingCoil related parameters from input file
		if ( GetCoilsInputFlag ) { // First time subroutine has been called, get input data
			// Get the HXAssistedCoolingCoil input
			GetHXAssistedCoolingCoilInput();
			GetCoilsInputFlag = false; // Set logic flag to disallow getting the input data on future calls to this subroutine
		}

		Found = false;

		if ( TotalNumHXAssistedCoils > 0 ) {
			WhichCoil = FindItem( HXName, HXAssistedCoil, &HXAssistedCoilParameters::HeatExchangerName );
		} else {
			WhichCoil = 0;
		}

		if ( WhichCoil != 0 ) {
			if ( SameString( HXAssistedCoil( WhichCoil ).HeatExchangerType, HXType ) ) {
				Found = true;
			}
		}

		return Found;

	}

	//        End of Utility subroutines for the HXAssistedCoil Module
	// *****************************************************************************

} // HVACHXAssistedCoolingCoil

} // EnergyPlus
