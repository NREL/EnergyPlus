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

// EnergyPlus Headers
#include <BaseboardRadiator.hh>
#include <BranchNodeConnections.hh>
#include <DataEnvironment.hh>
#include <DataHeatBalance.hh>
#include <DataHVACGlobals.hh>
#include <DataIPShortCuts.hh>
#include <DataLoopNode.hh>
#include <DataPlant.hh>
#include <DataPrecisionGlobals.hh>
#include <DataSizing.hh>
#include <DataZoneEnergyDemands.hh>
#include <DataZoneEquipment.hh>
#include <FluidProperties.hh>
#include <General.hh>
#include <GeneralRoutines.hh>
#include <GlobalNames.hh>
#include <InputProcessor.hh>
#include <NodeInputManager.hh>
#include <OutputProcessor.hh>
#include <PlantUtilities.hh>
#include <Psychrometrics.hh>
#include <ReportSizingManager.hh>
#include <ScheduleManager.hh>
#include <UtilityRoutines.hh>

namespace EnergyPlus {

// Note: This file contains two modules:
// Module BaseboardRadiator -- (ref: Object: ZoneHVAC:Baseboard:Convective:Water)
// Module BaseboardElectric -- (ref: Object: ZoneHVAC:Baseboard:Convective:Electric)

namespace BaseboardRadiator {
	// Module containing the routines dealing with the BASEBOARD HEATER
	// component(s).

	// MODULE INFORMATION:
	//       AUTHOR         Russ Taylor
	//       DATE WRITTEN   Jan 1998
	//       MODIFIED       Fred Buhl, October 1999
	//       RE-ENGINEERED  na

	// PURPOSE OF THIS MODULE:
	// Needs description

	// METHODOLOGY EMPLOYED:
	// Needs description, as appropriate

	// REFERENCES: none

	// OTHER NOTES: none

	// USE STATEMENTS:
	// Use statements for data only modules
	// Using/Aliasing
	using namespace DataPrecisionGlobals;
	using namespace DataGlobals;
	using DataHVACGlobals::SmallLoad;
	using DataEnvironment::StdRhoAir;
	using DataPlant::PlantLoop;
	using DataPlant::TypeOf_Baseboard_Conv_Water;

	// Use statements for access to subroutines in other modules
	using namespace ScheduleManager;
	using Psychrometrics::PsyCpAirFnWTdb;
	using Psychrometrics::PsyRhoAirFnPbTdbW;
	using FluidProperties::GetDensityGlycol;
	using FluidProperties::GetSpecificHeatGlycol;

	// Data
	//MODULE PARAMETER DEFINITIONS
	Real64 const SimpConvAirFlowSpeed( 0.5 ); // m/s
	static std::string const cCMO_BBRadiator_Water( "ZoneHVAC:Baseboard:Convective:Water" );

	// DERIVED TYPE DEFINITIONS

	//MODULE VARIABLE DECLARATIONS:
	int NumBaseboards( 0 );
	Array1D_bool MySizeFlag;
	Array1D_bool CheckEquipName;
	Array1D_bool SetLoopIndexFlag; // get loop number flag

	//SUBROUTINE SPECIFICATIONS FOR MODULE BaseboardRadiator

	// Object Data
	Array1D< BaseboardParams > Baseboard;
	Array1D< BaseboardParamsNumericFieldData > BaseboardParamsNumericFields;

	// Functions

	void
	clear_state()
	{
		NumBaseboards = 0;
		MySizeFlag.deallocate();
		CheckEquipName.deallocate();
		SetLoopIndexFlag.deallocate();
		Baseboard.deallocate();
		BaseboardParamsNumericFields.deallocate();
	}

	void
	SimBaseboard(
		std::string const & EquipName,
		int const ActualZoneNum,
		int const ControlledZoneNum,
		bool const FirstHVACIteration,
		Real64 & PowerMet,
		int & CompIndex
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Russ Taylor
		//       DATE WRITTEN   Nov 1997
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine simulates the Baseboard Radiators.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataLoopNode::Node;
		using InputProcessor::FindItemInList;
		using DataZoneEnergyDemands::ZoneSysEnergyDemand;
		using General::TrimSigDigits;
		using PlantUtilities::SetActuatedBranchFlowRate;

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		int BaseboardNum; // index of unit in baseboard array
		static bool GetInputFlag( true ); // one time get input flag
		Real64 QZnReq; // zone load not yet satisfied
		Real64 MaxWaterFlow;
		Real64 MinWaterFlow;
		Real64 DummyMdot;

		if ( GetInputFlag ) {
			GetBaseboardInput();
			GetInputFlag = false;
		}

		// Find the correct Baseboard Equipment
		if ( CompIndex == 0 ) {
			BaseboardNum = FindItemInList( EquipName, Baseboard, &BaseboardParams::EquipID );
			if ( BaseboardNum == 0 ) {
				ShowFatalError( "SimBaseboard: Unit not found=" + EquipName );
			}
			CompIndex = BaseboardNum;
		} else {
			BaseboardNum = CompIndex;
			if ( BaseboardNum > NumBaseboards || BaseboardNum < 1 ) {
				ShowFatalError( "SimBaseboard:  Invalid CompIndex passed=" + TrimSigDigits( BaseboardNum ) + ", Number of Units=" + TrimSigDigits( NumBaseboards ) + ", Entered Unit name=" + EquipName );
			}
			if ( CheckEquipName( BaseboardNum ) ) {
				if ( EquipName != Baseboard( BaseboardNum ).EquipID ) {
					ShowFatalError( "SimBaseboard: Invalid CompIndex passed=" + TrimSigDigits( BaseboardNum ) + ", Unit name=" + EquipName + ", stored Unit Name for that index=" + Baseboard( BaseboardNum ).EquipID );
				}
				CheckEquipName( BaseboardNum ) = false;
			}
		}

		InitBaseboard( BaseboardNum, ControlledZoneNum );

		QZnReq = ZoneSysEnergyDemand( ActualZoneNum ).RemainingOutputReqToHeatSP;

		if ( ( QZnReq < SmallLoad ) || ( Baseboard( BaseboardNum ).WaterInletTemp <= Baseboard( BaseboardNum ).AirInletTemp ) ) {
			//  IF (Baseboard(BaseboardNum)%WaterInletTemp <= Baseboard(BaseboardNum)%AirInletTemp) THEN
			// The baseboard cannot provide cooling.  Thus, if the zone required load is negative or the water inlet
			// temperature is lower than the zone air temperature, then we need to shut down the baseboard unit

			Baseboard( BaseboardNum ).WaterOutletTemp = Baseboard( BaseboardNum ).WaterInletTemp;
			Baseboard( BaseboardNum ).AirOutletTemp = Baseboard( BaseboardNum ).AirInletTemp;
			Baseboard( BaseboardNum ).Power = 0.0;
			Baseboard( BaseboardNum ).WaterMassFlowRate = 0.0;
			// init hot water flow rate to zero
			DummyMdot = 0.0;
			SetActuatedBranchFlowRate( DummyMdot, Baseboard( BaseboardNum ).WaterInletNode, Baseboard( BaseboardNum ).LoopNum, Baseboard( BaseboardNum ).LoopSideNum, Baseboard( BaseboardNum ).BranchNum, false );

		} else {
			// init hot water flow rate to zero
			DummyMdot = 0.0;
			SetActuatedBranchFlowRate( DummyMdot, Baseboard( BaseboardNum ).WaterInletNode, Baseboard( BaseboardNum ).LoopNum, Baseboard( BaseboardNum ).LoopSideNum, Baseboard( BaseboardNum ).BranchNum, true );

			//On the first HVAC iteration the system values are given to the controller, but after that
			// the demand limits are in place and there needs to be feedback to the Zone Equipment
			if ( FirstHVACIteration ) {
				MaxWaterFlow = Baseboard( BaseboardNum ).WaterMassFlowRateMax;
				MinWaterFlow = 0.0;
			} else {
				MaxWaterFlow = Node( Baseboard( BaseboardNum ).WaterInletNode ).MassFlowRateMaxAvail;
				MinWaterFlow = Node( Baseboard( BaseboardNum ).WaterInletNode ).MassFlowRateMinAvail;
			}

			ControlCompOutput( Baseboard( BaseboardNum ).EquipID, cCMO_BBRadiator_Water, BaseboardNum, FirstHVACIteration, QZnReq, Baseboard( BaseboardNum ).WaterInletNode, MaxWaterFlow, MinWaterFlow, Baseboard( BaseboardNum ).Offset, Baseboard( BaseboardNum ).ControlCompTypeNum, Baseboard( BaseboardNum ).CompErrIndex, _, _, _, _, _, Baseboard( BaseboardNum ).LoopNum, Baseboard( BaseboardNum ).LoopSideNum, Baseboard( BaseboardNum ).BranchNum );

			PowerMet = Baseboard( BaseboardNum ).Power;

		}

		UpdateBaseboard( BaseboardNum );
		ReportBaseboard( BaseboardNum );

	}

	void
	GetBaseboardInput()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Russ Taylor
		//       DATE WRITTEN   Nov 1997
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine gets the input for the Baseboard units.

		// METHODOLOGY EMPLOYED:
		// Standard input processor calls.

		// REFERENCES:
		// na

		// Using/Aliasing
		using InputProcessor::GetNumObjectsFound;
		using InputProcessor::GetObjectItem;
		using InputProcessor::VerifyName;
		using InputProcessor::SameString;
		using NodeInputManager::GetOnlySingleNode;
		using BranchNodeConnections::TestCompSet;
		using namespace DataLoopNode;
		using GlobalNames::VerifyUniqueBaseboardName;
		using namespace DataIPShortCuts;
		using namespace DataSizing;
		using General::TrimSigDigits;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "GetBaseboardInput: " ); // include trailing blank space
		int const iHeatCAPMAlphaNum( 5 ); // get input index to water baseboard Radiator system heating capacity sizing method
		int const iHeatDesignCapacityNumericNum( 1 ); // get input index to water baseboard Radiator system electric heating capacity
		int const iHeatCapacityPerFloorAreaNumericNum( 2 ); // get input index to water baseboard Radiator system electric heating capacity per floor area sizing
		int const iHeatFracOfAutosizedCapacityNumericNum( 3 ); //  get input index to water baseboard Radiator system electric heating capacity sizing as fraction of autozized heating capacity

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int BaseboardNum;
		int NumConvHWBaseboards;
		int ConvHWBaseboardNum;
		int NumAlphas;
		int NumNums;
		int IOStat;
		static bool ErrorsFound( false ); // If errors detected in input
		bool IsNotOK; // Flag to verify name
		bool IsBlank; // Flag for blank name
		bool errFlag;

		cCurrentModuleObject = cCMO_BBRadiator_Water;

		NumConvHWBaseboards = GetNumObjectsFound( cCurrentModuleObject );

		// Calculate total number of baseboard units
		NumBaseboards = NumConvHWBaseboards;

		Baseboard.allocate( NumBaseboards );
		CheckEquipName.dimension( NumBaseboards, true );
		BaseboardParamsNumericFields.allocate( NumBaseboards );

		if ( NumConvHWBaseboards > 0 ) { //Get the data for cooling schemes
			BaseboardNum = 0;
			for ( ConvHWBaseboardNum = 1; ConvHWBaseboardNum <= NumConvHWBaseboards; ++ConvHWBaseboardNum ) {

				GetObjectItem( cCurrentModuleObject, ConvHWBaseboardNum, cAlphaArgs, NumAlphas, rNumericArgs, NumNums, IOStat, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );

				BaseboardParamsNumericFields( ConvHWBaseboardNum ).FieldNames.allocate(NumNums);
				BaseboardParamsNumericFields( ConvHWBaseboardNum ).FieldNames = "";
				BaseboardParamsNumericFields( ConvHWBaseboardNum ).FieldNames = cNumericFieldNames;

				IsNotOK = false;
				IsBlank = false;
				VerifyName( cAlphaArgs( 1 ), Baseboard, &BaseboardParams::EquipID, BaseboardNum, IsNotOK, IsBlank, cCurrentModuleObject + " Name" );
				if ( IsNotOK ) {
					ErrorsFound = true;
					continue;
				}
				VerifyUniqueBaseboardName( cCurrentModuleObject, cAlphaArgs( 1 ), errFlag, cCurrentModuleObject + " Name" );
				if ( errFlag ) {
					ErrorsFound = true;
				}
				++BaseboardNum;
				Baseboard( BaseboardNum ).EquipID = cAlphaArgs( 1 ); // name of this baseboard
				Baseboard( BaseboardNum ).EquipType = TypeOf_Baseboard_Conv_Water;
				Baseboard( BaseboardNum ).Schedule = cAlphaArgs( 2 );
				if ( lAlphaFieldBlanks( 2 ) ) {
					Baseboard( BaseboardNum ).SchedPtr = ScheduleAlwaysOn;
				} else {
					Baseboard( BaseboardNum ).SchedPtr = GetScheduleIndex( cAlphaArgs( 2 ) );
					if ( Baseboard( BaseboardNum ).SchedPtr == 0 ) {
						ShowSevereError( RoutineName + cCurrentModuleObject + ": invalid " + cAlphaFieldNames( 2 ) + " entered =" + cAlphaArgs( 2 ) + " for " + cAlphaFieldNames( 1 ) + '=' + cAlphaArgs( 1 ) );
						ErrorsFound = true;
					}
				}
				// get inlet node number
				Baseboard( BaseboardNum ).WaterInletNode = GetOnlySingleNode( cAlphaArgs( 3 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Water, NodeConnectionType_Inlet, 1, ObjectIsNotParent );
				// get outlet node number
				Baseboard( BaseboardNum ).WaterOutletNode = GetOnlySingleNode( cAlphaArgs( 4 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Water, NodeConnectionType_Outlet, 1, ObjectIsNotParent );

				TestCompSet( cCMO_BBRadiator_Water, cAlphaArgs( 1 ), cAlphaArgs( 3 ), cAlphaArgs( 4 ), "Hot Water Nodes" );

				// Determine steam baseboard radiator system heating design capacity sizing method
				if ( SameString( cAlphaArgs(iHeatCAPMAlphaNum), "HeatingDesignCapacity" ) ) {
					Baseboard(BaseboardNum).HeatingCapMethod = HeatingDesignCapacity;
					if ( !lNumericFieldBlanks(iHeatDesignCapacityNumericNum) ) {
						Baseboard(BaseboardNum).ScaledHeatingCapacity = rNumericArgs(iHeatDesignCapacityNumericNum);
						if ( Baseboard( BaseboardNum ).ScaledHeatingCapacity < 0.0 && Baseboard( BaseboardNum ).ScaledHeatingCapacity != AutoSize ) {
							ShowSevereError( cCMO_BBRadiator_Water  + " = " + Baseboard( BaseboardNum ).EquipID);
							ShowContinueError("Illegal " + cNumericFieldNames(iHeatDesignCapacityNumericNum) + " = " + TrimSigDigits(rNumericArgs(iHeatDesignCapacityNumericNum), 7));
							ErrorsFound = true;
						}
					} else {
						ShowSevereError( cCMO_BBRadiator_Water  + " = " + Baseboard( BaseboardNum ).EquipID);
						ShowContinueError("Input for " + cAlphaFieldNames(iHeatCAPMAlphaNum) + " = " + cAlphaArgs(iHeatCAPMAlphaNum));
						ShowContinueError("Blank field not allowed for " + cNumericFieldNames(iHeatDesignCapacityNumericNum));
						ErrorsFound = true;
					}
				} else if ( SameString( cAlphaArgs(iHeatCAPMAlphaNum), "CapacityPerFloorArea" ) ) {
					Baseboard( BaseboardNum ).HeatingCapMethod = CapacityPerFloorArea;
					if ( !lNumericFieldBlanks(iHeatCapacityPerFloorAreaNumericNum) ) {
						Baseboard( BaseboardNum ).ScaledHeatingCapacity = rNumericArgs(iHeatCapacityPerFloorAreaNumericNum);
						if ( Baseboard( BaseboardNum ).ScaledHeatingCapacity <= 0.0 ) {
							ShowSevereError( cCMO_BBRadiator_Water  + " = " + Baseboard( BaseboardNum ).EquipID);
							ShowContinueError("Input for " + cAlphaFieldNames(iHeatCAPMAlphaNum) + " = " + cAlphaArgs(iHeatCAPMAlphaNum));
							ShowContinueError("Illegal " + cNumericFieldNames(iHeatCapacityPerFloorAreaNumericNum) + " = " + TrimSigDigits(rNumericArgs(iHeatCapacityPerFloorAreaNumericNum), 7));
							ErrorsFound = true;
						} else if ( Baseboard( BaseboardNum ).ScaledHeatingCapacity == AutoSize ) {
							ShowSevereError( cCMO_BBRadiator_Water  + " = " + Baseboard( BaseboardNum ).EquipID);
							ShowContinueError("Input for " + cAlphaFieldNames(iHeatCAPMAlphaNum) + " = " + cAlphaArgs(iHeatCAPMAlphaNum));
							ShowContinueError("Illegal " + cNumericFieldNames(iHeatCapacityPerFloorAreaNumericNum) + " = Autosize");
							ErrorsFound = true;
						}
					} else {
						ShowSevereError( cCMO_BBRadiator_Water  + " = " + Baseboard( BaseboardNum ).EquipID);
						ShowContinueError("Input for " + cAlphaFieldNames(iHeatCAPMAlphaNum) + " = " + cAlphaArgs(iHeatCAPMAlphaNum));
						ShowContinueError("Blank field not allowed for " + cNumericFieldNames(iHeatCapacityPerFloorAreaNumericNum));
						ErrorsFound = true;
					}
				} else if ( SameString( cAlphaArgs(iHeatCAPMAlphaNum), "FractionOfAutosizedHeatingCapacity" ) ) {
					Baseboard( BaseboardNum ).HeatingCapMethod = FractionOfAutosizedHeatingCapacity;
					if ( !lNumericFieldBlanks(iHeatFracOfAutosizedCapacityNumericNum) ) {
						Baseboard( BaseboardNum ).ScaledHeatingCapacity = rNumericArgs(iHeatFracOfAutosizedCapacityNumericNum);
						if ( Baseboard( BaseboardNum ).ScaledHeatingCapacity < 0.0 ) {
							ShowSevereError( cCMO_BBRadiator_Water  + " = " + Baseboard( BaseboardNum ).EquipID);
							ShowContinueError("Illegal " + cNumericFieldNames(iHeatFracOfAutosizedCapacityNumericNum) + " = " + TrimSigDigits(rNumericArgs(iHeatFracOfAutosizedCapacityNumericNum), 7));
							ErrorsFound = true;
						}
					} else {
						ShowSevereError( cCMO_BBRadiator_Water  + " = " + Baseboard( BaseboardNum ).EquipID);
						ShowContinueError("Input for " + cAlphaFieldNames(iHeatCAPMAlphaNum) + " = " + cAlphaArgs(iHeatCAPMAlphaNum));
						ShowContinueError("Blank field not allowed for " + cNumericFieldNames(iHeatFracOfAutosizedCapacityNumericNum));
						ErrorsFound = true;
					}
				} else {
					ShowSevereError( cCMO_BBRadiator_Water  + " = " + Baseboard( BaseboardNum ).EquipID);
					ShowContinueError("Illegal " + cAlphaFieldNames(iHeatCAPMAlphaNum) + " = " + cAlphaArgs(iHeatCAPMAlphaNum));
					ErrorsFound = true;
				}

				Baseboard( BaseboardNum ).UA = rNumericArgs( 4 );
				Baseboard( BaseboardNum ).WaterVolFlowRateMax = rNumericArgs( 5 );
				Baseboard( BaseboardNum ).Offset = rNumericArgs( 6 );
				// Set default convergence tolerance
				if ( Baseboard( BaseboardNum ).Offset <= 0.0 ) {
					Baseboard( BaseboardNum ).Offset = 0.001;
				}
			}

			if ( ErrorsFound ) {
				ShowFatalError( RoutineName + "Errors found in getting input.  Preceding condition(s) cause termination." );
			}
		}

		for ( BaseboardNum = 1; BaseboardNum <= NumBaseboards; ++BaseboardNum ) {

			// Setup Report variables for the unit
			// CurrentModuleObject='ZoneHVAC:Baseboard:Convective:Water'
			SetupOutputVariable( "Baseboard Total Heating Energy [J]", Baseboard( BaseboardNum ).Energy, "System", "Sum", Baseboard( BaseboardNum ).EquipID, _, "ENERGYTRANSFER", "BASEBOARD", _, "System" );

			SetupOutputVariable( "Baseboard Hot Water Energy [J]", Baseboard( BaseboardNum ).Energy, "System", "Sum", Baseboard( BaseboardNum ).EquipID, _, "PLANTLOOPHEATINGDEMAND", "BASEBOARD", _, "System" );

			SetupOutputVariable( "Baseboard Total Heating Rate [W]", Baseboard( BaseboardNum ).Power, "System", "Average", Baseboard( BaseboardNum ).EquipID );

			SetupOutputVariable( "Baseboard Hot Water Mass Flow Rate [kg/s]", Baseboard( BaseboardNum ).WaterMassFlowRate, "System", "Average", Baseboard( BaseboardNum ).EquipID );

			SetupOutputVariable( "Baseboard Air Mass Flow Rate [kg/s]", Baseboard( BaseboardNum ).AirMassFlowRate, "System", "Average", Baseboard( BaseboardNum ).EquipID );

			SetupOutputVariable( "Baseboard Air Inlet Temperature [C]", Baseboard( BaseboardNum ).AirInletTemp, "System", "Average", Baseboard( BaseboardNum ).EquipID );

			SetupOutputVariable( "Baseboard Air Outlet Temperature [C]", Baseboard( BaseboardNum ).AirOutletTemp, "System", "Average", Baseboard( BaseboardNum ).EquipID );

			SetupOutputVariable( "Baseboard Water Inlet Temperature [C]", Baseboard( BaseboardNum ).WaterInletTemp, "System", "Average", Baseboard( BaseboardNum ).EquipID );

			SetupOutputVariable( "Baseboard Water Outlet Temperature [C]", Baseboard( BaseboardNum ).WaterOutletTemp, "System", "Average", Baseboard( BaseboardNum ).EquipID );
		}

	}

	void
	InitBaseboard(
		int const BaseboardNum,
		int const ControlledZoneNumSub
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Russ Taylor
		//       DATE WRITTEN   Nov 1997
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine initializes the Baseboard units during simulation.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataLoopNode::Node;
		using DataZoneEquipment::ZoneEquipInputsFilled;
		using DataZoneEquipment::CheckZoneEquipmentList;
		using DataZoneEquipment::ZoneEquipConfig;
		using PlantUtilities::InitComponentNodes;
		using DataPlant::ScanPlantLoopsForObject;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "BaseboardRadiator:InitBaseboard" );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int WaterInletNode;
		int ZoneNode;
		static bool MyOneTimeFlag( true );
		static bool ZoneEquipmentListChecked( false ); // True after the Zone Equipment List has been checked for items
		int Loop;
		static Array1D_bool MyEnvrnFlag;
		Real64 RhoAirStdInit;
		Real64 rho; // local fluid density
		Real64 Cp; // local fluid specific heat
		bool errFlag;

		if ( Baseboard( BaseboardNum ).ZonePtr <= 0 ) Baseboard( BaseboardNum ).ZonePtr = ZoneEquipConfig( ControlledZoneNumSub ).ActualZoneNum;

		// Do the one time initializations
		if ( MyOneTimeFlag ) {
			// initialize the environment and sizing flags
			MyEnvrnFlag.allocate( NumBaseboards );
			MySizeFlag.allocate( NumBaseboards );
			SetLoopIndexFlag.allocate( NumBaseboards );
			MyEnvrnFlag = true;
			MySizeFlag = true;
			MyOneTimeFlag = false;
			SetLoopIndexFlag = true;
		}
		if ( SetLoopIndexFlag( BaseboardNum ) && allocated( PlantLoop ) ) {
			errFlag = false;
			ScanPlantLoopsForObject( Baseboard( BaseboardNum ).EquipID, Baseboard( BaseboardNum ).EquipType, Baseboard( BaseboardNum ).LoopNum, Baseboard( BaseboardNum ).LoopSideNum, Baseboard( BaseboardNum ).BranchNum, Baseboard( BaseboardNum ).CompNum, _, _, _, _, _, errFlag );
			if ( errFlag ) {
				ShowFatalError( "InitBaseboard: Program terminated for previous conditions." );
			}
			SetLoopIndexFlag( BaseboardNum ) = false;
		}
		// need to check all units to see if they are on ZoneHVAC:EquipmentList or issue warning
		if ( ! ZoneEquipmentListChecked && ZoneEquipInputsFilled ) {
			ZoneEquipmentListChecked = true;
			for ( Loop = 1; Loop <= NumBaseboards; ++Loop ) {
				if ( CheckZoneEquipmentList( cCMO_BBRadiator_Water, Baseboard( Loop ).EquipID ) ) continue;
				ShowSevereError( "InitBaseboard: Unit=[" + cCMO_BBRadiator_Water + ',' + Baseboard( Loop ).EquipID + "] is not on any ZoneHVAC:EquipmentList.  It will not be simulated." );
			}
		}

		if ( ! SysSizingCalc && MySizeFlag( BaseboardNum ) && ! SetLoopIndexFlag( BaseboardNum ) ) {
			// for each coil, do the sizing once.
			SizeBaseboard( BaseboardNum );

			MySizeFlag( BaseboardNum ) = false;
		}

		// Do the Begin Environment initializations
		if ( BeginEnvrnFlag && MyEnvrnFlag( BaseboardNum ) && ! SetLoopIndexFlag( BaseboardNum ) ) {
			RhoAirStdInit = StdRhoAir;
			WaterInletNode = Baseboard( BaseboardNum ).WaterInletNode;
			rho = GetDensityGlycol( PlantLoop( Baseboard( BaseboardNum ).LoopNum ).FluidName, InitConvTemp, PlantLoop( Baseboard( BaseboardNum ).LoopNum ).FluidIndex, RoutineName );
			Baseboard( BaseboardNum ).WaterMassFlowRateMax = rho * Baseboard( BaseboardNum ).WaterVolFlowRateMax;
			InitComponentNodes( 0.0, Baseboard( BaseboardNum ).WaterMassFlowRateMax, Baseboard( BaseboardNum ).WaterInletNode, Baseboard( BaseboardNum ).WaterOutletNode, Baseboard( BaseboardNum ).LoopNum, Baseboard( BaseboardNum ).LoopSideNum, Baseboard( BaseboardNum ).BranchNum, Baseboard( BaseboardNum ).CompNum );
			Node( WaterInletNode ).Temp = 60.0;
			Cp = GetSpecificHeatGlycol( PlantLoop( Baseboard( BaseboardNum ).LoopNum ).FluidName, Node( WaterInletNode ).Temp, PlantLoop( Baseboard( BaseboardNum ).LoopNum ).FluidIndex, RoutineName );
			Node( WaterInletNode ).Enthalpy = Cp * Node( WaterInletNode ).Temp;
			Node( WaterInletNode ).Quality = 0.0;
			Node( WaterInletNode ).Press = 0.0;
			Node( WaterInletNode ).HumRat = 0.0;
			// pick a mass flow rate that depends on the max water mass flow rate. CR 8842 changed to factor of 2.0
			if ( Baseboard( BaseboardNum ).AirMassFlowRate <= 0.0 ) {
				Baseboard( BaseboardNum ).AirMassFlowRate = 2.0 * Baseboard( BaseboardNum ).WaterMassFlowRateMax;
			}
			MyEnvrnFlag( BaseboardNum ) = false;
		}

		if ( ! BeginEnvrnFlag ) {
			MyEnvrnFlag( BaseboardNum ) = true;
		}

		// Do the every time step initializations
		WaterInletNode = Baseboard( BaseboardNum ).WaterInletNode;
		ZoneNode = ZoneEquipConfig( ControlledZoneNumSub ).ZoneNode;
		Baseboard( BaseboardNum ).WaterMassFlowRate = Node( WaterInletNode ).MassFlowRate;
		Baseboard( BaseboardNum ).WaterInletTemp = Node( WaterInletNode ).Temp;
		Baseboard( BaseboardNum ).WaterInletEnthalpy = Node( WaterInletNode ).Enthalpy;
		Baseboard( BaseboardNum ).AirInletTemp = Node( ZoneNode ).Temp;
		Baseboard( BaseboardNum ).AirInletHumRat = Node( ZoneNode ).HumRat;

	}

	void
	SizeBaseboard( int const BaseboardNum )
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Fred Buhl
		//       DATE WRITTEN   February 2002
		//       MODIFIED       August 2013 Daeho Kang, add component sizing table entries
		//                      July 2014, B.Nigusse, added scalable sizing
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine is for sizing hot water baseboard components for which flow rates and UAs have not been
		// specified in the input.

		// METHODOLOGY EMPLOYED:
		// Obtains flow rates from the zone sizing arrays and plant sizing data. UAs are
		// calculated by numerically inverting the baseboard calculation routine.

		// REFERENCES:
		// na

		// Using/Aliasing
		using namespace DataSizing;
		using General::SolveRegulaFalsi;
		using General::RoundSigDigits;
		using PlantUtilities::RegisterPlantCompDesignFlow;
		using ReportSizingManager::ReportSizingOutput;
		using ReportSizingManager::RequestSizing;
		using DataLoopNode::Node;
		using DataHVACGlobals::HeatingCapacitySizing;
		using DataHeatBalance::Zone;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		Real64 const Acc( 0.0001 ); // Accuracy of result
		int const MaxIte( 500 ); // Maximum number of iterations
		static std::string const RoutineName( cCMO_BBRadiator_Water + ":SizeBaseboard" );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int WaterInletNode;
		int PltSizHeatNum( 0 ); // index of plant sizing object for 1st heating loop
		Real64 DesCoilLoad( 0.0 );
		int SolFla; // Flag of solver
		Real64 UA0; // lower bound for UA
		Real64 UA1; // upper bound for UA
		Real64 UA;
		Array1D< Real64 > Par( 2 );
		bool ErrorsFound( false ); // If errors detected in input
		Real64 rho; // local fluid density
		Real64 Cp; // local fluid specific heat
		bool FlowAutoSize( false ); // Indicator to autosizing water volume flow
		bool UAAutoSize( false ); // Indicator to autosizing UA
		Real64 WaterVolFlowRateMaxDes( 0.0 ); // Design water volume flow for reproting
		Real64 WaterVolFlowRateMaxUser( 0.0 ); // User hard-sized volume flow for reporting
		Real64 UADes( 0.0 ); // Design UA value for reproting
		Real64 UAUser( 0.0 ); // User hard-sized value for reporting
		std::string CompName; // component name
		std::string CompType; // component type
		std::string SizingString; // input field sizing description (e.g., Nominal Capacity)
		Real64 TempSize; // autosized value of coil input field
		int FieldNum = 1; // IDD numeric field number where input field description is found
		int SizingMethod; // Integer representation of sizing method name (HeatingCapacitySizing)
		bool PrintFlag; // TRUE when sizing information is reported in the eio file
		int CapSizingMethod( 0 ); // capacity sizing methods (HeatingDesignCapacity, CapacityPerFloorArea, and FractionOfAutosizedHeatingCapacity )

		// find the appropriate heating Plant Sizing object
		PltSizHeatNum = PlantLoop( Baseboard( BaseboardNum ).LoopNum ).PlantSizNum;

		if ( PltSizHeatNum > 0 ) {

			if ( CurZoneEqNum > 0 ) {

				if ( Baseboard( BaseboardNum ).WaterVolFlowRateMax == AutoSize ) {
					FlowAutoSize = true;
				}
				if ( ! FlowAutoSize && ! ZoneSizingRunDone ) { // Simulation should continue
					if ( Baseboard( BaseboardNum ).WaterVolFlowRateMax > 0.0 ) {
						ReportSizingOutput( cCMO_BBRadiator_Water, Baseboard( BaseboardNum ).EquipID, "User-Specified Maximum Water Flow Rate [m3/s]", Baseboard( BaseboardNum ).WaterVolFlowRateMax );
					}
				} else {
					CheckZoneSizing( cCMO_BBRadiator_Water, Baseboard( BaseboardNum ).EquipID );
					CompType = cCMO_BBRadiator_Water;
					CompName = Baseboard(BaseboardNum).EquipID;
					DataFracOfAutosizedHeatingCapacity = 1.0;
					DataZoneNumber = Baseboard(BaseboardNum).ZonePtr;
					SizingMethod = HeatingCapacitySizing;
					FieldNum = 1;
					PrintFlag = false;
					SizingString = BaseboardParamsNumericFields( BaseboardNum ).FieldNames(FieldNum) + " [W]";
					CapSizingMethod = Baseboard( BaseboardNum ).HeatingCapMethod;
					ZoneEqSizing(CurZoneEqNum).SizingMethod(SizingMethod) = CapSizingMethod;
					if ( CapSizingMethod == HeatingDesignCapacity || CapSizingMethod == CapacityPerFloorArea || CapSizingMethod == FractionOfAutosizedHeatingCapacity ) {

						if ( CapSizingMethod == HeatingDesignCapacity ) {
							if ( Baseboard( BaseboardNum ).ScaledHeatingCapacity == AutoSize ) {
								CheckZoneSizing(CompType, CompName);
								ZoneEqSizing(CurZoneEqNum).DesHeatingLoad = CalcFinalZoneSizing( CurZoneEqNum ).DesHeatLoad * CalcFinalZoneSizing( CurZoneEqNum ).HeatSizingFactor;
							} else {
								ZoneEqSizing( CurZoneEqNum ).DesHeatingLoad = Baseboard( BaseboardNum ).ScaledHeatingCapacity;
							}
							ZoneEqSizing( CurZoneEqNum ).HeatingCapacity = true;
							TempSize = ZoneEqSizing( CurZoneEqNum ).DesHeatingLoad;
						} else if ( CapSizingMethod == CapacityPerFloorArea ) {
							ZoneEqSizing( CurZoneEqNum ).HeatingCapacity = true;
							ZoneEqSizing( CurZoneEqNum ).DesHeatingLoad = Baseboard( BaseboardNum ).ScaledHeatingCapacity * Zone( DataZoneNumber ).FloorArea;
							TempSize = ZoneEqSizing( CurZoneEqNum ).DesHeatingLoad;
							DataScalableCapSizingON = true;
						} else if ( CapSizingMethod == FractionOfAutosizedHeatingCapacity ) {
							CheckZoneSizing(CompType, CompName);
							ZoneEqSizing( CurZoneEqNum ).HeatingCapacity = true;
							DataFracOfAutosizedHeatingCapacity = Baseboard( BaseboardNum ).ScaledHeatingCapacity;
							ZoneEqSizing( CurZoneEqNum ).DesHeatingLoad = CalcFinalZoneSizing( CurZoneEqNum ).DesHeatLoad * CalcFinalZoneSizing( CurZoneEqNum ).HeatSizingFactor;
							TempSize = AutoSize;
							DataScalableCapSizingON = true;
						} else {
							TempSize = Baseboard( BaseboardNum ).ScaledHeatingCapacity;
						}
						RequestSizing(CompType, CompName, SizingMethod, SizingString, TempSize, PrintFlag, RoutineName);
						DesCoilLoad = TempSize;
					} else {
						DesCoilLoad = 0.0;
					}

					if ( DesCoilLoad >= SmallLoad ) {
						Cp = GetSpecificHeatGlycol( PlantLoop( Baseboard( BaseboardNum ).LoopNum ).FluidName, 60.0, PlantLoop( Baseboard( BaseboardNum ).LoopNum ).FluidIndex, RoutineName );
						rho = GetDensityGlycol( PlantLoop( Baseboard( BaseboardNum ).LoopNum ).FluidName, InitConvTemp, PlantLoop( Baseboard( BaseboardNum ).LoopNum ).FluidIndex, RoutineName );
						WaterVolFlowRateMaxDes = DesCoilLoad / ( PlantSizData( PltSizHeatNum ).DeltaT * Cp * rho );
					} else {
						WaterVolFlowRateMaxDes = 0.0;
					}

					if ( FlowAutoSize ) {
						Baseboard( BaseboardNum ).WaterVolFlowRateMax = WaterVolFlowRateMaxDes;
						ReportSizingOutput( cCMO_BBRadiator_Water, Baseboard( BaseboardNum ).EquipID, "Design Size Maximum Water Flow Rate [m3/s]", WaterVolFlowRateMaxDes );
					} else { // hard-sized with sizing data
						if ( Baseboard( BaseboardNum ).WaterVolFlowRateMax > 0.0 && WaterVolFlowRateMaxDes > 0.0 ) {
							WaterVolFlowRateMaxUser = Baseboard( BaseboardNum ).WaterVolFlowRateMax;
							ReportSizingOutput( cCMO_BBRadiator_Water, Baseboard( BaseboardNum ).EquipID, "Design Size Maximum Water Flow Rate [m3/s]", WaterVolFlowRateMaxDes, "User-Specified Maximum Water Flow Rate [m3/s]", WaterVolFlowRateMaxUser );
							// Report a warning to note difference between the two
							if ( DisplayExtraWarnings ) {
								if ( ( std::abs( WaterVolFlowRateMaxDes - WaterVolFlowRateMaxUser ) / WaterVolFlowRateMaxUser ) > AutoVsHardSizingThreshold ) {
									ShowMessage( "SizeBaseboard: Potential issue with equipment sizing for ZoneHVAC:Baseboard:Convective:Water=\"" + Baseboard( BaseboardNum ).EquipID + "\"." );
									ShowContinueError( "User-Specified Maximum Water Flow Rate of " + RoundSigDigits( WaterVolFlowRateMaxUser, 5 ) + " [m3/s]" );
									ShowContinueError( "differs from Design Size Maximum Water Flow Rate of " + RoundSigDigits( WaterVolFlowRateMaxDes, 5 ) + " [m3/s]" );
									ShowContinueError( "This may, or may not, indicate mismatched component sizes." );
									ShowContinueError( "Verify that the value entered is intended and is consistent with other components." );
								}
							}
						}
					}
				}

				// UA sizing
				// Set hard-sized values to the local variable to correct a false indication aftet SolFla function calculation
				if ( Baseboard( BaseboardNum ).UA == AutoSize ) {
					UAAutoSize = true;
				} else {
					UAUser = Baseboard( BaseboardNum ).UA;
				}
				if ( ! UAAutoSize && ! ZoneSizingRunDone ) { // Simulation should continue
					if ( Baseboard( BaseboardNum ).UA > 0.0 ) {
						ReportSizingOutput( cCMO_BBRadiator_Water, Baseboard( BaseboardNum ).EquipID, "User-Specified U-Factor Times Area Value [W/K]", Baseboard( BaseboardNum ).UA );
					}
				} else {
					//CALL CheckZoneSizing(cCMO_BBRadiator_Water,Baseboard(BaseboardNum)%EquipID)
					Baseboard( BaseboardNum ).WaterInletTemp = PlantSizData( PltSizHeatNum ).ExitTemp;
					Baseboard( BaseboardNum ).AirInletTemp = FinalZoneSizing( CurZoneEqNum ).ZoneTempAtHeatPeak;
					Baseboard( BaseboardNum ).AirInletHumRat = FinalZoneSizing( CurZoneEqNum ).ZoneHumRatAtHeatPeak;
					WaterInletNode = Baseboard( BaseboardNum ).WaterInletNode;
					rho = GetDensityGlycol( PlantLoop( Baseboard( BaseboardNum ).LoopNum ).FluidName, InitConvTemp, PlantLoop( Baseboard( BaseboardNum ).LoopNum ).FluidIndex, RoutineName );
					Node( WaterInletNode ).MassFlowRate = rho * Baseboard( BaseboardNum ).WaterVolFlowRateMax;

					CompType = cCMO_BBRadiator_Water;
					CompName = Baseboard( BaseboardNum ).EquipID;
					DataFracOfAutosizedHeatingCapacity = 1.0;
					DataZoneNumber = Baseboard( BaseboardNum ).ZonePtr;
					SizingMethod = HeatingCapacitySizing;
					FieldNum = 1;
					PrintFlag = false;
					SizingString = BaseboardParamsNumericFields( BaseboardNum ).FieldNames(FieldNum) + " [W]";
					CapSizingMethod = Baseboard( BaseboardNum ).HeatingCapMethod;
					ZoneEqSizing(CurZoneEqNum).SizingMethod(SizingMethod) = CapSizingMethod;
					if ( CapSizingMethod == HeatingDesignCapacity || CapSizingMethod == CapacityPerFloorArea || CapSizingMethod == FractionOfAutosizedHeatingCapacity ) {
						if ( CapSizingMethod == HeatingDesignCapacity ) {
							if ( Baseboard( BaseboardNum ).ScaledHeatingCapacity == AutoSize ) {
								CheckZoneSizing(CompType, CompName);
								ZoneEqSizing( CurZoneEqNum ).DesHeatingLoad = CalcFinalZoneSizing( CurZoneEqNum ).DesHeatLoad * CalcFinalZoneSizing( CurZoneEqNum ).HeatSizingFactor;
							} else {
								ZoneEqSizing( CurZoneEqNum ).DesHeatingLoad = Baseboard( BaseboardNum ).ScaledHeatingCapacity;;
							}
							ZoneEqSizing( CurZoneEqNum ).HeatingCapacity = true;
							TempSize = ZoneEqSizing( CurZoneEqNum ).DesHeatingLoad;
						} else if ( CapSizingMethod == CapacityPerFloorArea ) {
							ZoneEqSizing( CurZoneEqNum ).HeatingCapacity = true;
							ZoneEqSizing( CurZoneEqNum ).DesHeatingLoad = Baseboard( BaseboardNum ).ScaledHeatingCapacity * Zone( DataZoneNumber ).FloorArea;
							TempSize = ZoneEqSizing( CurZoneEqNum ).DesHeatingLoad;
							DataScalableCapSizingON = true;
						} else if (CapSizingMethod == FractionOfAutosizedHeatingCapacity) {
							CheckZoneSizing(CompType, CompName);
							ZoneEqSizing( CurZoneEqNum ).HeatingCapacity = true;
							DataFracOfAutosizedHeatingCapacity = Baseboard(BaseboardNum).ScaledHeatingCapacity;
							ZoneEqSizing( CurZoneEqNum ).DesHeatingLoad = CalcFinalZoneSizing( CurZoneEqNum ).DesHeatLoad * CalcFinalZoneSizing( CurZoneEqNum ).HeatSizingFactor;
							TempSize = AutoSize;
							DataScalableCapSizingON = true;
						} else {
							TempSize = Baseboard( BaseboardNum ).ScaledHeatingCapacity;
						}
						RequestSizing(CompType, CompName, SizingMethod, SizingString, TempSize, PrintFlag, RoutineName);
						DesCoilLoad = TempSize;
					} else {
						DesCoilLoad = 0.0; // CalcFinalZoneSizing(CurZoneEqNum).DesHeatLoad * CalcFinalZoneSizing(CurZoneEqNum).HeatSizingFactor;
					}
					if ( DesCoilLoad >= SmallLoad ) {
						// pick an air  mass flow rate that is twice the water mass flow rate (CR8842)
						Baseboard( BaseboardNum ).DesAirMassFlowRate = 2.0 * rho * Baseboard( BaseboardNum ).WaterVolFlowRateMax;
						// pass along the coil number and the design load to the residual calculation
						Par( 1 ) = DesCoilLoad;
						Par( 2 ) = BaseboardNum;
						// set the lower and upper limits on the UA
						UA0 = 0.001 * DesCoilLoad;
						UA1 = DesCoilLoad;
						// Invert the baseboard model: given the design inlet conditions and the design load,
						// find the design UA.
						SolveRegulaFalsi( Acc, MaxIte, SolFla, UA, HWBaseboardUAResidual, UA0, UA1, Par );
						// if the numerical inversion failed, issue error messages.
						if ( SolFla == -1 ) {
							ShowSevereError( "SizeBaseboard: Autosizing of HW baseboard UA failed for " + cCMO_BBRadiator_Water + "=\"" + Baseboard( BaseboardNum ).EquipID + "\"" );
							ShowContinueError( "Iteration limit exceeded in calculating coil UA" );
							if ( UAAutoSize ) {
								ErrorsFound = true;
							} else {
								ShowContinueError( "Could not calculate design value for comparison to user value, and the simulation continues" );
								UA = 0.0;
							}
						} else if ( SolFla == -2 ) {
							ShowSevereError( "SizeBaseboard: Autosizing of HW baseboard UA failed for " + cCMO_BBRadiator_Water + "=\"" + Baseboard( BaseboardNum ).EquipID + "\"" );
							ShowContinueError( "Bad starting values for UA" );
							if ( UAAutoSize ) {
								ErrorsFound = true;
							} else {
								ShowContinueError( "Could not calculate design value for comparison to user value, and the simulation continues" );
								UA = 0.0;
							}
						}
						UADes = UA; //Baseboard(BaseboardNum)%UA = UA
					} else {
						UADes = 0.0;
					}

					if ( UAAutoSize ) {
						Baseboard( BaseboardNum ).UA = UADes;
						ReportSizingOutput( cCMO_BBRadiator_Water, Baseboard( BaseboardNum ).EquipID, "Design Size U-Factor Times Area Value [W/K]", UADes );
					} else { // Hard-sized with sizing data
						Baseboard( BaseboardNum ).UA = UAUser; // need to put this back as HWBaseboardUAResidual will have reset it, CR9377
						if ( UAUser > 0.0 && UADes > 0.0 ) {
							ReportSizingOutput( cCMO_BBRadiator_Water, Baseboard( BaseboardNum ).EquipID, "Design Size U-Factor Times Area Value [W/K]", UADes, "User-Specified U-Factor Times Area Value [W/K]", UAUser );
							// Report difference between design size and hard-sized values
							if ( DisplayExtraWarnings ) {
								if ( ( std::abs( UADes - UAUser ) / UAUser ) > AutoVsHardSizingThreshold ) {
									ShowMessage( "SizeBaseboard: Potential issue with equipment sizing for ZoneHVAC:Baseboard:Convective:Water=\"" + Baseboard( BaseboardNum ).EquipID + "\"." );
									ShowContinueError( "User-Specified U-Factor Times Area Value of " + RoundSigDigits( UAUser, 2 ) + " [W/K]" );
									ShowContinueError( "differs from Design Size U-Factor Times Area Value of " + RoundSigDigits( UADes, 2 ) + " [W/K]" );
									ShowContinueError( "This may, or may not, indicate mismatched component sizes." );
									ShowContinueError( "Verify that the value entered is intended and is consistent with other components." );
								}
							}
						}
					}
				}
			}
		} else {
			// if there is no heating Sizing:Plant object and autosizng was requested, issue an error message
			if ( FlowAutoSize || UAAutoSize ) {
				ShowSevereError( "SizeBaseboard: " + cCMO_BBRadiator_Water + "=\"" + Baseboard( BaseboardNum ).EquipID + "\"" );
				ShowContinueError( "...Autosizing of hot water baseboard requires a heating loop Sizing:Plant object" );
				ErrorsFound = true;
			}
		}

		// save the design water flow rate for use by the water loop sizing algorithms
		RegisterPlantCompDesignFlow( Baseboard( BaseboardNum ).WaterInletNode, Baseboard( BaseboardNum ).WaterVolFlowRateMax );

		if ( ErrorsFound ) {
			ShowFatalError( "SizeBaseboard: Preceding sizing errors cause program termination" );
		}

	}

	void
	SimHWConvective(
		int & BaseboardNum,
		Real64 & LoadMet
	)
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         Russ Taylor
		//       DATE WRITTEN   Nov 1997
		//       MODIFIED       May 2000 Fred Buhl
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE: This subroutine calculates the heat exchange rate
		// in a pure convective baseboard heater.  The heater is assumed to be crossflow
		// with both fluids unmixed. The air flow is bouyancy driven and a constant air
		// flow velocity of 0.5m/s is assumed. The solution is by the effectiveness-NTU
		// method found in Icropera and DeWitt, Fundamentals of Heat and Mass Transfer,
		// Chapter 11.4, p. 523, eq. 11.33

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// Icropera and DeWitt, Fundamentals of Heat and Mass Transfer,
		// Chapter 11.4, p. 523, eq. 11.33

		// Using/Aliasing
		using DataLoopNode::Node;
		using namespace DataSizing;
		using DataZoneEnergyDemands::ZoneSysEnergyDemand;
		using DataZoneEnergyDemands::CurDeadBandOrSetback;
		using PlantUtilities::SetActuatedBranchFlowRate;
		using DataHVACGlobals::SmallLoad;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( cCMO_BBRadiator_Water + ":SimHWConvective" );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int ZoneNum;
		Real64 WaterInletTemp;
		Real64 AirInletTemp;
		Real64 CpAir;
		Real64 CpWater;
		Real64 AirMassFlowRate;
		Real64 WaterMassFlowRate;
		Real64 CapacitanceAir;
		Real64 CapacitanceWater;
		Real64 CapacitanceMax;
		Real64 CapacitanceMin;
		Real64 CapacityRatio;
		Real64 NTU;
		Real64 Effectiveness;
		Real64 WaterOutletTemp;
		Real64 AirOutletTemp;
		Real64 AA;
		Real64 BB;
		Real64 CC;
		Real64 QZnReq;

		ZoneNum = Baseboard( BaseboardNum ).ZonePtr;
		QZnReq = ZoneSysEnergyDemand( ZoneNum ).RemainingOutputReqToHeatSP;
		if ( MySizeFlag( BaseboardNum ) ) QZnReq = CalcFinalZoneSizing( CurZoneEqNum ).DesHeatLoad * CalcFinalZoneSizing( CurZoneEqNum ).HeatSizingFactor; // If in sizing, assign design condition

		WaterInletTemp = Baseboard( BaseboardNum ).WaterInletTemp;
		WaterOutletTemp = WaterInletTemp;
		AirInletTemp = Baseboard( BaseboardNum ).AirInletTemp;
		AirOutletTemp = AirInletTemp;

		CpWater = GetSpecificHeatGlycol( PlantLoop( Baseboard( BaseboardNum ).LoopNum ).FluidName, WaterInletTemp, PlantLoop( Baseboard( BaseboardNum ).LoopNum ).FluidIndex, RoutineName );
		CpAir = PsyCpAirFnWTdb( Baseboard( BaseboardNum ).AirInletHumRat, AirInletTemp );

		if ( Baseboard( BaseboardNum ).DesAirMassFlowRate > 0.0 ) { // If UA is autosized, assign design condition
			AirMassFlowRate = Baseboard( BaseboardNum ).DesAirMassFlowRate;
		} else {
			AirMassFlowRate = Baseboard( BaseboardNum ).AirMassFlowRate;
			// pick a mass flow rate that depends on the max water mass flow rate. CR 8842 changed to factor of 2.0
			if ( AirMassFlowRate <= 0.0 ) AirMassFlowRate = 2.0 * Baseboard( BaseboardNum ).WaterMassFlowRateMax;
		}

		WaterMassFlowRate = Node( Baseboard( BaseboardNum ).WaterInletNode ).MassFlowRate;
		CapacitanceAir = CpAir * AirMassFlowRate;

		if ( QZnReq > SmallLoad && ( ! CurDeadBandOrSetback( ZoneNum ) || MySizeFlag( BaseboardNum ) ) && ( GetCurrentScheduleValue( Baseboard( BaseboardNum ).SchedPtr ) > 0 || MySizeFlag( BaseboardNum ) ) && ( WaterMassFlowRate > 0.0 ) ) {
			CapacitanceWater = CpWater * WaterMassFlowRate;
			CapacitanceMax = max( CapacitanceAir, CapacitanceWater );
			CapacitanceMin = min( CapacitanceAir, CapacitanceWater );
			CapacityRatio = CapacitanceMin / CapacitanceMax;
			NTU = Baseboard( BaseboardNum ).UA / CapacitanceMin;
			// The effectiveness is given by the following formula:
			// Effectiveness = 1. - EXP((1./CapacityRatio)*(NTU)**0.22*(EXP(-CapacityRatio*(NTU)**0.78)-1.))
			// To prevent possible underflows (numbers smaller than the computer can handle) we must break
			// the calculation up into steps and check the size of the exponential arguments.
			AA = -CapacityRatio * std::pow( NTU, 0.78 );
			if ( AA < EXP_LowerLimit ) {
				BB = 0.0;
			} else {
				BB = std::exp( AA );
			}
			CC = ( 1.0 / CapacityRatio ) * std::pow( NTU, 0.22 ) * ( BB - 1.0 );
			if ( CC < EXP_LowerLimit ) {
				Effectiveness = 1.0;
			} else {
				Effectiveness = 1.0 - std::exp( CC );
			}
			AirOutletTemp = AirInletTemp + Effectiveness * CapacitanceMin * ( WaterInletTemp - AirInletTemp ) / CapacitanceAir;
			WaterOutletTemp = WaterInletTemp - CapacitanceAir * ( AirOutletTemp - AirInletTemp ) / CapacitanceWater;
			LoadMet = CapacitanceWater * ( WaterInletTemp - WaterOutletTemp );
			Baseboard( BaseboardNum ).WaterOutletEnthalpy = Baseboard( BaseboardNum ).WaterInletEnthalpy - LoadMet / WaterMassFlowRate;
		} else {
			CapacitanceWater = 0.0;
			CapacitanceMax = CapacitanceAir;
			CapacitanceMin = 0.0;
			NTU = 0.0;
			Effectiveness = 0.0;
			AirOutletTemp = AirInletTemp;
			WaterOutletTemp = WaterInletTemp;
			LoadMet = 0.0;
			Baseboard( BaseboardNum ).WaterOutletEnthalpy = Baseboard( BaseboardNum ).WaterInletEnthalpy;
			WaterMassFlowRate = 0.0;

			SetActuatedBranchFlowRate( WaterMassFlowRate, Baseboard( BaseboardNum ).WaterInletNode, Baseboard( BaseboardNum ).LoopNum, Baseboard( BaseboardNum ).LoopSideNum, Baseboard( BaseboardNum ).BranchNum, false );
			AirMassFlowRate = 0.0;
		}

		Baseboard( BaseboardNum ).WaterOutletTemp = WaterOutletTemp;
		Baseboard( BaseboardNum ).AirOutletTemp = AirOutletTemp;
		Baseboard( BaseboardNum ).Power = LoadMet;
		Baseboard( BaseboardNum ).WaterMassFlowRate = WaterMassFlowRate;
		Baseboard( BaseboardNum ).AirMassFlowRate = AirMassFlowRate;

	}

	void
	UpdateBaseboard( int & BaseboardNum )
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Russ Taylor
		//       DATE WRITTEN   Nov 1997
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE: This subroutine

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataLoopNode::Node;
		using PlantUtilities::SafeCopyPlantNode;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int WaterInletNode;
		int WaterOutletNode;

		WaterInletNode = Baseboard( BaseboardNum ).WaterInletNode;
		WaterOutletNode = Baseboard( BaseboardNum ).WaterOutletNode;

		SafeCopyPlantNode( WaterInletNode, WaterOutletNode );
		// Set the outlet air nodes of the Baseboard
		// Set the outlet water nodes for the Coil
		//   Node(WaterOutletNode)%MassFlowRate = Baseboard(BaseboardNum)%WaterMassFlowRate
		Node( WaterOutletNode ).Temp = Baseboard( BaseboardNum ).WaterOutletTemp;
		Node( WaterOutletNode ).Enthalpy = Baseboard( BaseboardNum ).WaterOutletEnthalpy;

		// Set the outlet nodes for properties that just pass through & not used

		// Set the outlet nodes for properties that just pass through & not used
		//   Node(WaterOutletNode)%Quality             = Node(WaterInletNode)%Quality
		//   Node(WaterOutletNode)%Press               = Node(WaterInletNode)%Press
		//   Node(WaterOutletNode)%HumRat              = Node(WaterInletNode)%HumRat
		//   Node(WaterOutletNode)%MassFlowRateMin     = Node(WaterInletNode)%MassFlowRateMin
		//   Node(WaterOutletNode)%MassFlowRateMax     = Node(WaterInletNode)%MassFlowRateMax
		//   Node(WaterOutletNode)%MassFlowRateMinAvail= Node(WaterInletNode)%MassFlowRateMinAvail
		//   Node(WaterOutletNode)%MassFlowRateMaxAvail= Node(WaterInletNode)%MassFlowRateMaxAvail

	}

	void
	ReportBaseboard( int const BaseboardNum )
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Russ Taylor
		//       DATE WRITTEN   Nov 1997
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE: This subroutine

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
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
		// na

		Baseboard( BaseboardNum ).Energy = Baseboard( BaseboardNum ).Power * TimeStepSys * SecInHour;

	}

	Real64
	HWBaseboardUAResidual(
		Real64 const UA, // UA of coil
		Array1< Real64 > const & Par // par(1) = design coil load [W]
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Fred Buhl
		//       DATE WRITTEN   February 2002
		//       MODIFIED
		//       RE-ENGINEERED

		// PURPOSE OF THIS FUNCTION:
		// Calculates residual function (Design Coil Load - Coil Heating Output) / Design Coil Load.
		// Coil Heating Output depends on the UA which is being varied to zero the residual.

		// METHODOLOGY EMPLOYED:
		// Puts UA into the baseboard data structure, calls SimHWConvective, and calculates
		// the residual as defined above.

		// REFERENCES:

		// USE STATEMENTS:
		// na

		// Return value
		Real64 Residuum; // residual to be minimized to zero

		// Argument array dimensioning

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		int BaseboardIndex;
		Real64 LoadMet;

		BaseboardIndex = int( Par( 2 ) );
		Baseboard( BaseboardIndex ).UA = UA;
		SimHWConvective( BaseboardIndex, LoadMet );
		Residuum = ( Par( 1 ) - LoadMet ) / Par( 1 );

		return Residuum;
	}

	void
	UpdateBaseboardPlantConnection(
		int const BaseboardTypeNum, // type index
		std::string const & BaseboardName, // component name
		int const EP_UNUSED( EquipFlowCtrl ), // Flow control mode for the equipment
		int const EP_UNUSED( LoopNum ), // Plant loop index for where called from
		int const EP_UNUSED( LoopSide ), // Plant loop side index for where called from
		int & CompIndex, // Chiller number pointer
		bool const EP_UNUSED( FirstHVACIteration ),
		bool & InitLoopEquip // If not zero, calculate the max load for operating conditions
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Chandan Sharma, FSEC
		//       DATE WRITTEN   Sept. 2010
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// update sim routine called from plant

		// METHODOLOGY EMPLOYED:
		// check input, provide comp index, call utility routines

		// REFERENCES:
		// Based on UpdateBaseboardPlantConnection from Brent Griffith, Sept 2010

		// Using/Aliasing
		using PlantUtilities::PullCompInterconnectTrigger;
		using DataPlant::ccSimPlantEquipTypes;
		using DataPlant::TypeOf_Baseboard_Conv_Water;
		using DataPlant::CriteriaType_MassFlowRate;
		using DataPlant::CriteriaType_Temperature;
		using DataPlant::CriteriaType_HeatTransferRate;
		using InputProcessor::FindItemInList;
		using General::TrimSigDigits;
		using DataGlobals::KickOffSimulation;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		int BaseboardNum;

		// Find the correct baseboard
		if ( CompIndex == 0 ) {
			BaseboardNum = FindItemInList( BaseboardName, Baseboard, &BaseboardParams::EquipID );
			if ( BaseboardNum == 0 ) {
				ShowFatalError( "UpdateBaseboardPlantConnection: Invalid Unit Specified " + cCMO_BBRadiator_Water + "=\"" + BaseboardName + "\"" );
			}
			CompIndex = BaseboardNum;
		} else {
			BaseboardNum = CompIndex;
			if ( BaseboardNum > NumBaseboards || BaseboardNum < 1 ) {
				ShowFatalError( "UpdateBaseboardPlantConnection:  Invalid CompIndex passed=" + TrimSigDigits( BaseboardNum ) + ", Number of baseboards=" + TrimSigDigits( NumBaseboards ) + ", Entered baseboard name=" + BaseboardName );
			}
			if ( KickOffSimulation ) {
				if ( BaseboardName != Baseboard( BaseboardNum ).EquipID ) {
					ShowFatalError( "UpdateBaseboardPlantConnection: Invalid CompIndex passed=" + TrimSigDigits( BaseboardNum ) + ", baseboard name=" + BaseboardName + ", stored baseboard Name for that index=" + Baseboard( BaseboardNum ).EquipID );
				}
				if ( BaseboardTypeNum != TypeOf_Baseboard_Conv_Water ) {
					ShowFatalError( "UpdateBaseboardPlantConnection: Invalid CompIndex passed=" + TrimSigDigits( BaseboardNum ) + ", baseboard name=" + BaseboardName + ", stored baseboard Name for that index=" + ccSimPlantEquipTypes( BaseboardTypeNum ) );
				}
			}
		}

		if ( InitLoopEquip ) {
			return;
		}

		PullCompInterconnectTrigger( Baseboard( BaseboardNum ).LoopNum, Baseboard( BaseboardNum ).LoopSideNum, Baseboard( BaseboardNum ).BranchNum, Baseboard( BaseboardNum ).CompNum, Baseboard( BaseboardNum ).BBLoadReSimIndex, Baseboard( BaseboardNum ).LoopNum, Baseboard( BaseboardNum ).LoopSideNum, CriteriaType_HeatTransferRate, Baseboard( BaseboardNum ).Power );

		PullCompInterconnectTrigger( Baseboard( BaseboardNum ).LoopNum, Baseboard( BaseboardNum ).LoopSideNum, Baseboard( BaseboardNum ).BranchNum, Baseboard( BaseboardNum ).CompNum, Baseboard( BaseboardNum ).BBLoadReSimIndex, Baseboard( BaseboardNum ).LoopNum, Baseboard( BaseboardNum ).LoopSideNum, CriteriaType_MassFlowRate, Baseboard( BaseboardNum ).WaterMassFlowRate );

		PullCompInterconnectTrigger( Baseboard( BaseboardNum ).LoopNum, Baseboard( BaseboardNum ).LoopSideNum, Baseboard( BaseboardNum ).BranchNum, Baseboard( BaseboardNum ).CompNum, Baseboard( BaseboardNum ).BBLoadReSimIndex, Baseboard( BaseboardNum ).LoopNum, Baseboard( BaseboardNum ).LoopSideNum, CriteriaType_Temperature, Baseboard( BaseboardNum ).WaterOutletTemp );

	}

} // BaseboardRadiator

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//******************************************************************************************************
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//******************************************************************************************************
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


} // EnergyPlus
