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

// EnergyPlus Headers
#include <SteamBaseboardRadiator.hh>
#include <BranchNodeConnections.hh>
#include <DataBranchAirLoopPlant.hh>
#include <DataEnvironment.hh>
#include <DataHeatBalance.hh>
#include <DataHeatBalFanSys.hh>
#include <DataHeatBalSurface.hh>
#include <DataHVACGlobals.hh>
#include <DataIPShortCuts.hh>
#include <DataLoopNode.hh>
#include <DataPlant.hh>
#include <DataPrecisionGlobals.hh>
#include <DataSizing.hh>
#include <DataSurfaces.hh>
#include <DataZoneEnergyDemands.hh>
#include <DataZoneEquipment.hh>
#include <FluidProperties.hh>
#include <General.hh>
#include <GeneralRoutines.hh>
#include <GlobalNames.hh>
#include <HeatBalanceSurfaceManager.hh>
#include <InputProcessor.hh>
#include <NodeInputManager.hh>
#include <OutputProcessor.hh>
#include <PlantUtilities.hh>
#include <Psychrometrics.hh>
#include <ReportSizingManager.hh>
#include <ScheduleManager.hh>
#include <UtilityRoutines.hh>

namespace EnergyPlus {

namespace SteamBaseboardRadiator {

	// Module -- (ref: Object: ZoneHVAC:Baseboard:RadiantConvective:Steam)

	// Module containing the routines dealing with the steam baseboard heaters

	// MODULE INFORMATION:
	//       AUTHOR         Daeho Kang
	//       DATE WRITTEN   September 2009
	//       MODIFIED       na
	//       RE-ENGINEERED  na

	// PURPOSE OF THIS MODULE:
	// The purpose of this module is to simulate steam baseboard heaters.

	// METHODOLOGY EMPLOYED:

	// REFERENCES:
	// 1. HWBaseboardRadiator module (ZoneHVAC:Baseboard:RadiantConvective:Water)
	// 2. SteamCoils module (Coil:Heating:Steam)

	// OTHER NOTES:
	// na

	// USE STATEMENTS:
	// Use statements for data only modules
	// Using/Aliasing
	using namespace DataPrecisionGlobals;
	using namespace DataGlobals;
	using DataZoneEquipment::ZoneEquipInputsFilled;
	using DataZoneEquipment::CheckZoneEquipmentList;
	using DataZoneEquipment::ZoneEquipConfig;
	using DataHVACGlobals::SmallLoad;
	using DataHVACGlobals::TimeStepSys;
	using DataHVACGlobals::SysTimeElapsed;
	using DataLoopNode::Node;
	using DataLoopNode::NodeType_Steam;
	using DataLoopNode::NodeConnectionType_Inlet;
	using DataLoopNode::NodeConnectionType_Outlet;
	using DataLoopNode::ObjectIsNotParent;
	using DataPlant::PlantLoop;
	using DataPlant::TypeOf_Baseboard_Rad_Conv_Steam;

	// Use statements for access to subroutines in other modules
	using Psychrometrics::PsyCpAirFnWTdb;
	using Psychrometrics::PsyRhoAirFnPbTdbW;

	// Data
	//MODULE PARAMETER DEFINITIONS
	std::string const cCMO_BBRadiator_Steam( "ZoneHVAC:Baseboard:RadiantConvective:Steam" );
	static std::string const fluidNameSteam( "STEAM" );

	// DERIVED TYPE DEFINITIONS

	//MODULE VARIABLE DECLARATIONS:
	int NumSteamBaseboards( 0 );
	int SteamIndex( 0 );

	Array1D< Real64 > QBBSteamRadSource; // Need to keep the last value in case we are still iterating
	Array1D< Real64 > QBBSteamRadSrcAvg; // Need to keep the last value in case we are still iterating
	Array1D< Real64 > ZeroSourceSumHATsurf; // Equal to the SumHATsurf for all the walls in a zone
	// with no source

	// Record keeping variables used to calculate QBBRadSrcAvg locally
	Array1D< Real64 > LastQBBSteamRadSrc; // Need to keep the last value in case we are still iterating
	Array1D< Real64 > LastSysTimeElapsed; // Need to keep the last value in case we are still iterating
	Array1D< Real64 > LastTimeStepSys; // Need to keep the last value in case we are still iterating
	Array1D_bool MySizeFlag;
	Array1D_bool CheckEquipName;
	Array1D_bool SetLoopIndexFlag; // get loop number flag

	//SUBROUTINE SPECIFICATIONS FOR MODULE BaseboardRadiator

	// Object Data
	Array1D< SteamBaseboardParams > SteamBaseboard;
	Array1D< SteamBaseboardNumericFieldData > SteamBaseboardNumericFields;

	// Functions

	void
	SimSteamBaseboard(
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
		//       MODIFIED
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine simulates the steam baseboards or radiators.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using InputProcessor::FindItemInList;
		using General::TrimSigDigits;
		using ScheduleManager::GetCurrentScheduleValue;
		using DataZoneEnergyDemands::ZoneSysEnergyDemand;
		using DataZoneEnergyDemands::CurDeadBandOrSetback;
		using PlantUtilities::SetComponentFlowRate;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:

		// INTERFACE BLOCK SPECIFICATIONS

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int BaseboardNum; // index of unit in baseboard array
		static bool GetInputFlag( true ); // one time get input flag
		Real64 QZnReq; // zone load not yet satisfied
		Real64 MaxSteamFlow;
		Real64 MinSteamFlow;
		static Real64 mdot( 0.0 );

		if ( GetInputFlag ) {
			GetSteamBaseboardInput();
			GetInputFlag = false;
		}

		// Find the correct Baseboard Equipment
		if ( CompIndex == 0 ) {
			BaseboardNum = FindItemInList( EquipName, SteamBaseboard, &SteamBaseboardParams::EquipID );
			if ( BaseboardNum == 0 ) {
				ShowFatalError( "SimSteamBaseboard: Unit not found=" + EquipName );
			}
			CompIndex = BaseboardNum;
		} else {
			BaseboardNum = CompIndex;
			if ( BaseboardNum > NumSteamBaseboards || BaseboardNum < 1 ) {
				ShowFatalError( "SimSteamBaseboard:  Invalid CompIndex passed=" + TrimSigDigits( BaseboardNum ) + ", Number of Units=" + TrimSigDigits( NumSteamBaseboards ) + ", Entered Unit name=" + EquipName );
			}
			if ( CheckEquipName( BaseboardNum ) ) {
				if ( EquipName != SteamBaseboard( BaseboardNum ).EquipID ) {
					ShowFatalError( "SimSteamBaseboard: Invalid CompIndex passed=" + TrimSigDigits( BaseboardNum ) + ", Unit name=" + EquipName + ", stored Unit Name for that index=" + SteamBaseboard( BaseboardNum ).EquipID );
				}
				CheckEquipName( BaseboardNum ) = false;
			}
		}

		if ( CompIndex > 0 ) {

			InitSteamBaseboard( BaseboardNum, ControlledZoneNum, FirstHVACIteration );

			QZnReq = ZoneSysEnergyDemand( ActualZoneNum ).RemainingOutputReqToHeatSP;

			if ( QZnReq > SmallLoad && ! CurDeadBandOrSetback( ActualZoneNum ) && ( GetCurrentScheduleValue( SteamBaseboard( BaseboardNum ).SchedPtr ) > 0.0 ) ) {

				// On the first HVAC iteration the system values are given to the controller, but after that
				// the demand limits are in place and there needs to be feedback to the Zone Equipment
				if ( FirstHVACIteration ) {
					MaxSteamFlow = SteamBaseboard( BaseboardNum ).SteamMassFlowRateMax;
					MinSteamFlow = 0.0;
				} else {
					MaxSteamFlow = Node( SteamBaseboard( BaseboardNum ).SteamInletNode ).MassFlowRateMaxAvail;
					MinSteamFlow = Node( SteamBaseboard( BaseboardNum ).SteamInletNode ).MassFlowRateMinAvail;
				}

				{ auto const SELECT_CASE_var( SteamBaseboard( BaseboardNum ).EquipType );

				if ( SELECT_CASE_var == TypeOf_Baseboard_Rad_Conv_Steam ) { // 'ZoneHVAC:Baseboard:RadiantConvective:Steam'
					ControlCompOutput( SteamBaseboard( BaseboardNum ).EquipID, cCMO_BBRadiator_Steam, BaseboardNum, FirstHVACIteration, QZnReq, SteamBaseboard( BaseboardNum ).SteamInletNode, MaxSteamFlow, MinSteamFlow, SteamBaseboard( BaseboardNum ).Offset, SteamBaseboard( BaseboardNum ).ControlCompTypeNum, SteamBaseboard( BaseboardNum ).CompErrIndex, _, _, _, _, _, SteamBaseboard( BaseboardNum ).LoopNum, SteamBaseboard( BaseboardNum ).LoopSideNum, SteamBaseboard( BaseboardNum ).BranchNum );
				} else {
					ShowSevereError( "SimSteamBaseboard: Errors in Baseboard=" + SteamBaseboard( BaseboardNum ).EquipID );
					ShowContinueError( "Invalid or unimplemented equipment type=" + TrimSigDigits( SteamBaseboard( BaseboardNum ).EquipType ) );
					ShowFatalError( "Preceding condition causes termination." );

				}}

				PowerMet = SteamBaseboard( BaseboardNum ).TotPower;
			} else {
				// baseboard is off, don't bother going into ControlCompOutput
				mdot = 0.0;
				SetComponentFlowRate( mdot, SteamBaseboard( BaseboardNum ).SteamInletNode, SteamBaseboard( BaseboardNum ).SteamOutletNode, SteamBaseboard( BaseboardNum ).LoopNum, SteamBaseboard( BaseboardNum ).LoopSideNum, SteamBaseboard( BaseboardNum ).BranchNum, SteamBaseboard( BaseboardNum ).CompNum );
				CalcSteamBaseboard( BaseboardNum, PowerMet );

			}

			UpdateSteamBaseboard( BaseboardNum );

			ReportSteamBaseboard( BaseboardNum );

		} else {
			ShowFatalError( "SimSteamBaseboard: Unit not found=" + EquipName );
		}

	}

	void
	GetSteamBaseboardInput()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Daeho Kang
		//       DATE WRITTEN   September 2009
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine gets the input for the baseboard units.

		// METHODOLOGY EMPLOYED:
		// Standard input processor calls.

		// REFERENCES:
		// HWBaseboardRadiator module

		// Using/Aliasing
		using InputProcessor::GetNumObjectsFound;
		using InputProcessor::GetObjectItem;
		using InputProcessor::FindItemInList;
		using InputProcessor::SameString;
		using InputProcessor::VerifyName;
		using NodeInputManager::GetOnlySingleNode;
		using BranchNodeConnections::TestCompSet;
		using DataSurfaces::Surface;
		using ScheduleManager::GetScheduleIndex;
		using ScheduleManager::GetCurrentScheduleValue;
		using GlobalNames::VerifyUniqueBaseboardName;
		using General::RoundSigDigits;
		using General::TrimSigDigits;
		using FluidProperties::FindRefrigerant;
		using namespace DataIPShortCuts;
		using namespace DataSizing;
		using ReportSizingManager::ReportSizingOutput;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "GetSteamBaseboardInput:" );
		Real64 const MaxFraction( 1.0 ); // Maximum limit of fractional values
		Real64 const MinFraction( 0.0 ); // Minimum limit of fractional values
		Real64 const MaxSteamFlowRate( 10.0 ); // Maximum limit of steam volume flow rate in m3/s
		Real64 const MinSteamFlowRate( 0.0 ); // Minimum limit of steam volume flow rate in m3/s
		//    INTEGER,PARAMETER :: MaxDistribSurfaces   = 20          ! Maximum number of surfaces that a baseboard heater can radiate to
		int const MinDistribSurfaces( 1 ); // Minimum number of surfaces that a baseboard heater can radiate to
		int const iHeatCAPMAlphaNum( 5 ); // get input index to steam baseboard Radiator system heating capacity sizing method
		int const iHeatDesignCapacityNumericNum( 1 ); // get input index to steam baseboard Radiator system electric heating capacity
		int const iHeatCapacityPerFloorAreaNumericNum( 2 ); // get input index to steam baseboard Radiator system electric heating capacity per floor area sizing
		int const iHeatFracOfAutosizedCapacityNumericNum( 3 ); //  get input index to steam baseboard Radiator system electric heating capacity sizing as fraction of autozized heating capacity

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 AllFracsSummed; // Sum of the fractions radiant
		int BaseboardNum; // Baseboard number
		int NumAlphas; // Number of Alphas for each GetobjectItem call
		int NumNumbers; // Number of Numbers for each GetobjectItem call
		int SurfNum; // Surface number Do loop counter
		int IOStat;
		static bool ErrorsFound( false ); // If errors detected in input
		bool IsNotOK; // Flag to verify name
		bool IsBlank; // Flag for blank name
		bool errFlag;
		bool SteamMessageNeeded;

		SteamMessageNeeded = true;
		NumSteamBaseboards = GetNumObjectsFound( cCMO_BBRadiator_Steam );

		// Count total number of baseboard units

		SteamBaseboard.allocate( NumSteamBaseboards );
		CheckEquipName.dimension( NumSteamBaseboards, true );
		SteamBaseboardNumericFields.allocate( NumSteamBaseboards );

		// Get the data from the user input related to baseboard heaters
		for ( BaseboardNum = 1; BaseboardNum <= NumSteamBaseboards; ++BaseboardNum ) {

			GetObjectItem( cCMO_BBRadiator_Steam, BaseboardNum, cAlphaArgs, NumAlphas, rNumericArgs, NumNumbers, IOStat, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );

			SteamBaseboardNumericFields( BaseboardNum ).FieldNames.allocate( NumNumbers );
			SteamBaseboardNumericFields( BaseboardNum ).FieldNames = "";
			SteamBaseboardNumericFields( BaseboardNum ).FieldNames = cNumericFieldNames;

			IsNotOK = false;
			IsBlank = false;
			VerifyName( cAlphaArgs( 1 ), SteamBaseboard, &SteamBaseboardParams::EquipID, BaseboardNum, IsNotOK, IsBlank, cCMO_BBRadiator_Steam + " Name" );

			if ( IsNotOK ) {
				ErrorsFound = true;
			}
			VerifyUniqueBaseboardName( cCMO_BBRadiator_Steam, cAlphaArgs( 1 ), errFlag, cCMO_BBRadiator_Steam + " Name" );
			if ( errFlag ) {
				ErrorsFound = true;
			}

			SteamBaseboard( BaseboardNum ).EquipID = cAlphaArgs( 1 ); // Name of the baseboard
			SteamBaseboard( BaseboardNum ).EquipType = TypeOf_Baseboard_Rad_Conv_Steam; //'ZoneHVAC:Baseboard:RadiantConvective:Steam'

			// Get schedule
			SteamBaseboard( BaseboardNum ).Schedule = cAlphaArgs( 2 );
			if ( lAlphaFieldBlanks( 2 ) ) {
				SteamBaseboard( BaseboardNum ).SchedPtr = ScheduleAlwaysOn;
			} else {
				SteamBaseboard( BaseboardNum ).SchedPtr = GetScheduleIndex( cAlphaArgs( 2 ) );
				if ( SteamBaseboard( BaseboardNum ).SchedPtr == 0 ) {
					ShowSevereError( RoutineName + cCMO_BBRadiator_Steam + "=\"" + cAlphaArgs( 1 ) + "\", " + cAlphaFieldNames( 2 ) + "=\"" + cAlphaArgs( 2 ) + "\" not found." );
					ErrorsFound = true;
				}
			}

			// Get inlet node number
			SteamBaseboard( BaseboardNum ).SteamInletNode = GetOnlySingleNode( cAlphaArgs( 3 ), ErrorsFound, cCMO_BBRadiator_Steam, cAlphaArgs( 1 ), NodeType_Steam, NodeConnectionType_Inlet, 1, ObjectIsNotParent );

			// Get outlet node number
			SteamBaseboard( BaseboardNum ).SteamOutletNode = GetOnlySingleNode( cAlphaArgs( 4 ), ErrorsFound, cCMO_BBRadiator_Steam, cAlphaArgs( 1 ), NodeType_Steam, NodeConnectionType_Outlet, 1, ObjectIsNotParent );
			TestCompSet( cCMO_BBRadiator_Steam, cAlphaArgs( 1 ), cAlphaArgs( 3 ), cAlphaArgs( 4 ), "Hot Steam Nodes" );

			// Determine steam baseboard radiator system heating design capacity sizing method
			if ( SameString( cAlphaArgs( iHeatCAPMAlphaNum ), "HeatingDesignCapacity" ) ) {
				SteamBaseboard( BaseboardNum ).HeatingCapMethod = HeatingDesignCapacity;

				if ( !lNumericFieldBlanks( iHeatDesignCapacityNumericNum ) ) {
					SteamBaseboard( BaseboardNum ).ScaledHeatingCapacity = rNumericArgs( iHeatDesignCapacityNumericNum );
					if ( SteamBaseboard( BaseboardNum ).ScaledHeatingCapacity < 0.0 && SteamBaseboard( BaseboardNum ).ScaledHeatingCapacity != AutoSize ) {
						ShowSevereError( cCMO_BBRadiator_Steam + " = " + SteamBaseboard( BaseboardNum ).EquipID );
						ShowContinueError( "Illegal " + cNumericFieldNames( iHeatDesignCapacityNumericNum ) + " = " + TrimSigDigits( rNumericArgs( iHeatDesignCapacityNumericNum ), 7 ) );
						ErrorsFound = true;
					}
				} else {
					ShowSevereError( cCMO_BBRadiator_Steam + " = " + SteamBaseboard( BaseboardNum ).EquipID );
					ShowContinueError( "Input for " + cAlphaFieldNames( iHeatCAPMAlphaNum ) + " = " + cAlphaArgs( iHeatCAPMAlphaNum ) );
					ShowContinueError( "Blank field not allowed for " + cNumericFieldNames( iHeatDesignCapacityNumericNum ) );
					ErrorsFound = true;
				}
			} else if ( SameString( cAlphaArgs( iHeatCAPMAlphaNum ), "CapacityPerFloorArea" ) ) {
				SteamBaseboard( BaseboardNum ).HeatingCapMethod = CapacityPerFloorArea;
				if ( !lNumericFieldBlanks( iHeatCapacityPerFloorAreaNumericNum ) ) {
					SteamBaseboard( BaseboardNum ).ScaledHeatingCapacity = rNumericArgs( iHeatCapacityPerFloorAreaNumericNum );
					if ( SteamBaseboard( BaseboardNum ).ScaledHeatingCapacity <= 0.0 ) {
						ShowSevereError( cCMO_BBRadiator_Steam + " = " + SteamBaseboard( BaseboardNum ).EquipID );
						ShowContinueError( "Input for " + cAlphaFieldNames( iHeatCAPMAlphaNum ) + " = " + cAlphaArgs( iHeatCAPMAlphaNum ) );
						ShowContinueError( "Illegal " + cNumericFieldNames( iHeatCapacityPerFloorAreaNumericNum ) + " = " + TrimSigDigits( rNumericArgs( iHeatCapacityPerFloorAreaNumericNum ), 7 ) );
						ErrorsFound = true;
					} else if ( SteamBaseboard( BaseboardNum ).ScaledHeatingCapacity == AutoSize ) {
						ShowSevereError( cCMO_BBRadiator_Steam + " = " + SteamBaseboard( BaseboardNum ).EquipID );
						ShowContinueError( "Input for " + cAlphaFieldNames( iHeatCAPMAlphaNum ) + " = " + cAlphaArgs( iHeatCAPMAlphaNum ) );
						ShowContinueError( "Illegal " + cNumericFieldNames( iHeatCapacityPerFloorAreaNumericNum ) + " = Autosize" );
						ErrorsFound = true;
					}
				} else {
					ShowSevereError( cCMO_BBRadiator_Steam + " = " + SteamBaseboard( BaseboardNum ).EquipID );
					ShowContinueError( "Input for " + cAlphaFieldNames( iHeatCAPMAlphaNum ) + " = " + cAlphaArgs( iHeatCAPMAlphaNum ) );
					ShowContinueError( "Blank field not allowed for " + cNumericFieldNames( iHeatCapacityPerFloorAreaNumericNum ) );
					ErrorsFound = true;
				}
			} else if ( SameString( cAlphaArgs( iHeatCAPMAlphaNum ), "FractionOfAutosizedHeatingCapacity" ) ) {
				SteamBaseboard( BaseboardNum ).HeatingCapMethod = FractionOfAutosizedHeatingCapacity;
				if ( !lNumericFieldBlanks( iHeatFracOfAutosizedCapacityNumericNum ) ) {
					SteamBaseboard( BaseboardNum ).ScaledHeatingCapacity = rNumericArgs( iHeatFracOfAutosizedCapacityNumericNum );
					if ( SteamBaseboard( BaseboardNum ).ScaledHeatingCapacity < 0.0 ) {
						ShowSevereError( cCMO_BBRadiator_Steam + " = " + SteamBaseboard( BaseboardNum ).EquipID );
						ShowContinueError( "Illegal " + cNumericFieldNames( iHeatFracOfAutosizedCapacityNumericNum ) + " = " + TrimSigDigits( rNumericArgs( iHeatFracOfAutosizedCapacityNumericNum ), 7 ) );
						ErrorsFound = true;
					}
				} else {
					ShowSevereError( cCMO_BBRadiator_Steam + " = " + SteamBaseboard( BaseboardNum ).EquipID );
					ShowContinueError( "Input for " + cAlphaFieldNames( iHeatCAPMAlphaNum ) + " = " + cAlphaArgs( iHeatCAPMAlphaNum ) );
					ShowContinueError( "Blank field not allowed for " + cNumericFieldNames( iHeatFracOfAutosizedCapacityNumericNum ) );
					ErrorsFound = true;
				}
			} else {
				ShowSevereError( cCMO_BBRadiator_Steam + " = " + SteamBaseboard( BaseboardNum ).EquipID );
				ShowContinueError( "Illegal " + cAlphaFieldNames( iHeatCAPMAlphaNum ) + " = " + cAlphaArgs( iHeatCAPMAlphaNum ) );
				ErrorsFound = true;
			}

			// Desired degree of cooling
			SteamBaseboard( BaseboardNum ).DegOfSubcooling = rNumericArgs( 4 );
			// Maximum steam flow rate
			SteamBaseboard( BaseboardNum ).SteamVolFlowRateMax = rNumericArgs( 5 );
			if ( SteamBaseboard( BaseboardNum ).SteamVolFlowRateMax >= MaxSteamFlowRate ) {
				ShowWarningError( RoutineName + cCMO_BBRadiator_Steam + "=\"" + cAlphaArgs( 1 ) + "\", " + cNumericFieldNames( 5 ) + " was higher than the allowable maximum." );
				ShowContinueError( "...reset to maximum value=[" + RoundSigDigits( MaxSteamFlowRate, 2 ) + "]." );
				SteamBaseboard( BaseboardNum ).SteamVolFlowRateMax = MaxSteamFlowRate;
			} else if ( SteamBaseboard( BaseboardNum ).SteamVolFlowRateMax <= MinSteamFlowRate && SteamBaseboard( BaseboardNum ).SteamVolFlowRateMax != AutoSize ) {
				ShowWarningError( RoutineName + cCMO_BBRadiator_Steam + "=\"" + cAlphaArgs( 1 ) + "\", " + cNumericFieldNames( 2 ) + " was less than the allowable minimum." );
				ShowContinueError( "...reset to minimum value=[" + RoundSigDigits( MinSteamFlowRate, 2 ) + "]." );
				SteamBaseboard( BaseboardNum ).SteamVolFlowRateMax = MinSteamFlowRate;
			}

			SteamBaseboard( BaseboardNum ).Offset = rNumericArgs( 6 );
			// Set default convergence tolerance
			if ( SteamBaseboard( BaseboardNum ).Offset <= 0.0 ) {
				SteamBaseboard( BaseboardNum ).Offset = 0.001;
				ShowWarningError( RoutineName + cCMO_BBRadiator_Steam + "=\"" + cAlphaArgs( 1 ) + "\", " + cNumericFieldNames( 6 ) + " was less than the allowable minimum." );
				ShowContinueError( "...reset to default value=[0.001]." );
			}
			// Fraction of radiant heat out of the total heating rate of the unit
			SteamBaseboard( BaseboardNum ).FracRadiant = rNumericArgs( 7 );
			if ( SteamBaseboard( BaseboardNum ).FracRadiant < MinFraction ) {
				ShowWarningError( RoutineName + cCMO_BBRadiator_Steam + "=\"" + cAlphaArgs( 1 ) + "\", " + cNumericFieldNames( 7 ) + " was lower than the allowable minimum." );
				ShowContinueError( "...reset to minimum value=[" + RoundSigDigits( MinFraction, 3 ) + "]." );
				SteamBaseboard( BaseboardNum ).FracRadiant = MinFraction;
			} else if ( SteamBaseboard( BaseboardNum ).FracRadiant > MaxFraction ) {
				ShowWarningError( RoutineName + cCMO_BBRadiator_Steam + "=\"" + cAlphaArgs( 1 ) + "\", " + cNumericFieldNames( 7 ) + " was higher than the allowable maximum." );
				ShowContinueError( "...reset to maximum value=[" + RoundSigDigits( MaxFraction, 3 ) + "]." );
				SteamBaseboard( BaseboardNum ).FracRadiant = MaxFraction;
			}

			// Remaining fraction is added to the zone as convective heat transfer
			AllFracsSummed = SteamBaseboard( BaseboardNum ).FracRadiant;
			if ( AllFracsSummed > MaxFraction ) {
				ShowWarningError( RoutineName + cCMO_BBRadiator_Steam + "=\"" + cAlphaArgs( 1 ) + "\", Fraction Radiant was higher than the allowable maximum." );
				SteamBaseboard( BaseboardNum ).FracRadiant = MaxFraction;
				SteamBaseboard( BaseboardNum ).FracConvect = 0.0;
			} else {
				SteamBaseboard( BaseboardNum ).FracConvect = 1.0 - AllFracsSummed;
			}
			// Fraction of radiant heat addition to the people within the radiant heating capacity specified by the user
			SteamBaseboard( BaseboardNum ).FracDistribPerson = rNumericArgs( 8 );
			if ( SteamBaseboard( BaseboardNum ).FracDistribPerson < MinFraction ) {
				ShowWarningError( RoutineName + cCMO_BBRadiator_Steam + "=\"" + cAlphaArgs( 1 ) + "\", " + cNumericFieldNames( 8 ) + " was lower than the allowable minimum." );
				ShowContinueError( "...reset to minimum value=[" + RoundSigDigits( MinFraction, 3 ) + "]." );
				SteamBaseboard( BaseboardNum ).FracDistribPerson = MinFraction;
			}
			if ( SteamBaseboard( BaseboardNum ).FracDistribPerson > MaxFraction ) {
				ShowWarningError( RoutineName + cCMO_BBRadiator_Steam + "=\"" + cAlphaArgs( 1 ) + "\", " + cNumericFieldNames( 8 ) + " was higher than the allowable maximum." );
				ShowContinueError( "...reset to maximum value=[" + RoundSigDigits( MaxFraction, 3 ) + "]." );
				SteamBaseboard( BaseboardNum ).FracDistribPerson = MaxFraction;
			}
			SteamBaseboard( BaseboardNum ).TotSurfToDistrib = NumNumbers - 8;
			//      IF (SteamBaseboard(BaseboardNum)%TotSurfToDistrib > MaxDistribSurfaces) THEN
			//        CALL ShowWarningError(RoutineName//cCMO_BBRadiator_Steam//'="'//TRIM(cAlphaArgs(1))// &
			//          '", the number of surface/radiant fraction groups entered was higher than the allowable maximum.')
			//        CALL ShowContinueError('...only the maximum value=['//TRIM(RoundSigDigits(MaxDistribSurfaces))//  &
			//           '] will be processed.')
			//        SteamBaseboard(BaseboardNum)%TotSurfToDistrib = MaxDistribSurfaces
			//      END IF
			if ( ( SteamBaseboard( BaseboardNum ).TotSurfToDistrib < MinDistribSurfaces ) && ( SteamBaseboard( BaseboardNum ).FracRadiant > MinFraction ) ) {
				ShowSevereError( RoutineName + cCMO_BBRadiator_Steam + "=\"" + cAlphaArgs( 1 ) + "\", the number of surface/radiant fraction groups entered was less than the allowable minimum." );
				ShowContinueError( "...the minimum that must be entered=[" + RoundSigDigits( MinDistribSurfaces ) + "]." );
				ErrorsFound = true;
				SteamBaseboard( BaseboardNum ).TotSurfToDistrib = 0;
			}
			// Allocate the surfaces and fractions
			SteamBaseboard( BaseboardNum ).SurfaceName.allocate( SteamBaseboard( BaseboardNum ).TotSurfToDistrib );
			SteamBaseboard( BaseboardNum ).SurfaceName = "";
			SteamBaseboard( BaseboardNum ).SurfacePtr.allocate( SteamBaseboard( BaseboardNum ).TotSurfToDistrib );
			SteamBaseboard( BaseboardNum ).SurfacePtr = 0;
			SteamBaseboard( BaseboardNum ).FracDistribToSurf.allocate( SteamBaseboard( BaseboardNum ).TotSurfToDistrib );
			SteamBaseboard( BaseboardNum ).FracDistribToSurf = 0.0;

			AllFracsSummed = SteamBaseboard( BaseboardNum ).FracDistribPerson;
			for ( SurfNum = 1; SurfNum <= SteamBaseboard( BaseboardNum ).TotSurfToDistrib; ++SurfNum ) {
				SteamBaseboard( BaseboardNum ).SurfaceName( SurfNum ) = cAlphaArgs( SurfNum + 5 );
				SteamBaseboard( BaseboardNum ).SurfacePtr( SurfNum ) = FindItemInList( cAlphaArgs( SurfNum + 5 ), Surface );
				SteamBaseboard( BaseboardNum ).FracDistribToSurf( SurfNum ) = rNumericArgs( SurfNum + 8 );
				if ( SteamBaseboard( BaseboardNum ).SurfacePtr( SurfNum ) == 0 ) {
					ShowSevereError( RoutineName + cCMO_BBRadiator_Steam + "=\"" + cAlphaArgs( 1 ) + "\", " + cAlphaFieldNames( SurfNum + 5 ) + "=\"" + cAlphaArgs( SurfNum + 5 ) + "\" invalid - not found." );
					ErrorsFound = true;
				}
				if ( SteamBaseboard( BaseboardNum ).FracDistribToSurf( SurfNum ) > MaxFraction ) {
					ShowWarningError( RoutineName + cCMO_BBRadiator_Steam + "=\"" + cAlphaArgs( 1 ) + "\", " + cNumericFieldNames( SurfNum + 8 ) + "was greater than the allowable maximum." );
					ShowContinueError( "...reset to maximum value=[" + RoundSigDigits( MaxFraction, 1 ) + "]." );
					SteamBaseboard( BaseboardNum ).TotSurfToDistrib = MaxFraction;
				}
				if ( SteamBaseboard( BaseboardNum ).FracDistribToSurf( SurfNum ) < MinFraction ) {
					ShowWarningError( RoutineName + cCMO_BBRadiator_Steam + "=\"" + cAlphaArgs( 1 ) + "\", " + cNumericFieldNames( SurfNum + 8 ) + "was less than the allowable minimum." );
					ShowContinueError( "...reset to maximum value=[" + RoundSigDigits( MinFraction, 1 ) + "]." );
					SteamBaseboard( BaseboardNum ).TotSurfToDistrib = MinFraction;
				}
				if ( SteamBaseboard( BaseboardNum ).SurfacePtr( SurfNum ) != 0 ) {
					Surface( SteamBaseboard( BaseboardNum ).SurfacePtr( SurfNum ) ).IntConvSurfGetsRadiantHeat = true;
				}

				AllFracsSummed += SteamBaseboard( BaseboardNum ).FracDistribToSurf( SurfNum );
			} // surfaces

			if ( AllFracsSummed > ( MaxFraction + 0.01 ) ) {
				ShowSevereError( "Fraction of radiation distributed to surfaces sums up to greater than 1 for " + cAlphaArgs( 1 ) );
				ShowContinueError( "Occurs in Baseboard Heater=" + cAlphaArgs( 1 ) );
				ErrorsFound = true;
			}
			if ( ( AllFracsSummed < ( MaxFraction - 0.01 ) ) && ( SteamBaseboard( BaseboardNum ).FracRadiant > MinFraction ) ) { // User didn't distribute all of the | radiation warn that some will be lost
				ShowWarningError( RoutineName + cCMO_BBRadiator_Steam + "=\"" + cAlphaArgs( 1 ) + "\", Summed radiant fractions for people + surface groups < 1.0" );
				ShowContinueError( "The rest of the radiant energy delivered by the baseboard heater will be lost" );
			}

			if ( SteamIndex == 0 && BaseboardNum == 1 ) {
				SteamIndex = FindRefrigerant( "Steam" );
				if ( SteamIndex == 0 ) {
					ShowSevereError( RoutineName + "Steam Properties for " + cAlphaArgs( 1 ) + " not found." );
					if ( SteamMessageNeeded ) ShowContinueError( "Steam Fluid Properties should have been included in the input file." );
					ErrorsFound = true;
					SteamMessageNeeded = false;
				}
			}

			SteamBaseboard( BaseboardNum ).FluidIndex = SteamIndex;

		}

		if ( ErrorsFound ) {
			ShowFatalError( RoutineName + cCMO_BBRadiator_Steam + "Errors found getting input. Program terminates." );
		}

		// Setup Report variables for the Coils
		for ( BaseboardNum = 1; BaseboardNum <= NumSteamBaseboards; ++BaseboardNum ) {
			// CurrentModuleObject='ZoneHVAC:Baseboard:RadiantConvective:Steam'
			SetupOutputVariable( "Baseboard Total Heating Rate [W]", SteamBaseboard( BaseboardNum ).TotPower, "System", "Average", SteamBaseboard( BaseboardNum ).EquipID );

			SetupOutputVariable( "Baseboard Convective Heating Rate [W]", SteamBaseboard( BaseboardNum ).ConvPower, "System", "Average", SteamBaseboard( BaseboardNum ).EquipID );
			SetupOutputVariable( "Baseboard Radiant Heating Rate [W]", SteamBaseboard( BaseboardNum ).RadPower, "System", "Average", SteamBaseboard( BaseboardNum ).EquipID );
			SetupOutputVariable( "Baseboard Total Heating Energy [J]", SteamBaseboard( BaseboardNum ).TotEnergy, "System", "Sum", SteamBaseboard( BaseboardNum ).EquipID, _, "ENERGYTRANSFER", "BASEBOARD", _, "System" );
			SetupOutputVariable( "Baseboard Convective Heating Energy [J]", SteamBaseboard( BaseboardNum ).ConvEnergy, "System", "Sum", SteamBaseboard( BaseboardNum ).EquipID );
			SetupOutputVariable( "Baseboard Radiant Heating Energy [J]", SteamBaseboard( BaseboardNum ).RadEnergy, "System", "Sum", SteamBaseboard( BaseboardNum ).EquipID );
			SetupOutputVariable( "Baseboard Steam Energy [J]", SteamBaseboard( BaseboardNum ).Energy, "System", "Sum", SteamBaseboard( BaseboardNum ).EquipID, _, "PLANTLOOPHEATINGDEMAND", "BASEBOARD", _, "System" );
			SetupOutputVariable( "Baseboard Steam Rate [W]", SteamBaseboard( BaseboardNum ).Power, "System", "Average", SteamBaseboard( BaseboardNum ).EquipID );
			SetupOutputVariable( "Baseboard Steam Mass Flow Rate [kg/s]", SteamBaseboard( BaseboardNum ).SteamMassFlowRate, "System", "Average", SteamBaseboard( BaseboardNum ).EquipID );
			SetupOutputVariable( "Baseboard Steam Inlet Temperature [C]", SteamBaseboard( BaseboardNum ).SteamInletTemp, "System", "Average", SteamBaseboard( BaseboardNum ).EquipID );
			SetupOutputVariable( "Baseboard Steam Outlet Temperature [C]", SteamBaseboard( BaseboardNum ).SteamOutletTemp, "System", "Average", SteamBaseboard( BaseboardNum ).EquipID );
		}

	}

	void
	InitSteamBaseboard(
		int const BaseboardNum,
		int const ControlledZoneNumSub,
		bool const FirstHVACIteration
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Russ Taylor
		//                      Rick Strand
		//       DATE WRITTEN   Nov 1997
		//                      Feb 2001
		//       MODIFIED       Sep 2009 Daeho Kang (Add Radiant Component)
		//                      Sept 2010 Chandan Sharma, FSEC (plant interactions)
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine initializes the baseboard units.

		// METHODOLOGY EMPLOYED:
		// The initialization subrotines both in high temperature radiant radiator
		// and convective only baseboard radiator are combined and modified.
		// The heater is assumed to be crossflow with both fluids unmixed.

		// REFERENCES:

		// Using/Aliasing
		using FluidProperties::GetSatEnthalpyRefrig;
		using FluidProperties::GetSatDensityRefrig;
		using PlantUtilities::InitComponentNodes;
		using DataPlant::ScanPlantLoopsForObject;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "InitSteamCoil" );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		static bool MyOneTimeFlag( true );
		static bool ZoneEquipmentListChecked( false );
		static Array1D_bool MyEnvrnFlag;
		int Loop;
		int SteamInletNode;
		int ZoneNode;
		int ZoneNum;
		Real64 StartEnthSteam;
		Real64 SteamDensity;
		bool errFlag;

		// Do the one time initializations
		if ( MyOneTimeFlag ) {

			// initialize the environment and sizing flags
			MyEnvrnFlag.allocate( NumSteamBaseboards );
			MySizeFlag.allocate( NumSteamBaseboards );
			ZeroSourceSumHATsurf.dimension( NumOfZones, 0.0 );
			QBBSteamRadSource.dimension( NumSteamBaseboards, 0.0 );
			QBBSteamRadSrcAvg.dimension( NumSteamBaseboards, 0.0 );
			LastQBBSteamRadSrc.dimension( NumSteamBaseboards, 0.0 );
			LastSysTimeElapsed.dimension( NumSteamBaseboards, 0.0 );
			LastTimeStepSys.dimension( NumSteamBaseboards, 0.0 );
			SetLoopIndexFlag.allocate( NumSteamBaseboards );
			MyEnvrnFlag = true;
			MySizeFlag = true;
			MyOneTimeFlag = false;
			SetLoopIndexFlag = true;
		}

		if ( SteamBaseboard( BaseboardNum ).ZonePtr <= 0 ) SteamBaseboard( BaseboardNum ).ZonePtr = ZoneEquipConfig( ControlledZoneNumSub ).ActualZoneNum;

		// Need to check all units to see if they are on ZoneHVAC:EquipmentList or issue warning
		if ( ! ZoneEquipmentListChecked && ZoneEquipInputsFilled ) {
			ZoneEquipmentListChecked = true;
			for ( Loop = 1; Loop <= NumSteamBaseboards; ++Loop ) {
				if ( CheckZoneEquipmentList( cCMO_BBRadiator_Steam, SteamBaseboard( Loop ).EquipID ) ) continue;
				ShowSevereError( "InitBaseboard: Unit=[" + cCMO_BBRadiator_Steam + ',' + SteamBaseboard( Loop ).EquipID + "] is not on any ZoneHVAC:EquipmentList.  It will not be simulated." );
			}
		}

		if ( SetLoopIndexFlag( BaseboardNum ) ) {
			if ( allocated( PlantLoop ) ) {
				errFlag = false;
				ScanPlantLoopsForObject( SteamBaseboard( BaseboardNum ).EquipID, SteamBaseboard( BaseboardNum ).EquipType, SteamBaseboard( BaseboardNum ).LoopNum, SteamBaseboard( BaseboardNum ).LoopSideNum, SteamBaseboard( BaseboardNum ).BranchNum, SteamBaseboard( BaseboardNum ).CompNum, _, _, _, _, _, errFlag );
				SetLoopIndexFlag( BaseboardNum ) = false;
				if ( errFlag ) {
					ShowFatalError( "InitSteamBaseboard: Program terminated for previous conditions." );
				}
			}
		}

		if ( ! SysSizingCalc && MySizeFlag( BaseboardNum ) && ( ! SetLoopIndexFlag( BaseboardNum ) ) ) {
			// For each coil, do the sizing once
			SizeSteamBaseboard( BaseboardNum );
			MySizeFlag( BaseboardNum ) = false;
		}

		// Do the Begin Environment initializations
		if ( BeginEnvrnFlag && MyEnvrnFlag( BaseboardNum ) ) {
			// Initialize
			SteamInletNode = SteamBaseboard( BaseboardNum ).SteamInletNode;
			Node( SteamInletNode ).Temp = 100.0;
			Node( SteamInletNode ).Press = 101325.0;
			SteamDensity = GetSatDensityRefrig( fluidNameSteam, Node( SteamInletNode ).Temp, 1.0, Node( SteamInletNode ).FluidIndex, RoutineName );
			StartEnthSteam = GetSatEnthalpyRefrig( fluidNameSteam, Node( SteamInletNode ).Temp, 1.0, Node( SteamInletNode ).FluidIndex, RoutineName );
			SteamBaseboard( BaseboardNum ).SteamMassFlowRateMax = SteamDensity * SteamBaseboard( BaseboardNum ).SteamVolFlowRateMax;
			InitComponentNodes( 0.0, SteamBaseboard( BaseboardNum ).SteamMassFlowRateMax, SteamBaseboard( BaseboardNum ).SteamInletNode, SteamBaseboard( BaseboardNum ).SteamOutletNode, SteamBaseboard( BaseboardNum ).LoopNum, SteamBaseboard( BaseboardNum ).LoopSideNum, SteamBaseboard( BaseboardNum ).BranchNum, SteamBaseboard( BaseboardNum ).CompNum );
			Node( SteamInletNode ).Enthalpy = StartEnthSteam;
			Node( SteamInletNode ).Quality = 1.0;
			Node( SteamInletNode ).HumRat = 0.0;

			// Initializes radiant sources
			ZeroSourceSumHATsurf = 0.0;
			QBBSteamRadSource = 0.0;
			QBBSteamRadSrcAvg = 0.0;
			LastQBBSteamRadSrc = 0.0;
			LastSysTimeElapsed = 0.0;
			LastTimeStepSys = 0.0;

			MyEnvrnFlag( BaseboardNum ) = false;
		}

		if ( ! BeginEnvrnFlag ) {
			MyEnvrnFlag( BaseboardNum ) = true;
		}

		if ( BeginTimeStepFlag && FirstHVACIteration ) {
			ZoneNum = SteamBaseboard( BaseboardNum ).ZonePtr;
			ZeroSourceSumHATsurf( ZoneNum ) = SumHATsurf( ZoneNum );
			QBBSteamRadSrcAvg( BaseboardNum ) = 0.0;
			LastQBBSteamRadSrc( BaseboardNum ) = 0.0;
			LastSysTimeElapsed( BaseboardNum ) = 0.0;
			LastTimeStepSys( BaseboardNum ) = 0.0;
		}

		// Do the every time step initializations
		SteamInletNode = SteamBaseboard( BaseboardNum ).SteamInletNode;
		ZoneNode = ZoneEquipConfig( ControlledZoneNumSub ).ZoneNode;
		SteamBaseboard( BaseboardNum ).SteamMassFlowRate = Node( SteamInletNode ).MassFlowRate;
		SteamBaseboard( BaseboardNum ).SteamInletTemp = Node( SteamInletNode ).Temp;
		SteamBaseboard( BaseboardNum ).SteamInletEnthalpy = Node( SteamInletNode ).Enthalpy;
		SteamBaseboard( BaseboardNum ).SteamInletPress = Node( SteamInletNode ).Press;
		SteamBaseboard( BaseboardNum ).SteamInletQuality = Node( SteamInletNode ).Quality;

		SteamBaseboard( BaseboardNum ).TotPower = 0.0;
		SteamBaseboard( BaseboardNum ).Power = 0.0;
		SteamBaseboard( BaseboardNum ).ConvPower = 0.0;
		SteamBaseboard( BaseboardNum ).RadPower = 0.0;
		SteamBaseboard( BaseboardNum ).TotEnergy = 0.0;
		SteamBaseboard( BaseboardNum ).Energy = 0.0;
		SteamBaseboard( BaseboardNum ).ConvEnergy = 0.0;
		SteamBaseboard( BaseboardNum ).RadEnergy = 0.0;

	}

	void
	SizeSteamBaseboard( int const BaseboardNum )
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Fred Buhl
		//       DATE WRITTEN   February 2002
		//       MODIFIED       August 2013 Daeho Kang, add component sizing table entries
		//                      August 2014 Bereket Nigusse, added scalable sizing
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine is for sizing steam baseboard components

		// METHODOLOGY EMPLOYED:
		// Obtains flow rates from the zone sizing arrays and plant sizing data.

		// REFERENCES:
		// na

		// Using/Aliasing
		using namespace DataSizing;
		using PlantUtilities::RegisterPlantCompDesignFlow;
		using FluidProperties::GetSatEnthalpyRefrig;
		using FluidProperties::GetSatDensityRefrig;
		using FluidProperties::GetSatSpecificHeatRefrig;
		//  USE BranchInputManager,  ONLY: MyPlantSizingIndex
		using ReportSizingManager::ReportSizingOutput;
		using ReportSizingManager::RequestSizing;
		using General::RoundSigDigits;
		using DataHVACGlobals::HeatingCapacitySizing;
		using DataHeatBalance::Zone;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "SizeSteamBaseboard" );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int PltSizSteamNum( 0 ); // Index of plant sizing object for 1st steam loop
		Real64 DesCoilLoad( 0.0 ); // Design heating load in the zone
		Real64 SteamInletTemp; // Inlet steam temperature in C
		Real64 EnthSteamInDry; // Enthalpy of dry steam
		Real64 EnthSteamOutWet; // Enthalpy of wet steam
		Real64 LatentHeatSteam; // latent heat of steam
		Real64 SteamDensity; // Density of steam
		Real64 Cp; // local fluid specific heat
		bool ErrorsFound( false ); // If errors detected in input
		bool IsAutoSize( false ); // Indicator to autosizing steam flow
		Real64 SteamVolFlowRateMaxDes( 0.0 ); // Design maximum steam volume flow for reporting
		Real64 SteamVolFlowRateMaxUser( 0.0 ); // User hard-sized maximum steam volume flow for reporting
		std::string CompName; // component name
		std::string CompType; // component type
		std::string SizingString; // input field sizing description (e.g., Nominal Capacity)
		Real64 TempSize; // autosized value of coil input field
		int FieldNum = 1; // IDD numeric field number where input field description is found
		int SizingMethod; // Integer representation of sizing method name (HeatingCapacitySizing)
		bool PrintFlag; // TRUE when sizing information is reported in the eio file
		int CapSizingMethod( 0 ); // capacity sizing methods (HeatingDesignCapacity, CapacityPerFloorArea, and FractionOfAutosizedHeatingCapacity )

		// Find the appropriate steam plant sizing object
		PltSizSteamNum = PlantLoop( SteamBaseboard( BaseboardNum ).LoopNum ).PlantSizNum;
		//    PltSizSteamNum = MyPlantSizingIndex('Coil:Heating:Steam', SteamBaseboard(BaseboardNum)%EquipID, &
		//                    SteamBaseboard(BaseboardNum)%SteamInletNode, &
		//                    SteamBaseboard(BaseboardNum)%SteamOutletNode, ErrorsFound)

		if ( PltSizSteamNum > 0 ) {

			if ( CurZoneEqNum > 0 ) {

				if ( SteamBaseboard( BaseboardNum ).SteamVolFlowRateMax == AutoSize ) {
					IsAutoSize = true;
				}
				if ( ! IsAutoSize && ! ZoneSizingRunDone ) {
					if ( SteamBaseboard( BaseboardNum ).SteamVolFlowRateMax > 0.0 ) {
						ReportSizingOutput( cCMO_BBRadiator_Steam, SteamBaseboard( BaseboardNum ).EquipID, "User-Specified Maximum Water Flow Rate [m3/s]", SteamBaseboard( BaseboardNum ).SteamVolFlowRateMax );
					}
				} else {
					CheckZoneSizing( cCMO_BBRadiator_Steam, SteamBaseboard( BaseboardNum ).EquipID );

					CompType = cCMO_BBRadiator_Steam;
					CompName = SteamBaseboard( BaseboardNum ).EquipID;
					DataFracOfAutosizedHeatingCapacity = 1.0;
					DataZoneNumber = SteamBaseboard( BaseboardNum ).ZonePtr;
					SizingMethod = HeatingCapacitySizing;
					FieldNum = 1;
					PrintFlag = false;
					SizingString = SteamBaseboardNumericFields( BaseboardNum ).FieldNames(FieldNum) + " [W]";
					CapSizingMethod = SteamBaseboard( BaseboardNum ).HeatingCapMethod;
					ZoneEqSizing( CurZoneEqNum ).SizingMethod( SizingMethod ) = CapSizingMethod;
					if ( CapSizingMethod == HeatingDesignCapacity || CapSizingMethod == CapacityPerFloorArea || CapSizingMethod == FractionOfAutosizedHeatingCapacity ) {

						if ( CapSizingMethod == HeatingDesignCapacity ) {
							if ( SteamBaseboard( BaseboardNum ).ScaledHeatingCapacity == AutoSize ) {
								CheckZoneSizing( CompType, CompName );
								ZoneEqSizing( CurZoneEqNum ).HeatingCapacity = true;
								ZoneEqSizing( CurZoneEqNum ).DesHeatingLoad = CalcFinalZoneSizing( CurZoneEqNum ).DesHeatLoad * CalcFinalZoneSizing( CurZoneEqNum ).HeatSizingFactor;
							}
							TempSize = SteamBaseboard( BaseboardNum ).ScaledHeatingCapacity;
						} else if ( CapSizingMethod == CapacityPerFloorArea ) {
							ZoneEqSizing( CurZoneEqNum ).HeatingCapacity = true;
							ZoneEqSizing( CurZoneEqNum ).DesHeatingLoad = SteamBaseboard( BaseboardNum ).ScaledHeatingCapacity * Zone( DataZoneNumber ).FloorArea;
							TempSize = ZoneEqSizing( CurZoneEqNum ).DesHeatingLoad;
							DataScalableCapSizingON = true;
						} else if ( CapSizingMethod == FractionOfAutosizedHeatingCapacity ) {
							CheckZoneSizing( CompType, CompName );
							ZoneEqSizing( CurZoneEqNum ).HeatingCapacity = true;
							DataFracOfAutosizedHeatingCapacity = SteamBaseboard( BaseboardNum ).ScaledHeatingCapacity;
							ZoneEqSizing( CurZoneEqNum ).DesHeatingLoad = CalcFinalZoneSizing( CurZoneEqNum ).DesHeatLoad * CalcFinalZoneSizing( CurZoneEqNum ).HeatSizingFactor;
							TempSize = AutoSize;
							DataScalableCapSizingON = true;
						} else {
							TempSize = SteamBaseboard( BaseboardNum ).ScaledHeatingCapacity;
						}
						RequestSizing( CompType, CompName, SizingMethod, SizingString, TempSize, PrintFlag, RoutineName );
						DesCoilLoad = TempSize;
					} else {
						DesCoilLoad = 0.0; // CalcFinalZoneSizing(CurZoneEqNum).DesHeatLoad * CalcFinalZoneSizing(CurZoneEqNum).HeatSizingFactor;
					}


					if ( DesCoilLoad >= SmallLoad ) {
						SteamInletTemp = 100.0;
						EnthSteamInDry = GetSatEnthalpyRefrig( fluidNameSteam, SteamInletTemp, 1.0, SteamBaseboard( BaseboardNum ).FluidIndex, RoutineName );
						EnthSteamOutWet = GetSatEnthalpyRefrig( fluidNameSteam, SteamInletTemp, 0.0, SteamBaseboard( BaseboardNum ).FluidIndex, RoutineName );
						LatentHeatSteam = EnthSteamInDry - EnthSteamOutWet;
						SteamDensity = GetSatDensityRefrig( fluidNameSteam, SteamInletTemp, 1.0, SteamBaseboard( BaseboardNum ).FluidIndex, RoutineName );
						Cp = GetSatSpecificHeatRefrig( fluidNameSteam, SteamInletTemp, 0.0, SteamBaseboard( BaseboardNum ).FluidIndex, RoutineName );

						SteamVolFlowRateMaxDes = DesCoilLoad / ( SteamDensity * ( LatentHeatSteam + SteamBaseboard( BaseboardNum ).DegOfSubcooling * Cp ) );
					} else {
						SteamVolFlowRateMaxDes = 0.0;
					}

					if ( IsAutoSize ) {
						SteamBaseboard( BaseboardNum ).SteamVolFlowRateMax = SteamVolFlowRateMaxDes;
						ReportSizingOutput( cCMO_BBRadiator_Steam, SteamBaseboard( BaseboardNum ).EquipID, "Design Size Maximum Steam Flow Rate [m3/s]", SteamVolFlowRateMaxDes );
					} else { // Hard size with sizing data
						if ( SteamBaseboard( BaseboardNum ).SteamVolFlowRateMax > 0.0 && SteamVolFlowRateMaxDes > 0.0 ) {
							SteamVolFlowRateMaxUser = SteamBaseboard( BaseboardNum ).SteamVolFlowRateMax;
							ReportSizingOutput( cCMO_BBRadiator_Steam, SteamBaseboard( BaseboardNum ).EquipID, "Design Size Maximum Steam Flow Rate [m3/s]", SteamVolFlowRateMaxDes, "User-Speicified Maximum Steam Flow Rate [m3/s]", SteamVolFlowRateMaxUser );
							if ( DisplayExtraWarnings ) {
								// Report difference between design size and user-specified values
								if ( ( std::abs( SteamVolFlowRateMaxDes - SteamVolFlowRateMaxUser ) / SteamVolFlowRateMaxUser ) > AutoVsHardSizingThreshold ) {
									ShowMessage( "SizeSteamBaseboard: Potential issue with equipment sizing for ZoneHVAC:Baseboard:RadiantConvective:Steam=\"" + SteamBaseboard( BaseboardNum ).EquipID + "\"." );
									ShowContinueError( "User-Specified Maximum Steam Flow Rate of " + RoundSigDigits( SteamVolFlowRateMaxUser, 5 ) + " [m3/s]" );
									ShowContinueError( "differs from Design Size Maximum Steam Flow Rate of " + RoundSigDigits( SteamVolFlowRateMaxDes, 5 ) + " [m3/s]" );
									ShowContinueError( "This may, or may not, indicate mismatched component sizes." );
									ShowContinueError( "Verify that the value entered is intended and is consistent with other components." );
								}
							}
						}
					}
				}
			}
		} else {
			if ( IsAutoSize ) {
				// if there is no heating Sizing:Plant object and autosizng was requested, issue an error message
				// first error will be issued by MyPlantSizingIndex
				ShowSevereError( "Autosizing of steam baseboard requires a heating loop Sizing:Plant object" );
				ShowContinueError( "Occurs in Baseboard Heater=" + SteamBaseboard( BaseboardNum ).EquipID );
				ErrorsFound = true;
			}
		}

		RegisterPlantCompDesignFlow( SteamBaseboard( BaseboardNum ).SteamInletNode, SteamBaseboard( BaseboardNum ).SteamVolFlowRateMax );

		if ( ErrorsFound ) {
			ShowFatalError( "Preceding sizing errors cause program termination" );
		}

	}

	void
	CalcSteamBaseboard(
		int & BaseboardNum,
		Real64 & LoadMet
	)
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         Daeho Kang
		//       DATE WRITTEN   September 2009
		//       MODIFIED       Sep 2011 LKL/BG - resimulate only zones needing it for Radiant systems
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine calculates both the convective and radiant heat transfer rate
		// of steam baseboard heaters. The heater is assumed to be crossflow with
		// both fluids unmixed. The air flow is bouyancy driven and a constant airflow.

		// METHODOLOGY EMPLOYED:
		// Equations that calculates heating capacity of steam coils and outlet air and water temperatures
		// of the zone control steam coil in steam coil module in EnergyPlus are employed.

		// REFERENCES:

		// Using/Aliasing
		using ScheduleManager::GetCurrentScheduleValue;
		using DataZoneEnergyDemands::ZoneSysEnergyDemand;
		using DataZoneEnergyDemands::CurDeadBandOrSetback;
		using FluidProperties::GetSatEnthalpyRefrig;
		using FluidProperties::GetSatDensityRefrig;
		using FluidProperties::GetSatSpecificHeatRefrig;
		using DataHVACGlobals::SmallLoad;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "CalcSteamBaseboard" );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int ZoneNum;
		Real64 RadHeat;
		Real64 SteamBBHeat;
		Real64 SteamInletTemp;
		Real64 SteamOutletTemp;
		Real64 SteamMassFlowRate;
		Real64 SubcoolDeltaT;
		Real64 QZnReq;
		Real64 EnthSteamInDry;
		Real64 EnthSteamOutWet;
		Real64 LatentHeatSteam;
		Real64 Cp;

		ZoneNum = SteamBaseboard( BaseboardNum ).ZonePtr;
		QZnReq = ZoneSysEnergyDemand( ZoneNum ).RemainingOutputReqToHeatSP;
		SteamInletTemp = Node( SteamBaseboard( BaseboardNum ).SteamInletNode ).Temp;
		SteamOutletTemp = SteamInletTemp;
		SteamMassFlowRate = Node( SteamBaseboard( BaseboardNum ).SteamInletNode ).MassFlowRate;
		SubcoolDeltaT = SteamBaseboard( BaseboardNum ).DegOfSubcooling;

		if ( QZnReq > SmallLoad && ! CurDeadBandOrSetback( ZoneNum ) && SteamMassFlowRate > 0.0 && GetCurrentScheduleValue( SteamBaseboard( BaseboardNum ).SchedPtr ) > 0 ) {
			// Unit is on
			EnthSteamInDry = GetSatEnthalpyRefrig( fluidNameSteam, SteamInletTemp, 1.0, SteamBaseboard( BaseboardNum ).FluidIndex, RoutineName );
			EnthSteamOutWet = GetSatEnthalpyRefrig( fluidNameSteam, SteamInletTemp, 0.0, SteamBaseboard( BaseboardNum ).FluidIndex, RoutineName );
			LatentHeatSteam = EnthSteamInDry - EnthSteamOutWet;
			Cp = GetSatSpecificHeatRefrig( fluidNameSteam, SteamInletTemp, 0.0, SteamBaseboard( BaseboardNum ).FluidIndex, RoutineName );
			SteamBBHeat = SteamMassFlowRate * ( LatentHeatSteam + SubcoolDeltaT * Cp ); // Baseboard heating rate
			SteamOutletTemp = SteamInletTemp - SubcoolDeltaT; // Outlet temperature of steam
			// Estimate radiant heat addition
			RadHeat = SteamBBHeat * SteamBaseboard( BaseboardNum ).FracRadiant; // Radiant heating rate
			QBBSteamRadSource( BaseboardNum ) = RadHeat; // Radiant heat source which will be distributed to surfaces and people

			// Now, distribute the radiant energy of all systems to the appropriate surfaces, to people, and the air
			DistributeBBSteamRadGains();
			// Now "simulate" the system by recalculating the heat balances
			HeatBalanceSurfaceManager::CalcHeatBalanceOutsideSurf( ZoneNum );
			HeatBalanceSurfaceManager::CalcHeatBalanceInsideSurf( ZoneNum );

			// Here an assumption is made regarding radiant heat transfer to people.
			// While the radiant heat transfer to people array will be used by the thermal comfort
			// routines, the energy transfer to people would get lost from the perspective
			// of the heat balance.  So, to avoid this net loss of energy which clearly
			// gets added to the zones, we must account for it somehow.  This assumption
			// that all energy radiated to people is converted to convective energy is
			// not very precise, but at least it conserves energy. The system impact to heat balance
			// should include this.

			// Actual system load that the unit should meet
			LoadMet = ( SumHATsurf( ZoneNum ) - ZeroSourceSumHATsurf( ZoneNum ) ) + ( SteamBBHeat * SteamBaseboard( BaseboardNum ).FracConvect ) + ( RadHeat * SteamBaseboard( BaseboardNum ).FracDistribPerson );
			SteamBaseboard( BaseboardNum ).SteamOutletEnthalpy = SteamBaseboard( BaseboardNum ).SteamInletEnthalpy - SteamBBHeat / SteamMassFlowRate;
			SteamBaseboard( BaseboardNum ).SteamOutletQuality = 0.0;
		} else {
			SteamOutletTemp = SteamInletTemp;
			SteamBBHeat = 0.0;
			LoadMet = 0.0;
			RadHeat = 0.0;
			SteamMassFlowRate = 0.0;
			QBBSteamRadSource( BaseboardNum ) = 0.0;
			SteamBaseboard( BaseboardNum ).SteamOutletQuality = 0.0;
			SteamBaseboard( BaseboardNum ).SteamOutletEnthalpy = SteamBaseboard( BaseboardNum ).SteamInletEnthalpy;
		}

		SteamBaseboard( BaseboardNum ).SteamOutletTemp = SteamOutletTemp;
		SteamBaseboard( BaseboardNum ).SteamMassFlowRate = SteamMassFlowRate;
		SteamBaseboard( BaseboardNum ).SteamOutletEnthalpy = SteamBaseboard( BaseboardNum ).SteamOutletEnthalpy;
		SteamBaseboard( BaseboardNum ).SteamOutletQuality = SteamBaseboard( BaseboardNum ).SteamOutletQuality;
		SteamBaseboard( BaseboardNum ).TotPower = LoadMet;
		SteamBaseboard( BaseboardNum ).Power = SteamBBHeat;
		SteamBaseboard( BaseboardNum ).ConvPower = SteamBBHeat - RadHeat;
		SteamBaseboard( BaseboardNum ).RadPower = RadHeat;

	}

	void
	UpdateSteamBaseboard( int const BaseboardNum )
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         Russ Taylor
		//                      Rick Strand
		//       DATE WRITTEN   Nov 1997
		//                      February 2001
		//       MODIFIED       Sep 2009 Daeho Kang (add radiant component)
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:

		// METHODOLOGY EMPLOYED:
		// The update subrotines both in high temperature radiant radiator
		// and convective only baseboard radiator are combined and modified.

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataGlobals::TimeStepZone;
		using DataGlobals::BeginEnvrnFlag;
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
		int SteamInletNode;
		int SteamOutletNode;
		static int Iter( 0 );
		static bool MyEnvrnFlag( true );

		if ( BeginEnvrnFlag && MyEnvrnFlag ) {
			Iter = 0;
			MyEnvrnFlag = false;
		}
		if ( ! BeginEnvrnFlag ) {
			MyEnvrnFlag = true;
		}

		// First, update the running average if necessary...
		if ( LastSysTimeElapsed( BaseboardNum ) == SysTimeElapsed ) {
			QBBSteamRadSrcAvg( BaseboardNum ) -= LastQBBSteamRadSrc( BaseboardNum ) * LastTimeStepSys( BaseboardNum ) / TimeStepZone;
		}
		// Update the running average and the "last" values with the current values of the appropriate variables
		QBBSteamRadSrcAvg( BaseboardNum ) += QBBSteamRadSource( BaseboardNum ) * TimeStepSys / TimeStepZone;

		LastQBBSteamRadSrc( BaseboardNum ) = QBBSteamRadSource( BaseboardNum );
		LastSysTimeElapsed( BaseboardNum ) = SysTimeElapsed;
		LastTimeStepSys( BaseboardNum ) = TimeStepSys;

		SteamInletNode = SteamBaseboard( BaseboardNum ).SteamInletNode;
		SteamOutletNode = SteamBaseboard( BaseboardNum ).SteamOutletNode;

		// Set the outlet air nodes of the Baseboard
		// Set the outlet water nodes for the Coil
		SafeCopyPlantNode( SteamInletNode, SteamOutletNode );
		Node( SteamOutletNode ).Temp = SteamBaseboard( BaseboardNum ).SteamOutletTemp;
		Node( SteamOutletNode ).Enthalpy = SteamBaseboard( BaseboardNum ).SteamOutletEnthalpy;

	}

	void
	UpdateBBSteamRadSourceValAvg( bool & SteamBaseboardSysOn ) // .TRUE. if the radiant system has run this zone time step
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Rick Strand
		//       DATE WRITTEN   February 2001
		//       MODIFIED       Aug 2009 Daeho Kang (modify only for baseboard)
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// To transfer the average value of the heat source over the entire
		// zone time step back to the heat balance routines so that the heat
		// balance algorithms can simulate one last time with the average source
		// to maintain some reasonable amount of continuity and energy balance
		// in the temperature and flux histories.

		// METHODOLOGY EMPLOYED:
		// All of the record keeping for the average term is done in the Update
		// routine so the only other thing that this subroutine does is check to
		// see if the system was even on.  If any average term is non-zero, then
		// one or more of the radiant systems was running.

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int BaseboardNum; // DO loop counter for surface index

		// FLOW:
		SteamBaseboardSysOn = false;

		// If this was never allocated, then there are no radiant systems in this input file (just RETURN)
		if ( ! allocated( QBBSteamRadSrcAvg ) ) return;

		// If it was allocated, then we have to check to see if this was running at all...
		for ( BaseboardNum = 1; BaseboardNum <= NumSteamBaseboards; ++BaseboardNum ) {
			if ( QBBSteamRadSrcAvg( BaseboardNum ) != 0.0 ) {
				SteamBaseboardSysOn = true;
				break; //DO loop
			}
		}

		QBBSteamRadSource = QBBSteamRadSrcAvg;

		DistributeBBSteamRadGains(); // QBBRadSource has been modified so we need to redistribute gains

	}

	void
	DistributeBBSteamRadGains()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Rick Strand
		//       DATE WRITTEN   February 2001
		//       MODIFIED       Aug. 2009 Daeho Kang (modify only for steam baseboard)
		//                      April 2010 Brent Griffith, max limit to protect surface temperature calcs
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// To distribute the gains from the steam basebaord heater
		// as specified in the user input file.  This includes distribution
		// of long wavelength radiant gains to surfaces and "people."

		// METHODOLOGY EMPLOYED:
		// We must cycle through all of the radiant systems because each
		// surface could feel the effect of more than one radiant system.
		// Note that the energy radiated to people is assumed to affect them
		// but them it is assumed to be convected to the air.

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataHeatBalance::Zone;
		using DataHeatBalFanSys::QSteamBaseboardToPerson;
		using DataHeatBalFanSys::QSteamBaseboardSurf;
		using DataHeatBalFanSys::MaxRadHeatFlux;
		using DataSurfaces::Surface;
		using General::RoundSigDigits;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		Real64 const SmallestArea( 0.001 ); // Smallest area in meters squared (to avoid a divide by zero)

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int RadSurfNum; // Counter for surfaces receiving radiation from radiant heater
		int BaseboardNum; // Counter for the baseboard
		int SurfNum; // Pointer to the Surface derived type
		int ZoneNum; // Pointer to the Zone derived type
		Real64 ThisSurfIntensity; // temporary for W/m2 term for rad on a surface

		// FLOW:
		// Initialize arrays
		QSteamBaseboardSurf = 0.0;
		QSteamBaseboardToPerson = 0.0;

		for ( BaseboardNum = 1; BaseboardNum <= NumSteamBaseboards; ++BaseboardNum ) {

			ZoneNum = SteamBaseboard( BaseboardNum ).ZonePtr;
			QSteamBaseboardToPerson( ZoneNum ) += QBBSteamRadSource( BaseboardNum ) * SteamBaseboard( BaseboardNum ).FracDistribPerson;

			for ( RadSurfNum = 1; RadSurfNum <= SteamBaseboard( BaseboardNum ).TotSurfToDistrib; ++RadSurfNum ) {
				SurfNum = SteamBaseboard( BaseboardNum ).SurfacePtr( RadSurfNum );
				if ( Surface( SurfNum ).Area > SmallestArea ) {
					ThisSurfIntensity = ( QBBSteamRadSource( BaseboardNum ) * SteamBaseboard( BaseboardNum ).FracDistribToSurf( RadSurfNum ) / Surface( SurfNum ).Area );
					QSteamBaseboardSurf( SurfNum ) += ThisSurfIntensity;

					if ( ThisSurfIntensity > MaxRadHeatFlux ) { // CR 8074, trap for excessive intensity (throws off surface balance )
						ShowSevereError( "DistributeBBSteamRadGains:  excessive thermal radiation heat flux intensity detected" );
						ShowContinueError( "Surface = " + Surface( SurfNum ).Name );
						ShowContinueError( "Surface area = " + RoundSigDigits( Surface( SurfNum ).Area, 3 ) + " [m2]" );
						ShowContinueError( "Occurs in " + cCMO_BBRadiator_Steam + " = " + SteamBaseboard( BaseboardNum ).EquipID );
						ShowContinueError( "Radiation intensity = " + RoundSigDigits( ThisSurfIntensity, 2 ) + " [W/m2]" );
						ShowContinueError( "Assign a larger surface area or more surfaces in " + cCMO_BBRadiator_Steam );
						ShowFatalError( "DistributeBBSteamRadGains:  excessive thermal radiation heat flux intensity detected" );
					}
				} else { // small surface
					ShowSevereError( "DistributeBBSteamRadGains:  surface not large enough to receive thermal radiation heat flux" );
					ShowContinueError( "Surface = " + Surface( SurfNum ).Name );
					ShowContinueError( "Surface area = " + RoundSigDigits( Surface( SurfNum ).Area, 3 ) + " [m2]" );
					ShowContinueError( "Occurs in " + cCMO_BBRadiator_Steam + " = " + SteamBaseboard( BaseboardNum ).EquipID );
					ShowContinueError( "Assign a larger surface area or more surfaces in " + cCMO_BBRadiator_Steam );
					ShowFatalError( "DistributeBBSteamRadGains:  surface not large enough to receive thermal radiation heat flux" );

				}
			}

		}

	}

	void
	ReportSteamBaseboard( int const BaseboardNum )
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Daeho Kang
		//       DATE WRITTEN   September 2009
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataSurfaces::Surface;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		SteamBaseboard( BaseboardNum ).TotEnergy = SteamBaseboard( BaseboardNum ).TotPower * TimeStepSys * SecInHour;
		SteamBaseboard( BaseboardNum ).Energy = SteamBaseboard( BaseboardNum ).Power * TimeStepSys * SecInHour;
		SteamBaseboard( BaseboardNum ).ConvEnergy = SteamBaseboard( BaseboardNum ).ConvPower * TimeStepSys * SecInHour;
		SteamBaseboard( BaseboardNum ).RadEnergy = SteamBaseboard( BaseboardNum ).RadPower * TimeStepSys * SecInHour;

	}

	Real64
	SumHATsurf( int const ZoneNum ) // Zone number
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Peter Graham Ellis
		//       DATE WRITTEN   July 2003
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// This function calculates the zone sum of Hc*Area*Tsurf.  It replaces the old SUMHAT.
		// The SumHATsurf code below is also in the CalcZoneSums subroutine in ZoneTempPredictorCorrector
		// and should be updated accordingly.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using namespace DataSurfaces;
		using namespace DataHeatBalance;
		using namespace DataHeatBalSurface;

		// Return value
		Real64 SumHATsurf;

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		int SurfNum; // Surface number
		Real64 Area; // Effective surface area

		// FLOW:
		SumHATsurf = 0.0;

		for ( SurfNum = Zone( ZoneNum ).SurfaceFirst; SurfNum <= Zone( ZoneNum ).SurfaceLast; ++SurfNum ) {
			if ( ! Surface( SurfNum ).HeatTransSurf ) continue; // Skip non-heat transfer surfaces

			Area = Surface( SurfNum ).Area;

			if ( Surface( SurfNum ).Class == SurfaceClass_Window ) {
				if ( SurfaceWindow( SurfNum ).ShadingFlag == IntShadeOn || SurfaceWindow( SurfNum ).ShadingFlag == IntBlindOn ) {
					// The area is the shade or blind area = the sum of the glazing area and the divider area (which is zero if no divider)
					Area += SurfaceWindow( SurfNum ).DividerArea;
				}

				if ( SurfaceWindow( SurfNum ).FrameArea > 0.0 ) {
					// Window frame contribution
					SumHATsurf += HConvIn( SurfNum ) * SurfaceWindow( SurfNum ).FrameArea * ( 1.0 + SurfaceWindow( SurfNum ).ProjCorrFrIn ) * SurfaceWindow( SurfNum ).FrameTempSurfIn;
				}

				if ( SurfaceWindow( SurfNum ).DividerArea > 0.0 && SurfaceWindow( SurfNum ).ShadingFlag != IntShadeOn && SurfaceWindow( SurfNum ).ShadingFlag != IntBlindOn ) {
					// Window divider contribution (only from shade or blind for window with divider and interior shade or blind)
					SumHATsurf += HConvIn( SurfNum ) * SurfaceWindow( SurfNum ).DividerArea * ( 1.0 + 2.0 * SurfaceWindow( SurfNum ).ProjCorrDivIn ) * SurfaceWindow( SurfNum ).DividerTempSurfIn;
				}
			}

			SumHATsurf += HConvIn( SurfNum ) * Area * TempSurfInTmp( SurfNum );
		}

		return SumHATsurf;

	}

	void
	UpdateSteamBaseboardPlantConnection(
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
		//       AUTHOR         Chandan Sharma
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
		using DataPlant::TypeOf_Baseboard_Rad_Conv_Steam;
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
			BaseboardNum = FindItemInList( BaseboardName, SteamBaseboard, &SteamBaseboardParams::EquipID );
			if ( BaseboardNum == 0 ) {
				ShowFatalError( "UpdateSteamBaseboardPlantConnection: Specified baseboard not valid =" + BaseboardName );
			}
			CompIndex = BaseboardNum;
		} else {
			BaseboardNum = CompIndex;
			if ( BaseboardNum > NumSteamBaseboards || BaseboardNum < 1 ) {
				ShowFatalError( "UpdateSteamBaseboardPlantConnection:  Invalid CompIndex passed=" + TrimSigDigits( BaseboardNum ) + ", Number of baseboards=" + TrimSigDigits( NumSteamBaseboards ) + ", Entered baseboard name=" + BaseboardName );
			}
			if ( KickOffSimulation ) {
				if ( BaseboardName != SteamBaseboard( BaseboardNum ).EquipID ) {
					ShowFatalError( "UpdateSteamBaseboardPlantConnection: Invalid CompIndex passed=" + TrimSigDigits( BaseboardNum ) + ", baseboard name=" + BaseboardName + ", stored baseboard Name for that index=" + SteamBaseboard( BaseboardNum ).EquipID );
				}
				if ( BaseboardTypeNum != TypeOf_Baseboard_Rad_Conv_Steam ) {
					ShowFatalError( "UpdateSteamBaseboardPlantConnection: Invalid CompIndex passed=" + TrimSigDigits( BaseboardNum ) + ", baseboard name=" + BaseboardName + ", stored baseboard Name for that index=" + ccSimPlantEquipTypes( BaseboardTypeNum ) );
				}
			}
		}

		if ( InitLoopEquip ) {
			return;
		}

		PullCompInterconnectTrigger( SteamBaseboard( BaseboardNum ).LoopNum, SteamBaseboard( BaseboardNum ).LoopSideNum, SteamBaseboard( BaseboardNum ).BranchNum, SteamBaseboard( BaseboardNum ).CompNum, SteamBaseboard( BaseboardNum ).BBLoadReSimIndex, SteamBaseboard( BaseboardNum ).LoopNum, SteamBaseboard( BaseboardNum ).LoopSideNum, CriteriaType_HeatTransferRate, SteamBaseboard( BaseboardNum ).Power );

		PullCompInterconnectTrigger( SteamBaseboard( BaseboardNum ).LoopNum, SteamBaseboard( BaseboardNum ).LoopSideNum, SteamBaseboard( BaseboardNum ).BranchNum, SteamBaseboard( BaseboardNum ).CompNum, SteamBaseboard( BaseboardNum ).BBLoadReSimIndex, SteamBaseboard( BaseboardNum ).LoopNum, SteamBaseboard( BaseboardNum ).LoopSideNum, CriteriaType_MassFlowRate, SteamBaseboard( BaseboardNum ).SteamMassFlowRate );

		PullCompInterconnectTrigger( SteamBaseboard( BaseboardNum ).LoopNum, SteamBaseboard( BaseboardNum ).LoopSideNum, SteamBaseboard( BaseboardNum ).BranchNum, SteamBaseboard( BaseboardNum ).CompNum, SteamBaseboard( BaseboardNum ).BBLoadReSimIndex, SteamBaseboard( BaseboardNum ).LoopNum, SteamBaseboard( BaseboardNum ).LoopSideNum, CriteriaType_Temperature, SteamBaseboard( BaseboardNum ).SteamOutletTemp );

	}

} // SteamBaseboardRadiator

} // EnergyPlus
