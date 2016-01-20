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
#include <ObjexxFCL/string.functions.hh>

// EnergyPlus Headers
#include <PurchasedAirManager.hh>
#include <DataContaminantBalance.hh>
#include <DataEnvironment.hh>
#include <DataHeatBalance.hh>
#include <DataHeatBalFanSys.hh>
#include <DataHVACGlobals.hh>
#include <DataIPShortCuts.hh>
#include <DataLoopNode.hh>
#include <DataPrecisionGlobals.hh>
#include <DataSizing.hh>
#include <DataZoneEnergyDemands.hh>
#include <DataZoneEquipment.hh>
#include <EMSManager.hh>
#include <General.hh>
#include <GeneralRoutines.hh>
#include <InputProcessor.hh>
#include <NodeInputManager.hh>
#include <OutputProcessor.hh>
#include <OutAirNodeManager.hh>
#include <Psychrometrics.hh>
#include <ReportSizingManager.hh>
#include <ScheduleManager.hh>
#include <UtilityRoutines.hh>

namespace EnergyPlus {

namespace PurchasedAirManager {

	// Module containing data and routines dealing with Ideal Loads Air System (formerly PURCHASED AIR).

	// MODULE INFORMATION:
	//       AUTHOR         Russ Taylor
	//       DATE WRITTEN   May 1997
	//       MODIFIED       Fred Buhl Dec 1999
	//                      B. Griffith Dec 2006. added OA lookup function, moved getinputflag up to Module
	//                      M. Witte June 2011, add new features including DCV, economizer, dehumidification and humidification
	//                      NOTE: MJW Sep 13, 2011:  Still need to review checks for negative loads and impossible supply temps???
	//                           There are no Deallocate statements in here - should there be?
	//       RE-ENGINEERED  na

	// PURPOSE OF THIS MODULE:
	// To encapsulate the data and algorithms required to simulate the
	// Zone Ideal Loads Air System component. This component supplies hot or cold air
	// at a fixed or variable temperature to a zone to meet the zone load.
	// With the June 2011 enhancements it will also supply outdoor air with optional demand-controlled ventilation
	// and economizer controls, plus new options for controlling zone humidity.

	// METHODOLOGY EMPLOYED:
	// The user can choose via input the max/min hot and cold supply air
	// temperature and humidity ratio. The air mass flow rate is chosen
	// to meet the (remaining) zone load or based on the outdoor air flow requirement.
	// If the outdoor air flow sets the flow rate, the supply air temperature and
	// humidity ratio are adjusted to meet the zone load.

	// REFERENCES: none

	// OTHER NOTES: none

	// USE STATEMENTS:
	// Use statements for data only modules
	// Using/Aliasing
	using namespace DataPrecisionGlobals;
	using namespace DataGlobals;
	using namespace DataHVACGlobals;
	using DataHeatBalFanSys::ZoneAirHumRat;
	using DataHeatBalFanSys::ZoneThermostatSetPointHi;
	using DataHeatBalFanSys::ZoneThermostatSetPointLo;
	using DataEnvironment::StdBaroPress;
	using DataEnvironment::OutBaroPress;
	using DataEnvironment::OutHumRat;
	using DataEnvironment::OutEnthalpy;
	using DataEnvironment::StdRhoAir;

	// Use statements for access to subroutines in other modules
	using namespace ScheduleManager;
	using Psychrometrics::PsyRhoAirFnPbTdbW;
	using Psychrometrics::PsyHFnTdbW;
	using Psychrometrics::PsyTdbFnHW;
	using Psychrometrics::PsyCpAirFnWTdb;
	using Psychrometrics::PsyTsatFnHPb;
	using Psychrometrics::PsyWFnTdbH;
	using Psychrometrics::PsyWFnTdbRhPb;

	// Data
	//MODULE PARAMETER DEFINITIONS:
	// MODULE PARAMETER DEFINITIONS:
	// Heating and Cooling Limit type parameters
	int const NoLimit( 1 );
	int const LimitFlowRate( 2 );
	int const LimitCapacity( 3 );
	int const LimitFlowRateAndCapacity( 4 );
	Array1D_string const cLimitType( 4, { "NoLimit", "LimitFlowRate", "LimitCapacity", "LimitFlowRateAndCapacity" } );
	// Dehumidification and Humidification control type parameters
	int const None( 1 );
	int const ConstantSensibleHeatRatio( 2 );
	int const Humidistat( 3 );
	int const ConstantSupplyHumidityRatio( 4 );
	// Demand controlled ventilation type parameters
	int const NoDCV( 1 );
	int const OccupancySchedule( 2 );
	int const CO2SetPoint( 3 );
	// Outdoor air economizer type parameters
	int const NoEconomizer( 1 );
	int const DifferentialDryBulb( 2 );
	int const DifferentialEnthalpy( 3 );
	// Heat recovery type parameters
	int const NoHeatRecovery( 1 );
	int const Sensible( 2 );
	int const Enthalpy( 3 );
	// Operating mode parameters
	int const Off( 0 );
	int const Heat( 1 );
	int const Cool( 2 );
	int const DeadBand( 3 );
	// Delta humidity ratio limit, 0.00025 equals delta between 45F dewpoint and 46F dewpoint
	// used to prevent dividing by near zero
	Real64 const SmallDeltaHumRat( 0.00025 );

	// DERIVED TYPE DEFINITIONS:

	//MODULE VARIABLE DECLARATIONS:

	int NumPurchAir;
	bool GetPurchAirInputFlag( true );
	Array1D_bool CheckEquipName;
	//SUBROUTINE SPECIFICATIONS FOR MODULE PurchasedAir:

	// Object Data
	Array1D< ZonePurchasedAir > PurchAir; // Used to specify purchased air parameters
	Array1D< PurchAirNumericFieldData > PurchAirNumericFields; // Used to save the indecies of scalable sizing object for zone HVAC

	// Functions

	void
	SimPurchasedAir(
		std::string const & PurchAirName,
		Real64 & SysOutputProvided,
		Real64 & MoistOutputProvided, // Moisture output provided (kg/s), dehumidification = negative
		bool const FirstHVACIteration,
		int const ControlledZoneNum,
		int const ActualZoneNum,
		int & CompIndex
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Russ Taylor
		//       DATE WRITTEN   May 1997
		//       MODIFIED       Don Shirey, Aug 2009 (LatOutputProvided - now MoistOutputProvided)
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine manages Purchased Air component simulation.
		// It is called from SimZoneEquipment in the ZoneEquipmentManager
		// at the system time step.

		// METHODOLOGY EMPLOYED:

		// REFERENCES:
		// na

		// Using/Aliasing
		using InputProcessor::FindItemInList;
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

		int PurchAirNum;

		// Beginning of Code

		if ( GetPurchAirInputFlag ) {
			GetPurchasedAir();
			GetPurchAirInputFlag = false;
		}

		// Find the correct PurchasedAir Equipment
		if ( CompIndex == 0 ) {
			PurchAirNum = FindItemInList( PurchAirName, PurchAir );
			if ( PurchAirNum == 0 ) {
				ShowFatalError( "SimPurchasedAir: Unit not found=" + PurchAirName );
			}
			CompIndex = PurchAirNum;
		} else {
			PurchAirNum = CompIndex;
			if ( PurchAirNum > NumPurchAir || PurchAirNum < 1 ) {
				ShowFatalError( "SimPurchasedAir:  Invalid CompIndex passed=" + TrimSigDigits( PurchAirNum ) + ", Number of Units=" + TrimSigDigits( NumPurchAir ) + ", Entered Unit name=" + PurchAirName );
			}
			if ( CheckEquipName( PurchAirNum ) ) {
				if ( PurchAirName != PurchAir( PurchAirNum ).Name ) {
					ShowFatalError( "SimPurchasedAir: Invalid CompIndex passed=" + TrimSigDigits( PurchAirNum ) + ", Unit name=" + PurchAirName + ", stored Unit Name for that index=" + PurchAir( PurchAirNum ).Name );
				}
				CheckEquipName( PurchAirNum ) = false;
			}
		}

		InitPurchasedAir( PurchAirNum, FirstHVACIteration, ControlledZoneNum, ActualZoneNum );

		CalcPurchAirLoads( PurchAirNum, SysOutputProvided, MoistOutputProvided, ControlledZoneNum, ActualZoneNum );

		UpdatePurchasedAir( PurchAirNum );

		ReportPurchasedAir( PurchAirNum );

	}

	void
	GetPurchasedAir()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Russ Taylor
		//       DATE WRITTEN   June 1997
		//       MODIFIED       M. Witte, June 2011, add new features including DCV, economizer, dehumidification
		//                                           and humidification controls
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Get the input data for the Purchased Air objects.
		// Set up output variables.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using InputProcessor::GetNumObjectsFound;
		using InputProcessor::GetObjectItem;
		using InputProcessor::VerifyName;
		using InputProcessor::SameString;
		using InputProcessor::FindItemInList;
		using NodeInputManager::GetOnlySingleNode;
		using NodeInputManager::InitUniqueNodeCheck;
		using NodeInputManager::CheckUniqueNodes;
		using NodeInputManager::EndUniqueNodeCheck;
		using OutAirNodeManager::CheckAndAddAirNodeNumber;
		using namespace DataLoopNode;
		using namespace DataIPShortCuts;
		using DataSizing::OARequirements; // to find DesignSpecification:OutdoorAir pointer
		using DataContaminantBalance::Contaminant;
		using DataSizing::ZoneHVACSizing;
		using DataZoneEquipment::ZoneEquipConfig;

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
		int PurchAirNum;
		int NumAlphas;
		int NumNums;
		int IOStat;
		int CtrlZone; // zone index
		int NodeNum; // node index
		static std::string const RoutineName( "GetPurchasedAir: " ); // include trailing blank space
		static bool ErrorsFound( false ); // If errors detected in input
		bool IsNotOK; // Flag to verify name
		bool IsBlank; // Flag for blank name
		bool IsOANodeListed; // Flag for OA node name listed in OutdoorAir:Node or Nodelist
		bool UniqueNodeError; // Flag for non-unique node error(s)

		cCurrentModuleObject = "ZoneHVAC:IdealLoadsAirSystem";

		NumPurchAir = GetNumObjectsFound( cCurrentModuleObject );

		PurchAir.allocate( NumPurchAir );
		CheckEquipName.allocate( NumPurchAir );
		PurchAirNumericFields.allocate( NumPurchAir );
		CheckEquipName = true;

		if ( NumPurchAir > 0 ) {
			InitUniqueNodeCheck( cCurrentModuleObject );
			for ( PurchAirNum = 1; PurchAirNum <= NumPurchAir; ++PurchAirNum ) {
				PurchAir( PurchAirNum ).cObjectName = cCurrentModuleObject;

				GetObjectItem( cCurrentModuleObject, PurchAirNum, cAlphaArgs, NumAlphas, rNumericArgs, NumNums, IOStat, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );

				PurchAirNumericFields( PurchAirNum ).FieldNames.allocate( NumNums );
				PurchAirNumericFields( PurchAirNum ).FieldNames = "";
				PurchAirNumericFields( PurchAirNum ).FieldNames = cNumericFieldNames;

				IsNotOK = false;
				IsBlank = false;
				VerifyName( cAlphaArgs( 1 ), PurchAir, PurchAirNum - 1, IsNotOK, IsBlank, cCurrentModuleObject + " Name" );
				if ( IsNotOK ) {
					ErrorsFound = true;
					if ( IsBlank ) cAlphaArgs( 1 ) = "xxxxx";
				}
				PurchAir( PurchAirNum ).Name = cAlphaArgs( 1 );
				// get optional  availability schedule
				PurchAir( PurchAirNum ).AvailSched = cAlphaArgs( 2 );
				if ( lAlphaFieldBlanks( 2 ) ) {
					PurchAir( PurchAirNum ).AvailSchedPtr = ScheduleAlwaysOn;
				} else {
					PurchAir( PurchAirNum ).AvailSchedPtr = GetScheduleIndex( cAlphaArgs( 2 ) );
					if ( PurchAir( PurchAirNum ).AvailSchedPtr == 0 ) {
						ShowSevereError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + " invalid data" );
						ShowContinueError( "Invalid-not found " + cAlphaFieldNames( 2 ) + "=\"" + cAlphaArgs( 2 ) + "\"." );
						ErrorsFound = true;
					}
				}
				// Purchased air supply air node is an outlet node
				PurchAir( PurchAirNum ).ZoneSupplyAirNodeNum = GetOnlySingleNode( cAlphaArgs( 3 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Air, NodeConnectionType_Outlet, 1, ObjectIsNotParent );
				UniqueNodeError = false;
				CheckUniqueNodes( cAlphaFieldNames( 3 ), "NodeName", UniqueNodeError, cAlphaArgs( 3 ), _, cAlphaArgs( 1 ) );
				if ( UniqueNodeError ) ErrorsFound = true;
				// If new (optional) exhaust air node name is present, then register it as inlet
				if ( ! lAlphaFieldBlanks( 4 ) ) {
					PurchAir( PurchAirNum ).ZoneExhaustAirNodeNum = GetOnlySingleNode( cAlphaArgs( 4 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Air, NodeConnectionType_Inlet, 1, ObjectIsNotParent );
					UniqueNodeError = false;
					CheckUniqueNodes( cAlphaFieldNames( 4 ), "NodeName", UniqueNodeError, cAlphaArgs( 4 ), _, cAlphaArgs( 1 ) );
					if ( UniqueNodeError ) ErrorsFound = true;
				}
				PurchAir( PurchAirNum ).MaxHeatSuppAirTemp = rNumericArgs( 1 );
				PurchAir( PurchAirNum ).MinCoolSuppAirTemp = rNumericArgs( 2 );
				PurchAir( PurchAirNum ).MaxHeatSuppAirHumRat = rNumericArgs( 3 );
				PurchAir( PurchAirNum ).MinCoolSuppAirHumRat = rNumericArgs( 4 );

				if ( SameString( cAlphaArgs( 5 ), "NoLimit" ) ) {
					PurchAir( PurchAirNum ).HeatingLimit = NoLimit;
				} else if ( SameString( cAlphaArgs( 5 ), "LimitFlowRate" ) ) {
					if ( lNumericFieldBlanks( 5 ) ) {
						PurchAir( PurchAirNum ).HeatingLimit = NoLimit;
					} else {
						PurchAir( PurchAirNum ).HeatingLimit = LimitFlowRate;
					}
				} else if ( SameString( cAlphaArgs( 5 ), "LimitCapacity" ) ) {
					if ( lNumericFieldBlanks( 6 ) ) {
						PurchAir( PurchAirNum ).HeatingLimit = NoLimit;
					} else {
						PurchAir( PurchAirNum ).HeatingLimit = LimitCapacity;
					}
				} else if ( SameString( cAlphaArgs( 5 ), "LimitFlowRateAndCapacity" ) ) {
					if ( lNumericFieldBlanks( 5 ) && lNumericFieldBlanks( 6 ) ) {
						PurchAir( PurchAirNum ).HeatingLimit = NoLimit;
					} else if ( lNumericFieldBlanks( 5 ) ) {
						PurchAir( PurchAirNum ).HeatingLimit = LimitCapacity;
					} else if ( lNumericFieldBlanks( 6 ) ) {
						PurchAir( PurchAirNum ).HeatingLimit = LimitFlowRate;
					} else {
						PurchAir( PurchAirNum ).HeatingLimit = LimitFlowRateAndCapacity;
					}
				} else {
					ShowSevereError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + " invalid data" );
					ShowContinueError( "Invalid-entry " + cAlphaFieldNames( 5 ) + "=\"" + cAlphaArgs( 5 ) + "\"." );
					ShowContinueError( "Valid entries are NoLimit, LimitFlowRate, LimitCapacity, or LimitFlowRateAndCapacity" );
					ErrorsFound = true;
				}
				PurchAir( PurchAirNum ).MaxHeatVolFlowRate = rNumericArgs( 5 );
				PurchAir( PurchAirNum ).MaxHeatSensCap = rNumericArgs( 6 );

				if ( SameString( cAlphaArgs( 6 ), "NoLimit" ) ) {
					PurchAir( PurchAirNum ).CoolingLimit = NoLimit;
				} else if ( SameString( cAlphaArgs( 6 ), "LimitFlowRate" ) ) {
					if ( lNumericFieldBlanks( 7 ) ) {
						PurchAir( PurchAirNum ).CoolingLimit = NoLimit;
					} else {
						PurchAir( PurchAirNum ).CoolingLimit = LimitFlowRate;
					}
				} else if ( SameString( cAlphaArgs( 6 ), "LimitCapacity" ) ) {
					if ( lNumericFieldBlanks( 8 ) ) {
						PurchAir( PurchAirNum ).CoolingLimit = NoLimit;
					} else {
						PurchAir( PurchAirNum ).CoolingLimit = LimitCapacity;
					}
				} else if ( SameString( cAlphaArgs( 6 ), "LimitFlowRateAndCapacity" ) ) {
					if ( lNumericFieldBlanks( 7 ) && lNumericFieldBlanks( 8 ) ) {
						PurchAir( PurchAirNum ).CoolingLimit = NoLimit;
					} else if ( lNumericFieldBlanks( 7 ) ) {
						PurchAir( PurchAirNum ).CoolingLimit = LimitCapacity;
					} else if ( lNumericFieldBlanks( 8 ) ) {
						PurchAir( PurchAirNum ).CoolingLimit = LimitFlowRate;
					} else {
						PurchAir( PurchAirNum ).CoolingLimit = LimitFlowRateAndCapacity;
					}
				} else {
					ShowSevereError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + " invalid data" );
					ShowContinueError( "Invalid-entry " + cAlphaFieldNames( 6 ) + "=\"" + cAlphaArgs( 6 ) + "\"." );
					ShowContinueError( "Valid entries are NoLimit, LimitFlowRate, LimitCapacity, or LimitFlowRateAndCapacity" );
					ErrorsFound = true;
				}
				PurchAir( PurchAirNum ).MaxCoolVolFlowRate = rNumericArgs( 7 );
				PurchAir( PurchAirNum ).MaxCoolTotCap = rNumericArgs( 8 );

				// get optional heating availability schedule
				PurchAir( PurchAirNum ).HeatSched = cAlphaArgs( 7 );
				if ( lAlphaFieldBlanks( 7 ) ) {
					PurchAir( PurchAirNum ).HeatSchedPtr = ScheduleAlwaysOn;
				} else {
					PurchAir( PurchAirNum ).HeatSchedPtr = GetScheduleIndex( cAlphaArgs( 7 ) );
					if ( PurchAir( PurchAirNum ).HeatSchedPtr == 0 ) {
						ShowSevereError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + " invalid data" );
						ShowContinueError( "Invalid-not found " + cAlphaFieldNames( 7 ) + "=\"" + cAlphaArgs( 7 ) + "\"." );
						ErrorsFound = true;
					}
				}
				// get optional cooling availability schedule
				PurchAir( PurchAirNum ).CoolSched = cAlphaArgs( 8 );
				if ( lAlphaFieldBlanks( 8 ) ) {
					PurchAir( PurchAirNum ).CoolSchedPtr = ScheduleAlwaysOn;
				} else {
					PurchAir( PurchAirNum ).CoolSchedPtr = GetScheduleIndex( cAlphaArgs( 8 ) );
					if ( PurchAir( PurchAirNum ).CoolSchedPtr == 0 ) {
						ShowSevereError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + " invalid data" );
						ShowContinueError( "Invalid-not found " + cAlphaFieldNames( 8 ) + "=\"" + cAlphaArgs( 8 ) + "\"." );
						ErrorsFound = true;
					}
				}
				// get Dehumidification control type
				if ( SameString( cAlphaArgs( 9 ), "None" ) ) {
					PurchAir( PurchAirNum ).DehumidCtrlType = None;
				} else if ( SameString( cAlphaArgs( 9 ), "ConstantSensibleHeatRatio" ) ) {
					PurchAir( PurchAirNum ).DehumidCtrlType = ConstantSensibleHeatRatio;
				} else if ( SameString( cAlphaArgs( 9 ), "Humidistat" ) ) {
					PurchAir( PurchAirNum ).DehumidCtrlType = Humidistat;
				} else if ( SameString( cAlphaArgs( 9 ), "ConstantSupplyHumidityRatio" ) ) {
					PurchAir( PurchAirNum ).DehumidCtrlType = ConstantSupplyHumidityRatio;
				} else {
					ShowSevereError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + " invalid data" );
					ShowContinueError( "Invalid-entry " + cAlphaFieldNames( 9 ) + "=\"" + cAlphaArgs( 9 ) + "\"." );
					ShowContinueError( "Valid entries are ConstantSensibleHeatRatio, Humidistat, or ConstantSupplyHumidityRatio" );
					ErrorsFound = true;
				}
				PurchAir( PurchAirNum ).CoolSHR = rNumericArgs( 9 );

				// get Humidification control type
				if ( SameString( cAlphaArgs( 10 ), "None" ) ) {
					PurchAir( PurchAirNum ).HumidCtrlType = None;
				} else if ( SameString( cAlphaArgs( 10 ), "Humidistat" ) ) {
					PurchAir( PurchAirNum ).HumidCtrlType = Humidistat;
				} else if ( SameString( cAlphaArgs( 10 ), "ConstantSupplyHumidityRatio" ) ) {
					PurchAir( PurchAirNum ).HumidCtrlType = ConstantSupplyHumidityRatio;
				} else {
					ShowSevereError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + " invalid data" );
					ShowContinueError( "Invalid-entry " + cAlphaFieldNames( 10 ) + "=\"" + cAlphaArgs( 10 ) + "\"." );
					ShowContinueError( "Valid entries are None, Humidistat, or ConstantSupplyHumidityRatio" );
					ErrorsFound = true;
				}

				// get Design specification outdoor air object
				if ( ! lAlphaFieldBlanks( 11 ) ) {
					PurchAir( PurchAirNum ).OARequirementsPtr = FindItemInList( cAlphaArgs( 11 ), OARequirements );
					if ( PurchAir( PurchAirNum ).OARequirementsPtr == 0 ) {
						ShowSevereError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + " invalid data" );
						ShowContinueError( "Invalid-not found" + cAlphaFieldNames( 11 ) + "=\"" + cAlphaArgs( 11 ) + "\"." );
						ErrorsFound = true;
					} else {
						PurchAir( PurchAirNum ).OutdoorAir = true;
					}
				}

				// If outdoor air specified, then get Outdoor air inlet node and other outdoor air inputs
				if ( PurchAir( PurchAirNum ).OutdoorAir ) {
					if ( lAlphaFieldBlanks( 12 ) ) {
						// If there is outdoor air and outdoor air inlet node is blank, then create one
						if ( len( cAlphaArgs( 1 ) ) < MaxNameLength - 23 ) { // protect against long name leading to > 100 chars
							cAlphaArgs( 12 ) = cAlphaArgs( 1 ) + " OUTDOOR AIR INLET NODE";
						} else {
							cAlphaArgs( 12 ) = cAlphaArgs( 1 ).substr( 0, 75 ) + " OUTDOOR AIR INLET NODE";
						}
						if ( DisplayExtraWarnings ) {
							ShowWarningError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + " blank field" );
							ShowContinueError( cAlphaFieldNames( 12 ) + " is blank, but there is outdoor air requested for this system." );
							ShowContinueError( "Creating node name =" + cAlphaArgs( 12 ) );
						}
					}
					// Register OA node
					PurchAir( PurchAirNum ).OutdoorAirNodeNum = GetOnlySingleNode( cAlphaArgs( 12 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Air, NodeConnectionType_Outlet, 1, ObjectIsNotParent );
					// Check if OA node is initialized in OutdoorAir:Node or OutdoorAir:Nodelist
					CheckAndAddAirNodeNumber( PurchAir( PurchAirNum ).OutdoorAirNodeNum, IsOANodeListed );
					if ( ( ! IsOANodeListed ) && DisplayExtraWarnings ) {
						ShowWarningError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + " missing data" );
						ShowContinueError( cAlphaArgs( 12 ) + " does not appear in an OutdoorAir:NodeList or as an OutdoorAir:Node." );
						ShowContinueError( "Adding OutdoorAir:Node=" + cAlphaArgs( 12 ) );
					}
					UniqueNodeError = false;
					CheckUniqueNodes( cAlphaFieldNames( 12 ), "NodeName", UniqueNodeError, cAlphaArgs( 12 ), _, cAlphaArgs( 1 ) );
					if ( UniqueNodeError ) ErrorsFound = true;

					// get Demand controlled ventilation type
					if ( SameString( cAlphaArgs( 13 ), "None" ) ) {
						PurchAir( PurchAirNum ).DCVType = NoDCV;
					} else if ( SameString( cAlphaArgs( 13 ), "OccupancySchedule" ) ) {
						PurchAir( PurchAirNum ).DCVType = OccupancySchedule;
					} else if ( SameString( cAlphaArgs( 13 ), "CO2Setpoint" ) ) {
						if ( Contaminant.CO2Simulation ) {
							PurchAir( PurchAirNum ).DCVType = CO2SetPoint;
						} else {
							PurchAir( PurchAirNum ).DCVType = NoDCV;
							ShowWarningError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + " invalid data" );
							ShowContinueError( cAlphaFieldNames( 13 ) + '=' + cAlphaArgs( 13 ) + " but CO2 simulation is not active." );
							ShowContinueError( "Resetting " + cAlphaFieldNames( 13 ) + " to NoDCV" );
							ShowContinueError( "To activate CO2 simulation, use ZoneAirContaminantBalance object and specify \"Carbon Dioxide Concentration\"=\"Yes\"." );
						}
					} else {
						ShowSevereError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + " invalid data" );
						ShowContinueError( "Invalid-entry " + cAlphaFieldNames( 13 ) + '=' + cAlphaArgs( 13 ) );
						ShowContinueError( "Valid entries are None, OccupancySchedule, or CO2Setpoint" );
						ErrorsFound = true;
					}
					// get Outdoor air economizer type
					if ( SameString( cAlphaArgs( 14 ), "NoEconomizer" ) ) {
						PurchAir( PurchAirNum ).EconomizerType = NoEconomizer;
					} else if ( SameString( cAlphaArgs( 14 ), "DifferentialDryBulb" ) ) {
						PurchAir( PurchAirNum ).EconomizerType = DifferentialDryBulb;
					} else if ( SameString( cAlphaArgs( 14 ), "DifferentialEnthalpy" ) ) {
						PurchAir( PurchAirNum ).EconomizerType = DifferentialEnthalpy;
					} else {
						ShowSevereError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + " invalid data" );
						ShowContinueError( "Invalid-entry " + cAlphaFieldNames( 14 ) + '=' + cAlphaArgs( 14 ) );
						ShowContinueError( "Valid entries are NoEconomizer, DifferentialDryBulb, or DifferentialEnthalpy" );
						ErrorsFound = true;
					}
					// get Outdoor air heat recovery type and effectiveness
					if ( SameString( cAlphaArgs( 15 ), "None" ) ) {
						PurchAir( PurchAirNum ).HtRecType = NoHeatRecovery;
					} else if ( SameString( cAlphaArgs( 15 ), "Sensible" ) ) {
						PurchAir( PurchAirNum ).HtRecType = Sensible;
					} else if ( SameString( cAlphaArgs( 15 ), "Enthalpy" ) ) {
						PurchAir( PurchAirNum ).HtRecType = Enthalpy;
					} else {
						ShowSevereError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + " invalid data" );
						ShowContinueError( "Invalid-entry " + cAlphaFieldNames( 15 ) + '=' + cAlphaArgs( 15 ) );
						ShowContinueError( "Valid entries are None, Sensible, or Enthalpy" );
						ErrorsFound = true;
					}
				} else { //No outdoorair
					PurchAir( PurchAirNum ).DCVType = NoDCV;
					PurchAir( PurchAirNum ).EconomizerType = NoEconomizer;
					PurchAir( PurchAirNum ).HtRecType = NoHeatRecovery;
				}

				PurchAir( PurchAirNum ).HtRecSenEff = rNumericArgs( 10 );
				PurchAir( PurchAirNum ).HtRecLatEff = rNumericArgs( 11 );

				for ( CtrlZone = 1; CtrlZone <= NumOfZones; ++CtrlZone ) {
					if ( ! ZoneEquipConfig( CtrlZone ).IsControlled ) continue;
					for ( NodeNum = 1; NodeNum <= ZoneEquipConfig( CtrlZone ).NumInletNodes; ++NodeNum ) {
						if (PurchAir( PurchAirNum ).ZoneSupplyAirNodeNum == ZoneEquipConfig( CtrlZone ).InletNode( NodeNum ) ) {
							PurchAir( PurchAirNum ).ZonePtr = CtrlZone;
						}
					}
				}

				PurchAir( PurchAirNum ).HVACSizingIndex = 0;
				if ( ! lAlphaFieldBlanks( 16 ) ) {
					PurchAir(PurchAirNum).HVACSizingIndex = FindItemInList( cAlphaArgs( 16 ), ZoneHVACSizing );
					if ( PurchAir(PurchAirNum).HVACSizingIndex == 0 ) {
						ShowSevereError( cAlphaFieldNames( 16 ) + " = " + cAlphaArgs( 16 ) + " not found.");
						ShowContinueError( "Occurs in " + cCurrentModuleObject + " = " + PurchAir( PurchAirNum ).Name);
						ErrorsFound = true;
					}
				}

				// initialize the calculated and report values
				PurchAir( PurchAirNum ).MaxHeatMassFlowRate = 0.0;
				PurchAir( PurchAirNum ).MaxCoolMassFlowRate = 0.0;
				PurchAir( PurchAirNum ).SenHeatEnergy = 0.0;
				PurchAir( PurchAirNum ).LatHeatEnergy = 0.0;
				PurchAir( PurchAirNum ).TotHeatEnergy = 0.0;
				PurchAir( PurchAirNum ).SenCoolEnergy = 0.0;
				PurchAir( PurchAirNum ).LatCoolEnergy = 0.0;
				PurchAir( PurchAirNum ).TotCoolEnergy = 0.0;
				PurchAir( PurchAirNum ).ZoneSenHeatEnergy = 0.0;
				PurchAir( PurchAirNum ).ZoneLatHeatEnergy = 0.0;
				PurchAir( PurchAirNum ).ZoneTotHeatEnergy = 0.0;
				PurchAir( PurchAirNum ).ZoneSenCoolEnergy = 0.0;
				PurchAir( PurchAirNum ).ZoneLatCoolEnergy = 0.0;
				PurchAir( PurchAirNum ).ZoneTotCoolEnergy = 0.0;
				PurchAir( PurchAirNum ).OASenHeatEnergy = 0.0;
				PurchAir( PurchAirNum ).OALatHeatEnergy = 0.0;
				PurchAir( PurchAirNum ).OATotHeatEnergy = 0.0;
				PurchAir( PurchAirNum ).OASenCoolEnergy = 0.0;
				PurchAir( PurchAirNum ).OALatCoolEnergy = 0.0;
				PurchAir( PurchAirNum ).OATotCoolEnergy = 0.0;
				PurchAir( PurchAirNum ).HtRecSenHeatEnergy = 0.0;
				PurchAir( PurchAirNum ).HtRecLatHeatEnergy = 0.0;
				PurchAir( PurchAirNum ).HtRecTotHeatEnergy = 0.0;
				PurchAir( PurchAirNum ).HtRecSenCoolEnergy = 0.0;
				PurchAir( PurchAirNum ).HtRecLatCoolEnergy = 0.0;
				PurchAir( PurchAirNum ).HtRecTotCoolEnergy = 0.0;
				PurchAir( PurchAirNum ).SenHeatRate = 0.0;
				PurchAir( PurchAirNum ).LatHeatRate = 0.0;
				PurchAir( PurchAirNum ).TotHeatRate = 0.0;
				PurchAir( PurchAirNum ).SenCoolRate = 0.0;
				PurchAir( PurchAirNum ).LatCoolRate = 0.0;
				PurchAir( PurchAirNum ).TotCoolRate = 0.0;
				PurchAir( PurchAirNum ).ZoneSenHeatRate = 0.0;
				PurchAir( PurchAirNum ).ZoneLatHeatRate = 0.0;
				PurchAir( PurchAirNum ).ZoneTotHeatRate = 0.0;
				PurchAir( PurchAirNum ).ZoneSenCoolRate = 0.0;
				PurchAir( PurchAirNum ).ZoneLatCoolRate = 0.0;
				PurchAir( PurchAirNum ).ZoneTotCoolRate = 0.0;
				PurchAir( PurchAirNum ).OASenHeatRate = 0.0;
				PurchAir( PurchAirNum ).OALatHeatRate = 0.0;
				PurchAir( PurchAirNum ).OATotHeatRate = 0.0;
				PurchAir( PurchAirNum ).OASenCoolRate = 0.0;
				PurchAir( PurchAirNum ).OALatCoolRate = 0.0;
				PurchAir( PurchAirNum ).OATotCoolRate = 0.0;
				PurchAir( PurchAirNum ).HtRecSenHeatRate = 0.0;
				PurchAir( PurchAirNum ).HtRecLatHeatRate = 0.0;
				PurchAir( PurchAirNum ).HtRecTotHeatRate = 0.0;
				PurchAir( PurchAirNum ).HtRecSenCoolRate = 0.0;
				PurchAir( PurchAirNum ).HtRecLatCoolRate = 0.0;
				PurchAir( PurchAirNum ).HtRecTotCoolRate = 0.0;

				PurchAir( PurchAirNum ).OutdoorAirMassFlowRate = 0.0;
				PurchAir( PurchAirNum ).OutdoorAirVolFlowRateStdRho = 0.0;
				PurchAir( PurchAirNum ).SupplyAirMassFlowRate = 0.0;
				PurchAir( PurchAirNum ).SupplyAirVolFlowRateStdRho = 0.0;

			}
			EndUniqueNodeCheck( cCurrentModuleObject );
		}

		for ( PurchAirNum = 1; PurchAirNum <= NumPurchAir; ++PurchAirNum ) {

			// Setup Output variables
			//    energy variables
			SetupOutputVariable( "Zone Ideal Loads Supply Air Sensible Heating Energy [J]", PurchAir( PurchAirNum ).SenHeatEnergy, "System", "Sum", PurchAir( PurchAirNum ).Name );
			SetupOutputVariable( "Zone Ideal Loads Supply Air Latent Heating Energy [J]", PurchAir( PurchAirNum ).LatHeatEnergy, "System", "Sum", PurchAir( PurchAirNum ).Name );
			SetupOutputVariable( "Zone Ideal Loads Supply Air Total Heating Energy [J]", PurchAir( PurchAirNum ).TotHeatEnergy, "System", "Sum", PurchAir( PurchAirNum ).Name, _, "DISTRICTHEATING", "Heating", _, "System" );
			SetupOutputVariable( "Zone Ideal Loads Supply Air Sensible Cooling Energy [J]", PurchAir( PurchAirNum ).SenCoolEnergy, "System", "Sum", PurchAir( PurchAirNum ).Name );
			SetupOutputVariable( "Zone Ideal Loads Supply Air Latent Cooling Energy [J]", PurchAir( PurchAirNum ).LatCoolEnergy, "System", "Sum", PurchAir( PurchAirNum ).Name );
			SetupOutputVariable( "Zone Ideal Loads Supply Air Total Cooling Energy [J]", PurchAir( PurchAirNum ).TotCoolEnergy, "System", "Sum", PurchAir( PurchAirNum ).Name, _, "DISTRICTCOOLING", "Cooling", _, "System" );
			SetupOutputVariable( "Zone Ideal Loads Zone Sensible Heating Energy [J]", PurchAir( PurchAirNum ).ZoneSenHeatEnergy, "System", "Sum", PurchAir( PurchAirNum ).Name );
			SetupOutputVariable( "Zone Ideal Loads Zone Latent Heating Energy [J]", PurchAir( PurchAirNum ).ZoneLatHeatEnergy, "System", "Sum", PurchAir( PurchAirNum ).Name );
			SetupOutputVariable( "Zone Ideal Loads Zone Total Heating Energy [J]", PurchAir( PurchAirNum ).ZoneTotHeatEnergy, "System", "Sum", PurchAir( PurchAirNum ).Name );
			SetupOutputVariable( "Zone Ideal Loads Zone Sensible Cooling Energy [J]", PurchAir( PurchAirNum ).ZoneSenCoolEnergy, "System", "Sum", PurchAir( PurchAirNum ).Name );
			SetupOutputVariable( "Zone Ideal Loads Zone Latent Cooling Energy [J]", PurchAir( PurchAirNum ).ZoneLatCoolEnergy, "System", "Sum", PurchAir( PurchAirNum ).Name );
			SetupOutputVariable( "Zone Ideal Loads Zone Total Cooling Energy [J]", PurchAir( PurchAirNum ).ZoneTotCoolEnergy, "System", "Sum", PurchAir( PurchAirNum ).Name );
			SetupOutputVariable( "Zone Ideal Loads Outdoor Air Sensible Heating Energy [J]", PurchAir( PurchAirNum ).OASenHeatEnergy, "System", "Sum", PurchAir( PurchAirNum ).Name );
			SetupOutputVariable( "Zone Ideal Loads Outdoor Air Latent Heating Energy [J]", PurchAir( PurchAirNum ).OALatHeatEnergy, "System", "Sum", PurchAir( PurchAirNum ).Name );
			SetupOutputVariable( "Zone Ideal Loads Outdoor Air Total Heating Energy [J]", PurchAir( PurchAirNum ).OATotHeatEnergy, "System", "Sum", PurchAir( PurchAirNum ).Name );
			SetupOutputVariable( "Zone Ideal Loads Outdoor Air Sensible Cooling Energy [J]", PurchAir( PurchAirNum ).OASenCoolEnergy, "System", "Sum", PurchAir( PurchAirNum ).Name );
			SetupOutputVariable( "Zone Ideal Loads Outdoor Air Latent Cooling Energy [J]", PurchAir( PurchAirNum ).OALatCoolEnergy, "System", "Sum", PurchAir( PurchAirNum ).Name );
			SetupOutputVariable( "Zone Ideal Loads Outdoor Air Total Cooling Energy [J]", PurchAir( PurchAirNum ).OATotCoolEnergy, "System", "Sum", PurchAir( PurchAirNum ).Name );
			SetupOutputVariable( "Zone Ideal Loads Heat Recovery Sensible Heating Energy [J]", PurchAir( PurchAirNum ).HtRecSenHeatEnergy, "System", "Sum", PurchAir( PurchAirNum ).Name );
			SetupOutputVariable( "Zone Ideal Loads Heat Recovery Latent Heating Energy [J]", PurchAir( PurchAirNum ).HtRecLatHeatEnergy, "System", "Sum", PurchAir( PurchAirNum ).Name );
			SetupOutputVariable( "Zone Ideal Loads Heat Recovery Total Heating Energy [J]", PurchAir( PurchAirNum ).HtRecTotHeatEnergy, "System", "Sum", PurchAir( PurchAirNum ).Name );
			SetupOutputVariable( "Zone Ideal Loads Heat Recovery Sensible Cooling Energy [J]", PurchAir( PurchAirNum ).HtRecSenCoolEnergy, "System", "Sum", PurchAir( PurchAirNum ).Name );
			SetupOutputVariable( "Zone Ideal Loads Heat Recovery Latent Cooling Energy [J]", PurchAir( PurchAirNum ).HtRecLatCoolEnergy, "System", "Sum", PurchAir( PurchAirNum ).Name );
			SetupOutputVariable( "Zone Ideal Loads Heat Recovery Total Cooling Energy [J]", PurchAir( PurchAirNum ).HtRecTotCoolEnergy, "System", "Sum", PurchAir( PurchAirNum ).Name );

			//    rate variables
			SetupOutputVariable( "Zone Ideal Loads Supply Air Sensible Heating Rate [W]", PurchAir( PurchAirNum ).SenHeatRate, "System", "Average", PurchAir( PurchAirNum ).Name );
			SetupOutputVariable( "Zone Ideal Loads Supply Air Latent Heating Rate [W]", PurchAir( PurchAirNum ).LatHeatRate, "System", "Average", PurchAir( PurchAirNum ).Name );
			SetupOutputVariable( "Zone Ideal Loads Supply Air Total Heating Rate [W]", PurchAir( PurchAirNum ).TotHeatRate, "System", "Average", PurchAir( PurchAirNum ).Name );
			SetupOutputVariable( "Zone Ideal Loads Supply Air Sensible Cooling Rate [W]", PurchAir( PurchAirNum ).SenCoolRate, "System", "Average", PurchAir( PurchAirNum ).Name );
			SetupOutputVariable( "Zone Ideal Loads Supply Air Latent Cooling Rate [W]", PurchAir( PurchAirNum ).LatCoolRate, "System", "Average", PurchAir( PurchAirNum ).Name );
			SetupOutputVariable( "Zone Ideal Loads Supply Air Total Cooling Rate [W]", PurchAir( PurchAirNum ).TotCoolRate, "System", "Average", PurchAir( PurchAirNum ).Name );
			SetupOutputVariable( "Zone Ideal Loads Zone Sensible Heating Rate [W]", PurchAir( PurchAirNum ).ZoneSenHeatRate, "System", "Average", PurchAir( PurchAirNum ).Name );
			SetupOutputVariable( "Zone Ideal Loads Zone Latent Heating Rate [W]", PurchAir( PurchAirNum ).ZoneLatHeatRate, "System", "Average", PurchAir( PurchAirNum ).Name );
			SetupOutputVariable( "Zone Ideal Loads Zone Total Heating Rate [W]", PurchAir( PurchAirNum ).ZoneTotHeatRate, "System", "Average", PurchAir( PurchAirNum ).Name );
			SetupOutputVariable( "Zone Ideal Loads Zone Sensible Cooling Rate [W]", PurchAir( PurchAirNum ).ZoneSenCoolRate, "System", "Average", PurchAir( PurchAirNum ).Name );
			SetupOutputVariable( "Zone Ideal Loads Zone Latent Cooling Rate [W]", PurchAir( PurchAirNum ).ZoneLatCoolRate, "System", "Average", PurchAir( PurchAirNum ).Name );
			SetupOutputVariable( "Zone Ideal Loads Zone Total Cooling Rate [W]", PurchAir( PurchAirNum ).ZoneTotCoolRate, "System", "Average", PurchAir( PurchAirNum ).Name );
			SetupOutputVariable( "Zone Ideal Loads Outdoor Air Sensible Heating Rate [W]", PurchAir( PurchAirNum ).OASenHeatRate, "System", "Average", PurchAir( PurchAirNum ).Name );
			SetupOutputVariable( "Zone Ideal Loads Outdoor Air Latent Heating Rate [W]", PurchAir( PurchAirNum ).OALatHeatRate, "System", "Average", PurchAir( PurchAirNum ).Name );
			SetupOutputVariable( "Zone Ideal Loads Outdoor Air Total Heating Rate [W]", PurchAir( PurchAirNum ).OATotHeatRate, "System", "Average", PurchAir( PurchAirNum ).Name );
			SetupOutputVariable( "Zone Ideal Loads Outdoor Air Sensible Cooling Rate [W]", PurchAir( PurchAirNum ).OASenCoolRate, "System", "Average", PurchAir( PurchAirNum ).Name );
			SetupOutputVariable( "Zone Ideal Loads Outdoor Air Latent Cooling Rate [W]", PurchAir( PurchAirNum ).OALatCoolRate, "System", "Average", PurchAir( PurchAirNum ).Name );
			SetupOutputVariable( "Zone Ideal Loads Outdoor Air Total Cooling Rate [W]", PurchAir( PurchAirNum ).OATotCoolRate, "System", "Average", PurchAir( PurchAirNum ).Name );
			SetupOutputVariable( "Zone Ideal Loads Heat Recovery Sensible Heating Rate [W]", PurchAir( PurchAirNum ).HtRecSenHeatRate, "System", "Average", PurchAir( PurchAirNum ).Name );
			SetupOutputVariable( "Zone Ideal Loads Heat Recovery Latent Heating Rate [W]", PurchAir( PurchAirNum ).HtRecLatHeatRate, "System", "Average", PurchAir( PurchAirNum ).Name );
			SetupOutputVariable( "Zone Ideal Loads Heat Recovery Total Heating Rate [W]", PurchAir( PurchAirNum ).HtRecTotHeatRate, "System", "Average", PurchAir( PurchAirNum ).Name );
			SetupOutputVariable( "Zone Ideal Loads Heat Recovery Sensible Cooling Rate [W]", PurchAir( PurchAirNum ).HtRecSenCoolRate, "System", "Average", PurchAir( PurchAirNum ).Name );
			SetupOutputVariable( "Zone Ideal Loads Heat Recovery Latent Cooling Rate [W]", PurchAir( PurchAirNum ).HtRecLatCoolRate, "System", "Average", PurchAir( PurchAirNum ).Name );
			SetupOutputVariable( "Zone Ideal Loads Heat Recovery Total Cooling Rate [W]", PurchAir( PurchAirNum ).HtRecTotCoolRate, "System", "Average", PurchAir( PurchAirNum ).Name );

			SetupOutputVariable( "Zone Ideal Loads Economizer Active Time [hr]", PurchAir( PurchAirNum ).TimeEconoActive, "System", "Sum", PurchAir( PurchAirNum ).Name );
			SetupOutputVariable( "Zone Ideal Loads Heat Recovery Active Time [hr]", PurchAir( PurchAirNum ).TimeHtRecActive, "System", "Sum", PurchAir( PurchAirNum ).Name );

			SetupOutputVariable( "Zone Ideal Loads Hybrid Ventilation Available Status []", PurchAir( PurchAirNum ).AvailStatus, "System", "Average", PurchAir( PurchAirNum ).Name );

			//air flows
			SetupOutputVariable( "Zone Ideal Loads Outdoor Air Mass Flow Rate [kg/s]", PurchAir( PurchAirNum ).OutdoorAirMassFlowRate, "System", "Average", PurchAir( PurchAirNum ).Name );
			SetupOutputVariable( "Zone Ideal Loads Outdoor Air Standard Density Volume Flow Rate [m3/s]", PurchAir( PurchAirNum ).OutdoorAirVolFlowRateStdRho, "System", "Average", PurchAir( PurchAirNum ).Name );
			SetupOutputVariable( "Zone Ideal Loads Supply Air Mass Flow Rate [kg/s]", PurchAir( PurchAirNum ).SupplyAirMassFlowRate, "System", "Average", PurchAir( PurchAirNum ).Name );
			SetupOutputVariable( "Zone Ideal Loads Supply Air Standard Density Volume Flow Rate [m3/s]", PurchAir( PurchAirNum ).SupplyAirVolFlowRateStdRho, "System", "Average", PurchAir( PurchAirNum ).Name );

			if ( AnyEnergyManagementSystemInModel ) {
				SetupEMSActuator( "Ideal Loads Air System", PurchAir( PurchAirNum ).Name, "Air Mass Flow Rate", "[kg/s]", PurchAir( PurchAirNum ).EMSOverrideMdotOn, PurchAir( PurchAirNum ).EMSValueMassFlowRate );
				SetupEMSActuator( "Ideal Loads Air System", PurchAir( PurchAirNum ).Name, "Outdoor Air Mass Flow Rate", "[kg/s]", PurchAir( PurchAirNum ).EMSOverrideOAMdotOn, PurchAir( PurchAirNum ).EMSValueOAMassFlowRate );
				SetupEMSActuator( "Ideal Loads Air System", PurchAir( PurchAirNum ).Name, "Air Temperature", "[C]", PurchAir( PurchAirNum ).EMSOverrideSupplyTempOn, PurchAir( PurchAirNum ).EMSValueSupplyTemp );
				SetupEMSActuator( "Ideal Loads Air System", PurchAir( PurchAirNum ).Name, "Air Humidity Ratio", "[kgWater/kgDryAir]", PurchAir( PurchAirNum ).EMSOverrideSupplyHumRatOn, PurchAir( PurchAirNum ).EMSValueSupplyHumRat );

			}
		}

		if ( ErrorsFound ) {
			ShowFatalError( RoutineName + "Errors found in input. Preceding conditions cause termination." );
		}

	}

	void
	InitPurchasedAir(
		int const PurchAirNum,
		bool const EP_UNUSED( FirstHVACIteration ), // unused1208
		int const ControlledZoneNum,
		int const ActualZoneNum
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Russ Taylor
		//       DATE WRITTEN   Nov 1997
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Initialize the PurchAir data structure.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using General::RoundSigDigits;
		using DataZoneEquipment::ZoneEquipInputsFilled;
		using DataZoneEquipment::CheckZoneEquipmentList;
		using DataZoneEquipment::ZoneEquipConfig;
		using DataSizing::OARequirements; // to access DesignSpecification:OutdoorAir inputs
		using DataHeatBalance::Zone; // to access zone area, volume, and multipliers
		using General::FindNumberInList;
		using DataLoopNode::NodeID;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		static bool MyOneTimeFlag( true );
		static bool ZoneEquipmentListChecked( false ); // True after the Zone Equipment List has been checked for items
		int Loop;
		static Array1D_bool MyEnvrnFlag;
		static Array1D_bool MySizeFlag;
		static Array1D_bool OneTimeUnitInitsDone; // True if one-time inits for PurchAirNum are completed
		//      LOGICAL :: ErrorsFound = .FALSE.   ! If errors detected in input
		bool UnitOn; // simple checks for error
		bool CoolOn; // simple checks for error
		bool HeatOn; // simple checks for error
		int SupplyNodeNum; // Node number for ideal loads supply node
		int ExhaustNodeNum; // Node number for ideal loads exhaust node
		int NodeIndex; // Array index of zone inlet or zone exhaust node that matches ideal loads node
		bool UseReturnNode; // simple checks for error

		// Do the Begin Simulation initializations
		if ( MyOneTimeFlag ) {

			MyEnvrnFlag.allocate( NumPurchAir );
			MySizeFlag.allocate( NumPurchAir );
			OneTimeUnitInitsDone.allocate( NumPurchAir );
			MyEnvrnFlag = true;
			MySizeFlag = true;
			OneTimeUnitInitsDone = false;
			MyOneTimeFlag = false;

		}

		// need to check all units to see if they are on Zone Equipment List or issue warning
		if ( ! ZoneEquipmentListChecked && ZoneEquipInputsFilled ) {
			ZoneEquipmentListChecked = true;
			for ( Loop = 1; Loop <= NumPurchAir; ++Loop ) {
				if ( CheckZoneEquipmentList( PurchAir( Loop ).cObjectName, PurchAir( Loop ).Name ) ) continue;
				ShowSevereError( "InitPurchasedAir: " + PurchAir( Loop ).cObjectName + " = " + PurchAir( Loop ).Name + " is not on any ZoneHVAC:EquipmentList.  It will not be simulated." );
			}
		}

		// one time inits for each unit - links PurchAirNum with static input data from ControlledZoneNum and ActualZoneNum
		if ( ! OneTimeUnitInitsDone( PurchAirNum ) ) {
			OneTimeUnitInitsDone( PurchAirNum ) = true;

			// Is the supply node really a zone inlet node?
			// this check has to be done here because of SimPurchasedAir passing in ControlledZoneNum
			SupplyNodeNum = PurchAir( PurchAirNum ).ZoneSupplyAirNodeNum;
			if ( SupplyNodeNum > 0 ) {
				NodeIndex = FindNumberInList( SupplyNodeNum, ZoneEquipConfig( ControlledZoneNum ).InletNode, ZoneEquipConfig( ControlledZoneNum ).NumInletNodes );
				if ( NodeIndex == 0 ) {
					ShowSevereError( "InitPurchasedAir: In " + PurchAir( PurchAirNum ).cObjectName + " = " + PurchAir( PurchAirNum ).Name );
					ShowContinueError( "Zone Supply Air Node Name=" + NodeID( SupplyNodeNum ) + " is not a zone inlet node." );
					ShowContinueError( "Check ZoneHVAC:EquipmentConnections for zone=" + ZoneEquipConfig( ControlledZoneNum ).ZoneName );
					ShowFatalError( "Preceding condition causes termination." );
				}
			}

			// Set recirculation node number
			// If exhaust node is specified, then recirculation is exhaust node, otherwise use zone return node
			// this check has to be done here because of SimPurchasedAir passing in ControlledZoneNum
			UseReturnNode = false;
			if ( PurchAir( PurchAirNum ).ZoneExhaustAirNodeNum > 0 ) {
				ExhaustNodeNum = PurchAir( PurchAirNum ).ZoneExhaustAirNodeNum;
				NodeIndex = FindNumberInList( ExhaustNodeNum, ZoneEquipConfig( ControlledZoneNum ).ExhaustNode, ZoneEquipConfig( ControlledZoneNum ).NumExhaustNodes );
				if ( NodeIndex == 0 ) {
					ShowSevereError( "InitPurchasedAir: In " + PurchAir( PurchAirNum ).cObjectName + " = " + PurchAir( PurchAirNum ).Name );
					ShowContinueError( "Zone Exhaust Air Node Name=" + NodeID( ExhaustNodeNum ) + " is not a zone exhaust node." );
					ShowContinueError( "Check ZoneHVAC:EquipmentConnections for zone=" + ZoneEquipConfig( ControlledZoneNum ).ZoneName );
					ShowContinueError( "Zone return air node will be used for ideal loads recirculation air." );
					UseReturnNode = true;
				} else {
					PurchAir( PurchAirNum ).ZoneRecircAirNodeNum = PurchAir( PurchAirNum ).ZoneExhaustAirNodeNum;
				}
			} else {
				UseReturnNode = true;
			}
			if ( UseReturnNode ) {
				if ( ZoneEquipConfig( ControlledZoneNum ).ReturnAirNode > 0 ) {
					PurchAir( PurchAirNum ).ZoneRecircAirNodeNum = ZoneEquipConfig( ControlledZoneNum ).ReturnAirNode;
				} else {
					ShowFatalError( "InitPurchasedAir: In " + PurchAir( PurchAirNum ).cObjectName + " = " + PurchAir( PurchAirNum ).Name );
					ShowContinueError( " Invalid recirculation node. No exhaust or return node has been specified for this zone in ZoneHVAC:EquipmentConnections." );
					ShowFatalError( "Preceding condition causes termination." );
				}
			}
			// If there is OA and economizer is active, then there must be a limit on cooling flow rate
			if ( PurchAir( PurchAirNum ).OutdoorAir && ( PurchAir( PurchAirNum ).EconomizerType != NoEconomizer ) ) {
				if ( ( PurchAir( PurchAirNum ).CoolingLimit == NoLimit ) || ( PurchAir( PurchAirNum ).CoolingLimit == LimitCapacity ) ) {
					ShowSevereError( "InitPurchasedAir: In " + PurchAir( PurchAirNum ).cObjectName + " = " + PurchAir( PurchAirNum ).Name );
					ShowContinueError( "There is outdoor air with economizer active but there is no limit on cooling air flow rate." );
					ShowContinueError( "Cooling Limit must be set to LimitFlowRate or LimitFlowRateAndCapacity, and Maximum Cooling Air Flow Rate must be set to a value or autosize." );
					ShowContinueError( "Simulation will proceed with no limit on outdoor air flow rate." );
				}
			}
		}

		if ( ! SysSizingCalc && MySizeFlag( PurchAirNum ) ) {

			SizePurchasedAir( PurchAirNum );

			MySizeFlag( PurchAirNum ) = false;
		}

		// Do the Begin Environment initializations
		if ( BeginEnvrnFlag && MyEnvrnFlag( PurchAirNum ) ) {

			if ( ( PurchAir( PurchAirNum ).HeatingLimit == LimitFlowRate ) || ( PurchAir( PurchAirNum ).HeatingLimit == LimitFlowRateAndCapacity ) ) {
				PurchAir( PurchAirNum ).MaxHeatMassFlowRate = StdRhoAir * PurchAir( PurchAirNum ).MaxHeatVolFlowRate;
			} else {
				PurchAir( PurchAirNum ).MaxHeatMassFlowRate = 0.0;
			}
			if ( ( PurchAir( PurchAirNum ).CoolingLimit == LimitFlowRate ) || ( PurchAir( PurchAirNum ).CoolingLimit == LimitFlowRateAndCapacity ) ) {
				PurchAir( PurchAirNum ).MaxCoolMassFlowRate = StdRhoAir * PurchAir( PurchAirNum ).MaxCoolVolFlowRate;
			} else {
				PurchAir( PurchAirNum ).MaxCoolMassFlowRate = 0.0;
			}
			MyEnvrnFlag( PurchAirNum ) = false;
		}

		if ( ! BeginEnvrnFlag ) {
			MyEnvrnFlag( PurchAirNum ) = true;
		}

		// These initializations are done every iteration
		// check that supply air temps can meet the zone thermostat setpoints
		if ( PurchAir( PurchAirNum ).MinCoolSuppAirTemp > ZoneThermostatSetPointHi( ActualZoneNum ) && ZoneThermostatSetPointHi( ActualZoneNum ) != 0 && PurchAir( PurchAirNum ).CoolingLimit == NoLimit ) {
			// Check if the unit is scheduled off
			UnitOn = true;
			//        IF (PurchAir(PurchAirNum)%AvailSchedPtr > 0) THEN
			if ( GetCurrentScheduleValue( PurchAir( PurchAirNum ).AvailSchedPtr ) <= 0 ) {
				UnitOn = false;
			}
			//        END IF
			// Check if cooling available
			CoolOn = true;
			//        IF (PurchAir(PurchAirNum)%CoolSchedPtr > 0) THEN
			if ( GetCurrentScheduleValue( PurchAir( PurchAirNum ).CoolSchedPtr ) <= 0 ) {
				CoolOn = false;
			}
			//        END IF
			if ( UnitOn && CoolOn ) {
				if ( PurchAir( PurchAirNum ).CoolErrIndex == 0 ) {
					ShowSevereError( "InitPurchasedAir: For " + PurchAir( PurchAirNum ).cObjectName + " = " + PurchAir( PurchAirNum ).Name + " serving Zone " + Zone( ActualZoneNum ).Name );
					ShowContinueError( "..the minimum supply air temperature for cooling [" + RoundSigDigits( PurchAir( PurchAirNum ).MinCoolSuppAirTemp, 2 ) + "] is greater than the zone cooling mean air temperature (MAT) setpoint [" + RoundSigDigits( ZoneThermostatSetPointHi( ActualZoneNum ), 2 ) + "]." );
					ShowContinueError( "..For operative and comfort thermostat controls, the MAT setpoint is computed." );
					ShowContinueError( "..This error may indicate that the mean radiant temperature or another comfort factor is too warm." );
					ShowContinueError( "Unit availability is nominally ON and Cooling availability is nominally ON." );
					ShowContinueError( "Limit Cooling Capacity Type=" + cLimitType( PurchAir( PurchAirNum ).CoolingLimit ) );
					// could check for optemp control or comfort control here
					ShowContinueErrorTimeStamp( "" );
				}
				ShowRecurringSevereErrorAtEnd( "InitPurchasedAir: For " + PurchAir( PurchAirNum ).cObjectName + " = " + PurchAir( PurchAirNum ).Name + " serving Zone " + Zone( ActualZoneNum ).Name + ", the minimum supply air temperature for cooling error continues", PurchAir( PurchAirNum ).CoolErrIndex, PurchAir( PurchAirNum ).MinCoolSuppAirTemp, PurchAir( PurchAirNum ).MinCoolSuppAirTemp, _, "C", "C" );
			}
		}
		if ( PurchAir( PurchAirNum ).MaxHeatSuppAirTemp < ZoneThermostatSetPointLo( ActualZoneNum ) && ZoneThermostatSetPointLo( ActualZoneNum ) != 0 && PurchAir( PurchAirNum ).HeatingLimit == NoLimit ) {
			// Check if the unit is scheduled off
			UnitOn = true;
			//        IF (PurchAir(PurchAirNum)%AvailSchedPtr > 0) THEN
			if ( GetCurrentScheduleValue( PurchAir( PurchAirNum ).AvailSchedPtr ) <= 0 ) {
				UnitOn = false;
			}
			//        END IF
			// Check if heating and cooling available
			HeatOn = true;
			//        IF (PurchAir(PurchAirNum)%HeatSchedPtr > 0) THEN
			if ( GetCurrentScheduleValue( PurchAir( PurchAirNum ).HeatSchedPtr ) <= 0 ) {
				HeatOn = false;
			}
			//        END IF
			if ( UnitOn && HeatOn ) {
				if ( PurchAir( PurchAirNum ).HeatErrIndex == 0 ) {
					ShowSevereMessage( "InitPurchasedAir: For " + PurchAir( PurchAirNum ).cObjectName + " = " + PurchAir( PurchAirNum ).Name + " serving Zone " + Zone( ActualZoneNum ).Name );
					ShowContinueError( "..the maximum supply air temperature for heating [" + RoundSigDigits( PurchAir( PurchAirNum ).MaxHeatSuppAirTemp, 2 ) + "] is less than the zone mean air temperature heating setpoint [" + RoundSigDigits( ZoneThermostatSetPointLo( ActualZoneNum ), 2 ) + "]." );
					ShowContinueError( "..For operative and comfort thermostat controls, the MAT setpoint is computed." );
					ShowContinueError( "..This error may indicate that the mean radiant temperature or another comfort factor is too cold." );
					ShowContinueError( "Unit availability is nominally ON and Heating availability is nominally ON." );
					ShowContinueError( "Limit Heating Capacity Type=" + cLimitType( PurchAir( PurchAirNum ).HeatingLimit ) );
					// could check for optemp control or comfort control here
					ShowContinueErrorTimeStamp( "" );
				}
				ShowRecurringSevereErrorAtEnd( "InitPurchasedAir: For " + PurchAir( PurchAirNum ).cObjectName + " = " + PurchAir( PurchAirNum ).Name + " serving Zone " + Zone( ActualZoneNum ).Name + ", maximum supply air temperature for heating error continues", PurchAir( PurchAirNum ).HeatErrIndex, PurchAir( PurchAirNum ).MaxHeatSuppAirTemp, PurchAir( PurchAirNum ).MaxHeatSuppAirTemp, _, "C", "C" );
			}
		}
		//      IF (ErrorsFound .and. .not. WarmupFlag) THEN
		//        CALL ShowFatalError('Preceding conditions cause termination.')
		//      ENDIF

	}

	void
	SizePurchasedAir( int const PurchAirNum )
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Fred Buhl
		//       DATE WRITTEN   April 2003
		//       MODIFIED       M. Witte, June 2011, add sizing for new capacity fields
		//                      August 2013 Daeho Kang, add component sizing table entries
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine is for sizing Purchased Air Components for which flow rates have not been
		// specified in the input.

		// METHODOLOGY EMPLOYED:
		// Obtains flow rates from the zone sizing arrays.

		// REFERENCES:
		// na

		// Using/Aliasing
		using namespace DataSizing;
		using namespace InputProcessor;
		using ReportSizingManager::RequestSizing;
		using ReportSizingManager::ReportSizingOutput;
		using Psychrometrics::PsyCpAirFnWTdb;
		using Psychrometrics::RhoH2O;
		using Psychrometrics::CPHW;
		using Psychrometrics::CPCW;
		using Psychrometrics::PsyHFnTdbW;
		using General::RoundSigDigits;
		using DataHVACGlobals::CoolingAirflowSizing;
		using DataHVACGlobals::HeatingAirflowSizing;
		using DataHVACGlobals::CoolingCapacitySizing;
		using DataHVACGlobals::HeatingCapacitySizing;
		using DataHeatBalance::Zone;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName("SizePurchasedAir: "); // include trailing blank space

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		bool IsAutoSize; // Indicator to autosize
		Real64 MaxHeatVolFlowRateDes; // Autosized maximum heating air flow for reporting
		Real64 MaxHeatVolFlowRateUser; // Hardsized maximum heating air flow for reporting
		Real64 MaxCoolVolFlowRateDes; // Autosized maximum cooling air flow for reporting
		Real64 MaxCoolVolFlowRateUser; // Hardsized maximum cooling air flow for reporting
		Real64 MaxHeatSensCapDes; // Autosized maximum sensible heating capacity for reporting
		Real64 MaxHeatSensCapUser; // Hardsized maximum sensible heating capacity for reporting
		Real64 MaxCoolTotCapDes; // Autosized maximum sensible cooling capacity for reporting
		Real64 MaxCoolTotCapUser; // Hardsized maximum sensible cooling capacity for reporting
		std::string CompName; // component name
		std::string CompType; // component type
		std::string SizingString; // input field sizing description (e.g., Nominal Capacity)
		Real64 TempSize; // autosized value of coil input field
		int FieldNum = 2; // IDD numeric field number where input field description is found
		int SizingMethod; // Integer representation of sizing method name (e.g., CoolingAirflowSizing, HeatingAirflowSizing, CoolingCapacitySizing, HeatingCapacitySizing, etc.)
		bool PrintFlag; // TRUE when sizing information is reported in the eio file
		int zoneHVACIndex; // index of zoneHVAC equipment sizing specification
		int SAFMethod( 0 ); // supply air flow rate sizing method (SupplyAirFlowRate, FlowPerFloorArea, FractionOfAutosizedCoolingAirflow, FractionOfAutosizedHeatingAirflow ...)
		int CapSizingMethod(0); // capacity sizing methods (HeatingDesignCapacity, CapacityPerFloorArea, FractionOfAutosizedCoolingCapacity, and FractionOfAutosizedHeatingCapacity )
		Real64 CoolingAirVolFlowDes( 0.0 ); // cooling supply air flow rate
		Real64 HeatingAirVolFlowDes( 0.0 ); // heating supply air flow rate

		IsAutoSize = false;
		MaxHeatVolFlowRateDes = 0.0;
		MaxHeatVolFlowRateUser = 0.0;
		MaxCoolVolFlowRateDes = 0.0;
		MaxCoolVolFlowRateUser = 0.0;
		MaxHeatSensCapDes = 0.0;
		MaxHeatSensCapUser = 0.0;
		MaxCoolTotCapDes = 0.0;
		MaxCoolTotCapUser = 0.0;

		ZoneHeatingOnlyFan = false;
		ZoneCoolingOnlyFan = false;
		CompType = PurchAir(PurchAirNum).cObjectName;
		CompName = PurchAir(PurchAirNum).Name;

		if ( CurZoneEqNum > 0 ) {
			if ( PurchAir( PurchAirNum ).HVACSizingIndex > 0 ) {
				DataZoneNumber = PurchAir( PurchAirNum ).ZonePtr;
				zoneHVACIndex = PurchAir( PurchAirNum ).HVACSizingIndex;

				FieldNum = 5; // N5 , \field Maximum Heating Air Flow Rate
				PrintFlag = true;
				SizingString = PurchAirNumericFields( PurchAirNum ).FieldNames( FieldNum ) + " [m3/s]";
				if ( ZoneHVACSizing( zoneHVACIndex ).HeatingSAFMethod > 0 ) {
					SizingMethod = HeatingAirflowSizing;
					ZoneHeatingOnlyFan = true;
					SAFMethod = ZoneHVACSizing( zoneHVACIndex ).HeatingSAFMethod;
					ZoneEqSizing( CurZoneEqNum ).SizingMethod( SizingMethod ) = SAFMethod;
					if ( SAFMethod == SupplyAirFlowRate || SAFMethod == FlowPerFloorArea || SAFMethod == FractionOfAutosizedHeatingAirflow ) {
						if ( SAFMethod == SupplyAirFlowRate ) {
							if ( ( ZoneHVACSizing( zoneHVACIndex ).MaxHeatAirVolFlow == AutoSize ) && ( ( PurchAir( PurchAirNum ).HeatingLimit == LimitFlowRate ) || ( PurchAir( PurchAirNum ).HeatingLimit == LimitFlowRateAndCapacity ) ) ) {
								TempSize = ZoneHVACSizing( zoneHVACIndex ).MaxHeatAirVolFlow;
								RequestSizing( CompType, CompName, SizingMethod, SizingString, TempSize, PrintFlag, RoutineName );
								HeatingAirVolFlowDes = TempSize;
							} else {
								if ( ZoneHVACSizing( zoneHVACIndex ).MaxHeatAirVolFlow > 0.0 ) {
									RequestSizing( CompType, CompName, SizingMethod, SizingString, ZoneHVACSizing( zoneHVACIndex ).MaxHeatAirVolFlow, PrintFlag, RoutineName );
									HeatingAirVolFlowDes = ZoneHVACSizing( zoneHVACIndex ).MaxHeatAirVolFlow;
								}
							}
						} else if ( SAFMethod == FlowPerFloorArea ) {
							ZoneEqSizing( CurZoneEqNum ).SystemAirFlow = true;
							ZoneEqSizing( CurZoneEqNum ).AirVolFlow = ZoneHVACSizing( zoneHVACIndex ).MaxHeatAirVolFlow * Zone( DataZoneNumber ).FloorArea;
							TempSize = ZoneEqSizing( CurZoneEqNum ).AirVolFlow;
							DataScalableSizingON = true;
							RequestSizing( CompType, CompName, SizingMethod, SizingString, TempSize, PrintFlag, RoutineName );
							HeatingAirVolFlowDes = TempSize;
						} else if ( SAFMethod == FractionOfAutosizedHeatingAirflow ) {
							DataFracOfAutosizedHeatingAirflow = ZoneHVACSizing( zoneHVACIndex ).MaxHeatAirVolFlow;
							if ( ( ZoneHVACSizing( zoneHVACIndex ).MaxHeatAirVolFlow == AutoSize ) && ( ( PurchAir( PurchAirNum ).HeatingLimit == LimitFlowRate ) || ( PurchAir( PurchAirNum ).HeatingLimit == LimitFlowRateAndCapacity ) ) ) {
								TempSize = AutoSize;
								DataScalableSizingON = true;
								RequestSizing( CompType, CompName, SizingMethod, SizingString, TempSize, PrintFlag, RoutineName );
								HeatingAirVolFlowDes = TempSize;
							}

						} else {
							// Invalid sizing method
						}
					} else if ( SAFMethod == FlowPerHeatingCapacity ) {
						SizingMethod = HeatingCapacitySizing;
						TempSize = AutoSize;
						PrintFlag = false;
						if ( ( ZoneHVACSizing( zoneHVACIndex ).MaxHeatAirVolFlow == AutoSize ) && ( ( PurchAir( PurchAirNum ).HeatingLimit == LimitFlowRate ) || ( PurchAir( PurchAirNum ).HeatingLimit == LimitFlowRateAndCapacity ) ) ) {
							TempSize = AutoSize;
							DataScalableSizingON = true;
							RequestSizing( CompType, CompName, SizingMethod, SizingString, TempSize, PrintFlag, RoutineName );
							DataAutosizedHeatingCapacity = TempSize;
							DataFlowPerHeatingCapacity = ZoneHVACSizing( zoneHVACIndex ).MaxHeatAirVolFlow;
							SizingMethod = HeatingAirflowSizing;
							PrintFlag = true;
							TempSize = AutoSize;
							RequestSizing( CompType, CompName, SizingMethod, SizingString, TempSize, PrintFlag, RoutineName );
							HeatingAirVolFlowDes = TempSize;
						}
					}
					MaxHeatVolFlowRateDes = max( 0.0, HeatingAirVolFlowDes );
					PurchAir( PurchAirNum ).MaxHeatVolFlowRate = MaxHeatVolFlowRateDes;
					ZoneHeatingOnlyFan = false;

					CapSizingMethod = ZoneHVACSizing( zoneHVACIndex ).HeatingCapMethod;
					ZoneEqSizing( CurZoneEqNum ).CapSizingMethod = CapSizingMethod;
					if ( CapSizingMethod == HeatingDesignCapacity || CapSizingMethod == CapacityPerFloorArea || CapSizingMethod == FractionOfAutosizedHeatingCapacity ) {
						if ( CapSizingMethod == HeatingDesignCapacity ) {
							if ( ZoneHVACSizing( zoneHVACIndex ).ScaledHeatingCapacity > 0.0 ) {
								ZoneEqSizing( CurZoneEqNum ).HeatingCapacity = true;
								ZoneEqSizing( CurZoneEqNum ).DesHeatingLoad = ZoneHVACSizing( zoneHVACIndex ).ScaledHeatingCapacity;
							}
							TempSize = ZoneHVACSizing( zoneHVACIndex ).ScaledHeatingCapacity;
						} else if ( CapSizingMethod == CapacityPerFloorArea ) {
							ZoneEqSizing( CurZoneEqNum ).HeatingCapacity = true;
							ZoneEqSizing( CurZoneEqNum ).DesHeatingLoad = ZoneHVACSizing( zoneHVACIndex ).ScaledHeatingCapacity * Zone( DataZoneNumber ).FloorArea;
							DataScalableSizingON = true;
						} else if ( CapSizingMethod == FractionOfAutosizedHeatingCapacity ) {
							DataFracOfAutosizedHeatingCapacity = ZoneHVACSizing( zoneHVACIndex ).ScaledHeatingCapacity;
							TempSize = AutoSize;
						}
					}
					SizingMethod = HeatingCapacitySizing;
					SizingString = "";
					ZoneHeatingOnlyFan = true;
					PrintFlag = false;
					RequestSizing( CompType, CompName, SizingMethod, SizingString, TempSize, PrintFlag, RoutineName );
					MaxHeatSensCapDes = TempSize;
					ZoneHeatingOnlyFan = false;
					if ( MaxHeatSensCapDes < SmallLoad ) {
						MaxHeatSensCapDes = 0.0;
					}
					if ( IsAutoSize ) {
						PurchAir( PurchAirNum ).MaxHeatSensCap = MaxHeatSensCapDes;
						ReportSizingOutput( PurchAir( PurchAirNum ).cObjectName, PurchAir( PurchAirNum ).Name, "Design Size Maximum Sensible Heating Capacity [W]", MaxHeatSensCapDes );
						// If there is OA, check if sizing calcs have OA>0, throw warning if not
						if ( ( PurchAir( PurchAirNum ).OutdoorAir ) && ( FinalZoneSizing( CurZoneEqNum ).MinOA == 0.0 ) ) {
							ShowWarningError( "InitPurchasedAir: In " + PurchAir( PurchAirNum ).cObjectName + " = " + PurchAir( PurchAirNum ).Name );
							ShowContinueError( "There is outdoor air specified in this object, but the design outdoor air flow rate for this " );
							ShowContinueError( "zone is zero. The Maximum Sensible Heating Capacity will be autosized for zero outdoor air flow. " );
							ShowContinueError( "Check the outdoor air specifications in the Sizing:Zone object for zone " + FinalZoneSizing( CurZoneEqNum ).ZoneName + '.' );
						}
					} else {
						if ( PurchAir( PurchAirNum ).MaxHeatSensCap > 0.0 && MaxHeatSensCapDes > 0.0 ) {
							MaxHeatSensCapUser = PurchAir( PurchAirNum ).MaxHeatSensCap;
							ReportSizingOutput( PurchAir( PurchAirNum ).cObjectName, PurchAir( PurchAirNum ).Name, "Design Size Maximum Sensible Heating Capacity [W]", MaxHeatSensCapDes, "User-Specified Maximum Sensible Heating Capacity [W]", MaxHeatSensCapUser );
							if ( DisplayExtraWarnings ) {
								if ( ( std::abs( MaxHeatSensCapDes - MaxHeatSensCapUser ) / MaxHeatSensCapUser ) > AutoVsHardSizingThreshold ) {
									ShowMessage( "SizePurchasedAir: Potential issue with equipment sizing for " + PurchAir( PurchAirNum ).cObjectName + ' ' + PurchAir( PurchAirNum ).Name );
									ShowContinueError( "...User-Specified Maximum Sensible Heating Capacity of " + RoundSigDigits( MaxHeatSensCapUser, 2 ) + " [W]" );
									ShowContinueError( "...differs from Design Size Maximum Sensible Heating Capacity of " + RoundSigDigits( MaxHeatSensCapDes, 2 ) + " [W]" );
									ShowContinueError( "This may, or may not, indicate mismatched component sizes." );
									ShowContinueError( "Verify that the value entered is intended and is consistent with other components." );
								}
							}
						}
					}
				}

				FieldNum = 7; //N7 , \field Maximum Cooling Air Flow Rate
				PrintFlag = true;
				SizingString = PurchAirNumericFields( PurchAirNum ).FieldNames( FieldNum ) + " [m3/s]";
				if ( ZoneHVACSizing( zoneHVACIndex ).CoolingSAFMethod > 0 ) {
					SizingMethod = CoolingAirflowSizing;
					ZoneCoolingOnlyFan = true;
					SAFMethod = ZoneHVACSizing( zoneHVACIndex ).CoolingSAFMethod;
					ZoneEqSizing( CurZoneEqNum ).SizingMethod( SizingMethod ) = SAFMethod;
					if ( SAFMethod == SupplyAirFlowRate || SAFMethod == FlowPerFloorArea || SAFMethod == FractionOfAutosizedCoolingAirflow ) {
						if ( SAFMethod == SupplyAirFlowRate ) {
							if ( ( ZoneHVACSizing( zoneHVACIndex ).MaxCoolAirVolFlow == AutoSize ) && ( ( PurchAir( PurchAirNum ).CoolingLimit == LimitFlowRate ) || ( PurchAir( PurchAirNum ).CoolingLimit == LimitFlowRateAndCapacity ) || ( PurchAir( PurchAirNum ).OutdoorAir && PurchAir( PurchAirNum ).EconomizerType != NoEconomizer ) ) ) {
								TempSize = ZoneHVACSizing( zoneHVACIndex ).MaxCoolAirVolFlow;
								RequestSizing( CompType, CompName, SizingMethod, SizingString, TempSize, PrintFlag, RoutineName );
								CoolingAirVolFlowDes = TempSize;
							} else {
								if ( ZoneHVACSizing( zoneHVACIndex ).MaxCoolAirVolFlow > 0.0 ) {
									RequestSizing( CompType, CompName, SizingMethod, SizingString, ZoneHVACSizing( zoneHVACIndex ).MaxCoolAirVolFlow, PrintFlag, RoutineName );
									CoolingAirVolFlowDes = ZoneHVACSizing( zoneHVACIndex ).MaxCoolAirVolFlow;
								}
							}
						} else if ( SAFMethod == FlowPerFloorArea ) {
							ZoneEqSizing( CurZoneEqNum ).SystemAirFlow = true;
							ZoneEqSizing( CurZoneEqNum ).AirVolFlow = ZoneHVACSizing( zoneHVACIndex ).MaxCoolAirVolFlow * Zone( DataZoneNumber ).FloorArea;
							TempSize = ZoneEqSizing( CurZoneEqNum ).AirVolFlow;
							DataScalableSizingON = true;
							RequestSizing( CompType, CompName, SizingMethod, SizingString, TempSize, PrintFlag, RoutineName );
							CoolingAirVolFlowDes = TempSize;
						} else if ( SAFMethod == FractionOfAutosizedCoolingAirflow ) {
							if ( ( ZoneHVACSizing( zoneHVACIndex ).MaxCoolAirVolFlow == AutoSize ) && ( ( PurchAir( PurchAirNum ).CoolingLimit == LimitFlowRate ) || ( PurchAir( PurchAirNum ).CoolingLimit == LimitFlowRateAndCapacity ) || ( PurchAir( PurchAirNum ).OutdoorAir && PurchAir( PurchAirNum ).EconomizerType != NoEconomizer ) ) ) {
								DataFracOfAutosizedCoolingAirflow = ZoneHVACSizing( zoneHVACIndex ).MaxCoolAirVolFlow;
								TempSize = AutoSize;
								DataScalableSizingON = true;
								RequestSizing( CompType, CompName, SizingMethod, SizingString, TempSize, PrintFlag, RoutineName );
								CoolingAirVolFlowDes = TempSize;
							}
						} else {
							// Invalid scalable sizing method
						}
					} else if ( SAFMethod == FlowPerCoolingCapacity ) {
						if ( ( ZoneHVACSizing( zoneHVACIndex ).MaxCoolAirVolFlow == AutoSize ) && ( ( PurchAir( PurchAirNum ).CoolingLimit == LimitFlowRate ) || ( PurchAir( PurchAirNum ).CoolingLimit == LimitFlowRateAndCapacity ) || ( PurchAir( PurchAirNum ).OutdoorAir && PurchAir( PurchAirNum ).EconomizerType != NoEconomizer ) ) ) {
							SizingMethod = CoolingCapacitySizing;
							TempSize = AutoSize;
							PrintFlag = false;
							RequestSizing( CompType, CompName, SizingMethod, SizingString, TempSize, PrintFlag, RoutineName );
							DataAutosizedCoolingCapacity = TempSize;
							DataFlowPerCoolingCapacity = ZoneHVACSizing( zoneHVACIndex ).MaxCoolAirVolFlow;
							SizingMethod = CoolingAirflowSizing;
							PrintFlag = true;
							TempSize = AutoSize;
							DataScalableSizingON = true;
							RequestSizing( CompType, CompName, SizingMethod, SizingString, TempSize, PrintFlag, RoutineName );
							CoolingAirVolFlowDes = TempSize;
						}
					}
					MaxCoolVolFlowRateDes = max( 0.0, CoolingAirVolFlowDes );
					PurchAir( PurchAirNum ).MaxCoolVolFlowRate = MaxCoolVolFlowRateDes;
					ZoneCoolingOnlyFan = false;
					DataScalableSizingON = false;

					CapSizingMethod = ZoneHVACSizing( zoneHVACIndex ).CoolingCapMethod;
					ZoneEqSizing( CurZoneEqNum ).CapSizingMethod = CapSizingMethod;
					if ( CapSizingMethod == CoolingDesignCapacity || CapSizingMethod == CapacityPerFloorArea || CapSizingMethod == FractionOfAutosizedCoolingCapacity ) {
						if ( CapSizingMethod == CoolingDesignCapacity ) {
							if ( ZoneHVACSizing( zoneHVACIndex ).ScaledCoolingCapacity > 0.0 ) {
								ZoneEqSizing( CurZoneEqNum ).CoolingCapacity = true;
								ZoneEqSizing( CurZoneEqNum ).DesCoolingLoad = ZoneHVACSizing( zoneHVACIndex ).ScaledCoolingCapacity;
							} else {
								DataFlowUsedForSizing = FinalZoneSizing( CurZoneEqNum ).DesCoolMassFlow;
							}
							TempSize = ZoneHVACSizing( zoneHVACIndex ).ScaledCoolingCapacity;
						} else if ( CapSizingMethod == CapacityPerFloorArea ) {
							ZoneEqSizing( CurZoneEqNum ).CoolingCapacity = true;
							ZoneEqSizing( CurZoneEqNum ).DesCoolingLoad = ZoneHVACSizing( zoneHVACIndex ).ScaledCoolingCapacity * Zone( DataZoneNumber ).FloorArea;
							DataScalableSizingON = true;
						} else if ( CapSizingMethod == FractionOfAutosizedCoolingCapacity ) {
							DataFracOfAutosizedHeatingCapacity = ZoneHVACSizing( zoneHVACIndex ).ScaledCoolingCapacity;
							DataFlowUsedForSizing = FinalZoneSizing( CurZoneEqNum ).DesCoolMassFlow;
							TempSize = AutoSize;
						}
					}
					SizingMethod = CoolingCapacitySizing;
					SizingString = "";
					ZoneCoolingOnlyFan = true;
					PrintFlag = false;
					TempSize = PurchAir( PurchAirNum ).MaxCoolTotCap;
					RequestSizing( CompType, CompName, SizingMethod, SizingString, TempSize, PrintFlag, RoutineName );
					MaxCoolTotCapDes = TempSize;
					ZoneCoolingOnlyFan = false;
					if ( MaxCoolTotCapDes < SmallLoad ) {
						MaxCoolTotCapDes = 0.0;
					}
					if ( IsAutoSize ) {
						PurchAir( PurchAirNum ).MaxCoolTotCap = MaxCoolTotCapDes;
						ReportSizingOutput( PurchAir( PurchAirNum ).cObjectName, PurchAir( PurchAirNum ).Name, "Design Size Maximum Total Cooling Capacity [W]", MaxCoolTotCapDes );
						// If there is OA, check if sizing calcs have OA>0, throw warning if not
						if ( ( PurchAir( PurchAirNum ).OutdoorAir ) && ( FinalZoneSizing( CurZoneEqNum ).MinOA == 0.0 ) ) {
							ShowWarningError( "SizePurchasedAir: In " + PurchAir( PurchAirNum ).cObjectName + " = " + PurchAir( PurchAirNum ).Name );
							ShowContinueError( "There is outdoor air specified in this object, but the design outdoor air flow rate for this " );
							ShowContinueError( "zone is zero. The Maximum Total Cooling Capacity will be autosized for zero outdoor air flow. " );
							ShowContinueError( "Check the outdoor air specifications in the Sizing:Zone object for zone " + FinalZoneSizing( CurZoneEqNum ).ZoneName + '.' );
						}
					} else {
						if ( PurchAir( PurchAirNum ).MaxCoolTotCap > 0.0 && MaxCoolTotCapDes > 0.0 ) {
							MaxCoolTotCapUser = PurchAir( PurchAirNum ).MaxCoolTotCap;
							ReportSizingOutput( PurchAir( PurchAirNum ).cObjectName, PurchAir( PurchAirNum ).Name, "Design Size Maximum Total Cooling Capacity [W]", MaxCoolTotCapDes, "User-Specified Maximum Total Cooling Capacity [W]", MaxCoolTotCapUser );
							if ( DisplayExtraWarnings ) {
								if ( ( std::abs( MaxCoolTotCapDes - MaxCoolTotCapUser ) / MaxCoolTotCapUser ) > AutoVsHardSizingThreshold ) {
									ShowMessage( "SizePurchasedAir: Potential issue with equipment sizing for " + PurchAir( PurchAirNum ).cObjectName + ' ' + PurchAir( PurchAirNum ).Name );
									ShowContinueError( "User-Specified Maximum Total Cooling Capacity of " + RoundSigDigits( MaxCoolTotCapUser, 2 ) + " [W]" );
									ShowContinueError( "differs from Design Size Maximum Total Cooling Capacity of " + RoundSigDigits( MaxCoolTotCapDes, 2 ) + " [W]" );
									ShowContinueError( "This may, or may not, indicate mismatched component sizes." );
									ShowContinueError( "Verify that the value entered is intended and is consistent with other components." );
								}
							}
						}
					}

				}

			} else {
				//SizingString = "Maximum Heating Air Flow Rate [m3/s]";
				SizingMethod = HeatingAirflowSizing;
				FieldNum = 5;
				SizingString = PurchAirNumericFields( PurchAirNum ).FieldNames( FieldNum ) + " [m3/s]";
				IsAutoSize = false;
				PrintFlag = true;
				if ( ( PurchAir( PurchAirNum ).MaxHeatVolFlowRate == AutoSize ) && ( ( PurchAir( PurchAirNum ).HeatingLimit == LimitFlowRate ) || ( PurchAir( PurchAirNum ).HeatingLimit == LimitFlowRateAndCapacity ) ) ) {
					IsAutoSize = true;
				}
				if ( !IsAutoSize && !ZoneSizingRunDone ) { // Simulation continue
					if ( PurchAir( PurchAirNum ).MaxHeatVolFlowRate > 0.0 ) {
						RequestSizing( CompType, CompName, SizingMethod, SizingString, PurchAir( PurchAirNum ).MaxHeatVolFlowRate, PrintFlag, RoutineName );
					}
					MaxHeatVolFlowRateDes = 0.0;
				} else {
					ZoneHeatingOnlyFan = true;
					TempSize = PurchAir( PurchAirNum ).MaxHeatVolFlowRate;
					RequestSizing( CompType, CompName, SizingMethod, SizingString, TempSize, PrintFlag, RoutineName );
					MaxHeatVolFlowRateDes = TempSize;
					PurchAir( PurchAirNum ).MaxHeatVolFlowRate = MaxHeatVolFlowRateDes;
					ZoneHeatingOnlyFan = false;
				}

				IsAutoSize = false;
				SizingMethod = HeatingCapacitySizing;
				FieldNum = 6; // N6, \field Maximum Sensible Heating Capacity
				SizingString = PurchAirNumericFields( PurchAirNum ).FieldNames( FieldNum ) + " [m3/s]";
				if ( ( PurchAir( PurchAirNum ).MaxHeatSensCap == AutoSize ) && ( ( PurchAir( PurchAirNum ).HeatingLimit == LimitCapacity ) || ( PurchAir( PurchAirNum ).HeatingLimit == LimitFlowRateAndCapacity ) ) ) {
					IsAutoSize = true;
				}
				if ( !IsAutoSize && !ZoneSizingRunDone ) { // Simulation continue
					if ( PurchAir( PurchAirNum ).MaxHeatSensCap > 0.0 ) {
						RequestSizing( CompType, CompName, SizingMethod, SizingString, PurchAir( PurchAirNum ).MaxHeatSensCap, PrintFlag, RoutineName );
					}
				} else {
					TempSize = PurchAir( PurchAirNum ).MaxHeatSensCap;
					ZoneHeatingOnlyFan = true;
					PrintFlag = false;
					RequestSizing( CompType, CompName, SizingMethod, SizingString, TempSize, PrintFlag, RoutineName );
					MaxHeatSensCapDes = TempSize;
					ZoneHeatingOnlyFan = false;
				}
				if ( MaxHeatSensCapDes < SmallLoad ) {
					MaxHeatSensCapDes = 0.0;
				}
				if ( IsAutoSize ) {
					PurchAir( PurchAirNum ).MaxHeatSensCap = MaxHeatSensCapDes;
					ReportSizingOutput( PurchAir( PurchAirNum ).cObjectName, PurchAir( PurchAirNum ).Name, "Design Size Maximum Sensible Heating Capacity [W]", MaxHeatSensCapDes );
					// If there is OA, check if sizing calcs have OA>0, throw warning if not
					if ( ( PurchAir( PurchAirNum ).OutdoorAir ) && ( FinalZoneSizing( CurZoneEqNum ).MinOA == 0.0 ) ) {
						ShowWarningError( "InitPurchasedAir: In " + PurchAir( PurchAirNum ).cObjectName + " = " + PurchAir( PurchAirNum ).Name );
						ShowContinueError( "There is outdoor air specified in this object, but the design outdoor air flow rate for this " );
						ShowContinueError( "zone is zero. The Maximum Sensible Heating Capacity will be autosized for zero outdoor air flow. " );
						ShowContinueError( "Check the outdoor air specifications in the Sizing:Zone object for zone " + FinalZoneSizing( CurZoneEqNum ).ZoneName + '.' );
					}
				} else {
					if ( PurchAir( PurchAirNum ).MaxHeatSensCap > 0.0 && MaxHeatSensCapDes > 0.0 ) {
						MaxHeatSensCapUser = PurchAir( PurchAirNum ).MaxHeatSensCap;
						ReportSizingOutput( PurchAir( PurchAirNum ).cObjectName, PurchAir( PurchAirNum ).Name, "Design Size Maximum Sensible Heating Capacity [W]", MaxHeatSensCapDes, "User-Specified Maximum Sensible Heating Capacity [W]", MaxHeatSensCapUser );
						if ( DisplayExtraWarnings ) {
							if ( ( std::abs( MaxHeatSensCapDes - MaxHeatSensCapUser ) / MaxHeatSensCapUser ) > AutoVsHardSizingThreshold ) {
								ShowMessage( "SizePurchasedAir: Potential issue with equipment sizing for " + PurchAir( PurchAirNum ).cObjectName + ' ' + PurchAir( PurchAirNum ).Name );
								ShowContinueError( "...User-Specified Maximum Sensible Heating Capacity of " + RoundSigDigits( MaxHeatSensCapUser, 2 ) + " [W]" );
								ShowContinueError( "...differs from Design Size Maximum Sensible Heating Capacity of " + RoundSigDigits( MaxHeatSensCapDes, 2 ) + " [W]" );
								ShowContinueError( "This may, or may not, indicate mismatched component sizes." );
								ShowContinueError( "Verify that the value entered is intended and is consistent with other components." );
							}
						}
					}
				}

				SizingMethod = CoolingAirflowSizing;
				FieldNum = 7; //N7 , \field Maximum Cooling Air Flow Rate
				PrintFlag = true;
				SizingString = PurchAirNumericFields( PurchAirNum ).FieldNames( FieldNum ) + " [m3/s]";
				IsAutoSize = false;
				if ( ( PurchAir( PurchAirNum ).MaxCoolVolFlowRate == AutoSize ) && ( ( PurchAir( PurchAirNum ).CoolingLimit == LimitFlowRate ) || ( PurchAir( PurchAirNum ).CoolingLimit == LimitFlowRateAndCapacity ) || ( PurchAir( PurchAirNum ).OutdoorAir && PurchAir( PurchAirNum ).EconomizerType != NoEconomizer ) ) ) {
					IsAutoSize = true;
				}
				if ( !IsAutoSize && !ZoneSizingRunDone ) { // Simulation continue
					if ( PurchAir( PurchAirNum ).MaxCoolVolFlowRate > 0.0 ) {
						RequestSizing( CompType, CompName, SizingMethod, SizingString, PurchAir( PurchAirNum ).MaxCoolVolFlowRate, PrintFlag, RoutineName );
					}
				} else {
					ZoneCoolingOnlyFan = true;
					TempSize = PurchAir( PurchAirNum ).MaxCoolVolFlowRate;
					RequestSizing( CompType, CompName, SizingMethod, SizingString, TempSize, PrintFlag, RoutineName );
					MaxCoolVolFlowRateDes = TempSize;
					PurchAir( PurchAirNum ).MaxCoolVolFlowRate = MaxCoolVolFlowRateDes;
					ZoneCoolingOnlyFan = false;
				}

				IsAutoSize = false;
				SizingMethod = CoolingCapacitySizing;
				FieldNum = 8; // N8, \field Maximum Total Cooling Capacity
				SizingString = PurchAirNumericFields( PurchAirNum ).FieldNames( FieldNum ) + " [m3/s]";
				if ( ( PurchAir( PurchAirNum ).MaxCoolTotCap == AutoSize ) && ( ( PurchAir( PurchAirNum ).CoolingLimit == LimitCapacity ) || ( PurchAir( PurchAirNum ).CoolingLimit == LimitFlowRateAndCapacity ) ) ) {
					IsAutoSize = true;
				}
				if ( !IsAutoSize && !ZoneSizingRunDone ) { // Simulation continue
					if ( PurchAir( PurchAirNum ).MaxCoolTotCap > 0.0 ) {
						RequestSizing( CompType, CompName, SizingMethod, SizingString, PurchAir( PurchAirNum ).MaxCoolTotCap, PrintFlag, RoutineName );
					}
				} else {
					ZoneCoolingOnlyFan = true;
					PrintFlag = false;
					TempSize = PurchAir( PurchAirNum ).MaxCoolTotCap;
					RequestSizing( CompType, CompName, SizingMethod, SizingString, TempSize, PrintFlag, RoutineName );
					MaxCoolTotCapDes = TempSize;
					ZoneCoolingOnlyFan = false;
				}
				if ( MaxCoolTotCapDes < SmallLoad ) {
					MaxCoolTotCapDes = 0.0;
				}
				if ( IsAutoSize ) {
					PurchAir( PurchAirNum ).MaxCoolTotCap = MaxCoolTotCapDes;
					ReportSizingOutput( PurchAir( PurchAirNum ).cObjectName, PurchAir( PurchAirNum ).Name, "Design Size Maximum Total Cooling Capacity [W]", MaxCoolTotCapDes );
					// If there is OA, check if sizing calcs have OA>0, throw warning if not
					if ( ( PurchAir( PurchAirNum ).OutdoorAir ) && ( FinalZoneSizing( CurZoneEqNum ).MinOA == 0.0 ) ) {
						ShowWarningError( "SizePurchasedAir: In " + PurchAir( PurchAirNum ).cObjectName + " = " + PurchAir( PurchAirNum ).Name );
						ShowContinueError( "There is outdoor air specified in this object, but the design outdoor air flow rate for this " );
						ShowContinueError( "zone is zero. The Maximum Total Cooling Capacity will be autosized for zero outdoor air flow. " );
						ShowContinueError( "Check the outdoor air specifications in the Sizing:Zone object for zone " + FinalZoneSizing( CurZoneEqNum ).ZoneName + '.' );
					}
				} else {
					if ( PurchAir( PurchAirNum ).MaxCoolTotCap > 0.0 && MaxCoolTotCapDes > 0.0 ) {
						MaxCoolTotCapUser = PurchAir( PurchAirNum ).MaxCoolTotCap;
						ReportSizingOutput( PurchAir( PurchAirNum ).cObjectName, PurchAir( PurchAirNum ).Name, "Design Size Maximum Total Cooling Capacity [W]", MaxCoolTotCapDes, "User-Specified Maximum Total Cooling Capacity [W]", MaxCoolTotCapUser );
						if ( DisplayExtraWarnings ) {
							if ( ( std::abs( MaxCoolTotCapDes - MaxCoolTotCapUser ) / MaxCoolTotCapUser ) > AutoVsHardSizingThreshold ) {
								ShowMessage( "SizePurchasedAir: Potential issue with equipment sizing for " + PurchAir( PurchAirNum ).cObjectName + ' ' + PurchAir( PurchAirNum ).Name );
								ShowContinueError( "User-Specified Maximum Total Cooling Capacity of " + RoundSigDigits( MaxCoolTotCapUser, 2 ) + " [W]" );
								ShowContinueError( "differs from Design Size Maximum Total Cooling Capacity of " + RoundSigDigits( MaxCoolTotCapDes, 2 ) + " [W]" );
								ShowContinueError( "This may, or may not, indicate mismatched component sizes." );
								ShowContinueError( "Verify that the value entered is intended and is consistent with other components." );
							}
						}
					}
				}
			}
		}

		//      IF (PurchAir(PurchAirNum)%OutdoorAir .AND. PurchAir(PurchAirNum)%OutsideAirVolFlowRate == AutoSize) THEN
		//        IF (CurZoneEqNum > 0) THEN
		//          CALL CheckZoneSizing(TRIM(PurchAir(PurchAirNum)%cObjectName), PurchAir(PurchAirNum)%Name)
		//          PurchAir(PurchAirNum)%OutsideAirVolFlowRate = FinalZoneSizing(CurZoneEqNum)%MinOA
		//          IF (PurchAir(PurchAirNum)%OutsideAirVolFlowRate < SmallAirVolFlow) THEN
		//            PurchAir(PurchAirNum)%OutsideAirVolFlowRate = 0.0
		//          END IF
		//          CALL ReportSizingOutput(TRIM(PurchAir(PurchAirNum)%cObjectName), PurchAir(PurchAirNum)%Name, &
		//                              'Outdoor Air Flow Rate [m3/s]', PurchAir(PurchAirNum)%OutsideAirVolFlowRate )
		//        END IF
		//      END IF

	}

	void
	CalcPurchAirLoads(
		int const PurchAirNum,
		Real64 & SysOutputProvided, // Sensible output provided [W] cooling = negative
		Real64 & MoistOutputProvided, // Moisture output provided [kg/s] dehumidification = negative
		int const ControlledZoneNum,
		int const ActualZoneNum
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Russ Taylor
		//       DATE WRITTEN   Nov 1997
		//       MODIFIED       Shirey, Aug 2009 (LatOutputProvided - now MoistOutputProvided)
		//                      M. Witte June 2011, add new features including DCV, economizer, dehumidification
		//                          and humidification,
		//                      July 2012, Chandan Sharma - FSEC: Added hybrid ventilation manager
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Needs description.

		// METHODOLOGY EMPLOYED:
		// Needs description, as appropriate.

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataZoneEnergyDemands::ZoneSysEnergyDemand;
		using DataZoneEnergyDemands::ZoneSysMoistureDemand;
		using DataLoopNode::Node;
		using DataZoneEquipment::ZoneEquipConfig;
		using DataHVACGlobals::SmallLoad;
		using DataHVACGlobals::ZoneComp;
		using DataHVACGlobals::ForceOff;
		using DataHeatBalance::Zone;
		using DataHeatBalFanSys::TempControlType;
		using General::TrimSigDigits;
		using DataContaminantBalance::Contaminant;
		using DataZoneEquipment::PurchasedAir_Num;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "CalcPurchAirLoads" );

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int InNodeNum; // Ideal loads supply node to zone
		//         INTEGER   :: ExhNodeNum        ! Ideal loads exhaust node from zone
		int ZoneNodeNum; // Zone air node
		int OANodeNum; // Outdoor air inlet node
		int RecircNodeNum; // Return air or zone exhaust node
		int OperatingMode; // current operating mode, Off, Heat, Cool, or DeadBand
		Real64 SupplyMassFlowRate; // System supply air mass flow rate [kg/s]
		Real64 SupplyMassFlowRateForHumid; // System supply air mass flow rate required to meet humdification load [kg/s]
		Real64 SupplyMassFlowRateForDehum; // System supply air mass flow rate required to meet dehumidification load [kg/s]
		Real64 SupplyMassFlowRateForCool; // System supply air mass flow rate required to meet sensible cooling load[kg/s]
		Real64 SupplyMassFlowRateForHeat; // System supply air mass flow rate required to meet sensible heating load[kg/s]
		Real64 SupplyHumRatForHumid; // Supply air humidity ratio require to meet the humidification load [kgH2O/kgAir]
		Real64 SupplyHumRatForDehum; // Supply air humidity ratio require to meet the dehumidification load [kgH2O/kgAir]
		Real64 OAMassFlowRate; // Outdoor air mass flow rate [kg/s]
		Real64 OAVolFlowRate; // Outdoor air volume flow rate at standard density [m3/s]
		Real64 MinOASensOutput; // Minimum Outdoor air sensible output [W], <0 means OA is cooler than zone air
		Real64 MinOALatOutput; // Minimum Outdoor air moisture load [kg/s]
		Real64 SensOutput; // Sensible output [W] (psitive means heating, negative means cooling)
		Real64 HeatSensOutput; // Heating sensible output [W]
		Real64 CoolSensOutput; // Cooling sensible output [W] (positive value menas cooling)
		Real64 LatOutput; // Latent output [W] (positive value means hudmification, negative means dehumidification)
		Real64 CoolLatOutput; // Cooling latent output [W] (positive value means dehumidification)
		Real64 CoolTotOutput; // Cooling total output [W] (positive value means cooling)
		Real64 DeltaT; // Delta temperature - reused in multiple places
		Real64 DeltaHumRat; // Delta humidity ratio - reused in multiple places
		Real64 QZnHeatSP; // Load required to meet heating setpoint [W] (>0 is a heating load)
		Real64 QZnCoolSP; // Load required to meet cooling setpoint [W] (<0 is a cooling load)
		Real64 MdotZnHumidSP; // Load required to meet humidifying setpoint [kg H2O/s] (>0 = a humidify load)
		Real64 MdotZnDehumidSP; // Load required to meet dehumidifying setpoint [kg H2O/s] (<0 = a dehumidify load)
		bool UnitOn;
		bool HeatOn; // Flag for heating and humidification availbility schedule, true if heating is on
		bool CoolOn; // Flag for cooling and dehumidification availbility schedule, true if cooling is on
		bool EconoOn; // Flag for economizer operation, true if economizer is on
		Real64 SupplyTemp; // Supply inlet to zone dry bulb temperature [C]
		Real64 SupplyHumRat; // Supply inlet to zone humidity ratio [kg H2O/kg Air]
		Real64 SupplyHumRatOrig; // Supply inlet to zone humidity ratio before saturation check [kg H2O/kg Air]
		Real64 SupplyHumRatSat; // Supply inlet to zone humidity ratio saturation at SupplyTemp [kg H2O/kg Air]
		Real64 SupplyEnthalpy; // Supply inlet to zone enthalpy [J/kg]
		Real64 MixedAirTemp; // Mixed air dry bulb temperature [C]
		Real64 MixedAirHumRat; // Mixed air humidity ratio [kg H2O/kg Air]
		Real64 MixedAirEnthalpy; // Mixed air enthalpy [J/kg]
		Real64 CpAir; // Specific heat [J/kg-C] reused in multiple places
		//         REAL(r64) :: SpecHumOut   ! Specific humidity ratio of outlet air (kg moisture / kg moist air)
		//         REAL(r64) :: SpecHumIn    ! Specific humidity ratio of inlet [zone] air (kg moisture / kg moist air)

		// Sign convention: SysOutputProvided <0 Supply air is heated on entering zone (zone is cooled)
		//                  SysOutputProvided >0 Supply air is cooled on entering zone (zone is heated)
		InNodeNum = PurchAir( PurchAirNum ).ZoneSupplyAirNodeNum;
		ZoneNodeNum = ZoneEquipConfig( ControlledZoneNum ).ZoneNode;
		OANodeNum = PurchAir( PurchAirNum ).OutdoorAirNodeNum;
		RecircNodeNum = PurchAir( PurchAirNum ).ZoneRecircAirNodeNum;
		SupplyMassFlowRate = 0.0;
		OAMassFlowRate = 0.0;
		PurchAir( PurchAirNum ).MinOAMassFlowRate = 0.0;
		PurchAir( PurchAirNum ).TimeEconoActive = 0.0;
		PurchAir( PurchAirNum ).TimeHtRecActive = 0.0;
		SysOutputProvided = 0.0;
		MoistOutputProvided = 0.0;
		CoolSensOutput = 0.0;
		CoolLatOutput = 0.0;
		CoolTotOutput = 0.0;
		HeatSensOutput = 0.0;
		LatOutput = 0.0;

		// default unit to ON
		UnitOn = true;
		EconoOn = false;
		// get current zone requirements
		QZnHeatSP = ZoneSysEnergyDemand( ActualZoneNum ).RemainingOutputReqToHeatSP;
		QZnCoolSP = ZoneSysEnergyDemand( ActualZoneNum ).RemainingOutputReqToCoolSP;

		if ( allocated( ZoneComp ) ) {
			ZoneComp( PurchasedAir_Num ).ZoneCompAvailMgrs( PurchAirNum ).ZoneNum = ActualZoneNum;
			PurchAir( PurchAirNum ).AvailStatus = ZoneComp( PurchasedAir_Num ).ZoneCompAvailMgrs( PurchAirNum ).AvailStatus;
			// Check if the hybrid ventilation availability manager is turning the unit off
			if ( PurchAir( PurchAirNum ).AvailStatus == ForceOff ) {
				UnitOn = false;
			}
		}

		// Check if the unit is scheduled off
		//         IF (PurchAir(PurchAirNum)%AvailSchedPtr > 0) THEN
		if ( GetCurrentScheduleValue( PurchAir( PurchAirNum ).AvailSchedPtr ) <= 0 ) {
			UnitOn = false;
		}
		//         END IF
		// Check if heating and cooling available
		HeatOn = true;
		//         IF (PurchAir(PurchAirNum)%HeatSchedPtr > 0) THEN
		if ( GetCurrentScheduleValue( PurchAir( PurchAirNum ).HeatSchedPtr ) <= 0 ) {
			HeatOn = false;
		}
		//         END IF
		CoolOn = true;
		//         IF (PurchAir(PurchAirNum)%CoolSchedPtr > 0) THEN
		if ( GetCurrentScheduleValue( PurchAir( PurchAirNum ).CoolSchedPtr ) <= 0 ) {
			CoolOn = false;
		}
		//         END IF

		if ( UnitOn ) {
			// Calculate current minimum outdoor air flow rate based on design OA specifications and DCV or CO2 control
			CalcPurchAirMinOAMassFlow( PurchAirNum, ActualZoneNum, OAMassFlowRate );

			// EMS override point  Purch air outdoor air massflow rate.....
			if ( PurchAir( PurchAirNum ).EMSOverrideOAMdotOn ) {
				OAMassFlowRate = PurchAir( PurchAirNum ).EMSValueOAMassFlowRate;
			}

			// Calculate minimum outdoor air sensible and latent load
			if ( PurchAir( PurchAirNum ).OutdoorAir ) {
				CpAir = PsyCpAirFnWTdb( Node( OANodeNum ).HumRat, Node( OANodeNum ).Temp );
				MinOASensOutput = OAMassFlowRate * CpAir * ( Node( OANodeNum ).Temp - Node( ZoneNodeNum ).Temp );
				MinOALatOutput = OAMassFlowRate * ( Node( OANodeNum ).HumRat - Node( ZoneNodeNum ).HumRat );
			} else {
				MinOASensOutput = 0.0;
				MinOALatOutput = 0.0;
			}
			SupplyMassFlowRate = OAMassFlowRate;

			// Check if cooling of the supply air stream is required

			// Cooling operation
			if ( ( MinOASensOutput >= QZnCoolSP ) && ( TempControlType( ActualZoneNum ) != SingleHeatingSetPoint ) ) {
				OperatingMode = Cool;
				// Calculate supply mass flow, temp and humidity with the following constraints:
				//  Min cooling supply temp
				//  Max total cooling capacity
				//  Max cooling airflow
				//  Min cooling supply humrat  (and Max heating supply humrat)
				//  Min OA mass flow rate

				// Check if OA flow rate greater than max cooling airflow limit
				if ( ( ( PurchAir( PurchAirNum ).CoolingLimit == LimitFlowRate ) || ( PurchAir( PurchAirNum ).CoolingLimit == LimitFlowRateAndCapacity ) ) && ( OAMassFlowRate > PurchAir( PurchAirNum ).MaxCoolMassFlowRate ) ) {
					OAVolFlowRate = OAMassFlowRate / StdRhoAir;
					if ( PurchAir( PurchAirNum ).OAFlowMaxCoolOutputError < 1 ) {
						++PurchAir( PurchAirNum ).OAFlowMaxCoolOutputError;
						ShowWarningError( PurchAir( PurchAirNum ).cObjectName + " \"" + PurchAir( PurchAirNum ).Name + "\" Requested outdoor air flow rate = " + TrimSigDigits( OAVolFlowRate, 5 ) + " [m3/s] exceeds limit." );
						ShowContinueError( " Will be reduced to the Maximum Cooling Air Flow Rate = " + TrimSigDigits( PurchAir( PurchAirNum ).MaxCoolVolFlowRate, 5 ) + " [m3/s]" );
						ShowContinueErrorTimeStamp( "" );
					} else {
						ShowRecurringWarningErrorAtEnd( PurchAir( PurchAirNum ).cObjectName + " \"" + PurchAir( PurchAirNum ).Name + "\" Requested outdoor air flow rate [m3/s] reduced to Maximum Cooling Air Flow Rate warning continues...", PurchAir( PurchAirNum ).OAFlowMaxCoolOutputIndex, OAVolFlowRate );
					}
					OAMassFlowRate = PurchAir( PurchAirNum ).MaxCoolMassFlowRate;

				} else {
					// Model economizer
					if ( PurchAir( PurchAirNum ).EconomizerType != NoEconomizer ) {
						if ( ( ( PurchAir( PurchAirNum ).EconomizerType == DifferentialDryBulb ) && ( Node( OANodeNum ).Temp < Node( PurchAir( PurchAirNum ).ZoneRecircAirNodeNum ).Temp ) ) || ( ( PurchAir( PurchAirNum ).EconomizerType == DifferentialEnthalpy ) && ( Node( OANodeNum ).Enthalpy < Node( PurchAir( PurchAirNum ).ZoneRecircAirNodeNum ).Enthalpy ) ) ) {

							// Calculate supply MassFlowRate based on sensible load but limit to Max Cooling Supply Air Flow Rate if specified
							CpAir = PsyCpAirFnWTdb( ZoneAirHumRat( ActualZoneNum ), Node( ZoneNodeNum ).Temp );
							DeltaT = ( Node( OANodeNum ).Temp - Node( ZoneNodeNum ).Temp );
							if ( DeltaT < -SmallTempDiff ) {
								SupplyMassFlowRate = QZnCoolSP / CpAir / DeltaT;
								if ( ( ( PurchAir( PurchAirNum ).CoolingLimit == LimitFlowRate ) || ( PurchAir( PurchAirNum ).CoolingLimit == LimitFlowRateAndCapacity ) ) && ( PurchAir( PurchAirNum ).MaxCoolMassFlowRate > 0.0 ) ) {
									SupplyMassFlowRate = min( max( SupplyMassFlowRate, 0.0 ), PurchAir( PurchAirNum ).MaxCoolMassFlowRate );
								}
								if ( SupplyMassFlowRate > OAMassFlowRate ) {
									EconoOn = true;
									OAMassFlowRate = SupplyMassFlowRate;
									PurchAir( PurchAirNum ).TimeEconoActive = TimeStepSys;
								}
							}
						}
					}
				}

				// Determine supply mass flow rate
				// Mass flow rate to meet sensible load, at Minimum Cooling Supply Air Temperature
				SupplyMassFlowRateForCool = 0.0;
				if ( CoolOn ) {
					CpAir = PsyCpAirFnWTdb( ZoneAirHumRat( ActualZoneNum ), Node( ZoneNodeNum ).Temp );
					DeltaT = ( PurchAir( PurchAirNum ).MinCoolSuppAirTemp - Node( ZoneNodeNum ).Temp );
					if ( DeltaT < -SmallTempDiff ) {
						SupplyMassFlowRateForCool = QZnCoolSP / CpAir / DeltaT;
					}
				}

				// Mass flow rate to meet dehumidification load, if applicable, at Minimum Cooling Supply Humidity Ratio
				SupplyMassFlowRateForDehum = 0.0;
				if ( CoolOn ) {
					if ( PurchAir( PurchAirNum ).DehumidCtrlType == Humidistat ) {
						MdotZnDehumidSP = ZoneSysMoistureDemand( ActualZoneNum ).RemainingOutputReqToDehumidSP;
						DeltaHumRat = ( PurchAir( PurchAirNum ).MinCoolSuppAirHumRat - Node( ZoneNodeNum ).HumRat );
						if ( ( DeltaHumRat < -SmallDeltaHumRat ) && ( MdotZnDehumidSP < 0.0 ) ) {
							SupplyMassFlowRateForDehum = MdotZnDehumidSP / DeltaHumRat;
						}
					}
				}

				// Mass flow rate to meet humidification load, if applicable, at Maximum Heating Supply Humidity Ratio
				// This section is the cooling section, so humidification should activate only if humidification control = humidistat
				//   and if dehumidification control = humidistat or none
				SupplyMassFlowRateForHumid = 0.0;
				if ( HeatOn ) {
					if ( PurchAir( PurchAirNum ).HumidCtrlType == Humidistat ) {
						if ( ( PurchAir( PurchAirNum ).DehumidCtrlType == Humidistat ) || ( PurchAir( PurchAirNum ).DehumidCtrlType == None ) ) {
							MdotZnHumidSP = ZoneSysMoistureDemand( ActualZoneNum ).RemainingOutputReqToHumidSP;
							DeltaHumRat = ( PurchAir( PurchAirNum ).MaxHeatSuppAirHumRat - Node( ZoneNodeNum ).HumRat );
							if ( ( DeltaHumRat > SmallDeltaHumRat ) && ( MdotZnHumidSP > 0.0 ) ) {
								SupplyMassFlowRateForHumid = MdotZnHumidSP / DeltaHumRat;
							}
						}
					}
				}

				// Supply mass flow is greatest of these, but limit to cooling max flow rate, if applicable
				SupplyMassFlowRate = max( 0.0, OAMassFlowRate, SupplyMassFlowRateForCool, SupplyMassFlowRateForDehum, SupplyMassFlowRateForHumid );
				// EMS override point  Purch air massflow rate..... but only if unit is on, i.e. SupplyMassFlowRate>0.0
				if ( ( PurchAir( PurchAirNum ).EMSOverrideMdotOn ) && ( SupplyMassFlowRate > 0.0 ) ) {
					SupplyMassFlowRate = PurchAir( PurchAirNum ).EMSValueMassFlowRate;
					OAMassFlowRate = min( OAMassFlowRate, SupplyMassFlowRate );
				}
				if ( ( ( PurchAir( PurchAirNum ).CoolingLimit == LimitFlowRate ) || ( PurchAir( PurchAirNum ).CoolingLimit == LimitFlowRateAndCapacity ) ) && ( PurchAir( PurchAirNum ).MaxCoolMassFlowRate > 0.0 ) ) {
					SupplyMassFlowRate = min( SupplyMassFlowRate, PurchAir( PurchAirNum ).MaxCoolMassFlowRate );
				}

				if ( SupplyMassFlowRate <= VerySmallMassFlow ) SupplyMassFlowRate = 0.0;

				// Calculate mixed air conditions
				CalcPurchAirMixedAir( PurchAirNum, OAMassFlowRate, SupplyMassFlowRate, MixedAirTemp, MixedAirHumRat, MixedAirEnthalpy, OperatingMode );

				// Calculate supply air conditions using final massflow rate, imposing capacity limits if specified
				// If capacity limits are exceeded, keep massflow rate where it is and adjust supply temp
				// In general, in the cooling section, don't let SupplyTemp be set to something that results in heating
				if ( SupplyMassFlowRate > 0.0 ) {
					// Calculate supply temp at SupplyMassFlowRate and recheck limit on Minimum Cooling Supply Air Temperature
					CpAir = PsyCpAirFnWTdb( ZoneAirHumRat( ActualZoneNum ), Node( ZoneNodeNum ).Temp );
					SupplyTemp = QZnCoolSP / ( CpAir * SupplyMassFlowRate ) + Node( ZoneNodeNum ).Temp;
					SupplyTemp = max( SupplyTemp, PurchAir( PurchAirNum ).MinCoolSuppAirTemp );
					// This is the cooling mode, so SupplyTemp can't be more than MixedAirTemp
					SupplyTemp = min( SupplyTemp, MixedAirTemp );
					SupplyHumRat = MixedAirHumRat;
					SupplyEnthalpy = PsyHFnTdbW( SupplyTemp, SupplyHumRat );

					// Check sensible load vs max total cooling capacity, if specified, and adjust supply temp before applying humidity controls
					// Will check again later, too
					if ( ( PurchAir( PurchAirNum ).CoolingLimit == LimitCapacity ) || ( PurchAir( PurchAirNum ).CoolingLimit == LimitFlowRateAndCapacity ) ) {
						CpAir = PsyCpAirFnWTdb( MixedAirHumRat, MixedAirTemp );
						CoolSensOutput = SupplyMassFlowRate * ( MixedAirEnthalpy - SupplyEnthalpy );
						if ( CoolSensOutput >= PurchAir( PurchAirNum ).MaxCoolTotCap ) {
							CoolSensOutput = PurchAir( PurchAirNum ).MaxCoolTotCap;
							SupplyEnthalpy = MixedAirEnthalpy - CoolSensOutput / SupplyMassFlowRate;
							SupplyTemp = PsyTdbFnHW( SupplyEnthalpy, SupplyHumRat );
							// This is the cooling mode, so SupplyTemp can't be more than MixedAirTemp
							SupplyTemp = min( SupplyTemp, MixedAirTemp );
						} // Capacity limit exceeded
					}

					// Set supply humidity ratio for cooling/dehumidification
					SupplyHumRat = MixedAirHumRat;
					{ auto const SELECT_CASE_var( PurchAir( PurchAirNum ).DehumidCtrlType );
					if ( SELECT_CASE_var == None ) {
						SupplyHumRat = MixedAirHumRat;
					} else if ( SELECT_CASE_var == ConstantSensibleHeatRatio ) {
						// SHR = CoolSensOutput/CoolTotOutput
						// CoolTotOutput = CoolSensOutput/SHR
						CpAir = PsyCpAirFnWTdb( MixedAirHumRat, MixedAirTemp );
						CoolSensOutput = SupplyMassFlowRate * CpAir * ( MixedAirTemp - SupplyTemp );
						CoolTotOutput = CoolSensOutput / PurchAir( PurchAirNum ).CoolSHR;
						SupplyEnthalpy = MixedAirEnthalpy - CoolTotOutput / SupplyMassFlowRate;
						//  Limit for overdrying (avoid Pysch errors which occur if SupplyEnthalpy is too low for SupplyTemp)
						SupplyEnthalpy = max( SupplyEnthalpy, PsyHFnTdbW( SupplyTemp, 0.00001 ) );
						SupplyHumRat = min( SupplyHumRat, PsyWFnTdbH( SupplyTemp, SupplyEnthalpy, RoutineName ) );
						// Apply min cooling humidity ratio limit
						SupplyHumRat = max( SupplyHumRat, PurchAir( PurchAirNum ).MinCoolSuppAirHumRat );
						// But don't let it be higher than incoming MixedAirHumRat
						SupplyHumRat = min( SupplyHumRat, MixedAirHumRat );
					} else if ( SELECT_CASE_var == Humidistat ) {
						MdotZnDehumidSP = ZoneSysMoistureDemand( ActualZoneNum ).RemainingOutputReqToDehumidSP;
						SupplyHumRatForDehum = MdotZnDehumidSP / SupplyMassFlowRate + Node( ZoneNodeNum ).HumRat;
						SupplyHumRatForDehum = min( SupplyHumRatForDehum, PurchAir( PurchAirNum ).MinCoolSuppAirHumRat );
						SupplyHumRat = min( MixedAirHumRat, SupplyHumRatForDehum );
					} else if ( SELECT_CASE_var == ConstantSupplyHumidityRatio ) {
						SupplyHumRat = PurchAir( PurchAirNum ).MinCoolSuppAirHumRat;
					} else {
						SupplyHumRat = MixedAirHumRat;
					}}

					// Check supply humidity ratio for humidification (SupplyHumRatForHum should always be < SupplyHumRatForDehum)
					// This section is the cooling section, so humidification should activate only if humidification control = humidistat
					//   and if dehumidification control = humidistat or none
					if ( HeatOn ) {
						if ( PurchAir( PurchAirNum ).HumidCtrlType == Humidistat ) {
							if ( ( PurchAir( PurchAirNum ).DehumidCtrlType == Humidistat ) || ( PurchAir( PurchAirNum ).DehumidCtrlType == None ) ) {
								MdotZnHumidSP = ZoneSysMoistureDemand( ActualZoneNum ).RemainingOutputReqToHumidSP;
								SupplyHumRatForHumid = MdotZnHumidSP / SupplyMassFlowRate + Node( ZoneNodeNum ).HumRat;
								SupplyHumRatForHumid = min( SupplyHumRatForHumid, PurchAir( PurchAirNum ).MaxHeatSuppAirHumRat );
								SupplyHumRat = max( SupplyHumRat, SupplyHumRatForHumid );
							}
						}
					}

					//   Limit supply humidity ratio to saturation at supply outlet temp
					SupplyHumRatOrig = SupplyHumRat;
					SupplyHumRatSat = PsyWFnTdbRhPb( SupplyTemp, 1.0, OutBaroPress, RoutineName );
					SupplyHumRat = min( SupplyHumRatOrig, SupplyHumRatSat );
					SupplyEnthalpy = PsyHFnTdbW( SupplyTemp, SupplyHumRat );

					// Check max total Cooling capacity, if specified
					if ( ( PurchAir( PurchAirNum ).CoolingLimit == LimitCapacity ) || ( PurchAir( PurchAirNum ).CoolingLimit == LimitFlowRateAndCapacity ) ) {
						// If dehumidifying, compare total cooling to the limit
						if ( SupplyHumRat < MixedAirHumRat ) { // Dehumidifying
							CoolTotOutput = SupplyMassFlowRate * ( MixedAirEnthalpy - SupplyEnthalpy );
							if ( ( CoolTotOutput ) > PurchAir( PurchAirNum ).MaxCoolTotCap ) {
								CoolTotOutput = PurchAir( PurchAirNum ).MaxCoolTotCap;
								SupplyEnthalpy = MixedAirEnthalpy - CoolTotOutput / SupplyMassFlowRate;
								// Adjust output based on dehumidification control type
								{ auto const SELECT_CASE_var( PurchAir( PurchAirNum ).DehumidCtrlType );
								if ( SELECT_CASE_var == ConstantSensibleHeatRatio ) {
									// Adjust both supply temp and humidity ratio to maintain SHR
									// SHR = CoolSensOutput/CoolTotOutput
									// CoolSensOutput = SHR*CoolTotOutput
									CpAir = PsyCpAirFnWTdb( MixedAirHumRat, MixedAirTemp );
									CoolSensOutput = CoolTotOutput * PurchAir( PurchAirNum ).CoolSHR;
									SupplyTemp = MixedAirTemp - CoolSensOutput / ( CpAir * SupplyMassFlowRate );
									// This is the cooling mode, so SupplyTemp can't be more than MixedAirTemp
									SupplyTemp = min( SupplyTemp, MixedAirTemp );
									//  Limit for overdrying (avoid Pysch errors which occur if SupplyEnthalpy is too low for SupplyTemp)
									SupplyEnthalpy = max( SupplyEnthalpy, PsyHFnTdbW( SupplyTemp, 0.00001 ) );
									SupplyHumRat = PsyWFnTdbH( SupplyTemp, SupplyEnthalpy, RoutineName );
								} else if ( SELECT_CASE_var == Humidistat ) {
									// Keep supply temp and adjust humidity ratio to reduce load
									SupplyHumRat = PsyWFnTdbH( SupplyTemp, SupplyEnthalpy, RoutineName );
								} else if ( ( SELECT_CASE_var == None ) || ( SELECT_CASE_var == ConstantSupplyHumidityRatio ) ) {
									// Keep humidity ratio and adjust supply temp
									// Check if latent output exceeds capacity
									CpAir = PsyCpAirFnWTdb( MixedAirHumRat, MixedAirTemp );
									CoolSensOutput = SupplyMassFlowRate * CpAir * ( MixedAirTemp - SupplyTemp );
									CoolLatOutput = CoolTotOutput - CoolSensOutput;
									if ( CoolLatOutput >= PurchAir( PurchAirNum ).MaxCoolTotCap ) {
										SupplyTemp = MixedAirTemp;
										SupplyHumRat = PsyWFnTdbH( SupplyTemp, SupplyEnthalpy, RoutineName );
										CoolLatOutput = PurchAir( PurchAirNum ).MaxCoolTotCap;
									} else {
										SupplyTemp = PsyTdbFnHW( SupplyEnthalpy, SupplyHumRat );
										// This is the cooling mode, so SupplyTemp can't be more than MixedAirTemp
										SupplyTemp = min( SupplyTemp, MixedAirTemp );
									}
								}}
								// Limit supply humidity ratio to saturation at supply outlet temp
								// If saturation exceeded, then honor capacity limit and set to dew point at supplyenthalpy
								SupplyHumRatOrig = SupplyHumRat;
								SupplyHumRatSat = PsyWFnTdbRhPb( SupplyTemp, 1.0, OutBaroPress, RoutineName );
								if ( SupplyHumRatSat < SupplyHumRatOrig ) {
									SupplyTemp = PsyTsatFnHPb( SupplyEnthalpy, OutBaroPress, RoutineName );
									// This is the cooling mode, so SupplyTemp can't be more than MixedAirTemp
									SupplyTemp = min( SupplyTemp, MixedAirTemp );
									SupplyHumRat = PsyWFnTdbH( SupplyTemp, SupplyEnthalpy, RoutineName );
									SupplyEnthalpy = PsyHFnTdbW( SupplyTemp, SupplyHumRat );
									// CpAir = PsyCpAirFnWTdb(MixedAirHumRat,MixedAirTemp)
									// CoolSensOutput = SupplyMassFlowRate * CpAir * (MixedAirTemp - SupplyTemp)
									// CoolTotOutput = SupplyMassFlowRate * (MixedAirEnthalpy - SupplyEnthalpy)
								}
							} // Capacity limit exceeded
						} else { // Not dehumidifying
							// If not dehumidifying, compare sensible cooling to the limit
							// This section will only increase supply temp, so no need to recheck for super-saturation
							CpAir = PsyCpAirFnWTdb( MixedAirHumRat, MixedAirTemp );
							CoolSensOutput = SupplyMassFlowRate * CpAir * ( MixedAirTemp - SupplyTemp );
							if ( CoolSensOutput >= PurchAir( PurchAirNum ).MaxCoolTotCap ) {
								CoolSensOutput = PurchAir( PurchAirNum ).MaxCoolTotCap;
								SupplyTemp = MixedAirTemp - CoolSensOutput / ( SupplyMassFlowRate * CpAir );
							} // Capacity limit exceeded
						} // Dehumidifying or not
					} // Capacity limit active

				} else { // SupplyMassFlowRate is zero
					SupplyEnthalpy = MixedAirEnthalpy;
					SupplyHumRat = MixedAirHumRat;
					SupplyTemp = MixedAirTemp;
					CoolSensOutput = 0.0;
					CoolTotOutput = 0.0;
				}
				// Heating or no-load operation
			} else { // Heating or no-load case
				if ( ( MinOASensOutput < QZnHeatSP ) && ( TempControlType( ActualZoneNum ) != SingleCoolingSetPoint ) ) {
					OperatingMode = Heat;
				} else { // DeadBand mode shuts off heat recovery and economizer
					OperatingMode = DeadBand;
				}
				// Calculate supply mass flow, temp and humidity with the following constraints:
				//  Max heating supply temp
				//  Max sensible heating capacity
				//  Max heating airflow
				//  Max heating supply humrat (and Min cooling supply humrat)
				//  Min OA mass flow rate

				// Check if OA flow rate greater than max heating airflow limit
				if ( ( ( PurchAir( PurchAirNum ).HeatingLimit == LimitFlowRate ) || ( PurchAir( PurchAirNum ).HeatingLimit == LimitFlowRateAndCapacity ) ) && ( OAMassFlowRate > PurchAir( PurchAirNum ).MaxHeatMassFlowRate ) ) {
					OAVolFlowRate = OAMassFlowRate / StdRhoAir;
					if ( PurchAir( PurchAirNum ).OAFlowMaxHeatOutputError < 1 ) {
						++PurchAir( PurchAirNum ).OAFlowMaxHeatOutputError;
						ShowWarningError( PurchAir( PurchAirNum ).cObjectName + " \"" + PurchAir( PurchAirNum ).Name + "\" Requested outdoor air flow rate = " + TrimSigDigits( OAVolFlowRate, 5 ) + " [m3/s] exceeds limit." );
						ShowContinueError( " Will be reduced to the Maximum Heating Air Flow Rate = " + TrimSigDigits( PurchAir( PurchAirNum ).MaxHeatVolFlowRate, 5 ) + " [m3/s]" );
						ShowContinueErrorTimeStamp( "" );
					} else {
						ShowRecurringWarningErrorAtEnd( PurchAir( PurchAirNum ).cObjectName + " \"" + PurchAir( PurchAirNum ).Name + "\" Requested outdoor air flow rate [m3/s] reduced to Maximum Heating Air Flow Rate warning continues...", PurchAir( PurchAirNum ).OAFlowMaxHeatOutputIndex, OAVolFlowRate );
					}
					OAMassFlowRate = PurchAir( PurchAirNum ).MaxHeatMassFlowRate;
				}

				SupplyMassFlowRate = OAMassFlowRate;

				// Determine supply mass flow rate
				// Mass flow rate to meet sensible load, at Minimum Cooling Supply Air Temperature
				SupplyMassFlowRateForHeat = 0.0;
				if ( ( HeatOn ) && ( OperatingMode == Heat ) ) {
					CpAir = PsyCpAirFnWTdb( ZoneAirHumRat( ActualZoneNum ), Node( ZoneNodeNum ).Temp );
					DeltaT = ( PurchAir( PurchAirNum ).MaxHeatSuppAirTemp - Node( ZoneNodeNum ).Temp );
					if ( DeltaT > SmallTempDiff ) {
						SupplyMassFlowRateForHeat = QZnHeatSP / CpAir / DeltaT;
					}
				}

				// Mass flow rate to meet dehumidification load, if applicable, at Minimum Cooling Supply Humidity Ratio
				// This section is the heating/deadband section, so dehumidification should activate
				//   only if dehumidification control = humidistat
				//   and if humidification control = humidistat or none or if operating in deadband mode
				SupplyMassFlowRateForDehum = 0.0;
				if ( CoolOn ) {
					if ( PurchAir( PurchAirNum ).DehumidCtrlType == Humidistat ) {
						if ( ( PurchAir( PurchAirNum ).HumidCtrlType == Humidistat ) || ( PurchAir( PurchAirNum ).HumidCtrlType == None ) || ( OperatingMode == DeadBand ) ) {
							MdotZnDehumidSP = ZoneSysMoistureDemand( ActualZoneNum ).RemainingOutputReqToDehumidSP;
							DeltaHumRat = ( PurchAir( PurchAirNum ).MinCoolSuppAirHumRat - Node( ZoneNodeNum ).HumRat );
							if ( ( DeltaHumRat < -SmallDeltaHumRat ) && ( MdotZnDehumidSP < 0.0 ) ) {
								SupplyMassFlowRateForDehum = MdotZnDehumidSP / DeltaHumRat;
							}
						}
					}
				}

				// Mass flow rate to meet humidification load, if applicable, at Maximum Heating Supply Humidity Ratio
				SupplyMassFlowRateForHumid = 0.0;
				if ( HeatOn ) {
					if ( PurchAir( PurchAirNum ).HumidCtrlType == Humidistat ) {
						MdotZnHumidSP = ZoneSysMoistureDemand( ActualZoneNum ).RemainingOutputReqToHumidSP;
						DeltaHumRat = ( PurchAir( PurchAirNum ).MaxHeatSuppAirHumRat - Node( ZoneNodeNum ).HumRat );
						if ( ( DeltaHumRat > SmallDeltaHumRat ) && ( MdotZnHumidSP > 0.0 ) ) {
							SupplyMassFlowRateForHumid = MdotZnHumidSP / DeltaHumRat;
						}
					}
				}

				// Supply mass flow is greatest of these, but limit to heating max flow rate, if applicable
				SupplyMassFlowRate = max( 0.0, OAMassFlowRate, SupplyMassFlowRateForHeat, SupplyMassFlowRateForDehum, SupplyMassFlowRateForHumid );
				// EMS override point  Purch air massflow rate..... but only if unit is on, i.e. SupplyMassFlowRate>0.0
				if ( ( PurchAir( PurchAirNum ).EMSOverrideMdotOn ) && ( SupplyMassFlowRate > 0.0 ) ) {
					SupplyMassFlowRate = PurchAir( PurchAirNum ).EMSValueMassFlowRate;
					OAMassFlowRate = min( OAMassFlowRate, SupplyMassFlowRate );
				}
				if ( ( ( PurchAir( PurchAirNum ).HeatingLimit == LimitFlowRate ) || ( PurchAir( PurchAirNum ).HeatingLimit == LimitFlowRateAndCapacity ) ) && ( PurchAir( PurchAirNum ).MaxHeatMassFlowRate > 0.0 ) ) {
					SupplyMassFlowRate = min( SupplyMassFlowRate, PurchAir( PurchAirNum ).MaxHeatMassFlowRate );
				}

				if ( SupplyMassFlowRate <= VerySmallMassFlow ) SupplyMassFlowRate = 0.0;

				// Calculate mixed air conditions
				CalcPurchAirMixedAir( PurchAirNum, OAMassFlowRate, SupplyMassFlowRate, MixedAirTemp, MixedAirHumRat, MixedAirEnthalpy, OperatingMode );

				// Calculate supply air conditions using final massflow rate, imposing capacity limits if specified
				// If capacity limits are exceeded, keep massflow rate where it is and adjust supply temp
				if ( SupplyMassFlowRate > 0.0 ) {
					if ( ( HeatOn ) && ( OperatingMode == Heat ) ) {
						// Calculate supply temp at SupplyMassFlowRate and check limit on Maximum Heating Supply Air Temperature
						CpAir = PsyCpAirFnWTdb( ZoneAirHumRat( ActualZoneNum ), Node( ZoneNodeNum ).Temp );
						SupplyTemp = QZnHeatSP / ( CpAir * SupplyMassFlowRate ) + Node( ZoneNodeNum ).Temp;
						SupplyTemp = min( SupplyTemp, PurchAir( PurchAirNum ).MaxHeatSuppAirTemp );
						// This is the heating mode, so SupplyTemp can't be less than MixedAirTemp
						SupplyTemp = max( SupplyTemp, MixedAirTemp );
						// Check max heating capacity, if specified
						if ( ( PurchAir( PurchAirNum ).HeatingLimit == LimitCapacity ) || ( PurchAir( PurchAirNum ).HeatingLimit == LimitFlowRateAndCapacity ) ) {
							CpAir = PsyCpAirFnWTdb( MixedAirHumRat, MixedAirTemp );
							HeatSensOutput = SupplyMassFlowRate * CpAir * ( SupplyTemp - MixedAirTemp );
							if ( HeatSensOutput > PurchAir( PurchAirNum ).MaxHeatSensCap ) {
								SupplyTemp = PurchAir( PurchAirNum ).MaxHeatSensCap / ( SupplyMassFlowRate * CpAir ) + MixedAirTemp;
								HeatSensOutput = PurchAir( PurchAirNum ).MaxHeatSensCap;
							}
						}
					} else { // Heat is off or operating mode is deadband (i.e. don't do any heating)
						SupplyTemp = MixedAirTemp;
					}

					// Set supply humidity ratio first for heating/humidification
					SupplyHumRat = MixedAirHumRat;
					{ auto const SELECT_CASE_var( PurchAir( PurchAirNum ).HumidCtrlType );
					if ( SELECT_CASE_var == None ) {
						SupplyHumRat = MixedAirHumRat;
					} else if ( SELECT_CASE_var == Humidistat ) {
						MdotZnHumidSP = ZoneSysMoistureDemand( ActualZoneNum ).RemainingOutputReqToHumidSP;
						SupplyHumRatForHumid = MdotZnHumidSP / SupplyMassFlowRate + Node( ZoneNodeNum ).HumRat;
						SupplyHumRatForHumid = min( SupplyHumRatForHumid, PurchAir( PurchAirNum ).MaxHeatSuppAirHumRat );
						SupplyHumRat = max( SupplyHumRat, SupplyHumRatForHumid );
					} else if ( SELECT_CASE_var == ConstantSupplyHumidityRatio ) {
						if ( OperatingMode == Heat ) {
							// If this results in dehumidification, must check cooling capacity limit
							if ( MixedAirHumRat > PurchAir( PurchAirNum ).MaxHeatSuppAirHumRat ) {
								if ( ( PurchAir( PurchAirNum ).CoolingLimit == LimitCapacity ) || ( PurchAir( PurchAirNum ).CoolingLimit == LimitFlowRateAndCapacity ) ) {
									SupplyHumRat = PurchAir( PurchAirNum ).MaxHeatSuppAirHumRat;
									SupplyEnthalpy = PsyHFnTdbW( SupplyTemp, SupplyHumRat );
									CoolTotOutput = SupplyMassFlowRate * ( MixedAirEnthalpy - SupplyEnthalpy );
									CpAir = PsyCpAirFnWTdb( MixedAirHumRat, MixedAirTemp );
									CoolSensOutput = SupplyMassFlowRate * CpAir * ( MixedAirTemp - SupplyTemp );
									CoolLatOutput = CoolTotOutput - CoolSensOutput;
									if ( CoolLatOutput >= PurchAir( PurchAirNum ).MaxCoolTotCap ) {
										CoolLatOutput = PurchAir( PurchAirNum ).MaxCoolTotCap;
										CoolTotOutput = CoolSensOutput + CoolLatOutput;
										SupplyEnthalpy = MixedAirEnthalpy - CoolTotOutput / SupplyMassFlowRate;
										SupplyHumRat = PsyWFnTdbH( SupplyTemp, SupplyEnthalpy, RoutineName );
									}
								} else {
									SupplyHumRat = PurchAir( PurchAirNum ).MaxHeatSuppAirHumRat;
								}
							} else {
								SupplyHumRat = PurchAir( PurchAirNum ).MaxHeatSuppAirHumRat;
							}
						} else {
							SupplyHumRat = MixedAirHumRat;
						}
					} else {
						SupplyHumRat = MixedAirHumRat;
					}}
					SupplyEnthalpy = PsyHFnTdbW( SupplyTemp, SupplyHumRat );

					// Check supply humidity ratio for dehumidification (SupplyHumRatForHumid should always be < SupplyHumRatForDehum)
					// This section is the heating/deadband section, so dehumidification should activate
					//   only if dehumidification control = humidistat
					//   and if humidification control = humidistat or none or if operating in deadband mode
					if ( CoolOn ) {
						if ( PurchAir( PurchAirNum ).DehumidCtrlType == Humidistat ) {
							if ( ( PurchAir( PurchAirNum ).HumidCtrlType == Humidistat ) || ( PurchAir( PurchAirNum ).HumidCtrlType == None ) || ( OperatingMode == DeadBand ) ) {
								MdotZnDehumidSP = ZoneSysMoistureDemand( ActualZoneNum ).RemainingOutputReqToDehumidSP;
								SupplyHumRatForDehum = MdotZnDehumidSP / SupplyMassFlowRate + Node( ZoneNodeNum ).HumRat;
								SupplyHumRatForDehum = max( SupplyHumRatForDehum, PurchAir( PurchAirNum ).MinCoolSuppAirHumRat );
								SupplyHumRat = min( SupplyHumRat, SupplyHumRatForDehum );
								SupplyEnthalpy = PsyHFnTdbW( SupplyTemp, SupplyHumRat );
								if ( SupplyHumRat < MixedAirHumRat ) {
									// At this point, the system is heating or deadband but dehumidifying, check max cooling cap limit
									CpAir = PsyCpAirFnWTdb( MixedAirHumRat, MixedAirTemp );
									SensOutput = SupplyMassFlowRate * CpAir * ( SupplyTemp - MixedAirTemp );
									LatOutput = SupplyMassFlowRate * ( SupplyEnthalpy - MixedAirEnthalpy ) - SensOutput;
									if ( ( PurchAir( PurchAirNum ).CoolingLimit == LimitCapacity ) || ( PurchAir( PurchAirNum ).CoolingLimit == LimitFlowRateAndCapacity ) ) {
										if ( LatOutput > PurchAir( PurchAirNum ).MaxCoolTotCap ) {
											LatOutput = PurchAir( PurchAirNum ).MaxCoolTotCap;
											SupplyEnthalpy = MixedAirEnthalpy + ( LatOutput + SensOutput ) / SupplyMassFlowRate;
											SupplyHumRat = PsyWFnTdbH( SupplyTemp, SupplyEnthalpy, RoutineName );
										}
									}
								}
							}
						}
					}

					//   Limit supply humidity ratio to saturation at supply outlet temp
					SupplyHumRatOrig = SupplyHumRat;
					SupplyHumRat = min( SupplyHumRat, PsyWFnTdbRhPb( SupplyTemp, 1.0, OutBaroPress, RoutineName ) );
					SupplyEnthalpy = PsyHFnTdbW( SupplyTemp, SupplyHumRat );

				} else { // SupplyMassFlowRate is zero
					SupplyEnthalpy = MixedAirEnthalpy;
					SupplyHumRat = MixedAirHumRat;
					SupplyTemp = MixedAirTemp;
					HeatSensOutput = 0.0;
				}

			} // Cooling or heating required

			// EMS override point  Purch air supply temp and humidty ratio ..... but only if unit is on, SupplyMassFlowRate>0.0
			if ( ( PurchAir( PurchAirNum ).EMSOverrideSupplyTempOn ) && ( SupplyMassFlowRate > 0.0 ) ) {
				SupplyTemp = PurchAir( PurchAirNum ).EMSValueSupplyTemp;
			}
			if ( ( PurchAir( PurchAirNum ).EMSOverrideSupplyHumRatOn ) && ( SupplyMassFlowRate > 0.0 ) ) {
				SupplyHumRat = PurchAir( PurchAirNum ).EMSValueSupplyHumRat;
			}

			if ( SupplyMassFlowRate > 0.0 ) {
				PurchAir( PurchAirNum ).FinalMixedAirTemp = MixedAirTemp;
				PurchAir( PurchAirNum ).FinalMixedAirHumRat = MixedAirHumRat;
				// compute coil loads
				if ( ( SupplyHumRat == MixedAirHumRat ) && ( SupplyTemp == MixedAirTemp ) ) {
					// If no change in humrat or temp, then set loads to zero
					PurchAir( PurchAirNum ).SenCoilLoad = 0.0;
					PurchAir( PurchAirNum ).LatCoilLoad = 0.0;
				} else if ( ( SupplyHumRat == MixedAirHumRat ) && ( SupplyTemp != MixedAirTemp ) ) {
					// If no change in humrat, then set latent load to zero and use enthalpies to calculate sensible load
					PurchAir( PurchAirNum ).SenCoilLoad = SupplyMassFlowRate * ( SupplyEnthalpy - MixedAirEnthalpy );
					PurchAir( PurchAirNum ).LatCoilLoad = 0.0;
				} else {
					CpAir = PsyCpAirFnWTdb( MixedAirHumRat, MixedAirTemp );
					PurchAir( PurchAirNum ).SenCoilLoad = SupplyMassFlowRate * CpAir * ( SupplyTemp - MixedAirTemp );
					PurchAir( PurchAirNum ).LatCoilLoad = SupplyMassFlowRate * ( SupplyEnthalpy - MixedAirEnthalpy ) - PurchAir( PurchAirNum ).SenCoilLoad;
				}

				// Apply heating and cooling availability schedules to sensible load
				if ( ( ( PurchAir( PurchAirNum ).SenCoilLoad > 0.0 ) && ! HeatOn ) || ( ( PurchAir( PurchAirNum ).SenCoilLoad < 0.0 ) && ! CoolOn ) ) {
					// Coil is off
					PurchAir( PurchAirNum ).SenCoilLoad = 0.0;
					SupplyTemp = MixedAirTemp;
				}

				// Apply heating and cooling availability schedules to latent load
				if ( ( ( PurchAir( PurchAirNum ).LatCoilLoad > 0.0 ) && ! HeatOn ) || ( ( PurchAir( PurchAirNum ).LatCoilLoad < 0.0 ) && ! CoolOn ) ) {
					// Coil is off
					PurchAir( PurchAirNum ).LatCoilLoad = 0.0;
					SupplyHumRat = MixedAirHumRat;
				}

				// Double-check if saturation exceeded, then thow warning, shouldn't happen here, don't reset, just warn
				SupplyHumRatOrig = SupplyHumRat;
				SupplyHumRatSat = PsyWFnTdbRhPb( SupplyTemp, 1.0, OutBaroPress, RoutineName );
				DeltaHumRat = SupplyHumRatOrig - SupplyHumRatSat;
				if ( DeltaHumRat > SmallDeltaHumRat ) {
					if ( PurchAir( PurchAirNum ).SaturationOutputError < 1 ) {
						++PurchAir( PurchAirNum ).SaturationOutputError;
						ShowWarningError( PurchAir( PurchAirNum ).cObjectName + " \"" + PurchAir( PurchAirNum ).Name + "\" Supply humidity ratio = " + TrimSigDigits( SupplyHumRatOrig, 5 ) + " exceeds saturation limit " + TrimSigDigits( SupplyHumRatSat, 5 ) + " [kgWater/kgDryAir]" );
						ShowContinueError( " Simulation continuing . . . " );
						ShowContinueErrorTimeStamp( "" );
					} else {
						ShowRecurringWarningErrorAtEnd( PurchAir( PurchAirNum ).cObjectName + " \"" + PurchAir( PurchAirNum ).Name + "\" Supply humidity ratio exceeds saturation limit warning continues, delta max/min [kgWater/kgDryAir]...", PurchAir( PurchAirNum ).SaturationOutputIndex, DeltaHumRat, DeltaHumRat );
					}
				}

				SupplyEnthalpy = PsyHFnTdbW( SupplyTemp, SupplyHumRat );

				CpAir = PsyCpAirFnWTdb( ZoneAirHumRat( ActualZoneNum ), Node( ZoneNodeNum ).Temp );
				SysOutputProvided = SupplyMassFlowRate * CpAir * ( SupplyTemp - Node( ZoneNodeNum ).Temp );
				MoistOutputProvided = SupplyMassFlowRate * ( SupplyHumRat - Node( ZoneNodeNum ).HumRat ); // Latent rate, kg/s

				PurchAir( PurchAirNum ).SenOutputToZone = SysOutputProvided;
				PurchAir( PurchAirNum ).LatOutputToZone = SupplyMassFlowRate * ( SupplyEnthalpy - Node( ZoneNodeNum ).Enthalpy ) - PurchAir( PurchAirNum ).SenOutputToZone;

				CpAir = PsyCpAirFnWTdb( ZoneAirHumRat( ActualZoneNum ), Node( ZoneNodeNum ).Temp );
				if ( PurchAir( PurchAirNum ).OutdoorAir ) {
					PurchAir( PurchAirNum ).OASenOutput = OAMassFlowRate * CpAir * ( Node( OANodeNum ).Temp - Node( ZoneNodeNum ).Temp );
					PurchAir( PurchAirNum ).OALatOutput = OAMassFlowRate * ( Node( OANodeNum ).Enthalpy - Node( ZoneNodeNum ).Enthalpy ) - PurchAir( PurchAirNum ).OASenOutput;
				} else {
					PurchAir( PurchAirNum ).OASenOutput = 0.0;
					PurchAir( PurchAirNum ).OALatOutput = 0.0;
				}
				if ( Contaminant.CO2Simulation ) {
					if ( PurchAir( PurchAirNum ).OutdoorAir ) {
						Node( InNodeNum ).CO2 = ( ( SupplyMassFlowRate - OAMassFlowRate ) * Node( RecircNodeNum ).CO2 + OAMassFlowRate * Node( OANodeNum ).CO2 ) / SupplyMassFlowRate;
					} else {
						Node( InNodeNum ).CO2 = Node( RecircNodeNum ).CO2;
					}
				}
				if ( Contaminant.GenericContamSimulation ) {
					if ( PurchAir( PurchAirNum ).OutdoorAir ) {
						Node( InNodeNum ).GenContam = ( ( SupplyMassFlowRate - OAMassFlowRate ) * Node( RecircNodeNum ).GenContam + OAMassFlowRate * Node( OANodeNum ).GenContam ) / SupplyMassFlowRate;
					} else {
						Node( InNodeNum ).GenContam = Node( RecircNodeNum ).GenContam;
					}
				}
			} else { // SupplyMassFlowRate = 0.0
				SysOutputProvided = 0.0;
				MoistOutputProvided = 0.0;

				PurchAir( PurchAirNum ).SenOutputToZone = 0.0;
				PurchAir( PurchAirNum ).LatOutputToZone = 0.0;
				PurchAir( PurchAirNum ).SenCoilLoad = 0.0;
				PurchAir( PurchAirNum ).LatCoilLoad = 0.0;
				PurchAir( PurchAirNum ).OASenOutput = 0.0;
				PurchAir( PurchAirNum ).OALatOutput = 0.0;
				PurchAir( PurchAirNum ).FinalMixedAirTemp = Node( RecircNodeNum ).Temp;
				PurchAir( PurchAirNum ).FinalMixedAirHumRat = Node( RecircNodeNum ).HumRat;
				if ( Contaminant.CO2Simulation ) {
					Node( InNodeNum ).CO2 = Node( ZoneNodeNum ).CO2;
				}
				if ( Contaminant.GenericContamSimulation ) {
					Node( InNodeNum ).GenContam = Node( ZoneNodeNum ).GenContam;
				}
			}

			Node( InNodeNum ).Temp = SupplyTemp;
			Node( InNodeNum ).HumRat = SupplyHumRat;
			Node( InNodeNum ).Enthalpy = SupplyEnthalpy;
			Node( InNodeNum ).MassFlowRate = SupplyMassFlowRate;
			if ( PurchAir( PurchAirNum ).OutdoorAir ) Node( OANodeNum ).MassFlowRate = OAMassFlowRate;

		} else { // purchased air OFF

			SysOutputProvided = 0.0;
			MoistOutputProvided = 0.0;
			SupplyMassFlowRate = 0.0;
			OAMassFlowRate = 0.0;
			Node( InNodeNum ).Temp = Node( ZoneNodeNum ).Temp;
			Node( InNodeNum ).HumRat = Node( ZoneNodeNum ).HumRat;
			Node( InNodeNum ).Enthalpy = Node( ZoneNodeNum ).Enthalpy;
			if ( Contaminant.CO2Simulation ) {
				Node( InNodeNum ).CO2 = Node( ZoneNodeNum ).CO2;
			}
			if ( Contaminant.GenericContamSimulation ) {
				Node( InNodeNum ).GenContam = Node( ZoneNodeNum ).GenContam;
			}

			Node( InNodeNum ).MassFlowRate = 0.0;
			if ( PurchAir( PurchAirNum ).OutdoorAir ) Node( OANodeNum ).MassFlowRate = 0.0;
			PurchAir( PurchAirNum ).SenHeatRate = 0.0;
			PurchAir( PurchAirNum ).SenCoolRate = 0.0;
			PurchAir( PurchAirNum ).TotCoolRate = 0.0;

			PurchAir( PurchAirNum ).SenOutputToZone = 0.0;
			PurchAir( PurchAirNum ).LatOutputToZone = 0.0;
			PurchAir( PurchAirNum ).SenCoilLoad = 0.0;
			PurchAir( PurchAirNum ).LatCoilLoad = 0.0;
			PurchAir( PurchAirNum ).OASenOutput = 0.0;
			PurchAir( PurchAirNum ).OALatOutput = 0.0;
			PurchAir( PurchAirNum ).FinalMixedAirTemp = Node( RecircNodeNum ).Temp;
			PurchAir( PurchAirNum ).FinalMixedAirHumRat = Node( RecircNodeNum ).HumRat;

		}

		PurchAir( PurchAirNum ).OutdoorAirMassFlowRate = OAMassFlowRate;
		PurchAir( PurchAirNum ).OutdoorAirVolFlowRateStdRho = OAMassFlowRate / StdRhoAir;
		PurchAir( PurchAirNum ).SupplyAirMassFlowRate = SupplyMassFlowRate;
		PurchAir( PurchAirNum ).SupplyAirVolFlowRateStdRho = SupplyMassFlowRate / StdRhoAir;

	}

	void
	CalcPurchAirMinOAMassFlow(
		int const PurchAirNum, // index to ideal loads unit
		int const ActualZoneNum, // index to actual zone number
		Real64 & OAMassFlowRate // outside air mass flow rate [kg/s] from volume flow using std density
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         M. Witte (GARD)
		//       DATE WRITTEN   Jun 2011 (taken from HVACSingleDuctSystem.cc and adapted for Ideal Loads System)
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Calculates the amount of outside air required based on optional user input.
		// Zone multipliers have been applied in GetInput.

		// METHODOLOGY EMPLOYED:
		// User input defines method used to calculate OA.

		// REFERENCES:

		// Using/Aliasing
		using DataHeatBalance::Zone;
		using DataEnvironment::StdRhoAir;
		using DataContaminantBalance::ZoneSysContDemand;
		using DataZoneEquipment::CalcDesignSpecificationOutdoorAir;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		bool const UseMinOASchFlag( true ); // Always use min OA schedule in calculations.

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		bool UseOccSchFlag; // TRUE = use actual occupancy, FALSE = use total zone people
		Real64 OAVolumeFlowRate; // outside air flow rate (m3/s)

		if ( PurchAir( PurchAirNum ).OutdoorAir ) {

			if ( PurchAir( PurchAirNum ).DCVType == OccupancySchedule ) {
				UseOccSchFlag = true;
			} else {
				UseOccSchFlag = false;
			}
			OAVolumeFlowRate = CalcDesignSpecificationOutdoorAir( PurchAir( PurchAirNum ).OARequirementsPtr, ActualZoneNum, UseOccSchFlag, UseMinOASchFlag );
			OAMassFlowRate = OAVolumeFlowRate * StdRhoAir;

			// If DCV with CO2SetPoint then check required OA flow to meet CO2 setpoint
			if ( PurchAir( PurchAirNum ).DCVType == CO2SetPoint ) {
				OAMassFlowRate = max( OAMassFlowRate, ZoneSysContDemand( ActualZoneNum ).OutputRequiredToCO2SP );
			}

			if ( OAMassFlowRate <= VerySmallMassFlow ) OAMassFlowRate = 0.0;

		} else { //No outdoor air
			OAMassFlowRate = 0.0;
		}
		PurchAir( PurchAirNum ).MinOAMassFlowRate = OAMassFlowRate;

	}

	void
	CalcPurchAirMixedAir(
		int const PurchAirNum, // index to ideal loads unit
		Real64 const OAMassFlowRate, // outside air mass flow rate [kg/s]
		Real64 const SupplyMassFlowRate, // supply air mass flow rate [kg/s]
		Real64 & MixedAirTemp, // Mixed air dry bulb temperature [C]
		Real64 & MixedAirHumRat, // Mixed air humidity ratio [kg H2O/kg Air]
		Real64 & MixedAirEnthalpy, // Mixed air enthalpy [J/kg]
		int const OperatingMode // current operating mode, Off, Heating, Cooling, or DeadBand
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         M. Witte (GARD)
		//       DATE WRITTEN   Sep 2011
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Calculates the mixed air conditions, accounting for heat recovery.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:

		// Using/Aliasing
		using DataLoopNode::Node;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "CalcPurchAirMixedAir" );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int RecircNodeNum; // Zone return air node
		int OANodeNum; // Outdoor air inlet node
		Real64 RecircTemp; // Recirculated air from zone dry bulb temperature [C]
		Real64 RecircHumRat; // Recirculated air from zone humidity ratio [kg H2O/kg Air]
		Real64 RecircEnthalpy; // Recirculated air from zone enthalpy [J/kg]
		Real64 RecircMassFlowRate; // Recirculated air mass flow rate [kg/s]
		Real64 OAInletTemp; // Outdoor air inlet dry bulb temperature [C]
		Real64 OAInletHumRat; // Outdoor air inlet humidity ratio [kg H2O/kg Air]
		Real64 OAInletEnthalpy; // Outdoor air inlet enthalpy [J/kg]
		Real64 OAAfterHtRecTemp; // Outdoor air after heat recovery to mixing box dry bulb temperature [C]
		Real64 OAAfterHtRecHumRat; // Outdoor air after heat recovery to mixing box humidity ratio [kg H2O/kg Air]
		Real64 OAAfterHtRecEnthalpy; // Outdoor air after heat recovery to mixing box enthalpy [J/kg]
		bool HeatRecOn;
		Real64 CpAir; // Specific heat [J/kg-C] reused in multiple places

		// Initializations
		OANodeNum = PurchAir( PurchAirNum ).OutdoorAirNodeNum;
		RecircNodeNum = PurchAir( PurchAirNum ).ZoneRecircAirNodeNum;

		RecircMassFlowRate = 0.0;
		RecircTemp = Node( RecircNodeNum ).Temp;
		RecircHumRat = Node( RecircNodeNum ).HumRat;
		RecircEnthalpy = Node( RecircNodeNum ).Enthalpy;
		if ( PurchAir( PurchAirNum ).OutdoorAir ) {
			OAInletTemp = Node( OANodeNum ).Temp;
			OAInletHumRat = Node( OANodeNum ).HumRat;
			OAInletEnthalpy = Node( OANodeNum ).Enthalpy;
			OAAfterHtRecTemp = OAInletTemp;
			OAAfterHtRecHumRat = OAInletHumRat;
			OAAfterHtRecEnthalpy = OAInletEnthalpy;
		} else {
			OAInletTemp = 0.0;
			OAInletHumRat = 0.0;
			OAInletEnthalpy = 0.0;
			OAAfterHtRecTemp = OAInletTemp;
			OAAfterHtRecHumRat = OAInletHumRat;
			OAAfterHtRecEnthalpy = OAInletEnthalpy;
		}
		HeatRecOn = false;

		if ( PurchAir( PurchAirNum ).OutdoorAir && ( OAMassFlowRate > 0.0 ) ) {
			// Determine if heat recovery is beneficial
			if ( PurchAir( PurchAirNum ).HtRecType == Sensible ) {
				if ( ( OperatingMode == Heat ) && ( RecircTemp > OAInletTemp ) ) HeatRecOn = true;
				if ( ( OperatingMode == Cool ) && ( RecircTemp < OAInletTemp ) ) HeatRecOn = true;
			}
			if ( PurchAir( PurchAirNum ).HtRecType == Enthalpy ) {
				if ( ( OperatingMode == Heat ) && ( RecircEnthalpy > OAInletEnthalpy ) ) HeatRecOn = true;
				if ( ( OperatingMode == Cool ) && ( RecircEnthalpy < OAInletEnthalpy ) ) HeatRecOn = true;
			}
			// Calculate heat recovery if active
			if ( HeatRecOn ) {
				PurchAir( PurchAirNum ).TimeHtRecActive = TimeStepSys;
				OAAfterHtRecTemp = OAInletTemp + PurchAir( PurchAirNum ).HtRecSenEff * ( RecircTemp - OAInletTemp );
				if ( PurchAir( PurchAirNum ).HtRecType == Enthalpy ) OAAfterHtRecHumRat = OAInletHumRat + PurchAir( PurchAirNum ).HtRecLatEff * ( RecircHumRat - OAInletHumRat );
				OAAfterHtRecEnthalpy = PsyHFnTdbW( OAAfterHtRecTemp, OAAfterHtRecHumRat );
				//   Check for saturation in supply outlet and reset temp, then humidity ratio at constant enthalpy
				if ( PsyTsatFnHPb( OAAfterHtRecEnthalpy, OutBaroPress, RoutineName ) > OAAfterHtRecTemp ) {
					OAAfterHtRecTemp = PsyTsatFnHPb( OAAfterHtRecEnthalpy, OutBaroPress, RoutineName );
					OAAfterHtRecHumRat = PsyWFnTdbH( OAAfterHtRecTemp, OAAfterHtRecEnthalpy, RoutineName );
				}
			}

			if ( SupplyMassFlowRate > OAMassFlowRate ) {
				RecircMassFlowRate = SupplyMassFlowRate - OAMassFlowRate;
				MixedAirEnthalpy = ( RecircMassFlowRate * Node( RecircNodeNum ).Enthalpy + OAMassFlowRate * OAAfterHtRecEnthalpy ) / SupplyMassFlowRate;
				MixedAirHumRat = ( RecircMassFlowRate * Node( RecircNodeNum ).HumRat + OAMassFlowRate * OAAfterHtRecHumRat ) / SupplyMassFlowRate;
				// Mixed air temperature is calculated from the mixed air enthalpy and humidity ratio.
				MixedAirTemp = PsyTdbFnHW( MixedAirEnthalpy, MixedAirHumRat );
			} else {
				RecircMassFlowRate = 0.0;
				MixedAirEnthalpy = OAAfterHtRecEnthalpy;
				MixedAirHumRat = OAAfterHtRecHumRat;
				MixedAirTemp = OAAfterHtRecTemp;
			}

			// Calculate OA and heat recovery sensible and latent rates
			CpAir = PsyCpAirFnWTdb( OAInletHumRat, OAInletTemp );
			PurchAir( PurchAirNum ).HtRecSenOutput = OAMassFlowRate * CpAir * ( OAAfterHtRecTemp - OAInletTemp );
			PurchAir( PurchAirNum ).HtRecLatOutput = OAMassFlowRate * ( OAAfterHtRecEnthalpy - OAInletEnthalpy ) - PurchAir( PurchAirNum ).HtRecSenOutput;

		} else { // No outdoor air
			RecircMassFlowRate = SupplyMassFlowRate;
			MixedAirTemp = RecircTemp;
			MixedAirHumRat = RecircHumRat;
			MixedAirEnthalpy = RecircEnthalpy;
			PurchAir( PurchAirNum ).HtRecSenOutput = 0.0;
			PurchAir( PurchAirNum ).HtRecLatOutput = 0.0;
		}
		// If exhaust node is specified, then set massflow on exhaust node, otherwise return node sets its own massflow
		if ( PurchAir( PurchAirNum ).ZoneExhaustAirNodeNum > 0 ) {
			Node( RecircNodeNum ).MassFlowRate = RecircMassFlowRate;
		}

	}

	void
	UpdatePurchasedAir( int const EP_UNUSED( PurchAirNum ) )
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         M. J. Witte
		//       DATE WRITTEN   Sep 2011
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Update node data for Ideal Loads (purchased air) system

		// METHODOLOGY EMPLOYED:

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		// na

	}

	void
	ReportPurchasedAir( int const PurchAirNum )
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Russ Taylor
		//       DATE WRITTEN   Nov 1997
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Calculate values of report variables, if necessary.

		// METHODOLOGY EMPLOYED:

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataHVACGlobals::TimeStepSys;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 ReportingConstant;

		// Sort out heating and cooling rates
		PurchAir( PurchAirNum ).SenHeatRate = max( PurchAir( PurchAirNum ).SenCoilLoad, 0.0 );
		PurchAir( PurchAirNum ).SenCoolRate = std::abs( min( PurchAir( PurchAirNum ).SenCoilLoad, 0.0 ) );
		PurchAir( PurchAirNum ).LatHeatRate = max( PurchAir( PurchAirNum ).LatCoilLoad, 0.0 );
		PurchAir( PurchAirNum ).LatCoolRate = std::abs( min( PurchAir( PurchAirNum ).LatCoilLoad, 0.0 ) );
		PurchAir( PurchAirNum ).TotHeatRate = PurchAir( PurchAirNum ).SenHeatRate + PurchAir( PurchAirNum ).LatHeatRate;
		PurchAir( PurchAirNum ).TotCoolRate = PurchAir( PurchAirNum ).SenCoolRate + PurchAir( PurchAirNum ).LatCoolRate;

		PurchAir( PurchAirNum ).ZoneSenHeatRate = max( PurchAir( PurchAirNum ).SenOutputToZone, 0.0 );
		PurchAir( PurchAirNum ).ZoneSenCoolRate = std::abs( min( PurchAir( PurchAirNum ).SenOutputToZone, 0.0 ) );
		PurchAir( PurchAirNum ).ZoneLatHeatRate = max( PurchAir( PurchAirNum ).LatOutputToZone, 0.0 );
		PurchAir( PurchAirNum ).ZoneLatCoolRate = std::abs( min( PurchAir( PurchAirNum ).LatOutputToZone, 0.0 ) );
		PurchAir( PurchAirNum ).ZoneTotHeatRate = PurchAir( PurchAirNum ).ZoneSenHeatRate + PurchAir( PurchAirNum ).ZoneLatHeatRate;
		PurchAir( PurchAirNum ).ZoneTotCoolRate = PurchAir( PurchAirNum ).ZoneSenCoolRate + PurchAir( PurchAirNum ).ZoneLatCoolRate;

		// Sort out outdoor air "loads"
		// OASenOutput = Outdoor air sensible output relative to zone conditions [W], <0 means OA is cooler than zone air
		// OALatOutput  = Outdoor air latent output relative to zone conditions [W], <0 means OA is drier than zone air
		if ( PurchAir( PurchAirNum ).SenCoilLoad > 0.0 ) { // Heating is active
			PurchAir( PurchAirNum ).OASenHeatRate = std::abs( min( PurchAir( PurchAirNum ).OASenOutput, 0.0 ) );
		} else {
			PurchAir( PurchAirNum ).OASenHeatRate = 0.0;
		}
		if ( PurchAir( PurchAirNum ).SenCoilLoad < 0.0 ) { // Cooling is active
			PurchAir( PurchAirNum ).OASenCoolRate = max( PurchAir( PurchAirNum ).OASenOutput, 0.0 );
		} else {
			PurchAir( PurchAirNum ).OASenCoolRate = 0.0;
		}
		if ( PurchAir( PurchAirNum ).LatCoilLoad > 0.0 ) { // Humidification is active
			PurchAir( PurchAirNum ).OALatHeatRate = std::abs( min( PurchAir( PurchAirNum ).OALatOutput, 0.0 ) );
		} else {
			PurchAir( PurchAirNum ).OALatHeatRate = 0.0;
		}
		if ( PurchAir( PurchAirNum ).LatCoilLoad < 0.0 ) { // Dehumidification is active
			PurchAir( PurchAirNum ).OALatCoolRate = max( PurchAir( PurchAirNum ).OALatOutput, 0.0 );
		} else {
			PurchAir( PurchAirNum ).OALatCoolRate = 0.0;
		}

		PurchAir( PurchAirNum ).OATotHeatRate = PurchAir( PurchAirNum ).OASenHeatRate + PurchAir( PurchAirNum ).OALatHeatRate;
		PurchAir( PurchAirNum ).OATotCoolRate = PurchAir( PurchAirNum ).OASenCoolRate + PurchAir( PurchAirNum ).OALatCoolRate;

		PurchAir( PurchAirNum ).HtRecSenHeatRate = max( PurchAir( PurchAirNum ).HtRecSenOutput, 0.0 );
		PurchAir( PurchAirNum ).HtRecSenCoolRate = std::abs( min( PurchAir( PurchAirNum ).HtRecSenOutput, 0.0 ) );
		PurchAir( PurchAirNum ).HtRecLatHeatRate = max( PurchAir( PurchAirNum ).HtRecLatOutput, 0.0 );
		PurchAir( PurchAirNum ).HtRecLatCoolRate = std::abs( min( PurchAir( PurchAirNum ).HtRecLatOutput, 0.0 ) );
		PurchAir( PurchAirNum ).HtRecTotHeatRate = PurchAir( PurchAirNum ).HtRecSenHeatRate + PurchAir( PurchAirNum ).HtRecLatHeatRate;
		PurchAir( PurchAirNum ).HtRecTotCoolRate = PurchAir( PurchAirNum ).HtRecSenCoolRate + PurchAir( PurchAirNum ).HtRecLatCoolRate;

		ReportingConstant = TimeStepSys * SecInHour;

		PurchAir( PurchAirNum ).SenHeatEnergy = PurchAir( PurchAirNum ).SenHeatRate * ReportingConstant;
		PurchAir( PurchAirNum ).SenCoolEnergy = PurchAir( PurchAirNum ).SenCoolRate * ReportingConstant;
		PurchAir( PurchAirNum ).LatHeatEnergy = PurchAir( PurchAirNum ).LatHeatRate * ReportingConstant;
		PurchAir( PurchAirNum ).LatCoolEnergy = PurchAir( PurchAirNum ).LatCoolRate * ReportingConstant;
		PurchAir( PurchAirNum ).TotHeatEnergy = PurchAir( PurchAirNum ).TotHeatRate * ReportingConstant;
		PurchAir( PurchAirNum ).TotCoolEnergy = PurchAir( PurchAirNum ).TotCoolRate * ReportingConstant;

		PurchAir( PurchAirNum ).ZoneSenHeatEnergy = PurchAir( PurchAirNum ).ZoneSenHeatRate * ReportingConstant;
		PurchAir( PurchAirNum ).ZoneSenCoolEnergy = PurchAir( PurchAirNum ).ZoneSenCoolRate * ReportingConstant;
		PurchAir( PurchAirNum ).ZoneLatHeatEnergy = PurchAir( PurchAirNum ).ZoneLatHeatRate * ReportingConstant;
		PurchAir( PurchAirNum ).ZoneLatCoolEnergy = PurchAir( PurchAirNum ).ZoneLatCoolRate * ReportingConstant;
		PurchAir( PurchAirNum ).ZoneTotHeatEnergy = PurchAir( PurchAirNum ).ZoneTotHeatRate * ReportingConstant;
		PurchAir( PurchAirNum ).ZoneTotCoolEnergy = PurchAir( PurchAirNum ).ZoneTotCoolRate * ReportingConstant;

		PurchAir( PurchAirNum ).OASenHeatEnergy = PurchAir( PurchAirNum ).OASenHeatRate * ReportingConstant;
		PurchAir( PurchAirNum ).OASenCoolEnergy = PurchAir( PurchAirNum ).OASenCoolRate * ReportingConstant;
		PurchAir( PurchAirNum ).OALatHeatEnergy = PurchAir( PurchAirNum ).OALatHeatRate * ReportingConstant;
		PurchAir( PurchAirNum ).OALatCoolEnergy = PurchAir( PurchAirNum ).OALatCoolRate * ReportingConstant;
		PurchAir( PurchAirNum ).OATotHeatEnergy = PurchAir( PurchAirNum ).OATotHeatRate * ReportingConstant;
		PurchAir( PurchAirNum ).OATotCoolEnergy = PurchAir( PurchAirNum ).OATotCoolRate * ReportingConstant;

		PurchAir( PurchAirNum ).HtRecSenHeatEnergy = PurchAir( PurchAirNum ).HtRecSenHeatRate * ReportingConstant;
		PurchAir( PurchAirNum ).HtRecSenCoolEnergy = PurchAir( PurchAirNum ).HtRecSenCoolRate * ReportingConstant;
		PurchAir( PurchAirNum ).HtRecLatHeatEnergy = PurchAir( PurchAirNum ).HtRecLatHeatRate * ReportingConstant;
		PurchAir( PurchAirNum ).HtRecLatCoolEnergy = PurchAir( PurchAirNum ).HtRecLatCoolRate * ReportingConstant;
		PurchAir( PurchAirNum ).HtRecTotHeatEnergy = PurchAir( PurchAirNum ).HtRecTotHeatRate * ReportingConstant;
		PurchAir( PurchAirNum ).HtRecTotCoolEnergy = PurchAir( PurchAirNum ).HtRecTotCoolRate * ReportingConstant;

	}

	Real64
	GetPurchasedAirOutAirMassFlow( int const PurchAirNum )
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         B Griffith
		//       DATE WRITTEN   Dec  2006
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// lookup function for OA inlet mass flow for ventilation rate reporting

		// METHODOLOGY EMPLOYED:
		// most analagous functions look up an outside air node but this function
		// gets the actual mass flow of outdoor air, following the features of the model

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Return value
		Real64 OutAirMassFlow;

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		// na
		if ( GetPurchAirInputFlag ) {
			GetPurchasedAir();
			GetPurchAirInputFlag = false;
		}

		OutAirMassFlow = PurchAir( PurchAirNum ).OutdoorAirMassFlowRate;

		return OutAirMassFlow;

	}

	int
	GetPurchasedAirZoneInletAirNode( int const PurchAirNum )
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         B Griffith
		//       DATE WRITTEN   Dec  2006
		//       MODIFIED       Adapted for purchased air by M.J. Witte, Oct 2013
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// lookup function for zone inlet node for ventilation rate reporting

		// METHODOLOGY EMPLOYED:
		// <description>

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Return value
		int GetPurchasedAirZoneInletAirNode;

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		// na

		if ( GetPurchAirInputFlag ) {
			GetPurchasedAir();
			GetPurchAirInputFlag = false;
		}

		GetPurchasedAirZoneInletAirNode = 0;
		if ( PurchAirNum > 0 && PurchAirNum <= NumPurchAir ) {
			GetPurchasedAirZoneInletAirNode = PurchAir( PurchAirNum ).ZoneSupplyAirNodeNum;
		}

		return GetPurchasedAirZoneInletAirNode;

	}

	int
	GetPurchasedAirReturnAirNode( int const PurchAirNum )
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         B Griffith
		//       DATE WRITTEN   Dec  2006
		//       MODIFIED       Adapted for purchased air by M.J. Witte, Oct 2013
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// lookup function for recirculation air node for ventilation rate reporting

		// METHODOLOGY EMPLOYED:
		// <description>

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Return value
		int GetPurchasedAirReturnAirNode;

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		// na

		if ( GetPurchAirInputFlag ) {
			GetPurchasedAir();
			GetPurchAirInputFlag = false;
		}

		GetPurchasedAirReturnAirNode = 0;
		if ( PurchAirNum > 0 && PurchAirNum <= NumPurchAir ) {
			GetPurchasedAirReturnAirNode = PurchAir( PurchAirNum ).ZoneRecircAirNodeNum;
		}

		return GetPurchasedAirReturnAirNode;

	}

	Real64
	GetPurchasedAirMixedAirTemp( int const PurchAirNum )
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         B Griffith
		//       DATE WRITTEN   Dec  2006
		//       MODIFIED       Adapted for purchased air by M.J. Witte, Oct 2013
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// lookup function for mixed air Temp for ventilation rate reporting

		// METHODOLOGY EMPLOYED:
		// most analagous functions look up an outside air node but this function
		// gets the actual mass flow of outdoor air, following the features of the model

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Return value
		Real64 MixedAirTemp;

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		// na
		if ( GetPurchAirInputFlag ) {
			GetPurchasedAir();
			GetPurchAirInputFlag = false;
		}

		MixedAirTemp = PurchAir( PurchAirNum ).FinalMixedAirTemp;

		return MixedAirTemp;

	}

	Real64
	GetPurchasedAirMixedAirHumRat( int const PurchAirNum )
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         B Griffith
		//       DATE WRITTEN   Dec  2006
		//       MODIFIED       Adapted for purchased air by M.J. Witte, Oct 2013
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// lookup function for mixed air HumRat for ventilation rate reporting

		// METHODOLOGY EMPLOYED:
		// most analagous functions look up an outside air node but this function
		// gets the actual mass flow of outdoor air, following the features of the model

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Return value
		Real64 MixedAirHumRat;

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		// na
		if ( GetPurchAirInputFlag ) {
			GetPurchasedAir();
			GetPurchAirInputFlag = false;
		}

		MixedAirHumRat = PurchAir( PurchAirNum ).FinalMixedAirHumRat;

		return MixedAirHumRat;

	}

	// Clears the global data in Fans.
	// Needed for unit tests, should not be normally called.
	void
	clear_state()
	{
		NumPurchAir = 0;
		PurchAir.deallocate();
		PurchAirNumericFields.deallocate();
	}

} // PurchasedAirManager

} // EnergyPlus
