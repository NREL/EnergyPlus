// ObjexxFCL Headers
#include <ObjexxFCL/FArray.functions.hh>
#include <ObjexxFCL/Fmath.hh>

// EnergyPlus Headers
#include <DataZoneEquipment.hh>
#include <BranchNodeConnections.hh>
#include <DataHeatBalance.hh>
#include <DataHVACGlobals.hh>
#include <DataLoopNode.hh>
#include <DataPrecisionGlobals.hh>
#include <DataSizing.hh>
#include <General.hh>
#include <GeneralRoutines.hh>
#include <InputProcessor.hh>
#include <NodeInputManager.hh>
#include <ScheduleManager.hh>
#include <UtilityRoutines.hh>

namespace EnergyPlus {

namespace DataZoneEquipment {

	// MODULE INFORMATION
	//             AUTHOR:  Russ Taylor
	//       DATE WRITTEN:  June 1998

	// PURPOSE OF THIS MODULE:
	// This module contains variable declarations for zone equipment configuration data

	// Using/Aliasing
	using namespace DataPrecisionGlobals;
	using DataGlobals::NumOfZones;

	// Data
	// -only module should be available to other modules and routines.
	// Thus, all variables in this module must be PUBLIC.

	// MODULE PARAMETER DEFINITIONS:
	int const PathInlet( 1 );
	int const CompInlet( 2 );
	int const Intermediate( 3 );
	int const Outlet( 4 );

	int const ZoneSplitter_Type( 1 );
	int const ZoneSupplyPlenum_Type( 2 );
	int const ZoneMixer_Type( 3 );
	int const ZoneReturnPlenum_Type( 4 );

	// Start zone equip objects
	// list units that are valid for zone system availability managers first
	int const FanCoil4Pipe_Num( 1 );
	int const PkgTermHPAirToAir_Num( 2 );
	int const PkgTermACAirToAir_Num( 3 );
	int const PkgTermHPWaterToAir_Num( 4 );
	int const WindowAC_Num( 5 );
	int const UnitHeater_Num( 6 );
	int const UnitVentilator_Num( 7 );
	int const ERVStandAlone_Num( 8 );
	int const VentilatedSlab_Num( 9 );
	int const OutdoorAirUnit_Num( 10 );
	int const VRFTerminalUnit_Num( 11 );
	int const PurchasedAir_Num( 12 );
	int const ZoneEvaporativeCoolerUnit_Num( 13 );
	int const AirDistUnit_Num( 14 );
	int const DirectAir_Num( 15 );
	int const BBWaterConvective_Num( 16 );
	int const BBElectricConvective_Num( 17 );
	int const HiTempRadiant_Num( 18 );
	int const LoTempRadiant_Num( 19 );
	int const ZoneExhaustFan_Num( 20 );
	int const HeatXchngr_Num( 21 );
	int const HPWaterHeater_Num( 22 );
	int const BBWater_Num( 23 );
	int const ZoneDXDehumidifier_Num( 24 );
	int const BBSteam_Num( 25 );
	int const BBElectric_Num( 26 );
	int const RefrigerationAirChillerSet_Num( 27 );
	int const UserDefinedZoneHVACForcedAir_Num( 28 );
	int const ZoneUnitarySystem_Num( 29 ); // AirloopHVAC:UnitarySystem configured as zone equipment
	int const TotalNumZoneEquipType( 29 );
	// **NOTE**... if you add another zone equipment object, then increment
	// TotalNumZoneEquipType above to match the total number of zone equipment types
	// End zone equip objects

	int const NumValidSysAvailZoneComponents( 13 );
	FArray1D_string const cValidSysAvailManagerCompTypes( NumValidSysAvailZoneComponents, { "ZoneHVAC:FourPipeFanCoil", "ZoneHVAC:PackagedTerminalHeatPump", "ZoneHVAC:PackagedTerminalAirConditioner", "ZoneHVAC:WaterToAirHeatPump", "ZoneHVAC:WindowAirConditioner", "ZoneHVAC:UnitHeater", "ZoneHVAC:UnitVentilator", "ZoneHVAC:EnergyRecoveryVentilator", "ZoneHVAC:VentilatedSlab", "ZoneHVAC:OutdoorAirUnit", "ZoneHVAC:TerminalUnit:VariableRefrigerantFlow", "ZoneHVAC:IdealLoadsAirSystem", "ZoneHVAC:EvaporativeCoolerUnit" } );

	// DERIVED TYPE DEFINITIONS:

	// MODULE VARIABLE DECLARATIONS:
	int NumSupplyAirPaths( 0 );
	int NumReturnAirPaths( 0 );
	bool ZoneEquipInputsFilled( false );
	bool ZoneEquipSimulatedOnce( false );
	int NumOfZoneEquipLists( 0 ); // The Number of Zone Equipment List objects
	FArray1D_int ZoneEquipAvail;

	FArray1D_bool CrossMixingReportFlag;
	FArray1D_bool MixingReportFlag;
	FArray1D< Real64 > VentMCP;

	// Utility routines for module

	// Object Data
	FArray1D< EquipConfiguration > ZoneEquipConfig;
	FArray1D< EquipList > ZoneEquipList;
	FArray1D< ControlList > HeatingControlList;
	FArray1D< ControlList > CoolingControlList;
	FArray1D< SupplyAir > SupplyAirPath;
	FArray1D< ReturnAir > ReturnAirPath;

	// Functions

	void
	GetZoneEquipmentData()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Linda Lawrie
		//       DATE WRITTEN   March 2008
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This is a stub routine to allow an outside module (ZoneEquipmentManager) to get input while
		// allowing the routine itself to remain PRIVATE to this module.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		// na

		GetZoneEquipmentData1();

	}

	void
	GetZoneEquipmentData1()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Russ Taylor
		//       DATE WRITTEN   June 1997
		//       MODIFIED       Aug 2003, FCW: set ZoneEquipConfig number for each zone
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Get all the system related equipment which may be attached to
		// a zone

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using InputProcessor::GetNumObjectsFound;
		using InputProcessor::GetObjectItem;
		using InputProcessor::FindItemInList;
		using InputProcessor::GetObjectItemNum;
		using InputProcessor::VerifyName;
		using InputProcessor::GetObjectDefMaxArgs;
		using InputProcessor::MakeUPPERCase;
		using InputProcessor::SameString;
		using DataHeatBalance::Zone;
		using NodeInputManager::GetOnlySingleNode;
		using NodeInputManager::GetNodeNums;
		using NodeInputManager::InitUniqueNodeCheck;
		using NodeInputManager::CheckUniqueNodes;
		using NodeInputManager::EndUniqueNodeCheck;
		using namespace DataHVACGlobals;
		using BranchNodeConnections::SetUpCompSets;
		using namespace DataLoopNode;
		using General::TrimSigDigits;
		using General::RoundSigDigits;
		using DataGlobals::NumOfZones;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "GetZoneEquipmentData1: " ); // include trailing blank space

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int NumAlphas;
		int NumNums;
		int NodeNum;
		int PathNum;
		int CompNum;
		int ControlledZoneNum;
		int ControlledZoneLoop;
		int ZoneEquipTypeNum;
		int ZoneEquipListNum;
		int IOStat;
		std::string InletNodeListName;
		std::string ExhaustNodeListName;
		FArray1D_string AlphArray;
		FArray1D< Real64 > NumArray;
		int MaxAlphas;
		int MaxNums;
		int NumParams;
		int NumNodes;
		FArray1D_int NodeNums;
		int Counter;
		static bool ErrorsFound( false ); // If errors detected in input
		bool IsNotOK; // Flag to verify name
		bool IsBlank; // Flag for blank name
		bool NodeListError;
		bool UniqueNodeError;
		int NumOfControlledZones; // The number of Controlled Zone Equip Configuration objects
		std::string CurrentModuleObject; // Object type for getting and error messages
		FArray1D_string cAlphaFields; // Alpha field names
		FArray1D_string cNumericFields; // Numeric field names
		FArray1D_bool lAlphaBlanks; // Logical array, alpha field input BLANK = .TRUE.
		FArray1D_bool lNumericBlanks; // Logical array, numeric field input BLANK = .TRUE.
		bool IdealLoadsOnEquipmentList;
		static int found( 0 );
		int maxEquipCount;
		int numEquipCount;
		int overallEquipCount;
		int Loop1;
		int Loop2;

		struct EquipListAudit
		{
			// Members
			std::string ObjectType;
			std::string ObjectName;
			int OnListNum;

			// Default Constructor
			EquipListAudit() :
				OnListNum( 0 )
			{}

			// Member Constructor
			EquipListAudit(
				std::string const & ObjectType,
				std::string const & ObjectName,
				int const OnListNum
			) :
				ObjectType( ObjectType ),
				ObjectName( ObjectName ),
				OnListNum( OnListNum )
			{}

		};

		// Object Data
		FArray1D< EquipListAudit > ZoneEquipListAcct;

		ExhaustNodeListName = "";
		InletNodeListName = "";

		// Look in the input file for zones with air loop and zone equipment attached

		NumOfControlledZones = GetNumObjectsFound( "ZoneHVAC:EquipmentConnections" );
		NumOfZoneEquipLists = GetNumObjectsFound( "ZoneHVAC:EquipmentList" ); // Look for lists of equipment data - there should
		// be as many of these as there are controlled zones
		GetObjectDefMaxArgs( "NodeList", NumParams, NumAlphas, NumNums );
		NodeNums.dimension( NumParams, 0 );
		GetObjectDefMaxArgs( "ZoneHVAC:EquipmentList", NumParams, NumAlphas, NumNums );
		MaxAlphas = NumAlphas;
		MaxNums = NumNums;
		GetObjectDefMaxArgs( "ZoneHVAC:EquipmentConnections", NumParams, NumAlphas, NumNums );
		MaxAlphas = max( MaxAlphas, NumAlphas );
		MaxNums = max( MaxNums, NumNums );
		GetObjectDefMaxArgs( "AirLoopHVAC:SupplyPath", NumParams, NumAlphas, NumNums );
		MaxAlphas = max( MaxAlphas, NumAlphas );
		MaxNums = max( MaxNums, NumNums );
		GetObjectDefMaxArgs( "AirLoopHVAC:ReturnPath", NumParams, NumAlphas, NumNums );
		MaxAlphas = max( MaxAlphas, NumAlphas );
		MaxNums = max( MaxNums, NumNums );
		AlphArray.allocate( MaxAlphas );
		NumArray.dimension( MaxNums, 0.0 );
		cAlphaFields.allocate( MaxAlphas );
		cNumericFields.allocate( MaxNums );
		lAlphaBlanks.dimension( MaxAlphas, true );
		lNumericBlanks.dimension( MaxNums, true );

		if ( ! allocated( SupplyAirPath ) ) {
			// Look for and read in the air supply path
			// component (splitters) information for each zone
			NumSupplyAirPaths = GetNumObjectsFound( "AirLoopHVAC:SupplyPath" );
			SupplyAirPath.allocate( NumSupplyAirPaths );
		}

		if ( ! allocated( ReturnAirPath ) ) {
			// Look for and read in the air return path
			// component (mixers & plenums) information for each zone
			NumReturnAirPaths = GetNumObjectsFound( "AirLoopHVAC:ReturnPath" );
			ReturnAirPath.allocate( NumReturnAirPaths );
		}

		ZoneEquipConfig.allocate( NumOfZones ); // Allocate the array containing the configuration
		// data for each zone to the number of controlled zones
		// found in the input file.  This may or may not
		// be the same as the number of zones in the building
		ZoneEquipList.allocate( NumOfZones );
		ZoneEquipAvail.dimension( NumOfZones, NoAction );

		if ( NumOfZoneEquipLists != NumOfControlledZones ) {
			ShowSevereError( RoutineName + "Number of Zone Equipment lists [" + TrimSigDigits( NumOfZoneEquipLists ) + "] not equal Number of Controlled Zones [" + TrimSigDigits( NumOfControlledZones ) + ']' );
			ShowContinueError( "..Each Controlled Zone [ZoneHVAC:EquipmentConnections] must have a corresponding" "(unique) ZoneHVAC:EquipmentList" );
			ShowFatalError( "GetZoneEquipment: Incorrect number of zone equipment lists" );
		}

		if ( NumOfControlledZones > NumOfZones ) {
			ShowSevereError( RoutineName + "Number of Controlled Zone objects [" + TrimSigDigits( NumOfControlledZones ) + "] greater than Number of Zones [" + TrimSigDigits( NumOfZones ) + ']' );
			ShowFatalError( RoutineName + "Too many ZoneHVAC:EquipmentConnections objects." );
		}

		InitUniqueNodeCheck( "ZoneHVAC:EquipmentConnections" );

		overallEquipCount = 0;

		for ( ControlledZoneLoop = 1; ControlledZoneLoop <= NumOfControlledZones; ++ControlledZoneLoop ) {

			CurrentModuleObject = "ZoneHVAC:EquipmentConnections";

			GetObjectItem( CurrentModuleObject, ControlledZoneLoop, AlphArray, NumAlphas, NumArray, NumNums, IOStat, lNumericBlanks, lAlphaBlanks, cAlphaFields, cNumericFields ); // Get Equipment | data for one zone

			ControlledZoneNum = FindItemInList( AlphArray( 1 ), Zone.Name(), NumOfZones );

			if ( ControlledZoneNum == 0 ) {
				ShowSevereError( RoutineName + CurrentModuleObject + ": " + cAlphaFields( 1 ) + "=\"" + AlphArray( 1 ) + "\"" );
				ShowContinueError( "..Requested Controlled Zone not among Zones, remaining items for this object not processed." );
				ErrorsFound = true;
				continue;
			} else {
				//    Zone(ZoneEquipConfig(ControlledZoneNum)%ActualZoneNum)%ZoneEquipConfigNum = ControlledZoneNum
				if ( Zone( ControlledZoneNum ).IsControlled ) {
					ShowSevereError( RoutineName + CurrentModuleObject + ": " + cAlphaFields( 1 ) + "=\"" + AlphArray( 1 ) + "\"" );
					ShowContinueError( "..Duplicate Controlled Zone entered, only one " + CurrentModuleObject + " per zone is allowed." );
					ErrorsFound = true;
					continue;
				}
				Zone( ControlledZoneNum ).IsControlled = true;
				ZoneEquipConfig( ControlledZoneNum ).IsControlled = true;
				ZoneEquipConfig( ControlledZoneNum ).ActualZoneNum = ControlledZoneNum;
			}
			ZoneEquipConfig( ControlledZoneNum ).ZoneName = AlphArray( 1 ); // for x-referencing with the geometry data

			IsNotOK = false;
			IsBlank = false;
			VerifyName( AlphArray( 2 ), ZoneEquipConfig.EquipListName(), ControlledZoneLoop - 1, IsNotOK, IsBlank, CurrentModuleObject + cAlphaFields( 2 ) );
			if ( IsNotOK ) {
				ShowContinueError( "..another Controlled Zone has been assigned that " + cAlphaFields( 2 ) + '.' );
				ErrorsFound = true;
				if ( IsBlank ) AlphArray( 2 ) = "xxxxx";
			}
			ZoneEquipConfig( ControlledZoneNum ).EquipListName = AlphArray( 2 ); // the name of the list containing all the zone eq.
			InletNodeListName = AlphArray( 3 );
			ExhaustNodeListName = AlphArray( 4 );
			ZoneEquipConfig( ControlledZoneNum ).ZoneNode = GetOnlySingleNode( AlphArray( 5 ), ErrorsFound, CurrentModuleObject, AlphArray( 1 ), NodeType_Air, NodeConnectionType_ZoneNode, 1, ObjectIsNotParent ); // all zone air state variables are
			if ( ZoneEquipConfig( ControlledZoneNum ).ZoneNode == 0 ) {
				ShowSevereError( RoutineName + CurrentModuleObject + ": " + cAlphaFields( 1 ) + "=\"" + AlphArray( 1 ) + "\", invalid" );
				ShowContinueError( cAlphaFields( 5 ) + " must be present." );
				ErrorsFound = true;
			} else {
				UniqueNodeError = false;
				CheckUniqueNodes( cAlphaFields( 5 ), "NodeName", UniqueNodeError, AlphArray( 5 ), _, AlphArray( 1 ) );
				if ( UniqueNodeError ) {
					//ShowContinueError( "Occurs for " + trim( cAlphaFields( 1 ) ) + " = " + trim( AlphArray( 1 ) ) );
					ErrorsFound = true;
				}
			}
			// assigned to this node
			if ( ZoneEquipConfig( ControlledZoneNum ).ActualZoneNum > 0 ) {
				Zone( ZoneEquipConfig( ControlledZoneNum ).ActualZoneNum ).SystemZoneNodeNumber = ZoneEquipConfig( ControlledZoneNum ).ZoneNode;
			} // This error already detected and program will be terminated.

			ZoneEquipConfig( ControlledZoneNum ).ReturnAirNode = GetOnlySingleNode( AlphArray( 6 ), ErrorsFound, CurrentModuleObject, AlphArray( 1 ), NodeType_Air, NodeConnectionType_ZoneReturn, 1, ObjectIsNotParent ); // all return air state variables are
			// assigned to this node
			if ( ZoneEquipConfig( ControlledZoneNum ).ReturnAirNode != 0 ) {
				UniqueNodeError = false;
				CheckUniqueNodes( cAlphaFields( 6 ), "NodeName", UniqueNodeError, AlphArray( 6 ), _, AlphArray( 1 ) );
				if ( UniqueNodeError ) {
					//ShowContinueError( "Occurs for " + trim( cAlphaFields( 1 ) ) + " = " + trim( AlphArray( 1 ) ) );
					ErrorsFound = true;
				}
			}

			// Read in the equipment type, name and sequence information
			// for each equipment list

			CurrentModuleObject = "ZoneHVAC:EquipmentList";

			ZoneEquipListNum = GetObjectItemNum( CurrentModuleObject, ZoneEquipConfig( ControlledZoneNum ).EquipListName );
			if ( ZoneEquipListNum > 0 ) {

				GetObjectItem( CurrentModuleObject, ZoneEquipListNum, AlphArray, NumAlphas, NumArray, NumNums, IOStat, lNumericBlanks, lAlphaBlanks, cAlphaFields, cNumericFields ); //  data for one zone
				IsNotOK = false;
				IsBlank = false;
				VerifyName( AlphArray( 1 ), ZoneEquipList.Name(), ControlledZoneNum - 1, IsNotOK, IsBlank, CurrentModuleObject + " Name" );
				if ( IsNotOK ) {
					ShowContinueError( "Bad Zone Equipment name in " + CurrentModuleObject + "=\"" + ZoneEquipConfig( ControlledZoneNum ).EquipListName + "\"" );
					ShowContinueError( "For Zone=\"" + ZoneEquipConfig( ControlledZoneNum ).ZoneName + "\"." );
					ErrorsFound = true;
					if ( IsBlank ) AlphArray( 1 ) = "xxxxx";
				}

				ZoneEquipList( ControlledZoneNum ).Name = AlphArray( 1 );

				maxEquipCount = 0;
				numEquipCount = ( NumAlphas - 1 ) / 2;
				if ( numEquipCount * 2 != ( NumAlphas - 1 ) ) ++numEquipCount;
				for ( ZoneEquipTypeNum = 1; ZoneEquipTypeNum <= numEquipCount; ++ZoneEquipTypeNum ) {
					if ( ! lAlphaBlanks( 2 * ZoneEquipTypeNum ) && ! lAlphaBlanks( 2 * ZoneEquipTypeNum + 1 ) ) {
						++maxEquipCount;
						continue;
					}
					ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + ZoneEquipList( ControlledZoneNum ).Name + "\", truncated list at blank field; object count=" + RoundSigDigits( maxEquipCount ) );
					break;
				}

				overallEquipCount += maxEquipCount;
				ZoneEquipList( ControlledZoneNum ).NumOfEquipTypes = maxEquipCount;
				ZoneEquipList( ControlledZoneNum ).EquipType.allocate( ZoneEquipList( ControlledZoneNum ).NumOfEquipTypes );
				ZoneEquipList( ControlledZoneNum ).EquipType_Num.allocate( ZoneEquipList( ControlledZoneNum ).NumOfEquipTypes );
				ZoneEquipList( ControlledZoneNum ).EquipName.allocate( ZoneEquipList( ControlledZoneNum ).NumOfEquipTypes );
				ZoneEquipList( ControlledZoneNum ).EquipIndex.allocate( ZoneEquipList( ControlledZoneNum ).NumOfEquipTypes );
				ZoneEquipList( ControlledZoneNum ).EquipData.allocate( ZoneEquipList( ControlledZoneNum ).NumOfEquipTypes );
				ZoneEquipList( ControlledZoneNum ).CoolingPriority.allocate( ZoneEquipList( ControlledZoneNum ).NumOfEquipTypes );
				ZoneEquipList( ControlledZoneNum ).HeatingPriority.allocate( ZoneEquipList( ControlledZoneNum ).NumOfEquipTypes );
				ZoneEquipList( ControlledZoneNum ).EquipType = "";
				ZoneEquipList( ControlledZoneNum ).EquipType_Num = 0;
				ZoneEquipList( ControlledZoneNum ).EquipName = "";
				ZoneEquipList( ControlledZoneNum ).EquipIndex = 0;
				ZoneEquipList( ControlledZoneNum ).CoolingPriority = 0;
				ZoneEquipList( ControlledZoneNum ).HeatingPriority = 0;

				IdealLoadsOnEquipmentList = false;

				for ( ZoneEquipTypeNum = 1; ZoneEquipTypeNum <= ZoneEquipList( ControlledZoneNum ).NumOfEquipTypes; ++ZoneEquipTypeNum ) {
					ZoneEquipList( ControlledZoneNum ).EquipType( ZoneEquipTypeNum ) = AlphArray( 2 * ZoneEquipTypeNum );
					ZoneEquipList( ControlledZoneNum ).EquipName( ZoneEquipTypeNum ) = AlphArray( 2 * ZoneEquipTypeNum + 1 );
					ValidateComponent( ZoneEquipList( ControlledZoneNum ).EquipType( ZoneEquipTypeNum ), ZoneEquipList( ControlledZoneNum ).EquipName( ZoneEquipTypeNum ), IsNotOK, CurrentModuleObject );
					if ( IsNotOK ) {
						ShowContinueError( "In " + CurrentModuleObject + '=' + ZoneEquipList( ControlledZoneNum ).Name );
						ErrorsFound = true;
					}
					ZoneEquipList( ControlledZoneNum ).CoolingPriority( ZoneEquipTypeNum ) = nint( NumArray( 2 * ZoneEquipTypeNum - 1 ) );
					if ( ( ZoneEquipList( ControlledZoneNum ).CoolingPriority( ZoneEquipTypeNum ) <= 0 ) || ( ZoneEquipList( ControlledZoneNum ).CoolingPriority( ZoneEquipTypeNum ) > ZoneEquipList( ControlledZoneNum ).NumOfEquipTypes ) ) {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + AlphArray( 1 ) + "\"." );
						ShowContinueError( "invalid " + cNumericFields( 2 * ZoneEquipTypeNum - 1 ) + "=[" + RoundSigDigits( ZoneEquipList( ControlledZoneNum ).CoolingPriority( ZoneEquipTypeNum ) ) + "]." );
						ShowContinueError( "equipment sequence must be > 0 and <= number of equipments in the list." );
						if ( ZoneEquipList( ControlledZoneNum ).CoolingPriority( ZoneEquipTypeNum ) > 0 ) ShowContinueError( "only " + RoundSigDigits( ZoneEquipList( ControlledZoneNum ).NumOfEquipTypes ) + " in the list." );
						ErrorsFound = true;
					}

					ZoneEquipList( ControlledZoneNum ).HeatingPriority( ZoneEquipTypeNum ) = nint( NumArray( 2 * ZoneEquipTypeNum ) );
					if ( ( ZoneEquipList( ControlledZoneNum ).HeatingPriority( ZoneEquipTypeNum ) <= 0 ) || ( ZoneEquipList( ControlledZoneNum ).HeatingPriority( ZoneEquipTypeNum ) > ZoneEquipList( ControlledZoneNum ).NumOfEquipTypes ) ) {
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + AlphArray( 1 ) + "\"." );
						ShowContinueError( "invalid " + cNumericFields( 2 * ZoneEquipTypeNum ) + "=[" + RoundSigDigits( ZoneEquipList( ControlledZoneNum ).HeatingPriority( ZoneEquipTypeNum ) ) + "]." );
						ShowContinueError( "equipment sequence must be > 0 and <= number of equipments in the list." );
						if ( ZoneEquipList( ControlledZoneNum ).HeatingPriority( ZoneEquipTypeNum ) > 0 ) ShowContinueError( "only " + RoundSigDigits( ZoneEquipList( ControlledZoneNum ).NumOfEquipTypes ) + " in the list." );
						ErrorsFound = true;
					}

					{ auto const SELECT_CASE_var( MakeUPPERCase( ZoneEquipList( ControlledZoneNum ).EquipType( ZoneEquipTypeNum ) ) );

					if ( SELECT_CASE_var == "ZONEHVAC:AIRDISTRIBUTIONUNIT" ) {
						ZoneEquipList( ControlledZoneNum ).EquipType_Num( ZoneEquipTypeNum ) = AirDistUnit_Num;

					} else if ( SELECT_CASE_var == "AIRTERMINAL:SINGLEDUCT:UNCONTROLLED" ) {
						ZoneEquipList( ControlledZoneNum ).EquipType_Num( ZoneEquipTypeNum ) = DirectAir_Num;

					} else if ( SELECT_CASE_var == "ZONEHVAC:WINDOWAIRCONDITIONER" ) { // Window Air Conditioner
						ZoneEquipList( ControlledZoneNum ).EquipType_Num( ZoneEquipTypeNum ) = WindowAC_Num;

					} else if ( SELECT_CASE_var == "ZONEHVAC:PACKAGEDTERMINALHEATPUMP" ) { // Packaged Terminal Heat Pump
						ZoneEquipList( ControlledZoneNum ).EquipType_Num( ZoneEquipTypeNum ) = PkgTermHPAirToAir_Num;

					} else if ( SELECT_CASE_var == "ZONEHVAC:PACKAGEDTERMINALAIRCONDITIONER" ) { // Packaged Terminal Air Conditioner
						ZoneEquipList( ControlledZoneNum ).EquipType_Num( ZoneEquipTypeNum ) = PkgTermACAirToAir_Num;

					} else if ( SELECT_CASE_var == "AIRLOOPHVAC:UNITARYSYSTEM" ) { // Unitary System
						ZoneEquipList( ControlledZoneNum ).EquipType_Num( ZoneEquipTypeNum ) = ZoneUnitarySystem_Num;

					} else if ( SELECT_CASE_var == "ZONEHVAC:DEHUMIDIFIER:DX" ) { // Zone dehumidifier
						ZoneEquipList( ControlledZoneNum ).EquipType_Num( ZoneEquipTypeNum ) = ZoneDXDehumidifier_Num;

					} else if ( SELECT_CASE_var == "ZONEHVAC:WATERTOAIRHEATPUMP" ) { // Zone Water to Air Heat Pump
						ZoneEquipList( ControlledZoneNum ).EquipType_Num( ZoneEquipTypeNum ) = PkgTermHPWaterToAir_Num;

					} else if ( SELECT_CASE_var == "ZONEHVAC:FOURPIPEFANCOIL" ) { // 4-Pipe Fan Coil
						ZoneEquipList( ControlledZoneNum ).EquipType_Num( ZoneEquipTypeNum ) = FanCoil4Pipe_Num;

					} else if ( SELECT_CASE_var == "ZONEHVAC:UNITVENTILATOR" ) { // Unit Ventilator
						ZoneEquipList( ControlledZoneNum ).EquipType_Num( ZoneEquipTypeNum ) = UnitVentilator_Num;

					} else if ( SELECT_CASE_var == "ZONEHVAC:UNITHEATER" ) { // Unit Heater
						ZoneEquipList( ControlledZoneNum ).EquipType_Num( ZoneEquipTypeNum ) = UnitHeater_Num;

					} else if ( SELECT_CASE_var == "ZONEHVAC:IDEALLOADSAIRSYSTEM" ) {
						ZoneEquipList( ControlledZoneNum ).EquipType_Num( ZoneEquipTypeNum ) = PurchasedAir_Num;

					} else if ( SELECT_CASE_var == "ZONEHVAC:BASEBOARD:RADIANTCONVECTIVE:WATER" ) { // Hot Water Baseboard
						ZoneEquipList( ControlledZoneNum ).EquipType_Num( ZoneEquipTypeNum ) = BBWater_Num;

					} else if ( SELECT_CASE_var == "ZONEHVAC:BASEBOARD:CONVECTIVE:WATER" ) { // Baseboard
						ZoneEquipList( ControlledZoneNum ).EquipType_Num( ZoneEquipTypeNum ) = BBWaterConvective_Num;

					} else if ( SELECT_CASE_var == "ZONEHVAC:BASEBOARD:CONVECTIVE:ELECTRIC" ) { // Electric Baseboard
						ZoneEquipList( ControlledZoneNum ).EquipType_Num( ZoneEquipTypeNum ) = BBElectricConvective_Num;

					} else if ( SELECT_CASE_var == "ZONEHVAC:HIGHTEMPERATURERADIANT" ) { // High Temperature Radiators
						ZoneEquipList( ControlledZoneNum ).EquipType_Num( ZoneEquipTypeNum ) = HiTempRadiant_Num;

					} else if ( SELECT_CASE_var == "ZONEHVAC:LOWTEMPERATURERADIANT:VARIABLEFLOW" ) { // Low temperature radiant system (hydronic)
						ZoneEquipList( ControlledZoneNum ).EquipType_Num( ZoneEquipTypeNum ) = LoTempRadiant_Num;

					} else if ( SELECT_CASE_var == "ZONEHVAC:LOWTEMPERATURERADIANT:CONSTANTFLOW" ) { // Low temperature radiant system (hydronic, constant flow)
						ZoneEquipList( ControlledZoneNum ).EquipType_Num( ZoneEquipTypeNum ) = LoTempRadiant_Num;

					} else if ( SELECT_CASE_var == "ZONEHVAC:LOWTEMPERATURERADIANT:ELECTRIC" ) { // Low temperature radiant system (electric)
						ZoneEquipList( ControlledZoneNum ).EquipType_Num( ZoneEquipTypeNum ) = LoTempRadiant_Num;

					} else if ( SELECT_CASE_var == "FAN:ZONEEXHAUST" ) {
						ZoneEquipList( ControlledZoneNum ).EquipType_Num( ZoneEquipTypeNum ) = ZoneExhaustFan_Num;

					} else if ( SELECT_CASE_var == "HEATEXCHANGER:AIRTOAIR:FLATPLATE" ) {
						ZoneEquipList( ControlledZoneNum ).EquipType_Num( ZoneEquipTypeNum ) = HeatXchngr_Num;

					} else if ( SELECT_CASE_var == "ZONEHVAC:ENERGYRECOVERYVENTILATOR" ) {
						ZoneEquipList( ControlledZoneNum ).EquipType_Num( ZoneEquipTypeNum ) = ERVStandAlone_Num;

					} else if ( SELECT_CASE_var == "WATERHEATER:HEATPUMP" ) {
						ZoneEquipList( ControlledZoneNum ).EquipType_Num( ZoneEquipTypeNum ) = HPWaterHeater_Num;

					} else if ( SELECT_CASE_var == "ZONEHVAC:VENTILATEDSLAB" ) { // Ventilated Slab
						ZoneEquipList( ControlledZoneNum ).EquipType_Num( ZoneEquipTypeNum ) = VentilatedSlab_Num;

					} else if ( SELECT_CASE_var == "ZONEHVAC:BASEBOARD:RADIANTCONVECTIVE:STEAM" ) { // Steam Baseboard
						ZoneEquipList( ControlledZoneNum ).EquipType_Num( ZoneEquipTypeNum ) = BBSteam_Num;

					} else if ( SELECT_CASE_var == "ZONEHVAC:OUTDOORAIRUNIT" ) { // Outdoor Air Unit
						ZoneEquipList( ControlledZoneNum ).EquipType_Num( ZoneEquipTypeNum ) = OutdoorAirUnit_Num;

					} else if ( SELECT_CASE_var == "ZONEHVAC:BASEBOARD:RADIANTCONVECTIVE:ELECTRIC" ) { // Radiant electric Baseboard
						ZoneEquipList( ControlledZoneNum ).EquipType_Num( ZoneEquipTypeNum ) = BBElectric_Num;

					} else if ( SELECT_CASE_var == "ZONEHVAC:TERMINALUNIT:VARIABLEREFRIGERANTFLOW" ) { // VRF AC System
						ZoneEquipList( ControlledZoneNum ).EquipType_Num( ZoneEquipTypeNum ) = VRFTerminalUnit_Num;

					} else if ( SELECT_CASE_var == "ZONEHVAC:REFRIGERATIONCHILLERSET" ) { // Refrigeration chiller designed for warehouse applications
						ZoneEquipList( ControlledZoneNum ).EquipType_Num( ZoneEquipTypeNum ) = RefrigerationAirChillerSet_Num;

					} else if ( SELECT_CASE_var == "ZONEHVAC:FORCEDAIR:USERDEFINED" ) {
						ZoneEquipList( ControlledZoneNum ).EquipType_Num( ZoneEquipTypeNum ) = UserDefinedZoneHVACForcedAir_Num;

					} else if ( SELECT_CASE_var == "ZONEHVAC:EVAPORATIVECOOLERUNIT" ) {
						ZoneEquipList( ControlledZoneNum ).EquipType_Num( ZoneEquipTypeNum ) = ZoneEvaporativeCoolerUnit_Num;
					} else {
						ShowSevereError( RoutineName + CurrentModuleObject + " = " + ZoneEquipList( ControlledZoneNum ).Name );
						ShowContinueError( "..Invalid Equipment Type = " + ZoneEquipList( ControlledZoneNum ).EquipType( ZoneEquipTypeNum ) );
						ErrorsFound = true;

					}}
				}
				for ( ZoneEquipTypeNum = 1; ZoneEquipTypeNum <= ZoneEquipList( ControlledZoneNum ).NumOfEquipTypes; ++ZoneEquipTypeNum ) {
					if ( count_eq( ZoneEquipList( ControlledZoneNum ).CoolingPriority, ZoneEquipTypeNum ) > 1 ) {
						ShowSevereError( RoutineName + CurrentModuleObject + " = " + ZoneEquipList( ControlledZoneNum ).Name );
						ShowContinueError( "...multiple assignments for Zone Equipment Cooling Sequence=" + RoundSigDigits( ZoneEquipTypeNum ) + ", must be 1-1 correspondence between sequence assignments and number of equipments." );
						ErrorsFound = true;
					} else if ( count_eq( ZoneEquipList( ControlledZoneNum ).CoolingPriority, ZoneEquipTypeNum ) == 0 ) {
						ShowWarningError( RoutineName + CurrentModuleObject + " = " + ZoneEquipList( ControlledZoneNum ).Name );
						ShowContinueError( "...zero assignments for Zone Equipment Cooling Sequence=" + RoundSigDigits( ZoneEquipTypeNum ) + ", apparent gap in sequence assignments in this equipment list." );
					}
					if ( count_eq( ZoneEquipList( ControlledZoneNum ).HeatingPriority, ZoneEquipTypeNum ) > 1 ) {
						ShowSevereError( RoutineName + CurrentModuleObject + " = " + ZoneEquipList( ControlledZoneNum ).Name );
						ShowContinueError( "...multiple assignments for Zone Equipment Heating or No-Load Sequence=" + RoundSigDigits( ZoneEquipTypeNum ) + ", must be 1-1 correspondence between sequence assignments and number of equipments." );
						ErrorsFound = true;
					} else if ( count_eq( ZoneEquipList( ControlledZoneNum ).HeatingPriority, ZoneEquipTypeNum ) == 0 ) {
						ShowWarningError( RoutineName + CurrentModuleObject + " = " + ZoneEquipList( ControlledZoneNum ).Name );
						ShowContinueError( "...zero assignments for Zone Equipment Heating or No-Load Sequence=" + RoundSigDigits( ZoneEquipTypeNum ) + ", apparent gap in sequence assignments in this equipment list." );
					}
				}

			} else {
				ShowSevereError( RoutineName + CurrentModuleObject + " not found = " + ZoneEquipConfig( ControlledZoneNum ).EquipListName );
				ShowContinueError( "In ZoneHVAC:EquipmentConnections object, for Zone = " + ZoneEquipConfig( ControlledZoneNum ).ZoneName );
				ErrorsFound = true;
			}

			// End ZoneHVAC:EquipmentList

			NodeListError = false;
			GetNodeNums( InletNodeListName, NumNodes, NodeNums, NodeListError, NodeType_Air, "ZoneHVAC:EquipmentConnections", ZoneEquipConfig( ControlledZoneNum ).ZoneName, NodeConnectionType_ZoneInlet, 1, ObjectIsNotParent );

			if ( ! NodeListError ) {
				ZoneEquipConfig( ControlledZoneNum ).NumInletNodes = NumNodes;

				ZoneEquipConfig( ControlledZoneNum ).InletNode.allocate( NumNodes );
				ZoneEquipConfig( ControlledZoneNum ).AirDistUnitCool.allocate( NumNodes );
				ZoneEquipConfig( ControlledZoneNum ).AirDistUnitHeat.allocate( NumNodes );

				for ( NodeNum = 1; NodeNum <= NumNodes; ++NodeNum ) {
					ZoneEquipConfig( ControlledZoneNum ).InletNode( NodeNum ) = NodeNums( NodeNum );
					UniqueNodeError = false;
					CheckUniqueNodes( "Zone Air Inlet Nodes", "NodeNumber", UniqueNodeError, _, NodeNums( NodeNum ), ZoneEquipConfig( ControlledZoneNum ).ZoneName );
					if ( UniqueNodeError ) {
						//ShowContinueError( "Occurs for Zone = " + trim( AlphArray( 1 ) ) );
						ErrorsFound = true;
					}
					ZoneEquipConfig( ControlledZoneNum ).AirDistUnitCool( NodeNum ).InNode = 0;
					ZoneEquipConfig( ControlledZoneNum ).AirDistUnitHeat( NodeNum ).InNode = 0;
					ZoneEquipConfig( ControlledZoneNum ).AirDistUnitCool( NodeNum ).OutNode = 0;
					ZoneEquipConfig( ControlledZoneNum ).AirDistUnitHeat( NodeNum ).OutNode = 0;
				}
			} else {
				ShowContinueError( "Invalid inlet node or NodeList name in ZoneHVAC:EquipmentConnections object, for Zone = " + ZoneEquipConfig( ControlledZoneNum ).ZoneName );
				ErrorsFound = true;
			}

			NodeListError = false;
			GetNodeNums( ExhaustNodeListName, NumNodes, NodeNums, NodeListError, NodeType_Air, "ZoneHVAC:EquipmentConnections", ZoneEquipConfig( ControlledZoneNum ).ZoneName, NodeConnectionType_ZoneExhaust, 1, ObjectIsNotParent );

			if ( ! NodeListError ) {
				ZoneEquipConfig( ControlledZoneNum ).NumExhaustNodes = NumNodes;

				ZoneEquipConfig( ControlledZoneNum ).ExhaustNode.allocate( NumNodes );

				for ( NodeNum = 1; NodeNum <= NumNodes; ++NodeNum ) {
					ZoneEquipConfig( ControlledZoneNum ).ExhaustNode( NodeNum ) = NodeNums( NodeNum );
					UniqueNodeError = false;
					CheckUniqueNodes( "Zone Air Exhaust Nodes", "NodeNumber", UniqueNodeError, _, NodeNums( NodeNum ), ZoneEquipConfig( ControlledZoneNum ).ZoneName );
					if ( UniqueNodeError ) {
						//ShowContinueError( "Occurs for Zone = " + trim( AlphArray( 1 ) ) );
						ErrorsFound = true;
					}
				}
			} else {
				ShowContinueError( "Invalid exhaust node or NodeList name in ZoneHVAC:EquipmentConnections object, for Zone=" + ZoneEquipConfig( ControlledZoneNum ).ZoneName );
				ErrorsFound = true;
			}
		} // end loop over controlled zones

		if ( ErrorsFound ) {
			ShowWarningError( RoutineName + CurrentModuleObject + ", duplicate items NOT CHECKED due to previous errors." );
			overallEquipCount = 0;
		}
		if ( overallEquipCount > 0 ) {
			ZoneEquipListAcct.allocate( overallEquipCount );
			overallEquipCount = 0;
			for ( Loop1 = 1; Loop1 <= NumOfControlledZones; ++Loop1 ) {
				for ( Loop2 = 1; Loop2 <= ZoneEquipList( Loop1 ).NumOfEquipTypes; ++Loop2 ) {
					++overallEquipCount;
					ZoneEquipListAcct( overallEquipCount ).ObjectType = ZoneEquipList( Loop1 ).EquipType( Loop2 );
					ZoneEquipListAcct( overallEquipCount ).ObjectName = ZoneEquipList( Loop1 ).EquipName( Loop2 );
					ZoneEquipListAcct( overallEquipCount ).OnListNum = Loop1;
				}
			}
			// Now check for uniqueness
			for ( Loop1 = 1; Loop1 <= overallEquipCount; ++Loop1 ) {
				for ( Loop2 = Loop1 + 1; Loop2 <= overallEquipCount; ++Loop2 ) {
					if ( ZoneEquipListAcct( Loop1 ).ObjectType != ZoneEquipListAcct( Loop2 ).ObjectType || ZoneEquipListAcct( Loop1 ).ObjectName != ZoneEquipListAcct( Loop2 ).ObjectName ) continue;
					// Duplicated -- not allowed
					ShowSevereError( RoutineName + CurrentModuleObject + ", duplicate items in ZoneHVAC:EquipmentList." );
					ShowContinueError( "Equipment: Type=" + ZoneEquipListAcct( Loop1 ).ObjectType + ", Name=" + ZoneEquipListAcct( Loop1 ).ObjectName );
					ShowContinueError( "Found on List=\"" + ZoneEquipList( ZoneEquipListAcct( Loop1 ).OnListNum ).Name + "\"." );
					ShowContinueError( "Equipment Duplicated on List=\"" + ZoneEquipList( ZoneEquipListAcct( Loop2 ).OnListNum ).Name + "\"." );
					ErrorsFound = true;
				}
			}
			ZoneEquipListAcct.deallocate();
		}

		//map ZoneEquipConfig%EquipListIndex to ZoneEquipList%Name

		for ( ControlledZoneLoop = 1; ControlledZoneLoop <= NumOfZones; ++ControlledZoneLoop ) {
			found = FindItemInList( ZoneEquipList( ControlledZoneLoop ).Name, ZoneEquipConfig.EquipListName(), NumOfZones );
			if ( found > 0 ) ZoneEquipConfig( found ).EquipListIndex = ControlledZoneLoop;
		} // end loop over controlled zones

		EndUniqueNodeCheck( "ZoneHVAC:EquipmentConnections" );

		CurrentModuleObject = "AirLoopHVAC:SupplyPath";
		for ( PathNum = 1; PathNum <= NumSupplyAirPaths; ++PathNum ) {

			GetObjectItem( CurrentModuleObject, PathNum, AlphArray, NumAlphas, NumArray, NumNums, IOStat, lNumericBlanks, lAlphaBlanks, cAlphaFields, cNumericFields ); //  data for one zone
			IsNotOK = false;
			IsBlank = false;
			VerifyName( AlphArray( 1 ), SupplyAirPath.Name(), PathNum - 1, IsNotOK, IsBlank, CurrentModuleObject + " Name" );
			if ( IsNotOK ) {
				ErrorsFound = true;
				if ( IsBlank ) AlphArray( 1 ) = "xxxxx";
			}
			SupplyAirPath( PathNum ).Name = AlphArray( 1 );
			SupplyAirPath( PathNum ).NumOfComponents = nint( ( double( NumAlphas ) - 2.0 ) / 2.0 );

			SupplyAirPath( PathNum ).InletNodeNum = GetOnlySingleNode( AlphArray( 2 ), ErrorsFound, CurrentModuleObject, AlphArray( 1 ), NodeType_Air, NodeConnectionType_Inlet, 1, ObjectIsParent );

			SupplyAirPath( PathNum ).ComponentType.allocate( SupplyAirPath( PathNum ).NumOfComponents );
			SupplyAirPath( PathNum ).ComponentType_Num.allocate( SupplyAirPath( PathNum ).NumOfComponents );
			SupplyAirPath( PathNum ).ComponentType_Num = 0;
			SupplyAirPath( PathNum ).ComponentName.allocate( SupplyAirPath( PathNum ).NumOfComponents );
			SupplyAirPath( PathNum ).ComponentIndex.allocate( SupplyAirPath( PathNum ).NumOfComponents );
			SupplyAirPath( PathNum ).SplitterIndex.allocate( SupplyAirPath( PathNum ).NumOfComponents );
			SupplyAirPath( PathNum ).PlenumIndex.allocate( SupplyAirPath( PathNum ).NumOfComponents );

			Counter = 3;

			for ( CompNum = 1; CompNum <= SupplyAirPath( PathNum ).NumOfComponents; ++CompNum ) {

				if ( ( AlphArray( Counter ) == "AIRLOOPHVAC:ZONESPLITTER" ) || ( AlphArray( Counter ) == "AIRLOOPHVAC:SUPPLYPLENUM" ) ) {

					SupplyAirPath( PathNum ).ComponentType( CompNum ) = AlphArray( Counter );
					SupplyAirPath( PathNum ).ComponentName( CompNum ) = AlphArray( Counter + 1 );
					ValidateComponent( SupplyAirPath( PathNum ).ComponentType( CompNum ), SupplyAirPath( PathNum ).ComponentName( CompNum ), IsNotOK, CurrentModuleObject );
					SupplyAirPath( PathNum ).ComponentIndex( CompNum ) = 0;
					SupplyAirPath( PathNum ).SplitterIndex( CompNum ) = 0;
					SupplyAirPath( PathNum ).PlenumIndex( CompNum ) = 0;
					if ( AlphArray( Counter ) == "AIRLOOPHVAC:ZONESPLITTER" ) SupplyAirPath( PathNum ).ComponentType_Num( CompNum ) = ZoneSplitter_Type;
					if ( AlphArray( Counter ) == "AIRLOOPHVAC:SUPPLYPLENUM" ) SupplyAirPath( PathNum ).ComponentType_Num( CompNum ) = ZoneSupplyPlenum_Type;

				} else {
					ShowSevereError( RoutineName + cAlphaFields( 1 ) + "=\"" + SupplyAirPath( PathNum ).Name + "\"" );
					ShowContinueError( "Unhandled component type =\"" + AlphArray( Counter ) + "\"." );
					ShowContinueError( "Must be \"AirLoopHVAC:ZoneSplitter\" or \"AirLoopHVAC:SupplyPlenum\"" );
					ErrorsFound = true;
				}

				Counter += 2;

			}

			SupplyAirPath( PathNum ).NumOutletNodes = 0;
			SupplyAirPath( PathNum ).NumNodes = 0;

		} // end loop over supply air paths

		CurrentModuleObject = "AirLoopHVAC:ReturnPath";
		for ( PathNum = 1; PathNum <= NumReturnAirPaths; ++PathNum ) {

			GetObjectItem( CurrentModuleObject, PathNum, AlphArray, NumAlphas, NumArray, NumNums, IOStat, lNumericBlanks, lAlphaBlanks, cAlphaFields, cNumericFields ); //  data for one zone

			IsNotOK = false;
			IsBlank = false;
			VerifyName( AlphArray( 1 ), ReturnAirPath.Name(), PathNum - 1, IsNotOK, IsBlank, CurrentModuleObject + " Name" );
			if ( IsNotOK ) {
				ErrorsFound = true;
				if ( IsBlank ) AlphArray( 1 ) = "xxxxx";
			}
			ReturnAirPath( PathNum ).Name = AlphArray( 1 );
			ReturnAirPath( PathNum ).NumOfComponents = nint( ( double( NumAlphas ) - 2.0 ) / 2.0 );

			ReturnAirPath( PathNum ).OutletNodeNum = GetOnlySingleNode( AlphArray( 2 ), ErrorsFound, CurrentModuleObject, AlphArray( 1 ), NodeType_Air, NodeConnectionType_Outlet, 1, ObjectIsParent );

			ReturnAirPath( PathNum ).ComponentType.allocate( ReturnAirPath( PathNum ).NumOfComponents );
			ReturnAirPath( PathNum ).ComponentType_Num.allocate( ReturnAirPath( PathNum ).NumOfComponents );
			ReturnAirPath( PathNum ).ComponentType_Num = 0;
			ReturnAirPath( PathNum ).ComponentName.allocate( ReturnAirPath( PathNum ).NumOfComponents );
			ReturnAirPath( PathNum ).ComponentIndex.allocate( ReturnAirPath( PathNum ).NumOfComponents );

			Counter = 3;

			for ( CompNum = 1; CompNum <= ReturnAirPath( PathNum ).NumOfComponents; ++CompNum ) {

				if ( ( AlphArray( Counter ) == "AIRLOOPHVAC:ZONEMIXER" ) || ( AlphArray( Counter ) == "AIRLOOPHVAC:RETURNPLENUM" ) ) {

					ReturnAirPath( PathNum ).ComponentType( CompNum ) = AlphArray( Counter );
					ReturnAirPath( PathNum ).ComponentName( CompNum ) = AlphArray( Counter + 1 );
					ReturnAirPath( PathNum ).ComponentIndex( CompNum ) = 0;
					ValidateComponent( ReturnAirPath( PathNum ).ComponentType( CompNum ), ReturnAirPath( PathNum ).ComponentName( CompNum ), IsNotOK, CurrentModuleObject );
					if ( IsNotOK ) {
						ShowContinueError( "In " + CurrentModuleObject + " = " + ReturnAirPath( PathNum ).Name );
						ErrorsFound = true;
					}
					if ( AlphArray( Counter ) == "AIRLOOPHVAC:ZONEMIXER" ) ReturnAirPath( PathNum ).ComponentType_Num( CompNum ) = ZoneMixer_Type;
					if ( AlphArray( Counter ) == "AIRLOOPHVAC:RETURNPLENUM" ) ReturnAirPath( PathNum ).ComponentType_Num( CompNum ) = ZoneReturnPlenum_Type;
				} else {
					ShowSevereError( RoutineName + cAlphaFields( 1 ) + "=\"" + ReturnAirPath( PathNum ).Name + "\"" );
					ShowContinueError( "Unhandled component type =\"" + AlphArray( Counter ) + "\"." );
					ShowContinueError( "Must be \"AirLoopHVAC:ZoneMixer\" or \"AirLoopHVAC:ReturnPlenum\"" );
					ErrorsFound = true;
				}

				Counter += 2;

			}

		} // end loop over return air paths

		AlphArray.deallocate();
		NumArray.deallocate();
		cAlphaFields.deallocate();
		cNumericFields.deallocate();
		lAlphaBlanks.deallocate();
		lNumericBlanks.deallocate();

		//setup zone equipment info for convection correlations
		SetupZoneEquipmentForConvectionFlowRegime();

		if ( ErrorsFound ) {
			ShowFatalError( RoutineName + "Errors found in getting Zone Equipment input." );
		}

	}

	void
	SetupZoneEquipmentForConvectionFlowRegime()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Brent Griffith
		//       DATE WRITTEN   Aug 2010
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Decide a few one-time things for later
		// determination of flow regime for convection

		// METHODOLOGY EMPLOYED:
		// <description>

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

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
		int ZoneLoop;

		for ( ZoneLoop = 1; ZoneLoop <= NumOfZones; ++ZoneLoop ) {

		}

	}

	bool
	CheckZoneEquipmentList(
		std::string const & ComponentType, // Type of component
		std::string const & ComponentName, // Name of component
		Optional_int CtrlZoneNum
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Linda Lawrie
		//       DATE WRITTEN   May 2007
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// Provides a way to check if a component name is listed on a zone equipment list.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using InputProcessor::SameString;

		// Return value
		bool IsOnList; // True if item is on a list, false if not.

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		int Loop;
		int ListLoop;
		int CtrlZoneNumLocal;

		CtrlZoneNumLocal = 0;
		IsOnList = false;
		for ( Loop = 1; Loop <= NumOfZones; ++Loop ) { // NumOfZoneEquipLists
			if ( ZoneEquipList( Loop ).Name == "" ) continue; // dimensioned by NumOfZones.  Only valid ones have names.
			for ( ListLoop = 1; ListLoop <= ZoneEquipList( Loop ).NumOfEquipTypes; ++ListLoop ) {
				if ( ! SameString( ZoneEquipList( Loop ).EquipType( ListLoop ), ComponentType ) ) continue;
				if ( ComponentName == "*" ) {
					IsOnList = true;
					CtrlZoneNumLocal = Loop;
					goto EquipList_exit;
				}
				if ( ! SameString( ZoneEquipList( Loop ).EquipName( ListLoop ), ComponentName ) ) continue;
				IsOnList = true;
				CtrlZoneNumLocal = Loop;
				goto EquipList_exit;
			}
		}
		EquipList_exit: ;
		if ( present( CtrlZoneNum ) ) {
			CtrlZoneNum = CtrlZoneNumLocal;
		}
		return IsOnList;

	}

	int
	GetControlledZoneIndex( std::string const & ZoneName ) // Zone name to match into Controlled Zone structure
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Linda Lawrie
		//       DATE WRITTEN   March 2008
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// This function returns the index into the Controlled Zone Equipment structure
		// of the indicated zone.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using InputProcessor::FindItemInList;

		// Return value
		int ControlledZoneIndex; // Index into Controlled Zone structure

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
		if ( ! ZoneEquipInputsFilled ) {
			GetZoneEquipmentData1();
			ZoneEquipInputsFilled = true;
		}

		ControlledZoneIndex = FindItemInList( ZoneName, ZoneEquipConfig.ZoneName(), NumOfZones );

		return ControlledZoneIndex;

	}

	int
	FindControlledZoneIndexFromSystemNodeNumberForZone( int const TrialZoneNodeNum ) // Node number to match into Controlled Zone structure
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Brent Griffith
		//       DATE WRITTEN   August 2013
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// This function returns the zone number for the indicated
		// zone node num.  Returns 0 if did not find zone node in any Zone

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using InputProcessor::FindItemInList;

		// Return value
		int ControlledZoneIndex; // Index into Controlled Zone structure

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		bool FoundIt;
		int ZoneNum;

		FoundIt = false;

		if ( ! ZoneEquipInputsFilled ) {
			GetZoneEquipmentData1();
			ZoneEquipInputsFilled = true;
		}
		ControlledZoneIndex = 0;
		for ( ZoneNum = 1; ZoneNum <= NumOfZones; ++ZoneNum ) {
			if ( ZoneEquipConfig( ZoneNum ).ActualZoneNum > 0 ) {
				if ( TrialZoneNodeNum == ZoneEquipConfig( ZoneNum ).ZoneNode ) {
					// found it.
					FoundIt = true;
					ControlledZoneIndex = ZoneEquipConfig( ZoneNum ).ActualZoneNum;
				}
			}
		}

		return ControlledZoneIndex;

	}

	int
	GetSystemNodeNumberForZone( std::string const & ZoneName ) // Zone name to match into Controlled Zone structure
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Linda Lawrie
		//       DATE WRITTEN   March 2008
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// This function returns the system node number for the indicated
		// zone.  Returns 0 if the Zone is not a controlled zone.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using InputProcessor::FindItemInList;

		// Return value
		int SystemZoneNodeNumber; // System node number for controlled zone

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		int ControlledZoneIndex;

		if ( ! ZoneEquipInputsFilled ) {
			GetZoneEquipmentData1();
			ZoneEquipInputsFilled = true;
		}

		ControlledZoneIndex = FindItemInList( ZoneName, ZoneEquipConfig.ZoneName(), NumOfZones );
		SystemZoneNodeNumber = 0; // default is not found
		if ( ControlledZoneIndex > 0 ) {
			if ( ZoneEquipConfig( ControlledZoneIndex ).ActualZoneNum > 0 ) {
				SystemZoneNodeNumber = ZoneEquipConfig( ControlledZoneIndex ).ZoneNode;
			}
		}

		return SystemZoneNodeNumber;

	}

	int
	GetReturnAirNodeForZone( std::string const & ZoneName ) // Zone name to match into Controlled Zone structure
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Linda Lawrie
		//       DATE WRITTEN   March 2008
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// This function returns the return air node number for the indicated
		// zone.  Returns 0 if the Zone is not a controlled zone.

		// METHODOLOGY EMPLOYED:
		// <description>

		// REFERENCES:
		// na

		// Using/Aliasing
		using InputProcessor::FindItemInList;

		// Return value
		int ReturnAirNodeNumber; // Return Air node number for controlled zone

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		int ControlledZoneIndex;

		if ( ! ZoneEquipInputsFilled ) {
			GetZoneEquipmentData1();
			ZoneEquipInputsFilled = true;
		}

		ControlledZoneIndex = FindItemInList( ZoneName, ZoneEquipConfig.ZoneName(), NumOfZones );
		ReturnAirNodeNumber = 0; // default is not found
		if ( ControlledZoneIndex > 0 ) {
			if ( ZoneEquipConfig( ControlledZoneIndex ).ActualZoneNum > 0 ) {
				ReturnAirNodeNumber = ZoneEquipConfig( ControlledZoneIndex ).ReturnAirNode;
			}
		}

		return ReturnAirNodeNumber;

	}

	Real64
	CalcDesignSpecificationOutdoorAir(
		int const DSOAPtr, // Pointer to DesignSpecification:OutdoorAir object
		int const ActualZoneNum, // Zone index
		bool const UseOccSchFlag, // Zone occupancy schedule will be used instead of using total zone occupancy
		bool const UseMinOASchFlag, // Use min OA schedule in DesignSpecification:OutdoorAir object
		Optional_bool_const PerPersonNotSet, // when calculation should not include occupants (e.g., dual duct)
		Optional_bool_const MaxOAVolFlowFlag // TRUE when calculation uses occupancy schedule  (e.g., dual duct)
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Richard Raustad, FSEC
		//       DATE WRITTEN   October 2012
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// This function returns the air volume flow rate based on DesignSpecification:OutdoorAir object.

		// METHODOLOGY EMPLOYED:
		// User inputs and zone index allows calculation of outdoor air quantity.
		// Sizing does not use occupancy or min OA schedule and will call with flags set to FALSE
		// Ventilation Rate Procedure uses occupancy schedule based on user input.

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataSizing::OARequirements; // to access DesignSpecification:OutdoorAir inputs
		using DataSizing::OAFlowNone;
		using DataSizing::OAFlowPPer;
		using DataSizing::OAFlow;
		using DataSizing::OAFlowPerArea;
		using DataSizing::OAFlowACH;
		using DataSizing::OAFlowSum;
		using DataSizing::OAFlowMax;
		using ScheduleManager::GetCurrentScheduleValue;
		using ScheduleManager::GetScheduleMaxValue;
		using DataHeatBalance::Zone;
		using DataHeatBalance::ZoneIntGain;
		using DataHeatBalance::People;
		using DataHeatBalance::TotPeople;

		// Return value
		Real64 OAVolumeFlowRate; // Return value for calculated outdoor air volume flow rate [m3/s]

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		Real64 DSOAFlowPeople; // Outdoor air volume flow rate based on occupancy (m3/s)
		Real64 DSOAFlowPerZone; // Outdoor air volume flow rate (m3/s)
		Real64 DSOAFlowPerArea; // Outdoor air volume flow rate based on zone floor area (m3/s)
		Real64 DSOAFlowACH; // Outdoor air volume flow rate based on air changes per hour (m3/s)
		Real64 PeopleCount; // total count of people in people objects
		int Loop; // index counter in LOOP
		bool PerPersonModeNotSet;
		bool MaxOAFlag;

		OAVolumeFlowRate = 0.0;
		if ( DSOAPtr == 0 ) return OAVolumeFlowRate;

		if ( present( PerPersonNotSet ) ) {
			PerPersonModeNotSet = PerPersonNotSet;
		} else {
			PerPersonModeNotSet = false;
		}

		if ( present( MaxOAVolFlowFlag ) ) {
			MaxOAFlag = MaxOAVolFlowFlag;
		} else {
			MaxOAFlag = false;
		}

		// Calculate people outdoor air flow rate as needed
		{ auto const SELECT_CASE_var( OARequirements( DSOAPtr ).OAFlowMethod );
		if ( ( SELECT_CASE_var == OAFlowPPer ) || ( SELECT_CASE_var == OAFlowSum ) || ( SELECT_CASE_var == OAFlowMax ) ) {
			if ( UseOccSchFlag ) {
				if ( MaxOAFlag ) {
					// OAPerPersonMode == PerPersonDCVByCurrentLevel (UseOccSchFlag = TRUE)
					// for dual duct, get max people according to max schedule value when requesting MaxOAFlow
					PeopleCount = 0.0;
					for ( Loop = 1; Loop <= TotPeople; ++Loop ) {
						if ( ActualZoneNum != People( Loop ).ZonePtr ) continue;
						PeopleCount += People( Loop ).NumberOfPeople * GetScheduleMaxValue( People( Loop ).NumberOfPeoplePtr );
					}
					DSOAFlowPeople = PeopleCount * OARequirements( DSOAPtr ).OAFlowPerPerson;
				} else {
					DSOAFlowPeople = ZoneIntGain( ActualZoneNum ).NOFOCC * OARequirements( DSOAPtr ).OAFlowPerPerson;
				}
			} else {
				if ( MaxOAFlag ) {
					// OAPerPersonMode == PerPersonByDesignLevel (UseOccSchFlag = FALSE)
					// use total people when requesting MaxOAFlow
					DSOAFlowPeople = Zone( ActualZoneNum ).TotOccupants * OARequirements( DSOAPtr ).OAFlowPerPerson;
				} else {
					DSOAFlowPeople = Zone( ActualZoneNum ).TotOccupants * OARequirements( DSOAPtr ).OAFlowPerPerson;
				}
			}
			if ( PerPersonModeNotSet ) DSOAFlowPeople = 0.0; // for Dual Duct if Per Person Ventilation Rate Mode is not entered
		} else {
			DSOAFlowPeople = 0.0;
		}}

		// Calculate minimum outdoor air flow rate
		{ auto const SELECT_CASE_var( OARequirements( DSOAPtr ).OAFlowMethod );
		if ( SELECT_CASE_var == OAFlowNone ) {
			// Special case for no DesignSpecification:OutdoorAir object in Sizing:Zone object
			// probably won't get to this CASE statement since it will RETURN above (Ptr=0)
			// See SizingManager GetZoneSizingInput for Sizing:Zone input field Design Specification Outdoor Air Object Name
			OAVolumeFlowRate = 0.0;
		} else if ( SELECT_CASE_var == OAFlowPPer ) {
			// Multiplied by occupancy
			OAVolumeFlowRate = DSOAFlowPeople;
		} else if ( SELECT_CASE_var == OAFlow ) {
			// User input
			OAVolumeFlowRate = OARequirements( DSOAPtr ).OAFlowPerZone;
		} else if ( SELECT_CASE_var == OAFlowPerArea ) {
			// Multiplied by zone floor area
			OAVolumeFlowRate = OARequirements( DSOAPtr ).OAFlowPerArea * Zone( ActualZoneNum ).FloorArea;
		} else if ( SELECT_CASE_var == OAFlowACH ) {
			// Multiplied by zone volume
			OAVolumeFlowRate = OARequirements( DSOAPtr ).OAFlowACH * Zone( ActualZoneNum ).Volume / 3600.0;

		} else if ( ( SELECT_CASE_var == OAFlowSum ) || ( SELECT_CASE_var == OAFlowMax ) ) {
			// Use sum or max of per person and the following
			DSOAFlowPerZone = OARequirements( DSOAPtr ).OAFlowPerZone;
			DSOAFlowPerArea = OARequirements( DSOAPtr ).OAFlowPerArea * Zone( ActualZoneNum ).FloorArea;
			DSOAFlowACH = OARequirements( DSOAPtr ).OAFlowACH * Zone( ActualZoneNum ).Volume / 3600.0;
			if ( OARequirements( DSOAPtr ).OAFlowMethod == OAFlowMax ) {
				OAVolumeFlowRate = max( DSOAFlowPeople, DSOAFlowPerZone, DSOAFlowPerArea, DSOAFlowACH );
			} else {
				OAVolumeFlowRate = DSOAFlowPeople + DSOAFlowPerZone + DSOAFlowPerArea + DSOAFlowACH;
			}

		} else {
			// Will never get here
			OAVolumeFlowRate = 0.0;
		}}

		// Apply zone multipliers and zone list multipliers
		OAVolumeFlowRate *= Zone( ActualZoneNum ).Multiplier * Zone( ActualZoneNum ).ListMultiplier;

		// Apply schedule as needed. Sizing does not use schedule.
		if ( OARequirements( DSOAPtr ).OAFlowFracSchPtr > 0 && UseMinOASchFlag ) {
			if ( MaxOAFlag ) {
				OAVolumeFlowRate *= GetScheduleMaxValue( OARequirements( DSOAPtr ).OAFlowFracSchPtr );
			} else {
				OAVolumeFlowRate *= GetCurrentScheduleValue( OARequirements( DSOAPtr ).OAFlowFracSchPtr );
			}
		}

		return OAVolumeFlowRate;
	}

	//     NOTICE

	//     Copyright � 1996-2014 The Board of Trustees of the University of Illinois
	//     and The Regents of the University of California through Ernest Orlando Lawrence
	//     Berkeley National Laboratory.  All rights reserved.

	//     Portions of the EnergyPlus software package have been developed and copyrighted
	//     by other individuals, companies and institutions.  These portions have been
	//     incorporated into the EnergyPlus software package under license.   For a complete
	//     list of contributors, see "Notice" located in main.cc.

	//     NOTICE: The U.S. Government is granted for itself and others acting on its
	//     behalf a paid-up, nonexclusive, irrevocable, worldwide license in this data to
	//     reproduce, prepare derivative works, and perform publicly and display publicly.
	//     Beginning five (5) years after permission to assert copyright is granted,
	//     subject to two possible five year renewals, the U.S. Government is granted for
	//     itself and others acting on its behalf a paid-up, non-exclusive, irrevocable
	//     worldwide license in this data to reproduce, prepare derivative works,
	//     distribute copies to the public, perform publicly and display publicly, and to
	//     permit others to do so.

	//     TRADEMARKS: EnergyPlus is a trademark of the US Department of Energy.

} // DataZoneEquipment

} // EnergyPlus
