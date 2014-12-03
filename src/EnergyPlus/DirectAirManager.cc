// C++ Headers
#include <cmath>

// ObjexxFCL Headers
#include <ObjexxFCL/FArray.functions.hh>
#include <ObjexxFCL/Fmath.hh>

// EnergyPlus Headers
#include <DirectAirManager.hh>
#include <DataAirflowNetwork.hh>
#include <DataEnvironment.hh>
#include <DataHVACGlobals.hh>
#include <DataIPShortCuts.hh>
#include <DataLoopNode.hh>
#include <DataPrecisionGlobals.hh>
#include <DataSizing.hh>
#include <DataZoneEquipment.hh>
#include <EMSManager.hh>
#include <General.hh>
#include <GeneralRoutines.hh>
#include <InputProcessor.hh>
#include <NodeInputManager.hh>
#include <OutputProcessor.hh>
#include <Psychrometrics.hh>
#include <ReportSizingManager.hh>
#include <ScheduleManager.hh>
#include <SplitterComponent.hh>
#include <UtilityRoutines.hh>

namespace EnergyPlus {

namespace DirectAirManager {

	// Module containing the routines dealing with the DIRECT AIR
	// component.

	// MODULE INFORMATION:
	//       AUTHOR         Fred Buhl
	//       DATE WRITTEN   October 1999
	//       MODIFIED       Brent Griffith, May 2009 added EMS override control of flow rate
	//       RE-ENGINEERED  na

	// PURPOSE OF THIS MODULE:
	// To encapsulate the data and algorithms required to
	// simulate the direct air component  Direct air
	// is the component used to pass supply air directly
	// into a zone without any thermostatic control.

	// METHODOLOGY EMPLOYED:

	// REFERENCES: none

	// OTHER NOTES: none

	// USE STATEMENTS:
	// Use statements for data only modules
	// Using/Aliasing
	using namespace DataPrecisionGlobals;
	using namespace DataGlobals;
	using namespace ScheduleManager;
	using DataLoopNode::Node;
	using DataEnvironment::StdBaroPress;
	using DataEnvironment::StdRhoAir;
	using DataHVACGlobals::SmallAirVolFlow;

	// Data
	//MODULE PARAMETER DEFINITIONS:

	//Type declarations in DirectAir module

	//MODULE VARIABLE DECLARATIONS:
	int NumDirectAir( 0 );
	FArray1D_bool CheckEquipName;

	//SUBROUTINE SPECIFICATIONS FOR MODULE AirLoopSplitter

	// Object Data
	FArray1D< DirectAirProps > DirectAir;

	// Functions

	void
	SimDirectAir(
		std::string const & EquipName,
		int const ControlledZoneNum,
		bool const FirstHVACIteration,
		Real64 & SensOutputProvided,
		Real64 & LatOutputProvided, // Latent output provided (kg/s), dehumidification = negative
		int & CompIndex
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Fred Buhl
		//       DATE WRITTEN   Oct 1999
		//       MODIFIED       Don Shirey, Aug 2009, LatOutputProvided
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE
		// This subroutine manages Direct Air component simulation.
		// It is called from SimZoneEquipment.

		// METHODOLOGY EMPLOYED:

		// REFERENCES:

		// USE STATEMENTS:

		// Using/Aliasing
		using InputProcessor::FindItemInList;
		using General::TrimSigDigits;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS

		// SUBROUTINE PARAMETER DEFINITIONS:

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int DirectAirNum;
		static bool GetDirectAirInputFlag( true );

		if ( GetDirectAirInputFlag ) { //First time subroutine has been entered
			GetDirectAirInput();
			GetDirectAirInputFlag = false;
		}

		// Find the correct Direct Air Equipment
		if ( CompIndex == 0 ) {
			DirectAirNum = FindItemInList( EquipName, DirectAir.EquipID(), NumDirectAir );
			if ( DirectAirNum == 0 ) {
				ShowFatalError( "SimDirectAir: Unit not found=" + EquipName );
			}
			CompIndex = DirectAirNum;
		} else {
			DirectAirNum = CompIndex;
			if ( DirectAirNum > NumDirectAir || DirectAirNum < 1 ) {
				ShowFatalError( "SimDirectAir:  Invalid CompIndex passed=" + TrimSigDigits( DirectAirNum ) + ", Number of Units=" + TrimSigDigits( NumDirectAir ) + ", Entered Unit name=" + EquipName );
			}
			if ( CheckEquipName( DirectAirNum ) ) {
				if ( EquipName != DirectAir( DirectAirNum ).EquipID ) {
					ShowFatalError( "SimDirectAir: Invalid CompIndex passed=" + TrimSigDigits( DirectAirNum ) + ", Unit name=" + EquipName + ", stored Unit Name for that index=" + DirectAir( DirectAirNum ).EquipID );
				}
				CheckEquipName( DirectAirNum ) = false;
			}
		}
		if ( DirectAirNum <= 0 ) {
			ShowFatalError( "SimDirectAir: Unit not found=" + EquipName );
		}

		// With the correct DirectAirNum to Initialize the system
		InitDirectAir( DirectAirNum, FirstHVACIteration );

		CalcDirectAir( DirectAirNum, ControlledZoneNum, SensOutputProvided, LatOutputProvided );

		// No Update

		ReportDirectAir( DirectAirNum );

	}

	void
	GetDirectAirInput()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Fred Buhl
		//       DATE WRITTEN   Mar 1999
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE
		// Input the AirLoopSplitter data and store it in the AirLoopSplitter array.

		// METHODOLOGY EMPLOYED:
		// Use the Get routines from the InputProcessor module.

		// REFERENCES:
		// na

		// Using/Aliasing
		using InputProcessor::GetNumObjectsFound;
		using InputProcessor::GetObjectItem;
		using InputProcessor::VerifyName;
		using NodeInputManager::GetOnlySingleNode;
		using DataGlobals::AnyEnergyManagementSystemInModel;
		using DataGlobals::ScheduleAlwaysOn;
		using DataZoneEquipment::ZoneEquipConfig;
		using SplitterComponent::SplitterCond;
		using SplitterComponent::NumSplitters;
		using namespace DataLoopNode;
		using namespace DataIPShortCuts;

		// Locals
		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:    INTEGER :: BaseboardNum
		int NumNums; // Number of REAL(r64) numbers returned by GetObjectItem
		int NumAlphas; // Number of alphanumerics returned by GetObjectItem
		int DirectAirNum;
		int IOStat;
		static std::string const RoutineName( "GetDirectAirInput: " ); // include trailing blank space
		static bool ErrorsFound( false );
		bool IsNotOK; // Flag to verify name
		bool IsBlank; // Flag for blank name
		int Loop; // Do Loop Index
		int CtrlZone; // controlled zome do loop index
		int SupAirIn; // controlled zone supply air inlet index
		int NodeNum;
		int SplitNum;

		cCurrentModuleObject = "AirTerminal:SingleDuct:Uncontrolled";

		NumDirectAir = GetNumObjectsFound( cCurrentModuleObject );

		if ( NumDirectAir > 0 ) {

			DirectAir.allocate( NumDirectAir );
			CheckEquipName.dimension( NumDirectAir, true );

			for ( DirectAirNum = 1; DirectAirNum <= NumDirectAir; ++DirectAirNum ) {
				DirectAir( DirectAirNum ).cObjectName = cCurrentModuleObject; // push Object Name into data array
				GetObjectItem( cCurrentModuleObject, DirectAirNum, cAlphaArgs, NumAlphas, rNumericArgs, NumNums, IOStat, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );
				IsNotOK = false;
				IsBlank = false;
				VerifyName( cAlphaArgs( 1 ), DirectAir.EquipID(), DirectAirNum - 1, IsNotOK, IsBlank, cCurrentModuleObject + " Name" );
				if ( IsNotOK ) {
					ErrorsFound = true;
					if ( IsBlank ) cAlphaArgs( 1 ) = "xxxxxxxx";
				}
				DirectAir( DirectAirNum ).EquipID = cAlphaArgs( 1 );
				DirectAir( DirectAirNum ).Schedule = cAlphaArgs( 2 );
				if ( lAlphaFieldBlanks( 2 ) ) {
					DirectAir( DirectAirNum ).SchedPtr = ScheduleAlwaysOn;
				} else {
					DirectAir( DirectAirNum ).SchedPtr = GetScheduleIndex( cAlphaArgs( 2 ) );
					if ( DirectAir( DirectAirNum ).SchedPtr == 0 ) {
						ShowSevereError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\", invalid data." );
						ShowContinueError( "..invalid " + cAlphaFieldNames( 2 ) + "=\"" + cAlphaArgs( 2 ) + "\"." );
						ErrorsFound = true;
					}
				}
				// Direct air is a problem for node connections since it only has a single node
				// make this an outlet
				DirectAir( DirectAirNum ).ZoneSupplyAirNode = GetOnlySingleNode( cAlphaArgs( 3 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Air, NodeConnectionType_Outlet, 1, ObjectIsNotParent, cAlphaFieldNames( 3 ) );
				//Load the maximum volume flow rate
				DirectAir( DirectAirNum ).MaxAirVolFlowRate = rNumericArgs( 1 );

				// Fill the Zone Equipment data with the supply air inlet node number of this unit.
				for ( CtrlZone = 1; CtrlZone <= NumOfZones; ++CtrlZone ) {
					if ( ! ZoneEquipConfig( CtrlZone ).IsControlled ) continue;
					for ( SupAirIn = 1; SupAirIn <= ZoneEquipConfig( CtrlZone ).NumInletNodes; ++SupAirIn ) {
						if ( DirectAir( DirectAirNum ).ZoneSupplyAirNode == ZoneEquipConfig( CtrlZone ).InletNode( SupAirIn ) ) {
							ZoneEquipConfig( CtrlZone ).AirDistUnitCool( SupAirIn ).InNode = DirectAir( DirectAirNum ).ZoneSupplyAirNode;
							ZoneEquipConfig( CtrlZone ).AirDistUnitCool( SupAirIn ).OutNode = DirectAir( DirectAirNum ).ZoneSupplyAirNode;
						}
					}
				}

				// Find the Zone Equipment Inlet Node from the Supply Air Path Splitter
				for ( SplitNum = 1; SplitNum <= NumSplitters; ++SplitNum ) {
					for ( NodeNum = 1; NodeNum <= SplitterCond( SplitNum ).NumOutletNodes; ++NodeNum ) {
						if ( DirectAir( DirectAirNum ).ZoneSupplyAirNode == SplitterCond( SplitNum ).OutletNode( NodeNum ) ) {
							DirectAir( DirectAirNum ).ZoneEquipAirInletNode = SplitterCond( SplitNum ).InletNode;
							break;
						}
					}
				}

				// If no splitter, set Zone Equipment Inlet Node to the Zone Supply Air Node
				if ( NumSplitters == 0 ) {
					DirectAir( DirectAirNum ).ZoneEquipAirInletNode = DirectAir( DirectAirNum ).ZoneSupplyAirNode;
				}

			}
		}

		if ( ErrorsFound ) {
			ShowFatalError( RoutineName + "Errors found in input.  Program terminates." );
		}

		//Setup output for the Direct Air Units.  This allows a comparison with
		for ( Loop = 1; Loop <= NumDirectAir; ++Loop ) {
			SetupOutputVariable( "Zone Air Terminal Sensible Heating Energy [J]", DirectAir( Loop ).HeatEnergy, "System", "Sum", DirectAir( Loop ).EquipID );
			SetupOutputVariable( "Zone Air Terminal Sensible Cooling Energy [J]", DirectAir( Loop ).CoolEnergy, "System", "Sum", DirectAir( Loop ).EquipID );
			SetupOutputVariable( "Zone Air Terminal Sensible Heating Rate [W]", DirectAir( Loop ).HeatRate, "System", "Average", DirectAir( Loop ).EquipID );
			SetupOutputVariable( "Zone Air Terminal Sensible Cooling Rate [W]", DirectAir( Loop ).CoolRate, "System", "Average", DirectAir( Loop ).EquipID );

			if ( AnyEnergyManagementSystemInModel ) {
				SetupEMSActuator( "AirTerminal:SingleDuct:Uncontrolled", DirectAir( Loop ).EquipID, "Mass Flow Rate", "[kg/s]", DirectAir( Loop ).EMSOverrideAirFlow, DirectAir( Loop ).EMSMassFlowRateValue );
				SetupEMSInternalVariable( "AirTerminal:SingleDuct:Uncontrolled Maximum Mass Flow Rate", DirectAir( Loop ).EquipID, "[kg/s]", DirectAir( Loop ).AirMassFlowRateMax );
			}
		}

	}

	// Beginning Initialization Section of the Module
	//******************************************************************************

	void
	InitDirectAir(
		int const DirectAirNum,
		bool const FirstHVACIteration
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Richard J. Liesen
		//       DATE WRITTEN   January 2001
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine is for  initializations of the Direct Air Components.

		// METHODOLOGY EMPLOYED:
		// Uses the status flags to trigger events.

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataEnvironment::StdBaroPress;
		using Psychrometrics::PsyRhoAirFnPbTdbW;
		using DataAirflowNetwork::SimulateAirflowNetwork;
		using DataAirflowNetwork::AirflowNetworkFanActivated;
		using DataAirflowNetwork::AirflowNetworkControlMultizone;
		using DataZoneEquipment::ZoneEquipInputsFilled;
		using DataZoneEquipment::CheckZoneEquipmentList;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		static bool MyOneTimeFlag( true );
		static bool ZoneEquipmentListChecked( false ); // True after the Zone Equipment List has been checked for items
		static FArray1D_bool MyEnvrnFlag;
		static FArray1D_bool MySizeFlag;
		int ZoneNode;
		int Loop;

		// FLOW:
		// Do the Begin Simulation initializations
		if ( MyOneTimeFlag ) {

			MyEnvrnFlag.allocate( NumDirectAir );
			MySizeFlag.allocate( NumDirectAir );
			MyEnvrnFlag = true;
			MySizeFlag = true;

			MyOneTimeFlag = false;

		}

		// need to check all direct air units to see if they are on Zone Equipment List or issue warning
		if ( ! ZoneEquipmentListChecked && ZoneEquipInputsFilled ) {
			ZoneEquipmentListChecked = true;
			for ( Loop = 1; Loop <= NumDirectAir; ++Loop ) {
				if ( CheckZoneEquipmentList( DirectAir( DirectAirNum ).cObjectName, DirectAir( Loop ).EquipID ) ) continue;
				ShowWarningError( "InitDirectAir: [" + DirectAir( DirectAirNum ).cObjectName + " = " + DirectAir( Loop ).EquipID + "] is not on any ZoneHVAC:EquipmentList.  It will not be simulated." );
			}
		}
		if ( ! SysSizingCalc && MySizeFlag( DirectAirNum ) ) {

			SizeDirectAir( DirectAirNum );

			MySizeFlag( DirectAirNum ) = false;
		}
		// Do the Begin Environment initializations
		if ( BeginEnvrnFlag && MyEnvrnFlag( DirectAirNum ) ) {

			// Calculate the Max Mass flow rate using the air density at Standard Conditions
			DirectAir( DirectAirNum ).AirMassFlowRateMax = DirectAir( DirectAirNum ).MaxAirVolFlowRate * StdRhoAir;

			Node( DirectAir( DirectAirNum ).ZoneSupplyAirNode ).MassFlowRateMax = DirectAir( DirectAirNum ).AirMassFlowRateMax;
			Node( DirectAir( DirectAirNum ).ZoneSupplyAirNode ).MassFlowRateMin = 0.0;

			MyEnvrnFlag( DirectAirNum ) = false;
		}

		if ( ! BeginEnvrnFlag ) {
			MyEnvrnFlag( DirectAirNum ) = true;
		}

		// Set the ZoneNode number
		ZoneNode = DirectAir( DirectAirNum ).ZoneSupplyAirNode;
		if ( FirstHVACIteration ) {
			//The first time through set the mass flow rate to the Max
			if ( ( Node( ZoneNode ).MassFlowRateMaxAvail > 0.0 ) && ( GetCurrentScheduleValue( DirectAir( DirectAirNum ).SchedPtr ) > 0.0 ) ) {
				if ( ! ( SimulateAirflowNetwork > AirflowNetworkControlMultizone && AirflowNetworkFanActivated ) ) {
					Node( ZoneNode ).MassFlowRate = DirectAir( DirectAirNum ).AirMassFlowRateMax;
					Node( ZoneNode ).MassFlowRateMaxAvail = DirectAir( DirectAirNum ).AirMassFlowRateMax;
					if ( DirectAir( DirectAirNum ).EMSOverrideAirFlow ) Node( ZoneNode ).MassFlowRate = DirectAir( DirectAirNum ).EMSMassFlowRateValue;
				}
				Node( ZoneNode ).MassFlowRateMinAvail = 0.0;
			} else {
				Node( ZoneNode ).MassFlowRate = 0.0;
				Node( ZoneNode ).MassFlowRateMaxAvail = 0.0;
			}
		} else {
			//When not FirstHCAVIteration
			if ( ! DirectAir( DirectAirNum ).EMSOverrideAirFlow ) {
				if ( ( Node( ZoneNode ).MassFlowRateMaxAvail > 0.0 ) && ( GetCurrentScheduleValue( DirectAir( DirectAirNum ).SchedPtr ) > 0.0 ) ) {
					if ( Node( ZoneNode ).MassFlowRateMaxAvail < Node( ZoneNode ).MassFlowRateMax ) {
						Node( ZoneNode ).MassFlowRate = Node( ZoneNode ).MassFlowRateMaxAvail;
					} else if ( Node( ZoneNode ).MassFlowRateMinAvail > Node( ZoneNode ).MassFlowRateMin ) {
						Node( ZoneNode ).MassFlowRate = Node( ZoneNode ).MassFlowRateMinAvail;
					} else {
						Node( ZoneNode ).MassFlowRate = Node( ZoneNode ).MassFlowRateMaxAvail;
					}
				} else {
					Node( ZoneNode ).MassFlowRate = 0.0;
					Node( ZoneNode ).MassFlowRateMaxAvail = 0.0;
				}
			} else { // EMS override on
				Node( ZoneNode ).MassFlowRate = DirectAir( DirectAirNum ).EMSMassFlowRateValue;
				// but also apply constraints
				Node( ZoneNode ).MassFlowRate = min( Node( ZoneNode ).MassFlowRate, Node( ZoneNode ).MassFlowRateMaxAvail );
				Node( ZoneNode ).MassFlowRate = min( Node( ZoneNode ).MassFlowRate, Node( ZoneNode ).MassFlowRateMax );
				Node( ZoneNode ).MassFlowRate = max( Node( ZoneNode ).MassFlowRate, Node( ZoneNode ).MassFlowRateMinAvail );
				Node( ZoneNode ).MassFlowRate = max( Node( ZoneNode ).MassFlowRate, Node( ZoneNode ).MassFlowRateMin );

			}

		}

		//Set reporting varialbes to zero for the Direct Air Output
		DirectAir( DirectAirNum ).HeatRate = 0.0;
		DirectAir( DirectAirNum ).CoolRate = 0.0;
		DirectAir( DirectAirNum ).HeatEnergy = 0.0;
		DirectAir( DirectAirNum ).CoolEnergy = 0.0;

	}

	void
	SizeDirectAir( int const DirectAirNum )
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Fred Buhl
		//       DATE WRITTEN   January 2002
		//       MODIFIED       August 2013 Daeho Kang, add component sizing table entries
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine is for sizing Direct Air Components for which flow rates have not been
		// specified in the input.

		// METHODOLOGY EMPLOYED:
		// Obtains flow rates from the zone arrays.

		// REFERENCES:
		// na

		// Using/Aliasing
		using namespace DataSizing;
		using namespace InputProcessor;
		using ReportSizingManager::ReportSizingOutput;
		using General::RoundSigDigits;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 MaxAirVolFlowRateDes; // Design maximum air volume flow rate for reporting
		Real64 MaxAirVolFlowRateUser; // User hard-sized maximum air volume flow rate for reporting
		bool IsAutoSize; // Indicator to autosizing max air flow rate
		bool SizingDesRunThisZone;

		IsAutoSize = false;
		MaxAirVolFlowRateDes = 0.0;
		MaxAirVolFlowRateUser = 0.0;
		SizingDesRunThisZone = false;

		if ( CurZoneEqNum > 0 ) {

			if ( DirectAir( DirectAirNum ).MaxAirVolFlowRate == AutoSize ) {
				IsAutoSize = true;
			}
			CheckThisZoneForSizing( CurZoneEqNum, SizingDesRunThisZone );

			// Check if all are hard-sized
			if ( ! IsAutoSize && ! SizingDesRunThisZone ) { // simulation should continue
				if ( DirectAir( DirectAirNum ).MaxAirVolFlowRate > 0.0 ) {
					ReportSizingOutput( DirectAir( DirectAirNum ).cObjectName, DirectAir( DirectAirNum ).EquipID, "User-Specified Maximum Air Flow Rate [m3/s]", DirectAir( DirectAirNum ).MaxAirVolFlowRate );
				}
			} else { // AutoSize or hard-size with design run
				CheckZoneSizing( DirectAir( DirectAirNum ).cObjectName, DirectAir( DirectAirNum ).EquipID );
				MaxAirVolFlowRateDes = max( TermUnitFinalZoneSizing( CurZoneEqNum ).DesCoolVolFlow, TermUnitFinalZoneSizing( CurZoneEqNum ).DesHeatVolFlow );
				if ( MaxAirVolFlowRateDes < SmallAirVolFlow ) {
					MaxAirVolFlowRateDes = 0.0;
				}
				if ( IsAutoSize ) {
					DirectAir( DirectAirNum ).MaxAirVolFlowRate = MaxAirVolFlowRateDes;
					ReportSizingOutput( DirectAir( DirectAirNum ).cObjectName, DirectAir( DirectAirNum ).EquipID, "Design Size Maximum Air Flow Rate [m3/s]", MaxAirVolFlowRateDes );
				} else { // Hard-size with sizing data
					if ( DirectAir( DirectAirNum ).MaxAirVolFlowRate > 0.0 && MaxAirVolFlowRateDes > 0.0 && SizingDesRunThisZone ) {
						MaxAirVolFlowRateUser = DirectAir( DirectAirNum ).MaxAirVolFlowRate;
						ReportSizingOutput( DirectAir( DirectAirNum ).cObjectName, DirectAir( DirectAirNum ).EquipID, "Design Size Maximum Air Flow Rate [m3/s]", MaxAirVolFlowRateDes, "User-Specified Maximum Air Flow Rate [m3/s]", MaxAirVolFlowRateUser );
						if ( DisplayExtraWarnings ) {
							if ( ( std::abs( MaxAirVolFlowRateDes - MaxAirVolFlowRateUser ) / MaxAirVolFlowRateUser ) > AutoVsHardSizingThreshold ) {
								ShowMessage( "SizeDirectAir: Potential issue with equipment sizing for AirTerminal:SingleDuct:Uncontrolled=\"" + DirectAir( DirectAirNum ).EquipID + "\"." );
								ShowContinueError( "User-Specified Maximum Air Flow Rate of " + RoundSigDigits( MaxAirVolFlowRateUser, 5 ) + " [m3/s]" );
								ShowContinueError( "differs from Design Size Maximum Air Flow Rate of " + RoundSigDigits( MaxAirVolFlowRateDes, 5 ) + " [m3/s]" );
								ShowContinueError( "This may, or may not, indicate mismatched component sizes." );
								ShowContinueError( "Verify that the value entered is intended and is consistent with other components." );
							}
						}
					}
				}
			}
		}

	}

	// End Initialization Section of the Module
	//******************************************************************************

	void
	CalcDirectAir(
		int const DirectAirNum,
		int const ControlledZoneNum,
		Real64 & SensOutputProvided,
		Real64 & LatOutputProvided // Latent output provided, kg/s, dehumidification = negative
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Fred Buhl
		//       DATE WRITTEN   Oct 1999
		//       MODIFIED       Shirey, Aug 2009 (LatOutputProvided)
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE
		// Calculate the system SensOutputProvided and LatOutputProvided by the direct supply air connection

		// METHODOLOGY EMPLOYED:
		// Enthalpy balance

		// REFERENCES:

		// Using/Aliasing
		using DataZoneEquipment::ZoneEquipConfig;
		using DataHVACGlobals::SmallMassFlow;
		using Psychrometrics::PsyHFnTdbW;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS

		// SUBROUTINE PARAMETER DEFINITIONS:

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 MassFlowRate; // Air mass flow rate in kg/s
		Real64 SpecHumOut; // Specific humidity ratio of outlet air (kg moisture / kg moist air)
		Real64 SpecHumIn; // Specific humidity ratio of inlet [zone] air (kg moisture / kg moist air)

		// Sign convention: SysSensOutputProvided <0 Zone is cooled
		//                  SysSensOutputProvided >0 Zone is heated
		//                  SysLatOutputProvided <0 Zone is dehumidified
		//                  SysLatOutputProvided >0 Zone is humidified

		MassFlowRate = Node( DirectAir( DirectAirNum ).ZoneSupplyAirNode ).MassFlowRate;

		if ( GetCurrentScheduleValue( DirectAir( DirectAirNum ).SchedPtr ) > 0.0 && MassFlowRate > SmallMassFlow ) {

			//  Change this later ... should be using minimum humidity ratio in the calculation of enthalpy
			//    MinHumRat = MIN(Node(ZoneEquipConfig(ControlledZoneNum)%ZoneNode)%HumRat,   &
			//                         Node(DirectAir(DirectAirNum)%ZoneSupplyAirNode)%HumRat)

			SensOutputProvided = MassFlowRate * ( PsyHFnTdbW( Node( DirectAir( DirectAirNum ).ZoneSupplyAirNode ).Temp, Node( ZoneEquipConfig( ControlledZoneNum ).ZoneNode ).HumRat ) - PsyHFnTdbW( Node( ZoneEquipConfig( ControlledZoneNum ).ZoneNode ).Temp, Node( ZoneEquipConfig( ControlledZoneNum ).ZoneNode ).HumRat ) );

			//   CR9155 Remove specific humidity calculations
			SpecHumOut = Node( DirectAir( DirectAirNum ).ZoneSupplyAirNode ).HumRat;
			SpecHumIn = Node( ZoneEquipConfig( ControlledZoneNum ).ZoneNode ).HumRat;

			LatOutputProvided = MassFlowRate * ( SpecHumOut - SpecHumIn ); // Latent rate, kg/s

		} else {
			SensOutputProvided = 0.0;
			LatOutputProvided = 0.0;
		}

		DirectAir( DirectAirNum ).SensOutputProvided = SensOutputProvided;

	}

	void
	ReportDirectAir( int & DirectAirNum )
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR:          Russ Taylor
		//       DATE WRITTEN:    Nov 1997

		// PURPOSE OF THIS SUBROUTINE: This subroutine

		// METHODOLOGY EMPLOYED:

		// REFERENCES:

		// Using/Aliasing
		using DataHVACGlobals::TimeStepSys;

		//report the Direct Air Output
		DirectAir( DirectAirNum ).HeatRate = max( DirectAir( DirectAirNum ).SensOutputProvided, 0.0 );
		DirectAir( DirectAirNum ).CoolRate = std::abs( min( DirectAir( DirectAirNum ).SensOutputProvided, 0.0 ) );
		DirectAir( DirectAirNum ).HeatEnergy = max( DirectAir( DirectAirNum ).SensOutputProvided, 0.0 ) * TimeStepSys * SecInHour;
		DirectAir( DirectAirNum ).CoolEnergy = std::abs( min( DirectAir( DirectAirNum ).SensOutputProvided, 0.0 ) * TimeStepSys * SecInHour );

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

} // DirectAirManager

} // EnergyPlus
