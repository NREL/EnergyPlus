// ObjexxFCL Headers
#include <ObjexxFCL/FArray.functions.hh>
#include <ObjexxFCL/FArrayS.functions.hh>
#include <ObjexxFCL/FArray1D.hh>
#include <ObjexxFCL/Fmath.hh>
#include <ObjexxFCL/MArray.functions.hh>

// EnergyPlus Headers
#include <RoomAirModelAirflowNetwork.hh>
// #include <DataInterfaces.hh>
#include <BaseboardElectric.hh>
#include <BaseboardRadiator.hh>
#include <DataAirflowNetwork.hh>
#include <DataDefineEquip.hh>
#include <DataEnvironment.hh>
#include <DataGlobals.hh>
#include <DataHeatBalFanSys.hh>
#include <DataHeatBalance.hh>
#include <DataHeatBalSurface.hh>
#include <DataHVACGlobals.hh>
#include <DataLoopNode.hh>
#include <DataMoistureBalance.hh>
#include <DataMoistureBalanceEMPD.hh>
#include <DataPrecisionGlobals.hh>
#include <DataRoomAirModel.hh>
#include <DataSurfaces.hh>
#include <DataSurfaceLists.hh>
#include <DataZoneEquipment.hh>
#include <ElectricBaseboardRadiator.hh>
#include <General.hh>
#include <HeatBalanceHAMTManager.hh>
#include <HeatBalFiniteDiffManager.hh>
#include <HighTempRadiantSystem.hh>
#include <HWBaseboardRadiator.hh>
#include <InternalHeatGains.hh>
#include <InputProcessor.hh>
#include <UtilityRoutines.hh>
#include <FluidProperties.hh>
#include <General.hh>
#include <InputProcessor.hh>
#include <InternalHeatGains.hh>
#include <MoistureBalanceEMPDManager.hh>
#include <OutputProcessor.hh>
#include <OutputReportTabular.hh>
#include <Psychrometrics.hh>
#include <RefrigeratedCase.hh>
#include <ScheduleManager.hh>
#include <SteamBaseboardRadiator.hh>
#include <UtilityRoutines.hh>
#include <ZoneDehumidifier.hh>
#include <ZonePlenum.hh>

namespace EnergyPlus {

namespace RoomAirModelAirflowNetwork {

	// MODULE INFORMATION:
	//       AUTHOR         Brent Griffith
	//       DATE WRITTEN   November 2009
	//       MODIFIED       na
	//       RE-ENGINEERED  na

	// PURPOSE OF THIS MODULE:
	// contains the RoomAir model portions of RoomAirflowNetwork modeling

	// METHODOLOGY EMPLOYED:
	// Interact with Surface HB and Airflow Network Domains to get
	// inputs.Do heat balance calculations on roomair nodes.

	// REFERENCES:
	// none

	// OTHER NOTES:
	// na

	// Using/Aliasing
	using namespace DataPrecisionGlobals;
	using DataGlobals::MaxNameLength;
//	using DataInterfaces::ShowWarningError;
//	using DataInterfaces::ShowSevereError;
//	using DataInterfaces::ShowFatalError;
	using namespace DataRoomAirModel;
	using namespace DataHeatBalSurface;
	using namespace DataMoistureBalance;
	using namespace DataSurfaces;
	using namespace DataHeatBalance;
	// Data
	// MODULE PARAMETER DEFINITIONS:

	// MODULE DERIVED TYPE DEFINITIONS:

	// INTERFACE BLOCK SPECIFICATIONS:
	// na

	// MODULE VARIABLE DECLARATIONS:
	// see DataRoomAir

	// SUBROUTINE SPECIFICATIONS FOR MODULE 

	// Functions

	void
	SimRoomAirModelAirflowNetwork(int const ZoneNum) // index number for the specified zone
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Brent Griffith
		//       DATE WRITTEN   January 2004/Aug 2005
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		//  manage the user-defined air temp. distribution model

		// METHODOLOGY EMPLOYED:
		// calls subroutines

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int ThisRoomAirNode;

		// FLOW:


		// model control volume for each roomAir:node in the zone.
		for (ThisRoomAirNode = 1; ThisRoomAirNode <= RoomAirflowNetworkZoneInfo(ZoneNum).NumOfAirNodes; ++ThisRoomAirNode) {

			InitRoomAirModelAirflowNetwork(ZoneNum, ThisRoomAirNode);

			CalcRoomAirModelAirflowNetwork(ZoneNum, ThisRoomAirNode);

		}

		UpdateRoomAirModelAirflowNetwork(ZoneNum);

		ReportRoomAirModelAirflowNetwork(ZoneNum);


	}  //SimRoomAirModelAirflowNetwork

	//****************************************************

	void
		InitRoomAirModelAirflowNetwork(int const ZoneNum, int const RoomAirNode) // index number for the specified zone
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         B. Griffith
		//       DATE WRITTEN   November 2009
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// <description>

		// METHODOLOGY EMPLOYED:
		// <description>

		// REFERENCES:
		// na

		// Using/Aliasing
		using Psychrometrics::PsyCpAirFnWTdb;
		using Psychrometrics::PsyRhoAirFnPbTdbW;
		using DataHeatBalFanSys::ZoneLatentGain;
		using DataHeatBalFanSys::SumLatentHTRadSys;
		using DataHeatBalFanSys::NonAirSystemResponse;
		using DataHeatBalFanSys::SysDepZoneLoadsLagged;
		using DataHeatBalance::Zone;
		using DataHeatBalance::RefrigCaseCredit;
		using DataEnvironment::OutBaroPress;
		using DataGlobals::NumOfZones;
		using DataGlobals::BeginEnvrnFlag;
		using DataAirflowNetwork::AirflowNetworkLinkageData;
		using DataAirflowNetwork::AirflowNetworkLinkSimu;
		using DataAirflowNetwork::AirflowNetworkNodeSimu;
		using InternalHeatGains::SumInternalLatentGainsByTypes;
		using DataZoneEquipment::ZoneEquipConfig;
		using DataZoneEquipment::ZoneEquipList;
		using DataLoopNode::NumOfNodes;
		using DataLoopNode::NodeID;
		using DataLoopNode::Node;
		using InputProcessor::SameString;
		using General::RoundSigDigits;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		static bool MyOneTimeFlag(true);  // one time setup flag
		static bool MyOneTimeFlagConf(true); // one time setup flag for zone configuration
		static bool MyEnvrnFlag(true); // one time setup flag for zone configuration
		Real64 SumIntGain; // entire zone's sum for internal gains.
		Real64 SumHA;
		Real64 SumHATsurf;
		Real64 SumHATref;
		Real64 SumMCpDummy;
		Real64 SumMCpTDummy;
		Real64 SumSysMCpDummy;
		Real64 SumSysMCpTDummy;
		Real64 SumLinkMCp;
		Real64 SumLinkMCpT;
		int linkNum;
		Real64 LinkInTemp;
		Real64 CpAir;
		Real64 LinkInHumRat;
		Real64 LinkInMdot;
		Real64 SumLinkM;
		Real64 SumLinkMW;
		int LoopZone;
		int NumSurfs;
		int LoopAirNode;
		int NodeNum;
		int NodeIn;
		int Link;
		int IdZone;
		int IdNode;
		int EquipLoop;
		int IdEquipIndex;
		int MaxNodeNum;
		FArray1D_bool NodeFound; // True if a node is found.
		int MaxEquipNum;
		FArray1D_bool EquipFound;
		int ISum;
		bool ErrorsFound;
		int I;
		FArray1D< Real64 > SupplyFrac;
		FArray1D< Real64 > ReturnFrac;

		if (MyOneTimeFlag) {  // then do one - time setup inits

			// loop over all zones with RoomAirflowNetwork model
			for (LoopZone = 1; LoopZone <= NumOfZones; ++LoopZone) {
				if (!RoomAirflowNetworkZoneInfo(LoopZone).IsUsed) continue;
				NumSurfs = Zone(LoopZone).SurfaceLast - Zone(LoopZone).SurfaceFirst + 1;
				for (LoopAirNode = 1; LoopAirNode <= RoomAirflowNetworkZoneInfo(LoopZone).NumOfAirNodes; ++LoopAirNode) { //loop over all the modeled room air nodes
					// calculate volume of air in node's control volume
					RoomAirflowNetworkZoneInfo(LoopZone).Node(LoopAirNode).AirVolume = Zone(LoopZone).Volume* RoomAirflowNetworkZoneInfo(LoopZone).Node(LoopAirNode).ZoneVolumeFraction;

					SetupOutputVariable("RoomAirflowNetwork Node SumIntSensibleGain []", RoomAirflowNetworkZoneInfo(LoopZone).Node(LoopAirNode).SumIntSensibleGain,
						"HVAC", "Average", RoomAirflowNetworkZoneInfo(LoopZone).Node(LoopAirNode).Name);
					SetupOutputVariable("RoomAirflowNetwork Node SumHA []", RoomAirflowNetworkZoneInfo(LoopZone).Node(LoopAirNode).SumHA,
						"HVAC", "Average", RoomAirflowNetworkZoneInfo(LoopZone).Node(LoopAirNode).Name);
					SetupOutputVariable("RoomAirflowNetwork Node SumHATsurf []", RoomAirflowNetworkZoneInfo(LoopZone).Node(LoopAirNode).SumHATsurf,
						"HVAC", "Average", RoomAirflowNetworkZoneInfo(LoopZone).Node(LoopAirNode).Name);
					SetupOutputVariable("RoomAirflowNetwork Node SumHATref []", RoomAirflowNetworkZoneInfo(LoopZone).Node(LoopAirNode).SumHATref,
						"HVAC", "Average", RoomAirflowNetworkZoneInfo(LoopZone).Node(LoopAirNode).Name);
					SetupOutputVariable("RoomAirflowNetwork Node NonAirSystemResponse []", RoomAirflowNetworkZoneInfo(LoopZone).Node(LoopAirNode).NonAirSystemResponse,
						"HVAC", "Average", RoomAirflowNetworkZoneInfo(LoopZone).Node(LoopAirNode).Name);
					SetupOutputVariable("RoomAirflowNetwork Node SysDepZoneLoadsLagged []", RoomAirflowNetworkZoneInfo(LoopZone).Node(LoopAirNode).SysDepZoneLoadsLagged,
						"HVAC", "Average", RoomAirflowNetworkZoneInfo(LoopZone).Node(LoopAirNode).Name);

					SetupOutputVariable("RoomAirflowNetwork Node SumIntLatentGain []", RoomAirflowNetworkZoneInfo(LoopZone).Node(LoopAirNode).SumIntLatentGain,
						"HVAC", "Average", RoomAirflowNetworkZoneInfo(LoopZone).Node(LoopAirNode).Name);
					SetupOutputVariable("RoomAirflowNetwork Node SumHmARaW []", RoomAirflowNetworkZoneInfo(LoopZone).Node(LoopAirNode).SumHmARaW,
						"HVAC", "Average", RoomAirflowNetworkZoneInfo(LoopZone).Node(LoopAirNode).Name);
					SetupOutputVariable("RoomAirflowNetwork Node SumHmARa []", RoomAirflowNetworkZoneInfo(LoopZone).Node(LoopAirNode).SumHmARa,
						"HVAC", "Average", RoomAirflowNetworkZoneInfo(LoopZone).Node(LoopAirNode).Name);
				}
			}
			MyOneTimeFlag = false;
		}

		if (MyOneTimeFlagConf) { //then do one - time setup inits
			if (allocated(ZoneEquipConfig) && allocated(ZoneEquipList)) {
				MaxNodeNum = 0;
				MaxEquipNum = 0;
				ErrorsFound = false;
				for (LoopZone = 1; LoopZone <= NumOfZones; ++LoopZone) {
					if (!Zone(LoopZone).IsControlled) continue;
					MaxEquipNum = max(MaxEquipNum, ZoneEquipList(LoopZone).NumOfEquipTypes);
					MaxNodeNum = max(MaxNodeNum, ZoneEquipConfig(LoopZone).NumInletNodes);
				}
				if (MaxNodeNum > 0) {
					NodeFound.allocate(MaxNodeNum);
					NodeFound = false;
				}
				if (MaxEquipNum > 0) {
					EquipFound.allocate(MaxEquipNum);
					SupplyFrac.allocate(MaxEquipNum);
					ReturnFrac.allocate(MaxEquipNum);
					EquipFound = false;
					SupplyFrac = 0.0;
					ReturnFrac = 0.0;
				}

				// loop over all zones with RoomAirflowNetwork model
				for (LoopZone = 1; LoopZone <= NumOfZones; ++LoopZone) {
					if ( !Zone(LoopZone).IsControlled ) continue;
					if ( !RoomAirflowNetworkZoneInfo( LoopZone ).IsUsed ) continue;
					// find actualZoneID in ZoneEquipConfig
					for (IdZone = 1; IdZone <= NumOfZones; ++IdZone) {
						if (ZoneEquipConfig(IdZone).ActualZoneNum == LoopZone) {
							RoomAirflowNetworkZoneInfo(LoopZone).ActualZoneID = IdZone;
							break;
						}
					}
					SupplyFrac = 0.0;
					ReturnFrac = 0.0;

					// find supply air node number
					for (LoopAirNode = 1; LoopAirNode <= RoomAirflowNetworkZoneInfo(LoopZone).NumOfAirNodes; ++LoopAirNode) { //loop over all the modeled room air nodes
						for (EquipLoop = 1; EquipLoop <= RoomAirflowNetworkZoneInfo(LoopZone).Node(LoopAirNode).NumHVACs; ++EquipLoop) { //loop over all the equip for a single room air node
							// RoomAirflowNetworkZoneInfo(ZoneNum).Node(LoopAirNode).HVAC(EquipLoop).TypeOfNum = TypeNum
							// Check zone equipment name
							for (I = 1; I <= ZoneEquipList(LoopZone).NumOfEquipTypes; ++I) { //loop over all equip types
								if (SameString(ZoneEquipList(LoopZone).EquipName(I), RoomAirflowNetworkZoneInfo(LoopZone).Node(LoopAirNode).HVAC(EquipLoop).Name)) {
									if ( RoomAirflowNetworkZoneInfo( LoopZone ).Node( LoopAirNode ).HVAC( EquipLoop ).EquipConfigIndex == 0 )
										RoomAirflowNetworkZoneInfo( LoopZone ).Node( LoopAirNode ).HVAC( EquipLoop ).EquipConfigIndex = I;
									EquipFound(I) = true;
									SupplyFrac(I) = SupplyFrac(I) + RoomAirflowNetworkZoneInfo(LoopZone).Node(LoopAirNode).HVAC(EquipLoop).SupplyFraction;
									ReturnFrac(I) = ReturnFrac(I) + RoomAirflowNetworkZoneInfo(LoopZone).Node(LoopAirNode).HVAC(EquipLoop).ReturnFraction;
								}
							}
							for (IdNode = 1; IdNode <= NumOfNodes; ++IdNode) { //loop over all nodes to find supply node ID
								if (SameString(NodeID(IdNode), RoomAirflowNetworkZoneInfo(LoopZone).Node(LoopAirNode).HVAC(EquipLoop).SupplyNodeName)) {
									RoomAirflowNetworkZoneInfo(LoopZone).Node(LoopAirNode).HVAC(EquipLoop).SupNodeNum = IdNode;
									break;
								}
							}
							// Verify inlet nodes
							for (NodeNum = 1; NodeNum <= ZoneEquipConfig(LoopZone).NumInletNodes; ++NodeNum) { //loop over all supply inlet nodes in a single zone
								// !Get node conditions
								if (ZoneEquipConfig(LoopZone).InletNode(NodeNum) == IdNode) {
									NodeFound(NodeNum) = true;
								}
							}

							for ( IdNode = 1; IdNode <= NumOfNodes; ++IdNode ) { //loop over all nodes to find return node ID
								if ( SameString( NodeID( IdNode ), RoomAirflowNetworkZoneInfo( LoopZone ).Node( LoopAirNode ).HVAC( EquipLoop ).ReturnNodeName ) ) {
									RoomAirflowNetworkZoneInfo( LoopZone ).Node( LoopAirNode ).HVAC( EquipLoop ).RetNodeNum = IdNode;
									break;
								}
							}
							SetupOutputVariable( "RoomAirflowNetwork Node HVAC Supply Fraction []", RoomAirflowNetworkZoneInfo( LoopZone ).Node( LoopAirNode ).HVAC( EquipLoop ).SupplyFraction,
								"HVAC", "Average", RoomAirflowNetworkZoneInfo(LoopZone).Node(LoopAirNode).HVAC(EquipLoop).Name);
							SetupOutputVariable("RoomAirflowNetwork Node HVAC Return Fraction []", RoomAirflowNetworkZoneInfo(LoopZone).Node(LoopAirNode).HVAC(EquipLoop).ReturnFraction,
								"HVAC", "Average", RoomAirflowNetworkZoneInfo(LoopZone).Node(LoopAirNode).HVAC(EquipLoop).Name);
						}
						SetupOutputVariable("RoomAirflowNetwork Node HVAC System MCp []", RoomAirflowNetworkZoneInfo(LoopZone).Node(LoopAirNode).SumSysMCp,
							"HVAC", "Average", RoomAirflowNetworkZoneInfo(LoopZone).Node(LoopAirNode).Name);
						SetupOutputVariable("RoomAirflowNetwork Node HVAC System MCpT []", RoomAirflowNetworkZoneInfo(LoopZone).Node(LoopAirNode).SumSysMCpT,
							"HVAC", "Average", RoomAirflowNetworkZoneInfo(LoopZone).Node(LoopAirNode).Name);
						SetupOutputVariable("RoomAirflowNetwork Node HVAC System M []", RoomAirflowNetworkZoneInfo(LoopZone).Node(LoopAirNode).SumSysM,
							"HVAC", "Average", RoomAirflowNetworkZoneInfo(LoopZone).Node(LoopAirNode).Name);
						SetupOutputVariable("RoomAirflowNetwork Node HVAC System MW []", RoomAirflowNetworkZoneInfo(LoopZone).Node(LoopAirNode).SumSysMW,
							"HVAC", "Average", RoomAirflowNetworkZoneInfo(LoopZone).Node(LoopAirNode).Name);

					}
					// Count node with.TRUE.
					ISum = 0;
					for (NodeNum = 1; NodeNum <= MaxNodeNum; ++NodeNum) { //loop over all supply inlet nodes in a single zone
						if (NodeFound(NodeNum)) ISum = ISum + 1;
					}
					// Do we need to include airloop inlet node from terminals ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ?
					if (ISum != ZoneEquipConfig(LoopZone).NumInletNodes) {
						if (ISum > ZoneEquipConfig(LoopZone).NumInletNodes) {
							ShowSevereError("GetRoomAirflowNetworkData: The number of equipment listed in RoomAirflowNetwork:Node:HVACEquipment objects");
							ShowContinueError("is greater than the number of zone configuration inlet nodes in "+ Zone(LoopZone).Name);
							ShowContinueError("Please check inputs of both objects");
							ErrorsFound = true;
						}
						else {
							ShowSevereError("GetRoomAirflowNetworkData: The number of equipment listed in RoomAirflowNetwork:Node:HVACEquipment objects");
							ShowContinueError("is less than the number of zone configuration inlet nodes in "+ Zone(LoopZone).Name);
							ShowContinueError("Please check inputs of both objects");
							ErrorsFound = true;
						}
					}

					// Check equipment names to ensure they are used in RoomAirflowNetwork : Node : HVACEquipment objects
					for (I = 1; I <= ZoneEquipList(LoopZone).NumOfEquipTypes; ++I) { //loop over all equip types
						if (!EquipFound(I)) {
							ShowSevereError("GetRoomAirflowNetworkData: The equipment listed in ZoneEquipList is not found in the lsit of RoomAir:Node:AirflowNetwork:HVACEquipment objects =");
							ShowContinueError(ZoneEquipList(LoopZone).EquipName(I) + ". Please check inputs of both objects."); 
							ErrorsFound = true;
						}
					}

					// Check fraction to ensure sum = 1.0 for every equipment
					for (I = 1; I <= ZoneEquipList(LoopZone).NumOfEquipTypes; ++I) { //loop over all equip types
						if (abs(SupplyFrac(I) - 1.0) > 0.001) {
							ShowSevereError("GetRoomAirflowNetworkData: Invalid, zone supply fractions do not sum to 1.0");
							ShowContinueError("Entered in " + ZoneEquipList(LoopZone).EquipName(I) + " defined in RoomAir:Node:AirflowNetwork:HVACEquipment");
							ShowContinueError("The Fraction of supply fraction values across all the roomair nodes in a zone needs to sum to 1.0.");
							ShowContinueError("The sum of fractions entered = " + RoundSigDigits(SupplyFrac(I), 3));
							ErrorsFound = true;
						}
						if (abs(ReturnFrac(I) - 1.0) > 0.001) {
							ShowSevereError("GetRoomAirflowNetworkData: Invalid, zone return fractions do not sum to 1.0");
							ShowContinueError("Entered in " + ZoneEquipList(LoopZone).EquipName(I) + " defined in RoomAir:Node:AirflowNetwork:HVACEquipment");
							ShowContinueError("The Fraction of return fraction values across all the roomair nodes in a zone needs to sum to 1.0.");
							ShowContinueError("The sum of fractions entered = " + RoundSigDigits(ReturnFrac(I), 3));
							ErrorsFound = true;
						}
					}

				}
				MyOneTimeFlagConf = false;
				if (allocated(NodeFound)) NodeFound.deallocate();
				if (ErrorsFound) {
					ShowFatalError("GetRoomAirflowNetworkData: Errors found getting air model input.  Program terminates.");
				}
			}
		}
		//	!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
		//TYPE RoomAirflowNetworkHVACStruct
		//INTEGER::EquipConfigIndex = 0


		if (BeginEnvrnFlag && MyEnvrnFlag) {
			for (LoopZone = 1; LoopZone <= NumOfZones; ++LoopZone) {
				if (!RoomAirflowNetworkZoneInfo(LoopZone).IsUsed) continue;
				for (LoopAirNode = 1; LoopAirNode <= RoomAirflowNetworkZoneInfo(LoopZone).NumOfAirNodes; ++LoopAirNode) {  // loop over all the modeled room air nodes
					RoomAirflowNetworkZoneInfo(LoopZone).Node(LoopAirNode).AirTemp = 23.0;
					RoomAirflowNetworkZoneInfo(LoopZone).Node(LoopAirNode).AirTempX1 = 23.0;
					RoomAirflowNetworkZoneInfo(LoopZone).Node(LoopAirNode).AirTempX2 = 23.0;
					RoomAirflowNetworkZoneInfo(LoopZone).Node(LoopAirNode).AirTempX3 = 23.0;
					RoomAirflowNetworkZoneInfo(LoopZone).Node(LoopAirNode).AirTempX4 = 23.0;
					RoomAirflowNetworkZoneInfo(LoopZone).Node(LoopAirNode).AirTempDSX1 = 23.0;
					RoomAirflowNetworkZoneInfo(LoopZone).Node(LoopAirNode).AirTempDSX2 = 23.0;
					RoomAirflowNetworkZoneInfo(LoopZone).Node(LoopAirNode).AirTempDSX3 = 23.0;
					RoomAirflowNetworkZoneInfo(LoopZone).Node(LoopAirNode).AirTempDSX4 = 23.0;

					RoomAirflowNetworkZoneInfo(LoopZone).Node(LoopAirNode).HumRat = 0.0;
					RoomAirflowNetworkZoneInfo(LoopZone).Node(LoopAirNode).HumRatX1 = 0.0;
					RoomAirflowNetworkZoneInfo(LoopZone).Node(LoopAirNode).HumRatX2 = 0.0;
					RoomAirflowNetworkZoneInfo(LoopZone).Node(LoopAirNode).HumRatX3 = 0.0;
					RoomAirflowNetworkZoneInfo(LoopZone).Node(LoopAirNode).HumRatX4 = 0.0;
					RoomAirflowNetworkZoneInfo(LoopZone).Node(LoopAirNode).HumRatDSX1 = 0.0;
					RoomAirflowNetworkZoneInfo(LoopZone).Node(LoopAirNode).HumRatDSX2 = 0.0;
					RoomAirflowNetworkZoneInfo(LoopZone).Node(LoopAirNode).HumRatDSX3 = 0.0;
					RoomAirflowNetworkZoneInfo(LoopZone).Node(LoopAirNode).HumRatDSX4 = 0.0;
				}
			}
			MyEnvrnFlag = false;
		}
		if (!BeginEnvrnFlag) {
			MyEnvrnFlag = true;
		}

		// reuse code in ZoneTempPredictorCorrector for sensible components.
		CalcNodeSums(ZoneNum, RoomAirNode);
		
		/* move assignments to the subroutine of
			!RoomAirflowNetworkZoneInfo(ZoneNum).Node(RoomAirNode).SumHA = SumHA
			!RoomAirflowNetworkZoneInfo(ZoneNum).Node(RoomAirNode).SumHATsurf = SumHATsurf
			!RoomAirflowNetworkZoneInfo(ZoneNum).Node(RoomAirNode).SumHATref = SumHATref */

		SumNonAirSystemResponseForNode(ZoneNum, RoomAirNode);

		/* RoomAirflowNetworkZoneInfo(ZoneNum).Node(RoomAirNode).NonAirSystemResponse = &
			!RoomAirflowNetworkZoneInfo(ZoneNum).Node(RoomAirNode).ConvectSensibleGainFraction  &
			!* NonAirSystemResponse(ZoneNum) / (Zone(ZoneNum).Multiplier * Zone(ZoneNum).ListMultiplier) */

		SumSystemDepResponseForNode(ZoneNum, RoomAirNode);
		// RoomAirflowNetworkZoneInfo(ZoneNum).Node(RoomAirNode).SysDepZoneLoadsLagged = &
		// RoomAirflowNetworkZoneInfo(ZoneNum).Node(RoomAirNode).ConvectSensibleGainFraction * SysDepZoneLoadsLagged(ZoneNum)

		// latent gains.

		/**************************************** Need to revise
			!CALL SumInternalLatentGainsByTypes(ZoneNum, RoomAirflowNetworkZoneInfo(ZoneNum).Node(RoomAirNode).IntGainsDeviceIndices, &
			!RoomAirflowNetworkZoneInfo(ZoneNum).Node(RoomAirNode).IntGainsFractions, &
			!RoomAirflowNetworkZoneInfo(ZoneNum).Node(RoomAirNode).SumIntLatentGain) */
		if (allocated(RoomAirflowNetworkZoneInfo(ZoneNum).Node(RoomAirNode).SurfMask)) {
			CalcSurfaceMoistureSums(ZoneNum, RoomAirNode, RoomAirflowNetworkZoneInfo(ZoneNum).Node(RoomAirNode).SumHmAW,
				RoomAirflowNetworkZoneInfo(ZoneNum).Node(RoomAirNode).SumHmARa,
				RoomAirflowNetworkZoneInfo(ZoneNum).Node(RoomAirNode).SumHmARaW,
				RoomAirflowNetworkZoneInfo(ZoneNum).Node(RoomAirNode).SurfMask);
		}

		/* prepare AirflowNetwor flow rates and temperatures
			!Call UpdateConditionsFromAirflowNetwork(not sure if in this module or in airflow network, but couples the two)
			!Assign values from the AirflowNetwork model
			!? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? */
		SumLinkMCp = 0.0;
		SumLinkMCpT = 0.0;
		SumLinkM = 0.0;
		SumLinkMW = 0.0;

		// Is necessary ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? */
			NodeNum = RoomAirflowNetworkZoneInfo(ZoneNum).Node(RoomAirNode).AirflowNetworkNodeID;
		if (NodeNum > 0) {
			for (linkNum = 1; linkNum <= RoomAirflowNetworkZoneInfo(ZoneNum).Node(RoomAirNode).NumOfAirflowLinks; ++linkNum) {
				Link = RoomAirflowNetworkZoneInfo(ZoneNum).Node(RoomAirNode).Link(linkNum).AirflowNetworkLinkSimuID;
				if (AirflowNetworkLinkageData(Link).NodeNums(1) == NodeNum) {  // incoming flow
					NodeIn = AirflowNetworkLinkageData(Link).NodeNums(2);
					RoomAirflowNetworkZoneInfo(ZoneNum).Node(RoomAirNode).Link(linkNum).TempIn = AirflowNetworkNodeSimu(NodeIn).TZ;
					RoomAirflowNetworkZoneInfo(ZoneNum).Node(RoomAirNode).Link(linkNum).HumRatIn = AirflowNetworkNodeSimu(NodeIn).WZ;
					RoomAirflowNetworkZoneInfo(ZoneNum).Node(RoomAirNode).Link(linkNum).MdotIn = AirflowNetworkLinkSimu(Link).FLOW2;
				}
				if (AirflowNetworkLinkageData(Link).NodeNums(2) == NodeNum) { // outgoing flow
					NodeIn = AirflowNetworkLinkageData(Link).NodeNums(1);
					RoomAirflowNetworkZoneInfo(ZoneNum).Node(RoomAirNode).Link(linkNum).TempIn = AirflowNetworkNodeSimu(NodeIn).TZ;
					RoomAirflowNetworkZoneInfo(ZoneNum).Node(RoomAirNode).Link(linkNum).HumRatIn = AirflowNetworkNodeSimu(NodeIn).WZ;
					RoomAirflowNetworkZoneInfo(ZoneNum).Node(RoomAirNode).Link(linkNum).MdotIn = AirflowNetworkLinkSimu(Link).FLOW;
				}
			}

			for (linkNum = 1; linkNum <= RoomAirflowNetworkZoneInfo(ZoneNum).Node(RoomAirNode).NumOfAirflowLinks; ++linkNum) {
				LinkInTemp = RoomAirflowNetworkZoneInfo(ZoneNum).Node(RoomAirNode).Link(linkNum).TempIn;
				LinkInHumRat = RoomAirflowNetworkZoneInfo(ZoneNum).Node(RoomAirNode).Link(linkNum).HumRatIn;
				LinkInMdot = RoomAirflowNetworkZoneInfo(ZoneNum).Node(RoomAirNode).Link(linkNum).MdotIn;
				CpAir = PsyCpAirFnWTdb(LinkInHumRat, LinkInTemp, "InitRoomAirModelAirflowNetwork");
				SumLinkMCp = SumLinkMCp + CpAir * LinkInMdot;
				SumLinkMCpT = SumLinkMCpT + CpAir * LinkInMdot * LinkInTemp;
				SumLinkM = SumLinkM + LinkInMdot;
				SumLinkMW = SumLinkMW + LinkInMdot * LinkInHumRat;
			}
		}


		// RoomAirflowNetworkZoneInfo(ZoneNum).Node(RoomAirNode).SumLinkMCp = SumLinkMCp
		// RoomAirflowNetworkZoneInfo(ZoneNum).Node(RoomAirNode).SumLinkMCpT = SumLinkMCpT
		// RoomAirflowNetworkZoneInfo(ZoneNum).Node(RoomAirNode).SumLinkM = SumLinkM
		// RoomAirflowNetworkZoneInfo(ZoneNum).Node(RoomAirNode).SumLinkMW = SumLinkMW

		RoomAirflowNetworkZoneInfo(ZoneNum).Node(RoomAirNode).RhoAir =
			PsyRhoAirFnPbTdbW(OutBaroPress, RoomAirflowNetworkZoneInfo(ZoneNum).Node(RoomAirNode).AirTemp,
			RoomAirflowNetworkZoneInfo(ZoneNum).Node(RoomAirNode).HumRat, "InitRoomAirModelAirflowNetwork");

		RoomAirflowNetworkZoneInfo(ZoneNum).Node(RoomAirNode).CpAir =
			PsyCpAirFnWTdb(RoomAirflowNetworkZoneInfo(ZoneNum).Node(RoomAirNode).HumRat,
			RoomAirflowNetworkZoneInfo(ZoneNum).Node(RoomAirNode).AirTemp, "InitRoomAirModelAirflowNetwork");

	} // InitRoomAirModelAirflowNetwork

	//*****************************************************************************************

	void
		CalcRoomAirModelAirflowNetwork(int const ZoneNum, int const RoomAirNode) // index number for the specified zone and node
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Brent Griffith
		//       DATE WRITTEN   November 2009
		//       MODIFIED
		//       RE-ENGINEERED

		// PURPOSE OF THIS SUBROUTINE:
		// calculate new values for temperature and humidity ratio for room air node

		// METHODOLOGY EMPLOYED:
		// take terms(updated in init routine) and use classic air balance equations
		// solved for state variables.Store results in structure.

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataHVACGlobals::UseZoneTimeStepHistory;
		using DataHVACGlobals::TimeStepSys;
		using DataHeatBalFanSys::ZoneVolCapMultpSens;
		using DataHeatBalFanSys::ZoneVolCapMultpMoist;
		using DataGlobals::SecInHour;
		using Psychrometrics::PsyHgAirFnWTdb;
		using DataHeatBalFanSys::ZoneAirHumRat;
		using DataHeatBalFanSys::MAT;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		//unused    INTEGER    :: thisZoneInfo
		Real64 AvailTest;
		Real64 H2OHtOfVap;
		Real64 HumRatTmp;
		Real64 NodeTempX1;
		Real64 NodeTempX2;
		Real64 NodeTempX3;
		Real64 NodeHumRatX1;
		Real64 NodeHumRatX2;
		Real64 NodeHumRatX3;
		Real64 TempDepCoef;
		Real64 TempIndCoef;
		Real64 AirCap;
		Real64 TempTmp;
		Real64 A;
		Real64 B;
		Real64 C;			

		if (UseZoneTimeStepHistory) {
			NodeTempX1 = RoomAirflowNetworkZoneInfo(ZoneNum).Node(RoomAirNode).AirTempX1;
			NodeTempX2 = RoomAirflowNetworkZoneInfo(ZoneNum).Node(RoomAirNode).AirTempX2;
			NodeTempX3 = RoomAirflowNetworkZoneInfo(ZoneNum).Node(RoomAirNode).AirTempX3;

			NodeHumRatX1 = RoomAirflowNetworkZoneInfo(ZoneNum).Node(RoomAirNode).HumRatX1;
			NodeHumRatX2 = RoomAirflowNetworkZoneInfo(ZoneNum).Node(RoomAirNode).HumRatX2;
			NodeHumRatX3 = RoomAirflowNetworkZoneInfo(ZoneNum).Node(RoomAirNode).HumRatX3;
		} 
		else {  // use down - stepped history
			NodeTempX1 = RoomAirflowNetworkZoneInfo(ZoneNum).Node(RoomAirNode).AirTempDSX1;
			NodeTempX2 = RoomAirflowNetworkZoneInfo(ZoneNum).Node(RoomAirNode).AirTempDSX2;
			NodeTempX3 = RoomAirflowNetworkZoneInfo(ZoneNum).Node(RoomAirNode).AirTempDSX3;

			NodeHumRatX1 = RoomAirflowNetworkZoneInfo(ZoneNum).Node(RoomAirNode).HumRatDSX1;
			NodeHumRatX2 = RoomAirflowNetworkZoneInfo(ZoneNum).Node(RoomAirNode).HumRatDSX2;
			NodeHumRatX3 = RoomAirflowNetworkZoneInfo(ZoneNum).Node(RoomAirNode).HumRatDSX3;
		}

		// solve for node drybulb temperature
		TempDepCoef = RoomAirflowNetworkZoneInfo(ZoneNum).Node(RoomAirNode).SumHA + RoomAirflowNetworkZoneInfo(ZoneNum).Node(RoomAirNode).SumLinkMCp +
			RoomAirflowNetworkZoneInfo(ZoneNum).Node(RoomAirNode).SumSysMCp;
		TempIndCoef = RoomAirflowNetworkZoneInfo(ZoneNum).Node(RoomAirNode).SumIntSensibleGain
			+ RoomAirflowNetworkZoneInfo(ZoneNum).Node(RoomAirNode).SumHATsurf
			- RoomAirflowNetworkZoneInfo(ZoneNum).Node(RoomAirNode).SumHATref
			+ RoomAirflowNetworkZoneInfo(ZoneNum).Node(RoomAirNode).SumLinkMCpT + RoomAirflowNetworkZoneInfo(ZoneNum).Node(RoomAirNode).SumSysMCpT
			+ RoomAirflowNetworkZoneInfo(ZoneNum).Node(RoomAirNode).NonAirSystemResponse
			+ RoomAirflowNetworkZoneInfo(ZoneNum).Node(RoomAirNode).SysDepZoneLoadsLagged;
		AirCap = RoomAirflowNetworkZoneInfo(ZoneNum).Node(RoomAirNode).AirVolume * ZoneVolCapMultpSens
			* RoomAirflowNetworkZoneInfo(ZoneNum).Node(RoomAirNode).RhoAir
			* RoomAirflowNetworkZoneInfo(ZoneNum).Node(RoomAirNode).CpAir
			/ (TimeStepSys*SecInHour);

		TempTmp = (TempIndCoef + AirCap*(3.0*NodeTempX1 - (3.0 / 2.0)*NodeTempX2 + (1.0 / 3.0)*NodeTempX3))
			/ ((11.0 / 6.0) * AirCap + TempDepCoef);

		RoomAirflowNetworkZoneInfo(ZoneNum).Node(RoomAirNode).AirTemp = TempTmp;
		/* If(AirNode(RoomAirflowNetworkZoneInfo(ZoneNum).Node(RoomAirNode).RoomAirNodeID).IsZone) &
			!RoomAirflowNetworkZoneInfo(ZoneNum).Node(RoomAirNode).AirTemp = MAT(ZoneNum) */

		// solve for node humidity ratio using 3rd order derivative
		H2OHtOfVap = PsyHgAirFnWTdb(RoomAirflowNetworkZoneInfo(ZoneNum).Node(RoomAirNode).HumRat,
			RoomAirflowNetworkZoneInfo(ZoneNum).Node(RoomAirNode).AirTemp,"CalcRoomAirModelAirflowNetwork");
		A = RoomAirflowNetworkZoneInfo(ZoneNum).Node(RoomAirNode).SumLinkM + RoomAirflowNetworkZoneInfo(ZoneNum).Node(RoomAirNode).SumHmARa +
			RoomAirflowNetworkZoneInfo(ZoneNum).Node(RoomAirNode).SumSysM;
		B = (RoomAirflowNetworkZoneInfo(ZoneNum).Node(RoomAirNode).SumIntLatentGain / H2OHtOfVap) + RoomAirflowNetworkZoneInfo(ZoneNum).Node(RoomAirNode).SumSysMW
			+ RoomAirflowNetworkZoneInfo(ZoneNum).Node(RoomAirNode).SumLinkMW + RoomAirflowNetworkZoneInfo(ZoneNum).Node(RoomAirNode).SumHmARaW;
		C = RoomAirflowNetworkZoneInfo(ZoneNum).Node(RoomAirNode).RhoAir * RoomAirflowNetworkZoneInfo(ZoneNum).Node(RoomAirNode).AirVolume
			* ZoneVolCapMultpMoist / (SecInHour * TimeStepSys);

		HumRatTmp = (B + C*(3.0*NodeHumRatX1 - (3.0 / 2.0)*NodeHumRatX2 + (1.0 / 3.0)*NodeHumRatX3)) / ((11.0 / 6.0)*C + A);

		RoomAirflowNetworkZoneInfo(ZoneNum).Node(RoomAirNode).HumRat = HumRatTmp;
		/* If(AirNode(RoomAirflowNetworkZoneInfo(ZoneNum).Node(RoomAirNode).RoomAirNodeID).IsZone) &
			!RoomAirflowNetworkZoneInfo(ZoneNum).Node(RoomAirNode).HumRat = ZoneAirHumRat(ZoneNum) */

	} // CalcRoomAirModelAirflowNetwork

	void
		UpdateRoomAirModelAirflowNetwork( int const ZoneNum
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         B Griffith
		//       DATE WRITTEN   November 2009
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// update variables

		// METHODOLOGY EMPLOYED:
		// <description>

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataHeatBalFanSys::TempTstatAir;
		using DataHeatBalance::Zone;
		using DataZoneEquipment::ZoneEquipConfig;
		using DataZoneEquipment::ZoneEquipList;
		using DataLoopNode::Node;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		static bool MyOneTimeFlag( true );  // one time setup flag
		int ZoneNodeNum; // system node array index
		int AirNodeNum; // nested node structure index
		int I;
		int LoopAirNode;
		int EquipLoop;
		Real64 NodeMass;
		Real64 SumMass;
		Real64 SumMassT;
		Real64 SumMassW;
		int RetNodeNum;

		if ( MyOneTimeFlag ) {  // then do one - time setup inits
			MyOneTimeFlag = false;
		}

		if ( !RoomAirflowNetworkZoneInfo( ZoneNum ).IsUsed ) return;

		AirNodeNum = RoomAirflowNetworkZoneInfo(ZoneNum).ControlAirNodeID;
		TempTstatAir(ZoneNum) = RoomAirflowNetworkZoneInfo(ZoneNum).Node(AirNodeNum).AirTemp;

		// set system node value, to what ? area weighted average ?

		ZoneNodeNum = Zone(ZoneNum).SystemZoneNodeNumber;
		// Node(ZoneNodeNum).Temp =

		// Update return node conditions
		for ( I = 1; I <= ZoneEquipList( ZoneNum ).NumOfEquipTypes; ++I ) { //loop over all equip types
			SumMass = 0.0;
			SumMassT = 0.0;
			SumMassW = 0.0;
			for ( LoopAirNode = 1; LoopAirNode <= RoomAirflowNetworkZoneInfo( ZoneNum ).NumOfAirNodes; ++LoopAirNode ) { //loop over all the modeled room air nodes
				for ( EquipLoop = 1; EquipLoop <= RoomAirflowNetworkZoneInfo( ZoneNum ).Node( LoopAirNode ).NumHVACs; ++EquipLoop ) { //loop over all the equip for a single room air node
					if ( RoomAirflowNetworkZoneInfo( ZoneNum ).Node( LoopAirNode ).HVAC( EquipLoop ).EquipConfigIndex == I ) {
						NodeMass = Node( RoomAirflowNetworkZoneInfo( ZoneNum ).Node( LoopAirNode ).HVAC( EquipLoop ).SupNodeNum ).MassFlowRate * RoomAirflowNetworkZoneInfo( ZoneNum ).Node( LoopAirNode ).HVAC( EquipLoop ).ReturnFraction;
						SumMass += NodeMass;
						SumMassT += NodeMass * RoomAirflowNetworkZoneInfo( ZoneNum ).Node( LoopAirNode ).AirTemp;
						SumMassW += NodeMass * RoomAirflowNetworkZoneInfo( ZoneNum ).Node( LoopAirNode ).HumRat;
						RetNodeNum = RoomAirflowNetworkZoneInfo( ZoneNum ).Node( LoopAirNode ).HVAC( EquipLoop ).RetNodeNum;
					}
				}
			}
			if ( SumMass > 0.0 ) {
				Node( RetNodeNum ).Temp = SumMassT / SumMass;
				Node( RetNodeNum ).HumRat = SumMassW / SumMass;
			}
		}
	}  // UpdateRoomAirModelAirflowNetwork

	void
		ReportRoomAirModelAirflowNetwork(int const ZoneNum
		)
	{

			// SUBROUTINE INFORMATION:
			//       AUTHOR         B Griffith
			//       DATE WRITTEN   November 2009
			//       MODIFIED       na
			//       RE-ENGINEERED  na

			// PURPOSE OF THIS SUBROUTINE:
			// update variables

			// METHODOLOGY EMPLOYED:
			// <description>

			// REFERENCES:
			// na

			// Using/Aliasing
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

		}  // ReportRoomAirModelAirflowNetwork


	void
		CalcNodeSums(
		int const ZoneNum, 
		int const RoomAirNodeNum
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         B Griffith
		//       DATE WRITTEN   August 2009
		//       MODIFIED       Aug 2003, FCW: add SumHA contributions from window frame and divider
		//       Aug 2003, CC : change how the reference temperatures are used
		//       RE - ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE :
		// This subroutine calculates the various sums that go into the zone heat balance
		// equation.This replaces the SUMC, SUMHA, and SUMHAT calculations that were
		// previously done in various places throughout the program.
		// The SumHAT portion of the code is reproduced in RadiantSystemHighTemp and
		// RadiantSystemLowTemp and should be updated accordingly.
		// 
		// A reference temperature(Tref) is specified for use with the ceiling diffuser
		// convection correlation.A bogus value of Tref = -999.9 defaults to using
		// the zone air(i.e.outlet) temperature for the reference temperature.
		// If Tref is applied to all surfaces, SumHA = 0, and SumHATref /= 0.
		// If Tref is not used at all, SumHATref = 0, and SumHA /= 0.
		// 
		// For future implementations, Tref can be easily converted into an array to
		// allow a different reference temperature to be specified for each surface.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na
		// Using/Aliasing
		using DataSurfaces::Surface;
		using DataSurfaces::SurfaceClass_Window;
		using DataSurfaces::SurfaceWindow;
		using DataHeatBalance::Zone;
//		using namespace DataHeatBalSurface;
		using DataLoopNode::Node;
		using DataZoneEquipment::ZoneEquipConfig;
		using ZonePlenum::ZoneRetPlenCond;
		using ZonePlenum::ZoneSupPlenCond;
		using ZonePlenum::NumZoneReturnPlenums;
		using ZonePlenum::NumZoneSupplyPlenums;
		using DataDefineEquip::AirDistUnit;
		using DataDefineEquip::NumAirDistUnits;
		using DataHeatBalFanSys::ZoneAirHumRat;
		using DataAirflowNetwork::SimulateAirflowNetwork;
		using DataAirflowNetwork::AirflowNetworkExchangeData;
		using DataAirflowNetwork::AirflowNetworkZoneExhaustFan;
		using DataAirflowNetwork::AirflowNetworkNumOfExhFan;
		using DataAirflowNetwork::AirflowNetworkFanActivated;
		using DataAirflowNetwork::AirflowNetworkControlMultizone;
		using DataAirflowNetwork::AirflowNetworkControlSimpleADS;
		using DataAirflowNetwork::AirflowNetworkControlMultiADS;
		using DataAirflowNetwork::AirflowNetworkRAFNExchangeData;
		using DataGlobals::NumOfZones;
		using DataGlobals::TimeStepZone;
		using DataGlobals::SecInHour;
		using Psychrometrics::PsyCpAirFnWTdb;
		using Psychrometrics::PsyRhoAirFnPbTdbW;
		using InternalHeatGains::SumInternalConvectionGainsByTypes;
		using InternalHeatGains::SumReturnAirConvectionGainsByTypes;
		using InternalHeatGains::SumInternalConvectionGainsByIndices;
		using InternalHeatGains::SumInternalLatentGainsByIndices;
		using InternalHeatGains::SumReturnAirConvectionGainsByIndices;
		using DataHeatBalFanSys::MAT;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int NodeNum; // System node number
		Real64 NodeTemp; // System node temperature
		Real64 NodeW; // System node humidity ratio
		Real64 MassFlowRate; // System node mass flow rate
		int ZoneEquipConfigNum;
		bool  ControlledZoneAirFlag;
		int ZoneRetPlenumNum;
		int ZoneSupPlenumNum;
		bool  ZoneRetPlenumAirFlag;
		bool  ZoneSupPlenumAirFlag;
		Real64 CpAir;  // Specific heat of air
		int SurfNum; // Surface number
		Real64 HA; //                     !Hc*Area
		Real64 Area; //                   !Effective surface area
		Real64 RefAirTemp; //             !Reference air temperature for surface convection calculations
		Real64 ZoneMult;
		int ADUListIndex; 
		int ADUNum;
		int ADUInNode;
		int ADUOutNode;
		Real64 SumIntGain; //             !node sum of convective internal gains
		Real64 SumHA;                 // Zone sum of Hc*Area
		Real64 SumHATsurf; //             !Zone sum of Hc*Area*Tsurf
		Real64 SumHATref; //              !Zone sum of Hc*Area*Tref, for ceiling diffuser convection correlation
		Real64 SumMCp; //                !Zone sum of MassFlowRate*Cp
		Real64 SumMCpT; //                !Zone sum of MassFlowRate*Cp*T
		Real64 SumSysMCp; //              !Zone sum of air system MassFlowRate*Cp
		Real64 SumSysMCpT; //             !Zone sum of air system MassFlowRate*Cp*T
		Real64 SumSysM; //                !Zone sum of air system MassFlowRate
		Real64 SumSysMW; //               !Zone sum of air system MassFlowRate*W
		int EquipLoop; //              !Index of equipment loop
		int Loop; //                   !Index of RAFN node
		bool  Found; // 
		Real64 SumLinkM; //               !Zone sum of MassFlowRate from the AirflowNetwork model
		Real64 SumLinkMW; //             !Zone sum of MassFlowRate*W from the AirflowNetwork model

	// FLOW
		SumIntGain = 0.0;
		SumHA = 0.0;
		SumHATsurf = 0.0;
		SumHATref = 0.0;
		SumMCp = 0.0;
		SumMCpT = 0.0;
		SumSysMCp = 0.0;
		SumSysMCpT = 0.0;
		SumSysM = 0.0;
		SumSysMW = 0.0;
		SumLinkM = 0.0;
		SumLinkMW = 0.0;

		// Sum all convective internal gains: SumIntGain
		SumInternalConvectionGainsByIndices(ZoneNum, RoomAirflowNetworkZoneInfo(ZoneNum).Node(RoomAirNodeNum).IntGainsDeviceIndices,
			RoomAirflowNetworkZoneInfo(ZoneNum).Node(RoomAirNodeNum).IntGainsFractions, RoomAirflowNetworkZoneInfo(ZoneNum).Node(RoomAirNodeNum).SumIntSensibleGain);

		SumInternalLatentGainsByIndices( ZoneNum, RoomAirflowNetworkZoneInfo( ZoneNum ).Node( RoomAirNodeNum ).IntGainsDeviceIndices,
			RoomAirflowNetworkZoneInfo( ZoneNum ).Node( RoomAirNodeNum ).IntGainsFractions, RoomAirflowNetworkZoneInfo( ZoneNum ).Node( RoomAirNodeNum ).SumIntLatentGain );
		// Add heat to return air if zonal system(no return air) or cycling system(return air frequently very low or zero)
		if (Zone(ZoneNum).NoHeatToReturnAir) {
			// *******************************************
			SumReturnAirConvectionGainsByIndices(ZoneNum, RoomAirflowNetworkZoneInfo(ZoneNum).Node(RoomAirNodeNum).IntGainsDeviceIndices,
				RoomAirflowNetworkZoneInfo(ZoneNum).Node(RoomAirNodeNum).IntGainsFractions, SumIntGain);
			RoomAirflowNetworkZoneInfo(ZoneNum).Node(RoomAirNodeNum).SumIntSensibleGain += SumIntGain;
		}

		// we have no non - system air flow, i.e.infiltration, simple ventilation, mixing, earth tube, becasue this is AFN modeling


		// Sum all multizone air flow calculated from AirflowNetwork by assuming no simple air infiltration model
		//
		// TODO, this next needs to be for just the node, not the whole zone
		// Added by Gu *****************************************

		if (SimulateAirflowNetwork == AirflowNetworkControlMultizone || SimulateAirflowNetwork == AirflowNetworkControlMultiADS ||
			(SimulateAirflowNetwork == AirflowNetworkControlSimpleADS && AirflowNetworkFanActivated)) {
			// Multizone airflow calculated in AirflowNetwork
				//SumMCp = AirflowNetworkExchangeData(ZoneNum).SumMCp + AirflowNetworkExchangeData(ZoneNum).SumMMCp
				//SumMCpT = AirflowNetworkExchangeData(ZoneNum).SumMCpT + AirflowNetworkExchangeData(ZoneNum).SumMMCpT
			if (allocated(AirflowNetworkRAFNExchangeData)) {
				SumMCp = AirflowNetworkRAFNExchangeData(RoomAirNodeNum).SumMCp;
				SumMCpT = AirflowNetworkRAFNExchangeData(RoomAirNodeNum).SumMCpT;
				SumLinkM = AirflowNetworkRAFNExchangeData(RoomAirNodeNum).SumMHr;
				SumLinkMW = AirflowNetworkRAFNExchangeData(RoomAirNodeNum).SumMHrW;
			}
		}

		// Sum all system air flow : SumSysMCp, SumSysMCpT
		// Check to see if this is a controlled zone

		ControlledZoneAirFlag = false;
			/*If(Zone(ZoneNum).IsControlled) Then   !more CR 7384
			!ControlledZoneAirFlag = .TRUE.       !more CR 7384
			!ZoneEquipConfigNum = ZoneNum         !more CR 7384
			!endif
			!BG feb 2008 repeating this do loop every time seems crazy, store ControlledZoneAirFlag in Zone structure ?*/
		for (ZoneEquipConfigNum = 1; ZoneEquipConfigNum <= NumOfZones; ++ZoneEquipConfigNum) {
			if (!Zone(ZoneEquipConfigNum).IsControlled) continue;
			if (ZoneEquipConfig(ZoneEquipConfigNum).ActualZoneNum != ZoneNum) continue;
			ControlledZoneAirFlag = true;
			break; // sloppy way of finding ZoneEquipConfigNum for later use.
		} // ZoneEquipConfigNum

		// Check to see if this is a plenum zone
		// BG feb 2008 repeating this do loop every time seems crazy, store ControlledZoneAirFlag in Zone structure ?
		ZoneRetPlenumAirFlag = false;
		for (ZoneRetPlenumNum = 1; ZoneRetPlenumNum <= NumZoneReturnPlenums; ++ZoneRetPlenumNum) {
			if (ZoneRetPlenCond(ZoneRetPlenumNum).ActualZoneNum != ZoneNum) continue;
			ZoneRetPlenumAirFlag = true;
			break;
		} // ZoneRetPlenumNum
		ZoneSupPlenumAirFlag = false;
		for (ZoneSupPlenumNum = 1; ZoneSupPlenumNum <= NumZoneSupplyPlenums; ++ZoneSupPlenumNum) {
			if (ZoneSupPlenCond(ZoneSupPlenumNum).ActualZoneNum != ZoneNum) continue;
			ZoneSupPlenumAirFlag = true;
			break;
		} // ZoneSupPlenumNum

// Apply RoomAirNode number ?????????????????????????????????????????????
		// Plenum and controlled zones have a different set of inlet nodes which must be calculated.
		if (ControlledZoneAirFlag) {
			for (NodeNum = 1; NodeNum <= ZoneEquipConfig(ZoneEquipConfigNum).NumInletNodes; ++NodeNum) {
				// Get node conditions
				// this next block is of interest to irratic system loads... maybe nodes are not accurate at time of call ?
				// how can we tell ? predict step must be lagged ? correct step, systems have run.
				for (EquipLoop = 1; EquipLoop <= RoomAirflowNetworkZoneInfo(ZoneNum).Node(RoomAirNodeNum).NumHVACs; ++EquipLoop) {
					if (RoomAirflowNetworkZoneInfo(ZoneNum).Node(RoomAirNodeNum).HVAC(EquipLoop).SupNodeNum == ZoneEquipConfig(ZoneEquipConfigNum).InletNode(NodeNum)) {
						NodeTemp = Node(ZoneEquipConfig(ZoneEquipConfigNum).InletNode(NodeNum)).Temp;
						NodeW = Node(ZoneEquipConfig(ZoneEquipConfigNum).InletNode(NodeNum)).HumRat;
						MassFlowRate = Node(ZoneEquipConfig(ZoneEquipConfigNum).InletNode(NodeNum)).MassFlowRate * RoomAirflowNetworkZoneInfo(ZoneNum).Node(RoomAirNodeNum).HVAC(EquipLoop).SupplyFraction;
						CpAir = PsyCpAirFnWTdb(ZoneAirHumRat(ZoneNum), NodeTemp);
						SumSysMCp += MassFlowRate * CpAir;
						SumSysMCpT += MassFlowRate * CpAir * NodeTemp;
						SumSysM += MassFlowRate;
						SumSysMW += MassFlowRate * NodeW;
					}
				} // EquipLoop
			} // NodeNum
		}
		// Apply RoomAirNode number ?????????????????????????????????????????????
		// moisture and CO2 ????????????????????????????????
		else if (ZoneRetPlenumAirFlag) {
			for (NodeNum = 1; NodeNum <= ZoneRetPlenCond(ZoneRetPlenumNum).NumInletNodes; ++NodeNum) {
				// Get node conditions
				NodeTemp = Node(ZoneRetPlenCond(ZoneRetPlenumNum).InletNode(NodeNum)).Temp;
				MassFlowRate = Node(ZoneRetPlenCond(ZoneRetPlenumNum).InletNode(NodeNum)).MassFlowRate;
				CpAir = PsyCpAirFnWTdb(ZoneAirHumRat(ZoneNum), NodeTemp);
				SumSysMCp += MassFlowRate * CpAir;
				SumSysMCpT += MassFlowRate * CpAir * NodeTemp;
			} // NodeNum
			// add in the leaks
			// Apply RoomAirNode number ?????????????????????????????????????????????
			for (ADUListIndex = 1; ADUListIndex <= ZoneRetPlenCond(ZoneRetPlenumNum).NumADUs; ++ADUListIndex) {
				ADUNum = ZoneRetPlenCond(ZoneRetPlenumNum).ADUIndex(ADUListIndex);
				if (AirDistUnit(ADUNum).UpStreamLeak) {
					ADUInNode = AirDistUnit(ADUNum).InletNodeNum;
					NodeTemp = Node(ADUInNode).Temp;
					MassFlowRate = AirDistUnit(ADUNum).MassFlowRateUpStrLk;
					CpAir = PsyCpAirFnWTdb(ZoneAirHumRat(ZoneNum), NodeTemp);
					SumSysMCp += MassFlowRate * CpAir;
					SumSysMCpT += MassFlowRate * CpAir * NodeTemp;
				}
				if (AirDistUnit(ADUNum).DownStreamLeak) {
					ADUOutNode = AirDistUnit(ADUNum).OutletNodeNum;
					NodeTemp = Node(ADUOutNode).Temp;
					MassFlowRate = AirDistUnit(ADUNum).MassFlowRateDnStrLk;
					CpAir = PsyCpAirFnWTdb(ZoneAirHumRat(ZoneNum), NodeTemp);
					SumSysMCp += MassFlowRate * CpAir;
					SumSysMCpT += MassFlowRate * CpAir * NodeTemp;
				}
			} // ADUListIndex

		}
		else if (ZoneSupPlenumAirFlag) {
			// Get node conditions
			NodeTemp = Node(ZoneSupPlenCond(ZoneSupPlenumNum).InletNode).Temp;
			MassFlowRate = Node(ZoneSupPlenCond(ZoneSupPlenumNum).InletNode).MassFlowRate;
			CpAir = PsyCpAirFnWTdb(ZoneAirHumRat(ZoneNum), NodeTemp);
			SumSysMCp += MassFlowRate * CpAir;
			SumSysMCpT += MassFlowRate * CpAir * NodeTemp;

		}

		ZoneMult = Zone(ZoneNum).Multiplier * Zone(ZoneNum).ListMultiplier;

		SumSysMCp = SumSysMCp / ZoneMult;
		SumSysMCpT = SumSysMCpT / ZoneMult;
		SumSysM = SumSysM / ZoneMult;
		SumSysMW = SumSysMW / ZoneMult;

		// Sum all surface convection : SumHA, SumHATsurf, SumHATref(and additional contributions to SumIntGain)
		// Modified by Gu to include surfaces only shown in the surface lsit
		if (!RoomAirflowNetworkZoneInfo(ZoneNum).Node(RoomAirNodeNum).HasSurfacesAssigned) return;

		for (SurfNum = Zone(ZoneNum).SurfaceFirst; SurfNum <= Zone(ZoneNum).SurfaceLast; ++SurfNum) {

			if (!Surface(SurfNum).HeatTransSurf) continue;  // Skip non - heat transfer surfaces
			if (RoomAirflowNetworkZoneInfo(ZoneNum).ControlAirNodeID == RoomAirNodeNum) {
				Found = false;
				for (Loop = 1; Loop <= RoomAirflowNetworkZoneInfo(ZoneNum).NumOfAirNodes; ++Loop) {
					if (Loop != RoomAirNodeNum) {
						if (RoomAirflowNetworkZoneInfo(ZoneNum).Node(Loop).SurfMask(SurfNum - Zone(ZoneNum).SurfaceFirst + 1)) {
							Found = true;
							break;
						}
					}
				}
				if (Found) continue;
			}
			else {

				if (!RoomAirflowNetworkZoneInfo(ZoneNum).Node(RoomAirNodeNum).SurfMask(SurfNum - Zone(ZoneNum).SurfaceFirst + 1)) continue;
			}
			/* IF(Present(SurfMask)) THEN
					!!IF(.NOT.SurfMask(SurfNum)) CYCLE
					!IF(.NOT.SurfMask(SurfNum - Zone(ZoneNum).SurfaceFirst + 1)) CYCLE
					!ENDIF */

			HA = 0.0;
			Area = Surface(SurfNum).Area; // For windows, this is the glazing area

			if (Surface(SurfNum).Class == SurfaceClass_Window) {

				// Add to the convective internal gains
				if (SurfaceWindow(SurfNum).ShadingFlag == IntShadeOn || SurfaceWindow(SurfNum).ShadingFlag == IntBlindOn) {
					// The shade area covers the area of the glazing plus the area of the dividers.
					Area += SurfaceWindow(SurfNum).DividerArea;
					/*If interior shade or blind is present it is assumed that both the convective and IR radiative gain
						!from the inside surface of the divider goes directly into the zone air -- i.e., the IR radiative
						!interaction between divider and shade or blind is ignored due to the difficulty of calculating this interaction
						!at the same time that the interaction between glass and shade is calculated. */
					SumIntGain += SurfaceWindow(SurfNum).DividerConduction;
				}

				// Convective heat gain from natural convection in gap between glass and interior shade or blind
				if (SurfaceWindow(SurfNum).ShadingFlag == IntShadeOn || SurfaceWindow(SurfNum).ShadingFlag == IntBlindOn)
					SumIntGain += SurfaceWindow(SurfNum).ConvHeatFlowNatural;

				// Convective heat gain from airflow window
				if (SurfaceWindow(SurfNum).AirflowThisTS > 0.0) {
					SumIntGain += SurfaceWindow(SurfNum).ConvHeatGainToZoneAir;
					if (Zone(ZoneNum).NoHeatToReturnAir) {
						SumIntGain += SurfaceWindow(SurfNum).RetHeatGainToZoneAir;
						WinHeatGain(SurfNum) += SurfaceWindow(SurfNum).RetHeatGainToZoneAir;
						if (WinHeatGain(SurfNum) >= 0.0) {
							WinHeatGainRep(SurfNum) = WinHeatGain(SurfNum);
								WinHeatGainRepEnergy(SurfNum) = WinHeatGainRep(SurfNum) * TimeStepZone * SecInHour;
						} 
						else {
							WinHeatLossRep(SurfNum) = -WinHeatGain(SurfNum);
							WinHeatLossRepEnergy(SurfNum) = WinHeatLossRep(SurfNum) * TimeStepZone * SecInHour;
						}
					}
				}

				// Add to the surface convection sums
				if (SurfaceWindow(SurfNum).FrameArea > 0.0) {
					//Window frame contribution
					SumHATsurf += HConvIn(SurfNum) * SurfaceWindow(SurfNum).FrameArea
						* (1.0 + SurfaceWindow(SurfNum).ProjCorrFrIn) * SurfaceWindow(SurfNum).FrameTempSurfIn;
					HA += HConvIn(SurfNum) * SurfaceWindow(SurfNum).FrameArea * (1.0 + SurfaceWindow(SurfNum).ProjCorrFrIn);
				}

				if (SurfaceWindow(SurfNum).DividerArea > 0.0 && SurfaceWindow(SurfNum).ShadingFlag != IntShadeOn && SurfaceWindow(SurfNum).ShadingFlag != IntBlindOn) {
					// Window divider contribution(only from shade or blind for window with divider and interior shade or blind)
					SumHATsurf += HConvIn(SurfNum) * SurfaceWindow(SurfNum).DividerArea
						* (1.0 + 2.0 * SurfaceWindow(SurfNum).ProjCorrDivIn) * SurfaceWindow(SurfNum).DividerTempSurfIn;
					HA += HConvIn(SurfNum) * SurfaceWindow(SurfNum).DividerArea * (1.0 + 2.0 * SurfaceWindow(SurfNum).ProjCorrDivIn);
				}

			}  // End of check if window

			HA = HA + HConvIn(SurfNum) * Area;
			SumHATsurf += HConvIn(SurfNum) * Area * TempSurfInTmp(SurfNum);

			{	auto const SELECT_CASE_var(Surface(SurfNum).TAirRef);
			if (SELECT_CASE_var == ZoneMeanAirTemp) {
				// The zone air is the reference temperature(which is to be solved for in CorrectZoneAirTemp).
				RefAirTemp = MAT(ZoneNum);
				SumHA += HA;
			}
			else if (SELECT_CASE_var == AdjacentAirTemp) {
				RefAirTemp = TempEffBulkAir(SurfNum);
				SumHATref += HA * RefAirTemp;
			}
			else if (SELECT_CASE_var == ZoneSupplyAirTemp) {
				// check whether this zone is a controlled zone or not
				if (!ControlledZoneAirFlag) {
					ShowFatalError("Zones must be controlled for Ceiling-Diffuser Convection model. No system serves zone " + Zone(ZoneNum).Name);
					return;
				}
				// determine supply air temperature as a weighted average of the inlet temperatures.
				RefAirTemp = SumSysMCpT / SumSysMCp;
				SumHATref += HA * RefAirTemp;
			}
			
			else { 
				RefAirTemp = MAT(ZoneNum);
				SumHA = SumHA + HA;
			}}

		} // SurfNum

		// Assemble values
		RoomAirflowNetworkZoneInfo(ZoneNum).Node(RoomAirNodeNum).SumHA = SumHA;
		RoomAirflowNetworkZoneInfo(ZoneNum).Node(RoomAirNodeNum).SumHATsurf = SumHATsurf;
		RoomAirflowNetworkZoneInfo(ZoneNum).Node(RoomAirNodeNum).SumHATref = SumHATref;
		RoomAirflowNetworkZoneInfo(ZoneNum).Node(RoomAirNodeNum).SumLinkMCp = SumMCp;
		RoomAirflowNetworkZoneInfo(ZoneNum).Node(RoomAirNodeNum).SumLinkMCpT = SumMCpT;
		RoomAirflowNetworkZoneInfo(ZoneNum).Node(RoomAirNodeNum).SumSysMCp = SumSysMCp;
		RoomAirflowNetworkZoneInfo(ZoneNum).Node(RoomAirNodeNum).SumSysMCpT = SumSysMCpT;
		RoomAirflowNetworkZoneInfo(ZoneNum).Node(RoomAirNodeNum).SumSysM = SumSysM;
		RoomAirflowNetworkZoneInfo(ZoneNum).Node(RoomAirNodeNum).SumSysMW = SumSysMW;
		RoomAirflowNetworkZoneInfo(ZoneNum).Node(RoomAirNodeNum).SumLinkM = SumLinkM;
		RoomAirflowNetworkZoneInfo(ZoneNum).Node(RoomAirNodeNum).SumLinkMW = SumLinkMW;

	}

	void
	CalcSurfaceMoistureSums(
		int const ZoneNum,
		int const RoomAirNode,
		Real64 & SumHmAW,
		Real64 & SumHmARa,
		Real64 & SumHmARaW,
		FArray1< bool > const & SurfMask
//		Optional< FArray1D< bool > const > SurfMask // par(1) = design coil load [W]
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         B Griffith
		//                      derived from P. Biddulph-- HAMT, L. Gu -- EPMD, 
		//       DATE WRITTEN   November 2009
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Breakout summation of surface moisture interaction terms

		// METHODOLOGY EMPLOYED:
		// <description>

		// REFERENCES:
		// na

		// Using/Aliasing
		using Psychrometrics::PsyRhoAirFnPbTdbW;
		using Psychrometrics::PsyWFnTdbRhPb;
		using Psychrometrics::PsyRhFnTdbRhov;
		using Psychrometrics::PsyRhFnTdbRhovLBnd0C;
		using DataMoistureBalanceEMPD::MoistEMPDNew;
		using HeatBalFiniteDiffManager::UpdateMoistureBalanceFD;
		using HeatBalanceHAMTManager::UpdateHeatBalHAMT;
		using MoistureBalanceEMPDManager::UpdateMoistureBalanceEMPD;
//		using DataHeatBalance;
//		using DataSurfaces;
		using DataEnvironment::OutBaroPress;
		using DataHeatBalSurface::TempSurfInTmp;
		using DataHeatBalFanSys::MAT;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int SurfNum;
		int Loop;
		Real64 RhoAirZone;
		Real64 Wsurf;
		bool Found;

		SumHmAW = 0.0;
		SumHmARa = 0.0;
		SumHmARaW = 0.0;

		for (SurfNum = Zone(ZoneNum).SurfaceFirst; SurfNum <= Zone(ZoneNum).SurfaceLast; ++SurfNum) {
			if (!Surface(SurfNum).HeatTransSurf) continue; // Skip non - heat transfer surfaces
			if (Surface(SurfNum).Class == SurfaceClass_Window) continue;

			if (RoomAirflowNetworkZoneInfo(ZoneNum).ControlAirNodeID == RoomAirNode) {
				Found = false;
				for (Loop = 1; Loop <= RoomAirflowNetworkZoneInfo(ZoneNum).NumOfAirNodes; ++Loop) {
					// None - assigned surfaces belong to the zone node
					if (Loop != RoomAirNode) {
						if (RoomAirflowNetworkZoneInfo(ZoneNum).Node(Loop).SurfMask(SurfNum - Zone(ZoneNum).SurfaceFirst + 1)) {
							Found = true;
							break;
						}
					}
				}
				if (Found) continue;
			}
			else {
				if (!RoomAirflowNetworkZoneInfo(ZoneNum).Node(RoomAirNode).SurfMask(SurfNum - Zone(ZoneNum).SurfaceFirst + 1)) continue;
			}

			/*IF(Present(SurfMask)) THEN
				!IF(.NOT.SurfMask(SurfNum - Zone(ZoneNum).SurfaceFirst + 1)) CYCLE
				!ENDIF */

			if (Surface(SurfNum).HeatTransferAlgorithm == HeatTransferModel_CondFD) UpdateMoistureBalanceFD(SurfNum);

			if (Surface(SurfNum).HeatTransferAlgorithm == HeatTransferModel_HAMT) {
				UpdateHeatBalHAMT(SurfNum);

				SumHmAW += HMassConvInFD(SurfNum)*Surface(SurfNum).Area* (RhoVaporSurfIn(SurfNum) - RhoVaporAirIn(SurfNum));

				RhoAirZone = PsyRhoAirFnPbTdbW(OutBaroPress, MAT(Surface(SurfNum).Zone), PsyRhFnTdbRhov(MAT(Surface(SurfNum).Zone), RhoVaporAirIn(SurfNum), "RhoAirZone"));

				Wsurf = PsyWFnTdbRhPb(TempSurfInTmp(SurfNum), PsyRhFnTdbRhov(TempSurfInTmp(SurfNum), RhoVaporSurfIn(SurfNum), "Wsurf"), OutBaroPress);

				SumHmARa = SumHmARa + HMassConvInFD(SurfNum)*Surface(SurfNum).Area*RhoAirZone;

				SumHmARaW = SumHmARaW + HMassConvInFD(SurfNum)*Surface(SurfNum).Area*RhoAirZone*Wsurf;
			}

			if (Surface(SurfNum).HeatTransferAlgorithm == HeatTransferModel_EMPD) {
				/*need to calculate the amount of moisture that is entering or
					!leaving the zone  Qm[kg / sec] = hmi * Area * (Del Rhov)
					!{Hmi[m / sec];     Area[m2];    Rhov[kg moist / m3]  }
				!Positive values are into the zone and negative values are
					!leaving the zone.SumHmAw is the sum of the moisture entering or
					!leaving the zone from all of the surfaces and is a rate.Multiply
					!by time to get the actual amount affecting the zone volume of air.*/

				UpdateMoistureBalanceEMPD(SurfNum);
				RhoVaporSurfIn(SurfNum) = MoistEMPDNew(SurfNum);
				// SUMC(ZoneNum) = SUMC(ZoneNum) - MoistEMPDFlux(SurfNum)*Surface(SurfNum).Area

				SumHmAW = SumHmAW + HMassConvInFD(SurfNum)*Surface(SurfNum).Area* (RhoVaporSurfIn(SurfNum) - RhoVaporAirIn(SurfNum));
				SumHmARa = SumHmARa + HMassConvInFD(SurfNum)*Surface(SurfNum).Area* PsyRhoAirFnPbTdbW(OutBaroPress, TempSurfInTmp(SurfNum),
					PsyWFnTdbRhPb(TempSurfInTmp(SurfNum), PsyRhFnTdbRhovLBnd0C(TempSurfInTmp(SurfNum), RhoVaporAirIn(SurfNum)), OutBaroPress));
				SumHmARaW = SumHmARaW + HMassConvInFD(SurfNum)*Surface(SurfNum).Area*RhoVaporSurfIn(SurfNum);
			}
		}

	} // CalcSurfaceMoistureSums

	void
		SumNonAirSystemResponseForNode(
		int const ZoneNum,
		int const RAFNNodeNum
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         B. Griffith
		//       DATE WRITTEN   June 2012
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// <description>

		// METHODOLOGY EMPLOYED:
		// <description>

		// REFERENCES:
		// na

		// USE STATEMENTS:
		using HWBaseboardRadiator::SimHWBaseboard;
		using SteamBaseboardRadiator::SimSteamBaseboard;
		using BaseboardRadiator::SimBaseboard;
		using BaseboardElectric::SimElectricBaseboard;
		using RefrigeratedCase::SimAirChillerSet;
		using ElectricBaseboardRadiator::SimElecBaseboard;
		using HighTempRadiantSystem::SimHighTempRadiantSystem;
		using DataHVACGlobals::ZoneEquipTypeOf_BaseboardConvectiveWater;
		using DataHVACGlobals::ZoneEquipTypeOf_BaseboardRadiantConvectiveSteam;
		using DataHVACGlobals::ZoneEquipTypeOf_BaseboardRadiantConvectiveWater;
		using DataHVACGlobals::ZoneEquipTypeOf_BaseboardConvectiveElectric;
		using DataHVACGlobals::ZoneEquipTypeOf_RefrigerationChillerSet;
		using DataHVACGlobals::ZoneEquipTypeOf_BaseboardRadiantConvectiveElectric;
		using DataHVACGlobals::ZoneEquipTypeOf_HighTemperatureRadiant;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		int I;
		Real64 SysOutputProvided;
		Real64 LatOutputProvided;

		// TODO, FINISH THIS
		RoomAirflowNetworkZoneInfo(ZoneNum).Node(RAFNNodeNum).NonAirSystemResponse = 0.0;

		for (I = 1; I <= RoomAirflowNetworkZoneInfo(ZoneNum).Node(RAFNNodeNum).NumHVACs; ++I) {

			if ( RoomAirflowNetworkZoneInfo ( ZoneNum ).Node ( RAFNNodeNum ).HVAC ( I ).TypeOfNum == ZoneEquipTypeOf_BaseboardRadiantConvectiveWater ) {
				//'ZoneHVAC:Baseboard:RadiantConvective:Water' 13
				SimHWBaseboard(RoomAirflowNetworkZoneInfo(ZoneNum).Node(RAFNNodeNum).HVAC(I).Name, ZoneNum, RoomAirflowNetworkZoneInfo(ZoneNum).ActualZoneID, false, SysOutputProvided,
					RoomAirflowNetworkZoneInfo(ZoneNum).Node(RAFNNodeNum).HVAC(I).EquipConfigIndex);
				RoomAirflowNetworkZoneInfo(ZoneNum).Node(RAFNNodeNum).NonAirSystemResponse += SysOutputProvided;
				// LatOutputProvided = 0.0d0 !This baseboard does not add / remove any latent heat
			}

			if ( RoomAirflowNetworkZoneInfo ( ZoneNum ).Node ( RAFNNodeNum ).HVAC ( I ).TypeOfNum == ZoneEquipTypeOf_BaseboardRadiantConvectiveSteam ) {
				// CASE(BBSteam_Num) !'ZoneHVAC:Baseboard:RadiantConvective:Steam' 14
				SimSteamBaseboard(RoomAirflowNetworkZoneInfo(ZoneNum).Node(RAFNNodeNum).HVAC(I).Name, ZoneNum, RoomAirflowNetworkZoneInfo(ZoneNum).ActualZoneID, false, SysOutputProvided,
					RoomAirflowNetworkZoneInfo(ZoneNum).Node(RAFNNodeNum).HVAC(I).EquipConfigIndex);

				RoomAirflowNetworkZoneInfo(ZoneNum).Node(RAFNNodeNum).NonAirSystemResponse += SysOutputProvided;
				// LatOutputProvided = 0.0d0 !This baseboard does not add / remove any latent heat
			}

			if ( RoomAirflowNetworkZoneInfo ( ZoneNum ).Node ( RAFNNodeNum ).HVAC ( I ).TypeOfNum == ZoneEquipTypeOf_BaseboardConvectiveWater ) {
				// CASE(BBWaterConvective_Num)  !'ZoneHVAC:Baseboard:Convective:Water' 16
				SimBaseboard(RoomAirflowNetworkZoneInfo(ZoneNum).Node(RAFNNodeNum).HVAC(I).Name, ZoneNum, RoomAirflowNetworkZoneInfo(ZoneNum).ActualZoneID, false, SysOutputProvided,
					RoomAirflowNetworkZoneInfo(ZoneNum).Node(RAFNNodeNum).HVAC(I).EquipConfigIndex);
				RoomAirflowNetworkZoneInfo(ZoneNum).Node(RAFNNodeNum).NonAirSystemResponse += SysOutputProvided;
				// LatOutputProvided = 0.0d0 !This baseboard does not add / remove any latent heat
			}

			if ( RoomAirflowNetworkZoneInfo ( ZoneNum ).Node ( RAFNNodeNum ).HVAC ( I ).TypeOfNum == ZoneEquipTypeOf_BaseboardConvectiveElectric ) {
				// CASE(BBElectricConvective_Num)  !'ZoneHVAC:Baseboard:Convective:Electric' 15
				SimElectricBaseboard(RoomAirflowNetworkZoneInfo(ZoneNum).Node(RAFNNodeNum).HVAC(I).Name, ZoneNum, RoomAirflowNetworkZoneInfo(ZoneNum).ActualZoneID, SysOutputProvided,
					RoomAirflowNetworkZoneInfo(ZoneNum).Node(RAFNNodeNum).HVAC(I).EquipConfigIndex);
				RoomAirflowNetworkZoneInfo(ZoneNum).Node(RAFNNodeNum).NonAirSystemResponse += SysOutputProvided;
					// LatOutputProvided = 0.0d0 !This baseboard does not add / remove any latent heat
			}

			if ( RoomAirflowNetworkZoneInfo ( ZoneNum ).Node ( RAFNNodeNum ).HVAC ( I ).TypeOfNum == ZoneEquipTypeOf_RefrigerationChillerSet ) {
				// CASE(RefrigerationAirChillerSet_Num)  !'ZoneHVAC:RefrigerationChillerSet' 20
				SimAirChillerSet(RoomAirflowNetworkZoneInfo(ZoneNum).Node(RAFNNodeNum).HVAC(I).Name, ZoneNum, false, SysOutputProvided, LatOutputProvided, RoomAirflowNetworkZoneInfo(ZoneNum).Node(RAFNNodeNum).HVAC(I).EquipConfigIndex);
				RoomAirflowNetworkZoneInfo(ZoneNum).Node(RAFNNodeNum).NonAirSystemResponse += SysOutputProvided;
			}

			if ( RoomAirflowNetworkZoneInfo ( ZoneNum ).Node ( RAFNNodeNum ).HVAC ( I ).TypeOfNum == ZoneEquipTypeOf_BaseboardRadiantConvectiveElectric ) {
				//CASE(BBElectric_Num)  !'ZoneHVAC:Baseboard:RadiantConvective:Electric' 12
				SimElecBaseboard(RoomAirflowNetworkZoneInfo(ZoneNum).Node(RAFNNodeNum).HVAC(I).Name, ZoneNum, RoomAirflowNetworkZoneInfo(ZoneNum).ActualZoneID, false, SysOutputProvided,
					RoomAirflowNetworkZoneInfo(ZoneNum).Node(RAFNNodeNum).HVAC(I).EquipConfigIndex);
				RoomAirflowNetworkZoneInfo(ZoneNum).Node(RAFNNodeNum).NonAirSystemResponse += SysOutputProvided;
				// LatOutputProvided = 0.0d0 !This baseboard does not add / remove any latent heat
			}

			if ( RoomAirflowNetworkZoneInfo ( ZoneNum ).Node ( RAFNNodeNum ).HVAC ( I ).TypeOfNum == ZoneEquipTypeOf_HighTemperatureRadiant ) {
				//CASE(BBElectric_Num)  !'ZoneHVAC:HighTemperatureRadiant' 17
				SimHighTempRadiantSystem ( RoomAirflowNetworkZoneInfo ( ZoneNum ).Node ( RAFNNodeNum ).HVAC ( I ).Name, false, SysOutputProvided,
					RoomAirflowNetworkZoneInfo ( ZoneNum ).Node ( RAFNNodeNum ).HVAC ( I ).EquipConfigIndex );
				RoomAirflowNetworkZoneInfo ( ZoneNum ).Node ( RAFNNodeNum ).NonAirSystemResponse += SysOutputProvided;
				// LatOutputProvided = 0.0d0 !This baseboard does not add / remove any latent heat
			}

			//Zone sum of system convective gains, collected via NonAirSystemResponse
		}

	} // SumNonAirSystemResponseForNode

	//*****************************************************************************************

	void
		SumSystemDepResponseForNode(
		int const ZoneNum,
		int const RAFNNodeNum
		)
	{
		// FUNCTION INFORMATION:
		//       AUTHOR         B.Griffith
		//       DATE WRITTEN   aug 2005, Jan2004
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// return a non-dimensional height zeta

		// METHODOLOGY EMPLOYED:
		// figure average floor height (follows code in surfacegeometry.f90
		// use ceiling height from Zone structure
		// non dimensionalize surface's centroid's Z value

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na
		// Using/Aliasing
		using ZoneDehumidifier::SimZoneDehumidifier;
		using DataHVACGlobals::ZoneEquipTypeOf_DehumidifierDX;

		// Return value
		// na

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		Real64 const TolValue( .0001 );

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		int I;
		Real64 SysOutputProvided;
		Real64 LatOutputProvided;

		// TODO, FINISH THIS
		// SysDepZoneLoads saved to be added to zone heat balance next

		RoomAirflowNetworkZoneInfo(ZoneNum).Node(RAFNNodeNum).SysDepZoneLoadsLagged = 0.0;

		for (I = 1; I <= RoomAirflowNetworkZoneInfo(ZoneNum).Node(RAFNNodeNum).NumHVACs; ++I) {
			if ( RoomAirflowNetworkZoneInfo ( ZoneNum ).Node ( RAFNNodeNum ).HVAC ( I ).TypeOfNum == ZoneEquipTypeOf_DehumidifierDX ) {
				// 'ZoneHVAC:Dehumidifier:DX' 18
				SimZoneDehumidifier(RoomAirflowNetworkZoneInfo(ZoneNum).Node(RAFNNodeNum).HVAC(I).Name, ZoneNum, false, SysOutputProvided, LatOutputProvided, RoomAirflowNetworkZoneInfo(ZoneNum).Node(RAFNNodeNum).HVAC(I).EquipConfigIndex);
				RoomAirflowNetworkZoneInfo(ZoneNum).Node(RAFNNodeNum).SysDepZoneLoadsLagged = RoomAirflowNetworkZoneInfo(ZoneNum).Node(RAFNNodeNum).SysDepZoneLoadsLagged + SysOutputProvided;
			}
		}

	} // SumSystemDepResponseForNode



	//*****************************************************************************************

	//     NOTICE

	//     Copyright © 1996-2014 The Board of Trustees of the University of Illinois
	//     and The Regents of the University of California through Ernest Orlando Lawrence
	//     Berkeley National Laboratory.  All rights reserved.

	//     Portions of the EnergyPlus software package have been developed and copyrighted
	//     by other individuals, companies and institutions.  These portions have been
	//     incorporated into the EnergyPlus software package under license.   For a complete
	//     list of contributors, see "Notice" located in EnergyPlus.f90.

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

} // RoomAirModelUserTempPattern

} // EnergyPlus
