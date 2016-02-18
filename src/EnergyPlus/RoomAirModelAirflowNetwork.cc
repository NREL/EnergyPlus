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

// ObjexxFCL Headers
#include <ObjexxFCL/Array.functions.hh>
#include <ObjexxFCL/Array1D.hh>
#include <ObjexxFCL/Fmath.hh>

// EnergyPlus Headers
#include <RoomAirModelAirflowNetwork.hh>
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
	//       MODIFIED       Lixing Gu, Aug. 2015 for v8.4 replease
	//       RE-ENGINEERED  na

	// PURPOSE OF THIS MODULE:
	// contains the RoomAir model portions of RoomAirflowNetwork modeling

	// METHODOLOGY EMPLOYED:
	// Interact with Surface HB, internal gain, HVAC system and Airflow Network Domains
	// Do heat and moisture balance calculations on roomair nodes.

	// REFERENCES:
	// none

	// OTHER NOTES:
	// na

	// Using/Aliasing
	using namespace DataPrecisionGlobals;
	using DataGlobals::MaxNameLength;
	using DataGlobals::NumOfZones;
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

	// Object Data
	Array1D< RAFNData > RAFN;

	namespace {
		bool InitRoomAirModelAirflowNetworkOneTimeFlag( true );
		bool InitRoomAirModelAirflowNetworkOneTimeFlagConf( true );
		bool InitRoomAirModelAirflowNetworkEnvrnFlag( true );
		bool LoadPredictionRoomAirModelAirflowNetworkOneTimeFlag( true );
	}

	// Functions

	void
	clear_state()
	{
		InitRoomAirModelAirflowNetworkOneTimeFlag = true;
		InitRoomAirModelAirflowNetworkOneTimeFlagConf = true;
		InitRoomAirModelAirflowNetworkEnvrnFlag = true;
		LoadPredictionRoomAirModelAirflowNetworkOneTimeFlag = true;
		RAFN.deallocate();
	}

	void
	SimRoomAirModelAirflowNetwork( int const ZoneNum ) // index number for the specified zone
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Brent Griffith
		//       DATE WRITTEN   January 2004/Aug 2005
		//       MODIFIED       Lixing Gu, Aug. 2015 for v8.4 replease
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine manages RoomAirflowNetwork model simulation

		// METHODOLOGY EMPLOYED:
		// calls subroutines

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int ThisRoomAirNode;
		int RAFNNum;

		// FLOW:
		RAFNNum = RoomAirflowNetworkZoneInfo( ZoneNum ).RAFNNum;

		if ( RAFNNum == 0 ) {
			ShowFatalError( "SimRoomAirModelAirflowNetwork: Zone is not defined in the RoomAirModelAirflowNetwork model =" + Zone(ZoneNum).Name );
		}

		auto & thisRAFN( RAFN( RAFNNum ) );
		thisRAFN.ZoneNum = ZoneNum;

		// model control volume for each roomAir:node in the zone.
		for (ThisRoomAirNode = 1; ThisRoomAirNode <= RoomAirflowNetworkZoneInfo(ZoneNum).NumOfAirNodes; ++ThisRoomAirNode) {

			thisRAFN.RoomAirNode = ThisRoomAirNode;

			thisRAFN.InitRoomAirModelAirflowNetwork( ThisRoomAirNode );

			thisRAFN.CalcRoomAirModelAirflowNetwork( ThisRoomAirNode );

		}

		thisRAFN.UpdateRoomAirModelAirflowNetwork();

	}  //SimRoomAirModelAirflowNetwork

	//****************************************************

	void
	LoadPredictionRoomAirModelAirflowNetwork( int const ZoneNum, int const RoomAirNode ) // index number for the specified zone and node
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Lixing Gu
		//       DATE WRITTEN   June, 2015
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Predict zone loads at a controlled node

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		//////////// hoisted into namespace ////////////////////////////////////////////////
		// static bool OneTimeFlag( true );  // one time setup flag // LoadPredictionRoomAirModelAirflowNetworkOneTimeFlag
		////////////////////////////////////////////////////////////////////////////////////
		int RAFNNum;

		// FLOW:
		if ( LoadPredictionRoomAirModelAirflowNetworkOneTimeFlag ) {
			RAFN.allocate( NumOfRoomAirflowNetControl );
			LoadPredictionRoomAirModelAirflowNetworkOneTimeFlag = false;
		}

		RAFNNum = RoomAirflowNetworkZoneInfo( ZoneNum ).RAFNNum;

		if ( RAFNNum == 0 ) {
			ShowFatalError( "LoadPredictionRoomAirModelAirflowNetwork: Zone is not defined in the RoomAirModelAirflowNetwork model =" + Zone( ZoneNum ).Name );
		}
		auto & thisRAFN( RAFN( RAFNNum ) );
		thisRAFN.ZoneNum = ZoneNum;

		thisRAFN.InitRoomAirModelAirflowNetwork( RoomAirNode );

	}  //LoadPredictionRoomAirModelAirflowNetwork

	//****************************************************

	void
	RAFNData::InitRoomAirModelAirflowNetwork( int const RoomAirNode ) // index number for the specified zone
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         B. Griffith
		//       DATE WRITTEN   November 2009
		//       MODIFIED       Lixing Gu, Aug. 2015 for v8.4 replease
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Perform one-time checking and term calculations

		// METHODOLOGY EMPLOYED:
		// na

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
		using InputProcessor::SameString;
		using General::RoundSigDigits;

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		//////////// hoisted into namespace ////////////////////////////////////////////////
		// static bool MyOneTimeFlag( true );  // one time setup flag // InitRoomAirModelAirflowNetworkOneTimeFlag
		// static bool MyOneTimeFlagConf( true ); // one time setup flag for zone configuration // InitRoomAirModelAirflowNetworkOneTimeFlagConf
		// static bool MyEnvrnFlag( true ); // one time setup flag for zone configuration // InitRoomAirModelAirflowNetworkEnvrnFlag
		////////////////////////////////////////////////////////////////////////////////////
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
		int MaxNodeNum;
		Array1D_bool NodeFound; // True if a node is found.
		int MaxEquipNum;
		Array1D_bool EquipFound;
		int ISum;
		bool ErrorsFound;
		int I;
		Array1D< Real64 > SupplyFrac;
		Array1D< Real64 > ReturnFrac;

		if ( InitRoomAirModelAirflowNetworkOneTimeFlag ) {  // then do one - time setup inits

			// loop over all zones with RoomAirflowNetwork model
			for ( LoopZone = 1; LoopZone <= NumOfZones; ++LoopZone ) {
				if ( !RoomAirflowNetworkZoneInfo( LoopZone ).IsUsed ) continue;
				NumSurfs = Zone( LoopZone ).SurfaceLast - Zone( LoopZone ).SurfaceFirst + 1;
				for ( LoopAirNode = 1; LoopAirNode <= RoomAirflowNetworkZoneInfo( LoopZone ).NumOfAirNodes; ++LoopAirNode ) { //loop over all the modeled room air nodes
					// calculate volume of air in node's control volume
					RoomAirflowNetworkZoneInfo( LoopZone ).Node( LoopAirNode ).AirVolume = Zone( LoopZone ).Volume * RoomAirflowNetworkZoneInfo( LoopZone ).Node( LoopAirNode ).ZoneVolumeFraction;

					SetupOutputVariable( "RoomAirflowNetwork Node NonAirSystemResponse [W]", RoomAirflowNetworkZoneInfo( LoopZone ).Node( LoopAirNode ).NonAirSystemResponse, "HVAC", "Average", RoomAirflowNetworkZoneInfo( LoopZone ).Node( LoopAirNode ).Name );
					SetupOutputVariable( "RoomAirflowNetwork Node SysDepZoneLoadsLagged [W]", RoomAirflowNetworkZoneInfo( LoopZone ).Node( LoopAirNode ).SysDepZoneLoadsLagged, "HVAC", "Average", RoomAirflowNetworkZoneInfo( LoopZone ).Node( LoopAirNode ).Name );
					SetupOutputVariable( "RoomAirflowNetwork Node SumIntSensibleGain [W]", RoomAirflowNetworkZoneInfo( LoopZone ).Node( LoopAirNode ).SumIntSensibleGain, "HVAC", "Average", RoomAirflowNetworkZoneInfo( LoopZone ).Node( LoopAirNode ).Name );
					SetupOutputVariable( "RoomAirflowNetwork Node SumIntLatentGain [W]", RoomAirflowNetworkZoneInfo( LoopZone ).Node( LoopAirNode ).SumIntLatentGain, "HVAC", "Average", RoomAirflowNetworkZoneInfo( LoopZone ).Node( LoopAirNode ).Name );
				}
			}
			InitRoomAirModelAirflowNetworkOneTimeFlag = false;
		}

		if ( InitRoomAirModelAirflowNetworkOneTimeFlagConf ) { //then do one - time setup inits
			if ( allocated( ZoneEquipConfig ) && allocated( ZoneEquipList ) ) {
				MaxNodeNum = 0;
				MaxEquipNum = 0;
				ErrorsFound = false;
				for ( LoopZone = 1; LoopZone <= NumOfZones; ++LoopZone ) {
					if ( !Zone( LoopZone ).IsControlled ) continue;
					MaxEquipNum = max( MaxEquipNum, ZoneEquipList( LoopZone ).NumOfEquipTypes );
					MaxNodeNum = max( MaxNodeNum, ZoneEquipConfig( LoopZone ).NumInletNodes );
				}
				if ( MaxNodeNum > 0 ) {
					NodeFound.allocate( MaxNodeNum );
					NodeFound = false;
				}
				if ( MaxEquipNum > 0 ) {
					EquipFound.allocate( MaxEquipNum );
					SupplyFrac.allocate( MaxEquipNum );
					ReturnFrac.allocate( MaxEquipNum );
					EquipFound = false;
					SupplyFrac = 0.0;
					ReturnFrac = 0.0;
				}

				// loop over all zones with RoomAirflowNetwork model
				for ( LoopZone = 1; LoopZone <= NumOfZones; ++LoopZone ) {
					if ( !Zone( LoopZone ).IsControlled ) continue;
					if ( !RoomAirflowNetworkZoneInfo( LoopZone ).IsUsed ) continue;
					// find actualZoneID in ZoneEquipConfig
					for ( IdZone = 1; IdZone <= NumOfZones; ++IdZone ) {
						if ( ZoneEquipConfig( IdZone ).ActualZoneNum == LoopZone ) {
							RoomAirflowNetworkZoneInfo( LoopZone ).ActualZoneID = IdZone;
							break;
						}
					}
					SupplyFrac = 0.0;
					ReturnFrac = 0.0;
					NodeFound = false;

					// find supply air node number
					for ( LoopAirNode = 1; LoopAirNode <= RoomAirflowNetworkZoneInfo( LoopZone ).NumOfAirNodes; ++LoopAirNode ) { //loop over all the modeled room air nodes
						for ( EquipLoop = 1; EquipLoop <= RoomAirflowNetworkZoneInfo( LoopZone ).Node( LoopAirNode ).NumHVACs; ++EquipLoop ) { //loop over all the equip for a single room air node
							// Check zone equipment name
							for ( I = 1; I <= ZoneEquipList( LoopZone ).NumOfEquipTypes; ++I ) { //loop over all equip types
								if ( SameString( ZoneEquipList( LoopZone ).EquipName( I ), RoomAirflowNetworkZoneInfo( LoopZone ).Node( LoopAirNode ).HVAC( EquipLoop ).Name ) ) {
									if ( RoomAirflowNetworkZoneInfo( LoopZone ).Node( LoopAirNode ).HVAC( EquipLoop ).EquipConfigIndex == 0 )
										RoomAirflowNetworkZoneInfo( LoopZone ).Node( LoopAirNode ).HVAC( EquipLoop ).EquipConfigIndex = I;
									EquipFound( I ) = true;
									SupplyFrac( I ) = SupplyFrac( I ) + RoomAirflowNetworkZoneInfo( LoopZone ).Node( LoopAirNode ).HVAC( EquipLoop ).SupplyFraction;
									ReturnFrac( I ) = ReturnFrac( I ) + RoomAirflowNetworkZoneInfo( LoopZone ).Node( LoopAirNode ).HVAC( EquipLoop ).ReturnFraction;
								}
							}
							for ( IdNode = 1; IdNode <= NumOfNodes; ++IdNode ) { //loop over all nodes to find supply node ID
								if ( SameString( NodeID( IdNode ), RoomAirflowNetworkZoneInfo( LoopZone ).Node( LoopAirNode ).HVAC( EquipLoop ).SupplyNodeName ) ) {
									RoomAirflowNetworkZoneInfo( LoopZone ).Node( LoopAirNode ).HVAC( EquipLoop ).SupNodeNum = IdNode;
									break;
								}
							}
							// Verify inlet nodes
							for ( NodeNum = 1; NodeNum <= ZoneEquipConfig( LoopZone ).NumInletNodes; ++NodeNum ) { //loop over all supply inlet nodes in a single zone
								// !Get node conditions
								if ( ZoneEquipConfig( LoopZone ).InletNode( NodeNum ) == IdNode ) {
									NodeFound( NodeNum ) = true;
								}
							}

							if ( RoomAirflowNetworkZoneInfo( LoopZone ).Node( LoopAirNode ).HVAC( EquipLoop ).SupNodeNum > 0 && SameString( RoomAirflowNetworkZoneInfo( LoopZone ).Node( LoopAirNode ).HVAC( EquipLoop ).ReturnNodeName, "" ) ) {
								RoomAirflowNetworkZoneInfo( LoopZone ).Node( LoopAirNode ).HVAC( EquipLoop ).ReturnNodeName = NodeID( ZoneEquipConfig( LoopZone ).ReturnAirNode ); // Zone return node
							}

							for ( IdNode = 1; IdNode <= NumOfNodes; ++IdNode ) { //loop over all nodes to find return node ID
								if ( SameString( NodeID( IdNode ), RoomAirflowNetworkZoneInfo( LoopZone ).Node( LoopAirNode ).HVAC( EquipLoop ).ReturnNodeName ) ) {
									RoomAirflowNetworkZoneInfo( LoopZone ).Node( LoopAirNode ).HVAC( EquipLoop ).RetNodeNum = IdNode;
									break;
								}
							}
							SetupOutputVariable( "RoomAirflowNetwork Node HVAC Supply Fraction []", RoomAirflowNetworkZoneInfo( LoopZone ).Node( LoopAirNode ).HVAC( EquipLoop ).SupplyFraction, "HVAC", "Average", RoomAirflowNetworkZoneInfo(LoopZone).Node(LoopAirNode).HVAC(EquipLoop).Name);
							SetupOutputVariable("RoomAirflowNetwork Node HVAC Return Fraction []", RoomAirflowNetworkZoneInfo(LoopZone).Node(LoopAirNode).HVAC(EquipLoop).ReturnFraction, "HVAC", "Average", RoomAirflowNetworkZoneInfo(LoopZone).Node(LoopAirNode).HVAC(EquipLoop).Name);
						}
					}
					// Count node with.TRUE.
					ISum = 0;
					for ( NodeNum = 1; NodeNum <= MaxNodeNum; ++NodeNum ) { //loop over all supply inlet nodes in a single zone
						if ( NodeFound( NodeNum ) ) ISum = ISum + 1;
					}
                    // Provide error messages with incorrect supplu node inputs
					if ( ISum != ZoneEquipConfig( LoopZone ).NumInletNodes ) {
						if ( ISum > ZoneEquipConfig( LoopZone ).NumInletNodes ) {
							ShowSevereError( "GetRoomAirflowNetworkData: The number of equipment listed in RoomAirflowNetwork:Node:HVACEquipment objects" );
							ShowContinueError( "is greater than the number of zone configuration inlet nodes in " + Zone( LoopZone ).Name );
							ShowContinueError( "Please check inputs of both objects." );
							ErrorsFound = true;
						} else {
							ShowSevereError( "GetRoomAirflowNetworkData: The number of equipment listed in RoomAirflowNetwork:Node:HVACEquipment objects" );
							ShowContinueError( "is less than the number of zone configuration inlet nodes in " + Zone( LoopZone ).Name );
							ShowContinueError( "Please check inputs of both objects." );
							ErrorsFound = true;
						}
					}

					// Check equipment names to ensure they are used in RoomAirflowNetwork : Node : HVACEquipment objects
					for ( I = 1; I <= ZoneEquipList( LoopZone ).NumOfEquipTypes; ++I ) { //loop over all equip types
						if ( !EquipFound( I ) ) {
							ShowSevereError( "GetRoomAirflowNetworkData: The equipment listed in ZoneEquipList is not found in the lsit of RoomAir:Node:AirflowNetwork:HVACEquipment objects =" );
							ShowContinueError( ZoneEquipList( LoopZone ).EquipName( I ) + ". Please check inputs of both objects." );
							ErrorsFound = true;
						}
					}

					// Check fraction to ensure sum = 1.0 for every equipment
					for ( I = 1; I <= ZoneEquipList( LoopZone ).NumOfEquipTypes; ++I ) { //loop over all equip types
						if ( abs( SupplyFrac( I ) - 1.0 ) > 0.001 ) {
							ShowSevereError( "GetRoomAirflowNetworkData: Invalid, zone supply fractions do not sum to 1.0" );
							ShowContinueError( "Entered in " + ZoneEquipList( LoopZone ).EquipName( I ) + " defined in RoomAir:Node:AirflowNetwork:HVACEquipment" );
							ShowContinueError( "The Fraction of supply fraction values across all the roomair nodes in a zone needs to sum to 1.0." );
							ShowContinueError( "The sum of fractions entered = " + RoundSigDigits( SupplyFrac( I ), 3 ) );
							ErrorsFound = true;
						}
						if ( abs( ReturnFrac( I ) - 1.0 ) > 0.001 ) {
							ShowSevereError( "GetRoomAirflowNetworkData: Invalid, zone return fractions do not sum to 1.0" );
							ShowContinueError( "Entered in " + ZoneEquipList( LoopZone ).EquipName( I ) + " defined in RoomAir:Node:AirflowNetwork:HVACEquipment" );
							ShowContinueError( "The Fraction of return fraction values across all the roomair nodes in a zone needs to sum to 1.0." );
							ShowContinueError( "The sum of fractions entered = " + RoundSigDigits( ReturnFrac( I ), 3 ) );
							ErrorsFound = true;
						}
					}

				}
				InitRoomAirModelAirflowNetworkOneTimeFlagConf = false;
				if ( allocated( NodeFound ) ) NodeFound.deallocate( );
				if ( ErrorsFound ) {
					ShowFatalError( "GetRoomAirflowNetworkData: Errors found getting air model input.  Program terminates." );
				}
			}
		} //End of InitRoomAirModelAirflowNetworkOneTimeFlagConf

		if ( BeginEnvrnFlag && InitRoomAirModelAirflowNetworkEnvrnFlag ) {
			for ( LoopZone = 1; LoopZone <= NumOfZones; ++LoopZone ) {
				if ( !RoomAirflowNetworkZoneInfo( LoopZone ).IsUsed ) continue;
				for ( LoopAirNode = 1; LoopAirNode <= RoomAirflowNetworkZoneInfo( LoopZone ).NumOfAirNodes; ++LoopAirNode ) {  // loop over all the modeled room air nodes
					RoomAirflowNetworkZoneInfo( LoopZone ).Node( LoopAirNode ).AirTemp = 23.0;
					RoomAirflowNetworkZoneInfo( LoopZone ).Node( LoopAirNode ).AirTempX1 = 23.0;
					RoomAirflowNetworkZoneInfo( LoopZone ).Node( LoopAirNode ).AirTempX2 = 23.0;
					RoomAirflowNetworkZoneInfo( LoopZone ).Node( LoopAirNode ).AirTempX3 = 23.0;
					RoomAirflowNetworkZoneInfo( LoopZone ).Node( LoopAirNode ).AirTempX4 = 23.0;
					RoomAirflowNetworkZoneInfo( LoopZone ).Node( LoopAirNode ).AirTempDSX1 = 23.0;
					RoomAirflowNetworkZoneInfo( LoopZone ).Node( LoopAirNode ).AirTempDSX2 = 23.0;
					RoomAirflowNetworkZoneInfo( LoopZone ).Node( LoopAirNode ).AirTempDSX3 = 23.0;
					RoomAirflowNetworkZoneInfo( LoopZone ).Node( LoopAirNode ).AirTempDSX4 = 23.0;
					RoomAirflowNetworkZoneInfo( LoopZone ).Node( LoopAirNode ).AirTempT1 = 23.0;
					RoomAirflowNetworkZoneInfo( LoopZone ).Node( LoopAirNode ).AirTempTMX = 23.0;
					RoomAirflowNetworkZoneInfo( LoopZone ).Node( LoopAirNode ).AirTempTM2 = 23.0;

					RoomAirflowNetworkZoneInfo( LoopZone ).Node( LoopAirNode ).HumRat = 0.0;
					RoomAirflowNetworkZoneInfo( LoopZone ).Node( LoopAirNode ).HumRatX1 = 0.0;
					RoomAirflowNetworkZoneInfo( LoopZone ).Node( LoopAirNode ).HumRatX2 = 0.0;
					RoomAirflowNetworkZoneInfo( LoopZone ).Node( LoopAirNode ).HumRatX3 = 0.0;
					RoomAirflowNetworkZoneInfo( LoopZone ).Node( LoopAirNode ).HumRatX4 = 0.0;
					RoomAirflowNetworkZoneInfo( LoopZone ).Node( LoopAirNode ).HumRatDSX1 = 0.0;
					RoomAirflowNetworkZoneInfo( LoopZone ).Node( LoopAirNode ).HumRatDSX2 = 0.0;
					RoomAirflowNetworkZoneInfo( LoopZone ).Node( LoopAirNode ).HumRatDSX3 = 0.0;
					RoomAirflowNetworkZoneInfo( LoopZone ).Node( LoopAirNode ).HumRatDSX4 = 0.0;
					RoomAirflowNetworkZoneInfo( LoopZone ).Node( LoopAirNode ).HumRatW1 = 0.0;
					RoomAirflowNetworkZoneInfo( LoopZone ).Node( LoopAirNode ).HumRatWMX = 0.0;
					RoomAirflowNetworkZoneInfo( LoopZone ).Node( LoopAirNode ).HumRatWM2 = 0.0;

					RoomAirflowNetworkZoneInfo( LoopZone ).Node( LoopAirNode ).SysDepZoneLoadsLagged = 0.0;
					RoomAirflowNetworkZoneInfo( LoopZone ).Node( LoopAirNode ).SysDepZoneLoadsLaggedOld = 0.0;
				}
			}
			InitRoomAirModelAirflowNetworkEnvrnFlag = false;
		}
		if ( !BeginEnvrnFlag ) {
			InitRoomAirModelAirflowNetworkEnvrnFlag = true;
		}

		// reuse code in ZoneTempPredictorCorrector for sensible components.
		CalcNodeSums( RoomAirNode );

		SumNonAirSystemResponseForNode( RoomAirNode );

		// latent gains.
		auto & ThisRAFNNode( RoomAirflowNetworkZoneInfo( ZoneNum ).Node( RoomAirNode ) );

		if ( allocated( ThisRAFNNode.SurfMask ) ) {
			CalcSurfaceMoistureSums( RoomAirNode, ThisRAFNNode.SumHmAW, ThisRAFNNode.SumHmARa, ThisRAFNNode.SumHmARaW, ThisRAFNNode.SurfMask );
		}

		// prepare AirflowNetwor flow rates and temperatures
		SumLinkMCp = 0.0;
		SumLinkMCpT = 0.0;
		SumLinkM = 0.0;
		SumLinkMW = 0.0;

		NodeNum = ThisRAFNNode.AirflowNetworkNodeID;
		if (NodeNum > 0) {
			for ( linkNum = 1; linkNum <= ThisRAFNNode.NumOfAirflowLinks; ++linkNum ) {
				Link = ThisRAFNNode.Link( linkNum ).AirflowNetworkLinkSimuID;
				if (AirflowNetworkLinkageData(Link).NodeNums(1) == NodeNum) {  // incoming flow
					NodeIn = AirflowNetworkLinkageData(Link).NodeNums(2);
					ThisRAFNNode.Link( linkNum ).TempIn = AirflowNetworkNodeSimu( NodeIn ).TZ;
					ThisRAFNNode.Link( linkNum ).HumRatIn = AirflowNetworkNodeSimu( NodeIn ).WZ;
					ThisRAFNNode.Link( linkNum ).MdotIn = AirflowNetworkLinkSimu( Link ).FLOW2;
				}
				if (AirflowNetworkLinkageData(Link).NodeNums(2) == NodeNum) { // outgoing flow
					NodeIn = AirflowNetworkLinkageData(Link).NodeNums(1);
					ThisRAFNNode.Link( linkNum ).TempIn = AirflowNetworkNodeSimu( NodeIn ).TZ;
					ThisRAFNNode.Link( linkNum ).HumRatIn = AirflowNetworkNodeSimu( NodeIn ).WZ;
					ThisRAFNNode.Link( linkNum ).MdotIn = AirflowNetworkLinkSimu( Link ).FLOW;
				}
			}

			for ( linkNum = 1; linkNum <= ThisRAFNNode.NumOfAirflowLinks; ++linkNum ) {
				LinkInTemp = ThisRAFNNode.Link( linkNum ).TempIn;
				LinkInHumRat = ThisRAFNNode.Link( linkNum ).HumRatIn;
				LinkInMdot = ThisRAFNNode.Link( linkNum ).MdotIn;
				CpAir = PsyCpAirFnWTdb( LinkInHumRat, LinkInTemp );
				SumLinkMCp = SumLinkMCp + CpAir * LinkInMdot;
				SumLinkMCpT = SumLinkMCpT + CpAir * LinkInMdot * LinkInTemp;
				SumLinkM = SumLinkM + LinkInMdot;
				SumLinkMW = SumLinkMW + LinkInMdot * LinkInHumRat;
			}
		}

		ThisRAFNNode.SumLinkMCp = SumLinkMCp;
		ThisRAFNNode.SumLinkMCpT = SumLinkMCpT;
		ThisRAFNNode.SumLinkM = SumLinkM;
		ThisRAFNNode.SumLinkMW = SumLinkMW;
		ThisRAFNNode.SysDepZoneLoadsLagged = ThisRAFNNode.SysDepZoneLoadsLaggedOld;

		ThisRAFNNode.RhoAir = PsyRhoAirFnPbTdbW( OutBaroPress, ThisRAFNNode.AirTemp, ThisRAFNNode.HumRat, "InitRoomAirModelAirflowNetwork" );

		ThisRAFNNode.CpAir = PsyCpAirFnWTdb( ThisRAFNNode.HumRat, ThisRAFNNode.AirTemp );

	} // InitRoomAirModelAirflowNetwork

	//*****************************************************************************************

	void
	RAFNData::CalcRoomAirModelAirflowNetwork( int const RoomAirNode ) // index number for the specified zone and node
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Brent Griffith
		//       DATE WRITTEN   November 2009
		//       MODIFIED       Lixing Gu, Aug. 2015 for v8.4 replease
		//       RE-ENGINEERED

		// PURPOSE OF THIS SUBROUTINE:
		// calculate new values for temperature and humidity ratio for room air node

		// METHODOLOGY EMPLOYED:
		// take terms(updated in init routine) and use classic air balance equations
		// solved for state variables. Store results in structure.

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
		using DataEnvironment::OutBaroPress;
		using Psychrometrics::PsyRhFnTdbWPb;

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
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
		Real64 AirTempT1;
		Real64 HumRatW1;

		auto & ThisRAFNNode( RoomAirflowNetworkZoneInfo( ZoneNum ).Node( RoomAirNode ) );

		if (UseZoneTimeStepHistory) {
			NodeTempX1 = ThisRAFNNode.AirTempX1;
			NodeTempX2 = ThisRAFNNode.AirTempX2;
			NodeTempX3 = ThisRAFNNode.AirTempX3;

			NodeHumRatX1 = ThisRAFNNode.HumRatX1;
			NodeHumRatX2 = ThisRAFNNode.HumRatX2;
			NodeHumRatX3 = ThisRAFNNode.HumRatX3;
		}
		else {  // use down - stepped history
			NodeTempX1 = ThisRAFNNode.AirTempDSX1;
			NodeTempX2 = ThisRAFNNode.AirTempDSX2;
			NodeTempX3 = ThisRAFNNode.AirTempDSX3;

			NodeHumRatX1 = ThisRAFNNode.HumRatDSX1;
			NodeHumRatX2 = ThisRAFNNode.HumRatDSX2;
			NodeHumRatX3 = ThisRAFNNode.HumRatDSX3;
		}

		if ( ZoneAirSolutionAlgo != Use3rdOrder ) {
			AirTempT1 = ThisRAFNNode.AirTempT1;
			HumRatW1 = ThisRAFNNode.HumRatW1;
		}
		// solve for node drybulb temperature
		TempDepCoef = ThisRAFNNode.SumHA + ThisRAFNNode.SumLinkMCp + ThisRAFNNode.SumSysMCp;
		TempIndCoef = ThisRAFNNode.SumIntSensibleGain + ThisRAFNNode.SumHATsurf - ThisRAFNNode.SumHATref + ThisRAFNNode.SumLinkMCpT + ThisRAFNNode.SumSysMCpT + ThisRAFNNode.NonAirSystemResponse + ThisRAFNNode.SysDepZoneLoadsLagged;
		AirCap = ThisRAFNNode.AirVolume * ZoneVolCapMultpSens * ThisRAFNNode.RhoAir * ThisRAFNNode.CpAir / ( TimeStepSys*SecInHour );

		if ( ZoneAirSolutionAlgo == UseAnalyticalSolution ) {
			if ( TempDepCoef == 0.0 ) { // B=0
				TempTmp = AirTempT1 + TempIndCoef / AirCap;
			} else {
				TempTmp = ( AirTempT1 - TempIndCoef / TempDepCoef ) * std::exp( min( 700.0, -TempDepCoef / AirCap ) ) + TempIndCoef / TempDepCoef;
			}
		} else if ( ZoneAirSolutionAlgo == UseEulerMethod ) {
			TempTmp = ( AirCap * AirTempT1 + TempIndCoef ) / ( AirCap + TempDepCoef );
		} else {
			TempTmp = ( TempIndCoef + AirCap*( 3.0*NodeTempX1 - ( 3.0 / 2.0 )*NodeTempX2 + ( 1.0 / 3.0 )*NodeTempX3 ) )
				/ ( ( 11.0 / 6.0 ) * AirCap + TempDepCoef );
		}

		ThisRAFNNode.AirTemp = TempTmp;

		// solve for node humidity ratio using 3 algorithms
		H2OHtOfVap = PsyHgAirFnWTdb( ThisRAFNNode.HumRat, ThisRAFNNode.AirTemp );
		A = ThisRAFNNode.SumLinkM + ThisRAFNNode.SumHmARa + ThisRAFNNode.SumSysM;
		B = ( ThisRAFNNode.SumIntLatentGain / H2OHtOfVap ) + ThisRAFNNode.SumSysMW + ThisRAFNNode.SumLinkMW + ThisRAFNNode.SumHmARaW;
		C = ThisRAFNNode.RhoAir * ThisRAFNNode.AirVolume * ZoneVolCapMultpMoist / ( SecInHour * TimeStepSys );

		// Exact solution
		if ( ZoneAirSolutionAlgo == UseAnalyticalSolution ) {
			if ( A == 0.0 ) { // B=0
				HumRatTmp = HumRatW1 + B / C;
			} else {
				HumRatTmp = ( HumRatW1 - B / A ) * std::exp( min( 700., -A / C ) ) + B / A;
			}
		} else if ( ZoneAirSolutionAlgo == UseEulerMethod ) {
			HumRatTmp = ( C * HumRatW1 + B ) / ( C + A );
		} else {
			HumRatTmp = ( B + C*( 3.0*NodeHumRatX1 - ( 3.0 / 2.0 )*NodeHumRatX2 + ( 1.0 / 3.0 )*NodeHumRatX3 ) ) / ( ( 11.0 / 6.0 )*C + A );
		}

		ThisRAFNNode.HumRat = HumRatTmp;

		ThisRAFNNode.AirCap = AirCap;
		ThisRAFNNode.AirHumRat = C;

		ThisRAFNNode.RelHumidity = PsyRhFnTdbWPb( TempTmp, HumRatTmp, OutBaroPress, "CalcRoomAirModelAirflowNetwork" ) * 100.0;

	} // CalcRoomAirModelAirflowNetwork

	void
	RAFNData::UpdateRoomAirModelAirflowNetwork()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         B Griffith
		//       DATE WRITTEN   November 2009
		//       MODIFIED       Lixing Gu, Aug. 2015 for v8.4 replease
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// update variables

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataHeatBalFanSys::TempTstatAir;
		using DataHeatBalance::Zone;
		using DataZoneEquipment::ZoneEquipConfig;
		using DataZoneEquipment::ZoneEquipList;
		using DataLoopNode::Node;
		using DataGlobals::ZoneSizingCalc;

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int AirNodeNum; // nested node structure index
		int I;
		int LoopAirNode;
		int EquipLoop;
		Real64 NodeMass;
		Real64 SumMass;
		Real64 SumMassT;
		Real64 SumMassW;
		int RetNodeNum;

		auto & ThisRAFNZone( RoomAirflowNetworkZoneInfo( ZoneNum ) );

		if ( !ThisRAFNZone.IsUsed ) return;

		if ( !ZoneSizingCalc ) SumSystemDepResponseForNode();

		AirNodeNum = RoomAirflowNetworkZoneInfo(ZoneNum).ControlAirNodeID;

		// Update return node conditions
		for ( I = 1; I <= ZoneEquipList( ZoneNum ).NumOfEquipTypes; ++I ) { //loop over all equip types
			SumMass = 0.0;
			SumMassT = 0.0;
			SumMassW = 0.0;
			for ( LoopAirNode = 1; LoopAirNode <= ThisRAFNZone.NumOfAirNodes; ++LoopAirNode ) { //loop over all the modeled room air nodes
				for ( EquipLoop = 1; EquipLoop <= ThisRAFNZone.Node( LoopAirNode ).NumHVACs; ++EquipLoop ) { //loop over all the equip for a single room air node
					if ( ThisRAFNZone.Node( LoopAirNode ).HVAC( EquipLoop ).EquipConfigIndex == I ) {
						if ( ThisRAFNZone.Node( LoopAirNode ).HVAC( EquipLoop ).SupNodeNum > 0 && ThisRAFNZone.Node( LoopAirNode ).HVAC( EquipLoop ).RetNodeNum > 0 ) {
							NodeMass = Node( ThisRAFNZone.Node( LoopAirNode ).HVAC( EquipLoop ).SupNodeNum ).MassFlowRate * ThisRAFNZone.Node( LoopAirNode ).HVAC( EquipLoop ).ReturnFraction;
							SumMass += NodeMass;
							SumMassT += NodeMass * ThisRAFNZone.Node( LoopAirNode ).AirTemp;
							SumMassW += NodeMass * ThisRAFNZone.Node( LoopAirNode ).HumRat;
							RetNodeNum = ThisRAFNZone.Node( LoopAirNode ).HVAC( EquipLoop ).RetNodeNum;
						}
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
	RAFNData::CalcNodeSums(
		int const RoomAirNodeNum
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         B Griffith
		//       DATE WRITTEN   August 2009
		//       MODIFIED       Lixing Gu, Aug. 2015 for v8.4 replease
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

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// USE STATEMENTS:
		using DataSurfaces::Surface;
		using DataSurfaces::SurfaceClass_Window;
		using DataSurfaces::SurfaceWindow;
		using DataHeatBalance::Zone;
		using DataLoopNode::Node;
		using DataZoneEquipment::ZoneEquipConfig;
		using ZonePlenum::ZoneRetPlenCond;
		using ZonePlenum::ZoneSupPlenCond;
		using ZonePlenum::NumZoneReturnPlenums;
		using ZonePlenum::NumZoneSupplyPlenums;
		using DataDefineEquip::AirDistUnit;
		using DataHeatBalFanSys::ZoneAirHumRat;
		using DataAirflowNetwork::AirflowNetworkZoneExhaustFan;
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
		SumInternalConvectionGainsByIndices( ZoneNum, RoomAirflowNetworkZoneInfo( ZoneNum ).Node( RoomAirNodeNum ).IntGainsDeviceIndices,
			RoomAirflowNetworkZoneInfo( ZoneNum ).Node( RoomAirNodeNum ).IntGainsFractions, RoomAirflowNetworkZoneInfo( ZoneNum ).Node( RoomAirNodeNum ).SumIntSensibleGain );

		SumInternalLatentGainsByIndices( ZoneNum, RoomAirflowNetworkZoneInfo( ZoneNum ).Node( RoomAirNodeNum ).IntGainsDeviceIndices,
			RoomAirflowNetworkZoneInfo( ZoneNum ).Node( RoomAirNodeNum ).IntGainsFractions, RoomAirflowNetworkZoneInfo( ZoneNum ).Node( RoomAirNodeNum ).SumIntLatentGain );
		// Add heat to return air if zonal system(no return air) or cycling system(return air frequently very low or zero)
		if ( Zone( ZoneNum ).NoHeatToReturnAir ) {
			// *******************************************
			SumReturnAirConvectionGainsByIndices( ZoneNum, RoomAirflowNetworkZoneInfo( ZoneNum ).Node( RoomAirNodeNum ).IntGainsDeviceIndices,
				RoomAirflowNetworkZoneInfo( ZoneNum ).Node( RoomAirNodeNum ).IntGainsFractions, SumIntGain );
			RoomAirflowNetworkZoneInfo( ZoneNum ).Node( RoomAirNodeNum ).SumIntSensibleGain += SumIntGain;
		}

		// Check to see if this is a controlled zone

		ControlledZoneAirFlag = false;
		for ( ZoneEquipConfigNum = 1; ZoneEquipConfigNum <= NumOfZones; ++ZoneEquipConfigNum ) {
			if ( !Zone( ZoneEquipConfigNum ).IsControlled ) continue;
			if ( ZoneEquipConfig( ZoneEquipConfigNum ).ActualZoneNum != ZoneNum ) continue;
			ControlledZoneAirFlag = true;
			break; // sloppy way of finding ZoneEquipConfigNum for later use.
		} // ZoneEquipConfigNum

		// Check to see if this is a plenum zone
		ZoneRetPlenumAirFlag = false;
		for ( ZoneRetPlenumNum = 1; ZoneRetPlenumNum <= NumZoneReturnPlenums; ++ZoneRetPlenumNum ) {
			if ( ZoneRetPlenCond( ZoneRetPlenumNum ).ActualZoneNum != ZoneNum ) continue;
			ZoneRetPlenumAirFlag = true;
			break;
		} // ZoneRetPlenumNum
		ZoneSupPlenumAirFlag = false;
		for ( ZoneSupPlenumNum = 1; ZoneSupPlenumNum <= NumZoneSupplyPlenums; ++ZoneSupPlenumNum ) {
			if ( ZoneSupPlenCond( ZoneSupPlenumNum ).ActualZoneNum != ZoneNum ) continue;
			ZoneSupPlenumAirFlag = true;
			break;
		} // ZoneSupPlenumNum

		// Plenum and controlled zones have a different set of inlet nodes which must be calculated.
		if ( ControlledZoneAirFlag ) {
			for ( NodeNum = 1; NodeNum <= ZoneEquipConfig( ZoneEquipConfigNum ).NumInletNodes; ++NodeNum ) {
				// Get node conditions
				// this next block is of interest to irratic system loads... maybe nodes are not accurate at time of call ?
				// how can we tell ? predict step must be lagged ? correct step, systems have run.
				for ( EquipLoop = 1; EquipLoop <= RoomAirflowNetworkZoneInfo( ZoneNum ).Node( RoomAirNodeNum ).NumHVACs; ++EquipLoop ) {
					if ( RoomAirflowNetworkZoneInfo( ZoneNum ).Node( RoomAirNodeNum ).HVAC( EquipLoop ).SupNodeNum == ZoneEquipConfig( ZoneEquipConfigNum ).InletNode( NodeNum ) ) {
						NodeTemp = Node( ZoneEquipConfig( ZoneEquipConfigNum ).InletNode( NodeNum ) ).Temp;
						NodeW = Node( ZoneEquipConfig( ZoneEquipConfigNum ).InletNode( NodeNum ) ).HumRat;
						MassFlowRate = Node( ZoneEquipConfig( ZoneEquipConfigNum ).InletNode( NodeNum ) ).MassFlowRate * RoomAirflowNetworkZoneInfo( ZoneNum ).Node( RoomAirNodeNum ).HVAC( EquipLoop ).SupplyFraction;
						CpAir = PsyCpAirFnWTdb( ZoneAirHumRat( ZoneNum ), NodeTemp );
						SumSysMCp += MassFlowRate * CpAir;
						SumSysMCpT += MassFlowRate * CpAir * NodeTemp;
						SumSysM += MassFlowRate;
						SumSysMW += MassFlowRate * NodeW;
					}
				} // EquipLoop
			} // NodeNum
		}
		else if ( ZoneRetPlenumAirFlag ) {
			for ( NodeNum = 1; NodeNum <= ZoneRetPlenCond( ZoneRetPlenumNum ).NumInletNodes; ++NodeNum ) {
				// Get node conditions
				NodeTemp = Node( ZoneRetPlenCond( ZoneRetPlenumNum ).InletNode( NodeNum ) ).Temp;
				MassFlowRate = Node( ZoneRetPlenCond( ZoneRetPlenumNum ).InletNode( NodeNum ) ).MassFlowRate;
				CpAir = PsyCpAirFnWTdb( ZoneAirHumRat( ZoneNum ), NodeTemp );
				SumSysMCp += MassFlowRate * CpAir;
				SumSysMCpT += MassFlowRate * CpAir * NodeTemp;
			} // NodeNum
			// add in the leaks
			for ( ADUListIndex = 1; ADUListIndex <= ZoneRetPlenCond( ZoneRetPlenumNum ).NumADUs; ++ADUListIndex ) {
				ADUNum = ZoneRetPlenCond( ZoneRetPlenumNum ).ADUIndex( ADUListIndex );
				if ( AirDistUnit( ADUNum ).UpStreamLeak ) {
					ADUInNode = AirDistUnit( ADUNum ).InletNodeNum;
					NodeTemp = Node( ADUInNode ).Temp;
					MassFlowRate = AirDistUnit( ADUNum ).MassFlowRateUpStrLk;
					CpAir = PsyCpAirFnWTdb( ZoneAirHumRat( ZoneNum ), NodeTemp );
					SumSysMCp += MassFlowRate * CpAir;
					SumSysMCpT += MassFlowRate * CpAir * NodeTemp;
				}
				if ( AirDistUnit( ADUNum ).DownStreamLeak ) {
					ADUOutNode = AirDistUnit( ADUNum ).OutletNodeNum;
					NodeTemp = Node( ADUOutNode ).Temp;
					MassFlowRate = AirDistUnit( ADUNum ).MassFlowRateDnStrLk;
					CpAir = PsyCpAirFnWTdb( ZoneAirHumRat( ZoneNum ), NodeTemp );
					SumSysMCp += MassFlowRate * CpAir;
					SumSysMCpT += MassFlowRate * CpAir * NodeTemp;
				}
			} // ADUListIndex
		} else if ( ZoneSupPlenumAirFlag ) {
			// Get node conditions
			NodeTemp = Node( ZoneSupPlenCond( ZoneSupPlenumNum ).InletNode ).Temp;
			MassFlowRate = Node( ZoneSupPlenCond( ZoneSupPlenumNum ).InletNode ).MassFlowRate;
			CpAir = PsyCpAirFnWTdb( ZoneAirHumRat( ZoneNum ), NodeTemp );
			SumSysMCp += MassFlowRate * CpAir;
			SumSysMCpT += MassFlowRate * CpAir * NodeTemp;
		}

		ZoneMult = Zone(ZoneNum).Multiplier * Zone(ZoneNum).ListMultiplier;

		SumSysMCp = SumSysMCp / ZoneMult;
		SumSysMCpT = SumSysMCpT / ZoneMult;
		SumSysM = SumSysM / ZoneMult;
		SumSysMW = SumSysMW / ZoneMult;

		// Sum all surface convection : SumHA, SumHATsurf, SumHATref(and additional contributions to SumIntGain)
		// Modified by Gu to include assigned surfaces only shown in the surface lsit
		if ( !RoomAirflowNetworkZoneInfo( ZoneNum ).Node( RoomAirNodeNum ).HasSurfacesAssigned ) return;

		for ( SurfNum = Zone( ZoneNum ).SurfaceFirst; SurfNum <= Zone( ZoneNum ).SurfaceLast; ++SurfNum ) {

			if ( !Surface( SurfNum ).HeatTransSurf ) continue;  // Skip non - heat transfer surfaces
			if ( RoomAirflowNetworkZoneInfo( ZoneNum ).ControlAirNodeID == RoomAirNodeNum ) {
				Found = false;
				for ( Loop = 1; Loop <= RoomAirflowNetworkZoneInfo( ZoneNum ).NumOfAirNodes; ++Loop ) {
					if ( Loop != RoomAirNodeNum ) {
						if ( RoomAirflowNetworkZoneInfo( ZoneNum ).Node( Loop ).SurfMask( SurfNum - Zone( ZoneNum ).SurfaceFirst + 1 ) ) {
							Found = true;
							break;
						}
					}
				}
				if ( Found ) continue;
			} else {
				if ( !RoomAirflowNetworkZoneInfo( ZoneNum ).Node( RoomAirNodeNum ).SurfMask( SurfNum - Zone( ZoneNum ).SurfaceFirst + 1 ) ) continue;
			}

			HA = 0.0;
			Area = Surface( SurfNum ).Area; // For windows, this is the glazing area

			if ( Surface( SurfNum ).Class == SurfaceClass_Window ) {

				// Add to the convective internal gains
				if ( SurfaceWindow( SurfNum ).ShadingFlag == IntShadeOn || SurfaceWindow( SurfNum ).ShadingFlag == IntBlindOn ) {
					// The shade area covers the area of the glazing plus the area of the dividers.
					Area += SurfaceWindow( SurfNum ).DividerArea;
					SumIntGain += SurfaceWindow( SurfNum ).DividerConduction;
				}

				// Convective heat gain from natural convection in gap between glass and interior shade or blind
				if ( SurfaceWindow( SurfNum ).ShadingFlag == IntShadeOn || SurfaceWindow( SurfNum ).ShadingFlag == IntBlindOn )
					SumIntGain += SurfaceWindow( SurfNum ).ConvHeatFlowNatural;

				// Convective heat gain from airflow window
				if ( SurfaceWindow( SurfNum ).AirflowThisTS > 0.0 ) {
					SumIntGain += SurfaceWindow( SurfNum ).ConvHeatGainToZoneAir;
					if ( Zone( ZoneNum ).NoHeatToReturnAir ) {
						SumIntGain += SurfaceWindow( SurfNum ).RetHeatGainToZoneAir;
						WinHeatGain( SurfNum ) += SurfaceWindow( SurfNum ).RetHeatGainToZoneAir;
						if ( WinHeatGain( SurfNum ) >= 0.0 ) {
							WinHeatGainRep( SurfNum ) = WinHeatGain( SurfNum );
							WinHeatGainRepEnergy( SurfNum ) = WinHeatGainRep( SurfNum ) * TimeStepZone * SecInHour;
						} else {
							WinHeatLossRep( SurfNum ) = -WinHeatGain( SurfNum );
							WinHeatLossRepEnergy( SurfNum ) = WinHeatLossRep( SurfNum ) * TimeStepZone * SecInHour;
						}
					}
				}

				// Add to the surface convection sums
				if ( SurfaceWindow( SurfNum ).FrameArea > 0.0 ) {
					//Window frame contribution
					SumHATsurf += HConvIn( SurfNum ) * SurfaceWindow( SurfNum ).FrameArea
						* ( 1.0 + SurfaceWindow( SurfNum ).ProjCorrFrIn ) * SurfaceWindow( SurfNum ).FrameTempSurfIn;
					HA += HConvIn( SurfNum ) * SurfaceWindow( SurfNum ).FrameArea * ( 1.0 + SurfaceWindow( SurfNum ).ProjCorrFrIn );
				}

				if ( SurfaceWindow( SurfNum ).DividerArea > 0.0 && SurfaceWindow( SurfNum ).ShadingFlag != IntShadeOn && SurfaceWindow( SurfNum ).ShadingFlag != IntBlindOn ) {
					// Window divider contribution(only from shade or blind for window with divider and interior shade or blind)
					SumHATsurf += HConvIn( SurfNum ) * SurfaceWindow( SurfNum ).DividerArea
						* ( 1.0 + 2.0 * SurfaceWindow( SurfNum ).ProjCorrDivIn ) * SurfaceWindow( SurfNum ).DividerTempSurfIn;
					HA += HConvIn( SurfNum ) * SurfaceWindow( SurfNum ).DividerArea * ( 1.0 + 2.0 * SurfaceWindow( SurfNum ).ProjCorrDivIn );
				}

			}  // End of check if window

			HA = HA + HConvIn( SurfNum ) * Area;
			SumHATsurf += HConvIn( SurfNum ) * Area * TempSurfInTmp( SurfNum );

			if ( Surface( SurfNum ).TAirRef == ZoneMeanAirTemp ) {
				// The zone air is the reference temperature(which is to be solved for in CorrectZoneAirTemp).
				RefAirTemp = MAT( ZoneNum );
				SumHA += HA;
			} else if ( Surface( SurfNum ).TAirRef == AdjacentAirTemp ) {
				RefAirTemp = TempEffBulkAir( SurfNum );
				SumHATref += HA * RefAirTemp;
			} else if ( Surface( SurfNum ).TAirRef == ZoneSupplyAirTemp ) {
				// check whether this zone is a controlled zone or not
				if ( !ControlledZoneAirFlag ) {
					ShowFatalError( "Zones must be controlled for Ceiling-Diffuser Convection model. No system serves zone " + Zone( ZoneNum ).Name );
					return;
				}
				// determine supply air temperature as a weighted average of the inlet temperatures.
				RefAirTemp = SumSysMCpT / SumSysMCp;
				SumHATref += HA * RefAirTemp;
			} else {
				RefAirTemp = MAT( ZoneNum );
				SumHA = SumHA + HA;
			}

		} // SurfNum

		// Assemble values
		RoomAirflowNetworkZoneInfo( ZoneNum ).Node( RoomAirNodeNum ).SumHA = SumHA;
		RoomAirflowNetworkZoneInfo( ZoneNum ).Node( RoomAirNodeNum ).SumHATsurf = SumHATsurf;
		RoomAirflowNetworkZoneInfo( ZoneNum ).Node( RoomAirNodeNum ).SumHATref = SumHATref;
		RoomAirflowNetworkZoneInfo( ZoneNum ).Node( RoomAirNodeNum ).SumSysMCp = SumSysMCp;
		RoomAirflowNetworkZoneInfo( ZoneNum ).Node( RoomAirNodeNum ).SumSysMCpT = SumSysMCpT;
		RoomAirflowNetworkZoneInfo( ZoneNum ).Node( RoomAirNodeNum ).SumSysM = SumSysM;
		RoomAirflowNetworkZoneInfo( ZoneNum ).Node( RoomAirNodeNum ).SumSysMW = SumSysMW;

	} // CalcNodeSums

	void
	RAFNData::CalcSurfaceMoistureSums(
		int const RoomAirNode,
		Real64 & SumHmAW,
		Real64 & SumHmARa,
		Real64 & SumHmARaW,
		Array1< bool > const & EP_UNUSED(SurfMask)
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         B Griffith
		//                      derived from P. Biddulph-- HAMT, L. Gu -- EPMD,
		//       DATE WRITTEN   November 2009
		//       MODIFIED       Lixing Gu, Aug. 2015 for v8.4 replease
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Breakout summation of surface moisture interaction terms

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using Psychrometrics::PsyRhoAirFnPbTdbW;
		using Psychrometrics::PsyWFnTdbRhPb;
		using Psychrometrics::PsyRhFnTdbRhov;
		using Psychrometrics::PsyRhFnTdbRhovLBnd0C;
		using DataMoistureBalanceEMPD::MoistEMPDNew;
		using HeatBalanceHAMTManager::UpdateHeatBalHAMT;
		using MoistureBalanceEMPDManager::UpdateMoistureBalanceEMPD;
		using DataEnvironment::OutBaroPress;
		using DataHeatBalSurface::TempSurfInTmp;
		using DataHeatBalFanSys::MAT;

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int SurfNum;
		int Loop;
		Real64 RhoAirZone;
		Real64 Wsurf;
		bool Found;

		SumHmAW = 0.0;
		SumHmARa = 0.0;
		SumHmARaW = 0.0;

		for ( SurfNum = Zone( ZoneNum ).SurfaceFirst; SurfNum <= Zone( ZoneNum ).SurfaceLast; ++SurfNum ) {
			if ( !Surface( SurfNum ).HeatTransSurf ) continue; // Skip non - heat transfer surfaces
			if ( Surface( SurfNum ).Class == SurfaceClass_Window ) continue;

			if ( RoomAirflowNetworkZoneInfo( ZoneNum ).ControlAirNodeID == RoomAirNode ) {
				Found = false;
				for ( Loop = 1; Loop <= RoomAirflowNetworkZoneInfo( ZoneNum ).NumOfAirNodes; ++Loop ) {
					// None - assigned surfaces belong to the zone node
					if ( Loop != RoomAirNode ) {
						if ( RoomAirflowNetworkZoneInfo( ZoneNum ).Node( Loop ).SurfMask( SurfNum - Zone( ZoneNum ).SurfaceFirst + 1 ) ) {
							Found = true;
							break;
						}
					}
				}
				if ( Found ) continue;
			} else {
				if ( !RoomAirflowNetworkZoneInfo( ZoneNum ).Node( RoomAirNode ).SurfMask( SurfNum - Zone( ZoneNum ).SurfaceFirst + 1 ) ) continue;
			}

			if ( Surface( SurfNum ).HeatTransferAlgorithm == HeatTransferModel_HAMT ) {
				UpdateHeatBalHAMT( SurfNum );

				SumHmAW += HMassConvInFD( SurfNum )*Surface( SurfNum ).Area* ( RhoVaporSurfIn( SurfNum ) - RhoVaporAirIn( SurfNum ) );

				RhoAirZone = PsyRhoAirFnPbTdbW( OutBaroPress, MAT( Surface( SurfNum ).Zone ), PsyRhFnTdbRhov( MAT( Surface( SurfNum ).Zone ), RhoVaporAirIn( SurfNum ), "RhoAirZone" ) );

				Wsurf = PsyWFnTdbRhPb( TempSurfInTmp( SurfNum ), PsyRhFnTdbRhov( TempSurfInTmp( SurfNum ), RhoVaporSurfIn( SurfNum ), "Wsurf" ), OutBaroPress );

				SumHmARa = SumHmARa + HMassConvInFD( SurfNum )*Surface( SurfNum ).Area*RhoAirZone;

				SumHmARaW = SumHmARaW + HMassConvInFD( SurfNum )*Surface( SurfNum ).Area*RhoAirZone*Wsurf;
			}

			if ( Surface( SurfNum ).HeatTransferAlgorithm == HeatTransferModel_EMPD ) {

				UpdateMoistureBalanceEMPD( SurfNum );
				RhoVaporSurfIn( SurfNum ) = MoistEMPDNew( SurfNum );

				SumHmAW = SumHmAW + HMassConvInFD( SurfNum )*Surface( SurfNum ).Area* ( RhoVaporSurfIn( SurfNum ) - RhoVaporAirIn( SurfNum ) );
				SumHmARa = SumHmARa + HMassConvInFD( SurfNum )*Surface( SurfNum ).Area* PsyRhoAirFnPbTdbW( OutBaroPress, TempSurfInTmp( SurfNum ),
					PsyWFnTdbRhPb( TempSurfInTmp( SurfNum ), PsyRhFnTdbRhovLBnd0C( TempSurfInTmp( SurfNum ), RhoVaporAirIn( SurfNum ) ), OutBaroPress ) );
				SumHmARaW = SumHmARaW + HMassConvInFD( SurfNum )*Surface( SurfNum ).Area*RhoVaporSurfIn( SurfNum );
			}
		}

	} // CalcSurfaceMoistureSums

	void
	RAFNData::SumNonAirSystemResponseForNode(
		int const RAFNNodeNum
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         B. Griffith
		//       DATE WRITTEN   June 2012
		//       MODIFIED       Lixing Gu, Aug. 2015 for v8.4 replease
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Sum system response from none air systems

		// METHODOLOGY EMPLOYED:
		// na

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
		using DataZoneEquipment::ZoneEquipConfig;
		using DataHVACGlobals::ZoneEquipTypeOf_BaseboardConvectiveWater;
		using DataHVACGlobals::ZoneEquipTypeOf_BaseboardRadiantConvectiveSteam;
		using DataHVACGlobals::ZoneEquipTypeOf_BaseboardRadiantConvectiveWater;
		using DataHVACGlobals::ZoneEquipTypeOf_BaseboardConvectiveElectric;
		using DataHVACGlobals::ZoneEquipTypeOf_RefrigerationChillerSet;
		using DataHVACGlobals::ZoneEquipTypeOf_BaseboardRadiantConvectiveElectric;
		using DataHVACGlobals::ZoneEquipTypeOf_HighTemperatureRadiant;

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int I;
		Real64 SysOutputProvided;
		Real64 LatOutputProvided;

		// TODO
		auto & ThisRAFNNode( RoomAirflowNetworkZoneInfo( ZoneNum ).Node( RAFNNodeNum ) );

		ThisRAFNNode.NonAirSystemResponse = 0.0;

		if ( !allocated( ZoneEquipConfig ) ) return;


		for ( I = 1; I <= ThisRAFNNode.NumHVACs; ++I ) {

			if ( ThisRAFNNode.HVAC( I ).TypeOfNum == ZoneEquipTypeOf_BaseboardRadiantConvectiveWater ) {
				//'ZoneHVAC:Baseboard:RadiantConvective:Water' 13
				SimHWBaseboard( ThisRAFNNode.HVAC( I ).Name, ZoneNum, RoomAirflowNetworkZoneInfo( ZoneNum ).ActualZoneID, false, SysOutputProvided,
					ThisRAFNNode.HVAC( I ).CompIndex );
				ThisRAFNNode.NonAirSystemResponse += ThisRAFNNode.HVAC( I ).SupplyFraction * SysOutputProvided;
				// LatOutputProvided = 0.0d0 !This baseboard does not add / remove any latent heat
			}

			if ( ThisRAFNNode.HVAC( I ).TypeOfNum == ZoneEquipTypeOf_BaseboardRadiantConvectiveSteam ) {
				// CASE(BBSteam_Num) !'ZoneHVAC:Baseboard:RadiantConvective:Steam' 14
				SimSteamBaseboard( ThisRAFNNode.HVAC( I ).Name, ZoneNum, RoomAirflowNetworkZoneInfo( ZoneNum ).ActualZoneID, false, SysOutputProvided,
					ThisRAFNNode.HVAC( I ).CompIndex );

				ThisRAFNNode.NonAirSystemResponse += ThisRAFNNode.HVAC( I ).SupplyFraction * SysOutputProvided;
				// LatOutputProvided = 0.0d0 !This baseboard does not add / remove any latent heat
			}

			if ( ThisRAFNNode.HVAC( I ).TypeOfNum == ZoneEquipTypeOf_BaseboardConvectiveWater ) {
				// CASE(BBWaterConvective_Num)  !'ZoneHVAC:Baseboard:Convective:Water' 16
				SimBaseboard( ThisRAFNNode.HVAC( I ).Name, ZoneNum, RoomAirflowNetworkZoneInfo( ZoneNum ).ActualZoneID, false, SysOutputProvided,
					ThisRAFNNode.HVAC( I ).CompIndex );
				ThisRAFNNode.NonAirSystemResponse += ThisRAFNNode.HVAC( I ).SupplyFraction * SysOutputProvided;
				// LatOutputProvided = 0.0d0 !This baseboard does not add / remove any latent heat
			}

			if ( ThisRAFNNode.HVAC( I ).TypeOfNum == ZoneEquipTypeOf_BaseboardConvectiveElectric ) {
				// CASE(BBElectricConvective_Num)  !'ZoneHVAC:Baseboard:Convective:Electric' 15
				SimElectricBaseboard( ThisRAFNNode.HVAC( I ).Name, ZoneNum, RoomAirflowNetworkZoneInfo( ZoneNum ).ActualZoneID, SysOutputProvided,
					ThisRAFNNode.HVAC( I ).CompIndex );
				ThisRAFNNode.NonAirSystemResponse += ThisRAFNNode.HVAC( I ).SupplyFraction * SysOutputProvided;
				// LatOutputProvided = 0.0d0 !This baseboard does not add / remove any latent heat
			}

			if ( ThisRAFNNode.HVAC( I ).TypeOfNum == ZoneEquipTypeOf_RefrigerationChillerSet ) {
				// CASE(RefrigerationAirChillerSet_Num)  !'ZoneHVAC:RefrigerationChillerSet' 20
				SimAirChillerSet( ThisRAFNNode.HVAC( I ).Name, ZoneNum, false, SysOutputProvided, LatOutputProvided, ThisRAFNNode.HVAC( I ).CompIndex );
				ThisRAFNNode.NonAirSystemResponse += ThisRAFNNode.HVAC( I ).SupplyFraction * SysOutputProvided;
			}

			if ( ThisRAFNNode.HVAC( I ).TypeOfNum == ZoneEquipTypeOf_BaseboardRadiantConvectiveElectric ) {
				//CASE(BBElectric_Num)  !'ZoneHVAC:Baseboard:RadiantConvective:Electric' 12
				SimElecBaseboard( ThisRAFNNode.HVAC( I ).Name, ZoneNum, RoomAirflowNetworkZoneInfo( ZoneNum ).ActualZoneID, false, SysOutputProvided,
					ThisRAFNNode.HVAC( I ).CompIndex );
				ThisRAFNNode.NonAirSystemResponse += ThisRAFNNode.HVAC( I ).SupplyFraction * SysOutputProvided;
				// LatOutputProvided = 0.0d0 !This baseboard does not add / remove any latent heat
			}

			if ( ThisRAFNNode.HVAC( I ).TypeOfNum == ZoneEquipTypeOf_HighTemperatureRadiant ) {
				//CASE(BBElectric_Num)  !'ZoneHVAC:HighTemperatureRadiant' 17
				SimHighTempRadiantSystem( ThisRAFNNode.HVAC( I ).Name, false, SysOutputProvided,
					ThisRAFNNode.HVAC( I ).CompIndex );
				ThisRAFNNode.NonAirSystemResponse += ThisRAFNNode.HVAC( I ).SupplyFraction * SysOutputProvided;
				// LatOutputProvided = 0.0d0 !This baseboard does not add / remove any latent heat
			}

			//Zone sum of system convective gains, collected via NonAirSystemResponse
		}

	} // SumNonAirSystemResponseForNode

	//*****************************************************************************************

	void
	RAFNData::SumSystemDepResponseForNode(
	)
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         B.Griffith
		//       DATE WRITTEN   aug 2005, Jan2004
		//       MODIFIED       Lixing Gu, Aug. 2015 for v8.4 replease
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Sum system sensible loads used at the next time step

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// USE STATEMENTS:
		using ZoneDehumidifier::SimZoneDehumidifier;
		using DataHVACGlobals::ZoneEquipTypeOf_DehumidifierDX;

		// Return value
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int I;
		Real64 SysOutputProvided;
		Real64 LatOutputProvided;
		int RoomAirNode;

		// TODO

		auto & ThisRAFNZone( RoomAirflowNetworkZoneInfo( ZoneNum ) );

		// SysDepZoneLoads saved to be added to zone heat balance next
		SysOutputProvided = 0.0;
		for ( RoomAirNode = 1; RoomAirNode <= ThisRAFNZone.NumOfAirNodes; ++RoomAirNode ) {
			ThisRAFNZone.Node( RoomAirNode ).SysDepZoneLoadsLaggedOld = 0.0;
			for ( I = 1; I <= ThisRAFNZone.Node( RoomAirNode ).NumHVACs; ++I ) {
				if ( ThisRAFNZone.Node( RoomAirNode ).HVAC( I ).TypeOfNum == ZoneEquipTypeOf_DehumidifierDX ) {
					if ( SysOutputProvided == 0.0 ) SimZoneDehumidifier( ThisRAFNZone.Node( RoomAirNode ).HVAC( I ).Name, ZoneNum, false, SysOutputProvided, LatOutputProvided, ThisRAFNZone.Node( RoomAirNode ).HVAC( I ).CompIndex );
					if ( SysOutputProvided > 0.0 ) break;
				}
			}
		}

		if ( SysOutputProvided > 0.0 ) {
			for ( RoomAirNode = 1; RoomAirNode <= ThisRAFNZone.NumOfAirNodes; ++RoomAirNode ) {
				for ( I = 1; I <= ThisRAFNZone.Node( RoomAirNode ).NumHVACs; ++I ) {
					if ( ThisRAFNZone.Node( RoomAirNode ).HVAC( I ).TypeOfNum == ZoneEquipTypeOf_DehumidifierDX ) {
						ThisRAFNZone.Node( RoomAirNode ).SysDepZoneLoadsLaggedOld += ThisRAFNZone.Node( RoomAirNode ).HVAC( I ).SupplyFraction * SysOutputProvided;
					}
				}
			}
		}

	} // SumSystemDepResponseForNode

	//*****************************************************************************************

} // RoomAirModelAirflowNetwork

} // EnergyPlus
