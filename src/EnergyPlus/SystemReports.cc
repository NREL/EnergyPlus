// C++ Headers
#include <algorithm>
#include <cassert>
#include <cmath>
#include <string>
#include <vector>

// ObjexxFCL Headers
#include <ObjexxFCL/FArray.functions.hh>
#include <ObjexxFCL/Fmath.hh>
#include <ObjexxFCL/gio.hh>
#include <ObjexxFCL/string.functions.hh>

// EnergyPlus Headers
#include <SystemReports.hh>
#include <BranchNodeConnections.hh>
#include <DataAirLoop.hh>
#include <DataAirSystems.hh>
#include <DataConvergParams.hh>
#include <DataEnvironment.hh>
#include <DataGlobalConstants.hh>
#include <DataGlobals.hh>
#include <DataHeatBalance.hh>
#include <DataHeatBalFanSys.hh>
#include <DataHVACGlobals.hh>
#include <DataLoopNode.hh>
#include <DataPlant.hh>
#include <DataPrecisionGlobals.hh>
#include <DataSizing.hh>
#include <DataZoneEnergyDemands.hh>
#include <DataZoneEquipment.hh>
#include <FanCoilUnits.hh>
#include <HVACStandAloneERV.hh>
#include <InputProcessor.hh>
#include <OutputProcessor.hh>
#include <PackagedTerminalHeatPump.hh>
#include <Psychrometrics.hh>
#include <PurchasedAirManager.hh>
#include <SplitterComponent.hh>
#include <UnitVentilator.hh>
#include <UtilityRoutines.hh>
#include <WindowAC.hh>
#include <ZonePlenum.hh>

namespace EnergyPlus {

namespace SystemReports {

	// Module containing the routines dealing with Mechanical Ventilation Loads and Energy Reporting (Outside Air)

	// MODULE INFORMATION:
	//       AUTHOR         Mike Witte, Linda Lawrie, Dan Fisher
	//       DATE WRITTEN   Apr-Jul 2005
	//       MODIFIED       22Aug2010 Craig Wray - added Fan:ComponentModel
	//       RE-ENGINEERED  na

	// PURPOSE OF THIS MODULE:
	// This module embodies the scheme(s) for reporting ventilation loads and energy use.

	// METHODOLOGY EMPLOYED:
	// na

	// REFERENCES:
	// na

	// OTHER NOTES:
	// na

	// Using/Aliasing
	using namespace DataPrecisionGlobals;
	using namespace DataLoopNode;
	using namespace DataAirLoop;
	using namespace DataGlobals;
	using namespace DataHVACGlobals;
	using namespace DataPlant;
	using namespace DataSizing;
	using namespace DataZoneEquipment;
	using namespace DataAirSystems;

	// Data
	// MODULE PARAMETER DEFINITIONS:
	int const NoHeatNoCool( 0 );
	int const CoolingOnly( 1 );
	int const HeatingOnly( 2 );
	int const HeatAndCool( 3 );
	int const MaxSetBackCount( 3 );

	static std::string const BlankString;

	// DERIVED TYPE DEFINITIONS:

	// MODULE VARIABLE DECLARATIONS:
	//Ventilation Report Variables
	FArray1D< Real64 > MaxCoolingLoadMetByVent;
	FArray1D< Real64 > MaxCoolingLoadAddedByVent;
	FArray1D< Real64 > MaxOvercoolingByVent;
	FArray1D< Real64 > MaxHeatingLoadMetByVent;
	FArray1D< Real64 > MaxHeatingLoadAddedByVent;
	FArray1D< Real64 > MaxOverheatingByVent;
	FArray1D< Real64 > MaxNoLoadHeatingByVent;
	FArray1D< Real64 > MaxNoLoadCoolingByVent;

	FArray1D< Real64 > RemMaxCoolingLoadMetByVent;
	FArray1D< Real64 > RemMaxCoolingLoadAddedByVent;
	FArray1D< Real64 > RemMaxOvercoolingByVent;
	FArray1D< Real64 > RemMaxHeatingLoadMetByVent;
	FArray1D< Real64 > RemMaxHeatingLoadAddedByVent;
	FArray1D< Real64 > RemMaxOverheatingByVent;
	FArray1D< Real64 > RemMaxNoLoadHeatingByVent;
	FArray1D< Real64 > RemMaxNoLoadCoolingByVent;

	FArray1D< Real64 > LastMaxCoolingLoadMetByVent;
	FArray1D< Real64 > LastMaxCoolingLoadAddedByVent;
	FArray1D< Real64 > LastMaxOvercoolingByVent;
	FArray1D< Real64 > LastMaxHeatingLoadMetByVent;
	FArray1D< Real64 > LastMaxHeatingLoadAddedByVent;
	FArray1D< Real64 > LastMaxOverheatingByVent;
	FArray1D< Real64 > LastMaxNoLoadHeatingByVent;
	FArray1D< Real64 > LastMaxNoLoadCoolingByVent;

	FArray1D< Real64 > SysTotZoneLoadHTNG;
	FArray1D< Real64 > SysTotZoneLoadCLNG;
	FArray1D< Real64 > SysOALoadHTNG;
	FArray1D< Real64 > SysOALoadCLNG;
	FArray1D< Real64 > SysTotHTNG;
	FArray1D< Real64 > SysTotCLNG;

	FArray1D< Real64 > SysTotH2OHOT;
	FArray1D< Real64 > SysTotH2OCOLD;
	FArray1D< Real64 > SysTotElec;
	FArray1D< Real64 > SysTotGas;
	FArray1D< Real64 > SysTotSteam;

	FArray1D< Real64 > SysHumidHTNG;
	FArray1D< Real64 > SysHumidElec;
	FArray1D< Real64 > SysEvapCLNG;
	FArray1D< Real64 > SysEvapElec;
	FArray1D< Real64 > SysHeatExHTNG;
	FArray1D< Real64 > SysHeatExCLNG;
	FArray1D< Real64 > DesDehumidCLNG;
	FArray1D< Real64 > DesDehumidElec;
	FArray1D< Real64 > SysSolarCollectHeating;
	FArray1D< Real64 > SysSolarCollectCooling;
	FArray1D< Real64 > SysUserDefinedTerminalHeating;
	FArray1D< Real64 > SysUserDefinedTerminalCooling;

	FArray1D< Real64 > SysFANCompHTNG;
	FArray1D< Real64 > SysFANCompElec;
	FArray1D< Real64 > SysCCCompCLNG;
	FArray1D< Real64 > SysCCCompH2OCOLD;
	FArray1D< Real64 > SysCCCompElec;
	FArray1D< Real64 > SysHCCompH2OHOT;
	FArray1D< Real64 > SysHCCompElec;
	FArray1D< Real64 > SysHCCompElecRes;
	FArray1D< Real64 > SysHCCompHTNG;
	FArray1D< Real64 > SysHCCompGas;
	FArray1D< Real64 > SysHCCompSteam;
	FArray1D< Real64 > SysDomesticH20;

	FArray1D< Real64 > ZoneOAMassFlow; // zone mech vent mass flow rate {kg/s}
	FArray1D< Real64 > ZoneOAMass; // zone mech vent total mass for time {kg}
	FArray1D< Real64 > ZoneOAVolFlowStdRho; // zone mech vent volume flow rate at standard density {m3/s}
	FArray1D< Real64 > ZoneOAVolStdRho; // zone mech vent total volume OA at standard density {m3/s}
	FArray1D< Real64 > ZoneOAVolFlowCrntRho; // zone mech vent volume flow rate at current density {m3/s}
	FArray1D< Real64 > ZoneOAVolCrntRho; // zone mech vent total volume OA at current density {m3/s}
	FArray1D< Real64 > ZoneMechACH; // zone mech vent air changes per hour {ACH}

	bool AirLoopLoadsReportEnabled( true );
	bool VentLoadsReportEnabled( true );
	bool VentEnergyReportEnabled( false );
	bool VentReportStructureCreated( false );
	int TotalLoopConnects( 0 ); // Total number of loop connections
	int MaxLoopArraySize( 100 );
	int MaxCompArraySize( 500 );
	int DBFlag( 0 );

	FArray1D_int SetBackCounter;
	FArray1D_int HeatCoolFlag;
	FArray1D_int FirstHeatCoolFlag;
	FArray1D_int FirstHeatCoolHour;
	FArray1D_int LastHeatCoolFlag;
	FArray1D_int LastHeatCoolHour;
	FArray1D_bool AirLoopCalcDone;
	FArray1D_bool NoLoadFlag;
	FArray1D_bool UnmetLoadFlag;

	static gio::Fmt const fmtLD( "*" );
	static gio::Fmt const fmtA( "(A)" );

	// SUBROUTINE SPECIFICATIONS FOR MODULE SystemReports

	//Reporting Initialization

	// Reporting routines for module

	// Object Data
	FArray1D< SummarizeLoads > Vent;

	// MODULE SUBROUTINES:
	//*************************************************************************

	// Functions

	void
	InitEnergyReports()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Dan Fisher
		//       DATE WRITTEN   April 2005
		//       MODIFIED
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Initializes the energy components of the data structures

		// METHODOLOGY EMPLOYED:
		// Once all compsets have been established (second iteration) find all components
		// subcomponents, etc.

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataEnvironment::StdBaroPress;
		using DataEnvironment::OutHumRat;
		using SplitterComponent::SplitterCond;
		using SplitterComponent::NumSplitters;
		using InputProcessor::FindItemInList;
		using Psychrometrics::PsyHFnTdbW;
		using Psychrometrics::PsyRhoAirFnPbTdbW;
		using ZonePlenum::ZoneSupPlenCond;
		using ZonePlenum::NumZoneSupplyPlenums;
		using DataConvergParams::HVACFlowRateToler;
		using namespace DataGlobalConstants;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS

		// SUBROUTINE PARAMETER DEFINITIONS:
		int const TypeComp( 1 );
		int const TypeSubComp( 2 );
		int const TypeSubSubComp( 3 );
		int const EnergyTransfer( 1 );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		int AirDistUnitNum;
		int MatchLoop;
		int MatchLoopType;
		int MatchBranch;
		int MatchComp;
		int AirLoopNum;
		int BranchNum;
		int ZoneInletNodeNum;
		int CompNum;
		int VarNum;
		int SubCompNum;
		int SubSubCompNum;
		int EquipNum;
		int SubEquipNum;
		int SubSubEquipNum;
		int CtrlZoneNum;
		int NodeIndex;
		int Idx;
		int TempIndex;
		int ListNum;
		int SAPNum;
		int SAPOutNode;
		int MainBranchNum;
		int SupplyCoolBranchNum;
		int SupplyHeatBranchNum;
		int VarType;
		int VarIndex;
		int OutNum;
		int NodeCount;
		int PlantLoopNum;
		int NumZoneConnectComps;
		int NumZoneConnectSubComps;
		int NumZoneConnectSubSubComps;
		int NumAirSysConnectComps;
		int NumAirSysConnectSubComps;
		int NumAirSysConnectSubSubComps;
		int ArrayCount;
		int LoopType;
		int LoopNum;
		int FirstIndex;
		int LastIndex;
		int LoopCount;
		std::string CompType;
		std::string CompName;
		bool MatchFound;
		static bool OneTimeFlag( true ); // Flag set to make sure you initialize reports one time
		bool Duplicate;
		bool ConnectionFlag;

		if ( ! VentReportStructureCreated ) return;

		if ( OneTimeFlag ) {

			// ***I think we need to preprocess the main components on the branch to get them in order***
			// This needs to be done before we start in on the component loop
			// GetChildrenData will put all of the subcomponents in order for us

			for ( CtrlZoneNum = 1; CtrlZoneNum <= NumOfZones; ++CtrlZoneNum ) {
				if ( ! ZoneEquipConfig( CtrlZoneNum ).IsControlled ) continue;
				AirLoopNum = ZoneEquipConfig( CtrlZoneNum ).AirLoopNum;
				ZoneEquipConfig( CtrlZoneNum ).EquipListIndex = FindItemInList( ZoneEquipConfig( CtrlZoneNum ).EquipListName, ZoneEquipList.Name(), NumOfZones );
				ListNum = ZoneEquipConfig( CtrlZoneNum ).EquipListIndex;
				for ( ZoneInletNodeNum = 1; ZoneInletNodeNum <= ZoneEquipConfig( CtrlZoneNum ).NumInletNodes; ++ZoneInletNodeNum ) {
					for ( CompNum = 1; CompNum <= ZoneEquipList( ListNum ).NumOfEquipTypes; ++CompNum ) {
						for ( NodeCount = 1; NodeCount <= ZoneEquipList( ListNum ).EquipData( CompNum ).NumOutlets; ++NodeCount ) {
							if ( ZoneEquipList( ListNum ).EquipData( CompNum ).OutletNodeNums( NodeCount ) == ZoneEquipConfig( CtrlZoneNum ).AirDistUnitCool( ZoneInletNodeNum ).OutNode ) {
								ZoneEquipConfig( CtrlZoneNum ).AirDistUnitCool( ZoneInletNodeNum ).AirDistUnitIndex = CompNum;
								if ( ZoneEquipConfig( CtrlZoneNum ).AirDistUnitCool( ZoneInletNodeNum ).SupplyAirPathExists ) {
									for ( SAPNum = 1; SAPNum <= NumSupplyAirPaths; ++SAPNum ) {
										for ( SAPOutNode = 1; SAPOutNode <= SupplyAirPath( SAPNum ).NumOutletNodes; ++SAPOutNode ) {
											if ( ZoneEquipConfig( CtrlZoneNum ).AirDistUnitCool( ZoneInletNodeNum ).InNode == SupplyAirPath( SAPNum ).OutletNode( SAPOutNode ) ) {
												ZoneEquipConfig( CtrlZoneNum ).AirDistUnitCool( ZoneInletNodeNum ).SupplyAirPathIndex = SAPNum;
												for ( OutNum = 1; OutNum <= AirToZoneNodeInfo( AirLoopNum ).NumSupplyNodes; ++OutNum ) {
													if ( AirToZoneNodeInfo( AirLoopNum ).ZoneEquipSupplyNodeNum( OutNum ) == SupplyAirPath( SAPNum ).InletNodeNum ) {
														ZoneEquipConfig( CtrlZoneNum ).AirDistUnitCool( ZoneInletNodeNum ).SupplyBranchIndex = PrimaryAirSystem( AirLoopNum ).OutletBranchNum( OutNum );
														if ( PrimaryAirSystem( AirLoopNum ).Splitter.Exists ) {
															for ( MainBranchNum = 1; MainBranchNum <= PrimaryAirSystem( AirLoopNum ).NumBranches; ++MainBranchNum ) {
																if ( PrimaryAirSystem( AirLoopNum ).Branch( MainBranchNum ).NodeNumOut == PrimaryAirSystem( AirLoopNum ).Splitter.NodeNumIn ) {
																	ZoneEquipConfig( CtrlZoneNum ).AirDistUnitCool( ZoneInletNodeNum ).MainBranchIndex = MainBranchNum;
																}
															}
														} else { //no splitter
															ZoneEquipConfig( CtrlZoneNum ).AirDistUnitCool( ZoneInletNodeNum ).MainBranchIndex = ZoneEquipConfig( CtrlZoneNum ).AirDistUnitCool( ZoneInletNodeNum ).SupplyBranchIndex;
														}
													}
												}
											}
										}
									}
								} else { //no supply air path
									if ( AirLoopNum > 0 ) {
										for ( NodeIndex = 1; NodeIndex <= AirToZoneNodeInfo( AirLoopNum ).NumSupplyNodes; ++NodeIndex ) {
											if ( AirToZoneNodeInfo( AirLoopNum ).ZoneEquipSupplyNodeNum( NodeIndex ) == ZoneEquipConfig( CtrlZoneNum ).AirDistUnitCool( ZoneInletNodeNum ).InNode ) {
												for ( BranchNum = 1; BranchNum <= PrimaryAirSystem( AirLoopNum ).NumBranches; ++BranchNum ) {
													if ( PrimaryAirSystem( AirLoopNum ).Branch( BranchNum ).NodeNumOut == AirToZoneNodeInfo( AirLoopNum ).AirLoopSupplyNodeNum( NodeIndex ) ) {
														ZoneEquipConfig( CtrlZoneNum ).AirDistUnitCool( ZoneInletNodeNum ).SupplyBranchIndex = BranchNum;
														if ( PrimaryAirSystem( AirLoopNum ).Splitter.Exists ) {
															for ( MainBranchNum = 1; MainBranchNum <= PrimaryAirSystem( AirLoopNum ).NumBranches; ++MainBranchNum ) {
																if ( PrimaryAirSystem( AirLoopNum ).Branch( MainBranchNum ).NodeNumOut == PrimaryAirSystem( AirLoopNum ).Splitter.NodeNumIn ) {
																	ZoneEquipConfig( CtrlZoneNum ).AirDistUnitCool( ZoneInletNodeNum ).MainBranchIndex = MainBranchNum;
																}
															}
														} else { //no splitter
															ZoneEquipConfig( CtrlZoneNum ).AirDistUnitCool( ZoneInletNodeNum ).MainBranchIndex = ZoneEquipConfig( CtrlZoneNum ).AirDistUnitCool( ZoneInletNodeNum ).SupplyAirPathIndex;
														}
													}
												}
											}
										}
									}
								}
							} else if ( ZoneEquipList( ListNum ).EquipData( CompNum ).OutletNodeNums( NodeCount ) == ZoneEquipConfig( CtrlZoneNum ).AirDistUnitHeat( ZoneInletNodeNum ).InNode ) {
								ZoneEquipConfig( CtrlZoneNum ).AirDistUnitHeat( ZoneInletNodeNum ).AirDistUnitIndex = CompNum;
								if ( ZoneEquipConfig( CtrlZoneNum ).AirDistUnitHeat( ZoneInletNodeNum ).SupplyAirPathExists ) {
									for ( SAPNum = 1; SAPNum <= NumSupplyAirPaths; ++SAPNum ) {
										for ( NodeIndex = 1; NodeIndex <= AirToZoneNodeInfo( AirLoopNum ).NumSupplyNodes; ++NodeIndex ) {
											if ( AirToZoneNodeInfo( AirLoopNum ).ZoneEquipSupplyNodeNum( NodeIndex ) == SupplyAirPath( SAPNum ).InletNodeNum ) {
												for ( BranchNum = 1; BranchNum <= PrimaryAirSystem( AirLoopNum ).NumBranches; ++BranchNum ) {
													if ( PrimaryAirSystem( AirLoopNum ).Branch( BranchNum ).NodeNumOut == AirToZoneNodeInfo( AirLoopNum ).AirLoopSupplyNodeNum( NodeIndex ) ) {
														ZoneEquipConfig( CtrlZoneNum ).AirDistUnitHeat( ZoneInletNodeNum ).SupplyBranchIndex = BranchNum;
														if ( PrimaryAirSystem( AirLoopNum ).Splitter.Exists ) {
															for ( MainBranchNum = 1; MainBranchNum <= PrimaryAirSystem( AirLoopNum ).NumBranches; ++MainBranchNum ) {
																if ( PrimaryAirSystem( AirLoopNum ).Branch( MainBranchNum ).NodeNumOut == PrimaryAirSystem( AirLoopNum ).Splitter.NodeNumIn ) {
																	ZoneEquipConfig( CtrlZoneNum ).AirDistUnitHeat( ZoneInletNodeNum ).MainBranchIndex = MainBranchNum;
																}
															}
														} else { //no splitter
															ZoneEquipConfig( CtrlZoneNum ).AirDistUnitHeat( ZoneInletNodeNum ).MainBranchIndex = ZoneEquipConfig( CtrlZoneNum ).AirDistUnitHeat( ZoneInletNodeNum ).SupplyAirPathIndex;
														}
													}
												}
											}
										}

										for ( SAPOutNode = 1; SAPOutNode <= SupplyAirPath( SAPNum ).NumOutletNodes; ++SAPOutNode ) {
											if ( ZoneInletNodeNum == SupplyAirPath( SAPNum ).OutletNode( SAPOutNode ) ) {
												ZoneEquipConfig( CtrlZoneNum ).AirDistUnitHeat( ZoneInletNodeNum ).SupplyAirPathIndex = SAPNum;
											}
										}
									}
								} else { //no supply air path
									if ( AirLoopNum > 0 ) {
										for ( NodeIndex = 1; NodeIndex <= AirToZoneNodeInfo( AirLoopNum ).NumSupplyNodes; ++NodeIndex ) {
											if ( AirToZoneNodeInfo( AirLoopNum ).ZoneEquipSupplyNodeNum( NodeIndex ) == ZoneEquipConfig( CtrlZoneNum ).AirDistUnitHeat( ZoneInletNodeNum ).InNode ) {
												for ( BranchNum = 1; BranchNum <= PrimaryAirSystem( AirLoopNum ).NumBranches; ++BranchNum ) {
													if ( PrimaryAirSystem( AirLoopNum ).Branch( BranchNum ).NodeNumOut == AirToZoneNodeInfo( AirLoopNum ).AirLoopSupplyNodeNum( NodeIndex ) ) {
														ZoneEquipConfig( CtrlZoneNum ).AirDistUnitHeat( ZoneInletNodeNum ).SupplyBranchIndex = BranchNum;
														if ( PrimaryAirSystem( AirLoopNum ).Splitter.Exists ) {
															for ( MainBranchNum = 1; MainBranchNum <= PrimaryAirSystem( AirLoopNum ).NumBranches; ++MainBranchNum ) {
																if ( PrimaryAirSystem( AirLoopNum ).Branch( MainBranchNum ).NodeNumOut == PrimaryAirSystem( AirLoopNum ).Splitter.NodeNumIn ) {
																	ZoneEquipConfig( CtrlZoneNum ).AirDistUnitHeat( ZoneInletNodeNum ).MainBranchIndex = MainBranchNum;
																}
															}
														} else { //no splitter
															ZoneEquipConfig( CtrlZoneNum ).AirDistUnitHeat( ZoneInletNodeNum ).MainBranchIndex = ZoneEquipConfig( CtrlZoneNum ).AirDistUnitHeat( ZoneInletNodeNum ).SupplyAirPathIndex;
														}
													}
												}
											}
										}
									}
								}
							} else {

								//Can't tell if there's an error based on this code...need to check logical flags separately
							}
						}
					}
				}
			}

			for ( CtrlZoneNum = 1; CtrlZoneNum <= NumOfZones; ++CtrlZoneNum ) {
				if ( ! ZoneEquipConfig( CtrlZoneNum ).IsControlled ) continue;
				AirLoopNum = ZoneEquipConfig( CtrlZoneNum ).AirLoopNum;
				ZoneEquipConfig( CtrlZoneNum ).EquipListIndex = FindItemInList( ZoneEquipConfig( CtrlZoneNum ).EquipListName, ZoneEquipList.Name(), NumOfZones );
				ListNum = ZoneEquipConfig( CtrlZoneNum ).EquipListIndex;
				//loop over the zone supply air path inlet nodes
				for ( ZoneInletNodeNum = 1; ZoneInletNodeNum <= ZoneEquipConfig( CtrlZoneNum ).NumInletNodes; ++ZoneInletNodeNum ) {

					// 1. Find HVAC component plant loop connections
					MainBranchNum = ZoneEquipConfig( CtrlZoneNum ).AirDistUnitHeat( ZoneInletNodeNum ).MainBranchIndex;
					MainBranchNum = max( MainBranchNum, ZoneEquipConfig( CtrlZoneNum ).AirDistUnitCool( ZoneInletNodeNum ).MainBranchIndex );
					if ( MainBranchNum > 0 ) MatchPlantSys( AirLoopNum, MainBranchNum );
					SupplyCoolBranchNum = ZoneEquipConfig( CtrlZoneNum ).AirDistUnitCool( ZoneInletNodeNum ).SupplyBranchIndex;
					if ( SupplyCoolBranchNum > 0 && ( SupplyCoolBranchNum != MainBranchNum ) ) MatchPlantSys( AirLoopNum, SupplyCoolBranchNum );
					SupplyHeatBranchNum = ZoneEquipConfig( CtrlZoneNum ).AirDistUnitHeat( ZoneInletNodeNum ).SupplyBranchIndex;
					if ( SupplyHeatBranchNum > 0 && ( SupplyHeatBranchNum != MainBranchNum ) ) MatchPlantSys( AirLoopNum, SupplyHeatBranchNum );

					AirDistUnitNum = ZoneEquipConfig( CtrlZoneNum ).AirDistUnitCool( ZoneInletNodeNum ).AirDistUnitIndex;
					AirDistUnitNum = max( AirDistUnitNum, ZoneEquipConfig( CtrlZoneNum ).AirDistUnitHeat( ZoneInletNodeNum ).AirDistUnitIndex );
					if ( ListNum > 0 && AirDistUnitNum > 0 ) {
						for ( VarNum = 1; VarNum <= ZoneEquipList( ListNum ).EquipData( AirDistUnitNum ).NumMeteredVars; ++VarNum ) {
							if ( ZoneEquipList( ListNum ).EquipData( AirDistUnitNum ).MeteredVar( VarNum ).ResourceType == iRT_EnergyTransfer ) {
								ZoneEquipList( ListNum ).EquipData( AirDistUnitNum ).EnergyTransComp = EnergyTransfer;
								CompType = ZoneEquipList( ListNum ).EquipData( AirDistUnitNum ).TypeOf;
								CompName = ZoneEquipList( ListNum ).EquipData( AirDistUnitNum ).Name;
								Idx = 0;
								FindDemandSideMatch( CompType, CompName, MatchFound, MatchLoopType, MatchLoop, MatchBranch, MatchComp );
								if ( MatchFound ) UpdateZoneCompPtrArray( Idx, ListNum, AirDistUnitNum, MatchLoopType, MatchLoop, MatchBranch, MatchComp );
								ZoneEquipList( ListNum ).EquipData( AirDistUnitNum ).ZoneEqToPlantPtr = Idx;
								break;
							}
						}
						for ( SubEquipNum = 1; SubEquipNum <= ZoneEquipList( ListNum ).EquipData( AirDistUnitNum ).NumSubEquip; ++SubEquipNum ) {
							for ( VarNum = 1; VarNum <= ZoneEquipList( ListNum ).EquipData( AirDistUnitNum ).SubEquipData( SubEquipNum ).NumMeteredVars; ++VarNum ) {
								if ( ZoneEquipList( ListNum ).EquipData( AirDistUnitNum ).SubEquipData( SubEquipNum ).MeteredVar( VarNum ).ResourceType == iRT_EnergyTransfer ) {
									ZoneEquipList( ListNum ).EquipData( AirDistUnitNum ).SubEquipData( SubEquipNum ).EnergyTransComp = EnergyTransfer;
									CompType = ZoneEquipList( ListNum ).EquipData( AirDistUnitNum ).SubEquipData( SubEquipNum ).TypeOf;
									CompName = ZoneEquipList( ListNum ).EquipData( AirDistUnitNum ).SubEquipData( SubEquipNum ).Name;
									Idx = 0;
									FindDemandSideMatch( CompType, CompName, MatchFound, MatchLoopType, MatchLoop, MatchBranch, MatchComp );
									if ( MatchFound ) UpdateZoneSubCompPtrArray( Idx, ListNum, AirDistUnitNum, SubEquipNum, MatchLoopType, MatchLoop, MatchBranch, MatchComp );
									ZoneEquipList( ListNum ).EquipData( AirDistUnitNum ).SubEquipData( SubEquipNum ).ZoneEqToPlantPtr = Idx;
									break;
								}
							}
							for ( SubSubEquipNum = 1; SubSubEquipNum <= ZoneEquipList( ListNum ).EquipData( AirDistUnitNum ).SubEquipData( SubEquipNum ).NumSubSubEquip; ++SubSubEquipNum ) {
								for ( VarNum = 1; VarNum <= ZoneEquipList( ListNum ).EquipData( AirDistUnitNum ).SubEquipData( SubEquipNum ).SubSubEquipData( SubSubEquipNum ).NumMeteredVars; ++VarNum ) {
									if ( ZoneEquipList( ListNum ).EquipData( AirDistUnitNum ).SubEquipData( SubEquipNum ).SubSubEquipData( SubSubEquipNum ).MeteredVar( VarNum ).ResourceType == iRT_EnergyTransfer ) {
										ZoneEquipList( ListNum ).EquipData( AirDistUnitNum ).SubEquipData( SubEquipNum ).SubSubEquipData( SubSubEquipNum ).EnergyTransComp = EnergyTransfer;
										CompType = ZoneEquipList( ListNum ).EquipData( AirDistUnitNum ).SubEquipData( SubEquipNum ).SubSubEquipData( SubSubEquipNum ).TypeOf;
										CompName = ZoneEquipList( ListNum ).EquipData( AirDistUnitNum ).SubEquipData( SubEquipNum ).SubSubEquipData( SubSubEquipNum ).Name;
										Idx = 0;
										FindDemandSideMatch( CompType, CompName, MatchFound, MatchLoopType, MatchLoop, MatchBranch, MatchComp );
										if ( MatchFound ) UpdateZoneSubSubCompPtrArray( Idx, ListNum, AirDistUnitNum, SubEquipNum, SubSubEquipNum, MatchLoopType, MatchLoop, MatchBranch, MatchComp );
										ZoneEquipList( ListNum ).EquipData( AirDistUnitNum ).SubEquipData( SubEquipNum ).SubSubEquipData( SubSubEquipNum ).ZoneEqToPlantPtr = Idx;
										break;
									}
								}
							}
						}
					}

					//Eliminate duplicates in the connection arrays
					if ( allocated( ZoneCompToPlant ) ) {
						EquipNum = isize( ZoneCompToPlant );
					} else {
						EquipNum = 0;
					}
					if ( allocated( ZoneSubCompToPlant ) ) {
						SubEquipNum = isize( ZoneSubCompToPlant );
					} else {
						SubEquipNum = 0;
					}
					if ( allocated( ZoneSubSubCompToPlant ) ) {
						SubSubEquipNum = isize( ZoneSubSubCompToPlant );
					} else {
						SubSubEquipNum = 0;
					}
					if ( allocated( AirSysCompToPlant ) ) {
						CompNum = isize( AirSysCompToPlant );
					} else {
						CompNum = 0;
					}
					if ( allocated( AirSysSubCompToPlant ) ) {
						SubCompNum = isize( AirSysSubCompToPlant );
					} else {
						SubCompNum = 0;
					}
					if ( allocated( AirSysSubSubCompToPlant ) ) {
						SubSubCompNum = isize( AirSysSubSubCompToPlant );
					} else {
						SubSubCompNum = 0;
					}

					if ( EquipNum > 0 ) {
						TempZoneCompToPlant.ZoneEqListNum() = 0;
						TempZoneCompToPlant.ZoneEqCompNum() = 0;
						TempZoneCompToPlant.PlantLoopType() = 0;
						TempZoneCompToPlant.PlantLoopNum() = 0;
						TempZoneCompToPlant.PlantLoopBranch() = 0;
						TempZoneCompToPlant.PlantLoopComp() = 0;
						TempZoneCompToPlant.FirstDemandSidePtr() = 0;
						TempZoneCompToPlant.LastDemandSidePtr() = 0;

						ArrayCount = 0;
						for ( Idx = 1; Idx <= EquipNum; ++Idx ) {
							Duplicate = false;
							for ( TempIndex = 1; TempIndex <= EquipNum; ++TempIndex ) {
								if ( ZoneCompToPlant( Idx ).ZoneEqListNum == TempZoneCompToPlant( TempIndex ).ZoneEqListNum && ZoneCompToPlant( Idx ).ZoneEqCompNum == ZoneCompToPlant( TempIndex ).ZoneEqCompNum ) {
									Duplicate = true;
									break;
								}
							}
							if ( ! Duplicate ) {
								++ArrayCount;
								TempZoneCompToPlant( ArrayCount ).ZoneEqListNum = ZoneCompToPlant( Idx ).ZoneEqListNum;
								TempZoneCompToPlant( ArrayCount ).ZoneEqCompNum = ZoneCompToPlant( Idx ).ZoneEqCompNum;
								TempZoneCompToPlant( ArrayCount ).PlantLoopType = ZoneCompToPlant( Idx ).PlantLoopType;
								TempZoneCompToPlant( ArrayCount ).PlantLoopNum = ZoneCompToPlant( Idx ).PlantLoopNum;
								TempZoneCompToPlant( ArrayCount ).PlantLoopBranch = ZoneCompToPlant( Idx ).PlantLoopBranch;
								TempZoneCompToPlant( ArrayCount ).PlantLoopComp = ZoneCompToPlant( Idx ).PlantLoopComp;
								TempZoneCompToPlant( ArrayCount ).FirstDemandSidePtr = ZoneCompToPlant( Idx ).FirstDemandSidePtr;
								TempZoneCompToPlant( ArrayCount ).LastDemandSidePtr = ZoneCompToPlant( Idx ).LastDemandSidePtr;
							}
						}

						ZoneCompToPlant.ZoneEqListNum() = TempZoneCompToPlant.ZoneEqListNum();
						ZoneCompToPlant.ZoneEqCompNum() = TempZoneCompToPlant.ZoneEqCompNum();
						ZoneCompToPlant.PlantLoopType() = TempZoneCompToPlant.PlantLoopType();
						ZoneCompToPlant.PlantLoopNum() = TempZoneCompToPlant.PlantLoopNum();
						ZoneCompToPlant.PlantLoopBranch() = TempZoneCompToPlant.PlantLoopBranch();
						ZoneCompToPlant.PlantLoopComp() = TempZoneCompToPlant.PlantLoopComp();
						ZoneCompToPlant.FirstDemandSidePtr() = TempZoneCompToPlant.FirstDemandSidePtr();
						ZoneCompToPlant.LastDemandSidePtr() = TempZoneCompToPlant.LastDemandSidePtr();

					}

					if ( SubEquipNum > 0 ) {
						TempZoneSubCompToPlant.ZoneEqListNum() = 0;
						TempZoneSubCompToPlant.ZoneEqCompNum() = 0;
						TempZoneSubCompToPlant.ZoneEqSubCompNum() = 0;
						TempZoneSubCompToPlant.PlantLoopType() = 0;
						TempZoneSubCompToPlant.PlantLoopNum() = 0;
						TempZoneSubCompToPlant.PlantLoopBranch() = 0;
						TempZoneSubCompToPlant.PlantLoopComp() = 0;
						TempZoneSubCompToPlant.FirstDemandSidePtr() = 0;
						TempZoneSubCompToPlant.LastDemandSidePtr() = 0;

						ArrayCount = 0;
						for ( Idx = 1; Idx <= SubEquipNum; ++Idx ) {
							Duplicate = false;
							for ( TempIndex = 1; TempIndex <= SubEquipNum; ++TempIndex ) {
								if ( ZoneSubCompToPlant( Idx ).ZoneEqListNum == TempZoneSubCompToPlant( TempIndex ).ZoneEqListNum && ZoneSubCompToPlant( Idx ).ZoneEqCompNum == TempZoneSubCompToPlant( TempIndex ).ZoneEqCompNum && ZoneSubCompToPlant( Idx ).ZoneEqSubCompNum == TempZoneSubCompToPlant( TempIndex ).ZoneEqSubCompNum ) {
									Duplicate = true;
									break;
								}
							}
							if ( ! Duplicate ) {
								++ArrayCount;
								TempZoneSubCompToPlant( ArrayCount ).ZoneEqListNum = ZoneSubCompToPlant( Idx ).ZoneEqListNum;
								TempZoneSubCompToPlant( ArrayCount ).ZoneEqCompNum = ZoneSubCompToPlant( Idx ).ZoneEqCompNum;
								TempZoneSubCompToPlant( ArrayCount ).ZoneEqSubCompNum = ZoneSubCompToPlant( Idx ).ZoneEqSubCompNum;
								TempZoneSubCompToPlant( ArrayCount ).PlantLoopType = ZoneSubCompToPlant( Idx ).PlantLoopType;
								TempZoneSubCompToPlant( ArrayCount ).PlantLoopNum = ZoneSubCompToPlant( Idx ).PlantLoopNum;
								TempZoneSubCompToPlant( ArrayCount ).PlantLoopBranch = ZoneSubCompToPlant( Idx ).PlantLoopBranch;
								TempZoneSubCompToPlant( ArrayCount ).PlantLoopComp = ZoneSubCompToPlant( Idx ).PlantLoopComp;
								TempZoneSubCompToPlant( ArrayCount ).FirstDemandSidePtr = ZoneSubCompToPlant( Idx ).FirstDemandSidePtr;
								TempZoneSubCompToPlant( ArrayCount ).LastDemandSidePtr = ZoneSubCompToPlant( Idx ).LastDemandSidePtr;
							}
						}

						ZoneSubCompToPlant.ZoneEqListNum() = TempZoneSubCompToPlant.ZoneEqListNum();
						ZoneSubCompToPlant.ZoneEqCompNum() = TempZoneSubCompToPlant.ZoneEqCompNum();
						ZoneSubCompToPlant.ZoneEqSubCompNum() = TempZoneSubCompToPlant.ZoneEqSubCompNum();
						ZoneSubCompToPlant.PlantLoopType() = TempZoneSubCompToPlant.PlantLoopType();
						ZoneSubCompToPlant.PlantLoopNum() = TempZoneSubCompToPlant.PlantLoopNum();
						ZoneSubCompToPlant.PlantLoopBranch() = TempZoneSubCompToPlant.PlantLoopBranch();
						ZoneSubCompToPlant.PlantLoopComp() = TempZoneSubCompToPlant.PlantLoopComp();
						ZoneSubCompToPlant.FirstDemandSidePtr() = TempZoneSubCompToPlant.FirstDemandSidePtr();
						ZoneSubCompToPlant.LastDemandSidePtr() = TempZoneSubCompToPlant.LastDemandSidePtr();

					}

					if ( SubSubEquipNum > 0 ) {
						TempZoneSubSubCompToPlant.ZoneEqListNum() = 0;
						TempZoneSubSubCompToPlant.ZoneEqCompNum() = 0;
						TempZoneSubSubCompToPlant.ZoneEqSubCompNum() = 0;
						TempZoneSubSubCompToPlant.ZoneEqSubSubCompNum() = 0;
						TempZoneSubSubCompToPlant.PlantLoopType() = 0;
						TempZoneSubSubCompToPlant.PlantLoopNum() = 0;
						TempZoneSubSubCompToPlant.PlantLoopBranch() = 0;
						TempZoneSubSubCompToPlant.PlantLoopComp() = 0;
						TempZoneSubSubCompToPlant.FirstDemandSidePtr() = 0;
						TempZoneSubSubCompToPlant.LastDemandSidePtr() = 0;

						ArrayCount = 0;
						for ( Idx = 1; Idx <= SubSubEquipNum; ++Idx ) {
							Duplicate = false;
							for ( TempIndex = 1; TempIndex <= SubSubEquipNum; ++TempIndex ) {
								if ( ZoneSubSubCompToPlant( Idx ).ZoneEqListNum == TempZoneSubSubCompToPlant( TempIndex ).ZoneEqListNum && ZoneSubSubCompToPlant( Idx ).ZoneEqCompNum == TempZoneSubSubCompToPlant( TempIndex ).ZoneEqCompNum && ZoneSubSubCompToPlant( Idx ).ZoneEqSubCompNum == TempZoneSubSubCompToPlant( TempIndex ).ZoneEqSubCompNum && ZoneSubSubCompToPlant( Idx ).ZoneEqSubSubCompNum == TempZoneSubSubCompToPlant( TempIndex ).ZoneEqSubSubCompNum ) {
									Duplicate = true;
									break;
								}
							}
							if ( ! Duplicate ) {
								++ArrayCount;
								TempZoneSubSubCompToPlant( ArrayCount ).ZoneEqListNum = ZoneSubSubCompToPlant( Idx ).ZoneEqListNum;
								TempZoneSubSubCompToPlant( ArrayCount ).ZoneEqCompNum = ZoneSubSubCompToPlant( Idx ).ZoneEqCompNum;
								TempZoneSubSubCompToPlant( ArrayCount ).ZoneEqSubCompNum = ZoneSubSubCompToPlant( Idx ).ZoneEqSubCompNum;
								TempZoneSubSubCompToPlant( ArrayCount ).ZoneEqSubSubCompNum = ZoneSubSubCompToPlant( Idx ).ZoneEqSubSubCompNum;
								TempZoneSubSubCompToPlant( ArrayCount ).PlantLoopType = ZoneSubSubCompToPlant( Idx ).PlantLoopType;
								TempZoneSubSubCompToPlant( ArrayCount ).PlantLoopNum = ZoneSubSubCompToPlant( Idx ).PlantLoopNum;
								TempZoneSubSubCompToPlant( ArrayCount ).PlantLoopBranch = ZoneSubSubCompToPlant( Idx ).PlantLoopBranch;
								TempZoneSubSubCompToPlant( ArrayCount ).PlantLoopComp = ZoneSubSubCompToPlant( Idx ).PlantLoopComp;
								TempZoneSubSubCompToPlant( ArrayCount ).FirstDemandSidePtr = ZoneSubSubCompToPlant( Idx ).FirstDemandSidePtr;
								TempZoneSubSubCompToPlant( ArrayCount ).LastDemandSidePtr = ZoneSubSubCompToPlant( Idx ).LastDemandSidePtr;
							}
						}

						ZoneSubSubCompToPlant.ZoneEqListNum() = TempZoneSubSubCompToPlant.ZoneEqListNum();
						ZoneSubSubCompToPlant.ZoneEqCompNum() = TempZoneSubSubCompToPlant.ZoneEqCompNum();
						ZoneSubSubCompToPlant.ZoneEqSubCompNum() = TempZoneSubSubCompToPlant.ZoneEqSubCompNum();
						ZoneSubSubCompToPlant.ZoneEqSubSubCompNum() = TempZoneSubSubCompToPlant.ZoneEqSubSubCompNum();
						ZoneSubSubCompToPlant.PlantLoopType() = TempZoneSubSubCompToPlant.PlantLoopType();
						ZoneSubSubCompToPlant.PlantLoopNum() = TempZoneSubSubCompToPlant.PlantLoopNum();
						ZoneSubSubCompToPlant.PlantLoopBranch() = TempZoneSubSubCompToPlant.PlantLoopBranch();
						ZoneSubSubCompToPlant.PlantLoopComp() = TempZoneSubSubCompToPlant.PlantLoopComp();
						ZoneSubSubCompToPlant.FirstDemandSidePtr() = TempZoneSubSubCompToPlant.FirstDemandSidePtr();
						ZoneSubSubCompToPlant.LastDemandSidePtr() = TempZoneSubSubCompToPlant.LastDemandSidePtr();

					}

					if ( CompNum > 0 ) {
						TempAirSysCompToPlant.AirLoopNum() = 0;
						TempAirSysCompToPlant.AirLoopBranch() = 0;
						TempAirSysCompToPlant.AirLoopComp() = 0;
						TempAirSysCompToPlant.PlantLoopType() = 0;
						TempAirSysCompToPlant.PlantLoopNum() = 0;
						TempAirSysCompToPlant.PlantLoopBranch() = 0;
						TempAirSysCompToPlant.PlantLoopComp() = 0;
						TempAirSysCompToPlant.FirstDemandSidePtr() = 0;
						TempAirSysCompToPlant.LastDemandSidePtr() = 0;

						ArrayCount = 0;
						for ( Idx = 1; Idx <= CompNum; ++Idx ) {
							Duplicate = false;
							for ( TempIndex = 1; TempIndex <= CompNum; ++TempIndex ) {
								if ( AirSysCompToPlant( Idx ).AirLoopNum == TempAirSysCompToPlant( TempIndex ).AirLoopNum && AirSysCompToPlant( Idx ).AirLoopBranch == TempAirSysCompToPlant( TempIndex ).AirLoopBranch && AirSysCompToPlant( Idx ).AirLoopComp == TempAirSysCompToPlant( TempIndex ).AirLoopComp ) {
									Duplicate = true;
									break;
								}
							}
							if ( ! Duplicate ) {
								++ArrayCount;
								TempAirSysCompToPlant( ArrayCount ).AirLoopNum = AirSysCompToPlant( Idx ).AirLoopNum;
								TempAirSysCompToPlant( ArrayCount ).AirLoopBranch = AirSysCompToPlant( Idx ).AirLoopBranch;
								TempAirSysCompToPlant( ArrayCount ).AirLoopComp = AirSysCompToPlant( Idx ).AirLoopComp;
								TempAirSysCompToPlant( ArrayCount ).PlantLoopType = AirSysCompToPlant( Idx ).PlantLoopType;
								TempAirSysCompToPlant( ArrayCount ).PlantLoopNum = AirSysCompToPlant( Idx ).PlantLoopNum;
								TempAirSysCompToPlant( ArrayCount ).PlantLoopBranch = AirSysCompToPlant( Idx ).PlantLoopBranch;
								TempAirSysCompToPlant( ArrayCount ).PlantLoopComp = AirSysCompToPlant( Idx ).PlantLoopComp;
								TempAirSysCompToPlant( ArrayCount ).FirstDemandSidePtr = AirSysCompToPlant( Idx ).FirstDemandSidePtr;
								TempAirSysCompToPlant( ArrayCount ).LastDemandSidePtr = AirSysCompToPlant( Idx ).LastDemandSidePtr;
							}
						}

						AirSysCompToPlant.AirLoopNum() = TempAirSysCompToPlant.AirLoopNum();
						AirSysCompToPlant.AirLoopBranch() = TempAirSysCompToPlant.AirLoopBranch();
						AirSysCompToPlant.AirLoopComp() = TempAirSysCompToPlant.AirLoopComp();
						AirSysCompToPlant.PlantLoopType() = TempAirSysCompToPlant.PlantLoopType();
						AirSysCompToPlant.PlantLoopNum() = TempAirSysCompToPlant.PlantLoopNum();
						AirSysCompToPlant.PlantLoopBranch() = TempAirSysCompToPlant.PlantLoopBranch();
						AirSysCompToPlant.PlantLoopComp() = TempAirSysCompToPlant.PlantLoopComp();
						AirSysCompToPlant.FirstDemandSidePtr() = TempAirSysCompToPlant.FirstDemandSidePtr();
						AirSysCompToPlant.LastDemandSidePtr() = TempAirSysCompToPlant.LastDemandSidePtr();

					}

					if ( SubCompNum > 0 ) {
						TempAirSysSubCompToPlant.AirLoopNum() = 0;
						TempAirSysSubCompToPlant.AirLoopBranch() = 0;
						TempAirSysSubCompToPlant.AirLoopComp() = 0;
						TempAirSysSubCompToPlant.AirLoopSubComp() = 0;
						TempAirSysSubCompToPlant.PlantLoopType() = 0;
						TempAirSysSubCompToPlant.PlantLoopNum() = 0;
						TempAirSysSubCompToPlant.PlantLoopBranch() = 0;
						TempAirSysSubCompToPlant.PlantLoopComp() = 0;
						TempAirSysSubCompToPlant.FirstDemandSidePtr() = 0;
						TempAirSysSubCompToPlant.LastDemandSidePtr() = 0;

						ArrayCount = 0;
						for ( Idx = 1; Idx <= SubCompNum; ++Idx ) {
							Duplicate = false;
							for ( TempIndex = 1; TempIndex <= SubCompNum; ++TempIndex ) {
								if ( AirSysSubCompToPlant( Idx ).AirLoopNum == TempAirSysSubCompToPlant( TempIndex ).AirLoopNum && AirSysSubCompToPlant( Idx ).AirLoopBranch == TempAirSysSubCompToPlant( TempIndex ).AirLoopBranch && AirSysSubCompToPlant( Idx ).AirLoopComp == TempAirSysSubCompToPlant( TempIndex ).AirLoopComp && AirSysSubCompToPlant( Idx ).AirLoopSubComp == TempAirSysSubCompToPlant( TempIndex ).AirLoopSubComp ) {
									Duplicate = true;
									break;
								}
							}
							if ( ! Duplicate ) {
								++ArrayCount;
								TempAirSysSubCompToPlant( ArrayCount ).AirLoopNum = AirSysSubCompToPlant( Idx ).AirLoopNum;
								TempAirSysSubCompToPlant( ArrayCount ).AirLoopBranch = AirSysSubCompToPlant( Idx ).AirLoopBranch;
								TempAirSysSubCompToPlant( ArrayCount ).AirLoopComp = AirSysSubCompToPlant( Idx ).AirLoopComp;
								TempAirSysSubCompToPlant( ArrayCount ).AirLoopSubComp = AirSysSubCompToPlant( Idx ).AirLoopSubComp;
								TempAirSysSubCompToPlant( ArrayCount ).PlantLoopType = AirSysSubCompToPlant( Idx ).PlantLoopType;
								TempAirSysSubCompToPlant( ArrayCount ).PlantLoopNum = AirSysSubCompToPlant( Idx ).PlantLoopNum;
								TempAirSysSubCompToPlant( ArrayCount ).PlantLoopBranch = AirSysSubCompToPlant( Idx ).PlantLoopBranch;
								TempAirSysSubCompToPlant( ArrayCount ).PlantLoopComp = AirSysSubCompToPlant( Idx ).PlantLoopComp;
								TempAirSysSubCompToPlant( ArrayCount ).FirstDemandSidePtr = AirSysSubCompToPlant( Idx ).FirstDemandSidePtr;
								TempAirSysSubCompToPlant( ArrayCount ).LastDemandSidePtr = AirSysSubCompToPlant( Idx ).LastDemandSidePtr;
							}
						}

						AirSysSubCompToPlant.AirLoopNum() = TempAirSysSubCompToPlant.AirLoopNum();
						AirSysSubCompToPlant.AirLoopBranch() = TempAirSysSubCompToPlant.AirLoopBranch();
						AirSysSubCompToPlant.AirLoopComp() = TempAirSysSubCompToPlant.AirLoopComp();
						AirSysSubCompToPlant.AirLoopSubComp() = TempAirSysSubCompToPlant.AirLoopSubComp();
						AirSysSubCompToPlant.PlantLoopType() = TempAirSysSubCompToPlant.PlantLoopType();
						AirSysSubCompToPlant.PlantLoopNum() = TempAirSysSubCompToPlant.PlantLoopNum();
						AirSysSubCompToPlant.PlantLoopBranch() = TempAirSysSubCompToPlant.PlantLoopBranch();
						AirSysSubCompToPlant.PlantLoopComp() = TempAirSysSubCompToPlant.PlantLoopComp();
						AirSysSubCompToPlant.FirstDemandSidePtr() = TempAirSysSubCompToPlant.FirstDemandSidePtr();
						AirSysSubCompToPlant.LastDemandSidePtr() = TempAirSysSubCompToPlant.LastDemandSidePtr();

					}

					if ( SubSubCompNum > 0 ) {
						TempAirSysSubSubCompToPlant.AirLoopNum() = 0;
						TempAirSysSubSubCompToPlant.AirLoopBranch() = 0;
						TempAirSysSubSubCompToPlant.AirLoopComp() = 0;
						TempAirSysSubSubCompToPlant.AirLoopSubComp() = 0;
						TempAirSysSubSubCompToPlant.AirLoopSubSubComp() = 0;
						TempAirSysSubSubCompToPlant.PlantLoopType() = 0;
						TempAirSysSubSubCompToPlant.PlantLoopNum() = 0;
						TempAirSysSubSubCompToPlant.PlantLoopBranch() = 0;
						TempAirSysSubSubCompToPlant.PlantLoopComp() = 0;
						TempAirSysSubSubCompToPlant.FirstDemandSidePtr() = 0;
						TempAirSysSubSubCompToPlant.LastDemandSidePtr() = 0;

						ArrayCount = 0;
						for ( Idx = 1; Idx <= SubSubCompNum; ++Idx ) {
							Duplicate = false;
							for ( TempIndex = 1; TempIndex <= SubSubCompNum; ++TempIndex ) {
								if ( AirSysSubSubCompToPlant( Idx ).AirLoopNum == TempAirSysSubSubCompToPlant( TempIndex ).AirLoopNum && AirSysSubSubCompToPlant( Idx ).AirLoopBranch == TempAirSysSubSubCompToPlant( TempIndex ).AirLoopBranch && AirSysSubSubCompToPlant( Idx ).AirLoopComp == TempAirSysSubSubCompToPlant( TempIndex ).AirLoopComp && AirSysSubSubCompToPlant( Idx ).AirLoopSubComp == TempAirSysSubSubCompToPlant( TempIndex ).AirLoopSubComp && AirSysSubSubCompToPlant( Idx ).AirLoopSubSubComp == TempAirSysSubSubCompToPlant( TempIndex ).AirLoopSubSubComp ) {
									Duplicate = true;
									break;
								}
							}
							if ( ! Duplicate ) {
								++ArrayCount;
								TempAirSysSubSubCompToPlant( ArrayCount ).AirLoopNum = AirSysSubSubCompToPlant( Idx ).AirLoopNum;
								TempAirSysSubSubCompToPlant( ArrayCount ).AirLoopBranch = AirSysSubSubCompToPlant( Idx ).AirLoopBranch;
								TempAirSysSubSubCompToPlant( ArrayCount ).AirLoopComp = AirSysSubSubCompToPlant( Idx ).AirLoopComp;
								TempAirSysSubSubCompToPlant( ArrayCount ).AirLoopSubComp = AirSysSubSubCompToPlant( Idx ).AirLoopSubComp;
								TempAirSysSubSubCompToPlant( ArrayCount ).AirLoopSubSubComp = AirSysSubSubCompToPlant( Idx ).AirLoopSubSubComp;
								TempAirSysSubSubCompToPlant( ArrayCount ).PlantLoopType = AirSysSubSubCompToPlant( Idx ).PlantLoopType;
								TempAirSysSubSubCompToPlant( ArrayCount ).PlantLoopNum = AirSysSubSubCompToPlant( Idx ).PlantLoopNum;
								TempAirSysSubSubCompToPlant( ArrayCount ).PlantLoopBranch = AirSysSubSubCompToPlant( Idx ).PlantLoopBranch;
								TempAirSysSubSubCompToPlant( ArrayCount ).PlantLoopComp = AirSysSubSubCompToPlant( Idx ).PlantLoopComp;
								TempAirSysSubSubCompToPlant( ArrayCount ).FirstDemandSidePtr = AirSysSubSubCompToPlant( Idx ).FirstDemandSidePtr;
								TempAirSysSubSubCompToPlant( ArrayCount ).LastDemandSidePtr = AirSysSubSubCompToPlant( Idx ).LastDemandSidePtr;
							}
						}

						AirSysSubSubCompToPlant.AirLoopNum() = TempAirSysSubSubCompToPlant.AirLoopNum();
						AirSysSubSubCompToPlant.AirLoopBranch() = TempAirSysSubSubCompToPlant.AirLoopBranch();
						AirSysSubSubCompToPlant.AirLoopComp() = TempAirSysSubSubCompToPlant.AirLoopComp();
						AirSysSubSubCompToPlant.AirLoopSubComp() = TempAirSysSubSubCompToPlant.AirLoopSubComp();
						AirSysSubSubCompToPlant.AirLoopSubSubComp() = TempAirSysSubSubCompToPlant.AirLoopSubSubComp();
						AirSysSubSubCompToPlant.PlantLoopType() = TempAirSysSubSubCompToPlant.PlantLoopType();
						AirSysSubSubCompToPlant.PlantLoopNum() = TempAirSysSubSubCompToPlant.PlantLoopNum();
						AirSysSubSubCompToPlant.PlantLoopBranch() = TempAirSysSubSubCompToPlant.PlantLoopBranch();
						AirSysSubSubCompToPlant.PlantLoopComp() = TempAirSysSubSubCompToPlant.PlantLoopComp();
						AirSysSubSubCompToPlant.FirstDemandSidePtr() = TempAirSysSubSubCompToPlant.FirstDemandSidePtr();
						AirSysSubSubCompToPlant.LastDemandSidePtr() = TempAirSysSubSubCompToPlant.LastDemandSidePtr();

					}

					// 2. Find Supply Side loop for every demand side component
					//The demand side components only need to know what supply side loop
					//they are connected to.  The input and plant data structure will
					//force the loop numbers to be the same.

					// 3. Find Demand Side Component Corresponding to Supply Side Component
					for ( PlantLoopNum = 1; PlantLoopNum <= NumPlantLoops; ++PlantLoopNum ) {
						for ( BranchNum = 1; BranchNum <= VentRepPlantSupplySide( PlantLoopNum ).TotalBranches; ++BranchNum ) {
							for ( CompNum = 1; CompNum <= VentRepPlantSupplySide( PlantLoopNum ).Branch( BranchNum ).TotalComponents; ++CompNum ) {
								CompType = VentRepPlantSupplySide( PlantLoopNum ).Branch( BranchNum ).Comp( CompNum ).TypeOf;
								CompName = VentRepPlantSupplySide( PlantLoopNum ).Branch( BranchNum ).Comp( CompNum ).Name;
								FindDemandSideMatch( CompType, CompName, MatchFound, MatchLoopType, MatchLoop, MatchBranch, MatchComp );
								VentRepPlantSupplySide( PlantLoopNum ).Branch( BranchNum ).Comp( CompNum ).ConnectPlant.LoopType = MatchLoopType;
								VentRepPlantSupplySide( PlantLoopNum ).Branch( BranchNum ).Comp( CompNum ).ConnectPlant.LoopNum = MatchLoop;
								VentRepPlantSupplySide( PlantLoopNum ).Branch( BranchNum ).Comp( CompNum ).ConnectPlant.BranchNum = MatchBranch;
								VentRepPlantSupplySide( PlantLoopNum ).Branch( BranchNum ).Comp( CompNum ).ConnectPlant.CompNum = MatchComp;
							}
						}
					}

					for ( PlantLoopNum = 1; PlantLoopNum <= NumCondLoops; ++PlantLoopNum ) {
						for ( BranchNum = 1; BranchNum <= VentRepCondSupplySide( PlantLoopNum ).TotalBranches; ++BranchNum ) {
							for ( CompNum = 1; CompNum <= VentRepCondSupplySide( PlantLoopNum ).Branch( BranchNum ).TotalComponents; ++CompNum ) {
								CompType = VentRepCondSupplySide( PlantLoopNum ).Branch( BranchNum ).Comp( CompNum ).TypeOf;
								CompName = VentRepCondSupplySide( PlantLoopNum ).Branch( BranchNum ).Comp( CompNum ).Name;
								FindDemandSideMatch( CompType, CompName, MatchFound, MatchLoopType, MatchLoop, MatchBranch, MatchComp );
								VentRepCondSupplySide( PlantLoopNum ).Branch( BranchNum ).Comp( CompNum ).ConnectPlant.LoopType = MatchLoopType;
								VentRepCondSupplySide( PlantLoopNum ).Branch( BranchNum ).Comp( CompNum ).ConnectPlant.LoopNum = MatchLoop;
								VentRepCondSupplySide( PlantLoopNum ).Branch( BranchNum ).Comp( CompNum ).ConnectPlant.BranchNum = MatchBranch;
								VentRepCondSupplySide( PlantLoopNum ).Branch( BranchNum ).Comp( CompNum ).ConnectPlant.CompNum = MatchComp;
							}
						}
					}
				}
			} // Controlled Zone Loop

			//4.  Now Load all of the plant supply/demand side connections in a single array with pointers from the
			//    connection arrays (ZoneCompToPlant, ZoneSubCompToPlant, ZoneSubSubCompToPlant, AirSysCompToPlant, etc.)
			if ( allocated( ZoneCompToPlant ) ) {
				NumZoneConnectComps = isize( ZoneCompToPlant );
			} else {
				NumZoneConnectComps = 0;
			}
			if ( allocated( ZoneSubCompToPlant ) ) {
				NumZoneConnectSubComps = isize( ZoneSubCompToPlant );
			} else {
				NumZoneConnectSubComps = 0;
			}
			if ( allocated( ZoneSubSubCompToPlant ) ) {
				NumZoneConnectSubSubComps = isize( ZoneSubSubCompToPlant );
			} else {
				NumZoneConnectSubSubComps = 0;
			}
			if ( allocated( AirSysCompToPlant ) ) {
				NumAirSysConnectComps = isize( AirSysCompToPlant );
			} else {
				NumAirSysConnectComps = 0;
			}
			if ( allocated( AirSysSubCompToPlant ) ) {
				NumAirSysConnectSubComps = isize( AirSysSubCompToPlant );
			} else {
				NumAirSysConnectSubComps = 0;
			}
			if ( allocated( AirSysSubSubCompToPlant ) ) {
				NumAirSysConnectSubSubComps = isize( AirSysSubSubCompToPlant );
			} else {
				NumAirSysConnectSubSubComps = 0;
			}
			OneTimeFlag = false;

			ArrayCount = 0;
			for ( CompNum = 1; CompNum <= NumZoneConnectComps; ++CompNum ) {
				LoopType = ZoneCompToPlant( CompNum ).PlantLoopType;
				LoopNum = ZoneCompToPlant( CompNum ).PlantLoopNum;
				FirstIndex = ArrayCount + 1;
				LoopCount = 1;

				if ( LoopType > 0 && LoopNum > 0 ) {
					FindFirstLastPtr( LoopType, LoopNum, ArrayCount, LoopCount, ConnectionFlag );
				}

				LastIndex = ArrayCount;
				if ( FirstIndex > LastIndex ) FirstIndex = LastIndex;
				if ( ConnectionFlag ) {
					ZoneCompToPlant( CompNum ).FirstDemandSidePtr = FirstIndex;
					ZoneCompToPlant( CompNum ).LastDemandSidePtr = LastIndex;
				}
			}

			for ( SubCompNum = 1; SubCompNum <= NumZoneConnectSubComps; ++SubCompNum ) {
				LoopType = ZoneSubCompToPlant( SubCompNum ).PlantLoopType;
				LoopNum = ZoneSubCompToPlant( SubCompNum ).PlantLoopNum;
				FirstIndex = ArrayCount + 1;
				LoopCount = 1;

				if ( LoopType > 0 && LoopNum > 0 ) {
					FindFirstLastPtr( LoopType, LoopNum, ArrayCount, LoopCount, ConnectionFlag );
				}

				LastIndex = ArrayCount;
				if ( FirstIndex > LastIndex ) FirstIndex = LastIndex;
				if ( ConnectionFlag ) {
					ZoneSubCompToPlant( SubCompNum ).FirstDemandSidePtr = FirstIndex;
					ZoneSubCompToPlant( SubCompNum ).LastDemandSidePtr = LastIndex;
				}
			}

			for ( SubSubCompNum = 1; SubSubCompNum <= NumZoneConnectSubSubComps; ++SubSubCompNum ) {
				LoopType = ZoneSubSubCompToPlant( SubSubCompNum ).PlantLoopType;
				LoopNum = ZoneSubSubCompToPlant( SubSubCompNum ).PlantLoopNum;
				FirstIndex = ArrayCount + 1;
				LoopCount = 1;

				if ( LoopType > 0 && LoopNum > 0 ) {
					FindFirstLastPtr( LoopType, LoopNum, ArrayCount, LoopCount, ConnectionFlag );
				}

				LastIndex = ArrayCount;
				if ( FirstIndex > LastIndex ) FirstIndex = LastIndex;
				if ( ConnectionFlag ) {
					ZoneSubSubCompToPlant( SubSubCompNum ).FirstDemandSidePtr = FirstIndex;
					ZoneSubSubCompToPlant( SubSubCompNum ).LastDemandSidePtr = LastIndex;
				}
			}
			for ( CompNum = 1; CompNum <= NumAirSysConnectComps; ++CompNum ) {
				LoopType = AirSysCompToPlant( CompNum ).PlantLoopType;
				LoopNum = AirSysCompToPlant( CompNum ).PlantLoopNum;
				FirstIndex = ArrayCount + 1;
				LoopCount = 1;

				if ( LoopType > 0 && LoopNum > 0 ) {
					FindFirstLastPtr( LoopType, LoopNum, ArrayCount, LoopCount, ConnectionFlag );
				}

				LastIndex = ArrayCount;
				if ( FirstIndex > LastIndex ) FirstIndex = LastIndex;
				if ( ConnectionFlag ) {
					AirSysCompToPlant( CompNum ).FirstDemandSidePtr = FirstIndex;
					AirSysCompToPlant( CompNum ).LastDemandSidePtr = LastIndex;
				}
			}

			for ( SubCompNum = 1; SubCompNum <= NumAirSysConnectSubComps; ++SubCompNum ) {
				LoopType = AirSysSubCompToPlant( SubCompNum ).PlantLoopType;
				LoopNum = AirSysSubCompToPlant( SubCompNum ).PlantLoopNum;
				FirstIndex = ArrayCount + 1;
				LoopCount = 1;

				if ( LoopType > 0 && LoopNum > 0 ) {
					FindFirstLastPtr( LoopType, LoopNum, ArrayCount, LoopCount, ConnectionFlag );
				}

				LastIndex = ArrayCount;
				if ( FirstIndex > LastIndex ) FirstIndex = LastIndex;
				if ( ConnectionFlag ) {
					AirSysSubCompToPlant( SubCompNum ).FirstDemandSidePtr = FirstIndex;
					AirSysSubCompToPlant( SubCompNum ).LastDemandSidePtr = LastIndex;
				}
			}

			for ( SubSubCompNum = 1; SubSubCompNum <= NumAirSysConnectSubSubComps; ++SubSubCompNum ) {
				LoopType = AirSysSubSubCompToPlant( SubSubCompNum ).PlantLoopType;
				LoopNum = AirSysSubSubCompToPlant( SubSubCompNum ).PlantLoopNum;
				FirstIndex = ArrayCount + 1;
				LoopCount = 1;

				if ( LoopType > 0 && LoopNum > 0 ) {
					FindFirstLastPtr( LoopType, LoopNum, ArrayCount, LoopCount, ConnectionFlag );
				}

				LastIndex = ArrayCount;
				if ( FirstIndex > LastIndex ) FirstIndex = LastIndex;
				if ( ConnectionFlag ) {
					AirSysSubSubCompToPlant( SubSubCompNum ).FirstDemandSidePtr = FirstIndex;
					AirSysSubSubCompToPlant( SubSubCompNum ).LastDemandSidePtr = LastIndex;
				}
			}

			OneTimeFlag = false;

		}

		// On every iteration, load the air loop energy data
		for ( AirLoopNum = 1; AirLoopNum <= NumPrimaryAirSys; ++AirLoopNum ) {
			for ( BranchNum = 1; BranchNum <= PrimaryAirSystem( AirLoopNum ).NumBranches; ++BranchNum ) {
				for ( CompNum = 1; CompNum <= PrimaryAirSystem( AirLoopNum ).Branch( BranchNum ).TotalComponents; ++CompNum ) {
					for ( VarNum = 1; VarNum <= PrimaryAirSystem( AirLoopNum ).Branch( BranchNum ).Comp( CompNum ).NumMeteredVars; ++VarNum ) {
						VarType = PrimaryAirSystem( AirLoopNum ).Branch( BranchNum ).Comp( CompNum ).MeteredVar( VarNum ).ReportVarType;
						VarIndex = PrimaryAirSystem( AirLoopNum ).Branch( BranchNum ).Comp( CompNum ).MeteredVar( VarNum ).ReportVarIndex;
						PrimaryAirSystem( AirLoopNum ).Branch( BranchNum ).Comp( CompNum ).MeteredVar( VarNum ).CurMeterReading = GetInternalVariableValue( VarType, VarIndex );
					}
					for ( SubCompNum = 1; SubCompNum <= PrimaryAirSystem( AirLoopNum ).Branch( BranchNum ).Comp( CompNum ).NumSubComps; ++SubCompNum ) {
						for ( VarNum = 1; VarNum <= PrimaryAirSystem( AirLoopNum ).Branch( BranchNum ).Comp( CompNum ).SubComp( SubCompNum ).NumMeteredVars; ++VarNum ) {
							VarType = PrimaryAirSystem( AirLoopNum ).Branch( BranchNum ).Comp( CompNum ).SubComp( SubCompNum ).MeteredVar( VarNum ).ReportVarType;
							VarIndex = PrimaryAirSystem( AirLoopNum ).Branch( BranchNum ).Comp( CompNum ).SubComp( SubCompNum ).MeteredVar( VarNum ).ReportVarIndex;
							PrimaryAirSystem( AirLoopNum ).Branch( BranchNum ).Comp( CompNum ).SubComp( SubCompNum ).MeteredVar( VarNum ).CurMeterReading = GetInternalVariableValue( VarType, VarIndex );
						}
						for ( SubSubCompNum = 1; SubSubCompNum <= PrimaryAirSystem( AirLoopNum ).Branch( BranchNum ).Comp( CompNum ).SubComp( SubCompNum ).NumSubSubComps; ++SubSubCompNum ) {
							for ( VarNum = 1; VarNum <= PrimaryAirSystem( AirLoopNum ).Branch( BranchNum ).Comp( CompNum ).SubComp( SubCompNum ).SubSubComp( SubSubCompNum ).NumMeteredVars; ++VarNum ) {
								VarType = PrimaryAirSystem( AirLoopNum ).Branch( BranchNum ).Comp( CompNum ).SubComp( SubCompNum ).SubSubComp( SubSubCompNum ).MeteredVar( VarNum ).ReportVarType;
								VarIndex = PrimaryAirSystem( AirLoopNum ).Branch( BranchNum ).Comp( CompNum ).SubComp( SubCompNum ).SubSubComp( SubSubCompNum ).MeteredVar( VarNum ).ReportVarIndex;
								PrimaryAirSystem( AirLoopNum ).Branch( BranchNum ).Comp( CompNum ).SubComp( SubCompNum ).SubSubComp( SubSubCompNum ).MeteredVar( VarNum ).CurMeterReading = GetInternalVariableValue( VarType, VarIndex );
							}
						}
					}
				}
			}
		}

		// On every iteration, load the zone equipment energy data
		for ( ListNum = 1; ListNum <= NumOfZones; ++ListNum ) {
			if ( ! ZoneEquipConfig( ListNum ).IsControlled ) continue;
			for ( CompNum = 1; CompNum <= ZoneEquipList( ListNum ).NumOfEquipTypes; ++CompNum ) {
				for ( VarNum = 1; VarNum <= ZoneEquipList( ListNum ).EquipData( CompNum ).NumMeteredVars; ++VarNum ) {
					VarType = ZoneEquipList( ListNum ).EquipData( CompNum ).MeteredVar( VarNum ).ReportVarType;
					VarIndex = ZoneEquipList( ListNum ).EquipData( CompNum ).MeteredVar( VarNum ).ReportVarIndex;
					ZoneEquipList( ListNum ).EquipData( CompNum ).MeteredVar( VarNum ).CurMeterReading = GetInternalVariableValue( VarType, VarIndex );
				}
				for ( SubCompNum = 1; SubCompNum <= ZoneEquipList( ListNum ).EquipData( CompNum ).NumSubEquip; ++SubCompNum ) {
					for ( VarNum = 1; VarNum <= ZoneEquipList( ListNum ).EquipData( CompNum ).SubEquipData( SubCompNum ).NumMeteredVars; ++VarNum ) {
						VarType = ZoneEquipList( ListNum ).EquipData( CompNum ).SubEquipData( SubCompNum ).MeteredVar( VarNum ).ReportVarType;
						VarIndex = ZoneEquipList( ListNum ).EquipData( CompNum ).SubEquipData( SubCompNum ).MeteredVar( VarNum ).ReportVarIndex;
						ZoneEquipList( ListNum ).EquipData( CompNum ).SubEquipData( SubCompNum ).MeteredVar( VarNum ).CurMeterReading = GetInternalVariableValue( VarType, VarIndex );
					}
					for ( SubSubCompNum = 1; SubSubCompNum <= ZoneEquipList( ListNum ).EquipData( CompNum ).SubEquipData( SubCompNum ).NumSubSubEquip; ++SubSubCompNum ) {
						for ( VarNum = 1; VarNum <= ZoneEquipList( ListNum ).EquipData( CompNum ).SubEquipData( SubCompNum ).SubSubEquipData( SubSubCompNum ).NumMeteredVars; ++VarNum ) {
							VarType = ZoneEquipList( ListNum ).EquipData( CompNum ).SubEquipData( SubCompNum ).SubSubEquipData( SubSubCompNum ).MeteredVar( VarNum ).ReportVarType;
							VarIndex = ZoneEquipList( ListNum ).EquipData( CompNum ).SubEquipData( SubCompNum ).SubSubEquipData( SubSubCompNum ).MeteredVar( VarNum ).ReportVarIndex;
							ZoneEquipList( ListNum ).EquipData( CompNum ).SubEquipData( SubCompNum ).SubSubEquipData( SubSubCompNum ).MeteredVar( VarNum ).CurMeterReading = GetInternalVariableValue( VarType, VarIndex ); //Sankar Corrected zone array
						}
					}
				}
			}
		}

		// On every iteration, load the Plant Supply Side Data
		for ( PlantLoopNum = 1; PlantLoopNum <= NumPlantLoops; ++PlantLoopNum ) {
			for ( BranchNum = 1; BranchNum <= VentRepPlantSupplySide( PlantLoopNum ).TotalBranches; ++BranchNum ) {
				for ( CompNum = 1; CompNum <= VentRepPlantSupplySide( PlantLoopNum ).Branch( BranchNum ).TotalComponents; ++CompNum ) {
					for ( VarNum = 1; VarNum <= VentRepPlantSupplySide( PlantLoopNum ).Branch( BranchNum ).Comp( CompNum ).NumMeteredVars; ++VarNum ) {
						VarType = VentRepPlantSupplySide( PlantLoopNum ).Branch( BranchNum ).Comp( CompNum ).MeteredVar( VarNum ).ReportVarType;
						VarIndex = VentRepPlantSupplySide( PlantLoopNum ).Branch( BranchNum ).Comp( CompNum ).MeteredVar( VarNum ).ReportVarIndex;
						VentRepPlantSupplySide( PlantLoopNum ).Branch( BranchNum ).Comp( CompNum ).MeteredVar( VarNum ).CurMeterReading = GetInternalVariableValue( VarType, VarIndex );
					}
				}
			}
		}

		// On every iteration, load the Plant Demand Side Data
		for ( PlantLoopNum = 1; PlantLoopNum <= NumPlantLoops; ++PlantLoopNum ) {
			for ( BranchNum = 1; BranchNum <= VentRepPlantDemandSide( PlantLoopNum ).TotalBranches; ++BranchNum ) {
				for ( CompNum = 1; CompNum <= VentRepPlantDemandSide( PlantLoopNum ).Branch( BranchNum ).TotalComponents; ++CompNum ) {
					for ( VarNum = 1; VarNum <= VentRepPlantDemandSide( PlantLoopNum ).Branch( BranchNum ).Comp( CompNum ).NumMeteredVars; ++VarNum ) {
						VarType = VentRepPlantDemandSide( PlantLoopNum ).Branch( BranchNum ).Comp( CompNum ).MeteredVar( VarNum ).ReportVarType;
						VarIndex = VentRepPlantDemandSide( PlantLoopNum ).Branch( BranchNum ).Comp( CompNum ).MeteredVar( VarNum ).ReportVarIndex;
						VentRepPlantDemandSide( PlantLoopNum ).Branch( BranchNum ).Comp( CompNum ).MeteredVar( VarNum ).CurMeterReading = GetInternalVariableValue( VarType, VarIndex );
					}
				}
			}
		}

		// On every iteration, load the Condenser Supply Side Data
		for ( PlantLoopNum = 1; PlantLoopNum <= NumCondLoops; ++PlantLoopNum ) {
			for ( BranchNum = 1; BranchNum <= VentRepCondSupplySide( PlantLoopNum ).TotalBranches; ++BranchNum ) {
				for ( CompNum = 1; CompNum <= VentRepCondSupplySide( PlantLoopNum ).Branch( BranchNum ).TotalComponents; ++CompNum ) {
					for ( VarNum = 1; VarNum <= VentRepCondSupplySide( PlantLoopNum ).Branch( BranchNum ).Comp( CompNum ).NumMeteredVars; ++VarNum ) {
						VarType = VentRepCondSupplySide( PlantLoopNum ).Branch( BranchNum ).Comp( CompNum ).MeteredVar( VarNum ).ReportVarType;
						VarIndex = VentRepCondSupplySide( PlantLoopNum ).Branch( BranchNum ).Comp( CompNum ).MeteredVar( VarNum ).ReportVarIndex;
						VentRepCondSupplySide( PlantLoopNum ).Branch( BranchNum ).Comp( CompNum ).MeteredVar( VarNum ).CurMeterReading = GetInternalVariableValue( VarType, VarIndex );
					}
				}
			}
		}

		// On every iteration, load the Condenser Demand Side Data
		for ( PlantLoopNum = 1; PlantLoopNum <= NumCondLoops; ++PlantLoopNum ) {
			for ( BranchNum = 1; BranchNum <= VentRepCondDemandSide( PlantLoopNum ).TotalBranches; ++BranchNum ) {
				for ( CompNum = 1; CompNum <= VentRepCondDemandSide( PlantLoopNum ).Branch( BranchNum ).TotalComponents; ++CompNum ) {
					for ( VarNum = 1; VarNum <= VentRepCondDemandSide( PlantLoopNum ).Branch( BranchNum ).Comp( CompNum ).NumMeteredVars; ++VarNum ) {
						VarType = VentRepCondDemandSide( PlantLoopNum ).Branch( BranchNum ).Comp( CompNum ).MeteredVar( VarNum ).ReportVarType;
						VarIndex = VentRepCondDemandSide( PlantLoopNum ).Branch( BranchNum ).Comp( CompNum ).MeteredVar( VarNum ).ReportVarIndex;
						VentRepCondDemandSide( PlantLoopNum ).Branch( BranchNum ).Comp( CompNum ).MeteredVar( VarNum ).CurMeterReading = GetInternalVariableValue( VarType, VarIndex );
					}
				}
			}
		}

		// initialize energy report variables

	}

	void
	FindFirstLastPtr(
		int & LoopType,
		int & LoopNum,
		int & ArrayCount,
		int & LoopCount,
		bool & ConnectionFlag
	)
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         Dan Fisher
		//       DATE WRITTEN   July 2005
		//       MODIFIED
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Initializes the energy components of the data structures

		// METHODOLOGY EMPLOYED:
		// Once all compsets have been established (second iteration) find all components
		// subcomponents, etc.

		// REFERENCES:
		// na

		// USE STATEMENTS:

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS

		// SUBROUTINE PARAMETER DEFINITIONS:
		int const TypeComp( 1 );
		int const TypeSubComp( 2 );
		int const TypeSubSubComp( 3 );
		int const EnergyTransfer( 1 );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		int BranchNum;
		int Idx;
		int DemandSideLoopNum;
		int DemandSideBranchNum;
		int DemandSideCompNum;
		int SupplySideCompNum;
		int DemandSideLoopType;
		static bool OneTimeFlag( true ); // Flag set to make sure you initialize reports one time
		bool found;
//		int countloop;

		struct IdentifyLoop
		{
			// Members
			int LoopNum;
			int LoopType;

			// Default Constructor
			IdentifyLoop() :
				LoopNum( 0 ),
				LoopType( 0 )
			{}

			// Member Constructor
			IdentifyLoop(
				int const LoopNum,
				int const LoopType
			) :
				LoopNum( LoopNum ),
				LoopType( LoopType )
			{}

		};

		// Object Data
		static FArray1D< IdentifyLoop > LoopStack;
		static FArray1D< IdentifyLoop > TempLoopStack;

		return; //Autodesk:? Is this routine now an intentional NOOP?

		if ( OneTimeFlag ) {
			LoopStack.allocate( MaxLoopArraySize );
			TempLoopStack.allocate( MaxLoopArraySize );
			DemandSideConnect.allocate( MaxCompArraySize );

			OneTimeFlag = false;

		}
		LoopStack.LoopNum() = 0;
		LoopStack.LoopType() = 0;

		TempLoopStack.LoopNum() = 0;
		TempLoopStack.LoopType() = 0;
		ConnectionFlag = false;
		//    countloop=0
		//    write(outputfiledebug,*) '1228=lt,lc,lnum,cflag,arrcnt',looptype,loopcount,LoopNum,connectionflag,arraycount

		while ( LoopCount > 0 ) {
			//        write(outputfiledebug,*) '1231==lt,lc,lnum,cflag,arrcnt',looptype,loopcount,LoopNum,connectionflag,arraycount
			//        write(outputfiledebug,*) 'loop=plname',TRIM(plantloop(LoopNum)%name)
			--LoopCount;
			//        countloop=countloop+1
			//        if (countloop > 100) exit
			if ( LoopType == 1 ) {
				for ( BranchNum = 1; BranchNum <= VentRepPlantSupplySide( LoopNum ).TotalBranches; ++BranchNum ) {
					for ( SupplySideCompNum = 1; SupplySideCompNum <= VentRepPlantSupplySide( LoopNum ).Branch( BranchNum ).TotalComponents; ++SupplySideCompNum ) {
						DemandSideLoopType = VentRepPlantSupplySide( LoopNum ).Branch( BranchNum ).Comp( SupplySideCompNum ).ConnectPlant.LoopType;
						DemandSideLoopNum = VentRepPlantSupplySide( LoopNum ).Branch( BranchNum ).Comp( SupplySideCompNum ).ConnectPlant.LoopNum;
						DemandSideBranchNum = VentRepPlantSupplySide( LoopNum ).Branch( BranchNum ).Comp( SupplySideCompNum ).ConnectPlant.BranchNum;
						DemandSideCompNum = VentRepPlantSupplySide( LoopNum ).Branch( BranchNum ).Comp( SupplySideCompNum ).ConnectPlant.CompNum;

						//If the connection is valid load the connection array
						if ( DemandSideLoopType == 1 || DemandSideLoopType == 2 ) {
							ConnectionFlag = true;
							++ArrayCount;
							if ( ArrayCount > MaxCompArraySize ) {
								//                  ALLOCATE(TempDemandSideConnect(MaxCompArraySize*2))
								TempDemandSideConnect.allocate( MaxCompArraySize + 100 );
								TempDemandSideConnect( {1,MaxCompArraySize} ) = DemandSideConnect( {1,MaxCompArraySize} );
								DemandSideConnect.deallocate();
								DemandSideConnect.allocate( MaxCompArraySize * 2 );
								DemandSideConnect( {1,MaxCompArraySize} ) = TempDemandSideConnect( {1,MaxCompArraySize} );
								TempDemandSideConnect.deallocate();
								//                  MaxCompArraySize=MaxCompArraySize*2
								MaxCompArraySize += 100;
							}
							DemandSideConnect( ArrayCount ).LoopType = DemandSideLoopType;
							DemandSideConnect( ArrayCount ).LoopNum = DemandSideLoopNum;
							DemandSideConnect( ArrayCount ).BranchNum = DemandSideBranchNum;
							DemandSideConnect( ArrayCount ).CompNum = DemandSideCompNum;

							found = false;
							gio::write( OutputFileDebug, fmtLD ) << "1271=lstacksize" << size( LoopStack );
							for ( Idx = 1; Idx <= isize( LoopStack ); ++Idx ) {
								if ( DemandSideLoopNum == LoopStack( Idx ).LoopNum && DemandSideLoopType == LoopStack( Idx ).LoopType ) {
									found = true;
									break;
								}
							}
							if ( ! found ) {
								++LoopCount;
								//       write(outputfiledebug,*) '1280=lc,mxsize',loopcount,maxlooparraysize
								//       write(outputfiledebug,*) '1281=dsloopnum,dslooptype',DemandSideLoopNum,DemandSideLoopType
								if ( LoopCount > MaxLoopArraySize ) {
									//                    ALLOCATE(TempLoopStack(MaxLoopArraySize*2))
									TempLoopStack.allocate( MaxLoopArraySize + 100 );
									TempLoopStack( {1,MaxLoopArraySize} ) = LoopStack( {1,MaxLoopArraySize} );
									LoopStack.deallocate();
									//                    ALLOCATE(LoopStack(MaxLoopArraySize*2))
									LoopStack.allocate( MaxLoopArraySize + 100 );
									LoopStack( {1,MaxLoopArraySize} ) = TempLoopStack( {1,MaxLoopArraySize} );
									TempLoopStack.deallocate();
									//                    MaxLoopArraySize=MaxLoopArraySize*2
									MaxLoopArraySize += 100;
								}
								//               write(outputfiledebug,*) '1294=lcnt,dsloopnum,dslooptype',loopcount,DemandSideLoopNum,DemandSideLoopType
								LoopStack( LoopCount ).LoopNum = DemandSideLoopNum;
								LoopStack( LoopCount ).LoopType = DemandSideLoopType;
							}
						}
					}
				}
			} else if ( LoopType == 2 ) {
				for ( BranchNum = 1; BranchNum <= VentRepCondSupplySide( LoopNum ).TotalBranches; ++BranchNum ) {
					for ( SupplySideCompNum = 1; SupplySideCompNum <= VentRepCondSupplySide( LoopNum ).Branch( BranchNum ).TotalComponents; ++SupplySideCompNum ) {
						DemandSideLoopType = VentRepCondSupplySide( LoopNum ).Branch( BranchNum ).Comp( SupplySideCompNum ).ConnectPlant.LoopType;
						DemandSideLoopNum = VentRepCondSupplySide( LoopNum ).Branch( BranchNum ).Comp( SupplySideCompNum ).ConnectPlant.LoopNum;
						DemandSideBranchNum = VentRepCondSupplySide( LoopNum ).Branch( BranchNum ).Comp( SupplySideCompNum ).ConnectPlant.BranchNum;
						DemandSideCompNum = VentRepCondSupplySide( LoopNum ).Branch( BranchNum ).Comp( SupplySideCompNum ).ConnectPlant.CompNum;

						//If the connection is valid load the connection array
						if ( DemandSideLoopType == 1 || DemandSideLoopType == 2 ) {
							ConnectionFlag = true;
							++ArrayCount;
							if ( ArrayCount > MaxCompArraySize ) {
								//                  ALLOCATE(TempDemandSideConnect(MaxCompArraySize*2))
								TempDemandSideConnect.allocate( MaxCompArraySize + 100 );
								TempDemandSideConnect( {1,MaxCompArraySize} ) = DemandSideConnect( {1,MaxCompArraySize} );
								DemandSideConnect.deallocate();
								//                  ALLOCATE(DemandSideConnect(MaxCompArraySize*2))
								DemandSideConnect.allocate( MaxCompArraySize + 100 );
								DemandSideConnect( {1,MaxCompArraySize} ) = TempDemandSideConnect( {1,MaxCompArraySize} );
								TempDemandSideConnect.deallocate();
								//                  MaxCompArraySize=MaxCompArraySize*2
								MaxCompArraySize += 100;
							}
							DemandSideConnect( ArrayCount ).LoopType = DemandSideLoopType;
							DemandSideConnect( ArrayCount ).LoopNum = DemandSideLoopNum;
							DemandSideConnect( ArrayCount ).BranchNum = DemandSideBranchNum;
							DemandSideConnect( ArrayCount ).CompNum = DemandSideCompNum;

							found = false;
							for ( Idx = 1; Idx <= isize( LoopStack ); ++Idx ) {
								if ( DemandSideLoopNum == LoopStack( Idx ).LoopNum && DemandSideLoopType == LoopStack( Idx ).LoopType ) {
									found = true;
									break;
								}
							}
							if ( ! found ) {
								++LoopCount;
								//       write(outputfiledebug,*) '1341=lcnt,arrsize',loopcount,maxlooparraysize
								//       write(outputfiledebug,*) '1342=lsloopnum,dslooptype',DemandSideLoopNum,DemandSideLoopType
								if ( LoopCount > MaxLoopArraySize ) {
									//                    ALLOCATE(TempLoopStack(MaxLoopArraySize*2))
									TempLoopStack.allocate( MaxLoopArraySize + 100 );
									TempLoopStack( {1,MaxLoopArraySize} ) = LoopStack( {1,MaxLoopArraySize} );
									LoopStack.deallocate();
									//                    ALLOCATE(LoopStack(MaxLoopArraySize*2))
									LoopStack.allocate( MaxLoopArraySize + 100 );
									LoopStack( {1,MaxLoopArraySize} ) = TempLoopStack( {1,MaxLoopArraySize} );
									TempLoopStack.deallocate();
									//                    MaxLoopArraySize=MaxLoopArraySize*2
									MaxLoopArraySize += 100;
								}
								LoopStack( LoopCount ).LoopNum = DemandSideLoopNum;
								LoopStack( LoopCount ).LoopType = DemandSideLoopType;
							}
						}
					}
				}
			} else {
				gio::write( OutputFileDebug, fmtLD ) << "1361=error";
				//error
			}

			//now unload the LoopNum and LoopType arrays
			if ( LoopCount > 0 ) {
				LoopType = LoopStack( LoopCount ).LoopType;
				LoopNum = LoopStack( LoopCount ).LoopNum;
			}

		} //While loop

	}

	void
	UpdateZoneCompPtrArray(
		int & Idx,
		int const ListNum,
		int const AirDistUnitNum,
		int const PlantLoopType,
		int const PlantLoop,
		int const PlantBranch,
		int const PlantComp
	)
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         Dan Fisher
		//       DATE WRITTEN   June 2005
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Update Zone Component pointers

		// METHODOLOGY EMPLOYED:
		// na

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
		static bool OneTimeFlag( true );
		int OldArrayLimit;
		static int ArrayLimit( 100 );
		static int ArrayCounter( 1 );

		if ( OneTimeFlag ) {
			ZoneCompToPlant.allocate( ArrayLimit );
			ZoneCompToPlant.ZoneEqListNum() = 0;
			ZoneCompToPlant.ZoneEqCompNum() = 0;
			ZoneCompToPlant.PlantLoopType() = 0;
			ZoneCompToPlant.PlantLoopNum() = 0;
			ZoneCompToPlant.PlantLoopBranch() = 0;
			ZoneCompToPlant.PlantLoopComp() = 0;
			ZoneCompToPlant.FirstDemandSidePtr() = 0;
			ZoneCompToPlant.LastDemandSidePtr() = 0;

			TempZoneCompToPlant.allocate( ArrayLimit );
			TempZoneCompToPlant.ZoneEqListNum() = 0;
			TempZoneCompToPlant.ZoneEqCompNum() = 0;
			TempZoneCompToPlant.PlantLoopType() = 0;
			TempZoneCompToPlant.PlantLoopNum() = 0;
			TempZoneCompToPlant.PlantLoopBranch() = 0;
			TempZoneCompToPlant.PlantLoopComp() = 0;
			TempZoneCompToPlant.FirstDemandSidePtr() = 0;
			TempZoneCompToPlant.LastDemandSidePtr() = 0;
			OneTimeFlag = false;
		}

		if ( ArrayCounter < ArrayLimit ) {
			Idx = ArrayCounter;
			ZoneCompToPlant( Idx ).ZoneEqListNum = ListNum;
			ZoneCompToPlant( Idx ).ZoneEqCompNum = AirDistUnitNum;
			ZoneCompToPlant( Idx ).PlantLoopType = PlantLoopType;
			ZoneCompToPlant( Idx ).PlantLoopNum = PlantLoop;
			ZoneCompToPlant( Idx ).PlantLoopBranch = PlantBranch;
			ZoneCompToPlant( Idx ).PlantLoopComp = PlantComp;
			++ArrayCounter;
		} else {
			TempZoneCompToPlant = ZoneCompToPlant;
			ZoneCompToPlant.deallocate();
			OldArrayLimit = ArrayLimit;
			ArrayLimit *= 2;
			ZoneCompToPlant.allocate( ArrayLimit );
			ZoneCompToPlant.ZoneEqListNum() = 0;
			ZoneCompToPlant.ZoneEqCompNum() = 0;
			ZoneCompToPlant.PlantLoopType() = 0;
			ZoneCompToPlant.PlantLoopNum() = 0;
			ZoneCompToPlant.PlantLoopBranch() = 0;
			ZoneCompToPlant.PlantLoopComp() = 0;
			ZoneCompToPlant.FirstDemandSidePtr() = 0;
			ZoneCompToPlant.LastDemandSidePtr() = 0;
			ZoneCompToPlant( {1,OldArrayLimit} ) = TempZoneCompToPlant( {1,OldArrayLimit} );
			TempZoneCompToPlant.deallocate();
			TempZoneCompToPlant.allocate( ArrayLimit );
			TempZoneCompToPlant.ZoneEqListNum() = 0;
			TempZoneCompToPlant.ZoneEqCompNum() = 0;
			TempZoneCompToPlant.PlantLoopType() = 0;
			TempZoneCompToPlant.PlantLoopNum() = 0;
			TempZoneCompToPlant.PlantLoopBranch() = 0;
			TempZoneCompToPlant.PlantLoopComp() = 0;
			TempZoneCompToPlant.FirstDemandSidePtr() = 0;
			TempZoneCompToPlant.LastDemandSidePtr() = 0;

			Idx = ArrayCounter;
			ZoneCompToPlant( Idx ).ZoneEqListNum = ListNum;
			ZoneCompToPlant( Idx ).ZoneEqCompNum = AirDistUnitNum;
			ZoneCompToPlant( Idx ).PlantLoopType = PlantLoopType;
			ZoneCompToPlant( Idx ).PlantLoopNum = PlantLoop;
			ZoneCompToPlant( Idx ).PlantLoopBranch = PlantBranch;
			ZoneCompToPlant( Idx ).PlantLoopComp = PlantComp;
			++ArrayCounter;
		}
	}

	void
	UpdateZoneSubCompPtrArray(
		int & Idx,
		int const ListNum,
		int const AirDistUnitNum,
		int const SubCompNum,
		int const PlantLoopType,
		int const PlantLoop,
		int const PlantBranch,
		int const PlantComp
	)
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         Dan Fisher
		//       DATE WRITTEN   June 2005
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Update Zone Sub Component Pointer Array

		// METHODOLOGY EMPLOYED:
		// na

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
		static bool OneTimeFlag( true );
		int OldArrayLimit;
		static int ArrayLimit( 100 );
		static int ArrayCounter( 1 );

		if ( OneTimeFlag ) {
			ZoneSubCompToPlant.allocate( ArrayLimit );
			ZoneSubCompToPlant.ZoneEqListNum() = 0;
			ZoneSubCompToPlant.ZoneEqCompNum() = 0;
			ZoneSubCompToPlant.ZoneEqSubCompNum() = 0;
			ZoneSubCompToPlant.PlantLoopType() = 0;
			ZoneSubCompToPlant.PlantLoopNum() = 0;
			ZoneSubCompToPlant.PlantLoopBranch() = 0;
			ZoneSubCompToPlant.PlantLoopComp() = 0;
			ZoneSubCompToPlant.FirstDemandSidePtr() = 0;
			ZoneSubCompToPlant.LastDemandSidePtr() = 0;
			TempZoneSubCompToPlant.allocate( ArrayLimit );
			TempZoneSubCompToPlant.ZoneEqListNum() = 0;
			TempZoneSubCompToPlant.ZoneEqCompNum() = 0;
			TempZoneSubCompToPlant.ZoneEqSubCompNum() = 0;
			TempZoneSubCompToPlant.PlantLoopType() = 0;
			TempZoneSubCompToPlant.PlantLoopNum() = 0;
			TempZoneSubCompToPlant.PlantLoopBranch() = 0;
			TempZoneSubCompToPlant.PlantLoopComp() = 0;
			TempZoneSubCompToPlant.FirstDemandSidePtr() = 0;
			TempZoneSubCompToPlant.LastDemandSidePtr() = 0;
			OneTimeFlag = false;
		}

		if ( ArrayCounter < ArrayLimit ) {
			Idx = ArrayCounter;
			ZoneSubCompToPlant( Idx ).ZoneEqListNum = ListNum;
			ZoneSubCompToPlant( Idx ).ZoneEqCompNum = AirDistUnitNum;
			ZoneSubCompToPlant( Idx ).ZoneEqSubCompNum = SubCompNum;
			ZoneSubCompToPlant( Idx ).PlantLoopType = PlantLoopType;
			ZoneSubCompToPlant( Idx ).PlantLoopNum = PlantLoop;
			ZoneSubCompToPlant( Idx ).PlantLoopBranch = PlantBranch;
			ZoneSubCompToPlant( Idx ).PlantLoopComp = PlantComp;
			++ArrayCounter;
		} else {
			TempZoneSubCompToPlant = ZoneSubCompToPlant;
			ZoneSubCompToPlant.deallocate();
			OldArrayLimit = ArrayLimit;
			ArrayLimit *= 2;
			ZoneSubCompToPlant.allocate( ArrayLimit );
			ZoneSubCompToPlant.ZoneEqListNum() = 0;
			ZoneSubCompToPlant.ZoneEqCompNum() = 0;
			ZoneSubCompToPlant.ZoneEqSubCompNum() = 0;
			ZoneSubCompToPlant.PlantLoopType() = 0;
			ZoneSubCompToPlant.PlantLoopNum() = 0;
			ZoneSubCompToPlant.PlantLoopBranch() = 0;
			ZoneSubCompToPlant.PlantLoopComp() = 0;
			ZoneSubCompToPlant.FirstDemandSidePtr() = 0;
			ZoneSubCompToPlant.LastDemandSidePtr() = 0;
			ZoneSubCompToPlant( {1,OldArrayLimit} ) = TempZoneSubCompToPlant( {1,OldArrayLimit} );
			TempZoneSubCompToPlant.deallocate();
			TempZoneSubCompToPlant.allocate( ArrayLimit );
			TempZoneSubCompToPlant.ZoneEqListNum() = 0;
			TempZoneSubCompToPlant.ZoneEqCompNum() = 0;
			TempZoneSubCompToPlant.ZoneEqSubCompNum() = 0;
			TempZoneSubCompToPlant.PlantLoopType() = 0;
			TempZoneSubCompToPlant.PlantLoopNum() = 0;
			TempZoneSubCompToPlant.PlantLoopBranch() = 0;
			TempZoneSubCompToPlant.PlantLoopComp() = 0;
			TempZoneSubCompToPlant.FirstDemandSidePtr() = 0;
			TempZoneSubCompToPlant.LastDemandSidePtr() = 0;

			Idx = ArrayCounter;
			ZoneSubCompToPlant( Idx ).ZoneEqListNum = ListNum;
			ZoneSubCompToPlant( Idx ).ZoneEqCompNum = AirDistUnitNum;
			ZoneSubCompToPlant( Idx ).ZoneEqSubCompNum = SubCompNum;
			ZoneSubCompToPlant( Idx ).PlantLoopType = PlantLoopType;
			ZoneSubCompToPlant( Idx ).PlantLoopNum = PlantLoop;
			ZoneSubCompToPlant( Idx ).PlantLoopBranch = PlantBranch;
			ZoneSubCompToPlant( Idx ).PlantLoopComp = PlantComp;
			++ArrayCounter;
		}
	}

	void
	UpdateZoneSubSubCompPtrArray(
		int & Idx,
		int const ListNum,
		int const AirDistUnitNum,
		int const SubCompNum,
		int const SubSubCompNum,
		int const PlantLoopType,
		int const PlantLoop,
		int const PlantBranch,
		int const PlantComp
	)
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         Dan Fisher
		//       DATE WRITTEN   June 2005
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Update Zone Sub Component Pointer Array

		// METHODOLOGY EMPLOYED:
		// na

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
		static bool OneTimeFlag( true );
		int OldArrayLimit;
		static int ArrayLimit( 100 );
		static int ArrayCounter( 1 );

		if ( OneTimeFlag ) {
			ZoneSubSubCompToPlant.allocate( ArrayLimit );
			ZoneSubSubCompToPlant.ZoneEqListNum() = 0;
			ZoneSubSubCompToPlant.ZoneEqCompNum() = 0;
			ZoneSubSubCompToPlant.ZoneEqSubCompNum() = 0;
			ZoneSubSubCompToPlant.ZoneEqSubSubCompNum() = 0;
			ZoneSubSubCompToPlant.PlantLoopType() = 0;
			ZoneSubSubCompToPlant.PlantLoopNum() = 0;
			ZoneSubSubCompToPlant.PlantLoopBranch() = 0;
			ZoneSubSubCompToPlant.PlantLoopComp() = 0;
			ZoneSubSubCompToPlant.FirstDemandSidePtr() = 0;
			ZoneSubSubCompToPlant.LastDemandSidePtr() = 0;
			TempZoneSubSubCompToPlant.allocate( ArrayLimit );
			TempZoneSubSubCompToPlant.ZoneEqListNum() = 0;
			TempZoneSubSubCompToPlant.ZoneEqCompNum() = 0;
			TempZoneSubSubCompToPlant.ZoneEqSubCompNum() = 0;
			TempZoneSubSubCompToPlant.ZoneEqSubSubCompNum() = 0;
			TempZoneSubSubCompToPlant.PlantLoopType() = 0;
			TempZoneSubSubCompToPlant.PlantLoopNum() = 0;
			TempZoneSubSubCompToPlant.PlantLoopBranch() = 0;
			TempZoneSubSubCompToPlant.PlantLoopComp() = 0;
			TempZoneSubSubCompToPlant.FirstDemandSidePtr() = 0;
			TempZoneSubSubCompToPlant.LastDemandSidePtr() = 0;
			OneTimeFlag = false;
		}

		if ( ArrayCounter < ArrayLimit ) {
			Idx = ArrayCounter;
			ZoneSubSubCompToPlant( Idx ).ZoneEqListNum = ListNum;
			ZoneSubSubCompToPlant( Idx ).ZoneEqCompNum = AirDistUnitNum;
			ZoneSubSubCompToPlant( Idx ).ZoneEqSubCompNum = SubCompNum;
			ZoneSubSubCompToPlant( Idx ).ZoneEqSubSubCompNum = SubSubCompNum;
			ZoneSubSubCompToPlant( Idx ).PlantLoopType = PlantLoopType;
			ZoneSubSubCompToPlant( Idx ).PlantLoopNum = PlantLoop;
			ZoneSubSubCompToPlant( Idx ).PlantLoopBranch = PlantBranch;
			ZoneSubSubCompToPlant( Idx ).PlantLoopComp = PlantComp;
			++ArrayCounter;
		} else {
			TempZoneSubSubCompToPlant = ZoneSubSubCompToPlant;
			ZoneSubSubCompToPlant.deallocate();
			OldArrayLimit = ArrayLimit;
			ArrayLimit *= 2;
			ZoneSubSubCompToPlant.allocate( ArrayLimit );
			ZoneSubSubCompToPlant.ZoneEqListNum() = 0;
			ZoneSubSubCompToPlant.ZoneEqCompNum() = 0;
			ZoneSubSubCompToPlant.ZoneEqSubCompNum() = 0;
			ZoneSubSubCompToPlant.ZoneEqSubSubCompNum() = 0;
			ZoneSubSubCompToPlant.PlantLoopType() = 0;
			ZoneSubSubCompToPlant.PlantLoopNum() = 0;
			ZoneSubSubCompToPlant.PlantLoopBranch() = 0;
			ZoneSubSubCompToPlant.PlantLoopComp() = 0;
			ZoneSubSubCompToPlant.FirstDemandSidePtr() = 0;
			ZoneSubSubCompToPlant.LastDemandSidePtr() = 0;
			ZoneSubSubCompToPlant( {1,OldArrayLimit} ) = TempZoneSubSubCompToPlant( {1,OldArrayLimit} );
			TempZoneSubSubCompToPlant.deallocate();
			TempZoneSubSubCompToPlant.allocate( ArrayLimit );
			TempZoneSubSubCompToPlant.ZoneEqListNum() = 0;
			TempZoneSubSubCompToPlant.ZoneEqCompNum() = 0;
			TempZoneSubSubCompToPlant.ZoneEqSubCompNum() = 0;
			TempZoneSubSubCompToPlant.ZoneEqSubSubCompNum() = 0;
			TempZoneSubSubCompToPlant.PlantLoopType() = 0;
			TempZoneSubSubCompToPlant.PlantLoopNum() = 0;
			TempZoneSubSubCompToPlant.PlantLoopBranch() = 0;
			TempZoneSubSubCompToPlant.PlantLoopComp() = 0;
			TempZoneSubSubCompToPlant.FirstDemandSidePtr() = 0;
			TempZoneSubSubCompToPlant.LastDemandSidePtr() = 0;

			Idx = ArrayCounter;
			ZoneSubSubCompToPlant( Idx ).ZoneEqListNum = ListNum;
			ZoneSubSubCompToPlant( Idx ).ZoneEqCompNum = AirDistUnitNum;
			ZoneSubSubCompToPlant( Idx ).ZoneEqSubCompNum = SubCompNum;
			ZoneSubSubCompToPlant( Idx ).ZoneEqSubSubCompNum = SubSubCompNum;
			ZoneSubSubCompToPlant( Idx ).PlantLoopType = PlantLoopType;
			ZoneSubSubCompToPlant( Idx ).PlantLoopNum = PlantLoop;
			ZoneSubSubCompToPlant( Idx ).PlantLoopBranch = PlantBranch;
			ZoneSubSubCompToPlant( Idx ).PlantLoopComp = PlantComp;
			++ArrayCounter;
		}
	}

	void
	UpdateAirSysCompPtrArray(
		int & Idx,
		int const AirLoopNum,
		int const BranchNum,
		int const CompNum,
		int const PlantLoopType,
		int const PlantLoop,
		int const PlantBranch,
		int const PlantComp
	)
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         Dan Fisher
		//       DATE WRITTEN   June 2005
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Update Air System Component Pointer Array

		// METHODOLOGY EMPLOYED:
		// na

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
		static bool OneTimeFlag( true );
		static int ArrayLimit( 100 );
		int OldArrayLimit;
		static int ArrayCounter( 1 );

		if ( OneTimeFlag ) {
			AirSysCompToPlant.allocate( ArrayLimit );
			TempAirSysCompToPlant.allocate( ArrayLimit );
			AirSysCompToPlant.AirLoopNum() = 0;
			AirSysCompToPlant.AirLoopBranch() = 0;
			AirSysCompToPlant.AirLoopComp() = 0;
			AirSysCompToPlant.PlantLoopType() = 0;
			AirSysCompToPlant.PlantLoopNum() = 0;
			AirSysCompToPlant.PlantLoopBranch() = 0;
			AirSysCompToPlant.PlantLoopComp() = 0;
			AirSysCompToPlant.FirstDemandSidePtr() = 0;
			AirSysCompToPlant.LastDemandSidePtr() = 0;
			TempAirSysCompToPlant.AirLoopNum() = 0;
			TempAirSysCompToPlant.AirLoopBranch() = 0;
			TempAirSysCompToPlant.AirLoopComp() = 0;
			TempAirSysCompToPlant.PlantLoopType() = 0;
			TempAirSysCompToPlant.PlantLoopNum() = 0;
			TempAirSysCompToPlant.PlantLoopBranch() = 0;
			TempAirSysCompToPlant.PlantLoopComp() = 0;
			TempAirSysCompToPlant.FirstDemandSidePtr() = 0;
			TempAirSysCompToPlant.LastDemandSidePtr() = 0;
			OneTimeFlag = false;
		}

		if ( ArrayCounter < ArrayLimit ) {
			Idx = ArrayCounter;
			AirSysCompToPlant( Idx ).AirLoopNum = AirLoopNum;
			AirSysCompToPlant( Idx ).AirLoopBranch = BranchNum;
			AirSysCompToPlant( Idx ).AirLoopComp = CompNum;
			AirSysCompToPlant( Idx ).PlantLoopType = PlantLoopType;
			AirSysCompToPlant( Idx ).PlantLoopNum = PlantLoop;
			AirSysCompToPlant( Idx ).PlantLoopBranch = PlantBranch;
			AirSysCompToPlant( Idx ).PlantLoopComp = PlantComp;
			++ArrayCounter;
		} else {
			TempAirSysCompToPlant = AirSysCompToPlant;
			AirSysCompToPlant.deallocate();
			OldArrayLimit = ArrayLimit;
			ArrayLimit *= 2;
			AirSysCompToPlant.allocate( ArrayLimit );
			AirSysCompToPlant.AirLoopNum() = 0;
			AirSysCompToPlant.AirLoopBranch() = 0;
			AirSysCompToPlant.AirLoopComp() = 0;
			AirSysCompToPlant.PlantLoopType() = 0;
			AirSysCompToPlant.PlantLoopNum() = 0;
			AirSysCompToPlant.PlantLoopBranch() = 0;
			AirSysCompToPlant.PlantLoopComp() = 0;
			AirSysCompToPlant.FirstDemandSidePtr() = 0;
			AirSysCompToPlant.LastDemandSidePtr() = 0;
			AirSysCompToPlant( {1,OldArrayLimit} ) = TempAirSysCompToPlant( {1,OldArrayLimit} );
			TempAirSysCompToPlant.deallocate();
			TempAirSysCompToPlant.allocate( ArrayLimit );
			TempAirSysCompToPlant.AirLoopNum() = 0;
			TempAirSysCompToPlant.AirLoopBranch() = 0;
			TempAirSysCompToPlant.AirLoopComp() = 0;
			TempAirSysCompToPlant.PlantLoopType() = 0;
			TempAirSysCompToPlant.PlantLoopNum() = 0;
			TempAirSysCompToPlant.PlantLoopBranch() = 0;
			TempAirSysCompToPlant.PlantLoopComp() = 0;
			TempAirSysCompToPlant.FirstDemandSidePtr() = 0;
			TempAirSysCompToPlant.LastDemandSidePtr() = 0;

			Idx = ArrayCounter;
			AirSysCompToPlant( Idx ).AirLoopNum = AirLoopNum;
			AirSysCompToPlant( Idx ).AirLoopBranch = BranchNum;
			AirSysCompToPlant( Idx ).AirLoopComp = CompNum;
			AirSysCompToPlant( Idx ).PlantLoopType = PlantLoopType;
			AirSysCompToPlant( Idx ).PlantLoopNum = PlantLoop;
			AirSysCompToPlant( Idx ).PlantLoopBranch = PlantBranch;
			AirSysCompToPlant( Idx ).PlantLoopComp = PlantComp;
			++ArrayCounter;
		}
	}

	void
	UpdateAirSysSubCompPtrArray(
		int & Idx,
		int const AirLoopNum,
		int const BranchNum,
		int const CompNum,
		int const SubCompNum,
		int const PlantLoopType,
		int const PlantLoop,
		int const PlantBranch,
		int const PlantComp
	)
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         Dan Fisher
		//       DATE WRITTEN   June 2005
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Update Air System Sub Component Pointer Array

		// METHODOLOGY EMPLOYED:
		// na

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
		static bool OneTimeFlag( true );
		int OldArrayLimit;
		static int ArrayLimit( 100 );
		static int ArrayCounter( 1 );

		if ( OneTimeFlag ) {
			AirSysSubCompToPlant.allocate( ArrayLimit );
			TempAirSysSubCompToPlant.allocate( ArrayLimit );
			AirSysSubCompToPlant.AirLoopNum() = 0;
			AirSysSubCompToPlant.AirLoopBranch() = 0;
			AirSysSubCompToPlant.AirLoopComp() = 0;
			AirSysSubCompToPlant.AirLoopSubComp() = 0;
			AirSysSubCompToPlant.PlantLoopType() = 0;
			AirSysSubCompToPlant.PlantLoopNum() = 0;
			AirSysSubCompToPlant.PlantLoopBranch() = 0;
			AirSysSubCompToPlant.PlantLoopComp() = 0;
			AirSysSubCompToPlant.FirstDemandSidePtr() = 0;
			AirSysSubCompToPlant.LastDemandSidePtr() = 0;
			TempAirSysSubCompToPlant.AirLoopNum() = 0;
			TempAirSysSubCompToPlant.AirLoopBranch() = 0;
			TempAirSysSubCompToPlant.AirLoopComp() = 0;
			TempAirSysSubCompToPlant.AirLoopSubComp() = 0;
			TempAirSysSubCompToPlant.PlantLoopType() = 0;
			TempAirSysSubCompToPlant.PlantLoopNum() = 0;
			TempAirSysSubCompToPlant.PlantLoopBranch() = 0;
			TempAirSysSubCompToPlant.PlantLoopComp() = 0;
			TempAirSysSubCompToPlant.FirstDemandSidePtr() = 0;
			TempAirSysSubCompToPlant.LastDemandSidePtr() = 0;
			OneTimeFlag = false;
		}

		if ( ArrayCounter < ArrayLimit ) {
			Idx = ArrayCounter;
			AirSysSubCompToPlant( Idx ).AirLoopNum = AirLoopNum;
			AirSysSubCompToPlant( Idx ).AirLoopBranch = BranchNum;
			AirSysSubCompToPlant( Idx ).AirLoopComp = CompNum;
			AirSysSubCompToPlant( Idx ).AirLoopSubComp = SubCompNum;
			AirSysSubCompToPlant( Idx ).PlantLoopType = PlantLoopType;
			AirSysSubCompToPlant( Idx ).PlantLoopNum = PlantLoop;
			AirSysSubCompToPlant( Idx ).PlantLoopBranch = PlantBranch;
			AirSysSubCompToPlant( Idx ).PlantLoopComp = PlantComp;
			++ArrayCounter;
		} else {
			TempAirSysSubCompToPlant = AirSysSubCompToPlant;
			AirSysSubCompToPlant.deallocate();
			OldArrayLimit = ArrayLimit;
			ArrayLimit *= 2;
			AirSysSubCompToPlant.allocate( ArrayLimit );
			AirSysSubCompToPlant.AirLoopNum() = 0;
			AirSysSubCompToPlant.AirLoopBranch() = 0;
			AirSysSubCompToPlant.AirLoopComp() = 0;
			AirSysSubCompToPlant.AirLoopSubComp() = 0;
			AirSysSubCompToPlant.PlantLoopType() = 0;
			AirSysSubCompToPlant.PlantLoopNum() = 0;
			AirSysSubCompToPlant.PlantLoopBranch() = 0;
			AirSysSubCompToPlant.PlantLoopComp() = 0;
			AirSysSubCompToPlant.FirstDemandSidePtr() = 0;
			AirSysSubCompToPlant.LastDemandSidePtr() = 0;
			AirSysSubCompToPlant( {1,OldArrayLimit} ) = TempAirSysSubCompToPlant( {1,OldArrayLimit} );
			TempAirSysSubCompToPlant.deallocate();
			TempAirSysSubCompToPlant.allocate( ArrayLimit );
			TempAirSysSubCompToPlant.AirLoopNum() = 0;
			TempAirSysSubCompToPlant.AirLoopBranch() = 0;
			TempAirSysSubCompToPlant.AirLoopComp() = 0;
			TempAirSysSubCompToPlant.AirLoopSubComp() = 0;
			TempAirSysSubCompToPlant.PlantLoopType() = 0;
			TempAirSysSubCompToPlant.PlantLoopNum() = 0;
			TempAirSysSubCompToPlant.PlantLoopBranch() = 0;
			TempAirSysSubCompToPlant.PlantLoopComp() = 0;
			TempAirSysSubCompToPlant.FirstDemandSidePtr() = 0;
			TempAirSysSubCompToPlant.LastDemandSidePtr() = 0;

			Idx = ArrayCounter;
			AirSysSubCompToPlant( Idx ).AirLoopNum = AirLoopNum;
			AirSysSubCompToPlant( Idx ).AirLoopBranch = BranchNum;
			AirSysSubCompToPlant( Idx ).AirLoopComp = CompNum;
			AirSysSubCompToPlant( Idx ).AirLoopSubComp = SubCompNum;
			AirSysSubCompToPlant( Idx ).PlantLoopType = PlantLoopType;
			AirSysSubCompToPlant( Idx ).PlantLoopNum = PlantLoop;
			AirSysSubCompToPlant( Idx ).PlantLoopBranch = PlantBranch;
			AirSysSubCompToPlant( Idx ).PlantLoopComp = PlantComp;
			++ArrayCounter;
		}
	}

	void
	UpdateAirSysSubSubCompPtrArray(
		int & Idx,
		int const AirLoopNum,
		int const BranchNum,
		int const CompNum,
		int const SubCompNum,
		int const SubSubCompNum,
		int const PlantLoopType,
		int const PlantLoop,
		int const PlantBranch,
		int const PlantComp
	)
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         Dan Fisher
		//       DATE WRITTEN   June 2005
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Update Air System Sub Sub Component Pointer Array

		// METHODOLOGY EMPLOYED:
		// na

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
		static bool OneTimeFlag( true );
		static int ArrayLimit( 100 );
		int OldArrayLimit;
		static int ArrayCounter( 1 );

		if ( OneTimeFlag ) {
			AirSysSubSubCompToPlant.allocate( ArrayLimit );
			TempAirSysSubSubCompToPlant.allocate( ArrayLimit );
			AirSysSubSubCompToPlant.AirLoopNum() = 0;
			AirSysSubSubCompToPlant.AirLoopBranch() = 0;
			AirSysSubSubCompToPlant.AirLoopComp() = 0;
			AirSysSubSubCompToPlant.AirLoopSubComp() = 0;
			AirSysSubSubCompToPlant.AirLoopSubSubComp() = 0;
			AirSysSubSubCompToPlant.PlantLoopType() = 0;
			AirSysSubSubCompToPlant.PlantLoopNum() = 0;
			AirSysSubSubCompToPlant.PlantLoopBranch() = 0;
			AirSysSubSubCompToPlant.PlantLoopComp() = 0;
			AirSysSubSubCompToPlant.FirstDemandSidePtr() = 0;
			AirSysSubSubCompToPlant.LastDemandSidePtr() = 0;
			TempAirSysSubSubCompToPlant.AirLoopNum() = 0;
			TempAirSysSubSubCompToPlant.AirLoopBranch() = 0;
			TempAirSysSubSubCompToPlant.AirLoopComp() = 0;
			TempAirSysSubSubCompToPlant.AirLoopSubComp() = 0;
			TempAirSysSubSubCompToPlant.AirLoopSubSubComp() = 0;
			TempAirSysSubSubCompToPlant.PlantLoopType() = 0;
			TempAirSysSubSubCompToPlant.PlantLoopNum() = 0;
			TempAirSysSubSubCompToPlant.PlantLoopBranch() = 0;
			TempAirSysSubSubCompToPlant.PlantLoopComp() = 0;
			TempAirSysSubSubCompToPlant.FirstDemandSidePtr() = 0;
			TempAirSysSubSubCompToPlant.LastDemandSidePtr() = 0;
			OneTimeFlag = false;
		}

		if ( ArrayCounter < ArrayLimit ) {
			Idx = ArrayCounter;
			AirSysSubSubCompToPlant( Idx ).AirLoopNum = AirLoopNum;
			AirSysSubSubCompToPlant( Idx ).AirLoopBranch = BranchNum;
			AirSysSubSubCompToPlant( Idx ).AirLoopComp = CompNum;
			AirSysSubSubCompToPlant( Idx ).AirLoopSubComp = SubCompNum;
			AirSysSubSubCompToPlant( Idx ).AirLoopSubSubComp = SubSubCompNum;
			AirSysSubSubCompToPlant( Idx ).PlantLoopType = PlantLoopType;
			AirSysSubSubCompToPlant( Idx ).PlantLoopNum = PlantLoop;
			AirSysSubSubCompToPlant( Idx ).PlantLoopBranch = PlantBranch;
			AirSysSubSubCompToPlant( Idx ).PlantLoopComp = PlantComp;
			++ArrayCounter;
		} else {
			TempAirSysSubSubCompToPlant = AirSysSubSubCompToPlant;
			AirSysSubSubCompToPlant.deallocate();
			OldArrayLimit = ArrayLimit;
			ArrayLimit *= 2;
			AirSysSubSubCompToPlant.allocate( ArrayLimit );
			AirSysSubSubCompToPlant.AirLoopNum() = 0;
			AirSysSubSubCompToPlant.AirLoopBranch() = 0;
			AirSysSubSubCompToPlant.AirLoopComp() = 0;
			AirSysSubSubCompToPlant.AirLoopSubComp() = 0;
			AirSysSubSubCompToPlant.AirLoopSubSubComp() = 0;
			AirSysSubSubCompToPlant.PlantLoopType() = 0;
			AirSysSubSubCompToPlant.PlantLoopNum() = 0;
			AirSysSubSubCompToPlant.PlantLoopBranch() = 0;
			AirSysSubSubCompToPlant.PlantLoopComp() = 0;
			AirSysSubSubCompToPlant.FirstDemandSidePtr() = 0;
			AirSysSubSubCompToPlant.LastDemandSidePtr() = 0;
			AirSysSubSubCompToPlant( {1,OldArrayLimit} ) = TempAirSysSubSubCompToPlant( {1,OldArrayLimit} );
			TempAirSysSubSubCompToPlant.deallocate();
			TempAirSysSubSubCompToPlant.allocate( ArrayLimit );
			TempAirSysSubSubCompToPlant.AirLoopNum() = 0;
			TempAirSysSubSubCompToPlant.AirLoopBranch() = 0;
			TempAirSysSubSubCompToPlant.AirLoopComp() = 0;
			TempAirSysSubSubCompToPlant.AirLoopSubComp() = 0;
			TempAirSysSubSubCompToPlant.AirLoopSubSubComp() = 0;
			TempAirSysSubSubCompToPlant.PlantLoopType() = 0;
			TempAirSysSubSubCompToPlant.PlantLoopNum() = 0;
			TempAirSysSubSubCompToPlant.PlantLoopBranch() = 0;
			TempAirSysSubSubCompToPlant.PlantLoopComp() = 0;
			TempAirSysSubSubCompToPlant.FirstDemandSidePtr() = 0;
			TempAirSysSubSubCompToPlant.LastDemandSidePtr() = 0;

			Idx = ArrayCounter;
			AirSysSubSubCompToPlant( Idx ).AirLoopNum = AirLoopNum;
			AirSysSubSubCompToPlant( Idx ).AirLoopBranch = BranchNum;
			AirSysSubSubCompToPlant( Idx ).AirLoopComp = CompNum;
			AirSysSubSubCompToPlant( Idx ).AirLoopSubComp = SubCompNum;
			AirSysSubSubCompToPlant( Idx ).AirLoopSubSubComp = SubSubCompNum;
			AirSysSubSubCompToPlant( Idx ).PlantLoopType = PlantLoopType;
			AirSysSubSubCompToPlant( Idx ).PlantLoopNum = PlantLoop;
			AirSysSubSubCompToPlant( Idx ).PlantLoopBranch = PlantBranch;
			AirSysSubSubCompToPlant( Idx ).PlantLoopComp = PlantComp;
			++ArrayCounter;
		}
	}

	void
	AllocateAndSetUpVentReports()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Sankaranarayanan K P
		//       DATE WRITTEN   July 2005
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Allocates Arrays and setup output variables related to Ventilation reports.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataZoneEquipment::ZoneEquipConfig;
		using DataZoneEquipment::NumOfZones;

		// Subroutine Variable Declaration

		// Locals
		int ZoneIndex;
		int SysIndex;

		MaxCoolingLoadMetByVent.allocate( NumOfZones );
		MaxCoolingLoadAddedByVent.allocate( NumOfZones );
		MaxOvercoolingByVent.allocate( NumOfZones );
		MaxHeatingLoadMetByVent.allocate( NumOfZones );
		MaxHeatingLoadAddedByVent.allocate( NumOfZones );
		MaxOverheatingByVent.allocate( NumOfZones );
		MaxNoLoadHeatingByVent.allocate( NumOfZones );
		MaxNoLoadCoolingByVent.allocate( NumOfZones );

		ZoneOAMassFlow.allocate( NumOfZones );
		ZoneOAMass.allocate( NumOfZones );
		ZoneOAVolFlowStdRho.allocate( NumOfZones );
		ZoneOAVolStdRho.allocate( NumOfZones );
		ZoneOAVolFlowCrntRho.allocate( NumOfZones );
		ZoneOAVolCrntRho.allocate( NumOfZones );
		ZoneMechACH.allocate( NumOfZones );

		SysTotZoneLoadHTNG.allocate( NumPrimaryAirSys );
		SysTotZoneLoadCLNG.allocate( NumPrimaryAirSys );
		SysOALoadHTNG.allocate( NumPrimaryAirSys );
		SysOALoadCLNG.allocate( NumPrimaryAirSys );
		SysTotHTNG.allocate( NumPrimaryAirSys );
		SysTotCLNG.allocate( NumPrimaryAirSys );

		SysTotElec.allocate( NumPrimaryAirSys );
		SysTotGas.allocate( NumPrimaryAirSys );
		SysTotSteam.allocate( NumPrimaryAirSys );
		SysTotH2OCOLD.allocate( NumPrimaryAirSys );
		SysTotH2OHOT.allocate( NumPrimaryAirSys );

		SysHumidHTNG.allocate( NumPrimaryAirSys );
		SysHumidElec.allocate( NumPrimaryAirSys );
		DesDehumidCLNG.allocate( NumPrimaryAirSys );
		DesDehumidElec.allocate( NumPrimaryAirSys );
		SysEvapCLNG.allocate( NumPrimaryAirSys );
		SysEvapElec.allocate( NumPrimaryAirSys );
		SysHeatExHTNG.allocate( NumPrimaryAirSys );
		SysHeatExCLNG.allocate( NumPrimaryAirSys );
		SysSolarCollectHeating.allocate( NumPrimaryAirSys );
		SysSolarCollectCooling.allocate( NumPrimaryAirSys );
		SysUserDefinedTerminalHeating.allocate( NumPrimaryAirSys );
		SysUserDefinedTerminalCooling.allocate( NumPrimaryAirSys );
		SysFANCompHTNG.allocate( NumPrimaryAirSys );
		SysFANCompElec.allocate( NumPrimaryAirSys );
		SysCCCompCLNG.allocate( NumPrimaryAirSys );
		SysCCCompH2OCOLD.allocate( NumPrimaryAirSys );
		SysCCCompElec.allocate( NumPrimaryAirSys );
		SysHCCompH2OHOT.allocate( NumPrimaryAirSys );
		SysHCCompElec.allocate( NumPrimaryAirSys );
		SysHCCompElecRes.allocate( NumPrimaryAirSys );
		SysHCCompHTNG.allocate( NumPrimaryAirSys );
		SysHCCompGas.allocate( NumPrimaryAirSys );
		SysHCCompSteam.allocate( NumPrimaryAirSys );
		SysDomesticH20.allocate( NumPrimaryAirSys );

		SetBackCounter.allocate( NumOfZones );
		HeatCoolFlag.allocate( NumOfZones );
		LastHeatCoolFlag.allocate( NumOfZones );
		FirstHeatCoolFlag.allocate( NumOfZones );
		LastHeatCoolHour.allocate( NumOfZones );
		FirstHeatCoolHour.allocate( NumOfZones );
		NoLoadFlag.allocate( NumOfZones );
		UnmetLoadFlag.allocate( NumOfZones );

		UnmetLoadFlag = false;
		SetBackCounter = 0;
		HeatCoolFlag = 0;
		LastHeatCoolFlag = 0;
		FirstHeatCoolFlag = 0;
		LastHeatCoolHour = 0;
		FirstHeatCoolHour = 0;
		NoLoadFlag = false;

		MaxCoolingLoadMetByVent = 0.0;
		MaxCoolingLoadAddedByVent = 0.0;
		MaxOvercoolingByVent = 0.0;
		MaxHeatingLoadMetByVent = 0.0;
		MaxHeatingLoadAddedByVent = 0.0;
		MaxOverheatingByVent = 0.0;
		MaxNoLoadHeatingByVent = 0.0;
		MaxNoLoadCoolingByVent = 0.0;

		ZoneOAMassFlow = 0.0;
		ZoneOAMass = 0.0;
		ZoneOAVolFlowStdRho = 0.0;
		ZoneOAVolStdRho = 0.0;
		ZoneOAVolFlowCrntRho = 0.0;
		ZoneOAVolCrntRho = 0.0;
		ZoneMechACH = 0.0;

		//SYSTEM LOADS REPORT
		SysTotZoneLoadHTNG = 0.0;
		SysTotZoneLoadCLNG = 0.0;
		SysOALoadHTNG = 0.0;
		SysOALoadCLNG = 0.0;
		SysTotHTNG = 0.0;
		SysTotCLNG = 0.0;

		//SYSTEM ENERGY USE REPORT
		SysTotElec = 0.0;
		SysTotGas = 0.0;
		SysTotSteam = 0.0;
		SysTotH2OCOLD = 0.0;
		SysTotH2OHOT = 0.0;

		//SYSTEM COMPONENT LOADS REPORT
		SysFANCompHTNG = 0.0;
		SysCCCompCLNG = 0.0;
		SysHCCompHTNG = 0.0;
		SysHeatExHTNG = 0.0;
		SysHeatExCLNG = 0.0;
		SysSolarCollectHeating = 0.0;
		SysSolarCollectCooling = 0.0;
		SysUserDefinedTerminalHeating = 0.0;
		SysUserDefinedTerminalCooling = 0.0;
		SysHumidHTNG = 0.0;
		SysEvapCLNG = 0.0;
		DesDehumidCLNG = 0.0;
		SysDomesticH20 = 0.0;

		//SYSTEM COMPONENT ENERGY REPORT
		SysFANCompElec = 0.0;
		SysHCCompH2OHOT = 0.0;
		SysCCCompH2OCOLD = 0.0;
		SysHCCompElec = 0.0;
		SysCCCompElec = 0.0;
		SysHCCompElecRes = 0.0;
		SysHCCompGas = 0.0;
		SysHCCompSteam = 0.0;
		SysHumidElec = 0.0;
		DesDehumidElec = 0.0;
		SysEvapElec = 0.0;

		if ( AirLoopLoadsReportEnabled ) {
			for ( SysIndex = 1; SysIndex <= NumPrimaryAirSys; ++SysIndex ) {

				//CurrentModuleObject='AirloopHVAC'
				//SYSTEM LOADS REPORT
				SetupOutputVariable( "Air System Total Heating Energy [J]", SysTotHTNG( SysIndex ), "HVAC", "Sum", PrimaryAirSystem( SysIndex ).Name );

				SetupOutputVariable( "Air System Total Cooling Energy [J]", SysTotCLNG( SysIndex ), "HVAC", "Sum", PrimaryAirSystem( SysIndex ).Name );

				//SYSTEM ENERGY USE REPORT
				SetupOutputVariable( "Air System Hot Water Energy [J]", SysTotH2OHOT( SysIndex ), "HVAC", "Sum", PrimaryAirSystem( SysIndex ).Name );

				SetupOutputVariable( "Air System Steam Energy [J]", SysTotSteam( SysIndex ), "HVAC", "Sum", PrimaryAirSystem( SysIndex ).Name );

				SetupOutputVariable( "Air System Chilled Water Energy [J]", SysTotH2OCOLD( SysIndex ), "HVAC", "Sum", PrimaryAirSystem( SysIndex ).Name );

				SetupOutputVariable( "Air System Electric Energy [J]", SysTotElec( SysIndex ), "HVAC", "Sum", PrimaryAirSystem( SysIndex ).Name );

				SetupOutputVariable( "Air System Gas Energy [J]", SysTotGas( SysIndex ), "HVAC", "Sum", PrimaryAirSystem( SysIndex ).Name );

				SetupOutputVariable( "Air System Water Volume [m3]", SysDomesticH20( SysIndex ), "HVAC", "Sum", PrimaryAirSystem( SysIndex ).Name );

				//SYSTEM COMPONENT LOAD REPORT
				SetupOutputVariable( "Air System Fan Air Heating Energy [J]", SysFANCompHTNG( SysIndex ), "HVAC", "Sum", PrimaryAirSystem( SysIndex ).Name );

				SetupOutputVariable( "Air System Cooling Coil Total Cooling Energy [J]", SysCCCompCLNG( SysIndex ), "HVAC", "Sum", PrimaryAirSystem( SysIndex ).Name );

				SetupOutputVariable( "Air System Heating Coil Total Heating Energy [J]", SysHCCompHTNG( SysIndex ), "HVAC", "Sum", PrimaryAirSystem( SysIndex ).Name );

				SetupOutputVariable( "Air System Heat Exchanger Total Heating Energy [J]", SysHeatExHTNG( SysIndex ), "HVAC", "Sum", PrimaryAirSystem( SysIndex ).Name );

				SetupOutputVariable( "Air System Heat Exchanger Total Cooling Energy [J]", SysHeatExCLNG( SysIndex ), "HVAC", "Sum", PrimaryAirSystem( SysIndex ).Name );

				SetupOutputVariable( "Air System Solar Collector Total Heating Energy [J]", SysSolarCollectHeating( SysIndex ), "HVAC", "Sum", PrimaryAirSystem( SysIndex ).Name );

				SetupOutputVariable( "Air System Solar Collector Total Cooling Energy [J]", SysSolarCollectCooling( SysIndex ), "HVAC", "Sum", PrimaryAirSystem( SysIndex ).Name );

				SetupOutputVariable( "Air System User Defined Air Terminal Total Heating Energy [J]", SysUserDefinedTerminalHeating( SysIndex ), "HVAC", "Sum", PrimaryAirSystem( SysIndex ).Name );

				SetupOutputVariable( "Air System User Defined Air Terminal Total Cooling Energy [J]", SysUserDefinedTerminalCooling( SysIndex ), "HVAC", "Sum", PrimaryAirSystem( SysIndex ).Name );

				SetupOutputVariable( "Air System Humidifier Total Heating Energy [J]", SysHumidHTNG( SysIndex ), "HVAC", "Sum", PrimaryAirSystem( SysIndex ).Name );

				SetupOutputVariable( "Air System Evaporative Cooler Total Cooling Energy [J]", SysEvapCLNG( SysIndex ), "HVAC", "Sum", PrimaryAirSystem( SysIndex ).Name );

				SetupOutputVariable( "Air System Desiccant Dehumidifier Total Cooling Energy [J]", DesDehumidCLNG( SysIndex ), "HVAC", "Sum", PrimaryAirSystem( SysIndex ).Name );

				//SYSTEM COMPONENT ENERGY REPORT
				SetupOutputVariable( "Air System Fan Electric Energy [J]", SysFANCompElec( SysIndex ), "HVAC", "Sum", PrimaryAirSystem( SysIndex ).Name );

				SetupOutputVariable( "Air System Heating Coil Hot Water Energy [J]", SysHCCompH2OHOT( SysIndex ), "HVAC", "Sum", PrimaryAirSystem( SysIndex ).Name );

				SetupOutputVariable( "Air System Cooling Coil Chilled Water Energy [J]", SysCCCompH2OCOLD( SysIndex ), "HVAC", "Sum", PrimaryAirSystem( SysIndex ).Name );

				SetupOutputVariable( "Air System DX Heating Coil Electric Energy [J]", SysHCCompElec( SysIndex ), "HVAC", "Sum", PrimaryAirSystem( SysIndex ).Name );

				SetupOutputVariable( "Air System DX Cooling Coil Electric Energy [J]", SysCCCompElec( SysIndex ), "HVAC", "Sum", PrimaryAirSystem( SysIndex ).Name );

				SetupOutputVariable( "Air System Heating Coil Electric Energy [J]", SysHCCompElecRes( SysIndex ), "HVAC", "Sum", PrimaryAirSystem( SysIndex ).Name );

				SetupOutputVariable( "Air System Heating Coil Gas Energy [J]", SysHCCompGas( SysIndex ), "HVAC", "Sum", PrimaryAirSystem( SysIndex ).Name );

				SetupOutputVariable( "Air System Heating Coil Steam Energy [J]", SysHCCompSteam( SysIndex ), "HVAC", "Sum", PrimaryAirSystem( SysIndex ).Name );

				SetupOutputVariable( "Air System Humidifier Electric Energy [J]", SysHumidElec( SysIndex ), "HVAC", "Sum", PrimaryAirSystem( SysIndex ).Name );

				SetupOutputVariable( "Air System Evaporative Cooler Electric Energy [J]", SysEvapElec( SysIndex ), "HVAC", "Sum", PrimaryAirSystem( SysIndex ).Name );

				SetupOutputVariable( "Air System Desiccant Dehumidifier Electric Energy [J]", DesDehumidElec( SysIndex ), "HVAC", "Sum", PrimaryAirSystem( SysIndex ).Name );

			}
		}

		for ( ZoneIndex = 1; ZoneIndex <= NumOfZones; ++ZoneIndex ) {
			if ( ! ZoneEquipConfig( ZoneIndex ).IsControlled ) continue;
			// CurrentModuleObject='Zones(Controlled)'
			if ( VentLoadsReportEnabled ) {
				//Cooling Loads
				SetupOutputVariable( "Zone Mechanical Ventilation No Load Heat Removal Energy [J]", MaxNoLoadCoolingByVent( ZoneIndex ), "HVAC", "Sum", ZoneEquipConfig( ZoneIndex ).ZoneName );

				SetupOutputVariable( "Zone Mechanical Ventilation Cooling Load Increase Energy [J]", MaxCoolingLoadAddedByVent( ZoneIndex ), "HVAC", "Sum", ZoneEquipConfig( ZoneIndex ).ZoneName );

				SetupOutputVariable( "Zone Mechanical Ventilation Cooling Load Increase Due to Overheating Energy [J]", MaxOverheatingByVent( ZoneIndex ), "HVAC", "Sum", ZoneEquipConfig( ZoneIndex ).ZoneName );

				SetupOutputVariable( "Zone Mechanical Ventilation Cooling Load Decrease Energy [J]", MaxCoolingLoadMetByVent( ZoneIndex ), "HVAC", "Sum", ZoneEquipConfig( ZoneIndex ).ZoneName );
				//Heating Loads
				SetupOutputVariable( "Zone Mechanical Ventilation No Load Heat Addition Energy [J]", MaxNoLoadHeatingByVent( ZoneIndex ), "HVAC", "Sum", ZoneEquipConfig( ZoneIndex ).ZoneName );

				SetupOutputVariable( "Zone Mechanical Ventilation Heating Load Increase Energy [J]", MaxHeatingLoadAddedByVent( ZoneIndex ), "HVAC", "Sum", ZoneEquipConfig( ZoneIndex ).ZoneName );

				SetupOutputVariable( "Zone Mechanical Ventilation Heating Load Increase Due to Overcooling Energy [J]", MaxOvercoolingByVent( ZoneIndex ), "HVAC", "Sum", ZoneEquipConfig( ZoneIndex ).ZoneName );

				SetupOutputVariable( "Zone Mechanical Ventilation Heating Load Decrease Energy [J]", MaxHeatingLoadMetByVent( ZoneIndex ), "HVAC", "Sum", ZoneEquipConfig( ZoneIndex ).ZoneName );
			}

			SetupOutputVariable( "Zone Mechanical Ventilation Mass Flow Rate [kg/s]", ZoneOAMassFlow( ZoneIndex ), "HVAC", "Average", ZoneEquipConfig( ZoneIndex ).ZoneName );

			SetupOutputVariable( "Zone Mechanical Ventilation Mass [kg]", ZoneOAMass( ZoneIndex ), "HVAC", "Sum", ZoneEquipConfig( ZoneIndex ).ZoneName );

			SetupOutputVariable( "Zone Mechanical Ventilation Standard Density Volume Flow Rate [m3/s]", ZoneOAVolFlowStdRho( ZoneIndex ), "HVAC", "Average", ZoneEquipConfig( ZoneIndex ).ZoneName );

			SetupOutputVariable( "Zone Mechanical Ventilation Standard Density Volume [m3]", ZoneOAVolStdRho( ZoneIndex ), "HVAC", "Sum", ZoneEquipConfig( ZoneIndex ).ZoneName );

			SetupOutputVariable( "Zone Mechanical Ventilation Current Density Volume Flow Rate [m3/s]", ZoneOAVolFlowCrntRho( ZoneIndex ), "HVAC", "Average", ZoneEquipConfig( ZoneIndex ).ZoneName );

			SetupOutputVariable( "Zone Mechanical Ventilation Current Density Volume [m3]", ZoneOAVolCrntRho( ZoneIndex ), "HVAC", "Sum", ZoneEquipConfig( ZoneIndex ).ZoneName );

			SetupOutputVariable( "Zone Mechanical Ventilation Air Changes per Hour [ach]", ZoneMechACH( ZoneIndex ), "HVAC", "Average", ZoneEquipConfig( ZoneIndex ).ZoneName );
		}

	}

	void
	CreateEnergyReportStructure()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Dan Fisher/Linda Lawrie
		//       DATE WRITTEN   June 2005
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Creates the Energy Reporting Structure.  This routine is only called once --
		// so string compares have been left in.

		// METHODOLOGY EMPLOYED:
		// Once all compsets/nodes/connections have been established find all components
		// subcomponents, etc.

		// REFERENCES:
		// na

		// Using/Aliasing
		using BranchNodeConnections::GetComponentData;
		using BranchNodeConnections::GetChildrenData;
		using BranchNodeConnections::GetNumChildren;
		using BranchNodeConnections::IsParentObject;
		using namespace DataGlobalConstants;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int AirLoopNum;
		int BranchNum;
		int CompNum;
		int SubCompNum;
		int SubSubCompNum;
		int VarNum;
		int VarNum1;
		int CtrlZoneNum;
		std::string TypeOfComp;
		std::string NameOfComp;
		bool ErrorsFound;
		bool ModeFlagOn;
		int NumInlets;
		int NumOutlets;
		int PlantLoopNum;

		//Dimension GetChildrenData arrays
		FArray1D_string SubCompTypes;
		FArray1D_string SubCompNames;
		FArray1D_string InletNodeNames;
		FArray1D_int InletNodeNumbers;
		FArray1D_int InletFluidStreams;
		FArray1D_string OutletNodeNames;
		FArray1D_int OutletNodeNumbers;
		FArray1D_int OutletFluidStreams;
		int NumChildren;
		int NumGrandChildren;
		bool IsParent;

		//Dimension GetMeteredVariables arrays
		FArray1D_int VarIndexes; // Variable Numbers
		FArray1D_int VarTypes; // Variable Types (1=integer, 2=real, 3=meter)
		FArray1D_int IndexTypes; // Variable Idx Types (1=Zone,2=HVAC)
		FArray1D_string UnitsStrings; // UnitsStrings for each variable
		FArray1D_int ResourceTypes; // ResourceTypes for each variable
		FArray1D_string EndUses; // EndUses for each variable
		FArray1D_string Groups; // Groups for each variable
		FArray1D_string Names; // Variable Names for each variable
		int NumFound; // Number Found
		int NumVariables;
		int NumLeft; // Counter for deeper components

		// some variables for setting up the plant data structures
		int LoopSideNum;

		VentReportStructureCreated = true;

		AllocateAndSetUpVentReports();

		for ( AirLoopNum = 1; AirLoopNum <= NumPrimaryAirSys; ++AirLoopNum ) {
			for ( BranchNum = 1; BranchNum <= PrimaryAirSystem( AirLoopNum ).NumBranches; ++BranchNum ) {
				for ( CompNum = 1; CompNum <= PrimaryAirSystem( AirLoopNum ).Branch( BranchNum ).TotalComponents; ++CompNum ) {
					TypeOfComp = PrimaryAirSystem( AirLoopNum ).Branch( BranchNum ).Comp( CompNum ).TypeOf;
					NameOfComp = PrimaryAirSystem( AirLoopNum ).Branch( BranchNum ).Comp( CompNum ).Name;
					// Get complete list of components for complex branches
					if ( IsParentObject( TypeOfComp, NameOfComp ) ) {

						PrimaryAirSystem( AirLoopNum ).Branch( BranchNum ).Comp( CompNum ).Parent = true;
						NumChildren = GetNumChildren( TypeOfComp, NameOfComp );

						SubCompTypes.allocate( NumChildren );
						SubCompNames.allocate( NumChildren );
						InletNodeNames.allocate( NumChildren );
						InletNodeNumbers.allocate( NumChildren );
						OutletNodeNames.allocate( NumChildren );
						OutletNodeNumbers.allocate( NumChildren );
						PrimaryAirSystem( AirLoopNum ).Branch( BranchNum ).Comp( CompNum ).SubComp.allocate( NumChildren );

						GetChildrenData( TypeOfComp, NameOfComp, NumChildren, SubCompTypes, SubCompNames, InletNodeNames, InletNodeNumbers, OutletNodeNames, OutletNodeNumbers, ErrorsFound );

						for ( SubCompNum = 1; SubCompNum <= NumChildren; ++SubCompNum ) {
							PrimaryAirSystem( AirLoopNum ).Branch( BranchNum ).Comp( CompNum ).SubComp( SubCompNum ).TypeOf = SubCompTypes( SubCompNum );
							PrimaryAirSystem( AirLoopNum ).Branch( BranchNum ).Comp( CompNum ).SubComp( SubCompNum ).Name = SubCompNames( SubCompNum );
							PrimaryAirSystem( AirLoopNum ).Branch( BranchNum ).Comp( CompNum ).SubComp( SubCompNum ).NodeNameIn = InletNodeNames( SubCompNum );
							PrimaryAirSystem( AirLoopNum ).Branch( BranchNum ).Comp( CompNum ).SubComp( SubCompNum ).NodeNameOut = OutletNodeNames( SubCompNum );
							PrimaryAirSystem( AirLoopNum ).Branch( BranchNum ).Comp( CompNum ).SubComp( SubCompNum ).NodeNumIn = InletNodeNumbers( SubCompNum );
							PrimaryAirSystem( AirLoopNum ).Branch( BranchNum ).Comp( CompNum ).SubComp( SubCompNum ).NodeNumOut = OutletNodeNumbers( SubCompNum );
						}

						SubCompTypes.deallocate();
						SubCompNames.deallocate();
						InletNodeNames.deallocate();
						InletNodeNumbers.deallocate();
						OutletNodeNames.deallocate();
						OutletNodeNumbers.deallocate();

					} else {
						NumChildren = 0;
						PrimaryAirSystem( AirLoopNum ).Branch( BranchNum ).Comp( CompNum ).Parent = false;
					}
					PrimaryAirSystem( AirLoopNum ).Branch( BranchNum ).Comp( CompNum ).NumSubComps = NumChildren;

					//check for 'grandchildren'
					for ( SubCompNum = 1; SubCompNum <= NumChildren; ++SubCompNum ) {
						TypeOfComp = PrimaryAirSystem( AirLoopNum ).Branch( BranchNum ).Comp( CompNum ).SubComp( SubCompNum ).TypeOf;
						NameOfComp = PrimaryAirSystem( AirLoopNum ).Branch( BranchNum ).Comp( CompNum ).SubComp( SubCompNum ).Name;
						if ( IsParentObject( TypeOfComp, NameOfComp ) ) {
							NumGrandChildren = GetNumChildren( TypeOfComp, NameOfComp );
							SubCompTypes.allocate( NumGrandChildren );
							SubCompNames.allocate( NumGrandChildren );
							InletNodeNames.allocate( NumGrandChildren );
							InletNodeNumbers.allocate( NumGrandChildren );
							OutletNodeNames.allocate( NumGrandChildren );
							OutletNodeNumbers.allocate( NumGrandChildren );
							PrimaryAirSystem( AirLoopNum ).Branch( BranchNum ).Comp( CompNum ).SubComp( SubCompNum ).SubSubComp.allocate( NumGrandChildren );

							GetChildrenData( TypeOfComp, NameOfComp, NumGrandChildren, SubCompTypes, SubCompNames, InletNodeNames, InletNodeNumbers, OutletNodeNames, OutletNodeNumbers, ErrorsFound );

							for ( SubSubCompNum = 1; SubSubCompNum <= NumGrandChildren; ++SubSubCompNum ) {
								PrimaryAirSystem( AirLoopNum ).Branch( BranchNum ).Comp( CompNum ).SubComp( SubCompNum ).SubSubComp( SubSubCompNum ).TypeOf = SubCompTypes( SubSubCompNum );
								PrimaryAirSystem( AirLoopNum ).Branch( BranchNum ).Comp( CompNum ).SubComp( SubCompNum ).SubSubComp( SubSubCompNum ).Name = SubCompNames( SubSubCompNum );
								PrimaryAirSystem( AirLoopNum ).Branch( BranchNum ).Comp( CompNum ).SubComp( SubCompNum ).SubSubComp( SubSubCompNum ).NodeNameIn = InletNodeNames( SubSubCompNum );
								PrimaryAirSystem( AirLoopNum ).Branch( BranchNum ).Comp( CompNum ).SubComp( SubCompNum ).SubSubComp( SubSubCompNum ).NodeNameOut = OutletNodeNames( SubSubCompNum );
								PrimaryAirSystem( AirLoopNum ).Branch( BranchNum ).Comp( CompNum ).SubComp( SubCompNum ).SubSubComp( SubSubCompNum ).NodeNumIn = InletNodeNumbers( SubSubCompNum );
								PrimaryAirSystem( AirLoopNum ).Branch( BranchNum ).Comp( CompNum ).SubComp( SubCompNum ).SubSubComp( SubSubCompNum ).NodeNumOut = OutletNodeNumbers( SubSubCompNum );
								NumLeft = GetNumChildren( SubCompTypes( SubSubCompNum ), SubCompNames( SubSubCompNum ) );
								if ( NumLeft > 0 ) {
									ShowSevereError( "Hanging Children for component=" + SubCompTypes( SubSubCompNum ) + ':' + SubCompNames( SubSubCompNum ) );
								}
							}

							SubCompTypes.deallocate();
							SubCompNames.deallocate();
							InletNodeNames.deallocate();
							InletNodeNumbers.deallocate();
							OutletNodeNames.deallocate();
							OutletNodeNumbers.deallocate();
						} else {
							NumGrandChildren = 0;
							PrimaryAirSystem( AirLoopNum ).Branch( BranchNum ).Comp( CompNum ).SubComp( SubCompNum ).Parent = false;
						}

						PrimaryAirSystem( AirLoopNum ).Branch( BranchNum ).Comp( CompNum ).SubComp( SubCompNum ).NumSubSubComps = NumGrandChildren;

					}
				}
			}
		}

		for ( AirLoopNum = 1; AirLoopNum <= NumPrimaryAirSys; ++AirLoopNum ) {
			for ( BranchNum = 1; BranchNum <= PrimaryAirSystem( AirLoopNum ).NumBranches; ++BranchNum ) {
				for ( CompNum = 1; CompNum <= PrimaryAirSystem( AirLoopNum ).Branch( BranchNum ).TotalComponents; ++CompNum ) {
					// Get complete list of components for complex branches
					TypeOfComp = PrimaryAirSystem( AirLoopNum ).Branch( BranchNum ).Comp( CompNum ).TypeOf;
					NameOfComp = PrimaryAirSystem( AirLoopNum ).Branch( BranchNum ).Comp( CompNum ).Name;
					NumVariables = GetNumMeteredVariables( TypeOfComp, NameOfComp );
					VarIndexes.allocate( NumVariables );
					VarTypes.allocate( NumVariables );
					IndexTypes.allocate( NumVariables );
					UnitsStrings.allocate( NumVariables );
					ResourceTypes.allocate( NumVariables );
					EndUses.allocate( NumVariables );
					Groups.allocate( NumVariables );
					Names.allocate( NumVariables );
					PrimaryAirSystem( AirLoopNum ).Branch( BranchNum ).Comp( CompNum ).MeteredVar.allocate( NumVariables );

					PrimaryAirSystem( AirLoopNum ).Branch( BranchNum ).Comp( CompNum ).NumMeteredVars = NumVariables;
					GetMeteredVariables( TypeOfComp, NameOfComp, VarIndexes, VarTypes, IndexTypes, UnitsStrings, ResourceTypes, EndUses, Groups, Names, NumFound );
					ModeFlagOn = true;
					for ( VarNum = 1; VarNum <= NumVariables; ++VarNum ) {
						PrimaryAirSystem( AirLoopNum ).Branch( BranchNum ).Comp( CompNum ).MeteredVar( VarNum ).ReportVarName = Names( VarNum );
						PrimaryAirSystem( AirLoopNum ).Branch( BranchNum ).Comp( CompNum ).MeteredVar( VarNum ).ReportVarUnits = UnitsStrings( VarNum );
						PrimaryAirSystem( AirLoopNum ).Branch( BranchNum ).Comp( CompNum ).MeteredVar( VarNum ).ReportVarIndex = VarIndexes( VarNum );
						PrimaryAirSystem( AirLoopNum ).Branch( BranchNum ).Comp( CompNum ).MeteredVar( VarNum ).ReportVarIndexType = IndexTypes( VarNum );
						PrimaryAirSystem( AirLoopNum ).Branch( BranchNum ).Comp( CompNum ).MeteredVar( VarNum ).ReportVarType = VarTypes( VarNum );
						PrimaryAirSystem( AirLoopNum ).Branch( BranchNum ).Comp( CompNum ).MeteredVar( VarNum ).ResourceType = ResourceTypes( VarNum );
						PrimaryAirSystem( AirLoopNum ).Branch( BranchNum ).Comp( CompNum ).MeteredVar( VarNum ).EndUse = EndUses( VarNum );
						if ( PrimaryAirSystem( AirLoopNum ).Branch( BranchNum ).Comp( CompNum ).MeteredVar( VarNum ).EndUse == "HEATINGCOILS" && ModeFlagOn ) {
							for ( VarNum1 = 1; VarNum1 <= NumVariables; ++VarNum1 ) {
								PrimaryAirSystem( AirLoopNum ).Branch( BranchNum ).Comp( CompNum ).MeteredVar( VarNum1 ).EndUse_CompMode = HeatingOnly;
							}
							ModeFlagOn = false;
						} else if ( PrimaryAirSystem( AirLoopNum ).Branch( BranchNum ).Comp( CompNum ).MeteredVar( VarNum ).EndUse == "COOLINGCOILS" && ModeFlagOn ) {
							for ( VarNum1 = 1; VarNum1 <= NumVariables; ++VarNum1 ) {
								PrimaryAirSystem( AirLoopNum ).Branch( BranchNum ).Comp( CompNum ).MeteredVar( VarNum1 ).EndUse_CompMode = CoolingOnly;
							}
							ModeFlagOn = false;
						} else if ( ModeFlagOn ) {
							PrimaryAirSystem( AirLoopNum ).Branch( BranchNum ).Comp( CompNum ).MeteredVar( VarNum ).EndUse_CompMode = NoHeatNoCool;
						}
						PrimaryAirSystem( AirLoopNum ).Branch( BranchNum ).Comp( CompNum ).MeteredVar( VarNum ).Group = Groups( VarNum );
					}

					VarIndexes.deallocate();
					VarTypes.deallocate();
					IndexTypes.deallocate();
					UnitsStrings.deallocate();
					ResourceTypes.deallocate();
					EndUses.deallocate();
					Groups.deallocate();
					Names.deallocate();

					for ( SubCompNum = 1; SubCompNum <= PrimaryAirSystem( AirLoopNum ).Branch( BranchNum ).Comp( CompNum ).NumSubComps; ++SubCompNum ) {
						// Get complete list of components for complex branches
						TypeOfComp = PrimaryAirSystem( AirLoopNum ).Branch( BranchNum ).Comp( CompNum ).SubComp( SubCompNum ).TypeOf;
						NameOfComp = PrimaryAirSystem( AirLoopNum ).Branch( BranchNum ).Comp( CompNum ).SubComp( SubCompNum ).Name;
						NumVariables = GetNumMeteredVariables( TypeOfComp, NameOfComp );
						VarIndexes.allocate( NumVariables );
						VarTypes.allocate( NumVariables );
						IndexTypes.allocate( NumVariables );
						UnitsStrings.allocate( NumVariables );
						ResourceTypes.allocate( NumVariables );
						EndUses.allocate( NumVariables );
						Groups.allocate( NumVariables );
						Names.allocate( NumVariables );
						PrimaryAirSystem( AirLoopNum ).Branch( BranchNum ).Comp( CompNum ).SubComp( SubCompNum ).MeteredVar.allocate( NumVariables );

						GetMeteredVariables( TypeOfComp, NameOfComp, VarIndexes, VarTypes, IndexTypes, UnitsStrings, ResourceTypes, EndUses, Groups, Names, NumFound );

						ModeFlagOn = true;
						for ( VarNum = 1; VarNum <= NumVariables; ++VarNum ) {
							PrimaryAirSystem( AirLoopNum ).Branch( BranchNum ).Comp( CompNum ).SubComp( SubCompNum ).MeteredVar( VarNum ).ReportVarName = Names( VarNum );
							PrimaryAirSystem( AirLoopNum ).Branch( BranchNum ).Comp( CompNum ).SubComp( SubCompNum ).MeteredVar( VarNum ).ReportVarUnits = UnitsStrings( VarNum );
							PrimaryAirSystem( AirLoopNum ).Branch( BranchNum ).Comp( CompNum ).SubComp( SubCompNum ).MeteredVar( VarNum ).ReportVarIndex = VarIndexes( VarNum );
							PrimaryAirSystem( AirLoopNum ).Branch( BranchNum ).Comp( CompNum ).SubComp( SubCompNum ).MeteredVar( VarNum ).ReportVarIndexType = IndexTypes( VarNum );
							PrimaryAirSystem( AirLoopNum ).Branch( BranchNum ).Comp( CompNum ).SubComp( SubCompNum ).MeteredVar( VarNum ).ReportVarType = VarTypes( VarNum );
							PrimaryAirSystem( AirLoopNum ).Branch( BranchNum ).Comp( CompNum ).SubComp( SubCompNum ).MeteredVar( VarNum ).ResourceType = ResourceTypes( VarNum );
							PrimaryAirSystem( AirLoopNum ).Branch( BranchNum ).Comp( CompNum ).SubComp( SubCompNum ).MeteredVar( VarNum ).EndUse = EndUses( VarNum );
							if ( PrimaryAirSystem( AirLoopNum ).Branch( BranchNum ).Comp( CompNum ).SubComp( SubCompNum ).MeteredVar( VarNum ).EndUse == "HEATINGCOILS" && ModeFlagOn ) {
								for ( VarNum1 = 1; VarNum1 <= NumVariables; ++VarNum1 ) {
									PrimaryAirSystem( AirLoopNum ).Branch( BranchNum ).Comp( CompNum ).SubComp( SubCompNum ).MeteredVar( VarNum1 ).EndUse_CompMode = HeatingOnly;
								}
								ModeFlagOn = false;
							} else if ( PrimaryAirSystem( AirLoopNum ).Branch( BranchNum ).Comp( CompNum ).SubComp( SubCompNum ).MeteredVar( VarNum ).EndUse == "COOLINGCOILS" && ModeFlagOn ) {
								for ( VarNum1 = 1; VarNum1 <= NumVariables; ++VarNum1 ) {
									PrimaryAirSystem( AirLoopNum ).Branch( BranchNum ).Comp( CompNum ).SubComp( SubCompNum ).MeteredVar( VarNum1 ).EndUse_CompMode = CoolingOnly;
								}
								ModeFlagOn = false;
							} else if ( ModeFlagOn ) {
								PrimaryAirSystem( AirLoopNum ).Branch( BranchNum ).Comp( CompNum ).SubComp( SubCompNum ).MeteredVar( VarNum ).EndUse_CompMode = NoHeatNoCool;
							}
							PrimaryAirSystem( AirLoopNum ).Branch( BranchNum ).Comp( CompNum ).SubComp( SubCompNum ).MeteredVar( VarNum ).Group = Groups( VarNum );
						}

						VarIndexes.deallocate();
						VarTypes.deallocate();
						IndexTypes.deallocate();
						UnitsStrings.deallocate();
						ResourceTypes.deallocate();
						EndUses.deallocate();
						Groups.deallocate();
						Names.deallocate();

						PrimaryAirSystem( AirLoopNum ).Branch( BranchNum ).Comp( CompNum ).SubComp( SubCompNum ).NumMeteredVars = NumVariables;

						for ( SubSubCompNum = 1; SubSubCompNum <= PrimaryAirSystem( AirLoopNum ).Branch( BranchNum ).Comp( CompNum ).SubComp( SubCompNum ).NumSubSubComps; ++SubSubCompNum ) {
							// Get complete list of components for complex branches
							TypeOfComp = PrimaryAirSystem( AirLoopNum ).Branch( BranchNum ).Comp( CompNum ).SubComp( SubCompNum ).SubSubComp( SubSubCompNum ).TypeOf;
							NameOfComp = PrimaryAirSystem( AirLoopNum ).Branch( BranchNum ).Comp( CompNum ).SubComp( SubCompNum ).SubSubComp( SubSubCompNum ).Name;
							NumVariables = GetNumMeteredVariables( TypeOfComp, NameOfComp );
							VarIndexes.allocate( NumVariables );
							VarTypes.allocate( NumVariables );
							IndexTypes.allocate( NumVariables );
							UnitsStrings.allocate( NumVariables );
							ResourceTypes.allocate( NumVariables );
							EndUses.allocate( NumVariables );
							Groups.allocate( NumVariables );
							Names.allocate( NumVariables );
							PrimaryAirSystem( AirLoopNum ).Branch( BranchNum ).Comp( CompNum ).SubComp( SubCompNum ).SubSubComp( SubSubCompNum ).MeteredVar.allocate( NumVariables );

							GetMeteredVariables( TypeOfComp, NameOfComp, VarIndexes, VarTypes, IndexTypes, UnitsStrings, ResourceTypes, EndUses, Groups, Names, NumFound );

							ModeFlagOn = true;
							for ( VarNum = 1; VarNum <= NumVariables; ++VarNum ) {
								PrimaryAirSystem( AirLoopNum ).Branch( BranchNum ).Comp( CompNum ).SubComp( SubCompNum ).SubSubComp( SubSubCompNum ).MeteredVar( VarNum ).ReportVarName = Names( VarNum );
								PrimaryAirSystem( AirLoopNum ).Branch( BranchNum ).Comp( CompNum ).SubComp( SubCompNum ).SubSubComp( SubSubCompNum ).MeteredVar( VarNum ).ReportVarUnits = UnitsStrings( VarNum );
								PrimaryAirSystem( AirLoopNum ).Branch( BranchNum ).Comp( CompNum ).SubComp( SubCompNum ).SubSubComp( SubSubCompNum ).MeteredVar( VarNum ).ReportVarIndex = VarIndexes( VarNum );
								PrimaryAirSystem( AirLoopNum ).Branch( BranchNum ).Comp( CompNum ).SubComp( SubCompNum ).SubSubComp( SubSubCompNum ).MeteredVar( VarNum ).ReportVarIndexType = IndexTypes( VarNum );
								PrimaryAirSystem( AirLoopNum ).Branch( BranchNum ).Comp( CompNum ).SubComp( SubCompNum ).SubSubComp( SubSubCompNum ).MeteredVar( VarNum ).ReportVarType = VarTypes( VarNum );
								PrimaryAirSystem( AirLoopNum ).Branch( BranchNum ).Comp( CompNum ).SubComp( SubCompNum ).SubSubComp( SubSubCompNum ).MeteredVar( VarNum ).ResourceType = ResourceTypes( VarNum );
								PrimaryAirSystem( AirLoopNum ).Branch( BranchNum ).Comp( CompNum ).SubComp( SubCompNum ).SubSubComp( SubSubCompNum ).MeteredVar( VarNum ).EndUse = EndUses( VarNum );
								if ( PrimaryAirSystem( AirLoopNum ).Branch( BranchNum ).Comp( CompNum ).SubComp( SubCompNum ).SubSubComp( SubSubCompNum ).MeteredVar( VarNum ).EndUse == "HEATINGCOILS" && ModeFlagOn ) {
									for ( VarNum1 = 1; VarNum1 <= NumVariables; ++VarNum1 ) {
										PrimaryAirSystem( AirLoopNum ).Branch( BranchNum ).Comp( CompNum ).SubComp( SubCompNum ).SubSubComp( SubSubCompNum ).MeteredVar( VarNum1 ).EndUse_CompMode = HeatingOnly;
									}
									ModeFlagOn = false;
								} else if ( PrimaryAirSystem( AirLoopNum ).Branch( BranchNum ).Comp( CompNum ).SubComp( SubCompNum ).SubSubComp( SubSubCompNum ).MeteredVar( VarNum ).EndUse == "COOLINGCOILS" && ModeFlagOn ) {
									for ( VarNum1 = 1; VarNum1 <= NumVariables; ++VarNum1 ) {
										PrimaryAirSystem( AirLoopNum ).Branch( BranchNum ).Comp( CompNum ).SubComp( SubCompNum ).SubSubComp( SubSubCompNum ).MeteredVar( VarNum1 ).EndUse_CompMode = CoolingOnly;
									}
									ModeFlagOn = false;
								} else if ( ModeFlagOn ) {
									PrimaryAirSystem( AirLoopNum ).Branch( BranchNum ).Comp( CompNum ).SubComp( SubCompNum ).SubSubComp( SubSubCompNum ).MeteredVar( VarNum ).EndUse_CompMode = NoHeatNoCool;
								}
								PrimaryAirSystem( AirLoopNum ).Branch( BranchNum ).Comp( CompNum ).SubComp( SubCompNum ).SubSubComp( SubSubCompNum ).MeteredVar( VarNum ).Group = Groups( VarNum );
							}

							VarIndexes.deallocate();
							VarTypes.deallocate();
							IndexTypes.deallocate();
							UnitsStrings.deallocate();
							ResourceTypes.deallocate();
							EndUses.deallocate();
							Groups.deallocate();
							Names.deallocate();

							PrimaryAirSystem( AirLoopNum ).Branch( BranchNum ).Comp( CompNum ).SubComp( SubCompNum ).SubSubComp( SubSubCompNum ).NumMeteredVars = NumVariables;
						}
					}

				}
			}
		}

		// Allocate the system serving zone equipment component arrays
		for ( CtrlZoneNum = 1; CtrlZoneNum <= NumOfZones; ++CtrlZoneNum ) {
			if ( ! ZoneEquipConfig( CtrlZoneNum ).IsControlled ) continue;
			// Set index of air loop serving zone
			for ( CompNum = 1; CompNum <= ZoneEquipList( CtrlZoneNum ).NumOfEquipTypes; ++CompNum ) {
				TypeOfComp = ZoneEquipList( CtrlZoneNum ).EquipType( CompNum );
				NameOfComp = ZoneEquipList( CtrlZoneNum ).EquipName( CompNum );
				GetComponentData( TypeOfComp, NameOfComp, IsParent, NumInlets, InletNodeNames, InletNodeNumbers, InletFluidStreams, NumOutlets, OutletNodeNames, OutletNodeNumbers, OutletFluidStreams, ErrorsFound );
				ZoneEquipList( CtrlZoneNum ).EquipData( CompNum ).TypeOf = TypeOfComp;
				ZoneEquipList( CtrlZoneNum ).EquipData( CompNum ).Name = NameOfComp;
				ZoneEquipList( CtrlZoneNum ).EquipData( CompNum ).OutletNodeNums.allocate( NumOutlets );
				ZoneEquipList( CtrlZoneNum ).EquipData( CompNum ).NumOutlets = NumOutlets;
				ZoneEquipList( CtrlZoneNum ).EquipData( CompNum ).OutletNodeNums = OutletNodeNumbers;
				ZoneEquipList( CtrlZoneNum ).EquipData( CompNum ).InletNodeNums.allocate( NumInlets );
				ZoneEquipList( CtrlZoneNum ).EquipData( CompNum ).NumInlets = NumInlets;
				ZoneEquipList( CtrlZoneNum ).EquipData( CompNum ).InletNodeNums = InletNodeNumbers;
				ZoneEquipList( CtrlZoneNum ).EquipData( CompNum ).Parent = IsParent;
				NumVariables = GetNumMeteredVariables( TypeOfComp, NameOfComp );
				ZoneEquipList( CtrlZoneNum ).EquipData( CompNum ).NumMeteredVars = NumVariables;
				InletNodeNames.deallocate();
				InletNodeNumbers.deallocate();
				InletFluidStreams.deallocate();
				OutletNodeNames.deallocate();
				OutletNodeNumbers.deallocate();
				OutletFluidStreams.deallocate();

				VarIndexes.allocate( NumVariables );
				VarTypes.allocate( NumVariables );
				IndexTypes.allocate( NumVariables );
				UnitsStrings.allocate( NumVariables );
				ResourceTypes.allocate( NumVariables );
				EndUses.allocate( NumVariables );
				Groups.allocate( NumVariables );
				Names.allocate( NumVariables );
				ZoneEquipList( CtrlZoneNum ).EquipData( CompNum ).MeteredVar.allocate( NumVariables );

				GetMeteredVariables( TypeOfComp, NameOfComp, VarIndexes, VarTypes, IndexTypes, UnitsStrings, ResourceTypes, EndUses, Groups, Names, NumFound );

				ModeFlagOn = true;
				for ( VarNum = 1; VarNum <= NumVariables; ++VarNum ) {
					ZoneEquipList( CtrlZoneNum ).EquipData( CompNum ).MeteredVar( VarNum ).ReportVarName = Names( VarNum );
					ZoneEquipList( CtrlZoneNum ).EquipData( CompNum ).MeteredVar( VarNum ).ReportVarUnits = UnitsStrings( VarNum );
					ZoneEquipList( CtrlZoneNum ).EquipData( CompNum ).MeteredVar( VarNum ).ReportVarIndex = VarIndexes( VarNum );
					ZoneEquipList( CtrlZoneNum ).EquipData( CompNum ).MeteredVar( VarNum ).ReportVarIndexType = IndexTypes( VarNum );
					ZoneEquipList( CtrlZoneNum ).EquipData( CompNum ).MeteredVar( VarNum ).ReportVarType = VarTypes( VarNum );
					ZoneEquipList( CtrlZoneNum ).EquipData( CompNum ).MeteredVar( VarNum ).ResourceType = ResourceTypes( VarNum );
					ZoneEquipList( CtrlZoneNum ).EquipData( CompNum ).MeteredVar( VarNum ).EndUse = EndUses( VarNum );
					if ( ZoneEquipList( CtrlZoneNum ).EquipData( CompNum ).MeteredVar( VarNum ).EndUse == "HEATINGCOILS" && ModeFlagOn ) {
						for ( VarNum1 = 1; VarNum1 <= NumVariables; ++VarNum1 ) {
							ZoneEquipList( CtrlZoneNum ).EquipData( CompNum ).MeteredVar( VarNum1 ).EndUse_CompMode = HeatingOnly;
						}
						ModeFlagOn = false;
					} else if ( ZoneEquipList( CtrlZoneNum ).EquipData( CompNum ).MeteredVar( VarNum ).EndUse == "COOLINGCOILS" && ModeFlagOn ) {
						for ( VarNum1 = 1; VarNum1 <= NumVariables; ++VarNum1 ) {
							ZoneEquipList( CtrlZoneNum ).EquipData( CompNum ).MeteredVar( VarNum1 ).EndUse_CompMode = CoolingOnly;
						}
						ModeFlagOn = false;
					} else if ( ModeFlagOn ) {
						ZoneEquipList( CtrlZoneNum ).EquipData( CompNum ).MeteredVar( VarNum ).EndUse_CompMode = NoHeatNoCool;
					}
					ZoneEquipList( CtrlZoneNum ).EquipData( CompNum ).MeteredVar( VarNum ).Group = Groups( VarNum );
				}

				VarIndexes.deallocate();
				VarTypes.deallocate();
				IndexTypes.deallocate();
				UnitsStrings.deallocate();
				ResourceTypes.deallocate();
				EndUses.deallocate();
				Groups.deallocate();
				Names.deallocate();

				if ( IsParentObject( TypeOfComp, NameOfComp ) ) {
					NumChildren = GetNumChildren( TypeOfComp, NameOfComp );
					ZoneEquipList( CtrlZoneNum ).EquipData( CompNum ).NumSubEquip = NumChildren;

					SubCompTypes.allocate( NumChildren );
					SubCompNames.allocate( NumChildren );
					InletNodeNames.allocate( NumChildren );
					InletNodeNumbers.allocate( NumChildren );
					OutletNodeNames.allocate( NumChildren );
					OutletNodeNumbers.allocate( NumChildren );
					ZoneEquipList( CtrlZoneNum ).EquipData( CompNum ).SubEquipData.allocate( NumChildren );

					GetChildrenData( TypeOfComp, NameOfComp, NumChildren, SubCompTypes, SubCompNames, InletNodeNames, InletNodeNumbers, OutletNodeNames, OutletNodeNumbers, ErrorsFound );

					for ( SubCompNum = 1; SubCompNum <= NumChildren; ++SubCompNum ) {
						ZoneEquipList( CtrlZoneNum ).EquipData( CompNum ).SubEquipData( SubCompNum ).TypeOf = SubCompTypes( SubCompNum );
						ZoneEquipList( CtrlZoneNum ).EquipData( CompNum ).SubEquipData( SubCompNum ).Name = SubCompNames( SubCompNum );
						ZoneEquipList( CtrlZoneNum ).EquipData( CompNum ).SubEquipData( SubCompNum ).OutletNodeNum = OutletNodeNumbers( SubCompNum );
						ZoneEquipList( CtrlZoneNum ).EquipData( CompNum ).SubEquipData( SubCompNum ).InletNodeNum = InletNodeNumbers( SubCompNum );
					}

					SubCompTypes.deallocate();
					SubCompNames.deallocate();
					InletNodeNames.deallocate();
					InletNodeNumbers.deallocate();
					OutletNodeNames.deallocate();
					OutletNodeNumbers.deallocate();
				} else {
					NumChildren = 0;
				}

				for ( SubCompNum = 1; SubCompNum <= NumChildren; ++SubCompNum ) {
					TypeOfComp = ZoneEquipList( CtrlZoneNum ).EquipData( CompNum ).SubEquipData( SubCompNum ).TypeOf;
					NameOfComp = ZoneEquipList( CtrlZoneNum ).EquipData( CompNum ).SubEquipData( SubCompNum ).Name;
					if ( IsParentObject( TypeOfComp, NameOfComp ) ) {
						NumGrandChildren = GetNumChildren( TypeOfComp, NameOfComp );
						ZoneEquipList( CtrlZoneNum ).EquipData( CompNum ).SubEquipData( SubCompNum ).NumSubSubEquip = NumGrandChildren;
						SubCompTypes.allocate( NumGrandChildren );
						SubCompNames.allocate( NumGrandChildren );
						InletNodeNames.allocate( NumGrandChildren );
						InletNodeNumbers.allocate( NumGrandChildren );
						OutletNodeNames.allocate( NumGrandChildren );
						OutletNodeNumbers.allocate( NumGrandChildren );
						ZoneEquipList( CtrlZoneNum ).EquipData( CompNum ).SubEquipData( SubCompNum ).SubSubEquipData.allocate( NumGrandChildren );
						//Sankar added the array number for EquipData
						GetChildrenData( TypeOfComp, NameOfComp, NumGrandChildren, SubCompTypes, SubCompNames, InletNodeNames, InletNodeNumbers, OutletNodeNames, OutletNodeNumbers, ErrorsFound );

						for ( SubSubCompNum = 1; SubSubCompNum <= NumGrandChildren; ++SubSubCompNum ) {
							ZoneEquipList( CtrlZoneNum ).EquipData( CompNum ).SubEquipData( SubCompNum ).SubSubEquipData( SubSubCompNum ).TypeOf = SubCompTypes( SubSubCompNum );
							ZoneEquipList( CtrlZoneNum ).EquipData( CompNum ).SubEquipData( SubCompNum ).SubSubEquipData( SubSubCompNum ).Name = SubCompNames( SubSubCompNum );
							ZoneEquipList( CtrlZoneNum ).EquipData( CompNum ).SubEquipData( SubCompNum ).SubSubEquipData( SubSubCompNum ).OutletNodeNum = OutletNodeNumbers( SubSubCompNum );
							ZoneEquipList( CtrlZoneNum ).EquipData( CompNum ).SubEquipData( SubCompNum ).SubSubEquipData( SubSubCompNum ).InletNodeNum = InletNodeNumbers( SubSubCompNum );
						}
						SubCompTypes.deallocate();
						SubCompNames.deallocate();
						InletNodeNames.deallocate();
						InletNodeNumbers.deallocate();
						OutletNodeNames.deallocate();
						OutletNodeNumbers.deallocate();
					} else {
						NumGrandChildren = 0;
					}
				}
			}
		}

		for ( CtrlZoneNum = 1; CtrlZoneNum <= NumOfZones; ++CtrlZoneNum ) {
			if ( ! ZoneEquipConfig( CtrlZoneNum ).IsControlled ) continue;
			for ( CompNum = 1; CompNum <= ZoneEquipList( CtrlZoneNum ).NumOfEquipTypes; ++CompNum ) {
				for ( SubCompNum = 1; SubCompNum <= ZoneEquipList( CtrlZoneNum ).EquipData( CompNum ).NumSubEquip; ++SubCompNum ) {
					TypeOfComp = ZoneEquipList( CtrlZoneNum ).EquipData( CompNum ).SubEquipData( SubCompNum ).TypeOf;
					NameOfComp = ZoneEquipList( CtrlZoneNum ).EquipData( CompNum ).SubEquipData( SubCompNum ).Name;

					NumVariables = GetNumMeteredVariables( TypeOfComp, NameOfComp );
					ZoneEquipList( CtrlZoneNum ).EquipData( CompNum ).SubEquipData( SubCompNum ).NumMeteredVars = NumVariables; //Sankar added this line
					VarIndexes.allocate( NumVariables );
					VarTypes.allocate( NumVariables );
					IndexTypes.allocate( NumVariables );
					UnitsStrings.allocate( NumVariables );
					ResourceTypes.allocate( NumVariables );
					EndUses.allocate( NumVariables );
					Groups.allocate( NumVariables );
					Names.allocate( NumVariables );
					ZoneEquipList( CtrlZoneNum ).EquipData( CompNum ).SubEquipData( SubCompNum ).MeteredVar.allocate( NumVariables );

					GetMeteredVariables( TypeOfComp, NameOfComp, VarIndexes, VarTypes, IndexTypes, UnitsStrings, ResourceTypes, EndUses, Groups, Names, NumFound );

					ModeFlagOn = true;
					for ( VarNum = 1; VarNum <= NumVariables; ++VarNum ) {
						ZoneEquipList( CtrlZoneNum ).EquipData( CompNum ).SubEquipData( SubCompNum ).MeteredVar( VarNum ).ReportVarName = Names( VarNum );
						ZoneEquipList( CtrlZoneNum ).EquipData( CompNum ).SubEquipData( SubCompNum ).MeteredVar( VarNum ).ReportVarUnits = UnitsStrings( VarNum );
						ZoneEquipList( CtrlZoneNum ).EquipData( CompNum ).SubEquipData( SubCompNum ).MeteredVar( VarNum ).ReportVarIndex = VarIndexes( VarNum );
						ZoneEquipList( CtrlZoneNum ).EquipData( CompNum ).SubEquipData( SubCompNum ).MeteredVar( VarNum ).ReportVarIndexType = IndexTypes( VarNum );
						ZoneEquipList( CtrlZoneNum ).EquipData( CompNum ).SubEquipData( SubCompNum ).MeteredVar( VarNum ).ReportVarType = VarTypes( VarNum );
						ZoneEquipList( CtrlZoneNum ).EquipData( CompNum ).SubEquipData( SubCompNum ).MeteredVar( VarNum ).ResourceType = ResourceTypes( VarNum );
						ZoneEquipList( CtrlZoneNum ).EquipData( CompNum ).SubEquipData( SubCompNum ).MeteredVar( VarNum ).EndUse = EndUses( VarNum );
						if ( ZoneEquipList( CtrlZoneNum ).EquipData( CompNum ).SubEquipData( SubCompNum ).MeteredVar( VarNum ).EndUse == "HEATINGCOILS" && ModeFlagOn ) {
							for ( VarNum1 = 1; VarNum1 <= NumVariables; ++VarNum1 ) {
								ZoneEquipList( CtrlZoneNum ).EquipData( CompNum ).SubEquipData( SubCompNum ).MeteredVar( VarNum1 ).EndUse_CompMode = HeatingOnly;
							}
							ModeFlagOn = false;
						} else if ( ZoneEquipList( CtrlZoneNum ).EquipData( CompNum ).SubEquipData( SubCompNum ).MeteredVar( VarNum ).EndUse == "COOLINGCOILS" && ModeFlagOn ) {
							for ( VarNum1 = 1; VarNum1 <= NumVariables; ++VarNum1 ) {
								ZoneEquipList( CtrlZoneNum ).EquipData( CompNum ).SubEquipData( SubCompNum ).MeteredVar( VarNum1 ).EndUse_CompMode = CoolingOnly;
							}
							ModeFlagOn = false;
						} else if ( ModeFlagOn ) {
							ZoneEquipList( CtrlZoneNum ).EquipData( CompNum ).SubEquipData( SubCompNum ).MeteredVar( VarNum ).EndUse_CompMode = NoHeatNoCool;
						}
						ZoneEquipList( CtrlZoneNum ).EquipData( CompNum ).SubEquipData( SubCompNum ).MeteredVar( VarNum ).Group = Groups( VarNum );
					}

					VarIndexes.deallocate();
					VarTypes.deallocate();
					IndexTypes.deallocate();
					UnitsStrings.deallocate();
					ResourceTypes.deallocate();
					EndUses.deallocate();
					Groups.deallocate();
					Names.deallocate();

					for ( SubSubCompNum = 1; SubSubCompNum <= ZoneEquipList( CtrlZoneNum ).EquipData( CompNum ).SubEquipData( SubCompNum ).NumSubSubEquip; ++SubSubCompNum ) {
						TypeOfComp = ZoneEquipList( CtrlZoneNum ).EquipData( CompNum ).SubEquipData( SubCompNum ).SubSubEquipData( SubSubCompNum ).TypeOf;
						NameOfComp = ZoneEquipList( CtrlZoneNum ).EquipData( CompNum ).SubEquipData( SubCompNum ).SubSubEquipData( SubSubCompNum ).Name;

						NumVariables = GetNumMeteredVariables( TypeOfComp, NameOfComp );
						ZoneEquipList( CtrlZoneNum ).EquipData( CompNum ).SubEquipData( SubCompNum ).SubSubEquipData( SubSubCompNum ).NumMeteredVars = NumVariables; //Sankar added this line
						VarIndexes.allocate( NumVariables );
						VarTypes.allocate( NumVariables );
						IndexTypes.allocate( NumVariables );
						UnitsStrings.allocate( NumVariables );
						ResourceTypes.allocate( NumVariables );
						EndUses.allocate( NumVariables );
						Groups.allocate( NumVariables );
						Names.allocate( NumVariables );
						ZoneEquipList( CtrlZoneNum ).EquipData( CompNum ).SubEquipData( SubCompNum ).SubSubEquipData( SubSubCompNum ).MeteredVar.allocate( NumVariables );

						GetMeteredVariables( TypeOfComp, NameOfComp, VarIndexes, VarTypes, IndexTypes, UnitsStrings, ResourceTypes, EndUses, Groups, Names, NumFound );

						ModeFlagOn = true;
						for ( VarNum = 1; VarNum <= NumVariables; ++VarNum ) {
							ZoneEquipList( CtrlZoneNum ).EquipData( CompNum ).SubEquipData( SubCompNum ).SubSubEquipData( SubSubCompNum ).MeteredVar( VarNum ).ReportVarName = Names( VarNum );
							ZoneEquipList( CtrlZoneNum ).EquipData( CompNum ).SubEquipData( SubCompNum ).SubSubEquipData( SubSubCompNum ).MeteredVar( VarNum ).ReportVarUnits = UnitsStrings( VarNum );
							ZoneEquipList( CtrlZoneNum ).EquipData( CompNum ).SubEquipData( SubCompNum ).SubSubEquipData( SubSubCompNum ).MeteredVar( VarNum ).ReportVarIndex = VarIndexes( VarNum );
							ZoneEquipList( CtrlZoneNum ).EquipData( CompNum ).SubEquipData( SubCompNum ).SubSubEquipData( SubSubCompNum ).MeteredVar( VarNum ).ReportVarIndexType = IndexTypes( VarNum );
							ZoneEquipList( CtrlZoneNum ).EquipData( CompNum ).SubEquipData( SubCompNum ).SubSubEquipData( SubSubCompNum ).MeteredVar( VarNum ).ReportVarType = VarTypes( VarNum );
							ZoneEquipList( CtrlZoneNum ).EquipData( CompNum ).SubEquipData( SubCompNum ).SubSubEquipData( SubSubCompNum ).MeteredVar( VarNum ).ResourceType = ResourceTypes( VarNum );
							ZoneEquipList( CtrlZoneNum ).EquipData( CompNum ).SubEquipData( SubCompNum ).SubSubEquipData( SubSubCompNum ).MeteredVar( VarNum ).EndUse = EndUses( VarNum );
							if ( ZoneEquipList( CtrlZoneNum ).EquipData( CompNum ).SubEquipData( SubCompNum ).SubSubEquipData( SubSubCompNum ).MeteredVar( VarNum ).EndUse == "HEATINGCOILS" && ModeFlagOn ) {
								for ( VarNum1 = 1; VarNum1 <= NumVariables; ++VarNum1 ) {
									ZoneEquipList( CtrlZoneNum ).EquipData( CompNum ).SubEquipData( SubCompNum ).SubSubEquipData( SubSubCompNum ).MeteredVar( VarNum1 ).EndUse_CompMode = HeatingOnly;
								}
								ModeFlagOn = false;
							} else if ( ZoneEquipList( CtrlZoneNum ).EquipData( CompNum ).SubEquipData( SubCompNum ).SubSubEquipData( SubSubCompNum ).MeteredVar( VarNum ).EndUse == "COOLINGCOILS" && ModeFlagOn ) {
								for ( VarNum1 = 1; VarNum1 <= NumVariables; ++VarNum1 ) {
									ZoneEquipList( CtrlZoneNum ).EquipData( CompNum ).SubEquipData( SubCompNum ).SubSubEquipData( SubSubCompNum ).MeteredVar( VarNum1 ).EndUse_CompMode = CoolingOnly;
								}
								ModeFlagOn = false;
							} else if ( ModeFlagOn ) {
								ZoneEquipList( CtrlZoneNum ).EquipData( CompNum ).SubEquipData( SubCompNum ).SubSubEquipData( SubSubCompNum ).MeteredVar( VarNum ).EndUse_CompMode = NoHeatNoCool;
							}
							ZoneEquipList( CtrlZoneNum ).EquipData( CompNum ).SubEquipData( SubCompNum ).SubSubEquipData( SubSubCompNum ).MeteredVar( VarNum ).Group = Groups( VarNum );
						}

						VarIndexes.deallocate();
						VarTypes.deallocate();
						IndexTypes.deallocate();
						UnitsStrings.deallocate();
						ResourceTypes.deallocate();
						EndUses.deallocate();
						Groups.deallocate();
						Names.deallocate();

					}
				}
			}
		}

		//***Plant Loops

		// previously, four separate huge DO loops all looking very very similar were used here
		// each individual block would operate on a single type of loop-side (plant demand, cond supply, etc.)
		// now, a bigger DO loop is applied iterating over all loops
		// a pointer (ThisReportData) is then directed to a particular item in the appropriate array
		// by operating on the pointer directly, we are actually operating on the item in the TARGET array item
		// in making this change, over 700 lines of code were dropped down to a single block

		for ( PlantLoopNum = 1; PlantLoopNum <= NumPlantLoops + NumCondLoops; ++PlantLoopNum ) {
			for ( LoopSideNum = DemandSide; LoopSideNum <= SupplySide; ++LoopSideNum ) {

				// Report selection
				ReportLoopData * select_ThisReportData( nullptr );

				if ( PlantLoopNum <= NumPlantLoops ) {
					{ auto const SELECT_CASE_var( LoopSideNum );
					if ( SELECT_CASE_var == DemandSide ) {
						select_ThisReportData = &VentRepPlantDemandSide( PlantLoopNum );
					} else if ( SELECT_CASE_var == SupplySide ) {
						select_ThisReportData = &VentRepPlantSupplySide( PlantLoopNum );
					} else {
						assert( false );
					}}
				} else { // CondLoop
					{ auto const SELECT_CASE_var( LoopSideNum );
					if ( SELECT_CASE_var == DemandSide ) {
						select_ThisReportData = &VentRepCondDemandSide( PlantLoopNum - NumPlantLoops );
					} else if ( SELECT_CASE_var == SupplySide ) {
						select_ThisReportData = &VentRepCondSupplySide( PlantLoopNum - NumPlantLoops );
					} else {
						assert( false );
					}}
				}

				// Object Data
				ReportLoopData & ThisReportData( *select_ThisReportData );

				for ( BranchNum = 1; BranchNum <= ThisReportData.TotalBranches; ++BranchNum ) {
					for ( CompNum = 1; CompNum <= ThisReportData.Branch( BranchNum ).TotalComponents; ++CompNum ) {
						TypeOfComp = ThisReportData.Branch( BranchNum ).Comp( CompNum ).TypeOf;
						NameOfComp = ThisReportData.Branch( BranchNum ).Comp( CompNum ).Name;
						// Get complete list of components for complex branches
						if ( IsParentObject( TypeOfComp, NameOfComp ) ) {

							ThisReportData.Branch( BranchNum ).Comp( CompNum ).Parent = true;
							NumChildren = GetNumChildren( TypeOfComp, NameOfComp );

							SubCompTypes.allocate( NumChildren );
							SubCompNames.allocate( NumChildren );
							InletNodeNames.allocate( NumChildren );
							InletNodeNumbers.allocate( NumChildren );
							OutletNodeNames.allocate( NumChildren );
							OutletNodeNumbers.allocate( NumChildren );
							ThisReportData.Branch( BranchNum ).Comp( CompNum ).SubComp.allocate( NumChildren );

							GetChildrenData( TypeOfComp, NameOfComp, NumChildren, SubCompTypes, SubCompNames, InletNodeNames, InletNodeNumbers, OutletNodeNames, OutletNodeNumbers, ErrorsFound );

							for ( SubCompNum = 1; SubCompNum <= NumChildren; ++SubCompNum ) {
								ThisReportData.Branch( BranchNum ).Comp( CompNum ).SubComp( SubCompNum ).TypeOf = SubCompTypes( SubCompNum );
								ThisReportData.Branch( BranchNum ).Comp( CompNum ).SubComp( SubCompNum ).Name = SubCompNames( SubCompNum );
								ThisReportData.Branch( BranchNum ).Comp( CompNum ).SubComp( SubCompNum ).NodeNameIn = InletNodeNames( SubCompNum );
								ThisReportData.Branch( BranchNum ).Comp( CompNum ).SubComp( SubCompNum ).NodeNameOut = OutletNodeNames( SubCompNum );
								ThisReportData.Branch( BranchNum ).Comp( CompNum ).SubComp( SubCompNum ).NodeNumIn = InletNodeNumbers( SubCompNum );
								ThisReportData.Branch( BranchNum ).Comp( CompNum ).SubComp( SubCompNum ).NodeNumOut = OutletNodeNumbers( SubCompNum );
							}

							SubCompTypes.deallocate();
							SubCompNames.deallocate();
							InletNodeNames.deallocate();
							InletNodeNumbers.deallocate();
							OutletNodeNames.deallocate();
							OutletNodeNumbers.deallocate();

						} else {
							NumChildren = 0;
							ThisReportData.Branch( BranchNum ).Comp( CompNum ).Parent = false;
						}
						ThisReportData.Branch( BranchNum ).Comp( CompNum ).NumSubComps = NumChildren;

						//check for 'grandchildren'
						for ( SubCompNum = 1; SubCompNum <= NumChildren; ++SubCompNum ) {
							TypeOfComp = ThisReportData.Branch( BranchNum ).Comp( CompNum ).SubComp( SubCompNum ).TypeOf;
							NameOfComp = ThisReportData.Branch( BranchNum ).Comp( CompNum ).SubComp( SubCompNum ).Name;
							if ( IsParentObject( TypeOfComp, NameOfComp ) ) {
								NumGrandChildren = GetNumChildren( TypeOfComp, NameOfComp );
								SubCompTypes.allocate( NumGrandChildren );
								SubCompNames.allocate( NumGrandChildren );
								InletNodeNames.allocate( NumGrandChildren );
								InletNodeNumbers.allocate( NumGrandChildren );
								OutletNodeNames.allocate( NumGrandChildren );
								OutletNodeNumbers.allocate( NumGrandChildren );
								ThisReportData.Branch( BranchNum ).Comp( CompNum ).SubComp( SubCompNum ).SubSubComp.allocate( NumGrandChildren );

								GetChildrenData( TypeOfComp, NameOfComp, NumGrandChildren, SubCompTypes, SubCompNames, InletNodeNames, InletNodeNumbers, OutletNodeNames, OutletNodeNumbers, ErrorsFound );

								for ( SubSubCompNum = 1; SubSubCompNum <= NumGrandChildren; ++SubSubCompNum ) {
									ThisReportData.Branch( BranchNum ).Comp( CompNum ).SubComp( SubCompNum ).SubSubComp( SubSubCompNum ).TypeOf = SubCompTypes( SubSubCompNum );
									ThisReportData.Branch( BranchNum ).Comp( CompNum ).SubComp( SubCompNum ).SubSubComp( SubSubCompNum ).Name = SubCompNames( SubSubCompNum );
									ThisReportData.Branch( BranchNum ).Comp( CompNum ).SubComp( SubCompNum ).SubSubComp( SubSubCompNum ).NodeNameIn = InletNodeNames( SubSubCompNum );
									ThisReportData.Branch( BranchNum ).Comp( CompNum ).SubComp( SubCompNum ).SubSubComp( SubSubCompNum ).NodeNameOut = OutletNodeNames( SubSubCompNum );
									ThisReportData.Branch( BranchNum ).Comp( CompNum ).SubComp( SubCompNum ).SubSubComp( SubSubCompNum ).NodeNumIn = InletNodeNumbers( SubSubCompNum );
									ThisReportData.Branch( BranchNum ).Comp( CompNum ).SubComp( SubCompNum ).SubSubComp( SubSubCompNum ).NodeNumOut = OutletNodeNumbers( SubSubCompNum );
								}

								SubCompTypes.deallocate();
								SubCompNames.deallocate();
								InletNodeNames.deallocate();
								InletNodeNumbers.deallocate();
								OutletNodeNames.deallocate();
								OutletNodeNumbers.deallocate();
							} else {
								NumGrandChildren = 0;
								ThisReportData.Branch( BranchNum ).Comp( CompNum ).SubComp( SubCompNum ).Parent = false;
							}

							ThisReportData.Branch( BranchNum ).Comp( CompNum ).SubComp( SubCompNum ).NumSubSubComps = NumGrandChildren;

						}
					}
				}
			}
		}

		for ( PlantLoopNum = 1; PlantLoopNum <= NumPlantLoops + NumCondLoops; ++PlantLoopNum ) {

			for ( LoopSideNum = DemandSide; LoopSideNum <= SupplySide; ++LoopSideNum ) {

				// Report selection
				ReportLoopData * select_ThisReportData( nullptr );

				if ( PlantLoopNum <= NumPlantLoops ) {
					{ auto const SELECT_CASE_var( LoopSideNum );
					if ( SELECT_CASE_var == DemandSide ) {
						select_ThisReportData = &VentRepPlantDemandSide( PlantLoopNum );
					} else if ( SELECT_CASE_var == SupplySide ) {
						select_ThisReportData = &VentRepPlantSupplySide( PlantLoopNum );
					} else {
						assert( false );
					}}
				} else { // CondLoop
					{ auto const SELECT_CASE_var( LoopSideNum );
					if ( SELECT_CASE_var == DemandSide ) {
						select_ThisReportData = &VentRepCondDemandSide( PlantLoopNum - NumPlantLoops );
					} else if ( SELECT_CASE_var == SupplySide ) {
						select_ThisReportData = &VentRepCondSupplySide( PlantLoopNum - NumPlantLoops );
					} else {
						assert( false );
					}}
				}

				// Object Data
				ReportLoopData & ThisReportData( *select_ThisReportData );

				for ( BranchNum = 1; BranchNum <= ThisReportData.TotalBranches; ++BranchNum ) {
					for ( CompNum = 1; CompNum <= ThisReportData.Branch( BranchNum ).TotalComponents; ++CompNum ) {
						// Get complete list of components for complex branches
						TypeOfComp = ThisReportData.Branch( BranchNum ).Comp( CompNum ).TypeOf;
						NameOfComp = ThisReportData.Branch( BranchNum ).Comp( CompNum ).Name;
						NumVariables = GetNumMeteredVariables( TypeOfComp, NameOfComp );
						VarIndexes.allocate( NumVariables );
						VarTypes.allocate( NumVariables );
						IndexTypes.allocate( NumVariables );
						UnitsStrings.allocate( NumVariables );
						ResourceTypes.allocate( NumVariables );
						EndUses.allocate( NumVariables );
						Groups.allocate( NumVariables );
						Names.allocate( NumVariables );
						ThisReportData.Branch( BranchNum ).Comp( CompNum ).MeteredVar.allocate( NumVariables );

						ThisReportData.Branch( BranchNum ).Comp( CompNum ).NumMeteredVars = NumVariables;
						GetMeteredVariables( TypeOfComp, NameOfComp, VarIndexes, VarTypes, IndexTypes, UnitsStrings, ResourceTypes, EndUses, Groups, Names, NumFound );

						ModeFlagOn = true;
						for ( VarNum = 1; VarNum <= NumVariables; ++VarNum ) {
							ThisReportData.Branch( BranchNum ).Comp( CompNum ).MeteredVar( VarNum ).ReportVarName = Names( VarNum );
							ThisReportData.Branch( BranchNum ).Comp( CompNum ).MeteredVar( VarNum ).ReportVarUnits = UnitsStrings( VarNum );
							ThisReportData.Branch( BranchNum ).Comp( CompNum ).MeteredVar( VarNum ).ReportVarIndex = VarIndexes( VarNum );
							ThisReportData.Branch( BranchNum ).Comp( CompNum ).MeteredVar( VarNum ).ReportVarIndexType = IndexTypes( VarNum );
							ThisReportData.Branch( BranchNum ).Comp( CompNum ).MeteredVar( VarNum ).ReportVarType = VarTypes( VarNum );
							ThisReportData.Branch( BranchNum ).Comp( CompNum ).MeteredVar( VarNum ).ResourceType = ResourceTypes( VarNum );
							ThisReportData.Branch( BranchNum ).Comp( CompNum ).MeteredVar( VarNum ).EndUse = EndUses( VarNum );
							if ( ThisReportData.Branch( BranchNum ).Comp( CompNum ).MeteredVar( VarNum ).EndUse == "HEATINGCOILS" && ModeFlagOn ) {
								for ( VarNum1 = 1; VarNum1 <= NumVariables; ++VarNum1 ) {
									ThisReportData.Branch( BranchNum ).Comp( CompNum ).MeteredVar( VarNum1 ).EndUse_CompMode = HeatingOnly;
								}
								ModeFlagOn = false;
							} else if ( ThisReportData.Branch( BranchNum ).Comp( CompNum ).MeteredVar( VarNum ).EndUse == "COOLINGCOILS" && ModeFlagOn ) {
								for ( VarNum1 = 1; VarNum1 <= NumVariables; ++VarNum1 ) {
									ThisReportData.Branch( BranchNum ).Comp( CompNum ).MeteredVar( VarNum1 ).EndUse_CompMode = CoolingOnly;
								}
								ModeFlagOn = false;
							} else if ( ModeFlagOn ) {
								ThisReportData.Branch( BranchNum ).Comp( CompNum ).MeteredVar( VarNum ).EndUse_CompMode = NoHeatNoCool;
							}
							ThisReportData.Branch( BranchNum ).Comp( CompNum ).MeteredVar( VarNum ).Group = Groups( VarNum );
						}

						VarIndexes.deallocate();
						VarTypes.deallocate();
						IndexTypes.deallocate();
						UnitsStrings.deallocate();
						ResourceTypes.deallocate();
						EndUses.deallocate();
						Groups.deallocate();
						Names.deallocate();

						for ( SubCompNum = 1; SubCompNum <= ThisReportData.Branch( BranchNum ).Comp( CompNum ).NumSubComps; ++SubCompNum ) {
							// Get complete list of components for complex branches
							TypeOfComp = ThisReportData.Branch( BranchNum ).Comp( CompNum ).SubComp( SubCompNum ).TypeOf;
							NameOfComp = ThisReportData.Branch( BranchNum ).Comp( CompNum ).SubComp( SubCompNum ).Name;
							NumVariables = GetNumMeteredVariables( TypeOfComp, NameOfComp );
							VarIndexes.allocate( NumVariables );
							VarTypes.allocate( NumVariables );
							IndexTypes.allocate( NumVariables );
							UnitsStrings.allocate( NumVariables );
							ResourceTypes.allocate( NumVariables );
							EndUses.allocate( NumVariables );
							Groups.allocate( NumVariables );
							Names.allocate( NumVariables );
							ThisReportData.Branch( BranchNum ).Comp( CompNum ).SubComp( SubCompNum ).MeteredVar.allocate( NumVariables );

							GetMeteredVariables( TypeOfComp, NameOfComp, VarIndexes, VarTypes, IndexTypes, UnitsStrings, ResourceTypes, EndUses, Groups, Names, NumFound );

							ModeFlagOn = true;
							for ( VarNum = 1; VarNum <= NumVariables; ++VarNum ) {
								ThisReportData.Branch( BranchNum ).Comp( CompNum ).SubComp( SubCompNum ).MeteredVar( VarNum ).ReportVarName = Names( VarNum );
								ThisReportData.Branch( BranchNum ).Comp( CompNum ).SubComp( SubCompNum ).MeteredVar( VarNum ).ReportVarUnits = UnitsStrings( VarNum );
								ThisReportData.Branch( BranchNum ).Comp( CompNum ).SubComp( SubCompNum ).MeteredVar( VarNum ).ReportVarIndex = VarIndexes( VarNum );
								ThisReportData.Branch( BranchNum ).Comp( CompNum ).SubComp( SubCompNum ).MeteredVar( VarNum ).ReportVarIndexType = IndexTypes( VarNum );
								ThisReportData.Branch( BranchNum ).Comp( CompNum ).SubComp( SubCompNum ).MeteredVar( VarNum ).ReportVarType = VarTypes( VarNum );
								ThisReportData.Branch( BranchNum ).Comp( CompNum ).SubComp( SubCompNum ).MeteredVar( VarNum ).ResourceType = ResourceTypes( VarNum );
								ThisReportData.Branch( BranchNum ).Comp( CompNum ).SubComp( SubCompNum ).MeteredVar( VarNum ).EndUse = EndUses( VarNum );
								if ( ThisReportData.Branch( BranchNum ).Comp( CompNum ).SubComp( SubCompNum ).MeteredVar( VarNum ).EndUse == "HEATINGCOILS" && ModeFlagOn ) {
									for ( VarNum1 = 1; VarNum1 <= NumVariables; ++VarNum1 ) {
										ThisReportData.Branch( BranchNum ).Comp( CompNum ).SubComp( SubCompNum ).MeteredVar( VarNum1 ).EndUse_CompMode = HeatingOnly;
									}
									ModeFlagOn = false;
								} else if ( ThisReportData.Branch( BranchNum ).Comp( CompNum ).SubComp( SubCompNum ).MeteredVar( VarNum ).EndUse == "COOLINGCOILS" && ModeFlagOn ) {
									for ( VarNum1 = 1; VarNum1 <= NumVariables; ++VarNum1 ) {
										ThisReportData.Branch( BranchNum ).Comp( CompNum ).SubComp( SubCompNum ).MeteredVar( VarNum1 ).EndUse_CompMode = CoolingOnly;
									}
									ModeFlagOn = false;
								} else if ( ModeFlagOn ) {
									ThisReportData.Branch( BranchNum ).Comp( CompNum ).SubComp( SubCompNum ).MeteredVar( VarNum ).EndUse_CompMode = NoHeatNoCool;
								}
								ThisReportData.Branch( BranchNum ).Comp( CompNum ).SubComp( SubCompNum ).MeteredVar( VarNum ).Group = Groups( VarNum );
							}

							VarIndexes.deallocate();
							VarTypes.deallocate();
							IndexTypes.deallocate();
							UnitsStrings.deallocate();
							ResourceTypes.deallocate();
							EndUses.deallocate();
							Groups.deallocate();
							Names.deallocate();

							ThisReportData.Branch( BranchNum ).Comp( CompNum ).SubComp( SubCompNum ).NumMeteredVars = NumVariables;
						}
					}
				}
			}
		}

	}

	// End Initialization Section of the Module
	//******************************************************************************

	// Beginning of Reporting subroutines for the SimAir Module
	// *****************************************************************************

	void
	ReportSystemEnergyUse()
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         Dan Fisher
		//       DATE WRITTEN   November 2005
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// calculate and report system loads and energy

		// METHODOLOGY EMPLOYED:
		//Accumulate meter data to appropriate report variables

		// REFERENCES:
		// na

		// Using/Aliasing
		using namespace DataZoneEnergyDemands;
		using Psychrometrics::PsyHFnTdbW;
		using namespace DataGlobalConstants;
		using DataEnvironment::OutDryBulbTemp;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		std::string CompType;
		std::string CompName;
		int Idx; // loop counter
		int nodes; // loop counter
		int CtrlZoneNum; // ZONE counter
		int ZoneInNum; // counter for zone air distribution inlets
		int AirLoopNum; // counter for zone air distribution inlets
		int BranchNum; // counter for zone air distribution inlets
		int EquipListNum; // counter for zone air distribution inlets
		int VarNum; // counter for zone air distribution inlets
		int CompNum;
		int SubCompNum;
		int SubSubCompNum;
		int CompMode;
		int InletNodeNum;
		int OutletNodeNum;
		int ADUNum;
		int ADUCoolNum;
		int ADUHeatNum;
		int AirDistCoolInletNodeNum;
		int AirDistHeatInletNodeNum;
		int EnergyType;
		int ActualZoneNum;
		Real64 CompEnergyUse;
		Real64 ZoneLoad;
		Real64 CompLoad;
		Real64 ADUCoolFlowrate;
		Real64 ADUHeatFlowrate;
		bool CompLoadFlag;

		if ( ! AirLoopLoadsReportEnabled ) return;

		//SYSTEM LOADS REPORT
		SysTotZoneLoadHTNG = 0.0;
		SysTotZoneLoadCLNG = 0.0;
		SysOALoadHTNG = 0.0;
		SysOALoadCLNG = 0.0;
		SysTotHTNG = 0.0;
		SysTotCLNG = 0.0;

		//SYSTEM ENERGY USE REPORT
		SysTotElec = 0.0;
		SysTotGas = 0.0;
		SysTotSteam = 0.0;
		SysTotH2OCOLD = 0.0;
		SysTotH2OHOT = 0.0;

		//SYSTEM COMPONENT LOADS REPORT
		SysFANCompHTNG = 0.0;
		SysCCCompCLNG = 0.0;
		SysHCCompHTNG = 0.0;
		SysHeatExHTNG = 0.0;
		SysHeatExCLNG = 0.0;
		SysSolarCollectHeating = 0.0;
		SysSolarCollectCooling = 0.0;
		SysUserDefinedTerminalHeating = 0.0;
		SysUserDefinedTerminalCooling = 0.0;
		SysHumidHTNG = 0.0;
		SysEvapCLNG = 0.0;
		DesDehumidCLNG = 0.0;
		SysDomesticH20 = 0.0;

		//SYSTEM COMPONENT ENERGY REPORT
		SysFANCompElec = 0.0;
		SysHCCompH2OHOT = 0.0;
		SysCCCompH2OCOLD = 0.0;
		SysHCCompElec = 0.0;
		SysCCCompElec = 0.0;
		SysHCCompElecRes = 0.0;
		SysHCCompGas = 0.0;
		SysHCCompSteam = 0.0;
		SysHumidElec = 0.0;
		DesDehumidElec = 0.0;
		SysEvapElec = 0.0;

		for ( AirLoopNum = 1; AirLoopNum <= NumPrimaryAirSys; ++AirLoopNum ) {
			for ( BranchNum = 1; BranchNum <= PrimaryAirSystem( AirLoopNum ).NumBranches; ++BranchNum ) {
				if ( Node( PrimaryAirSystem( AirLoopNum ).Branch( BranchNum ).NodeNumOut ).MassFlowRate <= 0.0 ) continue;
				for ( CompNum = 1; CompNum <= PrimaryAirSystem( AirLoopNum ).Branch( BranchNum ).TotalComponents; ++CompNum ) {
					CompName = PrimaryAirSystem( AirLoopNum ).Branch( BranchNum ).Comp( CompNum ).Name;
					CompType = PrimaryAirSystem( AirLoopNum ).Branch( BranchNum ).Comp( CompNum ).TypeOf;
					InletNodeNum = PrimaryAirSystem( AirLoopNum ).Branch( BranchNum ).Comp( CompNum ).NodeNumIn;
					OutletNodeNum = PrimaryAirSystem( AirLoopNum ).Branch( BranchNum ).Comp( CompNum ).NodeNumOut;
					if ( InletNodeNum <= 0 || OutletNodeNum <= 0 ) continue;
					CompLoad = Node( OutletNodeNum ).MassFlowRate * ( PsyHFnTdbW( Node( InletNodeNum ).Temp, Node( InletNodeNum ).HumRat ) - PsyHFnTdbW( Node( OutletNodeNum ).Temp, Node( OutletNodeNum ).HumRat ) );
					CompLoad *= TimeStepSys * SecInHour;
					CompEnergyUse = 0.0;
					EnergyType = iRT_None;
					CompLoadFlag = true;
					CalcSystemEnergyUse( CompLoadFlag, AirLoopNum, CompType, EnergyType, CompLoad, CompEnergyUse );
					CompLoadFlag = false;
					for ( VarNum = 1; VarNum <= PrimaryAirSystem( AirLoopNum ).Branch( BranchNum ).Comp( CompNum ).NumMeteredVars; ++VarNum ) {
						CompMode = PrimaryAirSystem( AirLoopNum ).Branch( BranchNum ).Comp( CompNum ).MeteredVar( VarNum ).EndUse_CompMode;
						CompEnergyUse = PrimaryAirSystem( AirLoopNum ).Branch( BranchNum ).Comp( CompNum ).MeteredVar( VarNum ).CurMeterReading;
						EnergyType = PrimaryAirSystem( AirLoopNum ).Branch( BranchNum ).Comp( CompNum ).MeteredVar( VarNum ).ResourceType;
						CalcSystemEnergyUse( CompLoadFlag, AirLoopNum, CompType, EnergyType, CompLoad, CompEnergyUse );
					}

					for ( SubCompNum = 1; SubCompNum <= PrimaryAirSystem( AirLoopNum ).Branch( BranchNum ).Comp( CompNum ).NumSubComps; ++SubCompNum ) {
						CompName = PrimaryAirSystem( AirLoopNum ).Branch( BranchNum ).Comp( CompNum ).SubComp( SubCompNum ).Name;
						CompType = PrimaryAirSystem( AirLoopNum ).Branch( BranchNum ).Comp( CompNum ).SubComp( SubCompNum ).TypeOf;
						InletNodeNum = PrimaryAirSystem( AirLoopNum ).Branch( BranchNum ).Comp( CompNum ).SubComp( SubCompNum ).NodeNumIn;
						if ( InletNodeNum <= 0 || OutletNodeNum <= 0 ) continue;
						OutletNodeNum = PrimaryAirSystem( AirLoopNum ).Branch( BranchNum ).Comp( CompNum ).SubComp( SubCompNum ).NodeNumOut;
						CompLoad = Node( OutletNodeNum ).MassFlowRate * ( PsyHFnTdbW( Node( InletNodeNum ).Temp, Node( InletNodeNum ).HumRat ) - PsyHFnTdbW( Node( OutletNodeNum ).Temp, Node( OutletNodeNum ).HumRat ) );
						CompLoad *= TimeStepSys * SecInHour;
						CompEnergyUse = 0.0;
						EnergyType = iRT_None;
						CompLoadFlag = true;
						CalcSystemEnergyUse( CompLoadFlag, AirLoopNum, CompType, EnergyType, CompLoad, CompEnergyUse );
						CompLoadFlag = false;
						for ( VarNum = 1; VarNum <= PrimaryAirSystem( AirLoopNum ).Branch( BranchNum ).Comp( CompNum ).SubComp( SubCompNum ).NumMeteredVars; ++VarNum ) {
							CompMode = PrimaryAirSystem( AirLoopNum ).Branch( BranchNum ).Comp( CompNum ).SubComp( SubCompNum ).MeteredVar( VarNum ).EndUse_CompMode;
							CompEnergyUse = PrimaryAirSystem( AirLoopNum ).Branch( BranchNum ).Comp( CompNum ).SubComp( SubCompNum ).MeteredVar( VarNum ).CurMeterReading;
							EnergyType = PrimaryAirSystem( AirLoopNum ).Branch( BranchNum ).Comp( CompNum ).SubComp( SubCompNum ).MeteredVar( VarNum ).ResourceType;
							CalcSystemEnergyUse( CompLoadFlag, AirLoopNum, CompType, EnergyType, CompLoad, CompEnergyUse );
						}

						for ( SubSubCompNum = 1; SubSubCompNum <= PrimaryAirSystem( AirLoopNum ).Branch( BranchNum ).Comp( CompNum ).SubComp( SubCompNum ).NumSubSubComps; ++SubSubCompNum ) {
							CompName = PrimaryAirSystem( AirLoopNum ).Branch( BranchNum ).Comp( CompNum ).SubComp( SubCompNum ).SubSubComp( SubSubCompNum ).Name;
							CompType = PrimaryAirSystem( AirLoopNum ).Branch( BranchNum ).Comp( CompNum ).SubComp( SubCompNum ).SubSubComp( SubSubCompNum ).TypeOf;
							InletNodeNum = PrimaryAirSystem( AirLoopNum ).Branch( BranchNum ).Comp( CompNum ).SubComp( SubCompNum ).SubSubComp( SubSubCompNum ).NodeNumIn;
							OutletNodeNum = PrimaryAirSystem( AirLoopNum ).Branch( BranchNum ).Comp( CompNum ).SubComp( SubCompNum ).SubSubComp( SubSubCompNum ).NodeNumOut;
							if ( InletNodeNum <= 0 || OutletNodeNum <= 0 ) continue;
							CompLoad = Node( OutletNodeNum ).MassFlowRate * ( PsyHFnTdbW( Node( InletNodeNum ).Temp, Node( InletNodeNum ).HumRat ) - PsyHFnTdbW( Node( OutletNodeNum ).Temp, Node( OutletNodeNum ).HumRat ) );
							CompLoad *= TimeStepSys * SecInHour;
							CompEnergyUse = 0.0;
							EnergyType = iRT_None;
							CompLoadFlag = true;
							CalcSystemEnergyUse( CompLoadFlag, AirLoopNum, CompType, EnergyType, CompLoad, CompEnergyUse );
							CompLoadFlag = false;
							for ( VarNum = 1; VarNum <= PrimaryAirSystem( AirLoopNum ).Branch( BranchNum ).Comp( CompNum ).SubComp( SubCompNum ).SubSubComp( SubSubCompNum ).NumMeteredVars; ++VarNum ) {
								CompMode = PrimaryAirSystem( AirLoopNum ).Branch( BranchNum ).Comp( CompNum ).SubComp( SubCompNum ).SubSubComp( SubSubCompNum ).MeteredVar( VarNum ).EndUse_CompMode;
								CompEnergyUse = PrimaryAirSystem( AirLoopNum ).Branch( BranchNum ).Comp( CompNum ).SubComp( SubCompNum ).SubSubComp( SubSubCompNum ).MeteredVar( VarNum ).CurMeterReading;
								EnergyType = PrimaryAirSystem( AirLoopNum ).Branch( BranchNum ).Comp( CompNum ).SubComp( SubCompNum ).SubSubComp( SubSubCompNum ).MeteredVar( VarNum ).ResourceType;
								CalcSystemEnergyUse( CompLoadFlag, AirLoopNum, CompType, EnergyType, CompLoad, CompEnergyUse );
							}

						}
					}
				}
			}
		}

		for ( CtrlZoneNum = 1; CtrlZoneNum <= NumOfZones; ++CtrlZoneNum ) {
			if ( ! ZoneEquipConfig( CtrlZoneNum ).IsControlled ) continue;

			//retrieve the zone load for each zone
			ActualZoneNum = ZoneEquipConfig( CtrlZoneNum ).ActualZoneNum;
			ZoneLoad = ZoneSysEnergyDemand( ActualZoneNum ).TotalOutputRequired;

			//if system operating in deadband reset zone load
			if ( DeadBandOrSetback( ActualZoneNum ) ) ZoneLoad = 0.0;

			// retrieve air loop indexes
			AirLoopNum = ZoneEquipConfig( CtrlZoneNum ).AirLoopNum;
			if ( AirLoopNum == 0 ) continue;

			//Zone cooling load
			if ( ZoneLoad < -SmallLoad ) {
				SysTotZoneLoadCLNG( AirLoopNum ) += std::abs( ZoneLoad );

				//Zone heating load
			} else if ( ZoneLoad > SmallLoad ) {
				SysTotZoneLoadHTNG( AirLoopNum ) += std::abs( ZoneLoad );
			}

			//loop over the zone supply air path inlet nodes
			for ( ZoneInNum = 1; ZoneInNum <= ZoneEquipConfig( CtrlZoneNum ).NumInletNodes; ++ZoneInNum ) {
				AirDistCoolInletNodeNum = max( ZoneEquipConfig( CtrlZoneNum ).AirDistUnitCool( ZoneInNum ).InNode, 0 );
				AirDistHeatInletNodeNum = max( ZoneEquipConfig( CtrlZoneNum ).AirDistUnitHeat( ZoneInNum ).InNode, 0 );

				// Set for cooling or heating path
				if ( AirDistCoolInletNodeNum > 0 && AirDistHeatInletNodeNum == 0 ) {
					ADUCoolFlowrate = max( Node( ZoneEquipConfig( CtrlZoneNum ).AirDistUnitCool( ZoneInNum ).InNode ).MassFlowRate, 0.0 );
				} else if ( AirDistHeatInletNodeNum > 0 && AirDistCoolInletNodeNum == 0 ) {
					ADUHeatFlowrate = max( Node( ZoneEquipConfig( CtrlZoneNum ).AirDistUnitHeat( ZoneInNum ).InNode ).MassFlowRate, 0.0 );
				} else {
					ADUCoolFlowrate = 0.0;
					ADUHeatFlowrate = 0.0;
				}

				for ( Idx = 1; Idx <= 2; ++Idx ) {
					EquipListNum = ZoneEquipConfig( CtrlZoneNum ).EquipListIndex;

					if ( Idx == 1 ) {
						ADUCoolNum = max( ZoneEquipConfig( CtrlZoneNum ).AirDistUnitCool( ZoneInNum ).AirDistUnitIndex, 0 );
						if ( ADUCoolNum == 0 ) continue;
						ADUNum = ADUCoolNum;
					} else { //(Idx =2)THEN
						ADUHeatNum = max( ZoneEquipConfig( CtrlZoneNum ).AirDistUnitHeat( ZoneInNum ).AirDistUnitIndex, 0 );
						if ( ADUHeatNum == 0 ) continue;
						ADUNum = ADUHeatNum;
					}

					CompLoad = 0.0;
					if ( ZoneEquipList( EquipListNum ).EquipData( ADUNum ).NumInlets > 0 ) {
						for ( nodes = 1; nodes <= ZoneEquipList( EquipListNum ).EquipData( ADUNum ).NumInlets; ++nodes ) {
							InletNodeNum = ZoneEquipList( EquipListNum ).EquipData( ADUNum ).InletNodeNums( Idx );
							CompLoad += ( PsyHFnTdbW( Node( InletNodeNum ).Temp, Node( InletNodeNum ).HumRat ) * Node( InletNodeNum ).MassFlowRate );
						}
						for ( nodes = 1; nodes <= ZoneEquipList( EquipListNum ).EquipData( ADUNum ).NumOutlets; ++nodes ) {
							OutletNodeNum = ZoneEquipList( EquipListNum ).EquipData( ADUNum ).OutletNodeNums( Idx );
							CompLoad -= ( PsyHFnTdbW( Node( OutletNodeNum ).Temp, Node( OutletNodeNum ).HumRat ) * Node( OutletNodeNum ).MassFlowRate );
						}
					}
					CompLoad *= TimeStepSys * SecInHour;
					CompName = ZoneEquipList( EquipListNum ).EquipData( ADUNum ).Name;
					CompType = ZoneEquipList( EquipListNum ).EquipData( ADUNum ).TypeOf;
					CompEnergyUse = 0.0;
					EnergyType = iRT_None;
					CompLoadFlag = true;
					CalcSystemEnergyUse( CompLoadFlag, AirLoopNum, CompType, EnergyType, CompLoad, CompEnergyUse );
					CompLoadFlag = false;
					for ( VarNum = 1; VarNum <= ZoneEquipList( EquipListNum ).EquipData( ADUNum ).NumMeteredVars; ++VarNum ) {
						CompEnergyUse = ZoneEquipList( EquipListNum ).EquipData( ADUNum ).MeteredVar( VarNum ).CurMeterReading;
						EnergyType = ZoneEquipList( EquipListNum ).EquipData( ADUNum ).MeteredVar( VarNum ).ResourceType;
						CalcSystemEnergyUse( CompLoadFlag, AirLoopNum, CompType, EnergyType, CompLoad, CompEnergyUse );
					}

					for ( SubCompNum = 1; SubCompNum <= ZoneEquipList( EquipListNum ).EquipData( ADUNum ).NumSubEquip; ++SubCompNum ) {
						CompName = ZoneEquipList( EquipListNum ).EquipData( ADUNum ).SubEquipData( SubCompNum ).Name;
						CompType = ZoneEquipList( EquipListNum ).EquipData( ADUNum ).SubEquipData( SubCompNum ).TypeOf;
						InletNodeNum = ZoneEquipList( EquipListNum ).EquipData( ADUNum ).SubEquipData( SubCompNum ).InletNodeNum;
						OutletNodeNum = ZoneEquipList( EquipListNum ).EquipData( ADUNum ).SubEquipData( SubCompNum ).OutletNodeNum;
						if ( InletNodeNum <= 0 || OutletNodeNum <= 0 ) continue;
						CompLoad = Node( InletNodeNum ).MassFlowRate * ( PsyHFnTdbW( Node( InletNodeNum ).Temp, Node( InletNodeNum ).HumRat ) - PsyHFnTdbW( Node( OutletNodeNum ).Temp, Node( OutletNodeNum ).HumRat ) );
						CompLoad *= TimeStepSys * SecInHour;
						CompEnergyUse = 0.0;
						EnergyType = iRT_None;
						CompLoadFlag = true;
						CalcSystemEnergyUse( CompLoadFlag, AirLoopNum, CompType, EnergyType, CompLoad, CompEnergyUse );
						CompLoadFlag = false;
						for ( VarNum = 1; VarNum <= ZoneEquipList( EquipListNum ).EquipData( ADUNum ).SubEquipData( SubCompNum ).NumMeteredVars; ++VarNum ) {
							CompEnergyUse = ZoneEquipList( EquipListNum ).EquipData( ADUNum ).SubEquipData( SubCompNum ).MeteredVar( VarNum ).CurMeterReading;
							CompMode = ZoneEquipList( EquipListNum ).EquipData( ADUNum ).SubEquipData( SubCompNum ).MeteredVar( VarNum ).EndUse_CompMode;
							EnergyType = ZoneEquipList( EquipListNum ).EquipData( ADUNum ).SubEquipData( SubCompNum ).MeteredVar( VarNum ).ResourceType;
							CalcSystemEnergyUse( CompLoadFlag, AirLoopNum, CompType, EnergyType, CompLoad, CompEnergyUse );
						}

						for ( SubSubCompNum = 1; SubSubCompNum <= ZoneEquipList( EquipListNum ).EquipData( ADUNum ).SubEquipData( SubCompNum ).NumSubSubEquip; ++SubSubCompNum ) {
							CompName = ZoneEquipList( EquipListNum ).EquipData( ADUNum ).SubEquipData( SubCompNum ).SubSubEquipData( SubSubCompNum ).Name;
							CompType = ZoneEquipList( EquipListNum ).EquipData( ADUNum ).SubEquipData( SubCompNum ).SubSubEquipData( SubSubCompNum ).TypeOf;
							InletNodeNum = ZoneEquipList( EquipListNum ).EquipData( ADUNum ).SubEquipData( SubCompNum ).SubSubEquipData( SubSubCompNum ).InletNodeNum;
							OutletNodeNum = ZoneEquipList( EquipListNum ).EquipData( ADUNum ).SubEquipData( SubCompNum ).SubSubEquipData( SubSubCompNum ).OutletNodeNum;
							if ( InletNodeNum <= 0 || OutletNodeNum <= 0 ) continue;
							CompLoad = Node( InletNodeNum ).MassFlowRate * ( PsyHFnTdbW( Node( InletNodeNum ).Temp, Node( InletNodeNum ).HumRat ) - PsyHFnTdbW( Node( OutletNodeNum ).Temp, Node( OutletNodeNum ).HumRat ) );
							CompLoad *= TimeStepSys * SecInHour;
							CompEnergyUse = 0.0;
							EnergyType = iRT_None;
							CompLoadFlag = true;
							CalcSystemEnergyUse( CompLoadFlag, AirLoopNum, CompType, EnergyType, CompLoad, CompEnergyUse );
							CompLoadFlag = false;
							for ( VarNum = 1; VarNum <= ZoneEquipList( EquipListNum ).EquipData( ADUNum ).SubEquipData( SubCompNum ).SubSubEquipData( SubSubCompNum ).NumMeteredVars; ++VarNum ) {
								CompEnergyUse = ZoneEquipList( EquipListNum ).EquipData( ADUNum ).SubEquipData( SubCompNum ).SubSubEquipData( SubSubCompNum ).MeteredVar( VarNum ).CurMeterReading;
								CompMode = ZoneEquipList( EquipListNum ).EquipData( ADUNum ).SubEquipData( SubCompNum ).SubSubEquipData( SubSubCompNum ).MeteredVar( VarNum ).EndUse_CompMode;
								EnergyType = ZoneEquipList( EquipListNum ).EquipData( ADUNum ).SubEquipData( SubCompNum ).SubSubEquipData( SubSubCompNum ).MeteredVar( VarNum ).ResourceType;
								CalcSystemEnergyUse( CompLoadFlag, AirLoopNum, CompType, EnergyType, CompLoad, CompEnergyUse );
							}
						} //SubSubCompNum
					} //SubCompNum
				} //Idx
			} // ZoneInNum
		} // Controlled Zone Loop

		for ( AirLoopNum = 1; AirLoopNum <= NumPrimaryAirSys; ++AirLoopNum ) {
			SysTotHTNG( AirLoopNum ) = SysFANCompHTNG( AirLoopNum ) + SysHCCompHTNG( AirLoopNum ) + SysHeatExHTNG( AirLoopNum ) + SysHumidHTNG( AirLoopNum ) + SysSolarCollectHeating( AirLoopNum ) + SysUserDefinedTerminalHeating( AirLoopNum );
			SysTotCLNG( AirLoopNum ) = SysCCCompCLNG( AirLoopNum ) + SysHeatExCLNG( AirLoopNum ) + SysEvapCLNG( AirLoopNum ) + DesDehumidCLNG( AirLoopNum ) + SysSolarCollectCooling( AirLoopNum ) + SysUserDefinedTerminalCooling( AirLoopNum );
			SysTotElec( AirLoopNum ) = SysFANCompElec( AirLoopNum ) + SysHCCompElec( AirLoopNum ) + SysCCCompElec( AirLoopNum ) + SysHCCompElecRes( AirLoopNum ) + SysHumidElec( AirLoopNum ) + DesDehumidElec( AirLoopNum ) + SysEvapElec( AirLoopNum );
			SysTotGas( AirLoopNum ) = SysHCCompGas( AirLoopNum );
			SysTotSteam( AirLoopNum ) = SysHCCompSteam( AirLoopNum );
			SysTotH2OCOLD( AirLoopNum ) = SysCCCompH2OCOLD( AirLoopNum );
			SysTotH2OHOT( AirLoopNum ) = SysHCCompH2OHOT( AirLoopNum );
		}

	}

	bool
	index_in_sorted_string_vector(
		std::vector< std::string > const & v,
		std::string const & s,
		std::vector< std::string >::size_type & i
	)
	{
		// FUNCTION INFORMATION:
		//       AUTHOR         Stuart Mentzer
		//       DATE WRITTEN   June 2014
		//       MODIFIED
		//       RE-ENGINEERED

		// PURPOSE OF THIS FUNCTION:
		// Find a string in a sorted vector of strings
		// std::lower_bound can do this but is slower

		// METHODOLOGY EMPLOYED:
		// Binary search

		assert( std::is_sorted( v.begin(), v.end() ) );
		typedef  std::vector< std::string >::size_type  size_type;
		size_type const v_size( v.size() );
		if ( v_size == 0 ) { // Empty
			i = 1u;
			return false;
		} else if ( s < v[ 0 ] ) { // Less than all
			i = v_size;
			return false;
		} else if ( s > v[ v_size - 1 ] ) { // Greater than all
			i = v_size;
			return false;
		} else {
			size_type beg( 0 ), mid, end( v_size );
			while ( beg + 1 < end ) {
				mid = ( ( beg + end ) >> 1 ); // bit shifting is faster than /2
				( s >= v[ mid ] ? beg : end ) = mid;
			} // Invariant: v[beg] <= s < v[end] (if end < v.size())
			if ( s == v[ beg ] ) {
				i = beg;
				return true;
			} else {
				i = v_size;
				return false;
			}
		}
	}

	void
	CalcSystemEnergyUse(
		bool const CompLoadFlag,
		int const AirLoopNum,
		std::string const & CompType,
		int const EnergyType,
		Real64 const CompLoad,
		Real64 const CompEnergy
	)
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         Dan Fisher
		//       DATE WRITTEN   Nov. 2005
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// accumulate system loads and energy to report variables

		// METHODOLOGY EMPLOYED:

		// REFERENCES:
		// na

		// Using/Aliasing
		using Psychrometrics::PsyHFnTdbW;
		using namespace DataZoneEnergyDemands;
		using namespace DataGlobalConstants;
		using InputProcessor::FindItemInList;
		using InputProcessor::SameString;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:

		//Tuned String comparisons were a big performance hit
		// ComponentTypes and component_strings must remain in sync and sorted
		enum ComponentTypes : std::vector< std::string >::size_type { // Using older enum style to avoid the name scoping cruft
			AIRLOOPHVAC_OUTDOORAIRSYSTEM,
			AIRLOOPHVAC_UNITARY_FURNACE_HEATCOOL,
			AIRLOOPHVAC_UNITARY_FURNACE_HEATONLY,
			AIRLOOPHVAC_UNITARYHEATCOOL,
			AIRLOOPHVAC_UNITARYHEATCOOL_VAVCHANGEOVERBYPASS,
			AIRLOOPHVAC_UNITARYHEATONLY,
			AIRLOOPHVAC_UNITARYHEATPUMP_AIRTOAIR,
			AIRLOOPHVAC_UNITARYHEATPUMP_AIRTOAIR_MULTISPEED,
			AIRLOOPHVAC_UNITARYHEATPUMP_WATERTOAIR,
			AIRLOOPHVAC_UNITARYSYSTEM,
			AIRTERMINAL_DUALDUCT_CONSTANTVOLUME_COOL,
			AIRTERMINAL_DUALDUCT_CONSTANTVOLUME_HEAT,
			AIRTERMINAL_DUALDUCT_VAV_COOL,
			AIRTERMINAL_DUALDUCT_VAV_HEAT,
			AIRTERMINAL_DUALDUCT_VAV_OUTDOORAIR_OUTDOORAIR,
			AIRTERMINAL_DUALDUCT_VAV_OUTDOORAIR_RECIRCULATEDAIR,
			AIRTERMINAL_SINGLEDUCT_CONSTANTVOLUME_COOLEDBEAM,
			AIRTERMINAL_SINGLEDUCT_CONSTANTVOLUME_FOURPIPEINDUCTION,
			AIRTERMINAL_SINGLEDUCT_CONSTANTVOLUME_REHEAT,
			AIRTERMINAL_SINGLEDUCT_INLETSIDEMIXER,
			AIRTERMINAL_SINGLEDUCT_PARALLELPIU_REHEAT,
			AIRTERMINAL_SINGLEDUCT_SERIESPIU_REHEAT,
			AIRTERMINAL_SINGLEDUCT_SUPPLYSIDEMIXER,
			AIRTERMINAL_SINGLEDUCT_UNCONTROLLED,
			AIRTERMINAL_SINGLEDUCT_USERDEFINED,
			AIRTERMINAL_SINGLEDUCT_VAV_HEATANDCOOL_NOREHEAT,
			AIRTERMINAL_SINGLEDUCT_VAV_HEATANDCOOL_REHEAT,
			AIRTERMINAL_SINGLEDUCT_VAV_NOREHEAT,
			AIRTERMINAL_SINGLEDUCT_VAV_REHEAT,
			AIRTERMINAL_SINGLEDUCT_VAV_REHEAT_VARIABLESPEEDFAN,
			COIL_COOLING_DX_MULTISPEED,
			COIL_COOLING_DX_SINGLESPEED,
			COIL_COOLING_DX_SINGLESPEED_THERMALSTORAGE,
			COIL_COOLING_DX_TWOSPEED,
			COIL_COOLING_DX_TWOSTAGEWITHHUMIDITYCONTROLMODE,
			COIL_COOLING_DX_VARIABLESPEED,
			COIL_COOLING_WATER,
			COIL_COOLING_WATER_DETAILEDGEOMETRY,
			COIL_COOLING_WATERTOAIRHEATPUMP_EQUATIONFIT,
			COIL_COOLING_WATERTOAIRHEATPUMP_PARAMETERESTIMATION,
			COIL_COOLING_WATERTOAIRHEATPUMP_VARIABLESPEEDEQUATIONFIT,
			COIL_HEATING_DESUPERHEATER,
			COIL_HEATING_DX_MULTISPEED,
			COIL_HEATING_DX_SINGLESPEED,
			COIL_HEATING_DX_VARIABLESPEED,
			COIL_HEATING_ELECTRIC,
			COIL_HEATING_ELECTRIC_MULTISTAGE,
			COIL_HEATING_GAS,
			COIL_HEATING_GAS_MULTISTAGE,
			COIL_HEATING_STEAM,
			COIL_HEATING_WATER,
			COIL_HEATING_WATERTOAIRHEATPUMP_EQUATIONFIT,
			COIL_HEATING_WATERTOAIRHEATPUMP_PARAMETERESTIMATION,
			COIL_HEATING_WATERTOAIRHEATPUMP_VARIABLESPEEDEQUATIONFIT,
			COIL_USERDEFINED,
			COILSYSTEM_COOLING_DX,
			COILSYSTEM_COOLING_DX_HEATEXCHANGERASSISTED,
			COILSYSTEM_COOLING_WATER_HEATEXCHANGERASSISTED,
			COILSYSTEM_HEATING_DX,
			DEHUMIDIFIER_DESICCANT_NOFANS,
			DEHUMIDIFIER_DESICCANT_SYSTEM,
			DUCT,
			EVAPORATIVECOOLER_DIRECT_CELDEKPAD,
			EVAPORATIVECOOLER_DIRECT_RESEARCHSPECIAL,
			EVAPORATIVECOOLER_INDIRECT_CELDEKPAD,
			EVAPORATIVECOOLER_INDIRECT_RESEARCHSPECIAL,
			EVAPORATIVECOOLER_INDIRECT_WETCOIL,
			FAN_COMPONENTMODEL,
			FAN_CONSTANTVOLUME,
			FAN_ONOFF,
			FAN_VARIABLEVOLUME,
			HEATEXCHANGER_AIRTOAIR_FLATPLATE,
			HEATEXCHANGER_AIRTOAIR_SENSIBLEANDLATENT,
			HEATEXCHANGER_DESICCANT_BALANCEDFLOW,
			HUMIDIFIER_STEAM_ELECTRIC,
			OUTDOORAIR_MIXER,
			SOLARCOLLECTOR_FLATPLATE_PHOTOVOLTAICTHERMAL,
			SOLARCOLLECTOR_UNGLAZEDTRANSPIRED,
			ZONEHVAC_AIRDISTRIBUTIONUNIT,
			n_ComponentTypes,
			Unknown_ComponentType
		};

		static std::vector< std::string > const component_strings = { // Must be sorted!
			"AIRLOOPHVAC:OUTDOORAIRSYSTEM",
			"AIRLOOPHVAC:UNITARY:FURNACE:HEATCOOL",
			"AIRLOOPHVAC:UNITARY:FURNACE:HEATONLY",
			"AIRLOOPHVAC:UNITARYHEATCOOL",
			"AIRLOOPHVAC:UNITARYHEATCOOL:VAVCHANGEOVERBYPASS",
			"AIRLOOPHVAC:UNITARYHEATONLY",
			"AIRLOOPHVAC:UNITARYHEATPUMP:AIRTOAIR",
			"AIRLOOPHVAC:UNITARYHEATPUMP:AIRTOAIR:MULTISPEED",
			"AIRLOOPHVAC:UNITARYHEATPUMP:WATERTOAIR",
			"AIRLOOPHVAC:UNITARYSYSTEM",
			"AIRTERMINAL:DUALDUCT:CONSTANTVOLUME:COOL",
			"AIRTERMINAL:DUALDUCT:CONSTANTVOLUME:HEAT",
			"AIRTERMINAL:DUALDUCT:VAV:COOL",
			"AIRTERMINAL:DUALDUCT:VAV:HEAT",
			"AIRTERMINAL:DUALDUCT:VAV:OUTDOORAIR:OUTDOORAIR",
			"AIRTERMINAL:DUALDUCT:VAV:OUTDOORAIR:RECIRCULATEDAIR",
			"AIRTERMINAL:SINGLEDUCT:CONSTANTVOLUME:COOLEDBEAM",
			"AIRTERMINAL:SINGLEDUCT:CONSTANTVOLUME:FOURPIPEINDUCTION",
			"AIRTERMINAL:SINGLEDUCT:CONSTANTVOLUME:REHEAT",
			"AIRTERMINAL:SINGLEDUCT:INLETSIDEMIXER",
			"AIRTERMINAL:SINGLEDUCT:PARALLELPIU:REHEAT",
			"AIRTERMINAL:SINGLEDUCT:SERIESPIU:REHEAT",
			"AIRTERMINAL:SINGLEDUCT:SUPPLYSIDEMIXER",
			"AIRTERMINAL:SINGLEDUCT:UNCONTROLLED",
			"AIRTERMINAL:SINGLEDUCT:USERDEFINED",
			"AIRTERMINAL:SINGLEDUCT:VAV:HEATANDCOOL:NOREHEAT",
			"AIRTERMINAL:SINGLEDUCT:VAV:HEATANDCOOL:REHEAT",
			"AIRTERMINAL:SINGLEDUCT:VAV:NOREHEAT",
			"AIRTERMINAL:SINGLEDUCT:VAV:REHEAT",
			"AIRTERMINAL:SINGLEDUCT:VAV:REHEAT:VARIABLESPEEDFAN",
			"COIL:COOLING:DX:MULTISPEED",
			"COIL:COOLING:DX:SINGLESPEED",
			"COIL:COOLING:DX:SINGLESPEED:THERMALSTORAGE",
			"COIL:COOLING:DX:TWOSPEED",
			"COIL:COOLING:DX:TWOSTAGEWITHHUMIDITYCONTROLMODE",
			"COIL:COOLING:DX:VARIABLESPEED",
			"COIL:COOLING:WATER",
			"COIL:COOLING:WATER:DETAILEDGEOMETRY",
			"COIL:COOLING:WATERTOAIRHEATPUMP:EQUATIONFIT",
			"COIL:COOLING:WATERTOAIRHEATPUMP:PARAMETERESTIMATION",
			"COIL:COOLING:WATERTOAIRHEATPUMP:VARIABLESPEEDEQUATIONFIT",
			"COIL:HEATING:DESUPERHEATER",
			"COIL:HEATING:DX:MULTISPEED",
			"COIL:HEATING:DX:SINGLESPEED",
			"COIL:HEATING:DX:VARIABLESPEED",
			"COIL:HEATING:ELECTRIC",
			"COIL:HEATING:ELECTRIC:MULTISTAGE",
			"COIL:HEATING:GAS",
			"COIL:HEATING:GAS:MULTISTAGE",
			"COIL:HEATING:STEAM",
			"COIL:HEATING:WATER",
			"COIL:HEATING:WATERTOAIRHEATPUMP:EQUATIONFIT",
			"COIL:HEATING:WATERTOAIRHEATPUMP:PARAMETERESTIMATION",
			"COIL:HEATING:WATERTOAIRHEATPUMP:VARIABLESPEEDEQUATIONFIT",
			"COIL:USERDEFINED",
			"COILSYSTEM:COOLING:DX",
			"COILSYSTEM:COOLING:DX:HEATEXCHANGERASSISTED",
			"COILSYSTEM:COOLING:WATER:HEATEXCHANGERASSISTED",
			"COILSYSTEM:HEATING:DX",
			"DEHUMIDIFIER:DESICCANT:NOFANS",
			"DEHUMIDIFIER:DESICCANT:SYSTEM",
			"DUCT",
			"EVAPORATIVECOOLER:DIRECT:CELDEKPAD",
			"EVAPORATIVECOOLER:DIRECT:RESEARCHSPECIAL",
			"EVAPORATIVECOOLER:INDIRECT:CELDEKPAD",
			"EVAPORATIVECOOLER:INDIRECT:RESEARCHSPECIAL",
			"EVAPORATIVECOOLER:INDIRECT:WETCOIL",
			"FAN:COMPONENTMODEL",
			"FAN:CONSTANTVOLUME",
			"FAN:ONOFF",
			"FAN:VARIABLEVOLUME",
			"HEATEXCHANGER:AIRTOAIR:FLATPLATE",
			"HEATEXCHANGER:AIRTOAIR:SENSIBLEANDLATENT",
			"HEATEXCHANGER:DESICCANT:BALANCEDFLOW",
			"HUMIDIFIER:STEAM:ELECTRIC",
			"OUTDOORAIR:MIXER",
			"SOLARCOLLECTOR:FLATPLATE:PHOTOVOLTAICTHERMAL",
			"SOLARCOLLECTOR:UNGLAZEDTRANSPIRED",
			"ZONEHVAC:AIRDISTRIBUTIONUNIT"
		};
		assert( std::is_sorted( component_strings.begin(), component_strings.end() ) );
		assert( component_strings.size() == n_ComponentTypes );

		Real64 const SmallLoad( 0.1 ); // (W)
		Real64 const KJperJ( 0.001 ); // kilojoules per joules

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		static int NumCompTypes( 0 );
		int found;

		struct CompTypeError
		{
			// Members
			std::string CompType;
			int CompErrIndex;

			// Default Constructor
			CompTypeError() :
				CompErrIndex( 0 )
			{}

			// Member Constructor
			CompTypeError(
				std::string const & CompType,
				int const CompErrIndex
			) :
				CompType( CompType ),
				CompErrIndex( CompErrIndex )
			{}

		};

		// Object Data
		static FArray1D< CompTypeError > CompTypeErrors( 100 );

		if ( ! AirLoopLoadsReportEnabled ) return;

		// following for debug
		//    CHARACTER(len=60) :: cEnergyType

		//    cEnergyType=cRT_ValidTypes(EnergyType-ResourceTypeInitialOffset)

		// Find enum for the component type string
		std::vector< std::string >::size_type iCompType;
		ComponentTypes comp_type( Unknown_ComponentType );
		if ( index_in_sorted_string_vector( component_strings, CompType, iCompType ) ) {
			comp_type = static_cast< ComponentTypes >( iCompType );
		}

		switch( comp_type ) {
		case AIRLOOPHVAC_OUTDOORAIRSYSTEM: // Outside Air System
			if ( CompLoadFlag ) {
				if ( CompLoad > 0.0 ) {
					SysOALoadCLNG( AirLoopNum ) += std::abs( CompLoad );
				} else {
					SysOALoadHTNG( AirLoopNum ) += std::abs( CompLoad );
				}
			}
			break;
		case OUTDOORAIR_MIXER: // Outdoor Air Mixer
			//No energy transfers to account for
			break;
		case AIRTERMINAL_SINGLEDUCT_INLETSIDEMIXER:
			//No energy transfers to account for

			break;
		case AIRTERMINAL_SINGLEDUCT_SUPPLYSIDEMIXER:
			//No energy transfers to account for

			// Fan Types for the air sys simulation
			break;
		case FAN_CONSTANTVOLUME:
		case FAN_VARIABLEVOLUME:
		case FAN_ONOFF:
		case FAN_COMPONENTMODEL : //cpw22Aug2010 Add 'FAN_COMPONENTMODEL')

			if ( CompLoadFlag ) SysFANCompHTNG( AirLoopNum ) += std::abs( CompLoad );
			SysFANCompElec( AirLoopNum ) += CompEnergy;

			// Cooling Coil Types for the air sys simulation
			break;
		case COILSYSTEM_COOLING_DX_HEATEXCHANGERASSISTED:
		case COIL_COOLING_DX_SINGLESPEED:
		case COIL_COOLING_DX_TWOSPEED:
		case COIL_COOLING_DX_TWOSTAGEWITHHUMIDITYCONTROLMODE:
		case COIL_COOLING_DX_MULTISPEED:
		case COIL_COOLING_WATERTOAIRHEATPUMP_EQUATIONFIT:
		case COIL_COOLING_WATERTOAIRHEATPUMP_PARAMETERESTIMATION:
		case COIL_COOLING_WATERTOAIRHEATPUMP_VARIABLESPEEDEQUATIONFIT:
		case COIL_COOLING_DX_VARIABLESPEED:
		case COILSYSTEM_COOLING_WATER_HEATEXCHANGERASSISTED:
		case COIL_COOLING_WATER_DETAILEDGEOMETRY:
		case COIL_COOLING_WATER:
		case COIL_COOLING_DX_SINGLESPEED_THERMALSTORAGE:

			if ( CompLoadFlag ) SysCCCompCLNG( AirLoopNum ) += std::abs( CompLoad );
			if ( ( EnergyType == iRT_PlantLoopCoolingDemand ) || ( EnergyType == iRT_DistrictCooling ) ) {
				SysCCCompH2OCOLD( AirLoopNum ) += CompEnergy;
			} else if ( EnergyType == iRT_Electricity ) {
				SysCCCompElec( AirLoopNum ) += CompEnergy;
			}

			// Heating Coil Types for the air sys simulation
			break;
		case COIL_HEATING_WATER:
		case COIL_HEATING_DX_SINGLESPEED:
		case COIL_HEATING_DX_MULTISPEED:
		case COIL_HEATING_WATERTOAIRHEATPUMP_EQUATIONFIT:
		case COIL_HEATING_WATERTOAIRHEATPUMP_PARAMETERESTIMATION:
		case COIL_HEATING_WATERTOAIRHEATPUMP_VARIABLESPEEDEQUATIONFIT:
		case COIL_HEATING_DX_VARIABLESPEED:
		case COIL_HEATING_STEAM:
		case COIL_HEATING_GAS:
		case COIL_HEATING_GAS_MULTISTAGE:
		case COIL_HEATING_DESUPERHEATER:

			if ( CompLoadFlag ) SysHCCompHTNG( AirLoopNum ) += std::abs( CompLoad );
			if ( ( EnergyType == iRT_PlantLoopHeatingDemand ) || ( EnergyType == iRT_DistrictHeating ) ) {
				SysHCCompH2OHOT( AirLoopNum ) += CompEnergy;
			} else if ( EnergyType == iRT_Steam ) {
				SysHCCompSteam( AirLoopNum ) += CompEnergy;
			} else if ( EnergyType == iRT_Electricity ) {
				SysHCCompElec( AirLoopNum ) += CompEnergy;
			} else if ( ( EnergyType == iRT_Natural_Gas ) || ( EnergyType == iRT_Propane ) ) {
				SysHCCompGas( AirLoopNum ) += CompEnergy;
			}

			break;
		case COIL_HEATING_ELECTRIC:
		case COIL_HEATING_ELECTRIC_MULTISTAGE:

			if ( CompLoadFlag ) SysHCCompHTNG( AirLoopNum ) += std::abs( CompLoad );
			if ( EnergyType == iRT_Electricity ) {
				SysHCCompElecRes( AirLoopNum ) += CompEnergy;
			} else {
			}

			break;
		case COIL_USERDEFINED:

			if ( CompLoadFlag ) {
				if ( CompLoad > 0.0 ) {
					SysCCCompCLNG( AirLoopNum ) += std::abs( CompLoad );
				} else {
					SysHCCompHTNG( AirLoopNum ) += std::abs( CompLoad );
				}
			}
			if ( ( EnergyType == iRT_PlantLoopHeatingDemand ) || ( EnergyType == iRT_DistrictHeating ) ) {
				SysHCCompH2OHOT( AirLoopNum ) += CompEnergy;
			} else if ( ( EnergyType == iRT_PlantLoopCoolingDemand ) || ( EnergyType == iRT_DistrictCooling ) ) {
				SysCCCompH2OCOLD( AirLoopNum ) += CompEnergy;
			} else if ( EnergyType == iRT_Steam ) {
				SysHCCompSteam( AirLoopNum ) += CompEnergy;
			} else if ( EnergyType == iRT_Electricity ) {
				if ( CompLoad > 0.0 ) {
					SysCCCompElec( AirLoopNum ) += CompEnergy;
				} else {
					SysHCCompElec( AirLoopNum ) += CompEnergy;
				}
			} else if ( ( EnergyType == iRT_Natural_Gas ) || ( EnergyType == iRT_Propane ) ) {
				SysHCCompGas( AirLoopNum ) += CompEnergy;
			}

			//DX Systems
			break;
		case AIRLOOPHVAC_UNITARYSYSTEM:
			//All energy transfers accounted for in subcomponent models
			break;
		case AIRLOOPHVAC_UNITARYHEATPUMP_AIRTOAIR:
			//All energy transfers accounted for in subcomponent models
			break;
		case AIRLOOPHVAC_UNITARYHEATPUMP_WATERTOAIR:
			//All energy transfers accounted for in subcomponent models
			break;
		case COILSYSTEM_COOLING_DX:
			//All energy transfers accounted for in subcomponent models
			break;
		case COILSYSTEM_HEATING_DX:
			//All energy transfers accounted for in subcomponent models
			break;
		case AIRLOOPHVAC_UNITARY_FURNACE_HEATONLY:
			//All energy transfers accounted for in subcomponent models
			break;
		case AIRLOOPHVAC_UNITARY_FURNACE_HEATCOOL:
			//All energy transfers accounted for in subcomponent models
			break;
		case AIRLOOPHVAC_UNITARYHEATONLY:
			//All energy transfers accounted for in subcomponent models
			break;
		case AIRLOOPHVAC_UNITARYHEATCOOL:
			//All energy transfers accounted for in subcomponent models
			break;
		case AIRLOOPHVAC_UNITARYHEATCOOL_VAVCHANGEOVERBYPASS:
			//All energy transfers accounted for in subcomponent models
			break;
		case AIRLOOPHVAC_UNITARYHEATPUMP_AIRTOAIR_MULTISPEED:
			//All energy transfers accounted for in subcomponent models

			// Humidifier Types for the air system simulation
			break;
		case HUMIDIFIER_STEAM_ELECTRIC:
			if ( CompLoadFlag ) SysHumidHTNG( AirLoopNum ) += std::abs( CompLoad );
			if ( EnergyType == iRT_Water ) {
				SysDomesticH20( AirLoopNum ) += std::abs( CompEnergy );
			} else if ( EnergyType == iRT_Electricity ) {
				SysHumidElec( AirLoopNum ) += CompEnergy;
			}

			// Evap Cooler Types for the air system simulation
			break;
		case EVAPORATIVECOOLER_DIRECT_CELDEKPAD:
		case EVAPORATIVECOOLER_INDIRECT_CELDEKPAD:
		case EVAPORATIVECOOLER_INDIRECT_WETCOIL:
		case EVAPORATIVECOOLER_DIRECT_RESEARCHSPECIAL:
		case EVAPORATIVECOOLER_INDIRECT_RESEARCHSPECIAL:
			if ( CompLoadFlag ) SysEvapCLNG( AirLoopNum ) += std::abs( CompLoad );
			if ( EnergyType == iRT_Water ) {
				SysDomesticH20( AirLoopNum ) += std::abs( CompEnergy );
			} else if ( EnergyType == iRT_Electricity ) {
				SysEvapElec( AirLoopNum ) += CompEnergy;
			}

			// Desiccant Dehumidifier Types for the air system simulation
			break;
		case DEHUMIDIFIER_DESICCANT_NOFANS:
		case DEHUMIDIFIER_DESICCANT_SYSTEM:
			if ( CompLoadFlag ) DesDehumidCLNG( AirLoopNum ) += std::abs( CompLoad );
			if ( EnergyType == iRT_Electricity ) {
				DesDehumidElec( AirLoopNum ) += CompEnergy;
			}

			// Heat Exchanger Types
			break;
		case HEATEXCHANGER_AIRTOAIR_FLATPLATE:
		case HEATEXCHANGER_AIRTOAIR_SENSIBLEANDLATENT:
		case HEATEXCHANGER_DESICCANT_BALANCEDFLOW:
			if ( CompLoadFlag ) {
				if ( CompLoad > 0.0 ) {
					SysHeatExCLNG( AirLoopNum ) += std::abs( CompLoad );
				} else {
					SysHeatExHTNG( AirLoopNum ) += std::abs( CompLoad );
				}
			}

			// Air Terminal Types
			break;
		case AIRTERMINAL_DUALDUCT_CONSTANTVOLUME_COOL:
		case AIRTERMINAL_DUALDUCT_CONSTANTVOLUME_HEAT:
		case AIRTERMINAL_DUALDUCT_VAV_COOL:
		case AIRTERMINAL_DUALDUCT_VAV_HEAT:
		case AIRTERMINAL_DUALDUCT_VAV_OUTDOORAIR_OUTDOORAIR:
		case AIRTERMINAL_DUALDUCT_VAV_OUTDOORAIR_RECIRCULATEDAIR:
		case AIRTERMINAL_SINGLEDUCT_CONSTANTVOLUME_FOURPIPEINDUCTION:
		case AIRTERMINAL_SINGLEDUCT_CONSTANTVOLUME_REHEAT:
		case AIRTERMINAL_SINGLEDUCT_PARALLELPIU_REHEAT:
		case AIRTERMINAL_SINGLEDUCT_SERIESPIU_REHEAT:
		case AIRTERMINAL_SINGLEDUCT_UNCONTROLLED:
		case AIRTERMINAL_SINGLEDUCT_VAV_HEATANDCOOL_NOREHEAT:
		case AIRTERMINAL_SINGLEDUCT_VAV_HEATANDCOOL_REHEAT:
		case AIRTERMINAL_SINGLEDUCT_VAV_NOREHEAT:
		case AIRTERMINAL_SINGLEDUCT_VAV_REHEAT:
		case AIRTERMINAL_SINGLEDUCT_VAV_REHEAT_VARIABLESPEEDFAN:
		case AIRTERMINAL_SINGLEDUCT_CONSTANTVOLUME_COOLEDBEAM:
		case ZONEHVAC_AIRDISTRIBUTIONUNIT:
			//All energy transfers accounted for in component models

			// Duct Types
			break;
		case DUCT:
			// duct losses should be accounted for here ???
			// requires addition of a new variable to sum duct losses
			// Example:
			//      IF(CompLoad > 0.0d0)THEN
			//        SysDuctHTNG(AirLoopNum) =  SysDuctHTNG(AirLoopNum) + ABS(CompLoad)
			//      ELSE
			//        SysDuctCLNG(AirLoopNum) =  SysDuctCLNG(AirLoopNum) + ABS(CompLoad)
			//      ENDIF

			// Solar Collector Types
			break;
		case SOLARCOLLECTOR_FLATPLATE_PHOTOVOLTAICTHERMAL:
		case SOLARCOLLECTOR_UNGLAZEDTRANSPIRED:
			if ( CompLoadFlag ) {
				if ( CompLoad > 0.0 ) {
					SysSolarCollectCooling( AirLoopNum ) += std::abs( CompLoad );
				} else {
					SysSolarCollectHeating( AirLoopNum ) += std::abs( CompLoad );
				}
			}

			break;
		case AIRTERMINAL_SINGLEDUCT_USERDEFINED:
			// User component model energy use should be accounted for here
			if ( CompLoadFlag ) {
				if ( CompLoad > 0.0 ) {
					SysUserDefinedTerminalCooling( AirLoopNum ) += std::abs( CompLoad );
				} else {
					SysUserDefinedTerminalHeating( AirLoopNum ) += std::abs( CompLoad );
				}
			}
			if ( ( EnergyType == iRT_PlantLoopHeatingDemand ) || ( EnergyType == iRT_DistrictHeating ) ) {
				SysHCCompH2OHOT( AirLoopNum ) += CompEnergy;
			} else if ( ( EnergyType == iRT_PlantLoopCoolingDemand ) || ( EnergyType == iRT_DistrictCooling ) ) {
				SysCCCompH2OCOLD( AirLoopNum ) += CompEnergy;
			} else if ( EnergyType == iRT_Steam ) {
				SysHCCompSteam( AirLoopNum ) += CompEnergy;
			} else if ( EnergyType == iRT_Electricity ) {
				if ( CompLoad > 0.0 ) {
					SysCCCompElec( AirLoopNum ) += CompEnergy;
				} else {
					SysHCCompElec( AirLoopNum ) += CompEnergy;
				}
			} else if ( ( EnergyType == iRT_Natural_Gas ) || ( EnergyType == iRT_Propane ) ) {
				SysHCCompGas( AirLoopNum ) += CompEnergy;
			}
			// Recurring warning for unaccounted equipment types
			// (should never happen, when this does happen enter appropriate equipment CASE statement above)
			break;
		default:
			found = 0;
			if ( NumCompTypes > 0 ) {
				found = FindItemInList( CompType, CompTypeErrors.CompType(), NumCompTypes );
			}
			if ( found == 0 ) {
				CompTypeErrors( ++NumCompTypes ).CompType = CompType;
				found = NumCompTypes;
			}
			ShowRecurringSevereErrorAtEnd( "CalcSystemEnergyUse: Component Type=" + CompType + " not logged as one of allowable Component Types.", CompTypeErrors( found ).CompErrIndex );
			break;
		} // switch

	}

	void
	ReportMaxVentilationLoads()
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         Dan Fisher (with minor assistance from RKS)
		//       DATE WRITTEN   July 2004
		//       MODIFIED       Dec. 2006, BG. reengineered to add zone forced air units to vent rates and loads
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// calculate and report zone ventilation loads

		// METHODOLOGY EMPLOYED:
		// calculate energy contribution of outside air through mixing box and pro-rate to
		// zones according to zone mass flow rates.

		// REFERENCES:
		// na

		// Using/Aliasing
		using Psychrometrics::PsyHFnTdbW;
		using Psychrometrics::PsyRhoAirFnPbTdbW;
		using namespace DataZoneEnergyDemands;
		using namespace DataGlobalConstants;
		using DataHeatBalance::Zone;
		using DataHeatBalance::ZnAirRpt;
		using DataHeatBalance::ZonePreDefRep;
		using DataHeatBalFanSys::MAT;
		using DataHeatBalFanSys::ZoneAirHumRatAvg;
		using DataEnvironment::StdBaroPress;
		using DataEnvironment::StdRhoAir;
		using DataEnvironment::OutAirDensity;
		using DataEnvironment::OutBaroPress;

		using WindowAC::GetWindowACOutAirNode;
		using WindowAC::GetWindowACMixedAirNode;
		using WindowAC::GetWindowACZoneInletAirNode;
		using WindowAC::GetWindowACReturnAirNode;
		using PackagedTerminalHeatPump::GetPTUnitOutAirNode;
		using PackagedTerminalHeatPump::GetPTUnitMixedAirNode;
		using PackagedTerminalHeatPump::GetPTUnitZoneInletAirNode;
		using PackagedTerminalHeatPump::GetPTUnitReturnAirNode;
		using FanCoilUnits::GetFanCoilOutAirNode;
		using FanCoilUnits::GetFanCoilMixedAirNode;
		using FanCoilUnits::GetFanCoilZoneInletAirNode;
		using FanCoilUnits::GetFanCoilReturnAirNode;
		using UnitVentilator::GetUnitVentilatorOutAirNode;
		using UnitVentilator::GetUnitVentilatorMixedAirNode;
		using UnitVentilator::GetUnitVentilatorZoneInletAirNode;
		using UnitVentilator::GetUnitVentilatorReturnAirNode;
		using PurchasedAirManager::GetPurchasedAirOutAirMassFlow;
		using PurchasedAirManager::GetPurchasedAirZoneInletAirNode;
		using PurchasedAirManager::GetPurchasedAirMixedAirTemp;
		using PurchasedAirManager::GetPurchasedAirMixedAirHumRat;
		using PurchasedAirManager::GetPurchasedAirReturnAirNode;
		using HVACStandAloneERV::GetStandAloneERVOutAirNode;
		using HVACStandAloneERV::GetStandAloneERVReturnAirNode;
		using HVACStandAloneERV::GetStandAloneERVZoneInletAirNode;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		Real64 const SmallLoad( 0.1 ); // (W)
		Real64 const KJperJ( 0.001 ); // kilojoules per joules

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int CtrlZoneNum; // ZONE counter
		int ZoneInNum; // counter for zone air distribution inlets
		int ReturnAirNode; // node number for return node on primary air loop
		int MixedAirNode; // mixed air node number (right after the mixing box) on primary air loop
		int AirLoopNum;
		int AirDistCoolInletNodeNum;
		int AirDistHeatInletNodeNum;

		Real64 AirSysEnthReturnAir; // enthalpy of the return air (mixing box inlet node, return side)
		Real64 AirSysEnthMixedAir; // enthalpy of the mixed air (mixing box outlet node, mixed air side)
		Real64 AirSysZoneVentLoad; // ventilation load attributed to a particular zone from primary air system
		Real64 ADUCoolFlowrate;
		Real64 ADUHeatFlowrate;
		Real64 AirSysTotalMixFlowRate; // Mixed air flow
		Real64 AirSysOutAirFlow; // outside air flow rate for zone from primary air system

		Real64 ZFAUEnthReturnAir; // Zone forced Air unit enthalpy of the return air
		Real64 ZFAUTempMixedAir; // Zone forced Air unit dry-bulb temperature of the mixed air
		Real64 ZFAUHumRatMixedAir; // Zone forced Air unit humidity ratio of the mixed air
		Real64 ZFAUEnthMixedAir; // Zone forced Air unit enthalpy of the mixed air
		Real64 ZFAUFlowRate;
		Real64 ZFAUZoneVentLoad; // ventilation load attributed to a particular zone from zone forced air units
		Real64 ZFAUOutAirFlow; // outside air flow rate for zone from zone forced air units.
		int ZoneInletAirNode;

		Real64 ZoneVentLoad; // ventilation load attributed to a particular zone
		Real64 ZoneLoad; // ventilation load attributed to a particular zone
		Real64 OutAirFlow; // Total outside air flow
		Real64 ZoneFlowFrac; // fraction of mixed air flowing to a zone
		Real64 ZoneVolume; // Volume of zone
		Real64 currentZoneAirDensity; // current zone air density (outside barometric pressure)

		int ActualZoneNum;
		int OutAirNode;
		int thisZoneEquipNum; // loop counter

		//  CALL GetComponentEnergyUse
		if ( ! VentReportStructureCreated ) return;
		if ( ! VentLoadsReportEnabled ) return;
		//following inits are array assignments across all controlled zones.
		ZoneOAMassFlow = 0.0;
		ZoneOAMass = 0.0;
		ZoneOAVolFlowStdRho = 0.0;
		ZoneOAVolStdRho = 0.0;
		ZoneOAVolFlowCrntRho = 0.0;
		ZoneOAVolCrntRho = 0.0;
		ZoneMechACH = 0.0;
		MaxCoolingLoadMetByVent = 0.0;
		MaxCoolingLoadAddedByVent = 0.0;
		MaxOvercoolingByVent = 0.0;
		MaxHeatingLoadMetByVent = 0.0;
		MaxHeatingLoadAddedByVent = 0.0;
		MaxOverheatingByVent = 0.0;
		MaxNoLoadHeatingByVent = 0.0;
		MaxNoLoadCoolingByVent = 0.0;

		for ( CtrlZoneNum = 1; CtrlZoneNum <= NumOfZones; ++CtrlZoneNum ) {
			if ( ! ZoneEquipConfig( CtrlZoneNum ).IsControlled ) continue;
			// first clear out working variables from previous zone.
			AirDistCoolInletNodeNum = 0;
			AirDistHeatInletNodeNum = 0;
			ADUCoolFlowrate = 0.0;
			ADUHeatFlowrate = 0.0;
			AirSysTotalMixFlowRate = 0.0;
			AirSysZoneVentLoad = 0.0;
			AirSysOutAirFlow = 0.0;
			ZFAUFlowRate = 0.0;
			ZFAUZoneVentLoad = 0.0;
			ZFAUOutAirFlow = 0.0;
			OutAirFlow = 0.0;
			ZoneFlowFrac = 0.0;
			ZoneVolume = 0.0;

			//retrieve the zone load for each zone
			ActualZoneNum = ZoneEquipConfig( CtrlZoneNum ).ActualZoneNum;
			ZoneLoad = ZoneSysEnergyDemand( ActualZoneNum ).TotalOutputRequired;
			ZoneVolume = Zone( ActualZoneNum ).Volume * Zone( ActualZoneNum ).Multiplier * Zone( ActualZoneNum ).ListMultiplier; //CR 7170

			//if system operating in deadband reset zone load
			if ( DeadBandOrSetback( ActualZoneNum ) ) ZoneLoad = 0.0;
			if ( DeadBandOrSetback( ActualZoneNum ) ) {
				DBFlag = 1;
			} else {
				DBFlag = 0;
			}

			//  IF(AirLoopNum == 0 ) CYCLE   !orig line (BG 12-8-06 changed, zone forced air equipment seems to get excluded here...)

			// first deal with any (and all) Zone Forced Air Units that might have outside air.
			for ( thisZoneEquipNum = 1; thisZoneEquipNum <= ZoneEquipList( ZoneEquipConfig( CtrlZoneNum ).EquipListIndex ).NumOfEquipTypes; ++thisZoneEquipNum ) {
				{ auto const SELECT_CASE_var( ZoneEquipList( ZoneEquipConfig( CtrlZoneNum ).EquipListIndex ).EquipType_Num( thisZoneEquipNum ) );
				// case statement to cover all possible zone forced air units that could have outside air
				if ( SELECT_CASE_var == WindowAC_Num ) { // Window Air Conditioner
					OutAirNode = GetWindowACOutAirNode( ZoneEquipList( ZoneEquipConfig( CtrlZoneNum ).EquipListIndex ).EquipIndex( thisZoneEquipNum ) );
					if ( OutAirNode > 0 ) ZFAUOutAirFlow += Node( OutAirNode ).MassFlowRate;

					ZoneInletAirNode = GetWindowACZoneInletAirNode( ZoneEquipList( ZoneEquipConfig( CtrlZoneNum ).EquipListIndex ).EquipIndex( thisZoneEquipNum ) );
					if ( ZoneInletAirNode > 0 ) ZFAUFlowRate = max( Node( ZoneInletAirNode ).MassFlowRate, 0.0 );
					MixedAirNode = GetWindowACMixedAirNode( ZoneEquipList( ZoneEquipConfig( CtrlZoneNum ).EquipListIndex ).EquipIndex( thisZoneEquipNum ) );
					ReturnAirNode = GetWindowACReturnAirNode( ZoneEquipList( ZoneEquipConfig( CtrlZoneNum ).EquipListIndex ).EquipIndex( thisZoneEquipNum ) );
					if ( ( MixedAirNode > 0 ) && ( ReturnAirNode > 0 ) ) {
						ZFAUEnthMixedAir = PsyHFnTdbW( Node( MixedAirNode ).Temp, Node( MixedAirNode ).HumRat );
						ZFAUEnthReturnAir = PsyHFnTdbW( Node( ReturnAirNode ).Temp, Node( ReturnAirNode ).HumRat );
						//Calculate the zone ventilation load for this supply air path (i.e. zone inlet)
						ZFAUZoneVentLoad += ( ZFAUFlowRate ) * ( ZFAUEnthMixedAir - ZFAUEnthReturnAir ) * TimeStepSys * SecInHour; //*KJperJ
					} else {
						ZFAUZoneVentLoad += 0.0;
					}

				} else if ( ( SELECT_CASE_var == PkgTermHPAirToAir_Num ) || ( SELECT_CASE_var == PkgTermACAirToAir_Num ) || ( SELECT_CASE_var == PkgTermHPWaterToAir_Num ) ) {
					OutAirNode = GetPTUnitOutAirNode( ZoneEquipList( ZoneEquipConfig( CtrlZoneNum ).EquipListIndex ).EquipIndex( thisZoneEquipNum ), ZoneEquipList( ZoneEquipConfig( CtrlZoneNum ).EquipListIndex ).EquipType_Num( thisZoneEquipNum ) );
					if ( OutAirNode > 0 ) ZFAUOutAirFlow += Node( OutAirNode ).MassFlowRate;

					ZoneInletAirNode = GetPTUnitZoneInletAirNode( ZoneEquipList( ZoneEquipConfig( CtrlZoneNum ).EquipListIndex ).EquipIndex( thisZoneEquipNum ), ZoneEquipList( ZoneEquipConfig( CtrlZoneNum ).EquipListIndex ).EquipType_Num( thisZoneEquipNum ) );
					if ( ZoneInletAirNode > 0 ) ZFAUFlowRate = max( Node( ZoneInletAirNode ).MassFlowRate, 0.0 );
					MixedAirNode = GetPTUnitMixedAirNode( ZoneEquipList( ZoneEquipConfig( CtrlZoneNum ).EquipListIndex ).EquipIndex( thisZoneEquipNum ), ZoneEquipList( ZoneEquipConfig( CtrlZoneNum ).EquipListIndex ).EquipType_Num( thisZoneEquipNum ) );
					ReturnAirNode = GetPTUnitReturnAirNode( ZoneEquipList( ZoneEquipConfig( CtrlZoneNum ).EquipListIndex ).EquipIndex( thisZoneEquipNum ), ZoneEquipList( ZoneEquipConfig( CtrlZoneNum ).EquipListIndex ).EquipType_Num( thisZoneEquipNum ) );
					if ( ( MixedAirNode > 0 ) && ( ReturnAirNode > 0 ) ) {
						ZFAUEnthMixedAir = PsyHFnTdbW( Node( MixedAirNode ).Temp, Node( MixedAirNode ).HumRat );
						ZFAUEnthReturnAir = PsyHFnTdbW( Node( ReturnAirNode ).Temp, Node( ReturnAirNode ).HumRat );
						//Calculate the zone ventilation load for this supply air path (i.e. zone inlet)
						ZFAUZoneVentLoad += ( ZFAUFlowRate ) * ( ZFAUEnthMixedAir - ZFAUEnthReturnAir ) * TimeStepSys * SecInHour; //*KJperJ
					} else {
						ZFAUZoneVentLoad += 0.0;
					}

				} else if ( SELECT_CASE_var == FanCoil4Pipe_Num ) {
					OutAirNode = GetFanCoilOutAirNode( ZoneEquipList( ZoneEquipConfig( CtrlZoneNum ).EquipListIndex ).EquipIndex( thisZoneEquipNum ) );
					if ( OutAirNode > 0 ) ZFAUOutAirFlow += Node( OutAirNode ).MassFlowRate;

					ZoneInletAirNode = GetFanCoilZoneInletAirNode( ZoneEquipList( ZoneEquipConfig( CtrlZoneNum ).EquipListIndex ).EquipIndex( thisZoneEquipNum ) );
					if ( ZoneInletAirNode > 0 ) ZFAUFlowRate = max( Node( ZoneInletAirNode ).MassFlowRate, 0.0 );
					MixedAirNode = GetFanCoilMixedAirNode( ZoneEquipList( ZoneEquipConfig( CtrlZoneNum ).EquipListIndex ).EquipIndex( thisZoneEquipNum ) );
					ReturnAirNode = GetFanCoilReturnAirNode( ZoneEquipList( ZoneEquipConfig( CtrlZoneNum ).EquipListIndex ).EquipIndex( thisZoneEquipNum ) );
					if ( ( MixedAirNode > 0 ) && ( ReturnAirNode > 0 ) ) {
						ZFAUEnthMixedAir = PsyHFnTdbW( Node( MixedAirNode ).Temp, Node( MixedAirNode ).HumRat );
						ZFAUEnthReturnAir = PsyHFnTdbW( Node( ReturnAirNode ).Temp, Node( ReturnAirNode ).HumRat );
						//Calculate the zone ventilation load for this supply air path (i.e. zone inlet)
						ZFAUZoneVentLoad += ( ZFAUFlowRate ) * ( ZFAUEnthMixedAir - ZFAUEnthReturnAir ) * TimeStepSys * SecInHour; //*KJperJ
					} else {
						ZFAUZoneVentLoad += 0.0;
					}

				} else if ( SELECT_CASE_var == UnitVentilator_Num ) {
					OutAirNode = GetUnitVentilatorOutAirNode( ZoneEquipList( ZoneEquipConfig( CtrlZoneNum ).EquipListIndex ).EquipIndex( thisZoneEquipNum ) );
					if ( OutAirNode > 0 ) ZFAUOutAirFlow += Node( OutAirNode ).MassFlowRate;

					ZoneInletAirNode = GetUnitVentilatorZoneInletAirNode( ZoneEquipList( ZoneEquipConfig( CtrlZoneNum ).EquipListIndex ).EquipIndex( thisZoneEquipNum ) );
					if ( ZoneInletAirNode > 0 ) ZFAUFlowRate = max( Node( ZoneInletAirNode ).MassFlowRate, 0.0 );
					MixedAirNode = GetUnitVentilatorMixedAirNode( ZoneEquipList( ZoneEquipConfig( CtrlZoneNum ).EquipListIndex ).EquipIndex( thisZoneEquipNum ) );
					ReturnAirNode = GetUnitVentilatorReturnAirNode( ZoneEquipList( ZoneEquipConfig( CtrlZoneNum ).EquipListIndex ).EquipIndex( thisZoneEquipNum ) );
					if ( ( MixedAirNode > 0 ) && ( ReturnAirNode > 0 ) ) {
						ZFAUEnthMixedAir = PsyHFnTdbW( Node( MixedAirNode ).Temp, Node( MixedAirNode ).HumRat );
						ZFAUEnthReturnAir = PsyHFnTdbW( Node( ReturnAirNode ).Temp, Node( ReturnAirNode ).HumRat );
						//Calculate the zone ventilation load for this supply air path (i.e. zone inlet)
						ZFAUZoneVentLoad += ( ZFAUFlowRate ) * ( ZFAUEnthMixedAir - ZFAUEnthReturnAir ) * TimeStepSys * SecInHour; //*KJperJ
					} else {
						ZFAUZoneVentLoad += 0.0;
					}
				} else if ( SELECT_CASE_var == PurchasedAir_Num ) {
					ZFAUOutAirFlow += GetPurchasedAirOutAirMassFlow( ZoneEquipList( ZoneEquipConfig( CtrlZoneNum ).EquipListIndex ).EquipIndex( thisZoneEquipNum ) );
					ZoneInletAirNode = GetPurchasedAirZoneInletAirNode( ZoneEquipList( ZoneEquipConfig( CtrlZoneNum ).EquipListIndex ).EquipIndex( thisZoneEquipNum ) );
					if ( ZoneInletAirNode > 0 ) ZFAUFlowRate = max( Node( ZoneInletAirNode ).MassFlowRate, 0.0 );
					ZFAUTempMixedAir = GetPurchasedAirMixedAirTemp( ZoneEquipList( ZoneEquipConfig( CtrlZoneNum ).EquipListIndex ).EquipIndex( thisZoneEquipNum ) );
					ZFAUHumRatMixedAir = GetPurchasedAirMixedAirHumRat( ZoneEquipList( ZoneEquipConfig( CtrlZoneNum ).EquipListIndex ).EquipIndex( thisZoneEquipNum ) );
					ReturnAirNode = GetPurchasedAirReturnAirNode( ZoneEquipList( ZoneEquipConfig( CtrlZoneNum ).EquipListIndex ).EquipIndex( thisZoneEquipNum ) );
					if ( ( ZFAUFlowRate > 0 ) && ( ReturnAirNode > 0 ) ) {
						ZFAUEnthMixedAir = PsyHFnTdbW( ZFAUTempMixedAir, ZFAUHumRatMixedAir );
						ZFAUEnthReturnAir = PsyHFnTdbW( Node( ReturnAirNode ).Temp, Node( ReturnAirNode ).HumRat );
						//Calculate the zone ventilation load for this supply air path (i.e. zone inlet)
						ZFAUZoneVentLoad += ( ZFAUFlowRate ) * ( ZFAUEnthMixedAir - ZFAUEnthReturnAir ) * TimeStepSys * SecInHour; //*KJperJ
					} else {
						ZFAUZoneVentLoad += 0.0;
					}
				} else if ( SELECT_CASE_var == ERVStandAlone_Num ) {
					OutAirNode = GetStandAloneERVOutAirNode( ZoneEquipList( ZoneEquipConfig( CtrlZoneNum ).EquipListIndex ).EquipIndex( thisZoneEquipNum ) );
					if ( OutAirNode > 0 ) ZFAUOutAirFlow += Node( OutAirNode ).MassFlowRate;

					ZoneInletAirNode = GetStandAloneERVZoneInletAirNode( ZoneEquipList( ZoneEquipConfig( CtrlZoneNum ).EquipListIndex ).EquipIndex( thisZoneEquipNum ) );
					if ( ZoneInletAirNode > 0 ) ZFAUFlowRate = max( Node( ZoneInletAirNode ).MassFlowRate, 0.0 );
					MixedAirNode = ZoneInletAirNode;
					ReturnAirNode = GetStandAloneERVReturnAirNode( ZoneEquipList( ZoneEquipConfig( CtrlZoneNum ).EquipListIndex ).EquipIndex( thisZoneEquipNum ) );
					if ( ( MixedAirNode > 0 ) && ( ReturnAirNode > 0 ) ) {
						ZFAUEnthMixedAir = PsyHFnTdbW( Node( MixedAirNode ).Temp, Node( MixedAirNode ).HumRat );
						ZFAUEnthReturnAir = PsyHFnTdbW( Node( ReturnAirNode ).Temp, Node( ReturnAirNode ).HumRat );
						//Calculate the zone ventilation load for this supply air path (i.e. zone inlet)
						ZFAUZoneVentLoad += ( ZFAUFlowRate ) * ( ZFAUEnthMixedAir - ZFAUEnthReturnAir ) * TimeStepSys * SecInHour; //*KJperJ
					} else {
						ZFAUZoneVentLoad += 0.0;
					}

				}}

			}

			// retrieve air loop indexes
			AirLoopNum = ZoneEquipConfig( CtrlZoneNum ).AirLoopNum;
			if ( AirLoopNum != 0 ) { // deal with primary air system
				//loop over the zone supply air path inlet nodes

				for ( ZoneInNum = 1; ZoneInNum <= ZoneEquipConfig( CtrlZoneNum ).NumInletNodes; ++ZoneInNum ) {
					AirDistCoolInletNodeNum = max( ZoneEquipConfig( CtrlZoneNum ).AirDistUnitCool( ZoneInNum ).InNode, 0 );
					AirDistHeatInletNodeNum = max( ZoneEquipConfig( CtrlZoneNum ).AirDistUnitHeat( ZoneInNum ).InNode, 0 );
					// Set for cooling or heating path
					if ( AirDistCoolInletNodeNum > 0 && AirDistHeatInletNodeNum == 0 ) {
						ADUCoolFlowrate += max( Node( ZoneEquipConfig( CtrlZoneNum ).AirDistUnitCool( ZoneInNum ).InNode ).MassFlowRate, 0.0 ); // CR7244 need to accumulate flow across multiple inlets
					} else if ( AirDistHeatInletNodeNum > 0 && AirDistCoolInletNodeNum == 0 ) {
						ADUHeatFlowrate += max( Node( ZoneEquipConfig( CtrlZoneNum ).AirDistUnitHeat( ZoneInNum ).InNode ).MassFlowRate, 0.0 ); // CR7244 need to accumulate flow across multiple inlets
					} else if ( AirDistCoolInletNodeNum > 0 && AirDistHeatInletNodeNum > 0 && AirDistCoolInletNodeNum != AirDistHeatInletNodeNum ) {
						// dual ducts! CR7244 need to accumulate flow across multiple inlets (don't count same inlet twice)
						ADUHeatFlowrate += max( Node( ZoneEquipConfig( CtrlZoneNum ).AirDistUnitHeat( ZoneInNum ).InNode ).MassFlowRate, 0.0 ); // CR7244 need to accumulate flow across multiple inlets
						ADUCoolFlowrate += max( Node( ZoneEquipConfig( CtrlZoneNum ).AirDistUnitCool( ZoneInNum ).InNode ).MassFlowRate, 0.0 ); // CR7244 need to accumulate flow across multiple inlets
					} else if ( AirDistCoolInletNodeNum > 0 && AirDistHeatInletNodeNum > 0 ) {
						// dual ducts! CR7244 need to accumulate flow across multiple inlets (don't count same inlet twice)
						ADUCoolFlowrate += max( Node( ZoneEquipConfig( CtrlZoneNum ).AirDistUnitCool( ZoneInNum ).InNode ).MassFlowRate, 0.0 ); // CR7244 need to accumulate flow across multiple inlets
					} else {
						// do nothing (already inits)
					}
				}

				//Find the mixed air node and return air node of the system that supplies the zone
				MixedAirNode = PrimaryAirSystem( AirLoopNum ).OASysOutletNodeNum;
				ReturnAirNode = PrimaryAirSystem( AirLoopNum ).OASysInletNodeNum;
				if ( MixedAirNode == 0 || ReturnAirNode == 0 ) {
					AirSysZoneVentLoad = 0.0;
					AirSysOutAirFlow = 0.0;
				} else {
					//Calculate return and mixed air ethalpies
					AirSysEnthReturnAir = PsyHFnTdbW( Node( ReturnAirNode ).Temp, Node( ReturnAirNode ).HumRat );
					AirSysEnthMixedAir = PsyHFnTdbW( Node( MixedAirNode ).Temp, Node( MixedAirNode ).HumRat );

					if ( PrimaryAirSystem( AirLoopNum ).OASysExists ) {
						OutAirNode = PrimaryAirSystem( AirLoopNum ).OAMixOAInNodeNum;
						AirSysOutAirFlow = Node( OutAirNode ).MassFlowRate;
					} else {
						AirSysOutAirFlow = 0.0;
					}

					AirSysTotalMixFlowRate = Node( MixedAirNode ).MassFlowRate;

					if ( AirSysTotalMixFlowRate != 0.0 ) {
						ZoneFlowFrac = ( ADUCoolFlowrate + ADUHeatFlowrate ) / AirSysTotalMixFlowRate;
						AirSysOutAirFlow *= ZoneFlowFrac;
					} else {
						ZoneFlowFrac = 0.0;
						AirSysOutAirFlow = 0.0;
					}
					//Calculate the zone ventilation load for this supply air path (i.e. zone inlet)
					AirSysZoneVentLoad = ( ADUCoolFlowrate + ADUHeatFlowrate ) * ( AirSysEnthMixedAir - AirSysEnthReturnAir ) * TimeStepSys * SecInHour; //*KJperJ
				}

			} // primary air system present

			//now combine OA flow from zone forced air units with primary air system
			OutAirFlow = AirSysOutAirFlow + ZFAUOutAirFlow;
			// assign report variables
			ZoneOAMassFlow( CtrlZoneNum ) = OutAirFlow;
			ZoneOAMass( CtrlZoneNum ) = ZoneOAMassFlow( CtrlZoneNum ) * TimeStepSys * SecInHour;

			// determine volumetric values from mass flow using standard density (adjusted for elevation)
			ZoneOAVolFlowStdRho( CtrlZoneNum ) = ZoneOAMassFlow( CtrlZoneNum ) / StdRhoAir;
			ZoneOAVolStdRho( CtrlZoneNum ) = ZoneOAVolFlowStdRho( CtrlZoneNum ) * TimeStepSys * SecInHour;

			// determine volumetric values from mass flow using current air density for zone (adjusted for elevation)
			currentZoneAirDensity = PsyRhoAirFnPbTdbW( OutBaroPress, MAT( ActualZoneNum ), ZoneAirHumRatAvg( ActualZoneNum ) );
			if ( currentZoneAirDensity > 0.0 ) ZoneOAVolFlowCrntRho( CtrlZoneNum ) = ZoneOAMassFlow( CtrlZoneNum ) / currentZoneAirDensity;
			ZoneOAVolCrntRho( CtrlZoneNum ) = ZoneOAVolFlowCrntRho( CtrlZoneNum ) * TimeStepSys * SecInHour;
			if ( ZoneVolume > 0.0 ) ZoneMechACH( CtrlZoneNum ) = ( ZoneOAVolCrntRho( CtrlZoneNum ) / TimeStepSys ) / ZoneVolume;

			//store data for predefined tabular report on outside air
			if ( ZonePreDefRep( ActualZoneNum ).isOccupied ) {
				//accumulate the occupied time
				ZonePreDefRep( ActualZoneNum ).TotTimeOcc += TimeStepSys;
				//mechnical ventilation
				ZonePreDefRep( ActualZoneNum ).MechVentVolTotal += ZoneOAVolCrntRho( CtrlZoneNum );
				if ( ( ZoneOAVolCrntRho( CtrlZoneNum ) / TimeStepSys ) < ZonePreDefRep( ActualZoneNum ).MechVentVolMin ) {
					ZonePreDefRep( ActualZoneNum ).MechVentVolMin = ZoneOAVolCrntRho( CtrlZoneNum ) / TimeStepSys;
				}
				//infiltration
				ZonePreDefRep( ActualZoneNum ).InfilVolTotal += ZnAirRpt( ActualZoneNum ).InfilVolumeCurDensity;
				if ( ZnAirRpt( ActualZoneNum ).InfilVolumeCurDensity < ZonePreDefRep( ActualZoneNum ).InfilVolMin ) {
					ZonePreDefRep( ActualZoneNum ).InfilVolMin = ZnAirRpt( ActualZoneNum ).InfilVolumeCurDensity;
				}
				//'simple' mechanical ventilation
				ZonePreDefRep( ActualZoneNum ).SimpVentVolTotal += ZnAirRpt( ActualZoneNum ).VentilVolumeCurDensity;
				if ( ZnAirRpt( ActualZoneNum ).VentilVolumeCurDensity < ZonePreDefRep( ActualZoneNum ).SimpVentVolMin ) {
					ZonePreDefRep( ActualZoneNum ).SimpVentVolMin = ZnAirRpt( ActualZoneNum ).VentilVolumeCurDensity;
				}
			}

			//now combine Vent load from zone forced air units with primary air system
			ZoneVentLoad = AirSysZoneVentLoad + ZFAUZoneVentLoad;
			//cycle if ZoneVentLoad is small
			if ( std::abs( ZoneVentLoad ) < SmallLoad ) continue; // orig. had RETURN here, BG changed to CYCLE for next controlled zone in do loop.

			//Ventilation Heating
			if ( ZoneVentLoad > SmallLoad ) {
				//Zone cooling load
				if ( ZoneLoad < -SmallLoad ) {
					MaxCoolingLoadAddedByVent( CtrlZoneNum ) += std::abs( ZoneVentLoad );
					//Zone heating load
				} else if ( ZoneLoad > SmallLoad ) {
					if ( ZoneVentLoad > ZoneLoad ) {
						MaxHeatingLoadMetByVent( CtrlZoneNum ) += std::abs( ZoneLoad );
						MaxOverheatingByVent( CtrlZoneNum ) += ( ZoneVentLoad - ZoneLoad );
					} else {
						MaxHeatingLoadMetByVent( CtrlZoneNum ) += std::abs( ZoneVentLoad );
					}
					//No Zone Load
				} else {
					MaxNoLoadHeatingByVent( CtrlZoneNum ) += std::abs( ZoneVentLoad );
				}

				//Ventilation Cooling
			} else if ( ZoneVentLoad < -SmallLoad ) {
				//Zone cooling load
				if ( ZoneLoad < -SmallLoad ) {
					if ( ZoneVentLoad < ZoneLoad ) {
						MaxCoolingLoadMetByVent( CtrlZoneNum ) += std::abs( ZoneLoad );
						MaxOvercoolingByVent( CtrlZoneNum ) += std::abs( ZoneVentLoad - ZoneLoad );
					} else {
						MaxCoolingLoadMetByVent( CtrlZoneNum ) += std::abs( ZoneVentLoad );
					}
					//Zone heating load
				} else if ( ZoneLoad > SmallLoad ) {
					MaxHeatingLoadAddedByVent( CtrlZoneNum ) += std::abs( ZoneVentLoad );
					//No Zone Load
				} else {
					MaxNoLoadCoolingByVent( CtrlZoneNum ) += std::abs( ZoneVentLoad );
				}

				//Ventilation No Load
			} else {
			}
		} // loop over controlled zones
	}

	void
	MatchPlantSys(
		int const AirLoopNum, // counter for zone air distribution inlets
		int const BranchNum // counter for zone air distribution inlets
	)
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         Dan Fisher
		//       DATE WRITTEN   May 2005
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// calculate and report zone ventilation loads

		// METHODOLOGY EMPLOYED:
		// calculate energy contribution of outside air through mixing box and pro-rate to
		// zones according to zone mass flow rates.

		// REFERENCES:
		// na

		// Using/Aliasing
		using namespace DataGlobalConstants;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		int const EnergyTrans( 1 );
		int const PrimaryAirLoop( 1 );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		std::string CompType;
		std::string CompName;
		int CompNum; // counter for components on air loop branch connected to air distribution unit
		int VarNum;
		int SubCompNum; // counter for components on air loop branch connected to air distribution unit
		int SubSubCompNum; // counter for components on air loop branch connected to air distribution unit
		bool MatchFound; // Set to .TRUE. when a match is found
		int MatchLoop; // Loop number of the match
		int MatchBranch; // Branch number of the match
		int MatchComp; // Component number of the match
		int MatchLoopType;
		int Idx;

		for ( CompNum = 1; CompNum <= PrimaryAirSystem( AirLoopNum ).Branch( BranchNum ).TotalComponents; ++CompNum ) {
			for ( VarNum = 1; VarNum <= PrimaryAirSystem( AirLoopNum ).Branch( BranchNum ).Comp( CompNum ).NumMeteredVars; ++VarNum ) {
				if ( PrimaryAirSystem( AirLoopNum ).Branch( BranchNum ).Comp( CompNum ).MeteredVar( VarNum ).ResourceType == iRT_EnergyTransfer ) {
					PrimaryAirSystem( AirLoopNum ).Branch( BranchNum ).Comp( CompNum ).EnergyTransComp = EnergyTrans;
					CompType = PrimaryAirSystem( AirLoopNum ).Branch( BranchNum ).Comp( CompNum ).TypeOf;
					CompName = PrimaryAirSystem( AirLoopNum ).Branch( BranchNum ).Comp( CompNum ).Name;
					Idx = 0;
					FindDemandSideMatch( CompType, CompName, MatchFound, MatchLoopType, MatchLoop, MatchBranch, MatchComp );
					if ( MatchFound ) UpdateAirSysCompPtrArray( Idx, AirLoopNum, BranchNum, CompNum, MatchLoopType, MatchLoop, MatchBranch, MatchComp );
					PrimaryAirSystem( AirLoopNum ).Branch( BranchNum ).Comp( CompNum ).AirSysToPlantPtr = Idx;
					break;
				}
			}
			for ( SubCompNum = 1; SubCompNum <= PrimaryAirSystem( AirLoopNum ).Branch( BranchNum ).Comp( CompNum ).NumSubComps; ++SubCompNum ) {
				//!!!!          IF(SysVentLoad == 0.0d0)EXIT
				for ( VarNum = 1; VarNum <= PrimaryAirSystem( AirLoopNum ).Branch( BranchNum ).Comp( CompNum ).SubComp( SubCompNum ).NumMeteredVars; ++VarNum ) {
					if ( PrimaryAirSystem( AirLoopNum ).Branch( BranchNum ).Comp( CompNum ).SubComp( SubCompNum ).MeteredVar( VarNum ).ResourceType == iRT_EnergyTransfer ) {
						PrimaryAirSystem( AirLoopNum ).Branch( BranchNum ).Comp( CompNum ).SubComp( SubCompNum ).EnergyTransComp = EnergyTrans;
						CompType = PrimaryAirSystem( AirLoopNum ).Branch( BranchNum ).Comp( CompNum ).SubComp( SubCompNum ).TypeOf;
						CompName = PrimaryAirSystem( AirLoopNum ).Branch( BranchNum ).Comp( CompNum ).SubComp( SubCompNum ).Name;
						Idx = 0;
						FindDemandSideMatch( CompType, CompName, MatchFound, MatchLoopType, MatchLoop, MatchBranch, MatchComp );
						if ( MatchFound ) UpdateAirSysSubCompPtrArray( Idx, AirLoopNum, BranchNum, CompNum, SubCompNum, MatchLoopType, MatchLoop, MatchBranch, MatchComp );
						PrimaryAirSystem( AirLoopNum ).Branch( BranchNum ).Comp( CompNum ).SubComp( SubCompNum ).AirSysToPlantPtr = Idx;
						break;
					}
				}
				for ( SubSubCompNum = 1; SubSubCompNum <= PrimaryAirSystem( AirLoopNum ).Branch( BranchNum ).Comp( CompNum ).SubComp( SubCompNum ).NumSubSubComps; ++SubSubCompNum ) {
					//!!!!            IF(SysVentLoad == 0.0d0)EXIT
					for ( VarNum = 1; VarNum <= PrimaryAirSystem( AirLoopNum ).Branch( BranchNum ).Comp( CompNum ).SubComp( SubCompNum ).SubSubComp( SubSubCompNum ).NumMeteredVars; ++VarNum ) {
						if ( PrimaryAirSystem( AirLoopNum ).Branch( BranchNum ).Comp( CompNum ).SubComp( SubCompNum ).SubSubComp( SubSubCompNum ).MeteredVar( VarNum ).ResourceType == iRT_EnergyTransfer ) {
							PrimaryAirSystem( AirLoopNum ).Branch( BranchNum ).Comp( CompNum ).SubComp( SubCompNum ).SubSubComp( SubSubCompNum ).EnergyTransComp = EnergyTrans;
							CompType = PrimaryAirSystem( AirLoopNum ).Branch( BranchNum ).Comp( CompNum ).SubComp( SubCompNum ).SubSubComp( SubSubCompNum ).TypeOf;
							CompName = PrimaryAirSystem( AirLoopNum ).Branch( BranchNum ).Comp( CompNum ).SubComp( SubCompNum ).SubSubComp( SubSubCompNum ).Name;
							Idx = 0;
							FindDemandSideMatch( CompType, CompName, MatchFound, MatchLoopType, MatchLoop, MatchBranch, MatchComp );
							if ( MatchFound ) UpdateAirSysSubSubCompPtrArray( Idx, AirLoopNum, BranchNum, CompNum, SubCompNum, SubSubCompNum, MatchLoopType, MatchLoop, MatchBranch, MatchComp );
							PrimaryAirSystem( AirLoopNum ).Branch( BranchNum ).Comp( CompNum ).SubComp( SubCompNum ).SubSubComp( SubSubCompNum ).AirSysToPlantPtr = Idx;
							break;
						}
					}
				}
			}
		}

	}

	void
	FindDemandSideMatch(
		std::string const & CompType, // Inlet node of the component to find the match of
		std::string const & CompName, // Outlet node of the component to find the match of
		bool & MatchFound, // Set to .TRUE. when a match is found
		int & MatchLoopType, // Loop number of the match
		int & MatchLoop, // Loop number of the match
		int & MatchBranch, // Branch number of the match
		int & MatchComp // Component number of the match
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Rick Strand
		//       DATE WRITTEN   September 2004
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine intializes the connections between various loops.
		// Due to the fact that this requires numerous string compares, it
		// is much more efficient to find this information once and then
		// store it in module level variables (LoopConnect derived type).

		// METHODOLOGY EMPLOYED:
		// Simply cycles through the plant and condenser demand sides until
		// a component is found that matches the component type and name

		// REFERENCES:
		// na

		// Using/Aliasing
		using InputProcessor::SameString;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int PassBranchNum; // DO loop counter for branches
		int PassCompNum; // DO loop counter for components
		int PassLoopNum; // DO loop counter for loops or the top level of the hierarchy
		// FLOW:
		// Initialize all of the output variables

		MatchFound = false;
		MatchLoopType = 0;
		MatchLoop = 0;
		MatchLoop = 0;
		MatchBranch = 0;
		MatchComp = 0;

		// Now cycle through all of the demand side loops to see if we can find
		// a match for the component type and name.  Once a match is found,
		// record the type of loop and the loop, branch, and component numbers.
		if ( ! MatchFound ) { // Go through the plant demand side loops
			for ( PassLoopNum = 1; PassLoopNum <= NumPlantLoops; ++PassLoopNum ) {
				for ( PassBranchNum = 1; PassBranchNum <= VentRepPlantDemandSide( PassLoopNum ).TotalBranches; ++PassBranchNum ) {
					for ( PassCompNum = 1; PassCompNum <= VentRepPlantDemandSide( PassLoopNum ).Branch( PassBranchNum ).TotalComponents; ++PassCompNum ) {
						if ( SameString( CompType, VentRepPlantDemandSide( PassLoopNum ).Branch( PassBranchNum ).Comp( PassCompNum ).TypeOf ) && SameString( CompName, VentRepPlantDemandSide( PassLoopNum ).Branch( PassBranchNum ).Comp( PassCompNum ).Name ) ) {
							// Found a match on the plant demand side--increment the counter
							MatchFound = true;
							MatchLoopType = 1;
							MatchLoop = PassLoopNum;
							MatchBranch = PassBranchNum;
							MatchComp = PassCompNum;
							break; // PassCompNum DO loop
						}
					}
					if ( MatchFound ) break; // PassBranchNum DO loop
				}
				if ( MatchFound ) break; // PassLoopNum DO loop
			}
		}

		if ( ! MatchFound ) { // Go through the condenser demand side loops
			for ( PassLoopNum = 1; PassLoopNum <= NumCondLoops; ++PassLoopNum ) {
				for ( PassBranchNum = 1; PassBranchNum <= VentRepCondDemandSide( PassLoopNum ).TotalBranches; ++PassBranchNum ) {
					for ( PassCompNum = 1; PassCompNum <= VentRepCondDemandSide( PassLoopNum ).Branch( PassBranchNum ).TotalComponents; ++PassCompNum ) {
						if ( SameString( CompType, VentRepCondDemandSide( PassLoopNum ).Branch( PassBranchNum ).Comp( PassCompNum ).TypeOf ) && SameString( CompName, VentRepCondDemandSide( PassLoopNum ).Branch( PassBranchNum ).Comp( PassCompNum ).Name ) ) {
							// Found a match on the plant demand side--increment the counter
							MatchFound = true;
							MatchLoopType = 2;
							MatchLoop = PassLoopNum;
							MatchBranch = PassBranchNum;
							MatchComp = PassCompNum;
							break; // PassCompNum DO loop
						}
					}
					if ( MatchFound ) break; // PassBranchNum DO loop
				}
				if ( MatchFound ) break; // PassLoopNum DO loop
			}
		}

	}

	void
	ReportAirLoopConnections()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Michael J. Witte, Linda K. Lawrie
		//       DATE WRITTEN   February 2004 (moved from BranchInputManager ReportLoopConnections)
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Report air loop splitter connections to the BND file.

		// METHODOLOGY EMPLOYED:
		// Needs description, as appropriate.

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataGlobals::OutputFileBNDetails;
		using DataHVACGlobals::NumPrimaryAirSys;
		using DataHeatBalance::Zone;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const errstring( "**error**" );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int Count;
		int Count1;
		int CtrldZoneNum;
		int ZoneNum;
		std::string ChrOut;
		std::string ChrOut2;
		std::string ChrOut3;
		std::string ChrOut4;
		std::string ChrOut5;

		// Formats
		static gio::Fmt const Format_701( "(A)" );
		static gio::Fmt const Format_706( "('! <#AirLoopHVACs>,<Number of AirLoopHVACs>')" );
		static gio::Fmt const Format_707( "(1X,A)" );
		static gio::Fmt const Format_708( "('! <AirLoopHVAC>,<Air Loop Name>,<# Return Nodes>,<# Supply Nodes>,','<# Zones Cooled>,<# Zones Heated>,<Outdoor Air Used>')" );
		static gio::Fmt const Format_709( "('! <AirLoop Return Connections>,<Connection Count>,<AirLoopHVAC Name>,','<Zn Eqp Return Node #>,<Zn Eqp Return Node Name>,','<AirLoop Return Node #>,<Air Loop Return Node Name>')" );
		static gio::Fmt const Format_710( "('! <AirLoop Supply Connections>,<Connection Count>,<AirLoopHVAC Name>,','<Zn Eqp Supply Node #>,<Zn Eqp Supply Node Name>,','<AirLoop Supply Node #>,<Air Loop Supply Node Name>')" );
		static gio::Fmt const Format_711( "('! <Cooled Zone Info>,<Cooled Zone Count>,<Cooled Zone Name>,','<Cooled Zone Inlet Node #>,<Cooled Zone Inlet Node Name>,<AirLoopHVAC Name>')" );
		static gio::Fmt const Format_712( "('! <Heated Zone Info>,<Heated Zone Count>,<Heated Zone Name>,','<Heated Zone Inlet Node #>,<Heated Zone Inlet Node Name>,<AirLoopHVAC Name>')" );
		static gio::Fmt const Format_714( "('! <Outdoor Air Connections>,<OA Inlet Node #>,<OA Return Air Inlet Node Name>,','<OA Outlet Node #>,<OA Mixed Air Outlet Node Name>,<AirLoopHVAC Name>'s)" );
		static gio::Fmt const Format_713( "(A)" );

		gio::write( OutputFileBNDetails, Format_701 ) << "! ===============================================================";
		gio::write( OutputFileBNDetails, Format_706 );
		gio::write( ChrOut, fmtLD ) << NumPrimaryAirSys;
		gio::write( OutputFileBNDetails, Format_707 ) << "#AirLoopHVACs," + stripped( ChrOut );
		gio::write( OutputFileBNDetails, Format_708 );
		gio::write( OutputFileBNDetails, Format_709 );
		gio::write( OutputFileBNDetails, Format_710 );
		gio::write( OutputFileBNDetails, Format_711 );
		gio::write( OutputFileBNDetails, Format_712 );
		gio::write( OutputFileBNDetails, Format_714 );
		gio::write( OutputFileBNDetails, Format_713 ) << "! <AirLoopHVAC Connector>,<Connector Type>,<Connector Name>," "<Loop Name>,<Loop Type>,<Number of Inlets/Outlets>";
		gio::write( OutputFileBNDetails, Format_713 ) << "! <AirLoopHVAC Connector Branches>,<Connector Node Count>,<Connector Type>," "<Connector Name>,<Inlet Branch>,<Outlet Branch>," "<Loop Name>,<Loop Type>";
		gio::write( OutputFileBNDetails, Format_713 ) << "! <AirLoopHVAC Connector Nodes>,<Connector Node Count>,<Connector Type>," "<Connector Name>,<Inlet Node>,<Outlet Node>," "<Loop Name>,<Loop Type>";
		for ( Count = 1; Count <= NumPrimaryAirSys; ++Count ) {
			gio::write( ChrOut, fmtLD ) << AirToZoneNodeInfo( Count ).NumReturnNodes;
			gio::write( ChrOut2, fmtLD ) << AirToZoneNodeInfo( Count ).NumSupplyNodes;
			gio::write( ChrOut3, fmtLD ) << AirToZoneNodeInfo( Count ).NumZonesCooled;
			gio::write( ChrOut4, fmtLD ) << AirToZoneNodeInfo( Count ).NumZonesHeated;
			strip( ChrOut );
			strip( ChrOut2 );
			strip( ChrOut3 );
			strip( ChrOut4 );
			if ( AirToOANodeInfo( Count ).OASysExists ) {
				ChrOut5 = "Yes";
			} else {
				ChrOut5 = "No";
			}
			gio::write( OutputFileBNDetails, Format_701 ) << " AirLoopHVAC," + AirToZoneNodeInfo( Count ).AirLoopName + ',' + ChrOut + ',' + ChrOut2 + ',' + ChrOut3 + ',' + ChrOut4 + ',' + ChrOut5;
			for ( Count1 = 1; Count1 <= AirToZoneNodeInfo( Count ).NumReturnNodes; ++Count1 ) {
				gio::write( ChrOut, fmtLD ) << Count1;
				if ( AirToZoneNodeInfo( Count ).ZoneEquipReturnNodeNum( Count1 ) > 0 ) {
					gio::write( ChrOut2, fmtLD ) << AirToZoneNodeInfo( Count ).ZoneEquipReturnNodeNum( Count1 );
				} else {
					ChrOut2 = errstring;
				}
				if ( AirToZoneNodeInfo( Count ).AirLoopReturnNodeNum( Count1 ) > 0 ) {
					gio::write( ChrOut3, fmtLD ) << AirToZoneNodeInfo( Count ).AirLoopReturnNodeNum( Count1 );
				} else {
					ChrOut3 = errstring;
				}
				strip( ChrOut );
				strip( ChrOut2 );
				strip( ChrOut3 );
				{ IOFlags flags; flags.ADVANCE( "No" ); gio::write( OutputFileBNDetails, Format_707, flags ) << "  AirLoop Return Connections," + ChrOut + ',' + AirToZoneNodeInfo( Count ).AirLoopName + ','; }
				if ( ChrOut2 != errstring ) {
					{ IOFlags flags; flags.ADVANCE( "No" ); gio::write( OutputFileBNDetails, Format_701, flags ) << ChrOut2 + ',' + NodeID( AirToZoneNodeInfo( Count ).ZoneEquipReturnNodeNum( Count1 ) ) + ','; }
				} else {
					{ IOFlags flags; flags.ADVANCE( "No" ); gio::write( OutputFileBNDetails, Format_701, flags ) << errstring + ',' + errstring + ','; }
				}
				if ( ChrOut3 != errstring ) {
					gio::write( OutputFileBNDetails, Format_701 ) << ChrOut3 + ',' + NodeID( AirToZoneNodeInfo( Count ).AirLoopReturnNodeNum( Count1 ) );
				} else {
					gio::write( OutputFileBNDetails, Format_701 ) << errstring + ',' + errstring;
				}
			}
			for ( Count1 = 1; Count1 <= AirToZoneNodeInfo( Count ).NumSupplyNodes; ++Count1 ) {
				gio::write( ChrOut, fmtLD ) << Count1;
				if ( AirToZoneNodeInfo( Count ).ZoneEquipSupplyNodeNum( Count1 ) > 0 ) {
					gio::write( ChrOut2, fmtLD ) << AirToZoneNodeInfo( Count ).ZoneEquipSupplyNodeNum( Count1 );
				} else {
					ChrOut2 = errstring;
				}
				if ( AirToZoneNodeInfo( Count ).AirLoopSupplyNodeNum( Count1 ) > 0 ) {
					gio::write( ChrOut3, fmtLD ) << AirToZoneNodeInfo( Count ).AirLoopSupplyNodeNum( Count1 );
				} else {
					ChrOut3 = errstring;
				}
				strip( ChrOut );
				strip( ChrOut2 );
				strip( ChrOut3 );
				{ IOFlags flags; flags.ADVANCE( "No" ); gio::write( OutputFileBNDetails, Format_707, flags ) << "  AirLoop Supply Connections," + ChrOut + ',' + AirToZoneNodeInfo( Count ).AirLoopName + ','; }
				if ( ChrOut2 != errstring ) {
					{ IOFlags flags; flags.ADVANCE( "No" ); gio::write( OutputFileBNDetails, Format_701, flags ) << ChrOut2 + ',' + NodeID( AirToZoneNodeInfo( Count ).ZoneEquipSupplyNodeNum( Count1 ) ) + ','; }
				} else {
					{ IOFlags flags; flags.ADVANCE( "No" ); gio::write( OutputFileBNDetails, Format_701, flags ) << errstring + ',' + errstring + ','; }
				}
				if ( ChrOut3 != errstring ) {
					gio::write( OutputFileBNDetails, Format_701 ) << ChrOut3 + ',' + NodeID( AirToZoneNodeInfo( Count ).AirLoopSupplyNodeNum( Count1 ) );
				} else {
					gio::write( OutputFileBNDetails, Format_701 ) << errstring + ',' + errstring;
				}
			}

			for ( Count1 = 1; Count1 <= AirToZoneNodeInfo( Count ).NumZonesCooled; ++Count1 ) {
				gio::write( ChrOut, fmtLD ) << Count1;
				if ( AirToZoneNodeInfo( Count ).CoolZoneInletNodes( Count1 ) > 0 ) {
					gio::write( ChrOut2, fmtLD ) << AirToZoneNodeInfo( Count ).CoolZoneInletNodes( Count1 );
				} else {
					ChrOut2 = errstring;
				}
				strip( ChrOut );
				strip( ChrOut2 );
				CtrldZoneNum = AirToZoneNodeInfo( Count ).CoolCtrlZoneNums( Count1 );
				ZoneNum = ZoneEquipConfig( CtrldZoneNum ).ActualZoneNum;
				{ IOFlags flags; flags.ADVANCE( "No" ); gio::write( OutputFileBNDetails, Format_707, flags ) << "  Cooled Zone Info," + ChrOut + ',' + Zone( ZoneNum ).Name + ','; }
				if ( ChrOut2 != errstring ) {
					gio::write( OutputFileBNDetails, Format_701 ) << ChrOut2 + ',' + NodeID( AirToZoneNodeInfo( Count ).CoolZoneInletNodes( Count1 ) ) + ',' + AirToZoneNodeInfo( Count ).AirLoopName;
				} else {
					gio::write( OutputFileBNDetails, Format_701 ) << errstring + ',' + errstring + ',' + AirToZoneNodeInfo( Count ).AirLoopName;
				}
			}
			for ( Count1 = 1; Count1 <= AirToZoneNodeInfo( Count ).NumZonesHeated; ++Count1 ) {
				gio::write( ChrOut, fmtLD ) << Count1;
				if ( AirToZoneNodeInfo( Count ).HeatZoneInletNodes( Count1 ) > 0 ) {
					gio::write( ChrOut2, fmtLD ) << AirToZoneNodeInfo( Count ).HeatZoneInletNodes( Count1 );
				} else {
					ChrOut2 = errstring;
				}
				strip( ChrOut );
				strip( ChrOut2 );
				CtrldZoneNum = AirToZoneNodeInfo( Count ).HeatCtrlZoneNums( Count1 );
				ZoneNum = ZoneEquipConfig( CtrldZoneNum ).ActualZoneNum;
				{ IOFlags flags; flags.ADVANCE( "No" ); gio::write( OutputFileBNDetails, Format_707, flags ) << "  Heated Zone Info," + ChrOut + ',' + Zone( ZoneNum ).Name + ','; }
				if ( ChrOut2 != errstring ) {
					gio::write( OutputFileBNDetails, Format_701 ) << ChrOut2 + ',' + NodeID( AirToZoneNodeInfo( Count ).HeatZoneInletNodes( Count1 ) ) + ',' + AirToZoneNodeInfo( Count ).AirLoopName;
				} else {
					gio::write( OutputFileBNDetails, Format_701 ) << errstring + ',' + errstring + ',' + AirToZoneNodeInfo( Count ).AirLoopName;
				}
			}
			if ( AirToOANodeInfo( Count ).OASysExists ) {
				if ( AirToOANodeInfo( Count ).OASysInletNodeNum > 0 ) {
					gio::write( ChrOut, fmtLD ) << AirToOANodeInfo( Count ).OASysInletNodeNum;
				} else {
					ChrOut = errstring;
				}
				if ( AirToOANodeInfo( Count ).OASysOutletNodeNum > 0 ) {
					gio::write( ChrOut2, fmtLD ) << AirToOANodeInfo( Count ).OASysOutletNodeNum;
				} else {
					ChrOut2 = errstring;
				}
				strip( ChrOut );
				strip( ChrOut2 );
				{ IOFlags flags; flags.ADVANCE( "No" ); gio::write( OutputFileBNDetails, Format_707, flags ) << "  Outdoor Air Connections," + ChrOut + ','; }
				if ( ChrOut != errstring ) {
					{ IOFlags flags; flags.ADVANCE( "No" ); gio::write( OutputFileBNDetails, Format_701, flags ) << NodeID( AirToOANodeInfo( Count ).OASysInletNodeNum ) + ','; }
				} else {
					{ IOFlags flags; flags.ADVANCE( "No" ); gio::write( OutputFileBNDetails, Format_701, flags ) << errstring + ','; }
				}
				if ( ChrOut2 != errstring ) {
					gio::write( OutputFileBNDetails, Format_701 ) << ChrOut2 + ',' + NodeID( AirToOANodeInfo( Count ).OASysOutletNodeNum ) + ',' + AirToZoneNodeInfo( Count ).AirLoopName;
				} else {
					gio::write( OutputFileBNDetails, Format_701 ) << errstring + ',' + errstring + ',' + AirToZoneNodeInfo( Count ).AirLoopName;
				}
			}
			//  Report HVAC Air Loop Splitter to BND file
			if ( PrimaryAirSystem( Count ).Splitter.Exists ) {
				gio::write( ChrOut, fmtLD ) << PrimaryAirSystem( Count ).Splitter.TotalOutletNodes;
				gio::write( OutputFileBNDetails, Format_701 ) << "   AirLoopHVAC Connector,Splitter," + PrimaryAirSystem( Count ).Splitter.Name + ',' + PrimaryAirSystem( Count ).Name + ",Air," + stripped( ChrOut );
				for ( Count1 = 1; Count1 <= PrimaryAirSystem( Count ).Splitter.TotalOutletNodes; ++Count1 ) {
					gio::write( ChrOut, fmtLD ) << Count1;
					if ( PrimaryAirSystem( Count ).Splitter.BranchNumIn <= 0 ) {
						ChrOut2 = errstring;
					}
					if ( PrimaryAirSystem( Count ).Splitter.BranchNumOut( Count1 ) <= 0 ) {
						ChrOut3 = errstring;
					}
					{ IOFlags flags; flags.ADVANCE( "No" ); gio::write( OutputFileBNDetails, Format_701, flags ) << "     AirLoopHVAC Connector Branches," + stripped( ChrOut ) + ",Splitter," + PrimaryAirSystem( Count ).Splitter.Name + ','; }
					if ( ChrOut2 != errstring ) {
						{ IOFlags flags; flags.ADVANCE( "No" ); gio::write( OutputFileBNDetails, Format_701, flags ) << PrimaryAirSystem( Count ).Branch( PrimaryAirSystem( Count ).Splitter.BranchNumIn ).Name + ','; }
					} else {
						{ IOFlags flags; flags.ADVANCE( "No" ); gio::write( OutputFileBNDetails, Format_701, flags ) << ChrOut2 + ','; }
					}
					if ( ChrOut3 != errstring ) {
						gio::write( OutputFileBNDetails, Format_701 ) << PrimaryAirSystem( Count ).Branch( PrimaryAirSystem( Count ).Splitter.BranchNumOut( Count1 ) ).Name + ',' + PrimaryAirSystem( Count ).Name + ",Air";
					} else {
						gio::write( OutputFileBNDetails, Format_701 ) << ChrOut3 + ',' + PrimaryAirSystem( Count ).Name + ",Air";
					}
					gio::write( OutputFileBNDetails, Format_701 ) << "     AirLoopHVAC Connector Nodes,   " + stripped( ChrOut ) + ",Splitter," + PrimaryAirSystem( Count ).Splitter.Name + ',' + PrimaryAirSystem( Count ).Splitter.NodeNameIn + ',' + PrimaryAirSystem( Count ).Splitter.NodeNameOut( Count1 ) + ',' + PrimaryAirSystem( Count ).Name + ",Air";
				}
			}
		}

	}

	//        End of Reporting subroutines for the SimAir Module
	// *****************************************************************************

	//     NOTICE

	//     Copyright © 1996-2014 The Board of Trustees of the University of Illinois
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

} // SystemReports

} // EnergyPlus
