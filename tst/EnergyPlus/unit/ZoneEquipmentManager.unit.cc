// EnergyPlus::ZoneEquipmentManager Unit Tests

// Google Test Headers
#include <gtest/gtest.h>

// EnergyPlus Headers
#include <EnergyPlus/DataZoneControls.hh>
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataGlobals.hh>
#include <EnergyPlus/DataHeatBalance.hh>
#include <EnergyPlus/DataHeatBalFanSys.hh>
#include <EnergyPlus/DataHVACGlobals.hh>
#include <EnergyPlus/DataSizing.hh>
#include <EnergyPlus/DataSurfaces.hh>
#include <EnergyPlus/DataPrecisionGlobals.hh>
#include <EnergyPlus/DataZoneEquipment.hh>
#include <EnergyPlus/DataZoneEnergyDemands.hh>
#include <EnergyPlus/ZoneEquipmentManager.hh>
#include <EnergyPlus/DataLoopNode.hh>
#include <EnergyPlus/DataContaminantBalance.hh>
#include <EnergyPlus/Psychrometrics.hh>
#include <EnergyPlus/ScheduleManager.hh>
#include <ObjexxFCL/gio.hh>

using namespace EnergyPlus;
using namespace ObjexxFCL;
using namespace EnergyPlus::DataEnvironment;
using namespace EnergyPlus::DataGlobals;
using namespace EnergyPlus::DataHeatBalance;
using namespace EnergyPlus::DataSizing;
using namespace EnergyPlus::DataZoneEquipment;
using namespace EnergyPlus::DataZoneEnergyDemands;
using namespace EnergyPlus::DataHeatBalFanSys;
using namespace EnergyPlus::DataHVACGlobals;
using namespace EnergyPlus::DataLoopNode;
using namespace EnergyPlus::DataSurfaces;
using namespace EnergyPlus::DataContaminantBalance;
using namespace EnergyPlus::Psychrometrics;
using namespace EnergyPlus::ScheduleManager;
using namespace EnergyPlus::ZoneEquipmentManager;
using DataZoneControls::TempControlledZone;
using DataZoneControls::NumTempControlledZones;
using DataHVACGlobals::SingleCoolingSetPoint;

TEST( ZoneEquipmentManager, SizeZoneEquipmentTest )
{
	ShowMessage( "Begin Test: ZoneEquipmentManager, SizeZoneEquipmentTest" );
	// checks if the CalcZoneSizing().CoolZoneHumRat variables is allocated
	// to zero value when the zone design cooling load is zero.

	// local variables
	int ZoneAirNode;
	int ZoneSupplyNode;
	int ZoneReturnNode;
	int ZoneExhaustNode;

	// local parameters
	int const CtrlZoneNum = 1;
	int const ZoneSizNum = 1;
	int const SurfNum = 1;

	int write_stat;
	// Open the Initialization Output File (lifted from SimulationManager.cc)
	OutputFileInits = GetNewUnitNumber();
	{ IOFlags flags; flags.ACTION( "write" ); flags.STATUS( "UNKNOWN" ); gio::open( OutputFileInits, "eplusout.eio", flags ); write_stat = flags.ios(); }

	InitializePsychRoutines();

	// setup global initialization
	DataSizing::CurZoneEqNum = 1;
	DataSizing::CurOverallSimDay = 2;  // cooling design day only
	DataSizing::NumZoneSizingInput = 1;
	DataZoneEquipment::NumOfZones = 1;
	DataEnvironment::TotDesDays = 2;
	DataEnvironment::TotRunDesPersDays = 0;
	DataGlobals::NumOfTimeStepInHour = 1;
	DataSurfaces::TotSurfaces = 1;
	DataZoneControls::NumTempControlledZones = 1;
	DataGlobals::AnyEnergyManagementSystemInModel = false;
	DataHVACGlobals::NumPrimaryAirSys = 0;

	ZoneAirNode = 1;
	ZoneSupplyNode = 2;
	ZoneReturnNode = 3;
	ZoneExhaustNode = 4;

	DataZoneEquipment::ZoneEquipConfig.allocate( NumOfZones );
	DataZoneEquipment::ZoneEquipConfig( NumOfZones ).InletNode.allocate( 1 );
	DataZoneEquipment::ZoneEquipConfig( NumOfZones ).ExhaustNode.allocate( 1 );
	DataSizing::ZoneSizingInput.allocate( NumOfZones );
	DataSizing::ZoneSizing.allocate( 2, NumOfZones );
	DataSizing::CalcZoneSizing.allocate( 2, NumOfZones );
	DataSizing::FinalZoneSizing.allocate( NumOfZones );
	DataSizing::CalcFinalZoneSizing.allocate( NumOfZones );
	DataZoneEnergyDemands::DeadBandOrSetback.allocate( NumOfZones );
	DataZoneEnergyDemands::CurDeadBandOrSetback.allocate( NumOfZones );	
	DataZoneEnergyDemands::ZoneSysEnergyDemand.allocate( NumOfZones );
	DataZoneEnergyDemands::ZoneSysMoistureDemand.allocate( NumOfZones );	
	DataZoneControls::TempControlledZone.allocate( NumTempControlledZones );
	DataHeatBalFanSys::NonAirSystemResponse.allocate( NumOfZones );
	DataHeatBalFanSys::SysDepZoneLoads.allocate( NumOfZones );
	DataHeatBalFanSys::TempZoneThermostatSetPoint.allocate( NumOfZones );
	DataHeatBalFanSys::ZoneMassBalanceFlag.allocate( NumOfZones );
	DataHeatBalFanSys::ZoneInfiltrationFlag.allocate( NumOfZones );
	DataHeatBalFanSys::ZoneLatentGain.allocate( NumOfZones );
	DataHeatBalFanSys::TempControlType.allocate( NumOfZones );
	DataHeatBalance::Zone.allocate( NumOfZones );
	DataHeatBalance::MassConservation.allocate( NumOfZones );
	DataHeatBalance::ZoneIntGain.allocate( NumOfZones );
	DataHeatBalance::RefrigCaseCredit.allocate( NumOfZones );
	DataSurfaces::SurfaceWindow.allocate( TotSurfaces );
	DataLoopNode::Node.allocate( 4 );

	Zone( CtrlZoneNum ).Name = "VirtualZone";
	Zone( CtrlZoneNum ).FloorArea = 35.0;
	Zone( CtrlZoneNum ).Multiplier = 1;
	Zone( CtrlZoneNum ).ListMultiplier = 1;
	Zone( CtrlZoneNum ).NumSurfaces = 1;
	Zone( CtrlZoneNum ).SurfaceFirst = 1;
	Zone( CtrlZoneNum ).SurfaceLast = 1;

	Zone( CtrlZoneNum ).NoHeatToReturnAir = true;
	SurfaceWindow( SurfNum ).AirflowThisTS = 0.0;
	SurfaceWindow( SurfNum ).AirflowDestination = AirFlowWindow_Destination_OutdoorAir;
	ZoneSizingInput( ZoneSizNum ).ZoneName = "VirtualZone";
	ZoneSizingInput( ZoneSizNum ).ZnCoolDgnSAMethod = DataSizing::SupplyAirTemperature;
	ZoneSizingInput( ZoneSizNum ).ZnHeatDgnSAMethod = DataSizing::SupplyAirTemperature;
	ZoneSizingInput( ZoneSizNum ).CoolDesTemp = 13.0;
	ZoneSizingInput( ZoneSizNum ).HeatDesTemp = 29.0;
	ZoneSizingInput( ZoneSizNum ).CoolDesTempDiff = 0.0;
	ZoneSizingInput( ZoneSizNum ).HeatDesTempDiff = 0.0;
	ZoneSizingInput( ZoneSizNum ).CoolDesHumRat = 0.0075;
	ZoneSizingInput( ZoneSizNum ).HeatDesHumRat = 0.0040;
	ZoneSizingInput( ZoneSizNum ).OADesMethod = 0;
	ZoneSizingInput( ZoneSizNum ).DesOAFlowPPer = 0.0;
	ZoneSizingInput( ZoneSizNum ).DesOAFlowPerArea = 0.0;
	ZoneSizingInput( ZoneSizNum ).DesOAFlow = 0.0;
	ZoneSizingInput( ZoneSizNum ).CoolAirDesMethod = DataSizing::FromDDCalc;
	ZoneSizingInput( ZoneSizNum ).HeatAirDesMethod = DataSizing::FromDDCalc;
	ZoneSizingInput( ZoneSizNum ).DesCoolAirFlow = 0.0;
	ZoneSizingInput( ZoneSizNum ).DesCoolMinAirFlowPerArea = 0.0;
	ZoneSizingInput( ZoneSizNum ).DesCoolMinAirFlow = 0.0;
	ZoneSizingInput( ZoneSizNum ).DesCoolMinAirFlowFrac = 0.0;
	ZoneSizingInput( ZoneSizNum ).DesHeatAirFlow = 0.0;
	ZoneSizingInput( ZoneSizNum ).DesHeatMaxAirFlowPerArea = 0.0;
	ZoneSizingInput( ZoneSizNum ).DesHeatMaxAirFlow = 0.0;
	ZoneSizingInput( ZoneSizNum ).DesHeatMaxAirFlowFrac = 0.0;
	ZoneSizingInput( ZoneSizNum ).HeatSizingFactor = 1.0;
	ZoneSizingInput( ZoneSizNum ).CoolSizingFactor = 1.0;

	ZoneEquipConfig( CtrlZoneNum ).ZoneName = "VirtualZone";
	ZoneEquipConfig( CtrlZoneNum ).IsControlled = true;
	ZoneEquipConfig( CtrlZoneNum ).ActualZoneNum = CtrlZoneNum;
	ZoneEquipConfig( CtrlZoneNum ).ZoneNode = ZoneAirNode;
	ZoneEquipConfig( CtrlZoneNum ).ReturnAirNode = ZoneReturnNode;
	ZoneEquipConfig( CtrlZoneNum ).NumInletNodes = 1;
	ZoneEquipConfig( CtrlZoneNum ).InletNode( 1 ) = ZoneSupplyNode;
	ZoneEquipConfig( CtrlZoneNum ).NumExhaustNodes = 1;
	ZoneEquipConfig( CtrlZoneNum ).ExhaustNode( 1 ) = ZoneExhaustNode;
	ZoneEquipConfig( CtrlZoneNum ).AirLoopNum = 0;
	ZoneEquipConfig( CtrlZoneNum ).ZoneExhBalanced = 0.0;
	ZoneEquipConfig( CtrlZoneNum ).ZoneExh = 0.0;
	ZoneEquipConfig( CtrlZoneNum ).PlenumMassFlow = 0.0;
	
	DataHeatBalance::ZoneIntGain( CtrlZoneNum ).NumberOfDevices = 0;
	DataHeatBalance::MassConservation( CtrlZoneNum ).InfiltrationPtr = 0; // no infiltration objects defined
	DataHeatBalance::ZoneAirMassFlow.InfiltrationTreatment = DataHeatBalance::AddInfiltrationFlow;
	DataHeatBalance::ZoneAirMassFlow.EnforceZoneMassBalance = false;
	DataHeatBalFanSys::ZoneMassBalanceFlag( CtrlZoneNum ) = false;
	DataHeatBalFanSys::ZoneLatentGain( CtrlZoneNum ) = 0.0;
	DataHeatBalFanSys::TempControlType( CtrlZoneNum ) = DataHVACGlobals::SingleCoolingSetPoint;
	DataHeatBalance::RefrigCaseCredit( CtrlZoneNum ).LatCaseCreditToHVAC = 0.0;
	DataContaminantBalance::Contaminant.CO2Simulation = false;
	DataContaminantBalance::Contaminant.GenericContamSimulation = false;

	CalcZoneSizing( CurOverallSimDay, CtrlZoneNum ).ZnCoolDgnSAMethod = DataSizing::SupplyAirTemperature;
	CalcZoneSizing( CurOverallSimDay, CtrlZoneNum ).ActualZoneNum = CtrlZoneNum;
	CalcZoneSizing( CurOverallSimDay, CtrlZoneNum ).SupplyAirNode = ZoneSupplyNode;
	CalcZoneSizing( CurOverallSimDay, CtrlZoneNum ).SupplyAirAdjustFactor = 1.0;
	CalcZoneSizing( CurOverallSimDay, CtrlZoneNum ).CoolDesTemp = 25.0;
	CalcZoneSizing( CurOverallSimDay, CtrlZoneNum ).CoolDesHumRat = 0.0075;
	CalcZoneSizing( CurOverallSimDay, CtrlZoneNum ).HeatLoad = 0.0;
	TempZoneThermostatSetPoint( CtrlZoneNum ) = 24.0;
	DataZoneControls::TempControlledZone( CtrlZoneNum ).ZoneName = "VirtualZone";
	DataZoneControls::GetZoneAirStatsInputFlag = false;
	DataHeatBalance::TotPeople = 1;
	People.allocate( TotPeople );
	People( 1 ).ZonePtr = 1;
	People( 1 ).NumberOfPeople = 0.0;
	People( 1 ).NumberOfPeoplePtr = DataGlobals::ScheduleAlwaysOn;
	ZoneSysEnergyDemand( CtrlZoneNum ).TotalOutputRequired = 0.0;
	ZoneSysEnergyDemand( CtrlZoneNum ).OutputRequiredToHeatingSP = 0.0;
	ZoneSysEnergyDemand( CtrlZoneNum ).OutputRequiredToCoolingSP = 0.0;
	ZoneSysMoistureDemand( CtrlZoneNum ).TotalOutputRequired = 0.0;
	ZoneSysMoistureDemand( CtrlZoneNum ).OutputRequiredToHumidifyingSP = 0.0;
	ZoneSysMoistureDemand( CtrlZoneNum ).OutputRequiredToDehumidifyingSP = 0.0;
	Node( ZoneAirNode ).Temp = 24.0;
	Node( ZoneAirNode ).HumRat = 0.005;
	Node( ZoneAirNode ).Enthalpy = 32000.0;
	Node( ZoneReturnNode ).Temp = 25.0;

	ZoneEquipmentManager::SizeZoneEquipment();
	
	// tests humidity ratio variable is initialized to zero when zone load is zero
	EXPECT_EQ( 0.0, CalcZoneSizing( CurOverallSimDay, CtrlZoneNum ).CoolZoneHumRat );
	
	// delete variables
	CalcZoneSizing.deallocate();
	CalcFinalZoneSizing.deallocate();
	CurDeadBandOrSetback.deallocate();
	DeadBandOrSetback.deallocate();
	FinalZoneSizing.deallocate();
	MassConservation.deallocate();
	Node.deallocate();
	NonAirSystemResponse.deallocate();
	People.deallocate();
	RefrigCaseCredit.deallocate();
	SurfaceWindow.deallocate();
	SysDepZoneLoads.deallocate();
	TempZoneThermostatSetPoint.deallocate();
	TempControlledZone.deallocate();
	TempControlType.deallocate();
	Zone.deallocate();
	ZoneEquipConfig( 1 ).InletNode.deallocate();
	ZoneEquipConfig( 1 ).ExhaustNode.deallocate();
	ZoneEquipConfig.deallocate();
	ZoneSizingInput.deallocate();
	ZoneSizing.deallocate();
	ZoneMassBalanceFlag.deallocate();
	ZoneInfiltrationFlag.deallocate();
	ZoneLatentGain.deallocate();
	ZoneIntGain.deallocate();
	ZoneSysEnergyDemand.deallocate();
	ZoneSysMoistureDemand.deallocate();	

	// Close and delete eio output file
	{ IOFlags flags; flags.DISPOSE( "DELETE" ); gio::close( OutputFileInits, flags ); }
}
