// EnergyPlus::Fans Unit Tests

// Google Test Headers
#include <gtest/gtest.h>

// EnergyPlus Headers
#include <EnergyPlus/AirflowNetworkBalanceManager.hh>
#include <EnergyPlus/DataAirflowNetwork.hh>
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataGlobals.hh>
#include <EnergyPlus/DataLoopNode.hh>
#include <EnergyPlus/DataMoistureBalanceEMPD.hh>
#include <EnergyPlus/DataMoistureBalance.hh>
#include <EnergyPlus/DataRoomAirModel.hh>
#include <EnergyPlus/DataSizing.hh>
#include <EnergyPlus/DataHeatBalance.hh>
#include <EnergyPlus/DataHVACGlobals.hh>
#include <EnergyPlus/DataSurfaces.hh>
#include <EnergyPlus/DataHeatBalSurface.hh>
#include <EnergyPlus/DataZoneControls.hh>
#include <Psychrometrics.hh>
#include <EnergyPlus/RoomAirModelAirflowNetwork.hh>
#include <EnergyPlus/RoomAirModelManager.hh>
#include <EnergyPlus/UtilityRoutines.hh>
#include <EnergyPlus/DataZoneEquipment.hh>
#include <EnergyPlus/DataHeatBalFanSys.hh>
#include <ObjexxFCL/gio.hh>

using namespace EnergyPlus;
using namespace DataAirflowNetwork;
using namespace DataEnvironment;
using namespace DataGlobals;
using namespace EnergyPlus::DataSizing;
using namespace EnergyPlus::DataHeatBalance;
using namespace EnergyPlus::DataHVACGlobals;
using namespace DataRoomAirModel;
using namespace DataMoistureBalanceEMPD;
using namespace DataMoistureBalance;
using namespace DataSurfaces;
using namespace DataHeatBalSurface;
using namespace EnergyPlus::RoomAirModelAirflowNetwork;
using namespace EnergyPlus::RoomAirModelManager;
using namespace EnergyPlus::DataLoopNode;
using namespace EnergyPlus::DataZoneControls;
using namespace EnergyPlus::DataHeatBalFanSys;
using namespace EnergyPlus::Psychrometrics;
using DataZoneEquipment::ZoneEquipConfig;
using DataZoneEquipment::ZoneEquipList;

class RoomAirflowNetworkTest : public testing::Test
{

public:

	RoomAirflowNetworkTest( ) // Setup global state
	{
		CurZoneEqNum = 0;
		CurSysNum = 0;
		CurOASysNum = 0;
		NumOfZones = 1;
		NumOfNodes = 5;
		BeginEnvrnFlag = true;
		int NumOfSurfaces = 2;
		RoomAirflowNetworkZoneInfo.allocate( NumOfZones );
		Zone.allocate( NumOfZones );
		ZoneEquipConfig.allocate( NumOfZones );
		ZoneEquipList.allocate( NumOfZones );
		ZoneIntGain.allocate( NumOfZones );
		NodeID.allocate( NumOfNodes );
		Node.allocate( NumOfNodes );
		Surface.allocate( NumOfSurfaces );
		HConvIn.allocate( NumOfSurfaces );
		TempSurfInTmp.allocate( NumOfSurfaces );
		MoistEMPDNew.allocate( NumOfSurfaces );
		MoistEMPDOld.allocate( NumOfSurfaces );
		RhoVaporSurfIn.allocate( NumOfSurfaces );
		RhoVaporAirIn.allocate( NumOfSurfaces );
		HMassConvInFD.allocate( NumOfSurfaces );
		MAT.allocate( NumOfZones );
		ZoneAirHumRat.allocate( 1 );
		AirflowNetworkLinkageData.allocate( 5 );
		AirflowNetworkNodeSimu.allocate( 6 );
		AirflowNetworkLinkSimu.allocate( 5 );
		RAFN.allocate( NumOfZones );

	}

	~RoomAirflowNetworkTest( ) // Reset global state
	{
		NumOfZones = 0;
		NumOfNodes = 0;
		BeginEnvrnFlag = false;
		RoomAirflowNetworkZoneInfo.clear( );
		ZoneEquipConfig.clear( );
		ZoneEquipList.clear( );
		Zone.clear( );
		ZoneIntGain.clear( );
		NodeID.clear( );
		Node.clear( );
		Surface.clear( );
		HConvIn.clear( );
		TempSurfInTmp.clear( );
		MoistEMPDNew.clear( );
		MoistEMPDOld.clear( );
		RhoVaporSurfIn.clear( );
		RhoVaporAirIn.clear( );
		HMassConvInFD.clear( );
		MAT.clear( );
		ZoneAirHumRat.clear( );
		AirflowNetworkLinkageData.clear( );
		AirflowNetworkNodeSimu.clear( );
		AirflowNetworkLinkSimu.clear( );
		RAFN.clear( );
	}

};

TEST_F( RoomAirflowNetworkTest, RAFNTest )
{

	ShowMessage( "Begin Test: RoomAirflowNetworkTest, RAFNTest" );

	int NumOfAirNodes = 2;
	int ZoneNum = 1;
	int RoomAirNode;
	TimeStepSys = 15.0 / 60.0;
	OutBaroPress = 101325.0;
	ZoneVolCapMultpSens = 1;

	InitializePsychRoutines( );

	RoomAirflowNetworkZoneInfo( ZoneNum ).IsUsed = true;
	RoomAirflowNetworkZoneInfo( ZoneNum ).ActualZoneID = ZoneNum;
	RoomAirflowNetworkZoneInfo( ZoneNum ).NumOfAirNodes = NumOfAirNodes;
	RoomAirflowNetworkZoneInfo( ZoneNum ).Node.allocate( NumOfAirNodes );
	RoomAirflowNetworkZoneInfo( ZoneNum ).ControlAirNodeID = 1;
	RoomAirflowNetworkZoneInfo( ZoneNum ).Node( 1 ).ZoneVolumeFraction = 0.2;
	RoomAirflowNetworkZoneInfo( ZoneNum ).Node( 2 ).ZoneVolumeFraction = 0.8;
	RoomAirflowNetworkZoneInfo( ZoneNum ).Node( 1 ).HVAC.allocate( 1 );
	RoomAirflowNetworkZoneInfo( ZoneNum ).Node( 2 ).HVAC.allocate( 1 );
	RoomAirflowNetworkZoneInfo( ZoneNum ).Node( 1 ).NumHVACs = 1;
	RoomAirflowNetworkZoneInfo( ZoneNum ).Node( 2 ).NumHVACs = 1;
	RoomAirflowNetworkZoneInfo( ZoneNum ).Node( 1 ).HVAC( 1 ).SupplyFraction = 0.4;
	RoomAirflowNetworkZoneInfo( ZoneNum ).Node( 2 ).HVAC( 1 ).SupplyFraction = 0.6;
	RoomAirflowNetworkZoneInfo( ZoneNum ).Node( 1 ).HVAC( 1 ).ReturnFraction = 0.4;
	RoomAirflowNetworkZoneInfo( ZoneNum ).Node( 2 ).HVAC( 1 ).ReturnFraction = 0.6;
	RoomAirflowNetworkZoneInfo( ZoneNum ).Node( 1 ).HVAC( 1 ).Name = "ZoneHVAC";
	RoomAirflowNetworkZoneInfo( ZoneNum ).Node( 2 ).HVAC( 1 ).Name = "ZoneHVAC";
	RoomAirflowNetworkZoneInfo( ZoneNum ).Node( 1 ).HVAC( 1 ).SupplyNodeName = "Supply";
	RoomAirflowNetworkZoneInfo( ZoneNum ).Node( 2 ).HVAC( 1 ).SupplyNodeName = "Supply";
	RoomAirflowNetworkZoneInfo( ZoneNum ).Node( 1 ).HVAC( 1 ).ReturnNodeName = "Return";
	RoomAirflowNetworkZoneInfo( ZoneNum ).Node( 2 ).HVAC( 1 ).ReturnNodeName = "Return";
	RoomAirflowNetworkZoneInfo( ZoneNum ).Node( 1 ).HVAC( 1 ).Name = "ZoneHVAC";
	RoomAirflowNetworkZoneInfo( ZoneNum ).Node( 2 ).HVAC( 1 ).Name = "ZoneHVAC";
	RoomAirflowNetworkZoneInfo( ZoneNum ).Node( 1 ).IntGainsDeviceIndices.allocate( 1 );
	RoomAirflowNetworkZoneInfo( ZoneNum ).Node( 2 ).IntGainsDeviceIndices.allocate( 1 );
	RoomAirflowNetworkZoneInfo( ZoneNum ).Node( 1 ).IntGainsDeviceIndices( 1 ) = 1;
	RoomAirflowNetworkZoneInfo( ZoneNum ).Node( 2 ).IntGainsDeviceIndices( 1 ) = 1;
	RoomAirflowNetworkZoneInfo( ZoneNum ).Node( 1 ).IntGainsFractions.allocate( 1 );
	RoomAirflowNetworkZoneInfo( ZoneNum ).Node( 2 ).IntGainsFractions.allocate( 1 );
	RoomAirflowNetworkZoneInfo( ZoneNum ).Node( 1 ).IntGainsFractions( 1 ) = 0.4;
	RoomAirflowNetworkZoneInfo( ZoneNum ).Node( 2 ).IntGainsFractions( 1 ) = 0.6;
	RoomAirflowNetworkZoneInfo( ZoneNum ).Node( 1 ).HasIntGainsAssigned = true;
	RoomAirflowNetworkZoneInfo( ZoneNum ).Node( 2 ).HasIntGainsAssigned = true;
	RoomAirflowNetworkZoneInfo( ZoneNum ).Node( 1 ).HasSurfacesAssigned = true;
	RoomAirflowNetworkZoneInfo( ZoneNum ).Node( 2 ).HasSurfacesAssigned = true;
	RoomAirflowNetworkZoneInfo( ZoneNum ).Node( 1 ).HasHVACAssigned = true;
	RoomAirflowNetworkZoneInfo( ZoneNum ).Node( 2 ).HasHVACAssigned = true;
	RoomAirflowNetworkZoneInfo( ZoneNum ).Node( 1 ).SurfMask.allocate( 2 );
	RoomAirflowNetworkZoneInfo( ZoneNum ).Node( 2 ).SurfMask.allocate( 2 );
	RoomAirflowNetworkZoneInfo( ZoneNum ).Node( 1 ).SurfMask( 1 ) = true;
	RoomAirflowNetworkZoneInfo( ZoneNum ).Node( 1 ).SurfMask( 2 ) = false;
	RoomAirflowNetworkZoneInfo( ZoneNum ).Node( 2 ).SurfMask( 1 ) = false;
	RoomAirflowNetworkZoneInfo( ZoneNum ).Node( 2 ).SurfMask( 2 ) = true;
	RoomAirflowNetworkZoneInfo( ZoneNum ).Node( 1 ).NumOfAirflowLinks = 3;
	RoomAirflowNetworkZoneInfo( ZoneNum ).Node( 1 ).Link.allocate( 3 );
	RoomAirflowNetworkZoneInfo( ZoneNum ).Node( 1 ).Link( 1 ).AirflowNetworkLinkSimuID = 1;
	RoomAirflowNetworkZoneInfo( ZoneNum ).Node( 1 ).Link( 2 ).AirflowNetworkLinkSimuID = 2;
	RoomAirflowNetworkZoneInfo( ZoneNum ).Node( 1 ).Link( 3 ).AirflowNetworkLinkSimuID = 3;
	RoomAirflowNetworkZoneInfo( ZoneNum ).Node( 1 ).AirflowNetworkNodeID = 1;
	RoomAirflowNetworkZoneInfo( ZoneNum ).Node( 2 ).NumOfAirflowLinks = 3;
	RoomAirflowNetworkZoneInfo( ZoneNum ).Node( 2 ).Link.allocate( 3 );
	RoomAirflowNetworkZoneInfo( ZoneNum ).Node( 2 ).Link( 1 ).AirflowNetworkLinkSimuID = 3;
	RoomAirflowNetworkZoneInfo( ZoneNum ).Node( 2 ).Link( 2 ).AirflowNetworkLinkSimuID = 4;
	RoomAirflowNetworkZoneInfo( ZoneNum ).Node( 2 ).Link( 3 ).AirflowNetworkLinkSimuID = 5;
	RoomAirflowNetworkZoneInfo( ZoneNum ).Node( 2 ).AirflowNetworkNodeID = 2;

	AirflowNetworkLinkageData( 1 ).NodeNums( 1 ) = 1;
	AirflowNetworkLinkageData( 2 ).NodeNums( 1 ) = 1;
	AirflowNetworkLinkageData( 3 ).NodeNums( 1 ) = 1;
	AirflowNetworkLinkageData( 1 ).NodeNums( 2 ) = 3;
	AirflowNetworkLinkageData( 2 ).NodeNums( 2 ) = 4;
	AirflowNetworkLinkageData( 3 ).NodeNums( 2 ) = 2;
	AirflowNetworkLinkageData( 4 ).NodeNums( 1 ) = 2;
	AirflowNetworkLinkageData( 5 ).NodeNums( 1 ) = 2;
	AirflowNetworkLinkageData( 4 ).NodeNums( 2 ) = 5;
	AirflowNetworkLinkageData( 5 ).NodeNums( 2 ) = 6;
	AirflowNetworkNodeSimu( 1 ).TZ = 25.0;
	AirflowNetworkNodeSimu( 1 ).WZ = 0.001;
	AirflowNetworkNodeSimu( 2 ).TZ = 20.0;
	AirflowNetworkNodeSimu( 2 ).WZ = 0.002;
	AirflowNetworkNodeSimu( 3 ).TZ = 30.0;
	AirflowNetworkNodeSimu( 3 ).WZ = 0.001;
	AirflowNetworkNodeSimu( 4 ).TZ = 22.0;
	AirflowNetworkNodeSimu( 4 ).WZ = 0.001;
	AirflowNetworkNodeSimu( 5 ).TZ = 27.0;
	AirflowNetworkNodeSimu( 5 ).WZ = 0.0015;
	AirflowNetworkNodeSimu( 6 ).TZ = 20.0;
	AirflowNetworkNodeSimu( 6 ).WZ = 0.002;
	AirflowNetworkLinkSimu( 1 ).FLOW = 0.0;
	AirflowNetworkLinkSimu( 1 ).FLOW2 = 0.01;
	AirflowNetworkLinkSimu( 2 ).FLOW = 0.0;
	AirflowNetworkLinkSimu( 2 ).FLOW2 = 0.02;
	AirflowNetworkLinkSimu( 3 ).FLOW = 0.01;
	AirflowNetworkLinkSimu( 3 ).FLOW2 = 0.0;
	AirflowNetworkLinkSimu( 4 ).FLOW = 0.0;
	AirflowNetworkLinkSimu( 4 ).FLOW2 = 0.01;
	AirflowNetworkLinkSimu( 5 ).FLOW = 0.01;
	AirflowNetworkLinkSimu( 5 ).FLOW2 = 0.0;

	ZoneEquipList( ZoneNum ).NumOfEquipTypes = 1;
	ZoneEquipList( ZoneNum ).EquipName.allocate( 1 );
	ZoneEquipList( ZoneNum ).EquipName( 1 ) = "ZoneHVAC";

	ZoneEquipConfig( ZoneNum ).NumInletNodes = 1;
	ZoneEquipConfig( ZoneNum ).ActualZoneNum = ZoneNum;
	ZoneEquipConfig( ZoneNum ).InletNode.allocate( 1 );
	ZoneEquipConfig( ZoneNum ).InletNode( 1 ) = 1;
	NodeID.allocate( NumOfNodes );
	Node.allocate( NumOfNodes );
	ZoneEquipConfig( ZoneNum ).ReturnAirNode = 2;

	Zone( ZoneNum ).Volume = 100;
	Zone( ZoneNum ).IsControlled = true;
	Zone( ZoneNum ).SurfaceFirst = 1;
	Zone( ZoneNum ).SurfaceLast = 2;

	ZoneIntGain( ZoneNum ).NumberOfDevices = 1;
	ZoneIntGain( ZoneNum ).Device.allocate( ZoneIntGain( 1 ).NumberOfDevices );
	ZoneIntGain( ZoneNum ).Device( 1 ).CompObjectName = "PEOPLE";
	ZoneIntGain( ZoneNum ).Device( 1 ).CompTypeOfNum = IntGainTypeOf_People;
	ZoneIntGain( ZoneNum ).Device( 1 ).ConvectGainRate = 300.0;
	ZoneIntGain( ZoneNum ).Device( 1 ).LatentGainRate = 200.0;

	Surface( 1 ).HeatTransSurf = true;
	Surface( 2 ).HeatTransSurf = true;
	Surface( 1 ).Area = 1.0;
	Surface( 2 ).Area = 2.0;

	Surface( 1 ).HeatTransferAlgorithm = HeatTransferModel_EMPD;
	Surface( 2 ).HeatTransferAlgorithm = HeatTransferModel_EMPD;
	MoistEMPDNew( 1 ) = 0.0011;
	MoistEMPDNew( 2 ) = 0.0012;

	NodeID( 1 ) = "Supply";
	NodeID( 2 ) = "Return";

	ZoneAirHumRat( 1 ) = 0.001;

	Node( 1 ).Temp = 20.0;
	Node( 1 ).HumRat = 0.001;
	Node( 1 ).MassFlowRate = 0.01;

	MAT( 1 ) = 20.0;
	HConvIn( 1 ) = 1.0;
	HConvIn( 2 ) = 1.0;
	TempSurfInTmp( 1 ) = 25.0 ;
	TempSurfInTmp( 2 ) = 30.0;
	RhoVaporAirIn( 1 ) = PsyRhovFnTdbWPb( MAT( ZoneNum ), ZoneAirHumRat( ZoneNum ), OutBaroPress );
	RhoVaporAirIn( 2 ) = PsyRhovFnTdbWPb( MAT( ZoneNum ), ZoneAirHumRat( ZoneNum ), OutBaroPress );
	HMassConvInFD( 1 ) = HConvIn( 1 ) / ( ( PsyRhoAirFnPbTdbW( OutBaroPress, MAT( ZoneNum ), ZoneAirHumRat( ZoneNum ) ) + RhoVaporAirIn( 1 ) ) * PsyCpAirFnWTdb( ZoneAirHumRat( ZoneNum ), MAT( ZoneNum ) ) );
	HMassConvInFD( 2 ) = HConvIn( 2 ) / ( ( PsyRhoAirFnPbTdbW( OutBaroPress, MAT( ZoneNum ), ZoneAirHumRat( ZoneNum ) ) + RhoVaporAirIn( 2 ) ) * PsyCpAirFnWTdb( ZoneAirHumRat( ZoneNum ), MAT( ZoneNum ) ) );

	RoomAirNode = 1;
	auto & thisRAFN( RAFN( ZoneNum ) );
	thisRAFN.ZoneNum = ZoneNum;

	thisRAFN.InitRoomAirModelAirflowNetwork( RoomAirNode );

	EXPECT_NEAR( 120.0, RoomAirflowNetworkZoneInfo( ZoneNum ).Node( RoomAirNode ).SumIntSensibleGain, 0.00001 );
	EXPECT_NEAR( 80.0, RoomAirflowNetworkZoneInfo( ZoneNum ).Node( RoomAirNode ).SumIntLatentGain, 0.00001 );
	EXPECT_NEAR( 1.0, RoomAirflowNetworkZoneInfo( ZoneNum ).Node( RoomAirNode ).SumHA, 0.00001 );
	EXPECT_NEAR( 25.0, RoomAirflowNetworkZoneInfo( ZoneNum ).Node( RoomAirNode ).SumHATsurf, 0.00001 );
	EXPECT_NEAR( 0.0, RoomAirflowNetworkZoneInfo( ZoneNum ).Node( RoomAirNode ).SumHATref, 0.00001 );
	EXPECT_NEAR( 4.0268, RoomAirflowNetworkZoneInfo( ZoneNum ).Node( RoomAirNode ).SumSysMCp, 0.0001 );
	EXPECT_NEAR( 80.536, RoomAirflowNetworkZoneInfo( ZoneNum ).Node( RoomAirNode ).SumSysMCpT, 0.001 );
	EXPECT_NEAR( 0.004, RoomAirflowNetworkZoneInfo( ZoneNum ).Node( RoomAirNode ).SumSysM, 0.00001 );
	EXPECT_NEAR( 4.0e-6, RoomAirflowNetworkZoneInfo( ZoneNum ).Node( RoomAirNode ).SumSysMW, 0.00001 );
	EXPECT_NEAR( 30.200968, RoomAirflowNetworkZoneInfo( ZoneNum ).Node( RoomAirNode ).SumLinkMCp, 0.0001 );
	EXPECT_NEAR( 744.95722, RoomAirflowNetworkZoneInfo( ZoneNum ).Node( RoomAirNode ).SumLinkMCpT, 0.001 );
	EXPECT_NEAR( 0.03, RoomAirflowNetworkZoneInfo( ZoneNum ).Node( RoomAirNode ).SumLinkM, 0.00001 );
	EXPECT_NEAR( 3.0e-5, RoomAirflowNetworkZoneInfo( ZoneNum ).Node( RoomAirNode ).SumLinkMW, 0.00001 );
	EXPECT_NEAR( -8.431365e-8, RoomAirflowNetworkZoneInfo( ZoneNum ).Node( RoomAirNode ).SumHmAW, 0.0000001 );
	EXPECT_NEAR( 0.0009756833, RoomAirflowNetworkZoneInfo( ZoneNum ).Node( RoomAirNode ).SumHmARa, 0.0000001 );
	EXPECT_NEAR( 9.0784549e-7, RoomAirflowNetworkZoneInfo( ZoneNum ).Node( RoomAirNode ).SumHmARaW, 0.0000001 );

	thisRAFN.CalcRoomAirModelAirflowNetwork( RoomAirNode );

	EXPECT_NEAR( 24.907085, RoomAirflowNetworkZoneInfo( ZoneNum ).Node( RoomAirNode ).AirTemp, 0.00001 );
	EXPECT_NEAR( 0.00189601, RoomAirflowNetworkZoneInfo( ZoneNum ).Node( RoomAirNode ).HumRat, 0.00001 );
	EXPECT_NEAR( 9.770445, RoomAirflowNetworkZoneInfo( ZoneNum ).Node( RoomAirNode ).RelHumidity, 0.00001 );

	RoomAirNode = 2;
	thisRAFN.InitRoomAirModelAirflowNetwork( RoomAirNode );

	EXPECT_NEAR( 180.0, RoomAirflowNetworkZoneInfo( ZoneNum ).Node( RoomAirNode ).SumIntSensibleGain, 0.00001 );
	EXPECT_NEAR( 120.0, RoomAirflowNetworkZoneInfo( ZoneNum ).Node( RoomAirNode ).SumIntLatentGain, 0.00001 );
	EXPECT_NEAR( 2.0, RoomAirflowNetworkZoneInfo( ZoneNum ).Node( RoomAirNode ).SumHA, 0.00001 );
	EXPECT_NEAR( 60.0, RoomAirflowNetworkZoneInfo( ZoneNum ).Node( RoomAirNode ).SumHATsurf, 0.00001 );
	EXPECT_NEAR( 0.0, RoomAirflowNetworkZoneInfo( ZoneNum ).Node( RoomAirNode ).SumHATref, 0.00001 );
	EXPECT_NEAR( 6.04019, RoomAirflowNetworkZoneInfo( ZoneNum ).Node( RoomAirNode ).SumSysMCp, 0.0001 );
	EXPECT_NEAR( 120.803874, RoomAirflowNetworkZoneInfo( ZoneNum ).Node( RoomAirNode ).SumSysMCpT, 0.00001 );
	EXPECT_NEAR( 0.006, RoomAirflowNetworkZoneInfo( ZoneNum ).Node( RoomAirNode ).SumSysM, 0.00001 );
	EXPECT_NEAR( 6.0e-6, RoomAirflowNetworkZoneInfo( ZoneNum ).Node( RoomAirNode ).SumSysMW, 0.00001 );
	EXPECT_NEAR( 20.14327, RoomAirflowNetworkZoneInfo( ZoneNum ).Node( RoomAirNode ).SumLinkMCp, 0.0001 );
	EXPECT_NEAR( 523.73441, RoomAirflowNetworkZoneInfo( ZoneNum ).Node( RoomAirNode ).SumLinkMCpT, 0.001 );
	EXPECT_NEAR( 0.02, RoomAirflowNetworkZoneInfo( ZoneNum ).Node( RoomAirNode ).SumLinkM, 0.00001 );
	EXPECT_NEAR( 2.5e-5, RoomAirflowNetworkZoneInfo( ZoneNum ).Node( RoomAirNode ).SumLinkMW, 0.00001 );
	EXPECT_NEAR( -3.5644894e-9, RoomAirflowNetworkZoneInfo( ZoneNum ).Node( RoomAirNode ).SumHmAW, 0.0000001 );
	EXPECT_NEAR( 0.0019191284, RoomAirflowNetworkZoneInfo( ZoneNum ).Node( RoomAirNode ).SumHmARa, 0.0000001 );
	EXPECT_NEAR( 1.98975381e-6, RoomAirflowNetworkZoneInfo( ZoneNum ).Node( RoomAirNode ).SumHmARaW, 0.0000001 );

	thisRAFN.CalcRoomAirModelAirflowNetwork( RoomAirNode );

	EXPECT_NEAR( 24.057841, RoomAirflowNetworkZoneInfo( ZoneNum ).Node( RoomAirNode ).AirTemp, 0.00001 );
	EXPECT_NEAR( 0.0028697086, RoomAirflowNetworkZoneInfo( ZoneNum ).Node( RoomAirNode ).HumRat, 0.00001 );
	EXPECT_NEAR( 15.53486185, RoomAirflowNetworkZoneInfo( ZoneNum ).Node( RoomAirNode ).RelHumidity, 0.00001 );

	thisRAFN.UpdateRoomAirModelAirflowNetwork( );

	EXPECT_NEAR( 24.397538, Node( 2 ).Temp, 0.00001 );
	EXPECT_NEAR( 0.0024802305, Node( 2 ).HumRat, 0.000001 );

	cached_Twb.deallocate( );
	cached_Psat.deallocate( );

}
