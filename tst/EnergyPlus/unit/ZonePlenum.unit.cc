// EnergyPlus::ZonePlenum Unit Tests

// Google Test Headers
#include <gtest/gtest.h>

// EnergyPlus Headers
#include <EnergyPlus/UtilityRoutines.hh>
#include <EnergyPlus/ZonePlenum.hh>
#include <EnergyPlus/DataLoopNode.hh>
#include <EnergyPlus/Psychrometrics.hh>
#include <DataContaminantBalance.hh>

using namespace EnergyPlus;
using namespace ObjexxFCL;
using namespace DataGlobals;
using namespace EnergyPlus::ZonePlenum;
using namespace EnergyPlus::DataLoopNode;
using namespace EnergyPlus::Psychrometrics;
using DataContaminantBalance::Contaminant;


TEST( ZonePlenum, InitAirZoneReturnPlenumTest )
{
	ShowMessage( "Begin Test: ZonePlenum, InitAirZoneReturnPlenumTest" );

	InitializePsychRoutines();
	BeginEnvrnFlag = false;
	Contaminant.CO2Simulation = true;
	Contaminant.GenericContamSimulation = true;

	NumZoneReturnPlenums = 1;
	ZoneRetPlenCond.allocate( NumZoneReturnPlenums );
	int ZonePlenumNum = 1;
	ZoneRetPlenCond( ZonePlenumNum ).NumInletNodes = 0; // To avoid initializing extra zone equip config and ADU data
	ZoneRetPlenCond( ZonePlenumNum ).NumInducedNodes = 2;
	ZoneRetPlenCond( ZonePlenumNum ).InletNode.allocate(1); // Needed for the Update routine
	ZoneRetPlenCond( ZonePlenumNum ).InducedNode.allocate( ZoneRetPlenCond( ZonePlenumNum ).NumInducedNodes );
	ZoneRetPlenCond( ZonePlenumNum ).InducedMassFlowRate.allocate( ZoneRetPlenCond( ZonePlenumNum ).NumInducedNodes );
	ZoneRetPlenCond( ZonePlenumNum ).InducedMassFlowRateMaxAvail.allocate( ZoneRetPlenCond( ZonePlenumNum ).NumInducedNodes );
	ZoneRetPlenCond( ZonePlenumNum ).InducedMassFlowRateMinAvail.allocate( ZoneRetPlenCond( ZonePlenumNum ).NumInducedNodes );
	ZoneRetPlenCond( ZonePlenumNum ).InducedTemp.allocate( ZoneRetPlenCond( ZonePlenumNum ).NumInducedNodes );
	ZoneRetPlenCond( ZonePlenumNum ).InducedHumRat.allocate( ZoneRetPlenCond( ZonePlenumNum ).NumInducedNodes );
	ZoneRetPlenCond( ZonePlenumNum ).InducedEnthalpy.allocate( ZoneRetPlenCond( ZonePlenumNum ).NumInducedNodes );
	ZoneRetPlenCond( ZonePlenumNum ).InducedPressure.allocate( ZoneRetPlenCond( ZonePlenumNum ).NumInducedNodes );
	ZoneRetPlenCond( ZonePlenumNum ).InducedCO2.allocate( ZoneRetPlenCond( ZonePlenumNum ).NumInducedNodes );
	ZoneRetPlenCond( ZonePlenumNum ).InducedGenContam.allocate( ZoneRetPlenCond( ZonePlenumNum ).NumInducedNodes );
	ZoneRetPlenCond( ZonePlenumNum ).InletNode(1) = 1;
	ZoneRetPlenCond( ZonePlenumNum ).InducedMassFlowRate = 0.0;
	ZoneRetPlenCond( ZonePlenumNum ).InducedMassFlowRateMaxAvail = 0.0;
	ZoneRetPlenCond( ZonePlenumNum ).InducedMassFlowRateMinAvail = 0.0;
	ZoneRetPlenCond( ZonePlenumNum ).InducedTemp = 0.0;
	ZoneRetPlenCond( ZonePlenumNum ).InducedHumRat = 0.0;
	ZoneRetPlenCond( ZonePlenumNum ).InducedEnthalpy = 0.0;
	ZoneRetPlenCond( ZonePlenumNum ).InducedPressure = 0.0;
	ZoneRetPlenCond( ZonePlenumNum ).InducedCO2 = 0.0;
	ZoneRetPlenCond( ZonePlenumNum ).InducedGenContam = 0.0;

	Node.allocate( 4 ); // One node per plenum plus total of NumInducedNodes for all plenums)
	int ZoneNodeNum = 1;
	ZoneRetPlenCond( ZonePlenumNum ).ZoneNodeNum = ZoneNodeNum;
	Node( ZoneNodeNum ).Temp = 24.2;
	Node( ZoneNodeNum ).HumRat= 0.0003;
	Node( ZoneNodeNum ).Enthalpy = 40000.0;
	Node( ZoneNodeNum ).Press = 99000.0;
	Node( ZoneNodeNum ).CO2 = 950.0;
	Node( ZoneNodeNum ).GenContam = 100.0;
	ZoneRetPlenCond( ZonePlenumNum ).OutletPressure = 99000.0;

	int InducedNodeIndex = 1;
	int InducedNodeNum = 2;
	ZoneRetPlenCond( ZonePlenumNum ).InducedNode( InducedNodeIndex ) = InducedNodeNum;
	Node( InducedNodeNum ).MassFlowRate = 0.20;
	Node( InducedNodeNum ).MassFlowRateMaxAvail = 0.25;
	Node( InducedNodeNum ).MassFlowRateMinAvail = 0.10;

	InducedNodeIndex = 2;
	InducedNodeNum = 3;
	ZoneRetPlenCond( ZonePlenumNum ).InducedNode( InducedNodeIndex ) = InducedNodeNum;
	Node( InducedNodeNum ).MassFlowRate = 0.40;
	Node( InducedNodeNum ).MassFlowRateMaxAvail = 0.50;
	Node( InducedNodeNum ).MassFlowRateMinAvail = 0.22;

	ZoneRetPlenCond( ZonePlenumNum ).OutletNode = 4;

	InitAirZoneReturnPlenum( ZonePlenumNum );
	UpdateAirZoneReturnPlenum( ZonePlenumNum );

	EXPECT_EQ( Node( ZoneRetPlenCond( ZonePlenumNum ).ZoneNodeNum ).CO2, Node(ZoneRetPlenCond( ZonePlenumNum ).OutletNode ).CO2 );
	EXPECT_EQ( Node( ZoneRetPlenCond( ZonePlenumNum ).ZoneNodeNum ).CO2, Node(ZoneRetPlenCond( ZonePlenumNum ).OutletNode ).CO2 );

	for ( InducedNodeIndex = 1; InducedNodeIndex <= ZoneRetPlenCond( ZonePlenumNum ).NumInducedNodes; ++InducedNodeIndex ) {
		InducedNodeNum = ZoneRetPlenCond( ZonePlenumNum ).InducedNode( InducedNodeIndex );
		EXPECT_EQ( Node( InducedNodeNum ).MassFlowRate, ZoneRetPlenCond( ZonePlenumNum ).InducedMassFlowRate( InducedNodeIndex ) );
		EXPECT_EQ( Node( InducedNodeNum ).MassFlowRateMaxAvail, ZoneRetPlenCond( ZonePlenumNum ).InducedMassFlowRateMaxAvail( InducedNodeIndex ) );
		EXPECT_EQ( Node( InducedNodeNum ).MassFlowRateMinAvail, ZoneRetPlenCond( ZonePlenumNum ).InducedMassFlowRateMinAvail( InducedNodeIndex ) );
		EXPECT_EQ( Node( ZoneNodeNum ).Temp, ZoneRetPlenCond( ZonePlenumNum ).InducedTemp( InducedNodeIndex ) );
		EXPECT_EQ( Node( ZoneNodeNum ).HumRat, ZoneRetPlenCond( ZonePlenumNum ).InducedHumRat( InducedNodeIndex ) );
		EXPECT_EQ( Node( ZoneNodeNum ).Enthalpy, ZoneRetPlenCond( ZonePlenumNum ).InducedEnthalpy( InducedNodeIndex ) );
		EXPECT_EQ( Node( ZoneNodeNum ).Press, ZoneRetPlenCond( ZonePlenumNum ).InducedPressure( InducedNodeIndex ) );
		EXPECT_EQ( Node( ZoneNodeNum ).CO2, ZoneRetPlenCond( ZonePlenumNum ).InducedCO2( InducedNodeIndex ) );
		EXPECT_EQ( Node( ZoneNodeNum ).GenContam, ZoneRetPlenCond( ZonePlenumNum ).InducedGenContam( InducedNodeIndex ) );
		EXPECT_EQ( Node( ZoneNodeNum ).Temp, ZoneRetPlenCond( ZonePlenumNum ).ZoneTemp );
		EXPECT_EQ( Node( ZoneNodeNum ).HumRat, ZoneRetPlenCond( ZonePlenumNum ).ZoneHumRat );
		EXPECT_EQ( Node( ZoneNodeNum ).Enthalpy, ZoneRetPlenCond( ZonePlenumNum ).ZoneEnthalpy );
	}

	// Deallocate everything
	ZoneRetPlenCond( ZonePlenumNum ).InletNode.deallocate( );
	ZoneRetPlenCond( ZonePlenumNum ).InducedNode.deallocate( );
	ZoneRetPlenCond( ZonePlenumNum ).InducedMassFlowRate.deallocate( );
	ZoneRetPlenCond( ZonePlenumNum ).InducedMassFlowRateMaxAvail.deallocate( );
	ZoneRetPlenCond( ZonePlenumNum ).InducedMassFlowRateMinAvail.deallocate( );
	ZoneRetPlenCond( ZonePlenumNum ).InducedTemp.deallocate( );
	ZoneRetPlenCond( ZonePlenumNum ).InducedHumRat.deallocate( );
	ZoneRetPlenCond( ZonePlenumNum ).InducedEnthalpy.deallocate( );
	ZoneRetPlenCond( ZonePlenumNum ).InducedPressure.deallocate( );
	ZoneRetPlenCond( ZonePlenumNum ).InducedCO2.deallocate( );
	ZoneRetPlenCond( ZonePlenumNum ).InducedGenContam.deallocate( );
	ZoneRetPlenCond.deallocate( );
	Node.deallocate();

}
