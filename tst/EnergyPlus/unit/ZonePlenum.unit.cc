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
	ZoneRetPlenCond( ZonePlenumNum ).InducedMassFlowRate = 0.0;
	ZoneRetPlenCond( ZonePlenumNum ).InducedMassFlowRateMaxAvail = 0.0;
	ZoneRetPlenCond( ZonePlenumNum ).InducedMassFlowRateMinAvail = 0.0;
	ZoneRetPlenCond( ZonePlenumNum ).InducedTemp = 0.0;
	ZoneRetPlenCond( ZonePlenumNum ).InducedHumRat = 0.0;
	ZoneRetPlenCond( ZonePlenumNum ).InducedEnthalpy = 0.0;
	ZoneRetPlenCond( ZonePlenumNum ).InducedPressure = 0.0;
	ZoneRetPlenCond( ZonePlenumNum ).InducedCO2 = 0.0;
	ZoneRetPlenCond( ZonePlenumNum ).InducedGenContam = 0.0;

	Node.allocate( 3 ); // One node per plenum plus total of NumInducedNodes for all plenums)
	int ZoneNodeNum = 1;
	ZoneRetPlenCond( ZonePlenumNum ).ZoneNodeNum = ZoneNodeNum;
	Node( ZoneNodeNum ).Temp = 24.2;
	Node( ZoneNodeNum ).HumRat= 0.0003;
	Node( ZoneNodeNum ).Enthalpy = 40000.0;
	Node( ZoneNodeNum ).Press = 99000.0;
	Node( ZoneNodeNum ).CO2 = 50000.0;
	Node( ZoneNodeNum ).GenContam = 100.0;

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

	InitAirZoneReturnPlenum( ZonePlenumNum );
	
	for ( InducedNodeIndex = 1; InducedNodeIndex <= ZoneRetPlenCond( ZonePlenumNum ).NumInducedNodes; ++InducedNodeIndex ) {
		InducedNodeNum = ZoneRetPlenCond( ZonePlenumNum ).InducedNode( InducedNodeIndex );
		EXPECT_EQ( ZoneRetPlenCond( ZonePlenumNum ).InducedMassFlowRate( InducedNodeIndex ), Node( InducedNodeNum ).MassFlowRate );
		EXPECT_EQ( ZoneRetPlenCond( ZonePlenumNum ).InducedMassFlowRateMaxAvail( InducedNodeIndex ) , Node( InducedNodeNum ).MassFlowRateMaxAvail );
		EXPECT_EQ( ZoneRetPlenCond( ZonePlenumNum ).InducedMassFlowRateMinAvail( InducedNodeIndex ) , Node( InducedNodeNum ).MassFlowRateMinAvail );
		EXPECT_EQ( ZoneRetPlenCond( ZonePlenumNum ).InducedTemp( InducedNodeIndex ), Node( ZoneNodeNum ).Temp );
		EXPECT_EQ( ZoneRetPlenCond( ZonePlenumNum ).InducedHumRat( InducedNodeIndex ) , Node( ZoneNodeNum ).HumRat );
		EXPECT_EQ( ZoneRetPlenCond( ZonePlenumNum ).InducedEnthalpy( InducedNodeIndex ) , Node( ZoneNodeNum ).Enthalpy );
		EXPECT_EQ( ZoneRetPlenCond( ZonePlenumNum ).InducedPressure( InducedNodeIndex ) , Node( ZoneNodeNum ).Press );
		EXPECT_EQ( ZoneRetPlenCond( ZonePlenumNum ).InducedCO2( InducedNodeIndex ), Node( ZoneNodeNum ).CO2 );
		EXPECT_EQ( ZoneRetPlenCond( ZonePlenumNum ).InducedGenContam( InducedNodeIndex ), Node( ZoneNodeNum ).GenContam );
		EXPECT_EQ( ZoneRetPlenCond( ZonePlenumNum ).ZoneTemp, Node( ZoneNodeNum ).Temp );
		EXPECT_EQ( ZoneRetPlenCond( ZonePlenumNum ).ZoneHumRat , Node( ZoneNodeNum ).HumRat );
		EXPECT_EQ( ZoneRetPlenCond( ZonePlenumNum ).ZoneEnthalpy , Node( ZoneNodeNum ).Enthalpy );
	}

	// Deallocate everything
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
