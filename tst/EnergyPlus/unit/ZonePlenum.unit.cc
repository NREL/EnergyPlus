// EnergyPlus::ZonePlenum Unit Tests

// Google Test Headers
#include <gtest/gtest.h>

// EnergyPlus Headers
#include <EnergyPlus/UtilityRoutines.hh>
#include <EnergyPlus/ZonePlenum.hh>
#include <EnergyPlus/DataLoopNode.hh>
#include <EnergyPlus/Psychrometrics.hh>

using namespace EnergyPlus;
using namespace ObjexxFCL;
using namespace DataGlobals;
using namespace EnergyPlus::ZonePlenum;
using namespace EnergyPlus::DataLoopNode;
using namespace EnergyPlus::Psychrometrics;


TEST( ZonePlenum, InitAirZoneReturnPlenumTest )
{
	ShowMessage( "Begin Test: ZonePlenum, InitAirZoneReturnPlenumTest" );

	InitializePsychRoutines();
	BeginEnvrnFlag = false;

	//TimeStepSys = 15.0 / 60.0; // System timestep in hours

	//ZoneEquipConfig.allocate( 1 );
	//ZoneEquipConfig( 1 ).ZoneName = "Zone 1";
	//ZoneEquipConfig( 1 ).ActualZoneNum = 1;
	//std::vector< int > controlledZoneEquipConfigNums;
	//controlledZoneEquipConfigNums.push_back( 1 );

	//ZoneEquipConfig( 1 ).NumInletNodes = 2;
	//ZoneEquipConfig( 1 ).InletNode.allocate( 2 );
	//ZoneEquipConfig( 1 ).InletNode( 1 ) = 1;
	//ZoneEquipConfig( 1 ).InletNode( 2 ) = 2;
	//ZoneEquipConfig( 1 ).NumExhaustNodes = 1;
	//ZoneEquipConfig( 1 ).ExhaustNode.allocate( 1 );
	//ZoneEquipConfig( 1 ).ExhaustNode( 1 ) = 3;
	//ZoneEquipConfig( 1 ).ReturnAirNode = 4;

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
	ZoneRetPlenCond( ZonePlenumNum ).InducedMassFlowRate = 0.0;
	ZoneRetPlenCond( ZonePlenumNum ).InducedMassFlowRateMaxAvail = 0.0;
	ZoneRetPlenCond( ZonePlenumNum ).InducedMassFlowRateMinAvail = 0.0;
	ZoneRetPlenCond( ZonePlenumNum ).InducedTemp = 0.0;
	ZoneRetPlenCond( ZonePlenumNum ).InducedHumRat = 0.0;
	ZoneRetPlenCond( ZonePlenumNum ).InducedEnthalpy = 0.0;
	ZoneRetPlenCond( ZonePlenumNum ).InducedPressure = 0.0;

	Node.allocate( 3 ); // One node per plenum plus total of NumInducedNodes for all plenums)
	int ZoneNodeNum = 1;
	ZoneRetPlenCond( ZonePlenumNum ).ZoneNodeNum = ZoneNodeNum;
	Node( ZoneNodeNum ).Temp = 24.2;
	Node( ZoneNodeNum ).HumRat= 0.0003;
	Node( ZoneNodeNum ).Enthalpy = 40000.0;
	Node( ZoneNodeNum ).Press = 99000.0;

	int InducedNodeIndex = 1;
	int InducedNode = 2;
	ZoneRetPlenCond( ZonePlenumNum ).InducedNode( InducedNodeIndex ) = InducedNode;
	Node( InducedNode ).MassFlowRate = 0.20;
	Node( InducedNode ).MassFlowRateMaxAvail = 0.25;
	Node( InducedNode ).MassFlowRateMinAvail = 0.10;

	InducedNodeIndex = 2;
	InducedNode = 3;
	ZoneRetPlenCond( ZonePlenumNum ).InducedNode( InducedNodeIndex ) = InducedNode;
	Node( InducedNode ).MassFlowRate = 0.40;
	Node( InducedNode ).MassFlowRateMaxAvail = 0.50;
	Node( InducedNode ).MassFlowRateMinAvail = 0.22;

	InitAirZoneReturnPlenum( ZonePlenumNum );
	
	// Check first induced air node
	InducedNodeIndex = 1;
	InducedNode = ZoneRetPlenCond( ZonePlenumNum ).InducedNode( InducedNodeIndex );
	EXPECT_EQ( ZoneRetPlenCond( ZonePlenumNum ).InducedMassFlowRate( InducedNodeIndex ), Node( InducedNode ).MassFlowRate );
	EXPECT_EQ( ZoneRetPlenCond( ZonePlenumNum ).InducedMassFlowRateMaxAvail( InducedNodeIndex ) , Node( InducedNode ).MassFlowRateMaxAvail );
	EXPECT_EQ( ZoneRetPlenCond( ZonePlenumNum ).InducedMassFlowRateMinAvail( InducedNodeIndex ) , Node( InducedNode ).MassFlowRateMinAvail );
	EXPECT_EQ( ZoneRetPlenCond( ZonePlenumNum ).InducedTemp( InducedNodeIndex ), Node( ZoneNodeNum ).Temp );
	EXPECT_EQ( ZoneRetPlenCond( ZonePlenumNum ).InducedHumRat( InducedNodeIndex ) , Node( ZoneNodeNum ).HumRat );
	EXPECT_EQ( ZoneRetPlenCond( ZonePlenumNum ).InducedEnthalpy( InducedNodeIndex ) , Node( ZoneNodeNum ).Enthalpy );
	EXPECT_EQ( ZoneRetPlenCond( ZonePlenumNum ).InducedPressure( InducedNodeIndex ) , Node( ZoneNodeNum ).Press );
	EXPECT_EQ( ZoneRetPlenCond( ZonePlenumNum ).ZoneTemp , Node( ZoneNodeNum ).Temp );
	EXPECT_EQ( ZoneRetPlenCond( ZonePlenumNum ).ZoneHumRat , Node( ZoneNodeNum ).HumRat );
	EXPECT_EQ( ZoneRetPlenCond( ZonePlenumNum ).ZoneEnthalpy , Node( ZoneNodeNum ).Enthalpy );

	// Check second induced air node
	InducedNodeIndex = 2;
	InducedNode = ZoneRetPlenCond( ZonePlenumNum ).InducedNode( InducedNodeIndex );
	EXPECT_EQ( ZoneRetPlenCond( ZonePlenumNum ).InducedMassFlowRate( InducedNodeIndex ), Node( InducedNode ).MassFlowRate );
	EXPECT_EQ( ZoneRetPlenCond( ZonePlenumNum ).InducedMassFlowRateMaxAvail( InducedNodeIndex ), Node( InducedNode ).MassFlowRateMaxAvail );
	EXPECT_EQ( ZoneRetPlenCond( ZonePlenumNum ).InducedMassFlowRateMinAvail( InducedNodeIndex ), Node( InducedNode ).MassFlowRateMinAvail );
	EXPECT_EQ( ZoneRetPlenCond( ZonePlenumNum ).InducedTemp( InducedNodeIndex ), Node( ZoneNodeNum ).Temp );
	EXPECT_EQ( ZoneRetPlenCond( ZonePlenumNum ).InducedHumRat( InducedNodeIndex ), Node( ZoneNodeNum ).HumRat );
	EXPECT_EQ( ZoneRetPlenCond( ZonePlenumNum ).InducedEnthalpy( InducedNodeIndex ), Node( ZoneNodeNum ).Enthalpy );
	EXPECT_EQ( ZoneRetPlenCond( ZonePlenumNum ).InducedPressure( InducedNodeIndex ), Node( ZoneNodeNum ).Press );
	EXPECT_EQ( ZoneRetPlenCond( ZonePlenumNum ).ZoneTemp, Node( ZoneNodeNum ).Temp );
	EXPECT_EQ( ZoneRetPlenCond( ZonePlenumNum ).ZoneHumRat, Node( ZoneNodeNum ).HumRat );
	EXPECT_EQ( ZoneRetPlenCond( ZonePlenumNum ).ZoneEnthalpy, Node( ZoneNodeNum ).Enthalpy );

	// Deallocate everything
	ZoneRetPlenCond( ZonePlenumNum ).InducedNode.deallocate( );
	ZoneRetPlenCond( ZonePlenumNum ).InducedMassFlowRate.deallocate( );
	ZoneRetPlenCond( ZonePlenumNum ).InducedMassFlowRateMaxAvail.deallocate( );
	ZoneRetPlenCond( ZonePlenumNum ).InducedMassFlowRateMinAvail.deallocate( );
	ZoneRetPlenCond( ZonePlenumNum ).InducedTemp.deallocate( );
	ZoneRetPlenCond( ZonePlenumNum ).InducedHumRat.deallocate( );
	ZoneRetPlenCond( ZonePlenumNum ).InducedEnthalpy.deallocate( );
	ZoneRetPlenCond( ZonePlenumNum ).InducedPressure.deallocate( );
	ZoneRetPlenCond.deallocate( );
	Node.deallocate();

}
