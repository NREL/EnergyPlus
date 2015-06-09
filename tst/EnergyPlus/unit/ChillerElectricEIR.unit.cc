// EnergyPlus::Standalone ERV Unit Tests

// Google Test Headers
#include <gtest/gtest.h>

// EnergyPlus Headers
#include <EnergyPlus/ChillerElectricEIR.hh>
#include <EnergyPlus/DataLoopNode.hh>

using namespace EnergyPlus;
using namespace EnergyPlus::ChillerElectricEIR;
using namespace EnergyPlus::DataLoopNode;

TEST( ChillerElectricEIR, TestOutletNodeConditions )
{

	int Num = 1;

	ElectricEIRChiller.allocate( Num );
	ElectricEIRChillerReport.allocate( Num );

	ElectricEIRChiller( Num ).EvapInletNodeNum = 1;
	ElectricEIRChiller( Num ).EvapOutletNodeNum = 2;
	ElectricEIRChiller( Num ).CondInletNodeNum = 3;
	ElectricEIRChiller( Num ).CondOutletNodeNum = 4;
	ElectricEIRChiller( Num ).HeatRecInletNodeNum = 5;
	ElectricEIRChiller( Num ).HeatRecOutletNodeNum = 6;

	Node.allocate( 6 );
	Node( ElectricEIRChiller( Num ).EvapInletNodeNum ).Temp = 18.0;
	Node( ElectricEIRChiller( Num ).CondInletNodeNum ).Temp = 35.0;

	CondMassFlowRate = 0.0;
	EvapMassFlowRate = 0.0;

	UpdateElectricEIRChillerRecords( -2000, true, 1 );

	EXPECT_EQ( 18, ElectricEIRChillerReport( Num ).EvapOutletTemp );
	EXPECT_EQ( 35, ElectricEIRChillerReport( Num ).CondOutletTemp );

	Node.deallocate( );
	ElectricEIRChiller.deallocate( );
	ElectricEIRChillerReport.deallocate( );
}
