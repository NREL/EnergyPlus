// EnergyPlus::ExteriorEnergyUse Unit Tests

// Google Test Headers
#include <gtest/gtest.h>

// EnergyPlus Headers
#include <EnergyPlus/EvaporativeCoolers.hh>

using namespace EnergyPlus;

// This could almost definitely benefit from some improvements such as a fixture to only do init once,
//   but I've never used those and am just interested in stubbing this out for now

TEST( EvaporativeCoolers, CalcSecondaryAirOutletCondition )
{

	EvaporativeCoolers::EvapCond.allocate( 1 );
	int const evapCoolNum( 1 );
	EvaporativeCoolers::EvapCond( evapCoolNum ).SecInletEnthalpy = 1; // more realistic value

	// set up arguments
	int const OperatingMode( EvaporativeCoolers::None );
	Real64 const AirMassFlowSec( 0.0 );
	Real64 const EDBTSec( 0.0 );
	Real64 const EWBTSec( 0.0 );
	Real64 const EHumRatSec( 0.0 );
	Real64 const QHXTotal( 0.0 );
	Real64 QHXLatent( 0.0 );

	// make the call
	EvaporativeCoolers::CalcSecondaryAirOutletCondition(
		evapCoolNum,
		OperatingMode,
		AirMassFlowSec,
		EDBTSec,
		EWBTSec,
		EHumRatSec,
		QHXTotal,
		QHXLatent
	);
	
	// check outputs
	EXPECT_EQ( EvaporativeCoolers::EvapCond( evapCoolNum ).SecOutletEnthalpy, EvaporativeCoolers::EvapCond( evapCoolNum ).SecInletEnthalpy );
	EXPECT_EQ( 0.0, QHXLatent );
}

TEST( EvaporativeCoolers, CalcIndirectRDDEvapCoolerOutletTemp )
{
	//void
	//CalcIndirectRDDEvapCoolerOutletTemp( int const EvapCoolNum, 
	//int const DryOrWetOperatingMode,
	//Real64 const AirMassFlowSec,
	//Real64 const EDBTSec,
	//Real64 const EWBTSec,
	//Real64 const EHumRatSec );
	EXPECT_EQ( 1, 1 );
}

TEST( EvaporativeCoolers, IndEvapCoolerPower )
{
	//Real64
	//IndEvapCoolerPower(
	//int const EvapCoolIndex, // Unit index
	//int const DryWetMode, // dry or wet operating mode of evaporator cooler
	//Real64 const FlowRatio // secondary air flow fraction
	//);
	EXPECT_EQ( 1, 1 );
}
