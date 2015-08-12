// EnergyPlus::VentilatedSlab Unit Tests

// Google Test Headers
#include <gtest/gtest.h>

// EnergyPlus Headers
#include <EnergyPlus/UtilityRoutines.hh>
#include <EnergyPlus/VentilatedSlab.hh>
#include <EnergyPlus/DataLoopNode.hh>
#include <EnergyPlus/Psychrometrics.hh>

using namespace EnergyPlus;
using namespace ObjexxFCL;
using namespace DataGlobals;
using namespace EnergyPlus::VentilatedSlab;
using namespace EnergyPlus::DataLoopNode;
using namespace EnergyPlus::Psychrometrics;


TEST( VentilatedSlab, CalcVentilatedSlabCoilOutputTest )
{
	ShowMessage( "Begin Test: VentilatedSlab, CalcVentilatedSlabCoilOutputTest" );

	InitializePsychRoutines();
	BeginEnvrnFlag = false;
	Real64 PowerMet = 0.0;
	Real64 LatOutputProvided = 0.0;

	NumOfVentSlabs = 1;
	VentSlab.allocate( NumOfVentSlabs );
	int Item = 1;
	int FanOutletNode = 1;
	int OutletNode = 2;
	VentSlab( Item ).FanOutletNode = FanOutletNode;
	VentSlab( Item ).RadInNode = OutletNode;
	Node.allocate( 2 );
	Node( OutletNode ).MassFlowRate = 0.5;

	// Calcs being tested
	//	VentSlab( Item ).HeatCoilPower = max( 0.0, QUnitOut );
	//	VentSlab( Item ).SensCoolCoilPower = std::abs( min( 0.0, QUnitOut ) );
	//	VentSlab( Item ).TotCoolCoilPower = std::abs( min( 0.0, QTotUnitOut ) );
	//	VentSlab( Item ).LateCoolCoilPower = VentSlab( Item ).TotCoolCoilPower - VentSlab( Item ).SensCoolCoilPower;
	//	LatOutputProvided = AirMassFlow * ( SpecHumOut - SpecHumIn ); // Latent rate (kg/s), dehumid = negative
	//	PowerMet = QUnitOut;

	// Sensible Heating
	Node( FanOutletNode ).Temp = 15.0;
	Node( FanOutletNode ).HumRat = 0.003;
	Node( OutletNode ).Temp = 20.0;
	Node( OutletNode ).HumRat = 0.003;
	CalcVentilatedSlabCoilOutput( Item, PowerMet, LatOutputProvided );

	EXPECT_TRUE( VentSlab( Item ).HeatCoilPower > 0.0 );
	EXPECT_TRUE( VentSlab( Item ).SensCoolCoilPower == 0.0 );
	EXPECT_TRUE( VentSlab( Item ).TotCoolCoilPower == 0.0 );
	EXPECT_TRUE( VentSlab( Item ).LateCoolCoilPower == 0.0 );
	EXPECT_TRUE( LatOutputProvided == 0.0 );
	EXPECT_TRUE( PowerMet > 0.0 );

	// Sensible Cooling
	Node( FanOutletNode ).Temp = 25.0;
	Node( FanOutletNode ).HumRat = 0.003;
	Node( OutletNode ).Temp = 20.0;
	Node( OutletNode ).HumRat = 0.003;
	CalcVentilatedSlabCoilOutput( Item, PowerMet, LatOutputProvided );

	EXPECT_TRUE( VentSlab( Item ).HeatCoilPower == 0.0 );
	EXPECT_TRUE( VentSlab( Item ).SensCoolCoilPower > 0.0 );
	EXPECT_TRUE( VentSlab( Item ).TotCoolCoilPower == VentSlab( Item ).SensCoolCoilPower );
	EXPECT_TRUE( VentSlab( Item ).LateCoolCoilPower == 0.0 );
	EXPECT_TRUE( LatOutputProvided == 0.0 );
	EXPECT_TRUE( PowerMet < 0.0 );

	// Sensible and Latent Cooling
	Node( FanOutletNode ).Temp = 25.0;
	Node( FanOutletNode ).HumRat = 0.008;
	Node( OutletNode ).Temp = 20.0;
	Node( OutletNode ).HumRat = 0.003;
	CalcVentilatedSlabCoilOutput( Item, PowerMet, LatOutputProvided );

	EXPECT_TRUE( VentSlab( Item ).HeatCoilPower == 0.0 );
	EXPECT_TRUE( VentSlab( Item ).SensCoolCoilPower > 0.0 );
	EXPECT_TRUE( VentSlab( Item ).TotCoolCoilPower > VentSlab( Item ).SensCoolCoilPower );
	EXPECT_TRUE( VentSlab( Item ).LateCoolCoilPower > 0.0 );
	EXPECT_TRUE( LatOutputProvided < 0.0 );
	EXPECT_TRUE( PowerMet < 0.0 );

	// Deallocate everything
	VentSlab.deallocate( );
	Node.deallocate( );

}
