// EnergyPlus::Stand alone unit test of Issue4347; i.e., CalcHWBaseboard NTU-eff calculation

// Google Test Headers
#include <gtest/gtest.h>

// EnergyPlus Headers
#include <DataZoneEnergyDemands.hh>
#include <DataLoopNode.hh>
#include <ScheduleManager.hh>
#include <Psychrometrics.hh>
#include <InputProcessor.hh>
#include <HWBaseboardRadiator.hh>
#include <FluidProperties.hh>
#include <DataPlant.hh>

using namespace EnergyPlus;
using namespace DataZoneEnergyDemands;
using namespace ScheduleManager;
using namespace Psychrometrics;
using namespace InputProcessor;
using namespace HWBaseboardRadiator;
using namespace DataLoopNode;
using namespace FluidProperties;
using namespace DataPlant;

using namespace ObjexxFCL;

TEST(HWBaseboardRadiator, CalcHWBaseboard)
{
	Real64 LoadMet;
	int BBNum;
	ShowMessage( "Begin Test: HWBaseboardRadiator, CalcHWBaseboard - Issue4347" );
	InitializePsychRoutines();
	
	Node.allocate( 1 );
	HWBaseboard.allocate( 1 );
	ZoneSysEnergyDemand.allocate( 1 );
	CurDeadBandOrSetback.allocate( 1 );
	PlantLoop.allocate( 1 );
	QBBRadSource.allocate( 1 );
	
	Node( 1 ).MassFlowRate = 0.40;
	CurDeadBandOrSetback( 1 ) = false;
	ZoneSysEnergyDemand( 1 ).RemainingOutputReqToHeatSP = 12000.;
	BBNum = 1;
	LoadMet = 0.0;
	HWBaseboard( 1 ).ZonePtr = 1;
	HWBaseboard( 1 ).AirInletTemp = 21.;
	HWBaseboard( 1 ).WaterInletTemp = 82.;
	HWBaseboard( 1 ).WaterInletNode = 1;
	HWBaseboard( 1 ).WaterMassFlowRateMax = 0.40;
	HWBaseboard( 1 ).AirMassFlowRateStd = 0.5;
	HWBaseboard( 1 ).SchedPtr = -1;
	HWBaseboard( 1 ).LoopNum = 1;
	HWBaseboard( 1 ).UA = 370;
	PlantLoop( 1 ).FluidName = "Water";
	PlantLoop( 1 ).FluidIndex = 1;
	PlantLoop( 1 ).FluidType = 2;
	QBBRadSource( 1 ) = 0.0;
	
	CalcHWBaseboard( BBNum, LoadMet );

	EXPECT_NEAR( 14746.226690452937, HWBaseboard( 1 ).TotPower, 0.000001);
	EXPECT_NEAR( 50.349854486072232, HWBaseboard( 1 ).AirOutletTemp, 0.000001 );
	EXPECT_NEAR( 73.224991258180438, HWBaseboard( 1 ).WaterOutletTemp, 0.000001 );
	EXPECT_NEAR( 0.5, HWBaseboard( 1 ).AirMassFlowRate, 0.000001 );
	
	Node.deallocate();
	HWBaseboard.deallocate();
	ZoneSysEnergyDemand.deallocate();
	CurDeadBandOrSetback.deallocate();
	PlantLoop.deallocate();
	QBBRadSource.deallocate();
	
}
