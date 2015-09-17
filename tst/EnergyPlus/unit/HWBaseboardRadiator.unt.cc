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

using namespace EnergyPlus;
using namespace DataZoneEnergyDemands;
using namespace ScheduleManager;
using namespace Psychrometrics;
using namespace InputProcessor;
using namespace HWBaseboardRadiator;

using namespace ObjexxFCL;

TEST(HWBaseboardRadiator, CalcHWBaseboard)
{
  ShowMessage( "Begin Test: HWBaseboardRadiator, CalcHWBaseboard - Issue4347" );
  Real64 LoadMet;
  int BBNum;
  InitializePsychRoutines();
  
  Node.allocate( 1 );
  HWBaseboard.allocate( 1 );
  ZoneSysEnergyDemand.allocate( 1 );
  CurDeadBandOrSetback.allocate( 1 );
  
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
  HWBaseboard( 1 ).AirMassFlowRateSts = 0.5;
  HWBaseboard( 1 ).SchedPtr = -1;
  
  CalcHWBaseboard( BBNum, LoadMet );
  
  Node.deallocate();
  HWBaseboard.deallocate();
  ZoneSysEnergyDemand.deallocate();
  CurDeadBandOrSetback.deallocate;
  
}
  
  