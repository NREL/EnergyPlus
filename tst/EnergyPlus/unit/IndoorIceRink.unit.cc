// EnergyPlus::IndoorIceRink Unit Tests

// Google Test Headers
#include <gtest/gtest.h>

// EnergyPlus Headers
#include <EnergyPlus/DataHeatBalance.hh>
#include <EnergyPlus/DataLoopNode.hh>
#include <EnergyPlus/DataSurfaceLists.hh>
#include <EnergyPlus/DataSurfaces.hh>
#include <EnergyPlus/HeatBalanceManager.hh>
#include <EnergyPlus/IndoorIceRink.hh>
#include <EnergyPlus/ScheduleManager.hh>
#include <EnergyPlus/SurfaceGeometry.hh>
#include <EnergyPlus/UtilityRoutines.hh>

#include "Fixtures/EnergyPlusFixture.hh"

using namespace EnergyPlus;
using namespace ObjexxFCL;
using namespace DataGlobals;
using namespace EnergyPlus::IceRink;
using namespace EnergyPlus::DataHeatBalance;
using namespace EnergyPlus::DataLoopNode;
using namespace EnergyPlus::DataSurfaces;
using namespace EnergyPlus::DataSurfaceLists;
using namespace EnergyPlus::HeatBalanceManager;
using namespace EnergyPlus::ScheduleManager;
using namespace EnergyPlus::SurfaceGeometry;

TEST_F(EnergyPlusFixture, IndoorIceRink_ResurfacerTest)
{
}
