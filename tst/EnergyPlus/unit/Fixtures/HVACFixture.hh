#ifndef HVACFixture_hh_INCLUDED
#define HVACFixture_hh_INCLUDED

// Google test headers
#include <gtest/gtest.h>

// EnergyPlus Headers
#include "EnergyPlusFixture.hh"
#include <EnergyPlus/CurveManager.hh>
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataGlobals.hh>
#include <EnergyPlus/DataHeatBalance.hh>
#include <EnergyPlus/DataIPShortCuts.hh>
#include <EnergyPlus/DataOutputs.hh>
#include <EnergyPlus/DataSurfaces.hh>
#include <EnergyPlus/DataZoneControls.hh>
#include <EnergyPlus/ExteriorEnergyUse.hh>
#include <EnergyPlus/Psychrometrics.hh>
#include <EnergyPlus/HeatBalanceManager.hh>
#include <EnergyPlus/InputProcessor.hh>
#include <EnergyPlus/OutputProcessor.hh>
#include <EnergyPlus/ScheduleManager.hh>

#include <EnergyPlus/DataSystemVariables.hh>
#include <EnergyPlus/FileSystem.hh>
#include <EnergyPlus/SortAndStringUtilities.hh>
#include <EnergyPlus/DataAirLoop.hh>
#include <EnergyPlus/DataBranchNodeConnections.hh>
#include <EnergyPlus/DataHVACGlobals.hh>
#include <EnergyPlus/DataLoopNode.hh>
#include <EnergyPlus/DataSizing.hh>
#include <EnergyPlus/Humidifiers.hh>
#include <EnergyPlus/MixedAir.hh>
#include <EnergyPlus/NodeInputManager.hh>
#include <EnergyPlus/OutAirNodeManager.hh>
#include <EnergyPlus/Psychrometrics.hh>
#include <EnergyPlus/DataDefineEquip.hh>
#include <EnergyPlus/BranchNodeConnections.hh>
#include <EnergyPlus/DataZoneEquipment.hh>
#include <EnergyPlus/DataHeatBalance.hh>
#include <EnergyPlus/DataPlant.hh>
#include <EnergyPlus/GlobalNames.hh>
#include <EnergyPlus/PlantManager.hh>
#include <EnergyPlus/SetPointManager.hh>
#include <EnergyPlus/OutsideEnergySources.hh>
#include <EnergyPlus/HVACManager.hh>
#include <EnergyPlus/BranchInputManager.hh>
#include <EnergyPlus/SimulationManager.hh>

#include <EnergyPlus/DataConvergParams.hh>
#include <EnergyPlus/SizingManager.hh>
#include <EnergyPlus/WeatherManager.hh>
#include <EnergyPlus/OutputReportPredefined.hh>
// following have not added clear_state yet

//#include <EnergyPlus/ZoneEquipmentManager.hh>


#include <EnergyPlus/DataZoneEnergyDemands.hh>
#include <EnergyPlus/DataHeatBalFanSys.hh>
#include <EnergyPlus/PlantUtilities.hh>

namespace EnergyPlus {

	class HVACFixture : public EnergyPlusFixture
	{
	protected:
		static void SetUpTestCase() { }
		static void TearDownTestCase() { }
		
		virtual void SetUp() {
			EnergyPlusFixture::SetUp();  // Sets up the base fixture first.

			Psychrometrics::InitializePsychRoutines();
		}

		virtual void TearDown() {
			Psychrometrics::cached_Twb.deallocate();
			Psychrometrics::cached_Psat.deallocate();



			EnergyPlusFixture::TearDown();  // Remember to tear down the base fixture after cleaning up derived fixture!
		}
	};

}

#endif
