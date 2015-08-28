#ifndef WaterThermalTankFixture_hh_INCLUDED
#define WaterThermalTankFixture_hh_INCLUDED

// Google test headers
#include <gtest/gtest.h>

#include "HVACFixture.hh"
#include <EnergyPlus/DataZoneEquipment.hh>
#include <EnergyPlus/WaterThermalTanks.hh>

namespace EnergyPlus {
	
	class WaterThermalTankFixture : public HVACFixture
	{
	protected:
		static void SetUpTestCase() { }
		static void TearDownTestCase() { }
		
		virtual void SetUp() {
			HVACFixture::SetUp();
			DataZoneEquipment::ZoneEquipInputsFilled = false;
		}
		virtual void TearDown() {
			WaterThermalTanks::clear_state();
			HVACFixture::TearDown();
		}
	};
	
}

#endif