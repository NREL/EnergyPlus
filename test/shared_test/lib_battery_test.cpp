
#include <math.h>
#include <gtest/gtest.h>

#include <lib_battery.h>

#include "lib_battery_test.h"

/// Test  lithium ion battery capacity response
TEST_F(BatteryTest, LithiumIonCapacityTest_lib_battery)
{
	q = 100;
	SOC_init = 100;
	SOC_min = 20;
	SOC_max = 100;

	if (capacityModel) {
		delete capacityModel;
	}
	capacityModel = new capacity_lithium_ion_t(q, SOC_init, SOC_max, SOC_min);

	// Check that initial capacity is equal to max
	EXPECT_EQ(capacityModel->SOC(), SOC_max);

	// Check that discharge of battery results in correct capacity
	double I = 10;
	capacityModel->updateCapacity(I, 1);
	EXPECT_EQ(capacityModel->SOC(), 90);
	EXPECT_EQ(capacityModel->q0(), 90);

	// check that charge of battery results in correct capacity
	I = -10;
	capacityModel->updateCapacity(I, 1);
	EXPECT_EQ(capacityModel->SOC(), 100);
	EXPECT_EQ(capacityModel->q0(), 100);

	// check that updating thermal behavior changes capacity as expected
	capacityModel->updateCapacityForThermal(95);
	capacityModel->check_SOC();
	EXPECT_EQ(capacityModel->q0(), 95);
	EXPECT_EQ(capacityModel->qmax(), 100);
	capacityModel->updateCapacityForThermal(100);
	capacityModel->check_SOC();

	// check that updating lifetime degradation changes capacity
	capacityModel->updateCapacityForLifetime(95);
	EXPECT_EQ(capacityModel->q0(), 95);
	EXPECT_EQ(capacityModel->qmax(), 95);

	// check that battery replacement works
	capacityModel->replace_battery(100);
	EXPECT_EQ(capacityModel->SOC(), SOC_max);
	EXPECT_EQ(capacityModel->q0(), 100);
	EXPECT_EQ(capacityModel->qmax(), 100);

	// check that model correctly detects overcharge, undercharge
	capacityModel->updateCapacity(I, 1);
	EXPECT_EQ(capacityModel->q0(), 100);
	EXPECT_EQ(capacityModel->SOC(), SOC_max);

	I = 110;
	capacityModel->updateCapacity(I, 1);
	EXPECT_EQ(capacityModel->q0(), 20);
	EXPECT_EQ(capacityModel->SOC(), SOC_min);
}

TEST_F(BatteryTest, LossesModel_lib_battery)
{
	size_t idx = 1000;

	// Return loss for february
	lossModel->run_losses(idx);
	EXPECT_EQ(lossModel->getLoss(idx), 1);

}

TEST_F(BatteryTest, AugmentCapacity)
{
	
	std::vector<int> replacement_schedule = { 1, 1, 1 };
	std::vector<double> augmentation_percent = { 50, 40 , 30 };
	batteryModel->lifetime_model()->set_replacement_option(battery_t::REPLACE_BY_SCHEDULE);

	// Correct future approach for augmenting batteries, by treating as seperate entities
	std::vector<battery_t *> batteries;
	batteries.push_back(batteryModel);
	batteries.push_back(new battery_t(dtHour, chemistry));
	batteries[1]->initialize(capacityModel, voltageModel, lifetimeModel, thermalModel, lossModel);
	batteries[1]->lifetime_model()->set_replacement_option(battery_t::REPLACE_BY_SCHEDULE);
	batteries.push_back(new battery_t(dtHour, chemistry));
	batteries[2]->initialize(capacityModel, voltageModel, lifetimeModel, thermalModel, lossModel);
	batteries[2]->lifetime_model()->set_replacement_option(battery_t::REPLACE_BY_SCHEDULE);

	size_t i = 0;
	double I = 100;
	double mult = 1.0;
	size_t replaceCount = 0;
	for (size_t y = 0; y < replacement_schedule.size(); y++) {
		for (size_t t = 0; t < 8760; t++) {
			mult = fmod(t, 2) == 0 ? 1 : -1;
			double current = mult*I;
			batteries[replaceCount]->run(i, current);
		}
		if (replacement_schedule[y] == 1) {
			replaceCount++;
		}
	}
	
	// Current, limited approach which only augments capacity in models, does not update lifetime degradation 
	// trajectories or consider impacts on voltage and other aspects.
	battery_t * battery = new battery_t(dtHour, chemistry);
	battery->initialize(capacityModel, voltageModel, lifetimeModel, thermalModel, lossModel);
	battery->lifetime_model()->set_replacement_option(battery_t::REPLACE_BY_SCHEDULE);





}

TEST_F(BatteryTest, TestLifetimeDegradation) {
	double vals[] = { 0, 100, 365, 50 };
	util::matrix_t<double> lifetime_matrix;
	lifetime_matrix.assign(vals, 2, 2);

	double dt_hour = 1;
	lifetime_calendar_t hourly_lifetime(lifetime_calendar_t::CALENDAR_LOSS_OPTIONS::CALENDAR_LOSS_TABLE, lifetime_matrix, dt_hour);

	for (int idx = 0; idx < 8760; idx++) {
		hourly_lifetime.runLifetimeCalendarModel(idx, 20, 80);
	}

	EXPECT_NEAR(hourly_lifetime.capacity_percent(), 50, 1);

	dt_hour = 1.0 / 12.0; // Every 5 mins
	lifetime_calendar_t subhourly_lifetime(lifetime_calendar_t::CALENDAR_LOSS_OPTIONS::CALENDAR_LOSS_TABLE, lifetime_matrix, dt_hour);

	for (int idx = 0; idx < 8760 * 12; idx++) {
		subhourly_lifetime.runLifetimeCalendarModel(idx, 20, 80);
	}

	EXPECT_NEAR(subhourly_lifetime.capacity_percent(), 50, 1);
}