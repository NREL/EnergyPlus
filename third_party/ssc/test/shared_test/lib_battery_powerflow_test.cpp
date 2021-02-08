#include <gtest/gtest.h>

#include "lib_battery_powerflow_test.h"
#include "lib_ondinv.h"
#include "lib_pvinv.h"
#include "lib_sandia.h"
#include "lib_shared_inverter.h"


void BatteryPowerFlowTest_lib_battery_powerflow::SetUp()
{
	error = 0.02;
	double dtHour = 1.0;
	m_batteryPowerFlow = new BatteryPowerFlow(dtHour);
	m_batteryPower = m_batteryPowerFlow->getBatteryPower();
	m_batteryPower->reset();
	m_batteryPower->canDischarge = false;
	m_batteryPower->canSystemCharge = false;
	m_batteryPower->canGridCharge = false;
	m_batteryPower->singlePointEfficiencyACToDC = 0.96;
	m_batteryPower->singlePointEfficiencyDCToAC = 0.96;
	m_batteryPower->singlePointEfficiencyDCToDC = 0.98;
	m_batteryPower->powerBatteryChargeMaxDC = 100;
	m_batteryPower->powerBatteryDischargeMaxDC = 50;
	m_batteryPower->connectionMode = ChargeController::AC_CONNECTED;

    // setup Sandia inverter using SMA America: SB3800TL-US-22 (240V) [CEC 2013]
    int numberOfInverters = 100;
    sandia = new sandia_inverter_t();
    partload = new partload_inverter_t();
    ond = new ond_inverter();
    sandia->C0 = -3.18e-6;
    sandia->C1 = -5.12e-5;
    sandia->C2 = 0.000984;
    sandia->C3 = -0.00151;
    sandia->Paco = 3800;
    sandia->Pdco = 3928.11;
    sandia->Vdco = 398.497;
    sandia->Pso = 19.4516;
    sandia->Pntare = 0.99;
    m_sharedInverter = new SharedInverter(SharedInverter::SANDIA_INVERTER, numberOfInverters, sandia, partload, ond);
    m_batteryPower->setSharedInverter(m_sharedInverter);
}

TEST_F(BatteryPowerFlowTest_lib_battery_powerflow, TestInitialize)
{
	// PV Charging Scenario
	m_batteryPower->canSystemCharge = true;
	m_batteryPower->powerSystem = 100;
	m_batteryPower->powerLoad = 50;
	m_batteryPowerFlow->initialize(50);
	EXPECT_EQ(m_batteryPower->powerBatteryDC, -50);

    // Grid charging Scenario
    m_batteryPower->canGridCharge = true;
    m_batteryPowerFlow->initialize(50);
    EXPECT_EQ(m_batteryPower->powerBatteryDC, -m_batteryPower->powerBatteryChargeMaxDC);

	// Discharging Scenario
	m_batteryPower->canDischarge = true;
	m_batteryPower->powerSystem = 50;
	m_batteryPower->powerLoad = 100;
	m_batteryPowerFlow->initialize(50);
	EXPECT_EQ(m_batteryPower->powerBatteryDC, m_batteryPower->powerBatteryDischargeMaxDC);
}

// Excess PV production
TEST_F(BatteryPowerFlowTest_lib_battery_powerflow, AC_PVCharging_ExcessPV) {
    m_batteryPower->connectionMode = ChargeController::AC_CONNECTED;

    m_batteryPower->canSystemCharge = true;
    m_batteryPower->canDischarge = true;
    m_batteryPower->canGridCharge = false;
    m_batteryPower->powerSystem = 100;
    m_batteryPower->powerLoad = 50;

    // Try to charge
    m_batteryPower->powerBatteryDC = -50 * m_batteryPower->singlePointEfficiencyACToDC;
    m_batteryPowerFlow->calculate();

    EXPECT_NEAR(m_batteryPower->powerBatteryAC, -50, error);
    EXPECT_NEAR(m_batteryPower->powerSystemToLoad, 50, error);
    EXPECT_NEAR(m_batteryPower->powerSystemToBattery, 50, error);
    EXPECT_NEAR(m_batteryPower->powerGridToBattery, 0, error);
    EXPECT_NEAR(m_batteryPower->powerGridToLoad, 0, error);
    EXPECT_NEAR(m_batteryPower->powerSystemToGrid, 0, error);
    EXPECT_NEAR(m_batteryPower->powerBatteryToLoad, 0, error);
    EXPECT_NEAR(m_batteryPower->powerConversionLoss, 2.0, error);
    EXPECT_NEAR(m_batteryPower->powerSystemLoss, 0.0, error);

    double gen = m_batteryPower->powerSystem + m_batteryPower->powerBatteryAC;
    EXPECT_NEAR(m_batteryPower->powerGeneratedBySystem, gen, error);
    EXPECT_NEAR(m_batteryPower->powerLoad, 50, error);

    // Try to charge more than is available from PV, disallowed
    m_batteryPower->powerBatteryDC = -100;
    m_batteryPowerFlow->calculate();

    EXPECT_NEAR(m_batteryPower->powerBatteryAC, -50, error);
    EXPECT_NEAR(m_batteryPower->powerSystemToLoad, 50, error);
    EXPECT_NEAR(m_batteryPower->powerSystemToBattery, 50, error);
    EXPECT_NEAR(m_batteryPower->powerGridToBattery, 0, error);
    EXPECT_NEAR(m_batteryPower->powerGridToLoad, 0, error);
    EXPECT_NEAR(m_batteryPower->powerSystemToGrid, 0, error);
    EXPECT_NEAR(m_batteryPower->powerBatteryToLoad, 0, error);
    EXPECT_NEAR(m_batteryPower->powerConversionLoss, 2.0, error);

    gen = m_batteryPower->powerSystem + m_batteryPower->powerBatteryAC;
    EXPECT_NEAR(m_batteryPower->powerGeneratedBySystem, gen, error);
    EXPECT_NEAR(m_batteryPower->powerLoad, 50, error);
    EXPECT_NEAR(m_batteryPower->powerSystemLoss, 0.0, error);

    // Try to discharge
    m_batteryPower->powerBatteryDC = 50;
    m_batteryPowerFlow->calculate();

    EXPECT_NEAR(m_batteryPower->powerBatteryAC, 48, error);
    EXPECT_NEAR(m_batteryPower->powerSystemToLoad, 50, error);
    EXPECT_NEAR(m_batteryPower->powerSystemToBattery, 0, error);
    EXPECT_NEAR(m_batteryPower->powerGridToBattery, 0, error);
    EXPECT_NEAR(m_batteryPower->powerGridToLoad, 0, error);
    EXPECT_NEAR(m_batteryPower->powerSystemToGrid, 50, error);
    EXPECT_NEAR(m_batteryPower->powerBatteryToLoad, 0, error);
    EXPECT_NEAR(m_batteryPower->powerConversionLoss, 2.0, error);
    EXPECT_NEAR(m_batteryPower->powerSystemLoss, 0.0, error);

    gen = m_batteryPower->powerSystem + m_batteryPower->powerBatteryAC;
    EXPECT_NEAR(m_batteryPower->powerGeneratedBySystem, gen, error);
    EXPECT_NEAR(m_batteryPower->powerLoad, 50, error);

    // Redo the above tests, with losses Try to charge
    m_batteryPower->powerBatteryDC = -48 * m_batteryPower->singlePointEfficiencyACToDC;
    m_batteryPower->powerSystemLoss = 2.0;
    m_batteryPowerFlow->calculate();

    EXPECT_NEAR(m_batteryPower->powerBatteryAC, -48, error);
    EXPECT_NEAR(m_batteryPower->powerSystemToLoad, 50, error);
    EXPECT_NEAR(m_batteryPower->powerSystemToBattery, 48, error);
    EXPECT_NEAR(m_batteryPower->powerGridToBattery, 0, error);
    EXPECT_NEAR(m_batteryPower->powerGridToLoad, 0.0, error);
    EXPECT_NEAR(m_batteryPower->powerSystemToGrid, 0, error);
    EXPECT_NEAR(m_batteryPower->powerBatteryToLoad, 0, error);
    EXPECT_NEAR(m_batteryPower->powerConversionLoss, 1.92, error);
    EXPECT_NEAR(m_batteryPower->powerSystemLoss, 2.0, error);

    gen = m_batteryPower->powerSystem + m_batteryPower->powerBatteryAC - m_batteryPower->powerSystemLoss;
    EXPECT_NEAR(m_batteryPower->powerGeneratedBySystem, gen, error);
    EXPECT_NEAR(m_batteryPower->powerLoad, 50, error);

    // Try to charge more than is available from PV, disallowed
    m_batteryPower->powerBatteryDC = -100;
    m_batteryPowerFlow->calculate();

    EXPECT_NEAR(m_batteryPower->powerBatteryAC, -48, error);
    EXPECT_NEAR(m_batteryPower->powerSystemToLoad, 50, error);
    EXPECT_NEAR(m_batteryPower->powerSystemToBattery, 48, error);
    EXPECT_NEAR(m_batteryPower->powerGridToBattery, 0, error);
    EXPECT_NEAR(m_batteryPower->powerGridToLoad, 0, error);
    EXPECT_NEAR(m_batteryPower->powerSystemToGrid, 0, error);
    EXPECT_NEAR(m_batteryPower->powerBatteryToLoad, 0, error);
    EXPECT_NEAR(m_batteryPower->powerConversionLoss, 1.92, error);
    EXPECT_NEAR(m_batteryPower->powerSystemLoss, 2.0, error);

    gen = m_batteryPower->powerSystem + m_batteryPower->powerBatteryAC - m_batteryPower->powerSystemLoss;
    EXPECT_NEAR(m_batteryPower->powerGeneratedBySystem, gen, error);
    EXPECT_NEAR(m_batteryPower->powerLoad, 50, error);
    EXPECT_NEAR(m_batteryPower->powerSystemLoss, 2.0, error);

    // Try to discharge
    m_batteryPower->powerBatteryDC = 50;
    m_batteryPowerFlow->calculate();

    EXPECT_NEAR(m_batteryPower->powerBatteryAC, 48, error);
    EXPECT_NEAR(m_batteryPower->powerSystemToLoad, 50, error);
    EXPECT_NEAR(m_batteryPower->powerSystemToBattery, 0, error);
    EXPECT_NEAR(m_batteryPower->powerGridToBattery, 0, error);
    EXPECT_NEAR(m_batteryPower->powerGridToLoad, 0, error);
    EXPECT_NEAR(m_batteryPower->powerSystemToGrid, 50, error);
    EXPECT_NEAR(m_batteryPower->powerBatteryToLoad, 0, error);
    EXPECT_NEAR(m_batteryPower->powerConversionLoss, 2.0, error);
    EXPECT_NEAR(m_batteryPower->powerSystemLoss, 2.0, error);

    gen = m_batteryPower->powerSystem + m_batteryPower->powerBatteryAC - m_batteryPower->powerSystemLoss;
    EXPECT_NEAR(m_batteryPower->powerGeneratedBySystem, gen, error);
    EXPECT_NEAR(m_batteryPower->powerLoad, 50, error);

}

// Not enough PV for load
TEST_F(BatteryPowerFlowTest_lib_battery_powerflow, AC_PVCharging_ExcessLoad) {
    m_batteryPower->connectionMode = ChargeController::AC_CONNECTED;

    m_batteryPower->canSystemCharge = true;
    m_batteryPower->canDischarge = true;
    m_batteryPower->canGridCharge = false;
    m_batteryPower->powerSystem = 25;
    m_batteryPower->powerLoad = 50;

    // do not discharge battery
    m_batteryPower->powerBatteryDC = 0;
    m_batteryPowerFlow->calculate();

    EXPECT_NEAR(m_batteryPower->powerBatteryAC, 0, error);
    EXPECT_NEAR(m_batteryPower->powerSystemToLoad, 25, error);
    EXPECT_NEAR(m_batteryPower->powerSystemToBattery, 0, error);
    EXPECT_NEAR(m_batteryPower->powerGridToBattery, 0, error);
    EXPECT_NEAR(m_batteryPower->powerGridToLoad, 25, error);
    EXPECT_NEAR(m_batteryPower->powerSystemToGrid, 0, error);
    EXPECT_NEAR(m_batteryPower->powerBatteryToLoad, 0, error);
    EXPECT_NEAR(m_batteryPower->powerConversionLoss, 0, error);
    EXPECT_NEAR(m_batteryPower->powerSystemLoss, 0.0, error);

    double gen = m_batteryPower->powerSystem + m_batteryPower->powerBatteryAC;
    EXPECT_NEAR(m_batteryPower->powerGeneratedBySystem, gen, error);
    EXPECT_NEAR(m_batteryPower->powerLoad, 50, error);

    // discharge battery
    m_batteryPower->powerBatteryDC = 20;
    m_batteryPowerFlow->calculate();

    EXPECT_NEAR(m_batteryPower->powerBatteryAC, 19.19, error);
    EXPECT_NEAR(m_batteryPower->powerSystemToLoad, 25, error);
    EXPECT_NEAR(m_batteryPower->powerSystemToBattery, 0, error);
    EXPECT_NEAR(m_batteryPower->powerGridToBattery, 0, error);
    EXPECT_NEAR(m_batteryPower->powerGridToLoad, 5.8, error);
    EXPECT_NEAR(m_batteryPower->powerSystemToGrid, 0, error);
    EXPECT_NEAR(m_batteryPower->powerBatteryToLoad, 19.19, error);
    EXPECT_NEAR(m_batteryPower->powerConversionLoss, 0.8, error);
    EXPECT_NEAR(m_batteryPower->powerSystemLoss, 0.0, error);

    gen = m_batteryPower->powerSystem + m_batteryPower->powerBatteryAC;
    EXPECT_NEAR(m_batteryPower->powerGeneratedBySystem, gen, error);
    EXPECT_NEAR(m_batteryPower->powerLoad, 50, error);

    // try to charge battery from grid, not allowed
    m_batteryPower->powerBatteryDC = -20;
    m_batteryPowerFlow->calculate();

    EXPECT_NEAR(m_batteryPower->powerBatteryAC, 0, error);
    EXPECT_NEAR(m_batteryPower->powerSystemToLoad, 25, error);
    EXPECT_NEAR(m_batteryPower->powerSystemToBattery, 0, error);
    EXPECT_NEAR(m_batteryPower->powerGridToBattery, 0, error);
    EXPECT_NEAR(m_batteryPower->powerGridToLoad, 25, error);
    EXPECT_NEAR(m_batteryPower->powerSystemToGrid, 0, error);
    EXPECT_NEAR(m_batteryPower->powerBatteryToLoad, 0, error);
    EXPECT_NEAR(m_batteryPower->powerConversionLoss, 0, error);
    EXPECT_NEAR(m_batteryPower->powerSystemLoss, 0.0, error);

    gen = m_batteryPower->powerSystem + m_batteryPower->powerBatteryAC;
    EXPECT_NEAR(m_batteryPower->powerGeneratedBySystem, gen, error);
    EXPECT_NEAR(m_batteryPower->powerLoad, 50, error);

    // Redo the above tests with system losses: do not discharge battery
    m_batteryPower->powerBatteryDC = 0;
    m_batteryPower->powerSystemLoss = 0.5; // Idle loss
    m_batteryPowerFlow->calculate();

    EXPECT_NEAR(m_batteryPower->powerBatteryAC, 0, error);
    EXPECT_NEAR(m_batteryPower->powerSystemToLoad, 25, error);
    EXPECT_NEAR(m_batteryPower->powerSystemToBattery, 0, error);
    EXPECT_NEAR(m_batteryPower->powerGridToBattery, 0, error);
    EXPECT_NEAR(m_batteryPower->powerGridToLoad, 25, error);
    EXPECT_NEAR(m_batteryPower->powerSystemToGrid, 0, error);
    EXPECT_NEAR(m_batteryPower->powerBatteryToLoad, 0, error);
    EXPECT_NEAR(m_batteryPower->powerConversionLoss, 0, error);
    EXPECT_NEAR(m_batteryPower->powerSystemLoss, 0.5, error);

    gen = m_batteryPower->powerSystem + m_batteryPower->powerBatteryAC - m_batteryPower->powerSystemLoss;
    EXPECT_NEAR(m_batteryPower->powerGeneratedBySystem, gen, error);
    EXPECT_NEAR(m_batteryPower->powerLoad, 50, error);

    // discharge battery
    m_batteryPower->powerBatteryDC = 20;
    m_batteryPower->powerSystemLoss = 1.0; // Charging or discharging loss
    m_batteryPowerFlow->calculate();

    EXPECT_NEAR(m_batteryPower->powerBatteryAC, 19.19, error);
    EXPECT_NEAR(m_batteryPower->powerSystemToLoad, 25, error);
    EXPECT_NEAR(m_batteryPower->powerSystemToBattery, 0, error);
    EXPECT_NEAR(m_batteryPower->powerGridToBattery, 0, error);
    EXPECT_NEAR(m_batteryPower->powerGridToLoad, 6.8, error);
    EXPECT_NEAR(m_batteryPower->powerSystemToGrid, 0, error);
    EXPECT_NEAR(m_batteryPower->powerBatteryToLoad, 18.19, error);
    EXPECT_NEAR(m_batteryPower->powerConversionLoss, 0.8, error);
    EXPECT_NEAR(m_batteryPower->powerSystemLoss, 1.0, error);

    gen = m_batteryPower->powerSystem + m_batteryPower->powerBatteryAC - m_batteryPower->powerSystemLoss;
    EXPECT_NEAR(m_batteryPower->powerGeneratedBySystem, gen, error);
    EXPECT_NEAR(m_batteryPower->powerLoad, 50, error);

    // try to charge battery from grid, not allowed
    m_batteryPower->powerBatteryDC = -20;
    m_batteryPowerFlow->calculate();

    EXPECT_NEAR(m_batteryPower->powerBatteryAC, 0, error);
    EXPECT_NEAR(m_batteryPower->powerSystemToLoad, 25, error);
    EXPECT_NEAR(m_batteryPower->powerSystemToBattery, 0, error);
    EXPECT_NEAR(m_batteryPower->powerGridToBattery, 0, error);
    EXPECT_NEAR(m_batteryPower->powerGridToLoad, 25, error);
    EXPECT_NEAR(m_batteryPower->powerSystemToGrid, 0, error);
    EXPECT_NEAR(m_batteryPower->powerBatteryToLoad, 0, error);
    EXPECT_NEAR(m_batteryPower->powerConversionLoss, 0, error);
    EXPECT_NEAR(m_batteryPower->powerSystemLoss, 1.0, error);

    gen = m_batteryPower->powerSystem + m_batteryPower->powerBatteryAC - m_batteryPower->powerSystemLoss;
    EXPECT_NEAR(m_batteryPower->powerGeneratedBySystem, gen, error);
    EXPECT_NEAR(m_batteryPower->powerLoad, 50, error);
}

// Excess PV production
TEST_F(BatteryPowerFlowTest_lib_battery_powerflow, AC_GridCharging_ExcessPV) {
    m_batteryPower->connectionMode = ChargeController::AC_CONNECTED;

    m_batteryPower->canGridCharge = true;
    m_batteryPower->canDischarge = true;
    m_batteryPower->canSystemCharge = false;
    m_batteryPower->powerSystem = 100;
    m_batteryPower->powerLoad = 50;

    // charging will be from grid
    m_batteryPower->powerBatteryDC = -50;
    m_batteryPowerFlow->calculate();

    EXPECT_NEAR(m_batteryPower->powerBatteryAC, -52.08, error);
    EXPECT_NEAR(m_batteryPower->powerSystemToLoad, 50, error);
    EXPECT_NEAR(m_batteryPower->powerSystemToBattery, 0, error);
    EXPECT_NEAR(m_batteryPower->powerSystemToGrid, 50, error);
    EXPECT_NEAR(m_batteryPower->powerGridToBattery, 52.08, error);
    EXPECT_NEAR(m_batteryPower->powerGridToLoad, 0, error);
    EXPECT_NEAR(m_batteryPower->powerSystemToGrid, 50, error);
    EXPECT_NEAR(m_batteryPower->powerBatteryToLoad, 0, error);
    EXPECT_NEAR(m_batteryPower->powerConversionLoss, 2.08, error);
    EXPECT_NEAR(m_batteryPower->powerSystemLoss, 0.0, error);

    double gen = m_batteryPower->powerSystem + m_batteryPower->powerBatteryAC;
    EXPECT_NEAR(m_batteryPower->powerGeneratedBySystem, gen, error);
    EXPECT_NEAR(m_batteryPower->powerLoad, 50, error);

    // discharge to grid
    m_batteryPower->powerBatteryDC = 20;
    m_batteryPowerFlow->calculate();

    EXPECT_NEAR(m_batteryPower->powerBatteryAC, 19.2, error);
    EXPECT_NEAR(m_batteryPower->powerSystemToLoad, 50, error);
    EXPECT_NEAR(m_batteryPower->powerSystemToBattery, 0, error);
    EXPECT_NEAR(m_batteryPower->powerSystemToGrid, 50, error);
    EXPECT_NEAR(m_batteryPower->powerGridToBattery, 0, error);
    EXPECT_NEAR(m_batteryPower->powerGridToLoad, 0, error);
    EXPECT_NEAR(m_batteryPower->powerSystemToGrid, 50, error);
    EXPECT_NEAR(m_batteryPower->powerBatteryToLoad, 0, error);
    EXPECT_NEAR(m_batteryPower->powerBatteryToGrid, 19.2, error);
    EXPECT_NEAR(m_batteryPower->powerConversionLoss, 0.80, error);
    EXPECT_NEAR(m_batteryPower->powerSystemLoss, 0.0, error);

    gen = m_batteryPower->powerSystem + m_batteryPower->powerBatteryAC;
    EXPECT_NEAR(m_batteryPower->powerGeneratedBySystem, gen, error);
    EXPECT_NEAR(m_batteryPower->powerLoad, 50, error);

    // charging will be from grid
    m_batteryPower->powerBatteryDC = -50;
    m_batteryPower->powerSystemLoss = 2.0;
    m_batteryPowerFlow->calculate();

    EXPECT_NEAR(m_batteryPower->powerBatteryAC, -52.08, error);
    EXPECT_NEAR(m_batteryPower->powerSystemToLoad, 50, error);
    EXPECT_NEAR(m_batteryPower->powerSystemToBattery, 0, error);
    EXPECT_NEAR(m_batteryPower->powerSystemToGrid, 50, error);
    EXPECT_NEAR(m_batteryPower->powerGridToBattery, 52.08, error);
    EXPECT_NEAR(m_batteryPower->powerGridToLoad, 0, error);
    EXPECT_NEAR(m_batteryPower->powerSystemToGrid, 50, error);
    EXPECT_NEAR(m_batteryPower->powerBatteryToLoad, 0, error);
    EXPECT_NEAR(m_batteryPower->powerConversionLoss, 2.08, error);
    EXPECT_NEAR(m_batteryPower->powerSystemLoss, 2.0, error);

    gen = m_batteryPower->powerSystem + m_batteryPower->powerBatteryAC - m_batteryPower->powerSystemLoss;
    EXPECT_NEAR(m_batteryPower->powerGeneratedBySystem, gen, error);
    EXPECT_NEAR(m_batteryPower->powerLoad, 50, error);

    // discharge to grid
    m_batteryPower->powerBatteryDC = 20;
    m_batteryPowerFlow->calculate();

    EXPECT_NEAR(m_batteryPower->powerBatteryAC, 19.2, error);
    EXPECT_NEAR(m_batteryPower->powerSystemToLoad, 50, error);
    EXPECT_NEAR(m_batteryPower->powerSystemToBattery, 0, error);
    EXPECT_NEAR(m_batteryPower->powerSystemToGrid, 50, error);
    EXPECT_NEAR(m_batteryPower->powerGridToBattery, 0, error);
    EXPECT_NEAR(m_batteryPower->powerGridToLoad, 0, error);
    EXPECT_NEAR(m_batteryPower->powerSystemToGrid, 50, error);
    EXPECT_NEAR(m_batteryPower->powerBatteryToLoad, 0, error);
    EXPECT_NEAR(m_batteryPower->powerBatteryToGrid, 17.2, error);
    EXPECT_NEAR(m_batteryPower->powerConversionLoss, 0.80, error);
    EXPECT_NEAR(m_batteryPower->powerSystemLoss, 2.0, error);

    gen = m_batteryPower->powerSystem + m_batteryPower->powerBatteryAC - m_batteryPower->powerSystemLoss;
    EXPECT_NEAR(m_batteryPower->powerGeneratedBySystem, gen, error);
    EXPECT_NEAR(m_batteryPower->powerLoad, 50, error);
}

// Not enough PV, pull from grid
TEST_F(BatteryPowerFlowTest_lib_battery_powerflow, AC_GridCharging_ExcessLoad)
{
    m_batteryPower->connectionMode = ChargeController::AC_CONNECTED;

    m_batteryPower->canGridCharge = true;
    m_batteryPower->canDischarge = true;
    m_batteryPower->canSystemCharge = false;
    m_batteryPower->powerSystem = 10;
    m_batteryPower->powerLoad = 50;

    // don't dispatch
    m_batteryPower->powerBatteryDC = 0;
    m_batteryPowerFlow->calculate();

    EXPECT_NEAR(m_batteryPower->powerBatteryAC, 0, error);
    EXPECT_NEAR(m_batteryPower->powerSystemToLoad, 10, error);
    EXPECT_NEAR(m_batteryPower->powerSystemToBattery, 0, error);
    EXPECT_NEAR(m_batteryPower->powerSystemToGrid, 0, error);
    EXPECT_NEAR(m_batteryPower->powerGridToBattery, 0, error);
    EXPECT_NEAR(m_batteryPower->powerGridToLoad, 40, error);
    EXPECT_NEAR(m_batteryPower->powerSystemToGrid, 0, error);
    EXPECT_NEAR(m_batteryPower->powerBatteryToLoad, 0, error);
    EXPECT_NEAR(m_batteryPower->powerBatteryToGrid, 0, error);
    EXPECT_NEAR(m_batteryPower->powerConversionLoss, 0, error);
    EXPECT_NEAR(m_batteryPower->powerSystemLoss, 0.0, error);

    double gen = m_batteryPower->powerSystem + m_batteryPower->powerBatteryAC;
    EXPECT_NEAR(m_batteryPower->powerGeneratedBySystem, gen, error);
    EXPECT_NEAR(m_batteryPower->powerLoad, 50, error);

    // charging will happen from grid
    m_batteryPower->powerBatteryDC = -20;
    m_batteryPowerFlow->calculate();

    EXPECT_NEAR(m_batteryPower->powerBatteryAC, -20.83, error);
    EXPECT_NEAR(m_batteryPower->powerSystemToLoad, 10, error);
    EXPECT_NEAR(m_batteryPower->powerSystemToBattery, 0, error);
    EXPECT_NEAR(m_batteryPower->powerSystemToGrid, 0, error);
    EXPECT_NEAR(m_batteryPower->powerGridToBattery, 20.83, error);
    EXPECT_NEAR(m_batteryPower->powerGridToLoad, 40, error);
    EXPECT_NEAR(m_batteryPower->powerSystemToGrid, 0, error);
    EXPECT_NEAR(m_batteryPower->powerBatteryToLoad, 0, error);
    EXPECT_NEAR(m_batteryPower->powerBatteryToGrid, 0, error);
    EXPECT_NEAR(m_batteryPower->powerConversionLoss, 0.83, error);
    EXPECT_NEAR(m_batteryPower->powerSystemLoss, 0.0, error);

    gen = m_batteryPower->powerSystem + m_batteryPower->powerBatteryAC;
    EXPECT_NEAR(m_batteryPower->powerGeneratedBySystem, gen, error);
    EXPECT_NEAR(m_batteryPower->powerLoad, 50, error);

    // discharge
    m_batteryPower->powerBatteryDC = 20;
    m_batteryPowerFlow->calculate();

    EXPECT_NEAR(m_batteryPower->powerBatteryAC, 19.19, error);
    EXPECT_NEAR(m_batteryPower->powerSystemToLoad, 10, error);
    EXPECT_NEAR(m_batteryPower->powerSystemToBattery, 0, error);
    EXPECT_NEAR(m_batteryPower->powerSystemToGrid, 0, error);
    EXPECT_NEAR(m_batteryPower->powerGridToBattery, 0, error);
    EXPECT_NEAR(m_batteryPower->powerGridToLoad, 20.8, error);
    EXPECT_NEAR(m_batteryPower->powerSystemToGrid, 0, error);
    EXPECT_NEAR(m_batteryPower->powerBatteryToLoad, 19.19, error);
    EXPECT_NEAR(m_batteryPower->powerBatteryToGrid, 0, error);
    EXPECT_NEAR(m_batteryPower->powerConversionLoss, 0.8, error);
    EXPECT_NEAR(m_batteryPower->powerSystemLoss, 0.0, error);

    gen = m_batteryPower->powerSystem + m_batteryPower->powerBatteryAC;
    EXPECT_NEAR(m_batteryPower->powerGeneratedBySystem, gen, error);
    EXPECT_NEAR(m_batteryPower->powerLoad, 50, error);

    // don't dispatch
    m_batteryPower->powerBatteryDC = 0;
    m_batteryPower->powerSystemLoss = 0.5; // Idle loss
    m_batteryPowerFlow->calculate();

    EXPECT_NEAR(m_batteryPower->powerBatteryAC, 0, error);
    EXPECT_NEAR(m_batteryPower->powerSystemToLoad, 10.0, error);
    EXPECT_NEAR(m_batteryPower->powerSystemToBattery, 0, error);
    EXPECT_NEAR(m_batteryPower->powerSystemToGrid, 0, error);
    EXPECT_NEAR(m_batteryPower->powerGridToBattery, 0, error);
    EXPECT_NEAR(m_batteryPower->powerGridToLoad, 40, error);
    EXPECT_NEAR(m_batteryPower->powerSystemToGrid, 0, error);
    EXPECT_NEAR(m_batteryPower->powerBatteryToLoad, 0, error);
    EXPECT_NEAR(m_batteryPower->powerBatteryToGrid, 0, error);
    EXPECT_NEAR(m_batteryPower->powerConversionLoss, 0, error);
    EXPECT_NEAR(m_batteryPower->powerSystemLoss, 0.5, error);

    gen = m_batteryPower->powerSystem + m_batteryPower->powerBatteryAC - m_batteryPower->powerSystemLoss;
    EXPECT_NEAR(m_batteryPower->powerGeneratedBySystem, gen, error);
    EXPECT_NEAR(m_batteryPower->powerLoad, 50, error);

    // charging will happen from grid
    m_batteryPower->powerBatteryDC = -20;
    m_batteryPower->powerSystemLoss = 1.0; // Charging or discharging loss
    m_batteryPowerFlow->calculate();

    EXPECT_NEAR(m_batteryPower->powerBatteryAC, -20.83, error);
    EXPECT_NEAR(m_batteryPower->powerSystemToLoad, 10, error);
    EXPECT_NEAR(m_batteryPower->powerSystemToBattery, 0, error);
    EXPECT_NEAR(m_batteryPower->powerSystemToGrid, 0, error);
    EXPECT_NEAR(m_batteryPower->powerGridToBattery, 20.83, error);
    EXPECT_NEAR(m_batteryPower->powerGridToLoad, 40, error);
    EXPECT_NEAR(m_batteryPower->powerSystemToGrid, 0, error);
    EXPECT_NEAR(m_batteryPower->powerBatteryToLoad, 0, error);
    EXPECT_NEAR(m_batteryPower->powerBatteryToGrid, 0, error);
    EXPECT_NEAR(m_batteryPower->powerConversionLoss, 0.83, error);
    EXPECT_NEAR(m_batteryPower->powerSystemLoss, 1.0, error);

    gen = m_batteryPower->powerSystem + m_batteryPower->powerBatteryAC - m_batteryPower->powerSystemLoss;
    EXPECT_NEAR(m_batteryPower->powerGeneratedBySystem, gen, error);
    EXPECT_NEAR(m_batteryPower->powerLoad, 50, error);

    // discharge
    m_batteryPower->powerBatteryDC = 20;
    m_batteryPowerFlow->calculate();

    EXPECT_NEAR(m_batteryPower->powerBatteryAC, 19.19, error);
    EXPECT_NEAR(m_batteryPower->powerSystemToLoad, 10, error);
    EXPECT_NEAR(m_batteryPower->powerSystemToBattery, 0, error);
    EXPECT_NEAR(m_batteryPower->powerSystemToGrid, 0, error);
    EXPECT_NEAR(m_batteryPower->powerGridToBattery, 0, error);
    EXPECT_NEAR(m_batteryPower->powerGridToLoad, 21.8, error);
    EXPECT_NEAR(m_batteryPower->powerSystemToGrid, 0, error);
    EXPECT_NEAR(m_batteryPower->powerBatteryToLoad, 18.19, error);
    EXPECT_NEAR(m_batteryPower->powerBatteryToGrid, 0, error);
    EXPECT_NEAR(m_batteryPower->powerConversionLoss, 0.8, error);
    EXPECT_NEAR(m_batteryPower->powerSystemLoss, 1.0, error);

    gen = m_batteryPower->powerSystem + m_batteryPower->powerBatteryAC - m_batteryPower->powerSystemLoss;
    EXPECT_NEAR(m_batteryPower->powerGeneratedBySystem, gen, error);
    EXPECT_NEAR(m_batteryPower->powerLoad, 50, error);
}

// Excess PV production
TEST_F(BatteryPowerFlowTest_lib_battery_powerflow, AC_GridPVCharging_ExcessPV) {
    m_batteryPower->connectionMode = ChargeController::AC_CONNECTED;

    m_batteryPower->canGridCharge = true;
    m_batteryPower->canDischarge = true;
    m_batteryPower->canSystemCharge = true;
    m_batteryPower->powerSystem = 100;
    m_batteryPower->powerLoad = 50;

    // charging will be from PV
    m_batteryPower->powerBatteryDC = -40;
    m_batteryPowerFlow->calculate();

    EXPECT_NEAR(m_batteryPower->powerBatteryAC, -41.66, error);
    EXPECT_NEAR(m_batteryPower->powerSystemToLoad, 50, error);
    EXPECT_NEAR(m_batteryPower->powerSystemToBattery, 41.66, error);
    EXPECT_NEAR(m_batteryPower->powerSystemToGrid, 8.33, error);
    EXPECT_NEAR(m_batteryPower->powerGridToBattery, 0, error);
    EXPECT_NEAR(m_batteryPower->powerGridToLoad, 0, error);
    EXPECT_NEAR(m_batteryPower->powerSystemToGrid, 8.33, error);
    EXPECT_NEAR(m_batteryPower->powerBatteryToLoad, 0, error);
    EXPECT_NEAR(m_batteryPower->powerConversionLoss, 1.66, error);
    EXPECT_NEAR(m_batteryPower->powerSystemLoss, 0.0, error);

    double gen = m_batteryPower->powerSystem + m_batteryPower->powerBatteryAC;
    EXPECT_NEAR(m_batteryPower->powerGeneratedBySystem, gen, error);
    EXPECT_NEAR(m_batteryPower->powerLoad, 50, error);

    // charging will be from both PV and grid
    m_batteryPower->powerBatteryDC = -100;
    m_batteryPowerFlow->calculate();

    EXPECT_NEAR(m_batteryPower->powerBatteryAC,  -104.16, error);
    EXPECT_NEAR(m_batteryPower->powerSystemToLoad, 50, error);
    EXPECT_NEAR(m_batteryPower->powerSystemToBattery, 50, error);
    EXPECT_NEAR(m_batteryPower->powerSystemToGrid, 0, error);
    EXPECT_NEAR(m_batteryPower->powerGridToBattery, 54.16, error);
    EXPECT_NEAR(m_batteryPower->powerGridToLoad, 0, error);
    EXPECT_NEAR(m_batteryPower->powerSystemToGrid, 0, error);
    EXPECT_NEAR(m_batteryPower->powerBatteryToLoad, 0, error);
    EXPECT_NEAR(m_batteryPower->powerConversionLoss, 4.16, error);
    EXPECT_NEAR(m_batteryPower->powerSystemLoss, 0.0, error);

    gen = m_batteryPower->powerSystem + m_batteryPower->powerBatteryAC;
    EXPECT_NEAR(m_batteryPower->powerGeneratedBySystem, gen, error);
    EXPECT_NEAR(m_batteryPower->powerLoad, 50, error);

    // discharge to grid
    m_batteryPower->powerBatteryDC = 20;
    m_batteryPowerFlow->calculate();

    EXPECT_NEAR(m_batteryPower->powerBatteryAC, 19.2, error);
    EXPECT_NEAR(m_batteryPower->powerSystemToLoad, 50, error);
    EXPECT_NEAR(m_batteryPower->powerSystemToBattery, 0, error);
    EXPECT_NEAR(m_batteryPower->powerSystemToGrid, 50, error);
    EXPECT_NEAR(m_batteryPower->powerGridToBattery, 0, error);
    EXPECT_NEAR(m_batteryPower->powerGridToLoad, 0, error);
    EXPECT_NEAR(m_batteryPower->powerSystemToGrid, 50, error);
    EXPECT_NEAR(m_batteryPower->powerBatteryToLoad, 0, error);
    EXPECT_NEAR(m_batteryPower->powerBatteryToGrid, 19.2, error);
    EXPECT_NEAR(m_batteryPower->powerConversionLoss, 0.80, error);
    EXPECT_NEAR(m_batteryPower->powerSystemLoss, 0.0, error);

    gen = m_batteryPower->powerSystem + m_batteryPower->powerBatteryAC;
    EXPECT_NEAR(m_batteryPower->powerGeneratedBySystem, gen, error);
    EXPECT_NEAR(m_batteryPower->powerLoad, 50, error);

    // charging will be from PV, with losses
    m_batteryPower->powerBatteryDC = -40;
    m_batteryPower->powerSystemLoss = 1.0;
    m_batteryPowerFlow->calculate();

    EXPECT_NEAR(m_batteryPower->powerBatteryAC, -41.66, error);
    EXPECT_NEAR(m_batteryPower->powerSystemToLoad, 50, error);
    EXPECT_NEAR(m_batteryPower->powerSystemToBattery, 41.66, error);
    EXPECT_NEAR(m_batteryPower->powerSystemToGrid, 7.33, error);
    EXPECT_NEAR(m_batteryPower->powerGridToBattery, 0, error);
    EXPECT_NEAR(m_batteryPower->powerGridToLoad, 0, error);
    EXPECT_NEAR(m_batteryPower->powerBatteryToLoad, 0, error);
    EXPECT_NEAR(m_batteryPower->powerConversionLoss, 1.66, error);
    EXPECT_NEAR(m_batteryPower->powerSystemLoss, 1.0, error);

    gen = m_batteryPower->powerSystem + m_batteryPower->powerBatteryAC - m_batteryPower->powerSystemLoss;
    EXPECT_NEAR(m_batteryPower->powerGeneratedBySystem, gen, error);
    EXPECT_NEAR(m_batteryPower->powerLoad, 50, error);

    // charging will be from both PV and grid
    m_batteryPower->powerBatteryDC = -100;
    m_batteryPowerFlow->calculate();

    EXPECT_NEAR(m_batteryPower->powerBatteryAC, -104.16, error);
    EXPECT_NEAR(m_batteryPower->powerSystemToLoad, 50, error);
    EXPECT_NEAR(m_batteryPower->powerSystemToBattery, 49, error);
    EXPECT_NEAR(m_batteryPower->powerSystemToGrid, 0, error);
    EXPECT_NEAR(m_batteryPower->powerGridToBattery, 55.16, error);
    EXPECT_NEAR(m_batteryPower->powerGridToLoad, 0, error);
    EXPECT_NEAR(m_batteryPower->powerSystemToGrid, 0, error);
    EXPECT_NEAR(m_batteryPower->powerBatteryToLoad, 0, error);
    EXPECT_NEAR(m_batteryPower->powerConversionLoss, 4.16, error);
    EXPECT_NEAR(m_batteryPower->powerSystemLoss, 1.0, error);

    gen = m_batteryPower->powerSystem + m_batteryPower->powerBatteryAC - m_batteryPower->powerSystemLoss;
    EXPECT_NEAR(m_batteryPower->powerGeneratedBySystem, gen, error);
    EXPECT_NEAR(m_batteryPower->powerLoad, 50, error);

    // discharge to grid
    m_batteryPower->powerBatteryDC = 20;
    m_batteryPowerFlow->calculate();

    EXPECT_NEAR(m_batteryPower->powerBatteryAC, 19.2, error);
    EXPECT_NEAR(m_batteryPower->powerSystemToLoad, 50, error);
    EXPECT_NEAR(m_batteryPower->powerSystemToBattery, 0, error);
    EXPECT_NEAR(m_batteryPower->powerSystemToGrid, 50, error);
    EXPECT_NEAR(m_batteryPower->powerGridToBattery, 0, error);
    EXPECT_NEAR(m_batteryPower->powerGridToLoad, 0, error);
    EXPECT_NEAR(m_batteryPower->powerSystemToGrid, 50, error);
    EXPECT_NEAR(m_batteryPower->powerBatteryToLoad, 0, error);
    EXPECT_NEAR(m_batteryPower->powerBatteryToGrid, 18.2, error);
    EXPECT_NEAR(m_batteryPower->powerConversionLoss, 0.80, error);
    EXPECT_NEAR(m_batteryPower->powerSystemLoss, 1.0, error);

    gen = m_batteryPower->powerSystem + m_batteryPower->powerBatteryAC - m_batteryPower->powerSystemLoss;
    EXPECT_NEAR(m_batteryPower->powerGeneratedBySystem, gen, error);
    EXPECT_NEAR(m_batteryPower->powerLoad, 50, error);
}

// Not enough PV, pull from grid
TEST_F(BatteryPowerFlowTest_lib_battery_powerflow, AC_GridPVCharging_ExcessLoad)
{
    m_batteryPower->connectionMode = ChargeController::AC_CONNECTED;

    m_batteryPower->canGridCharge = true;
    m_batteryPower->canDischarge = true;
    m_batteryPower->canSystemCharge = true;
    m_batteryPower->powerSystem = 10;
    m_batteryPower->powerLoad = 50;

    // don't dispatch
    m_batteryPower->powerBatteryDC = 0;
    m_batteryPowerFlow->calculate();

    EXPECT_NEAR(m_batteryPower->powerBatteryAC, 0, error);
    EXPECT_NEAR(m_batteryPower->powerSystemToLoad, 10, error);
    EXPECT_NEAR(m_batteryPower->powerSystemToBattery, 0, error);
    EXPECT_NEAR(m_batteryPower->powerSystemToGrid, 0, error);
    EXPECT_NEAR(m_batteryPower->powerGridToBattery, 0, error);
    EXPECT_NEAR(m_batteryPower->powerGridToLoad, 40, error);
    EXPECT_NEAR(m_batteryPower->powerSystemToGrid, 0, error);
    EXPECT_NEAR(m_batteryPower->powerBatteryToLoad, 0, error);
    EXPECT_NEAR(m_batteryPower->powerBatteryToGrid, 0, error);
    EXPECT_NEAR(m_batteryPower->powerConversionLoss, 0, error);
    EXPECT_NEAR(m_batteryPower->powerSystemLoss, 0.0, error);

    double gen = m_batteryPower->powerSystem + m_batteryPower->powerBatteryAC - m_batteryPower->powerSystemLoss;
    EXPECT_NEAR(m_batteryPower->powerGeneratedBySystem, gen, error);
    EXPECT_NEAR(m_batteryPower->powerLoad, 50, error);

    // charging will happen from grid since pv first goes to load
    m_batteryPower->powerBatteryDC = -20;
    m_batteryPowerFlow->calculate();

    EXPECT_NEAR(m_batteryPower->powerBatteryAC, -20.83, error);
    EXPECT_NEAR(m_batteryPower->powerSystemToLoad, 10, error);
    EXPECT_NEAR(m_batteryPower->powerSystemToBattery, 0, error);
    EXPECT_NEAR(m_batteryPower->powerSystemToGrid, 0, error);
    EXPECT_NEAR(m_batteryPower->powerGridToBattery, 20.83, error);
    EXPECT_NEAR(m_batteryPower->powerGridToLoad, 40, error);
    EXPECT_NEAR(m_batteryPower->powerSystemToGrid, 0, error);
    EXPECT_NEAR(m_batteryPower->powerBatteryToLoad, 0, error);
    EXPECT_NEAR(m_batteryPower->powerBatteryToGrid, 0, error);
    EXPECT_NEAR(m_batteryPower->powerConversionLoss, 0.83, error);
    EXPECT_NEAR(m_batteryPower->powerSystemLoss, 0.0, error);

    gen = m_batteryPower->powerSystem + m_batteryPower->powerBatteryAC - m_batteryPower->powerSystemLoss;
    EXPECT_NEAR(m_batteryPower->powerGeneratedBySystem, gen, error);
    EXPECT_NEAR(m_batteryPower->powerLoad, 50, error);

    // discharge
    m_batteryPower->powerBatteryDC = 20;
    m_batteryPowerFlow->calculate();

    EXPECT_NEAR(m_batteryPower->powerBatteryAC, 19.19, error);
    EXPECT_NEAR(m_batteryPower->powerSystemToLoad, 10, error);
    EXPECT_NEAR(m_batteryPower->powerSystemToBattery, 0, error);
    EXPECT_NEAR(m_batteryPower->powerSystemToGrid, 0, error);
    EXPECT_NEAR(m_batteryPower->powerGridToBattery, 0, error);
    EXPECT_NEAR(m_batteryPower->powerGridToLoad, 20.8, error);
    EXPECT_NEAR(m_batteryPower->powerSystemToGrid, 0, error);
    EXPECT_NEAR(m_batteryPower->powerBatteryToLoad, 19.19, error);
    EXPECT_NEAR(m_batteryPower->powerBatteryToGrid, 0, error);
    EXPECT_NEAR(m_batteryPower->powerConversionLoss, 0.8, error);
    EXPECT_NEAR(m_batteryPower->powerSystemLoss, 0.0, error);

    gen = m_batteryPower->powerSystem + m_batteryPower->powerBatteryAC - m_batteryPower->powerSystemLoss;
    EXPECT_NEAR(m_batteryPower->powerGeneratedBySystem, gen, error);
    EXPECT_NEAR(m_batteryPower->powerLoad, 50, error);

    // don't dispatch
    m_batteryPower->powerBatteryDC = 0;
    m_batteryPower->powerSystemLoss = 0.5; // Idle loss
    m_batteryPowerFlow->calculate();

    EXPECT_NEAR(m_batteryPower->powerBatteryAC, 0, error);
    EXPECT_NEAR(m_batteryPower->powerSystemToLoad, 10, error);
    EXPECT_NEAR(m_batteryPower->powerSystemToBattery, 0, error);
    EXPECT_NEAR(m_batteryPower->powerSystemToGrid, 0, error);
    EXPECT_NEAR(m_batteryPower->powerGridToBattery, 0, error);
    EXPECT_NEAR(m_batteryPower->powerGridToLoad, 40, error);
    EXPECT_NEAR(m_batteryPower->powerSystemToGrid, 0, error);
    EXPECT_NEAR(m_batteryPower->powerBatteryToLoad, 0, error);
    EXPECT_NEAR(m_batteryPower->powerBatteryToGrid, 0, error);
    EXPECT_NEAR(m_batteryPower->powerConversionLoss, 0, error);
    EXPECT_NEAR(m_batteryPower->powerSystemLoss, 0.5, error);

    gen = m_batteryPower->powerSystem + m_batteryPower->powerBatteryAC - m_batteryPower->powerSystemLoss;
    EXPECT_NEAR(m_batteryPower->powerGeneratedBySystem, gen, error);
    EXPECT_NEAR(m_batteryPower->powerLoad, 50, error);

    // charging will happen from grid since pv first goes to load
    m_batteryPower->powerBatteryDC = -20;
    m_batteryPower->powerSystemLoss = 1.0;
    m_batteryPowerFlow->calculate();

    EXPECT_NEAR(m_batteryPower->powerBatteryAC, -20.83, error);
    EXPECT_NEAR(m_batteryPower->powerSystemToLoad, 10, error);
    EXPECT_NEAR(m_batteryPower->powerSystemToBattery, 0, error);
    EXPECT_NEAR(m_batteryPower->powerSystemToGrid, 0, error);
    EXPECT_NEAR(m_batteryPower->powerGridToBattery, 20.83, error);
    EXPECT_NEAR(m_batteryPower->powerGridToLoad, 40, error);
    EXPECT_NEAR(m_batteryPower->powerSystemToGrid, 0, error);
    EXPECT_NEAR(m_batteryPower->powerBatteryToLoad, 0, error);
    EXPECT_NEAR(m_batteryPower->powerBatteryToGrid, 0, error);
    EXPECT_NEAR(m_batteryPower->powerConversionLoss, 0.83, error);
    EXPECT_NEAR(m_batteryPower->powerSystemLoss, 1.0, error);

    gen = m_batteryPower->powerSystem + m_batteryPower->powerBatteryAC - m_batteryPower->powerSystemLoss;
    EXPECT_NEAR(m_batteryPower->powerGeneratedBySystem, gen, error);
    EXPECT_NEAR(m_batteryPower->powerLoad, 50, error);

    // discharge
    m_batteryPower->powerBatteryDC = 20;
    m_batteryPowerFlow->calculate();

    EXPECT_NEAR(m_batteryPower->powerBatteryAC, 19.19, error);
    EXPECT_NEAR(m_batteryPower->powerSystemToLoad, 10, error);
    EXPECT_NEAR(m_batteryPower->powerSystemToBattery, 0, error);
    EXPECT_NEAR(m_batteryPower->powerSystemToGrid, 0, error);
    EXPECT_NEAR(m_batteryPower->powerGridToBattery, 0, error);
    EXPECT_NEAR(m_batteryPower->powerGridToLoad, 21.8, error);
    EXPECT_NEAR(m_batteryPower->powerSystemToGrid, 0, error);
    EXPECT_NEAR(m_batteryPower->powerBatteryToLoad, 18.19, error);
    EXPECT_NEAR(m_batteryPower->powerBatteryToGrid, 0, error);
    EXPECT_NEAR(m_batteryPower->powerConversionLoss, 0.8, error);
    EXPECT_NEAR(m_batteryPower->powerSystemLoss, 1.0, error);

    gen = m_batteryPower->powerSystem + m_batteryPower->powerBatteryAC - m_batteryPower->powerSystemLoss;
    EXPECT_NEAR(m_batteryPower->powerGeneratedBySystem, gen, error);
    EXPECT_NEAR(m_batteryPower->powerLoad, 50, error);
}

// Excess PV production
TEST_F(BatteryPowerFlowTest_lib_battery_powerflow, DC_PVCharging_ExcessPV) {
    m_batteryPower->connectionMode = ChargeController::DC_CONNECTED;

    m_batteryPower->canSystemCharge = true;
    m_batteryPower->canDischarge = true;
    m_batteryPower->canGridCharge = false;
    m_batteryPower->powerSystem = 100;
    m_batteryPower->powerLoad = 50;

    // Try to charge
    m_batteryPower->powerBatteryDC = -50;
    m_batteryPowerFlow->calculate();

    EXPECT_NEAR(m_batteryPower->powerBatteryAC, -51.02, error);
    EXPECT_NEAR(m_batteryPower->powerSystemToLoad, 46.24, error);
    EXPECT_NEAR(m_batteryPower->powerSystemToBattery, 51.02, error);
    EXPECT_NEAR(m_batteryPower->powerGridToBattery, 0, error);
    EXPECT_NEAR(m_batteryPower->powerGridToLoad, 3.75, error);
    EXPECT_NEAR(m_batteryPower->powerSystemToGrid, 0, error);
    EXPECT_NEAR(m_batteryPower->powerBatteryToLoad, 0, error);
    EXPECT_NEAR(m_batteryPower->powerConversionLoss, 3.75, error);
    EXPECT_NEAR(m_batteryPower->powerSystemLoss, 0.0, error);

    check_net_flows(std::string());

    // Charge battery from PV and meet load from grid
    m_batteryPower->powerBatteryDC = -100;
    m_batteryPowerFlow->calculate();

    EXPECT_NEAR(m_batteryPower->powerBatteryAC, -100, error);
    EXPECT_NEAR(m_batteryPower->powerSystemToLoad, 0, error);
    EXPECT_NEAR(m_batteryPower->powerSystemToBattery, 100, error);
    EXPECT_NEAR(m_batteryPower->powerGridToBattery, 0, error);
    EXPECT_NEAR(m_batteryPower->powerGridToLoad, 50, error);
    EXPECT_NEAR(m_batteryPower->powerSystemToGrid, 0, error);
    EXPECT_NEAR(m_batteryPower->powerBatteryToLoad, 0, error);
    EXPECT_NEAR(m_batteryPower->powerConversionLoss, 2.0, error);
    EXPECT_NEAR(m_batteryPower->powerSystemLoss, 0.0, error);

    check_net_flows(std::string());


    // Try to charge more than available from PV, disallowed
    m_batteryPower->powerBatteryDC = -150;
    m_batteryPowerFlow->calculate();

    EXPECT_NEAR(m_batteryPower->powerBatteryAC, -100, error);
    EXPECT_NEAR(m_batteryPower->powerSystemToLoad, 0, error);
    EXPECT_NEAR(m_batteryPower->powerSystemToBattery, 100, error);
    EXPECT_NEAR(m_batteryPower->powerGridToBattery, 0, error);
    EXPECT_NEAR(m_batteryPower->powerGridToLoad, 50, error);
    EXPECT_NEAR(m_batteryPower->powerSystemToGrid, 0, error);
    EXPECT_NEAR(m_batteryPower->powerBatteryToLoad, 0, error);
    EXPECT_NEAR(m_batteryPower->powerConversionLoss, 2.0, error);
    EXPECT_NEAR(m_batteryPower->powerSystemLoss, 0.0, error);

    check_net_flows(std::string());


    // Try to discharge
    m_batteryPower->powerBatteryDC = 50;
    m_batteryPowerFlow->calculate();

    EXPECT_NEAR(m_batteryPower->powerBatteryAC, 47.39, error);
    EXPECT_NEAR(m_batteryPower->powerSystemToLoad, 50, error);
    EXPECT_NEAR(m_batteryPower->powerSystemToBattery, 0, error);
    EXPECT_NEAR(m_batteryPower->powerGridToBattery, 0, error);
    EXPECT_NEAR(m_batteryPower->powerGridToLoad, 0, error);
    EXPECT_NEAR(m_batteryPower->powerSystemToGrid, 46.71, error);
    EXPECT_NEAR(m_batteryPower->powerBatteryToLoad, 0, error);
    EXPECT_NEAR(m_batteryPower->powerConversionLoss, 5.89, error);
    EXPECT_NEAR(m_batteryPower->powerSystemLoss, 0.0, error);

    check_net_flows(std::string());

    // Redo tests from above with losses: Try to charge
    m_batteryPower->powerBatteryDC = -50;
    m_batteryPower->powerSystemLoss = 1.0;
    m_batteryPowerFlow->calculate();

    EXPECT_NEAR(m_batteryPower->powerBatteryAC, -51.02, error);
    EXPECT_NEAR(m_batteryPower->powerSystemToLoad, 45.24, error);
    EXPECT_NEAR(m_batteryPower->powerSystemToBattery, 51.02, error);
    EXPECT_NEAR(m_batteryPower->powerGridToBattery, 0, error);
    EXPECT_NEAR(m_batteryPower->powerGridToLoad, 4.75, error);
    EXPECT_NEAR(m_batteryPower->powerSystemToGrid, 0, error);
    EXPECT_NEAR(m_batteryPower->powerBatteryToLoad, 0, error);
    EXPECT_NEAR(m_batteryPower->powerConversionLoss, 3.75, error);
    EXPECT_NEAR(m_batteryPower->powerSystemLoss, 1.0, error);

    check_net_flows(std::string());

    // Charge battery from PV and meet load from grid
    m_batteryPower->powerBatteryDC = -100;
    m_batteryPowerFlow->calculate();

    EXPECT_NEAR(m_batteryPower->powerBatteryAC, -99, error);
    EXPECT_NEAR(m_batteryPower->powerSystemToLoad, 0, error);
    EXPECT_NEAR(m_batteryPower->powerSystemToBattery, 99, error);
    EXPECT_NEAR(m_batteryPower->powerGridToBattery, 0, error);
    EXPECT_NEAR(m_batteryPower->powerGridToLoad, 50, error);
    EXPECT_NEAR(m_batteryPower->powerSystemToGrid, 0, error);
    EXPECT_NEAR(m_batteryPower->powerBatteryToLoad, 0, error);
    EXPECT_NEAR(m_batteryPower->powerConversionLoss, 2.0, error);
    EXPECT_NEAR(m_batteryPower->powerSystemLoss, 1.0, error);

    check_net_flows(std::string());


    // Try to charge more than available from PV, disallowed
    m_batteryPower->powerBatteryDC = -150;
    m_batteryPowerFlow->calculate();

    EXPECT_NEAR(m_batteryPower->powerBatteryAC, -99, error);
    EXPECT_NEAR(m_batteryPower->powerSystemToLoad, 0, error);
    EXPECT_NEAR(m_batteryPower->powerSystemToBattery, 99, error);
    EXPECT_NEAR(m_batteryPower->powerGridToBattery, 0, error);
    EXPECT_NEAR(m_batteryPower->powerGridToLoad, 50, error);
    EXPECT_NEAR(m_batteryPower->powerSystemToGrid, 0, error);
    EXPECT_NEAR(m_batteryPower->powerBatteryToLoad, 0, error);
    EXPECT_NEAR(m_batteryPower->powerConversionLoss, 2.0, error);
    EXPECT_NEAR(m_batteryPower->powerSystemLoss, 1.0, error);

    check_net_flows(std::string());


    // Try to discharge
    m_batteryPower->powerBatteryDC = 50;
    m_batteryPowerFlow->calculate();

    EXPECT_NEAR(m_batteryPower->powerBatteryAC, 46.42, error);
    EXPECT_NEAR(m_batteryPower->powerSystemToLoad, 50, error);
    EXPECT_NEAR(m_batteryPower->powerSystemToBattery, 0, error);
    EXPECT_NEAR(m_batteryPower->powerGridToBattery, 0, error);
    EXPECT_NEAR(m_batteryPower->powerGridToLoad, 0, error);
    EXPECT_NEAR(m_batteryPower->powerSystemToGrid, 46.71, error);
    EXPECT_NEAR(m_batteryPower->powerBatteryToLoad, 0, error);
    EXPECT_NEAR(m_batteryPower->powerConversionLoss, 5.87, error);
    EXPECT_NEAR(m_batteryPower->powerSystemLoss, 1.0, error);

    check_net_flows(std::string());

}

// Not enough PV for load
TEST_F(BatteryPowerFlowTest_lib_battery_powerflow, DC_PVCharging_ExcessLoad) {
    m_batteryPower->connectionMode = ChargeController::DC_CONNECTED;

    m_batteryPower->canSystemCharge = true;
    m_batteryPower->canDischarge = true;
    m_batteryPower->canGridCharge = false;
    m_batteryPower->powerSystem = 25;
    m_batteryPower->powerLoad = 50;

    // do not discharge battery
    m_batteryPower->powerBatteryDC = 0;
    m_batteryPowerFlow->calculate();

    EXPECT_NEAR(m_batteryPower->powerBatteryAC, 0, error);
    EXPECT_NEAR(m_batteryPower->powerSystemToLoad, 22.68, error);
    EXPECT_NEAR(m_batteryPower->powerSystemToBattery, 0, error);
    EXPECT_NEAR(m_batteryPower->powerGridToBattery, 0, error);
    EXPECT_NEAR(m_batteryPower->powerGridToLoad, 27.31, error);
    EXPECT_NEAR(m_batteryPower->powerSystemToGrid, 0, error);
    EXPECT_NEAR(m_batteryPower->powerBatteryToLoad, 0, error);
    EXPECT_NEAR(m_batteryPower->powerConversionLoss, 2.32, error);
    EXPECT_NEAR(m_batteryPower->powerSystemLoss, 0.0, error);

    check_net_flows(std::string());


    // discharge battery
    m_batteryPower->powerBatteryDC = 20;
    m_batteryPowerFlow->calculate();

    EXPECT_NEAR(m_batteryPower->powerBatteryAC, 18.43, error);
    EXPECT_NEAR(m_batteryPower->powerSystemToLoad, 23.50, error);
    EXPECT_NEAR(m_batteryPower->powerSystemToBattery, 0, error);
    EXPECT_NEAR(m_batteryPower->powerGridToBattery, 0, error);
    EXPECT_NEAR(m_batteryPower->powerGridToLoad, 8.05, error);
    EXPECT_NEAR(m_batteryPower->powerSystemToGrid, 0, error);
    EXPECT_NEAR(m_batteryPower->powerBatteryToLoad,  18.43, error);
    EXPECT_NEAR(m_batteryPower->powerConversionLoss, 3.05, error);
    EXPECT_NEAR(m_batteryPower->powerSystemLoss, 0.0, error);

    check_net_flows(std::string());


    // charging happens from pv
    m_batteryPower->powerBatteryDC = -20;
    m_batteryPowerFlow->calculate();

    EXPECT_NEAR(m_batteryPower->powerBatteryAC, -20.4, error);
    EXPECT_NEAR(m_batteryPower->powerSystemToLoad, 2.60, error);
    EXPECT_NEAR(m_batteryPower->powerSystemToBattery, 20.40, error);
    EXPECT_NEAR(m_batteryPower->powerGridToBattery, 0, error);
    EXPECT_NEAR(m_batteryPower->powerGridToLoad, 47.39, error);
    EXPECT_NEAR(m_batteryPower->powerSystemToGrid, 0, error);
    EXPECT_NEAR(m_batteryPower->powerBatteryToLoad, 0, error);
    EXPECT_NEAR(m_batteryPower->powerConversionLoss, 2.39, error);
    EXPECT_NEAR(m_batteryPower->powerSystemLoss, 0.0, error);

    check_net_flows(std::string());

    // do not discharge battery
    m_batteryPower->powerBatteryDC = 0;
    m_batteryPower->powerSystemLoss = 0.5;
    m_batteryPowerFlow->calculate();

    EXPECT_NEAR(m_batteryPower->powerBatteryAC, 0, error);
    EXPECT_NEAR(m_batteryPower->powerSystemToLoad, 22.18, error);
    EXPECT_NEAR(m_batteryPower->powerSystemToBattery, 0, error);
    EXPECT_NEAR(m_batteryPower->powerGridToBattery, 0, error);
    EXPECT_NEAR(m_batteryPower->powerGridToLoad, 27.81, error);
    EXPECT_NEAR(m_batteryPower->powerSystemToGrid, 0, error);
    EXPECT_NEAR(m_batteryPower->powerBatteryToLoad, 0, error);
    EXPECT_NEAR(m_batteryPower->powerConversionLoss, 2.32, error);
    EXPECT_NEAR(m_batteryPower->powerSystemLoss, 0.5, error);

    check_net_flows(std::string());


    // discharge battery
    m_batteryPower->powerBatteryDC = 20;
    m_batteryPower->powerSystemLoss = 1.0;
    m_batteryPowerFlow->calculate();

    EXPECT_NEAR(m_batteryPower->powerBatteryAC, 17.47, error);
    EXPECT_NEAR(m_batteryPower->powerSystemToLoad, 23.50, error);
    EXPECT_NEAR(m_batteryPower->powerSystemToBattery, 0, error);
    EXPECT_NEAR(m_batteryPower->powerGridToBattery, 0, error);
    EXPECT_NEAR(m_batteryPower->powerGridToLoad, 9.05, error);
    EXPECT_NEAR(m_batteryPower->powerSystemToGrid, 0, error);
    EXPECT_NEAR(m_batteryPower->powerBatteryToLoad, 17.47, error);
    EXPECT_NEAR(m_batteryPower->powerConversionLoss, 3.05, error);
    EXPECT_NEAR(m_batteryPower->powerSystemLoss, 1.0, error);

    check_net_flows(std::string());


    // charging happens from pv
    m_batteryPower->powerBatteryDC = -20;
    m_batteryPowerFlow->calculate();

    EXPECT_NEAR(m_batteryPower->powerBatteryAC, -20.4, error);
    EXPECT_NEAR(m_batteryPower->powerSystemToLoad, 1.62, error);
    EXPECT_NEAR(m_batteryPower->powerSystemToBattery, 20.40, error);
    EXPECT_NEAR(m_batteryPower->powerGridToBattery, 0, error);
    EXPECT_NEAR(m_batteryPower->powerGridToLoad, 48.39, error);
    EXPECT_NEAR(m_batteryPower->powerSystemToGrid, 0, error);
    EXPECT_NEAR(m_batteryPower->powerBatteryToLoad, 0, error);
    EXPECT_NEAR(m_batteryPower->powerConversionLoss, 2.39, error);
    EXPECT_NEAR(m_batteryPower->powerSystemLoss, 1.0, error);

    check_net_flows(std::string());
}

// Excess PV production
TEST_F(BatteryPowerFlowTest_lib_battery_powerflow, DC_GridCharging_ExcessPV) {
    m_batteryPower->connectionMode = ChargeController::DC_CONNECTED;

    m_batteryPower->canGridCharge = true;
    m_batteryPower->canDischarge = true;
    m_batteryPower->canSystemCharge = false;
    m_batteryPower->powerSystem = 100;
    m_batteryPower->powerLoad = 50;

    // charging will be from grid
    m_batteryPower->powerBatteryDC = -50;
    m_batteryPowerFlow->calculate();

    EXPECT_NEAR(m_batteryPower->powerBatteryAC, -54.04, error) << "1st case";
    EXPECT_NEAR(m_batteryPower->powerSystemToLoad, 50, error) << "1st case";
    EXPECT_NEAR(m_batteryPower->powerSystemToBattery, 0, error) << "1st case";
    EXPECT_NEAR(m_batteryPower->powerSystemToGrid, 44.40, error) << "1st case";
    EXPECT_NEAR(m_batteryPower->powerGridToBattery, 54.04, error) << "1st case";
    EXPECT_NEAR(m_batteryPower->powerGridToLoad, 0, error) << "1st case";
    EXPECT_NEAR(m_batteryPower->powerBatteryToLoad, 0, error) << "1st case";
    EXPECT_NEAR(m_batteryPower->powerConversionLoss, 3.75, error) << "1st case";
    EXPECT_NEAR(m_batteryPower->powerSystemLoss, 0.0, error) << "1st case";

    check_net_flows("1st case");


    // discharge to grid
    m_batteryPower->powerBatteryDC = 20;
    m_batteryPowerFlow->calculate();

    EXPECT_NEAR(m_batteryPower->powerBatteryAC, 18.91, error) << "2nd case";
    EXPECT_NEAR(m_batteryPower->powerSystemToLoad, 50, error) << "2nd case";
    EXPECT_NEAR(m_batteryPower->powerSystemToBattery, 0, error) << "2nd case";
    EXPECT_NEAR(m_batteryPower->powerSystemToGrid, 46.49, error) << "2nd case";
    EXPECT_NEAR(m_batteryPower->powerGridToBattery, 0, error) << "2nd case";
    EXPECT_NEAR(m_batteryPower->powerGridToLoad, 0, error) << "2nd case";
    EXPECT_NEAR(m_batteryPower->powerBatteryToLoad, 0, error) << "2nd case";
    EXPECT_NEAR(m_batteryPower->powerBatteryToGrid, 18.91, error) << "2nd case";
    EXPECT_NEAR(m_batteryPower->powerConversionLoss, 4.59, error) << "2nd case";
    EXPECT_NEAR(m_batteryPower->powerSystemLoss, 0.0, error) << "2nd case";

    check_net_flows("2nd case");

    // Redo the above with losses: charging will be from grid
    m_batteryPower->powerSystemLoss = 1.0;
    m_batteryPower->powerBatteryDC = -50;
    m_batteryPowerFlow->calculate();

    EXPECT_NEAR(m_batteryPower->powerBatteryAC, -54.08, error) << "1st case";
    EXPECT_NEAR(m_batteryPower->powerSystemToLoad, 50, error) << "1st case";
    EXPECT_NEAR(m_batteryPower->powerSystemToBattery, 0, error) << "1st case";
    EXPECT_NEAR(m_batteryPower->powerSystemToGrid, 43.40, error) << "1st case";
    EXPECT_NEAR(m_batteryPower->powerGridToBattery, 54.08, error) << "1st case";
    EXPECT_NEAR(m_batteryPower->powerGridToLoad, 0, error) << "1st case";
    EXPECT_NEAR(m_batteryPower->powerBatteryToLoad, 0, error) << "1st case";
    EXPECT_NEAR(m_batteryPower->powerConversionLoss, 3.75, error) << "1st case";
    EXPECT_NEAR(m_batteryPower->powerSystemLoss, 1.0, error) << "1st case";

    check_net_flows("1st case");


    // discharge to grid
    m_batteryPower->powerBatteryDC = 20;
    m_batteryPowerFlow->calculate();

    EXPECT_NEAR(m_batteryPower->powerBatteryAC, 17.95, error) << "2nd case";
    EXPECT_NEAR(m_batteryPower->powerSystemToLoad, 50, error) << "2nd case";
    EXPECT_NEAR(m_batteryPower->powerSystemToBattery, 0, error) << "2nd case";
    EXPECT_NEAR(m_batteryPower->powerSystemToGrid, 46.49, error) << "2nd case";
    EXPECT_NEAR(m_batteryPower->powerGridToBattery, 0, error) << "2nd case";
    EXPECT_NEAR(m_batteryPower->powerGridToLoad, 0, error) << "2nd case";
    EXPECT_NEAR(m_batteryPower->powerBatteryToLoad, 0, error) << "2nd case";
    EXPECT_NEAR(m_batteryPower->powerBatteryToGrid, 17.95, error) << "2nd case";
    EXPECT_NEAR(m_batteryPower->powerConversionLoss, 4.59, error) << "2nd case";
    EXPECT_NEAR(m_batteryPower->powerSystemLoss, 1.0, error) << "2nd case";

    check_net_flows("2nd case");
}

// Not enough PV, pull from grid
TEST_F(BatteryPowerFlowTest_lib_battery_powerflow, DC_GridCharging_ExcessLoad)
{
    m_batteryPower->connectionMode = ChargeController::DC_CONNECTED;

    m_batteryPower->canGridCharge = true;
    m_batteryPower->canDischarge = true;
    m_batteryPower->canSystemCharge = false;
    m_batteryPower->powerSystem = 10;
    m_batteryPower->powerLoad = 50;

    // don't dispatch
    m_batteryPower->powerBatteryDC = 0;
    m_batteryPowerFlow->calculate();

    EXPECT_NEAR(m_batteryPower->powerBatteryAC, 0, error);
    EXPECT_NEAR(m_batteryPower->powerSystemToLoad, 7.93, error);
    EXPECT_NEAR(m_batteryPower->powerSystemToBattery, 0, error);
    EXPECT_NEAR(m_batteryPower->powerSystemToGrid, 0, error);
    EXPECT_NEAR(m_batteryPower->powerGridToBattery, 0, error);
    EXPECT_NEAR(m_batteryPower->powerGridToLoad, 42.07, error);
    EXPECT_NEAR(m_batteryPower->powerSystemToGrid, 0, error);
    EXPECT_NEAR(m_batteryPower->powerBatteryToLoad, 0, error);
    EXPECT_NEAR(m_batteryPower->powerBatteryToGrid, 0, error);
    EXPECT_NEAR(m_batteryPower->powerConversionLoss, 2.07, error);
    EXPECT_NEAR(m_batteryPower->powerSystemLoss, 0.0, error);

    check_net_flows(std::string());


    // charging will happen from grid
    m_batteryPower->powerBatteryDC = -20;
    m_batteryPowerFlow->calculate();

    EXPECT_NEAR(m_batteryPower->powerBatteryAC, -25.49, error);
    EXPECT_NEAR(m_batteryPower->powerSystemToLoad, 8.00, error);
    EXPECT_NEAR(m_batteryPower->powerSystemToBattery, 0, error);
    EXPECT_NEAR(m_batteryPower->powerSystemToGrid, 0, error);
    EXPECT_NEAR(m_batteryPower->powerGridToBattery, 25.49, error);
    EXPECT_NEAR(m_batteryPower->powerGridToLoad, 41.99, error);
    EXPECT_NEAR(m_batteryPower->powerSystemToGrid, 0, error);
    EXPECT_NEAR(m_batteryPower->powerBatteryToLoad, 0, error);
    EXPECT_NEAR(m_batteryPower->powerBatteryToGrid, 0, error);
    EXPECT_NEAR(m_batteryPower->powerConversionLoss, 3.00, error);
    EXPECT_NEAR(m_batteryPower->powerSystemLoss, 0.0, error);

    check_net_flows(std::string());


    // discharge
    m_batteryPower->powerBatteryDC = 20;
    m_batteryPowerFlow->calculate();

    EXPECT_NEAR(m_batteryPower->powerBatteryAC, 18.01, error);
    EXPECT_NEAR(m_batteryPower->powerSystemToLoad, 9.19, error);
    EXPECT_NEAR(m_batteryPower->powerSystemToBattery, 0, error);
    EXPECT_NEAR(m_batteryPower->powerSystemToGrid, 0, error);
    EXPECT_NEAR(m_batteryPower->powerGridToBattery, 0, error);
    EXPECT_NEAR(m_batteryPower->powerGridToLoad, 22.79, error);
    EXPECT_NEAR(m_batteryPower->powerSystemToGrid, 0, error);
    EXPECT_NEAR(m_batteryPower->powerBatteryToLoad, 18.01, error);
    EXPECT_NEAR(m_batteryPower->powerBatteryToGrid, 0, error);
    EXPECT_NEAR(m_batteryPower->powerConversionLoss, 2.79, error);
    EXPECT_NEAR(m_batteryPower->powerSystemLoss, 0.0, error);

    check_net_flows(std::string());

    // Redo the above, with losses: don't dispatch
    m_batteryPower->powerBatteryDC = 0;
    m_batteryPower->powerSystemLoss = 0.5;
    m_batteryPowerFlow->calculate();

    EXPECT_NEAR(m_batteryPower->powerBatteryAC, 0, error);
    EXPECT_NEAR(m_batteryPower->powerSystemToLoad, 7.44, error);
    EXPECT_NEAR(m_batteryPower->powerSystemToBattery, 0, error);
    EXPECT_NEAR(m_batteryPower->powerSystemToGrid, 0, error);
    EXPECT_NEAR(m_batteryPower->powerGridToBattery, 0, error);
    EXPECT_NEAR(m_batteryPower->powerGridToLoad, 42.56, error);
    EXPECT_NEAR(m_batteryPower->powerSystemToGrid, 0, error);
    EXPECT_NEAR(m_batteryPower->powerBatteryToLoad, 0, error);
    EXPECT_NEAR(m_batteryPower->powerBatteryToGrid, 0, error);
    EXPECT_NEAR(m_batteryPower->powerConversionLoss, 2.07, error);
    EXPECT_NEAR(m_batteryPower->powerSystemLoss, 0.5, error);

    check_net_flows(std::string());


    // charging will happen from grid (though dispatch constraints would prevent grid charging with PV output)
    m_batteryPower->powerBatteryDC = -20;
    m_batteryPower->powerSystemLoss = 1.0;
    m_batteryPowerFlow->calculate();

    EXPECT_NEAR(m_batteryPower->powerBatteryAC, -24.99, error);
    EXPECT_NEAR(m_batteryPower->powerSystemToLoad, 7.35, error);
    EXPECT_NEAR(m_batteryPower->powerSystemToBattery, 0, error);
    EXPECT_NEAR(m_batteryPower->powerSystemToGrid, 0, error);
    EXPECT_NEAR(m_batteryPower->powerGridToBattery, 24.99, error);
    EXPECT_NEAR(m_batteryPower->powerGridToLoad, 42.65, error);
    EXPECT_NEAR(m_batteryPower->powerSystemToGrid, 0, error);
    EXPECT_NEAR(m_batteryPower->powerBatteryToLoad, 0, error);
    EXPECT_NEAR(m_batteryPower->powerBatteryToGrid, 0, error);
    EXPECT_NEAR(m_batteryPower->powerConversionLoss, 2.97, error);
    EXPECT_NEAR(m_batteryPower->powerSystemLoss, 1.0, error);

    check_net_flows(std::string());


    // discharge
    m_batteryPower->powerBatteryDC = 20;
    m_batteryPowerFlow->calculate();

    EXPECT_NEAR(m_batteryPower->powerBatteryAC, 17.05, error);
    EXPECT_NEAR(m_batteryPower->powerSystemToLoad, 9.17, error);
    EXPECT_NEAR(m_batteryPower->powerSystemToBattery, 0, error);
    EXPECT_NEAR(m_batteryPower->powerSystemToGrid, 0, error);
    EXPECT_NEAR(m_batteryPower->powerGridToBattery, 0, error);
    EXPECT_NEAR(m_batteryPower->powerGridToLoad, 23.79, error);
    EXPECT_NEAR(m_batteryPower->powerSystemToGrid, 0, error);
    EXPECT_NEAR(m_batteryPower->powerBatteryToLoad, 17.05, error);
    EXPECT_NEAR(m_batteryPower->powerBatteryToGrid, 0, error);
    EXPECT_NEAR(m_batteryPower->powerConversionLoss, 2.79, error);
    EXPECT_NEAR(m_batteryPower->powerSystemLoss, 1.0, error);

    check_net_flows(std::string());

}

// Excess PV production
TEST_F(BatteryPowerFlowTest_lib_battery_powerflow, DC_GridPVCharging_ExcessPV) {
    m_batteryPower->connectionMode = ChargeController::DC_CONNECTED;

    m_batteryPower->canGridCharge = true;
    m_batteryPower->canDischarge = true;
    m_batteryPower->canSystemCharge = true;
    m_batteryPower->powerSystem = 100;
    m_batteryPower->powerLoad = 50;

    // charging will be from PV
    m_batteryPower->powerBatteryDC = -40;
    m_batteryPowerFlow->calculate();

    EXPECT_NEAR(m_batteryPower->powerBatteryAC, -40.81, error);
    EXPECT_NEAR(m_batteryPower->powerSystemToLoad, 50, error);
    EXPECT_NEAR(m_batteryPower->powerSystemToBattery, 40.81, error);
    EXPECT_NEAR(m_batteryPower->powerSystemToGrid, 6.25, error);
    EXPECT_NEAR(m_batteryPower->powerGridToBattery, 0, error);
    EXPECT_NEAR(m_batteryPower->powerGridToLoad, 0, error);
    EXPECT_NEAR(m_batteryPower->powerBatteryToLoad, 0, error);
    EXPECT_NEAR(m_batteryPower->powerConversionLoss, 3.74, error);
    EXPECT_NEAR(m_batteryPower->powerSystemLoss, 0.0, error);

    check_net_flows("1st case");

    // charging will be from PV then grid
    m_batteryPower->powerBatteryDC = -100;
    m_batteryPowerFlow->calculate();

    EXPECT_NEAR(m_batteryPower->powerBatteryAC,  -140.81, error);
    EXPECT_NEAR(m_batteryPower->powerSystemToLoad, 0, error);
    EXPECT_NEAR(m_batteryPower->powerSystemToBattery, 100, error);
    EXPECT_NEAR(m_batteryPower->powerSystemToGrid, 0, error);
    EXPECT_NEAR(m_batteryPower->powerGridToBattery, 40.81, error);
    EXPECT_NEAR(m_batteryPower->powerGridToLoad, 50, error);
    EXPECT_NEAR(m_batteryPower->powerBatteryToLoad, 0, error);
    EXPECT_NEAR(m_batteryPower->powerConversionLoss, 40.81, error);
    EXPECT_NEAR(m_batteryPower->powerSystemLoss, 0.0, error);

    check_net_flows("2nd case");


    // discharge to grid
    m_batteryPower->powerBatteryDC = 20;
    m_batteryPowerFlow->calculate();

    EXPECT_NEAR(m_batteryPower->powerBatteryAC, 18.91, error);
    EXPECT_NEAR(m_batteryPower->powerSystemToLoad, 50, error);
    EXPECT_NEAR(m_batteryPower->powerSystemToBattery, 0, error);
    EXPECT_NEAR(m_batteryPower->powerSystemToGrid, 46.49, error);
    EXPECT_NEAR(m_batteryPower->powerGridToBattery, 0, error);
    EXPECT_NEAR(m_batteryPower->powerGridToLoad, 0, error);
    EXPECT_NEAR(m_batteryPower->powerBatteryToLoad, 0, error);
    EXPECT_NEAR(m_batteryPower->powerBatteryToGrid, 18.91, error);
    EXPECT_NEAR(m_batteryPower->powerConversionLoss, 4.59, error);
    EXPECT_NEAR(m_batteryPower->powerSystemLoss, 0.0, error);

    check_net_flows("3rd case");

    // Redo the above, with losses: charging will be from PV
    m_batteryPower->powerSystemLoss = 1.0;
    m_batteryPower->powerBatteryDC = -40;
    m_batteryPowerFlow->calculate();

    EXPECT_NEAR(m_batteryPower->powerBatteryAC, -40.81, error);
    EXPECT_NEAR(m_batteryPower->powerSystemToLoad, 50, error);
    EXPECT_NEAR(m_batteryPower->powerSystemToBattery, 40.81, error);
    EXPECT_NEAR(m_batteryPower->powerSystemToGrid, 5.27, error);
    EXPECT_NEAR(m_batteryPower->powerGridToBattery, 0, error);
    EXPECT_NEAR(m_batteryPower->powerGridToLoad, 0, error);
    EXPECT_NEAR(m_batteryPower->powerBatteryToLoad, 0, error);
    EXPECT_NEAR(m_batteryPower->powerConversionLoss, 3.74, error);
    EXPECT_NEAR(m_batteryPower->powerSystemLoss, 1.0, error);

    check_net_flows("4th case");

    // charging will be from PV then grid
    m_batteryPower->powerBatteryDC = -100;
    m_batteryPowerFlow->calculate();

    EXPECT_NEAR(m_batteryPower->powerBatteryAC, -107.57, error);
    EXPECT_NEAR(m_batteryPower->powerSystemToLoad, 0, error);
    EXPECT_NEAR(m_batteryPower->powerSystemToBattery, 99, error);
    EXPECT_NEAR(m_batteryPower->powerSystemToGrid, 0, error);
    EXPECT_NEAR(m_batteryPower->powerGridToBattery, 8.57, error);
    EXPECT_NEAR(m_batteryPower->powerGridToLoad, 50, error);
    EXPECT_NEAR(m_batteryPower->powerBatteryToLoad, 0, error);
    EXPECT_NEAR(m_batteryPower->powerConversionLoss, 7.57, error);
    EXPECT_NEAR(m_batteryPower->powerSystemLoss, 1.0, error);

    check_net_flows("5th case");


    // discharge to grid
    m_batteryPower->powerBatteryDC = 20;
    m_batteryPowerFlow->calculate();

    EXPECT_NEAR(m_batteryPower->powerBatteryAC, 17.95, error);
    EXPECT_NEAR(m_batteryPower->powerSystemToLoad, 50, error);
    EXPECT_NEAR(m_batteryPower->powerSystemToBattery, 0, error);
    EXPECT_NEAR(m_batteryPower->powerSystemToGrid, 46.49, error);
    EXPECT_NEAR(m_batteryPower->powerGridToBattery, 0, error);
    EXPECT_NEAR(m_batteryPower->powerGridToLoad, 0, error);
    EXPECT_NEAR(m_batteryPower->powerBatteryToLoad, 0, error);
    EXPECT_NEAR(m_batteryPower->powerBatteryToGrid, 17.95, error);
    EXPECT_NEAR(m_batteryPower->powerConversionLoss, 4.59, error);
    EXPECT_NEAR(m_batteryPower->powerSystemLoss, 1.0, error);

    check_net_flows("6th case");

}

// Not enough PV, pull from grid
TEST_F(BatteryPowerFlowTest_lib_battery_powerflow, DC_GridPVCharging_ExcessLoad)
{
    m_batteryPower->connectionMode = ChargeController::DC_CONNECTED;

    m_batteryPower->canGridCharge = true;
    m_batteryPower->canDischarge = true;
    m_batteryPower->canSystemCharge = true;
    m_batteryPower->powerSystem = 10;
    m_batteryPower->powerLoad = 50;

    // don't dispatch
    m_batteryPower->powerBatteryDC = 0;
    m_batteryPowerFlow->calculate();

    EXPECT_NEAR(m_batteryPower->powerBatteryAC, 0, error);
    EXPECT_NEAR(m_batteryPower->powerSystemToLoad, 7.92, error);
    EXPECT_NEAR(m_batteryPower->powerSystemToBattery, 0, error);
    EXPECT_NEAR(m_batteryPower->powerSystemToGrid, 0, error);
    EXPECT_NEAR(m_batteryPower->powerGridToBattery, 0, error);
    EXPECT_NEAR(m_batteryPower->powerGridToLoad, 42.07, error);
    EXPECT_NEAR(m_batteryPower->powerSystemToGrid, 0, error);
    EXPECT_NEAR(m_batteryPower->powerBatteryToLoad, 0, error);
    EXPECT_NEAR(m_batteryPower->powerBatteryToGrid, 0, error);
    EXPECT_NEAR(m_batteryPower->powerConversionLoss, 2.07, error);
    EXPECT_NEAR(m_batteryPower->powerSystemLoss, 0.0, error);

    check_net_flows(std::string());


    // charging will happen from pv first
    m_batteryPower->powerBatteryDC = -20;
    m_batteryPowerFlow->calculate();

    EXPECT_NEAR(m_batteryPower->powerBatteryAC, -23.0, error);
    EXPECT_NEAR(m_batteryPower->powerSystemToLoad, 0, error);
    EXPECT_NEAR(m_batteryPower->powerSystemToBattery, 10, error);
    EXPECT_NEAR(m_batteryPower->powerSystemToGrid, 0, error);
    EXPECT_NEAR(m_batteryPower->powerGridToBattery, 13.00, error);
    EXPECT_NEAR(m_batteryPower->powerGridToLoad, 50, error);
    EXPECT_NEAR(m_batteryPower->powerSystemToGrid, 0, error);
    EXPECT_NEAR(m_batteryPower->powerBatteryToLoad, 0, error);
    EXPECT_NEAR(m_batteryPower->powerBatteryToGrid, 0, error);
    EXPECT_NEAR(m_batteryPower->powerConversionLoss, 3.00, error);
    EXPECT_NEAR(m_batteryPower->powerSystemLoss, 0.0, error);

    check_net_flows(std::string());

    // discharge
    m_batteryPower->powerBatteryDC = 20;
    m_batteryPowerFlow->calculate();

    EXPECT_NEAR(m_batteryPower->powerBatteryAC, 18.01, error);
    EXPECT_NEAR(m_batteryPower->powerSystemToLoad, 9.19, error);
    EXPECT_NEAR(m_batteryPower->powerSystemToBattery, 0, error);
    EXPECT_NEAR(m_batteryPower->powerSystemToGrid, 0, error);
    EXPECT_NEAR(m_batteryPower->powerGridToBattery, 0, error);
    EXPECT_NEAR(m_batteryPower->powerGridToLoad, 22.79, error);
    EXPECT_NEAR(m_batteryPower->powerSystemToGrid, 0, error);
    EXPECT_NEAR(m_batteryPower->powerBatteryToLoad, 18.01, error);
    EXPECT_NEAR(m_batteryPower->powerBatteryToGrid, 0, error);
    EXPECT_NEAR(m_batteryPower->powerConversionLoss, 2.79, error);
    EXPECT_NEAR(m_batteryPower->powerSystemLoss, 0.0, error);

    check_net_flows(std::string());

    // Redo the above, with losses: don't dispatch
    m_batteryPower->powerSystemLoss = 0.5;
    m_batteryPower->powerBatteryDC = 0;
    m_batteryPowerFlow->calculate();

    EXPECT_NEAR(m_batteryPower->powerBatteryAC, 0, error);
    EXPECT_NEAR(m_batteryPower->powerSystemToLoad, 7.42, error);
    EXPECT_NEAR(m_batteryPower->powerSystemToBattery, 0, error);
    EXPECT_NEAR(m_batteryPower->powerSystemToGrid, 0, error);
    EXPECT_NEAR(m_batteryPower->powerGridToBattery, 0, error);
    EXPECT_NEAR(m_batteryPower->powerGridToLoad, 42.57, error);
    EXPECT_NEAR(m_batteryPower->powerSystemToGrid, 0, error);
    EXPECT_NEAR(m_batteryPower->powerBatteryToLoad, 0, error);
    EXPECT_NEAR(m_batteryPower->powerBatteryToGrid, 0, error);
    EXPECT_NEAR(m_batteryPower->powerConversionLoss, 2.07, error);
    EXPECT_NEAR(m_batteryPower->powerSystemLoss, 0.5, error);

    check_net_flows(std::string());

    // charging will happen from pv first
    m_batteryPower->powerSystemLoss = 1.0;
    m_batteryPower->powerBatteryDC = -20;
    m_batteryPowerFlow->calculate();

    EXPECT_NEAR(m_batteryPower->powerBatteryAC, -22.97, error);
    EXPECT_NEAR(m_batteryPower->powerSystemToLoad, 0, error);
    EXPECT_NEAR(m_batteryPower->powerSystemToBattery, 9, error);
    EXPECT_NEAR(m_batteryPower->powerSystemToGrid, 0, error);
    EXPECT_NEAR(m_batteryPower->powerGridToBattery, 13.97, error);
    EXPECT_NEAR(m_batteryPower->powerGridToLoad, 50, error);
    EXPECT_NEAR(m_batteryPower->powerSystemToGrid, 0, error);
    EXPECT_NEAR(m_batteryPower->powerBatteryToLoad, 0, error);
    EXPECT_NEAR(m_batteryPower->powerBatteryToGrid, 0, error);
    EXPECT_NEAR(m_batteryPower->powerConversionLoss, 2.97, error);
    EXPECT_NEAR(m_batteryPower->powerSystemLoss, 1.0, error);

    check_net_flows(std::string());

    // discharge
    m_batteryPower->powerBatteryDC = 20;
    m_batteryPowerFlow->calculate();

    EXPECT_NEAR(m_batteryPower->powerBatteryAC, 17.05, error);
    EXPECT_NEAR(m_batteryPower->powerSystemToLoad, 9.17, error);
    EXPECT_NEAR(m_batteryPower->powerSystemToBattery, 0, error);
    EXPECT_NEAR(m_batteryPower->powerSystemToGrid, 0, error);
    EXPECT_NEAR(m_batteryPower->powerGridToBattery, 0, error);
    EXPECT_NEAR(m_batteryPower->powerGridToLoad, 23.79, error);
    EXPECT_NEAR(m_batteryPower->powerSystemToGrid, 0, error);
    EXPECT_NEAR(m_batteryPower->powerBatteryToLoad, 17.05, error);
    EXPECT_NEAR(m_batteryPower->powerBatteryToGrid, 0, error);
    EXPECT_NEAR(m_batteryPower->powerConversionLoss, 2.79, error);
    EXPECT_NEAR(m_batteryPower->powerSystemLoss, 1.0, error);

    check_net_flows(std::string());
}


TEST_F(BatteryPowerFlowTest_lib_battery_powerflow, TestDCConnected)
{
    m_batteryPower->connectionMode = ChargeController::DC_CONNECTED;

	// PV and Grid Charging Scenario
	m_batteryPower->canSystemCharge = true;
	m_batteryPower->powerSystem = 300;
	m_batteryPower->powerLoad = 200;
	m_batteryPowerFlow->initialize(50);
	m_batteryPowerFlow->calculate();

    EXPECT_NEAR(m_batteryPower->powerBatteryAC, -102.04, error);
    EXPECT_NEAR(m_batteryPower->powerSystemToLoad, 191.78, error);
    EXPECT_NEAR(m_batteryPower->powerSystemToBattery, 102.04, error);
    EXPECT_NEAR(m_batteryPower->powerGridToBattery, 0.00, error);
    EXPECT_NEAR(m_batteryPower->powerConversionLoss, 8.22, error);
    EXPECT_NEAR(m_batteryPower->powerSystemLoss, 0.0, error);

	// Exclusive Grid Charging Scenario
	m_batteryPower->canGridCharge = true;
	m_batteryPower->canSystemCharge = false;
	m_batteryPower->powerSystem = 300;
	m_batteryPower->powerLoad = 200;
	m_batteryPowerFlow->initialize(50);
	m_batteryPowerFlow->calculate();

    EXPECT_NEAR(m_batteryPower->powerBatteryAC, -105.33, error);
    EXPECT_NEAR(m_batteryPower->powerSystemToLoad, 200, error);
    EXPECT_NEAR(m_batteryPower->powerSystemToBattery, 0, error);
    EXPECT_NEAR(m_batteryPower->powerSystemToGrid, 90.63, error);
    EXPECT_NEAR(m_batteryPower->powerGridToBattery, 105.33, error);
    EXPECT_NEAR(m_batteryPower->powerConversionLoss, 8.22, error);
    EXPECT_NEAR(m_batteryPower->powerSystemLoss, 0.0, error);

	// Discharging Scenario
	m_batteryPower->canDischarge = true;
	m_batteryPower->powerSystem = 200;
	m_batteryPower->powerLoad = 300;
	m_batteryPowerFlow->initialize(50);
	m_batteryPowerFlow->calculate();

    EXPECT_NEAR(m_batteryPower->powerBatteryAC, 47.49, error);
    EXPECT_NEAR(m_batteryPower->powerBatteryToLoad, 47.49, error);
    EXPECT_NEAR(m_batteryPower->powerSystemToLoad, 193.83, error);
    EXPECT_NEAR(m_batteryPower->powerSystemToBattery, 0, error);
    EXPECT_NEAR(m_batteryPower->powerSystemToGrid, 0, error);
    EXPECT_NEAR(m_batteryPower->powerGridToBattery, 0, error);
    EXPECT_NEAR(m_batteryPower->powerGridToLoad, 58.68, error);
    EXPECT_NEAR(m_batteryPower->powerConversionLoss, 8.68, error);
    EXPECT_NEAR(m_batteryPower->powerSystemLoss, 0.0, error);

    // Redo the above, with losses: PV and Grid Charging Scenario
    m_batteryPower->canSystemCharge = true;
    m_batteryPower->powerSystem = 300;
    m_batteryPower->powerLoad = 200;
    m_batteryPower->powerSystemLoss = 1.0;
    m_batteryPowerFlow->initialize(50);
    m_batteryPowerFlow->calculate();

    EXPECT_NEAR(m_batteryPower->powerBatteryAC, -102.04, error);
    EXPECT_NEAR(m_batteryPower->powerSystemToLoad, 190.80, error);
    EXPECT_NEAR(m_batteryPower->powerSystemToBattery, 102.04, error);
    EXPECT_NEAR(m_batteryPower->powerGridToBattery, 0.00, error);
    EXPECT_NEAR(m_batteryPower->powerConversionLoss, 8.19, error);
    EXPECT_NEAR(m_batteryPower->powerSystemLoss, 1.0, error);

    // Exclusive Grid Charging Scenario
    m_batteryPower->canGridCharge = true;
    m_batteryPower->canSystemCharge = false;
    m_batteryPower->powerSystem = 300;
    m_batteryPower->powerLoad = 200;
    m_batteryPowerFlow->initialize(50);
    m_batteryPowerFlow->calculate();

    EXPECT_NEAR(m_batteryPower->powerBatteryAC, -105.33, error);
    EXPECT_NEAR(m_batteryPower->powerSystemToLoad, 200, error);
    EXPECT_NEAR(m_batteryPower->powerSystemToBattery, 0, error);
    EXPECT_NEAR(m_batteryPower->powerSystemToGrid, 89.66, error);
    EXPECT_NEAR(m_batteryPower->powerGridToBattery, 105.33, error);
    EXPECT_NEAR(m_batteryPower->powerConversionLoss, 8.19, error);
    EXPECT_NEAR(m_batteryPower->powerSystemLoss, 1.0, error);

    // Discharging Scenario
    m_batteryPower->canDischarge = true;
    m_batteryPower->powerSystem = 200;
    m_batteryPower->powerLoad = 300;
    m_batteryPowerFlow->initialize(50);
    m_batteryPowerFlow->calculate();

    EXPECT_NEAR(m_batteryPower->powerBatteryAC, 46.52, error);
    EXPECT_NEAR(m_batteryPower->powerBatteryToLoad, 46.52, error);
    EXPECT_NEAR(m_batteryPower->powerSystemToLoad, 193.83, error);
    EXPECT_NEAR(m_batteryPower->powerSystemToBattery, 0, error);
    EXPECT_NEAR(m_batteryPower->powerSystemToGrid, 0, error);
    EXPECT_NEAR(m_batteryPower->powerGridToBattery, 0, error);
    EXPECT_NEAR(m_batteryPower->powerGridToLoad, 59.65, error);
    EXPECT_NEAR(m_batteryPower->powerConversionLoss, 8.65, error);
    EXPECT_NEAR(m_batteryPower->powerSystemLoss, 1.0, error);
}
