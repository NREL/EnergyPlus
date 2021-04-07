#include <memory>
#include <math.h>

#include "lib_fuel_cell.h"
#include "lib_fuel_cell_dispatch.h"
#include "lib_power_electronics.h"


FuelCellDispatch::FuelCellDispatch(FuelCell * fuelCell, size_t numberOfUnits, int dispatchOption, int shutdownOption, double dt_hour, double fixed_percent, 
	std::vector<double> dispatchInput_kW, std::vector<bool> canCharge, std::vector<bool> canDischarge, 
	std::map<size_t, double> discharge_percent, std::map<size_t, size_t> discharge_units, util::matrix_t<size_t> scheduleWeekday, util::matrix_t<size_t> scheduleWeekend)
	: m_powerTotal_kW(0), m_numberOfUnits(numberOfUnits), m_dispatchOption(dispatchOption), m_shutdownOption(shutdownOption), dt_hour(dt_hour), m_fixed_percent(fixed_percent * 0.01), 
	m_dispatchInput_kW(dispatchInput_kW), m_canCharge(canCharge), m_canDischarge(canDischarge), 
	m_discharge_percent(discharge_percent), m_discharge_units(discharge_units),
	m_scheduleWeekday(scheduleWeekday), m_scheduleWeekend(scheduleWeekend)
{
	// Convert percentages to fractions
	for (auto percent = m_discharge_percent.begin(); percent != m_discharge_percent.end(); percent++) {
		percent->second *= 0.01;
	}
	// Could have no units
	if (numberOfUnits > 0) {
		m_fuelCellVector.push_back(fuelCell);
	}
	// Create duplicate fuel cell units
	for (size_t fc = 1; fc < numberOfUnits; fc++) {
		m_fuelCellVector.push_back(new FuelCell(*fuelCell));
	}
	// Check if fuel cell needs initialized for time step zero based on dispatch
	for (size_t fc = 0; fc < numberOfUnits; fc++){

		if (!m_fuelCellVector[fc]->isInitialized()) {
			double power_kW = 0;
			if (m_dispatchOption == FuelCellDispatch::FC_DISPATCH_OPTION::FIXED) {
				power_kW = m_fuelCellVector[fc]->getMaxPowerOriginal() * m_fixed_percent;
			}
			// Assume running at full
			else if (m_dispatchOption == FuelCellDispatch::FC_DISPATCH_OPTION::LOAD_FOLLOW) {
				power_kW = m_fuelCellVector[fc]->getMaxPowerOriginal();
			}
			// Assume running at hour 1 level
			else if (m_dispatchOption == FuelCellDispatch::FC_DISPATCH_OPTION::MANUAL) {
				size_t period = m_scheduleWeekday(0, 0);

				if (!util::weekday(0)) {
					period = m_scheduleWeekend(0, 0);
				}
				double discharge_percent_init = 0;
				bool canDischargeInit = m_canDischarge[period - 1];

				size_t numberOfUnitsToRun = 0;
				if (canDischargeInit) {
					numberOfUnitsToRun = m_discharge_units[period - 1];
					discharge_percent_init = m_discharge_percent[period - 1];

					if (numberOfUnitsToRun > m_numberOfUnits) {
						numberOfUnitsToRun = m_numberOfUnits;
					}
				}
				double on = fc < numberOfUnitsToRun ? 1.0 : 0.0;
				power_kW = on * discharge_percent_init * m_fuelCellVector[fc]->getMaxPowerOriginal();
			}
			// Assume running at hour 1 level
			else {
				 power_kW = m_dispatchInput_kW[0];
			}

			// initialize
			m_fuelCellVector[fc]->initializeHourZeroPower(power_kW);
		}
	}



	// Set up battery power flow
	std::unique_ptr<BatteryPowerFlow> tmp(new BatteryPowerFlow(dt_hour));
	m_batteryPowerFlow = std::move(tmp);
	m_batteryPower = m_batteryPowerFlow->getBatteryPower();
	m_batteryPower->connectionMode = ChargeController::AC_CONNECTED;
}
FuelCellDispatch::~FuelCellDispatch() {
	for (size_t fc = 1; fc < m_numberOfUnits; fc++) {
		if (m_fuelCellVector[fc]) {
			delete m_fuelCellVector[fc];
			m_fuelCellVector[fc] = nullptr;
		}
	}
}

void FuelCellDispatch::runSingleTimeStep(size_t hour_of_year, size_t year_idx, double powerSystem_kWac, double powerLoad_kWac) {

	m_powerTotal_kW = 0;
	m_powerMaxPercentAverage_percent = 0;
	m_loadAverage_percent = 0;
	m_efficiencyAverage_percent = 0;
	m_powerThermalTotal_kW = 0;
	m_fuelConsumedTotal_MCf = 0;

	// Specified to run at fixed percent of original unit max kW
	if (m_dispatchOption == FuelCellDispatch::FC_DISPATCH_OPTION::FIXED) {
		for (size_t fc = 0; fc < m_fuelCellVector.size(); fc++) {
			double power_kW = m_fuelCellVector[fc]->getMaxPowerOriginal() * m_fixed_percent;
			m_fuelCellVector[fc]->runSingleTimeStep(power_kW);
			m_powerTotal_kW += m_fuelCellVector[fc]->getPower();
			m_powerMaxPercentAverage_percent += m_fuelCellVector[fc]->getPowerMaxPercent() / m_numberOfUnits;
			m_loadAverage_percent += m_fuelCellVector[fc]->getPercentLoad() / m_numberOfUnits;
			m_efficiencyAverage_percent += m_fuelCellVector[fc]->getElectricalEfficiency() * 100.0 / m_numberOfUnits;
			m_powerThermalTotal_kW += m_fuelCellVector[fc]->getPowerThermal();
			m_fuelConsumedTotal_MCf += m_fuelCellVector[fc]->getFuelConsumption();
		}
	}
	// Specified to follow load, fuel cell attempts to make up difference of system - load
	else if (m_dispatchOption == FuelCellDispatch::FC_DISPATCH_OPTION::LOAD_FOLLOW) {
		for (size_t fc = 0; fc < m_fuelCellVector.size(); fc++) {
			double power_kW = fmax(0, powerLoad_kWac - powerSystem_kWac);
			m_fuelCellVector[fc]->runSingleTimeStep(power_kW / m_fuelCellVector.size());
			m_powerTotal_kW += m_fuelCellVector[fc]->getPower();
			m_powerMaxPercentAverage_percent += m_fuelCellVector[fc]->getPowerMaxPercent() / m_numberOfUnits;
			m_loadAverage_percent += m_fuelCellVector[fc]->getPercentLoad() / m_numberOfUnits;
			m_efficiencyAverage_percent += m_fuelCellVector[fc]->getElectricalEfficiency()* 100.0 / m_numberOfUnits;
			m_powerThermalTotal_kW += m_fuelCellVector[fc]->getPowerThermal();
			m_fuelConsumedTotal_MCf += m_fuelCellVector[fc]->getFuelConsumption();
		}
	}
	// Specified to follow manual dispatch input (add logic)
	else if (m_dispatchOption == FuelCellDispatch::FC_DISPATCH_OPTION::MANUAL) {
		
		// Get period (1-based)
		size_t month, hour = 0;
		util::month_hour(hour_of_year, month, hour);
		size_t period = m_scheduleWeekday(month - 1, hour - 1);
		
		if (!util::weekday(hour_of_year)) {
			period = m_scheduleWeekend(month - 1, hour - 1);
		}

		// Get discharge properties for period
		size_t numberOfUnitsToRun = 0;
		double discharge_percent = 0;
		bool canDischarge = m_canDischarge[period - 1];

		if (canDischarge) {
			numberOfUnitsToRun = m_discharge_units[period - 1];
			discharge_percent = m_discharge_percent[period - 1];

			if (numberOfUnitsToRun > m_numberOfUnits) {
				numberOfUnitsToRun = m_numberOfUnits;
			}
		}

		// Iterate over units
		for (size_t fc = 0; fc < m_numberOfUnits; fc++) {
			
			double on = fc < numberOfUnitsToRun ? 1.0 : 0.0;
			double power_kW = on * discharge_percent * m_fuelCellVector[fc]->getMaxPowerOriginal();

			m_fuelCellVector[fc]->runSingleTimeStep(power_kW);
			m_fuelConsumedTotal_MCf += m_fuelCellVector[fc]->getFuelConsumption();
			m_powerTotal_kW += m_fuelCellVector[fc]->getPower();
			m_powerMaxPercentAverage_percent += m_fuelCellVector[fc]->getPowerMaxPercent() / m_numberOfUnits;
			m_loadAverage_percent += m_fuelCellVector[fc]->getPercentLoad() / m_numberOfUnits;
			m_efficiencyAverage_percent += m_fuelCellVector[fc]->getElectricalEfficiency()* 100.0 / m_numberOfUnits;

		}
	}
	// Input dispatch
	else {
		for (size_t fc = 0; fc < m_fuelCellVector.size(); fc++) {
			double power_kW = m_dispatchInput_kW[year_idx];
			m_fuelCellVector[fc]->runSingleTimeStep(power_kW);
			m_fuelConsumedTotal_MCf += m_fuelCellVector[fc]->getFuelConsumption();
			m_powerTotal_kW += m_fuelCellVector[fc]->getPower();
			m_powerMaxPercentAverage_percent += m_fuelCellVector[fc]->getPowerMaxPercent() / m_numberOfUnits;
			m_loadAverage_percent += m_fuelCellVector[fc]->getPercentLoad() / m_numberOfUnits;
			m_efficiencyAverage_percent += m_fuelCellVector[fc]->getElectricalEfficiency() * 100.0 / m_numberOfUnits;
			m_powerThermalTotal_kW += m_fuelCellVector[fc]->getPowerThermal();
		}
	}
	m_batteryPower->powerSystem = powerSystem_kWac;
	m_batteryPower->powerLoad = powerLoad_kWac;
	m_batteryPower->powerFuelCell = m_powerTotal_kW;
	m_batteryPowerFlow->calculate();
}

void FuelCellDispatch::setDispatchOption(int dispatchOption) {
	m_dispatchOption = dispatchOption;
}

void FuelCellDispatch::setFixedDischargePercentage(double discharge_percent) {
	m_fixed_percent = discharge_percent * 0.01;
}

void FuelCellDispatch::setManualDispatchUnits(std::map<size_t, size_t> unitsByPeriod) {
	if (unitsByPeriod.size() == m_discharge_units.size()) {
		m_discharge_units = unitsByPeriod;
	}
}

double FuelCellDispatch::getPower(){
	return m_powerTotal_kW;
}

double FuelCellDispatch::getPowerMaxPercent() {
	return m_powerMaxPercentAverage_percent;
}

double FuelCellDispatch::getPercentLoad() {
	return m_loadAverage_percent;
}

double FuelCellDispatch::getElectricalEfficiencyPercent() {
	return m_efficiencyAverage_percent;
}

double FuelCellDispatch::getPowerThermal() {
	return m_powerThermalTotal_kW;
}

double FuelCellDispatch::getFuelConsumption() {
	return m_fuelConsumedTotal_MCf;
}