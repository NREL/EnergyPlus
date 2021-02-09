#ifndef __LIB_BATTERY_POWERFLOW_TEST_H__
#define __LIB_BATTERY_POWERFLOW_TEST_H__

#include <gtest/gtest.h>
#include <lib_fuel_cell.h>
#include <lib_fuel_cell_dispatch.h>
#include <lib_util.h>


class FuelCellProperties : public ::testing::Test
{
protected:

	size_t numberOfUnits;
	double unitPowerMax_kW;
	double unitPowerMin_kW;
	double startup_hours;
	bool is_started;
	double shutdown_hours;
	double dynamicResponseUp_kWperHour;
	double dynamicResponseDown_kWperHour;
	double degradation_kWperHour;
	double degradationRestart_kW;
	size_t replacementOption;
	double replacement_percent;
	std::vector<size_t> replacementSchedule;
	util::matrix_t<size_t> shutdownTable;
	size_t efficiencyChoice;
	util::matrix_t<double> efficiencyTable;
	double lowerHeatingValue_BtuPerFt3;
	double higherHeatingValue_BtuPerFt3;
	double availableFuel_Mcf;
	int shutdownOption;
	int dispatchOption;
	double dt_hour;
	double fixed_percent;

	std::vector<double> dispatchInput_kW;
	std::vector<bool> canCharge;
	std::vector<bool> canDischarge;
	std::map<size_t, double> discharge_percent;
	std::map<size_t, size_t> discharge_units;

	util::matrix_t<size_t> scheduleWeekday;
	util::matrix_t<size_t> scheduleWeekend;


	void SetUp()
	{
		numberOfUnits = 1;
		unitPowerMax_kW = 100;
		unitPowerMin_kW = 20;
		startup_hours = 8;
		is_started = false;
		shutdown_hours = 8;
		dynamicResponseUp_kWperHour = 20;
		dynamicResponseDown_kWperHour = 10;
		degradation_kWperHour = 0.01;
		degradationRestart_kW = 5;
		replacementOption = 0;
		replacement_percent = 50;
		replacementSchedule.push_back(0);
		lowerHeatingValue_BtuPerFt3 = 1033;
		higherHeatingValue_BtuPerFt3 = 1033;
		availableFuel_Mcf = 10000;
		shutdownOption = FuelCell::FC_SHUTDOWN_OPTION::IDLE;
		dispatchOption = FuelCellDispatch::FC_DISPATCH_OPTION::FIXED;
		dt_hour = 1.0;
		fixed_percent = 40;

		const double tmpValues[33] = { 0,0,50,16,21,50,25,25,50,34,32,50,44,37,50,53,42,50,62,47,49,72,50,48,82,52,47,90,52,46,100,51,45 };
		efficiencyTable.assign(tmpValues, 11, 3);
		efficiencyChoice = 1;

		canCharge.push_back(1);
		canDischarge.push_back(1);
		discharge_percent[0] = 40;
		discharge_units[0] = 1;

		scheduleWeekday.resize_fill(12, 24, 1);
		scheduleWeekend.resize_fill(12, 24, 1);

		shutdownTable.resize_fill(1, 2, 0);

		for (size_t t = 0; t < 8760; t++) {
			dispatchInput_kW.push_back(50);
		}
		
	}
};

/**
* \class FuelCellTest
*
* This class contains the setup and teardown structure required to test the fuel cell model
*
*/
class FuelCellTest : public FuelCellProperties
{
protected:

	FuelCell * fuelCell;
//	FuelCell * fuelCellStarted;
	FuelCellDispatch * fuelCellDispatch;
//	FuelCellDispatch * fuelCellDispatchStarted;
	FuelCellDispatch * fuelCellDispatchMultiple;
//	FuelCellDispatch * fuelCellDispatchMultipleStarted;
	size_t n_multipleFuelCells = 4;
	
	double dt_subHourly = 0.25;
	FuelCell * fuelCellSubHourly;
	FuelCellDispatch * fuelCellDispatchSubhourly;
	
public:

	void SetUp()
	{
		FuelCellProperties::SetUp();
		
		fuelCell = new FuelCell(unitPowerMax_kW, unitPowerMin_kW, startup_hours, is_started, shutdown_hours,
			dynamicResponseUp_kWperHour, dynamicResponseDown_kWperHour,
			degradation_kWperHour, degradationRestart_kW,
			replacementOption, replacement_percent, replacementSchedule,
			shutdownTable, efficiencyChoice, efficiencyTable,
			lowerHeatingValue_BtuPerFt3, higherHeatingValue_BtuPerFt3, availableFuel_Mcf, shutdownOption, dt_hour);

/*		fuelCellStarted = new FuelCell(unitPowerMax_kW, unitPowerMin_kW, startup_hours, true, shutdown_hours,
			dynamicResponseUp_kWperHour, dynamicResponseDown_kWperHour,
			degradation_kWperHour, degradationRestart_kW,
			replacementOption, replacement_percent, replacementSchedule,
			shutdownTable, efficiencyChoice, efficiencyTable,
			lowerHeatingValue_BtuPerFt3, higherHeatingValue_BtuPerFt3, availableFuel_Mcf, shutdownOption, dt_hour);
*/
		fuelCellSubHourly = new FuelCell(unitPowerMax_kW, unitPowerMin_kW, startup_hours, is_started, shutdown_hours,
			dynamicResponseUp_kWperHour, dynamicResponseDown_kWperHour,
			degradation_kWperHour, degradationRestart_kW,
			replacementOption, replacement_percent, replacementSchedule, shutdownTable, efficiencyChoice, efficiencyTable, lowerHeatingValue_BtuPerFt3, higherHeatingValue_BtuPerFt3, availableFuel_Mcf, shutdownOption, dt_subHourly);

/*		fuelCellDispatchStarted = new FuelCellDispatch(fuelCellStarted, numberOfUnits, dispatchOption, shutdownOption, dt_hour, fixed_percent,
			dispatchInput_kW, canCharge, canDischarge, discharge_percent, discharge_units, scheduleWeekday, scheduleWeekend);
*/
		fuelCellDispatch = new FuelCellDispatch(fuelCell, numberOfUnits, dispatchOption, shutdownOption, dt_hour, fixed_percent,
			dispatchInput_kW, canCharge, canDischarge, discharge_percent, discharge_units, scheduleWeekday, scheduleWeekend);

		fuelCellDispatchSubhourly = new FuelCellDispatch(fuelCellSubHourly, numberOfUnits, dispatchOption, shutdownOption, dt_subHourly, fixed_percent,
			dispatchInput_kW, canCharge, canDischarge, discharge_percent, discharge_units, scheduleWeekday, scheduleWeekend);

		discharge_units[0] = n_multipleFuelCells;
		fuelCellDispatchMultiple = new FuelCellDispatch(fuelCell, n_multipleFuelCells, dispatchOption, shutdownOption, dt_hour, fixed_percent,
			dispatchInput_kW, canCharge, canDischarge, discharge_percent, discharge_units, scheduleWeekday, scheduleWeekend);

/*		fuelCellDispatchMultipleStarted = new FuelCellDispatch(fuelCellStarted, n_multipleFuelCells, dispatchOption, shutdownOption, dt_hour, fixed_percent,
			dispatchInput_kW, canCharge, canDischarge, discharge_percent, discharge_units, scheduleWeekday, scheduleWeekend);
*/	}
	void TearDown()
	{
		if (fuelCell) {
			delete fuelCell;
			fuelCell = nullptr;
		}
		if (fuelCellDispatch) {
			delete fuelCellDispatch;
			fuelCellDispatch = nullptr;
		}
		if (fuelCellDispatchMultiple) {
			delete fuelCellDispatchMultiple;
			fuelCellDispatchMultiple = nullptr;
		}
/*		if (fuelCellStarted) {
			delete fuelCellStarted;
			fuelCellStarted = nullptr;
		}
		if (fuelCellDispatchStarted) {
			delete fuelCellDispatchStarted;
			fuelCellDispatchStarted = nullptr;
		}
		if (fuelCellDispatchMultipleStarted) {
			delete fuelCellDispatchMultipleStarted;
			fuelCellDispatchMultipleStarted = nullptr;
		}
*/	}

};



#endif