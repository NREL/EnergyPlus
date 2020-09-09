#ifndef __LIB_BATTERY_PROPERTIES_H__
#define __LIB_BATTERY_PROPERTIES_H__

#include <gtest/gtest.h>

#include <lib_util.h>
#include <lib_utility_rate.h>


// Generic Lithium-ion battery system to be re-used
class BatteryProperties : public ::testing::Test
{
public:

	// capacity
	double q;
	double SOC_min;
	double SOC_max;
	double SOC_init;

	// voltage
	int n_series;
	int n_strings;
	double Vnom_default;
	double Vfull;
	double Vexp;
	double Vnom;
	double Qfull;
	double Qexp;
	double Qnom;
	double C_rate;
	double resistance;

	// lifetime
	util::matrix_t<double> cycleLifeMatrix;
	util::matrix_t<double> calendarLifeMatrix;
	int calendarChoice;
	int replacementOption;
	double replacementCapacity;

	// thermal
	double mass;
	double length;
	double width;
	double height;
	double Cp;
	double h;
	std::vector<double> T_room;
	util::matrix_t<double> capacityVsTemperature;

	// losses
	std::vector<double> monthlyLosses;
	std::vector<double> fullLosses;
	std::vector<double> fullLossesMinute;
	int lossChoice;

	// battery
	int chemistry;
	double dtHour;

	void SetUp() override
	{
		// capacity
		q = 1000;
		SOC_init = 50;
		SOC_min = 15;
		SOC_max = 95;

		// voltage
		n_series = 139;
		n_strings = 89;
		Vnom_default = 3.6;
		Vfull = 4.1;
		Vexp = 4.05;
		Vnom = 3.4;
		Qfull = 2.25;
		Qexp = 0.04;
		Qnom = 2.0;
		C_rate = 0.2;
		resistance = 0.2;

		// lifetime
		double vals[] = { 20, 0, 100, 20, 5000, 80, 20, 10000, 60, 80, 0, 100, 80, 1000, 80, 80, 2000, 60 };
		cycleLifeMatrix.assign(vals, 6, 3);
		double vals2[] = { 0, 100, 3650, 80, 7300, 50 };
		calendarLifeMatrix.assign(vals2, 3, 2);
		calendarChoice = 1;
		replacementOption = 0;

		// thermal
		mass = 507;
		length = 0.58;
		width = 0.58;
		height = 0.58;
		Cp = 1004;
		h = 500;
		for (size_t i = 0; i < 8760; i++) {
			T_room.push_back(20 + 273.15);
		}
		double vals3[] = { -10, 60, 0, 80, 25, 100, 40, 100 };
		capacityVsTemperature.assign(vals3, 4, 2);

		// losses
		for (size_t m = 0; m < 12; m++) {
			monthlyLosses.push_back((double)m);
		}
		for (size_t i = 0; i < 8760; i++) {
			fullLosses.push_back(0);
		}
		for (size_t i = 0; i < 8760 * 60; i++) {
			fullLossesMinute.push_back(0);
		}
		lossChoice = 0;

		// battery
		chemistry = 1;
		dtHour = 1.0;
	}

	// nothing to do
	void TearDown(){}

};

#endif