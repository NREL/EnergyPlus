#ifndef MHK_WAVE_INPUTS_H_
#define MHK_WAVE_INPUTS_H_

#include <stdio.h>
#include "../test/input_cases/code_generator_utilities.h"

void wave_inputs(ssc_data_t &data) {
	char device_matrix[256];
	char resource_matrix[256];
	int nb2 = sprintf(resource_matrix, "%s/test/input_cases/mhk/wave_resource_matrix.csv", SSCDIR);
	int nb3 = sprintf(device_matrix, "%s/test/input_cases/mhk/wave_power_matrix.csv", SSCDIR);

	set_matrix(data, "wave_resource_matrix", resource_matrix, 21, 22);
	set_matrix(data, "wave_power_matrix", device_matrix, 21, 22);

	ssc_data_set_number(data, "number_devices", 1);
	ssc_data_set_number(data, "system_capacity", 286);
	ssc_data_set_number(data, "device_rated_power", 286);
	ssc_data_set_number(data, "loss_array_spacing", 0);
	ssc_data_set_number(data, "loss_resource_overprediction", 0);
	ssc_data_set_number(data, "loss_transmission", 2);
	ssc_data_set_number(data, "loss_downtime", 5);
	ssc_data_set_number(data, "loss_additional", 0);

	ssc_data_set_number(data, "capital_cost", 12105156);
	ssc_data_set_number(data, "fixed_operating_cost", 1239344.125);
	ssc_data_set_number(data, "variable_operating_cost", 0);
	ssc_data_set_number(data, "fixed_charge_rate", 0.1080000028014183);

}

#endif