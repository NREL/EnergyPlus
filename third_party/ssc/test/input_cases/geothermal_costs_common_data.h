#ifndef _GEOTHERMAL_COSTS_COMMON_DATA_H_
#define _GEOTHERMAL_COSTS_COMMON_DATA_H_

#include <stdio.h>
#include "code_generator_utilities.h"

void geothermal_costs_default(ssc_data_t &data) {

	ssc_data_set_number(data, "gross_output", 5435.705);
	ssc_data_set_number(data, "design_temp", 200.0);
	ssc_data_set_number(data, "eff_secondlaw", 0.405130);
	ssc_data_set_number(data, "conversion_type", 0);
	ssc_data_set_number(data, "qRejectTotal", 178754.609375);
	ssc_data_set_number(data, "qCondenser", 165823.921875);
	ssc_data_set_number(data, "v_stage_1", 0);
	ssc_data_set_number(data, "v_stage_2", 0);
	ssc_data_set_number(data, "v_stage_3", 0.035296);
	ssc_data_set_number(data, "GF_flowrate", 665994.875000);
	ssc_data_set_number(data, "qRejectByStage_1", 10269.627930);
	ssc_data_set_number(data, "qRejectByStage_2", 2573.040039);
	ssc_data_set_number(data, "qRejectByStage_3", 88.023651);
	ssc_data_set_number(data, "ncg_condensate_pump", 0.000192);
	ssc_data_set_number(data, "cw_pump_work", 0.024510);
	ssc_data_set_number(data, "pressure_ratio_1", 0.014923);
	ssc_data_set_number(data, "pressure_ratio_2", 0.036414);
	ssc_data_set_number(data, "pressure_ratio_3", 0.088857);
	ssc_data_set_number(data, "condensate_pump_power", 0.009464);
	ssc_data_set_number(data, "cwflow", 6632.956543);
	ssc_data_set_number(data, "cw_pump_head", 88.076920);
	ssc_data_set_number(data, "spec_vol", 6.419918);
	ssc_data_set_number(data, "x_hp", 0.105419);
	ssc_data_set_number(data, "hp_flash_pressure", 67.795647);
	ssc_data_set_number(data, "spec_vol_lp", 25.282135);
	ssc_data_set_number(data, "x_lp", 0.089993);
	ssc_data_set_number(data, "lp_flash_pressure", 15.700000);
	ssc_data_set_number(data, "flash_count", 2);
	//ssc_data_set_number(data, "baseline_cost", -1);
}

#endif