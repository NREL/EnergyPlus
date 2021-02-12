#ifndef _WINDPOWER_CASES_
#define _WINDPOWER_CASES_

#include <stdio.h>
#include <string>
#include "code_generator_utilities.h"

int windpower_nofinancial_testfile(ssc_data_t &data)
{
	ssc_module_exec_set_print(0);
	if (data == NULL)
	{
		printf("error: out of memory."); 
		return -1;
	}

	char filepath[1024];
	int n1 = sprintf(filepath, "%s/test/input_docs/wind.srw", std::getenv("SSCDIR"));

	ssc_data_set_string(data, "wind_resource_filename", filepath);
	ssc_data_set_number(data, "wind_resource_shear", (ssc_number_t)0.140);
	ssc_data_set_number(data, "wind_resource_turbulence_coeff", (ssc_number_t)0.100);
	ssc_data_set_number(data, "system_capacity", 48000);
	ssc_data_set_number(data, "wind_resource_model_choice", 0);
	ssc_data_set_number(data, "weibull_reference_height", 50);
	ssc_data_set_number(data, "weibull_k_factor", 2);
	ssc_data_set_number(data, "weibull_wind_speed", 7.25);
	ssc_data_set_number(data, "wind_turbine_rotor_diameter", 77);

	double dpowercurve_windspeed[161] = { 0, 0.25, 0.5, 0.75, 1, 1.25, 1.5, 1.75, 2, 2.25, 2.5, 2.75, 3, 3.25, 3.5, 3.75, 4, 4.25, 4.5, 4.75, 5, 5.25, 5.5, 5.75, 6, 6.25, 6.5, 6.75, 7, 7.25, 7.5, 7.75, 8, 8.25, 8.5, 8.75, 9, 9.25, 9.5, 9.75, 10, 10.25, 10.5, 10.75, 11, 11.25, 11.5, 11.75, 12, 12.25, 12.5, 12.75, 13, 13.25, 13.5, 13.75, 14, 14.25, 14.5, 14.75, 15, 15.25, 15.5, 15.75, 16, 16.25, 16.5, 16.75, 17, 17.25, 17.5, 17.75, 18, 18.25, 18.5, 18.75, 19, 19.25, 19.5, 19.75, 20, 20.25, 20.5, 20.75, 21, 21.25, 21.5, 21.75, 22, 22.25, 22.5, 22.75, 23, 23.25, 23.5, 23.75, 24, 24.25, 24.5, 24.75, 25, 25.25, 25.5, 25.75, 26, 26.25, 26.5, 26.75, 27, 27.25, 27.5, 27.75, 28, 28.25, 28.5, 28.75, 29, 29.25, 29.5, 29.75, 30, 30.25, 30.5, 30.75, 31, 31.25, 31.5, 31.75, 32, 32.25, 32.5, 32.75, 33, 33.25, 33.5, 33.75, 34, 34.25, 34.5, 34.75, 35, 35.25, 35.5, 35.75, 36, 36.25, 36.5, 36.75, 37, 37.25, 37.5, 37.75, 38, 38.25, 38.5, 38.75, 39, 39.25, 39.5, 39.75, 40 };
	double dpowercurve_powerout[161] = { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 21.320, 33.510, 45.690, 65.210, 79.830, 104.25, 128.660, 157.970, 187.270, 216.580, 250.780, 292.320, 333.850, 375.400, 426.720, 475.600, 534.270, 597.810, 656.490, 724.940, 798.290, 871.630, 940.080, 1010, 1060, 1130, 1190, 1240, 1290, 1330, 1370, 1390, 1410, 1430, 1440, 1460, 1470, 1475, 1480, 1485, 1490, 1495, 1500, 1500, 1500, 1500, 1500, 1500, 1500, 1500, 1500, 1500, 1500, 1500, 1500, 1500, 1500, 1500, 1500, 1500, 1500, 1500, 1500, 1500, 1500, 1500, 1500, 1500, 1500, 1500, 1500, 1500, 1500, 1500, 1500, 1500, 1500, 1500, 1500, 1500, 1500, 1500, 1500, 1500, 1500, 1500, 1500, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 };
	double dxcoord[32] = { 0, 616, 1232, 1848, 2464, 3080, 3696, 4312, 308, 924, 1540, 2156, 2772, 3388, 4004, 4620, 0, 616, 1232, 1848, 2464, 3080, 3696, 4312, 308, 924, 1540, 2156, 2772, 3388, 4004, 4620 };
	double dycoord[32] = { 0, 0, 0, 0, 0, 0, 0, 0, 616, 616, 616, 616, 616, 616, 616, 616, 1232, 1232, 1232, 1232, 1232, 1232, 1232, 1232, 1848, 1848, 1848, 1848, 1848, 1848, 1848, 1848 };

	ssc_number_t powercurve_windspeed[161];
	ssc_number_t powercurve_powerout[161];
	for (int i = 0; i < 161; i++){
		powercurve_windspeed[i] = (ssc_number_t)dpowercurve_windspeed[i];
		powercurve_powerout[i] = (ssc_number_t)dpowercurve_powerout[i];
	}
	ssc_number_t xcoord[32];
	ssc_number_t ycoord[32];
	for (int i = 0; i < 32; i++){
		xcoord[i] = (ssc_number_t)dxcoord[i];
		ycoord[i] = (ssc_number_t)dycoord[i];
	}


	ssc_data_set_array(data, "wind_turbine_powercurve_windspeeds", powercurve_windspeed, 161);
	ssc_data_set_array(data, "wind_turbine_powercurve_powerout", powercurve_powerout, 161);
	ssc_data_set_number(data, "wind_turbine_hub_ht", 80);
	ssc_data_set_number(data, "wind_turbine_max_cp", (ssc_number_t)0.450);
	ssc_data_set_array(data, "wind_farm_xCoordinates", xcoord, 32);
	ssc_data_set_array(data, "wind_farm_yCoordinates", ycoord, 32);
	ssc_data_set_number(data, "wind_farm_wake_model", 0);
	ssc_data_set_number(data, "adjust:constant", 0);
	
	return 0;
}

#endif