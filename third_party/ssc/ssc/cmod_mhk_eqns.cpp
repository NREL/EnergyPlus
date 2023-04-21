#include <math.h>

#include "vartab.h"

#include "cmod_mhk_eqns.h"
#pragma warning(disable: 4297)  // ignore warning: 'function assumed not to throw an exception but does'

void me_array_cable_length(ssc_data_t data)
{
    auto vt = static_cast<var_table*>(data);
    if (!vt){
        throw std::runtime_error("ssc_data_t data invalid");
    }

    double devices_per_row, device_spacing_in_row, number_rows, row_spacing, cable_system_overbuild, floating_array, export_cable_redundancy, water_depth, number_devices, distance_to_shore;

    vt_get_number(vt, "devices_per_row", &devices_per_row);
    vt_get_number(vt, "device_spacing_in_row", &device_spacing_in_row);
    vt_get_number(vt, "number_rows", &number_rows);
    vt_get_number(vt, "row_spacing", &row_spacing);
    vt_get_number(vt, "cable_system_overbuild", &cable_system_overbuild);
    vt_get_number(vt, "floating_array", &floating_array);
    vt_get_number(vt, "export_cable_redundancy", &export_cable_redundancy);
    vt_get_number(vt, "water_depth", &water_depth);
    vt_get_number(vt, "number_devices", &number_devices);
    vt_get_number(vt, "distance_to_shore", &distance_to_shore);

		
	double length = (devices_per_row - 1) * device_spacing_in_row * number_rows + row_spacing * (number_rows - 1);
	length *= (1.0 + cable_system_overbuild / 100.0);
	var_data cablelength = var_data(length);
	vt->assign("inter_array_cable_length", cablelength);

	if (fabs(floating_array) > 0.1)
	{
		length = 1.5 * water_depth * number_devices;
		length *= (1.0 + cable_system_overbuild / 100.0);
	}
	else
	{
		length = 0;
	}
	vt->assign("riser_cable_length", var_data(length));

	if (fabs(export_cable_redundancy) > 0.1)
	{
		length = (water_depth + distance_to_shore) * 2;
		length *= (1.0 + cable_system_overbuild / 100.0);
	}
	else
	{
		length = water_depth + distance_to_shore;
		length *= (1.0 + cable_system_overbuild / 100.0);
	}
	vt->assign("export_cable_length", var_data(length));

}



