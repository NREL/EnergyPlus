#ifndef _CMOD_WINDPOWER_BUILDER_H_
#define _CMOD_WINDPOWER_BUILDER_H_

#include "sscapi.h"

#ifdef __cplusplus
extern "C" {
#endif

static const char* Turbine_calculate_powercurve_doc =
    "Calculates the power produced by a wind turbine at windspeeds incremented by 0.25 m/s\\n\\n"
    "Input: var_table with key-value pairs\\n"
    "     'turbine_size': double [kW]\\n"
    "     'rotor_diameter': int [m]\\n"
    "     'elevation': double [m], required if using Weibull resource model, otherwise 0\\n"
    "     'max_cp': double max Cp [-],\\n"
    "     'max_tip_speed': double [m/s]\\n"
    "     'max_tip_sp_ratio': double max tip speed ratio [-]\\n"
    "     'cut_in': double cut in speed [m/s]\\n"
    "     'cut_out': double cut out speed [m/s]\\n"
    "     'drive_train': int 0: 3 Stage Planetary, 1: Single Stage - Low Speed Generator, 2: Multi-Generator, 3: Direct Drive\\n\\n"
    "Output: key-value pairs added to var_table\\n"
    "     'wind_turbine_powercurve_windspeeds': array [m/s]\\n"
    "     'wind_turbine_powercurve_powerout': array [m/s]\\n"
    "     'rated_wind_speed': double [m/s[\\n"
    "     'hub_efficiency': array [m/s]";

SSCEXPORT void Turbine_calculate_powercurve(ssc_data_t data);


#ifdef __cplusplus
}
#endif

#endif
