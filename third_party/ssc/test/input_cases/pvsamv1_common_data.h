#ifndef _PVSAMV1_COMMON_DATA_H_
#define _PVSAMV1_COMMON_DATA_H_

#include <stdio.h>

#include "code_generator_utilities.h"

extern char solar_resource_path[256];
extern char solar_resource_path_15_min[256];
extern char load_profile_path[256];
extern char target_power_path[256];
extern char sell_rate_path[256];
extern char subarray1_shading[256];
extern char subarray2_shading[256];
extern char temperature_path[256];
extern char solar_resource_path_15min_fail[256];


/**
*  Default data for no-financial pvsamv1 run that can be further modified
*/
void pvsamv_nofinancial_default(ssc_data_t& data);

/**
*  Default data for belpe run that can be further modified
*  Includes a critical load for resiliency. Will increase runtimes if used
*/
void belpe_default(ssc_data_t& data);

/**
* Default battery data that can be further modified
*/
void battery_data_default(ssc_data_t& data);

/**
*  Default data for pvsamv1 residential run that can be further modified
*  Also runs battery_data_default
*/
void pvsamv1_with_residential_default(ssc_data_t& data);

/**
*  Default data for utility_rate5 run that can be further modified
*/
void utility_rate5_default(ssc_data_t& data);

/**
*  Default data for cashloan run that can be further modified
*/
void cashloan_default(ssc_data_t& data);

void setup_residential_utility_rates(ssc_data_t& data);

#endif
