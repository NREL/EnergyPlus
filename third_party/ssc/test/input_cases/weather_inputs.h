#ifndef _WEATHERINPUT_
#define _WEATHERINPUT_

#include "../ssc/common.h"

/**
 * Creates resources as var_data, as opposed to resources from files, for testing use through SDK
 */

var_data* create_weatherdata_array(int length);

void free_weatherdata_array(var_data* data);

/**
 * intervalsPerHour: 1 for hourly, 2 for 30m, etc
 * nMeasurementHeights: starts at 80m, increases by 10m for next height; pres, tmp, spd, dir also increases linearly
 */
var_data* create_winddata_array(int intervalsPerHour, int nMeasurementHeights);

void free_winddata_array(var_data* data);

#endif