#ifndef _SHARED_RATE_DATA_H_
#define _SHARED_RATE_DATA_H_

#include <lib_utility_rate_equations.h>

void set_up_default_commercial_rate_data(rate_data& data);

// Rate is PG&E's EV9A from https://www.pge.com/includes/docs/pdfs/shared/environment/pge/cleanair/electricdrivevehicles/PEV_rate_options.pdf
void set_up_pge_residential_rate_data(rate_data& data);

// Modify typical default schedule so we can test August's behavior in "January" and avoid long forecast vectors
void set_up_residential_1_4_peak(rate_data& data, size_t steps_per_hour);

void set_up_time_series(rate_data& data);

void set_up_simple_demand_charge(rate_data& data);

#endif // _SHARED_RATE_DATA_H_
