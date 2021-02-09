#ifndef __LIB_TIME_H__
#define __LIB_TIME_H__

#include <vector>
#include <cstddef>
#include "lib_util.h"

/**
Function takes lifetime (multi-year) vector and single year vector of possibly lower time resolution
and returns the single year vector scaled to lifetime at the time resolution of the lifetime input vector
Optionally can scale single year vector by annual escalation factors in scale_factor
*/
template <typename T>
void single_year_to_lifetime_interpolated(
	bool is_lifetime,
	size_t n_years,
	size_t n_lifetime,
	std::vector<T> singleyear_vector,
    std::vector<T> scale_factor,
    double interpolation_factor,
	std::vector<T> &lifetime_from_singleyear_vector,
	size_t &n_rec_single_year,
	double &dt_hour);

/**
Function takes in a weekday and weekend schedule, plus the period values and an optional multiplier and returns
a vector
*/
template<typename T>
std::vector<T> flatten_diurnal(util::matrix_t<size_t> weekday_schedule, util::matrix_t<size_t> weekend_schedule, size_t steps_per_hour, std::vector<T> period_values, T multiplier = 1.0);

/**
Function takes input values, desired steps per hour, and an optional multiplier and returns
a vector
*/
template<typename T>
std::vector<T> extrapolate_timeseries(std::vector<T> input_values, size_t steps_per_hour, T multiplier = 1.0);

#endif // !__LIB_TIME_H__

