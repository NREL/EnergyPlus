#include "lib_time.h"

/**
*  \function  single_year_to_lifetime_interpolated
*
*  Takes information about the desired lifetime vector, a single-year vector, and returns the single-year vector
*  as a lifetime length vector, interpolated as needed.  As an example, consider that solar generation is passed in
*  as a 15-minute, 25 year vector, and the electric load is currently a single-year and hourly.  The function will
*  take the single-year hourly load, interpolate it to 15-minutes, and scale to 25 years.
*
* \param[in] is_lifetime (true/false)
* \param[in] n_years (1 - 100)
* \param[in] n_lifetime (length of desired lifetime vector)
* \param[in] singleyear_vector (the single year vector to scale to lifetime and interpolate)
* \param[in] scale_factor (scaling factors for years 2 through n, must be length n prior to calling this function)
* \param[in] interpolation_factor (scaling as needed between single year records and the interpolated records. Given annual data, should be 1 for power, and 1/dt_hour for energy)
* \param[out] lifetime_from_singleyear_vector (the lifetime, interpolated vector)
* \param[out] n_rec_single_year (the length of a single year vector, interpolated at the lifetime vector timescale)
* \param[out] dt_hour (the time step in hours)
*/
template <class T>
void single_year_to_lifetime_interpolated(
	bool is_lifetime,
	size_t n_years,
	size_t n_rec_lifetime,
	std::vector<T> singleyear_vector,
	std::vector<T> scale_factor,
    double interpolation_factor,
	std::vector<T> &lifetime_from_singleyear_vector,
	size_t &n_rec_single_year,
	double &dt_hour)
{
	// Parse lifetime properties
	n_rec_single_year = n_rec_lifetime;
	if (is_lifetime) {
		n_rec_single_year = n_rec_lifetime / n_years;
	}
	else {
		n_years = 1;
	}
	dt_hour = (double)(util::hours_per_year * n_years) / n_rec_lifetime;

	lifetime_from_singleyear_vector.reserve(n_rec_lifetime);
    if (singleyear_vector.empty() ) {
        for (size_t i = 0; i < n_rec_lifetime; i++)
            lifetime_from_singleyear_vector.emplace_back(0);
        return;
    }

	auto step_per_hour = (size_t)(1 / dt_hour);
	if (step_per_hour == 0)
	    throw std::runtime_error("single_year_to_lifetime_interpolated error: Calculated step_per_hour was 0.");

	// Parse single year properties
	double dt_hour_singleyear_input = (double)(util::hours_per_year) / (double)(singleyear_vector.size());
	auto step_per_hour_singleyear_input = (size_t)(1 / dt_hour_singleyear_input);
	T step_factor = (T)step_per_hour / (T)step_per_hour_singleyear_input;

	// Possible that there is no single year vector
	if (singleyear_vector.size() > 1)
	{
		// Interpolate single year vector to dt_hour
		std::vector<T> singleyear_sampled;
		if (singleyear_vector.size() <= n_rec_single_year) {
			size_t sy_idx = 0;
			for (size_t h = 0; h < util::hours_per_year; h++) {
				for (size_t sy = 0; sy < step_per_hour_singleyear_input; sy++) {
					for (size_t i = 0; i < (size_t)step_factor; i++) {
						singleyear_sampled.push_back(singleyear_vector[sy_idx] / interpolation_factor);
					}
					sy_idx++;
				}
			}
		}
		// Downsample single year vector to dt_hour
		else {
			size_t sy_idx = 0;
			for (size_t h = 0; h < util::hours_per_year; h++) {
				for (size_t sy = 0; sy < step_per_hour; sy++) {
					// eventually add more sophisticated downsampling, ignoring information
					singleyear_sampled.push_back(singleyear_vector[(size_t)(sy_idx/step_factor)] / interpolation_factor);
					sy_idx++;
				}
			}
		}

		// Scale single year interpolated vector to lifetime
		for (size_t y = 0; y < n_years; y++) {
			for (size_t i = 0; i < n_rec_single_year; i++) {
				lifetime_from_singleyear_vector.push_back(singleyear_sampled[i] * scale_factor[y]);
			}
		}
	}
	else if (singleyear_vector.size() == 1) {
        for (size_t y = 0; y < n_years; y++) {
            for (size_t i = 0; i < n_rec_single_year; i++) {
	            lifetime_from_singleyear_vector.push_back(singleyear_vector[0] * scale_factor[y]);
            }
        }
	}
}

template void single_year_to_lifetime_interpolated<double>(bool, size_t, size_t,std::vector<double>, std::vector<double>, double, std::vector<double> &, size_t &, double &);
template void single_year_to_lifetime_interpolated<float>(bool, size_t, size_t, std::vector<float>, std::vector<float>, double, std::vector<float> &, size_t &, double &);



/**
*  \function  flatten_diurnal
*
* Function takes in a weekday and weekend schedule, plus the period values and an optional multiplier and returns
* a vector of the scaled hourly values throughout the entire year
*
* \param[in] weekday_schedule - 12x24 scheduled of periods
* \param[in] weekday_schedule - 12x24 scheduled of periods
* \param[in] steps_per_hour - Number of time steps per hour
* \param[in] period_values - the value assigned to each period number
* \param[in] multiplier - a multiplier on the period value
* \param[out] flat_vector - The 8760*steps per hour values at each hour
*/
template <class T>
std::vector<T> flatten_diurnal(util::matrix_t<size_t> weekday_schedule, util::matrix_t<size_t> weekend_schedule, size_t steps_per_hour, std::vector<T> period_values, T multiplier)
{
	std::vector<T> flat_vector;
	flat_vector.reserve(8760 * steps_per_hour);
	size_t month, hour, iprofile;
	T period_value;

	for (size_t hour_of_year = 0; hour_of_year != 8760; hour_of_year++)
	{
		util::month_hour(hour_of_year % 8760, month, hour);
		if (util::weekday(hour_of_year))
			iprofile = weekday_schedule(month - 1, hour - 1);
		else
			iprofile = weekend_schedule(month - 1, hour - 1);

		period_value = period_values[iprofile - 1];
		for (size_t s = 0; s < steps_per_hour; s++) {
			flat_vector.push_back(period_value * multiplier);
		}
	}
	return flat_vector;
}

template std::vector<double> flatten_diurnal(util::matrix_t<size_t> weekday_schedule, util::matrix_t<size_t> weekend_schedule, size_t steps_per_hour, std::vector<double> period_values, double multiplier);


/**
*  \function  extrapolate_timeseries
*
* Function takes in a timeseries vector (daily, weekly, monthly, hourly or subhourly), and the number of steps per hour desired, and an optional multiplier and returns
* n output vector of the extrapolated values throughout the entire year
*
* \param[in] steps_per_hour - Number of time steps per hour
* \param[in] input_values - the value assigned to each period number
* \param[in] multiplier - a multiplier on the period value
* \param[out] extrapolated_vector - The 8760*steps per hour values
*/
template <class T>
std::vector<T> extrapolate_timeseries(std::vector<T> input_values, size_t steps_per_hour, T multiplier)
{
	std::vector<T> extrapolated_vector;
	extrapolated_vector.reserve(8760 * steps_per_hour);
	size_t month, week, day, hour, minute_step;
	size_t input_size = input_values.size();
	int input_steps_per_hour = input_size / 8760;
	T extrapolated_value;

	for (size_t hour_of_year = 0; hour_of_year != 8760; hour_of_year++)
	{
		month = util::month_of(hour_of_year);
		if (month > 0) month--; // month_of is 1 based and all other time functions are 0 based.
		week = util::week_of(hour_of_year);
		day = util::day_of(hour_of_year);
		hour = hour_of_year;
		for (size_t s = 0; s < steps_per_hour; s++)
		{
			minute_step = (size_t)((T)s * (T)input_steps_per_hour / (T)steps_per_hour);
			if (input_size == 12) extrapolated_value = input_values[month];
			else if (input_size == 52) extrapolated_value = input_values[week];
			else if (input_size == 365) extrapolated_value = input_values[day];
			else if (input_size == 8760) extrapolated_value = input_values[hour];
			else if (input_size > 8760 && (hour * input_steps_per_hour + minute_step) < input_size)
				extrapolated_value = input_values[hour * input_steps_per_hour + minute_step];
			else // throw?
				extrapolated_value = 0.0;
			extrapolated_vector.push_back(extrapolated_value * multiplier);
		}
	}
	return extrapolated_vector;
}

template std::vector<double> extrapolate_timeseries(std::vector<double> input_values, size_t steps_per_hour, double multiplier);
