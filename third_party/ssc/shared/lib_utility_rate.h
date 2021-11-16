#ifndef _LIB_UTILITY_RATE_H_
#define _LIB_UTILITY_RATE_H_

#include "lib_util.h"
#include "lib_utility_rate_equations.h"
#include <map>

class UtilityRate
{
public:

    /*
     * Original class for determining costs for grid charging in front of the meter batteries
     * It may be possible to replace this with rate_data in UtilityRateCalculator in order to handle tiers and demand charges as appropriate for FOM
     */
	UtilityRate(){};

	UtilityRate(bool useRealTimePrices,
		util::matrix_t<size_t> ecWeekday, 
		util::matrix_t<size_t> ecWeekend, 
		util::matrix_t<double> ecRatesMatrix,
		std::vector<double> ecRealTimeBuy);

	UtilityRate(const UtilityRate& tmp);

	virtual ~UtilityRate() {/* nothing to do */ };

protected:
	/// Energy charge schedule for weekdays
	util::matrix_t<size_t> m_ecWeekday;

	/// Energy charge schedule for weekends
	util::matrix_t<size_t> m_ecWeekend;

	/// Energy charge periods, tiers, maxes, units, buy rate, (sell rate optional)
	util::matrix_t<double> m_ecRatesMatrix;

	/// Energy Tiers per period
	std::map<size_t, size_t> m_energyTiersPerPeriod;

	/// Real time energy prices
	std::vector<double> m_ecRealTimeBuy;

	/// Use real time prices or not
	bool m_useRealTimePrices;
};

class UtilityRateCalculator : protected UtilityRate
{
public:
	/// Constructor for rate calculator where load will be input on the fly
	UtilityRateCalculator(UtilityRate * Rate, size_t stepsPerHour);

	/// Constructor for rate calculator where full load is known
	UtilityRateCalculator(UtilityRate * Rate, size_t stepsPerHour, std::vector<double> loadProfile);

	/// Copy Ctor
	UtilityRateCalculator(const UtilityRateCalculator& tmp);

	/// Parse the incoming data
	void initializeRate();

	/// Update the bill to include the load at the current timestep
	void updateLoad(double loadPower);

	/// Calculate the utility bill for the full load
	void calculateEnergyUsagePerPeriod();

	/// Get the energy rate at the given hour of year
	double getEnergyRate(size_t );

	/// Get the period for a given hour of year
	size_t getEnergyPeriod(size_t hourOfYear);

	virtual ~UtilityRateCalculator() {/* nothing to do*/ };

protected:

	/// The load profile to evaluate (kW)
	std::vector<double> m_loadProfile;
	
	/// The calculated electricity bill for the UtilityRate and load profile ($)
	double m_electricBill;

	/// The number of time steps per hour
	size_t m_stepsPerHour;

	/// The energy usage per period
	std::vector<double> m_energyUsagePerPeriod;
};


class UtilityRateForecast
{
public:

    /*
     * Full forecast function using utility rate data. Computes the impact of tiers, time of use, and time series buy and sell rates
     * The rate_data object provided will be modified by this class as appropriate to the net grid usage, especially the ur_month objects
     * *_forecast vectors need to be 12 * analysis_period in length. Predictions are used to estimate which tiers will be used for energy charges.
     * and set up baseline demand costs - equal to the demand charge at the average load during the month.
     */
	UtilityRateForecast(rate_data* util_rate, size_t stepsPerHour, const std::vector<double>& monthly_load_forecast, const std::vector<double>& monthly_gen_forecast, const std::vector<double>& monthly_peak_forecast, size_t analysis_period);

	UtilityRateForecast(UtilityRateForecast& tmp);

	~UtilityRateForecast();

	/*
    * Returns the increase in cost for the utility bill over the forecast period (length of predicted loads * steps per hour)
    * Loads provided to this function are included in the forecast bill going forward, so if you need to run the same period multiple times, make copies of this class
    * For net metering, if there are surplus credits prior to the start of the the forecast period, cost will be zero
    * End of year net metering credits are counted as zero, since this improves dispatch overall
    * For demand charges, in order to avoid those charges occuring at 12 am on the first of each month, ur_month.dc_flat_peak and tou_peak are
    * set to the average load for that month in initialize_month. This means the peak charges will be recorded the first time the average load goes over the month.
    * Usage notes: initialize first month prior to calling this function
    */
	double forecastCost(std::vector<double>& predicted_loads, size_t year, size_t hour_of_year, size_t step);

    /*
     * Runs when the new month appears in the forecast for the first time. Year accounts for inflation and other pricing escalations
     * Modifies ur_month and buy/sell rates for the months specified
     * If the mismatch between int month and size_t year looks wierd to you, you're going to have to change lib_util to fix it. Good luck.
     */
    void initializeMonth(int month, size_t year);
    // Runs when the starting hour of the forecast is in the new month. Copies next rates onto current rates
	void copyTOUForecast();
	
	/*
     * Combine the monthly forecasts with the time of use tiers to anticipate the value of shifting load from peak to off-peak
     * The forecast is needed so that the anticipated value of load shifting remains the same throughout the month
     * See https://www.pge.com/includes/docs/pdfs/shared/environment/pge/cleanair/electricdrivevehicles/PEV_rate_options.pdf or cmod_utility_rate_5 for more details on the calculation in a real bill
     * Public for testing
     */
	void compute_next_composite_tou(int month, size_t year);

    // Composite buy/sell rates given the usage in the forecasts provided to the constructor.
	std::vector<double> current_composite_sell_rates; // Sell rates at the start of the forecast
	std::vector<double> current_composite_buy_rates;
	std::vector<double> next_composite_sell_rates; // Sell rates if the forecast crosses into the next month
	std::vector<double> next_composite_buy_rates;
protected:

    /* Transfer net metering surplus credits from previous month to current month */
	void restartMonth(int prevMonth, int currentMonth, size_t year);

    double getEnergyChargeNetMetering(int month, std::vector<double>& buy_rates, std::vector<double>& sell_rates);
    double getEnergyChargeNetBillingOrTimeSeries(double energy, size_t year_one_index, int current_month, size_t year, bool use_next_month);

	std::shared_ptr<rate_data> rate;

	size_t steps_per_hour;
	float dt_hour;

	size_t last_step;
    int last_month_init;
    size_t nyears;

    // Load: total net energy from the grid over the month
	std::vector<double> m_monthly_load_forecast; // Length is 12 * analysis period
    // gen: total net energy to the grid (generation) over the month
	std::vector<double> m_monthly_gen_forecast; // Length is 12 * analysis period
    // Avg load: the average gross load (no gen) over the month. Used to establish a floor for peak shaving costs
	std::vector<double> m_monthly_avg_load_forecast; // Length is 12 * analysis period


};

#endif // !_LIB_UTILITY_RATE_H_
