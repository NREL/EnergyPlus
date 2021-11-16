#include "lib_utility_rate_equations.h"

#include <sstream>

ur_month::ur_month() :
	ec_periods(),
	dc_periods(),
	ec_rollover_periods(),
	energy_net(),
	hours_per_month(),
	ec_energy_use(),
	ec_periods_tiers(),
	ec_energy_surplus(),
	dc_tou_peak(),
	dc_tou_peak_hour(),
	dc_flat_peak(),
	dc_flat_peak_hour(),
	ec_tou_ub_init(),
	ec_tou_br_init(),
	ec_tou_sr_init(),
	ec_tou_ub(),
	ec_tou_br(),
	ec_tou_sr(),
	ec_tou_units(),
	ec_charge(),
	dc_tou_ub(),
	dc_tou_ch(),
	dc_flat_ub(),
	dc_flat_ch(),
	dc_tou_charge(),
	dc_flat_charge()
{}

ur_month::ur_month(const ur_month& tmp) :
	ec_periods(tmp.ec_periods),
	dc_periods(tmp.dc_periods),
	ec_rollover_periods(tmp.ec_rollover_periods),
	energy_net(tmp.energy_net),
	hours_per_month(tmp.hours_per_month),
	ec_energy_use(tmp.ec_energy_use),
	ec_periods_tiers(tmp.ec_periods_tiers),
	ec_energy_surplus(tmp.ec_energy_surplus),
	dc_tou_peak(tmp.dc_tou_peak),
	dc_tou_peak_hour(tmp.dc_tou_peak_hour),
	dc_flat_peak(tmp.dc_flat_peak),
	dc_flat_peak_hour(tmp.dc_flat_peak_hour),
	ec_tou_ub_init(tmp.ec_tou_ub_init),
	ec_tou_br_init(tmp.ec_tou_br_init),
	ec_tou_sr_init(tmp.ec_tou_sr_init),
	ec_tou_ub(tmp.ec_tou_ub),
	ec_tou_br(tmp.ec_tou_br),
	ec_tou_sr(tmp.ec_tou_sr),
	ec_tou_units(tmp.ec_tou_units),
	ec_charge(tmp.ec_charge),
	dc_tou_ub(tmp.dc_tou_ub),
	dc_tou_ch(tmp.dc_tou_ch),
	dc_flat_ub(tmp.dc_flat_ub),
	dc_flat_ch(tmp.dc_flat_ch),
	dc_tou_charge(tmp.dc_tou_charge),
	dc_flat_charge(tmp.dc_flat_charge)
{}

void ur_month::update_net_and_peak(double energy, double power, size_t step) {
	// net energy use per month
	energy_net += energy; // -load and +gen
	// hours per period per month
	hours_per_month++;
	// peak
	if (power < 0 && power < -dc_flat_peak)
	{
		dc_flat_peak = -power;
		dc_flat_peak_hour = step;
	}
}

void ur_month::reset()
{
    energy_net = 0;
    hours_per_month = 0;
    dc_flat_peak = 0;
    dc_flat_peak_hour = 0;

    size_t start_tier = 0;
    size_t end_tier = ec_tou_ub.ncols() - 1;
    size_t num_periods = ec_tou_ub.nrows();
    size_t num_tiers = end_tier - start_tier + 1;

    ec_energy_surplus.resize_fill(num_periods, num_tiers, 0);
    ec_energy_use.resize_fill(num_periods, num_tiers, 0);
    ec_charge.resize_fill(num_periods, num_tiers, 0);
}

rate_data::rate_data() :
	m_ec_tou_sched(),
	m_dc_tou_sched(),
	m_month(),
	m_ec_periods(),
	m_ec_ts_sell_rate(),
	m_ec_ts_buy_rate(),
	m_ec_periods_tiers_init(),
	m_dc_tou_periods(),
	m_dc_tou_periods_tiers(),
	m_dc_flat_tiers(),
	m_num_rec_yearly(),
	dc_hourly_peak(),
	monthly_dc_fixed(12),
	monthly_dc_tou(12),
	tou_demand_single_peak(false),
    enable_nm(false),
    nm_credits_w_rollover(false),
    net_metering_credit_month(11),
    nm_credit_sell_rate(0.0),
    rate_scale(),
    en_ts_buy_rate(false),
    en_ts_sell_rate(false)
{}

rate_data::rate_data(const rate_data& tmp) :
	m_ec_tou_sched(tmp.m_ec_tou_sched),
	m_dc_tou_sched(tmp.m_dc_tou_sched),
	m_month(tmp.m_month),
	m_ec_periods(tmp.m_ec_periods),
	m_ec_ts_sell_rate(tmp.m_ec_ts_sell_rate),
	m_ec_ts_buy_rate(tmp.m_ec_ts_buy_rate),
	m_ec_periods_tiers_init(tmp.m_ec_periods_tiers_init),
	m_dc_tou_periods(tmp.m_dc_tou_periods),
	m_dc_tou_periods_tiers(tmp.m_dc_tou_periods_tiers),
	m_dc_flat_tiers(tmp.m_dc_flat_tiers),
	m_num_rec_yearly(tmp.m_num_rec_yearly),
	dc_hourly_peak(tmp.dc_hourly_peak),
	monthly_dc_fixed(tmp.monthly_dc_fixed),
	monthly_dc_tou(tmp.monthly_dc_tou),
	tou_demand_single_peak(tmp.tou_demand_single_peak),
    enable_nm(tmp.enable_nm),
    nm_credits_w_rollover(tmp.nm_credits_w_rollover),
    net_metering_credit_month(tmp.net_metering_credit_month),
    nm_credit_sell_rate(tmp.nm_credit_sell_rate),
    rate_scale(tmp.rate_scale),
    en_ts_buy_rate(tmp.en_ts_buy_rate),
    en_ts_sell_rate(tmp.en_ts_sell_rate)
{}

void rate_data::init(int num_rec_yearly) {
	size_t i, m;

    m_num_rec_yearly = num_rec_yearly;

	for (i = 0; i < m_ec_periods_tiers_init.size(); i++)
		m_ec_periods_tiers_init[i].clear();
	m_ec_periods.clear();

	for (i = 0; i < m_dc_tou_periods_tiers.size(); i++)
		m_dc_tou_periods_tiers[i].clear();
	m_dc_tou_periods.clear();

	for (i = 0; i < m_dc_flat_tiers.size(); i++)
		m_dc_flat_tiers[i].clear();

	m_month.clear();
	for (m = 0; m < 12; m++)
	{
		ur_month urm;
		m_month.push_back(urm);
	}

	m_ec_ts_sell_rate.clear();
	m_ec_ts_buy_rate.clear();
    dc_hourly_peak.clear();

	// for reporting purposes
    m_ec_tou_sched = std::vector<int>(m_num_rec_yearly, 1);
    m_dc_tou_sched = std::vector<int>(m_num_rec_yearly, 1);
    dc_hourly_peak = std::vector<ssc_number_t>(m_num_rec_yearly, 0);
}

bool rate_data::check_for_kwh_per_kw_rate(int units) {
    return (units == 1) || (units == 3);
}

void rate_data::init_energy_rates(bool gen_only) {
	// calculate the monthly net energy per tier and period based on units
	for (int m = 0; m < (int)m_month.size(); m++)
	{
		// check for kWh/kW
        size_t num_periods = (int)m_month[m].ec_tou_ub.nrows();
        size_t num_tiers = (int)m_month[m].ec_tou_ub.ncols(); // Likely to be overwritten later

		if (!gen_only) // added for two meter no load scenarios to use load tier sizing
		{

			// kWh/kW (kWh/kW daily handled in Setup)
			// 1. find kWh/kW tier
			// 2. fill in kWh tiers as needed, up to kWh/kW * flat peak
            // 3. repeat until maximum tier is reached
			// 4. assumption is that all periods in same month have same tier breakdown
			// 5. assumption is that tier numbering is correct for the kWh/kW breakdown
			// That is, first tier must be kWh/kW
            // See example at: https://github.com/NREL/SAM-documentation/blob/master/Unit%20Testing/Utility%20Rates/block_step/GPC_PLL_Tiered_Bill_Calc_Example_v3_btm_tests.xlsx
			if ((m_month[m].ec_tou_units.ncols() > 0 && m_month[m].ec_tou_units.nrows() > 0)
				&& check_for_kwh_per_kw_rate(m_month[m].ec_tou_units.at(0, 0)))
			{
                std::vector<double> kWh_per_kW_tiers; // Fill this first so we can see where the kWh tiers break
                std::vector<size_t> tier_numbers;
                std::vector<double> tier_kwh;

				// track monthly peak to determine which kWh/kW tier
                double flat_peak = m_month[m].dc_flat_peak;

                // get kWh/kW break points based on actual demand
                for (size_t i_tier = 0; i_tier < m_month[m].ec_tou_units.ncols(); i_tier++)
                {
                    int units = (int)m_month[m].ec_tou_units.at(0, i_tier);
                    if (check_for_kwh_per_kw_rate(units))
                    {
                        double kwh_per_kw = m_month[m].ec_tou_ub_init.at(0, i_tier);
                        if (kwh_per_kw > 1e+37) {
                            // Max double stays max double
                            kWh_per_kW_tiers.push_back(kwh_per_kw);
                        }
                        else {
                            kWh_per_kW_tiers.push_back(kwh_per_kw * flat_peak);
                        }
                    }
                }

                size_t block = 0; // Start with first kWh/kW tier
                size_t total_tiers = m_month[m].ec_tou_units.ncols();
				for (size_t i_tier = 0; i_tier < total_tiers; i_tier++)
				{
					int units = (int)m_month[m].ec_tou_units.at(0, i_tier);
					if (check_for_kwh_per_kw_rate(units))
					{
                        // Update to current block
                        double kwh_per_kw = m_month[m].ec_tou_ub_init.at(0, i_tier);
                        if (kWh_per_kW_tiers[block] < kwh_per_kw * flat_peak && (block + 1 < kWh_per_kW_tiers.size())) {
                            block++; // Move on to next block tier
                        }
						// Check this block, if it has kWh steps skip it and move on, otherwise add it
                        if (i_tier + 1 < total_tiers) {
                            int next_units = (int)m_month[m].ec_tou_units.at(0, i_tier+1);
                            
                            if (check_for_kwh_per_kw_rate(next_units)) {
                                tier_numbers.push_back(i_tier);
                                tier_kwh.push_back(kWh_per_kW_tiers[block]);
                            }
                            // Else do nothing, loop again and add the step
                        }
                        else {
                            // Last tier - add it
                            tier_numbers.push_back(i_tier);
                            tier_kwh.push_back(kWh_per_kW_tiers[block]);
                        }
					}
                    else {
                        // Add steps up to block max
                        double max = m_month[m].ec_tou_ub_init.at(0, i_tier);
                        if (max < kWh_per_kW_tiers[block]) {
                            tier_kwh.push_back(max);
                            tier_numbers.push_back(i_tier);
                        }
                        else {
                            if (tier_kwh.empty() || (tier_kwh[tier_kwh.size() - 1] < kWh_per_kW_tiers[block])) {
                                tier_kwh.push_back(kWh_per_kW_tiers[block]);
                                tier_numbers.push_back(i_tier);
                            }
                        }
                        
                    }
				}

                num_tiers = tier_kwh.size();
				// resize everytime to handle load and energy changes
				// resize sr, br and ub for use in energy charge calculations below
				util::matrix_t<double> br(num_periods, num_tiers);
				util::matrix_t<double> sr(num_periods, num_tiers);
				util::matrix_t<double> ub(num_periods, num_tiers);
				// assign appropriate values.
				for (size_t period = 0; period < num_periods; period++)
				{
					for (size_t tier = 0; tier < num_tiers; tier++)
					{
						br.at(period, tier) = m_month[m].ec_tou_br_init.at(period, tier_numbers[tier]);
						sr.at(period, tier) = m_month[m].ec_tou_sr_init.at(period, tier_numbers[tier]);
						ub.at(period, tier) = tier_kwh[tier];
						// update for correct tier number column headings
						m_month[m].ec_periods_tiers[period][tier] = (int) (m_ec_periods_tiers_init[period][tier_numbers[tier]]);
					}
				}

				m_month[m].ec_tou_br = br;
				m_month[m].ec_tou_sr = sr;
				m_month[m].ec_tou_ub = ub;
			}
		}

		m_month[m].ec_energy_surplus.resize_fill(num_periods, num_tiers, 0);
		m_month[m].ec_energy_use.resize_fill(num_periods, num_tiers, 0);
		m_month[m].ec_charge.resize_fill(num_periods, num_tiers, 0);

	}

}

void rate_data::setup_time_series(size_t cnt, ssc_number_t* ts_sr, ssc_number_t* ts_br)
{
	size_t i;

	size_t ts_step_per_hour = cnt / 8760;
	// assign timestep values for utility rate calculations
	size_t idx = 0;
	ssc_number_t sr, br;
	sr = br = 0;
	size_t step_per_hour = m_num_rec_yearly / 8760;
	//time step rates - fill out to number of generation records per year
	// handle cases
	// 1. if no time step rate  s
	// 2. if time step rate  has 8760 and gen has more records
	// 3. if number records same for time step rate  and gen
	idx = 0;

    if (ts_br != NULL)
    {
        for (i = 0; i < 8760; i++)
        {
            for (size_t ii = 0; ii < step_per_hour; ii++)
            {
                br = (idx < cnt) ? ts_br[idx] : 0;
                m_ec_ts_buy_rate.push_back(br);
                if (ii < ts_step_per_hour) idx++;
            }
        }
    }

    idx = 0;
    if (ts_sr != NULL)
    {
        for (i = 0; i < 8760; i++)
        {
            for (size_t ii = 0; ii < step_per_hour; ii++)
            {
                sr = (idx < cnt) ? ts_sr[idx] : 0;
                m_ec_ts_sell_rate.push_back(sr);
                if (ii < ts_step_per_hour) idx++;
            }
        }
    }
		
}

void rate_data::setup_energy_rates(ssc_number_t* ec_weekday, ssc_number_t* ec_weekend,
	size_t ec_tou_rows, ssc_number_t* ec_tou_in, bool sell_eq_buy)
{
	size_t nrows, ncols, r, c, m, i, j;
	int period, tier;
	size_t steps_per_hour = m_num_rec_yearly / 8760;
	size_t idx = 0;

	// This is error checked in cmod_utilityrate5 (and other calling functions)
	nrows = 12;
	ncols = 24;

	util::matrix_t<double> ec_schedwkday(nrows, ncols);
	ec_schedwkday.assign(ec_weekday, nrows, ncols);
	util::matrix_t<double> ec_schedwkend(nrows, ncols);
	ec_schedwkend.assign(ec_weekend, nrows, ncols);

	// for each row (month) determine periods in the month
	// m_monthly_ec_tou_ub max of period tier matrix of period xtier +1
	// columns are period, tier1 max, tier 2 max, ..., tier n max


	int ec_tod[8760];

	if (!util::translate_schedule(ec_tod, ec_schedwkday, ec_schedwkend, 1, 12))
		throw general_error("Could not translate weekday and weekend schedules for energy rates.");
	for (i = 0; i < 8760; i++)
	{
		for (size_t ii = 0; ii < steps_per_hour; ii++)
		{
			if (idx < m_num_rec_yearly)
				m_ec_tou_sched[idx] = ec_tod[i];
			idx++;
		}
	}

	// 6 columns period, tier, max usage, max usage units, buy, sell
	ncols = 6;
	util::matrix_t<double> ec_tou_mat(ec_tou_rows, ncols);
	ec_tou_mat.assign(ec_tou_in, ec_tou_rows, ncols);

	for (r = 0; r < ec_tou_rows; r++)
	{
		period = (int)ec_tou_mat.at(r, 0);
		if (std::find(m_ec_periods.begin(), m_ec_periods.end(), period) == m_ec_periods.end())
			m_ec_periods.push_back(period);
	}
	// sorted periods smallest to largest
	std::sort(m_ec_periods.begin(), m_ec_periods.end());
	// for each period, get list of tier numbers and then sort and construct
	//m_ec_tou_ub, m_ec_tou_units, m_ec_tou_br, ec_tou_sr vectors of vectors

    m_ec_periods_tiers_init = std::vector<std::vector<int>>(m_ec_periods.size());

	for (r = 0; r < ec_tou_rows; r++)
	{
		period = (int)ec_tou_mat.at(r, 0);
		tier = (int)ec_tou_mat.at(r, 1);
		std::vector<int>::iterator result = std::find(m_ec_periods.begin(), m_ec_periods.end(), period);
		if (result == m_ec_periods.end())
		{
			std::ostringstream ss;
			ss << "Energy rate Period " << period << " not found.";
			throw exec_error("lib_utility_rate_equations", ss.str());
		}
		int ndx = (int)(result - m_ec_periods.begin());
		m_ec_periods_tiers_init[ndx].push_back(tier);
	}
	// sort tier values for each period
	for (r = 0; r < m_ec_periods_tiers_init.size(); r++)
		std::sort(m_ec_periods_tiers_init[r].begin(), m_ec_periods_tiers_init[r].end());

	// find all periods for each month m through schedules
	for (m = 0; m < m_month.size(); m++)
	{
		// energy charges
		for (c = 0; c < ec_schedwkday.ncols(); c++)
		{
			if (std::find(m_month[m].ec_periods.begin(), m_month[m].ec_periods.end(), ec_schedwkday.at(m, c)) == m_month[m].ec_periods.end())
				m_month[m].ec_periods.push_back((int)ec_schedwkday.at(m, c));

			// rollover periods considered from weekday schedule at 12a [0], 6a [5], 12p [11], and 6p [17]
			if ((m_month[m].ec_rollover_periods.size() < 5) && (c == 0 || c == 5 || c == 11 || c == 17))
			{
				m_month[m].ec_rollover_periods.push_back((int)ec_schedwkday.at(m, c));
			}
		}
		for (c = 0; c < ec_schedwkend.ncols(); c++)
		{
			if (std::find(m_month[m].ec_periods.begin(), m_month[m].ec_periods.end(), ec_schedwkend.at(m, c)) == m_month[m].ec_periods.end())
				m_month[m].ec_periods.push_back((int)ec_schedwkend.at(m, c));
		}
		std::sort(m_month[m].ec_periods.begin(), m_month[m].ec_periods.end());

		// set m_ec_periods_tiers
		for (size_t pt_period = 0; pt_period < m_ec_periods_tiers_init.size(); pt_period++)
			m_month[m].ec_periods_tiers.push_back(std::vector<int>());
		for (size_t pt_period = 0; pt_period < m_ec_periods_tiers_init.size(); pt_period++)
		{
			for (size_t pt_tier = 0; pt_tier < m_ec_periods_tiers_init[pt_period].size(); pt_tier++)
				m_month[m].ec_periods_tiers[pt_period].push_back(m_ec_periods_tiers_init[pt_period][pt_tier]);
		}


	}

	// periods are rows and tiers are columns - note that columns can change based on rows
	// Initialize each month variables that are constant over the simulation
	for (m = 0; m < m_month.size(); m++)
	{
		int num_periods = 0;
		int num_tiers = 0;

		for (i = 0; i < m_month[m].ec_periods.size(); i++)
		{
			// find all periods and check that number of tiers the same for all for the month, if not through error
			std::vector<int>::iterator per_num = std::find(m_ec_periods.begin(), m_ec_periods.end(), m_month[m].ec_periods[i]);
			if (per_num == m_ec_periods.end())
			{
				std::ostringstream ss;
				ss << "Period " << m_month[m].ec_periods[i] << " is in Month " << m << " but is not defined in the energy rate table. Rates for each period in the Weekday and Weekend schedules must be defined in the energy rate table.";
				throw exec_error("lib_utility_rate_equations", ss.str());
			}
			period = (*per_num);
			int ndx = (int)(per_num - m_ec_periods.begin());
			num_tiers = (int)m_ec_periods_tiers_init[ndx].size();
			if (i == 0)
			{
				// redimension ec_ field of ur_month class
				num_periods = (int)m_month[m].ec_periods.size();
				m_month[m].ec_tou_ub.resize_fill(num_periods, num_tiers, (ssc_number_t)1e+38);
				m_month[m].ec_tou_units.resize_fill(num_periods, num_tiers, 0); // kWh
				m_month[m].ec_tou_br.resize_fill(num_periods, num_tiers, 0);
				m_month[m].ec_tou_sr.resize_fill(num_periods, num_tiers, 0);
			}
			else
			{
				if ((int)m_ec_periods_tiers_init[ndx].size() != num_tiers)
				{
					std::ostringstream ss;
					ss << "The number of tiers in the energy rate table, " << m_ec_periods_tiers_init[ndx].size() << ", is incorrect for Month " << m << " and Period " << m_month[m].ec_periods[i] << ". The correct number of tiers for that month and period is " << num_tiers << ".";
					throw exec_error("lib_utility_rate_equations", ss.str());
				}
			}
			for (j = 0; j < m_ec_periods_tiers_init[ndx].size(); j++)
			{
				tier = m_ec_periods_tiers_init[ndx][j];
				// initialize for each period and tier
				bool found = false;
				for (r = 0; (r < ec_tou_rows) && !found; r++)
				{
					if ((period == (int)ec_tou_mat.at(r, 0))
						&& (tier == (int)ec_tou_mat.at(r, 1)))
					{
						m_month[m].ec_tou_ub.at(i, j) = ec_tou_mat.at(r, 2);
						// units kWh, kWh/kW, kWh daily, kWh/kW daily
						m_month[m].ec_tou_units.at(i, j) = (int)ec_tou_mat.at(r, 3);
						if ((m_month[m].ec_tou_units.at(i, j) == 2)
							|| (m_month[m].ec_tou_units.at(i, j) == 3))
						{// kWh daily or kWh/kW daily - adjust max usage by number of days in month (or billing cycle per Eric 12/14/15
							m_month[m].ec_tou_ub.at(i, j) *= util::nday[m];
						}
						m_month[m].ec_tou_br.at(i, j) = ec_tou_mat.at(r, 4);
						// adjust sell rate based on input selections
						ssc_number_t sell = ec_tou_mat.at(r, 5);
						if (sell_eq_buy)
							sell = ec_tou_mat.at(r, 4);
						m_month[m].ec_tou_sr.at(i, j) = sell;
						found = true;
					}
				}

			}
			// copy all sr, br, ub for ec in case units force change
			// copy not same memory location
			m_month[m].ec_tou_ub_init = m_month[m].ec_tou_ub;
			m_month[m].ec_tou_br_init = m_month[m].ec_tou_br;
			m_month[m].ec_tou_sr_init = m_month[m].ec_tou_sr;
		}
	}
}

void rate_data::setup_demand_charges(ssc_number_t* dc_weekday, ssc_number_t* dc_weekend, 
	size_t dc_tou_rows, ssc_number_t* dc_tou_in, size_t dc_flat_rows, ssc_number_t* dc_flat_in) {
	size_t nrows, ncols, r, c, m, i, j, idx;
	int period, tier, month;

	// This is error checked in cmod_utilitrate5 (and other calling functions)
	nrows = 12;
	ncols = 24;
	util::matrix_t<double> dc_schedwkday(nrows, ncols);
	dc_schedwkday.assign(dc_weekday, nrows, ncols);
	util::matrix_t<double> dc_schedwkend(nrows, ncols);
	dc_schedwkend.assign(dc_weekend, nrows, ncols);

	size_t steps_per_hour = m_num_rec_yearly / 8760;

	// for each row (month) determine periods in the month
	// m_monthly_dc_tou_ub max of period tier matrix of period xtier +1
	// columns are period, tier1 max, tier 2 max, ..., tier n max


	int dc_tod[8760];

	if (!util::translate_schedule(dc_tod, dc_schedwkday, dc_schedwkend, 1, 12))
		throw general_error("Could not translate weekday and weekend schedules for demand charges");

	idx = 0;
	for (i = 0; i < 8760; i++)
	{
		for (size_t ii = 0; ii < steps_per_hour; ii++)
		{
			if (idx < m_num_rec_yearly)
				m_dc_tou_sched[idx] = dc_tod[i];
			idx++;
		}
	}

	// 4 columns period, tier, max usage, charge
	ncols = 4;
	util::matrix_t<double> dc_tou_mat(dc_tou_rows, ncols);
	dc_tou_mat.assign(dc_tou_in, dc_tou_rows, ncols);

	// find all periods for each month m through schedules
	for (m = 0; m < m_month.size(); m++)
	{
		// demand charges
		for (c = 0; c < dc_schedwkday.ncols(); c++)
		{
			if (std::find(m_month[m].dc_periods.begin(), m_month[m].dc_periods.end(), dc_schedwkday.at(m, c)) == m_month[m].dc_periods.end())
				m_month[m].dc_periods.push_back((int)dc_schedwkday.at(m, c));
		}
		for (c = 0; c < dc_schedwkend.ncols(); c++)
		{
			if (std::find(m_month[m].dc_periods.begin(), m_month[m].dc_periods.end(), dc_schedwkend.at(m, c)) == m_month[m].dc_periods.end())
				m_month[m].dc_periods.push_back((int)dc_schedwkend.at(m, c));
		}
		std::sort(m_month[m].dc_periods.begin(), m_month[m].dc_periods.end());
	}

	for (r = 0; r < dc_tou_rows; r++)
	{
		period = (int)dc_tou_mat.at(r, 0);
		if (std::find(m_dc_tou_periods.begin(), m_dc_tou_periods.end(), period) == m_dc_tou_periods.end())
			m_dc_tou_periods.push_back(period);
	}
	// sorted periods smallest to largest
	std::sort(m_dc_tou_periods.begin(), m_dc_tou_periods.end());
	// for each period, get list of tier numbers and then sort and construct
	//m_dc_tou_ub, m_dc_tou_units, m_dc_tou_br, dc_tou_sr vectors of vectors
	m_dc_tou_periods_tiers = std::vector<std::vector<int>>(m_dc_tou_periods.size());

	for (r = 0; r < dc_tou_rows; r++)
	{
		period = (int)dc_tou_mat.at(r, 0);
		tier = (int)dc_tou_mat.at(r, 1);
		std::vector<int>::iterator result = std::find(m_dc_tou_periods.begin(), m_dc_tou_periods.end(), period);
		if (result == m_dc_tou_periods.end())
		{
			std::ostringstream ss;
			ss << "Demand charge Period " << period << " not found.";
			throw exec_error("utilityrate5", ss.str());
		}
		int ndx = (int)(result - m_dc_tou_periods.begin());
		m_dc_tou_periods_tiers[ndx].push_back(tier);
	}
	// sort tier values for each period
	for (r = 0; r < m_dc_tou_periods_tiers.size(); r++)
		std::sort(m_dc_tou_periods_tiers[r].begin(), m_dc_tou_periods_tiers[r].end());


	// periods are rows and tiers are columns - note that columns can change based on rows
	// Initialize each month variables that are constant over the simulation

	for (m = 0; m < m_month.size(); m++)
	{
		int num_periods = 0;
		int num_tiers = 0;
		num_periods = (int)m_month[m].dc_periods.size();
		for (i = 0; i < m_month[m].dc_periods.size(); i++)
		{
			// find all periods and check that number of tiers the same for all for the month, if not through error
			std::vector<int>::iterator per_num = std::find(m_dc_tou_periods.begin(), m_dc_tou_periods.end(), m_month[m].dc_periods[i]);
			if (per_num == m_dc_tou_periods.end())
			{
				std::ostringstream ss;
				ss << "Period " << m_month[m].dc_periods[i] << " is in Month " << m << " but is not defined in the demand rate table.  Rates for each period in the Weekday and Weekend schedules must be defined in the demand rate table.";
				throw exec_error("utilityrate5", ss.str());
			}
			period = (*per_num);
			int ndx = (int)(per_num - m_dc_tou_periods.begin());
			// redimension dc_ field of ur_month class
			if ((int)m_dc_tou_periods_tiers[ndx].size() > num_tiers)
				num_tiers = (int)m_dc_tou_periods_tiers[ndx].size();
		}
		m_month[m].dc_tou_ub.resize_fill(num_periods, num_tiers, (ssc_number_t)1e38);
		m_month[m].dc_tou_ch.resize_fill(num_periods, num_tiers, 0); // kWh
		for (i = 0; i < m_month[m].dc_periods.size(); i++)
		{
			// find all periods and check that number of tiers the same for all for the month, if not through error
			std::vector<int>::iterator per_num = std::find(m_dc_tou_periods.begin(), m_dc_tou_periods.end(), m_month[m].dc_periods[i]);
			period = (*per_num);
			int ndx = (int)(per_num - m_dc_tou_periods.begin());
			for (j = 0; j < m_dc_tou_periods_tiers[ndx].size(); j++)
			{
				tier = m_dc_tou_periods_tiers[ndx][j];
				// initialize for each period and tier
				bool found = false;
				for (r = 0; (r < dc_tou_rows) && !found; r++)
				{
					if ((period == (int)dc_tou_mat.at(r, 0))
						&& (tier == (int)dc_tou_mat.at(r, 1)))
					{
						m_month[m].dc_tou_ub.at(i, j) = dc_tou_mat.at(r, 2);
						m_month[m].dc_tou_ch.at(i, j) = dc_tou_mat.at(r, 3);//demand charge;
						found = true;
					}
				}
			}
		}
	}
	// flat demand charge
	// 4 columns month, tier, max usage, charge
	ncols = 4;
	util::matrix_t<double> dc_flat_mat(dc_flat_rows, ncols);
	dc_flat_mat.assign(dc_flat_in, dc_flat_rows, ncols);

	for (r = 0; r < m_month.size(); r++)
	{
		m_dc_flat_tiers.push_back(std::vector<int>());
	}

	for (r = 0; r < dc_flat_rows; r++)
	{
		month = (int)dc_flat_mat.at(r, 0);
		tier = (int)dc_flat_mat.at(r, 1);
		if ((month < 0) || (month >= (int)m_month.size()))
		{
			std::ostringstream ss;
			ss << "Demand for Month " << month << " not found.";
			throw exec_error("utilityrate5", ss.str());
		}
		m_dc_flat_tiers[month].push_back(tier);
	}
	// sort tier values for each period
	for (r = 0; r < m_dc_flat_tiers.size(); r++)
		std::sort(m_dc_flat_tiers[r].begin(), m_dc_flat_tiers[r].end());


	// months are rows and tiers are columns - note that columns can change based on rows
	// Initialize each month variables that are constant over the simulation



	for (m = 0; m < m_month.size(); m++)
	{
		m_month[m].dc_flat_ub.clear();
		m_month[m].dc_flat_ch.clear();
		for (j = 0; j < m_dc_flat_tiers[m].size(); j++)
		{
			tier = m_dc_flat_tiers[m][j];
			// initialize for each period and tier
			bool found = false;
			for (r = 0; (r < dc_flat_rows) && !found; r++)
			{
				if ((m == dc_flat_mat.at(r, 0))
					&& (tier == (int)dc_flat_mat.at(r, 1)))
				{
					m_month[m].dc_flat_ub.push_back(dc_flat_mat.at(r, 2));
					m_month[m].dc_flat_ch.push_back(dc_flat_mat.at(r, 3));//rate_esc;
					found = true;
				}
			}

		}
	}
}

void rate_data::sort_energy_to_periods(int month, double energy, size_t step) {
	// accumulate energy per period - place all in tier 0 initially and then
	// break up according to tier boundaries and number of periods
	ur_month& curr_month = m_month[month];
	int toup = m_ec_tou_sched[step];
	std::vector<int>::iterator per_num = std::find(curr_month.ec_periods.begin(), curr_month.ec_periods.end(), toup);
	if (per_num == curr_month.ec_periods.end())
	{
		std::ostringstream ss;
		ss << "Energy rate TOU Period " << toup << " not found for Month " << util::schedule_int_to_month(month) << ".";
		throw exec_error("utilityrate5", ss.str());
	}
	int row = (int)(per_num - curr_month.ec_periods.begin());
	// place all in tier 0 initially and then update appropriately
	// net energy per period per month
	curr_month.ec_energy_use(row, 0) += energy;
}

void rate_data::init_dc_peak_vectors(int month)
{
	ur_month& curr_month = m_month[month];
	curr_month.dc_tou_peak.clear();
	curr_month.dc_tou_peak_hour.clear();
	
	curr_month.dc_tou_peak = std::vector<ssc_number_t>(curr_month.dc_periods.size());
	curr_month.dc_tou_peak_hour = std::vector<size_t>(curr_month.dc_periods.size());
	
}

void rate_data::find_dc_tou_peak(int month, double power, size_t step) {
    ur_month& curr_month = m_month[month];
    if (curr_month.dc_periods.size() > 0)
    {
        int todp = m_dc_tou_sched[step];
        std::vector<int>::iterator per_num = std::find(curr_month.dc_periods.begin(), curr_month.dc_periods.end(), todp);
        if (per_num == curr_month.dc_periods.end())
        {
            std::ostringstream ss;
            ss << "Demand charge Period " << todp << " not found for Month " << month << ".";
            throw exec_error("lib_utility_rate_equations", ss.str());
        }
        int row = (int)(per_num - curr_month.dc_periods.begin());
        if (power < 0 && power < -curr_month.dc_tou_peak[row])
        {
            curr_month.dc_tou_peak[row] = -power;
            curr_month.dc_tou_peak_hour[row] = step;
        }
    }
}

ssc_number_t rate_data::get_demand_charge(int month, size_t year)
{
	size_t tier, period;
	ur_month& curr_month = m_month[month];
	double rate_esc = rate_scale[year];

	// fixed demand charge
	// compute charge based on tier structure for the month
	ssc_number_t charge = 0;
	ssc_number_t d_lower = 0;
	ssc_number_t total_charge = 0;
	ssc_number_t demand = curr_month.dc_flat_peak;
	bool found = false;
	for (tier = 0; tier < (int)curr_month.dc_flat_ub.size() && !found; tier++)
	{
		if (demand < curr_month.dc_flat_ub[tier])
		{
			found = true;
			charge += (demand - d_lower) *
				curr_month.dc_flat_ch[tier] * rate_esc;
			curr_month.dc_flat_charge = charge;
		}
		else
		{
			charge += (curr_month.dc_flat_ub[tier] - d_lower) *
				curr_month.dc_flat_ch[tier] * rate_esc;
			d_lower = curr_month.dc_flat_ub[tier];
		}
	}

	dc_hourly_peak[curr_month.dc_flat_peak_hour] = curr_month.dc_flat_peak;
	monthly_dc_fixed[month] = charge; // redundant...
	total_charge += charge;

	// TOU demand charge for each period find correct tier
	demand = 0;
	d_lower = 0;
	int peak_hour = 0;
	curr_month.dc_tou_charge.clear();
	for (period = 0; period < (int)curr_month.dc_tou_ub.nrows(); period++)
	{
		charge = 0;
		d_lower = 0;
		if (tou_demand_single_peak)
		{
			demand = curr_month.dc_flat_peak;
			if (curr_month.dc_flat_peak_hour != curr_month.dc_tou_peak_hour[period]) continue; // only one peak per month.
		}
		else if (period < curr_month.dc_periods.size())
			demand = curr_month.dc_tou_peak[period];
		// find tier corresponding to peak demand
		found = false;
		for (tier = 0; tier < (int)curr_month.dc_tou_ub.ncols() && !found; tier++)
		{
			if (demand < curr_month.dc_tou_ub.at(period, tier))
			{
				found = true;
				charge += (demand - d_lower) *
					curr_month.dc_tou_ch.at(period, tier) * rate_esc;
				curr_month.dc_tou_charge.push_back(charge);
			}
			else if (period < curr_month.dc_periods.size())
			{
				charge += (curr_month.dc_tou_ub.at(period, tier) - d_lower) * curr_month.dc_tou_ch.at(period, tier) * rate_esc;
				d_lower = curr_month.dc_tou_ub.at(period, tier);
			}
		}

		dc_hourly_peak[peak_hour] = demand;
		// add to payments
		monthly_dc_tou[month] += charge;
		total_charge += charge;
	}

	return total_charge;
}

int rate_data::get_tou_row(size_t year_one_index, int month)
{
	int period = m_ec_tou_sched[year_one_index];
	ur_month& curr_month = m_month[month];
	// find corresponding monthly period
	// check for valid period
	std::vector<int>::iterator per_num = std::find(curr_month.ec_periods.begin(), curr_month.ec_periods.end(), period);
	if (per_num == curr_month.ec_periods.end())
	{
		std::ostringstream ss;
		ss << "Energy rate Period " << period << " not found for Month " << month << ".";
		throw exec_error("lib_utility_rate_equations", ss.str());
	}
	return (int)(per_num - curr_month.ec_periods.begin());
}

int rate_data::transfer_surplus(ur_month& curr_month, ur_month& prev_month)
{
    int returnValue = 0;
    // check for surplus in previous month for same period
    for (size_t ir = 0; ir < prev_month.ec_energy_surplus.nrows(); ir++)
    {
        if (prev_month.ec_energy_surplus.at(ir, 0) > 0) // surplus - check period
        {
            int toup_source = prev_month.ec_periods[ir]; // number of rows of previous month - and period with surplus
            // find source period in rollover map for previous month
            std::vector<int>::iterator source_per_num = std::find(prev_month.ec_rollover_periods.begin(), prev_month.ec_rollover_periods.end(), toup_source);
            if (source_per_num == prev_month.ec_rollover_periods.end())
            {
                returnValue = 100 + toup_source;
            }
            else
            {
                // find corresponding target period for same time of day
                ssc_number_t extra = 0;
                int rollover_index = (int)(source_per_num - prev_month.ec_rollover_periods.begin());
                if (rollover_index < (int)curr_month.ec_rollover_periods.size())
                {
                    int toup_target = curr_month.ec_rollover_periods[rollover_index];
                    std::vector<int>::iterator target_per_num = std::find(curr_month.ec_periods.begin(), curr_month.ec_periods.end(), toup_target);
                    if (target_per_num == curr_month.ec_periods.end())
                    {
                        returnValue = 200 + toup_target;
                    }
                    int target_row = (int)(target_per_num - curr_month.ec_periods.begin());
                    for (size_t ic = 0; ic < prev_month.ec_energy_surplus.ncols(); ic++)
                        extra += prev_month.ec_energy_surplus.at(ir, ic);

                    curr_month.ec_energy_use(target_row, 0) += extra;
                }
            }
        }
    }
    return returnValue;
}

void rate_data::compute_surplus(ur_month& curr_month)
{
    // set surplus or use
    for (size_t ir = 0; ir < curr_month.ec_energy_use.nrows(); ir++)
    {
        if (curr_month.ec_energy_use.at(ir, 0) > 0)
        {
            curr_month.ec_energy_surplus.at(ir, 0) = curr_month.ec_energy_use.at(ir, 0);
            curr_month.ec_energy_use.at(ir, 0) = 0;
        }
        else
            curr_month.ec_energy_use.at(ir, 0) = -curr_month.ec_energy_use.at(ir, 0);
    }
}
