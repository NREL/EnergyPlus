#include "lib_battery_dispatch_automatic_fom.h"
#include "lib_battery_powerflow.h"
#include "lib_utility_rate.h"

#include <numeric>

dispatch_automatic_front_of_meter_t::dispatch_automatic_front_of_meter_t(
	battery_t * Battery,
	double dt_hour,
	double SOC_min,
	double SOC_max,
	int current_choice,
	double Ic_max,
	double Id_max,
	double Pc_max_kwdc,
	double Pd_max_kwdc,
	double Pc_max_kwac,
	double Pd_max_kwac,
	double t_min,
	int dispatch_mode,
	int pv_dispatch,
	size_t nyears,
	size_t look_ahead_hours,
	double dispatch_update_frequency_hours,
	bool can_charge,
	bool can_clip_charge,
	bool can_grid_charge,
	bool can_fuelcell_charge,
	double inverter_paco,
    std::vector<double> battReplacementCostPerkWh,
	int battCycleCostChoice,
    std::vector<double> battCycleCost,
	std::vector<double> forecast_price_series_dollar_per_kwh,
	UtilityRate * utilityRate,
	double etaPVCharge,
	double etaGridCharge,
	double etaDischarge) : dispatch_automatic_t(Battery, dt_hour, SOC_min, SOC_max, current_choice, Ic_max, Id_max, Pc_max_kwdc, Pd_max_kwdc, Pc_max_kwac, Pd_max_kwac,
		t_min, dispatch_mode, pv_dispatch, nyears, look_ahead_hours, dispatch_update_frequency_hours, can_charge, can_clip_charge, can_grid_charge, can_fuelcell_charge,
        battReplacementCostPerkWh, battCycleCostChoice, battCycleCost)
{
	// if look behind, only allow 24 hours
	if (_mode == dispatch_t::FOM_LOOK_BEHIND)
		_forecast_hours = 24;

	_inverter_paco = inverter_paco;
	_forecast_price_rt_series = forecast_price_series_dollar_per_kwh;

	// only create utility rate calculator if utility rate is defined
	if (utilityRate) {
		std::unique_ptr<UtilityRateCalculator> tmp(new UtilityRateCalculator(utilityRate, _steps_per_hour));
		m_utilityRateCalculator = std::move(tmp);
	}

	m_etaPVCharge = etaPVCharge * 0.01;
	m_etaGridCharge = etaGridCharge * 0.01;
	m_etaDischarge = etaDischarge * 0.01;

	revenueToClipCharge = revenueToDischarge = revenueToGridCharge = revenueToPVCharge = 0;

    costToCycle();
	setup_cost_forecast_vector();
}
dispatch_automatic_front_of_meter_t::~dispatch_automatic_front_of_meter_t(){ /* NOTHING TO DO */}
void dispatch_automatic_front_of_meter_t::init_with_pointer(const dispatch_automatic_front_of_meter_t* tmp)
{
	_forecast_hours = tmp->_forecast_hours;
	_inverter_paco = tmp->_inverter_paco;
	_forecast_price_rt_series = tmp->_forecast_price_rt_series;

	m_etaPVCharge = tmp->m_etaPVCharge;
	m_etaGridCharge = tmp->m_etaGridCharge;
	m_etaDischarge = tmp->m_etaDischarge;
}

void dispatch_automatic_front_of_meter_t::setup_cost_forecast_vector()
{
	std::vector<double> ppa_price_series;
	ppa_price_series.reserve(_forecast_price_rt_series.size());

	// add elements at beginning, so our forecast is looking at yesterday's prices
	if (_mode == dispatch_t::FOM_LOOK_BEHIND) {
		for (size_t i = 0; i != _forecast_hours * _steps_per_hour; i++)
			ppa_price_series.push_back(0);
	}

	// add elements at the end, so we have forecast information at end of year
	for (size_t i = 0; i != _forecast_price_rt_series.size(); i++){
		ppa_price_series.push_back(_forecast_price_rt_series[i]);
	}
	for (size_t i = 0; i != _forecast_hours * _steps_per_hour; i++) {
		ppa_price_series.push_back(_forecast_price_rt_series[i]);
	}
	_forecast_price_rt_series = ppa_price_series;
}

// deep copy from dispatch to this
dispatch_automatic_front_of_meter_t::dispatch_automatic_front_of_meter_t(const dispatch_t & dispatch) :
dispatch_automatic_t(dispatch)
{
	const dispatch_automatic_front_of_meter_t * tmp = dynamic_cast<const dispatch_automatic_front_of_meter_t *>(&dispatch);
	init_with_pointer(tmp);
}

// shallow copy from dispatch to this
void dispatch_automatic_front_of_meter_t::copy(const dispatch_t * dispatch)
{
	dispatch_automatic_t::copy(dispatch);
	const dispatch_automatic_front_of_meter_t * tmp = dynamic_cast<const dispatch_automatic_front_of_meter_t *>(dispatch);
	init_with_pointer(tmp);
}

void dispatch_automatic_front_of_meter_t::dispatch(size_t year,
	size_t hour_of_year,
	size_t step)
{
    curr_year = year;
	size_t step_per_hour = (size_t)(1 / _dt_hour);
	size_t lifetimeIndex = util::lifetimeIndex(year, hour_of_year, step, step_per_hour);

	update_dispatch(year, hour_of_year, step, lifetimeIndex);
	dispatch_automatic_t::dispatch(year, hour_of_year, step);
}

void dispatch_automatic_front_of_meter_t::update_dispatch(size_t year, size_t hour_of_year, size_t , size_t lifetimeIndex)
{
	// Initialize
	m_batteryPower->powerBatteryDC = 0;
	m_batteryPower->powerBatteryAC = 0;
	m_batteryPower->powerBatteryTarget = 0;


	if (_mode != dispatch_t::FOM_CUSTOM_DISPATCH)
	{

		// Power to charge (<0) or discharge (>0)
		double powerBattery = 0;

        /*! Cost to cycle the battery at all, using maximum DOD or user input */
        costToCycle();

        // Compute forecast variables which don't change from year to year
        size_t idx_year1 = hour_of_year * _steps_per_hour;
        size_t idx_lookahead = _forecast_hours * _steps_per_hour;
        auto max_ppa_cost = std::max_element(_forecast_price_rt_series.begin() + idx_year1, _forecast_price_rt_series.begin() + idx_year1 + idx_lookahead);
        auto min_ppa_cost = std::min_element(_forecast_price_rt_series.begin() + idx_year1, _forecast_price_rt_series.begin() + idx_year1 + idx_lookahead);
        double ppa_cost = _forecast_price_rt_series[idx_year1];

        /*! Cost to purchase electricity from the utility */
        double usage_cost = ppa_cost;
        std::vector<double> usage_cost_forecast;
        if (m_utilityRateCalculator) {
            usage_cost = m_utilityRateCalculator->getEnergyRate(hour_of_year);
            for (size_t i = hour_of_year; i < hour_of_year + _forecast_hours; i++)
            {
                for (size_t s = 0; s < _steps_per_hour; s++) {
                    usage_cost_forecast.push_back(m_utilityRateCalculator->getEnergyRate(i % 8760));
                }
            }
        }

        // Compute forecast variables which potentially do change from year to year
        double energyToStoreClipped = 0;
        if (_P_cliploss_dc.size() > lifetimeIndex + _forecast_hours) {
            energyToStoreClipped = std::accumulate(_P_cliploss_dc.begin() + lifetimeIndex, _P_cliploss_dc.begin() + lifetimeIndex + _forecast_hours * _steps_per_hour, 0.0) * _dt_hour;
        }

        /*! Economic benefit of charging from the grid in current time step to discharge sometime in next X hours ($/kWh)*/
        revenueToGridCharge = *max_ppa_cost * m_etaDischarge - usage_cost / m_etaGridCharge - m_cycleCost;

        /*! Computed revenue to charge from Grid in each of next X hours ($/kWh)*/
        double revenueToGridChargeMax = 0;
        if (m_batteryPower->canGridCharge) {
            std::vector<double> revenueToGridChargeForecast;
            size_t j = 0;
            for (size_t i = idx_year1; i < idx_year1 + idx_lookahead; i++) {
                if (m_utilityRateCalculator) {
                    revenueToGridChargeForecast.push_back(*max_ppa_cost * m_etaDischarge - usage_cost_forecast[j] / m_etaGridCharge - m_cycleCost);
                }
                else {
                    revenueToGridChargeForecast.push_back(*max_ppa_cost * m_etaDischarge - _forecast_price_rt_series[i] / m_etaGridCharge - m_cycleCost);
                }
                j++;
            }
            revenueToGridChargeMax = *std::max_element(std::begin(revenueToGridChargeForecast), std::end(revenueToGridChargeForecast));
        }

        /*! Economic benefit of charging from regular PV in current time step to discharge sometime in next X hours ($/kWh)*/
        revenueToPVCharge = _P_pv_ac[idx_year1] > 0 ? *max_ppa_cost * m_etaDischarge - ppa_cost / m_etaPVCharge - m_cycleCost : 0;

        /*! Computed revenue to charge from PV in each of next X hours ($/kWh)*/
        size_t t_duration = static_cast<size_t>(ceilf( (float)
                _Battery->energy_nominal() / (float) m_batteryPower->powerBatteryChargeMaxDC));
        size_t pv_hours_on;
        double revenueToPVChargeMax = 0;
        if (m_batteryPower->canSystemCharge) {
            std::vector<double> revenueToPVChargeForecast;
            for (size_t i = idx_year1; i < idx_year1 + idx_lookahead; i++) {
                // when considering grid charging, require PV output to exceed battery input capacity before accepting as a better option
                bool system_on = _P_pv_ac[i] >= m_batteryPower->powerBatteryChargeMaxDC ? 1 : 0;
                if (system_on) {
                    revenueToPVChargeForecast.push_back(system_on * (*max_ppa_cost * m_etaDischarge - _forecast_price_rt_series[i] / m_etaPVCharge - m_cycleCost));
                }
            }
            pv_hours_on = revenueToPVChargeForecast.size() / _steps_per_hour;
            revenueToPVChargeMax = pv_hours_on >= t_duration ? *std::max_element(std::begin(revenueToPVChargeForecast), std::end(revenueToPVChargeForecast)): 0;
        }

        /*! Economic benefit of charging from clipped PV in current time step to discharge sometime in the next X hours (clipped PV is free) ($/kWh) */
        revenueToClipCharge = *max_ppa_cost * m_etaDischarge - m_cycleCost;

        /*! Economic benefit of discharging in current time step ($/kWh) */
        revenueToDischarge = ppa_cost * m_etaDischarge - m_cycleCost;

        /*! Energy need to charge the battery (kWh) */
        double energyNeededToFillBattery = _Battery->energy_to_fill(m_batteryPower->stateOfChargeMax);

        /* Booleans to assist decisions */
        bool highDischargeValuePeriod = ppa_cost == *max_ppa_cost;
        bool highChargeValuePeriod = ppa_cost == *min_ppa_cost;
        bool excessAcCapacity = _inverter_paco > m_batteryPower->powerSystemThroughSharedInverter;
        bool batteryHasDischargeCapacity = _Battery->SOC() >= m_batteryPower->stateOfChargeMin + 1.0;

        // Always Charge if PV is clipping
        if (m_batteryPower->canClipCharge && m_batteryPower->powerSystemClipped > 0 && revenueToClipCharge > 0)
        {
            powerBattery = -m_batteryPower->powerSystemClipped;
        }

        // Increase charge from system (PV) if it is more valuable later than selling now
        if (m_batteryPower->canSystemCharge &&
            //revenueToPVCharge >= revenueToGridChargeMax &&
            revenueToPVCharge > 0 &&
            highChargeValuePeriod &&
            m_batteryPower->powerSystem > 0)
        {
            // leave EnergyToStoreClipped capacity in battery
            if (m_batteryPower->canClipCharge)
            {
                if (energyToStoreClipped < energyNeededToFillBattery)
                {
                    double energyCanCharge = (energyNeededToFillBattery - energyToStoreClipped);
                    if (energyCanCharge <= m_batteryPower->powerSystem * _dt_hour)
                        powerBattery = -std::fmax(energyCanCharge / _dt_hour, m_batteryPower->powerSystemClipped);
                    else
                        powerBattery = -std::fmax(m_batteryPower->powerSystem, m_batteryPower->powerSystemClipped);

                    energyNeededToFillBattery = std::fmax(0, energyNeededToFillBattery + (powerBattery * _dt_hour));
                }

            }
            // otherwise, don't reserve capacity for clipping
            else {
                powerBattery = -m_batteryPower->powerSystem;
            }
        }

        // Also charge from grid if it is valuable to do so, still leaving EnergyToStoreClipped capacity in battery
        if (m_batteryPower->canGridCharge &&
            revenueToGridCharge >= revenueToPVChargeMax &&
            revenueToGridCharge > 0 &&
            highChargeValuePeriod &&
            energyNeededToFillBattery > 0)
        {
            // leave EnergyToStoreClipped capacity in battery
            if (m_batteryPower->canClipCharge)
            {
                if (energyToStoreClipped < energyNeededToFillBattery)
                {
                    double energyCanCharge = (energyNeededToFillBattery - energyToStoreClipped);
                    powerBattery -= energyCanCharge / _dt_hour;
                }
            }
            else
                powerBattery = -energyNeededToFillBattery / _dt_hour;
        }

        // Discharge if we are in a high-price period and have battery and inverter capacity
        if (highDischargeValuePeriod && revenueToDischarge > 0 && excessAcCapacity && batteryHasDischargeCapacity) {
            double loss_kw = _Battery->calculate_loss(m_batteryPower->powerBatteryTarget, lifetimeIndex); // Battery is responsible for covering discharge losses
            if (m_batteryPower->connectionMode == BatteryPower::DC_CONNECTED) {
                powerBattery = _inverter_paco + loss_kw - m_batteryPower->powerSystem;
            }
            else {
                powerBattery = _inverter_paco; // AC connected battery is already maxed out by AC power limit, cannot increase dispatch to ccover losses
            }
        }
		// save for extraction
		m_batteryPower->powerBatteryTarget = powerBattery;
	}
    // Custom dispatch
	else
	{
		// extract input power by modifying lifetime index to year 1
		m_batteryPower->powerBatteryTarget = _P_battery_use[lifetimeIndex % (8760 * _steps_per_hour)];
        double loss_kw = _Battery->calculate_loss(m_batteryPower->powerBatteryTarget, lifetimeIndex); // Battery is responsible for covering discharge losses
        if (m_batteryPower->connectionMode == AC_CONNECTED){
            m_batteryPower->powerBatteryTarget = m_batteryPower->adjustForACEfficiencies(m_batteryPower->powerBatteryTarget, loss_kw);
        }
        else if (m_batteryPower->powerBatteryTarget > 0) {
            // Adjust for DC discharge losses
            m_batteryPower->powerBatteryTarget += loss_kw;
        }

	}

	m_batteryPower->powerBatteryDC = m_batteryPower->powerBatteryTarget;
}

void dispatch_automatic_front_of_meter_t::update_pv_data(double_vec P_pv_ac)
{
	_P_pv_ac = P_pv_ac;

	// append to end to allow for look-ahead
	for (size_t i = 0; i != _forecast_hours * _steps_per_hour; i++)
		_P_pv_ac.push_back(P_pv_ac[i]);
}

void dispatch_automatic_front_of_meter_t::costToCycle()
{
    // Calculate assuming maximum depth of discharge (most conservative assumption)
    if (m_battCycleCostChoice == dispatch_t::MODEL_CYCLE_COST)
    {
        double capacityPercentDamagePerCycle = _Battery->estimateCycleDamage();
        m_cycleCost = 0.01 * capacityPercentDamagePerCycle * m_battReplacementCostPerKWH[curr_year];
    }
    else if(m_battCycleCostChoice == dispatch_t::INPUT_CYCLE_COST)
    {
        m_cycleCost = cycle_costs_by_year[curr_year];
    }
}
