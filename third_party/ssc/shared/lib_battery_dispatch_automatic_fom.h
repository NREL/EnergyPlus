/**
BSD-3-Clause
Copyright 2019 Alliance for Sustainable Energy, LLC
Redistribution and use in source and binary forms, with or without modification, are permitted provided
that the following conditions are met :
1.	Redistributions of source code must retain the above copyright notice, this list of conditions
and the following disclaimer.
2.	Redistributions in binary form must reproduce the above copyright notice, this list of conditions
and the following disclaimer in the documentation and/or other materials provided with the distribution.
3.	Neither the name of the copyright holder nor the names of its contributors may be used to endorse
or promote products derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES,
INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED.IN NO EVENT SHALL THE COPYRIGHT HOLDER, CONTRIBUTORS, UNITED STATES GOVERNMENT OR UNITED STATES
DEPARTMENT OF ENERGY, NOR ANY OF THEIR EMPLOYEES, BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY,
OR CONSEQUENTIAL DAMAGES(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT
OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/
#ifndef __LIB_BATTERY_DISPATCH_AUTOMATIC_FOM_H__
#define __LIB_BATTERY_DISPATCH_AUTOMATIC_FOM_H__

#include "lib_battery_dispatch.h"

/*! Automated Front of Meter DC-connected battery dispatch */
class dispatch_automatic_front_of_meter_t : public dispatch_automatic_t
{
public:
	/**
	 Class takes forecast information about the PV production and Load Profile, plus PPA sell rate and electricity buy-rate signals
	 and programs battery to strategically dispatch to maximize economic benefit by:
	 1. Discharging during times of high PPA sell rates
	 2. Charging from the grid during times of low electricity buy-rates (if grid charging allowed)
	 3. Charging from the PV array during times of low PPA sell rates
	 4. Charging from the PV array during times where the PV power would be clipped due to inverter limits (if DC-connected)
	*/
	dispatch_automatic_front_of_meter_t(
		battery_t * Battery,
		double dt,
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
		bool can_clipcharge,
		bool can_grid_charge,
		bool can_fuelcell_charge,
		double inverter_paco,
        std::vector<double> battReplacementCostPerkWh,
		int battCycleCostChoice,
        std::vector<double> battCycleCost,
		std::vector<double> ppa_price_series_dollar_per_kwh,
		UtilityRate * utilityRate,
		double etaPVCharge,
		double etaGridCharge,
		double etaDischarge
		);

	~dispatch_automatic_front_of_meter_t();

	/*! deep copy constructor (new memory), from dispatch to this */
	dispatch_automatic_front_of_meter_t(const dispatch_t& dispatch);

	/*! shallow copy from dispatch to this */
	void copy(const dispatch_t* dispatch);

	/// Public API to run the battery dispatch model for the current timestep, given the system power, and optionally the electric load, amount of system clipping, or specified battery power
	void dispatch(size_t year,
		size_t hour_of_year,
		size_t step);

	/// Compute the updated power to send to the battery over the next N hours
	void update_dispatch(size_t year, size_t hour_of_year, size_t step, size_t lifetimeIndex);

	/// Pass in the PV power forecast [kW]
    void update_pv_data(double_vec P_pv_ac);

	/// Return benefit calculations
	double benefit_charge(){ return revenueToPVCharge; }
	double benefit_gridcharge() { return revenueToGridCharge; }
	double benefit_clipcharge() { return revenueToClipCharge; }
	double benefit_discharge() { return revenueToDischarge; }


protected:

	void init_with_pointer(const dispatch_automatic_front_of_meter_t* tmp);
	void setup_cost_forecast_vector();

    /*! Calculate the cost to cycle per kWh */
    void costToCycle();

	/*! Inverter AC power limit */
	double _inverter_paco;

	/*! Market real time and forecast prices */
	std::vector<double> _forecast_price_rt_series;

	/*! Utility rate information */
    std::shared_ptr<UtilityRateCalculator> m_utilityRateCalculator;

	/*! Efficiencies of the charge and discharge of the battery*/
	double m_etaPVCharge;
	double m_etaGridCharge;
	double m_etaDischarge;

	/* Computed benefits to charge, discharge, gridcharge, clipcharge */
	double revenueToPVCharge;
	double revenueToGridCharge;
	double revenueToClipCharge;
	double revenueToDischarge;
};

#endif // __LIB_BATTERY_DISPATCH_AUTOMATIC_FOM_H__
