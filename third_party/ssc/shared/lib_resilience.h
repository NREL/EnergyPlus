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

#ifndef SYSTEM_ADVISOR_MODEL_LIB_RESILIENCE_H
#define SYSTEM_ADVISOR_MODEL_LIB_RESILIENCE_H

#include <numeric>
#include <algorithm>
#include <functional>

#include "lib_shared_inverter.h"
#include "lib_battery_dispatch.h"
#include "lib_battery_powerflow.h"
#include "../ssc/cmod_battery.h"

/**
*
* \class dispatch_resilience
*
*  Dispatches the battery when the grid is unavailable is during an outage.
*
*  The battery model is run according to the purpose of meeting a critical load for as many time steps as possible.
*  Normal operational limits such as min/max state-of-charge and dis/charging power and current are disregarded.
*
*  A fully-initialized dispatch_t instance is required to construct a dispatch_resilience. The original dispatch_t's
*  dispatch algorithm is not used, but its underlying models (capacity, voltage, inverter for DC-connected batteries, etc)
*  and certain dispatch parameters (conversion efficiencies, connection mode, time interval) are required.
*
* The start_outage_index is the time step at which the outage starts. Each time one of the run_outage_step functions are
* run, met_loads_kw is updated and if the battery system meets the critical load, the current_outage_index is incremented.
*
*/
class dispatch_resilience : public dispatch_t {
public:

    /// Construct from fully-initialized dispatch_t
    dispatch_resilience(const dispatch_t &orig, size_t start_index);

	/// Delete battery and battery initial
	~dispatch_resilience();

    /// AC or DC connection
    const CONNECTION connection;

    /// Runs a time step of an outage for an AC-connected battery, returns true if critical load was met
    bool run_outage_step_ac(double crit_load_kwac, double pv_kwac);

    /// Runs a time step of an outage for an DC-connected battery, returns true if critical load was met
    bool run_outage_step_dc(double crit_load_kwac, double pv_kwdc, double V_pv, double pv_clipped, double tdry);

    /// Returns the number of time steps during which the battery has met the critical load
    size_t get_indices_survived();

    /// Returns the total critical loads met at all survived time steps [kW]
    double get_met_loads();

protected:
    size_t start_outage_index;
    size_t current_outage_index;
    double met_loads_kw;

    /// valid inverter required for DC-connected batteries
    std::unique_ptr<SharedInverter> inverter;

    /// dispatch the battery model with the target power, where kw < 0 is discharging
    double dispatch_kw(double kw);

    void dispatch(size_t, size_t, size_t) override {}
};

/**
*
* \class resilience_runner
*
*  Maintains a collection of batteries operating for resilience (no grid access, access to renewable energy sources)
*  organized by the starting time step of the outage, for calculating annual or lifetime statistics about hours of
*  autonomy and total met loads for a battery system design.
*
*  An example from cmod_battwatts.cpp for calculating how a battery would respond if an outage occurred at every time step
*  of the simulation. This is done by simulating the original battery system as it steps through the entire simulation
*  while adding a copy of the dispatch_t to resilience_runner for simulating outage conditions. After the annual simulation
*  is complete, the battery added at the last time step (and likely several of the ones added before that) is still
*  surviving so run_surviving_batteries_by_looping simulates how many hours the batteries still surviving would last until
*  the next year assuming the critical load and pv production is exactly the same.
*
*    for (hour = 0; hour < 8760; hour++)
*    {
*       for (size_t jj = 0; jj < batt->step_per_hour; jj++)
*       {
*           batt->initialize_time(year, hour, jj);
*
*           resilience->add_battery_at_outage_timestep(*batt->dispatch_model, count);
*           resilience->run_surviving_batteries(p_crit_load[count % n_rec_single_year], p_ac[count]);
*
*           batt->advance(m_vartab, p_ac[count], voltage, p_load[count]);
*           p_gen[count] = batt->outGenPower[count];
*           count++;
*       }
*    }
*
*    resilience->run_surviving_batteries_by_looping(&p_crit_load[0], &p_ac[0]);
*
*  Provides metrics for the total load met and the time steps survived for each outage.
*/

class resilience_runner {
private:
    /// Required for time interval, battery connection and inverter parameters
    std::shared_ptr<battstor> batt;

    /// Outage simulations mapped by the index at which the outage started
    std::map<size_t, std::shared_ptr<dispatch_resilience>> battery_per_outage_start;

    /// i-th entry is the number of time steps survived for an outage starting at time step i
    std::vector<size_t> indices_survived;

    /// i-th entry is the total load met during an outage starting at time step i
    std::vector<double> total_load_met;

    /// i-th entry is the i-th smallest possible number of hours survived during any outage
    std::vector<double> outage_durations;

    /// i-th entry corresponds to the number of hours in the i-th entry of outage_durations
    std::vector<double> probs_of_surviving;

    std::vector<std::string> logs;

public:
    /// Construct from fully-initialized battstor with time interval information
    explicit resilience_runner(const std::shared_ptr<battstor>& battery);

    std::vector<std::string> get_logs() {return logs;}

    /// Adds a battery operating during an outage starting at given index for simulating hours of autonomy
    void add_battery_at_outage_timestep(const dispatch_t& orig, size_t index);

    /// Given the crit load and PV production (and string voltage, clipped pv power and temperature for DC-connected),
    /// run another outage time step for all surviving batteries
    void run_surviving_batteries(double crit_loads_kwac, double pv_kwac, double pv_kwdc = 0., double V = 0.,
                                 double pv_clipped_kw = 0., double tdry_c = 0.);

    /// Critical load and temp are single year arrays; pv production, string voltage, clipped power are lifetime arrays
    void run_surviving_batteries_by_looping(double* crit_loads_kwac, double* pv_kwac, double* pv_kwdc = nullptr,
                                            double* V = nullptr, double* pv_clipped_kw = nullptr, double* tdry_c = nullptr);

    /// Return average hours survived
    double compute_metrics();

    /// Returns how many batteries are still operating during their respective outages
    size_t get_n_surviving_batteries();

    /// Returns how many hours were survived by a battery during an outage that started at the vector's index
    std::vector<double> get_hours_survived();

    /// Returns the average critical energy load met
    double get_avg_crit_load_kwh();

    /// Returns the set of possible hours of autonomy, ordered from least to greatest
    std::vector<double> get_outage_duration_hrs();

    /// i-th entry is the probability of surviving for x hours, where x is the i-th entry of get_outage_duration_hrs
    std::vector<double> get_probs_of_surviving();

    /// i-th entry is the probability of surviving for at least x hours, where x is the i-th entry of get_outage_duration_hrs
    std::vector<double> get_cdf_of_surviving();

    /// i-th entry is the probability of surviving for greater than x hours, where x is the i-th entry of get_outage_duration_hrs
    std::vector<double> get_survival_function();
};


#endif //SYSTEM_ADVISOR_MODEL_LIB_RESILIENCE_H
