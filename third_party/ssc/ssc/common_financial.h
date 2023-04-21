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

#ifndef __common_financial_h
#define __common_financial_h

#include <vector>
#include "core.h"


double Const_per_principal(double const_per_percent /*%*/, double total_installed_cost /*$*/);		// [$]

double Const_per_interest(double const_per_principal /*$*/, double const_per_interest_rate /*$*/,
	double const_per_months /*months*/);		// [$]

double Const_per_total(double const_per_interest /*$*/, double const_per_principal /*$*/,
	double const_per_upfront_rate /*%*/);		// [$]


void save_cf(compute_module *cm, util::matrix_t<double>& mat, int cf_line, int nyears, const std::string &name);



class dispatch_calculations
{
private:
	compute_module *m_cm;
	std::vector<int> m_periods;
	std::string m_error;
	util::matrix_t<double> m_cf;
	std::vector<double> m_degradation;
	std::vector<double> m_hourly_energy;
	int m_nyears;
	bool m_timestep;
	ssc_number_t *m_gen; // Time series power
	ssc_number_t *m_multipliers; // Time series ppa multiplers
	size_t m_ngen; // Number of records in gen
	size_t m_nmultipliers; // Number of records in m_multipliers

public:
	dispatch_calculations() {};
	dispatch_calculations(compute_module *cm, std::vector<double>& degradation, std::vector<double>& hourly_energy);
	bool init(compute_module *cm, std::vector<double>& degradation, std::vector<double>& hourly_energy);
	bool setup();
	bool setup_ts();
	bool compute_outputs(std::vector<double>& ppa);
	bool compute_outputs_ts(std::vector<double>& ppa);
	int operator()(size_t time);
	std::string error() { return m_error; }
	bool process_dispatch_output();
	bool compute_dispatch_output();
	bool process_lifetime_dispatch_output();
	bool compute_lifetime_dispatch_output();
	bool compute_dispatch_output_ts();
	bool compute_lifetime_dispatch_output_ts();
	util::matrix_t<double>& dispatch_output();
	double tod_energy(int period, int year);
	double tod_energy_value(int period, int year);
	double tod_energy_value(int year);
};


class hourly_energy_calculation
{
private:
	compute_module *m_cm;
	std::vector<double> m_hourly_energy; // Energy used in PPA calculations
    std::vector<double> m_energy_sales; // Hourly gen values > 0
    std::vector<double> m_energy_purchases; // Hourly gen values < 0
    std::vector<double> m_energy_without_battery;
	std::string m_error;
	size_t m_nyears;
    ssc_number_t m_ts_hour_gen;
    size_t m_step_per_hour_gen;

public:
	bool calculate(compute_module *cm);
	std::vector<double>& hourly_energy() {
		return m_hourly_energy;
	}
    std::vector<double>& hourly_sales() {
        return m_energy_sales;
    }
    std::vector<double>& hourly_purchases() {
        return m_energy_purchases;
    }
    std::vector<double>& hourly_energy_without_battery() {
        return m_energy_without_battery;
    }
	std::string error() { return m_error; }
    void sum_ts_to_hourly(ssc_number_t* timestep_power, std::vector<double>& hourly);
};




/*
extern var_info vtab_advanced_financing_cost[];


class advanced_financing_cost
{
private:
	compute_module *m_cm;

public:
	advanced_financing_cost(compute_module *cm);
	bool compute_cost(double cost_installed, double equity, double debt, double cbi, double ibi);
};
*/


#endif

