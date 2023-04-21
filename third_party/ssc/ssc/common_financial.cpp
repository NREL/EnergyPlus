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

#include "common_financial.h"
#include "core.h"
#include <sstream>
#include <sstream>

#ifndef WIN32
#include <float.h>
#endif


// P
double Const_per_principal(double const_per_percent /*%*/, double total_installed_cost /*$*/) {		// [$]
	return (const_per_percent / 100.) * total_installed_cost;
}

// I
double Const_per_interest(double const_per_principal /*$*/, double const_per_interest_rate /*$*/,
	double const_per_months /*months*/) {		// [$]
	return const_per_principal * (const_per_interest_rate / 100.) / 12. * const_per_months / 2;
}

// F
double Const_per_total(double const_per_interest /*$*/, double const_per_principal /*$*/,
	double const_per_upfront_rate /*%*/) {		// [$]
	
	double up_front_fee = const_per_principal * (const_per_upfront_rate / 100.);
	return const_per_interest + up_front_fee;
}



void save_cf(compute_module *cm, util::matrix_t<double>& mat, int cf_line, int m_nyears, const std::string &name)
{
	ssc_number_t *arrp = cm->allocate(name, m_nyears + 1);
	for (size_t i = 0; i <= (size_t)m_nyears; i++)
		arrp[i] = (ssc_number_t)mat.at(cf_line, i);
}



enum {
	// Dispatch

	CF_TODJanEnergy,
	CF_TODFebEnergy,
	CF_TODMarEnergy,
	CF_TODAprEnergy,
	CF_TODMayEnergy,
	CF_TODJunEnergy,
	CF_TODJulEnergy,
	CF_TODAugEnergy,
	CF_TODSepEnergy,
	CF_TODOctEnergy,
	CF_TODNovEnergy,
	CF_TODDecEnergy,

	CF_TODJanRevenue,
	CF_TODFebRevenue,
	CF_TODMarRevenue,
	CF_TODAprRevenue,
	CF_TODMayRevenue,
	CF_TODJunRevenue,
	CF_TODJulRevenue,
	CF_TODAugRevenue,
	CF_TODSepRevenue,
	CF_TODOctRevenue,
	CF_TODNovRevenue,
	CF_TODDecRevenue,

	CF_max_timestep,


	CF_TOD1Energy,
	CF_TOD2Energy,
	CF_TOD3Energy,
	CF_TOD4Energy,
	CF_TOD5Energy,
	CF_TOD6Energy,
	CF_TOD7Energy,
	CF_TOD8Energy,
	CF_TOD9Energy,

	CF_TOD1JanEnergy,
	CF_TOD1FebEnergy,
	CF_TOD1MarEnergy,
	CF_TOD1AprEnergy,
	CF_TOD1MayEnergy,
	CF_TOD1JunEnergy,
	CF_TOD1JulEnergy,
	CF_TOD1AugEnergy,
	CF_TOD1SepEnergy,
	CF_TOD1OctEnergy,
	CF_TOD1NovEnergy,
	CF_TOD1DecEnergy,

	CF_TOD2JanEnergy,
	CF_TOD2FebEnergy,
	CF_TOD2MarEnergy,
	CF_TOD2AprEnergy,
	CF_TOD2MayEnergy,
	CF_TOD2JunEnergy,
	CF_TOD2JulEnergy,
	CF_TOD2AugEnergy,
	CF_TOD2SepEnergy,
	CF_TOD2OctEnergy,
	CF_TOD2NovEnergy,
	CF_TOD2DecEnergy,

	CF_TOD3JanEnergy,
	CF_TOD3FebEnergy,
	CF_TOD3MarEnergy,
	CF_TOD3AprEnergy,
	CF_TOD3MayEnergy,
	CF_TOD3JunEnergy,
	CF_TOD3JulEnergy,
	CF_TOD3AugEnergy,
	CF_TOD3SepEnergy,
	CF_TOD3OctEnergy,
	CF_TOD3NovEnergy,
	CF_TOD3DecEnergy,

	CF_TOD4JanEnergy,
	CF_TOD4FebEnergy,
	CF_TOD4MarEnergy,
	CF_TOD4AprEnergy,
	CF_TOD4MayEnergy,
	CF_TOD4JunEnergy,
	CF_TOD4JulEnergy,
	CF_TOD4AugEnergy,
	CF_TOD4SepEnergy,
	CF_TOD4OctEnergy,
	CF_TOD4NovEnergy,
	CF_TOD4DecEnergy,

	CF_TOD5JanEnergy,
	CF_TOD5FebEnergy,
	CF_TOD5MarEnergy,
	CF_TOD5AprEnergy,
	CF_TOD5MayEnergy,
	CF_TOD5JunEnergy,
	CF_TOD5JulEnergy,
	CF_TOD5AugEnergy,
	CF_TOD5SepEnergy,
	CF_TOD5OctEnergy,
	CF_TOD5NovEnergy,
	CF_TOD5DecEnergy,

	CF_TOD6JanEnergy,
	CF_TOD6FebEnergy,
	CF_TOD6MarEnergy,
	CF_TOD6AprEnergy,
	CF_TOD6MayEnergy,
	CF_TOD6JunEnergy,
	CF_TOD6JulEnergy,
	CF_TOD6AugEnergy,
	CF_TOD6SepEnergy,
	CF_TOD6OctEnergy,
	CF_TOD6NovEnergy,
	CF_TOD6DecEnergy,

	CF_TOD7JanEnergy,
	CF_TOD7FebEnergy,
	CF_TOD7MarEnergy,
	CF_TOD7AprEnergy,
	CF_TOD7MayEnergy,
	CF_TOD7JunEnergy,
	CF_TOD7JulEnergy,
	CF_TOD7AugEnergy,
	CF_TOD7SepEnergy,
	CF_TOD7OctEnergy,
	CF_TOD7NovEnergy,
	CF_TOD7DecEnergy,

	CF_TOD8JanEnergy,
	CF_TOD8FebEnergy,
	CF_TOD8MarEnergy,
	CF_TOD8AprEnergy,
	CF_TOD8MayEnergy,
	CF_TOD8JunEnergy,
	CF_TOD8JulEnergy,
	CF_TOD8AugEnergy,
	CF_TOD8SepEnergy,
	CF_TOD8OctEnergy,
	CF_TOD8NovEnergy,
	CF_TOD8DecEnergy,

	CF_TOD9JanEnergy,
	CF_TOD9FebEnergy,
	CF_TOD9MarEnergy,
	CF_TOD9AprEnergy,
	CF_TOD9MayEnergy,
	CF_TOD9JunEnergy,
	CF_TOD9JulEnergy,
	CF_TOD9AugEnergy,
	CF_TOD9SepEnergy,
	CF_TOD9OctEnergy,
	CF_TOD9NovEnergy,
	CF_TOD9DecEnergy,

	CF_TOD1Revenue,
	CF_TOD2Revenue,
	CF_TOD3Revenue,
	CF_TOD4Revenue,
	CF_TOD5Revenue,
	CF_TOD6Revenue,
	CF_TOD7Revenue,
	CF_TOD8Revenue,
	CF_TOD9Revenue,

	CF_revenue_monthly_firstyear_TOD1,
	CF_energy_net_monthly_firstyear_TOD1,
	CF_revenue_monthly_firstyear_TOD2,
	CF_energy_net_monthly_firstyear_TOD2,
	CF_revenue_monthly_firstyear_TOD3,
	CF_energy_net_monthly_firstyear_TOD3,
	CF_revenue_monthly_firstyear_TOD4,
	CF_energy_net_monthly_firstyear_TOD4,
	CF_revenue_monthly_firstyear_TOD5,
	CF_energy_net_monthly_firstyear_TOD5,
	CF_revenue_monthly_firstyear_TOD6,
	CF_energy_net_monthly_firstyear_TOD6,
	CF_revenue_monthly_firstyear_TOD7,
	CF_energy_net_monthly_firstyear_TOD7,
	CF_revenue_monthly_firstyear_TOD8,
	CF_energy_net_monthly_firstyear_TOD8,
	CF_revenue_monthly_firstyear_TOD9,
	CF_energy_net_monthly_firstyear_TOD9,

	CF_max_dispatch
};



//var_info vtab_dispatch_periods[] = {
	/*   VARTYPE           DATATYPE         NAME                               LABEL                                       UNITS     META                                     GROUP                 REQUIRED_IF                 CONSTRAINTS                      UI_HINTS*/

	//{ SSC_INPUT, SSC_NUMBER, "dispatch_factor1", "Dispatch period 1 value", "", "", "Dispatch values", "*", "POSITIVE", "" },
	//{ SSC_INPUT, SSC_NUMBER, "dispatch_factor2", "Dispatch period 2 value", "", "", "Dispatch values", "*", "POSITIVE", "" },
	//{ SSC_INPUT, SSC_NUMBER, "dispatch_factor3", "Dispatch period 3 value", "", "", "Dispatch values", "*", "POSITIVE", "" },
	//{ SSC_INPUT, SSC_NUMBER, "dispatch_factor4", "Dispatch period 4 value", "", "", "Dispatch values", "*", "POSITIVE", "" },
	//{ SSC_INPUT, SSC_NUMBER, "dispatch_factor5", "Dispatch period 5 value", "", "", "Dispatch values", "*", "POSITIVE", "" },
	//{ SSC_INPUT, SSC_NUMBER, "dispatch_factor6", "Dispatch period 6 value", "", "", "Dispatch values", "*", "POSITIVE", "" },
	//{ SSC_INPUT, SSC_NUMBER, "dispatch_factor7", "Dispatch period 7 value", "", "", "Dispatch values", "*", "POSITIVE", "" },
	//{ SSC_INPUT, SSC_NUMBER, "dispatch_factor8", "Dispatch period 8 value", "", "", "Dispatch values", "*", "POSITIVE", "" },
	//{ SSC_INPUT, SSC_NUMBER, "dispatch_factor9", "Dispatch period 9 value", "", "", "Dispatch values", "*", "POSITIVE", "" },
//	{ SSC_INPUT, SSC_MATRIX, "dispatch_sched_weekday", "Diurnal weekday dispatch periods", "1..9", "12 x 24 matrix", "Dispatch values", "*", "", "" },
//	{ SSC_INPUT, SSC_MATRIX, "dispatch_sched_weekend", "Diurnal weekend dispatch periods", "1..9", "12 x 24 matrix", "Dispatch values", "*", "", "" },

//	var_info_invalid };


dispatch_calculations::dispatch_calculations(compute_module *cm, std::vector<double>& degradation, std::vector<double>& hourly_energy)
{
	init(cm, degradation, hourly_energy);
}

bool dispatch_calculations::init(compute_module *cm, std::vector<double>& degradation, std::vector<double>& hourly_energy)
{
	if (!cm) return false;

	m_cm = cm;
	m_degradation = degradation;
	m_hourly_energy = hourly_energy;
	m_timestep = (m_cm->as_integer("ppa_multiplier_model")==1);

	m_nyears = m_cm->as_integer("analysis_period");
	if (m_degradation.size() != (size_t)m_nyears + 1) return false;

	if (m_timestep)
	{
		setup_ts();
		if (m_cm->as_integer("system_use_lifetime_output"))
			compute_lifetime_dispatch_output_ts(); // TODO - finish and test this!!
		else
			compute_dispatch_output_ts();
	}
	else
	{
		setup();
		if (m_cm->as_integer("system_use_lifetime_output"))
			compute_lifetime_dispatch_output();
		else
			compute_dispatch_output();
	}
	return true;
}

bool dispatch_calculations::compute_outputs_ts(std::vector<double>& ppa)
{

	if (ppa.size() != (size_t)m_nyears + 1) return false;

	// outputs
	// dispatch energy
	save_cf(m_cm, m_cf, CF_TODJanEnergy, m_nyears, "cf_energy_net_jan");
	save_cf(m_cm, m_cf, CF_TODFebEnergy, m_nyears, "cf_energy_net_feb");
	save_cf(m_cm, m_cf, CF_TODMarEnergy, m_nyears, "cf_energy_net_mar");
	save_cf(m_cm, m_cf, CF_TODAprEnergy, m_nyears, "cf_energy_net_apr");
	save_cf(m_cm, m_cf, CF_TODMayEnergy, m_nyears, "cf_energy_net_may");
	save_cf(m_cm, m_cf, CF_TODJunEnergy, m_nyears, "cf_energy_net_jun");
	save_cf(m_cm, m_cf, CF_TODJulEnergy, m_nyears, "cf_energy_net_jul");
	save_cf(m_cm, m_cf, CF_TODAugEnergy, m_nyears, "cf_energy_net_aug");
	save_cf(m_cm, m_cf, CF_TODSepEnergy, m_nyears, "cf_energy_net_sep");
	save_cf(m_cm, m_cf, CF_TODOctEnergy, m_nyears, "cf_energy_net_oct");
	save_cf(m_cm, m_cf, CF_TODNovEnergy, m_nyears, "cf_energy_net_nov");
	save_cf(m_cm, m_cf, CF_TODDecEnergy, m_nyears, "cf_energy_net_dec");

	for (int y = 0; y <= m_nyears; y++)
	{
		// compute energy value
		m_cf.at(CF_TODJanRevenue, y) *= (ppa[y] / 100.0);
		m_cf.at(CF_TODFebRevenue, y) *= (ppa[y] / 100.0);
		m_cf.at(CF_TODMarRevenue, y) *= (ppa[y] / 100.0);
		m_cf.at(CF_TODAprRevenue, y) *= (ppa[y] / 100.0);
		m_cf.at(CF_TODMayRevenue, y) *= (ppa[y] / 100.0);
		m_cf.at(CF_TODJunRevenue, y) *= (ppa[y] / 100.0);
		m_cf.at(CF_TODJulRevenue, y) *= (ppa[y] / 100.0);
		m_cf.at(CF_TODAugRevenue, y) *= (ppa[y] / 100.0);
		m_cf.at(CF_TODSepRevenue, y) *= (ppa[y] / 100.0);
		m_cf.at(CF_TODOctRevenue, y) *= (ppa[y] / 100.0);
		m_cf.at(CF_TODNovRevenue, y) *= (ppa[y] / 100.0);
		m_cf.at(CF_TODDecRevenue, y) *= (ppa[y] / 100.0);
	}

	// dispatch revenue
	save_cf(m_cm, m_cf, CF_TODJanRevenue, m_nyears, "cf_revenue_jan");
	save_cf(m_cm, m_cf, CF_TODFebRevenue, m_nyears, "cf_revenue_feb");
	save_cf(m_cm, m_cf, CF_TODMarRevenue, m_nyears, "cf_revenue_mar");
	save_cf(m_cm, m_cf, CF_TODAprRevenue, m_nyears, "cf_revenue_apr");
	save_cf(m_cm, m_cf, CF_TODMayRevenue, m_nyears, "cf_revenue_may");
	save_cf(m_cm, m_cf, CF_TODJunRevenue, m_nyears, "cf_revenue_jun");
	save_cf(m_cm, m_cf, CF_TODJulRevenue, m_nyears, "cf_revenue_jul");
	save_cf(m_cm, m_cf, CF_TODAugRevenue, m_nyears, "cf_revenue_aug");
	save_cf(m_cm, m_cf, CF_TODSepRevenue, m_nyears, "cf_revenue_sep");
	save_cf(m_cm, m_cf, CF_TODOctRevenue, m_nyears, "cf_revenue_oct");
	save_cf(m_cm, m_cf, CF_TODNovRevenue, m_nyears, "cf_revenue_nov");
	save_cf(m_cm, m_cf, CF_TODDecRevenue, m_nyears, "cf_revenue_dec");

	return true;
}


bool dispatch_calculations::compute_outputs( std::vector<double>& ppa)
{
	if (ppa.size() != (size_t)m_nyears+1) return false;

	if (m_timestep)
	{
		return compute_outputs_ts(ppa);
	}

	size_t i;
	double dispatch_factor1 = m_cm->as_double("dispatch_factor1");
	double dispatch_factor2 = m_cm->as_double("dispatch_factor2");
	double dispatch_factor3 = m_cm->as_double("dispatch_factor3");
	double dispatch_factor4 = m_cm->as_double("dispatch_factor4");
	double dispatch_factor5 = m_cm->as_double("dispatch_factor5");
	double dispatch_factor6 = m_cm->as_double("dispatch_factor6");
	double dispatch_factor7 = m_cm->as_double("dispatch_factor7");
	double dispatch_factor8 = m_cm->as_double("dispatch_factor8");
	double dispatch_factor9 = m_cm->as_double("dispatch_factor9");

	if (m_cm->as_integer("system_use_lifetime_output"))
		process_lifetime_dispatch_output();
	else
		process_dispatch_output();


// outputs
	// dispatch energy
	save_cf( m_cm, m_cf,  CF_TODJanEnergy, m_nyears, "cf_energy_net_jan");
	save_cf( m_cm, m_cf,  CF_TODFebEnergy, m_nyears, "cf_energy_net_feb");
	save_cf( m_cm, m_cf,  CF_TODMarEnergy, m_nyears, "cf_energy_net_mar");
	save_cf( m_cm, m_cf,  CF_TODAprEnergy, m_nyears, "cf_energy_net_apr");
	save_cf( m_cm, m_cf,  CF_TODMayEnergy, m_nyears, "cf_energy_net_may");
	save_cf( m_cm, m_cf,  CF_TODJunEnergy, m_nyears, "cf_energy_net_jun");
	save_cf( m_cm, m_cf,  CF_TODJulEnergy, m_nyears, "cf_energy_net_jul");
	save_cf( m_cm, m_cf,  CF_TODAugEnergy, m_nyears, "cf_energy_net_aug");
	save_cf( m_cm, m_cf,  CF_TODSepEnergy, m_nyears, "cf_energy_net_sep");
	save_cf( m_cm, m_cf,  CF_TODOctEnergy, m_nyears, "cf_energy_net_oct");
	save_cf( m_cm, m_cf,  CF_TODNovEnergy, m_nyears, "cf_energy_net_nov");
	save_cf( m_cm, m_cf,  CF_TODDecEnergy, m_nyears, "cf_energy_net_dec");

	save_cf( m_cm, m_cf,  CF_TOD1Energy, m_nyears, "cf_energy_net_dispatch1");
	save_cf( m_cm, m_cf,  CF_TOD2Energy, m_nyears, "cf_energy_net_dispatch2");
	save_cf( m_cm, m_cf,  CF_TOD3Energy, m_nyears, "cf_energy_net_dispatch3");
	save_cf( m_cm, m_cf,  CF_TOD4Energy, m_nyears, "cf_energy_net_dispatch4");
	save_cf( m_cm, m_cf,  CF_TOD5Energy, m_nyears, "cf_energy_net_dispatch5");
	save_cf( m_cm, m_cf,  CF_TOD6Energy, m_nyears, "cf_energy_net_dispatch6");
	save_cf( m_cm, m_cf,  CF_TOD7Energy, m_nyears, "cf_energy_net_dispatch7");
	save_cf( m_cm, m_cf,  CF_TOD8Energy, m_nyears, "cf_energy_net_dispatch8");
	save_cf( m_cm, m_cf,  CF_TOD9Energy, m_nyears, "cf_energy_net_dispatch9");

	// dispatch revenue cents/kWh ppa input in cents per kWh - revenue in dollars
	for (i = 0; i <= (size_t)m_nyears; i++)
	{
		m_cf.at(CF_TOD1Revenue, i) = ppa[i] / 100.0 * dispatch_factor1 * m_cf.at(CF_TOD1Energy, i);
		m_cf.at(CF_TOD2Revenue, i) = ppa[i] / 100.0 * dispatch_factor2 * m_cf.at(CF_TOD2Energy, i);
		m_cf.at(CF_TOD3Revenue, i) = ppa[i] / 100.0 * dispatch_factor3 * m_cf.at(CF_TOD3Energy, i);
		m_cf.at(CF_TOD4Revenue, i) = ppa[i] / 100.0 * dispatch_factor4 * m_cf.at(CF_TOD4Energy, i);
		m_cf.at(CF_TOD5Revenue, i) = ppa[i] / 100.0 * dispatch_factor5 *m_cf.at(CF_TOD5Energy, i);
		m_cf.at(CF_TOD6Revenue, i) = ppa[i] / 100.0 * dispatch_factor6 * m_cf.at(CF_TOD6Energy, i);
		m_cf.at(CF_TOD7Revenue, i) = ppa[i] / 100.0 * dispatch_factor7 * m_cf.at(CF_TOD7Energy, i);
		m_cf.at(CF_TOD8Revenue, i) = ppa[i] / 100.0 * dispatch_factor8 * m_cf.at(CF_TOD8Energy, i);
		m_cf.at(CF_TOD9Revenue, i) = ppa[i] / 100.0 * dispatch_factor9 * m_cf.at(CF_TOD9Energy, i);
	}

	save_cf( m_cm, m_cf,  CF_TOD1Revenue, m_nyears, "cf_revenue_dispatch1");
	save_cf( m_cm, m_cf,  CF_TOD2Revenue, m_nyears, "cf_revenue_dispatch2");
	save_cf( m_cm, m_cf,  CF_TOD3Revenue, m_nyears, "cf_revenue_dispatch3");
	save_cf( m_cm, m_cf,  CF_TOD4Revenue, m_nyears, "cf_revenue_dispatch4");
	save_cf( m_cm, m_cf,  CF_TOD5Revenue, m_nyears, "cf_revenue_dispatch5");
	save_cf( m_cm, m_cf,  CF_TOD6Revenue, m_nyears, "cf_revenue_dispatch6");
	save_cf( m_cm, m_cf,  CF_TOD7Revenue, m_nyears, "cf_revenue_dispatch7");
	save_cf( m_cm, m_cf,  CF_TOD8Revenue, m_nyears, "cf_revenue_dispatch8");
	save_cf( m_cm, m_cf,  CF_TOD9Revenue, m_nyears, "cf_revenue_dispatch9");

	for (i = 0; i <= (size_t)m_nyears; i++)
	{
		m_cf.at(CF_TODJanRevenue, i) = ppa[i] / 100.0 * (
			dispatch_factor1 * m_cf.at(CF_TOD1JanEnergy, i) +
			dispatch_factor2 * m_cf.at(CF_TOD2JanEnergy, i) +
			dispatch_factor3 * m_cf.at(CF_TOD3JanEnergy, i) +
			dispatch_factor4 * m_cf.at(CF_TOD4JanEnergy, i) +
			dispatch_factor5 * m_cf.at(CF_TOD5JanEnergy, i) +
			dispatch_factor6 * m_cf.at(CF_TOD6JanEnergy, i) +
			dispatch_factor7 * m_cf.at(CF_TOD7JanEnergy, i) +
			dispatch_factor8 * m_cf.at(CF_TOD8JanEnergy, i) +
			dispatch_factor9 * m_cf.at(CF_TOD9JanEnergy, i));
	}
	save_cf( m_cm, m_cf,  CF_TODJanRevenue, m_nyears, "cf_revenue_jan");

	for (i = 0; i <= (size_t)m_nyears; i++)
	{
		m_cf.at(CF_TODFebRevenue, i) = ppa[i] / 100.0 * (
			dispatch_factor1 * m_cf.at(CF_TOD1FebEnergy, i) +
			dispatch_factor2 * m_cf.at(CF_TOD2FebEnergy, i) +
			dispatch_factor3 * m_cf.at(CF_TOD3FebEnergy, i) +
			dispatch_factor4 * m_cf.at(CF_TOD4FebEnergy, i) +
			dispatch_factor5 * m_cf.at(CF_TOD5FebEnergy, i) +
			dispatch_factor6 * m_cf.at(CF_TOD6FebEnergy, i) +
			dispatch_factor7 * m_cf.at(CF_TOD7FebEnergy, i) +
			dispatch_factor8 * m_cf.at(CF_TOD8FebEnergy, i) +
			dispatch_factor9 * m_cf.at(CF_TOD9FebEnergy, i));
	}
	save_cf( m_cm, m_cf,  CF_TODFebRevenue, m_nyears, "cf_revenue_feb");

	for (i = 0; i <= (size_t)m_nyears; i++)
	{
		m_cf.at(CF_TODMarRevenue, i) = ppa[i] / 100.0 * (
			dispatch_factor1 * m_cf.at(CF_TOD1MarEnergy, i) +
			dispatch_factor2 * m_cf.at(CF_TOD2MarEnergy, i) +
			dispatch_factor3 * m_cf.at(CF_TOD3MarEnergy, i) +
			dispatch_factor4 * m_cf.at(CF_TOD4MarEnergy, i) +
			dispatch_factor5 * m_cf.at(CF_TOD5MarEnergy, i) +
			dispatch_factor6 * m_cf.at(CF_TOD6MarEnergy, i) +
			dispatch_factor7 * m_cf.at(CF_TOD7MarEnergy, i) +
			dispatch_factor8 * m_cf.at(CF_TOD8MarEnergy, i) +
			dispatch_factor9 * m_cf.at(CF_TOD9MarEnergy, i));
	}
	save_cf( m_cm, m_cf,  CF_TODMarRevenue, m_nyears, "cf_revenue_mar");

	for (i = 0; i <= (size_t)m_nyears; i++)
	{
		m_cf.at(CF_TODAprRevenue, i) = ppa[i] / 100.0 * (
			dispatch_factor1 * m_cf.at(CF_TOD1AprEnergy, i) +
			dispatch_factor2 * m_cf.at(CF_TOD2AprEnergy, i) +
			dispatch_factor3 * m_cf.at(CF_TOD3AprEnergy, i) +
			dispatch_factor4 * m_cf.at(CF_TOD4AprEnergy, i) +
			dispatch_factor5 * m_cf.at(CF_TOD5AprEnergy, i) +
			dispatch_factor6 * m_cf.at(CF_TOD6AprEnergy, i) +
			dispatch_factor7 * m_cf.at(CF_TOD7AprEnergy, i) +
			dispatch_factor8 * m_cf.at(CF_TOD8AprEnergy, i) +
			dispatch_factor9 * m_cf.at(CF_TOD9AprEnergy, i));
	}
	save_cf( m_cm, m_cf,  CF_TODAprRevenue, m_nyears, "cf_revenue_apr");

	for (i = 0; i <= (size_t)m_nyears; i++)
	{
		m_cf.at(CF_TODMayRevenue, i) = ppa[i] / 100.0 * (
			dispatch_factor1 * m_cf.at(CF_TOD1MayEnergy, i) +
			dispatch_factor2 * m_cf.at(CF_TOD2MayEnergy, i) +
			dispatch_factor3 * m_cf.at(CF_TOD3MayEnergy, i) +
			dispatch_factor4 * m_cf.at(CF_TOD4MayEnergy, i) +
			dispatch_factor5 * m_cf.at(CF_TOD5MayEnergy, i) +
			dispatch_factor6 * m_cf.at(CF_TOD6MayEnergy, i) +
			dispatch_factor7 * m_cf.at(CF_TOD7MayEnergy, i) +
			dispatch_factor8 * m_cf.at(CF_TOD8MayEnergy, i) +
			dispatch_factor9 * m_cf.at(CF_TOD9MayEnergy, i));
	}
	save_cf( m_cm, m_cf,  CF_TODMayRevenue, m_nyears, "cf_revenue_may");

	for (i = 0; i <= (size_t)m_nyears; i++)
	{
		m_cf.at(CF_TODJunRevenue, i) = ppa[i] / 100.0 * (
			dispatch_factor1 * m_cf.at(CF_TOD1JunEnergy, i) +
			dispatch_factor2 * m_cf.at(CF_TOD2JunEnergy, i) +
			dispatch_factor3 * m_cf.at(CF_TOD3JunEnergy, i) +
			dispatch_factor4 * m_cf.at(CF_TOD4JunEnergy, i) +
			dispatch_factor5 * m_cf.at(CF_TOD5JunEnergy, i) +
			dispatch_factor6 * m_cf.at(CF_TOD6JunEnergy, i) +
			dispatch_factor7 * m_cf.at(CF_TOD7JunEnergy, i) +
			dispatch_factor8 * m_cf.at(CF_TOD8JunEnergy, i) +
			dispatch_factor9 * m_cf.at(CF_TOD9JunEnergy, i));
	}
	save_cf( m_cm, m_cf,  CF_TODJunRevenue, m_nyears, "cf_revenue_jun");

	for (i = 0; i <= (size_t)m_nyears; i++)
	{
		m_cf.at(CF_TODJulRevenue, i) = ppa[i] / 100.0 * (
			dispatch_factor1 * m_cf.at(CF_TOD1JulEnergy, i) +
			dispatch_factor2 * m_cf.at(CF_TOD2JulEnergy, i) +
			dispatch_factor3 * m_cf.at(CF_TOD3JulEnergy, i) +
			dispatch_factor4 * m_cf.at(CF_TOD4JulEnergy, i) +
			dispatch_factor5 * m_cf.at(CF_TOD5JulEnergy, i) +
			dispatch_factor6 * m_cf.at(CF_TOD6JulEnergy, i) +
			dispatch_factor7 * m_cf.at(CF_TOD7JulEnergy, i) +
			dispatch_factor8 * m_cf.at(CF_TOD8JulEnergy, i) +
			dispatch_factor9 * m_cf.at(CF_TOD9JulEnergy, i));
	}
	save_cf( m_cm, m_cf,  CF_TODJulRevenue, m_nyears, "cf_revenue_jul");

	for (i = 0; i <= (size_t)m_nyears; i++)
	{
		m_cf.at(CF_TODAugRevenue, i) = ppa[i] / 100.0 * (
			dispatch_factor1 * m_cf.at(CF_TOD1AugEnergy, i) +
			dispatch_factor2 * m_cf.at(CF_TOD2AugEnergy, i) +
			dispatch_factor3 * m_cf.at(CF_TOD3AugEnergy, i) +
			dispatch_factor4 * m_cf.at(CF_TOD4AugEnergy, i) +
			dispatch_factor5 * m_cf.at(CF_TOD5AugEnergy, i) +
			dispatch_factor6 * m_cf.at(CF_TOD6AugEnergy, i) +
			dispatch_factor7 * m_cf.at(CF_TOD7AugEnergy, i) +
			dispatch_factor8 * m_cf.at(CF_TOD8AugEnergy, i) +
			dispatch_factor9 * m_cf.at(CF_TOD9AugEnergy, i));
	}
	save_cf( m_cm, m_cf,  CF_TODAugRevenue, m_nyears, "cf_revenue_aug");

	for (i = 0; i <= (size_t)m_nyears; i++)
	{
		m_cf.at(CF_TODSepRevenue, i) = ppa[i] / 100.0 * (
			dispatch_factor1 * m_cf.at(CF_TOD1SepEnergy, i) +
			dispatch_factor2 * m_cf.at(CF_TOD2SepEnergy, i) +
			dispatch_factor3 * m_cf.at(CF_TOD3SepEnergy, i) +
			dispatch_factor4 * m_cf.at(CF_TOD4SepEnergy, i) +
			dispatch_factor5 * m_cf.at(CF_TOD5SepEnergy, i) +
			dispatch_factor6 * m_cf.at(CF_TOD6SepEnergy, i) +
			dispatch_factor7 * m_cf.at(CF_TOD7SepEnergy, i) +
			dispatch_factor8 * m_cf.at(CF_TOD8SepEnergy, i) +
			dispatch_factor9 * m_cf.at(CF_TOD9SepEnergy, i));
	}
	save_cf( m_cm, m_cf,  CF_TODSepRevenue, m_nyears, "cf_revenue_sep");

	for (i = 0; i <= (size_t)m_nyears; i++)
	{
		m_cf.at(CF_TODOctRevenue, i) = ppa[i] / 100.0 * (
			dispatch_factor1 * m_cf.at(CF_TOD1OctEnergy, i) +
			dispatch_factor2 * m_cf.at(CF_TOD2OctEnergy, i) +
			dispatch_factor3 * m_cf.at(CF_TOD3OctEnergy, i) +
			dispatch_factor4 * m_cf.at(CF_TOD4OctEnergy, i) +
			dispatch_factor5 * m_cf.at(CF_TOD5OctEnergy, i) +
			dispatch_factor6 * m_cf.at(CF_TOD6OctEnergy, i) +
			dispatch_factor7 * m_cf.at(CF_TOD7OctEnergy, i) +
			dispatch_factor8 * m_cf.at(CF_TOD8OctEnergy, i) +
			dispatch_factor9 * m_cf.at(CF_TOD9OctEnergy, i));
	}
	save_cf( m_cm, m_cf,  CF_TODOctRevenue, m_nyears, "cf_revenue_oct");

	for (i = 0; i <= (size_t)m_nyears; i++)
	{
		m_cf.at(CF_TODNovRevenue, i) = ppa[i] / 100.0 * (
			dispatch_factor1 * m_cf.at(CF_TOD1NovEnergy, i) +
			dispatch_factor2 * m_cf.at(CF_TOD2NovEnergy, i) +
			dispatch_factor3 * m_cf.at(CF_TOD3NovEnergy, i) +
			dispatch_factor4 * m_cf.at(CF_TOD4NovEnergy, i) +
			dispatch_factor5 * m_cf.at(CF_TOD5NovEnergy, i) +
			dispatch_factor6 * m_cf.at(CF_TOD6NovEnergy, i) +
			dispatch_factor7 * m_cf.at(CF_TOD7NovEnergy, i) +
			dispatch_factor8 * m_cf.at(CF_TOD8NovEnergy, i) +
			dispatch_factor9 * m_cf.at(CF_TOD9NovEnergy, i));
	}
	save_cf( m_cm, m_cf,  CF_TODNovRevenue, m_nyears, "cf_revenue_nov");

	for (i = 0; i <= (size_t)m_nyears; i++)
	{
		m_cf.at(CF_TODDecRevenue, i) = ppa[i] / 100.0 * (
			dispatch_factor1 * m_cf.at(CF_TOD1DecEnergy, i) +
			dispatch_factor2 * m_cf.at(CF_TOD2DecEnergy, i) +
			dispatch_factor3 * m_cf.at(CF_TOD3DecEnergy, i) +
			dispatch_factor4 * m_cf.at(CF_TOD4DecEnergy, i) +
			dispatch_factor5 * m_cf.at(CF_TOD5DecEnergy, i) +
			dispatch_factor6 * m_cf.at(CF_TOD6DecEnergy, i) +
			dispatch_factor7 * m_cf.at(CF_TOD7DecEnergy, i) +
			dispatch_factor8 * m_cf.at(CF_TOD8DecEnergy, i) +
			dispatch_factor9 * m_cf.at(CF_TOD9DecEnergy, i));
	}
	save_cf( m_cm, m_cf,  CF_TODDecRevenue, m_nyears, "cf_revenue_Dec");

	/*
	m_cf.at(CF_revenue_monthly_firstyear, 0) = m_cf.at(CF_TODJanRevenue, 1);
	m_cf.at(CF_revenue_monthly_firstyear, 1) = m_cf.at(CF_TODFebRevenue, 1);
	m_cf.at(CF_revenue_monthly_firstyear, 2) = m_cf.at(CF_TODMarRevenue, 1);
	m_cf.at(CF_revenue_monthly_firstyear, 3) = m_cf.at(CF_TODAprRevenue, 1);
	m_cf.at(CF_revenue_monthly_firstyear, 4) = m_cf.at(CF_TODMayRevenue, 1);
	m_cf.at(CF_revenue_monthly_firstyear, 5) = m_cf.at(CF_TODJunRevenue, 1);
	m_cf.at(CF_revenue_monthly_firstyear, 6) = m_cf.at(CF_TODJulRevenue, 1);
	m_cf.at(CF_revenue_monthly_firstyear, 7) = m_cf.at(CF_TODAugRevenue, 1);
	m_cf.at(CF_revenue_monthly_firstyear, 8) = m_cf.at(CF_TODSepRevenue, 1);
	m_cf.at(CF_revenue_monthly_firstyear, 9) = m_cf.at(CF_TODOctRevenue, 1);
	m_cf.at(CF_revenue_monthly_firstyear, 10) = m_cf.at(CF_TODNovRevenue, 1);
	m_cf.at(CF_revenue_monthly_firstyear, 11) = m_cf.at(CF_TODDecRevenue, 1);

	m_cf.at(CF_energy_net_monthly_firstyear, 0) = m_cf.at(CF_TODJanEnergy, 1);
	m_cf.at(CF_energy_net_monthly_firstyear, 1) = m_cf.at(CF_TODFebEnergy, 1);
	m_cf.at(CF_energy_net_monthly_firstyear, 2) = m_cf.at(CF_TODMarEnergy, 1);
	m_cf.at(CF_energy_net_monthly_firstyear, 3) = m_cf.at(CF_TODAprEnergy, 1);
	m_cf.at(CF_energy_net_monthly_firstyear, 4) = m_cf.at(CF_TODMayEnergy, 1);
	m_cf.at(CF_energy_net_monthly_firstyear, 5) = m_cf.at(CF_TODJunEnergy, 1);
	m_cf.at(CF_energy_net_monthly_firstyear, 6) = m_cf.at(CF_TODJulEnergy, 1);
	m_cf.at(CF_energy_net_monthly_firstyear, 7) = m_cf.at(CF_TODAugEnergy, 1);
	m_cf.at(CF_energy_net_monthly_firstyear, 8) = m_cf.at(CF_TODSepEnergy, 1);
	m_cf.at(CF_energy_net_monthly_firstyear, 9) = m_cf.at(CF_TODOctEnergy, 1);
	m_cf.at(CF_energy_net_monthly_firstyear, 10) = m_cf.at(CF_TODNovEnergy, 1);
	m_cf.at(CF_energy_net_monthly_firstyear, 11) = m_cf.at(CF_TODDecEnergy, 1);
	
	save_cf( m_cm, m_cf,  CF_revenue_monthly_firstyear, 11, "cf_revenue_monthly_firstyear");
	save_cf( m_cm, m_cf,  CF_energy_net_monthly_firstyear, 11, "cf_energy_net_monthly_firstyear");
	*/


	m_cf.at(CF_revenue_monthly_firstyear_TOD1, 0) = ppa[1] / 100.0 *
		dispatch_factor1 * m_cf.at(CF_TOD1JanEnergy, 1);
	m_cf.at(CF_revenue_monthly_firstyear_TOD1, 1) = ppa[1] / 100.0 *
		dispatch_factor1 * m_cf.at(CF_TOD1FebEnergy, 1);
	m_cf.at(CF_revenue_monthly_firstyear_TOD1, 2) = ppa[1] / 100.0 *
		dispatch_factor1 * m_cf.at(CF_TOD1MarEnergy, 1);
	m_cf.at(CF_revenue_monthly_firstyear_TOD1, 3) = ppa[1] / 100.0 *
		dispatch_factor1 * m_cf.at(CF_TOD1AprEnergy, 1);
	m_cf.at(CF_revenue_monthly_firstyear_TOD1, 4) = ppa[1] / 100.0 *
		dispatch_factor1 * m_cf.at(CF_TOD1MayEnergy, 1);
	m_cf.at(CF_revenue_monthly_firstyear_TOD1, 5) = ppa[1] / 100.0 *
		dispatch_factor1 * m_cf.at(CF_TOD1JunEnergy, 1);
	m_cf.at(CF_revenue_monthly_firstyear_TOD1, 6) = ppa[1] / 100.0 *
		dispatch_factor1 * m_cf.at(CF_TOD1JulEnergy, 1);
	m_cf.at(CF_revenue_monthly_firstyear_TOD1, 7) = ppa[1] / 100.0 *
		dispatch_factor1 * m_cf.at(CF_TOD1AugEnergy, 1);
	m_cf.at(CF_revenue_monthly_firstyear_TOD1, 8) = ppa[1] / 100.0 *
		dispatch_factor1 * m_cf.at(CF_TOD1SepEnergy, 1);
	m_cf.at(CF_revenue_monthly_firstyear_TOD1, 9) = ppa[1] / 100.0 *
		dispatch_factor1 * m_cf.at(CF_TOD1OctEnergy, 1);
	m_cf.at(CF_revenue_monthly_firstyear_TOD1, 10) = ppa[1] / 100.0 *
		dispatch_factor1 * m_cf.at(CF_TOD1NovEnergy, 1);
	m_cf.at(CF_revenue_monthly_firstyear_TOD1, 11) = ppa[1] / 100.0 *
		dispatch_factor1 * m_cf.at(CF_TOD1DecEnergy, 1);

	m_cf.at(CF_energy_net_monthly_firstyear_TOD1, 0) = m_cf.at(CF_TOD1JanEnergy, 1);
	m_cf.at(CF_energy_net_monthly_firstyear_TOD1, 1) = m_cf.at(CF_TOD1FebEnergy, 1);
	m_cf.at(CF_energy_net_monthly_firstyear_TOD1, 2) = m_cf.at(CF_TOD1MarEnergy, 1);
	m_cf.at(CF_energy_net_monthly_firstyear_TOD1, 3) = m_cf.at(CF_TOD1AprEnergy, 1);
	m_cf.at(CF_energy_net_monthly_firstyear_TOD1, 4) = m_cf.at(CF_TOD1MayEnergy, 1);
	m_cf.at(CF_energy_net_monthly_firstyear_TOD1, 5) = m_cf.at(CF_TOD1JunEnergy, 1);
	m_cf.at(CF_energy_net_monthly_firstyear_TOD1, 6) = m_cf.at(CF_TOD1JulEnergy, 1);
	m_cf.at(CF_energy_net_monthly_firstyear_TOD1, 7) = m_cf.at(CF_TOD1AugEnergy, 1);
	m_cf.at(CF_energy_net_monthly_firstyear_TOD1, 8) = m_cf.at(CF_TOD1SepEnergy, 1);
	m_cf.at(CF_energy_net_monthly_firstyear_TOD1, 9) = m_cf.at(CF_TOD1OctEnergy, 1);
	m_cf.at(CF_energy_net_monthly_firstyear_TOD1, 10) = m_cf.at(CF_TOD1NovEnergy, 1);
	m_cf.at(CF_energy_net_monthly_firstyear_TOD1, 11) = m_cf.at(CF_TOD1DecEnergy, 1);

	save_cf( m_cm, m_cf,  CF_revenue_monthly_firstyear_TOD1, 11, "cf_revenue_monthly_firstyear_TOD1");
	save_cf( m_cm, m_cf,  CF_energy_net_monthly_firstyear_TOD1, 11, "cf_energy_net_monthly_firstyear_TOD1");


	m_cf.at(CF_revenue_monthly_firstyear_TOD2, 0) = ppa[1] / 100.0 *
		dispatch_factor2 * m_cf.at(CF_TOD2JanEnergy, 1);
	m_cf.at(CF_revenue_monthly_firstyear_TOD2, 1) = ppa[1] / 100.0 *
		dispatch_factor2 * m_cf.at(CF_TOD2FebEnergy, 1);
	m_cf.at(CF_revenue_monthly_firstyear_TOD2, 2) = ppa[1] / 100.0 *
		dispatch_factor2 * m_cf.at(CF_TOD2MarEnergy, 1);
	m_cf.at(CF_revenue_monthly_firstyear_TOD2, 3) = ppa[1] / 100.0 *
		dispatch_factor2 * m_cf.at(CF_TOD2AprEnergy, 1);
	m_cf.at(CF_revenue_monthly_firstyear_TOD2, 4) = ppa[1] / 100.0 *
		dispatch_factor2 * m_cf.at(CF_TOD2MayEnergy, 1);
	m_cf.at(CF_revenue_monthly_firstyear_TOD2, 5) = ppa[1] / 100.0 *
		dispatch_factor2 * m_cf.at(CF_TOD2JunEnergy, 1);
	m_cf.at(CF_revenue_monthly_firstyear_TOD2, 6) = ppa[1] / 100.0 *
		dispatch_factor2 * m_cf.at(CF_TOD2JulEnergy, 1);
	m_cf.at(CF_revenue_monthly_firstyear_TOD2, 7) = ppa[1] / 100.0 *
		dispatch_factor2 * m_cf.at(CF_TOD2AugEnergy, 1);
	m_cf.at(CF_revenue_monthly_firstyear_TOD2, 8) = ppa[1] / 100.0 *
		dispatch_factor2 * m_cf.at(CF_TOD2SepEnergy, 1);
	m_cf.at(CF_revenue_monthly_firstyear_TOD2, 9) = ppa[1] / 100.0 *
		dispatch_factor2 * m_cf.at(CF_TOD2OctEnergy, 1);
	m_cf.at(CF_revenue_monthly_firstyear_TOD2, 10) = ppa[1] / 100.0 *
		dispatch_factor2 * m_cf.at(CF_TOD2NovEnergy, 1);
	m_cf.at(CF_revenue_monthly_firstyear_TOD2, 11) = ppa[1] / 100.0 *
		dispatch_factor2 * m_cf.at(CF_TOD2DecEnergy, 1);

	m_cf.at(CF_energy_net_monthly_firstyear_TOD2, 0) = m_cf.at(CF_TOD2JanEnergy, 1);
	m_cf.at(CF_energy_net_monthly_firstyear_TOD2, 1) = m_cf.at(CF_TOD2FebEnergy, 1);
	m_cf.at(CF_energy_net_monthly_firstyear_TOD2, 2) = m_cf.at(CF_TOD2MarEnergy, 1);
	m_cf.at(CF_energy_net_monthly_firstyear_TOD2, 3) = m_cf.at(CF_TOD2AprEnergy, 1);
	m_cf.at(CF_energy_net_monthly_firstyear_TOD2, 4) = m_cf.at(CF_TOD2MayEnergy, 1);
	m_cf.at(CF_energy_net_monthly_firstyear_TOD2, 5) = m_cf.at(CF_TOD2JunEnergy, 1);
	m_cf.at(CF_energy_net_monthly_firstyear_TOD2, 6) = m_cf.at(CF_TOD2JulEnergy, 1);
	m_cf.at(CF_energy_net_monthly_firstyear_TOD2, 7) = m_cf.at(CF_TOD2AugEnergy, 1);
	m_cf.at(CF_energy_net_monthly_firstyear_TOD2, 8) = m_cf.at(CF_TOD2SepEnergy, 1);
	m_cf.at(CF_energy_net_monthly_firstyear_TOD2, 9) = m_cf.at(CF_TOD2OctEnergy, 1);
	m_cf.at(CF_energy_net_monthly_firstyear_TOD2, 10) = m_cf.at(CF_TOD2NovEnergy, 1);
	m_cf.at(CF_energy_net_monthly_firstyear_TOD2, 11) = m_cf.at(CF_TOD2DecEnergy, 1);

	save_cf( m_cm, m_cf,  CF_revenue_monthly_firstyear_TOD2, 11, "cf_revenue_monthly_firstyear_TOD2");
	save_cf( m_cm, m_cf,  CF_energy_net_monthly_firstyear_TOD2, 11, "cf_energy_net_monthly_firstyear_TOD2");


	m_cf.at(CF_revenue_monthly_firstyear_TOD3, 0) = ppa[1] / 100.0 *
		dispatch_factor3 * m_cf.at(CF_TOD3JanEnergy, 1);
	m_cf.at(CF_revenue_monthly_firstyear_TOD3, 1) = ppa[1] / 100.0 *
		dispatch_factor3 * m_cf.at(CF_TOD3FebEnergy, 1);
	m_cf.at(CF_revenue_monthly_firstyear_TOD3, 2) = ppa[1] / 100.0 *
		dispatch_factor3 * m_cf.at(CF_TOD3MarEnergy, 1);
	m_cf.at(CF_revenue_monthly_firstyear_TOD3, 3) = ppa[1] / 100.0 *
		dispatch_factor3 * m_cf.at(CF_TOD3AprEnergy, 1);
	m_cf.at(CF_revenue_monthly_firstyear_TOD3, 4) = ppa[1] / 100.0 *
		dispatch_factor3 * m_cf.at(CF_TOD3MayEnergy, 1);
	m_cf.at(CF_revenue_monthly_firstyear_TOD3, 5) = ppa[1] / 100.0 *
		dispatch_factor3 * m_cf.at(CF_TOD3JunEnergy, 1);
	m_cf.at(CF_revenue_monthly_firstyear_TOD3, 6) = ppa[1] / 100.0 *
		dispatch_factor3 * m_cf.at(CF_TOD3JulEnergy, 1);
	m_cf.at(CF_revenue_monthly_firstyear_TOD3, 7) = ppa[1] / 100.0 *
		dispatch_factor3 * m_cf.at(CF_TOD3AugEnergy, 1);
	m_cf.at(CF_revenue_monthly_firstyear_TOD3, 8) = ppa[1] / 100.0 *
		dispatch_factor3 * m_cf.at(CF_TOD3SepEnergy, 1);
	m_cf.at(CF_revenue_monthly_firstyear_TOD3, 9) = ppa[1] / 100.0 *
		dispatch_factor3 * m_cf.at(CF_TOD3OctEnergy, 1);
	m_cf.at(CF_revenue_monthly_firstyear_TOD3, 10) = ppa[1] / 100.0 *
		dispatch_factor3 * m_cf.at(CF_TOD3NovEnergy, 1);
	m_cf.at(CF_revenue_monthly_firstyear_TOD3, 11) = ppa[1] / 100.0 *
		dispatch_factor3 * m_cf.at(CF_TOD3DecEnergy, 1);

	m_cf.at(CF_energy_net_monthly_firstyear_TOD3, 0) = m_cf.at(CF_TOD3JanEnergy, 1);
	m_cf.at(CF_energy_net_monthly_firstyear_TOD3, 1) = m_cf.at(CF_TOD3FebEnergy, 1);
	m_cf.at(CF_energy_net_monthly_firstyear_TOD3, 2) = m_cf.at(CF_TOD3MarEnergy, 1);
	m_cf.at(CF_energy_net_monthly_firstyear_TOD3, 3) = m_cf.at(CF_TOD3AprEnergy, 1);
	m_cf.at(CF_energy_net_monthly_firstyear_TOD3, 4) = m_cf.at(CF_TOD3MayEnergy, 1);
	m_cf.at(CF_energy_net_monthly_firstyear_TOD3, 5) = m_cf.at(CF_TOD3JunEnergy, 1);
	m_cf.at(CF_energy_net_monthly_firstyear_TOD3, 6) = m_cf.at(CF_TOD3JulEnergy, 1);
	m_cf.at(CF_energy_net_monthly_firstyear_TOD3, 7) = m_cf.at(CF_TOD3AugEnergy, 1);
	m_cf.at(CF_energy_net_monthly_firstyear_TOD3, 8) = m_cf.at(CF_TOD3SepEnergy, 1);
	m_cf.at(CF_energy_net_monthly_firstyear_TOD3, 9) = m_cf.at(CF_TOD3OctEnergy, 1);
	m_cf.at(CF_energy_net_monthly_firstyear_TOD3, 10) = m_cf.at(CF_TOD3NovEnergy, 1);
	m_cf.at(CF_energy_net_monthly_firstyear_TOD3, 11) = m_cf.at(CF_TOD3DecEnergy, 1);

	save_cf( m_cm, m_cf,  CF_revenue_monthly_firstyear_TOD3, 11, "cf_revenue_monthly_firstyear_TOD3");
	save_cf( m_cm, m_cf,  CF_energy_net_monthly_firstyear_TOD3, 11, "cf_energy_net_monthly_firstyear_TOD3");


	m_cf.at(CF_revenue_monthly_firstyear_TOD4, 0) = ppa[1] / 100.0 *
		dispatch_factor4 * m_cf.at(CF_TOD4JanEnergy, 1);
	m_cf.at(CF_revenue_monthly_firstyear_TOD4, 1) = ppa[1] / 100.0 *
		dispatch_factor4 * m_cf.at(CF_TOD4FebEnergy, 1);
	m_cf.at(CF_revenue_monthly_firstyear_TOD4, 2) = ppa[1] / 100.0 *
		dispatch_factor4 * m_cf.at(CF_TOD4MarEnergy, 1);
	m_cf.at(CF_revenue_monthly_firstyear_TOD4, 3) = ppa[1] / 100.0 *
		dispatch_factor4 * m_cf.at(CF_TOD4AprEnergy, 1);
	m_cf.at(CF_revenue_monthly_firstyear_TOD4, 4) = ppa[1] / 100.0 *
		dispatch_factor4 * m_cf.at(CF_TOD4MayEnergy, 1);
	m_cf.at(CF_revenue_monthly_firstyear_TOD4, 5) = ppa[1] / 100.0 *
		dispatch_factor4 * m_cf.at(CF_TOD4JunEnergy, 1);
	m_cf.at(CF_revenue_monthly_firstyear_TOD4, 6) = ppa[1] / 100.0 *
		dispatch_factor4 * m_cf.at(CF_TOD4JulEnergy, 1);
	m_cf.at(CF_revenue_monthly_firstyear_TOD4, 7) = ppa[1] / 100.0 *
		dispatch_factor4 * m_cf.at(CF_TOD4AugEnergy, 1);
	m_cf.at(CF_revenue_monthly_firstyear_TOD4, 8) = ppa[1] / 100.0 *
		dispatch_factor4 * m_cf.at(CF_TOD4SepEnergy, 1);
	m_cf.at(CF_revenue_monthly_firstyear_TOD4, 9) = ppa[1] / 100.0 *
		dispatch_factor4 * m_cf.at(CF_TOD4OctEnergy, 1);
	m_cf.at(CF_revenue_monthly_firstyear_TOD4, 10) = ppa[1] / 100.0 *
		dispatch_factor4 * m_cf.at(CF_TOD4NovEnergy, 1);
	m_cf.at(CF_revenue_monthly_firstyear_TOD4, 11) = ppa[1] / 100.0 *
		dispatch_factor4 * m_cf.at(CF_TOD4DecEnergy, 1);

	m_cf.at(CF_energy_net_monthly_firstyear_TOD4, 0) = m_cf.at(CF_TOD4JanEnergy, 1);
	m_cf.at(CF_energy_net_monthly_firstyear_TOD4, 1) = m_cf.at(CF_TOD4FebEnergy, 1);
	m_cf.at(CF_energy_net_monthly_firstyear_TOD4, 2) = m_cf.at(CF_TOD4MarEnergy, 1);
	m_cf.at(CF_energy_net_monthly_firstyear_TOD4, 3) = m_cf.at(CF_TOD4AprEnergy, 1);
	m_cf.at(CF_energy_net_monthly_firstyear_TOD4, 4) = m_cf.at(CF_TOD4MayEnergy, 1);
	m_cf.at(CF_energy_net_monthly_firstyear_TOD4, 5) = m_cf.at(CF_TOD4JunEnergy, 1);
	m_cf.at(CF_energy_net_monthly_firstyear_TOD4, 6) = m_cf.at(CF_TOD4JulEnergy, 1);
	m_cf.at(CF_energy_net_monthly_firstyear_TOD4, 7) = m_cf.at(CF_TOD4AugEnergy, 1);
	m_cf.at(CF_energy_net_monthly_firstyear_TOD4, 8) = m_cf.at(CF_TOD4SepEnergy, 1);
	m_cf.at(CF_energy_net_monthly_firstyear_TOD4, 9) = m_cf.at(CF_TOD4OctEnergy, 1);
	m_cf.at(CF_energy_net_monthly_firstyear_TOD4, 10) = m_cf.at(CF_TOD4NovEnergy, 1);
	m_cf.at(CF_energy_net_monthly_firstyear_TOD4, 11) = m_cf.at(CF_TOD4DecEnergy, 1);

	save_cf( m_cm, m_cf,  CF_revenue_monthly_firstyear_TOD4, 11, "cf_revenue_monthly_firstyear_TOD4");
	save_cf( m_cm, m_cf,  CF_energy_net_monthly_firstyear_TOD4, 11, "cf_energy_net_monthly_firstyear_TOD4");


	m_cf.at(CF_revenue_monthly_firstyear_TOD5, 0) = ppa[1] / 100.0 *
		dispatch_factor5 * m_cf.at(CF_TOD5JanEnergy, 1);
	m_cf.at(CF_revenue_monthly_firstyear_TOD5, 1) = ppa[1] / 100.0 *
		dispatch_factor5 * m_cf.at(CF_TOD5FebEnergy, 1);
	m_cf.at(CF_revenue_monthly_firstyear_TOD5, 2) = ppa[1] / 100.0 *
		dispatch_factor5 * m_cf.at(CF_TOD5MarEnergy, 1);
	m_cf.at(CF_revenue_monthly_firstyear_TOD5, 3) = ppa[1] / 100.0 *
		dispatch_factor5 * m_cf.at(CF_TOD5AprEnergy, 1);
	m_cf.at(CF_revenue_monthly_firstyear_TOD5, 4) = ppa[1] / 100.0 *
		dispatch_factor5 * m_cf.at(CF_TOD5MayEnergy, 1);
	m_cf.at(CF_revenue_monthly_firstyear_TOD5, 5) = ppa[1] / 100.0 *
		dispatch_factor5 * m_cf.at(CF_TOD5JunEnergy, 1);
	m_cf.at(CF_revenue_monthly_firstyear_TOD5, 6) = ppa[1] / 100.0 *
		dispatch_factor5 * m_cf.at(CF_TOD5JulEnergy, 1);
	m_cf.at(CF_revenue_monthly_firstyear_TOD5, 7) = ppa[1] / 100.0 *
		dispatch_factor5 * m_cf.at(CF_TOD5AugEnergy, 1);
	m_cf.at(CF_revenue_monthly_firstyear_TOD5, 8) = ppa[1] / 100.0 *
		dispatch_factor5 * m_cf.at(CF_TOD5SepEnergy, 1);
	m_cf.at(CF_revenue_monthly_firstyear_TOD5, 9) = ppa[1] / 100.0 *
		dispatch_factor5 * m_cf.at(CF_TOD5OctEnergy, 1);
	m_cf.at(CF_revenue_monthly_firstyear_TOD5, 10) = ppa[1] / 100.0 *
		dispatch_factor5 * m_cf.at(CF_TOD5NovEnergy, 1);
	m_cf.at(CF_revenue_monthly_firstyear_TOD5, 11) = ppa[1] / 100.0 *
		dispatch_factor5 * m_cf.at(CF_TOD5DecEnergy, 1);

	m_cf.at(CF_energy_net_monthly_firstyear_TOD5, 0) = m_cf.at(CF_TOD5JanEnergy, 1);
	m_cf.at(CF_energy_net_monthly_firstyear_TOD5, 1) = m_cf.at(CF_TOD5FebEnergy, 1);
	m_cf.at(CF_energy_net_monthly_firstyear_TOD5, 2) = m_cf.at(CF_TOD5MarEnergy, 1);
	m_cf.at(CF_energy_net_monthly_firstyear_TOD5, 3) = m_cf.at(CF_TOD5AprEnergy, 1);
	m_cf.at(CF_energy_net_monthly_firstyear_TOD5, 4) = m_cf.at(CF_TOD5MayEnergy, 1);
	m_cf.at(CF_energy_net_monthly_firstyear_TOD5, 5) = m_cf.at(CF_TOD5JunEnergy, 1);
	m_cf.at(CF_energy_net_monthly_firstyear_TOD5, 6) = m_cf.at(CF_TOD5JulEnergy, 1);
	m_cf.at(CF_energy_net_monthly_firstyear_TOD5, 7) = m_cf.at(CF_TOD5AugEnergy, 1);
	m_cf.at(CF_energy_net_monthly_firstyear_TOD5, 8) = m_cf.at(CF_TOD5SepEnergy, 1);
	m_cf.at(CF_energy_net_monthly_firstyear_TOD5, 9) = m_cf.at(CF_TOD5OctEnergy, 1);
	m_cf.at(CF_energy_net_monthly_firstyear_TOD5, 10) = m_cf.at(CF_TOD5NovEnergy, 1);
	m_cf.at(CF_energy_net_monthly_firstyear_TOD5, 11) = m_cf.at(CF_TOD5DecEnergy, 1);

	save_cf( m_cm, m_cf,  CF_revenue_monthly_firstyear_TOD5, 11, "cf_revenue_monthly_firstyear_TOD5");
	save_cf( m_cm, m_cf,  CF_energy_net_monthly_firstyear_TOD5, 11, "cf_energy_net_monthly_firstyear_TOD5");


	m_cf.at(CF_revenue_monthly_firstyear_TOD6, 0) = ppa[1] / 100.0 *
		dispatch_factor6 * m_cf.at(CF_TOD6JanEnergy, 1);
	m_cf.at(CF_revenue_monthly_firstyear_TOD6, 1) = ppa[1] / 100.0 *
		dispatch_factor6 * m_cf.at(CF_TOD6FebEnergy, 1);
	m_cf.at(CF_revenue_monthly_firstyear_TOD6, 2) = ppa[1] / 100.0 *
		dispatch_factor6 * m_cf.at(CF_TOD6MarEnergy, 1);
	m_cf.at(CF_revenue_monthly_firstyear_TOD6, 3) = ppa[1] / 100.0 *
		dispatch_factor6 * m_cf.at(CF_TOD6AprEnergy, 1);
	m_cf.at(CF_revenue_monthly_firstyear_TOD6, 4) = ppa[1] / 100.0 *
		dispatch_factor6 * m_cf.at(CF_TOD6MayEnergy, 1);
	m_cf.at(CF_revenue_monthly_firstyear_TOD6, 5) = ppa[1] / 100.0 *
		dispatch_factor6 * m_cf.at(CF_TOD6JunEnergy, 1);
	m_cf.at(CF_revenue_monthly_firstyear_TOD6, 6) = ppa[1] / 100.0 *
		dispatch_factor6 * m_cf.at(CF_TOD6JulEnergy, 1);
	m_cf.at(CF_revenue_monthly_firstyear_TOD6, 7) = ppa[1] / 100.0 *
		dispatch_factor6 * m_cf.at(CF_TOD6AugEnergy, 1);
	m_cf.at(CF_revenue_monthly_firstyear_TOD6, 8) = ppa[1] / 100.0 *
		dispatch_factor6 * m_cf.at(CF_TOD6SepEnergy, 1);
	m_cf.at(CF_revenue_monthly_firstyear_TOD6, 9) = ppa[1] / 100.0 *
		dispatch_factor6 * m_cf.at(CF_TOD6OctEnergy, 1);
	m_cf.at(CF_revenue_monthly_firstyear_TOD6, 10) = ppa[1] / 100.0 *
		dispatch_factor6 * m_cf.at(CF_TOD6NovEnergy, 1);
	m_cf.at(CF_revenue_monthly_firstyear_TOD6, 11) = ppa[1] / 100.0 *
		dispatch_factor6 * m_cf.at(CF_TOD6DecEnergy, 1);

	m_cf.at(CF_energy_net_monthly_firstyear_TOD6, 0) = m_cf.at(CF_TOD6JanEnergy, 1);
	m_cf.at(CF_energy_net_monthly_firstyear_TOD6, 1) = m_cf.at(CF_TOD6FebEnergy, 1);
	m_cf.at(CF_energy_net_monthly_firstyear_TOD6, 2) = m_cf.at(CF_TOD6MarEnergy, 1);
	m_cf.at(CF_energy_net_monthly_firstyear_TOD6, 3) = m_cf.at(CF_TOD6AprEnergy, 1);
	m_cf.at(CF_energy_net_monthly_firstyear_TOD6, 4) = m_cf.at(CF_TOD6MayEnergy, 1);
	m_cf.at(CF_energy_net_monthly_firstyear_TOD6, 5) = m_cf.at(CF_TOD6JunEnergy, 1);
	m_cf.at(CF_energy_net_monthly_firstyear_TOD6, 6) = m_cf.at(CF_TOD6JulEnergy, 1);
	m_cf.at(CF_energy_net_monthly_firstyear_TOD6, 7) = m_cf.at(CF_TOD6AugEnergy, 1);
	m_cf.at(CF_energy_net_monthly_firstyear_TOD6, 8) = m_cf.at(CF_TOD6SepEnergy, 1);
	m_cf.at(CF_energy_net_monthly_firstyear_TOD6, 9) = m_cf.at(CF_TOD6OctEnergy, 1);
	m_cf.at(CF_energy_net_monthly_firstyear_TOD6, 10) = m_cf.at(CF_TOD6NovEnergy, 1);
	m_cf.at(CF_energy_net_monthly_firstyear_TOD6, 11) = m_cf.at(CF_TOD6DecEnergy, 1);

	save_cf( m_cm, m_cf,  CF_revenue_monthly_firstyear_TOD6, 11, "cf_revenue_monthly_firstyear_TOD6");
	save_cf( m_cm, m_cf,  CF_energy_net_monthly_firstyear_TOD6, 11, "cf_energy_net_monthly_firstyear_TOD6");


	m_cf.at(CF_revenue_monthly_firstyear_TOD7, 0) = ppa[1] / 100.0 *
		dispatch_factor7 * m_cf.at(CF_TOD7JanEnergy, 1);
	m_cf.at(CF_revenue_monthly_firstyear_TOD7, 1) = ppa[1] / 100.0 *
		dispatch_factor7 * m_cf.at(CF_TOD7FebEnergy, 1);
	m_cf.at(CF_revenue_monthly_firstyear_TOD7, 2) = ppa[1] / 100.0 *
		dispatch_factor7 * m_cf.at(CF_TOD7MarEnergy, 1);
	m_cf.at(CF_revenue_monthly_firstyear_TOD7, 3) = ppa[1] / 100.0 *
		dispatch_factor7 * m_cf.at(CF_TOD7AprEnergy, 1);
	m_cf.at(CF_revenue_monthly_firstyear_TOD7, 4) = ppa[1] / 100.0 *
		dispatch_factor7 * m_cf.at(CF_TOD7MayEnergy, 1);
	m_cf.at(CF_revenue_monthly_firstyear_TOD7, 5) = ppa[1] / 100.0 *
		dispatch_factor7 * m_cf.at(CF_TOD7JunEnergy, 1);
	m_cf.at(CF_revenue_monthly_firstyear_TOD7, 6) = ppa[1] / 100.0 *
		dispatch_factor7 * m_cf.at(CF_TOD7JulEnergy, 1);
	m_cf.at(CF_revenue_monthly_firstyear_TOD7, 7) = ppa[1] / 100.0 *
		dispatch_factor7 * m_cf.at(CF_TOD7AugEnergy, 1);
	m_cf.at(CF_revenue_monthly_firstyear_TOD7, 8) = ppa[1] / 100.0 *
		dispatch_factor7 * m_cf.at(CF_TOD7SepEnergy, 1);
	m_cf.at(CF_revenue_monthly_firstyear_TOD7, 9) = ppa[1] / 100.0 *
		dispatch_factor7 * m_cf.at(CF_TOD7OctEnergy, 1);
	m_cf.at(CF_revenue_monthly_firstyear_TOD7, 10) = ppa[1] / 100.0 *
		dispatch_factor7 * m_cf.at(CF_TOD7NovEnergy, 1);
	m_cf.at(CF_revenue_monthly_firstyear_TOD7, 11) = ppa[1] / 100.0 *
		dispatch_factor7 * m_cf.at(CF_TOD7DecEnergy, 1);

	m_cf.at(CF_energy_net_monthly_firstyear_TOD7, 0) = m_cf.at(CF_TOD7JanEnergy, 1);
	m_cf.at(CF_energy_net_monthly_firstyear_TOD7, 1) = m_cf.at(CF_TOD7FebEnergy, 1);
	m_cf.at(CF_energy_net_monthly_firstyear_TOD7, 2) = m_cf.at(CF_TOD7MarEnergy, 1);
	m_cf.at(CF_energy_net_monthly_firstyear_TOD7, 3) = m_cf.at(CF_TOD7AprEnergy, 1);
	m_cf.at(CF_energy_net_monthly_firstyear_TOD7, 4) = m_cf.at(CF_TOD7MayEnergy, 1);
	m_cf.at(CF_energy_net_monthly_firstyear_TOD7, 5) = m_cf.at(CF_TOD7JunEnergy, 1);
	m_cf.at(CF_energy_net_monthly_firstyear_TOD7, 6) = m_cf.at(CF_TOD7JulEnergy, 1);
	m_cf.at(CF_energy_net_monthly_firstyear_TOD7, 7) = m_cf.at(CF_TOD7AugEnergy, 1);
	m_cf.at(CF_energy_net_monthly_firstyear_TOD7, 8) = m_cf.at(CF_TOD7SepEnergy, 1);
	m_cf.at(CF_energy_net_monthly_firstyear_TOD7, 9) = m_cf.at(CF_TOD7OctEnergy, 1);
	m_cf.at(CF_energy_net_monthly_firstyear_TOD7, 10) = m_cf.at(CF_TOD7NovEnergy, 1);
	m_cf.at(CF_energy_net_monthly_firstyear_TOD7, 11) = m_cf.at(CF_TOD7DecEnergy, 1);

	save_cf( m_cm, m_cf,  CF_revenue_monthly_firstyear_TOD7, 11, "cf_revenue_monthly_firstyear_TOD7");
	save_cf( m_cm, m_cf,  CF_energy_net_monthly_firstyear_TOD7, 11, "cf_energy_net_monthly_firstyear_TOD7");


	m_cf.at(CF_revenue_monthly_firstyear_TOD8, 0) = ppa[1] / 100.0 *
		dispatch_factor8 * m_cf.at(CF_TOD8JanEnergy, 1);
	m_cf.at(CF_revenue_monthly_firstyear_TOD8, 1) = ppa[1] / 100.0 *
		dispatch_factor8 * m_cf.at(CF_TOD8FebEnergy, 1);
	m_cf.at(CF_revenue_monthly_firstyear_TOD8, 2) = ppa[1] / 100.0 *
		dispatch_factor8 * m_cf.at(CF_TOD8MarEnergy, 1);
	m_cf.at(CF_revenue_monthly_firstyear_TOD8, 3) = ppa[1] / 100.0 *
		dispatch_factor8 * m_cf.at(CF_TOD8AprEnergy, 1);
	m_cf.at(CF_revenue_monthly_firstyear_TOD8, 4) = ppa[1] / 100.0 *
		dispatch_factor8 * m_cf.at(CF_TOD8MayEnergy, 1);
	m_cf.at(CF_revenue_monthly_firstyear_TOD8, 5) = ppa[1] / 100.0 *
		dispatch_factor8 * m_cf.at(CF_TOD8JunEnergy, 1);
	m_cf.at(CF_revenue_monthly_firstyear_TOD8, 6) = ppa[1] / 100.0 *
		dispatch_factor8 * m_cf.at(CF_TOD8JulEnergy, 1);
	m_cf.at(CF_revenue_monthly_firstyear_TOD8, 7) = ppa[1] / 100.0 *
		dispatch_factor8 * m_cf.at(CF_TOD8AugEnergy, 1);
	m_cf.at(CF_revenue_monthly_firstyear_TOD8, 8) = ppa[1] / 100.0 *
		dispatch_factor8 * m_cf.at(CF_TOD8SepEnergy, 1);
	m_cf.at(CF_revenue_monthly_firstyear_TOD8, 9) = ppa[1] / 100.0 *
		dispatch_factor8 * m_cf.at(CF_TOD8OctEnergy, 1);
	m_cf.at(CF_revenue_monthly_firstyear_TOD8, 10) = ppa[1] / 100.0 *
		dispatch_factor8 * m_cf.at(CF_TOD8NovEnergy, 1);
	m_cf.at(CF_revenue_monthly_firstyear_TOD8, 11) = ppa[1] / 100.0 *
		dispatch_factor8 * m_cf.at(CF_TOD8DecEnergy, 1);

	m_cf.at(CF_energy_net_monthly_firstyear_TOD8, 0) = m_cf.at(CF_TOD8JanEnergy, 1);
	m_cf.at(CF_energy_net_monthly_firstyear_TOD8, 1) = m_cf.at(CF_TOD8FebEnergy, 1);
	m_cf.at(CF_energy_net_monthly_firstyear_TOD8, 2) = m_cf.at(CF_TOD8MarEnergy, 1);
	m_cf.at(CF_energy_net_monthly_firstyear_TOD8, 3) = m_cf.at(CF_TOD8AprEnergy, 1);
	m_cf.at(CF_energy_net_monthly_firstyear_TOD8, 4) = m_cf.at(CF_TOD8MayEnergy, 1);
	m_cf.at(CF_energy_net_monthly_firstyear_TOD8, 5) = m_cf.at(CF_TOD8JunEnergy, 1);
	m_cf.at(CF_energy_net_monthly_firstyear_TOD8, 6) = m_cf.at(CF_TOD8JulEnergy, 1);
	m_cf.at(CF_energy_net_monthly_firstyear_TOD8, 7) = m_cf.at(CF_TOD8AugEnergy, 1);
	m_cf.at(CF_energy_net_monthly_firstyear_TOD8, 8) = m_cf.at(CF_TOD8SepEnergy, 1);
	m_cf.at(CF_energy_net_monthly_firstyear_TOD8, 9) = m_cf.at(CF_TOD8OctEnergy, 1);
	m_cf.at(CF_energy_net_monthly_firstyear_TOD8, 10) = m_cf.at(CF_TOD8NovEnergy, 1);
	m_cf.at(CF_energy_net_monthly_firstyear_TOD8, 11) = m_cf.at(CF_TOD8DecEnergy, 1);

	save_cf( m_cm, m_cf,  CF_revenue_monthly_firstyear_TOD8, 11, "cf_revenue_monthly_firstyear_TOD8");
	save_cf( m_cm, m_cf,  CF_energy_net_monthly_firstyear_TOD8, 11, "cf_energy_net_monthly_firstyear_TOD8");


	m_cf.at(CF_revenue_monthly_firstyear_TOD9, 0) = ppa[1] / 100.0 *
		dispatch_factor9 * m_cf.at(CF_TOD9JanEnergy, 1);
	m_cf.at(CF_revenue_monthly_firstyear_TOD9, 1) = ppa[1] / 100.0 *
		dispatch_factor9 * m_cf.at(CF_TOD9FebEnergy, 1);
	m_cf.at(CF_revenue_monthly_firstyear_TOD9, 2) = ppa[1] / 100.0 *
		dispatch_factor9 * m_cf.at(CF_TOD9MarEnergy, 1);
	m_cf.at(CF_revenue_monthly_firstyear_TOD9, 3) = ppa[1] / 100.0 *
		dispatch_factor9 * m_cf.at(CF_TOD9AprEnergy, 1);
	m_cf.at(CF_revenue_monthly_firstyear_TOD9, 4) = ppa[1] / 100.0 *
		dispatch_factor9 * m_cf.at(CF_TOD9MayEnergy, 1);
	m_cf.at(CF_revenue_monthly_firstyear_TOD9, 5) = ppa[1] / 100.0 *
		dispatch_factor9 * m_cf.at(CF_TOD9JunEnergy, 1);
	m_cf.at(CF_revenue_monthly_firstyear_TOD9, 6) = ppa[1] / 100.0 *
		dispatch_factor9 * m_cf.at(CF_TOD9JulEnergy, 1);
	m_cf.at(CF_revenue_monthly_firstyear_TOD9, 7) = ppa[1] / 100.0 *
		dispatch_factor9 * m_cf.at(CF_TOD9AugEnergy, 1);
	m_cf.at(CF_revenue_monthly_firstyear_TOD9, 8) = ppa[1] / 100.0 *
		dispatch_factor9 * m_cf.at(CF_TOD9SepEnergy, 1);
	m_cf.at(CF_revenue_monthly_firstyear_TOD9, 9) = ppa[1] / 100.0 *
		dispatch_factor9 * m_cf.at(CF_TOD9OctEnergy, 1);
	m_cf.at(CF_revenue_monthly_firstyear_TOD9, 10) = ppa[1] / 100.0 *
		dispatch_factor9 * m_cf.at(CF_TOD9NovEnergy, 1);
	m_cf.at(CF_revenue_monthly_firstyear_TOD9, 11) = ppa[1] / 100.0 *
		dispatch_factor9 * m_cf.at(CF_TOD9DecEnergy, 1);

	m_cf.at(CF_energy_net_monthly_firstyear_TOD9, 0) = m_cf.at(CF_TOD9JanEnergy, 1);
	m_cf.at(CF_energy_net_monthly_firstyear_TOD9, 1) = m_cf.at(CF_TOD9FebEnergy, 1);
	m_cf.at(CF_energy_net_monthly_firstyear_TOD9, 2) = m_cf.at(CF_TOD9MarEnergy, 1);
	m_cf.at(CF_energy_net_monthly_firstyear_TOD9, 3) = m_cf.at(CF_TOD9AprEnergy, 1);
	m_cf.at(CF_energy_net_monthly_firstyear_TOD9, 4) = m_cf.at(CF_TOD9MayEnergy, 1);
	m_cf.at(CF_energy_net_monthly_firstyear_TOD9, 5) = m_cf.at(CF_TOD9JunEnergy, 1);
	m_cf.at(CF_energy_net_monthly_firstyear_TOD9, 6) = m_cf.at(CF_TOD9JulEnergy, 1);
	m_cf.at(CF_energy_net_monthly_firstyear_TOD9, 7) = m_cf.at(CF_TOD9AugEnergy, 1);
	m_cf.at(CF_energy_net_monthly_firstyear_TOD9, 8) = m_cf.at(CF_TOD9SepEnergy, 1);
	m_cf.at(CF_energy_net_monthly_firstyear_TOD9, 9) = m_cf.at(CF_TOD9OctEnergy, 1);
	m_cf.at(CF_energy_net_monthly_firstyear_TOD9, 10) = m_cf.at(CF_TOD9NovEnergy, 1);
	m_cf.at(CF_energy_net_monthly_firstyear_TOD9, 11) = m_cf.at(CF_TOD9DecEnergy, 1);

	save_cf( m_cm, m_cf,  CF_revenue_monthly_firstyear_TOD9, 11, "cf_revenue_monthly_firstyear_TOD9");
	save_cf( m_cm, m_cf,  CF_energy_net_monthly_firstyear_TOD9, 11, "cf_energy_net_monthly_firstyear_TOD9");


	m_cm->assign("firstyear_revenue_dispatch1", var_data((ssc_number_t)m_cf.at(CF_TOD1Revenue, 1)));
	m_cm->assign("firstyear_revenue_dispatch2", var_data((ssc_number_t)m_cf.at(CF_TOD2Revenue, 1)));
	m_cm->assign("firstyear_revenue_dispatch3", var_data((ssc_number_t)m_cf.at(CF_TOD3Revenue, 1)));
	m_cm->assign("firstyear_revenue_dispatch4", var_data((ssc_number_t)m_cf.at(CF_TOD4Revenue, 1)));
	m_cm->assign("firstyear_revenue_dispatch5", var_data((ssc_number_t)m_cf.at(CF_TOD5Revenue, 1)));
	m_cm->assign("firstyear_revenue_dispatch6", var_data((ssc_number_t)m_cf.at(CF_TOD6Revenue, 1)));
	m_cm->assign("firstyear_revenue_dispatch7", var_data((ssc_number_t)m_cf.at(CF_TOD7Revenue, 1)));
	m_cm->assign("firstyear_revenue_dispatch8", var_data((ssc_number_t)m_cf.at(CF_TOD8Revenue, 1)));
	m_cm->assign("firstyear_revenue_dispatch9", var_data((ssc_number_t)m_cf.at(CF_TOD9Revenue, 1)));

	m_cm->assign("firstyear_energy_dispatch1", var_data((ssc_number_t)m_cf.at(CF_TOD1Energy, 1)));
	m_cm->assign("firstyear_energy_dispatch2", var_data((ssc_number_t)m_cf.at(CF_TOD2Energy, 1)));
	m_cm->assign("firstyear_energy_dispatch3", var_data((ssc_number_t)m_cf.at(CF_TOD3Energy, 1)));
	m_cm->assign("firstyear_energy_dispatch4", var_data((ssc_number_t)m_cf.at(CF_TOD4Energy, 1)));
	m_cm->assign("firstyear_energy_dispatch5", var_data((ssc_number_t)m_cf.at(CF_TOD5Energy, 1)));
	m_cm->assign("firstyear_energy_dispatch6", var_data((ssc_number_t)m_cf.at(CF_TOD6Energy, 1)));
	m_cm->assign("firstyear_energy_dispatch7", var_data((ssc_number_t)m_cf.at(CF_TOD7Energy, 1)));
	m_cm->assign("firstyear_energy_dispatch8", var_data((ssc_number_t)m_cf.at(CF_TOD8Energy, 1)));
	m_cm->assign("firstyear_energy_dispatch9", var_data((ssc_number_t)m_cf.at(CF_TOD9Energy, 1)));
	// revenue in $
	m_cm->assign("firstyear_energy_price1", var_data((ssc_number_t)((m_cf.at(CF_TOD1Energy, 1) == 0) ? 0 : (m_cf.at(CF_TOD1Revenue, 1) / m_cf.at(CF_TOD1Energy, 1)) * 100.0)));
	m_cm->assign("firstyear_energy_price2", var_data((ssc_number_t)((m_cf.at(CF_TOD2Energy, 1) == 0) ? 0 : (m_cf.at(CF_TOD2Revenue, 1) / m_cf.at(CF_TOD2Energy, 1)) * 100.0)));
	m_cm->assign("firstyear_energy_price3", var_data((ssc_number_t)((m_cf.at(CF_TOD3Energy, 1) == 0) ? 0 : (m_cf.at(CF_TOD3Revenue, 1) / m_cf.at(CF_TOD3Energy, 1)) * 100.0)));
	m_cm->assign("firstyear_energy_price4", var_data((ssc_number_t)((m_cf.at(CF_TOD4Energy, 1) == 0) ? 0 : (m_cf.at(CF_TOD4Revenue, 1) / m_cf.at(CF_TOD4Energy, 1)) * 100.0)));
	m_cm->assign("firstyear_energy_price5", var_data((ssc_number_t)((m_cf.at(CF_TOD5Energy, 1) == 0) ? 0 : (m_cf.at(CF_TOD5Revenue, 1) / m_cf.at(CF_TOD5Energy, 1)) * 100.0)));
	m_cm->assign("firstyear_energy_price6", var_data((ssc_number_t)((m_cf.at(CF_TOD6Energy, 1) == 0) ? 0 : (m_cf.at(CF_TOD6Revenue, 1) / m_cf.at(CF_TOD6Energy, 1)) * 100.0)));
	m_cm->assign("firstyear_energy_price7", var_data((ssc_number_t)((m_cf.at(CF_TOD7Energy, 1) == 0) ? 0 : (m_cf.at(CF_TOD7Revenue, 1) / m_cf.at(CF_TOD7Energy, 1)) * 100.0)));
	m_cm->assign("firstyear_energy_price8", var_data((ssc_number_t)((m_cf.at(CF_TOD8Energy, 1) == 0) ? 0 : (m_cf.at(CF_TOD8Revenue, 1) / m_cf.at(CF_TOD8Energy, 1)) * 100.0)));
	m_cm->assign("firstyear_energy_price9", var_data((ssc_number_t)((m_cf.at(CF_TOD9Energy, 1) == 0) ? 0 : (m_cf.at(CF_TOD9Revenue, 1) / m_cf.at(CF_TOD9Energy, 1)) * 100.0)));

	return true;
}

util::matrix_t<double>& dispatch_calculations::dispatch_output()
{
	return m_cf;
}

double dispatch_calculations::tod_energy(int period, int year)
{
	double energy = 0;
	switch (period)
	{
	case 1:
		energy = m_cf.at(CF_TOD1Energy, year);
		break;
	case 2:
		energy = m_cf.at(CF_TOD2Energy, year);
		break;
	case 3:
		energy = m_cf.at(CF_TOD3Energy, year);
		break;
	case 4:
		energy = m_cf.at(CF_TOD4Energy, year);
		break;
	case 5:
		energy = m_cf.at(CF_TOD5Energy, year);
		break;
	case 6:
		energy = m_cf.at(CF_TOD6Energy, year);
		break;
	case 7:
		energy = m_cf.at(CF_TOD7Energy, year);
		break;
	case 8:
		energy = m_cf.at(CF_TOD8Energy, year);
		break;
	case 9:
		energy = m_cf.at(CF_TOD9Energy, year);
		break;
	}
	return energy;
}
//  convenience function for tod periods 1 through 9
double dispatch_calculations::tod_energy_value(int year)
{
	double energy_value = 0;
	if (m_timestep)
	{
		energy_value += m_cf.at(CF_TODJanRevenue, year);
		energy_value += m_cf.at(CF_TODFebRevenue, year);
		energy_value += m_cf.at(CF_TODMarRevenue, year);
		energy_value += m_cf.at(CF_TODAprRevenue, year);
		energy_value += m_cf.at(CF_TODMayRevenue, year);
		energy_value += m_cf.at(CF_TODJunRevenue, year);
		energy_value += m_cf.at(CF_TODJulRevenue, year);
		energy_value += m_cf.at(CF_TODAugRevenue, year);
		energy_value += m_cf.at(CF_TODSepRevenue, year);
		energy_value += m_cf.at(CF_TODOctRevenue, year);
		energy_value += m_cf.at(CF_TODNovRevenue, year);
		energy_value += m_cf.at(CF_TODDecRevenue, year);
	}
	else  // diurnal
	{
		for (int i = 1; i < 10; i++)
			energy_value += tod_energy_value(i, year);
	}
	return energy_value;
}

double dispatch_calculations::tod_energy_value(int period, int year)
{
	double energy_value = 0;


	switch (period)
	{
	case 1:
		energy_value = m_cf.at(CF_TOD1Energy, year)
			* m_cm->as_double("dispatch_factor1");
		break;
	case 2:
		energy_value = m_cf.at(CF_TOD2Energy, year)
			* m_cm->as_double("dispatch_factor2");
		break;
	case 3:
		energy_value = m_cf.at(CF_TOD3Energy, year)
			* m_cm->as_double("dispatch_factor3");
		break;
	case 4:
		energy_value = m_cf.at(CF_TOD4Energy, year)
			* m_cm->as_double("dispatch_factor4");
		break;
	case 5:
		energy_value = m_cf.at(CF_TOD5Energy, year)
			* m_cm->as_double("dispatch_factor5");
		break;
	case 6:
		energy_value = m_cf.at(CF_TOD6Energy, year)
			* m_cm->as_double("dispatch_factor6");
		break;
	case 7:
		energy_value = m_cf.at(CF_TOD7Energy, year)
			* m_cm->as_double("dispatch_factor7");
		break;
	case 8:
		energy_value = m_cf.at(CF_TOD8Energy, year)
			* m_cm->as_double("dispatch_factor8");
		break;
	case 9:
		energy_value = m_cf.at(CF_TOD9Energy, year)
			* m_cm->as_double("dispatch_factor9");
		break;
	}
	return energy_value;
}

bool dispatch_calculations::setup()
{
	// initialize cashflow matrix
	if ((m_nyears + 1) > 12)
		m_cf.resize_fill(CF_max_dispatch, m_nyears + 1, 0.0);
	else // must be able to handle TOD periods and months hard crash in releases 2016.3.14-r1 and before
		m_cf.resize_fill(CF_max_dispatch, 12, 0.0);

	size_t nrows, ncols;
	ssc_number_t *disp_weekday = m_cm->as_matrix("dispatch_sched_weekday", &nrows, &ncols);
	if (nrows != 12 || ncols != 24)
	{
		m_error = util::format("dispatch values weekday schedule must be 12x24, input is %dx%d", (int)nrows, (int)ncols);
		throw exec_error("dispatch_values", m_error);
	}
	ssc_number_t *disp_weekend = m_cm->as_matrix("dispatch_sched_weekend", &nrows, &ncols);
	if (nrows != 12 || ncols != 24)
	{
		m_error = util::format("dispatch values weekend schedule must be 12x24, input is %dx%d", (int)nrows, (int)ncols);
		throw exec_error("dispatch_values", m_error);
	}
	util::matrix_t<double> schedwkday(12, 24);
	schedwkday.assign(disp_weekday, nrows, ncols);
	util::matrix_t<double> schedwkend(12, 24);
	schedwkend.assign(disp_weekend, nrows, ncols);

	int tod[8760];

	if (!util::translate_schedule(tod, schedwkday, schedwkend, 1, 9))
	{
		m_error = "could not translate weekday and weekend schedules for dispatch values";
		throw general_error(m_error);
	}

	m_periods.resize(8760, 1);
	ssc_number_t *ppa_multipliers = m_cm->allocate("ppa_multipliers", 8760);
	
	for (int i = 0; i < 8760; i++)
	{
		m_periods[i] = tod[i];
	
		switch (tod[i])
		{
		case 1:
			ppa_multipliers[i] = m_cm->as_number("dispatch_factor1");
			break;
		case 2:
			ppa_multipliers[i] = m_cm->as_number("dispatch_factor2");
			break;
		case 3:
			ppa_multipliers[i] = m_cm->as_number("dispatch_factor3");
			break;
		case 4:
			ppa_multipliers[i] = m_cm->as_number("dispatch_factor4");
			break;
		case 5:
			ppa_multipliers[i] = m_cm->as_number("dispatch_factor5");
			break;
		case 6:
			ppa_multipliers[i] = m_cm->as_number("dispatch_factor6");
			break;
		case 7:
			ppa_multipliers[i] = m_cm->as_number("dispatch_factor7");
			break;
		case 8:
			ppa_multipliers[i] = m_cm->as_number("dispatch_factor8");
			break;
		case 9:
			ppa_multipliers[i] = m_cm->as_number("dispatch_factor9");
			break;
		}
	}

	return m_error.length() == 0;
}

bool dispatch_calculations::setup_ts()
{
	// initialize cashflow matrix
	if ((m_nyears + 1) > 12)
		m_cf.resize_fill(CF_max_timestep, m_nyears + 1, 0.0);
	else // must be able to handle TOD periods and months hard crash in releases 2016.3.14-r1 and before
		m_cf.resize_fill(CF_max_timestep, 12, 0.0);

	m_multipliers = m_cm->as_array("dispatch_factors_ts", &m_nmultipliers);
    if ((m_cm->is_assigned("en_electricity_rates") && m_cm->as_number("en_electricity_rates") == 1)) {
        m_gen = m_cm->as_array("revenue_gen", &m_ngen);
    }
    else {
        m_gen = m_cm->as_array("gen", &m_ngen);
    }

	// TODO - handle differences in ngen and nmultipliers - checked in compute_lifetime_dispatch_ts
	// Could interporlate for different number of records like for PV and utility rates
//	if (m_ngen != m_nmultipliers)
//	{
//		m_error = "issue with timestep dispatch multipliers";
//		throw general_error(m_error);
//	}

	ssc_number_t *ppa_multipliers = m_cm->allocate("ppa_multipliers", m_nmultipliers);

	for (size_t i = 0; i < m_nmultipliers; i++)
		ppa_multipliers[i] = m_multipliers[i];

	return m_error.length() == 0;
}


int dispatch_calculations::operator()(size_t time)
{
	if (time < m_periods.size()) return m_periods[time];
	else return 1;
}



bool dispatch_calculations::compute_dispatch_output()
{
	//Calculate energy dispatched in each dispatch period 


	int h;
	size_t count = m_hourly_energy.size();

	// hourly energy

	if (count != 8760)
	{
		std::stringstream outm;
		outm << "Bad hourly gen output length (" << count << "), should be 8760 value";
		m_cm->log(outm.str());
		return false;
	}


	m_cf.at(CF_TOD1Energy, 1) = 0;
	m_cf.at(CF_TOD2Energy, 1) = 0;
	m_cf.at(CF_TOD3Energy, 1) = 0;
	m_cf.at(CF_TOD4Energy, 1) = 0;
	m_cf.at(CF_TOD5Energy, 1) = 0;
	m_cf.at(CF_TOD6Energy, 1) = 0;
	m_cf.at(CF_TOD7Energy, 1) = 0;
	m_cf.at(CF_TOD8Energy, 1) = 0;
	m_cf.at(CF_TOD9Energy, 1) = 0;


	// hourly net energy include first year curtailment, availability and degradation
	// unapply first year availability and degradation so that dispatch can be properly calculated and 
	// so that availability and degradation is not applied multiple times
	// Better would be to calculate dispatch energy in cmod_annual output; however, dispatch only
	// applies to IPP and DHF markets.



	for (h = 0; h<8760; h++)
	{
		switch (m_periods[h])
		{
		case 1:
			m_cf.at(CF_TOD1Energy, 1) += m_hourly_energy[h];
			break;
		case 2:
			m_cf.at(CF_TOD2Energy, 1) += m_hourly_energy[h];
			break;
		case 3:
			m_cf.at(CF_TOD3Energy, 1) += m_hourly_energy[h];
			break;
		case 4:
			m_cf.at(CF_TOD4Energy, 1) += m_hourly_energy[h];
			break;
		case 5:
			m_cf.at(CF_TOD5Energy, 1) += m_hourly_energy[h];
			break;
		case 6:
			m_cf.at(CF_TOD6Energy, 1) += m_hourly_energy[h];
			break;
		case 7:
			m_cf.at(CF_TOD7Energy, 1) += m_hourly_energy[h];
			break;
		case 8:
			m_cf.at(CF_TOD8Energy, 1) += m_hourly_energy[h];
			break;
		case 9:
			m_cf.at(CF_TOD9Energy, 1) += m_hourly_energy[h];
			break;
		}
	}
	// remove degradation and availability from year 1 values but keep curtailment so that
	// availability and degradation yearly schedules from cmod_annualoutput can be properly applied.
	double year1_TOD1Energy = m_cf.at(CF_TOD1Energy, 1);
	double year1_TOD2Energy = m_cf.at(CF_TOD2Energy, 1);
	double year1_TOD3Energy = m_cf.at(CF_TOD3Energy, 1);
	double year1_TOD4Energy = m_cf.at(CF_TOD4Energy, 1);
	double year1_TOD5Energy = m_cf.at(CF_TOD5Energy, 1);
	double year1_TOD6Energy = m_cf.at(CF_TOD6Energy, 1);
	double year1_TOD7Energy = m_cf.at(CF_TOD7Energy, 1);
	double year1_TOD8Energy = m_cf.at(CF_TOD8Energy, 1);
	double year1_TOD9Energy = m_cf.at(CF_TOD9Energy, 1);


	for (int y = 0; y <= m_nyears; y++)
	{
		// compute energy dispatched
		m_cf.at(CF_TOD1Energy, y) = year1_TOD1Energy * m_degradation[y]; 
		m_cf.at(CF_TOD2Energy, y) = year1_TOD2Energy * m_degradation[y]; 
		m_cf.at(CF_TOD3Energy, y) = year1_TOD3Energy * m_degradation[y]; 
		m_cf.at(CF_TOD4Energy, y) = year1_TOD4Energy * m_degradation[y]; 
		m_cf.at(CF_TOD5Energy, y) = year1_TOD5Energy * m_degradation[y]; 
		m_cf.at(CF_TOD6Energy, y) = year1_TOD6Energy * m_degradation[y]; 
		m_cf.at(CF_TOD7Energy, y) = year1_TOD7Energy * m_degradation[y]; 
		m_cf.at(CF_TOD8Energy, y) = year1_TOD8Energy * m_degradation[y]; 
		m_cf.at(CF_TOD9Energy, y) = year1_TOD9Energy * m_degradation[y]; 
	}


	return true;
}

bool dispatch_calculations::process_dispatch_output()
{
	//Calculate energy dispatched in each dispatch period 

	size_t count=m_hourly_energy.size();

	// hourly energy
	if (count != 8760)
	{
		std::stringstream outm;
		outm << "Bad hourly gen output length (" << count << "), should be 8760 value";
		m_cm->log(outm.str());
		return false;
	}



	m_cf.at(CF_TODJanEnergy, 1) = 0;
	m_cf.at(CF_TODFebEnergy, 1) = 0;
	m_cf.at(CF_TODMarEnergy, 1) = 0;
	m_cf.at(CF_TODAprEnergy, 1) = 0;
	m_cf.at(CF_TODMayEnergy, 1) = 0;
	m_cf.at(CF_TODJunEnergy, 1) = 0;
	m_cf.at(CF_TODJulEnergy, 1) = 0;
	m_cf.at(CF_TODAugEnergy, 1) = 0;
	m_cf.at(CF_TODSepEnergy, 1) = 0;
	m_cf.at(CF_TODOctEnergy, 1) = 0;
	m_cf.at(CF_TODNovEnergy, 1) = 0;
	m_cf.at(CF_TODDecEnergy, 1) = 0;

	m_cf.at(CF_TOD1JanEnergy, 1) = 0;
	m_cf.at(CF_TOD1FebEnergy, 1) = 0;
	m_cf.at(CF_TOD1MarEnergy, 1) = 0;
	m_cf.at(CF_TOD1AprEnergy, 1) = 0;
	m_cf.at(CF_TOD1MayEnergy, 1) = 0;
	m_cf.at(CF_TOD1JunEnergy, 1) = 0;
	m_cf.at(CF_TOD1JulEnergy, 1) = 0;
	m_cf.at(CF_TOD1AugEnergy, 1) = 0;
	m_cf.at(CF_TOD1SepEnergy, 1) = 0;
	m_cf.at(CF_TOD1OctEnergy, 1) = 0;
	m_cf.at(CF_TOD1NovEnergy, 1) = 0;
	m_cf.at(CF_TOD1DecEnergy, 1) = 0;

	m_cf.at(CF_TOD2JanEnergy, 1) = 0;
	m_cf.at(CF_TOD2FebEnergy, 1) = 0;
	m_cf.at(CF_TOD2MarEnergy, 1) = 0;
	m_cf.at(CF_TOD2AprEnergy, 1) = 0;
	m_cf.at(CF_TOD2MayEnergy, 1) = 0;
	m_cf.at(CF_TOD2JunEnergy, 1) = 0;
	m_cf.at(CF_TOD2JulEnergy, 1) = 0;
	m_cf.at(CF_TOD2AugEnergy, 1) = 0;
	m_cf.at(CF_TOD2SepEnergy, 1) = 0;
	m_cf.at(CF_TOD2OctEnergy, 1) = 0;
	m_cf.at(CF_TOD2NovEnergy, 1) = 0;
	m_cf.at(CF_TOD2DecEnergy, 1) = 0;

	m_cf.at(CF_TOD3JanEnergy, 1) = 0;
	m_cf.at(CF_TOD3FebEnergy, 1) = 0;
	m_cf.at(CF_TOD3MarEnergy, 1) = 0;
	m_cf.at(CF_TOD3AprEnergy, 1) = 0;
	m_cf.at(CF_TOD3MayEnergy, 1) = 0;
	m_cf.at(CF_TOD3JunEnergy, 1) = 0;
	m_cf.at(CF_TOD3JulEnergy, 1) = 0;
	m_cf.at(CF_TOD3AugEnergy, 1) = 0;
	m_cf.at(CF_TOD3SepEnergy, 1) = 0;
	m_cf.at(CF_TOD3OctEnergy, 1) = 0;
	m_cf.at(CF_TOD3NovEnergy, 1) = 0;
	m_cf.at(CF_TOD3DecEnergy, 1) = 0;

	m_cf.at(CF_TOD4JanEnergy, 1) = 0;
	m_cf.at(CF_TOD4FebEnergy, 1) = 0;
	m_cf.at(CF_TOD4MarEnergy, 1) = 0;
	m_cf.at(CF_TOD4AprEnergy, 1) = 0;
	m_cf.at(CF_TOD4MayEnergy, 1) = 0;
	m_cf.at(CF_TOD4JunEnergy, 1) = 0;
	m_cf.at(CF_TOD4JulEnergy, 1) = 0;
	m_cf.at(CF_TOD4AugEnergy, 1) = 0;
	m_cf.at(CF_TOD4SepEnergy, 1) = 0;
	m_cf.at(CF_TOD4OctEnergy, 1) = 0;
	m_cf.at(CF_TOD4NovEnergy, 1) = 0;
	m_cf.at(CF_TOD4DecEnergy, 1) = 0;

	m_cf.at(CF_TOD5JanEnergy, 1) = 0;
	m_cf.at(CF_TOD5FebEnergy, 1) = 0;
	m_cf.at(CF_TOD5MarEnergy, 1) = 0;
	m_cf.at(CF_TOD5AprEnergy, 1) = 0;
	m_cf.at(CF_TOD5MayEnergy, 1) = 0;
	m_cf.at(CF_TOD5JunEnergy, 1) = 0;
	m_cf.at(CF_TOD5JulEnergy, 1) = 0;
	m_cf.at(CF_TOD5AugEnergy, 1) = 0;
	m_cf.at(CF_TOD5SepEnergy, 1) = 0;
	m_cf.at(CF_TOD5OctEnergy, 1) = 0;
	m_cf.at(CF_TOD5NovEnergy, 1) = 0;
	m_cf.at(CF_TOD5DecEnergy, 1) = 0;

	m_cf.at(CF_TOD6JanEnergy, 1) = 0;
	m_cf.at(CF_TOD6FebEnergy, 1) = 0;
	m_cf.at(CF_TOD6MarEnergy, 1) = 0;
	m_cf.at(CF_TOD6AprEnergy, 1) = 0;
	m_cf.at(CF_TOD6MayEnergy, 1) = 0;
	m_cf.at(CF_TOD6JunEnergy, 1) = 0;
	m_cf.at(CF_TOD6JulEnergy, 1) = 0;
	m_cf.at(CF_TOD6AugEnergy, 1) = 0;
	m_cf.at(CF_TOD6SepEnergy, 1) = 0;
	m_cf.at(CF_TOD6OctEnergy, 1) = 0;
	m_cf.at(CF_TOD6NovEnergy, 1) = 0;
	m_cf.at(CF_TOD6DecEnergy, 1) = 0;

	m_cf.at(CF_TOD7JanEnergy, 1) = 0;
	m_cf.at(CF_TOD7FebEnergy, 1) = 0;
	m_cf.at(CF_TOD7MarEnergy, 1) = 0;
	m_cf.at(CF_TOD7AprEnergy, 1) = 0;
	m_cf.at(CF_TOD7MayEnergy, 1) = 0;
	m_cf.at(CF_TOD7JunEnergy, 1) = 0;
	m_cf.at(CF_TOD7JulEnergy, 1) = 0;
	m_cf.at(CF_TOD7AugEnergy, 1) = 0;
	m_cf.at(CF_TOD7SepEnergy, 1) = 0;
	m_cf.at(CF_TOD7OctEnergy, 1) = 0;
	m_cf.at(CF_TOD7NovEnergy, 1) = 0;
	m_cf.at(CF_TOD7DecEnergy, 1) = 0;

	m_cf.at(CF_TOD8JanEnergy, 1) = 0;
	m_cf.at(CF_TOD8FebEnergy, 1) = 0;
	m_cf.at(CF_TOD8MarEnergy, 1) = 0;
	m_cf.at(CF_TOD8AprEnergy, 1) = 0;
	m_cf.at(CF_TOD8MayEnergy, 1) = 0;
	m_cf.at(CF_TOD8JunEnergy, 1) = 0;
	m_cf.at(CF_TOD8JulEnergy, 1) = 0;
	m_cf.at(CF_TOD8AugEnergy, 1) = 0;
	m_cf.at(CF_TOD8SepEnergy, 1) = 0;
	m_cf.at(CF_TOD8OctEnergy, 1) = 0;
	m_cf.at(CF_TOD8NovEnergy, 1) = 0;
	m_cf.at(CF_TOD8DecEnergy, 1) = 0;

	m_cf.at(CF_TOD9JanEnergy, 1) = 0;
	m_cf.at(CF_TOD9FebEnergy, 1) = 0;
	m_cf.at(CF_TOD9MarEnergy, 1) = 0;
	m_cf.at(CF_TOD9AprEnergy, 1) = 0;
	m_cf.at(CF_TOD9MayEnergy, 1) = 0;
	m_cf.at(CF_TOD9JunEnergy, 1) = 0;
	m_cf.at(CF_TOD9JulEnergy, 1) = 0;
	m_cf.at(CF_TOD9AugEnergy, 1) = 0;
	m_cf.at(CF_TOD9SepEnergy, 1) = 0;
	m_cf.at(CF_TOD9OctEnergy, 1) = 0;
	m_cf.at(CF_TOD9NovEnergy, 1) = 0;
	m_cf.at(CF_TOD9DecEnergy, 1) = 0;

	int i = 0;
	for (int m = 0; m<12; m++)
	{
		for (size_t d = 0; d<util::nday[m]; d++)
		{
			for (int h = 0; h<24 && i<8760 && m * 24 + h<288; h++)
			{
				switch (m)
				{
				case 0:
					m_cf.at(CF_TODJanEnergy, 1) += m_hourly_energy[i];
					switch (m_periods[i])
					{
					case 1:
						m_cf.at(CF_TOD1JanEnergy, 1) += m_hourly_energy[i];
						break;
					case 2:
						m_cf.at(CF_TOD2JanEnergy, 1) += m_hourly_energy[i];
						break;
					case 3:
						m_cf.at(CF_TOD3JanEnergy, 1) += m_hourly_energy[i];
						break;
					case 4:
						m_cf.at(CF_TOD4JanEnergy, 1) += m_hourly_energy[i];
						break;
					case 5:
						m_cf.at(CF_TOD5JanEnergy, 1) += m_hourly_energy[i];
						break;
					case 6:
						m_cf.at(CF_TOD6JanEnergy, 1) += m_hourly_energy[i];
						break;
					case 7:
						m_cf.at(CF_TOD7JanEnergy, 1) += m_hourly_energy[i];
						break;
					case 8:
						m_cf.at(CF_TOD8JanEnergy, 1) += m_hourly_energy[i];
						break;
					case 9:
						m_cf.at(CF_TOD9JanEnergy, 1) += m_hourly_energy[i];
						break;
					}
					break;
				case 1:
					m_cf.at(CF_TODFebEnergy, 1) += m_hourly_energy[i];
					switch (m_periods[i])
					{
					case 1:
						m_cf.at(CF_TOD1FebEnergy, 1) += m_hourly_energy[i];
						break;
					case 2:
						m_cf.at(CF_TOD2FebEnergy, 1) += m_hourly_energy[i];
						break;
					case 3:
						m_cf.at(CF_TOD3FebEnergy, 1) += m_hourly_energy[i];
						break;
					case 4:
						m_cf.at(CF_TOD4FebEnergy, 1) += m_hourly_energy[i];
						break;
					case 5:
						m_cf.at(CF_TOD5FebEnergy, 1) += m_hourly_energy[i];
						break;
					case 6:
						m_cf.at(CF_TOD6FebEnergy, 1) += m_hourly_energy[i];
						break;
					case 7:
						m_cf.at(CF_TOD7FebEnergy, 1) += m_hourly_energy[i];
						break;
					case 8:
						m_cf.at(CF_TOD8FebEnergy, 1) += m_hourly_energy[i];
						break;
					case 9:
						m_cf.at(CF_TOD9FebEnergy, 1) += m_hourly_energy[i];
						break;
					}
					break;
				case 2:
					m_cf.at(CF_TODMarEnergy, 1) += m_hourly_energy[i];
					switch (m_periods[i])
					{
					case 1:
						m_cf.at(CF_TOD1MarEnergy, 1) += m_hourly_energy[i];
						break;
					case 2:
						m_cf.at(CF_TOD2MarEnergy, 1) += m_hourly_energy[i];
						break;
					case 3:
						m_cf.at(CF_TOD3MarEnergy, 1) += m_hourly_energy[i];
						break;
					case 4:
						m_cf.at(CF_TOD4MarEnergy, 1) += m_hourly_energy[i];
						break;
					case 5:
						m_cf.at(CF_TOD5MarEnergy, 1) += m_hourly_energy[i];
						break;
					case 6:
						m_cf.at(CF_TOD6MarEnergy, 1) += m_hourly_energy[i];
						break;
					case 7:
						m_cf.at(CF_TOD7MarEnergy, 1) += m_hourly_energy[i];
						break;
					case 8:
						m_cf.at(CF_TOD8MarEnergy, 1) += m_hourly_energy[i];
						break;
					case 9:
						m_cf.at(CF_TOD9MarEnergy, 1) += m_hourly_energy[i];
						break;
					}
					break;
				case 3:
					m_cf.at(CF_TODAprEnergy, 1) += m_hourly_energy[i];
					switch (m_periods[i])
					{
					case 1:
						m_cf.at(CF_TOD1AprEnergy, 1) += m_hourly_energy[i];
						break;
					case 2:
						m_cf.at(CF_TOD2AprEnergy, 1) += m_hourly_energy[i];
						break;
					case 3:
						m_cf.at(CF_TOD3AprEnergy, 1) += m_hourly_energy[i];
						break;
					case 4:
						m_cf.at(CF_TOD4AprEnergy, 1) += m_hourly_energy[i];
						break;
					case 5:
						m_cf.at(CF_TOD5AprEnergy, 1) += m_hourly_energy[i];
						break;
					case 6:
						m_cf.at(CF_TOD6AprEnergy, 1) += m_hourly_energy[i];
						break;
					case 7:
						m_cf.at(CF_TOD7AprEnergy, 1) += m_hourly_energy[i];
						break;
					case 8:
						m_cf.at(CF_TOD8AprEnergy, 1) += m_hourly_energy[i];
						break;
					case 9:
						m_cf.at(CF_TOD9AprEnergy, 1) += m_hourly_energy[i];
						break;
					}
					break;
				case 4:
					m_cf.at(CF_TODMayEnergy, 1) += m_hourly_energy[i];
					switch (m_periods[i])
					{
					case 1:
						m_cf.at(CF_TOD1MayEnergy, 1) += m_hourly_energy[i];
						break;
					case 2:
						m_cf.at(CF_TOD2MayEnergy, 1) += m_hourly_energy[i];
						break;
					case 3:
						m_cf.at(CF_TOD3MayEnergy, 1) += m_hourly_energy[i];
						break;
					case 4:
						m_cf.at(CF_TOD4MayEnergy, 1) += m_hourly_energy[i];
						break;
					case 5:
						m_cf.at(CF_TOD5MayEnergy, 1) += m_hourly_energy[i];
						break;
					case 6:
						m_cf.at(CF_TOD6MayEnergy, 1) += m_hourly_energy[i];
						break;
					case 7:
						m_cf.at(CF_TOD7MayEnergy, 1) += m_hourly_energy[i];
						break;
					case 8:
						m_cf.at(CF_TOD8MayEnergy, 1) += m_hourly_energy[i];
						break;
					case 9:
						m_cf.at(CF_TOD9MayEnergy, 1) += m_hourly_energy[i];
						break;
					}
					break;
				case 5:
					m_cf.at(CF_TODJunEnergy, 1) += m_hourly_energy[i];
					switch (m_periods[i])
					{
					case 1:
						m_cf.at(CF_TOD1JunEnergy, 1) += m_hourly_energy[i];
						break;
					case 2:
						m_cf.at(CF_TOD2JunEnergy, 1) += m_hourly_energy[i];
						break;
					case 3:
						m_cf.at(CF_TOD3JunEnergy, 1) += m_hourly_energy[i];
						break;
					case 4:
						m_cf.at(CF_TOD4JunEnergy, 1) += m_hourly_energy[i];
						break;
					case 5:
						m_cf.at(CF_TOD5JunEnergy, 1) += m_hourly_energy[i];
						break;
					case 6:
						m_cf.at(CF_TOD6JunEnergy, 1) += m_hourly_energy[i];
						break;
					case 7:
						m_cf.at(CF_TOD7JunEnergy, 1) += m_hourly_energy[i];
						break;
					case 8:
						m_cf.at(CF_TOD8JunEnergy, 1) += m_hourly_energy[i];
						break;
					case 9:
						m_cf.at(CF_TOD9JunEnergy, 1) += m_hourly_energy[i];
						break;
					}
					break;
				case 6:
					m_cf.at(CF_TODJulEnergy, 1) += m_hourly_energy[i];
					switch (m_periods[i])
					{
					case 1:
						m_cf.at(CF_TOD1JulEnergy, 1) += m_hourly_energy[i];
						break;
					case 2:
						m_cf.at(CF_TOD2JulEnergy, 1) += m_hourly_energy[i];
						break;
					case 3:
						m_cf.at(CF_TOD3JulEnergy, 1) += m_hourly_energy[i];
						break;
					case 4:
						m_cf.at(CF_TOD4JulEnergy, 1) += m_hourly_energy[i];
						break;
					case 5:
						m_cf.at(CF_TOD5JulEnergy, 1) += m_hourly_energy[i];
						break;
					case 6:
						m_cf.at(CF_TOD6JulEnergy, 1) += m_hourly_energy[i];
						break;
					case 7:
						m_cf.at(CF_TOD7JulEnergy, 1) += m_hourly_energy[i];
						break;
					case 8:
						m_cf.at(CF_TOD8JulEnergy, 1) += m_hourly_energy[i];
						break;
					case 9:
						m_cf.at(CF_TOD9JulEnergy, 1) += m_hourly_energy[i];
						break;
					}
					break;
				case 7:
					m_cf.at(CF_TODAugEnergy, 1) += m_hourly_energy[i];
					switch (m_periods[i])
					{
					case 1:
						m_cf.at(CF_TOD1AugEnergy, 1) += m_hourly_energy[i];
						break;
					case 2:
						m_cf.at(CF_TOD2AugEnergy, 1) += m_hourly_energy[i];
						break;
					case 3:
						m_cf.at(CF_TOD3AugEnergy, 1) += m_hourly_energy[i];
						break;
					case 4:
						m_cf.at(CF_TOD4AugEnergy, 1) += m_hourly_energy[i];
						break;
					case 5:
						m_cf.at(CF_TOD5AugEnergy, 1) += m_hourly_energy[i];
						break;
					case 6:
						m_cf.at(CF_TOD6AugEnergy, 1) += m_hourly_energy[i];
						break;
					case 7:
						m_cf.at(CF_TOD7AugEnergy, 1) += m_hourly_energy[i];
						break;
					case 8:
						m_cf.at(CF_TOD8AugEnergy, 1) += m_hourly_energy[i];
						break;
					case 9:
						m_cf.at(CF_TOD9AugEnergy, 1) += m_hourly_energy[i];
						break;
					}
					break;
				case 8:
					m_cf.at(CF_TODSepEnergy, 1) += m_hourly_energy[i];
					switch (m_periods[i])
					{
					case 1:
						m_cf.at(CF_TOD1SepEnergy, 1) += m_hourly_energy[i];
						break;
					case 2:
						m_cf.at(CF_TOD2SepEnergy, 1) += m_hourly_energy[i];
						break;
					case 3:
						m_cf.at(CF_TOD3SepEnergy, 1) += m_hourly_energy[i];
						break;
					case 4:
						m_cf.at(CF_TOD4SepEnergy, 1) += m_hourly_energy[i];
						break;
					case 5:
						m_cf.at(CF_TOD5SepEnergy, 1) += m_hourly_energy[i];
						break;
					case 6:
						m_cf.at(CF_TOD6SepEnergy, 1) += m_hourly_energy[i];
						break;
					case 7:
						m_cf.at(CF_TOD7SepEnergy, 1) += m_hourly_energy[i];
						break;
					case 8:
						m_cf.at(CF_TOD8SepEnergy, 1) += m_hourly_energy[i];
						break;
					case 9:
						m_cf.at(CF_TOD9SepEnergy, 1) += m_hourly_energy[i];
						break;
					}
					break;
				case 9:
					m_cf.at(CF_TODOctEnergy, 1) += m_hourly_energy[i];
					switch (m_periods[i])
					{
					case 1:
						m_cf.at(CF_TOD1OctEnergy, 1) += m_hourly_energy[i];
						break;
					case 2:
						m_cf.at(CF_TOD2OctEnergy, 1) += m_hourly_energy[i];
						break;
					case 3:
						m_cf.at(CF_TOD3OctEnergy, 1) += m_hourly_energy[i];
						break;
					case 4:
						m_cf.at(CF_TOD4OctEnergy, 1) += m_hourly_energy[i];
						break;
					case 5:
						m_cf.at(CF_TOD5OctEnergy, 1) += m_hourly_energy[i];
						break;
					case 6:
						m_cf.at(CF_TOD6OctEnergy, 1) += m_hourly_energy[i];
						break;
					case 7:
						m_cf.at(CF_TOD7OctEnergy, 1) += m_hourly_energy[i];
						break;
					case 8:
						m_cf.at(CF_TOD8OctEnergy, 1) += m_hourly_energy[i];
						break;
					case 9:
						m_cf.at(CF_TOD9OctEnergy, 1) += m_hourly_energy[i];
						break;
					}
					break;
				case 10:
					m_cf.at(CF_TODNovEnergy, 1) += m_hourly_energy[i];
					switch (m_periods[i])
					{
					case 1:
						m_cf.at(CF_TOD1NovEnergy, 1) += m_hourly_energy[i];
						break;
					case 2:
						m_cf.at(CF_TOD2NovEnergy, 1) += m_hourly_energy[i];
						break;
					case 3:
						m_cf.at(CF_TOD3NovEnergy, 1) += m_hourly_energy[i];
						break;
					case 4:
						m_cf.at(CF_TOD4NovEnergy, 1) += m_hourly_energy[i];
						break;
					case 5:
						m_cf.at(CF_TOD5NovEnergy, 1) += m_hourly_energy[i];
						break;
					case 6:
						m_cf.at(CF_TOD6NovEnergy, 1) += m_hourly_energy[i];
						break;
					case 7:
						m_cf.at(CF_TOD7NovEnergy, 1) += m_hourly_energy[i];
						break;
					case 8:
						m_cf.at(CF_TOD8NovEnergy, 1) += m_hourly_energy[i];
						break;
					case 9:
						m_cf.at(CF_TOD9NovEnergy, 1) += m_hourly_energy[i];
						break;
					}
					break;
				case 11:
					m_cf.at(CF_TODDecEnergy, 1) += m_hourly_energy[i];
					switch (m_periods[i])
					{
					case 1:
						m_cf.at(CF_TOD1DecEnergy, 1) += m_hourly_energy[i];
						break;
					case 2:
						m_cf.at(CF_TOD2DecEnergy, 1) += m_hourly_energy[i];
						break;
					case 3:
						m_cf.at(CF_TOD3DecEnergy, 1) += m_hourly_energy[i];
						break;
					case 4:
						m_cf.at(CF_TOD4DecEnergy, 1) += m_hourly_energy[i];
						break;
					case 5:
						m_cf.at(CF_TOD5DecEnergy, 1) += m_hourly_energy[i];
						break;
					case 6:
						m_cf.at(CF_TOD6DecEnergy, 1) += m_hourly_energy[i];
						break;
					case 7:
						m_cf.at(CF_TOD7DecEnergy, 1) += m_hourly_energy[i];
						break;
					case 8:
						m_cf.at(CF_TOD8DecEnergy, 1) += m_hourly_energy[i];
						break;
					case 9:
						m_cf.at(CF_TOD9DecEnergy, 1) += m_hourly_energy[i];
						break;
					}
					break;
				}
				i++;
			}
		}
	}


	double year1_TODJanEnergy = m_cf.at(CF_TODJanEnergy, 1);
	double year1_TODFebEnergy = m_cf.at(CF_TODFebEnergy, 1);
	double year1_TODMarEnergy = m_cf.at(CF_TODMarEnergy, 1);
	double year1_TODAprEnergy = m_cf.at(CF_TODAprEnergy, 1);
	double year1_TODMayEnergy = m_cf.at(CF_TODMayEnergy, 1);
	double year1_TODJunEnergy = m_cf.at(CF_TODJunEnergy, 1);
	double year1_TODJulEnergy = m_cf.at(CF_TODJulEnergy, 1);
	double year1_TODAugEnergy = m_cf.at(CF_TODAugEnergy, 1);
	double year1_TODSepEnergy = m_cf.at(CF_TODSepEnergy, 1);
	double year1_TODOctEnergy = m_cf.at(CF_TODOctEnergy, 1);
	double year1_TODNovEnergy = m_cf.at(CF_TODNovEnergy, 1);
	double year1_TODDecEnergy = m_cf.at(CF_TODDecEnergy, 1);

	double year1_TOD1JanEnergy = m_cf.at(CF_TOD1JanEnergy, 1);
	double year1_TOD1FebEnergy = m_cf.at(CF_TOD1FebEnergy, 1);
	double year1_TOD1MarEnergy = m_cf.at(CF_TOD1MarEnergy, 1);
	double year1_TOD1AprEnergy = m_cf.at(CF_TOD1AprEnergy, 1);
	double year1_TOD1MayEnergy = m_cf.at(CF_TOD1MayEnergy, 1);
	double year1_TOD1JunEnergy = m_cf.at(CF_TOD1JunEnergy, 1);
	double year1_TOD1JulEnergy = m_cf.at(CF_TOD1JulEnergy, 1);
	double year1_TOD1AugEnergy = m_cf.at(CF_TOD1AugEnergy, 1);
	double year1_TOD1SepEnergy = m_cf.at(CF_TOD1SepEnergy, 1);
	double year1_TOD1OctEnergy = m_cf.at(CF_TOD1OctEnergy, 1);
	double year1_TOD1NovEnergy = m_cf.at(CF_TOD1NovEnergy, 1);
	double year1_TOD1DecEnergy = m_cf.at(CF_TOD1DecEnergy, 1);

	double year1_TOD2JanEnergy = m_cf.at(CF_TOD2JanEnergy, 1);
	double year1_TOD2FebEnergy = m_cf.at(CF_TOD2FebEnergy, 1);
	double year1_TOD2MarEnergy = m_cf.at(CF_TOD2MarEnergy, 1);
	double year1_TOD2AprEnergy = m_cf.at(CF_TOD2AprEnergy, 1);
	double year1_TOD2MayEnergy = m_cf.at(CF_TOD2MayEnergy, 1);
	double year1_TOD2JunEnergy = m_cf.at(CF_TOD2JunEnergy, 1);
	double year1_TOD2JulEnergy = m_cf.at(CF_TOD2JulEnergy, 1);
	double year1_TOD2AugEnergy = m_cf.at(CF_TOD2AugEnergy, 1);
	double year1_TOD2SepEnergy = m_cf.at(CF_TOD2SepEnergy, 1);
	double year1_TOD2OctEnergy = m_cf.at(CF_TOD2OctEnergy, 1);
	double year1_TOD2NovEnergy = m_cf.at(CF_TOD2NovEnergy, 1);
	double year1_TOD2DecEnergy = m_cf.at(CF_TOD2DecEnergy, 1);

	double year1_TOD3JanEnergy = m_cf.at(CF_TOD3JanEnergy, 1);
	double year1_TOD3FebEnergy = m_cf.at(CF_TOD3FebEnergy, 1);
	double year1_TOD3MarEnergy = m_cf.at(CF_TOD3MarEnergy, 1);
	double year1_TOD3AprEnergy = m_cf.at(CF_TOD3AprEnergy, 1);
	double year1_TOD3MayEnergy = m_cf.at(CF_TOD3MayEnergy, 1);
	double year1_TOD3JunEnergy = m_cf.at(CF_TOD3JunEnergy, 1);
	double year1_TOD3JulEnergy = m_cf.at(CF_TOD3JulEnergy, 1);
	double year1_TOD3AugEnergy = m_cf.at(CF_TOD3AugEnergy, 1);
	double year1_TOD3SepEnergy = m_cf.at(CF_TOD3SepEnergy, 1);
	double year1_TOD3OctEnergy = m_cf.at(CF_TOD3OctEnergy, 1);
	double year1_TOD3NovEnergy = m_cf.at(CF_TOD3NovEnergy, 1);
	double year1_TOD3DecEnergy = m_cf.at(CF_TOD3DecEnergy, 1);

	double year1_TOD4JanEnergy = m_cf.at(CF_TOD4JanEnergy, 1);
	double year1_TOD4FebEnergy = m_cf.at(CF_TOD4FebEnergy, 1);
	double year1_TOD4MarEnergy = m_cf.at(CF_TOD4MarEnergy, 1);
	double year1_TOD4AprEnergy = m_cf.at(CF_TOD4AprEnergy, 1);
	double year1_TOD4MayEnergy = m_cf.at(CF_TOD4MayEnergy, 1);
	double year1_TOD4JunEnergy = m_cf.at(CF_TOD4JunEnergy, 1);
	double year1_TOD4JulEnergy = m_cf.at(CF_TOD4JulEnergy, 1);
	double year1_TOD4AugEnergy = m_cf.at(CF_TOD4AugEnergy, 1);
	double year1_TOD4SepEnergy = m_cf.at(CF_TOD4SepEnergy, 1);
	double year1_TOD4OctEnergy = m_cf.at(CF_TOD4OctEnergy, 1);
	double year1_TOD4NovEnergy = m_cf.at(CF_TOD4NovEnergy, 1);
	double year1_TOD4DecEnergy = m_cf.at(CF_TOD4DecEnergy, 1);

	double year1_TOD5JanEnergy = m_cf.at(CF_TOD5JanEnergy, 1);
	double year1_TOD5FebEnergy = m_cf.at(CF_TOD5FebEnergy, 1);
	double year1_TOD5MarEnergy = m_cf.at(CF_TOD5MarEnergy, 1);
	double year1_TOD5AprEnergy = m_cf.at(CF_TOD5AprEnergy, 1);
	double year1_TOD5MayEnergy = m_cf.at(CF_TOD5MayEnergy, 1);
	double year1_TOD5JunEnergy = m_cf.at(CF_TOD5JunEnergy, 1);
	double year1_TOD5JulEnergy = m_cf.at(CF_TOD5JulEnergy, 1);
	double year1_TOD5AugEnergy = m_cf.at(CF_TOD5AugEnergy, 1);
	double year1_TOD5SepEnergy = m_cf.at(CF_TOD5SepEnergy, 1);
	double year1_TOD5OctEnergy = m_cf.at(CF_TOD5OctEnergy, 1);
	double year1_TOD5NovEnergy = m_cf.at(CF_TOD5NovEnergy, 1);
	double year1_TOD5DecEnergy = m_cf.at(CF_TOD5DecEnergy, 1);

	double year1_TOD6JanEnergy = m_cf.at(CF_TOD6JanEnergy, 1);
	double year1_TOD6FebEnergy = m_cf.at(CF_TOD6FebEnergy, 1);
	double year1_TOD6MarEnergy = m_cf.at(CF_TOD6MarEnergy, 1);
	double year1_TOD6AprEnergy = m_cf.at(CF_TOD6AprEnergy, 1);
	double year1_TOD6MayEnergy = m_cf.at(CF_TOD6MayEnergy, 1);
	double year1_TOD6JunEnergy = m_cf.at(CF_TOD6JunEnergy, 1);
	double year1_TOD6JulEnergy = m_cf.at(CF_TOD6JulEnergy, 1);
	double year1_TOD6AugEnergy = m_cf.at(CF_TOD6AugEnergy, 1);
	double year1_TOD6SepEnergy = m_cf.at(CF_TOD6SepEnergy, 1);
	double year1_TOD6OctEnergy = m_cf.at(CF_TOD6OctEnergy, 1);
	double year1_TOD6NovEnergy = m_cf.at(CF_TOD6NovEnergy, 1);
	double year1_TOD6DecEnergy = m_cf.at(CF_TOD6DecEnergy, 1);

	double year1_TOD7JanEnergy = m_cf.at(CF_TOD7JanEnergy, 1);
	double year1_TOD7FebEnergy = m_cf.at(CF_TOD7FebEnergy, 1);
	double year1_TOD7MarEnergy = m_cf.at(CF_TOD7MarEnergy, 1);
	double year1_TOD7AprEnergy = m_cf.at(CF_TOD7AprEnergy, 1);
	double year1_TOD7MayEnergy = m_cf.at(CF_TOD7MayEnergy, 1);
	double year1_TOD7JunEnergy = m_cf.at(CF_TOD7JunEnergy, 1);
	double year1_TOD7JulEnergy = m_cf.at(CF_TOD7JulEnergy, 1);
	double year1_TOD7AugEnergy = m_cf.at(CF_TOD7AugEnergy, 1);
	double year1_TOD7SepEnergy = m_cf.at(CF_TOD7SepEnergy, 1);
	double year1_TOD7OctEnergy = m_cf.at(CF_TOD7OctEnergy, 1);
	double year1_TOD7NovEnergy = m_cf.at(CF_TOD7NovEnergy, 1);
	double year1_TOD7DecEnergy = m_cf.at(CF_TOD7DecEnergy, 1);

	double year1_TOD8JanEnergy = m_cf.at(CF_TOD8JanEnergy, 1);
	double year1_TOD8FebEnergy = m_cf.at(CF_TOD8FebEnergy, 1);
	double year1_TOD8MarEnergy = m_cf.at(CF_TOD8MarEnergy, 1);
	double year1_TOD8AprEnergy = m_cf.at(CF_TOD8AprEnergy, 1);
	double year1_TOD8MayEnergy = m_cf.at(CF_TOD8MayEnergy, 1);
	double year1_TOD8JunEnergy = m_cf.at(CF_TOD8JunEnergy, 1);
	double year1_TOD8JulEnergy = m_cf.at(CF_TOD8JulEnergy, 1);
	double year1_TOD8AugEnergy = m_cf.at(CF_TOD8AugEnergy, 1);
	double year1_TOD8SepEnergy = m_cf.at(CF_TOD8SepEnergy, 1);
	double year1_TOD8OctEnergy = m_cf.at(CF_TOD8OctEnergy, 1);
	double year1_TOD8NovEnergy = m_cf.at(CF_TOD8NovEnergy, 1);
	double year1_TOD8DecEnergy = m_cf.at(CF_TOD8DecEnergy, 1);

	double year1_TOD9JanEnergy = m_cf.at(CF_TOD9JanEnergy, 1);
	double year1_TOD9FebEnergy = m_cf.at(CF_TOD9FebEnergy, 1);
	double year1_TOD9MarEnergy = m_cf.at(CF_TOD9MarEnergy, 1);
	double year1_TOD9AprEnergy = m_cf.at(CF_TOD9AprEnergy, 1);
	double year1_TOD9MayEnergy = m_cf.at(CF_TOD9MayEnergy, 1);
	double year1_TOD9JunEnergy = m_cf.at(CF_TOD9JunEnergy, 1);
	double year1_TOD9JulEnergy = m_cf.at(CF_TOD9JulEnergy, 1);
	double year1_TOD9AugEnergy = m_cf.at(CF_TOD9AugEnergy, 1);
	double year1_TOD9SepEnergy = m_cf.at(CF_TOD9SepEnergy, 1);
	double year1_TOD9OctEnergy = m_cf.at(CF_TOD9OctEnergy, 1);
	double year1_TOD9NovEnergy = m_cf.at(CF_TOD9NovEnergy, 1);
	double year1_TOD9DecEnergy = m_cf.at(CF_TOD9DecEnergy, 1);

	for (int y = 0; y <= m_nyears; y++)
	{
		// compute energy dispatched
		m_cf.at(CF_TODJanEnergy, y) = year1_TODJanEnergy * m_degradation[y];
		m_cf.at(CF_TODFebEnergy, y) = year1_TODFebEnergy * m_degradation[y];
		m_cf.at(CF_TODMarEnergy, y) = year1_TODMarEnergy * m_degradation[y];
		m_cf.at(CF_TODAprEnergy, y) = year1_TODAprEnergy * m_degradation[y];
		m_cf.at(CF_TODMayEnergy, y) = year1_TODMayEnergy * m_degradation[y];
		m_cf.at(CF_TODJunEnergy, y) = year1_TODJunEnergy * m_degradation[y];
		m_cf.at(CF_TODJulEnergy, y) = year1_TODJulEnergy * m_degradation[y];
		m_cf.at(CF_TODAugEnergy, y) = year1_TODAugEnergy * m_degradation[y];
		m_cf.at(CF_TODSepEnergy, y) = year1_TODSepEnergy * m_degradation[y];
		m_cf.at(CF_TODOctEnergy, y) = year1_TODOctEnergy * m_degradation[y];
		m_cf.at(CF_TODNovEnergy, y) = year1_TODNovEnergy * m_degradation[y];
		m_cf.at(CF_TODDecEnergy, y) = year1_TODDecEnergy * m_degradation[y];

		m_cf.at(CF_TOD1JanEnergy, y) = year1_TOD1JanEnergy * m_degradation[y];
		m_cf.at(CF_TOD1FebEnergy, y) = year1_TOD1FebEnergy * m_degradation[y];
		m_cf.at(CF_TOD1MarEnergy, y) = year1_TOD1MarEnergy * m_degradation[y];
		m_cf.at(CF_TOD1AprEnergy, y) = year1_TOD1AprEnergy * m_degradation[y];
		m_cf.at(CF_TOD1MayEnergy, y) = year1_TOD1MayEnergy * m_degradation[y];
		m_cf.at(CF_TOD1JunEnergy, y) = year1_TOD1JunEnergy * m_degradation[y];
		m_cf.at(CF_TOD1JulEnergy, y) = year1_TOD1JulEnergy * m_degradation[y];
		m_cf.at(CF_TOD1AugEnergy, y) = year1_TOD1AugEnergy * m_degradation[y];
		m_cf.at(CF_TOD1SepEnergy, y) = year1_TOD1SepEnergy * m_degradation[y];
		m_cf.at(CF_TOD1OctEnergy, y) = year1_TOD1OctEnergy * m_degradation[y];
		m_cf.at(CF_TOD1NovEnergy, y) = year1_TOD1NovEnergy * m_degradation[y];
		m_cf.at(CF_TOD1DecEnergy, y) = year1_TOD1DecEnergy * m_degradation[y];

		m_cf.at(CF_TOD2JanEnergy, y) = year1_TOD2JanEnergy * m_degradation[y];
		m_cf.at(CF_TOD2FebEnergy, y) = year1_TOD2FebEnergy * m_degradation[y];
		m_cf.at(CF_TOD2MarEnergy, y) = year1_TOD2MarEnergy * m_degradation[y];
		m_cf.at(CF_TOD2AprEnergy, y) = year1_TOD2AprEnergy * m_degradation[y];
		m_cf.at(CF_TOD2MayEnergy, y) = year1_TOD2MayEnergy * m_degradation[y];
		m_cf.at(CF_TOD2JunEnergy, y) = year1_TOD2JunEnergy * m_degradation[y];
		m_cf.at(CF_TOD2JulEnergy, y) = year1_TOD2JulEnergy * m_degradation[y];
		m_cf.at(CF_TOD2AugEnergy, y) = year1_TOD2AugEnergy * m_degradation[y];
		m_cf.at(CF_TOD2SepEnergy, y) = year1_TOD2SepEnergy * m_degradation[y];
		m_cf.at(CF_TOD2OctEnergy, y) = year1_TOD2OctEnergy * m_degradation[y];
		m_cf.at(CF_TOD2NovEnergy, y) = year1_TOD2NovEnergy * m_degradation[y];
		m_cf.at(CF_TOD2DecEnergy, y) = year1_TOD2DecEnergy * m_degradation[y];

		m_cf.at(CF_TOD3JanEnergy, y) = year1_TOD3JanEnergy * m_degradation[y];
		m_cf.at(CF_TOD3FebEnergy, y) = year1_TOD3FebEnergy * m_degradation[y];
		m_cf.at(CF_TOD3MarEnergy, y) = year1_TOD3MarEnergy * m_degradation[y];
		m_cf.at(CF_TOD3AprEnergy, y) = year1_TOD3AprEnergy * m_degradation[y];
		m_cf.at(CF_TOD3MayEnergy, y) = year1_TOD3MayEnergy * m_degradation[y];
		m_cf.at(CF_TOD3JunEnergy, y) = year1_TOD3JunEnergy * m_degradation[y];
		m_cf.at(CF_TOD3JulEnergy, y) = year1_TOD3JulEnergy * m_degradation[y];
		m_cf.at(CF_TOD3AugEnergy, y) = year1_TOD3AugEnergy * m_degradation[y];
		m_cf.at(CF_TOD3SepEnergy, y) = year1_TOD3SepEnergy * m_degradation[y];
		m_cf.at(CF_TOD3OctEnergy, y) = year1_TOD3OctEnergy * m_degradation[y];
		m_cf.at(CF_TOD3NovEnergy, y) = year1_TOD3NovEnergy * m_degradation[y];
		m_cf.at(CF_TOD3DecEnergy, y) = year1_TOD3DecEnergy * m_degradation[y];

		m_cf.at(CF_TOD4JanEnergy, y) = year1_TOD4JanEnergy * m_degradation[y];
		m_cf.at(CF_TOD4FebEnergy, y) = year1_TOD4FebEnergy * m_degradation[y];
		m_cf.at(CF_TOD4MarEnergy, y) = year1_TOD4MarEnergy * m_degradation[y];
		m_cf.at(CF_TOD4AprEnergy, y) = year1_TOD4AprEnergy * m_degradation[y];
		m_cf.at(CF_TOD4MayEnergy, y) = year1_TOD4MayEnergy * m_degradation[y];
		m_cf.at(CF_TOD4JunEnergy, y) = year1_TOD4JunEnergy * m_degradation[y];
		m_cf.at(CF_TOD4JulEnergy, y) = year1_TOD4JulEnergy * m_degradation[y];
		m_cf.at(CF_TOD4AugEnergy, y) = year1_TOD4AugEnergy * m_degradation[y];
		m_cf.at(CF_TOD4SepEnergy, y) = year1_TOD4SepEnergy * m_degradation[y];
		m_cf.at(CF_TOD4OctEnergy, y) = year1_TOD4OctEnergy * m_degradation[y];
		m_cf.at(CF_TOD4NovEnergy, y) = year1_TOD4NovEnergy * m_degradation[y];
		m_cf.at(CF_TOD4DecEnergy, y) = year1_TOD4DecEnergy * m_degradation[y];

		m_cf.at(CF_TOD5JanEnergy, y) = year1_TOD5JanEnergy * m_degradation[y];
		m_cf.at(CF_TOD5FebEnergy, y) = year1_TOD5FebEnergy * m_degradation[y];
		m_cf.at(CF_TOD5MarEnergy, y) = year1_TOD5MarEnergy * m_degradation[y];
		m_cf.at(CF_TOD5AprEnergy, y) = year1_TOD5AprEnergy * m_degradation[y];
		m_cf.at(CF_TOD5MayEnergy, y) = year1_TOD5MayEnergy * m_degradation[y];
		m_cf.at(CF_TOD5JunEnergy, y) = year1_TOD5JunEnergy * m_degradation[y];
		m_cf.at(CF_TOD5JulEnergy, y) = year1_TOD5JulEnergy * m_degradation[y];
		m_cf.at(CF_TOD5AugEnergy, y) = year1_TOD5AugEnergy * m_degradation[y];
		m_cf.at(CF_TOD5SepEnergy, y) = year1_TOD5SepEnergy * m_degradation[y];
		m_cf.at(CF_TOD5OctEnergy, y) = year1_TOD5OctEnergy * m_degradation[y];
		m_cf.at(CF_TOD5NovEnergy, y) = year1_TOD5NovEnergy * m_degradation[y];
		m_cf.at(CF_TOD5DecEnergy, y) = year1_TOD5DecEnergy * m_degradation[y];

		m_cf.at(CF_TOD6JanEnergy, y) = year1_TOD6JanEnergy * m_degradation[y];
		m_cf.at(CF_TOD6FebEnergy, y) = year1_TOD6FebEnergy * m_degradation[y];
		m_cf.at(CF_TOD6MarEnergy, y) = year1_TOD6MarEnergy * m_degradation[y];
		m_cf.at(CF_TOD6AprEnergy, y) = year1_TOD6AprEnergy * m_degradation[y];
		m_cf.at(CF_TOD6MayEnergy, y) = year1_TOD6MayEnergy * m_degradation[y];
		m_cf.at(CF_TOD6JunEnergy, y) = year1_TOD6JunEnergy * m_degradation[y];
		m_cf.at(CF_TOD6JulEnergy, y) = year1_TOD6JulEnergy * m_degradation[y];
		m_cf.at(CF_TOD6AugEnergy, y) = year1_TOD6AugEnergy * m_degradation[y];
		m_cf.at(CF_TOD6SepEnergy, y) = year1_TOD6SepEnergy * m_degradation[y];
		m_cf.at(CF_TOD6OctEnergy, y) = year1_TOD6OctEnergy * m_degradation[y];
		m_cf.at(CF_TOD6NovEnergy, y) = year1_TOD6NovEnergy * m_degradation[y];
		m_cf.at(CF_TOD6DecEnergy, y) = year1_TOD6DecEnergy * m_degradation[y];

		m_cf.at(CF_TOD7JanEnergy, y) = year1_TOD7JanEnergy * m_degradation[y];
		m_cf.at(CF_TOD7FebEnergy, y) = year1_TOD7FebEnergy * m_degradation[y];
		m_cf.at(CF_TOD7MarEnergy, y) = year1_TOD7MarEnergy * m_degradation[y];
		m_cf.at(CF_TOD7AprEnergy, y) = year1_TOD7AprEnergy * m_degradation[y];
		m_cf.at(CF_TOD7MayEnergy, y) = year1_TOD7MayEnergy * m_degradation[y];
		m_cf.at(CF_TOD7JunEnergy, y) = year1_TOD7JunEnergy * m_degradation[y];
		m_cf.at(CF_TOD7JulEnergy, y) = year1_TOD7JulEnergy * m_degradation[y];
		m_cf.at(CF_TOD7AugEnergy, y) = year1_TOD7AugEnergy * m_degradation[y];
		m_cf.at(CF_TOD7SepEnergy, y) = year1_TOD7SepEnergy * m_degradation[y];
		m_cf.at(CF_TOD7OctEnergy, y) = year1_TOD7OctEnergy * m_degradation[y];
		m_cf.at(CF_TOD7NovEnergy, y) = year1_TOD7NovEnergy * m_degradation[y];
		m_cf.at(CF_TOD7DecEnergy, y) = year1_TOD7DecEnergy * m_degradation[y];

		m_cf.at(CF_TOD8JanEnergy, y) = year1_TOD8JanEnergy * m_degradation[y];
		m_cf.at(CF_TOD8FebEnergy, y) = year1_TOD8FebEnergy * m_degradation[y];
		m_cf.at(CF_TOD8MarEnergy, y) = year1_TOD8MarEnergy * m_degradation[y];
		m_cf.at(CF_TOD8AprEnergy, y) = year1_TOD8AprEnergy * m_degradation[y];
		m_cf.at(CF_TOD8MayEnergy, y) = year1_TOD8MayEnergy * m_degradation[y];
		m_cf.at(CF_TOD8JunEnergy, y) = year1_TOD8JunEnergy * m_degradation[y];
		m_cf.at(CF_TOD8JulEnergy, y) = year1_TOD8JulEnergy * m_degradation[y];
		m_cf.at(CF_TOD8AugEnergy, y) = year1_TOD8AugEnergy * m_degradation[y];
		m_cf.at(CF_TOD8SepEnergy, y) = year1_TOD8SepEnergy * m_degradation[y];
		m_cf.at(CF_TOD8OctEnergy, y) = year1_TOD8OctEnergy * m_degradation[y];
		m_cf.at(CF_TOD8NovEnergy, y) = year1_TOD8NovEnergy * m_degradation[y];
		m_cf.at(CF_TOD8DecEnergy, y) = year1_TOD8DecEnergy * m_degradation[y];

		m_cf.at(CF_TOD9JanEnergy, y) = year1_TOD9JanEnergy * m_degradation[y];
		m_cf.at(CF_TOD9FebEnergy, y) = year1_TOD9FebEnergy * m_degradation[y];
		m_cf.at(CF_TOD9MarEnergy, y) = year1_TOD9MarEnergy * m_degradation[y];
		m_cf.at(CF_TOD9AprEnergy, y) = year1_TOD9AprEnergy * m_degradation[y];
		m_cf.at(CF_TOD9MayEnergy, y) = year1_TOD9MayEnergy * m_degradation[y];
		m_cf.at(CF_TOD9JunEnergy, y) = year1_TOD9JunEnergy * m_degradation[y];
		m_cf.at(CF_TOD9JulEnergy, y) = year1_TOD9JulEnergy * m_degradation[y];
		m_cf.at(CF_TOD9AugEnergy, y) = year1_TOD9AugEnergy * m_degradation[y];
		m_cf.at(CF_TOD9SepEnergy, y) = year1_TOD9SepEnergy * m_degradation[y];
		m_cf.at(CF_TOD9OctEnergy, y) = year1_TOD9OctEnergy * m_degradation[y];
		m_cf.at(CF_TOD9NovEnergy, y) = year1_TOD9NovEnergy * m_degradation[y];
		m_cf.at(CF_TOD9DecEnergy, y) = year1_TOD9DecEnergy * m_degradation[y];
	}
	return true;
}


bool dispatch_calculations::compute_dispatch_output_ts()
{
	//Calculate energy dispatched in each month

	size_t nrec_gen_per_year = m_ngen;
//	if (m_cm->as_integer("system_use_lifetime_output") == 1)
//		nrec_gen_per_year = m_ngen / m_nyears;
	size_t step_per_hour_gen = nrec_gen_per_year / 8760;
	if (step_per_hour_gen < 1 || step_per_hour_gen > 60 || step_per_hour_gen * 8760 != nrec_gen_per_year)
	{
		m_error = util::format("invalid number of gen records (%d): must be an integer multiple of 8760", (int)nrec_gen_per_year);
		throw exec_error("dispatch_calculations", m_error);
		return false;
	}
	if (m_nmultipliers != nrec_gen_per_year)
	{
		m_error = util::format("invalid number of gen records per year (%d) must be equal to number of ppa multiplier records (%d)", (int)nrec_gen_per_year, (int)m_nmultipliers);
		throw exec_error("dispatch_calculations", m_error);
		return false;
	}
	ssc_number_t ts_hour_gen = 1.0f / step_per_hour_gen;


	m_cf.at(CF_TODJanEnergy, 1) = 0;
	m_cf.at(CF_TODFebEnergy, 1) = 0;
	m_cf.at(CF_TODMarEnergy, 1) = 0;
	m_cf.at(CF_TODAprEnergy, 1) = 0;
	m_cf.at(CF_TODMayEnergy, 1) = 0;
	m_cf.at(CF_TODJunEnergy, 1) = 0;
	m_cf.at(CF_TODJulEnergy, 1) = 0;
	m_cf.at(CF_TODAugEnergy, 1) = 0;
	m_cf.at(CF_TODSepEnergy, 1) = 0;
	m_cf.at(CF_TODOctEnergy, 1) = 0;
	m_cf.at(CF_TODNovEnergy, 1) = 0;
	m_cf.at(CF_TODDecEnergy, 1) = 0;

	m_cf.at(CF_TODJanRevenue, 1) = 0;
	m_cf.at(CF_TODFebRevenue, 1) = 0;
	m_cf.at(CF_TODMarRevenue, 1) = 0;
	m_cf.at(CF_TODAprRevenue, 1) = 0;
	m_cf.at(CF_TODMayRevenue, 1) = 0;
	m_cf.at(CF_TODJunRevenue, 1) = 0;
	m_cf.at(CF_TODJulRevenue, 1) = 0;
	m_cf.at(CF_TODAugRevenue, 1) = 0;
	m_cf.at(CF_TODSepRevenue, 1) = 0;
	m_cf.at(CF_TODOctRevenue, 1) = 0;
	m_cf.at(CF_TODNovRevenue, 1) = 0;
	m_cf.at(CF_TODDecRevenue, 1) = 0;

	int i = 0;
	for (int m = 0; m<12; m++)
	{
		for (size_t d = 0; d<util::nday[m]; d++)
		{
			for (int h = 0; h<24 && i<(int)nrec_gen_per_year; h++)
			{
				for (int k = 0; k < (int)step_per_hour_gen; k++)
				{
					switch (m)
					{
					case 0:
						m_cf.at(CF_TODJanEnergy, 1) += m_gen[i] * ts_hour_gen;
						m_cf.at(CF_TODJanRevenue, 1) += m_gen[i] * ts_hour_gen * m_multipliers[i];
						break;
					case 1:
						m_cf.at(CF_TODFebEnergy, 1) += m_gen[i] * ts_hour_gen;
						m_cf.at(CF_TODFebRevenue, 1) += m_gen[i] * ts_hour_gen* m_multipliers[i];
						break;
					case 2:
						m_cf.at(CF_TODMarEnergy, 1) += m_gen[i] * ts_hour_gen;
						m_cf.at(CF_TODMarRevenue, 1) += m_gen[i] * ts_hour_gen * m_multipliers[i];
						break;
					case 3:
						m_cf.at(CF_TODAprEnergy, 1) += m_gen[i] * ts_hour_gen;
						m_cf.at(CF_TODAprRevenue, 1) += m_gen[i] * ts_hour_gen * m_multipliers[i];
						break;
					case 4:
						m_cf.at(CF_TODMayEnergy, 1) += m_gen[i] * ts_hour_gen;
						m_cf.at(CF_TODMayRevenue, 1) += m_gen[i] * ts_hour_gen * m_multipliers[i];
						break;
					case 5:
						m_cf.at(CF_TODJunEnergy, 1) += m_gen[i] * ts_hour_gen;
						m_cf.at(CF_TODJunRevenue, 1) += m_gen[i] * ts_hour_gen* m_multipliers[i];
						break;
					case 6:
						m_cf.at(CF_TODJulEnergy, 1) += m_gen[i] * ts_hour_gen;
						m_cf.at(CF_TODJulRevenue, 1) += m_gen[i] * ts_hour_gen * m_multipliers[i];
						break;
					case 7:
						m_cf.at(CF_TODAugEnergy, 1) += m_gen[i] * ts_hour_gen;
						m_cf.at(CF_TODAugRevenue, 1) += m_gen[i] * ts_hour_gen * m_multipliers[i];
						break;
					case 8:
						m_cf.at(CF_TODSepEnergy, 1) += m_gen[i] * ts_hour_gen;
						m_cf.at(CF_TODSepRevenue, 1) += m_gen[i] * ts_hour_gen * m_multipliers[i];
						break;
					case 9:
						m_cf.at(CF_TODOctEnergy, 1) += m_gen[i] * ts_hour_gen;
						m_cf.at(CF_TODOctRevenue, 1) += m_gen[i] * ts_hour_gen * m_multipliers[i];
						break;
					case 10:
						m_cf.at(CF_TODNovEnergy, 1) += m_gen[i] * ts_hour_gen;
						m_cf.at(CF_TODNovRevenue, 1) += m_gen[i] * ts_hour_gen * m_multipliers[i];
						break;
					case 11:
						m_cf.at(CF_TODDecEnergy, 1) += m_gen[i] * ts_hour_gen;
						m_cf.at(CF_TODDecRevenue, 1) += m_gen[i] * ts_hour_gen * m_multipliers[i];
						break;
					}
					i++;
				}
			}
		}
	}

	double year1_TODJanEnergy = m_cf.at(CF_TODJanEnergy, 1);
	double year1_TODFebEnergy = m_cf.at(CF_TODFebEnergy, 1);
	double year1_TODMarEnergy = m_cf.at(CF_TODMarEnergy, 1);
	double year1_TODAprEnergy = m_cf.at(CF_TODAprEnergy, 1);
	double year1_TODMayEnergy = m_cf.at(CF_TODMayEnergy, 1);
	double year1_TODJunEnergy = m_cf.at(CF_TODJunEnergy, 1);
	double year1_TODJulEnergy = m_cf.at(CF_TODJulEnergy, 1);
	double year1_TODAugEnergy = m_cf.at(CF_TODAugEnergy, 1);
	double year1_TODSepEnergy = m_cf.at(CF_TODSepEnergy, 1);
	double year1_TODOctEnergy = m_cf.at(CF_TODOctEnergy, 1);
	double year1_TODNovEnergy = m_cf.at(CF_TODNovEnergy, 1);
	double year1_TODDecEnergy = m_cf.at(CF_TODDecEnergy, 1);

	double year1_TODJanRevenue = m_cf.at(CF_TODJanRevenue, 1);
	double year1_TODFebRevenue = m_cf.at(CF_TODFebRevenue, 1);
	double year1_TODMarRevenue = m_cf.at(CF_TODMarRevenue, 1);
	double year1_TODAprRevenue = m_cf.at(CF_TODAprRevenue, 1);
	double year1_TODMayRevenue = m_cf.at(CF_TODMayRevenue, 1);
	double year1_TODJunRevenue = m_cf.at(CF_TODJunRevenue, 1);
	double year1_TODJulRevenue = m_cf.at(CF_TODJulRevenue, 1);
	double year1_TODAugRevenue = m_cf.at(CF_TODAugRevenue, 1);
	double year1_TODSepRevenue = m_cf.at(CF_TODSepRevenue, 1);
	double year1_TODOctRevenue = m_cf.at(CF_TODOctRevenue, 1);
	double year1_TODNovRevenue = m_cf.at(CF_TODNovRevenue, 1);
	double year1_TODDecRevenue = m_cf.at(CF_TODDecRevenue, 1);

	for (int y = 0; y <= m_nyears; y++)
	{
		// compute energy dispatched
		m_cf.at(CF_TODJanEnergy, y) = year1_TODJanEnergy * m_degradation[y];
		m_cf.at(CF_TODFebEnergy, y) = year1_TODFebEnergy * m_degradation[y];
		m_cf.at(CF_TODMarEnergy, y) = year1_TODMarEnergy * m_degradation[y];
		m_cf.at(CF_TODAprEnergy, y) = year1_TODAprEnergy * m_degradation[y];
		m_cf.at(CF_TODMayEnergy, y) = year1_TODMayEnergy * m_degradation[y];
		m_cf.at(CF_TODJunEnergy, y) = year1_TODJunEnergy * m_degradation[y];
		m_cf.at(CF_TODJulEnergy, y) = year1_TODJulEnergy * m_degradation[y];
		m_cf.at(CF_TODAugEnergy, y) = year1_TODAugEnergy * m_degradation[y];
		m_cf.at(CF_TODSepEnergy, y) = year1_TODSepEnergy * m_degradation[y];
		m_cf.at(CF_TODOctEnergy, y) = year1_TODOctEnergy * m_degradation[y];
		m_cf.at(CF_TODNovEnergy, y) = year1_TODNovEnergy * m_degradation[y];
		m_cf.at(CF_TODDecEnergy, y) = year1_TODDecEnergy * m_degradation[y];
		// compute energy value
		m_cf.at(CF_TODJanRevenue, y) = year1_TODJanRevenue * m_degradation[y];
		m_cf.at(CF_TODFebRevenue, y) = year1_TODFebRevenue * m_degradation[y];
		m_cf.at(CF_TODMarRevenue, y) = year1_TODMarRevenue * m_degradation[y];
		m_cf.at(CF_TODAprRevenue, y) = year1_TODAprRevenue * m_degradation[y];
		m_cf.at(CF_TODMayRevenue, y) = year1_TODMayRevenue * m_degradation[y];
		m_cf.at(CF_TODJunRevenue, y) = year1_TODJunRevenue * m_degradation[y];
		m_cf.at(CF_TODJulRevenue, y) = year1_TODJulRevenue * m_degradation[y];
		m_cf.at(CF_TODAugRevenue, y) = year1_TODAugRevenue * m_degradation[y];
		m_cf.at(CF_TODSepRevenue, y) = year1_TODSepRevenue * m_degradation[y];
		m_cf.at(CF_TODOctRevenue, y) = year1_TODOctRevenue * m_degradation[y];
		m_cf.at(CF_TODNovRevenue, y) = year1_TODNovRevenue * m_degradation[y];
		m_cf.at(CF_TODDecRevenue, y) = year1_TODDecRevenue * m_degradation[y];
	}
	return true;
}

bool dispatch_calculations::compute_lifetime_dispatch_output_ts()
{
	//Calculate energy dispatched in each month
	// assumption is that gen contains all avaialbility and curtailment
	size_t nrec_gen_per_year = m_ngen;
	if (m_cm->as_integer("system_use_lifetime_output") == 1) // should be true always
		nrec_gen_per_year = m_ngen / m_nyears;
	size_t step_per_hour_gen = nrec_gen_per_year / 8760;
	if (step_per_hour_gen < 1 || step_per_hour_gen > 60 || step_per_hour_gen * 8760 != nrec_gen_per_year)
	{
		m_error = util::format("invalid number of gen records (%d): must be an integer multiple of 8760", (int)nrec_gen_per_year);
		throw exec_error("dispatch_calculations", m_error);
		return false;
	}
	if (m_nmultipliers != nrec_gen_per_year)
	{
		m_error = util::format("invalid number of gen records per year (%d) must be equal to number of ppa multiplier records (%d)", (int)nrec_gen_per_year, (int)m_nmultipliers);
		throw exec_error("dispatch_calculations", m_error);
		return false;
	}
	ssc_number_t ts_hour_gen = 1.0f / step_per_hour_gen;

	for (int iyear = 0; iyear < m_nyears; iyear++)
	{
		m_cf.at(CF_TODJanEnergy, iyear + 1) = 0;
		m_cf.at(CF_TODFebEnergy, iyear + 1) = 0;
		m_cf.at(CF_TODMarEnergy, iyear + 1) = 0;
		m_cf.at(CF_TODAprEnergy, iyear + 1) = 0;
		m_cf.at(CF_TODMayEnergy, iyear + 1) = 0;
		m_cf.at(CF_TODJunEnergy, iyear + 1) = 0;
		m_cf.at(CF_TODJulEnergy, iyear + 1) = 0;
		m_cf.at(CF_TODAugEnergy, iyear + 1) = 0;
		m_cf.at(CF_TODSepEnergy, iyear + 1) = 0;
		m_cf.at(CF_TODOctEnergy, iyear + 1) = 0;
		m_cf.at(CF_TODNovEnergy, iyear + 1) = 0;
		m_cf.at(CF_TODDecEnergy, iyear + 1) = 0;

		m_cf.at(CF_TODJanRevenue, iyear + 1) = 0;
		m_cf.at(CF_TODFebRevenue, iyear + 1) = 0;
		m_cf.at(CF_TODMarRevenue, iyear + 1) = 0;
		m_cf.at(CF_TODAprRevenue, iyear + 1) = 0;
		m_cf.at(CF_TODMayRevenue, iyear + 1) = 0;
		m_cf.at(CF_TODJunRevenue, iyear + 1) = 0;
		m_cf.at(CF_TODJulRevenue, iyear + 1) = 0;
		m_cf.at(CF_TODAugRevenue, iyear + 1) = 0;
		m_cf.at(CF_TODSepRevenue, iyear + 1) = 0;
		m_cf.at(CF_TODOctRevenue, iyear + 1) = 0;
		m_cf.at(CF_TODNovRevenue, iyear + 1) = 0;
		m_cf.at(CF_TODDecRevenue, iyear + 1) = 0;

		int i = 0; // iterator for current year

		for (int m = 0; m < 12; m++)
		{
			for (size_t d = 0; d < util::nday[m]; d++)
			{
				for (int h = 0; h < 24 && i < (int)nrec_gen_per_year; h++)
				{
					for (int k = 0; k < (int)step_per_hour_gen; k++)
					{
						switch (m)
						{
						case 0:
							m_cf.at(CF_TODJanEnergy, iyear + 1) += m_gen[i + (iyear * nrec_gen_per_year)] * ts_hour_gen;
							m_cf.at(CF_TODJanRevenue, iyear + 1) += m_gen[i + (iyear * nrec_gen_per_year)] * ts_hour_gen * m_multipliers[i];
							break;
						case 1:
							m_cf.at(CF_TODFebEnergy, iyear + 1) += m_gen[i + (iyear * nrec_gen_per_year)] * ts_hour_gen;
							m_cf.at(CF_TODFebRevenue, iyear + 1) += m_gen[i + (iyear * nrec_gen_per_year)] * ts_hour_gen* m_multipliers[i];
							break;
						case 2:
							m_cf.at(CF_TODMarEnergy, iyear + 1) += m_gen[i + (iyear * nrec_gen_per_year)] * ts_hour_gen;
							m_cf.at(CF_TODMarRevenue, iyear + 1) += m_gen[i + (iyear * nrec_gen_per_year)] * ts_hour_gen * m_multipliers[i];
							break;
						case 3:
							m_cf.at(CF_TODAprEnergy, iyear + 1) += m_gen[i + (iyear * nrec_gen_per_year)] * ts_hour_gen;
							m_cf.at(CF_TODAprRevenue, iyear + 1) += m_gen[i + (iyear * nrec_gen_per_year)] * ts_hour_gen * m_multipliers[i];
							break;
						case 4:
							m_cf.at(CF_TODMayEnergy, iyear + 1) += m_gen[i + (iyear * nrec_gen_per_year)] * ts_hour_gen;
							m_cf.at(CF_TODMayRevenue, iyear + 1) += m_gen[i + (iyear * nrec_gen_per_year)] * ts_hour_gen * m_multipliers[i];
							break;
						case 5:
							m_cf.at(CF_TODJunEnergy, iyear + 1) += m_gen[i + (iyear * nrec_gen_per_year)] * ts_hour_gen;
							m_cf.at(CF_TODJunRevenue, iyear + 1) += m_gen[i + (iyear * nrec_gen_per_year)] * ts_hour_gen* m_multipliers[i];
							break;
						case 6:
							m_cf.at(CF_TODJulEnergy, iyear + 1) += m_gen[i + (iyear * nrec_gen_per_year)] * ts_hour_gen;
							m_cf.at(CF_TODJulRevenue, iyear + 1) += m_gen[i + (iyear * nrec_gen_per_year)] * ts_hour_gen * m_multipliers[i];
							break;
						case 7:
							m_cf.at(CF_TODAugEnergy, iyear + 1) += m_gen[i + (iyear * nrec_gen_per_year)] * ts_hour_gen;
							m_cf.at(CF_TODAugRevenue, iyear + 1) += m_gen[i + (iyear * nrec_gen_per_year)] * ts_hour_gen * m_multipliers[i];
							break;
						case 8:
							m_cf.at(CF_TODSepEnergy, iyear + 1) += m_gen[i + (iyear * nrec_gen_per_year)] * ts_hour_gen;
							m_cf.at(CF_TODSepRevenue, iyear + 1) += m_gen[i + (iyear * nrec_gen_per_year)] * ts_hour_gen * m_multipliers[i];
							break;
						case 9:
							m_cf.at(CF_TODOctEnergy, iyear + 1) += m_gen[i + (iyear * nrec_gen_per_year)] * ts_hour_gen;
							m_cf.at(CF_TODOctRevenue, iyear + 1) += m_gen[i + (iyear * nrec_gen_per_year)] * ts_hour_gen * m_multipliers[i];
							break;
						case 10:
							m_cf.at(CF_TODNovEnergy, iyear + 1) += m_gen[i + (iyear * nrec_gen_per_year)] * ts_hour_gen;
							m_cf.at(CF_TODNovRevenue, iyear + 1) += m_gen[i + (iyear * nrec_gen_per_year)] * ts_hour_gen * m_multipliers[i];
							break;
						case 11:
							m_cf.at(CF_TODDecEnergy, iyear + 1) += m_gen[i + (iyear * nrec_gen_per_year)] * ts_hour_gen;
							m_cf.at(CF_TODDecRevenue, iyear + 1) += m_gen[i + (iyear * nrec_gen_per_year)] * ts_hour_gen * m_multipliers[i];
							break;
						}
						i++;
					}  // steps per hour
				} // hours per day
			} // days per month
		} // months per year
	} // years per analysis period
	return true;
}


bool dispatch_calculations::compute_lifetime_dispatch_output()
{
	//Calculate energy dispatched in each dispatch period 


	int h;
	size_t count=m_hourly_energy.size();

	// hourly energy includes all curtailment, availability
	if ((int)count != (8760 * m_nyears))
	{
		std::stringstream outm;
		outm << "Bad hourly gen output length (" << count << "), should be (analysis period-1) * 8760 value (" << 8760 * m_nyears << ")";
		m_cm->log(outm.str());
		return false;
	}

	//// hourly dispatch
	//dispatch_calculations hourly_dispatch(this);
	//if (!hourly_dispatch.setup())
	//	throw exec_error("ippppa", "failed to setup dispatch periods: " + hourly_dispatch.error());

	for (int y = 1; y <= m_nyears; y++)
	{
		m_cf.at(CF_TOD1Energy, y) = 0;
		m_cf.at(CF_TOD2Energy, y) = 0;
		m_cf.at(CF_TOD3Energy, y) = 0;
		m_cf.at(CF_TOD4Energy, y) = 0;
		m_cf.at(CF_TOD5Energy, y) = 0;
		m_cf.at(CF_TOD6Energy, y) = 0;
		m_cf.at(CF_TOD7Energy, y) = 0;
		m_cf.at(CF_TOD8Energy, y) = 0;
		m_cf.at(CF_TOD9Energy, y) = 0;

		for (h = 0; h<8760; h++)
		{
			switch (m_periods[h])
			{
			case 1:
				m_cf.at(CF_TOD1Energy, y) += m_hourly_energy[(y - 1) * 8760 + h];
				break;
			case 2:
				m_cf.at(CF_TOD2Energy, y) += m_hourly_energy[(y - 1) * 8760 + h];
				break;
			case 3:
				m_cf.at(CF_TOD3Energy, y) += m_hourly_energy[(y - 1) * 8760 + h];
				break;
			case 4:
				m_cf.at(CF_TOD4Energy, y) += m_hourly_energy[(y - 1) * 8760 + h];
				break;
			case 5:
				m_cf.at(CF_TOD5Energy, y) += m_hourly_energy[(y - 1) * 8760 + h];
				break;
			case 6:
				m_cf.at(CF_TOD6Energy, y) += m_hourly_energy[(y - 1) * 8760 + h];
				break;
			case 7:
				m_cf.at(CF_TOD7Energy, y) += m_hourly_energy[(y - 1) * 8760 + h];
				break;
			case 8:
				m_cf.at(CF_TOD8Energy, y) += m_hourly_energy[(y - 1) * 8760 + h];
				break;
			case 9:
				m_cf.at(CF_TOD9Energy, y) += m_hourly_energy[(y - 1) * 8760 + h];
				break;
			}
		}
	}


	return true;
}

bool dispatch_calculations::process_lifetime_dispatch_output()
{
	//Calculate energy dispatched in each dispatch period 

	size_t count=m_hourly_energy.size();

	// hourly energy include all curtailment, availability 
	if ((int)count != (8760 * m_nyears))
	{
		std::stringstream outm;
		outm << "Bad hourly gen output length (" << count << "), should be (analysis period-1) * 8760 value (" << 8760 * m_nyears << ")";
		m_cm->log(outm.str());
		return false;
	}

	//// hourly dispatch
	//dispatch_calculations hourly_dispatch(this);
	//if (!hourly_dispatch.setup())
	//	throw exec_error("ippppa", "failed to setup dispatch periods: " + hourly_dispatch.error());


	for (int y = 1; y <= m_nyears; y++)
	{
		m_cf.at(CF_TODJanEnergy, y) = 0;
		m_cf.at(CF_TODFebEnergy, y) = 0;
		m_cf.at(CF_TODMarEnergy, y) = 0;
		m_cf.at(CF_TODAprEnergy, y) = 0;
		m_cf.at(CF_TODMayEnergy, y) = 0;
		m_cf.at(CF_TODJunEnergy, y) = 0;
		m_cf.at(CF_TODJulEnergy, y) = 0;
		m_cf.at(CF_TODAugEnergy, y) = 0;
		m_cf.at(CF_TODSepEnergy, y) = 0;
		m_cf.at(CF_TODOctEnergy, y) = 0;
		m_cf.at(CF_TODNovEnergy, y) = 0;
		m_cf.at(CF_TODDecEnergy, y) = 0;

		m_cf.at(CF_TOD1JanEnergy, y) = 0;
		m_cf.at(CF_TOD1FebEnergy, y) = 0;
		m_cf.at(CF_TOD1MarEnergy, y) = 0;
		m_cf.at(CF_TOD1AprEnergy, y) = 0;
		m_cf.at(CF_TOD1MayEnergy, y) = 0;
		m_cf.at(CF_TOD1JunEnergy, y) = 0;
		m_cf.at(CF_TOD1JulEnergy, y) = 0;
		m_cf.at(CF_TOD1AugEnergy, y) = 0;
		m_cf.at(CF_TOD1SepEnergy, y) = 0;
		m_cf.at(CF_TOD1OctEnergy, y) = 0;
		m_cf.at(CF_TOD1NovEnergy, y) = 0;
		m_cf.at(CF_TOD1DecEnergy, y) = 0;

		m_cf.at(CF_TOD2JanEnergy, y) = 0;
		m_cf.at(CF_TOD2FebEnergy, y) = 0;
		m_cf.at(CF_TOD2MarEnergy, y) = 0;
		m_cf.at(CF_TOD2AprEnergy, y) = 0;
		m_cf.at(CF_TOD2MayEnergy, y) = 0;
		m_cf.at(CF_TOD2JunEnergy, y) = 0;
		m_cf.at(CF_TOD2JulEnergy, y) = 0;
		m_cf.at(CF_TOD2AugEnergy, y) = 0;
		m_cf.at(CF_TOD2SepEnergy, y) = 0;
		m_cf.at(CF_TOD2OctEnergy, y) = 0;
		m_cf.at(CF_TOD2NovEnergy, y) = 0;
		m_cf.at(CF_TOD2DecEnergy, y) = 0;

		m_cf.at(CF_TOD3JanEnergy, y) = 0;
		m_cf.at(CF_TOD3FebEnergy, y) = 0;
		m_cf.at(CF_TOD3MarEnergy, y) = 0;
		m_cf.at(CF_TOD3AprEnergy, y) = 0;
		m_cf.at(CF_TOD3MayEnergy, y) = 0;
		m_cf.at(CF_TOD3JunEnergy, y) = 0;
		m_cf.at(CF_TOD3JulEnergy, y) = 0;
		m_cf.at(CF_TOD3AugEnergy, y) = 0;
		m_cf.at(CF_TOD3SepEnergy, y) = 0;
		m_cf.at(CF_TOD3OctEnergy, y) = 0;
		m_cf.at(CF_TOD3NovEnergy, y) = 0;
		m_cf.at(CF_TOD3DecEnergy, y) = 0;

		m_cf.at(CF_TOD4JanEnergy, y) = 0;
		m_cf.at(CF_TOD4FebEnergy, y) = 0;
		m_cf.at(CF_TOD4MarEnergy, y) = 0;
		m_cf.at(CF_TOD4AprEnergy, y) = 0;
		m_cf.at(CF_TOD4MayEnergy, y) = 0;
		m_cf.at(CF_TOD4JunEnergy, y) = 0;
		m_cf.at(CF_TOD4JulEnergy, y) = 0;
		m_cf.at(CF_TOD4AugEnergy, y) = 0;
		m_cf.at(CF_TOD4SepEnergy, y) = 0;
		m_cf.at(CF_TOD4OctEnergy, y) = 0;
		m_cf.at(CF_TOD4NovEnergy, y) = 0;
		m_cf.at(CF_TOD4DecEnergy, y) = 0;

		m_cf.at(CF_TOD5JanEnergy, y) = 0;
		m_cf.at(CF_TOD5FebEnergy, y) = 0;
		m_cf.at(CF_TOD5MarEnergy, y) = 0;
		m_cf.at(CF_TOD5AprEnergy, y) = 0;
		m_cf.at(CF_TOD5MayEnergy, y) = 0;
		m_cf.at(CF_TOD5JunEnergy, y) = 0;
		m_cf.at(CF_TOD5JulEnergy, y) = 0;
		m_cf.at(CF_TOD5AugEnergy, y) = 0;
		m_cf.at(CF_TOD5SepEnergy, y) = 0;
		m_cf.at(CF_TOD5OctEnergy, y) = 0;
		m_cf.at(CF_TOD5NovEnergy, y) = 0;
		m_cf.at(CF_TOD5DecEnergy, y) = 0;

		m_cf.at(CF_TOD6JanEnergy, y) = 0;
		m_cf.at(CF_TOD6FebEnergy, y) = 0;
		m_cf.at(CF_TOD6MarEnergy, y) = 0;
		m_cf.at(CF_TOD6AprEnergy, y) = 0;
		m_cf.at(CF_TOD6MayEnergy, y) = 0;
		m_cf.at(CF_TOD6JunEnergy, y) = 0;
		m_cf.at(CF_TOD6JulEnergy, y) = 0;
		m_cf.at(CF_TOD6AugEnergy, y) = 0;
		m_cf.at(CF_TOD6SepEnergy, y) = 0;
		m_cf.at(CF_TOD6OctEnergy, y) = 0;
		m_cf.at(CF_TOD6NovEnergy, y) = 0;
		m_cf.at(CF_TOD6DecEnergy, y) = 0;

		m_cf.at(CF_TOD7JanEnergy, y) = 0;
		m_cf.at(CF_TOD7FebEnergy, y) = 0;
		m_cf.at(CF_TOD7MarEnergy, y) = 0;
		m_cf.at(CF_TOD7AprEnergy, y) = 0;
		m_cf.at(CF_TOD7MayEnergy, y) = 0;
		m_cf.at(CF_TOD7JunEnergy, y) = 0;
		m_cf.at(CF_TOD7JulEnergy, y) = 0;
		m_cf.at(CF_TOD7AugEnergy, y) = 0;
		m_cf.at(CF_TOD7SepEnergy, y) = 0;
		m_cf.at(CF_TOD7OctEnergy, y) = 0;
		m_cf.at(CF_TOD7NovEnergy, y) = 0;
		m_cf.at(CF_TOD7DecEnergy, y) = 0;

		m_cf.at(CF_TOD8JanEnergy, y) = 0;
		m_cf.at(CF_TOD8FebEnergy, y) = 0;
		m_cf.at(CF_TOD8MarEnergy, y) = 0;
		m_cf.at(CF_TOD8AprEnergy, y) = 0;
		m_cf.at(CF_TOD8MayEnergy, y) = 0;
		m_cf.at(CF_TOD8JunEnergy, y) = 0;
		m_cf.at(CF_TOD8JulEnergy, y) = 0;
		m_cf.at(CF_TOD8AugEnergy, y) = 0;
		m_cf.at(CF_TOD8SepEnergy, y) = 0;
		m_cf.at(CF_TOD8OctEnergy, y) = 0;
		m_cf.at(CF_TOD8NovEnergy, y) = 0;
		m_cf.at(CF_TOD8DecEnergy, y) = 0;

		m_cf.at(CF_TOD9JanEnergy, y) = 0;
		m_cf.at(CF_TOD9FebEnergy, y) = 0;
		m_cf.at(CF_TOD9MarEnergy, y) = 0;
		m_cf.at(CF_TOD9AprEnergy, y) = 0;
		m_cf.at(CF_TOD9MayEnergy, y) = 0;
		m_cf.at(CF_TOD9JunEnergy, y) = 0;
		m_cf.at(CF_TOD9JulEnergy, y) = 0;
		m_cf.at(CF_TOD9AugEnergy, y) = 0;
		m_cf.at(CF_TOD9SepEnergy, y) = 0;
		m_cf.at(CF_TOD9OctEnergy, y) = 0;
		m_cf.at(CF_TOD9NovEnergy, y) = 0;
		m_cf.at(CF_TOD9DecEnergy, y) = 0;

		int i = 0;
		for (int m = 0; m<12; m++)
		{
			for (size_t d = 0; d<util::nday[m]; d++)
			{
				for (int h = 0; h<24 && i<8760 && m * 24 + h<288; h++)
				{
					switch (m)
					{
					case 0:
						m_cf.at(CF_TODJanEnergy, y) += m_hourly_energy[(y - 1) * 8760 + i];
						switch (m_periods[i])
						{
						case 1:
							m_cf.at(CF_TOD1JanEnergy, y) += m_hourly_energy[(y - 1) * 8760 + i];
							break;
						case 2:
							m_cf.at(CF_TOD2JanEnergy, y) += m_hourly_energy[(y - 1) * 8760 + i];
							break;
						case 3:
							m_cf.at(CF_TOD3JanEnergy, y) += m_hourly_energy[(y - 1) * 8760 + i];
							break;
						case 4:
							m_cf.at(CF_TOD4JanEnergy, y) += m_hourly_energy[(y - 1) * 8760 + i];
							break;
						case 5:
							m_cf.at(CF_TOD5JanEnergy, y) += m_hourly_energy[(y - 1) * 8760 + i];
							break;
						case 6:
							m_cf.at(CF_TOD6JanEnergy, y) += m_hourly_energy[(y - 1) * 8760 + i];
							break;
						case 7:
							m_cf.at(CF_TOD7JanEnergy, y) += m_hourly_energy[(y - 1) * 8760 + i];
							break;
						case 8:
							m_cf.at(CF_TOD8JanEnergy, y) += m_hourly_energy[(y - 1) * 8760 + i];
							break;
						case 9:
							m_cf.at(CF_TOD9JanEnergy, y) += m_hourly_energy[(y - 1) * 8760 + i];
							break;
						}
						break;
					case 1:
						m_cf.at(CF_TODFebEnergy, y) += m_hourly_energy[(y - 1) * 8760 + i];
						switch (m_periods[i])
						{
						case 1:
							m_cf.at(CF_TOD1FebEnergy, y) += m_hourly_energy[(y - 1) * 8760 + i];
							break;
						case 2:
							m_cf.at(CF_TOD2FebEnergy, y) += m_hourly_energy[(y - 1) * 8760 + i];
							break;
						case 3:
							m_cf.at(CF_TOD3FebEnergy, y) += m_hourly_energy[(y - 1) * 8760 + i];
							break;
						case 4:
							m_cf.at(CF_TOD4FebEnergy, y) += m_hourly_energy[(y - 1) * 8760 + i];
							break;
						case 5:
							m_cf.at(CF_TOD5FebEnergy, y) += m_hourly_energy[(y - 1) * 8760 + i];
							break;
						case 6:
							m_cf.at(CF_TOD6FebEnergy, y) += m_hourly_energy[(y - 1) * 8760 + i];
							break;
						case 7:
							m_cf.at(CF_TOD7FebEnergy, y) += m_hourly_energy[(y - 1) * 8760 + i];
							break;
						case 8:
							m_cf.at(CF_TOD8FebEnergy, y) += m_hourly_energy[(y - 1) * 8760 + i];
							break;
						case 9:
							m_cf.at(CF_TOD9FebEnergy, y) += m_hourly_energy[(y - 1) * 8760 + i];
							break;
						}
						break;
					case 2:
						m_cf.at(CF_TODMarEnergy, y) += m_hourly_energy[(y - 1) * 8760 + i];
						switch (m_periods[i])
						{
						case 1:
							m_cf.at(CF_TOD1MarEnergy, y) += m_hourly_energy[(y - 1) * 8760 + i];
							break;
						case 2:
							m_cf.at(CF_TOD2MarEnergy, y) += m_hourly_energy[(y - 1) * 8760 + i];
							break;
						case 3:
							m_cf.at(CF_TOD3MarEnergy, y) += m_hourly_energy[(y - 1) * 8760 + i];
							break;
						case 4:
							m_cf.at(CF_TOD4MarEnergy, y) += m_hourly_energy[(y - 1) * 8760 + i];
							break;
						case 5:
							m_cf.at(CF_TOD5MarEnergy, y) += m_hourly_energy[(y - 1) * 8760 + i];
							break;
						case 6:
							m_cf.at(CF_TOD6MarEnergy, y) += m_hourly_energy[(y - 1) * 8760 + i];
							break;
						case 7:
							m_cf.at(CF_TOD7MarEnergy, y) += m_hourly_energy[(y - 1) * 8760 + i];
							break;
						case 8:
							m_cf.at(CF_TOD8MarEnergy, y) += m_hourly_energy[(y - 1) * 8760 + i];
							break;
						case 9:
							m_cf.at(CF_TOD9MarEnergy, y) += m_hourly_energy[(y - 1) * 8760 + i];
							break;
						}
						break;
					case 3:
						m_cf.at(CF_TODAprEnergy, y) += m_hourly_energy[(y - 1) * 8760 + i];
						switch (m_periods[i])
						{
						case 1:
							m_cf.at(CF_TOD1AprEnergy, y) += m_hourly_energy[(y - 1) * 8760 + i];
							break;
						case 2:
							m_cf.at(CF_TOD2AprEnergy, y) += m_hourly_energy[(y - 1) * 8760 + i];
							break;
						case 3:
							m_cf.at(CF_TOD3AprEnergy, y) += m_hourly_energy[(y - 1) * 8760 + i];
							break;
						case 4:
							m_cf.at(CF_TOD4AprEnergy, y) += m_hourly_energy[(y - 1) * 8760 + i];
							break;
						case 5:
							m_cf.at(CF_TOD5AprEnergy, y) += m_hourly_energy[(y - 1) * 8760 + i];
							break;
						case 6:
							m_cf.at(CF_TOD6AprEnergy, y) += m_hourly_energy[(y - 1) * 8760 + i];
							break;
						case 7:
							m_cf.at(CF_TOD7AprEnergy, y) += m_hourly_energy[(y - 1) * 8760 + i];
							break;
						case 8:
							m_cf.at(CF_TOD8AprEnergy, y) += m_hourly_energy[(y - 1) * 8760 + i];
							break;
						case 9:
							m_cf.at(CF_TOD9AprEnergy, y) += m_hourly_energy[(y - 1) * 8760 + i];
							break;
						}
						break;
					case 4:
						m_cf.at(CF_TODMayEnergy, y) += m_hourly_energy[(y - 1) * 8760 + i];
						switch (m_periods[i])
						{
						case 1:
							m_cf.at(CF_TOD1MayEnergy, y) += m_hourly_energy[(y - 1) * 8760 + i];
							break;
						case 2:
							m_cf.at(CF_TOD2MayEnergy, y) += m_hourly_energy[(y - 1) * 8760 + i];
							break;
						case 3:
							m_cf.at(CF_TOD3MayEnergy, y) += m_hourly_energy[(y - 1) * 8760 + i];
							break;
						case 4:
							m_cf.at(CF_TOD4MayEnergy, y) += m_hourly_energy[(y - 1) * 8760 + i];
							break;
						case 5:
							m_cf.at(CF_TOD5MayEnergy, y) += m_hourly_energy[(y - 1) * 8760 + i];
							break;
						case 6:
							m_cf.at(CF_TOD6MayEnergy, y) += m_hourly_energy[(y - 1) * 8760 + i];
							break;
						case 7:
							m_cf.at(CF_TOD7MayEnergy, y) += m_hourly_energy[(y - 1) * 8760 + i];
							break;
						case 8:
							m_cf.at(CF_TOD8MayEnergy, y) += m_hourly_energy[(y - 1) * 8760 + i];
							break;
						case 9:
							m_cf.at(CF_TOD9MayEnergy, y) += m_hourly_energy[(y - 1) * 8760 + i];
							break;
						}
						break;
					case 5:
						m_cf.at(CF_TODJunEnergy, y) += m_hourly_energy[(y - 1) * 8760 + i];
						switch (m_periods[i])
						{
						case 1:
							m_cf.at(CF_TOD1JunEnergy, y) += m_hourly_energy[(y - 1) * 8760 + i];
							break;
						case 2:
							m_cf.at(CF_TOD2JunEnergy, y) += m_hourly_energy[(y - 1) * 8760 + i];
							break;
						case 3:
							m_cf.at(CF_TOD3JunEnergy, y) += m_hourly_energy[(y - 1) * 8760 + i];
							break;
						case 4:
							m_cf.at(CF_TOD4JunEnergy, y) += m_hourly_energy[(y - 1) * 8760 + i];
							break;
						case 5:
							m_cf.at(CF_TOD5JunEnergy, y) += m_hourly_energy[(y - 1) * 8760 + i];
							break;
						case 6:
							m_cf.at(CF_TOD6JunEnergy, y) += m_hourly_energy[(y - 1) * 8760 + i];
							break;
						case 7:
							m_cf.at(CF_TOD7JunEnergy, y) += m_hourly_energy[(y - 1) * 8760 + i];
							break;
						case 8:
							m_cf.at(CF_TOD8JunEnergy, y) += m_hourly_energy[(y - 1) * 8760 + i];
							break;
						case 9:
							m_cf.at(CF_TOD9JunEnergy, y) += m_hourly_energy[(y - 1) * 8760 + i];
							break;
						}
						break;
					case 6:
						m_cf.at(CF_TODJulEnergy, y) += m_hourly_energy[(y - 1) * 8760 + i];
						switch (m_periods[i])
						{
						case 1:
							m_cf.at(CF_TOD1JulEnergy, y) += m_hourly_energy[(y - 1) * 8760 + i];
							break;
						case 2:
							m_cf.at(CF_TOD2JulEnergy, y) += m_hourly_energy[(y - 1) * 8760 + i];
							break;
						case 3:
							m_cf.at(CF_TOD3JulEnergy, y) += m_hourly_energy[(y - 1) * 8760 + i];
							break;
						case 4:
							m_cf.at(CF_TOD4JulEnergy, y) += m_hourly_energy[(y - 1) * 8760 + i];
							break;
						case 5:
							m_cf.at(CF_TOD5JulEnergy, y) += m_hourly_energy[(y - 1) * 8760 + i];
							break;
						case 6:
							m_cf.at(CF_TOD6JulEnergy, y) += m_hourly_energy[(y - 1) * 8760 + i];
							break;
						case 7:
							m_cf.at(CF_TOD7JulEnergy, y) += m_hourly_energy[(y - 1) * 8760 + i];
							break;
						case 8:
							m_cf.at(CF_TOD8JulEnergy, y) += m_hourly_energy[(y - 1) * 8760 + i];
							break;
						case 9:
							m_cf.at(CF_TOD9JulEnergy, y) += m_hourly_energy[(y - 1) * 8760 + i];
							break;
						}
						break;
					case 7:
						m_cf.at(CF_TODAugEnergy, y) += m_hourly_energy[(y - 1) * 8760 + i];
						switch (m_periods[i])
						{
						case 1:
							m_cf.at(CF_TOD1AugEnergy, y) += m_hourly_energy[(y - 1) * 8760 + i];
							break;
						case 2:
							m_cf.at(CF_TOD2AugEnergy, y) += m_hourly_energy[(y - 1) * 8760 + i];
							break;
						case 3:
							m_cf.at(CF_TOD3AugEnergy, y) += m_hourly_energy[(y - 1) * 8760 + i];
							break;
						case 4:
							m_cf.at(CF_TOD4AugEnergy, y) += m_hourly_energy[(y - 1) * 8760 + i];
							break;
						case 5:
							m_cf.at(CF_TOD5AugEnergy, y) += m_hourly_energy[(y - 1) * 8760 + i];
							break;
						case 6:
							m_cf.at(CF_TOD6AugEnergy, y) += m_hourly_energy[(y - 1) * 8760 + i];
							break;
						case 7:
							m_cf.at(CF_TOD7AugEnergy, y) += m_hourly_energy[(y - 1) * 8760 + i];
							break;
						case 8:
							m_cf.at(CF_TOD8AugEnergy, y) += m_hourly_energy[(y - 1) * 8760 + i];
							break;
						case 9:
							m_cf.at(CF_TOD9AugEnergy, y) += m_hourly_energy[(y - 1) * 8760 + i];
							break;
						}
						break;
					case 8:
						m_cf.at(CF_TODSepEnergy, y) += m_hourly_energy[(y - 1) * 8760 + i];
						switch (m_periods[i])
						{
						case 1:
							m_cf.at(CF_TOD1SepEnergy, y) += m_hourly_energy[(y - 1) * 8760 + i];
							break;
						case 2:
							m_cf.at(CF_TOD2SepEnergy, y) += m_hourly_energy[(y - 1) * 8760 + i];
							break;
						case 3:
							m_cf.at(CF_TOD3SepEnergy, y) += m_hourly_energy[(y - 1) * 8760 + i];
							break;
						case 4:
							m_cf.at(CF_TOD4SepEnergy, y) += m_hourly_energy[(y - 1) * 8760 + i];
							break;
						case 5:
							m_cf.at(CF_TOD5SepEnergy, y) += m_hourly_energy[(y - 1) * 8760 + i];
							break;
						case 6:
							m_cf.at(CF_TOD6SepEnergy, y) += m_hourly_energy[(y - 1) * 8760 + i];
							break;
						case 7:
							m_cf.at(CF_TOD7SepEnergy, y) += m_hourly_energy[(y - 1) * 8760 + i];
							break;
						case 8:
							m_cf.at(CF_TOD8SepEnergy, y) += m_hourly_energy[(y - 1) * 8760 + i];
							break;
						case 9:
							m_cf.at(CF_TOD9SepEnergy, y) += m_hourly_energy[(y - 1) * 8760 + i];
							break;
						}
						break;
					case 9:
						m_cf.at(CF_TODOctEnergy, y) += m_hourly_energy[(y - 1) * 8760 + i];
						switch (m_periods[i])
						{
						case 1:
							m_cf.at(CF_TOD1OctEnergy, y) += m_hourly_energy[(y - 1) * 8760 + i];
							break;
						case 2:
							m_cf.at(CF_TOD2OctEnergy, y) += m_hourly_energy[(y - 1) * 8760 + i];
							break;
						case 3:
							m_cf.at(CF_TOD3OctEnergy, y) += m_hourly_energy[(y - 1) * 8760 + i];
							break;
						case 4:
							m_cf.at(CF_TOD4OctEnergy, y) += m_hourly_energy[(y - 1) * 8760 + i];
							break;
						case 5:
							m_cf.at(CF_TOD5OctEnergy, y) += m_hourly_energy[(y - 1) * 8760 + i];
							break;
						case 6:
							m_cf.at(CF_TOD6OctEnergy, y) += m_hourly_energy[(y - 1) * 8760 + i];
							break;
						case 7:
							m_cf.at(CF_TOD7OctEnergy, y) += m_hourly_energy[(y - 1) * 8760 + i];
							break;
						case 8:
							m_cf.at(CF_TOD8OctEnergy, y) += m_hourly_energy[(y - 1) * 8760 + i];
							break;
						case 9:
							m_cf.at(CF_TOD9OctEnergy, y) += m_hourly_energy[(y - 1) * 8760 + i];
							break;
						}
						break;
					case 10:
						m_cf.at(CF_TODNovEnergy, y) += m_hourly_energy[(y - 1) * 8760 + i];
						switch (m_periods[i])
						{
						case 1:
							m_cf.at(CF_TOD1NovEnergy, y) += m_hourly_energy[(y - 1) * 8760 + i];
							break;
						case 2:
							m_cf.at(CF_TOD2NovEnergy, y) += m_hourly_energy[(y - 1) * 8760 + i];
							break;
						case 3:
							m_cf.at(CF_TOD3NovEnergy, y) += m_hourly_energy[(y - 1) * 8760 + i];
							break;
						case 4:
							m_cf.at(CF_TOD4NovEnergy, y) += m_hourly_energy[(y - 1) * 8760 + i];
							break;
						case 5:
							m_cf.at(CF_TOD5NovEnergy, y) += m_hourly_energy[(y - 1) * 8760 + i];
							break;
						case 6:
							m_cf.at(CF_TOD6NovEnergy, y) += m_hourly_energy[(y - 1) * 8760 + i];
							break;
						case 7:
							m_cf.at(CF_TOD7NovEnergy, y) += m_hourly_energy[(y - 1) * 8760 + i];
							break;
						case 8:
							m_cf.at(CF_TOD8NovEnergy, y) += m_hourly_energy[(y - 1) * 8760 + i];
							break;
						case 9:
							m_cf.at(CF_TOD9NovEnergy, y) += m_hourly_energy[(y - 1) * 8760 + i];
							break;
						}
						break;
					case 11:
						m_cf.at(CF_TODDecEnergy, y) += m_hourly_energy[(y - 1) * 8760 + i];
						switch (m_periods[i])
						{
						case 1:
							m_cf.at(CF_TOD1DecEnergy, y) += m_hourly_energy[(y - 1) * 8760 + i];
							break;
						case 2:
							m_cf.at(CF_TOD2DecEnergy, y) += m_hourly_energy[(y - 1) * 8760 + i];
							break;
						case 3:
							m_cf.at(CF_TOD3DecEnergy, y) += m_hourly_energy[(y - 1) * 8760 + i];
							break;
						case 4:
							m_cf.at(CF_TOD4DecEnergy, y) += m_hourly_energy[(y - 1) * 8760 + i];
							break;
						case 5:
							m_cf.at(CF_TOD5DecEnergy, y) += m_hourly_energy[(y - 1) * 8760 + i];
							break;
						case 6:
							m_cf.at(CF_TOD6DecEnergy, y) += m_hourly_energy[(y - 1) * 8760 + i];
							break;
						case 7:
							m_cf.at(CF_TOD7DecEnergy, y) += m_hourly_energy[(y - 1) * 8760 + i];
							break;
						case 8:
							m_cf.at(CF_TOD8DecEnergy, y) += m_hourly_energy[(y - 1) * 8760 + i];
							break;
						case 9:
							m_cf.at(CF_TOD9DecEnergy, y) += m_hourly_energy[(y - 1) * 8760 + i];
							break;
						}
						break;
					}
					i++;
				}
			}
		}

	}

	return true;
}

/*   VARTYPE           DATATYPE         NAME                               LABEL                                       UNITS     META                                     GROUP                 REQUIRED_IF                 CONSTRAINTS                      UI_HINTS*/
/*
var_info vtab_advanced_financing_cost[] = {

{ SSC_INPUT, SSC_NUMBER, "total_direct_cost", "Total Direct Cost", "", "", "advanced_financing_costs", "*", "", "" },
{ SSC_OUTPUT, SSC_NUMBER, "direct_cost", "Total Direct Cost", "", "", "advanced_financing_cost", "*", "", "" },
{ SSC_OUTPUT, SSC_NUMBER, "indirect_cost", "Total Indirect Cost", "", "", "advanced_financing_cost", "*", "", "" },
{ SSC_OUTPUT, SSC_NUMBER, "financing_cost", "Total Financing Cost", "", "", "advanced_financing_cost", "*", "", "" },

	var_info_invalid };


advanced_financing_cost::advanced_financing_cost(compute_module *cm)
: m_cm(cm)
{
}


bool advanced_financing_cost::compute_cost(double cost_installed, double equity, double debt, double cbi, double ibi)
{
	double installed_cost = m_cm->as_double("total_installed_cost");
	double direct_cost = m_cm->as_double("total_direct_cost");
	double indirect_cost = installed_cost - direct_cost;
	double project_cost = cost_installed;

	m_cm->assign("direct_cost", var_data((ssc_number_t)direct_cost));
	m_cm->assign("indirect_cost", var_data((ssc_number_t)indirect_cost));
	m_cm->assign("financing_cost", var_data((ssc_number_t)(project_cost - indirect_cost - direct_cost)));

	// rounding on debt/equity
//	if (project_cost != (ibi + cbi + debt + equity))
//	{
		m_cm->assign("size_of_equity", var_data((ssc_number_t)(project_cost - ibi - cbi - debt)));
//	}
	return true;
}
*/

bool hourly_energy_calculation::calculate(compute_module *cm)
{
	if (!cm) return false;

	m_cm = cm;

	m_nyears = m_cm->as_integer("analysis_period");


	ssc_number_t *pgen;
    size_t nrec_gen = 0;
    m_step_per_hour_gen = 1;
	pgen = m_cm->as_array("gen", &nrec_gen);

	// in front of meter - account for charging and 
	size_t i;
	ssc_number_t *revenue_gen = m_cm->allocate("revenue_gen", nrec_gen);
	ssc_number_t *gen_purchases = m_cm->allocate("gen_purchases", nrec_gen);
		 
	// we do this so that grid energy purchased through the electricity rate is not inadvertently double counted as lost revenue
	for (i = 0; i < nrec_gen; i++) {
        gen_purchases[i] = std::min(pgen[i], 0.0);
		revenue_gen[i] = std::max(pgen[i], 0.0);
	}
	
	// for lifetime analysis
	size_t nrec_gen_per_year = nrec_gen;
	if (m_cm->as_integer("system_use_lifetime_output") == 1)
		nrec_gen_per_year = nrec_gen / m_nyears;
    m_step_per_hour_gen = nrec_gen_per_year / 8760;
	if (m_step_per_hour_gen < 1 || m_step_per_hour_gen > 60 || m_step_per_hour_gen * 8760 != nrec_gen_per_year)
	{
		m_error = util::format("invalid number of gen records (%d): must be an integer multiple of 8760", (int)nrec_gen_per_year);
		throw exec_error("hourly_energy_calculation", m_error);
	}
    m_ts_hour_gen = 1.0f / m_step_per_hour_gen;

	m_hourly_energy.clear();
    m_energy_sales.clear();
    m_energy_purchases.clear();
    m_energy_without_battery.clear();
 
	// assign hourly values for "gen"
    ssc_number_t* ppa_gen;
    // Choose which variable goes through the hourly PPA process. If electricity purchases are through a utility rate, use only the positive revenue (revenue_gen), otherwise use gen
    if (cm->is_assigned("en_electricity_rates") && cm->as_number("en_electricity_rates") == 1) {
        ppa_gen = revenue_gen;
    }
    else {
        ppa_gen = pgen;
    }

    sum_ts_to_hourly(ppa_gen, m_hourly_energy);
    sum_ts_to_hourly(revenue_gen, m_energy_sales);
    sum_ts_to_hourly(gen_purchases, m_energy_purchases);

    if (cm->is_assigned("gen_without_battery")) {
        ssc_number_t* gen_without_battery = m_cm->as_array("gen_without_battery", &nrec_gen);
        if (nrec_gen % 8760 == 0) {
            sum_ts_to_hourly(gen_without_battery, m_energy_without_battery);
        }
    }

	return true;
}

void hourly_energy_calculation::sum_ts_to_hourly(ssc_number_t* timestep_power, std::vector<double>& hourly)
{
    size_t idx = 0;
	ssc_number_t ts_power = 0;

    if (m_cm->as_integer("system_use_lifetime_output") == 1)
    {   // availability, curtailment and degradation included in lifetime output
        for (size_t y = 0; y < m_nyears; y++)
        {
            for (size_t i = 0; i < 8760; i++)
            {
                double hourly_energy = 0;
                for (size_t ii = 0; ii < m_step_per_hour_gen; ii++)
                {
                    ts_power = timestep_power[idx];
                    hourly_energy += ts_power * m_ts_hour_gen;
                    idx++;
                }
                hourly.push_back(hourly_energy);
            }
        }
        // check size
        if (hourly.size() != 8760 * m_nyears)
        {
            m_error = util::format("invalid number of hourly energy records (%d): must be %d", (int)hourly.size(), 8760 * m_nyears);
            throw exec_error("hourly_energy_calculation", m_error);
        }
    }
    else
    {
        for (size_t i = 0; i < 8760; i++)
        {
            double hourly_energy = 0;
            for (size_t ii = 0; ii < m_step_per_hour_gen; ii++)
            {
                ts_power = timestep_power[idx];
                hourly_energy += ts_power * m_ts_hour_gen;
                idx++;
            }
            hourly.push_back(hourly_energy);
        }
        // check size
        if (m_hourly_energy.size() != 8760)
        {
            m_error = util::format("invalid number of hourly energy records (%d): must be 8760", (int)hourly.size());
            throw exec_error("hourly_energy_calculation", m_error);
        }
    }
}
