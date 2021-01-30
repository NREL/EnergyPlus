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

#ifndef _CSP_COMMON_
#define _CSP_COMMON_ 1
#include <memory>

#include "core.h"
#include "AutoPilot_API.h"
#include "lib_weatherfile.h"

#include "sco2_pc_csp_int.h"

class solarpilot_invoke : public var_map
{
    compute_module *m_cmod;
    AutoPilot_S *m_sapi;
	std::vector<std::vector<double> > _optimization_sim_points;
	std::vector<double>
		_optimization_objectives,
		_optimization_fluxes;

public:

	void getOptimizationSimulationHistory(std::vector<std::vector<double> > &sim_points, std::vector<double> &obj_values, std::vector<double> &flux_values);
	void setOptimizationSimulationHistory(std::vector<std::vector<double> > &sim_points, std::vector<double> &obj_values, std::vector<double> &flux_values);

    //sp_optimize opt;
    /*sp_ambient amb;
    sp_cost cost;
    sp_heliostats helios;
    sp_receivers recs;*/
    sp_layout layout;
    sp_flux_table fluxtab;
    sp_layout_table heliotab;

    solarpilot_invoke( compute_module *cm );
    ~solarpilot_invoke();
    AutoPilot_S *GetSAPI();
    bool run(std::shared_ptr<weather_data_provider> wdata = nullptr);
    bool postsim_calcs( compute_module *cm );
};

bool are_values_sig_different(double v1, double v2, double tol);

bool ssc_cmod_solarpilot_callback(simulation_info *siminfo, void *data);

extern var_info vtab_sco2_design[];

int sco2_design_cmod_common(compute_module *cm, C_sco2_phx_air_cooler & c_sco2_cycle);





#endif