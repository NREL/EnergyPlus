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

#ifndef _INTEROP_
#define _INTEROP_ 1

#include <string>
#include <map>
#include <vector>
#include <stdio.h>

#include "string_util.h"
#include "mod_base.h"
#include "heliodata.h"
#include "definitions.h"
#ifdef SP_USE_SOLTRACE
#include "stapi.h"
#endif

class SolarField;
class Heliostat;
class FluxSurface;
struct ST_System;
typedef std::vector<FluxSurface> FluxSurfaces;
typedef std::vector<Heliostat*> Hvector;

/* 
NOTE

No references to GUI functions in these methods. These are callable by the GUI and by the performance
code, but should operate independently from the GUI. 

No references to WX library.
*/


class ArrayString
{
	std::vector<std::string> data;

public:
	ArrayString();
	
	ArrayString &operator=( std::vector<std::string> &arr );

	int size();
	std::string operator[](int i);
	std::string& at(int i);
	
	void clear();
	void Clear();

	void resize(int newsize);
	void push_back(std::string value);
	void Add(std::string value);

	std::string back();
	int Index(std::string item);

	std::vector<std::string>::iterator erase(std::vector<std::string>::iterator position);
	std::vector<std::string>::iterator begin();
};

struct par_variable
{
    std::string varname;	//The variable map name 
	std::string display_text;
	std::string units;
	std::string data_type;	//one of {"int", "double", "string", "bool", "combo", "checkbox", "location"}
	ArrayString selections;
	ArrayString choices;
	ArrayString sim_values;
	bool linked;
	bool layout_required;
	par_variable();

};


class multivar
{
protected:
	ArrayString weather_files;
	bool wf_are_set;  //disabled in base class
    std::vector<par_variable> variables;
	ArrayString current_varpaths;
public:
	multivar();
	void addVar(spbase *var);	//Add a variable by reference to its variable map object
	int size();
	void clear();
	par_variable &at(int index);
	par_variable &operator[](int index);
	par_variable &back();
	void remove(int index);
	int Index(std::string pathname);	//Returns the index if the pathname is found as a current variable. Otherwise returns -1.
};

class parametric : public multivar
{
public:

	parametric();
	void SetWeatherFileList(ArrayString &list);

};

class optimization : public multivar {};


class simulation_table
{
	unordered_map<std::string, ArrayString> data;

public:
	ArrayString &operator[](const std::string &varname);
	ArrayString &at(const std::string &varname);
	size_t nvar();
	size_t nsim();
	void getKeys(ArrayString &keys);
	void ClearAll();
};


namespace interop
{
	/* 
	Provides functions that calculate simulatin input values based on other specified data.

	E.g. - given a layout simulation method, calculate the days/hours of the year to simulate.

	Methods in this class can be called by the GUI or the (future) API to update variables
	within the var_set object.
	*/

	//-- Methods for calculating simulation input values
	void GenerateSimulationWeatherData(var_map &V, int design_method, ArrayString &wf_entries);
	void GenerateSimulationWeatherData(var_map &V, int design_method, std::vector<std::string> &wf_entries);	//overload
	bool parseRange(std::string &range, int &rangelow, int &rangehi, bool &include_low, bool &include_hi);
	void ticker_initialize(int indices[], int n);
	bool ticker_increment(int lengths[], int indices[], bool changed[], int n);

	//Simulation setup methods
	bool PerformanceSimulationPrep(SolarField &SF, Hvector &helios, int sim_method);
#ifdef SP_USE_SOLTRACE
	bool SolTraceFluxSimulation_ST(st_context_t cxt, SolarField &SF, Hvector &helios, Vect &sunvect,
                                int callback(st_uint_t ntracedtotal, st_uint_t ntraced, st_uint_t ntotrace, st_uint_t curstage, st_uint_t nstages, void *data),
                                void *par, 
                                std::vector<std::vector<double> > *st0data, std::vector<std::vector<double> > *st1data, bool save_stage_data, bool load_stage_data);
	bool SolTraceFluxSimulation_ST(st_context_t cxt, int seed, ST_System &ST, 
                                int callback(st_uint_t ntracedtotal, st_uint_t ntraced, st_uint_t ntotrace, st_uint_t curstage, st_uint_t nstages, void *data),
                                void *par, 
std::vector<std::vector<double> > *st0data, std::vector<std::vector<double> > *st1data, bool save_stage_data, bool load_stage_data);
#endif
	void UpdateMapLayoutData(var_map &V, Hvector *helios);
};


//Classes for collecting and processing simulation results
class stat_object
{
public:
	double
		min,
		max,
		ave,
		stdev,
		sum,
        wtmean;

	void set(double _min, double _max, double _ave, double _stdev, double _sum, double _wtmean);
	void initialize();

};

class sim_result
{
	double
		_q_coe;
public:
	sim_result();
	//Keep track of individual heliostat performance metrics
	unordered_map<int, helio_perf_data> data_by_helio; //< heliostat ID, performance metrics object>

	double
		total_heliostat_area,
		total_receiver_area,
		total_land_area,
		power_on_field,
		power_absorbed,
        power_thermal_loss, 
        power_piping_loss,
        power_to_htf,
		power_to_cycle,
		power_gross,
		power_net,
		dni,
		solar_az,
		solar_zen,
		total_installed_cost,
		coe_metric;

	//whole-field statistics
	stat_object
		eff_total_heliostat,
		eff_total_sf,
		eff_cosine,
		eff_attenuation,
		eff_blocking,
		eff_shading,
		eff_reflect,
		eff_intercept,
		eff_absorption,
		flux_density,
		eff_cloud;
	int
		sim_type,	/* 0 = layout, 1 = optimization, 2 = flux simulation, 3 = parametric */
		sim_id,
		num_heliostats_used,
		num_heliostats_avail,
        num_ray_traced,
        num_ray_heliostat,
        num_ray_receiver;
	bool is_soltrace;

	struct SIM_TYPE { enum A { LAYOUT, OPTIMIZATION, FLUX_SIMULATION, PARAMETRIC }; };
	
	std::vector<std::string> receiver_names;

	std::vector<FluxSurfaces> flux_surfaces;

	void initialize();

	void add_heliostat(Heliostat &H);

	void process_analytical_simulation(SolarField &SF, int nsim_type, double sun_az_zen[2], Hvector &helios);

	void process_analytical_simulation(SolarField &SF, int sim_type, double sun_az_zen[2]);

	void process_raytrace_simulation(SolarField &SF, int nsim_type, double sun_az_zen[2], Hvector &helios, double qray, int *emap, int *smap, int *rnum, int ntot, double *boxinfo);

	void process_flux(SolarField *SF, bool normalize);
	
	void process_field_stats();

	void process_flux_stats(SolarField &SF);
	
};

typedef std::vector<sim_result> sim_results;

#endif
