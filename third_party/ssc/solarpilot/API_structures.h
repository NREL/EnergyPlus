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

#ifndef _API_STRUCTURES_
#define _API_STRUCTURES_ 1
#include <vector>
#include <numeric>
#include <limits>

#include "mod_base.h"

struct sp_optimize 
{
private:
	std::vector<std::vector<double> > _optimization_sim_points;
	std::vector<double>
		_optimization_objectives,
		_optimization_fluxes;

public:

	void getOptimizationSimulationHistory(std::vector<std::vector<double> > &sim_points, std::vector<double> &obj_values, std::vector<double> &flux_values);
	void setOptimizationSimulationHistory(std::vector<std::vector<double> > &sim_points, std::vector<double> &obj_values, std::vector<double> &flux_values);
};

struct sp_layout
{

	struct h_position
	{
        struct { double x, y, z; } 
            location, 
            aimpoint;
		int template_number; //0 based
        struct {double i, j, k; } cant_vector;	//[optional] Canting aim std::vector of total magnitude equal to the cant radius
		double focal_length;	//[optional] Heliostat focal length
	};
	
    std::vector<h_position> heliostat_positions;
};

struct sp_optical_table 
{
	/* 
	Optical table stores whole-field optical efficiency as a function of 
	solar azimuth and zenith angles.
	*/
	sp_optical_table();
	bool is_user_positions;		//user will specify azimuths and zeniths
	std::vector<double> zeniths;
	std::vector<double> azimuths;
	std::vector<std::vector<double> > eff_data;
};

struct sp_flux_map
{
	struct sp_flux_stack
	{
		std::string map_name;
		std::vector<double> xpos;
		std::vector<double> ypos;
		block_t<double> flux_data;
	};
	std::vector<sp_flux_stack> flux_surfaces;  

};

struct sp_flux_table : sp_flux_map
{
	/* 
	Flux table stores flux maps for each receiver & receiver surface (if applicable) 
	for the annual set of sun azimuth and zenith angles. 
	*/
	
	sp_flux_table();

	bool is_user_spacing;	//user will specify data in 'n_flux_days' and 'delta_flux_hours'
	int n_flux_days;		//How many days are used to calculate flux maps? (default = 8)
	double delta_flux_hrs;		//How much time (hrs) between each flux map? (default = 1)
	//-- data calculated by the algorithm:
	
	std::vector<double> azimuths;
	std::vector<double> zeniths;
	std::vector<double> efficiency;
	//---
};

struct sp_layout_table 
{
	struct h_position
	{
        struct { double x, y, z; } 
            location, 
            aimpoint;
		int template_number; //0 based
		bool user_optics;	//indicate whether the user will provide a cant/focus std::vector
        struct {double i, j, k; } cant_vector;	//[optional] canting aim std::vector of total magnitude equal to the cant radius
		double focal_length;	//[optional] heliostat focal length
	};

    std::vector<sp_layout_table::h_position> positions;
};


#endif
