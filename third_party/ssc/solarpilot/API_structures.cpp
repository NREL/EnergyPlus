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

#include <stdio.h>

#include "API_structures.h"
#include "exceptions.hpp"
#include "definitions.h"

using namespace std;

// ----------------- optimize --------------------
void sp_optimize::getOptimizationSimulationHistory(vector<vector<double> > &sim_points, vector<double> &obj_values, vector<double> &flux_values)
{
	/* 
	Return the addresses of the optimization simulation history data, if applicable.
	*/
	sim_points = _optimization_sim_points;
	obj_values = _optimization_objectives;
	flux_values = _optimization_fluxes;
}

void sp_optimize::setOptimizationSimulationHistory(vector<vector<double> > &sim_points, vector<double> &obj_values, vector<double> &flux_values)
{
	//Create local copies
	_optimization_sim_points = sim_points;
	_optimization_objectives = obj_values;
	_optimization_fluxes = flux_values;
}

// ----------------- flux table --------------------
sp_flux_table::sp_flux_table()
{
	is_user_spacing = false;
}

// ----------------- optical table --------------------
sp_optical_table::sp_optical_table()
{
	is_user_positions = false;
}
