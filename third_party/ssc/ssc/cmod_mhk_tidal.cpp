/*******************************************************************************************************
*  Copyright 2017 Alliance for Sustainable Energy, LLC
*
*  NOTICE: This software was developed at least in part by Alliance for Sustainable Energy, LLC
*  (�Alliance�) under Contract No. DE-AC36-08GO28308 with the U.S. Department of Energy and the U.S.
*  The Government retains for itself and others acting on its behalf a nonexclusive, paid-up,
*  irrevocable worldwide license in the software to reproduce, prepare derivative works, distribute
*  copies to the public, perform publicly and display publicly, and to permit others to do so.
*  copies to the public, perform publicly and display publicly, and to permit others to do so.
*
*  Redistribution and use in source and binary forms, with or without modification, are permitted
*  provided that the following conditions are met:
*
*  1. Redistributions of source code must retain the above copyright notice, the above government
*  rights notice, this list of conditions and the following disclaimer.
*
*  2. Redistributions in binary form must reproduce the above copyright notice, the above government
*  rights notice, this list of conditions and the following disclaimer in the documentation and/or
*  other materials provided with the distribution.
*
*  3. The entire corresponding source code of any redistribution, with or without modification, by a
*  research entity, including but not limited to any contracting manager/operator of a United States
*  National Laboratory, any institution of higher learning, and any non-profit organization, must be
*  made publicly available under this license for as long as the redistribution is made available by
*  the research entity.
*
*  4. Redistribution of this software, without modification, must refer to the software by the same
*  designation. Redistribution of a modified version of this software (i) may not refer to the modified
*  version by the same designation, or by any confusingly similar designation, and (ii) must refer to
*  the underlying software originally provided by Alliance as �System Advisor Model� or �SAM�. Except
*  to comply with the foregoing, the terms �System Advisor Model�, �SAM�, or any confusingly similar
*  designation may not be used to refer to any modified version of this software or any modified
*  version of the underlying software originally provided by Alliance without the prior written consent
*  of Alliance.
*
*  5. The name of the copyright holder, contributors, the United States Government, the United States
*  Department of Energy, or any of their employees may not be used to endorse or promote products
*  derived from this software without specific prior written permission.
*
*  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR
*  IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND
*  FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER,
*  CONTRIBUTORS, UNITED STATES GOVERNMENT OR UNITED STATES DEPARTMENT OF ENERGY, NOR ANY OF THEIR
*  EMPLOYEES, BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
*  DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
*  DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER
*  IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF
*  THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*******************************************************************************************************/

#include "core.h"
#include "common.h"

static var_info _cm_vtab_mhk_tidal[] = {
	//   VARTYPE			DATATYPE			NAME									LABEL																		UNITS           META            GROUP              REQUIRED_IF					CONSTRAINTS				UI_HINTS	
	{ SSC_INPUT,			SSC_MATRIX,			"tidal_resource",					    "Frequency distribution of resource as a function of stream speeds",		"",				"",             "MHKTidal",			"*",						"",						"" },	
	{ SSC_INPUT,			SSC_MATRIX,			"tidal_power_curve",					"Power curve of tidal energy device as function of stream speeds",			"kW",			"",             "MHKTidal",			"*",						"",						"" },	
	//{ SSC_INPUT,			SSC_NUMBER,			"calculate_capacity",					"Calculate device rated capacity from power curve",							"0/1",			"",             "MHKTidal",         "?=1",                      "INTEGER,MIN=0,MAX=1",	"" },
	{ SSC_INPUT,			SSC_NUMBER,			"number_devices",						"Number of tidal devices in the system",									"",				"",             "MHKTidal",         "?=1",                      "INTEGER",				"" },



	// losses
	{ SSC_INPUT,			SSC_NUMBER,			"loss_array_spacing",				"Array spacing loss",													"%",			"",				"MHKTidal",			"*",		"",						"" },
	{ SSC_INPUT,			SSC_NUMBER,			"loss_resource_overprediction",				"Resource overprediction loss",													"%",			"",				"MHKTidal",			"*",		"",						"" },
	{ SSC_INPUT,			SSC_NUMBER,			"loss_transmission",				"Transmission losses",													"%",			"",				"MHKTidal",			"*",		"",						"" },
	{ SSC_INPUT,			SSC_NUMBER,			"loss_downtime",				"Array/WEC downtime loss",													"%",			"",				"MHKTidal",			"*",		"",						"" },
	{ SSC_INPUT,			SSC_NUMBER,			"loss_additional",				"Additional losses",													"%",			"",				"MHKTidal",			"*",		"",						"" },


//	{ SSC_OUTPUT,			SSC_NUMBER,			"device_rated_capacity",				"Rated capacity of device",													"kW",			"",				"MHKTidal",			"calculate_capacity=0",		"",						"" },
	{ SSC_OUTPUT,			SSC_NUMBER,			"device_rated_capacity",				"Rated capacity of device",													"kW",			"",				"MHKTidal",			"",		"",						"" },
	{ SSC_OUTPUT,			SSC_NUMBER,			"device_average_power",					"Average power production of a single device",								"kW",			"",				"MHKTidal",			"*",						"",						"" },
	{ SSC_OUTPUT,			SSC_NUMBER,			"annual_energy",						"Annual energy production of array",										"kWh",			"",				"MHKTidal",			"*",						"",						"" },
	{ SSC_OUTPUT,			SSC_NUMBER,			"capacity_factor",						"Capacity Factor of array",													"%",			"",				"MHKTidal",			"*",						"",						"" },
	{ SSC_OUTPUT,			SSC_ARRAY,			"annual_energy_distribution",			"Annual energy production of array as function of speed",					"kWh",			"",				"MHKTidal",			"*",						"",						"" },
	{ SSC_OUTPUT,			SSC_ARRAY,			"annual_cumulative_energy_distribution","Cumulative annual energy production of array as function of speed",		"kWh",			"",				"MHKTidal",			"*",						"",						"" },

	var_info_invalid
};


class cm_mhk_tidal : public compute_module
{
private:
public: 
	cm_mhk_tidal() {
		add_var_info(_cm_vtab_mhk_tidal);
	}
	
	void exec() {

	//Read and store tidal resource and power curve:
		util::matrix_t<double>  tidal_resource_matrix = as_matrix("tidal_resource");
		util::matrix_t<double>  tidal_power_curve = as_matrix("tidal_power_curve");
		
		//Check to ensure size of _power_vect == _speed_vect : 
		if ( tidal_power_curve.nrows() != tidal_resource_matrix.nrows() )
			throw exec_error("mhk_tidal", "Size of Power Curve is not equal to Tidal Resource");

		//Store the number of rows- this will have to change if resource and power curve can have different stream speeds
		int number_rows = (int)tidal_resource_matrix.nrows();

		//Check that the power matrix only has two columns
		if (tidal_power_curve.ncols() != (size_t)2)
			throw exec_error("mhk_tidal", "Power curve must contain two columns");

		//Check that the resource matrix has at least two columns
		if (tidal_power_curve.ncols() < (size_t)2)
			throw exec_error("mhk_tidal", "Resource matrix must have at least two columns");


		//Create vectors to store individual columns from the user input matrix "tidal_resource"
		//Size the vectors based on the resource matrix so that the length can change in the future
		std::vector<double> _speed_vect(number_rows);	// Stream speed (u [m/s])
		std::vector<double> _probability_vect(number_rows);	// Probability in decimals (i.e. 0.5, not 50%) at different depths***************************************************Need to program in the possibility of probabilities at multiple depths
		
		//Vector to store power curve of tidal energy conversion system from "tidal_power_curve":
		//Size the vector based on the tidal power matrix so that the length can be different from the resource in the future
		std::vector<double> _power_vect(number_rows);	//Tidal power curve (P [kW])

		
	//Initialize variables to store calculated values and outputs:
		ssc_number_t *p_annual_energy_dist = allocate("annual_energy_distribution", number_rows);
		ssc_number_t *p_annual_cumulative_energy_dist = allocate("annual_cumulative_energy_distribution", number_rows);
		double annual_energy = 0, device_average_power = 0, _probability_vect_checker = 0, capacity_factor = 0, device_rated_capacity = 0;
		
		//User either sets device_rated_capacity in the UI, or allows cmod to determine from power curve:
		if (is_assigned("device_rated_capacity"))
			device_rated_capacity = as_double("device_rated_capacity");
		else
			device_rated_capacity = 0.0;

		//Read number of devices
		int number_devices = as_integer("number_devices");

		// total loss
		double total_loss = as_double("loss_array_spacing")
			+ as_double("loss_resource_overprediction")
			+ as_double("loss_transmission")
			+ as_double("loss_downtime")
			+ as_double("loss_additional");


		//Storing each column of the tidal_resource_matrix and tidal_power_curve as vectors:
		for (int i = 0; i < number_rows; i++) {
			
			_speed_vect[i] = tidal_resource_matrix.at(i, 0);	
			_probability_vect[i] = tidal_resource_matrix.at(i, 1); //*******************again need to modify to handle different depths
			_power_vect[i] = tidal_power_curve.at(i, 1);
			
			//Store max power if not set in UI:
			/*if (as_boolean("calculate_capacity")) */
				if (_power_vect[i] > device_rated_capacity)
					device_rated_capacity = _power_vect[i];
			
			//Checker to ensure probability distribution adds to >= 99.5%:
			_probability_vect_checker += _probability_vect[i];
		
			//Calculate annual energy production at each stream speed bin:
			p_annual_energy_dist[i] = _power_vect[i] * _probability_vect[i] * number_devices * 8760;

			//Add current annual energy bin to total annual energy
			annual_energy += p_annual_energy_dist[i];

			//Calculate the cumulative energy probability distribution
			if (i == 0)
				p_annual_cumulative_energy_dist[i] = p_annual_energy_dist[i];
			else
				p_annual_cumulative_energy_dist[i] = p_annual_energy_dist[i] + p_annual_cumulative_energy_dist[i - 1];
			
			//Contribution to Average Power from this speed bin 
			device_average_power += _power_vect[i] * _probability_vect[i];
		}
				
		//Throw exception if frequency distribution vector sums to < 99.5%
		double probability_tolerance = 0.005;
		if (fabs(1.0 - _probability_vect_checker) > probability_tolerance)
			throw exec_error("mhk_tidal", "Probability distribution vector does not add up to 100%.");

		//Factoring in losses in total annual energy production:
		annual_energy *= (1 - (total_loss / 100 ));
		// leave device power without losses

		//Calculating capacity factor:
		capacity_factor = annual_energy / (device_rated_capacity * number_devices * 8760);

		//Assigning values to outputs:
		assign("annual_energy", var_data((ssc_number_t)annual_energy));
		assign("device_average_power", var_data((ssc_number_t)device_average_power));
		assign("device_rated_capacity", var_data((ssc_number_t)device_rated_capacity));
		assign("capacity_factor", var_data((ssc_number_t)capacity_factor * 100));
	}
}; 

DEFINE_MODULE_ENTRY( mhk_tidal , "MHK Tidal power calculation model using power distribution.", 3);



