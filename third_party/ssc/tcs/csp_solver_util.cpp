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

#include "csp_solver_util.h"
#include <math.h>
#include <algorithm>

const C_csp_reported_outputs::S_output_info csp_info_invalid = {-1, -1};

C_csp_reported_outputs::C_output::C_output()
{
	mp_reporting_ts_array = 0;		// Initialize pointer to NULL

	m_is_allocated = false;

	m_subts_weight_type = -1;

	m_counter_reporting_ts_array = 0;

	m_n_reporting_ts_array = -1;
}

void C_csp_reported_outputs::C_output::assign(double *p_reporting_ts_array, size_t n_reporting_ts_array)
{
	mp_reporting_ts_array = p_reporting_ts_array;
	mv_temp_outputs.reserve(10);

	m_is_allocated = true;

	m_n_reporting_ts_array = n_reporting_ts_array;
}

void C_csp_reported_outputs::C_output::set_m_is_ts_weighted(int subts_weight_type)
{
	m_subts_weight_type = subts_weight_type;

	if( !(m_subts_weight_type == TS_WEIGHTED_AVE ||
		  m_subts_weight_type == TS_1ST ||
		  m_subts_weight_type == TS_LAST ||
          m_subts_weight_type == TS_MAX) )
	{
		throw(C_csp_exception("C_csp_reported_outputs::C_output::send_to_reporting_ts_array did not recognize subtimestep weighting type"));
	}
}

int C_csp_reported_outputs::C_output::get_vector_size()
{
	return (int)mv_temp_outputs.size();
}

void C_csp_reported_outputs::C_output::set_timestep_output(double output_value)
{
	if( m_is_allocated )
	{	
		mv_temp_outputs.push_back(output_value);
	}
}

void C_csp_reported_outputs::C_output::send_to_reporting_ts_array(double report_time_start, int n_report, 
		const std::vector<double> & v_temp_ts_time_end, double report_time_end, bool is_save_last_step, int n_pop_back )
{
	if( m_is_allocated )
	{	
		if( mv_temp_outputs.size() != n_report )
		{
			throw(C_csp_exception("Time and data arrays are not the same size", "C_csp_reported_outputs::send_to_reporting_ts_array"));
		}

		if( m_counter_reporting_ts_array + 1 > m_n_reporting_ts_array )
		{
			throw(C_csp_exception("Attempting store more points in Reporting Timestep Array than it was allocated for"));
		}
	
		double m_report_step = report_time_end - report_time_start;

		if( m_subts_weight_type == TS_WEIGHTED_AVE )
		{	// ***********************************************************
			//      Set outputs that are reported as weighted averages if 
			//       multiple csp-timesteps for one reporting timestep
			//    The following code assumes 'SOLZEN' is the first such output
			//    and that all names following it in 'E_reported_outputs' are weight averages
			// **************************************************************			
			double time_prev = report_time_start;		//[s]
			for( int i = 0; i < n_report; i++ )
			{
				mp_reporting_ts_array[m_counter_reporting_ts_array] += (float)((fmin(v_temp_ts_time_end[i], report_time_end) - time_prev)*mv_temp_outputs[i]);	//[units]*[s]
				time_prev = fmin(v_temp_ts_time_end[i], report_time_end);
			}
			mp_reporting_ts_array[m_counter_reporting_ts_array] /= (float)m_report_step;
		}
		else if (m_subts_weight_type == TS_1ST)
		{	// ************************************************************
			// Set instantaneous outputs that are reported as the first value
			//   if multiple csp-timesteps for one reporting timestep
			// ************************************************************
			mp_reporting_ts_array[m_counter_reporting_ts_array] = (float)mv_temp_outputs[0];
		}
		else if (m_subts_weight_type == TS_LAST)
		{	// ************************************************************
			// Set instantaneous outputs that are reported as the last value
			//   if multiple csp-timesteps for one reporting timestep
			// ************************************************************
			mp_reporting_ts_array[m_counter_reporting_ts_array] = (float)mv_temp_outputs[n_report - 1];
		}
        else if (m_subts_weight_type == TS_MAX) {
            // ************************************************************
            // Set instantaneous outputs that are reported as the maximum value
            //   if multiple csp-timesteps for one reporting timestep
            // ************************************************************
            mp_reporting_ts_array[m_counter_reporting_ts_array] =(float)(*std::max_element(mv_temp_outputs.begin(), mv_temp_outputs.end()));
        }
		else
		{
			throw(C_csp_exception("C_csp_reported_outputs::C_output::send_to_reporting_ts_array did not recognize subtimestep weighting type"));
		}

		if( is_save_last_step )
		{
			mv_temp_outputs[0] = mv_temp_outputs[n_report - 1];
		}

		for( int i = 0; i < n_pop_back; i++ )
		{
			mv_temp_outputs.pop_back();
		}

		m_counter_reporting_ts_array++;

	}	
}

void C_csp_reported_outputs::send_to_reporting_ts_array(double report_time_start,
	const std::vector<double> & v_temp_ts_time_end, double report_time_end)
{
	int n_report = (int)v_temp_ts_time_end.size();
	if( n_report < 1 )
	{
		throw(C_csp_exception("No data to report", "C_csp_reported_outputs::send_to_reporting_ts_array"));
	}

	bool is_save_last_step = true;
	int n_pop_back = n_report - 1;
	if( v_temp_ts_time_end[n_report - 1] == report_time_end )
	{
		is_save_last_step = false;
		n_pop_back = n_report;
	}

	for( int i = 0; i < m_n_outputs; i++ )
	{
		mvc_outputs[i].send_to_reporting_ts_array(report_time_start, n_report, v_temp_ts_time_end,
			report_time_end, is_save_last_step, n_pop_back);
	}
}

std::vector<double> C_csp_reported_outputs::C_output::get_output_vector()
{
	return mv_temp_outputs;
}

std::vector<double> C_csp_reported_outputs::get_output_vector(int index)
{
	return mvc_outputs[index].get_output_vector();
}

void C_csp_reported_outputs::construct(const S_output_info *output_info)
{
	int n_outputs = 0;

	while( output_info[n_outputs].m_name != csp_info_invalid.m_name )
	{
		n_outputs++;
	}

	// Resize the vector of Output Classes
	mvc_outputs.resize(n_outputs);
	m_n_outputs = n_outputs;

	mv_latest_calculated_outputs.resize(n_outputs);

	// Loop through the output info and set m_is_ts_weighted for each output
	for( int i = 0; i < n_outputs; i++ )
	{
		mvc_outputs[i].set_m_is_ts_weighted(output_info[i].m_subts_weight_type);
	}

	m_n_reporting_ts_array = -1;
}

bool C_csp_reported_outputs::assign(int index, double *p_reporting_ts_array, size_t n_reporting_ts_array)
{
	if(index < 0 || index >= m_n_outputs)
		return false;

	if(m_n_reporting_ts_array == -1)
	{
		m_n_reporting_ts_array = n_reporting_ts_array;
	}
	else
	{
		if( m_n_reporting_ts_array != n_reporting_ts_array )
			return false;
	}

	mvc_outputs[index].assign(p_reporting_ts_array, n_reporting_ts_array);

	return true;
}

void C_csp_reported_outputs::set_timestep_outputs()
{
	for(int i = 0; i < m_n_outputs; i++)
		mvc_outputs[i].set_timestep_output(mv_latest_calculated_outputs[i]);
}

void C_csp_reported_outputs::C_output::overwrite_vector_to_constant(double value)
{
	int n_timesteps = get_vector_size();

	for( int i = 0; i < n_timesteps; i++ )
	{
		mv_temp_outputs[i] = value;
	}
}

void C_csp_reported_outputs::overwrite_vector_to_constant(int index, double value)
{
	mvc_outputs[index].overwrite_vector_to_constant(value);
}

void C_csp_reported_outputs::C_output::overwrite_most_recent_timestep(double value)
{
	int n_timesteps = get_vector_size();

	if(n_timesteps == 0)
		return;

	mv_temp_outputs[n_timesteps-1] = value;
}

void C_csp_reported_outputs::overwrite_most_recent_timestep(int index, double value)
{
	mvc_outputs[index].overwrite_most_recent_timestep(value);
}

int C_csp_reported_outputs::size(int index)
{
	return mvc_outputs[index].get_vector_size();
}

void C_csp_reported_outputs::value(int index, double value)
{
	mv_latest_calculated_outputs[index] = value;
}

double C_csp_reported_outputs::value(int index)
{
	return mv_latest_calculated_outputs[index];
}

C_csp_messages::C_csp_messages()
{
	//m_message_list.resize(0);
    m_message_list.clear();
}

void C_csp_messages::add_message(int type, std::string msg)
{
	// Want first message last...
    m_message_list.insert( m_message_list.begin(), S_message_def(type, msg) );

}

void C_csp_messages::add_notice(std::string msg)
{
	add_message(C_csp_messages::NOTICE, msg);
}

bool C_csp_messages::get_message(int *type, std::string *msg)
{
    if(m_message_list.size() == 0)
        return false;

    S_message_def temp = m_message_list.back();
    m_message_list.pop_back();

    *msg = temp.msg;
    *type = temp.m_type;

    return true;
}

bool C_csp_messages::get_message(std::string *msg)
{
    int itemp;

    return get_message(&itemp, msg);
}

const char* C_csp_exception::what()
{
	return "CSP exception";
}

C_csp_exception::C_csp_exception(const char *cmsg)
{
	m_error_message = cmsg;
	m_code_location = "unknown";
	m_error_code = -1;
}
C_csp_exception::C_csp_exception(const std::string &error_message, const std::string &code_location)
{
	m_error_message = error_message;
	m_code_location = code_location;
	m_error_code = -1;
}
C_csp_exception::C_csp_exception(const std::string &error_message, const std::string &code_location, int error_code)
{
	m_error_message = error_message;
	m_code_location = code_location;
	m_error_code = error_code;
}

bool check_double(double x)
{
	if( x != x )
	{
		return false;
	}
	return true;
}
