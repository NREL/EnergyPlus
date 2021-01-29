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

#ifndef __csp_solver_util_
#define __csp_solver_util_

#include <string>
#include <vector>

#include <exception>

class C_csp_reported_outputs
{

public:
	
	enum E_subts_weight_type
	{
		TS_WEIGHTED_AVE,
		TS_1ST,
		TS_LAST,
        TS_MAX
	};

	class C_output
	{
	private:
		double *mp_reporting_ts_array;
		size_t m_n_reporting_ts_array;			//[-] Length of allocated array
		std::vector<double> mv_temp_outputs;

		bool m_is_allocated;		// True = memory allocated for array. False = no memory allocated, won't write outputs
		
		int m_subts_weight_type;	// 0: timestep-weighted average, 1: Take first piont in mv_temp_outputs, 2: Take final point in mv_temp_outupts
		//bool m_is_ts_weighted;		// True = timestep-weighted average of mv_temp_outputs, False = take first point in mv_temp_outputs
		
		int m_counter_reporting_ts_array;	//[-] Tracking current location of reporting array

	public:
		C_output();

		int get_vector_size();

		void set_m_is_ts_weighted(int subts_weight_type);

		void assign(double *p_reporting_ts_array, size_t n_reporting_ts_array);

		void set_timestep_output(double output_value);

		void overwrite_most_recent_timestep(double value);

		void overwrite_vector_to_constant(double value);

		std::vector<double> get_output_vector();

		void send_to_reporting_ts_array(double report_time_start, int n_report,
			const std::vector<double> & v_temp_ts_time_end, double report_time_end, bool is_save_last_step, int n_pop_back);
	};

	struct S_output_info
	{
		// Finally name must be = OUTPUT_END, so that we know how many outputs are in the table
		int m_name;					//[-] Integer key for variable
		int m_subts_weight_type;	// 0: timestep-weighted average, 1: Take first piont in mv_temp_outputs, 2: Take final point in mv_temp_outupts

		//bool m_is_ts_weighted;		// True = timestep-weighted average of mv_temp_outputs, False = take first point in mv_temp_outputs	
	};

private:

	std::vector<C_output> mvc_outputs;	//[-] vector of Output Classes
	int m_n_outputs;					//[-] number of Output Classes in vector
	
	size_t m_n_reporting_ts_array;			//[-] Length of allocated array

	std::vector<double> mv_latest_calculated_outputs;	//[-] Output after most recent 

public:

	C_csp_reported_outputs(){};

	void construct(const S_output_info *output_info);

	bool assign(int index, double *p_reporting_ts_array, size_t n_reporting_ts_array);

	void send_to_reporting_ts_array(double report_time_start,
		const std::vector<double> & v_temp_ts_time_end, double report_time_end);

	std::vector<double> get_output_vector(int index);

	void value(int index, double value);

	double value(int index);

	void overwrite_most_recent_timestep(int index, double value);

	void overwrite_vector_to_constant(int index, double value);

	int size(int index);

	void set_timestep_outputs();

};

extern const C_csp_reported_outputs::S_output_info csp_info_invalid;

class C_csp_messages
{

public:
	enum 
	{
		NOTICE = 1,
		WARNING
	};

	struct S_message_def
	{
		int m_type;
		std::string msg;

		S_message_def()
		{
			m_type = -1;
		}

        S_message_def(int type, std::string msgin)
        {
            m_type = type;
            msg = msgin;
        };

	};

	std::vector<S_message_def> m_message_list;	

public:
	C_csp_messages();

	void add_message(int type, std::string msg);

	void add_notice(std::string msg);

	bool get_message(int *type, std::string *msg);

	bool get_message(std::string *msg);

	void transfer_messages(C_csp_messages & c_csp_messages_downstream)
	{
		int out_type = -1;
		std::string out_msg = "";

		while( c_csp_messages_downstream.get_message(&out_type, &out_msg) )
		{
			add_message(out_type, out_msg);
		}
	}

};

class C_csp_exception : public std::exception
{
public:
	std::string m_error_message;
	std::string m_code_location;
	int m_error_code;
	
	// Useful in case exception goes uncatched
	virtual const char* what();

	C_csp_exception( const char *msg );
	C_csp_exception(const std::string &error_message, const std::string &code_location);
	C_csp_exception(const std::string &error_message, const std::string &code_location, int error_code);
	virtual ~C_csp_exception() throw() { }

};

bool check_double(double x);


#endif
