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

#ifndef __tckernel_h
#define __tckernel_h

#include "tcskernel.h"
#include "core.h"

// Fix these
#pragma warning( disable : 4264)

class tcKernel : public tcskernel, public compute_module
{
public:
	tcKernel(tcstypeprovider *prov);
	virtual ~tcKernel();
	
	virtual void message( const std::string & text, int msgtype );
	virtual bool progress( float percent, const std::string &status );
	virtual bool converged( double time );
	void set_store_array_matrix_data( bool b ) { m_storeArrMatData = b; }
	void set_store_all_parameters( bool b ) { m_storeAllParameters = b; }

	// Bad practice, inheriting simulate() method from tcskernal, this masks it
#pragma warning( disable: 4263)
	virtual int simulate( double start, double end, double step, int max_iter = 100 );

	void set_unit_value_ssc_string( int id, const char *name );
	void set_unit_value_ssc_double( int id, const char *name );
	void set_unit_value_ssc_double( int id, const char *name, double x );
	void set_unit_value_ssc_array( int id, const char *name );
	void set_unit_value_ssc_matrix(int id, const char *name);
	void set_unit_value_ssc_matrix_transpose(int id, const char *name);

	// change ssc name to tcs name
	void set_unit_value_ssc_string(int id, const char *tcs_name, const char *ssc_name);
	void set_unit_value_ssc_double(int id, const char *tcs_name, const char *ssc_name);
	void set_unit_value_ssc_array(int id, const char *tcs_name, const char *ssc_name);
	void set_unit_value_ssc_matrix(int id, const char *tcs_name, const char *ssc_name);
	void set_unit_value_ssc_matrix_transpose(int id, const char *tcs_name, const char *ssc_name);

	bool set_output_array(const char *output_name, size_t len, double scaling = 1);
	bool set_output_array(const char *ssc_output_name, const char *tcs_output_name, size_t len, double scaling = 1);
	bool set_all_output_arrays();

	struct dataitem {
		dataitem( const char *s ) : sval(s) { }
		dataitem( const std::string &s ) : sval(s) { }
		dataitem( double d ) : dval(d) { }
		std::string sval;
		double dval;
	};

	struct dataset {
		unit *u;
		int uidx;
		int idx;
		std::string name;
		std::string units;
		std::string group;
		int type;
		std::vector<dataitem> values;
	};

	dataset *get_results(int idx);

private:
	bool m_storeArrMatData;
	bool m_storeAllParameters; // true = all inputs/outputs for all units will be saved for every time step; false = only store values that match SSC parameters defined as SSC_OUTPUT or SSC_INOUT
	std::vector< dataset > m_results;
	double m_start, m_end, m_step;
	size_t m_dataIndex;
};

#endif