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

#include "tckernel.h"


tcKernel::tcKernel(tcstypeprovider *prov)
	: tcskernel(prov), m_start(0), m_end(0), m_step(0)
{
	m_storeArrMatData = false;
	m_storeAllParameters = false;
}

tcKernel::~tcKernel()
{

}

#ifdef _MSC_VER
#define mysnprintf _snprintf
#else
#define mysnprintf snprintf
#endif

void tcKernel::message( const std::string & text, int msgtype )
{
	int ssctype = SSC_ERROR;
	if ( msgtype == TCS_WARNING ) ssctype = SSC_WARNING;
	else if ( msgtype == TCS_NOTICE ) ssctype = SSC_NOTICE;
	compute_module::log( text, ssctype, (float)tcskernel::current_time()/8760.0f );
}

bool tcKernel::progress( float percent, const std::string &status )
{
	return compute_module::update( status, percent );
}

bool tcKernel::converged( double time )
{
	if (m_step != 0.0 )
	{
		int istep = (int) ((time-m_start)/m_step);
		int nstep = (int) ((m_end-m_start)/m_step);
		int nnsteps = nstep/200;
		if ( nnsteps == 0 ) nnsteps = 1;		
		if (istep % nnsteps == 0)
		{
			double percent = 100 * (((double)istep) / ((double)nstep) );

			if ( !compute_module::update( "", (float)percent, (float)istep ) )
				return false; // abort simulation if compute_module update returned false from controller
		}
	}

	std::string buf;
	char ibuf[128];
	size_t j,k;
	for ( size_t i=0;i<m_results.size(); i++ )
	{
		tcsvalue &v = m_results[i].u->values[ m_results[i].idx ];
		switch( m_results[i].type )
		{
		case TCS_NUMBER:
			m_results[i].values[ m_dataIndex ].dval = v.data.value;
			break;
		case TCS_STRING:
			m_results[i].values[ m_dataIndex ].sval = v.data.cstr;
			break;
		case TCS_ARRAY:
			if ( m_storeArrMatData )
			{
				buf = "[ ";
				for (j=0;j<v.data.array.length;j++)
				{
					mysnprintf(ibuf, 126, "%lg%c", v.data.array.values[j],
						j < v.data.array.length-1 ? ',' : ' ');
					buf += ibuf;
				}
				buf += "]";
				m_results[i].values[ m_dataIndex ].sval = buf;
			}
			break;
		case TCS_MATRIX:
			if ( m_storeArrMatData )
			{
				mysnprintf( ibuf, 126, "{ %dx%d ", v.data.matrix.nrows, v.data.matrix.ncols );
				buf = ibuf;
				for (j=0;j<v.data.matrix.nrows;j++)
				{
					buf += " [";
					for (k=0;k<v.data.matrix.ncols;k++)
					{
						mysnprintf(ibuf, 126, "%lg%c", TCS_MATRIX_INDEX(&v, j, k),
							k < v.data.matrix.ncols-1 ? ',' : ' ');
						buf += ibuf;
					}
					buf += "]";
				}
				buf += " }";
				m_results[i].values[ m_dataIndex ].sval = buf;		
			}
			break;
		}
	}

	m_dataIndex++;

	return true;
}

int tcKernel::simulate( double start, double end, double step, int max_iter )
{

	// find all output variables and add to results vector
	m_start = start;
	m_end = end;
	m_step = step;
	m_dataIndex = 0;

	if ( end <= start || step <= 0 )
		return -77;

	int nsteps = (int)( (end-start)/step ) + 1;

	size_t ndatasets = 0;
	for (size_t i=0;i<m_units.size();i++)
	{
		tcsvarinfo *vars = m_units[i].type->variables;
		int idx=0;
		while( vars[idx].var_type != TCS_INVALID )
		{
			if (is_ssc_array_output(vars[idx].name) || m_storeAllParameters)
				ndatasets++;
			idx++;
		}
	}

	if ( ndatasets < 1 )
		return -88;

	m_results.resize( ndatasets );

	size_t idataset = 0;
	for (size_t i=0;i<m_units.size();i++)
	{
		tcsvarinfo *vars = m_units[i].type->variables;
		int idx = 0;
		while( vars[idx].var_type != TCS_INVALID )
		{
			if (is_ssc_array_output(vars[idx].name) || m_storeAllParameters )
			{
				dataset &d = m_results[ idataset++ ];
				char buf[32];
				sprintf(buf, "%d", (int)i);
				d.u = &m_units[i];
				d.uidx = (int)i;
				d.idx = idx;
				d.group = "Unit " + std::string(buf) + " (" + std::string(m_units[i].type->name) + ")";//: " + m_units[i].name;
				d.name = vars[idx].name;
				d.units = vars[idx].units;
				d.type = vars[idx].data_type;
				d.values.resize( nsteps, dataitem(0.0) );
			}
			idx++;
		}
	}
	tcskernel::set_max_iterations(max_iter, true);
	return tcskernel::simulate( start, end, step );
}

tcKernel::dataset *tcKernel::get_results(int idx)
{
	if (idx >= (int) m_results.size()) return 0;
	else return &m_results[idx];
}

void tcKernel::set_unit_value_ssc_string(int id, const char *name)
{
	set_unit_value(id, name, as_string(name));
}

void tcKernel::set_unit_value_ssc_string(int id, const char *tcs_name, const char *ssc_name)
{
	set_unit_value(id, tcs_name, as_string(ssc_name));
}

void tcKernel::set_unit_value_ssc_double( int id, const char *name )
{
	set_unit_value( id, name, as_double(name) );
}

void tcKernel::set_unit_value_ssc_double(int id, const char *tcs_name, const char *ssc_name)
{
	set_unit_value(id, tcs_name, as_double(ssc_name));
}


void tcKernel::set_unit_value_ssc_double( int id, const char *name, double x )
{
	set_unit_value( id, name, x );
}


void tcKernel::set_unit_value_ssc_array( int id, const char *name )
{
	size_t len;
	ssc_number_t * p = as_array(name, &len);
	double *pt = new double[len];
	for ( size_t i=0;i<len;i++ ) pt[i] = (double) p[i];
	set_unit_value(id, name, pt, (int)len);
	delete [] pt;
	return;
}

void tcKernel::set_unit_value_ssc_array(int id, const char *tcs_name, const char *ssc_name)
{
	size_t len;
	ssc_number_t * p = as_array(ssc_name, &len);
	double *pt = new double[len];
	for (size_t i = 0; i<len; i++) pt[i] = (double)p[i];
	set_unit_value(id, tcs_name, pt, (int)len);
	delete[] pt;
	return;
}


void tcKernel::set_unit_value_ssc_matrix(int id, const char *name)
{
	size_t nr, nc;
	ssc_number_t *p = as_matrix(name, &nr, &nc);
	double *pt = new double[nr*nc];
	for (size_t i = 0; i<nr*nc; i++) pt[i] = (double)p[i];
	set_unit_value(id, name, pt, (int)nr, (int)nc);
	delete[] pt;
	return;
}

void tcKernel::set_unit_value_ssc_matrix(int id, const char *tcs_name, const char *ssc_name)
{
	size_t nr, nc;
	ssc_number_t *p = as_matrix(ssc_name, &nr, &nc);
	double *pt = new double[nr*nc];
	for (size_t i = 0; i<nr*nc; i++) pt[i] = (double)p[i];
	set_unit_value(id, tcs_name, pt, (int)nr, (int)nc);
	delete[] pt;
	return;
}

void tcKernel::set_unit_value_ssc_matrix_transpose(int id, const char *name)
{
	size_t nr, nc;
	ssc_number_t *p = as_matrix(name, &nr, &nc);
	double *pt = new double[nr*nc];
	size_t i = 0;
		for (size_t c = 0; c< nc; c++)
			for (size_t r = 0; r < nr; r++)
				pt[i++] = (double)p[r*nc + c];
		set_unit_value(id, name, pt, (int)nc, (int)nr);
	delete[] pt;
	return;
}

void tcKernel::set_unit_value_ssc_matrix_transpose(int id, const char *tcs_name, const char *ssc_name)
{
	size_t nr, nc;
	ssc_number_t *p = as_matrix(ssc_name, &nr, &nc);
	double *pt = new double[nr*nc];
	size_t i = 0;
	for (size_t c = 0; c< nc; c++)
		for (size_t r = 0; r < nr; r++)
			pt[i++] = (double)p[r*nc + c];
	set_unit_value(id, tcs_name, pt, (int)nc, (int)nr);
	delete[] pt;
	return;
}


bool tcKernel::set_output_array(const char *output_name, size_t len, double scaling)
{
	return set_output_array(output_name, output_name, len, scaling);
}

bool tcKernel::set_output_array(const char *ssc_output_name, const char *tcs_output_name, size_t len, double scaling)
{
	int idx=0;
	ssc_number_t *output_array = allocate( ssc_output_name, len );
	while( tcKernel::dataset *d = get_results(idx++) )
	{
		if ( (d->type == TCS_NUMBER) && (d->name == tcs_output_name) && (d->values.size() == len ) )
		{
			for (size_t i=0;i<len;i++)
				output_array[i] = (ssc_number_t)(d->values[i].dval * scaling);
			return true;
		}
	}

	return false;
}

bool tcKernel::set_all_output_arrays()
{
	int idx=0;
	while( tcKernel::dataset *d = get_results(idx++) )
	{	// if the TCS value is a TCS_NUMBER (so that we can put the value into a one-dimensional array - single value for 8760 hours)
		// and
		// if there is an SSC_OUTPUT with the same name
		if ( (d->type == TCS_NUMBER) && ( is_ssc_array_output(d->name) ) )
		{
			ssc_number_t *output_array = allocate( d->name, d->values.size() );
			for (size_t i=0; i<d->values.size(); i++)
				output_array[i] = (ssc_number_t) d->values[i].dval;
		}
	}

	return true;
}
