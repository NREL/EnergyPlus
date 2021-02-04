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

#define _CRT_SECURE_NO_WARNINGS 1

#include <string>
#include <numeric>
#include <cstring>
#include <cstdio>
#include <cstdlib>
#include <cstdarg>
#include <cmath>
#include <limits>
#include <iostream>
#include <algorithm>

#include "tcskernel.h"

//#include "tcs_debug.h"

#if defined(__WINDOWS__)||defined(WIN32)||defined(_WIN32)||defined(__MINGW___)||defined(_MSC_VER)
#include <Windows.h>
void *dll_open(const char *name) { return (void*) ::LoadLibraryA( name ); }
void dll_close( void *handle ) { ::FreeLibrary( (HMODULE)handle ); }
void *dll_sym( void *handle, const char *name ) { return (void*) ::GetProcAddress( (HMODULE)handle, name ); }
#else
#include <dlfcn.h>
void *dll_open(const char *name) { return dlopen( name, RTLD_NOW ); }
void dll_close( void *handle ) { dlclose( handle ); }
void *dll_sym( void *handle, const char *name ) { return dlsym( handle, name ); }
#endif

extern "C" tcstypeinfo **tcsdynamictypes();

tcstypeprovider sg_tcsTypeProvider;

tcstypeprovider::tcstypeprovider()
{

	tcstypeinfo **built_in = tcsdynamictypes();
	size_t i = 0;
	while (tcstypeinfo *ti = built_in[i++])
		register_type(ti->name, ti);
}

tcstypeprovider::~tcstypeprovider()
{
	unload_libraries();
}

void tcstypeprovider::add_search_path( const std::string &path )
{
	std::vector<std::string>::iterator it = std::find( m_pathList.begin(), m_pathList.end(), path );
	if ( it == m_pathList.end() )
		m_pathList.push_back( path );
}

void tcstypeprovider::clear_search_paths()
{
	m_pathList.clear();
}

void tcstypeprovider::register_type( const std::string &type, tcstypeinfo *ti )
{	
	typedata x;
	x.type = type;
	x.dyn = 0;
	x.info = ti;			
	m_types.push_back( x );
}

std::vector<tcstypeprovider::typedata> tcstypeprovider::types()
{
	return m_types;
}

int tcstypeprovider::load_library( const std::string &name )
{	
	std::string ext;
#if defined(_WIN32)
	ext = ".dll";
#elif defined(__MACH__)
	ext = ".dylib";
#else
	ext = ".so";
#endif
	for ( std::vector<std::string>::const_iterator it = m_pathList.begin();
		it != m_pathList.end();
		++it )
	{
		std::string path = *it + "/" + name + ext;

		m_messages.push_back( "attempting: " + path );
		
		void *pdl, *pf;
		tcstypeinfo **ti;
		if ( (pdl = dll_open( path.c_str() ))
			&& (pf = dll_sym( pdl, "tcsdynamictypes" ))
			&& (ti = (*((tcstypeinfo**(*)())pf))() ) )
		{

			m_libraries.push_back( dyndata() );
			dyndata *d = &m_libraries[ m_libraries.size() - 1 ];
			d->path = path;
			d->dynlib = pdl;
			d->types = ti;

			size_t idx = 0;
			while( ti[idx] != 0 )
			{
				typedata x;
				x.type = std::string( ti[idx]->name );
				x.dyn = d;
				x.info = ti[idx];		
				m_types.push_back( x );
				std::ostringstream ss;
				ss << "type " << ti[idx]->name << "\n\tdesc=" << ti[idx]->description << "\n\tauth=" << ti[idx]->author
					<< "\n\ttime=" << ti[idx]->timestamp << "\n\tver=" << ti[idx]->version
					<< "\n\treqker=" << ti[idx]->require_kernel_version 
					<< "\n\tonconv=" << ti[idx]->call_after_convergence;
				
				m_messages.push_back( ss.str() );
				
				idx++;
			}
			
			std::ostringstream ss;
			ss << "loaded " << idx << " dynamic type(s) from " << path;
			m_messages.push_back( ss.str() );
			return (int)idx;
		}

		if (pdl) dll_close( pdl );
	}
	
	return 0;
}

void tcstypeprovider::unload_libraries()
{
	// unregister all types that are dynamically linked in
	size_t idx = 0;
	while( idx < m_types.size() )
	{
		if (m_types[idx].dyn != 0)
		{
			// delete this type from m_types
			m_messages.push_back( "unregistered type "  + m_types[idx].type );
			m_types.erase( m_types.begin() + idx );
		}
		else
			idx++;
	}

	// unload all dynamic libraries
	for ( std::vector<dyndata>::iterator it = m_libraries.begin();
		it != m_libraries.end();
		++it )
		if ( (*it).dynlib != 0 )
		{
			m_messages.push_back( "unloaded dynamic type library " + (*it).path );
			dll_close( (*it).dynlib );
		}
	
	m_libraries.clear();
}


tcstypeinfo *tcstypeprovider::find_type( const std::string &type )
{
	for ( std::vector<typedata>::const_iterator it = m_types.begin();
		it != m_types.end();
		++it )
		if ( (*it).type == type && (*it).info != 0 )
			return (*it).info;

	return 0;
}


static void _parse_number_list( char* &p, std::vector<double> &vals )
{
	char buf[256];
	
	while( 1 )
	{
		while ( *p && ( *p == ' ' || *p == '\t' || *p == ',' ) )
			p++;
	
		char *pb = buf;
		int idx = 0;
		while ( *p 
			&& ( isdigit(*p) 
				|| *p == '+' 
				|| *p == '-' 
				|| *p == '.'
				|| *p == 'e' 
				|| *p == 'E' ) 
			&& idx++ < 254 )
			*pb++ = *p++;
		*pb = 0;
		vals.push_back( atof(buf) );
		
		while ( *p && ( *p == ' ' || *p == '\t' ) )
			p++;
		
		if (*p != ',') return;
	}
}

static void tcsvalue_free( tcsvalue *v )
{
	switch( v->type )
	{
	case TCS_ARRAY:
		delete [] v->data.array.values;
		break;
	case TCS_MATRIX:
		delete [] v->data.matrix.values;
		break;
	case TCS_STRING:
		delete [] v->data.cstr;
		break;
	}
	
	v->type = TCS_INVALID;
}

static bool tcsvalue_parse_array( tcsvalue *v, const char *s )
{
	if ( !s ) return false;	
	std::vector<double> vals;
	char *p = (char*)s;
	_parse_number_list( p, vals );	
	if (vals.size() == 0) return false;

	tcsvalue_free( v );		
	v->type = TCS_ARRAY;
	v->data.array.values = new double[ vals.size() ];
	v->data.array.length = (unsigned int)vals.size();
	for (int i=0;i<(int)vals.size();i++)
		v->data.array.values[i] = vals[i];	

	return true;
}

static bool tcsvalue_parse_matrix( tcsvalue *v, const char *s )
{
	if( !s ) return false;
	
	std::vector< std::vector<double> > mat;
	char *p = (char*)s;
	size_t maxcol = 0;
	while (*p == '[')
	{
		p++;		
		std::vector<double> row;
		_parse_number_list( p, row );
		mat.push_back(row);
		if ( row.size() > maxcol ) maxcol = row.size();
		
		while (*p && (*p == ' ' || *p == '\t'))
			p++;
		
		if ( *p != ']' )
			return false;
		
		p++;
		
		while (*p && (*p == ' ' || *p == '\t'))
			p++;
	}
	
	if ( mat.size() == 0 || maxcol == 0 ) return false;
	
	int len = (int)(mat.size() * maxcol);

	tcsvalue_free( v );
	v->type = TCS_MATRIX;
	v->data.matrix.values = new double[ len ];
	v->data.matrix.nrows = (int)mat.size();
	v->data.matrix.ncols = (int)maxcol;
	
	for (int i=0;i<len;i++) v->data.matrix.values[i] = 0;
	
	for (size_t r=0;r<mat.size();r++)
		for(size_t c=0;c<maxcol;c++)
			if ( c < mat[r].size() )
				TCS_MATRIX_INDEX(v, r, c) = mat[r][c];

	return true;
}

static void tcsvalue_set_number( tcsvalue *v, double d)
{
	tcsvalue_free( v );
	v->type = TCS_NUMBER;
	v->data.value = d;
}

static void tcsvalue_set_array( tcsvalue *v, double *p, int len )
{
	if ( !p || len < 1 ) return;
	tcsvalue_free( v );
	v->type = TCS_ARRAY;
	v->data.array.values = new double[ len ];
	v->data.array.length = len;
	for(int i=0;i<len;i++) v->data.array.values[i] = p[i];
}

static void tcsvalue_set_matrix( tcsvalue *v, double *p, int nr, int nc )
{
	if ( !p || nr*nc < 1 ) return;
	tcsvalue_free( v );
	v->type = TCS_MATRIX;
	int len = nr*nc;
	v->data.matrix.values = new double[ len ];
	v->data.matrix.nrows = nr;
	v->data.matrix.ncols = nc;
	for (int i=0;i<nr*nc;i++) v->data.matrix.values[i] = p[i];
}

static void tcsvalue_set_string( tcsvalue *v, const char *s )
{
	tcsvalue_free( v );
	v->type = TCS_STRING;
	if ( !s )
	{
		v->data.cstr = new char[1];
		*v->data.cstr = 0;
		return;
	}
	v->data.cstr = new char[ strlen(s) + 1 ];
	strcpy( v->data.cstr, s );
}

#ifdef _MSC_VER
#define mysnprintf _snprintf
#else
#define mysnprintf snprintf
#endif

static std::string tcsvalue_as_string( tcsvalue &v )
{
	char ibuf[128];
	std::string buf;
	size_t j,k;
	switch( v.type )
	{
	case TCS_NUMBER:
		mysnprintf( ibuf, 126, "%lg", v.data.value );
		return std::string(ibuf);
	case TCS_STRING:
		return "'" + std::string( v.data.cstr ) + "'";
	case TCS_ARRAY:
		buf = "[ ";
		for (j=0;j<v.data.array.length;j++)
		{
			mysnprintf(ibuf, 126, "%lg%c", v.data.array.values[j],
				j < v.data.array.length-1 ? ',' : ' ');
			buf += ibuf;
		}
		buf += "]";
		return buf;
	case TCS_MATRIX:
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
		return buf;
	}
	return "<invalid>";
}

#ifdef _MSC_VER
#define my_snprintf _snprintf
#else
#define my_snprintf snprintf
#endif

static void _message( struct _tcscontext *t, int msgtype, const char *message )
{
	tcskernel *k = (tcskernel*)t->kernel_internal;
	int uid = t->unit_internal;
	k->message( uid, msgtype, message );
}

static bool _progress( struct _tcscontext *t, float percent, const char *message )
{
	tcskernel *k = (tcskernel*)t->kernel_internal;
	return k->progress( percent, message ? std::string(message) : std::string("") );
}

static tcsvalue *_get_value( struct _tcscontext *t, int idx )
{
	tcskernel::unit *u = (tcskernel::unit*) t->unit_internal;
	if ( !u || idx < 0 || idx >= (int)u->values.size() ) return 0;
	else return &u->values[idx];
}

static int _get_num_values( struct _tcscontext *t )
{
	tcskernel::unit *u = (tcskernel::unit*) t->unit_internal;
	if ( !u ) return 0;
	return (int)u->values.size();
}


tcskernel::tcskernel( tcstypeprovider *prov )
{
	m_provider = prov;
	m_proceedAnyway = true;
	m_maxIterations = 100;
	m_currentTime = 0;
	m_timeStep = 0;
	m_startTime = 0;
	m_endTime = 0;
}

tcskernel::~tcskernel()
{
	// nothing to do
}

int tcskernel::version()
{
	return TCS_KERNEL_VERSION;
}

void tcskernel::set_max_iterations( int iter, bool proceed )
{
	if (iter >= 1 )
		m_maxIterations = iter;

	m_proceedAnyway = proceed;
}

double tcskernel::current_time()
{
	return m_currentTime;
}

double tcskernel::time_step()
{
	return m_timeStep;
}

void tcskernel::message( int unit, int msgtype, const char *text )
{
#define TBUFLEN 128
	char tbuf[TBUFLEN];
	if (unit >= 0 && unit < (int) m_units.size())
	{
		my_snprintf( tbuf, TBUFLEN, "time %.2lf { %s %d }:\n", current_time(),
			m_units[unit].name.c_str(), unit );
	}
	else
	{
		my_snprintf( tbuf, TBUFLEN, "time %.2lf { invalid unit %d }:\n", current_time(),
			 unit );
	}

	message( std::string(tbuf) + std::string( text ), msgtype );
}

void tcskernel::message( const std::string &text, int msgtype )
{
	std::string preface("Notice: ");
	if ( msgtype == TCS_WARNING ) preface = "Warning: ";
	else if ( msgtype == TCS_ERROR ) preface = "Error: ";

	std::cout << text << std::endl;
}

bool tcskernel::progress( float percent, const std::string &status )
{
	std::cout << percent << "% " << status << std::endl;
	return true;
}

bool tcskernel::converged( double )
{
	/* by default, nothing to do - simply a notification mechanism for decendent classes */
	return true;
}


void tcskernel::set_unit_name( int id, const std::string &name )
{
	if ( id >= 0 && id < (int) m_units.size() ) m_units[id].name = name;
}

void tcskernel::set_unit_value( int id, int idx, double val )
{
	if ( id >= 0 && id < (int) m_units.size()
		&& idx >= 0 && idx < (int) m_units[id].values.size() )
		tcsvalue_set_number( &m_units[id].values[idx], val );
}

void tcskernel::set_unit_value( int id, int idx, double *p, int len )
{
	if ( id >= 0 && id < (int) m_units.size()
		&& idx >= 0 && idx < (int) m_units[id].values.size() )
		tcsvalue_set_array( &m_units[id].values[idx], p, len );
}

void tcskernel::set_unit_value( int id, int idx, double *p, int nr, int nc )
{
	if ( id >= 0 && id < (int) m_units.size()
		&& idx >= 0 && idx < (int) m_units[id].values.size() )
		tcsvalue_set_matrix( &m_units[id].values[idx], p, nr, nc );
}

void tcskernel::set_unit_value( int id, int idx, const char *s )
{
	if ( id >= 0 && id < (int) m_units.size()
		&& idx >= 0 && idx < (int) m_units[id].values.size() )
		tcsvalue_set_string( &m_units[id].values[idx], s );
}
	
void tcskernel::set_unit_value( int id, const char *name, double val )
{
	set_unit_value( id, find_var(id, name), val );
}

void tcskernel::set_unit_value( int id, const char *name, double *p, int len )
{
	set_unit_value( id, find_var(id, name), p, len );
}

void tcskernel::set_unit_value( int id, const char *name, double *p, int nr, int nc )
{
	set_unit_value( id, find_var(id, name), p, nr, nc );
}

void tcskernel::set_unit_value( int id, const char *name, const char *s )
{
	set_unit_value( id, find_var(id, name), s );
}

double tcskernel::get_unit_value_number( int id, const char *name )
{
	int idx = find_var( id,name );
	if ( id >= 0 && id < (int) m_units.size()
		&& idx >= 0 && idx < (int) m_units[id].values.size() )
	{
		if ( m_units[id].values[idx].type == TCS_NUMBER )
			return m_units[id].values[idx].data.value;
	}
	
	// otherwise NaN
	return std::numeric_limits<double>::quiet_NaN();
}

const char *tcskernel::get_unit_value_string( int id, const char *name )
{
	int idx = find_var( id,name );
	if ( id >= 0 && id < (int) m_units.size()
		&& idx >= 0 && idx < (int) m_units[id].values.size() )
	{
		if ( m_units[id].values[idx].type == TCS_STRING )
			return m_units[id].values[idx].data.cstr;
	}

	return 0;
}

double *tcskernel::get_unit_value( int id, const char *name, int *len )
{
	int idx = find_var( id,name );
	if ( id >= 0 && id < (int) m_units.size()
		&& idx >= 0 && idx < (int) m_units[id].values.size() )
	{
		if ( m_units[id].values[idx].type == TCS_ARRAY )
		{
			*len = (int)m_units[id].values[idx].data.array.length;
			return m_units[id].values[idx].data.array.values;
		}
	}
	return 0;
}

double *tcskernel::get_unit_value( int id, const char *name, int *nr, int *nc )
{
	int idx = find_var( id,name );
	if ( id >= 0 && id < (int) m_units.size()
		&& idx >= 0 && idx < (int) m_units[id].values.size() )
	{
		if ( m_units[id].values[idx].type  == TCS_MATRIX )
		{
			*nr = (int)m_units[id].values[idx].data.matrix.nrows;
			*nc = (int)m_units[id].values[idx].data.matrix.ncols;
			return m_units[id].values[idx].data.matrix.values;
		}
	}

	return 0;
}


bool tcskernel::parse_unit_value( tcsvalue *v, int type, const char *value )
{
	switch( type )
	{
	case TCS_STRING: tcsvalue_set_string( v, value ); return true;
	case TCS_NUMBER: tcsvalue_set_number( v, atof(value) ); return true;
	case TCS_ARRAY: return tcsvalue_parse_array( v, value );
	case TCS_MATRIX: return tcsvalue_parse_matrix( v, value );
	default: return false;
	}
}

bool tcskernel::parse_unit_value( int id, const char *name, const char *value )
{
	if ( id < 0 || id >= (int)m_units.size()) return false;
	int idx = find_var(id,name);
	if (idx < 0 || idx >= (int)m_units[id].values.size()) return false;

	tcsvarinfo &inf = m_units[id].type->variables[idx];
	tcsvalue &v = m_units[id].values[idx];

	return parse_unit_value( &v, inf.data_type, value );
}


int tcskernel::copy( tcskernel &tk )
{
	clear_units();
	
	for ( std::vector<unit>::iterator it = tk.m_units.begin();
		it != tk.m_units.end();
		++it )
	{
		unit &u = *it;
		// add unit
		int id = add_unit( u.type->name, u.name );
		if ( id < 0 )
			return -1;
		
		// copy unit values
		if ( m_units[id].values.size() != u.values.size() ) return -2;

		for ( size_t k=0;k<u.values.size();k++ )
		{

			tcsvalue *lhs = &m_units[id].values[k];
			tcsvalue *rhs = &u.values[k];

			switch( rhs->type )
			{
			case TCS_STRING:
				tcsvalue_set_string( lhs, rhs->data.cstr );
				break;
			case TCS_NUMBER:
				tcsvalue_set_number( lhs, rhs->data.value );
				break;
			case TCS_ARRAY:
				tcsvalue_set_array( lhs, rhs->data.array.values, rhs->data.array.length );
				break;
			case TCS_MATRIX:
				tcsvalue_set_matrix( lhs, rhs->data.matrix.values,
					rhs->data.matrix.nrows,
					rhs->data.matrix.ncols );
				break;
			}
		}
	}

	// all units added before connections are copied

	for ( size_t id = 0; id<m_units.size();id++ )
	{
		unit &u = tk.m_units[id];

		// copy connections		
		for ( size_t j=0;j<u.conn.size();j++ )
		{
			std::vector<connection> &cc = u.conn[j];
			for ( size_t k=0;k<cc.size();k++)
			{
				connect( (int)id, (int)j, cc[k].target_unit, cc[k].target_index, 
					cc[k].ftol, cc[k].arridx );
			}
		}
	}

	return 0;
}


int tcskernel::add_unit( const std::string &type, const std::string &name )
{
	if ( !m_provider ) return -2;

	tcstypeinfo *t = m_provider->find_type(type);
	if ( t == 0 )
	{
		message( TCS_ERROR, "could not add unit of type '%s': type information not found.", type.c_str());
		return -1;
	}
	
	// push an empty unit, obtain a reference to it
	m_units.push_back( unit() );
	int id = (int)m_units.size() - 1;
	unit &u = m_units[ id ];
	u.id = id;
	u.name = name;
	u.type = t;
	u.instance = 0;
	
	u.context.kernel_internal = this;
	u.context.unit_internal = id;
	u.context.message = _message;
	u.context.progress = _progress;
	u.context.get_value = _get_value;
	u.context.get_num_values = _get_num_values;
	u.context.tcsvalue_set_number = tcsvalue_set_number;
	u.context.tcsvalue_set_array = tcsvalue_set_array;
	u.context.tcsvalue_set_matrix = tcsvalue_set_matrix;
	u.context.tcsvalue_set_string = tcsvalue_set_string;

	// determine number of variables ( inputs and outputs )
	int idx = 0;
	tcsvarinfo *vi = t->variables;
	while( vi[idx++].var_type != TCS_INVALID );

	int nvars = idx-1;
	
	// resize data in/out vectors
	u.values.resize( nvars );
	u.conn.resize( nvars );
	
	idx = 0;
	while( vi[idx].var_type != TCS_INVALID )
	{
		tcsvalue *v = &u.values[idx];
		v->type = TCS_INVALID; // nullify type
		switch( vi[idx].data_type )
		{
		case TCS_NUMBER:
			v->type = TCS_NUMBER;
			v->data.value = 0;
			if ( vi[idx].default_value != 0 )
				tcsvalue_set_number( v, atof(vi[idx].default_value) );
			break;
		case TCS_ARRAY:
			v->type = TCS_ARRAY;
			v->data.array.values = new double[1];
			v->data.array.values[0] = 0;
			v->data.array.length = 1;
			if ( vi[idx].default_value != 0 && strlen( vi[idx].default_value ) > 0 )
				tcsvalue_parse_array( v, vi[idx].default_value );
			break;
		case TCS_MATRIX:
			v->type = TCS_MATRIX;
			v->data.matrix.values = new double[1];
			v->data.matrix.values[0] = 0;
			v->data.matrix.nrows = 1;
			v->data.matrix.ncols = 1;
			if ( vi[idx].default_value != 0 && strlen( vi[idx].default_value ) > 0 )
				tcsvalue_parse_matrix( v, vi[idx].default_value );
			break;
		case TCS_STRING:
			if ( vi[idx].default_value != 0 ) tcsvalue_set_string( v, vi[idx].default_value );
			else tcsvalue_set_string( v, "" );
			break;
		}

		idx++;
	}
	
	return u.id;
}

void tcskernel::clear_units()
{	
	m_units.clear();
}

bool tcskernel::connect( int unit1, int output, 
		int unit2, int input,
		double tol,
		int arridx )
{
	if ( unit1 < 0 || unit1 > (int) m_units.size()
		|| unit2 < 0 || unit2 > (int) m_units.size()
		|| output < 0 || input < 0 )
		return false;
	
	unit &u1 = m_units[unit1];
	unit &u2 = m_units[unit2];
	
	if ( output >= (int) u1.values.size() ) return false;
	if ( output >= (int) u1.conn.size() ) return false; // should never happen
	if ( input >= (int) u2.values.size() ) return false;
	
	
	// check if this connection already exists
	std::vector< connection > &list = u1.conn[ output ];
	for (size_t i=0;i<list.size();i++)
		if ( list[i].target_unit == unit2 && list[i].target_index == input )
			return true; // already exists, so return success
	
	// add a new connection
	connection c;
	c.target_unit = unit2;
	c.target_index = input;
	c.ftol = tol;
	c.arridx = arridx;
	u1.conn[ output ].push_back( c );
	
	return true;
}

int tcskernel::find_var( int unit, const char *name )
{
	if ( unit < 0 || unit >= (int)m_units.size() ) return -1;
	tcsvarinfo *varlist = m_units[unit].type->variables;
	int idx = 0;
	while ( varlist[idx].var_type != TCS_INVALID
		&& varlist[idx].name != 0)
	{
		if (strcmp(varlist[idx].name, name) == 0)
			return idx;
		idx++;
	}
	message( TCS_NOTICE, "could not locate variable '%s' in unit %d (%s), type %s",
		name, unit, m_units[unit].name.c_str(), m_units[unit].type->name );
	return -1;
}

bool tcskernel::connect( int unit1, const char *var1,
		int unit2, const char *var2,
		double tol,
		int arridx )
{
	return connect( unit1, find_var(unit1, var1), 
		unit2, find_var(unit2, var2), 
		tol, arridx );
}

bool tcskernel::check_tolerance( double val1, double val2, double ftol )
{
	if ( val1 == val2 ) return true;
	
	if ( ftol <= 0 )
	{ // ftol is negative or zero: test is absolute
		if ( fabs( val1 - val2 ) > fabs( ftol ) )
			return false;
	}
	else
	{ // ftol is positive: test is relative (percentage difference)
		double denom = val1;
		if ( denom == 0.0 )	denom = val2;
		if ( denom == 0.0 )	denom = 1.0;
			
		if (  fabs( (val1-val2)/denom ) > fabs( ftol/100.0 ) )
			return false;
	}
	
	return true;
}

void tcskernel::create_instances()
{
	for (size_t i=0;i<m_units.size();i++)
		m_units[i].instance = m_units[i].type->create_instance( &m_units[i].context, m_units[i].type );
}

void tcskernel::free_instances()
{
	for (size_t i=0;i<m_units.size();i++)
	{
		m_units[i].type->free_instance( m_units[i].instance );
		m_units[i].instance = 0;
	}
}

int tcskernel::solve( double time, double step )
{
	// must call each unit at least once each timestep
	for (size_t i=0;i<m_units.size();i++)
	{
		m_units[i].ncall = 0;
		m_units[i].mustcall = true;
	}
	
	int iterations = 0;
	bool converged = false;		
	while( !converged )
	{
		if (iterations++ >= m_maxIterations )
		{
			message( TCS_NOTICE, "kernel exceeded maximum iterations of %d, at time %lf", m_maxIterations, time);
			if ( m_proceedAnyway )
				return iterations;
			else
				return -1;
		}
		
		for (size_t i=0;i<m_units.size();i++)
		{
			if ( !m_units[i].mustcall )
				continue;

			/*if ( m_units[i].ncall > 0 )
			{
				notice( "@ time %.2lf, iteration %d for unit %d\n", time, m_units[i].ncall, i );
			}*/

			if ( m_units[i].type->invoke( &m_units[i].context, m_units[i].instance, TCS_INVOKE,
					&m_units[i].values[0], (unsigned int)m_units[i].values.size(),
					time, step, m_units[i].ncall ) < 0 )
			{
				message( TCS_ERROR,"unit %d (%s) type '%s' failed at time %.2lf", i, m_units[i].name.c_str(),
					m_units[i].type->name, time );
				return -2;
			}
			
			m_units[i].mustcall = false;
			m_units[i].ncall++;
			
			// check all values of the current unit
			// for connections to other units to see if their 
			// inputs need to be updated
			for (size_t j=0;j<m_units[i].values.size();j++)
			{
				// reference current output value
				tcsvalue *val1 = &m_units[i].values[j];
				
				// go through each connection attached to this output
				for (size_t k=0;k<m_units[i].conn[j].size();k++)
				{
					connection &c = m_units[i].conn[j][k];
					tcsvalue *val2 = &m_units[c.target_unit].values[c.target_index];
					
					// check that 'val2' and 'val1' are
					// within tolerances of one another
					
					if ( val1->type == TCS_NUMBER 
						&& val2->type == TCS_NUMBER)
					{
						if ( !check_tolerance( val1->data.value, val2->data.value, c.ftol ) )
						{
							// mark units for recalculation and propagate new output value to input									
							val2->data.value = val1->data.value;									
							m_units[c.target_unit].mustcall = true;
						}
					}
					else if ( val1->type == TCS_ARRAY
						&& val2->type == TCS_NUMBER
						&& c.arridx >= 0 && c.arridx < (int)val1->data.array.length )
					{
						if ( !check_tolerance( val1->data.array.values[c.arridx], val2->data.value, c.ftol ))
						{
							val2->data.value = val1->data.array.values[c.arridx];
							m_units[c.target_unit].mustcall = true;
						}
					}
					else if ( val1->type == TCS_ARRAY && val2->type == TCS_ARRAY
						 && val1->data.array.length == val2->data.array.length )
					{
						int len = val1->data.array.length;
						bool pass = true;
						for ( int m=0;m<len;m++ )
							pass = pass && check_tolerance( val1->data.array.values[m],
								val2->data.array.values[m], c.ftol );
						
						if ( !pass )
						{
							// propagate values and mark for recalculation
							for ( int m=0;m<len;m++ )
								val2->data.array.values[m] = val1->data.array.values[m];
							m_units[c.target_unit].mustcall = true;									
						}
					}
					else if ( val1->type == TCS_MATRIX && val2->type == TCS_MATRIX
						&& val1->data.matrix.nrows == val2->data.matrix.nrows
						&& val1->data.matrix.ncols == val2->data.matrix.ncols )
					{
						int len = val1->data.matrix.nrows * val1->data.matrix.ncols;
						bool pass = true;
						for ( int m=0;m<len;m++ )
							pass = pass && check_tolerance( val1->data.matrix.values[m],
								val2->data.matrix.values[m], c.ftol );
						
						if ( !pass )
						{
							// propagate values and mark for recalculation
							for ( int m=0;m<len;m++ )
								val2->data.matrix.values[m] = val1->data.matrix.values[m];
							m_units[c.target_unit].mustcall = true;	
						}
					}
					else
					{
						// type mismatch,
						// dimension mismatch,
						// or cannot compare strings for convergence
						message( TCS_ERROR, "kernel could not check connection between [%d,%d] and [%d,%d]: type mismatch, dimension mismatch, or invalid type connection",
							i, j, c.target_unit, c.target_index);
						return -3;						
					}
				}
			} // loop over all output connections, checking for output->input propagations
			
		} // loop over all units, invoke each if needed, check outputs etc
		
		// check if any units still need to be called
		// if not, then all of them have converged
		converged = true;
		for (size_t i=0;i<m_units.size();i++)
			if (m_units[i].mustcall)
				converged = false;			
				
	} // while loop for convergence at this timestep
	
	return iterations; // success
}

void tcskernel::message( int msgtype, const char *fmt, ... )
{
	char buf[2048];
	va_list ap;
	va_start(ap, fmt);
#if defined(_MSC_VER)||defined(_WIN32)
	_vsnprintf(buf, 2047, fmt, ap);
#else
	vsnprintf(buf, 2047, fmt, ap);
#endif
	va_end(ap);

	buf[2047] = 0;
	
	message( std::string( buf ), msgtype );
}

int tcskernel::simulate( double start, double end, double step )
{
	if ( end <= start || step <= 0 ) 
	{
		message( TCS_ERROR, "invalid time sequence specified (start: %lf end: %lf step: %lf)", start, end, step);
		return -1;
	}

	m_startTime = start;
	m_endTime = end;
	m_timeStep = step;
	
	create_instances(); // allows types to define local storage classes
	
	// call init on each type to setup arrays, internal data, etc
	for (size_t i=0;i<m_units.size();i++)
	{
		if( m_units[i].type->invoke( &m_units[i].context, m_units[i].instance, TCS_INIT,
				&m_units[i].values[0], (unsigned int)m_units[i].values.size(),
				-1, step, -1 )  < 0 )
		{
			message( TCS_ERROR, "unit %d (%s) type '%s' failed at initialization", i, 
				m_units[i].name.c_str(), m_units[i].type->name );
			free_instances();
			return -1;
		}
		// for debugging when running from SSC SDK Tool or SAM - include tcs_debug.h
		//debug_log_init_call(m_units[i].type->name, m_units[i].type, m_units[i].values);

	}
	
	for (size_t i = 0; i < m_units.size(); i++)
	{
		// check all values of the current unit
		// for connections to other units to see if their 
		// inputs need to be updated
		for (size_t j = 0; j < m_units[i].values.size(); j++)
		{
			// reference current output value
			tcsvalue *val1 = &m_units[i].values[j];

			// go through each connection attached to this output
			for (size_t k = 0; k < m_units[i].conn[j].size(); k++)
			{
				connection &c = m_units[i].conn[j][k];
				tcsvalue *val2 = &m_units[c.target_unit].values[c.target_index];

				if (val2->type == TCS_NUMBER && val2->data.value == -999)
				{

					// check that 'val2' and 'val1' are
					// within tolerances of one another

					if (val1->type == TCS_NUMBER
						&& val2->type == TCS_NUMBER)
					{
						if (!check_tolerance(val1->data.value, val2->data.value, c.ftol))
						{
							// mark units for recalculation and propagate new output value to input									
							val2->data.value = val1->data.value;
							m_units[c.target_unit].mustcall = true;
						}
					}
					else if (val1->type == TCS_ARRAY
						&& val2->type == TCS_NUMBER
						&& c.arridx >= 0 && c.arridx < (int)val1->data.array.length)
					{
						if (!check_tolerance(val1->data.array.values[c.arridx], val2->data.value, c.ftol))
						{
							val2->data.value = val1->data.array.values[c.arridx];
							m_units[c.target_unit].mustcall = true;
						}
					}
					else if (val1->type == TCS_ARRAY && val2->type == TCS_ARRAY
						&& val1->data.array.length == val2->data.array.length)
					{
						int len = val1->data.array.length;
						bool pass = true;
						for (int m = 0; m < len; m++)
							pass = pass && check_tolerance(val1->data.array.values[m],
								val2->data.array.values[m], c.ftol);

						if (!pass)
						{
							// propagate values and mark for recalculation
							for (int m = 0; m < len; m++)
								val2->data.array.values[m] = val1->data.array.values[m];
							m_units[c.target_unit].mustcall = true;
						}
					}
					else if (val1->type == TCS_MATRIX && val2->type == TCS_MATRIX
						&& val1->data.matrix.nrows == val2->data.matrix.nrows
						&& val1->data.matrix.ncols == val2->data.matrix.ncols)
					{
						int len = val1->data.matrix.nrows * val1->data.matrix.ncols;
						bool pass = true;
						for (int m = 0; m < len; m++)
							pass = pass && check_tolerance(val1->data.matrix.values[m],
								val2->data.matrix.values[m], c.ftol);

						if (!pass)
						{
							// propagate values and mark for recalculation
							for (int m = 0; m < len; m++)
								val2->data.matrix.values[m] = val1->data.matrix.values[m];
							m_units[c.target_unit].mustcall = true;
						}
					}
					else
					{
						// type mismatch,
						// dimension mismatch,
						// or cannot compare strings for convergence
						message(TCS_ERROR, "kernel could not check connection between [%d,%d] and [%d,%d]: type mismatch, dimension mismatch, or invalid type connection",
							i, j, c.target_unit, c.target_index);
						return -3;
					}
				}
			}
		} // loop over all output connections, checking for output->input propagations
	}

	for( m_currentTime = m_startTime;
		m_currentTime <= m_endTime;
		m_currentTime += m_timeStep )
	{
		// solve all units at each timestep
		int code = solve( m_currentTime, m_timeStep );
		if ( code < 0 )
		{
			free_instances();
			return code - 10;
		}
	
		// call types to notify convergence at 
		// end of timestep if requested
		for (size_t i=0;i<m_units.size();i++)
		{
			if ( m_units[i].type->call_after_convergence > 0 )
			{
				if ( m_units[i].type->invoke( &m_units[i].context, m_units[i].instance, TCS_CONVERGED,
					&m_units[i].values[0], (unsigned int)m_units[i].values.size(),
					m_currentTime, m_timeStep, -2 ) < 0 )
				{
					free_instances();
					message( TCS_ERROR, "unit %d (%s) type '%s' failed at post-convergence at time %lf", i, 
						m_units[i].name.c_str(), m_units[i].type->name, m_currentTime );
					return -3;
				}
			}
		}

		// call convergence virtual as notification (default does nothing)
		// can be used to store simulation outputs in the client, or update
		// simulation progress (and potentially cancel the simulation loop)
		if( !converged( m_currentTime ) ) 
		{
			message( TCS_NOTICE, "simulation aborted at time %.2lf", m_currentTime );
			break;
		}
		
	}
	
	free_instances();
	return 0;
}
					

std::string tcskernel::netlist()
{
	std::ostringstream buf;

	for ( size_t i=0;i<m_units.size(); i++ )
	{
		buf << "unit: " << i << " " << m_units[i].type->name << " '" << m_units[i].name << "'\n";
		for (size_t j=0;j<m_units[i].values.size();j++)
		{
			std::string io = "in";
			if ( m_units[i].type->variables[j].var_type == TCS_OUTPUT ) io = "out";
			else if ( m_units[i].type->variables[j].var_type == TCS_PARAM ) io = "param";
			else if ( m_units[i].type->variables[j].var_type == TCS_DEBUG ) io = "debug";

			buf << "\tvar[" << j << "] " << io << ": " << m_units[i].type->variables[j].name 
				<< "=" << tcsvalue_as_string( m_units[i].values[j] ) << "\n";
		}

		for (size_t j=0;j<m_units[i].conn.size();j++)
		{
			for (size_t k=0;k<m_units[i].conn[j].size();k++)
			{
				connection &c = m_units[i].conn[j][k];
				buf << "\tconn: " << m_units[i].type->variables[j].name << " --> ["
					<< c.target_unit << "." << m_units[c.target_unit].type->variables[c.target_index].name << "] tol " << c.ftol
					<< " arr " << c.arridx << "\n";
			}
		}
		
		buf << "\n";
	}

	return buf.str();
}
