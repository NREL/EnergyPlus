/*******************************************************************************************************
*  Copyright 2017 Alliance for Sustainable Energy, LLC
*
*  NOTICE: This software was developed at least in part by Alliance for Sustainable Energy, LLC
*  (“Alliance”) under Contract No. DE-AC36-08GO28308 with the U.S. Department of Energy and the U.S.
*  The Government retains for itself and others acting on its behalf a nonexclusive, paid-up,
*  irrevocable worldwide license in the software to reproduce, prepare derivative works, distribute
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
*  the underlying software originally provided by Alliance as “System Advisor Model” or “SAM”. Except
*  to comply with the foregoing, the terms “System Advisor Model”, “SAM”, or any confusingly similar
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

#ifndef __ssc_core_h
#define __ssc_core_h


#include <iostream>
#include <string>
#include <vector>
#include <algorithm>
#include <exception>
#include <cstdarg>
#include <cmath>
#include <limits>
#include <memory>

/* Macros for C++11 support */
template <typename T>
struct smart_ptr 
{
	#if __cplusplus <= 201103L
	typedef std::unique_ptr<T> ptr;
	#else
	typedef std::auto_ptr<T> ptr;
	#endif
};

/* Macros require for building
	__32BIT__ *or* __64BIT__
	__WINDOWS__ *or* __UNIX__
	_DEBUG *if debug mode*
*/

#ifdef _MSC_VER
#define __COMPILER__  "Visual C++"
#pragma warning(disable: 4290)  // ignore warning: 'C++ exception specification ignored except to indicate a function is not __declspec(nothrow)'
#endif
#ifdef __GNUC__
#define __COMPILER__ "GNU/C++"
#endif

#ifdef __WINDOWS__
#define __PLATFORM__ "Windows"
#endif
#ifdef __UNIX__
#define __PLATFORM__ "Linux"
#endif
#ifdef __MACOSX__
#define __PLATFORM__ "OS X"
#endif
#ifdef __IOS__
#define __PLATFORM__ "iOS"
#endif
#ifdef __ANDROID__
#define __PLATFORM__ "Android"
#endif

#ifdef __32BIT__
#define __ARCH__ "32 bit"
#define __ARCHBITS__ 32
#endif
#ifdef __64BIT__
#define __ARCH__ "64 bit"
#define __ARCHBITS__ 64
#endif

#ifdef _DEBUG
#define __BUILD__ "Debug"
#define __DEBUG__ 1
#else
#define __BUILD__ "Release"
#define __DEBUG__ 0
#endif

//#include "lib_util.h"
#include "vartab.h"
#include "sscapi.h"

struct var_info
{
	int var_type; //  SSC_INVALID, SSC_INPUT, SSC_OUTPUT, SSC_INOUT
	unsigned char data_type; // SSC_INVALID, SSC_STRING, SSC_NUMBER, SSC_ARRAY, SSC_MATRIX
	
	const char *name;
	const char *label;
	const char *units;
	const char *meta;
	const char *group;
	const char *required_if; // e.g. always required: "*"  optional: "?"  depends: "pv.model=3" "pv.model=3|pv.size<3" "pv.model=3&pv.tilt=33.2"
	const char *constraints; // e.g. "MIN=10,MAX=12,NVALUES=23,ARRAYLEN=8760,TMYEPW,LOCAL_FILE,MXH_SCHEDULE"
	const char *ui_hint; // e.g. "hide,decpt=3" so hints for a user interface's treatment of this variable
};

extern const var_info var_info_invalid;

class handler_interface; // forward decl

class compute_module
{
public:
	class log_item
	{
	public:
		log_item() { }
		log_item(int t, const std::string &s, float f=-1.0) 
			: type(t), text(s), time(f) {  }

		int type;
		std::string text;
		float time;
	};
	
	class general_error : public std::exception
	{
	public:
		general_error(const std::string &s, float t=-1.0) : err_text(s), time(t) { }
		virtual ~general_error() throw() { }
		std::string err_text;
		float time;
	};

	class cast_error : public general_error
	{
	public:
		cast_error(const char *target_type, var_data &source, const std::string &name)
			: general_error( "cast fail: <" + std::string(target_type) + "> from " + std::string(source.type_name()) + " for: " + name ) { }
	};

	class check_error : public general_error
	{
	public:
		check_error( const std::string &cur_var, const std::string &reason, const std::string &expr )
			: general_error( "check fail: reason " + reason + ", with '" + expr + "' for: " + cur_var ) {  }
	};

	class constraint_error : public general_error
	{
	public:
		constraint_error( const std::string &cur_var, const std::string &reason, const std::string &expr )
			: general_error( "constraint fail: reason " + reason + ", with '" + expr + "' for: " + cur_var ) {  }
	};

	class exec_error : public general_error
	{
	public:
		exec_error( const std::string &mod_name, const std::string &reason )
			: general_error( "exec fail(" + mod_name + "): " + reason ) {  }
	};

	class mismatch_error : public general_error
	{
	public:
		mismatch_error( int required, int specified, const std::string &reason )
			: general_error(util::format("size mismatch error with %d required, but %d given: %s", required, specified, reason.c_str())) {  }
	};

	class timestep_error : public general_error
	{
	public:
		timestep_error( double start, double end, double step, const char *reason )
			: general_error( util::format("timestep fail(%lg %lg %lg): %s", start, end, step, reason) ) {  }
	};

public:
	compute_module( ); // cannot be created directly - has a pure virtual function
	virtual ~compute_module();

	bool update( const std::string &current_action, float percent_done, float time=-1.0 );
	void log( const std::string &msg, int type=SSC_NOTICE, float time=-1.0 );
	bool extproc( const std::string &command, const std::string &workdir );
	void clear_log();
	log_item *log(int index);
	var_info *info(int index);
		
	bool compute( handler_interface *handler, var_table *data );
		

	/* on_extproc_output: this function will be called by the
	   registered handler interface when it receives standard output
	   from an externally running child process (i.e. trnsys).  it can
	   be used to determine whether the child process produced error
	   messages, or if it succeeded correctly.  it is also
	   for retrieving the resulting data if the process calculation
	   result is dumped to stdout.
	   
	   if the function returns 'true', the output data was processed
	   and does not need to be reported.  returning false causes
	   the output string to be sent to the log as a NOTICE
	*/
	virtual bool on_extproc_output( const std::string & ) { return false; }	
	
protected:
	/* must be implemented to perform calculations
	   note: can throw exceptions of type 'compute_module::error' */
	virtual void exec( ) throw( general_error ) = 0;

	
	/* can be called in constructors to build up the variable table references */
	void add_var_info( var_info vi[] );
	void build_info_map();
	bool has_info_map() { return m_infomap!=NULL; }
	
	/* can be called in exec if determine shouldn't run module */
	void remove_var_info(var_info vi[]);


public:
	/* for working with input/output/inout variables during 'compute'*/
	const var_info &info( const std::string &name ) throw( general_error );
	bool is_ssc_array_output( const std::string &name ) throw( general_error );
	var_data *lookup( const std::string &name ) throw( general_error );
	var_data *assign( const std::string &name, const var_data &value ) throw( general_error );
	ssc_number_t *allocate( const std::string &name, size_t length ) throw( general_error );
	ssc_number_t *allocate( const std::string &name, size_t nrows, size_t ncols ) throw( general_error );
	util::matrix_t<ssc_number_t>& allocate_matrix( const std::string &name, size_t nrows, size_t ncols ) throw( general_error );
	var_data &value( const std::string &name ) throw( general_error );
	bool is_assigned( const std::string &name ) throw( general_error );
	size_t as_unsigned_long(const std::string &name) throw(general_error);
	int as_integer( const std::string &name ) throw( general_error );
	bool as_boolean( const std::string &name ) throw( general_error );
	float as_float( const std::string &name ) throw( general_error );
	ssc_number_t as_number( const std::string &name ) throw( general_error );
	double as_double( const std::string &name ) throw( general_error );
	const char *as_string( const std::string &name ) throw( general_error );
	ssc_number_t *as_array( const std::string &name, size_t *count ) throw( general_error );
	std::vector<int> as_vector_integer(const std::string &name) throw(general_error);
	std::vector<ssc_number_t> as_vector_ssc_number_t(const std::string &name) throw(general_error);
	std::vector<double> as_vector_double( const std::string &name ) throw( general_error );
	std::vector<float> as_vector_float(const std::string &name) throw(general_error);
	std::vector<bool> as_vector_bool(const std::string &name) throw(general_error);
	std::vector<size_t> as_vector_unsigned_long(const std::string &name) throw(general_error);
	ssc_number_t *as_matrix( const std::string &name, size_t *rows, size_t *cols ) throw( general_error );
	util::matrix_t<double> as_matrix(const std::string & name) throw(general_error);
	util::matrix_t<size_t> as_matrix_unsigned_long(const std::string & name) throw(general_error);
	util::matrix_t<double> as_matrix_transpose(const std::string & name) throw(general_error);
	bool get_matrix(const std::string &name, util::matrix_t<ssc_number_t> &mat) throw(general_error);

	size_t check_timestep_seconds( double t_start, double t_end, double t_step ) throw( timestep_error );
	
	ssc_number_t accumulate_annual(const std::string &hourly_var, const std::string &annual_var, double scale=1.0) throw(exec_error);
	ssc_number_t *accumulate_monthly(const std::string &hourly_var, const std::string &annual_var, double scale=1.0) throw(exec_error);

	ssc_number_t accumulate_annual_for_year(const std::string &hourly_var, const std::string &annual_var, double scale, size_t step_per_hour, size_t year = 1, size_t steps = 8760) throw(exec_error);
	ssc_number_t *accumulate_monthly_for_year(const std::string &hourly_var, const std::string &annual_var, double scale, size_t step_per_hour, size_t year = 1) throw(exec_error);

private:
	// called by 'compute' as necessary for precheck and postcheck
	bool verify(const std::string &phase, int var_types) throw( general_error );
	
	bool check_required( const std::string &name ) throw( general_error );
	bool check_constraints( const std::string &name, std::string &fail_text ) throw( general_error );

	// helper functions for check_required
	ssc_number_t get_operand_value( const std::string &input, const std::string &cur_var_name ) throw( general_error );

	var_data m_null_value;
	
	std::vector< var_info* > m_varlist;
	std::vector< log_item > m_loglist;
	
	unordered_map< std::string, var_info* > *m_infomap;

	/* these members are take values only during a call to 'compute(..)'
	  and are NULL otherwise */
	handler_interface   *m_handler;
	var_table           *m_vartab;
};


class handler_interface
{
private:
	compute_module *m_cm;
protected:
	handler_interface( compute_module *cm ) : m_cm(cm) {  }
public:
	virtual ~handler_interface() {  /* nothing to do */ }
	virtual void on_log( const std::string &text, int type, float time ) = 0;
	virtual bool on_update( const std::string &text, float percent_done, float time ) = 0;
//	virtual bool on_exec( const std::string &command, const std::string &workdir ) = 0;

	compute_module *module() { return m_cm; }

	void on_stdout( const std::string &text )
		{ if (!m_cm) return;
		  if (!m_cm->on_extproc_output(text)) m_cm->log( "stdout(child): " + text, SSC_NOTICE ); }
};



#define DEFINE_MODULE_ENTRY( name, desc, ver ) \
	static compute_module *_create_ ## name () { return new cm_ ## name; } \
	module_entry_info cm_entry_ ## name = { \
		#name, desc, ver, _create_ ## name }; \

#define DEFINE_TCS_MODULE_ENTRY( name, desc, ver ) \
	static compute_module *_create_ ## name() { extern tcstypeprovider sg_tcsTypeProvider; return new cm_ ## name(&sg_tcsTypeProvider); } \
	module_entry_info cm_entry_ ## name = { \
		#name, desc, ver, _create_ ## name }; \

struct module_entry_info
{
	const char *name;
	const char *description;
	int version;
	compute_module * (*f_create)();
};


#endif
