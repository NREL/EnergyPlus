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


#include "dllinvoke.h"

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


// to reference count unique dll loads so that local static caches can be cleared and reset
static unsigned int ssc_access_id = 0; 
// dll access handle
static void *ssc_handle = 0;

bool sscdll_load( const char *path )
{
	sscdll_unload();
	ssc_handle = dll_open( path );
	ssc_access_id++;
	
	if ( 0 == ssc_handle || 0 == dll_sym(ssc_handle, "ssc_version"))
	{
		dll_close( ssc_handle );
		ssc_handle = 0;
		return false;
	}
	else
		return true;
}

void sscdll_unload()
{
	if (ssc_handle!=0)
	{
		ssc_access_id++;
		dll_close( ssc_handle );
		ssc_handle = 0;
	}
}

bool sscdll_isloaded()
{
	return (ssc_handle!=0);
}

/*  dynamically linked implementations */

#define CHECK_DLL_LOADED() \
	static unsigned int f_access_id = 0; \
	if (!sscdll_isloaded()) {f=NULL; throw sscdll_error("ssc not loaded", __FUNCTION__); } \
	if (f_access_id!=ssc_access_id) { f=NULL; f_access_id = ssc_access_id; } \
	static const char *func_name = __FUNCTION__

#define FAIL_ON_LOCATE() \
	{ f=NULL; throw sscdll_error("lookup address fail", func_name); }

#define PROCADDR() dll_sym(ssc_handle, func_name)

int ssc_version()
{
	static int (*f)() = NULL;
	CHECK_DLL_LOADED();
	if (!f && 0 == ( f = (int(*)())PROCADDR() )) FAIL_ON_LOCATE();
	return (*f)(); 
}

const char *ssc_build_info()
{
	static const char *(*f)() = NULL;
	CHECK_DLL_LOADED();
	if (!f && 0 == ( f = (const char*(*)())PROCADDR() )) FAIL_ON_LOCATE();
	return (*f)();
}

ssc_data_t ssc_data_create()
{
	static ssc_data_t (*f)() = NULL;
	CHECK_DLL_LOADED();
	if (!f && 0 == ( f = (ssc_data_t(*)())PROCADDR() )) FAIL_ON_LOCATE();
	return (*f)();	
}

void ssc_data_free( ssc_data_t p_data )
{
	static void (*f)(ssc_data_t) = NULL;
	CHECK_DLL_LOADED();
	if (!f && 0 == ( f = (void(*)(ssc_data_t))PROCADDR() )) FAIL_ON_LOCATE();
	(*f)( p_data );
}

void ssc_data_clear( ssc_data_t p_data )
{
	static void (*f)(ssc_data_t) = NULL;
	CHECK_DLL_LOADED();
	if (!f && 0 == ( f = (void(*)(ssc_data_t))PROCADDR() )) FAIL_ON_LOCATE();
	(*f)( p_data );
}

void ssc_data_unassign( ssc_data_t p_data, const char *name )
{
	static void (*f)(ssc_data_t,const char*) = NULL;
	CHECK_DLL_LOADED();
	if (!f && 0 == ( f = (void(*)(ssc_data_t,const char*))PROCADDR() )) FAIL_ON_LOCATE();
	(*f)( p_data, name );
}

int ssc_data_query( ssc_data_t p_data, const char *name )
{
	static int (*f)(ssc_data_t, const char *) = NULL;
	CHECK_DLL_LOADED();
	if (!f && 0 == ( f = (int(*)(ssc_data_t, const char *))PROCADDR() )) FAIL_ON_LOCATE();
	return (*f)( p_data, name );
}

void ssc_data_set_string( ssc_data_t p_data, const char *name, const char *value )
{
	static void (*f)(ssc_data_t, const char*, const char*) = NULL;
	CHECK_DLL_LOADED();
	if (!f && 0 == ( f = (void(*)(ssc_data_t, const char*, const char*))PROCADDR() )) FAIL_ON_LOCATE();
	(*f)(p_data, name, value);
}

void ssc_data_set_number( ssc_data_t p_data, const char *name, ssc_number_t value )
{
	static void (*f)(ssc_data_t, const char*, ssc_number_t) = NULL;
	CHECK_DLL_LOADED();
	if (!f && 0 == ( f = (void(*)(ssc_data_t, const char*, ssc_number_t))PROCADDR() )) FAIL_ON_LOCATE();
	(*f)(p_data, name, value);
}

void ssc_data_set_array( ssc_data_t p_data, const char *name, ssc_number_t *pvalues, int length )
{
	static void (*f)(ssc_data_t, const char*, ssc_number_t*, int) = NULL;
	CHECK_DLL_LOADED();
	if (!f && 0 == ( f = (void(*)(ssc_data_t, const char*, ssc_number_t*, int))PROCADDR() )) FAIL_ON_LOCATE();
	(*f)(p_data, name, pvalues, length);
}

void ssc_data_set_matrix( ssc_data_t p_data, const char *name, ssc_number_t *pvalues, int nrows, int ncols )
{
	static void (*f)(ssc_data_t, const char*, ssc_number_t*, int, int) = NULL;
	CHECK_DLL_LOADED();
	if (!f && 0 == ( f = (void(*)(ssc_data_t, const char*, ssc_number_t*, int, int))PROCADDR() )) FAIL_ON_LOCATE();
	(*f)(p_data, name, pvalues, nrows, ncols);
}

void ssc_data_set_table( ssc_data_t p_data, const char *name, ssc_data_t table )
{
	static void (*f)(ssc_data_t, const char*, ssc_data_t) = 0;
	CHECK_DLL_LOADED();
	if (!f && 0 == ( f = (void(*)(ssc_data_t, const char*, ssc_data_t))PROCADDR() )) FAIL_ON_LOCATE();
	(*f)(p_data, name, table);
}

const char *ssc_data_get_string( ssc_data_t p_data, const char *name )
{
	static const char* (*f)( ssc_data_t, const char* ) = NULL;
	CHECK_DLL_LOADED();
	if (!f && 0 == ( f = (const char*(*)(ssc_data_t, const char*))PROCADDR() )) FAIL_ON_LOCATE();
	return (*f)(p_data, name);
}

ssc_bool_t ssc_data_get_number( ssc_data_t p_data, const char *name, ssc_number_t *value )
{
	static ssc_bool_t (*f)(ssc_data_t, const char*, ssc_number_t*) = NULL;
	CHECK_DLL_LOADED();
	if (!f && 0 == ( f = (ssc_bool_t(*)(ssc_data_t, const char*, ssc_number_t*))PROCADDR() )) FAIL_ON_LOCATE();
	return (*f)(p_data, name, value);
}

ssc_number_t *ssc_data_get_array( ssc_data_t p_data, const char *name, int *length )
{
	static ssc_number_t* (*f)(ssc_data_t, const char*, int*) = NULL;
	CHECK_DLL_LOADED();
	if (!f && 0 == (f = (ssc_number_t*(*)(ssc_data_t, const char*, int*))PROCADDR() )) FAIL_ON_LOCATE();
	return (*f)( p_data, name, length );
}

ssc_number_t *ssc_data_get_matrix( ssc_data_t p_data, const char *name, int *nrows, int *ncols )
{
	static ssc_number_t* (*f)(ssc_data_t, const char*, int*, int*) = NULL;
	CHECK_DLL_LOADED();
	if (!f && 0 == (f = (ssc_number_t*(*)(ssc_data_t, const char*, int*, int*))PROCADDR())) FAIL_ON_LOCATE();
	return (*f)( p_data, name, nrows, ncols );
}

ssc_data_t ssc_data_get_table( ssc_data_t p_data, const char *name )
{
	static ssc_data_t (*f)(ssc_data_t, const char*) = NULL;
	CHECK_DLL_LOADED();
	if (!f && 0 == (f = (ssc_data_t(*)(ssc_data_t, const char*))PROCADDR() )) FAIL_ON_LOCATE();
	return (*f)( p_data, name );
}

#define DYNAMICCALL_CONSTCHARSTAR__SSCDATAT() \
	static const char *(*f)(ssc_data_t) = NULL; \
	CHECK_DLL_LOADED(); \
	if (!f && 0 == (f = (const char*(*)(ssc_data_t))PROCADDR())) FAIL_ON_LOCATE(); \
	return (*f)( p_data );

const char *ssc_data_first( ssc_data_t p_data )
{
	DYNAMICCALL_CONSTCHARSTAR__SSCDATAT();
}

const char *ssc_data_next( ssc_data_t p_data )
{
	DYNAMICCALL_CONSTCHARSTAR__SSCDATAT();
}

ssc_entry_t ssc_module_entry( int index )
{
	static ssc_entry_t (*f)(int) = NULL;
	CHECK_DLL_LOADED();
	if (!f && 0 == (f = (ssc_entry_t(*)(int))PROCADDR())) FAIL_ON_LOCATE();
	return (*f)( index );
}

const char *ssc_entry_name( ssc_entry_t p_entry )
{
	static const char* (*f)(ssc_entry_t) = NULL;
	CHECK_DLL_LOADED();
	if (!f && 0 == (f = (const char* (*)(ssc_entry_t))PROCADDR())) FAIL_ON_LOCATE();
	return (*f)( p_entry );
}

const char *ssc_entry_description( ssc_entry_t p_entry )
{
	static const char* (*f)(ssc_entry_t) = NULL;
	CHECK_DLL_LOADED();
	if (!f && 0 == (f = (const char* (*)(ssc_entry_t))PROCADDR())) FAIL_ON_LOCATE();
	return (*f)( p_entry );
}

int ssc_entry_version( ssc_entry_t p_entry )
{
	static int (*f)(ssc_entry_t) = NULL;
	CHECK_DLL_LOADED();
	if (!f && 0 == (f = (int (*)(ssc_entry_t))PROCADDR())) FAIL_ON_LOCATE();
	return (*f)( p_entry );
}

ssc_module_t ssc_module_create( const char *name )
{
	static ssc_module_t (*f)( const char * ) = NULL;
	CHECK_DLL_LOADED();
	if (!f && 0 == (f = (ssc_module_t (*)(const char*))PROCADDR())) FAIL_ON_LOCATE();
	return (*f)( name );
}

void ssc_module_free( ssc_module_t p_mod )
{
	static void (*f)(ssc_module_t) = NULL;
	CHECK_DLL_LOADED();
	if (!f && 0 == (f = (void(*)(ssc_module_t))PROCADDR())) FAIL_ON_LOCATE();	
	(*f)( p_mod );
}

const ssc_info_t ssc_module_var_info( ssc_module_t p_mod, int index )
{
	static ssc_info_t(*f)(ssc_module_t, int) = NULL;
	CHECK_DLL_LOADED();
	if (!f && 0 == (f = (ssc_info_t(*)(ssc_module_t,int))PROCADDR())) FAIL_ON_LOCATE();
	return (*f)( p_mod, index );
}

#define DYNAMICCALL_INT__SSCINFOT() \
	static int (*f)(ssc_info_t) = NULL; \
	CHECK_DLL_LOADED(); \
	if (!f && 0 == (f = (int(*)(ssc_info_t))PROCADDR())) FAIL_ON_LOCATE(); \
	return (*f)(p_inf);

#define DYNAMICCALL_CONSTCHARSTAR__SSCINFOT() \
	static const char* (*f)(ssc_info_t) = NULL; \
	CHECK_DLL_LOADED(); \
	if (!f && 0 == (f = (const char*(*)(ssc_info_t))PROCADDR())) FAIL_ON_LOCATE(); \
	return (*f)(p_inf);

int ssc_info_var_type( ssc_info_t p_inf )
{
	DYNAMICCALL_INT__SSCINFOT();
}

int ssc_info_data_type( ssc_info_t p_inf )
{
	DYNAMICCALL_INT__SSCINFOT();
}

const char *ssc_info_name( ssc_info_t p_inf )
{
	DYNAMICCALL_CONSTCHARSTAR__SSCINFOT();
}

const char *ssc_info_label( ssc_info_t p_inf )
{
	DYNAMICCALL_CONSTCHARSTAR__SSCINFOT();
}

const char *ssc_info_units( ssc_info_t p_inf )
{
	DYNAMICCALL_CONSTCHARSTAR__SSCINFOT();
}

const char *ssc_info_meta( ssc_info_t p_inf )
{
	DYNAMICCALL_CONSTCHARSTAR__SSCINFOT();
}

const char *ssc_info_group( ssc_info_t p_inf )
{
	DYNAMICCALL_CONSTCHARSTAR__SSCINFOT();
}

const char *ssc_info_required( ssc_info_t p_inf )
{
	DYNAMICCALL_CONSTCHARSTAR__SSCINFOT();
}

const char *ssc_info_constraints( ssc_info_t p_inf )
{
	DYNAMICCALL_CONSTCHARSTAR__SSCINFOT();
}

const char *ssc_info_uihint( ssc_info_t p_inf )
{
	DYNAMICCALL_CONSTCHARSTAR__SSCINFOT();
}

ssc_bool_t ssc_module_exec_simple( const char *name, ssc_data_t p_data )
{
	static ssc_bool_t (*f)(const char*, ssc_data_t) = NULL;
	CHECK_DLL_LOADED();
	if (!f && 0 == (f = (ssc_bool_t(*)(const char*,ssc_data_t))PROCADDR())) FAIL_ON_LOCATE();

	return (*f)( name, p_data );
}


const char *ssc_module_exec_simple_nothread( const char *name, ssc_data_t p_data )
{
	static const char *(*f)(const char *, ssc_data_t) = NULL;
	CHECK_DLL_LOADED();
	if (!f && 0 == (f = (const char*(*)(const char *, ssc_data_t))PROCADDR())) FAIL_ON_LOCATE();
	return (*f)(name, p_data);
}

ssc_bool_t ssc_module_exec( ssc_module_t p_mod, ssc_data_t p_data )
{
	static ssc_bool_t (*f)( ssc_module_t, ssc_data_t ) = NULL;
	CHECK_DLL_LOADED();
	if (!f && 0 == (f = (ssc_bool_t(*)(ssc_module_t,ssc_data_t))PROCADDR())) FAIL_ON_LOCATE();
	return (*f)( p_mod, p_data );
}

ssc_bool_t ssc_module_exec_with_handler( 
	ssc_module_t p_mod, 
	ssc_data_t p_data, 
	ssc_bool_t (*pf_handler)( ssc_module_t, ssc_handler_t, int action, float f0, float f1, const char *s0, const char *s1, void *user_data ),
	void *pf_user_data )
{
	static ssc_bool_t (*f) ( ssc_module_t,
		ssc_data_t,
		ssc_bool_t (*)(ssc_module_t, ssc_handler_t, int, float, float, const char *, const char *, void *),
		void *) = NULL;

	CHECK_DLL_LOADED();
	if (!f && 0 == (f = (ssc_bool_t (*) ( ssc_module_t,
		ssc_data_t,
		ssc_bool_t (*)(ssc_module_t, ssc_handler_t, int, float, float, const char *, const char *, void *),
		void *) ) PROCADDR())) FAIL_ON_LOCATE();

	return (*f)( p_mod, p_data, pf_handler, pf_user_data );
}

void ssc_module_extproc_output( ssc_handler_t p_mod, const char *output_line )
{
	static void (*f)(ssc_handler_t, const char*) = NULL;
	CHECK_DLL_LOADED();
	if (!f && 0 == ( f = (void(*)(ssc_handler_t,const char*))PROCADDR())) FAIL_ON_LOCATE();
	(*f)(p_mod, output_line);
}

const char *ssc_module_log( ssc_module_t p_mod, int index, int *item_type, float *time )
{
	static const char *(*f)(ssc_module_t, int, int*, float*) = NULL;
	CHECK_DLL_LOADED();
	if (!f && 0 == (f = (const char*(*)(ssc_module_t, int, int*, float*))PROCADDR())) FAIL_ON_LOCATE();
	return (*f)( p_mod, index, item_type, time );
}

void __ssc_segfault()
{
	static void (*f)() = NULL;
	CHECK_DLL_LOADED();
	if (!f && 0 == (f = (void(*)())PROCADDR())) FAIL_ON_LOCATE();
	(*f)();
}

/* include shared ssc code here */
#include <lib_util.cpp>
#include <vartab.cpp>

