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

#ifndef __lib_vartab_h
#define __lib_vartab_h

#include "../shared/lib_util.h"
#include <string>
#include <vector>
#include "sscapi.h"


#include <unordered_map>
using std::unordered_map;

#ifdef _MSC_VER
#pragma warning(disable: 4290)  // ignore warning: 'C++ exception specification ignored except to indicate a function is not __declspec(nothrow)'
#endif

class var_data;

typedef unordered_map< std::string, var_data* > var_hash;

class var_table
{
public:
	explicit var_table();
	virtual ~var_table();

	void clear();
	var_data *assign( const std::string &name, const var_data &value );
	void unassign( const std::string &name );
	bool rename( const std::string &oldname, const std::string &newname );
	var_data *lookup( const std::string &name );
	const char *first();
	const char *next();
	const char *key(int pos);
	unsigned int size() { return (unsigned int)m_hash.size(); }
	var_table &operator=( const var_table &rhs );

private:
	var_hash m_hash;
	var_hash::iterator m_iterator;
};


class var_data
{
public:
	
	var_data() : type(SSC_INVALID) { num=0.0; }
	var_data( const var_data &cp ) : type(cp.type), num(cp.num), str(cp.str) {  }
	var_data( const std::string &s ) : type(SSC_STRING), str(s) {  }
	var_data(ssc_number_t n) : type(SSC_NUMBER) { num = n; }
	var_data(float n) : type(SSC_NUMBER) { num = n; }
	var_data(int n) : type(SSC_NUMBER) { num = n; }
	var_data(const ssc_number_t *pvalues, int length) : type(SSC_ARRAY) { num.assign(pvalues, (size_t)length); }
	var_data(const ssc_number_t *pvalues, size_t length) : type(SSC_ARRAY) { num.assign(pvalues, length); }
	var_data(const ssc_number_t *pvalues, int nr, int nc) : type(SSC_MATRIX) { num.assign(pvalues, (size_t)nr, (size_t)nc); }

	const char *type_name();
	static std::string type_name(int type);

	std::string to_string();
	static std::string to_string( const var_data &value );

	std::vector<double> arr_vector();
	std::vector<std::vector<double>> matrix_vector();

	static bool parse( unsigned char type, const std::string &buf, var_data &value );

	var_data &operator=(const var_data &rhs) { copy(rhs); return *this; }
	void copy( const var_data &rhs ) { type=rhs.type; num=rhs.num; str=rhs.str; table = rhs.table; }
	
	unsigned char type;
	util::matrix_t<ssc_number_t> num;
	std::string str;
	var_table table;
};

#define VT_GET_INPUT(vt, name, lvalue) \
if (var_data* vd = vt->lookup(name)) lvalue = vd->num; \
else throw std::runtime_error(std::string(name)+std::string(" must be assigned."));


#endif
