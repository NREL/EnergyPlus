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

#include <string>
#include <vector>

#include <unordered_map>
using std::unordered_map;

#include "../shared/lib_util.h"
#include "sscapi.h"

#ifdef _MSC_VER
#pragma warning(disable: 4290)  // ignore warning: 'C++ exception specification ignored except to indicate a function is not __declspec(nothrow)'
#pragma warning(disable: 4297)	// ignore warning: 'function assumed not to throw an exception but does'
#endif

class var_data;

typedef unordered_map< std::string, var_data* > var_hash;

class var_table
{
public:
	var_table();
    var_table(const var_table &rhs);
    virtual ~var_table();
	var_table &operator=( const var_table &rhs );

	void clear();
    bool is_assigned( const std::string &name );
    void unassign( const std::string &name );
    bool rename( const std::string &oldname, const std::string &newname );
    bool rename_match_case( const std::string &oldname, const std::string &newname );
	const char *first();
	const char *next();
	const char *key(int pos);
	unsigned int size() { return (unsigned int)m_hash.size(); }

    // setters
    ssc_number_t *allocate( const std::string &name, size_t length );
    ssc_number_t *allocate( const std::string &name, size_t nrows, size_t ncols );
    util::matrix_t<ssc_number_t>& allocate_matrix( const std::string &name, size_t nrows, size_t ncols );
	var_data *assign( const std::string &name, const var_data &value );
    var_data *assign_match_case( const std::string &name, const var_data &value );
    void merge(const var_table &rhs, bool overwrite_existing);

	// getters
	var_data *lookup( const std::string &name );
	var_data *lookup_match_case( const std::string &name );
    size_t as_unsigned_long(const std::string &name);
    int as_integer( const std::string &name );
    bool as_boolean( const std::string &name );
    float as_float( const std::string &name );
    ssc_number_t as_number( const std::string &name );
    double as_double( const std::string &name );
    const char *as_string( const std::string &name );
    ssc_number_t *as_array( const std::string &name, size_t *count );
    std::vector<int> as_vector_integer(const std::string &name);
    std::vector<ssc_number_t> as_vector_ssc_number_t(const std::string &name);
    std::vector<double> as_vector_double( const std::string &name );
    std::vector<float> as_vector_float(const std::string &name);
    std::vector<bool> as_vector_bool(const std::string &name);
    std::vector<size_t> as_vector_unsigned_long(const std::string &name);
    ssc_number_t *as_matrix( const std::string &name, size_t *rows, size_t *cols );
    util::matrix_t<double> as_matrix(const std::string & name);
    util::matrix_t<size_t> as_matrix_unsigned_long(const std::string & name);
    util::matrix_t<double> as_matrix_transpose(const std::string & name);
    bool get_matrix(const std::string &name, util::matrix_t<ssc_number_t> &mat);

    unordered_map< std::string, var_data*>* get_hash() {return &m_hash;};
private:
	var_hash m_hash;
	var_hash::iterator m_iterator;
};


class var_data
{
public:

	var_data() : type(SSC_INVALID) { num=0.0; }
	var_data( const var_data &cp ) { copy(cp); }
    var_data( const std::string &s ) : type(SSC_STRING), str(s) {  }
	var_data(ssc_number_t n) : type(SSC_NUMBER) { num = n; }
	var_data(float n) : type(SSC_NUMBER) { num = n; }
	var_data(int n) : type(SSC_NUMBER) { num = n; }
    var_data(std::vector<double> arr) : type(SSC_ARRAY) { if (!arr.empty()) num.assign(&arr[0], arr.size()); }
    var_data(std::vector<int> arr);
    var_data(const ssc_number_t *pvalues, int length) : type(SSC_ARRAY) { num.assign(pvalues, (size_t)length); }
	var_data(const ssc_number_t *pvalues, size_t length) : type(SSC_ARRAY) { num.assign(pvalues, length); }
	var_data(const ssc_number_t *pvalues, int nr, int nc) : type(SSC_MATRIX) { num.assign(pvalues, (size_t)nr, (size_t)nc); }
    var_data(const util::matrix_t<ssc_number_t>& matrix): type(SSC_MATRIX) { num = matrix; }
    var_data(const var_table& vt) : type(SSC_TABLE) {table = vt; }
	var_data(const std::vector<var_data>& vd_vec): type(SSC_DATARR) { vec = vd_vec; }
    var_data(const std::vector<std::vector<var_data>>& vd_mat): type(SSC_DATMAT) { mat = vd_mat; }


    const char *type_name();
	static std::string type_name(int type);

	std::string to_string();
	static std::string to_string( const var_data &value );

	std::vector<double> arr_vector();
	std::vector<std::vector<double>> matrix_vector();

	static bool parse( unsigned char type, const std::string &buf, var_data &value );

	var_data &operator=(const var_data &rhs) { copy(rhs); return *this; }
	void copy( const var_data &rhs ) {
	    type=rhs.type;
	    num=rhs.num;
	    str=rhs.str;
	    table = rhs.table;
	    for (const auto& i : rhs.vec){
	        vec.push_back(i);
	    }
        for (const auto& i : rhs.mat){
            std::vector<var_data> vt;
            for (const auto& j : i){
                vt.push_back(j);
            }
            mat.push_back(vt);
        }
	}

	void clear(){
	    type = SSC_INVALID;
	    num.clear();
	    str.clear();
	    table.clear();
	    vec.clear();
	    mat.clear();
	}

	unsigned char type;
	util::matrix_t<ssc_number_t> num;
	std::string str;
	var_table table;
	std::vector<var_data> vec;
    std::vector<std::vector<var_data>> mat;

};

class general_error : public std::exception
{
public:
    explicit general_error(std::string s, float t=-1.0) : err_text(move(s)), time(t) { }
    std::string err_text;
    float time;
};

class cast_error : public general_error
{
public:
    cast_error(const char *target_type, var_data &source, const std::string &name)
            : general_error( "cast fail: <" + std::string(target_type) + "> from " + std::string(source.type_name()) + " for: " + name ) { }
};

void vt_get_int(var_table* vt, const std::string& name, int* lvalue);

void vt_get_uint(var_table* vt, const std::string& name, size_t* lvalue);

void vt_get_bool(var_table* vt, const std::string& name, bool* lvalue);

void vt_get_number(var_table* vt, const std::string& name, double* lvalue);

void vt_get_array_vec(var_table* vt, const std::string& name, std::vector<double>& vec_double);

void vt_get_array_vec(var_table* vt, const std::string& name, std::vector<int>& vec_int);

void vt_get_matrix(var_table* vt, const std::string& name, util::matrix_t<double>& mat);

void vt_get_matrix_vec(var_table* vt, const std::string& name, std::vector<std::vector<double>>& mat);


#endif
