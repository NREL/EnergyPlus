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

#include "lib_util.h"
#include "vartab.h"

static const char *var_data_types[] =
{	"<invalid>", // SSC_INVALID
	"<string>",  // SSC_STRING
	"<number>",  // SSC_NUMBER
	"<array>",   // SSC_ARRAY
	"<matrix>",  // SSC_MATRIX
	"<table>",   // SSC_TABLE
	NULL };

var_data::var_data(std::vector<int> arr) : type(SSC_ARRAY) {
    num.resize(arr.size());
    for (size_t i = 0; i < arr.size(); i++) {
        num[i] = (ssc_number_t)arr[i];
    }
}

const char *var_data::type_name()
{
	if (type < 6) return var_data_types[ (int)type ];
	else return NULL;
}

std::string var_data::type_name(int type)
{
	if (type >= 0 && type < 5) return var_data_types[ (int)type ];
	else return "";
}


std::string var_data::to_string()
{
	return var_data::to_string( *this );
}

std::string var_data::to_string( const var_data &value )
{
	switch( value.type )
	{
	case SSC_STRING:
		return value.str;
	case SSC_NUMBER:
		return util::to_string( value.num.value() );
	case SSC_ARRAY:
		{
			std::string s;
			for (size_t i=0;i<value.num.length();i++)
			{
				s += util::to_string( (double) value.num[i] );
				if ( i < value.num.length()-1 )	s += ',';
			}
			return s;
		}
	case SSC_MATRIX:
		{
			std::string s;
			for (size_t r=0;r<value.num.nrows();r++)
			{
				s += "[";
				for (size_t c=0; c<value.num.ncols();c++)
				{
					s += util::to_string( (double) value.num.at(r,c) );
					if ( c < value.num.ncols()-1 ) s += ' ';
				}
				s += "]";
			}
			return s;
		}
	}

	return "<invalid>";
}

std::vector<double> var_data::arr_vector()
{
    if (type != SSC_ARRAY)
        throw std::runtime_error("arr_vector error: var_data type not SSC_ARRAY.");
    std::vector<double> v;
    for (unsigned int i = 0; i < num.length(); i++){
        v.push_back(num[i]);
    }
    return v;
}

std::vector<std::vector<double>> var_data::matrix_vector()
{
    if (type != SSC_MATRIX)
        throw std::runtime_error("arr_matrix error: var_data type not SSC_MATRIX.");
    std::vector<std::vector<double>> v;
    for (unsigned int i = 0; i < num.nrows(); i++){
        std::vector<double> row;
        for (unsigned int j = 0; j < num.ncols(); j++){
            row.push_back(num.at(i, j));
        }
        v.push_back(row);
    }
    return v;
}

bool var_data::parse( unsigned char type, const std::string &buf, var_data &value )
{
	switch(type)
	{
	case SSC_STRING:
		{
			value.type = SSC_STRING;
			value.str = buf;
			return true;
		}
	case SSC_NUMBER:
		{
			double x;
			if (util::to_double(buf, &x))
			{
				value.type = SSC_NUMBER;
				value.num = (ssc_number_t)x;
				return true;
			}
			else
				return false;

		}
	case SSC_ARRAY:
		{
			std::vector<std::string> tokens = util::split(buf," ,\t[]\n");
			value.type = SSC_ARRAY;
			value.num.resize_fill( tokens.size(), 0.0 );
			for (size_t i=0; i<tokens.size(); i++)
			{
				double x;
				if (util::to_double( tokens[i], &x ))
					value.num[i] = (ssc_number_t) x;
				else
					return false;
			}
			return true;
		}
	case SSC_MATRIX:
		{
			std::vector<std::string> rows = util::split(buf,"[]\n");
			if (rows.size() < 1) return false;
			std::vector<std::string> cur_row = util::split(rows[0], " ,\t");
			if (cur_row.size() < 1) return false;

			value.type = SSC_MATRIX;
			value.num.resize_fill( rows.size(), cur_row.size(), 0.0 );

			for( size_t c=0; c < cur_row.size(); c++)
			{
				double x;
				if (util::to_double(cur_row[c], &x)) value.num.at(0,c) = (ssc_number_t)x;
			}

			for (size_t r=1; r < rows.size(); r++)
			{
				cur_row = util::split(rows[r], " ,\t");
				for (size_t c=0; c<cur_row.size() && c<value.num.ncols(); c++)
				{
					double x;
					if (util::to_double(cur_row[c], &x))
						value.num.at(r,c) = (ssc_number_t)x;
				}
			}
			return true;
		}
	}

	return false;
}

var_table::var_table() : m_iterator(m_hash.begin())
{
	/* nothing to do here */
}

var_table::var_table(const var_table &rhs) : var_table() {
    operator=(rhs);
}

var_table::~var_table()
{
	clear();
}

var_table &var_table::operator=( const var_table &rhs )
{
	clear();

	for ( var_hash::const_iterator it = rhs.m_hash.begin();
		it != rhs.m_hash.end();
		++it )
		assign_match_case( (*it).first, *((*it).second) );

	return *this;
}

void var_table::clear()
{
	for (var_hash::iterator it = m_hash.begin(); it != m_hash.end(); ++it)
	{
		// debug heap corruption
		delete it->second; // delete the var_data object
	}
	if (!m_hash.empty()) m_hash.clear();
}

var_data *var_table::assign( const std::string &name, const var_data &val )
{
	var_data *v = lookup(name);
	if (!v)
	{
		v = new var_data;
		m_hash[ util::lower_case(name) ] = v;
	}

	v->copy(val);
	return v;
}

var_data *var_table::assign_match_case( const std::string &name, const var_data &val )
{
    var_data *v = lookup(name);
    if (!v)
    {
        v = new var_data;
        m_hash[ name ] = v;
    }

    v->copy(val);
    return v;
}

void var_table::merge(const var_table &rhs, bool overwrite_existing){
    for ( var_hash::const_iterator it = rhs.m_hash.begin();
          it != rhs.m_hash.end();
          ++it ){
        if (is_assigned(it->first)){
            if (overwrite_existing)
                assign_match_case( (*it).first, *((*it).second) );
        }
        else
            assign_match_case( (*it).first, *((*it).second) );
    }
}


bool var_table::is_assigned( const std::string &name )
{
    return (lookup(name) != 0);
}

void var_table::unassign( const std::string &name )
{
	var_hash::iterator it = m_hash.find( util::lower_case(name) );
	if (it != m_hash.end())
	{
		delete (*it).second; // delete the associated data
		m_hash.erase( it );
	}
}

bool var_table::rename( const std::string &oldname, const std::string &newname )
{
    return rename_match_case(util::lower_case(oldname), util::lower_case(newname));
}

bool var_table::rename_match_case( const std::string &oldname, const std::string &newname )
{

    var_hash::iterator it = m_hash.find( oldname );
    if ( it != m_hash.end() )
    {
        std::string lcnewname( newname );

        var_data *data = it->second; // save ptr to data
        m_hash.erase( it );

        // if a variable with 'newname' already exists,
        // delete its data, and reassign the name to the new data
        it = m_hash.find( lcnewname );
        if ( it != m_hash.end() )
        {
            delete it->second;
            it->second = data;
        }
        else // otherwise, just add a new itme
            m_hash[ lcnewname ] = data;

        return true;
    }
    else
        return false;
}

var_data *var_table::lookup( const std::string &name )
{
    var_hash::iterator it = m_hash.find(name);
    if (it == m_hash.end())
      it = m_hash.find( util::lower_case(name));
    if ( it != m_hash.end() )
      return (*it).second;
    else
      return NULL;
}

var_data *var_table::lookup_match_case( const std::string &name )
{
    var_hash::iterator it = m_hash.find( name );
    if ( it != m_hash.end() )
        return (*it).second;
    else
        return NULL;
}

const char *var_table::first( )
{
	m_iterator = m_hash.begin();
	if (m_iterator != m_hash.end())
		return m_iterator->first.c_str();
	else
		return NULL;
}

const char *var_table::key(int pos){
    m_iterator = m_hash.begin();
    if (m_iterator == m_hash.end()) return NULL;

    int n = 0;
    for (n = 0; n < pos; n++)
        ++m_iterator;

    if (m_iterator != m_hash.end())
        return m_iterator->first.c_str();
    return NULL;
}

const char *var_table::next()
{
	if (m_iterator == m_hash.end()) return NULL;

	++m_iterator;

	if (m_iterator != m_hash.end())	return m_iterator->first.c_str();

	return NULL;
}

void vt_get_int(var_table* vt, const std::string& name, int* lvalue) {
	if (var_data* vd = vt->lookup(name)) *lvalue = (int)vd->num;
	else throw std::runtime_error(std::string(name) + std::string(" must be assigned."));
}

void vt_get_uint(var_table* vt, const std::string& name, size_t* lvalue) {
    if (var_data* vd = vt->lookup(name)) *lvalue = (size_t)vd->num;
    else throw std::runtime_error(std::string(name) + std::string(" must be assigned."));
}

void vt_get_bool(var_table* vt, const std::string& name, bool* lvalue) {
    if (var_data* vd = vt->lookup(name)) *lvalue = (bool)vd->num;
    else throw std::runtime_error(std::string(name) + std::string(" must be assigned."));
}

void vt_get_number(var_table* vt, const std::string& name, double* lvalue) {
	if (var_data* vd = vt->lookup(name)) *lvalue = vd->num;
	else throw std::runtime_error(std::string(name) + std::string(" must be assigned."));
}

void vt_get_array_vec(var_table* vt, const std::string& name, std::vector<double>& vec_double) {
	if (var_data* vd = vt->lookup(name)){
	    if (vd->type != SSC_ARRAY)
            throw std::runtime_error(std::string(name) + std::string(" must be array type."));
	    vec_double = vd->arr_vector();
	}
	else throw std::runtime_error(std::string(name) + std::string(" must be assigned."));
}

void vt_get_array_vec(var_table* vt, const std::string& name, std::vector<int>& vec_int) {
    if (var_data* vd = vt->lookup(name)){
        if (vd->type != SSC_ARRAY)
            throw std::runtime_error(std::string(name) + std::string(" must be array type."));
        vec_int.clear();
        for (auto &i : vd->arr_vector()) {
            vec_int.push_back((int)i);
        }
    }
    else throw std::runtime_error(std::string(name) + std::string(" must be assigned."));
}

void vt_get_matrix(var_table* vt, const std::string& name, util::matrix_t<double>& matrix) {
	if (var_data* vd = vt->lookup(name)){
        if (vd->type == SSC_ARRAY)
        {
            std::vector<double> vec_double = vd->arr_vector();
            matrix.resize(vec_double.size());
            for (size_t i = 0; i < vec_double.size(); i++)
                matrix.at(i) = vec_double[i];
        }
        else if (vd->type != SSC_MATRIX)
            throw std::runtime_error(std::string(name) + std::string(" must be matrix type."));
        matrix = vd->num;
    }
	else throw std::runtime_error(std::string(name) + std::string(" must be assigned."));
}

void vt_get_matrix_vec(var_table* vt, const std::string& name, std::vector<std::vector<double>>& mat) {
    if (var_data* vd = vt->lookup(name))
        mat = vd->matrix_vector();
    else throw std::runtime_error(std::string(name)+std::string(" must be assigned."));
}

int var_table::as_integer( const std::string &name )
{
    var_data* x = lookup(name);
    if (!x) throw general_error(name + " not assigned");
    if (x->type != SSC_NUMBER) throw cast_error("integer", *x, name);
    return static_cast<int>(x->num);
}
size_t var_table::as_unsigned_long(const std::string &name)
{
    var_data*x = lookup(name);
    if (!x) throw general_error(name + " not assigned");
    if (x->type != SSC_NUMBER) throw cast_error("unsigned long", *x, name);
    return static_cast<size_t>(x->num);
}

bool var_table::as_boolean( const std::string &name )
{
    var_data* x = lookup(name);
    if (!x) throw general_error(name + " not assigned");
    if (x->type != SSC_NUMBER) throw cast_error("boolean", *x, name);
    return static_cast<bool> ( (int)(x->num!=0) );
}

float var_table::as_float( const std::string &name )
{
    var_data* x = lookup(name);
    if (!x) throw general_error(name + " not assigned");
    if (x->type != SSC_NUMBER) throw cast_error("float", *x, name);
    return static_cast<float>(x->num);
}

ssc_number_t var_table::as_number( const std::string &name )
{
    var_data* x = lookup(name);
    if (!x) throw general_error(name + " not assigned");
    if (x->type != SSC_NUMBER) throw cast_error("ssc_number_t", *x, name);
    return x->num;
}

double var_table::as_double( const std::string &name )
{
    var_data* x = lookup(name);
    if (!x) throw general_error(name + " not assigned");
    if (x->type != SSC_NUMBER) throw cast_error("double", *x, name);
    return static_cast<double>(x->num);
}

const char *var_table::as_string( const std::string &name )
{
    var_data* x = lookup(name);
    if (!x) throw general_error(name + " not assigned");
    if (x->type != SSC_STRING) throw cast_error("string", *x, name);
    return x->str.c_str();
}

ssc_number_t *var_table::as_array( const std::string &name, size_t *count )
{
    var_data* x = lookup(name);
    if (!x) throw general_error(name + " not assigned");
    if (x->type != SSC_ARRAY) throw cast_error("array", *x, name);
    if (count) *count = x->num.length();
    return x->num.data();
}

std::vector<int> var_table::as_vector_integer(const std::string &name)
{
    var_data* x = lookup(name);
    if (!x) throw general_error(name + " not assigned");
    if (x->type != SSC_ARRAY) throw cast_error("array", *x, name);
    size_t len = x->num.length();
    std::vector<int> v(len);
    ssc_number_t *p = x->num.data();
    for (size_t k = 0; k<len; k++)
        v[k] = static_cast<int>(p[k]);
    return v;
}

std::vector<ssc_number_t> var_table::as_vector_ssc_number_t(const std::string &name)
{
    var_data* x = lookup(name);
    if (!x) throw general_error(name + " not assigned");
    if (x->type != SSC_ARRAY) throw cast_error("array", *x, name);
    size_t len = x->num.length();
    std::vector<ssc_number_t> v(len);
    ssc_number_t *p = x->num.data();
    for (size_t k = 0; k<len; k++)
        v[k] = static_cast<ssc_number_t>(p[k]);
    return v;
}

std::vector<double> var_table::as_vector_double(const std::string &name)
{
    var_data* x = lookup(name);
    if (!x) throw general_error(name + " not assigned");
    if (x->type != SSC_ARRAY) throw cast_error("array", *x, name);
    size_t len = x->num.length();
    std::vector<double> v(len);
    ssc_number_t *p = x->num.data();
    for (size_t k=0;k<len;k++)
        v[k] = static_cast<double>(p[k]);
    return v;
}
std::vector<float> var_table::as_vector_float(const std::string &name)
{
    var_data* x = lookup(name);
    if (!x) throw general_error(name + " not assigned");
    if (x->type != SSC_ARRAY) throw cast_error("array", *x, name);
    size_t len = x->num.length();
    std::vector<float> v(len);
    ssc_number_t *p = x->num.data();
    for (size_t k = 0; k<len; k++)
        v[k] = static_cast<float>(p[k]);
    return v;
}
std::vector<size_t> var_table::as_vector_unsigned_long(const std::string &name)
{
    var_data* x = lookup(name);
    if (!x) throw general_error(name + " not assigned");
    if (x->type != SSC_ARRAY) throw cast_error("array", *x, name);
    size_t len = x->num.length();
    std::vector<size_t> v(len);
    ssc_number_t *p = x->num.data();
    for (size_t k = 0; k<len; k++)
        v[k] = static_cast<size_t>(p[k]);
    return v;
}
std::vector<bool> var_table::as_vector_bool(const std::string &name)
{
    var_data* x = lookup(name);
    if (!x) throw general_error(name + " not assigned");
    if (x->type != SSC_ARRAY) throw cast_error("array", *x, name);
    size_t len = x->num.length();
    std::vector<bool> v(len);
    ssc_number_t *p = x->num.data();
    for (size_t k = 0; k<len; k++)
        v[k] = p[k] != 0;
    return v;
}

ssc_number_t *var_table::as_matrix( const std::string &name, size_t *rows, size_t *cols )
{
    var_data* x = lookup(name);
    if (!x) throw general_error(name + " not assigned");
    if (x->type != SSC_MATRIX) throw cast_error("matrix", *x, name);
    if (rows) *rows = x->num.nrows();
    if (cols) *cols = x->num.ncols();
    return x->num.data();
}

util::matrix_t<double> var_table::as_matrix(const std::string &name)
{
    var_data* x = lookup(name);
    if (!x) throw general_error(name + " not assigned");
    if (x->type != SSC_MATRIX) throw cast_error("matrix", *x, name);

    util::matrix_t<double> mat(x->num.nrows(), x->num.ncols(), 0.0);
    for (size_t r = 0; r<x->num.nrows(); r++)
        for (size_t c = 0; c<x->num.ncols(); c++)
            mat.at(r, c) = static_cast<double>(x->num(r, c));

    return mat;
}

util::matrix_t<size_t> var_table::as_matrix_unsigned_long(const std::string &name)
{
    var_data* x = lookup(name);
    if (!x) throw general_error(name + " not assigned");
    if (x->type != SSC_MATRIX) throw cast_error("matrix", *x, name);

    util::matrix_t<size_t> mat(x->num.nrows(), x->num.ncols(), (size_t)0.0);
    for (size_t r = 0; r<x->num.nrows(); r++)
        for (size_t c = 0; c<x->num.ncols(); c++)
            mat.at(r, c) = static_cast<size_t>(x->num(r, c));

    return mat;
}


util::matrix_t<double> var_table::as_matrix_transpose(const std::string &name)
{
    var_data* x = lookup(name);
    if (!x) throw general_error(name + " not assigned");
    if (x->type != SSC_MATRIX) throw cast_error("matrix", *x, name);

    util::matrix_t<double> mat(x->num.ncols(), x->num.nrows(), 0.0);
    for (size_t r = 0; r<x->num.nrows(); r++)
        for (size_t c = 0; c<x->num.ncols(); c++)
            mat.at(c, r) = static_cast<double>(x->num(r, c));

    return mat;
}

bool var_table::get_matrix(const std::string &name, util::matrix_t<ssc_number_t> &mat)
{
    var_data* x = lookup(name);
    if (!x) throw general_error(name + " not assigned");
    if (x->type != SSC_MATRIX) throw cast_error("matrix", *x, name);

    size_t nrows, ncols;
    ssc_number_t *arr = as_matrix(name, &nrows, &ncols);

    if (nrows < 1 || ncols < 1)
        return false;

    mat.resize_fill(nrows, ncols, 1.0);
    for (size_t r = 0; r<nrows; r++)
        for (size_t c = 0; c<ncols; c++)
            mat.at(r, c) = arr[r*ncols + c];

    return true;
}

ssc_number_t *var_table::allocate( const std::string &name, size_t length )
{
    var_data *v = assign(name, var_data());
    v->type = SSC_ARRAY;
    v->num.resize_fill( length, 0.0 );
    return v->num.data();
}

ssc_number_t *var_table::allocate( const std::string &name, size_t nrows, size_t ncols )
{
    var_data *v = assign(name, var_data());
    v->type = SSC_MATRIX;
    v->num.resize_fill(nrows, ncols, 0.0);
    return v->num.data();
}

util::matrix_t<ssc_number_t>& var_table::allocate_matrix( const std::string &name, size_t nrows, size_t ncols )
{
    var_data *v = assign(name, var_data());
    v->type = SSC_MATRIX;
    v->num.resize_fill(nrows, ncols, 0.0);
    return v->num;
}
