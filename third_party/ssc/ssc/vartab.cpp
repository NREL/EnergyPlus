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
		assign( (*it).first, *((*it).second) );

	return *this;
}

void var_table::clear()
{
	for (var_hash::iterator it = m_hash.begin(); it != m_hash.end(); ++it)
	{
		// debug heap corruption

		delete it->second; // delete the var_data object
	}
	m_hash.clear();
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
	
	var_hash::iterator it = m_hash.find( util::lower_case(oldname) );
	if ( it != m_hash.end() )
	{
		std::string lcnewname( util::lower_case(newname) );

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
	var_hash::iterator it = m_hash.find( util::lower_case(name) );
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

const char *var_table::next()
{
	if (m_iterator == m_hash.end()) return NULL;

	++m_iterator;

	if (m_iterator != m_hash.end())	return m_iterator->first.c_str();

	return NULL;
}

