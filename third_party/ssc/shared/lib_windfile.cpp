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

#include <stdio.h>
#include <cmath>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <ctype.h>
#include <limits>
#include <numeric>
#include <sstream>
#include <typeinfo>
#include <stdio.h>

#if defined(__WINDOWS__)||defined(WIN32)||defined(_WIN32)
#define CASECMP(a,b) _stricmp(a,b)
#define CASENCMP(a,b,n) _strnicmp(a,b,n)
#else
#define CASECMP(a,b) strcasecmp(a,b)
#define CASENCMP(a,b,n) strncasecmp(a,b,n)
#endif

#include "lib_util.h"
#include "lib_windfile.h"

#ifdef _MSC_VER
#define my_isnan(x) ::_isnan( x )
#else
#define my_isnan(x) std::isnan( x )
#endif

static void trim(std::string &buf)
{
	if (buf.back() == '\n') // strip newline
	  	buf.pop_back();
	if (buf.back() == '\r') // strip carriage return
	  	buf.pop_back();
}

static int locate2(std::string buf, std::vector<std::string> &vstring, char delim)
{
	trim(buf);
	std::stringstream ss(buf);
	std::string token;

	vstring.clear();
	while (getline(ss, token, delim)) {
	  vstring.push_back(token);
	}
	return (int)vstring.size();
}

winddata_provider::winddata_provider()
{
	year = 1900;
	lat = lon = elev = 0;
	measurementHeight = 0;
	m_errorMsg.clear();
}
winddata_provider::~winddata_provider()
{
	// nothing to do
}

bool winddata_provider::find_closest( int& closest_index, int id, int ncols, double requested_height, int index_to_exclude /* = -1 */ )
{
	closest_index = -1;
	double height_diff = 1e99;
	for ( size_t i=0;i<m_dataid.size();i++ )
	{
		if ( (m_dataid[i] == id) && ((int)i != index_to_exclude) )
		{
			if ( fabs(m_heights[i] - requested_height) < height_diff )
			{
				if ( index_to_exclude>=0 ) // we're looking for the next closest column for interpolation
				{	// the next closest measurement height can't be on the same side of requested_height as index_to_exclude
					if ( (m_heights[i] > requested_height) && (m_heights[index_to_exclude] > requested_height) ) continue;
					if ( (m_heights[i] < requested_height) && (m_heights[index_to_exclude] < requested_height) ) continue;
				}
				closest_index = (int)i;
				height_diff = fabs(m_heights[i] - requested_height);
			}
		}
	}

	return (closest_index >= 0 && closest_index < ncols);
}

bool winddata_provider::can_interpolate( int index1, int index2, int ncols, double requested_height )
{
	if ( index1<0 || index2<0 ) return false;
	if ( index1>=ncols || index2>=ncols ) return false;
	if ( m_heights[index1]<requested_height && requested_height<m_heights[index2] ) return true; // height 1 < height 2
	if ( m_heights[index1]>requested_height && requested_height>m_heights[index2] ) return true; // height 1 > height 2

	return false;
}

bool winddata_provider::read( double requested_height,
	double *speed,
	double *direction,
	double *temperature,
	double *pressure,
	double *closest_speed_meas_height_in_file,
	double *closest_dir_meas_height_in_file,
	bool bInterpolate /*= false*/)
{	
	std::vector<double> values;
	if ( !read_line( values ) )
		return false;
	
	if (values.size() < m_heights.size() || values.size() < m_dataid.size())
		return false;

	int ncols = (int)values.size();

	*speed = *direction = *temperature = *pressure = *closest_speed_meas_height_in_file = *closest_dir_meas_height_in_file = std::numeric_limits<double>::quiet_NaN();

	int index = -1, index2 = -1;
	if ( find_closest(index, SPEED, ncols, requested_height) )
	{
		if ( (bInterpolate) && (m_heights[index] != requested_height) && find_closest(index2, SPEED, ncols, requested_height, index) && can_interpolate(index, index2, ncols, requested_height)  )
		{
			*speed = util::interpolate(m_heights[index], values[index], m_heights[index2], values[index2], requested_height);
			*closest_speed_meas_height_in_file = requested_height;
		}
		else
		{
			*speed = values[index];
			*closest_speed_meas_height_in_file = m_heights[index];
		}
	}

	if (find_closest(index, DIR, ncols, requested_height) )
	{
		// interpolating direction is a little more complicated
		double dir1=0, dir2=0, angle;
		double ht1=0, ht2=0;
		bool interp_direction = ( (bInterpolate) && (m_heights[index] != requested_height) && find_closest(index2, DIR, ncols, requested_height, index) && can_interpolate(index, index2, ncols, requested_height)  );
		if ( interp_direction )
		{
			dir1 = values[index];
			dir2 = values[index2];
			if (my_isnan(dir1) || my_isnan(dir2))
				return false;
			while (dir1 < 0) dir1 += 360; //add 360 to negative values until it is positive
			while (dir1 >= 360) dir1 -= 360; //360 is set to zero, anything above 360 has 360 subtracted until it's below 360
			//dir1 = (values[index]<360) ? values[index] : 0; // set any 360 deg values to zero //error checking added 11/28/16 jmf
			while (dir2 < 0) dir2 += 360;
			while (dir2 >= 360) dir2 -= 360; //same error checking as above, added 11/28/16 jmf
			//dir2 = (values[index2]<360) ? values[index2] : 0;
			ht1 = m_heights[index];
			ht2 = m_heights[index2];
			if (dir1>dir2)
			{	// swap
				double temp = dir2;
				dir2=dir1;
				dir1 = temp;
				temp = ht2;
				ht2 = ht1;
				ht1 = temp;
			}
			angle = ( (dir2-dir1) < 180 ) ? (dir2-dir1) : 360.0 - (dir2-dir1);
			interp_direction &= (angle <= 180 ); // not sure if it makes sense to 'interpolate' between directions that are 180 deg apart?
		}
		
		if (interp_direction)
		{
			// special case when interpolating across straight north (0 degrees)
			if (dir1<90 && dir2>270) 
			{
				*direction = util::interpolate(ht1, dir1+90.0, ht2, dir2-270.0, requested_height)-90.0;
				if (*direction<0) *direction += 360.0;
			}
			else
				*direction = util::interpolate(ht1, dir1, ht2, dir2, requested_height);

			*closest_dir_meas_height_in_file = requested_height;
		}
		else
		{
			*direction = values[index];
			*closest_dir_meas_height_in_file = m_heights[index];
		}
	}

	if ( find_closest(index, TEMP, ncols, requested_height) )
	{
		if ( (bInterpolate) && (m_heights[index] != requested_height) && find_closest(index2, TEMP, ncols, requested_height, index) && can_interpolate(index, index2, ncols, requested_height)  )
			*temperature = util::interpolate(m_heights[index], values[index], m_heights[index2], values[index2], requested_height);
		else
			*temperature = values[index];
	}

	if ( find_closest(index, PRES, ncols, requested_height) )
	{
		if ( (bInterpolate) && (m_heights[index] != requested_height) && find_closest(index2, PRES, ncols, requested_height, index) && can_interpolate(index, index2, ncols, requested_height)  )
			*pressure = util::interpolate(m_heights[index], values[index], m_heights[index2], values[index2], requested_height);
		else
			*pressure = values[index];
	}

	bool found_all 
		= !my_isnan( *speed )
		&& !my_isnan( *direction )
		&& !my_isnan( *temperature )
		&& !my_isnan( *pressure );

	//add error checking. direction error checking performed in the averaging function.
	if (*speed < 0 || *speed > 120) //units are m/s, wind speed cannot be negative and highest recorded wind speed ever was 113 m/s (https://en.wikipedia.org/wiki/Wind_speed)
	{
		found_all = false;
		m_errorMsg = util::format("Error: wind speed of %g m/s found in weather file, this speed is outside the possible range of 0 to 120 m/s", *speed);
	}
	if (*temperature < -200 || *temperature > 100) //units are Celsius
	{
		found_all = false;
		m_errorMsg = util::format("Error: temperature of %g degrees Celsius found in weather file, this temperature is outside the possible range of -200 to 100 degrees C", *pressure);
	}
	if (*pressure < 0.5 || *pressure > 1.1) //units are atm, highest recorded pressure was 1085.7 Hectopascals (1.07 atm)  (https://en.wikipedia.org/wiki/Atmospheric_pressure#Records)
	{
		found_all = false;
		m_errorMsg = util::format("Error: atmospheric pressure of %g atm found in weather file, this pressure is outside the possible range of 0.5 to 1.1 atm", *pressure);
	}

	return found_all;

}




windfile::windfile()
	: winddata_provider()
{
	m_nrec = 0;
	close();
}

windfile::windfile( const std::string &file )
	: winddata_provider()
{
	m_nrec = 0;
	close();
	open( file );
}

windfile::~windfile()
{
  	m_ifs.close();
}

bool windfile::ok()
{
  	return m_ifs.good();
}


std::string windfile::filename()
{
	return m_file;
}

bool windfile::open( const std::string &file )
{
	close();
	if (file.empty()) return false;
	
	/*  // don't be strict about requiring .srw extensions for now
	if ( !cmp_ext(file.c_str(), "srw") )
		return false;
		*/

	m_ifs.open(file);
	if (!m_ifs.good())
	{
		m_errorMsg = "could not open file for reading: " + file;
		return false;
	}
		
	/* read header information */
	
	// read line 1 (header info)
	getline(m_ifs, m_buf);
	std::vector<std::string> cols;
	int ncols = locate2(m_buf, cols, ',');

	if (ncols < 8)
	{
		m_errorMsg = util::format("error reading header (line 1).  At least 8 columns required, %d found.", ncols);
		m_ifs.close();
		return false;
	}

	locid = cols[0];
	city = cols[1];
	state = cols[2];
	country = cols[3];

	try { year = std::stoi(cols[4]); } 
	catch (const std::invalid_argument &) {/* nothing to do */};
	try { lat = std::stoi(cols[5]); }
	catch (const std::invalid_argument &) {/* nothing to do */ };
	try { lon = std::stoi(cols[6]); }
	catch (const std::invalid_argument &) {/* nothing to do */ };
	try { elev = std::stoi(cols[7]); }
	catch (const std::invalid_argument &) {/* nothing to do */ };

	// read line 2, description
	getline(m_ifs, desc);
	trim(desc);
	
	// read line 3, column names (must be pressure, temperature, speed, direction)
	getline(m_ifs, m_buf);
	ncols = locate2( m_buf, cols, ',' );
	if (ncols < 3)
	{
		m_errorMsg = util::format("too few data column types found: %d.  at least 3 required.", ncols);
		m_ifs.close();
		return false;
	}
	
	for (int i=0;i<ncols;i++)
	{
		std::string ctype = util::lower_case( cols[i] );
		if ( ctype == "temperature" || ctype == "temp" )
			m_dataid.push_back(TEMP);
		else if ( ctype == "pressure" || ctype == "pres" )
			m_dataid.push_back(PRES);
		else if ( ctype == "speed" || ctype == "velocity" )
			m_dataid.push_back(SPEED);
		else if ( ctype == "direction" || ctype == "dir" )
			m_dataid.push_back(DIR);
		else if ( ctype.length() > 0 )
		{
			m_errorMsg = util::format( "error reading data column type specifier in col %d of %d: '%s' len: %d", i+1, ncols, ctype.c_str(), ctype.length() );
			m_ifs.close();
			return false;
		}
	}

	m_heights.resize( m_dataid.size(), -1  );


	// read line 4, units for each column (ignore this for now)
	getline(m_ifs, m_buf);

	// read line 5, height in meters for each data column
	getline(m_ifs, m_buf);
	ncols = locate2( m_buf, cols, ',' );
	if ( ncols < (int)m_heights.size() )
	{
		m_errorMsg = util::format("too few columns in the height row.  %d required but only %d found", (int)m_heights.size(), ncols);
		m_ifs.close();
		return false;
	}

	for (size_t i=0;i<m_heights.size();i++)
		m_heights[i] = stof( cols[i] );
	

	// read all the lines to determine the nubmer of records in the file
	m_nrec = 0;
	while (getline(m_ifs, m_buf))
		m_nrec++;

	// rewind the file and reposition right after the header information
	m_ifs.clear();
	m_ifs.seekg(0);
	for (size_t i = 0; i < 5; i++)
		getline(m_ifs, m_buf);

	
	// ready to read line-by-line.  subsequent columns of data correspond to the
	// data types in m_dataid and measurement heights in m_heights
	m_file = file;
	return true;
}

void windfile::close()
{
  	m_ifs.close();

	m_file.clear();
	city.clear();
	state.clear();
	locid.clear();
	country.clear();
	desc.clear();
	year = 1900;
	lat = lon = elev = 0.0;
	m_nrec = 0;
}

size_t windfile::nrecords()
{
	return m_nrec;
}

bool windfile::read_line( std::vector<double> &values )
{
	if ( !ok() ) return false;

	std::vector<std::string> cols;
	getline(m_ifs, m_buf);
	int ncols = locate2(m_buf, cols, ',');
	if (ncols >= (int)m_heights.size() 
		&& ncols >= (int)m_dataid.size())
	{
		values.resize( m_heights.size(), 0.0 );
		for (size_t i=0;i<m_heights.size();i++)
			values[i] = stof( cols[i] );

		return true;
	}
	else
		return false;
}
