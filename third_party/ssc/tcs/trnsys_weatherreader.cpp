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

#define _TCSTYPEINTERFACE_
#include "tcstype.h"
#include <vector>
#include <algorithm>
#include <sstream>
#include <fstream>
#include <cmath>
#include <stdlib.h>

#ifndef M_PI
#define M_PI 3.14159265358979323
#endif




enum {	I_FILENAME, 

		I_YEAR,
		I_MONTH,
		I_DAY,
		I_HOUR,
		I_MINUTE,

		I_GLOBAL, 
		I_BEAM, 
		I_DIFFUSE,
		I_TDRY,
		I_TWET,
		I_TDEW,
		I_WSPD,
		I_WDIR,
		I_RHUM,
		I_PRES,
		I_SNOW,
		I_ALBEDO,

		I_POA,

		I_SOLAZI,
		I_SOLZEN,
		I_LAT,
		I_LON,
		I_TZ,
		I_SHIFT,

		O_YEAR,
		O_MONTH,
		O_DAY,
		O_HOUR,
		O_MINUTE,

		O_GLOBAL, 
		O_BEAM, 
		O_DIFFUSE,
		O_TDRY,
		O_TWET,
		O_TDEW,
		O_WSPD,
		O_WDIR,
		O_RHUM,
		O_PRES,
		O_SNOW,
		O_ALBEDO,

		O_POA,

		O_SOLAZI,
		O_SOLZEN,
		O_LAT,
		O_LON,
		O_SHIFT,
		O_TZ,		

		// debug
//		D_NDX_HOUR,
//		D_NDX_SOLAZI,

		N_MAX };

tcsvarinfo trnsys_weatherreader_variables[] = {
	/* DIRECTION    DATATYPE      INDEX       NAME           LABEL                                  UNITS      GROUP    META    DEFAULTVALUE */
	{ TCS_INPUT,   TCS_STRING,   I_FILENAME, "file_name",   "TRNSYS hourly output with weather data on local computer",  "",        "",      "",     "" },

	{ TCS_INPUT,  TCS_STRING,   I_YEAR,     "i_year",        "Year column from TRNSYS input file",                                 "yr",      "Time",  "",     "" },
	{ TCS_INPUT,  TCS_STRING,   I_MONTH,    "i_month",       "Month column from TRNSYS input file",                                "mn",      "Time",  "",     "" },
	{ TCS_INPUT,  TCS_STRING,   I_DAY,      "i_day",         "Day column from TRNSYS input file",                                  "dy",      "Time",  "",     "" },
	{ TCS_INPUT,  TCS_STRING,   I_HOUR,     "i_hour",        "Hour column from TRNSYS input file",                                 "hr",      "Time",  "",     "" },
	{ TCS_INPUT,  TCS_STRING,   I_MINUTE,   "i_minute",      "Minute column from TRNSYS input file",                               "mi",      "Time",  "",     "" },

	{ TCS_INPUT,  TCS_STRING,   I_GLOBAL,   "i_global",      "Global horizontal irradiance column from TRNSYS input file",         "W/m2",    "Solar", "",     "" },
	{ TCS_INPUT,  TCS_STRING,   I_BEAM,     "i_beam",        "Beam normal irradiance column from TRNSYS input file",               "W/m2",    "Solar", "",     "" },
	{ TCS_INPUT,  TCS_STRING,   I_DIFFUSE,  "i_diff",        "Diffuse horizontal irradiance column from TRNSYS input file",        "W/m2",    "Solar", "",     "" },
	{ TCS_INPUT,  TCS_STRING,   I_TDRY,     "i_tdry",        "Dry bulb temperature column from TRNSYS input file",                 "'C",      "Meteo", "",     "" },
	{ TCS_INPUT,  TCS_STRING,   I_TWET,     "i_twet",        "Wet bulb temperature column from TRNSYS input file",                 "'C",      "Meteo", "",     "" },
	{ TCS_INPUT,  TCS_STRING,   I_TDEW,     "i_tdew",        "Dew point temperature column from TRNSYS input file",                "'C",      "Meteo", "",     "" },
	{ TCS_INPUT,  TCS_STRING,   I_WSPD,     "i_wspd",        "Wind speed column from TRNSYS input file",                           "m/s",     "Meteo", "",     "" },
	{ TCS_INPUT,  TCS_STRING,   I_WDIR,     "i_wdir",        "Wind direction column from TRNSYS input file",                       "deg",     "Meteo", "",     "" },
	{ TCS_INPUT,  TCS_STRING,   I_RHUM,     "i_rhum",        "Relative humidity column from TRNSYS input file",                    "%",       "Meteo", "",     "" },
	{ TCS_INPUT,  TCS_STRING,   I_PRES,     "i_pres",        "Pressure column from TRNSYS input file",                             "mbar",    "Meteo", "",     "" },
	{ TCS_INPUT,  TCS_STRING,   I_SNOW,     "i_snow",        "Snow cover column from TRNSYS input file",                           "cm",      "Meteo", "valid (0,150)",   "" },
	{ TCS_INPUT,  TCS_STRING,   I_ALBEDO,   "i_albedo",      "Ground albedo column from TRNSYS input file",                        "0..1",    "Meteo", "valid (0,1)",     "" },
	
	{ TCS_INPUT,  TCS_STRING,   I_POA,      "i_poa",         "Plane-of-array total incident irradiance column from TRNSYS input file", "W/m2",    "Irrad", "",     "" },
	{ TCS_INPUT,  TCS_STRING,   I_SOLAZI,	"i_solazi",       "Solar Azimuth column from TRNSYS input file", "deg",    "", "",     "" },
	{ TCS_INPUT,  TCS_STRING,   I_SOLZEN,	"i_solzen",       "Solar Zenith column from TRNSYS input file", "deg",    "", "",     "" },
	{ TCS_INPUT,  TCS_STRING,   I_LAT,		"i_lat",       "Latitude column from TRNSYS input file", "DDD",    "", "",     "" },
	{ TCS_INPUT,  TCS_STRING,   I_LON,		"i_lon",       "Longitude column from TRNSYS input file", "DDD",    "", "",     "" },
	{ TCS_INPUT,  TCS_STRING,   I_TZ,		"i_tz",       "Timezone column from TRNSYS input file", "DDD",    "", "",     "" },
	{ TCS_INPUT,  TCS_NUMBER,   I_SHIFT,    "i_shift",       "shift in longitude from local standard meridian from TRNSYS input file", "deg", "Solar", "", "" },


	{ TCS_OUTPUT,  TCS_NUMBER,   O_YEAR,     "year",        "Year",                                 "yr",      "Time",  "",     "" },
	{ TCS_OUTPUT,  TCS_NUMBER,   O_MONTH,    "month",       "Month",                                "mn",      "Time",  "",     "" },
	{ TCS_OUTPUT,  TCS_NUMBER,   O_DAY,      "day",         "Day",                                  "dy",      "Time",  "",     "" },
	{ TCS_OUTPUT,  TCS_NUMBER,   O_HOUR,     "hour",        "Hour",                                 "hr",      "Time",  "",     "" },
	{ TCS_OUTPUT,  TCS_NUMBER,   O_MINUTE,   "minute",      "Minute",                               "mi",      "Time",  "",     "" },

	{ TCS_OUTPUT,  TCS_NUMBER,   O_GLOBAL,   "global",      "Global horizontal irradiance",         "W/m2",    "Solar", "",     "" },
	{ TCS_OUTPUT,  TCS_NUMBER,   O_BEAM,     "beam",        "Beam normal irradiance",               "W/m2",    "Solar", "",     "" },
	{ TCS_OUTPUT,  TCS_NUMBER,   O_DIFFUSE,  "diff",        "Diffuse horizontal irradiance",        "W/m2",    "Solar", "",     "" },
	{ TCS_OUTPUT,  TCS_NUMBER,   O_TDRY,     "tdry",        "Dry bulb temperature",                 "'C",      "Meteo", "",     "" },
	{ TCS_OUTPUT,  TCS_NUMBER,   O_TWET,     "twet",        "Wet bulb temperature",                 "'C",      "Meteo", "",     "" },
	{ TCS_OUTPUT,  TCS_NUMBER,   O_TDEW,     "tdew",        "Dew point temperature",                "'C",      "Meteo", "",     "" },
	{ TCS_OUTPUT,  TCS_NUMBER,   O_WSPD,     "wspd",        "Wind speed",                           "m/s",     "Meteo", "",     "" },
	{ TCS_OUTPUT,  TCS_NUMBER,   O_WDIR,     "wdir",        "Wind direction",                       "deg",     "Meteo", "",     "" },
	{ TCS_OUTPUT,  TCS_NUMBER,   O_RHUM,     "rhum",        "Relative humidity",                    "%",       "Meteo", "",     "" },
	{ TCS_OUTPUT,  TCS_NUMBER,   O_PRES,     "pres",        "Pressure",                             "mbar",    "Meteo", "",     "" },
	{ TCS_OUTPUT,  TCS_NUMBER,   O_SNOW,     "snow",        "Snow cover",                           "cm",      "Meteo", "valid (0,150)",   "" },
	{ TCS_OUTPUT,  TCS_NUMBER,   O_ALBEDO,   "albedo",      "Ground albedo",                        "0..1",    "Meteo", "valid (0,1)",     "" },
	
	{ TCS_OUTPUT,  TCS_NUMBER,   O_POA,      "poa",         "Plane-of-array total incident irradiance", "W/m2",    "Irrad", "",     "" },
	{ TCS_OUTPUT,  TCS_NUMBER,   O_SOLAZI,   "solazi",      "Solar Azimuth", "deg",    "", "",     "" },
	{ TCS_OUTPUT,  TCS_NUMBER,   O_SOLZEN,   "solzen",      "Solar Zenith", "deg",    "", "",     "" },
	{ TCS_OUTPUT,  TCS_NUMBER,   O_LAT,      "lat",         "Latitude", "DDD",    "", "",     "" },
	{ TCS_OUTPUT,  TCS_NUMBER,   O_LON,      "lon",         "Longitude", "DDD",    "", "",     "" },
	{ TCS_OUTPUT,  TCS_NUMBER,   O_SHIFT,    "shift",       "shift in longitude from local standard meridian", "deg", "Solar", "", "" },
	{ TCS_OUTPUT,  TCS_NUMBER,   O_TZ,       "tz",          "Timezone", "DDD",    "", "",     "" },
	

	// testing - check indices assigned
//	{ TCS_DEBUG,  TCS_NUMBER,   D_NDX_HOUR,  "d_hour",       "", "",    "", "",     "" },
//	{ TCS_DEBUG,  TCS_NUMBER,   D_NDX_SOLAZI,  "d_solazi",       "", "",    "", "",     "" },

	{ TCS_INVALID, TCS_INVALID,  N_MAX,       0,            0, 0, 0, 0, 0 }
};

class trnsys_weatherreader : public tcstypeinterface
{
private:
	std::ifstream m_trnsys_file;
	std::vector<std::string> m_trnsys_values;
	int m_ndx_year;
	int m_ndx_month;
	int m_ndx_day;     
	int m_ndx_hour;
	int m_ndx_minute;  
	int m_ndx_global;    
	int m_ndx_beam;      
	int m_ndx_diff;      
	int m_ndx_tdry;      
	int m_ndx_twet;      
	int m_ndx_tdew;      
	int m_ndx_wspd;      
	int m_ndx_wdir;      
	int m_ndx_rhum;      
	int m_ndx_pres;      
	int m_ndx_snow;      
	int m_ndx_albedo;    
	int m_ndx_poa;     
	int m_ndx_solazi;  
	int m_ndx_solzen;  
	int m_ndx_lat;     
	int m_ndx_lon;     
	int m_ndx_tz;  
	int m_ndx_shift;

public:
	trnsys_weatherreader( tcscontext *cxt, tcstypeinfo *ti )
		: tcstypeinterface( cxt, ti ) { }

	virtual ~trnsys_weatherreader() { }

	virtual int init()
	{
		std::vector<std::string> trnsys_columns;
		std::string file = value_str( I_FILENAME ).c_str();
		m_trnsys_file.open( file.c_str(), std::ios::in );
		std::string file_line;
		if ( std::getline( m_trnsys_file, file_line, '\n'))
		{
			std::stringstream ss( file_line );
			std::string col_name;
			while ( std::getline( ss, col_name, '\t' ))
			{
				// remove whitespace
				col_name.erase(std::remove_if(col_name.begin(), col_name.end(), ::isspace), col_name.end());
				trnsys_columns.push_back( col_name );
			}
		}
		// skip units - can do conversion 
		std::getline( m_trnsys_file, file_line, '\n');

		// set column index for setting variables
		std::vector<std::string>::iterator it;

		if ( (it = std::find( trnsys_columns.begin(), trnsys_columns.end(), value_str(I_YEAR).c_str())) != trnsys_columns.end())
			m_ndx_year = (int)std::distance( trnsys_columns.begin(), it );
		else
			m_ndx_year = -1;

		if ( (it = std::find( trnsys_columns.begin(), trnsys_columns.end(), value_str(I_MONTH).c_str())) != trnsys_columns.end())
			m_ndx_month = (int)std::distance( trnsys_columns.begin(), it );
		else
			m_ndx_month = -1;

		if ( (it = std::find( trnsys_columns.begin(), trnsys_columns.end(), value_str(I_DAY).c_str())) != trnsys_columns.end())
			m_ndx_day = (int)std::distance( trnsys_columns.begin(), it );
		else
			m_ndx_day = -1;

		if ( (it = std::find( trnsys_columns.begin(), trnsys_columns.end(), value_str(I_HOUR).c_str())) != trnsys_columns.end())
			m_ndx_hour = (int)std::distance( trnsys_columns.begin(), it );
		else
			m_ndx_hour = -1;

		if ( (it = std::find( trnsys_columns.begin(), trnsys_columns.end(), value_str(I_MINUTE).c_str())) != trnsys_columns.end())
			m_ndx_minute = (int)std::distance( trnsys_columns.begin(), it );
		else
			m_ndx_minute = -1;

		if ( (it = std::find( trnsys_columns.begin(), trnsys_columns.end(), value_str(I_GLOBAL).c_str())) != trnsys_columns.end())
			m_ndx_global = (int)std::distance( trnsys_columns.begin(), it );
		else
			m_ndx_global = -1;

		if ( (it = std::find( trnsys_columns.begin(), trnsys_columns.end(), value_str(I_BEAM).c_str())) != trnsys_columns.end())
			m_ndx_beam = (int)std::distance( trnsys_columns.begin(), it );
		else
			m_ndx_beam = -1;

		if ( (it = std::find( trnsys_columns.begin(), trnsys_columns.end(), value_str(I_DIFFUSE).c_str())) != trnsys_columns.end())
			m_ndx_diff = (int)std::distance( trnsys_columns.begin(), it );
		else
			m_ndx_diff = -1;

		if ( (it = std::find( trnsys_columns.begin(), trnsys_columns.end(), value_str(I_TDRY).c_str())) != trnsys_columns.end())
			m_ndx_tdry = (int)std::distance( trnsys_columns.begin(), it );
		else
			m_ndx_tdry = -1;

		if ( (it = std::find( trnsys_columns.begin(), trnsys_columns.end(), value_str(I_TWET).c_str())) != trnsys_columns.end())
			m_ndx_twet = (int)std::distance( trnsys_columns.begin(), it );
		else
			m_ndx_twet = -1;

		if ( (it = std::find( trnsys_columns.begin(), trnsys_columns.end(), value_str(I_TDEW).c_str())) != trnsys_columns.end())
			m_ndx_tdew = (int)std::distance( trnsys_columns.begin(), it );
		else
			m_ndx_tdew = -1;

		if ( (it = std::find( trnsys_columns.begin(), trnsys_columns.end(), value_str(I_WSPD).c_str())) != trnsys_columns.end())
			m_ndx_wspd = (int)std::distance( trnsys_columns.begin(), it );
		else
			m_ndx_wspd = -1;

		if ( (it = std::find( trnsys_columns.begin(), trnsys_columns.end(), value_str(I_WDIR).c_str())) != trnsys_columns.end())
			m_ndx_wdir = (int)std::distance( trnsys_columns.begin(), it );
		else
			m_ndx_wdir = -1;

		if ( (it = std::find( trnsys_columns.begin(), trnsys_columns.end(), value_str(I_RHUM).c_str())) != trnsys_columns.end())
			m_ndx_rhum = (int)std::distance( trnsys_columns.begin(), it );
		else
			m_ndx_rhum = -1;

		if ( (it = std::find( trnsys_columns.begin(), trnsys_columns.end(), value_str(I_PRES).c_str())) != trnsys_columns.end())
			m_ndx_pres = (int)std::distance( trnsys_columns.begin(), it );
		else
			m_ndx_pres = -1;

		if ( (it = std::find( trnsys_columns.begin(), trnsys_columns.end(), value_str(I_SNOW).c_str())) != trnsys_columns.end())
			m_ndx_snow = (int)std::distance( trnsys_columns.begin(), it );
		else
			m_ndx_snow = -1;

		if ( (it = std::find( trnsys_columns.begin(), trnsys_columns.end(), value_str(I_ALBEDO).c_str())) != trnsys_columns.end())
			m_ndx_albedo = (int)std::distance( trnsys_columns.begin(), it );
		else
			m_ndx_albedo = -1;

		if ( (it = std::find( trnsys_columns.begin(), trnsys_columns.end(), value_str(I_POA).c_str())) != trnsys_columns.end())
			m_ndx_poa = (int)std::distance( trnsys_columns.begin(), it );
		else
			m_ndx_poa = -1;

		if ( (it = std::find( trnsys_columns.begin(), trnsys_columns.end(), value_str(I_SOLAZI).c_str())) != trnsys_columns.end())
			m_ndx_solazi = (int)std::distance( trnsys_columns.begin(), it );
		else
			m_ndx_solazi = -1;

		if ( (it = std::find( trnsys_columns.begin(), trnsys_columns.end(), value_str(I_SOLZEN).c_str())) != trnsys_columns.end())
			m_ndx_solzen = (int)std::distance( trnsys_columns.begin(), it );
		else
			m_ndx_solzen = -1;

		if ( (it = std::find( trnsys_columns.begin(), trnsys_columns.end(), value_str(I_LAT).c_str())) != trnsys_columns.end())
			m_ndx_lat = (int)std::distance( trnsys_columns.begin(), it );
		else
			m_ndx_lat = -1;

		if ( (it = std::find( trnsys_columns.begin(), trnsys_columns.end(), value_str(I_LON).c_str())) != trnsys_columns.end())
			m_ndx_lon = (int)std::distance( trnsys_columns.begin(), it );
		else
			m_ndx_lon = -1;

		if ( (it = std::find( trnsys_columns.begin(), trnsys_columns.end(), value_str(I_TZ).c_str())) != trnsys_columns.end())
			m_ndx_tz = (int)std::distance( trnsys_columns.begin(), it );
		else
			m_ndx_tz = -1;

		if ( (it = std::find( trnsys_columns.begin(), trnsys_columns.end(), value_str(I_SHIFT).c_str())) != trnsys_columns.end())
			m_ndx_shift = (int)std::distance( trnsys_columns.begin(), it );
		else
			m_ndx_shift = -1;

		return 0; // success
	}

	virtual int call( double time, double /*step*/, int ncall )
	{
		if ( ncall == 0 ) // only read data values once per timestep
		{
			m_trnsys_values.clear();
			std::string file_line;
			if ( std::getline( m_trnsys_file, file_line, '\n'))
			{
				std::stringstream ss( file_line );
				std::string col_value;
				while ( std::getline( ss, col_value, '\t' ))
				{
				// remove whitespace
					col_value.erase(std::remove_if(col_value.begin(), col_value.end(), ::isspace), col_value.end());
					m_trnsys_values.push_back( col_value );
				}
			}
			else
			{
				message(TCS_ERROR, "failed to read from weather file %s at time %lg", value_str(I_FILENAME).c_str(), time );
				return -1; // error code
			}
		}
		if (m_ndx_year > -1)
            value( O_YEAR, atof(m_trnsys_values[m_ndx_year].c_str()) );
		else
			value( O_YEAR, 0.0 );

		if (m_ndx_month > -1)
			value( O_MONTH, atof(m_trnsys_values[m_ndx_month].c_str()) );
		else
			value( O_MONTH, 0.0 );

		if (m_ndx_day > -1)
			value( O_DAY, atof(m_trnsys_values[m_ndx_day].c_str()) );
		else
			value( O_DAY, 0.0 );

		if (m_ndx_hour > -1)
			value( O_HOUR, atof(m_trnsys_values[m_ndx_hour].c_str()) );
		else
			value( O_HOUR, 0.0 );

		if (m_ndx_minute > -1)
			value( O_MINUTE, atof(m_trnsys_values[m_ndx_minute].c_str()) );
		else
			value( O_MINUTE, 0.0 );

		if (m_ndx_global > -1)
			value( O_GLOBAL, atof(m_trnsys_values[m_ndx_global].c_str())/3.6 );
		else
			value( O_GLOBAL, 0.0 );

		if (m_ndx_beam > -1)
			// conversion to TCS units W/m2
			value( O_BEAM, atof(m_trnsys_values[m_ndx_beam].c_str())/3.6 );
			//value( O_BEAM, std::atof(m_trnsys_values[m_ndx_beam].c_str()) );
		else
			value( O_BEAM, 0.0 );

		if (m_ndx_diff > -1)
			value( O_DIFFUSE, atof(m_trnsys_values[m_ndx_diff].c_str())/3.6 );
		else
			value( O_DIFFUSE, 0.0 );

		if (m_ndx_tdry > -1)
			value( O_TDRY, atof(m_trnsys_values[m_ndx_tdry].c_str()) );
		else
			value( O_TDRY, 0.0 );

		if (m_ndx_twet > -1)
			value( O_TWET, atof(m_trnsys_values[m_ndx_twet].c_str()) );
		else
			value( O_TWET, 0.0 );

		if (m_ndx_tdew > -1)
			value( O_TDEW, atof(m_trnsys_values[m_ndx_tdew].c_str()) );
		else
			value( O_TDEW, 0.0 );

		if (m_ndx_wspd > -1)
			value( O_WSPD, atof(m_trnsys_values[m_ndx_wspd].c_str()) );
		else
			value( O_WSPD, 0.0 );

		if (m_ndx_wdir > -1)
			value( O_WDIR, atof(m_trnsys_values[m_ndx_wdir].c_str()) );
		else
			value( O_WDIR, 0.0 );

		if (m_ndx_rhum > -1)
			value( O_RHUM, atof(m_trnsys_values[m_ndx_rhum].c_str()) );
		else
			value( O_RHUM, 0.0 );

		// Convert from ATM to mbar
		if (m_ndx_pres > -1)
			value( O_PRES, atof(m_trnsys_values[m_ndx_pres].c_str())*1013.25 );
		else
			value( O_PRES, 0.0 );

		if (m_ndx_snow > -1)
			value( O_SNOW, atof(m_trnsys_values[m_ndx_snow].c_str()) );
		else
			value( O_SNOW, 0.0 );

		if (m_ndx_albedo > -1)
			value( O_ALBEDO, atof(m_trnsys_values[m_ndx_albedo].c_str()) );
		else
			value( O_ALBEDO, 0.0 );

		if (m_ndx_poa > -1)
			value( O_POA, atof(m_trnsys_values[m_ndx_poa].c_str())/3.6 );
		else
			value( O_POA, 0.0 );

		if (m_ndx_solazi > -1)
			// convert to TCS convention
			value( O_SOLAZI, atof(m_trnsys_values[m_ndx_solazi].c_str()) + 180 );
			//value( O_SOLAZI, std::atof(m_trnsys_values[m_ndx_solazi].c_str()) );
		else
			value( O_SOLAZI, 0.0 );

		if (m_ndx_solzen > -1)
			value( O_SOLZEN, atof(m_trnsys_values[m_ndx_solzen].c_str()) );
		else
			value( O_SOLZEN, 0.0 );

		if (m_ndx_lat > -1)
			value( O_LAT, atof(m_trnsys_values[m_ndx_lat].c_str()) );
		else
			value( O_LAT, 0.0 );

		if (m_ndx_lon > -1)
			value( O_LON, atof(m_trnsys_values[m_ndx_lon].c_str()) );
		else
			value( O_LON, 0.0 );

		if (m_ndx_tz > -1)
			value( O_TZ, atof(m_trnsys_values[m_ndx_tz].c_str()) );
		else
			value( O_TZ, 0.0 );

		if (m_ndx_shift > -1)
			value( O_SHIFT, atof(m_trnsys_values[m_ndx_shift].c_str()) );
		else
			value( O_SHIFT, 0.0 );

//		value(D_NDX_HOUR, m_ndx_hour);
//		value(D_NDX_SOLAZI, m_ndx_solazi);

		return 0; // success
	}
};



TCS_IMPLEMENT_TYPE( trnsys_weatherreader, "TRNSYS Weather File reader", "Steven Janzou", 1, trnsys_weatherreader_variables, NULL, 0 )

