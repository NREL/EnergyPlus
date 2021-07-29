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

#include "core.h"
#include "lib_util.h"
#include "lib_windfile.h"

static var_info _cm_wind_file_reader[] = {
/*   VARTYPE           DATATYPE         NAME                           LABEL                                        UNITS     META                      GROUP                 REQUIRED_IF                CONSTRAINTS        UI_HINTS*/
	{ SSC_INPUT,         SSC_STRING,      "file_name",               "local weather file path",                     "",       "",                      "Weather Reader",      "*",                       "LOCAL_FILE",      "" },
	{ SSC_INPUT,         SSC_NUMBER,      "scan_header_only",	     "only reader headers",                         "0/1",    "",                      "Weather Reader",      "?=0",                     "BOOLEAN",         "" },
	{ SSC_INPUT,         SSC_NUMBER,      "requested_ht",	         "requested measurement height",                "m",      "",                      "Weather Reader",      "*",                       "",                "" },
	{ SSC_INPUT,         SSC_NUMBER,      "interpolate",	         "interpolate to closest height measured?",     "m",      "",                      "Weather Reader",      "scan_header_only=0",      "BOOLEAN",         "" },

// header data
	{ SSC_OUTPUT,        SSC_STRING,      "city",                    "City",                                        "",       "",                      "Weather Reader",      "*",                        "",               "" },
	{ SSC_OUTPUT,        SSC_STRING,      "state",                   "State",                                       "",       "",                      "Weather Reader",      "*",                        "",               "" },
	{ SSC_OUTPUT,        SSC_STRING,      "location_id",             "Location ID",                                 "",       "",                      "Weather Reader",      "*",                        "",               "" },
	{ SSC_OUTPUT,        SSC_STRING,      "country",                 "Country",                                     "",       "",                      "Weather Reader",      "*",                        "",               "" },
	{ SSC_OUTPUT,        SSC_STRING,      "description",             "Description",                                 "",       "",                      "Weather Reader",      "*",                        "",               "" },
																										            
	{ SSC_OUTPUT,        SSC_NUMBER,      "year",                    "Calendar year of data",                       "",       "",                      "Weather Reader",      "*",                        "INTEGER",        "" },
	{ SSC_OUTPUT,        SSC_NUMBER,      "lat",                     "Latitude",                                    "deg",    "",                      "Weather Reader",      "*",                        "",               "" },
	{ SSC_OUTPUT,        SSC_NUMBER,      "lon",                     "Longitude",                                   "deg",    "",                      "Weather Reader",      "*",                        "",               "" },
	{ SSC_OUTPUT,        SSC_NUMBER,      "elev",                    "Elevation",                                   "m",      "",                      "Weather Reader",      "*",                        "",               "" },
																										            
//	{ SSC_OUTPUT,        SSC_NUMBER,      "nrecords",                "Number of records",                           "",       "",                      "Weather Reader",      "*",                        "",                          "" },
	{ SSC_OUTPUT,        SSC_NUMBER,      "closest_speed_meas_ht",   "Height of closest speed meas in file",        "m",      "",                      "Weather Reader",      "*",                        "",                          "" },
	{ SSC_OUTPUT,        SSC_NUMBER,      "closest_dir_meas_ht",     "Height of closest direction meas in file",    "m",      "",                      "Weather Reader",      "*",                        "",                          "" },
																										            
// weather data records																					            
	{ SSC_OUTPUT,        SSC_ARRAY,       "wind_speed",              "Wind Speed",                                  "m/s",   "",                       "Weather Reader",      "*",                        "",                            "" },
	{ SSC_OUTPUT,        SSC_ARRAY,       "wind_direction",          "Wind Direction",                              "deg",   "0=N,E=90",               "Weather Reader",      "*",                        "LENGTH_EQUAL=wind_speed",     "" },
	{ SSC_OUTPUT,        SSC_ARRAY,       "temperature",             "Temperature",                                 "'C",    "",                       "Weather Reader",      "*",                        "LENGTH_EQUAL=wind_speed",     "" },
	{ SSC_OUTPUT,        SSC_ARRAY,       "pressure",                "Atmospheric Pressure",                        "atm",   "",                       "Weather Reader",      "*",                        "LENGTH_EQUAL=wind_speed",     "" },

var_info_invalid };

class cm_wind_file_reader : public compute_module
{
public:

	cm_wind_file_reader()
	{
		add_var_info(_cm_wind_file_reader);
	}
	
	void exec( )
	{	
		const char *file = as_string("file_name");

		windfile wf( file );
		if (!wf.ok())
			throw exec_error("windfile", "failed to read local weather file: " + std::string(file) + " " + wf.error());

		assign( "city", var_data( std::string( wf.city ) ) );
		assign( "state", var_data( std::string( wf.state ) ) );
		assign( "location_id", var_data( std::string( wf.locid ) ) );
		assign( "country", var_data(std::string(wf.country)));
		assign( "description", var_data(std::string(wf.desc)));

		assign( "year", var_data( (ssc_number_t)wf.year));
		assign( "lat", var_data( (ssc_number_t)wf.lat));
		assign( "lon", var_data( (ssc_number_t)wf.lon ) );
		assign( "elev", var_data( (ssc_number_t)wf.elev ) );

		bool bHeaderOnly = as_boolean("scan_header_only");

		double wind, dir, temp, pres, closest_speed_meas_ht=0, closest_dir_meas_ht=0;
		if (bHeaderOnly) {
			if (!wf.read(as_double("requested_ht"), &wind, &dir, &temp, &pres, &closest_speed_meas_ht, &closest_dir_meas_ht))
				throw exec_error("windpower", util::format("error reading wind resource file at %d: ", 1) + wf.error());

			assign("closest_speed_meas_ht", var_data((ssc_number_t)closest_speed_meas_ht));
			assign("closest_dir_meas_ht", var_data((ssc_number_t)closest_dir_meas_ht));

			allocate("wind_speed", 1 );
			allocate("wind_direction", 1);
			allocate("temperature", 1);
			allocate("pressure", 1);
			return;
		}

		int nsteps = 8760;
		ssc_number_t *p_speed = allocate("wind_speed", nsteps);
		ssc_number_t *p_dir = allocate("wind_direction", nsteps);
		ssc_number_t *p_temp = allocate("temperature", nsteps);
		ssc_number_t *p_pres = allocate("pressure", nsteps);

		for (int i = 0; i<nsteps; i++)
		{
			if (!wf.read(as_double("requested_ht"), &wind, &dir, &temp, &pres, &closest_speed_meas_ht, &closest_dir_meas_ht, as_boolean("interpolate") ) )
				throw exec_error("windpower", util::format("error reading wind resource file at %d: ", i) + wf.error());

			p_speed[i] = (ssc_number_t)wind;
			p_dir[i] = (ssc_number_t)dir;
			p_temp[i] = (ssc_number_t)temp;
			p_pres[i] = (ssc_number_t)pres;
		}

		assign("closest_speed_meas_ht", var_data((ssc_number_t)closest_speed_meas_ht));
		assign("closest_dir_meas_ht", var_data((ssc_number_t)closest_dir_meas_ht));

		return;
	}
};

DEFINE_MODULE_ENTRY(wind_file_reader, "SAM Wind Resource File Reader (SRW)", 1)
