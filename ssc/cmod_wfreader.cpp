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
#include "lib_weatherfile.h"

static var_info _cm_vtab_wfreader[] = {
/*   VARTYPE           DATATYPE         NAME                           LABEL                                UNITS     META                      GROUP                      REQUIRED_IF                 CONSTRAINTS                      UI_HINTS*/
	{ SSC_INPUT,         SSC_STRING,      "file_name",               "local weather file path",          "",       "",                      "Weather Reader",      "*",                       "LOCAL_FILE",      "" },
	{ SSC_INPUT,         SSC_NUMBER,      "header_only",             "read header only",                 "0/1",    "",                      "Weather Reader",      "?=0",                     "BOOLEAN",      "" },
	
// header data
	{ SSC_OUTPUT,        SSC_NUMBER,      "lat",                     "Latitude",                         "deg",    "",                      "Weather Reader",      "*",                        "",                      "" },
	{ SSC_OUTPUT,        SSC_NUMBER,      "lon",                     "Longitude",                        "deg",    "",                      "Weather Reader",      "*",                        "",                      "" },
	{ SSC_OUTPUT,        SSC_NUMBER,      "tz",                      "Time zone",                        "hr",     "",                      "Weather Reader",      "*",                        "",                      "" },
	{ SSC_OUTPUT,        SSC_NUMBER,      "elev",                    "Elevation",                        "m",      "",                      "Weather Reader",      "*",                        "",                      "" },
	{ SSC_OUTPUT,        SSC_STRING,      "location",                "Location ID",                      "",       "",                      "Weather Reader",      "*",                        "",                      "" },
	{ SSC_OUTPUT,        SSC_STRING,      "city",                    "City",                             "",       "",                      "Weather Reader",      "*",                        "",                      "" },
	{ SSC_OUTPUT,        SSC_STRING,      "state",                   "State",                            "",       "",                      "Weather Reader",      "*",                        "",                      "" },
	{ SSC_OUTPUT,        SSC_STRING,      "country",                 "Country",                          "",       "",                      "Weather Reader",      "*",                        "",                      "" },
	{ SSC_OUTPUT,        SSC_STRING,      "description",             "Description",                      "",       "",                      "Weather Reader",      "*",                        "",                      "" },
	{ SSC_OUTPUT,        SSC_STRING,      "source",                  "Source",                           "",       "",                      "Weather Reader",      "*",                        "",                      "" },
	{ SSC_OUTPUT,        SSC_STRING,      "url",                     "URL",                              "",       "",                      "Weather Reader",      "*",                        "",                      "" },
	{ SSC_OUTPUT,        SSC_STRING,      "format",                  "File format",                      "",       "tmy2,tmy3,epw,smw,wfcsv", "Weather Reader",    "*",                        "",                      "" },
	
	{ SSC_OUTPUT,        SSC_NUMBER,      "start",                   "Start",                            "sec",    "",                      "Weather Reader",      "*",                       "",                          "" },
	{ SSC_OUTPUT,        SSC_NUMBER,      "step",                    "Step",                             "sec",    "",                      "Weather Reader",      "*",                       "",                          "" },
	{ SSC_OUTPUT,        SSC_NUMBER,      "nrecords",                "Number of records",                "",       "",                      "Weather Reader",      "*",                       "",                          "" },


// timestamp data
	{ SSC_OUTPUT,        SSC_ARRAY,       "year",                    "Year",                             "yr",     "",                      "Weather Reader",      "header_only=0",                       "LENGTH_EQUAL=year",               "" },
	{ SSC_OUTPUT,        SSC_ARRAY,       "month",                   "Month",                            "mn",     "1-12",                  "Weather Reader",      "header_only=0",                       "LENGTH_EQUAL=year",                          "" },
	{ SSC_OUTPUT,        SSC_ARRAY,       "day",                     "Day",                              "dy",     "1-365",                 "Weather Reader",      "header_only=0",                       "LENGTH_EQUAL=year",                          "" },
	{ SSC_OUTPUT,        SSC_ARRAY,       "hour",                    "Hour",                             "hr",     "0-23",                  "Weather Reader",      "header_only=0",                       "LENGTH_EQUAL=year",                          "" },
	{ SSC_OUTPUT,        SSC_ARRAY,       "minute",                  "Minute",                           "min",    "0-59",                  "Weather Reader",      "header_only=0",                       "LENGTH_EQUAL=year",                          "" },

// solar & weather data records
	{ SSC_OUTPUT,        SSC_ARRAY,       "global",                  "Global Horizontal Irradiance",     "W/m2",   "",                      "Weather Reader",      "header_only=0",                        "LENGTH_EQUAL=year",                      "" },
	{ SSC_OUTPUT,        SSC_ARRAY,       "beam",                    "Beam Normal Irradiance",           "W/m2",   "",                      "Weather Reader",      "header_only=0",                        "LENGTH_EQUAL=year",                      "" },
	{ SSC_OUTPUT,        SSC_ARRAY,       "diffuse",                 "Diffuse Horizontal Irradiance",    "W/m2",   "",                      "Weather Reader",      "header_only=0",                        "LENGTH_EQUAL=year",     "" },
	{ SSC_OUTPUT,        SSC_ARRAY,       "poa",                     "Plane of Array Irradiance",        "W/m2",   "",                      "Weather Reader",      "header_only=0",                        "LENGTH_EQUAL=year",     "" },

	{ SSC_OUTPUT,        SSC_ARRAY,       "wspd",                    "Wind Speed",                       "m/s",   "",                       "Weather Reader",      "header_only=0",                        "LENGTH_EQUAL=year",     "" },
	{ SSC_OUTPUT,        SSC_ARRAY,       "wdir",                    "Wind Direction",                   "deg",   "0=N,E=90",               "Weather Reader",      "header_only=0",                        "LENGTH_EQUAL=year",     "" },
	{ SSC_OUTPUT,        SSC_ARRAY,       "tdry",                    "Temperature Dry Bulb",            "'C",    "",                       "Weather Reader",      "header_only=0",                        "LENGTH_EQUAL=year",     "" },
	{ SSC_OUTPUT,        SSC_ARRAY,       "twet",                    "Temperature Wet Bulb",            "'C",    "",                       "Weather Reader",      "header_only=0",                        "LENGTH_EQUAL=year",     "" },
	{ SSC_OUTPUT,        SSC_ARRAY,       "tdew",                    "Temperature Dew Point",           "'C",    "",                       "Weather Reader",      "header_only=0",                        "LENGTH_EQUAL=year",     "" },
	{ SSC_OUTPUT,        SSC_ARRAY,       "rhum",                    "Relative Humidity",                "%",     "",                       "Weather Reader",      "header_only=0",                        "LENGTH_EQUAL=year",     "" },
	{ SSC_OUTPUT,        SSC_ARRAY,       "pres",                    "Atmospheric Pressure",             "millibar", "",                    "Weather Reader",      "header_only=0",                        "LENGTH_EQUAL=year",     "" },
	{ SSC_OUTPUT,        SSC_ARRAY,       "snow",                    "Snow Depth",                       "cm",    "",                       "Weather Reader",      "header_only=0",                        "LENGTH_EQUAL=year",     "" },
	{ SSC_OUTPUT,        SSC_ARRAY,       "albedo",                  "Ground Reflectance",               "frac",  "0..1",                   "Weather Reader",      "header_only=0",                        "LENGTH_EQUAL=year",     "" },

// annual statistics
	{ SSC_OUTPUT,        SSC_NUMBER,      "annual_global",           "Average daily global horizontal",  "kWh/m2/day",   "",                "Weather Reader",      "header_only=0",                        "",     "" },
	{ SSC_OUTPUT,        SSC_NUMBER,      "annual_beam",             "Average daily beam normal",        "kWh/m2/day",   "",                "Weather Reader",      "header_only=0",                        "",     "" },
	{ SSC_OUTPUT,        SSC_NUMBER,      "annual_diffuse",          "Average daily diffuse",            "kWh/m2/day",   "",                "Weather Reader",      "header_only=0",                        "",     "" },
	{ SSC_OUTPUT,        SSC_NUMBER,      "annual_tdry",             "Average dry bulb temperature",     "'C",           "",                "Weather Reader",      "header_only=0",                        "",     "" },
	{ SSC_OUTPUT, SSC_NUMBER, "annual_wspd", "Average wind speed", "m/s", "", "Weather Reader", "header_only=0", "", "" },

	{ SSC_OUTPUT, SSC_NUMBER, "annual_snow", "Maximum snow depth", "cm", "", "Weather Reader", "header_only=0", "", "" },
	{ SSC_OUTPUT, SSC_NUMBER, "annual_albedo", "Average albedo", "", "", "Weather Reader", "header_only=0", "", "" },

var_info_invalid };

class cm_wfreader : public compute_module
{
public:

	cm_wfreader()
	{
		add_var_info( _cm_vtab_wfreader );
	}
	
	void exec( )
	{
		bool header_only = as_boolean("header_only");
		const char *file = as_string("file_name");

		weatherfile wfile( file, header_only );
		if (!wfile.ok()) 
		{
			assign( "error", var_data(wfile.message()) );
			throw exec_error("wfreader", "failed to read local weather file: " + std::string(file) + "  " + wfile.message());
		}
		
		if( wfile.has_message() ) log( wfile.message(), SSC_WARNING );

		weather_header hdr;
		wfile.header( &hdr );

		size_t records = wfile.nrecords();
		
		assign( "lat", var_data( (ssc_number_t)hdr.lat ) );
		assign( "lon", var_data( (ssc_number_t)hdr.lon ) );
		assign( "tz", var_data( (ssc_number_t)hdr.tz ) );
		assign( "elev", var_data( (ssc_number_t)hdr.elev ) );
		assign( "location", var_data( std::string( hdr.location ) ) );
		assign( "city", var_data( std::string( hdr.city ) ) );
		assign( "state", var_data( std::string( hdr.state ) ) );
		assign( "country", var_data( std::string( hdr.country ) ) );
		assign( "description", var_data( std::string( hdr.description ) ) );
		assign( "source", var_data( std::string( hdr.source ) ) );
		assign( "url", var_data( std::string( hdr.url ) ) );

		assign( "start", var_data( (ssc_number_t)wfile.start_sec() ) );
		assign( "step", var_data( (ssc_number_t)wfile.step_sec() ) );
		assign( "nrecords", var_data( (ssc_number_t)wfile.nrecords() ) );

		switch( wfile.type() )
		{
		case weatherfile::TMY2: assign("format", var_data("tmy2") ); break;
		case weatherfile::TMY3: assign("format", var_data("tmy3") ); break;
		case weatherfile::EPW: assign("format", var_data("epw") ); break;
		case weatherfile::SMW: assign("format", var_data("smw") ); break;
		case weatherfile::WFCSV: assign("format", var_data("csv") ); break;
		default: assign("format", var_data("invalid")); break;
		}

		if ( header_only )
			return;

		ssc_number_t *p_year = allocate( "year", records );
		ssc_number_t *p_month = allocate( "month", records );
		ssc_number_t *p_day = allocate( "day", records );
		ssc_number_t *p_hour = allocate( "hour", records );
		ssc_number_t *p_minute = allocate( "minute", records );
		
		ssc_number_t *p_global = allocate( "global", records );
		ssc_number_t *p_beam = allocate( "beam", records );
		ssc_number_t *p_diffuse = allocate( "diffuse", records );
		ssc_number_t *p_poa = allocate( "poa", records );
		
		ssc_number_t *p_wspd = allocate( "wspd", records );
		ssc_number_t *p_wdir = allocate( "wdir", records );
		ssc_number_t *p_tdry = allocate( "tdry", records );
		ssc_number_t *p_twet = allocate( "twet", records );
		ssc_number_t *p_tdew = allocate( "tdew", records );
		ssc_number_t *p_rhum = allocate( "rhum", records );
		ssc_number_t *p_pres = allocate( "pres", records );
		ssc_number_t *p_snow = allocate( "snow", records );
		ssc_number_t *p_albedo = allocate( "albedo", records );

		double gh_sum = 0.0, dn_sum = 0.0, df_sum = 0.0;
		double temp_sum = 0.0, wind_sum = 0.0, albedo_sum = 0.0;
		double snow_max = -1;

		double ts_hour = wfile.step_sec() / 3600.0;

		weather_record wf;

		for (int i=0;i<(int)records;i++)
		{
			if (!wfile.read( &wf ))
				throw exec_error("wfreader", "could not read data line " + util::to_string(i+1) + " of 8760");

			p_year[i] = (ssc_number_t)wf.year;
			p_month[i] = (ssc_number_t)wf.month;
			p_day[i] = (ssc_number_t)wf.day;
			p_hour[i] = (ssc_number_t)wf.hour;
			p_minute[i] = (ssc_number_t)wf.minute;

			p_global[i] = (ssc_number_t)wf.gh;
			p_beam[i] = (ssc_number_t)wf.dn;
			p_diffuse[i] = (ssc_number_t)wf.df;
			p_poa[i] = (ssc_number_t)wf.poa;

			p_wspd[i] = (ssc_number_t)wf.wspd;
			p_wdir[i] = (ssc_number_t)wf.wdir;
			p_tdry[i] = (ssc_number_t)wf.tdry;
			p_twet[i] = (ssc_number_t)wf.twet;
			p_tdew[i] = (ssc_number_t)wf.tdew;
			p_rhum[i] = (ssc_number_t)wf.rhum;
			p_pres[i] = (ssc_number_t)wf.pres;
			p_snow[i] = (ssc_number_t)wf.snow;
			p_albedo[i] = (ssc_number_t)wf.alb;	

			gh_sum += wf.gh * ts_hour;
			dn_sum += wf.dn * ts_hour;
			df_sum += wf.df * ts_hour;
			temp_sum += wf.tdry;
			wind_sum += wf.wspd; 
			albedo_sum += wf.alb;
			if (!std::isnan(wf.snow) && (wf.snow > snow_max))
				snow_max = wf.snow;
		}
		
		if (snow_max < 0)
			snow_max = std::numeric_limits<double>::quiet_NaN();


		assign("annual_global", var_data((ssc_number_t)(0.001 * gh_sum / 365)));
		assign("annual_beam", var_data((ssc_number_t) (0.001 * dn_sum / 365)));
		assign("annual_diffuse", var_data((ssc_number_t)(0.001 * df_sum / 365)));
		assign("annual_tdry", var_data((ssc_number_t)(temp_sum / records)));
		assign("annual_wspd", var_data((ssc_number_t)(wind_sum / records)));
		assign("annual_snow", var_data((ssc_number_t)snow_max));
		assign("annual_albedo", var_data((ssc_number_t)(albedo_sum / records)));

	}
};

DEFINE_MODULE_ENTRY( wfreader, "Standard Weather File Format Reader (TMY2, TMY3, EPW, SMW, WFCSV)", 1 )
