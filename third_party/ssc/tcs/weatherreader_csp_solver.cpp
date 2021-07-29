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
#include <memory>
#include "tcstype.h"

#include "lib_weatherfile.h"
#include "lib_irradproc.h"

#include "csp_solver_core.h"

#ifndef M_PI
#define M_PI 3.14159265358979323
#endif




enum {	P_FILENAME, 

		P_TRACKMODE,
		P_TILT,
		P_AZIMUTH,

		O_YEAR,
		O_MONTH,
		O_DAY,
		O_HOUR,
		O_MINUTE,

		O_GLOBAL, 
		O_BEAM,
		O_HOR_BEAM,
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
		O_TZ,
		O_SHIFT,
		O_ELEV,

		// debugging outputs
		D_POABEAM,
		D_POADIFF,
		D_POAGND,


		N_MAX };

tcsvarinfo weatherreader_variables[] = {
	/* DIRECTION    DATATYPE      INDEX       NAME           LABEL                                  UNITS      GROUP    META    DEFAULTVALUE */
	{ TCS_PARAM,   TCS_STRING,   P_FILENAME, "file_name",   "Weather file name on local computer",  "",        "",      "",     "" },

	{ TCS_PARAM,   TCS_NUMBER,   P_TRACKMODE,"track_mode",  "Tracking mode for surface",            "0..2",    "Proc",  "0=fixed,1=1axis,2=2axis", "0" },
	{ TCS_PARAM,   TCS_NUMBER,   P_TILT,     "tilt",        "Tilt angle of surface/axis",           "deg",     "Proc",  "",     "" },
	{ TCS_PARAM,   TCS_NUMBER,   P_AZIMUTH,  "azimuth",     "Azimuth angle of surface/axis",        "deg",     "Proc",  "",     "" },

	{ TCS_OUTPUT,  TCS_NUMBER,   O_YEAR,     "year",        "Year",                                 "yr",      "Time",  "",     "" },
	{ TCS_OUTPUT,  TCS_NUMBER,   O_MONTH,    "month",       "Month",                                "mn",      "Time",  "",     "" },
	{ TCS_OUTPUT,  TCS_NUMBER,   O_DAY,      "day",         "Day",                                  "dy",      "Time",  "",     "" },
	{ TCS_OUTPUT,  TCS_NUMBER,   O_HOUR,     "hour",        "Hour",                                 "hr",      "Time",  "",     "" },
	{ TCS_OUTPUT,  TCS_NUMBER,   O_MINUTE,   "minute",      "Minute",                               "mi",      "Time",  "",     "" },

	{ TCS_OUTPUT,  TCS_NUMBER,   O_GLOBAL,   "global",      "Global horizontal irradiance",         "W/m2",    "Solar", "",     "" },
	{ TCS_OUTPUT,  TCS_NUMBER,   O_BEAM,     "beam",        "Beam normal irradiance",               "W/m2",    "Solar", "",     "" },
	{ TCS_OUTPUT,  TCS_NUMBER,   O_HOR_BEAM, "hor_beam",    "Beam-horizontal irradiance",           "W/m2",    "Solar", "",     "" },
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
	
	{ TCS_OUTPUT,  TCS_NUMBER,   O_POA,      "poa",         "Plane-of-array total incident irradiance", "W/m2","Irrad", "",     "" },
	{ TCS_OUTPUT,  TCS_NUMBER,   O_SOLAZI,   "solazi",      "Solar Azimuth",                        "deg",     "",      "",     "" },
	{ TCS_OUTPUT,  TCS_NUMBER,   O_SOLZEN,   "solzen",      "Solar Zenith",                         "deg",     "",      "",     "" },
	{ TCS_OUTPUT,  TCS_NUMBER,   O_LAT,      "lat",         "Latitude",                             "DDD",     "",      "",     "" },
	{ TCS_OUTPUT,  TCS_NUMBER,   O_LON,      "lon",         "Longitude",                            "DDD",     "",      "",     "" },
	{ TCS_OUTPUT,  TCS_NUMBER,   O_TZ,       "tz",          "Timezone",                             "DDD",     "",      "",     "" },
	{ TCS_OUTPUT,  TCS_NUMBER,   O_SHIFT,    "shift",       "shift in longitude from local standard meridian", "deg", "Solar", "", "" },
	{ TCS_OUTPUT,  TCS_NUMBER,   O_ELEV,     "elev",        "Site elevation",                       "m",       "Meteo", "",     "" },

	{ TCS_DEBUG,   TCS_NUMBER,   D_POABEAM,  "poa_beam",    "Plane-of-array beam irradiance",       "W/m2",    "Irrad", "",     "" },
	{ TCS_DEBUG,   TCS_NUMBER,   D_POADIFF,  "poa_diff",    "Plane-of-array diffuse irradiance",    "W/m2",    "Irrad", "",     "" },
	{ TCS_DEBUG,   TCS_NUMBER,   D_POAGND,   "poa_gnd",     "Plane-of-array ground irradiance",     "W/m2",    "Irrad", "",     "" },

	{ TCS_INVALID, TCS_INVALID,  N_MAX,       0,            0, 0, 0, 0, 0 }
};

class weatherreader : public tcstypeinterface
{
private:
	C_csp_weatherreader c_wr;
	const C_csp_weatherreader::S_outputs * m_wf;
	C_csp_solver_sim_info ms_sim_info;

public:
	weatherreader( tcscontext *cxt, tcstypeinfo *ti )
		: tcstypeinterface( cxt, ti ) { }

	virtual ~weatherreader() {}

	virtual int init()
	{
		c_wr.m_filename = value_str(P_FILENAME);
		c_wr.m_trackmode = (int) value(P_TRACKMODE);
		c_wr.m_tilt = value(P_TILT);
		c_wr.m_azimuth = value(P_AZIMUTH);
		

		int out_type = -1;
		std::string out_msg = "";

		try
		{
			if (c_wr.m_filename.size() > 0)
			{
				c_wr.m_weather_data_provider = std::make_shared<weatherfile>(c_wr.m_filename);
				if (c_wr.m_weather_data_provider->has_message()){
					message(TCS_ERROR, c_wr.m_weather_data_provider->message().c_str());
					return -1;
				}
			}
			c_wr.init();
		}

		catch( C_csp_exception &csp_exception )
		{
			// Report warning before exiting with error
			while( c_wr.mc_csp_messages.get_message(&out_type, &out_msg) )
			{
				if( out_type == C_csp_messages::NOTICE )
					message(TCS_NOTICE, out_msg.c_str());
				else if( out_type == C_csp_messages::WARNING )
					message(TCS_WARNING, out_msg.c_str());
			}

			message(TCS_ERROR, csp_exception.m_error_message.c_str());
			return -1;
		}

		// If no exception, then report messages and move on
		while( c_wr.mc_csp_messages.get_message(&out_type, &out_msg) )
		{
			if( out_type == C_csp_messages::NOTICE )
				message(TCS_NOTICE, out_msg.c_str());
			else if( out_type == C_csp_messages::WARNING )
				message(TCS_WARNING, out_msg.c_str());
		}

		return 0;
	}

	virtual int call( double time, double step, int /*ncall*/ )
	{
		// set sim info
		ms_sim_info.ms_ts.m_time = time;
		ms_sim_info.ms_ts.m_step = step;
		//ms_sim_info.m_ncall = ncall;

		int out_type = -1;
		std::string out_msg = "";

		try
		{
			c_wr.timestep_call(ms_sim_info);
		}

		catch( C_csp_exception &csp_exception)
		{
			// Report warning before exiting with error
			while( c_wr.mc_csp_messages.get_message(&out_type, &out_msg) )
			{
				if( out_type == C_csp_messages::NOTICE )
					message(TCS_NOTICE, out_msg.c_str());
				else if( out_type == C_csp_messages::WARNING )
					message(TCS_WARNING, out_msg.c_str());
			}

			message(TCS_ERROR, csp_exception.m_error_message.c_str());
			return -1;		
		}

		// If no exception, then report messages and move on
		while( c_wr.mc_csp_messages.get_message(&out_type, &out_msg) )
		{
			if( out_type == C_csp_messages::NOTICE )
				message(TCS_NOTICE, out_msg.c_str());
			else if( out_type == C_csp_messages::WARNING )
				message(TCS_WARNING, out_msg.c_str());
		}
		
		m_wf = &c_wr.ms_outputs;

		value(O_YEAR, m_wf->m_year);
		value(O_MONTH, m_wf->m_month);
		value(O_DAY, m_wf->m_day);
		value(O_HOUR, m_wf->m_hour);
		value(O_MINUTE, m_wf->m_minute);

		value(O_GLOBAL, m_wf->m_global);
		value(O_BEAM, m_wf->m_beam);
		value(O_HOR_BEAM, m_wf->m_hor_beam);
		value(O_DIFFUSE, m_wf->m_diffuse);
		value(O_TDRY, m_wf->m_tdry);
		value(O_TWET, m_wf->m_twet);
		value(O_TDEW, m_wf->m_tdew);
		value(O_WSPD, m_wf->m_wspd);
		value(O_WDIR, m_wf->m_wdir);
		value(O_RHUM, m_wf->m_rhum);
		value(O_PRES, m_wf->m_pres);
		value(O_SNOW, m_wf->m_snow);
		value(O_ALBEDO, m_wf->m_albedo);

		value(O_POA, m_wf->m_poa);
		value(O_SOLAZI, m_wf->m_solazi);
		value(O_SOLZEN, m_wf->m_solzen);
		value(O_LAT, m_wf->m_lat);
		value(O_LON, m_wf->m_lon);
		value(O_TZ, m_wf->m_tz);
		value(O_SHIFT, m_wf->m_shift);
		value(O_ELEV, m_wf->m_elev);

		return 0;
	}

	virtual int converged( double /*time*/ )
	{
		int out_type = -1;
		std::string out_msg = "";

		try
		{
			c_wr.converged();
		}

		catch( C_csp_exception &csp_exception )
		{
			// Report warning before exiting with error
			while( c_wr.mc_csp_messages.get_message(&out_type, &out_msg) )
			{
				if( out_type == C_csp_messages::NOTICE )
					message(TCS_NOTICE, out_msg.c_str());
				else if( out_type == C_csp_messages::WARNING )
					message(TCS_WARNING, out_msg.c_str());
			}

			message(TCS_ERROR, csp_exception.m_error_message.c_str());
			return -1;
		}

		// If no exception, then report messages and move on
		while( c_wr.mc_csp_messages.get_message(&out_type, &out_msg) )
		{
			if( out_type == C_csp_messages::NOTICE )
				message(TCS_NOTICE, out_msg.c_str());
			else if( out_type == C_csp_messages::WARNING )
				message(TCS_WARNING, out_msg.c_str());
		}

		return 0;
	
	}
};

TCS_IMPLEMENT_TYPE( weatherreader, "Standard Weather File format reader", "Aron Dobos", 1, weatherreader_variables, NULL, 1 )
