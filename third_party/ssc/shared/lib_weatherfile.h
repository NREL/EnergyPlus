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

#ifndef __lib_weatherfile_h
#define __lib_weatherfile_h

#include <string>
#include <vector>
#include <algorithm>
#include <cmath>




/***************************************************************************\

   Function humidity()
	This function calculates the relative humidity(%) based on the drybulb
	temperature(C) and the dewpoint temperature.  It uses equations and
	procedures presented in the 1993 ASHRAE Fundamentals Handbook, p6.7-10.
	If humidity cannot be calculated an error value of 999 is returned.
														  1/4/00
	List of Parameters Passed to Function:
	db     = dry bulb temperature in degrees C
	dpt    = dew point temperature in degrees C

	Variable Returned
	rh    = relative humidity in %, or error value of 999  

\***************************************************************************/
int calc_humidity(float db,float dpt);

/* This function calculates the dewpoint temperature(C) based on the drybulb
	temperature(C) and the relative humidity(%).  It uses equations and
	procedures presented in the 1993 ASHRAE Fundamentals Handbook, p6.7-10.
	If dewpoint cannot be calculated an error value of 99.9 is returned.

	List of Parameters Passed to Function:
	db     = dry bulb temperature in degrees C
	rh     = relative humidity in %

	Variable Returned
	dpt    = dew point temperature in degrees C, or error value of 99.9   */
float calc_dewpt(float db,float rh);

// Calculate wet bulb temperature from T (dry bulb, 'C), RH (%), Pressure (mbar)
// see http://www.ejournal.unam.mx/atm/Vol07-3/ATM07304.pdf for eqns.
double calc_twet( double T, double RH, double P );

static inline double wiki_dew_calc(double T, double RH)
{
	// ref: http://en.wikipedia.org/wiki/Dew_point

	if (RH > 0 && RH < 100)
	{
		static const double a = 17.271;
		static const double b = 237.7;
		double gamma = a*T / (b + T) + log(RH / 100.0);
		double denom = a - gamma;
		if (denom != 0.0)
			return b*gamma / denom;
	}

	// ultra-simple equation (OK as long as RH > 50%)
	return  T - (100 - RH) / 5;
};

struct weather_header {	
	weather_header() { reset(); }
	void reset();

	std::string location;
	std::string city;
	std::string state;
	std::string country;
	std::string source;
	std::string description;
	std::string url;
	bool hasunits;
	double tz;
	double lat;
	double lon;
	double elev;
};

struct weather_record {		
	weather_record() { reset(); }
	void reset();
	
	int year;
	int month;
	int day;
	int hour;
	double minute;
	double gh;     // global (W/m2)
	double dn;     // direct (W/m2)
	double df;     // diffuse (W/m2)
	double poa;    // plane of array (W/m2)
	double wspd;   // wind speed (m/s)
	double wdir;   // wind direction (deg: N = 0 or 360, E = 90, S = 180,W = 270 )
	double tdry;   // dry bulb temp (C)
	double twet;   // wet bulb temp (C)
	double tdew;   // dew point temp (C)
	double rhum;   // relative humidity (%)
	double pres;   // pressure (mbar)
	double snow;   // snow depth (cm) 0-150
	double alb; // ground reflectance 0-1.  values outside this range mean it is not included
	double aod;    // aerosol optical depth
};

class weather_data_provider
{
public:
	enum { YEAR, MONTH, DAY, HOUR, MINUTE,
		GHI, DNI, DHI, POA, 
		TDRY, TWET, TDEW, 
		WSPD, WDIR, 
		RH, PRES, SNOW, ALB, AOD,
	_MAXCOL_ };

protected:
	bool m_ok;
	bool m_msg;
    int m_startYear;
    int m_hour_of_year = -1; // For error checking
	double m_time;
	// error messages
	std::string m_message;

	size_t m_startSec;
	size_t m_stepSec;
	size_t m_nRecords;
	size_t m_index;
	bool m_hasLeapYear = false;
	bool m_continuousYear = true; //boolean to identify if the provided weather data is a complete, continuous, single year with an even timestep- 
									//OR- if it's non-annual or single timestep
	
	weather_header m_hdr;
	bool m_hdrInitialized;

public:
	weather_data_provider() : m_hdrInitialized( false ) { }
	virtual ~weather_data_provider() { }
	
	bool header(weather_header *hdr)
	{
		if (!hdr) return false;
		*hdr = m_hdr;
		return true;
	}
	bool ok(){ return m_ok; }
	size_t start_sec(){ return m_startSec; } // start time in seconds, 0 = jan 1st midnight
	size_t step_sec(){ return m_stepSec; } // step time in seconds
	size_t nrecords(){ return m_nRecords; } // number of data records in file
	bool annualSimulation() { return m_continuousYear; } //whether the weather data is a complete, continuous, single year with an even timestep, OR, if it's non-annual/single timestep
	int get_counter_value(){ return (int)m_index; }
	void rewind(){ m_index = 0; }

	///set the m_index to a specified value. next read will be at this time step index.
	void set_counter_to(size_t cur_index) {
		if (cur_index < m_nRecords){
			m_index = cur_index;
		}
	}
	
	bool has_message() { return m_message.size() > 0; }
	std::string message() { return m_message; }

	double lat() { return header().lat; }
	double lon() { return header().lon; }
	double tz() { return header().tz; }
	double elev() { return header().elev; }

    bool check_hour_of_year(int hour, int line);

	// virtual functions specific to weather data source
	/// check if the data is available from weather file
	virtual bool has_data_column(size_t id) = 0;

	/// reads one more record
	virtual bool read( weather_record *r ) = 0; 


	// some helper methods for ease of use of this class
	virtual weather_header &header()  {
		if ( !m_hdrInitialized )
			m_hdrInitialized = header( &m_hdr );
			
		return m_hdr;
	}
};

class weatherfile : public weather_data_provider
{
private:
	int m_type;
	std::string m_file;

	struct column
	{
		int index; // used for wfcsv to get column index in CSV file from which to read
		std::vector<float> data;
	};
	column m_columns[_MAXCOL_];

    void start_hours_at_0();

public:
	weatherfile();
	/* Detects file format, read header information, detects which data columns are available and at what index
	and read weather record information.
	Calculates twet if missing*/
	weatherfile( const std::string &file, bool header_only = false );
	virtual ~weatherfile();

	void reset();
	enum { INVALID, TMY2, TMY3, EPW, SMW, WFCSV };
	int type();
	std::string filename();

	/// Check field for missing values & return interpolant as necessary
	void handle_missing_field(size_t index, int col);

	/// Check timestep of weatherfile and leap year, returns true if success
	bool timeStepChecks(int hdr_step_sec = -1);

	bool open( const std::string &file, bool header_only = false );

	bool read( weather_record *r ); 
	bool read_average(weather_record *r, std::vector<int> &cols, size_t &num_timesteps);
	bool has_data_column( size_t id );
	
	static std::string normalize_city( const std::string &in );
	static bool convert_to_wfcsv( const std::string &input, const std::string &output );
	
};



#endif

