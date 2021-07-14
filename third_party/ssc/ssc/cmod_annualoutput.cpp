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

#include "lib_financial.h"
using namespace libfin;
#include "core.h"
#include <sstream>

#ifndef WIN32
#include <float.h>
#endif


static var_info _cm_vtab_annualoutput[] = {


/*   VARTYPE           DATATYPE         NAME                           LABEL                                    UNITS     META                                      GROUP                REQUIRED_IF                 CONSTRAINTS                     UI_HINTS*/
	{ SSC_INPUT,        SSC_NUMBER,     "analysis_period",              "Analyis period",                        "years",  "",                                       "AnnualOutput",      "?=30",                   "INTEGER,MIN=0,MAX=50",           "" },
	{ SSC_INPUT,        SSC_ARRAY,      "energy_availability",		   "Annual energy availability",	        "%",      "",                                       "AnnualOutput",      "*",						"",                              "" },
	{ SSC_INPUT,        SSC_ARRAY,      "energy_degradation",		   "Annual energy degradation",	            "%",      "",                                       "AnnualOutput",      "*",						"",                              "" },
	{ SSC_INPUT,        SSC_MATRIX,     "energy_curtailment",		   "First year energy curtailment",	         "",      "(0..1)",                                 "AnnualOutput",      "*",						"",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "system_use_lifetime_output",  "Lifetime hourly system outputs",        "0/1",    "0=hourly first year,1=hourly lifetime",  "AnnualOutput",      "*",						"INTEGER,MIN=0",                 "" },
//	{ SSC_INPUT,        SSC_ARRAY,		"energy_net",	       "Hourly energy produced by the system",  "kW",     "",                                       "AnnualOutput",      "*",						"",                              "" },
	{ SSC_INPUT,        SSC_ARRAY,		"system_hourly_energy",	       "Hourly energy produced by the system",  "kW",     "",                                       "AnnualOutput",      "*",						"",                              "" },


/* output */
//	{ SSC_OUTPUT,        SSC_ARRAY,     "annual_e_net_delivered",               "Annual energy",                            "kWh",     "",                                      "AnnualOutput",      "*",                      "",                               "" },
	{ SSC_OUTPUT,        SSC_ARRAY,     "annual_energy",               "Annual energy",                            "kWh",     "",                                      "AnnualOutput",      "*",                      "",                               "" },
//	{ SSC_OUTPUT,        SSC_ARRAY,     "monthly_e_net_delivered",               "Monthly energy",                            "kWh",     "",                                      "AnnualOutput",      "*",                      "",                               "" },
	{ SSC_OUTPUT,        SSC_ARRAY,     "monthly_energy",               "Monthly energy",                            "kWh",     "",                                      "AnnualOutput",      "*",                      "",                               "" },
//	{ SSC_OUTPUT,        SSC_ARRAY,     "hourly_e_net_delivered",               "Hourly energy",                            "kWh",     "",                                      "AnnualOutput",      "*",                      "",                               "" },
	{ SSC_OUTPUT,        SSC_ARRAY,     "hourly_energy",               "Hourly energy",                            "kWh",     "",                                      "AnnualOutput",      "*",                      "",                               "" },
	{ SSC_OUTPUT,        SSC_ARRAY,     "annual_availability",               "Annual availability",                            "",     "",                                      "AnnualOutput",      "*",                      "",                               "" },
	{ SSC_OUTPUT,        SSC_ARRAY,     "annual_degradation",               "Annual degradation",                            "",     "",                                      "AnnualOutput",      "*",                      "",                               "" },

var_info_invalid };

extern var_info
	vtab_standard_financial[],
	vtab_oandm[],
	vtab_tax_credits[],
	vtab_payment_incentives[];

enum {
	CF_energy_net,

	CF_availability,
	CF_degradation,

	CF_max };



class cm_annualoutput : public compute_module
{
private:
	util::matrix_t<double> cf;
//	std::vector<double> hourly_curtailment;


public:
	cm_annualoutput()
	{
		add_var_info( _cm_vtab_annualoutput );
	}

	void exec( )
	{

		int nyears = as_integer("analysis_period");
		cf.resize_fill( CF_max, nyears+1, 0.0 );


		int i=0;
		size_t count_avail = 0;
		ssc_number_t *avail = 0;
		avail = as_array("energy_availability", &count_avail);
		size_t count_degrad = 0;
		ssc_number_t *degrad = 0;
		degrad = as_array("energy_degradation", &count_degrad);

		// degradation starts in year 2 for single value degradation - no degradation in year 1 - degradation =1.0
		if (count_degrad == 1)
		{
			if (as_integer("system_use_lifetime_output"))
			{
				if (nyears>=1) cf.at(CF_degradation,1) = 1.0;
				for (i=2;i<=nyears;i++) cf.at(CF_degradation,i) = 1.0 - degrad[0]/100.0;
			}
			else
				for (i=1;i<=nyears;i++) cf.at(CF_degradation,i) = pow((1.0 - degrad[0]/100.0),i-1);
		}
		else if (count_degrad > 0)
		{
			for (i=0;i<nyears && i<(int)count_degrad;i++) cf.at(CF_degradation,i+1) = (1.0 - degrad[i]/100.0);
		}

		if (count_avail == 1)
		{
			for (i=1;i<=nyears;i++) cf.at(CF_availability,i)  = avail[0]/100.0;
		}
		else if (count_avail > 0)
		{
			for (i=0;i<nyears && i<(int)count_avail;i++) cf.at(CF_availability,i+1) = avail[i]/100.0;
		}

		// dispatch
		if (as_integer("system_use_lifetime_output"))
		{
			compute_lifetime_output(nyears);
		}
		else
		{
			compute_output(nyears);
		}

		save_cf( CF_energy_net, nyears,"annual_energy" );
		
		save_cf( CF_availability, nyears,"annual_availability" );
		save_cf( CF_degradation, nyears,"annual_degradation" );

	}

	bool compute_output(int nyears)
	{
		ssc_number_t *hourly_enet; // hourly energy output
	
		size_t count;

	// hourly energy
		hourly_enet = as_array("system_hourly_energy", &count );
		
		if ( (int)count != (8760))
		{
			std::stringstream outm;
			outm <<  "Bad hourly energy output length (" << count << "), should be 8760.";
			log( outm.str() );
			return false;
		}

		ssc_number_t *monthly_energy_to_grid = allocate( "monthly_energy", 12 );
		ssc_number_t *hourly_energy_to_grid = allocate( "hourly_energy", 8760 );


		double first_year_energy = 0.0;
		int i=0;
		size_t nrows, ncols;
		ssc_number_t *diurnal_curtailment = as_matrix( "energy_curtailment", &nrows, &ncols );
		if ( nrows != 12 || ncols != 24 )
		{
			std::ostringstream stream_error;
			stream_error << "month x hour curtailment factors must have 12 rows and 24 columns, input has " << nrows << " rows and " << ncols << " columns." ;
			std::string const str_error = stream_error.str();
			throw exec_error("annualoutput", str_error);
		}

		for (int m=0;m<12;m++)
			for (size_t d=0;d<util::nday[m];d++)
				for (int h=0;h<24;h++)
					if (i<8760)
					{
						first_year_energy += diurnal_curtailment[m*ncols+h]*hourly_enet[i];
						// first year availability applied
						hourly_energy_to_grid[i] = (ssc_number_t)(diurnal_curtailment[m*ncols + h] * hourly_enet[i] * cf.at(CF_availability, 1) * cf.at(CF_degradation, 1));
						monthly_energy_to_grid[m] += hourly_energy_to_grid[i];
						i++;
					}


		for (int y=1;y<=nyears;y++)
		{
			cf.at(CF_energy_net,y) = first_year_energy * cf.at(CF_availability,y) * cf.at(CF_degradation,y);
		}

		return true;

	}

	bool compute_lifetime_output(int nyears)
	{
		ssc_number_t *hourly_enet; // hourly energy output

		size_t count;

	// hourly energy
		hourly_enet = as_array("system_hourly_energy", &count );
		if ( (int)count != (8760*nyears))
		{
			std::stringstream outm;
			outm <<  "Bad hourly lifetime energy output length (" << count << "), should be (analysis period-1) * 8760 value (" << 8760*nyears << ")";
			log( outm.str() );
			return false;
		}

		size_t nrows, ncols;
		ssc_number_t *diurnal_curtailment = as_matrix( "energy_curtailment", &nrows, &ncols );
		if ( nrows != 12 || ncols != 24 )
			throw exec_error("annualoutput", "month x hour curtailment factors must have 12 rows and 24 columns");

		// all years
		ssc_number_t *monthly_energy_to_grid = allocate( "monthly_energy", 12*nyears );
		ssc_number_t *hourly_energy_to_grid = allocate( "hourly_energy", 8760*nyears );

		for (int y=1;y<=nyears;y++)
		{
			cf.at(CF_energy_net,y)=0;
			int i=0;
			for (int m=0;m<12;m++)
			{
				monthly_energy_to_grid[(y-1)*12+m] = 0;
				for (size_t d=0;d<util::nday[m];d++)
				{
					for (int h=0;h<24;h++)
					{
						if (i<8760)
						{
							hourly_energy_to_grid[(y - 1) * 8760 + i] = (ssc_number_t)(diurnal_curtailment[m*ncols + h] * hourly_enet[(y - 1) * 8760 + i] * cf.at(CF_availability, y) * cf.at(CF_degradation, y));
							monthly_energy_to_grid[(y-1)*12+m] += hourly_energy_to_grid[(y-1)*8760+i];
							cf.at(CF_energy_net,y) += hourly_energy_to_grid[(y-1)*8760+i];
							i++;
						}
					}
				}
			}


		}

	
		return true;
	}

	void save_cf(int cf_line, int nyears, const std::string &name)
	{
		ssc_number_t *arrp = allocate( name, nyears+1 );
		for (int i=0;i<=nyears;i++)
			arrp[i] = (ssc_number_t)cf.at(cf_line, i);
	}


};




DEFINE_MODULE_ENTRY( annualoutput, "Annual Output_", 1 );


