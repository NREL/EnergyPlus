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

static var_info _cm_vtab_timeseq[] = 
{	
/*   VARTYPE           DATATYPE         NAME                         LABEL                              UNITS     META                      GROUP          REQUIRED_IF                 CONSTRAINTS                      UI_HINTS*/
	{ SSC_INPUT,        SSC_NUMBER,      "start_time",                 "Start time",                     "seconds", "0=jan1st 12am",         "Time Sequence", "*",                       "MIN=0,MAX=31536000",                     "" },
	{ SSC_INPUT,        SSC_NUMBER,      "end_time",                   "End time",                       "seconds", "0=jan1st 12am",         "Time Sequence", "*",                       "MIN=0,MAX=31536000",                     "" },
	{ SSC_INPUT,        SSC_NUMBER,      "time_step",                  "Time step",                      "seconds", "",                     "Time Sequence", "*",                       "MIN=1,MAX=3600",                         "" },

	{ SSC_OUTPUT,       SSC_ARRAY,       "time",                       "Time",                           "secs",   "0=jan1st 12am",        "Time",          "*",                       "",                                       "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "timehr",                     "HourTime",                       "hours",  "0=jan1st 12am",        "Time",          "*",                       "",                                       "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "month",                      "Month",                          "",       "1-12",                 "Time",          "*",                       "",                                       "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "day",                        "Day",                            "",       "1-{28,30,31}",         "Time",          "*",                       "",                                       "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "hour",                       "Hour",                           "",       "0-23",                 "Time",          "*",                       "",                                       "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "minute",                     "Minute",                         "",       "0-59",                 "Time",          "*",                       "",                                       "" },

var_info_invalid };

class cm_timeseq : public compute_module
{
private:
public:
	cm_timeseq()
	{
		add_var_info( _cm_vtab_timeseq );
	}

	void exec( )
	{
		double t_start = as_double("start_time");
		double t_end = as_double("end_time");
		double t_step = as_double("time_step"); // seconds

		size_t num_steps = check_timestep_seconds( t_start, t_end, t_step );

		ssc_number_t *time = allocate("time", num_steps);
		ssc_number_t *timehr = allocate("timehr", num_steps);
		ssc_number_t *month = allocate("month", num_steps);
		ssc_number_t *day = allocate("day", num_steps);
		ssc_number_t *hour = allocate("hour", num_steps);
		ssc_number_t *minute = allocate("minute", num_steps);

		double T = t_start;
		size_t idx = 0;
		while (T < t_end && idx < num_steps)
		{
			double Thr = T / 3600.0;

			time[idx] = (float) T;
			timehr[idx] = (float) Thr;
						
			int m = util::month_of(Thr);
			month[idx] = (ssc_number_t) m ;              // month goes 1-12
			day[idx] = (ssc_number_t) util::day_of_month(m,Thr) ;   // day goes 1-nday_in_month
			hour[idx] = (ssc_number_t) ((int)(Thr)%24);		         // hour goes 0-23
			minute[idx] = (ssc_number_t) ((int)( (Thr-floor(Thr))*60  + t_step/3600.0*30));      // minute goes 0-59
	
			T += t_step;
			idx++;
		}

	}
};

DEFINE_MODULE_ENTRY( timeseq, "Time sequence generator", 1 )
