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

#include "csp_solver_tou_block_schedules.h"
#include "csp_solver_util.h"
#include <algorithm>

void C_block_schedule::check_dimensions()
{
	// Check that each schedule is a 12x24 matrix
	// If not, throw exception
	
	if (mc_weekdays.nrows() != mc_weekends.nrows()
			|| mc_weekdays.nrows() != 12 
	    || mc_weekdays.ncols() != mc_weekends.ncols()
			|| mc_weekdays.ncols() != 24 ) 
	{
		m_error_msg = "TOU schedules must have 12 rows and 24 columns";
		throw C_csp_exception( m_error_msg, "TOU block schedule init" );
	}
/*
	if( mc_weekdays.nrows() != mstatic_n_rows )
	{
		m_error_msg = util::format("TOU schedules require 12 rows and 24 columns. The loaded weekday schedule has %d rows.", (int)mc_weekdays.nrows());
		throw(C_csp_exception(m_error_msg, "TOU block schedule initialization"));
	}

	if( mc_weekdays.ncols() != mstatic_n_cols )
	{
		m_error_msg = util::format("TOU schedules require 12 rows and 24 columns. The loaded weekday schedule has %d columns.", (int)mc_weekdays.ncols());
		throw(C_csp_exception(m_error_msg, "TOU block schedule initialization"));
	}

	if( mc_weekends.nrows() != mstatic_n_rows )
	{
		m_error_msg = util::format("TOU schedules require 12 rows and 24 columns. The loaded weekend schedule has %d rows.",(int) mc_weekends.nrows());
		throw(C_csp_exception(m_error_msg, "TOU block schedule initialization"));
	}

	if( mc_weekends.ncols() != mstatic_n_cols )
	{
		m_error_msg = util::format("TOU schedules require 12 rows and 24 columns. The loaded weekend schedule has %d columns.", (int)mc_weekends.ncols());
		throw(C_csp_exception(m_error_msg, "TOU block schedule initialization"));
	}
*/
	return;
}

void C_block_schedule::size_vv(int n_arrays)
{
	mvv_tou_arrays.resize(n_arrays, std::vector<double>(0, std::numeric_limits<double>::quiet_NaN()));
}

void C_block_schedule::check_arrays_for_tous(int n_arrays)
{

	// Check that all TOU periods represented in the schedules are available in the tou arrays
	
	int i_tou_min = 1;
	int i_tou_max = 1;
	int i_tou_day = -1;
	int i_tou_end = -1;
	int i_temp_max = -1;
	int i_temp_min = -1;

	for( int i = 0; i < 12; i++ )
	{
		for( int j = 0; j < 24; j++ )
		{
			i_tou_day = (int) mc_weekdays(i, j) - 1;
			i_tou_end = (int) mc_weekends(i, j) - 1;
			i_temp_max = std::max(i_tou_day, i_tou_end);
			i_temp_min = std::min(i_tou_day, i_tou_end);
			if( i_temp_max > i_tou_max )
				i_tou_max = i_temp_max;
			if( i_temp_min < i_tou_min )
				i_tou_min = i_temp_min;
		}
	}

	if( i_tou_min < 0 )
	{
		throw(C_csp_exception("Smallest TOU period cannot be less than 1", "TOU block schedule initialization"));
	}
	
	for( int k = 0; k < n_arrays; k++ )
	{
		
		if( i_tou_max + 1 > (int)mvv_tou_arrays[k].size() )
		{
			m_error_msg = util::format("TOU schedule contains TOU period = %d, while the %s array contains %d elements", (int)i_temp_max, mv_labels[k].c_str(), mvv_tou_arrays[k].size());
			throw(C_csp_exception(m_error_msg, "TOU block schedule initialization"));
		}

	}
}

void C_block_schedule::set_hr_tou(bool is_leapyear)
{
    /* 
    This method sets the TOU schedule month by hour for an entire year, so only makes sense in the context of an annual simulation.

    */
    if( m_hr_tou != 0 )
        delete [] m_hr_tou;

    int nhrann = 8760+(is_leapyear?24:0);

    m_hr_tou = new double[nhrann];

	int nday[12] = {31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31};
    if( is_leapyear )
        nday[1] ++;

	int wday = 5, i = 0;
	for( int m = 0; m<12; m++ )
	{
		for( int d = 0; d<nday[m]; d++ )
		{
			bool bWeekend = (wday <= 0);

			if( wday >= 0 ) wday--;
			else wday = 5;

			for( int h = 0; h<24 && i<nhrann && m * 24 + h<288; h++ )
			{
				if( bWeekend )
					m_hr_tou[i] = mc_weekends(m, h);	// weekends[m * 24 + h];
				else
					m_hr_tou[i] = mc_weekdays(m, h);	// weekdays[m * 24 + h];
				i++;
			}
		}
	}
}

void C_block_schedule::init(int n_arrays, bool is_leapyear)
{
	check_dimensions();

	check_arrays_for_tous(n_arrays);

	set_hr_tou(is_leapyear);
}

C_block_schedule_csp_ops::C_block_schedule_csp_ops()
{
	// Initializie temporary output 2D vector
	size_vv(N_END);

	mv_labels.resize(N_END);

	mv_labels[0] = "Turbine Fraction";
}

C_block_schedule_pricing::C_block_schedule_pricing()
{
	// Initializie temporary output 2D vector
	size_vv(N_END);

	mv_labels.resize(N_END);

	mv_labels[0] = "Price Multiplier";

	mv_is_diurnal = true;
}

void C_csp_tou_block_schedules::init()
{
	try
	{
		ms_params.mc_csp_ops.init(C_block_schedule_csp_ops::N_END, mc_dispatch_params.m_isleapyear);
	}
	catch( C_csp_exception &csp_exception )
	{
		m_error_msg = "The CSP ops " + csp_exception.m_error_message;
		throw(C_csp_exception(m_error_msg, "TOU block schedule initialization"));
	}

	// time step initialization of actual price multipliers done in calling compute modules.
	// mv_is_diurnal is set to true in constructor 
	if (ms_params.mc_pricing.mv_is_diurnal)
	{
		try
		{
			ms_params.mc_pricing.init(C_block_schedule_pricing::N_END, mc_dispatch_params.m_isleapyear);
		}
		catch (C_csp_exception &csp_exception)
		{
			m_error_msg = "The CSP pricing " + csp_exception.m_error_message;
			throw(C_csp_exception(m_error_msg, "TOU block schedule initialization"));
		}
	}
	return;
}

void C_csp_tou_block_schedules::call(double time_s, C_csp_tou::S_csp_tou_outputs & tou_outputs)
{
	int i_hour = (int)(ceil(time_s/3600.0 - 1.e-6) - 1);

	if( i_hour > 8760 - 1 + (mc_dispatch_params.m_isleapyear ? 24 : 0) || i_hour<0 ) 
	{
		m_error_msg = util::format("The hour input to the TOU schedule must be from 1 to 8760. The input hour was %d.", i_hour+1);
		throw(C_csp_exception(m_error_msg, "TOU timestep call"));
	}

	size_t csp_op_tou = (size_t)ms_params.mc_csp_ops.m_hr_tou[i_hour];

	tou_outputs.m_csp_op_tou = (int)csp_op_tou;
	
	tou_outputs.m_f_turbine = ms_params.mc_csp_ops.mvv_tou_arrays[C_block_schedule_csp_ops::TURB_FRAC][csp_op_tou-1];
	
	if (ms_params.mc_pricing.mv_is_diurnal)
	{
		int pricing_tou = (int)ms_params.mc_pricing.m_hr_tou[i_hour];
		tou_outputs.m_pricing_tou = pricing_tou;
		tou_outputs.m_price_mult = ms_params.mc_pricing.mvv_tou_arrays[C_block_schedule_pricing::MULT_PRICE][pricing_tou - 1];
	}
	else // note limited to hour but can be extended to timestep using size 
	{
		// these can be set in initialize and we may want to include time series inputs for other multipliers and fractions
		size_t nrecs = ms_params.mc_pricing.mvv_tou_arrays[C_block_schedule_pricing::MULT_PRICE].size();
		if (nrecs <= 0)
		{
			m_error_msg = util::format("The timestep price multiplier array was empty.");
			throw(C_csp_exception(m_error_msg, "TOU timestep call"));
		}
		size_t nrecs_per_hour = nrecs / 8760;
		int ndx = (int)((ceil(time_s / 3600.0 - 1.e-6) - 1) * nrecs_per_hour);

		if (ndx > (int)nrecs - 1 + (mc_dispatch_params.m_isleapyear ? 24 : 0) || ndx<0)
		{
			m_error_msg = util::format("The index input to the TOU schedule must be from 1 to %d. The input timestep index was %d.", (int)nrecs, ndx + 1);
			throw(C_csp_exception(m_error_msg, "TOU timestep call"));
		}
		tou_outputs.m_price_mult = ms_params.mc_pricing.mvv_tou_arrays[C_block_schedule_pricing::MULT_PRICE][ndx];
	}
}

void C_csp_tou_block_schedules::setup_block_uniform_tod()
{
	int nrows = ms_params.mc_csp_ops.mstatic_n_rows;
	int ncols = ms_params.mc_csp_ops.mstatic_n_cols;
	
	for( int i = 0; i < ms_params.mc_csp_ops.N_END; i++ )
		ms_params.mc_csp_ops.mvv_tou_arrays[i].resize(2, 1.0);

	for( int i = 0; i < ms_params.mc_pricing.N_END; i++ )
		ms_params.mc_pricing.mvv_tou_arrays[i].resize(2, 1.0);

	ms_params.mc_csp_ops.mc_weekdays.resize_fill(nrows, ncols, 1.0);
	ms_params.mc_csp_ops.mc_weekends.resize_fill(nrows, ncols, 1.0);

	ms_params.mc_pricing.mc_weekdays.resize_fill(nrows, ncols, 1.0);
	ms_params.mc_pricing.mc_weekends.resize_fill(nrows, ncols, 1.0);
	
}
