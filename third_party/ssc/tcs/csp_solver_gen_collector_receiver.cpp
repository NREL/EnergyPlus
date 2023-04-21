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

#include "csp_solver_gen_collector_receiver.h"
#include "csp_solver_core.h"
#include "csp_solver_util.h"

#include "sam_csp_util.h"
#include "lib_util.h"
#include <string>
#include "interpolation_routines.h"
#include <algorithm>

static const double zen_scale = 1.570781477;
static const double az_scale = 6.283125908;

static C_csp_reported_outputs::S_output_info S_output_info[] = 
{
	{ C_csp_gen_collector_receiver::E_Q_DOT_FIELD_INC, C_csp_reported_outputs::TS_WEIGHTED_AVE},
	{C_csp_gen_collector_receiver::E_ETA_FIELD, C_csp_reported_outputs::TS_WEIGHTED_AVE},
	{C_csp_gen_collector_receiver::E_Q_DOT_REC_INC, C_csp_reported_outputs::TS_WEIGHTED_AVE},
	{C_csp_gen_collector_receiver::E_ETA_THERMAL, C_csp_reported_outputs::TS_WEIGHTED_AVE},
	{C_csp_gen_collector_receiver::E_F_SFHL_QDNI, C_csp_reported_outputs::TS_WEIGHTED_AVE},
	{C_csp_gen_collector_receiver::E_F_SFHL_QWSPD, C_csp_reported_outputs::TS_WEIGHTED_AVE},
	{C_csp_gen_collector_receiver::E_F_SFHL_QTDRY, C_csp_reported_outputs::TS_WEIGHTED_AVE},

    csp_info_invalid
};

C_csp_gen_collector_receiver::C_csp_gen_collector_receiver()
{
	// *************************************************************
	// Define temperature and cp values so code interfaces with solver
	// *************************************************************
	m_T_htf_cold_fixed = 300.0+273.15;		//[K]
	m_T_htf_hot_fixed = 500.0+273.15;		//[K]
	m_cp_htf_fixed = 2.0;					//[kJ/kg-K]
	// *************************************************************
	
	mpc_optical_table_uns = 0;		// NULL

	m_eff_scale = m_A_sf_calc = std::numeric_limits<double>::quiet_NaN();
	
	m_mode = -1;
	m_mode_prev = -1;

	mc_reported_outputs.construct(S_output_info);
}

C_csp_gen_collector_receiver::~C_csp_gen_collector_receiver()
{
	/* Clean up on simulation terminate */
	if( mpc_optical_table_uns != 0 )
	{
		delete mpc_optical_table_uns;
	}
}

void C_csp_gen_collector_receiver::check_double_params_are_set()
{
	if( !check_double(ms_params.m_latitude) )
	{
		throw(C_csp_exception("The following parameter was not set prior to calling a C_csp_gen_collector_receiver method:","m_latitude"));
	}
	if( !check_double(ms_params.m_longitude) )
	{
		throw(C_csp_exception("The following parameter was not set prior to calling a C_csp_gen_collector_receiver method:", "m_longitude"));
	}
	if( !check_double(ms_params.m_theta_stow) )
	{
		throw(C_csp_exception("The following parameter was not set prior to calling a C_csp_gen_collector_receiver method:", "m_theta_stow"));
	}
	if( !check_double(ms_params.m_theta_dep) )
	{
		throw(C_csp_exception("The following parameter was not set prior to calling a C_csp_gen_collector_receiver method:", "m_theta_dep"));
	}
	if( !check_double(ms_params.m_solarm) )
	{
		throw(C_csp_exception("The following parameter was not set prior to calling a C_csp_gen_collector_receiver method:", "m_solarm"));
	}
	if( !check_double(ms_params.m_T_sfdes) )
	{
		throw(C_csp_exception("The following parameter was not set prior to calling a C_csp_gen_collector_receiver method:", "m_T_sfdes"));
	}
	if( !check_double(ms_params.m_irr_des) )
	{
		throw(C_csp_exception("The following parameter was not set prior to calling a C_csp_gen_collector_receiver method:", "m_irr_des"));
	}
	if( !check_double(ms_params.m_eta_opt_soil) )
	{
		throw(C_csp_exception("The following parameter was not set prior to calling a C_csp_gen_collector_receiver method:", "m_eta_opt_soil"));
	}
	if( !check_double(ms_params.m_eta_opt_gen) )
	{
		throw(C_csp_exception("The following parameter was not set prior to calling a C_csp_gen_collector_receiver method:", "m_eta_opt_gen"));
	}
	if( !check_double(ms_params.m_f_sfhl_ref) )
	{
		throw(C_csp_exception("The following parameter was not set prior to calling a C_csp_gen_collector_receiver method:", "m_f_sfhl_ref"));
	}
	if( !check_double(ms_params.m_qsf_des) )
	{
		throw(C_csp_exception("The following parameter was not set prior to calling a C_csp_gen_collector_receiver method:", "m_qsf_des"));
	}
}

void C_csp_gen_collector_receiver::init(const C_csp_collector_receiver::S_csp_cr_init_inputs init_inputs, 
			C_csp_collector_receiver::S_csp_cr_solved_params & solved_params)
{
	// Check that ms_params are set
	check_double_params_are_set();

	// Could sanity-check other parameters here...
	if(ms_params.m_interp_arr < 1 || ms_params.m_interp_arr > 2)
	{
		std::string msg = util::format("The interpolation code must be 1 (interpolate) or 2 (nearest neighbor)"
			"The input value was %d, so it was reset to 1", ms_params.m_interp_arr);
		mc_csp_messages.add_notice(msg);
		ms_params.m_interp_arr = 1;
	}
	if(ms_params.m_rad_type < 1 || ms_params.m_rad_type > 3)
	{	// Fairly important to know the intent of this input, so throw an exception if it's not one of the three options
		std::string msg = util::format("The solar resource radiation type must be 1 (DNI), 2 (Beam horizontal), or "
			"3 (Total horizontal). The input value was %d.");
		throw(C_csp_exception("C_csp_gen_collector_receiver::init",msg));
	}

	if(ms_params.mv_sfhlQ_coefs.size() < 1)
	{
		throw(C_csp_exception("C_csp_gen_collector_receiver::init","The model requires at least one irradiation-based "
			"thermal loss adjustment coefficient (mv_sfhlQ_coefs)"));
	}
	if(ms_params.mv_sfhlT_coefs.size() < 1)
	{
		throw(C_csp_exception("C_csp_gen_collector_receiver::init", "The model requires at least one temperature-based "
			"thermal loss adjustment coefficient (mv_sfhlT_coefs)"));
	}
	if( ms_params.mv_sfhlV_coefs.size() < 1 )
	{
		throw(C_csp_exception("C_csp_gen_collector_receiver::init", "The model requires at least one wind-based "
			"thermal loss adjustment coefficient (mv_sfhlV_coefs)"));
	}

	// Unit conversions
	ms_params.m_latitude *= CSP::pi/180.0;		//[rad], convert from deg
	ms_params.m_longitude *= CSP::pi / 180.0;	//[rad], convert from deg
	ms_params.m_theta_stow *= CSP::pi / 180.0;	//[rad], convert from deg
	ms_params.m_theta_dep *= CSP::pi / 180.0;	//[rad], convert from deg
	ms_params.m_T_sfdes += 273.15;	//[K], convert from C

	if( !ms_params.m_is_table_unsorted )
	{
		/*
		Standard azimuth-elevation table
		*/

		//does the table look right?
		if( (ms_params.m_optical_table.nrows() < 5 && ms_params.m_optical_table.ncols() > 3) || 
			(ms_params.m_optical_table.ncols() == 3 && ms_params.m_optical_table.nrows() > 4) )
		{
			mc_csp_messages.add_message(C_csp_messages::WARNING, "The optical efficiency table option flag may not match the specified table format. If running SSC, ensure \"IsTableUnsorted\""
			" =0 if regularly-spaced azimuth-zenith matrix is used and =1 if azimuth,zenith,efficiency points are specified.");
		}



		if( ms_params.m_optical_table.nrows() <= 0 || ms_params.m_optical_table.ncols() <= 0 ) // If these were not set correctly, it will create memory allocation crash not caught by error handling.
		{
			throw(C_csp_exception("C_csp_gen_collector_receiver::init","The optical table must have a positive number of rows and columns"));
		}

		double *xax = new double[ms_params.m_optical_table.ncols() - 1];
		double *yax = new double[ms_params.m_optical_table.nrows() - 1];
		double *data = new double[(ms_params.m_optical_table.ncols() - 1) * (ms_params.m_optical_table.nrows() - 1)];

		//get the xaxis data values
		for( size_t i = 1; i<ms_params.m_optical_table.ncols(); i++ ){
			xax[i - 1] = ms_params.m_optical_table(0,i)*CSP::pi/180.0;
		}
		//get the yaxis data values
		for( size_t j = 1; j<ms_params.m_optical_table.nrows(); j++ ){
			yax[j - 1] = ms_params.m_optical_table(j,0)*CSP::pi / 180.0;
		}
		//Get the data values
		for( size_t j = 1; j<ms_params.m_optical_table.nrows(); j++ ){
			for( size_t i = 1; i<ms_params.m_optical_table.ncols(); i++ ){
				data[i - 1 + (ms_params.m_optical_table.ncols() - 1)*(j - 1)] = ms_params.m_optical_table(j, i);
			}
		}

		mc_optical_table.AddXAxis(xax, (int)ms_params.m_optical_table.ncols() - 1);
		mc_optical_table.AddYAxis(yax, (int)ms_params.m_optical_table.nrows() - 1);
		mc_optical_table.AddData(data);
		delete [] xax;
		delete [] yax;
		delete [] data;
	}
	else
	{
		/*
		Use the unstructured data table
		*/

		/*
		------------------------------------------------------------------------------
		Create the regression fit on the efficiency map
		------------------------------------------------------------------------------
		*/

		if( ms_params.m_optical_table.ncols() != 3 )
		{
			std::string msg = util::format("The heliostat field efficiency file is not formatted correctly. Type expects 3 columns"
				" (zenith angle, azimuth angle, efficiency value) and instead has %d cols.", ms_params.m_optical_table.ncols());
			throw(C_csp_exception("C_csp_gen_collector_receiver::init", msg));
		}

		MatDoub sunpos;
		vector<double> effs;

		int nrows = (int)ms_params.m_optical_table.nrows();

		//read the data from the array into the local storage arrays
		sunpos.resize(nrows, VectDoub(2));
		effs.resize(nrows);
		double eff_maxval = -9.e9;
		for( int i = 0; i<nrows; i++ )
		{
			sunpos.at(i).at(0) = ms_params.m_optical_table(i,0) / az_scale * CSP::pi / 180.;
			sunpos.at(i).at(1) = ms_params.m_optical_table(i,1) / zen_scale * CSP::pi / 180.;
			double eff = ms_params.m_optical_table(i,2);

			effs.at(i) = eff;
			if( eff > eff_maxval ) eff_maxval = eff;
		}

		//scale values based on maximum. This helps the GM interpolation routine
		m_eff_scale = eff_maxval;
		for( int i = 0; i<nrows; i++ )
			effs.at(i) /= m_eff_scale;

		//Create the field efficiency table
		Powvargram vgram(sunpos, effs, 1.99, 0.);
		mpc_optical_table_uns = new GaussMarkov(sunpos, effs, vgram);

		//test how well the fit matches the data
		double err_fit = 0.;
		int npoints = (int)sunpos.size();
		for( int i = 0; i<npoints; i++ ){
			double zref = effs.at(i);
			double zfit = mpc_optical_table_uns->interp(sunpos.at(i));
			double dz = zref - zfit;
			err_fit += dz * dz;
		}
		err_fit = sqrt(err_fit);
		if( err_fit > 0.01 )
		{
			std::string msg = util::format("The heliostat field interpolation function fit is poor! (err_fit=%f RMS)", err_fit);
			mc_csp_messages.add_message(C_csp_messages::WARNING, msg);
		}

	}	// end unstructured data table

	init_sf();

	m_mode = C_csp_collector_receiver::OFF;					//[-] 0 = requires startup, 1 = starting up, 2 = running
	m_mode_prev = m_mode;
	
	return;
}

void C_csp_gen_collector_receiver::init_sf()
{
	// In TCS, the latitude, longitude, and timezone were passed during the call() method, 
	//     so had to finish initialization with this method.
	
	//---Design point values---  
	//Calculate the design point efficiency based on the solar position at solstice noon, rather than maxval of the table
	double omega = 0.0; //solar noon
	double dec = 23.45*CSP::pi/180.0;		//[rad] declination at summer solstice
	//Solar altitude at noon on the summer solstice
	double solalt = asin(sin(dec)*sin(ms_params.m_latitude) + cos(ms_params.m_latitude)*cos(dec)*cos(omega));
	double opt_des = std::numeric_limits<double>::quiet_NaN();
	if( ms_params.m_is_table_unsorted )
	{
		// Use current solar position to interpolate field efficiency table and find solar field efficiency
		vector<double> sunpos;
		sunpos.push_back(0.);
		sunpos.push_back((CSP::pi / 2. - solalt) / zen_scale);

		opt_des = mpc_optical_table_uns->interp(sunpos) * m_eff_scale;
	}
	else
	{
		opt_des = ms_params.m_interp_arr == 1 ?
			mc_optical_table.interpolate(0.0, max(CSP::pi / 2. - solalt, 0.0)) :
			mc_optical_table.nearest(0.0, max(CSP::pi / 2. - solalt, 0.0));
	}

	double eta_opt_ref = ms_params.m_eta_opt_soil*ms_params.m_eta_opt_gen*opt_des;

    // Calculate solar field area required to achieve 'delivered' solar field power using design optical and thermal losses
    m_A_sf_calc = (ms_params.m_qsf_des*(1.+ms_params.m_f_sfhl_ref))/(ms_params.m_irr_des*eta_opt_ref)*1.E6;    //[m2]

	return;
}

int C_csp_gen_collector_receiver::get_operating_state()
{
	throw(C_csp_exception("C_csp_gen_collector_receiver::get_operating_state() is not complete"));

	return -1;
}

double C_csp_gen_collector_receiver::get_startup_time()
{
	throw(C_csp_exception("C_csp_gen_collector_receiver::get_startup_time() is not complete"));

	return std::numeric_limits<double>::quiet_NaN();
}

double C_csp_gen_collector_receiver::get_startup_energy()
{
	throw(C_csp_exception("C_csp_gen_collector_receiver::get_startup_energy() is not complete"));

	return std::numeric_limits<double>::quiet_NaN();
}

double C_csp_gen_collector_receiver::get_pumping_parasitic_coef()
{
	throw(C_csp_exception("C_csp_gen_collector_receiver::get_pumping_parasitic_coef() is not complete"));

	return std::numeric_limits<double>::quiet_NaN();
}

double C_csp_gen_collector_receiver::get_min_power_delivery()
{
	throw(C_csp_exception("C_csp_gen_collector_receiver::get_min_power_delivery() is not complete"));

	return std::numeric_limits<double>::quiet_NaN();
}

double C_csp_gen_collector_receiver::get_tracking_power()
{
	throw(C_csp_exception("C_csp_gen_collector_receiver::get_tracking_power() is not complete"));
	return std::numeric_limits<double>::quiet_NaN(); //MWe
}

double C_csp_gen_collector_receiver::get_col_startup_power()
{
	throw(C_csp_exception("C_csp_gen_collector_receiver::get_col_startup_power() is not complete"));
	return std::numeric_limits<double>::quiet_NaN(); //MWe-hr
}

void C_csp_gen_collector_receiver::on(const C_csp_weatherreader::S_outputs &weather,
	const C_csp_solver_htf_1state &htf_state_in,
	double field_control,
	C_csp_collector_receiver::S_csp_cr_out_solver &cr_out_solver,
	//C_csp_collector_receiver::S_csp_cr_out_report &cr_out_report,
	const C_csp_solver_sim_info &sim_info)
{
	throw(C_csp_exception("C_csp_gen_collector_receiver::on(...) is not complete"));
}

void C_csp_gen_collector_receiver::call(const C_csp_weatherreader::S_outputs &weather,
	const C_csp_solver_htf_1state &htf_state_in,
	const C_csp_collector_receiver::S_csp_cr_inputs &inputs,
	C_csp_collector_receiver::S_csp_cr_out_solver &cr_out_solver,
	//C_csp_collector_receiver::S_csp_cr_out_report &cr_out_report,
	const C_csp_solver_sim_info &sim_info)
{
	double ibn = weather.m_beam;		//[W/m2] DNI
	double ibh = weather.m_hor_beam;	//[W/m2] Beam-horizontal irradiance
	double itoth = weather.m_global;	//[W/m2] Total horizontal irradiance
	double tdb = weather.m_tdry+273.15;	//[K] Ambient dry-bulb temperature, convert from C
	double vwind = weather.m_wspd;		//[m/s] Wind speed

	double shift = weather.m_shift*CSP::pi/180.0;	//[rad]	

	//Choose which irradiation source will be used
	double irr_used = std::numeric_limits<double>::quiet_NaN();
	switch( ms_params.m_rad_type )
	{
	case 1:
		irr_used = ibn;		//[W/m2]
		break;
	case 2:
		irr_used = ibh;		//[W/m2]
		break;
	case 3:
		irr_used = itoth;	//[W/m2]
		break;
	}

	double hour_of_day = fmod(sim_info.ms_ts.m_time / 3600.0, 24.0);     //[hr] hour_of_day of the day (1..24)
	double day_of_year = ceil(sim_info.ms_ts.m_time / 3600.0 / 24.0);		 //[-] Day of the year
	// Duffie & Beckman 1.5.3b
	double B = (day_of_year - 1)*360.0 / 365.0*CSP::pi / 180.0;
	// Eqn of time in minutes
	double EOT = 229.2 * (0.000075 + 0.001868 * cos(B) - 0.032077 * sin(B) - 0.014615 * cos(B*2.0) - 0.04089 * sin(B*2.0));
	// Declination in radians (Duffie & Beckman 1.6.1)
	double dec = 23.45 * sin(360.0*(284.0 + day_of_year) / 365.0*CSP::pi / 180.0) * CSP::pi / 180.0;
	// Solar Noon and time in hours
	double SolarNoon = 12. - ((shift)*180.0 / CSP::pi) / 15.0 - EOT / 60.0;

	// Deploy & stow times in hours
	// Calculations modified by MJW 11/13/2009 to correct bug
	double theta_dep = max(ms_params.m_theta_dep, 1.e-6);
	double DepHr1 = cos(ms_params.m_latitude) / tan(theta_dep);
	double DepHr2 = -tan(dec) * sin(ms_params.m_latitude) / tan(theta_dep);
	double DepHr3 = (tan(CSP::pi - theta_dep) < 0. ? -1. : 1.)*acos((DepHr1*DepHr2 + sqrt(DepHr1*DepHr1 - DepHr2*DepHr2 + 1.0)) / (DepHr1 * DepHr1 + 1.0)) * 180.0 / CSP::pi / 15.0;
	double DepTime = SolarNoon + DepHr3;

	double theta_stow = max(ms_params.m_theta_stow, 1.e-6);
	double StwHr1 = cos(ms_params.m_latitude) / tan(theta_stow);
	double StwHr2 = -tan(dec) * sin(ms_params.m_latitude) / tan(theta_stow);
	double StwHr3 = (tan(CSP::pi - theta_stow) < 0. ? -1. : 1.)*acos((StwHr1*StwHr2 + sqrt(StwHr1*StwHr1 - StwHr2*StwHr2 + 1.0)) / (StwHr1 * StwHr1 + 1.0)) * 180.0 / CSP::pi / 15.0;
	double StwTime = SolarNoon + StwHr3;

	// Ftrack is the fraction of the time period that the field is tracking. MidTrack is time at midpoint of operation
	double HrA = hour_of_day - sim_info.ms_ts.m_step / 3600.0;		//[hr]
	double HrB = hour_of_day;

	// Solar field operates
	double Ftrack, MidTrack;
	if( (HrB > DepTime) && (HrA < StwTime) )
	{
		// solar field deploys during time period
		if( HrA < DepTime )
		{
			Ftrack = (HrB - DepTime) *sim_info.ms_ts.m_step / 3600.0;
			MidTrack = HrB - Ftrack * 0.5 *sim_info.ms_ts.m_step / 3600.0;
			// Solar field stows during time period
		}
		else if( HrB > StwTime )
		{
			Ftrack = (StwTime - HrA) *sim_info.ms_ts.m_step / 3600.0;
			MidTrack = HrA + Ftrack * 0.5 *sim_info.ms_ts.m_step / 3600.0;
		}
		// solar field operates during entire period
		else
		{
			Ftrack = 1.0;
			MidTrack = HrA + 0.5 *sim_info.ms_ts.m_step / 3600.0;
		}
	}
	// solar field doesn't operate
	else
	{
		Ftrack = 0.0;
		MidTrack = HrA + 0.5 *sim_info.ms_ts.m_step / 3600.0;
	}

	double StdTime = MidTrack;
	double soltime = StdTime + ((shift)*180.0 / CSP::pi) / 15.0 + EOT / 60.0;
	// hour_of_day angle (arc of sun) in radians
	double omega = (soltime - 12.0)*15.0*CSP::pi / 180.0;
	// B. Stine equation for Solar Altitude angle in radians
	double solalt = asin(sin(dec)*sin(ms_params.m_latitude) + cos(ms_params.m_latitude)*cos(dec)*cos(omega));
	double solaz = (omega < 0. ? -1. : 1.)*fabs(acos(min(1.0, (cos(CSP::pi / 2. - solalt)*sin(ms_params.m_latitude) - sin(dec)) / (sin(CSP::pi / 2. - solalt)*cos(ms_params.m_latitude)))));

	//Get the current optical efficiency
	double opt_val;
	if( ms_params.m_is_table_unsorted )
	{
		// Use current solar position to interpolate field efficiency table and find solar field efficiency
		vector<double> sunpos;
		sunpos.push_back(solaz / az_scale);
		sunpos.push_back((CSP::pi / 2. - solalt) / zen_scale);

		opt_val = mpc_optical_table_uns->interp(sunpos) * m_eff_scale;
	}
	else
	{
		opt_val = ms_params.m_interp_arr == 1 ?
			mc_optical_table.interpolate(solaz, max(CSP::pi / 2. - solalt, 0.0)) :
			mc_optical_table.nearest(solaz, max(CSP::pi / 2. - solalt, 0.0));
	}

	double eta_arr = max(opt_val*Ftrack, 0.0);  //mjw 7.25.11 limit zenith to <90, otherwise the interpolation error message gets called during night hours.
	double eta_opt_sf = eta_arr*ms_params.m_eta_opt_soil*ms_params.m_eta_opt_gen*inputs.m_adjust;

	//Evaluate solar field thermal efficiency derate
	double f_sfhl_qdni = 0.0;
	double f_sfhl_tamb = 0.0;
	double f_sfhl_vwind = 0.0;
	for( size_t i = 0; i<ms_params.mv_sfhlQ_coefs.size(); i++ )
		f_sfhl_qdni += ms_params.mv_sfhlQ_coefs[i] * pow(irr_used / ms_params.m_irr_des, i);
	for( size_t i = 0; i<ms_params.mv_sfhlT_coefs.size(); i++ )
		f_sfhl_tamb += ms_params.mv_sfhlT_coefs[i] * pow(tdb - ms_params.m_T_sfdes, i);
	for( size_t i = 0; i<ms_params.mv_sfhlV_coefs.size(); i++ )
		f_sfhl_vwind += ms_params.mv_sfhlV_coefs[i] * pow(vwind, i);

	double f_sfhl = 1.0 - ms_params.m_f_sfhl_ref * (f_sfhl_qdni + f_sfhl_tamb + f_sfhl_vwind);  //sf thermal efficiency
    double q_hl_sf = (1. - f_sfhl) * ms_params.m_qsf_des;       //[MWt]

	//Calculate the total solar field thermal output
    double q_dot_field_inc = m_A_sf_calc * irr_used * 1.E-6;    //[MWt]
    double q_dot_rec_inc = q_dot_field_inc * eta_opt_sf;    //[MWt]
    double q_sf = q_dot_rec_inc - q_hl_sf;  //[MWt]
    if( q_sf < 0. )
        q_sf = 0.;

	// Set collector-receiver class outputs required by solver
	cr_out_solver.m_q_startup = 0.0;
	cr_out_solver.m_time_required_su = 0.0;
	cr_out_solver.m_m_dot_salt_tot = q_sf*100.0 / (m_cp_htf_fixed*(m_T_htf_hot_fixed - m_T_htf_cold_fixed))*3600.0;	//[kg/hr]
	cr_out_solver.m_q_thermal = q_sf;							//[MWt]
	cr_out_solver.m_T_salt_hot = m_T_htf_hot_fixed-273.15;		//[C]
	
	cr_out_solver.m_W_dot_col_tracking = 0.0;		//[MWe]
	cr_out_solver.m_W_dot_htf_pump = 0.0;			//[MWe]

    mc_reported_outputs.value(E_Q_DOT_FIELD_INC, q_dot_field_inc);  //[MWt]
	mc_reported_outputs.value(E_ETA_FIELD, eta_opt_sf);		//[-]
	mc_reported_outputs.value(E_Q_DOT_REC_INC, q_dot_rec_inc);	//[-]
	mc_reported_outputs.value(E_ETA_THERMAL, q_sf / q_dot_rec_inc);		//[-]
	mc_reported_outputs.value(E_F_SFHL_QDNI, f_sfhl_qdni);	//[-]
	mc_reported_outputs.value(E_F_SFHL_QWSPD, f_sfhl_vwind);	//[-]
	mc_reported_outputs.value(E_F_SFHL_QTDRY, f_sfhl_tamb);	//[-]
}

void C_csp_gen_collector_receiver::startup(const C_csp_weatherreader::S_outputs &weather,
	const C_csp_solver_htf_1state &htf_state_in,
	C_csp_collector_receiver::S_csp_cr_out_solver &cr_out_solver,
	//C_csp_collector_receiver::S_csp_cr_out_report &cr_out_report,
	const C_csp_solver_sim_info &sim_info)
{
	throw(C_csp_exception("C_csp_gen_collector_receiver::startup(...) is not complete"));
	
	return;
}

void C_csp_gen_collector_receiver::estimates(const C_csp_weatherreader::S_outputs &weather,
	const C_csp_solver_htf_1state &htf_state_in,
	C_csp_collector_receiver::S_csp_cr_est_out &est_out,
	const C_csp_solver_sim_info &sim_info)
{
	throw(C_csp_exception("C_csp_gen_collector_receiver::estimates(...) is not complete"));

	return;
}

void C_csp_gen_collector_receiver::off(const C_csp_weatherreader::S_outputs &weather,
	const C_csp_solver_htf_1state &htf_state_in,
	C_csp_collector_receiver::S_csp_cr_out_solver &cr_out_solver,
	//C_csp_collector_receiver::S_csp_cr_out_report &cr_out_report,
	const C_csp_solver_sim_info &sim_info)
{
	m_mode = C_csp_collector_receiver::OFF;

	// All outputs should be zero, as model is assuming no recirculation or freeze protection parasitics
	cr_out_solver.m_q_startup = 0.0;
	cr_out_solver.m_time_required_su = 0.0;
	cr_out_solver.m_m_dot_salt_tot = 0.0;		//[kg/hr]
	cr_out_solver.m_q_thermal = 0.0;			//[MWt]
	cr_out_solver.m_T_salt_hot = 0.0;			//[C]
	cr_out_solver.m_W_dot_col_tracking = 0.0;	//[MWe]
	cr_out_solver.m_W_dot_htf_pump = 0.0;		//[MWe]
	cr_out_solver.m_component_defocus = 1.0;	//[-]

	mc_reported_outputs.value(E_Q_DOT_FIELD_INC, 0.0);	//[MWt]
	mc_reported_outputs.value(E_ETA_FIELD, 0.0);		//[-]
	mc_reported_outputs.value(E_Q_DOT_REC_INC, 0.0);	//[-]
	mc_reported_outputs.value(E_ETA_THERMAL, 0.0);		//[-]
	mc_reported_outputs.value(E_F_SFHL_QDNI, 0.0);		//[-]
	mc_reported_outputs.value(E_F_SFHL_QWSPD, 0.0);		//[-]
	mc_reported_outputs.value(E_F_SFHL_QTDRY, 0.0);		//[-]


	//cr_out_report.m_q_dot_field_inc = 0.0;		//[MWt]
	//cr_out_report.m_eta_field = 0.0;			//[-]
	//cr_out_report.m_q_dot_rec_inc = 0.0;		//[MWt]
	//cr_out_report.m_eta_thermal = 0.0;			//[-]
	//cr_out_report.m_q_dot_piping_loss = 0.0;	//[MWt]

	return;
}

void C_csp_gen_collector_receiver::converged()
{
	m_mode_prev = m_mode;
	
	return;
}

void C_csp_gen_collector_receiver::write_output_intervals(double report_time_start,
	const std::vector<double> & v_temp_ts_time_end, double report_time_end)
{
	return;
}

double C_csp_gen_collector_receiver::calculate_optical_efficiency(const C_csp_weatherreader::S_outputs &weather, const C_csp_solver_sim_info &sim)
{
	throw(C_csp_exception("C_csp_gen_collector_receiver::calculate_optical_efficiency() is not complete"));

	return std::numeric_limits<double>::quiet_NaN();
}

double C_csp_gen_collector_receiver::calculate_thermal_efficiency_approx(const C_csp_weatherreader::S_outputs &weather, double q_incident /*MW*/)
{
	throw(C_csp_exception("C_csp_gen_collector_receiver::calculate_thermal_efficiency_approx() is not complete"));

	return std::numeric_limits<double>::quiet_NaN();
}

double C_csp_gen_collector_receiver::get_collector_area()
{
	throw(C_csp_exception("C_csp_gen_collector_receiver::get_collector_area() is not complete"));

	return std::numeric_limits<double>::quiet_NaN();
}
