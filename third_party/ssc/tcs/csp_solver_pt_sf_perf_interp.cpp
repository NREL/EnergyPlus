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

#include "csp_solver_pt_sf_perf_interp.h"
#include "sam_csp_util.h"
#include "csp_solver_core.h"

#include "interpolation_routines.h"
#include "AutoPilot_API.h"
#include "IOUtil.h"
#include "sort_method.h"
#include "Heliostat.h"

#include "lib_weatherfile.h"

#include <sstream>

#define az_scale 6.283125908 
#define zen_scale 1.570781477 
#define eff_scale 0.7

C_pt_sf_perf_interp::C_pt_sf_perf_interp()
{
	m_p_start = m_p_track = m_hel_stow_deploy = m_v_wind_max = std::numeric_limits<double>::quiet_NaN();

    // Initialize to field stowed - can overwrite after class is constructed if desired
    m_is_field_tracking = m_is_field_tracking_prev = false;

	m_n_flux_x = m_n_flux_y = -1;

	field_efficiency_table = 0;

	m_cdata = 0;		// = NULL
	mf_callback = 0;	// = NULL

	m_ncall = -1;
}

C_pt_sf_perf_interp::~C_pt_sf_perf_interp()
{
	if( field_efficiency_table != 0 )
		delete field_efficiency_table;
}

void C_pt_sf_perf_interp::init()
{
	//Read in parameters
	util::matrix_t<double> eta_map;
	util::matrix_t<double> flux_maps;
	
	m_p_start = ms_params.m_p_start;
	m_p_track = ms_params.m_p_track;
	m_hel_stow_deploy = ms_params.m_hel_stow_deploy*CSP::pi / 180.0;
	m_v_wind_max = ms_params.m_v_wind_max;

	eta_map = ms_params.m_eta_map;

	m_n_flux_x = ms_params.m_n_flux_x;
	m_n_flux_y = ms_params.m_n_flux_y;
		
	int nfluxpos = eta_map.nrows();
	int nfposdim = 2;

	flux_maps = ms_params.m_flux_maps;
	int nfluxmap = flux_maps.nrows();
	int nfluxcol = flux_maps.ncols();

	//check that flux maps match dimensions
	if( nfluxmap % nfluxpos != 0 )
	{
		error_msg = util::format("The number of flux maps provided does not match the number of flux map sun positions provided. Please "
			"ensure that the dimensionality of each flux map is consistent and that one sun position is provided for "
			"each flux map. (Sun pos. = %d, mismatch lines = %d)", nfluxpos, nfluxmap % nfluxpos);
		throw(C_csp_exception(error_msg, "heliostat field initialization"));
	}
	//copy the flux positions over to the local member
	m_map_sol_pos.resize(nfluxpos, VectDoub(nfposdim));
	for (int i = 0; i < nfluxpos; i++)
		for (int j = 0; j < nfposdim; j++)
			m_map_sol_pos.at(i).at(j) = eta_map(i, j) * CSP::pi / 180.0;

	MatDoub sunpos;
	vector<double> effs;
	vector<double> vis;

	if(! ms_params.m_eta_map_aod_format )
	{

		int nrows = ms_params.m_eta_map.nrows();
		int ncols = ms_params.m_eta_map.ncols();
		
		if(ncols != 3)
		{
			error_msg = util::format("The heliostat field efficiency file is not formatted correctly. Type expects 3 columns"
				" (zenith angle, azimuth angle, efficiency value) and instead has %d cols.", ncols);

			throw(C_csp_exception(error_msg, "heliostat field initialization"));
		}
		
		//read the data from the array into the local storage arrays
		sunpos.resize(nrows, VectDoub(2));
		effs.resize(nrows);
		for(int i=0; i<nrows; i++)
		{
			sunpos.at(i).at(0) = eta_map(i, 0) / az_scale * CSP::pi / 180.0;
			sunpos.at(i).at(1) = eta_map(i, 1) / zen_scale * CSP::pi / 180.0;
			effs.at(i) = eta_map(i, 2) / eff_scale;
		}
	}
	else
	{
		int nrows = ms_params.m_eta_map.nrows()-1;
		int ncols = ms_params.m_eta_map.ncols();
		int nvis = ncols-2;
                
		//read the data from the array into the local storage arrays
		sunpos.resize(nrows*nvis, VectDoub(3));
		effs.resize(nrows*nvis);
		        
		for(int j=0; j<nvis; j++)
		{
			double vis = eta_map(0, j+2);

			for(int i=0; i<nrows; i++)
			{
				sunpos.at(i+nrows*j).at(0) = eta_map(i+1, 0) / az_scale * CSP::pi / 180.0;
				sunpos.at(i+nrows*j).at(1) = eta_map(i+1, 1) / zen_scale * CSP::pi / 180.0;
				sunpos.at(i+nrows*j).at(2) = vis;
				effs.at(i+nrows*j) = eta_map(i+1, j+2) / eff_scale;
			}
		}

		//after processing, pop the first row 
		util::matrix_t<double> eta_temp(nrows, ncols);
		ms_params.m_eta_map.resize(nrows, ncols);
		for(int i=0; i<nrows; i++)
			for(int j=0; j<ncols; j++)
				ms_params.m_eta_map.at(i,j) = eta_map(i+1, j);
	}

	ms_outputs.m_flux_map_out.resize_fill(m_n_flux_y, m_n_flux_x, 0.0);

	/*
	------------------------------------------------------------------------------
	Create the regression fit on the efficiency map
	------------------------------------------------------------------------------
	*/

	double interp_nug = 0.0;
	double interp_beta = 1.99;

	//Create the field efficiency table
	Powvargram vgram(sunpos, effs, interp_beta, interp_nug);
	field_efficiency_table = new GaussMarkov(sunpos, effs, vgram);

	//test how well the fit matches the data
	double err_fit = 0.;
	int npoints = (int)sunpos.size();
	for( int i = 0; i<npoints; i++ ){
		double zref = effs.at(i);
		double zfit = field_efficiency_table->interp(sunpos.at(i));
		double dz = zref - zfit;
		err_fit += dz * dz;
	}
	err_fit = sqrt(err_fit);
	if( err_fit > 0.01 )
	{
		error_msg = util::format("The heliostat field interpolation function fit is poor! (err_fit=%f RMS)", err_fit);
		mc_csp_messages.add_message(C_csp_messages::WARNING, error_msg);
	}

	// Initialize stored variables
	//m_eta_prev = 0.0;
	//m_v_wind_prev = 0.0;

	m_ncall = -1;
}

void C_pt_sf_perf_interp::call(const C_csp_weatherreader::S_outputs &weather, double field_control_in, const C_csp_solver_sim_info &sim_info)
{
	// Increase call-per-timestep counter
	// Converge() sets it to -1, so on first call this line will adjust it = 0
	m_ncall++;
	
	// Get sim info
	double time = sim_info.ms_ts.m_time;
	double step = sim_info.ms_ts.m_step;

    double sf_adjust = 1.;
	if (ms_params.m_sf_adjust.ncells() >= 8760)
	{
		double full_step = 8760.*3600. / (double)ms_params.m_sf_adjust.ncells();	// full time step size (s)
		sf_adjust = ms_params.m_sf_adjust.at((int)(time / full_step) - 1);
	}

	double v_wind = weather.m_wspd;			//[m/s]
	double field_control = field_control_in;	// Control Parameter ( range from 0 to 1; 0=off, 1=all on)
	if( field_control_in > 1.0 )
		field_control = 1.0;
	if( field_control_in < 0.0 )
		field_control = 0.0;

	double solzen = weather.m_solzen*CSP::pi / 180.0;

    // Check stow/deploy angle, max wind speed, and input field control to determine if heliostats are tracking
    if (solzen > (CSP::pi / 2 - .001 - m_hel_stow_deploy) || v_wind > m_v_wind_max || field_control < 1.e-4) {
        m_is_field_tracking = false;
        field_control = 0.0;
    }
    else {
        m_is_field_tracking = true;
    }

	double solaz = weather.m_solazi*CSP::pi / 180.0;

	// clear out the existing flux map
	ms_outputs.m_flux_map_out.fill(0.0);

	// Parasitics for startup or shutdown
	double pparasi = 0.0;

    // If field is switching from either i) stowed to tracking or ii) tracking to stowed,
    //    then need to apply startup parasitic
    if ((m_is_field_tracking && !m_is_field_tracking_prev) ||
        (!m_is_field_tracking && m_is_field_tracking_prev)) {
        pparasi = ms_params.m_N_hel * m_p_start / (step / 3600.0);			// [kWe-hr]/[hr] = kWe 
    }

    // If field is tracking then need to apply tracking parasitic
    if (m_is_field_tracking) {
        pparasi += ms_params.m_N_hel * m_p_track * field_control;				// [kWe]
    }

	double eta_field = 0.;
    if(!m_is_field_tracking){
		eta_field = 1.e-6;
	}
	else
	{
		// Use current solar position to interpolate field efficiency table and find solar field efficiency
		vector<double> sunpos;
		sunpos.push_back(solaz / az_scale);
		sunpos.push_back(solzen / zen_scale);
        if( ms_params.m_eta_map_aod_format )
        {
            if( weather.m_aod != weather.m_aod )
                sunpos.push_back( 0. );
            else
                sunpos.push_back( weather.m_aod );
        }

		eta_field = field_efficiency_table->interp(sunpos) * eff_scale;
		eta_field = fmin(fmax(eta_field, 0.0), 1.0) * field_control * sf_adjust;		// Ensure physical behavior 

		//Set the active flux map
		VectDoub pos_now(sunpos);
		
        //find the nearest neighbors to the current point
		vector<double> distances;
		vector<int> indices;
		for (int i = 0; i<(int)m_map_sol_pos.size(); i++){
			distances.push_back(rdist(&pos_now, &m_map_sol_pos.at(i)));
			indices.push_back(i);
		}
		quicksort<double, int>(distances, indices);
		//calculate weights for the nearest 6 points
		double avepoints = 0.;
		const int npt = 6;
		for( int i = 0; i<npt; i++ )
			avepoints += distances.at(i);
		avepoints *= 1. / (double)npt;
		VectDoub weights(npt);
		double normalizer = 0.;
		for( int i = 0; i<npt; i++ ){
			double w = exp(-pow(distances.at(i) / avepoints, 2));
			weights.at(i) = w;
			normalizer += w;
		}
		for( int i = 0; i<npt; i++ )
			weights.at(i) *= 1. / normalizer;

		//set the values
		for( int k = 0; k<npt; k++ )
		{
			int imap = indices.at(k);
			for( int j = 0; j<m_n_flux_y; j++ )
			{
				for( int i = 0; i<m_n_flux_x; i++ )
				{
					ms_outputs.m_flux_map_out(j, i) += ms_params.m_flux_maps(imap*m_n_flux_y + j, i)*weights.at(k);
				}
			}
		}

	}

	ms_outputs.m_q_dot_field_inc = weather.m_beam*ms_params.m_A_sf*1.E-6;		//[MWt]

	ms_outputs.m_pparasi = pparasi / 1.E3;		//[MW], convert from kJ/hr: Parasitic power for tracking
	ms_outputs.m_eta_field = eta_field;			//[-], field efficiency
    ms_outputs.m_sf_adjust_out = sf_adjust;

}

void C_pt_sf_perf_interp::off(const C_csp_solver_sim_info &sim_info)
{
	// Increase call-per-timestep counter
	// Converge() sets it to -1, so on first call this line will adjust it = 0
	m_ncall++;

	// Get sim info
	double step = sim_info.ms_ts.m_step;

    m_is_field_tracking = false;
	// Calculate stow parasitics (if applicable)
	double pparasi = 0.0;

    // Is field shutting down, i.e. was it on the previous timestep
	if( m_is_field_tracking_prev ) {
		pparasi = ms_params.m_N_hel * m_p_start / (step / 3600.0);			// [kWe-hr]/[hr] = kWe 
	}

	ms_outputs.m_pparasi = pparasi / 1.E3;		//[MW], convert from kJ/hr: Parasitic power for tracking
	// Other outputs
		// clear out the existing flux map
	ms_outputs.m_flux_map_out.fill(0.0);
	ms_outputs.m_q_dot_field_inc = 0.0;		//[MWt]
	ms_outputs.m_eta_field = 0.0;			//[-], field efficiency

}

void C_pt_sf_perf_interp::converged()
{
    m_is_field_tracking_prev = m_is_field_tracking;     //[-]

	m_ncall = -1;
}

double C_pt_sf_perf_interp::rdist(VectDoub *p1, VectDoub *p2, int dim)
{
	double d = 0;
	for( int i = 0; i<dim; i++ ){
		double rd = p1->at(i) - p2->at(i);
		d += rd * rd;
	}
	return sqrt(d);
}
