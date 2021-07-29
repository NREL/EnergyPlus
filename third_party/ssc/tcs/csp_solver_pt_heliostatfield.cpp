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

#include "csp_solver_pt_heliostatfield.h"
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

C_pt_heliostatfield::C_pt_heliostatfield()
{
	m_p_start = m_p_track = m_hel_stow_deploy = m_v_wind_max =
		m_eta_prev = m_v_wind_prev = m_v_wind_current = std::numeric_limits<double>::quiet_NaN();

	m_n_flux_x = m_n_flux_y = m_N_hel = -1;

	field_efficiency_table = 0;

	m_cdata = 0;		// = NULL
	mf_callback = 0;	// = NULL

	m_ncall = -1;
}

C_pt_heliostatfield::~C_pt_heliostatfield()
{
	if( field_efficiency_table != 0 )
		delete field_efficiency_table;
}

//void C_pt_heliostatfield::init(bool(*callback)(simulation_info* siminfo, void *data), void *cdata)
void C_pt_heliostatfield::init()
{
	//Read in parameters
	int nrows1, ncols1;
	int nrows2;
	int nrows4, ncols4;
	int nrows5, ncols5;
	int nfluxpos, nfposdim;
	int nfluxmap, nfluxcol;

	// Declare and initialize variables that are only used in initial call
	std::string weather_file;

	double helio_width = std::numeric_limits<double>::quiet_NaN();
	double helio_height = std::numeric_limits<double>::quiet_NaN();
	double helio_optical_error = std::numeric_limits<double>::quiet_NaN();
	double helio_active_fraction = std::numeric_limits<double>::quiet_NaN();
	double dens_mirror = std::numeric_limits<double>::quiet_NaN();
	double helio_reflectance = std::numeric_limits<double>::quiet_NaN();
	double rec_absorptance = std::numeric_limits<double>::quiet_NaN();
	double rec_height = std::numeric_limits<double>::quiet_NaN();
	double rec_aspect = std::numeric_limits<double>::quiet_NaN();
	double rec_hl_perm2 = std::numeric_limits<double>::quiet_NaN();
	double q_design = std::numeric_limits<double>::quiet_NaN();
	double h_tower = std::numeric_limits<double>::quiet_NaN();
	int land_bound_type = 0;
	double land_max = std::numeric_limits<double>::quiet_NaN();
	double land_min = std::numeric_limits<double>::quiet_NaN();
	double interp_nug = std::numeric_limits<double>::quiet_NaN();
	double interp_beta = std::numeric_limits<double>::quiet_NaN();

	double c_atm_0 = std::numeric_limits<double>::quiet_NaN();
	double c_atm_1 = std::numeric_limits<double>::quiet_NaN();
	double c_atm_2 = std::numeric_limits<double>::quiet_NaN();
	double c_atm_3 = std::numeric_limits<double>::quiet_NaN();

	int n_facet_x = 0;
	int n_facet_y = 0;

	int cant_type = 0;
	int focus_type = 0;

	int n_flux_days = 0;
	int delta_flux_hrs = 0;

	double dni_des = std::numeric_limits<double>::quiet_NaN();

	//double *helio_positions = NULL;
	util::matrix_t<double> helio_positions;	
	//double *eta_map = NULL;
	util::matrix_t<double> eta_map;
	//double *flux_maps = NULL;
	util::matrix_t<double> flux_maps;
	//double *land_bound_table = NULL;
	util::matrix_t<double> land_bound_table;
	//double *land_bound_list = NULL;
	util::matrix_t<double> land_bound_list;
	//double *helio_aim_points = NULL;
	util::matrix_t<double> helio_aim_points;
	//double *flux_positions = NULL;
	util::matrix_t<double> flux_positions;
	
	int pos_dim = 0;

	//double *flux_map = NULL;

	int run_type = ms_params.m_run_type;

	//Read in only those parameters that are relevant to the run scheme
	switch( run_type )
	{
	case RUN_TYPE::AUTO:
	case RUN_TYPE::USER_FIELD:
		helio_width = ms_params.m_helio_width;
		helio_height = ms_params.m_helio_height;
		helio_optical_error = ms_params.m_helio_optical_error;
		helio_active_fraction = ms_params.m_helio_active_fraction;
		dens_mirror = ms_params.m_dens_mirror;
		helio_reflectance = ms_params.m_helio_reflectance;
		rec_absorptance = ms_params.m_rec_absorptance;
		rec_height = ms_params.m_rec_height;
		rec_aspect = ms_params.m_rec_aspect;
		rec_hl_perm2 = ms_params.m_rec_hl_perm2;
		q_design = ms_params.m_q_design;
		h_tower = ms_params.m_h_tower;
		weather_file = ms_params.m_weather_file;
		land_bound_type = ms_params.m_land_bound_type;
		land_max = ms_params.m_land_max;
		land_min = ms_params.m_land_min;

		land_bound_table = ms_params.m_land_bound_table;
		nrows1 = land_bound_table.nrows();
		ncols1 = land_bound_table.ncols();
		//nrows1 = ms_params.m_nrows_land_bound_table;
		//ncols1 = ms_params.m_ncols_land_bound_table;
		//value(P_land_bound_table, &nrows1, &ncols1);

		land_bound_list = ms_params.m_land_bound_list;
		nrows2 = land_bound_list.nrows();
		//nrows2 = ms_params.m_nrows_land_bound_list;
		//land_bound_list = value(P_land_bound_list, &nrows2);

		m_p_start = ms_params.m_p_start;		//[kWe-hr] Heliostat startup energy
		m_p_track = ms_params.m_p_track;		//[kWe] Heliostat tracking power
		m_hel_stow_deploy = ms_params.m_hel_stow_deploy*CSP::pi / 180.0;	//[rad]
		m_v_wind_max = ms_params.m_v_wind_max;
		
		interp_nug = ms_params.m_interp_nug;
		interp_beta = ms_params.m_interp_beta;
		
		m_n_flux_x = ms_params.m_n_flux_x;
		m_n_flux_y = ms_params.m_n_flux_y;
		
		c_atm_0 = ms_params.m_c_atm_0;
		c_atm_1 = ms_params.m_c_atm_1;
		c_atm_2 = ms_params.m_c_atm_2;
		c_atm_3 = ms_params.m_c_atm_3;
		n_facet_x = ms_params.m_n_facet_x;
		n_facet_y = ms_params.m_n_facet_y;
		cant_type = ms_params.m_cant_type;
		focus_type = ms_params.m_focus_type;
		n_flux_days = ms_params.m_n_flux_days;
		delta_flux_hrs = ms_params.m_delta_flux_hrs;
		dni_des = ms_params.m_dni_des;

		pos_dim = 2;	//initiaize with 2 dimensions (x,y) on helio positions
		if( run_type != RUN_TYPE::USER_FIELD ) break;

		helio_positions = ms_params.m_helio_positions;
		m_N_hel = helio_positions.nrows();
		pos_dim = helio_positions.ncols();		
		//m_N_hel = ms_params.m_N_hel;
		//pos_dim = ms_params.m_pos_dim;
		//helio_positions = value(P_helio_positions, &N_hel, &pos_dim);
		
		helio_aim_points = ms_params.m_helio_aim_points;
		nrows4 = helio_aim_points.nrows();
		ncols4 = helio_aim_points.ncols();
		//nrows4 = ms_params.m_nrows_helio_aim_points;
		//ncols4 = ms_params.m_ncols_helio_aim_points;
		//helio_aim_points = value(P_helio_aim_points, &nrows4, &ncols4);

		break;
	case RUN_TYPE::USER_DATA:

		h_tower = ms_params.m_h_tower;
		land_bound_type = ms_params.m_land_bound_type;
		land_max = ms_params.m_land_max;
		land_min = ms_params.m_land_min;
		
		land_bound_table = ms_params.m_land_bound_table;
		nrows1 = land_bound_table.nrows();
		ncols1 = land_bound_table.ncols();
		//nrows1 = ms_params.m_nrows_land_bound_table;
		//ncols1 = ms_params.m_ncols_land_bound_table;
		//value(P_land_bound_table, &nrows1, &ncols1);
		
		land_bound_list = ms_params.m_land_bound_list;
		nrows2 = land_bound_list.nrows();
		//nrows2 = ms_params.m_nrows_land_bound_list;
		//land_bound_list = value(P_land_bound_list, &nrows2);
		
		m_p_start = ms_params.m_p_start;
		m_p_track = ms_params.m_p_track;
		m_hel_stow_deploy = ms_params.m_hel_stow_deploy*CSP::pi / 180.0;
		m_v_wind_max = ms_params.m_v_wind_max;
		interp_nug = ms_params.m_interp_nug;
		interp_beta = ms_params.m_interp_beta;

		helio_positions = ms_params.m_helio_positions;
		m_N_hel = helio_positions.nrows();
		pos_dim = helio_positions.ncols();
		//m_N_hel = ms_params.m_N_hel;
		//pos_dim = ms_params.m_pos_dim;
		//helio_positions = value(P_helio_positions, &N_hel, &pos_dim);
		
		helio_aim_points = ms_params.m_helio_aim_points;
		nrows4 = helio_aim_points.nrows();
		ncols4 = helio_aim_points.ncols();
		//nrows4 = ms_params.m_nrows_helio_aim_points;
		//ncols4 = ms_params.m_ncols_helio_aim_points;
		//helio_aim_points = value(P_helio_aim_points, &nrows4, &ncols4);
		
		eta_map = ms_params.m_eta_map;
		nrows5 = eta_map.nrows();
		ncols5 = eta_map.ncols();
		//nrows5 = ms_params.m_nrows_eta_map;
		//ncols5 = ms_params.m_ncols_eta_map;
		//eta_map = value(P_eta_map, &nrows5, &ncols5);

		m_n_flux_x = ms_params.m_n_flux_x;
		m_n_flux_y = ms_params.m_n_flux_y;
		
		flux_positions = ms_params.m_flux_positions;
		nfluxpos = flux_positions.nrows();
		nfposdim = flux_positions.ncols();
		//nfluxpos = ms_params.m_nfluxpos;
		//nfposdim = ms_params.m_nfposdim;
		//flux_positions = value(P_flux_positions, &nfluxpos, &nfposdim);

		flux_maps = ms_params.m_flux_maps;
		nfluxmap = flux_maps.nrows();
		nfluxcol = flux_maps.ncols();
		//nfluxmap = ms_params.m_nfluxmap;
		//nfluxcol = ms_params.m_nfluxcol;
		//flux_maps = value(P_flux_maps, &nfluxmap, &nfluxcol);

		c_atm_0 = ms_params.m_c_atm_0;
		c_atm_1 = ms_params.m_c_atm_1;
		c_atm_2 = ms_params.m_c_atm_2;
		c_atm_3 = ms_params.m_c_atm_3;
		n_facet_x = ms_params.m_n_facet_x;
		n_facet_y = ms_params.m_n_facet_y;
		cant_type = ms_params.m_cant_type;
		focus_type = ms_params.m_focus_type;
		n_flux_days = ms_params.m_n_flux_days;
		delta_flux_hrs = ms_params.m_delta_flux_hrs;
		dni_des = ms_params.m_dni_des;

		//check that flux maps match dimensions
		if( nfluxmap % nfluxpos != 0 )
		{
			error_msg = util::format("The number of flux maps provided does not match the number of flux map sun positions provided. Please "
				"ensure that the dimensionality of each flux map is consistent and that one sun position is provided for "
				"each flux map. (Sun pos. = %d, mismatch lines = %d)", nfluxpos, nfluxmap % nfluxpos);
			throw(C_csp_exception(error_msg, "heliostat field initialization"));
		}
		//copy the flux positions over to the local member
		m_flux_positions.resize(nfluxpos, VectDoub(nfposdim));
		for( int i = 0; i<nfluxpos; i++ )
		for( int j = 0; j<nfposdim; j++ )
			m_flux_positions.at(i).at(j) = flux_positions.at(i, j); 

		break;
	default:
		break;
	}

	MatDoub sunpos;
	vector<double> effs;
	vector<double> vis;

	//do initial runs of SolarPILOT and/or set up tables
	switch (run_type)
		{
		case RUN_TYPE::AUTO:
		case RUN_TYPE::USER_FIELD:
		{
			AutoPilot_S sapi;

			sp_optimize opt;
			sp_layout layout;
	
            var_map V;
	
            var_heliostat *hf = &V.hels.front();
			hf->width.val = helio_width;
			hf->height.val = helio_height;
			hf->err_azimuth.val = hf->err_elevation.val = hf->err_reflect_x.val = hf->err_reflect_y.val = 0.;   //all other error =0
			hf->err_surface_x.val = hf->err_surface_y.val = helio_optical_error;
			hf->reflect_ratio.val = helio_active_fraction * dens_mirror;   //availability * mirror area fraction
			hf->reflectivity.val = helio_reflectance;
            hf->soiling.val = 1.;   //all in the reflectance
			int cmap[5];
			cmap[0] = var_heliostat::CANT_METHOD::NO_CANTING;
            cmap[1] = var_heliostat::CANT_METHOD::ONAXIS_AT_SLANT;
            cmap[2] = cmap[3] = cmap[4] = var_heliostat::CANT_METHOD::OFFAXIS_DAY_AND_HOUR;
            hf->cant_method.combo_select_by_mapval( cmap[cant_type] );

            switch (cant_type)
            {
            case AutoPilot::API_CANT_TYPE::NONE:
            case AutoPilot::API_CANT_TYPE::ON_AXIS:
                //do nothing
                break;
            case AutoPilot::API_CANT_TYPE::EQUINOX:
                hf->cant_day.val = 81;  //spring equinox
	            hf->cant_hour.val = 12;
                break;
            case AutoPilot::API_CANT_TYPE::SOLSTICE_SUMMER:
                hf->cant_day.val = 172;  //Summer solstice
	            hf->cant_hour.val = 12;
                break;
            case AutoPilot::API_CANT_TYPE::SOLSTICE_WINTER:
                hf->cant_day.val = 355;  //Winter solstice
	            hf->cant_hour.val = 12;
                break;
            default:
            {
                stringstream msg;
                msg << "Invalid Cant Type specified in AutoPILOT API. Method must be one of: \n" <<
                        "NONE(0), ON_AXIS(1), EQUINOX(2), SOLSTICE_SUMMER(3), SOLSTICE_WINTER(4).\n" <<
                        "Method specified is: " << cant_type << ".";
                throw spexception(msg.str());
            }
                break;
            }

            hf->focus_method.combo_select_by_choice_index( focus_type );

            var_receiver *rf = &V.recs.front();
			rf->absorptance.val = rec_absorptance;
			rf->rec_height.val = rec_height;
			rf->rec_width.val = rf->rec_diameter.val = rec_height / rec_aspect;
			rf->therm_loss_base.val = rec_hl_perm2;
			
			V.sf.q_des.val = q_design;
			V.sf.dni_des.val = dni_des;
			V.land.is_bounds_scaled.val = true;
			V.land.is_bounds_fixed.val = false;
			V.land.is_bounds_array.val = false;
			V.land.max_scaled_rad.val = land_max;
			V.land.min_scaled_rad.val = land_min;
			V.sf.tht.val = h_tower;

			//set up the weather data for simulation
			const char *wffile = weather_file.c_str();
			if( !wffile )
			{
				mc_csp_messages.add_message(C_csp_messages::WARNING, "solarpilot: could not open weather file or invalid weather file format");
				//message(TCS_WARNING, "solarpilot: no weather file specified");
			}
			weatherfile wfile( wffile );
			if( !wfile.ok() || wfile.type() == weatherfile::INVALID )
			{
				mc_csp_messages.add_message(C_csp_messages::WARNING, "solarpilot: could not open weather file or invalid weather file format");
				//message(TCS_WARNING, "solarpilot: could not open weather file or invalid weather file format");
			}

			weather_header hdr;
			wfile.header( &hdr );

			V.amb.latitude.val = hdr.lat;
			V.amb.longitude.val = hdr.lon;
			V.amb.time_zone.val = hdr.tz;
			V.amb.atm_model.combo_select_by_mapval( var_ambient::ATM_MODEL::USERDEFINED );
            /*V.amb.atm_coefs.val.clear();
            V.amb.atm_coefs.val.resize(4);*/
			V.amb.atm_coefs.val.at(var_ambient::ATM_MODEL::USERDEFINED, 0) = c_atm_0;
			V.amb.atm_coefs.val.at(var_ambient::ATM_MODEL::USERDEFINED, 1) = c_atm_1;
			V.amb.atm_coefs.val.at(var_ambient::ATM_MODEL::USERDEFINED, 2) = c_atm_2;
			V.amb.atm_coefs.val.at(var_ambient::ATM_MODEL::USERDEFINED, 3) = c_atm_3;

			V.recs.front().peak_flux.val = 1000.0;
			V.opt.max_step.val = 0.06;
			V.opt.max_iter.val = 200;
			V.opt.converge_tol.val = 0.001;
			V.opt.algorithm.combo_select_by_mapval(1);
			V.opt.flux_penalty.val = 0.25;

			if(run_type == RUN_TYPE::AUTO)
			{
				V.recs.front().peak_flux.val = 1000.0;
				V.opt.max_step.val = 0.06;
				V.opt.max_iter.val = 200;
				V.opt.converge_tol.val = 0.001;
				V.opt.algorithm.combo_select_by_mapval(1);
				V.opt.flux_penalty.val = 0.25;

				/* 
				Generate the heliostat field layout using the settings provided by the user				
				*/
				vector<string> wfdata;
				wfdata.reserve( 8760 );
				for( int i=0;i<8760;i++ )
				{
					weather_record rec;
					if( !wfile.read( &rec ) )
					{
						error_msg = "solarpilot: could not read data line " + util::to_string(i+1) + " of 8760 in weather file";
						mc_csp_messages.add_message(C_csp_messages::WARNING, error_msg);
					}

					error_msg = util::format("%d,%d,%d,%.2lf,%.1lf,%.1lf,%.1lf", 
						rec.day, rec.hour, rec.month, rec.dn, rec.tdry, rec.pres / 1000., rec.wspd);
					wfdata.push_back(error_msg);
				}

				if( mf_callback && m_cdata )
				{
					sapi.SetSummaryCallback(mf_callback, m_cdata);
				}
				sapi.SetSummaryCallbackStatus(false);



				sapi.GenerateDesignPointSimulations( V, wfdata );
	
				sapi.Setup(V);

				sapi.CreateLayout(layout);


				//Copy the heliostat field positions into the 'helio_positions' data structure
				m_N_hel = (int)layout.heliostat_positions.size();
                string msg = "Auto-generated field: Number of heliostats " + util::to_string(m_N_hel);
				mc_csp_messages.add_message(C_csp_messages::NOTICE, msg);		//message(TCS_NOTICE, msg.c_str());
				//helio_positions = allocate(P_helio_positions, m_N_hel, pos_dim);
				ms_params.m_helio_positions.resize(m_N_hel, pos_dim);
				for( int i=0; i<m_N_hel; i++)
				{
					ms_params.m_helio_positions(i,0) = layout.heliostat_positions.at(i).location.x;
					ms_params.m_helio_positions(i,1) = layout.heliostat_positions.at(i).location.y;
					if(pos_dim==3)
						ms_params.m_helio_positions(i, 2) = layout.heliostat_positions.at(i).location.z;
				}
				
				//update the callbacks
				sapi.SetDetailCallbackStatus(false);
				
			}
			else{

				/* 
				Load in the heliostat field positions that are provided by the user.
				*/
				string format = "0,%f,%f,%f,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL;";
                V.sf.layout_data.val.clear();

                char row[200];
				for( int i=0; i<m_N_hel; i++)
				{
                    sprintf(row, format.c_str(), helio_positions(i,0), helio_positions(i,1), pos_dim == 3 ? helio_positions(i,2) : 0. );

                    V.sf.layout_data.val.append( row );

				}

                //set the template name 
                V.sf.temp_which.set_from_string( "Template 1" ); 

                sapi.Setup(V);
								
			}
            //land area update
			ms_params.m_land_area = V.land.land_area.Val();		//value(P_land_area, layout.land_area);

			if(!mf_callback || !m_cdata)
				sapi.SetSummaryCallbackStatus(false);
			else
			{
				sapi.SetSummaryCallbackStatus(true);
				sapi.SetSummaryCallback(mf_callback, m_cdata);
			}

			

			// set up flux map resolution
			sp_flux_table fluxtab;
			fluxtab.is_user_spacing = true;
			fluxtab.n_flux_days = n_flux_days;
			fluxtab.delta_flux_hrs = delta_flux_hrs;

			//run the flux maps
            if( m_n_flux_y == 1 )
                V.flux.aim_method.combo_select_by_mapval( var_fluxsim::AIM_METHOD::SIMPLE_AIM_POINTS );

			if(! sapi.CalculateFluxMaps(fluxtab, m_n_flux_x, m_n_flux_y, true) )
			{
				throw(C_csp_exception("Simulation cancelled during fluxmap preparation","heliostat field initialization"));
            }

			//collect efficiencies
			sunpos.clear();
			effs.clear();
			int npos = (int)fluxtab.azimuths.size();
			sunpos.reserve(npos);
			effs.reserve(npos);

            //eta_map = allocate( P_eta_map, npos, 3, 0.);
			ms_params.m_eta_map.resize_fill(npos, 3, 0.0);

			m_flux_positions.resize(npos, VectDoub(2) );

			for(int i=0; i<npos; i++)
			{
				sunpos.push_back( vector<double>(2, 0.) );

				sunpos.back().at(0) = fluxtab.azimuths.at(i) / az_scale;
				sunpos.back().at(1) = fluxtab.zeniths.at(i) / zen_scale;
				effs.push_back( fluxtab.efficiency.at(i) / eff_scale );

                //fill the parameter matrix to return this data to calling program
                //also fill the flux sun positions matrix
                m_flux_positions.at(i).at(0) = fluxtab.azimuths.at(i);
                ms_params.m_eta_map(i,0) = m_flux_positions.at(i).at(0)*180./CSP::pi;
                m_flux_positions.at(i).at(1) = fluxtab.zeniths.at(i);
                ms_params.m_eta_map(i,1) = m_flux_positions.at(i).at(1)*180./CSP::pi;
                
                ms_params.m_eta_map(i,2) = fluxtab.efficiency.at(i);
			}

			//collect flux's
			ms_params.m_flux_maps.resize_fill(m_n_flux_y*npos, m_n_flux_x, 0.0);
			
			block_t<double> *f = &fluxtab.flux_surfaces.front().flux_data;

			int nfl = f->nlayers();

			for(int i=0; i<nfl; i++)
			{
				for(int j=0; j<m_n_flux_y; j++)
				{
					for(int k=0; k<m_n_flux_x; k++)
					{
						ms_params.m_flux_maps(i*m_n_flux_y + j, k) = f->at(j, k, i);
					}
				}
			}

			break;
		}
		case RUN_TYPE::USER_DATA:
		{

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

            break;
		}
		default:
			break;
		}

		ms_outputs.m_flux_map_out.resize_fill(m_n_flux_y, m_n_flux_x, 0.0);

		//report back the flux positions used
		int nflux = (int)m_flux_positions.size();
		ms_params.m_flux_positions.resize_fill(nflux, 2, 0.0);
		
		for( int i = 0; i<nflux; i++ )
		{
			ms_params.m_flux_positions(i,0) = m_flux_positions.at(i).at(0);
			ms_params.m_flux_positions(i,1) = m_flux_positions.at(i).at(1);
		}

		/*
		------------------------------------------------------------------------------
		Create the regression fit on the efficiency map
		------------------------------------------------------------------------------
		*/

		//collect nug and beta
		interp_nug = ms_params.m_interp_nug;
		interp_beta = ms_params.m_interp_beta;

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
		
		// Calculate the total solar field reflective area
		ms_params.m_A_sf = ms_params.m_helio_height*ms_params.m_helio_width*ms_params.m_dens_mirror*m_N_hel;		//[m^2]
		
		// Initialize stored variables
		m_eta_prev = 0.0;
		m_v_wind_prev = 0.0;

		m_ncall = -1;
}

void C_pt_heliostatfield::call(const C_csp_weatherreader::S_outputs &weather, double field_control_in, const C_csp_solver_sim_info &sim_info)
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
	m_v_wind_current = v_wind;
	double field_control = field_control_in;	// Control Parameter ( range from 0 to 1; 0=off, 1=all on)
	if( field_control_in > 1.0 )
		field_control = 1.0;
	if( field_control_in < 0.0 )
		field_control = 0.0;

	double solzen = weather.m_solzen*CSP::pi / 180.0;

	if( solzen >= CSP::pi / 2.0 )
		field_control = 0.0;			// No tracking before sunrise or after sunset

	double solaz = weather.m_solazi*CSP::pi / 180.0;

	// clear out the existing flux map
	ms_outputs.m_flux_map_out.fill(0.0);

	// Parasitics for startup or shutdown
	double pparasi = 0.0;

	// If starting up or shutting down, calculate parasitics
	if( (field_control > 1.e-4 && m_eta_prev < 1.e-4) ||		// Startup by setting of control paramter (Field_control 0-> 1)
		(field_control < 1.e-4 && m_eta_prev >= 1.e-4) ||			// OR Shutdown by setting of control paramter (Field_control 1->0 )
		(field_control > 1.e-4 && v_wind >= m_v_wind_max) ||		// OR Shutdown by high wind speed
		(m_eta_prev > 1.e-4 && m_v_wind_prev >= m_v_wind_max && v_wind < m_v_wind_max) )	// OR Startup after high wind speed
		pparasi = m_N_hel * m_p_start / (step / 3600.0);			// [kWe-hr]/[hr] = kWe 

	// Parasitics for tracking      
	if( v_wind < m_v_wind_max && m_v_wind_prev < m_v_wind_max )
		pparasi += m_N_hel * m_p_track * field_control;				// [kWe]

	double eta_field = 0.;

	if( solzen > (CSP::pi / 2 - .001 - m_hel_stow_deploy) || v_wind > m_v_wind_max || time < 3601 )
	{
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
		for( int i = 0; i<(int)m_flux_positions.size(); i++ ){
			distances.push_back(rdist(&pos_now, &m_flux_positions.at(i)));
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

void C_pt_heliostatfield::off(const C_csp_solver_sim_info &sim_info)
{
	// Increase call-per-timestep counter
	// Converge() sets it to -1, so on first call this line will adjust it = 0
	m_ncall++;

	// Get sim info
	double step = sim_info.ms_ts.m_step;

	// Calculate stow parasitics (if applicable)
	double pparasi = 0.0;
		// Is field shutting down?
	if( m_eta_prev >= 1.e-4 )
	{
		pparasi = m_N_hel * m_p_start / (step / 3600.0);			// [kWe-hr]/[hr] = kWe 
	}

	ms_outputs.m_pparasi = pparasi / 1.E3;		//[MW], convert from kJ/hr: Parasitic power for tracking
	// Other outputs
		// clear out the existing flux map
	ms_outputs.m_flux_map_out.fill(0.0);
	ms_outputs.m_q_dot_field_inc = 0.0;		//[MWt]
	ms_outputs.m_eta_field = 0.0;			//[-], field efficiency

}

void C_pt_heliostatfield::converged()
{
	m_eta_prev = ms_outputs.m_eta_field;
	m_ncall = -1;
}

double C_pt_heliostatfield::rdist(VectDoub *p1, VectDoub *p2, int dim )
{
	double d = 0;
	for( int i = 0; i<dim; i++ ){
		double rd = p1->at(i) - p2->at(i);
		d += rd * rd;
	}
	return sqrt(d);
}
