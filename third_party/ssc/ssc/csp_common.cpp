/*******************************************************************************************************
*  Copyright 2017 Alliance for Sustainable Energy, LLC
*
*  NOTICE: This software was developed at least in part by Alliance for Sustainable Energy, LLC
*  (“Alliance”) under Contract No. DE-AC36-08GO28308 with the U.S. Department of Energy and the U.S.
*  The Government retains for itself and others acting on its behalf a nonexclusive, paid-up,
*  irrevocable worldwide license in the software to reproduce, prepare derivative works, distribute
*  copies to the public, perform publicly and display publicly, and to permit others to do so.
*
*  Redistribution and use in source and binary forms, with or without modification, are permitted
*  provided that the following conditions are met:
*
*  1. Redistributions of source code must retain the above copyright notice, the above government
*  rights notice, this list of conditions and the following disclaimer.
*
*  2. Redistributions in binary form must reproduce the above copyright notice, the above government
*  rights notice, this list of conditions and the following disclaimer in the documentation and/or
*  other materials provided with the distribution.
*
*  3. The entire corresponding source code of any redistribution, with or without modification, by a
*  research entity, including but not limited to any contracting manager/operator of a United States
*  National Laboratory, any institution of higher learning, and any non-profit organization, must be
*  made publicly available under this license for as long as the redistribution is made available by
*  the research entity.
*
*  4. Redistribution of this software, without modification, must refer to the software by the same
*  designation. Redistribution of a modified version of this software (i) may not refer to the modified
*  version by the same designation, or by any confusingly similar designation, and (ii) must refer to
*  the underlying software originally provided by Alliance as �System Advisor Model� or �SAM�. Except
*  to comply with the foregoing, the terms �System Advisor Model�, �SAM�, or any confusingly similar
*  designation may not be used to refer to any modified version of this software or any modified
*  version of the underlying software originally provided by Alliance without the prior written consent
*  of Alliance.
*
*  5. The name of the copyright holder, contributors, the United States Government, the United States
*  Department of Energy, or any of their employees may not be used to endorse or promote products
*  derived from this software without specific prior written permission.
*
*  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR
*  IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND
*  FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER,
*  CONTRIBUTORS, UNITED STATES GOVERNMENT OR UNITED STATES DEPARTMENT OF ENERGY, NOR ANY OF THEIR
*  EMPLOYEES, BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
*  DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
*  DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER
*  IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF
*  THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*******************************************************************************************************/

#include "csp_common.h"
#include "core.h"
#include "lib_weatherfile.h"
#include "lib_util.h"
#include <sstream>

#include "common.h"

// solarpilot header files
#include "AutoPilot_API.h"
#include "SolarField.h"
#include "IOUtil.h"

#ifdef _MSC_VER
#define mysnprintf _snprintf
#else
#define mysnprintf snprintf
#endif

using namespace std;

//static bool solarpilot_callback( simulation_info *siminfo, void *data );
static bool optimize_callback( simulation_info *siminfo, void *data );

solarpilot_invoke::solarpilot_invoke(compute_module *cm)
{
    m_cmod = cm;
    //anything else?
    m_sapi = 0;

}

solarpilot_invoke::~solarpilot_invoke()
{
    if(m_sapi != 0)
        delete m_sapi;
}

AutoPilot_S *solarpilot_invoke::GetSAPI()
{
    return m_sapi;
}

bool solarpilot_invoke::run(std::shared_ptr<weather_data_provider> wdata)
{
    /* 
    
    */
    if(m_sapi != 0)
        delete m_sapi;

    m_sapi = new AutoPilot_S();

	// read inputs from SSC module
		
    //fin.is_pmt_factors.val = true;
    //testing <<<


	bool isopt = m_cmod->as_boolean( "is_optimize" );
    if(isopt)
    {
		//opt.flux_max = m_cmod->as_double("flux_max");
        opt.max_step.val = m_cmod->as_double("opt_init_step");
        opt.max_iter.val = m_cmod->as_integer("opt_max_iter");
        opt.converge_tol.val = m_cmod->as_double("opt_conv_tol");
        opt.algorithm.combo_select_by_mapval( m_cmod->as_integer("opt_algorithm") ); //map correctly?
        opt.flux_penalty.val = m_cmod->as_double("opt_flux_penalty");
    }

	recs.front().peak_flux.val = m_cmod->as_double("flux_max");

    var_heliostat *hf = &hels.front();
    //need to set up the template combo
    sf.temp_which.combo_clear();
    std::string name = "Template 1", val = "0";
    sf.temp_which.combo_add_choice(name, val);
    sf.temp_which.combo_select_by_choice_index( 0 ); //use the first heliostat template

	hf->width.val = m_cmod->as_double("helio_width");
	hf->height.val = m_cmod->as_double("helio_height");
    hf->err_azimuth.val = hf->err_elevation.val = hf->err_reflect_x.val = hf->err_reflect_y.val = 0.;   //all other error =0
    hf->err_surface_x.val = hf->err_surface_y.val = m_cmod->as_double("helio_optical_error");       //slope error
    hf->soiling.val = 1.;   //reflectivity is the only consideration for this model

	hf->reflect_ratio.val = m_cmod->as_double("helio_active_fraction") * m_cmod->as_double("dens_mirror");
	hf->reflectivity.val = m_cmod->as_double("helio_reflectance");
	hf->n_cant_x.val = m_cmod->as_integer("n_facet_x");
	hf->n_cant_y.val = m_cmod->as_integer("n_facet_y");

    string cant_choices[] = {"No canting","On-axis at slant","On-axis, user-defined","Off-axis, day and hour","User-defined vector"};

	int cmap[5];
    cmap[0] = var_heliostat::CANT_METHOD::NO_CANTING;
    cmap[1] = var_heliostat::CANT_METHOD::ONAXIS_AT_SLANT;
    cmap[2] = cmap[3] = cmap[4] = var_heliostat::CANT_METHOD::OFFAXIS_DAY_AND_HOUR;

	int cant_type = m_cmod->as_integer("cant_type");

	//hf->cant_method.val = cmap[ cant_type ];       //Convert to the Heliostat::CANT_METHOD list
    //hf->cant_method.combo_select( cant_choices[cmap[cant_type]] );
    hf->cant_method.combo_select( cant_choices[cant_type] );
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


    hf->focus_method.combo_select_by_choice_index( m_cmod->as_integer("focus_type") );

    var_receiver *rf = &recs.front();

	rf->absorptance.val = m_cmod->as_double("rec_absorptance");
	rf->rec_height.val = m_cmod->as_double("rec_height");
	rf->rec_width.val = rf->rec_diameter.val = rf->rec_height.val/m_cmod->as_double("rec_aspect"); 
	rf->therm_loss_base.val = m_cmod->as_double("rec_hl_perm2");
		
    sf.q_des.val = m_cmod->as_double("q_design");
	sf.dni_des.val = m_cmod->as_double("dni_des");
    land.is_bounds_scaled.val = true;
    land.is_bounds_fixed.val = false;
    land.is_bounds_array.val = false;
	land.max_scaled_rad.val = m_cmod->as_double("land_max");
	land.min_scaled_rad.val = m_cmod->as_double("land_min");
	sf.tht.val = m_cmod->as_double("h_tower");
		
	fin.tower_fixed_cost.val = m_cmod->as_double("tower_fixed_cost");
	fin.tower_exp.val = m_cmod->as_double("tower_exp");
	fin.rec_ref_cost.val = m_cmod->as_double("rec_ref_cost");
	fin.rec_ref_area.val = m_cmod->as_double("rec_ref_area");
	fin.rec_cost_exp.val = m_cmod->as_double("rec_cost_exp");
	fin.site_spec_cost.val = m_cmod->as_double("site_spec_cost");
	fin.heliostat_spec_cost.val = m_cmod->as_double("heliostat_spec_cost");
	//fin.plant_spec_cost.val = m_cmod->as_double("plant_spec_cost") + m_cmod->as_double("bop_spec_cost");
	//fin.tes_spec_cost.val = m_cmod->as_double("tes_spec_cost");
	fin.land_spec_cost.val = m_cmod->as_double("land_spec_cost");
	fin.contingency_rate.val = m_cmod->as_double("contingency_rate");
	fin.sales_tax_rate.val = m_cmod->as_double("sales_tax_rate");
	fin.sales_tax_frac.val = m_cmod->as_double("sales_tax_frac");
	fin.fixed_cost.val = m_cmod->as_double("cost_sf_fixed");
    ////update financial tables
    //fin.weekday_sched.val = m_cmod->value("dispatch_sched_weekday").str;
    //fin.weekend_sched.val = m_cmod->value("dispatch_sched_weekend").str;
    //std::string ps;
    //for(int i=0; i<9; i++)
    //    ps.append( m_cmod->as_double("dispatch_factor" + my_to_string(i+1)) + i < 8 ? "," : "" );
    //fin.pricing_array.Val().clear();
    //fin.pricing_array.set_from_string( ps.c_str() );
    
	
	//set up the weather data for simulation
	if (wdata == nullptr){
		const char *wffile = m_cmod->as_string("solar_resource_file" );
		wdata = make_shared<weatherfile>( wffile );
		if ( !wdata ) throw exec_error( "solarpilot", "no weather file specified" );
		if ( !wdata->ok() || wdata->has_message() ) throw exec_error("solarpilot", wdata->message());
	}

	weather_header hdr;
	wdata->header(&hdr);
		
    amb.latitude.val = hdr.lat;
	amb.longitude.val = hdr.lon;
	amb.time_zone.val = hdr.tz;
	amb.atm_model.combo_select_by_choice_index(2); //USER_DEFINED
	
	amb.atm_coefs.val.at(2,0) = m_cmod->as_double("c_atm_0");
    amb.atm_coefs.val.at(2,1) = m_cmod->as_double("c_atm_1");
    amb.atm_coefs.val.at(2,2) = m_cmod->as_double("c_atm_2");
    amb.atm_coefs.val.at(2,3) = m_cmod->as_double("c_atm_3");

    if(! m_cmod->is_assigned("helio_positions_in") ) 
    {

	    weather_record wf;

	    vector<string> wfdata;
	    wfdata.reserve( 8760 );
	    char buf[1024];
	    for( int i=0;i<8760;i++ )
	    {
			if (!wdata->read(&wf))
			    throw exec_error("solarpilot", "could not read data line " + util::to_string(i+1) + " of 8760 in weather data");

		    mysnprintf(buf, 1023, "%d,%d,%d,%.2lf,%.1lf,%.1lf,%.1lf", wf.day, wf.hour, wf.month, wf.dn, wf.tdry, wf.pres/1000., wf.wspd);
		    wfdata.push_back( std::string(buf) );
	    }

	    m_sapi->SetDetailCallback( ssc_cmod_solarpilot_callback, m_cmod);
	    m_sapi->SetSummaryCallbackStatus(false);

	    m_sapi->GenerateDesignPointSimulations( *this, wfdata );
	
        if(isopt){
            m_cmod->log("Optimizing...", SSC_WARNING, 0.);
            m_sapi->SetSummaryCallback( optimize_callback, m_cmod);
		    m_sapi->Setup(*this, true);
            
            //set up optimization variables
            {
                int nv = 3;
                vector<double*> optvars(nv);
                vector<double> upper(nv, HUGE_VAL);
                vector<double> lower(nv, -HUGE_VAL);
                vector<double> stepsize(nv);
                vector<string> names(nv);

                //pointers
                optvars.at(0) = &sf.tht.val;
                optvars.at(1) = &recs.front().rec_height.val;
                optvars.at(2) = &recs.front().rec_diameter.val;
                //names
                names.at(0) = (split(sf.tht.name, ".")).back();
                names.at(1) = (split(recs.front().rec_height.name, ".")).back();
                names.at(2) = (split(recs.front().rec_diameter.name, ".")).back();
                //step size
                stepsize.at(0) = sf.tht.val*opt.max_step.val;
                stepsize.at(1) = recs.front().rec_height.val*opt.max_step.val;
                stepsize.at(2) = recs.front().rec_diameter.val*opt.max_step.val;

                if(! m_sapi->Optimize(opt.algorithm.mapval(), optvars, upper, lower, stepsize, &names) )
                    return false;
            }

			m_sapi->Setup(*this);
            m_sapi->SetSummaryCallbackStatus(false);
            m_sapi->PreSimCallbackUpdate();
            
        }
        else{
		    m_sapi->Setup(*this);
        }
        if(! m_sapi->CreateLayout(layout) )
            return false;

    }
    else
    {
        /* 
		Load in the heliostat field positions that are provided by the user.
		*/
		//layout.heliostat_positions.clear();
		//layout.heliostat_positions.resize(m_N_hel);
		string format = "0,%f,%f,%f,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL;";
        sf.layout_data.val.clear();

        util::matrix_t<double> hpos = m_cmod->as_matrix("helio_positions_in");

        char row[200];
		for( size_t i=0; i<hpos.nrows(); i++)
		{
            sprintf(row, format.c_str(), hpos.at(i,0), hpos.at(i,1),  0. );

            sf.layout_data.val.append( row );
		}

		m_sapi->Setup(*this);
    }
    
    //check if flux map calculations are desired
	if( m_cmod->as_boolean("calc_fluxmaps") ){      // <<--- was set "false" for some reason

		m_sapi->SetDetailCallbackStatus(false);
		m_sapi->SetSummaryCallbackStatus(true);
		m_sapi->SetSummaryCallback( ssc_cmod_solarpilot_callback, m_cmod );
		
	
		//sp_optical_table opttab;
		fluxtab.is_user_spacing = true;
		fluxtab.n_flux_days = m_cmod->as_integer("n_flux_days");
		fluxtab.delta_flux_hrs = m_cmod->as_integer("delta_flux_hrs");
		
        string aim_method_save = flux.aim_method.val;
        flux.aim_method.combo_select( "Simple aim points" );

		int nflux_x = m_cmod->as_integer("n_flux_x");
		int nflux_y = m_cmod->as_integer("n_flux_y");
		//int nflux_x = 12, nflux_y = 1;
		if(! m_sapi->CalculateFluxMaps(fluxtab, nflux_x, nflux_y, true) )
        {
            flux.aim_method.combo_select( aim_method_save );
            return false;  //simulation failed or was cancelled.
        }
        flux.aim_method.combo_select( aim_method_save );


		//collect the optical efficiency data and sun positions
		if ( fluxtab.zeniths.size() == 0 || fluxtab.azimuths.size() == 0
			|| fluxtab.efficiency.size() == 0 )
			throw exec_error("solarpilot", "failed to calculate a correct optical efficiency table");
		
		//collect the flux map data
		block_t<double> *flux_data = &fluxtab.flux_surfaces.front().flux_data;  //there should be only one flux stack for SAM
		if( flux_data->ncols() == 0 || flux_data->nlayers() == 0 )
			throw exec_error("solarpilot", "failed to calculate a correct flux map table");
	}

    //check if max flux check is desired
    if( m_cmod->as_boolean("check_max_flux") )
    {
        m_sapi->SetDetailCallbackStatus(false);
		m_sapi->SetSummaryCallbackStatus(true);
		m_sapi->SetSummaryCallback( ssc_cmod_solarpilot_callback, m_cmod );
		
	    sp_flux_table flux_temp;

		//sp_optical_table opttab;
		flux_temp.is_user_spacing = false;
        flux_temp.azimuths.clear();
        flux_temp.zeniths.clear();

        flux_temp.azimuths.push_back( (flux.flux_solar_az.Val())*D2R );
        flux_temp.zeniths.push_back( (90.-flux.flux_solar_el.Val())*D2R );

		
		if(! m_sapi->CalculateFluxMaps(flux_temp, 20, 15, false) )
            return false;  //simulation failed or was cancelled.
            
        
		block_t<double> *flux_data = &flux_temp.flux_surfaces.front().flux_data;  //there should be only one flux stack for SAM
        
        double flux_max_observed = 0.;

        for(size_t i=0; i<flux_data->nrows(); i++)
        {
            for(size_t j=0; j<flux_data->ncols(); j++)
            {
                if( flux_data->at(i, j, 0) > flux_max_observed ) 
                    flux_max_observed = flux_data->at(i, j, 0);
            }
        }

		m_cmod->assign("flux_max_observed", (ssc_number_t)flux_max_observed);
    }
        
    return true;
}

bool solarpilot_invoke::postsim_calcs(compute_module *cm)
{
    /* 
    Update calculated values and cost model number to be used in subsequent simulation and analysis.

    The variable values used in this are consistent with the solarpilot compute module. These same variables are used in all 
    tower modules that use solarpilot API.

    */


    //receiver calculations
    double H_rec = recs.front().rec_height.val;
    double rec_aspect = recs.front().rec_aspect.Val();
    double THT = sf.tht.val;
    //update heliostat position table
    int nr = (int)heliotab.positions.size();
    ssc_number_t *ssc_hl = cm->allocate( "helio_positions", nr, 2 );
    for(int i=0; i<nr; i++){
        ssc_hl[i*2] = (ssc_number_t)layout.heliostat_positions.at(i).location.x;
        ssc_hl[i*2+1] = (ssc_number_t)layout.heliostat_positions.at(i).location.y;
    }

    double A_sf = cm->as_double("helio_height") * cm->as_double("helio_width") * cm->as_double("dens_mirror") * (double)nr;

    //update piping length for parasitic calculation
    double piping_length = THT * cm->as_double("csp.pt.par.piping_length_mult") + cm->as_double("csp.pt.par.piping_length_const");
            
    //update assignments for cost model
	cm->assign("H_rec", var_data((ssc_number_t)H_rec));
    cm->assign("rec_height", var_data((ssc_number_t)H_rec));
	cm->assign("rec_aspect", var_data((ssc_number_t)rec_aspect));
    cm->assign("D_rec", var_data((ssc_number_t)(H_rec/rec_aspect)));
	cm->assign("THT", var_data((ssc_number_t)THT));
    cm->assign("h_tower", var_data((ssc_number_t)THT));
	cm->assign("A_sf", var_data((ssc_number_t)A_sf));
    cm->assign("Piping_length", var_data((ssc_number_t)piping_length) );

    //Update the total installed cost
    double total_direct_cost = 0.;
    double A_rec = std::numeric_limits<double>::quiet_NaN();
    switch (recs.front().rec_type.mapval())
    {
    case var_receiver::REC_TYPE::EXTERNAL_CYLINDRICAL:
      {
        double h = recs.front().rec_height.val;
        double d = h/recs.front().rec_aspect.Val();
        A_rec =  h*d*3.1415926;
        break;
      }
    case var_receiver::REC_TYPE::FLAT_PLATE:
      {
        double h = recs.front().rec_height.val;
        double w = h/recs.front().rec_aspect.Val();
        A_rec = h*w;
        break;
      }
    }
    double receiver = cm->as_double("rec_ref_cost")*pow(A_rec/cm->as_double("rec_ref_area"), cm->as_double("rec_cost_exp"));     //receiver cost

    //storage cost
    double storage = cm->as_double("q_pb_design")*cm->as_double("tshours")*cm->as_double("tes_spec_cost")*1000.;

    //power block + BOP
    double P_ref = cm->as_double("P_ref") * 1000.;  //kWe
    double power_block = P_ref * (cm->as_double("plant_spec_cost") + cm->as_double("bop_spec_cost") ); //$/kWe --> $

    //site improvements
    double site_improvements = A_sf * cm->as_double("site_spec_cost");
            
    //heliostats
    double heliostats = A_sf * cm->as_double("heliostat_spec_cost");
            
    //fixed cost
    double cost_fixed = cm->as_double("cost_sf_fixed");

    //fossil
    double fossil = P_ref * cm->as_double("fossil_spec_cost");

    //tower cost
    double tower = cm->as_double("tower_fixed_cost") * exp( cm->as_double("tower_exp") * (THT + 0.5*(-H_rec + cm->as_double("helio_height")) ) );

    //---- total direct cost -----
    total_direct_cost = (1. + cm->as_double("contingency_rate")/100.) * (
        site_improvements + heliostats + power_block + 
        cost_fixed + storage + fossil + tower + receiver);
    //-----

    //land area
    double land_area = land.land_area.Val() * cm->as_double("csp.pt.sf.land_overhead_factor") + cm->as_double("csp.pt.sf.fixed_land_area");

    //EPC
    double cost_epc = 
        cm->as_double("csp.pt.cost.epc.per_acre") * land_area
        + cm->as_double("csp.pt.cost.epc.percent") * total_direct_cost / 100.
        + P_ref * 1000. * cm->as_double("csp.pt.cost.epc.per_watt") 
        + cm->as_double("csp.pt.cost.epc.fixed");

    //PLM
    double cost_plm = 
        cm->as_double("csp.pt.cost.plm.per_acre") * land_area
        + cm->as_double("csp.pt.cost.plm.percent") * total_direct_cost / 100.
        + P_ref * 1000. * cm->as_double("csp.pt.cost.plm.per_watt") 
        + cm->as_double("csp.pt.cost.plm.fixed");

    //sales tax
    //return ${csp.pt.cost.sales_tax.value}/100*${total_direct_cost}*${csp.pt.cost.sales_tax.percent}/100; };
    double cost_sales_tax = cm->as_double("sales_tax_rate")/100. * total_direct_cost * cm->as_double("sales_tax_frac")/100.;

    //----- indirect cost
    double total_indirect_cost = cost_epc + cost_plm + cost_sales_tax;
            
    //----- total installed cost!
    double total_installed_cost = total_direct_cost + total_indirect_cost;
    cm->assign("total_installed_cost", var_data((ssc_number_t)total_installed_cost ));

    return true;

}


void solarpilot_invoke::getOptimizationSimulationHistory(vector<vector<double> > &sim_points, vector<double> &obj_values, vector<double> &flux_values)
{
	/* 
	Return the addresses of the optimization simulation history data, if applicable.
	*/
	sim_points = _optimization_sim_points;
	obj_values = _optimization_objectives;
	flux_values = _optimization_fluxes;
}

void solarpilot_invoke::setOptimizationSimulationHistory(vector<vector<double> > &sim_points, vector<double> &obj_values, vector<double> &flux_values)
{
	//Create local copies
	_optimization_sim_points = sim_points;
	_optimization_objectives = obj_values;
	_optimization_fluxes = flux_values;
}


bool ssc_cmod_solarpilot_callback( simulation_info *siminfo, void *data )
{
	compute_module *cm = static_cast<compute_module*>( data );
	if ( !cm ) return false;
	float simprogress = (float)siminfo->getCurrentSimulation()/(float)(max(siminfo->getTotalSimulationCount(),1));

	return cm->update( *siminfo->getSimulationNotices(),
		simprogress*100.0f );

}

static bool optimize_callback( simulation_info *siminfo, void *data )
{
    compute_module *cm = static_cast<compute_module*>( data );
    if(! cm) return false;
    
    std::string notices = *siminfo->getSimulationNotices();
    cm->log( notices, SSC_WARNING, 0. );
    
    return true;
}

bool are_values_sig_different(double v1, double v2, double tol)
{
    if (fabs(v1) < tol || fabs(v2) < tol)
    {
        if (fabs(v1 - v2) > tol)
        {
            return true;
        }
    }
    else
    {
        if (fabs(v1 - v2) / std::min(fabs(v1), fabs(v2)) > tol)
        {
            return true;
        }
    }
    
    return false;
}

var_info vtab_sco2_design[] = {

	/*   VARTYPE   DATATYPE         NAME               LABEL                                                    UNITS     META  GROUP REQUIRED_IF CONSTRAINTS     UI_HINTS*/
	// ** Design Parameters **
		// System Design
	{ SSC_INPUT,  SSC_NUMBER,  "htf",                  "Integer code for HTF used in PHX",                       "",           "",    "System Design",      "*",     "",       "" },
    { SSC_INPUT,  SSC_MATRIX,  "htf_props",            "User defined HTF property data",                         "", "7 columns (T,Cp,dens,visc,kvisc,cond,h), at least 3 rows", "System Design", "?=[[0]]", "", "" },
	{ SSC_INPUT,  SSC_NUMBER,  "T_htf_hot_des",        "HTF design hot temperature (PHX inlet)",                 "C",          "",    "System Design",      "*",     "",       "" },
	{ SSC_INPUT,  SSC_NUMBER,  "dT_PHX_hot_approach",  "Temp diff btw hot HTF and turbine inlet",                "C",          "",    "System Design",      "*",     "",       "" },
	{ SSC_INPUT,  SSC_NUMBER,  "T_amb_des",            "Ambient temperature",                                    "C",          "",    "System Design",      "*",     "",       "" },
	{ SSC_INPUT,  SSC_NUMBER,  "dT_mc_approach",       "Temp diff btw ambient air and main compressor inlet",    "C",          "",    "System Design",      "*",     "",       "" },
	{ SSC_INPUT,  SSC_NUMBER,  "site_elevation",       "Site elevation",                                         "m",          "",    "System Design",      "*",     "",       "" },
	{ SSC_INPUT,  SSC_NUMBER,  "W_dot_net_des",        "Design cycle power output (no cooling parasitics)",      "MWe",        "",    "System Design",      "*",     "",       "" },
	{ SSC_INPUT,  SSC_NUMBER,  "design_method",        "1 = Specify efficiency, 2 = Specify total recup UA, 3 = Specify each recup design","","","System Design","*","",       "" },
	{ SSC_INPUT,  SSC_NUMBER,  "eta_thermal_des",      "Power cycle thermal efficiency",                         "",           "",    "System Design",      "design_method=1","",  "" },
	    
        // Heat exchanger design
            // Combined recuperator design parameter (design_method == 2)
    { SSC_INPUT,  SSC_NUMBER,  "UA_recup_tot_des",     "Total recuperator conductance",                          "kW/K",       "Combined recuperator design",    "Heat Exchanger Design",      "design_method=2","",  "" },
	        // Low temperature recuperator parameters
    { SSC_INPUT,  SSC_NUMBER,  "LTR_design_code",      "1 = UA, 2 = min dT, 3 = effectiveness",                  "-",          "Low temperature recuperator",    "Heat Exchanger Design",      "design_method=3", "", "" },
	{ SSC_INPUT,  SSC_NUMBER,  "LTR_UA_des_in",        "Design LTR conductance",                                 "kW/K",       "Low temperature recuperator",    "Heat Exchanger Design",      "design_method=3", "", "" },
	{ SSC_INPUT,  SSC_NUMBER,  "LTR_min_dT_des_in",    "Design minimum allowable temperature difference in LTR", "C",          "Low temperature recuperator",    "Heat Exchanger Design",      "design_method=3", "", "" },
    { SSC_INPUT,  SSC_NUMBER,  "LTR_eff_des_in",       "Design effectiveness for LTR",                           "-",          "Low temperature recuperator",    "Heat Exchanger Design",      "design_method=3", "", "" },
    { SSC_INPUT,  SSC_NUMBER,  "LT_recup_eff_max",     "Maximum allowable effectiveness in LTR",                 "-",          "Low temperature recuperator",    "Heat Exchanger Design",      "?=1.0", "",       "" },
    { SSC_INPUT,  SSC_NUMBER,  "LTR_LP_deltaP_des_in", "LTR low pressure side pressure drop as fraction of inlet pressure","-", "Low temperature recuperator",   "Heat Exchanger Design",      "",      "",       "" },
    { SSC_INPUT,  SSC_NUMBER,  "LTR_HP_deltaP_des_in", "LTR high pressure side pressure drop as fraction of inlet pressure","-", "Low temperature recuperator",  "Heat Exchanger Design",      "",      "",       "" },
    { SSC_INPUT,  SSC_NUMBER,  "LTR_n_sub_hx",         "LTR number of model subsections",                        "-",          "Low temperature recuperator",    "Heat Exchanger Design",      "?=10",  "",       "" },
    { SSC_INPUT,  SSC_NUMBER,  "LTR_od_model",         "0: mass flow scale, 1: conductance ratio model",         "-",          "Low temperature recuperator",    "Heat Exchanger Design",      "?=1",   "",       "" },
        // High temperature recuperator parameters
    { SSC_INPUT,  SSC_NUMBER,  "HTR_design_code",      "1 = UA, 2 = min dT, 3 = effectiveness",                  "-",          "High temperature recuperator",    "Heat Exchanger Design",      "design_method=3", "", "" },
	{ SSC_INPUT,  SSC_NUMBER,  "HTR_UA_des_in",        "Design HTR conductance",                                 "kW/K",       "High temperature recuperator",    "Heat Exchanger Design",      "design_method=3", "", "" },
	{ SSC_INPUT,  SSC_NUMBER,  "HTR_min_dT_des_in",    "Design minimum allowable temperature difference in HTR", "C",          "High temperature recuperator",    "Heat Exchanger Design",      "design_method=3", "", "" },
    { SSC_INPUT,  SSC_NUMBER,  "HTR_eff_des_in",       "Design effectiveness for HTR",                           "-",          "High temperature recuperator",    "Heat Exchanger Design",      "design_method=3", "", "" },
    { SSC_INPUT,  SSC_NUMBER,  "HT_recup_eff_max",     "Maximum allowable effectiveness in HTR",                 "-",          "High temperature recuperator",    "Heat Exchanger Design",      "?=1.0", "",       "" },
    { SSC_INPUT,  SSC_NUMBER,  "HTR_LP_deltaP_des_in", "HTR low pressure side pressure drop as fraction of inlet pressure","-", "High temperature recuperator",   "Heat Exchanger Design",      "",      "",       "" },
    { SSC_INPUT,  SSC_NUMBER,  "HTR_HP_deltaP_des_in", "HTR high pressure side pressure drop as fraction of inlet pressure","-", "High temperature recuperator",  "Heat Exchanger Design",      "",      "",       "" },
    { SSC_INPUT,  SSC_NUMBER,  "HTR_n_sub_hx",         "HTR number of model subsections",                        "-",          "High temperature recuperator",    "Heat Exchanger Design",      "?=10",  "",       "" },
    { SSC_INPUT,  SSC_NUMBER,  "HTR_od_model",         "0: mass flow scale, 1: conductance ratio model",         "-",          "High temperature recuperator",    "Heat Exchanger Design",      "?=1",   "",       "" },

    { SSC_INPUT,  SSC_NUMBER,  "cycle_config",         "1 = recompression, 2 = partial cooling",                 "",           "High temperature recuperator",    "Heat Exchanger Design",      "?=1",   "",       "" },
	{ SSC_INPUT,  SSC_NUMBER,  "is_recomp_ok",         "1 = Yes, 0 = simple cycle only, < 0 = fix f_recomp to abs(input)","",  "High temperature recuperator",    "Heat Exchanger Design",      "?=1",   "",       "" },
	{ SSC_INPUT,  SSC_NUMBER,  "is_P_high_fixed",      "1 = Yes (=P_high_limit), 0 = No, optimized (default)",                   "",           "High temperature recuperator",    "Heat Exchanger Design",      "?=0",   "",       "" },
	{ SSC_INPUT,  SSC_NUMBER,  "is_PR_fixed",          "0 = No, >0 = fixed pressure ratio at input <0 = fixed LP at abs(input)",                      "High temperature recuperator",           "",    "Heat Exchanger Design",      "?=0",   "",       "" },
    { SSC_INPUT,  SSC_NUMBER,  "is_IP_fixed",          "partial cooling config: 0 = No, >0 = fixed HP-IP pressure ratio at input, <0 = fixed IP at abs(input)","","High temperature recuperator","Heat Exchanger Design","?=0", "",       "" },

    { SSC_INPUT,  SSC_NUMBER,  "des_objective",        "[2] = hit min phx deltat then max eta, [else] max eta",  "",           "High temperature recuperator",    "Heat Exchanger Design",      "?=0",   "",       "" },
	{ SSC_INPUT,  SSC_NUMBER,  "min_phx_deltaT",       "Minimum design temperature difference across PHX",       "C",          "High temperature recuperator",    "Heat Exchanger Design",      "?=0",   "",       "" },
	{ SSC_INPUT,  SSC_NUMBER,  "rel_tol",              "Baseline solver and optimization relative tolerance exponent (10^-rel_tol)", "-", "High temperature recuperator", "Heat Exchanger Design", "?=3","",       "" },
		// Cycle Design
	{ SSC_INPUT,  SSC_NUMBER,  "eta_isen_mc",          "Design main compressor isentropic efficiency",           "-",          "",    "",      "*",     "",       "" },
    { SSC_INPUT,  SSC_NUMBER,  "mc_comp_type",         "Main compressor compressor type 1: SNL 2: CompA",        "-",          "",    "",      "?=1",   "",       "" },
    { SSC_INPUT,  SSC_NUMBER,  "eta_isen_rc",          "Design re-compressor isentropic efficiency",             "-",          "",    "",      "*",     "",       "" },
	{ SSC_INPUT,  SSC_NUMBER,  "eta_isen_pc",          "Design precompressor isentropic efficiency",             "-",          "",    "",      "cycle_config=2",     "",       "" },
	{ SSC_INPUT,  SSC_NUMBER,  "eta_isen_t",           "Design turbine isentropic efficiency",                   "-",          "",    "",      "*",     "",       "" },
	{ SSC_INPUT,  SSC_NUMBER,  "PHX_co2_deltaP_des_in","PHX co2 side pressure drop as fraction of inlet pressure","-",         "",    "",      "",      "",       "" },
    { SSC_INPUT,  SSC_NUMBER,  "deltaP_counterHX_frac","Fraction of CO2 inlet pressure that is design point counterflow HX (recups & PHX) pressure drop", "-", "", "", "?=0", "", ""},
	{ SSC_INPUT,  SSC_NUMBER,  "P_high_limit",         "High pressure limit in cycle",                           "MPa",        "",    "",      "*",     "",       "" },

		// PHX Design
	{ SSC_INPUT,  SSC_NUMBER,  "dT_PHX_cold_approach", "Temp diff btw cold HTF and cold CO2",                    "C",          "",    "PHX Design",      "*",     "",       "" },
    { SSC_INPUT,  SSC_NUMBER,  "PHX_n_sub_hx",         "Number of subsections in PHX model",                     "-",          "",    "PHX Design",      "?=10",  "",       "" },
    { SSC_INPUT,  SSC_NUMBER,  "PHX_od_model",         "0: mass flow scale, 1: conductance ratio model",         "-",          "",    "PHX Design",      "?=1",   "",       "" },

        // Air Cooler Design
	{ SSC_INPUT,  SSC_NUMBER,  "is_design_air_cooler", "Defaults to True. False will skip air cooler calcs",     "",           "",    "Air Cooler Design",      "?=1.0", "",       "" },
	{ SSC_INPUT,  SSC_NUMBER,  "fan_power_frac",       "Fraction of net cycle power consumed by air cooler fan", "",           "",    "Air Cooler Design",      "*",     "",       "" },
	{ SSC_INPUT,  SSC_NUMBER,  "deltaP_cooler_frac",   "Fraction of CO2 inlet pressure that is design point cooler CO2 pressure drop", "", "", "Air Cooler Design", "*", "",       "" },
    { SSC_INPUT,  SSC_NUMBER,  "eta_air_cooler_fan",   "Air cooler fan isentropic efficiency",                   "",           "",    "Air Cooler Design",      "?=0.5", "",       "" },
    { SSC_INPUT,  SSC_NUMBER,  "N_nodes_air_cooler_pass", "Number of nodes in single air cooler pass",           "",           "",    "Air Cooler Design",      "?=10",  "",       "" },

	// ** Design OUTPUTS **
		// System Design Solution
	{ SSC_OUTPUT, SSC_NUMBER,  "T_htf_cold_des",       "HTF design cold temperature (PHX outlet)",               "C",          "System Design Solution",    "",      "*",     "",       "" },
	{ SSC_OUTPUT, SSC_NUMBER,  "m_dot_htf_des",        "HTF mass flow rate",                                     "kg/s",       "System Design Solution",    "",      "*",     "",       "" },
	{ SSC_OUTPUT, SSC_NUMBER,  "eta_thermal_calc",     "Calculated cycle thermal efficiency",                    "-",          "System Design Solution",    "",      "*",     "",       "" },
    { SSC_OUTPUT, SSC_NUMBER,  "m_dot_co2_full",       "CO2 mass flow rate through HTR, PHX, turbine",           "kg/s",       "System Design Solution",    "",      "*",     "",       "" },
	{ SSC_OUTPUT, SSC_NUMBER,  "recomp_frac",          "Recompression fraction",                                 "-",          "System Design Solution",    "",      "*",     "",       "" },
	{ SSC_OUTPUT, SSC_NUMBER,  "cycle_cost",           "Cycle cost",                                             "M$",         "System Design Solution",    "",      "*",     "",       "" },
	{ SSC_OUTPUT, SSC_NUMBER,  "cycle_spec_cost",      "Cycle specific cost",                                    "$/kWe",      "System Design Solution",    "",      "*",     "",       "" },
	{ SSC_OUTPUT, SSC_NUMBER,  "cycle_spec_cost_thermal", "Cycle specific cost - thermal",                       "$/kWt",      "System Design Solution",    "",      "*",     "",       "" },
	{ SSC_OUTPUT, SSC_NUMBER,  "W_dot_net_less_cooling", "System power output subtracting cooling parastics",    "MWe,"        "System Design Solution",    "",      "*",     "",       "" },
    { SSC_OUTPUT, SSC_NUMBER,  "eta_thermal_net_less_cooling_des","Calculated cycle thermal efficiency using W_dot_net_less_cooling", "-", "System Design Solution","",  "*", "",       "" },
    // Compressor
	{ SSC_OUTPUT, SSC_NUMBER,  "T_comp_in",            "Compressor inlet temperature",                           "C",          "Compressor",    "",      "*",     "",       "" },
	{ SSC_OUTPUT, SSC_NUMBER,  "P_comp_in",            "Compressor inlet pressure",                              "MPa",        "Compressor",    "",      "*",     "",       "" },
	{ SSC_OUTPUT, SSC_NUMBER,  "P_comp_out",           "Compressor outlet pressure",                             "MPa",        "Compressor",    "",      "*",     "",       "" },
	{ SSC_OUTPUT, SSC_NUMBER,  "mc_T_out",             "Compressor outlet temperature",                          "C",          "Compressor",    "",      "*",     "",       "" },
	{ SSC_OUTPUT, SSC_NUMBER,  "mc_W_dot",             "Compressor power",                                       "MWe",        "Compressor",    "",      "*",     "",       "" },
	{ SSC_OUTPUT, SSC_NUMBER,  "mc_m_dot_des",         "Compressor mass flow rate",                              "kg/s",       "Compressor",    "",      "*",     "",       "" },
	{ SSC_OUTPUT, SSC_NUMBER,  "mc_rho_in",            "Compressor inlet density",                               "kg/m3",      "Compressor",    "",      "*",     "",       "" },
	{ SSC_OUTPUT, SSC_NUMBER,  "mc_ideal_spec_work",   "Compressor ideal spec work",                             "kJ/kg",      "Compressor",    "",      "*",     "",       "" },
	{ SSC_OUTPUT, SSC_NUMBER,  "mc_phi_des",           "Compressor design flow coefficient",					 "",           "Compressor",    "",      "*",     "",       "" },
    { SSC_OUTPUT, SSC_NUMBER,  "mc_psi_des",           "Compressor design ideal head coefficient",               "",           "Compressor",    "",      "*",     "",       "" },
    { SSC_OUTPUT, SSC_ARRAY,   "mc_tip_ratio_des",     "Compressor design stage tip speed ratio",                "",           "Compressor",    "",      "*",     "",       "" },
	{ SSC_OUTPUT, SSC_NUMBER,  "mc_n_stages",          "Compressor stages",                                      "",           "Compressor",    "",      "*",     "",       "" },
	{ SSC_OUTPUT, SSC_NUMBER,  "mc_N_des",             "Compressor design shaft speed",                          "rpm",        "Compressor",    "",      "*",     "",       "" },
	{ SSC_OUTPUT, SSC_ARRAY,   "mc_D",                 "Compressor stage diameters",                             "m",          "Compressor",    "",      "*",     "",       "" },
	{ SSC_OUTPUT, SSC_NUMBER,  "mc_phi_surge",         "Compressor flow coefficient where surge occurs",         "",           "Compressor",    "",      "*",     "",       "" },
    { SSC_OUTPUT, SSC_NUMBER,  "mc_psi_max_at_N_des",  "Compressor max ideal head coefficient at design shaft speed", "",      "Compressor",    "",      "*",     "",       "" },
    { SSC_OUTPUT, SSC_ARRAY,   "mc_eta_stages_des",    "Compressor design stage isentropic efficiencies",        "",           "Compressor",    "",      "*",     "",       "" },
	{ SSC_OUTPUT, SSC_NUMBER,  "mc_cost",              "Compressor cost",                                        "M$",         "Compressor",    "",      "*",     "",       "" },
		// Recompressor
	{ SSC_OUTPUT, SSC_NUMBER,  "rc_T_in_des",          "Recompressor inlet temperature",                         "C",          "Recompressor",    "",      "*",     "",       "" },
	{ SSC_OUTPUT, SSC_NUMBER,  "rc_P_in_des",          "Recompressor inlet pressure",                            "MPa",        "Recompressor",    "",      "*",     "",       "" },
	{ SSC_OUTPUT, SSC_NUMBER,  "rc_T_out_des",         "Recompressor inlet temperature",                         "C",          "Recompressor",    "",      "*",     "",       "" },
	{ SSC_OUTPUT, SSC_NUMBER,  "rc_P_out_des",         "Recompressor inlet pressure",                            "MPa",        "Recompressor",    "",      "*",     "",       "" },
	{ SSC_OUTPUT, SSC_NUMBER,  "rc_W_dot",             "Recompressor power",                                     "MWe",        "Recompressor",    "",      "*",     "",       "" },
	{ SSC_OUTPUT, SSC_NUMBER,  "rc_m_dot_des",         "Recompressor mass flow rate",                            "kg/s",       "Recompressor",    "",      "*",     "",       "" },
	{ SSC_OUTPUT, SSC_NUMBER,  "rc_phi_des",           "Recompressor design flow coefficient",                   "",           "Recompressor",    "",      "*",     "",       "" },
    { SSC_OUTPUT, SSC_NUMBER,  "rc_psi_des",           "Recompressor design ideal head coefficient",             "",           "Recompressor",    "",      "*",     "",       "" },
    { SSC_OUTPUT, SSC_ARRAY,   "rc_tip_ratio_des",     "Recompressor design stage tip speed ratio",              "",           "Recompressor",    "",      "*",     "",       "" },
	{ SSC_OUTPUT, SSC_NUMBER,  "rc_n_stages",          "Recompressor stages",                                    "",           "Recompressor",    "",      "*",     "",       "" },
	{ SSC_OUTPUT, SSC_NUMBER,  "rc_N_des",             "Recompressor design shaft speed",                        "rpm",        "Recompressor",    "",      "*",     "",       "" },
	{ SSC_OUTPUT, SSC_ARRAY,   "rc_D",                 "Recompressor stage diameters",                           "m",          "Recompressor",    "",      "*",     "",       "" },
	{ SSC_OUTPUT, SSC_NUMBER,  "rc_phi_surge",         "Recompressor flow coefficient where surge occurs",       "",           "Recompressor",    "",      "*",     "",       "" },
    { SSC_OUTPUT, SSC_NUMBER,  "rc_psi_max_at_N_des",  "Recompressor max ideal head coefficient at design shaft speed", "",    "Recompressor",    "",      "*",     "",       "" },
    { SSC_OUTPUT, SSC_ARRAY,   "rc_eta_stages_des",    "Recompressor design stage isenstropic efficiencies",     "",           "Recompressor",    "",      "*",     "",       "" },
	{ SSC_OUTPUT, SSC_NUMBER,  "rc_cost",              "Recompressor cost",                                      "M$",         "Recompressor",    "",      "*",     "",       "" },
		// Precompressor	
	{ SSC_OUTPUT, SSC_NUMBER,  "pc_T_in_des",          "Precompressor inlet temperature",                        "C",          "Precompressor",    "",      "*",     "",       "" },
	{ SSC_OUTPUT, SSC_NUMBER,  "pc_P_in_des",          "Precompressor inlet pressure",                           "MPa",        "Precompressor",    "",      "*",     "",       "" },
	{ SSC_OUTPUT, SSC_NUMBER,  "pc_W_dot",             "Precompressor power",                                    "MWe",        "Precompressor",    "",      "*",     "",       "" },
	{ SSC_OUTPUT, SSC_NUMBER,  "pc_m_dot_des",         "Precompressor mass flow rate",                           "kg/s",       "Precompressor",    "",      "*",     "",       "" },
    { SSC_OUTPUT, SSC_NUMBER,  "pc_rho_in_des",        "Precompressor inlet density",                            "kg/m3",      "Precompressor",    "",      "*",     "",       "" },
    { SSC_OUTPUT, SSC_NUMBER,  "pc_ideal_spec_work_des", "Precompressor ideal spec work",                          "kJ/kg",      "Precompressor",    "",      "*",     "",       "" },
    { SSC_OUTPUT, SSC_NUMBER,  "pc_phi_des",           "Precompressor design flow coefficient",                  "",           "Precompressor",    "",      "*",     "",       "" },
	{ SSC_OUTPUT, SSC_ARRAY,   "pc_tip_ratio_des",     "Precompressor design stage tip speed ratio",             "",           "Precompressor",    "",      "*",     "",       "" },
	{ SSC_OUTPUT, SSC_NUMBER,  "pc_n_stages",          "Precompressor stages",                                   "",           "Precompressor",    "",      "*",     "",       "" },
	{ SSC_OUTPUT, SSC_NUMBER,  "pc_N_des",             "Precompressor design shaft speed",                       "rpm",        "Precompressor",    "",      "*",     "",       "" },
	{ SSC_OUTPUT, SSC_ARRAY,   "pc_D",                 "Precompressor stage diameters",                          "m",          "Precompressor",    "",      "*",     "",       "" },
	{ SSC_OUTPUT, SSC_NUMBER,  "pc_phi_surge",         "Precompressor flow coefficient where surge occurs",      "",           "Precompressor",    "",      "*",     "",       "" },
	{ SSC_OUTPUT, SSC_ARRAY,   "pc_eta_stages_des",    "Precompressor design stage isenstropic efficiencies",    "",           "Precompressor",    "",      "*",     "",       "" },
	{ SSC_OUTPUT, SSC_NUMBER,  "pc_cost",              "Precompressor cost",                                     "M$",         "Precompressor",    "",      "*",     "",       "" },
		// Compressor Totals	
	{ SSC_OUTPUT, SSC_NUMBER,   "c_tot_cost",          "Compressor total cost",                                  "M$",         "Compressor Totals",    "",      "*",     "",       "" },
	{ SSC_OUTPUT, SSC_NUMBER,   "c_tot_W_dot",         "Compressor total summed power",                          "MWe",        "Compressor Totals",    "",      "*",     "",       "" },
		// Turbine
	{ SSC_OUTPUT, SSC_NUMBER,  "t_W_dot",              "Turbine power",                                          "MWe",        "Turbine",    "",      "*",     "",       "" },
	{ SSC_OUTPUT, SSC_NUMBER,  "t_m_dot_des",          "Turbine mass flow rate",                                 "kg/s",       "Turbine",    "",      "*",     "",       "" },
	{ SSC_OUTPUT, SSC_NUMBER,  "T_turb_in",            "Turbine inlet temperature",                              "C",          "Turbine",    "",      "*",     "",       "" },
	{ SSC_OUTPUT, SSC_NUMBER,  "t_P_in_des",           "Turbine design inlet pressure",                          "MPa",        "Turbine",    "",      "*",     "",       "" },
	{ SSC_OUTPUT, SSC_NUMBER,  "t_T_out_des",          "Turbine outlet temperature",                             "C",          "Turbine",    "",      "*",     "",       "" },
	{ SSC_OUTPUT, SSC_NUMBER,  "t_P_out_des",          "Turbine design outlet pressure",                         "MPa",        "Turbine",    "",      "*",     "",       "" },
	{ SSC_OUTPUT, SSC_NUMBER,  "t_delta_h_isen_des",   "Turbine isentropic specific work",                       "kJ/kg",      "Turbine",    "",      "*",     "",       "" },
	{ SSC_OUTPUT, SSC_NUMBER,  "t_rho_in_des",         "Turbine inlet density",                                  "kg/m3",      "Turbine",    "",      "*",     "",       "" },
	{ SSC_OUTPUT, SSC_NUMBER,  "t_nu_des",             "Turbine design velocity ratio",                          "",           "Turbine",    "",      "*",     "",       "" },
	{ SSC_OUTPUT, SSC_NUMBER,  "t_tip_ratio_des",	   "Turbine design tip speed ratio",                         "",           "Turbine",    "",      "*",     "",       "" },
	{ SSC_OUTPUT, SSC_NUMBER,  "t_N_des",			   "Turbine design shaft speed",	                         "rpm",        "Turbine",    "",      "*",     "",       "" },
	{ SSC_OUTPUT, SSC_NUMBER,  "t_D",                  "Turbine diameter",                                       "m",          "Turbine",    "",      "*",     "",       "" },
	{ SSC_OUTPUT, SSC_NUMBER,  "t_cost",               "Tubine cost",                                            "M$",         "Turbine",    "",      "*",     "",       "" },
		// Recuperators																				 
	{ SSC_OUTPUT, SSC_NUMBER,  "recup_total_UA_assigned", "Total recuperator UA assigned to design routine",     "MW/K",       "Recuperators",    "",      "*",     "",       "" },
    { SSC_OUTPUT, SSC_NUMBER,  "recup_total_UA_calculated", "Total recuperator UA calculated considering max eff and/or min temp diff parameter", "MW/K",       "Recuperators",    "",      "*",     "",       "" },
    { SSC_OUTPUT, SSC_NUMBER,  "recup_total_cost",     "Total recuperator cost",                                 "M$",         "Recuperators",    "",      "*",     "",       "" },
	{ SSC_OUTPUT, SSC_NUMBER,  "recup_LTR_UA_frac",    "Fraction of total conductance to LTR",                   "",           "Recuperators",    "",      "*",     "",       "" },
	{ SSC_OUTPUT, SSC_NUMBER,  "LTR_HP_T_out_des",     "Low temp recuperator HP outlet temperature",             "C",          "Recuperators",    "",      "*",     "",       "" },
	{ SSC_OUTPUT, SSC_NUMBER,  "LTR_UA_assigned",      "Low temp recuperator UA assigned from total",            "MW/K",       "Recuperators",    "",      "*",     "",       "" },
    { SSC_OUTPUT, SSC_NUMBER,  "LTR_UA_calculated",    "Low temp recuperator UA calculated considering max eff and/or min temp diff parameter", "MW/K",       "Recuperators",    "",      "*",     "",       "" },
    { SSC_OUTPUT, SSC_NUMBER,  "eff_LTR",              "Low temp recuperator effectiveness",                     "",           "Recuperators",    "",      "*",     "",       "" },
	{ SSC_OUTPUT, SSC_NUMBER,  "NTU_LTR",              "Low temp recuperator NTU",                               "",           "Recuperators",    "",      "*",     "",       "" },
	{ SSC_OUTPUT, SSC_NUMBER,  "q_dot_LTR",            "Low temp recuperator heat transfer",                     "MWt",        "Recuperators",    "",      "*",     "",       "" },
	{ SSC_OUTPUT, SSC_NUMBER,  "LTR_LP_deltaP_des",    "Low temp recuperator low pressure design pressure drop", "-",          "Recuperators",    "",      "*",     "",       "" },
	{ SSC_OUTPUT, SSC_NUMBER,  "LTR_HP_deltaP_des",    "Low temp recuperator high pressure design pressure drop","-",          "Recuperators",    "",      "*",     "",       "" },
	{ SSC_OUTPUT, SSC_NUMBER,  "LTR_min_dT",           "Low temp recuperator min temperature difference",        "C",          "Recuperators",    "",      "*",     "",       "" },
	{ SSC_OUTPUT, SSC_NUMBER,  "LTR_cost",             "Low temp recuperator cost",                              "M$",         "Recuperators",    "",      "*",     "",       "" },
	{ SSC_OUTPUT, SSC_NUMBER,  "HTR_LP_T_out_des",     "High temp recuperator LP outlet temperature",            "C",          "Recuperators",    "",      "*",     "",       "" },
	{ SSC_OUTPUT, SSC_NUMBER,  "HTR_HP_T_in_des",      "High temp recuperator HP inlet temperature",             "C",          "Recuperators",    "",      "*",     "",       "" },
	{ SSC_OUTPUT, SSC_NUMBER,  "HTR_UA_assigned",      "High temp recuperator UA assigned from total",           "MW/K",       "Recuperators",    "",      "*",     "",       "" },
    { SSC_OUTPUT, SSC_NUMBER,  "HTR_UA_calculated",    "High temp recuperator UA calculated considering max eff and/or min temp diff parameter", "MW/K",       "Recuperators",    "",      "*",     "",       "" },
    { SSC_OUTPUT, SSC_NUMBER,  "eff_HTR",              "High temp recuperator effectiveness",                    "",           "Recuperators",    "",      "*",     "",       "" },
	{ SSC_OUTPUT, SSC_NUMBER,  "NTU_HTR",              "High temp recuperator NTRU",                             "",           "Recuperators",    "",      "*",     "",       "" },
	{ SSC_OUTPUT, SSC_NUMBER,  "q_dot_HTR",            "High temp recuperator heat transfer",                    "MWt",        "Recuperators",    "",      "*",     "",       "" },
	{ SSC_OUTPUT, SSC_NUMBER,  "HTR_LP_deltaP_des",    "High temp recuperator low pressure design pressure drop","-",          "Recuperators",    "",      "*",     "",       "" },
	{ SSC_OUTPUT, SSC_NUMBER,  "HTR_HP_deltaP_des",    "High temp recuperator high pressure design pressure drop","-",         "Recuperators",    "",      "*",     "",       "" },
	{ SSC_OUTPUT, SSC_NUMBER,  "HTR_min_dT",           "High temp recuperator min temperature difference",       "C",          "Recuperators",    "",      "*",     "",       "" },
	{ SSC_OUTPUT, SSC_NUMBER,  "HTR_cost",             "High temp recuperator cost",                             "M$",         "Recuperators",    "",      "*",     "",       "" },
		// PHX Design Solution
	{ SSC_OUTPUT, SSC_NUMBER,  "UA_PHX",               "PHX Conductance",                                        "MW/K",       "PHX Design Solution",    "",      "*",     "",       "" },
	{ SSC_OUTPUT, SSC_NUMBER,  "eff_PHX",              "PHX effectiveness",                                      "",           "PHX Design Solution",    "",      "*",     "",       "" },
	{ SSC_OUTPUT, SSC_NUMBER,  "NTU_PHX",              "PHX NTU",                                                "",           "PHX Design Solution",    "",      "*",     "",       "" },
	{ SSC_OUTPUT, SSC_NUMBER,  "T_co2_PHX_in",         "CO2 temperature at PHX inlet",                           "C",          "PHX Design Solution",    "",      "*",     "",       "" },
	{ SSC_OUTPUT, SSC_NUMBER,  "P_co2_PHX_in",         "CO2 pressure at PHX inlet",                              "MPa",        "PHX Design Solution",    "",      "*",     "",       "" },
	{ SSC_OUTPUT, SSC_NUMBER,  "deltaT_HTF_PHX",       "HTF temp difference across PHX",                         "C",          "PHX Design Solution",    "",      "*",     "",       "" },
	{ SSC_OUTPUT, SSC_NUMBER,  "q_dot_PHX",            "PHX heat transfer",                                      "MWt",        "PHX Design Solution",    "",      "*",     "",       "" },
	{ SSC_OUTPUT, SSC_NUMBER,  "PHX_co2_deltaP_des",   "PHX co2 side design pressure drop",                      "-",          "PHX Design Solution",    "",      "*",     "",       "" },
	{ SSC_OUTPUT, SSC_NUMBER,  "PHX_cost",             "PHX cost",                                               "M$",         "PHX Design Solution",    "",      "*",     "",       "" },
		// main compressor cooler
	{ SSC_OUTPUT, SSC_NUMBER,  "mc_cooler_T_in",       "Low pressure cross flow cooler inlet temperature",       "C",          "Low Pressure Cooler",    "",      "*",     "",       "" },
	{ SSC_OUTPUT, SSC_NUMBER,  "mc_cooler_P_in",       "Low pressure cross flow cooler inlet pressure",          "MPa",        "Low Pressure Cooler",    "",      "*",     "",       "" },
	{ SSC_OUTPUT, SSC_NUMBER,  "mc_cooler_rho_in",     "Low pressure cross flow cooler inlet density",           "kg/m3",      "Low Pressure Cooler",    "",      "*",     "",       "" },
	{ SSC_OUTPUT, SSC_NUMBER,  "mc_cooler_in_isen_deltah_to_P_mc_out", "Low pressure cross flow cooler inlet isen enthalpy rise to mc outlet pressure", "kJ/kg", "Low Pressure Cooler", "", "*", "", "" },
	{ SSC_OUTPUT, SSC_NUMBER,  "mc_cooler_m_dot_co2",  "Low pressure cross flow cooler CO2 mass flow rate",      "kg/s",       "Low Pressure Cooler",    "",      "*",     "",       "" },
	{ SSC_OUTPUT, SSC_NUMBER,  "mc_cooler_UA",         "Low pressure cross flow cooler conductance",             "MW/K",       "Low Pressure Cooler",    "",      "*",     "",       "" },
	{ SSC_OUTPUT, SSC_NUMBER,  "mc_cooler_q_dot",      "Low pressure cooler heat transfer",                      "MWt",        "Low Pressure Cooler",    "",      "*",     "",       "" },
	{ SSC_OUTPUT, SSC_NUMBER,  "mc_cooler_co2_deltaP_des","Low pressure cooler co2 side design pressure drop",   "-",          "Low Pressure Cooler",    "",      "*",     "",       "" },
	{ SSC_OUTPUT, SSC_NUMBER,  "mc_cooler_W_dot_fan",  "Low pressure cooler fan power",                          "MWe",        "Low Pressure Cooler",    "",      "*",     "",       "" },
	{ SSC_OUTPUT, SSC_NUMBER,  "mc_cooler_cost",       "Low pressure cooler cost",                               "M$",         "Low Pressure Cooler",    "",      "*",     "",       "" },
		// pre compressor cooler
	{ SSC_OUTPUT, SSC_NUMBER,  "pc_cooler_T_in",       "Intermediate pressure cross flow cooler inlet temperature",       "C",          "Intermediate Pressure Cooler",    "",      "*",     "",       "" },
	{ SSC_OUTPUT, SSC_NUMBER,  "pc_cooler_P_in",       "Intermediate pressure cross flow cooler inlet pressure",          "MPa",        "Intermediate Pressure Cooler",    "",      "*",     "",       "" },
	{ SSC_OUTPUT, SSC_NUMBER,  "pc_cooler_m_dot_co2",  "Intermediate pressure cross flow cooler CO2 mass flow rate",      "kg/s",       "Intermediate Pressure Cooler",    "",      "*",     "",       "" },
	{ SSC_OUTPUT, SSC_NUMBER,  "pc_cooler_UA",         "Intermediate pressure cross flow cooler conductance",             "MW/K",       "Intermediate Pressure Cooler",    "",      "*",     "",       "" },
	{ SSC_OUTPUT, SSC_NUMBER,  "pc_cooler_q_dot",      "Intermediate pressure cooler heat transfer",                      "MWt",        "Intermediate Pressure Cooler",    "",      "*",     "",       "" },
	{ SSC_OUTPUT, SSC_NUMBER,  "pc_cooler_W_dot_fan",  "Intermediate pressure cooler fan power",                          "MWe",        "Intermediate Pressure Cooler",    "",      "*",     "",       "" },
	{ SSC_OUTPUT, SSC_NUMBER,  "pc_cooler_cost",       "Intermediate pressure cooler cost",                               "M$",         "Intermediate Pressure Cooler",    "",      "*",     "",       "" },
		// Cooler Totals
	{ SSC_OUTPUT, SSC_NUMBER,  "cooler_tot_cost",      "Total cooler cost",                  "M$",        "Cooler Totals",   "",   "*",   "",   "" },
	{ SSC_OUTPUT, SSC_NUMBER,  "cooler_tot_UA",        "Total cooler conductance",           "MW/K",      "Cooler Totals",   "",   "*",   "",   "" },
	{ SSC_OUTPUT, SSC_NUMBER,  "cooler_tot_W_dot_fan", "Total cooler fan power",             "MWe",       "Cooler Totals",   "",   "*",   "",   "" },
		// State Points
	{ SSC_OUTPUT, SSC_ARRAY,  "T_state_points",       "Cycle temperature state points",      "C",	      "State Points",   "",   "*",   "",   "" },
	{ SSC_OUTPUT, SSC_ARRAY,  "P_state_points",       "Cycle pressure state points",         "MPa",       "State Points",   "",   "*",   "",   "" },
	{ SSC_OUTPUT, SSC_ARRAY,  "s_state_points",       "Cycle entropy state points",          "kJ/kg-K",   "State Points",   "",   "*",   "",   "" },
	{ SSC_OUTPUT, SSC_ARRAY,  "h_state_points",       "Cycle enthalpy state points",         "kJ/kg",     "State Points",   "",   "*",   "",   "" },
		// T-s plot data
	{ SSC_OUTPUT, SSC_ARRAY,  "T_LTR_HP_data",        "Temperature points along LTR HP stream",        "C",	       "T-s plot data",   "",   "*",   "",   "" },
	{ SSC_OUTPUT, SSC_ARRAY,  "s_LTR_HP_data",        "Entropy points along LTR HP stream",            "kJ/kg-K",    "T-s plot data",   "",   "*",   "",   "" },
	{ SSC_OUTPUT, SSC_ARRAY,  "T_HTR_HP_data",        "Temperature points along HTR HP stream",        "C",	       "T-s plot data",   "",   "*",   "",   "" },
	{ SSC_OUTPUT, SSC_ARRAY,  "s_HTR_HP_data",        "Entropy points along HTR HP stream",            "kJ/kg-K",    "T-s plot data",   "",   "*",   "",   "" },
	{ SSC_OUTPUT, SSC_ARRAY,  "T_PHX_data",           "Temperature points along PHX stream",           "C",	       "T-s plot data",   "",   "*",   "",   "" },
	{ SSC_OUTPUT, SSC_ARRAY,  "s_PHX_data",           "Entropy points along PHX stream",               "kJ/kg-K",    "T-s plot data",   "",   "*",   "",   "" },
	{ SSC_OUTPUT, SSC_ARRAY,  "T_HTR_LP_data",        "Temperature points along HTR LP stream",        "C",	       "T-s plot data",   "",   "*",   "",   "" },
	{ SSC_OUTPUT, SSC_ARRAY,  "s_HTR_LP_data",        "Entropy points along HTR LP stream",            "kJ/kg-K",    "T-s plot data",   "",   "*",   "",   "" },
	{ SSC_OUTPUT, SSC_ARRAY,  "T_LTR_LP_data",        "Temperature points along LTR LP stream",        "C",	       "T-s plot data",   "",   "*",   "",   "" },
	{ SSC_OUTPUT, SSC_ARRAY,  "s_LTR_LP_data",        "Entropy points along LTR LP stream",            "kJ/kg-K",    "T-s plot data",   "",   "*",   "",   "" },
	{ SSC_OUTPUT, SSC_ARRAY,  "T_main_cooler_data",   "Temperature points along main cooler stream",   "C",	       "T-s plot data",   "",   "*",   "",   "" },
	{ SSC_OUTPUT, SSC_ARRAY,  "s_main_cooler_data",   "Entropy points along main cooler stream",       "kJ/kg-K",    "T-s plot data",   "",   "*",   "",   "" },
	{ SSC_OUTPUT, SSC_ARRAY,  "T_pre_cooler_data",    "Temperature points along pre cooler stream",    "C",	       "T-s plot data",   "",   "*",   "",   "" },
	{ SSC_OUTPUT, SSC_ARRAY,  "s_pre_cooler_data",    "Entropy points along pre cooler stream",        "kJ/kg-K",    "T-s plot data",   "",   "*",   "",   "" },
		// P-h plot data
	{ SSC_OUTPUT, SSC_ARRAY,  "P_t_data",             "Pressure points along turbine expansion",       "MPa",	   "P-h plot data",   "",   "*",   "",   "" },
	{ SSC_OUTPUT, SSC_ARRAY,  "h_t_data",             "Enthalpy points along turbine expansion",       "kJ/kg",    "P-h plot data",   "",   "*",   "",   "" },
	{ SSC_OUTPUT, SSC_ARRAY,  "P_mc_data",            "Pressure points along main compression",        "MPa",	   "P-h plot data",   "",   "*",   "",   "" },
	{ SSC_OUTPUT, SSC_ARRAY,  "h_mc_data",            "Enthalpy points along main compression",        "kJ/kg",    "P-h plot data",   "",   "*",   "",   "" },
	{ SSC_OUTPUT, SSC_ARRAY,  "P_rc_data",            "Pressure points along re compression",          "MPa",	   "P-h plot data",   "",   "*",   "",   "" },
	{ SSC_OUTPUT, SSC_ARRAY,  "h_rc_data",            "Enthalpy points along re compression",          "kJ/kg",    "P-h plot data",   "",   "*",   "",   "" },
	{ SSC_OUTPUT, SSC_ARRAY,  "P_pc_data",            "Pressure points along pre compression",         "MPa",	   "P-h plot data",   "",   "*",   "",   "" },
	{ SSC_OUTPUT, SSC_ARRAY,  "h_pc_data",            "Enthalpy points along pre compression",         "kJ/kg",    "P-h plot data",   "",   "*",   "",   "" },

var_info_invalid };

int sco2_design_cmod_common(compute_module *cm, C_sco2_phx_air_cooler & c_sco2_cycle)
{
	C_sco2_phx_air_cooler::S_des_par s_sco2_des_par;
	// System design parameters
	s_sco2_des_par.m_hot_fl_code = cm->as_integer("htf");							//[-] Integer code for HTF
	s_sco2_des_par.mc_hot_fl_props = cm->as_matrix("htf_props");					//[-] Custom HTF properties
	s_sco2_des_par.m_T_htf_hot_in = cm->as_double("T_htf_hot_des") + 273.15;		//[K] Convert from C
	s_sco2_des_par.m_phx_dt_hot_approach = cm->as_double("dT_PHX_hot_approach");	//[K/C] Temperature difference between hot HTF and turbine CO2 inlet
	s_sco2_des_par.m_T_amb_des = cm->as_double("T_amb_des") + 273.15;				//[K] Convert from C
	s_sco2_des_par.m_dt_mc_approach = cm->as_double("dT_mc_approach");				//[K/C] Temperature difference between ambient air and main compressor inlet
	s_sco2_des_par.m_elevation = cm->as_double("site_elevation");					//[m] Site elevation
	s_sco2_des_par.m_W_dot_net = cm->as_double("W_dot_net_des")*1000.0;			//[kWe] Convert from MWe, cycle power output w/o cooling parasitics

    s_sco2_des_par.m_cycle_config = cm->as_integer("cycle_config");			//[-] 1 = recompression, 2 = partial cooling

    s_sco2_des_par.m_LTR_od_UA_target_type = static_cast<NS_HX_counterflow_eqs::E_UA_target_type>(cm->as_integer("LTR_od_model"));   // E_calc_UA;
    s_sco2_des_par.m_HTR_od_UA_target_type = static_cast<NS_HX_counterflow_eqs::E_UA_target_type>(cm->as_integer("HTR_od_model"));   // E_calc_UA;
    s_sco2_des_par.m_design_method = cm->as_integer("design_method");			//[-] 1 = Specify efficiency, 2 = Specify total recup UA, 3 = Specify each recup design
	if (s_sco2_des_par.m_design_method == 1)
	{
		s_sco2_des_par.m_LTR_target_code = 0;      // 0 = optimize, 1 = UA, 2 = min dT, 3 = effectiveness
		s_sco2_des_par.m_HTR_target_code = 0;		// 0 = optimize, 1 = UA, 2 = min dT, 3 = effectiveness

        s_sco2_des_par.m_eta_thermal = cm->as_double("eta_thermal_des");				//[-] Cycle thermal efficiency
		if (s_sco2_des_par.m_eta_thermal < 0.0)
		{
			cm->log("For cycle design method = 1, the input cycle thermal efficiency must be greater than 0", SSC_ERROR, -1.0);
			return -1;
		}
        s_sco2_des_par.m_UA_recup_tot_des = std::numeric_limits<double>::quiet_NaN();
	}
	else if (s_sco2_des_par.m_design_method == 2)
	{
		s_sco2_des_par.m_LTR_target_code = 0;      // 0 = optimize, 1 = UA, 2 = min dT, 3 = effectiveness
		s_sco2_des_par.m_HTR_target_code = 0;		// 0 = optimize, 1 = UA, 2 = min dT, 3 = effectiveness

        s_sco2_des_par.m_UA_recup_tot_des = cm->as_double("UA_recup_tot_des");		//[kW/K] Total recuperator conductance
		if (s_sco2_des_par.m_UA_recup_tot_des < 0.0)
		{
			cm->log("For cycle design method = 2, the input total recuperator conductance must be greater than 0", SSC_ERROR, -1.0);
			return -1;
		}
        s_sco2_des_par.m_eta_thermal = std::numeric_limits<double>::quiet_NaN();
	}
	else if (s_sco2_des_par.m_design_method == 3)
	{
		// LTR
		s_sco2_des_par.m_LTR_target_code = cm->as_integer("LTR_design_code");		// 0 = optimize, 1 = UA, 2 = min dT, 3 = effectiveness
		s_sco2_des_par.m_LTR_UA = cm->as_double("LTR_UA_des_in");					//[kW/K]
		s_sco2_des_par.m_LTR_min_dT = cm->as_double("LTR_min_dT_des_in");			//[C]
		s_sco2_des_par.m_LTR_eff_target = cm->as_double("LTR_eff_des_in");			//[-]

		// HTR
		s_sco2_des_par.m_HTR_target_code = cm->as_integer("HTR_design_code");		// 0 = optimize, 1 = UA, 2 = min dT, 3 = effectiveness
		s_sco2_des_par.m_HTR_UA = cm->as_double("HTR_UA_des_in");					//[kW/K]
		s_sco2_des_par.m_HTR_min_dT = cm->as_double("HTR_min_dT_des_in");			//[C]
		s_sco2_des_par.m_HTR_eff_target = cm->as_double("HTR_eff_des_in");			//[-]
	}
	else
	{
		std::string err_msg = util::format("The input cycle design method, %d, is invalid. It must be "
			" 1 = Specify efficiency, 2 = Specify total recup UA, 3 = Specify each recup design.", s_sco2_des_par.m_design_method);
		cm->log(err_msg, SSC_ERROR, -1.0);
	}

    s_sco2_des_par.m_is_recomp_ok = cm->as_double("is_recomp_ok");

	s_sco2_des_par.m_P_high_limit = cm->as_double("P_high_limit")*1000.0;		//[kPa], convert from MPa
	s_sco2_des_par.m_fixed_P_mc_out = cm->as_integer("is_P_high_fixed");		//[-]
	double mc_PR_in = cm->as_double("is_PR_fixed");		//[-]
	if (mc_PR_in != 0.0)
	{
		if (mc_PR_in < 0.0)     // mc_PR_in is in [MPa]
		{
            s_sco2_des_par.m_PR_HP_to_LP_guess = s_sco2_des_par.m_P_high_limit / (-mc_PR_in * 1.E3);		//[kPa] convert from MPa
		}
		else
		{
            s_sco2_des_par.m_PR_HP_to_LP_guess = mc_PR_in;			//[-] Pressure Ratio!
		}
        s_sco2_des_par.m_fixed_PR_HP_to_LP = true;
	}
	else
	{
		s_sco2_des_par.m_PR_HP_to_LP_guess = std::numeric_limits<double>::quiet_NaN();
		s_sco2_des_par.m_fixed_PR_HP_to_LP = false;
	}

    double PR_HP_to_IP_in = cm->as_double("is_IP_fixed");
    if (PR_HP_to_IP_in != 0.0)
    {
        if (!s_sco2_des_par.m_fixed_PR_HP_to_LP)
        {   // If HP to LP pressure ratio is not fixed, then don't fix HP to IP, regardless of input
            s_sco2_des_par.m_f_PR_HP_to_IP_guess = std::numeric_limits<double>::quiet_NaN();
            s_sco2_des_par.m_fixed_f_PR_HP_to_IP = false;
        }
        else
        {
            s_sco2_des_par.m_fixed_f_PR_HP_to_IP = true;
            double P_LP_in_local = s_sco2_des_par.m_P_high_limit / s_sco2_des_par.m_PR_HP_to_LP_guess;    //[kPa]
            double P_IP_in_local = fabs(PR_HP_to_IP_in)*1.E3;      //[kPa]
            if (PR_HP_to_IP_in > 0.0)
            {
                P_IP_in_local = s_sco2_des_par.m_P_high_limit / PR_HP_to_IP_in;  //[-]
            }
            s_sco2_des_par.m_f_PR_HP_to_IP_guess = (s_sco2_des_par.m_P_high_limit - P_IP_in_local) / (s_sco2_des_par.m_P_high_limit - P_LP_in_local);  //[kPa]
        }
    }
    else
    {
        s_sco2_des_par.m_f_PR_HP_to_IP_guess = std::numeric_limits<double>::quiet_NaN();
        s_sco2_des_par.m_fixed_f_PR_HP_to_IP = false;
    }

        // LTR pressure drops
	std::vector<double> DP_LT(2);
	/*(cold -hp-, hot -lp-) positive values are absolute [kPa], negative values are relative (-)*/
    if (cm->is_assigned("LTR_HP_deltaP_des_in"))
    {
        DP_LT[0] = -cm->as_double("LTR_HP_deltaP_des_in");      //[-]
    }
    else
    {
        DP_LT[0] = -cm->as_double("deltaP_counterHX_frac");		//[-]
    }
    if (cm->is_assigned("LTR_LP_deltaP_des_in"))
    {
        DP_LT[1] = -cm->as_double("LTR_LP_deltaP_des_in");      //[-]
    }
    else
    {
        DP_LT[1] = -cm->as_double("deltaP_counterHX_frac");		//[-]
    }

	    // HTR pressure drops
    std::vector<double> DP_HT(2);
    /*(cold, hot) positive values are absolute [kPa], negative values are relative (-)*/
    if (cm->is_assigned("HTR_HP_deltaP_des_in"))
    {
        DP_HT[0] = -cm->as_double("HTR_HP_deltaP_des_in");      //[-]
    }
    else
    {
        DP_HT[0] = -cm->as_double("deltaP_counterHX_frac");		//[-]
    }
    if (cm->is_assigned("HTR_LP_deltaP_des_in"))
    {
        DP_HT[1] = -cm->as_double("HTR_LP_deltaP_des_in");      //[-]
    }
    else
    {
        DP_HT[1] = -cm->as_double("deltaP_counterHX_frac");		//[-]
    }

        // PHX pressure drops
    std::vector<double> DP_PHX(2);
    /*(cold, hot) positive values are absolute [kPa], negative values are relative (-)*/
    DP_PHX[1] = 0;
    if (cm->is_assigned("PHX_co2_deltaP_des_in"))
    {
        DP_PHX[0] = -cm->as_double("PHX_co2_deltaP_des_in");	//[-]
    }
    else
    {
        DP_PHX[0] = -cm->as_double("deltaP_counterHX_frac");	//[-]
    }

	/*(cold, hot) positive values are absolute [kPa], negative values are relative (-)*/
	std::vector<double> DP_PC(2);
	DP_PC[0] = 0;
	DP_PC[1] = -cm->as_double("deltaP_cooler_frac");		//[-]
	
	s_sco2_des_par.m_DP_LT = DP_LT;
	s_sco2_des_par.m_DP_HT = DP_HT;
	s_sco2_des_par.m_DP_PC = DP_PC;
	s_sco2_des_par.m_DP_PHX = DP_PHX;

	// Choose shaft speed similar to design from Steve Wright's sco2 symposium presentation on test loop turbomachinery
    s_sco2_des_par.m_N_turbine = 30000.0;

	//sco2_rc_des_par.m_tol = 1.E-3;
	//sco2_rc_des_par.m_opt_tol = 1.E-3;
	s_sco2_des_par.m_des_tol = pow(10, -cm->as_double("rel_tol"));
	s_sco2_des_par.m_des_opt_tol = pow(10, -cm->as_double("rel_tol"));

	// Remaining cycle design parameters
        // LTR
    s_sco2_des_par.m_LTR_N_sub_hxrs = cm->as_integer("LTR_n_sub_hx");           //[-] 10;
    s_sco2_des_par.m_LTR_eff_max = cm->as_double("LT_recup_eff_max");  //[-]
	    // HTR
    s_sco2_des_par.m_HTR_N_sub_hxrs = cm->as_integer("HTR_n_sub_hx");           //[-] 10;
    s_sco2_des_par.m_HTR_eff_max = cm->as_double("HT_recup_eff_max");  //[-]
        // Turbomachinery
	s_sco2_des_par.m_eta_mc = cm->as_double("eta_isen_mc");		   //[-]

    int mc_comp_type = cm->as_integer("mc_comp_type"); //[-]
    if (mc_comp_type == 2)
    {
		throw exec_error("sco2_csp_system", "main compressor type 2 not available in this code base");
    }
    else
    {
        s_sco2_des_par.m_mc_comp_type = C_comp__psi_eta_vs_phi::E_snl_radial_via_Dyreby;
    }
    
    s_sco2_des_par.m_eta_rc = cm->as_double("eta_isen_rc");		   //[-]
	if (s_sco2_des_par.m_cycle_config == 2)
        s_sco2_des_par.m_eta_pc = cm->as_double("eta_isen_pc");		   //[-]
	else
        s_sco2_des_par.m_eta_pc = s_sco2_des_par.m_eta_mc;
    s_sco2_des_par.m_eta_t = cm->as_double("eta_isen_t");			   //[-]

	// PHX design parameters
	s_sco2_des_par.m_des_objective_type = cm->as_integer("des_objective");		//[-] 
	s_sco2_des_par.m_min_phx_deltaT = cm->as_double("min_phx_deltaT");			//[C]
	s_sco2_des_par.m_phx_dt_cold_approach = cm->as_double("dT_PHX_cold_approach");  //[C]
    s_sco2_des_par.m_phx_N_sub_hx = cm->as_integer("PHX_n_sub_hx");              //[-]
    s_sco2_des_par.m_phx_od_UA_target_type = static_cast<NS_HX_counterflow_eqs::E_UA_target_type>(cm->as_integer("PHX_od_model"));   // E_calc_UA;

	// Air cooler parameters
	s_sco2_des_par.m_is_des_air_cooler = cm->as_boolean("is_design_air_cooler");
	s_sco2_des_par.m_frac_fan_power = cm->as_double("fan_power_frac");         //[-]
	s_sco2_des_par.m_deltaP_cooler_frac = cm->as_double("deltaP_cooler_frac");	//[-]
    s_sco2_des_par.m_eta_fan = cm->as_double("eta_air_cooler_fan");     // 0.5;
    s_sco2_des_par.m_N_nodes_pass = cm->as_integer("N_nodes_air_cooler_pass");     // 10;

	// For try/catch below
	int out_type = -1;
	std::string out_msg = "";

	// Pass through callback function and pointer
	c_sco2_cycle.mf_callback_update = ssc_cmod_update;
	c_sco2_cycle.mp_mf_update = (void*)(cm);

	try
	{
		c_sco2_cycle.design(s_sco2_des_par);
	}
	catch (C_csp_exception &csp_exception)
	{
		// Report warning before exiting with error
		while (c_sco2_cycle.mc_messages.get_message(&out_type, &out_msg))
		{
			cm->log(out_msg + "\n");
			cm->log("\n");
		}

		throw exec_error("sco2_csp_system", csp_exception.m_error_message);
	}
	// If all calls were successful, log to SSC any messages from sco2_recomp_csp
	while (c_sco2_cycle.mc_messages.get_message(&out_type, &out_msg))
	{
		cm->log(out_msg + "\n");
	}

	// Helpful to know right away whether cycle contains recompressor
	bool is_rc = c_sco2_cycle.get_design_solved()->ms_rc_cycle_solved.m_is_rc;

	// Get data for P-h cycle plot
	std::vector<double> P_t;		//[MPa]
	std::vector<double> h_t;		//[kJ/kg]
	std::vector<double> P_mc;		//[MPa]
	std::vector<double> h_mc;		//[kJ/kg]
	std::vector<double> P_rc;		//[MPa]
	std::vector<double> h_rc;		//[kJ/kg]
	std::vector<double> P_pc;		//[MPa]
	std::vector<double> h_pc;		//[kJ/kg]
	int ph_err_code = sco2_cycle_plot_data_PH(s_sco2_des_par.m_cycle_config,
		c_sco2_cycle.get_design_solved()->ms_rc_cycle_solved.m_temp,
		c_sco2_cycle.get_design_solved()->ms_rc_cycle_solved.m_pres,
		P_t,
		h_t,
		P_mc,
		h_mc,
		P_rc,
		h_rc,
		P_pc,
		h_pc);

	if (ph_err_code != 0)
		throw exec_error("sco2_csp_system", "cycle plot data routine failed");

	size_t n_v = P_t.size();
	ssc_number_t *p_P_t_data = cm->allocate("P_t_data", n_v);
	ssc_number_t *p_h_t_data = cm->allocate("h_t_data", n_v);
	for (size_t i = 0; i < n_v; i++)
	{
		p_P_t_data[i] = (ssc_number_t)(P_t[i]);		//[MPa]
		p_h_t_data[i] = (ssc_number_t)(h_t[i]);		//[kJ/kg]
	}

	n_v = P_mc.size();
	ssc_number_t *p_P_mc_data = cm->allocate("P_mc_data", n_v);
	ssc_number_t *p_h_mc_data = cm->allocate("h_mc_data", n_v);
	for (size_t i = 0; i < n_v; i++)
	{
		p_P_mc_data[i] = (ssc_number_t)(P_mc[i]);		//[MPa]
		p_h_mc_data[i] = (ssc_number_t)(h_mc[i]);		//[kJ/kg]
	}

	n_v = P_rc.size();
	ssc_number_t *p_P_rc_data = cm->allocate("P_rc_data", n_v);
	ssc_number_t *p_h_rc_data = cm->allocate("h_rc_data", n_v);
	for (size_t i = 0; i < n_v; i++)
	{
		p_P_rc_data[i] = (ssc_number_t)(P_rc[i]);		//[MPa]
		p_h_rc_data[i] = (ssc_number_t)(h_rc[i]);		//[kJ/kg]
	}

	n_v = P_pc.size();
	ssc_number_t *p_P_pc_data = cm->allocate("P_pc_data", n_v);
	ssc_number_t *p_h_pc_data = cm->allocate("h_pc_data", n_v);
	for (size_t i = 0; i < n_v; i++)
	{
		p_P_pc_data[i] = (ssc_number_t)(P_pc[i]);		//[MPa]
		p_h_pc_data[i] = (ssc_number_t)(h_pc[i]);		//[kJ/kg]
	}

	// Get data for T-s cycle plot
	std::vector<double> T_LTR_HP;	//[C]
	std::vector<double> s_LTR_HP;	//[kJ/kg-K]
	std::vector<double> T_HTR_HP;	//[C]
	std::vector<double> s_HTR_HP;	//[kJ/kg-K]
	std::vector<double> T_PHX;		//[C]
	std::vector<double> s_PHX;		//[kJ/kg-K]
	std::vector<double> T_HTR_LP;	//[C]
	std::vector<double> s_HTR_LP;	//[kJ/kg-K]
	std::vector<double> T_LTR_LP;	//[C]
	std::vector<double> s_LTR_LP;	//[kJ/kg-K]
	std::vector<double> T_main_cooler;	//[C]
	std::vector<double> s_main_cooler;	//[kJ/kg-K]
	std::vector<double> T_pre_cooler;	//[C]
	std::vector<double> s_pre_cooler;	//[kJ/kg-K]
	int plot_data_err_code = sco2_cycle_plot_data_TS(s_sco2_des_par.m_cycle_config,
		c_sco2_cycle.get_design_solved()->ms_rc_cycle_solved.m_pres,
		c_sco2_cycle.get_design_solved()->ms_rc_cycle_solved.m_entr,
		T_LTR_HP,
		s_LTR_HP,
		T_HTR_HP,
		s_HTR_HP,
		T_PHX,
		s_PHX,
		T_HTR_LP,
		s_HTR_LP,
		T_LTR_LP,
		s_LTR_LP,
		T_main_cooler,
		s_main_cooler,
		T_pre_cooler,
		s_pre_cooler);

	if (plot_data_err_code != 0)
		throw exec_error("sco2_csp_system", "cycle plot data routine failed");

	n_v = T_LTR_HP.size();
	ssc_number_t *p_T_LTR_HP_data = cm->allocate("T_LTR_HP_data", n_v);
	ssc_number_t *p_s_LTR_HP_data = cm->allocate("s_LTR_HP_data", n_v);
	for (size_t i = 0; i < n_v; i++)
	{
		p_T_LTR_HP_data[i] = (ssc_number_t)(T_LTR_HP[i]);	//[C]
		p_s_LTR_HP_data[i] = (ssc_number_t)(s_LTR_HP[i]);	//[kJ/kg-K]
	}

	n_v = T_HTR_HP.size();
	ssc_number_t *p_T_HTR_HP_data = cm->allocate("T_HTR_HP_data", n_v);
	ssc_number_t *p_s_HTR_HP_data = cm->allocate("s_HTR_HP_data", n_v);
	for (size_t i = 0; i < n_v; i++)
	{
		p_T_HTR_HP_data[i] = (ssc_number_t)(T_HTR_HP[i]);		//[C]
		p_s_HTR_HP_data[i] = (ssc_number_t)(s_HTR_HP[i]);		//[kJ/kg-K]
	}

	n_v = T_PHX.size();
	ssc_number_t *p_T_PHX_data = cm->allocate("T_PHX_data", n_v);
	ssc_number_t *p_s_PHX_data = cm->allocate("s_PHX_data", n_v);
	for (size_t i = 0; i < n_v; i++)
	{
		p_T_PHX_data[i] = (ssc_number_t)(T_PHX[i]);			//[C]
		p_s_PHX_data[i] = (ssc_number_t)(s_PHX[i]);			//[kJ/kg-K]
	}

	n_v = T_HTR_LP.size();
	ssc_number_t *p_T_HTR_LP_data = cm->allocate("T_HTR_LP_data", n_v);
	ssc_number_t *p_s_HTR_LP_data = cm->allocate("s_HTR_LP_data", n_v);
	for (size_t i = 0; i < n_v; i++)
	{
		p_T_HTR_LP_data[i] = (ssc_number_t)(T_HTR_LP[i]);	//[C]
		p_s_HTR_LP_data[i] = (ssc_number_t)(s_HTR_LP[i]);	//[kJ/kg-K]
	}

	n_v = T_LTR_LP.size();
	ssc_number_t *p_T_LTR_LP_data = cm->allocate("T_LTR_LP_data", n_v);
	ssc_number_t *p_s_LTR_LP_data = cm->allocate("s_LTR_LP_data", n_v);
	for (size_t i = 0; i < n_v; i++)
	{
		p_T_LTR_LP_data[i] = (ssc_number_t)(T_LTR_LP[i]);	//[C]
		p_s_LTR_LP_data[i] = (ssc_number_t)(s_LTR_LP[i]);	//[kJ/kg-K]
	}

	n_v = T_main_cooler.size();
	ssc_number_t *p_T_main_cooler = cm->allocate("T_main_cooler_data", n_v);
	ssc_number_t *p_s_main_cooler = cm->allocate("s_main_cooler_data", n_v);
	for (size_t i = 0; i < n_v; i++)
	{
		p_T_main_cooler[i] = (ssc_number_t)(T_main_cooler[i]);	//[C]
		p_s_main_cooler[i] = (ssc_number_t)(s_main_cooler[i]);	//[kJ/kg-K]
	}

	n_v = T_pre_cooler.size();
	ssc_number_t *p_T_pre_cooler = cm->allocate("T_pre_cooler_data", n_v);
	ssc_number_t *p_s_pre_cooler = cm->allocate("s_pre_cooler_data", n_v);
	for (size_t i = 0; i < n_v; i++)
	{
		p_T_pre_cooler[i] = (ssc_number_t)(T_pre_cooler[i]);	//[C]
		p_s_pre_cooler[i] = (ssc_number_t)(s_pre_cooler[i]);	//[kJ/kg-K]
	}

	// Set SSC design outputs
	// System
	double cost_sum = 0.0;		 //[M$]
	double comp_cost_sum = 0.0;	 //[M$]
	double comp_power_sum = 0.0; //[MWe]
	double m_dot_htf_design = c_sco2_cycle.get_phx_des_par()->m_m_dot_hot_des;	//[kg/s]
	double T_htf_cold_calc = c_sco2_cycle.get_design_solved()->ms_phx_des_solved.m_T_h_out;		//[K]
	cm->assign("T_htf_cold_des", (ssc_number_t)(T_htf_cold_calc - 273.15));		//[C] convert from K
	cm->assign("m_dot_htf_des", (ssc_number_t)m_dot_htf_design);				//[kg/s]
	cm->assign("eta_thermal_calc", (ssc_number_t)c_sco2_cycle.get_design_solved()->ms_rc_cycle_solved.m_eta_thermal);	//[-]
	cm->assign("m_dot_co2_full", (ssc_number_t)c_sco2_cycle.get_design_solved()->ms_rc_cycle_solved.m_m_dot_t);		//[kg/s]
	cm->assign("recomp_frac", (ssc_number_t)c_sco2_cycle.get_design_solved()->ms_rc_cycle_solved.m_recomp_frac);		//[-]
	// Compressor
	cm->assign("T_comp_in", (ssc_number_t)(c_sco2_cycle.get_design_solved()->ms_rc_cycle_solved.m_temp[C_sco2_cycle_core::MC_IN] - 273.15));		//[C] convert from K
	cm->assign("P_comp_in", (ssc_number_t)(c_sco2_cycle.get_design_solved()->ms_rc_cycle_solved.m_pres[C_sco2_cycle_core::MC_IN] / 1000.0));		//[MPa] convert from kPa
	cm->assign("P_comp_out", (ssc_number_t)(c_sco2_cycle.get_design_solved()->ms_rc_cycle_solved.m_pres[C_sco2_cycle_core::MC_OUT] / 1000.0));		//[MPa] convert from kPa
	cm->assign("mc_T_out", (ssc_number_t)(c_sco2_cycle.get_design_solved()->ms_rc_cycle_solved.m_temp[C_sco2_cycle_core::MC_OUT] - 273.15));		//[C] convert from K
	cm->assign("mc_W_dot", (ssc_number_t)(-c_sco2_cycle.get_design_solved()->ms_rc_cycle_solved.m_W_dot_mc*1.E-3));	//[MWe] convert kWe
	cm->assign("mc_rho_in", (ssc_number_t)(c_sco2_cycle.get_design_solved()->ms_rc_cycle_solved.m_dens[C_sco2_cycle_core::MC_IN]));	//[kg/m3]
	cm->assign("mc_ideal_spec_work", (ssc_number_t)c_sco2_cycle.get_design_solved()->ms_rc_cycle_solved.ms_mc_ms_des_solved.m_isen_spec_work);	//[kJ/kg]
	cm->assign("mc_m_dot_des", (ssc_number_t)c_sco2_cycle.get_design_solved()->ms_rc_cycle_solved.m_m_dot_t*(1.0 - c_sco2_cycle.get_design_solved()->ms_rc_cycle_solved.m_recomp_frac));	//[kg/s]
	cm->assign("mc_phi_des", (ssc_number_t)c_sco2_cycle.get_design_solved()->ms_rc_cycle_solved.ms_mc_ms_des_solved.m_phi_des);     //[-]
    cm->assign("mc_psi_des", (ssc_number_t)c_sco2_cycle.get_design_solved()->ms_rc_cycle_solved.ms_mc_ms_des_solved.m_psi_des);     //[-] ideal head coefficient
    cm->assign("mc_tip_ratio_des", (ssc_number_t)c_sco2_cycle.get_design_solved()->ms_rc_cycle_solved.ms_mc_ms_des_solved.m_tip_ratio_max);		//[-]

	int n_mc_stages = c_sco2_cycle.get_design_solved()->ms_rc_cycle_solved.ms_mc_ms_des_solved.m_n_stages;
	cm->assign("mc_n_stages", (ssc_number_t)n_mc_stages);	//[-]
	cm->assign("mc_N_des", (ssc_number_t)c_sco2_cycle.get_design_solved()->ms_rc_cycle_solved.ms_mc_ms_des_solved.m_N_design);	//[rpm]
    cm->assign("mc_psi_max_at_N_des", (ssc_number_t)c_sco2_cycle.get_design_solved()->ms_rc_cycle_solved.ms_mc_ms_des_solved.m_psi_max_at_N_des);    //[-] Max ideal head coefficient at design shaft speed

	ssc_number_t *p_mc_D = cm->allocate("mc_D", n_mc_stages);
	ssc_number_t *p_mc_tip_ratio_des = cm->allocate("mc_tip_ratio_des", n_mc_stages);
	ssc_number_t *p_mc_eta_stages_des = cm->allocate("mc_eta_stages_des", n_mc_stages);
	std::vector<double> v_mc_D = c_sco2_cycle.get_design_solved()->ms_rc_cycle_solved.ms_mc_ms_des_solved.mv_D;	//[m]
	std::vector<double> v_mc_tip_ratio_des = c_sco2_cycle.get_design_solved()->ms_rc_cycle_solved.ms_mc_ms_des_solved.mv_tip_speed_ratio;	//[-]
	std::vector<double> v_mc_eta_stages_des = c_sco2_cycle.get_design_solved()->ms_rc_cycle_solved.ms_mc_ms_des_solved.mv_eta_stages;		//[-]
	for (int i = 0; i < n_mc_stages; i++)
	{
		p_mc_D[i] = (ssc_number_t)v_mc_D[i];		//[m]
		p_mc_tip_ratio_des[i] = (ssc_number_t)v_mc_tip_ratio_des[i];	//[-]
		p_mc_eta_stages_des[i] = (ssc_number_t)v_mc_eta_stages_des[i];	//[-]
	}

	cm->assign("mc_phi_surge", (ssc_number_t)c_sco2_cycle.get_design_solved()->ms_rc_cycle_solved.ms_mc_ms_des_solved.m_phi_surge);	//[-]
	cm->assign("mc_cost", (ssc_number_t)c_sco2_cycle.get_design_solved()->ms_rc_cycle_solved.ms_mc_ms_des_solved.m_cost);		//[M$]
	cost_sum += c_sco2_cycle.get_design_solved()->ms_rc_cycle_solved.ms_mc_ms_des_solved.m_cost;		//[M$]
	comp_cost_sum += c_sco2_cycle.get_design_solved()->ms_rc_cycle_solved.ms_mc_ms_des_solved.m_cost;	//[M$]
	comp_power_sum += -c_sco2_cycle.get_design_solved()->ms_rc_cycle_solved.m_W_dot_mc*1.E-3;			//[MWe]

	// Recompressor
	cm->assign("rc_W_dot", (ssc_number_t)(-c_sco2_cycle.get_design_solved()->ms_rc_cycle_solved.m_W_dot_rc*1.E-3));	//[MWe] convert from kWe
	cm->assign("rc_m_dot_des", (ssc_number_t)c_sco2_cycle.get_design_solved()->ms_rc_cycle_solved.m_m_dot_t*c_sco2_cycle.get_design_solved()->ms_rc_cycle_solved.m_recomp_frac);	//[kg/s]
	int n_rc_stages = 0;
	if (is_rc)
	{
		cm->assign("rc_T_in_des", (ssc_number_t)(c_sco2_cycle.get_design_solved()->ms_rc_cycle_solved.ms_rc_ms_des_solved.m_T_in - 273.15));	//[C]
		cm->assign("rc_P_in_des", (ssc_number_t)(c_sco2_cycle.get_design_solved()->ms_rc_cycle_solved.ms_rc_ms_des_solved.m_P_in*1.E-3));		//[MPa]
		cm->assign("rc_T_out_des", (ssc_number_t)(c_sco2_cycle.get_design_solved()->ms_rc_cycle_solved.ms_rc_ms_des_solved.m_T_out - 273.15));	//[C]
		cm->assign("rc_P_out_des", (ssc_number_t)(c_sco2_cycle.get_design_solved()->ms_rc_cycle_solved.ms_rc_ms_des_solved.m_P_out*1.E-3));		//[MPa]
		cm->assign("rc_phi_des", (ssc_number_t)c_sco2_cycle.get_design_solved()->ms_rc_cycle_solved.ms_rc_ms_des_solved.m_phi_des);	//[-]
        cm->assign("rc_psi_des", (ssc_number_t)c_sco2_cycle.get_design_solved()->ms_rc_cycle_solved.ms_rc_ms_des_solved.m_psi_des);	//[-]

		n_rc_stages = c_sco2_cycle.get_design_solved()->ms_rc_cycle_solved.ms_rc_ms_des_solved.m_n_stages;		//[-]
		cm->assign("rc_n_stages", (ssc_number_t)n_rc_stages);	//[-]
		cm->assign("rc_N_des", (ssc_number_t)c_sco2_cycle.get_design_solved()->ms_rc_cycle_solved.ms_rc_ms_des_solved.m_N_design);	//[rpm]
        cm->assign("rc_psi_max_at_N_des", (ssc_number_t)c_sco2_cycle.get_design_solved()->ms_rc_cycle_solved.ms_rc_ms_des_solved.m_psi_max_at_N_des);    //[-] Max ideal head coefficient at design shaft speed

		ssc_number_t *p_rc_D = cm->allocate("rc_D", n_rc_stages);
		ssc_number_t *p_rc_tip_ratio_des = cm->allocate("rc_tip_ratio_des", n_rc_stages);
		ssc_number_t *p_rc_eta_stages_des = cm->allocate("rc_eta_stages_des", n_rc_stages);
		std::vector<double> v_rc_D = c_sco2_cycle.get_design_solved()->ms_rc_cycle_solved.ms_rc_ms_des_solved.mv_D;
		std::vector<double> v_rc_tip_ratio_des = c_sco2_cycle.get_design_solved()->ms_rc_cycle_solved.ms_rc_ms_des_solved.mv_tip_speed_ratio;	//[-]
		std::vector<double> v_rc_eta_stages_des = c_sco2_cycle.get_design_solved()->ms_rc_cycle_solved.ms_rc_ms_des_solved.mv_eta_stages;	//[-]
		for (int i = 0; i < n_rc_stages; i++)
		{
			p_rc_D[i] = (ssc_number_t)v_rc_D[i];		//[m]
			p_rc_tip_ratio_des[i] = (ssc_number_t)v_rc_tip_ratio_des[i];	//[-]
			p_rc_eta_stages_des[i] = (ssc_number_t)v_rc_eta_stages_des[i];	//[-]
		}
		cm->assign("rc_phi_surge", (ssc_number_t)c_sco2_cycle.get_design_solved()->ms_rc_cycle_solved.ms_rc_ms_des_solved.m_phi_surge);//[-]
		cm->assign("rc_cost", (ssc_number_t)c_sco2_cycle.get_design_solved()->ms_rc_cycle_solved.ms_rc_ms_des_solved.m_cost);	//[M$]
		cost_sum += c_sco2_cycle.get_design_solved()->ms_rc_cycle_solved.ms_rc_ms_des_solved.m_cost;	//[M$]
		comp_cost_sum += c_sco2_cycle.get_design_solved()->ms_rc_cycle_solved.ms_rc_ms_des_solved.m_cost;	//[M$]
		comp_power_sum += -c_sco2_cycle.get_design_solved()->ms_rc_cycle_solved.m_W_dot_rc*1.E-3;		//[MWe]
	}
	else
	{
		double ssc_nan = std::numeric_limits<ssc_number_t>::quiet_NaN();
		cm->assign("rc_T_in_des", ssc_nan);
		cm->assign("rc_P_in_des", ssc_nan);
		cm->assign("rc_T_out_des", ssc_nan);
		cm->assign("rc_P_out_des", ssc_nan);
		cm->assign("rc_phi_des", ssc_nan);
        cm->assign("rc_psi_des", ssc_nan);
        cm->assign("rc_n_stages", n_rc_stages);
		cm->assign("rc_N_des", ssc_nan);
        cm->assign("rc_psi_max_at_N_des", ssc_nan);
		ssc_number_t *p_rc_D = cm->allocate("rc_D", 1);
		p_rc_D[0] = ssc_nan;
		ssc_number_t *p_rc_tip_ratio_des = cm->allocate("rc_tip_ratio_des", 1);
		p_rc_tip_ratio_des[0] = ssc_nan;
		ssc_number_t *p_rc_eta_stages_des = cm->allocate("rc_eta_stages_des", 1);
		p_rc_eta_stages_des[0] = ssc_nan;
		cm->assign("rc_phi_surge", ssc_nan);
		cm->assign("rc_cost", ssc_nan);
	}

	// Precompressor		
	cm->assign("pc_W_dot", (ssc_number_t)(-c_sco2_cycle.get_design_solved()->ms_rc_cycle_solved.m_W_dot_pc*1.E-3));	//[MWe] convert from kWe
	cm->assign("pc_m_dot_des", (ssc_number_t)(c_sco2_cycle.get_design_solved()->ms_rc_cycle_solved.m_m_dot_pc));		//[kg/s]
	int n_pc_stages = 0;
	if (s_sco2_des_par.m_cycle_config == 2)
	{
		cm->assign("pc_T_in_des", (ssc_number_t)(c_sco2_cycle.get_design_solved()->ms_rc_cycle_solved.ms_pc_ms_des_solved.m_T_in - 273.15));
		cm->assign("pc_P_in_des", (ssc_number_t)(c_sco2_cycle.get_design_solved()->ms_rc_cycle_solved.ms_pc_ms_des_solved.m_P_in*1.E-3));
        cm->assign("pc_rho_in_des", (ssc_number_t)(c_sco2_cycle.get_design_solved()->ms_rc_cycle_solved.m_dens[C_sco2_cycle_core::PC_IN])); //[kg/m3]
        cm->assign("pc_ideal_spec_work_des", (ssc_number_t)(c_sco2_cycle.get_design_solved()->ms_rc_cycle_solved.ms_pc_ms_des_solved.m_isen_spec_work));    //[kJ/kg]
		cm->assign("pc_phi_des", (ssc_number_t)(c_sco2_cycle.get_design_solved()->ms_rc_cycle_solved.ms_pc_ms_des_solved.m_phi_des)); //[-]

		n_pc_stages = c_sco2_cycle.get_design_solved()->ms_rc_cycle_solved.ms_pc_ms_des_solved.m_n_stages;		//[-]
		cm->assign("pc_n_stages", (ssc_number_t)n_pc_stages);	//[-]
		cm->assign("pc_N_des", (ssc_number_t)c_sco2_cycle.get_design_solved()->ms_rc_cycle_solved.ms_pc_ms_des_solved.m_N_design);	//[rpm]

		ssc_number_t *p_pc_D = cm->allocate("pc_D", n_pc_stages);
		ssc_number_t *p_pc_tip_ratio_des = cm->allocate("pc_tip_ratio_des", n_pc_stages);
		ssc_number_t *p_pc_eta_stages_des = cm->allocate("pc_eta_stages_des", n_pc_stages);
		std::vector<double> v_pc_D = c_sco2_cycle.get_design_solved()->ms_rc_cycle_solved.ms_pc_ms_des_solved.mv_D;	//[m]
		std::vector<double> v_pc_tip_ratio_des = c_sco2_cycle.get_design_solved()->ms_rc_cycle_solved.ms_pc_ms_des_solved.mv_tip_speed_ratio;	//[-]
		std::vector<double> v_pc_eta_stages_des = c_sco2_cycle.get_design_solved()->ms_rc_cycle_solved.ms_pc_ms_des_solved.mv_eta_stages;		//[-]
		for (int i = 0; i < n_pc_stages; i++)
		{
			p_pc_D[i] = (ssc_number_t)v_pc_D[i];	//[m]
			p_pc_tip_ratio_des[i] = (ssc_number_t)v_pc_tip_ratio_des[i];		//[-]
			p_pc_eta_stages_des[i] = (ssc_number_t)v_pc_eta_stages_des[i];		//[-]
		}
		cm->assign("pc_phi_surge", (ssc_number_t)c_sco2_cycle.get_design_solved()->ms_rc_cycle_solved.ms_pc_ms_des_solved.m_phi_surge);	//[-]
		cm->assign("pc_cost", (ssc_number_t)c_sco2_cycle.get_design_solved()->ms_rc_cycle_solved.ms_pc_ms_des_solved.m_cost);	//[M$]
		cost_sum += c_sco2_cycle.get_design_solved()->ms_rc_cycle_solved.ms_pc_ms_des_solved.m_cost;	//[M$]
		comp_cost_sum += c_sco2_cycle.get_design_solved()->ms_rc_cycle_solved.ms_pc_ms_des_solved.m_cost;	//[M$]
		comp_power_sum += -c_sco2_cycle.get_design_solved()->ms_rc_cycle_solved.m_W_dot_pc*1.E-3;		//[MWe]
	}
	else
	{
		double ssc_nan = std::numeric_limits<ssc_number_t>::quiet_NaN();
		cm->assign("pc_T_in_des", ssc_nan);
		cm->assign("pc_P_in_des", ssc_nan);
        cm->assign("pc_rho_in_des", ssc_nan);
        cm->assign("pc_ideal_spec_work_des", ssc_nan);
		cm->assign("pc_phi_des", ssc_nan);
		cm->assign("pc_n_stages", ssc_nan);
		cm->assign("pc_N_des", ssc_nan);

		ssc_number_t *p_pc_D = cm->allocate("pc_D", 1);
		p_pc_D[0] = ssc_nan;
		ssc_number_t *p_pc_tip_ratio_des = cm->allocate("pc_tip_ratio_des", 1);
		p_pc_tip_ratio_des[0] = ssc_nan;
		ssc_number_t *p_pc_eta_stages_des = cm->allocate("pc_eta_stages_des", 1);
		p_pc_eta_stages_des[0] = ssc_nan;

		cm->assign("pc_phi_surge", ssc_nan);
		cm->assign("pc_cost", ssc_nan);
	}
	// Compressor Totals
	cm->assign("c_tot_cost", comp_cost_sum);		//[M$]
	cm->assign("c_tot_W_dot", comp_power_sum);		//[MWe]
	// Turbine
	cm->assign("t_W_dot", (ssc_number_t)(c_sco2_cycle.get_design_solved()->ms_rc_cycle_solved.m_W_dot_t*1.E-3));	//[MWe] convert from kWe
	cm->assign("t_m_dot_des", (ssc_number_t)c_sco2_cycle.get_design_solved()->ms_rc_cycle_solved.m_m_dot_t);		//[kg/s]
	cm->assign("T_turb_in", (ssc_number_t)(c_sco2_cycle.get_design_solved()->ms_rc_cycle_solved.m_temp[C_sco2_cycle_core::TURB_IN] - 273.15));	//[C] Turbine inlet temp, convert from K
	cm->assign("t_P_in_des", (ssc_number_t)(c_sco2_cycle.get_design_solved()->ms_rc_cycle_solved.m_pres[C_sco2_cycle_core::TURB_IN] * 1.E-3));	//[MPa] Turbine inlet pressure, convert from kPa
	cm->assign("t_T_out_des", (ssc_number_t)(c_sco2_cycle.get_design_solved()->ms_rc_cycle_solved.m_temp[C_sco2_cycle_core::TURB_OUT] - 273.15)); //[C] Turbine outlet temp, convert from K
	cm->assign("t_P_out_des", (ssc_number_t)(c_sco2_cycle.get_design_solved()->ms_rc_cycle_solved.m_pres[C_sco2_cycle_core::TURB_OUT] * 1.E-3));	//[MPa] Turbine outlet pressure, convert from kPa
	cm->assign("t_delta_h_isen_des", (ssc_number_t)(c_sco2_cycle.get_design_solved()->ms_rc_cycle_solved.ms_t_des_solved.m_delta_h_isen));	//[kJ/kg]
	cm->assign("t_rho_in_des", (ssc_number_t)(c_sco2_cycle.get_design_solved()->ms_rc_cycle_solved.ms_t_des_solved.m_rho_in));	//[kg/m3]
	cm->assign("t_nu_des", (ssc_number_t)c_sco2_cycle.get_design_solved()->ms_rc_cycle_solved.ms_t_des_solved.m_nu_design);           //[-]
	cm->assign("t_tip_ratio_des", (ssc_number_t)c_sco2_cycle.get_design_solved()->ms_rc_cycle_solved.ms_t_des_solved.m_w_tip_ratio);  //[-]
	cm->assign("t_N_des", (ssc_number_t)c_sco2_cycle.get_design_solved()->ms_rc_cycle_solved.ms_t_des_solved.m_N_design);			   //[rpm]
	cm->assign("t_D", (ssc_number_t)c_sco2_cycle.get_design_solved()->ms_rc_cycle_solved.ms_t_des_solved.m_D_rotor);                  //[m]
	cm->assign("t_cost", (ssc_number_t)c_sco2_cycle.get_design_solved()->ms_rc_cycle_solved.ms_t_des_solved.m_cost);			//[M$]
	cost_sum += c_sco2_cycle.get_design_solved()->ms_rc_cycle_solved.ms_t_des_solved.m_cost;			//[M$]
		// Recuperator
	double recup_total_UA_assigned = c_sco2_cycle.get_design_solved()->ms_rc_cycle_solved.ms_LTR_des_solved.m_UA_allocated*1.E-3;	//[MW/K] convert from kW/K
    double recup_total_UA_calculated = c_sco2_cycle.get_design_solved()->ms_rc_cycle_solved.ms_LTR_des_solved.m_UA_calc_at_eff_max*1.E-3;	//[MW/K] convert from kW/K
    double recup_total_cost = c_sco2_cycle.get_design_solved()->ms_rc_cycle_solved.ms_LTR_des_solved.m_cost;					//[M$]
			// Low-temp
	cm->assign("LTR_HP_T_out_des", (ssc_number_t)(c_sco2_cycle.get_design_solved()->ms_rc_cycle_solved.m_temp[C_sco2_cycle_core::LTR_HP_OUT] - 273.15));	//[C] LTR HP stream outlet temp, convert from K
	cm->assign("LTR_UA_assigned", (ssc_number_t)(c_sco2_cycle.get_design_solved()->ms_rc_cycle_solved.ms_LTR_des_solved.m_UA_allocated*1.E-3));	//[MW/K] convert from kW/K
    cm->assign("LTR_UA_calculated", (ssc_number_t)(c_sco2_cycle.get_design_solved()->ms_rc_cycle_solved.ms_LTR_des_solved.m_UA_calc_at_eff_max*1.E-3));	//[MW/K] convert from kW/K
    cm->assign("eff_LTR", (ssc_number_t)c_sco2_cycle.get_design_solved()->ms_rc_cycle_solved.ms_LTR_des_solved.m_eff_design);		//[-]
	cm->assign("NTU_LTR", (ssc_number_t)c_sco2_cycle.get_design_solved()->ms_rc_cycle_solved.ms_LTR_des_solved.m_NTU_design);		//[-]
	cm->assign("q_dot_LTR", (ssc_number_t)(c_sco2_cycle.get_design_solved()->ms_rc_cycle_solved.ms_LTR_des_solved.m_Q_dot_design*1.E-3));	//[MWt] convert from kWt
	double LTR_LP_deltaP_frac = 1.0 - c_sco2_cycle.get_design_solved()->ms_rc_cycle_solved.m_pres[C_sco2_cycle_core::LTR_LP_OUT] / 
									c_sco2_cycle.get_design_solved()->ms_rc_cycle_solved.m_pres[C_sco2_cycle_core::HTR_LP_OUT];   //[-] Fractional pressure drop through LP side of LTR
	cm->assign("LTR_LP_deltaP_des", (ssc_number_t)LTR_LP_deltaP_frac);		//[-]
	double LTR_HP_deltaP_frac = 1.0 - c_sco2_cycle.get_design_solved()->ms_rc_cycle_solved.m_pres[C_sco2_cycle_core::LTR_HP_OUT] /
		c_sco2_cycle.get_design_solved()->ms_rc_cycle_solved.m_pres[C_sco2_cycle_core::MC_OUT];   //[-] Fractional pressure drop through HP side of LTR
	cm->assign("LTR_HP_deltaP_des", (ssc_number_t)LTR_HP_deltaP_frac);		//[-]
	cm->assign("LTR_min_dT", (ssc_number_t)c_sco2_cycle.get_design_solved()->ms_rc_cycle_solved.ms_LTR_des_solved.m_min_DT_design);	//[C/K]
	cm->assign("LTR_cost", (ssc_number_t)c_sco2_cycle.get_design_solved()->ms_rc_cycle_solved.ms_LTR_des_solved.m_cost);			//[M$]
	cost_sum += c_sco2_cycle.get_design_solved()->ms_rc_cycle_solved.ms_LTR_des_solved.m_cost;		//[M$]
			// High-temp
	if (is_rc)
	{
		recup_total_UA_assigned += c_sco2_cycle.get_design_solved()->ms_rc_cycle_solved.ms_HTR_des_solved.m_UA_allocated*1.E-3;	//[MW/K] convert from kW/K
        recup_total_UA_calculated += c_sco2_cycle.get_design_solved()->ms_rc_cycle_solved.ms_HTR_des_solved.m_UA_calc_at_eff_max*1.E-3;	//[MW/K] convert from kW/K
        cm->assign("HTR_LP_T_out_des", (ssc_number_t)(c_sco2_cycle.get_design_solved()->ms_rc_cycle_solved.m_temp[C_sco2_cycle_core::HTR_LP_OUT] - 273.15));	//[C] HTR LP stream outlet temp, convert from K
		cm->assign("HTR_HP_T_in_des", (ssc_number_t)(c_sco2_cycle.get_design_solved()->ms_rc_cycle_solved.m_temp[C_sco2_cycle_core::MIXER_OUT] - 273.15));		//[C] HTR HP stream inlet temp, convert from K
		cm->assign("HTR_UA_assigned", (ssc_number_t)(c_sco2_cycle.get_design_solved()->ms_rc_cycle_solved.ms_HTR_des_solved.m_UA_allocated*1.E-3));	//[MW/K] convert from kW/K
        cm->assign("HTR_UA_calculated", (ssc_number_t)(c_sco2_cycle.get_design_solved()->ms_rc_cycle_solved.ms_HTR_des_solved.m_UA_calc_at_eff_max*1.E-3));	//[MW/K] convert from kW/K
        cm->assign("eff_HTR", (ssc_number_t)c_sco2_cycle.get_design_solved()->ms_rc_cycle_solved.ms_HTR_des_solved.m_eff_design);		//[-]
		cm->assign("NTU_HTR", (ssc_number_t)c_sco2_cycle.get_design_solved()->ms_rc_cycle_solved.ms_HTR_des_solved.m_NTU_design);		//[-]
		cm->assign("q_dot_HTR", (ssc_number_t)(c_sco2_cycle.get_design_solved()->ms_rc_cycle_solved.ms_HTR_des_solved.m_Q_dot_design*1.E-3));	//[MWt] convert from kWt
		cm->assign("HTR_min_dT", (ssc_number_t)c_sco2_cycle.get_design_solved()->ms_rc_cycle_solved.ms_HTR_des_solved.m_min_DT_design);	//[C/K]
		cm->assign("HTR_cost", (ssc_number_t)c_sco2_cycle.get_design_solved()->ms_rc_cycle_solved.ms_HTR_des_solved.m_cost);			//[M$]
		cost_sum += c_sco2_cycle.get_design_solved()->ms_rc_cycle_solved.ms_HTR_des_solved.m_cost;			//[M$]
		recup_total_cost += c_sco2_cycle.get_design_solved()->ms_rc_cycle_solved.ms_HTR_des_solved.m_cost;			//[M$]
		cm->assign("recup_LTR_UA_frac", (ssc_number_t((c_sco2_cycle.get_design_solved()->ms_rc_cycle_solved.ms_LTR_des_solved.m_UA_allocated*1.E-3) / recup_total_UA_assigned)));	//[-]

		double HTR_LP_deltaP_frac = 1.0 - c_sco2_cycle.get_design_solved()->ms_rc_cycle_solved.m_pres[C_sco2_cycle_core::HTR_LP_OUT] /
			c_sco2_cycle.get_design_solved()->ms_rc_cycle_solved.m_pres[C_sco2_cycle_core::TURB_OUT];   //[-] Fractional pressure drop through LP side of HTR
		cm->assign("HTR_LP_deltaP_des", (ssc_number_t)HTR_LP_deltaP_frac);		//[-]
		double HTR_HP_deltaP_frac = 1.0 - c_sco2_cycle.get_design_solved()->ms_rc_cycle_solved.m_pres[C_sco2_cycle_core::HTR_HP_OUT] /
			c_sco2_cycle.get_design_solved()->ms_rc_cycle_solved.m_pres[C_sco2_cycle_core::LTR_HP_OUT];   //[-] Fractional pressure drop through HP side of HTR
		cm->assign("HTR_HP_deltaP_des", (ssc_number_t)HTR_HP_deltaP_frac);		//[-]
	}
	else
	{
		double ssc_nan = std::numeric_limits<ssc_number_t>::quiet_NaN();
		cm->assign("HTR_LP_T_out_des", ssc_nan);	//[C]
		cm->assign("HTR_HP_T_in_des", ssc_nan);		//[C]
		cm->assign("HTR_UA_assigned", ssc_nan);		//[MW/K]
        cm->assign("HTR_UA_calculated", ssc_nan);	//[MW/K]
        cm->assign("eff_HTR", ssc_nan);		//[-]
		cm->assign("NTU_HTR", ssc_nan);		//[-]
		cm->assign("q_dot_HTR", ssc_nan);	//[MWt] convert from kWt
		cm->assign("HTR_min_dT", ssc_nan);	//[C/K]
		cm->assign("HTR_cost", ssc_nan);	//[M$]
		cm->assign("recup_LTR_UA_frac", ssc_nan);	//[-]
		cm->assign("HTR_LP_deltaP_des", ssc_nan);   //[-]
		cm->assign("HTR_HP_deltaP_des", ssc_nan);   //[-]
	}
	cm->assign("recup_total_UA_assigned", (ssc_number_t)(recup_total_UA_assigned));		//[MW/K]
    cm->assign("recup_total_UA_calculated", (ssc_number_t)(recup_total_UA_calculated));	//[MW/K]
    cm->assign("recup_total_cost", (ssc_number_t)(recup_total_cost));	//[MW/K]
		// PHX
	cm->assign("UA_PHX", (ssc_number_t)(c_sco2_cycle.get_design_solved()->ms_phx_des_solved.m_UA_design*1.E-3));	//[MW/K] convert from kW/K
	cm->assign("eff_PHX", (ssc_number_t)c_sco2_cycle.get_design_solved()->ms_phx_des_solved.m_eff_design);				//[-]
	cm->assign("NTU_PHX", (ssc_number_t)c_sco2_cycle.get_design_solved()->ms_phx_des_solved.m_NTU_design);				//[-]
	cm->assign("T_co2_PHX_in", (ssc_number_t)(c_sco2_cycle.get_design_solved()->ms_rc_cycle_solved.m_temp[C_sco2_cycle_core::HTR_HP_OUT] - 273.15));	//[C]
	cm->assign("P_co2_PHX_in", (ssc_number_t)(c_sco2_cycle.get_design_solved()->ms_rc_cycle_solved.m_pres[C_sco2_cycle_core::HTR_HP_OUT] * 1.E-3));		//[MPa] convert from kPa
	cm->assign("deltaT_HTF_PHX", (ssc_number_t)s_sco2_des_par.m_T_htf_hot_in - T_htf_cold_calc);		//[K]
	cm->assign("q_dot_PHX", (ssc_number_t)(c_sco2_cycle.get_design_solved()->ms_phx_des_solved.m_Q_dot_design*1.E-3));	//[MWt] convert from kWt
	double PHX_deltaP_frac = 1.0 - c_sco2_cycle.get_design_solved()->ms_rc_cycle_solved.m_pres[C_sco2_cycle_core::TURB_IN] /
		c_sco2_cycle.get_design_solved()->ms_rc_cycle_solved.m_pres[C_sco2_cycle_core::HTR_HP_OUT];   //[-] Fractional pressure drop through co2 side of PHX
	cm->assign("PHX_co2_deltaP_des", (ssc_number_t)PHX_deltaP_frac);
	cm->assign("PHX_cost", (ssc_number_t)c_sco2_cycle.get_design_solved()->ms_phx_des_solved.m_cost);	//[M$]
	cost_sum += c_sco2_cycle.get_design_solved()->ms_phx_des_solved.m_cost;	//[M$]
		// Low Pressure Cooler
	cm->assign("mc_cooler_T_in", (ssc_number_t)(c_sco2_cycle.get_design_solved()->ms_rc_cycle_solved.ms_mc_air_cooler.m_T_in_co2 - 273.15));	//[C]
	cm->assign("mc_cooler_P_in", (ssc_number_t)(c_sco2_cycle.get_design_solved()->ms_rc_cycle_solved.ms_mc_air_cooler.m_P_in_co2 / 1.E3));		//[MPa]
	cm->assign("mc_cooler_rho_in", (ssc_number_t)(c_sco2_cycle.get_design_solved()->ms_rc_cycle_solved.m_dens[C_sco2_cycle_core::LTR_LP_OUT]));	//[kg/m3]
	cm->assign("mc_cooler_m_dot_co2", (ssc_number_t)c_sco2_cycle.get_design_solved()->ms_rc_cycle_solved.ms_mc_air_cooler.m_m_dot_co2);		//[kg/s]
	cm->assign("mc_cooler_UA", (ssc_number_t)(c_sco2_cycle.get_design_solved()->ms_rc_cycle_solved.ms_mc_air_cooler.m_UA_total*1.E-6));		//[MW/K] convert from W/K
	cm->assign("mc_cooler_q_dot", (ssc_number_t)(c_sco2_cycle.get_design_solved()->ms_rc_cycle_solved.ms_mc_air_cooler.m_q_dot*1.E-6));		//[MWt] convert from W
	double LP_cooler_deltaP_frac = 1.0 - c_sco2_cycle.get_design_solved()->ms_rc_cycle_solved.m_pres[C_sco2_cycle_core::MC_IN] /
		c_sco2_cycle.get_design_solved()->ms_rc_cycle_solved.m_pres[C_sco2_cycle_core::LTR_LP_OUT];   //[-] Fractional pressure drop through co2 side of PHX
	cm->assign("mc_cooler_co2_deltaP_des", (ssc_number_t)LP_cooler_deltaP_frac);
	cm->assign("mc_cooler_W_dot_fan", (ssc_number_t)(c_sco2_cycle.get_design_solved()->ms_rc_cycle_solved.ms_mc_air_cooler.m_W_dot_fan));	//[MWe]
	cm->assign("mc_cooler_cost", (ssc_number_t)c_sco2_cycle.get_design_solved()->ms_rc_cycle_solved.ms_mc_air_cooler.m_cost);					//[M$]
	cost_sum += c_sco2_cycle.get_design_solved()->ms_rc_cycle_solved.ms_mc_air_cooler.m_cost;					//[M$]
	double cooler_tot_cost = c_sco2_cycle.get_design_solved()->ms_rc_cycle_solved.ms_mc_air_cooler.m_cost;		//[M$]
	double cooler_tot_UA = c_sco2_cycle.get_design_solved()->ms_rc_cycle_solved.ms_mc_air_cooler.m_UA_total*1.E-6;	//[MW/K]
	double cooler_tot_W_dot_fan = c_sco2_cycle.get_design_solved()->ms_rc_cycle_solved.ms_mc_air_cooler.m_W_dot_fan;	//[MWe]
		// Intermediate Pressure Cooler
	if (s_sco2_des_par.m_cycle_config == 2)
	{
		cm->assign("pc_cooler_T_in", (ssc_number_t)(c_sco2_cycle.get_design_solved()->ms_rc_cycle_solved.ms_pc_air_cooler.m_T_in_co2 - 273.15));	//[C]
		cm->assign("pc_cooler_P_in", (ssc_number_t)(c_sco2_cycle.get_design_solved()->ms_rc_cycle_solved.ms_pc_air_cooler.m_P_in_co2 / 1.E3));		//[MPa]
		cm->assign("pc_cooler_m_dot_co2", (ssc_number_t)c_sco2_cycle.get_design_solved()->ms_rc_cycle_solved.ms_pc_air_cooler.m_m_dot_co2);		//[kg/s]
		cm->assign("pc_cooler_UA", (ssc_number_t)(c_sco2_cycle.get_design_solved()->ms_rc_cycle_solved.ms_pc_air_cooler.m_UA_total*1.E-6));		//[MW/K] convert from W/K
		cm->assign("pc_cooler_q_dot", (ssc_number_t)(c_sco2_cycle.get_design_solved()->ms_rc_cycle_solved.ms_pc_air_cooler.m_q_dot*1.E-6));		//[MWt] convert from W
		cm->assign("pc_cooler_W_dot_fan", (ssc_number_t)(c_sco2_cycle.get_design_solved()->ms_rc_cycle_solved.ms_pc_air_cooler.m_W_dot_fan));	//[MWe]
		cm->assign("pc_cooler_cost", (ssc_number_t)c_sco2_cycle.get_design_solved()->ms_rc_cycle_solved.ms_pc_air_cooler.m_cost);					//[M$]
		cost_sum += c_sco2_cycle.get_design_solved()->ms_rc_cycle_solved.ms_pc_air_cooler.m_cost;					//[M$]
		cooler_tot_cost += c_sco2_cycle.get_design_solved()->ms_rc_cycle_solved.ms_pc_air_cooler.m_cost;			//[M$]
		cooler_tot_UA += c_sco2_cycle.get_design_solved()->ms_rc_cycle_solved.ms_pc_air_cooler.m_UA_total*1.E-6;	//[MW/K]
		cooler_tot_W_dot_fan += c_sco2_cycle.get_design_solved()->ms_rc_cycle_solved.ms_pc_air_cooler.m_W_dot_fan;	//[MWe]
	}
	else
	{
		double ssc_nan = std::numeric_limits<ssc_number_t>::quiet_NaN();
		cm->assign("pc_cooler_T_in", ssc_nan);		//[C]
		cm->assign("pc_cooler_P_in", ssc_nan);		//[MPa]
		cm->assign("pc_cooler_m_dot_co2", ssc_nan);	//[kg/s]
		cm->assign("pc_cooler_UA", ssc_nan);		//[MW/K] convert from W/K
		cm->assign("pc_cooler_q_dot", ssc_nan);		//[MWt] convert from W
		cm->assign("pc_cooler_W_dot_fan", ssc_nan);	//[MWe]
		cm->assign("pc_cooler_cost", ssc_nan);		//[M$]
	}
	cm->assign("cooler_tot_cost", (ssc_number_t)cooler_tot_cost);	//[M$]
	cm->assign("cooler_tot_UA", (ssc_number_t)cooler_tot_UA);		//[MW/K]
	cm->assign("cooler_tot_W_dot_fan", (ssc_number_t)cooler_tot_W_dot_fan);	//[MWe]

	cm->assign("cycle_cost", (ssc_number_t)cost_sum);		//[M$]
	cm->assign("cycle_spec_cost", (ssc_number_t)(cost_sum*1.E6 / s_sco2_des_par.m_W_dot_net));	//[$/kWe]
	cm->assign("cycle_spec_cost_thermal", (ssc_number_t)(cost_sum*1.E6 / (s_sco2_des_par.m_W_dot_net / c_sco2_cycle.get_design_solved()->ms_rc_cycle_solved.m_eta_thermal)));	//[$/kWt]
    double W_dot_net_less_cooling = (1.0 - s_sco2_des_par.m_frac_fan_power) * s_sco2_des_par.m_W_dot_net * 1.E-3;   //[MWe]
    cm->assign("W_dot_net_less_cooling", (ssc_number_t)W_dot_net_less_cooling);     //[MWe]
    cm->assign("eta_thermal_net_less_cooling_des", (ssc_number_t)(W_dot_net_less_cooling/(c_sco2_cycle.get_design_solved()->ms_phx_des_solved.m_Q_dot_design*1.E-3)));  //[-]
		// State Points
	ssc_number_t *p_T_state_points = cm->allocate("T_state_points", C_sco2_cycle_core::END_SCO2_STATES);
	ssc_number_t *p_P_state_points = cm->allocate("P_state_points", C_sco2_cycle_core::END_SCO2_STATES);
	ssc_number_t *p_s_state_points = cm->allocate("s_state_points", C_sco2_cycle_core::END_SCO2_STATES);
	ssc_number_t *p_h_state_points = cm->allocate("h_state_points", C_sco2_cycle_core::END_SCO2_STATES);
	for (int i = 0; i < C_sco2_cycle_core::END_SCO2_STATES; i++)
	{
		p_T_state_points[i] = (ssc_number_t)(c_sco2_cycle.get_design_solved()->ms_rc_cycle_solved.m_temp[i] - 273.15);	//[C]
		p_P_state_points[i] = (ssc_number_t)(c_sco2_cycle.get_design_solved()->ms_rc_cycle_solved.m_pres[i] / 1.E3);	//[MPa]
		p_s_state_points[i] = (ssc_number_t)(c_sco2_cycle.get_design_solved()->ms_rc_cycle_solved.m_entr[i]);			//[kJ/kg-K]
		p_h_state_points[i] = (ssc_number_t)(c_sco2_cycle.get_design_solved()->ms_rc_cycle_solved.m_enth[i]);			//[kJ/kg]
	}

		// Calculate Isentropic Enthalpy Rise from LP Cooler Inlet to MC Outlet Pressure
	double T_cooler_in = c_sco2_cycle.get_design_solved()->ms_rc_cycle_solved.m_temp[C_sco2_cycle_core::LTR_LP_OUT];	//[K]
	double P_cooler_in = c_sco2_cycle.get_design_solved()->ms_rc_cycle_solved.m_pres[C_sco2_cycle_core::LTR_LP_OUT];	//[MPa]
	double P_cooler_out = c_sco2_cycle.get_design_solved()->ms_rc_cycle_solved.m_pres[C_sco2_cycle_core::MC_OUT];		//[MPa]
	int isen_enth_check_err = 0;
	double h_cooler_in = std::numeric_limits<double>::quiet_NaN();
	double s_cooler_in = std::numeric_limits<double>::quiet_NaN();
	double rho_cooler_in = std::numeric_limits<double>::quiet_NaN();
	double T_isen_out = std::numeric_limits<double>::quiet_NaN();
	double h_isen_out = std::numeric_limits<double>::quiet_NaN();
	double s_isen_out = std::numeric_limits<double>::quiet_NaN();
	double rho_isen_out = std::numeric_limits<double>::quiet_NaN();
	double deltah_isen = std::numeric_limits<double>::quiet_NaN();

	calculate_turbomachinery_outlet_1(T_cooler_in, P_cooler_in, P_cooler_out, 1.0, true, isen_enth_check_err,
		h_cooler_in, s_cooler_in, rho_cooler_in, T_isen_out,
		h_isen_out, s_isen_out, rho_isen_out, deltah_isen);

	cm->assign("mc_cooler_in_isen_deltah_to_P_mc_out", (ssc_number_t)-deltah_isen);

	return 0;
}
