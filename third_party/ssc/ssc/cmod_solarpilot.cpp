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
#include "lib_weatherfile.h"
#include "lib_util.h"

// solarpilot header files
//#include "AutoPilot_API.h"
//#include "SolarField.h"
//#include "IOUtil.h"
#include "csp_common.h"

#include "AutoPilot_API.h"  //solarpilot
#include "definitions.h"    //solarpilot

#define pi 3.141592654


static var_info _cm_vtab_solarpilot[] = {
/*   VARTYPE           DATATYPE         NAME                         LABEL                                          UNITS     META        GROUP          REQUIRED_IF         CONSTRAINTS         UI_HINTS*/

	/*
	{ SSC_INPUT,        SSC_NUMBER,      "optimize",                  "Enable constrained optimization",            "0/1",    "",         "SolarPILOT",   "*",                "",                "" },
	{ SSC_INPUT,        SSC_NUMBER,      "range_tht_min",             "Tower height, minimum",                      "m",      "",         "SolarPILOT",   "*",                "",                "" },
	{ SSC_INPUT,        SSC_NUMBER,      "range_tht_max",             "Tower height, maximum",                      "m",      "",         "SolarPILOT",   "*",                "",                "" },
	{ SSC_INPUT,        SSC_NUMBER,      "range_rec_aspect_min",      "Receiver aspect ratio, minimum",             "",       "",         "SolarPILOT",   "*",                "",                "" },
	{ SSC_INPUT,        SSC_NUMBER,      "range_rec_aspect_max",      "Receiver aspect ratio, maximum",             "",       "",         "SolarPILOT",   "*",                "",                "" },
	{ SSC_INPUT,        SSC_NUMBER,      "range_rec_height_min",      "Receiver height, minimum",                   "m",      "",         "SolarPILOT",   "*",                "",                "" },
	{ SSC_INPUT,        SSC_NUMBER,      "range_rec_height_max",      "Receiver height, maximum",                   "m",      "",         "SolarPILOT",   "*",                "",                "" },
	{ SSC_INPUT,        SSC_NUMBER,      "flux_max",                  "Maximum flux",                               "kW/m2",  "",         "SolarPILOT",   "*",                "",                "" },
	*/

	{ SSC_INPUT,        SSC_STRING,      "solar_resource_file",       "Solar weather data file",                    "",       "",         "SolarPILOT",   "?",                "LOCAL_FILE",      "" },

	{ SSC_INPUT,        SSC_NUMBER,      "helio_width",               "Heliostat width",                            "m",      "",         "SolarPILOT",   "*",                "",                "" },
	{ SSC_INPUT,        SSC_NUMBER,      "helio_height",              "Heliostat height",                           "m",      "",         "SolarPILOT",   "*",                "",                "" },
	{ SSC_INPUT,        SSC_NUMBER,      "helio_optical_error",       "Optical error",                              "rad",    "",         "SolarPILOT",   "*",                "",                "" },
	{ SSC_INPUT,        SSC_NUMBER,      "helio_active_fraction",     "Active fraction of reflective area",         "frac",   "",         "SolarPILOT",   "*",                "",                "" },
    { SSC_INPUT,        SSC_NUMBER,      "dens_mirror",               "Ratio of reflective area to profile",        "frac",   "",         "SolarPILOT",   "*",                "",                "" },
    { SSC_INPUT,        SSC_NUMBER,      "helio_reflectance",         "Mirror reflectance",                         "frac",   "",         "SolarPILOT",   "*",                "",                "" },
	{ SSC_INPUT,        SSC_NUMBER,      "rec_absorptance",           "Absorptance",                                "frac",   "",         "SolarPILOT",   "*",                "",                "" },
	{ SSC_INPUT,        SSC_NUMBER,      "rec_height",                "Receiver height",                            "m",      "",         "SolarPILOT",   "*",                "",                "" },
	{ SSC_INPUT,        SSC_NUMBER,      "rec_aspect",                "Receiver aspect ratio (H/W)",                "frac",   "",         "SolarPILOT",   "*",                "",                "" },
	{ SSC_INPUT,        SSC_NUMBER,      "rec_hl_perm2",              "Receiver design heat loss",                  "kW/m2",  "",         "SolarPILOT",   "*",                "",                "" },
	{ SSC_INPUT,        SSC_NUMBER,      "q_design",                  "Receiver thermal design power",              "MW",     "",         "SolarPILOT",   "*",                "",                "" },
	{ SSC_INPUT,        SSC_NUMBER,      "dni_des",                   "Design-point DNI",                           "W/m2",   "",         "SolarPILOT",   "*",                "",                "" },
	{ SSC_INPUT,        SSC_NUMBER,      "land_max",                  "Max heliostat-dist-to-tower-height ratio",   "",       "",         "SolarPILOT",   "*",                "",                "" },
	{ SSC_INPUT,        SSC_NUMBER,      "land_min",                  "Min heliostat-dist-to-tower-height ratio",   "",       "",         "SolarPILOT",   "*",                "",                "" },
	{ SSC_INPUT,        SSC_NUMBER,      "h_tower",                   "Tower height",                               "m",      "",         "SolarPILOT",   "*",                "",                "" },
	{ SSC_INPUT,        SSC_NUMBER,      "c_atm_0",					  "Attenuation coefficient 0",                  "",       "",         "SolarPILOT",   "?=0.006789",       "",                "" },
	{ SSC_INPUT,        SSC_NUMBER,      "c_atm_1",					  "Attenuation coefficient 1",                  "",       "",         "SolarPILOT",   "?=0.1046",         "",                "" },
	{ SSC_INPUT,        SSC_NUMBER,      "c_atm_2",					  "Attenuation coefficient 2",                  "",       "",         "SolarPILOT",   "?=-0.0107",        "",                "" },
	{ SSC_INPUT,        SSC_NUMBER,      "c_atm_3",					  "Attenuation coefficient 3",                  "",       "",         "SolarPILOT",   "?=0.002845",       "",                "" },
	
	{ SSC_INPUT,        SSC_NUMBER,      "n_facet_x",                 "Number of heliostat facets - X",             "",       "",         "SolarPILOT",   "*",                "",                "" },
	{ SSC_INPUT,        SSC_NUMBER,      "n_facet_y",                 "Number of heliostat facets - Y",             "",       "",         "SolarPILOT",   "*",                "",                "" },
	{ SSC_INPUT,        SSC_NUMBER,      "focus_type",                "Heliostat focus method",                     "",       "",         "SolarPILOT",   "*",                "",                "" },
	{ SSC_INPUT,        SSC_NUMBER,      "cant_type",                 "Heliostat cant method",                      "",       "",         "SolarPILOT",   "*",                "",                "" },
	{ SSC_INPUT,        SSC_NUMBER,      "n_flux_days",               "No. days in flux map lookup",                "",       "",         "SolarPILOT",   "?=8",              "",                "" },
	{ SSC_INPUT,        SSC_NUMBER,      "delta_flux_hrs",            "Hourly frequency in flux map lookup",        "",       "",         "SolarPILOT",   "?=1",              "",                "" },

	{ SSC_INPUT,        SSC_NUMBER,      "calc_fluxmaps",             "Include fluxmap calculations",               "",       "",         "SolarPILOT",   "?=0",              "",                "" },
	{ SSC_INPUT,        SSC_NUMBER,      "n_flux_x",                  "Flux map X resolution",                      "",       "",         "SolarPILOT",   "?=12",             "",                "" },
    { SSC_INPUT,        SSC_NUMBER,      "n_flux_y",                  "Flux map Y resolution",                      "",       "",         "SolarPILOT",   "?=1",              "",                "" },
    { SSC_INPUT,        SSC_NUMBER,      "check_max_flux",            "Check max flux at design point",             "",       "",         "SolarPILOT",   "?=0",              "",                "" },
	{ SSC_INPUT,        SSC_NUMBER,      "tower_fixed_cost",          "Tower fixed cost",                           "$",      "",         "SolarPILOT",   "*",                "",                "" },
	{ SSC_INPUT,        SSC_NUMBER,      "tower_exp",                 "Tower cost scaling exponent",                "",       "",         "SolarPILOT",   "*",                "",                "" },
	{ SSC_INPUT,        SSC_NUMBER,      "rec_ref_cost",              "Receiver reference cost",                    "$",      "",         "SolarPILOT",   "*",                "",                "" },
	{ SSC_INPUT,        SSC_NUMBER,      "rec_ref_area",              "Receiver reference area for cost scale",     "",       "",         "SolarPILOT",   "*",                "",                "" },
	{ SSC_INPUT,        SSC_NUMBER,      "rec_cost_exp",              "Receiver cost scaling exponent",             "",       "",         "SolarPILOT",   "*",                "",                "" },
	{ SSC_INPUT,        SSC_NUMBER,      "site_spec_cost",            "Site improvement cost",                      "$/m2",   "",         "SolarPILOT",   "*",                "",                "" },
	{ SSC_INPUT,        SSC_NUMBER,      "heliostat_spec_cost",       "Heliostat field cost",                       "$/m2",   "",         "SolarPILOT",   "*",                "",                "" },
	//{ SSC_INPUT,        SSC_NUMBER,      "plant_spec_cost",           "Power cycle specific cost",                  "$/kWe",  "",         "SolarPILOT",   "*",                "",                "" },
	//{ SSC_INPUT,        SSC_NUMBER,      "bop_spec_cost",             "BOS specific cost",                          "$/kWe",  "",         "SolarPILOT",   "*",                "",                "" },
	//{ SSC_INPUT,        SSC_NUMBER,      "tes_spec_cost",             "Thermal energy storage cost",                "$/kWht", "",         "SolarPILOT",   "*",                "",                "" },
	{ SSC_INPUT,        SSC_NUMBER,      "land_spec_cost",            "Total land area cost",                       "$/acre", "",         "SolarPILOT",   "*",                "",                "" },
	{ SSC_INPUT,        SSC_NUMBER,      "contingency_rate",          "Contingency for cost overrun",               "%",      "",         "SolarPILOT",   "*",                "",                "" },
	{ SSC_INPUT,        SSC_NUMBER,      "sales_tax_rate",            "Sales tax rate",                             "%",      "",         "SolarPILOT",   "*",                "",                "" },
	{ SSC_INPUT,        SSC_NUMBER,      "sales_tax_frac",            "Percent of cost to which sales tax applies", "%",      "",         "SolarPILOT",   "*",                "",                "" },
	{ SSC_INPUT,        SSC_NUMBER,      "cost_sf_fixed",             "Soalr field fixed cost",                     "$",      "",         "SolarPILOT",   "*",                "",                "" },

    { SSC_INPUT,        SSC_NUMBER,      "is_optimize",               "Do SolarPILOT optimization",                 "",       "",         "SolarPILOT",   "?=0",              "",                "" },
    { SSC_INPUT,        SSC_NUMBER,      "flux_max",                  "Maximum allowable flux",                     "",       "",         "SolarPILOT",   "?=1000",           "",                "" },
    { SSC_INPUT,        SSC_NUMBER,      "opt_init_step",             "Optimization initial step size",             "",       "",         "SolarPILOT",   "?=0.05",           "",                "" },
    { SSC_INPUT,        SSC_NUMBER,      "opt_max_iter",              "Max. number iteration steps",                "",       "",         "SolarPILOT",   "?=200",            "",                "" },
    { SSC_INPUT,        SSC_NUMBER,      "opt_conv_tol",              "Optimization convergence tol",               "",       "",         "SolarPILOT",   "?=0.001",          "",                "" },
    { SSC_INPUT,        SSC_NUMBER,      "opt_algorithm",             "Optimization algorithm",                     "",       "",         "SolarPILOT",   "?=0",              "",                "" },
    { SSC_INPUT,        SSC_NUMBER,      "opt_flux_penalty",          "Optimization flux overage penalty",          "",       "",         "SolarPILOT",   "*",                "",                "" },
	{ SSC_INPUT,        SSC_MATRIX,      "helio_positions_in",        "Heliostat position table",                   "",       "",         "SolarPILOT",   "",                "",                "" },


	/* outputs */
	{ SSC_OUTPUT,       SSC_MATRIX,      "opteff_table",              "Optical efficiency (azi, zen, eff x nsim)",  "",       "",         "SolarPILOT",   "*",                "",                "" },
	{ SSC_OUTPUT,       SSC_MATRIX,      "flux_table",                "Flux intensity table (flux(X) x (flux(y) x position)",  "frac", "", "SolarPILOT",  "*",                "",                "" },
	{ SSC_OUTPUT,       SSC_MATRIX,      "heliostat_positions",       "Heliostat positions (x,y)",                  "m",      "",         "SolarPILOT",   "*",                "",                "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "number_heliostats",         "Number of heliostats",                       "",        "",        "SolarPILOT",   "*",                "",                "" },
    { SSC_OUTPUT,       SSC_NUMBER,      "area_sf",                   "Total reflective heliostat area",            "m^2",    "",         "SolarPILOT",   "*",                "",                "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "base_land_area",            "Land area occupied by heliostats",           "acre",   "",         "SolarPILOT",   "*",                "",                "" },
    { SSC_OUTPUT,       SSC_NUMBER,      "land_area",                 "Total land area",                            "acre",   "",         "SolarPILOT",   "*",                "",                "" }, 
	{ SSC_OUTPUT,       SSC_NUMBER,      "h_tower_opt",               "Optimized tower height",                     "m",      "",         "SolarPILOT",   "*",                "",                "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "rec_height_opt",            "Optimized receiver height",                  "m",      "",         "SolarPILOT",   "*",                "",                "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "rec_aspect_opt",            "Optimized receiver aspect ratio",            "-",      "",         "SolarPILOT",   "*",                "",                "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "flux_max_observed",         "Maximum observed flux at design",            "kW/m2",  "",         "SolarPILOT",   "check_max_flux=1", "",                "" },

    { SSC_OUTPUT,       SSC_NUMBER,      "cost_rec_tot",              "Total receiver cost",                        "$",      "",         "SolarPILOT",   "*",                "",                "" },
    { SSC_OUTPUT,       SSC_NUMBER,      "cost_sf_tot",               "Total heliostat field cost",                 "$",      "",         "SolarPILOT",   "*",                "",                "" },
    { SSC_OUTPUT,       SSC_NUMBER,      "cost_tower_tot",            "Total tower cost",                           "$",      "",         "SolarPILOT",   "*",                "",                "" },
    { SSC_OUTPUT,       SSC_NUMBER,      "cost_land_tot",             "Total land cost",                            "$",      "",         "SolarPILOT",   "*",                "",                "" },
    { SSC_OUTPUT,       SSC_NUMBER,      "cost_site_tot",             "Total site cost",                            "$",      "",         "SolarPILOT",   "*",                "",                "" },

	var_info_invalid };

//static bool solarpilot_callback( simulation_info *siminfo, void *data );
//static bool optimize_callback( simulation_info *siminfo, void *data );

#ifdef _MSC_VER
#define mysnprintf _snprintf
#else
#define mysnprintf snprintf
#endif

class cm_solarpilot : public compute_module
{
public:
	
	cm_solarpilot()
	{
		add_var_info( _cm_vtab_solarpilot );
	}

	void exec( )
	{
		std::shared_ptr<weather_data_provider> wdata = std::make_shared<weatherfile>(as_string("solar_resource_file"));
		solarpilot_invoke spi( this );
		spi.run(wdata);

		assign("h_tower_opt", (ssc_number_t)spi.sf.tht.val);
		assign("rec_height_opt", (ssc_number_t)spi.recs.front().rec_height.val);
		assign("rec_aspect_opt", (ssc_number_t)spi.recs.front().rec_aspect.Val());
		assign("cost_rec_tot", (ssc_number_t)spi.fin.rec_cost.Val());
		assign("cost_sf_tot", (ssc_number_t)spi.fin.heliostat_cost.Val());
		assign("cost_tower_tot", (ssc_number_t)spi.fin.tower_cost.Val());
		assign("cost_land_tot", (ssc_number_t)spi.fin.land_cost.Val());
		assign("cost_site_tot", (ssc_number_t)spi.fin.site_cost.Val());
		assign("land_area", (ssc_number_t)spi.land.land_area.Val());
		assign("area_sf", (ssc_number_t)spi.sf.sf_area.Val());

        if( is_assigned("helio_positions_in") )
        {
            util::matrix_t<double> hposin = as_matrix("helio_positions_in");
			ssc_number_t *hpos = allocate( "heliostat_positions",  hposin.nrows(), 2 );
			for(size_t i=0; i< hposin.nrows(); i++){
				hpos[i * 2] = (ssc_number_t)hposin.at(i, 0);
				hpos[i * 2 + 1] = (ssc_number_t)hposin.at(i, 1);
			}
            //return the number of heliostats
			assign("number_heliostats", (ssc_number_t)hposin.nrows());
        }
        else
        {
		    //Collect the heliostat position data
		    if(  spi.layout.heliostat_positions.size() > 0 )
		    {
			    ssc_number_t *hpos = allocate( "heliostat_positions",  spi.layout.heliostat_positions.size(), 2 );
			    for(size_t i=0; i< spi.layout.heliostat_positions.size(); i++){
				    hpos[ i*2     ] = (float) spi.layout.heliostat_positions.at(i).location.x;
				    hpos[ i*2 + 1 ] = (float) spi.layout.heliostat_positions.at(i).location.y;
			    }
		    }
		    else
			    throw exec_error("solarpilot", "failed to generate a heliostat field layout");
 		    
            //return the number of heliostats
			assign("number_heliostats", (ssc_number_t)spi.layout.heliostat_positions.size());
        }

 
		
		//return the land area
		assign("base_land_area", (ssc_number_t)spi.land.land_area.Val());

		//check if flux map calculations are desired
		if( as_boolean("calc_fluxmaps") ){


			//collect the optical efficiency data and sun positions
			if ( spi.fluxtab.zeniths.size() > 0 && spi.fluxtab.azimuths.size() > 0
				&& spi.fluxtab.efficiency.size() > 0 )
			{
				size_t nvals = spi.fluxtab.efficiency.size();
				ssc_number_t *opteff = allocate( "opteff_table", nvals, 3 );
				for( size_t i=0;i<nvals;i++ )
                {
					opteff[i * 3] = (ssc_number_t)(spi.fluxtab.azimuths[i] * 180. / pi - 180.);      //Convention is usually S=0, E<0, W>0 
					opteff[i * 3 + 1] = (ssc_number_t)(spi.fluxtab.zeniths[i] * 180. / pi);     //Provide zenith angle
					opteff[i * 3 + 2] = (ssc_number_t)spi.fluxtab.efficiency[i];
                }
			}
			else
				throw exec_error("solarpilot", "failed to calculate a correct optical efficiency table");
		
			//collect the flux map data
			block_t<double> *flux_data = &spi.fluxtab.flux_surfaces.front().flux_data;  //there should be only one flux stack for SAM
			if( flux_data->ncols() > 0 && flux_data->nlayers() > 0 ){
			    
                int nflux_y = (int)flux_data->nrows();
                int nflux_x = (int)flux_data->ncols();

				ssc_number_t *fluxdata = allocate( "flux_table", nflux_y * flux_data->nlayers(), nflux_x );
			
				int cur_row=0;
			
				for( size_t i=0; i<flux_data->nlayers(); i++){
					for( int j=0; j<nflux_y; j++){
						for( int k=0; k<nflux_x; k++){
							fluxdata[cur_row * nflux_x + k] = (float)flux_data->at(j, k, i);
						}
						cur_row++;
					}
				}
			}
			else
				throw exec_error("solarpilot", "failed to calculate a correct flux map table");

		}
		else 
		{
			//fluxmaps not required, so declare required variables and fill with zeros
			allocate("opteff_table", 1, 3);
			allocate("flux_table", 1, 1);
		}

	}
};


DEFINE_MODULE_ENTRY( solarpilot, "SolarPILOT - CSP tower solar field layout tool.", 0 )
