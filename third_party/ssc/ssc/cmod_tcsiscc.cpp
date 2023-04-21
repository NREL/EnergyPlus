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

// Integrated Solar Combined Cycle
#include "core.h"
#include "tckernel.h"
// for adjustment factors
#include "common.h"
// solarpilot header files
#include "AutoPilot_API.h"
#include "SolarField.h"
#include "IOUtil.h"
#include "csp_common.h"

using namespace std;

static var_info _cm_vtab_tcsiscc[] = {
//    VARTYPE           DATATYPE          NAME                   LABEL                                                                UNITS           META            GROUP            REQUIRED_IF                CONSTRAINTS              UI_HINTS
    { SSC_INPUT,        SSC_STRING,      "solar_resource_file",  "local weather file path",                                           "",             "",            "Weather",        "*",                       "LOCAL_FILE",            "" },
	{ SSC_INPUT,        SSC_NUMBER,      "system_capacity",      "Nameplate capacity",                                                "kW",           "",            "molten salt tower", "*",                    "",   "" },
														     																	  
	// Heliostat field  parameters				     																	  
	{ SSC_INPUT,        SSC_NUMBER,      "run_type",             "Run type",                                                          "-",            "",            "heliostat",      "*",                       "",                     "" },
    { SSC_INPUT,        SSC_NUMBER,      "helio_width",          "Heliostat width",                                                   "m",            "",            "heliostat",      "*",                       "",                     "" },
    { SSC_INPUT,        SSC_NUMBER,      "helio_height",         "Heliostat height",                                                  "m",            "",            "heliostat",      "*",                       "",                     "" },
    { SSC_INPUT,        SSC_NUMBER,      "helio_optical_error",  "Heliostat optical error",                                           "rad",          "",            "heliostat",      "*",                       "",                     "" },
    { SSC_INPUT,        SSC_NUMBER,      "helio_active_fraction","Heliostat active frac.",                                            "-",            "",            "heliostat",      "*",                       "",                     "" },
    { SSC_INPUT,        SSC_NUMBER,      "dens_mirror",          "Ratio of Reflective Area to Profile",                               "-",            "",            "heliostat",      "*",                       "",                     "" },
    { SSC_INPUT,        SSC_NUMBER,      "helio_reflectance",    "Heliostat reflectance",                                             "-",            "",            "heliostat",      "*",                       "",                     "" },
    { SSC_INPUT,        SSC_NUMBER,      "rec_absorptance",      "Receiver absorptance",                                              "-",            "",            "heliostat",      "*",                       "",                     "" },
    { SSC_INPUT,        SSC_NUMBER,      "rec_height",           "Receiver height",                                                   "m",            "",            "heliostat",      "*",                       "",                     "" },
    { SSC_INPUT,        SSC_NUMBER,      "rec_aspect",           "Receiver aspect ratio",                                             "-",            "",            "heliostat",      "*",                       "",                     "" },
    { SSC_INPUT,        SSC_NUMBER,      "rec_hl_perm2",         "Receiver design heatloss",                                          "kW/m2",        "",            "heliostat",      "*",                       "",                     "" },
    { SSC_INPUT,        SSC_NUMBER,      "land_bound_type",      "Land boundary type",                                                "-",            "",            "heliostat",      "?=0",                     "",                     "" },
    { SSC_INPUT,        SSC_NUMBER,      "land_max",             "Land max boundary",                                                 "-ORm",         "",            "heliostat",      "?=7.5",                   "",                     "" },
    { SSC_INPUT,        SSC_NUMBER,      "land_min",             "Land min boundary",                                                 "-ORm",         "",            "heliostat",      "?=0.75",                  "",                     "" },
    { SSC_INPUT,        SSC_MATRIX,      "land_bound_table",     "Land boundary table",                                               "m",            "",            "heliostat",      "?",                       "",                     "" },
    { SSC_INPUT,        SSC_ARRAY,       "land_bound_list",      "Boundary table listing",                                            "-",            "",            "heliostat",      "?",                       "",                     "" },
	{ SSC_INPUT,        SSC_NUMBER,      "dni_des",              "Design-point DNI",                                                  "W/m2",         "",            "heliostat",      "*",                       "",                     "" },
    { SSC_INPUT,        SSC_NUMBER,      "p_start",              "Heliostat startup energy",                                          "kWe-hr",       "",            "heliostat",      "*",                       "",                     "" },
    { SSC_INPUT,        SSC_NUMBER,      "p_track",              "Heliostat tracking energy",                                         "kWe",          "",            "heliostat",      "*",                       "",                     "" },
    { SSC_INPUT,        SSC_NUMBER,      "hel_stow_deploy",      "Stow/deploy elevation",                                             "deg",          "",            "heliostat",      "*",                       "",                     "" },
    { SSC_INPUT,        SSC_NUMBER,      "v_wind_max",           "Max. wind velocity",                                                "m/s",          "",            "heliostat",      "*",                       "",                     "" },
    { SSC_INPUT,        SSC_NUMBER,      "interp_nug",           "Interpolation nugget",                                              "-",            "",            "heliostat",      "?=0",                     "",                     "" },
    { SSC_INPUT,        SSC_NUMBER,      "interp_beta",          "Interpolation beta coef.",                                          "-",            "",            "heliostat",      "?=1.99",                  "",                     "" },
    { SSC_INPUT,        SSC_NUMBER,      "n_flux_x",             "Flux map X resolution",                                             "-",            "",            "heliostat",      "?=12",                    "",                     "" },
    { SSC_INPUT,        SSC_NUMBER,      "n_flux_y",             "Flux map Y resolution",                                             "-",            "",            "heliostat",      "?=1",                     "",                     "" },
    { SSC_INPUT,        SSC_MATRIX,      "helio_positions",      "Heliostat position table",                                          "m",            "",            "heliostat",      "run_type=1",              "",                     "" },
    { SSC_INPUT,        SSC_MATRIX,      "helio_aim_points",     "Heliostat aim point table",                                         "m",            "",            "heliostat",      "?",                       "",                     "" },
    { SSC_INPUT,        SSC_NUMBER,      "N_hel",                "Number of heliostats",                                              "-",            "",            "heliostat",      "?",                       "",                     "" },
    { SSC_INPUT,        SSC_MATRIX,      "eta_map",              "Field efficiency array",                                            "-",            "",            "heliostat",      "?",                       "",                     "" },
    { SSC_INPUT,        SSC_MATRIX,      "flux_positions",       "Flux map sun positions",                                            "deg",          "",            "heliostat",      "?",                       "",                     "" },
    { SSC_INPUT,        SSC_MATRIX,      "flux_maps",            "Flux map intensities",                                              "-",            "",            "heliostat",      "?",                       "",                     "" },
	{ SSC_INPUT,        SSC_NUMBER,      "c_atm_0",              "Attenuation coefficient 0",                                         "",             "",            "heliostat",      "?=0.006789",              "",                     "" },
	{ SSC_INPUT,        SSC_NUMBER,      "c_atm_1",              "Attenuation coefficient 1",                                         "",             "",            "heliostat",      "?=0.1046",                "",                     "" },
	{ SSC_INPUT,        SSC_NUMBER,      "c_atm_2",              "Attenuation coefficient 2",                                         "",             "",            "heliostat",      "?=-0.0107",               "",                     "" },
	{ SSC_INPUT,        SSC_NUMBER,      "c_atm_3",              "Attenuation coefficient 3",                                         "",             "",            "heliostat",      "?=0.002845",              "",                     "" },
	{ SSC_INPUT,        SSC_NUMBER,      "n_facet_x",            "Number of heliostat facets - X",                                    "",             "",            "heliostat",      "*",                       "",                     "" },
	{ SSC_INPUT,        SSC_NUMBER,      "n_facet_y",            "Number of heliostat facets - Y",                                    "",             "",            "heliostat",      "*",                       "",                     "" },
	{ SSC_INPUT,        SSC_NUMBER,      "focus_type",           "Heliostat focus method",                                            "",             "",            "heliostat",      "*",                       "",                     "" },
	{ SSC_INPUT,        SSC_NUMBER,      "cant_type",            "Heliostat cant method",                                             "",             "",            "heliostat",      "*",                       "",                     "" },
    { SSC_INPUT,        SSC_NUMBER,      "n_flux_days",          "No. days in flux map lookup",                                       "",             "",            "heliostat",      "?=8",                     "",                     "" },
	{ SSC_INPUT,        SSC_NUMBER,      "delta_flux_hrs",       "Hourly frequency in flux map lookup",                               "",             "",            "heliostat",      "?=1",                     "",                     "" },
        
	{ SSC_INPUT,        SSC_NUMBER,      "h_tower",                   "Tower height",                               "m",      "",         "heliostat",   "*",                "",                "" },
	{ SSC_INPUT,        SSC_NUMBER,      "q_design",                  "Receiver thermal design power",              "MW",     "",         "heliostat",   "*",                "",                "" },
    { SSC_INPUT,        SSC_NUMBER,      "calc_fluxmaps",             "Include fluxmap calculations",               "",       "",         "heliostat",   "?=0",              "",                "" },
	{ SSC_INPUT,        SSC_NUMBER,      "tower_fixed_cost",          "Tower fixed cost",                           "$",      "",         "heliostat",   "*",                "",                "" },
	{ SSC_INPUT,        SSC_NUMBER,      "tower_exp",                 "Tower cost scaling exponent",                "",       "",         "heliostat",   "*",                "",                "" },
	{ SSC_INPUT,        SSC_NUMBER,      "rec_ref_cost",              "Receiver reference cost",                    "$",      "",         "heliostat",   "*",                "",                "" },
	{ SSC_INPUT,        SSC_NUMBER,      "rec_ref_area",              "Receiver reference area for cost scale",     "",       "",         "heliostat",   "*",                "",                "" },
	{ SSC_INPUT,        SSC_NUMBER,      "rec_cost_exp",              "Receiver cost scaling exponent",             "",       "",         "heliostat",   "*",                "",                "" },
	{ SSC_INPUT,        SSC_NUMBER,      "site_spec_cost",            "Site improvement cost",                      "$/m2",   "",         "heliostat",   "*",                "",                "" },
	{ SSC_INPUT,        SSC_NUMBER,      "heliostat_spec_cost",       "Heliostat field cost",                       "$/m2",   "",         "heliostat",   "*",                "",                "" },
	{ SSC_INPUT,        SSC_NUMBER,      "plant_spec_cost",           "Power cycle specific cost",                  "$/kWe",  "",         "heliostat",   "*",                "",                "" },
	{ SSC_INPUT,        SSC_NUMBER,      "bop_spec_cost",             "BOS specific cost",                          "$/kWe",  "",         "heliostat",   "*",                "",                "" },
	{ SSC_INPUT,        SSC_NUMBER,      "tes_spec_cost",             "Thermal energy storage cost",                "$/kWht", "",         "heliostat",   "*",                "",                "" },
	{ SSC_INPUT,        SSC_NUMBER,      "land_spec_cost",            "Total land area cost",                       "$/acre", "",         "heliostat",   "*",                "",                "" },
	{ SSC_INPUT,        SSC_NUMBER,      "contingency_rate",          "Contingency for cost overrun",               "%",      "",         "heliostat",   "*",                "",                "" },
	{ SSC_INPUT,        SSC_NUMBER,      "sales_tax_rate",            "Sales tax rate",                             "%",      "",         "heliostat",   "*",                "",                "" },
	{ SSC_INPUT,        SSC_NUMBER,      "sales_tax_frac",            "Percent of cost to which sales tax applies", "%",      "",         "heliostat",   "*",                "",                "" },
	{ SSC_INPUT,        SSC_NUMBER,      "cost_sf_fixed",             "Solar field fixed cost",                     "$",      "",         "heliostat",   "*",                "",                "" },
	{ SSC_INPUT,        SSC_NUMBER,      "fossil_spec_cost",          "Fossil system specific cost",                "$/kWe",      "",     "heliostat",   "*",                "",                "" },

    { SSC_INPUT,        SSC_NUMBER,      "is_optimize",          "Do SolarPILOT optimization",                                        "",             "",            "heliostat",       "?=0",                    "",                "" },
    { SSC_INPUT,        SSC_NUMBER,      "flux_max",             "Maximum allowable flux",                                            "",             "",            "heliostat",       "?=1000",                 "",                "" },
    { SSC_INPUT,        SSC_NUMBER,      "opt_init_step",        "Optimization initial step size",                                    "",             "",            "heliostat",       "?=0.05",                 "",                "" },
    { SSC_INPUT,        SSC_NUMBER,      "opt_max_iter",         "Max. number iteration steps",                                       "",             "",            "heliostat",       "?=200",                 "",                "" },
    { SSC_INPUT,        SSC_NUMBER,      "opt_conv_tol",         "Optimization convergence tol",                                      "",             "",            "heliostat",       "?=0.001",                "",                "" },
    { SSC_INPUT,        SSC_NUMBER,      "opt_flux_penalty",     "Optimization flux overage penalty",                                 "",             "",            "heliostat",       "*",                      "",                     "" },
    { SSC_INPUT,        SSC_NUMBER,      "opt_algorithm",        "Optimization algorithm",                                            "",             "",            "heliostat",       "?=0",                    "",                "" },
	{ SSC_INPUT,        SSC_NUMBER,      "check_max_flux",       "Check max flux at design point",                                    "",             "",            "heliostat",      "?=0",                     "",                     "" },

    //other costs needed for optimization update
	{ SSC_INPUT,        SSC_NUMBER,      "csp.pt.cost.epc.per_acre",       "EPC cost per acre",                 "$/acre",   "",     "heliostat",   "*",                "",                "" },
	{ SSC_INPUT,        SSC_NUMBER,      "csp.pt.cost.epc.percent",        "EPC cost percent of direct",        "",         "",     "heliostat",   "*",                "",                "" },
	{ SSC_INPUT,        SSC_NUMBER,      "csp.pt.cost.epc.per_watt",       "EPC cost per watt",                 "$/W",      "",     "heliostat",   "*",                "",                "" },
	{ SSC_INPUT,        SSC_NUMBER,      "csp.pt.cost.epc.fixed",          "EPC fixed",                         "$",        "",     "heliostat",   "*",                "",                "" },
	{ SSC_INPUT,        SSC_NUMBER,      "csp.pt.cost.plm.per_acre",       "PLM cost per acre",                 "$/acre",   "",     "heliostat",   "*",                "",                "" },
	{ SSC_INPUT,        SSC_NUMBER,      "csp.pt.cost.plm.percent",        "PLM cost percent of direct",        "",         "",     "heliostat",   "*",                "",                "" },
	{ SSC_INPUT,        SSC_NUMBER,      "csp.pt.cost.plm.per_watt",       "PLM cost per watt",                 "$/W",      "",     "heliostat",   "*",                "",                "" },
	{ SSC_INPUT,        SSC_NUMBER,      "csp.pt.cost.plm.fixed",          "PLM fixed",                         "$",        "",     "heliostat",   "*",                "",                "" },
	{ SSC_INPUT,        SSC_NUMBER,      "csp.pt.sf.fixed_land_area",      "Fixed land area",                   "acre",     "",     "heliostat",   "*",                "",                "" },
	{ SSC_INPUT,        SSC_NUMBER,      "csp.pt.sf.land_overhead_factor", "Land overhead factor",              "",         "",     "heliostat",   "*",                "",                "" },
	//The total installed cost from the cost page
    { SSC_INPUT,        SSC_NUMBER,      "total_installed_cost",           "Total installed cost",              "$",        "",     "heliostat",   "*",                "",                "" },
	
	
	{ SSC_INPUT,        SSC_NUMBER,      "receiver_type",        "External=0, Cavity=1",                                              "",             "",            "receiver",       "*",                       "INTEGER",               "" },															     																	  
	// Receiver (type 222) parameters						     																	  
    { SSC_INPUT,        SSC_NUMBER,      "N_panels",             "Number of individual panels on the receiver",                       "",             "",            "receiver",       "*",                       "INTEGER",               "" },
    { SSC_INPUT,        SSC_NUMBER,      "D_rec",                "The overall outer diameter of the receiver",                        "m",            "",            "receiver",       "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "H_rec",                "The height of the receiver",                                        "m",            "",            "receiver",       "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "THT",                  "The height of the tower (hel. pivot to rec equator)",               "m",            "",            "receiver",       "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "d_tube_out",           "The outer diameter of an individual receiver tube",                 "mm",           "",            "receiver",       "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "th_tube",              "The wall thickness of a single receiver tube",                      "mm",           "",            "receiver",       "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "mat_tube",             "The material name of the receiver tubes",                           "",             "",            "receiver",       "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "rec_htf",              "The name of the HTF used in the receiver",                          "",             "",            "receiver",       "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "field_fl_props",       "User defined field fluid property data",                            "-",            "",            "receiver",       "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "Flow_type",            "A flag indicating which flow pattern is used",                      "",             "",            "receiver",       "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "crossover_shift",      "No. panels shift in receiver crossover position",                   "",             "",            "receiver",       "?=0",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "epsilon",              "The emissivity of the receiver surface coating",                    "",             "",            "receiver",       "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "hl_ffact",             "The heat loss factor (thermal loss fudge factor)",                  "",             "",            "receiver",       "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "T_htf_hot_des",        "Hot HTF outlet temperature at design conditions",                   "C",            "",            "receiver",       "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "T_htf_cold_des",       "Cold HTF inlet temperature at design conditions",                   "C",            "",            "receiver",       "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "f_rec_min",            "Minimum receiver mass flow rate turn down fraction",                "",             "",            "receiver",       "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "Q_rec_des",            "Design-point receiver thermal power output",                        "MWt",          "",            "receiver",       "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "rec_su_delay",         "Fixed startup delay time for the receiver",                         "hr",           "",            "receiver",       "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "rec_qf_delay",         "Energy-based rcvr startup delay (fraction of rated thermal power)", "",             "",            "receiver",       "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "m_dot_htf_max",        "Maximum receiver mass flow rate",                                   "kg/hr",        "",            "receiver",       "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "A_sf",                 "Solar Field Area",                                                  "m^2",          "",            "receiver",       "*",                       "",                      "" },
	{ SSC_INPUT,        SSC_NUMBER,      "eta_pump",             "Receiver HTF pump efficiency",                                      "",             "",            "receiver",       "*",                       "",                      "" },

	
	// sam_iscc_powerblock.cpp: not reading in parameters that have already been read in for type 222! need to set these below 
	{ SSC_INPUT,  SSC_NUMBER,  "q_pb_design",          "Design point power block thermal power",         "MWt",     "",    "powerblock",     "*",     "",                "" },
	{ SSC_INPUT,  SSC_NUMBER,  "elev",                 "Plant elevation",                                "m",       "",    "powerblock",     "*",     "",                "" },
	{ SSC_INPUT,  SSC_NUMBER,  "ngcc_model",           "1: NREL, 2: GE",                                 "",        "",    "powerblock",     "*",     "",                "" },
	{ SSC_INPUT,  SSC_NUMBER,  "pinch_point_hotside",  "Hot side temperature HX temperature difference", "C",       "",    "powerblock",     "*",     "",                "" },
	{ SSC_INPUT,  SSC_NUMBER,  "pinch_point_coldside", "Cold side HX pinch point",                       "C",       "",    "powerblock",     "*",     "",                "" },
	 
	// // sam_iscc_parasitics 
	{ SSC_INPUT,        SSC_NUMBER,      "pb_pump_coef",        "Required pumping power for HTF through power block",              "kJ/kg",       "",        "parasitics",     "*",                       "",                      "" },
	{ SSC_INPUT,        SSC_NUMBER,      "piping_loss",         "Thermal loss per meter of piping",                                "Wt/m",        "",        "parasitics",     "*",                       "",                      "" },
	{ SSC_INPUT,        SSC_NUMBER,      "piping_length",       "Total length of exposed piping",                                  "m",           "",        "parasitics",     "*",                       "",                      "" },
	{ SSC_INPUT,        SSC_NUMBER,      "piping_length_mult",  "Piping length multiplier",                                        "",            "",        "parasitics",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "piping_length_const", "Piping constant length",                                          "m",           "",        "parasitics",     "*",                       "",                      "" },
	{ SSC_INPUT,        SSC_NUMBER,      "Q_rec_des",           "Design point solar field thermal output",                         "MW",          "",        "parasitics",     "*",                       "",                      "" },
	{ SSC_INPUT,        SSC_NUMBER,      "pb_fixed_par",        "Fixed parasitic load - runs at all times",                        "MWe/MWcap",   "",        "parasitics",     "*",                       "",                      "" },
	{ SSC_INPUT,        SSC_NUMBER,      "bop_par",             "Balance of plant parasitic power fraction",                       "MWe/MWcap",   "",        "parasitics",     "*",                       "",                      "" },
	{ SSC_INPUT,        SSC_NUMBER,      "bop_par_f",           "Balance of plant parasitic power fraction - mult frac",           "none",        "",        "parasitics",     "*",                       "",                      "" },
	{ SSC_INPUT,        SSC_NUMBER,      "bop_par_0",           "Balance of plant parasitic power fraction - const coeff",         "none",        "",        "parasitics",     "*",                       "",                      "" },
	{ SSC_INPUT,        SSC_NUMBER,      "bop_par_1",           "Balance of plant parasitic power fraction - linear coeff",        "none",        "",        "parasitics",     "*",                       "",                      "" },
	{ SSC_INPUT,        SSC_NUMBER,      "bop_par_2",           "Balance of plant parasitic power fraction - quadratic coeff",     "none",        "",        "parasitics",     "*",                       "",                      "" },
	{ SSC_INPUT,        SSC_NUMBER,      "fossil_output",       "Fossil-only cycle output at design",                              "MWe",         "",        "parasitics",     "*",                       "",                      "" },
	{ SSC_INPUT,        SSC_NUMBER,      "W_dot_solar_des",     "Solar contribution to cycle output at design",                    "MWe",         "",        "parasitics",     "*",                       "",                      "" },

	// OUTPUTS
		// weather
	{ SSC_OUTPUT,       SSC_ARRAY,       "month",             "Resource Month",                                                  "",             "",            "weather",        "*",                       "LENGTH=8760",           "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "hour",              "Resource Hour of Day",                                            "",             "",            "weather",        "*",                       "LENGTH=8760",           "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "solazi",            "Resource Solar Azimuth",                                          "deg",          "",            "weather",        "*",                       "LENGTH=8760",           "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "solzen",            "Resource Solar Zenith",                                           "deg",          "",            "weather",        "*",                       "LENGTH=8760",           "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "beam",              "Resource Beam normal irradiance",                                 "W/m2",         "",            "weather",        "*",                       "LENGTH=8760",           "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "tdry",              "Resource Dry bulb temperature",                                   "C",            "",            "weather",        "*",                       "LENGTH=8760",           "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "twet",              "Resource Wet bulb temperature",                                   "C",            "",            "weather",        "*",                       "LENGTH=8760",           "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "wspd",              "Resource Wind Speed",                                             "m/s",          "",            "weather",        "*",                       "LENGTH=8760",           "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "pres",              "Resource Pressure",                                               "mbar",         "",            "weather",        "*",                       "LENGTH=8760",           "" },

	// sam_type221
	{ SSC_OUTPUT,       SSC_ARRAY,       "eta_field",         "Field optical efficiency",                                        "",             "",            "Outputs",        "*",                       "",                       "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "field_eff_adj",     "Solar field efficiency w/ defocusing",                            "",             "",            "Outputs",        "*",                       "",                       "" },

	// sam_type222 MS receiver
	{ SSC_OUTPUT,       SSC_ARRAY,       "eta_therm",         "Receiver thermal efficiency",                                     "",             "",            "Outputs",        "*",                       "",                       "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "Q_solar_total",     "Receiver thermal power absorbed",                                 "MWt",          "",            "Outputs",        "*",                       "",                       "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "q_conv_sum",        "Receiver thermal power loss to convection",                       "MWt",          "",            "Outputs",        "*",                       "",                       "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "q_rad_sum",         "Receiver thermal power loss to radiation",                        "MWt",          "",            "Outputs",        "*",                       "",                       "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "Q_thermal",         "Receiver thermal power to HTF",                                   "MWt",          "",            "Outputs",        "*",                       "",                       "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "m_dot_ss",          "Receiver mass flow rate, steady state",                           "kg/s",         "",            "Outputs",        "*",                       "",                       "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "m_dot_salt_tot",    "Receiver mass flow rate, derated for startup",                    "kg/s",         "",            "Outputs",        "*",                       "",                       "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "T_htf_cold",        "Receiver HTF temperature in",                                     "C",            "",            "Outputs",        "*",                       "",                       "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "T_salt_hot",        "Receiver HTF temperature out",                                    "C",            "",            "Outputs",        "*",                       "",                       "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "q_startup",         "Receiver startup power",                                          "MWt",          "",            "Outputs",        "*",                       "",                       "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "f_timestep",        "Receiver operating fraction after startup",                       "",             "",            "Outputs",        "*",                       "",                       "" },
	
	// sam_iscc_powerblock
	{ SSC_OUTPUT,       SSC_ARRAY,       "m_dot_steam",       "Cycle solar steam mass flow rate",                                "kg/hr",         "",             "Outputs",        "*",                      "",                       "" },	
	{ SSC_OUTPUT,       SSC_ARRAY,       "T_st_cold",         "Cycle steam temp from NGCC to HX",                                "C",             "",             "Outputs",        "*",                      "",                       "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "T_st_hot",          "Cycle steam temp from HX back to NGCC",                           "C",             "",             "Outputs",        "*",                      "",                       "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "Q_dot_max",         "Cycle max allowable thermal power to NGCC",                       "MWt",           "",             "Outputs",        "*",                      "",                       "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "fuel_use",          "Cycle natural gas used during timestep",                          "MMBTU",         "",             "Outputs",        "*",                      "",                       "" },

	{ SSC_OUTPUT,       SSC_ARRAY,       "W_dot_pc_hybrid",   "Cycle net output including solar power",                          "MWe",           "",             "Outputs",        "*",                      "",                       "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "W_dot_pc_fossil",   "Cycle net output only considering fossil power",                  "MWe",           "",             "Outputs",        "*",                      "",                       "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "W_dot_plant_hybrid","Plant net output including solar power & parasitics",             "MWe",           "",             "Outputs",        "*",                      "",                       "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "W_dot_plant_fossil","Plant net output only considering fossil power & parasitics",     "MWe",           "",             "Outputs",        "*",                      "",                       "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "W_dot_plant_solar", "Plant net output attributable to solar",                          "MWe",           "",             "Outputs",        "*",                      "",                       "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "eta_solar_use",     "Plant solar use efficiency considering parasitics",               "-",             "",             "Outputs",        "*",                      "",                       "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "eta_fuel",          "Plant efficiency of fossil only operation (LHV basis)",           "%",             "",             "Outputs",        "*",                      "",                       "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "solar_fraction",    "Plant solar fraction",                                            "-",             "",             "Outputs",        "*",                      "",                       "" },

	// sam_iscc_parasitics		
	{ SSC_OUTPUT,       SSC_ARRAY,       "W_dot_pump",        "Parasitic power receiver HTF pump",                               "MWe",           "",             "Outputs",        "*",                      "",           "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "pparasi",           "Parasitic power heliostat drives",                                "MWe",           "",             "Outputs",        "*",                      "",           "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "P_plant_balance_tot","Parasitic power generation-dependent load",                      "MWe",           "",             "Outputs",        "*",                      "",           "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "P_fixed",           "Parasitic power fixed load",                                      "MWe",           "",             "Outputs",        "*",                      "",           "" },
																      
	// Annual Outputs		   										                                        
	{ SSC_OUTPUT, SSC_NUMBER, "annual_energy",      "Annual Energy",      "kW", "", "Net_E_Calc", "*", "", "" },


	{ SSC_OUTPUT, SSC_NUMBER, "capacity_factor",    "Capacity factor",    "%", "", "", "*", "", "" },
	{ SSC_OUTPUT, SSC_NUMBER, "kwh_per_kw",         "First year kWh/kW",  "kWh/kW", "", "", "*", "", "" },
	{ SSC_OUTPUT, SSC_NUMBER, "system_heat_rate",   "System heat rate",   "MMBtu/MWh", "", "", "*", "", "" },
	{ SSC_OUTPUT, SSC_NUMBER, "annual_fuel_usage",  "Annual fuel usage",  "kWh", "", "", "*", "", "" },


	var_info_invalid };

class cm_tcsiscc : public tcKernel
{
public:

	cm_tcsiscc(tcstypeprovider *prov)
	:tcKernel(prov)
	{
		add_var_info( _cm_vtab_tcsiscc );
		//set_store_all_parameters(true); // default is 'false' = only store TCS parameters that match the SSC_OUTPUT variables above 
		add_var_info(vtab_adjustment_factors);
		add_var_info(vtab_technology_outputs);
        add_var_info(vtab_sf_adjustment_factors);
	}

	void exec( )
	{
		int weather = add_unit("weatherreader", "TCS weather reader");
		int type_hel_field = add_unit("sam_mw_pt_heliostatfield");
		int type222_receiver = add_unit("sam_mw_pt_type222");		
		int iscc_pb = add_unit("sam_iscc_powerblock");
		int iscc_parasitics = add_unit("sam_iscc_parasitics");
		bool bConnected = true;

		//Set weatherreader parameters
		set_unit_value(weather, "file_name", as_string("solar_resource_file"));
		set_unit_value(weather, "track_mode", 0.0); 
		set_unit_value(weather, "tilt", 0.0);
		set_unit_value(weather, "azimuth", 0.0);

		// Heliostat field
		set_unit_value_ssc_double(type_hel_field, "run_type");// 0=auto, 1=user-type_hel_field, 2=user data
		set_unit_value_ssc_double(type_hel_field, "helio_width");	//[m]
		set_unit_value_ssc_double(type_hel_field, "helio_height");	//[m]
		set_unit_value_ssc_double(type_hel_field, "helio_optical_error");
		set_unit_value_ssc_double(type_hel_field, "helio_active_fraction");
		set_unit_value_ssc_double(type_hel_field, "dens_mirror");
		set_unit_value_ssc_double(type_hel_field, "helio_reflectance");
		set_unit_value_ssc_double(type_hel_field, "rec_absorptance");

		bool is_optimize = as_boolean("is_optimize");

		/*
		Any parameter that's dependent on the size of the solar field must be recalculated here
		if the optimization is happening within the cmod
		*/
		double H_rec, D_rec, rec_aspect, THT, A_sf;

		// Assume ISCC has no storage!!!
		//double tshours = 0.0;

		if( is_optimize )
		{
			//Run solarpilot right away to update values as needed
			solarpilot_invoke spi(this);
			spi.run();
			//AutoPilot_S *sapi = spi.GetSAPI();

			//Optimization iteration history
			vector<vector<double> > steps;
			vector<double> obj, flux;
			spi.getOptimizationSimulationHistory(steps, obj, flux);
			size_t nr = steps.size();
			size_t nc = steps.front().size() + 2;
			ssc_number_t *ssc_hist = allocate("opt_history", nr, nc);
			for( size_t i = 0; i<nr; i++ ){

				for( size_t j = 0; j<steps.front().size(); j++ )
					ssc_hist[i*nc + j] = (ssc_number_t)steps.at(i).at(j);
				ssc_hist[i*nc + nc - 2] = (ssc_number_t)obj.at(i);
				ssc_hist[i*nc + nc - 1] = (ssc_number_t)flux.at(i);

			}

			//receiver calculations
			H_rec = spi.recs.front().rec_height.val;
			rec_aspect = spi.recs.front().rec_aspect.Val();
			THT = spi.sf.tht.val;
			//update heliostat position table
			nr = (int)spi.layout.heliostat_positions.size();
			ssc_number_t *ssc_hl = allocate("helio_positions", nr, 2);
			for( size_t i = 0; i<nr; i++ ){
				ssc_hl[i * 2] = (ssc_number_t)spi.layout.heliostat_positions.at(i).location.x;
				ssc_hl[i * 2 + 1] = (ssc_number_t)spi.layout.heliostat_positions.at(i).location.y;
			}

			A_sf = as_double("helio_height") * as_double("helio_width") * as_double("dens_mirror") * (double)nr;

			//update piping length for parasitic calculation
			double piping_length = THT *  as_double("piping_length_mult") + as_double("piping_length_const");

			//update assignments for cost model
			assign("H_rec", var_data((ssc_number_t)H_rec));
			assign("rec_height", var_data((ssc_number_t)H_rec));
			assign("rec_aspect", var_data((ssc_number_t)rec_aspect));
			assign("D_rec", var_data((ssc_number_t)(H_rec / rec_aspect)));
			assign("THT", var_data((ssc_number_t)THT));			
			assign("h_tower", var_data((ssc_number_t)THT));
			assign("A_sf", var_data((ssc_number_t)A_sf));
			assign("piping_length", var_data((ssc_number_t)piping_length));

			//Update the total installed cost
			double total_direct_cost = 0.;
			double A_rec = std::numeric_limits<double>::quiet_NaN();
			switch( spi.recs.front().rec_type.mapval() )
			  {
			  case var_receiver::REC_TYPE::EXTERNAL_CYLINDRICAL:
			    {
			      double h = spi.recs.front().rec_height.val;
			      double d = h / spi.recs.front().rec_aspect.Val();
			      A_rec = h*d*3.1415926;
			      break;
			    }
			  case var_receiver::REC_TYPE::FLAT_PLATE:
			    {
			      double h = spi.recs.front().rec_height.val;
			      double w = h / spi.recs.front().rec_aspect.Val();
			      A_rec = h*w;
			      break;
			    }
			}
			
			double receiver = as_double("rec_ref_cost")*pow(A_rec / as_double("rec_ref_area"), as_double("rec_cost_exp"));     //receiver cost

			//storage cost
			//double storage = as_double("q_pb_design")*as_double("tshours")*as_double("tes_spec_cost")*1000.;
			double storage = 0.0;

			//power block + BOP
			double P_ref = as_double("W_dot_solar_des") * 1000.;  //kWe
			double power_block = P_ref * (as_double("plant_spec_cost") + as_double("bop_spec_cost")); //$/kWe --> $

			//site improvements
			double site_improvements = A_sf * as_double("site_spec_cost");

			//heliostats
			double heliostats = A_sf * as_double("heliostat_spec_cost");

			//fixed cost
			double cost_fixed = as_double("cost_sf_fixed");

			//fossil
			double fossil = P_ref * as_double("fossil_spec_cost");

			//tower cost
			double tower = as_double("tower_fixed_cost") * exp(as_double("tower_exp") * (THT + 0.5*(-H_rec + as_double("helio_height"))));

			//---- total direct cost -----
			total_direct_cost = (1. + as_double("contingency_rate") / 100.) * (
				site_improvements + heliostats + power_block +
				cost_fixed + storage + fossil + tower + receiver);
			//-----

			//land area
			double land_area = spi.land.land_area.Val() * as_double("csp.pt.sf.land_overhead_factor") + as_double("csp.pt.sf.fixed_land_area");

			//EPC
			double cost_epc =
				as_double("csp.pt.cost.epc.per_acre") * land_area
				+ as_double("csp.pt.cost.epc.percent") * total_direct_cost / 100.
				+ P_ref * 1000. * as_double("csp.pt.cost.epc.per_watt")
				+ as_double("csp.pt.cost.epc.fixed");

			//PLM
			double cost_plm =
				as_double("csp.pt.cost.plm.per_acre") * land_area
				+ as_double("csp.pt.cost.plm.percent") * total_direct_cost / 100.
				+ P_ref * 1000. * as_double("csp.pt.cost.plm.per_watt")
				+ as_double("csp.pt.cost.plm.fixed");

			//sales tax
			//return ${csp.pt.cost.sales_tax.value}/100*${total_direct_cost}*${csp.pt.cost.sales_tax.percent}/100; };
			double cost_sales_tax = as_double("sales_tax_rate") / 100. * total_direct_cost * as_double("sales_tax_frac") / 100.;

			//----- indirect cost
			double total_indirect_cost = cost_epc + cost_plm + cost_sales_tax;

			//----- total installed cost!
			double total_installed_cost = total_direct_cost + total_indirect_cost;
			assign("total_installed_cost", var_data((ssc_number_t)total_installed_cost));

		}
		else
		{
			H_rec = as_double("H_rec");
			rec_aspect = as_double("rec_aspect");
			THT = as_double("THT");
			A_sf = as_double("A_sf");
		}
		D_rec = H_rec / rec_aspect;

		set_unit_value_ssc_double(type_hel_field, "rec_height", H_rec);//, 5.);
		set_unit_value_ssc_double(type_hel_field, "rec_aspect", rec_aspect);
		set_unit_value_ssc_double(type_hel_field, "h_tower", THT);//, 50);
		set_unit_value_ssc_double(type_hel_field, "rec_hl_perm2");//, 0.);
		set_unit_value_ssc_double(type_hel_field, "q_design", as_double("Q_rec_des"));//, 25.);
		set_unit_value_ssc_double(type_hel_field, "dni_des");
		set_unit_value(type_hel_field, "weather_file", as_string("solar_resource_file"));
		set_unit_value_ssc_double(type_hel_field, "land_bound_type");//, 0);
		set_unit_value_ssc_double(type_hel_field, "land_max");//, 7.5);
		set_unit_value_ssc_double(type_hel_field, "land_min");//, 0.75);
		set_unit_value_ssc_double(type_hel_field, "p_start");//, 0.025);
		set_unit_value_ssc_double(type_hel_field, "p_track");//, 0.055);
		set_unit_value_ssc_double(type_hel_field, "hel_stow_deploy");//, 8);
		set_unit_value_ssc_double(type_hel_field, "v_wind_max");//, 25.);
		set_unit_value_ssc_double(type_hel_field, "n_flux_x");//, 10);
		set_unit_value_ssc_double(type_hel_field, "n_flux_y");//, 1);
		set_unit_value_ssc_double(type_hel_field, "c_atm_0");
		set_unit_value_ssc_double(type_hel_field, "c_atm_1");
		set_unit_value_ssc_double(type_hel_field, "c_atm_2");
		set_unit_value_ssc_double(type_hel_field, "c_atm_3");
		set_unit_value_ssc_double(type_hel_field, "n_facet_x");
		set_unit_value_ssc_double(type_hel_field, "n_facet_y");
		set_unit_value_ssc_double(type_hel_field, "focus_type");
		set_unit_value_ssc_double(type_hel_field, "cant_type");
		set_unit_value_ssc_double(type_hel_field, "n_flux_days");
		set_unit_value_ssc_double(type_hel_field, "delta_flux_hrs");

		int run_type = (int)get_unit_value_number(type_hel_field, "run_type");
		/*if(run_type == 0){
		set_unit_value_ssc_matrix(type_hel_field, "helio_positions");
		set_unit_value_ssc_matrix(type_hel_field, "eta_map");
		set_unit_value_ssc_matrix(type_hel_field, "flux_positions");
		set_unit_value_ssc_matrix(type_hel_field, "flux_maps");

		}
		else*/
		if( run_type == 1 ){
			set_unit_value_ssc_matrix(type_hel_field, "helio_positions");
		}
		else if( run_type == 2 ){
			set_unit_value_ssc_matrix(type_hel_field, "eta_map");
			set_unit_value_ssc_matrix(type_hel_field, "flux_positions");
			set_unit_value_ssc_matrix(type_hel_field, "flux_maps");
		}

		bConnected &= connect(weather, "wspd", type_hel_field, "vwind");
		set_unit_value_ssc_double(type_hel_field, "field_control", 1.);
		set_unit_value_ssc_double(weather, "solzen", 90.);	//initialize to be on the horizon
		bConnected &= connect(weather, "solzen", type_hel_field, "solzen");
		bConnected &= connect(weather, "solazi", type_hel_field, "solaz");

		if( as_integer("receiver_type") == 0 )
		{
			// Receiver (type 222) parameters
			set_unit_value_ssc_double(type222_receiver, "N_panels");
			set_unit_value_ssc_double(type222_receiver, "D_rec", D_rec);
			set_unit_value_ssc_double(type222_receiver, "H_rec", H_rec);
			set_unit_value_ssc_double(type222_receiver, "THT", THT);
			set_unit_value_ssc_double(type222_receiver, "d_tube_out");
			set_unit_value_ssc_double(type222_receiver, "th_tube");
			set_unit_value_ssc_double(type222_receiver, "mat_tube");
			set_unit_value_ssc_double(type222_receiver, "rec_htf");
			set_unit_value_ssc_matrix(type222_receiver, "field_fl_props");
			set_unit_value_ssc_double(type222_receiver, "Flow_type");
			set_unit_value_ssc_double(type222_receiver, "crossover_shift");
			set_unit_value_ssc_double(type222_receiver, "epsilon");
			set_unit_value_ssc_double(type222_receiver, "hl_ffact");
			set_unit_value_ssc_double(type222_receiver, "T_htf_hot_des");
			set_unit_value_ssc_double(type222_receiver, "T_htf_cold_des");
			set_unit_value_ssc_double(type222_receiver, "f_rec_min");
			set_unit_value_ssc_double(type222_receiver, "Q_rec_des");
			set_unit_value_ssc_double(type222_receiver, "rec_su_delay");
			set_unit_value_ssc_double(type222_receiver, "rec_qf_delay");
			set_unit_value_ssc_double(type222_receiver, "m_dot_htf_max");
			set_unit_value_ssc_double(type222_receiver, "A_sf", A_sf);
			set_unit_value_ssc_double(type222_receiver, "n_flux_x");
			set_unit_value_ssc_double(type222_receiver, "n_flux_y");
			set_unit_value_ssc_double(type222_receiver, "piping_loss");
			set_unit_value_ssc_double(type222_receiver, "piping_length_add", "piping_length_const");
			set_unit_value_ssc_double(type222_receiver, "piping_length_mult", "piping_length_mult");

			// Constant inputs (so ... should be parameters??)
			set_unit_value_ssc_double(type222_receiver, "T_salt_hot_target", as_double("T_htf_hot_des"));
			set_unit_value_ssc_double(type222_receiver, "eta_pump");
			set_unit_value_ssc_double(type222_receiver, "night_recirc", 0.0);
			set_unit_value_ssc_double(type222_receiver, "hel_stow_deploy");


			// Make all the connections to/from the Receiver (type 222)
			bConnected &= connect(weather, "solazi", type222_receiver, "azimuth");
			bConnected &= connect(weather, "solzen", type222_receiver, "zenith");
			bConnected &= connect(iscc_pb, "T_htf_cold", type222_receiver, "T_salt_cold");
			bConnected &= connect(weather, "wspd", type222_receiver, "V_wind_10");
			bConnected &= connect(weather, "pres", type222_receiver, "P_amb");
			bConnected &= connect(weather, "tdew", type222_receiver, "T_dp");
			bConnected &= connect(weather, "beam", type222_receiver, "I_bn");
			bConnected &= connect(type_hel_field, "eta_field", type222_receiver, "field_eff");
			bConnected &= connect(weather, "tdry", type222_receiver, "T_db");
			bConnected &= connect(type_hel_field, "flux_map", type222_receiver, "flux_map");
			
			// Set necessary receiver initial values
			set_unit_value(type222_receiver, "T_salt_cold", as_double("T_htf_cold_des"));

		} // external receiver


		// Set NGCC Parameters
		set_unit_value(iscc_pb, "HTF_code", as_double("rec_htf"));						
		set_unit_value_ssc_matrix(iscc_pb, "field_fl_props");
		set_unit_value(iscc_pb, "Q_sf_des", as_double("q_pb_design"));
		set_unit_value_ssc_double(iscc_pb, "plant_elevation", as_double("elev"));
		set_unit_value(iscc_pb, "cycle_config", as_double("ngcc_model"));
		set_unit_value_ssc_double(iscc_pb, "hot_side_delta_t", as_double("pinch_point_hotside"));
		set_unit_value_ssc_double(iscc_pb, "pinch_point", as_double("pinch_point_coldside"));

		// Connect NGCC Inputs
		bConnected &= connect(weather, "tdry", iscc_pb, "T_amb");
		bConnected &= connect(weather, "pres", iscc_pb, "P_amb");
		bConnected &= connect(type222_receiver, "m_dot_salt_tot", iscc_pb, "m_dot_ms_ss");
		bConnected &= connect(type222_receiver, "q_dot_ss", iscc_pb, "q_dot_rec_ss");
		bConnected &= connect(type222_receiver, "T_salt_cold", iscc_pb, "T_rec_in");
		bConnected &= connect(type222_receiver, "T_salt_hot", iscc_pb, "T_rec_out");

		// Set ISCC Parasitic Parameters
			// 8.15.15 twn: For MSPT, we're calculating piping losses in physical receiver model, so zero out tower piping parasitics here
			// ...... still need this in Type 228 for DSGPT
		//set_unit_value_ssc_double(iscc_parasitics, "Piping_loss", 0.0);
		//set_unit_value_ssc_double(iscc_parasitics, "piping_length_add", 0.0);
		//set_unit_value_ssc_double(iscc_parasitics, "piping_length_mult", 0.0);
		//set_unit_value_ssc_double(iscc_parasitics, "THT", THT);
		
		set_unit_value_ssc_double(iscc_parasitics, "W_htf_pc_pump", as_double("pb_pump_coef"));
		set_unit_value_ssc_double(iscc_parasitics, "Q_sf_des", as_double("Q_rec_des"));
		set_unit_value_ssc_double(iscc_parasitics, "pb_fixed_par");
		set_unit_value_ssc_double(iscc_parasitics, "bop_par");
		set_unit_value_ssc_double(iscc_parasitics, "bop_par_f");
		set_unit_value_ssc_double(iscc_parasitics, "bop_par_0");
		set_unit_value_ssc_double(iscc_parasitics, "bop_par_1");
		set_unit_value_ssc_double(iscc_parasitics, "bop_par_2");
		set_unit_value_ssc_double(iscc_parasitics, "W_dot_fossil_des", as_double("fossil_output"));
		set_unit_value_ssc_double(iscc_parasitics, "W_dot_solar_des");

		// Connect ISCC Parasitic Inputs
		bConnected &= connect(type_hel_field, "pparasi", iscc_parasitics, "W_dot_tracking");
		bConnected &= connect(type222_receiver, "W_dot_pump", iscc_parasitics, "W_dot_rec_pump");
		bConnected &= connect(type222_receiver, "m_dot_ss", iscc_parasitics, "m_dot_htf_ss");
		bConnected &= connect(iscc_pb, "W_dot_pc_hybrid", iscc_parasitics, "W_dot_pc_hybrid");
		bConnected &= connect(iscc_pb, "W_dot_pc_fossil", iscc_parasitics, "W_dot_pc_fossil");
		bConnected &= connect(type222_receiver, "f_timestep", iscc_parasitics, "f_timestep");
		bConnected &= connect(type222_receiver, "q_dot_ss", iscc_parasitics, "q_solar_ss");
		bConnected &= connect(iscc_pb, "q_dot_fuel", iscc_parasitics, "q_dot_fuel");

        //Load the solar field adjustment factors
        sf_adjustment_factors sf_haf(this);
        if (!sf_haf.setup())
			throw exec_error("tcsgeneric_solar", "failed to setup sf adjustment factors: " + sf_haf.error());
        //allocate array to pass to tcs
        ssc_number_t *sf_adjust = allocate("sf_adjust", 8760);
        for( int i=0; i<8760; i++)
            sf_adjust[i] = sf_haf(i);
        set_unit_value_ssc_array(type_hel_field, "sf_adjust");

		// check if all connections worked
		if ( !bConnected )
			throw exec_error( "tcs_iscc", util::format("there was a problem connecting outputs of one unit to inputs of another for the simulation.") );

		// Run simulation
		size_t hours = 8760;
		if (0 > simulate(3600.0, hours*3600.0, 3600.0) )
			throw exec_error( "tcs_iscc", util::format("there was a problem simulating in tcs_iscc.") );

		// get the outputs
		if (!set_all_output_arrays() )
			throw exec_error( "tcs_iscc", util::format("there was a problem returning the results from the simulation.") );

		// performance adjustement factors
		adjustment_factors haf(this, "adjust");
		if( !haf.setup() )
			throw exec_error("tcsmolten_salt", "failed to setup adjustment factors: " + haf.error());
		// hourly_energy output
		ssc_number_t *p_hourly_energy = allocate("gen", 8760);
		// set hourly energy = tcs output Enet
		size_t count;
		ssc_number_t *hourly_energy = as_array("W_dot_plant_solar", &count);//MWh
		if( count != 8760 )
		{
			std::stringstream msg;
			msg << "gen count incorrect (should be 8760): " << count;
			throw exec_error("tcsiscc", msg.str());
		}
		// apply performance adjustments and convert from MWh to kWh
		for (size_t i = 0; i < count; i++)
		{
			p_hourly_energy[i] = hourly_energy[i] * (ssc_number_t)(haf(i) * 1000.0);
		}

		accumulate_annual("gen", "annual_energy"); // already in kWh

		// metric outputs moved to technology
		double kWhperkW = 0.0;
		double nameplate = as_double("system_capacity");
		double annual_energy = 0.0;
		for( int i = 0; i < 8760; i++ )
			annual_energy += p_hourly_energy[i];
		if( nameplate > 0 ) kWhperkW = annual_energy / nameplate;
		assign("capacity_factor", var_data((ssc_number_t)(kWhperkW / 87.6)));
		assign("kwh_per_kw", var_data((ssc_number_t)kWhperkW));

		//ssc_number_t *hourly_fuel = as_array("q_aux_fuel", &count);//MWh
		//if( count != 8760 )
		//	throw exec_error("tcsmolten_salt", "q_aux_fuel count incorrect (should be 8760): " + count);
		//for( size_t i = 0; i < count; i++ )
		//	fuel_usage_mmbtu += hourly_fuel[i];
		assign("system_heat_rate", 0.0); // samsim tcstrough_physical
		// www.unitjuggler.com/convert-energy-from-MMBtu-to-kWh.html
		assign("annual_fuel_usage", 0.0);

	}

};

DEFINE_TCS_MODULE_ENTRY( tcsiscc, "Triple pressure NGCC integrated with MS power tower", 4 )
