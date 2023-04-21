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
#include "tckernel.h"
// for adjustment factors
#include "common.h"
// solarpilot header files
#include "AutoPilot_API.h"
#include "SolarField.h"
#include "IOUtil.h"
#include "csp_common.h"

static var_info _cm_vtab_tcsdirect_steam[] = {
/*	EXAMPLE LINES FOR INPUTS
    { SSC_INPUT,        SSC_NUMBER,      "XXXXXXXXXXXXXX",      "Label",                                                          "",             "",            "sca",            "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "INTINTINTINT",        "Label",                                                          "",             "",            "parasitic",      "*",                       "INTEGER",               "" },
    { SSC_INPUT,        SSC_ARRAY,       "XXXXXXXXXXX",         "Number indicating the receiver type",                            "",             "",            "hce",            "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "XXXXXXXXXXX",         "Label",                                                          "",             "",            "tes",            "*",                       "",                      "" },
*/														     
														     
//    VARTYPE           DATATYPE          NAME                   LABEL                                                            UNITS           META            GROUP            REQUIRED_IF                 CONSTRAINTS             UI_HINTS
	{ SSC_INPUT, SSC_STRING, "solar_resource_file", "local weather file path", "", "", "Weather", "*", "LOCAL_FILE", "" },
	
	{ SSC_INPUT, SSC_NUMBER, "system_capacity", "Nameplate capacity", "kW", "", "direct steam tower", "*", "", "" },
												     
    // TOU												     
    { SSC_INPUT,        SSC_MATRIX,      "weekday_schedule",    "12x24 Time of Use Values for week days",                         "",             "",            "tou_translator", "*",                       "",                      "" }, 
    { SSC_INPUT,        SSC_MATRIX,      "weekend_schedule",    "12x24 Time of Use Values for week end days",                     "",             "",            "tou_translator", "*",                       "",                      "" }, 

	// Heliostat field  parameters				     										
	{ SSC_INPUT, SSC_NUMBER, "run_type",              "Run type",                            "-",     "", "heliostat", "*", "", "" },
	{ SSC_INPUT, SSC_NUMBER, "helio_width",           "Heliostat width",                     "m",     "", "heliostat", "*", "", "" },
	{ SSC_INPUT, SSC_NUMBER, "helio_height",          "Heliostat height",                    "m",     "", "heliostat", "*", "", "" },
	{ SSC_INPUT, SSC_NUMBER, "helio_optical_error",   "Heliostat optical error",             "rad",   "", "heliostat", "*", "", "" },
	{ SSC_INPUT, SSC_NUMBER, "helio_active_fraction", "Heliostat active frac.",              "",      "", "heliostat", "*", "", "" },
	{ SSC_INPUT, SSC_NUMBER, "helio_reflectance",     "Heliostat reflectance",               "",      "", "heliostat", "*", "", "" },
	{ SSC_INPUT, SSC_NUMBER, "rec_absorptance",       "Receiver absorptance",                "",      "", "heliostat", "*", "", "" },
	{ SSC_INPUT, SSC_NUMBER, "rec_aspect",            "Receiver aspect ratio",               "",      "", "heliostat", "*", "", "" },
    { SSC_INPUT, SSC_NUMBER, "rec_height",            "Receiver height",                     "m",     "", "heliostat", "*", "", "" },
	{ SSC_INPUT, SSC_NUMBER, "rec_hl_perm2",          "Receiver design heatloss",            "kW/m2", "", "heliostat", "*", "", "" },
	{ SSC_INPUT, SSC_NUMBER, "land_bound_type",       "Land boundary type",                  "",      "", "heliostat", "?=0", "", "" },
	{ SSC_INPUT, SSC_NUMBER, "land_max",              "Land max boundary",                   "-ORm",  "", "heliostat", "?=7.5", "", "" },
	{ SSC_INPUT, SSC_NUMBER, "land_min",              "Land min boundary",                   "-ORm",  "", "heliostat", "?=0.75", "", "" },
	{ SSC_INPUT, SSC_MATRIX, "land_bound_table",      "Land boundary table",                 "m",     "", "heliostat", "?", "", "" },
	{ SSC_INPUT, SSC_ARRAY, "land_bound_list",        "Boundary table listing",              "",      "", "heliostat", "?", "", "" },
	{ SSC_INPUT, SSC_NUMBER, "dni_des",               "Design-point DNI",                    "W/m2",  "", "heliostat", "*", "", "" },
	{ SSC_INPUT, SSC_NUMBER, "p_start",               "Heliostat startup energy",            "kWe-hr","", "heliostat", "*", "", "" },
	{ SSC_INPUT, SSC_NUMBER, "p_track",               "Heliostat tracking energy",           "kWe",   "", "heliostat", "*", "", "" },
	{ SSC_INPUT, SSC_NUMBER, "hel_stow_deploy",       "Stow/deploy elevation",               "deg",   "", "heliostat", "*", "", "" },
	{ SSC_INPUT, SSC_NUMBER, "v_wind_max",            "Max. wind velocity",                  "m/s",   "", "heliostat", "*", "", "" },
	{ SSC_INPUT, SSC_NUMBER, "interp_nug",            "Interpolation nugget",                "",      "", "heliostat", "?=0", "", "" },
	{ SSC_INPUT, SSC_NUMBER, "interp_beta",           "Interpolation beta coef.",            "",      "", "heliostat", "?=1.99", "", "" },
	{ SSC_INPUT, SSC_NUMBER, "n_flux_x",              "Flux map X resolution",               "",      "", "heliostat", "?=12", "", "" },
	{ SSC_INPUT, SSC_NUMBER, "n_flux_y",              "Flux map Y resolution",               "",      "", "heliostat", "?=1", "", "" },
    { SSC_INPUT, SSC_NUMBER, "dens_mirror",           "Ratio of reflective area to profile", "",      "", "heliostat", "*", "", ""},
	{ SSC_INPUT, SSC_MATRIX, "helio_positions",       "Heliostat position table",            "m",     "", "heliostat", "run_type=1", "", "" },
	{ SSC_INPUT, SSC_MATRIX, "helio_aim_points",      "Heliostat aim point table",           "m",     "", "heliostat", "?", "", "" },
	{ SSC_INPUT, SSC_NUMBER, "N_hel",                 "Number of heliostats",                "",      "", "heliostat", "?", "", "" },
	{ SSC_INPUT, SSC_MATRIX, "eta_map",               "Field efficiency array",              "",      "", "heliostat", "?", "", "" },
	{ SSC_INPUT, SSC_MATRIX, "flux_positions",        "Flux map sun positions",              "deg",   "", "heliostat", "?", "", "" },
	{ SSC_INPUT, SSC_MATRIX, "flux_maps",             "Flux map intensities",                "",      "", "heliostat", "?", "", "" },
	{ SSC_INPUT, SSC_NUMBER, "c_atm_0",               "Attenuation coefficient 0",           "",      "", "heliostat", "?=0.006789", "", "" },
	{ SSC_INPUT, SSC_NUMBER, "c_atm_1",               "Attenuation coefficient 1",           "",      "", "heliostat", "?=0.1046", "", "" },
	{ SSC_INPUT, SSC_NUMBER, "c_atm_2",               "Attenuation coefficient 2",           "",      "", "heliostat", "?=-0.0107", "", "" },
	{ SSC_INPUT, SSC_NUMBER, "c_atm_3",               "Attenuation coefficient 3",           "",      "", "heliostat", "?=0.002845", "", "" },
	{ SSC_INPUT, SSC_NUMBER, "n_facet_x",             "Number of heliostat facets - X",      "",      "", "heliostat", "*", "", "" },
	{ SSC_INPUT, SSC_NUMBER, "n_facet_y",             "Number of heliostat facets - Y",      "",      "", "heliostat", "*", "", "" },
	{ SSC_INPUT, SSC_NUMBER, "focus_type",            "Heliostat focus method",              "",      "", "heliostat", "*", "", "" },
	{ SSC_INPUT, SSC_NUMBER, "cant_type",             "Heliostat cant method",               "",      "", "heliostat", "*", "", "" },
	{ SSC_INPUT, SSC_NUMBER, "n_flux_days",           "No. days in flux map lookup",         "",      "", "heliostat", "?=8", "", "" },
	{ SSC_INPUT, SSC_NUMBER, "delta_flux_hrs",        "Hourly frequency in flux map lookup", "",      "", "heliostat", "?=1", "", "" },
	{ SSC_INPUT, SSC_NUMBER, "water_usage_per_wash",  "Water usage per wash",                "L/m2_aper", "", "heliostat", "*", "", "" },
	{ SSC_INPUT, SSC_NUMBER, "washing_frequency",     "Mirror washing frequency",            "",          "", "heliostat", "*", "", "" },
	
	{ SSC_INPUT, SSC_NUMBER, "H_rec",     "The height of the receiver",                          "m", "", "receiver",  "*", "", "" },
	{ SSC_INPUT, SSC_NUMBER, "THT",       "The height of the tower (hel. pivot to rec equator)", "m", "", "receiver",  "*", "", "" },

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

	// Direct steam controller (type 265) parameters	       
    { SSC_INPUT,        SSC_NUMBER,      "fossil_mode",         "Fossil model: 1=Normal, 2=Supplemental",                           "-",            "",            "dsg_controller", "*",                       "INTEGER",               "" },
    { SSC_INPUT,        SSC_NUMBER,      "q_pb_design",         "Heat rate into powerblock at design",                              "MW",           "",            "dsg_controller", "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "q_aux_max",           "Maximum heat rate of auxiliary heater",                            "MW",           "",            "dsg_controller", "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "lhv_eff",             "Aux Heater lower heating value efficiency",                        "-",            "",            "dsg_controller", "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "h_tower",             "Tower Height",                                                     "m",            "",            "dsg_controller", "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "n_panels",            "Number of panels",                                                 "-",            "",            "dsg_controller", "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "flowtype",            "Code for flow pattern through rec.",                               "-",            "",            "dsg_controller", "*",                       "INTEGER",               "" },
    { SSC_INPUT,        SSC_NUMBER,      "d_rec",               "Diameter of Receiver",                                             "m",            "",            "dsg_controller", "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "q_rec_des",           "Design-point thermal power",                                       "MW",           "",            "dsg_controller", "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "f_rec_min",           "Minimum receiver absorbed power fraction",                         "-",            "",            "dsg_controller", "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "rec_qf_delay",        "Receiver start-up delay fraction of thermal energy of receiver running at design for 1 hour", "-", "", "dsg_controller", "*",                  "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "rec_su_delay",        "Receiver start-up delay time",                                     "hr",           "",            "dsg_controller", "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "f_pb_cutoff",         "Cycle cut-off fraction",                                           "-",            "",            "dsg_controller", "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "f_pb_sb",             "Cycle minimum standby fraction",                                   "-",            "",            "dsg_controller", "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "t_standby_ini",       "Power block standby time",                                         "hr",           "",            "dsg_controller", "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "x_b_target",          "Target boiler outlet quality",                                     "-",            "",            "dsg_controller", "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "eta_rec_pump",        "Feedwater pump efficiency",                                        "-",            "",            "dsg_controller", "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "P_hp_in_des",         "Design HP Turbine Inlet Pressure",                                 "bar",          "",            "dsg_controller", "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "P_hp_out_des",        "Design HP Turbine Outlet Pressure",                                "bar",          "",            "dsg_controller", "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "f_mdotrh_des",        "Design reheat mass flow rate fraction",                            "-",            "",            "dsg_controller", "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "p_cycle_design",      "Design Cycle Power",                                               "MW",           "",            "dsg_controller", "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "ct",                  "Cooling Type",                                                     "-",            "",            "dsg_controller", "*",                       "INTEGER",               "" },
    { SSC_INPUT,        SSC_NUMBER,      "T_amb_des",           "Design ambient temperature (power cycle)",                         "C",            "",            "dsg_controller", "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "dT_cw_ref",           "Reference condenser water dT",                                     "C",            "",            "dsg_controller", "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "T_approach",          "Approach temperature for wet cooling",                             "C",            "",            "dsg_controller", "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "T_ITD_des",           "Approach temperature for dry cooling",                             "C",            "",            "dsg_controller", "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "hl_ffact",            "Heat Loss Fudge FACTor",                                           "-",            "",            "dsg_controller", "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "h_boiler",            "Height of boiler",                                                 "m",            "",            "dsg_controller", "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "d_t_boiler",          "O.D. of boiler tubes",                                             "m",            "",            "dsg_controller", "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "th_t_boiler",         "Thickness of boiler tubes",                                        "m",            "",            "dsg_controller", "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "rec_emis",            "Emissivity of receiver tubes",                                     "-",            "",            "dsg_controller", "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "rec_absorptance",     "Absorptance of receiver tubes",                                    "-",            "",            "dsg_controller", "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "mat_boiler",          "Numerical code for tube material",                                 "-",            "",            "dsg_controller", "*",                       "INTEGER",               "" },
    { SSC_INPUT,        SSC_NUMBER,      "h_sh",                "Height of superheater",                                            "m",            "",            "dsg_controller", "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "d_sh",                "O.D. of superheater tubes",                                        "m",            "",            "dsg_controller", "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "th_sh",               "Thickness of superheater tubes",                                   "m",            "",            "dsg_controller", "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "mat_sh",              "Numerical code for superheater material",                          "-",            "",            "dsg_controller", "*",                       "INTEGER",               "" },
    { SSC_INPUT,        SSC_NUMBER,      "T_sh_out_des",        "Target superheater outlet temperature",                            "C",            "",            "dsg_controller", "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "h_rh",                "Height of reheater",                                               "m",            "",            "dsg_controller", "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "d_rh",                "O.D. of reheater tubes",                                           "m",            "",            "dsg_controller", "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "th_rh",               "Thickness of reheater tubes",                                      "m",            "",            "dsg_controller", "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "mat_rh",              "Numerical code for reheater material",                             "-",            "",            "dsg_controller", "*",                       "INTEGER",               "" },
    { SSC_INPUT,        SSC_NUMBER,      "T_rh_out_des",        "Target reheater outlet temperature",                               "C",            "",            "dsg_controller", "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "cycle_max_frac",      "Cycle maximum overdesign fraction",                                "-",            "",            "dsg_controller", "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "A_sf",                "Solar field area",                                                 "m^2",          "",            "dsg_controller", "*",                       "",                      "" },
	{ SSC_INPUT,        SSC_ARRAY,       "ffrac",               "Fossil dispatch logic",                                            "-",            "",            "dsg_controller", "*",                       "",                      "" },

	// Direct steam controller (type 265) inputs		       
    { SSC_INPUT,        SSC_NUMBER,      "P_b_in_init",         "Initial Boiler inlet pressure",                                    "bar",          "",            "dsg_controller", "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "f_mdot_rh_init",      "Reheat mass flow rate fraction",                                   "-",            "",            "dsg_controller", "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "P_hp_out",            "HP turbine outlet pressure",                                       "bar",          "",            "dsg_controller", "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "T_hp_out",            "HP turbine outlet temperature",                                    "C",            "",            "dsg_controller", "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "T_rh_target",         "Target reheater outlet temp.",                                     "C",            "",            "dsg_controller", "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "T_fw_init",           "Initial Feedwater outlet temperature",                             "C",            "",            "dsg_controller", "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "P_cond_init",         "Condenser pressure",                                               "Pa",           "",            "dsg_controller", "*",                       "",                      "" },
														     
	// Power block (type 234) parameters				     
    { SSC_INPUT,        SSC_NUMBER,      "P_ref",               "Reference output electric power at design condition",               "MW",          "",            "powerblock",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "eta_ref",             "Reference conversion efficiency at design condition",               "none",        "",            "powerblock",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "T_hot_ref",           "Reference HTF inlet temperature at design",                         "C",           "",            "powerblock",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "T_cold_ref",          "Reference HTF outlet temperature at design",                        "C",           "",            "powerblock",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "dT_cw_ref",           "Reference condenser cooling water inlet/outlet T diff",             "C",           "",            "powerblock",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "T_amb_des",           "Reference ambient temperature at design point",                     "C",           "",            "powerblock",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "q_sby_frac",          "Fraction of thermal power required for standby mode",               "none",        "",            "powerblock",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "P_boil_des",          "Boiler operating pressure @ design",                                "bar",         "",            "powerblock",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "P_rh_ref",            "Reheater operating pressure at design",                             "bar",         "",            "powerblock",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "rh_frac_ref",         "Reheater flow fraction at design",                                  "none",        "",            "powerblock",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "startup_time",        "Time needed for power block startup",                               "hr",          "",            "powerblock",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "startup_frac",        "Fraction of design thermal power needed for startup",               "none",        "",            "powerblock",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "T_ITD_des",           "ITD at design for dry system",                                      "C",           "",            "powerblock",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "P_cond_ratio",        "Condenser pressure ratio",                                          "none",        "",            "powerblock",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "pb_bd_frac",          "Power block blowdown steam fraction ",                              "none",        "",            "powerblock",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "P_cond_min",          "Minimum condenser pressure",                                        "inHg",        "",            "powerblock",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "n_pl_inc",            "Number of part-load increments for the heat rejection system",      "none",        "",            "powerblock",     "*",                       "INTEGER",               "" },
    { SSC_INPUT,        SSC_ARRAY,       "F_wc",                "Fraction indicating wet cooling use for hybrid system",             "none",        "",            "powerblock",     "*",                       "",                      "" },
	// Power block (type 234) inputs					       
    { SSC_INPUT,        SSC_NUMBER,      "T_hot",               "Hot HTF inlet temperature, from storage tank",                      "C",           "",            "powerblock",     "*",                       "",                      "" },
															  
	// Parasitics (type 228) parameters						  
    {SSC_INPUT,         SSC_NUMBER,      "Piping_loss",         "Thermal loss per meter of piping",                                  "Wt/m",        "",            "parasitics",     "*",                       "",                      "" },
    {SSC_INPUT,         SSC_NUMBER,      "Piping_length",       "Total length of exposed piping",                                    "m",           "",            "parasitics",     "*",                       "",                      "" },
    {SSC_INPUT,         SSC_NUMBER,      "piping_length_mult",  "Piping length multiplier",                                          "",            "",            "parasitics",     "*",                       "",                      "" },
    {SSC_INPUT,         SSC_NUMBER,      "piping_length_add",   "Piping constant length",                                            "m",           "",            "parasitics",     "*",                       "",                      "" },
    {SSC_INPUT,         SSC_NUMBER,      "Design_power",        "Power production at design conditions",                             "MWe",         "",            "parasitics",     "*",                       "",                      "" },
    {SSC_INPUT,         SSC_NUMBER,      "design_eff",          "Power cycle efficiency at design",                                  "none",        "",            "parasitics",     "*",                       "",                      "" },
    {SSC_INPUT,         SSC_NUMBER,      "pb_fixed_par",        "Fixed parasitic load - runs at all times",                          "MWe/MWcap",   "",            "parasitics",     "*",                       "",                      "" },
    {SSC_INPUT,         SSC_NUMBER,      "aux_par",             "Aux heater, boiler parasitic",                                      "MWe/MWcap",   "",            "parasitics",     "*",                       "",                      "" },
    {SSC_INPUT,         SSC_NUMBER,      "aux_par_f",           "Aux heater, boiler parasitic - multiplying fraction",               "none",        "",            "parasitics",     "*",                       "",                      "" },
    {SSC_INPUT,         SSC_NUMBER,      "aux_par_0",           "Aux heater, boiler parasitic - constant coefficient",               "none",        "",            "parasitics",     "*",                       "",                      "" },
    {SSC_INPUT,         SSC_NUMBER,      "aux_par_1",           "Aux heater, boiler parasitic - linear coefficient",                 "none",        "",            "parasitics",     "*",                       "",                      "" },
    {SSC_INPUT,         SSC_NUMBER,      "aux_par_2",           "Aux heater, boiler parasitic - quadratic coefficient",              "none",        "",            "parasitics",     "*",                       "",                      "" },
    {SSC_INPUT,         SSC_NUMBER,      "bop_par",             "Balance of plant parasitic power fraction",                         "MWe/MWcap",   "",            "parasitics",     "*",                       "",                      "" },
    {SSC_INPUT,         SSC_NUMBER,      "bop_par_f",           "Balance of plant parasitic power fraction - mult frac",             "none",        "",            "parasitics",     "*",                       "",                      "" },
    {SSC_INPUT,         SSC_NUMBER,      "bop_par_0",           "Balance of plant parasitic power fraction - const coeff",           "none",        "",            "parasitics",     "*",                       "",                      "" },
    {SSC_INPUT,         SSC_NUMBER,      "bop_par_1",           "Balance of plant parasitic power fraction - linear coeff",          "none",        "",            "parasitics",     "*",                       "",                      "" },
    {SSC_INPUT,         SSC_NUMBER,      "bop_par_2",           "Balance of plant parasitic power fraction - quadratic coeff",       "none",        "",            "parasitics",     "*",                       "",                      "" },

// OUTPUTS
// The names of the output variables should match the parameter names for the TCS units in order to signal to the TCS kernel to store the values by timestep

	// VARTYPE          DATATYPE          NAME                 LABEL                                                            UNITS           META            GROUP            REQUIRED_IF                 CONSTRAINTS             UI_HINTS
    { SSC_OUTPUT,       SSC_ARRAY,       "month",             "Resource Month",                                                  "",             "",            "weather",        "*",                       "LENGTH=8760",           "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "hour",              "Resource Hour of Day",                                            "",             "",            "weather",        "*",                       "LENGTH=8760",           "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "solazi",            "Resource Solar Azimuth",                                          "deg",          "",            "weather",        "*",                       "LENGTH=8760",           "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "solzen",            "Resource Solar Zenith",                                           "deg",          "",            "weather",        "*",                       "LENGTH=8760",           "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "beam",              "Resource Beam normal irradiance",                                 "W/m2",         "",            "weather",        "*",                       "LENGTH=8760",           "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "tdry",              "Resource Dry bulb temperature",                                   "C",            "",            "weather",        "*",                       "LENGTH=8760",           "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "twet",              "Resource Wet bulb temperature",                                   "C",            "",            "weather",        "*",                       "LENGTH=8760",           "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "wspd",              "Resource Wind Speed",                                             "m/s",          "",            "weather",        "*",                       "LENGTH=8760",           "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "pres",              "Resource Pressure",                                               "mbar",         "",            "weather",        "*",                       "LENGTH=8760",           "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "tou_value",         "Resource Time-of-use value",                                      "",             "",            "tou",            "*",                       "LENGTH=8760",           "" },

    //heliostat field
    { SSC_OUTPUT,       SSC_ARRAY,       "eta_field",            "Field optical efficiency",                                     "",             "",            "Outputs",        "*",                       "LENGTH=8760",           "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "defocus",              "Field optical focus fraction",                                 "",             "",            "Outputs",        "*",                       "LENGTH=8760",           "" },

    //Receiver
	{ SSC_OUTPUT,       SSC_ARRAY,       "q_b_conv",            "Receiver boiler power loss to convection",                      "MWt",           "",            "Outputs",        "*",                       "LENGTH=8760",           "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "q_b_rad",             "Receiver boiler power loss to radiation",                       "MWt",           "",            "Outputs",        "*",                       "LENGTH=8760",           "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "q_b_abs",             "Receiver boiler power absorbed",                                "MWt",           "",            "Outputs",        "*",                       "LENGTH=8760",           "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "P_b_in",              "Receiver boiler pressure inlet",                                "kPa",          "",            "Outputs",        "*",                       "LENGTH=8760",           "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "P_b_out",             "Receiver boiler pressure outlet",                               "kPa",          "",            "Outputs",        "*",                       "LENGTH=8760",           "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "P_drop_b",            "Receiver boiler pressure drop",                                 "Pa",           "",            "Outputs",        "*",                       "LENGTH=8760",           "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "eta_b",               "Receiver boiler thermal efficiency",                            "",            "",            "Outputs",        "*",                       "LENGTH=8760",           "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "T_b_in",              "Receiver boiler temperature inlet",                             "C",            "",            "Outputs",        "*",                       "LENGTH=8760",           "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "T_boiling",           "Receiver boiler temperature drum",                              "C",            "",            "Outputs",        "*",                       "LENGTH=8760",           "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "T_max_b_surf",        "Receiver boiler temperature surface max",                       "C",            "",            "Outputs",        "*",                       "LENGTH=8760",           "" },
	
    { SSC_OUTPUT,       SSC_ARRAY,       "m_dot_sh",            "Receiver superheater mass flow rate",                           "kg/hr",        "",            "Outputs",        "*",                       "LENGTH=8760",           "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "q_sh_conv",           "Receiver superheater power loss to convection",                 "MWt",           "",            "Outputs",        "*",                       "LENGTH=8760",           "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "q_sh_rad",            "Receiver superheater power loss to radiation",                  "MWt",           "",            "Outputs",        "*",                       "LENGTH=8760",           "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "q_sh_abs",            "Receiver superheater power absorbed",                           "MWt",           "",            "Outputs",        "*",                       "LENGTH=8760",           "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "P_sh_out",            "Receiver superheater pressure outlet",                          "kPa",          "",            "Outputs",        "*",                       "LENGTH=8760",           "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "dP_sh",               "Receiver superheater pressure drop",                            "Pa",           "",            "Outputs",        "*",                       "LENGTH=8760",           "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "T_max_sh_surf",       "Receiver superheater temperature surface max",                  "C",            "",            "Outputs",        "*",                       "LENGTH=8760",           "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "eta_sh",              "Receiver superheater thermal efficiency",                       "",             "",            "Outputs",        "*",                       "LENGTH=8760",           "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "v_sh_max",            "Receiver superheater velocity at outlet",                       "m/s",          "",            "Outputs",        "*",                       "LENGTH=8760",           "" },
	
    { SSC_OUTPUT,       SSC_ARRAY,       "f_mdot_rh",           "Receiver reheater mass flow rate fraction",                     "-",            "",            "Outputs",        "*",                       "LENGTH=8760",           "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "q_rh_conv",           "Receiver reheater power loss to convection",                    "MWt",           "",            "Outputs",        "*",                       "LENGTH=8760",           "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "q_rh_rad",            "Receiver reheater power loss to radiation",                     "MWt",           "",            "Outputs",        "*",                       "LENGTH=8760",           "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "q_rh_abs",            "Receiver reheater power absorbed",                              "MWt",           "",            "Outputs",        "*",                       "LENGTH=8760",           "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "P_rh_in",             "Receiver reheater pressure inlet",                              "kPa",          "",            "Outputs",        "*",                       "LENGTH=8760",           "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "P_rh_out",            "Receiver reheater pressure outlet",                             "kPa",          "",            "Outputs",        "*",                       "LENGTH=8760",           "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "dP_rh",               "Receiver reheater pressure drop",                               "Pa",           "",            "Outputs",        "*",                       "LENGTH=8760",           "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "eta_rh",              "Receiver reheater thermal efficiency",                          "-",            "",            "Outputs",        "*",                       "LENGTH=8760",           "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "T_rh_in",             "Receiver reheater temperature inlet",                           "C",            "",            "Outputs",        "*",                       "LENGTH=8760",           "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "T_rh_out",            "Receiver reheater temperature outlet",                          "C",            "",            "Outputs",        "*",                       "LENGTH=8760",           "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "T_max_rh_surf",       "Receiver reheater temperature surface max",                     "C",            "",            "Outputs",        "*",                       "LENGTH=8760",           "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "v_rh_max",            "Receiver reheater velocity at outlet",                          "m/s",          "",            "Outputs",        "*",                       "LENGTH=8760",           "" },
	
    { SSC_OUTPUT,       SSC_ARRAY,       "q_inc_full",          "Receiver power incident (excl. defocus)",                       "MWt",           "",            "Outputs",        "*",                       "LENGTH=8760",           "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "q_abs_rec",           "Receiver power absorbed total",                                 "MWt",           "",            "Outputs",        "*",                       "LENGTH=8760",           "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "q_rad_rec",           "Receiver power loss to radiation total",                        "MWt",           "",            "Outputs",        "*",                       "LENGTH=8760",           "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "q_conv_rec",          "Receiver power loss to convection total",                       "MWt",           "",            "Outputs",        "*",                       "LENGTH=8760",           "" },
	//{ SSC_OUTPUT,       SSC_ARRAY,       "q_abs_less_rad",      "Receiver->Comb: Power to receiver less radiation",               "MW",           "",            "Outputs",        "*",                       "LENGTH=8760",           "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "q_therm_in_rec",      "Receiver power to steam total",                                 "MWt",           "",            "Outputs",        "*",                       "LENGTH=8760",           "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "eta_rec",             "Receiver thermal efficiency",                                   "",            "",            "Outputs",        "*",                       "LENGTH=8760",           "" },
	
    //Fossil backup
    { SSC_OUTPUT,       SSC_ARRAY,       "m_dot_aux",           "Auxiliary mass flow rate",                                      "kg/hr",        "",            "Outputs",        "*",                       "LENGTH=8760",           "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "q_aux",               "Auxiliary heat rate delivered to cycle",                        "MW",           "",            "Outputs",        "*",                       "LENGTH=8760",           "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "q_aux_fuel",          "Fuel energy rate to aux heater",                                "MMBTU",        "",            "Outputs",        "*",                       "LENGTH=8760",           "" },
	
    //power cycle
	{ SSC_OUTPUT,       SSC_ARRAY,       "P_out_net",         "Cycle electrical power output (net)",                             "MWe",          "",            "Net_E_Calc",     "*",                       "LENGTH=8760",           "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "P_cycle",           "Cycle electrical power output (gross)",                           "MWe",          "",            "Net_E_Calc",     "*",                       "LENGTH=8760",           "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "T_fw",              "Cycle temperature feedwater outlet",                              "C",            "",            "Outputs",        "*",                       "LENGTH=8760",           "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "m_dot_makeup",      "Cycle mass flow rate cooling water makeup",                       "kg/hr",        "",            "Type250",        "*",                       "LENGTH=8760",           "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "P_cond",            "Condenser pressure",                                              "Pa",           "",            "Type250",        "*",                       "LENGTH=8760",           "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "f_bays",            "Condenser fraction of operating bays",                            "",         "",            "Type250",        "*",                       "LENGTH=8760",           "" },

    //parasitics
	{ SSC_OUTPUT,       SSC_ARRAY,       "W_dot_boost",          "Parasitic power receiver boost pump",                            "MWe",          "",            "Outputs",        "*",                       "LENGTH=8760",           "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "pparasi",              "Parasitic power heliostat drives",                               "MWe",          "",            "Outputs",        "*",                       "LENGTH=8760",           "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "P_plant_balance_tot",  "Parasitic power generation-dependent load",                      "MWe",          "",            "Outputs",        "*",                       "LENGTH=8760",           "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "P_fixed",              "Parasitic power fixed load",                                     "MWe",          "",            "Outputs",        "*",                       "LENGTH=8760",           "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "P_cooling_tower_tot",  "Parasitic power condenser operation",                            "MWe",          "",            "Outputs",        "*",                       "LENGTH=8760",           "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "P_piping_tot",         "Parasitic power equiv. header pipe losses",                      "MWe",          "",            "Outputs",        "*",                       "LENGTH=8760",           "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "P_parasitics",         "Parasitic power total consumption",                              "MWe",          "",            "Outputs",        "*",                       "LENGTH=8760",           "" },

//	{ SSC_OUTPUT, SSC_ARRAY, "hourly_energy", "Hourly Energy", "kW", "", "Net_E_Calc", "*", "LENGTH=8760", "" },

	// Annual Outputs
	{ SSC_OUTPUT, SSC_NUMBER, "annual_energy",         "Annual Energy",                                      "kWh",    "", "Type228",        "*",  "", "" },
	{ SSC_OUTPUT, SSC_NUMBER, "annual_W_cycle_gross",  "Electrical source - Power cycle gross output",       "kWh",    "", "Type228",        "*",  "", "" },
	{ SSC_OUTPUT, SSC_NUMBER, "annual_total_water_use","Total Annual Water Usage: cycle + mirror washing",   "m3",     "", "PostProcess",    "*",  "", "" },


	{ SSC_OUTPUT, SSC_NUMBER, "conversion_factor",    "Gross to Net Conversion Factor",               "%",         "", "Calculated", "*", "", "" },
	{ SSC_OUTPUT, SSC_NUMBER, "capacity_factor",      "Capacity factor",                              "%",         "", "", "*", "", "" },
	{ SSC_OUTPUT, SSC_NUMBER, "kwh_per_kw",           "First year kWh/kW",                            "kWh/kW",    "", "", "*", "", "" },
	{ SSC_OUTPUT, SSC_NUMBER, "system_heat_rate",     "System heat rate",                             "MMBtu/MWh", "", "", "*", "", "" },
	{ SSC_OUTPUT, SSC_NUMBER, "annual_fuel_usage",    "Annual fuel usage",                            "kWh",       "", "", "*", "", "" },

	var_info_invalid };

class cm_tcsdirect_steam : public tcKernel
{
public:

	cm_tcsdirect_steam(tcstypeprovider *prov)
	:tcKernel(prov)
	{
		add_var_info( _cm_vtab_tcsdirect_steam );
		//set_store_all_parameters(true); // default is 'false' = only store TCS parameters that match the SSC_OUTPUT variables above
		// performance adjustment factors
		add_var_info(vtab_adjustment_factors);
		add_var_info(vtab_technology_outputs);
        add_var_info(vtab_sf_adjustment_factors);
	}

	void exec( )
	{
		//bool debug_mode = (__DEBUG__ == 1);  // When compiled in VS debug mode, this will use the trnsys weather file; otherwise, it will attempt to open the file with name that was passed in
		bool debug_mode = false;
		//Add weather file reader unit
		int weather = 0;
		if(debug_mode) weather = add_unit("trnsys_weatherreader", "TRNSYS weather reader");
		else weather = add_unit("weatherreader", "TCS weather reader");

		// Add units
		int	tou = add_unit("tou_translator", "Time of Use Translator");
		int type_hel_field = add_unit("sam_mw_pt_heliostatfield");
		int type265_dsg_controller = add_unit("sam_dsg_controller_type265");
		int type234_powerblock = add_unit("sam_mw_type234");
		int type228_parasitics = add_unit("sam_mw_pt_type228");

		if(debug_mode)
		{
			set_unit_value( weather, "file_name", "C:/svn_NREL/main/ssc/tcsdata/typelib/TRNSYS_weather_outputs/daggett_trnsys_weather.out" );
			set_unit_value( weather, "i_hour", "TIME" );
			set_unit_value( weather, "i_month", "month" );
			set_unit_value( weather, "i_day", "day" );
			set_unit_value( weather, "i_global", "GlobalHorizontal" );
			set_unit_value( weather, "i_beam", "DNI" );
			set_unit_value( weather, "i_diff", "DiffuseHorizontal" );
			set_unit_value( weather, "i_tdry", "T_dry" );
			set_unit_value( weather, "i_twet", "T_wet" );
			set_unit_value( weather, "i_tdew", "T_dew" );
			set_unit_value( weather, "i_wspd", "WindSpeed" );
			set_unit_value( weather, "i_wdir", "WindDir" );
			set_unit_value( weather, "i_rhum", "RelHum" );
			set_unit_value( weather, "i_pres", "AtmPres" );
			set_unit_value( weather, "i_snow", "SnowCover" );
			set_unit_value( weather, "i_albedo", "GroundAlbedo" );
			set_unit_value( weather, "i_poa", "POA" );
			set_unit_value( weather, "i_solazi", "Azimuth" );
			set_unit_value( weather, "i_solzen", "Zenith" );
			set_unit_value( weather, "i_lat", "Latitude" );
			set_unit_value( weather, "i_lon", "Longitude" );
			set_unit_value( weather, "i_shift", "Shift" );
		}
		else
		{
			//Set weatherreader parameters
			set_unit_value(weather, "file_name", as_string("solar_resource_file"));
			set_unit_value(weather, "track_mode", 0.0);
			set_unit_value(weather, "tilt", 0.0);
			set_unit_value(weather, "azimuth", 0.0);
		}

		set_unit_value_ssc_matrix(tou, "weekday_schedule"); // tou values from control will be between 1 and 9
		set_unit_value_ssc_matrix(tou, "weekend_schedule");

		// Heliostat field
		set_unit_value_ssc_double(type_hel_field, "run_type");// , 0);	//0=auto, 1=user-type_hel_field, 2=user data
		set_unit_value_ssc_double(type_hel_field, "helio_width");//, 12.);
		set_unit_value_ssc_double(type_hel_field, "helio_height");//, 12.);
		set_unit_value_ssc_double(type_hel_field, "helio_optical_error");//, 0.00153);
		set_unit_value_ssc_double(type_hel_field, "helio_active_fraction");//, 0.97);
		set_unit_value_ssc_double(type_hel_field, "helio_reflectance");//, 0.90);
		set_unit_value_ssc_double(type_hel_field, "rec_absorptance");//, 0.94);

                
        bool is_optimize = as_boolean("is_optimize");
        
        /* 
        Any parameter that's dependent on the size of the solar field must be recalculated here 
        if the optimization is happening within the cmod
        */
        double H_rec, d_rec, rec_aspect, THT, A_sf;

        if(is_optimize)
        {
            message("Sorry, auto-optimization of Direct Steam systems is still under development and not yet available. Please optimize within the Solar Field page!", SSC_ERROR);
            return;


            //Run solarpilot right away to update values as needed
            solarpilot_invoke spi( this );
            spi.run();
            //AutoPilot_S *sapi = spi.GetSAPI();

            H_rec = spi.recs.front().rec_height.val;
            rec_aspect = spi.recs.front().rec_aspect.Val();
            THT = spi.sf.tht.val;
            //update heliostat position table
            int nr = (int)spi.layout.heliostat_positions.size();
            ssc_number_t *ssc_hl = allocate( "helio_positions", nr, 2 );
            for(int i=0; i<nr; i++){
                ssc_hl[i*2] = (ssc_number_t)spi.layout.heliostat_positions.at(i).location.x;
                ssc_hl[i*2+1] = (ssc_number_t)spi.layout.heliostat_positions.at(i).location.y;
            }

            A_sf = as_double("helio_height") * as_double("helio_width") * as_double("dens_mirror") * (double)nr;

            //update piping length for parasitic calculation
            double piping_length = THT * as_double("piping_length_mult") + as_double("piping_length_add");
            
            //update assignments for cost model
		    assign("H_rec", var_data((ssc_number_t)H_rec));
            assign("rec_height", var_data((ssc_number_t)H_rec));
		    assign("rec_aspect", var_data((ssc_number_t)rec_aspect));
            assign("d_rec", var_data((ssc_number_t)(H_rec/rec_aspect)));
		    assign("THT", var_data((ssc_number_t)THT));
            assign("h_tower", var_data((ssc_number_t)THT));
		    assign("A_sf", var_data((ssc_number_t)A_sf));
            assign("Piping_length", var_data((ssc_number_t)piping_length) );

            //Update the total installed cost
            double total_direct_cost = 0.;
            double A_rec;
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
            //case Receiver::REC_TYPE::CAVITY:
				double h = spi.recs.front().rec_height.val;
				double w = h / spi.recs.front().rec_aspect.Val();
				A_rec = h*w;
				break;
			}
            double receiver = as_double("rec_ref_cost")*pow(A_rec/as_double("rec_ref_area"), as_double("rec_cost_exp"));     //receiver cost

            //storage cost
            double storage = as_double("q_pb_design")*as_double("tshours")*as_double("tes_spec_cost")*1000.;

            //power block + BOP
            double P_ref = as_double("P_ref") * 1000.;  //kWe
            double power_block = P_ref * (as_double("plant_spec_cost") + as_double("bop_spec_cost") ); //$/kWe --> $

            //site improvements
            double site_improvements = A_sf * as_double("site_spec_cost");
            
            //heliostats
            double heliostats = A_sf * as_double("heliostat_spec_cost");
            
            //fixed cost
            double cost_fixed = as_double("cost_sf_fixed");

            //fossil
            double fossil = P_ref * as_double("fossil_spec_cost");

            //tower cost
            double tower = as_double("tower_fixed_cost") * exp( as_double("tower_exp") * (THT + 0.5*(-H_rec + as_double("helio_height")) ) );

            //---- total direct cost -----
            total_direct_cost = (1. + as_double("contingency_rate")/100.) * (
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
            double cost_sales_tax = as_double("sales_tax_rate")/100. * total_direct_cost * as_double("sales_tax_frac")/100.;

            //----- indirect cost
            double total_indirect_cost = cost_epc + cost_plm + cost_sales_tax;
            
            //----- total installed cost!
            double total_installed_cost = total_direct_cost + total_indirect_cost;
            assign("total_installed_cost", var_data((ssc_number_t)total_installed_cost ));
            
        }
        else
        {
		    H_rec = as_double("H_rec");
            rec_aspect = as_double("rec_aspect");
            THT = as_double("THT");
            A_sf = as_double("A_sf");
        }
        d_rec = H_rec/rec_aspect;

        set_unit_value_ssc_double(type_hel_field, "rec_height", H_rec);
		set_unit_value_ssc_double(type_hel_field, "rec_aspect", rec_aspect);
		set_unit_value_ssc_double(type_hel_field, "h_tower", THT);
		set_unit_value_ssc_double(type_hel_field, "rec_hl_perm2");
		set_unit_value_ssc_double(type_hel_field, "q_design", as_double("Q_rec_des"));
		set_unit_value_ssc_double(type_hel_field, "dni_des");
		set_unit_value(type_hel_field, "weather_file", as_string("solar_resource_file"));
		set_unit_value_ssc_double(type_hel_field, "land_bound_type");
		set_unit_value_ssc_double(type_hel_field, "land_max");
		set_unit_value_ssc_double(type_hel_field, "land_min");;
		set_unit_value_ssc_double(type_hel_field, "p_start");
		set_unit_value_ssc_double(type_hel_field, "p_track");
		set_unit_value_ssc_double(type_hel_field, "hel_stow_deploy");
		set_unit_value_ssc_double(type_hel_field, "v_wind_max");
		set_unit_value_ssc_double(type_hel_field, "n_flux_x");
		set_unit_value_ssc_double(type_hel_field, "n_flux_y");
        set_unit_value_ssc_double(type_hel_field, "dens_mirror");
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

		}
		else */
		if (run_type == 1){
			set_unit_value_ssc_matrix(type_hel_field, "helio_positions");
		}
		else if (run_type == 2){
			set_unit_value_ssc_matrix(type_hel_field, "eta_map");
			set_unit_value_ssc_matrix(type_hel_field, "flux_positions");
			set_unit_value_ssc_matrix(type_hel_field, "flux_maps");
		}

		// for user specified x,y field

		bool bConnected = connect(weather, "wspd", type_hel_field, "vwind");
		set_unit_value_ssc_double(type_hel_field, "field_control", 1.);
		set_unit_value_ssc_double(weather, "solzen", 90.);	//initialize to be on the horizon
		bConnected &= connect(weather, "solzen", type_hel_field, "solzen");
		bConnected &= connect(weather, "solazi", type_hel_field, "solaz");


        //Load the solar field adjustment factors
        sf_adjustment_factors sf_haf(this);
        if (!sf_haf.setup())
			throw exec_error("tcsgeneric_solar", "failed to setup sf adjustment factors: " + sf_haf.error());
        //allocate array to pass to tcs
        ssc_number_t *sf_adjust = allocate("sf_adjust", 8760);
        for( int i=0; i<8760; i++)
            sf_adjust[i] = sf_haf(i);
        set_unit_value_ssc_array(type_hel_field, "sf_adjust");


		//Set DSG Controller Parameters
		set_unit_value_ssc_double( type265_dsg_controller, "fossil_mode");
        set_unit_value_ssc_double( type265_dsg_controller, "q_pb_design");
		set_unit_value_ssc_double( type265_dsg_controller, "q_aux_max"); 
		set_unit_value_ssc_double( type265_dsg_controller, "lhv_eff"); 
		set_unit_value_ssc_double( type265_dsg_controller, "h_tower", THT); 
		set_unit_value_ssc_double( type265_dsg_controller, "n_panels"); 
		set_unit_value_ssc_double( type265_dsg_controller, "flowtype"); 
		set_unit_value_ssc_double( type265_dsg_controller, "d_rec", d_rec);
		set_unit_value_ssc_double( type265_dsg_controller, "q_rec_des");
		set_unit_value_ssc_double( type265_dsg_controller, "f_rec_min");
		set_unit_value_ssc_double( type265_dsg_controller, "rec_qf_delay");
		set_unit_value_ssc_double( type265_dsg_controller, "rec_su_delay");
		set_unit_value_ssc_double( type265_dsg_controller, "f_pb_cutoff");
		set_unit_value_ssc_double( type265_dsg_controller, "f_pb_sb");
		set_unit_value_ssc_double( type265_dsg_controller, "t_standby_ini");
		set_unit_value_ssc_double( type265_dsg_controller, "x_b_target");
		set_unit_value_ssc_double( type265_dsg_controller, "eta_rec_pump");
		set_unit_value_ssc_double( type265_dsg_controller, "P_hp_in_des");
		set_unit_value_ssc_double( type265_dsg_controller, "P_hp_out_des");
		set_unit_value_ssc_double( type265_dsg_controller, "f_mdotrh_des");
		set_unit_value_ssc_double( type265_dsg_controller, "p_cycle_design");
		set_unit_value_ssc_double( type265_dsg_controller, "ct");
		set_unit_value_ssc_double( type265_dsg_controller, "T_amb_des");
		set_unit_value_ssc_double( type265_dsg_controller, "dT_cw_ref");
		set_unit_value_ssc_double( type265_dsg_controller, "T_approach");
		set_unit_value_ssc_double( type265_dsg_controller, "T_ITD_des");
		set_unit_value_ssc_double( type265_dsg_controller, "hl_ffact");
		set_unit_value_ssc_double( type265_dsg_controller, "h_boiler");
		set_unit_value_ssc_double( type265_dsg_controller, "d_t_boiler");
		set_unit_value_ssc_double( type265_dsg_controller, "th_t_boiler");
		set_unit_value_ssc_double( type265_dsg_controller, "emis_boiler", "rec_emis");
		set_unit_value_ssc_double( type265_dsg_controller, "abs_boiler", "rec_absorptance");
		set_unit_value_ssc_double( type265_dsg_controller, "mat_boiler");
		
		set_unit_value_ssc_double( type265_dsg_controller, "h_sh");
		set_unit_value_ssc_double( type265_dsg_controller, "d_sh");
		set_unit_value_ssc_double( type265_dsg_controller, "th_sh");
		set_unit_value_ssc_double( type265_dsg_controller, "emis_sh", "rec_emis");
		set_unit_value_ssc_double( type265_dsg_controller, "abs_sh", "rec_absorptance");
		set_unit_value_ssc_double( type265_dsg_controller, "mat_sh");
		set_unit_value_ssc_double( type265_dsg_controller, "T_sh_out_des");
		set_unit_value_ssc_double( type265_dsg_controller, "h_rh");
		set_unit_value_ssc_double( type265_dsg_controller, "d_rh");
		set_unit_value_ssc_double( type265_dsg_controller, "th_rh");
		set_unit_value_ssc_double( type265_dsg_controller, "emis_rh", "rec_emis");
		set_unit_value_ssc_double( type265_dsg_controller, "abs_rh", "rec_absorptance");
		set_unit_value_ssc_double( type265_dsg_controller, "mat_rh");
		set_unit_value_ssc_double( type265_dsg_controller, "T_rh_out_des");
		set_unit_value_ssc_double( type265_dsg_controller, "cycle_max_frac");
		set_unit_value_ssc_double( type265_dsg_controller, "A_sf", A_sf );
		set_unit_value_ssc_array( type265_dsg_controller, "ffrac");
		set_unit_value_ssc_double(type265_dsg_controller, "n_flux_x");
		set_unit_value_ssc_double(type265_dsg_controller, "n_flux_y");

		// initial values for dsg controller
		set_unit_value_ssc_double(type265_dsg_controller, "P_b_in", as_double("P_b_in_init"));
		set_unit_value_ssc_double(type265_dsg_controller, "f_mdot_rh", as_double("f_mdot_rh_init"));
		set_unit_value_ssc_double(type265_dsg_controller, "P_hp_out");
		set_unit_value_ssc_double(type265_dsg_controller, "T_hp_out");
		set_unit_value_ssc_double(type265_dsg_controller, "T_rh_target");
		set_unit_value_ssc_double(type265_dsg_controller, "T_fw", as_double("T_fw_init"));
		set_unit_value_ssc_double(type265_dsg_controller, "P_cond", as_double("P_cond_init"));


		// Connect DSG Controller Inputs
		bConnected &= connect(weather, "solazi", type265_dsg_controller, "azimuth");
		bConnected &= connect(weather, "solzen", type265_dsg_controller, "zenith");
		bConnected &= connect(weather, "beam", type265_dsg_controller, "DNI");
		bConnected &= connect(weather, "tdry", type265_dsg_controller, "T_amb");
		bConnected &= connect(weather, "wspd", type265_dsg_controller, "v_wind_10");
		bConnected &= connect(weather, "pres", type265_dsg_controller, "P_atm");
		bConnected &= connect(weather, "tdew", type265_dsg_controller, "T_dp");
		bConnected &= connect(type_hel_field, "eta_field", type265_dsg_controller, "field_eff");
		bConnected &= connect(type234_powerblock, "P_boiler_in", type265_dsg_controller, "P_b_in");
		bConnected &= connect(type234_powerblock, "f_rh", type265_dsg_controller, "f_mdot_rh");
		bConnected &= connect(type234_powerblock, "P_rh_in", type265_dsg_controller, "P_hp_out");
		bConnected &= connect(type234_powerblock, "T_rh_in", type265_dsg_controller, "T_hp_out");
		bConnected &= connect(type234_powerblock, "T_rh_out", type265_dsg_controller, "T_rh_target");
		bConnected &= connect(type234_powerblock, "T_cold", type265_dsg_controller, "T_fw");
		bConnected &= connect(type234_powerblock, "P_cond", type265_dsg_controller, "P_cond");
		bConnected &= connect(tou, "tou_value", type265_dsg_controller, "TOUPeriod");
		bConnected &= connect(type_hel_field, "flux_map", type265_dsg_controller, "flux_map");


		// Set Powerblock Parameters
		set_unit_value_ssc_double(type234_powerblock, "P_ref");
		set_unit_value_ssc_double(type234_powerblock, "eta_ref");
		set_unit_value_ssc_double(type234_powerblock, "T_hot_ref");
		set_unit_value_ssc_double(type234_powerblock, "T_cold_ref");
		set_unit_value_ssc_double(type234_powerblock, "dT_cw_ref");
		set_unit_value_ssc_double(type234_powerblock, "T_amb_des");
		set_unit_value_ssc_double(type234_powerblock, "q_sby_frac");
		set_unit_value_ssc_double(type234_powerblock, "P_boil_des");
		
		set_unit_value_ssc_double(type234_powerblock, "is_rh", 1);
		
		
		set_unit_value_ssc_double(type234_powerblock, "P_rh_ref");
				
		set_unit_value_ssc_double(type234_powerblock, "T_rh_hot_ref", as_double("T_rh_out_des"));
		
		
		set_unit_value_ssc_double(type234_powerblock, "rh_frac_ref");
		set_unit_value_ssc_double(type234_powerblock, "CT");
		set_unit_value_ssc_double(type234_powerblock, "startup_time");
		set_unit_value_ssc_double(type234_powerblock, "startup_frac");
		
		set_unit_value_ssc_double(type234_powerblock, "tech_type", 5);
		
		
		set_unit_value_ssc_double(type234_powerblock, "T_approach");
		set_unit_value_ssc_double(type234_powerblock, "T_ITD_des");
		set_unit_value_ssc_double(type234_powerblock, "P_cond_ratio");
		set_unit_value_ssc_double(type234_powerblock, "pb_bd_frac");
		set_unit_value_ssc_double(type234_powerblock, "P_cond_min");
		set_unit_value_ssc_double(type234_powerblock, "n_pl_inc");
		set_unit_value_ssc_array(type234_powerblock, "F_wc");

		// Set Powerblock initial values
		set_unit_value_ssc_double(type234_powerblock, "mode", 2);		//8.6.15 twn: this value should be hardcoded (and maybe removed from Type234)
		set_unit_value_ssc_double(type234_powerblock, "T_hot");

		// Connect Powerblock Inputs (and initial values?)
		bConnected &= connect(type265_dsg_controller, "m_dot_toPB", type234_powerblock, "m_dot_st");
		bConnected &= connect(weather, "twet", type234_powerblock, "T_wb");
		bConnected &= connect(type265_dsg_controller, "m_dot_toPB", type234_powerblock, "m_dot_st");
		bConnected &= connect(type265_dsg_controller, "standby_control", type234_powerblock, "standby_control");
		bConnected &= connect(weather, "tdry", type234_powerblock, "T_db");
		bConnected &= connect(weather, "pres", type234_powerblock, "P_amb");
		bConnected &= connect(weather, "rhum", type234_powerblock, "relhum");
		bConnected &= connect(type265_dsg_controller, "f_timestep", type234_powerblock, "f_recSU");
		bConnected &= connect(type265_dsg_controller, "P_drop_b", type234_powerblock, "dp_b");
		bConnected &= connect(type265_dsg_controller, "dP_sh", type234_powerblock, "dp_sh");
		bConnected &= connect(type265_dsg_controller, "dP_rh", type234_powerblock, "dp_rh");
		bConnected &= connect(tou, "tou_value", type234_powerblock, "TOU");

		// Set Parasitics Parameters
		set_unit_value_ssc_double(type228_parasitics, "Piping_loss");
		set_unit_value_ssc_double(type228_parasitics, "piping_length_add");
		set_unit_value_ssc_double(type228_parasitics, "piping_length_mult");
		set_unit_value_ssc_double(type228_parasitics, "THT", THT);
		set_unit_value_ssc_double(type228_parasitics, "Design_power");
		set_unit_value_ssc_double(type228_parasitics, "design_eff");
		set_unit_value_ssc_double(type228_parasitics, "pb_fixed_par");
		set_unit_value_ssc_double(type228_parasitics, "aux_par");
		set_unit_value_ssc_double(type228_parasitics, "aux_par_f"); 
		set_unit_value_ssc_double(type228_parasitics, "aux_par_0"); 
		set_unit_value_ssc_double(type228_parasitics, "aux_par_1"); 
		set_unit_value_ssc_double(type228_parasitics, "aux_par_2"); 
		set_unit_value_ssc_double(type228_parasitics, "bop_par"); 
		set_unit_value_ssc_double(type228_parasitics, "bop_par_f"); 
		set_unit_value_ssc_double(type228_parasitics, "bop_par_0"); 
		set_unit_value_ssc_double(type228_parasitics, "bop_par_1"); 
		set_unit_value_ssc_double(type228_parasitics, "bop_par_2"); 

		// Set Parasitics Inputs (Initial values?)
		set_unit_value_ssc_double(type228_parasitics, "P_cold_tank", 0.0);
		set_unit_value_ssc_double(type228_parasitics, "P_hot_tank", 0.0);
		set_unit_value_ssc_double(type228_parasitics, "P_htf_pump", 0.0);

		bConnected &= connect(type234_powerblock, "W_cool_par", type228_parasitics, "P_cooling_tower");
		bConnected &= connect(type265_dsg_controller, "W_dot_boost", type228_parasitics, "P_tower_pump");
		bConnected &= connect(type_hel_field, "pparasi", type228_parasitics, "P_helio_track");
		bConnected &= connect(type234_powerblock, "P_cycle", type228_parasitics, "P_plant_output");
		bConnected &= connect(type234_powerblock, "eta", type228_parasitics, "eta_cycle");
		bConnected &= connect(type265_dsg_controller, "q_aux", type228_parasitics, "aux_power");

		// check if all connections worked
		if ( !bConnected )
			throw exec_error( "tcsdirect_steam", util::format("there was a problem connecting outputs of one unit to inputs of another for the simulation.") );


		// Run simulation
		size_t hours = 8760;
		int error = simulate(3600.0, hours * 3600.0, 3600.0, 30);
//		if (0 > simulate(3600, hours*3600, 3600) )
		if (0>error)
			throw exec_error( "tcsdirect_steam", util::format("there was a problem simulating in the TCS direct steam power tower model.") );

		// get the outputs
		if (!set_all_output_arrays() )
			throw exec_error( "tcsdirect_steam", util::format("there was a problem returning the results from the simulation.") );

		set_output_array("gen", "P_out_net", 8760, 1000.0); // MWh to kWh

		//calculated field parameters
		int nr, nc;
		if (double *fm = get_unit_value(type_hel_field, "flux_maps", &nr, &nc))
		{
			ssc_number_t *ssc_fm = allocate("flux_lookup", nr, nc);
			for (int i = 0; i<nr*nc; i++)
				ssc_fm[i] = (ssc_number_t)fm[i];
		}

		if (double *etam = get_unit_value(type_hel_field, "eta_map", &nr, &nc))
		{
			ssc_number_t *ssc_etam = allocate("eff_lookup", nr, nc);
			for (int i = 0; i<nr*nc; i++)
				ssc_etam[i] = (ssc_number_t)etam[i];
		}

		if (double *fpm = get_unit_value(type_hel_field, "flux_positions", &nr, &nc))
		{
			ssc_number_t *ssc_fpm = allocate("sunpos_eval", nr, nc);
			for (int i = 0; i<nr*nc; i++)
				ssc_fpm[i] = (ssc_number_t)fpm[i];
		}
		assign("land_area", var_data((ssc_number_t)get_unit_value_number(type_hel_field, "land_area")));
		//-----------

		accumulate_annual("gen", "annual_energy"); // already in kWh

		accumulate_annual("P_cycle", "annual_W_cycle_gross", 1000.0);	// convert to kWh

		// Calculated outputs
		ssc_number_t ae = as_number("annual_energy");
		ssc_number_t pg = as_number("annual_W_cycle_gross");
		ssc_number_t convfactor = (pg != 0) ? 100 * ae / pg : 0;
		assign("conversion_factor", convfactor);

		// performance adjustement factors
		adjustment_factors haf(this, "adjust");
		if (!haf.setup())
			throw exec_error("tcsmolten_salt", "failed to setup adjustment factors: " + haf.error());
		// hourly_energy output
		ssc_number_t *p_hourly_energy = allocate("gen", 8760);
		// set hourly energy = tcs output Enet
		size_t count;
		ssc_number_t *hourly_energy = as_array("P_out_net", &count);//MWh
		if (count != 8760)
		  {
		    std::stringstream msg;
		    msg << "gen count incorrect (should be 8760): " << count;
		    throw exec_error("tcsmolten_salt", msg.str());
		  }
		// apply performance adjustments and convert from MWh to kWh
		for (size_t i = 0; i < count; i++)
		{
			p_hourly_energy[i] = hourly_energy[i] * (ssc_number_t)(haf(i) * 1000.0);
		}

		accumulate_annual("gen", "annual_energy"); // already in kWh

		// First, sum power cycle water consumption timeseries outputs
		accumulate_annual_for_year("m_dot_makeup", "annual_total_water_use", 1.0/1000.0, 1);	//[m^3], convert from kg
		// Then, add water usage from mirror cleaning
		ssc_number_t V_water_cycle = as_number("annual_total_water_use");
		double V_water_mirrors = as_double("water_usage_per_wash") / 1000.0*A_sf*as_double("washing_frequency");
		assign("annual_total_water_use", var_data((ssc_number_t)(V_water_cycle + V_water_mirrors)));

		// metric outputs moved to technology
		double kWhperkW = 0.0;
		double nameplate = as_double("system_capacity");
		double annual_energy = 0.0;
		for (int i = 0; i < 8760; i++)
			annual_energy += p_hourly_energy[i];
		if (nameplate > 0) kWhperkW = annual_energy / nameplate;
		assign("capacity_factor", var_data((ssc_number_t)(kWhperkW / 87.6)));
		assign("kwh_per_kw", var_data((ssc_number_t)kWhperkW));

		double fuel_usage_mmbtu = 0;
		ssc_number_t *hourly_fuel = as_array("q_aux_fuel", &count);//MWh
		if (count != 8760)
		{
			std::stringstream msg;
			msg << "q_aux_fuel count incorrect (should be 8760): " << count;
			throw exec_error("tcsmolten_salt", msg.str());
		}
		for (size_t i = 0; i < count; i++)
			fuel_usage_mmbtu += hourly_fuel[i];
		assign("system_heat_rate", var_data((ssc_number_t)3.413)); // samsim tcstrough_physical
		// www.unitjuggler.com/convert-energy-from-MMBtu-to-kWh.html
		assign("annual_fuel_usage", var_data((ssc_number_t)(fuel_usage_mmbtu * 293.297)));


	}

};

DEFINE_TCS_MODULE_ENTRY( tcsdirect_steam, "CSP model using the direct steam power tower TCS types.", 4 )
