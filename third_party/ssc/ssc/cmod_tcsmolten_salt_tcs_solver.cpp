// Power Tower - molten salt, cavity and external receiver models
#include "core.h"
#include "tckernel.h"
// for adjustment factors
#include "common.h"
// solarpilot header files
#include "AutoPilot_API.h"
#include "SolarField.h"
#include "IOUtil.h"
#include "csp_common.h"

static var_info _cm_vtab_tcsmolten_salt[] = {

//    VARTYPE           DATATYPE          NAME                   LABEL                                                                UNITS           META            GROUP            REQUIRED_IF                CONSTRAINTS              UI_HINTS
    { SSC_INPUT,        SSC_STRING,      "solar_resource_file",  "local weather file path",                                           "",             "",            "Weather",        "*",                       "LOCAL_FILE",            "" },
	{ SSC_INPUT,        SSC_NUMBER,      "system_capacity",      "Nameplate capacity",                                                "kW",           "",            "molten salt tower", "*",                    "",   "" },

    // TOU													     																	  
    { SSC_INPUT,        SSC_MATRIX,      "weekday_schedule",     "12x24 Time of Use Values for week days",                            "",             "",            "tou_translator", "*",                       "",                      "" }, 
    { SSC_INPUT,        SSC_MATRIX,      "weekend_schedule",     "12x24 Time of Use Values for week end days",                        "",             "",            "tou_translator", "*",                       "",                      "" }, 
															     																	  
	// Heliostat field  parameters				     																	  
    //{ SSC_INPUT,        SSC_MATRIX,      "eta_map",              "Field efficiency matrix",                                           "-",            "",            "heliostat",      "*",                       "",                      "" },
    //{ SSC_INPUT,        SSC_NUMBER,      "n_zen",                "Number of zenith angle data points in file",                        "-",            "",            "heliostat",      "*",                       "",                      "" },
    //{ SSC_INPUT,        SSC_NUMBER,      "n_azi",                "Number of azimuth angle data points in file",                       "-",            "",            "heliostat",      "*",                       "",                      "" },
    //{ SSC_INPUT,        SSC_NUMBER,      "n_hel",                "Number of heliostats in the field",                                 "-",            "",            "heliostat",      "*",                       "",                      "" },
    //{ SSC_INPUT,        SSC_NUMBER,      "q_start",              "Electric work for starting up one heliostat",                       "kWe-hr",       "",            "heliostat",      "*",                       "",                      "" },
    //{ SSC_INPUT,        SSC_NUMBER,      "p_run",                "Electric power for tracking one heliostat",                         "kWe",          "",            "heliostat",      "*",                       "",                      "" },
    //{ SSC_INPUT,        SSC_NUMBER,      "v_wind_max",           "Maximum tolerable wind speed",                                      "m/s",          "",            "heliostat",      "*",                       "",                      "" },
    //{ SSC_INPUT,        SSC_NUMBER,      "hel_stow_deploy",      "Heliostat field stow/deploy solar elevation angle",                 "deg",          "",            "heliostat",      "*",                       "",                      "" },

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
    { SSC_INPUT,        SSC_NUMBER,      "calc_fluxmaps",             "Include fluxmap calculations",               "",       "",         "heliostat",   "?=1",              "",                "" },
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
    { SSC_INPUT,        SSC_NUMBER,      "opt_algorithm",        "Optimization algorithm",                                            "",             "",            "heliostat",       "?=0",                    "",                "" },
    { SSC_INPUT,        SSC_NUMBER,      "opt_flux_penalty",     "Flux over-design penalty",                                          "",             "",            "heliostat",       "?=0.35",                 "",                "" },

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


	//// Heliostat field inputs					     																	  
 //   { SSC_INPUT,        SSC_NUMBER,      "field_control",        "Field defocus control",                                             "",             "",            "heliostat",      "*",                       "",                      "" },
 //   { SSC_INPUT,        SSC_NUMBER,      "vwind",                "Wind velocity",                                                     "m/s",          "",            "heliostat",      "*",                       "",                     "" },
 //   { SSC_INPUT,        SSC_NUMBER,      "solaz",                "Solar azimuth angle: 0 due north, clocwise to +360",                "deg",          "",            "heliostat",      "*",                       "",                     "" },
 //   { SSC_INPUT,        SSC_NUMBER,      "solzen",               "Solar zenith angle",                                                "deg",          "",            "heliostat",      "*",                       "",                     "" },
    

	// Which type of receiver model to use in the simulation     																	  
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
    //{ SSC_INPUT,        SSC_MATRIX,      "fluxmap_angles",       "Matrix containing zenith and azimuth angles for flux maps",         "-",            "",            "receiver",       "*",                       "",                      "" },
    //{ SSC_INPUT,        SSC_MATRIX,      "fluxmap",              "Matrix containing flux map for various solar positions",            "-",            "",            "receiver",       "*",                       "",                      "" },
															     																	  
    // Receiver (type 222) inputs							     																	  
  //{ SSC_INPUT,        SSC_NUMBER,      "azimuth_ini",          "Solar azimuth angle",                                               "deg",          "",            "receiver",       "*",                       "",                      "" },
  //{ SSC_INPUT,        SSC_NUMBER,      "zenith_ini",           "Solar zenith angle",                                                "deg",          "",            "receiver",       "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "T_salt_hot_target",    "Desired HTF outlet temperature",                                    "C",            "",            "receiver",       "*",                       "",                      "" },
  //{ SSC_INPUT,        SSC_NUMBER,      "T_salt_cold",          "Desired HTF inlet temperature",                                     "C",            "",            "receiver",       "*",                       "",                      "" },
  //{ SSC_INPUT,        SSC_NUMBER,      "V_wind_10",            "Ambient wind velocity, ground level",                               "m/s",          "",            "receiver",       "*",                       "",                      "" },
  //{ SSC_INPUT,        SSC_NUMBER,      "P_amb",                "Ambient atmospheric pressure",                                      "mbar",         "",            "receiver",       "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "eta_pump",             "Receiver HTF pump efficiency",                                      "",             "",            "receiver",       "*",                       "",                      "" },
  //{ SSC_INPUT,        SSC_NUMBER,      "T_dp",                 "Ambient dew point temperature",                                     "C",            "",            "receiver",       "*",                       "",                      "" },
  //{ SSC_INPUT,        SSC_NUMBER,      "I_bn",                 "Direct (beam) normal radiation",                                    "W/m^2-K",      "",            "receiver",       "*",                       "",                      "" },
  //{ SSC_INPUT,        SSC_NUMBER,      "field_eff",            "Heliostat field efficiency",                                        "",             "",            "receiver",       "*",                       "",                      "" },
  //{ SSC_INPUT,        SSC_NUMBER,      "T_db",                 "Ambient dry bulb temperature",                                      "C",            "",            "receiver",       "*",                       "",                      "" },
  //{ SSC_INPUT,        SSC_NUMBER,      "night_recirc",         "Flag to indicate night recirculation through the rec.",             "",             "",            "receiver",       "*",                       "INTEGER",               "" },
  //{ SSC_INPUT,        SSC_NUMBER,      "hel_stow_deploy",      "Heliostat field stow/deploy solar angle",                           "deg",          "",            "receiver",       "*",                       "",                      "" },
															     																	  
															     																	  
    // Cavity Receiver (type 232) specific parameters		     																	  
    { SSC_INPUT,        SSC_NUMBER,      "rec_d_spec",           "Receiver aperture width",                                           "m",            "",            "cavity_receiver","*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "h_rec_panel",          "Height of a receiver panel",                                        "m",            "",            "cavity_receiver","*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "h_lip",                "Height of upper lip of cavity",                                     "m",            "",            "cavity_receiver","*",                       "",                      "" },
  //{ SSC_INPUT,        SSC_NUMBER,      "h_tower",              "Total height of the solar tower",                                   "m",            "",            "cavity_receiver","*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "rec_angle",            "Section of the cavity circle covered in panels",                    "deg",          "",            "cavity_receiver","*",                       "",                      "" },
  //{ SSC_INPUT,        SSC_NUMBER,      "d_tube_out",           "Outer diameter of a single tube",                                   "mm",           "",            "cavity_receiver","*",                       "",                      "" },
  //{ SSC_INPUT,        SSC_NUMBER,      "th_tube",              "Wall thickness of a single tube",                                   "mm",           "",            "cavity_receiver","*",                       "",                      "" },
  //{ SSC_INPUT,        SSC_NUMBER,      "eta_pump",             "Efficiency of HTF pump",                                            "-",            "",            "cavity_receiver","*",                       "",                      "" },
  //{ SSC_INPUT,        SSC_NUMBER,      "hel_stow",             "Heliostat field stow/deploy solar angle",                           "deg",          "",            "cavity_receiver","*",                       "",                      "" },
  //{ SSC_INPUT,        SSC_NUMBER,      "flow_pattern",         "HTF flow scheme through receiver panels",                           "-",            "",            "cavity_receiver","*",                       "",                      "" },
  //{ SSC_INPUT,        SSC_NUMBER,      "htf",                  "Flag indicating heat transfer fluid",                               "-",            "",            "cavity_receiver","*",                       "",                      "" },
  //{ SSC_INPUT,        SSC_MATRIX,      "field_fl_props",       "User defined field fluid property data",                            "-",            "",            "cavity_receiver","*",                       "",                      "" },
  //{ SSC_INPUT,        SSC_NUMBER,      "material",             "Receiver tube material",                                            "-",            "",            "cavity_receiver","*",                       "",                      "" },
  //{ SSC_INPUT,        SSC_NUMBER,      "hl_ffact",             "Heat loss factor (thermal loss fudge factor)",                      "-",            "",            "cavity_receiver","*",                       "",                      "" },
  //{ SSC_INPUT,        SSC_NUMBER,      "T_htf_hot_des",        "Hot HTF outlet temperature at design",                              "C",            "",            "cavity_receiver","*",                       "",                      "" },
  //{ SSC_INPUT,        SSC_NUMBER,      "T_htf_cold_des",       "Cold HTF outlet temperature at design",                             "C",            "",            "cavity_receiver","*",                       "",                      "" },
  //{ SSC_INPUT,        SSC_NUMBER,      "f_rec_min",            "Minimum receiver mass flow rate turndown fraction",                 "-",            "",            "cavity_receiver","*",                       "",                      "" },
  //{ SSC_INPUT,        SSC_NUMBER,      "q_rec_des",            "Design-point receiver thermal power output",                        "MWt",          "",            "cavity_receiver","*",                       "",                      "" },
  //{ SSC_INPUT,        SSC_NUMBER,      "rec_su_delay",         "Fixed startup delay time for the receiver",                         "hr",           "",            "cavity_receiver","*",                       "",                      "" },
  //{ SSC_INPUT,        SSC_NUMBER,      "rec_qf_delay",         "Energy-based receiver startup delay (frac of rated power)",         "-",            "",            "cavity_receiver","*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "conv_model",           "Type of convection model (1=Clausing, 2=Siebers/Kraabel)",          "-",            "",            "cavity_receiver","*",                       "",                      "" },
  //{ SSC_INPUT,        SSC_NUMBER,      "m_dot_htf_max",        "Maximum receiver mass flow rate",                                   "kg/hr",        "",            "cavity_receiver","*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "eps_wavelength",       "Matrix containing wavelengths, active & passive surface eps",       "-",            "",            "cavity_receiver","*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "conv_coupled",         "1=coupled, 2=uncoupled",                                            "-",            "",            "cavity_receiver","*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "conv_forced",          "1=forced (use wind), 0=natural",                                    "-",            "",            "cavity_receiver","*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "h_wind_meas",          "Height at which wind measurements are given",                       "m",            "",            "cavity_receiver","*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "conv_wind_dir",        "Wind direction dependent forced convection 1=on 0=off",             "-",            "",            "cavity_receiver","*",                       "",                      "" },
  //{ SSC_INPUT,        SSC_MATRIX,      "fluxmap_angles",       "Matrix containing zenith and azimuth angles for flux maps",         "-",            "",            "cavity_receiver","*",                       "",                      "" },
  //{ SSC_INPUT,        SSC_MATRIX,      "fluxmap",              "Matrix containing 10x12 flux map for various solar positions",      "-",            "",            "cavity_receiver","*",                       "",                      "" },
															     																	  
  //{ SSC_INPUT,        SSC_NUMBER,      "azimuth",              "0 at due north, ranges clockwise from 0 to 360",                    "deg",          "",            "cavity_receiver","*",                       "",                      "" },
  //{ SSC_INPUT,        SSC_NUMBER,      "zenith",               "solar zenith angle",                                                "deg",          "",            "cavity_receiver","*",                       "",                      "" },
  //{ SSC_INPUT,        SSC_NUMBER,      "T_htf_hot",            "Target hot outlet temperature of the working fluid",                "C",            "",            "cavity_receiver","*",                       "",                      "" },
  //{ SSC_INPUT,        SSC_NUMBER,      "T_htf_cold",           "Inlet temperature of the HTF",                                      "C",            "",            "cavity_receiver","*",                       "",                      "" },
  //{ SSC_INPUT,        SSC_NUMBER,      "P_amb",                "Ambient pressure",                                                  "atm",          "",            "cavity_receiver","*",                       "",                      "" },
  //{ SSC_INPUT,        SSC_NUMBER,      "T_dp",                 "Dew point temperature",                                             "C",            "",            "cavity_receiver","*",                       "",                      "" },
  //{ SSC_INPUT,        SSC_NUMBER,      "I_bn",                 "Direct normal irradiation",                                         "W/m2",         "",            "cavity_receiver","*",                       "",                      "" },
  //{ SSC_INPUT,        SSC_NUMBER,      "eta_field",            "Overall efficiency of heliostat field",                             "-",            "",            "cavity_receiver","*",                       "",                      "" },
  //{ SSC_INPUT,        SSC_NUMBER,      "T_amb",                "Ambient temperature",                                               "C",            "",            "cavity_receiver","*",                       "",                      "" },
  //{ SSC_INPUT,        SSC_NUMBER,      "u_wind",               "Wind velocity",                                                     "m/s",          "",            "cavity_receiver","*",                       "",                      "" },
  //{ SSC_INPUT,        SSC_NUMBER,      "deg_wind",             "Wind direction",                                                    "deg",          "",            "cavity_receiver","*",                       "",                      "" },
  //{ SSC_INPUT,        SSC_NUMBER,      "P_htf",                "Average coolant pressure",                                          "bar",          "",            "cavity_receiver","*",                       "",                      "" },
															     																	  
															     																	  
															     																	  
    // Controller (type 251) parameters						     																	  
    { SSC_INPUT,        SSC_NUMBER,      "field_fluid",          "Material number for the collector field",                           "-",            "",            "controller",     "*",                       "",                      "" },
  //{ SSC_INPUT,        SSC_MATRIX,      "field_fl_props",       "User defined field fluid property data",                            "-",            "",            "controller",     "*",                       "",                      "" },
  //{ SSC_INPUT,        SSC_NUMBER,      "store_fluid",          "Material number for storage fluid",                                 "-",            "",            "controller",     "*",                       "",                      "" },
  //{ SSC_INPUT,        SSC_MATRIX,      "user_fluid",           "User defined fluid property data",                                  "-",            "",            "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "tshours",              "Equivalent full-load thermal storage hours",                        "hr",           "",            "controller",     "*",                       "",                      "" },
  //{ SSC_INPUT,        SSC_NUMBER,      "is_hx",                "1=yes, 0=no"                                                        "-",            "",            "controller",     "*",                       "",                      "" },
  //{ SSC_INPUT,        SSC_NUMBER,      "dt_hot",               "Hot side HX approach temp",                                         "C",            "",            "controller",     "*",                       "",                      "" },
  //{ SSC_INPUT,        SSC_NUMBER,      "dt_cold",              "Cold side HX approach temp",                                        "C",            "",            "controller",     "*",                       "",                      "" },
  //{ SSC_INPUT,        SSC_NUMBER,      "hx_config",            "HX configuration",                                                  "-",            "",            "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "q_max_aux",            "Max heat rate of auxiliary heater",                                 "MWt",          "",            "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "T_set_aux",            "Aux heater outlet temp set point",                                  "C",            "",            "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "V_tank_hot_ini",       "Initial hot tank fluid volume",                                     "m3",           "",            "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "T_tank_hot_ini",       "Initial hot tank fluid temperature",                                "C",            "",            "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "T_tank_cold_ini",      "Initial cold tank fluid tmeperature",                               "C",            "",            "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "vol_tank",             "Total tank volume, including unusable HTF at bottom",               "m3",           "",            "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "h_tank",               "Total height of tank (height of HTF when tank is full",             "m",            "",            "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "h_tank_min",           "Minimum allowable HTF height in storage tank",                      "m",            "",            "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "u_tank",               "Loss coefficient from the tank",                                    "W/m2-K",       "",            "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "tank_pairs",           "Number of equivalent tank pairs",                                   "-",            "",            "controller",     "*",                       "INTEGER",               "" },
    { SSC_INPUT,        SSC_NUMBER,      "cold_tank_Thtr",       "Minimum allowable cold tank HTF temp",                              "C",            "",            "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "hot_tank_Thtr",        "Minimum allowable hot tank HTF temp",                               "C",            "",            "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "hot_tank_max_heat",    "Rated heater capacity for hot tank heating",                        "MW",           "",            "controller",     "*",                       "",                      "" },
	{ SSC_INPUT,        SSC_NUMBER,      "cold_tank_max_heat",   "Rated heater capacity for cold tank heating",                       "MW",           "",            "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "T_field_in_des",       "Field design inlet temperature",                                    "C",            "",            "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "T_field_out_des",      "Field design outlet temperature",                                   "C",            "",            "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "q_pb_design",          "Design heat input to power block",                                  "MWt",          "",            "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "W_pb_design",          "Rated plant capacity",                                              "MWe",          "",            "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "cycle_max_frac",       "Maximum turbine over design operation fraction",                    "-",            "",            "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "cycle_cutoff_frac",    "Minimum turbine operation fraction before shutdown",                "-",            "",            "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "solarm",               "Solar Multiple",                                                    "-",            "",            "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "pb_pump_coef",         "Pumping power to move 1kg of HTF through PB loop",                  "kW/kg",        "",            "controller",     "*",                       "",                      "" },
    //{ SSC_INPUT,        SSC_NUMBER,      "tes_pump_coef",        "Pumping power to move 1kg of HTF through tes loop",                 "kW/kg",        "",            "controller",     "*",                       "",                      "" },
    //{ SSC_INPUT,        SSC_NUMBER,      "pb_fixed_par_cntl",    "Fraction of rated gross power constantly consumed by controller",   "-",            "",            "controller",     "*",                       "",                      "" },
    //{ SSC_INPUT,        SSC_ARRAY,       "bop_array",            "Coefficients for balance of plant parasitics calcs",                "-",            "",            "controller",     "*",                       "",                      "" },
    //{ SSC_INPUT,        SSC_ARRAY,       "aux_array",            "Coefficients for auxiliary heater parasitics calcs",                "-",            "",            "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "T_startup",            "Startup temperature",                                               "C",            "",            "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "fossil_mode",          "Fossil backup mode 1=Normal 2=Topping",                             "-",            "",            "controller",     "*",                       "",                      "" },
    //{ SSC_INPUT,        SSC_NUMBER,      "fthr_ok",              "Does the defocus control allow partial defocusing",                 "-",            "",            "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "nSCA",                 "Number of SCAs in a single loop",                                   "-",            "",            "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "I_bn_des",             "Design point irradiation value",                                    "W/m2",         "",            "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "fc_on",                "DNI forecasting enabled",                                           "-",            "",            "controller",     "?=0",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "q_sby_frac",           "Fraction of thermal power required for standby",                    "-",            "",            "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "t_standby_reset",      "Maximum allowable time for PB standby operation",                   "hr",           "",            "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "sf_type",              "Solar field type, 1 = trough, 2 = tower",                           "-",            "",            "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "tes_type",             "1=2-tank, 2=thermocline",                                           "-",            "",            "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_ARRAY,       "tslogic_a",            "Dispatch logic without solar",                                      "-",            "",            "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_ARRAY,       "tslogic_b",            "Dispatch logic with solar",                                         "-",            "",            "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_ARRAY,       "tslogic_c",            "Dispatch logic for turbine load fraction",                          "-",            "",            "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_ARRAY,       "ffrac",                "Fossil dispatch logic",                                             "-",            "",            "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "tc_fill",              "Thermocline fill material",                                         "-",            "",            "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "tc_void",              "Thermocline void fraction",                                         "-",            "",            "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "t_dis_out_min",        "Min allowable hot side outlet temp during discharge",               "C",            "",            "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "t_ch_out_max",         "Max allowable cold side outlet temp during charge",                 "C",            "",            "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "nodes",                "Nodes modeled in the flow path",                                    "-",            "",            "controller",     "*",                       "INTEGER",               "" },
    { SSC_INPUT,        SSC_NUMBER,      "f_tc_cold",            "0=entire tank is hot, 1=entire tank is cold",                       "-",            "",            "controller",     "*",                       "",                      "" },
  //{ SSC_INPUT,        SSC_ARRAY,       "TOU_schedule",         "Annual hourly time-of-use schedule",                                "-",            "",            "controller",     "*",                       "",                      "" },
															     																	  
    // Controller (type 251) inputs							     																	  
    //{ SSC_INPUT,        SSC_NUMBER,      "m_dot_htf_ref",        "Reference HTF flow rate at design conditions",                      "kg/hr",        "",            "controller",     "*",                       "",                      "" },
    //{ SSC_INPUT,        SSC_NUMBER,      "T_pb_out",             "Fluid temperature from the power block",                            "C",            "",            "controller",     "*",                       "",                      "" },
	{ SSC_INPUT,        SSC_NUMBER,      "eta_lhv",              "Fossil fuel lower heating value - Thermal power generated per unit fuel",   "MW/MMBTU",     "",    "controller",     "*",                       "",                      "" },													     																	  
															     																	  
    // Powerblock (type 224) parameters						     																	  
    { SSC_INPUT,        SSC_NUMBER,      "P_ref",                "Reference output electric power at design condition",               "MW",           "",            "powerblock",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "eta_ref",              "Reference conversion efficiency at design condition",               "none",         "",            "powerblock",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "T_htf_hot_ref",        "Reference HTF inlet temperature at design",                         "C",            "",            "powerblock",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "T_htf_cold_ref",       "Reference HTF outlet temperature at design",                        "C",            "",            "powerblock",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "dT_cw_ref",            "Reference condenser cooling water inlet/outlet T diff",             "C",            "",            "powerblock",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "T_amb_des",            "Reference ambient temperature at design point",                     "C",            "",            "powerblock",     "*",                       "",                      "" },
  //{ SSC_INPUT,        SSC_NUMBER,      "HTF",                  "Integer flag identifying HTF in power block",                       "none",         "",            "powerblock",     "*",                       "",                      "" },
  //{ SSC_INPUT,        SSC_NUMBER,      "q_sby_frac",           "Fraction of thermal power required for standby mode",               "none",         "",            "powerblock",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "P_boil",               "Boiler operating pressure",                                         "bar",          "",            "powerblock",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "CT",                   "Flag for using dry cooling or wet cooling system",                  "none",         "",            "powerblock",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "startup_time",         "Time needed for power block startup",                               "hr",           "",            "powerblock",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "startup_frac",         "Fraction of design thermal power needed for startup",               "none",         "",            "powerblock",     "*",                       "",                      "" },
  //{ SSC_INPUT,        SSC_NUMBER,      "tech_type",            "Flag indicating which coef. set to use. (1=tower,2=trough,3=user)", "none",         "",            "powerblock",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "T_approach",           "Cooling tower approach temperature",                                "C",            "",            "powerblock",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "T_ITD_des",            "ITD at design for dry system",                                      "C",            "",            "powerblock",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "P_cond_ratio",         "Condenser pressure ratio",                                          "none",         "",            "powerblock",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "pb_bd_frac",           "Power block blowdown steam fraction ",                              "none",         "",            "powerblock",     "*",                       "",                      "" },
  //{ SSC_INPUT,        SSC_STRING,      "pb_input_file",        "Power block coefficient file name",                                 "none",         "",            "powerblock",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "P_cond_min",           "Minimum condenser pressure",                                        "inHg",         "",            "powerblock",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "n_pl_inc",             "Number of part-load increments for the heat rejection system",      "none",         "",            "powerblock",     "*",                       "INTEGER",               "" },
    { SSC_INPUT,        SSC_ARRAY,       "F_wc",                 "Fraction indicating wet cooling use for hybrid system",             "none",         "",            "powerblock",     "*",                       "",                      "" },
															     																	  
    // Powerblock (type 224) inputs							     																	  
  //{ SSC_INPUT,        SSC_NUMBER,      "mode",                 "Cycle part load control, from plant controller",                    "none",         "",            "powerblock",     "*",                       "",                      "" },
  //{ SSC_INPUT,        SSC_NUMBER,      "T_htf_hot",            "Hot HTF inlet temperature, from storage tank",                      "C",            "",            "powerblock",     "*",                       "",                      "" },
  //{ SSC_INPUT,        SSC_NUMBER,      "m_dot_htf",            "HTF mass flow rate",                                                "kg/hr",        "",            "powerblock",     "*",                       "",                      "" },
  //{ SSC_INPUT,        SSC_NUMBER,      "T_wb",                 "Ambient wet bulb temperature",                                      "C",            "",            "powerblock",     "*",                       "",                      "" },
  //{ SSC_INPUT,        SSC_NUMBER,      "demand_var",           "Control signal indicating operational mode",                        "none",         "",            "powerblock",     "*",                       "",                      "" },
  //{ SSC_INPUT,        SSC_NUMBER,      "standby_control",      "Control signal indicating standby mode",                            "none",         "",            "powerblock",     "*",                       "",                      "" },
  //{ SSC_INPUT,        SSC_NUMBER,      "T_db",                 "Ambient dry bulb temperature",                                      "C",            "",            "powerblock",     "*",                       "",                      "" },
  //{ SSC_INPUT,        SSC_NUMBER,      "P_amb",                "Ambient pressure",                                                  "atm",          "",            "powerblock",     "*",                       "",                      "" },
  //{ SSC_INPUT,        SSC_NUMBER,      "TOU",                  "Current Time-of-use period",                                        "none",         "",            "powerblock",     "*",                       "",                      "" },
  //{ SSC_INPUT,        SSC_NUMBER,      "rh",                   "Relative humidity of the ambient air",                              "none",         "",            "powerblock",     "*",                       "",                      "" },
	
	// sCO2 Powerblock (type 424) inputs
	{ SSC_INPUT,        SSC_NUMBER,      "pc_config",            "0: Steam Rankine (224), 1: sCO2 Recompression (424)",               "none",         "",            "powerblock",     "?=0",                       "INTEGER",               "" },
	{ SSC_INPUT,        SSC_NUMBER,      "eta_c",                "Isentropic efficiency of compressor(s)",                            "none",         "",            "powerblock",     "*",                       "",                      "" },
	{ SSC_INPUT,        SSC_NUMBER,      "eta_t",                "Isentropic efficiency of turbine",							      "none",         "",            "powerblock",     "*",                       "",                      "" },
	{ SSC_INPUT,        SSC_NUMBER,      "P_high_limit",         "Upper pressure limit in cycle",								      "MPa",          "",            "powerblock",     "*",                       "",                      "" },
	{ SSC_INPUT,        SSC_NUMBER,      "deltaT_PHX",           "Design temperature difference in PHX",						      "C",	          "",            "powerblock",     "*",                       "",                      "" },
	{ SSC_INPUT,        SSC_NUMBER,      "fan_power_perc_net",   "% of net cycle output used for fan power at design",			      "%",	          "",            "powerblock",     "*",                       "",                      "" },
	{ SSC_INPUT,        SSC_NUMBER,      "elev",                 "Site elevation",                                                    "m",            "",            "powerblock",     "*",                       "",                      "" },

	// Parasitics (type 228) parameters						     																	  
   // {SSC_INPUT,         SSC_NUMBER,      "P_storage_pump",       "Storage pump power, rated per MWt of storage use",                  "MWe/MWt",      "",            "parasitics",     "*",                       "",                      "" },
    {SSC_INPUT,         SSC_NUMBER,      "piping_loss",          "Thermal loss per meter of piping",                                  "Wt/m",         "",            "parasitics",     "*",                       "",                      "" },
    {SSC_INPUT,         SSC_NUMBER,      "piping_length",        "Total length of exposed piping",                                    "m",            "",            "parasitics",     "*",                       "",                      "" },
    {SSC_INPUT,         SSC_NUMBER,      "piping_length_mult",   "Piping length multiplier",                                          "",             "",            "parasitics",     "*",                       "",                      "" },
    {SSC_INPUT,         SSC_NUMBER,      "piping_length_const",  "Piping constant length",                                            "m",            "",            "parasitics",     "*",                       "",                      "" },
    //{SSC_INPUT,         SSC_NUMBER,      "Design_power",         "Power production at design conditions",                             "MWe",          "",            "parasitics",     "*",                       "",                      "" },
    //{SSC_INPUT,         SSC_NUMBER,      "recirc_htr_eff",       "Recirculation heater efficiency",                                   "none",         "",            "parasitics",     "*",                       "",                      "" },
    {SSC_INPUT,         SSC_NUMBER,      "design_eff",           "Power cycle efficiency at design",                                  "none",         "",            "parasitics",     "*",                       "",                      "" },
  //{SSC_INPUT,         SSC_NUMBER,      "night_recirc",         "Flag indicating whether night recirculation is allowed",            "none",         "",            "parasitics",     "*",                       "",                      "" },
    {SSC_INPUT,         SSC_NUMBER,      "pb_fixed_par",         "Fixed parasitic load - runs at all times",                          "MWe/MWcap",    "",            "parasitics",     "*",                       "",                      "" },
    {SSC_INPUT,         SSC_NUMBER,      "aux_par",              "Aux heater, boiler parasitic",                                      "MWe/MWcap",    "",            "parasitics",     "*",                       "",                      "" },
    {SSC_INPUT,         SSC_NUMBER,      "aux_par_f",            "Aux heater, boiler parasitic - multiplying fraction",               "none",         "",            "parasitics",     "*",                       "",                      "" },
    {SSC_INPUT,         SSC_NUMBER,      "aux_par_0",            "Aux heater, boiler parasitic - constant coefficient",               "none",         "",            "parasitics",     "*",                       "",                      "" },
    {SSC_INPUT,         SSC_NUMBER,      "aux_par_1",            "Aux heater, boiler parasitic - linear coefficient",                 "none",         "",            "parasitics",     "*",                       "",                      "" },
    {SSC_INPUT,         SSC_NUMBER,      "aux_par_2",            "Aux heater, boiler parasitic - quadratic coefficient",              "none",         "",            "parasitics",     "*",                       "",                      "" },
    {SSC_INPUT,         SSC_NUMBER,      "bop_par",              "Balance of plant parasitic power fraction",                         "MWe/MWcap",    "",            "parasitics",     "*",                       "",                      "" },
    {SSC_INPUT,         SSC_NUMBER,      "bop_par_f",            "Balance of plant parasitic power fraction - mult frac",             "none",         "",            "parasitics",     "*",                       "",                      "" },
    {SSC_INPUT,         SSC_NUMBER,      "bop_par_0",            "Balance of plant parasitic power fraction - const coeff",           "none",         "",            "parasitics",     "*",                       "",                      "" },
    {SSC_INPUT,         SSC_NUMBER,      "bop_par_1",            "Balance of plant parasitic power fraction - linear coeff",          "none",         "",            "parasitics",     "*",                       "",                      "" },
    {SSC_INPUT,         SSC_NUMBER,      "bop_par_2",            "Balance of plant parasitic power fraction - quadratic coeff",       "none",         "",            "parasitics",     "*",                       "",                      "" },
   // {SSC_INPUT,         SSC_NUMBER,      "storage_bypass",       "Flag indicating whether the hot salt pump always runs w/ PB",       "none",         "",            "parasitics",     "*",                       "",                      "" },
    // Parasitics (type 228) inputs							     																	  
    //{SSC_INPUT,         SSC_NUMBER,      "flow_from_storage",    "Flow rate from storage",                                            "kg/hr",       "",             "parasitics",     "*",                       "",                      "" },
  //{SSC_INPUT,         SSC_NUMBER,      "P_cooling_tower",      "Cooling tower parasitic power fraction",                            "MWe",         "",             "parasitics",     "*",                       "",                      "" },
  //{SSC_INPUT,         SSC_NUMBER,      "P_tower_pump",         "Reported tower pump power",                                         "MWe",         "",             "parasitics",     "*",                       "",                      "" },
  //{SSC_INPUT,         SSC_NUMBER,      "P_helio_track",        "Reported heliostat tracking power",                                 "MWe",         "",             "parasitics",     "*",                       "",                      "" },
  //{SSC_INPUT,         SSC_NUMBER,      "P_plant_output",       "Reported plant power output",                                       "MWe",         "",             "parasitics",     "*",                       "",                      "" },
  //{SSC_INPUT,         SSC_NUMBER,      "eta_cycle",            "Power cycle efficiency",                                            "none",        "",             "parasitics",     "*",                       "",                      "" },
  //{SSC_INPUT,         SSC_NUMBER,      "P_cold_tank",          "Cold tank heater parasitic power",                                  "MWe",         "",             "parasitics",     "*",                       "",                      "" },
   //{SSC_INPUT,         SSC_NUMBER,      "P_hot_tank",           "Hot tank heater parasitic power",                                   "MWe",         "",             "parasitics",     "*",                       "",                      "" },
  //{SSC_INPUT,         SSC_NUMBER,      "P_tower_conv",         "Reported tower convection loss",                                    "MWt",         "",             "parasitics",     "*",                       "",                      "" },
  //{SSC_INPUT,         SSC_NUMBER,      "P_tower_rad",          "Reported tower radiation loss",                                     "MWt",         "",             "parasitics",     "*",                       "",                      "" },
   // {SSC_INPUT,         SSC_NUMBER,      "recirc_source",        "Recirculation heater control",                                      "none",        "",             "parasitics",     "*",                       "",                      "" },
  //{SSC_INPUT,         SSC_NUMBER,      "ref_htf_flow",         "HTF flow rate through the power cycle at design",                   "kg/hr",       "",             "parasitics",     "*",                       "",                      "" },
  //{SSC_INPUT,         SSC_NUMBER,      "aux_power",            "Auxiliary heater thermal power output",                             "MWt",         "",             "parasitics",     "*",                       "",                      "" },
  //{SSC_INPUT,         SSC_NUMBER,      "P_htf_pump",           "HTF pumping power",                                                 "MWe",         "",             "parasitics",     "*",                       "",                      "" },



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

    //solar field
    { SSC_OUTPUT,       SSC_ARRAY,       "eta_field",            "Field optical efficiency",                                     "",             "",            "Outputs",        "*",                       "LENGTH=8760",           "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "defocus",              "Field optical focus fraction",                                 "",             "",            "Outputs",        "*",                       "LENGTH=8760",           "" },
    
    // These outputs come from either type 222 (external), or type 232(cavity), depending on which receiver type the user chose.
	// Therefore, these outputs have to have the same name in both types, or TCS will throw an error when trying to read the results.
	{ SSC_OUTPUT,       SSC_ARRAY,       "eta_therm",            "Receiver thermal efficiency",                                    "",            "",            "Outputs",        "*",                       "LENGTH=8760",           "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "Q_solar_total",        "Receiver thermal power absorbed",                                "MWt",           "",            "Outputs",        "*",                       "LENGTH=8760",           "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "q_conv_sum",           "Receiver thermal power loss to convection",                      "MWt",           "",            "Outputs",        "*",                       "LENGTH=8760",           "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "q_rad_sum",            "Receiver thermal power loss to radiation",                       "MWt",           "",            "Outputs",        "*",                       "LENGTH=8760",           "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "Q_thermal",            "Receiver thermal power to HTF",                                  "MWt",           "",            "Outputs",        "*",                       "LENGTH=8760",           "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "q_startup",            "Receiver startup thermal energy consumed",                       "MWt-hr",       "",            "Outputs",        "*",                       "LENGTH=8760",           "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "m_dot_field",          "Receiver HTF mass flow rate",                                    "kg/hr",        "",            "Outputs",        "*",                       "LENGTH=8760",           "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "T_field_in",           "Receiver HTF temperature in",                                    "C",            "",            "Outputs",        "*",                       "LENGTH=8760",           "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "T_field_out",          "Receiver HTF temperature out",                                   "C",            "",            "Outputs",        "*",                       "LENGTH=8760",           "" },
    
    //thermal storage
    { SSC_OUTPUT,       SSC_ARRAY,       "mass_tank_cold",       "TES HTF mass in cold tank",                                      "kg",           "",            "Type251",        "*",                       "LENGTH=8760",           "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "mass_tank_hot",        "TES HTF mass in hot tank",                                       "kg",           "",            "Type251",        "*",                       "LENGTH=8760",           "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "vol_tank_cold_fin",    "TES HTF volume in cold tank",                                    "m3",           "",            "Type251",        "*",                       "LENGTH=8760",           "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "vol_tank_hot_fin",     "TES HTF volume in hot tank",                                     "m3",           "",            "Type251",        "*",                       "LENGTH=8760",           "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "m_dot_charge_field",   "TES HTF mass flow rate (charging)",                              "kg/hr",        "",            "Type250",        "*",                       "LENGTH=8760",           "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "tank_losses",          "TES thermal losses from tank(s)",                                "MWt",          "",            "Type251",        "*",                       "LENGTH=8760",           "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "q_to_tes",             "TES thermal energy into storage",                                "MWt",          "",            "Type251",        "*",                       "LENGTH=8760",           "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "T_hot_node",           "TES [thermocline] temperature - hot node",                       "C",            "",            "Outputs",        "*",                       "LENGTH=8760",           "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "T_cold_node",          "TES [thermocline] temperature - cold node",                      "C",            "",            "Outputs",        "*",                       "LENGTH=8760",           "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "T_max",                "TES [thermocline] temperature - max",                            "C",            "",            "Outputs",        "*",                       "LENGTH=8760",           "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "f_hot",                "TES [thermocline] Hot depth fraction",                           "",            "",            "Outputs",        "*",                       "LENGTH=8760",           "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "f_cold",               "TES [thermocline] Cold depth fraction",                          "",            "",            "Outputs",        "*",                       "LENGTH=8760",           "" },

    //power block
    { SSC_OUTPUT,       SSC_ARRAY,       "eta",               "Cycle efficiency (gross)",                                          "",         "",            "Type224",        "*",                       "LENGTH=8760",           "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "P_out_net",         "Cycle electrical power output (net)",                               "MWe",          "",            "Net_E_Calc",     "*",                       "LENGTH=8760",           "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "P_cycle",           "Cycle electrical power output (gross)",                             "MWe",          "",            "Net_E_Calc",     "*",                       "LENGTH=8760",           "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "m_dot_pb",          "Cycle HTF mass flow rate",                                          "kg/hr",        "",            "Type250",        "*",                       "LENGTH=8760",           "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "T_pb_in",           "Cycle HTF temperature in (hot)",                                    "C",            "",            "Type251",        "*",                       "LENGTH=8760",           "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "T_pb_out",          "Cycle HTF temperature out (cold)",                                  "C",            "",            "Type251",        "*",                       "LENGTH=8760",           "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "m_dot_makeup",      "Cycle cooling water mass flow rate - makeup",                       "kg/hr",        "",            "Type250",        "*",                       "LENGTH=8760",           "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "q_pb",              "Cycle thermal power input",                                         "MWt",          "",            "Type251",        "*",                       "LENGTH=8760",           "" },
    
	{ SSC_OUTPUT,       SSC_ARRAY,       "q_pc_startup",      "Cycle startup energy",                                        "MWt-hr",       "",            "Type251",        "*",                       "LENGTH=8760",           "" },

	// add sco2 power block outputs here so they line up in Time Series display
	// Hourly off-design values. These should be displayed as Time Series data in SAM
	{ SSC_OUTPUT,       SSC_ARRAY,       "T_turbine_in",         "Cycle turbine inlet temperature",                                         "C",            "",            "Outputs",        "",                       "LENGTH=8760",           "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "P_mc_in",              "Cycle main comp inlet pressure",                                          "kPa",          "",            "Outputs",        "",                       "LENGTH=8760",           "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "P_mc_out",             "Cycle main comp outlet pressure",                                         "kPa",		  "",            "Outputs",        "",                       "LENGTH=8760",           "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "f_recomp",             "Cycle recomp fraction",                                                   "",			  "",            "Outputs",        "",                       "LENGTH=8760",           "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "N_MC",                 "Cycle main comp. shaft speed",                                            "rpm",          "",            "Outputs",        "",                       "LENGTH=8760",           "" },

	{ SSC_OUTPUT,       SSC_ARRAY,       "P_cond",            "Condenser pressure",                                                "Pa",           "",            "Type250",        "*",                       "LENGTH=8760",           "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "f_bays",            "Condenser fraction of operating bays",                              "",         "",            "Type250",        "*",                       "LENGTH=8760",           "" },

    //fossil backup
	{ SSC_OUTPUT,       SSC_ARRAY,       "q_aux_heat",           "Fossil thermal power produced",                                  "MWt",          "",            "Outputs",        "*",                       "LENGTH=8760",           "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "q_aux_fuel",           "Fossil fuel usage",                                              "MMBTU",        "",            "Outputs",        "*",                       "LENGTH=8760",           "" },

    //parasitics
	{ SSC_OUTPUT,       SSC_ARRAY,       "P_tower_pump",         "Parasitic power receiver HTF pump",                              "MWe",           "",            "Outputs",        "*",                       "LENGTH=8760",           "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "htf_pump_power",       "Parasitic power TES and Cycle HTF pump",                         "MWe",          "",            "Outputs",        "*",                       "LENGTH=8760",           "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "pparasi",              "Parasitic power heliostat drives",                               "MWe",          "",            "Outputs",        "*",                       "LENGTH=8760",           "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "P_plant_balance_tot",  "Parasitic power generation-dependent load",                      "MWe",          "",            "Outputs",        "*",                       "LENGTH=8760",           "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "P_fixed",              "Parasitic power fixed load",                                     "MWe",          "",            "Outputs",        "*",                       "LENGTH=8760",           "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "P_aux",                "Parasitic power auxiliary heater operation",                     "MWe",          "",            "Outputs",        "*",                       "LENGTH=8760",           "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "P_cooling_tower_tot",  "Parasitic power condenser operation",                            "MWe",          "",            "Outputs",        "*",                       "LENGTH=8760",           "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "P_piping_tot",         "Parasitic power equiv. header pipe losses",                      "MWe",          "",            "Outputs",        "*",                       "LENGTH=8760",           "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "P_tank_heater",        "Parasitic power TES freeze protection",                          "MWe",          "",            "Outputs",        "*",                       "LENGTH=8760",           "" },
	//{ SSC_OUTPUT,       SSC_ARRAY,       "P_tower_par",          "Parasitic power receiver freeze protection",                     "MWe",          "",            "Outputs",        "*",                       "LENGTH=8760",           "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "P_parasitics",         "Parasitic power total consumption",                              "MWe",          "",            "Outputs",        "*",                       "LENGTH=8760",           "" },

	
    //additional SDK parameters
	{ SSC_OUTPUT,       SSC_MATRIX,      "eff_lookup",           "Field efficiency lookup matrix",                                    "",             "",            "Outputs",        "*",                       "",                      "" },
	{ SSC_OUTPUT,       SSC_MATRIX,      "flux_lookup",          "Receiver flux map lookup matrix",                                   "",             "",            "Outputs",        "*",                       "",                      "" },
	{ SSC_OUTPUT,       SSC_MATRIX,      "sunpos_eval",          "Sun positions for lookup calcs",                                    "deg",          "",            "Outputs",        "*",                       "",                      "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "land_area",            "Calculated solar field land area",                                  "acre",         "",            "Outputs",        "*",                       "",                      "" },
    { SSC_OUTPUT,       SSC_MATRIX,      "opt_history",          "Step history of optimization",                                      "",             "",            "Outputs",        "",                       "",           "" },


	// Add sco2 specific outputs: will need to figure out how to merge this with molten salt model
		// These values are imported from TCS to get design-point values - do NOT pass to SAM
	//{ SSC_OUTPUT, SSC_ARRAY, "UA_recup_des", "Recuperator conductance - array", "kW/K", "", "Outputs", "", "LENGTH=8760", "" },
	//{ SSC_OUTPUT, SSC_ARRAY, "P_low_des", "Main compressor inlet pressure - array", "kPa", "", "Outputs", "", "LENGTH=8760", "" },
	//{ SSC_OUTPUT, SSC_ARRAY, "P_high_des", "Main compressor outlet pressure - array", "kPa", "", "Outputs", "", "LENGTH=8760", "" },
	//{ SSC_OUTPUT, SSC_ARRAY, "f_recomp_des", "Recompression fraction - array", "", "", "Outputs", "", "LENGTH=8760", "" },
	//{ SSC_OUTPUT, SSC_ARRAY, "UA_PHX_des", "PHX conductance - array", "", "", "Outputs", "", "LENGTH=8760", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "UA_recup_des", "", "kW/K", "", "Outputs", "", "LENGTH=8760", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "P_low_des", "", "kPa", "", "Outputs", "", "LENGTH=8760", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "P_high_des", "", "kPa", "", "Outputs", "", "LENGTH=8760", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "f_recomp_des", "", "", "", "Outputs", "", "LENGTH=8760", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "UA_PHX_des", "", "", "", "Outputs", "", "LENGTH=8760", "" },

		// Single values from design-point hourly arrays
	{ SSC_OUTPUT,       SSC_NUMBER,       "UA_recup_des_value",         "Cycle: Recuperator conductance",                                           "kW/K",         "",            "Outputs",        "",                       "",           "" },
	{ SSC_OUTPUT,       SSC_NUMBER,       "P_low_des_value",            "Cycle: Main compressor inlet pressure",                                    "kPa",          "",            "Outputs",        "",                       "",           "" },
	{ SSC_OUTPUT,       SSC_NUMBER,       "P_high_des_value",           "Cycle: Main compressor outlet pressure",                                   "kPa",          "",            "Outputs",        "",                       "",           "" },
	{ SSC_OUTPUT,       SSC_NUMBER,       "f_recomp_des_value",         "Cycle: Recompression fraction",                                            "",             "",            "Outputs",        "",                       "",           "" },
	{ SSC_OUTPUT,       SSC_NUMBER,       "UA_PHX_des_value",           "Cycle: PHX conductance",                                                   "kW/K",         "",            "Outputs",        "",                       "",           "" },
	
	{ SSC_OUTPUT,       SSC_ARRAY,       "m_dot_balance",        "Relative mass flow balance error",                             "",             "",            "Controller",     "*",                       "",           "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "q_balance",            "Relative energy balance error",                                "",             "",            "Controller",     "*",                       "",           "" },

//	{ SSC_OUTPUT, SSC_ARRAY, "hourly_energy", "Hourly energy", "kWh", "", "Net_E_Calc", "*", "LENGTH=8760", "" },  

	// Annual Outputs - Displayed under Data->Single Values
	{ SSC_OUTPUT, SSC_NUMBER, "annual_energy",        "Annual energy",                                "kWh",         "",    "Type228",    "*", "", "" },
	{ SSC_OUTPUT, SSC_NUMBER, "annual_W_cycle_gross", "Electrical source - Power cycle gross output", "kWh",         "",    "Type228",    "*", "", "" },
																												     
	{ SSC_OUTPUT, SSC_NUMBER, "conversion_factor",    "Gross to Net Conversion Factor",                "%",          "",    "Calculated", "*", "", "" },
	{ SSC_OUTPUT, SSC_NUMBER, "capacity_factor",      "Capacity factor",                               "%",          "",    "",           "*", "", "" },
	{ SSC_OUTPUT, SSC_NUMBER, "kwh_per_kw",           "First year kWh/kW",                             "kWh/kW",     "",    "",           "*", "", "" },
	{ SSC_OUTPUT, SSC_NUMBER, "system_heat_rate",     "System heat rate",                              "MMBtu/MWh",  "",    "",           "*", "", "" },
	{ SSC_OUTPUT, SSC_NUMBER, "annual_fuel_usage",     "Annual fuel usage",                            "kWh",        "",    "",           "*", "", "" },



	var_info_invalid };

class cm_tcsmolten_salt : public tcKernel
{
public:

	cm_tcsmolten_salt(tcstypeprovider *prov)
	:tcKernel(prov)
	{
		add_var_info( _cm_vtab_tcsmolten_salt );
		//set_store_all_parameters(true); // default is 'false' = only store TCS parameters that match the SSC_OUTPUT variables above
		// performance adjustment factors
		add_var_info(vtab_adjustment_factors);
		add_var_info(vtab_technology_outputs);
	}

	void exec( )
	{

        //Add weather file reader unit
		int weather = 0;
		double avg_temp=0, avg_wind_v=0;

#ifdef DEBUG_WITH_TRNSYS_READER
		weather = add_unit("trnsys_weatherreader", "TRNSYS weather reader");
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

		//avg_temp = ( as_integer("receiver_type") == 0 ) ? 10.3 : 15;
		//avg_wind_v = 0;
#else
		weather = add_unit("weatherreader", "TCS weather reader");

		//Set weatherreader parameters
		set_unit_value( weather, "file_name", as_string("solar_resource_file") );
		set_unit_value( weather, "track_mode", 0.0 );
		set_unit_value( weather, "tilt", 0.0 );
		set_unit_value( weather, "azimuth", 0.0 );

		//avg_temp = as_double("T_db");
		//avg_wind_v = as_double("V_wind_10");
#endif

		// need to check on what to do with these values
		avg_temp = ( as_integer("receiver_type") == 0 ) ? 10.3 : 15;
		avg_wind_v = 0;

		// Logic to choose between steam and sco2 power cycle 
		bool is_steam_pc = true;
		int pb_tech_type = as_integer("pc_config");
		if( pb_tech_type == 1 )
		{
			pb_tech_type = 424;
			is_steam_pc = false;
		}

		// add other units		
		int	tou = add_unit("tou_translator", "Time of Use Translator");
		//int type221_hel_field = add_unit("sam_mw_pt_type221");

		int type251_controller = 0;
		int type_hel_field = 0;
		int type222_receiver = 0, type232_cav_rec = 0;

		if( is_steam_pc )
		{
			type_hel_field = add_unit("sam_mw_pt_heliostatfield");

			if( as_integer("receiver_type") == 0 )
				type222_receiver = add_unit("sam_mw_pt_type222");
			else
				type232_cav_rec = add_unit("sam_lf_st_pt_type232");

			type251_controller = add_unit("sam_mw_trough_type251");
		}
		else
		{
			type251_controller = add_unit("sam_mw_trough_type251");
			type_hel_field = add_unit("sam_mw_pt_heliostatfield");

			if( as_integer("receiver_type") == 0 )
				type222_receiver = add_unit("sam_mw_pt_type222");
			else
				type232_cav_rec = add_unit("sam_lf_st_pt_type232");
		}

		int type224_powerblock = 0;
		int type424_sco2 = 0;
		if( is_steam_pc )
			type224_powerblock = add_unit("sam_mw_pt_type224");
		else
		{
			type424_sco2 = add_unit("sam_sco2_recomp_type424");
		}
				
		int type228_parasitics = add_unit("sam_mw_pt_type228");

		set_unit_value_ssc_matrix(tou, "weekday_schedule"); // tou values from control will be between 1 and 9
		set_unit_value_ssc_matrix(tou, "weekend_schedule");

		// Heliostat field
		set_unit_value_ssc_double(type_hel_field, "run_type");// , 0);	//0=auto, 1=user-type_hel_field, 2=user data
		set_unit_value_ssc_double(type_hel_field, "helio_width");//, 12.);
		set_unit_value_ssc_double(type_hel_field, "helio_height");//, 12.);
		set_unit_value_ssc_double(type_hel_field, "helio_optical_error");//, 0.00153);
		set_unit_value_ssc_double(type_hel_field, "helio_active_fraction");//, 0.97);
        set_unit_value_ssc_double(type_hel_field, "dens_mirror");
		set_unit_value_ssc_double(type_hel_field, "helio_reflectance");//, 0.90);
		set_unit_value_ssc_double(type_hel_field, "rec_absorptance");//, 0.94);
        
        bool is_optimize = as_boolean("is_optimize");
        
        /* 
        Any parameter that's dependent on the size of the solar field must be recalculated here 
        if the optimization is happening within the cmod
        */
        double H_rec, D_rec, rec_aspect, THT, A_sf;

        if(is_optimize)
        {
            //Run solarpilot right away to update values as needed
            solarpilot_invoke spi( this );
            spi.run();
            //AutoPilot_S *sapi = spi.GetSAPI();

            //Optimization iteration history
            vector<vector<double> > steps;
            vector<double> obj, flux;
            spi.opt.getOptimizationSimulationHistory(steps, obj, flux);
            int nr = steps.size();
            int nc = steps.front().size() + 2;
            ssc_number_t *ssc_hist = allocate( "opt_history", nr, nc );
            for( size_t i=0; i<nr; i++){
                
                for( size_t j=0; j<steps.front().size(); j++)
                    ssc_hist[i*nc + j] = steps.at(i).at(j);
                ssc_hist[i*nc + nc-2] = obj.at(i);
                ssc_hist[i*nc + nc-1] = flux.at(i);

            }

		    //receiver calculations
            H_rec = spi.recs.front().height;
            rec_aspect = spi.recs.front().aspect;
            THT = spi.layout.h_tower;
            //update heliostat position table
            nr = (int)spi.layout.heliostat_positions.size();
            ssc_number_t *ssc_hl = allocate( "helio_positions", nr, 2 );
            for(int i=0; i<nr; i++){
                ssc_hl[i*2] = (ssc_number_t)spi.layout.heliostat_positions.at(i).location.x;
                ssc_hl[i*2+1] = (ssc_number_t)spi.layout.heliostat_positions.at(i).location.y;
            }

            A_sf = as_double("helio_height") * as_double("helio_width") * as_double("dens_mirror") * (double)nr;

            //update piping length for parasitic calculation
            double piping_length = THT * as_double("piping_length_mult") + as_double("piping_length_const");
            
            //update assignments for cost model
		    assign("H_rec", var_data((ssc_number_t)H_rec));
            assign("rec_height", var_data((ssc_number_t)H_rec));
		    assign("rec_aspect", var_data((ssc_number_t)rec_aspect));
            assign("D_rec", var_data((ssc_number_t)(H_rec/rec_aspect)));
		    assign("THT", var_data((ssc_number_t)THT));
            assign("h_tower", var_data((ssc_number_t)THT));
		    assign("A_sf", var_data((ssc_number_t)A_sf));
            assign("piping_length", var_data((ssc_number_t)piping_length) );

            //Update the total installed cost
            double total_direct_cost = 0.;
            double A_rec;
            switch (spi.recs.front().type)
            {
            case sp_receiver::TYPE::CYLINDRICAL:
            {
                double h = spi.recs.front().height;
                double d = h/spi.recs.front().aspect;
                A_rec =  h*d*3.1415926;
                break;
            }
            case sp_receiver::TYPE::CAVITY:
            case sp_receiver::TYPE::FLAT:
                double h = spi.recs.front().height;
                double w = h/spi.recs.front().aspect;
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
            double land_area = spi.layout.land_area * as_double("csp.pt.sf.land_overhead_factor") + as_double("csp.pt.sf.fixed_land_area");

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
        D_rec = H_rec/rec_aspect;

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


        if(run_type == 1){
            set_unit_value_ssc_matrix(type_hel_field, "helio_positions");
        }
        else if(run_type == 2){
            set_unit_value_ssc_matrix(type_hel_field, "eta_map");
		    set_unit_value_ssc_matrix(type_hel_field, "flux_positions");
		    set_unit_value_ssc_matrix(type_hel_field, "flux_maps");
        }
        

		
// for user specified x,y field

		bool bConnected = connect(weather, "wspd", type_hel_field, "vwind");
		set_unit_value_ssc_double(type_hel_field, "field_control", 1.);
		set_unit_value_ssc_double(weather, "solzen", 90.);	//initialize to be on the horizon
		
		// 4.17.15 twn: Need to connect controller defocus to heliostat field model
		bConnected &= connect(type251_controller, "defocus", type_hel_field, "field_control");
		bConnected &= connect(weather, "solzen", type_hel_field, "solzen");
		bConnected &= connect(weather, "solazi", type_hel_field, "solaz");



		if ( as_integer("receiver_type") == 0 )
		{
			// Receiver (type 222) parameters
			set_unit_value_ssc_double( type222_receiver, "N_panels" );
			set_unit_value_ssc_double( type222_receiver, "D_rec" , D_rec);
			set_unit_value_ssc_double( type222_receiver, "H_rec" , H_rec);
			set_unit_value_ssc_double( type222_receiver, "THT" , THT);
			set_unit_value_ssc_double( type222_receiver, "d_tube_out" );
			set_unit_value_ssc_double( type222_receiver, "th_tube" );
			set_unit_value_ssc_double( type222_receiver, "mat_tube" );
			set_unit_value_ssc_double( type222_receiver, "rec_htf" );
			set_unit_value_ssc_matrix( type222_receiver, "field_fl_props" );
			set_unit_value_ssc_double( type222_receiver, "Flow_type" );
			set_unit_value_ssc_double( type222_receiver, "epsilon" );
			set_unit_value_ssc_double( type222_receiver, "hl_ffact" );
			set_unit_value_ssc_double( type222_receiver, "T_htf_hot_des" );
			set_unit_value_ssc_double( type222_receiver, "T_htf_cold_des" );
			set_unit_value_ssc_double( type222_receiver, "f_rec_min" );
			set_unit_value_ssc_double( type222_receiver, "Q_rec_des" );
			set_unit_value_ssc_double( type222_receiver, "rec_su_delay" );
			set_unit_value_ssc_double( type222_receiver, "rec_qf_delay" );
			set_unit_value_ssc_double( type222_receiver, "m_dot_htf_max" );
			set_unit_value_ssc_double( type222_receiver, "A_sf", A_sf );
			set_unit_value_ssc_double( type222_receiver, "n_flux_x" );
			set_unit_value_ssc_double( type222_receiver, "n_flux_y" );
			set_unit_value_ssc_double( type222_receiver, "piping_loss" );
			set_unit_value_ssc_double( type222_receiver, "piping_length_add", "piping_length_const");
			set_unit_value_ssc_double( type222_receiver, "piping_length_mult", "piping_length_mult");

			// Constant inputs (so ... should be parameters??)
			set_unit_value_ssc_double(type222_receiver, "T_salt_hot_target");
			set_unit_value_ssc_double(type222_receiver, "eta_pump"); 
			set_unit_value_ssc_double(type222_receiver, "night_recirc", 0.0);		// 8.15.15 twn: this is hardcoded for now - need to check that it is functioning correctly and reporting correct parasitics
			set_unit_value_ssc_double(type222_receiver, "hel_stow_deploy");

			// Make all the connections to/from the Receiver (type 222)
			bConnected &= connect(weather, "solazi", type222_receiver, "azimuth");
			bConnected &= connect(weather, "solzen", type222_receiver, "zenith");
			bConnected &= connect(type251_controller, "T_field_in", type222_receiver, "T_salt_cold");
			bConnected &= connect(weather, "wspd", type222_receiver, "V_wind_10");
			bConnected &= connect(weather, "pres", type222_receiver, "P_amb");
			bConnected &= connect(weather, "tdew", type222_receiver, "T_dp");
			bConnected &= connect(weather, "beam", type222_receiver, "I_bn");
			bConnected &= connect(type_hel_field, "eta_field", type222_receiver, "field_eff");
			bConnected &= connect(weather, "tdry", type222_receiver, "T_db");
					   
			bConnected &= connect( type222_receiver, "m_dot_salt_tot", type251_controller, "m_dot_field" );
			bConnected &= connect( type222_receiver, "T_salt_hot", type251_controller, "T_field_out" );
			bConnected &= connect( type222_receiver, "q_startup", type251_controller, "q_startup" );		// This input is not used by the controller
			bConnected &= connect( type222_receiver, "W_dot_pump", type228_parasitics, "P_tower_pump");

			bConnected &= connect( type_hel_field, "flux_map", type222_receiver, "flux_map");

		} // external receiver

		else {
			// cavity receiver
            throw exec_error("tcs_moltensalt", util::format("Connections and value settings haven't been programmed for this module yet! (MJW 2104.11.21") );
			set_unit_value_ssc_double(type232_cav_rec, "rec_d_spec"); //  Rec_d_spec);
			set_unit_value_ssc_double(type232_cav_rec, "h_rec", as_double("h_rec_panel")); //  h_rec);
			set_unit_value_ssc_double(type232_cav_rec, "h_lip"); //  H_lip);
			set_unit_value_ssc_double(type232_cav_rec, "h_tower", THT ); //  h_tower);
			set_unit_value_ssc_double(type232_cav_rec, "rec_angle"); //  recangle);
			set_unit_value_ssc_double(type232_cav_rec, "d_tube_out"); //  d_tube);
			set_unit_value_ssc_double(type232_cav_rec, "th_tube"); //  th_tube);
			set_unit_value_ssc_double(type232_cav_rec, "eta_pump"); //  eta_rec_pump);
			set_unit_value_ssc_double(type232_cav_rec, "hel_stow", as_double("hel_stow_deploy")); //  hel_stow_deploy);
			set_unit_value_ssc_double(type232_cav_rec, "flow_pattern", as_double("Flow_type") );
			set_unit_value_ssc_double(type232_cav_rec, "htf", as_double("rec_htf") ); //  HTF);
			set_unit_value_ssc_matrix(type232_cav_rec, "field_fl_props"); //  [0]);
			set_unit_value_ssc_double(type232_cav_rec, "material", as_double("mat_tube")); //  Material);
			set_unit_value_ssc_double(type232_cav_rec, "hl_ffact"); //  hl_ffact);
			set_unit_value_ssc_double(type232_cav_rec, "T_htf_hot_des"); //  HTF_rec_out);
			set_unit_value_ssc_double(type232_cav_rec, "T_htf_cold_des"); //  T_HTF_out_ref);
			set_unit_value_ssc_double(type232_cav_rec, "f_rec_min"); //  f_rec_min);
			set_unit_value_ssc_double(type232_cav_rec, "q_rec_des", as_double("Q_rec_des")); //  Q_rec_des);
			set_unit_value_ssc_double(type232_cav_rec, "rec_su_delay"); //  rec_su_delay);
			set_unit_value_ssc_double(type232_cav_rec, "rec_qf_delay"); //  rec_qf_delay);
			set_unit_value_ssc_double(type232_cav_rec, "conv_model"); //  h_Model);
			set_unit_value_ssc_double(type232_cav_rec, "m_dot_htf_max"); //  Rec_HTF_max_flow);
			set_unit_value_ssc_matrix(type232_cav_rec, "eps_wavelength"); //  [[4, e_solar, e_solar_p], [100,e_thermal,e_thermal_p]]);
			set_unit_value_ssc_double(type232_cav_rec, "conv_coupled"); //  h_Type);
			set_unit_value_ssc_double(type232_cav_rec, "conv_forced"); //  forced_convection);
			set_unit_value_ssc_double(type232_cav_rec, "h_wind_meas"); //  h_wind_measurement);
			set_unit_value_ssc_double(type232_cav_rec, "conv_wind_dir"); //  wind_direct_depend);
			set_unit_value_ssc_matrix(type232_cav_rec, "fluxmap_angles"); //  arr_sol_pos);
			set_unit_value_ssc_matrix(type232_cav_rec, "fluxmap"); //  arr_flux);

			// Set initial values for inputs
			set_unit_value_ssc_double( type232_cav_rec, "azimuth", as_double("azimuth_ini") ); // 0
			set_unit_value_ssc_double( type232_cav_rec, "zenith", as_double("zenith_ini") ); // 0
			set_unit_value_ssc_double( type232_cav_rec, "T_htf_hot", as_double("T_htf_hot_des")); //  HTF_rec_out );
			set_unit_value_ssc_double( type232_cav_rec, "T_htf_cold", as_double("T_htf_cold_des")); //  290 );
			set_unit_value_ssc_double( type232_cav_rec, "P_amb", 1.0); //  0.9435 );
			set_unit_value_ssc_double( type232_cav_rec, "T_dp", 0.0); //  0.0 );
			set_unit_value_ssc_double( type232_cav_rec, "I_bn", 0.0); //  0.0 );
			set_unit_value_ssc_double( type232_cav_rec, "eta_field", as_double("field_eff")); //  0.0 );
			set_unit_value_ssc_double( type232_cav_rec, "T_amb", avg_temp); //  15.0 );
			set_unit_value_ssc_double( type232_cav_rec, "u_wind", avg_wind_v ); //  0.0 );
			set_unit_value_ssc_double( type232_cav_rec, "deg_wind", 0.0); // 8.7.15 twn: Don't need an SSC INPUT for a TCS *INPUT* as it is provided by another TCS type OUTPUT

			// Make all the connections to/from the Cavity Receiver (type 232)
			bConnected &= connect( weather, "solazi", type232_cav_rec, "azimuth" );
			bConnected &= connect( weather, "solzen", type232_cav_rec, "zenith" );
			bConnected &= connect( type251_controller, "T_field_in", type232_cav_rec, "T_htf_cold" );
			bConnected &= connect( weather, "pres", type232_cav_rec, "P_amb" );
			bConnected &= connect( weather, "tdew", type232_cav_rec, "T_dp" );
			bConnected &= connect( weather, "beam", type232_cav_rec, "I_bn" );
			bConnected &= connect( type_hel_field, "eta_field", type232_cav_rec, "eta_field" );
			bConnected &= connect(weather, "tdry", type232_cav_rec, "T_amb");
			bConnected &= connect(weather, "wspd", type232_cav_rec, "u_wind");
			bConnected &= connect(weather, "wdir", type232_cav_rec, "deg_wind");
					   
			bConnected &= connect( type232_cav_rec, "m_htf_total", type251_controller, "m_dot_field" );
			bConnected &= connect( type232_cav_rec, "T_htf_hot_out", type251_controller, "T_field_out" );
			bConnected &= connect( type232_cav_rec, "Q_startup", type251_controller, "q_startup" );
			bConnected &= connect( type232_cav_rec, "W_pump", type228_parasitics, "P_tower_pump");

			// **********************************************************************
			// 8.15.15 twn: Need to handle tower piping losses in Type 232 or add them back to 228
			// **********************************************************************


		}// cavity receiver

		set_unit_value_ssc_double(type251_controller, "field_fluid" ); //, 17);
		set_unit_value_ssc_matrix(type251_controller, "field_fl_props" ); //, [0]);
		set_unit_value_ssc_double(type251_controller, "store_fluid", as_integer("field_fluid") ); //, 17);
//		set_unit_value_ssc_matrix(type251_controller, "field_fl_props", "store_fl_props");
		set_unit_value_ssc_matrix(type251_controller, "store_fl_props", "field_fl_props");
		set_unit_value_ssc_double(type251_controller, "tshours"); //, 10);
		set_unit_value_ssc_double(type251_controller, "is_hx", 0.0 );	// 8.7.15 twn: MSPT assumes direct storage, no user input required here
		set_unit_value_ssc_double(type251_controller, "dt_hot", 0.0 );	// 8.7.15 twn: MSPT assumes direct storage, so can hardcode hot and cold approach temps = 0 and remove from SSC INPUTs
		set_unit_value_ssc_double(type251_controller, "dt_cold", 0.0 );
		set_unit_value_ssc_double(type251_controller, "hx_config", 0.0 );	// 8.7.15 twn: similarly, there is no HX, so no user input required
		set_unit_value_ssc_double(type251_controller, "q_max_aux" ); //, 115/0.412);
		set_unit_value_ssc_double(type251_controller, "lhv_eff", as_double("eta_lhv"));			// 9.17.14 twn: input lhv here to calculate fuel usage

		if ( as_integer("receiver_type") == 0 )
			set_unit_value_ssc_double(type251_controller, "T_set_aux" ); //, 594);
		else
			set_unit_value_ssc_double(type251_controller, "T_htf_hot_ref"); //, 574 );

		set_unit_value_ssc_double(type251_controller, "V_tank_hot_ini" ); //, 3895.75);
		set_unit_value_ssc_double(type251_controller, "T_tank_hot_ini" ); //, 574.0);
		set_unit_value_ssc_double(type251_controller, "T_tank_cold_ini" ); //, 290.0);
		set_unit_value_ssc_double(type251_controller, "vol_tank" ); //, 12985.8);
		set_unit_value_ssc_double(type251_controller, "h_tank" ); //, 20.0);
		set_unit_value_ssc_double(type251_controller, "h_tank_min" ); //, 1.0);
		set_unit_value_ssc_double(type251_controller, "u_tank" ); //, 0.4);
		set_unit_value_ssc_double(type251_controller, "tank_pairs" ); //, 1);
		set_unit_value_ssc_double(type251_controller, "cold_tank_Thtr" ); //, 280.0);
		set_unit_value_ssc_double(type251_controller, "hot_tank_Thtr" ); //, 500.);
		set_unit_value_ssc_double(type251_controller, "cold_tank_max_heat"); //, 30);
		set_unit_value_ssc_double(type251_controller, "hot_tank_max_heat");
		set_unit_value_ssc_double(type251_controller, "T_field_in_des" ); //, 290);
		set_unit_value_ssc_double(type251_controller, "T_field_out_des" ); //, 574);
		set_unit_value_ssc_double(type251_controller, "q_pb_design" ); //, 115/0.412);
		set_unit_value_ssc_double(type251_controller, "W_pb_design" ); //, 115);
		set_unit_value_ssc_double(type251_controller, "cycle_max_frac" ); //, 1.05);

		double cycle_cutoff_fraction = as_double("cycle_cutoff_frac");
		bool is_cycle_cutoff_message = false;
		string msg_to_log = "is this working?";
		if( !is_steam_pc )
		{
			if(cycle_cutoff_fraction < 0.5)
			{
				char tstr[300];
				sprintf(tstr, "The cycle cutoff fraction, %lg, was reset to 0.5 to improve the sCO2 model off-design convergence\n", cycle_cutoff_fraction);

				msg_to_log = "";
				msg_to_log.append(tstr);

				cycle_cutoff_fraction = 0.5;
				is_cycle_cutoff_message = true;
			}
		}
		set_unit_value_ssc_double(type251_controller, "cycle_cutoff_frac", cycle_cutoff_fraction ); //, 0.25);
		

		set_unit_value_ssc_double(type251_controller, "solarm" ); //, 669.903/(115/0.412) );
		set_unit_value_ssc_double(type251_controller, "pb_pump_coef" ); //, 0.55);
		set_unit_value_ssc_double(type251_controller, "tes_pump_coef", 0.0 ); // 8.5.15 twn: MSPT does not have TES pumping losses as it assumes direct storage
		set_unit_value_ssc_double(type251_controller, "pb_fixed_par", 0.0 ); //  8.7.15 twn: MSPT calculates fixed parasitic losses in type 228, not type 251

		ssc_number_t *bop_array_test = allocate("bop_array_test", 5);
		ssc_number_t *aux_array_test = allocate("aux_array_test", 5);
		for( int ii = 0; ii < 5; ii++ )
		{
			bop_array_test[ii] = 0.0;
			aux_array_test[ii] = 0.0;
		}
		set_unit_value_ssc_array(type251_controller, "bop_array", "bop_array_test" ); //, [0.0,0.0,0.0,0.0,0.0]);
		set_unit_value_ssc_array(type251_controller, "aux_array", "aux_array_test" ); //, [0.0,0.0,0.0,0.0,0.0]);
		
		
		set_unit_value_ssc_double(type251_controller, "T_startup" ); //, 500);
		set_unit_value_ssc_double(type251_controller, "fossil_mode" ); //, 1);
		
		// 4.17.15 twn: for heliostat solar fields, should always assume partial defocusing is possible
		set_unit_value_ssc_double(type251_controller, "fthr_ok", 1);
		//double check_fthr_ok = as_double("fthr_ok"); 

		set_unit_value_ssc_double(type251_controller, "nSCA" ); //, 1);
		set_unit_value_ssc_double(type251_controller, "I_bn_des" ); //, 950);
		set_unit_value_ssc_double(type251_controller, "fc_on" ); //, 0);
		set_unit_value_ssc_double(type251_controller, "q_sby_frac" ); //, 0.2);
		set_unit_value_ssc_double(type251_controller, "t_standby_reset" ); //, 0.2);
		set_unit_value_ssc_double(type251_controller, "sf_type" ); //, 2);
		set_unit_value_ssc_double(type251_controller, "tes_type" ); //, 1);
		set_unit_value_ssc_array(type251_controller, "tslogic_a" ); //, [0,0,0,0,0,0,0,0,0]);
		set_unit_value_ssc_array(type251_controller, "tslogic_b" ); //, [0,0,0,0,0,0,0,0,0]);
		set_unit_value_ssc_array(type251_controller, "tslogic_c" ); //, [1,1,1,1,1,1,1,1,1]);	//Not sure if TOU schedule is synced, so make all variables independent of it
		set_unit_value_ssc_array(type251_controller, "ffrac" ); //, [0,0,0,0,0,0,0,0,0]);
		set_unit_value_ssc_double(type251_controller, "tc_fill" ); //, 8);
		set_unit_value_ssc_double(type251_controller, "tc_void" ); //, 0.25);
		set_unit_value_ssc_double(type251_controller, "t_dis_out_min" ); //, 500);
		set_unit_value_ssc_double(type251_controller, "t_ch_out_max" ); //, 400);
		set_unit_value_ssc_double(type251_controller, "nodes" ); //, 100);
		set_unit_value_ssc_double(type251_controller, "f_tc_cold" ); //, 0.7);
		if( !is_steam_pc )
		{
			set_unit_value_ssc_double(type251_controller, "pb_tech_type", pb_tech_type);
		}
	  //set_unit_value_ssc_array( type251_controller, "TOU_schedule");

		// Set initial values for inputs generated from subsequently called types
		set_unit_value_ssc_double( type251_controller, "m_dot_htf_ref", 1.0 ); //, 1.0 );
		set_unit_value_ssc_double( type251_controller, "T_pb_out", "T_htf_hot_ref"); //, T_HTF_out_ref );

		// Connect Controller (type 251) inputs
		bConnected &= connect( weather, "beam", type251_controller, "I_bn" );
		if( is_steam_pc )
		{
			bConnected &= connect(type224_powerblock, "m_dot_htf_ref", type251_controller, "m_dot_htf_ref");
			bConnected &= connect(type224_powerblock, "T_htf_cold", type251_controller, "T_pb_out");
			bConnected &= connect(type224_powerblock, "m_dot_demand", type251_controller, "m_pb_demand");	//This is input is not used by the controller
		}
		else
		{
			bConnected &= connect(type424_sco2, "m_dot_htf_ref", type251_controller, "m_dot_htf_ref");
			bConnected &= connect(type424_sco2, "T_htf_cold", type251_controller, "T_pb_out");
			bConnected &= connect(type424_sco2, "m_dot_demand", type251_controller, "m_pb_demand");	//This is input is not used by the controller
		}
		bConnected &= connect( weather, "tdry", type251_controller, "T_amb" );		
		bConnected &= connect(tou, "tou_value", type251_controller, "TOUPeriod");

		// Inputs for sCO2 connection: design parameters solved in type 424 initial call
		if( !is_steam_pc )
		{
			// bConnected &= connect(type424_sco2, "o_W_dot_net",     type251_controller,   "i_W_dot_net"      );
			// bConnected &= connect(type424_sco2, "o_T_mc_in",       type251_controller,	 "i_T_mc_in"      	);
			// bConnected &= connect(type424_sco2,	"o_T_t_in",        type251_controller,	 "i_T_t_in"       	);
			// bConnected &= connect(type424_sco2,	"o_P_mc_in",       type251_controller,	 "i_P_mc_in"      	);
			// bConnected &= connect(type424_sco2,	"o_P_mc_out",      type251_controller,	 "i_P_mc_out"     	);
			// bConnected &= connect(type424_sco2,	"o_UA_LT",         type251_controller,	 "i_UA_LT"        	);
			// bConnected &= connect(type424_sco2,	"o_UA_HT",         type251_controller,	 "i_UA_HT"        	);
			// bConnected &= connect(type424_sco2,	"o_recomp_frac",   type251_controller,	 "i_recomp_frac"  	);
			// bConnected &= connect(type424_sco2,	"o_eta_mc",        type251_controller,	 "i_eta_mc"       	);
			// bConnected &= connect(type424_sco2,	"o_eta_rc",        type251_controller,	 "i_eta_rc"       	);
			// bConnected &= connect(type424_sco2,	"o_eta_t",         type251_controller,	 "i_eta_t"        	);
			// bConnected &= connect(type424_sco2,	"o_N_sub_hxrs",    type251_controller,	 "i_N_sub_hxrs"   	);
			// bConnected &= connect(type424_sco2,	"o_P_high_limit",  type251_controller,	 "i_P_high_limit" 	);
			// bConnected &= connect(type424_sco2,	"o_N_turbine",     type251_controller,	 "i_N_turbine"    	);
			// bConnected &= connect(type424_sco2,	"o_DP_LT_c",       type251_controller,	 "o_DP_LT_c"      	);
			// bConnected &= connect(type424_sco2,	"o_DP_LT_h",       type251_controller,	 "o_DP_LT_h"      	);
			// bConnected &= connect(type424_sco2,	"o_DP_HT_c",       type251_controller,	 "o_DP_HT_c"      	);
			// bConnected &= connect(type424_sco2,	"o_DP_HT_h",       type251_controller,	 "o_DP_HT_h"      	);
			// bConnected &= connect(type424_sco2,	"o_DP_PC_h",       type251_controller,	 "o_DP_PC_h"      	);
			// bConnected &= connect(type424_sco2,	"o_DP_PHX_c",      type251_controller,	 "o_DP_PHX_c"     	);
			// bConnected &= connect(type424_sco2,	"o_deltaT_mc",     type251_controller,	 "o_deltaT_mc"    	);
			// bConnected &= connect(type424_sco2,	"o_deltaT_t",      type251_controller,	 "o_deltaT_t"     	);
			bConnected &= connect(type424_sco2, "o_T_htf_cold_des", type251_controller, "i_T_htf_cold_des");
		}

		// Set powerblock parameters
		if( is_steam_pc )
		{
			// Set Powerblock (type 224) Parameters
			set_unit_value_ssc_double(type224_powerblock, "P_ref"); //, 115);
			set_unit_value_ssc_double(type224_powerblock, "eta_ref"); //, 0.412);
			set_unit_value_ssc_double(type224_powerblock, "T_htf_hot_ref"); //, 574);
			set_unit_value_ssc_double(type224_powerblock, "T_htf_cold_ref"); //, 290);
			set_unit_value_ssc_double(type224_powerblock, "dT_cw_ref"); //, 10);
			set_unit_value_ssc_double(type224_powerblock, "T_amb_des"); //, 43.0);
			set_unit_value_ssc_double(type224_powerblock, "HTF", "rec_htf"); // 8.7.15 twn: PC htf is same as receiver htf
			set_unit_value_ssc_matrix(type224_powerblock, "field_fl_props");
			set_unit_value_ssc_double(type224_powerblock, "q_sby_frac"); //, 0.2);
			set_unit_value_ssc_double(type224_powerblock, "P_boil"); //, 100);
			set_unit_value_ssc_double(type224_powerblock, "CT"); //, 2);
			set_unit_value_ssc_double(type224_powerblock, "startup_time"); //, 0.5);
			set_unit_value_ssc_double(type224_powerblock, "startup_frac"); //, 0.5);
			set_unit_value_ssc_double(type224_powerblock, "tech_type", 1.0);	// 8.7.15 twn: compute module is for MSPT, so hardcode tech type and remove user input
			set_unit_value_ssc_double(type224_powerblock, "T_approach"); //, 5);
			set_unit_value_ssc_double(type224_powerblock, "T_ITD_des"); //, 16);
			set_unit_value_ssc_double(type224_powerblock, "P_cond_ratio"); //, 1.0028);
			set_unit_value_ssc_double(type224_powerblock, "pb_bd_frac"); //, 0.02);
			set_unit_value_ssc_double(type224_powerblock, "P_cond_min"); //, 2);
			set_unit_value_ssc_double(type224_powerblock, "n_pl_inc"); //, 8);
			set_unit_value_ssc_array(type224_powerblock, "F_wc"); //, [0,0,0,0,0,0,0,0,0]);
		}
		else
		{
			set_unit_value_ssc_double(type424_sco2, "W_dot_net_des", as_double("P_ref"));					
			set_unit_value_ssc_double(type424_sco2, "eta_c");												
			set_unit_value_ssc_double(type424_sco2, "eta_t");												
			set_unit_value_ssc_double(type424_sco2, "P_high_limit");										
			set_unit_value_ssc_double(type424_sco2, "deltaT_PHX");											

			set_unit_value_ssc_double(type424_sco2, "deltaT_ACC", as_double("T_ITD_des"));
			set_unit_value_ssc_double(type424_sco2, "T_amb_des");
			set_unit_value_ssc_double(type424_sco2, "fan_power_perc", as_double("fan_power_perc_net"));
			set_unit_value_ssc_double(type424_sco2, "plant_elevation", 0.0);

			set_unit_value_ssc_double(type424_sco2, "T_htf_hot_des", as_double("T_htf_hot_ref"));
			set_unit_value_ssc_double(type424_sco2, "T_htf_cold_est", as_double("T_htf_cold_ref"));
			set_unit_value_ssc_double(type424_sco2, "eta_des", as_double("eta_ref"));
			set_unit_value_ssc_double(type424_sco2, "rec_htf");
			set_unit_value_ssc_matrix(type424_sco2, "field_fl_props");

			set_unit_value_ssc_double(type424_sco2, "startup_time");
			set_unit_value_ssc_double(type424_sco2, "startup_frac");
			set_unit_value_ssc_double(type424_sco2, "q_sby_frac");
			set_unit_value_ssc_double(type424_sco2, "cycle_cutoff_frac", cycle_cutoff_fraction);
		}

		if( is_steam_pc )
		{
			// Set Powerblock (type 224) Inputs
			set_unit_value_ssc_double(type224_powerblock, "mode", 2.0);		//Always set to 2 for type 251
			//set_unit_value_ssc_double( type224_powerblock, "demand_var" ); //, 110.0 );		//Don't need to set this?

			// Connect Powerblock (type 224) inputs
			bConnected &= connect(type251_controller, "T_pb_in", type224_powerblock, "T_htf_hot");
			bConnected &= connect(type251_controller, "m_dot_pb", type224_powerblock, "m_dot_htf");
			bConnected &= connect(weather, "twet", type224_powerblock, "T_wb");
			bConnected &= connect(type251_controller, "standby_control", type224_powerblock, "standby_control");
			bConnected &= connect(weather, "tdry", type224_powerblock, "T_db");
			bConnected &= connect(weather, "pres", type224_powerblock, "P_amb");
			bConnected &= connect(weather, "rhum", type224_powerblock, "rh");
			bConnected &= connect(tou, "tou_value", type224_powerblock, "TOU");
		}
		else
		{
			bConnected &= connect(type251_controller, "T_pb_in", type424_sco2, "T_htf_hot");
			bConnected &= connect(type251_controller, "m_dot_pb", type424_sco2, "m_dot_htf");
			bConnected &= connect(type251_controller, "standby_control", type424_sco2, "standby_control");
			bConnected &= connect(weather, "tdry", type424_sco2, "T_db");
			bConnected &= connect(weather, "pres", type424_sco2, "P_amb");
		}

		// Set Parasitics (type 228) Parameters
			// 8.15.15 twn: For MSPT, we're calculating piping losses in physical receiver model, so zero out tower piping parasitics here
			// ...... still need this in Type 228 for DSGPT
		set_unit_value_ssc_double(type228_parasitics, "Piping_loss", 0.0);
		set_unit_value_ssc_double(type228_parasitics, "piping_length_add", 0.0);
		set_unit_value_ssc_double(type228_parasitics, "piping_length_mult", 0.0);
		set_unit_value_ssc_double(type228_parasitics, "THT", THT);

		set_unit_value_ssc_double(type228_parasitics, "Design_power", "P_ref");
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

		// Set Parasitics (type 228) Inputs (Initial values?)
		// set_unit_value_ssc_double(type228_parasitics, "flow_from_storage", 0.0);
		set_unit_value_ssc_double(type228_parasitics, "P_hot_tank", 0.0);	// "tank_fp_par" from controller contains both hot and cold tank htr values
		//set_unit_value_ssc_double(type228_parasitics, "recirc_source", 0.0);

		// Connect Parasitics (type 228) module to others
		if( is_steam_pc )
		{
			bConnected &= connect(type224_powerblock, "W_cool_par", type228_parasitics, "P_cooling_tower");
			bConnected &= connect(type224_powerblock, "P_cycle", type228_parasitics, "P_plant_output");
			bConnected &= connect(type224_powerblock, "eta", type228_parasitics, "eta_cycle");
			//bConnected &= connect(type224_powerblock, "m_dot_htf_ref", type228_parasitics, "ref_htf_flow");
		}
		else
		{
			bConnected &= connect(type424_sco2, "W_cool_par", type228_parasitics, "P_cooling_tower");
			bConnected &= connect(type424_sco2, "P_cycle", type228_parasitics, "P_plant_output");
			bConnected &= connect(type424_sco2, "eta", type228_parasitics, "eta_cycle");
			//bConnected &= connect(type424_sco2, "m_dot_htf_ref", type228_parasitics, "ref_htf_flow");
		}
		
		bConnected &= connect(type_hel_field, "pparasi", type228_parasitics, "P_helio_track");		
		bConnected &= connect(type251_controller, "tank_fp_par", type228_parasitics, "P_cold_tank");		
		bConnected &= connect(type251_controller, "q_aux_heat", type228_parasitics, "aux_power");
		bConnected &= connect(type251_controller, "htf_pump_power", type228_parasitics, "P_htf_pump");

		// check if all connections worked
		if ( !bConnected )
			throw exec_error( "tcsmolten_salt", util::format("there was a problem connecting outputs of one unit to inputs of another for the simulation.") );

		// Run simulation
		size_t hours = 8760;
		if (0 > simulate(3600, hours*3600, 3600) )
			throw exec_error( "tcsmolten_salt", util::format("there was a problem simulating in the TCS molten salt model.") );

		// get the outputs
		if (!set_all_output_arrays() )
			throw exec_error( "tcsmolten_salt", util::format("there was a problem returning the results from the simulation.") );

		if(is_cycle_cutoff_message)
			log(msg_to_log, SSC_WARNING);

		set_output_array("gen", "P_out_net", 8760, 1000.0); // MWh to kWh

		//calculated field parameters
		int nr, nc;
		if ( double *fm = get_unit_value(type_hel_field, "flux_maps", &nr, &nc ) )
		{
			ssc_number_t *ssc_fm = allocate( "flux_lookup", nr, nc );
			for( size_t i=0;i<nr*nc;i++ )
				ssc_fm[i] = (ssc_number_t)fm[i];
		}

        if( double *etam = get_unit_value(type_hel_field, "eta_map", &nr, &nc ) )
        {
            ssc_number_t *ssc_etam = allocate( "eff_lookup", nr, nc );
            for( size_t i=0; i<nr*nc; i++ )
                ssc_etam[i] = (ssc_number_t)etam[i];
        }

        if( double *fpm = get_unit_value(type_hel_field, "flux_positions", &nr, &nc ) )
        {
            ssc_number_t *ssc_fpm = allocate( "sunpos_eval", nr, nc );
            for( size_t i=0; i<nr*nc; i++ )
                ssc_fpm[i] = (ssc_number_t)fpm[i];
        }
        if( run_type == 0){
            if( double *hl = get_unit_value(type_hel_field, "helio_positions", &nr, &nc ) )
            {
                ssc_number_t *ssc_hl = allocate( "layout_positions", nr, nc );
                for(size_t i=0; i<nr*nc; i++)
                    ssc_hl[i] = (ssc_number_t)hl[i];
            }
        }
        assign( "land_area", var_data( (ssc_number_t) get_unit_value_number(type_hel_field, "land_area" ) ) );
		//-----------

        accumulate_annual("gen", "annual_energy");			// already in kWh
		accumulate_annual("P_cycle", "annual_W_cycle_gross", 1000.0);	// convert to kWh

		// Calculated outputs
		ssc_number_t ae = as_number("annual_energy");
		ssc_number_t pg = as_number("annual_W_cycle_gross");
		ssc_number_t convfactor = (pg != 0) ? 100 * ae / pg : 0;
		assign("conversion_factor", convfactor);

		// performance adjustement factors
		adjustment_factors haf(this);
		if (!haf.setup())
			throw exec_error("tcsmolten_salt", "failed to setup adjustment factors: " + haf.error());
		// hourly_energy output
		ssc_number_t *p_hourly_energy = allocate("gen", 8760);
		// set hourly energy = tcs output Enet
		size_t count;
		ssc_number_t *hourly_energy = as_array("P_out_net", &count);//MWh
		if (count != 8760)
			throw exec_error("tcsmolten_salt", "gen count incorrect (should be 8760): " + count);
		// apply performance adjustments and convert from MWh to kWh
		for (size_t i = 0; i < count; i++)
		{
			p_hourly_energy[i] = hourly_energy[i] * (ssc_number_t)(haf(i) * 1000.0);
//			p_gen[i] = p_hourly_energy[i];
		}
		accumulate_annual("gen", "annual_energy"); // already in kWh

		ssc_number_t *q_balance = allocate("q_balance", count);
		
		ssc_number_t *q_rec_thermal = as_array("Q_thermal", &count);			// receiver to HTF
		ssc_number_t *q_tes_thermal = as_array("q_to_tes", &count);				// TES thermal energy into storage
		ssc_number_t *q_pc_thermal = as_array("q_pb", &count);					// Cycle thermal power input


		ssc_number_t *m_dot_balance = allocate("m_dot_balance", count);
		
		ssc_number_t *m_dot_rec = as_array("m_dot_field", &count);				// receiver mass flow rate
		ssc_number_t *m_dot_tes = as_array("m_dot_charge_field", &count);		// mass flow rate to storage
		ssc_number_t *m_dot_pc = as_array("m_dot_pb", &count);					// mass flow rate to power cycle

		double q_max = 0.0;
		double m_dot_max = 0.0;
		for( size_t i = 0; i < count; i++ )
		{
			q_max = fmax(0.0, fmax( fmax(abs(q_rec_thermal[i]), abs(q_tes_thermal[i])), abs(q_pc_thermal[i])));
			m_dot_max = fmax(0.0, fmax( fmax(abs(m_dot_rec[i]), abs(m_dot_tes[i])), abs(m_dot_pc[i])));

			if(q_max > 0.0)
				q_balance[i] = (q_rec_thermal[i] - (q_tes_thermal[i] + q_pc_thermal[i]))/q_max;
			else
				q_balance[i] = 0.0;
			
			if(m_dot_max > 0.0)
				m_dot_balance[i] = (m_dot_rec[i] - (m_dot_tes[i] + m_dot_pc[i]))/m_dot_max;
			else
				m_dot_balance[i] = 0.0;				
		}


		if (!is_steam_pc)
		{
			// Single values for sCO2 power cycle
			ssc_number_t *array_place_holder = as_array("UA_recup_des", &count);
			assign("UA_recup_des_value", array_place_holder[0]);

			array_place_holder = as_array("P_low_des", &count);
			assign("P_low_des_value", array_place_holder[0]);

			array_place_holder = as_array("P_high_des", &count);
			assign("P_high_des_value", array_place_holder[0]);

			array_place_holder = as_array("f_recomp_des", &count);
			assign("f_recomp_des_value", array_place_holder[0]);

			array_place_holder = as_array("UA_PHX_des", &count);
			assign("UA_PHX_des_value", array_place_holder[0]);
		}
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
			throw exec_error("tcsmolten_salt", "q_aux_fuel count incorrect (should be 8760): " + count);
		for (size_t i = 0; i < count; i++)
			fuel_usage_mmbtu += hourly_fuel[i];
		assign("system_heat_rate", 3.413); // samsim tcstrough_physical
		// www.unitjuggler.com/convert-energy-from-MMBtu-to-kWh.html
		assign("annual_fuel_usage", var_data((ssc_number_t)(fuel_usage_mmbtu * 293.297)));

	}

};

DEFINE_TCS_MODULE_ENTRY( tcsmolten_salt, "CSP model using the molten salt power tower TCS types.", 4 )
