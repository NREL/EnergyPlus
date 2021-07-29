// Steam Linear Fresnel - direct steam generation 
#include "core.h"
#include "tckernel.h"

// for new solver class.
#include "common.h"
#include "lib_weatherfile.h"
#include "csp_solver_lf_dsg_collector_receiver.h"

#include "csp_solver_pc_heat_sink.h"
#include "csp_solver_tou_block_schedules.h"
#include "csp_solver_two_tank_tes.h"

static var_info _cm_vtab_iph_linear_fresnel_dsg[] = {
/*	EXAMPLE LINES FOR INPUTS
    { SSC_INPUT,        SSC_NUMBER,      "XXXXXXXXXXXXXX",    "Label",                                                                               "",              "",            "sca",            "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "INTINTINTINT",      "Label",                                                                               "",              "",            "parasitic",      "*",                       "INTEGER",               "" },
    { SSC_INPUT,        SSC_ARRAY,       "XXXXXXXXXXX",       "Number indicating the receiver type",                                                 "",              "",            "hce",            "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "XXXXXXXXXXX",       "Label",                                                                               "",              "",            "tes",            "*",                       "",                      "" },
*/

//    VARTYPE           DATATYPE          NAME                 LABEL                                                                                   UNITS            META            GROUP            REQUIRED_IF                 CONSTRAINTS             UI_HINTS
    { SSC_INPUT,        SSC_STRING,      "file_name",         "local weather file path",                                                             "",              "",            "Weather",        "*",                       "LOCAL_FILE",            "" },
    { SSC_INPUT,        SSC_NUMBER,      "track_mode",        "Tracking mode",                                                                       "",              "",            "Weather",        "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "tilt",              "Tilt angle of surface/axis",                                                          "",              "",            "Weather",        "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "azimuth",           "Azimuth angle of surface/axis",                                                       "",              "",            "Weather",        "*",                       "",                      "" },
	{ SSC_INPUT,        SSC_NUMBER,      "system_capacity",   "Nameplate capacity",                                                                 "kW",             "",            "linear fresnelr", "*", "", "" },

    // TOU
    { SSC_INPUT,        SSC_MATRIX,      "weekday_schedule",  "12x24 Time of Use Values for week days",                                              "",             "",             "tou_translator", "*",                       "",                      "" }, 
    { SSC_INPUT,        SSC_MATRIX,      "weekend_schedule",  "12x24 Time of Use Values for week end days",                                          "",             "",             "tou_translator", "*",                       "",                      "" }, 

	// Type 261 (solar field collector) parameters
    { SSC_INPUT,        SSC_NUMBER,      "tes_hours",         "Equivalent full-load thermal storage hours",                                          "hr",            "",            "solarfield",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "q_max_aux",         "Maximum heat rate of the auxiliary heater",                                           "MW",            "",            "solarfield",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "LHV_eff",           "Fuel LHV efficiency (0..1)",                                                          "none",          "",            "solarfield",     "*",                       "",                      "" },
  //{ SSC_INPUT,        SSC_NUMBER,      "T_set_aux",         "Aux heater outlet temperature set point",                                             "C",             "",            "solarfield",     "*",                       "",                      "" },
  //{ SSC_INPUT,        SSC_NUMBER,      "T_field_in_des",    "Field design inlet temperature",                                                      "C",             "",            "solarfield",     "*",                       "",                      "" },
  //{ SSC_INPUT,        SSC_NUMBER,      "T_field_out_des",   "Field loop outlet design temperature",                                                "C",             "",            "solarfield",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "x_b_des",           "Design point boiler outlet steam quality",                                            "none",          "",            "solarfield",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "P_turb_des",        "Design-point turbine inlet pressure",                                                 "bar",           "",            "solarfield",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "fP_hdr_c",          "Average design-point cold header pressure drop fraction",                             "none",          "",            "solarfield",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "fP_sf_boil",        "Design-point pressure drop across the solar field boiler fraction",                   "none",          "",            "solarfield",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "fP_boil_to_sh",     "Design-point pressure drop between the boiler and superheater frac",                  "none",          "",            "solarfield",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "fP_sf_sh",          "Design-point pressure drop across the solar field superheater frac",                  "none",          "",            "solarfield",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "fP_hdr_h",          "Average design-point hot header pressure drop fraction",                              "none",          "",            "solarfield",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "q_pb_des",          "Design heat input to the power block",                                                "MW",            "",            "solarfield",     "*",                       "",                      "" },
  //{ SSC_INPUT,        SSC_NUMBER,      "W_pb_des",          "Rated plant capacity",                                                                "MW",            "",            "solarfield",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "cycle_max_fraction","Maximum turbine over design operation fraction",                                      "none",          "",            "solarfield",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "cycle_cutoff_frac", "Minimum turbine operation fraction before shutdown",                                  "none",          "",            "solarfield",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "t_sby",             "Low resource standby period",                                                         "hr",            "",            "solarfield",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "q_sby_frac",        "Fraction of thermal power required for standby",                                      "none",          "",            "solarfield",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "solarm",            "Solar multiple",                                                                      "none",          "",            "solarfield",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "PB_pump_coef",      "Pumping power required to move 1kg of HTF through power block flow",                  "kW/kg",         "",            "solarfield",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "PB_fixed_par",      "fraction of rated gross power consumed at all hours of the year",                     "none",          "",            "solarfield",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_ARRAY,       "bop_array",         "BOP_parVal, BOP_parPF, BOP_par0, BOP_par1, BOP_par2",                                 "-",             "",            "solarfield",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_ARRAY,       "aux_array",         "Aux_parVal, Aux_parPF, Aux_par0, Aux_par1, Aux_par2",                                 "-",             "",            "solarfield",     "*",                       "",                      "" },
  //{ SSC_INPUT,        SSC_NUMBER,      "T_startup",         "Startup temperature (same as field startup)",                                         "C",             "",            "solarfield",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "fossil_mode",       "Operation mode for the fossil backup {1=Normal,2=supp,3=toppin}",                     "none",          "",            "solarfield",     "*",                       "INTEGER",               "" },
    { SSC_INPUT,        SSC_NUMBER,      "I_bn_des",          "Design point irradiation value",                                                      "W/m2",          "",            "solarfield",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "is_sh",             "Does the solar field include a superheating section",                                 "none",          "",            "solarfield",     "*",                       "INTEGER",               "" },
    { SSC_INPUT,        SSC_NUMBER,      "is_oncethru",       "Flag indicating whether flow is once through with superheat",                         "none",          "",            "solarfield",     "*",                       "INTEGER",               "" },
    { SSC_INPUT,        SSC_NUMBER,      "is_multgeom",       "Does the superheater have a different geometry from the boiler {1=yes}",              "none",          "",            "solarfield",     "*",                       "INTEGER",               "" },
    { SSC_INPUT,        SSC_NUMBER,      "nModBoil",          "Number of modules in the boiler section",                                             "none",          "",            "solarfield",     "*",                       "INTEGER",               "" },
    { SSC_INPUT,        SSC_NUMBER,      "nModSH",            "Number of modules in the superheater section",                                        "none",          "",            "solarfield",     "*",                       "INTEGER",               "" },
    { SSC_INPUT,        SSC_NUMBER,      "nLoops",            "Number of loops",                                                                     "none",          "",            "solarfield",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "eta_pump",          "Feedwater pump efficiency",                                                           "none",          "",            "solarfield",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "latitude",          "Site latitude resource page",                                                         "deg",           "",            "solarfield",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "theta_stow",        "stow angle",                                                                          "deg",           "",            "solarfield",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "theta_dep",         "deploy angle",                                                                        "deg",           "",            "solarfield",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "m_dot_min",         "Minimum loop flow rate",                                                              "kg/s",          "",            "solarfield",     "*",                       "",                      "" },
  //{ SSC_INPUT,        SSC_NUMBER,      "T_field_ini",       "Initial field temperature",                                                           "C",             "",            "solarfield",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "T_fp",              "Freeze protection temperature (heat trace activation temperature)",                   "C",             "",            "solarfield",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "Pipe_hl_coef",      "Loss coefficient from the header.. runner pipe.. and non-HCE pipin",                  "W/m2-K",        "",            "solarfield",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "SCA_drives_elec",   "Tracking power.. in Watts per SCA drive",                                             "W/m2",          "",            "solarfield",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "ColAz",             "Collector azimuth angle",                                                             "deg",           "",            "solarfield",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "e_startup",         "Thermal inertia contribution per sq meter of solar field",                            "kJ/K-m2",       "",            "solarfield",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "T_amb_des_sf",      "Design-point ambient temperature",                                                    "C",             "",            "solarfield",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "V_wind_max",        "Maximum allowable wind velocity before safety stow",                                  "m/s",           "",            "solarfield",     "*",                       "",                      "" },
    
	{ SSC_INPUT,        SSC_NUMBER,      "csp.lf.sf.water_per_wash",  "Water usage per wash",                "L/m2_aper",    "",    "heliostat", "*", "", "" },
	{ SSC_INPUT,        SSC_NUMBER,      "csp.lf.sf.washes_per_year", "Mirror washing frequency",            "",             "",    "heliostat", "*", "", "" },
	
	{ SSC_INPUT,        SSC_ARRAY,       "ffrac",             "Fossil dispatch logic - TOU periods",                                                 "none",          "",            "solarfield",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "A_aperture",        "(boiler, SH) Reflective aperture area of the collector module",                       "m^2",           "",            "solarfield",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "L_col",             "(boiler, SH) Active length of the superheater section collector module",              "m",             "",            "solarfield",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "OptCharType",       "(boiler, SH) The optical characterization method",                                    "none",          "",            "solarfield",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "IAM_T",             "(boiler, SH) Transverse Incident angle modifiers (0,1,2,3,4 order terms)",            "none",          "",            "solarfield",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "IAM_L",             "(boiler, SH) Longitudinal Incident angle modifiers (0,1,2,3,4 order terms)",          "none",          "",            "solarfield",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "TrackingError",     "(boiler, SH) User-defined tracking error derate",                                     "none",          "",            "solarfield",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "GeomEffects",       "(boiler, SH) User-defined geometry effects derate",                                   "none",          "",            "solarfield",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "rho_mirror_clean",  "(boiler, SH) User-defined clean mirror reflectivity",                                 "none",          "",            "solarfield",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "dirt_mirror",       "(boiler, SH) User-defined dirt on mirror derate",                                     "none",          "",            "solarfield",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "error",             "(boiler, SH) User-defined general optical error derate",                              "none",          "",            "solarfield",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "HLCharType",        "(boiler, SH) Flag indicating the heat loss model type {1=poly.; 2=Forristall}",       "none",          "",            "solarfield",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "HL_dT",             "(boiler, SH) Heat loss coefficient - HTF temperature (0,1,2,3,4 order terms)",        "W/m-K^order",   "",            "solarfield",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "HL_W",              "(boiler, SH) Heat loss coef adj wind velocity (0,1,2,3,4 order terms)",               "1/(m/s)^order", "",            "solarfield",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "D_2",               "(boiler, SH) The inner absorber tube diameter",                                       "m",             "",            "solarfield",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "D_3",               "(boiler, SH) The outer absorber tube diameter",                                       "m",             "",            "solarfield",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "D_4",               "(boiler, SH) The inner glass envelope diameter",                                      "m",             "",            "solarfield",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "D_5",               "(boiler, SH) The outer glass envelope diameter",                                      "m",             "",            "solarfield",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "D_p",               "(boiler, SH) The diameter of the absorber flow plug (optional)",                      "m",             "",            "solarfield",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "Rough",             "(boiler, SH) Roughness of the internal surface",                                      "m",             "",            "solarfield",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "Flow_type",         "(boiler, SH) The flow type through the absorber",                                     "none",          "",            "solarfield",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "AbsorberMaterial",  "(boiler, SH) Absorber material type",                                                 "none",          "",            "solarfield",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "HCE_FieldFrac",     "(boiler, SH) The fraction of the field occupied by this HCE type (4: # field fracs)", "none",          "",            "solarfield",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "alpha_abs",         "(boiler, SH) Absorber absorptance (4: # field fracs)",                                "none",          "",            "solarfield",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "b_eps_HCE1",        "(temperature) Absorber emittance (eps)",                                              "none",          "",            "solarfield",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "b_eps_HCE2",        "(temperature) Absorber emittance (eps)",                                              "none",          "",            "solarfield",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "b_eps_HCE3",        "(temperature) Absorber emittance (eps)",                                              "none",          "",            "solarfield",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "b_eps_HCE4",        "(temperature) Absorber emittance (eps)",                                              "none",          "",            "solarfield",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "sh_eps_HCE1",       "(temperature) Absorber emittance (eps)",                                              "none",          "",            "solarfield",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "sh_eps_HCE2",       "(temperature) Absorber emittance (eps)",                                              "none",          "",            "solarfield",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "sh_eps_HCE3",       "(temperature) Absorber emittance (eps)",                                              "none",          "",            "solarfield",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "sh_eps_HCE4",       "(temperature) Absorber emittance (eps)",                                              "none",          "",            "solarfield",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "alpha_env",         "(boiler, SH) Envelope absorptance (4: # field fracs)",                                "none",          "",            "solarfield",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "EPSILON_4",         "(boiler, SH) Inner glass envelope emissivities (Pyrex) (4: # field fracs)",           "none",          "",            "solarfield",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "Tau_envelope",      "(boiler, SH) Envelope transmittance (4: # field fracs)",                              "none",          "",            "solarfield",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "GlazingIntactIn",   "(boiler, SH) The glazing intact flag {true=0; false=1} (4: # field fracs)",           "none",          "",            "solarfield",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "AnnulusGas",        "(boiler, SH) Annulus gas type {1=air; 26=Ar; 27=H2} (4: # field fracs)",              "none",          "",            "solarfield",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "P_a",               "(boiler, SH) Annulus gas pressure (4: # field fracs)",                                "torr",          "",            "solarfield",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "Design_loss",       "(boiler, SH) Receiver heat loss at design (4: # field fracs)",                        "W/m",           "",            "solarfield",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "Shadowing",         "(boiler, SH) Receiver bellows shadowing loss factor (4: # field fracs)",              "none",          "",            "solarfield",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "Dirt_HCE",          "(boiler, SH) Loss due to dirt on the receiver envelope (4: # field fracs)",           "none",          "",            "solarfield",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "b_OpticalTable",    "Values of the optical efficiency table",                                              "none",          "",            "solarfield",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "sh_OpticalTable",   "Values of the optical efficiency table",                                              "none",          "",            "solarfield",     "*",                       "",                      "" },

	// Type 261 (solar field collector) initial values
    { SSC_INPUT,        SSC_NUMBER,      "dnifc",             "Forecast DNI",                                                                        "W/m2",          "",            "solarfield",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "I_bn",              "Beam normal radiation (input kJ/m2-hr)",                                              "W/m2",          "",            "solarfield",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "T_db",              "Dry bulb air temperature",                                                            "C",             "",            "solarfield",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "T_dp",              "The dewpoint temperature",                                                            "C",             "",            "solarfield",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "P_amb",             "Ambient pressure",                                                                    "atm",           "",            "solarfield",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "V_wind",            "Ambient windspeed",                                                                   "m/s",           "",            "solarfield",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "m_dot_htf_ref",     "Reference HTF flow rate at design conditions",                                        "kg/hr",         "",            "solarfield",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "m_pb_demand",       "Demand htf flow from the power block",                                                "kg/hr",         "",            "solarfield",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "shift",             "Shift in longitude from local standard meridian",                                     "deg",           "",            "solarfield",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "SolarAz_init",      "Solar azimuth angle",                                                                 "deg",           "",            "solarfield",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "SolarZen",          "Solar zenith angle",                                                                  "deg",           "",            "solarfield",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "T_pb_out_init",     "Fluid temperature from the power block",                                              "C",             "",            "solarfield",     "*",                       "",                      "" },
  //{ SSC_INPUT,        SSC_NUMBER,      "TOUPeriod",         "Time of use period",                                                                  "none",          "",            "solarfield",     "*",                       "",                      "" },

	// Type 234 (powerblock) parameters
  //{ SSC_INPUT,        SSC_NUMBER,      "P_ref",             "Reference output electric power at design condition",                                 "MW",            "",            "powerblock",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "eta_ref",           "Reference conversion efficiency at design condition",                                 "none",          "",            "powerblock",     "*",                       "",                      "" },
  //{ SSC_INPUT,        SSC_NUMBER,      "T_hot_ref",         "Reference HTF inlet temperature at design",                                           "C",             "",            "powerblock",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "T_cold_ref",        "Reference HTF outlet temperature at design",                                          "C",             "",            "powerblock",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "dT_cw_ref",         "Reference condenser cooling water inlet/outlet T diff",                               "C",             "",            "powerblock",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "T_amb_des",         "Reference ambient temperature at design point",                                       "C",             "",            "powerblock",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "q_sby_frac",        "Fraction of thermal power required for standby mode",                                 "none",          "",            "powerblock",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "P_boil_des",        "Boiler operating pressure @ design",                                                  "bar",           "",            "powerblock",     "*",                       "",                      "" },
//    { SSC_INPUT,        SSC_NUMBER,      "is_rh",             "Flag indicating whether reheat is used 0:no, 1:yes",                                  "none",          "",            "powerblock",     "*",                       "INTEGER",               "" },
    { SSC_INPUT,        SSC_NUMBER,      "P_rh_ref",          "Reheater operating pressure at design",                                               "bar",           "",            "powerblock",     "*",                       "",                      "" },
//    { SSC_INPUT,        SSC_NUMBER,      "T_rh_hot_ref",      "Reheater design outlet temperature",                                                  "C",             "",            "powerblock",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "rh_frac_ref",       "Reheater flow fraction at design",                                                    "none",          "",            "powerblock",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "CT",                "Flag for using dry cooling or wet cooling system",                                    "none",          "",            "powerblock",     "*",                       "INTEGER",               "" },
    { SSC_INPUT,        SSC_NUMBER,      "startup_time",      "Time needed for power block startup",                                                 "hr",            "",            "powerblock",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "startup_frac",      "Fraction of design thermal power needed for startup",                                 "none",          "",            "powerblock",     "*",                       "",                      "" },
 //   { SSC_INPUT,        SSC_NUMBER,      "tech_type",         "Flag indicating which coef. set to use. (1=tower,2=trough,3=user)",                   "none",          "",            "powerblock",     "*",                       "INTEGER",               "" },
    { SSC_INPUT,        SSC_NUMBER,      "T_approach",        "Cooling tower approach temperature",                                                  "C",             "",            "powerblock",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "T_ITD_des",         "ITD at design for dry system",                                                        "C",             "",            "powerblock",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "P_cond_ratio",      "Condenser pressure ratio",                                                            "none",          "",            "powerblock",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "pb_bd_frac",        "Power block blowdown steam fraction ",                                                "none",          "",            "powerblock",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "P_cond_min",        "Minimum condenser pressure",                                                          "inHg",          "",            "powerblock",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "n_pl_inc",          "Number of part-load increments for the heat rejection system",                        "none",          "",            "powerblock",     "*",                       "INTEGER",               "" },
    { SSC_INPUT,        SSC_ARRAY,       "F_wc",              "Fraction indicating wet cooling use for hybrid system",                               "none",          "",            "powerblock",     "*",                       "",                      "" },
	// Type 234 (powerblock) inputs
    { SSC_INPUT,        SSC_NUMBER,      "pc_mode",           "Cycle part load control, from plant controller",                                      "none",          "",            "powerblock",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "T_hot",             "Hot HTF inlet temperature, from storage tank",                                        "C",             "",            "powerblock",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "m_dot_st",          "HTF mass flow rate",                                                                  "kg/hr",         "",            "powerblock",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "T_wb",              "Ambient wet bulb temperature",                                                        "C",             "",            "powerblock",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "demand_var",        "Control signal indicating operational mode",                                          "none",          "",            "powerblock",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "standby_control",   "Control signal indicating standby mode",                                              "none",          "",            "powerblock",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "T_db_pwb",          "Ambient dry bulb temperature",                                                        "C",             "",            "powerblock",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "P_amb_pwb",         "Ambient pressure",                                                                    "atm",           "",            "powerblock",     "*",                       "",                      "" },
  //{ SSC_INPUT,        SSC_NUMBER,      "TOU",               "Current Time-of-use period",                                                          "none",          "",            "powerblock",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "relhum",            "Relative humidity of the ambient air",                                                "none",          "",            "powerblock",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "f_recSU",           "Fraction powerblock can run due to receiver startup",                                 "none",          "",            "powerblock",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "dp_b",              "Pressure drop in boiler",                                                             "Pa",            "",            "powerblock",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "dp_sh",             "Pressure drop in superheater",                                                        "Pa",            "",            "powerblock",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "dp_rh",             "Pressure drop in reheater",                                                           "Pa",            "",            "powerblock",     "*",                       "",                      "" },

    // OUTPUTS
	// The names of the output variables should match the parameter names for the TCS units in order to signal to the TCS kernel to store the values by timestep

    // VARTYPE          DATATYPE          NAME                 LABEL                                                                                 UNITS           META            GROUP            REQUIRED_IF                 CONSTRAINTS             UI_HINTS
	// Type 261 (net energy calculator) outputs
	{ SSC_OUTPUT, SSC_ARRAY, "Q_thermal", "Thermal power to HTF", "MWt", "", "CR", "", "", "" },

	{ SSC_OUTPUT, SSC_ARRAY, "gen", "Total electric power to grid w/ avail. derate", "kWe", "", "System", "", "", "" },

	var_info_invalid };

	class cm_iph_linear_fresnel_dsg : public tcKernel
		
{
public:

	cm_iph_linear_fresnel_dsg(tcstypeprovider *prov)
	:tcKernel(prov)
	{
		add_var_info(_cm_vtab_iph_linear_fresnel_dsg);
		add_var_info(vtab_adjustment_factors);
		add_var_info(vtab_technology_outputs);

		//set_store_all_parameters(true); // default is 'false' = only store TCS parameters that match the SSC_OUTPUT variables above
	}

	void exec( )
	{
		//bool debug_mode = (__DEBUG__ == 1);  // When compiled in VS debug mode, this will use the trnsys weather file; otherwise, it will attempt to open the file with name that was passed in
		//
		//debug_mode = false;

		////Add weather file reader unit
		//int weather = 0;
		//if(debug_mode) weather = add_unit("trnsys_weatherreader", "TRNSYS weather reader");
		//else weather = add_unit("weatherreader", "TCS weather reader");

		//if(debug_mode)
		//{
		//	set_unit_value( weather, "file_name", "C:/svn_NREL/main/ssc/tcsdata/typelib/TRNSYS_weather_outputs/tucson_trnsys_weather.out" );
		//	set_unit_value( weather, "i_hour", "TIME" );
		//	set_unit_value( weather, "i_month", "month" );
		//	set_unit_value( weather, "i_day", "day" );
		//	set_unit_value( weather, "i_global", "GlobalHorizontal" );
		//	set_unit_value( weather, "i_beam", "DNI" );
		//	set_unit_value( weather, "i_diff", "DiffuseHorizontal" );
		//	set_unit_value( weather, "i_tdry", "T_dry" );
		//	set_unit_value( weather, "i_twet", "T_wet" );
		//	set_unit_value( weather, "i_tdew", "T_dew" );
		//	set_unit_value( weather, "i_wspd", "WindSpeed" );
		//	set_unit_value( weather, "i_wdir", "WindDir" );
		//	set_unit_value( weather, "i_rhum", "RelHum" );
		//	set_unit_value( weather, "i_pres", "AtmPres" );
		//	set_unit_value( weather, "i_snow", "SnowCover" );
		//	set_unit_value( weather, "i_albedo", "GroundAlbedo" );
		//	set_unit_value( weather, "i_poa", "POA" );
		//	set_unit_value( weather, "i_solazi", "Azimuth" );
		//	set_unit_value( weather, "i_solzen", "Zenith" );
		//	set_unit_value( weather, "i_lat", "Latitude" );
		//	set_unit_value( weather, "i_lon", "Longitude" );
		//	set_unit_value( weather, "i_shift", "Shift" );
		//}
		//else 
		//{
		//	//Set weather parameters
		//	set_unit_value_ssc_string( weather, "file_name" );
		//	set_unit_value_ssc_double( weather, "track_mode" );
		//	set_unit_value_ssc_double( weather, "tilt" );
		//	set_unit_value_ssc_double( weather, "azimuth" );
		//}

		//// Add tou translator
		//int	tou = add_unit("tou_translator", "Time of Use Translator");
		////Add the solar field collector unit
		//int type261_solarfield = add_unit("sam_mw_lf_type261_steam", "type 261 solarfield");
		////Add direct powerblock unit
		//int type234_powerblock = add_unit("sam_mw_type234", "type 234 powerblock");
		////Add unit to that summarizes energy output
		//int type261_summarizer = add_unit("sam_mw_lf_type261_Wnet","type 261 enet calculator");

		//set_unit_value_ssc_matrix(tou, "weekday_schedule"); // tou values from control will be between 1 and 9
		//set_unit_value_ssc_matrix(tou, "weekend_schedule");

		// Now set solar field collector unit parameters
		//set_unit_value_ssc_double( type261_solarfield, "tes_hours"); // TSHOURS );
		//set_unit_value_ssc_double( type261_solarfield, "q_max_aux"); // q_max_aux );
		//set_unit_value_ssc_double( type261_solarfield, "LHV_eff"); // LHV_eff );
		//set_unit_value_ssc_double( type261_solarfield, "T_set_aux", as_double("T_hot"));
		//set_unit_value_ssc_double( type261_solarfield, "T_field_in_des", as_double("T_cold_ref"));
		//set_unit_value_ssc_double( type261_solarfield, "T_field_out_des", as_double("T_hot"));
		//set_unit_value_ssc_double( type261_solarfield, "x_b_des"); // x_b_des );
		//set_unit_value_ssc_double( type261_solarfield, "P_turb_des"); // P_turb_des );
		//set_unit_value_ssc_double( type261_solarfield, "fP_hdr_c"); // fP_hdr_c );
		//set_unit_value_ssc_double( type261_solarfield, "fP_sf_boil"); // fP_sf_boil );
		//set_unit_value_ssc_double( type261_solarfield, "fP_boil_to_sh"); // fP_boil_to_SH );
		//set_unit_value_ssc_double( type261_solarfield, "fP_sf_sh"); // fP_sf_sh );
		//set_unit_value_ssc_double( type261_solarfield, "fP_hdr_h"); // fP_hdr_h );
		//set_unit_value_ssc_double( type261_solarfield, "q_pb_des"); // Q_ref ); // = P_ref/eta_ref;
		//set_unit_value_ssc_double( type261_solarfield, "W_pb_des", as_double("demand_var"));
		//set_unit_value_ssc_double( type261_solarfield, "cycle_max_fraction"); // cycle_max_fraction );
		//set_unit_value_ssc_double( type261_solarfield, "cycle_cutoff_frac"); // cycle_cutoff_frac );
		//set_unit_value_ssc_double( type261_solarfield, "t_sby"); // t_sby );
		//set_unit_value_ssc_double( type261_solarfield, "q_sby_frac"); // q_sby_frac );
		//set_unit_value_ssc_double( type261_solarfield, "solarm"); // solarm );
		//set_unit_value_ssc_double( type261_solarfield, "PB_pump_coef"); // PB_pump_coef );
		//set_unit_value_ssc_double( type261_solarfield, "PB_fixed_par"); // PB_fixed_par );
		//set_unit_value_ssc_array( type261_solarfield, "bop_array"); // [BOP_parVal, BOP_parPF, BOP_par0, BOP_par1, BOP_par2] );
		//set_unit_value_ssc_array( type261_solarfield, "aux_array"); // [Aux_parVal, Aux_parPF, Aux_par0, Aux_par1, Aux_par2] );
		//set_unit_value_ssc_double( type261_solarfield, "T_startup", as_double("T_hot"));
		//set_unit_value_ssc_double( type261_solarfield, "fossil_mode"); // fossil_mode );
		//set_unit_value_ssc_double( type261_solarfield, "I_bn_des"); // I_bn_des );
		//set_unit_value_ssc_double( type261_solarfield, "is_sh"); // is_sh );
		//set_unit_value_ssc_double( type261_solarfield, "is_oncethru"); // is_oncethru );
		//set_unit_value_ssc_double( type261_solarfield, "is_multgeom"); // is_multgeom );
		//set_unit_value_ssc_double( type261_solarfield, "nModBoil"); // nModBoil );
		//set_unit_value_ssc_double( type261_solarfield, "nModSH"); // nModSH );
		//set_unit_value_ssc_double( type261_solarfield, "nLoops"); // nLoops );
		//set_unit_value_ssc_double( type261_solarfield, "eta_pump"); // eta_pump );
		//set_unit_value_ssc_double( type261_solarfield, "latitude"); // latitude );
		//set_unit_value_ssc_double( type261_solarfield, "theta_stow"); // theta_stow );
		//set_unit_value_ssc_double( type261_solarfield, "theta_dep"); // theta_dep );
		//set_unit_value_ssc_double( type261_solarfield, "m_dot_min"); // m_dot_min );
		//set_unit_value_ssc_double( type261_solarfield, "T_field_ini", as_double("T_cold_ref"));
		//set_unit_value_ssc_double( type261_solarfield, "T_fp"); // T_fp );
		//set_unit_value_ssc_double( type261_solarfield, "Pipe_hl_coef"); // Pipe_hl_coef );
		//set_unit_value_ssc_double( type261_solarfield, "SCA_drives_elec"); // SCA_drives_elec );
		//set_unit_value_ssc_double( type261_solarfield, "ColAz"); // ColAz );
		//set_unit_value_ssc_double( type261_solarfield, "e_startup"); // e_startup );
		//set_unit_value_ssc_double( type261_solarfield, "T_amb_des_sf"); // T_amb_des_sf );
		//set_unit_value_ssc_double( type261_solarfield, "V_wind_max"); // V_wind_max );
		//set_unit_value_ssc_array( type261_solarfield, "ffrac"); // [FFRAC_1,FFRAC_2,FFRAC_3,FFRAC_4,FFRAC_5,FFRAC_6,FFRAC_7,FFRAC_8,FFRAC_9] );
		// Set all matrix parameters
		//set_unit_value_ssc_matrix(type261_solarfield,"A_aperture"); //A_aper);
		//set_unit_value_ssc_matrix(type261_solarfield,"L_col"); //L_col);
		//set_unit_value_ssc_matrix(type261_solarfield,"OptCharType"); //OptCharType);
		//set_unit_value_ssc_matrix(type261_solarfield,"IAM_T"); //IAM_T);
		//set_unit_value_ssc_matrix(type261_solarfield,"IAM_L"); //IAM_L);
		//set_unit_value_ssc_matrix(type261_solarfield,"TrackingError"); //TrackingError);
		//set_unit_value_ssc_matrix(type261_solarfield,"GeomEffects"); //GeomEffects);
		//set_unit_value_ssc_matrix(type261_solarfield,"rho_mirror_clean"); //rho_mirror_clean);
		//set_unit_value_ssc_matrix(type261_solarfield,"dirt_mirror"); //dirt_mirror);
		//set_unit_value_ssc_matrix(type261_solarfield,"error"); //error);
		//set_unit_value_ssc_matrix(type261_solarfield,"HLCharType"); //HLCharType);
		//set_unit_value_ssc_matrix(type261_solarfield,"HL_dT"); //HL_dT);
		//set_unit_value_ssc_matrix(type261_solarfield,"HL_W"); //HL_W);
		//set_unit_value_ssc_matrix(type261_solarfield,"D_2"); //D_2);
		//set_unit_value_ssc_matrix(type261_solarfield,"D_3"); //D_3);
		//set_unit_value_ssc_matrix(type261_solarfield,"D_4"); //D_4);
		//set_unit_value_ssc_matrix(type261_solarfield,"D_5"); //D_5);
		//set_unit_value_ssc_matrix(type261_solarfield,"D_p"); //D_p);
		//set_unit_value_ssc_matrix(type261_solarfield,"Rough"); //Rough);
		//set_unit_value_ssc_matrix(type261_solarfield,"Flow_type"); //Flow_type);
		//set_unit_value_ssc_matrix(type261_solarfield,"AbsorberMaterial"); //AbsorberMaterial);
		//set_unit_value_ssc_matrix(type261_solarfield,"HCE_FieldFrac"); //HCE_FieldFrac);
		//set_unit_value_ssc_matrix(type261_solarfield,"alpha_abs"); //alpha_abs);
		//set_unit_value_ssc_matrix(type261_solarfield,"b_eps_HCE1"); //b_eps_HCE1);
		//set_unit_value_ssc_matrix(type261_solarfield,"b_eps_HCE2"); //b_eps_HCE2);
		//set_unit_value_ssc_matrix(type261_solarfield,"b_eps_HCE3"); //b_eps_HCE3);
		//set_unit_value_ssc_matrix(type261_solarfield,"b_eps_HCE4"); //b_eps_HCE4);
		//if( as_integer("is_multgeom") != 0 )
		//{
		//	set_unit_value_ssc_matrix(type261_solarfield,"sh_eps_HCE1"); //s_eps_HCE1);
		//	set_unit_value_ssc_matrix(type261_solarfield,"sh_eps_HCE2"); //s_eps_HCE2);
		//	set_unit_value_ssc_matrix(type261_solarfield,"sh_eps_HCE3"); //s_eps_HCE3);
		//	set_unit_value_ssc_matrix(type261_solarfield,"sh_eps_HCE4"); //s_eps_HCE4);
		//}
		//set_unit_value_ssc_matrix(type261_solarfield,"alpha_env"); //alpha_env);
		//set_unit_value_ssc_matrix(type261_solarfield,"EPSILON_4"); //EPSILON_4);
		//set_unit_value_ssc_matrix(type261_solarfield,"Tau_envelope"); //Tau_envelope);
		//set_unit_value_ssc_matrix(type261_solarfield,"GlazingIntactIn"); //GlazingIntactIn);
		//set_unit_value_ssc_matrix(type261_solarfield,"AnnulusGas"); //AnnulusGas);
		//set_unit_value_ssc_matrix(type261_solarfield,"P_a"); //P_a);
		//set_unit_value_ssc_matrix(type261_solarfield,"Design_loss"); //Design_loss);
		//set_unit_value_ssc_matrix(type261_solarfield,"Shadowing"); //Shadowing);
		//set_unit_value_ssc_matrix(type261_solarfield,"Dirt_HCE"); //Dirt_HCE);
		//set_unit_value_ssc_matrix(type261_solarfield, "b_OpticalTable"); // opt_data);
		//set_unit_value_ssc_matrix(type261_solarfield, "sh_OpticalTable"); // opt_data);

		//// Type 261 (solar field collector) inputs
		//set_unit_value_ssc_double(type261_solarfield, "dnifc"); // , 0.0);				//[W/m2] - not used
		//set_unit_value_ssc_double(type261_solarfield, "I_bn"); // 0.0);			    //[W/m2] - initial value
		//set_unit_value_ssc_double(type261_solarfield, "T_db"); // 15.0);			//[C] - initial value
		//set_unit_value_ssc_double(type261_solarfield, "T_dp"); // 10.0);			//[C] - connect to dew point
		//set_unit_value_ssc_double(type261_solarfield, "P_amb"); // 930.50);			//[mbar] - initial value
		//set_unit_value_ssc_double(type261_solarfield, "V_wind"); // 0.0);			//[m/s] - initial value
		//set_unit_value_ssc_double(type261_solarfield, "m_dot_htf_ref"); // 0.0);	//[kg/hr] - initial value
		//set_unit_value_ssc_double(type261_solarfield, "m_pb_demand"); // 0.0);			//[kg/hr] - not used
		//set_unit_value_ssc_double(type261_solarfield, "shift"); // 0.0);			//[deg] - initial value
		//set_unit_value_ssc_double(type261_solarfield, "SolarAz", as_double("SolarAz_init")); // 0.0);			//[deg] - initial value
		//set_unit_value_ssc_double(type261_solarfield, "SolarZen"); // 0.0);			//[deg] - initial value
		//set_unit_value_ssc_double(type261_solarfield, "T_pb_out", as_double("T_pb_out_init")); // 290.0);			//[C] - initial value
		////set_unit_value_ssc_double(type261_solarfield, "TOUPeriod"); // 4);				//[-] - don't have TOU reader yet - all are same in default LF model though

		// connect solar field
		//bool bConnected = connect(weather, "beam", type261_solarfield, "I_bn");		//[W/m2] - connect to weather reader
		//bConnected &= connect(weather, "tdry", type261_solarfield, "T_db");		//[C] - connect to weather reader
		//bConnected &= connect(weather, "tdew", type261_solarfield, "T_dp");		//[C] - connect to weather reader
		//bConnected &= connect(weather, "pres", type261_solarfield, "P_amb");		//[mbar] - connect to weather reader
		//bConnected &= connect(weather, "wspd", type261_solarfield, "V_wind");		//[m/s] - connect to weather reader
		//bConnected &= connect(type234_powerblock, "m_dot_ref", type261_solarfield, "m_dot_htf_ref");	//[kg/hr] connect to powerblock
		//bConnected &= connect(weather, "shift", type261_solarfield, "shift");		//[deg] - connect to weather reader
		//bConnected &= connect(weather, "solazi", type261_solarfield, "SolarAz");	//[deg] - connect to weather reader
		//bConnected &= connect(weather, "solzen", type261_solarfield, "SolarZen"); //[deg] - connect to weather reader
		//bConnected &= connect(type234_powerblock, "T_cold", type261_solarfield, "T_pb_out");	//[C] - connect to powerblock
		//bConnected &= connect(tou, "tou_value", type261_solarfield, "TOUPeriod");

//		// Set Parameters for Direct Powerblock (type 234)
//		set_unit_value_ssc_double(type234_powerblock, "P_ref", as_double("demand_var"));
//		set_unit_value_ssc_double(type234_powerblock, "eta_ref"); // eta_ref);
//		set_unit_value_ssc_double(type234_powerblock, "T_hot_ref", as_double("T_hot"));
//		set_unit_value_ssc_double(type234_powerblock, "T_cold_ref"); // T_cold_ref);
//		set_unit_value_ssc_double(type234_powerblock, "dT_cw_ref"); // dT_cw_ref);
//		set_unit_value_ssc_double(type234_powerblock, "T_amb_des"); // T_amb_des);
//		set_unit_value_ssc_double(type234_powerblock, "q_sby_frac"); // q_sby_frac);
//		set_unit_value_ssc_double(type234_powerblock, "P_boil_des"); // P_boil_ref);
//		//set_unit_value_ssc_double(type234_powerblock, "is_rh"); // is_rh);
//		set_unit_value_ssc_double(type234_powerblock, "is_rh", 0.0); // is_rh);
//		set_unit_value_ssc_double(type234_powerblock, "P_rh_ref"); // P_rh_ref);
//		//set_unit_value_ssc_double(type234_powerblock, "T_rh_hot_ref"); // T_rh_hot_ref);
//		set_unit_value_ssc_double(type234_powerblock, "T_rh_hot_ref", 0.0);
//
//
//
//		set_unit_value_ssc_double(type234_powerblock, "rh_frac_ref"); // rh_frac_ref);
//		set_unit_value_ssc_double(type234_powerblock, "CT"); // CT);
//		set_unit_value_ssc_double(type234_powerblock, "startup_time"); // startup_time);
//		set_unit_value_ssc_double(type234_powerblock, "startup_frac"); // startup_frac);
////		set_unit_value_ssc_double(type234_powerblock, "tech_type"); // tech_type);
//		set_unit_value_ssc_double(type234_powerblock, "tech_type", 3); // tech_type);
//		set_unit_value_ssc_double(type234_powerblock, "T_approach"); // T_approach);
//		set_unit_value_ssc_double(type234_powerblock, "T_ITD_des"); // T_ITD_des);
//		set_unit_value_ssc_double(type234_powerblock, "P_cond_ratio"); // P_cond_ratio);
//		set_unit_value_ssc_double(type234_powerblock, "pb_bd_frac"); // pb_bd_frac);
//		set_unit_value_ssc_double(type234_powerblock, "P_cond_min"); // P_cond_min);
//		set_unit_value_ssc_double(type234_powerblock, "n_pl_inc"); // n_pl_inc);
//		set_unit_value_ssc_array(type234_powerblock, "F_wc"); // [F_wc_1, F_wc_2, F_wc_3, F_wc_4, F_wc_5, F_wc_6, F_wc_7, F_wc_8, F_wc_9]);
//
//
//		// Set Inputs for Powerblock (type 234)
//		set_unit_value_ssc_double(type234_powerblock, "mode", as_double("pc_mode")); // 1);	    //[-] initial value
//		set_unit_value_ssc_double(type234_powerblock, "T_hot"); // T_hot_ref);					//[C] initial value
//		set_unit_value_ssc_double(type234_powerblock, "m_dot_st"); // 0);						//[kg/hr] initial value
//		set_unit_value_ssc_double(type234_powerblock, "T_wb"); // 12.8);						//[C] Initial value
//		set_unit_value_ssc_double(type234_powerblock, "demand_var");							//[kg/hr]
//		set_unit_value_ssc_double(type234_powerblock, "standby_control"); // 0);
//		set_unit_value_ssc_double(type234_powerblock, "T_db", as_double("T_db_pwb") ); // 12.8);
//		set_unit_value_ssc_double(type234_powerblock, "P_amb", as_double("P_amb_pwb") ); // 960);
//		//set_unit_value_ssc_double(type234_powerblock, "TOU"); // 4);								//[-] No TOU reader yet
//		set_unit_value_ssc_double(type234_powerblock, "relhum"); // 0.25);						//[-] Initial value
//		set_unit_value_ssc_double(type234_powerblock, "f_recSU"); // 1);							//[-] Set to 1 for LF
//		set_unit_value_ssc_double(type234_powerblock, "dp_sh"); // 5.0);
//		set_unit_value_ssc_double(type234_powerblock,"dp_rh"); // 0.0);								//[Pa] no rh in LF

		//// connect the powerblock
		//bConnected &= connect(type261_solarfield, "cycle_pl_control", type234_powerblock, "mode");	//[-] connect to LF solarfield
		//bConnected &= connect(type261_solarfield, "T_field_out", type234_powerblock, "T_hot");		//[C] connect to LF solarfield
		//bConnected &= connect(type261_solarfield, "m_dot_to_pb", type234_powerblock, "m_dot_st");		//[kg/hr] connect to LF solarfield
		//bConnected &= connect(weather, "twet", type234_powerblock, "T_wb");					//[C] connect to weather reader
		//bConnected &= connect(type261_solarfield, "m_dot_to_pb", type234_powerblock, "demand_var");	//[kg/hr] 
		//bConnected &= connect(type261_solarfield, "standby_control", type234_powerblock, "standby_control");
		//bConnected &= connect(weather, "tdry", type234_powerblock, "T_db");
		//bConnected &= connect(weather, "pres", type234_powerblock, "P_amb");
		//bConnected &= connect(weather, "rhum", type234_powerblock, "relhum");					//[-] connect to weather reader
		//bConnected &= connect(type261_solarfield, "dP_sf_sh", type234_powerblock, "dp_sh");			//[Pa] Pressure drop in sh
		//bConnected &= connect(tou, "tou_value", type234_powerblock, "TOU");

		//// connect the net energy output calculator
		//bConnected &= connect(type234_powerblock, "P_cycle", type261_summarizer, "W_cycle_gross");
		//bConnected &= connect(type261_solarfield, "W_dot_par_tot", type261_summarizer, "W_par_sf_tot");
		//bConnected &= connect(type234_powerblock, "W_cool_par", type261_summarizer, "W_par_cooling");



		//// check if all connections worked
		//if ( !bConnected )
		//	throw exec_error( "tcslinear_fresnel", util::format("there was a problem connecting outputs of one unit to inputs of another for the simulation.") );

		//// Run simulation
		//size_t hours = 8760;
		//if (0 > simulate(3600, hours*3600, 3600) )
		//	throw exec_error( "tcslinear_fresnel", util::format("there was a problem simulating in the TCS linear fresnel model.") );

		//// get the outputs
		//if (!set_all_output_arrays() )
		//	throw exec_error( "tcslinear_fresnel", util::format("there was a problem returning the results from the simulation.") );

		//// performance adjustement factors
		//adjustment_factors haf(this);
		//if (!haf.setup())
		//	throw exec_error("tcstrough_physical", "failed to setup adjustment factors: " + haf.error());
	
		////1.7.15, twn: Need to calculated the conversion factor before the performance adjustments are applied to "hourly energy"
		//accumulate_annual("W_net", "annual_energy");				// MWh
		//accumulate_annual("W_cycle_gross", "annual_W_cycle_gross");		// MWh
		//// Calculated outputs
		//ssc_number_t ae = as_number("annual_energy");
		//ssc_number_t pg = as_number("annual_W_cycle_gross");
		//ssc_number_t convfactor = (pg != 0) ? 100 * ae / pg : 0;
		//assign("conversion_factor", convfactor);

		//
		//ssc_number_t *p_hourly_energy = allocate("gen", 8760);
		//// set hourly energy = tcs output Enet
		//size_t count;
		//ssc_number_t *hourly_energy = as_array("W_net", &count);//MWh
		//if (count != 8760)
		//	throw exec_error("tcslinear_fresnel", "gen count incorrect (should be 8760): " + count);

		//// apply performance adjustments and convert from MWh to kWh
		//for (size_t i = 0; i < count; i++)
		//{
		//	p_hourly_energy[i] = hourly_energy[i] * (ssc_number_t)(haf(i)*1000.0);
		//}

		//accumulate_annual("gen", "annual_energy"); // already in kWh
		//accumulate_monthly("gen", "monthly_energy"); // already in kWh

		//
		//// First, sum power cycle water consumption timeseries outputs
		//accumulate_annual_for_year("m_dot_makeup", "annual_total_water_use", 1.0 / 1000.0, 1);	//[m^3], convert from kg
		//// Then, add water usage from mirror cleaning
		//ssc_number_t V_water_cycle = as_number("annual_total_water_use");
		//double A_aper_tot = get_unit_value_number(type261_solarfield, "A_aper_tot");
		//double V_water_mirrors = as_double("csp.lf.sf.water_per_wash") / 1000.0*A_aper_tot*as_double("csp.lf.sf.washes_per_year");
		//assign("annual_total_water_use", V_water_cycle + V_water_mirrors);


		//double fuel_usage_mmbtu = 0;
		//ssc_number_t *hourly_fuel = as_array("q_aux_fuel", &count);//MWh
		//if (count != 8760)
		//	throw exec_error("tcslinear_fresnel", "q_aux_fuel count incorrect (should be 8760): " + count);
		//for (size_t i = 0; i < count; i++)
		//	fuel_usage_mmbtu += hourly_fuel[i];
		//assign("system_heat_rate", 3.413); // samsim tcstrough_physical
		//// www.unitjuggler.com/convert-energy-from-MMBtu-to-kWh.html
		//assign("annual_fuel_usage", var_data((ssc_number_t)(fuel_usage_mmbtu * 293.297)));

		//double kWhperkW = 0.0;
		//double nameplate = as_double("system_capacity");
		//double annual_energy = 0.0;
		//for (int i = 0; i < 8760; i++)
		//	annual_energy += p_hourly_energy[i];
		//if (nameplate > 0) kWhperkW = annual_energy / nameplate;
		//assign("capacity_factor", var_data((ssc_number_t)(kWhperkW / 87.6)));
		//assign("kwh_per_kw", var_data((ssc_number_t)kWhperkW));


		//Starting new code
		C_csp_lf_dsg_collector_receiver c_lf_dsg;
		
		// Now set solar field collector unit parameters
		c_lf_dsg.m_tes_hours = as_double("tes_hours");  // TSHOURS );
		c_lf_dsg.m_q_max_aux = as_double("q_max_aux")*1.0E3; // q_max_aux );
		c_lf_dsg.m_LHV_eff = as_double("LHV_eff"); // LHV_eff );
		c_lf_dsg.m_T_set_aux = as_double("T_hot") + 273.15;
		c_lf_dsg.m_T_field_in_des = as_double("T_cold_ref") +273.15;
		c_lf_dsg.m_T_field_out_des = as_double("T_hot") + 273.15;
		c_lf_dsg.m_x_b_des = as_double("x_b_des"); // x_b_des );
		c_lf_dsg.m_P_turb_des = as_double("P_turb_des"); // P_turb_des );
		c_lf_dsg.m_fP_hdr_c = as_double("fP_hdr_c"); // fP_hdr_c );
		c_lf_dsg.m_fP_sf_boil = as_double("fP_sf_boil"); // fP_sf_boil );
		c_lf_dsg.m_fP_boil_to_sh = as_double("fP_boil_to_sh"); // fP_boil_to_SH );
		c_lf_dsg.m_fP_sf_sh = as_double("fP_sf_sh"); // fP_sf_sh );
		c_lf_dsg.m_fP_hdr_h = as_double("fP_hdr_h"); // fP_hdr_h );
		c_lf_dsg.m_q_pb_des = as_double("q_pb_des")*1000.0; // Q_ref ); // = P_ref/eta_ref;
		c_lf_dsg.m_W_pb_des = as_double("demand_var")*1000.0;
		c_lf_dsg.m_cycle_max_fraction = as_double("cycle_max_fraction"); // cycle_max_fraction );
		c_lf_dsg.m_cycle_cutoff_frac = as_double("cycle_cutoff_frac"); // cycle_cutoff_frac );
		c_lf_dsg.m_t_sby_des = as_double("t_sby"); // t_sby );
		c_lf_dsg.m_q_sby_frac = as_double("q_sby_frac"); // q_sby_frac );
		c_lf_dsg.m_solarm = as_double("solarm"); // solarm );
		c_lf_dsg.m_PB_pump_coef = as_double("PB_pump_coef"); // PB_pump_coef );
		c_lf_dsg.m_PB_fixed_par = as_double("PB_fixed_par"); // PB_fixed_par );	c_lf_dsg.q_max_aux = as_double("T_startup", as_double("T_hot"));
		c_lf_dsg.m_fossil_mode = as_double("fossil_mode"); // fossil_mode );
		c_lf_dsg.m_I_bn_des = as_double("I_bn_des"); // I_bn_des );
		//c_lf_dsg.m_is_sh = (bool) as_double("is_sh"); // is_sh ); ? bool
		c_lf_dsg.m_is_sh = 0; // remove superheaters
		c_lf_dsg.m_is_oncethru = (bool) as_double("is_oncethru"); // is_oncethru ); ? bool
		c_lf_dsg.m_is_multgeom = (bool) as_double("is_multgeom"); // is_multgeom ); 
		//c_lf_dsg.m_is_multgeom = 0; // is_multgeom ); to remove superheater
		c_lf_dsg.m_nModBoil = as_integer("nModBoil"); // nModBoil );
		c_lf_dsg.m_nModSH = as_integer("nModSH"); // nModSH );
		c_lf_dsg.m_nLoops = as_integer("nLoops"); // nLoops );
		c_lf_dsg.m_eta_pump = as_double("eta_pump"); // eta_pump );
		c_lf_dsg.m_latitude = as_double("latitude")*0.0174533; // latitude );
		c_lf_dsg.m_theta_stow = as_double("theta_stow")*0.0174533; // theta_stow );
		c_lf_dsg.m_theta_dep = as_double("theta_dep")*0.0174533; // theta_dep );
		c_lf_dsg.m_m_dot_min = as_double("m_dot_min"); // m_dot_min );
		c_lf_dsg.m_T_field_ini = as_double("T_cold_ref") +275.15;
		c_lf_dsg.m_T_fp = as_double("T_fp") + 273.15; // T_fp );
		c_lf_dsg.m_Pipe_hl_coef = as_double("Pipe_hl_coef"); // Pipe_hl_coef );
		c_lf_dsg.m_SCA_drives_elec = as_double("SCA_drives_elec"); // SCA_drives_elec );
		c_lf_dsg.m_ColAz = as_double("ColAz")*0.0174533; // ColAz );
		c_lf_dsg.m_e_startup = as_double("e_startup"); // e_startup );
		c_lf_dsg.m_T_amb_des_sf = as_double("T_amb_des_sf") +273.15; // T_amb_des_sf );
		c_lf_dsg.m_V_wind_max = as_double("V_wind_max"); // V_wind_max );		
		
		//set_unit_value_ssc_array(type261_solarfield, "bop_array"); // [BOP_parVal, BOP_parPF, BOP_par0, BOP_par1, BOP_par2] );
		size_t nval_bop_array = -1;
		ssc_number_t *bop_array = as_array("bop_array", &nval_bop_array);
		c_lf_dsg.m_bop_array.resize(nval_bop_array);
		for (int i = 0; i < nval_bop_array; i++)
			c_lf_dsg.m_bop_array[i] = (double)bop_array[i];

		//set_unit_value_ssc_array(type261_solarfield, "aux_array"); // [Aux_parVal, Aux_parPF, Aux_par0, Aux_par1, Aux_par2] );
		size_t nval_aux_array = -1;
		ssc_number_t *aux_array = as_array("aux_array", &nval_aux_array);
		c_lf_dsg.m_aux_array.resize(nval_aux_array);
		for (int i = 0; i < nval_aux_array; i++)
			c_lf_dsg.m_aux_array[i] = (double)aux_array[i];

		//set_unit_value_ssc_array(type261_solarfield, "ffrac"); // [FFRAC_1,FFRAC_2,FFRAC_3,FFRAC_4,FFRAC_5,FFRAC_6,FFRAC_7,FFRAC_8,FFRAC_9] );
		size_t nval_ffrac = -1;
		ssc_number_t *ffrac = as_array("ffrac", &nval_ffrac);
		c_lf_dsg.m_ffrac.resize(nval_ffrac);
		for (int i = 0; i < nval_ffrac; i++)
			c_lf_dsg.m_ffrac[i] = (double)ffrac[i];

		// Set all matrix parameters
		double m_n_rows_matrix = 1;
		if (c_lf_dsg.m_is_multgeom)
			 m_n_rows_matrix = 2;
		c_lf_dsg.m_n_rows_matrix = m_n_rows_matrix;
		c_lf_dsg.m_A_aperture = as_matrix("A_aperture"); //A_aper);
		c_lf_dsg.m_L_col = as_matrix("L_col"); //L_col);
		c_lf_dsg.m_OptCharType = as_matrix("OptCharType"); //OptCharType);
		c_lf_dsg.m_IAM_T = as_matrix("IAM_T"); //IAM_T);
		c_lf_dsg.m_IAM_L = as_matrix("IAM_L"); //IAM_L);
		c_lf_dsg.m_TrackingError = as_matrix("TrackingError"); //TrackingError);
		c_lf_dsg.m_GeomEffects = as_matrix("GeomEffects"); //GeomEffects);
		c_lf_dsg.m_rho_mirror_clean = as_matrix("rho_mirror_clean"); //rho_mirror_clean);
		c_lf_dsg.m_dirt_mirror = as_matrix("dirt_mirror"); //dirt_mirror);
		c_lf_dsg.m_error = as_matrix("error"); //error);
		c_lf_dsg.m_HLCharType = as_matrix("HLCharType"); //HLCharType);
		c_lf_dsg.m_HL_dT = as_matrix("HL_dT"); //HL_dT);
		c_lf_dsg.m_HL_W = as_matrix("HL_W"); //HL_W);
		c_lf_dsg.m_D_2 = as_matrix("D_2"); //D_2);
		c_lf_dsg.m_D_3 = as_matrix("D_3"); //D_3);
		c_lf_dsg.m_D_4 = as_matrix("D_4"); //D_4);
		c_lf_dsg.m_D_5 = as_matrix("D_5"); //D_5);
		c_lf_dsg.m_D_p = as_matrix("D_p"); //D_p);
		c_lf_dsg.m_Rough = as_matrix("Rough"); //Rough);
		c_lf_dsg.m_Flow_type = as_matrix("Flow_type"); //Flow_type);
		c_lf_dsg.m_AbsorberMaterial_in = as_matrix("AbsorberMaterial");
		c_lf_dsg.m_HCE_FieldFrac = as_matrix("HCE_FieldFrac"); //HCE_FieldFrac);
		c_lf_dsg.m_alpha_abs = as_matrix("alpha_abs"); //alpha_abs);
		c_lf_dsg.m_b_eps_HCE1 = as_matrix("b_eps_HCE1"); //b_eps_HCE1);
		c_lf_dsg.m_b_eps_HCE2 = as_matrix("b_eps_HCE2"); //b_eps_HCE2);
		c_lf_dsg.m_b_eps_HCE3 = as_matrix("b_eps_HCE3"); //b_eps_HCE3);
		c_lf_dsg.m_b_eps_HCE4 = as_matrix("b_eps_HCE4"); //b_eps_HCE4);
		if (c_lf_dsg.m_is_multgeom != 0)
		{
			c_lf_dsg.m_sh_eps_HCE1 = as_matrix("sh_eps_HCE1"); //s_eps_HCE1);
			c_lf_dsg.m_sh_eps_HCE2 = as_matrix("sh_eps_HCE2"); //s_eps_HCE2);
			c_lf_dsg.m_sh_eps_HCE3 = as_matrix("sh_eps_HCE3"); //s_eps_HCE3);
			c_lf_dsg.m_sh_eps_HCE4 = as_matrix("sh_eps_HCE4"); //s_eps_HCE4);
		}
		c_lf_dsg.m_alpha_env = as_matrix("alpha_env"); //alpha_env); [-] Envelope absorptance
		c_lf_dsg.m_EPSILON_4 = as_matrix("EPSILON_4"); //EPSILON_4); [-] Inner glass envelope emissivities (Pyrex)
		c_lf_dsg.m_Tau_envelope = as_matrix("Tau_envelope"); //Tau_envelope); [-] Envelope transmittance
		c_lf_dsg.m_GlazingIntactIn = (bool) as_matrix("GlazingIntactIn"); //GlazingIntactIn); [-] Is the glazing intact?
		c_lf_dsg.m_AnnulusGas_in = as_matrix("AnnulusGas"); //AnnulusGas);
		c_lf_dsg.m_P_a = as_matrix("P_a"); //P_a); [torr] Annulus gas pressure 
		c_lf_dsg.m_Design_loss = as_matrix("Design_loss"); //Design_loss); [W/m] Receiver heat loss at design
		c_lf_dsg.m_Shadowing = as_matrix("Shadowing"); //Shadowing); [-] Receiver bellows shadowing loss factor
		c_lf_dsg.m_Dirt_HCE = as_matrix("Dirt_HCE"); //Dirt_HCE); [-] Loss due to dirt on the receiver envelope
		c_lf_dsg.m_b_OpticalTable = as_matrix("b_OpticalTable"); // opt_data); [-] Boiler Optical Table
		c_lf_dsg.m_sh_OpticalTable = as_matrix("sh_OpticalTable"); // opt_data);

		//// Type 261 (solar field collector) inputs
		//c_lf_dsg.m_dnifc = as_double("dnifc"); // , 0.0);				//[W/m2] - not used
		//c_lf_dsg.m_I_bn = as_double("I_bn"); // 0.0);			    //[W/m2] - initial value
		//c_lf_dsg.m_T_db = as_double("T_db"); // 15.0);			//[C] - initial value
		//c_lf_dsg.m_T_dp = as_double("T_dp"); // 10.0);			//[C] - connect to dew point
		//c_lf_dsg.m_P_amb = as_double("P_amb"); // 930.50);			//[mbar] - initial value
		//c_lf_dsg.m_V_wind = as_double("V_wind"); // 0.0);			//[m/s] - initial value
		//c_lf_dsg.m_V_wind = as_double("m_dot_htf_ref"); // 0.0);	//[kg/hr] - initial value
		//c_lf_dsg.m_m_pb_demand = as_double("m_pb_demand"); // 0.0);			//[kg/hr] - not used
		//c_lf_dsg.m_shift = as_double("shift"); // 0.0);			//[deg] - initial value 
		//c_lf_dsg.m_SolarAz =  as_double("SolarAz_init"); // 0.0);			//[deg] - initial value why no conversation on this solar angle?
		//c_lf_dsg.m_SolarZen = as_double("SolarZen"); // 0.0);			//[deg] - initial value
		//c_lf_dsg.m_T_pb_out = as_double("T_pb_out_init"); // 290.0);			//[C] - initial value
		
		//updated parameters for saturated steam generation
		c_lf_dsg.m_is_multgeom = false; // single geometry  is used
		c_lf_dsg.m_is_sh = false; // no superheater

		// ********************************
		// ********************************
		// Now add the Heat Sink as a power cycle class
		// ********************************
		// ********************************
		// Heat Sink
		C_pc_heat_sink heat_sink;
		heat_sink.ms_params.m_T_htf_hot_des = as_double("T_loop_out");		//[C] FIELD design outlet temperature
		heat_sink.ms_params.m_T_htf_cold_des = as_double("T_field_in");	//[C] FIELD design inlet temperature
		//heat_sink.ms_params.m_x_b_des = as_double("x_b_des");	//[C] FIELD design outlet steam quality
		//heat_sink.ms_params.m_P_Loop_in = as_double("P_turb_des");	//[C] FIELD design inlet water pressure
		//heat_sink.ms_params.m_P_Loop_out = as_double("P_turb_des") - as_double("fP_hdr_c") - as_double("fP_sf_boil");	//[C] FIELD design outlet steam pressure
		heat_sink.ms_params.m_q_dot_des = as_double("q_pb_des");	//[MWt] FIELD design thermal power
		heat_sink.ms_params.m_htf_pump_coef = as_double("PB_pump_coef");	//[kWe/kg/s]

		//heat_sink.ms_params.m_pc_fl = as_integer("Fluid");
		//heat_sink.ms_params.m_pc_fl_props = as_matrix("field_fl_props");

		// ********************************
		// ********************************
		// Now add the TOU class
		// ********************************
		// ********************************
		C_csp_tou_block_schedules tou;
		tou.setup_block_uniform_tod();
		tou.mc_dispatch_params.m_dispatch_optimize = false;

		// System parameters
		C_csp_solver::S_csp_system_params system;
		system.m_pb_fixed_par = as_double("pb_fixed_par");
		system.m_bop_par = 0.0;
		system.m_bop_par_f = 0.0;
		system.m_bop_par_0 = 0.0;
		system.m_bop_par_1 = 0.0;
		system.m_bop_par_2 = 0.0;

		// ********************************
		// ********************************
		// Now add the storage class
		// ********************************
		// ********************************
		C_csp_two_tank_tes storage;
		C_csp_two_tank_tes::S_params *tes = &storage.ms_params;

		// Weather reader
		C_csp_weatherreader weather_reader;
		weather_reader.m_filename = as_string("file_name");
		weather_reader.m_trackmode = 0;
		weather_reader.m_tilt = 0.0;
		weather_reader.m_azimuth = 0.0;
		weather_reader.init();

		// connect variables to operating modes
		c_lf_dsg.m_nSCA = c_lf_dsg.m_nModBoil;
		c_lf_dsg.m_m_dot_htfmin = c_lf_dsg.m_m_dot_min;
		c_lf_dsg.m_m_dot_htfmax = c_lf_dsg.m_m_dot_max;
		c_lf_dsg.m_P_out_des = c_lf_dsg.m_P_turb_des;

		// Instantiate Solver
		C_csp_solver csp_solver(weather_reader, c_lf_dsg, heat_sink, storage, tou, system);


		// For test only start
		//int out_type = -1;
		//std::string out_msg = "";

		//try
		//{
		//	c_lf_dsg.init();
		//}
		//catch (C_csp_exception &csp_exception)
		//{
		//	// Report warning before exiting with error
		//	while (c_lf_dsg.mc_csp_messages.get_message(&out_type, &out_msg))
		//	{
		//		log(out_msg);
		//	}

		//	log(csp_exception.m_error_message, SSC_ERROR, -1.0);

		//	return;
		//}

		//C_csp_weatherreader::S_outputs weather;
		//weather.m_beam = 1006;	//[W/m2]
		//weather.m_tdry = 15.0;	//[C]
		//weather.m_wspd = 4.6;	//[m/s]
		//weather.m_pres = 930; //[mbar]
		//weather.m_tdew = -5.0;	//[C]
		//weather.m_solazi = 164.17468078219139;	//[deg]
		//weather.m_lat = 32.116667000000000;	//[deg]
		//weather.m_lon = -110.93333300000000;	//[deg]
		//weather.m_shift = -5.9333330000000046;

		//C_csp_solver_htf_state htf_state;
		//htf_state.m_temp_in = 293.19332198917641;	//[C]
		//htf_state.m_m_dot = 0.00000000000000000;		//[kg/s]

		//C_csp_collector_receiver::S_csp_cr_inputs inputs;
		//inputs.m_field_control = 1.0;	//[-] Defocus from plant controller

		//C_csp_collector_receiver::S_csp_cr_outputs outputs;

		//C_csp_solver_sim_info sim_info;
		//sim_info.m_step = 3600.0;		//[s] timestep
		//sim_info.m_time = 12.0*3600.0;	//[s] time of year
		//sim_info.m_tou = 4;

		//try
		//{
		//	c_lf_dsg.call(weather,
		//		htf_state,
		//		inputs,
		//		outputs,
		//		sim_info);
		//}
		//catch (C_csp_exception &csp_exception)
		//{
		//	// Report warning before exiting with error
		//	while (c_lf_dsg.mc_csp_messages.get_message(&out_type, &out_msg))
		//	{
		//		log(out_msg);
		//	}

		//	log(csp_exception.m_error_message, SSC_ERROR, -1.0);

		//	return;
		//}
		// For test only end

	}

};

DEFINE_TCS_MODULE_ENTRY(iph_linear_fresnel_dsg, "CSP model using the linear fresnel TCS types.", 4)
