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

// Trough CSP - physical model
#include "core.h"
//#include "tckernel.h"

// for adjustment factors
#include "common.h"

//#include "lib_weatherfile.h
//#include "csp_solver_util.h"
#include "csp_solver_core.h"
#include "csp_solver_trough_collector_receiver.h"
#include "csp_solver_pc_Rankine_indirect_224.h"
#include "csp_solver_two_tank_tes.h"
#include "csp_solver_tou_block_schedules.h"

#include <ctime>

// signed/unsigned mismatch
#pragma warning (disable : 4388)

static var_info _cm_vtab_trough_physical[] = {
    /* VARTYPE          DATATYPE         NAME                         LABEL                                                                               UNITS           META              GROUP             REQUIRED_IF                CONSTRAINTS         UI_HINTS*/
    // Weather Reader
    { SSC_INPUT,        SSC_STRING,      "file_name",                 "Local weather file with path",                                                     "none",         "",               "weather",        "*",                       "LOCAL_FILE",            "" },
    //{ SSC_INPUT,        SSC_NUMBER,      "track_mode",                "Tracking mode",                                                                    "none",         "",               "weather",        "*",                       "",                      "" },

    // Solar Field, Trough
    { SSC_INPUT,        SSC_NUMBER,      "nSCA",                      "Number of SCAs in a loop",                                                         "none",         "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "nHCEt",                     "Number of HCE types",                                                              "none",         "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "nColt",                     "Number of collector types",                                                        "none",         "constant=4",     "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "nHCEVar",                   "Number of HCE variants per type",                                                  "none",         "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "nLoops",                    "Number of loops in the field",                                                     "none",         "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "FieldConfig",               "Number of subfield headers",                                                       "none",         "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "include_fixed_power_block_runner", "Should model consider piping through power block?",                         "none",         "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "L_power_block_piping",      "Length of piping (full mass flow) through heat sink (if applicable)",              "none",         "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "eta_pump",                  "HTF pump efficiency",                                                              "none",         "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "Fluid",                     "Field HTF fluid ID number",                                                        "none",         "",               "solar_field",    "*",                       "",                      "" },
    //{ SSC_INPUT,        SSC_NUMBER,      "fthrok",                    "Flag to allow partial defocusing of the collectors",                               "W/SCA",        "",               "solar_field",    "*",                       "INTEGER",               "" },
    //{ SSC_INPUT,        SSC_NUMBER,      "fthrctrl",                  "Defocusing strategy",                                                              "none",         "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "accept_loc",                "In acceptance testing mode - temperature sensor location",                         "1/2",          "hx/loop",        "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "HDR_rough",                 "Header pipe roughness",                                                            "m",            "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "theta_stow",                "Stow angle",                                                                       "deg",          "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "theta_dep",                 "Deploy angle",                                                                     "deg",          "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "Row_Distance",              "Spacing between rows (centerline to centerline)",                                  "m",            "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "T_loop_in_des",             "Design loop inlet temperature",                                                    "C",            "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "T_loop_out",                "Target loop outlet temperature",                                                   "C",            "",               "solar_field",    "*",                       "",                      "" },
    //{ SSC_INPUT,        SSC_NUMBER,      "T_startup",                 "Required temperature of the system before the power block can be switched on",     "C",            "",               "solar_field",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "m_dot_htfmin",              "Minimum loop HTF flow rate",                                                       "kg/s",         "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "m_dot_htfmax",              "Maximum loop HTF flow rate",                                                       "kg/s",         "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "field_fl_props",            "User defined field fluid property data",                                           "-",            "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "T_fp",                      "Freeze protection temperature (heat trace activation temperature)",                "none",         "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "I_bn_des",                  "Solar irradiation at design",                                                      "C",            "",               "solar_field",    "*",                       "",                      "" },
    //{ SSC_INPUT,        SSC_NUMBER,      "V_hdr_max",                 "Maximum HTF velocity in the header at design",                                     "W/m2",         "",               "solar_field",    "*",                       "",                      "" },
    //{ SSC_INPUT,        SSC_NUMBER,      "V_hdr_min",                 "Minimum HTF velocity in the header at design",                                     "m/s",          "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "Pipe_hl_coef",              "Loss coefficient from the header, runner pipe, and non-HCE piping",                "m/s",          "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "SCA_drives_elec",           "Tracking power, in Watts per SCA drive",                                           "W/m2-K",       "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "tilt",                      "Tilt angle of surface/axis",                                                       "none",         "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "azimuth",                   "Azimuth angle of surface/axis",                                                    "none",         "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "wind_stow_speed",           "Trough wind stow speed",                                                           "m/s",          "",               "solar_field",    "?=50",                    "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "accept_mode",               "Acceptance testing mode?",                                                         "0/1",          "no/yes",         "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "accept_init",               "In acceptance testing mode - require steady-state startup",                        "none",         "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "solar_mult",                "Solar multiple",                                                                   "none",         "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "mc_bal_hot",                "Heat capacity of the balance of plant on the hot side",                            "kWht/K-MWt",   "none",           "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "mc_bal_cold",               "Heat capacity of the balance of plant on the cold side",                           "kWht/K-MWt",   "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "mc_bal_sca",                "Non-HTF heat capacity associated with each SCA - per meter basis",                 "Wht/K-m",      "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_ARRAY,       "W_aperture",                "The collector aperture width (Total structural area used for shadowing)",          "m",            "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_ARRAY,       "A_aperture",                "Reflective aperture area of the collector",                                        "m2",           "",               "solar_field",    "*",                       "",                      "" },                                                                                                                                                         
    { SSC_INPUT,        SSC_ARRAY,       "TrackingError",             "User-defined tracking error derate",                                               "none",         "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_ARRAY,       "GeomEffects",               "User-defined geometry effects derate",                                             "none",         "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_ARRAY,       "Rho_mirror_clean",          "User-defined clean mirror reflectivity",                                           "none",         "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_ARRAY,       "Dirt_mirror",               "User-defined dirt on mirror derate",                                               "none",         "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_ARRAY,       "Error",                     "User-defined general optical error derate ",                                       "none",         "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_ARRAY,       "Ave_Focal_Length",          "Average focal length of the collector ",                                           "m",            "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_ARRAY,       "L_SCA",                     "Length of the SCA ",                                                               "m",            "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_ARRAY,       "L_aperture",                "Length of a single mirror/HCE unit",                                               "m",            "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_ARRAY,       "ColperSCA",                 "Number of individual collector sections in an SCA ",                               "none",         "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_ARRAY,       "Distance_SCA",              "Piping distance between SCA's in the field",                                       "m",            "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "IAM_matrix",                "IAM coefficients, matrix for 4 collectors",                                        "none",         "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "HCE_FieldFrac",             "Fraction of the field occupied by this HCE type ",                                 "none",         "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "D_2",                       "Inner absorber tube diameter",                                                     "m",            "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "D_3",                       "Outer absorber tube diameter",                                                     "m",            "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "D_4",                       "Inner glass envelope diameter ",                                                   "m",            "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "D_5",                       "Outer glass envelope diameter ",                                                   "m",            "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "D_p",                       "Diameter of the absorber flow plug (optional) ",                                   "m",            "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "Flow_type",                 "Flow type through the absorber",                                                   "none",         "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "Rough",                     "Relative roughness of the internal HCE surface ",                                  "-",            "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "alpha_env",                 "Envelope absorptance ",                                                            "none",         "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "epsilon_3_11",              "Absorber emittance for receiver type 1 variation 1",                               "none",         "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "epsilon_3_12",              "Absorber emittance for receiver type 1 variation 2",                               "none",         "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "epsilon_3_13",              "Absorber emittance for receiver type 1 variation 3",                               "none",         "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "epsilon_3_14",              "Absorber emittance for receiver type 1 variation 4",                               "none",         "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "epsilon_3_21",              "Absorber emittance for receiver type 2 variation 1",                               "none",         "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "epsilon_3_22",              "Absorber emittance for receiver type 2 variation 2",                               "none",         "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "epsilon_3_23",              "Absorber emittance for receiver type 2 variation 3",                               "none",         "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "epsilon_3_24",              "Absorber emittance for receiver type 2 variation 4",                               "none",         "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "epsilon_3_31",              "Absorber emittance for receiver type 3 variation 1",                               "none",         "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "epsilon_3_32",              "Absorber emittance for receiver type 3 variation 2",                               "none",         "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "epsilon_3_33",              "Absorber emittance for receiver type 3 variation 3",                               "none",         "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "epsilon_3_34",              "Absorber emittance for receiver type 3 variation 4",                               "none",         "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "epsilon_3_41",              "Absorber emittance for receiver type 4 variation 1",                               "none",         "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "epsilon_3_42",              "Absorber emittance for receiver type 4 variation 2",                               "none",         "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "epsilon_3_43",              "Absorber emittance for receiver type 4 variation 3",                               "none",         "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "epsilon_3_44",              "Absorber emittance for receiver type 4 variation 4",                               "none",         "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "alpha_abs",                 "Absorber absorptance ",                                                            "none",         "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "Tau_envelope",              "Envelope transmittance",                                                           "none",         "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "EPSILON_4",                 "Inner glass envelope emissivities (Pyrex) ",                                       "none",         "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "EPSILON_5",                 "Outer glass envelope emissivities (Pyrex) ",                                       "none",         "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "GlazingIntactIn",           "Glazing intact (broken glass) flag {1=true, else=false}",                          "none",         "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "P_a",                       "Annulus gas pressure",                                                             "torr",         "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "AnnulusGas",                "Annulus gas type (1=air, 26=Ar, 27=H2)",                                           "none",         "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "AbsorberMaterial",          "Absorber material type",                                                           "none",         "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "Shadowing",                 "Receiver bellows shadowing loss factor",                                           "none",         "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "Dirt_HCE",                  "Loss due to dirt on the receiver envelope",                                        "none",         "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "Design_loss",               "Receiver heat loss at design",                                                     "W/m",          "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "SCAInfoArray",              "Receiver (,1) and collector (,2) type for each assembly in loop",                  "none",         "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_ARRAY,       "SCADefocusArray",           "Collector defocus order",                                                          "none",         "",               "solar_field",    "*",                       "",                      "" },      
    { SSC_INPUT,        SSC_NUMBER,      "rec_su_delay",              "Fixed startup delay time for the receiver",                                        "hr",           "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "rec_qf_delay",              "Energy-based receiver startup delay (fraction of rated thermal power)",            "-",            "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "p_start",                   "Collector startup energy, per SCA",                                                "kWe-hr",       "",               "solar_field",    "*",                       "",                      "" },

    // Power Cycle
    //{ SSC_INPUT,        SSC_NUMBER,      "q_pb_design",               "Design heat input to power block",                                                 "MWt",          "",               "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "pc_config",                 "0: Steam Rankine (224), 1: user defined",                                          "-",            "",               "powerblock",     "?=0",                     "INTEGER",               "" },
    { SSC_INPUT,        SSC_NUMBER,      "P_ref",                     "Rated plant capacity",                                                             "MWe",          "",               "powerblock",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "eta_ref",                   "Power cycle efficiency at design",                                                 "none",         "",               "powerblock",     "*",                       "",                      "" },
    //{ SSC_INPUT,        SSC_NUMBER,      "T_htf_hot_des",             "Hot HTF outlet temperature at design conditions",                                  "C",            "",               "powerblock",     "*",                       "",                      "" },
    //{ SSC_INPUT,        SSC_NUMBER,      "T_htf_cold_des",            "Cold HTF inlet temperature at design conditions",                                  "C",            "",               "powerblock",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "cycle_max_frac",            "Maximum turbine over design operation fraction",                                   "-",            "",               "powerblock",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "cycle_cutoff_frac",         "Minimum turbine operation fraction before shutdown",                               "-",            "",               "powerblock",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "q_sby_frac",                "Fraction of thermal power required for standby mode",                              "none",         "",               "powerblock",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "startup_time",              "Time needed for power block startup",                                              "hr",           "",               "powerblock",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "startup_frac",              "Fraction of design thermal power needed for startup",                              "none",         "",               "powerblock",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "pb_pump_coef",              "Pumping power to move 1kg of HTF through PB loop",                                 "kW/kg",        "",               "powerblock",     "*",                       "",                      "" },
    //{ SSC_INPUT,        SSC_NUMBER,      "rec_htf",                   "17: Salt (60% NaNO3, 40% KNO3) 10: Salt (46.5% LiF 11.5% NaF 42% KF) 50: Lookup tables", "",       "",               "powerblock",     "*",                      "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "dT_cw_ref",                 "Reference condenser cooling water inlet/outlet T diff",                            "C",            "",               "powerblock",     "pc_config=0",             "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "T_amb_des",                 "Reference ambient temperature at design point",                                    "C",            "",               "powerblock",     "pc_config=0",             "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "P_boil",                    "Boiler operating pressure",                                                        "bar",          "",               "powerblock",     "pc_config=0",             "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "CT",                        "Flag for using dry cooling or wet cooling system",                                 "none",         "",               "powerblock",     "pc_config=0",             "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "tech_type",                 "Turbine inlet pressure control flag (sliding=user, fixed=trough)",                 "1/2/3",        "tower/trough/user", "powerblock",  "pc_config=0",             "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "T_approach",                "Cooling tower approach temperature",                                               "C",            "",               "powerblock",     "pc_config=0",             "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "T_ITD_des",                 "ITD at design for dry system",                                                     "C",            "",               "powerblock",     "pc_config=0",             "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "P_cond_ratio",              "Condenser pressure ratio",                                                         "none",         "",               "powerblock",     "pc_config=0",             "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "pb_bd_frac",                "Power block blowdown steam fraction ",                                             "none",         "",               "powerblock",     "pc_config=0",             "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "P_cond_min",                "Minimum condenser pressure",                                                       "inHg",         "",               "powerblock",     "pc_config=0",             "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "n_pl_inc",                  "Number of part-load increments for the heat rejection system",                     "none",         "",               "powerblock",     "pc_config=0",             "",                      "" },
    { SSC_INPUT,        SSC_ARRAY,       "F_wc",                      "Fraction indicating wet cooling use for hybrid system",                            "none",         "constant=[0,0,0,0,0,0,0,0,0]",     "powerblock", "pc_config=0", "",                    "" },
    
    // UDPC parameters
    { SSC_INPUT,        SSC_NUMBER,      "ud_f_W_dot_cool_des",       "Percent of user-defined power cycle design gross output consumed by cooling",      "%",            "",               "powerblock",     "pc_config=1",             "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "ud_m_dot_water_cool_des",   "Mass flow rate of water required at user-defined power cycle design point",        "kg/s",         "",               "powerblock",     "pc_config=1",             "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "ud_ind_od",                 "Off design user-defined power cycle performance as function of T_htf, m_dot_htf [ND], and T_amb",   "", "",          "powerblock",     "pc_config=1",             "",                      "" },

    // TES
    { SSC_INPUT,        SSC_NUMBER,      "store_fluid",               "Material number for storage fluid",                                                "-",            "",               "TES",            "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "store_fl_props",            "User defined storage fluid property data",                                         "-",            "",               "TES",            "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "is_hx",                     "Heat exchanger (HX) exists (1=yes, 0=no)" ,                                        "-",            "",               "TES",            "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "tshours",                   "Equivalent full-load thermal storage hours",                                       "hr",           "",               "TES",            "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "h_tank",                    "Total height of tank (height of HTF when tank is full",                            "m",            "",               "TES",            "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "u_tank",                    "Loss coefficient from the tank",                                                   "W/m2-K",       "",               "TES",            "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "tank_pairs",                "Number of equivalent tank pairs",                                                  "-",            "",               "TES",            "*",                       "INTEGER",               "" },
    { SSC_INPUT,        SSC_NUMBER,      "hot_tank_Thtr",             "Minimum allowable hot tank HTF temp",                                              "C",            "",               "TES",            "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "hot_tank_max_heat",         "Rated heater capacity for hot tank heating",                                       "MWe",          "",               "TES",            "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "cold_tank_Thtr",            "Minimum allowable cold tank HTF temp",                                             "C",            "",               "TES",            "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "cold_tank_max_heat",        "Rated heater capacity for cold tank heating",                                      "MWe",          "",               "TES",            "*",                       "",                      "" },         
    { SSC_INPUT,        SSC_NUMBER,      "dt_hot",                    "Hot side HX approach temp",                                                        "C",            "",               "TES",            "*",                       "",                      "" },
    //{ SSC_INPUT,        SSC_NUMBER,      "dt_cold",                   "Cold side HX approach temp",                                                       "C",            "",               "TES",            "*",                      "",                      "" },
    //{ SSC_INPUT,        SSC_NUMBER,      "T_tank_hot_ini",            "Initial hot tank fluid tmeperature",                                               "C",            "",               "TES",            "*",                      "",                      "" },
    //{ SSC_INPUT,        SSC_NUMBER,      "T_tank_cold_ini",           "Initial cold tank fluid tmeperature",                                              "C",            "",               "TES",            "*",                      "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "h_tank_min",                "Minimum allowable HTF height in storage tank",                                     "m",            "",               "TES",            "*",                       "",                      "" }, 
    { SSC_INPUT,        SSC_NUMBER,      "init_hot_htf_percent",      "Initial fraction of avail. vol that is hot",                                       "%",            "",               "TES",            "*",                       "",                      "" },
    
    // TOU
    { SSC_INPUT,        SSC_MATRIX,      "weekday_schedule",          "12x24 CSP operation Time-of-Use Weekday schedule",                                 "-",            "",               "tou",            "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "weekend_schedule",          "12x24 CSP operation Time-of-Use Weekend schedule",                                 "-",            "",               "tou",            "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "dispatch_sched_weekday",    "12x24 PPA pricing Weekday schedule",                                               "",             "",               "tou",            "?=1",                     "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "dispatch_sched_weekend",    "12x24 PPA pricing Weekend schedule",                                               "",             "",               "tou",            "?=1",                     "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "is_tod_pc_target_also_pc_max", "Is the TOD target cycle heat input also the max cycle heat input?",             "",             "",               "tou",            "?=0",                     "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "is_dispatch",               "Allow dispatch optimization?",  /*TRUE=1*/                                         "-",            "",               "tou",            "?=0",                     "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "is_write_ampl_dat",         "Write AMPL data files for dispatch run",                                           "-",            "",               "tou",            "?=0",                     "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "is_ampl_engine",            "Run dispatch optimization with external AMPL engine",                              "-",            "",               "tou",            "?=0",                     "",                      "" },
    { SSC_INPUT,        SSC_STRING,      "ampl_data_dir",             "AMPL data file directory",                                                         "-",            "",               "tou",            "?=''",                    "",                      "" },
    { SSC_INPUT,        SSC_STRING,      "ampl_exec_call",            "System command to run AMPL code",                                                  "-",            "",               "tou",            "?='ampl sdk_solution.run'", "",                    "" },
    { SSC_INPUT,        SSC_NUMBER,      "disp_frequency",            "Frequency for dispatch optimization calculations",                                 "hour",         "",               "tou",            "is_dispatch=1",           "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "disp_steps_per_hour",       "Time steps per hour for dispatch optimization calculations",                       "-",            "",               "tou",            "?=1",                     "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "disp_horizon",              "Time horizon for dispatch optimization",                                           "hour",         "",               "tou",            "is_dispatch=1",           "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "disp_max_iter",             "Max. no. dispatch optimization iterations",                                        "-",            "",               "tou",            "is_dispatch=1",           "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "disp_timeout",              "Max. dispatch optimization solve duration",                                        "s",            "",               "tou",            "is_dispatch=1",           "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "disp_mip_gap",              "Dispatch optimization solution tolerance",                                         "-",            "",               "tou",            "is_dispatch=1",           "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "disp_spec_presolve",        "Dispatch optimization presolve heuristic",                                         "-",            "",               "tou",            "?=-1",                    "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "disp_spec_bb",              "Dispatch optimization B&B heuristic",                                              "-",            "",               "tou",            "?=-1",                    "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "disp_reporting",            "Dispatch optimization reporting level",                                            "-",            "",               "tou",            "?=-1",                    "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "disp_spec_scaling",         "Dispatch optimization scaling heuristic",                                          "-",            "",               "tou",            "?=-1",                    "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "disp_time_weighting",       "Dispatch optimization future time discounting factor",                             "-",            "",               "tou",            "?=0.99",                  "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "disp_rsu_cost",             "Receiver startup cost",                                                            "$",            "",               "tou",            "is_dispatch=1",           "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "disp_csu_cost",             "Cycle startup cost",                                                               "$",            "",               "tou",            "is_dispatch=1",           "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "disp_pen_delta_w",          "Dispatch cycle production change penalty",                                         "$/kWe-change", "",               "tou",            "is_dispatch=1",           "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "q_rec_standby",             "Receiver standby energy consumption",                                              "kWt",          "",               "tou",            "?=9e99",                  "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "q_rec_heattrace",           "Receiver heat trace energy consumption during startup",                            "kWe-hr",       "",               "tou",            "?=0.0",                   "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "is_wlim_series",            "Use time-series net electricity generation limits",                                "",             "",               "tou",            "?=0",                     "",                      "" },
    { SSC_INPUT,        SSC_ARRAY,       "wlim_series",               "Time series net electicity generation limits",                                     "kWe",          "",               "tou",            "is_wlim_series=1",        "",                      "" },
    { SSC_INPUT,        SSC_ARRAY,       "f_turb_tou_periods",        "Dispatch logic for turbine load fraction",                                         "-",            "",               "tou",            "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "ppa_multiplier_model",      "PPA multiplier model",                                                             "0/1",          "0=diurnal,1=timestep","tou",       "?=0",                     "INTEGER,MIN=0",         "" },
    { SSC_INPUT,        SSC_ARRAY,       "dispatch_factors_ts",       "Dispatch payment factor array",                                                    "",             "",               "tou",            "ppa_multiplier_model=1",  "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "dispatch_factor1",          "Dispatch payment factor 1",                                                        "",             "",               "tou",            "?=1",                     "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "dispatch_factor2",          "Dispatch payment factor 2",                                                        "",             "",               "tou",            "?=1",                     "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "dispatch_factor3",          "Dispatch payment factor 3",                                                        "",             "",               "tou",            "?=1",                     "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "dispatch_factor4",          "Dispatch payment factor 4",                                                        "",             "",               "tou",            "?=1",                     "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "dispatch_factor5",          "Dispatch payment factor 5",                                                        "",             "",               "tou",            "?=1",                     "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "dispatch_factor6",          "Dispatch payment factor 6",                                                        "",             "",               "tou",            "?=1",                     "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "dispatch_factor7",          "Dispatch payment factor 7",                                                        "",             "",               "tou",            "?=1",                     "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "dispatch_factor8",          "Dispatch payment factor 8",                                                        "",             "",               "tou",            "?=1",                     "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "dispatch_factor9",          "Dispatch payment factor 9",                                                        "",             "",               "tou",            "?=1",                     "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "is_dispatch_series",        "Use time-series dispatch factors",                                                 "",             "",               "tou",            "?=1",                     "",                      "" },
    { SSC_INPUT,        SSC_ARRAY,       "dispatch_series",           "Time series dispatch factors",                                                     "",             "",               "tou",            "",                        "",                      "" },

    // System
    { SSC_INPUT,        SSC_NUMBER,      "pb_fixed_par",              "Fraction of rated gross power constantly consumed",                                "MWe/MWcap",    "",               "system",         "*",                       "",                      "" },
    { SSC_INPUT,        SSC_ARRAY,       "bop_array",                 "Balance of plant parasitic power fraction, mult frac and const, linear and quad coeff", "",        "",               "system",         "*",                       "",                      "" },
    { SSC_INPUT,        SSC_ARRAY,       "aux_array",                 "Auxiliary heater, mult frac and const, linear and quad coeff",                     "",             "",               "system",         "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "gross_net_conversion_factor", "Estimated gross to net conversion factor",                                       "",             "",               "system",         "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "water_usage_per_wash",      "Water usage per wash",                                                             "L/m2_aper",    "",               "system",         "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "washing_frequency",         "Mirror washing frequency",                                                         "-/year",       "",               "system",         "*",                       "",                      "" },
    //{ SSC_INPUT,        SSC_NUMBER,      "system_capacity",           "Nameplate capacity",                                                               "kW",           "",               "system",         "*",                       "",                      "" },

    // Newly added
    { SSC_INPUT,        SSC_NUMBER,      "calc_design_pipe_vals",     "Calculate temps and pressures at design conditions for runners and headers",       "none",         "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "V_hdr_cold_max",            "Maximum HTF velocity in the cold headers at design",                               "m/s",          "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "V_hdr_cold_min",            "Minimum HTF velocity in the cold headers at design",                               "m/s",          "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "V_hdr_hot_max",             "Maximum HTF velocity in the hot headers at design",                                "m/s",          "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "V_hdr_hot_min",             "Minimum HTF velocity in the hot headers at design",                                "m/s",          "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "N_max_hdr_diams",           "Maximum number of diameters in each of the hot and cold headers",                  "none",         "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "L_rnr_pb",                  "Length of runner pipe in power block",                                             "m",            "",               "powerblock",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "L_rnr_per_xpan",            "Threshold length of straight runner pipe without an expansion loop",               "m",            "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "L_xpan_hdr",                "Compined perpendicular lengths of each header expansion loop",                     "m",            "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "L_xpan_rnr",                "Compined perpendicular lengths of each runner expansion loop",                     "m",            "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "Min_rnr_xpans",             "Minimum number of expansion loops per single-diameter runner section",             "none",         "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "northsouth_field_sep",      "North/south separation between subfields. 0 = SCAs are touching",                  "m",            "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "N_hdr_per_xpan",            "Number of collector loops per expansion loop",                                     "none",         "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "offset_xpan_hdr",           "Location of first header expansion loop. 1 = after first collector loop",          "none",         "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "K_cpnt",                    "Interconnect component minor loss coefficients, row=intc, col=cpnt",               "none",         "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "D_cpnt",                    "Interconnect component diameters, row=intc, col=cpnt",                             "none",         "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "L_cpnt",                    "Interconnect component lengths, row=intc, col=cpnt",                               "none",         "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "Type_cpnt",                 "Interconnect component type, row=intc, col=cpnt",                                  "none",         "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "custom_sf_pipe_sizes",      "Use custom solar field pipe diams, wallthks, and lengths",                         "none",         "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "sf_rnr_diams",              "Custom runner diameters",                                                          "m",            "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "sf_rnr_wallthicks",         "Custom runner wall thicknesses",                                                   "m",            "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "sf_rnr_lengths",            "Custom runner lengths",                                                            "m",            "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "sf_hdr_diams",              "Custom header diameters",                                                          "m",            "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "sf_hdr_wallthicks",         "Custom header wall thicknesses",                                                   "m",            "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "sf_hdr_lengths",            "Custom header lengths",                                                            "m",            "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "tanks_in_parallel",         "Tanks are in parallel, not in series, with solar field",                           "-",            "",               "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "has_hot_tank_bypass",       "Bypass valve connects field outlet to cold tank",                                  "-",            "",               "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "T_tank_hot_inlet_min",      "Minimum hot tank htf inlet temperature",                                           "C",            "",               "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "tes_pump_coef",             "Pumping power to move 1kg of HTF through tes loop",                                "kW/(kg/s)",    "",               "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "V_tes_des",                 "Design-point velocity to size the TES pipe diameters",                             "m/s",          "",               "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "custom_tes_p_loss",         "TES pipe losses are based on custom lengths and coeffs",                           "-",            "",               "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "k_tes_loss_coeffs",         "Minor loss coeffs for the coll, gen, and bypass loops",                            "-",            "",               "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "custom_tes_pipe_sizes",     "Use custom TES pipe diams, wallthks, and lengths",                                 "-",            "",               "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "tes_diams",                 "Custom TES diameters",                                                             "m",            "",               "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "tes_wallthicks",            "Custom TES wall thicknesses",                                                      "m",            "",               "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "tes_lengths",               "Custom TES lengths",                                                               "m",            "",               "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "DP_SGS",                    "Pressure drop within the steam generator",                                         "bar",          "",               "controller",     "*",                       "",                      "" },


    // *************************************************************************************************
    //    OUTPUTS
    // *************************************************************************************************

    // Simulation Kernel
    { SSC_OUTPUT,       SSC_ARRAY,       "time_hr",                   "Time at end of timestep",                                                          "hr",           "",               "solver",         "*",                       "",                      "" },
        
    // Weather Reader
    { SSC_OUTPUT,       SSC_ARRAY,       "month",                     "Resource Month",                                                                   "",             "",               "weather",        "*",                       "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "hour_day",                  "Resource Hour of Day",                                                             "",             "",               "weather",        "*",                       "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "solazi",                    "Resource Solar Azimuth",                                                           "deg",          "",               "weather",        "*",                       "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "solzen",                    "Resource Solar Zenith",                                                            "deg",          "",               "weather",        "*",                       "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "beam",                      "Resource Beam normal irradiance",                                                  "W/m2",         "",               "weather",        "*",                       "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "tdry",                      "Resource Dry bulb temperature",                                                    "C",            "",               "weather",        "*",                       "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "twet",                      "Resource Wet bulb temperature",                                                    "C",            "",               "weather",        "*",                       "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "rh",                        "Resource Relative Humidity",                                                       "%",            "",               "weather",        "*",                       "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "wspd",                      "Resource Wind Speed",                                                              "m/s",          "",               "weather",        "*",                       "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "pres",                      "Resource Pressure",                                                                "mbar",         "",               "weather",        "*",                       "",                      "" },
   
    { SSC_OUTPUT,       SSC_ARRAY,       "defocus",                   "Field optical focus fraction",                                                     "",             "",               "weather",        "*",                       "",                      "" },

    // Solar Field                                                                                                                                                                                                                   
    { SSC_OUTPUT,       SSC_ARRAY,       "Theta_ave",                 "Field collector solar incidence angle",                                            "deg",          "",               "solar_field",    "*",                       "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "CosTh_ave",                 "Field collector cosine efficiency",                                                "",             "",               "solar_field",    "*",                       "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "IAM_ave",                   "Field collector incidence angle modifier",                                         "",             "",               "solar_field",    "*",                       "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "RowShadow_ave",             "Field collector row shadowing loss",                                               "",             "",               "solar_field",    "*",                       "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "EndLoss_ave",               "Field collector optical end loss",                                                 "",             "",               "solar_field",    "*",                       "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "dni_costh",                 "Field collector DNI-cosine product",                                               "W/m2",         "",               "solar_field",    "*",                       "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "EqOpteff",                  "Field optical efficiency before defocus",                                          "",             "",               "solar_field",    "*",                       "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "SCAs_def",                  "Field fraction of focused SCAs",                                                   "",             "",               "solar_field",    "*",                       "",                      "" },
                                                                                                                                                                                                                                                               
    { SSC_OUTPUT,       SSC_ARRAY,       "q_inc_sf_tot",              "Field thermal power incident",                                                     "MWt",          "",               "solar_field",    "*",                       "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "qinc_costh",                "Field thermal power incident after cosine",                                        "MWt",          "",               "solar_field",    "*",                       "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "q_dot_rec_inc",             "Receiver thermal power incident",                                                  "MWt",          "",               "solar_field",    "*",                       "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "q_dot_rec_thermal_loss",    "Receiver thermal losses",                                                          "MWt",          "",               "solar_field",    "*",                       "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "q_dot_rec_abs",             "Receiver thermal power absorbed",                                                  "MWt",          "",               "solar_field",    "*",                       "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "q_dot_piping_loss",         "Field piping thermal losses",                                                      "MWt",          "",               "solar_field",    "*",                       "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "e_dot_field_int_energy",    "Field change in material/htf internal energy",                                     "MWt",          "",               "solar_field",    "*",                       "",                      "" }, 
    { SSC_OUTPUT,       SSC_ARRAY,       "q_dot_htf_sf_out",          "Field thermal power leaving in HTF",                                               "MWt",          "",               "solar_field",    "*",                       "",                      "" },  
    { SSC_OUTPUT,       SSC_ARRAY,       "q_dot_freeze_prot",         "Field freeze protection required",                                                 "MWt",          "",               "solar_field",    "*",                       "",                      "" },
                                                                                                                                                                                                                                                               
    { SSC_OUTPUT,       SSC_ARRAY,       "m_dot_loop",                "Receiver mass flow rate",                                                          "kg/s",         "",               "solar_field",    "*",                       "",                      "" },  
    { SSC_OUTPUT,       SSC_ARRAY,       "m_dot_field_recirc",        "Field total mass flow recirculated",                                               "kg/s",         "",               "solar_field",    "*",                       "",                      "" },  
    { SSC_OUTPUT,       SSC_ARRAY,       "m_dot_field_delivered",     "Field total mass flow delivered",                                                  "kg/s",         "",               "solar_field",    "*",                       "",                      "" },  
    { SSC_OUTPUT,       SSC_ARRAY,       "T_field_cold_in",           "Field timestep-averaged inlet temperature",                                        "C",            "",               "solar_field",    "*",                       "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "T_rec_cold_in",             "Loop timestep-averaged inlet temperature",                                         "C",            "",               "solar_field",    "*",                       "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "T_rec_hot_out",             "Loop timestep-averaged outlet temperature",                                        "C",            "",               "solar_field",    "*",                       "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "T_field_hot_out",           "Field timestep-averaged outlet temperature",                                       "C",            "",               "solar_field",    "*",                       "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "deltaP_field",              "Field pressure drop",                                                              "bar",          "",               "solar_field",    "*",                       "",                      "" },
                                                                                                                                                                                                                                                               
    { SSC_OUTPUT,       SSC_ARRAY,       "W_dot_sca_track",           "Field collector tracking power",                                                   "MWe",          "",               "solar_field",    "*",                       "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "W_dot_field_pump",          "Field htf pumping power",                                                          "MWe",          "",               "solar_field",    "*",                       "",                      "" },

    { SSC_OUTPUT,       SSC_ARRAY,       "pipe_header_diams",         "Field piping header diameters",                                                    "m",            "",               "solar_field",    "*",                       "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "pipe_header_wallthk",       "Field piping header wall thicknesses",                                             "m",            "",               "solar_field",    "*",                       "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "pipe_header_lengths",       "Field piping header lengths",                                                      "m",            "",               "solar_field",    "*",                       "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "pipe_header_expansions",    "Number of field piping header expansions",                                         "-",            "",               "solar_field",    "*",                       "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "pipe_header_mdot_dsn",      "Field piping header mass flow at design",                                          "kg/s",         "",               "solar_field",    "*",                       "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "pipe_header_vel_dsn",       "Field piping header velocity at design",                                           "m/s",          "",               "solar_field",    "*",                       "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "pipe_header_T_dsn",         "Field piping header temperature at design",                                        "C",            "",               "solar_field",    "*",                       "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "pipe_header_P_dsn",         "Field piping header pressure at design",                                           "bar",          "",               "solar_field",    "*",                       "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "pipe_runner_diams",         "Field piping runner diameters",                                                    "m",            "",               "solar_field",    "*",                       "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "pipe_runner_wallthk",       "Field piping runner wall thicknesses",                                             "m",            "",               "solar_field",    "*",                       "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "pipe_runner_lengths",       "Field piping runner lengths",                                                      "m",            "",               "solar_field",    "*",                       "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "pipe_runner_expansions",    "Number of field piping runner expansions",                                         "-",            "",               "solar_field",    "*",                       "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "pipe_runner_mdot_dsn",      "Field piping runner mass flow at design",                                          "kg/s",         "",               "solar_field",    "*",                       "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "pipe_runner_vel_dsn",       "Field piping runner velocity at design",                                           "m/s",          "",               "solar_field",    "*",                       "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "pipe_runner_T_dsn",         "Field piping runner temperature at design",                                        "C",            "",               "solar_field",    "*",                       "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "pipe_runner_P_dsn",         "Field piping runner pressure at design",                                           "bar",          "",               "solar_field",    "*",                       "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "pipe_loop_T_dsn",           "Field piping loop temperature at design",                                          "C",            "",               "solar_field",    "*",                       "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "pipe_loop_P_dsn",           "Field piping loop pressure at design",                                             "bar",          "",               "solar_field",    "*",                       "",                      "" },
    
    // Power Block
    { SSC_OUTPUT,       SSC_ARRAY,       "eta",                       "PC efficiency: gross",                                                             "",             "",               "powerblock",     "*",                       "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "q_pb",                      "PC input energy",                                                                  "MWt",          "",               "powerblock",     "*",                       "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "m_dot_pc",                  "PC HTF mass flow rate",                                                            "kg/s",         "",               "powerblock",     "*",                       "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "q_dot_pc_startup",          "PC startup thermal power",                                                         "MWt",          "",               "powerblock",     "*",                       "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "P_cycle",                   "PC electrical power output: gross",                                                "MWe",          "",               "powerblock",     "*",                       "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "T_pc_in",                   "PC HTF inlet temperature",                                                         "C",            "",               "powerblock",     "*",                       "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "T_pc_out",                  "PC HTF outlet temperature",                                                        "C",            "",               "powerblock",     "*",                       "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "m_dot_water_pc",            "PC water consumption: makeup + cooling",                                           "kg/s",         "",               "powerblock",     "*",                       "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "q_pc_startup",              "PC startup thermal energy",                                                        "MWht",         "",               "powerblock",     "*",                       "",                      "" },

    // TES
    { SSC_OUTPUT,       SSC_ARRAY,       "tank_losses",               "TES thermal losses",                  "MWt",          "",          "TES",            "*",          "",    "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "q_tes_heater",              "TES freeze protection power",         "MWe",          "",          "TES",            "*",          "",    "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "T_tes_hot",                 "TES hot temperature",                 "C",            "",          "TES",            "*",          "",    "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "T_tes_cold",                "TES cold temperature",                "C",            "",          "TES",            "*",          "",    "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "mass_tes_cold",             "TES cold tank mass (end)",            "kg",           "",          "TES",            "*",          "",    "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "mass_tes_hot",              "TES hot tank mass (end)",             "kg",           "",          "TES",            "*",          "",    "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "q_dc_tes",                  "TES discharge thermal power",         "MWt",          "",          "TES",            "*",          "",    "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "q_ch_tes",                  "TES charge thermal power",            "MWt",          "",          "TES",            "*",          "",    "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "e_ch_tes",                  "TES charge state",                    "MWht",         "",          "TES",            "*",          "",    "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "m_dot_cr_to_tes_hot",       "Mass flow: field to hot TES",         "kg/s",         "",          "TES",            "*",          "",    "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "m_dot_tes_hot_out",         "Mass flow: TES hot out",              "kg/s",         "",          "TES",            "*",          "",    "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "m_dot_pc_to_tes_cold",      "Mass flow: cycle to cold TES",        "kg/s",         "",          "TES",            "*",          "",    "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "m_dot_tes_cold_out",        "Mass flow: TES cold out",             "kg/s",         "",          "TES",            "*",          "",    "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "m_dot_field_to_cycle",      "Mass flow: field to cycle",           "kg/s",         "",          "TES",            "*",          "",    "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "m_dot_cycle_to_field",      "Mass flow: cycle to field",           "kg/s",         "",          "TES",            "*",          "",    "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "m_dot_cold_tank_to_hot_tank", "Mass flow: cold tank to hot tank",  "kg/s",         "",          "TES",            "*",          "",    "" },


    //{ SSC_OUTPUT,       SSC_ARRAY,       "m_dot_tes_dc",              "TES discharge mass flow rate",                                                     "kg/s",         "",               "TES",            "*",                       "",                      "" },
    //{ SSC_OUTPUT,       SSC_ARRAY,       "m_dot_tes_ch",              "TES charge mass flow rate",                                                        "kg/s",         "",               "TES",            "*",                       "",                      "" },
    
    // SYSTEM
    //{ SSC_OUTPUT,       SSC_ARRAY,       "W_dot_parasitic_tot",       "System total electrical parasitic",                                                "MWe",          "",               "system",         "*",                       "",                      "" },
                                                                                                                                                                                                            
    // Controller                                                                                                                                                                                           
    { SSC_OUTPUT,       SSC_ARRAY,       "op_mode_1",                 "1st operating mode",                                                               "",             "",               "solver",         "*",                       "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "op_mode_2",                 "2nd op. mode, if applicable",                                                      "",             "",               "solver",         "*",                       "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "op_mode_3",                 "3rd op. mode, if applicable",                                                      "",             "",               "solver",         "*",                       "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "m_dot_balance",             "Relative mass flow balance error",                                                 "",             "",               "solver",         "*",                       "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "q_balance",                 "Relative energy balance error",                                                    "",             "",               "solver",         "*",                       "",                      "" },

    // Monthly Outputs
    { SSC_OUTPUT,       SSC_ARRAY,       "monthly_energy",            "Monthly Energy",                                                                   "kWh",          "",               "Post-process",   "*",              "LENGTH=12",                      "" },

    // Annual Outputs
    { SSC_OUTPUT,       SSC_NUMBER,      "annual_energy",                   "Annual Net Electrical Energy Production w/ avail derate",                    "kWe-hr",       "",               "Post-process",   "*",                       "",                      "" },
    //{ SSC_OUTPUT,       SSC_NUMBER,      "annual_gross_energy",             "Annual Gross Electrical Energy Production w/ avail derate",                  "kWe-hr",       "",               "Post-process",   "*",                       "",                      "" },
    { SSC_OUTPUT,       SSC_NUMBER,      "annual_thermal_consumption",      "Annual thermal freeze protection required",                                  "kWt-hr",       "",               "Post-process",   "*",                       "",                      "" },
    //{ SSC_OUTPUT,       SSC_NUMBER,      "annual_electricity_consumption",  "Annual electricity consumptoin w/ avail derate",                             "kWe-hr",       "",               "Post-process",   "*",                       "",                      "" },
    { SSC_OUTPUT,       SSC_NUMBER,      "annual_total_water_use",          "Total Annual Water Usage",                                                   "m^3",          "",               "Post-process",   "*",                       "",                      "" },
    { SSC_OUTPUT,       SSC_NUMBER,      "annual_field_freeze_protection",  "Annual thermal power for field freeze protection",                           "kWt-hr",       "",               "Post-process",   "*",                       "",                      "" },
    { SSC_OUTPUT,       SSC_NUMBER,      "annual_tes_freeze_protection",    "Annual thermal power for TES freeze protection",                             "kWt-hr",       "",               "Post-process",   "*",                       "",                      "" },

    // Newly added
    { SSC_OUTPUT,       SSC_ARRAY,       "n_op_modes",                "Operating modes in reporting timestep",                                            "",             "",               "solver",         "*",                       "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "tou_value",                 "CSP operating Time-of-use value",                                                  "",             "",               "solver",         "*",                       "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "pricing_mult",              "PPA price multiplier",                                                             "",             "",               "solver",         "*",                       "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "q_dot_pc_sb",               "Thermal power for PC standby",                                                     "MWt",          "",               "solver",         "*",                       "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "q_dot_pc_min",              "Thermal power for PC min operation",                                               "MWt",          "",               "solver",         "*",                       "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "q_dot_pc_target",           "Target thermal power to PC",                                                       "MWt",          "",               "solver",         "*",                       "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "q_dot_pc_max",              "Max thermal power to PC",                                                          "MWt",          "",               "solver",         "*",                       "",                      "" },
                                                                                                                                                                                                                                                                  
    { SSC_OUTPUT,       SSC_ARRAY,       "is_rec_su_allowed",         "is receiver startup allowed",                                                      "",             "",               "solver",         "*",                       "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "is_pc_su_allowed",          "is power cycle startup allowed",                                                   "",             "",               "solver",         "*",                       "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "is_pc_sb_allowed",          "is power cycle standby allowed",                                                   "",             "",               "solver",         "*",                       "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "q_dot_est_cr_su",           "Estimate rec. startup thermal power",                                              "MWt",           "",              "solver",         "*",                       "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "q_dot_est_cr_on",           "Estimate rec. thermal power TO HTF",                                               "MWt",          "",               "solver",         "*",                       "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "q_dot_est_tes_dc",          "Estimate max TES discharge thermal power",                                         "MWt",          "",               "solver",         "*",                       "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "q_dot_est_tes_ch",          "Estimate max TES charge thermal power",                                            "MWt",          "",               "solver",         "*",                       "",                      "" },
                                                                                                                                                                                                                                                                  
    { SSC_OUTPUT,       SSC_ARRAY,       "operating_modes_a",         "First 3 operating modes tried",                                                    "",             "",               "solver",         "*",                       "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "operating_modes_b",         "Next 3 operating modes tried",                                                     "",             "",               "solver",         "*",                       "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "operating_modes_c",         "Final 3 operating modes tried",                                                    "",             "",               "solver",         "*",                       "",                      "" },
                                                                                                                                                                                                                                                                  
    { SSC_OUTPUT,       SSC_ARRAY,       "disp_solve_state",          "Dispatch solver state",                                                            "",             "",               "tou",            "*",                       "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "disp_solve_iter",           "Dispatch iterations count",                                                        "",             "",               "tou",            "*",                       "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "disp_objective",            "Dispatch objective function value",                                                "",             "",               "tou",            "*",                       "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "disp_obj_relax",            "Dispatch objective function - relaxed max",                                        "",             "",               "tou",            "*",                       "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "disp_qsf_expected",         "Dispatch expected solar field available energy",                                   "MWt",          "",               "tou",            "*",                       "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "disp_qsfprod_expected",     "Dispatch expected solar field generation",                                         "MWt",          "",               "tou",            "*",                       "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "disp_qsfsu_expected",       "Dispatch expected solar field startup enegy",                                      "MWt",          "",               "tou",            "*",                       "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "disp_tes_expected",         "Dispatch expected TES charge level",                                               "MWht",         "",               "tou",            "*",                       "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "disp_pceff_expected",       "Dispatch expected power cycle efficiency adj.",                                    "",             "",               "tou",            "*",                       "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "disp_thermeff_expected",    "Dispatch expected SF thermal efficiency adj.",                                     "",             "",               "tou",            "*",                       "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "disp_qpbsu_expected",       "Dispatch expected power cycle startup energy",                                     "MWht",         "",               "tou",            "*",                       "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "disp_wpb_expected",         "Dispatch expected power generation",                                               "MWe",          "",               "tou",            "*",                       "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "disp_rev_expected",         "Dispatch expected revenue factor",                                                 "",             "",               "tou",            "*",                       "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "disp_presolve_nconstr",     "Dispatch number of constraints in problem",                                        "",             "",               "tou",            "*",                       "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "disp_presolve_nvar",        "Dispatch number of variables in problem",                                          "",             "",               "tou",            "*",                       "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "disp_solve_time",           "Dispatch solver time",                                                             "sec",          "",               "tou",            "*",                       "",                      "" },
                                                                                                                                                                                                                                                                  
    { SSC_OUTPUT,       SSC_ARRAY,       "htf_pump_power",            "Parasitic power TES and Cycle HTF pump",                                           "MWe",          "",               "system",         "*",                       "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "P_cooling_tower_tot",       "Parasitic power condenser operation",                                              "MWe",          "",               "system",         "*",                       "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "P_fixed",                   "Parasitic power fixed load",                                                       "MWe",          "",               "system",         "*",                       "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "P_plant_balance_tot",       "Parasitic power generation-dependent load",                                        "MWe",          "",               "system",         "*",                       "",                      "" },
                                                                                                                                                                                                                                                                  
    { SSC_OUTPUT,       SSC_ARRAY,       "P_out_net",                 "Total electric power to grid",                                                     "MWe",          "",               "system",         "*",                       "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "gen",                       "Total electric power to grid w/ avail. derate",                                    "kWe",          "",               "system",         "*",                       "",                      "" },
    { SSC_OUTPUT,       SSC_NUMBER,      "annual_W_cycle_gross",      "Electrical source - Power cycle gross output",                                     "kWhe",         "",               "system",         "*",                       "",                      "" },
    { SSC_OUTPUT,       SSC_NUMBER,      "conversion_factor",         "Gross to Net Conversion Factor",                                                   "%",            "",               "system",         "*",                       "",                      "" },
    { SSC_OUTPUT,       SSC_NUMBER,      "capacity_factor",           "Capacity factor",                                                                  "%",            "",               "system",         "*",                       "",                      "" },
    { SSC_OUTPUT,       SSC_NUMBER,      "kwh_per_kw",                "First year kWh/kW",                                                                "kWh/kW",       "",               "system",         "*",                       "",                      "" },
    { SSC_OUTPUT,       SSC_NUMBER,      "sim_duration",              "Computational time of timeseries simulation",                                      "s",            "",               "system",         "*",                       "",                      "" },
    //{ SSC_OUTPUT,       SSC_NUMBER,      "W_dot_par_tot_haf",         "Adjusted parasitic power",                                                         "kWe",          "",               "system",         "*",                       "",                      "" },
    //{ SSC_OUTPUT,       SSC_NUMBER,      "q_dot_defocus_est",         "Thermal energy intentionally lost by defocusing",                                  "MWt",          "",               "system",         "*",                       "",                      "" },

    { SSC_OUTPUT,       SSC_ARRAY,       "recirculating",             "Field recirculating (bypass valve open)",                                          "-",            "",               "solar_field",    "*",                       "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "pipe_tes_diams",            "Pipe diameters in TES",                                                            "m",            "",               "TES",            "*",                       "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "pipe_tes_wallthk",          "Pipe wall thickness in TES",                                                       "m",            "",               "TES",            "*",                       "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "pipe_tes_lengths",          "Pipe lengths in TES",                                                              "m",            "",               "TES",            "*",                       "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "pipe_tes_mdot_dsn",         "Mass flow TES pipes at design conditions",                                         "kg/s",         "",               "TES",            "*",                       "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "pipe_tes_vel_dsn",          "Velocity in TES pipes at design conditions",                                       "m/s",          "",               "TES",            "*",                       "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "pipe_tes_T_dsn",            "Temperature in TES pipes at design conditions",                                    "C",            "",               "TES",            "*",                       "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "pipe_tes_P_dsn",            "Pressure in TES pipes at design conditions",                                       "bar",          "",               "TES",            "*",                       "",                      "" },

    //{ SSC_OUTPUT,       SSC_ARRAY,       "defocus",                   "Field optical focus fraction",                                                     "",             "",               "solver",         "*",                       "",                      "" },

    var_info_invalid };
    
    
    
class cm_trough_physical : public compute_module
{
public:

    cm_trough_physical()
    {
        add_var_info( _cm_vtab_trough_physical );
        add_var_info( vtab_adjustment_factors );
    }

    void exec( )
    {   
        // ********************************
        // ********************************
        // Weather reader
        // ********************************
        // ********************************
        C_csp_weatherreader weather_reader;
        weather_reader.m_weather_data_provider = std::make_shared<weatherfile>(as_string("file_name"));
        weather_reader.m_filename = as_string("file_name");
        weather_reader.m_trackmode = 0;
        weather_reader.m_tilt = 0.0;
        weather_reader.m_azimuth = 0.0;
        // Initialize to get weather file info
        weather_reader.init();
        if (weather_reader.has_error()) throw exec_error("trough_physical", weather_reader.get_error());

        // Set up ssc output arrays
        // Set steps per hour
        double nhourssim = 8760.0;                                  //[hr] Number of hours to simulate
        C_csp_solver::S_sim_setup sim_setup;
        sim_setup.m_sim_time_start = 0.0;                           //[s] starting first hour of year
        sim_setup.m_sim_time_end = nhourssim*3600.;                 //[s] full year simulation

        int steps_per_hour = 1;                                     //[-]

        int n_wf_records = (int)weather_reader.m_weather_data_provider->nrecords();
        steps_per_hour = n_wf_records / 8760;                       //[-]

        int n_steps_fixed = steps_per_hour*8760;                    //[-]
        sim_setup.m_report_step = 3600.0 / (double)steps_per_hour;  //[s]

        // ********************************
        // ********************************
        // Solar field, trough
        // ********************************
        // ********************************
        C_csp_trough_collector_receiver c_trough;

        c_trough.m_nSCA = as_integer("nSCA");                       //[-] Number of SCA's in a loop
        c_trough.m_nHCEt = as_integer("nHCEt");                     //[-] Number of HCE types
        c_trough.m_nColt = as_integer("nColt");                     //[-] Number of collector types
        c_trough.m_nHCEVar = as_integer("nHCEVar");                 //[-] Number of HCE variants per t
        c_trough.m_nLoops = as_integer("nLoops");                   //[-] Number of loops in the field
        c_trough.m_FieldConfig = as_integer("FieldConfig");         //[-] Number of subfield headers
        c_trough.m_L_power_block_piping = as_double("L_power_block_piping");                           //[m] Length of piping (full mass flow) through power block (if applicable)
        c_trough.m_include_fixed_power_block_runner = as_boolean("include_fixed_power_block_runner");  //[-] Should model consider piping through power block?
        c_trough.m_eta_pump = as_double("eta_pump");                //[-] HTF pump efficiency
        c_trough.m_Fluid = as_integer("Fluid");                     //[-] Field HTF fluid number
        //c_trough.m_fthrok = as_integer("fthrok");                 //[-] Flag to allow partial defocusing of the collectors
        c_trough.m_fthrctrl = 2;                                    //[-] Defocusing strategy; hardcode = 2 for now
        c_trough.m_accept_loc = as_integer("accept_loc");           //[-] In acceptance testing mode - temperature sensor location (1=hx,2=loop)
        c_trough.m_HDR_rough = as_double("HDR_rough");              //[-] Header pipe relative roughness
        c_trough.m_theta_stow = as_double("theta_stow");            //[deg] stow angle
        c_trough.m_theta_dep = as_double("theta_dep");              //[deg] deploy angle
        c_trough.m_Row_Distance = as_double("Row_Distance");        //[m] Spacing between rows (centerline to centerline)
        
        double T_loop_in_des = as_double("T_loop_in_des");          //[C] Design loop inlet temperature, converted to K in init
        c_trough.m_T_loop_in_des = T_loop_in_des;                   //[C] Design loop inlet temperature, converted to K in init
        double T_loop_out_des = as_double("T_loop_out");            //[C] Target loop outlet temperature, converted to K in init
        c_trough.m_T_loop_out_des = T_loop_out_des;                 //[C] Target loop outlet temperature, converted to K in init
        double T_startup_min = T_loop_in_des;
        if (T_loop_out_des > 600.0)
        {
            T_startup_min = T_loop_out_des - 70.0;
        }
        double T_startup = std::max(T_startup_min, 0.67*T_loop_in_des + 0.33*T_loop_out_des); //[C]
        c_trough.m_T_startup = T_startup;                           //[C] The required temperature (converted to K in init) of the system before the power block can be switched on

        c_trough.m_m_dot_htfmin = as_double("m_dot_htfmin");        //[kg/s] Minimum loop HTF flow rate
        c_trough.m_m_dot_htfmax = as_double("m_dot_htfmax");        //[kg/s] Maximum loop HTF flow rate
        c_trough.m_field_fl_props = as_matrix("field_fl_props");    //[-] User-defined field HTF properties
        c_trough.m_T_fp = as_double("T_fp");                        //[C] Freeze protection temperature (heat trace activation temperature), convert to K in init
        c_trough.m_I_bn_des = as_double("I_bn_des");                //[W/m^2] Solar irradiation at design
        c_trough.m_V_hdr_cold_max = as_double("V_hdr_cold_max");    //[m/s] Maximum HTF velocity in the cold header at design
        c_trough.m_V_hdr_cold_min = as_double("V_hdr_cold_min");    //[m/s] Minimum HTF velocity in the cold header at design
        c_trough.m_V_hdr_hot_max = as_double("V_hdr_hot_max");      //[m/s] Maximum HTF velocity in the hot header at design
        c_trough.m_V_hdr_hot_min = as_double("V_hdr_hot_min");      //[m/s] Minimum HTF velocity in the hot header at design
        c_trough.m_Pipe_hl_coef = as_double("Pipe_hl_coef");        //[W/m2-K] Loss coefficient from the header, runner pipe, and non-HCE piping
        c_trough.m_SCA_drives_elec = as_double("SCA_drives_elec");  //[W/SCA] Tracking power, in Watts per SCA drive
        c_trough.m_ColTilt = as_double("tilt");                     //[deg] Collector tilt angle (0 is horizontal, 90deg is vertical)
        c_trough.m_ColAz = as_double("azimuth");                    //[deg] Collector azimuth angle
        c_trough.m_wind_stow_speed = as_double("wind_stow_speed");  //[m/s] Wind speed at and above which the collectors will be stowed
        c_trough.m_accept_mode = as_integer("accept_mode");         //[-] Acceptance testing mode? (1=yes, 0=no)
        c_trough.m_accept_init = as_boolean("accept_init");         //[-] In acceptance testing mode - require steady-state startup
        c_trough.m_solar_mult = as_double("solar_mult");            //[-] Solar Multiple
        c_trough.m_mc_bal_hot_per_MW = as_double("mc_bal_hot");     //[kWht/K-MWt] The heat capacity of the balance of plant on the hot side
        c_trough.m_mc_bal_cold_per_MW = as_double("mc_bal_cold");   //[kWht/K-MWt] The heat capacity of the balance of plant on the cold side
        c_trough.m_mc_bal_sca = as_double("mc_bal_sca");            //[Wht/K-m] Non-HTF heat capacity associated with each SCA - per meter basis
        
        //[m] The collector aperture width (Total structural area.. used for shadowing)
        size_t nval_W_aperture = 0;
        ssc_number_t *W_aperture = as_array("W_aperture", &nval_W_aperture);
        c_trough.m_W_aperture.resize(nval_W_aperture);
        for (size_t i = 0; i < nval_W_aperture; i++)
            c_trough.m_W_aperture[i] = (double)W_aperture[i];
        
        //[m^2] Reflective aperture area of the collector
        size_t nval_A_aperture = 0;
        ssc_number_t *A_aperture = as_array("A_aperture", &nval_A_aperture);
        c_trough.m_A_aperture.resize(nval_A_aperture);
        for (size_t i = 0; i < nval_A_aperture; i++)
            c_trough.m_A_aperture[i] = (double)A_aperture[i];

        //[-] Tracking error derate
        size_t nval_TrackingError = 0;
        ssc_number_t *TrackingError = as_array("TrackingError", &nval_TrackingError);
        c_trough.m_TrackingError.resize(nval_TrackingError);
        for (size_t i = 0; i < nval_TrackingError; i++)
            c_trough.m_TrackingError[i] = (double)TrackingError[i];
        
        //[-] Geometry effects derate
        size_t nval_GeomEffects = 0;
        ssc_number_t *GeomEffects = as_array("GeomEffects", &nval_GeomEffects);
        c_trough.m_GeomEffects.resize(nval_GeomEffects);
        for (size_t i = 0; i < nval_GeomEffects; i++)
            c_trough.m_GeomEffects[i] = (double)GeomEffects[i];

        //[-] Clean mirror reflectivity
        size_t nval_Rho_mirror_clean = 0;
        ssc_number_t *Rho_mirror_clean = as_array("Rho_mirror_clean", &nval_Rho_mirror_clean);
        c_trough.m_Rho_mirror_clean.resize(nval_Rho_mirror_clean);
        for (size_t i = 0; i < nval_Rho_mirror_clean; i++)
            c_trough.m_Rho_mirror_clean[i] = (double)Rho_mirror_clean[i];
        
        //[-] Dirt on mirror derate
        size_t nval_Dirt_mirror = 0;
        ssc_number_t *Dirt_mirror = as_array("Dirt_mirror", &nval_Dirt_mirror);
        c_trough.m_Dirt_mirror.resize(nval_Dirt_mirror);
        for (size_t i = 0; i < nval_Dirt_mirror; i++)
            c_trough.m_Dirt_mirror[i] = (double)Dirt_mirror[i];
        
        //[-] General optical error derate
        size_t nval_Error = 0;
        ssc_number_t *Error = as_array("Error", &nval_Error);
        c_trough.m_Error.resize(nval_Error);
        for (size_t i = 0; i < nval_Error; i++)
            c_trough.m_Error[i] = (double)Error[i];
        
        //[m] The average focal length of the collector 
        size_t nval_Ave_Focal_Length = 0;
        ssc_number_t *Ave_Focal_Length = as_array("Ave_Focal_Length", &nval_Ave_Focal_Length);
        c_trough.m_Ave_Focal_Length.resize(nval_Ave_Focal_Length);
        for (size_t i = 0; i < nval_Ave_Focal_Length; i++)
            c_trough.m_Ave_Focal_Length[i] = (double)Ave_Focal_Length[i];
        
        //[m] The length of the SCA 
        size_t nval_L_SCA = 0;
        ssc_number_t *L_SCA = as_array("L_SCA", &nval_L_SCA);
        c_trough.m_L_SCA.resize(nval_L_SCA);
        for (size_t i = 0; i < nval_L_SCA; i++)
            c_trough.m_L_SCA[i] = (double)L_SCA[i];

        //[m] The length of a single mirror/HCE unit
        size_t nval_L_aperture = 0;
        ssc_number_t *L_aperture = as_array("L_aperture", &nval_L_aperture);
        c_trough.m_L_aperture.resize(nval_L_aperture);
        for (size_t i = 0; i < nval_L_aperture; i++)
            c_trough.m_L_aperture[i] = (double)L_aperture[i];
        
        //[-] The number of individual collector sections in an SCA
        size_t nval_ColperSCA = 0;
        ssc_number_t *ColperSCA = as_array("ColperSCA", &nval_ColperSCA);
        c_trough.m_ColperSCA.resize(nval_ColperSCA);
        for (size_t i = 0; i < nval_ColperSCA; i++)
            c_trough.m_ColperSCA[i] = (double)ColperSCA[i];

        //[m] Piping distance between SCA's in the field
        size_t nval_Distance_SCA = 0;
        ssc_number_t *Distance_SCA = as_array("Distance_SCA", &nval_Distance_SCA);
        c_trough.m_Distance_SCA.resize(nval_Distance_SCA);
        for (size_t i = 0; i < nval_Distance_SCA; i++)
            c_trough.m_Distance_SCA[i] = (double)Distance_SCA[i];

        c_trough.m_IAM_matrix = as_matrix("IAM_matrix");                //[-] IAM coefficients, matrix for 4 collectors
                                                                        
        // Why are these matrices - can't they be arrays?               
        c_trough.m_HCE_FieldFrac = as_matrix("HCE_FieldFrac");          //[-] Fraction of the field occupied by this HCE type
        c_trough.m_D_2 = as_matrix("D_2");                              //[m] Inner absorber tube diameter
        c_trough.m_D_3 = as_matrix("D_3");                              //[m] Outer absorber tube diameter
        c_trough.m_D_4 = as_matrix("D_4");                              //[m] Inner glass envelope diameter
        c_trough.m_D_5 = as_matrix("D_5");                              //[m] Outer glass envelope diameter
        c_trough.m_D_p = as_matrix("D_p");                              //[m] Diameter of the absorber flow plug (optional)
        c_trough.m_Flow_type = as_matrix("Flow_type");                  //[-] Flow type through the absorber
        c_trough.m_Rough = as_matrix("Rough");                          //[-] Relative roughness of the internal HCE surface
        c_trough.m_alpha_env = as_matrix("alpha_env");                  //[-] Envelope absorptance
        // **********************************************************
        
        // Emittance vs. temperature profile for each receiver type and variation
        c_trough.m_epsilon_3_11 = as_matrix_transpose("epsilon_3_11");  //[-] Absorber emittance for receiver type 1 variation 1
        c_trough.m_epsilon_3_12 = as_matrix_transpose("epsilon_3_12");  //[-] Absorber emittance for receiver type 1 variation 2
        c_trough.m_epsilon_3_13 = as_matrix_transpose("epsilon_3_13");  //[-] Absorber emittance for receiver type 1 variation 3
        c_trough.m_epsilon_3_14 = as_matrix_transpose("epsilon_3_14");  //[-] Absorber emittance for receiver type 1 variation 4
        c_trough.m_epsilon_3_21 = as_matrix_transpose("epsilon_3_21");  //[-] Absorber emittance for receiver type 2 variation 1
        c_trough.m_epsilon_3_22 = as_matrix_transpose("epsilon_3_22");  //[-] Absorber emittance for receiver type 2 variation 2
        c_trough.m_epsilon_3_23 = as_matrix_transpose("epsilon_3_23");  //[-] Absorber emittance for receiver type 2 variation 3
        c_trough.m_epsilon_3_24 = as_matrix_transpose("epsilon_3_24");  //[-] Absorber emittance for receiver type 2 variation 4
        c_trough.m_epsilon_3_31 = as_matrix_transpose("epsilon_3_31");  //[-] Absorber emittance for receiver type 3 variation 1
        c_trough.m_epsilon_3_32 = as_matrix_transpose("epsilon_3_32");  //[-] Absorber emittance for receiver type 3 variation 2
        c_trough.m_epsilon_3_33 = as_matrix_transpose("epsilon_3_33");  //[-] Absorber emittance for receiver type 3 variation 3
        c_trough.m_epsilon_3_34 = as_matrix_transpose("epsilon_3_34");  //[-] Absorber emittance for receiver type 3 variation 4
        c_trough.m_epsilon_3_41 = as_matrix_transpose("epsilon_3_41");  //[-] Absorber emittance for receiver type 4 variation 1
        c_trough.m_epsilon_3_42 = as_matrix_transpose("epsilon_3_42");  //[-] Absorber emittance for receiver type 4 variation 2
        c_trough.m_epsilon_3_43 = as_matrix_transpose("epsilon_3_43");  //[-] Absorber emittance for receiver type 4 variation 3
        c_trough.m_epsilon_3_44 = as_matrix_transpose("epsilon_3_44");  //[-] Absorber emittance for receiver type 4 variation 4

        c_trough.m_alpha_abs = as_matrix("alpha_abs");                  //[-] Absorber absorptance
        c_trough.m_Tau_envelope = as_matrix("Tau_envelope");            //[-] Envelope transmittance
        c_trough.m_EPSILON_4 = as_matrix("EPSILON_4");                  //[-] Inner glass envelope emissivities
        c_trough.m_EPSILON_5 = as_matrix("EPSILON_5");                  //[-] Outer glass envelope emissivities
        
        //c_trough.m_GlazingIntact = (as_matrix("GlazingIntactIn") > 0);  //[-] Glazing intact (broken glass) flag {1=true, else=false}
        util::matrix_t<double> glazing_intact_double = as_matrix("GlazingIntactIn"); //[-] Is the glazing intact?
        int n_gl_row = (int)glazing_intact_double.nrows();
        int n_gl_col = (int)glazing_intact_double.ncols();
        c_trough.m_GlazingIntact.resize(n_gl_row, n_gl_col);
        for (int i = 0; i < n_gl_row; i++)
        {
            for (int j = 0; j < n_gl_col; j++)
            {
                c_trough.m_GlazingIntact(i, j) = (glazing_intact_double(i, j) > 0);
            }
        }

        c_trough.m_P_a = as_matrix("P_a");                              //[torr] Annulus gas pressure
        c_trough.m_AnnulusGas = as_matrix("AnnulusGas");                //[-] Annulus gas type (1=air, 26=Ar, 27=H2)
        c_trough.m_AbsorberMaterial = as_matrix("AbsorberMaterial");    //[-] Absorber material type
        c_trough.m_Shadowing = as_matrix("Shadowing");                  //[-] Receiver bellows shadowing loss factor
        c_trough.m_Dirt_HCE = as_matrix("Dirt_HCE");                    //[-] Loss due to dirt on the receiver envelope
        c_trough.m_Design_loss = as_matrix("Design_loss");              //[-] Receiver heat loss at design
        c_trough.m_SCAInfoArray = as_matrix("SCAInfoArray");            //[-] Receiver (,1) and collector (,2) type for each assembly in loop
        c_trough.m_rec_su_delay = as_double("rec_su_delay");            //[hr] Fixed startup delay time for the receiver
        c_trough.m_rec_qf_delay = as_double("rec_qf_delay");            //[-] Energy-based receiver startup delay (fraction of rated thermal power)
        c_trough.m_p_start = as_double("p_start");                      //[kWe-hr] Collector startup energy, per SCA
        
        c_trough.m_calc_design_pipe_vals = as_boolean("calc_design_pipe_vals"); //[-] Should the HTF state be calculated at design conditions
        c_trough.m_L_rnr_pb = as_double("L_rnr_pb");                      //[m] Length of hot or cold runner pipe around the power block
        c_trough.m_N_max_hdr_diams = as_double("N_max_hdr_diams");        //[-] Maximum number of allowed diameters in each of the hot and cold headers
        c_trough.m_L_rnr_per_xpan = as_double("L_rnr_per_xpan");          //[m] Threshold length of straight runner pipe without an expansion loop
        c_trough.m_L_xpan_hdr = as_double("L_xpan_hdr");                  //[m] Combined length in meters of the two perpendicular segments of a header expansion loop
        c_trough.m_L_xpan_rnr = as_double("L_xpan_rnr");                  //[m] Combined length in meters of the two perpendicular segments of a runner expansion loop
        c_trough.m_Min_rnr_xpans = as_double("Min_rnr_xpans");            //[-] Minimum number of expansion loops per single-diameter runner section
        c_trough.m_northsouth_field_sep = as_double("northsouth_field_sep"); //[m] Shortest north/south distance between SCAs in different subfields
        c_trough.m_N_hdr_per_xpan = as_double("N_hdr_per_xpan");          //[-] Number of collector loops per header expansion loops. 1 = expansion loop between every collector loop
        c_trough.m_offset_xpan_hdr = as_double("offset_xpan_hdr");        //[-] Location of first header expansion loop. 1 = after first collector loop
        c_trough.m_K_cpnt = as_matrix("K_cpnt");                          //[-] Minor loss coefficients of the components in each loop interconnect
        c_trough.m_D_cpnt = as_matrix("D_cpnt");                          //[m] Inner diameters of the components in each loop interconnect
        c_trough.m_L_cpnt = as_matrix("L_cpnt");                          //[m] Lengths of the components in each loop interconnect
        c_trough.m_Type_cpnt = as_matrix("Type_cpnt");                    //[-] Type of component in each loop interconnect [0=fitting | 1=pipe | 2=flex_hose]
        c_trough.m_custom_sf_pipe_sizes = as_boolean("custom_sf_pipe_sizes"); //[-] Should the field pipe diameters, wall thickness and lengths be imported instead of calculated
        c_trough.m_sf_rnr_diams = as_matrix("sf_rnr_diams");              //[m] Imported runner diameters, used if custom_sf_pipe_sizes is true
        c_trough.m_sf_rnr_wallthicks = as_matrix("sf_rnr_wallthicks");    //[m] Imported runner wall thicknesses, used if custom_sf_pipe_sizes is true
        c_trough.m_sf_rnr_lengths = as_matrix("sf_rnr_lengths");          //[m] Imported runner lengths, used if custom_sf_pipe_sizes is true
        c_trough.m_sf_hdr_diams = as_matrix("sf_hdr_diams");              //[m] Imported header diameters, used if custom_sf_pipe_sizes is true
        c_trough.m_sf_hdr_wallthicks = as_matrix("sf_hdr_wallthicks");    //[m] Imported header wall thicknesses, used if custom_sf_pipe_sizes is true
        c_trough.m_sf_hdr_lengths = as_matrix("sf_hdr_lengths");          //[m] Imported header lengths, used if custom_sf_pipe_sizes is true
        

        //[-] Collector defocus order
        size_t nval_SCADefocusArray = 0;
        ssc_number_t *SCADefocusArray = as_array("SCADefocusArray", &nval_SCADefocusArray);
        c_trough.m_SCADefocusArray.resize(nval_SCADefocusArray);
        for (size_t i = 0; i < nval_SCADefocusArray; i++)
            c_trough.m_SCADefocusArray[i] = (int)SCADefocusArray[i];

        // Allocate trough outputs
        c_trough.mc_reported_outputs.assign(C_csp_trough_collector_receiver::E_THETA_AVE, allocate("Theta_ave", n_steps_fixed), n_steps_fixed);
        c_trough.mc_reported_outputs.assign(C_csp_trough_collector_receiver::E_COSTH_AVE, allocate("CosTh_ave", n_steps_fixed), n_steps_fixed);
        c_trough.mc_reported_outputs.assign(C_csp_trough_collector_receiver::E_IAM_AVE, allocate("IAM_ave", n_steps_fixed), n_steps_fixed);      
        c_trough.mc_reported_outputs.assign(C_csp_trough_collector_receiver::E_ROWSHADOW_AVE, allocate("RowShadow_ave", n_steps_fixed), n_steps_fixed);
        c_trough.mc_reported_outputs.assign(C_csp_trough_collector_receiver::E_ENDLOSS_AVE, allocate("EndLoss_ave", n_steps_fixed), n_steps_fixed);  
        c_trough.mc_reported_outputs.assign(C_csp_trough_collector_receiver::E_DNI_COSTH, allocate("dni_costh", n_steps_fixed), n_steps_fixed);    
        c_trough.mc_reported_outputs.assign(C_csp_trough_collector_receiver::E_EQUIV_OPT_ETA_TOT, allocate("EqOpteff", n_steps_fixed), n_steps_fixed);
        c_trough.mc_reported_outputs.assign(C_csp_trough_collector_receiver::E_DEFOCUS, allocate("SCAs_def", n_steps_fixed), n_steps_fixed);
        
        c_trough.mc_reported_outputs.assign(C_csp_trough_collector_receiver::E_Q_DOT_INC_SF_TOT, allocate("q_inc_sf_tot", n_steps_fixed), n_steps_fixed);
        c_trough.mc_reported_outputs.assign(C_csp_trough_collector_receiver::E_Q_DOT_INC_SF_COSTH, allocate("qinc_costh", n_steps_fixed), n_steps_fixed);  
        c_trough.mc_reported_outputs.assign(C_csp_trough_collector_receiver::E_Q_DOT_REC_INC, allocate("q_dot_rec_inc", n_steps_fixed), n_steps_fixed);
        c_trough.mc_reported_outputs.assign(C_csp_trough_collector_receiver::E_Q_DOT_REC_THERMAL_LOSS, allocate("q_dot_rec_thermal_loss", n_steps_fixed), n_steps_fixed);
        c_trough.mc_reported_outputs.assign(C_csp_trough_collector_receiver::E_Q_DOT_REC_ABS, allocate("q_dot_rec_abs", n_steps_fixed), n_steps_fixed);
        c_trough.mc_reported_outputs.assign(C_csp_trough_collector_receiver::E_Q_DOT_PIPING_LOSS, allocate("q_dot_piping_loss", n_steps_fixed), n_steps_fixed);
        c_trough.mc_reported_outputs.assign(C_csp_trough_collector_receiver::E_E_DOT_INTERNAL_ENERGY, allocate("e_dot_field_int_energy", n_steps_fixed), n_steps_fixed);
        c_trough.mc_reported_outputs.assign(C_csp_trough_collector_receiver::E_Q_DOT_HTF_OUT, allocate("q_dot_htf_sf_out", n_steps_fixed), n_steps_fixed);
        c_trough.mc_reported_outputs.assign(C_csp_trough_collector_receiver::E_Q_DOT_FREEZE_PROT, allocate("q_dot_freeze_prot", n_steps_fixed), n_steps_fixed);

        c_trough.mc_reported_outputs.assign(C_csp_trough_collector_receiver::E_M_DOT_LOOP, allocate("m_dot_loop", n_steps_fixed), n_steps_fixed);
        c_trough.mc_reported_outputs.assign(C_csp_trough_collector_receiver::E_IS_RECIRCULATING, allocate("recirculating", n_steps_fixed), n_steps_fixed);
        c_trough.mc_reported_outputs.assign(C_csp_trough_collector_receiver::E_M_DOT_FIELD_RECIRC, allocate("m_dot_field_recirc", n_steps_fixed), n_steps_fixed);
        c_trough.mc_reported_outputs.assign(C_csp_trough_collector_receiver::E_M_DOT_FIELD_DELIVERED, allocate("m_dot_field_delivered", n_steps_fixed), n_steps_fixed);
        c_trough.mc_reported_outputs.assign(C_csp_trough_collector_receiver::E_T_FIELD_COLD_IN, allocate("T_field_cold_in", n_steps_fixed), n_steps_fixed);
        c_trough.mc_reported_outputs.assign(C_csp_trough_collector_receiver::E_T_REC_COLD_IN, allocate("T_rec_cold_in", n_steps_fixed), n_steps_fixed);
        c_trough.mc_reported_outputs.assign(C_csp_trough_collector_receiver::E_T_REC_HOT_OUT, allocate("T_rec_hot_out", n_steps_fixed), n_steps_fixed);
        c_trough.mc_reported_outputs.assign(C_csp_trough_collector_receiver::E_T_FIELD_HOT_OUT, allocate("T_field_hot_out", n_steps_fixed), n_steps_fixed);
        c_trough.mc_reported_outputs.assign(C_csp_trough_collector_receiver::E_PRESSURE_DROP, allocate("deltaP_field", n_steps_fixed), n_steps_fixed);          //[bar]

        c_trough.mc_reported_outputs.assign(C_csp_trough_collector_receiver::E_W_DOT_SCA_TRACK, allocate("W_dot_sca_track", n_steps_fixed), n_steps_fixed);     //[MWe]
        c_trough.mc_reported_outputs.assign(C_csp_trough_collector_receiver::E_W_DOT_PUMP, allocate("W_dot_field_pump", n_steps_fixed), n_steps_fixed);         //[MWe]

        // ********************************
        // ********************************
        // Power cycle
        // ********************************
        // ********************************
        C_csp_power_cycle * p_csp_power_cycle;
        // Steam Rankine and User Defined power cycle classes
        C_pc_Rankine_indirect_224 rankine_pc;

        int pb_tech_type = as_integer("pc_config");
        if ( !(pb_tech_type == 0 || pb_tech_type == 1) )  // 0 = Rankine, 1 = UDPC
        {
            throw exec_error("trough_physical", "unsupported power cycle");
        }
        else
        {
            C_pc_Rankine_indirect_224::S_params *pc = &rankine_pc.ms_params;
            pc->m_P_ref = as_double("P_ref");
            pc->m_eta_ref = as_double("eta_ref");
            pc->m_T_htf_hot_ref = as_double("T_loop_out");
            pc->m_T_htf_cold_ref = as_double("T_loop_in_des");
            pc->m_cycle_max_frac = as_double("cycle_max_frac");
            pc->m_cycle_cutoff_frac = as_double("cycle_cutoff_frac");
            pc->m_q_sby_frac = as_double("q_sby_frac");
            pc->m_startup_time = as_double("startup_time");
            pc->m_startup_frac = as_double("startup_frac");
            pc->m_htf_pump_coef = as_double("pb_pump_coef");
            pc->m_pc_fl = as_integer("Fluid");                            // power cycle HTF is same as receiver HTF
            pc->m_pc_fl_props = as_matrix("field_fl_props");
            pc->DP_SGS = as_double("DP_SGS");

            if (pb_tech_type == 0)
            {
                pc->m_dT_cw_ref = as_double("dT_cw_ref");
                pc->m_T_amb_des = as_double("T_amb_des");
                pc->m_P_boil = as_double("P_boil");
                pc->m_CT = as_integer("CT");                    // cooling tech type: 1=evaporative, 2=air, 3=hybrid    
                pc->m_tech_type = as_integer("tech_type");      // turbine inlet pressure: 1: Fixed, 3: Sliding
                if (pc->m_tech_type == 1) { pc->m_tech_type = 2; }; // changing fixed pressure for the tower to fixed pressure for the trough
                if (pc->m_tech_type == 3) { pc->m_tech_type = 8; }; // changing sliding pressure for the tower to sliding pressure for the trough
                if (!(pc->m_tech_type == 2 || pc->m_tech_type == 5 || pc->m_tech_type == 6 || pc->m_tech_type == 8))
                {
                    std::string tech_msg = util::format("tech_type must be either 2 (fixed pressure) or 8 (sliding). Input was %d."
                        " Simulation proceeded with fixed pressure", pc->m_tech_type);
                    pc->m_tech_type = 2;
                }
                pc->m_T_approach = as_double("T_approach");
                pc->m_T_ITD_des = as_double("T_ITD_des");
                pc->m_P_cond_ratio = as_double("P_cond_ratio");
                pc->m_pb_bd_frac = as_double("pb_bd_frac");
                pc->m_P_cond_min = as_double("P_cond_min");
                pc->m_n_pl_inc = as_integer("n_pl_inc");

                size_t n_F_wc = 0;
                ssc_number_t *p_F_wc = as_array("F_wc", &n_F_wc);
                pc->m_F_wc.resize(n_F_wc, 0.0);
                for (size_t i = 0; i < n_F_wc; i++)
                    pc->m_F_wc[i] = (double)p_F_wc[i];

                pc->m_is_user_defined_pc = false;
                pc->m_W_dot_cooling_des = std::numeric_limits<double>::quiet_NaN();
            }
            else if (pb_tech_type == 1)
            {
                pc->m_is_user_defined_pc = true;

                // User-Defined Cycle Parameters
                pc->m_W_dot_cooling_des = as_double("ud_f_W_dot_cool_des") / 100.0*as_double("P_ref");  //[MWe]
                pc->m_m_dot_water_des = as_double("ud_m_dot_water_cool_des");       //[kg/s]

                // User-Defined Cycle Off-Design Tables 
                pc->mc_combined_ind = as_matrix("ud_ind_od");
            }

            // Set pointer to parent class
            p_csp_power_cycle = &rankine_pc;

            // Set power cycle outputs common to all power cycle technologies
            p_csp_power_cycle->assign(C_pc_Rankine_indirect_224::E_ETA_THERMAL, allocate("eta", n_steps_fixed), n_steps_fixed);
            p_csp_power_cycle->assign(C_pc_Rankine_indirect_224::E_Q_DOT_HTF, allocate("q_pb", n_steps_fixed), n_steps_fixed);
            p_csp_power_cycle->assign(C_pc_Rankine_indirect_224::E_M_DOT_HTF, allocate("m_dot_pc", n_steps_fixed), n_steps_fixed);
            p_csp_power_cycle->assign(C_pc_Rankine_indirect_224::E_Q_DOT_STARTUP, allocate("q_dot_pc_startup", n_steps_fixed), n_steps_fixed);
            p_csp_power_cycle->assign(C_pc_Rankine_indirect_224::E_W_DOT, allocate("P_cycle", n_steps_fixed), n_steps_fixed);
            p_csp_power_cycle->assign(C_pc_Rankine_indirect_224::E_T_HTF_IN, allocate("T_pc_in", n_steps_fixed), n_steps_fixed);
            p_csp_power_cycle->assign(C_pc_Rankine_indirect_224::E_T_HTF_OUT, allocate("T_pc_out", n_steps_fixed), n_steps_fixed);
            p_csp_power_cycle->assign(C_pc_Rankine_indirect_224::E_M_DOT_WATER, allocate("m_dot_water_pc", n_steps_fixed), n_steps_fixed);
        }


        // ********************************
        // ********************************
        // TES
        // ********************************
        // ********************************
        C_csp_two_tank_tes storage;
        C_csp_two_tank_tes::S_params *tes = &storage.ms_params;
        tes->m_field_fl           = c_trough.m_Fluid;                       //[-]
        tes->m_field_fl_props     = c_trough.m_field_fl_props;              //[-]
        tes->m_tes_fl             = as_integer("store_fluid");              //[-]
        tes->m_tes_fl_props       = as_matrix("store_fl_props");            //[-]
        tes->m_is_hx              = as_boolean("is_hx");                    //[-]
        tes->m_W_dot_pc_design    = as_double("P_ref");                     //[MWt]
        tes->m_eta_pc             = as_double("eta_ref");                   //[-]
        tes->m_solarm             = as_double("solar_mult");                //[-]
        tes->m_ts_hours           = as_double("tshours");                   //[hr]
        tes->m_h_tank             = as_double("h_tank");                    //[m]
        tes->m_u_tank             = as_double("u_tank");                    //[W/m^2-K]
        tes->m_tank_pairs         = as_integer("tank_pairs");               //[-]
        tes->m_hot_tank_Thtr      = as_double("hot_tank_Thtr");             //[C]
        tes->m_hot_tank_max_heat  = as_double("hot_tank_max_heat");         //[MWe]
        tes->m_cold_tank_Thtr     = as_double("cold_tank_Thtr");            //[C]
        tes->m_cold_tank_max_heat = as_double("cold_tank_max_heat");        //[MWe]
        tes->m_dt_hot             = as_double("dt_hot");                    //[C]
        //tes->m_dt_cold            = as_double("dt_cold");                   //[C]
        tes->m_T_field_in_des     = T_loop_in_des;                          //[C]
        tes->m_T_field_out_des    = T_loop_out_des;                         //[C]
        tes->m_T_tank_hot_ini     = T_loop_out_des;                         //[C]
        tes->m_T_tank_cold_ini    = T_loop_in_des;                          //[C]
        tes->m_h_tank_min         = as_double("h_tank_min");                //[m]
        tes->m_f_V_hot_ini        = as_double("init_hot_htf_percent");      //[-]
        tes->m_htf_pump_coef      = as_double("pb_pump_coef");              //[kWe/kg/s]
        tes->m_tes_pump_coef      = as_double("tes_pump_coef");             //[kWe/kg/s]
        tes->eta_pump             = as_double("eta_pump");                  //[-]
        tes->tanks_in_parallel    = as_boolean("tanks_in_parallel");        //[-]
        tes->has_hot_tank_bypass  = as_boolean("has_hot_tank_bypass");      //[-]
        tes->T_tank_hot_inlet_min = as_double("T_tank_hot_inlet_min");      //[C]
        tes->V_tes_des            = as_double("V_tes_des");                 //[m/s]
        tes->custom_tes_p_loss    = as_boolean("custom_tes_p_loss");        //[-]
        tes->k_tes_loss_coeffs    = as_matrix("k_tes_loss_coeffs");         //[-]
        tes->custom_tes_pipe_sizes = as_boolean("custom_tes_pipe_sizes");   //[-]
        tes->tes_diams            = as_matrix("tes_diams");                 //[m]
        tes->tes_wallthicks       = as_matrix("tes_wallthicks");            //[m]
        tes->tes_lengths          = as_matrix("tes_lengths");               //[m]
        tes->calc_design_pipe_vals = as_boolean("calc_design_pipe_vals");   //[-]
        tes->pipe_rough           = as_double("HDR_rough");                 //[m]
        tes->DP_SGS               = as_double("DP_SGS");                    //[bar]

        // Set storage outputs
        storage.mc_reported_outputs.assign(C_csp_two_tank_tes::E_Q_DOT_LOSS, allocate("tank_losses", n_steps_fixed), n_steps_fixed);
        storage.mc_reported_outputs.assign(C_csp_two_tank_tes::E_W_DOT_HEATER, allocate("q_tes_heater", n_steps_fixed), n_steps_fixed);
        storage.mc_reported_outputs.assign(C_csp_two_tank_tes::E_TES_T_HOT, allocate("T_tes_hot", n_steps_fixed), n_steps_fixed);
        storage.mc_reported_outputs.assign(C_csp_two_tank_tes::E_TES_T_COLD, allocate("T_tes_cold", n_steps_fixed), n_steps_fixed);
        storage.mc_reported_outputs.assign(C_csp_two_tank_tes::E_M_DOT_TANK_TO_TANK, allocate("m_dot_cold_tank_to_hot_tank", n_steps_fixed), n_steps_fixed);
        storage.mc_reported_outputs.assign(C_csp_two_tank_tes::E_MASS_COLD_TANK, allocate("mass_tes_cold", n_steps_fixed), n_steps_fixed);
        storage.mc_reported_outputs.assign(C_csp_two_tank_tes::E_MASS_HOT_TANK, allocate("mass_tes_hot", n_steps_fixed), n_steps_fixed);


        // ********************************
        // ********************************
        // TOU
        // ********************************
        // ********************************
        C_csp_tou_block_schedules tou;
        C_csp_tou_block_schedules::S_params *tou_params = &tou.ms_params;
        tou_params->mc_csp_ops.mc_weekdays         = as_matrix("weekday_schedule");
        tou_params->mc_csp_ops.mc_weekends         = as_matrix("weekend_schedule");
        tou_params->mc_pricing.mc_weekdays         = as_matrix("dispatch_sched_weekday");
        tou_params->mc_pricing.mc_weekends         = as_matrix("dispatch_sched_weekend");
        if (tou_params->mc_pricing.mc_weekdays.ncells() == 1) {
            // Resize default value from var table to proper dimensions
            tou_params->mc_pricing.mc_weekdays = util::matrix_t<double>(12, 24, 1.0);
        }
        if (tou_params->mc_pricing.mc_weekends.ncells() == 1) {
            // Resize default value from var table to proper dimensions
            tou_params->mc_pricing.mc_weekends = util::matrix_t<double>(12, 24, 1.0);
        }
        tou.mc_dispatch_params.m_dispatch_optimize = as_boolean("is_dispatch");
        tou.mc_dispatch_params.m_is_write_ampl_dat = as_boolean("is_write_ampl_dat");
        tou.mc_dispatch_params.m_is_ampl_engine    = as_boolean("is_ampl_engine");
        tou.mc_dispatch_params.m_ampl_data_dir     = as_string("ampl_data_dir");
        tou.mc_dispatch_params.m_ampl_exec_call    = as_string("ampl_exec_call");
        if (tou.mc_dispatch_params.m_dispatch_optimize)
        {
            tou.mc_dispatch_params.m_optimize_frequency  = as_integer("disp_frequency");
            tou.mc_dispatch_params.m_disp_steps_per_hour = as_integer("disp_steps_per_hour");
            tou.mc_dispatch_params.m_optimize_horizon    = as_integer("disp_horizon");
            tou.mc_dispatch_params.m_max_iterations      = as_integer("disp_max_iter");
            tou.mc_dispatch_params.m_solver_timeout      = as_double("disp_timeout");
            tou.mc_dispatch_params.m_mip_gap             = as_double("disp_mip_gap");
            tou.mc_dispatch_params.m_presolve_type       = as_integer("disp_spec_presolve");
            tou.mc_dispatch_params.m_bb_type             = as_integer("disp_spec_bb");
            tou.mc_dispatch_params.m_disp_reporting      = as_integer("disp_reporting");
            tou.mc_dispatch_params.m_scaling_type        = as_integer("disp_spec_scaling");
            tou.mc_dispatch_params.m_disp_time_weighting = as_double("disp_time_weighting");
            tou.mc_dispatch_params.m_rsu_cost            = as_double("disp_rsu_cost");
            tou.mc_dispatch_params.m_csu_cost            = as_double("disp_csu_cost");
            tou.mc_dispatch_params.m_pen_delta_w         = as_double("disp_pen_delta_w");
            tou.mc_dispatch_params.m_q_rec_standby       = as_double("q_rec_standby");
            tou.mc_dispatch_params.m_w_rec_ht            = as_double("q_rec_heattrace");


            if (as_boolean("is_wlim_series"))
            {
                size_t n_wlim_series = 0;
                ssc_number_t* wlim_series = as_array("wlim_series", &n_wlim_series);
                if ((int)n_wlim_series != n_wf_records)
                    throw exec_error("trough_physical", "Invalid net electricity generation limit series dimension. Matrix must have " + util::to_string(n_wf_records) + " rows.");
                for (int i = 0; i < n_wf_records; i++)
                    tou.mc_dispatch_params.m_w_lim_full.at(i) = (double)wlim_series[i];
            }


        }
        tou.mc_dispatch_params.m_is_tod_pc_target_also_pc_max = as_boolean("is_tod_pc_target_also_pc_max");
        tou.mc_dispatch_params.m_is_block_dispatch    = !tou.mc_dispatch_params.m_dispatch_optimize;      //mw
        tou.mc_dispatch_params.m_use_rule_1           = true;
        tou.mc_dispatch_params.m_standby_off_buffer   = 2.0;
        tou.mc_dispatch_params.m_use_rule_2           = false;
        tou.mc_dispatch_params.m_q_dot_rec_des_mult   = -1.23;
        tou.mc_dispatch_params.m_f_q_dot_pc_overwrite = -1.23;

        size_t n_f_turbine = 0;
        ssc_number_t *p_f_turbine = as_array("f_turb_tou_periods", &n_f_turbine);
        tou_params->mc_csp_ops.mvv_tou_arrays[C_block_schedule_csp_ops::TURB_FRAC].resize(n_f_turbine, 0.0);
        //tou_params->mv_t_frac.resize(n_f_turbine, 0.0);
        for (size_t i = 0; i < n_f_turbine; i++)
            tou_params->mc_csp_ops.mvv_tou_arrays[C_block_schedule_csp_ops::TURB_FRAC][i] = (double)p_f_turbine[i];

        bool is_timestep_input = (as_integer("ppa_multiplier_model") == 1);
        tou_params->mc_pricing.mv_is_diurnal = !(is_timestep_input);
        if (is_timestep_input)
        {
            size_t nmultipliers;
            ssc_number_t *multipliers = as_array("dispatch_factors_ts", &nmultipliers);
            tou_params->mc_pricing.mvv_tou_arrays[C_block_schedule_pricing::MULT_PRICE].resize(nmultipliers, 0.0);
            for (size_t ii = 0; ii < nmultipliers; ii++)
                tou_params->mc_pricing.mvv_tou_arrays[C_block_schedule_pricing::MULT_PRICE][ii] = multipliers[ii];
        }
        else // standard diuranal input
        {
            tou_params->mc_pricing.mvv_tou_arrays[C_block_schedule_pricing::MULT_PRICE].resize(9, 0.0);
            tou_params->mc_pricing.mvv_tou_arrays[C_block_schedule_pricing::MULT_PRICE][0] = as_double("dispatch_factor1");
            tou_params->mc_pricing.mvv_tou_arrays[C_block_schedule_pricing::MULT_PRICE][1] = as_double("dispatch_factor2");
            tou_params->mc_pricing.mvv_tou_arrays[C_block_schedule_pricing::MULT_PRICE][2] = as_double("dispatch_factor3");
            tou_params->mc_pricing.mvv_tou_arrays[C_block_schedule_pricing::MULT_PRICE][3] = as_double("dispatch_factor4");
            tou_params->mc_pricing.mvv_tou_arrays[C_block_schedule_pricing::MULT_PRICE][4] = as_double("dispatch_factor5");
            tou_params->mc_pricing.mvv_tou_arrays[C_block_schedule_pricing::MULT_PRICE][5] = as_double("dispatch_factor6");
            tou_params->mc_pricing.mvv_tou_arrays[C_block_schedule_pricing::MULT_PRICE][6] = as_double("dispatch_factor7");
            tou_params->mc_pricing.mvv_tou_arrays[C_block_schedule_pricing::MULT_PRICE][7] = as_double("dispatch_factor8");
            tou_params->mc_pricing.mvv_tou_arrays[C_block_schedule_pricing::MULT_PRICE][8] = as_double("dispatch_factor9");
        }
        
        // System parameters
        C_csp_solver::S_csp_system_params system;
        system.m_pb_fixed_par = as_double("pb_fixed_par");
        size_t nval_bop_array = 0;
        ssc_number_t *bop_array = as_array("bop_array", &nval_bop_array);
        if (nval_bop_array != 5) throw exec_error("trough_physical", "Should be 5 elements in bop_array, has " + util::to_string((int)nval_bop_array) + ".");
        system.m_bop_par      = bop_array[0];    //as_double("bop_par");
        system.m_bop_par_f    = bop_array[1];    //as_double("bop_par_f");
        system.m_bop_par_0    = bop_array[2];    //as_double("bop_par_0");
        system.m_bop_par_1    = bop_array[3];    //as_double("bop_par_1");
        system.m_bop_par_2    = bop_array[4];    //as_double("bop_par_2");

        // Instantiate Solver
        C_csp_solver csp_solver(weather_reader, 
                                c_trough,
                                *p_csp_power_cycle,
                                storage, 
                                tou, 
                                system,
                                ssc_cmod_update,
                                (void*)(this));

        // Set solver reporting outputs
        // Simulation Kernel
        csp_solver.mc_reported_outputs.assign(C_csp_solver::C_solver_outputs::TIME_FINAL, allocate("time_hr", n_steps_fixed), n_steps_fixed);
        // Weather reader
        csp_solver.mc_reported_outputs.assign(C_csp_solver::C_solver_outputs::MONTH, allocate("month", n_steps_fixed), n_steps_fixed);
        csp_solver.mc_reported_outputs.assign(C_csp_solver::C_solver_outputs::HOUR_DAY, allocate("hour_day", n_steps_fixed), n_steps_fixed);
        csp_solver.mc_reported_outputs.assign(C_csp_solver::C_solver_outputs::SOLAZ, allocate("solazi", n_steps_fixed), n_steps_fixed);
        csp_solver.mc_reported_outputs.assign(C_csp_solver::C_solver_outputs::SOLZEN, allocate("solzen", n_steps_fixed), n_steps_fixed);
        csp_solver.mc_reported_outputs.assign(C_csp_solver::C_solver_outputs::BEAM, allocate("beam", n_steps_fixed), n_steps_fixed);
        csp_solver.mc_reported_outputs.assign(C_csp_solver::C_solver_outputs::TDRY, allocate("tdry", n_steps_fixed), n_steps_fixed);
        csp_solver.mc_reported_outputs.assign(C_csp_solver::C_solver_outputs::TWET, allocate("twet", n_steps_fixed), n_steps_fixed);
        csp_solver.mc_reported_outputs.assign(C_csp_solver::C_solver_outputs::RH, allocate("RH", n_steps_fixed), n_steps_fixed);
        csp_solver.mc_reported_outputs.assign(C_csp_solver::C_solver_outputs::WSPD, allocate("wspd", n_steps_fixed), n_steps_fixed);
        csp_solver.mc_reported_outputs.assign(C_csp_solver::C_solver_outputs::PRES, allocate("pres", n_steps_fixed), n_steps_fixed);

        csp_solver.mc_reported_outputs.assign(C_csp_solver::C_solver_outputs::CR_DEFOCUS, allocate("defocus", n_steps_fixed), n_steps_fixed);
        // TES
        csp_solver.mc_reported_outputs.assign(C_csp_solver::C_solver_outputs::TES_Q_DOT_DC, allocate("q_dc_tes", n_steps_fixed), n_steps_fixed);
        csp_solver.mc_reported_outputs.assign(C_csp_solver::C_solver_outputs::TES_Q_DOT_CH, allocate("q_ch_tes", n_steps_fixed), n_steps_fixed);
        csp_solver.mc_reported_outputs.assign(C_csp_solver::C_solver_outputs::TES_E_CH_STATE, allocate("e_ch_tes", n_steps_fixed), n_steps_fixed);
        csp_solver.mc_reported_outputs.assign(C_csp_solver::C_solver_outputs::M_DOT_CR_TO_TES_HOT, allocate("m_dot_cr_to_tes_hot", n_steps_fixed), n_steps_fixed);
        csp_solver.mc_reported_outputs.assign(C_csp_solver::C_solver_outputs::M_DOT_TES_HOT_OUT, allocate("m_dot_tes_hot_out", n_steps_fixed), n_steps_fixed);
        csp_solver.mc_reported_outputs.assign(C_csp_solver::C_solver_outputs::M_DOT_PC_TO_TES_COLD, allocate("m_dot_pc_to_tes_cold", n_steps_fixed), n_steps_fixed);
        csp_solver.mc_reported_outputs.assign(C_csp_solver::C_solver_outputs::M_DOT_TES_COLD_OUT, allocate("m_dot_tes_cold_out", n_steps_fixed), n_steps_fixed);
        csp_solver.mc_reported_outputs.assign(C_csp_solver::C_solver_outputs::M_DOT_FIELD_TO_CYCLE, allocate("m_dot_field_to_cycle", n_steps_fixed), n_steps_fixed);
        csp_solver.mc_reported_outputs.assign(C_csp_solver::C_solver_outputs::M_DOT_CYCLE_TO_FIELD, allocate("m_dot_cycle_to_field", n_steps_fixed), n_steps_fixed);
        // System
        csp_solver.mc_reported_outputs.assign(C_csp_solver::C_solver_outputs::COL_W_DOT_TRACK, allocate("pparasi", n_steps_fixed), n_steps_fixed);
        csp_solver.mc_reported_outputs.assign(C_csp_solver::C_solver_outputs::SYS_W_DOT_PUMP, allocate("htf_pump_power", n_steps_fixed), n_steps_fixed);
        csp_solver.mc_reported_outputs.assign(C_csp_solver::C_solver_outputs::PC_W_DOT_COOLING, allocate("P_cooling_tower_tot", n_steps_fixed), n_steps_fixed);
        csp_solver.mc_reported_outputs.assign(C_csp_solver::C_solver_outputs::SYS_W_DOT_FIXED, allocate("P_fixed", n_steps_fixed), n_steps_fixed);
        csp_solver.mc_reported_outputs.assign(C_csp_solver::C_solver_outputs::SYS_W_DOT_BOP, allocate("P_plant_balance_tot", n_steps_fixed), n_steps_fixed);
        csp_solver.mc_reported_outputs.assign(C_csp_solver::C_solver_outputs::W_DOT_NET, allocate("P_out_net", n_steps_fixed), n_steps_fixed);
        // Controller
        csp_solver.mc_reported_outputs.assign(C_csp_solver::C_solver_outputs::OP_MODE_1, allocate("op_mode_1", n_steps_fixed), n_steps_fixed);
        csp_solver.mc_reported_outputs.assign(C_csp_solver::C_solver_outputs::OP_MODE_2, allocate("op_mode_2", n_steps_fixed), n_steps_fixed);
        csp_solver.mc_reported_outputs.assign(C_csp_solver::C_solver_outputs::OP_MODE_3, allocate("op_mode_3", n_steps_fixed), n_steps_fixed);
        csp_solver.mc_reported_outputs.assign(C_csp_solver::C_solver_outputs::ERR_M_DOT, allocate("m_dot_balance", n_steps_fixed), n_steps_fixed);
        csp_solver.mc_reported_outputs.assign(C_csp_solver::C_solver_outputs::ERR_Q_DOT, allocate("q_balance", n_steps_fixed), n_steps_fixed);

        csp_solver.mc_reported_outputs.assign(C_csp_solver::C_solver_outputs::N_OP_MODES, allocate("n_op_modes", n_steps_fixed), n_steps_fixed);
        csp_solver.mc_reported_outputs.assign(C_csp_solver::C_solver_outputs::TOU_PERIOD, allocate("tou_value", n_steps_fixed), n_steps_fixed);
        csp_solver.mc_reported_outputs.assign(C_csp_solver::C_solver_outputs::PRICING_MULT, allocate("pricing_mult", n_steps_fixed), n_steps_fixed);
        csp_solver.mc_reported_outputs.assign(C_csp_solver::C_solver_outputs::PC_Q_DOT_SB, allocate("q_dot_pc_sb", n_steps_fixed), n_steps_fixed);
        csp_solver.mc_reported_outputs.assign(C_csp_solver::C_solver_outputs::PC_Q_DOT_MIN, allocate("q_dot_pc_min", n_steps_fixed), n_steps_fixed);
        csp_solver.mc_reported_outputs.assign(C_csp_solver::C_solver_outputs::PC_Q_DOT_TARGET, allocate("q_dot_pc_target", n_steps_fixed), n_steps_fixed);
        csp_solver.mc_reported_outputs.assign(C_csp_solver::C_solver_outputs::PC_Q_DOT_MAX, allocate("q_dot_pc_max", n_steps_fixed), n_steps_fixed);

        csp_solver.mc_reported_outputs.assign(C_csp_solver::C_solver_outputs::CTRL_IS_REC_SU, allocate("is_rec_su_allowed", n_steps_fixed), n_steps_fixed);
        csp_solver.mc_reported_outputs.assign(C_csp_solver::C_solver_outputs::CTRL_IS_PC_SU, allocate("is_pc_su_allowed", n_steps_fixed), n_steps_fixed);
        csp_solver.mc_reported_outputs.assign(C_csp_solver::C_solver_outputs::CTRL_IS_PC_SB, allocate("is_pc_sb_allowed", n_steps_fixed), n_steps_fixed);
        csp_solver.mc_reported_outputs.assign(C_csp_solver::C_solver_outputs::EST_Q_DOT_CR_SU, allocate("q_dot_est_cr_su", n_steps_fixed), n_steps_fixed);
        csp_solver.mc_reported_outputs.assign(C_csp_solver::C_solver_outputs::EST_Q_DOT_CR_ON, allocate("q_dot_est_cr_on", n_steps_fixed), n_steps_fixed);
        csp_solver.mc_reported_outputs.assign(C_csp_solver::C_solver_outputs::EST_Q_DOT_DC, allocate("q_dot_est_tes_dc", n_steps_fixed), n_steps_fixed);
        csp_solver.mc_reported_outputs.assign(C_csp_solver::C_solver_outputs::EST_Q_DOT_CH, allocate("q_dot_est_tes_ch", n_steps_fixed), n_steps_fixed);

        csp_solver.mc_reported_outputs.assign(C_csp_solver::C_solver_outputs::CTRL_OP_MODE_SEQ_A, allocate("operating_modes_a", n_steps_fixed), n_steps_fixed);
        csp_solver.mc_reported_outputs.assign(C_csp_solver::C_solver_outputs::CTRL_OP_MODE_SEQ_B, allocate("operating_modes_b", n_steps_fixed), n_steps_fixed);
        csp_solver.mc_reported_outputs.assign(C_csp_solver::C_solver_outputs::CTRL_OP_MODE_SEQ_C, allocate("operating_modes_c", n_steps_fixed), n_steps_fixed);

        csp_solver.mc_reported_outputs.assign(C_csp_solver::C_solver_outputs::DISPATCH_SOLVE_STATE, allocate("disp_solve_state", n_steps_fixed), n_steps_fixed);
        csp_solver.mc_reported_outputs.assign(C_csp_solver::C_solver_outputs::DISPATCH_SOLVE_ITER, allocate("disp_solve_iter", n_steps_fixed), n_steps_fixed);
        csp_solver.mc_reported_outputs.assign(C_csp_solver::C_solver_outputs::DISPATCH_SOLVE_OBJ, allocate("disp_objective", n_steps_fixed), n_steps_fixed);
        csp_solver.mc_reported_outputs.assign(C_csp_solver::C_solver_outputs::DISPATCH_SOLVE_OBJ_RELAX, allocate("disp_obj_relax", n_steps_fixed), n_steps_fixed);
        csp_solver.mc_reported_outputs.assign(C_csp_solver::C_solver_outputs::DISPATCH_QSF_EXPECT, allocate("disp_qsf_expected", n_steps_fixed), n_steps_fixed);
        csp_solver.mc_reported_outputs.assign(C_csp_solver::C_solver_outputs::DISPATCH_QSFPROD_EXPECT, allocate("disp_qsfprod_expected", n_steps_fixed), n_steps_fixed);
        csp_solver.mc_reported_outputs.assign(C_csp_solver::C_solver_outputs::DISPATCH_QSFSU_EXPECT, allocate("disp_qsfsu_expected", n_steps_fixed), n_steps_fixed);
        csp_solver.mc_reported_outputs.assign(C_csp_solver::C_solver_outputs::DISPATCH_TES_EXPECT, allocate("disp_tes_expected", n_steps_fixed), n_steps_fixed);
        csp_solver.mc_reported_outputs.assign(C_csp_solver::C_solver_outputs::DISPATCH_PCEFF_EXPECT, allocate("disp_pceff_expected", n_steps_fixed), n_steps_fixed);
        csp_solver.mc_reported_outputs.assign(C_csp_solver::C_solver_outputs::DISPATCH_SFEFF_EXPECT, allocate("disp_thermeff_expected", n_steps_fixed), n_steps_fixed);
        csp_solver.mc_reported_outputs.assign(C_csp_solver::C_solver_outputs::DISPATCH_QPBSU_EXPECT, allocate("disp_qpbsu_expected", n_steps_fixed), n_steps_fixed);
        csp_solver.mc_reported_outputs.assign(C_csp_solver::C_solver_outputs::DISPATCH_WPB_EXPECT, allocate("disp_wpb_expected", n_steps_fixed), n_steps_fixed);
        csp_solver.mc_reported_outputs.assign(C_csp_solver::C_solver_outputs::DISPATCH_REV_EXPECT, allocate("disp_rev_expected", n_steps_fixed), n_steps_fixed);
        csp_solver.mc_reported_outputs.assign(C_csp_solver::C_solver_outputs::DISPATCH_PRES_NCONSTR, allocate("disp_presolve_nconstr", n_steps_fixed), n_steps_fixed);
        csp_solver.mc_reported_outputs.assign(C_csp_solver::C_solver_outputs::DISPATCH_PRES_NVAR, allocate("disp_presolve_nvar", n_steps_fixed), n_steps_fixed);
        csp_solver.mc_reported_outputs.assign(C_csp_solver::C_solver_outputs::DISPATCH_SOLVE_TIME, allocate("disp_solve_time", n_steps_fixed), n_steps_fixed);


        update("Initialize physical trough model...", 0.0);

        int out_type = -1;
        std::string out_msg = "";
        try
        {
            // Initialize Solver
            csp_solver.init();
        }
        catch( C_csp_exception &csp_exception )
        {
            // Report warning before exiting with error
            while( csp_solver.mc_csp_messages.get_message(&out_type, &out_msg) )
            {
                log(out_msg, out_type);
            }

            throw exec_error("trough_physical", csp_exception.m_error_message);
        }

        // If no exception, then report messages
        while (csp_solver.mc_csp_messages.get_message(&out_type, &out_msg))
        {
            log(out_msg, out_type);
        }


        //if the pricing schedule is provided as hourly, overwrite the tou schedule
        if (as_boolean("is_dispatch_series"))
        {
            size_t n_dispatch_series;
            ssc_number_t *dispatch_series = as_array("dispatch_series", &n_dispatch_series);

            //if( n_dispatch_series != n_steps_fixed)
            //    throw exec_error("trough_physical", "Invalid dispatch pricing series dimension. Array length must match number of simulation time steps ("+my_to_string(n_steps_fixed)+").");

            //resize the m_hr_tou array
            if (tou_params->mc_pricing.m_hr_tou != 0)
                delete[] tou_params->mc_pricing.m_hr_tou;
            tou_params->mc_pricing.m_hr_tou = new double[n_steps_fixed];
            //set the tou period as unique for each time step
            for (int i = 0; i < n_steps_fixed; i++)
                tou_params->mc_pricing.m_hr_tou[i] = i + 1;
            //allocate reported arrays
            tou_params->mc_pricing.mvv_tou_arrays[C_block_schedule_pricing::MULT_PRICE].resize(n_steps_fixed);
            for (int i = 0; i < n_steps_fixed; i++)
                tou_params->mc_pricing.mvv_tou_arrays[C_block_schedule_pricing::MULT_PRICE][i] = dispatch_series[i];
        }

        update("Begin timeseries simulation...", 0.0);

        std::clock_t clock_start = std::clock();

        try
        {
            // Simulate !
            csp_solver.Ssimulate(sim_setup);
        }
        catch( C_csp_exception &csp_exception )
        {
            // Report warning before exiting with error
            while( csp_solver.mc_csp_messages.get_message(&out_type, &out_msg) )
            {
                log(out_msg);
            }

            throw exec_error("trough_physical", csp_exception.m_error_message);
        }

        // If no exception, then report messages
        while( csp_solver.mc_csp_messages.get_message(&out_type, &out_msg) )
        {
            log(out_msg, out_type);
        }

        std::clock_t clock_end = std::clock();
        double sim_duration = (clock_end - clock_start) / (double)CLOCKS_PER_SEC;		//[s]
        assign("sim_duration", (ssc_number_t)sim_duration);     

        // Do unit post-processing here
        double *p_q_pc_startup = allocate("q_pc_startup", n_steps_fixed);
        size_t count_pc_su = 0;
        ssc_number_t *p_q_dot_pc_startup = as_array("q_dot_pc_startup", &count_pc_su);
        if ((int)count_pc_su != n_steps_fixed)
        {
            log("q_dot_pc_startup array is a different length than 'n_steps_fixed'.", SSC_WARNING);
            return;
        }
        for (int i = 0; i < n_steps_fixed; i++)
        {
            p_q_pc_startup[i] = (float)(p_q_dot_pc_startup[i] * (sim_setup.m_report_step / 3600.0));    //[MWh]
        }

        // Convert mass flow rates from [kg/hr] to [kg/s]
        size_t count_m_dot_pc, count_m_dot_water_pc; //count_m_dot_rec, count_m_dot_tes_dc, count_m_dot_tes_ch;
        count_m_dot_pc = count_m_dot_water_pc = 0; //count_m_dot_rec = count_m_dot_tes_dc = count_m_dot_tes_ch = 0;
        ssc_number_t *p_m_dot_pc = as_array("m_dot_pc", &count_m_dot_pc);
        ssc_number_t *p_m_dot_water_pc = as_array("m_dot_water_pc", &count_m_dot_water_pc);
        //ssc_number_t *p_m_dot_tes_dc = as_array("m_dot_tes_dc", &count_m_dot_tes_dc);
        //ssc_number_t *p_m_dot_tes_ch = as_array("m_dot_tes_ch", &count_m_dot_tes_ch);
        if ((int)count_m_dot_pc != n_steps_fixed || (int)count_m_dot_water_pc != n_steps_fixed)
            //|| count_m_dot_rec != n_steps_fixed || count_m_dot_tes_dc != n_steps_fixed || count_m_dot_tes_ch != n_steps_fixed)
        {
            log("At least one m_dot array is a different length than 'n_steps_fixed'.", SSC_WARNING);
            return;
        }
        for (int i = 0; i < n_steps_fixed; i++)
        {
            //p_m_dot_rec[i] = (ssc_number_t)(p_m_dot_rec[i] / 3600.0); //[kg/s] convert from kg/hr
            p_m_dot_pc[i] = (ssc_number_t)(p_m_dot_pc[i] / 3600.0);     //[kg/s] convert from kg/hr
            p_m_dot_water_pc[i] = (ssc_number_t)(p_m_dot_water_pc[i] / 3600.0); //[kg/s] convert from kg/hr
            //p_m_dot_tes_dc[i] = (ssc_number_t)(p_m_dot_tes_dc[i] / 3600.0);     //[kg/s] convert from kg/hr
            //p_m_dot_tes_ch[i] = (ssc_number_t)(p_m_dot_tes_ch[i] / 3600.0);     //[kg/s] convert from kg/hr
        }


        size_t count;
        ssc_number_t *p_W_dot_net = as_array("P_out_net", &count);
        ssc_number_t *p_time_final_hr = as_array("time_hr", &count);
        if((int)count != n_steps_fixed)
            throw exec_error("trough_physical", "The number of fixed steps does not match the length of output data arrays");

        // 'adjustment_factors' class stores factors in hourly array, so need to index as such
        adjustment_factors haf(this, "adjust");
        if( !haf.setup() )
            throw exec_error("trough_physical", "failed to setup adjustment factors: " + haf.error());

        ssc_number_t *p_gen = allocate("gen", n_steps_fixed);
        //ssc_number_t *p_W_dot_par_tot_haf = allocate("W_dot_par_tot_haf", n_steps_fixed);
        ssc_number_t *p_q_dot_defocus_est = allocate("q_dot_defocus_est", n_steps_fixed);

        //ssc_number_t *p_W_dot_parasitic_tot = as_array("W_dot_parasitic_tot", &count);
        //if (count != n_steps_fixed)
        //    throw exec_error("trough_physical", "The number of fixed steps does not match the length of output data arrays1");
        
        ssc_number_t *p_SCAs_def = as_array("SCAs_def", &count);
        if ((int)count != n_steps_fixed)
            throw exec_error("trough_physical", "The number of fixed steps does not match the length of output data arrays2");

        ssc_number_t *p_q_dot_htf_sf_out = as_array("q_dot_htf_sf_out", &count);
        if ((int)count != n_steps_fixed)
            throw exec_error("trough_physical", "The number of fixed steps does not match the length of output data arrays3");

        //ssc_number_t *p_m_dot_tes_dc = as_array("m_dot_tes_dc", &count);
        //if ((int)count != n_steps_fixed)
        //    throw exec_error("trough_physical", "The number of fixed steps for 'm_dot_tes_dc' does not match the length of output data arrays");
        //
        //ssc_number_t *p_m_dot_tes_ch = as_array("m_dot_tes_ch", &count);
        //if ((int)count != n_steps_fixed)
        //    throw exec_error("trough_physical", "The number of fixed steps for 'm_dot_tes_ch' does not match the length of output data arrays");
        
        for(int i = 0; i < n_steps_fixed; i++)
        {
            size_t hour = (size_t)ceil(p_time_final_hr[i]);
            p_gen[i] = (ssc_number_t)(p_W_dot_net[i] * haf(hour) * 1.E3);     //[kWe]
            //p_W_dot_parasitic_tot[i] *= -1.0;           //[kWe] Label is total parasitics, so change to a positive value
            //p_W_dot_par_tot_haf[i] = (ssc_number_t)(p_W_dot_parasitic_tot[i] * haf(hour) * 1.E3);       //[kWe]
            p_q_dot_defocus_est[i] = (ssc_number_t)(1.0 - p_SCAs_def[i])*p_q_dot_htf_sf_out[i]; //[MWt]
            //p_m_dot_tes_dc[i] = (ssc_number_t)(p_m_dot_tes_dc[i] / 3600.0);     //[kg/s] convert from kg/hr
            //p_m_dot_tes_ch[i] = (ssc_number_t)(p_m_dot_tes_ch[i] / 3600.0);     //[kg/s] convert from kg/hr
        }

        // Non-timeseries array outputs
        double P_adj = storage.P_in_des; // slightly adjust all field design pressures to account for pressure drop in TES before hot tank
        transform(c_trough.m_P_rnr_dsn.begin(), c_trough.m_P_rnr_dsn.end(), c_trough.m_P_rnr_dsn.begin(), [P_adj](double x) {return x + P_adj; });
        transform(c_trough.m_P_hdr_dsn.begin(), c_trough.m_P_hdr_dsn.end(), c_trough.m_P_hdr_dsn.begin(), [P_adj](double x) {return x + P_adj; });
        transform(c_trough.m_P_loop_dsn.begin(), c_trough.m_P_loop_dsn.end(), c_trough.m_P_loop_dsn.begin(), [P_adj](double x) {return x + P_adj; });

        ssc_number_t *p_pipe_runner_diams = allocate("pipe_runner_diams", c_trough.m_D_runner.size());
        std::copy(c_trough.m_D_runner.begin(), c_trough.m_D_runner.end(), p_pipe_runner_diams);
        ssc_number_t *p_pipe_runner_wallthk = allocate("pipe_runner_wallthk", c_trough.m_WallThk_runner.size());
        std::copy(c_trough.m_WallThk_runner.begin(), c_trough.m_WallThk_runner.end(), p_pipe_runner_wallthk);
        ssc_number_t *p_pipe_runner_lengths = allocate("pipe_runner_lengths", c_trough.m_L_runner.size());
        std::copy(c_trough.m_L_runner.begin(), c_trough.m_L_runner.end(), p_pipe_runner_lengths);
        ssc_number_t *p_pipe_runner_expansions = allocate("pipe_runner_expansions", c_trough.m_N_rnr_xpans.size());
        std::copy(c_trough.m_N_rnr_xpans.begin(), c_trough.m_N_rnr_xpans.end(), p_pipe_runner_expansions);
        ssc_number_t *p_pipe_runner_mdot_dsn = allocate("pipe_runner_mdot_dsn", c_trough.m_m_dot_rnr_dsn.size());
        std::copy(c_trough.m_m_dot_rnr_dsn.begin(), c_trough.m_m_dot_rnr_dsn.end(), p_pipe_runner_mdot_dsn);
        ssc_number_t *p_pipe_runner_vel_dsn = allocate("pipe_runner_vel_dsn", c_trough.m_V_rnr_dsn.size());
        std::copy(c_trough.m_V_rnr_dsn.begin(), c_trough.m_V_rnr_dsn.end(), p_pipe_runner_vel_dsn);
        ssc_number_t *p_pipe_runner_T_dsn = allocate("pipe_runner_T_dsn", c_trough.m_T_rnr_dsn.size());
        std::copy(c_trough.m_T_rnr_dsn.begin(), c_trough.m_T_rnr_dsn.end(), p_pipe_runner_T_dsn);
        ssc_number_t *p_pipe_runner_P_dsn = allocate("pipe_runner_P_dsn", c_trough.m_P_rnr_dsn.size());
        std::copy(c_trough.m_P_rnr_dsn.begin(), c_trough.m_P_rnr_dsn.end(), p_pipe_runner_P_dsn);

        ssc_number_t *p_pipe_header_diams = allocate("pipe_header_diams", c_trough.m_D_hdr.size());
        std::copy(c_trough.m_D_hdr.begin(), c_trough.m_D_hdr.end(), p_pipe_header_diams);
        ssc_number_t *p_pipe_header_wallthk = allocate("pipe_header_wallthk", c_trough.m_WallThk_hdr.size());
        std::copy(c_trough.m_WallThk_hdr.begin(), c_trough.m_WallThk_hdr.end(), p_pipe_header_wallthk);
        ssc_number_t *p_pipe_header_lengths = allocate("pipe_header_lengths", c_trough.m_L_hdr.size());
        std::copy(c_trough.m_L_hdr.begin(), c_trough.m_L_hdr.end(), p_pipe_header_lengths);
        ssc_number_t *p_pipe_header_expansions = allocate("pipe_header_expansions", c_trough.m_N_hdr_xpans.size());
        std::copy(c_trough.m_N_hdr_xpans.begin(), c_trough.m_N_hdr_xpans.end(), p_pipe_header_expansions);
        ssc_number_t *p_pipe_header_mdot_dsn = allocate("pipe_header_mdot_dsn", c_trough.m_m_dot_hdr_dsn.size());
        std::copy(c_trough.m_m_dot_hdr_dsn.begin(), c_trough.m_m_dot_hdr_dsn.end(), p_pipe_header_mdot_dsn);
        ssc_number_t *p_pipe_header_vel_dsn = allocate("pipe_header_vel_dsn", c_trough.m_V_hdr_dsn.size());
        std::copy(c_trough.m_V_hdr_dsn.begin(), c_trough.m_V_hdr_dsn.end(), p_pipe_header_vel_dsn);
        ssc_number_t *p_pipe_header_T_dsn = allocate("pipe_header_T_dsn", c_trough.m_T_hdr_dsn.size());
        std::copy(c_trough.m_T_hdr_dsn.begin(), c_trough.m_T_hdr_dsn.end(), p_pipe_header_T_dsn);
        ssc_number_t *p_pipe_header_P_dsn = allocate("pipe_header_P_dsn", c_trough.m_P_hdr_dsn.size());
        std::copy(c_trough.m_P_hdr_dsn.begin(), c_trough.m_P_hdr_dsn.end(), p_pipe_header_P_dsn);

        ssc_number_t *p_pipe_loop_T_dsn = allocate("pipe_loop_T_dsn", c_trough.m_T_loop_dsn.size());
        std::copy(c_trough.m_T_loop_dsn.begin(), c_trough.m_T_loop_dsn.end(), p_pipe_loop_T_dsn);
        ssc_number_t *p_pipe_loop_P_dsn = allocate("pipe_loop_P_dsn", c_trough.m_P_loop_dsn.size());
        std::copy(c_trough.m_P_loop_dsn.begin(), c_trough.m_P_loop_dsn.end(), p_pipe_loop_P_dsn);

        ssc_number_t *p_pipe_tes_diams = allocate("pipe_tes_diams", storage.pipe_diams.ncells());
        std::copy(storage.pipe_diams.data(), storage.pipe_diams.data() + storage.pipe_diams.ncells(), p_pipe_tes_diams);
        ssc_number_t *p_pipe_tes_wallthk = allocate("pipe_tes_wallthk", storage.pipe_wall_thk.ncells());
        std::copy(storage.pipe_wall_thk.data(), storage.pipe_wall_thk.data() + storage.pipe_wall_thk.ncells(), p_pipe_tes_wallthk);
        ssc_number_t* p_pipe_tes_lengths = allocate("pipe_tes_lengths", storage.pipe_lengths.ncells());
        std::copy(storage.pipe_lengths.data(), storage.pipe_lengths.data() + storage.pipe_lengths.ncells(), p_pipe_tes_lengths);
        ssc_number_t *p_pipe_tes_mdot_dsn = allocate("pipe_tes_mdot_dsn", storage.pipe_m_dot_des.ncells());
        std::copy(storage.pipe_m_dot_des.data(), storage.pipe_m_dot_des.data() + storage.pipe_m_dot_des.ncells(), p_pipe_tes_mdot_dsn);
        ssc_number_t *p_pipe_tes_vel_dsn = allocate("pipe_tes_vel_dsn", storage.pipe_vel_des.ncells());
        std::copy(storage.pipe_vel_des.data(), storage.pipe_vel_des.data() + storage.pipe_vel_des.ncells(), p_pipe_tes_vel_dsn);
        ssc_number_t *p_pipe_tes_T_dsn = allocate("pipe_tes_T_dsn", storage.pipe_T_des.ncells());
        std::copy(storage.pipe_T_des.data(), storage.pipe_T_des.data() + storage.pipe_T_des.ncells(), p_pipe_tes_T_dsn);
        ssc_number_t *p_pipe_tes_P_dsn = allocate("pipe_tes_P_dsn", storage.pipe_P_des.ncells());
        std::copy(storage.pipe_P_des.data(), storage.pipe_P_des.data() + storage.pipe_P_des.ncells(), p_pipe_tes_P_dsn);


        // Monthly outputs
        accumulate_monthly_for_year("gen", "monthly_energy", sim_setup.m_report_step / 3600.0, steps_per_hour, 1);


        // Annual outputs
        accumulate_annual_for_year("gen", "annual_energy", sim_setup.m_report_step / 3600.0, steps_per_hour, 1, n_steps_fixed / steps_per_hour);
        accumulate_annual_for_year("P_cycle", "annual_W_cycle_gross", 1000.0*sim_setup.m_report_step / 3600.0, steps_per_hour, 1, n_steps_fixed / steps_per_hour);		//[kWe-hr]
        //accumulate_annual_for_year("W_dot_par_tot_haf", "annual_electricity_consumption", sim_setup.m_report_step/3600.0, steps_per_hour);  //[kWe-hr]
        accumulate_annual_for_year("disp_objective", "disp_objective_ann", 1000.0*sim_setup.m_report_step / 3600.0, steps_per_hour, 1, n_steps_fixed / steps_per_hour);
        accumulate_annual_for_year("disp_solve_iter", "disp_iter_ann", 1000.0*sim_setup.m_report_step / 3600.0, steps_per_hour, 1, n_steps_fixed / steps_per_hour);
        accumulate_annual_for_year("disp_presolve_nconstr", "disp_presolve_nconstr_ann", sim_setup.m_report_step / 3600.0 / as_double("disp_frequency"), steps_per_hour, 1, n_steps_fixed / steps_per_hour);
        accumulate_annual_for_year("disp_presolve_nvar", "disp_presolve_nvar_ann", sim_setup.m_report_step / 3600.0 / as_double("disp_frequency"), steps_per_hour, 1, n_steps_fixed / steps_per_hour);
        accumulate_annual_for_year("disp_solve_time", "disp_solve_time_ann", sim_setup.m_report_step / 3600. / as_double("disp_frequency"), steps_per_hour, 1, n_steps_fixed / steps_per_hour);
        accumulate_annual_for_year("q_dc_tes", "annual_q_dc_tes", sim_setup.m_report_step / 3600.0, steps_per_hour, 1, n_steps_fixed / steps_per_hour);
        accumulate_annual_for_year("q_ch_tes", "annual_q_ch_tes", sim_setup.m_report_step / 3600.0, steps_per_hour, 1, n_steps_fixed / steps_per_hour);
        
        ssc_number_t annual_field_fp = accumulate_annual_for_year("q_dot_freeze_prot", "annual_field_freeze_protection", sim_setup.m_report_step / 3600.0*1.E3, steps_per_hour);    //[kWt-hr]
        ssc_number_t annual_tes_fp = accumulate_annual_for_year("q_tes_heater", "annual_tes_freeze_protection", sim_setup.m_report_step / 3600.0*1.E3, steps_per_hour); //[kWt-hr]

        ssc_number_t annual_thermal_consumption = annual_field_fp + annual_tes_fp;  //[kWt-hr]
        assign("annual_thermal_consumption", annual_thermal_consumption);

        // Calculate water use
        // First, sum power cycle water consumption timeseries outputs
        accumulate_annual_for_year("m_dot_water_pc", "annual_total_water_use", sim_setup.m_report_step / 1000.0, steps_per_hour, 1, n_steps_fixed / steps_per_hour); //[m^3], convert from kg
        ssc_number_t V_water_cycle = as_number("annual_total_water_use");
        // Then, add water usage from mirror cleaning
        double A_aper_tot = csp_solver.get_cr_aperture_area();  //[m2]
        double V_water_mirrors = as_double("water_usage_per_wash")/1000.0*A_aper_tot*as_double("washing_frequency");
        assign("annual_total_water_use", (ssc_number_t)(V_water_mirrors + V_water_cycle));        //[m3]

        ssc_number_t ae = as_number("annual_energy");
        ssc_number_t pg = as_number("annual_W_cycle_gross");
        ssc_number_t convfactor = (pg != 0) ? 100 * ae / pg : (ssc_number_t)0.0;
        assign("conversion_factor", convfactor);

        double kWh_per_kW = 0.0;
        double system_capacity = as_double("P_ref") * as_double("gross_net_conversion_factor") *1.E3;       //[kWe]
        double nameplate = system_capacity;     //[kWe]
        if (nameplate > 0.0)
            kWh_per_kW = ae / nameplate;

        assign("capacity_factor", (ssc_number_t)(kWh_per_kW / ((double)n_steps_fixed / (double)steps_per_hour)*100.));
        assign("kwh_per_kw", (ssc_number_t)kWh_per_kW);
    }
    
};

DEFINE_MODULE_ENTRY(trough_physical, "Physical trough applications", 1)
