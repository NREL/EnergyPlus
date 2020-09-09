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
#include "tckernel.h"
#include <algorithm>
// for adjustment factors
#include "common.h"

#include "lib_weatherfile.h"

static var_info _cm_vtab_tcstrough_physical[] = {
//   weather reader inputs
//   VARTYPE            DATATYPE          NAME                        LABEL                                                                               UNITS           META            GROUP             REQUIRED_IF                CONSTRAINTS              UI_HINTS
    { SSC_INPUT,        SSC_STRING,      "file_name",                 "Local weather file with path",                                                     "none",         "",             "Weather",        "*",                       "LOCAL_FILE",            "" },
    { SSC_INPUT,        SSC_NUMBER,      "track_mode",                "Tracking mode",                                                                    "none",         "",             "Weather",        "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "tilt",                      "Tilt angle of surface/axis",                                                       "none",         "",             "Weather",        "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "azimuth",                   "Azimuth angle of surface/axis",                                                    "none",         "",             "Weather",        "*",                       "",                      "" },
	{ SSC_INPUT,        SSC_NUMBER,      "system_capacity",           "Nameplate capacity",                                                               "kW",           "",             "trough",         "*",                       "",                      "" },

//   solar field (type 250) inputs							          																	                  
//   VARTYPE            DATATYPE          NAME                        LABEL                                                                               UNITS           META              GROUP             REQUIRED_IF                CONSTRAINTS              UI_HINTS
    { SSC_INPUT,        SSC_NUMBER,      "nSCA",                      "Number of SCAs in a loop",                                                         "none",         "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "nHCEt",                     "Number of HCE types",                                                              "none",         "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "nColt",                     "Number of collector types",                                                        "none",         "constant=4",              "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "nHCEVar",                   "Number of HCE variants per type",                                                  "none",         "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "nLoops",                    "Number of loops in the field",                                                     "none",         "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "eta_pump",                  "HTF pump efficiency",                                                              "none",         "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "HDR_rough",                 "Header pipe roughness",                                                            "m",            "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "theta_stow",                "Stow angle",                                                                       "deg",          "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "theta_dep",                 "Deploy angle",                                                                     "deg",          "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "Row_Distance",              "Spacing between rows (centerline to centerline)",                                  "m",            "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "FieldConfig",               "Number of subfield headers",                                                       "none",         "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "T_startup",                 "Required temperature of the system before the power block can be switched on",     "C",            "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "P_ref",                     "Rated plant capacity",                                                             "MWe",          "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "m_dot_htfmin",              "Minimum loop HTF flow rate",                                                       "kg/s",         "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "m_dot_htfmax",              "Maximum loop HTF flow rate",                                                       "kg/s",         "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "T_loop_in_des",             "Design loop inlet temperature",                                                    "C",            "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "T_loop_out",                "Target loop outlet temperature",                                                   "C",            "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "Fluid",                     "Field HTF fluid ID number",                                                        "none",         "",               "solar_field",    "*",                       "",                      "" },
                                                                                                                                                              
	{ SSC_INPUT,        SSC_NUMBER,      "T_fp",                      "Freeze protection temperature (heat trace activation temperature)",                "C",            "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "I_bn_des",                  "Solar irradiation at design",                                                      "W/m2",         "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "calc_design_pipe_vals",     "Calculate temps and pressures at design conditions for runners and headers",       "none",         "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "V_hdr_cold_max",            "Maximum HTF velocity in the cold headers at design",                               "m/s",          "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "V_hdr_cold_min",            "Minimum HTF velocity in the cold headers at design",                               "m/s",          "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "V_hdr_hot_max",             "Maximum HTF velocity in the hot headers at design",                                "m/s",          "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "V_hdr_hot_min",             "Minimum HTF velocity in the hot headers at design",                                "m/s",          "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "N_max_hdr_diams",           "Maximum number of diameters in each of the hot and cold headers",                  "none",         "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "L_rnr_pb",                  "Length of runner pipe in power block",                                             "m",            "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "L_rnr_per_xpan",            "Threshold length of straight runner pipe without an expansion loop",               "m",            "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "L_xpan_hdr",                "Compined perpendicular lengths of each header expansion loop",                     "m",            "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "L_xpan_rnr",                "Compined perpendicular lengths of each runner expansion loop",                     "m",            "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "Min_rnr_xpans",             "Minimum number of expansion loops per single-diameter runner section",             "none",         "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "northsouth_field_sep",      "North/south separation between subfields. 0 = SCAs are touching",                  "m",            "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "N_hdr_per_xpan",            "Number of collector loops per expansion loop",                                     "none",         "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "offset_xpan_hdr",           "Location of first header expansion loop. 1 = after first collector loop",          "none",         "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "Pipe_hl_coef",              "Loss coefficient from the header, runner pipe, and non-HCE piping",                "W/m2-K",       "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "SCA_drives_elec",           "Tracking power, in Watts per SCA drive",                                           "W/SCA",        "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "fthrok",                    "Flag to allow partial defocusing of the collectors",                               "",             "",               "solar_field",    "*",                       "INTEGER",               "" },
    { SSC_INPUT,        SSC_NUMBER,      "fthrctrl",                  "Defocusing strategy",                                                              "none",         "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "water_usage_per_wash",      "Water usage per wash",                                                             "L/m2_aper",    "",               "solar_field",    "*",                       "",                      "" },
	{ SSC_INPUT,        SSC_NUMBER,      "washing_frequency",         "Mirror washing frequency",                                                         "none",         "",               "solar_field",    "*",                       "",                      "" },
	{ SSC_INPUT,        SSC_NUMBER,      "accept_mode",               "Acceptance testing mode?",                                                         "0/1",          "no/yes",         "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "accept_init",               "In acceptance testing mode - require steady-state startup",                        "none",         "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "accept_loc",                "In acceptance testing mode - temperature sensor location",                         "1/2",          "hx/loop",        "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "solar_mult",                "Solar multiple",                                                                   "none",         "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "mc_bal_hot",                "Heat capacity of the balance of plant on the hot side",                            "kWht/K-MWt",   "none",           "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "mc_bal_cold",               "Heat capacity of the balance of plant on the cold side",                           "kWht/K-MWt",   "",               "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "mc_bal_sca",                "Non-HTF heat capacity associated with each SCA - per meter basis",                 "Wht/K-m",      "",               "solar_field",    "*",                       "",                      "" },
                                                                                                                                                             
    { SSC_INPUT,        SSC_ARRAY,       "W_aperture",                "The collector aperture width (Total structural area used for shadowing)",          "m",            "",             "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_ARRAY,       "A_aperture",                "Reflective aperture area of the collector",                                        "m2",           "",             "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_ARRAY,       "TrackingError",             "User-defined tracking error derate",                                               "none",         "",             "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_ARRAY,       "GeomEffects",               "User-defined geometry effects derate",                                             "none",         "",             "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_ARRAY,       "Rho_mirror_clean",          "User-defined clean mirror reflectivity",                                           "none",         "",             "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_ARRAY,       "Dirt_mirror",               "User-defined dirt on mirror derate",                                               "none",         "",             "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_ARRAY,       "Error",                     "User-defined general optical error derate ",                                       "none",         "",             "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_ARRAY,       "Ave_Focal_Length",          "Average focal length of the collector ",                                           "m",            "",             "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_ARRAY,       "L_SCA",                     "Length of the SCA ",                                                               "m",            "",             "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_ARRAY,       "L_aperture",                "Length of a single mirror/HCE unit",                                               "m",            "",             "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_ARRAY,       "ColperSCA",                 "Number of individual collector sections in an SCA ",                               "none",         "",             "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_ARRAY,       "Distance_SCA",              "Piping distance between SCA's in the field",                                       "m",            "",             "solar_field",    "*",                       "",                      "" },

	{ SSC_INPUT,        SSC_MATRIX,      "IAM_matrix",                "IAM coefficients, matrix for 4 collectors",                                        "none",         "",             "solar_field",    "*",                       "",                      "" },

    { SSC_INPUT,        SSC_MATRIX,      "HCE_FieldFrac",             "Fraction of the field occupied by this HCE type ",                                 "none",         "",             "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "D_2",                       "Inner absorber tube diameter",                                                     "m",            "",             "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "D_3",                       "Outer absorber tube diameter",                                                     "m",            "",             "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "D_4",                       "Inner glass envelope diameter ",                                                   "m",            "",             "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "D_5",                       "Outer glass envelope diameter ",                                                   "m",            "",             "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "D_p",                       "Diameter of the absorber flow plug (optional) ",                                   "m",            "",             "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "Flow_type",                 "Flow type through the absorber",                                                   "none",         "",             "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "Rough",                     "Relative roughness of the internal HCE surface ",                                  "-",            "",             "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "alpha_env",                 "Envelope absorptance ",                                                            "none",         "",             "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "epsilon_3_11",              "Absorber emittance for receiver type 1 variation 1",                               "none",         "",             "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "epsilon_3_12",              "Absorber emittance for receiver type 1 variation 2",                               "none",         "",             "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "epsilon_3_13",              "Absorber emittance for receiver type 1 variation 3",                               "none",         "",             "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "epsilon_3_14",              "Absorber emittance for receiver type 1 variation 4",                               "none",         "",             "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "epsilon_3_21",              "Absorber emittance for receiver type 2 variation 1",                               "none",         "",             "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "epsilon_3_22",              "Absorber emittance for receiver type 2 variation 2",                               "none",         "",             "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "epsilon_3_23",              "Absorber emittance for receiver type 2 variation 3",                               "none",         "",             "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "epsilon_3_24",              "Absorber emittance for receiver type 2 variation 4",                               "none",         "",             "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "epsilon_3_31",              "Absorber emittance for receiver type 3 variation 1",                               "none",         "",             "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "epsilon_3_32",              "Absorber emittance for receiver type 3 variation 2",                               "none",         "",             "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "epsilon_3_33",              "Absorber emittance for receiver type 3 variation 3",                               "none",         "",             "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "epsilon_3_34",              "Absorber emittance for receiver type 3 variation 4",                               "none",         "",             "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "epsilon_3_41",              "Absorber emittance for receiver type 4 variation 1",                               "none",         "",             "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "epsilon_3_42",              "Absorber emittance for receiver type 4 variation 2",                               "none",         "",             "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "epsilon_3_43",              "Absorber emittance for receiver type 4 variation 3",                               "none",         "",             "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "epsilon_3_44",              "Absorber emittance for receiver type 4 variation 4",                               "none",         "",             "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "alpha_abs",                 "Absorber absorptance ",                                                            "none",         "",             "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "Tau_envelope",              "Envelope transmittance",                                                           "none",         "",             "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "EPSILON_4",                 "Inner glass envelope emissivities (Pyrex) ",                                       "none",         "",             "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "EPSILON_5",                 "Outer glass envelope emissivities (Pyrex) ",                                       "none",         "",             "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "GlazingIntactIn",           "Glazing intact (broken glass) flag {1=true, else=false}",                          "none",         "",             "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "P_a",                       "Annulus gas pressure",                                                             "torr",         "",             "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "AnnulusGas",                "Annulus gas type (1=air, 26=Ar, 27=H2)",                                           "none",         "",             "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "AbsorberMaterial",          "Absorber material type",                                                           "none",         "",             "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "Shadowing",                 "Receiver bellows shadowing loss factor",                                           "none",         "",             "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "Dirt_HCE",                  "Loss due to dirt on the receiver envelope",                                        "none",         "",             "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "Design_loss",               "Receiver heat loss at design",                                                     "W/m",          "",             "solar_field",    "*",                       "",                      "" },

    { SSC_INPUT,        SSC_MATRIX,      "SCAInfoArray",              "Receiver (,1) and collector (,2) type for each assembly in loop",                 "none",          "",             "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_ARRAY,       "SCADefocusArray",           "Collector defocus order",                                                         "none",          "",             "solar_field",    "*",                       "",                      "" },

    { SSC_INPUT,        SSC_MATRIX,      "K_cpnt",                    "Interconnect component minor loss coefficients, row=intc, col=cpnt",              "none",          "",             "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "D_cpnt",                    "Interconnect component diameters, row=intc, col=cpnt",                            "none",          "",             "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "L_cpnt",                    "Interconnect component lengths, row=intc, col=cpnt",                              "none",          "",             "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "Type_cpnt",                 "Interconnect component type, row=intc, col=cpnt",                                 "none",          "",             "solar_field",    "*",                       "",                      "" },

    { SSC_INPUT,        SSC_NUMBER,      "custom_sf_pipe_sizes",      "Use custom solar field pipe diams, wallthks, and lengths",                        "none",          "",             "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_ARRAY,       "sf_rnr_diams",              "Custom runner diameters",                                                            "m",          "",             "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_ARRAY,       "sf_rnr_wallthicks",         "Custom runner wall thicknesses",                                                     "m",          "",             "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_ARRAY,       "sf_rnr_lengths",            "Custom runner lengths",                                                              "m",          "",             "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_ARRAY,       "sf_hdr_diams",              "Custom header diameters",                                                            "m",          "",             "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_ARRAY,       "sf_hdr_wallthicks",         "Custom header wall thicknesses",                                                     "m",          "",             "solar_field",    "*",                       "",                      "" },
    { SSC_INPUT,        SSC_ARRAY,       "sf_hdr_lengths",            "Custom header lengths",                                                              "m",          "",             "solar_field",    "*",                       "",                      "" },
														          															          
//   controller (type 251) inputs							          
//   VARTYPE            DATATYPE          NAME                        LABEL                                                             UNITS           META            GROUP             REQUIRED_IF                CONSTRAINTS              UI_HINTS
    { SSC_INPUT,        SSC_MATRIX,      "field_fl_props",            "User defined field fluid property data",                         "-",            "",             "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "store_fl_props",            "User defined storage fluid property data",                       "-",            "",             "controller",     "*",                       "",                      "" },    
	{ SSC_INPUT,        SSC_NUMBER,      "store_fluid",               "Material number for storage fluid",                              "-",            "",             "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "tshours",                   "Equivalent full-load thermal storage hours",                     "hr",           "",             "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "eta_pump",                  "HTF pump efficiency",                                            "none",         "",             "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "HDR_rough",                 "Header pipe roughness - used as general pipe roughness",         "m",            "",             "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "is_hx",                     "Heat exchanger (HX) exists (1=yes, 0=no)" ,                      "-",            "",             "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "dt_hot",                    "Hot side HX approach temp",                                      "C",            "",             "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "dt_cold",                   "Cold side HX approach temp",                                     "C",            "",             "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "hx_config",                 "HX configuration",                                               "-",            "",             "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "q_max_aux",                 "Max heat rate of auxiliary heater",                              "MWt",          "",             "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "T_set_aux",                 "Aux heater outlet temp set point",                               "C",            "",             "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "V_tank_hot_ini",            "Initial hot tank fluid volume",                                  "m3",           "",             "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "T_tank_cold_ini",           "Initial cold tank fluid tmeperature",                            "C",            "",             "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "vol_tank",                  "Total tank volume, including unusable HTF at bottom",            "m3",           "",             "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "h_tank",                    "Total height of tank (height of HTF when tank is full",          "m",            "",             "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "h_tank_min",                "Minimum allowable HTF height in storage tank",                   "m",            "",             "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "u_tank",                    "Loss coefficient from the tank",                                 "W/m2-K",       "",             "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "tank_pairs",                "Number of equivalent tank pairs",                                "-",            "",             "controller",     "*",                       "INTEGER",               "" },
    { SSC_INPUT,        SSC_NUMBER,      "cold_tank_Thtr",            "Minimum allowable cold tank HTF temp",                           "C",            "",             "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "hot_tank_Thtr",             "Minimum allowable hot tank HTF temp",                            "C",            "",             "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "tank_max_heat",             "Rated heater capacity for tank heating",                         "MW",           "",             "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "tanks_in_parallel",         "Tanks are in parallel, not in series, with solar field",         "-",            "",             "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "has_hot_tank_bypass",       "Bypass valve connects field outlet to cold tank",                "-",            "",             "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "T_tank_hot_inlet_min",      "Minimum hot tank htf inlet temperature",                         "C",            "",             "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "q_pb_design",               "Design heat input to power block",                               "MWt",          "",             "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "W_pb_design",               "Rated plant capacity",                                           "MWe",          "",             "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "cycle_max_frac",            "Maximum turbine over design operation fraction",                 "-",            "",             "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "cycle_cutoff_frac",         "Minimum turbine operation fraction before shutdown",             "-",            "",             "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "pb_pump_coef",              "Pumping power to move 1kg of HTF through PB loop",               "kW/(kg/s)",    "",             "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "tes_pump_coef",             "Pumping power to move 1kg of HTF through tes loop",              "kW/(kg/s)",    "",             "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "V_tes_des",                 "Design-point velocity to size the TES pipe diameters",           "m/s",          "",             "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "custom_tes_p_loss",         "TES pipe losses are based on custom lengths and coeffs",         "-",            "",             "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_ARRAY,       "k_tes_loss_coeffs",         "Minor loss coeffs for the coll, gen, and bypass loops",          "-",            "",             "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "custom_sgs_pipe_sizes",     "Use custom SGS pipe diams, wallthks, and lengths",               "-",            "",             "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_ARRAY,       "sgs_diams",                 "Custom SGS diameters",                                           "m",            "",             "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_ARRAY,       "sgs_wallthicks",            "Custom SGS wall thicknesses",                                    "m",            "",             "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_ARRAY,       "sgs_lengths",               "Custom SGS lengths",                                             "m",            "",             "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "DP_SGS",                    "Pressure drop within the steam generator",                       "bar",          "",             "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "pb_fixed_par",              "Fraction of rated gross power constantly consumed",              "-",            "",             "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_ARRAY,       "bop_array",                 "Coefficients for balance of plant parasitics calcs",             "-",            "",             "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_ARRAY,       "aux_array",                 "Coefficients for auxiliary heater parasitics calcs",             "-",            "",             "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "fossil_mode",               "Fossil backup mode 1=Normal 2=Topping",                          "-",            "",             "controller",     "*",                       "INTEGER",               "" },
    { SSC_INPUT,        SSC_NUMBER,      "t_standby_reset",           "Maximum allowable time for PB standby operation",                "hr",           "",             "controller",     "*",                       "",                      "" },
	{ SSC_INPUT,        SSC_NUMBER,      "sf_type",                   "Solar field type, 1 = trough, 2 = tower",                        "-",            "",             "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "tes_type",                  "1=2-tank, 2=thermocline",                                        "-",            "",             "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_ARRAY,       "tslogic_a",                 "Dispatch logic without solar",                                   "-",            "",             "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_ARRAY,       "tslogic_b",                 "Dispatch logic with solar",                                      "-",            "",             "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_ARRAY,       "tslogic_c",                 "Dispatch logic for turbine load fraction",                       "-",            "",             "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_ARRAY,       "ffrac",                     "Fossil dispatch logic",                                          "-",            "",             "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "tc_fill",                   "Thermocline fill material",                                      "-",            "",             "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "tc_void",                   "Thermocline void fraction",                                      "-",            "",             "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "t_dis_out_min",             "Min allowable hot side outlet temp during discharge",            "C",            "",             "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "t_ch_out_max",              "Max allowable cold side outlet temp during charge",              "C",            "",             "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "nodes",                     "Nodes modeled in the flow path",                                 "-",            "",             "controller",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "f_tc_cold",                 "0=entire tank is hot, 1=entire tank is cold",                    "-",            "",             "controller",     "*",                       "",                      "" },

    // Time of use schedules for thermal storage
    { SSC_INPUT,        SSC_MATRIX,      "weekday_schedule",          "Dispatch 12mx24h schedule for week days",                         "",             "",             "tou_translator", "*",                       "",                      "" }, 
    { SSC_INPUT,        SSC_MATRIX,      "weekend_schedule",          "Dispatch 12mx24h schedule for weekends",                          "",             "",             "tou_translator", "*",                       "",                      "" }, 
															          																	                  
						          
//   VARTYPE            DATATYPE          NAME                LABEL                                                                        UNITS           META                            GROUP             REQUIRED_IF                CONSTRAINTS              UI_HINTS
	// Power Cycle Inputs
	{ SSC_INPUT,        SSC_NUMBER,      "pc_config",         "0: Steam Rankine (224), 1: user defined",                                   "-",            "",                             "powerblock",     "?=0",                     "INTEGER",               "" },        
	{ SSC_INPUT,        SSC_NUMBER,      "eta_ref",           "Reference conversion efficiency at design condition",                       "none",         "",                             "powerblock",     "*",                       "",                      "" },
	{ SSC_INPUT,        SSC_NUMBER,      "startup_time",      "Time needed for power block startup",                                       "hr",           "",                             "powerblock",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "startup_frac",      "Fraction of design thermal power needed for startup",                       "none",         "",                             "powerblock",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "q_sby_frac",        "Fraction of thermal power required for standby mode",                       "none",         "",                             "powerblock",     "*",                       "",                      "" },
    

	// Steam Rankine cycle
    { SSC_INPUT,        SSC_NUMBER,      "dT_cw_ref",         "Reference condenser cooling water inlet/outlet T diff",                     "C",            "",                             "powerblock",     "pc_config=0",             "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "T_amb_des",         "Reference ambient temperature at design point",                             "C",            "",                             "powerblock",     "pc_config=0",             "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "P_boil",            "Boiler operating pressure",                                                 "bar",          "",                             "powerblock",     "pc_config=0",             "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "CT",                "Flag for using dry cooling or wet cooling system",                          "none",         "",                             "powerblock",     "pc_config=0",             "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "T_approach",        "Cooling tower approach temperature",                                        "C",            "",                             "powerblock",     "pc_config=0",             "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "T_ITD_des",         "ITD at design for dry system",                                              "C",            "",                             "powerblock",     "pc_config=0",             "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "P_cond_ratio",      "Condenser pressure ratio",                                                  "none",         "",                             "powerblock",     "pc_config=0",             "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "pb_bd_frac",        "Power block blowdown steam fraction ",                                      "none",         "",                             "powerblock",     "pc_config=0",             "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "P_cond_min",        "Minimum condenser pressure",                                                "inHg",         "",                             "powerblock",     "pc_config=0",             "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "n_pl_inc",          "Number of part-load increments for the heat rejection system",              "none",         "",                             "powerblock",     "pc_config=0",             "",                      "" },
    { SSC_INPUT,        SSC_ARRAY,       "F_wc",              "Fraction indicating wet cooling use for hybrid system",                     "none",         "constant=[0,0,0,0,0,0,0,0,0]", "powerblock",     "pc_config=0",             "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "tech_type",         "Turbine inlet pressure control flag (sliding=user, fixed=trough)",          "1/2/3",         "tower/trough/user",           "powerblock",     "pc_config=0",             "",                      "" },
	
		// User Defined cycle
	{ SSC_INPUT,        SSC_NUMBER,      "ud_T_amb_des",         "Ambient temperature at user-defined power cycle design point",                   "C",	    "",                            "user_defined_PC", "pc_config=1",            "",                      "" },
	{ SSC_INPUT,        SSC_NUMBER,      "ud_f_W_dot_cool_des",  "Percent of user-defined power cycle design gross output consumed by cooling",    "%",	    "",                            "user_defined_PC", "pc_config=1",            "",                      "" },
	{ SSC_INPUT,        SSC_NUMBER,      "ud_m_dot_water_cool_des", "Mass flow rate of water required at user-defined power cycle design point",   "kg/s",  "",                            "user_defined_PC", "pc_config=1",            "",                      "" },
	{ SSC_INPUT,        SSC_NUMBER,      "ud_T_htf_low",         "Low level HTF inlet temperature for T_amb parametric",                           "C",     "",                            "user_defined_PC", "pc_config=1",            "",                      "" },
	{ SSC_INPUT,        SSC_NUMBER,      "ud_T_htf_high",        "High level HTF inlet temperature for T_amb parametric",                          "C",		"",                            "user_defined_PC", "pc_config=1",            "",                      "" },
	{ SSC_INPUT,        SSC_NUMBER,      "ud_T_amb_low",         "Low level ambient temperature for HTF mass flow rate parametric",                "C",		"",                            "user_defined_PC", "pc_config=1",            "",                      "" },
	{ SSC_INPUT,        SSC_NUMBER,      "ud_T_amb_high",        "High level ambient temperature for HTF mass flow rate parametric",               "C",		"",                            "user_defined_PC", "pc_config=1",            "",                      "" },
	{ SSC_INPUT,        SSC_NUMBER,      "ud_m_dot_htf_low",     "Low level normalized HTF mass flow rate for T_HTF parametric",                   "-",	    "",                            "user_defined_PC", "pc_config=1",            "",                      "" },
	{ SSC_INPUT,        SSC_NUMBER,      "ud_m_dot_htf_high",    "High level normalized HTF mass flow rate for T_HTF parametric",                  "-",	    "",                            "user_defined_PC", "pc_config=1",            "",                      "" },
	{ SSC_INPUT,        SSC_MATRIX,      "ud_T_htf_ind_od",      "Off design table of user-defined power cycle performance formed from parametric on T_htf_hot [C]", "", "",               "user_defined_PC", "?=[[0]]",            "",                      "" },
	{ SSC_INPUT,        SSC_MATRIX,      "ud_T_amb_ind_od",      "Off design table of user-defined power cycle performance formed from parametric on T_amb [C]",	 "", "",               "user_defined_PC", "?=[[0]]",            "",                      "" }, 
	{ SSC_INPUT,        SSC_MATRIX,      "ud_m_dot_htf_ind_od",  "Off design table of user-defined power cycle performance formed from parametric on m_dot_htf [ND]","", "",               "user_defined_PC", "?=[[0]]",            "",                      "" }, 
    { SSC_INPUT,        SSC_MATRIX,      "ud_ind_od",            "Off design user-defined power cycle performance as function of T_htf, m_dot_htf [ND], and T_amb", "", "", "user_defined_PC", "?=[[0]]",     "",                      "" },
		
	// Financial inputs
	// { SSC_INPUT,        SSC_MATRIX,      "dispatch_sched_weekday", "12x24 PPA pricing Weekday schedule",                              "",             "",            "tou",            "*",                       "",                      "" }, 
	// { SSC_INPUT,        SSC_MATRIX,      "dispatch_sched_weekend", "12x24 PPA pricing Weekend schedule",                              "",             "",            "tou",            "*",                       "",                      "" }, 
	// { SSC_INPUT,        SSC_NUMBER,      "dispatch_factor1",     "Dispatch payment factor 1",	                                      "",             "",            "tou",            "*",						  "",                      "" },
	// { SSC_INPUT,        SSC_NUMBER,      "dispatch_factor2",     "Dispatch payment factor 2",	                                      "",             "",            "tou",            "*",						  "",                      "" },
	// { SSC_INPUT,        SSC_NUMBER,      "dispatch_factor3",     "Dispatch payment factor 3",	                                      "",             "",            "tou",            "*",						  "",                      "" },
	// { SSC_INPUT,        SSC_NUMBER,      "dispatch_factor4",     "Dispatch payment factor 4",	                                      "",             "",            "tou",            "*",						  "",                      "" },
	// { SSC_INPUT,        SSC_NUMBER,      "dispatch_factor5",     "Dispatch payment factor 5",	                                      "",             "",            "tou",            "*",						  "",                      "" },
	// { SSC_INPUT,        SSC_NUMBER,      "dispatch_factor6",     "Dispatch payment factor 6",	                                      "",             "",            "tou",            "*",						  "",                      "" },
	// { SSC_INPUT,        SSC_NUMBER,      "dispatch_factor7",     "Dispatch payment factor 7",	                                      "",             "",            "tou",            "*",						  "",                      "" },
	// { SSC_INPUT,        SSC_NUMBER,      "dispatch_factor8",     "Dispatch payment factor 8",	                                      "",             "",            "tou",            "*",						  "",                      "" },
	// { SSC_INPUT,        SSC_NUMBER,      "dispatch_factor9",     "Dispatch payment factor 9",	                                      "",             "",            "tou",            "*",						  "",                      "" },

																																												  
 //  enet calculator																																							  
    { SSC_INPUT,        SSC_NUMBER,      "eta_lhv",           "Fossil fuel lower heating value - Thermal power generated per unit fuel",   "MW/MMBTU",     "",                             "enet",           "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "eta_tes_htr",       "Thermal storage tank heater efficiency (fp_mode=1 only)",                   "none",         "",                             "enet",           "*",                       "",                      "" },



// OUTPUTS
// The names of the output variables should match the parameter names for the TCS units in order to signal the TCS kernel to store the values by timestep

//   weather file reader
//   VARTYPE            DATATYPE          NAME                LABEL                                                             UNITS           META            GROUP            REQUIRED_IF                CONSTRAINTS              UI_HINTS
    
    { SSC_OUTPUT,       SSC_ARRAY,       "month",             "Resource Month",                                                  "",             "",            "weather",        "*",                      "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "hour",              "Resource Hour of Day",                                            "",             "",            "weather",        "*",                      "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "solazi",            "Resource Solar Azimuth",                                          "deg",          "",            "weather",        "*",                      "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "solzen",            "Resource Solar Zenith",                                           "deg",          "",            "weather",        "*",                      "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "beam",              "Resource Beam normal irradiance",                                 "W/m2",         "",            "weather",        "*",                      "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "tdry",              "Resource Dry bulb temperature",                                   "C",            "",            "weather",        "*",                      "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "twet",              "Resource Wet bulb temperature",                                   "C",            "",            "weather",        "*",                      "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "wspd",              "Resource Wind Speed",                                             "m/s",          "",            "weather",        "*",                      "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "pres",              "Resource Pressure",                                               "mbar",         "",            "weather",        "*",                      "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "tou_value",         "Resource Time-of-use value",                                      "",             "",            "tou",            "*",                      "",                      "" },
																																																			 			             
    //Solar field																																															 			             
    { SSC_OUTPUT,       SSC_ARRAY,       "recirculating",          "Field recirculating (bypass valve open)",			        "-",             "",            "Type250",        "*",                       "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "pipe_header_diams",      "Field piping header diameters",							    "m",             "",            "Type250",        "*",                       "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "pipe_header_wallthk",    "Field piping header wall thicknesses",	    			    "m",             "",            "Type250",        "*",                       "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "pipe_header_lengths",    "Field piping header lengths",                               "m",             "",            "Type250",        "*",                       "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "pipe_header_expansions", "Number of field piping header expansions",                  "-",             "",            "Type250",        "*",                       "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "pipe_header_mdot_dsn",   "Field piping header mass flow at design",				    "kg/s",          "",            "Type250",        "*",                       "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "pipe_header_vel_dsn",    "Field piping header velocity at design",				    "m/s",           "",            "Type250",        "*",                       "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "pipe_header_T_dsn",      "Field piping header temperature at design",				    "C",             "",            "Type250",        "*",                       "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "pipe_header_P_dsn",      "Field piping header pressure at design",				    "bar",           "",            "Type250",        "*",                       "",                      "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "pipe_runner_diams",      "Field piping runner diameters",								"m",             "",            "Type250",        "*",                       "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "pipe_runner_wallthk",    "Field piping runner wall thicknesses",  					"m",             "",            "Type250",        "*",                       "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "pipe_runner_lengths",    "Field piping runner lengths",								"m",             "",            "Type250",        "*",                       "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "pipe_runner_expansions", "Number of field piping runner expansions",                  "-",             "",            "Type250",        "*",                       "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "pipe_runner_mdot_dsn",   "Field piping runner mass flow at design",				    "kg/s",          "",            "Type250",        "*",                       "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "pipe_runner_vel_dsn",    "Field piping runner velocity at design",				    "m/s",           "",            "Type250",        "*",                       "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "pipe_runner_T_dsn",      "Field piping runner temperature at design",				    "C",             "",            "Type250",        "*",                       "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "pipe_runner_P_dsn",      "Field piping runner pressure at design",				    "bar",           "",            "Type250",        "*",                       "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "pipe_loop_T_dsn",        "Field piping loop temperature at design",				    "C",             "",            "Type250",        "*",                       "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "pipe_loop_P_dsn",        "Field piping loop pressure at design",				        "bar",           "",            "Type250",        "*",                       "",                      "" },

	{ SSC_OUTPUT,       SSC_ARRAY,       "Theta_ave",         "Field collector solar incidence angle",                          "deg",           "",            "Type250",        "*",                       "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "CosTh_ave",         "Field collector cosine efficiency",                              "",              "",            "Type250",        "*",                       "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "IAM_ave",           "Field collector incidence angle modifier",                       "",              "",            "Type250",        "*",                       "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "RowShadow_ave",     "Field collector row shadowing loss",                             "",              "",            "Type250",        "*",                       "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "EndLoss_ave",       "Field collector optical end loss",                               "",              "",            "Type250",        "*",                       "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "dni_costh",         "Field collector DNI-cosine product",                             "W/m2",          "",            "Type250",        "*",                       "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "SCAs_def",          "Field collector fraction of focused SCA's",                      "",              "",            "Type250",        "*",                       "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "EqOpteff",          "Field collector optical efficiency",                             "",              "",            "Type250",        "*",                       "",                      "" },
    																																																		 			             
    { SSC_OUTPUT,       SSC_ARRAY,       "q_inc_sf_tot",      "Field thermal power incident",                                   "MWt",          "",            "Type250",        "*",                       "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "qinc_costh",        "Field thermal power incident after cosine",                      "MWt",          "",            "Type250",        "*",                       "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "q_abs_tot",         "Field thermal power absorbed",                                   "MWt",          "",            "Type250",        "*",                       "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "q_dump",            "Field thermal power dumped",                                     "MWt",          "",            "Type250",        "*",                       "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "q_loss_tot",        "Field thermal power receiver loss",                              "MWt",          "",            "Type250",        "*",                       "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "Pipe_hl",           "Field thermal power header pipe losses",                         "MWt",          "",            "Type250",        "*",                       "",                      "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "q_avail",           "Field thermal power produced",                                   "MWt",          "",            "Type250",        "*",                       "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "q_loss_spec_tot",   "Field thermal power avg. receiver loss",                         "W/m",          "",            "Type250",        "*",                       "",                      "" },
    																																																		 			             
    { SSC_OUTPUT,       SSC_ARRAY,       "E_bal_startup",     "Field HTF energy inertial (consumed)",                           "MWht",          "",            "Type250",        "*",                      "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "m_dot_avail",       "Field HTF mass flow rate total",                                 "kg/hr",        "",            "Type250",        "*",                       "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "m_dot_htf2",        "Field HTF mass flow rate loop",                                  "kg/s",         "",            "Type250",        "*",                       "",                      "" },
    //{ SSC_OUTPUT,       SSC_ARRAY,       "m_dot_htf_tot",     "Field HTF mass flow rate",                                       "kg/hr",        "",            "Type250",        "*",                       "",                      "" },
    //{ SSC_OUTPUT,       SSC_ARRAY,       "m_dot_field_htf",   "Field HTF mass flow rate total inc. recirc.",                    "kg/hr",        "",            "Type250",        "*",                       "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "DP_tot",            "Field HTF pressure drop total",                                  "bar",          "",            "Type250",        "*",                       "",                      "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "T_sys_c",           "Field HTF temperature cold header inlet",                        "C",            "",            "Type250",        "*",                       "",                      "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "T_sys_h",           "Field HTF temperature hot header outlet",                        "C",            "",            "Type250",        "*",                       "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "T_field_in",        "Field HTF temperature collector inlet",                          "C",            "",            "Type251",        "*",                       "",                      "" },
    																																																		 			             
    //thermal storage																																														 			             
    { SSC_OUTPUT,       SSC_ARRAY,       "pipe_sgs_diams",    "Pipe diameters in SGS",                                          "m",            "",            "Type251",        "*",                       "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "pipe_sgs_wallthk",  "Pipe wall thickness in SGS",                                     "m",            "",            "Type251",        "*",                       "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "pipe_sgs_mdot_dsn", "Mass flow SGS pipes at design conditions",                       "kg/s",         "",            "Type251",        "*",                       "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "pipe_sgs_vel_dsn",  "Velocity in SGS pipes at design conditions",                     "m/s",          "",            "Type251",        "*",                       "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "pipe_sgs_T_dsn",    "Temperature in SGS pipes at design conditions",                  "C",            "",            "Type251",        "*",                       "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "pipe_sgs_P_dsn",    "Pressure in SGS pipes at design conditions",                     "bar",          "",            "Type251",        "*",                       "",                      "" },

    { SSC_OUTPUT,       SSC_ARRAY,       "mass_tank_cold",    "TES HTF mass in cold tank",                                      "kg",           "",            "Type251",        "*",                       "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "mass_tank_hot",     "TES HTF mass in hot tank",                                       "kg",           "",            "Type251",        "*",                       "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "m_dot_charge_field","TES HTF mass flow rate - field side of HX",                      "kg/hr",        "",            "Type250",        "*",                       "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,     "m_dot_discharge_tank","TES HTF mass flow rate - storage side of HX",                    "kg/hr",        "",            "Type250",        "*",                       "",                      "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "T_tank_cold_fin",   "TES HTF temperature in cold tank",                               "C",            "",            "Type251",        "*",                       "",                      "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "T_tank_hot_fin",    "TES HTF temperature in hot tank",                                "C",            "",            "Type251",        "*",                       "",                      "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "Ts_hot",            "TES HTF temperature HX field side hot",                          "C",            "",            "Type251",        "*",                       "",                      "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "Ts_cold",           "TES HTF temperature HX field side cold",                         "C",            "",            "Type251",        "*",                       "",                      "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "T_tank_hot_in",     "TES HTF temperature hot tank inlet",                             "C",            "",            "Type251",        "*",                       "",                      "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "T_tank_cold_in",    "TES HTF temperature cold tank inlet",                            "C",            "",            "Type251",        "*",                       "",                      "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "vol_tank_cold_fin", "TES HTF volume in cold tank",                                    "m3",           "",            "Type251",        "*",                       "",                      "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "vol_tank_hot_fin",  "TES HTF volume in hot tank",                                     "m3",           "",            "Type251",        "*",                       "",                      "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "vol_tank_total",    "TES HTF volume total",                                           "m3",           "",            "Type251",        "*",                       "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "q_to_tes",          "TES thermal energy into storage",                                "MWt",          "",            "Type251",        "*",                       "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "tank_losses",       "TES thermal losses from tank(s)",                                "MWt",          "",            "Type251",        "*",                       "",                      "" },
    																																																		 			             
    //power block																																															 			             
    { SSC_OUTPUT,       SSC_ARRAY,       "eta",               "Cycle efficiency (gross)",                                       "",         "",            "Type224",        "*",                           "",                      "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "W_net",             "Cycle electrical power output (net)",                            "MWe",          "",            "Net_E_Calc",     "*",                       "",                      "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "W_cycle_gross",     "Cycle electrical power output (gross)",                          "MWe",          "",            "Net_E_Calc",     "*",                       "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "m_dot_pb",          "Cycle HTF mass flow rate",                                       "kg/hr",        "",            "Type250",        "*",                       "",                      "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "T_pb_in",           "Cycle HTF temperature in (hot)",                                 "C",            "",            "Type251",        "*",                       "",                      "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "T_pb_out",          "Cycle HTF temperature out (cold)",                               "C",            "",            "Type251",        "*",                       "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "m_dot_makeup",      "Cycle cooling water mass flow rate - makeup",                    "kg/hr",        "",            "Type250",        "*",                       "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "q_pb",              "Cycle thermal power input",                                      "MWt",          "",            "Type251",        "*",                       "",                      "" },
    //{ SSC_OUTPUT,       SSC_ARRAY,       "P_cond",            "Condenser pressure",                                             "Pa",           "",            "Type250",        "*",                       "",                      "" },
    //{ SSC_OUTPUT,       SSC_ARRAY,       "f_bays",            "Condenser fraction of operating bays",                           "",         "",            "Type250",        "*",                           "",                      "" },
																																																			 			             
    //fossil backup																																															 			             
    { SSC_OUTPUT,       SSC_ARRAY,       "Q_aux_backup",      "Fossil thermal power produced",                                  "MWt",          "",            "SumCalc",        "*",                       "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "m_dot_aux",         "Fossil HTF mass flow rate",                                      "kg/hr",        "",            "Type250",        "*",                       "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "Fuel_usage",        "Fossil fuel usage (all subsystems)",                             "MMBTU",        "",            "SumCalc",        "*",                       "",                      "" },
    																																																		 			             
    //parasitics																																															 			             
    { SSC_OUTPUT,       SSC_ARRAY,       "W_dot_pump",        "Parasitic power solar field HTF pump",                           "MWe",          "",            "Type250",        "*",                       "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "htf_pump_power",    "Parasitic power TES and Cycle HTF pump",                         "MWe",          "",            "Type251",        "*",                       "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "SCA_par_tot",       "Parasitic power field collector drives",                         "MWe",          "",            "Type250",        "*",                       "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "bop_par",           "Parasitic power generation-dependent load",                      "MWe",          "",            "Type251",        "*",                       "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "fixed_par",         "Parasitic power fixed load",                                     "MWe",          "",            "Type251",        "*",                       "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "aux_par",           "Parasitic power auxiliary heater operation",                     "MWe",          "",            "Type251",        "*",                       "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "W_cool_par",        "Parasitic power condenser operation",                            "MWe",          "",            "Type224",        "*",                       "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "Q_par_sf_fp",       "Parasitic thermal field freeze protection",                      "MWt",          "",            "SumCalc",        "*",                       "",                      "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "Q_par_tes_fp",      "Parasitic thermal TES freeze protection",                        "MWt",          "",            "SumCalc",        "*",                       "",                      "" },
	
	// Monthly Outputs
	{ SSC_OUTPUT,       SSC_ARRAY,       "monthly_energy",        "Monthly Energy",                                             "kWh",           "",            "Net_E_Calc",     "*",                      "LENGTH=12",             "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "monthly_W_cycle_gross", "Electrical source - Power cycle gross output",               "MWhe",          "",            "Net_E_Calc",     "*",                      "LENGTH=12",             "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "monthly_q_inc_sf_tot",  "Total power incident on the field",                          "MWht",          "",            "Type250",        "*",                      "LENGTH=12",             "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "monthly_q_abs_tot",     "Total absorbed energy",                                      "MWht",          "",            "Type250",        "*",                      "LENGTH=12",             "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "monthly_q_avail",       "Thermal power produced by the field",                        "MWht",          "",            "Type250",        "*",                      "LENGTH=12",             "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "monthly_Fuel_usage",    "Total fossil fuel usage by all plant subsystems",            "MMBTU",        "",            "SumCalc",        "*",                       "LENGTH=12",             "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "monthly_q_dump",        "Dumped thermal energy",                                      "MWht",          "",            "Type250",        "*",                      "LENGTH=12",             "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "monthly_m_dot_makeup",  "Cooling water makeup flow rate",                             "kg/hr",        "",            "Type250",        "*",                       "LENGTH=12",             "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "monthly_q_pb",          "Thermal energy to the power block",                          "MWht",          "",            "Type251",        "*",                      "LENGTH=12",             "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "monthly_q_to_tes",      "Thermal energy into storage",                                "MWht",          "",            "Type251",        "*",                      "LENGTH=12",             "" },

	// Annual Outputs
	{ SSC_OUTPUT,       SSC_NUMBER,      "annual_energy",         "Annual Energy",                                              "kWh",           "",            "Net_E_Calc",     "*",                       "",                      "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "annual_W_cycle_gross",  "Electrical source - Power cycle gross output",               "MWhe",          "",            "Net_E_Calc",     "*",                       "",                      "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "annual_q_inc_sf_tot",   "Total power incident on the field",                          "MWht",          "",            "Type250",        "*",                       "",                      "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "annual_q_abs_tot",      "Total absorbed energy",                                      "MWht",          "",            "Type250",        "*",                       "",                      "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "annual_q_avail",        "Thermal power produced by the field",                        "MWht",          "",            "Type250",        "*",                       "",                      "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "annual_q_aux",          "Total fossil fuel usage by all plant subsystems",            "MMBTU",        "",             "SumCalc",        "*",                       "",                      "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "annual_q_dump",         "Dumped thermal energy",                                      "MWht",          "",            "Type250",        "*",                       "",                      "" },
	//{ SSC_OUTPUT,       SSC_NUMBER,      "annual_m_dot_makeup",   "Cooling water makeup flow rate",                             "kg/hr",        "",             "Type250",        "*",                       "",                      "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "annual_q_pb",           "Thermal energy to the power block",                          "MWht",          "",            "Type251",        "*",                       "",                      "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "annual_q_to_tes",       "Thermal energy into storage",                                "MWht",          "",            "Type251",        "*",                       "",                      "" },

	// Other single value outputs
//	{ SSC_OUTPUT,       SSC_ARRAY,       "hourly_energy",         "Hourly energy",                                              "kWh",           "",            "Caclulated",     "*",                       "LENGTH=8760",           "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "conversion_factor",     "Gross to Net Conversion Factor",                             "%",             "",            "Calculated",     "*",                       "",                      "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "capacity_factor",       "Capacity factor",                                            "%",              "",            "",               "*",                       "",                      "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "kwh_per_kw",            "First year kWh/kW",                                          "kWh/kW",              "",            "",               "*",                       "",                      "" },
	// TODO - consistent fuel usage and o and m caclulations						                                            					            	              	                         	                      
	{ SSC_OUTPUT,       SSC_NUMBER,      "system_heat_rate",      "System heat rate",                                           "MMBtu/MWh",     "",            "",               "*",                       "",                      "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "annual_fuel_usage",     "Annual fuel usage",                                          "kWht",          "",            "",               "*",                       "",                      "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "annual_total_water_use","Total Annual Water Usage: cycle + mirror washing",           "m3",            "",            "PostProcess",    "*",                       "",                      "" },


	var_info_invalid };



class cm_tcstrough_physical : public tcKernel
{
public:

	cm_tcstrough_physical(tcstypeprovider *prov)
	:tcKernel(prov)
	{
		add_var_info( _cm_vtab_tcstrough_physical );
		//set_store_all_parameters(true); // default is 'false' = only store TCS parameters that match the SSC_OUTPUT variables above
		// performance adjustment factors
		add_var_info(vtab_adjustment_factors);
		add_var_info(vtab_technology_outputs);
	}

	void exec( )
	{
		// **************************************************************************
		// Check weatherfile for timestep and such: use to setup TCS simulation
		weatherfile wfile(as_string("file_name"));
		if( !wfile.ok() ) throw exec_error("Physical Trough", wfile.message());
		if( wfile.has_message() ) log( wfile.message(), SSC_WARNING);

		weather_header hdr;
		wfile.header( &hdr );

		double lat = hdr.lat;	//[deg]
		double lon = hdr.lon;	//[deg]
		double shift = (lon - hdr.tz*15.0);		//[deg]

		size_t hours_year = 8760;
		size_t nrec = wfile.nrecords();
		size_t step_per_hour = nrec / hours_year;
		
		if( step_per_hour < 1 || step_per_hour > 60 || step_per_hour * hours_year != nrec )
			throw exec_error("pvsamv1", util::format("invalid number of data records (%d): must be an integer multiple of 8760", (int)nrec));

		double ts_hour = 1.0 / step_per_hour;
		double start_hour = ts_hour;
		// **************************************************************************
		// **************************************************************************

		bool debug_mode = (__DEBUG__ == 1);  // When compiled in VS debug mode, this will use the trnsys weather file; otherwise, it will attempt to open the file with name that was passed in
		debug_mode = false;

		//Add weather file reader unit
		int weather = 0;
		if(debug_mode) weather = add_unit("trnsys_weatherreader", "TRNSYS weather reader");
		else weather = add_unit("weatherreader", "TCS weather reader");
		// Add tou translator
		int	tou = add_unit("tou_translator", "Time of Use Translator");
		//Add Physical Solar Field Model
		int	type250_solarfield = add_unit( "sam_mw_trough_type250", "Physical Trough Solar Field" );
		//Add Physical Controller Model
		int type251_controller = add_unit( "sam_mw_trough_type251", "Controller" );
		//Add Physical Power Block Model
		int type224_powerblock = add_unit( "sam_mw_pt_type224", "Power Block" );
		//E_net calculator
		int sum_calculator = add_unit( "sam_mw_csp_SumCalcs", "Net Energy Calculator" );

		if(debug_mode)
		{
//			set_unit_value(weather, "file_name", "C:/svn_NREL/main/ssc/tcsdata/typelib/TRNSYS_weather_outputs/tucson_trnsys_weather.out");
			set_unit_value(weather, "file_name", "C:/Projects/SAM/trunk/ssc/tcsdata/typelib/TRNSYS_weather_outputs/tucson_trnsys_weather.out");
			set_unit_value(weather, "i_hour", "TIME");
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
			set_unit_value_ssc_string( weather, "file_name" );
			set_unit_value_ssc_double( weather, "track_mode" );    //, 0 ); SET TO 3 IN TRNSYS FILE, no user input !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
			set_unit_value_ssc_double( weather, "tilt" );          //, 0 );
			set_unit_value_ssc_double( weather, "azimuth" );       //, 0 );
		}

		set_unit_value_ssc_matrix(tou, "weekday_schedule"); // tou values from control will be between 1 and 9
		set_unit_value_ssc_matrix(tou, "weekend_schedule");

		//Set Solar Field Parameters ===========================================
        set_unit_value_ssc_double(type250_solarfield, "latitude", lat);		//[deg]
		set_unit_value_ssc_double(type250_solarfield, "longitude", lon);	//[deg]
		set_unit_value_ssc_double(type250_solarfield, "shift", shift);		//[deg]
		set_unit_value_ssc_double(type250_solarfield, "nSCA" ); // , 8);
        set_unit_value_ssc_double(type250_solarfield, "nHCEt" ); // , 4);
        set_unit_value_ssc_double(type250_solarfield, "nColt" ); // , 4);
        set_unit_value_ssc_double(type250_solarfield, "nHCEVar" ); // , 4);
        set_unit_value_ssc_double(type250_solarfield, "nLoops" ); // , 230);
        set_unit_value_ssc_double(type250_solarfield, "eta_pump" ); // , 0.85);
        set_unit_value_ssc_double(type250_solarfield, "HDR_rough" ); // , 4.57E-05);
        set_unit_value_ssc_double(type250_solarfield, "theta_stow" ); // , 170);
        set_unit_value_ssc_double(type250_solarfield, "theta_dep" ); // , 10);
        set_unit_value_ssc_double(type250_solarfield, "Row_Distance" ); // , 15);
        set_unit_value_ssc_double(type250_solarfield, "FieldConfig" ); // , 2);
        //set_unit_value_ssc_double(type250_solarfield, "T_startup" ); // , 300);
        if (as_boolean("tanks_in_parallel")) {
            set_unit_value_ssc_double(type250_solarfield, "T_recirc", as_double("T_startup"));
        }
        else {
            set_unit_value_ssc_double(type250_solarfield, "T_recirc", as_double("T_tank_hot_inlet_min"));
        }
        set_unit_value_ssc_double(type250_solarfield, "m_dot_htfmin" ); // , 1);
        set_unit_value_ssc_double(type250_solarfield, "m_dot_htfmax" ); // , 12);
        set_unit_value_ssc_double(type250_solarfield, "T_loop_in_des" ); // , 293);
        set_unit_value_ssc_double(type250_solarfield, "T_loop_out" ); // , 391);
        set_unit_value_ssc_double(type250_solarfield, "Fluid" ); // , 21);
        //set_unit_value_ssc_double(type250_solarfield, "T_field_ini" ); // , 150);
		set_unit_value_ssc_matrix(type250_solarfield, "field_fl_props");
		set_unit_value_ssc_double(type250_solarfield, "T_fp" ); // , 150);
        set_unit_value_ssc_double(type250_solarfield, "I_bn_des" ); // , 950);
        set_unit_value_ssc_double(type250_solarfield, "calc_design_pipe_vals"); // , true);
        set_unit_value_ssc_double(type250_solarfield, "V_hdr_cold_max" ); // , 3);
        set_unit_value_ssc_double(type250_solarfield, "V_hdr_cold_min" ); // , 2);
        set_unit_value_ssc_double(type250_solarfield, "V_hdr_hot_max"); // , 3);
        set_unit_value_ssc_double(type250_solarfield, "V_hdr_hot_min"); // , 2);
        set_unit_value_ssc_double(type250_solarfield, "N_max_hdr_diams"); // , 10);
        set_unit_value_ssc_double(type250_solarfield, "L_rnr_pb"); // , 25);
        set_unit_value_ssc_double(type250_solarfield, "L_rnr_per_xpan"); // , 70);
        set_unit_value_ssc_double(type250_solarfield, "L_xpan_hdr"); // , 20);
        set_unit_value_ssc_double(type250_solarfield, "L_xpan_rnr"); // , 20);
        set_unit_value_ssc_double(type250_solarfield, "Min_rnr_xpans"); // , 1);
        set_unit_value_ssc_double(type250_solarfield, "northsouth_field_sep"); // , 20);
        set_unit_value_ssc_double(type250_solarfield, "N_hdr_per_xpan"); // , 2);
        set_unit_value_ssc_double(type250_solarfield, "offset_xpan_hdr"); // , 1);
        set_unit_value_ssc_double(type250_solarfield, "Pipe_hl_coef" ); // , 0.45);
        set_unit_value_ssc_double(type250_solarfield, "SCA_drives_elec" ); // , 125);
        set_unit_value_ssc_double(type250_solarfield, "fthrok" ); // , 1);
        set_unit_value_ssc_double(type250_solarfield, "fthrctrl" ); // , 2);
        set_unit_value_ssc_double(type250_solarfield, "ColTilt", as_double("tilt") ); // , 0);
		set_unit_value_ssc_double(type250_solarfield, "ColAz", as_double("azimuth")); // , 0);
        set_unit_value_ssc_double(type250_solarfield, "accept_mode" ); // , 0);
        set_unit_value_ssc_double(type250_solarfield, "accept_init" ); // , 0);
        set_unit_value_ssc_double(type250_solarfield, "accept_loc" ); // , 1);
        set_unit_value_ssc_double(type250_solarfield, "solar_mult" ); // , 2);
        set_unit_value_ssc_double(type250_solarfield, "mc_bal_hot" ); // , 0.2);
        set_unit_value_ssc_double(type250_solarfield, "mc_bal_cold" ); // , 0.2);
        set_unit_value_ssc_double(type250_solarfield, "mc_bal_sca" ); // , 4.5);
        //set_unit_value_ssc_array(type250_solarfield, "OptCharType" ); // , [1,1,1,1]);
        //set_unit_value_ssc_array(type250_solarfield, "CollectorType" ); // , [1,1,1,1]);
        set_unit_value_ssc_array(type250_solarfield, "W_aperture" ); // , [5,5,5,5]);
        set_unit_value_ssc_array(type250_solarfield, "A_aperture" ); // , [470.3,470.3,470.3,470.3]);
        //set_unit_value_ssc_array(type250_solarfield, "IamF0" ); // , [1,1,1,1]);
        //set_unit_value_ssc_array(type250_solarfield, "IamF1" ); // , [0.0506,0.0506,0.0506,0.0506]);
        //set_unit_value_ssc_array(type250_solarfield, "IamF2" ); // , [-0.1763,-0.1763,-0.1763,-0.1763]);
        //set_unit_value_ssc_array(type250_solarfield, "reflectivity" ); // , [1,1,1,1]);
        set_unit_value_ssc_array(type250_solarfield, "TrackingError" ); // , [0.994,0.994,0.994,0.994]);
        set_unit_value_ssc_array(type250_solarfield, "GeomEffects" ); // , [0.98,0.98,0.98,0.98]);
        set_unit_value_ssc_array(type250_solarfield, "Rho_mirror_clean" ); // , [0.935,0.935,0.935,0.935]);
        set_unit_value_ssc_array(type250_solarfield, "Dirt_mirror" ); // , [0.95,0.95,0.95,0.95]);
        set_unit_value_ssc_array(type250_solarfield, "Error" ); // , [0.99,0.99,0.99,0.99]);
        set_unit_value_ssc_array(type250_solarfield, "Ave_Focal_Length" ); // , [1.8,1.8,1.8,1.8]);
        set_unit_value_ssc_array(type250_solarfield, "L_SCA" ); // , [100,100,100,100]);
        set_unit_value_ssc_array(type250_solarfield, "L_aperture" ); // , [8.33333,8.33333,8.33333,8.33333]);
        set_unit_value_ssc_array(type250_solarfield, "ColperSCA" ); // , [12,12,12,12]);
        set_unit_value_ssc_array(type250_solarfield, "Distance_SCA" ); // , [1,1,1,1]);

		set_unit_value_ssc_matrix(type250_solarfield, "IAM_matrix");

        set_unit_value_ssc_matrix(type250_solarfield, "HCE_FieldFrac" ); // , [[0.985,0.01,0.005,0],[0.985,0.01,0.005,0],[0.985,0.01,0.005,0],[0.985,0.01,0.005,0]]);
        set_unit_value_ssc_matrix(type250_solarfield, "D_2" ); // , [[0.066,0.066,0.066,0.066],[0.066,0.066,0.066,0.066],[0.066,0.066,0.066,0.066],[0.066,0.066,0.066,0.066]]);
        set_unit_value_ssc_matrix(type250_solarfield, "D_3" ); // , [[0.07,0.07,0.07,0.07],[0.07,0.07,0.07,0.07],[0.07,0.07,0.07,0.07],[0.07,0.07,0.07,0.07]]);
        set_unit_value_ssc_matrix(type250_solarfield, "D_4" ); // , [[0.115,0.115,0.115,0.115],[0.115,0.115,0.115,0.115],[0.115,0.115,0.115,0.115],[0.115,0.115,0.115,0.115]]);
        set_unit_value_ssc_matrix(type250_solarfield, "D_5" ); // , [[0.12,0.12,0.12,0.12],[0.12,0.12,0.12,0.12],[0.12,0.12,0.12,0.12],[0.12,0.12,0.12,0.12]]);
        set_unit_value_ssc_matrix(type250_solarfield, "D_p" ); // , [[0,0,0,0],[0,0,0,0],[0,0,0,0],[0,0,0,0]]);
        set_unit_value_ssc_matrix(type250_solarfield, "Flow_type" ); // , [[1,1,1,1],[1,1,1,1],[1,1,1,1],[1,1,1,1]]);
        set_unit_value_ssc_matrix(type250_solarfield, "Rough" ); // , [[4.50E-05,4.50E-05,4.50E-05,4.50E-05],[4.50E-05,4.50E-05,4.50E-05,4.50E-05],[4.50E-05,4.50E-05,4.50E-05,4.50E-05],[4.50E-05,4.50E-05,4.50E-05,4.50E-05]]);
        set_unit_value_ssc_matrix(type250_solarfield, "alpha_env" ); // , [[0.02,0.02,0,0],[0.02,0.02,0,0],[0.02,0.02,0,0],[0.02,0.02,0,0]]);
		set_unit_value_ssc_matrix_transpose(type250_solarfield, "epsilon_3_11"); // , [[100,150,200,250,300,350,400,450,500],[0.064,0.0665,0.07,0.0745,0.08,0.0865,0.094,0.1025,0.112]]);
        set_unit_value_ssc_matrix_transpose(type250_solarfield, "epsilon_3_12" ); // , [[0],[0.65]]);
		set_unit_value_ssc_matrix_transpose(type250_solarfield, "epsilon_3_13"); // , [[0],[0.65]]);
		set_unit_value_ssc_matrix_transpose(type250_solarfield, "epsilon_3_14"); // , [[0],[0]]);
		set_unit_value_ssc_matrix_transpose(type250_solarfield, "epsilon_3_21"); // , [[100,150,200,250,300,350,400,450,500],[0.064,0.0665,0.07,0.0745,0.08,0.0865,0.094,0.1025,0.112]]);
		set_unit_value_ssc_matrix_transpose(type250_solarfield, "epsilon_3_22"); // , [[0],[0.65]]);
		set_unit_value_ssc_matrix_transpose(type250_solarfield, "epsilon_3_23"); // , [[0],[0.65]]);
		set_unit_value_ssc_matrix_transpose(type250_solarfield, "epsilon_3_24"); // , [[0],[0]]);
		set_unit_value_ssc_matrix_transpose(type250_solarfield, "epsilon_3_31"); // , [[100,150,200,250,300,350,400,450,500],[0.064,0.0665,0.07,0.0745,0.08,0.0865,0.094,0.1025,0.112]]);
		set_unit_value_ssc_matrix_transpose(type250_solarfield, "epsilon_3_32"); // , [[0],[0.65]]);
		set_unit_value_ssc_matrix_transpose(type250_solarfield, "epsilon_3_33"); // , [[0],[0.65]]);
		set_unit_value_ssc_matrix_transpose(type250_solarfield, "epsilon_3_34"); // , [[0],[0]]);
		set_unit_value_ssc_matrix_transpose(type250_solarfield, "epsilon_3_41"); // , [[100,150,200,250,300,350,400,450,500],[0.064,0.0665,0.07,0.0745,0.08,0.0865,0.094,0.1025,0.112]]);
		set_unit_value_ssc_matrix_transpose(type250_solarfield, "epsilon_3_42"); // , [[0],[0.65]]);
		set_unit_value_ssc_matrix_transpose(type250_solarfield, "epsilon_3_43"); // , [[0],[0.65]]);
		set_unit_value_ssc_matrix_transpose(type250_solarfield, "epsilon_3_44"); // , [[0],[0]]);
        set_unit_value_ssc_matrix(type250_solarfield, "alpha_abs" ); // , [[0.96,0.96,0.8,0],[0.96,0.96,0.8,0],[0.96,0.96,0.8,0],[0.96,0.96,0.8,0]]);
        set_unit_value_ssc_matrix(type250_solarfield, "Tau_envelope" ); // , [[0.963,0.963,1,0],[0.963,0.963,1,0],[0.963,0.963,1,0],[0.963,0.963,1,0]]);
        set_unit_value_ssc_matrix(type250_solarfield, "EPSILON_4" ); // , [[0.86,0.86,1,0],[0.86,0.86,1,0],[0.86,0.86,1,0],[0.86,0.86,1,0]]);
        set_unit_value_ssc_matrix(type250_solarfield, "EPSILON_5" ); // , [[0.86,0.86,1,0],[0.86,0.86,1,0],[0.86,0.86,1,0],[0.86,0.86,1,0]]);
        set_unit_value_ssc_matrix(type250_solarfield, "GlazingIntactIn" ); // , [[1,1,0,1],[1,1,0,1],[1,1,0,1],[1,1,0,1]]);
        set_unit_value_ssc_matrix(type250_solarfield, "P_a" ); // , [[0.0001,750,750,0],[0.0001,750,750,0],[0.0001,750,750,0],[0.0001,750,750,0]]);
        set_unit_value_ssc_matrix(type250_solarfield, "AnnulusGas" ); // , [[27,1,1,27],[27,1,1,27],[27,1,1,27],[27,1,1,27]]);
        set_unit_value_ssc_matrix(type250_solarfield, "AbsorberMaterial" ); // , [[1,1,1,1],[1,1,1,1],[1,1,1,1],[1,1,1,1]]);
        set_unit_value_ssc_matrix(type250_solarfield, "Shadowing" ); // , [[0.96,0.96,0.96,0.963],[0.96,0.96,0.96,0.963],[0.96,0.96,0.96,0.963],[0.96,0.96,0.96,0.963]]);
        set_unit_value_ssc_matrix(type250_solarfield, "Dirt_HCE" ); // , [[0.98,0.98,1,0.98],[0.98,0.98,1,0.98],[0.98,0.98,1,0.98],[0.98,0.98,1,0.98]]);
        set_unit_value_ssc_matrix(type250_solarfield, "Design_loss" ); // , [[150,1100,1500,0],[150,1100,1500,0],[150,1100,1500,0],[150,1100,1500,0]]);
        set_unit_value_ssc_matrix(type250_solarfield, "SCAInfoArray" ); // , [[1,1],[1,1],[1,1],[1,1],[1,1],[1,1],[1,1],[1,1]]);
        set_unit_value_ssc_array(type250_solarfield, "SCADefocusArray" ); // , [8,7,6,5,4,3,2,1]);
        set_unit_value_ssc_matrix(type250_solarfield, "K_cpnt");
        set_unit_value_ssc_matrix(type250_solarfield, "D_cpnt");
        set_unit_value_ssc_matrix(type250_solarfield, "L_cpnt");
        set_unit_value_ssc_matrix(type250_solarfield, "Type_cpnt");
        set_unit_value_ssc_double(type250_solarfield, "custom_sf_pipe_sizes");
        set_unit_value_ssc_array(type250_solarfield, "sf_rnr_diams");
        set_unit_value_ssc_array(type250_solarfield, "sf_rnr_wallthicks");
        set_unit_value_ssc_array(type250_solarfield, "sf_rnr_lengths");
        set_unit_value_ssc_array(type250_solarfield, "sf_hdr_diams");
        set_unit_value_ssc_array(type250_solarfield, "sf_hdr_wallthicks");
        set_unit_value_ssc_array(type250_solarfield, "sf_hdr_lengths");
			// Set the initial values required from "downstream" types
        set_unit_value_ssc_double(type250_solarfield, "defocus", 1.0); // , 1.);
		set_unit_value_ssc_double(type250_solarfield, "T_cold_in", as_double("T_loop_in_des")); // , 293.);
        set_unit_value_ssc_double(type250_solarfield, "defocus_rel", 0.9);
		//Connect Solar Field Inputs
		bool bConnected = connect(weather, "beam", type250_solarfield, "I_b", 0);
		bConnected &= connect(weather, "tdry", type250_solarfield, "T_db", 0);
		bConnected &= connect(weather, "wspd", type250_solarfield, "V_wind", 0);
		bConnected &= connect(weather, "pres", type250_solarfield, "P_amb", 0);
		bConnected &= connect(weather, "tdew", type250_solarfield, "T_dp", 0);
		bConnected &= connect(weather, "solazi", type250_solarfield, "SolarAz", 0);
		bConnected &= connect(type251_controller, "defocus", type250_solarfield, "defocus" );
		bConnected &= connect(type251_controller, "T_field_in", type250_solarfield, "T_cold_in" );
        bConnected &= connect(type251_controller, "recirculating", type250_solarfield, "recirculating");
        set_unit_value_ssc_double(type250_solarfield, "v_sgs", -999);                                       // indicate that this value should be propagated at the simulation start
        bConnected &= connect(type251_controller, "SGS_vol_tot", type250_solarfield, "v_sgs");              // output first param, input second
        bConnected &= connect(type251_controller, "SGS_P_des_1", type250_solarfield, "DP_SGS_1");

		//Set controller parameters ===========================================
		set_unit_value_ssc_double(type251_controller, "field_fluid", as_double("Fluid") ); // , 21);
		set_unit_value_ssc_matrix(type251_controller, "field_fl_props" ); // , [0]);
		set_unit_value_ssc_matrix(type251_controller, "store_fl_props" );				
		set_unit_value_ssc_double(type251_controller, "store_fluid"); // , 18);
		set_unit_value_ssc_double(type251_controller, "tshours" ); // , 6);
        set_unit_value_ssc_double(type251_controller, "eta_pump"); // , 0.85);
        set_unit_value_ssc_double(type251_controller, "HDR_rough"); // , 4.57E-05);
		set_unit_value_ssc_double(type251_controller, "is_hx" ); // , 1);
		set_unit_value_ssc_double(type251_controller, "dt_hot" ); // , 5);
		set_unit_value_ssc_double(type251_controller, "dt_cold" ); // , 7);
		set_unit_value_ssc_double(type251_controller, "hx_config" ); // , 2);
		set_unit_value_ssc_double(type251_controller, "q_max_aux" ); // , 294.118);
		set_unit_value_ssc_double(type251_controller, "lhv_eff", as_double("eta_lhv") );			// 9.17.14 twn: input lhv here to calculate fuel usage
		set_unit_value_ssc_double(type251_controller, "T_set_aux" ); // , 391);
		set_unit_value_ssc_double(type251_controller, "V_tank_hot_ini" ); // , 1313.43);
		set_unit_value_ssc_double(type251_controller, "T_tank_hot_ini", as_double("T_tank_cold_ini") ); // , 300);
		set_unit_value_ssc_double(type251_controller, "T_tank_cold_ini" ); // , 300);
		set_unit_value_ssc_double(type251_controller, "vol_tank" ); // , 26268.7);
		set_unit_value_ssc_double(type251_controller, "h_tank" ); // , 20);
		set_unit_value_ssc_double(type251_controller, "h_tank_min" ); // , 1);
		set_unit_value_ssc_double(type251_controller, "u_tank" ); // , 0.4);
		set_unit_value_ssc_double(type251_controller, "tank_pairs" ); // , 1);
		set_unit_value_ssc_double(type251_controller, "cold_tank_Thtr" ); // , 250);
		set_unit_value_ssc_double(type251_controller, "hot_tank_Thtr" ); // , 365);
		set_unit_value_ssc_double(type251_controller, "cold_tank_max_heat","tank_max_heat"); // , 25);
		set_unit_value_ssc_double(type251_controller, "hot_tank_max_heat", "tank_max_heat");
		set_unit_value_ssc_double(type251_controller, "T_field_in_des", as_double("T_loop_in_des")); // , 293);
		set_unit_value_ssc_double(type251_controller, "T_field_out_des", as_double("T_loop_out")); // , 391);
        set_unit_value_ssc_double(type251_controller, "tanks_in_parallel"); // , 1 = true);
        set_unit_value_ssc_double(type251_controller, "has_hot_tank_bypass"); // , 0 = false);
        set_unit_value_ssc_double(type251_controller, "T_tank_hot_inlet_min"); // , 400);
        set_unit_value_ssc_double(type251_controller, "calc_design_pipe_vals"); // , 1 = true);
		set_unit_value_ssc_double(type251_controller, "q_pb_design" ); // , 294.118);
		set_unit_value_ssc_double(type251_controller, "W_pb_design" ); // , 111);
		set_unit_value_ssc_double(type251_controller, "cycle_max_frac" ); // , 1.05);
		set_unit_value_ssc_double(type251_controller, "cycle_cutoff_frac" ); // , 0.25);
		set_unit_value_ssc_double(type251_controller, "solarm", as_double("solar_mult") ); // , 2);
		set_unit_value_ssc_double(type251_controller, "pb_pump_coef" ); // , 0.55);
		set_unit_value_ssc_double(type251_controller, "tes_pump_coef" ); // , 0.15);
        set_unit_value_ssc_double(type251_controller, "V_tes_des"); // , 1.85);
        set_unit_value_ssc_double(type251_controller, "custom_tes_p_loss"); // , false);
        set_unit_value_ssc_array(type251_controller, "k_tes_loss_coeffs"); // , []);
        set_unit_value_ssc_double(type251_controller, "custom_sgs_pipe_sizes"); // , []);
        set_unit_value_ssc_array(type251_controller, "sgs_diams"); // , []);
        set_unit_value_ssc_array(type251_controller, "sgs_wallthicks"); // , []);
        set_unit_value_ssc_array(type251_controller, "sgs_lengths"); // , []);
        set_unit_value_ssc_double(type251_controller, "DP_SGS"); // , []);
		set_unit_value_ssc_double(type251_controller, "pb_fixed_par" ); // , 0.0055);
		set_unit_value_ssc_array(type251_controller, "bop_array" ); // , [0,1,0.483,0.517,0]);
		set_unit_value_ssc_array(type251_controller, "aux_array" ); // , [0.02273,1,0.483,0.517,0]);
		set_unit_value_ssc_double(type251_controller, "T_startup" ); // , 300);
		set_unit_value_ssc_double(type251_controller, "fossil_mode" ); // , 1);
		set_unit_value_ssc_double(type251_controller, "fthr_ok", as_double("fthrok") ); // , 1);
		set_unit_value_ssc_double(type251_controller, "nSCA" ); // , 8);
		set_unit_value_ssc_double(type251_controller, "I_bn_des" ); // , 950);
		set_unit_value_ssc_double(type251_controller, "fc_on", 0.0 ); //10.12.15, twn: Hardcode this to 0.0 - don't forecast DNI
		set_unit_value_ssc_double(type251_controller, "q_sby_frac" ); // , 0.2);
		set_unit_value_ssc_double(type251_controller, "t_standby_reset" ); // , 2);
		set_unit_value_ssc_double(type251_controller, "sf_type" ); // , 1);
		set_unit_value_ssc_double(type251_controller, "tes_type" ); // , 1);
		set_unit_value_ssc_array(type251_controller, "tslogic_a" ); // , [0,0,0,0,0,0,0,0,0]);
		set_unit_value_ssc_array(type251_controller, "tslogic_b" ); // , [0,0,0,0,0,0,0,0,0]);
		set_unit_value_ssc_array(type251_controller, "tslogic_c" ); // , [1.05,1,1,1,1,1,1,1,1]);
		set_unit_value_ssc_array(type251_controller, "ffrac" ); // , [0,0,0,0,0,0,0,0,0]);
		set_unit_value_ssc_double(type251_controller, "tc_fill" ); // , 7);
		set_unit_value_ssc_double(type251_controller, "tc_void" ); // , 0.25);
		set_unit_value_ssc_double(type251_controller, "t_dis_out_min" ); // , 500);
		set_unit_value_ssc_double(type251_controller, "t_ch_out_max" ); // , 500);
		set_unit_value_ssc_double(type251_controller, "nodes" ); // , 2000);
		set_unit_value_ssc_double(type251_controller, "f_tc_cold" ); // , 2);
			//Connections to controller
		bConnected &= connect(weather, "beam", type251_controller, "I_bn", 0);
		bConnected &= connect(weather, "tdry", type251_controller, "T_amb", 0);
		bConnected &= connect(type250_solarfield, "m_dot_field_htf", type251_controller, "m_dot_field");
		bConnected &= connect(type224_powerblock, "m_dot_htf_ref", type251_controller, "m_dot_htf_ref");
		bConnected &= connect(type250_solarfield, "T_sys_h", type251_controller, "T_field_out");
		bConnected &= connect(type224_powerblock, "T_htf_cold", type251_controller, "T_pb_out");
		bConnected &= connect(tou, "tou_value", type251_controller, "TOUPeriod");
        bConnected &= connect(type250_solarfield, "T_field_in_at_des", type251_controller, "T_field_in_at_des");
        bConnected &= connect(type250_solarfield, "T_field_out_at_des", type251_controller, "T_field_out_at_des");
        bConnected &= connect(type250_solarfield, "P_field_in_at_des", type251_controller, "P_field_in_at_des");
        bConnected &= connect(type250_solarfield, "defocus_rel", type251_controller, "defocus_prev");   // unique tolerance is just to stand out when debugging


			//Set controller initial values
		set_unit_value_ssc_double(type251_controller, "I_bn", 0.0);           // , 0.);
		set_unit_value_ssc_double(type251_controller, "T_amb", 0.0);          // , 15.);
		set_unit_value_ssc_double(type251_controller, "m_dot_field", 0.0);    // , 0.);
		set_unit_value_ssc_double(type251_controller, "m_dot_htf_ref", 0.0);  // , 0.);
		set_unit_value_ssc_double(type251_controller, "T_field_out", as_double("T_loop_in_des"));    // , 391.);
		set_unit_value(type251_controller, "T_pb_out", as_double("T_loop_in_des")); // , 293.);
        set_unit_value_ssc_double(type251_controller, "defocus_prev", 0.9);  // , 0.);

		//Set Powerblock Parameters ===========================================
		set_unit_value_ssc_double(type224_powerblock, "P_ref", as_double("W_pb_design") ); // , 111);
		set_unit_value_ssc_double(type224_powerblock, "eta_ref" ); // , 0.3774);
		set_unit_value_ssc_double(type224_powerblock, "T_htf_hot_ref", as_double("T_loop_out") ); // , 391);
		set_unit_value_ssc_double(type224_powerblock, "T_htf_cold_ref", as_double("T_loop_in_des") ); // , 293);
		set_unit_value_ssc_double(type224_powerblock, "cycle_max_frac");
		set_unit_value_ssc_double(type224_powerblock, "cycle_cutoff_frac");
		set_unit_value_ssc_double(type224_powerblock, "q_sby_frac"); // , 0.2);
		set_unit_value_ssc_double(type224_powerblock, "startup_time"); // , 0.5);
		set_unit_value_ssc_double(type224_powerblock, "startup_frac"); // , 0.2);
		set_unit_value_ssc_double(type224_powerblock, "pb_pump_coef");
		set_unit_value_ssc_double(type224_powerblock, "HTF", as_double("Fluid")); // , 21);
		set_unit_value_ssc_matrix(type224_powerblock, "field_fl_props");


		set_unit_value_ssc_double(type224_powerblock, "pc_config");

		int pc_config = as_integer("pc_config");

		if(pc_config == 0)
		{
			set_unit_value_ssc_double(type224_powerblock, "dT_cw_ref" ); // , 10);
			set_unit_value_ssc_double(type224_powerblock, "T_amb_des" ); // , 20);
		
			set_unit_value_ssc_double(type224_powerblock, "P_boil" ); // , 100);
			set_unit_value_ssc_double(type224_powerblock, "CT" ); // , 1);
		
			set_unit_value_ssc_double(type224_powerblock, "tech_type" ); // , 2);
			set_unit_value_ssc_double(type224_powerblock, "T_approach" ); // , 5);
			set_unit_value_ssc_double(type224_powerblock, "T_ITD_des" ); // , 16);
			set_unit_value_ssc_double(type224_powerblock, "P_cond_ratio" ); // , 1.0028);
			set_unit_value_ssc_double(type224_powerblock, "pb_bd_frac" ); // , 0.02);
			set_unit_value_ssc_double(type224_powerblock, "P_cond_min" ); // , 1.25);
			set_unit_value_ssc_double(type224_powerblock, "n_pl_inc" ); // , 2);
			set_unit_value_ssc_array(type224_powerblock, "F_wc" ); // , [0,0,0,0,0,0,0,0,0]);
		}
		else if(pc_config == 1)
		{			
			set_unit_value_ssc_double(type224_powerblock, "ud_T_amb_des");
			set_unit_value_ssc_double(type224_powerblock, "ud_f_W_dot_cool_des");
			set_unit_value_ssc_double(type224_powerblock, "ud_m_dot_water_cool_des");
			
			set_unit_value_ssc_double(type224_powerblock, "ud_T_htf_low");
			set_unit_value_ssc_double(type224_powerblock, "ud_T_htf_high");
			set_unit_value_ssc_double(type224_powerblock, "ud_T_amb_low");
			set_unit_value_ssc_double(type224_powerblock, "ud_T_amb_high");
			set_unit_value_ssc_double(type224_powerblock, "ud_m_dot_htf_low");
			set_unit_value_ssc_double(type224_powerblock, "ud_m_dot_htf_high");
			
			set_unit_value_ssc_matrix(type224_powerblock, "ud_T_htf_ind_od");
			set_unit_value_ssc_matrix(type224_powerblock, "ud_T_amb_ind_od");
			set_unit_value_ssc_matrix(type224_powerblock, "ud_m_dot_htf_ind_od");
            set_unit_value_ssc_matrix(type224_powerblock, "ud_ind_od");
		}
		else
		{
			log("The 'pc_config' must be either 0 or 1.\n", SSC_WARNING);
			return;
		}

		//Connect inputs 
		bConnected &= connect(weather, "twet", type224_powerblock, "T_wb", 0);
		bConnected &= connect(weather, "tdry", type224_powerblock, "T_db", 0);
		bConnected &= connect(weather, "pres", type224_powerblock, "P_amb", 0);
		bConnected &= connect(weather, "rhum", type224_powerblock, "rh", 0);
		bConnected &= connect(type251_controller, "T_pb_in", type224_powerblock, "T_htf_hot");
		bConnected &= connect(type251_controller, "m_dot_pb", type224_powerblock, "m_dot_htf");
		bConnected &= connect(type251_controller, "m_dot_pb", type224_powerblock, "demand_var");
		bConnected &= connect(type251_controller, "standby_control", type224_powerblock, "standby_control");
		bConnected &= connect(tou, "tou_value", type224_powerblock, "TOU");
		
		//Set initial values
		set_unit_value_ssc_double(type224_powerblock, "T_db", 0.0); // , 15.); 
		set_unit_value_ssc_double(type224_powerblock, "P_amb", 1.0); // , 1.);

		//Set enet calculator inputs and connect it to the parasitic values ===========================================
		set_unit_value_ssc_double(sum_calculator, "eta_lhv" ); // , 0.9);
		set_unit_value_ssc_double(sum_calculator, "eta_tes_htr" ); // , 0.98);
		set_unit_value_ssc_double(sum_calculator, "fp_mode", 1.0 );				// 10.12.15 twn: hardcode this to Electric freeze protection as it wasn't exposed in the UI. Perhaps revisit allowing using control...
		bConnected &= connect(type224_powerblock, "P_cycle", sum_calculator, "W_cycle_gross");
		bConnected &= connect(type224_powerblock, "W_cool_par", sum_calculator, "W_par_heatrej");
		bConnected &= connect(type250_solarfield, "W_dot_pump", sum_calculator, "W_par_sf_pump");
		bConnected &= connect(type251_controller, "htf_pump_power", sum_calculator, "W_par_tes_pump");
		bConnected &= connect(type251_controller, "bop_par", sum_calculator, "W_par_BOP");
		bConnected &= connect(type251_controller, "fixed_par", sum_calculator, "W_par_fixed");
		bConnected &= connect(type250_solarfield, "SCA_par_tot", sum_calculator, "W_par_tracking");
		bConnected &= connect(type251_controller, "aux_par", sum_calculator, "W_par_aux_boiler");
		bConnected &= connect(type251_controller, "tank_fp_par", sum_calculator, "Q_par_tes_fp");
		bConnected &= connect(type250_solarfield, "E_fp_tot", sum_calculator, "Q_par_sf_fp");
		bConnected &= connect(type251_controller, "q_aux_heat", sum_calculator, "Q_aux_backup");

		// check if all connections worked
		if ( !bConnected )
			throw exec_error( "tcstrough_physical", "there was a problem connecting outputs of one unit to inputs of another for the simulation." );

		// Run simulation
		// size_t hours = 8760; 
		// size_t start_hour = ts_hour;
		// if ( 0 != simulate(3600, hours * 3600, 3600))
		if( 0 != simulate(start_hour*3600.0, hours_year*3600.0, ts_hour*3600.0))
			throw exec_error( "tcstrough_physical", "there was a problem simulating in tcskernel(physical trough)" );

		// get the outputs
		if (!set_all_output_arrays() )
			throw exec_error( "tcstrough_physical", "there was a problem returning the results from the simulation." );
		//set_output_array("i_SfTi",8760);

		size_t count;
		ssc_number_t *timestep_energy_MW = as_array("W_net", &count);			//MW
		ssc_number_t *p_gen = allocate("gen", count);

		char tstr[500];
		std::string out_msg = "hourly energy count %d is incorrect (should be %d)";
		sprintf(tstr, out_msg.c_str(), count, nrec);
		out_msg = tstr;
		if( count != nrec )
			throw exec_error("tcstrough_physical", out_msg);

		//design parameters
		int nv;
        // header
		double *header_diams = get_unit_value(type250_solarfield, "pipe_header_diams", &nv);
		ssc_number_t *header_diams_cm = allocate("pipe_header_diams", nv);
        std::copy(header_diams, header_diams + nv, header_diams_cm);
        double *header_wallthk = get_unit_value(type250_solarfield, "pipe_header_wallthk", &nv);
        ssc_number_t *header_wallthk_cm = allocate("pipe_header_wallthk", nv);
        std::copy(header_wallthk, header_wallthk + nv, header_wallthk_cm);
        double *pipe_header_lengths = get_unit_value(type250_solarfield, "pipe_header_lengths", &nv);
        ssc_number_t *pipe_header_lengths_cm = allocate("pipe_header_lengths", nv);
        std::copy(pipe_header_lengths, pipe_header_lengths + nv, pipe_header_lengths_cm);
        double *pipe_header_expansions = get_unit_value(type250_solarfield, "pipe_header_expansions", &nv);
        ssc_number_t *pipe_header_expansions_cm = allocate("pipe_header_expansions", nv);
        std::copy(pipe_header_expansions, pipe_header_expansions + nv, pipe_header_expansions_cm);
        double *header_massflow_design = get_unit_value(type250_solarfield, "pipe_header_mdot_dsn", &nv);
        ssc_number_t *header_massflow_design_cm = allocate("pipe_header_mdot_dsn", nv);
        std::copy(header_massflow_design, header_massflow_design + nv, header_massflow_design_cm);
        double *header_velocity_design = get_unit_value(type250_solarfield, "pipe_header_vel_dsn", &nv);
        ssc_number_t *header_velocity_design_cm = allocate("pipe_header_vel_dsn", nv);
        std::copy(header_velocity_design, header_velocity_design + nv, header_velocity_design_cm);
        double *header_temp_design = get_unit_value(type250_solarfield, "pipe_header_T_dsn", &nv);
        ssc_number_t *header_temp_design_cm = allocate("pipe_header_T_dsn", nv);
        std::copy(header_temp_design, header_temp_design + nv, header_temp_design_cm);
        double *header_pressure_design = get_unit_value(type250_solarfield, "pipe_header_P_dsn", &nv);
        ssc_number_t *header_pressure_design_cm = allocate("pipe_header_P_dsn", nv);
        std::copy(header_pressure_design, header_pressure_design + nv, header_pressure_design_cm);
        // runner
        double *runner_diams = get_unit_value(type250_solarfield, "pipe_runner_diams", &nv);
		ssc_number_t *runner_diams_cm = allocate("pipe_runner_diams", nv);
        std::copy(runner_diams, runner_diams + nv, runner_diams_cm);
        double *runner_wallthk = get_unit_value(type250_solarfield, "pipe_runner_wallthk", &nv);
        ssc_number_t *runner_wallthk_cm = allocate("pipe_runner_wallthk", nv);
        std::copy(runner_wallthk, runner_wallthk + nv, runner_wallthk_cm);
		double *pipe_runner_lengths = get_unit_value(type250_solarfield, "pipe_runner_lengths", &nv);
		ssc_number_t *pipe_runner_lengths_cm = allocate("pipe_runner_lengths", nv);
        std::copy(pipe_runner_lengths, pipe_runner_lengths + nv, pipe_runner_lengths_cm);
        double *pipe_runner_expansions = get_unit_value(type250_solarfield, "pipe_runner_expansions", &nv);
        ssc_number_t *pipe_runner_expansions_cm = allocate("pipe_runner_expansions", nv);
        std::copy(pipe_runner_expansions, pipe_runner_expansions + nv, pipe_runner_expansions_cm);
        double *runner_massflow_design = get_unit_value(type250_solarfield, "pipe_runner_mdot_dsn", &nv);
        ssc_number_t *runner_massflow_design_cm = allocate("pipe_runner_mdot_dsn", nv);
        std::copy(runner_massflow_design, runner_massflow_design + nv, runner_massflow_design_cm);
        double *runner_velocity_design = get_unit_value(type250_solarfield, "pipe_runner_vel_dsn", &nv);
        ssc_number_t *runner_velocity_design_cm = allocate("pipe_runner_vel_dsn", nv);
        std::copy(runner_velocity_design, runner_velocity_design + nv, runner_velocity_design_cm);
        double *runner_temp_design = get_unit_value(type250_solarfield, "pipe_runner_T_dsn", &nv);
        ssc_number_t *runner_temp_design_cm = allocate("pipe_runner_T_dsn", nv);
        std::copy(runner_temp_design, runner_temp_design + nv, runner_temp_design_cm);
        double *runner_pressure_design = get_unit_value(type250_solarfield, "pipe_runner_P_dsn", &nv);
        ssc_number_t *runner_pressure_design_cm = allocate("pipe_runner_P_dsn", nv);
        std::copy(runner_pressure_design, runner_pressure_design + nv, runner_pressure_design_cm);
        // loop
        double *loop_temp_design = get_unit_value(type250_solarfield, "pipe_loop_T_dsn", &nv);
        ssc_number_t *loop_temp_design_cm = allocate("pipe_loop_T_dsn", nv);
        std::copy(loop_temp_design, loop_temp_design + nv, loop_temp_design_cm);
        double *loop_pressure_design = get_unit_value(type250_solarfield, "pipe_loop_P_dsn", &nv);
        ssc_number_t *loop_pressure_design_cm = allocate("pipe_loop_P_dsn", nv);
        std::copy(loop_pressure_design, loop_pressure_design + nv, loop_pressure_design_cm);
        // SGS
        double *sgs_diams = get_unit_value(type251_controller, "SGS_diams", &nv);
        ssc_number_t *sgs_diams_cm = allocate("pipe_sgs_diams", nv);
        std::copy(sgs_diams, sgs_diams + nv, sgs_diams_cm);
        double *sgs_wallthk = get_unit_value(type251_controller, "SGS_wall_thk", &nv);
        ssc_number_t *sgs_wallthk_cm = allocate("pipe_sgs_wallthk", nv);
        std::copy(sgs_wallthk, sgs_wallthk + nv, sgs_wallthk_cm);
        double *sgs_mdot_dsn = get_unit_value(type251_controller, "SGS_m_dot_des", &nv);
        ssc_number_t *sgs_mdot_dsn_cm = allocate("pipe_sgs_mdot_dsn", nv);
        std::copy(sgs_mdot_dsn, sgs_mdot_dsn + nv, sgs_mdot_dsn_cm);
        double *sgs_vel_dsn = get_unit_value(type251_controller, "SGS_vel_des", &nv);
        ssc_number_t *sgs_vel_dsn_cm = allocate("pipe_sgs_vel_dsn", nv);
        std::copy(sgs_vel_dsn, sgs_vel_dsn + nv, sgs_vel_dsn_cm);
        double *sgs_T_dsn = get_unit_value(type251_controller, "SGS_T_des", &nv);
        ssc_number_t *sgs_T_dsn_cm = allocate("pipe_sgs_T_dsn", nv);
        std::copy(sgs_T_dsn, sgs_T_dsn + nv, sgs_T_dsn_cm);
        double *sgs_P_dsn = get_unit_value(type251_controller, "SGS_P_des", &nv);
        ssc_number_t *sgs_P_dsn_cm = allocate("pipe_sgs_P_dsn", nv);
        std::copy(sgs_P_dsn, sgs_P_dsn + nv, sgs_P_dsn_cm);
		
		// performance adjustment factors
		adjustment_factors haf(this, "adjust");
		if (!haf.setup())
			throw exec_error("tcstrough_physical", "failed to setup adjustment factors: " + haf.error());

		size_t idx=0;
		ssc_number_t ae = 0;
		// Need to define an hourly array from potentially subhourly data
		for( size_t i_ts = 0; i_ts < hours_year; i_ts++ )
		{
			double ts_power = 0;
			for( size_t j_sh = 0; j_sh < step_per_hour; j_sh++ )
			{
				// convert MWh to kWh 
				ts_power += timestep_energy_MW[i_ts*step_per_hour + j_sh] * 1000.0;
				// apply performance adjustments
				p_gen[idx] = (ssc_number_t)(timestep_energy_MW[i_ts*step_per_hour + j_sh] * 1000.0 * haf(i_ts));
				idx++;
			}
			ae += (ssc_number_t)(ts_power * ts_hour); // honoring Ty's wishes below
		}

		//1.7.15, twn: Need to calculated the conversion factor before the performance adjustments are applied to "hourly energy"
		double annual_energy = accumulate_annual("gen", "annual_energy", ts_hour);						// already in kWh
		accumulate_annual("W_cycle_gross", "annual_W_cycle_gross", ts_hour * 1000);	// convert from MW to kWh
		// Calculated outputs
		ssc_number_t pg = as_number("annual_W_cycle_gross");
		ssc_number_t convfactor = (pg != 0) ? 100 * ae / pg : 0;
		assign("conversion_factor", convfactor);
	
		// First, sum power cycle water consumption timeseries outputs
		accumulate_annual_for_year("m_dot_makeup", "annual_total_water_use", 1.0/1000.0*ts_hour, step_per_hour); //[m^3], convert from kg
		// Then, add water usage from mirror cleaning
		ssc_number_t V_water_cycle = as_number("annual_total_water_use");
		double A_aper_tot = get_unit_value_number(type250_solarfield, "A_aper_tot");
		double V_water_mirrors = as_double("water_usage_per_wash")/1000.0*A_aper_tot*as_double("washing_frequency");
		assign("annual_total_water_use", (ssc_number_t)(V_water_cycle + V_water_mirrors));

		// Monthly accumulations
		accumulate_monthly("gen", "monthly_energy", ts_hour); // already in kWh
		accumulate_monthly("W_cycle_gross", "monthly_W_cycle_gross", ts_hour);
		accumulate_monthly("q_inc_sf_tot", "monthly_q_inc_sf_tot", ts_hour);
		accumulate_monthly("q_abs_tot", "monthly_q_abs_tot", ts_hour);
		accumulate_monthly("q_avail", "monthly_q_avail", ts_hour);
		accumulate_monthly("Fuel_usage", "monthly_Fuel_usage", ts_hour);
		accumulate_monthly("q_dump", "monthly_q_dump", ts_hour);
		accumulate_monthly("m_dot_makeup", "monthly_m_dot_makeup", ts_hour);
		accumulate_monthly("q_pb", "monthly_q_pb", ts_hour);
		accumulate_monthly("q_to_tes", "monthly_q_to_tes", ts_hour);

		// Annual accumulations
		accumulate_annual("W_cycle_gross", "annual_W_cycle_gross", ts_hour);
		accumulate_annual("q_inc_sf_tot", "annual_q_inc_sf_tot", ts_hour);
		accumulate_annual("q_abs_tot", "annual_q_abs_tot", ts_hour);
		accumulate_annual("q_avail", "annual_q_avail", ts_hour);
		double fuel_usage_mmbtu = accumulate_annual("Fuel_usage", "annual_q_aux", ts_hour);
		accumulate_annual("q_dump", "annual_q_dump", ts_hour);
		//accumulate_annual("m_dot_makeup", "annual_m_dot_makeup", ts_hour);
		accumulate_annual("q_pb", "annual_q_pb", ts_hour);
		accumulate_annual("q_to_tes", "annual_q_to_tes", ts_hour);
		
		// metric outputs moved to technology
		double kWhperkW = 0.0;
		double nameplate = as_double("system_capacity");
		if (nameplate > 0) kWhperkW = annual_energy / nameplate;
		assign("capacity_factor", var_data((ssc_number_t)(kWhperkW / 87.6))); 
		assign("kwh_per_kw", var_data((ssc_number_t)kWhperkW));
		assign("system_heat_rate", (ssc_number_t)3.413); // samsim tcstrough_physical
		// www.unitjuggler.com/convert-energy-from-MMBtu-to-kWh.html
		assign("annual_fuel_usage", var_data((ssc_number_t)(fuel_usage_mmbtu * 293.297)));
	}

};

DEFINE_TCS_MODULE_ENTRY( tcstrough_physical, "CSP model using the emperical trough TCS types.", 4 )
