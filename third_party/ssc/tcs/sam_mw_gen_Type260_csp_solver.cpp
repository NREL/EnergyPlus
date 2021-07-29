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

#define _TCSTYPEINTERFACE_
#include "tcstype.h"
//#include "htf_props.h"
#include "sam_csp_util.h"

#include <cmath>

#include "csp_solver_gen_collector_receiver.h"
#include "csp_solver_pc_gen.h"
#include "csp_solver_util.h"

using namespace std;

/*
************************************************************************
 Object: Generic solar model
 Simulation Studio Model: Type260
 
 Author: Michael J. Wagner
 Date:	 May 29, 2013

 COPYRIGHT 2013 NATIONAL RENEWABLE ENERGY LABORATORY
*/


enum{
	//parameters and inputs
	P_LATITUDE,
	P_LONGITUDE,
	P_TIMEZONE,
	P_THETA_STOW,
	P_THETA_DEP,
	P_INTERP_ARR,
	P_RAD_TYPE,
	P_SOLARM,
	P_T_SFDES,
	P_IRR_DES,
	P_ETA_OPT_SOIL,
	P_ETA_OPT_GEN,
	P_F_SFHL_REF,
	P_SFHLQ_COEFS,
	P_SFHLT_COEFS,
	P_SFHLV_COEFS,
	P_QSF_DES,
	P_W_DES,
	P_ETA_DES,
	P_F_WMAX,
	P_F_WMIN,
	P_F_STARTUP,
	P_ETA_LHV,
	P_ETAQ_COEFS,
	P_ETAT_COEFS,
	P_T_PCDES,
	P_PC_T_CORR,
	P_F_WPAR_FIXED,
	P_F_WPAR_PROD,
	P_WPAR_PRODQ_COEFS,
	P_WPAR_PRODT_COEFS,
	P_WPAR_PRODD_COEFS,
	P_HRS_TES,
	P_F_CHARGE,
	P_F_DISCH,
	P_F_ETES_0,
	P_F_TESHL_REF,
	P_TESHLX_COEFS,
	P_TESHLT_COEFS,
	P_NTOD,
	//P_TOD_SCHED,
	P_DISWS,
	P_DISWOS,
	P_QDISP,
	P_FDISP,
    P_ISTABLEUNSORTED,
	P_OPTICALTABLE,
    //P_OPTICALTABLEUNS,
    P_ADJUST,
    P_EXERGY_TABLE,
    P_STORAGE_CONFIG,

	I_IBN,
	I_IBH,
	I_ITOTH,
	I_TDB,
	I_TWB,
	I_VWIND,
	I_TOUPeriod,

	O_IRR_USED,
	O_HOUR_OF_DAY,
	O_DAY_OF_YEAR,
	O_DECLINATION,
	O_SOLTIME,
	O_HRANGLE,
	O_SOLALT,
	O_SOLAZ,
	O_ETA_OPT_SF,
	O_F_SFHL_QDNI,
	O_F_SFHL_TAMB,
	O_F_SFHL_VWIND,
	O_Q_HL_SF,
	O_Q_SF,
	O_Q_INC,
	O_PBMODE,
	O_PBSTARTF,
	O_Q_TO_PB,
	O_Q_STARTUP,
	O_Q_TO_TES,
	O_Q_FROM_TES,
	O_E_IN_TES,
	O_Q_HL_TES,
	O_Q_DUMP_TESFULL,
	O_Q_DUMP_TESCHG,
	O_Q_DUMP_UMIN,
	O_Q_DUMP_TOT,
	O_Q_FOSSIL,
	O_Q_GAS,
	O_F_EFFPC_QTPB,
	O_F_EFFPC_TAMB,
	O_ETA_CYCLE,
	O_W_GR_SOLAR,
	O_W_GR_FOSSIL,
	O_W_GR,
	O_W_PAR_FIXED,
	O_W_PAR_PROD,
	O_W_PAR_TOT,
	O_W_PAR_ONLINE,
	O_W_PAR_OFFLINE,
	O_ENET,

	//Include N_max
	N_MAX
};

tcsvarinfo sam_mw_gen_type260_variables[] = {
	{ TCS_PARAM,          TCS_NUMBER,          P_LATITUDE,               "latitude",                                                                           "Site latitude",          "deg",             "",             "",           "35" },
	{ TCS_PARAM,          TCS_NUMBER,         P_LONGITUDE,              "longitude",                                                                          "Site longitude",          "deg",             "",             "",         "-117" },
	{ TCS_PARAM,          TCS_NUMBER,          P_TIMEZONE,               "timezone",                                                                           "Site timezone",           "hr",             "",             "",           "-8" },
	{ TCS_PARAM,          TCS_NUMBER,        P_THETA_STOW,             "theta_stow",                          "Solar elevation angle at which the solar field stops operating",          "deg",             "",             "",          "170" },
	{ TCS_PARAM,          TCS_NUMBER,         P_THETA_DEP,              "theta_dep",                         "Solar elevation angle at which the solar field begins operating",          "deg",             "",             "",           "10" },
	{ TCS_PARAM,          TCS_NUMBER,        P_INTERP_ARR,             "interp_arr",                         "Interpolate the array or find nearest neighbor? (1=interp,2=no)",         "none",             "",             "",            "1" },
	{ TCS_PARAM,          TCS_NUMBER,          P_RAD_TYPE,               "rad_type",                          "Solar resource radiation type (1=DNI,2=horiz.beam,3=tot.horiz)",         "none",             "",             "",            "1" },
	{ TCS_PARAM,          TCS_NUMBER,            P_SOLARM,                 "solarm",                                                                          "Solar multiple",         "none",             "",             "",            "2" },
	{ TCS_PARAM,          TCS_NUMBER,           P_T_SFDES,                "T_sfdes",                                         "Solar field design point temperature (dry bulb)",            "C",             "",             "",           "25" },
	{ TCS_PARAM,          TCS_NUMBER,           P_IRR_DES,                "irr_des",                                                                "Irradiation design point",         "W/m2",             "",             "",          "950" },
	{ TCS_PARAM,          TCS_NUMBER,      P_ETA_OPT_SOIL,           "eta_opt_soil",                                                           "Soiling optical derate factor",         "none",             "",             "",         "0.95" },
	{ TCS_PARAM,          TCS_NUMBER,       P_ETA_OPT_GEN,            "eta_opt_gen",                                                            "General/other optical derate",         "none",             "",             "",         "0.99" },
	{ TCS_PARAM,          TCS_NUMBER,        P_F_SFHL_REF,             "f_sfhl_ref",                                             "Reference solar field thermal loss fraction",     "MW/MWcap",             "",             "",     "0.071591" },
	{ TCS_PARAM,           TCS_ARRAY,       P_SFHLQ_COEFS,            "sfhlQ_coefs",                              "Irr-based solar field thermal loss adjustment coefficients",        "1/MWt",             "",             "",   "1,-0.1,0,0" },
	{ TCS_PARAM,           TCS_ARRAY,       P_SFHLT_COEFS,            "sfhlT_coefs",                            "Temp.-based solar field thermal loss adjustment coefficients",          "1/C",             "",             "",  "1,0.005,0,0" },
	{ TCS_PARAM,           TCS_ARRAY,       P_SFHLV_COEFS,            "sfhlV_coefs",                             "Wind-based solar field thermal loss adjustment coefficients",      "1/(m/s)",             "",             "",   "1,0.01,0,0" },
	{ TCS_PARAM,          TCS_NUMBER,           P_QSF_DES,                "qsf_des",                                                "Solar field thermal production at design",          "MWt",             "",             "",          "628" },
	{ TCS_PARAM,          TCS_NUMBER,             P_W_DES,                  "w_des",                                                         "Design power cycle gross output",          "MWe",             "",             "",          "110" },
	{ TCS_PARAM,          TCS_NUMBER,           P_ETA_DES,                "eta_des",                                                     "Design power cycle gross efficiency",         "none",             "",             "",         "0.35" },
	{ TCS_PARAM,          TCS_NUMBER,            P_F_WMAX,                 "f_wmax",                                      "Maximum over-design power cycle operation fraction",         "none",             "",             "",         "1.05" },
	{ TCS_PARAM,          TCS_NUMBER,            P_F_WMIN,                 "f_wmin",                                        "Minimum part-load power cycle operation fraction",         "none",             "",             "",         "0.25" },
	{ TCS_PARAM,          TCS_NUMBER,         P_F_STARTUP,              "f_startup",                            "Equivalent full-load hours required for power system startup",        "hours",             "",             "",          "0.2" },
	{ TCS_PARAM,          TCS_NUMBER,           P_ETA_LHV,                "eta_lhv",                                            "Fossil backup lower heating value efficiency",         "none",             "",             "",          "0.9" },
	{ TCS_PARAM,           TCS_ARRAY,        P_ETAQ_COEFS,             "etaQ_coefs",                           "Part-load power conversion efficiency adjustment coefficients",        "1/MWt",             "",             "","0.9,0.1,0,0,0" },
	{ TCS_PARAM,           TCS_ARRAY,        P_ETAT_COEFS,             "etaT_coefs",                               "Temp.-based power conversion efficiency adjustment coefs.",          "1/C",             "",             "","1,-0.002,0,0,0" },
	{ TCS_PARAM,          TCS_NUMBER,           P_T_PCDES,                "T_pcdes",                                                  "Power conversion reference temperature",            "C",             "",             "",           "21" },
	{ TCS_PARAM,          TCS_NUMBER,         P_PC_T_CORR,              "PC_T_corr",                           "Power conversion temperature correction mode (1=wetb, 2=dryb)",         "none",             "",             "",            "1" },
	{ TCS_PARAM,          TCS_NUMBER,      P_F_WPAR_FIXED,           "f_Wpar_fixed",                                            "Fixed capacity-based parasitic loss fraction",    "MWe/MWcap",             "",             "",       "0.0055" },
	{ TCS_PARAM,          TCS_NUMBER,       P_F_WPAR_PROD,            "f_Wpar_prod",                                                "Production-based parasitic loss fraction",      "MWe/MWe",             "",             "",         "0.08" },
	{ TCS_PARAM,           TCS_ARRAY,  P_WPAR_PRODQ_COEFS,       "Wpar_prodQ_coefs",                                        "Part-load production parasitic adjustment coefs.",        "1/MWe",             "",             "",      "1,0,0,0" },
	{ TCS_PARAM,           TCS_ARRAY,  P_WPAR_PRODT_COEFS,       "Wpar_prodT_coefs",                                      "Temp.-based production parasitic adjustment coefs.",          "1/C",             "",             "",      "0,0,0,0" },
	{ TCS_PARAM,           TCS_ARRAY,  P_WPAR_PRODD_COEFS,       "Wpar_prodD_coefs",                                        "DNI-based production parasitic adjustment coefs.",         "m2/W",             "",             "",      "0,0,0,0" },
	{ TCS_PARAM,          TCS_NUMBER,           P_HRS_TES,                "hrs_tes",                                                   "Equivalent full-load hours of storage",        "hours",             "",             "",            "6" },
	{ TCS_PARAM,          TCS_NUMBER,          P_F_CHARGE,               "f_charge",                                                          "Storage charging energy derate",         "none",             "",             "",         "0.98" },
	{ TCS_PARAM,          TCS_NUMBER,           P_F_DISCH,                "f_disch",                                                       "Storage discharging energy derate",         "none",             "",             "",         "0.98" },
	{ TCS_PARAM,          TCS_NUMBER,          P_F_ETES_0,               "f_etes_0",                               "Initial fractional charge level of thermal storage (0..1)",         "none",             "",             "",          "0.1" },
	{ TCS_PARAM,          TCS_NUMBER,       P_F_TESHL_REF,            "f_teshl_ref",                                "Reference heat loss from storage per max stored capacity","kWt/MWhr-stored",             "",             "",         "0.35" },
	{ TCS_PARAM,           TCS_ARRAY,      P_TESHLX_COEFS,           "teshlX_coefs",                                   "Charge-based thermal loss adjustment - constant coef.","1/MWhr-stored",             "",             "",      "1,0,0,0" },
	{ TCS_PARAM,           TCS_ARRAY,      P_TESHLT_COEFS,           "teshlT_coefs",                                    "Temp.-based thermal loss adjustment - constant coef.",          "1/C",             "",             "",      "1,0,0,0" },
	{ TCS_PARAM,          TCS_NUMBER,              P_NTOD,                   "ntod",                             "Number of time-of-dispatch periods in the dispatch schedule",         "none",             "",             "",            "9" },
	{ TCS_PARAM,           TCS_ARRAY,             P_DISWS,                  "disws",                                      "Time-of-dispatch control for with-solar conditions",         "none",             "",             "","0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1" },
	{ TCS_PARAM,           TCS_ARRAY,            P_DISWOS,                 "diswos",                                   "Time-of-dispatch control for without-solar conditions",         "none",             "",             "","0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1" },
	{ TCS_PARAM,           TCS_ARRAY,             P_QDISP,                  "qdisp",                                                        "TOD power output control factors",         "none",             "",             "","1,1,1,1,1,1,1,1,1" },
	{ TCS_PARAM,           TCS_ARRAY,             P_FDISP,                  "fdisp",                                                    "Fossil backup output control factors",         "none",             "",             "","0,0,0,0,0,0,0,0,0" },
    { TCS_PARAM,          TCS_NUMBER,   P_ISTABLEUNSORTED,        "istableunsorted",                                                "Is optical table unsorted? (1=yes, 0=no)",         "none",             "",             "",            "0" },
    { TCS_PARAM,          TCS_MATRIX,      P_OPTICALTABLE,           "OpticalTable",                                                                           "Optical table",         "none",             "",             "",             "" },
	{ TCS_PARAM,           TCS_ARRAY,             P_ADJUST,             "sf_adjust",                                           "Time series solar field production adjustment",         "none",             "",             "",             "" },
    { TCS_PARAM,          TCS_MATRIX,      P_EXERGY_TABLE,           "exergy_table",                                        "Exergy penalty as a function of TES charge state",         "none",             "",             "",             "" },
    { TCS_PARAM,          TCS_NUMBER,      P_STORAGE_CONFIG,       "storage_config",                                                    "Thermal energy storage configuration",         "none",             "",             "",             "" },

	{ TCS_INPUT,          TCS_NUMBER,               I_IBN,                    "ibn",                                                           "Beam-normal (DNI) irradiation",         "W/m2",             "",             "",             "" },
	{ TCS_INPUT,          TCS_NUMBER,               I_IBH,                    "ibh",                                                             "Beam-horizontal irradiation",         "W/m2",             "",             "",             "" },
	{ TCS_INPUT,          TCS_NUMBER,             I_ITOTH,                  "itoth",                                                            "Total horizontal irradiation",         "W/m2",             "",             "",             "" },
	{ TCS_INPUT,          TCS_NUMBER,               I_TDB,                    "tdb",                                                            "Ambient dry-bulb temperature",            "C",             "",             "",             "" },
	{ TCS_INPUT,          TCS_NUMBER,               I_TWB,                    "twb",                                                            "Ambient wet-bulb temperature",            "C",             "",             "",             "" },
	{ TCS_INPUT,          TCS_NUMBER,             I_VWIND,                  "vwind",                                                                           "Wind velocity",          "m/s",             "",             "",             "" },
	{ TCS_INPUT,          TCS_NUMBER,         I_TOUPeriod,              "TOUPeriod",                                                                  "The time-of-use period",             "",             "",             "",             "" },

	{ TCS_OUTPUT,          TCS_NUMBER,          O_IRR_USED,               "irr_used",                                                    "Irradiation value used in simulation",         "W/m2",             "",             "",             "" },
	{ TCS_OUTPUT,          TCS_NUMBER,       O_HOUR_OF_DAY,            "hour_of_day",                                                                         "Hour of the day",         "hour",             "",             "",             "" },
	{ TCS_OUTPUT,          TCS_NUMBER,       O_DAY_OF_YEAR,            "day_of_year",                                                                         "Day of the year",          "day",             "",             "",             "" },
	{ TCS_OUTPUT,          TCS_NUMBER,       O_DECLINATION,            "declination",                                                                       "Declination angle",          "deg",             "",             "",             "" },
	{ TCS_OUTPUT,          TCS_NUMBER,           O_SOLTIME,                "soltime",                                                            "[hour] Solar time of the day",         "hour",             "",             "",             "" },
	{ TCS_OUTPUT,          TCS_NUMBER,           O_HRANGLE,                "hrangle",                                                                              "Hour angle",          "deg",             "",             "",             "" },
	{ TCS_OUTPUT,          TCS_NUMBER,            O_SOLALT,                 "solalt",                                                                   "Solar elevation angle",          "deg",             "",             "",             "" },
	{ TCS_OUTPUT,          TCS_NUMBER,             O_SOLAZ,                  "solaz",                                             "Solar azimuth angle (-180..180, 0deg=South)",          "deg",             "",             "",             "" },
	{ TCS_OUTPUT,          TCS_NUMBER,        O_ETA_OPT_SF,             "eta_opt_sf",                                                          "Solar field optical efficiency",         "none",             "",             "",             "" },
	{ TCS_OUTPUT,          TCS_NUMBER,       O_F_SFHL_QDNI,            "f_sfhl_qdni",                                          "Solar field load-based thermal loss correction",         "none",             "",             "",             "" },
	{ TCS_OUTPUT,          TCS_NUMBER,       O_F_SFHL_TAMB,            "f_sfhl_tamb",                                         "Solar field temp.-based thermal loss correction",         "none",             "",             "",             "" },
	{ TCS_OUTPUT,          TCS_NUMBER,      O_F_SFHL_VWIND,           "f_sfhl_vwind",                                          "Solar field wind-based thermal loss correction",         "none",             "",             "",             "" },
	{ TCS_OUTPUT,          TCS_NUMBER,           O_Q_HL_SF,                "q_hl_sf",                                                              "Solar field thermal losses",          "MWt",             "",             "",             "" },
	{ TCS_OUTPUT,          TCS_NUMBER,              O_Q_SF,                   "q_sf",                                                     "Solar field delivered thermal power",          "MWt",             "",             "",             "" },
	{ TCS_OUTPUT,          TCS_NUMBER,             O_Q_INC,                  "q_inc",                                         "Qdni - Solar incident energy, before all losses",          "MWt",             "",             "",             "" },
	{ TCS_OUTPUT,          TCS_NUMBER,            O_PBMODE,                 "pbmode",                                                                   "Power conversion mode",         "none",             "",             "",             "" },
	{ TCS_OUTPUT,          TCS_NUMBER,          O_PBSTARTF,               "pbstartf",                                                    "Flag indicating power system startup",         "none",             "",             "",             "" },
	{ TCS_OUTPUT,          TCS_NUMBER,           O_Q_TO_PB,                "q_to_pb",                                           "Thermal energy to the power conversion system",          "MWt",             "",             "",             "" },
	{ TCS_OUTPUT,          TCS_NUMBER,         O_Q_STARTUP,              "q_startup",                                                         "Power conversion startup energy",          "MWt",             "",             "",             "" },
	{ TCS_OUTPUT,          TCS_NUMBER,          O_Q_TO_TES,               "q_to_tes",                                                             "Thermal energy into storage",          "MWt",             "",             "",             "" },
	{ TCS_OUTPUT,          TCS_NUMBER,        O_Q_FROM_TES,             "q_from_tes",                                                             "Thermal energy from storage",          "MWt",             "",             "",             "" },
	{ TCS_OUTPUT,          TCS_NUMBER,          O_E_IN_TES,               "e_in_tes",                                                                       "Energy in storage",       "MWt-hr",             "",             "",             "" },
	{ TCS_OUTPUT,          TCS_NUMBER,          O_Q_HL_TES,               "q_hl_tes",                                                             "Thermal losses from storage",          "MWt",             "",             "",             "" },
	{ TCS_OUTPUT,          TCS_NUMBER,    O_Q_DUMP_TESFULL,         "q_dump_tesfull",                                       "Dumped energy  exceeding storage charge level max",          "MWt",             "",             "",             "" },
	{ TCS_OUTPUT,          TCS_NUMBER,     O_Q_DUMP_TESCHG,          "q_dump_teschg",                                   "Dumped energy exceeding exceeding storage charge rate",          "MWt",             "",             "",             "" },
	{ TCS_OUTPUT,          TCS_NUMBER,       O_Q_DUMP_UMIN,            "q_dump_umin",                                "Dumped energy from falling below min. operation fraction",          "MWt",             "",             "",             "" },
	{ TCS_OUTPUT,          TCS_NUMBER,        O_Q_DUMP_TOT,             "q_dump_tot",                                                                     "Total dumped energy",          "MWt",             "",             "",             "" },
	{ TCS_OUTPUT,          TCS_NUMBER,          O_Q_FOSSIL,               "q_fossil",                                                 "thermal energy supplied from aux firing",          "MWt",             "",             "",             "" },
	{ TCS_OUTPUT,          TCS_NUMBER,             O_Q_GAS,                  "q_gas",                                          "Energy content of fuel required to supply Qfos",          "MWt",             "",             "",             "" },
	{ TCS_OUTPUT,          TCS_NUMBER,      O_F_EFFPC_QTPB,           "f_effpc_qtpb",                                             "Load-based conversion efficiency correction",         "none",             "",             "",             "" },
	{ TCS_OUTPUT,          TCS_NUMBER,      O_F_EFFPC_TAMB,           "f_effpc_tamb",                                             "Temp-based conversion efficiency correction",         "none",             "",             "",             "" },
	{ TCS_OUTPUT,          TCS_NUMBER,         O_ETA_CYCLE,              "eta_cycle",                                                    "Adjusted power conversion efficiency",         "none",             "",             "",             "" },
	{ TCS_OUTPUT,          TCS_NUMBER,        O_W_GR_SOLAR,             "w_gr_solar",                                                 "Power produced from the solar component",          "MWe",             "",             "",             "" },
	{ TCS_OUTPUT,          TCS_NUMBER,       O_W_GR_FOSSIL,            "w_gr_fossil",                                                "Power produced from the fossil component",          "MWe",             "",             "",             "" },
	{ TCS_OUTPUT,          TCS_NUMBER,              O_W_GR,                   "w_gr",                                                            "Total gross power production",          "MWe",             "",             "",             "" },
	{ TCS_OUTPUT,          TCS_NUMBER,       O_W_PAR_FIXED,            "w_par_fixed",                                                                  "Fixed parasitic losses",          "MWe",             "",             "",             "" },
	{ TCS_OUTPUT,          TCS_NUMBER,        O_W_PAR_PROD,             "w_par_prod",                                                       "Production-based parasitic losses",          "MWe",             "",             "",             "" },
	{ TCS_OUTPUT,          TCS_NUMBER,         O_W_PAR_TOT,              "w_par_tot",                                                                  "Total parasitic losses",          "MWe",             "",             "",             "" },
	{ TCS_OUTPUT,          TCS_NUMBER,      O_W_PAR_ONLINE,           "w_par_online",                                                                       "Online parasitics",          "MWe",             "",             "",             "" },
	{ TCS_OUTPUT,          TCS_NUMBER,     O_W_PAR_OFFLINE,          "w_par_offline",                                                                      "Offline parasitics",          "MWe",             "",             "",             "" },
	{ TCS_OUTPUT,          TCS_NUMBER,              O_ENET,                   "enet",                                                                     "Net electric output",          "MWe",             "",             "",             "" },

	{ TCS_INVALID,    TCS_INVALID,    N_MAX,                0,                    0,                                                        0,                0,        0,        0 }
};


static const double zen_scale = 1.570781477;
static const double az_scale = 6.283125908;


class sam_mw_gen_type260 : public tcstypeinterface
{
private:
	// Add member class for csp-solver generic component models
	C_csp_gen_collector_receiver mc_gen_cr;
	C_csp_gen_collector_receiver::S_params *mp_params; // = &mc_gen_cr.ms_params;
	C_csp_collector_receiver::S_csp_cr_solved_params mc_cr_des_solved;

	C_pc_gen mc_gen_pc;
	C_pc_gen::S_params *mp_pc_params; // = &mc_gen_pc.ms_params;
	C_pc_gen::S_solved_params mc_pc_des_solved;


	double pi,Pi,d2r,r2d, g, mtoinch;

	double eta_lhv;		//Fossil backup lower heating value efficiency

	double T_pcdes;

	int PC_T_corr;

	double f_Wpar_fixed;		//Fixed capacity-based parasitic loss fraction
	double f_Wpar_prod;		//Production-based parasitic loss fraction
	double* Wpar_prodQ_coefs;		//Part-load production parasitic adjustment coefs.
	int nval_Wpar_prodQ_coefs;
	double* Wpar_prodT_coefs;		//Temp.-based production parasitic adjustment coefs.
	int nval_Wpar_prodT_coefs;
	double* Wpar_prodD_coefs;		//Temp.-based production parasitic adjustment coefs.
	int nval_Wpar_prodD_coefs;
	double hrs_tes;		//Equivalent full-load hours of storage
	double f_charge;		//Storage charging energy derate
	double f_disch;		//Storage discharging energy derate
	double f_etes_0;		//Initial fractional charge level of thermal storage (0..1)
	double f_teshl_ref;		//Reference heat loss from storage per max stored capacity
	double* teshlX_coefs;		//Charge-based thermal loss adjustment - constant coef.
	int nval_teshlX_coefs;
	double* teshlT_coefs;		//Temp.-based thermal loss adjustment - constant coef.
	int nval_teshlT_coefs;
	int ntod;		//Number of time-of-dispatch periods in the dispatch schedule
    int storage_config; //Direct storage=0, Indirect storage=1
	//double* tod_sched;		//Array of touperiod indices
	int nval_tod_sched;
	double* disws;		//Time-of-dispatch control for with-solar conditions
	int nval_disws;
	double* diswos;		//Time-of-dispatch control for without-solar conditions
	int nval_diswos;
	double* qdisp;		//touperiod power output control factors
	int nval_qdisp;
	double* fdisp;		//Fossil backup output control factors
	int nval_fdisp;
    double* sf_adjust;
    int nval_sf_adjust;
    double* exergy_table_in;		
	int nrow_exergy_table, ncol_exergy_table; 
    util::matrix_t<double> exergy_table_T;

	int pbmode;		//Power conversion mode

	// Calculated design variables
	double m_q_des;		//[MWt] Thermal power to cycle
	double m_qttmin;	//[MWt] Min allowable thermal power to cycle
	double m_qttmax;	//[MWt] Max allowable thermal power to cycle
	double m_q_startup;	//[MWt-hr] Startup energy
	// Stored
	double m_e_in_tes;		// Energy in storage calculated in current timestep
	double m_q_startup_used;	//[MWt-hr]
	double m_q_startup_remain;	//[MWt-hr]
	
	// ......
	double dt, etesmax, ptsmax, pfsmax;
	
	//Declare variables that require storage from step to step
	double etes0;
	int pbmode0;
	bool is_sf_init;

	// pointers to arrays
	double *p_q_dot_field_inc;
	double *p_eta_field;
	double *p_q_dot_rec_inc;
	double *p_eta_thermal;

	double *p_pc_eta_thermal;

public:

	sam_mw_gen_type260( tcscontext *cxt, tcstypeinfo *ti ) 
		: tcstypeinterface(cxt, ti)
	{
		// Set up arrays: don't forget to delete [] !!
		p_q_dot_field_inc = new double[8760];
		mc_gen_cr.mc_reported_outputs.assign(C_csp_gen_collector_receiver::E_Q_DOT_FIELD_INC, p_q_dot_field_inc, 8760);
		p_eta_field = new double[8760];
		mc_gen_cr.mc_reported_outputs.assign(C_csp_gen_collector_receiver::E_ETA_FIELD, p_eta_field, 8760);
		p_q_dot_rec_inc = new double[8760];
		mc_gen_cr.mc_reported_outputs.assign(C_csp_gen_collector_receiver::E_Q_DOT_REC_INC, p_q_dot_rec_inc, 8760);
		p_eta_thermal = new double[8760];
		mc_gen_cr.mc_reported_outputs.assign(C_csp_gen_collector_receiver::E_ETA_THERMAL, p_eta_thermal, 8760);
		
		p_pc_eta_thermal = new double [8760];
		mc_gen_pc.mc_reported_outputs.assign(C_pc_gen::E_ETA_THERMAL, p_pc_eta_thermal, 8760);

		//Commonly used values, conversions, etc...
		Pi = acos(-1.);
		pi = Pi;
		r2d = 180./pi;
		d2r = pi/180.;
		g = 9.81;	//gravitation constant
		mtoinch = 39.3700787;	//[m] -> [in]
		
		mp_params = &mc_gen_cr.ms_params;
		mp_pc_params = &mc_gen_pc.ms_params;
		
		is_sf_init = false;
		
		//Set all values to NaN or nonsense value to prevent misuse
		eta_lhv	= std::numeric_limits<double>::quiet_NaN();
		
		PC_T_corr = -1;
		T_pcdes = std::numeric_limits<double>::quiet_NaN();


		f_Wpar_fixed	= std::numeric_limits<double>::quiet_NaN();
		f_Wpar_prod	= std::numeric_limits<double>::quiet_NaN();
		Wpar_prodQ_coefs	= NULL;
		nval_Wpar_prodQ_coefs = -1;
		Wpar_prodT_coefs	= NULL;
		nval_Wpar_prodT_coefs = -1;
		Wpar_prodD_coefs	= NULL;
		nval_Wpar_prodD_coefs = -1;
		hrs_tes	= std::numeric_limits<double>::quiet_NaN();
		f_charge	= std::numeric_limits<double>::quiet_NaN();
		f_disch	= std::numeric_limits<double>::quiet_NaN();
		f_etes_0	= std::numeric_limits<double>::quiet_NaN();
		f_teshl_ref	= std::numeric_limits<double>::quiet_NaN();
		teshlX_coefs	= NULL;
		nval_teshlX_coefs = -1;
		teshlT_coefs	= NULL;
		nval_teshlT_coefs = -1;
		ntod	= -1;
        storage_config = -1;
		//tod_sched	= NULL;
		nval_tod_sched = -1;
		disws	= NULL;
		nval_disws = -1;
		diswos	= NULL;
		nval_diswos = -1;
		qdisp	= NULL;
		nval_qdisp = -1;
		fdisp	= NULL;
		nval_fdisp = -1;
        sf_adjust = NULL;
        nval_sf_adjust = -1;
        exergy_table_in = NULL;
	    nrow_exergy_table = ncol_exergy_table = -1; 

		pbmode	= -1;

		

		// Calculated design variables
		m_q_des = std::numeric_limits<double>::quiet_NaN();	//[MWt] Thermal power to cycle
		m_qttmin = std::numeric_limits<double>::quiet_NaN();	//[MWt] Min allowable thermal power to cycle
		m_qttmax = std::numeric_limits<double>::quiet_NaN();	//[MWt] Max allowable thermal power to cycle
		m_q_startup = std::numeric_limits<double>::quiet_NaN();	//[MWt-hr] Startup energy

		// Storage variable
		m_e_in_tes = std::numeric_limits<double>::quiet_NaN();
	}

	virtual ~sam_mw_gen_type260()
	{
		delete [] p_q_dot_field_inc;
		delete [] p_eta_field;
		delete [] p_q_dot_rec_inc;
		delete [] p_eta_thermal;
	}

	virtual int init(){
		/*
		--Initialization call-- 

		Do any setup required here.
		Get the values of the inputs and parameters
		*/

		// *****************************************************************************
		// Collector receiver class
		mp_params->m_latitude = value(P_LATITUDE);		//Site latitude [deg]
		mp_params->m_longitude = value(P_LONGITUDE);		//Site longitude [deg]
		mp_params->m_theta_stow = value(P_THETA_STOW);	//Solar elevation angle at which the solar field stops operating [deg]
		mp_params->m_theta_dep = value(P_THETA_DEP);		//Solar elevation angle at which the solar field begins operating [deg]
		mp_params->m_interp_arr = (int)value(P_INTERP_ARR);	//Interpolate the array or find nearest neighbor? (1=interp,2=no) [none]
		mp_params->m_rad_type = (int)value(P_RAD_TYPE);		//Solar resource radiation type (1=DNI,2=horiz.beam,3=tot.horiz) [none]
		mp_params->m_solarm = value(P_SOLARM);			//Solar multiple [none]
		mp_params->m_T_sfdes = value(P_T_SFDES);			//Solar field design point temperature (dry bulb) [C]
		mp_params->m_irr_des = value(P_IRR_DES);			//Irradiation design point [W/m2]
		mp_params->m_eta_opt_soil = value(P_ETA_OPT_SOIL);	//Soiling optical derate factor [none]
		mp_params->m_eta_opt_gen = value(P_ETA_OPT_GEN);		//General/other optical derate [none]
		mp_params->m_f_sfhl_ref = value(P_F_SFHL_REF);		//Reference solar field thermal loss fraction [MW/MWcap]
		mp_params->m_qsf_des = value(P_QSF_DES);				//Solar field thermal production at design [MWt]
		mp_params->m_is_table_unsorted = value(P_ISTABLEUNSORTED) == 1.;

		// Vectors...
		int n_sfhlQ_coefs = 0;
		double *pt_sfhlQ_coefs = value(P_SFHLQ_COEFS, &n_sfhlQ_coefs);	//Irr-based solar field thermal loss adjustment coefficients [1/MWt]
		mp_params->mv_sfhlQ_coefs.resize(n_sfhlQ_coefs);
		for(int i = 0; i < n_sfhlQ_coefs; i++)
		{
			mp_params->mv_sfhlQ_coefs[i] = pt_sfhlQ_coefs[i];
		}
		int n_sfhlT_coefs = 0;
		double *pt_sfhlT_coefs = value(P_SFHLT_COEFS, &n_sfhlT_coefs);		//Temp.-based solar field thermal loss adjustment coefficients [1/C]
		mp_params->mv_sfhlT_coefs.resize(n_sfhlT_coefs);
		for(int i = 0; i < n_sfhlT_coefs; i++)
		{
			mp_params->mv_sfhlT_coefs[i] = pt_sfhlT_coefs[i];
		}
		int n_sfhlV_coefs = 0;
		double *pt_sfhlV_coefs = value(P_SFHLV_COEFS, &n_sfhlV_coefs);		//Wind-based solar field thermal loss adjustment coefficients [1/(m/s)]
		mp_params->mv_sfhlV_coefs.resize(n_sfhlV_coefs);
		for(int i = 0; i < n_sfhlV_coefs; i++)
		{
			mp_params->mv_sfhlV_coefs[i] = pt_sfhlV_coefs[i];
		}

		// Matrices
		int n_row_opt_table = 0;
		int n_col_opt_table = 0;
		double *pt_opt_table_in = value(P_OPTICALTABLE, &n_row_opt_table, &n_col_opt_table);	//Optical table [none]
		// Rearrange the optical table into a more useful format
		mp_params->m_optical_table.assign(pt_opt_table_in, n_row_opt_table, n_col_opt_table);
		
		// *****************************************************************************
		// *****************************************************************************

		// *****************************************************************************
		// Power Cycle class
		mp_pc_params->m_W_dot_des = value(P_W_DES);		//[MWe] Design power cycle gross output
		mp_pc_params->m_eta_des = value(P_ETA_DES);		//[-] Design power cycle gross efficiency
		mp_pc_params->m_f_wmax = value(P_F_WMAX);		//[-] Maximum over-design power cycle operation fraction
		mp_pc_params->m_f_wmin = value(P_F_WMIN);		//[-] Minimum part-load power cycle operation fraction
		mp_pc_params->m_f_startup = value(P_F_STARTUP);	//[hr] Equivalent full-load hours required for power system startup
		mp_pc_params->m_T_pc_des = value(P_T_PCDES);	//[C] Power conversion reference temperature
		mp_pc_params->m_PC_T_corr = (int) value(P_PC_T_CORR);	//[-] Power conversion temperature correction mode (1=wetb, 2=dryb)

		// Vectors
		int n_etaQ_coefs = 0;
		double *pt_etaQ_coefs = value(P_ETAQ_COEFS, &n_etaQ_coefs);		//Part-load power conversion efficiency adjustment coefficients [1/MWt]
		mp_pc_params->mv_etaQ_coefs.resize(n_etaQ_coefs);
		for(int i = 0; i < n_etaQ_coefs; i++)
		{
			mp_pc_params->mv_etaQ_coefs[i] = pt_etaQ_coefs[i];
		}
		int n_etaT_coefs = 0;
		double *pt_etaT_coefs = value(P_ETAT_COEFS, &n_etaT_coefs);		//Temp.-based power conversion efficiency adjustment coefs. [1/C]
		mp_pc_params->mv_etaT_coefs.resize(n_etaT_coefs);
		for(int i = 0; i < n_etaT_coefs; i++)
		{
			mp_pc_params->mv_etaT_coefs[i] = pt_etaT_coefs[i];
		}
		// *****************************************************************************
		// *****************************************************************************

		dt = time_step()/3600.;

		eta_lhv = value(P_ETA_LHV);		//Fossil backup lower heating value efficiency [none]

		PC_T_corr = (int)value(P_PC_T_CORR);	//[-] Power conversion temperature correction mode (1=wetb, 2=dryb)
		T_pcdes = value(P_T_PCDES)+273.15;		//[K] Power conversion reference temperature

		f_Wpar_fixed = value(P_F_WPAR_FIXED);		//Fixed capacity-based parasitic loss fraction [MWe/MWcap]
		f_Wpar_prod = value(P_F_WPAR_PROD);		//Production-based parasitic loss fraction [MWe/MWe]
		Wpar_prodQ_coefs = value(P_WPAR_PRODQ_COEFS, &nval_Wpar_prodQ_coefs);		//Part-load production parasitic adjustment coefs. [1/MWe]
		Wpar_prodT_coefs = value(P_WPAR_PRODT_COEFS, &nval_Wpar_prodT_coefs);		//Temp.-based production parasitic adjustment coefs. [1/C]
		Wpar_prodD_coefs = value(P_WPAR_PRODD_COEFS, &nval_Wpar_prodD_coefs);		//DNI-based production parasitic adjustment coefs. [m2/W]
		hrs_tes = value(P_HRS_TES);		//Equivalent full-load hours of storage [hours]
		f_charge = value(P_F_CHARGE);		//Storage charging energy derate [none]
		f_disch = value(P_F_DISCH);		//Storage discharging energy derate [none]
		f_etes_0 = value(P_F_ETES_0);		//Initial fractional charge level of thermal storage (0..1) [none]
		f_teshl_ref = value(P_F_TESHL_REF);		//Reference heat loss from storage per max stored capacity [kWt/MWhr-stored]
		teshlX_coefs = value(P_TESHLX_COEFS, &nval_teshlX_coefs);		//Charge-based thermal loss adjustment - constant coef. [1/MWhr-stored]
		teshlT_coefs = value(P_TESHLT_COEFS, &nval_teshlT_coefs);		//Temp.-based thermal loss adjustment - constant coef. [1/C]
		ntod = (int)value(P_NTOD);		//Number of time-of-dispatch periods in the dispatch schedule [none]
        storage_config = (int)value(P_STORAGE_CONFIG); //storage type, direct=0, indirect=1

		disws = value(P_DISWS, &nval_disws);		//Time-of-dispatch control for with-solar conditions [none]
		diswos = value(P_DISWOS, &nval_diswos);		//Time-of-dispatch control for without-solar conditions [none]
		qdisp = value(P_QDISP, &nval_qdisp);		//touperiod power output control factors [none]
		fdisp = value(P_FDISP, &nval_fdisp);		//Fossil backup output control factors [none]
        sf_adjust = value(P_ADJUST, &nval_sf_adjust); //solar field adjust factors
        exergy_table_in = value(P_EXERGY_TABLE, &nrow_exergy_table, &ncol_exergy_table);
        exergy_table_T.resize(2, nrow_exergy_table);    //transpose

        for(int i=0; i<2; i++)
            for(int j=0; j<nrow_exergy_table; j++)
                exergy_table_T.at(i,j) = TCS_MATRIX_INDEX( var(P_EXERGY_TABLE) , j, i);

		// Update site parameters
		mp_params->m_latitude = value(P_LATITUDE);
		mp_params->m_longitude = value(P_LONGITUDE);

		int out_type = -1;
		std::string out_msg = "";

		try
		{
			C_csp_collector_receiver::S_csp_cr_init_inputs init_inputs;
			mc_gen_cr.init(init_inputs, mc_cr_des_solved);
			mc_gen_pc.init(mc_pc_des_solved);
		}
		catch( C_csp_exception &csp_exception )
		{
			// Report warning before exiting with error
			while( mc_gen_cr.mc_csp_messages.get_message(&out_type, &out_msg) )
			{
				if( out_type == C_csp_messages::NOTICE )
					message(TCS_NOTICE, out_msg.c_str());
				else if( out_type == C_csp_messages::WARNING )
					message(TCS_WARNING, out_msg.c_str());
			}

			message(TCS_ERROR, csp_exception.m_error_message.c_str());
			return -1;
		}

		// If no exception, then report messages and move on
		while( mc_gen_cr.mc_csp_messages.get_message(&out_type, &out_msg) )
		{
			if( out_type == C_csp_messages::NOTICE )
				message(TCS_NOTICE, out_msg.c_str());
			else if( out_type == C_csp_messages::WARNING )
				message(TCS_WARNING, out_msg.c_str());
		}

		//	Initial calculations
			//	Power block design power
		m_q_des = mc_pc_des_solved.m_q_dot_des;	//[MWt]
			//	Calculate min/max turbine operation rates
		m_qttmin = m_q_des*mc_pc_des_solved.m_cutoff_frac;	//[MWt]
		m_qttmax = m_q_des*mc_pc_des_solved.m_max_frac;		//[MWt]
		m_q_startup = mc_pc_des_solved.m_q_startup;			//[MWt]

		f_teshl_ref *= .001;

		//Maximum energy in storage
		etesmax = hrs_tes*m_q_des;   //[MW-hr]
		
		//Express the dispatch values in dimensional terms
		for(int i=0; i<ntod; i++){
			disws[i] *= etesmax;
			diswos[i] *= etesmax;
			qdisp[i] *= m_q_des;
			fdisp[i] *= m_q_des;
		}
		
		//Calculate max TES charging/discharging rates based on solar multiple
			//5.5.2016, twn: adopt Physical Trough Convention: size TES HX to accept design point receiver output 
		//ptsmax = m_q_des*max((mp_params->m_solarm - 1.), 1.0);
		ptsmax = m_q_des * mp_params->m_solarm;		
		pfsmax = ptsmax / f_disch*mc_pc_des_solved.m_max_frac;

		//Set initial storage values
		etes0 = f_etes_0*etesmax;		//[MW-hr] Initial value in thermal storage. This keeps track of energy in thermal storage, or e_in_tes
		pbmode0 = 0;					//[-] initial value of power block operation mode pbmode
		m_q_startup_remain = mc_pc_des_solved.m_q_startup;	//[MWt-hr]

		return true;
	}

	virtual int call(double time, double step, int ncall){
		/* 
		-- Standard timestep call --
		
		*get inputs
		*do calculations
		*set outputs

		*/
		//******************************************************************************************************************************
		//               Time-dependent conditions
		//******************************************************************************************************************************
		double ibn = value(I_IBN);		//Beam-normal (DNI) irradiation [W/m^2]
		double ibh = value(I_IBH);		//Beam-horizontal irradiation [W/m^2]
		double itoth = value(I_ITOTH);	//Total horizontal irradiation [W/m^2]
		double tdb = value(I_TDB);		//Ambient dry-bulb temperature [C]
		double twb = value(I_TWB);		//Ambient wet-bulb temperature [C]
		double vwind = value(I_VWIND);		//Wind velocity [m/s]

		//Re-read the location info from the weather reader
		double longitude = value(P_LONGITUDE)*d2r;
		double timezone = value(P_TIMEZONE);

		double shift = longitude - timezone*15.*d2r;
	  //int touperiod = CSP::TOU_Reader(tod_sched, time, nval_tod_sched); 
		int touperiod = (int)value(I_TOUPeriod) - 1; // control value between 1 & 9, have to change to 0-8 for array index

		// Weather file inputs
		C_csp_weatherreader::S_outputs weather;
		weather.m_beam = ibn;		//[W/m2] DNI
		weather.m_hor_beam = ibh;	//[W/m2] Beam-horizontal irradiance
		weather.m_global = itoth;	//[W/m2] Total horizontal irradiance
		weather.m_tdry = tdb;		//[C] Dry bulb
		weather.m_twet = twb;		//[C] Wet bulb
		weather.m_wspd = vwind;		//[m/s] Wind speed
		weather.m_shift = shift / d2r;	//[deg]

		//Unit conversions
		tdb += 273.15;
		twb += 273.15;

		// Set up generic collector-receiver csp-solver class		
		C_csp_solver_htf_1state cr_htf_state_in;
		C_csp_collector_receiver::S_csp_cr_inputs cr_inputs;
		C_csp_collector_receiver::S_csp_cr_out_solver cr_out_solver;
		//C_csp_collector_receiver::S_csp_cr_out_report cr_out_report;
		C_csp_solver_sim_info sim_info;

			// HTF inlet state: not required by generic class

			// Collector-receiver class inputs
		cr_inputs.m_field_control = 1.0;		//[-] Generic model doesn't require defocus signal
		cr_inputs.m_input_operation_mode = C_csp_collector_receiver::ON;	//[-] Model always solves as 'on'
        cr_inputs.m_adjust = sf_adjust[ (int)(time / step) ];

			// Set sim info
		sim_info.ms_ts.m_time = time;
		sim_info.ms_ts.m_step = step;
		sim_info.m_tou = touperiod;

		int out_type = -1;
		std::string out_msg = "";

		// Call cr class
		try
		{
			mc_gen_cr.call(weather, cr_htf_state_in, cr_inputs, cr_out_solver, sim_info);
		}
		catch( C_csp_exception &csp_exception )
		{
			// Report warning before exiting with error
			while( mc_gen_cr.mc_csp_messages.get_message(&out_type, &out_msg) )
			{
				if( out_type == C_csp_messages::NOTICE )
					message(TCS_NOTICE, out_msg.c_str());
				else if( out_type == C_csp_messages::WARNING )
					message(TCS_WARNING, out_msg.c_str());
			}

			message(TCS_ERROR, csp_exception.m_error_message.c_str());
			return -1;
		}

		double irr_used = 0.0;
		//Choose which irradiation source will be used
		switch( mp_params->m_rad_type )
		{
		case 1:
			irr_used = ibn;		//[W/m2]
			break;
		case 2:
			irr_used = ibh;		//[W/m2]
			break;
		case 3:
			irr_used = itoth;	//[W/m2]
			break;
		}

		//Choose which dispatch set will be used
		util::matrix_t<double> dispatch(ntod);
		if(irr_used>0.){
			for(int i=0; i<ntod; i++) dispatch.at(i) = disws[i];
		}
		else{
			for(int i=0; i<ntod; i++) dispatch.at(i) = diswos[i];
		}
		
		//*********** End of input section *********************************************************************************************

		double eta_opt_sf = mc_gen_cr.mc_reported_outputs.value(C_csp_gen_collector_receiver::E_ETA_FIELD);		// cr_out_report.m_eta_field;
		double q_sf = cr_out_solver.m_q_thermal;		//[MWt]

		//------------------------------------------------------------------------------------------------------------
		//       Dispatch calculations
		//------------------------------------------------------------------------------------------------------------
		// initialize outputs to 0
		double q_to_tes = 0.0;     //| Energy to Thermal Storage 
		double q_from_tes = 0.0;   //| Energy from Thermal Storage
		m_e_in_tes = 0.;     //| Energy in Thermal Storage
		double q_hl_tes = 0.0;     //| Energy losses from Thermal Storage
		double q_to_pb = 0.0;     //| Energy to the Power Block
		double q_dump_tesfull = 0.;     //| Energy dumped because the thermal storage is full
		double q_dump_umin = 0.;     //| Indicator of being below minimum operation level
		double q_dump_teschg = 0.;     //| The amount of energy dumped (more than turbine and storage)
		double q_startup = 0.;     //| The energy needed to startup the turbine
		int pbstartf = 0;     //| is 1 during the period when powerblock starts up otherwise 0
		m_q_startup_used = m_q_startup_remain;   //| Turbine startup energy for this timestep is equal to the remaining previous energy

		//--------Plant dispatch strategy--------------------
		if (hrs_tes <= 0.){ // No Storage
			if ((pbmode0==0)||(pbmode0==1)){ // if plant is not already operating in last timestep
				if (q_sf>0){
					if( q_sf>(m_q_startup_used / dt) ){ //  Starts plant as exceeds startup energy needed
						q_to_pb = q_sf - m_q_startup_used / dt;
						q_startup = m_q_startup_used / dt;
						pbmode = 2;      //Power block mode.. 2=starting up
						pbstartf = 1;    //Flag indicating whether the power block starts up in this time period
						m_q_startup_used = 0.;     //mjw 5-31-13 Reset to zero to handle cases where Qsf-TurSue leads to Qttb < Qttmin
					}
					else{ //  Plant starting up but not enough energy to make it run - will probably finish in the next timestep
						q_to_pb = 0.;
						m_q_startup_used = m_q_startup_remain - q_sf*dt;
						q_startup = q_sf;
						pbmode = 1;
						pbstartf = 0;
					}
				}
				else{ // No solar field output so still need same amount of energy as before and nothing changes
					m_q_startup_used = m_q_startup;	//[MWt-hr]
					pbmode = 0;
					pbstartf = 0;
				}
			}
			else{ // if the powerblock mode is already 2 (running previous timestep)
				if (q_sf>0){     // Plant operated last hour_of_day and this one
					q_to_pb = q_sf;          // all power goes from solar field to the powerblock
					pbmode = 2;          // powerblock continuing to operate
					pbstartf = 0;        // powerblock did not start during this timestep
				}
				else{                   //  Plant operated last hour_of_day but not this one
					q_to_pb = 0.;            // No energy to the powerblock
					pbmode = 0;          // turned off powrblock
					pbstartf = 0;        // it didn't start this timeperiod 
					m_q_startup_used = m_q_startup_remain;
				}
			}

			// following happens no matter what state the powerblock was in previously      
			if( q_to_pb < m_qttmin ){ // Energy to powerblock less than the minimum that the turbine can run at
				q_dump_umin =  q_to_pb;         // The minimum energy (less than the minimum)
				q_to_pb = 0;             // Energy to PB is now 0
				pbmode = 0;           // PB turned off
			}

			if( q_to_pb > m_qttmax ){   // Energy to powerblock greater than what the PB can handle (max)
				q_dump_teschg = q_to_pb - m_qttmax; // The energy dumped 
				q_to_pb = m_qttmax;          // the energy to the PB is exactly the maximum
			}
		}
		else{    //With thermal storage    

			//--initialize values
			q_startup = 0.0;
			pbstartf = 0;
			q_dump_teschg = 0.0;
			q_from_tes = 0.0;

			if (pbmode0 == 0){       
			//**********************************************************
			//******        plant is not already operating         *****
			//**********************************************************
				//---Start plant if any of the following conditions are met---
				// 1.) Solar field output > 0
				//       a.) AND energy in TES exceeds w/ solar touperiod fraction
				//       b.) AND energy in TES plus solar field output exceeds turbine fraction
				// OR
				// 2.) Solar field is off
				//       a.) AND energy in TES exceeds w/o solar touperiod
				//       b.) AND energy in TES exceeds turbine fraction
				// OR
				// 3.) Solar field energy exceeds maximum TES charging rate
				//------------------------------------------------------------
				double EtesA = max(0.0, etes0 - dispatch.at(touperiod));
                if (EtesA >= m_q_startup / dt && q_sf + max(EtesA - m_q_startup / dt, 0.0) >= qdisp[touperiod]) {

                    // Assumes Operator started plant during previous time period
                    // But TRNSYS cannot do this, so start-up energy is deducted during current timestep.     
                    pbmode = 1;
                    q_startup = m_q_startup / dt;

                    q_to_pb = qdisp[touperiod];       // set the energy to powerblock equal to the load for this TOU period

                    double q_to_cycle_total = q_startup + q_to_pb;

                    if (q_sf > q_to_pb) {             // if solar field output is greater than what the necessary load ?
                        q_to_tes = q_sf - q_to_pb;           // the extra goes to thermal storage
                        q_from_tes = q_startup;               // Use the energy from thermal storage to startup the power cycle
                        if (q_to_tes > ptsmax) {       // if q to thermal storage exceeds thermal storage max rate Added 9-10-02
                            q_dump_teschg = q_to_tes - ptsmax;   // then dump the excess for this period Added 9-10-02
                            q_to_tes = ptsmax;
                        }
                    }
                    else { // q_sf less than the powerblock requirement
                        q_to_tes = 0.0;
                        q_from_tes = q_startup + (1 - q_sf / q_to_pb) * min(pfsmax, m_q_des);
                        if (q_from_tes > pfsmax) q_from_tes = pfsmax;
                        q_to_pb = q_sf + (1 - q_sf / q_to_pb) * min(pfsmax, m_q_des);
                    }

                    m_e_in_tes = etes0 - q_startup + (q_sf - q_to_pb) * dt;   // thermal storage energy is initial + what was left 
                    pbmode = 2;   // powerblock is now running
                    pbstartf = 1; // the powerblock turns on during this timeperiod.

                }
				else{ //Store energy not enough stored to start plant
					q_to_tes = q_sf; // everything goes to thermal storage
					q_from_tes = 0;   // nothing from thermal storage
					m_e_in_tes = etes0 + q_to_tes * dt;
					q_to_pb = 0;
				}
			}
			else{       
			//**********************************************************
			//******        plant is already operating             *****
			//**********************************************************

				if ((q_sf + max(0.0,etes0-dispatch.at(touperiod)) /dt) > qdisp[touperiod]){ // if there is sufficient energy to operate at dispatch target output

					q_to_pb = qdisp[touperiod]; 

					if (q_sf>q_to_pb){ 
						q_to_tes = q_sf - q_to_pb; //extra from what is needed put in thermal storage
						q_from_tes = 0.;
						if (q_to_tes>ptsmax){  //check if max power rate to storage exceeded
							q_dump_teschg = q_to_tes - ptsmax; // if so, dump extra 
							q_to_tes = ptsmax;
						}
					}
					else{ // solar field outptu less than what powerblock needs
						q_to_tes = 0.;
						q_from_tes = (1. - q_sf / q_to_pb) * min(pfsmax, m_q_des);
						if( q_from_tes>pfsmax ) q_from_tes = min(pfsmax, m_q_des);
						q_to_pb = q_from_tes + q_sf;
					}

					m_e_in_tes = etes0 + (q_sf - q_to_pb - q_dump_teschg) *dt;  // energy of thermal storage is the extra

					// Check to see if throwing away energy 
					if( (m_e_in_tes>etesmax) && (q_to_pb<m_qttmax) ){ // qttmax (MWt) - power to turbine max
						if( (m_e_in_tes - etesmax) / dt < (m_qttmax - q_to_pb) ){
							q_to_pb = q_to_pb + (m_e_in_tes - etesmax) / dt;
							m_e_in_tes = etesmax;
						}
						else{
							m_e_in_tes = m_e_in_tes - (m_qttmax - q_to_pb) * dt;  // should this be etes0 instead of e_in_tes on RHS ??
							q_to_pb = m_qttmax;
						}
						q_to_tes = q_sf - q_to_pb;
					}
				}
				else{  //Empties tes to dispatch level if above min load level

					if( (q_sf + max(0., etes0 - dispatch.at(touperiod)) * dt) > m_qttmin ){
						q_from_tes = max(0.,etes0-dispatch.at(touperiod)) * dt;
						q_to_pb = q_sf + q_from_tes;
						q_to_tes = 0.;
						m_e_in_tes = etes0 - q_from_tes;
					}
					else{
						q_to_pb = 0.;
						q_from_tes = 0.;
						q_to_tes = q_sf;
						m_e_in_tes = etes0 + q_to_tes * dt;
					}
				}
			}

			if (q_to_pb>0)
				pbmode = 2;
			else
				pbmode = 0;
			

			//---Calculate TES thermal losses ---
			// First do the charge-based losses. Use the average charge over the timestep
			double f_EtesAve = max((m_e_in_tes + etes0) / 2. / etesmax, 0.0);
			double f_teshlX = 0., f_teshlT = 0.;
			for(int i=0; i<nval_teshlX_coefs; i++)
				f_teshlX += teshlX_coefs[i]*pow(f_EtesAve, i);     //Charge adjustment factor
			for(int i=0; i<nval_teshlT_coefs; i++)
				f_teshlT += teshlT_coefs[i]*pow(mp_params->m_T_sfdes - tdb, i);
			//thermal storage heat losses adjusted by charge level and ambient temp.
			q_hl_tes = f_teshl_ref*etesmax*(f_teshlX + f_teshlT);

			m_e_in_tes = max(m_e_in_tes - q_hl_tes*dt, 0.0); // Adjust the energy in thermal storage according to TES thermal losses

			if( m_e_in_tes>etesmax ){ // trying to put in more than storage can handle
				q_dump_tesfull = (m_e_in_tes - etesmax) / dt;  //this is the amount dumped when storage is completely full
				m_e_in_tes = etesmax;
				q_to_tes = q_to_tes - q_dump_tesfull;
			}
			else{
				q_dump_tesfull = 0.; // nothing is dumped if not overfilled
			}

			// Check min and max on turbine
			if( q_to_pb<m_qttmin ){
				q_dump_umin = q_to_pb;
				q_to_pb = 0.;
				pbmode = 0;
			}
			else{
				q_dump_umin = 0;
			}

			pbmode0 = pbmode;
		}

		//------------------------------------------------------------------------------------------------------------
		//       Fossil backup
		//------------------------------------------------------------------------------------------------------------
		double q_fossil = std::numeric_limits<double>::quiet_NaN();
		double q_gas = q_fossil;
		
		
		if( q_to_pb < fdisp[touperiod])
		{	// If the thermal power dispatched to the power cycle is less than the level in the fossil control
			q_fossil = fdisp[touperiod] - q_to_pb;	// then the fossil used is the fossil control value minus what's provided by the solar field
			q_gas = q_fossil / eta_lhv;       // Calculate the required fossil heat content based on the LHV efficiency
		}
		else
		{
			q_fossil = 0.0;
			q_gas = 0.0;
		}

		
		//if (q_sf < fdisp[touperiod])
		//{      // if the solar provided is less than the level stipulated in the fossil control
		//	q_fossil = fdisp[touperiod] - q_sf;     // then the fossil used is the fossil control value minus what's provided by the solar field
		//	q_gas = q_fossil / eta_lhv;       // Calculate the required fossil heat content based on the LHV efficiency
		//}
		//else
		//{
		//	q_fossil = 0.;
		//	q_gas = 0.;
		//}
		


		//Adjust the power block energy based on additional fossil backup
		q_to_pb = q_to_pb + q_fossil;
    
		//------------------------------------------------------------------------------------------------------------
		//       Power block calculations
		//------------------------------------------------------------------------------------------------------------
		double T_htf_cold_fixed, T_htf_hot_fixed, cp_htf_fixed;
		T_htf_cold_fixed = T_htf_hot_fixed = cp_htf_fixed = std::numeric_limits<double>::quiet_NaN();
		mc_gen_pc.get_fixed_properties(T_htf_cold_fixed, T_htf_hot_fixed, cp_htf_fixed);

		double m_dot_htf = q_to_pb*1.E3/(cp_htf_fixed*(T_htf_hot_fixed-T_htf_cold_fixed))*3600.0;
		C_csp_solver_htf_1state pc_htf_state_in;
		pc_htf_state_in.m_temp = T_htf_hot_fixed-273.15;	//[C]
		C_csp_power_cycle::S_control_inputs pc_control_inputs;
		pc_control_inputs.m_m_dot = m_dot_htf;				//[kg/hr]

		C_csp_power_cycle::S_csp_pc_out_solver pc_out_solver;

		mc_gen_pc.call(weather, pc_htf_state_in, pc_control_inputs, pc_out_solver, sim_info);
        
        //calculate exergy penalty
        double exergy_adj = 1.;
        if( exergy_table_T.ncols() > 1 )
        {
            exergy_adj = CSP::interp(&exergy_table_T.at(0,0), &exergy_table_T.at(1,0), m_e_in_tes/etesmax, 0, nrow_exergy_table-1, (exergy_table_T.at(0,1) > exergy_table_T.at(0,0)) );
        }
        else
        {
            exergy_adj = exergy_table_T.at(1,0);
        }
        if( storage_config == 1 ) //indirect
        {
            //for indirect storage, only the energy coming from storage impacts the exergy, since presumably the field and aux systems
            //produce energy at full temperature. We need to weight the exergy adjustment based on the fraction coming from storage.
            double weight = 0.;  
            if( q_to_pb > 0. )
                weight = q_from_tes / q_to_pb;
            exergy_adj = (1. - weight) + weight*exergy_adj;
        }


		//Calculate the gross power
        double eta_cycle_raw = mc_gen_pc.mc_reported_outputs.value(C_pc_gen::E_ETA_THERMAL);
		double eta_cycle = (eta_cycle_raw/mp_pc_params->m_eta_des + exergy_adj - 1.)*mp_pc_params->m_eta_des;	//[-]
		double w_gr;
        if( eta_cycle > 0.)
            w_gr = pc_out_solver.m_P_cycle/eta_cycle_raw*eta_cycle;				//[MWe]
        else
        {
            w_gr = 0.;
            eta_cycle = 0.;
        }

		//Keep track of what portion is from solar
		double w_gr_solar = (q_to_pb - q_fossil)*eta_cycle;


		//------------------------------------------------------------------------------------------------------------
		//       Parasitics
		//------------------------------------------------------------------------------------------------------------
		double w_par_fixed = f_Wpar_fixed * mc_pc_des_solved.m_W_dot_des;       //Fixed parasitic loss based on plant capacity
		//Production-based parasitic loss
		double qnorm = pc_out_solver.m_q_dot_htf / m_q_des;		//[-] Normalized thermal power to cycle
		double wpar_prodq = 0., wpar_prodt = 0., wpar_prodd = 0.;
		for(int i=0; i<nval_Wpar_prodQ_coefs; i++)
			wpar_prodq += Wpar_prodQ_coefs[i]*pow(qnorm, i);	//Power block part-load correction factor
		
		double tnorm = std::numeric_limits<double>::quiet_NaN();
		if( PC_T_corr == 1 )      //Select the dry or wet bulb temperature as the driving difference
			tnorm = twb - T_pcdes;
		else
			tnorm = tdb - T_pcdes;

		for(int i=0; i<nval_Wpar_prodT_coefs; i++)
			wpar_prodt += Wpar_prodT_coefs[i]*pow(tnorm, i);	//Temperature correction factor
		
        double dnorm = irr_used/mp_params->m_irr_des;
        for(int i=0; i<nval_Wpar_prodD_coefs; i++)
			wpar_prodd += Wpar_prodD_coefs[i]*pow(dnorm, i);	//DNI correction factor
		
        double wpar_adj = (wpar_prodq + wpar_prodt + wpar_prodd);
        if(wpar_adj < 0.)
            wpar_adj = 0.;

        double w_par_prod = f_Wpar_prod * w_gr * wpar_adj;    //MW 10/7/2016 : Making model additive to better fit regressions

		double w_par_tot = w_par_fixed + w_par_prod;   //Total parasitic loss

		// Keep track of online/offline parasitics
		double w_par_online = std::numeric_limits<double>::quiet_NaN();
		double w_par_offline = std::numeric_limits<double>::quiet_NaN();
		if(w_gr > 0.)
		{
			w_par_online = w_par_tot;
			w_par_offline = 0.;
		}
		else
		{
			w_par_online = 0.;
			w_par_offline = w_par_tot;
		}

		//---Calculate net energy output (enet<-->Wnet) ---
		double enet = w_gr - w_par_tot;

		//Calculate final values
		//declination = dec*r2d;           //[deg] Declination angle
		//hrangle = omega*r2d;         //[deg] hour_of_day angle
		//solalt = max(solalt*r2d, 0.0); //[deg] Solar elevation angle
		//solaz = solaz*r2d;       //[deg] Solar azimuth angle (-180..180, 0deg=South)
		
		double q_inc = mc_gen_cr.mc_reported_outputs.value(C_csp_gen_collector_receiver::E_Q_DOT_FIELD_INC);	// cr_out_report.m_q_dot_field_inc;            //[MWt] Qdni - Solar incident energy, before all losses
		double q_rec_inc = mc_gen_cr.mc_reported_outputs.value(C_csp_gen_collector_receiver::E_Q_DOT_REC_INC);	//[MWt] Receiver incident energy before thermal losses
		double q_dump_tot = q_dump_tesfull + q_dump_teschg + q_dump_umin; //[MWt] Total dumped energy
		double w_gr_fossil = w_gr - w_gr_solar;          //[MWe] Power produced from the fossil component
		double f_sfhl_qdni = mc_gen_cr.mc_reported_outputs.value(C_csp_gen_collector_receiver::E_F_SFHL_QDNI);
		double f_sfhl_tamb = mc_gen_cr.mc_reported_outputs.value(C_csp_gen_collector_receiver::E_F_SFHL_QTDRY);
		double f_sfhl_vwind = mc_gen_cr.mc_reported_outputs.value(C_csp_gen_collector_receiver::E_F_SFHL_QWSPD);

		//Set outputs and return
		value(O_IRR_USED, irr_used);		//[W/m2] Irradiation value used in simulation
		//value(O_HOUR_OF_DAY, hour_of_day);		//[hour_of_day] hour_of_day of the day
		//value(O_DAY_OF_YEAR, day_of_year);		//[day] Day of the year
		//value(O_DECLINATION, declination);		//[deg] Declination angle
		//value(O_SOLTIME, soltime);		//[hour_of_day] [hour_of_day] Solar time of the day
		//value(O_HRANGLE, hrangle);		//[deg] hour_of_day angle
		//value(O_SOLALT, solalt);		//[deg] Solar elevation angle
		//value(O_SOLAZ, solaz);		//[deg] Solar azimuth angle (-180..180, 0deg=South)
		value(O_ETA_OPT_SF, eta_opt_sf);		//[none] Solar field optical efficiency
		value(O_F_SFHL_QDNI, f_sfhl_qdni);		//[none] Solar field load-based thermal loss correction
		value(O_F_SFHL_TAMB, f_sfhl_tamb);		//[none] Solar field temp.-based thermal loss correction
		value(O_F_SFHL_VWIND, f_sfhl_vwind);		//[none] Solar field wind-based thermal loss correction
		value(O_Q_HL_SF, q_sf > 0. ? q_rec_inc - cr_out_solver.m_q_thermal : 0.);		//[MWt] Solar field thermal losses
		value(O_Q_SF, q_sf);		//[MWt] Solar field delivered thermal power
		value(O_Q_INC, q_inc);		//[MWt] Qdni - Solar incident energy, before all losses
		value(O_PBMODE, pbmode);		//[none] Power conversion mode
		value(O_PBSTARTF, pbstartf);		//[none] Flag indicating power system startup
		value(O_Q_TO_PB, q_to_pb);		//[MWt] Thermal energy to the power conversion system
		value(O_Q_STARTUP, q_startup);		//[MWt] Power conversion startup energy
		value(O_Q_TO_TES, q_to_tes);		//[MWt] Thermal energy into storage
		value(O_Q_FROM_TES, q_from_tes);		//[MWt] Thermal energy from storage
		value(O_E_IN_TES, m_e_in_tes);		//[MWt-hr] Energy in storage
		value(O_Q_HL_TES, q_hl_tes);		//[MWt] Thermal losses from storage
		value(O_Q_DUMP_TESFULL, q_dump_tesfull);		//[MWt] Dumped energy  exceeding storage charge level max
		value(O_Q_DUMP_TESCHG, q_dump_teschg);		//[MWt] Dumped energy exceeding exceeding storage charge rate
		value(O_Q_DUMP_UMIN, q_dump_umin);		//[MWt] Dumped energy from falling below min. operation fraction
		value(O_Q_DUMP_TOT, q_dump_tot);		//[MWt] Total dumped energy
		value(O_Q_FOSSIL, q_fossil);		//[MWt] thermal energy supplied from aux firing
		value(O_Q_GAS, q_gas);		//[MWt] Energy content of fuel required to supply Qfos
		//value(O_F_EFFPC_QTPB, f_effpc_qtpb);		//[none] Load-based conversion efficiency correction
		//value(O_F_EFFPC_TAMB, f_effpc_tamb);		//[none] Temp-based conversion efficiency correction
		value(O_ETA_CYCLE, eta_cycle);		//[none] Adjusted power conversion efficiency
		value(O_W_GR_SOLAR, w_gr_solar);		//[MWe] Power produced from the solar component
		value(O_W_GR_FOSSIL, w_gr_fossil);		//[MWe] Power produced from the fossil component
		value(O_W_GR, w_gr);		//[MWe] Total gross power production
		value(O_W_PAR_FIXED, w_par_fixed);		//[MWe] Fixed parasitic losses
		value(O_W_PAR_PROD, w_par_prod);		//[MWe] Production-based parasitic losses
		value(O_W_PAR_TOT, w_par_tot);		//[MWe] Total parasitic losses
		value(O_W_PAR_ONLINE, w_par_online);		//[MWe] Online parasitics
		value(O_W_PAR_OFFLINE, w_par_offline);		//[MWe] Offline parasitics
		value(O_ENET, enet);		//[MWe] Net electric output
		return 0;
	}

	virtual int converged(double time){
		/* 
		-- Post-convergence call --

		Update values that should be transferred to the next time step
		*/
		etes0 = m_e_in_tes;
		pbmode0 = pbmode;
		m_q_startup_remain = m_q_startup_used;
		return 0;
	}


};

TCS_IMPLEMENT_TYPE( sam_mw_gen_type260, "Generic Solar Model", "Mike Wagner", 1, sam_mw_gen_type260_variables, NULL, 1 );






















