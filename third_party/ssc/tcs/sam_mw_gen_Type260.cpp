#define _TCSTYPEINTERFACE_
#include "tcstype.h"
//#include "htf_props.h"
#include "sam_csp_util.h"

#include <cmath>

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
	{ TCS_PARAM,           TCS_ARRAY,  P_WPAR_PRODT_COEFS,       "Wpar_prodT_coefs",                                      "Temp.-based production parasitic adjustment coefs.",          "1/C",             "",             "",      "1,0,0,0" },
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
	
	OpticalDataTable optical_table;
    GaussMarkov *optical_table_uns;

    //Constants for scaling GM table
    double eff_scale;   //defined later based on max value

	double pi,Pi,d2r,r2d, g, mtoinch;

	double latitude;		//Site latitude
	double longitude;		//Site longitude
	double timezone;		//Site timezone
	double theta_stow;		//Solar elevation angle at which the solar field stops operating
	double theta_dep;		//Solar elevation angle at which the solar field begins operating
	int interp_arr;		//Interpolate the array or find nearest neighbor? (1=interp,2=no)
	int rad_type;		//Solar resource radiation type (1=DNI,2=horiz.beam,3=tot.horiz)
	double solarm;		//Solar multiple
	double T_sfdes;		//Solar field design point temperature (dry bulb)
	double irr_des;		//Irradiation design point
	double eta_opt_soil;		//Soiling optical derate factor
	double eta_opt_gen;		//General/other optical derate
	double f_sfhl_ref;		//Reference solar field thermal loss fraction
	double* sfhlQ_coefs;		//Irr-based solar field thermal loss adjustment coefficients
	int nval_sfhlQ_coefs;
	double* sfhlT_coefs;		//Temp.-based solar field thermal loss adjustment coefficients
	int nval_sfhlT_coefs;
	double* sfhlV_coefs;		//Wind-based solar field thermal loss adjustment coefficients
	int nval_sfhlV_coefs;
	double qsf_des;		//Solar field thermal production at design
	double w_des;		//Design power cycle gross output
	double eta_des;		//Design power cycle gross efficiency
	double f_wmax;		//Maximum over-design power cycle operation fraction
	double f_wmin;		//Minimum part-load power cycle operation fraction
	double f_startup;		//Equivalent full-load hours required for power system startup
	double eta_lhv;		//Fossil backup lower heating value efficiency
	double* etaQ_coefs;		//Part-load power conversion efficiency adjustment coefficients
	int nval_etaQ_coefs;
	double* etaT_coefs;		//Temp.-based power conversion efficiency adjustment coefs.
	int nval_etaT_coefs;
	double T_pcdes;		//Power conversion reference temperature
	double PC_T_corr;		//Power conversion temperature correction mode (1=wetb, 2=dryb)
	double f_Wpar_fixed;		//Fixed capacity-based parasitic loss fraction
	double f_Wpar_prod;		//Production-based parasitic loss fraction
	double* Wpar_prodQ_coefs;		//Part-load production parasitic adjustment coefs.
	int nval_Wpar_prodQ_coefs;
	double* Wpar_prodT_coefs;		//Temp.-based production parasitic adjustment coefs.
	int nval_Wpar_prodT_coefs;
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
	bool istableunsorted;
    double* OpticalTable_in;		//Optical table
	int nrow_OpticalTable,	ncol_OpticalTable;
    /*double* OpticalTableUns_in;
    int nrow_OpticalTableUns, ncol_OpticalTableUns;*/

	double ibn;		//Beam-normal (DNI) irradiation
	double ibh;		//Beam-horizontal irradiation
	double itoth;		//Total horizontal irradiation
	double tdb;		//Ambient dry-bulb temperature
	double twb;		//Ambient wet-bulb temperature
	double vwind;		//Wind velocity

	double irr_used;		//Irradiation value used in simulation
	double hour_of_day;		//Hour of the day
	double day_of_year;		//Day of the year
	double declination;		//Declination angle
	double soltime;		//[hour] Solar time of the day
	double hrangle;		//Hour angle
	double solalt;		//Solar elevation angle
	double solaz;		//Solar azimuth angle (-180..180, 0deg=South)
	double eta_opt_sf;		//Solar field optical efficiency
	double f_sfhl_qdni;		//Solar field load-based thermal loss correction
	double f_sfhl_tamb;		//Solar field temp.-based thermal loss correction
	double f_sfhl_vwind;		//Solar field wind-based thermal loss correction
	double q_hl_sf;		//Solar field thermal losses
	double q_sf;		//Solar field delivered thermal power
	double q_inc;		//Qdni - Solar incident energy, before all losses
	int pbmode;		//Power conversion mode
	int pbstartf;		//Flag indicating power system startup
	double q_to_pb;		//Thermal energy to the power conversion system
	double q_startup;		//Power conversion startup energy
	double q_to_tes;		//Thermal energy into storage
	double q_from_tes;		//Thermal energy from storage
	double e_in_tes;		//Energy in storage
	double q_hl_tes;		//Thermal losses from storage
	double q_dump_tesfull;		//Dumped energy  exceeding storage charge level max
	double q_dump_teschg;		//Dumped energy exceeding exceeding storage charge rate
	double q_dump_umin;		//Dumped energy from falling below min. operation fraction
	double q_dump_tot;		//Total dumped energy
	double q_fossil;		//thermal energy supplied from aux firing
	double q_gas;		//Energy content of fuel required to supply Qfos
	double f_effpc_qtpb;		//Load-based conversion efficiency correction
	double f_effpc_tamb;		//Temp-based conversion efficiency correction
	double eta_cycle;		//Adjusted power conversion efficiency
	double w_gr_solar;		//Power produced from the solar component
	double w_gr_fossil;		//Power produced from the fossil component
	double w_gr;		//Total gross power production
	double w_par_fixed;		//Fixed parasitic losses
	double w_par_prod;		//Production-based parasitic losses
	double w_par_tot;		//Total parasitic losses
	double w_par_online;		//Online parasitics
	double w_par_offline;		//Offline parasitics
	double enet;		//Net electric output

	util::matrix_t<double> OpticalTable;
    //util::matrix_t<double> OpticalTableUns;

	//Declare variables that require storage from step to step
	double dt, start_time, q_des, etesmax, omega, dec, eta_opt_ref, f_qsf, qttmin, qttmax, ptsmax, pfsmax;
	
	double etes0, q_startup_remain, q_startup_used;
	int pbmode0;
	bool is_sf_init;

public:

	sam_mw_gen_type260( tcscontext *cxt, tcstypeinfo *ti ) 
		: tcstypeinterface(cxt, ti)
	{
		//Commonly used values, conversions, etc...
		Pi = acos(-1.);
		pi = Pi;
		r2d = 180./pi;
		d2r = pi/180.;
		g = 9.81;	//gravitation constant
		mtoinch = 39.3700787;	//[m] -> [in]

		is_sf_init = false;
		
		//Set all values to NaN or nonsense value to prevent misuse
		latitude	= std::numeric_limits<double>::quiet_NaN();
		longitude	= std::numeric_limits<double>::quiet_NaN();
		timezone	= std::numeric_limits<double>::quiet_NaN();
		theta_stow	= std::numeric_limits<double>::quiet_NaN();
		theta_dep	= std::numeric_limits<double>::quiet_NaN();
		interp_arr	= -1;
		rad_type	= -1;
		solarm	= std::numeric_limits<double>::quiet_NaN();
		T_sfdes	= std::numeric_limits<double>::quiet_NaN();
		irr_des	= std::numeric_limits<double>::quiet_NaN();
		eta_opt_soil	= std::numeric_limits<double>::quiet_NaN();
		eta_opt_gen	= std::numeric_limits<double>::quiet_NaN();
		f_sfhl_ref	= std::numeric_limits<double>::quiet_NaN();
		sfhlQ_coefs	= NULL;
		nval_sfhlQ_coefs = -1;
		sfhlT_coefs	= NULL;
		nval_sfhlT_coefs = -1;
		sfhlV_coefs	= NULL;
		nval_sfhlV_coefs = -1;
		qsf_des	= std::numeric_limits<double>::quiet_NaN();
		w_des	= std::numeric_limits<double>::quiet_NaN();
		eta_des	= std::numeric_limits<double>::quiet_NaN();
		f_wmax	= std::numeric_limits<double>::quiet_NaN();
		f_wmin	= std::numeric_limits<double>::quiet_NaN();
		f_startup	= std::numeric_limits<double>::quiet_NaN();
		eta_lhv	= std::numeric_limits<double>::quiet_NaN();
		etaQ_coefs	= NULL;
		nval_etaQ_coefs = -1;
		etaT_coefs	= NULL;
		nval_etaT_coefs = -1;
		T_pcdes	= std::numeric_limits<double>::quiet_NaN();
		PC_T_corr	= std::numeric_limits<double>::quiet_NaN();
		f_Wpar_fixed	= std::numeric_limits<double>::quiet_NaN();
		f_Wpar_prod	= std::numeric_limits<double>::quiet_NaN();
		Wpar_prodQ_coefs	= NULL;
		nval_Wpar_prodQ_coefs = -1;
		Wpar_prodT_coefs	= NULL;
		nval_Wpar_prodT_coefs = -1;
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
		OpticalTable_in	= NULL;
		nrow_OpticalTable = -1, ncol_OpticalTable = -1;
        istableunsorted = false;
        optical_table_uns = 0;  //NULL
        eff_scale = 1.;     //Set here just in case
        /*OpticalTableUns_in = NULL;
        nrow_OpticalTableUns = -1, ncol_OpticalTableUns = -1;*/
		ibn	= std::numeric_limits<double>::quiet_NaN();
		ibh	= std::numeric_limits<double>::quiet_NaN();
		itoth	= std::numeric_limits<double>::quiet_NaN();
		tdb	= std::numeric_limits<double>::quiet_NaN();
		twb	= std::numeric_limits<double>::quiet_NaN();
		vwind	= std::numeric_limits<double>::quiet_NaN();
		irr_used	= std::numeric_limits<double>::quiet_NaN();
		hour_of_day	= std::numeric_limits<double>::quiet_NaN();
		day_of_year	= std::numeric_limits<double>::quiet_NaN();
		declination	= std::numeric_limits<double>::quiet_NaN();
		soltime	= std::numeric_limits<double>::quiet_NaN();
		hrangle	= std::numeric_limits<double>::quiet_NaN();
		solalt	= std::numeric_limits<double>::quiet_NaN();
		solaz	= std::numeric_limits<double>::quiet_NaN();
		eta_opt_sf	= std::numeric_limits<double>::quiet_NaN();
		f_sfhl_qdni	= std::numeric_limits<double>::quiet_NaN();
		f_sfhl_tamb	= std::numeric_limits<double>::quiet_NaN();
		f_sfhl_vwind	= std::numeric_limits<double>::quiet_NaN();
		q_hl_sf	= std::numeric_limits<double>::quiet_NaN();
		q_sf	= std::numeric_limits<double>::quiet_NaN();
		q_inc	= std::numeric_limits<double>::quiet_NaN();
		pbmode	= -1;
		pbstartf	= -1;
		q_to_pb	= std::numeric_limits<double>::quiet_NaN();
		q_startup	= std::numeric_limits<double>::quiet_NaN();
		q_to_tes	= std::numeric_limits<double>::quiet_NaN();
		q_from_tes	= std::numeric_limits<double>::quiet_NaN();
		e_in_tes	= std::numeric_limits<double>::quiet_NaN();
		q_hl_tes	= std::numeric_limits<double>::quiet_NaN();
		q_dump_tesfull	= std::numeric_limits<double>::quiet_NaN();
		q_dump_teschg	= std::numeric_limits<double>::quiet_NaN();
		q_dump_umin	= std::numeric_limits<double>::quiet_NaN();
		q_dump_tot	= std::numeric_limits<double>::quiet_NaN();
		q_fossil	= std::numeric_limits<double>::quiet_NaN();
		q_gas	= std::numeric_limits<double>::quiet_NaN();
		f_effpc_qtpb	= std::numeric_limits<double>::quiet_NaN();
		f_effpc_tamb	= std::numeric_limits<double>::quiet_NaN();
		eta_cycle	= std::numeric_limits<double>::quiet_NaN();
		w_gr_solar	= std::numeric_limits<double>::quiet_NaN();
		w_gr_fossil	= std::numeric_limits<double>::quiet_NaN();
		w_gr	= std::numeric_limits<double>::quiet_NaN();
		w_par_fixed	= std::numeric_limits<double>::quiet_NaN();
		w_par_prod	= std::numeric_limits<double>::quiet_NaN();
		w_par_tot	= std::numeric_limits<double>::quiet_NaN();
		w_par_online	= std::numeric_limits<double>::quiet_NaN();
		w_par_offline	= std::numeric_limits<double>::quiet_NaN();
		enet	= std::numeric_limits<double>::quiet_NaN();

	}

	virtual ~sam_mw_gen_type260(){
		/* Clean up on simulation terminate */
        if(optical_table_uns != 0) 
            delete optical_table_uns;
	}

	virtual int init(){
		/*
		--Initialization call-- 
		
		Do any setup required here.
		Get the values of the inputs and parameters
		*/
		dt = time_step()/3600.;
		start_time = -1; 

		latitude = value(P_LATITUDE);		//Site latitude [deg]
		longitude = value(P_LONGITUDE);		//Site longitude [deg]
		timezone = value(P_TIMEZONE);		//Site timezone [hr]
		theta_stow = value(P_THETA_STOW);		//Solar elevation angle at which the solar field stops operating [deg]
		theta_dep = value(P_THETA_DEP);		//Solar elevation angle at which the solar field begins operating [deg]
		interp_arr = (int)value(P_INTERP_ARR);		//Interpolate the array or find nearest neighbor? (1=interp,2=no) [none]
		rad_type = (int)value(P_RAD_TYPE);		//Solar resource radiation type (1=DNI,2=horiz.beam,3=tot.horiz) [none]
		solarm = value(P_SOLARM);		//Solar multiple [none]
		T_sfdes = value(P_T_SFDES);		//Solar field design point temperature (dry bulb) [C]
		irr_des = value(P_IRR_DES);		//Irradiation design point [W/m2]
		eta_opt_soil = value(P_ETA_OPT_SOIL);		//Soiling optical derate factor [none]
		eta_opt_gen = value(P_ETA_OPT_GEN);		//General/other optical derate [none]
		f_sfhl_ref = value(P_F_SFHL_REF);		//Reference solar field thermal loss fraction [MW/MWcap]
		sfhlQ_coefs = value(P_SFHLQ_COEFS, &nval_sfhlQ_coefs);		//Irr-based solar field thermal loss adjustment coefficients [1/MWt]
		sfhlT_coefs = value(P_SFHLT_COEFS, &nval_sfhlT_coefs);		//Temp.-based solar field thermal loss adjustment coefficients [1/C]
		sfhlV_coefs = value(P_SFHLV_COEFS, &nval_sfhlV_coefs);		//Wind-based solar field thermal loss adjustment coefficients [1/(m/s)]
		qsf_des = value(P_QSF_DES);		//Solar field thermal production at design [MWt]
		w_des = value(P_W_DES);		//Design power cycle gross output [MWe]
		eta_des = value(P_ETA_DES);		//Design power cycle gross efficiency [none]
		f_wmax = value(P_F_WMAX);		//Maximum over-design power cycle operation fraction [none]
		f_wmin = value(P_F_WMIN);		//Minimum part-load power cycle operation fraction [none]
		f_startup = value(P_F_STARTUP);		//Equivalent full-load hours required for power system startup [hours]
		eta_lhv = value(P_ETA_LHV);		//Fossil backup lower heating value efficiency [none]
		etaQ_coefs = value(P_ETAQ_COEFS, &nval_etaQ_coefs);		//Part-load power conversion efficiency adjustment coefficients [1/MWt]
		etaT_coefs = value(P_ETAT_COEFS, &nval_etaT_coefs);		//Temp.-based power conversion efficiency adjustment coefs. [1/C]
		T_pcdes = value(P_T_PCDES);		//Power conversion reference temperature [C]
		PC_T_corr = value(P_PC_T_CORR);		//Power conversion temperature correction mode (1=wetb, 2=dryb) [none]
		f_Wpar_fixed = value(P_F_WPAR_FIXED);		//Fixed capacity-based parasitic loss fraction [MWe/MWcap]
		f_Wpar_prod = value(P_F_WPAR_PROD);		//Production-based parasitic loss fraction [MWe/MWe]
		Wpar_prodQ_coefs = value(P_WPAR_PRODQ_COEFS, &nval_Wpar_prodQ_coefs);		//Part-load production parasitic adjustment coefs. [1/MWe]
		Wpar_prodT_coefs = value(P_WPAR_PRODT_COEFS, &nval_Wpar_prodT_coefs);		//Temp.-based production parasitic adjustment coefs. [1/C]
		hrs_tes = value(P_HRS_TES);		//Equivalent full-load hours of storage [hours]
		f_charge = value(P_F_CHARGE);		//Storage charging energy derate [none]
		f_disch = value(P_F_DISCH);		//Storage discharging energy derate [none]
		f_etes_0 = value(P_F_ETES_0);		//Initial fractional charge level of thermal storage (0..1) [none]
		f_teshl_ref = value(P_F_TESHL_REF);		//Reference heat loss from storage per max stored capacity [kWt/MWhr-stored]
		teshlX_coefs = value(P_TESHLX_COEFS, &nval_teshlX_coefs);		//Charge-based thermal loss adjustment - constant coef. [1/MWhr-stored]
		teshlT_coefs = value(P_TESHLT_COEFS, &nval_teshlT_coefs);		//Temp.-based thermal loss adjustment - constant coef. [1/C]
		ntod = (int)value(P_NTOD);		//Number of time-of-dispatch periods in the dispatch schedule [none]
		//tod_sched = value(P_TOD_SCHED, &nval_tod_sched);		//Array of touperiod indices [none]

		disws = value(P_DISWS, &nval_disws);		//Time-of-dispatch control for with-solar conditions [none]
		diswos = value(P_DISWOS, &nval_diswos);		//Time-of-dispatch control for without-solar conditions [none]
		qdisp = value(P_QDISP, &nval_qdisp);		//touperiod power output control factors [none]
		fdisp = value(P_FDISP, &nval_fdisp);		//Fossil backup output control factors [none]
		OpticalTable_in = value(P_OPTICALTABLE, &nrow_OpticalTable, &ncol_OpticalTable);		//Optical table [none]
        istableunsorted = value(P_ISTABLEUNSORTED) == 1.;

		//Rearrange the optical table into a more useful format
		OpticalTable.assign(OpticalTable_in, nrow_OpticalTable, ncol_OpticalTable);

		//Unit conversions
		latitude *= d2r;
		longitude *= d2r;
		theta_stow *= d2r;
		theta_dep *= d2r;
		T_sfdes += 273.15;
		T_pcdes += 273.15;
		f_teshl_ref *= .001;

		//Initial calculations
		//Power block design power
		q_des = w_des/eta_des;   //[MWt]
		//Maximum energy in storage
		etesmax = hrs_tes*q_des;   //[MW-hr]
		
		//Express the dispatch values in dimensional terms
		for(int i=0; i<ntod; i++){
			disws[i] *= etesmax;
			diswos[i] *= etesmax;
			qdisp[i] *= q_des;
			fdisp[i] *= q_des;
		}
		
		/* 
		Set up the optical table object..

		The input should be defined as follows:
		- Data of size nx, ny
		- OpticalTable of size (nx+1)*(ny+1)
		- First nx+1 values (row 1) are x-axis values, not data, starting at index 1
		- First value of remaining ny rows are y-axis values, not data
		- Data is contained in cells i,j : where i>1, j>1

        A second option using an unstructured array is also possible. The data should be defined as:
        - N rows
        - 3 values per row
        - Azimuth, Zenith, Efficiency point

        If the OpticalTableUns is given data, it will be used by default.

		*/
        if( ! istableunsorted )
        {
            /* 
            Standard azimuth-elevation table
            */

            //does the table look right?
            if( (nrow_OpticalTable < 5 && ncol_OpticalTable > 3 ) || (ncol_OpticalTable == 3 && nrow_OpticalTable > 4) )
                message(TCS_WARNING, "The optical efficiency table option flag may not match the specified table format. If running SSC, ensure \"IsTableUnsorted\""
                " =0 if regularly-spaced azimuth-zenith matrix is used and =1 if azimuth,zenith,efficiency points are specified.");

            if ( nrow_OpticalTable<=0 || ncol_OpticalTable<=0 ) // If these were not set correctly, it will create memory allocation crash not caught by error handling.
			    return -1;
		    double *xax = new double[ncol_OpticalTable-1];
		    double *yax = new double[nrow_OpticalTable-1];
		    double *data = new double[(ncol_OpticalTable -1) * (nrow_OpticalTable -1)];

		    //get the xaxis data values
		    for(int i=1; i<ncol_OpticalTable; i++){
			    xax[i-1] = OpticalTable.at(0, i)*d2r;
		    }
		    //get the yaxis data values
		    for(int j=1; j<nrow_OpticalTable; j++){
			    yax[j-1] = OpticalTable.at(j, 0)*d2r;
		    }
		    //Get the data values
		    for(int j=1; j<nrow_OpticalTable; j++){
			    for(int i=1; i<ncol_OpticalTable; i++){
				    data[ i-1 + (ncol_OpticalTable-1)*(j-1) ] = OpticalTable.at(j, i);
			    }
		    }

		    optical_table.AddXAxis(xax, ncol_OpticalTable-1);
		    optical_table.AddYAxis(yax, nrow_OpticalTable-1);
		    optical_table.AddData(data);
		    delete [] xax;
		    delete [] yax;
		    delete [] data;
        }
        else
        {
            /* 
            Use the unstructured data table
            */

            /* 
		    ------------------------------------------------------------------------------
		    Create the regression fit on the efficiency map
		    ------------------------------------------------------------------------------
		    */
		    
            if(ncol_OpticalTable != 3){
				message(TCS_ERROR,  "The heliostat field efficiency file is not formatted correctly. Type expects 3 columns"
					" (zenith angle, azimuth angle, efficiency value) and instead has %d cols.", ncol_OpticalTable);
				return -1;
			}
		    
            MatDoub sunpos;
		    vector<double> effs;

			//read the data from the array into the local storage arrays
			sunpos.resize(nrow_OpticalTable, VectDoub(2));
			effs.resize(nrow_OpticalTable);
            double eff_maxval = -9.e9;
			for(int i=0; i<nrow_OpticalTable; i++){
				sunpos.at(i).at(0) = TCS_MATRIX_INDEX( var( P_OPTICALTABLE ), i, 0 ) / az_scale * pi/180.;
				sunpos.at(i).at(1) = TCS_MATRIX_INDEX( var( P_OPTICALTABLE ), i, 1 ) / zen_scale * pi/180.;
				double eff = TCS_MATRIX_INDEX( var( P_OPTICALTABLE ), i, 2 );
                
                effs.at(i) = eff;
                if(eff > eff_maxval) eff_maxval = eff;    
			}

            //scale values based on maximum. This helps the GM interpolation routine
            eff_scale = eff_maxval;
            for( int i=0; i<nrow_OpticalTable; i++)
                effs.at(i) /= eff_scale;

		    //Create the field efficiency table
            Powvargram vgram(sunpos, effs, 1.99, 0.);
		    optical_table_uns = new GaussMarkov(sunpos, effs, vgram);

		    //test how well the fit matches the data
		    double err_fit = 0.;
		    int npoints = (int)sunpos.size();
		    for(int i=0; i<npoints; i++){
			    double zref = effs.at(i);
			    double zfit = optical_table_uns->interp( sunpos.at(i) );
			    double dz = zref - zfit;
			    err_fit += dz * dz;
		    }
		    err_fit = sqrt(err_fit);
		    if( err_fit > 0.01 )
			    message(TCS_WARNING, "The heliostat field interpolation function fit is poor! (err_fit=%f RMS)", err_fit);


        }

		//Calculate min/max turbine operation rates
		qttmin = q_des*f_wmin;
		qttmax = q_des*f_wmax;
		
		//Calculate max TES charging/discharging rates based on solar multiple
		ptsmax = q_des * solarm;
		pfsmax = ptsmax / f_disch*f_wmax;

		//Set initial storage values
		etes0 = f_etes_0*etesmax;		//[MW-hr] Initial value in thermal storage. This keeps track of energy in thermal storage, or e_in_tes
		pbmode0 = 0;					//[-] initial value of power block operation mode pbmode
		q_startup_remain = f_startup*q_des;     //[MW-hr] Initial value of turbine startup energy q_startup_used

		return true;
	}

	void init_sf()
	{
		//---- Calculate the "normalized" design-point thermal power. 
		
		//---Design point values---  
		//Calculate the design point efficiency based on the solar position at solstice noon, rather than maxval of the table
		omega = 0.0; //solar noon
		dec = 23.45*d2r; //declination at summer solstice
		//Solar altitude at noon on the summer solstice
		solalt = asin(sin(dec)*sin(latitude)+cos(latitude)*cos(dec)*cos(omega));
        double opt_des; 
        if(istableunsorted)
        {
            // Use current solar position to interpolate field efficiency table and find solar field efficiency
			vector<double> sunpos;
			sunpos.push_back(0.);
			sunpos.push_back((pi/2. - solalt)/zen_scale);

			opt_des = optical_table_uns->interp( sunpos ) * eff_scale;
        }
        else
        {
            opt_des = interp_arr == 1 ? 
			    optical_table.interpolate(0.0, max(pi/2.-solalt, 0.0)) :
			    optical_table.nearest(0.0, max(pi/2.-solalt, 0.0)) ;
        }
		eta_opt_ref = eta_opt_soil*eta_opt_gen*opt_des;
		f_qsf = qsf_des/(irr_des*eta_opt_ref*(1.-f_sfhl_ref));    //[MWt/([W/m2] * [-] * [-])]
    		
		return;
	}

	virtual int call(double time, double step, int ncall){
		/* 
		-- Standard timestep call --
		
		*get inputs
		*do calculations
		*set outputs

		*/

		//record the start time
		if(start_time < 0){ start_time = current_time(); }

		//If the solar field design point hasn't yet been initialized, do so here.
		if(! is_sf_init){
			//Re-read the location info from the weather reader
			latitude = value(P_LATITUDE)*d2r;
			longitude = value(P_LONGITUDE)*d2r;
			timezone = value(P_TIMEZONE);
			init_sf();

			is_sf_init = true;
		}

		
		//******************************************************************************************************************************
		//               Time-dependent conditions
		//******************************************************************************************************************************
		ibn = value(I_IBN);		//Beam-normal (DNI) irradiation [W/m^2]
		ibh = value(I_IBH);		//Beam-horizontal irradiation [W/m^2]
		itoth = value(I_ITOTH);	//Total horizontal irradiation [W/m^2]
		tdb = value(I_TDB);		//Ambient dry-bulb temperature [C]
		twb = value(I_TWB);		//Ambient wet-bulb temperature [C]
		vwind = value(I_VWIND);		//Wind velocity [m/s]

		double shift = longitude - timezone*15.*d2r;
	  //int touperiod = CSP::TOU_Reader(tod_sched, time, nval_tod_sched); 
		int touperiod = (int)value(I_TOUPeriod) - 1; // control value between 1 & 9, have to change to 0-8 for array index

		//Unit conversions
		tdb += 273.15;
		twb += 273.15;



		//Choose which irradiation source will be used
		switch(rad_type)
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


		
		//------------------------------------------------------------------------------------------------------------
		//       Solar field calculations
		//------------------------------------------------------------------------------------------------------------
		hour_of_day = fmod(time/3600., 24.);       //hour_of_day of the day (1..24)
		day_of_year = ceil(time/3600./24.);  //Day of the year
		// Duffie & Beckman 1.5.3b
		double B = (day_of_year-1)*360.0/365.0*pi/180.0;
		// Eqn of time in minutes
		double EOT = 229.2 * (0.000075 + 0.001868 * cos(B) - 0.032077 * sin(B)	- 0.014615 * cos(B*2.0) - 0.04089 * sin(B*2.0));
		// Declination in radians (Duffie & Beckman 1.6.1)
		dec = 23.45 * sin(360.0*(284.0+day_of_year)/365.0*pi/180.0) * pi/180.0;
		// Solar Noon and time in hours
		double SolarNoon = 12. - ((shift)*180.0/pi) / 15.0 - EOT / 60.0;

		// 3.13.16 twn: 'TSnow' doesn't seem to be used anywhere, so commenting this out
		/*double TSnow;
		if ((hour_of_day - int(hour_of_day)) == 0.00){
			TSnow = 1.0;
		}
		else{
			TSnow = (hour_of_day - floor(hour_of_day))/dt + 1.;
		}*/

		// Deploy & stow times in hours
		// Calculations modified by MJW 11/13/2009 to correct bug
		theta_dep = max(theta_dep,1.e-6);
		double DepHr1 = cos(latitude) / tan(theta_dep);
		double DepHr2 = -tan(dec) * sin(latitude) / tan(theta_dep);
		double DepHr3 = (tan(pi-theta_dep) < 0. ? -1. : 1.)*acos((DepHr1*DepHr2 + sqrt(DepHr1*DepHr1-DepHr2*DepHr2+1.0)) / (DepHr1 * DepHr1 + 1.0)) * 180.0 / pi / 15.0;
		double DepTime = SolarNoon + DepHr3;

		theta_stow = max(theta_stow,1.e-6);
		double StwHr1 = cos(latitude) / tan(theta_stow);
		double StwHr2 = -tan(dec) * sin(latitude) / tan(theta_stow);
		double StwHr3 = (tan(pi-theta_stow) < 0. ? -1. : 1.)*acos((StwHr1*StwHr2 + sqrt(StwHr1*StwHr1-StwHr2*StwHr2+1.0)) / (StwHr1 * StwHr1 + 1.0)) * 180.0 / pi / 15.0;
		double StwTime = SolarNoon + StwHr3;

		// Ftrack is the fraction of the time period that the field is tracking. MidTrack is time at midpoint of operation
		double HrA = hour_of_day-dt;
		double HrB = hour_of_day;

		// Solar field operates
		double Ftrack, MidTrack;
		if ((HrB > DepTime) && (HrA < StwTime)){
			// solar field deploys during time period
			if (HrA < DepTime){
				Ftrack = (HrB - DepTime) *dt;
				MidTrack = HrB - Ftrack * 0.5 *dt;
			// Solar field stows during time period
			}
			else if (HrB > StwTime){
				Ftrack = (StwTime - HrA) *dt;
				MidTrack = HrA + Ftrack * 0.5 *dt;
			}
			// solar field operates during entire period
			else{
				Ftrack = 1.0;
				MidTrack = HrA + 0.5 *dt;
			}
		}
		// solar field doesn't operate
		else{
			Ftrack = 0.0;
			MidTrack = HrA + 0.5 *dt;
		}

		double StdTime = MidTrack;
		soltime = StdTime+((shift)*180.0/pi)/15.0+ EOT/60.0;
		// hour_of_day angle (arc of sun) in radians
		omega = (soltime - 12.0)*15.0*pi/180.0;
		// B. Stine equation for Solar Altitude angle in radians
		solalt = asin(sin(dec)*sin(latitude)+cos(latitude)*cos(dec)*cos(omega));
		solaz = (omega < 0. ? -1. : 1.)*fabs(acos(min(1.0,(cos(pi/2.-solalt)*sin(latitude)-sin(dec))/(sin(pi/2.-solalt)*cos(latitude)))));

		//Get the current optical efficiency
		double opt_val;
        if(istableunsorted)
        {
            // Use current solar position to interpolate field efficiency table and find solar field efficiency
			vector<double> sunpos;
			sunpos.push_back(solaz/az_scale);
			sunpos.push_back((pi/2. - solalt)/zen_scale);

			opt_val = optical_table_uns->interp( sunpos ) * eff_scale;
        }
        else
        {
		    opt_val = interp_arr == 1 ? 
			    optical_table.interpolate(solaz, max(pi/2.-solalt, 0.0)) :
			    optical_table.nearest(solaz, max(pi/2.-solalt, 0.0) );
        }
		
		double eta_arr = max(opt_val*Ftrack, 0.0);  //mjw 7.25.11 limit zenith to <90, otherwise the interpolation error message gets called during night hours.
		eta_opt_sf = eta_arr*eta_opt_soil*eta_opt_gen;

		//Evaluate solar feild thermal efficiency derate
		f_sfhl_qdni = 0.;
		f_sfhl_tamb = 0.;
		f_sfhl_vwind = 0.;
		for(int i=0; i<nval_sfhlQ_coefs; i++)
			f_sfhl_qdni += sfhlQ_coefs[i]*pow(irr_used/irr_des, i);
		for(int i=0; i<nval_sfhlT_coefs; i++)
			f_sfhl_tamb += sfhlT_coefs[i]*pow(tdb - T_sfdes, i);
		for(int i=0; i<nval_sfhlV_coefs; i++)
			f_sfhl_vwind += sfhlV_coefs[i]*pow(vwind, i);
		
		double f_sfhl = 1.0 - f_sfhl_ref * f_sfhl_qdni * f_sfhl_tamb * f_sfhl_vwind;  //This ratio indicates the sf thermal efficiency
		q_hl_sf = f_qsf*irr_used*(1.-f_sfhl)*eta_opt_sf;   //[MWt]

		//Calculate the total solar field thermal output 
		q_sf = f_qsf * f_sfhl * eta_opt_sf * irr_used;    //[MWt]

		//------------------------------------------------------------------------------------------------------------
		//       Dispatch calculations
		//------------------------------------------------------------------------------------------------------------
		// initialize outputs to 0
		q_to_tes = 0.;     //| Energy to Thermal Storage 
		q_from_tes = 0.;     //| Energy from Thermal Storage
		e_in_tes = 0.;     //| Energy in Thermal Storage
		q_hl_tes = 0.;     //| Energy losses from Thermal Storage
		q_to_pb = 0.;     //| Energy to the Power Block
		q_dump_tesfull = 0.;     //| Energy dumped because the thermal storage is full
		q_dump_umin = 0.;     //| Indicator of being below minimum operation level
		q_dump_teschg = 0.;     //| The amount of energy dumped (more than turbine and storage)
		q_startup = 0.;     //| The energy needed to startup the turbine
		pbstartf = 0;     //| is 1 during the period when powerblock starts up otherwise 0
		q_startup_used = q_startup_remain;   //| Turbine startup energy for this timestep is equal to the remaining previous energy

		//--------Plant dispatch strategy--------------------
		if (hrs_tes <= 0.){ // No Storage
			if ((pbmode0==0)||(pbmode0==1)){ // if plant is not already operating in last timestep
				if (q_sf>0){
					if (q_sf>(q_startup_used/dt)){ //  Starts plant as exceeds startup energy needed
						q_to_pb = q_sf - q_startup_used/dt;
						q_startup = q_startup_used/dt;
						pbmode = 2;      //Power block mode.. 2=starting up
						pbstartf = 1;    //Flag indicating whether the power block starts up in this time period
						q_startup_used = 0.;     //mjw 5-31-13 Reset to zero to handle cases where Qsf-TurSue leads to Qttb < Qttmin
					}
					else{ //  Plant starting up but not enough energy to make it run - will probably finish in the next timestep
						q_to_pb = 0.;
						q_startup_used = q_startup_remain - q_sf*dt;
						q_startup = q_sf;
						pbmode = 1;
						pbstartf = 0;
					}
				}
				else{ // No solar field output so still need same amount of energy as before and nothing changes
					q_startup_used = f_startup * q_des;
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
					q_startup_used = q_startup_remain;
				}
			}

			// following happens no matter what state the powerblock was in previously      
			if (q_to_pb < qttmin){ // Energy to powerblock less than the minimum that the turbine can run at
				q_dump_umin =  q_to_pb;         // The minimum energy (less than the minimum)
				q_to_pb = 0;             // Energy to PB is now 0
				pbmode = 0;           // PB turned off
			}

			if (q_to_pb > qttmax){   // Energy to powerblock greater than what the PB can handle (max)
				q_dump_teschg =  q_to_pb - qttmax; // The energy dumped 
				q_to_pb = qttmax;          // the energy to the PB is exactly the maximum
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
				if ((q_sf + EtesA >= qdisp[touperiod]) || (q_sf > ptsmax)){

					// Assumes Operator started plant during previous time period
					// But TRNSYS cannot do this, so start-up energy is deducted during current timestep.     
					pbmode = 1;
					q_startup = f_startup * q_des /dt;

					q_to_pb = qdisp[touperiod];       // set the energy to powerblock equal to the load for this TOU period

					if (q_sf>q_to_pb){             // if solar field output is greater than what the necessary load ?
						q_to_tes = q_sf - q_to_pb;           // the extra goes to thermal storage
						q_from_tes = q_startup;               // Use the energy from thermal storage to startup the power cycle
						if (q_to_tes>ptsmax){       // if q to thermal storage exceeds thermal storage max rate Added 9-10-02
							q_dump_teschg = q_to_tes - ptsmax;   // then dump the excess for this period Added 9-10-02
							q_to_tes = ptsmax;
						}                     
					}
					else{ // q_sf less than the powerblock requirement
						q_to_tes = 0.0;
						q_from_tes = q_startup + (1 -  q_sf /  q_to_pb) * min(pfsmax, q_des);
						if (q_from_tes>pfsmax) q_from_tes = pfsmax;
						q_to_pb = q_sf + (1 -  q_sf /  q_to_pb) * min(pfsmax, q_des);
					}
                
					e_in_tes = etes0 - q_startup + (q_sf - q_to_pb) * dt;   // thermal storage energy is initial + what was left 
					pbmode = 2;   // powerblock is now running
					pbstartf = 1; // the powerblock turns on during this timeperiod.
				}
				else{ //Store energy not enough stored to start plant
					q_to_tes = q_sf; // everything goes to thermal storage
					q_from_tes = 0;   // nothing from thermal storage
					e_in_tes = etes0 + q_to_tes * dt ; 
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
						q_from_tes = (1. - q_sf / q_to_pb) * min(pfsmax, q_des);
						if (q_from_tes>pfsmax) q_from_tes = min(pfsmax, q_des);
						q_to_pb = q_from_tes + q_sf;
					}

					e_in_tes = etes0 + (q_sf - q_to_pb - q_dump_teschg) *dt;  // energy of thermal storage is the extra

					// Check to see if throwing away energy 
					if ( (e_in_tes>etesmax) && (q_to_pb<qttmax) ){ // qttmax (MWt) - power to turbine max
						if ((e_in_tes - etesmax)/dt < (qttmax - q_to_pb) ){
							q_to_pb = q_to_pb + (e_in_tes - etesmax) /dt;
							e_in_tes = etesmax;
						}
						else{
							e_in_tes = e_in_tes - (qttmax - q_to_pb) * dt;  // should this be etes0 instead of e_in_tes on RHS ??
							q_to_pb = qttmax;
						}
						q_to_tes = q_sf - q_to_pb;
					}
				}
				else{  //Empties tes to dispatch level if above min load level

					if ((q_sf + max(0.,etes0-dispatch.at(touperiod)) * dt) > qttmin){  
						q_from_tes = max(0.,etes0-dispatch.at(touperiod)) * dt;
						q_to_pb = q_sf + q_from_tes;
						q_to_tes = 0.;
						e_in_tes = etes0 - q_from_tes;
					}
					else{
						q_to_pb = 0.;
						q_from_tes = 0.;
						q_to_tes = q_sf;
						e_in_tes = etes0 + q_to_tes * dt;
					}
				}
			}

			if (q_to_pb>0)
				pbmode = 2;
			else
				pbmode = 0;
			

			//---Calculate TES thermal losses ---
			// First do the charge-based losses. Use the average charge over the timestep
			double f_EtesAve = max((e_in_tes + etes0)/2./etesmax, 0.0);
			double f_teshlX = 0., f_teshlT = 0.;
			for(int i=0; i<nval_teshlX_coefs; i++)
				f_teshlX += teshlX_coefs[i]*pow(f_EtesAve, i);     //Charge adjustment factor
			for(int i=0; i<nval_teshlT_coefs; i++)
				f_teshlT += teshlT_coefs[i]*pow(T_sfdes - tdb, i);
			//thermal storage heat losses adjusted by charge level and ambient temp.
			q_hl_tes = f_teshl_ref*etesmax*f_teshlX*f_teshlT;

			e_in_tes = max(e_in_tes - q_hl_tes*dt, 0.0); // Adjust the energy in thermal storage according to TES thermal losses

			if (e_in_tes>etesmax){ // trying to put in more than storage can handle
				q_dump_tesfull = (e_in_tes - etesmax)/dt;  //this is the amount dumped when storage is completely full
				e_in_tes = etesmax;
				q_to_tes = q_to_tes - q_dump_tesfull;
			}
			else{
				q_dump_tesfull = 0.; // nothing is dumped if not overfilled
			}

			// Check min and max on turbine
			if (q_to_pb<qttmin){
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
			// 5.23.16 twn bug fix (see svn)
		if( q_to_pb < fdisp[touperiod] )
		{	// If the thermal power dispatched to the power cycle is less than the level in the fossil control
			q_fossil = fdisp[touperiod] - q_to_pb;	// then the fossil used is the fossil control value minus what's provided by the solar field
			q_gas = q_fossil / eta_lhv;       // Calculate the required fossil heat content based on the LHV efficiency
		}
		else
		{
			q_fossil = 0.0;
			q_gas = 0.0;
		}
		
		//if (q_sf < fdisp[touperiod]){      // if the solar provided is less than the level stipulated in the fossil control
		//	q_fossil = fdisp[touperiod] - q_sf;     // then the fossil used is the fossil control value minus what's provided by the solar field
		//	q_gas = q_fossil / eta_lhv;       // Calculate the required fossil heat content based on the LHV efficiency
		//}
		//else{
		//	q_fossil = 0.;
		//	q_gas = 0.;
		//}

		//Adjust the power block energy based on additional fossil backup
		q_to_pb = q_to_pb + q_fossil;
    
		//------------------------------------------------------------------------------------------------------------
		//       Power block calculations
		//------------------------------------------------------------------------------------------------------------
		double qnorm = q_to_pb/q_des;             //The normalized thermal energy flow
		double tnorm;
		if(PC_T_corr==1.)      //Select the dry or wet bulb temperature as the driving difference
			tnorm = twb - T_pcdes;
		else
			tnorm = tdb - T_pcdes;
		
		//Calculate the load-based and temperature-based efficiency correction factors
		f_effpc_qtpb = 0.;
		f_effpc_tamb = 0.;
		for(int i=0; i<nval_etaQ_coefs; i++)
			f_effpc_qtpb += etaQ_coefs[i]*pow(qnorm, i); 
		for(int i=0; i<nval_etaT_coefs; i++)
			f_effpc_tamb += etaT_coefs[i]*pow(tnorm, i);
		eta_cycle = eta_des * f_effpc_qtpb * f_effpc_tamb;  //Adjusted power conversion efficiency

		if(q_to_pb <= 0.) eta_cycle = 0.0;  //Set conversion efficiency to zero when the power block isn't operating

		//Calculate the gross power
		w_gr = q_to_pb * eta_cycle;
		//Keep track of what portion is from solar
		w_gr_solar = (q_to_pb - q_fossil)*eta_cycle;


		//------------------------------------------------------------------------------------------------------------
		//       Parasitics
		//------------------------------------------------------------------------------------------------------------
		w_par_fixed = f_Wpar_fixed * w_des;       //Fixed parasitic loss based on plant capacity
		//Production-based parasitic loss
		double wpar_prodq = 0., wpar_prodt = 0.;
		for(int i=0; i<nval_Wpar_prodQ_coefs; i++)
			wpar_prodq += Wpar_prodQ_coefs[i]*pow(qnorm, i);	//Power block part-load correction factor
		for(int i=0; i<nval_Wpar_prodT_coefs; i++)
			wpar_prodt += Wpar_prodT_coefs[i]*pow(tnorm, i);	//Temperature correction factor
		w_par_prod = f_Wpar_prod * w_gr * wpar_prodq * wpar_prodt;

		w_par_tot = w_par_fixed + w_par_prod;   //Total parasitic loss

		//Keep track of online/offline parasitics
		if(w_gr > 0.){
			w_par_online = w_par_tot;
			w_par_offline = 0.;
		}
		else{
			w_par_online = 0.;
			w_par_offline = w_par_tot;
		}

		//---Calculate net energy output (enet<-->Wnet) ---
		enet = w_gr - w_par_tot;



		//Calculate final values
		declination = dec*r2d;           //[deg] Declination angle
		hrangle = omega*r2d;         //[deg] hour_of_day angle
		solalt = max(solalt*r2d, 0.0); //[deg] Solar elevation angle
		solaz = solaz*r2d;       //[deg] Solar azimuth angle (-180..180, 0deg=South)
		q_inc = f_qsf*irr_used;             //[MWt] Qdni - Solar incident energy, before all losses
		q_dump_tot = q_dump_tesfull + q_dump_teschg + q_dump_umin; //[MWt] Total dumped energy
		w_gr_fossil = w_gr - w_gr_solar;          //[MWe] Power produced from the fossil component
		

		//Set outputs and return
		value(O_IRR_USED, irr_used);		//[W/m2] Irradiation value used in simulation
		value(O_HOUR_OF_DAY, hour_of_day);		//[hour_of_day] hour_of_day of the day
		value(O_DAY_OF_YEAR, day_of_year);		//[day] Day of the year
		value(O_DECLINATION, declination);		//[deg] Declination angle
		value(O_SOLTIME, soltime);		//[hour_of_day] [hour_of_day] Solar time of the day
		value(O_HRANGLE, hrangle);		//[deg] hour_of_day angle
		value(O_SOLALT, solalt);		//[deg] Solar elevation angle
		value(O_SOLAZ, solaz);		//[deg] Solar azimuth angle (-180..180, 0deg=South)
		value(O_ETA_OPT_SF, eta_opt_sf);		//[none] Solar field optical efficiency
		value(O_F_SFHL_QDNI, f_sfhl_qdni);		//[none] Solar field load-based thermal loss correction
		value(O_F_SFHL_TAMB, f_sfhl_tamb);		//[none] Solar field temp.-based thermal loss correction
		value(O_F_SFHL_VWIND, f_sfhl_vwind);		//[none] Solar field wind-based thermal loss correction
		value(O_Q_HL_SF, q_hl_sf);		//[MWt] Solar field thermal losses
		value(O_Q_SF, q_sf);		//[MWt] Solar field delivered thermal power
		value(O_Q_INC, q_inc);		//[MWt] Qdni - Solar incident energy, before all losses
		value(O_PBMODE, pbmode);		//[none] Power conversion mode
		value(O_PBSTARTF, pbstartf);		//[none] Flag indicating power system startup
		value(O_Q_TO_PB, q_to_pb);		//[MWt] Thermal energy to the power conversion system
		value(O_Q_STARTUP, q_startup);		//[MWt] Power conversion startup energy
		value(O_Q_TO_TES, q_to_tes);		//[MWt] Thermal energy into storage
		value(O_Q_FROM_TES, q_from_tes);		//[MWt] Thermal energy from storage
		value(O_E_IN_TES, e_in_tes);		//[MWt-hr] Energy in storage
		value(O_Q_HL_TES, q_hl_tes);		//[MWt] Thermal losses from storage
		value(O_Q_DUMP_TESFULL, q_dump_tesfull);		//[MWt] Dumped energy  exceeding storage charge level max
		value(O_Q_DUMP_TESCHG, q_dump_teschg);		//[MWt] Dumped energy exceeding exceeding storage charge rate
		value(O_Q_DUMP_UMIN, q_dump_umin);		//[MWt] Dumped energy from falling below min. operation fraction
		value(O_Q_DUMP_TOT, q_dump_tot);		//[MWt] Total dumped energy
		value(O_Q_FOSSIL, q_fossil);		//[MWt] thermal energy supplied from aux firing
		value(O_Q_GAS, q_gas);		//[MWt] Energy content of fuel required to supply Qfos
		value(O_F_EFFPC_QTPB, f_effpc_qtpb);		//[none] Load-based conversion efficiency correction
		value(O_F_EFFPC_TAMB, f_effpc_tamb);		//[none] Temp-based conversion efficiency correction
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
		etes0 = e_in_tes;
		pbmode0 = pbmode;
		q_startup_remain = q_startup_used;
		return 0;
	}


};

TCS_IMPLEMENT_TYPE( sam_mw_gen_type260, "Generic Solar Model", "Mike Wagner", 1, sam_mw_gen_type260_variables, NULL, 1 );






















