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

using namespace std;

enum{
	P_ETA_LHV,
	P_ETA_TES_HTR,
	P_FP_MODE,

	I_W_CYCLE_GROSS,
	I_W_PAR_HEATREJ,
	I_W_PAR_SF_PUMP,
	I_W_PAR_TES_PUMP,
	I_W_PAR_BOP,
	I_W_PAR_FIXED,
	I_W_PAR_TRACKING,
	I_W_PAR_AUX_BOILER,
	I_Q_PAR_TES_FP,
	I_Q_PAR_SF_FP,
	I_Q_AUX_BACKUP,

	O_W_NET,
	O_HOURLY_ENERGY,
	O_W_PAR_TOT,
	O_FUEL_USAGE,
	O_Q_FP_TOT,

	//Include N_max
	N_MAX
};

tcsvarinfo sam_mw_csp_SumCalcs_variables[] = {
	{ TCS_PARAM,          TCS_NUMBER,           P_ETA_LHV,                "eta_lhv",                 "Fossil fuel lower heating value - Thermal power generated per unit fuel",     "MW/MMBTU",             "",             "",          "0.9" },
	{ TCS_PARAM,          TCS_NUMBER,       P_ETA_TES_HTR,            "eta_tes_htr",                                 "Thermal storage tank heater efficiency (fp_mode=1 only)",         "none",             "",             "",         "0.98" },
	{ TCS_PARAM,          TCS_NUMBER,           P_FP_MODE,                "fp_mode",                        "Freeze protection mode (1=Electrical heating ; 2=Fossil heating)",         "none",             "",             "",            "1" },

	{ TCS_INPUT,          TCS_NUMBER,     I_W_CYCLE_GROSS,          "W_cycle_gross",                                            "Electrical source - Power cycle gross output",           "MW",             "",             "",             "" },
	{ TCS_INPUT,          TCS_NUMBER,     I_W_PAR_HEATREJ,          "W_par_heatrej",                                "Electrical parasitic - power cycle heat rejection system",           "MW",             "",             "",             "" },
	{ TCS_INPUT,          TCS_NUMBER,     I_W_PAR_SF_PUMP,          "W_par_sf_pump",                                    "Electrical parasitic - solar field HTF pumping power",           "MW",             "",             "",             "" },
	{ TCS_INPUT,          TCS_NUMBER,    I_W_PAR_TES_PUMP,         "W_par_tes_pump",                                       "Electrical parasitic - TES dispatch pumping power",           "MW",             "",             "",             "" },
	{ TCS_INPUT,          TCS_NUMBER,         I_W_PAR_BOP,              "W_par_BOP",            "Electrical parasitic - Balance of plant equipment - variable with generation",           "MW",             "",             "",             "" },
	{ TCS_INPUT,          TCS_NUMBER,       I_W_PAR_FIXED,            "W_par_fixed",                          "Electrical parasitic - Constant parasitic for plant operations",           "MW",             "",             "",             "" },
	{ TCS_INPUT,          TCS_NUMBER,    I_W_PAR_TRACKING,         "W_par_tracking",         "Electrical parasitic - Power required for solar field collector drive operation",           "MW",             "",             "",             "" },
	{ TCS_INPUT,          TCS_NUMBER,  I_W_PAR_AUX_BOILER,       "W_par_aux_boiler",     "Electrical parasitic - Electrical power required to operate auxiliary fossil system",           "MW",             "",             "",             "" },
	{ TCS_INPUT,          TCS_NUMBER,      I_Q_PAR_TES_FP,           "Q_par_tes_fp",                      "Modal parasitic - Thermal energy used for freeze protection in TES",           "MW",             "",             "",             "" },
	{ TCS_INPUT,          TCS_NUMBER,       I_Q_PAR_SF_FP,            "Q_par_sf_fp", "Modal parasitic - Thermal energy used for freeze protection in the receiver/solar field",           "MW",             "",             "",             "" },
	{ TCS_INPUT,          TCS_NUMBER,      I_Q_AUX_BACKUP,           "Q_aux_backup","Thermal source - Thermal power provided by the auxiliary fossil backup system for generation",       "MW",             "",             "",             "" },

	{ TCS_OUTPUT,          TCS_NUMBER,             O_W_NET,                  "W_net",                                      "Net electricity generation (or usage) by the plant",           "MW",             "",             "",             "" },
	{ TCS_OUTPUT,          TCS_NUMBER,     O_HOURLY_ENERGY,               "W_net_kW",                                                                           "Hourly Energy",           "kW",             "",             "",             "" },
	{ TCS_OUTPUT,          TCS_NUMBER,         O_W_PAR_TOT,              "W_par_tot",                          "Total electrical parasitic consumption by all plant subsystems",           "MW",             "",             "",             "" },
	{ TCS_OUTPUT,          TCS_NUMBER,        O_FUEL_USAGE,             "Fuel_usage",                                         "Total fossil fuel usage by all plant subsystems",        "MMBTU",             "",             "",             "" },
	{ TCS_OUTPUT,          TCS_NUMBER,          O_Q_FP_TOT,               "Q_fp_tot",                                      "Total freeze protection thermal energy requirement",           "MW",             "",             "",             "" },

	{ TCS_INVALID,        TCS_INVALID,               N_MAX,                         0,                                                                                        0,              0,                0,            0,              0 }
};

class sam_mw_csp_SumCalcs : public tcstypeinterface
{
private:

	double eta_lhv;		//Fossil fuel lower heating value - Thermal power generated per unit fuel
	double eta_tes_htr;		//Thermal storage tank heater efficiency (fp_mode=1 only)
	double fp_mode;		//Freeze protection mode (1=Electrical heating ; 2=Fossil heating)

	double W_cycle_gross;		//Electrical source - Power cycle gross output
	double W_par_heatrej;		//Electrical parasitic - power cycle heat rejection system
	double W_par_sf_pump;		//Electrical parasitic - solar field HTF pumping power
	double W_par_tes_pump;		//Electrical parasitic - TES dispatch pumping power
	double W_par_BOP;		//Electrical parasitic - Balance of plant equipment - variable with generation
	double W_par_fixed;		//Electrical parasitic - Constant parasitic for plant operations
	double W_par_tracking;		//Electrical parasitic - Power required for solar field collector drive operation
	double W_par_aux_boiler;		//Electrical parasitic - Electrical power required to operate auxiliary fossil system
	double Q_par_tes_fp;		//Modal parasitic - Thermal energy used for freeze protection in TES
	double Q_par_sf_fp;		//Modal parasitic - Thermal energy used for freeze protection in the receiver/solar field
	double Q_aux_backup;		//Thermal source - Thermal power provided by the auxiliary fossil backup system for generation

	double W_net;		//Net electricity generation (or usage) by the plant
	double W_par_tot;		//Total electrical parasitic consumption by all plant subsystems
	double Fuel_usage;		//Total fossil fuel usage by all plant subsystems
	double Q_fp_tot;		//Total freeze protection thermal energy requirement

public:

	sam_mw_csp_SumCalcs(tcscontext *cxt, tcstypeinfo *ti)
		: tcstypeinterface(cxt, ti)
	{
		//Set all values to NaN or nonsense value to prevent misuse
		eta_lhv	= std::numeric_limits<double>::quiet_NaN();
		eta_tes_htr	= std::numeric_limits<double>::quiet_NaN();
		fp_mode	= std::numeric_limits<double>::quiet_NaN();
		W_cycle_gross	= std::numeric_limits<double>::quiet_NaN();
		W_par_heatrej	= std::numeric_limits<double>::quiet_NaN();
		W_par_sf_pump	= std::numeric_limits<double>::quiet_NaN();
		W_par_tes_pump	= std::numeric_limits<double>::quiet_NaN();
		W_par_BOP	= std::numeric_limits<double>::quiet_NaN();
		W_par_fixed	= std::numeric_limits<double>::quiet_NaN();
		W_par_tracking	= std::numeric_limits<double>::quiet_NaN();
		W_par_aux_boiler	= std::numeric_limits<double>::quiet_NaN();
		Q_par_tes_fp	= std::numeric_limits<double>::quiet_NaN();
		Q_par_sf_fp	= std::numeric_limits<double>::quiet_NaN();
		Q_aux_backup	= std::numeric_limits<double>::quiet_NaN();
		W_net	= std::numeric_limits<double>::quiet_NaN();
		W_par_tot	= std::numeric_limits<double>::quiet_NaN();
		Fuel_usage	= std::numeric_limits<double>::quiet_NaN();
		Q_fp_tot	= std::numeric_limits<double>::quiet_NaN();
		
	}

	virtual ~sam_mw_csp_SumCalcs(){
	}

	virtual int init(){
		/*
		--Initialization call-- 
		
		Do any setup required here.
		Get the values of the inputs and parameters
		*/
		eta_lhv = value(P_ETA_LHV);		//Fossil fuel lower heating value - Thermal power generated per unit fuel [MW/MMBTU]
		eta_tes_htr = value(P_ETA_TES_HTR);		//Thermal storage tank heater efficiency (fp_mode=1 only) [none]
		fp_mode = value(P_FP_MODE);		//Freeze protection mode (1=Electrical heating ; 2=Fossil heating) [none]

		return 0;
	}

	virtual int call(double /*time*/, double step, int /*ncall*/){

		/*
		
		E_net = [5,1] - ([4,29]+[4,28]+[4,27]+[4,17]+[3,18]+[4,26]+[3,5]+[5,8]+[3,6])  

		5 :: Power block
		1 | P_cycle			| MW
		8 | W_cool_par		| MW

		4 :: Controller
		17 | (q_tank_hot_htr + q_tank_cold_htr)/eta_heater_tank | MWe
		26 | htf_pump_power | MW
		27 | BOP_par		| MW
		28 | W_pb_design*PB_fixed_par/1.e6	| MW
		29 | Aux_par		| MW

		3 :: Solar field
		5  | W_dot_pump/1000.	| MW
		6  | E_fp_tot*1.e-6		| MW
		18 | SCA_par_tot/1.e6	| MW


		*/


		W_cycle_gross = value(I_W_CYCLE_GROSS);		//Electrical source - Power cycle gross output [MW]
		W_par_heatrej = value(I_W_PAR_HEATREJ);		//Electrical parasitic - power cycle heat rejection system [MW]
		W_par_sf_pump = value(I_W_PAR_SF_PUMP);		//Electrical parasitic - solar field HTF pumping power [MW]
		W_par_tes_pump = value(I_W_PAR_TES_PUMP);		//Electrical parasitic - TES dispatch pumping power [MW]
		W_par_BOP = value(I_W_PAR_BOP);		//Electrical parasitic - Balance of plant equipment - variable with generation [MW]
		W_par_fixed = value(I_W_PAR_FIXED);		//Electrical parasitic - Constant parasitic for plant operations [MW]
		W_par_tracking = value(I_W_PAR_TRACKING);		//Electrical parasitic - Power required for solar field collector drive operation [MW]
		W_par_aux_boiler = value(I_W_PAR_AUX_BOILER);		//Electrical parasitic - Electrical power required to operate auxiliary fossil system [MW]
		Q_par_tes_fp = value(I_Q_PAR_TES_FP);		//Modal parasitic - Thermal energy used for freeze protection in TES [MW]
		Q_par_sf_fp = value(I_Q_PAR_SF_FP);		//Modal parasitic - Thermal energy used for freeze protection in the receiver/solar field [MW]
		Q_aux_backup = value(I_Q_AUX_BACKUP);		//Thermal source - Thermal power provided by the auxiliary fossil backup system for generation [MW]

		
		//Sum all of the strictly electrical parasitic components
		W_par_tot = W_par_heatrej + W_par_sf_pump + W_par_tes_pump + W_par_BOP + W_par_fixed + W_par_tracking + W_par_aux_boiler;

		//Sum all of the freeze protection components
		Q_fp_tot = Q_par_tes_fp + Q_par_sf_fp;

		//Add freeze protection items to the appropriate category depending on the operation mode
		Fuel_usage = Q_aux_backup;
		if(fp_mode == 1){	//Electric heat tracing
			W_par_tot += Q_par_tes_fp/eta_tes_htr + Q_par_sf_fp;
		}
		else
		{
			Fuel_usage += Q_par_tes_fp + Q_par_sf_fp;
		}
		Fuel_usage *= 3.41214116*step/3600./eta_lhv;	//Convert from thermal power [MW] to fuel usage [MMBTU]. 1 MWhr = 3.412.. [MMBTU]

		//Calculate the final net power output
		W_net = W_cycle_gross - W_par_tot;


		//------ Set outputs and return -------------
		value(O_W_NET, W_net);		            //[MW] Net electricity generation (or usage) by the plant
		value(O_HOURLY_ENERGY, W_net*1000);		//[kW] Net electricity generation (or usage) by the plant
		value(O_W_PAR_TOT, W_par_tot);		    //[MW] Total electrical parasitic consumption by all plant subsystems
		value(O_FUEL_USAGE, Fuel_usage);		//[MMBTU] Total fossil fuel usage by all plant subsystems
		value(O_Q_FP_TOT, Q_fp_tot);		    //[MW] Total freeze protection thermal energy requirement

		
		return 0;
	}

	virtual int converged(double /*time*/){

		return 0;
	}
	
	
};


TCS_IMPLEMENT_TYPE( sam_mw_csp_SumCalcs, "Net electricity calculator for the Physical Trough", "Mike Wagner", 1, sam_mw_csp_SumCalcs_variables, NULL, 1 );