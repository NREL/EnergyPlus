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

enum{	//Parameters
		P_W_HTF_PC_PUMP,
		//P_PIPING_LOSS,
		//P_PIPING_LENGTH,
		P_Q_SF_DES,
		P_PB_FIXED_PAR,
		P_BOP_PAR,
		P_BOP_PAR_F,
		P_BOP_PAR_0,
		P_BOP_PAR_1,
		P_BOP_PAR_2,
		P_W_DOT_FOSSIL_DES,
		P_W_DOT_SOLAR_DES,

		//Inputs
		I_W_DOT_TRACKING,
		I_W_DOT_REC_PUMP,
		I_M_DOT_HTF_SS,
		I_W_DOT_PC_HYBRID,
		I_W_DOT_PC_FOSSIL,
		I_F_TIMESTEP,
		I_Q_SOLAR_SS,
		I_Q_DOT_FUEL,

		//Outputs
		O_W_DOT_PC_HYBRID,
		O_W_DOT_PC_FOSSIL,
		O_W_DOT_PLANT_HYBRID,
		O_W_DOT_PLANT_FOSSIL, 
		O_W_DOT_PLANT_SOLAR,
		O_ETA_SOLAR_USE,
		O_ETA_FUEL,
		O_SOLAR_FRACTION,
		O_P_PLANT_BALANCE_TOT,
		//O_P_PIPING_TOT,
		O_P_FIXED,

		//N_MAX
		N_MAX};

tcsvarinfo sam_iscc_parasitics_variables[] = {
	//PARAMETERS
	{TCS_PARAM, TCS_NUMBER, P_W_HTF_PC_PUMP,      "W_htf_pc_pump",          "Required pumping power for HTF through power block",             "kJ/kg",     "", "", ""},
	//{TCS_PARAM, TCS_NUMBER, P_PIPING_LOSS,        "Piping_loss",            "Thermal loss per meter of piping",                               "Wt/m",      "", "", ""},
	//{TCS_PARAM, TCS_NUMBER, P_PIPING_LENGTH,      "Piping_length",          "Total length of exposed piping",                                 "m",         "", "", ""},
	{TCS_PARAM, TCS_NUMBER, P_Q_SF_DES,           "Q_sf_des",               "Design point solar field thermal output",                        "MW",        "", "", ""},
	{TCS_PARAM, TCS_NUMBER, P_PB_FIXED_PAR,       "pb_fixed_par",           "Fixed parasitic load - runs at all times",                       "MWe/MWcap", "", "", ""},
	{TCS_PARAM, TCS_NUMBER, P_BOP_PAR,            "bop_par",                "Balance of plant parasitic power fraction",                      "MWe/MWcap", "", "", ""},
	{TCS_PARAM, TCS_NUMBER, P_BOP_PAR_F,          "bop_par_f",              "Balance of plant parasitic power fraction - mult frac",          "none",      "", "", ""},
	{TCS_PARAM, TCS_NUMBER, P_BOP_PAR_0,          "bop_par_0",              "Balance of plant parasitic power fraction - const coeff",        "none",      "", "", ""},
	{TCS_PARAM, TCS_NUMBER, P_BOP_PAR_1,          "bop_par_1",              "Balance of plant parasitic power fraction - linear coeff",       "none",      "", "", ""},
	{TCS_PARAM, TCS_NUMBER, P_BOP_PAR_2,          "bop_par_2",              "Balance of plant parasitic power fraction - quadratic coeff",    "none",      "", "", ""},
	{TCS_PARAM, TCS_NUMBER, P_W_DOT_FOSSIL_DES,   "W_dot_fossil_des",       "Fossil-only cycle output at design",                             "MWe",       "", "", ""},
	{TCS_PARAM, TCS_NUMBER, P_W_DOT_SOLAR_DES,    "W_dot_solar_des",        "Solar contribution to cycle output at design",                    "MWe",       "", "", ""},

	//INPUTS
	{TCS_INPUT, TCS_NUMBER, I_W_DOT_TRACKING,     "W_dot_tracking",         "Heliostat tracking power",                                 "MWe",   "", "", ""},
	{TCS_INPUT, TCS_NUMBER, I_W_DOT_REC_PUMP,     "W_dot_rec_pump",         "Receiver pumping power",                                   "MWe",   "", "", ""},
	{TCS_INPUT, TCS_NUMBER, I_M_DOT_HTF_SS,       "m_dot_htf_ss",           "HTF mass flow rate through PC HX at steady state - no derate for startup",                         "kg/s",  "", "", ""},
	{TCS_INPUT, TCS_NUMBER, I_W_DOT_PC_HYBRID,    "W_dot_pc_hybrid",        "Net PC power with solar",                                  "MWe",   "", "", ""},
	{TCS_INPUT, TCS_NUMBER, I_W_DOT_PC_FOSSIL,    "W_dot_pc_fossil",        "Net PC power at no-solar baseline",                      "MWe",   "", "", ""},
	{TCS_INPUT, TCS_NUMBER, I_F_TIMESTEP,         "f_timestep",             "Fraction of timestep that receiver is operational (not starting-up)",      "-",        "", "", ""},
	{TCS_INPUT, TCS_NUMBER, I_Q_SOLAR_SS,         "q_solar_ss",             "Solar thermal power at steady state - no derate for startup", "MWe", "", "", ""},     
	{TCS_INPUT, TCS_NUMBER, I_Q_DOT_FUEL,         "q_dot_fuel",             "Fuel thermal power into gas turbines",                      "kW",    "", "", ""},

	//OUTPUTS
	{TCS_OUTPUT, TCS_NUMBER, O_W_DOT_PC_HYBRID,   "W_dot_pc_hybrid",        "Net POWER CYCLE power output with solar",                  "MWe",      "", "", ""},
	{TCS_OUTPUT, TCS_NUMBER, O_W_DOT_PC_FOSSIL,   "W_dot_pc_fossil",        "Net POWER CYCLE power output at baseline",                 "MWe",      "", "", ""},
	{TCS_OUTPUT, TCS_NUMBER, O_W_DOT_PLANT_HYBRID,"W_dot_plant_hybrid",     "Net PLANT power output with solar",                        "MWe",      "", "", ""},
	{TCS_OUTPUT, TCS_NUMBER, O_W_DOT_PLANT_FOSSIL,"W_dot_plant_fossil",     "Net PLANT power output at baseline",                       "MWe",      "", "", ""},
	{TCS_OUTPUT, TCS_NUMBER, O_W_DOT_PLANT_SOLAR, "W_dot_plant_solar",      "Net PLANT power output attributable",                      "MWe",      "", "", ""},
	{TCS_OUTPUT, TCS_NUMBER, O_ETA_SOLAR_USE,     "eta_solar_use",          "Solar use efficiency considering parasitics",              "",         "", "", ""},
	{TCS_OUTPUT, TCS_NUMBER, O_ETA_FUEL,          "eta_fuel",               "Electrical efficiency of fossil only operation",           "%",        "", "", ""},
	{TCS_OUTPUT, TCS_NUMBER, O_SOLAR_FRACTION,    "solar_fraction",         "Solar contribution to total electrical power",             "-",        "", "", ""},
	{TCS_OUTPUT, TCS_NUMBER, O_P_PLANT_BALANCE_TOT, "P_plant_balance_tot",  "Total solar balance of plant parasitic power",             "MWe",      "", "", ""},
	//{TCS_OUTPUT, TCS_NUMBER, O_P_PIPING_TOT,      "P_piping_tot",           "Parasitic power estimated from piping thermal losses",     "MWe",      "", "", ""},
	{TCS_OUTPUT, TCS_NUMBER, O_P_FIXED,           "P_fixed",                "Total fixed parasitic losses",                             "MWe",      "", "", ""},

	//N_MAX
	{TCS_INVALID, TCS_INVALID, N_MAX,			0,					0, 0, 0, 0, 0	} } ;
	
	
class sam_iscc_parasitics : public tcstypeinterface
{
private:
	double W_htf_pc_pump;
	double Piping_loss;
	double Piping_length;
	double q_solar_design;
	double pb_fixed_par;
	double bop_par;
	double bop_par_f;
	double bop_par_0;
	double bop_par_1;
	double bop_par_2;
	double W_dot_fossil_des;
	double W_dot_solar_des;
	double W_dot_total_des;

public:
	sam_iscc_parasitics( tcscontext *cst, tcstypeinfo *ti)
		: tcstypeinterface( cst, ti)
	{

	}

	virtual ~sam_iscc_parasitics()
	{
		W_htf_pc_pump = std::numeric_limits<double>::quiet_NaN();
		Piping_loss = std::numeric_limits<double>::quiet_NaN();
		Piping_length = std::numeric_limits<double>::quiet_NaN();
		q_solar_design = std::numeric_limits<double>::quiet_NaN();
		pb_fixed_par = std::numeric_limits<double>::quiet_NaN();
		bop_par = std::numeric_limits<double>::quiet_NaN();
		bop_par_f = std::numeric_limits<double>::quiet_NaN();
		bop_par_0 = std::numeric_limits<double>::quiet_NaN();
		bop_par_1 = std::numeric_limits<double>::quiet_NaN();
		bop_par_2 = std::numeric_limits<double>::quiet_NaN();
		W_dot_fossil_des = std::numeric_limits<double>::quiet_NaN();
		W_dot_solar_des = std::numeric_limits<double>::quiet_NaN();
		W_dot_total_des = std::numeric_limits<double>::quiet_NaN();
	}

	virtual int init()
	{
		W_htf_pc_pump = value( P_W_HTF_PC_PUMP );				//[kJ/kg]
		//Piping_loss = value( P_PIPING_LOSS );					//[Wt/m]
		//Piping_length = value( P_PIPING_LENGTH );				//[m]
		q_solar_design = value( P_Q_SF_DES );					//[MWt]
		pb_fixed_par = value( P_PB_FIXED_PAR );					//[-]
		bop_par = value( P_BOP_PAR );							//[MWe/MWcap]
		bop_par_f = value( P_BOP_PAR_F );						//[-]
		bop_par_0 = value( P_BOP_PAR_0 );						//[-]
		bop_par_1 = value(P_BOP_PAR_1);							//[-]
		bop_par_2 = value(P_BOP_PAR_2);							//[-]
		W_dot_fossil_des = value( P_W_DOT_FOSSIL_DES );			//[MWe]
		W_dot_solar_des = value( P_W_DOT_SOLAR_DES );			//[MWe]
		W_dot_total_des = W_dot_fossil_des + W_dot_solar_des;	//[MWe]

		return 0;
	}

	virtual int call( double /*time*/, double /*step*/, int /*ncall*/ )
	{	
		double W_dot_tracking = value( I_W_DOT_TRACKING );		//[MWe] Solar field startup and tracking power
		double W_dot_rec_pump = value( I_W_DOT_REC_PUMP );		//[MWe] Power required to pump HTF through receiver
		double m_dot_htf = value( I_M_DOT_HTF_SS );				//[kg/hr] Steady-state mass flow rate through receiver and HX
		double W_dot_pc_hybrid = value( I_W_DOT_PC_HYBRID );	//[MWe] Steady state power cycle fossil+solar output
		double W_dot_pc_fossil = value( I_W_DOT_PC_FOSSIL );    //[MWe] Fossil-only power cycle output
		double f_timestep = value( I_F_TIMESTEP );				//[-] Fracion of timestep that receiver is operating (not starting up)
		double q_solar = value( I_Q_SOLAR_SS );					//[MWt] Steady-state receiver thermal power 
		double q_dot_fuel = value(I_Q_DOT_FUEL);				//[kWt] Fuel thermal power

		// HTF power cycle HX pumping power
		double W_dot_htf = W_htf_pc_pump*m_dot_htf/(3600.0)/1000.0;		//[kJ/kg]*[kg/hr]*[hr/s]*[MW/kW] = [MWe] HTF pumping power through power cycle heat exchanger
		//double W_htf = f_timestep*W_dot_htf*step/3600.0;				//[MW]*[s]*[hr/s] = [MWe-hr] HTF pumping energy considering timestep and fraction receiver is operational							

		// Electric-equivalent tower piping heat loss
		//double eta_cycle_base = 0.0;
		//double W_dot_piping_tot = 0.0;
		//if( q_solar > 0.0 )
		//{
		//	//eta_cycle_base = min(0.5, max(0.0, (W_dot_pc_hybrid - W_dot_pc_fossil) / (f_timestep*q_solar)));
		//	eta_cycle_base = abs(W_dot_pc_hybrid - W_dot_pc_fossil) / (q_solar);
		//	W_dot_piping_tot = Piping_loss * Piping_length * eta_cycle_base * (q_solar / q_solar_design)*1.E-6;	//[MWe] Electric equivalent loss from receiver piping heat loss
		//}
		//double W_piping_tot = f_timestep*W_dot_piping_tot*step / 3600.0;	//[MWe-hr] Energy considering timestep and fraction receiver operated

		// Balance of plant parasitics
		double P_ratio = (W_dot_pc_hybrid - W_dot_pc_fossil) / W_dot_solar_des;		//[-] Base on ratio of current and design solar contribution
		double W_dot_BOP = 0.0;
		if( P_ratio > 0.0 )
			W_dot_BOP = W_dot_solar_des * bop_par * bop_par_f * (bop_par_0 + bop_par_1*(P_ratio) + bop_par_2*pow(P_ratio, 2));
		//double W_BOP = f_timestep*W_dot_BOP*step/3600.0;		//[MWe-hr] Energy considering timestep and fraction receiver operated

		// Fixed plant parasitic
		double W_dot_fixed = pb_fixed_par * W_dot_total_des;		//[MWe]
		//double W_fixed = W_dot_fixed*step/3600.0;					//[MWe-hr] Energy considering timestep - parasitic is incurred regardless of receiver operting fraction
		
		// Calculate plant electrical power output for timestep
		double W_dot_plant_hybrid = f_timestep*W_dot_pc_hybrid + (1.0-f_timestep)*W_dot_pc_fossil
			                        - W_dot_rec_pump - W_dot_tracking - W_dot_fixed 
									- f_timestep*(W_dot_htf + /*W_dot_piping_tot +*/ W_dot_BOP);

		// Calculate plant fossil power output for timestep
		double W_dot_plant_fossil = W_dot_pc_fossil - W_dot_fixed;

		double eta_fuel = W_dot_plant_fossil * 1000 / q_dot_fuel * 100.0;	//[%] Electrical efficiency of fossil-only mode after fixed losses

		// Calculate solar use fraction with parasitics
		double eta_solar_use = 0.0;
		if( q_solar > 0.0 )
			eta_solar_use = max(0.0, (W_dot_plant_hybrid - W_dot_plant_fossil) / (f_timestep*q_solar) );		//[-] Solar use fraction with parasitics

		//double W_pc_hybrid = (f_timestep*W_dot_pc_hybrid + (1.0-f_timestep)*W_dot_pc_fossil)*step/3600.0;							//[MWe-hr]
		//double W_plant_hybrid = W_pc_hybrid - (W_dot_rec_pump+W_dot_tracking)*step/3600 - W_htf - W_piping_tot - W_BOP - W_fixed;	//[MWe-hr]
		//double W_plant_fossil = W_dot_pc_fossil*step/3600.0 - W_fixed;							//[MWe-hr]

		//double W_dot_plant_hybrid = W_dot_pc_hybrid - W_dot_rec_pump - W_dot_tracking - W_dot_htf - W_dot_piping_tot - W_dot_BOP - W_dot_fixed;	//[MWe] Net power output with solar
		//double W_dot_plant_fossil = W_dot_pc_fossil - W_dot_fixed;
		
		value(O_W_DOT_PC_HYBRID, f_timestep*W_dot_pc_hybrid + (1.0 - f_timestep)*W_dot_pc_fossil);
		value(O_W_DOT_PC_FOSSIL, W_dot_pc_fossil);
		value( O_W_DOT_PLANT_HYBRID, W_dot_plant_hybrid );			//[MWe]
		value( O_W_DOT_PLANT_FOSSIL, W_dot_plant_fossil );			//[MWe]
		value(O_W_DOT_PLANT_SOLAR, W_dot_plant_hybrid - W_dot_plant_fossil);
		value( O_ETA_SOLAR_USE, eta_solar_use );					//[MWe]
		value(O_ETA_FUEL, eta_fuel);					//[%]
		value(O_SOLAR_FRACTION, (W_dot_plant_hybrid - W_dot_plant_fossil) / W_dot_plant_hybrid);	//[-]

		value(O_P_PLANT_BALANCE_TOT, W_dot_BOP);
		//value(O_P_PIPING_TOT, W_dot_piping_tot);
		value(O_P_FIXED, W_dot_fixed);
		
		return 0;

	}

	virtual int converged( double /*time*/ )
	{

		return 0;
	}

};

TCS_IMPLEMENT_TYPE( sam_iscc_parasitics, "ISCC Powerblock ", "Ty Neises", 1, sam_iscc_parasitics_variables, NULL, 1 )

