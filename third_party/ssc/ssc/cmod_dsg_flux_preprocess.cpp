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
#include "water_properties.h"

static var_info _cm_vtab_dsg_flux_preprocess[] = {

	/*   VARTYPE   DATATYPE         NAME               LABEL                              UNITS     META  GROUP REQUIRED_IF CONSTRAINTS         UI_HINTS*/
	{ SSC_INPUT,  SSC_NUMBER,  "P_HP_in",         "HP Turbine inlet pressure",            "bar",    "",     "",    "*",        "",          "" },
	{ SSC_INPUT,  SSC_NUMBER,  "P_HP_out",        "HP Turbine outlet pressure",           "bar",    "",     "",    "*",        "",          "" },
	{ SSC_INPUT,  SSC_NUMBER,  "T_sh_out_ref",    "Superheater outlet temperature",       "C",      "",     "",    "*",        "",          "" },
	{ SSC_INPUT,  SSC_NUMBER,  "T_rh_out_ref",    "Reheater outlet temperature",          "C",      "",     "",    "*",        "",          "" },
	{ SSC_INPUT,  SSC_NUMBER,  "P_cycle_des",     "Cycle power output at design",         "MW",     "",     "",    "*",        "",          "" },
	{ SSC_INPUT,  SSC_NUMBER,  "eta_cycle_des",   "Cycle thermal efficiency at des.",     "",       "",     "",    "*",        "",          "" },
	{ SSC_INPUT,  SSC_NUMBER,  "rh_frac_ref",     "Mdot fraction to reheat at design",    "",       "",     "",    "*",        "",          "" },
																					      
	{ SSC_INPUT,  SSC_NUMBER,  "CT",              "Cooling type",                         "",       "",     "",    "*",        "",          "" },
	{ SSC_INPUT,  SSC_NUMBER,  "dT_cooling_ref",  "dT of cooling water",                  "C",      "",     "",    "*",        "",          "" },
	{ SSC_INPUT,  SSC_NUMBER,  "T_approach",      "dT cold cooling water - T_wb",         "C",      "",     "",    "*",        "",          "" },
	{ SSC_INPUT,  SSC_NUMBER,  "T_amb_des",       "Ambient (wb) temp at design",          "C",      "",     "",    "*",        "",          "" },
	{ SSC_INPUT,  SSC_NUMBER,  "T_ITD_des",       "T_cond - T_db",                        "C",      "",     "",    "*",        "",          "" },
																					      
	{ SSC_INPUT,  SSC_NUMBER,  "Q_rec_des",       "Receiver thermal power at des.",        "MW",     "",     "",    "*",        "",          "" },
	{ SSC_INPUT,  SSC_NUMBER,  "max_flux_b",      "Max allow. boiler flux",               "kW/m2",  "",     "",    "*",        "",          "" },
	{ SSC_INPUT,  SSC_NUMBER,  "max_flux_sh",     "Max allow. superheater flux",          "kW/m2",  "",     "",    "*",        "",          "" },  
	{ SSC_INPUT,  SSC_NUMBER,  "max_flux_rh",     "Max allow. reheater flux",             "kW/m2",  "",     "",    "*",        "",          "" },
	{ SSC_INPUT,  SSC_NUMBER,  "b_q_loss_flux",   "Boiler heat loss flux",                "kW/m2",  "",     "",    "*",        "",          "" },      
	{ SSC_INPUT,  SSC_NUMBER,  "sh_q_loss_flux",  "Superheater heat loss flux",	          "kW/m2",  "",     "",    "*",        "",          "" },
	{ SSC_INPUT,  SSC_NUMBER,  "rh_q_loss_flux",  "Reheater heat loss flux",              "kW/m2",  "",     "",    "*",        "",          "" },
																					      
	{ SSC_OUTPUT, SSC_NUMBER,  "max_flux",        "Maximum flux allow. on receiver",      "kW/m2",  "",     "",    "*",        "",          "" },
	{ SSC_OUTPUT, SSC_NUMBER,  "f_b",             "Fraction of total height to boiler",   "",	    "",     "",    "*",        "",          "" },
	{ SSC_OUTPUT, SSC_NUMBER,  "f_sh",            "Fraction of total height to SH",       "",	    "",     "",    "*",        "",          "" },
	{ SSC_OUTPUT, SSC_NUMBER,  "f_rh",            "Fraction of total height to RH",       "",       "",     "",    "*",        "",          "" },

	var_info_invalid };

class cm_dsg_flux_preprocess : public compute_module
{
public:

	cm_dsg_flux_preprocess()
	{
		add_var_info(_cm_vtab_dsg_flux_preprocess);
	}

	void exec( )
	{
		water_state wp;

		double P_HP_in = as_double("P_HP_in")*1.E2;					//[kPa] convert from bar
		double P_HP_out = as_double("P_HP_out")*1.E2;				//[kPa] convert from bar
		double T_sh_out_ref = as_double("T_sh_out_ref")+273.15;		//[K] convert from C
		double T_rh_out_ref = as_double("T_rh_out_ref")+273.15;		//[K] convert from C
		double P_cycle_des = as_double("P_cycle_des");				//[MW]
		double eta_cycle_des = as_double("eta_cycle_des");			//[-]
		double rh_frac_ref = as_double("rh_frac_ref");				//[-]		

		int ct = (int)as_double("CT");								//[-] Cooling type (1: evaporative, 2: air cooled, 3: Hybrid)
		double dT_cooling_ref = as_double("dT_cooling_ref");		//[C] 
		double T_approach = as_double("T_approach");				//[C]
		double T_amb_des = as_double("T_amb_des")+273.15;			//[C]
		double T_ITD_des = as_double("T_ITD_des");					//[C]

		double Q_rec_des = as_double("Q_rec_des");					//[MW]
		double max_flux_b = as_double("max_flux_b");                //[kW/m2]
		double max_flux_sh = as_double("max_flux_sh");				//[kW/m2]
		double max_flux_rh = as_double("max_flux_rh");				//[kW/m2]
		double b_q_loss_flux = as_double("b_q_loss_flux");			//[kW/m2]
		double sh_q_loss_flux = as_double("sh_q_loss_flux");		//[kW/m2]
		double rh_q_loss_flux = as_double("rh_q_loss_flux");		//[kW/m2]

		// HP turbine inlet conditions
		water_TP(T_sh_out_ref, P_HP_in, &wp);
		double h_HP_in_des = wp.enth;		//[kJ/kg] HP turbine inlet enthalpy
		double s_HP_in_des = wp.entr;		//[kJ/kg-K] HP turbine inlet entropy
	
		// HP turbine ISENTROPIC outlet conditions
		water_PS(P_HP_out, s_HP_in_des, &wp);
		double h_HP_out_isen = wp.enth;		//[kJ/kg] outlet isentropic enthalpy
		double h_HP_out_des = h_HP_in_des - (h_HP_in_des - h_HP_out_isen)*0.88;		//[kJ/kg] outlet enthalpy

		// HP turbine actual outlet conditions
		water_PH(P_HP_out, h_HP_out_des, &wp);
//		double T_rh_in_des = wp.temp;			//[K] Design reheat inlet temperature

		// Reheater outlet (LP turbine inlet)
		water_TP(T_rh_out_ref, P_HP_out, &wp);
		double h_rh_out_des = wp.enth;
		double s_rh_out_des = wp.entr;

		if( ct == 1 )
			water_TQ(dT_cooling_ref + 3.0 + T_approach + T_amb_des, 0.0, &wp);
		else
			water_TQ(T_ITD_des + T_amb_des, 0.0, &wp);

		double Psat_des = wp.pres;				//[kPa]
		// LP turbine Isentropic outlet conditions
		water_PS(Psat_des, s_rh_out_des, &wp);
		double h_LP_out_isen = wp.enth;			//[kJ/kg]
		
		// LP turbine outlet conditions
		double h_LP_out_des = h_rh_out_des - (h_rh_out_des - h_LP_out_isen)*0.88;	//[kJ/kg] Actual design low pressure outlet enthalpy
		
		// Boiler outlet / superheater inlet
		water_PQ(P_HP_in, 1.0, &wp);
		double h_sh_in_des = wp.enth;			//[kJ/kg]

		// Calculate design mass flow rate based on design setpoints
			// Equation: P_cycle_design = m_dot*(h_HP_in_des - h_HP_out_des) + m_dot*f_mdotrh_des*(h_rh_out_des - h_LP_out_des)
		double m_dot_des = (P_cycle_des*1.E3)/( (h_HP_in_des-h_HP_out_des) + rh_frac_ref*(h_rh_out_des - h_LP_out_des));	// [kW/(kJ/kg)] = kg/s

		// Now, find feedwater outlet enthalpy using design cycle efficiency
		double q_sh_des = (h_HP_in_des - h_sh_in_des)*m_dot_des;					//[kW] Design rate of energy input to superheater
		double q_rh_des = (h_rh_out_des - h_HP_out_des)*m_dot_des*rh_frac_ref;		//[kW] Design rate of energy input to reheater

		double Q_pb_des = (P_cycle_des*1.E3) / eta_cycle_des;		//[kW] Thermal power input to cycle at power cycle design
			//Equation: Q_pb_design = q_sh_des + q_rh_des + q_b_des
		double q_b_des = Q_pb_des - q_sh_des - q_rh_des;			//[kW] Thermal power input to boiler at design

			//Equation: q_b_des = (h_sh_in_des - h_fw_out_des)*m_dot_des
		double h_fw_out_des = h_sh_in_des - q_b_des / m_dot_des;	//[kJ/kg] Design feedwater outlet enthalpy

		// Feedwater outlet state point: is the temperature required info?
		// water_PH(P_HP_in, h_fw_out_des, &wp);
		// double T_fw = wp.temp;						//[K]

		// Specific heat rate of receivers
		double q_sh_des_sp = (h_HP_in_des - h_sh_in_des);	//[kJ/kg]
		double q_rh_des_sp = (h_rh_out_des - h_HP_out_des);	//[kJ/kg]
		double q_b_des_sp = (h_sh_in_des - h_fw_out_des);	//[kJ/kg]

		//*******************************************************************************
		//*** Next, find mass flow rate at TOWER design heat rate: (Q_pb_design x SM) ***
		//*******************************************************************************
		double m_dot_ref = (Q_rec_des*1.E3)/(q_b_des_sp + q_sh_des_sp + q_rh_des_sp*rh_frac_ref);	//[kW/(kJ/kg)] = kg/s Mass flow rate at tower design heat rate

		double Q_b_ref = q_b_des_sp * m_dot_ref;			//[kW] Design heat rate to boiler
		double Q_sh_ref = q_sh_des_sp * m_dot_ref;			//[kW] Design heat rate to superheater
		double Q_rh_ref = q_rh_des_sp * rh_frac_ref * m_dot_ref;	//[kW] Design heat rate to reheater

		// Calculate minimum allowable area for each receiver
		double A_b_min = Q_b_ref / (max_flux_b - b_q_loss_flux);		//[m^2] Minimum allowable boiler area
		double A_sh_min = Q_sh_ref / (max_flux_sh - sh_q_loss_flux);	//[m^2] Minimum allowable SH area
		double A_rh_min = Q_rh_ref / (max_flux_rh - rh_q_loss_flux);	//[m^2] Minimum allowable RH area
		double A_min = A_b_min + A_sh_min + A_rh_min;					//[m^2] Minimum allowable total receiver area

		// So the fraction of the total receiver height attributed to each receiver is equal to the receiver's area ratio (because receivers have equal width)
		double f_b = A_b_min / A_min;
		double f_sh = A_sh_min / A_min;
		double f_rh = A_rh_min / A_min;

		// Calculate design incident energy on the receiver: need to account for heat loss
		double Q_rec_inc = (Q_b_ref + A_b_min*b_q_loss_flux) + (Q_sh_ref + A_sh_min*sh_q_loss_flux) + (Q_rh_ref + A_rh_min*rh_q_loss_flux);

		// Then the maximum flux is a function of the tower's design incident energy and the minimum area
		double max_flux = Q_rec_inc / A_min;			//[kW/m^2] Maximum flux for entire receiver

		// Return outputs
		assign("max_flux", (ssc_number_t)max_flux);
		assign("f_b", (ssc_number_t)f_b);
		assign("f_sh", (ssc_number_t)f_sh);
		assign("f_rh", (ssc_number_t)f_rh);
	}


};

DEFINE_MODULE_ENTRY( dsg_flux_preprocess, "Calculate receiver max flux and absorber (boiler, etc.) fractions", 0)
