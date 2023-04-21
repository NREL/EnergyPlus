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
		P_piping_loss,   
		P_PIPE_LENGTH_ADD,
		P_PIPE_LENGTH_MULT,
		P_THT,
		P_design_power,  
		P_design_eff,    
		P_pb_fixed_par,  
		P_aux_par,       
		P_aux_par_f,     
		P_aux_par_0,     
		P_aux_par_1,     
		P_aux_par_2,     
		P_bop_par,       
		P_bop_par_f,     
		P_bop_par_0,     
		P_bop_par_1,     
		P_bop_par_2,     

		//Inputs
		I_P_cooling_tower,  
		I_P_tower_pump,     
		I_P_helio_track,    
		I_P_plant_output,   
		I_eta_cycle,        
		I_P_cold_tank,      
		I_P_hot_tank,                  
		I_aux_power,     
		I_P_htf_pump,

		//Outputs
		O_P_plant_balance_tot,
		O_P_cooling_tower_tot,
		O_P_piping_tot,       
		O_P_parasitics,       
		O_P_out_net,          
		O_P_tank_heater,            
		O_P_fixed,            
		O_P_aux,              

		//N_MAX
		N_MAX};

tcsvarinfo sam_mw_pt_type228_variables[] = {
	//PARAMETERS
	{TCS_PARAM,   TCS_NUMBER,   P_piping_loss,      "Piping_loss",         "Thermal loss per meter of piping",                               "Wt/m",      "", "", ""},
	{TCS_PARAM,   TCS_NUMBER,   P_PIPE_LENGTH_ADD,  "piping_length_add",   "Value added to product of tower height*piping length multiple",	 "m"		  "", "", ""},
	{TCS_PARAM,   TCS_NUMBER,   P_PIPE_LENGTH_MULT, "piping_length_mult",  "Value multiplied to tower height",								 "-"		  "", "", ""},
	{TCS_PARAM,   TCS_NUMBER,   P_THT,				"THT",				   "The height of the tower (hel. pivot to rec equator)",			 "m",		  "", "", ""},
	{TCS_PARAM,   TCS_NUMBER,   P_design_power,     "Design_power",        "Power production at design conditions",                          "MWe",       "", "", ""},
	{TCS_PARAM,   TCS_NUMBER,   P_design_eff,       "design_eff",          "Power cycle efficiency at design",                               "none",      "", "", ""},
	{TCS_PARAM,   TCS_NUMBER,   P_pb_fixed_par,     "pb_fixed_par",        "Fixed parasitic load - runs at all times",                       "MWe/MWcap", "", "", ""},
	{TCS_PARAM,   TCS_NUMBER,   P_aux_par,          "aux_par",             "Aux heater, boiler parasitic",                                   "MWe/MWcap", "", "", ""},
	{TCS_PARAM,   TCS_NUMBER,   P_aux_par_f,        "aux_par_f",           "Aux heater, boiler parasitic - multiplying fraction",            "none",      "", "", ""},
	{TCS_PARAM,   TCS_NUMBER,   P_aux_par_0,        "aux_par_0",           "Aux heater, boiler parasitic - constant coefficient",            "none",      "", "", ""},
	{TCS_PARAM,   TCS_NUMBER,   P_aux_par_1,        "aux_par_1",           "Aux heater, boiler parasitic - linear coefficient",              "none",      "", "", ""},
	{TCS_PARAM,   TCS_NUMBER,   P_aux_par_2,        "aux_par_2",           "Aux heater, boiler parasitic - quadratic coefficient",           "none",      "", "", ""},
	{TCS_PARAM,   TCS_NUMBER,   P_bop_par,          "bop_par",             "Balance of plant parasitic power fraction",                      "MWe/MWcap", "", "", ""},
	{TCS_PARAM,   TCS_NUMBER,   P_bop_par_f,        "bop_par_f",           "Balance of plant parasitic power fraction - mult frac",          "none",      "", "", ""},
	{TCS_PARAM,   TCS_NUMBER,   P_bop_par_0,        "bop_par_0",           "Balance of plant parasitic power fraction - const coeff",        "none",      "", "", ""},
	{TCS_PARAM,   TCS_NUMBER,   P_bop_par_1,        "bop_par_1",           "Balance of plant parasitic power fraction - linear coeff",       "none",      "", "", ""},
	{TCS_PARAM,   TCS_NUMBER,   P_bop_par_2,        "bop_par_2",           "Balance of plant parasitic power fraction - quadratic coeff",    "none",      "", "", ""},

	//INPUTS
	{TCS_INPUT,   TCS_NUMBER,   I_P_cooling_tower,   "P_cooling_tower",    "Cooling tower parasitic power fraction",                         "MWe",       "", "", ""},
	{TCS_INPUT,   TCS_NUMBER,   I_P_tower_pump,      "P_tower_pump",       "Reported tower pump power",                                      "MWe",       "", "", ""},
	{TCS_INPUT,   TCS_NUMBER,   I_P_helio_track,     "P_helio_track",      "Reported heliostat tracking power",                              "MWe",       "", "", ""},
	{TCS_INPUT,   TCS_NUMBER,   I_P_plant_output,    "P_plant_output",     "Reported plant power output",                                    "MWe",       "", "", ""},
	{TCS_INPUT,   TCS_NUMBER,   I_eta_cycle,         "eta_cycle",          "Power cycle efficiency",                                         "none",      "", "", ""},
	{TCS_INPUT,   TCS_NUMBER,   I_P_cold_tank,       "P_cold_tank",        "Cold tank heater parasitic power",                               "MWe",       "", "", "0.0"},
	{TCS_INPUT,   TCS_NUMBER,   I_P_hot_tank,        "P_hot_tank",         "Hot tank heater parasitic power",                                "MWe",       "", "", "0.0"},
	{TCS_INPUT,   TCS_NUMBER,   I_aux_power,         "aux_power",          "Auxiliary heater thermal power output",                          "MWt",       "", "", ""},
	{TCS_INPUT,   TCS_NUMBER,   I_P_htf_pump,        "P_htf_pump",         "HTF pumping power",                                              "MWe",       "", "", ""},

	//OUTPUTS
	{TCS_OUTPUT,  TCS_NUMBER,   O_P_plant_balance_tot,  "P_plant_balance_tot",  "Total balance of plant parasitic power",                    "MWe",       "", "", ""},
	{TCS_OUTPUT,  TCS_NUMBER,   O_P_cooling_tower_tot,  "P_cooling_tower_tot",  "Total cooling tower parasitic power",                       "MWe",       "", "", ""},
	{TCS_OUTPUT,  TCS_NUMBER,   O_P_piping_tot,         "P_piping_tot",         "Total piping loss parasitic power",                         "MWe",       "", "", ""},
	{TCS_OUTPUT,  TCS_NUMBER,   O_P_parasitics,         "P_parasitics",         "Overall parasitic losses",                                  "MWe",       "", "", ""},
	{TCS_OUTPUT,  TCS_NUMBER,   O_P_out_net,            "P_out_net",            "Power to the grid after parasitic losses",                  "MWe",       "", "", ""},
	{TCS_OUTPUT,  TCS_NUMBER,   O_P_tank_heater,        "P_tank_heater",        "Total tank heater parasitic power",                         "MWe",       "", "", ""},
	{TCS_OUTPUT,  TCS_NUMBER,   O_P_fixed,              "P_fixed",              "Total fixed parasitic loss",                                "MWe",       "", "", ""},
	{TCS_OUTPUT,  TCS_NUMBER,   O_P_aux,                "P_aux",                "Total auxiliary heater parasitic loss",                     "MWe",       "", "", ""},

	//N_MAX
	{TCS_INVALID, TCS_INVALID, N_MAX,			0,					0, 0, 0, 0, 0	} } ;
	
	
class sam_mw_pt_type228 : public tcstypeinterface
{
private:

	double m_Q_dot_piping_loss;
	double Design_power;
	double design_eff;
	double pb_fixed_par;
	double aux_par;
	double aux_par_f;
	double aux_par_0;
	double aux_par_1;
	double aux_par_2;
	double bop_par;
	double bop_par_f;
	double bop_par_0;
	double bop_par_1;
	double bop_par_2;

public:
	sam_mw_pt_type228( tcscontext *cst, tcstypeinfo *ti)
		: tcstypeinterface( cst, ti)
	{

		m_Q_dot_piping_loss = std::numeric_limits<double>::quiet_NaN();
		Design_power = std::numeric_limits<double>::quiet_NaN();
		design_eff = std::numeric_limits<double>::quiet_NaN();
		pb_fixed_par = std::numeric_limits<double>::quiet_NaN();
		aux_par = std::numeric_limits<double>::quiet_NaN();
		aux_par_f = std::numeric_limits<double>::quiet_NaN();
		aux_par_0 = std::numeric_limits<double>::quiet_NaN();
		aux_par_1 = std::numeric_limits<double>::quiet_NaN();
		aux_par_2 = std::numeric_limits<double>::quiet_NaN();
		bop_par = std::numeric_limits<double>::quiet_NaN();
		bop_par_f = std::numeric_limits<double>::quiet_NaN();
		bop_par_0 = std::numeric_limits<double>::quiet_NaN();
		bop_par_1 = std::numeric_limits<double>::quiet_NaN();
		bop_par_2 = std::numeric_limits<double>::quiet_NaN();
	 
	}

	virtual ~sam_mw_pt_type228()
	{

	}

	virtual int init()
	{
		double pipe_loss_per_m = value( P_piping_loss )/1.E6;		//[MWt/m] convert from Wt/m
		double h_tower = value(P_THT);								//[m] Tower height
		double pipe_length_mult = value(P_PIPE_LENGTH_MULT);		//[-]
		double pipe_length_add = value(P_PIPE_LENGTH_ADD);			//[m]

		m_Q_dot_piping_loss = pipe_loss_per_m*(h_tower*pipe_length_mult + pipe_length_add);	//[MWt]

		Design_power = value( P_design_power );			//[MWe]
		design_eff = value( P_design_eff );				//[-]
		pb_fixed_par = value( P_pb_fixed_par );			//[MWe/MWcap]
		aux_par = value( P_aux_par );					//[MWe/MWcap]
		aux_par_f = value( P_aux_par_f );				//[-]
		aux_par_0 = value( P_aux_par_0 );				//[-]
		aux_par_1 = value( P_aux_par_1 );				//[-]
		aux_par_2 = value( P_aux_par_2 );     			//[-]
		bop_par = value( P_bop_par );					//[MWe/MWcap]
		bop_par_f = value( P_bop_par_f );				//[-]
		bop_par_0 = value( P_bop_par_0 ); 				//[-]
		bop_par_1 = value( P_bop_par_1 );     			//[-]
		bop_par_2 = value( P_bop_par_2 ); 				//[-]

		return 0;
	}

	virtual int call( double /*time*/, double /*step*/, int /*ncall*/ )
	{						
		double P_cooling_tower = value( I_P_cooling_tower );	//[MWe] Cooling parasitics from power cycle model
		double P_tower_pump = value( I_P_tower_pump );			//[MWe] Power required to pump HTF through the tower
		double P_helio_track = value( I_P_helio_track );		//[MWe] Power required to startup/stow/track heliostats
		double P_plant_output = value( I_P_plant_output );		//[MWe] Electric output from power cycle model (not including cooling parasitics)
		double eta_cycle = value( I_eta_cycle );				//[-] Power cycle thermal efficiency considering cycle generation (defined above) and thermal input
		double P_cold_tank = value( I_P_cold_tank );			//[MWe] Power required to keep cold tank at its minimum temperature
		double P_hot_tank = value( I_P_hot_tank );				//[MWe] Power required to keep hot tank at its minimum temperature
		double aux_power = value( I_aux_power );				//[MWt] Aux power used during timestep
		double P_htf_pump = value( I_P_htf_pump );				//[MWe] Power required to pump HTF through PC AND TES (but no TES storage side pumping)

		double P_ratio = P_plant_output/Design_power;
		double aux_ratio = aux_power/Design_power/design_eff;

		//double P_storage_pump_tot;
		//if( storage_bypass )
		//	P_storage_pump_tot = P_storage_pump*flow_from_storage/max( ref_htf_flow, 1.E-6 )*Design_power/design_eff;	//Hot pump operates only when storage is dispatched
		//else
		//	P_storage_pump_tot = P_storage_pump*P_plant_output/design_eff;	//Hot pump operates when any hot HTF is sent to the power block

		double P_fixed = pb_fixed_par * Design_power;		//[MWe]

		double P_plant_balance_tot;
		if( P_plant_output > 0.0 )
			P_plant_balance_tot = Design_power * bop_par * bop_par_f * (bop_par_0 + bop_par_1*(P_ratio) + bop_par_2*pow(P_ratio,2));
		else
			P_plant_balance_tot = 0.0;

		double P_aux;
		if( aux_ratio > 0.0 )
			P_aux = Design_power * aux_par * aux_par_f * (aux_par_0 + aux_par_1*aux_ratio + aux_par_2*pow(aux_ratio,2));	
		else
			P_aux = 0.0;

		double P_cooling_tower_tot = P_cooling_tower;		//[MWe]

				
		//[MW] = piping loss thermal * conversion to electric * current timestep scaling
		double P_piping_tot = m_Q_dot_piping_loss * eta_cycle * P_plant_output / Design_power;	//MWe


		double P_tank_heater = (P_cold_tank + P_hot_tank);		//MWe

		//double P_tower_par;
		//if( recirc_source == 2 && night_recirc )
		//	P_tower_par = (P_tower_conv + P_tower_rad)/recirc_htr_eff;
		//else
		//	P_tower_par = 0.0;

		// 7.8.13, twn: Add htf pumping power to parasitic calcs: MWe
		double P_parasitics = P_plant_balance_tot + P_cooling_tower_tot + P_fixed + P_tower_pump + P_helio_track + P_piping_tot + P_tank_heater + P_aux + P_htf_pump;

		value( O_P_plant_balance_tot, P_plant_balance_tot );
		value( O_P_cooling_tower_tot, P_cooling_tower_tot );
		value( O_P_piping_tot, P_piping_tot );
		value( O_P_parasitics, P_parasitics );
		value( O_P_out_net, P_plant_output - P_parasitics );
		value( O_P_tank_heater, P_tank_heater );
		value( O_P_fixed, P_fixed );
		value( O_P_aux, P_aux );

		//value( O_pparasi, pparasi/3.6e6 );	// [MW], convert from kJ/hr: Parasitic power for tracking
		//value( O_eta_field, eta_field );	// [-], field efficiency


		return 0;
	}

	virtual int converged( double /*time*/ )
	{
		
		return 0;
	}

};

TCS_IMPLEMENT_TYPE( sam_mw_pt_type228, "Power Tower Parasitics", "Ty Neises", 1, sam_mw_pt_type228_variables, NULL, 1 )