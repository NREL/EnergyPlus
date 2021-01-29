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

#ifndef __C_PB_Type224__
#define __C_PB_Type224__

#include <string>
#include <cmath>
#include <math.h>
#include "htf_props.h"
//#include <shared/lib_util.h>
#include "lib_util.h"
//#include <shared/lib_physics.h>
#include "lib_physics.h"

struct S_Indirect_PB_Parameters
{
	S_Indirect_PB_Parameters()
	{
		P_ref = eta_ref = T_htf_hot_ref = T_htf_cold_ref = dT_cw_ref = T_amb_des = 0.0;
		CT = tech_type = n_pl_inc = 0;
		q_sby_frac = P_boil = startup_time = startup_frac = T_approach = T_ITD_des = 0.0;
		P_cond_ratio = pb_bd_frac = P_cond_min = 0.0;
		for (int i=0;i<9;i++)
			F_wc[i] = 0.0;
	}

	double P_ref;				// design electric power output (MW)
	double eta_ref;				// design conversion efficiency (%)
	double T_htf_hot_ref;		// design HTF inlet temperature (deg C)
	double T_htf_cold_ref;		// design HTF output temperature (deg C)
	double dT_cw_ref;			// design temp difference between cooling water inlet/outlet (C)
	double T_amb_des;			// design ambient temperature (C)
	// int HTF;					// integer flag identifying Heat Transfer Fluid (HTF) in power block {1-27}
	HTFProperties htfProps;		// class for HTF props
	double q_sby_frac;			// fraction of thermal power required for standby mode (%)
	double P_boil;				// boiler operating pressure (bar)
	int CT;						// integer flag for cooling technology type {1=evaporative cooling, 2=air cooling, 3=hybrid cooling}
	double startup_time;		// time needed for power block startup (hours)
	double startup_frac;		// fraction of design thermal power needed for startup (%)
	int tech_type;				// Flag indicating which coef. set to use. (1=tower,2=trough,3=user) 
	double T_approach;			// cooling tower approach temp (C)
	double T_ITD_des;			// design ITD for dry system (C)
	double P_cond_ratio;		// condenser pressure ratio
	double pb_bd_frac;			// blowdown steam fraction (%)
	double P_cond_min;			// minimum condenser pressure (inches Hg)
	int n_pl_inc;				// Number of part-load increments for the heat rejection system
	double F_wc[9];				// hybrid cooling dispatch fractions 1 thru 9 (array index 0-8)
};


struct S_Indirect_PB_Inputs
{
	S_Indirect_PB_Inputs()
	{
		mode = standby_control = TOU = 0;
		T_htf_hot = m_dot_htf = T_wb = demand_var = T_db = P_amb = rel_humidity = 0.0;
	}

	int mode;				// 1| mode                          | Cycle part load control, from plant controller						| none             | none
	double T_htf_hot;		// 2| T_htf_hot                     | Hot HTF inlet temperature, from storage tank							| C                | K
	double m_dot_htf;		// 3| m_dot_htf                     | HTF mass flow rate													| kg/hr            | kg/hr
	double T_wb;			// 4| T_wb                          | Ambient wet bulb temperature											| C                | C
	double demand_var;		// 5| demand_var                    | Control signal indicating operational mode							| none             | none
	int standby_control;	// 6| standby_control               | Control signal indicating standby mode (1=norm,2=standby,3=shutdown)	| none             | none
	double T_db;			// 7| T_db                          | Ambient dry bulb temperature											| C                | C
	double P_amb;			// 8| P_amb                         | Ambient pressure														| atm              | Pa
	int TOU;				// 9| TOU                           | Current Time-of-use period (0-8, for hybrid cooling only)				| none             | none
	double rel_humidity;	//10|								| Relative humidity of the ambient air								    | none             | none
};

struct S_Indirect_PB_Outputs
{
	S_Indirect_PB_Outputs()
	{
		P_cycle = eta = T_htf_cold = m_dot_makeup = m_dot_demand = m_dot_htf = m_dot_htf_ref = 0.0;
		W_cool_par = P_ref = f_hrsys = P_cond = 0.0;
	}

	double P_cycle;       //  1| P_cycle                          | Cycle power output                                                | MWe              | kWe
	double eta;           //  2| eta                              | Cycle thermal efficiency                                          | none             | none
	double T_htf_cold;    //  3| T_htf_cold                       | Heat transfer fluid outlet temperature                            | C                | C
	double m_dot_makeup;  //  4| m_dot_makeup                     | Cooling water makeup flow rate                                    | kg/hr            | kg/s
	double m_dot_demand;  //  5| m_dot_demand                     | HTF required flow rate to meet power load                         | kg/hr            | kg/hr
	double m_dot_htf;     //  6| m_dot_htf                        | Actual HTF flow rate passing through the power cycle              | kg/hr            | kg/hr
	double m_dot_htf_ref; //  7| m_dot_htf_ref                    | Calculated reference HTF flow rate at design                      | kg/hr            | kg/hr
	double W_cool_par;    //  8| W_cool_par                       | Cooling system parasitic load                                     | MWe              | MWe
	double P_ref;         //  9| P_ref                            | Reference power level output at design (mirror param)             | MWe              | kWe
	double f_hrsys;       // 10| f_hrsys                          | Fraction of operating heat rejection system                       | none             | none
	double P_cond;        // 11| P_cond                           | Condenser pressure                                                | Pa               | Pa
};

struct S_Indirect_PB_Stored	// these values are stored from timestep to timestep, only updated when the timestep changes
{
	S_Indirect_PB_Stored()
	{
		iLastStandbyControl = 0;
		dStartupTimeRemaining = dLastP_Cycle = dStartupEnergyRemaining = 0.0;
	}

	int iLastStandbyControl;
	double dStartupTimeRemaining;
	double dLastP_Cycle;
	double dStartupEnergyRemaining;
};

class C_Indirect_PB
{
public:
	C_Indirect_PB();
	~C_Indirect_PB();
	bool InitializeForParameters(const S_Indirect_PB_Parameters& pbp);
	bool Execute(const long lSecondsFromStart, const S_Indirect_PB_Inputs& pbi);
	double GetOutputMW(void) { return m_pbo.P_cycle; }
	double GetOutputkW(void) { return m_pbo.P_cycle*1000; }
	S_Indirect_PB_Outputs GetOutputs() const { return m_pbo; }
	std::string GetLastWarning() {return m_strWarningMsg;}
	std::string GetLastError() {return m_strLastError;}

private:
	long m_lCurrentSecondsFromStart;
	double m_dHoursSinceLastStep;

	int m_iLastStandbyControl;//=STORED(1)
	double m_dStartupRemain; //=STORED(2)
	double m_dLastPCycle; //=STORED(3)
	double m_dStartupERemain; //=STORED(4)

	double m_dStartupEnergy;
	double m_dDeltaEnthalpySteam;
	double m_F_wcMin;
	double m_F_wcMax;

	std::string m_strWarningMsg;
	std::string m_strLastError;
	S_Indirect_PB_Inputs m_pbi;
	S_Indirect_PB_Parameters m_pbp;
	S_Indirect_PB_Outputs m_pbo;
	S_Indirect_PB_Stored m_sv;

	util::matrix_t<double> m_db;
	bool m_bInitialized;
	bool m_bFirstCall;

	double eta_adj, T_hot_diff, eta_acfan_s, eta_acfan, C_air, drift_loss_frac, blowdown_frac, dP_evap, eta_pump, eta_pcw_s, eta_wcfan,
		  eta_wcfan_s, P_ratio_wcfan, mass_ratio_wcfan, Q_reject_des, q_ac_des, m_dot_acair_des, q_wc_des, c_cw, m_dot_cw_des;

	static inline double dmax1(double a, double b) {return (a > b) ? a : b; }
	static inline double dmin1(double a, double b) {return (a < b) ? a : b; }

	bool SetNewTime(const long lTimeInSeconds);
	void Step(const long lNewSecondsFromStart);

	double GetFieldToTurbineTemperatureDropC() { return 25.0; }

	// double f_dh_evap(double dPressurePa) { return 2.36230E+06 - (1.35459*dPressurePa) + (0.00000308492*dPressurePa*dPressurePa);} //Calculates enthalpy of evaporation (J/kg) given an atmospheric pressure (Pa)
	// double f_c_psat(double dPressurePa) { return 4170.46 + (0.000538088*dPressurePa) - (7.73437E-10*dPressurePa*dPressurePa); } // calculates the specific heat of water [J/kg-K] at the saturated liquid state as a function of pressure [Pa]
	// double f_psat_T(double T/*celcius*/) { return 1125.09 - (19.6444*T) + (4.42596*T*T) - (0.0391851*T*T*T) + (0.000965517*T*T*T*T); }
	// double f_hw_psat(double P) { return 229628.719 + (2.78471579*P) - (0.0000111907252*P*P) + (2.12030100E-11*P*P*P); } // Calculates the enthalpy of water [J/kg] at liquid saturation near ambient pressures [Pa]
	// double f_s_hw_psat(double P) { return 779.989872 + (0.00791597131*P) - (3.33033640E-08*P*P) + (6.38602593E-14*P*P*P); } // Calculates the entropy of water [J/kg-K] at liquid saturation near ambient pressures [Pa]
	// double f_rho_P(double P) { return 984.079732 - (0.000307058016*P) + (5.32272340E-10*P*P); } // Calculates density of water [kg/m3] at liquid saturation near ambient pressures [Pa]
	double f_h_air_T(double T) { return 273474.659 + (1002.9404*T) + (0.0326819988*T*T); } // Calculates enthalpy of air [J/kg] as a function of temperature [C]

	// The saturation temperture for pressures at the boiler
	// old equation: T_sat = 450.214 + 2.2691*P - 0.0123064*P**2 + 0.0000302829*P**3
	// double T_sat(double P/*Bar*/) { return 439.486188 + (2.88644991*P) - (0.0243308856*P*P) + (0.000125910226*P*P*P) - (2.66233987E-07*P*P*P*P); /*return value in Kelvin*/}
	
	// Added Aug 1, 2011 for Isopentane Rankine cycle
	// Valid for pressures 1.0 < P < 25 bar. Critical point of Isopentane is T=460.4[K] (187.3[C]), P=33.7[bar]
	// old equation { return 284.482349 + 0.000208848464*P - 1.58981470E-10*P*P + 6.55241456E-17*P*P*P - 1.01688219E-23*P*P*P*P;}
	double T_sat4(double P/*Bar*/) { return 284.482349 + 20.8848464*P - 1.5898147*P*P + 0.0655241456*P*P*P - 0.0010168822*P*P*P*P; /*return value in Kelvin*/}
       
    // old equation: P_sat = -1551.35 + 9.94705*T - 0.0218711*T**2 + 0.0000165747*T**3
	// double P_sat(double T/*Kelvin*/) { return 1965.19859 - (15.1689701*T) + (0.0452997046*T*T) - (0.000063150801*T*T*T) + (3.54340123E-08*T*T*T*T); }

	// Added Aug 3, 2011 for Isopentane Rankine cycle
	// Valid for temperature range from 300[K] to 440[K]. Critical point of Isopentane is T=460.4[K] (187.3[C]), P=33.7[bar]
	// double P_sat4(double T_celcius) {double T = physics::CelciusToKelvin(T_celcius); return (-99.7450105 + 1.02450484*T - 0.00360264243*T*T + 0.00000435512698*T*T*T)*1.e5; }

	// double f_Tsat_p(double P);
	double specheat(int fnum, double T, double P);

	void RankineCycle(/*double time,*/ double P_ref, double eta_ref, double T_htf_hot_ref, double T_htf_cold_ref, double T_db, double T_wb, 
				  double P_amb, double dT_cw_ref, /*double HTF,*/ double c_p_w, double T_htf_hot, double m_dot_htf, int /*double*/ mode, 
				  double demand_var, double P_boil, /*double tech_type,*/ double T_amb_des, double T_approach, double F_wc, double F_wcmin, 
				  double F_wcmax, double T_ITD_des, double P_cond_ratio, /*double CT,*/ double P_cond_min, /*double n_pl_inc,*/
				  /*double& fcall,*/ double& P_cycle, double& eta, double& T_htf_cold, double& m_dot_demand, double& m_dot_htf_ref, 
				  double& m_dot_makeup, double& W_cool_par, double& f_hrsys, double& P_cond);

	double Interpolate(int YT, int XT, double X);

};

#endif // __C_PB_Type224__
