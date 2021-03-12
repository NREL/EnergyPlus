/* ARD This is a new class for radiative cooling panel model. */

#ifndef __csp_solver_radiator_
#define __csp_solver_radiator_

#include "csp_solver_core.h"
#include "csp_solver_util.h"

#include "sam_csp_util.h"
#include "water_properties.h"

class C_csp_radiator
{
private:

	water_state mc_coldhtf;
	HTFProperties mc_air;


public:

	double T_S_measured[8760] = {};	//measured sky temperature [K], initially zeros.
	int T_S_localhr[8760] = {};	//local time in hours for measured sky temp, initially zeros.
	double T_S_time[8760] = {};		//time in seconds at end of timestep for measured sky temp, initially zeros.
	/*int T_EG30[1][71] = { -10, -9,-8,-7,-6,-5,-4,-3,-2,-1,0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60 };	// Temperatures [C] for EG properties
	double cp_EG30[1][71] = { 3.627,3.63,3.633,3.636,3.64,3.643,3.646,3.649,3.652,3.655,3.658,3.661,3.664,3.667,3.67,3.673,3.676,3.679,3.682,3.685,3.689,3.692,3.695,3.698,3.7,3.703,3.706,3.709,3.712,3.715,3.718,3.721,3.724,3.727,3.73,3.733,3.736,3.739,3.741,3.744,3.747,3.75,3.753,3.756,3.759,3.761,3.764,3.767,3.77,3.773,3.775,3.778,3.781,3.784,3.786,3.789,3.792,3.794,3.797,3.8,3.803,3.805,3.808,3.811,3.813,3.816,3.818,3.821,3.824,3.826,3.829 };		//Specific heat [kJ/kg-K] of ethyene glycol 30% by mass from -10 C to 60 C
	double rho_EG30[1][71] = { 1047,1047,1047,1047,1047,1046,1046,1046,1046,1045,1045,1045,1044,1044,1044,1043,1043,1043,1042,1042,1042,1041,1041,1041,1040,1040,1040,1039,1039,1038,1038,1038,1037,1037,1036,1036,1036,1035,1035,1034,1034,1033,1033,1032,1032,1031,1031,1030,1030,1029,1029,1028,1028,1027,1027,1026,1026,1025,1025,1024,1023,1023,1022,1022,1021,1020,1020,1019,1019,1018,1017 };	//Density [kg/m^3]
	double mu_EG30[1][71] = { 0.006508,0.006228,0.005964,0.005715,0.005478,0.005254,0.005042,0.004841,0.00465,0.004469,0.004298,0.004135,0.00398,0.003832,0.003692,0.003559,0.003433,0.003312,0.003197,0.003087,0.002983,0.002883,0.002788,0.002698,0.002611,0.002529,0.002449,0.002374,0.002302,0.002233,0.002166,0.002103,0.002042,0.001984,0.001929,0.001875,0.001824,0.001775,0.001728,0.001682,0.001639,0.001597,0.001557,0.001518,0.001481,0.001445,0.001411,0.001378,0.001346,0.001315,0.001286,0.001257,0.001229,0.001203,0.001177,0.001153,0.001129,0.001106,0.001083,0.001062,0.001041,0.001021,0.001001,0.0009824,0.0009642,0.0009465,0.0009294,0.0009128,0.0008967,0.0008812,0.000866 };		//Viscosity [kg/m-sec]
	double alpha_EG30[1][71] = { 1.15E-07,1.15E-07,1.15E-07,1.15E-07,1.16E-07,1.16E-07,1.16E-07,1.16E-07,1.16E-07,1.17E-07,1.17E-07,1.17E-07,1.17E-07,1.17E-07,1.17E-07,1.18E-07,1.18E-07,1.18E-07,1.18E-07,1.18E-07,1.19E-07,1.19E-07,1.19E-07,1.19E-07,1.19E-07,1.20E-07,1.20E-07,1.20E-07,1.20E-07,1.20E-07,1.20E-07,1.21E-07,1.21E-07,1.21E-07,1.21E-07,1.21E-07,1.22E-07,1.22E-07,1.22E-07,1.22E-07,1.22E-07,1.23E-07,1.23E-07,1.23E-07,1.23E-07,1.23E-07,1.24E-07,1.24E-07,1.24E-07,1.24E-07,1.24E-07,1.25E-07,1.25E-07,1.25E-07,1.25E-07,1.25E-07,1.26E-07,1.26E-07,1.26E-07,1.26E-07,1.26E-07,1.27E-07,1.27E-07,1.27E-07,1.27E-07,1.27E-07,1.28E-07,1.28E-07,1.28E-07,1.28E-07,1.28E-07 };	//Thermal diffusivity [m^2/sec]
	double k_EG30[1][71] = {0.4362, 0.4371, 0.4381, 0.4391, 0.4401, 0.4411, 0.442, 0.443, 0.444, 0.445, 0.4459, 0.4469, 0.4479, 0.4488, 0.4498, 0.4507, 0.4517, 0.4527, 0.4536, 0.4546, 0.4555, 0.4565, 0.4574, 0.4583, 0.4593, 0.4602, 0.4612, 0.4621, 0.463, 0.464, 0.4649, 0.4658, 0.4668, 0.4677, 0.4686, 0.4695, 0.4704, 0.4713, 0.4723, 0.4732, 0.4741, 0.475, 0.4759, 0.4768, 0.4777, 0.4786, 0.4795, 0.4804, 0.4813, 0.4821, 0.483, 0.4839, 0.4848, 0.4857, 0.4865, 0.4874, 0.4883, 0.4891, 0.49, 0.4909, 0.4917, 0.4926, 0.4934, 0.4943, 0.4951, 0.496, 0.4968, 0.4977, 0.4985, 0.4994, 0.5002}; //Thermal conductivity [W/m-K]
	*/

	int T_PG20[68] = { -7,-6,-5,-4,-3,-2,-1,0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60 };	// Temperatures [C] for PG properties
	double cp_PG20[68] = { 3.922,3.924,3.926,3.928,3.93,3.932,3.934,3.936,3.938,3.94,3.942,3.944,3.946,3.948,3.95,3.952,3.954,3.956,3.958,3.96,3.962,3.964,3.966,3.968,3.971,3.973,3.975,3.977,3.979,3.981,3.983,3.985,3.987,3.989,3.991,3.994,3.996,3.998,4,4.002,4.004,4.006,4.008,4.011,4.013,4.015,4.017,4.019,4.021,4.023,4.025,4.028,4.03,4.032,4.034,4.036,4.038,4.04,4.042,4.044,4.047,4.049,4.051,4.053,4.055,4.057,4.059,4.061 };		//Specific heat [kJ/kg-K] of propylene glycol 20% by mass from -7 C to 60 C
	double rho_PG20[68] = { 1021,1021,1021,1021,1021,1020,1020,1020,1020,1020,1020,1019,1019,1019,1019,1018,1018,1018,1018,1017,1017,1017,1016,1016,1016,1015,1015,1015,1014,1014,1014,1013,1013,1013,1012,1012,1011,1011,1010,1010,1010,1009,1009,1008,1008,1007,1007,1006,1006,1005,1005,1004,1004,1003,1003,1002,1002,1001,1001,1000,999.6,999,998.5,997.9,997.3,996.8,996.2,995.6 };	//Density [kg/m^3]
	double mu_PG20[68] = { 0.005951,0.005672,0.00541,0.005163,0.004931,0.004712,0.004506,0.004312,0.004129,0.003957,0.003794,0.00364,0.003494,0.003356,0.003226,0.003103,0.002986,0.002875,0.00277,0.00267,0.002575,0.002485,0.002399,0.002318,0.002241,0.002167,0.002097,0.00203,0.001966,0.001906,0.001848,0.001792,0.001739,0.001689,0.001641,0.001594,0.00155,0.001508,0.001467,0.001428,0.001391,0.001355,0.001321,0.001288,0.001257,0.001226,0.001197,0.001169,0.001142,0.001116,0.001091,0.001067,0.001044,0.001022,0.001,0.0009795,0.0009594,0.00094,0.0009213,0.0009032,0.0008857,0.0008688,0.0008524,0.0008365,0.0008211,0.0008062,0.0007918,0.0007778 };		//Viscosity [kg/m-sec]
	double alpha_PG20[68] = { 1.16E-07,1.16E-07,1.16E-07,1.16E-07,1.17E-07,1.17E-07,1.17E-07,1.17E-07,1.18E-07,1.18E-07,1.18E-07,1.18E-07,1.19E-07,1.19E-07,1.19E-07,1.19E-07,1.19E-07,1.20E-07,1.20E-07,1.20E-07,1.20E-07,1.21E-07,1.21E-07,1.21E-07,1.21E-07,1.22E-07,1.22E-07,1.22E-07,1.22E-07,1.22E-07,1.23E-07,1.23E-07,1.23E-07,1.23E-07,1.24E-07,1.24E-07,1.24E-07,1.24E-07,1.25E-07,1.25E-07,1.25E-07,1.25E-07,1.26E-07,1.26E-07,1.26E-07,1.26E-07,1.26E-07,1.27E-07,1.27E-07,1.27E-07,1.27E-07,1.28E-07,1.28E-07,1.28E-07,1.28E-07,1.29E-07,1.29E-07,1.29E-07,1.29E-07,1.30E-07,1.30E-07,1.30E-07,1.30E-07,1.30E-07,1.31E-07,1.31E-07,1.31E-07,1.31E-07 };	//Thermal diffusivity [m^2/sec]
	double k_PG20[68] = { 0.4636,0.4646,0.4657,0.4668,0.4679,0.469,0.47,0.4711,0.4722,0.4733,0.4743,0.4754,0.4765,0.4775,0.4786,0.4797,0.4807,0.4818,0.4828,0.4839,0.4849,0.486,0.487,0.4881,0.4891,0.4901,0.4912,0.4922,0.4932,0.4943,0.4953,0.4963,0.4973,0.4984,0.4994,0.5004,0.5014,0.5024,0.5034,0.5044,0.5054,0.5064,0.5074,0.5084,0.5093,0.5103,0.5113,0.5123,0.5132,0.5142,0.5152,0.5161,0.5171,0.518,0.519,0.5199,0.5209,0.5218,0.5228,0.5237,0.5246,0.5255,0.5265,0.5274,0.5283,0.5292,0.5301,0.531 }; //Thermal conductivity [W/m-K]

	struct S_params
	{
		int m_field_fl;
		util::matrix_t<double> m_field_fl_props;
		double m_dot_panel;		//Total mass flow rate through panel : m_dot[kg / sec]
		int n;					//Number of parallel tubes on a single panel : n
		double W;				//Distance between two parallel tubes : W[m]
		double L;				//Length of tubes : L[m]
		double L_c;				//Characteristic length for forced convection, typically equal to n*W
			//unless wind direction is known to determine flow path : Lc[m]
		double th;				//Thickness of plate : th[m]
		double D;				//Diameter of tube : D[m]
		double k_panel;			//Conductivity of plate : k[W / m - K]
		double epsilon;			//Emissivity of plate top surface : epsilon[-]
		double epsilonb;		//Emissivity of plate bottom surface : epsilonb[-]
		double epsilong;		//Emissivity of ground : epsilong[-]
		double Lsec;			//Length of series - connected sections of panels(if single panel, set equal to L) : Lsec[m]
		double m_night_hrs;		//Number of hours plant will run at summer peak
		double m_power_hrs;		//Number of hours plant operates in one day at summer peak
		double Afield, RM, Asolar_refl;
		int Np;					//Number of radiator panels in parallel
		double epsilon_HX;		//Effectiveness of the heat exchanger between cold storage and radiative field
		double radfield_dp;		//Pressure drop through panel and distribution in radiative field [kPa]
		S_params()
		{
			n = Np=Afield= radfield_dp=0;
			m_dot_panel = W = L = L_c = th = D = k_panel = epsilon = epsilonb = epsilong = Lsec = m_night_hrs = RM = epsilon_HX = Asolar_refl = std::numeric_limits<double>::quiet_NaN();
		}
	};

	S_params ms_params;

	C_csp_radiator();

	void init();

	void night_cool(double T_db /*K*/, double T_rad_in /*K*/, double u /*m/s*/, double T_s /*K*/, double m_dot_rad /*K*/, double Np, double m_dot_water /*kg/sec*/,
		//outputs
		double &T_rad_out /*K*/,double &W_radpump /*MW*/);

	void analytical_panel_calc(double T_db /*K*/, double T_rad_in /*K*/, double Tp_est /*K*/, double u /*m/s*/, double T_s /*K*/, double m_dot_rad /*kg/sec*/, 
		//outputs
		double &T_rad_out /*K*/,double &T_p /*K*/, double &W_radpump /*MW*/);

	void analytical_panel_calc_HX(double T_db /*K*/, double T_rad_in /*K*/, double Tp_est /*K*/, double u /*m/s*/, double T_s /*K*/, double m_dot_rad /*kg/sec*/, double Np, double m_dot_water /*kg/sec*/,
		//outputs
		double &T_rad_out /*K*/, double &T_p /*K*/, double &W_radpump /*MW*/);

};

#endif //__csp_solver_radiator_
