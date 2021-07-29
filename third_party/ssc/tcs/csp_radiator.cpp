/* ARD This is a new class for radiative cooling panel model.*/

#include "csp_radiator.h"
#include "csp_solver_util.h"
#include <fstream>
#include <iostream>


C_csp_radiator::C_csp_radiator()
{
	
}

void C_csp_radiator::init()
{
	//mc_coldhtf.SetFluid(ms_params.m_field_fl);  //initialize class for fluid circulating - not needed if using water /steam props
	mc_air.SetFluid(1);							//initialize class for air

	
	//Load measured sky temperature data
	int ii = 0;
	
	std::ifstream inputFile("C:/Users/adyreson/OneDrive/Documents/PhD/09_System/Desert_Rock_Weather/DesertRock2015_TS_localhr.txt");
	
	if (inputFile.is_open())	//check that input file was opened
	{
		while (!inputFile.eof()) //note that this only works if file is TAB separated and all spaces are deleted after end of file. Otherwise it reads last entry twice.
		{
			inputFile >> T_S_measured[ii];		//measured sky temp [K]
			inputFile >> T_S_localhr[ii];		//hr
			T_S_time[ii] = (ii + 1) * 3600;		//record the time in seconds at the end of the timestep.
			++ii;
		}
	}
	
}

void C_csp_radiator::night_cool(double T_db /*K*/, double T_rad_in /*K*/, double u /*m/s*/, double T_s /*K*/, double m_dot_rad /*K*/, double Np, double m_dot_coldstorage /*kg/sec*/,
	//outputs
	double &T_rad_out /*K*/, double &W_radpump /*MW*/)
{
	double Tp= std::numeric_limits<double>::quiet_NaN();
	double error_Tp = 10;		//[K]
	double tol = 1;				//[K]
	double Tp_est = T_rad_in;	//Initial estimate = inlet fluid temp.
	
	if (ms_params.m_field_fl == 3)	//If using water directly in radiator field (no HX)
	{
		while (error_Tp > tol)		//Iterate on the plate temperature.
		{
			analytical_panel_calc(T_db /*K*/, T_rad_in /*K*/, Tp_est /*K*/, u /*m/s*/, T_s /*K*/, m_dot_rad /*K*/,
				//outputs
				T_rad_out /*K*/, Tp /*K*/, W_radpump /*MW*/);
			error_Tp = abs(Tp_est - Tp);	//Update error
			Tp_est = Tp;					//Update guess value
		}
	}
	else
	{
		while (error_Tp > tol)		//Iterate on the plate temperature.
		{
			analytical_panel_calc_HX(T_db /*K*/, T_rad_in /*K*/, Tp_est /*K*/, u /*m/s*/, T_s /*K*/, m_dot_rad /*K*/, Np, m_dot_coldstorage/*kg/sec*/,
				//outputs
				T_rad_out /*K*/, Tp /*K*/, W_radpump /*MW*/);
			error_Tp = abs(Tp_est - Tp);	//Update error
			Tp_est = Tp;					//Update guess value
		}
	}
}	// night cool

void C_csp_radiator::analytical_panel_calc(double T_db /*K*/, double Tin /*K*/, double Tp_est /*K*/, double u /*m/s*/, double T_s /*K*/, double m_dot /*K*/,
	//outputs
	double &T_rad_out /*K*/, double &Tp /*K*/, double &W_radpump /*MW*/)
{
	/*	% Author: Ana Dyreson University of Wisconsin - Madison
		% Summary : This function determines the outlet temperature of a fluid
		% flowing through a radiative - convective cooling panel given the inlet
		% conditions, ambient weather, and geometry of the cooling panel.
		% This method is described in  Dyreson, A., Klein, S.A., Miller F.,
		% "Modeling Radiative-Convective Panels for Nighttime Passive Cooling Applications",
		% Journal of Solar Energy Engineering, October 2017, Volume 139.
		% This code demonstrates the method using water as the cooling fluid.
		% As described in the article, the calculation in this code can be iterated by updating
		% the estimated plate temperature using the results of the previous
		% calculation to obtain a more accurate solution. (This code does not
		% perform the iterations but can be called iteratively from another script.)

		% GEOMETRY:
		%This implementation assumes roll - bond type geometry or tubes otherwise
		%well connected to plate surface.The back of the plate is not insulated.
		%The method can be adapted for other geometry or other cooling fluids, etc.

		%INPUTS :
		%Inlet temperature of fluid : Tin[K]
		% Estimated temperature of plate(an initial estimate cold be Tin) : Tp_est[K]
		% Total mass flow rate through panel : m_dot[kg / sec]
		% Number of parallel tubes on a single panel : n
		% Distance between two parallel tubes : W[m]
		% Length of tubes : L[m]
		% Characteristic length for forced convection, typically equal to n*W
		%unless wind direction is known to determine flow path : Lc[m]
		% Dry bulb ambient air temperature : Tdb[K]
		% Ground temperature, often assumed equal to air temperature : Tg[K]
		% Wind speed : u[m / s]
		% Effective sky temperature, from measurement or correlations : Ts[K]
		% Thickness of plate : th[m]
		% Diameter of tube : D[m]
		% Conductivity of plate : k[W / m - K]
		% Emissivity of plate top surface : epsilon[-]
		% Emissivity of plate bottom surface : epsilonb[-]
		% Emissivity of ground : epsilong[-]
		% Length of series - connected sections of panels(if single panel, set equal
			%to L) : Lsec[m]

		% SAMPLE CALL in matlab
		%[Tout, Qu, Tp] = rad_cool(319.3, 319.3, 2.25, 50, 0.2, 100, 10, 299.3, 299.3, 3.1, 280.9, .002, .02, 235, .95, .07, .9, 100)
		%function[Tout, Qu, Tp, F, FR, h_w, h_forc_t, h_g, ULad, Fprime, Tad, hfi] = rad_cool(Tin, Tp_est, m_dot, n, W, L, Lc, Tdb, Tg, u, Ts, th, D, k, epsilon, epsilonb, epsilong, Lsec)
		% Function may be called with fewer outputs.*/
		
	

		//local variable names	
		int n = ms_params.n;
		double W = ms_params.W;
		double L = ms_params.L;
		double Lc = ms_params.L_c;
		double Lsec = ms_params.Lsec;
		double D = ms_params.D;
		double epsilon = ms_params.epsilon;
		double epsilonb = ms_params.epsilonb;
		double epsilong = ms_params.epsilong;
		double k = ms_params.k_panel;
		double th = ms_params.th;
		//define
		int c_free, c_force;
		double L_c_tot, hfi;
		
		double T_g = T_db;								// assume ground T = air T

		double m_dot_tube = m_dot / n;					//Tube mass flow rate
		double A_c = n * W*L;							//Area of panel
		double W_plate = n * W;							//Plate width
		double h_forc_t = 5.73 *pow(u,0.8)*pow(Lc,-0.2);	//Forced convection coefficient
		double Sigma = 5.67e-8;							//Stefan - Boltzmann constant
		double Tf = T_db + .25*(Tp_est - T_db);			//Estimate of film temperature
		
		double mu = mc_air.visc(300.);//0.00001787;							//[kg / m - sec] Viscosity of air
		double alpha = mc_air.therm_diff(300., 101300.);// 0.00001973;						//[m ^ 2 / sec] Thermal diffusivity of air
		
		double rho = mc_air.dens(300., 101300.);// 1.238;								//[kg / m ^ 3] Density of air
		double nu = mc_air.kin_visc(300., 101300.);							//[m ^ 2 / sec] Kinematic viscosity;
		double Pr = mc_air.Pr(300., 101300.);//.7092;								//[-] Prandtl number.
		double k_air = mc_air.cond(300.);	//0.02566;							//[W / m - K] Conductivity of air, assumed constant.
		double L_c_free = (Lsec*W_plate) / (2 * Lsec + 2 * W_plate);	 //[m] Characteristic length for free convection.
		double Ra = 9.81 *(1 / Tf)*abs((Tp_est - T_db))*pow(L_c_free,3) / (nu*alpha);  //[-] Rayleigh number estimate.
		double Gr = Ra / Pr;							//[-] Grashof number

		double Re = rho * u*Lc / mu;					//[-] Reynolds number for forced convection based on given characteristic length.
		double GrRe2 = Gr / (pow(Re,2) + .00001);			//Ratio of Grashof to Reynolds ^ 2 indicates importance of free vs.forced convection.
		if (GrRe2 <= 0.1)								//If < 0.1, free convection ignored.
		{
			c_free = 0;
		}
		else
		{
			c_free = 1;									//If > 0.1, free convection considered.
		}
		if (100 <= GrRe2)								//If > 100, forced convection ignored.
		{
			c_force = 0;
		}
		else
		{
			c_force = 1;								//If < 100, forced convection considered.
		}
				
														//Between 0.1 and 100, both are considered.
		if (100 <= GrRe2)								//Only if > 100, set characteristic length equal to Lc free.
		{
			L_c_tot = L_c_free;
		}
		else
		{
			L_c_tot = Lc;								//In all other cases, set characteristic length equal to Lc forced.
		}
					

					//Free convection & combination of free / forced convection considerations
					double Nusselt_free_t = 0.13 *pow(Ra,(1. / 3.));		//Correlation for free convection from heated plate.
					double h_free_t = Nusselt_free_t * k_air / L_c_free;	//Related free convection h.
					double Nusselt_forc_t = h_forc_t * Lc / k_air;			//Nusselt number related to forced convection correlation h.
					double m_conv = 3.5;									//Constant for combining free & forced convection.
					double Nusselt_tot_t = pow((c_free*pow(Nusselt_free_t,m_conv) + c_force * pow(Nusselt_forc_t,m_conv)) , (1 / m_conv));  //Combined free & forced convection.
					double h_w = Nusselt_tot_t * k_air / L_c_tot;			//Total Nu number.
					double Nusselt_free_b = 0.58 * pow(Ra,(.2));			//Correlation for free convection from heated plate(bottom).
					double h_g = Nusselt_free_b * k_air / L_c_free;			//Related free convection h from bottom.


					//Water properties
					water_TP(Tin, 101.3, &mc_coldhtf);					//Get water state at inlet temperature of fluid
					double cp =mc_coldhtf.cp*1000 ;						//mc_coldhtf.Cp(273)*1000.;               //[J / kg - K] Specific heat capacity of water
					double rho_water = mc_coldhtf.dens;					//mc_coldhtf.dens(273,101300.);	//[kg/m^3]
					double mu_water = water_visc(rho_water, Tin)*1e-6;	//Function result  is uPa-s, convert to kg/m-s. nu_water * rho_water;				//mc_coldhtf.visc(273);				//[kg / m - s]
					double nu_water = mu_water / rho_water; 			//1.66034557e-6 - 3.53322361e-8*(Tin - 273.15) + 2.50116564e-10*pow((Tin - 273.15), 2); //mc_coldhtf.kin_visc(273,101300.);	//Kinematic viscosity [m^2/s]
					double alpha_water = 1.478e-7;						//mc_coldhtf.therm_diff(273,101300.);  //[m^2/s] Assuming constant thermal diffusivity under temperatures 0 to 80 C.

					//Forced convection inside tube
					double Pr_water = nu_water / alpha_water;			//mc_coldhtf.Pr(273,101300.);			//[-] Prandtl number of water
					double Re_tube = 4 * m_dot_tube / (3.1415*mu_water*D);	//[-] Reynolds number inside tube
					double k_water = water_cond(rho_water, Tin);									//0.612;//[W / m - K]
					if (Re_tube < 2300)
					{
						hfi = 3.66*k_water / D;								//[W / m ^ 2 - K] For laminar flow assuming uniform wall temperature(conservative)
					}
					else
					{
						hfi = 0.023*pow(Re_tube,(0.8))*pow(Pr_water,0.3)*k_water / D;								//[W / m ^ 2 - K] Dittus - Boelter equation for turbulent, internal forced flow.Assumes smooth tubes.
					}


		//Collector model based in adiabatic temperature and using methods of Duffie & Beckman(2013).
		double T_bar = 0.5*(Tp_est + T_db);																		//An estimate of average of plate and ambient temperatures.
		double Tad = T_db - (Sigma*epsilon*(pow(T_db,4) - pow(T_s,4)) + Sigma * (1 / (1 / epsilonb + 1 / epsilong - 1))*(pow(T_db,4) - pow(T_g,4)) + h_g * (T_db - T_g)) / (4 * Sigma*(epsilon + 1 / (1 / epsilonb + 1 / epsilong - 1))*pow(T_bar,3) + h_g + h_w);	//Adiab. T
		double ULad = 4 * Sigma*(epsilon + 1 / (1 / epsilonb + 1 / epsilong - 1))*pow(T_bar,3)+ h_g + h_w;			//Adiabatic loss coefficient.
		double m = sqrt(ULad / (k*th));																				//Fin parameter
		double F = tanh(m*(W - D) / 2) / (m*(W - D) / 2);															//Fin efficiency
		double Fprime = 1 / (W*ULad / (3.1415*D*hfi) + W / (D + (W - D)*F));										//Collector efficiency based on roll bond type geometry.
		double FR = (m_dot*cp) / (A_c*ULad)*(1 - exp(-(A_c*ULad*Fprime) / (m_dot*cp)));								//Flow direction collector heat removal factor.
		double Qu = FR * A_c*ULad*(Tin - Tad);																		//Heat
		T_rad_out = Tin - Qu / (m_dot*cp);																			//Outlet temperature
		Tp = Qu / (ULad*A_c) + Tad;																			//Plate temperature
		double Tpa = 0.5*(Tp+T_db);																				//Updated value for average of plate & ambient temperature.

		//Pumping
		W_radpump = (ms_params.radfield_dp*ms_params.m_dot_panel*ms_params.Np) / (rho_water*0.75*0.85)/1000;	//MWe pumping power when radiator field is operating. Isentropic eff = 0.75 and Mechanical pump eff = 0.85.
}// adiabatic calc

void C_csp_radiator::analytical_panel_calc_HX(double T_db /*K*/, double Tin /*K*/, double Tp_est /*K*/, double u /*m/s*/, double T_s /*K*/, double m_dot /*K*/, double Np, double m_dot_water /*kg/sec*/,
	//outputs
	double &T_rad_out /*K*/, double &Tp /*K*/, double &W_radpump /*MW*/)
{
	/*	% Author: Ana Dyreson University of Wisconsin - Madison
	% Summary : This function determines the outlet temperature of a fluid
	% flowing through a radiative - convective cooling panel given the inlet
	% conditions, ambient weather, and geometry of the cooling panel.
	% This method is described in  Dyreson, A., Klein, S.A., Miller F.,
	% "Modeling Radiative-Convective Panels for Nighttime Passive Cooling Applications",
	% Journal of Solar Energy Engineering, October 2017, Volume 139.
	% This code demonstrates the method using water as the cooling fluid.
	% As described in the article, the calculation in this code can be iterated by updating
	% the estimated plate temperature using the results of the previous
	% calculation to obtain a more accurate solution. (This code does not
	% perform the iterations but can be called iteratively from another script.)

	% GEOMETRY:
	%This implementation assumes roll - bond type geometry or tubes otherwise
	%well connected to plate surface.The back of the plate is not insulated.
	%The method can be adapted for other geometry or other cooling fluids, etc.

	%INPUTS :
	%Inlet temperature of fluid : Tin[K]
	% Estimated temperature of plate(an initial estimate cold be Tin) : Tp_est[K]
	% Total mass flow rate through panel : m_dot[kg / sec]
	% Number of parallel tubes on a single panel : n
	% Distance between two parallel tubes : W[m]
	% Length of tubes : L[m]
	% Characteristic length for forced convection, typically equal to n*W
	%unless wind direction is known to determine flow path : Lc[m]
	% Dry bulb ambient air temperature : Tdb[K]
	% Ground temperature, often assumed equal to air temperature : Tg[K]
	% Wind speed : u[m / s]
	% Effective sky temperature, from measurement or correlations : Ts[K]
	% Thickness of plate : th[m]
	% Diameter of tube : D[m]
	% Conductivity of plate : k[W / m - K]
	% Emissivity of plate top surface : epsilon[-]
	% Emissivity of plate bottom surface : epsilonb[-]
	% Emissivity of ground : epsilong[-]
	% Length of series - connected sections of panels(if single panel, set equal
	%to L) : Lsec[m]
	
	% SAMPLE CALL in matlab
	%[Tout, Qu, Tp] = rad_cool(319.3, 319.3, 2.25, 50, 0.2, 100, 10, 299.3, 299.3, 3.1, 280.9, .002, .02, 235, .95, .07, .9, 100)
	%function[Tout, Qu, Tp, F, FR, h_w, h_forc_t, h_g, ULad, Fprime, Tad, hfi] = rad_cool(Tin, Tp_est, m_dot, n, W, L, Lc, Tdb, Tg, u, Ts, th, D, k, epsilon, epsilonb, epsilong, Lsec)
	% Function may be called with fewer outputs.*/



	//local variable names	
	int n = ms_params.n;
	double W = ms_params.W;
	double L = ms_params.L;
	double Lc = ms_params.L_c;
	double Lsec = ms_params.Lsec;
	double D = ms_params.D;
	double epsilon = ms_params.epsilon;
	double epsilonb = ms_params.epsilonb;
	double epsilong = ms_params.epsilong;
	double k = ms_params.k_panel;
	double th = ms_params.th;
	double epsilon_HX =ms_params.epsilon_HX;																					//Enter constant value HX effectiveness

	//define
	int c_free, c_force;
	double L_c_tot, hfi;

	double T_g = T_db;								// assume ground T = air T

	double m_dot_tube = m_dot / n;					//Tube mass flow rate
	double A_c = n * W*L;							//Area of panel
	double W_plate = n * W;							//Plate width
	double h_forc_t = 5.73 *pow(u, 0.8)*pow(Lc, -0.2);	//Forced convection coefficient
	double Sigma = 5.67e-8;							//Stefan - Boltzmann constant
	double Tf = T_db + .25*(Tp_est - T_db);			//Estimate of film temperature

	double mu = mc_air.visc(300.);//0.00001787;							//[kg / m - sec] Viscosity of air
	double alpha = mc_air.therm_diff(300., 101300.);// 0.00001973;						//[m ^ 2 / sec] Thermal diffusivity of air

	double rho = mc_air.dens(300., 101300.);// 1.238;								//[kg / m ^ 3] Density of air
	double nu = mc_air.kin_visc(300., 101300.);							//[m ^ 2 / sec] Kinematic viscosity;
	double Pr = mc_air.Pr(300., 101300.);//.7092;								//[-] Prandtl number.
	double k_air = mc_air.cond(300.);	//0.02566;							//[W / m - K] Conductivity of air, assumed constant.
	double L_c_free = (Lsec*W_plate) / (2 * Lsec + 2 * W_plate);	 //[m] Characteristic length for free convection.
	double Ra = 9.81 *(1 / Tf)*abs((Tp_est - T_db))*pow(L_c_free, 3) / (nu*alpha);  //[-] Rayleigh number estimate.
	double Gr = Ra / Pr;							//[-] Grashof number

	double Re = rho * u*Lc / mu;					//[-] Reynolds number for forced convection based on given characteristic length.
	double GrRe2 = Gr / (pow(Re, 2) + .00001);			//Ratio of Grashof to Reynolds ^ 2 indicates importance of free vs.forced convection.
	if (GrRe2 <= 0.1)								//If < 0.1, free convection ignored.
	{
		c_free = 0;
	}
	else
	{
		c_free = 1;									//If > 0.1, free convection considered.
	}
	if (100 <= GrRe2)								//If > 100, forced convection ignored.
	{
		c_force = 0;
	}
	else
	{
		c_force = 1;								//If < 100, forced convection considered.
	}

	//Between 0.1 and 100, both are considered.
	if (100 <= GrRe2)								//Only if > 100, set characteristic length equal to Lc free.
	{
		L_c_tot = L_c_free;
	}
	else
	{
		L_c_tot = Lc;								//In all other cases, set characteristic length equal to Lc forced.
	}


	//Free convection & combination of free / forced convection considerations
	double Nusselt_free_t = 0.13 *pow(Ra, (1. / 3.));		//Correlation for free convection from heated plate.
	double h_free_t = Nusselt_free_t * k_air / L_c_free;	//Related free convection h.
	double Nusselt_forc_t = h_forc_t * Lc / k_air;			//Nusselt number related to forced convection correlation h.
	double m_conv = 3.5;									//Constant for combining free & forced convection.
	double Nusselt_tot_t = pow((c_free*pow(Nusselt_free_t, m_conv) + c_force * pow(Nusselt_forc_t, m_conv)), (1 / m_conv));  //Combined free & forced convection.
	double h_w = Nusselt_tot_t * k_air / L_c_tot;			//Total Nu number.
	double Nusselt_free_b = 0.58 * pow(Ra, (.2));			//Correlation for free convection from heated plate(bottom).
	double h_g = Nusselt_free_b * k_air / L_c_free;			//Related free convection h from bottom.


	//Water properties
	double cp_water;
	if (Tin <= 274)
	{
		cp_water = 4183;									//[J/kg-K] hardcode in case glycol loop is less than freezing point of water
	}
	else
	{
		water_TP(Tin, 101.3, &mc_coldhtf);					//Get water state at inlet temperature of fluid - this only approximates water temp
		cp_water = mc_coldhtf.cp * 1000;					//[J / kg - K] Specific heat capacity of water
	}

	//Fluid properties within tube - using inlet temperature of water side of HX - this is approximate for glygcol temperature
	int idx_props =static_cast<int>(Tin - 273.15) - T_PG20[0] + 1; //Truncate temperature to degree [C] and get index in EG property data as provided based on starting point of that property data.
	int idx_props_check= std::numeric_limits<double>::quiet_NaN();	  //In case temperature is at an extreme end, use next closest value.
	if (idx_props > 67) 
	{
		idx_props_check = 67;	
	}
	else if (idx_props < 0)
	{
		idx_props_check = 0;
	}
	else
	{
		idx_props_check = idx_props;
	}
	
	double cp = cp_PG20[idx_props_check]*1000;					//Get property data based on temperature. Convert to J/kg-K
	double rho_fluid = rho_PG20[idx_props_check];
	double mu_fluid = mu_PG20[idx_props_check];
	double nu_fluid = mu_fluid / rho_fluid;
	double alpha_fluid = alpha_PG20[idx_props_check];
	double k_fluid = k_PG20[idx_props_check];
	

	//Forced convection inside tube
	double Pr_fluid = nu_fluid / alpha_fluid;				//[-] Prandtl number of water
	double Re_tube = 4 * m_dot_tube / (3.1415*mu_fluid*D);	//[-] Reynolds number inside tube
	if (Re_tube < 2300)
	{
		hfi = 3.66*k_fluid / D;								//[W / m ^ 2 - K] For laminar flow assuming uniform wall temperature(conservative)
	}
	else
	{
		hfi = 0.023*pow(Re_tube, (0.8))*pow(Pr_fluid, 0.3)*k_fluid / D;								//[W / m ^ 2 - K] Dittus - Boelter equation for turbulent, internal forced flow.Assumes smooth tubes.
	}


	//Collector model based in adiabatic temperature and using methods of Duffie & Beckman(2013).
	double T_bar = 0.5*(Tp_est + T_db);																		//An estimate of average of plate and ambient temperatures.
	double Tad = T_db - (Sigma*epsilon*(pow(T_db, 4) - pow(T_s, 4)) + Sigma * (1 / (1 / epsilonb + 1 / epsilong - 1))*(pow(T_db, 4) - pow(T_g, 4)) + h_g * (T_db - T_g)) / (4 * Sigma*(epsilon + 1 / (1 / epsilonb + 1 / epsilong - 1))*pow(T_bar, 3) + h_g + h_w);	//Adiab. T
	double ULad = 4 * Sigma*(epsilon + 1 / (1 / epsilonb + 1 / epsilong - 1))*pow(T_bar, 3) + h_g + h_w;			//Adiabatic loss coefficient.
	double m = sqrt(ULad / (k*th));																				//Fin parameter
	double F = tanh(m*(W - D) / 2) / (m*(W - D) / 2);															//Fin efficiency
	double Fprime = 1 / (W*ULad / (3.1415*D*hfi) + W / (D + (W - D)*F));										//Collector efficiency based on roll bond type geometry.
	double FR = (m_dot*cp) / (A_c*ULad)*(1 - exp(-(A_c*ULad*Fprime) / (m_dot*cp)));								//Flow direction collector heat removal factor.
	double CMIN= std::numeric_limits<double>::quiet_NaN();

	if((Np*m_dot*cp) < (m_dot_water*cp_water))	//Use the full flow rates at HX to compare																			//Determine minimum capacitance rate of HX
	{
		CMIN = Np*m_dot * cp;
	}
	else
	{
		CMIN = m_dot_water * cp_water;
	}
	double FRprime = FR / (1 + (A_c*FR*ULad) / (m_dot*cp)*((Np*m_dot*cp) / (epsilon_HX*CMIN) - 1));
	double Qu = FRprime * A_c*ULad*(Tin - Tad);																	//Heat
	//double T_panel_out = Tin - Qu /(epsilon_HX*CMIN);															//Outlet temperature from radiator panel (glycol)
	T_rad_out = Tin - Qu*Np / (m_dot_water*cp_water);															//Outlet temperature of water side of HX. Because this flow rate is full for all panels through HX, need to multiply Qu by Np.
	Tp = Qu / (ULad*A_c) + Tad;																					//Plate temperature
	double Tpa = 0.5*(Tp + T_db);																				//Updated value for average of plate & ambient temperature.
	//Pumping
	W_radpump = (ms_params.radfield_dp*ms_params.m_dot_panel*ms_params.Np) / (rho_fluid*0.75*0.85) / 1000;	//MWe pumping power when radiator field is operating. Isentropic eff = 0.75 and Mechanical pump eff = 0.85.


}// adiabatic calc using a separate fluid in radiator loop 