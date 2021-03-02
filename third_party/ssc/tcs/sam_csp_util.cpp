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

#include <algorithm>

#include "sam_csp_util.h"
//#include "waterprop.h"
#include "water_properties.h"

#include "csp_solver_util.h"

using namespace std;

/* 
Define functions and methods that are useful in CSP modules
*/


//--- generalized interpolation functions ---

double CSP::interp(util::matrix_t<double> *data, double x, int low_bound, int up_bound, bool increasing){
	/* 
	Given a matrix with 2 rows and N columns, interpolate along row 0 to find a corresponding 
	value in row 1. 

	-----------------------------------------------------------------------------
	data.at(0,:)	|	X - independent variable data
	data.at(1,:)	|	Y - dependent variable data
	x				|	independent variable
	low_bound		|	{optional} Minimum index of interest for interpolation
	up_bound		|	{optional} Maximum index of interest for interpolation
	increasing		|	The data is in increasing order
	-----------------------------------------------------------------------------

	Unlike the methods used in HTFProperties, this assumes no storage of indices from call to call.

	Method uses bisection.
	*/

	if(low_bound < 0) low_bound = 0;
	if (up_bound < 0) up_bound = (int)data->ncols() - 1;	//Index of the last entry

	if(up_bound < low_bound) return 0;
	if(up_bound == low_bound) return data->at(1,low_bound);
		
	int jl = low_bound, ju = up_bound;
	int jm;
	while (ju - jl > 1){
		jm = (ju + jl)/2;	//middle index of the range

		if(x < data->at(0,jm)) {
			if(increasing){
				ju = jm;
			}
			else{
				jl = jm;
			}
		}
		else{
			if(increasing){
				jl = jm;
			}
			else{
				ju = jm;
			}
		}
	}
	//now interpolate between the upper and lower bounds
	double y = data->at(1,jl) + (x - data->at(0,jl))/(data->at(0,ju) - data->at(0,jl))*(data->at(1,ju) - data->at(1,jl));
	if( (increasing && y<data->at(1,low_bound)) || (!increasing && y>data->at(1,low_bound)) ){ 
		y = data->at(1,low_bound);
	}
	else if( (!increasing && y < data->at(1, low_bound)) || (increasing && y>data->at(1, up_bound)) ){
		y = data->at(1, up_bound);
	}
	return y;
};

double CSP::interp(double *xdat, double *ydat, double x, int low_bound, int up_bound, bool increasing){
	/* 
	Given X and Y data arrays, interpolate along X to find a corresponding 
	value in Y. 

	This is an overload of the matrix_t<> call above. 

	-----------------------------------------------------------------------------
	xdat			|	X - independent variable data
	ydat			|	Y - dependent variable data
	x				|	independent variable
	low_bound		|	Minimum index of interest for interpolation
	up_bound		|	Maximum index of interest for interpolation
	increasing		|	The data is in increasing order
	-----------------------------------------------------------------------------

	Unlike the methods used in HTFProperties, this assumes no storage of indices from call to call.

	Method uses bisection.
	*/

	if(up_bound < low_bound) return 0;
	if(up_bound == low_bound) return ydat[up_bound];
		
	int jl = low_bound, ju = up_bound;
	int jm;
	while (ju - jl > 1){
		jm = (ju + jl)/2;	//middle index of the range

		if(x < xdat[jm]) {
			if(increasing){
				ju = jm;
			}
			else{
				jl = jm;
			}
		}
		else{
			if(increasing){
				jl = jm;
			}
			else{
				ju = jm;
			}
		}
	}
	//now interpolate between the upper and lower bounds
	double y = ydat[jl] + (x - xdat[jl])/(xdat[ju] - xdat[jl])*(ydat[ju] - ydat[jl]);
	
    if( (increasing && y<ydat[low_bound]) || (!increasing && y>ydat[low_bound]) )
		y = ydat[low_bound];
	
    else if( (!increasing && y < ydat[up_bound]) || (increasing && y>ydat[up_bound]) )
		y = ydat[up_bound];

	return y;
};

double CSP::interp2D(double *xvals, int &nx, double *yvals, int &ny, double *data2D, double x, double y, bool strict_range){
	/* 
	This method interpolates a 2D array (as a list of information) based on the values of x and y.

	xvals -> size(nx)	|	Positional data along the X dimension. Provide in ascending order
	yvals -> size(ny)	|	Positional data along the Y dimension. Provide in ascending order
	data2D-> size(nx*ny)|	Array containing values to be interpolated. Data provided as (1->ny) for rows (1->nx).
	x					|	basis of interpolation along X
	y					|	basis of interpolation along Y
	strict_range		|	Throw an error if either 'x' or 'y' are outside of the bounds of X and Y
		
	*/

	//Use a bisection approach

	//first in x
	int xlow = 0, xhi = nx-1;
	int xrange = xhi - xlow;
	int xmid = xrange/2;
	
	//Check for x in range
	if(strict_range && (x < xlow || x > xhi))
		return std::numeric_limits<double>::quiet_NaN();

	while(xrange>1){
		if(x > xvals[xmid]){
			xlow = xmid;
		}
		else
		{
			xhi = xmid;
		}
		//Check for bounds
		if(xlow > nx-2) break;
		if(xhi < 1) break;
		xmid = (xhi + xlow)/2;
		xrange = xhi-xlow;
	}



	//in y
	int ylow = 0, yhi = ny-1;
	int yrange = yhi - ylow;
	int ymid = yrange/2;
	
	//Check for y in range
	if(strict_range && (y < ylow || y > yhi))
		return std::numeric_limits<double>::quiet_NaN();

	while(yrange>1){
		if(y > yvals[ymid]){
			ylow = ymid;
		}
		else
		{
			yhi = ymid;
		}
		//Check for bounds
		if(ylow > ny-2) break;
		if(yhi < 1) break;
		ymid = (yhi + ylow)/2;
		yrange = yhi-ylow;
	}


	//interpolate
	double
		xf = (x - xvals[xlow])/(xvals[xhi] - xvals[xlow]),
		yf = (y - yvals[ylow])/(yvals[yhi] - yvals[ylow]);

	//Get the 4 surrounding points for interpolation
	double
		p11 = data2D[ ylow*nx + xlow ],
		p12 = data2D[ ylow*nx + xhi ],
		p21 = data2D[ yhi*nx + xlow ],
		p22 = data2D[ yhi*nx + xhi ];
	
	//Calculate the x-interpolated points
	double
		x1 = p11 + xf*(p12 - p11),
		x2 = p21 + xf*(p22 - p21);
	//calculate the final interpolated value
	return x1 + yf*(x2 - x1);
	
}

void CSP::theta_trans(double alpha_sun /*rad*/, double phi_sun /*rad*/, double alpha_fix /*rad*/, double &phi_t /*rad*/, double &theta /*rad*/)
{
	/*
	Take solar position and convert it into longitudinal and transversal incidence angles
    Reference: G. Zhu (2011). Incidence Angle Modifier for Parabolic Trough Collector and its 
                Measurement at SIMTA. Internal communication, NREL, August, 2011.
        
    ------------------------------------------------------------------------------------------
    INPUTS:
    ------------------------------------------------------------------------------------------
        *   alpha_sun       [rad] Solar azimuth angle, range is (-90=E..0=S..+90=W)
        *   phi_sun         [rad] Solar zenith angle, zero is directly overhead
        *   alpha_fix       [rad] Angle of rotation of the collector axis. Zero when aligned north-
                            south, positive clockwise
    OUTPUTS:
    ------------------------------------------------------------------------------------------
        *   phi_t           [rad] Collector angle in the transversal plane
        *   theta           [rad] Collector angle in the longitudinal plane
    ------------------------------------------------------------------------------------------
    */
        
    //if the sun is below the horizon, return zeros
    if(phi_sun >= pi/2.) 
	{
        phi_t = 0.0;	//[rad] 
		theta = 0.0;	//[rad]
        return;
	}
        
    //Convert the solar azimuth to 0=N
    double alpha_sunX = alpha_sun + pi;		//[rad]
        
    //Calculate angles
    phi_t = fabs(atan(tan(phi_sun)*sin(alpha_sunX - alpha_fix))); //[rad] collector angle in transversal plane
    theta = fabs(asin(sin(phi_sun)*cos(alpha_sunX - alpha_fix))); //[rad] collector angle in the longitudinal plane
        
    //check for NaN
    if(theta!=theta || phi_t != phi_t) 
	{
        phi_t = 0.0;	//[rad] 
		theta = 0.0;	//[rad]
	}
        
	return;
}

//sky temp function
double CSP::skytemp(double T_amb_K, double T_dp_K, double hour){
		
	/*
	**********************************************************************
		This function uses the correlation for Sky Temperature             *
		that was provided in Duffie & Beckman (2006), and was              *
		also implemented in EES.                                           *
																			*
		This function takes as inputs:                                     *
		- T_amb -> ambient air temperature, dry bulb [K]                 *
		- T_dp  -> the ambient dewpoint temperature [K]                  *
		- hour  -> the hour, in solar time starting at midnight           *
		The function outputs:                                              *
		- skytemp -> the effective temperature of the sky, in degrees [K]*
																			*
	**********************************************************************
	*/

	double T_dpC, time;
	double pi=acos(-1.);

	//express "time" in terms of an angle  [rad]
	time = hour*15.*pi/180.;

	//The inputs are in terms of degrees K, but the dewpoint temperature is in terms of degrees C
	T_dpC = T_dp_K-273.15;

	//The sky temperature relationship
	return T_amb_K*pow(.711+.0056*T_dpC+.000073*T_dpC*T_dpC+.013*cos(time), .25);
};

double CSP::sign(double val){
	if(val < 0.) { return -1.0; }
	else{ return 1.0; }
};

double CSP::nint(double val){
	// returns the nearest integer
	return fmod(val,1.) < 0.5 ? floor(val) : ceil(val);
}

int CSP::TOU_Reader(double *TOUSched, double time_sec, int nTOUSched){
	/* Returns the current Time of Use period */
	/* TOUSched should have zero indexed value (all values should be between 0 and 8) */
	int hr = (int)(floor(time_sec/3600.+1.e-6)-1);
	if(hr>nTOUSched-1 || hr<0){
		return -1;	//ERROR
	}
	return (int)TOUSched[hr];
}

double CSP::poly_eval(double x, const double *coefs, const int &order){
	/* 
	Evaluate a polynomial at 'x' with coefficients 'coefs[size=order]'. Return the evaluated result
	*/
	double y = 0.;

	for(int i=0; i<order; i++){
		y += coefs[i] * pow(x, i);
	}
	return y;

}

double CSP::Nusselt_FC( double ksDin, double Re )
{
	// This is the forced convection correlation presented in [Siebers and Kraabel, 1984]
	// The value of ks\D determines the interpolation that takes place between these 4 data points

	double ksD = ksDin;
	double Nomval = ksD;
	int rerun = 0;

	double Nu_FC, ValHi, ValLo, Nu_Lo, Nu_Hi, ValHi2, ValLo2;
	// Select the bounding conditions
	
	bool repeat_loop = true;

	do
	{
		repeat_loop = false;
	
		// Point 1: ksD = 0.0
		if(ksD < 75.E-5)
		{
			Nu_FC = 0.3 + 0.488*pow( Re, 0.5 )*pow( (1.0+pow( (Re/282000), 0.625 )), 0.8);
			ValHi = 75.E-5;
			ValLo = 0.0;
		}
		else		// Point 2: ksD = 75 E-5
		{
			if(  ksD>=75.E-5 && ksD<300.E-5 )
			{
				ValHi = 300.E-5;
				ValLo = 75.E-5;
				if( Re <= 7.E5 )
					Nu_FC = 0.3 + 0.488*pow( Re, 0.5 )*pow( (1.0+pow( (Re/282000), 0.625 )), 0.8);
				else
				{
					if( Re>7.0E5 && Re<2.2E7 )
						Nu_FC = 2.57E-3*pow( Re, 0.98 );
					else
						Nu_FC = 0.0455*pow( Re, 0.81 );
				}
			}	
			else	// Point 3: ksD = 300E-5
			{
				if( ksD>=300.E-5 && ksD<900.E-5 )
				{
					ValHi = 900.E-5;
					ValLo = 300.E-5;
					if( Re <= 1.8E5 )
						Nu_FC = 0.3 + 0.488*pow( Re, 0.5 )*pow( (1.0+pow( (Re/282000), 0.625 )), 0.8);
					else
					{
						if( Re>1.8E5 && Re<4.E6 )
							Nu_FC = 0.0135*pow( Re, 0.89 );
						else
							Nu_FC = 0.0455*pow( Re, 0.81 );
					}			
				}
				else	// Point 4: ksD = 900 E -5
				{
					if( ksD >= 900.0E-5 )
					{
						ValHi = 900.0E-5;
						ValLo = 900.0E-5;
						if( Re <= 1E5 )
							Nu_FC = 0.3 + 0.488*pow( Re, 0.5 )*pow( (1.0+pow( (Re/282000), 0.625 )), 0.8);
						else
							Nu_FC = 0.0455*pow( Re, 0.81 );
					}
				}
			}
		}

		if( rerun != 1 )
		{
			rerun = 1;
			Nu_Lo = Nu_FC;
			ksD = ValHi;
			ValLo2 = ValLo;
			ValHi2 = ValHi;
			repeat_loop = true;
		}

	} while( repeat_loop );

	Nu_Hi = Nu_FC;

	double chi;
	if( Nomval >= 900.E-5 )
		chi = 0.0;
	else
		chi = (Nomval - ValLo2)/(ValHi2 - ValLo2);

	Nu_FC = Nu_Lo + (Nu_Hi - Nu_Lo)*chi;

	return Nu_FC;
}

void CSP::PipeFlow(double Re, double Pr, double LoverD, double relRough, double &Nusselt, double &f){

	/*********************************************************************
	* PipeFlow_turbulent:                                               *
	* This procedure calculates the average Nusselt number and friction *
	* factor for turbulent flow in a pipe given Reynolds number (Re),   *
	* Prandtl number (Pr), the pipe length diameter ratio (LoverD) and  *
	* the relative roughness}                                           *
	*********************************************************************/
	double f_fd,Nusselt_L, Gz, Gm, Nusselt_T, Nusselt_H,fR,X;

	//Correlation for laminar flow.. Note that no transitional effects are considered
	if (Re < 2300.) {
		//This procedure calculates the average Nusselt number and friction factor for laminar flow in a pipe 
		//..given Reynolds number (Re), Prandtl number (Pr), the pipe length diameter ratio (LoverD) 
		//..and the relative roughness}
		Gz=Re*Pr/LoverD;
		X=LoverD/Re;
		fR=3.44/sqrt(X)+(1.25/(4*X)+16-3.44/sqrt(X))/(1+0.00021*pow(X,-2));
		f=4.*fR/Re;
		//{f$='Shah' {Shah, R.K.  and London, A.L. "Laminar Flow Forced Convection in Ducts", 
		//..Academic Press, 1978 ,Eqn 192, p98}}
		Gm=pow(Gz,1./3.);
		Nusselt_T=3.66+((0.049+0.02/Pr)*pow(Gz,1.12))/(1+0.065*pow(Gz,0.7));
		Nusselt_H=4.36+((0.1156 +0.08569 /pow(Pr,0.4))*Gz)/(1+0.1158*pow(Gz,0.6));
		//{Nusselt$='Nellis and Klein fit to Hornbeck'  {Shah, R.K.  and London, A.L. "Laminar Flow Forced Convection in Ducts",
		//..Academic Press, 1978 ,Tables  20 and 22}}
		Nusselt = Nusselt_T;  //Constant temperature Nu is better approximation
	}
	else { //Correlation for turbulent flow
		f_fd = pow(0.79*log(Re)-1.64, -2); //Petukhov, B.S., in Advances in Heat Transfer, Vol. 6, Irvine and Hartnett, Academic Press, 1970
		Nusselt_L= ((f_fd/8.)*(Re-1000)*Pr)/(1.+12.7*sqrt(f_fd/8.)*(pow(Pr, 2/3.)-1.)); //Gnielinski, V.,, Int. Chem. Eng., 16, 359, 1976

		if (relRough > 1e-5) {

		  //f=8.*((8./Re)**12+((2.457*log(1./((7./Re)**0.9+0.27*(RelRough))))**16+(37530./Re)**16)**(-1.5))**(1./12.)
		  //mjw 8.30.2010 :: not used  
    
		  f_fd=pow(-2.*log10(2*relRough/7.4-5.02*log10(2*relRough/7.4+13/Re)/Re), -2);

		  Nusselt_L= ((f_fd/8.)*(Re-1000.)*Pr)/(1.+12.7*sqrt(f_fd/8.)*(pow(Pr, 2/3.)-1.)); //Gnielinski, V.,, Int. Chem. Eng., 16, 359, 1976}
		}
		f=f_fd*(1.+pow(1./LoverD, 0.7)); //account for developing flow
		Nusselt= Nusselt_L*(1.+pow(1./LoverD, 0.7));  //account for developing flow
	}
}
	// CSP Cooling functions
double CSP::P_sat4(double T_celcius)
{
	double T_K = T_celcius + 273.15; 
	return (-99.7450105 + 1.02450484*T_K - 0.00360264243*T_K*T_K + 0.00000435512698*T_K*T_K*T_K)*1.e5;
}

// Calculates enthalpy of air [J/kg] as a function of temperature [C]
double CSP::f_h_air_T(double T_C) 
{ 
	return 273474.659 + (1002.9404*T_C) + (0.0326819988*T_C*T_C); 
} 

// Evaporative cooling calculations
void CSP::evap_tower(int tech_type, double P_cond_min, int n_pl_inc, double DeltaT_cw_des, double T_approach, double P_cycle, 
							 double eta_ref, double T_db_K, double T_wb_K, double P_amb, double q_reject, double &m_dot_water, 
							 double &W_dot_tot, double &P_cond, double &T_cond, double &f_hrsys)
{
	/*
	double c_air, c_cw, deltah_evap, deltat_cw, dp_evap, drift_loss_frac, dt_out, eta_fan, eta_fan_s,
          eta_pcw_s, eta_pump, h_fan_in, h_fan_out, h_fan_out_s, h_pcw_in, h_pcw_out,
          h_pcw_out_s, m_dot_air, m_dot_blowdown, m_dot_cw, m_dot_cw_des, m_dot_drift, blowdown_frac,
          m_dot_evap, mass_ratio_fan, p_ratio_fan, q_reject_des, R, rho_cw, s_pcw_in, t_fan_in, 
		  t_fan_in_k, t_fan_out, t_fan_out_k, w_dot_cw_pump, w_dot_fan;*/
	/*
	!------------------------------------------------------------------------------------------------------------
	!--Inputs
	!   * P_cond_min    [Pa]    Minimum allowable condenser pressure
	!   * n_pl_inc      [-]     Number of part load heat rejection levels
	!   * DeltaT_cw_des [K]     Cooling water temperature rise across condenser
	!   * T_approach    [K]     Cooling tower approach temperature, difference between cw out and wet bulb temp
	!   * P_cycle       [W]     Rated power block capacity
	!   * eta_ref       [-]     Rated gross conversion efficiency
	!   * T_db          [K]     Dry bulb temperature (converted to C)
	!   * P_amb         [Pa]    Atmospheric pressure
	!------------------------------------------------------------------------------------------------------------
	!--Output
	!   * m_dot_water   [kg/s]  Total cooling tower water usage
	!   * W_dot_tot     [MW]    Total parasitic power for cooling tower model
	!   * P_cond        [Pa]    Condenser steam pressure
	!   * T_cond        [K]     Condenser steam temperature
	!   * f_hrsys       [-]     Fraction of the cooling system operating
	!------------------------------------------------------------------------------------------------------------
	*/

	// Unit conversions
	double T_db = T_db_K - 273.15;    //[C] Converted dry bulb temp
	double T_wb = T_wb_K - 273.15;    //[C] Converted wet bulb temp

	// Values that can be estimated
	double dt_out = 3.0;				// Temperature difference at hot side of the condenser
	double drift_loss_frac = 0.001;    // Drift loss fraction
	double blowdown_frac = 0.003;      // Blowdown fraction
	double dp_evap = 0.37*1.0e5;       // [Pa] Pressure drop across the condenser and cooling tower
	double eta_pump = 0.75;            // Total pump efficiency
	double eta_pcw_s = 0.8;            // Isentropic cooling water pump efficiency
	double eta_fan = 0.75;             // Fan mechanical efficiency
	double eta_fan_s = 0.8;            // Fan isentropic efficiency
	double p_ratio_fan = 1.0025;       // Fan pressure ratio
	double mass_ratio_fan = 1.01;      // Ratio of air flow to water flow in the cooling tower

	// Cooling water specific heat
	water_state wp;
	water_TP( max( T_wb, 10.0 )+273.15, P_amb/1000.0, &wp );
	double c_cw = wp.cp * 1000.0;		// Convert to J/kg-K

	// **** Calculations for design conditions
	double q_reject_des = P_cycle*(1./eta_ref-1.0);    	    // Heat rejection from the cycle
	double m_dot_cw_des = q_reject_des/(c_cw*DeltaT_cw_des);	// Mass flow rate of cooling water required to absorb the rejected heat
	f_hrsys = 1.0;   // Initial fraction of cooling system operating

	// **** Calculations for performance
	// Calculate the cooling water temp. rise associated with normal cooling system operation
	double m_dot_cw = m_dot_cw_des;
	double deltat_cw = q_reject/(m_dot_cw*c_cw);

	// Condenser saturation temperature
	T_cond = T_wb + deltat_cw + dt_out + T_approach; // celcius

	// Condenser back pressure
	if(tech_type != 4)
	{	
		water_TQ(T_cond + 273.15, 1.0, &wp);
		P_cond = wp.pres * 1000.0;
	}
	else
		P_cond = CSP::P_sat4(T_cond); // isopentane


	// MJW 7.19.2010 :: Cooling system part-load strategy uses the number of part-load increments to determine how the coolign system is
	// partially shut down during under design operation. The condenser pressure is reduced with the cooling system running
	// at full load until it reaches the minimum condenser pressure. The cooling system then incrementally shuts off bays until
	// the condenser temperature/pressure rise above their minimum level. Default cond. pressure is 1.25 inHg (4233 Pa).
	if ( (P_cond < P_cond_min) && (tech_type != 4) ) // Aug 3, 2011: No lower limit on Isopentane
	{
		for (int i=2; i <=n_pl_inc; i++)
		{
			f_hrsys = (1.0 - (float)((i-1.0)/n_pl_inc));
			m_dot_cw = m_dot_cw_des*f_hrsys;
			deltat_cw = q_reject/(m_dot_cw*c_cw);
			T_cond = T_wb + deltat_cw + dt_out + T_approach;

			water_TQ(T_cond + 273.15, 1.0, &wp);
			P_cond = wp.pres * 1000.0;
			
			if(P_cond > P_cond_min) break;
		}
		if(P_cond <= P_cond_min)
		{
			// Still below min. fix to min condenser pressure and recalc. temp.
			
			P_cond = P_cond_min;

			water_PQ( P_cond/1000.0, 1.0, &wp );
			T_cond = wp.temp-273.15;
			
			deltat_cw = T_cond - (T_wb + dt_out + T_approach);
			m_dot_cw = q_reject/(deltat_cw * c_cw);
		}
	}
	water_TP( T_cond - 3.0 + 273.15, P_amb/1000.0, &wp );
	double h_pcw_in = wp.enth*1000.0;
	//double s_pcw_in = wp.entr*1000.0;
	double rho_cw = wp.dens;	
	
	double h_pcw_out_s = (dp_evap/rho_cw) + h_pcw_in;								// [J/kg] isentropic outlet enthalpy.. incompressible fluid
	double h_pcw_out = h_pcw_in + ((h_pcw_out_s - h_pcw_in)/eta_pcw_s);			// [J/kg] Outlet enthalpy accounting for irreversibility
	double w_dot_cw_pump = (h_pcw_out - h_pcw_in) * m_dot_cw/eta_pump * 1.0E-6;	// [MW] Cooling water circulating pump power

	// Fan power
	double m_dot_air = m_dot_cw*mass_ratio_fan;
	double t_fan_in = (T_db + T_wb + T_approach)/2.0;
	double h_fan_in = f_h_air_T(t_fan_in);

	double c_air = 1003.0;		// [J/kg-K] specific heat of air (This is relatively constant)
	double R = 8314./28.97;	// [J/kmol-K]/[kg/kmol] Gas constant over the molar mass of air

	double t_fan_in_k = t_fan_in + 273.15;										// Fan inlet temp, in K
	double t_fan_out_k = t_fan_in_k * pow(p_ratio_fan,(R/c_air));				// [K] isentropic temperature rise
	double t_fan_out = t_fan_out_k - 273.15;									// [C] Convert isentropic temperature rise to deg C
	double h_fan_out_s = f_h_air_T(t_fan_out);									// [J/kg] Calculate isentropic enthalpy at fan outlet
	double h_fan_out = h_fan_in + (h_fan_out_s - h_fan_in)/eta_fan_s;			// [J/kg] Actual enthalpy, accounting for irreversibility

	double w_dot_fan = (h_fan_out - h_fan_in)*m_dot_air/eta_fan*1.0E-6;  // [MW] Fan parasitic power

	// Total cooling tower parasitic power
	W_dot_tot = w_dot_cw_pump + w_dot_fan;   // [MW]
		
	// Enthalpy of evaporation
	// 1/28/13, twn: replace call to curve fit with call to steam properties routine
	//deltah_evap = f_dh_evap(P_amb);
	water_PQ( P_amb/1000.0, 0.0, &wp );
	double dh_low = wp.enth;
	water_PQ( P_amb/1000.0, 1.0, &wp );
	double dh_high = wp.enth;
	double deltah_evap = (dh_high - dh_low)*1000.0;	// [J/kg]

	// Evaporative water loss
	double m_dot_evap = q_reject/deltah_evap;

	// Other water losses
	double m_dot_drift = drift_loss_frac * m_dot_cw;			// Drift loss fraction, based on cooling water mass flow rate
	double m_dot_blowdown = blowdown_frac * m_dot_cw;			// Blow down fraction

	// Total power block water usage
	m_dot_water = m_dot_evap + m_dot_drift + m_dot_blowdown;

	// Unit conversions
	T_db = T_db + 273.15;		// [C] Converted dry bulb temp (TFF - I think this is irrelevant, since it's not passed back out)
	T_wb = T_wb + 273.15;		// [C] Converted wet bulb temp (TFF - I think this is irrelevant, since it's not passed back out)
	T_cond = T_cond + 273.15;	// [K] Convert to K for output
}


// Air cooling calculations
void CSP::ACC(int tech_type, double P_cond_min, int n_pl_inc, double T_ITD_des, double P_cond_ratio, double P_cycle, double eta_ref,
    double T_db, double /*P_amb*/, double q_reject, double& m_dot_air, double& W_dot_fan, double& P_cond, double& T_cond,
    double& f_hrsys)
{
    /*
    !------------------------------------------------------------------------------------------------------------
    !--Inputs
    !   * tech_type     [-]
    C   * P_cond_min    [Pa]    Minimum allowable condenser pressure
    C   * n_pl_inc      [-]     Number of part load heat rejection levels
    C   * T_ITD_des     [K]     ACC initial temperature difference, difference between dry bulb and steam inlet temp
    !   * P_cond_ratio  [-]     Condenser air inlet/outlet pressure ratio
    !   * P_cycle       [W]     Rated power block capacity
    !   * eta_ref       [-]     Rated gross conversion efficiency
    C   * T_db          [K]     Dry bulb temperature (converted to C)
    !   * P_amb         [Pa]    Atmospheric pressure
    C   * q_reject      [W]     Total required heat rejection load
    !------------------------------------------------------------------------------------------------------------
    !--Output
    C   * m_dot_air     [kg/s]  Total ACC air mass flow rate
    !   * W_dot_fan     [MW]    Total parasitic power for ACC model
    C   * P_cond        [Pa]    Condenser steam pressure
    !   * T_cond        [K]     Condenser steam temperature
    C   * f_hrsys       [-]     Fraction of the cooling system operating
    !------------------------------------------------------------------------------------------------------------
    */
    auto PvsQT = [](double Q /*[-]*/, double T /*[-]*/)
    {
        double a_0 = 147.96619 - 329.021562*T + 183.4601872*pow(T, 2.);
        double a_1 = 71.23482281 - 159.2675368*T + 89.50235831*pow(T, 2.);
        double a_2 = 27.55395547 - 62.24857193*T + 35.57127305*pow(T, 2.);
        double P = a_0 + a_1*Q + a_2*pow(Q, 2.);  // [-]

        return P;
    };
  
    double c_air = 1005.0;				          // [J/kg-K] Specific heat of air, relatively constant over dry bulb range
    const double T_db_des_C = 42.8;               // [C]
    //const double T_hot_diff = 3.0;                // [C] Temperature difference between saturation steam and condenser outlet air temp -> OLD VALUE
    const double T_hot_diff = 1.;                 // [C] Temperature difference between saturation steam and condenser outlet air temp
    const double P_cond_lower_bound_bar = 0.036;  // [bar] Default minimum condenser steam pressure
    double P_cond_min_bar = std::max(P_cond_lower_bound_bar, P_cond_min * 1.e-5);   // [Pa] -> [bar]

    double T_db_K = T_db;                         // [K]
    double T_db_C = T_db_K - 273.15;              // [C]


    // **** Calculations for design conditions
    double Q_rej_des = P_cycle * (1.0 / eta_ref - 1.0);							// Heat rejection from the cycle
    double m_dot_air_des = Q_rej_des / (c_air*(T_ITD_des - T_hot_diff));
    double T = T_db_K / (T_db_des_C + 273.15);
    double P_cond_bar;
    
    if (T >= 0.9) {                             // If T is less than 0.9 fit is not valid
        double Q = q_reject / Q_rej_des;
        double P = PvsQT(Q, T);
        P_cond_bar = P * P_cond_min_bar;
    }
    else {
        P_cond_bar = P_cond_min_bar;
    }
    
    water_state wp;
    double T_cond_K, dT_air;
    if ((P_cond_bar < P_cond_min_bar) && (tech_type != 4)) // No lower limit on Isopentane
    {
        for (size_t i = 2; i <= n_pl_inc; i++)
        {
            f_hrsys = 1.0 - (i - 1.0) / n_pl_inc;
            double Q = q_reject / (Q_rej_des * f_hrsys);
            double P = PvsQT(Q, T);
            P_cond_bar = P * P_cond_min_bar;
            
            if (P_cond_bar > P_cond_min_bar) break;
        }
        if (P_cond_bar <= P_cond_min_bar)
        {
            P_cond_bar = P_cond_min_bar;                // Still below min. fix to min condenser pressure
        }
    }
    else {
        f_hrsys = 1.;
    }

    m_dot_air = m_dot_air_des * f_hrsys;        // [kg/s]
    water_PQ(P_cond_bar * 100., 1.0, &wp);      // [bar] -> [kPa]
    T_cond_K = wp.temp;                         // [K]
    P_cond = P_cond_bar * 1.e5;                 // [bar] -> [Pa]
    T_cond = T_cond_K;

    
    // ===================== Fan Power =================================
    double eta_fan_s = 0.85;                    // [-] Fan isentropic efficiency
    //double eta_fan = pow(0.98, 3.0);            // [-] Fan mechanical efficiency -> OLD VALUE
    double eta_fan = 0.97;          	        // [-] Fan mechanical efficiency
 
    double h_fan_in = CSP::f_h_air_T(T_db_C);	// [J/kg] Fan inlet enthalpy
    const double MM = 28.97;		  			// [kg/kmol] molar mass of air
    double R = 8314.0 / MM;		    			// [J/kg-K] Gas constant for air

    // These temperature calculations are for the isentropic expansion across the fan, not accounting for heat gain in the ACC
    double T_fan_in_K = T_db_K;                                         // [K] Fan inlet temperature
    double T_fan_out_K = T_fan_in_K * pow(P_cond_ratio, (R / c_air));
    double T_fan_out_C = T_fan_out_K - 273.15;                          // [C] Fan outlet temperature

    double dT_fan = T_fan_out_K - T_fan_in_K;                           // [K] Temperature increase in fan
    
    double h_fan_out_s = CSP::f_h_air_T(T_fan_out_C);                   // [J/kg] Isentropic fan outlet enthalpy
    double h_fan_out = h_fan_in + (h_fan_out_s - h_fan_in) / eta_fan_s;	// [J/kg] Actual fan outlet enthalpy

    W_dot_fan = (h_fan_out - h_fan_in)*m_dot_air / eta_fan * 1.0e-6;    // [MW] Fan power
}

void CSP::HybridHR( int tech_type, double P_cond_min, int n_pl_inc, double F_wc, double F_wcmax, double F_wcmin, double T_ITD_des, double T_approach, 
				  double dT_cw_ref, double P_cond_ratio, double P_cycle, double eta_ref, 
				  double T_db, double T_wb, double P_amb, double q_reject, double& m_dot_water, double& W_dot_acfan, 
				  double& W_dot_wctot, double& W_dot_tot, double& P_cond, double& T_cond, double& f_hrsys)
{
	/*
	!------------------------------------------------------------------------------------------------------------
	!This subroutine models a hybrid wet/dry cooling heat rejection system. In this system, a dry-cooled condenser
	!is responsible for rejecting the thermal load, except a supplemental wet-cooled system is placed in parallel
	!to aid in heat rejection during the hottest hours of the day. The wet cooled system can reject heat based
	!on the wetbulb temperature, and thus will have much lower parasitics in rejecting a fraction of the heat than
	!the dry cooled system will, and the dry cooled system running at normal power will result in a lower
	!condenser temperature and pressure.
	!
	!Several assumptions are made in the control of this system. The user can specify a cooling distribution factor
	!on the thermal storage page with the other TOU factors. The fraction indicates what the distribution of
	!the heat rejection load will be. If the fraction is 0.2 for example, then the wet cooling tower will reject
	!20% of the load.
	!
	!The wet-cooling system is a forced-draft tower, and is sized based on the largest TOU fraction supplied in the
	!control array.
	!
	!--Inputs----------------------------------------------------------------------------------------------------
	!   * P_cond_min    [Pa]    Minimum allowable condenser pressure
	!   * n_pl_inc      [-]     Number of part load heat rejection levels
	!   * time          [-]     hour of the year
	!   * F_wc          [-]     Wet cooling fraction
	!   * F_wcmax       [-]     Maximum annual wet cooling fraction
	!   * F_wcmin       [-]     Minimum annual wet cooling fraction
	!   * T_ITD_des     [K]     ACC initial temperature difference, difference between dry bulb and steam inlet temp
	!   * T_approach    [K]     Wet cooling tower approach temperature, difference between cw out and wet bulb temp
	!   * P_cond_ratio  [-]     Condenser air inlet/outlet pressure ratio
	!   * P_cycle       [W]     Rated power block capacity
	!   * eta_ref       [-]     Rated gross conversion efficiency
	!   * T_db          [K]     Dry bulb temperature (converted to C)
	!   * T_wb          [K]     Wet bulb temperature (converted to C)
	!   * P_amb         [Pa]    Atmospheric pressure
	!   * q_reject      [W]     Total required heat rejection load
	!------------------------------------------------------------------------------------------------------------
	!--Output
	!   * m_dot_water   [kg/s]  Total cooling tower water usage
	!   * W_dot_acfan   [MW]    Total parasitic power for ACC fan
	!   * W_dot_wctot   [MW]    Total parasitic power for cooling tower
	!   * W_dot_tot     [MW]    Total overall parasitic power
	!   * P_cond        [Pa]    Condenser steam pressure
	!   * T_cond        [K]     Condenser steam temperature
	!------------------------------------------------------------------------------------------------------------
	*/

	// Values that can be estimated--------
	//-dry
	double T_hot_diff = 3.0;				//[C] Temperature difference between saturation steam and condenser outlet air temp
	double eta_acfan_s = 0.8;				//[-] Fan isentropic efficiency
	double eta_acfan = pow(0.98,3);		//[-] Fan mechanical efficiency
	double C_air = 1005.0;					//[J/kg-K] specific heat of air (This is relatively constant)
	double R = 286.986538;					//[J/kg-K] Gas constant for air = 8314./28.97

	//-wet
	double drift_loss_frac = 0.001;		//Drift loss fraction
	double blowdown_frac = 0.003;			//Blowdown fraction
	double dP_evap = 0.37*1.e5;			//[Pa] Pressure drop across the condenser and cooling tower
	double eta_pump = 0.75;				//Total pump efficiency
	double eta_pcw_s = 0.8;				//Isentropic cooling water pump efficiency
	double eta_wcfan = 0.75;				//Fan mechanical efficiency
	double eta_wcfan_s = 0.8;				//Fan isentropic efficiency
	double P_ratio_wcfan = 1.0025;			//Fan pressure ratio
	double mass_ratio_wcfan = 1.01;		//Ratio of air flow to water flow in the cooling tower

	//**** Calculations for design conditions
	double Q_reject_des = P_cycle*(1.0/eta_ref - 1.0);    	    //Heat rejection from the cycle
	//-dry
	double q_ac_des = Q_reject_des*(1.0 - F_wcmin);    //Size the ACC to always be able to handle the load that isn't going to the wet cooler
	double m_dot_acair_des = q_ac_des/(C_air*(T_ITD_des - T_hot_diff));
	//-wet
	double q_wc_des = Q_reject_des*F_wcmax;			//Size the wet cooler to handle the maximum fraction in the control array
	//c_cw = f_c_psat(P_amb);						//Cooling water specific heat

	//Unit conversions
	T_db = T_db - 273.15;        //[C] Converted dry bulb temp
	T_wb = T_wb - 273.15;
	
	// 1/28/13, twn: replace call to curve fit with call to steam properties routine
	// c_cw = f_c_psat(P_amb);      //Cooling water specific heat (TFF, this is also calculated above.)
	water_state wp;
	water_TP( max(T_wb, 10.0) + 273.15, P_amb/1000.0, &wp );
	double c_cw = wp.cp * 1000.0;		// [J/kg-K]

	double m_dot_cw_des = q_wc_des/(c_cw*dT_cw_ref);	//Mass flow rate of cooling water required to absorb the rejected heat 

	//Calculate the cooling loads
	double q_ac_rej = q_reject*(1.0 - F_wc);
	double q_wc_rej = q_reject*F_wc;
	double f_hrsyswc = 1.0;
	double f_hrsysair = 1.0;

	//-ACC
	double dT_air = q_ac_rej/(m_dot_acair_des * C_air);
	double T_ITD = T_hot_diff + dT_air;  //[C] Calculate the actual ITD during off-design operation
	//-WC
	double DeltaT_cw = q_wc_rej/(m_dot_cw_des * c_cw);

	//***Calculated output
	//Condensation temperature is the maximum of either the wet or dry system cooling stream outlet temperature (plus hot side dT)
	double T_condwc = T_wb + DeltaT_cw + T_hot_diff + T_approach;
	double T_condair = T_db + T_ITD;
	if (F_wc > 0.0) //MJW 7.23.2010
		T_cond = max(T_condwc, T_condair);
	else
		T_cond = T_condair;

	if(tech_type != 4)
	{	
		// 1/28/13, twn: replace call to curve fit with call to steam properties routine
		// P_cond =  f_psat_T(T_cond); // steam
		water_TQ(T_cond + 273.15, 1.0, &wp);
		P_cond = wp.pres * 1000.0;
	}
	else
		P_cond = CSP::P_sat4(T_cond); // isopentane

	// MJW 7.19.2010 :: Cooling system part-load strategy uses the number of part-load increments to determine how the coolign system is
	// partially shut down during under-design operation. The condenser pressure is reduced with the cooling system running
	// at full load until it reaches the minimum condenser pressure. The cooling system then incrementally shuts off bays until
	// the condenser temperature/pressure rise above their minimum level. Default cond. pressure is 2.0 inHg (6772 Pa).
	int i=1; int j=1;
	double m_dot_acair = m_dot_acair_des;
	double m_dot_cw = m_dot_cw_des;
	if ( (P_cond < P_cond_min) && (tech_type != 4) ) // Aug 3, 2011: No lower limit on Isopentane
	{
		do
		{
			if(T_condwc > T_condair)
			{
				i++;
				//Reduce just wet cooled
				f_hrsyswc = (1.0 - (float)((i-1.0)/n_pl_inc));
				double m_dot_cw = m_dot_cw_des*f_hrsyswc;
				DeltaT_cw = q_wc_rej/(m_dot_cw*c_cw);
				T_condwc = T_wb + DeltaT_cw + T_hot_diff + T_approach;
			}
			else
			{
				i++;
				j++;
				//Reduce both wet and dry cooled
				f_hrsysair = (1.0 - (float)((j-1.0)/n_pl_inc));
				double m_dot_acair = m_dot_acair_des*f_hrsysair;
				dT_air = q_ac_rej/(m_dot_acair*C_air);
				T_condair = T_db + dT_air + T_hot_diff;
				//--
				f_hrsyswc = (1.0 - (float)((i-1.0)/n_pl_inc));
				double m_dot_cw = m_dot_cw_des*f_hrsyswc;
				DeltaT_cw = q_wc_rej/(m_dot_cw*c_cw);
				T_condwc = T_wb + DeltaT_cw + T_hot_diff + T_approach;
			}

			if(F_wc > 0.0) //MJW 7.23.2010
				T_cond = max(T_condwc, T_condair);
			else
				T_cond = T_condair;
			
			// 1/28/13, twn: replace call to curve fit with call to steam properties routine
			// P_cond = f_psat_T(T_cond);
			water_TQ(T_cond + 273.15, 1.0, &wp);
			P_cond = wp.pres * 1000.0;

			//if(P_cond > P_cond_min) goto 100
			if((i >= n_pl_inc) || (j >= n_pl_inc) ) break;

		} while (P_cond < P_cond_min);

		if (P_cond <= P_cond_min)
		{
			//Still below min. fix to min condenser pressure and recalc. temp.
			P_cond = P_cond_min;
			
			// 1/28/13, twn: replace call to curve fit with call to steam properties routine
			// T_cond = f_Tsat_p(P_cond);
			water_PQ( P_cond/1000.0, 1.0, &wp );
			T_cond = wp.temp-273.15;
			
			if(T_condwc > T_condair)
			{
				DeltaT_cw = T_cond - (T_wb + T_hot_diff + T_approach);
				m_dot_cw = q_reject/(DeltaT_cw*c_cw);
			}
			else
			{
				dT_air = T_cond - (T_db + T_hot_diff);
				m_dot_acair = q_reject/(dT_air*C_air);
			}
		}
	}

//100	f_hrsys = (f_hrsyswc + f_hrsysair)/2;
	f_hrsys = (f_hrsyswc + f_hrsysair)/2.0;

	//-----ACC Fan power---------
	double h_acfan_in = f_h_air_T(T_db);  //[J/kg] Fan inlet enthalpy

	//These temperature calculations are for the isentropic expansion across the fan, not accounting for heat gain in the ACC
	double T_acfan_in_K = T_db + 273.15;  //[K] Fan inlet temperature
	double T_acfan_out_K = T_acfan_in_K * pow(P_cond_ratio,(R/C_air));
	double T_acfan_out = T_acfan_out_K - 273.15;    //[C] Fan outlet temperature
	//double dT_acfan = T_acfan_out - T_db;   //[C] Difference in temperature including irreversibilities in fan

	double h_acfan_out_s = f_h_air_T(T_acfan_out);	//[J/kg] Isentropic fan outlet temperature
	double h_acfan_out = h_acfan_in + (h_acfan_out_s - h_acfan_in)/eta_acfan_s;   //[J/kg] Actual fan outlet temperature
	//Total ACC parasitic power
	W_dot_acfan = (h_acfan_out - h_acfan_in) * m_dot_acair/eta_acfan*1.e-6;  //[MW] Fan power


	//-----Wet cooling parasitics --------
	if(q_wc_rej > 0.001)
	{
		//Circulating water pump power
		
		// 1/28/13, twn: replace call to curve fit with call to steam properties routine
		// h_pcw_in = f_hw_psat(P_amb);     //[J/kg] cw pump inlet enthalpy
		// s_pcw_in = f_s_hw_psat(P_amb);     //[J/kg-K] cw pump inlet entropy
		// rho_cw = f_rho_P(P_amb);         //[kg/m3] cooling water density in the pump
		water_TP( T_cond - 3.0 + 273.15, P_amb/1000.0, &wp );
		double h_pcw_in = wp.enth * 1000.0;
		//double s_pcw_in = wp.entr * 1000.0;
		double rho_cw = wp.dens;
		
		double h_pcw_out_s = dP_evap/rho_cw + h_pcw_in;                         //[J/kg] isentropic outlet enthalpy.. incompressible fluid
		double h_pcw_out = h_pcw_in + (h_pcw_out_s - h_pcw_in)/eta_pcw_s;       //[J/kg] Outlet enthalpy accounting for irreversibility
		double W_dot_cw_pump = (h_pcw_out - h_pcw_in)*m_dot_cw/eta_pump*1.e-6;  //[MW] Cooling water circulating pump power

		//Fan power
		double m_dot_wcair = m_dot_cw*mass_ratio_wcfan;
		double T_wcfan_in = (T_db + T_wb + T_approach)/2.0;
		double h_wcfan_in = f_h_air_T(T_wcfan_in);

		double T_wcfan_in_K = T_wcfan_in + 273.15;  //Fan inlet temp, in K
		double T_wcfan_out_K = T_wcfan_in_K * pow(P_ratio_wcfan,(R/C_air));    //[K] isentropic temperature rise
		double T_wcfan_out = T_wcfan_out_K - 273.15;    //[C] Convert isentropic temperature rise to deg C
		double h_wcfan_out_s = f_h_air_T(T_wcfan_out);  //[J/kg] Calculate isentropic enthalpy at fan outlet
		double h_wcfan_out = h_wcfan_in + (h_wcfan_out_s - h_wcfan_in)/eta_wcfan_s;   //[J/kg] Actual enthalpy, accounting for irreversibility

		double W_dot_wcfan = (h_wcfan_out - h_wcfan_in)*m_dot_wcair/eta_wcfan*1.0E-6;  //[MW] Fan parasitic power

		//Total wet cooling tower parasitic power
		W_dot_wctot = W_dot_cw_pump + W_dot_wcfan;   //[MW]

		//Enthalpy of evaporation
		// 1/28/13, twn: replace call to curve fit with call to steam properties routine
		// deltaH_evap = f_dh_evap(P_amb);
		water_PQ( P_amb/1000.0, 0.0, &wp );
		double dh_low = wp.enth;
		water_PQ( P_amb/1000.0, 1.0, &wp );
		double dh_high = wp.enth;
		double deltaH_evap = (dh_high - dh_low)*1000.0;

		//Evaporative water loss
		double m_dot_evap = q_wc_rej/deltaH_evap;

		//Other water losses
		double m_dot_drift = drift_loss_frac*m_dot_cw;	//Drift loss fraction, based on cooling water mass flow rate
		double m_dot_blowdown = blowdown_frac*m_dot_cw;	//Blow down fraction

		//Total power block water usage
		m_dot_water = m_dot_evap + m_dot_drift + m_dot_blowdown;
		}
	else
	{
		//Otherwise set the wet-cooling outputs to zero
		m_dot_water = 0.0;
		W_dot_wctot = 0.0;
	}

	W_dot_tot = W_dot_wctot + W_dot_acfan;

	//Unit conversions
	T_db = T_db + 273.15;    //[C] Converted dry bulb temp
	T_wb = T_wb + 273.15;    //[C] Converted wet bulb temp
	T_cond = T_cond + 273.15;    //[K] Convert to K for output
}


// Surface condenser calculations for once through cooling ARD
void CSP::surface_cond(int tech_type, double P_cond_min, int n_pl_inc, double DeltaT_cw_des, double T_approach, double P_cycle,
	double eta_ref, double T_db_K, double T_wb_K, double P_amb, double T_cold, double q_reject, double &m_dot_water,
	double &W_dot_tot, double &P_cond, double &T_cond, double &f_hrsys, double &T_cond_out)
{
	/*
	double c_air, c_cw, deltah_evap, deltat_cw, dp_evap, drift_loss_frac, dt_out, eta_fan, eta_fan_s,
	eta_pcw_s, eta_pump, h_fan_in, h_fan_out, h_fan_out_s, h_pcw_in, h_pcw_out,
	h_pcw_out_s, m_dot_air, m_dot_blowdown, m_dot_cw, m_dot_cw_des, m_dot_drift, blowdown_frac,
	m_dot_evap, mass_ratio_fan, p_ratio_fan, q_reject_des, R, rho_cw, s_pcw_in, t_fan_in,
	t_fan_in_k, t_fan_out, t_fan_out_k, w_dot_cw_pump, w_dot_fan;*/
	/*
	!------------------------------------------------------------------------------------------------------------
	!--Inputs
	!   * P_cond_min    [Pa]    Minimum allowable condenser pressure
	!   * n_pl_inc      [-]     Number of part load heat rejection levels
	!   * DeltaT_cw_des [K]     Cooling water temperature rise across condenser
	!   * T_approach    [K]     Cooling tower approach temperature, difference between cw out and wet bulb temp
	!   * P_cycle       [W]     Rated power block capacity
	!   * eta_ref       [-]     Rated gross conversion efficiency
	!   * T_db          [K]     Dry bulb temperature (converted to C)
	!   * P_amb         [Pa]    Atmospheric pressure
	!	* T_cold		[C]		Cold storage temperature (inlet to condenser)
	!------------------------------------------------------------------------------------------------------------
	!--Output
	!   * m_dot_water   [kg/s]  Total cooling tower water usage
	!   * W_dot_tot     [MW]    Total parasitic power for cooling tower model
	!   * P_cond        [Pa]    Condenser steam pressure
	!   * T_cond        [K]     Condenser steam temperature
	!   * f_hrsys       [-]     Fraction of the cooling system operating
	!	* T_cond_out	[C]		Condenser water outlet temperature
	!------------------------------------------------------------------------------------------------------------
	*/

	// Unit conversions
	double T_db = T_db_K - 273.15;    //[C] Converted dry bulb temp
	double T_wb = T_wb_K - 273.15;    //[C] Converted wet bulb temp

	// Values that can be estimated
	double dt_out = 3.0;				// Temperature difference at hot side of the condenser
	double drift_loss_frac = 0.001;    // Drift loss fraction
	double blowdown_frac = 0.003;      // Blowdown fraction
	double dp_evap = 0.37*1.0e5;       // [Pa] Pressure drop across the condenser and cooling tower
	double eta_pump = 0.75;            // Total pump efficiency
	double eta_pcw_s = 0.8;            // Isentropic cooling water pump efficiency
	double eta_fan = 0.75;             // Fan mechanical efficiency
	double eta_fan_s = 0.8;            // Fan isentropic efficiency
	double p_ratio_fan = 1.0025;       // Fan pressure ratio
	double mass_ratio_fan = 1.01;      // Ratio of air flow to water flow in the cooling tower

	// Cooling water specific heat
	water_state wp;
	water_TP(max(T_cold, 10.0) + 273.15, P_amb / 1000.0, &wp);
	double c_cw = wp.cp * 1000.0;		// Convert to J/kg-K

	// **** Calculations for design conditions
	double q_reject_des = P_cycle*(1. / eta_ref - 1.0);    	    // Heat rejection from the cycle
	double m_dot_cw_des = q_reject_des / (c_cw*DeltaT_cw_des);	// Mass flow rate of cooling water required to absorb the rejected heat
	f_hrsys = 1.0;   // Initial fraction of cooling system operating

	// **** Calculations for performance
	// Calculate the cooling water temp. rise associated with normal cooling system operation
	double m_dot_cw = m_dot_cw_des;
	double deltat_cw = q_reject / (m_dot_cw*c_cw);

	// Condenser saturation temperature
	T_cond = T_cold + deltat_cw + dt_out; // celcius

	// Condenser back pressure
	if (tech_type != 4)
	{
		water_TQ(T_cond + 273.15, 1.0, &wp);
		P_cond = wp.pres * 1000.0;
	}
	else
		P_cond = CSP::P_sat4(T_cond); // isopentane


	// MJW 7.19.2010 :: Cooling system part-load strategy uses the number of part-load increments to determine how the coolign system is
	// partially shut down during under design operation. The condenser pressure is reduced with the cooling system running
	// at full load until it reaches the minimum condenser pressure. The cooling system then incrementally shuts off bays until
	// the condenser temperature/pressure rise above their minimum level. Default cond. pressure is 1.25 inHg (4233 Pa).
	if ((P_cond < P_cond_min) && (tech_type != 4)) // Aug 3, 2011: No lower limit on Isopentane
	{
		for (int i = 2; i <= n_pl_inc; i++)
		{
			f_hrsys = (1.0 - (float)((i - 1.0) / n_pl_inc));
			m_dot_cw = m_dot_cw_des*f_hrsys;
			deltat_cw = q_reject / (m_dot_cw*c_cw);
			T_cond = T_cold + deltat_cw + dt_out;

			water_TQ(T_cond + 273.15, 1.0, &wp);
			P_cond = wp.pres * 1000.0;

			if (P_cond > P_cond_min) break;
		}
		if (P_cond <= P_cond_min)
		{
			// Still below min. fix to min condenser pressure and recalc. temp.

			P_cond = P_cond_min;

			water_PQ(P_cond / 1000.0, 1.0, &wp);
			T_cond = wp.temp - 273.15;

			deltat_cw = T_cond - (T_cold + dt_out);
			m_dot_cw = q_reject / (deltat_cw * c_cw);
		}
	}
	water_TP(T_cond - 3.0 + 273.15, P_amb / 1000.0, &wp);
	double h_pcw_in = wp.enth*1000.0;
	//double s_pcw_in = wp.entr*1000.0;
	double rho_cw = wp.dens;

	double h_pcw_out_s = (dp_evap / rho_cw) + h_pcw_in;								// [J/kg] isentropic outlet enthalpy.. incompressible fluid
	double h_pcw_out = h_pcw_in + ((h_pcw_out_s - h_pcw_in) / eta_pcw_s);			// [J/kg] Outlet enthalpy accounting for irreversibility
	double w_dot_cw_pump = (h_pcw_out - h_pcw_in) * m_dot_cw / eta_pump * 1.0E-6;	// [MW] Cooling water circulating pump power

	T_cond_out = T_cond - dt_out;	// [C] Return the condenser cooling water outlet temperature
	

	// Total cooling tower parasitic power
	W_dot_tot = w_dot_cw_pump;   // [MW]


	// Total power block water usage
	m_dot_water = 0;

	// Unit conversions
	T_db = T_db + 273.15;		// [C] Converted dry bulb temp (TFF - I think this is irrelevant, since it's not passed back out)
	T_wb = T_wb + 273.15;		// [C] Converted wet bulb temp (TFF - I think this is irrelevant, since it's not passed back out)
	T_cond = T_cond + 273.15;	// [K] Convert to K for output
}

double CSP::eta_pl(double mf)
{
	return 1.0 - (0.191 - 0.409*mf + 0.218*pow(mf,2.0));	// This number should be multiplied by the design-point isen. eff to get the final.
}

double CSP::pipe_sched(double De, bool selectLarger)
{
    /***************************************************************************************************
    This function takes a piping diameter "De" [m] and locates the appropriate pipe schedule
    from a list of common pipe sizes. The function always returns the pipe schedule equal to or
    immediately larger than the ideal diameter De.
    The pipe sizes are selected based on the assumption of a maximum hoop stress of 105 MPa and a total
    solar field pressure drop of 20 Bar. The sizes correspond to the pipe schedule with a wall thickness
    sufficient to match these conditions. For very large pipe diameters (above 42in), no suitable schedule
    was found, so the largest available schedule is applied.
    Data and stress calculations were obtained from Kelly & Kearney piping model, rev. 1/2011.
    */

    double D_m[] = { 0.01855, 0.02173, 0.03115, 0.0374, 0.04375, 0.0499, 0.0626,
        0.06880860, 0.08468360, 0.1082040, 0.16146780, 0.2063750, 0.260350, 0.311150, 0.33975040,
        0.39055040, 0.438150, 0.488950, 0.53340, 0.58420, 0.6350, 0.679450, 0.730250, 0.781050,
        0.82864960, 0.87630, 1.02870, 1.16840, 1.32080, 1.47320, 1.62560, 1.7780,
        1.8796, 1.9812, 2.1844, 2.286 };
    int np = sizeof(D_m) / sizeof(D_m[0]);

    if (selectLarger) {
        //Select the smallest real pipe diameter greater than the design diameter provided
        for (int i = 0; i < np; i++) {
            if (D_m[i] >= De) return D_m[i];
        }
    }
    if (!selectLarger) {
        //Select the smallest real pipe diameter less than the design diameter provided
        for (int i = np - 1; i >= 0; i--) {
            if (D_m[i] <= De) return D_m[i];
        }
    }

    //Nothing was found, so return the exact pipe diameter instead
    double mtoinch = 39.3700787;
    char buffer[512];
    sprintf(buffer, "No suitable pipe schedule found for this plant design. Looking for a schedule above %.2f in ID. "
        "Maximum schedule is %.2f in ID. Using the exact pipe diameter instead."
        "Consider increasing the header design velocity range or the number of field subsections.",
        De*mtoinch, D_m[np - 1] * mtoinch);
    //throw std::invalid_argument(buffer);
    return De;  //mjw 10/10/2014 - NO! ---> std::numeric_limits<double>::quiet_NaN();
}

double CSP::WallThickness(double d_in) {
    // regression of Wagner-2011 Table 9 values with an R^2=1.000
    return 0.0194*d_in;
}

double CSP::MinorPressureDrop(double vel, double rho, double k) {
    return k * (vel * vel) * rho / 2;
}

double CSP::MajorPressureDrop(double vel, double rho, double ff, double l, double d) {
    // Darcy Weisbach pressure drop for an incompressible fluid using a Darcy friction factor
    if (d <= 0) throw std::invalid_argument("The inner diameter must be greater than 0.");
    if (vel == 0) return 0;      // handles cases where ff = inf because vel and thus Re = 0

    return ff * (vel * vel) * l * rho / (2 * d);
}

double CSP::FrictionFactor(double rel_rough, double Re) {
    if (Re < 2100) {  // laminar flow
        return 64 / Re;
    }
    else if (Re < 4000) {  // transitional flow
                           // see Cheng, N.S. 2008 "Formulas for friction factors in transitional regions" for non-iterative solution
        return CSP::FricFactor_Iter(rel_rough, Re);
    }
    else {  // turbulent flow
            // Calculates according to Zigrang, Sylvester 1982 for Re = 4e3 to 1e8 and e/D = 4e-5 to 5e-2
        return pow(-2.0*log10(rel_rough / 3.7 - 5.02 / Re * log10(rel_rough / 3.7 - 5.02 / Re *
            log10(rel_rough / 3.7 + 13.0 / Re))), -2);
    }
}

double CSP::FricFactor_Iter(double rel_rough, double Re) {
    // Uses an iterative method to solve the implicit Colebrook friction factor function.
    // Taken from Piping loss model

    double Test, TestOld, X, Xold, Slope;
    double Acc = .01; //0.0001
    int NumTries;

    if (Re < 2750.) {
        return 64. / std::max(Re, 1.0);
    }

    X = 33.33333;  //1. / 0.03
    TestOld = X + 2. * log10(rel_rough / 3.7 + 2.51 * X / Re);
    Xold = X;
    X = 28.5714;  //1. / (0.03 + 0.005)
    NumTries = 0;

    while (NumTries < 21) {
        NumTries++;
        Test = X + 2 * log10(rel_rough / 3.7 + 2.51 * X / Re);
        if (fabs(Test - TestOld) <= Acc) {
            return 1. / (X * X);
        }

        Slope = (Test - TestOld) / (X - Xold);
        Xold = X;
        TestOld = Test;
        X = std::max((Slope * X - Test) / Slope, 1.e-5);
    }

    //call Messages(-1," Could not find friction factor solution",'Warning',0,250) 
    return 0;
}

//template <typename T, typename A>
//T mode(std::vector<T,A> const& v) {
double mode(std::vector<double> v) {
    if (v.size() == 0) { throw C_csp_exception("Vector size cannot be 0 for mode calculation."); }
    if (v.size() == 1) { return v[0]; }

    std::sort(v.begin(), v.end());
    //T mode = v[0];
    double mode = v[0];
    std::size_t new_count = 1;   // starting at second element so counting first element here
    std::size_t mode_count = 0;
    //for (std::vector<T>::iterator it = v.begin() + 1; it != v.end(); ++it) {
    for (std::vector<double>::iterator it = v.begin() + 1; it != v.end(); ++it) {
        if (*it == *(it - 1)) {
            new_count++;
        }
        else {  // next unique value seen
            if (new_count > mode_count) {
                mode = *(it - 1);
                mode_count = new_count;
            }
            new_count = 1;
        }
    }
    // handle last value
    if (new_count > mode_count) {
        mode = *(v.end() - 1);
        mode_count = new_count;
    }

    return mode;
}

//double CSP::Re(double rho, double vel, double d, double mu) {
//// Reynold's Number
//    return rho * vel * d / mu;
//}


void P_max_check::set_P_max( double P_max_set )
{
	P_max = P_max_set;		//[bar]
	P_save = 0.0;
	is_error = false;

	return;
}

void P_max_check::report_and_reset()
{
	if( is_error )
	{
		// Write message
	}

	P_save = 0.0;
	is_error = false;

	return;
}

double P_max_check::P_check( double P )
{
	
	//Check the pressure, store the highest value
    
	if( P > P_max )
	{
		is_error = true;
		if( P > P_save )
			P_save = P;
		return P_max;
	}
	
	return P;
}

void enth_lim::set_enth_limits( double h_min_in, double h_max_in )
{
	h_min = h_min_in;
	h_max = h_max_in;
	return;
}

double enth_lim::check( double h_in )
{
	if( h_in > h_max )
		return h_max;
	if( h_in < h_min )
		return h_min;
	else
		return h_in;
}

bool CSP::flow_patterns( int n_panels, int crossover_shift, int flow_type, int & n_lines, util::matrix_t<int> & flow_pattern, std::string *messages )
{
	/* !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	! This subroutine takes the number of panels, the requested flow type, and 
	! returns the corresponding flow pattern (the order of panels through which the
	! WF passes, this code is modified from the version in Type222
	!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */
	
    if(n_panels % 2 != 0){
        if( messages )
            messages->append("The number of panels must be divisible by 2. Simulation initialization failed.\n");
        return false;
    }
    
	//int n_p_quarter = n_panels/4;

	switch( flow_type )
	{
	case 1:
    {
		/* This flow pattern begins at the northmost 2 panels, splits into 2 flows, and crosses over
		at the quarter position, exiting in 2 flows on the southmost 2 panels. This is the flow
		configuration that was used for SOLAR II
		!Example = [13,14,15,16,17,18,6,5,4,3,2,1] [12,11,10,9,8,7,19,20,21,22,23,24] 

        If the number of panels is not divisible by 4, the pre-crossover sections have 1 more panel 
        than the post-crossover sections.
        Example = [7, 6, 5, 4, <> 12, 13, 14] [8, 9, 10, 11, <> 3, 2, 1]
        
        */
		n_lines = 2;
		flow_pattern.resize( n_lines, n_panels/n_lines );

        double npq = (double)n_panels/4.;

        int nq1, nq2;
        if(n_panels % 4 != 0){
            nq2 = (int)floor(npq);
            nq1 = nq2 + 1;
            if(messages)
                messages->append("Flow Path Setup: Allocating " + util::to_string(nq1) + "panels to flow quadrant 1 and " 
                                 + util::to_string(nq2) + " panels to flow quadrant 2.\n");
        }
        else{
            nq2 = nq1 = (int)floor(npq + 1.e-6);
        }
        
        nq1 += crossover_shift;
        nq2 += -crossover_shift;

        if( nq2 < 1 )
            throw C_csp_exception("Invalid flowpath crossover shift has been specified; at least 1 panel must remain in each quadrant.");

        int nhalf = n_panels/n_lines;

        for( int i=0; i<nq1; i++){
			flow_pattern.at( 0, i ) = nhalf + i;						// NE Quadrant - first half of flow path 0
            flow_pattern.at( 1, i ) = nhalf - 1 - i;					// NW Quadrant - first half of flow path 1
        }
        for( int i=0; i<nq2; i++){
			flow_pattern.at( 0, nq1 + i) = nq2 - 1 - i;	        		// SW Quadrant - final half of flow path 0
			flow_pattern.at( 1, nq1 + i) = n_panels - nq2 + i;			// SE Quadrant - final half of flow path 1
        }

        return true;
    }
	case 2:
    {
		 /* This flow pattern is the same as flow pattern #1, but in reverse. The salt enters
		on the 2 southmost panels, crosses over, and exits on the 2 northmost panels.
		Example = [1,2,3,4,5,6,17,16,15,14,13,12] [24,23,22,21,20,19,18,7,8,9,10,11,12] 
        
        If the number of panels is not divisible by 4, the post-crossover sections have 1 more panel 
        than the pre-crossover sections.
        Example = [1, 2, 3 <> 11, 10, 9, 8] [14, 13, 12, <> 4, 5, 6, 7]
        */
		n_lines = 2;
		flow_pattern.resize( n_lines, n_panels/n_lines );
        
        double npq = (double)n_panels/4.;

        int nq1, nq2;
        if(n_panels % 4 != 0){
            nq1 = (int)floor(npq);
            nq2 = nq1 + 1;
            if(messages)
                messages->append("Flow Path Setup: Allocating " + util::to_string(nq1) + " panels to flow quadrant 1 and " 
                                 + util::to_string(nq2) + " panels to flow quadrant 2.\n");
        }
        else{
            nq2 = nq1 = (int)floor(npq + 1.e-6);
        }

        nq1 += crossover_shift;
        nq2 += -crossover_shift;

        if( nq2 < 1 )
            throw C_csp_exception("Invalid flowpath crossover shift has been specified; at least 1 panel must remain in each quadrant.");

        int nhalf = n_panels/n_lines;
        for( int i=0; i<nq1; i++){
			flow_pattern.at( 0, i ) = i;			        			// SW Quadrant - first half of flow path 0
            flow_pattern.at( 1, i ) = n_panels - 1 - i;					// SE Quadrant - first half of flow path 1
        }
        for( int i=0; i<nq2; i++){
			flow_pattern.at( 0, nq1 + i) = nhalf + nq2 - 1 - i;			// NE Quadrant - second half of flow path 0
			flow_pattern.at( 1, nq1 + i) = nq1 + i;	        		    // NW Quadrant - second half of flow path 1
        }

		return true;
    }
	case 3:
		/* This flow pattern has 2 separate flows that enter in 2 of the northmost panels
		  and flow around (without crossing over), exiting at the 2 southmost panels */
		n_lines=2;
		flow_pattern.resize( n_lines, n_panels/n_lines );

		for( int i = 0; i < n_panels/2; i++ )
		{
			flow_pattern.at( 0, i ) = n_panels/2 - 1 - i;
			flow_pattern.at( 1, i ) = n_panels/2 + i;
		}
		return true;
	case 4:
		/* This flow pattern has 2 separate flows that enter in 2 of the southmost panels
			and flow around (without crossing over), exiting at the 2 northmost panels */
		n_lines = 2;
		flow_pattern.resize( n_lines, n_panels/n_lines );

		for( int i = 0; i < n_panels/2; i++ )
		{
			flow_pattern.at( 0, i ) = i;
			flow_pattern.at( 1, i ) = n_panels - 1 - i;
		}
		return true;
	case 5:
		/* This flow type enters on a panel at the southmost side of the receiver,
			travels completely around the receiver in a clockwise direction,
			and exits again on the south side */
		n_lines = 1;
		flow_pattern.resize( n_lines, n_panels/n_lines );

		for( int i = 0; i < n_panels; i++ )
		{
			flow_pattern.at( 0, i ) = i;
		}
		return true;
	case 6:
		/* This flow type enters on a panel at the southmost side of the receiver,
			travels completely around the receiver in a counter-clockwise direction,
			and exits again on the south side */
		n_lines = 1;
		flow_pattern.resize( n_lines, n_panels/n_lines );

		for( int i = 0; i < n_panels; i++ )
			flow_pattern.at( 0, i ) = n_panels - 1 - i;
		return true;
	case 7:
		/* This flow type enters on a panel at the northmost side of the receiver,
			travels completely around the receiver in a clockwise direction,
			and exits again on the north side */
		n_lines = 1;
		flow_pattern.resize( n_lines, n_panels/n_lines );

		for( int i = 0; i < n_panels; i++ )
			flow_pattern.at( 0, i ) = n_panels/2 + i - i/(n_panels/2)*n_panels;
		return true;
	case 8:
		/* This flow type enters on a panel at the northmost side of the receiver,
			travels completely around the receiver in a counter-clockwise direction,
			and exits again on the north side */
		n_lines = 1;
		flow_pattern.resize( n_lines, n_panels/n_lines );

		for( int i = 0; i < n_panels; i++ )
			flow_pattern.at( 0, i ) = n_panels/2 - 1 - i + i/(n_panels/2)*n_panels;
		return true;
    case 9:
        /* This flow type has parallel flow paths with 2 panels in each path.
            */
        size_t n_panels_per_path = 2;
        n_lines = n_panels / n_panels_per_path;     //[-]
        flow_pattern.resize(n_lines, n_panels_per_path);

        size_t i_start = n_panels/2 - (int)std::floor(n_panels/4);

        for (size_t i = 0; i < n_lines; i++)
        {
            flow_pattern(i, 0) = (i_start + i) % n_panels;
            flow_pattern(i, 1) = (i_start + i + n_lines) % n_panels;
        }

        return true;
	};
	return false;
}

/******************************************************************************************************************************
	FUNCTION fq_34conv :	Convective heat transfer rate between the absorber outer surface and the glazing inner surface
******************************************************************************************************************************"
  NOTE: Temperatures input in terms of degrees K

  Author: R.E. Forristall (2003, EES)
  Implemented and revised:  M.J. Wagner (10/2009)
				Copyright:  National Renewable Energy Lab (Golden, CO) 2009

{ Four cases:

	1. Vacuum in annulus: free-molecular heat transfer model for an annulus.
	2. Low or lost vacuum: natural convection heat transfer model for an annulus.
	3. No glazing, no wind: natural convection heat transfer model for a horizontal cylinder.
	4. No glazing, with wind: forced convection heat transfer model for a horizontal cylinder.


Case 1:

	Free-molecular heat transfer for an annular space between horizontal cylinders.

		q' = D_i * PI * h * (T_i - T_o)		
		h = k_gas / (D_i / 2 * ln(D_o / D_i) + b * Lambda * (D_i / D_o + 1))
		b = (2 - a) / a * (9 * Gamma - 5) / (2 * (Gamma + 1))
		Lambda = 2.331 * 10^(-20) * T_avg / (P * Delta^2)

	Where

		q' = convection heat transfer rate per unit length [W/m]
		D_i = outer absorber diameter [m]
		D_o = inner glazing diameter [m]
		h = convection heat transfer coefficient for annulus gas [W/m^2-K]
		T_i = outer absorber surface temperature [C]
		T_o = inner glazing surface temperature [C]
		k_gas = thermal conductivity of the annulus fluid at standard temperature and pressure [W/m^2-K]
		b = interaction coefficient [dimensionless]
		Lambda = mean-free-path between collisions of a molecule [cm]
		a = accommodation coefficient [dimensionless]
		Gamma = ratio of specific heats for the annulus fluid [dimensionless]
		T_avg = average temperature of the annulus fluid [K]
		P = pressure of the annulus gas [mm of Hg]
		Delta = molecular diameter of the annulus gas [cm]

	The above correlation is valid for Ra_Do < (D_o / (D_o -D_i))^4, but may over estimate q' slightly for large vacuums.

(Source: Ratzel, A., Hickox, C., Gartling, D., "Techniques for Reducing Thermal Conduction and Natural Convection Heat Losses 
 in Annular Receiver Geometries," Journal of Heat Transfer, Vol. 101, No. 1, February 1979; pp. 108-113)


Case 2:

	Modified Raithby and Hollands correlation for natural convection in an annular space between horizontal cylinders.

		q' = 2.425 * k * (T_i - T_o) / (1 + (D_i / D_o)^(3/5))^(5/4) * (Pr * Ra_Di / (0.861 + Pr))^(1/4)
		Pr = NU / Alpha
		Ra_Di = g * Beta * (T_i - T_o) * (D_i)^3 / (Alpha * NU)
		Beta = 1 / T_avg		"Ideal Gas"

	Where

		k = conduction heat transfer coefficient for the annulus gas [W/m-K]
		Pr = Prandtl number 
		NU = kinematic viscosity [m^2/s]
		Alpha = thermal diffusivity [m^2/s]
		Ra_Di = Rayleigh number based on the annulus inner diameter
		g = local acceleration due to gravity [m/s^2]
		Beta = volumetric thermal expansion coefficient [1/K]
		Rho_o = annulus gas density at the outer surface [kg/m^3]
		Rho_i = annulus gas density at the inner surface [kg/m^3]
		T_avg = average temperature, (T_i + T_o) / 2 [K]

	Above correlation is valid for Ra_Do > (D_o / (D_o -D_i))^4. All physical properties are evaluated at the average temperature, (T_i + T_o)/2.

(Source: Bejan, A., Convection Heat Transfer, Second Edition; John Wiley & Son's, New York, 1995, pp. 257-259.)


Case 3:

	Churchill and Chu correlation for natural convection from a long isothermal horizontal cylinder.

		Nu_bar = (0.60 + (0.387 * Ra_D^(1/6)) / (1 + (0.559 / Pr)^(9/16))^(8/27) )^2
		Ra_D = g * Beta * (T_s - T_inf) * D^3 / (Alpha * NU)
		Beta =  1 / T_f	"Ideal Gas"
		Alpha = k / (Cp * Rho)
		Pr = NU / Alpha

		h = Nu_bar * k / D

		q' = h * PI * D * (T_s - T_inf)

	Where

		Nu_bar = average Nusselt number
		Ra_D = Rayleigh number based on diameter
		Rho = fluid density  [kg/m^3]
		Cp = specific heat at constant pressure [kJ / kg-K]
		T_inf = fluid temperature in the free stream [C]
		T_s = surface temperature [C]
		T_f = film temperature, (T_s + T_inf) / 2 [K]
		T_inf = ambient air temperature [C]

	Above correlation is valid for  10^(-5) < Ra_D < 10^12. All physical properties are evaluated at the film temperature, (T_s + T_inf) / 2.

(Source: Incropera, F., DeWitt, D., Fundamentals of Heat and Mass Transfer, Third Edition; John Wiley and Sons, New York, 1981, pp. 550-552.)


Case 4:

	Zhukauskas's correlation for external forced convection flow normal to an isothermal cylinder.

		Nu_bar = C * Re_D^m * Pr^n * (Pr / Pr_s)^(1/4)

		Re_D		C			m
		1-40		0.75			0.4
		40-1000		0.51			0.5
		1e3- 2e5	0.26			0.6
		2e5-1e6	0.076			0.7

		n = 0.37, Pr <=10
		n = 0.36, Pr >10

		Re_D =  U_inf * D / NU
		Pr  = NU / Alpha
		Alpha = k / (Cp * Rho)

		Q =  h * D * PI * (T_s - T_inf) * L

	Where,

		Re_D = Reynolds number evaluated at the diameter
		Cp = specific heat at constant pressure of air [W/m-K]
		Rho = density of air [kg/m^3]
		C, m, n = constants

	Above correlation is valid for  0.7 < Pr < 500, and 1 < Re_D < 10^6. All physical properties evaluated 
   at the free stream temperature, T_inf,  except Pr_s.

(Source: Incropera, F., DeWitt, D., Fundamentals of Heat and Mass Transfer, Third Edition; John Wiley and 
 Sons, New York, 1981, p. 413.)
}*/
//subroutine FQ_34CONV(T_3,T_4, D_3, D_4, P_a, P_6, v_6, T_6, annulusGas, glazingIntact, q_34conv, h_34)
void Evacuated_Receiver::FQ_34CONV(double T_3, double T_4, double P_6, double v_6, double T_6, int hn, int hv, double &q_34conv, double &h_34)
{
   //      UNITS   ( K , K ,  Pa , m/s,  K , -, -, W/m, W/m2-K)

	double a, Alpha_34, b, Beta_34, C, C1, Cp_34, Cv_34, Delta, Gamma, k_34, Lambda, 
		  m, mu_34, n, nu_34, P, Pr_34, P_A1, Ra_D3, Ra_D4, rho_34, T_34, T_36, 
		  grav, Nu_bar, rho_3, rho_6, mu_36, rho_36, cp_36,
		  k_36, nu_36, alpha_36, beta_36, Pr_36, h_36, mu_3, mu_6, k_3, k_6, cp_3, Cp_6, nu_6, nu_3,
		  Alpha_3, alpha_6, Re_D3, Pr_3, Pr_6, Natq_34conv, Kineticq_34conv;

	grav = 9.81; //m/s2  gravitation constant

	P_A1 = m_P_a(hn,hv) * 133.322368;  //convert("torr", "Pa")  //[Pa]

	T_34 = (T_3 + T_4) / 2.;  //[C]
	T_36 = (T_3 + T_6) / 2.;  //[C]
	
	if(!m_Glazing_intact(hn,hv) ) {
	
		// Thermophysical Properties for air 
		rho_3 = m_airProps.dens(T_3, P_6);  //[kg/m**3], air is fluid 1.
		rho_6 = m_airProps.dens(T_6, P_6);  //[kg/m**3], air is fluid 1.

		if (v_6 <= 0.1) {
			mu_36 = m_airProps.visc(T_36);  //[N-s/m**2], AIR
			rho_36 = m_airProps.dens(T_36, P_6);  //[kg/m**3], AIR
			cp_36 = m_airProps.Cp(T_36)*1000.;  //[J/kg-K], AIR
			k_36 = m_airProps.cond(T_36);  //[W/m-K], AIR
			nu_36 = mu_36 / rho_36;  //[m**2/s] kinematic viscosity, AIR
			alpha_36 = k_36 / (cp_36 * rho_36);  //[m**2/s], thermal diffusivity, AIR
			beta_36 =  1.0 / T_36;  //[1/K]
			Ra_D3 = grav * beta_36 * fabs(T_3 - T_6) * pow(m_D_3.at(hn,0),3) / (alpha_36 * nu_36);

			// Warning Statement if following Nusselt Number correlation is used out of recommended range //
			//If ((Ra_D3 <= 1.e-5) || (Ra_D3 >= 1.e12)) continue
				//CALL WARNING('The result may not be accurate, since 10**(-5) < Ra_D3 < 10**12 does not hold. See Function fq_34conv. Ra_D3 = XXXA1', Ra_D3)

			// Churchill and Chu correlation for natural convection from a long isothermal horizontal cylinder //
			Pr_36 = nu_36 / alpha_36;
			Nu_bar = pow(0.60 + (0.387 * pow(Ra_D3,0.1667)) / pow(1. + pow(0.559 / Pr_36,0.5625), 0.2963) , 2);
			h_36 = Nu_bar * k_36 / m_D_3.at(hn,0);  //[W/m**2-K]//
			q_34conv = h_36 * CSP::pi * m_D_3.at(hn,0) * (T_3 - T_6);  //[W/m]//
			h_34 = h_36;  //Set output coefficient
		}
		else {

			// Thermophysical Properties for air 
			mu_3 = m_airProps.visc(T_3);  //[N-s/m**2]
			mu_6 = m_airProps.visc(T_6);  //[N-s/m**2]
			k_3 = m_airProps.cond(T_3);  //[W/m-K]
			k_6 = m_airProps.cond(T_6);  //[W/m-K]
			cp_3 = m_airProps.Cp(T_3)*1000.;  //[J/kg-K]
			Cp_6 = m_airProps.Cp(T_6)*1000.;  //[J/kg-K]
			nu_6 = mu_6 / rho_6;  //[m**2/s]
			nu_3 = mu_3 / rho_3;  //[m**2/s]
			Alpha_3 = k_3 / (cp_3 * rho_3);  //[m**2/s]
			alpha_6 = k_6 / (Cp_6 * rho_6);  //[m**2/s]
			Re_D3 = v_6 * m_D_3.at(hn,0) / nu_6;
			Pr_3 = nu_3 / Alpha_3;
			Pr_6 = nu_6 / alpha_6;

			// Warning Statements if following Nusselt Number correlation is used out of range //
			//if (Re_D3 <= 1) or (Re_D3 >= 10**6) { CALL WARNING('The result may not be accurate, since 1 < Re_D3 < 10**6 does not hold. See Function fq_34conv. Re_D3 = XXXA1', Re_D3)
			//If (Pr_6 <= 0.7) or (Pr_6 >= 500) Then CALL WARNING('The result may not be accurate, since 0.7 < Pr_6 < 500 does not hold. See Function fq_34conv. Pr_6 = XXXA1', Pr_6)

			// Coefficients for external forced convection Nusselt Number correlation (Zhukauskas's correlation) //
			if (Pr_6 <= 10) {
				n = 0.37;
			}
			else{
				n = 0.36;
			}

			if (Re_D3 < 40) {
				C = 0.75;
				m = 0.4;
			}
			else{

				if ((40 <= Re_D3) && (Re_D3 < 1000.)){
					C = 0.51;
					m = 0.5;
				}
				else{
					if ((1.e3 <= Re_D3) && (Re_D3 < 2.e5)) {
						C = 0.26;
						m = 0.6;
					}
					else{
						if ((2.e5 <= Re_D3) && (Re_D3 < 1.e6)) {
							C = 0.076;
							m = 0.7;
						}
					}
				}
			}

			// Zhukauskas's correlation for external forced convection flow normal to an isothermal cylinder 
			Nu_bar = C * pow(Re_D3, m)  * pow(Pr_6, n) * pow(Pr_6 / Pr_3, 0.25);
			h_36 = Nu_bar  *  k_6  /  m_D_3.at(hn,0);  //[W/m**2-K]
			q_34conv =  h_36  *  m_D_3.at(hn,0)  *  CSP::pi  *  (T_3 - T_6);  //[W/m]	
			h_34 = h_36;  //set output coefficient
		}
	}
	else {

		// Thermophysical Properties for gas in annulus space 
		mu_34 = m_AnnulusGasMat.at(hn,hv)->visc(T_34);  //[kg/m-s] 
		Cp_34 = m_AnnulusGasMat.at(hn,hv)->Cp(T_34)*1000.;  //[J/kg-K]
		Cv_34 = m_AnnulusGasMat.at(hn,hv)->Cv(T_34)*1000.;  //[J/kg-K]
		rho_34 = m_AnnulusGasMat.at(hn,hv)->dens(T_34, P_A1);  //[kg/m**3]
		k_34 = m_AnnulusGasMat.at(hn,hv)->cond(T_34);  //[W/m-K]

		// Modified Raithby and Hollands correlation for natural convection in an annular space between horizontal cylinders 
		Alpha_34 = k_34 /(Cp_34 * rho_34);  //[m**2/s]//
		nu_34 = mu_34 / rho_34;  //[m**2/s]//
		Beta_34 = 1. / max(T_34,1.0);  //[1/K]//
		Ra_D3 = grav * Beta_34 * fabs(T_3 - T_4) * pow(m_D_3.at(hn,0),3) / (Alpha_34 * nu_34);
		Ra_D4 = grav * Beta_34 * fabs(T_3 - T_4) * pow(m_D_4.at(hn,0),3) / (Alpha_34 * nu_34);
		Pr_34 = nu_34 / Alpha_34;
		Natq_34conv = 2.425 * k_34 * (T_3 - T_4) / pow(1 + pow(m_D_3.at(hn,0)/ m_D_4.at(hn,0), 0.6), 1.25) * pow(Pr_34 * Ra_D3 / (0.861 + Pr_34),0.25);  //[W/m]//	
		P = m_P_a(hn,hv);  //[mmHg] (note that 1 torr = 1 mmHg by definition)
		C1 = 2.331e-20;  //[mmHg-cm**3/K]//

		// Free-molecular heat transfer for an annular space between horizontal cylinders 
		if (m_AnnulusGasMat.at(hn,hv)->GetFluid() == HTFProperties::Air) 
		{ //AIR
			Delta = 3.53e-8;  //[cm]
		}
		else if (m_AnnulusGasMat.at(hn,hv)->GetFluid() == HTFProperties::Hydrogen_ideal)
		{ //H2
			Delta = 2.4e-8;  //[cm]
		}
		else if (m_AnnulusGasMat.at(hn,hv)->GetFluid() == HTFProperties::Argon_ideal)
		{  //Argon
			Delta = 3.8e-8;  //[cm]
		}
		else
		{
			std::string err_msg = util::format("Annulus Gas code, %d, not recognized", m_AnnulusGasMat.at(hn, hv)->GetFluid());
			throw(C_csp_exception(err_msg, "Evacuated Receiver solution"));
		}

		Lambda = C1 * T_34 / (P * Delta*Delta);  //[cm]
		Gamma = Cp_34 / Cv_34;
		a = 1.;
		b = (2. - a) / a * (9. * Gamma - 5.) / (2. * (Gamma + 1.));
		h_34 = k_34 / (m_D_3.at(hn,0) / 2. * log(m_D_4.at(hn,0) / m_D_3.at(hn,0)) + b * Lambda /100.* (m_D_3.at(hn,0) / m_D_4.at(hn,0) + 1.));  //[W/m**2-K]
		Kineticq_34conv  = m_D_3.at(hn,0) * CSP::pi * h_34 * (T_3 - T_4);  //[W/m]

		// Following compares free-molecular heat transfer with natural convection heat transfer and uses the largest value for heat transfer in annulus 
		if (Kineticq_34conv > Natq_34conv) {
			q_34conv = Kineticq_34conv;  //[W/m]
		}
		else{
			q_34conv = Natq_34conv;  //[W/m]
			h_34 = q_34conv/(m_D_3.at(hn,0)*CSP::pi*(T_3-T_4));  //Recalculate the convection coefficient for natural convection
		}
	}
	return;
 }

 	/******************************************************************************************************************************
 		FUNCTION fq_34rad :	Radiation heat transfer rate between the absorber surface and glazing inner surface
	******************************************************************************************************************************"
	  NOTE: Temperatures input in terms of degrees K

	  Author: R.E. Forristall (2003, EES)
	  Implemented and revised:  M.J. Wagner (10/2009)
					Copyright:  National Renewable Energy Lab (Golden, CO) 2009
					   note  :  Tested against original EES version

	{ 	Radiation heat transfer for a two-surface enclosure.

			Two cases, one if the glazing envelope is intact and one if the glazing is missing or damaged.

			Case 1: Long (infinite) concentric cylinders.

				q' = sigma * PI * D_1 * (T_1^4 - T_2^4) / (1 / EPSILON_1 + (1 - EPSILON_2) / EPSILON_2 * (D_1 / D_2))

				Where,

					q' = radiation heat transfer per unit length [W/m]
					sigma = Stephan-Boltzmann constant [W/m^2-K^4]
					T_1 = absorber outer surface temperature [K]
					T_2 = glazing inner surface temperature [K]
					D_1 = outer absorber diameter [m]
					D_2 = inner glazing diameter [m]
					EPSILON_1 = emissivity of inner surface
					EPSILON_2 = emissivity of outer surface

			Case 2: Small convex object in a large cavity.

				q' = sigma * PI * D_1 * EPSILON_1 * (T_1^4 - T_2^4)
	}*/

void Evacuated_Receiver::FQ_34RAD(double T_3, double T_4, double T_7, double epsilon_3_v, int hn, int hv, double &q_34rad, double &h_34)
{
	//units		(K, K, K, -, -, -, W/m, W/m2-K)
	double T_ave;
	T_ave = (T_3 + T_4)/2.;
		if (! m_Glazing_intact.at(hn,hv)) {
			q_34rad = epsilon_3_v * CSP::pi * m_D_3.at(hn,0)  * CSP::sigma * (pow(T_3, 4) - pow(T_7, 4));  //[W/m]
			h_34 = q_34rad/(CSP::pi*m_D_3.at(hn,0)*(T_3 - T_7));
		}
		else {
			h_34 = CSP::sigma*(T_3*T_3 + T_4*T_4)*(T_3 + T_4)/ (1.0 / epsilon_3_v + m_D_3.at(hn,0) / m_D_4.at(hn,0) * ( 1.0 / m_epsilon_4(hn,0) - 1.0)) ;
			q_34rad = CSP::pi* m_D_3.at(hn,0) * h_34 * (T_3 - T_4);
		}
	return;
}

/******************************************************************************************************************************
 		FUNCTION fq_56conv :	Convective heat transfer rate between the glazing outer surface and the ambient air
	******************************************************************************************************************************"
	  Author: R.E. Forristall (2003, EES)
	  Implemented and revised:  M.J. Wagner (10/2009)
					Copyright:  National Renewable Energy Lab (Golden, CO) 2009
					   note  :  Tested against original EES version

	{ 	h6	Heat Transfer Coefficient

		If no wind, then the Churchill and Chu correlation is used. If wind, then the Zhukauskas's correlation is used. These correlations are described above for q_34conv.
	}*/

void Evacuated_Receiver::FQ_56CONV(double T_5, double T_6, double P_6, double v_6, int hn, int hv, double &q_56conv, double &h_6)
//           units   ( K ,  K , torr, m/s,  W/m    , W/m2-K)
{
double alpha_5, alpha_6, C, Cp_5, Cp_56, Cp_6, k_5, k_56, k_6, m, mu_5, mu_56, mu_6, n, Nus_6,
			nu_5, nu_6, Pr_5, Pr_6, Re_D5, rho_5, rho_56, rho_6, T_56, Nu_bar,
			nu_56, alpha_56, beta_56, Ra_D5, Pr_56;

	T_56 = (T_5 + T_6)/2.0;  //[K]

	// Thermophysical Properties for air 
	mu_5 = m_airProps.visc(T_5);  //[kg/m-s]
	mu_6 = m_airProps.visc(T_6);  //[kg/m-s]
	mu_56 = m_airProps.visc(T_56);  //[kg/m-s]
	k_5 = m_airProps.cond(T_5);  //[W/m-K]
	k_6 = m_airProps.cond(T_6);  //[W/m-K]
	k_56 = m_airProps.cond(T_56);  //[W/m-K]
	Cp_5 = m_airProps.Cp(T_5)*1000.;  //[J/kg-K]
	Cp_6 = m_airProps.Cp(T_6)*1000.;  //[J/kg-K]
	Cp_56 = m_airProps.Cp(T_56)*1000.;  //[J/kg-K]
	rho_5 = m_airProps.dens(T_5, P_6);  //[kg/m^3]
	rho_6 = m_airProps.dens(T_6, P_6);  //[kg/m^3]
	rho_56 = m_airProps.dens(T_56, P_6);  //[kg/m^3]

	// if the glass envelope is missing then the convection heat transfer from the glass 
	//envelope is forced to zero by T_5 = T_6 
	if (! m_Glazing_intact(hn,hv)) {
		q_56conv = (T_5 - T_6);  //[W/m]
	}
	else{
		if (v_6 <= 0.1) {

			// Coefficients for Churchill and Chu natural convection correlation //
			nu_56 = mu_56 / rho_56;  //[m^2/s]
			alpha_56 = k_56 / (Cp_56 * rho_56 );  //[m^2/s]
			beta_56 =  1.0 / T_56;  //[1/K]
			Ra_D5 = CSP::grav *beta_56 * fabs(T_5 - T_6) * pow(m_D_5.at(hn,0),3) / (alpha_56 * nu_56);

			// Warning Statement if following Nusselt Number correlation is used out of range //
			//If (Ra_D5 <= 10**(-5)) or (Ra_D5 >= 10**12) Then CALL WARNING('The result may not be accurate, 
			//since 10**(-5) < Ra_D5 < 10**12 does not hold. See Function fq_56conv. Ra_D5 = XXXA1', Ra_D5)

			// Churchill and Chu correlation for natural convection for a horizontal cylinder //
			Pr_56 = nu_56 / alpha_56;
			Nu_bar = pow(0.60 + (0.387 * pow(Ra_D5, 0.1667)) / pow(1.0 + pow(0.559 / Pr_56, 0.5625), 0.2963) , 2);
			h_6 = Nu_bar * k_56 / m_D_5.at(hn,0);  //[W/m**2-K]
			q_56conv = h_6 * CSP::pi * m_D_5.at(hn,0) * (T_5 - T_6);  //[W/m]
		}
		else {

			// Coefficients for Zhukauskas's correlation //
			alpha_5 = k_5 / (Cp_5 * rho_5);  //[m**2/s]
			alpha_6 = k_6 / (Cp_6 * rho_6);  //[m**2/s]
			nu_5 = mu_5 / rho_5;  //[m**2/s]
			nu_6 = mu_6 / rho_6;  //[m**2/s]
			Pr_5 = nu_5 / alpha_5;
			Pr_6 = nu_6 / alpha_6;
			Re_D5 = v_6 * m_D_5.at(hn,0) * rho_6 / mu_6;

			// Warning Statement if following Nusselt Number correlation is used out of range //
//			if (Pr_6 <= 0.7) or (Pr_6 >= 500) { CALL WARNING('The result may not be accurate, since 0.7 < Pr_6 < 500 does not hold. See Function fq_56conv. Pr_6 = XXXA1', Pr_6)
//			If (Re_D5 <= 1) or (Re_D5 >= 10**6) Then CALL WARNING('The result may not be accurate, since 1 < Re_D5 < 10**6 does not hold. See Function fq_56conv. Re_D5 = XXXA1 ', Re_D5)

			// Zhukauskas's correlation for forced convection over a long horizontal cylinder //
			if (Pr_6 <= 10) {
				n = 0.37;
			}
			else{
				n = 0.36;
			}

			if (Re_D5 < 40.0) {
				C = 0.75;
				m = 0.4	;
			}
			else{
				if ((40.0 <= Re_D5) && (Re_D5 < 1.e3)) {
					C = 0.51;
					m = 0.5;
				}
				else{
					if ((1.e3 <= Re_D5) && (Re_D5 < 2.e5)) {
						C = 0.26;
						m = 0.6;
					}
					else{
						if ((2.e5 <= Re_D5) && (Re_D5 < 1.e6)) {
							C = 0.076;
							m = 0.7;
						}
					}
				}
			}

			Nus_6 = C * pow(Re_D5,m) *  pow(Pr_6,n)  * pow(Pr_6/Pr_5, 0.25);
			h_6 = Nus_6 * k_6 / m_D_5.at(hn,0);  //[W/m**2-K]
			q_56conv = h_6 * CSP::pi * m_D_5.at(hn,0) * (T_5 - T_6);  //[W/m]
		}
	}
	return;
}

/******************************************************************************************************************************
	FUNCTION fq_cond_bracket:	Heat loss estimate through HCE support bracket
	******************************************************************************************************************************"
	Author: R.E. Forristall (2003, EES)
	Implemented and revised:  M.J. Wagner (10/2009)
				Copyright:  National Renewable Energy Lab (Golden, CO) 2009
					note  :  Tested against original EES version
*/
double Evacuated_Receiver::FQ_COND_BRACKET(double T_3, double T_6, double P_6, double v_6, int /*hn*/, int /*hv*/){
	//           units                    ( K ,  K , bar, m/s)
	
	double P_brac, D_brac, A_CS_brac, k_brac, T_base, T_brac, T_brac6, mu_brac6, rho_brac6, 
			Cp_brac6, k_brac6, nu_brac6, Alpha_brac6, Beta_brac6, Ra_Dbrac, Pr_brac6, Nu_bar, h_brac6,
			mu_brac, mu_6, rho_6, rho_brac, k_6, Cp_brac, nu_6, Cp_6, Nu_brac, Alpha_brac,
			Re_Dbrac, Pr_brac, Pr_6, n, C, m, L_HCE, alpha_6;


	// effective bracket perimeter for convection heat transfer
	P_brac = 0.2032;  //[m]

	// effective bracket diameter (2 x 1in) 
	D_brac = 0.0508;  //[m]

	// minimum bracket cross-sectional area for conduction heat transfer
	A_CS_brac = 0.00016129;  //[m**2]

	// conduction coefficient for carbon steel at 600 K
	k_brac = 48.0;  //[W/m-K]

	// effective bracket base temperature
	T_base = T_3 - 10.0;  //[C]

	// estimate average bracket temperature 
	T_brac = (T_base + T_6) / 2.0;  //[C]  //NOTE: MJW modified from /3 to /2.. believed to be an error

	// estimate film temperature for support bracket 
	T_brac6 = (T_brac + T_6) /2.0;  //[C]

	// convection coefficient with and without wind
	if (v_6 <= 0.1) {
		
		mu_brac6 = m_airProps.visc(T_brac6);  //[N-s/m**2]
		rho_brac6 = m_airProps.dens(T_brac6, P_6);  //[kg/m**3]
		Cp_brac6 = m_airProps.Cp(T_brac6)*1000.;  //[J/kg-K]
		k_brac6 = m_airProps.cond(T_brac6);  //[W/m-K]
		nu_brac6 = mu_brac6 / rho_brac6;  //[m**2/s]
		Alpha_brac6 = k_brac6 / (Cp_brac6 * rho_brac6);  //[m**2/s]
		Beta_brac6 =  1.0 / T_brac6;  //[1/K]
		Ra_Dbrac = CSP::grav * Beta_brac6 * fabs(T_brac - T_6) * D_brac*D_brac*D_brac / (Alpha_brac6 * nu_brac6);

		// Warning Statement if following Nusselt Number correlation is used out of recommended range 
		//If ((Ra_Dbrac <= 1.e-5)) || (Ra_Dbrac >= 1.e12) Then CALL WARNING('The result may not be accurate, 
		//since 10**(-5) < Ra_Dbrac < 10**12 does not hold. See Function fq_cond_bracket. Ra_Dbrac = XXXA1', Ra_Dbrac)

		// Churchill and Chu correlation for natural convection from a long isothermal horizontal cylinder 
		Pr_brac6 = nu_brac6 / Alpha_brac6;
		Nu_bar = pow(0.60 + (0.387 * pow(Ra_Dbrac, 0.1667)) / pow(1.0 + pow(0.559 / Pr_brac6, 0.5625), 0.2963) , 2);
		h_brac6 = Nu_bar * k_brac6 / D_brac;  //[W/m**2-K]
	}
	else{
		
		// Thermophysical Properties for air 
		mu_brac = m_airProps.visc(T_brac);  //[N-s/m**2]
		mu_6 = m_airProps.visc(T_6);  //[N-s/m**2]
		rho_6 = m_airProps.dens(T_6, P_6);  //[kg/m**3]
		rho_brac = m_airProps.dens(T_brac, P_6);  //[kg/m**3]
		k_brac = m_airProps.cond(T_brac);  //[W/m-K]
		k_6 = m_airProps.cond(T_6);  //[W/m-K]
		k_brac6 = m_airProps.cond(T_brac6);  //[W/m-K]
		Cp_brac = m_airProps.Cp(T_brac)*1000.;  //[J/kg-K]
		Cp_6 = m_airProps.Cp(T_6)*1000.;  //[J/kg-K]
		nu_6 = mu_6 / rho_6;  //[m**2/s]
		Nu_brac = mu_brac / rho_brac;  //[m**2/s]

		Alpha_brac = k_brac / (Cp_brac * rho_brac * 1000.0);  //[m**2/s]
		alpha_6 = k_6 / (Cp_6 * rho_6 * 1000.0);  //[m**2/s]
		Re_Dbrac = v_6 * D_brac / nu_6;
		Pr_brac = Nu_brac / Alpha_brac;
		Pr_6 = nu_6 / alpha_6;

		// Warning Statements if following Nusselt Correlation is used out of range 
//		if (Re_Dbrac <= 1) or (Re_Dbrac >= 10**6) { CALL WARNING('The result may not be accurate, since 1 < Re_Dbrac < 10**6 does not hold. See Function fq_cond_bracket. Re_Dbrac = XXXA1', Re_Dbrac)
//		If (Pr_6 <= 0.7) or (Pr_6 >= 500) Then CALL WARNING('The result may not be accurate, since 0.7 < Pr_6 < 500 does not hold. See Function fq_cond_bracket. Pr_6 = XXXA1', Pr_6)

		// Coefficients for external forced convection Nusselt Number correlation (Zhukauskas's correlation) 
		if (Pr_6 <= 10.) {
			n = 0.37;
		} 
		else {
			n = 0.36;
		}

		if (Re_Dbrac < 40.) {
			C = 0.75;
			m = 0.4	;
		} 
		else {

			if ((40. <= Re_Dbrac) && (Re_Dbrac< 1.e3)) {
				C = 0.51;
				m = 0.5;
			} 
			else {
				if ((1.e3 <= Re_Dbrac) && (Re_Dbrac < 2.e5)) {
					C = 0.26;
					m = 0.6;
				} 
				else {
					if ((2.e5 <= Re_Dbrac) && (Re_Dbrac < 1.e6)) {
						C = 0.076;
						m = 0.7;
					}
				}
			}
		}

		// Zhukauskas's correlation for external forced convection flow normal to an isothermal cylinder 
		Nu_bar = C * pow(Re_Dbrac,m)  * pow(Pr_6,n) * pow(Pr_6 / Pr_brac,0.25);
		h_brac6 = Nu_bar  *  k_brac6  /  D_brac;  //[W/m**2-K]
	
	}

	// estimated conduction heat loss through HCE support brackets / HCE length 
	L_HCE = 4.06;  //[m]
	return sqrt(h_brac6 * P_brac * k_brac * A_CS_brac) * (T_base - T_6)/L_HCE;  //[W/m]

}


/*
	#################################################################################################################
	#################################################################################################################
	#################################################################################################################


	"******************************************************************************************************************************
 		FUNCTION Fq_12conv :  Convective heat transfer rate from the HTF to the inside of the receiver tube
	******************************************************************************************************************************"
	  Author: R.E. Forristall (2003, EES)
	  Implemented and revised:  M.J. Wagner (10/2009)
					Copyright:  National Renewable Energy Lab (Golden, CO) 2009
						 note:  This function was programmed and tested against the EES original.
								Small variations in output are due to slightly different fluid 
								properties used in the two models.

		Newton's Law of Cooling.

			q' = h * D_i *  PI * (T_m - T_s)

			h = Nu_Di * k / D_i

		Where

			q' = convection heat transfer rate per unit length [W/m]
			h = convection heat transfer coefficient [W/m^2-k]
			D_i = inside diameter of absorber pipe [m]
			T_m = mean (bulk) temperature of HTF [C]
			T_s = inside surface temperature of absorber pipe [C]
			Nu_Di = Nusselt number based on inside diameter
			k = conduction heat transfer coefficient of HTF [W/m-K]

		The Nusselt number is estimated with the correlation developed by Gnielinski. 

 			Nu# = (f / 8) * (Re_Di - 1000) * Pr / (1 + 12.7 * (f / 8)^(1/2) * (Pr^(2/3) -1))  * (Pr / Pr_w)^0.11
			f = (1.82 * log10(Re_Di) - 1.64)^(-2)
			Re_Di = Rho * v_m * Di / u
			Pr  = Cp * u / k

		Where

			Nu# = Nusselt number
			Re_Di = Reynolds number for internal pipe flow
			Pr = Prandtl number
			Pr_w = Prandtl number evaluated at the wall temperature
			u = fluid absolute viscosity [kg/m-s]
			Di = inside diameter [m]
			Cp = fluid specific heat [J/kg-K]
			k = fluid thermal conductivity [W/m-K]
			Rho = fluid density [kg/m^3]
			v_m = mean fluid velocity [m/s]

	The above correlation is valid for 0.5 < Pr < 2000 and 2300< Re_Di < 5 * 10^6 and can be used for both uniform heat flux and uniform wall temperature cases. With the exception of Pr_w, all properties are evaluated at the mean fluid temperature.

	If Re_D <= 2300 and the choice was made from the diagram window  to use the laminar flow model, one of  the following correlations is used.

			for inner tube flow (uniform flux condition)
			Nu# = 4.36

			for inner annulus flow (uniform flux condition -- estimated from table for Nu# with heat fluxes at both surfaces)
			D_p/D_2	Nu#
			0		4.364
			0.05		4.792
			0.10		4.834
			0.20		4.833
			0.40		4.979
			0.60		5.099
			0.80		5.24
			1.00		5.385		


	For the "SNL test platform" case the inside diameter in the above correlations is replaced with the following hydraulic diameter definition.

			D_h = 4 * A_c / P = D_ao - D_ai

		Where

			D_h = hydraulic diameter [m]
			A_c = flow cross sectional area [m^2]
			P = wetted perimeter [m]
			D_ai = inner annulus diameter [m]
			D_ao = outer annulus diameter [m]

	(Sources: Incropera, F., DeWitt, D., Fundamentals of Heat and Mass Transfer, Third Edition; John Wiley and Sons, New York, 1981, pp. 489-491, 502-503. Gnielinski, V., "New Equations for Heat and Mass Transfer in Turbulent Pipe and Channel Flow," International Chemical Engineering, Vol. 16, No. 2, April 1976.)
	*/

double Evacuated_Receiver::fT_2(double q_12conv, double T_1, double T_2g, double v_1, int hn, int hv)
{
	//		convection 1->2,  HTF temp, guess T2,  fluid velocity, HCE #, HCE variant
	//     Input units (  K   ,  K ,  real,  m/s, - , -)
	
	double Cp_1, Cp_2, f, h_1, k_1, k_2, mu_1, mu_2, Nu_D2, Pr_1, Pr_2, Re_D2, rho_1, DRatio;
	bool includelaminar = true;	//cc -- this is always set to TRUE in TRNSYS
	
	// Thermophysical properties for HTF 
	mu_1 = p_htfProps->visc(T_1);  //[kg/m-s]
	mu_2 = p_htfProps->visc(T_2g);  //[kg/m-s]
	Cp_1 = p_htfProps->Cp(T_1)*1000.;  //[J/kg-K]
	Cp_2 = p_htfProps->Cp(T_2g)*1000.;  //[J/kg-K]
	k_1 = max(p_htfProps->cond(T_1),1.e-4);  //[W/m-K]
	k_2 = max(p_htfProps->cond(T_2g),1.e-4);  //[W/m-K]
	rho_1 = p_htfProps->dens(T_1, 0.0);  //[kg/m^3]

	Pr_2 = (Cp_2 * mu_2) / k_2;
	Pr_1 = (Cp_1 * mu_1) / k_1;

	if(v_1 > 0.1) {

		Re_D2 = (rho_1 * m_D_h.at(hn,0) * v_1) / (mu_1);

		// Nusselt Number for laminar flow case if option to include laminar flow model is chosen 
		if (( includelaminar == true) && (Re_D2 <= 2300.)) {
			if(m_flowtype.at(hn,hv) == 2.0) {
				DRatio = m_D_p.at(hn,hv)/m_D_2.at(hn,hv);
				//Estimate for uniform heat flux case (poly. regression based on lookup table in Forristall EES model)
				//---Note that this regression is based on an 8-point table, and is highly non-practical outside of DRatio bounds
				//---0 and 1
				if(DRatio > 1.) {
					Nu_D2 = 5.385;
				}
				else if(DRatio < 0.) {
					Nu_D2 = 4.364;
				}
				else{
					Nu_D2 = 41.402*pow(DRatio,5) - 109.702*pow(DRatio,4) + 104.570*pow(DRatio,3) - 42.979*pow(DRatio,2) + 7.686*DRatio + 4.411;
				}
			}
			else{
				Nu_D2 = 4.36;				//uniform heat flux
			}
		}
		else{
			// Warning statements if turbulent/transitional flow Nusselt Number correlation is used out of recommended range 
	//		if (Pr_1 <= 0.5) or (2000 <= Pr_1) { CALL WARNING('The result may not be accurate, since 0.5 < Pr_1 < 2000 does not hold. See PROCEDURE Pq_12conv. Pr_1 = XXXA1', Pr_1)
	//		if (Pr_2 <= 0.5) or (2000 <= Pr_2) { CALL WARNING('The result may not be accurate, since 0.5 < Pr_2 < 2000 does not hold. See PROCEDURE Pq_12conv. Pr_2 = XXXA1', Pr_2)
	//		If ( Re_D2 <= (2300) ) or (5*10**6 <= Re_D2 ) Then CALL WARNING('The result may not be accurate, since 2300 < Re_D2 < (5 * 10**6) does not hold. See PROCEDURE Pq_12conv. Re_D2 = XXXA1', Re_D2)

			// Turbulent/transitional flow Nusselt Number correlation (modified Gnielinski correlation) 	
			f = pow(1.82 * log10(Re_D2) - 1.64,-2);
			Nu_D2 = (f / 8.) * (Re_D2 - 1000.) * Pr_1 / (1. + 12.7 * sqrt(f / 8.) * (pow(Pr_1,0.6667) -1.)) * pow(Pr_1 / Pr_2, 0.11);
		}

		h_1 = Nu_D2 * k_1 / m_D_h.at(hn,0);  //[W/m**2-K]
		return T_1 + q_12conv/(h_1*m_D_2.at(hn,0)*CSP::pi);
		//q_12conv = h_1 * D_2 * PI  * (T_2 - T_1ave)  //[W/m]
	}
	else{
		h_1 = 0.0001;
		return T_1;
	}
}

/******************************************************************************************************************************
	FUNCTION fk_23:	Absorber conductance
******************************************************************************************************************************"
{ Based on linear fit of data from "Alloy Digest, Sourcebook, Stainless Steels"; ASM International, 2000.}
*/

double Evacuated_Receiver::FK_23(double T_2, double T_3, int hn, int /*hv*/)
{
	double T_23;

	//Absorber materials:
	// (1)   304L
	// (2)   216L
	// (3)   321H
	// (4)   B42 Copper Pipe

	T_23 = (T_2 + T_3) / 2. - 273.15;  //[C]
	return m_AbsorberPropMat.at(hn,0)->cond(T_23);
		
}





void Evacuated_Receiver::EvacReceiver(double T_1_in, double m_dot, double T_amb, double T_sky, double v_6, double P_6, double q_i, 
	int hn /*HCE number [0..3] */, int hv /* HCE variant [0..3] */, int ct /*Collector type*/, int sca_num, bool single_point,  int ncall, double time,
	//outputs
	double &q_heatloss, double &q_12conv, double &q_34tot, double &c_1ave, double &rho_1ave )
{
	/*
	This subroutine contains the trough detailed plant model.  The collector field is modeled
	using an iterative solver.
	This code was written for the National Renewable Energy Laboratory
	Copyright 2009-2010
	Author: Mike Wagner

	Subroutine Inputs (and parameters)
	 ----------------------------------------------------------------------------------------------------------------------
	 Nb | Variable             | Description                                             | Input  Units   | Internal Units 
	 ---|----------------------|---------------------------------------------------------|----------------|----------------
	 1  | T_1_in               | Receiver inlet temperature                              |                |
	 2  | m_dot                | Heat transfer fluid mass flow rate                      |                |
	 3  | T_amb                | Ambient dry-bulb temperature                            |                |
	 4  | T_sky                | Sky temperature                                         |                |
	 5  | v_6                  | Ambient wind velocity                                   |                |
	 6  | P_6                  | Ambient atmospheric pressure                            |                |
	 7  | q_i                  | Total incident irradiation on the receiver              |                |
	 8  | A_cs                 | Internal absorber tube cross-sectional area             |                |
	 9  | D_2                  | Internal absorber tube diameter                         |                |
	 10 | D_3                  | External absorber tube diameter                         |                |
	 11 | D_4                  | Internal glass envelope diameter                        |                |
	 12 | D_5                  | External glass envelope diameter                        |                |
	 13 | D_p                  | (optional) Plug diameter                                |                |
	 14 | D_h                  | Absorber tube hydraulic diameter                        |                |
	 15 | eps_mode             | Interpolation mode for the emissivity (1=table,2=fixed) |                |
	 16 | xx                   | Array of temperature values for emissivity table        |                |
	 17 | yy                   | Array of emissivity values for table                    |                |
	 18 | nea                  | Number of entries in the emissivity table               |                |
	 19 | L_actSCA             | Length of the active receiver surface                   |                |
	 20 | single_point         | Logical flag - is the calculation for a single point?   |                |
	 21 | Epsilon_32           | Constant value for emissivity if table isn't used       |                |
	 22 | Epsilon_4            | Envelope inner surface emissivity                       |                |
	 23 | EPSILON_5            | Envelope outer surface emissivity                       |                |
	 24 | Alpha_abs            | Absorber tube absorptance                               |                |
	 25 | alpha_env            | Envelope absorptance                                    |                |
	 26 | ColOptEff            | Collector optical efficiency                            |                |
	 27 | Tau_envelope         | Total envelope transmittance                            |                |
	 28 | P_a                  | Annulus gas pressure                                    | torr           |
	 29 | Flow_type            | Flag indicating the presence of an internal plug        |                |
	 30 | AnnulusGas           | Annulus gas type                                        |                |
	 31 | Fluid                | Heat transfer fluid type                                |                |
	 32 | AbsorberMaterial     | Absorber material type                                  |                |
	 33 | time                 | Simulation time                                         |                |

	Subroutine outputs 
	 ----------------------------------------------------------------------------------------------------------------------
	 Nb | Variable             | Description                                             | Input  Units   | Internal Units 
	 ---|----------------------|---------------------------------------------------------|----------------|----------------
	 1  | q_heatloss           | Total heat loss from the receiver                       | W/m            |
	 2  | q_12conv             | Total heat absorption into the HTF                      | W/m            |
	 3  | q_34tot              | Convective and radiative heat loss                      |                |
	 4  | c_1ave               | Specific heat of the HTF across the receiver            | kJ/kg-K        |
	 5  | rho_1ave             | Density of the HTF across the receiver                  |                |

	 ----------------------------------------------------------------------------------------------------------------------
	Forristall Temperature distribution diagram
	*****************************************************
		Fluid (1) ----------->(2)<--Absorber-->(3)<-- Annulus -->(4)<--- Glass  --->(5)<-- Air (6)/Sky (7)


		T_1 = Bulk heat transfer fluid (HTF) temperature 
		T_2 = Absorber Inside surface temperature
		T_3 = Absorber outside surface temperature 
		T_4 = Glass envelope inside surface temperature
		T_5 = Glass envelope outside surface temperature
		T_6 = Ambient temperature
		T_7 = Effective Sky Temperature

		q_12conv = Convection heat transfer rate per unit length between the HTF and the inside of the receiver tube
		q_23cond = Conduction heat transfer rate per unit length through the absorber
		q_34conv = Convection heat transfer rate per unit length between the absorber outer surface and the glazing inner surface
		q_34rad = Radiation heat transfer rate per unit length between the absorber outer surface and the glazing inner surface
		q_45cond = Conduction heat transfer rate per unit length through the glazing
		q_56conv = Convection heat transfer rate per unit length between the glazing outer surface and the ambient air
		q_57rad = Radiation heat transfer rate per unit length between the glazing outer surface and the sky
	----------------------------------------------------------------------------------------------------------------------
	*/

	//cc -- note that collector/hce geometry is part of the parent class. Only the indices specifying the
	//		number of the HCE and collector need to be passed here.
	
	//---Variable declarations------
	/*
	bool reguess;
	double T_2, T_3, T_4, T_5, T_6, T_7,v_1, k_23, q_34conv, q_34rad, h_34conv, h_34rad, q_23cond, 
		k_45, q_45cond, q_56conv, h_56conv, q_57rad, q_3SolAbs, q_5solabs, q_cond_bracket, R_45cond,
		T_save[5], T_2g, cp_1, T3_tol, q5_tol, T1_tol, T2_tol, Diff_T3, diff_q5, T_lower, T_upper, 
		q_5out, T_1_out, diff_T1, T_1_ave, T_1_out1, diff_T2, eps_3, q_in_W, T_upper_max, y_upper,
		y_lower, upmult, q5_tol_1, T3_upper, T3_lower, y_T3_upper, y_T3_lower, abs_diffT3;
		
	bool UPFLAG, LOWFLAG, T3upflag, T3lowflag, is_e_table;
	int qq, q5_iter, T1_iter, q_conv_iter;
		
	double T_save_tot, colopteff_tot;
	*/
	//cc--> note that xx and yy have size 'nea'
		
	//bool glazingIntact = m_Glazing_intact.at(hn,hv);
	bool reguess = false;
	
	double T_save_tot = 0.0;
	// Re-guess criteria
	do
	{
		if( time <= 2 || ((int)mv_reguess_args[0] == 1) != m_Glazing_intact.at(hn,hv) || m_P_a.at(hn,hv) != mv_reguess_args[1] || fabs(mv_reguess_args[2]-T_1_in) > 50.0 )
		{
			reguess = true;
			break;
		}

		for( int i = 0; i < 5; i++ )
		{
			if( m_T_save.at(i,0) < T_sky - 1.0 )
			{
				reguess = true;
				break;
			}
		}
		if( reguess )
			break;

		for( int i = 0; i < 5; i++ )
			T_save_tot += m_T_save.at(i,0);
	
		if( T_save_tot != T_save_tot )
			reguess = true;
	}	while(false);


	double T_upper_max = std::numeric_limits<double>::quiet_NaN();

	if( reguess )
	{
		if( m_Glazing_intact.at(hn,hv) )
		{
			m_T_save.at(0,0) = T_1_in;
			m_T_save.at(1,0) = T_1_in + 2.0;
			m_T_save.at(2,0) = m_T_save.at(1,0) + 5.0;
			if( m_P_a.at(hn,hv) > 1.0 )
			{
				// Set guess values for different annulus pressures
				m_T_save.at(3,0) = m_T_save.at(2,0) - 0.5*(m_T_save.at(2,0) - T_amb);		// If higher pressure, guess higher T4
				T_upper_max = m_T_save.at(2,0) - 0.2*(m_T_save.at(2,0) - T_amb);			// Also high upper limit for T4
			}
			else
			{
				// Set guess values for different annulus pressures
				m_T_save.at(3,0) = m_T_save.at(2,0) - 0.9*(m_T_save.at(2,0) - T_amb);		// If higher pressure, guess higher T4
				T_upper_max = m_T_save.at(2,0) - 0.5*(m_T_save.at(2,0) - T_amb);			// Also high upper limit for T4
			}
			m_T_save.at(4,0) = m_T_save.at(3,0) - 2.0;

			mv_reguess_args[1] = m_P_a.at(hn,hv);				// Reset previous pressure
			mv_reguess_args[0] = m_Glazing_intact.at(hn,hn);	// Reset previous glazing logic
			mv_reguess_args[2] = T_1_in;						// Reset previous T_1_in
		}
		else
		{
			m_T_save.at(0,0) = T_1_in;
			m_T_save.at(1,0) = T_1_in + 2.0;
			m_T_save.at(2,0) = m_T_save.at(1,0) + 5.0;
			m_T_save.at(3,0) = T_amb;
			m_T_save.at(4,0) = T_amb;

			mv_reguess_args[0] = m_Glazing_intact.at(hn,hv) ? 1.0 : 0.0;		// Reset previous glazing logic
			mv_reguess_args[1] = T_1_in;										// Reset previous T_1_in

		}
	}

	// Set initial guess values
	double T_2 = m_T_save.at(1,0);
	double T_3 = m_T_save.at(2,0);
	double T_4 = m_T_save.at(3,0);
	double T_5 = m_T_save.at(4,0);
	// Set constant temps
	double T_6 = T_amb;
	double T_7 = T_sky;

	int qq = 0;			// Set iteration counter for T3 loop

	double T_2g = T_2;		// Initial guess value for T_2 (only used in property lookup)
	double cp_1 = 1950.0;	// Initial guess value for cp of WF

	// Tolerance for iteration
	double T3_tol = 1.5E-3;
	double q5_tol = 1.0E-4;		// Since iterations are nested inside T3, make tolerance a bit tighter
	double T1_tol = 1.0E-3;		
	double T2_tol = 1.0E-3;

	// Decreasing the tolerance helps get out of repeating defocus iterations
	if( ncall > 8 )
	{
		T3_tol = 1.5E-4;
		q5_tol = 1.0E-4;
		T1_tol = 1.0E-4;
		T2_tol = 1.0E-4;
	}

	double Diff_T3 = 10.0 + T3_tol;		// Set difference > tolerance

	// Constants
	double k_45 = 1.04;										//[W/m-K] Conductivity of glass
	double R_45cond = log( m_D_5.at(hn,0)/m_D_4.at(hn,0) )/(2.0*CSP::pi*k_45);	//[K-m/W] Equation of thermal resistance for conduction through a cylinder

	double colopteff_tot = m_ColOptEff.at(ct, sca_num)*m_Dirt_HCE.at(hn,hv)*m_Shadowing.at(hn,hv);	// The total optical efficiency

	double q_3SolAbs = 0.0;
	double q_5SolAbs = 0.0;
	if( m_Glazing_intact.at(hn,hv) )
	{
		// These calculations (q_3SolAbs, q_5SolAbs) are not dependent on temperature, so only need to be computed once per call to subroutine
		q_3SolAbs = q_i * colopteff_tot * m_tau_envelope.at(hn,hv) * m_alpha_abs.at(hn,hv);		//[W/m]
		// We must account for the radiation absorbed as it passes through the envelope
		q_5SolAbs = q_i * colopteff_tot * m_alpha_env.at(hn,hv);								//[W/m]
		
	}
	else
	{
		// Calculate the absorbed energy
		q_3SolAbs = q_i * colopteff_tot * m_alpha_abs.at(hn,hv);				//[W/m]
	}

	bool is_e_table = false;
	double eps_3 = std::numeric_limits<double>::quiet_NaN();

	if( m_eps3->getTableSize(hn, hv) < 2 )
		eps_3 = m_eps3->getSingleValue(hn, hv);
	else
	{
		eps_3 = m_eps3->interpolate(hn, hv, T_3 - 273.15);						// Set epsilon value for case that eps_mode = 1. Will reset inside temp if eps_mode > 1.
		is_e_table = true;														// The emissivity is in tabular form
	}

	double T3upflag = false;
	double T3lowflag = false;

	double T3_upper = std::numeric_limits<double>::quiet_NaN();
	double T3_lower = std::numeric_limits<double>::quiet_NaN();

	double y_T3_upper = std::numeric_limits<double>::quiet_NaN();
	double y_T3_lower = std::numeric_limits<double>::quiet_NaN();

	double abs_diffT3 = std::numeric_limits<double>::quiet_NaN();

	double q_cond_bracket = std::numeric_limits<double>::quiet_NaN();

	double T_lower  = std::numeric_limits<double>::quiet_NaN();
	double T_upper  = std::numeric_limits<double>::quiet_NaN();
	double q5_tol_1 = std::numeric_limits<double>::quiet_NaN();

	while( ( (fabs(Diff_T3)>T3_tol) && (qq<100) ) || (qq<2))
	{    //Outer loop: Find T_3 such than energy balance is satisfied
		qq++; //loop counter
		
		if(qq>1)
		{
			if((T3upflag)&&(T3lowflag))
			{
				if(Diff_T3 > 0.){
					T3_upper = T_3;
					y_T3_upper = Diff_T3;
				} 
				else {    
					T3_lower = T_3;
					y_T3_lower = Diff_T3;
				}
				T_3 = (y_T3_upper)/(y_T3_upper-y_T3_lower)*(T3_lower - T3_upper) + T3_upper;		
			} 
			else
			{
            	T3_lower = T_3;
				y_T3_lower = Diff_T3;
				T3lowflag = true;
			}
			    
			if((T3upflag)&&(T3lowflag))
			{  
				T_3 = (y_T3_upper)/(y_T3_upper-y_T3_lower)*(T3_lower - T3_upper) + T3_upper;
			} 
			else 
			{
				T_3 = T_3 - abs_diffT3;         //Note that recalculating T_3 using this exact equation, rather than T_3 = T_3 - frac*diff_T3 was found to solve in fewer iterations
			}
		}

		//Calculate temperature sensitive emissivity using T_3, if required
		if( is_e_table )
			eps_3 = m_eps3->interpolate(hn, hv, (T_3-273.15));			//call interp((T_3-273.15),eps_mode,xx,yy,eps3old,eps_3)

		//Separate GlazingIntact = true and GlazingIntact = false  If true, T4 must be solved, if false then T4 is explicitly known (or doesn't exist, depending on how you want to look at it)
		//Solving for correct T4 as it relates to current T3 value
		double q_34rad = std::numeric_limits<double>::quiet_NaN();
		double q_34conv = std::numeric_limits<double>::quiet_NaN();
		double h_34rad = std::numeric_limits<double>::quiet_NaN();
		double h_34conv = std::numeric_limits<double>::quiet_NaN();

		if( m_Glazing_intact.at(hn,hv) )
		{
			//**********************************************
			//************* SET UP T_4 ITERATION **********************
			//**********************************************
						
			double y_lower = std::numeric_limits<double>::quiet_NaN();
			double y_upper = std::numeric_limits<double>::quiet_NaN();			
			double upmult = std::numeric_limits<double>::quiet_NaN();

			if(qq==1)       // If first iteration, set T_4 bounds to phyiscal limits defined by T_3 and T_sky
			{               
				T_lower  = T_sky;         //Lowest possible temperature of T_4 is sky temp        
				T_upper  = max(T_upper_max,T_amb);    //Highest possible temperature is the highest temperature on either side of T_4: either T_3 or ambient
				q5_tol_1 = 0.001;           //Just get T4 in the ball park.  '20' may not be the optimum value.....
			} 
			else           // For additional iterations:
			{                                            
				T_lower  = T_lower - max(abs_diffT3,0.0);       //If diff_T3 is + then new T3 < old T3 so adjust lower limit
				T_upper  = T_upper + fabs(min(abs_diffT3,0.0));  //If diff_T3 is (-) then new T3 > old T3 so adjust upper limit
				q5_tol_1 = q5_tol;        //For remaining T3 iterations, use specified tolerance (note that 2 iterations for T3 are gauranteed)             
			}

			double diff_q5 = q5_tol_1 + 1.0;		//Set diff > tolerance
			int q5_iter = 0;						//Set iteration counter

			bool UPFLAG = false;					// Set logic to switch from bisection to false position mode
			bool LOWFLAG = false;					// Set logic to switch from bisection to false position mode

			//***********************************************************************************
			//************* Begin Bisection/False Position Iteration method *********************
			//***********************************************************************************

			while( (fabs(diff_q5)>q5_tol_1) && (q5_iter<100) )
			{       //Determine T_4 such that energy balance from T_3 to surroundings is satisfied

				q5_iter++;					// Increase iteration counter

				// The convective heat exchange between the absorber and the envelope
				//      UNITS   ( K , K, torr, Pa , m/s,  K , -, -, W/m, W/m2-K)
				q_34conv = 0.0;
				h_34conv = 0.0;
				FQ_34CONV( T_3, T_4, P_6, v_6, T_6, hn, hv, q_34conv, h_34conv );

				// The radiative heat exchange between the absorber and the envelope
				//    Units         ( K ,  K ,  m ,  m , K  ,    -     ,    -     ,   logical    ,  W/m   , W/m2-K)
				q_34rad = 0.0;
				h_34rad = 0.0;
				FQ_34RAD(T_3, T_4, T_7, eps_3, hn, hv, q_34rad, h_34rad);

				// The total heat exchange between absorber and envelope
				q_34tot = q_34conv + q_34rad;			// [W/m]

				// **********************************************
				// ************* Calculate T_5 *************
				// **********************************************
				// The thermal energy flow across 45 is equal to the energy from the absorber plus
				// the thermal energy that is generated by direct heating of the glass envelope
				double q_45cond = q_34tot + q_5SolAbs;			// [W/m]

				// Knowing heat flow and properties, T_5 can be calculated
				T_5 = T_4 - q_45cond*R_45cond;					// [K]

				// *************************************************************************
				// ************* Calculate HT from exterior surface to ambient *************
				// *************************************************************************
				// With T_5 and T_6 (amb T) calculate convective and radiative loss from the glass envelope
				//           units   ( K ,  K ,  torr, m/s, -, -, W/m, W/m2-K)
				double q_56conv = 0.0;
				double h_56conv = 0.0;
				FQ_56CONV( T_5, T_6, P_6, v_6, hn, hv, q_56conv, h_56conv );	// [W/m]
				double q_57rad = m_epsilon_5.at(hn,hv)*CSP::sigma*(pow(T_5,4)-pow(T_7,4));
				double q_5out = q_57rad + q_56conv;		// [W/m]

				//***************************************************************************
				//********** Compare q_5out with q_45 cond***********************************
				//***************************************************************************
				diff_q5 = (q_5out - q_45cond)/fabs(q_45cond);			// [W/m]

				//Determine next guess for T_4.  Want to use false position method, but it requires that the *results* at both ends of the bracket are known.  We have
				//defined a bracket but not the results.  Use the guess T_4 to get the results at one end of a new bracket.  Then calculate a new T_4 that is highly weighted 
				//towards the side of the original bracket that the 1st T_4 did not replace.  In most cases, this new T_4 will result in the opposite diff_q5, which 
				//defines both sides of the bracket.  If results for both sides are then defined, "LOWFLAG" and "UPFLAG" will be true, and false position method will be applied.

				if( LOWFLAG && UPFLAG )
				{          //False position method     
					if(diff_q5>0.0)
					{
						T_upper = T_4;       //If energy leaving T_5 is greater than energy entering T_5, then T_4 guess is too high
						y_upper = diff_q5;   //so set new upper limit to T_4
					} 
					else 
					{                    //If energy leaving T_5 is less than energy entering T_5, then T_4 guess is too low
						T_lower = T_4;       //so set new lower limit to T_4
						y_lower = diff_q5;   //also, set result to go along with lower limit
					}    
					T_4 = (y_upper)/(y_upper-y_lower)*(T_lower - T_upper) + T_upper;      				    
				} 
				else
				{
					if(diff_q5>0.0)			//If energy leaving T_5 is greater than energy entering T_5, then T_4 guess is too high
					{    
						T_upper = T_4;       //so set new upper limit to T_4
						y_upper = diff_q5;   //also, set result to go along with upper limit
						UPFLAG  = true;    //Upper result is now known
						if(qq==1){
							upmult  = 0.1;       //Just want to get in ballpark for first iteration of receiver
						} 
						else {
							upmult  = 0.1;       //Weight such that next calculated T_4 (if using bisection method) results in negative diff_q5
						}
					        
					} 
					else 
					{                    //If energy leaving T_5 is less than energy entering T_5, then T_4 guess is too low          
						T_lower = T_4;       //so set new lower limit to T_4
						y_lower = diff_q5;   //also, set result to go along with lower limit
						LOWFLAG = true;    //Lower result is now known
						if(qq==1)
						{
							upmult  = 0.1;       //Just want to get in ballpark for first iteration of receiver
						} 
						else 
						{
							upmult  = 0.9;       //Weight such that next calculated T_4 (if using bisection method) results in positive diff_q5
						}					        
					}
					    
					if(LOWFLAG && UPFLAG)
					{  //If results of bracket are defined, use false position
						T_4 = (y_upper)/(y_upper-y_lower)*(T_lower - T_upper) + T_upper;
					} 
					else 
					{                            //If not, keep bisection
						T_4 = (1.-upmult)*T_lower + upmult*T_upper;
					}
				}
				//*********************************************************************************************
				//********** END Bisection/False Position Iteration Loop on T_4 *******************************
				//*********************************************************************************************
			}
		}
		else		// Glazing is not intact
		{
			//Know convection and radiation forcing temps
			//----Having guessed the system temperatures, calculate the thermal losses starting from
			//----the absorber surface (3)
			//The convective heat exchange between the absorber and the envelope
			FQ_34CONV(T_3, T_4, P_6, v_6, T_6, hn, hv, q_34conv, h_34conv);
			//The radiative heat exchange between the absorber and the envelope
			FQ_34RAD(T_3, T_4, T_7, eps_3, hn, hv, q_34rad, h_34rad);
			//The total heat exchange between absorber and envelope
			q_34tot = q_34conv + q_34rad;    //[W/m]
		}	// Know heat transfer from outer surface of receiver tube to ambient

		//Bracket Losses
		//Bracket conduction losses apply
		q_cond_bracket = FQ_COND_BRACKET( T_3, T_6, P_6, v_6, hn, hv );

		q_12conv = q_3SolAbs - (q_34tot+q_cond_bracket);         //[W/m] Energy transfer to/from fluid based on energy balance at T_3

		double q_in_W  = q_12conv * m_L_actSCA.at(ct,0);	//Convert [W/m] to [W] for some calculations

		double T_1_out = std::numeric_limits<double>::quiet_NaN();
		double T_1_ave = std::numeric_limits<double>::quiet_NaN();
		if( !single_point )
		{
			T_1_out = max( T_sky, q_in_W/(m_dot*cp_1) + T_1_in );    //Estimate outlet temperature with previous cp
			    
			double diff_T1 = T1_tol + 1.0;								//Set diff > tolerance
			int T1_iter = 0;                                             //Set iteration counter    

			while( (fabs(diff_T1)>T1_tol) && (T1_iter<100))
			{       //Find correct cp& rho and solve for T_1_ave
				T1_iter ++;                   //Increase iteration counter
				T_1_ave = (T_1_out + T_1_in) / 2.0;			//Average fluid temperature
				cp_1 = p_htfProps->Cp( T_1_ave )*1000.0;	//Estimate outlet temperature with previous cp
				double T_1_out1 = max( T_sky, q_in_W/(m_dot*cp_1)+T_1_in );		// Estimate outlet temperature with previous cp
				diff_T1 = (T_1_out - T_1_out1)/T_1_out;		//Difference between T_1_out used to calculate T_ave, and T_1_out calculated with new cp
				T_1_out = T_1_out1;			//Calculate new T_1_out
			}

		}
		else
		{
			// If we're only calculating performance for a single point, set the receiver ave/outlet temperature to the inlet
			T_1_out = T_1_in;
			T_1_ave = T_1_in;
		}

		rho_1ave = p_htfProps->dens(T_1_ave, 0.0);		//[kg/m^3] Density
		double v_1 = m_dot/(rho_1ave*m_A_cs.at(hn,0));	// HTF bulk velocity

		int q_conv_iter = 0;				// Set iteration counter
		double diff_T2 = 1.0 + T2_tol;		// Set diff > tolerance

		// Ensure convective calculations are correct (convergence on T_2)
		while( (fabs(diff_T2)>T2_tol) && q_conv_iter < 100 )
		{
			q_conv_iter++;			// Increase iteration counter

			T_2 = max(10.0, fT_2( q_12conv, T_1_ave, T_2g, v_1, hn, hv ) );		// Calculate T_2 (with previous T_2 as input)
			diff_T2 = (T_2 - T_2g)/T_2;								// T_2 difference
			T_2g = T_2 - 0.5*(T_2 - T_2g);							// Reset T_2

			if( qq < 2 )			// For first T3 iteration, do not iterate on T_2 (again, this control is based on observations of solve time and may not be optimal for all simulations)
				break;
		}

		// The conductive heat transfer equals the convective heat transfer (energy balance)
		double q_23cond = q_12conv;			//[W/m]

		// Calculate tube conductivity
		double k_23 = FK_23( T_2, T_3, hn, hv );

		// Update the absorber surface temperature (T_3) according to new heat transfer rate
		abs_diffT3 = T_3 - (T_2 - q_23cond*log(m_D_3.at(hn,0)/m_D_2.at(hn,0))/(2.0*CSP::pi*k_23));

		Diff_T3 = abs_diffT3 / T_3;
	}
		
	//Warning of convergence failure
	//if(qq>99) {                           //End simulation if loop does not converge
	//    call messages(-1,"Trough Energy Balance Convergence Error 1",'WARNING',INFO(1),INFO(2))
	//    return
	//}
	//
	//if(T1_iter>99) {
	//    call messages(-1,"Trough Energy Balance Convergence Error 2",'WARNING',INFO(1),INFO(2))
	//    return
	//}
	//
	//if(q_conv_iter>99) {
	//    call messages(-1,"Trough Energy Balance Convergence Error 3",'WARNING',INFO(1),INFO(2))
	//    return
	//}
	//
	//if(q5_iter>99) {
	//    call messages(-1,"Trough Energy Balance Convergence Error 4",'WARNING',INFO(1),INFO(2))
	//    return
	//}
		  
	//Calculate specific heat in kJ/kg
	c_1ave = cp_1/1000.;

	q_heatloss = q_34tot + q_cond_bracket + q_5SolAbs;   //[W/m]
		
	//Save temperatures
	m_T_save.at(1,0) = T_2;
	m_T_save.at(2,0) = T_3; 
	m_T_save.at(3,0) = T_4;
	m_T_save.at(4,0) = T_5;

}
