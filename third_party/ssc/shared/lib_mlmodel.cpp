/*******************************************************************************************************
*  Copyright 2017 - pvyield GmbH / Timo Richert
*  Copyright 2017 Alliance for Sustainable Energy, LLC
*
*  NOTICE: This software was developed at least in part by Alliance for Sustainable Energy, LLC
*  (“Alliance”) under Contract No. DE-AC36-08GO28308 with the U.S. Department of Energy and the U.S.
*  The Government retains for itself and others acting on its behalf a nonexclusive, paid-up,
*  irrevocable worldwide license in the software to reproduce, prepare derivative works, distribute
*  copies to the public, perform publicly and display publicly, and to permit others to do so.
*
*  Redistribution and use in source and binary forms, with or without modification, are permitted
*  provided that the following conditions are met:
*
*  1. Redistributions of source code must retain the above copyright notice, the above government
*  rights notice, this list of conditions and the following disclaimer.
*
*  2. Redistributions in binary form must reproduce the above copyright notice, the above government
*  rights notice, this list of conditions and the following disclaimer in the documentation and/or
*  other materials provided with the distribution.
*
*  3. The entire corresponding source code of any redistribution, with or without modification, by a
*  research entity, including but not limited to any contracting manager/operator of a United States
*  National Laboratory, any institution of higher learning, and any non-profit organization, must be
*  made publicly available under this license for as long as the redistribution is made available by
*  the research entity.
*
*  4. Redistribution of this software, without modification, must refer to the software by the same
*  designation. Redistribution of a modified version of this software (i) may not refer to the modified
*  version by the same designation, or by any confusingly similar designation, and (ii) must refer to
*  the underlying software originally provided by Alliance as “System Advisor Model” or “SAM”. Except
*  to comply with the foregoing, the terms “System Advisor Model”, “SAM”, or any confusingly similar
*  designation may not be used to refer to any modified version of this software or any modified
*  version of the underlying software originally provided by Alliance without the prior written consent
*  of Alliance.
*
*  5. The name of the copyright holder, contributors, the United States Government, the United States
*  Department of Energy, or any of their employees may not be used to endorse or promote products
*  derived from this software without specific prior written permission.
*
*  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR
*  IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND
*  FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER,
*  CONTRIBUTORS, UNITED STATES GOVERNMENT OR UNITED STATES DEPARTMENT OF ENERGY, NOR ANY OF THEIR
*  EMPLOYEES, BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
*  DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
*  DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER
*  IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF
*  THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*******************************************************************************************************/

/*******************************************************************************************************
* Implementation of the Mermoud/Thibault single-diode model
*
* SOURCES
* [1] André Mermoud and Thibault Lejeune, "Performance assessment of a simulation model for PV modules
*     of any available technology", 2010 (https://archive-ouverte.unige.ch/unige:38547)
* [2] John A. Duffie, "Solar Engineering of Thermal Processes", 4th Edition, 2013 by John Wiley & Sons
* [3] W. De Soto et al., “Improvement and validation of a model for photovoltaic array performance”,
*     Solar Energy, vol 80, pp. 78-88, 2006.
*******************************************************************************************************/

#include <math.h>
#include <cmath>

#include "lib_mlmodel.h"
// #include "mlm_spline.h"
#include "bsplinebuilder.h"
#include "datatable.h"

static const double k = 1.38064852e-23; // Boltzmann constant [J/K]
static const double q = 1.60217662e-19; // Elemenatry charge [C]
static const double T_0 = 273.15; // 0 degrees Celsius in Kelvin [K]
static const double PI = 3.1415926535897932; // pi

double amavec[5] = { 0.918093, 0.086257, -0.024459, 0.002816, -0.000126 }; // DeSoto IAM coefficients [3]

// 1: NOCT, 2: Extended Faiman
static const int T_MODE_NOCT = 1;
static const int T_MODE_FAIMAN = 2;

// 1: Use ASHRAE formula, 2: Use Sandia polynomial, 3: Use cubic spline with user-supplied data
static const int IAM_MODE_ASHRAE = 1;
static const int IAM_MODE_SANDIA = 2;
static const int IAM_MODE_SPLINE = 3;

// 1: Do not use AM correction 1: Use Sandia polynomial [corr=f(AM)], 2: Use standard coefficients from DeSoto model [3] [corr=f(AM)], 3: Use First Solar polynomial [corr=f(AM, p_wat)]
static const int AM_MODE_OFF = 1;
static const int AM_MODE_SANDIA = 2;
static const int AM_MODE_DESOTO = 3;
static const int AM_MODE_LEE_PANCHULA = 4;

mlmodel_module_t::mlmodel_module_t()
          {
	m_bspline3 = BSpline(1);
	Width = Length = V_mp_ref = I_mp_ref = V_oc_ref = I_sc_ref = S_ref = T_ref
		= R_shref = R_sh0 = R_shexp = R_s
		= alpha_isc = beta_voc_spec = E_g = n_0 = mu_n = D2MuTau = T_c_no_tnoct
		= T_c_fa_alpha = T_c_fa_U0 = T_c_fa_U1
		= groundRelfectionFraction = std::numeric_limits<double>::quiet_NaN();

	nVT = I_0ref = I_Lref = Vbi = 0;
	N_series = N_parallel = N_diodes = 0;

	isInitialized = false;
}

// IAM functions
double IAMvalue_ASHRAE(double b0, double theta)
{
	return (1 - b0 * (1 / cos(theta) - 1));
}
double IAMvalue_SANDIA(double coeff[], double theta)
{
	return coeff[0] + coeff[1] * theta + coeff[2] * pow(theta, 2) + coeff[3] * pow(theta, 3) + coeff[4] * pow(theta, 4) + coeff[5] * pow(theta, 5);
}

// Initialize - Calculates values that only need calculation once
void mlmodel_module_t::initializeManual()
{
	if (!isInitialized)
	{
		Vbi = 0.9 * N_series;
		// Calculate values of constant reference values.
		double R_sh_STC = R_shref + (R_sh0 - R_shref) * exp(-R_shexp * (S_ref / S_ref));

		nVT = N_series * n_0 * k * (T_ref + T_0) / q;

		I_0ref = (I_sc_ref + (I_sc_ref * R_s - V_oc_ref) / R_sh_STC) / ((exp(V_oc_ref / nVT) - 1) - (exp((I_sc_ref * R_s) / nVT) - 1));
		I_Lref = I_0ref * (exp(V_oc_ref / nVT) - 1) + V_oc_ref / R_sh_STC;

		//double I_sc_ref_string = I_sc_ref; // / N_parallel;
		//I_0ref = (I_sc_ref_string + (I_sc_ref_string * R_s - V_oc_ref) / R_sh_STC) / ((exp(V_oc_ref / nVT) - 1) - (exp((I_sc_ref_string * R_s) / nVT) - 1));
		//I_Lref = I_0ref * (exp(V_oc_ref / nVT) - 1) + V_oc_ref / R_sh_STC;

		// set up IAM spline
		if (IAM_mode == IAM_MODE_SPLINE)
		{
			/*
			std::vector<double> X;
			std::vector<double> Y;
			X.clear();
			Y.clear();
			for (int i = 0; i <= IAM_c_cs_elements - 1; i = i + 1) {
				X.push_back(IAM_c_cs_incAngle[i]);
				Y.push_back(IAM_c_cs_iamValue[i]);
			}
			iamSpline.set_points(X, Y);
			*/
			DataTable samples;
			for (int i = 0; i <= IAM_c_cs_elements - 1; i = i + 1) {
				samples.addSample(IAM_c_cs_incAngle[i], IAM_c_cs_iamValue[i]);
			}
			m_bspline3 = BSpline::Builder(samples).degree(3).build();

			isInitialized = true;
		}
	}
}

// Main module model
bool mlmodel_module_t::operator() (pvinput_t &input, double T_C, double opvoltage, pvoutput_t &out)
{
	// initialize output first
	out.Power = out.Voltage = out.Current = out.Efficiency = out.Voc_oper = out.Isc_oper = 0.0;
	
	// Incidence Angle Modifier
	double f_IAM_beam = 0, f_IAM_diff = 0, f_IAM_gnd = 0;
	double theta_beam = input.IncAng;
	double theta_diff = (59.7 - 0.1388 * input.Tilt + 0.001497 * pow(input.Tilt, 2)); // from [2], equation 5.4.2
	double theta_gnd = (90.0 - 0.5788 * input.Tilt + 0.002693 * pow(input.Tilt, 2)); // from [2], equation 5.4.1

	switch (IAM_mode)
	{
		case IAM_MODE_ASHRAE:
			f_IAM_beam = IAMvalue_ASHRAE(IAM_c_as, theta_beam / 180 * PI);
			f_IAM_diff = IAMvalue_ASHRAE(IAM_c_as, theta_diff / 180 * PI);
			f_IAM_gnd = IAMvalue_ASHRAE(IAM_c_as, theta_gnd / 180 * PI);
			break;
		case IAM_MODE_SANDIA:
			f_IAM_beam = IAMvalue_SANDIA(IAM_c_sa, theta_beam / 180 * PI);
			f_IAM_diff = IAMvalue_SANDIA(IAM_c_sa, theta_diff / 180 * PI);
			f_IAM_gnd = IAMvalue_SANDIA(IAM_c_sa, theta_gnd / 180 * PI);
			break;
		case IAM_MODE_SPLINE:
//			f_IAM_beam = std::min(iamSpline(theta_beam), 1.0);
//			f_IAM_diff = std::min(iamSpline(theta_diff), 1.0);
//			f_IAM_gnd = std::min(iamSpline(theta_gnd), 1.0);
			DenseVector x(1);
			x(0) = theta_beam;
			f_IAM_beam = std::min(m_bspline3.eval(x), 1.0);
			x(0) = theta_diff;
			f_IAM_diff = std::min(m_bspline3.eval(x), 1.0);
			x(0) = theta_gnd;
			f_IAM_gnd = std::min(m_bspline3.eval(x), 1.0);
			break;
	}

	// Spectral correction function
	double f_AM = 0;
	switch (AM_mode)
	{
	case AM_MODE_OFF:
			f_AM = 1.0;
			break;
		case AM_MODE_SANDIA:
			f_AM = air_mass_modifier(input.Zenith, input.Elev, AM_c_sa);
			break;
		case AM_MODE_DESOTO:
			f_AM = air_mass_modifier(input.Zenith, input.Elev, amavec);
			break;
		case AM_MODE_LEE_PANCHULA:
			f_AM = -1; // TO BE ADDED
			break;
	}

	// Total effective irradiance
	double S;
	if(input.radmode != 3){ // Skip module cover effects if using POA reference cell data
		S = (f_IAM_beam * input.Ibeam + f_IAM_diff * input.Idiff + groundRelfectionFraction * f_IAM_gnd * input.Ignd) * f_AM;
    }
    else if(input.usePOAFromWF){ // Check if decomposed POA is required, if not use weather file POA directly
		S = input.poaIrr;
	}
    else { // Otherwise use decomposed POA
		S = (f_IAM_beam * input.Ibeam + f_IAM_diff * input.Idiff + groundRelfectionFraction * f_IAM_gnd * input.Ignd) * f_AM;
	}

	// Single diode model acc. to [1]
	if (S >= 1)
	{
		double n=0.0, a=0.0, I_L=0.0, I_0=0.0, R_sh=0.0, I_sc=0.0;
		double V_oc = V_oc_ref; // V_oc_ref as initial guess
		double P=0.0, V=0.0, I=0.0, eff=0.0;
		double T_cell = T_C;
		int iterations=0;

		if (T_mode == T_MODE_FAIMAN) {
			iterations = 1; // 2; // two iterations, 1st with guessed eff, 2nd with calculated efficiency
			eff = (I_mp_ref * V_mp_ref) / ((Width * Length) * S_ref); // efficiency guess for initial run
		}
		else {
			iterations = 1;
		}

		for (int i = 1; i <= iterations; i = i + 1) {
			if (T_mode == T_MODE_FAIMAN) {
				// T_cell = input.Tdry + (T_c_fa_alpha * G_total * (1 - eff)) / (T_c_fa_U0 + input.Wspd * T_c_fa_U1);
				T_cell = input.Tdry + (T_c_fa_alpha * S * (1 - eff)) / (T_c_fa_U0 + input.Wspd * T_c_fa_U1);
			}

			n = n_0 + mu_n * (T_cell - T_ref);
			a = N_series * k * (T_cell + T_0) * n / q;
			I_L = (S / S_ref) * (I_Lref + alpha_isc * (T_cell - T_ref));
			//I_L = (S / S_ref) * (I_Lref + alpha_isc / N_parallel * (T_cell - T_ref));
			I_0 = I_0ref * pow(((T_cell + T_0) / (T_ref + T_0)), 3) * exp((q * E_g) / (n * k) * (1 / (T_ref + T_0) - 1 / (T_cell + T_0)));

			R_sh = R_shref + (R_sh0 - R_shref) * exp(-R_shexp * (S / S_ref));

			V_oc = openvoltage_5par_rec(V_oc, a, I_L, I_0, R_sh, D2MuTau, Vbi);
			I_sc = I_L / (1 + R_s / R_sh);

			if (opvoltage < 0)
			{
				P = maxpower_5par_rec(V_oc, a, I_L, I_0, R_s, R_sh, D2MuTau, Vbi, &V, &I);
			}
			else
			{ // calculate power at specified operating voltage
				V = opvoltage;

				if (V >= V_oc) I = 0;
				else I = current_5par_rec(V, 0.9*I_L, a, I_L, I_0, R_s, R_sh, D2MuTau, Vbi);
				P = V*I;
			}
			eff = P / ((Width * Length) * (input.Ibeam + input.Idiff + input.Ignd));
		}

		out.Power = P;
		out.Voltage = V;
		out.Current = I;
		out.Efficiency = eff;
		out.Voc_oper = V_oc;
		out.Isc_oper = I_sc;
		out.CellTemp = T_cell;
		out.AOIModifier = S / (input.Ibeam + input.Idiff + input.Ignd);
	}

	return out.Power >= 0;
}

// mockup cell temperature model
// to be used in cases when Tcell is calculated within the module model
bool mock_celltemp_t::operator() (pvinput_t &, pvmodule_t &, double, double &Tcell)
{
	Tcell = -999;
	return true;
}
