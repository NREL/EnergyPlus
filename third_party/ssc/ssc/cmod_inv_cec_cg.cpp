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
#include <vector>
#include <sstream>

#include "lsqfit.h"


  
static var_info vtab_inv_cec_cg[] = {

/*   VARTYPE           DATATYPE         NAME                         LABEL                                           UNITS     META                      GROUP          REQUIRED_IF                 CONSTRAINTS                      UI_HINTS*/
	{ SSC_INPUT, SSC_NUMBER, "inv_cec_cg_paco", "Rated max output", "W", "", "", "*", "", "" },

	{ SSC_INPUT, SSC_NUMBER, "inv_cec_cg_sample_power_units", "Sample data units for power output", "0=W,1=kW", "", "", "?=0", "INTEGER,MIN=0,MAX=1", "" },
	// each sample has 18x3 entries:
	// 6 output power percentages 10%, 20%, 30%, 50%, 75%, 100% of rated power
	// 3 voltages Vmin, Vnom, Vmax that the 6 output powers measured at
	// for a total of 18 (=6x3) rows
	// 3 measured values for each row - Output Power, Input Voltage and Efficiency
	{ SSC_INPUT, SSC_MATRIX, "inv_cec_cg_test_samples", "Sample data", "", "", "", "*", "", "" },
	
	/* from pvsamv1
		{ SSC_INPUT,        SSC_NUMBER,      "mppt_low_inverter",                           "Minimum inverter MPPT voltage window",                    "Vdc",     "",                     "pvsamv1",       "",                    "?=0",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "mppt_hi_inverter",                            "Maximum inverter MPPT voltage window",                    "Vdc",     "",                     "pvsamv1",       "",                    "?=0",                              "" },

	{ SSC_INPUT,        SSC_NUMBER,      "inv_snl_c0",                                  "Curvature between ac-power and dc-power at ref",          "1/W",     "",                     "pvsamv1",       "inverter_model=0",                    "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "inv_snl_c1",                                  "Coefficient of Pdco variation with dc input voltage",     "1/V",     "",                     "pvsamv1",       "inverter_model=0",                    "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "inv_snl_c2",                                  "Coefficient of Pso variation with dc input voltage",      "1/V",     "",                     "pvsamv1",       "inverter_model=0",                    "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "inv_snl_c3",                                  "Coefficient of Co variation with dc input voltage",       "1/V",     "",                     "pvsamv1",       "inverter_model=0",                    "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "inv_snl_paco",                                "AC maximum power rating",                                 "Wac",     "",                     "pvsamv1",       "inverter_model=0",                    "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "inv_snl_pdco",                                "DC input power at which ac-power rating is achieved",     "Wdc",     "",                     "pvsamv1",       "inverter_model=0",                    "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "inv_snl_pnt",                                 "AC power consumed by inverter at night",                  "Wac",     "",                     "pvsamv1",       "inverter_model=0",                    "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "inv_snl_pso",                                 "DC power required to enable the inversion process",       "Wdc",     "",                     "pvsamv1",       "inverter_model=0",                    "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "inv_snl_vdco",                                "DC input voltage for the rated ac-power rating",          "Vdc",     "",                     "pvsamv1",       "inverter_model=0",                    "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "inv_snl_vdcmax",                              "Maximum dc input operating voltage",                      "Vdc",     "",                     "pvsamv1",       "inverter_model=0",                    "",                              "" },
	*/

	// intermediate outputs for testing and validation

	// test data reorganized
	{ SSC_OUTPUT, SSC_MATRIX, "inv_cec_cg_Vmin", "Vmin for least squares fit", "", "", "", "*", "", "" },
	{ SSC_OUTPUT, SSC_MATRIX, "inv_cec_cg_Vnom", "Vnom for least squares fit", "", "", "", "*", "", "" },
	{ SSC_OUTPUT, SSC_MATRIX, "inv_cec_cg_Vmax", "Vmax for least squares fit", "", "", "", "*", "", "" },

	// quadratic fits
	{ SSC_OUTPUT, SSC_ARRAY, "inv_cec_cg_Vmin_abc", "Vmin a,b,c for least squares fit", "", "", "", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "inv_cec_cg_Vnom_abc", "Vnom a,b,c for least squares fit", "", "", "", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "inv_cec_cg_Vmax_abc", "Vmax a,b,c for least squares fit", "", "", "", "*", "", "" },

	//intermediates based on quadratic least squares
	{ SSC_OUTPUT, SSC_ARRAY, "inv_cec_cg_Vdc", "Vdc at Vmin, Vnom, Vmax", "", "", "", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "inv_cec_cg_Vdc_Vnom", "Vdc - Vnom at Vmin, Vnom, Vmax", "", "", "", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "inv_cec_cg_Pdco", "Pdco at Vmin, Vnom, Vmax", "", "", "", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "inv_cec_cg_Psco", "Psco at Vmin, Vnom, Vmax", "", "", "", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "inv_cec_cg_C0", "C0 at Vmin, Vnom, Vmax", "", "", "", "*", "", "" },

	// intermediates based on linear least squares
	{ SSC_OUTPUT, SSC_ARRAY, "inv_cec_cg_C1", "C1 at m and b", "", "", "", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "inv_cec_cg_C2", "C1 at m and b", "", "", "", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "inv_cec_cg_C3", "C1 at m and b", "", "", "", "*", "", "" },

	// outputs Pdco, Vdco, Pso, c0, c1, c2, c3
	{ SSC_OUTPUT, SSC_NUMBER, "Pdco", "CEC generated Pdco", "Wac", "", "", "*", "", "" },
	{ SSC_OUTPUT, SSC_NUMBER, "Vdco", "CEC generated Vdco", "Vdc", "", "", "*", "", "" },
	{ SSC_OUTPUT, SSC_NUMBER, "Pso", "CEC generated Pso", "Wdc", "", "", "*", "", "" },
	{ SSC_OUTPUT, SSC_NUMBER, "c0", "CEC generated c0", "1/W", "", "", "*", "", "" },
	{ SSC_OUTPUT, SSC_NUMBER, "c1", "CEC generated c1", "1/V", "", "", "*", "", "" },
	{ SSC_OUTPUT, SSC_NUMBER, "c2", "CEC generated c2", "1/V", "", "", "*", "", "" },
	{ SSC_OUTPUT, SSC_NUMBER, "c3", "CEC generated c3", "1/V", "", "", "*", "", "" },

	var_info_invalid };


double Quadratic_fit_eqn(double _x, double *par, void *)
{
	return par[0] * _x * _x + par[1] * _x + par[2];
}

double Linear_fit_eqn(double _x, double *par, void *)
{
	return par[0] * _x + par[1];
}

class cm_inv_cec_cg : public compute_module
{
private:

public:
	cm_inv_cec_cg()
	{
		add_var_info( vtab_inv_cec_cg);
	}

	void exec( )
	{
		size_t i, j, nrows, ncols; 
		// rated output ac
		double Paco = as_double("inv_cec_cg_paco");
		bool kW_units = (as_integer("inv_cec_cg_sample_power_units") == 1);

		// 6 columns period, tier, max usage, max usage units, buy, sell
		ssc_number_t *inv_cec_cg_test_samples_in = as_matrix("inv_cec_cg_test_samples", &nrows, &ncols);
		if (nrows != 18)
		{
			std::ostringstream ss;
			ss << "The samples table must have 18 rows. Number of rows in samples table provided is " << nrows << " rows.";
			throw exec_error("inv_cec_cg", ss.str());
		}
		if ((ncols % 3) != 0)
		{
			std::ostringstream ss;
			ss << "The samples table must have number of columns divisible by 3. Number of columns in samples table provided is " << ncols << " columns.";
			throw exec_error("inv_cec_cg", ss.str());
		}
		size_t num_samples = ncols / 3;
		size_t columns_per_sample = 3;
		util::matrix_t<ssc_number_t> inv_cec_cg_test_samples(nrows, ncols);
		inv_cec_cg_test_samples.assign(inv_cec_cg_test_samples_in, nrows, ncols);

		ssc_number_t vdc = 0, Pout = 0, eff = 0, Pin=0, Pin2=0;

		// set Pout, Pin=Pout/eff and Pin^2 for least squares fit
		// 6 is for the required power output percentages at each voltage for each sample
		util::matrix_t<ssc_number_t> &inv_cec_cg_Vmin = allocate_matrix("inv_cec_cg_Vmin", 6 * num_samples, 3);
		util::matrix_t<ssc_number_t> &inv_cec_cg_Vnom = allocate_matrix("inv_cec_cg_Vnom", 6 * num_samples, 3);
		util::matrix_t<ssc_number_t> &inv_cec_cg_Vmax = allocate_matrix("inv_cec_cg_Vmax", 6 * num_samples, 3);


		ssc_number_t *inv_cec_cg_Vdc = allocate("inv_cec_cg_Vdc", 3);
		ssc_number_t *inv_cec_cg_Vdc_Vnom = allocate("inv_cec_cg_Vdc_Vnom", 3);
		ssc_number_t *inv_cec_cg_Pdco = allocate("inv_cec_cg_Pdco", 3);
		ssc_number_t *inv_cec_cg_Psco = allocate("inv_cec_cg_Psco", 3);
		ssc_number_t *inv_cec_cg_C0 = allocate("inv_cec_cg_C0", 3);
		ssc_number_t *inv_cec_cg_C1 = allocate("inv_cec_cg_C1", 2);
		ssc_number_t *inv_cec_cg_C2 = allocate("inv_cec_cg_C2", 2);
		ssc_number_t *inv_cec_cg_C3 = allocate("inv_cec_cg_C3", 2);

		for (i = 0; i < 3; i++)
			inv_cec_cg_Vdc[i] = 0;

		for (j = 0; j < num_samples; j++)
		{
			for (i = 0; i < inv_cec_cg_test_samples.nrows(); i++)
			{
				vdc = inv_cec_cg_test_samples.at(i, j*columns_per_sample + 1);
				Pout = inv_cec_cg_test_samples.at(i, j*columns_per_sample);
				if (kW_units) Pout *= 1000; // kW to W
				eff = inv_cec_cg_test_samples.at(i, j*columns_per_sample+2);
				Pin = Pout;
				if (eff != 0.0f) Pin = (ssc_number_t)(100.0*Pout) / eff;
				Pin2 = Pin*Pin;
				if (i < 6) // Vmin 0 offset
				{
					inv_cec_cg_Vdc[0] += vdc;
					inv_cec_cg_Vmin.at(j * 6 + i, 0) = Pout;
					inv_cec_cg_Vmin.at(j * 6 + i, 1) = Pin;
					inv_cec_cg_Vmin.at(j * 6 + i, 2) = Pin2;
				}
				else if (i < 12) // Vnom 6 offset 
				{
					inv_cec_cg_Vdc[1] += vdc;
					inv_cec_cg_Vnom.at(j * 6 + i - 6, 0) = Pout;
					inv_cec_cg_Vnom.at(j * 6 + i-6, 1) = Pin;
					inv_cec_cg_Vnom.at(j * 6 + i-6, 2) = Pin2;
				}
				else // Vmax 12 offset
				{
					inv_cec_cg_Vdc[2] += vdc;
					inv_cec_cg_Vmax.at(j * 6 + i - 12, 0) = Pout;
					inv_cec_cg_Vmax.at(j * 6 + i - 12, 1) = Pin;
					inv_cec_cg_Vmax.at(j * 6 + i - 12, 2) = Pin2;
				}
			}
		}
		
		

		ssc_number_t *inv_cec_cg_Vmin_abc = allocate("inv_cec_cg_Vmin_abc", 3);
		ssc_number_t *inv_cec_cg_Vnom_abc = allocate("inv_cec_cg_Vnom_abc", 3);
		ssc_number_t *inv_cec_cg_Vmax_abc = allocate("inv_cec_cg_Vmax_abc", 3);


		std::vector<double> Pout_vec(inv_cec_cg_Vmin.nrows());
		std::vector<double> Pin_vec(inv_cec_cg_Vmin.nrows());
		int info;
		double C[3];// initial guesses for lsqfit
		size_t data_size = 3;

		// Vmin non-linear
		for (i = 0; i < inv_cec_cg_Vmin.nrows(); i++)
		{
			Pin_vec[i] = inv_cec_cg_Vmin.at(i, 1);
			Pout_vec[i] = inv_cec_cg_Vmin.at(i, 0);
		}
		C[0] = -1e-6;
		C[1] = 1;
		C[2] = 1e3;

		info = lsqfit(Quadratic_fit_eqn, 0, C, data_size, &Pin_vec[0], &Pout_vec[0], inv_cec_cg_Vmin.nrows());
		if (!info)
		{
			throw exec_error("inv_cec_cg", util::format("error in nonlinear least squares fit, error %d", info));
			return;
		}
		inv_cec_cg_Vmin_abc[0] = (ssc_number_t)C[0];
		inv_cec_cg_Vmin_abc[1] = (ssc_number_t)C[1];
		inv_cec_cg_Vmin_abc[2] = (ssc_number_t)C[2];

		// Vnom non-linear
		for (i = 0; i < inv_cec_cg_Vnom.nrows(); i++)
		{
			Pin_vec[i] = inv_cec_cg_Vnom.at(i, 1);
			Pout_vec[i] = inv_cec_cg_Vnom.at(i, 0);
		}
		C[0] = -1e-6;
		C[1] = 1;
		C[2] = 1e3;

		info = lsqfit(Quadratic_fit_eqn, 0, C, data_size, &Pin_vec[0], &Pout_vec[0], inv_cec_cg_Vnom.nrows());
		if (!info)
		{
			throw exec_error("inv_cec_cg", util::format("error in nonlinear least squares fit, error %d", info));
			return;
		}
		inv_cec_cg_Vnom_abc[0] = (ssc_number_t)C[0];
		inv_cec_cg_Vnom_abc[1] = (ssc_number_t)C[1];
		inv_cec_cg_Vnom_abc[2] = (ssc_number_t)C[2];

		// Vmax non-linear
		for (i = 0; i < inv_cec_cg_Vmax.nrows(); i++)
		{
			Pin_vec[i] = inv_cec_cg_Vmax.at(i, 1);
			Pout_vec[i] = inv_cec_cg_Vmax.at(i, 0);
		}
		C[0] = -1e-6;
		C[1] = 1;
		C[2] = 1e3;
		info = lsqfit(Quadratic_fit_eqn, 0, C, data_size, &Pin_vec[0], &Pout_vec[0], inv_cec_cg_Vmax.nrows());
		if (!info)
		{
			throw exec_error("inv_cec_cg", util::format("error in nonlinear least squares fit, error %d", info));
			return;
		}
		inv_cec_cg_Vmax_abc[0] = (ssc_number_t)C[0];
		inv_cec_cg_Vmax_abc[1] = (ssc_number_t)C[1];
		inv_cec_cg_Vmax_abc[2] = (ssc_number_t)C[2];

		// Fill in intermediate values
		//Vdc (Vmin, Vnom, Vmax)
		for (i = 0; i < 3;i++)
			inv_cec_cg_Vdc[i] /= (6 * num_samples);

		// Vdc-Vnom
		for (i = 0; i < 3; i++)
			inv_cec_cg_Vdc_Vnom[i] = inv_cec_cg_Vdc[i] - inv_cec_cg_Vdc[1];

		ssc_number_t a, b, c;
		// Pdco and Psco and C0
		a = inv_cec_cg_Vmin_abc[0];
		b = inv_cec_cg_Vmin_abc[1];
		c = inv_cec_cg_Vmin_abc[2];
		inv_cec_cg_Pdco[0] = (ssc_number_t)(-b + sqrt(b*b - 4 * a*(c - Paco)));
		inv_cec_cg_Psco[0] = (-b + sqrt(b*b - 4 * a*c));
		inv_cec_cg_C0[0] = a;
		if (a != 0)
		{
			inv_cec_cg_Pdco[0] /= (ssc_number_t)(2.0*a);
			inv_cec_cg_Psco[0] /= (ssc_number_t)(2.0*a);
		}

		a = inv_cec_cg_Vnom_abc[0];
		b = inv_cec_cg_Vnom_abc[1];
		c = inv_cec_cg_Vnom_abc[2];
		inv_cec_cg_Pdco[1] = (ssc_number_t)(-b + sqrt(b*b - 4 * a*(c - Paco)));
		inv_cec_cg_Psco[1] = (-b + sqrt(b*b - 4 * a*c));
		inv_cec_cg_C0[1] = a;
		if (a != 0)
		{
			inv_cec_cg_Pdco[1] /= (ssc_number_t)(2.0*a);
			inv_cec_cg_Psco[1] /= (ssc_number_t)(2.0*a);
		}

		// TODO - limit Psco max to not be less than zero per note in Workbook
		a = inv_cec_cg_Vmax_abc[0];
		b = inv_cec_cg_Vmax_abc[1];
		c = inv_cec_cg_Vmax_abc[2];
		inv_cec_cg_Pdco[2] = (ssc_number_t)(-b + sqrt(b*b - 4 * a*(c - Paco)));
		inv_cec_cg_Psco[2] = (-b + sqrt(b*b - 4 * a*c));
		inv_cec_cg_C0[2] = a;
		if (a != 0)
		{
			inv_cec_cg_Pdco[2] /= (ssc_number_t)(2.0*a);
			inv_cec_cg_Psco[2] /= (ssc_number_t)(2.0*a);
		}

		// C1, C2, C3 linear least squares
		// C1 Y=Pdco, X=Vdc-Vnom
		std::vector<double> X(3);
		std::vector<double> Y(3);
		double slope, intercept;

		// C1 using linear least squares fit
		for (i = 0; i < 3; i++)
		{
			X[i] = inv_cec_cg_Vdc_Vnom[i];
			Y[i] = inv_cec_cg_Pdco[i];
		}
		info = linlsqfit(&slope, &intercept, &X[0], &Y[0], data_size);
		if (info)
		{
			throw exec_error("inv_cec_cg", util::format("error in linear least squares fit, error %d", info));
			return;
		}
		inv_cec_cg_C1[0] = (ssc_number_t)slope;
		inv_cec_cg_C1[1] = (ssc_number_t)intercept;


		// C2 using linear least squares fit
		for (i = 0; i < 3; i++)
		{
			X[i] = inv_cec_cg_Vdc_Vnom[i];
			Y[i] = inv_cec_cg_Psco[i];
		}
		info = linlsqfit(&slope, &intercept, &X[0], &Y[0], data_size);
		if (info)
		{
			throw exec_error("inv_cec_cg", util::format("error in linear least squares fit, error %d", info));
			return;
		}
		inv_cec_cg_C2[0] = (ssc_number_t)slope;
		inv_cec_cg_C2[1] = (ssc_number_t)intercept;

		// C2 using linear least squares fit
		for (i = 0; i < 3; i++)
		{
			X[i] = inv_cec_cg_Vdc_Vnom[i];
			Y[i] = inv_cec_cg_C0[i];
		}
		info = linlsqfit(&slope, &intercept, &X[0], &Y[0], data_size);
		if (info)
		{
			throw exec_error("inv_cec_cg", util::format("error in linear least squares fit, error %d", info));
			return;
		}
		inv_cec_cg_C3[0] = (ssc_number_t)slope;
		inv_cec_cg_C3[1] = (ssc_number_t)intercept;

		// vdco is the average of Vnom of all samples column 2 and rows 7 through 12

		assign("Pdco", (var_data)inv_cec_cg_C1[1]);
		assign("Vdco", (var_data)inv_cec_cg_Vdc[1]);
		assign("Pso", (var_data)inv_cec_cg_C2[1]);
		assign("c0", (var_data)inv_cec_cg_C3[1]);
		assign("c1", (var_data)(inv_cec_cg_C1[0] / inv_cec_cg_C1[1]));
		assign("c2", (var_data)(inv_cec_cg_C2[0] / inv_cec_cg_C2[1]));
		assign("c3", (var_data)(inv_cec_cg_C3[0] / inv_cec_cg_C3[1]));
	}


};

DEFINE_MODULE_ENTRY( inv_cec_cg, "CEC Inverter Coefficient Generator", 1 );


