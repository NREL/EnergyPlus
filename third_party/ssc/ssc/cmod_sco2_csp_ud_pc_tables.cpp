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
#include "common.h"

#include "csp_common.h"

#include "sco2_pc_csp_int.h"

static var_info _cm_vtab_sco2_csp_ud_pc_tables[] = {

	/*   VARTYPE   DATATYPE         NAME               LABEL                                                    UNITS     META  GROUP REQUIRED_IF CONSTRAINTS     UI_HINTS*/
	// Off Design UDPC Options
	{ SSC_INPUT,  SSC_NUMBER,  "is_generate_udpc",     "1 = generate udpc tables, 0 = only calculate design point cyle", "",   "",    "",      "?=1",   "",       "" },
	{ SSC_INPUT,  SSC_NUMBER,  "is_apply_default_htf_mins", "1 = yes (0.5 rc, 0.7 simple), 0 = no, only use 'm_dot_htf_ND_low'", "", "", "",   "?=1",   "",       "" },
	// User Defined Power Cycle Table Inputs
	{ SSC_INOUT,  SSC_NUMBER,  "T_htf_hot_low",        "Lower level of HTF hot temperature",					  "C",         "",    "",      "",     "",       "" },
	{ SSC_INOUT,  SSC_NUMBER,  "T_htf_hot_high",	   "Upper level of HTF hot temperature",					  "C",		   "",    "",      "",     "",       "" },
	{ SSC_INOUT,  SSC_NUMBER,  "n_T_htf_hot",		   "Number of HTF hot temperature parametric runs",			  "",		   "",    "",      "",     "",       "" },
	{ SSC_INOUT,  SSC_NUMBER,  "T_amb_low",			   "Lower level of ambient temperature",					  "C",		   "",    "",      "",     "",       "" },
	{ SSC_INOUT,  SSC_NUMBER,  "T_amb_high",		   "Upper level of ambient temperature",					  "C",		   "",    "",      "",     "",       "" },
	{ SSC_INOUT,  SSC_NUMBER,  "n_T_amb",			   "Number of ambient temperature parametric runs",			  "",		   "",    "",      "",     "",       "" },
	{ SSC_INOUT,  SSC_NUMBER,  "m_dot_htf_ND_low",	   "Lower level of normalized HTF mass flow rate",			  "",		   "",    "",      "",     "",       "" },
	{ SSC_INOUT,  SSC_NUMBER,  "m_dot_htf_ND_high",	   "Upper level of normalized HTF mass flow rate",			  "",		   "",    "",      "",     "",       "" },
	{ SSC_INOUT,  SSC_NUMBER,  "n_m_dot_htf_ND",	   "Number of normalized HTF mass flow rate parametric runs", "",		   "",    "",      "",     "",       "" },

	// Power Cycle Tables
	{ SSC_OUTPUT, SSC_MATRIX,  "T_htf_ind",            "Parametric of HTF temperature w/ ND HTF mass flow rate levels",     "",       "",    "",      "?=[[0,1,2,3,4,5,6,7,8,9,10,11,12][0,1,2,3,4,5,6,7,8,9,10,11,12]]",     "",       "" },
	{ SSC_OUTPUT, SSC_MATRIX,  "T_amb_ind",            "Parametric of ambient temp w/ HTF temp levels",                     "",       "",    "",      "?=[[0,1,2,3,4,5,6,7,8,9,10,11,12][0,1,2,3,4,5,6,7,8,9,10,11,12]]",     "",       "" },
	{ SSC_OUTPUT, SSC_MATRIX,  "m_dot_htf_ND_ind",     "Parametric of ND HTF mass flow rate w/ ambient temp levels",        "",       "",    "",      "?=[[0,1,2,3,4,5,6,7,8,9,10,11,12][0,1,2,3,4,5,6,7,8,9,10,11,12]]",     "",       "" },

	var_info_invalid };

int test_mono_function(double x, double *y);

class cm_sco2_csp_ud_pc_tables : public compute_module
{
public:

	cm_sco2_csp_ud_pc_tables()
	{
		add_var_info(vtab_sco2_design);
		
		add_var_info(_cm_vtab_sco2_csp_ud_pc_tables);
	}

	void exec() override
	{
		C_sco2_phx_air_cooler c_sco2_cycle;

		int sco2_des_err = sco2_design_cmod_common(this, c_sco2_cycle);
		if (sco2_des_err != 0)
			return;

		double sco2_f_min = 0.5;

		double m_dot_htf_ND_low = sco2_f_min;;
		if (is_assigned("m_dot_htf_ND_low"))
		{
			if (as_boolean("is_apply_default_htf_mins"))
				m_dot_htf_ND_low = std::max(sco2_f_min, as_double("m_dot_htf_ND_low"));	//[-]
			else
				m_dot_htf_ND_low = as_double("m_dot_htf_ND_low");
		}

		assign("m_dot_htf_ND_low", m_dot_htf_ND_low);

		if (as_integer("is_generate_udpc") == 0)
		{
			log("\n Design calculations complete; no off-design cases requested");
			return;
		}

		// Get or calculate user-defined power cycle parameters
		double T_htf_hot_low = c_sco2_cycle.get_design_par()->m_T_htf_hot_in - 273.15 - 30.0;		//[C]
		if (is_assigned("T_htf_hot_low"))
		{
			T_htf_hot_low = as_double("T_htf_hot_low");		//[C]
		}
		assign("T_htf_hot_low", T_htf_hot_low);

		double T_htf_hot_high = c_sco2_cycle.get_design_par()->m_T_htf_hot_in - 273.15 + 15.0;	//[C]
		if (is_assigned("T_htf_hot_high"))
		{
			T_htf_hot_high = as_double("T_htf_hot_high");	//[C]
		}
		assign("T_htf_hot_high", T_htf_hot_high);

		int n_T_htf_hot_in = 4;
		if (is_assigned("n_T_htf_hot"))
		{
			n_T_htf_hot_in = as_integer("n_T_htf_hot");			//[-]
		}
		assign("n_T_htf_hot", n_T_htf_hot_in);

		double T_amb_low = 0.0;
		if (is_assigned("T_amb_low"))
		{
			T_amb_low = as_double("T_amb_low");				//[C]
		}
		assign("T_amb_low", T_amb_low);

		double T_amb_high = std::max(45.0, c_sco2_cycle.get_design_par()->m_T_amb_des-273.15 + 5.0);
		if (is_assigned("T_amb_high"))
		{
			T_amb_high = as_double("T_amb_high");			//[C]
		}
		assign("T_amb_high", T_amb_high);

		int n_T_amb_in = std::round((T_amb_high - T_amb_low) / 2.0) + 1;     //[-]
		if (is_assigned("n_T_amb"))
		{
			n_T_amb_in = as_integer("n_T_amb");					//[-]
		}		
		assign("n_T_amb", n_T_amb_in);

		double m_dot_htf_ND_high = 1.05;
		if (is_assigned("m_dot_htf_ND_high"))
		{
			m_dot_htf_ND_high = as_double("m_dot_htf_ND_high");
		}
		assign("m_dot_htf_ND_high", m_dot_htf_ND_high);

		int n_m_dot_htf_ND_in = 10;
		if (is_assigned("n_m_dot_htf_ND"))
		{
			n_m_dot_htf_ND_in = as_integer("n_m_dot_htf_ND");
		}
		assign("n_m_dot_htf_ND", n_m_dot_htf_ND_in);

		if (n_T_htf_hot_in < 3 || n_T_amb_in < 3 || n_m_dot_htf_ND_in < 3)
		{
			throw exec_error("sco2_csp_ud_pc_tables", "Need at 3 three points for each independent variable");
		}

		util::matrix_t<double> T_htf_parametrics, T_amb_parametrics, m_dot_htf_ND_parametrics;

		// For try/catch below
		int out_type = -1;
		std::string out_msg = "";

        double od_opt_tol = 1.E-3;
        double od_tol = 1.E-3;

		try
		{
			c_sco2_cycle.generate_ud_pc_tables(T_htf_hot_low, T_htf_hot_high, n_T_htf_hot_in,
							T_amb_low, T_amb_high, n_T_amb_in,
							m_dot_htf_ND_low, m_dot_htf_ND_high, n_m_dot_htf_ND_in,
							T_htf_parametrics, T_amb_parametrics, m_dot_htf_ND_parametrics,
                            od_opt_tol, od_tol);
		}
		catch( C_csp_exception &csp_exception )
		{
			// Report warning before exiting with error
			while (c_sco2_cycle.mc_messages.get_message(&out_type, &out_msg))
			{
				log(out_msg);
			}

			throw exec_error("sco2_csp_system", csp_exception.m_error_message);
		}

		int n_T_htf_hot = (int)T_htf_parametrics.nrows();
		int n_T_amb = (int)T_amb_parametrics.nrows();
		int n_m_dot_htf_ND = (int)m_dot_htf_ND_parametrics.nrows();

		int ncols = (int)T_htf_parametrics.ncols();

		ssc_number_t *p_T_htf_ind = allocate("T_htf_ind", n_T_htf_hot, ncols);
		for(int i = 0; i < n_T_htf_hot; i++)
		{
			for(int j = 0; j < ncols; j++)
			{
				p_T_htf_ind[i*ncols + j] = (ssc_number_t)T_htf_parametrics(i, j);
			}
		}

		ssc_number_t *p_T_amb_ind = allocate("T_amb_ind", n_T_amb, ncols);
		for(int i = 0; i < n_T_amb; i++)
		{
			for(int j = 0; j < ncols; j++)
			{
				p_T_amb_ind[i*ncols + j] = (ssc_number_t)T_amb_parametrics(i, j);
			}
		}

		ssc_number_t *p_m_dot_htf_ND_ind = allocate("m_dot_htf_ND_ind", n_m_dot_htf_ND, ncols);
		for(int i = 0; i < n_m_dot_htf_ND; i++)
		{
			for(int j = 0; j < ncols; j++)
			{
				p_m_dot_htf_ND_ind[i*ncols + j] = (ssc_number_t)m_dot_htf_ND_parametrics(i, j);
			}
		}

		// If all calls were successful, log to SSC any messages from sco2_recomp_csp
		while (c_sco2_cycle.mc_messages.get_message(&out_type, &out_msg))
		{
			log(out_msg);
		}
		
		log("\n UDPC tables complete");
	}

};

DEFINE_MODULE_ENTRY(sco2_csp_ud_pc_tables, "...", 0)
