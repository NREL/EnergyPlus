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

#include "heat_exchangers.h"

static var_info _cm_vtab_sco2_air_cooler[] = {

	/*   VARTYPE   DATATYPE         NAME               LABEL                                      UNITS  META  GROUP REQUIRED_IF CONSTRAINTS         UI_HINTS*/
    { SSC_INPUT,  SSC_MATRIX,   "od_calc_W_dot_fan",  "Columns: T_co2_hot_C, P_co2_hot_MPa, T_co2_cold_C, m_dot_CO2_ND, T_amb_C. Rows: cases", "", "", "", "", "", "" },
    { SSC_INPUT,  SSC_MATRIX,   "od_calc_T_co2_cold", "Columns: T_co2_hot_C, P_co2_hot_MPa, W_dot_fan_ND, m_dot_CO2_ND, T_amb_C. Rows: cases", "", "", "", "", "", "" },

    { SSC_OUTPUT, SSC_ARRAY,    "T_amb_od",          "Off-design ambient temperature",           "C",   "", "",  "", "",  "" },
    { SSC_OUTPUT, SSC_ARRAY,    "T_co2_hot_od",      "Off-design co2 hot inlet temperature",     "C",   "", "",  "", "",  "" },
    { SSC_OUTPUT, SSC_ARRAY,    "P_co2_hot_od",      "Off-design co2 hot inlet pressure",        "MPa", "", "",  "", "",  "" },
    { SSC_OUTPUT, SSC_ARRAY,    "T_co2_cold_od",     "Off-design co2 cold outlet temperature",   "C",   "", "",  "", "",  "" },
    { SSC_OUTPUT, SSC_ARRAY,    "P_co2_cold_od",     "Off-design co2 cold outlet pressure",      "MPa", "", "",  "", "",  "" },
    { SSC_OUTPUT, SSC_ARRAY,    "deltaP_co2_od",     "Off-design co2 cold pressure drop",        "MPa", "", "",  "", "",  "" },
    { SSC_OUTPUT, SSC_ARRAY,    "m_dot_co2_od_ND",   "Off-design co2 mass flow normalized design","-",  "", "",  "", "",  "" },
    { SSC_OUTPUT, SSC_ARRAY,    "W_dot_fan_od",      "Off-design fan power",                     "MWe", "", "",  "", "",  "" },
    { SSC_OUTPUT, SSC_ARRAY,    "W_dot_fan_od_ND",   "Off-design fan power normalized v design", "-",   "", "",  "", "",  "" },
    { SSC_OUTPUT, SSC_ARRAY,    "q_dot_od",          "Off-design heat rejection",                "MWt", "", "",  "", "",  "" },
    { SSC_OUTPUT, SSC_ARRAY,    "q_dot_od_ND",       "Off-design heat rejection normalized design","-", "", "",  "", "",  "" },


	var_info_invalid };

var_info vtab_sco2_air_cooler_design[] = {

    /*   VARTYPE   DATATYPE         NAME               LABEL                                      UNITS  META  GROUP REQUIRED_IF CONSTRAINTS         UI_HINTS*/
    { SSC_INPUT,  SSC_NUMBER,  "T_amb_des",         "Ambient temperature at design",              "C",    "",  "", "*", "", ""},
    { SSC_INPUT,  SSC_NUMBER,  "q_dot_des",		    "Heat rejected from CO2 stream",			  "MWt",  "",  "", "*", "", ""},
    { SSC_INPUT,  SSC_NUMBER,  "T_co2_hot_des",		"Hot temperature of CO2 at inlet to cooler",  "C",	  "",  "", "*", "", ""},
    { SSC_INPUT,  SSC_NUMBER,  "P_co2_hot_des",		"Pressure of CO2 at inlet to cooler",		  "MPa",  "",  "", "*", "", ""},
    { SSC_INPUT,  SSC_NUMBER,  "deltaP_co2_des",	"Pressure drop of CO2 through cooler",		  "MPa",  "",  "", "*", "", ""},
    { SSC_INPUT,  SSC_NUMBER,  "T_co2_cold_des",	"Cold temperature of CO2 at cooler exit",	  "C",	  "",  "", "*", "", ""},
    { SSC_INPUT,  SSC_NUMBER,  "W_dot_fan_des",	    "Air fan power",							  "MWe",  "",  "", "*", "", ""},
    { SSC_INPUT,  SSC_NUMBER,  "site_elevation",	"Site elevation",							  "m",	  "",  "", "*", "", ""},

    { SSC_OUTPUT, SSC_NUMBER,  "d_tube_out",        "CO2 tube outer diameter",                    "cm",   "",  "", "*", "", ""},
    { SSC_OUTPUT, SSC_NUMBER,  "d_tube_in",         "CO2 tube inner diameter",                    "cm",   "",  "", "*", "", ""},
    { SSC_OUTPUT, SSC_NUMBER,  "depth_footprint",   "Dimension of total air cooler in loop/air flow direction",  "m",  "",  "", "*", "", ""},
    { SSC_OUTPUT, SSC_NUMBER,  "width_footprint",   "Dimension of total air cooler of parallel loops",           "m",  "",  "", "*", "", ""},
    { SSC_OUTPUT, SSC_NUMBER,  "parallel_paths",    "Number of parallel flow paths",              "-",    "",  "", "*", "", ""},
    { SSC_OUTPUT, SSC_NUMBER,  "number_of_tubes",   "Number of tubes (one pass)",                 "-",    "",  "", "*", "", ""},
    { SSC_OUTPUT, SSC_NUMBER,  "length",            "Length of tube (one pass)",                  "m",    "",  "", "*", "", ""},
    { SSC_OUTPUT, SSC_NUMBER,  "n_passes_series",   "Number of serial tubes in flow path",        "-",    "",  "", "*", "", ""},
    { SSC_OUTPUT, SSC_NUMBER,  "UA_total",          "Total air-side conductance",                 "kW/K", "",  "", "*", "", ""},
    { SSC_OUTPUT, SSC_NUMBER,  "m_V_hx_material",   "Total hx material volume - no headers",      "m^3",  "",  "", "*", "", ""},


    var_info_invalid };

int sco2_air_cooler_design_common(compute_module *cm, C_CO2_to_air_cooler & c_air_cooler)
{
    C_CO2_to_air_cooler::S_des_par_cycle_dep s_air_cooler_des_par_cycle;
    C_CO2_to_air_cooler::S_des_par_ind s_air_cooler_des_par_ambient;

    s_air_cooler_des_par_ambient.m_T_amb_des = cm->as_double("T_amb_des") + 273.15;		//[K]
    s_air_cooler_des_par_ambient.m_elev = cm->as_double("site_elevation");			//[m]

    s_air_cooler_des_par_cycle.m_Q_dot_des = cm->as_double("q_dot_des");			//[MWt]
    s_air_cooler_des_par_cycle.m_T_hot_in_des = cm->as_double("T_co2_hot_des") + 273.15;		//[K] convert from C
    s_air_cooler_des_par_cycle.m_P_hot_in_des = cm->as_double("P_co2_hot_des")*1.E3;			//[MPa] convert from MPa
    s_air_cooler_des_par_cycle.m_T_hot_out_des = cm->as_double("T_co2_cold_des") + 273.15;	//[K] convert from C
    s_air_cooler_des_par_cycle.m_delta_P_des = cm->as_double("deltaP_co2_des")*1.E3;				//[MPa] convert from MPa
    s_air_cooler_des_par_cycle.m_W_dot_fan_des = cm->as_double("W_dot_fan_des");		//[MWe]

    s_air_cooler_des_par_ambient.m_eta_fan = 0.5;
    s_air_cooler_des_par_ambient.m_N_nodes_pass = 10;

    // For try/catch below
    int out_type = -1;
    std::string out_msg = "";

    try
    {
        c_air_cooler.design_hx(s_air_cooler_des_par_ambient, s_air_cooler_des_par_cycle, 1.E-3);
    }
    catch (C_csp_exception &csp_exception)
    {
        // Report warning before exiting with error
        while (c_air_cooler.mc_messages.get_message(&out_type, &out_msg))
        {
            cm->log(out_msg);
        }

        cm->log(csp_exception.m_error_message, SSC_ERROR, -1.0);

        return 0;
    }

    // If all calls were successful, log to SSC any messages from sco2_recomp_csp
    while (c_air_cooler.mc_messages.get_message(&out_type, &out_msg))
    {
        cm->log(out_msg + "\n");
    }

    // Write outputs
    const C_CO2_to_air_cooler::S_des_solved *p_hx_des_sol;
    p_hx_des_sol = c_air_cooler.get_design_solved();

    cm->assign("d_tube_out", (ssc_number_t)(p_hx_des_sol->m_d_out*1.E2));		//[cm] convert from m
    cm->assign("d_tube_in", (ssc_number_t)(p_hx_des_sol->m_d_in*1.E2));			//[cm] convert from m
    cm->assign("depth_footprint", (ssc_number_t)p_hx_des_sol->m_Depth);		//[m]
    cm->assign("width_footprint", (ssc_number_t)p_hx_des_sol->m_W_par);		//[m]
    cm->assign("parallel_paths", (ssc_number_t)p_hx_des_sol->m_N_par);		//[-]
    cm->assign("number_of_tubes", (ssc_number_t)p_hx_des_sol->m_N_tubes);		//[-]
    cm->assign("length", (ssc_number_t)p_hx_des_sol->m_L_tube);				//[m]
    cm->assign("n_passes_series", (ssc_number_t)p_hx_des_sol->m_N_passes);	//[-]
    cm->assign("UA_total", (ssc_number_t)(p_hx_des_sol->m_UA_total / 1.E3));		//[kW/K]
    cm->assign("m_V_hx_material", (ssc_number_t)p_hx_des_sol->m_V_material_total);	//[m^3]

    return 0;
}

class cm_sco2_air_cooler : public compute_module
{
public:

    // Off-design parameters
    ssc_number_t *p_T_amb_od;
    ssc_number_t *p_T_co2_hot_od;
    ssc_number_t *p_P_co2_hot_od;
    ssc_number_t *p_T_co2_cold_od;
    ssc_number_t *p_P_co2_cold_od;
    ssc_number_t *p_deltaP_co2_od;
    ssc_number_t *p_m_dot_co2_ND;
    ssc_number_t *p_W_dot_fan_od;
    ssc_number_t *p_W_dot_fan_od_ND;
    ssc_number_t *p_q_dot_od;
    ssc_number_t *p_q_dot_od_ND;

	cm_sco2_air_cooler()
	{
        add_var_info(vtab_sco2_air_cooler_design);

        add_var_info(_cm_vtab_sco2_air_cooler);
	}
	
	void exec() override
	{
		C_CO2_to_air_cooler c_air_cooler;

        sco2_air_cooler_design_common(this, c_air_cooler);

        bool is_od_calc_W_dot_fan = is_assigned("od_calc_W_dot_fan");
        bool is_od_calc_T_co2_cold = is_assigned("od_calc_T_co2_cold");

        if (!is_od_calc_W_dot_fan && !is_od_calc_T_co2_cold)
        {
            log("No off-design cases specified");
            return;
        }

        util::matrix_t<double> od_cases;
        if (is_od_calc_W_dot_fan)
        {
            // Columns: T_co2_hot_C, P_co2_hot_MPa, *T_co2_cold_C*, m_dot_CO2_ND, T_amb_C. Rows: cases
            od_cases = as_matrix("od_calc_W_dot_fan");

            int n_od_cols_loc = (int)od_cases.ncols();
            int n_od_runs_loc = (int)od_cases.nrows();

            if (n_od_cols_loc != 5)
            {
                log("Input od_calc_W_dot_fan requires exactly 5 columns");
                return;
            }
        }
        else if (is_od_calc_T_co2_cold)
        {
            // Columns: T_co2_hot_C, P_co2_hot_MPa, *W_dot_fan_ND*, m_dot_CO2_ND, T_amb_C. Rows: cases
            od_cases = as_matrix("od_calc_T_co2_cold");

            int n_od_cols_loc = (int)od_cases.ncols();
            int n_od_runs_loc = (int)od_cases.nrows();

            if (n_od_cols_loc != 5)
            {
                log("Input od_calc_W_dot_fan requires exactly 5 columns");
                return;
            }
        }
        
        // Get design co2 mass flow rate through cooler
        double m_dot_co2_des = c_air_cooler.get_design_solved()->m_m_dot_co2;   //[kg/s]
        double W_dot_fan_des = c_air_cooler.get_design_solved()->m_W_dot_fan;   //[MWe]
        double q_dot_des = c_air_cooler.get_design_solved()->m_q_dot*1.E-6;     //[MWt]

        int n_od_runs = (int)od_cases.nrows();

        allocate_vtab_outputs(n_od_runs);

        for (int n_run = 0; n_run < n_od_runs; n_run++)
        {
            double i_T_co2_hot = od_cases(n_run, 0) + 273.15;   //[K] convert from C
            double i_P_co2_hot = od_cases(n_run, 1)*1.E3;       //[kPa] convert from MPa
            double i_m_dot_co2 = od_cases(n_run, 3)*m_dot_co2_des;  //[kg/s] convert from ND
            double i_T_amb = od_cases(n_run, 4) + 273.15;       //[K] convert from C

            int ac_od_err_code = -1;
            if (is_od_calc_W_dot_fan)
            {
                double i_T_co2_cold = od_cases(n_run, 2) + 273.15;  //[K] convert from C
                double i_W_dot_fan, i_P_co2_cold;
                i_W_dot_fan = i_P_co2_cold = std::numeric_limits<double>::quiet_NaN();
                ac_od_err_code = c_air_cooler.off_design_given_T_out(i_T_amb, i_T_co2_hot, i_P_co2_hot,
                    i_m_dot_co2, i_T_co2_cold, 1.E-4, 1.E-3, i_W_dot_fan, i_P_co2_cold);
            }
            else if (is_od_calc_T_co2_cold)
            {
                double i_W_dot_fan_target = od_cases(n_run, 2)*W_dot_fan_des;        //[MWe]
                double i_T_co2_out, i_P_co2_cold;
                i_T_co2_out = i_P_co2_cold = std::numeric_limits<double>::quiet_NaN();
                ac_od_err_code = c_air_cooler.off_design_given_fan_power(i_T_amb, i_T_co2_hot, i_P_co2_hot,
                    i_m_dot_co2, i_W_dot_fan_target, 1.E-4, 1.E-3, i_T_co2_out, i_P_co2_cold);
            }

            if (ac_od_err_code == 0)
            {
                p_T_amb_od[n_run] = (ssc_number_t)(i_T_amb - 273.15);           //[C] convert from K
                p_T_co2_hot_od[n_run] = (ssc_number_t)(i_T_co2_hot - 273.15);   //[C] convert from K
                p_P_co2_hot_od[n_run] = (ssc_number_t)(i_P_co2_hot*1.E-3);      //[MPa] convert from kPa
                p_T_co2_cold_od[n_run] = (ssc_number_t)(c_air_cooler.get_od_solved().m_T_co2_cold - 273.15); //[C] convert from K
                p_P_co2_cold_od[n_run] = (ssc_number_t)(c_air_cooler.get_od_solved().m_P_co2_cold*1.E-3);    //[MPa] convet from kPa
                p_deltaP_co2_od[n_run] = p_P_co2_hot_od[n_run] - p_P_co2_cold_od[n_run];    //[MPa]
                p_m_dot_co2_ND[n_run] = od_cases(n_run, 3);         //[-]
                p_W_dot_fan_od[n_run] = (ssc_number_t)c_air_cooler.get_od_solved().m_W_dot_fan;    //[MWe]
                p_W_dot_fan_od_ND[n_run] = (ssc_number_t)(c_air_cooler.get_od_solved().m_W_dot_fan / W_dot_fan_des); //[-]
                p_q_dot_od[n_run] = (ssc_number_t)c_air_cooler.get_od_solved().m_q_dot; //[MWt]
                p_q_dot_od_ND[n_run] = (ssc_number_t)(c_air_cooler.get_od_solved().m_q_dot / q_dot_des);
            }
            else
            {
                p_T_amb_od[n_run] = std::numeric_limits<ssc_number_t>::quiet_NaN();
                p_T_co2_hot_od[n_run] = std::numeric_limits<ssc_number_t>::quiet_NaN();
                p_P_co2_hot_od[n_run] = std::numeric_limits<ssc_number_t>::quiet_NaN();
                p_T_co2_cold_od[n_run] = std::numeric_limits<ssc_number_t>::quiet_NaN();
                p_P_co2_cold_od[n_run] = std::numeric_limits<ssc_number_t>::quiet_NaN();
                p_deltaP_co2_od[n_run] = std::numeric_limits<ssc_number_t>::quiet_NaN();
                p_m_dot_co2_ND[n_run] = std::numeric_limits<ssc_number_t>::quiet_NaN();
                p_W_dot_fan_od[n_run] = std::numeric_limits<ssc_number_t>::quiet_NaN();
                p_W_dot_fan_od_ND[n_run] = std::numeric_limits<ssc_number_t>::quiet_NaN();
                p_q_dot_od[n_run] = std::numeric_limits<ssc_number_t>::quiet_NaN();
                p_q_dot_od_ND[n_run] = std::numeric_limits<ssc_number_t>::quiet_NaN();
            }
        }

	}

    void allocate_vtab_outputs(int n_od_runs)
    {
        // Off-design parameters
        p_T_amb_od = allocate("T_amb_od", n_od_runs);
        p_T_co2_hot_od = allocate("T_co2_hot_od", n_od_runs);
        p_P_co2_hot_od = allocate("P_co2_hot_od", n_od_runs);
        p_T_co2_cold_od = allocate("T_co2_cold_od", n_od_runs);
        p_P_co2_cold_od = allocate("P_co2_cold_od", n_od_runs);
        p_deltaP_co2_od = allocate("deltaP_co2_od", n_od_runs);
        p_m_dot_co2_ND = allocate("m_dot_co2_od_ND", n_od_runs);
        p_W_dot_fan_od = allocate("W_dot_fan_od", n_od_runs);
        p_W_dot_fan_od_ND = allocate("W_dot_fan_od_ND", n_od_runs);
        p_q_dot_od = allocate("q_dot_od", n_od_runs);
        p_q_dot_od_ND = allocate("q_dot_od_ND", n_od_runs);

    }
};



DEFINE_MODULE_ENTRY(sco2_air_cooler, "Returns air cooler dimensions given fluid and location design points", 0)
