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
#include "csp_solver_pc_Rankine_indirect_224.h"

static var_info _cm_vtab_ui_udpc_checks[] = {

    /*   VARTYPE   DATATYPE         NAME               LABEL                                            UNITS     META  GROUP REQUIRED_IF CONSTRAINTS         UI_HINTS*/
    { SSC_INPUT,   SSC_MATRIX, "ud_ind_od",        "Off design user-defined power cycle performance as function of T_htf, m_dot_htf [ND], and T_amb",                                         "",             "",                                  "User Defined Power Cycle",                 "?=[[0]]",                                                      "",              ""},

    { SSC_OUTPUT,  SSC_NUMBER, "n_T_htf_pars",     "Number of HTF parametrics",   "-", "", "", "*", "", "" },
    { SSC_OUTPUT,  SSC_NUMBER, "T_htf_low",        "HTF low temperature",         "C", "", "", "*", "", "" },
    { SSC_OUTPUT,  SSC_NUMBER, "T_htf_des",        "HTF design temperature",      "C", "", "", "*", "", "" },
    { SSC_OUTPUT,  SSC_NUMBER, "T_htf_high",       "HTF high temperature",        "C", "", "", "*", "", "" },

    { SSC_OUTPUT,  SSC_NUMBER, "n_T_amb_pars",     "Number of ambient temperature parametrics", "-", "", "", "*", "", "" },
    { SSC_OUTPUT,  SSC_NUMBER, "T_amb_low",        "Low ambient temperature",         "C", "", "", "*", "", "" },
    { SSC_OUTPUT,  SSC_NUMBER, "T_amb_des",        "Design ambient temperature",      "C", "", "", "*", "", "" },
    { SSC_OUTPUT,  SSC_NUMBER, "T_amb_high",       "High ambient temperature",        "C", "", "", "*", "", "" },

    { SSC_OUTPUT,  SSC_NUMBER, "n_m_dot_pars",     "Number of HTF mass flow parametrics", "-", "", "", "*", "", "" },
    { SSC_OUTPUT,  SSC_NUMBER, "m_dot_low",        "Low ambient temperature",         "C", "", "", "*", "", "" },
    { SSC_OUTPUT,  SSC_NUMBER, "m_dot_des",        "Design ambient temperature",      "C", "", "", "*", "", "" },
    { SSC_OUTPUT,  SSC_NUMBER, "m_dot_high",       "High ambient temperature",        "C", "", "", "*", "", "" },


    var_info_invalid };

class cm_ui_udpc_checks : public compute_module
{
public:

    cm_ui_udpc_checks()
    {
        add_var_info(_cm_vtab_ui_udpc_checks);
    }

    void exec() override
    {
        int n_T_htf_pars, n_T_amb_pars, n_m_dot_pars;
        n_T_htf_pars = n_T_amb_pars = n_m_dot_pars = -1;
        double m_dot_low, m_dot_des, m_dot_high, T_htf_low, T_htf_des, T_htf_high, T_amb_low, T_amb_des, T_amb_high;
        m_dot_low = m_dot_des = m_dot_high = T_htf_low = T_htf_des = T_htf_high = T_amb_low = T_amb_des = T_amb_high = std::numeric_limits<double>::quiet_NaN();

        util::matrix_t<double> cmbd_ind = as_matrix("ud_ind_od");
        util::matrix_t<double> T_htf_ind, m_dot_ind, T_amb_ind;

        try
        {
            split_ind_tbl(cmbd_ind, T_htf_ind, m_dot_ind, T_amb_ind,
                n_T_htf_pars, n_T_amb_pars, n_m_dot_pars,
                m_dot_low, m_dot_des, m_dot_high,
                T_htf_low, T_htf_des, T_htf_high,
                T_amb_low, T_amb_des, T_amb_high);
        }
        catch (C_csp_exception &csp_exception)
        {
            n_T_htf_pars = n_T_amb_pars = n_m_dot_pars = -1;
            m_dot_low = m_dot_des = m_dot_high = T_htf_low = T_htf_des = T_htf_high = T_amb_low = T_amb_des = T_amb_high = std::numeric_limits<double>::quiet_NaN();
        }

        assign("n_T_htf_pars", (ssc_number_t)n_T_htf_pars);
        assign("T_htf_low", (ssc_number_t)T_htf_low);
        assign("T_htf_des", (ssc_number_t)T_htf_des);
        assign("T_htf_high", (ssc_number_t)T_htf_high);

        assign("n_T_amb_pars", (ssc_number_t)n_T_amb_pars);
        assign("T_amb_low", (ssc_number_t)T_amb_low);
        assign("T_amb_des", (ssc_number_t)T_amb_des);
        assign("T_amb_high", (ssc_number_t)T_amb_high);

        assign("n_m_dot_pars", (ssc_number_t)n_m_dot_pars);
        assign("m_dot_low", (ssc_number_t)m_dot_low);
        assign("m_dot_des", (ssc_number_t)m_dot_des);
        assign("m_dot_high", (ssc_number_t)m_dot_high);

        return;
    }
};

DEFINE_MODULE_ENTRY(ui_udpc_checks, "Calculates the levels and number of paramteric runs for 3 udpc ind variables", 0)