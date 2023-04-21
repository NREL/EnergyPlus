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

#ifndef __HEAT_EXCHANGERS_
#define __HEAT_EXCHANGERS_

#include "CO2_properties.h"
#include "water_properties.h"
#include "htf_props.h"
#include "lib_util.h"
#include "numeric_solvers.h"

#include "csp_solver_util.h"

namespace NS_HX_counterflow_eqs
{
    enum
    {
        CO2 = 200,
        WATER = 201
    };

    enum
    {
        OPTIMIZE_UA = 0,
        TARGET_UA,
        TARGET_MIN_DT,
        TARGET_EFFECTIVENESS
    };

    enum E_UA_target_type
    {
        E_constant_UA,
        E_calc_UA
    };

    struct S_hx_fluid_props
    {
        double k;   //[W/m-K] Thermal conductivity
        double rho; //[kg/m3] Density
        double mu;  //[uPa-s] Dynamic viscosity (units from co2 props)
        double cp;  //[kJ/kg-K] Specific heat
        double m_dot;   //[kg/s] Mass flow rate

        S_hx_fluid_props()
        {
            k = rho = mu = cp = m_dot = std::numeric_limits<double>::quiet_NaN();
        }
    };

    struct S_hx_node_info
    {
        S_hx_fluid_props s_fl_hot;
        S_hx_fluid_props s_fl_cold;
        double UA;      //[kW/K]
    };

    double hx_fl__calc_h__TP(int fl_code, HTFProperties& fl_htf_class,
        double T_K /*K*/, double P_kpa /*kPa*/);

    class C_hx_fl__TP__core
    {
    public:

        double m_h;     //[kJ/kg]
        double m_rho;   //[kg/m3]
        double m_cp;    //[kJ/kg-K]

        double m_k;     //[W/m-K]
        double m_mu;    //[uPa-s]

        C_hx_fl__TP__core(int fl_code, HTFProperties* fl_htf_class,
            double T_K /*K*/, double P_kpa /*kPa*/, bool is_calc_cond_visc);
    };

    double hx_fl__calc_T__Ph(int fl_code, HTFProperties& fl_htf_class,
        double P_kpa /*kPa*/, double h_kjkg /*kJ/kg*/);

    class C_hx_fl__Ph__core
    {
    public:

        double m_T;     //[K]
        double m_rho;   //[kg/m3]
        double m_cp;    //[kJ/kg-K]

        double m_k;     //[W/m-K]
        double m_mu;    //[uPa-s]

        C_hx_fl__Ph__core(int fl_code, HTFProperties* fl_htf_class,
            double P_kpa /*kPa*/, double h_kjkg /*kJ/kg*/, bool is_calc_cond_visc);
    };

    double calc_max_q_dot_enth(int hot_fl_code /*-*/, HTFProperties& hot_htf_class,
        int cold_fl_code /*-*/, HTFProperties& cold_htf_class,
        double h_h_in /*kJ/kg*/, double P_h_in /*kPa*/, double P_h_out /*kPa*/, double m_dot_h /*kg/s*/,
        double h_c_in /*kJ/kg*/, double P_c_in /*kPa*/, double P_c_out /*kPa*/, double m_dot_c /*kg/s*/);

    double calc_max_q_dot_enth(int hot_fl_code /*-*/, HTFProperties& hot_htf_class,
        int cold_fl_code /*-*/, HTFProperties& cold_htf_class,
        double h_h_in /*kJ/kg*/, double P_h_in /*kPa*/, double P_h_out /*kPa*/, double m_dot_h /*kg/s*/,
        double h_c_in /*kJ/kg*/, double P_c_in /*kPa*/, double P_c_out /*kPa*/, double m_dot_c /*kg/s*/,
        double& h_h_out /*kJ/kg*/, double& T_h_out /*K*/,
        double& h_c_out /*kJ/kg*/, double& T_c_out /*K*/,
        double& T_h_in /*K*/, double& T_c_in /*K*/);

    double calc_max_q_dot(int hot_fl_code /*-*/, HTFProperties& hot_htf_class,
        int cold_fl_code /*-*/, HTFProperties& cold_htf_class,
        double T_h_in, double P_h_in, double P_h_out, double m_dot_h,
        double T_c_in, double P_c_in, double P_c_out, double m_dot_c,
        double& h_h_out /*kJ/kg*/, double& T_h_out /*K*/,
        double& h_c_out /*kJ/kg*/, double& T_c_out /*K*/);

    void calc_req_UA(int hot_fl_code /*-*/, HTFProperties& hot_htf_class,
        int cold_fl_code /*-*/, HTFProperties& cold_htf_class,
        int N_sub_hx /*-*/,
        double q_dot /*kWt*/, double m_dot_c /*kg/s*/, double m_dot_h /*kg/s*/,
        double T_c_in /*K*/, double T_h_in /*K*/, double P_c_in /*kPa*/, double P_c_out /*kPa*/, double P_h_in /*kPa*/, double P_h_out /*kPa*/,
        double& UA /*kW/K*/, double& min_DT /*C*/, double& eff /*-*/, double& NTU /*-*/, double& T_h_out /*K*/, double& T_c_out /*K*/, double& q_dot_calc /*kWt*/,
        std::vector<S_hx_node_info>& v_s_node_info);

    void calc_req_UA_enth(int hot_fl_code /*-*/, HTFProperties& hot_htf_class,
        int cold_fl_code /*-*/, HTFProperties& cold_htf_class,
        int N_sub_hx /*-*/,
        double q_dot /*kWt*/, double m_dot_c /*kg/s*/, double m_dot_h /*kg/s*/,
        double h_c_in /*kJ/kg*/, double h_h_in /*kJ/kg*/, double P_c_in /*kPa*/, double P_c_out /*kPa*/, double P_h_in /*kPa*/, double P_h_out /*kPa*/,
        double& h_h_out /*kJ/kg*/, double& T_h_out /*K*/, double& h_c_out /*kJ/kg*/, double& T_c_out /*K*/,
        double& UA /*kW/K*/, double& min_DT /*C*/, double& eff /*-*/, double& NTU /*-*/, double& q_dot_calc /*kWt*/,
        std::vector<S_hx_node_info>& v_s_node_info);

    void solve_q_dot_for_fixed_UA(int hx_target_code /*-*/,
        int hot_fl_code /*-*/, HTFProperties& hot_htf_class,
        int cold_fl_code /*-*/, HTFProperties& cold_htf_class,
        S_hx_node_info& s_node_info_des,
        int N_sub_hx /*-*/, NS_HX_counterflow_eqs::E_UA_target_type UA_target_type,
        double T_c_in /*K*/, double P_c_in /*kPa*/, double m_dot_c /*kg/s*/, double P_c_out /*kPa*/,
        double T_h_in /*K*/, double P_h_in /*kPa*/, double m_dot_h /*kg/s*/, double P_h_out /*kPa*/,
        double UA_target /*kW/K*/, double min_dT_target /*K*/, double eff_target /*-*/,
        double eff_limit /*-*/, double eff_guess /*-*/,
        double tol /*-*/,
        double& q_dot /*kWt*/, double& T_c_out /*K*/, double& T_h_out /*K*/,
        double& h_c_in /*kJ/kg*/, double& h_c_out /*kJ/kg*/,
        double& h_h_in /*kJ/kg*/, double& h_h_out /*kJ/kg*/,
        double& eff_calc /*-*/, double& min_DT /*K*/, double& NTU /*-*/, double& UA_calc,
        std::vector<S_hx_node_info>& v_s_node_info);

    void solve_q_dot_for_fixed_UA_enth(int hot_fl_code /*-*/, HTFProperties& hot_htf_class,
        int cold_fl_code /*-*/, HTFProperties& cold_htf_class,
        S_hx_node_info& s_node_info_des,
        int N_sub_hx /*-*/, NS_HX_counterflow_eqs::E_UA_target_type UA_target_type,
        double h_c_in /*K*/, double P_c_in /*kPa*/, double m_dot_c /*kg/s*/, double P_c_out /*kPa*/,
        double h_h_in /*K*/, double P_h_in /*kPa*/, double m_dot_h /*kg/s*/, double P_h_out /*kPa*/,
        double UA_target /*kW/K*/, double eff_limit /*-*/, double eff_guess /*-*/,
        double tol /*-*/,
        double& T_c_out  /*K*/, double& h_c_out /*kJ/kg*/,
        double& T_h_out /*K*/, double& h_h_out /*kJ/kg*/,
        double& q_dot /*kWt*/, double& eff_calc /*-*/, double& min_DT /*K*/, double& NTU /*-*/, double& UA_calc,
        std::vector<S_hx_node_info>& v_s_node_info);

    void solve_q_dot__fixed_min_dT__enth(int hot_fl_code /*-*/, HTFProperties& hot_htf_class,
        int cold_fl_code /*-*/, HTFProperties& cold_htf_class,
        int N_sub_hx /*-*/,
        double h_c_in /*K*/, double P_c_in /*kPa*/, double m_dot_c /*kg/s*/, double P_c_out /*kPa*/,
        double h_h_in /*K*/, double P_h_in /*kPa*/, double m_dot_h /*kg/s*/, double P_h_out /*kPa*/,
        double min_dT_target /*C*/, double eff_limit /*-*/,
        double& T_c_out  /*K*/, double& h_c_out /*kJ/kg*/,
        double& T_h_out /*K*/, double& h_h_out /*kJ/kg*/,
        double& q_dot /*kWt*/, double& eff_calc /*-*/, double& min_DT /*K*/, double& NTU /*-*/, double& UA_calc,
        std::vector<S_hx_node_info>& v_s_node_info);

    void solve_q_dot__fixed_eff__enth(int hot_fl_code /*-*/, HTFProperties& hot_htf_class,
        int cold_fl_code /*-*/, HTFProperties& cold_htf_class,
        int N_sub_hx /*-*/,
        double h_c_in /*K*/, double P_c_in /*kPa*/, double m_dot_c /*kg/s*/, double P_c_out /*kPa*/,
        double h_h_in /*K*/, double P_h_in /*kPa*/, double m_dot_h /*kg/s*/, double P_h_out /*kPa*/,
        double eff_target /*-*/,
        double& T_c_out  /*K*/, double& h_c_out /*kJ/kg*/,
        double& T_h_out /*K*/, double& h_h_out /*kJ/kg*/,
        double& q_dot /*kWt*/, double& eff_calc /*-*/, double& min_DT /*K*/, double& NTU /*-*/, double& UA_calc,
        std::vector<S_hx_node_info>& v_s_node_info);

    double UA_scale_vs_m_dot(double m_dot_cold_over_des /*-*/, double m_dot_hot_over_des /*-*/);

    double UA_CRM(int hot_fl_code /*-*/, HTFProperties* hot_htf_class,
        int cold_fl_code /*-*/, HTFProperties* cold_htf_class,
        const S_hx_node_info& s_node_info_des,
        double P_hot_in /*kPa*/, double P_hot_out /*kPa*/,
        double h_hot_in /*kPa*/, double h_hot_out /*kPa*/,
        double m_dot_hot /*kg/s*/,
        double P_cold_in /*kPa*/, double P_cold_out /*kPa*/,
        double h_cold_in /*kPa*/, double h_cold_out /*kPa*/,
        double m_dot_cold /*kg/s*/);

    class C_MEQ__q_dot__target_UA__c_in_h_out__enth : public C_monotonic_equation
    {
    private:

        int m_hot_fl_code;		//[-]
        HTFProperties mc_hot_htf_class;

        int m_cold_fl_code;		//[-]
        HTFProperties mc_cold_htf_class;

        S_hx_node_info ms_node_info_des;

        int m_N_sub_hx;			//[-]

        // Design info
        //double m_UA_design;     //[kW/K]
        //double m_m_dot_hot_des;     //[kg/s]
        //double m_m_dot_cold_des;    //[kg/s]

        double m_P_cold_out;	//[kPa]
        double m_P_hot_out;		//[kPa]

        double m_h_cold_in;     //[kJ/kg]
        double m_P_cold_in;     //[kPa]
        double m_m_dot_cold;    //[kg/s]

        double m_h_hot_out;     //[kJ/kg]
        double m_P_hot_in;      //[kPa]
        double m_m_dot_hot;     //[kg/s]

    public:
        C_MEQ__q_dot__target_UA__c_in_h_out__enth(int hot_fl_code /*-*/, HTFProperties hot_htf_class,
            int cold_fl_code /*-*/, HTFProperties cold_htf_class,
            const S_hx_node_info s_node_info_des,
            double P_cold_out /*kPa*/, double P_hot_out /*kPa*/,
            double h_cold_in /*kJ/kg*/, double P_cold_in /*kPa*/, double m_dot_cold /*kg/s*/,
            double h_hot_out /*kJ/kg*/, double P_hot_in /*kPa*/, double m_dot_hot /*kg/s*/)
        {
            m_hot_fl_code = hot_fl_code;
            mc_hot_htf_class = hot_htf_class;

            m_cold_fl_code = cold_fl_code;
            mc_cold_htf_class = cold_htf_class;

            m_N_sub_hx = 1;
            ms_node_info_des = s_node_info_des;

            //m_UA_design = UA_design;    //[kW/K]
            //m_m_dot_hot_des = m_dot_hot_des;    //[kg/s]
            //m_m_dot_cold_des = m_dot_cold_des;  //[kg/s]

            m_P_cold_out = P_cold_out;	//[kPa]
            m_P_hot_out = P_hot_out;	//[kPa]

            m_h_cold_in = h_cold_in;		//[kJ/kg]
            m_P_cold_in = P_cold_in;		//[kPa]
            m_m_dot_cold = m_dot_cold;	//[kg/s]

            m_h_hot_out = h_hot_out;		//[kJ/kg]
            m_P_hot_in = P_hot_in;		//[kPa]
            m_m_dot_hot = m_dot_hot;	//[kg/s]

            m_h_cold_out = m_h_hot_in = m_T_cold_out = m_T_hot_in = m_eff =
                m_min_DT = m_NTU = m_UA_calc = std::numeric_limits<double>::quiet_NaN();
        }

        double m_h_cold_out;	//[kJ/kg]
        double m_h_hot_in;		//[kJ/kg]
        double m_T_cold_out;	//[K]
        double m_T_hot_in;		//[K]
        double m_eff;			//[-]
        double m_min_DT;		//[K]
        double m_NTU;			//[-]
        double m_UA_calc;		//[kW/K]

        std::vector<S_hx_node_info> mv_s_node_info_calc;

        virtual int operator()(double q_dot /*kWt*/, double* diff_UA_calc /*-*/);
    };


    class C_MEQ__q_dot__UA_target__enth : public C_monotonic_equation
    {

    private:

        int m_hot_fl_code;		//[-]
        HTFProperties mc_hot_htf_class;

        int m_cold_fl_code;		//[-]
        HTFProperties mc_cold_htf_class;

        S_hx_node_info* mps_node_info_des;

        int m_N_sub_hx;			//[-]

        E_UA_target_type m_UA_target_type;
        double m_UA_target;     //[kW/K]

        double m_P_c_out;		//[kPa]
        double m_P_h_out;		//[kPa]

        double m_h_c_in;		//[kJ/kg]
        double m_P_c_in;		//[kPa]
        double m_m_dot_c;		//[kg/s]
        double m_h_h_in;		//[kJ/kg]
        double m_P_h_in;		//[kPa]
        double m_m_dot_h;		//[kg/s]

    public:
        C_MEQ__q_dot__UA_target__enth(int hot_fl_code /*-*/, HTFProperties hot_htf_class,
            int cold_fl_code /*-*/, HTFProperties cold_htf_class,
            S_hx_node_info* ps_node_info_des,
            int N_sub_hx /*-*/,
            E_UA_target_type UA_target_type, double UA_target /*kW/K*/,
            double P_c_out /*kPa*/, double P_h_out /*kPa*/,
            double h_c_in /*kJ/kg*/, double P_c_in /*kPa*/, double m_dot_c /*kg/s*/,
            double h_h_in /*kJ/kg*/, double P_h_in /*kPa*/, double m_dot_h /*kg/s*/)
        {
            m_hot_fl_code = hot_fl_code;
            mc_hot_htf_class = hot_htf_class;

            m_cold_fl_code = cold_fl_code;
            mc_cold_htf_class = cold_htf_class;

            mps_node_info_des = ps_node_info_des;

            m_N_sub_hx = N_sub_hx;

            m_UA_target_type = UA_target_type;
            m_UA_target = UA_target;

            m_P_c_out = P_c_out;	//[kPa]
            m_P_h_out = P_h_out;	//[kPa]

            m_h_c_in = h_c_in;		//[kJ/kg]
            m_P_c_in = P_c_in;		//[kPa]
            m_m_dot_c = m_dot_c;	//[kg/s]

            m_h_h_in = h_h_in;		//[kJ/kg]
            m_P_h_in = P_h_in;		//[kPa]
            m_m_dot_h = m_dot_h;	//[kg/s]

            m_h_c_out = m_h_h_out = m_T_c_out = m_T_h_out = m_eff =
                m_min_DT = m_NTU = m_UA_calc = std::numeric_limits<double>::quiet_NaN();
        }

        double m_h_c_out;		//[kJ/kg]
        double m_h_h_out;		//[kJ/kg]
        double m_T_c_out;		//[K]
        double m_T_h_out;		//[K]
        double m_eff;			//[-]
        double m_min_DT;		//[K]
        double m_NTU;			//[-]
        double m_UA_calc;		//[kW/K]

        std::vector<S_hx_node_info> mv_s_node_info;

        virtual int operator()(double q_dot /*kWt*/, double* diff_UA /*-*/);
    };

    class C_MEQ__min_dT__q_dot : public C_monotonic_equation
    {
    private:

        int m_hot_fl_code;		//[-]
        HTFProperties mc_hot_htf_class;

        int m_cold_fl_code;		//[-]
        HTFProperties mc_cold_htf_class;

        int m_N_sub_hx;			//[-]

        double m_P_c_out;		//[kPa]
        double m_P_h_out;		//[kPa]

        double m_h_c_in;		//[K]
        double m_P_c_in;		//[kPa]
        double m_m_dot_c;		//[kg/s]
        double m_h_h_in;		//[K]
        double m_P_h_in;		//[kPa]
        double m_m_dot_h;		//[kg/s]

    public:
        C_MEQ__min_dT__q_dot(int hot_fl_code /*-*/, HTFProperties hot_htf_class,
            int cold_fl_code /*-*/, HTFProperties cold_htf_class,
            int N_sub_hx /*-*/,
            double P_c_out /*kPa*/, double P_h_out /*kPa*/,
            double h_c_in /*kJ/kg*/, double P_c_in /*kPa*/, double m_dot_c /*kg/s*/,
            double h_h_in /*kJ/kg*/, double P_h_in /*kPa*/, double m_dot_h /*kg/s*/)
        {
            m_hot_fl_code = hot_fl_code;
            mc_hot_htf_class = hot_htf_class;

            m_cold_fl_code = cold_fl_code;
            mc_cold_htf_class = cold_htf_class;

            m_N_sub_hx = N_sub_hx;

            m_P_c_out = P_c_out;	//[kPa]
            m_P_h_out = P_h_out;	//[kPa]

            m_h_c_in = h_c_in;		//[kJ/kg]
            m_P_c_in = P_c_in;		//[kPa]
            m_m_dot_c = m_dot_c;	//[kg/s]

            m_h_h_in = h_h_in;		//[kJ/kg]
            m_P_h_in = P_h_in;		//[kPa]
            m_m_dot_h = m_dot_h;	//[kg/s]

            m_h_c_out = m_h_h_out = m_T_c_out = m_T_h_out = m_eff =
                m_min_DT = m_NTU = m_UA_calc = std::numeric_limits<double>::quiet_NaN();
        }

        double m_h_c_out;		//[kJ/kg]
        double m_h_h_out;		//[kJ/kg]
        double m_T_c_out;		//[K]
        double m_T_h_out;		//[K]
        double m_eff;			//[-]
        double m_min_DT;		//[K]
        double m_NTU;			//[-]
        double m_UA_calc;		//[kW/K]

        std::vector<S_hx_node_info> mv_s_node_info;

        virtual int operator()(double q_dot /*kWt*/, double* min_dT /*C*/);
    };
}

class C_HX_counterflow_CRM
{
public:
    
    class C_od_thermal_solution_type
    {
    public:

        enum 
        {
            E_CRM_UA_PER_NODE,
            E_DEFAULT
        };
    };
    
    

protected:
    bool m_is_HX_initialized;		//[-] True = yes!
    bool m_is_HX_designed;			//[-] True = yes!
    NS_HX_counterflow_eqs::E_UA_target_type m_od_UA_target_type;

    std::vector<NS_HX_counterflow_eqs::S_hx_node_info> mv_s_node_info_des;
    std::vector<NS_HX_counterflow_eqs::S_hx_node_info> mv_s_node_info_od;

public:

    int m_cost_model;		//[-]
    int m_od_solution_type; //[-]

    bool m_is_single_node_des_set;
    NS_HX_counterflow_eqs::S_hx_node_info ms_node_info_des;

    enum
    {
        // Techno-Economic Comparison of Solar-Driven SCO2 Brayton Cycles Using 
        // Component Cost Models Baselined with Vendor Data and Estimates
        // ASME ES 2017		
        E_CARLSON_17_RECUP,		// CO2 - CO2 PCHE
        E_CARLSON_17_PHX		// Salt - CO2 PCHE high temperature
    };

    struct S_init_par
    {
        int m_N_sub_hx;				//[-] Number of sub-heat exchangers used in the model

        int m_hot_fl;				//[-] Integer code for hot fluid - assumed be HTF in library or w/ lookup unless = CO2 enumeration
        util::matrix_t<double> mc_hot_fl_props;		//[-] If applicable, user-defined properties

        int m_cold_fl;				//[-] Integer code for cold fluid - assumed be HTF in library or w/ lookup unless = CO2 enumeration
        util::matrix_t<double> mc_cold_fl_props;	//[-] If applicable, user-defined properties

        S_init_par()
        {
            m_N_sub_hx = m_hot_fl = m_cold_fl = -1;
        }
    };

    struct S_des_calc_UA_par
    {
        double m_T_h_in;			//[K] Design-point hot inlet temperature
        double m_P_h_in;			//[kPa] Hot fluid inlet pressure
        double m_P_h_out;			//[kPa] Hot fluid outlet pressure
        double m_m_dot_hot_des;		//[kg/s] hot fluid design mass flow rate
        double m_T_c_in;			//[K] Design-point cold inlet temperature
        double m_P_c_in;			//[kPa] Cold fluid inlet temperature
        double m_P_c_out;			//[kPa] Cold fluid outlet temperature
        double m_m_dot_cold_des;	//[kg/s] cold fluid design mass flow rate

        double m_eff_max;			//[-] Max allowable effectiveness

        S_des_calc_UA_par()
        {
            m_T_h_in = m_P_h_in = m_P_h_out = m_m_dot_hot_des =
                m_T_c_in = m_P_c_in = m_P_c_out = m_m_dot_cold_des =

                m_eff_max = std::numeric_limits<double>::quiet_NaN();
        }
    };

    struct S_des_solved
    {
        double m_UA_allocated;		    //[kW/K] Allocated design-point conductance
        double m_UA_calc_at_eff_max;	//[kW/K] May be less than design total if eff_max < 1


        double m_Q_dot_design;		//[kWt] Design-point heat transfer
        double m_UA_design;         //[kW/K] Design UA. used in cost model; off-design model scales 'm_UA_design_total'
        double m_min_DT_design;		//[K] Minimum temperature difference in heat exchanger
        double m_eff_design;		//[-] Effectiveness at design
        double m_NTU_design;		//[-] NTU at design
        double m_T_h_out;			//[K] Design-point hot outlet temperature
        double m_T_c_out;			//[K] Design-point cold outlet temperature
        double m_DP_cold_des;		//[kPa] cold fluid design pressure drop
        double m_DP_hot_des;		//[kPa] hot fluid design pressure drop

        double m_cost;				//[M$]

        S_des_solved()
        {
            m_UA_allocated = m_UA_calc_at_eff_max =

                m_Q_dot_design = m_UA_design = m_UA_calc_at_eff_max =
                m_min_DT_design = m_eff_design = m_NTU_design =
                m_T_h_out = m_T_c_out =
                m_DP_cold_des = m_DP_hot_des =
                m_cost = std::numeric_limits<double>::quiet_NaN();
        }
    };

    struct S_od_par
    {
        double m_T_c_in;		//[K] Cold fluid inlet temperature
        double m_P_c_in;		//[kPa] Cold fluid inlet pressure
        double m_m_dot_c;		//[kg/s] Cold fluid design mass flow rate
        double m_T_h_in;		//[K] Hot fluid inlet temperature
        double m_P_h_in;		//[kPa] Hot fluid inlet pressure
        double m_m_dot_h;		//[kg/s] Hot fluid design mass flow rate

        S_od_par()
        {
            m_T_c_in = m_P_c_in = m_m_dot_c =
                m_T_h_in = m_P_h_in = m_m_dot_h = std::numeric_limits<double>::quiet_NaN();
        }
    };

    struct S_od_solved
    {
        double m_q_dot;		//[kWt] Thermal power to cold fluid
        double m_T_c_out;	//[K] Cold fluid outlet temperature
        double m_P_c_out;	//[kPa] Cold fluid outlet pressure
        double m_T_h_out;	//[K] Hot fluid outlet temperature
        double m_P_h_out;	//[kPa] Hot fluid outlet temperature
        double m_UA_total;	//[kW/K] Conductance
        double m_min_DT;	//[K] Min temp difference
        double m_eff;		//[-]
        double m_NTU;

        double m_deltaP_cold;   //[kPa]
        double m_deltaP_hot;    //[kPa]

        S_od_solved()
        {
            m_q_dot =
                m_T_c_out = m_P_c_out = m_T_h_out = m_P_h_out =
                m_UA_total = m_min_DT = m_eff = m_NTU =
                m_deltaP_cold = m_deltaP_hot = std::numeric_limits<double>::quiet_NaN();
        }
    };

    S_init_par ms_init_par;
    S_des_calc_UA_par ms_des_calc_UA_par;
    S_des_solved ms_des_solved;
    S_od_par ms_od_par;
    S_od_solved ms_od_solved;

    HTFProperties mc_hot_fl;
    HTFProperties mc_cold_fl;
    CO2_state mc_co2_props;
    water_state ms_water_props;

    C_HX_counterflow_CRM();

    void design_calc_UA(C_HX_counterflow_CRM::S_des_calc_UA_par des_par,
        double q_dot_design /*kWt*/, C_HX_counterflow_CRM::S_des_solved &des_solved);

    double calc_max_q_dot_enth(double h_h_in /*kJ/kg*/, double P_h_in /*kPa*/, double P_h_out /*kPa*/, double m_dot_h /*kg/s*/,
        double h_c_in /*kJ/kg*/, double P_c_in /*kPa*/, double P_c_out /*kPa*/, double m_dot_c /*kg/s*/);

    void calc_req_UA(double q_dot /*kWt*/, double m_dot_c /*kg/s*/, double m_dot_h /*kg/s*/,
        double T_c_in /*K*/, double T_h_in /*K*/, double P_c_in /*kPa*/, double P_c_out /*kPa*/, double P_h_in /*kPa*/, double P_h_out /*kPa*/,
        double & UA /*kW/K*/, double & min_DT /*C*/, double & eff /*-*/, double & NTU /*-*/, double & T_h_out /*K*/, double & T_c_out /*K*/, double & q_dot_calc /*kWt*/,
        std::vector<NS_HX_counterflow_eqs::S_hx_node_info> & v_s_node_info);

    void calc_req_UA_enth(double q_dot /*kWt*/, double m_dot_c /*kg/s*/, double m_dot_h /*kg/s*/,
        double h_c_in /*kJ/kg*/, double h_h_in /*kJ/kg*/, double P_c_in /*kPa*/, double P_c_out /*kPa*/, double P_h_in /*kPa*/, double P_h_out /*kPa*/,
        double & UA /*kW/K*/, double & min_DT /*C*/, double & eff /*-*/, double & NTU /*-*/, double & h_h_out /*K*/, double & h_c_out /*K*/, double & q_dot_calc /*kWt*/,
        std::vector<NS_HX_counterflow_eqs::S_hx_node_info> & v_s_node_info);

    void design_for_target__calc_outlet(int hx_target_code /*-*/,
        double UA_target /*kW/K*/, double min_dT_target /*K*/, double eff_target /*-*/,
        double eff_max /*-*/,
        double T_c_in /*K*/, double P_c_in /*kPa*/, double m_dot_c /*kg/s*/, double P_c_out /*kPa*/,
        double T_h_in /*K*/, double P_h_in /*kPa*/, double m_dot_h /*kg/s*/, double P_h_out /*kPa*/,
        double tol /*-*/,
        double & q_dot /*kWt*/, double & T_c_out /*K*/, double & T_h_out /*K*/);

    void off_design_solution_fixed_dP(double T_c_in /*K*/, double P_c_in /*kPa*/, double m_dot_c /*kg/s*/, double P_c_out /*kPa*/,
        double T_h_in /*K*/, double P_h_in /*kPa*/, double m_dot_h /*kg/s*/, double P_h_out /*kPa*/,
        double od_tol /*-*/,
        double & q_dot /*kWt*/, double & T_c_out /*K*/, double & T_h_out /*K*/);

    void off_design_solution_calc_dP(double T_c_in /*K*/, double P_c_in /*kPa*/, double m_dot_c /*kg/s*/,
        double T_h_in /*K*/, double P_h_in /*kPa*/, double m_dot_h /*kg/s*/,
        double od_tol /*-*/,
        double & q_dot /*kWt*/, double & T_c_out /*K*/, double & P_c_out /*kPa*/, double & T_h_out /*K*/, double & P_h_out /*kPa*/);

    double od_delta_p_cold_frac(double m_dot_c /*kg/s*/);

    double od_delta_p_cold(double m_dot_c /*kg/s*/);

    double od_delta_p_hot_frac(double m_dot_h /*kg/s*/);

    double od_delta_p_hot(double m_dot_h /*kg/s*/);

    double od_UA_frac(double m_dot_c /*kg/s*/, double m_dot_h /*kg/s*/);

    double od_UA(double m_dot_c /*kg/s*/, double m_dot_h /*kg/s*/);

    double calculate_cost(double UA /*kWt/K*/,
        double T_hot_in /*K*/, double P_hot_in /*kPa*/, double m_dot_hot /*kg/s*/,
        double T_cold_in /*K*/, double P_cold_in /*kPa*/, double m_dot_cold /*kg/s*/);

    virtual void initialize(const S_init_par & init_par);

    class C_MEQ__hx_total_q_dot : public C_monotonic_equation
    {
    private:
        C_HX_counterflow_CRM *mpc_hx;
        double m_m_dot_cold;    //[kg/s]
        double m_m_dot_hot;     //[kg/s]
        double m_h_cold_in;     //[kJ/kg]
        double m_h_hot_in;      //[kJ/kg]
        double m_P_cold_in;     //[kPa]
        double m_P_cold_out;    //[kPa]
        double m_P_hot_in;      //[kPa]
        double m_P_hot_out;     //[kPa]

        int N_sub_hx_i;     //[-]
        double m_tol;       //[-]

    public:

        C_MEQ__hx_total_q_dot(C_HX_counterflow_CRM *pc_hx,
            double m_dot_cold /*kg/s*/, double m_dot_hot /*kg/s*/,
            double h_cold_in /*kJ/kg*/, double h_hot_in /*kJ/kg*/,
            double P_cold_in /*kPa*/, double P_cold_out /*kPa*/, 
            double P_hot_in /*kPa*/, double P_hot_out /*kPa*/,
            double tol /*-*/)
        {
            mpc_hx = pc_hx;

            m_m_dot_cold = m_dot_cold;
            m_m_dot_hot = m_dot_hot;
            m_h_cold_in = h_cold_in;
            m_h_hot_in = h_hot_in;
            m_P_cold_in = P_cold_in;
            m_P_cold_out = P_cold_out;
            m_P_hot_in = P_hot_in;
            m_P_hot_out = P_hot_out;

            N_sub_hx_i = 1;

            m_tol = tol;

            init_calc_member_vars();
        }

        double m_h_hot_out;     //[kJ/kg]
        double m_h_cold_out;    //[kJ/kg]
        double m_UA_tot_calc;   //[kW/K]
        double m_min_dT;        //[K]

        void init_calc_member_vars();

        virtual int operator()(double q_dot_hx /*kWt*/, double *diff_q_dot /*-*/);
    };

};



class C_HX_co2_to_htf : public C_HX_counterflow_CRM
{
private:



public:

	C_HX_co2_to_htf()
	{
		m_cost_model = C_HX_counterflow_CRM::E_CARLSON_17_PHX;
        m_od_solution_type = C_HX_counterflow_CRM::C_od_thermal_solution_type::E_CRM_UA_PER_NODE;
        
        m_od_solution_type = C_HX_counterflow_CRM::C_od_thermal_solution_type::E_DEFAULT;
        
        
	}

	//// This method calculates the HTF mass flow rate (m_m_dot_hot_des) that results in CR = 1
	//void design_with_m_dot(C_HX_counterflow::S_des_par &des_par, double T_htf_cold, C_HX_counterflow::S_des_solved &des_solved);

	// This method calculates the required HTF mass flow rate (m_m_dot_hot_des) given a cold side approach temperature (assuming hot HTF temp is a design parameter)
	void design_and_calc_m_dot_htf(C_HX_counterflow_CRM::S_des_calc_UA_par &des_par, 
		double q_dot_design /*kWt*/, double dt_cold_approach /*C/K*/, C_HX_counterflow_CRM::S_des_solved &des_solved);

	virtual void initialize(int hot_fl, util::matrix_t<double> hot_fl_props, int N_sub_hx, NS_HX_counterflow_eqs::E_UA_target_type od_UA_target_type);

	virtual void initialize(int hot_fl, int N_sub_hx, NS_HX_counterflow_eqs::E_UA_target_type od_UA_target_type);
	
};

class C_HX_co2_to_co2_CRM : public C_HX_counterflow_CRM
{

public:

    C_HX_co2_to_co2_CRM()
    {
        m_cost_model = C_HX_counterflow_CRM::E_CARLSON_17_RECUP;
        m_od_solution_type = C_HX_counterflow_CRM::C_od_thermal_solution_type::E_CRM_UA_PER_NODE;

        m_od_solution_type = C_HX_counterflow_CRM::C_od_thermal_solution_type::E_DEFAULT;

    }

    virtual void initialize(int N_sub_hx, NS_HX_counterflow_eqs::E_UA_target_type od_UA_target_type);
};

namespace N_compact_hx
{
	enum
	{
		fc_tubes_s80_38T = 1,
		fc_tubes_sCF_88_10Jb
	};

	bool get_compact_hx_geom(int enum_compact_hx_config, double & d_out, double & fin_pitch, double & D_h,
		double & fin_thk, double & sigma, double & alpha, double & A_fin_to_surf,
		double & s_h, double & s_v, double & fin_V_per_m);

	bool get_compact_hx_f_j(int enum_compact_hx_config, double Re, double & f, double & j_H);

};

class C_CO2_to_air_cooler
{

public:
	
	int m_cost_model;		//[-]

	enum
	{
		// Techno-Economic Comparison of Solar-Driven SCO2 Brayton Cycles Using 
		// Component Cost Models Baselined with Vendor Data and Estimates
		// ASME ES 2017		
		E_CARLSON_17		//[-]
	};

	// Class to save messages for up stream classes
	C_csp_messages mc_messages;

	// Design parameters that are independent of the cycle-side inputs
	struct S_des_par_ind
	{
		double m_T_amb_des;		//[K] Design point ambient temperature
		double m_elev;			//[m] Elevation (used to calculate ambient pressure)

        double m_eta_fan;       //[-] Fan isentropic efficiency
        int m_N_nodes_pass;     //[-] Number of nodes per pass

		S_des_par_ind()
		{
			m_T_amb_des = m_elev = std::numeric_limits<double>::quiet_NaN();

            // Set realistic default values so model can solve without inputs for these values
            m_eta_fan = 0.5;
            m_N_nodes_pass = 10;
		}
	};

	// Design parameters that are dependent on the cycle-side performance
	struct S_des_par_cycle_dep
	{
		// Either specify mass flow rate or heat rejection rate
			// If mass flow rate is > 0.0, will use that
		double m_m_dot_total;		//[kg/s] Total sCO2 mass flow into air-cooler
		double m_Q_dot_des;			//[MWt] Heat rejected over specified temperatures

		double m_T_hot_in_des;		//[K] sCO2 hot side (inlet) temperature
		double m_P_hot_in_des;		//[kPa] sCO2 hot side (inlet) pressure
		double m_delta_P_des;		//[kPa] sCO2 pressure drop
		double m_T_hot_out_des;		//[K] sCO2 cold outlet temperature
		double m_W_dot_fan_des;		//[MWe] Design point fan power

		S_des_par_cycle_dep()
		{
			// One of these needs to be > 0.
				// Will check mass flow rate first
			m_m_dot_total = m_Q_dot_des = -1;

			// All of these must be set
			m_T_hot_in_des = m_P_hot_in_des = 
				m_delta_P_des = m_T_hot_out_des = m_W_dot_fan_des = std::numeric_limits<double>::quiet_NaN();
		}
	};

	struct S_des_solved
	{
		// Design thermodynamic conditions
		double m_m_dot_co2;	//[kg/s] Total CO2 flow rate
		double m_T_in_co2;	//[K] Hot CO2 inlet temperature
		double m_P_in_co2;	//[kPa] Hot CO2 inlet pressure
		double m_T_out_co2;	//[K] Cold CO2 outlet temperature
		double m_P_out_co2;	//[kPa] Cold CO2 outlet pressure
		double m_q_dot;		//[Wt] Heat exchanger duty

		double m_W_dot_fan;	//[MWe] Fan power

		// Design Ambient Conditions
		double m_P_amb_des;		//[Pa]

		int m_N_passes;		//[-] Number of serial passes in flow direction
		
		double m_d_out;		//[m] CO2 tube outer diameter
		double m_d_in;		//[m] CO2 tube inner diameter
		double m_Depth;		//[m] Dimension in loop/air flow direction
		double m_W_par;		//[m] Dimension of parallel flow paths
		double m_N_par;		//[-] Number of parallel flow paths
		double m_N_tubes;	//[-] Number of tubes
		double m_L_tube;	//[m] Tube length
		double m_UA_total;	//[W/K] Total air-side conductance at design
		double m_V_material_total;	//[m^3] Total Material volume - no headers
		double m_V_total;	//[m^3] Total HX footprint volume

		double m_L_node;	//[m] Tube length of one node
		double m_V_node;	//[m3] Volume of one node

		double m_cost;		//[M$] Cost

		S_des_solved()
		{
			m_N_passes = -1;

			m_m_dot_co2 = m_T_in_co2 = m_P_in_co2 = m_T_out_co2 = m_P_out_co2 = m_q_dot =
				m_W_dot_fan = m_P_amb_des = m_d_out = m_d_in = m_Depth = m_W_par = m_N_par =
				m_N_tubes = m_L_tube = m_UA_total = 
				m_V_material_total = m_V_total =
				m_L_node = m_V_node =
				m_cost = std::numeric_limits<double>::quiet_NaN();
		}
	};

	struct S_od_solved
	{
		double m_W_dot_fan;	    //[MWe]
        double m_P_co2_cold;    //[kPa]
        double m_T_co2_cold;    //[K]
        double m_q_dot;         //[MWt]


		S_od_solved()
		{
			m_W_dot_fan = m_P_co2_cold = m_T_co2_cold = m_q_dot = std::numeric_limits<double>::quiet_NaN();
		}
	};

private:

	// Classes
	HTFProperties mc_air;				// Instance of HTFProperties class for ambient air

	// Remaining Air-Cooler Specs
	// Inputs
	int m_N_nodes;			//[-]
	double m_th;			//[m]
	double m_eta_fan;		//[-]
	double m_roughness;		//[m]
	// Calculated
	double m_A_cs;			//[m^2]
	double m_relRough;		//[-]
	double m_L_path;		//[m]		Total flow path length
	double m_A_surf_total;	//[m^2]		Total air-side surface area
	double m_V_footprint;	//[m^3]		Total HX footprint
	double m_A_surf_node;	//[m^2]
	double m_V_material_tubes;	//[m^3] Total material required for tubing
	double m_V_material_fins;	//[m^3] Total material required for fins

	// Design Performance Targets
	double m_m_dot_air_des;		//[kg/s]
	double m_Q_dot_des;			//[W]

	// Calculated Performance Target
	double m_P_hot_out_des;		//[kPa]

	double m_T_co2_hot_max;		//[K]

	// HX geometry
		// Input
	int m_enum_compact_hx_config;
		// Defined from Config
	double m_fin_pitch;	//[1/m]
	double m_D_h;		//[m]
	double m_fin_thk;	//[m]
	double m_sigma;		//[-]
	double m_alpha;		//[1/m]
	double m_A_fin_to_surf;	//[-]
	double m_s_h;		//[m]
	double m_s_v;		//[m]
	double m_fin_V_per_m;	//[1/m]

	int m_final_outlet_index;

	// Structures
		// In
	S_des_par_ind ms_des_par_ind;
	S_des_par_cycle_dep ms_des_par_cycle_dep;
		// Out
	S_des_solved ms_hx_des_sol;

	S_od_solved ms_od_solved;

public:

	CO2_state mc_co2_props;

	C_CO2_to_air_cooler();

	~C_CO2_to_air_cooler(){};

	bool design_hx(S_des_par_ind des_par_ind, S_des_par_cycle_dep des_par_cycle_dep, double tol /*-*/);

	int off_design_given_T_out(double T_amb /*K*/, double T_hot_in /*K*/, double P_hot_in /*kPa*/,
		double m_dot_hot /*kg/s*/, double T_hot_out /*K*/, double tol_m_dot /*-*/, double tol_m_pressure /*-*/,
        double & W_dot_fan /*MWe*/, double & P_hot_out /*kPa*/);

    int off_design_given_fan_power(double T_amb /*K*/, double T_hot_in /*K*/, double P_hot_in /*kPa*/,
        double m_dot_hot /*kg/s*/, double W_dot_fan_target /*MWe*/, double tol_od /*-*/, double tol_pressure /*-*/,
        double & T_co2_out /*K*/, double & P_co2_out /*kPa*/);
	
	double get_total_hx_volume()
	{
		return ms_hx_des_sol.m_V_material_total;
	}

	const C_CO2_to_air_cooler::S_des_solved * get_design_solved()
	{
		return &ms_hx_des_sol;
	}

	C_CO2_to_air_cooler::S_od_solved get_od_solved()
	{
		return ms_od_solved;
	}

	const C_CO2_to_air_cooler::S_des_par_ind * get_des_par_ind()
	{
		return &ms_des_par_ind;
	}

	const C_CO2_to_air_cooler::S_des_par_cycle_dep * get_des_par_cycle_dep()
	{
		return &ms_des_par_cycle_dep;
	}

    class C_MEQ_od__T_co2_out__fan_power : public C_monotonic_equation
    {
    private:
        C_CO2_to_air_cooler *mpc_ac;

        double m_m_dot_co2_total;		//[kg/s] Hot fluid mass flow rate
        double m_T_hot_in;		//[K] Hot fluid outlet temperature
        double m_P_hot_in;		//[kPa] Hot fluid inlet pressure

        double m_T_amb;			//[K] Ambient temperature

        double m_tol_op;	//[-] convergence tolerance applied to this class
        double m_tol_pressure;  //[-] Tolerance for pressure convergence

    public:
        C_MEQ_od__T_co2_out__fan_power(C_CO2_to_air_cooler *pc_ac,
            double m_dot_co2_total /*kg/s*/, double T_hot_in /*K*/,
            double P_hot_in /*kPa*/,
            double T_amb /*K*/,
            double tol_op /*-*/, double tol_pressure /*-*/)
        {
            mpc_ac = pc_ac;

            m_m_dot_co2_total = m_dot_co2_total;	//[kg/s]
            m_T_hot_in = T_hot_in;	//[K]
            m_P_hot_in = P_hot_in;		//[kPa]

            m_T_amb = T_amb;			//[K]

            m_tol_op = tol_op;		//[-]
            m_tol_pressure = tol_pressure;  //[-]
        }

        double m_P_co2_out_calc;    //[kPa]

        virtual int operator()(double T_co2_out /*K*/, double *W_dot_fan_calc /*MWe*/);
    };

	class C_MEQ_od_air_mdot__T_co2_out : public C_monotonic_equation
	{
	private:
		C_CO2_to_air_cooler *mpc_ac;
		double m_m_dot_hot_tube;		//[kg/s] Hot fluid mass flow rate
		double m_T_hot_out;		//[K] Hot fluid outlet temperature
		double m_P_hot_in;		//[kPa] Hot fluid inlet pressure
		
		double m_T_amb;			//[K] Ambient temperature

		double m_tol_op;	    //[-] convergence tolerance applied to this class
        double m_tol_pressure;  //[-] Tolerance for pressure convergence

		double m_mu_air;    //[kg/m-s] dynamic viscosity
		double m_v_air;	    //[1/m3] specific volume
		double m_cp_air;	//[J/kg-K] specific heat convert from kJ/kg-K
		double m_k_air;	    //[W/m-K] conductivity
		double m_Pr_air;	//[-] Prandtl number

	public:
		C_MEQ_od_air_mdot__T_co2_out(C_CO2_to_air_cooler *pc_ac,
			double m_dot_hot_tube /*kg/s*/, double T_hot_out /*K*/, 
            double P_hot_in /*kPa*/,
			double T_amb /*K*/,
			double tol_op /*-*/, double tol_pressure /*-*/,
			double mu_air /*kg/m-s*/, double v_air /*1/m3*/, double cp_air /*J/kg-K*/,
			double k_air /*W/m-K*/, double Pr_air /*-*/)
		{
			mpc_ac = pc_ac;

			m_m_dot_hot_tube = m_dot_hot_tube;	//[kg/s]
			m_T_hot_out = T_hot_out;	//[K]
			m_P_hot_in = P_hot_in;		//[kPa]

			m_T_amb = T_amb;			//[K]

			m_tol_op = tol_op;		//[-]
            m_tol_pressure = tol_pressure;  //[-]

			m_mu_air = mu_air;
			m_v_air = v_air;
			m_cp_air = cp_air;
			m_k_air = k_air;
			m_Pr_air = Pr_air;

			m_W_dot_fan = std::numeric_limits<double>::quiet_NaN();
		}

		double m_W_dot_fan;		//[MWe]
        double m_P_co2_out;     //[kPa]
        double m_q_dot_tube;    //[kWt]

		virtual int operator()(double m_dot_air /*kg/s*/, double *T_hot_out_calc /*K*/);
	};

    class C_MEQ_node_energy_balance__h_co2_out : public C_monotonic_equation
    {
    private:
        CO2_state *mpc_co2_props;
        double m_h_co2_cold_out;	//[kJ/kg] CO2 cold side enthalpy
        double m_P_co2_cold_out;    //[kPa]

        double m_P_co2_hot_in;      //[kPa]

        double m_P_co2_node_ave;    //[kPa]

        double m_m_dot_co2_tube;	//[kg/s] CO2 mass flow rate through tube

        double m_T_air_cold_in;		//[K] Air cold temperature
        double m_C_dot_air;		    //[W/K] Air flow capacitance

        double m_UA_node;		//[W/K] Conductance of node - assuming air convective heat transfer is governing resistance
    
    public:
        C_MEQ_node_energy_balance__h_co2_out(CO2_state *mc_co2_props,
            double h_co2_cold_out /*kJ/kg*/, 
            double P_co2_cold_out /*kPa*/, double P_co2_hot_in /*kPa*/,
            double m_dot_co2_tube /*kg/s*/,
            double T_air_cold_in /*K*/, double C_dot_air /*W/K*/,
            double UA_node /*W/K*/)
        {
            mpc_co2_props = mc_co2_props;

            m_h_co2_cold_out = h_co2_cold_out;
            m_P_co2_cold_out = P_co2_cold_out;

            m_P_co2_hot_in = P_co2_hot_in;

            m_P_co2_node_ave = 0.5*(m_P_co2_hot_in + m_P_co2_cold_out); //[kPa]

            m_m_dot_co2_tube = m_dot_co2_tube;	//[kg/s]

            m_T_air_cold_in = T_air_cold_in;	//[K]
            m_C_dot_air = C_dot_air;	//[W/K]

            m_UA_node = UA_node;		//[W/K]

            m_Q_dot_node = std::numeric_limits<double>::quiet_NaN();
            m_T_co2_hot_in = std::numeric_limits<double>::quiet_NaN();

            CO2_PH(m_P_co2_cold_out, m_h_co2_cold_out, mpc_co2_props);
            m_T_co2_cold_out = mpc_co2_props->temp;     //[K]
        }

        double m_Q_dot_node;	    //[W]
        double m_T_co2_hot_in;      //[K]
        double m_T_co2_cold_out;    //[K]

        virtual int operator()(double h_co2_hot_in /*kJ/kg*/, double *diff_h_co2_hot /*-*/);
    };

    class C_MEQ_node_energy_balance__T_co2_out : public C_monotonic_equation
	{
	private:
		CO2_state *mpc_co2_props;
		double m_T_co2_cold_out;	//[K] CO2 inlet temperature
		double m_P_co2_ave;			//[kPa] Average CO2 pressure

		double m_m_dot_co2_tube;	//[kg/s] CO2 mass flow rate through tube
		
		double m_T_air_cold_in;		//[K] Air cold temperature
		double m_C_dot_air;		//[W/K] CO2 flow capacitance

		double m_UA_node;		//[W/K] Conductance of node - assuming air convective heat transfer is governing resistance

	public:
		C_MEQ_node_energy_balance__T_co2_out(CO2_state *mc_co2_props,
					double T_co2_cold_out /*K*/, double P_co2_ave /*kPa*/, 
					double m_dot_co2_tube /*kg/s*/, 
					double T_air_cold_in /*K*/, double C_dot_air /*W/K*/,
					double UA_node /*W/K*/)
		{
			mpc_co2_props = mc_co2_props;

			m_T_co2_cold_out = T_co2_cold_out;	//[K]
			m_P_co2_ave = P_co2_ave;	//[kPa]

			m_m_dot_co2_tube = m_dot_co2_tube;	//[kg/s]
			
			m_T_air_cold_in = T_air_cold_in;	//[K]
			m_C_dot_air = C_dot_air;	//[W/K]

			m_UA_node = UA_node;		//[W/K]

			m_Q_dot_node = std::numeric_limits<double>::quiet_NaN();
		}

		double m_Q_dot_node;	//[W]

		virtual int operator()(double T_co2_hot_in /*K*/, double *diff_T_co2_cold /*-*/);
	};

	class C_MEQ_target_CO2_dP__L_tube_pass : public C_monotonic_equation
	{
	private:
		C_CO2_to_air_cooler *mpc_ac;
		double m_W_par;		//[m] Dimension of parallel paths
		double m_N_par;		//[-] Number of tubes in parallel

		double m_m_dot_tube;//[kg/s] Mass flow rate through one tube

		double m_mu_air;	//[kg/m-s] dynamic viscosity
		double m_v_air;		//[1/m3] specific volume
		double m_cp_air;	//[J/kg-K] specific heat convert from kJ/kg-K
		double m_Pr_air;	//[-] Prandtl number

		double m_tol_upper;	//[-] Tolerance from upper level loop
        double m_tol_pressure;  //[-] Tolerance for pressure convergence
		
	public:
		C_MEQ_target_CO2_dP__L_tube_pass(C_CO2_to_air_cooler *pc_ac,
			double W_par /*m*/, double N_par /*-*/,
            double m_dot_tube /*kg/s*/,
			double mu_air /*kg/m-s*/, double v_air /*1/m3*/,
			double cp_air /*J/kg-K*/, double Pr_air /*-*/,
			double tol_upper /*-*/, double tol_pressure /*-*/)
		{
			mpc_ac = pc_ac;
			m_W_par = W_par;	//[m]
			m_N_par = N_par;	//[-]

			//m_P_hot_ave = P_hot_ave;
			m_m_dot_tube = m_dot_tube;

			m_mu_air = mu_air;
			m_v_air = v_air;
			m_cp_air = cp_air;
			m_Pr_air = Pr_air;

			m_tol_upper = tol_upper;	    //[-]
            m_tol_pressure = tol_pressure;  //[-]

			m_V_total = std::numeric_limits<double>::quiet_NaN();
			m_h_conv_air = std::numeric_limits<double>::quiet_NaN();
			m_m_dot_air_total = std::numeric_limits<double>::quiet_NaN();
			m_A_surf_node = std::numeric_limits<double>::quiet_NaN();
			m_T_co2_in_calc = std::numeric_limits<double>::quiet_NaN();
		}

		double m_V_total;		//[m^3] Total HX "footprint" Volume
		double m_h_conv_air;	//[W/m2-K] Convective coefficient
		double m_m_dot_air_total;		//[kg/s] Total air mass flow rate
		double m_A_surf_node;	//[m2] Air-side surface area of node

		double m_T_co2_in_calc;	//[K] Calculated hot CO2 inlet temperature

		virtual int operator()(double L_tube /*m*/, double *delta_P_co2 /*kPa*/);
	};

	class C_MEQ_target_T_hot__width_parallel : public C_monotonic_equation
	{
	private:
		C_CO2_to_air_cooler *mpc_ac;

		double m_mu_air;	//[kg/m-s] dynamic viscosity
		double m_v_air;		//[1/m3] specific volume
		double m_cp_air;	//[J/kg-K] specific heat convert from kJ/kg-K
		double m_Pr_air;	//[-] Prandtl number

        // Weighted values used for pre-iteration estimates
		double m_T_co2_deltaP_eval;	//[K] Representative temperature for property evaluation
		double m_P_hot_ave;			//[kPa]

		double m_tol;		//[-] Convergence tolerance
        double m_tol_pressure;  //[-] Tolerance for pressure convergence

	public:
		C_MEQ_target_T_hot__width_parallel(C_CO2_to_air_cooler *pc_ac,
			double mu_air /*kg/m-s*/, double v_air /*1/m3*/,
			double cp_air /*J/kg-K*/, double Pr_air /*-*/,
			double T_co2_deltaP_eval /*K*/, double P_hot_ave /*kPa*/,
			double tol /*-*/, double tol_pressure /*-*/)
		{
			mpc_ac = pc_ac;

			m_mu_air = mu_air;
			m_v_air = v_air;
			m_cp_air = cp_air;
			m_Pr_air = Pr_air;

			m_T_co2_deltaP_eval = T_co2_deltaP_eval;
			m_P_hot_ave = P_hot_ave;

			m_tol = tol;
            m_tol_pressure = tol_pressure;

			m_L_tube = std::numeric_limits<double>::quiet_NaN();
			m_N_par = std::numeric_limits<double>::quiet_NaN();
			m_N_tubes = std::numeric_limits<double>::quiet_NaN();
			m_V_total = std::numeric_limits<double>::quiet_NaN();
			m_h_conv_air = std::numeric_limits<double>::quiet_NaN();
			m_m_dot_air_total = std::numeric_limits<double>::quiet_NaN();
			m_A_surf_node = std::numeric_limits<double>::quiet_NaN();
		}

		double m_L_tube;	//[m]
		double m_N_par;		//[-]
		double m_N_tubes;	//[-]
		double m_V_total;	//[m^3] Total HX "footprint" Volume
		double m_h_conv_air;	//[W/m2-K] Convective coefficient
		double m_m_dot_air_total;		//[kg/s] Total air mass flow rate
		double m_A_surf_node;	//[m2] Air-side surface area of node

		virtual int operator()(double W_par /*m*/, double *T_co2_hot /*K*/);
	};

	double air_pressure(double elevation /*m*/);

	void calc_air_props(double T_amb /*K*/, double P_amb /*Pa*/,
		double & mu_air /*kg/m-s*/, double & v_air /*m3/kg*/, double & cp_air /*J/kg-K*/,
		double & k_air /*W/m-K*/, double & Pr_air);

	double calculate_cost(double UA /*kWt/K*/, double V_material /*m^3*/,
		double T_hot_in /*K*/, double P_hot_in /*kPa*/, double m_dot_hot /*kg/s*/);
};

int co2_outlet_given_geom_and_air_m_dot(double T_co2_out /*K*/, double m_dot_co2_tube /*kg/s*/,
    double P_cold_out /*kPa*/, double P_hot_in /*kPa*/,
    double T_amb /*K*/,
    double tol_h_in /*-*/, double tol_pressure /*-*/,
    C_csp_messages *mc_messages, CO2_state *co2_props,
    double d_in_tube /*m*/, double A_cs_tube /*m2*/, double relrough /*-*/,
    double L_node /*m*/, double V_node /*m3*/, int N_nodes /*-*/,
    double N_par /*-*/, int N_passes /*-*/,
    double alpha /*1/m*/, double cp_air /*J/kg-K*/,
    double m_dot_air_total /*kg/s*/, double h_conv_air /*W/m2-K*/,
    double & delta_P_co2_calc /*kPa*/, double & T_co2_in_calc /*K*/,
    double & q_dot_tube /*kWt*/);

class C_MEQ_target_W_dot_fan__m_dot_air : public C_monotonic_equation
{
private:
	double m_L_tube;	//[m] Length of tube in one pass (flow direction)
	double m_W_par;		//[m] Dimension of parallel paths
	double m_V_total;	//[m3] Total HX "footprint" volume

	double m_mu_air;	//[kg/m-s] dynamic viscosity
	double m_v_air;		//[m3/kg] specific volume
	double m_cp_air;	//[J/kg-K] specific heat convert from kJ/kg-K
	double m_Pr_air;	//[-] Prandtl number

	double m_sigma;		//[-]
	double m_D_h;		//[m]
	int m_comp_hx_config;	//[-]
	double m_alpha;		//[1/m]
	double m_eta_fan;	//[-]

public:
	C_MEQ_target_W_dot_fan__m_dot_air(double L_tube /*m*/, double W_par /*m*/, double V_total /*m3*/,
		double mu_air /*kg/m-s*/, double v_air /*m3/kg*/,
		double cp_air /*J/kg-K*/, double Pr_air /*-*/,
		double sigma, double D_h /*m*/,
		int comp_hx_config /*-*/,
		double alpha /*1/m*/, double eta_fan /*-*/)
	{
		m_L_tube = L_tube;
		m_W_par = W_par;
		m_V_total = V_total;

		m_mu_air = mu_air;
		m_v_air = v_air;
		m_cp_air = cp_air;
		m_Pr_air = Pr_air;

		m_sigma = sigma;
		m_D_h = D_h;

		m_comp_hx_config = comp_hx_config;
		m_alpha = alpha;
		m_eta_fan = eta_fan;

		m_h_conv_air = std::numeric_limits<double>::quiet_NaN();
	}

	double m_h_conv_air;	//[W/m2-K] Convective coefficient

	virtual int operator()(double m_dot_air /*kg/s*/, double *W_dot_fan /*MWe*/);
};

#endif
