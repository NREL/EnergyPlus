/*******************************************************************************************************
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

#ifndef __csp_solver_pt_receiver_
#define __csp_solver_pt_receiver_

#include "csp_solver_util.h"

#include "htf_props.h"
#include "csp_solver_core.h"

class C_pt_receiver
{
// The abstract parent class for all receivers, including the original steady-state and the transient receiver

public:
    virtual ~C_pt_receiver() {};

    C_csp_messages csp_messages;        // Class to save messages for upstream classes

    double m_h_tower;				    //[m] height of the tower
    double m_epsilon;				    //[-] emissivity of the receiver panels
    double m_T_htf_hot_des;			    //[C] hot outlet HTF temperature at design, converted to [K] in init()
    double m_T_htf_cold_des;		    //[C] cold inlet HTF temperature at design, converted to [K] in init()
    double m_f_rec_min;				    //[-] minimum receiver thermal output as fraction of design
    double m_q_rec_des;				    //[MW] design recever thermal output, converted to [W] in init()
    double m_rec_su_delay;			    //[hr] required startup time
    double m_rec_qf_delay;			    //[-] required startup energy as fraction of design thermal output
    double m_m_dot_htf_max_frac;	    //[-] maximum receiver HTF mass flow as fraction of design mass flow

    double m_q_dot_inc_min;             //[Wt] minimum receiver thermal power

    double m_eta_pump;					//[-] HTF pump efficiency
    int m_night_recirc;					//[-] 1=receiver is circulating HTF at night, otherwise not
	
	int m_clearsky_model;
	std::vector<double> m_clearsky_data;

    struct S_inputs
    {
        double m_field_eff;					                //[-] = (irradiance on receiver) / (I_bn * area of all heliostats)
        C_csp_collector_receiver::E_csp_cr_modes m_input_operation_mode;			                //[-] operating mode of collector receiver, corresponding to enum C_csp_collector_receiver::E_csp_cr_modes
        const util::matrix_t<double> *m_flux_map_input;		//[-] flux values for each receiver surface node, as fraction of an evenly distributed irradiance

        S_inputs()
        {
            m_field_eff = std::numeric_limits<double>::quiet_NaN();
            m_input_operation_mode = C_csp_collector_receiver::E_csp_cr_modes::OFF;
        }
    };

    struct S_outputs
    {
        double m_m_dot_salt_tot;		//[kg/hr] HTF mass flow through receiver
        double m_eta_therm;				//[-] receiver thermal efficiency
        double m_W_dot_pump;			//[MW] HTF pumping power
        double m_q_conv_sum;			//[MW] total receiver convection losses
        double m_q_rad_sum;				//[MW] total receiver radiation losses
        double m_Q_thermal;				//[MW] thermal power delivered to TES/PC: subtracts piping losses (q_dot_rec - q_dot_piping_losses)
        double m_T_salt_hot;			//[C] HTF outlet temperature
        double m_field_eff_adj;			//[-] heliostat field efficiency including component defocus
        double m_component_defocus;		//[-] defocus applied by receiver to stay within mass flow or other constraints
        double m_q_dot_rec_inc;			//[MWt] receiver incident thermal power (after reflection losses)
        double m_q_startup;				//[MWt-hr] thermal energy used to start receiver
        double m_dP_receiver;			//[bar] receiver pressure drop
        double m_dP_total;				//[bar] total pressure drop
        double m_vel_htf;				//[m/s] HTF flow velocity through receiver tubes
        double m_T_salt_cold;			//[C] HTF inlet temperature
        double m_m_dot_ss;				//[kg/hr] HTF mass flow during steady-state operation (e.g., not equal to m_m_dot_salt_tot during startup)
        double m_q_dot_ss;				//[MW] thermal power delivered to TES/PC during steady-state operation (e.g., not equal to m_Q_thermal during startup)
        double m_f_timestep;			//[-] fraction of nominal timestep the receiver is not starting up
        double m_time_required_su;		//[s] time it took receiver to startup
        double m_q_dot_piping_loss;		//[MWt] thermal power lost from piping to surroundings
        double m_q_heattrace;			//[MWt-hr] Power required for heat tracing
        double m_inst_T_salt_hot;		//[C] Instantaneous HTF outlet T at the end of the time step
        double m_max_T_salt_hot;		//[C] Maximum HTF outlet T during the time step
        double m_min_T_salt_hot;		//[C] Minimum HTF outlet T during the time step
        double m_max_rec_tout;			//[C] Maximum HTF T (at the receiver outlet before downcomer piping loss) during the time step
        double m_Twall_inlet;			//[C] Average receiver wall temperature at inlet
        double m_Twall_outlet;			//[C] Average receiver wall temperature at receiver outlet
        double m_Triser;				//[C] Average riser wall temperature at inlet
        double m_Tdownc;				//[C] Average downcomer wall temperature at outlet

		double m_clearsky;				//[W/m2] Clear-sky DNI used in receiver flow control 
		double m_Q_thermal_csky_ss;		//[MWt]  Steady-state thermal power delivered to TES/PC if DNI is equal to clear-sky DNI 
		double m_Q_thermal_ss;			//[MWt] Steady-state thermal power delivered to TES/PC 

        S_outputs()
        {
            clear();
        }

        void clear()
        {
            m_m_dot_salt_tot = m_eta_therm = m_W_dot_pump = m_q_conv_sum = m_q_rad_sum = m_Q_thermal =
                m_T_salt_hot = m_field_eff_adj = m_component_defocus = m_q_dot_rec_inc = m_q_startup =
                m_dP_receiver = m_dP_total = m_vel_htf = m_T_salt_cold = m_m_dot_ss = m_q_dot_ss = m_f_timestep =
                m_time_required_su = m_q_dot_piping_loss = m_q_heattrace = std::numeric_limits<double>::quiet_NaN();

			m_inst_T_salt_hot = m_max_T_salt_hot = m_min_T_salt_hot = m_max_rec_tout = m_Twall_inlet = m_Twall_outlet = 
				m_Triser = m_Tdownc = m_clearsky = m_Q_thermal_csky_ss = m_Q_thermal_ss = std::numeric_limits<double>::quiet_NaN();
        }
    };

    S_outputs ms_outputs;

    virtual void init() = 0;

    int get_operating_state();

    virtual void call(const C_csp_weatherreader::S_outputs &weather,
        const C_csp_solver_htf_1state &htf_state_in,
        const C_pt_receiver::S_inputs &inputs,
        const C_csp_solver_sim_info &sim_info) = 0;

    virtual void off(const C_csp_weatherreader::S_outputs &weather,
        const C_csp_solver_htf_1state &htf_state_in,
        const C_csp_solver_sim_info &sim_info) = 0;

    virtual void converged() = 0;

    virtual double get_pumping_parasitic_coef() = 0;

    HTFProperties *get_htf_property_object();

    virtual double get_startup_time();   //[s]

    virtual double get_startup_energy(); //[MWh]

    virtual double area_proj() = 0; //[m^2]

protected:
    C_pt_receiver();
    HTFProperties field_htfProps;       // heat transfer fluid properties
    HTFProperties tube_material;		// receiver tube material
    HTFProperties ambient_air;			// ambient air properties

    double m_m_dot_htf_des;             //[kg/s] receiver HTF mass flow at design
    C_csp_collector_receiver::E_csp_cr_modes m_mode;                         //[-] current operating mode of receiver
    C_csp_collector_receiver::E_csp_cr_modes m_mode_prev;                    //[-] operating mode of receiver at end of last converged timestep

    std::string error_msg;              // member string for exception messages
	
	virtual double get_clearsky(const C_csp_weatherreader::S_outputs &weather, double hour);

};

#endif  // __csp_solver_pt_receiver_
