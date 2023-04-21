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

#ifndef __csp_solver_trough_collector_receiver_
#define __csp_solver_trough_collector_receiver_

#include "csp_solver_core.h"
#include "htf_props.h"
#include "tcstype.h"

#include "lib_weatherfile.h"
#include <cmath>
#include "sam_csp_util.h"
#include "interconnect.h"

#include "numeric_solvers.h"

#include <iosfwd>
#include <fstream>

class C_csp_trough_collector_receiver : public C_csp_collector_receiver
{

public:

	enum
	{
		E_THETA_AVE,			//[deg]
		E_COSTH_AVE,			//[-]
		E_IAM_AVE,				//[-]
		E_ROWSHADOW_AVE,		//[-]
		E_ENDLOSS_AVE,			//[-]
		E_DNI_COSTH,			//[W/m2]
		E_EQUIV_OPT_ETA_TOT,	//[-]
		E_DEFOCUS,				//[-]

		E_Q_DOT_INC_SF_TOT,        //[MWt]
		E_Q_DOT_INC_SF_COSTH,	   //[MWt]
		E_Q_DOT_REC_INC,		   //[MWt]
		E_Q_DOT_REC_THERMAL_LOSS,  //[MWt]
		E_Q_DOT_REC_ABS,		   //[MWt]
		E_Q_DOT_PIPING_LOSS,	   //[MWt]
		E_E_DOT_INTERNAL_ENERGY,   //[MWt]
		E_Q_DOT_HTF_OUT,		   //[MWt]
		E_Q_DOT_FREEZE_PROT,       //[MWt]

		E_M_DOT_LOOP,				//[kg/s]
        E_IS_RECIRCULATING,         //[-]
		E_M_DOT_FIELD_RECIRC,		//[kg/s]
		E_M_DOT_FIELD_DELIVERED,	//[kg/s]
		E_T_FIELD_COLD_IN,	//[C]
		E_T_REC_COLD_IN,	//[C]
		E_T_REC_HOT_OUT,	//[C]
		E_T_FIELD_HOT_OUT,	//[C]
		E_PRESSURE_DROP,	//[bar]

		E_W_DOT_SCA_TRACK,	//[MWe]
		E_W_DOT_PUMP		//[MWe]
	};

	C_csp_reported_outputs mc_reported_outputs;

private:

	// Member classes
	HTFProperties m_htfProps, m_airProps;
	
	// Hardcoded constants
	double m_d2r, m_r2d, m_mtoinch;
	double m_T_htf_prop_min;	//[K] Minimum temperature allowed in props call to minimize errors

	// Init() inputs
	double m_latitude;		//[deg] convert to [rad] in init()
	double m_longitude;		//[deg] convert to [rad] in init()
	double m_shift;			//[deg] convert to [rad] in init()

	// Parameters calculated in init()
	int m_n_c_iam_matrix;	//[-] Number of columns in the IAM matrix
	int m_n_r_iam_matrix;	//[-] Number of rows in the IAM matrix
	double m_v_hot;			//[m^3] Hot piping volume
	double m_v_cold;		//[m^3] Cold piping volume
	double m_Ap_tot;		//[m^2] Total field aperture area
	int	m_nfsec;			//[-] Number of field sections
	int	m_nhdrsec;			//[-] Number of header sections
	int	m_nrunsec;			//[-] Number of unique runner diameters
	double m_L_tot;			//[m] Total length of collectors in a loop
	double m_opteff_des;	//[-] Design-point optical efficieny (theta = 0) from the solar field
	double m_m_dot_design;	//[kg/s] Total solar field mass flow rate at design
	double m_m_dot_loop_des;//[kg/s] LOOP design mass flow rate
	double m_q_design;		//[Wt] Design-point thermal power from the solar field
	double m_W_dot_sca_tracking_nom;		//[MWe] Tracking parasitics when trough is on sun

	std::vector<double> m_L_actSCA;	//[m] Lengths of each SCA
	emit_table m_epsilon_3;			// Table of emissivity vs temperature for each variant of each receiver type
	util::matrix_t<HTFProperties*> m_AnnulusGasMat;		// HTF Property class for each variant of each receiver type
	util::matrix_t<AbsorberProps*> m_AbsorberPropMat;	// Absorber Property class for each variant of each receiver type

	util::matrix_t<double> m_A_cs;	//[m^2] Cross-sectional area for HTF flow for each receiver and variant (why variant?)
	util::matrix_t<double> m_D_h;	//[m^2] Hydraulic diameters for HTF flow for each receiver and variant (why variant?)	

	// Variables that we need to track between calls during one timestep
	double m_T_cold_in_1;	//[K] Calculated HTF inlet temperature
	double m_defocus;		//[-] Defocus during present call = m_defocus_new / m_defocus_old
	double m_defocus_new;	//[-] Defocus signal from controller during present timestep
	double m_defocus_old;	//[-] Defocus during previous call (= 1 at first call)
	bool m_no_fp;			//[-] Did previous call require freeze protection (T = no, F = yes) 
	double m_m_dot_htfX;	//[kg/s] Loop mass flow rate solved in previous timestep
	int m_ncall;			//[-] Track number of calls per timestep, reset = -1 in converged() call
	
	// Variables that are passed between methods, but not necessary to carry over timesteps
	double m_m_dot_htf_tot;	//[kg/s] The total flow rate through the entire field (m_dot_loop * N_loops)
	double m_c_htf_ave;		//[J/kg-K] Average solar field specific heat

	std::vector<double> m_E_int_loop;	//[J] Energy relative to ambient for each receiver
	std::vector<double> m_E_accum;		//[J] Internal energy change in timestep for each receiver
	std::vector<double> m_E_avail;		//[J] Energy absorbed less internal energy change for each receiver
	std::vector<double> m_q_abs_SCAtot;	//[W] Heat absorption into HTF in each SCA, weighted variants
	std::vector<double> m_q_loss_SCAtot;//[W] Total heat losses from each SCA, weighted variants
	std::vector<double> m_q_1abs_tot;	//[W/m] Thermal losses from each SCA, weighted variants

	std::vector<double> m_q_loss;		//[W/m] Total thermal losses per length in each SCA, one variant
	std::vector<double> m_q_abs;		//[W/m] Total heat absorption per length into HTF in each SCA, one variant
	std::vector<double> m_q_1abs;		//[W/m] Total *thermal* losses per length in each SCA, one variant
	std::vector<double> m_q_i;			//[W/m] DNI * A_aper / L_sca
		// This value (m_q_SCA) is passed to the Evacuated Tube model, where the other optical losses are applied
	std::vector<double> m_q_SCA;		//[W/m] Total incident irradiation on the receiver (q"*A_aper/L_sca*cos(theta)*all_defocus)
	std::vector<double> m_q_SCA_control_df;	//[W/m] Total incident irradiation less CONTROL defocus (m_q_sca * control_defocus)

	std::vector<double> m_IAM;			//[-] Incidence angle modifiers
	std::vector<double> m_RowShadow;	//[-] Row-to-row shadowing losses

	/*m_nColt, m_nSCA*/
	util::matrix_t<double> m_ColOptEff;	//[-] tracking * geom * rho * dirt * error * IAM * row shadow * end loss * ftrack
	util::matrix_t<double> m_EndGain;	//[-] Light from different collector hitting receiver
	util::matrix_t<double> m_EndLoss;	//[-] Light missing receiver due to length

	double m_Theta_ave;			//[rad] Field average m_theta value (but... nothing in our model allows for this to different over SCAs)
	
	double m_CosTh_ave;			//[-] Field average costheta value
	double m_IAM_ave;			//[-] Field average incidence angle modifier
	double m_RowShadow_ave;		//[-] Field average row shadowing loss
	double m_EndLoss_ave;		//[-] Field average end loss
	
	double m_costh;				//[-] Cosine of the incidence angle between sun and trough aperture
	double m_dni_costh;			//[W/m2] DNI x cos(theta) product
	double m_W_dot_sca_tracking;	//[MWe] SCA tracking power
	// Collector-receiver equivalent(weighted over variants AND all SCAs) optical efficiency
		// m_ColOptEff * m_Shadowing * m_Dirt_HCE * m_alpha_abs * m_tau_envelope
	double m_EqOpteff;			//[-] 
	double m_control_defocus;	//[-] Defocus signal from control model
	double m_component_defocus;	//[-] Defocus signal from this component (max mass flow rate reached and still over target...)

	double m_q_dot_inc_sf_tot;	//[MWt] Total incident radiation on solar field

	double m_Header_hl_cold;	//[W] Total heat loss from the cold headers *in one field section*
    double m_Header_hl_cold_tot;
	double m_Runner_hl_cold;	//[W] Total heat loss from the cold runners *in one field section*
    double m_Runner_hl_cold_tot;

	double m_Header_hl_hot;		//[W] Total heat loss from the hot headers *in one field section*
    double m_Header_hl_hot_tot;
	double m_Runner_hl_hot;		//[W] Total heat loss from the hot runners *in one field section*
    double m_Runner_hl_hot_tot;

    double Intc_hl;             //[W] Total heat loss from the loop interconnects *in one field section*

	double m_c_hdr_cold;		//[J/kg-K] Specific heat of fluid at m_T_sys_c
	double m_c_hdr_hot;			//[J/kg-K] Specific heat of fluid at outlet temperature of last SCA (not necessarily return temperature if modeling runners and headers)

	double m_mc_bal_hot;		//[J/K] The heat capacity of the balance of plant on the hot side
	double m_mc_bal_cold;		//[J/K] The heat capacity of the balance of plant on the cold side

    double m_T_loop_in;
    double m_P_field_in;

	// Classes that are defined as member data so are re-declared each time performance function is called
	std::vector<double> m_DP_tube;	//[Pa] Pressure drops in each receiver

	// *********************************************
	// TCS Temperature Tracking
		// Temperatures from the most recent converged() operation
	double m_TCS_T_sys_c_converged;					//[K] Temperature (bulk) of cold runners & headers in previous timestep
	std::vector<double> m_TCS_T_htf_ave_converged;	//[K] Average HTF temperature in each SCA
	double m_TCS_T_sys_h_converged;					//[K] Temperature (bulk) of hot runners & headers in previous timestep		
	
		// Temperatures from the most recent timestep (in the event that a method solves multiple, shorter timesteps)
	double m_TCS_T_sys_c_last;					//[K] Temperature (bulk) of cold runners & headers in previous timestep
	std::vector<double> m_TCS_T_htf_ave_last;	//[K] Average HTF temperature in each SCA
	double m_TCS_T_sys_h_last;					//[K] Temperature (bulk) of hot runners & headers in previous timestep		
	
		// Latest temperatures solved during present call to this class
	double m_TCS_T_sys_c;					//[K] Temperature (bulk) of cold runners & headers
	std::vector<double> m_TCS_T_htf_in;		//[K] Inlet HTF temperature to each SCA
	std::vector<double> m_TCS_T_htf_ave;	//[K] Average HTF temperature in each SCA
	std::vector<double> m_TCS_T_htf_out;	//[K] Outlet HTF temperature to each SCA
	double m_TCS_T_sys_h;					//[K] Solar field HTF outlet temperature
	// *********************************************
	// *********************************************

	// *********************************************
	// CSP Solver Temperature Tracking
		// Temperatures from the most recent converged() operation
	double m_T_sys_c_t_end_converged;
	std::vector<double> m_T_htf_out_t_end_converged;
	double m_T_sys_h_t_end_converged;
			// ** Check for these in other methods developed for CSP Solver **

		// Temperatures from the most recent timstep (in the event that a method solves multiple, shorter timesteps
	double m_T_sys_c_t_end_last;			//[K] Temperature (bulk) of cold runners & headers at end of previous timestep
	std::vector<double> m_T_htf_out_t_end_last;			//[K] Temperature of HTF temperature & material at end of previous timestep
	double m_T_sys_h_t_end_last;			//[K] Temperature (bulk) of hot runners & headers at end of previous timestep

			// ** Check for these in other methods developed for CSP Solver **
	
		// Latest temperature solved during present call to this class
			// SUB TIMESTEP outputs
	double m_T_sys_c_t_end;				//[K] Temperature (bulk) of cold runners & headers at end of current timestep
	double m_T_sys_c_t_int;				//[K] Temperature (bulk) of cold runners & headers at time-INTegrated-average	
	std::vector<double> m_T_htf_in_t_int;	//[K] time-integrated-average inlet HTF temperature to each SCA
	std::vector<double> m_T_htf_out_t_end;	//[K] end-of-timestep outlet HTF temperature of each SCA
	std::vector<double> m_T_htf_out_t_int;	//[K] time-integrated-average outlet HTF temp of each SCA
	double m_T_sys_h_t_end;				//[K] Temperature (bulk) of hot runners & headers at end of current timestep
	double m_T_sys_h_t_int;				//[K] Temperature (bulk) of hot runners & headers at timestep-integrated-average

	double m_Q_field_losses_total_subts;	//[MJ] SYSTEM scas + xover + hot_HR + cold_HR
	double m_c_htf_ave_ts_ave_temp;			//[J/kg-K] integrated-averaged cp over T_htf_cold_in, m_T_sys_h_t_in
	
	double m_q_dot_sca_loss_summed_subts;	//[MWt] SYSTEM SCA heat loss
	double m_q_dot_sca_abs_summed_subts;	//[MWt] SYSTEM SCA absorbed thermal power (into HTF stream & material)
	double m_q_dot_xover_loss_summed_subts;	//[MWt] SYSTEM Cross-over/connecting piping heat loss
	double m_q_dot_HR_cold_loss_subts;		//[MWt] SYSTEM Cold header heat loss
	double m_q_dot_HR_hot_loss_subts;		//[MWt] SYSTEM Hot header heat loss
	double m_E_dot_sca_summed_subts;		//[MWt] SYSTEM SCA internal energy change over time
	double m_E_dot_xover_summed_subts;		//[MWt] SYSTEM Cross-over/connecting piping internal energy change over time
	double m_E_dot_HR_cold_subts;			//[MWt] SYSTEM Cold header internal energy change
	double m_E_dot_HR_hot_subts;			//[MWt] SYSTEM hot header internal energy change
	double m_q_dot_htf_to_sink_subts;		//[MWt] SYSTEM thermal power to sink (or artificially added to system in recirculation...)
	// *********************************************
	// *********************************************
			// Full Timestep outputs
	double m_T_sys_c_t_int_fullts;			//[K] Temperature (bulk) of cold runners & headers at end of current timestep
	double m_T_htf_c_rec_in_t_int_fullts;	//[K] Time-integrated-average inlet HTF temperature to FIRST sca
	double m_T_htf_h_rec_out_t_int_fullts;	//[K] Time-integrated-average outlet HTF temperature from LAST sca
	double m_T_sys_h_t_int_fullts;			//[K] Temperature (bulk) of hot runners & headers at timestep-integrated-average

	double m_q_dot_sca_loss_summed_fullts;	//[MWt] SYSTEM SCA heat loss
	double m_q_dot_sca_abs_summed_fullts;	//[MWt] SYSTEM SCA absorbed thermal power (into HTF stream & material)
	double m_q_dot_xover_loss_summed_fullts;//[MWt] SYSTEM Cross-over/connecting piping heat loss
	double m_q_dot_HR_cold_loss_fullts;		//[MWt] SYSTEM Cold header heat loss
	double m_q_dot_HR_hot_loss_fullts;		//[MWt] SYSTEM Hot header heat loss
	double m_E_dot_sca_summed_fullts;		//[MWt] SYSTEM SCA internal energy change over time
	double m_E_dot_xover_summed_fullts;		//[MWt] SYSTEM Cross-over/connecting piping internal energy change over time
	double m_E_dot_HR_cold_fullts;			//[MWt] SYSTEM Cold header internal energy change
	double m_E_dot_HR_hot_fullts;			//[MWt] SYSTEM hot header internal energy change
	double m_q_dot_htf_to_sink_fullts;		//[MWt] SYSTEM thermal power to sink (or artificially added to system in recirculation...)
	double m_q_dot_freeze_protection;		//[MWt] SYSTEM thermal freeze protection

	double m_dP_total;						//[bar] FIELD pressure drop
	double m_W_dot_pump;					//[MWe] FIELD pumping power

	bool m_is_m_dot_recirc;		//[-] True: trough is recirculationg HTF with interacting with other CSP components

	bool m_ss_init_complete;	//[-] For TCS-based model in acceptance testing, has model achieved steady state at first timestep?

	// Member variables that are used to store information for the EvacReceiver method
	double m_T_save[5];			//[K] Saved temperatures from previous call to EvacReceiver single SCA energy balance model
	std::vector<double> mv_reguess_args;	//[-] Logic to determine whether to use previous guess values or start iteration fresh
	
	// member string for exception messages
	std::string m_error_msg;

	int m_operating_mode_converged;
	int m_operating_mode;

	int freeze_protection(const C_csp_weatherreader::S_outputs &weather, 
					double & T_cold_in /*K*/, double m_dot_loop /*kg/s*/, 
					const C_csp_solver_sim_info &sim_info, double & Q_fp /*MJ*/);

	double field_pressure_drop(double T_db, double m_dot_field, double P_in_field,
        const std::vector<double> &T_in_SCA, const std::vector<double> &T_out_SCA);

	void set_output_value();

public:

	// Class to save messages for up stream classes
	// C_csp_messages mc_csp_messages;

	//parameters and inputs
	int m_nSCA;				//[-] Number of SCA's in a loop
	int m_nHCEt;			//[-] Number of HCE types
	int m_nColt;			//[-] Number of collector types
	int m_nHCEVar;			//[-] Number of HCE variants per type
	int m_nLoops;			//[-] Number of loops in the field
	int m_FieldConfig;		//[-] Number of subfield headers
	bool m_include_fixed_power_block_runner;	//[-] True: model 50[m] of runner sized for full mass flow rate
	double m_L_power_block_piping;	//[m] Length of piping (full mass flow) through heat sink (if applicable)
	double m_eta_pump;		//[-] HTF pump efficiency
	double m_HDR_rough;		//[m] Header pipe roughness
	double m_theta_stow;	//[deg] stow angle
	double m_theta_dep;		//[deg] deploy angle
	double m_Row_Distance;	//[m] Spacing between rows (centerline to centerline)
	double m_T_startup;		//[C] The required temperature (converted to K in init) of the system before the power block can be switched on
	double m_m_dot_htfmin;	//[kg/s] Minimum loop HTF flow rate
	double m_m_dot_htfmax;	//[kg/s] Maximum loop HTF flow rate
	double m_T_loop_in_des;	//[C] Design loop inlet temperature, converted to K in init
	double m_T_loop_out_des;//[C] Target loop outlet temperature, converted to K in init
	int m_Fluid;			//[-] Field HTF fluid number
	
	double m_T_fp;			//[C] Freeze protection temperature (heat trace activation temperature), convert to K in init
	double m_I_bn_des;		//[W/m^2] Solar irradiation at design
    double m_V_hdr_cold_max;    //[m/s] Maximum HTF velocity in the cold header at design
    double m_V_hdr_cold_min;    //[m/s] Minimum HTF velocity in the cold header at design
    double m_V_hdr_hot_max;     //[m/s] Maximum HTF velocity in the hot header at design
    double m_V_hdr_hot_min;     //[m/s] Minimum HTF velocity in the hot header at design
    double m_V_hdr_max;		//[m/s] Maximum HTF velocity in the header at design
	double m_V_hdr_min;		//[m/s] Minimum HTF velocity in the header at design
	double m_Pipe_hl_coef;	//[W/m2-K] Loss coefficient from the header, runner pipe, and non-HCE piping
	double m_SCA_drives_elec;	//[W/SCA] Tracking power, in Watts per SCA drive
	int m_fthrok;			//[-] Flag to allow partial defocusing of the collectors
	int m_fthrctrl;			//[-] Defocusing strategy
	double m_ColTilt;		//[deg] Collector tilt angle (0 is horizontal, 90deg is vertical)
	double m_ColAz;			//[deg] Collector azimuth angle
	double m_wind_stow_speed;//[m/s] Wind speed at and above which the collectors will be stowed

	int m_accept_mode;		//[-] Acceptance testing mode? (1=yes, 0=no)
	bool m_accept_init;		//[-] In acceptance testing mode - require steady-state startup
	int m_accept_loc;		//[-] In acceptance testing mode - temperature sensor location (1=hx,2=loop)
	bool m_is_using_input_gen;

	double m_solar_mult;		//[-] Solar multiple 
	double m_mc_bal_hot_per_MW;		//[kWht/K-MWt] The heat capacity per MWt design of the balance of plant on the hot side
	double m_mc_bal_cold_per_MW;	//[kWht/K-MWt] The heat capacity per MWt design of the balance of plant on the cold side
	double m_mc_bal_sca;		//[Wht/K-m] Non-HTF heat capacity associated with each SCA - per meter basis

	std::vector<double> m_W_aperture;	//[m] The collector aperture width (Total structural area.. used for shadowing)
	std::vector<double> m_A_aperture;	//[m^2] Reflective aperture area of the collector
	std::vector<double> m_TrackingError;//[-] Tracking error derate
	std::vector<double> m_GeomEffects;	//[-] Geometry effects derate
	std::vector<double> m_Rho_mirror_clean;	//[-] Clean mirror reflectivity
	std::vector<double> m_Dirt_mirror;	//[-] Dirt on mirror derate
	std::vector<double> m_Error;		//[-] General optical error derate
	std::vector<double> m_Ave_Focal_Length; //[m] The average focal length of the collector 
	std::vector<double> m_L_SCA;		//[m] The length of the SCA 
	std::vector<double> m_L_aperture;	//[m] The length of a single mirror/HCE unit
	std::vector<double> m_ColperSCA;	//[-] The number of individual collector sections in an SCA
	std::vector<double> m_Distance_SCA; //[m] Piping distance between SCA's in the field
	
	std::vector<int> m_SCADefocusArray; //[-] Order in which the SCA's should be defocused

	util::matrix_t<double> m_field_fl_props;	//[-] User-defined field HTF properties

	util::matrix_t<double> m_HCE_FieldFrac,  //[-] Fraction of the field occupied by this HCE type
	m_D_2, 									 //[m] Inner absorber tube diameter
	m_D_3, 									 //[m] Outer absorber tube diameter
	m_D_4, 									 //[m] Inner glass envelope diameter
	m_D_5, 									 //[m] Outer glass envelope diameter
	m_D_p, 									 //[m] Diameter of the absorber flow plug (optional)
	m_Flow_type, 							 //[-] Flow type through the absorber
	m_Rough, 								 //[m] Roughness of the internal surface
	m_alpha_env, 							 //[-] Envelope absorptance
	
	//[-] Emittance vs. temperature profile for each receiver type and variation
	m_epsilon_3_11, m_epsilon_3_12,	m_epsilon_3_13, m_epsilon_3_14, 
	m_epsilon_3_21, m_epsilon_3_22, m_epsilon_3_23, m_epsilon_3_24, 
	m_epsilon_3_31, m_epsilon_3_32, m_epsilon_3_33,	m_epsilon_3_34,
	m_epsilon_3_41, m_epsilon_3_42, m_epsilon_3_43, m_epsilon_3_44, 
	
	m_alpha_abs,             //[-] Absorber absorptance
	m_Tau_envelope, 		 //[-] Envelope transmittance
	m_EPSILON_4, 			 //[-] Inner glass envelope emissivities
	m_EPSILON_5,			 //[-] Outer glass envelope emissivities
	m_P_a, 					 //[torr] Annulus gas pressure				 
	m_AnnulusGas, 			 //[-] Annulus gas type (1=air, 26=Ar, 27=H2)
	m_AbsorberMaterial, 	 //[-] Absorber material type
	m_Shadowing, 			 //[-] Receiver bellows shadowing loss factor
	m_Dirt_HCE, 			 //[-] Loss due to dirt on the receiver envelope
	m_Design_loss, 			 //[-] Receiver heat loss at design
	m_SCAInfoArray;          //[-] Receiver (,1) and collector (,2) type for each assembly in loop
    
    double m_rec_su_delay;   //[hr] Fixed startup delay time for the receiver
    double m_rec_qf_delay;   //[-] Energy-based receiver startup delay (fraction of rated thermal power)
    double m_p_start;        //[kWe-hr] Collector startup energy, per SCA

	util::matrix_t<double> m_IAM_matrix;		  //[-] IAM coefficients, matrix for 4 collectors
	
	util::matrix_t<bool> m_GlazingIntact;		  //[-] Glazing intact (broken glass) flag {1=true, else=false}

    bool m_calc_design_pipe_vals;                 //[-] Should the HTF state be calculated at design conditions
    double m_L_rnr_pb;                            //[m] Length of hot or cold runner pipe around the power block
    double m_N_max_hdr_diams;                     //[-] Maximum number of allowed diameters in each of the hot and cold headers
    double m_L_rnr_per_xpan;                      //[m] Threshold length of straight runner pipe without an expansion loop
    double m_L_xpan_hdr;                          //[m] Combined length in meters of the two perpendicular segments of a header expansion loop
    double m_L_xpan_rnr;                          //[m] Combined length in meters of the two perpendicular segments of a runner expansion loop
    double m_Min_rnr_xpans;                       //[-] Minimum number of expansion loops per single-diameter runner section
    double m_northsouth_field_sep;                //[m] Shortest north/south distance between SCAs in different subfields
    double m_N_hdr_per_xpan;                      //[-] Number of collector loops per header expansion loops. 1 = expansion loop between every collector loop
    double m_offset_xpan_hdr;                     //[-] Location of first header expansion loop. 1 = after first collector loop
    util::matrix_t<double> m_K_cpnt;              //[-] Minor loss coefficients of the components in each loop interconnect
    util::matrix_t<double> m_D_cpnt;              //[m] Inner diameters of the components in each loop interconnect
    util::matrix_t<double> m_L_cpnt;              //[m] Lengths of the components in each loop interconnect
    util::matrix_t<double> m_Type_cpnt;           //[-] Type of component in each loop interconnect [0=fitting | 1=pipe | 2=flex_hose]
    util::matrix_t<double> m_rough_cpnt;
    util::matrix_t<double> m_u_cpnt;
    util::matrix_t<double> m_mc_cpnt;
    bool m_custom_sf_pipe_sizes;                  //[-] Should the field pipe diameters, wall thickness and lengths be imported instead of calculated
    util::matrix_t<double> m_sf_rnr_diams;        //[m] Imported runner diameters, used if custom_sf_pipe_sizes is true
    util::matrix_t<double> m_sf_rnr_wallthicks;   //[m] Imported runner wall thicknesses, used if custom_sf_pipe_sizes is true
    util::matrix_t<double> m_sf_rnr_lengths;      //[m] Imported runner lengths, used if custom_sf_pipe_sizes is true
    util::matrix_t<double> m_sf_hdr_diams;        //[m] Imported header diameters, used if custom_sf_pipe_sizes is true
    util::matrix_t<double> m_sf_hdr_wallthicks;   //[m] Imported header wall thicknesses, used if custom_sf_pipe_sizes is true
    util::matrix_t<double> m_sf_hdr_lengths;      //[m] Imported header lengths, used if custom_sf_pipe_sizes is true

    std::vector<double> m_D_runner;	              //[m]    Diameters of runner sections
    std::vector<double> m_WallThk_runner;	      //[m]    Pipe wall thicknesses of runner sections
    std::vector<double> m_m_dot_rnr_dsn;          //[kg/s] Design mass flow through runner sections
    std::vector<double> m_V_rnr_dsn;              //[m/s]  Design velocity through runner sections
    std::vector<double> m_L_runner;	              //[m]    Lengths of runner sections
    std::vector<int> m_N_rnr_xpans;               //[-]    Number of expansions in runner sections
    std::vector<double> m_DP_rnr;                 //[bar]  Pressure drop in runner sections
    std::vector<double> m_T_rnr_dsn;              //[C]    Temperature entering runner sections at design
    std::vector<double> m_P_rnr_dsn;              //[bar]  Gauge pessure in runner sections at design
    std::vector<double> m_T_rnr;                  //[K]    Temperature entering runner sections
    double m_T_field_out;                         //[K]    Temperature exiting last runner, and thus exiting field
    std::vector<double> m_P_rnr;                  //[Pa ]  Gauge pessure in runner sections
                                                  
    std::vector<double> m_D_hdr;	              //[m]    Diameters of header sections
    std::vector<double> m_WallThk_hdr;   	      //[m]    Pipe wall thicknesses of header sections
    std::vector<double> m_m_dot_hdr_dsn;          //[kg/s] Design mass flow through header sections
    std::vector<double> m_V_hdr_dsn;              //[m/s]  Design velocity through header sections
    std::vector<double> m_L_hdr;	              //[m]    Lengths of header sections
    std::vector<int> m_N_hdr_xpans;               //[-]    Number of expansions in header sections
    std::vector<double> m_DP_hdr;                 //[bar]  Pressure drop in header sections
    std::vector<double> m_T_hdr_dsn;              //[C]    Temperature entering header sections at design
    std::vector<double> m_P_hdr_dsn;              //[bar]  Gauge pessure in header sections at design
    std::vector<double> m_T_hdr;                  //[K]    Temperature entering header sections
    std::vector<double> m_P_hdr;                  //[Pa]   Gauge pessure in header sections
                                                  
    std::vector<double> m_DP_loop;                //[bar]  Pressure drop in loop sections
    std::vector<double> m_T_loop_dsn;             //[C]    Temperature entering loop sections at design
    std::vector<double> m_P_loop_dsn;             //[bar]  Gauge pessure in loop sections at design
    std::vector<double> m_T_loop;                 //[K]    Temperature entering loop sections
    std::vector<double> m_P_loop;                 //[Pa]   Gauge pessure in loop sections

    vector<interconnect> m_interconnects;

	// **************************************************************************
	// **************************************************************************
	// **************************************************************************


	// ***********************
	// ***** T  E  M  P ******
	double m_step_recirc;

	std::ofstream m_outfile;	//("example.txt", std::ofstream); //("C:/Users/tneises/Documents/2015 SuNLaMP Projects/SAM/FY16/Q2 reporting/debug.csv");

	// ***********************
	// ***********************

	C_csp_trough_collector_receiver();

	~C_csp_trough_collector_receiver(){};

	virtual void init(const C_csp_collector_receiver::S_csp_cr_init_inputs init_inputs, 
				C_csp_collector_receiver::S_csp_cr_solved_params & solved_params);
	virtual bool init_fieldgeom();	

	virtual double get_startup_time();
	virtual double get_startup_energy(); //MWh
	virtual double get_pumping_parasitic_coef();  //MWe/MWt
	virtual double get_min_power_delivery();    //MWt
	virtual double get_tracking_power();		//MWe
	virtual double get_col_startup_power();		//MWe-hr

	virtual int get_operating_state();
	
	virtual void get_design_parameters(C_csp_collector_receiver::S_csp_cr_solved_params & solved_params);

	virtual void off(const C_csp_weatherreader::S_outputs &weather,
		const C_csp_solver_htf_1state &htf_state_in,
		C_csp_collector_receiver::S_csp_cr_out_solver &cr_out_solver,
		const C_csp_solver_sim_info &sim_info);

	virtual void startup(const C_csp_weatherreader::S_outputs &weather,
		const C_csp_solver_htf_1state &htf_state_in,
		C_csp_collector_receiver::S_csp_cr_out_solver &cr_out_solver,
		const C_csp_solver_sim_info &sim_info);

	virtual void on(const C_csp_weatherreader::S_outputs &weather,
		const C_csp_solver_htf_1state &htf_state_in,
		double field_control,
		C_csp_collector_receiver::S_csp_cr_out_solver &cr_out_solver,
		const C_csp_solver_sim_info &sim_info);

    virtual void steady_state(const C_csp_weatherreader::S_outputs &weather,
        const C_csp_solver_htf_1state &htf_state_in,
        double field_control,
        C_csp_collector_receiver::S_csp_cr_out_solver &cr_out_solver,
        const C_csp_solver_sim_info &sim_info);

	virtual void estimates(const C_csp_weatherreader::S_outputs &weather,
		const C_csp_solver_htf_1state &htf_state_in,
		C_csp_collector_receiver::S_csp_cr_est_out &est_out,
		const C_csp_solver_sim_info &sim_info);

	virtual void converged();

	virtual void write_output_intervals(double report_time_start,
		const std::vector<double> & v_temp_ts_time_end, double report_time_end);

	virtual double calculate_optical_efficiency(const C_csp_weatherreader::S_outputs &weather, const C_csp_solver_sim_info &sim);

	virtual double calculate_thermal_efficiency_approx(const C_csp_weatherreader::S_outputs &weather, double q_incident /*MW*/);

	virtual double get_collector_area();

	// ------------------------------------------ supplemental methods -----------------------------------------------------------
	class E_piping_config
	{
		public:
			enum 
			{
				FIELD = 1,
				LOOP
			};
	};
	class E_loop_energy_balance_exit
	{
		public:
			enum 
			{
				SOLVED,
				NaN
			};
	};

	// Input weather, and inlet HTF conditions
	// Calculates energy balances for headers (if applicable) and receivers
	// This method uses TCS convention where the End Of Timestep HTF temperature
	//    is passed to the next receiver
	int loop_energy_balance_T_t_end(const C_csp_weatherreader::S_outputs &weather, 
		double T_htf_cold_in /*K*/, double m_dot_htf_loop /*kg/s*/,
		const C_csp_solver_sim_info &sim_info);

	// This method is designed to pass the timestep integrated HTF temperature to successive energy balance nodes
	int loop_energy_balance_T_t_int(const C_csp_weatherreader::S_outputs &weather,
		double T_htf_cold_in /*K*/, double m_dot_htf_loop /*kg/s*/,
		const C_csp_solver_sim_info &sim_info);

	void loop_optical_eta(const C_csp_weatherreader::S_outputs &weather,
		const C_csp_solver_sim_info &sim_info);

	void loop_optical_eta_off();

	void loop_optical_wind_stow();

	void update_last_temps();

	void reset_last_temps();

	void call(const C_csp_weatherreader::S_outputs &weather,
		const C_csp_solver_htf_1state &htf_state_in,
		const C_csp_collector_receiver::S_csp_cr_inputs &inputs,
		C_csp_collector_receiver::S_csp_cr_out_solver &cr_out_solver,
		//C_csp_collector_receiver::S_csp_cr_out_report &cr_out_report,
		const C_csp_solver_sim_info &sim_info);

	class C_mono_eq_T_htf_loop_out : public C_monotonic_equation
	{
	private:
		C_csp_trough_collector_receiver *mpc_trough;
		C_csp_weatherreader::S_outputs ms_weather;
		double m_T_cold_in;				//[K]
		C_csp_solver_sim_info ms_sim_info;

	public:
		C_mono_eq_T_htf_loop_out(C_csp_trough_collector_receiver *pc_trough, const C_csp_weatherreader::S_outputs &weather,
			double T_htf_cold_in /*K*/, const C_csp_solver_sim_info &sim_info)
		{
			mpc_trough = pc_trough;
			ms_weather = weather;
			m_T_cold_in = T_htf_cold_in;	//[K]
			ms_sim_info = sim_info;
		}
	
		virtual int operator()(double m_dot_htf_loop /*kg/s*/, double *T_htf_loop_out /*K*/);
	};

	void apply_control_defocus(double defocus /*-*/);
	void apply_component_defocus(double defocus /*-*/);

	class C_mono_eq_defocus : public C_monotonic_equation
	{	// The solver chooses a defocus and sends it to the operator. The operator 
		//    calculates a new m_q_SCA and then solves the loop_energy_balance *at max HTF mass flow rate* 
		//    and returns T_htf_SCA_out. The solver finds the defocus resulting in the target HTF outlet temp
	private:
		C_csp_trough_collector_receiver *mpc_trough;
		C_csp_weatherreader::S_outputs ms_weather;
		double m_T_cold_in;				//[K]
		double m_m_dot_loop;			//[kg/s]
		C_csp_solver_sim_info ms_sim_info;

	public:
		C_mono_eq_defocus(C_csp_trough_collector_receiver *pc_trough, const C_csp_weatherreader::S_outputs &weather,
			double T_htf_cold_in /*K*/, double m_dot_loop /*kg/s*/, const C_csp_solver_sim_info &sim_info)
		{
			mpc_trough = pc_trough;
			ms_weather = weather;
			m_T_cold_in = T_htf_cold_in;	//[K]
			m_m_dot_loop = m_dot_loop;		//[kg/s]
			ms_sim_info = sim_info;
		}
	
		virtual int operator()(double defocus /*-*/, double *T_htf_loop_out /*K*/);
	};

	class C_mono_eq_freeze_prot_E_bal : public C_monotonic_equation
	{	// The solver chooses a cold inlet temperature and sends it to the operator. The operator
		//		call the loop energy balance at the recirculation mass flow rate
		//		and returns the total field heat loss. The solver finds the T_cold_in such that E_fp_htf = E_losses
	private:
		C_csp_trough_collector_receiver *mpc_trough;
		C_csp_weatherreader::S_outputs ms_weather;
		double m_m_dot_loop;					//[kg/s]
		C_csp_solver_sim_info ms_sim_info;

	public:
		
		double m_Q_htf_fp;	//[MJ]
				
		C_mono_eq_freeze_prot_E_bal(C_csp_trough_collector_receiver *pc_trough, const C_csp_weatherreader::S_outputs &weather,
						double m_dot_loop /*kg/s*/, const C_csp_solver_sim_info &sim_info)
		{
			mpc_trough = pc_trough;
			ms_weather = weather;
			m_m_dot_loop = m_dot_loop;	//[kg/s]
			ms_sim_info = sim_info;

			m_Q_htf_fp = std::numeric_limits<double>::quiet_NaN();
		}
	
		virtual int operator()(double T_htf_cold_in /*K*/, double *E_loss_balance /*-*/);
	};

	//int loop_energy_balance_T_t_int(const C_csp_weatherreader::S_outputs &weather,
	//	double T_htf_cold_in /*K*/, double m_dot_htf_loop /*kg/s*/,
	//	const C_csp_solver_sim_info &sim_info);

	void EvacReceiver(double T_1_in, double m_dot, double T_amb, double m_T_sky, double v_6, double P_6, double m_q_i,
		int hn /*HCE number [0..3] */, int hv /* HCE variant [0..3] */, int ct /*Collector type*/, int sca_num, bool single_point, int ncall, double time,
		//outputs
		double &q_heatloss, double &q_12conv, double &q_34tot, double &c_1ave, double &rho_1ave);
	double fT_2(double q_12conv, double T_1, double T_2g, double m_v_1, int hn, int hv);
	void FQ_34CONV(double T_3, double T_4, double P_6, double v_6, double T_6, int hn, int hv, double &q_34conv, double &h_34);
	void FQ_56CONV(double T_5, double T_6, double P_6, double v_6, int hn, int hv, double &q_56conv, double &h_6);
	double FQ_COND_BRACKET(double T_3, double T_6, double P_6, double v_6, int hn, int hv);
	void FQ_34RAD(double T_3, double T_4, double T_7, double epsilon_3_v, int hn, int hv, double &q_34rad, double &h_34);
	//void FQ_56CONV(double T_5, double T_6, double P_6, double v_6, int hn, int hv, double &q_56conv, double &h_6);
	//double FQ_COND_BRACKET(double T_3, double T_6, double P_6, double v_6, int hn, int hv);
	double FK_23(double T_2, double T_3, int hn, int hv);
	double PressureDrop(double m_dot, double T, double P, double D, double m_Rough, double L_pipe,
		double Nexp, double Ncon, double Nels, double Nelm, double Nell, double Ngav, double Nglv,
		double Nchv, double Nlw, double Nlcv, double Nbja);
	double FricFactor(double m_Rough, double Reynold);
	double Pump_SGS(double rho, double m_dotsf, double sm);
    void rnr_and_hdr_design(unsigned nhsec, int nfsec, unsigned nrunsec, double rho_cold, double rho_hot, double V_cold_max, double V_cold_min,
        double V_hot_max, double V_hot_min, int N_max_hdr_diams, double m_dot, std::vector<double> &D_hdr, std::vector<double> &D_runner,
        std::vector<double> &m_dot_rnr, std::vector<double> &m_dot_hdr, std::vector<double> &V_rnr, std::vector<double> &V_hdr,
        std::string *summary = NULL, bool custom_diams = false);
    int size_hdr_lengths(double L_row_sep, int Nhdrsec, int offset_hdr_xpan, int Ncol_loops_per_xpan, double L_hdr_xpan,
        std::vector<double> &L_hdr, std::vector<int> &N_hdr_xpans, bool custom_lengths = false);
    int size_rnr_lengths(int Nfieldsec, double L_rnr_pb, int Nrnrsec, int ColType, double northsouth_field_sep,
        const std::vector<double> &L_SCA, int min_rnr_xpans, const std::vector<double> &L_gap_sca, double Nsca_loop,
        double L_rnr_per_xpan, double L_rnr_xpan, std::vector<double> &L_runner, std::vector<int> &N_rnr_xpans,
        bool custom_lengths = false);
    double m_dot_runner(double m_dot_field, int nfieldsec, int irnr);
    double m_dot_header(double m_dot_field, int nfieldsec, int nLoopsField, int ihdr);
};







#endif
