#ifndef __SCO2_CYCLE_COMPONENTS_
#define __SCO2_CYCLE_COMPONENTS_

#include <vector>
#include <memory>

#include "numeric_solvers.h"
#include "CO2_properties.h"
#include "interpolation_routines.h"

void calculate_turbomachinery_outlet_1(double T_in /*K*/, double P_in /*kPa*/, double P_out /*kPa*/,
	double eta_isen /*-*/, bool is_comp, int & error_code,
	double & enth_in /*kJ/kg*/, double & entr_in /*kJ/kg-K*/, double & dens_in /*kg/m3*/, double & temp_out /*K*/,
	double & enth_out /*kJ/kg*/, double & entr_out /*kJ/kg-K*/, double & dens_out /*kg/m3*/, double & spec_work /*kJ/kg*/);

void calculate_turbomachinery_outlet_1(double T_in /*K*/, double P_in /*kPa*/, double P_out /*kPa*/,
	double eta_isen /*-*/, bool is_comp, int & error_code, double & spec_work /*kJ/kg*/);

void isen_eta_from_poly_eta(double T_in /*K*/, double P_in /*kPa*/, double P_out /*kPa*/, double poly_eta /*-*/, bool is_comp, int & error_code, double & isen_eta);

int calc_turbomachinery_eta_isen(double T_in /*K*/, double P_in /*kPa*/,
	double T_out /*K*/, double P_out /*kPa*/, double & eta_isen);

int Ts_data_over_linear_dP_ds(double P_in /*kPa*/, double s_in /*kJ/kg-K*/, double P_out /*kPa*/, double s_out /*kJ/kg-K*/,
	std::vector<double> & T_data /*C*/, std::vector<double> & s_data /*kJ/kg-K*/, int N_points = 30);

int Ph_data_over_turbomachinery(double T_in /*K*/, double P_in /*kPa*/,
	double T_out /*K*/, double P_out /*kPa*/,
	std::vector<double> & P_data /*MPa*/, std::vector<double> & h_data /*kJ/kg*/, int N_points = 30);

int sco2_cycle_plot_data_TS(int cycle_config,
	const std::vector<double> pres /*kPa*/,
	const std::vector<double> entr /*kJ/kg-K*/,
	std::vector<double> & T_LTR_HP /*C*/,
	std::vector<double> & s_LTR_HP /*kJ/kg-K*/,
	std::vector<double> & T_HTR_HP /*C*/,
	std::vector<double> & s_HTR_HP /*kJ/kg-K*/,
	std::vector<double> & T_PHX	   /*C*/,
	std::vector<double> & s_PHX    /*kJ/kg-K*/,
	std::vector<double> & T_HTR_LP /*C*/,
	std::vector<double> & s_HTR_LP /*kJ/kg-K*/,
	std::vector<double> & T_LTR_LP /*C*/,
	std::vector<double> & s_LTR_LP /*kJ/kg-K*/,
	std::vector<double> & T_main_cooler /*C*/,
	std::vector<double> & s_main_cooler /*kJ/kg-K*/,
	std::vector<double> & T_pre_cooler /*C*/,
	std::vector<double> & s_pre_cooler /*kJ/kg-K*/);

int sco2_cycle_plot_data_PH(int cycle_config,
	const std::vector<double> temp /*K*/,
	const std::vector<double> pres /*kPa*/,
	std::vector<double> & P_t /*MPa*/,
	std::vector<double> & h_t /*kJ/kg*/,
	std::vector<double> & P_mc /*MPa*/,
	std::vector<double> & h_mc /*kJ/kg*/,
	std::vector<double> & P_rc /*MPa*/,
	std::vector<double> & h_rc /*kJ/kg*/,
	std::vector<double> & P_pc /*MPa*/,
	std::vector<double> & h_pc /*kJ/kg*/);

int Ts_arrays_over_constP(double T_cold /*C*/, double T_hot /*C*/, std::vector<double> P_consts /*kPa*/,
	std::vector<std::vector<double>> & T_data /*C*/, std::vector<std::vector<double>> & s_data);

int Ph_arrays_over_constT(double P_low /*MPa*/, double P_high /*MPa*/, std::vector<double> T_consts /*C*/,
	std::vector<std::vector<double>> & P_data /*MPa*/, std::vector<std::vector<double>> & h_data);

int Ts_dome(double T_cold /*C*/, std::vector<double> & T_data /*C*/, std::vector<double> & s_data);

int Ts_full_dome(double T_cold /*C*/, std::vector<double> & T_data /*C*/, std::vector<double> & s_data /*kJ/kg-K*/,
	std::vector<double> & P_data /*MPa*/, std::vector<double> & h_data /*kJ/kg*/);

int Ph_dome(double P_low /*MPa*/, std::vector<double> & P_data /*MPa*/, std::vector<double> & h_data);

class C_MEQ_CO2_props_at_2phase_P : public C_monotonic_equation
{
private:


public:
	C_MEQ_CO2_props_at_2phase_P(){}

	CO2_state mc_co2_props;

	virtual int operator()(double T_co2 /*K*/, double *P_calc /*kPa*/);
};

// Heat Exchanger Class
class C_HeatExchanger
{
public:
	struct S_design_parameters
	{
		int m_N_sub;							//[-] Number of sub-heat exchangers used in the model
		std::vector<double> m_m_dot_design;		//[kg/s] Design-point mass flow rates of the two streams
		std::vector<double> m_DP_design;		//[kPa] Design-point pressure drops across the heat exchanger
		double m_UA_design;						//[kW/K] Design-point conductance
		double m_Q_dot_design;					//[kW] Design-point heat transfer
		double m_min_DT_design;					//[K] Minimum temperature difference in heat exchanger
		double m_eff_design;					//[-] Effectiveness at design

		S_design_parameters()
		{
			m_N_sub = -1;

			m_m_dot_design.resize(2);
			std::fill(m_m_dot_design.begin(), m_m_dot_design.end(), std::numeric_limits<double>::quiet_NaN());
			m_DP_design.resize(2);
			std::fill(m_DP_design.begin(), m_DP_design.end(), std::numeric_limits<double>::quiet_NaN());

			m_UA_design = m_Q_dot_design = m_min_DT_design = m_eff_design = std::numeric_limits<double>::quiet_NaN();
		}
	};

private:
	S_design_parameters ms_des_par;

public:
	~C_HeatExchanger(){};

	C_HeatExchanger(){};

	void initialize(const S_design_parameters & des_par_in);

	// Performance Methods:
	// *Some check to ensure member structures are initialized?*
	void hxr_pressure_drops(const std::vector<double> & m_dots, std::vector<double> & hxr_deltaP);

	void hxr_conductance(const std::vector<double> & m_dots, double & hxr_UA);

};

class C_turbine
{
public:

	double m_r_W_dot_scale;		//[-] W_dot_cycle / W_dot_comp_basis (10 MWe)

	int m_cost_model;		//[-]

	enum
	{
		// Techno-Economic Comparison of Solar-Driven SCO2 Brayton Cycles Using
		// Component Cost Models Baselined with Vendor Data and Estimates
		// ASME ES 2017
		E_CARLSON_17
	};

	struct S_design_parameters
	{
		double m_N_design;					//[rpm] turbine shaft speed
		double m_N_comp_design_if_linked;	//[rpm] compressor shaft speed
		// Turbine inlet state
		double m_P_in;						//[kPa]
		double m_T_in;						//[K]
		double m_D_in;						//[kg/m^3]
		double m_h_in;						//[kJ/kg]
		double m_s_in;						//[kJ/kg-K]
		// Turbine outlet state
		double m_P_out;						//[kPa]
		double m_h_out;						//[kJ/kg]
		// Mass flow rate
		double m_m_dot;						//[kg/s] (cycle, not basis)

		S_design_parameters()
		{
			m_N_design = m_N_comp_design_if_linked =
				m_P_in = m_T_in = m_D_in = m_h_in = m_s_in = m_P_out = m_h_out =
				m_m_dot = std::numeric_limits<double>::quiet_NaN();
		}
	};

	struct S_design_solved
	{
		double m_nu_design;					//[-] ratio of tip speed to spouting velocity
		double m_D_rotor;					//[m] Turbine diameter
		double m_A_nozzle;					//[m^2] Effective nozzle area
		double m_w_tip_ratio;				//[-] ratio of tip speed to local speed of sound
		double m_eta;						//[-]
		double m_N_design;					//[rpm] shaft speed

		double m_delta_h_isen;		//[kJ/kg] Isentropic specific work
		double m_rho_in;			//[kg/m3] Turbine inlet density

		double m_W_dot;				//[kWe] Turbine power

		double m_cost;				//[M$]

		S_design_solved()
		{
			m_nu_design = m_D_rotor = m_A_nozzle = m_w_tip_ratio =
				m_eta = m_N_design =
				m_delta_h_isen = m_rho_in =
				m_W_dot = m_cost = std::numeric_limits<double>::quiet_NaN();
		}
	};

	struct S_od_solved
	{
		double m_nu;						//[-] ratio of tip speed to spouting velocity
		double m_eta;						//[-] turbine efficiency
		double m_w_tip_ratio;				//[-] ratio of the tip speed to the local (turbine inlet) speed of sound
		double m_N;							//[rpm] off-design turbine shaft speed
        double m_m_dot;                     //[kg/s] mass flow rate

		double m_delta_h_isen;		//[kJ/kg] Isentropic specific work
		double m_rho_in;			//[kg/m3] Turbine inlet density

		double m_W_dot_out;			//[kW] Turbine power output, expected to be positive (cycle, not basis)

		S_od_solved()
		{
			m_nu = m_eta = m_w_tip_ratio = m_N = m_m_dot =
				m_W_dot_out = std::numeric_limits<double>::quiet_NaN();
		}
	};

private:
	S_design_parameters ms_des_par;
	S_design_solved ms_des_solved;
	S_od_solved ms_od_solved;

public:
	~C_turbine(){};

	C_turbine()
	{
		m_r_W_dot_scale = 1.0;

		m_cost_model = E_CARLSON_17;
	};

	static const double m_nu_design;

	const S_design_solved * get_design_solved()
	{
		return &ms_des_solved;
	}

	const S_od_solved * get_od_solved()
	{
		return &ms_od_solved;
	}

	double calculate_cost(double T_in /*K*/, double P_in /*kPa*/, double m_dot /*kg/s*/,
		double T_out /*K*/, double P_out /*kPa*/, double W_dot /*kWe*/);

	void turbine_sizing(const S_design_parameters & des_par_in, int & error_code);

	void off_design_turbine(double T_in, double P_in, double P_out, double N, int & error_code, double & m_dot, double & T_out);

	void od_turbine_at_N_des(double T_in, double P_in, double P_out, int & error_code, double & m_dot, double & T_out);
};

class C_comp__psi_eta_vs_phi
{
public:

    enum E_comp_models
    {
        E_snl_radial_via_Dyreby,
    };

    struct S_des_solved
    {
        // Compressor inlet conditions
        double m_T_in;			//[K]
        double m_P_in;			//[kPa]
        double m_D_in;			//[kg/m^3]
        double m_h_in;			//[kJ/kg]
        double m_s_in;			//[kJ/kg-K]
        // Compressor outlet conditions
        double m_T_out;			//[K]
        double m_P_out;			//[kPa]
        double m_h_out;			//[kJ/kg]
        double m_D_out;			//[kg/m^3]
        // Mass flow
        double m_m_dot;			//[kg/s]

        // Geometry
        double m_D_rotor;		//[m]
        double m_N_design;		//[rpm]
        double m_tip_ratio;		//[-]
        double m_eta_design;	//[-]

        double m_phi_des;		//[-]
        double m_phi_surge;		//[-]
        double m_phi_max;		//[-]

        double m_psi_des;       //[-] Ideal head coefficient
        double m_psi_max_at_N_des;  //[-] Max ideal head coefficient at design shaft speed

        S_des_solved()
        {
            m_T_in = m_P_in = m_D_in = m_h_in = m_s_in =
                m_T_out = m_P_out = m_h_out = m_D_out =
                m_m_dot = m_D_rotor = m_N_design = m_tip_ratio = m_eta_design =
                m_phi_surge = m_phi_des = m_phi_max =
                m_psi_des = m_psi_max_at_N_des = std::numeric_limits<double>::quiet_NaN();
        }
    };

    struct S_od_solved
    {
        double m_P_in;			//[kPa] Inlet pressure
        double m_h_in;			//[kJ/kg] Inlet enthalpy
        double m_T_in;			//[K] Inlet temperature
        double m_s_in;			//[kJ/kg-K] Inlet entropy

        double m_P_out;			//[kPa] Outlet pressure
        double m_h_out;			//[kPa] Outlet enthalpy
        double m_T_out;			//[K] Outlet temperature
        double m_s_out;			//[kJ/kg-K] Outlet entropy


        bool m_surge;			//[-]
        double m_eta;			//[-]
        double m_phi;			//[-]
        double m_psi;           //[-]
        double m_w_tip_ratio;	//[-]

        double m_N;			//[rpm]

        double m_W_dot_in;		//[KWe] Power required by compressor, positive value expected
        double m_surge_safety;	//[-] Flow coefficient / min flow coefficient

        S_od_solved()
        {
            m_P_in = m_h_in = m_T_in = m_s_in =
                m_P_out = m_h_out = m_T_out = m_s_out = std::numeric_limits<double>::quiet_NaN();
            m_surge = false;
            m_eta = m_phi = m_psi = m_w_tip_ratio = m_N =
                m_W_dot_in = m_surge_safety = std::numeric_limits<double>::quiet_NaN();
        }
    };

    C_comp__psi_eta_vs_phi(){}

    virtual ~C_comp__psi_eta_vs_phi(){}

    S_des_solved ms_des_solved;
    S_od_solved ms_od_solved;

    int design_given_shaft_speed(double T_in /*K*/, double P_in /*kPa*/, double m_dot /*kg/s*/,
        double N_rpm /*rpm*/, double eta_isen /*-*/, double & P_out /*kPa*/, double & T_out /*K*/, double & tip_ratio /*-*/);

    int design_given_performance(double T_in /*K*/, double P_in /*kPa*/, double m_dot /*kg/s*/,
        double T_out /*K*/, double P_out /*K*/);

    int off_design_given_N(double T_in /*K*/, double P_in /*kPa*/, double m_dot /*kg/s*/, double N_rpm /*rpm*/,
        double & T_out /*K*/, double & P_out /*kPa*/);

    int calc_N_from_phi(double T_in /*K*/, double P_in /*kPa*/,
                        double m_dot /*kg/s*/, double phi_in /*-*/, double & N_rpm /*rpm*/);

    int calc_m_dot__phi_des(double T_in /*K*/, double P_in /*kPa*/, double N_rpm /*rpm*/, double & m_dot /*kg/s*/);

    static std::unique_ptr<C_comp__psi_eta_vs_phi> construct_derived_C_comp__psi_eta_vs_phi(int comp_model_code);

    virtual void set_design_solution(double phi /*-*/, double T_comp_in_des /*K*/, double P_comp_in /*kPa*/) = 0;

    virtual void report_phi_psi_eta_vectors(std::vector<double> & phi, 
        std::vector<double> & psi, std::vector<double> & eta, double & eta_norm_design) = 0;
    
    virtual double calc_phi_min(double T_comp_in /*K*/, double P_comp_in /*kPa*/) = 0;

    virtual double calc_phi_design(double T_comp_in /*K*/, double P_comp_in /*kPa*/) = 0;

    virtual double calc_phi_max(double T_comp_in /*K*/, double P_comp_in /*kPa*/) = 0;

    virtual double calc_psi_isen_design(double T_comp_in /*K*/, double P_comp_in /*kPa*/) = 0;    //[-]

    virtual double calc_psi_isen(double phi /*-*/, double N_des_over_N_od /*-*/, double T_comp_in /*K*/, double P_comp_in /*kPa*/) = 0;

    virtual double calc_eta_OD_normalized(double phi /*-*/, double N_des_over_N_od /*-*/, double T_comp_in /*K*/, double P_comp_in /*kPa*/) = 0;
};

class C_comp__snl_radial_via_Dyreby : public C_comp__psi_eta_vs_phi
{
public:

    double m_phi_design;            //[-]
    double m_phi_min;               //[-]
    double m_phi_max;               //[-]

    C_comp__snl_radial_via_Dyreby()
    {
        m_phi_design = 0.02971;		//[-] Design-point flow coef. for Sandia compressor (corresponds to max eta)
        m_phi_min = 0.0225;			//[-] Approximate surge limit for SNL compressor
        m_phi_max = 0.05;			//[-] Approximate x-intercept for SNL compressor
    }

    double adjust_phi_for_N(double phi /*-*/, double N_des_over_N_od /*-*/);

    virtual void set_design_solution(double phi /*-*/, double T_comp_in_des /*K*/, double P_comp_in /*kPa*/);

    virtual void report_phi_psi_eta_vectors(std::vector<double> & phi, 
        std::vector<double> & psi, std::vector<double> & eta, double & eta_norm_design);

    virtual double calc_phi_min(double T_comp_in /*K*/, double P_comp_in /*kPa*/);

    virtual double calc_phi_design(double T_comp_in /*K*/, double P_comp_in /*kPa*/);

    virtual double calc_phi_max(double T_comp_in /*K*/, double P_comp_in /*kPa*/);

    virtual double calc_psi_isen_design(double T_comp_in /*K*/, double P_comp_in /*kPa*/);

    virtual double calc_psi_isen(double phi /*-*/, double N_des_over_N_od /*-*/, double T_comp_in /*K*/, double P_comp_in /*kPa*/);

    virtual double calc_eta_OD_normalized(double phi /*-*/, double N_des_over_N_od /*-*/, double T_comp_in /*K*/, double P_comp_in /*kPa*/);
};

class C_comp__compA__PT_map_template : public C_comp__psi_eta_vs_phi
{
public:

    Linear_Interp mc_data_at_PT;

    double m_P_mc_in;       //[kPa]
    double m_T_mc_in;       //[K]

    double m_phi_design;    //[-]
    double m_phi_min;       //[-]
    double m_phi_max;       //[-]  
    double m_eta_isen_norm; //[-]

    C_comp__compA__PT_map_template(){}

    virtual void set_design_solution(double phi /*-*/, double T_comp_in_des /*K*/, double P_comp_in /*kPa*/);

    virtual void report_phi_psi_eta_vectors(std::vector<double> & phi,
        std::vector<double> & psi, std::vector<double> & eta, double & eta_norm_design);
    
    virtual double calc_phi_min(double T_comp_in /*K*/, double P_comp_in /*kPa*/);

    virtual double calc_phi_design(double T_comp_in /*K*/, double P_comp_in /*kPa*/);

    virtual double calc_phi_max(double T_comp_in /*K*/, double P_comp_in /*kPa*/);

    virtual double calc_psi_isen_design(double T_comp_in /*K*/, double P_comp_in /*kPa*/);

    virtual double calc_psi_isen(double phi /*-*/, double N_des_over_N_od /*-*/, double T_comp_in /*K*/, double P_comp_in /*kPa*/);

    virtual double calc_eta_OD_normalized(double phi /*-*/, double N_des_over_N_od /*-*/, double T_comp_in /*K*/, double P_comp_in /*kPa*/);    
};

class C_comp_multi_stage
{
public:

    std::vector<std::unique_ptr<C_comp__psi_eta_vs_phi>> mv_c_stages;

	double m_r_W_dot_scale;		//[-] W_dot_cycle / W_dot_comp_basis (10 MWe)

	int m_cost_model;		//[-]
    int m_compressor_model; //[-]

	enum
	{
		// Techno-Economic Comparison of Solar-Driven SCO2 Brayton Cycles Using
		// Component Cost Models Baselined with Vendor Data and Estimates
		// ASME ES 2017
		E_CARLSON_17
	};

	struct S_des_solved
	{
		// Compressor inlet conditions
		double m_T_in;			//[K]
		double m_P_in;			//[kPa]
		double m_D_in;			//[kg/m^3]
		double m_h_in;			//[kJ/kg]
		double m_s_in;			//[kJ/kg-K]
		// Compressor outlet conditions
		double m_T_out;			//[K]
		double m_P_out;			//[kPa]
		double m_h_out;			//[kJ/kg]
		double m_D_out;			//[kg/m^3]

		double m_isen_spec_work;	//[kJ/kg]

		double m_m_dot;			//[kg/s] (cycle not basis)
		double m_W_dot;			//[kWe] power required by compressor

		// Cost
		double m_cost;			//[M$]

		// Stage Metrics
		int m_n_stages;			//[-] Number of stages
		double m_tip_ratio_max;	//[-] Max tip ratio over all stages
			// Metrics shared by all stages
		double m_N_design;		//[rpm] Shaft speed
		double m_phi_des;		//[-] Design flow coefficient
        double m_phi_surge;		//[-] Flow coefficient at surge

        double m_psi_des;       //[-] Ideal head coefficient
        double m_psi_max_at_N_des;  //[-] Max ideal head coefficient at design shaft speed
			// Metrics that vary by stage
		std::vector<double> mv_D;	//[m] Diameter
		std::vector<double> mv_tip_speed_ratio;	//[-]
		std::vector<double> mv_eta_stages;	//[-]

		S_des_solved()
		{
			m_n_stages = -1;

			m_T_in = m_P_in = m_D_in = m_h_in = m_s_in =
				m_T_out = m_P_out = m_h_out = m_D_out =
				m_isen_spec_work =
				m_m_dot = m_W_dot =
				m_cost =
				m_tip_ratio_max =
				m_N_design = m_phi_des =
                m_psi_des = m_phi_surge = std::numeric_limits<double>::quiet_NaN();
		}
	};

	struct S_od_solved
	{
		double m_P_in;			//[kPa] Inlet pressure
		double m_T_in;			//[K] Inlet temperature

		double m_P_out;			//[kPa] Outlet pressure
		double m_T_out;			//[K] Outlet temperature

		double m_m_dot;			//[kg/s] (cycle not basis)

		double m_isen_spec_work;	//[kJ/kg]

		bool m_surge;			//[-]
		double m_eta;			//[-]
		double m_phi_min;		//[-] Min phi over all stages
		double m_tip_ratio_max;	//[-] Max tip ratio over all stages

		double m_N;				//[rpm]

		double m_W_dot_in;		//[KWe] Power required by compressor, positive value expected
		double m_surge_safety;	//[-] Flow coefficient / min flow coefficient

			// Metrics that vary by stage
		std::vector<double> mv_tip_speed_ratio;	//[-]
		std::vector<double> mv_phi;		//[-]
        std::vector<double> mv_psi;     //[-]
		std::vector<double> mv_eta;		//[-]

		S_od_solved()
		{
			m_P_in = m_T_in =
				m_P_out = m_T_out =
				m_m_dot =
				m_isen_spec_work = std::numeric_limits<double>::quiet_NaN();

			m_surge = false;
			m_eta = m_phi_min = m_tip_ratio_max = m_N =
				m_W_dot_in = m_surge_safety = std::numeric_limits<double>::quiet_NaN();
		}
	};

	S_des_solved ms_des_solved;
	S_od_solved ms_od_solved;

	~C_comp_multi_stage(){};

	C_comp_multi_stage()
	{
		m_r_W_dot_scale = 1.0;
		m_cost_model = E_CARLSON_17;

        m_compressor_model = -1;
	};

	const S_des_solved * get_design_solved()
	{
		return &ms_des_solved;
	}

	const S_od_solved * get_od_solved()
	{
		return &ms_od_solved;
	}

	class C_MEQ_eta_isen__h_out : public C_monotonic_equation
	{
	private:
		C_comp_multi_stage *mpc_multi_stage;
		double m_T_in;	//[K]
		double m_P_in;	//[kPa]
		double m_P_out;	//[kPa]
		double m_m_dot_basis;	//[kg/s]
        double m_tol_in;   //[-] Convergence tolerance

	public:
		C_MEQ_eta_isen__h_out(C_comp_multi_stage *pc_multi_stage,
			double T_in /*K*/, double P_in /*kPa*/, double P_out /*kPa*/, double m_dot_basis /*kg/s*/,
            double tol /*-*/)
		{
			mpc_multi_stage = pc_multi_stage;
			m_T_in = T_in;
			m_P_in = P_in;
			m_P_out = P_out;
			m_m_dot_basis = m_dot_basis;
            m_tol_in = tol;
		}

		virtual int operator()(double eta_isen /*-*/, double *h_comp_out /*kJ/kg*/);
	};

	class C_MEQ_N_rpm__P_out : public C_monotonic_equation
	{
	private:
		C_comp_multi_stage *mpc_multi_stage;
		double m_T_in;	//[K]
		double m_P_in;	//[kPa]
		double m_m_dot_basis;	//[kg/s]
		double m_eta_isen;	//[-]

	public:
		C_MEQ_N_rpm__P_out(C_comp_multi_stage *pc_multi_stage,
			double T_in /*K*/, double P_in /*kPa*/, double m_dot_basis /*kg/s*/, double eta_isen /*-*/)
		{
			mpc_multi_stage = pc_multi_stage;
			m_T_in = T_in;	//[K]
			m_P_in = P_in;	//[kPa]
			m_m_dot_basis = m_dot_basis;	//[kg/s]
			m_eta_isen = eta_isen;	//[-]
		}

		virtual int operator()(double N_rpm /*rpm*/, double *P_comp_out /*kPa*/);
	};

	class C_MEQ_phi_od__P_out : public C_monotonic_equation
	{
	private:
		C_comp_multi_stage *mpc_multi_stage;
		double m_T_in;	//[K]
		double m_P_in;	//[kPa]
		double m_m_dot_cycle;	//[kg/s]

	public:
		C_MEQ_phi_od__P_out(C_comp_multi_stage *pc_multi_stage,
			double T_in /*K*/, double P_in /*kPa*/, double m_dot_cycle /*kg/s*/)
		{
			mpc_multi_stage = pc_multi_stage;
			m_T_in = T_in;		//[K]
			m_P_in = P_in;		//[kPa]
			m_m_dot_cycle = m_dot_cycle;	//[kg/s]
		}

		virtual int operator()(double phi_od /*-*/, double *P_comp_out /*kPa*/);
	};

	double calculate_cost(double T_in /*K*/, double P_in /*kPa*/, double m_dot /*kg/s*/,
		double T_out /*K*/, double P_out /*kPa*/, double W_dot /*kWe*/);

	int design_given_outlet_state(int comp_model_code, double T_in /*K*/, double P_in /*kPa*/, double m_dot /*kg/s*/,
		double T_out /*K*/, double P_out /*K*/, double tol /*-*/);

	void off_design_given_N(double T_in /*K*/, double P_in /*kPa*/, double m_dot_cycle /*kg/s*/, double N_rpm /*rpm*/,
		int & error_code, double & T_out /*K*/, double & P_out /*kPa*/);

	void off_design_at_N_des(double T_in /*K*/, double P_in /*kPa*/, double m_dot_cycle /*kg/s*/,
		int & error_code, double & T_out /*K*/, double & P_out /*kPa*/);

	void off_design_given_P_out(double T_in /*K*/, double P_in /*kPa*/, double m_dot_cycle /*kg/s*/,
		double P_out /*kPa*/, double tol /*-*/, int & error_code, double & T_out /*K*/);

	int calc_m_dot__N_des__phi_des_first_stage(double T_in /*K*/, double P_in /*kPa*/, double & m_dot_cycle /*kg/s*/);
};


#endif
