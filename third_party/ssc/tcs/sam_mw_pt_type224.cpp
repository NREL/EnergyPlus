#define _TCSTYPEINTERFACE_
#include "tcstype.h"
#include "htf_props.h"
//#include "sam_csp_util.h"
//#include "recore/lib_powerblock.h"
#include "powerblock.h"

using namespace std;

enum{
	P_P_REF,
	P_ETA_REF,
	P_T_HTF_HOT_REF,
	P_T_HTF_COLD_REF,
	P_DT_CW_REF,
	P_T_AMB_DES,
	P_HTF,
	P_FIELD_FL_PROPS,
	P_Q_SBY_FRAC,
	P_P_BOIL,
	P_CT,
	P_STARTUP_TIME,
	P_STARTUP_FRAC,
	P_TECH_TYPE,
	P_T_APPROACH,
	P_T_ITD_DES,
	P_P_COND_RATIO,
	P_PB_BD_FRAC,
	P_PB_INPUT_FILE,
	P_P_COND_MIN,
	P_N_PL_INC,
	P_F_WC,

	I_MODE,
	I_T_HTF_HOT,
	I_M_DOT_HTF,
	I_T_WB,
	I_DEMAND_VAR,
	I_STANDBY_CONTROL,
	I_T_DB,
	I_P_AMB,
	I_TOU,
	I_RH,

	O_P_CYCLE,
	O_ETA,
	O_T_HTF_COLD,
	O_M_DOT_MAKEUP,
	O_M_DOT_DEMAND,
	O_M_DOT_HTF_OUT,
	O_M_DOT_HTF_REF,
	O_W_COOL_PAR,
	O_P_REF_OUT,
	O_F_BAYS,
	O_P_COND,

	//Include N_max
	N_MAX
};

tcsvarinfo sam_mw_pt_type224_variables[] = {
	{ TCS_PARAM,          TCS_NUMBER,             P_P_REF,                  "P_ref",                                     "Reference output electric power at design condition",           "MW",             "",             "",          "111" },
	{ TCS_PARAM,          TCS_NUMBER,           P_ETA_REF,                "eta_ref",                                     "Reference conversion efficiency at design condition",         "none",             "",             "",       "0.3774" },
	{ TCS_PARAM,          TCS_NUMBER,     P_T_HTF_HOT_REF,          "T_htf_hot_ref",                                               "Reference HTF inlet temperature at design",            "C",             "",             "",          "391" },
	{ TCS_PARAM,          TCS_NUMBER,    P_T_HTF_COLD_REF,         "T_htf_cold_ref",                                              "Reference HTF outlet temperature at design",            "C",             "",             "",          "293" },
	{ TCS_PARAM,          TCS_NUMBER,         P_DT_CW_REF,              "dT_cw_ref",                                   "Reference condenser cooling water inlet/outlet T diff",            "C",             "",             "",           "10" },
	{ TCS_PARAM,          TCS_NUMBER,         P_T_AMB_DES,              "T_amb_des",                                           "Reference ambient temperature at design point",            "C",             "",             "",           "20" },
	{ TCS_PARAM,          TCS_NUMBER,               P_HTF,                    "HTF",                                             "Integer flag identifying HTF in power block",         "none",             "",             "",           "21" },
	{ TCS_PARAM,          TCS_NUMBER,    P_FIELD_FL_PROPS,         "field_fl_props",                                                  "User defined field fluid property data",            "-",             "7 columns (T,Cp,dens,visc,kvisc,cond,h), at least 3 rows",        "",        ""},
	{ TCS_PARAM,          TCS_NUMBER,        P_Q_SBY_FRAC,             "q_sby_frac",                                     "Fraction of thermal power required for standby mode",         "none",             "",             "",          "0.2" },
	{ TCS_PARAM,          TCS_NUMBER,            P_P_BOIL,                 "P_boil",                                                               "Boiler operating pressure",          "bar",             "",             "",          "100" },
	{ TCS_PARAM,          TCS_NUMBER,                P_CT,                     "CT",                                        "Flag for using dry cooling or wet cooling system",         "none",             "",             "",            "1" },
	{ TCS_PARAM,          TCS_NUMBER,      P_STARTUP_TIME,           "startup_time",                                                     "Time needed for power block startup",           "hr",             "",             "",          "0.5" },
	{ TCS_PARAM,          TCS_NUMBER,      P_STARTUP_FRAC,           "startup_frac",                                     "Fraction of design thermal power needed for startup",         "none",             "",             "",          "0.2" },
	{ TCS_PARAM,          TCS_NUMBER,         P_TECH_TYPE,              "tech_type",                       "Flag indicating which coef. set to use. (1=tower,2=trough,3=user)",         "none",             "",             "",            "2" },
	{ TCS_PARAM,          TCS_NUMBER,        P_T_APPROACH,             "T_approach",                                                      "Cooling tower approach temperature",            "C",             "",             "",            "5" },
	{ TCS_PARAM,          TCS_NUMBER,         P_T_ITD_DES,              "T_ITD_des",                                                            "ITD at design for dry system",            "C",             "",             "",           "16" },
	{ TCS_PARAM,          TCS_NUMBER,      P_P_COND_RATIO,           "P_cond_ratio",                                                                "Condenser pressure ratio",         "none",             "",             "",       "1.0028" },
	{ TCS_PARAM,          TCS_NUMBER,        P_PB_BD_FRAC,             "pb_bd_frac",                                                    "Power block blowdown steam fraction ",         "none",             "",             "",         "0.02" },
	{ TCS_PARAM,          TCS_STRING,     P_PB_INPUT_FILE,          "pb_input_file",                                                       "Power block coefficient file name",         "none",             "",             "","pb_coef_file.in" },
	{ TCS_PARAM,          TCS_NUMBER,        P_P_COND_MIN,             "P_cond_min",                                                              "Minimum condenser pressure",         "inHg",             "",             "",         "1.25" },
	{ TCS_PARAM,          TCS_NUMBER,          P_N_PL_INC,               "n_pl_inc",                            "Number of part-load increments for the heat rejection system",         "none",             "",             "",            "2" },
	{ TCS_PARAM,           TCS_ARRAY,              P_F_WC,                   "F_wc",                                   "Fraction indicating wet cooling use for hybrid system",         "none",             "",             "","0,0,0,0,0,0,0,0,0" },

	{ TCS_INPUT,          TCS_NUMBER,              I_MODE,                   "mode",                                          "Cycle part load control, from plant controller",         "none",             "",             "",             "" },
	{ TCS_INPUT,          TCS_NUMBER,         I_T_HTF_HOT,              "T_htf_hot",                                            "Hot HTF inlet temperature, from storage tank",            "C",             "",             "",             "" },
	{ TCS_INPUT,          TCS_NUMBER,         I_M_DOT_HTF,              "m_dot_htf",                                                                      "HTF mass flow rate",        "kg/hr",             "",             "",             "" },
	{ TCS_INPUT,          TCS_NUMBER,              I_T_WB,                   "T_wb",                                                            "Ambient wet bulb temperature",            "C",             "",             "",             "" },
	{ TCS_INPUT,          TCS_NUMBER,        I_DEMAND_VAR,             "demand_var",                                              "Control signal indicating operational mode",         "none",             "",             "",             "" },
	{ TCS_INPUT,          TCS_NUMBER,   I_STANDBY_CONTROL,        "standby_control",                                                  "Control signal indicating standby mode",         "none",             "",             "",             "" },
	{ TCS_INPUT,          TCS_NUMBER,              I_T_DB,                   "T_db",                                                            "Ambient dry bulb temperature",            "C",             "",             "",             "" },
	{ TCS_INPUT,          TCS_NUMBER,             I_P_AMB,                  "P_amb",                                                                        "Ambient pressure",          "mbar",             "",             "",             "" },
	{ TCS_INPUT,          TCS_NUMBER,               I_TOU,                    "TOU",                                                              "Current Time-of-use period",         "none",             "",             "",             "" },
	{ TCS_INPUT,          TCS_NUMBER,                I_RH,                     "rh",                                                    "Relative humidity of the ambient air",         "none",             "",             "",             "" },

	{ TCS_OUTPUT,          TCS_NUMBER,           O_P_CYCLE,                "P_cycle",                                                                      "Cycle power output",          "MWe",             "",             "",             "" },
	{ TCS_OUTPUT,          TCS_NUMBER,               O_ETA,                    "eta",                                                                "Cycle thermal efficiency",         "none",             "",             "",             "" },
	{ TCS_OUTPUT,          TCS_NUMBER,        O_T_HTF_COLD,             "T_htf_cold",                                                 "Heat transfer fluid outlet temperature ",            "C",             "",             "",             "" },
	{ TCS_OUTPUT,          TCS_NUMBER,      O_M_DOT_MAKEUP,           "m_dot_makeup",                                                          "Cooling water makeup flow rate",        "kg/hr",             "",             "",             "" },
	{ TCS_OUTPUT,          TCS_NUMBER,      O_M_DOT_DEMAND,           "m_dot_demand",                                               "HTF required flow rate to meet power load",        "kg/hr",             "",             "",             "" },
	{ TCS_OUTPUT,          TCS_NUMBER,     O_M_DOT_HTF_OUT,          "m_dot_htf_out",                                    "Actual HTF flow rate passing through the power cycle",        "kg/hr",             "",             "",             "" },
	{ TCS_OUTPUT,          TCS_NUMBER,     O_M_DOT_HTF_REF,          "m_dot_htf_ref",                                            "Calculated reference HTF flow rate at design",        "kg/hr",             "",             "",             "" },
	{ TCS_OUTPUT,          TCS_NUMBER,        O_W_COOL_PAR,             "W_cool_par",                                                           "Cooling system parasitic load",          "MWe",             "",             "",             "" },
	{ TCS_OUTPUT,          TCS_NUMBER,         O_P_REF_OUT,              "P_ref_out",                                   "Reference power level output at design (mirror param)",          "MWe",             "",             "",             "" },
	{ TCS_OUTPUT,          TCS_NUMBER,            O_F_BAYS,                 "f_bays",                                               "Fraction of operating heat rejection bays",         "none",             "",             "",             "" },
	{ TCS_OUTPUT,          TCS_NUMBER,            O_P_COND,                 "P_cond",                                                                      "Condenser pressure",           "Pa",             "",             "",             "" },
	
	{ TCS_INVALID,    TCS_INVALID,    N_MAX,                0,                    0,                                                        0,                0,        0,        0 }
};

class sam_mw_pt_type224 : public tcstypeinterface
{
private:
	HTFProperties htfProps;
	double pi, Pi;

	double P_ref;		//Reference output electric power at design condition
	double eta_ref;		//Reference conversion efficiency at design condition
	double T_htf_hot_ref;		//Reference HTF inlet temperature at design
	double T_htf_cold_ref;		//Reference HTF outlet temperature at design
	double dT_cw_ref;		//Reference condenser cooling water inlet/outlet T diff
	double T_amb_des;		//Reference ambient temperature at design point
	int HTF;		//Integer flag identifying HTF in power block
	double q_sby_frac;		//Fraction of thermal power required for standby mode
	double P_boil;		//Boiler operating pressure
	int CT;		//Flag for using dry cooling or wet cooling system               
	double startup_time;		//Time needed for power block startup
	double startup_frac;		//Fraction of design thermal power needed for startup
	int tech_type;		//Flag indicating which coef. set to use. (1=tower,2=trough,3=user)
	double T_approach;		//Cooling tower approach temperature
	double T_ITD_des;		//ITD at design for dry system
	double P_cond_ratio;		//Condenser pressure ratio
	double pb_bd_frac;		//Power block blowdown steam fraction 
	string pb_input_file;		//Power block coefficient file name
	double P_cond_min;		//Minimum condenser pressure
	int n_pl_inc;		//Number of part-load increments for the heat rejection system
	double* F_wc;		//Fraction indicating wet cooling use for hybrid system
	int nval_F_wc;

	int mode;		//Cycle part load control, from plant controller
	double T_htf_hot;		//Hot HTF inlet temperature, from storage tank
	double m_dot_htf;		//HTF mass flow rate
	double T_wb;		//Ambient wet bulb temperature
	double demand_var;		//Control signal indicating operational mode
	int standby_control;		//Control signal indicating standby mode
	double T_db;		//Ambient dry bulb temperature
	double P_amb;		//Ambient pressure
	int TOU;		//Current Time-of-use period
	double rh;		//Relative humidity of the ambient air

	double P_cycle;		//Cycle power output
	double eta;		//Cycle thermal efficiency
	double T_htf_cold;		//Heat transfer fluid outlet temperature 
	double m_dot_makeup;		//Cooling water makeup flow rate
	double m_dot_demand;		//HTF required flow rate to meet power load
	double m_dot_htf_out;		//Actual HTF flow rate passing through the power cycle
	double m_dot_htf_ref;		//Calculated reference HTF flow rate at design
	double W_cool_par;		//Cooling system parasitic load
	double P_ref_out;		//Reference power level output at design (mirror param)
	double f_bays;		//Fraction of operating heat rejection bays
	double P_cond;		//Condenser pressure

	//Declare other variables 
	double dt, start_time;
	double startup_remain0, P_cycle0, startup_e_remain0;
	int standby_control0;

	//Structures required for the SSC power block class
	S_Indirect_PB_Parameters params;
	S_Indirect_PB_Inputs inputs;
	S_Indirect_PB_Outputs outputs;
	S_Indirect_PB_Stored stored;
	C_Indirect_PB type224;


public:

	sam_mw_pt_type224(tcscontext *cxt, tcstypeinfo *ti)
		: tcstypeinterface(cxt, ti)
	{
		//Commonly used values, conversions, etc...
		Pi = acos(-1.);
		pi = Pi;
		
		//Set all values to NaN or nonsense value to prevent misuse
		P_ref	= std::numeric_limits<double>::quiet_NaN();
		eta_ref	= std::numeric_limits<double>::quiet_NaN();
		T_htf_hot_ref	= std::numeric_limits<double>::quiet_NaN();
		T_htf_cold_ref	= std::numeric_limits<double>::quiet_NaN();
		dT_cw_ref	= std::numeric_limits<double>::quiet_NaN();
		T_amb_des	= std::numeric_limits<double>::quiet_NaN();
		HTF	= -1;
		q_sby_frac	= std::numeric_limits<double>::quiet_NaN();
		P_boil	= std::numeric_limits<double>::quiet_NaN();
		CT	= -1;
		startup_time	= std::numeric_limits<double>::quiet_NaN();
		startup_frac	= std::numeric_limits<double>::quiet_NaN();
		tech_type	= -1;
		T_approach	= std::numeric_limits<double>::quiet_NaN();
		T_ITD_des	= std::numeric_limits<double>::quiet_NaN();
		P_cond_ratio	= std::numeric_limits<double>::quiet_NaN();
		pb_bd_frac	= std::numeric_limits<double>::quiet_NaN();
		pb_input_file	= "";
		P_cond_min	= std::numeric_limits<double>::quiet_NaN();
		n_pl_inc	= -1;
		F_wc	= NULL;
		nval_F_wc = -1;
		mode	= -1;
		T_htf_hot	= std::numeric_limits<double>::quiet_NaN();
		m_dot_htf	= std::numeric_limits<double>::quiet_NaN();
		T_wb	= std::numeric_limits<double>::quiet_NaN();
		demand_var	= std::numeric_limits<double>::quiet_NaN();
		standby_control	= -1;
		T_db	= std::numeric_limits<double>::quiet_NaN();
		P_amb	= std::numeric_limits<double>::quiet_NaN();
		TOU	= -1;
		rh	= std::numeric_limits<double>::quiet_NaN();
		P_cycle	= std::numeric_limits<double>::quiet_NaN();
		eta	= std::numeric_limits<double>::quiet_NaN();
		T_htf_cold	= std::numeric_limits<double>::quiet_NaN();
		m_dot_makeup	= std::numeric_limits<double>::quiet_NaN();
		m_dot_demand	= std::numeric_limits<double>::quiet_NaN();
		m_dot_htf_out	= std::numeric_limits<double>::quiet_NaN();
		m_dot_htf_ref	= std::numeric_limits<double>::quiet_NaN();
		W_cool_par	= std::numeric_limits<double>::quiet_NaN();
		P_ref_out	= std::numeric_limits<double>::quiet_NaN();
		f_bays	= std::numeric_limits<double>::quiet_NaN();
		P_cond	= std::numeric_limits<double>::quiet_NaN();

	}

	virtual ~sam_mw_pt_type224(){
	}

	virtual int init(){
		/*
		--Initialization call-- 
		
		Do any setup required here.
		Get the values of the inputs and parameters
		*/
		dt = time_step();
		start_time = -1; 

		//Get fluid properties
		HTF = (int) value(P_HTF);
		if(HTF != HTFProperties::User_defined )
		{
			if( !htfProps.SetFluid( HTF ) )
			{
				message(TCS_ERROR, "Field HTF code is not recognized");
				return -1;
			}
		}
		else
		{
			int nrows = 0, ncols = 0;
			double *fl_mat = value(P_FIELD_FL_PROPS, &nrows, &ncols);
			if ( fl_mat != 0 && nrows > 2 && ncols == 7 )
			{
				util::matrix_t<double> mat( nrows, ncols, 0.0 );
				for (int r=0;r<nrows;r++)
					for (int c=0;c<ncols;c++)
						mat.at(r, c) = TCS_MATRIX_INDEX(var(P_FIELD_FL_PROPS ), r, c);

				if ( !htfProps.SetUserDefinedFluid( mat ) )
				{
					message( TCS_ERROR, htfProps.UserFluidErrMessage(), nrows, ncols );
					return -1;
				}
			}
			else
			{
				message(TCS_ERROR, "The user defined HTF table must contain at least 3 rows and exactly 7 columns. The current table contains %d row(s) and %d column(s)", nrows, ncols);
				return -1;
			}
		}

		//Get values for any parameters here
		P_ref = value(P_P_REF);		//Reference output electric power at design condition [MW]
		eta_ref = value(P_ETA_REF);		//Reference conversion efficiency at design condition [none]
		T_htf_hot_ref = value(P_T_HTF_HOT_REF);		//Reference HTF inlet temperature at design [C]
		T_htf_cold_ref = value(P_T_HTF_COLD_REF);		//Reference HTF outlet temperature at design [C]
		dT_cw_ref = value(P_DT_CW_REF);		//Reference condenser cooling water inlet/outlet T diff [C]
		T_amb_des = value(P_T_AMB_DES);		//Reference ambient temperature at design point [C]
		HTF = (int)value(P_HTF);		//Integer flag identifying HTF in power block [none]
		q_sby_frac = value(P_Q_SBY_FRAC);		//Fraction of thermal power required for standby mode [none]
		P_boil = value(P_P_BOIL);		//Boiler operating pressure [bar]
		CT = (int)value(P_CT);		//Flag for using dry cooling or wet cooling system                [none]
		startup_time = value(P_STARTUP_TIME);		//Time needed for power block startup [hr]
		startup_frac = value(P_STARTUP_FRAC);		//Fraction of design thermal power needed for startup [none]
		tech_type = (int)value(P_TECH_TYPE);		//Flag indicating which coef. set to use. (1=tower,2=trough,3=user) [none]
		T_approach = value(P_T_APPROACH);		//Cooling tower approach temperature [C]
		T_ITD_des = value(P_T_ITD_DES);		//ITD at design for dry system [C]
		P_cond_ratio = value(P_P_COND_RATIO);		//Condenser pressure ratio [none]
		pb_bd_frac = value(P_PB_BD_FRAC);		//Power block blowdown steam fraction  [none]
		P_cond_min = value(P_P_COND_MIN);		//Minimum condenser pressure [inHg]
		n_pl_inc = (int)value(P_N_PL_INC);		//Number of part-load increments for the heat rejection system [none]
		F_wc = value(P_F_WC, &nval_F_wc);		//Fraction indicating wet cooling use for hybrid system [none]

		//unit conversions
		//P_ref *= 1000.;		//[MW] -> [kW]
		//P_cond_min *= 3386.;	//[inHg] -> [Pa]

		/* 
		--- 
		Code conversion note
		---
		This type uses the power block class from SSC/recore/lib_powerblock.h. The code was already converted for 
		the geothermal hourly model, so rather than reconvert it again, we just implement the existing class in 
		the TCS type framework.

		Set up the class and structure objects here...
		*/
		params.P_ref = P_ref;
		params.eta_ref = eta_ref;
		params.T_htf_hot_ref = T_htf_hot_ref;
		params.T_htf_cold_ref;
		params.T_htf_cold_ref = T_htf_cold_ref;
		params.dT_cw_ref = dT_cw_ref;
		params.T_amb_des = T_amb_des;
		// params.HTF = HTF;
		params.htfProps = htfProps;
		params.q_sby_frac = q_sby_frac;
		params.P_boil = P_boil;
		params.CT = CT;
		params.startup_time = startup_time;
		params.startup_frac = startup_frac;
		params.tech_type = tech_type;
		params.T_approach = T_approach;
		params.T_ITD_des = T_ITD_des;
		params.P_cond_ratio = P_cond_ratio;
		params.pb_bd_frac = pb_bd_frac;
		params.P_cond_min = P_cond_min;
		params.n_pl_inc = n_pl_inc;
		for(int i=0; i<9; i++){ params.F_wc[i] = F_wc[i]; }
		
		
		type224.InitializeForParameters( params );

		//Set initial stored values
		standby_control0 = 0;
		startup_remain0 = startup_time;
		P_cycle0 = 0.;
		//8.30.2010 :: Calculate the startup energy needed
		startup_e_remain0 = startup_frac*P_ref/eta_ref;         //[kWt]
		
		return 0;
	}

	virtual int call(double time, double step, int ncall){


		mode = (int)value(I_MODE);		//Cycle part load control, from plant controller [none]
		T_htf_hot = value(I_T_HTF_HOT);		//Hot HTF inlet temperature, from storage tank [C]
		m_dot_htf = value(I_M_DOT_HTF);		//HTF mass flow rate [kg/hr]
		T_wb = value(I_T_WB);		//Ambient wet bulb temperature [C]
		demand_var = value(I_DEMAND_VAR);		//Control signal indicating operational mode [none]
		standby_control = (int)value(I_STANDBY_CONTROL);		//Control signal indicating standby mode [none]
		T_db = value(I_T_DB);		//Ambient dry bulb temperature [C]
		P_amb = value(I_P_AMB);		//Ambient pressure [mbar]
		TOU = (int)value(I_TOU) - 1;		//Current Time-of-use period [none]
		rh = value(I_RH)/100.0;		//Relative humidity of the ambient air [none]

		
		//Set the inputs
		inputs.mode = mode;
		inputs.T_htf_hot = T_htf_hot;
		inputs.m_dot_htf = m_dot_htf;
		inputs.T_wb = T_wb + 273.15;		//Convert C to K
		inputs.demand_var = demand_var;
		inputs.standby_control = standby_control;
		inputs.T_db = T_db + 273.15;		//Convert C to K
		inputs.P_amb = P_amb*100.0;			//Convert mbar to Pa
		inputs.TOU = TOU;
		inputs.rel_humidity = rh;

		//Get the stored values
		/*
		stored.iLastStandbyControl = standby_control0;
		stored.dStartupTimeRemaining = startup_remain0;
		stored.dLastP_Cycle = P_cycle0;
		stored.dStartupEnergyRemaining = startup_e_remain0;
		*/

		//Simulate
		type224.Execute((long)time, inputs);
		outputs = type224.GetOutputs();

		
		//Set the outputs
		P_cycle = outputs.P_cycle;
		eta = outputs.eta;
		T_htf_cold = outputs.T_htf_cold;
		m_dot_makeup = outputs.m_dot_makeup;
		m_dot_demand = outputs.m_dot_demand;
		m_dot_htf_out = outputs.m_dot_htf;
		m_dot_htf_ref = outputs.m_dot_htf_ref;
		W_cool_par = outputs.W_cool_par;
		P_ref_out = outputs.P_ref;
		f_bays = outputs.f_hrsys;
		P_cond = outputs.P_cond;
		

		value(O_P_CYCLE, P_cycle);		//[MWe] Cycle power output
		value(O_ETA, eta);		//[none] Cycle thermal efficiency
		value(O_T_HTF_COLD, T_htf_cold);		//[C] Heat transfer fluid outlet temperature 
		value(O_M_DOT_MAKEUP, m_dot_makeup);		//[kg/hr] Cooling water makeup flow rate
		value(O_M_DOT_DEMAND, m_dot_demand);		//[kg/hr] HTF required flow rate to meet power load
		value(O_M_DOT_HTF_OUT, m_dot_htf_out);		//[kg/hr] Actual HTF flow rate passing through the power cycle
		value(O_M_DOT_HTF_REF, m_dot_htf_ref);		//[kg/hr] Calculated reference HTF flow rate at design
		value(O_W_COOL_PAR, W_cool_par);		//[MWe] Cooling system parasitic load
		value(O_P_REF_OUT, P_ref_out);		//[MWe] Reference power level output at design (mirror param)
		value(O_F_BAYS, f_bays);		//[none] Fraction of operating heat rejection bays
		value(O_P_COND, P_cond);		//[Pa] Condenser pressure
		
		return 0;
	}

	virtual int converged(double time){

		//Set the stored values for the next time step
		/*
		standby_control0 = stored.iLastStandbyControl;
		startup_remain0 = max(stored.dStartupTimeRemaining-dt/3600., 0.);
		P_cycle0 = stored.dLastP_Cycle;
		startup_e_remain0 = stored.dStartupEnergyRemaining;
		*/

		return 0;
	}
	
	
};


TCS_IMPLEMENT_TYPE( sam_mw_pt_type224, "Indirect HTF power cycle model", "Mike Wagner", 1, sam_mw_pt_type224_variables, NULL, 1 );
