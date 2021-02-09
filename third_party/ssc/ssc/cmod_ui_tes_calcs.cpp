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
#include "htf_props.h"
#include "sam_csp_util.h"
#include "csp_solver_two_tank_tes.h"

static var_info _cm_vtab_ui_tes_calcs[] = {

	/*   VARTYPE   DATATYPE         NAME               LABEL                                            UNITS     META  GROUP REQUIRED_IF CONSTRAINTS         UI_HINTS*/
	{ SSC_INPUT,   SSC_NUMBER,   "P_ref",                    "Power cycle output at design",                 "MWe",   "", "",  "*",  "", "" },
	{ SSC_INPUT,   SSC_NUMBER,   "design_eff",               "Power cycle thermal efficiency",               "",      "", "",  "*",  "", "" },
	{ SSC_INPUT,   SSC_NUMBER,   "tshours",                  "Hours of TES relative to q_dot_pb_des",        "hr",    "", "",  "*",  "", "" },
	{ SSC_INPUT,   SSC_NUMBER,   "T_htf_hot_des",            "Hot HTF temp (into TES HX, if applicable)",    "C",     "", "",  "*",  "", "" },
	{ SSC_INPUT,   SSC_NUMBER,   "T_htf_cold_des",           "Cold HTF temp (out of TES HX, if applicable)", "C",     "", "",  "*",  "", "" },
	{ SSC_INPUT,   SSC_NUMBER,   "rec_htf",                  "TES storage fluid code",                       "",      "", "",  "*",  "", "" },
	{ SSC_INPUT,   SSC_MATRIX,   "field_fl_props",           "User defined tes storage fluid prop data",     "",      "7 columns (T,Cp,dens,visc,kvisc,cond,h), at least 3 rows", "",  "*",  "", "" },
	{ SSC_INPUT,   SSC_NUMBER,   "h_tank_min",               "Min. allowable HTF height in storage tank",    "m",     "", "",  "*",  "", "" },
	{ SSC_INPUT,   SSC_NUMBER,   "h_tank",                   "Total height of tank (HTF when tank is full",  "m",     "", "",  "*",  "", "" },
	{ SSC_INPUT,   SSC_NUMBER,   "tank_pairs",               "Number of equivalent tank pairs",              "",      "", "",  "*",  "", "" },
	{ SSC_INPUT,   SSC_NUMBER,   "u_tank",                   "Loss coefficient from the tank",               "W/m2-K","", "",  "*",  "", "" },

	{ SSC_OUTPUT,  SSC_NUMBER,   "q_tes",                    "TES thermal capacity at design",               "MWt-hr","", "",  "*",  "", "" },
	{ SSC_OUTPUT,  SSC_NUMBER,   "tes_avail_vol",            "Available single temp storage volume",         "m^3",   "", "",  "*",  "", "" },
	{ SSC_OUTPUT,  SSC_NUMBER,   "vol_tank",                 "Total single temp storage volume",             "m^3",   "", "",  "*",  "", "" },
	{ SSC_OUTPUT,  SSC_NUMBER,   "csp_pt_tes_tank_diameter", "Single tank diameter",                         "m",     "", "",  "*",  "", "" },
	{ SSC_OUTPUT,  SSC_NUMBER,   "q_dot_tes_est",            "Estimated tank heat loss to env.",             "MWt",   "", "",  "*",  "", "" },
	{ SSC_OUTPUT,  SSC_NUMBER,   "csp_pt_tes_htf_density",   "HTF dens",                                     "kg/m^3","", "",  "*",  "", "" },

	var_info_invalid};

class cm_ui_tes_calcs : public compute_module
{
public:

	cm_ui_tes_calcs()
	{
		add_var_info(_cm_vtab_ui_tes_calcs);
	}

	void exec() override
	{
		double P_ref = as_double("P_ref");		//[MWe] Power cycle output at design
		double design_eff = as_double("design_eff");            //[-] Power cycle efficiency at design 
		double q_dot_pb_des = P_ref / design_eff;		//[MWt] Power cycle thermal power at design

		double tshours = as_double("tshours");                  //[hrs] Hours of TES relative to q_dot_pb_des
		double Q_tes_des = q_dot_pb_des*tshours;                //[MWt-hr] TES thermal capacity at design
		assign("q_tes", (ssc_number_t)Q_tes_des);

		// Initialize HTF class
		HTFProperties tes_htf_props;			// Instance of HTFProperties class for TES HTF
		int tes_fl = (int) as_double("rec_htf");
		util::matrix_t<double> tes_fl_props = as_matrix("field_fl_props");

		double T_htf_hot_des = as_double("T_htf_hot_des");			//[C] Hot HTF temp
		double T_htf_cold_des = as_double("T_htf_cold_des");		//[C] Cold HTF temp
		double T_HTF_ave = 0.5*(T_htf_hot_des + T_htf_cold_des);	//[C] Ave HTF temp at design

		// Set fluid number and copy over fluid matrix if it makes sense.
		if( tes_fl != HTFProperties::User_defined && tes_fl < HTFProperties::End_Library_Fluids )
		{
			if( !tes_htf_props.SetFluid(tes_fl) )
			{
				throw exec_error("ui_tes_calcs", util::format("The user-defined HTF did not read correctly"));
			}
		}
		else if( tes_fl == HTFProperties::User_defined )
		{
			size_t n_rows = tes_fl_props.nrows();
			size_t n_cols = tes_fl_props.ncols();
			if( n_rows > 2 && n_cols == 7 )
			{
				if( !tes_htf_props.SetUserDefinedFluid(tes_fl_props) )
				{
					std::string error_msg = util::format(tes_htf_props.UserFluidErrMessage(), n_rows, n_cols);
					throw exec_error("ui_tes_calcs", error_msg);
				}
			}
			else
			{
				std::string error_msg = util::format("The user defined storage HTF table must contain at least 3 rows and exactly 7 columns. The current table contains %d row(s) and %d column(s)", n_rows, n_cols);
				throw exec_error("ui_tes_calcs", error_msg);
			}
		}
		else
		{
			throw exec_error("ui_tes_calcs", "Storage HTF code is not recognized");
		}

		double h_min = as_double("h_tank_min");			//[m]
		double h_tank = as_double("h_tank");			//[m]
		double tank_pairs = as_double("tank_pairs");	//[-]
		double u_tank = as_double("u_tank");			//[W/m^2-K]

		double tes_avail_vol, vol_tank, csp_pt_tes_tank_diameter, q_dot_loss_des;
		tes_avail_vol = vol_tank = csp_pt_tes_tank_diameter = q_dot_loss_des = std::numeric_limits<double>::quiet_NaN();
		two_tank_tes_sizing(tes_htf_props, Q_tes_des, T_htf_hot_des+273.15, T_htf_cold_des+273.15, 
			h_min, h_tank, (int)tank_pairs, u_tank,
			tes_avail_vol, vol_tank, csp_pt_tes_tank_diameter, q_dot_loss_des);

		assign("tes_avail_vol", (ssc_number_t)tes_avail_vol);
		assign("vol_tank", (ssc_number_t)vol_tank);
		assign("q_dot_tes_est", (ssc_number_t)q_dot_loss_des);
		assign("csp_pt_tes_tank_diameter", (ssc_number_t)csp_pt_tes_tank_diameter);
        assign("csp.pt.tes.tank_diameter", (ssc_number_t)csp_pt_tes_tank_diameter);
		assign("csp_pt_tes_htf_density", (ssc_number_t)tes_htf_props.dens(T_HTF_ave + 273.15, 1.0));
        assign("csp.pt.tes.htf_density", (ssc_number_t)tes_htf_props.dens(T_HTF_ave + 273.15, 1.0));

		//double rho_ave = tes_htf_props.dens(T_HTF_ave+273.15, 1.0);		//[kg/m^3] Density at average temperature
		//double cp_ave = tes_htf_props.Cp(T_HTF_ave+273.15);				//[kJ/kg-K] Specific heat at average temperature
		//
		//	//[m^3] = [MJ/s-hr] * [sec]/[hr] = [MJ] / (kg/m^3 * MJ/kg-K * K 
		//double tes_avail_vol = Q_tes_des*3600.0 / (rho_ave * cp_ave/1000.0 * (T_htf_hot_des - T_htf_cold_des));
		//assign("tes_avail_vol", tes_avail_vol);
		//
		//double h_min = as_double("h_tank_min");
		//double h_tank = as_double("h_tank");
		//
		//double vol_tank = tes_avail_vol / (1.0 - h_min/h_tank);	//[m^3]
		//assign("vol_tank", vol_tank);
		//
		//double tank_pairs = as_double("tank_pairs");
		//
		//double A_cs = vol_tank / (h_tank*tank_pairs);		//[m^2] Cross-sectional area of a single tank
		//
		//double diameter = pow(A_cs / CSP::pi, 0.5)*2.0;			//[m] Diameter of a single tank
		////assign("csp_pt_tes_tank_diameter", diameter);
		//assign("csp_pt_tes_tank_diameter", 1234.5);
		//
		//double u_tank = as_double("u_tank");	//[W/m^2-K]
		//
		//double UA = u_tank*(A_cs + CSP::pi*diameter*h_tank)*tank_pairs;		//[W/K]
		//double q_dot_tes_est = UA*(T_HTF_ave - 15.0)*1.E-6;	//[MWt]
		//
		//assign("q_dot_tes_est",q_dot_tes_est);
		
		return;
	}
};

DEFINE_MODULE_ENTRY(ui_tes_calcs, "Calculates values for all calculated values on UI TES page(s)", 0)
