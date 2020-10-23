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

// HTF_props.h -- function prototypes for HTF property routines
#ifndef __HTF_PROPS_
#define __HTF_PROPS_

#include "interpolation_routines.h"
#include <limits>

class HTFProperties
{
public:
	HTFProperties();
	
	// The absolute value of these matter because we need to pass them in from SAM UI & cmods
	enum {
		Air = 1,
		Stainless_AISI316,
		Water_liquid,
		Steam,
		CO2,
		Salt_68_KCl_32_MgCl2,
		Salt_8_NaF_92_NaBF4,
		Salt_25_KF_75_KBF4,
		Salt_31_RbF_69_RbBF4,
		Salt_465_LiF_115_NaF_42KF,
		Salt_49_LiF_29_NaF_29_ZrF4,
		Salt_58_KF_42_ZrF4,
		Salt_58_LiCl_42_RbCl,
		Salt_58_NaCl_42_MgCl2,
		Salt_595_LiCl_405_KCl,
		Salt_595_NaF_405_ZrF4,
		Salt_60_NaNO3_40_KNO3,
		Nitrate_Salt,
		Caloria_HT_43,
		Hitec_XL,
		Therminol_VP1,
		Hitec,
		Dowtherm_Q,
		Dowtherm_RP,
		Blank1,
		Argon_ideal,
		Hydrogen_ideal,
		T91_Steel,
		Therminol_66,
		Therminol_59,
		Pressurized_Water,
        N06230,         // Nickel alloy
        N07740,         // Nickel alloy
		End_Library_Fluids,
		User_defined = 50
	};
	
	/* User defined table must have 7 columns, and at least 3 rows
	 col 0: Temp 'C
	 col 1: Cp kJ/kg-K
	 col 2: Dens kg/m3
	 col 3: Visc Pa-s
	 col 4: Kinematic visc m2/s
	 col 5: conductivity W/m-K
	 col 6: Enthalpy J/kg
	*/
	
	const char *UserFluidErrMessage(){return uf_err_msg.c_str();}
	bool SetFluid( int fluid );
	bool SetFluid( int fluid, bool calc_temp_enth_table);
	int GetFluid() { return m_fluid; }
	bool SetUserDefinedFluid( const util::matrix_t<double> &table );
	bool SetUserDefinedFluid(const util::matrix_t<double> &table, bool calc_temp_enth_table);

	double Cp( double T_K );    //[kJ/kg-K]
	double dens( double T_K, double P );
	double visc( double T_K );
	double cond( double T_K );
	double Cv( double T_K );
	double kin_visc( double T_K, double P );
	double therm_diff( double T_K, double P );
	double Pr( double T_K, double P );
	double Re( double T_K, double P, double vel, double d );
	double temp( double H );
	double enth( double T_K );

	double temp_lookup( double enth /*kJ/kg*/ );
	double enth_lookup( double temp /*K*/ );

	// 12.11.15 twn: Add method to calculate Cp as average of values throughout temperature range
	//               rather than at the range's midpoint
	double Cp_ave(double T_cold_K, double T_hot_K, int n_points);

	const util::matrix_t<double> *get_prop_table();
	//bool equals(const util::matrix_t<double> *comp_table);
	bool equals(HTFProperties *comp_class);

private:
	static const int m_m = 2;		// Integer for interpolation routine

	Linear_Interp User_Defined_Props;		// Define interpolation class in case user defined propeties are required

	Linear_Interp mc_temp_enth_lookup;		// Enthalpy-temperature relationship, populated by pre-processor: 'set_temp_enth_lookup' 
	void set_temp_enth_lookup();
	bool m_is_temp_enth_avail;

	int m_fluid;	// Store fluid number as member integer
	util::matrix_t<double> m_userTable;	// User table of properties

	std::string uf_err_msg;	//Error message when the user HTF table is invalid
	
};

class AbsorberProps
{
	int mnum;
public:
	//Absorber materials:
	// (1)   304L
	// (2)   216L
	// (3)   321H
	// (4)   B42 Copper Pipe
	enum{
		Mat_304L=1,
		Mat_216L,
		Mat_321H,
		Mat_B42_Copper_Pipe
	};

	void setMaterial(int mat_num){mnum = mat_num;}

	double cond(double T, int mat_num=-1){
		//T [C]
		//returns W/m-K
		//optional to use argument for material number
		int mtemp = mat_num;
		if(mat_num < 0) mtemp = mnum;
		switch(mtemp){
		case Mat_304L:
			return 0.013 * T + 15.2;  //[W/m-K];
		case Mat_216L:
			return 0.013 * T + 15.2;  //[W/m-K];
		case Mat_321H:
			return 0.0153 * T + 14.775;
		case Mat_B42_Copper_Pipe:
			return 400.;
		};
		return std::numeric_limits<double>::quiet_NaN();
	};
};

#endif