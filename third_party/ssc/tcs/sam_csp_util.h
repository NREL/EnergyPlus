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

#ifndef __CSP_UTIL_
#define __CSP_UTIL_
// fix compilation errors using gcc on linux
#include <cmath>
#include <algorithm>
#include <limits>
#include "../shared/lib_util.h"
#include "htf_props.h"


using namespace std;

/* 
Define functions and methods that are useful in CSP modules
*/

namespace CSP
{
	const double sigma = 5.67E-8;		//[W/m2K4] stefan boltzmann constant
	const double grav = 9.81;			//[m/s2] gravitational constant
	const double pi = 3.1415926;		//[-]

	//--- generalized interpolation functions ---

	double interp(util::matrix_t<double> *data, double x, int low_bound = -1, int up_bound = -1, bool increasing = true);

	double interp(double *xdat, double *ydat, double x, int low_bound, int up_bound, bool increasing = true);

	double interp2D(double *xvals, int &nx, double *yvals, int &ny, double *data2D, double x, double y, bool strict_range=false); 

	void theta_trans(double alpha_sun /*rad*/, double phi_sun /*rad*/, double alpha_fix /*rad*/, double &phi_t /*rad*/, double &theta /*rad*/);

	//sky temp function
	double skytemp(double T_amb_K, double T_dp_K, double hour);

	double sign(double val);

	double nint(double val);

    template<typename T>
    bool isequal(T a, T b)
    {
        return std::abs(a - b) <= std::min(std::abs(a), std::abs(b)) * std::numeric_limits<T>::epsilon();
    }

	int TOU_Reader(double *TOUSched, double time_sec, int nTOUSched=8760);

	double poly_eval(double x, const double *coefs, const int &order);

	double Nusselt_FC( double ksDin, double Re );

	void PipeFlow(double Re, double Pr, double LoverD, double relRough, double &Nusselt, double &f);

	bool flow_patterns( int n_panels, int crossover_shift, int flow_type, int & n_lines, util::matrix_t<int> & flow_pattern, std::string *messages = 0 );

	// CSP Cooling functions
	double P_sat4(double T_celcius);

	// Calculates enthalpy of air [J/kg] as a function of temperature [C]
	double f_h_air_T(double T_C);

	// Turbine isentropic efficiency penalty as a function of mass flow fraction (Patnode thesis)
	double eta_pl(double mf);

	// Evaporative cooling calculations
	void evap_tower(int tech_type, double P_cond_min, int n_pl_inc, double DeltaT_cw_des, double T_approach, double P_cycle, 
					double eta_ref, double T_db_K, double T_wb_K, double P_amb_Pa, double q_reject, double &m_dot_water,
					double &W_dot_tot, double &P_cond, double &T_cond, double &f_hrsys);

	// Air cooling calculations
	void ACC( int tech_type, double P_cond_min, int n_pl_inc, double T_ITD_des, double P_cond_ratio, double P_cycle, double eta_ref, 
		 double T_db_K, double P_amb_Pa, double q_reject, double& m_dot_air, double& W_dot_fan, double& P_cond, double& T_cond, 
		 double &f_hrsys);

	// Hybrid cooling calculations
	void HybridHR( int tech_type, double P_cond_min, int n_pl_inc, double F_wc, double F_wcmax, double F_wcmin,
				  double T_ITD_des, double T_approach, double dT_cw_ref, double P_cond_ratio, double P_cycle, double eta_ref, 
				  double T_db_K, double T_wb_K, double P_amb_Pa, double q_reject, double& m_dot_water, double& W_dot_acfan, 
				  double& W_dot_wctot, double& W_dot_tot, double& P_cond, double& T_cond, double& f_hrsys);
	// Surface condenser ARD
	void surface_cond(int tech_type, double P_cond_min, int n_pl_inc, double DeltaT_cw_des, double T_approach, double P_cycle,
		double eta_ref, double T_db_K, double T_wb_K, double P_amb_Pa, double T_cold, double q_reject, double &m_dot_water,
		double &W_dot_tot, double &P_cond, double &T_cond, double &f_hrsys, double &T_cond_out);


    // Pipe sizing
    double pipe_sched(double De, bool selectLarger = true);

    // Pipe wall thickness
    double WallThickness(double d_in);

    // Minor pressure drop
    double MinorPressureDrop(double vel, double rho, double k);

    // Major pressure drop
    double MajorPressureDrop(double vel, double rho, double ff, double l, double d);

    // Friction factor
    double FrictionFactor(double rel_rough, double Re);

    // Friction factor (iterative, helper function)
    double FricFactor_Iter(double rel_rough, double Re);

};
    // Statistical mode
    //template <typename T, typename A>  // need to specify an allocator 'A'
    //T mode(std::vector<T,A> const& v);
double mode(std::vector<double> v);

// Set up class for Pmax function so we can save maximum pressure for various CSP types
class P_max_check
{
	double P_max;
	double P_save;
	bool is_error;

public:
	P_max_check(){};

	~P_max_check(){};

	void set_P_max( double P_max_set );

	void report_and_reset();

	double P_check( double P );

};

// Set up class for enthalpy limit function
class enth_lim
{
private:
	double h_min;
	double h_max;

public:
	enth_lim() {};
	~enth_lim() {};

	void set_enth_limits( double h_min_in, double h_max_in );

	double check( double h_in );
};

// ------- define the emittance table class for the physical trough and linear fresnel-molten salt models
class emit_table
{
	double *T;	//Temperature
	double *E;	//Emittance
	int *lengths;	//Array of lengths of each table
	int *starts;	//Start position of each table
	int memsize, datasize, nloaded, ntables, nt, nv;

public:
	emit_table() {
		T = E = 0;
		lengths = starts = 0;
		memsize = datasize = nloaded = ntables = nt = nv = 0;
	};
	
	~emit_table(){
		if(lengths) delete [] lengths;
		if(starts) delete [] starts;
		if(T) delete[] T;
		if(E) delete[] E;
	};
	
	void init(int nHCEVars){
		init(1, nHCEVars);
	};


	void init(int nHCEtypes, int nHCEvars){
		ntables = nHCEtypes * nHCEvars;
		nt = nHCEtypes;
		nv = nHCEvars;
		lengths = new int[ntables];
		lengths[0] = 0;
		starts = new int[ntables];
		starts[0] = 0;

		//initialize the data arrays with a preallocation of 15 entries per table.
		memsize = 15*ntables;
		T = new double[memsize];
		E = new double[memsize];
		datasize = 0;	//The current number of total entries 
		nloaded = 0;	//The number of tables currently loaded
	};

	bool addTable(util::matrix_t<double> *table){
		/*
		Take the data from the matrix_t entry for a single emittance table and load it into the emit_table object

		Column 0: Temperature [C]
		Column 1: Emittance(T)	[-]
		*/

		//Is the object full?
		if(nloaded + 1 == ntables) return false;
		
		//Get the dimensions of the table thats up for addition
		int nr = (int)table->nrows();
		int nc = (int)table->ncols();

		// 11.17.14 twn: SAM is reporting single emis as 1 value, instead of original TCS convention of (0, emis)
		// maybe we can make a quick fix here?
		if( nr == 1 && nc == 1 )
		{
			lengths[nloaded] = 1;
			starts[nloaded] = datasize;

			T[datasize] = 0.0;
			E[datasize] = table->at(0, 0);

			datasize++;
			nloaded++;

			return true;
		}
		else if( nr != 2) 
			return false;

		//if we need to add space, copy data to a new array
		if(datasize + nc > memsize){
			memsize = datasize + nr;
			if(T) delete[] T;
			double *Ttemp = new double[memsize];
			if(E) delete[] E;
			double *Etemp = new double[memsize];
			
			for(int i=0; i<datasize; i++){
				Ttemp[i] = T[i];
				Etemp[i] = E[i];
			}

			delete [] T;
			delete [] E;
			T = Ttemp;
			E = Etemp;
			//Not sure this is right...
		}

		//Add the table
		lengths[nloaded] = nc;
		starts[nloaded] = datasize;
		for(int i=0; i<nc; i++){
			T[datasize + i] = table->at(0, i);
			E[datasize + i] = table->at(1, i);
		}
		datasize += nc;

		nloaded ++;
		return true;
	}

	int getTableSize(int hce_var){
		return getTableSize(0, hce_var);
	}

	int getTableSize(int hce_type, int hce_var){
		/* inputs should be indexed at 0 */
		int n = hce_type * nv + hce_var;
		return lengths[n];
	}

	bool isTable(int hce_var){
		return isTable(0, hce_var);
	}
	
	bool isTable(int hce_type, int hce_var){
		return getTableSize(hce_type, hce_var) == 1;
	}

	double interpolate(int hce_var, double Temp){
		return interpolate(0, hce_var, Temp);
	}
	
	double interpolate(int hce_type, int hce_var, double Temp){
		int n = hce_type * nt + hce_var;	//Index
		int lb = starts[n],
			ub = starts[n] + lengths[n]-1;
		return CSP::interp(T, E, Temp, lb, ub);
	}

	double getSingleValue(int hce_var){
		return getSingleValue(0, hce_var);
	}

	double getSingleValue(int hce_type, int hce_var){
		/*Get the emittance value for a single-entry table. For full tables, return NULL. */
		int n = hce_type * nt + hce_var;
		if(lengths[n] > 1){
			return std::numeric_limits<double>::quiet_NaN();
		}
		else{
			return E[starts[n]];
		}
	}
};

// Optical data table for 2D interpolation
class OpticalDataTable
{
	/* 
	Structure for providing optical efficiency in tabular form. 

	Use the following solar angle conventions:

	Azimuth		|	-pi & +pi are North 
				|	-pi/2 is East
				|	0 is South
				|	+pi/2 is West
	Zenith		|	0 is vertical
				|	pi/2 is horizon
				|	>pi/2. is below horizon
	*/
	
	double *xvals, *yvals, *data;
	bool xax_allocated, yax_allocated, data_allocated;
	int sizex, sizey;

public:
	OpticalDataTable(){
		xax_allocated = false;
		yax_allocated = false;
		data_allocated = false;
		xvals = 0;
		yvals = 0;
		data = 0;
	}

	~OpticalDataTable(){
		if(xax_allocated) delete [] xvals;
		if(yax_allocated) delete [] yvals;
		if(data_allocated) delete [] data;
	}

	void AddXAxis(double *xdata, int nx){
		if(xax_allocated) delete [] xvals;
		sizex = nx;
		xvals = new double[nx];
		xax_allocated = true;
		for(int i=0; i<nx; i++)
			xvals[i] = xdata[i];
	}

	void AddYAxis(double *ydata, int ny){
		if(yax_allocated) delete [] yvals;
		sizey = ny; 
		yvals = new double[ny];
		yax_allocated = true;
		for(int i=0; i<ny; i++)
			yvals[i] = ydata[i];
	}

	void AddData(double *dat){
		if(data_allocated) delete [] data;
		data = new double[sizex * sizey];
		data_allocated = true;
		for(int i=0; i<sizey; i++){
			for(int j=0; j<sizex; j++){
				data[ i*sizex + j ] = dat[ i*sizex + j ];
			}
		}
	}

	bool AddData(util::matrix_t<double> &dat){
		if( (int)dat.nrows() != sizey || (int)dat.ncols() != sizex )
			return false;

		if(data_allocated) delete [] data;
		data = new double[sizex * sizey];
		data_allocated = true;
		for(int i=0; i<sizey; i++){
			for(int j=0; j<sizex; j++){
				data[j*sizex + i] = dat.at(i, j);
			}
		}
		return true;
	}

	double interpolate(double x, double y){
		return CSP::interp2D(xvals, sizex, yvals, sizey, data, x, y);
	}

	double nearest(double x, double y){
		int nearx=0, neary=0;
		double rx=9.e9, ry=9.e9;
		for(int i=0; i<sizex; i++){
			double r = fabs(x - xvals[i]);
			if(r < rx){
				rx = r;
				nearx = i;
			}
		}
		for(int i=0; i<sizey; i++){
			double r = fabs(y - yvals[i]);
			if(r < ry){
				ry = r;
				neary = i;
			}
		}
		return data[neary*sizex + nearx];
	}

};

class TwoOptTables
{
	// Collect and manage two instances of OpticalDataTable class
private:
	OpticalDataTable * table0;
	OpticalDataTable * table1;

public:
	TwoOptTables(){};

	~TwoOptTables(){};

	bool Set_Table( OpticalDataTable * table_in, int table_index )
	{
		if( table_index == 0 )
		{
			table0 = table_in;
			return true;
		}
		else if( table_index == 1 )
		{
			table1 = table_in;
			return true;
		}
		else
			return false;
	}

	double interpolate( double x, double y, int table_index )
	{
		if( table_index == 0 )
			return table0->interpolate( x, y );
		else if( table_index == 1 )
			return table1->interpolate( x, y );
		else
			return -999.9;
	}

};

class Evacuated_Receiver
{
private:
	emit_table * m_eps3;
	HTFProperties m_airProps;
	HTFProperties * p_htfProps;

	// Trough properties set during initialization call
	// Need to figure out how to index boiler & superheater in main code
	util::matrix_t<bool> m_Glazing_intact;
	util::matrix_t<double> m_P_a;
	util::matrix_t<double> m_D_5;
	util::matrix_t<double> m_D_4;
	util::matrix_t<double> m_D_3;
	util::matrix_t<double> m_D_2;
	util::matrix_t<double> m_D_p;
	util::matrix_t<double> m_Dirt_HCE;
	util::matrix_t<double> m_Shadowing;
	util::matrix_t<double> m_tau_envelope;
	util::matrix_t<double> m_alpha_abs;
	util::matrix_t<double> m_alpha_env;
	util::matrix_t<HTFProperties*> m_AnnulusGasMat;		// Need separate classes for boiler and SH?
	util::matrix_t<AbsorberProps*> m_AbsorberPropMat;	// Need separate classes for boiler and SH?
	util::matrix_t<double> m_epsilon_4;
	util::matrix_t<double> m_epsilon_5;
	util::matrix_t<double> m_L_actSCA;
	util::matrix_t<double> m_A_cs;
	util::matrix_t<double> m_D_h;
	util::matrix_t<double> m_flowtype;

	std::vector<double> mv_reguess_args;

	// Updated once per timestep
	util::matrix_t<double> m_ColOptEff;

	// Saved between calls
	util::matrix_t<double> m_T_save;
	

public:
	Evacuated_Receiver()
	{
		mv_reguess_args.resize(3);
		std::fill(mv_reguess_args.begin(), mv_reguess_args.end(), std::numeric_limits<double>::quiet_NaN());
	};

	~Evacuated_Receiver(){};

	void Initialize_Receiver( util::matrix_t<bool> & Glazing_intact, util::matrix_t<double> & P_a, util::matrix_t<double> & D_5, util::matrix_t<double> & D_4, util::matrix_t<double> & D_3,
	                            util::matrix_t<double> & D_2, util::matrix_t<double> & D_p,
								util::matrix_t<double> & ColOptEff, util::matrix_t<double> & Dirt_HCE, util::matrix_t<double> & Shadowing, util::matrix_t<double> tau_envelope,
								util::matrix_t<double> & alpha_abs, util::matrix_t<double> & alpha_env, emit_table * eps3, util::matrix_t<HTFProperties*> & AnnulusGasMat,
								util::matrix_t<AbsorberProps*> & AbsorberPropMat, util::matrix_t<double> & epsilon_4, util::matrix_t<double> & epsilon_5, util::matrix_t<double> & L_actSCA,
								HTFProperties * htfProps, util::matrix_t<double> & A_cs, util::matrix_t<double> & D_h, util::matrix_t<double> & flowpattern)
	{
		m_Glazing_intact = Glazing_intact;
		m_P_a = P_a;
		m_D_5 = D_5;
		m_D_4 = D_4;
		m_D_3 = D_3;
		m_D_2 = D_2;
		m_D_p = D_p;
		m_ColOptEff = ColOptEff;
		m_Dirt_HCE = Dirt_HCE;
		m_Shadowing = Shadowing;
		m_tau_envelope = tau_envelope;
		m_alpha_abs = alpha_abs;
		m_alpha_env = alpha_env;
		m_eps3 = eps3;
		m_AnnulusGasMat = AnnulusGasMat;
		m_AbsorberPropMat = AbsorberPropMat;
		m_epsilon_4 = epsilon_4;
		m_epsilon_5 = epsilon_5;
		m_L_actSCA = L_actSCA;
		m_A_cs = A_cs;
		m_D_h = D_h;
		m_flowtype = flowpattern;		

		m_T_save.resize_fill(5,1,0.0);

		m_airProps.SetFluid( HTFProperties::Air );

		p_htfProps = htfProps;
	}

	void Update_Timestep_Properties( util::matrix_t<double> & ColOptEff )
	{
		m_ColOptEff = ColOptEff;
	}


	void EvacReceiver(double T_1_in, double m_dot, double T_amb, double T_sky, double v_6, double P_6, double q_i, 
	int hn /*HCE number [0..3] */, int hv /* HCE variant [0..3] */, int ct /*Collector type*/, int sca_num, bool single_point,  int ncall, double time,
	//outputs
	double &q_heatloss, double &q_12conv, double &q_34tot, double &c_1ave, double &rho_1ave );

	void FQ_34CONV(double T_3, double T_4, double P_6, double v_6, double T_6, int hn, int hv, double & q_34conv, double & h_34);

	void FQ_34RAD(double T_3, double T_4, double T_7, double epsilon_3_v, int hn, int hv, double & q_34rad, double & h_34);

	void FQ_56CONV(double T_5, double T_6, double P_6, double v_6, int hn, int hv, double & q_56conv, double & h_6);

	double FQ_COND_BRACKET(double T_3, double T_6, double P_6, double v_6, int hn, int hv);

	double fT_2(double q_12conv, double T_1, double T_2g, double v_1, int hn, int hv);

	double FK_23(double T_2, double T_3, int hn, int hv);
};

// Functor for advanced vector of vector sorting
class sort_vecOfvec
{
private:
    std::vector<int> columns;
    std::vector<bool> ascending;
public:
    sort_vecOfvec(std::vector<int> cols, std::vector<bool> asc) :
        columns(cols), ascending(asc) {
        if (columns.size() != ascending.size()) {
            throw logic_error("Column indice vector and sorting vector lengths must be equal");
        }
    }

    bool operator () (const vector<double> &a, const vector<double> &b) const {
        int col;
        col = 0;
        for (size_t col_idx = 0; col_idx < columns.size(); col_idx++) {
            col = columns[col_idx];
            if (CSP::isequal(a[col], b[col])) {
                continue;
            }
            else if ( (a[col] < b[col] && ascending[col_idx] ) ||
                ( !(a[col] < b[col]) && !ascending[col_idx] )) {
                return true;
            }
            else {
                return false;
            }
        }
        return false;  // all sorting values are equal, must return false to adhere to 'strict weak ordering'
    }
};
#endif
