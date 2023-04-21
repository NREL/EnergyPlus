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

// 1D linear interpolation and 2D bilinear interpolation routines
#ifndef __interpolation_routines_
#define __interpolation_routines_

#include <../shared/lib_util.h>

//#include "cavity_calcs.h"		// for access to "block_t"

class Linear_Interp
{
public:
	bool m_cor;			// Boolean value for interpolation routine
	
	bool Set_1D_Lookup_Table( const util::matrix_t<double> &table, int * ind_var_index, int n_ind_var, int & error_index );	
	double linear_1D_interp( int x_col, int y_col, double x );
	int Get_Index( int x_col, double x );	
	double Get_Value( int col, int index );

	int locate( int col, double T_C );		// Function for interpolation routine
	int hunt( int col, double x );			// Function for interpolation routine

	// If the x-column is always index 0, we can simplify linear_1D_interp
	double interpolate_x_col_0(int y_col, double x_val);

	double get_min_x_value_x_col_0();
	double get_max_x_value_x_col_0();
	double get_x_value_x_col_0(int index){return Get_Value(0, index);};

	bool check_x_value_x_col_0(double x_val);

	std::string get_error_msg(){ return m_error_msg; };

    std::vector<double> get_column_data(int col);

	int get_number_of_rows(){ return m_rows; };

private:
	static const int m_m = 2;		// Integer for interpolation routine

	// member string for messages
	std::string m_error_msg;

	util::matrix_t<double> m_userTable; // 1D User table

	int m_rows;			// Number of rows in table
	int m_lastIndex;	// Integer tracking index used by interpolation routine
	int m_dj;			// Integer for interpolation routine

};

class Bilinear_Interp
{
// 3 columns by X rows
// 0 column: 1..nx, 1..nx, 1..nx, -> repeat ny times
// 1 column: 1..1 (nx times), 2..2 (nx times), ... , ny..ny (nx times)
// 2 column: (nx*ny) values
public: 
	bool Set_2D_Lookup_Table( const util::matrix_t<double> &table );
	double bilinear_2D_interp( double x, double y );

private:
	util::matrix_t<double> m_2axis_table;	// 2D (ind. x and y cols, z dependent var col)

	// 1D interpolation instances for values of each independent variable
	//Interp y_vals;
	
	int m_nx;		// Number of x values in table
	int m_ny;		// Number of y values in table

	Linear_Interp x_vals;
	Linear_Interp y_vals;
};

class Trilinear_Interp
{
	// 4 columns by X rows
	// 0 column: x value - [1..nx, 1..nx, 1..nx, -> repeat ny times ] 
	// 1 column: y value - [1..1 (nx times), 2..2 (nx times), ... , ny..ny (nx times) ] 
	// 2 column: z value - [1..1 (nx*ny) times]
	// 3 column: result value	[(nx*ny) values]
	// Repeat list for each layer

public:
	bool Set_3D_Lookup_Table( const util::block_t<double> &table );
	double trilinear_3D_interp( double x, double y, double z);

private:
	util::block_t<double> m_3axis_table;

	int 
		m_nx,
		m_ny,
		m_nz;

	Linear_Interp x_vals;
	Linear_Interp y_vals;
	Linear_Interp z_vals;

};

typedef std::vector<double> VectDoub;
typedef std::vector<VectDoub >  MatDoub;

struct LUdcmp
{
	/* 
	LU Decomposition (solution to matrix equation A . x = b)
	*/

	int n;

	MatDoub lu;
	MatDoub aref;
	
	std::vector<int> indx;
	double d;

	LUdcmp(MatDoub &a);
	void solve(VectDoub &b, VectDoub &x);
	void solve(MatDoub &b, MatDoub &x);
	void inverse(MatDoub &ainv);
	double det();
	void mprove(VectDoub &b, VectDoub &x);
};



struct Powvargram {
	double alph, bet, nugsq;
	
	double SQR( const double a );  // a squared
	
	Powvargram();

	Powvargram(MatDoub &x, VectDoub &y, const double beta = 1.5, const double nug=0.);

	double operator() (const double r) const;
};

struct GaussMarkov {
    MatDoub x;
    Powvargram vgram;
    int ndim, npt;
    double lastval, lasterr;
    VectDoub y,dstar,vstar,yvi;
    MatDoub v;
    LUdcmp *vi;
    
	double SQR( const double a );

    GaussMarkov(MatDoub &xx, VectDoub &yy, Powvargram &vargram, const double *err=NULL);
    GaussMarkov();

    ~GaussMarkov();

    double interp(VectDoub &xstar);

    double interp(VectDoub &xstar, double &esterr);

    double rdist(VectDoub *x1, VectDoub *x2);
};




#endif