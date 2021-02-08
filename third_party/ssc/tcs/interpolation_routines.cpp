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

// Functions for interpolation routines
#include <algorithm>

#include <cmath>

#include "interpolation_routines.h"

using namespace std;
using std::min;
using std::max;

bool Linear_Interp::Set_1D_Lookup_Table( const util::matrix_t<double> &table, int * ind_var_index, int n_ind_var, int & error_index )
{
	// 'ind_var_index': array of ints that list which columns are independent variables
	// 'n_ind_var': length of 'ind_var_index' array
	
	//If a user defined fluid, check for minimum number of rows
	if ( table.nrows() < 3 ) 
	{
		error_index = -1;
		return false;
	}

	// check monotonically increasing independent variables
	for ( int i = 0; i < n_ind_var; i++ )
		for ( int r = 1; r < (int)table.nrows(); r++ )
			if ( table.at(r, ind_var_index[i]) < table.at(r-1, ind_var_index[i]) )
			{
				error_index = i;
				return false;
			}

	// Set class member data
	m_userTable = table;	
	m_rows = (int)table.nrows();
	m_lastIndex = m_rows * 2;	// So "hunt" scheme cannot be called 3rd property call
	m_dj = min(1, (int) pow(m_rows, 0.25) );
	m_cor = false;

	return true;
}

double Linear_Interp::linear_1D_interp( int x_col, int y_col, double x )
{
	/*Given a value x, return an interpolated value y, using data xx and yy of size n. Save   **
    previous table location as jsav, and return it to the subroutine when calling. 
	Code is adapted from Numerical Recipes, 3rd Edition
	Converted to c++ from Fortran code "sam_mw_pt_propmod.f90" in November 2012 by Ty Neises */  
			
	int j = Get_Index( x_col, x );
		
	// Calculate y value using linear interpolation
	double y = m_userTable.at(j,y_col) + ((x - m_userTable.at(j,x_col))/(m_userTable.at(j+1,x_col)-m_userTable.at(j,x_col)))*(m_userTable.at(j+1,y_col) - m_userTable.at(j,y_col));

	return y;
}

// If the x-column is always index 0, we can simplify linear_1D_interp
double Linear_Interp::interpolate_x_col_0( int y_col, double x_val )
{
	return linear_1D_interp(0, y_col, x_val);
}

int Linear_Interp::Get_Index( int x_col, double x )
{
	// Find starting index for interpolation
	int j;
	if(m_cor) {j = hunt( x_col, x );}
	else {j = locate( x_col, x );}
	
	return j;
}

double Linear_Interp::Get_Value( int col, int index)
{
	/* 
	Return the value in the data array for column "col" and index "index"
	*/

	return m_userTable.at(index, col);

}

double Linear_Interp::get_min_x_value_x_col_0()
{
	int index = 0;	// Min x value is always in first row, with monotonically increasing x values

	return Get_Value(0, index);
}

double Linear_Interp::get_max_x_value_x_col_0()
{
	int index = m_rows - 1;		// Max x value is always last row, with monotonically increasing x values

	return Get_Value(0, index);
}

std::vector<double> Linear_Interp::get_column_data(int col)
{
    util::matrix_t<double> mt_col = m_userTable.col(col);
    int n_cols = mt_col.ncols();
    std::vector<double> v_col(n_cols);
    for (int i = 0; i < n_cols; i++)
    {
        v_col[i] = mt_col[i];
    }
    return v_col;
}

bool Linear_Interp::check_x_value_x_col_0(double x_val)
{
	double min_val = get_min_x_value_x_col_0();
	double max_val = get_max_x_value_x_col_0();

	if(x_val < min_val)
	{
		m_error_msg = util::format("The minimum value is %lg", min_val);
		return false;
	}

	if(x_val > max_val)
	{
		m_error_msg = util::format("The maximum value is %lg", max_val);
		return false;
	}

	return true;
}

int Linear_Interp::locate( int col, double x )
{
	/* Given a value x, return a value j such that x is (insofar as possible) centered in the   
    subrange xx[j..j+mm-1], where xx is the stored pointer.  The values in xx must be mono-  
    tonic, either increasing or decreasing.  The returned value is not less than 0, nor      
    greater than n-1. 
	Adapted from Numerical Recipes, 3rd Edition 
	Converted to c++ from Fortran code "sam_mw_pt_propmod.f90" in November 2012 by Ty Neises */

	int ju, jm, jl;
	// *** Assuming monotonically increasing temperature, per SetUserDefinedFluid function logic ***
	jl = 0;
	ju = m_rows - 1;
	while (ju - jl > 1)
	{
		jm = (ju + jl) / 2;
		if(x >= m_userTable.at(jm, col))
		{
			jl = jm;
		}
		else
		{
			ju = jm;
		}
	}
	if(abs(jl - m_lastIndex) > m_dj)
	{
		m_cor = false;
	}
	else
	{
		m_cor = true;
	}
	m_lastIndex = jl;
	return max(0, min(m_rows - m_m, jl - ((m_m - 2)/2)));
}

int Linear_Interp::hunt( int col, double x )
{
	/* Given a value x, return a value j such that x is (insofar as possible) centered in the
    subrange xx(j..j+mm-1), where xx is the stored data array.  The values in xx must be
    monatonic, either increasing or decreasing.  The returned value is not less than 0, nor
    greater than n-1. 
	Code is adapted from Numerical Recipes, 3rd Edition
	Converted to c++ from Fortran code "sam_mw_pt_propmod.f90" in November 2012 by Ty Neises */

	int jl = m_lastIndex, jm, ju, inc=1;
	if( jl < 0 || jl > m_rows - 1 )
	{
		jl = 0, ju = m_rows - 1;
	}
	else
	{
		if ( x >= m_userTable.at(jl, col) )	// Hunt up
		{
			ju = jl + inc;
			while( ju < m_rows - 1 && x > m_userTable.at(ju, col) )
			{
				jl = ju;
				inc += inc;
				ju = jl + inc;
			}
		}
		else		// Hunt down
		{
			ju = jl;
			jl = ju - inc;
			while( jl > 0 && x < m_userTable.at(jl, col) )
			{
				ju = jl;
				inc += inc;
				jl = ju - inc;
			}
		}

	}
	
	// 4.13.15 twn: Check that indices are within bounds
	if( ju > m_rows - 1 )
		ju = m_rows - 1;
	if( jl < 0 )
		jl = 0;
	
	while (ju - jl > 1)	// Hunt is done, begin final bisection phase
	{
		jm = (ju + jl) / 2;
		if(x >= m_userTable.at(jm, col))
		{
			jl = jm;
		}
		else
		{
			ju = jm;
		}
	}
	if(abs(jl - m_lastIndex) > m_dj)
	{
		m_cor = false;
	}
	else
	{
		m_cor = true;
	}
	m_lastIndex = jl;
	return max(0, min(m_rows - m_m, jl - ((m_m - 2)/2)));
}

double Bilinear_Interp::bilinear_2D_interp( double x, double y )
{

	/*int i = x_vals.m_cor ? x_vals.hunt(0, x) : x_vals.locate(0, y);
	int j = y_vals.m_cor ? y_vals.hunt(0, y) : y_vals.locate(0, y);

	double yy, t, u;

	//find the grid square
	double 
		xlo = x_vals.Get_Value(0, i),
		xhi = x_vals.Get_Value(0, i+1),
		ylo = y_vals.Get_Value(0, j),
		yhi = y_vals.Get_Value(0, j+1);

	t = (x - xlo) / (xhi - xlo);
	u = (y - ylo) / (yhi - ylo);

	*/





	
	int i_x1 = x_vals.Get_Index( 0, x );
	int i_y1 = y_vals.Get_Index( 0, y );

	int i_x2 = i_x1 + 1;
	int i_y2 = i_y1 + 1;

	int i1 = m_nx*i_y1 + i_x1;
	double x1 = m_2axis_table.at( i1, 0 );
	double y1 = m_2axis_table.at( i1, 1 );
	double z1 = m_2axis_table.at( i1, 2 );

	int i2 = m_nx*i_y2 + i_x1;
	double x2 = m_2axis_table.at( i2, 0 );
	double y2 = m_2axis_table.at( i2, 1 );
	double z2 = m_2axis_table.at( i2, 2 );

	int i3 = m_nx*i_y2 + i_x2;
	double x3 = m_2axis_table.at( i3, 0 );
	double y3 = m_2axis_table.at( i3, 1 );
	double z3 = m_2axis_table.at( i3, 2 );

	int i4 = m_nx*i_y1 + i_x2;
	double x4 = m_2axis_table.at( i4, 0 );
	double y4 = m_2axis_table.at( i4, 1 );
	double z4 = m_2axis_table.at( i4, 2 );

	double x_frac = (x - x1)/(x4 - x1);
	double y_frac = (y - y1)/(y2 - y1);

	return (1.0-x_frac)*(1.0-y_frac)*z1 + (1.0-x_frac)*y_frac*z2 + x_frac*y_frac*z3 + x_frac*(1.0-y_frac)*z4;
	
}

bool Bilinear_Interp::Set_2D_Lookup_Table( const util::matrix_t<double> &table )
{
	// Initialize class member data
	m_2axis_table = table;
	int nrows = (int)table.nrows();
	if( nrows < 9 )
		return false;
	
	// Find number of x values in table
	double first_val = table.at(0,0);
	int i;
	for( i = 1; i < (int)table.nrows(); i++ )
		if( table.at(i,0) == first_val )	break;
	m_nx = i;
	if( m_nx < 3 )
		return false;

	// Find number of y values in table
	m_ny = 1;
	i = 0;
	for( int j = 0; j < nrows - 1; j++ )
	{
		if( table.at(j+1,1) != table.at(j,1))
			m_ny++;
	}
	if( m_ny < 3 )
		return false;

	// Create 1D table for x values
	util::matrix_t<double> x_matrix( m_nx, 1, 0.0 );
	for( int j = 0; j < m_nx; j++ )
		x_matrix.at(j,0) = table.at( j, 0 );

	// Create 1D table for y values
	util::matrix_t<double> y_matrix( m_ny, 1, 0.0 );
	for( int j = 0; j < m_ny; j++ )
		y_matrix.at(j,0) = table.at( m_nx*j, 1 );

	// Set up 1D interpolation class instances for x and y values
	int ind_var_index[1] = {0};
	int error_index = -99;
	if( !x_vals.Set_1D_Lookup_Table( x_matrix, ind_var_index, 1, error_index ) )
		return false;
	if( !y_vals.Set_1D_Lookup_Table( y_matrix, ind_var_index, 1, error_index ) )
		return false;

	return true;
}

bool Trilinear_Interp::Set_3D_Lookup_Table( const util::block_t<double> &table )
{
	// Initialize class member data
	m_3axis_table = table;

	int nrows = (int)table.nrows();
	int nlayers = (int)table.nlayers();

	if( nrows < 9 || nlayers < 3)
		return false;
	
	// Find number of x values in table
	double first_val = table.at(0,0,0);
	int i;
	for( i = 1; i < (int)table.nrows(); i++ )
		if( table.at(i,0,0) == first_val )	break;
	m_nx = i;
	if( m_nx < 3 )
		return false;

	// Find number of y values in table
	m_ny = 1;
	i = 0;
	for( int j = 0; j < nrows - 1; j++ )
	{
		if( table.at(j+1,1,0) != table.at(j,1,0))
			m_ny++;
	}
	if( m_ny < 3 )
		return false;

	//we already know now many z values are in the table
	m_nz = nlayers;

	// Create 1D table for x values
	util::matrix_t<double> x_matrix( m_nx, 1, 0.0 );
	for( int j = 0; j < m_nx; j++ )
		x_matrix.at(j,0) = table.at( j, 0, 0);

	// Create 1D table for y values
	util::matrix_t<double> y_matrix( m_ny, 1, 0.0 );
	for( int j = 0; j < m_ny; j++ )
		y_matrix.at(j,0) = table.at( m_nx*j, 1, 0 );

	// Create 1D table for z values
	util::matrix_t<double> z_matrix( m_nz, 1, 0.0 );
	for( int j=0; j<m_nz; j++)
		z_matrix.at(j,0) = table.at( 0, 2, j);

	// Set up 1D interpolation class instances for x and y values
	int ind_var_index[1] = {0};
	int error_index = -99;
	if( !x_vals.Set_1D_Lookup_Table( x_matrix, ind_var_index, 1, error_index ) )
		return false;
	if( !y_vals.Set_1D_Lookup_Table( y_matrix, ind_var_index, 1, error_index ) )
		return false;
	if( !z_vals.Set_1D_Lookup_Table( z_matrix, ind_var_index, 1, error_index ) )
		return false;

	return true;
}

double Trilinear_Interp::trilinear_3D_interp( double x, double y, double z )
{
	
	/* 
	This algorithm assumes that the X and Y values are identical in each layer
	*/
	
	int i_x1 = x_vals.Get_Index( 0, x );
	int i_y1 = y_vals.Get_Index( 0, y );
	int i_z1 = z_vals.Get_Index( 0, z );

	int i_x2 = i_x1 + 1;
	int i_y2 = i_y1 + 1;
	int i_z2 = i_z1 + 1;

	int i1 = m_nx*i_y1 + i_x1;
	double x1 = m_3axis_table.at( i1, 0, i_z1 );
	double y1 = m_3axis_table.at( i1, 1, i_z1 );
	double p1 = m_3axis_table.at( i1, 3, i_z1 );
	double q1 = m_3axis_table.at( i1, 3, i_z2 );

	int i2 = m_nx*i_y2 + i_x1;
	double x2 = m_3axis_table.at( i2, 0, i_z1 );
	double y2 = m_3axis_table.at( i2, 1, i_z1 );
	double p2 = m_3axis_table.at( i2, 3, i_z1 );
	double q2 = m_3axis_table.at( i2, 3, i_z2 );

	int i3 = m_nx*i_y2 + i_x2;
	double x3 = m_3axis_table.at( i3, 0, i_z1 );
	double y3 = m_3axis_table.at( i3, 1, i_z1 );
	double p3 = m_3axis_table.at( i3, 3, i_z1 );
	double q3 = m_3axis_table.at( i3, 3, i_z2 );
	
	int i4 = m_nx*i_y1 + i_x2;
	double x4 = m_3axis_table.at( i4, 0, i_z1 );
	double y4 = m_3axis_table.at( i4, 1, i_z1 );
	double p4 = m_3axis_table.at( i4, 3, i_z1 );
	double q4 = m_3axis_table.at( i4, 3, i_z2 );
	
	double z1 = m_3axis_table.at( 0, 2, i_z1 );
	double z2 = m_3axis_table.at( 0, 2, i_z2 );


	double x_frac = (x - x1)/(x4 - x1);
	double y_frac = (y - y1)/(y2 - y1);
	double z_frac = (z - z1)/(z2 - z1);
	if(z2 - z1 == 0.) z_frac = 1.;	//Check for index separation

	double
		m1 = (1.0-x_frac)*(1.0-y_frac),
		m2 = (1.0-x_frac)*y_frac,
		m3 = x_frac*y_frac,
		m4 = x_frac*(1.0-y_frac);

	return (m1*p1 + m2*p2 + m3*p3 + m4*p4) * z_frac + (m1*q1 + m2*q2 + m3*q3 + m4*q4) * (1.0 - z_frac);
}

LUdcmp::LUdcmp(MatDoub &a) 
{
	n = (int)a.size(); 
	lu = a;
	aref = a;
	indx.resize(n);

    const double TINY=1.0e-40;
    int i,imax,j,k;
    double big,temp;
    VectDoub vv(n);
    d=1.0;
    for (i=0;i<n;i++) {
        big=0.0;
        for (j=0;j<n;j++)
            if ((temp=fabs(lu.at(i).at(j))) > big) big=temp;
        if (big == 0.0) throw("Singular matrix in LUdcmp");
        vv[i]=1.0/big;
    }
    for (k=0;k<n;k++) {
        big=0.0;
        for (i=k;i<n;i++) {
            temp=vv[i]*fabs(lu.at(i).at(k));
            if (temp > big) {
                big=temp;
                imax=i;
            }
        }
        if (k != imax) {
            for (j=0;j<n;j++) {
                temp=lu.at(imax).at(j);
                lu.at(imax).at(j)=lu.at(k).at(j);
                lu.at(k).at(j)=temp;
            }
            d = -d;
            vv[imax]=vv[k];
        }
        indx[k]=imax;
        if (lu.at(k).at(k) == 0.0) lu.at(k).at(k)=TINY;
        for (i=k+1;i<n;i++) {
            temp=lu.at(i).at(k) /= lu.at(k).at(k);
            for (j=k+1;j<n;j++)
                lu.at(i).at(j) -= temp*lu.at(k).at(j);
        }
    }
}

void LUdcmp::solve(VectDoub &b, VectDoub &x)
{
    int i,ii=0,ip,j;
    double sum;
    if (b.size() != static_cast<size_t>(n) ||
	x.size() != static_cast<size_t>(n))
        throw("LUdcmp::solve bad sizes");
    for (i=0;i<n;i++) x[i] = b[i];
    for (i=0;i<n;i++) {
        ip=indx[i];
        sum=x[ip];
        x[ip]=x[i];
        if (ii != 0)
            for (j=ii-1;j<i;j++) sum -= lu.at(i).at(j)*x[j];
        else if (sum != 0.0)
            ii=i+1;
        x[i]=sum;
    }
    for (i=n-1;i>=0;i--) {
        sum=x[i];
        for (j=i+1;j<n;j++) sum -= lu.at(i).at(j)*x[j];
        x[i]=sum/lu.at(i).at(i);
    }
}

void LUdcmp::solve(MatDoub &b, MatDoub &x)
{
    int i,j,m=(int)b.front().size();
    if (b.size() != static_cast<size_t>(n) ||
	x.size() != static_cast<size_t>(n) ||
	b.front().size() != x.front().size())
        throw("LUdcmp::solve bad sizes");
    VectDoub xx(n);
    for (j=0;j<m;j++) {
        for (i=0;i<n;i++) xx[i] = b.at(i).at(j);
        solve(xx,xx);
        for (i=0;i<n;i++) x.at(i).at(j) = xx[i];
    }
}

void LUdcmp::inverse(MatDoub &ainv)
{
    int i,j;
	ainv.resize(n, VectDoub(n));
	for (i=0;i<n;i++) {
        for (j=0;j<n;j++) ainv.at(i).at(j) = 0.;
        ainv.at(i).at(i) = 1.;
    }
    solve(ainv,ainv);
}

double LUdcmp::det()
{
    double dd = d;
    for (int i=0;i<n;i++) dd *= lu.at(i).at(i);
    return dd;
}

void LUdcmp::mprove(VectDoub &b, VectDoub &x)
{
    int i,j;
    VectDoub r(n);
    for (i=0;i<n;i++) {
        double sdp = -b[i];
        for (j=0;j<n;j++)
            sdp += (double)aref.at(i).at(j) * (double)x[j];
        r[i]=sdp;
    }
    solve(r,r);
    for (i=0;i<n;i++) x[i] -= r[i];
}

double Powvargram::SQR( const double a ) { return a*a; };  // a squared
	
Powvargram::Powvargram(){};

Powvargram::Powvargram(MatDoub &x, VectDoub &y, const double beta, const double nug)
{
	bet = beta;
	nugsq = nug*nug;

	int i,j,k,npt=(int)x.size(),ndim=(int)x.front().size();
	double rb,num=0.,denom=0.;
	for (i=0;i<npt;i++) {
		for (j=i+1;j<npt;j++) {
			rb = 0.;
			for (k=0;k<ndim;k++) rb += SQR(x.at(i).at(k)-x.at(j).at(k));
			rb = pow(rb,0.5*beta);
			num += rb*(0.5*SQR(y[i]-y[j]) - nugsq);
			denom += SQR(rb);
		}
	}
	alph = num/denom;
}

double Powvargram::operator() (const double r) const 
{
	return nugsq+alph*pow(r,bet);
}

double GaussMarkov::SQR( const double a ) { return a*a; };

GaussMarkov::GaussMarkov(MatDoub &xx, VectDoub &yy, Powvargram &vargram, const double *err)
{

	//create local copies as needed
	vgram = vargram;
	x = xx;
	npt = (int)xx.size();
	ndim = (int)xx.front().size();
	dstar.resize(npt+1);
	vstar.resize(npt+1);
	v.resize(npt+1,VectDoub(npt+1));
	y.resize(npt+1);
	yvi.resize(npt+1); 
	//---------------

	int i,j;
	for (i=0;i<npt;i++) {
		y[i] = yy[i];
		for (j=i;j<npt;j++) {
			v.at(i).at(j) = v.at(j).at(i) = vgram(rdist(&x.at(i),&x.at(j)));
		}
		v.at(i).at(npt) = v.at(npt).at(i) = 1.;
	}
	v.at(npt).at(npt) = y[npt] = 0.;
	if (err) for (i=0;i<npt;i++) v.at(i).at(i) -= SQR(err[i]);
	vi = new LUdcmp(v);
	vi->solve(y,yvi);
}

GaussMarkov::GaussMarkov(){
    vi = 0;  //initialize null
}

GaussMarkov::~GaussMarkov() { 
	if(vi != 0)
        delete vi; 
}

double GaussMarkov::interp(VectDoub &xstar) {
    int i;
    for (i=0;i<npt;i++) vstar[i] = vgram(rdist(&xstar,&x.at(i)));
    vstar[npt] = 1.;
    lastval = 0.;
    for (i=0;i<=npt;i++) lastval += yvi[i]*vstar[i];
    return lastval;
}

double GaussMarkov::interp(VectDoub &xstar, double &esterr) {
    lastval = interp(xstar);
    vi->solve(vstar,dstar);
    lasterr = 0;
    for (int i=0;i<=npt;i++) lasterr += dstar[i]*vstar[i];
    esterr = lasterr = sqrt(fmax(0.,lasterr));
    return lastval;
}

double GaussMarkov::rdist(VectDoub *x1, VectDoub *x2) {
    double d=0.;
    for (int i=0;i<ndim;i++) d += SQR(x1->at(i)-x2->at(i));
    return sqrt(d);
}
