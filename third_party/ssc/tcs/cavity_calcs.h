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

#ifndef __CAVITY_CALCS_
#define __CAVITY_CALCS_

//#include <shared/lib_util.h>
#include "lib_util.h"

/* class array4d
{
private:
	double * t_array;
	int m_n_i, m_n_j, m_n_k, m_n_l;
public:
	
	virtual ~array4d()
	{
		if (t_array) delete [] t_array;
	}

	array4d()
	{
		t_array = new double [1];
		m_n_i = m_n_j = m_n_k = m_n_l = 1;
	}
	
	array4d(int n_i, int n_j, int n_k, int n_l, const double & val)
	{
		t_array = NULL;
		if( n_i < 1 ) n_i = 1;
		if( n_j < 1 ) n_j = 1;
		if( n_k < 1 ) n_k = 1;
		if( n_l < 1 ) n_l = 1;
		resize(n_i, n_j, n_k, n_l);
		fill( val );
	}

	void fill( const double & val )
	{
		int ncells = m_n_i*m_n_j*m_n_k*m_n_l;
		for( int i = 0; i < ncells; i++ )
			t_array[i] = val;
	}

	void resize(int n_i, int n_j, int n_k, int n_l)
	{
		if( n_i < 1 || n_j < 1 || n_k < 1 || n_l < 1 ) return;
		if( n_i == m_n_i && n_j == m_n_j && n_k == m_n_k && n_l == m_n_l ) return;

		if( t_array ) delete [] t_array;
		t_array = new double [n_i*n_j*n_k*n_l];
		m_n_i = n_i;
		m_n_j = n_j;
		m_n_k = n_k;
		m_n_l = n_l;
	}

	inline const double & at( int n_i, int n_j, int n_k, int n_l ) const
	{
#ifdef _DEBUG
		VEC_ASSERT( n_i >= 0 && n_i < m_n_i && n_j >= 0 && n_j < m_n_j && n_k >= 0 && n_k < m_n_k && n_l >= 0 && n_l < m_n_l  );
#endif
		return t_array[ (m_n_l*m_n_k*m_n_j)*n_i + (m_n_l*m_n_k)*n_j + m_n_l*n_k + n_l ];
	}

	inline double & at( int n_i, int n_j, int n_k, int n_l )
	{
#ifdef _DEBUG
		VEC_ASSERT( n_i >= 0 && n_i < m_n_i && n_j >= 0 && n_j < m_n_j && n_k >= 0 && n_k < m_n_k && n_l >= 0 && n_l < m_n_l  );
#endif
		return t_array[ (m_n_l*m_n_k*m_n_j)*n_i + (m_n_l*m_n_k)*n_j + m_n_l*n_k + n_l ];
	}

}; */

class point
{
public:
	double x, y;
	
	point()	{x=0.; y=0.;}
	point( double x_in, double y_in )		{ x = x_in; y = y_in; }

	~point(){};
	
};

class polygon
{
private:
	point *p_points;
	int l_points;
	int *p_vertices;
	int l_vertices;
public:
	~polygon()
	{
		delete [] p_points;
		delete [] p_vertices;
	};

	void sizePolygon(int m_n_nodes)
	{
		p_points = new point[m_n_nodes];
		l_points = m_n_nodes;
		l_vertices = 2*l_points;
		p_vertices = new int[l_vertices];
	};

	point getPoint(int i)	{return p_points[i];}

	int getVertice(int i)	{return p_vertices[i];}

	int n_points( )		{return l_points;}

	int n_vertices( )	{return l_vertices;}
	
	void SetPoint(int i, point p)
	{
		p_points[i].x = p.x;
		p_points[i].y = p.y;
	};

	void SetVertices( int i, int v )
	{
		p_vertices[i] = v;
	};

	void SetVertices( int * const arr )
	{
		for( int i = 0; i < l_vertices; i++ )
			p_vertices[i] = arr[i];
	};
};

class Cavity_Calcs
{

public:

	static const int m_n_nodes = 5;	// Number of nodes per panel

	Cavity_Calcs();

	bool Define_Cavity( int n_rays, double h_rec, double r_rec, double rec_angle, double h_lip );

	void OuterPanel_Floor( double * F_AF );

	void InnerPanel_Floor( double * F_BF );

	void Lip_Ceiling( double & F_LCE );

	void Lip_Floor( double & F_LF );

	void Opening_Ceiling( double & F_OCE );

	void Opening_Floor( double & F_OF );

	void PanelViewFactors( util::matrix_t<double> & F_A_B, util::matrix_t<double> & F_A_C, util::matrix_t<double> & F_A_D, 
									 double * F_A_O, double * F_A_L, double * F_B_O, double * F_B_L );

	void GetGeometry( double & h_node, double & alpha, double & W_panel, double & W_aperture, double & z )
	{
		h_node = m_h_node; alpha = m_alpha; W_panel = m_W; W_aperture = m_c; z = m_z;
		return;
	}

	void ConvectionClausing1983( int n_panels, util::matrix_t<double> & T_s, double T_F, double T_CE,
					double T_L, double T_amb, double P_amb, double A_node,
					double Q_radiation_loss, double & q_convection_Clausing1983, double & h_F, double & h_avg,
					double & h_stag, double & T_stag, double & T_bulk, int & S);
	
	void ConvectionClausing1987( int n_panels, util::matrix_t<double> & T_s, double T_F, double T_amb,
					double P_amb, double & q_convection );

private:
		
	int m_n_rays;		// Number of rays used in Monte Carlo analysis
	double m_h_rec;		// Receiver panel height
	double m_r_rec;		// Radius of circle where common panel edges are located
	double m_rec_angle;	// [rad] Section of circle which is covered by panels
	double m_h_lip;		// Height of receiver cavity lip
	double m_h_node;	// height of one receiver panel NODE
	double m_alpha;		// angle coverage of the receiver circle by each of the 4 panels
	double m_W;			// panel width if panels have equal size
	double m_c;			// distance between the vertical aperture edges if the aperature is considered to be at the outer edges of the outer panels
	double m_z;			// distance between the aperature plane and the centerline of the panel circle
	
	//double m_A_node;	// Area of a panel node
	double m_A_f;		// Floor surface area
	double m_A_ce;		// Ceiling surface area
	double m_A_lip;		// Lip surface area
	double m_A_o;		// Aperture surface area

	bool Point_Is_Inside( point & p, polygon & pol );
	bool Ray_Intersects_Seg( point p0, point a0, point b0 );

	double calG( double x, double y, double eta, double xi_1, double xi_2, double theta );
	double G3D30( double x, double y, double eta, double xi_1, double xi_2, double alpha );
	double F3D_30( double x_1, double x_2, double y_1, double y_2, double eta_1, double eta_2,
					double z_1, double z_2, double theta );
};

#endif
