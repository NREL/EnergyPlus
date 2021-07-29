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

#include "cavity_calcs.h"
#include "htf_props.h"
#include "tcstype.h"
#include <ctime>
#include <float.h>
#include "sam_csp_util.h"
//#include <shared/lib_util.h>
#include "lib_util.h"

using namespace std;

Cavity_Calcs::Cavity_Calcs()
{
	m_n_rays = -1;
	m_h_rec = std::numeric_limits<double>::quiet_NaN(); 
	m_r_rec = std::numeric_limits<double>::quiet_NaN(); 
	m_rec_angle = std::numeric_limits<double>::quiet_NaN();
	m_h_lip = std::numeric_limits<double>::quiet_NaN();	
	m_h_node = std::numeric_limits<double>::quiet_NaN();
	m_alpha = std::numeric_limits<double>::quiet_NaN();	
	m_W = std::numeric_limits<double>::quiet_NaN();		
	m_c = std::numeric_limits<double>::quiet_NaN();		
	m_z = std::numeric_limits<double>::quiet_NaN();		
	//m_A_node = std::numeric_limits<double>::quiet_NaN();
	m_A_f = std::numeric_limits<double>::quiet_NaN();	
	m_A_ce = std::numeric_limits<double>::quiet_NaN();	
	m_A_lip = std::numeric_limits<double>::quiet_NaN();	
	m_A_o = std::numeric_limits<double>::quiet_NaN();
}

bool Cavity_Calcs::Define_Cavity( int n_rays, double h_rec, double r_rec, double rec_angle, double h_lip )
{
	// Cavity geometry conditions taken from Type 232

	m_n_rays = n_rays;
	m_h_rec = h_rec;
	m_r_rec = r_rec;
	m_rec_angle = rec_angle;
	m_h_lip = h_lip;
	m_h_node = h_rec / (double) m_n_nodes;
	m_alpha = m_rec_angle * 0.25;				// angle coverage of the receiver circle by each of the 4 panels
	m_W = 2.0*m_r_rec*sin(m_alpha/2.0);			// panel width if panels have equal size
	m_c = 2.0*m_r_rec*sin(CSP::pi-2.0*m_alpha);	// distance between the vertical aperture edges if the aperature is considered to be at the outer edges of the outer panels
	m_z = m_r_rec*cos(CSP::pi - 2.0*m_alpha);		// distance between the aperature plane and the centerline of the panel circle
		
	m_A_f = 2.0*m_W*r_rec*cos(m_alpha/2.0)+m_z*m_c;	// [m2] Floor surface area
	m_A_ce = m_A_f;									// [m2] Ceiling surface area
	m_A_lip = h_lip*m_c;							// [m2] Lip surface area
	m_A_o = (h_rec-h_lip)*m_c;						// [m2] Aperture surface area		

	// W_panel = m_W
	// W_aperture = m_c
	return false;
}

void Cavity_Calcs::OuterPanel_Floor( double * F_AF )
{ 
	// View factor from all the nodes of outer panel to the floor surface
	// Author: Lukas Feierabend
	// Converted from Fortran (sam_lf_pt_viewmod) to c++ in November 2012 by Ty Neises

	// Initialize and size floor
	const int poly_sides = 5;
	polygon floor_polygon; 
	floor_polygon.sizePolygon(poly_sides);
	
	// Define each point of floor polygon
	floor_polygon.SetPoint( 0, point( 0.0, 0.0 ) );
	floor_polygon.SetPoint( 1, point( m_W, 0.0 ) );
	floor_polygon.SetPoint( 2, point( m_W*(1.0+cos(m_alpha)), m_W*sin(m_alpha) ) );
	floor_polygon.SetPoint( 3, point( m_W+2.0*m_r_rec*sin(m_alpha)*cos(m_alpha+acos(m_r_rec*sin(m_alpha)/m_W)), 
								2.0*m_r_rec*sin(m_alpha)*sin(m_alpha+acos(m_r_rec*sin(m_alpha)/m_W)) ) );
	floor_polygon.SetPoint( 4, point( m_c*cos(3.0/2.0*m_alpha), m_c*sin(3.0/2.0*m_alpha) ));

	// Define connections of vertices
	int vertices[2*poly_sides] = {0,1, 1,2, 2,3, 3,4, 4,0};
	floor_polygon.SetVertices( vertices );

	// Initialize and size bounding box for floor
	polygon floor_box;
	floor_box.sizePolygon( 4 );

	// Define each point of floor bounding box
	floor_box.SetPoint( 0, floor_polygon.getPoint( 0 ) );
	floor_box.SetPoint( 1, point( floor_polygon.getPoint(2).x, 0.0 ) );
	floor_box.SetPoint( 2, point( floor_polygon.getPoint(2).x, floor_polygon.getPoint(4).y ) );
	floor_box.SetPoint( 3, point( 0.0, floor_polygon.getPoint(4).y ) );
	
	int hits[m_n_nodes];
	for( int i = 0; i < m_n_nodes; i++ )	hits[i] = 0;

	double Ptheta, Pphi, theta, phi, r1, r2, x, y, y_i, z_i;
	point p;
	int hit;
	int ray_counts;

	// Only set the seed once!
	srand( (unsigned int)time( (time_t*)NULL) ); //Seed with time on init

	for( ray_counts = 1; ray_counts <= m_n_rays; ray_counts++ )
	{
		
		for( int i = 0; i < m_n_nodes; i++ )
		{
			Ptheta	= rand() / (double) (RAND_MAX);
			Pphi	= rand() / (double) (RAND_MAX);
			theta	= asin(sqrt(Ptheta));	// determine the polar angle             
			phi		= Pphi*2.0*CSP::pi;		// determine the azimuthal angle
			
			// check if ray goes through floor plane
			if( (CSP::pi/2.0 <= phi && phi <= 3.0/2.0*CSP::pi) || theta == 0.0 )
				hit = 0;
			
			else	// randomly selected ray origin
			{
				r1	= rand() / (double) (RAND_MAX);
				r2	= rand() / (double) (RAND_MAX);
				x	= ((double) (i+1) - 1.0+r1)*m_h_node;
				y	= r2*m_W;

				// determine the intersection point
				y_i = y+tan(phi)*(m_h_rec - x);
				z_i = (m_h_rec-x)/(cos(phi)*tan(theta));

				if( y_i < floor_box.getPoint(0).x )	hit = 0;
				else if( y_i > floor_box.getPoint(1).x ) hit = 0;
				else if( z_i < floor_box.getPoint(0).y ) hit = 0;
				else if( z_i > floor_box.getPoint(3).y ) hit = 0;
				else
				{
					p = point(y_i, z_i);
					if( Point_Is_Inside( p, floor_polygon ) )	
						hit = 1;
					else										
						hit = 0;
				}
			}
		// Hit Counter
		if( hit == 1 )	hits[i] = hits[i] + 1;
		}
	}

	for( int i = 0; i < m_n_nodes; i++ )
		F_AF[ m_n_nodes - 1 - i ] = (double) hits[i] / (double) ray_counts;
		
	return;
}

void Cavity_Calcs::InnerPanel_Floor( double * F_BF  )
{
	// View factor from all the nodes of an inner panel to the floor surface
	// Author: Lukas Feierabend
	// Converted from Fortran (sam_lf_pt_viewmod) to c++ in November 2012 by Ty Neises

	// Initialize and size floor
	const int poly_sides = 5;
	polygon floor_polygon; 
	floor_polygon.sizePolygon(poly_sides);
	
	// Define each point of floor polygon
	floor_polygon.SetPoint( 0, point( -m_W*sin(CSP::pi/2.0 - m_alpha), m_W*cos(CSP::pi/2.0 - m_alpha) ) );
	floor_polygon.SetPoint( 1, point( 0.0, 0.0 ) );
	floor_polygon.SetPoint( 2, point( m_W, 0.0 ) );
	floor_polygon.SetPoint( 3, point( m_W*(1.0+cos(m_alpha)), m_W*sin(m_alpha) ) );
	floor_polygon.SetPoint( 4, point( m_W+2.0*m_r_rec*sin(m_alpha)*cos(m_alpha+acos(m_r_rec*sin(m_alpha)/m_W)),
										2.0*m_r_rec*sin(m_alpha)*sin(m_alpha+acos(m_r_rec*sin(m_alpha)/m_W)) ) );

	// Define connections of vertices
	int vertices[2*poly_sides] = {0,1, 1,2, 2,3, 3,4, 4,0};
	floor_polygon.SetVertices( vertices );

	// Initialize and size bounding box for floor
	polygon floor_box;
	floor_box.sizePolygon( 4 );

	// Define each point of floor bounding box
	floor_box.SetPoint( 0, point( floor_polygon.getPoint(0).x, 0.0 ) );
	floor_box.SetPoint( 1, point( floor_polygon.getPoint(3).x, 0.0 ) );
	floor_box.SetPoint( 2, point( floor_polygon.getPoint(3).x, floor_polygon.getPoint(4).y ) );
	floor_box.SetPoint( 3, point( floor_polygon.getPoint(0).x, floor_polygon.getPoint(4).y ) );

	int hits[m_n_nodes];
	for( int i = 0; i < m_n_nodes; i++ )	hits[i] = 0;

	double Ptheta, Pphi, theta, phi, r1, r2, x, y, y_i, z_i;
	point p;
	int hit;
	int ray_counts;

	// Only set the seed once!
	srand( (unsigned int)time( (time_t*)NULL) ); //Seed with time on init

	for( ray_counts = 1; ray_counts <= m_n_rays; ray_counts++ )
	{
		
		for( int i = 0; i < m_n_nodes; i++ )
		{
			Ptheta	= rand() / (double) (RAND_MAX);
			Pphi	= rand() / (double) (RAND_MAX);
			theta	= asin(sqrt(Ptheta));	// determine the polar angle             
			phi		= Pphi*2.0*CSP::pi;		// determine the azimuthal angle
			
			// check if ray goes through floor plane
			if( (CSP::pi/2.0 <= phi && phi <= 3.0/2.0*CSP::pi) || theta == 0.0 )
				hit = 0;
			
			else	// randomly selected ray origin
			{
				r1	= rand() / (double) (RAND_MAX);
				r2	= rand() / (double) (RAND_MAX);
				x	= ((double) (i+1) - 1.0+r1)*m_h_node;
				y	= r2*m_W;

				// determine the intersection point
				y_i = y+tan(phi)*(m_h_rec - x);
				z_i = (m_h_rec-x)/(cos(phi)*tan(theta));

				if( y_i < floor_box.getPoint(0).x )	hit = 0;
				else if( y_i > floor_box.getPoint(1).x ) hit = 0;
				else if( z_i < floor_box.getPoint(0).y ) hit = 0;
				else if( z_i > floor_box.getPoint(3).y ) hit = 0;
				else
				{
					p = point(y_i, z_i);
					if( Point_Is_Inside( p, floor_polygon ) )	
						hit = 1;
					else										
						hit = 0;
				}
			}
		// Hit Counter
		if( hit == 1 )	hits[i] = hits[i] + 1;
		}
	}

	for( int i = 0; i < m_n_nodes; i++ )
		F_BF[ m_n_nodes - 1 - i ] = (double) hits[i] / (double) ray_counts;

	return;
}

void Cavity_Calcs::Lip_Ceiling( double & F_LCE )
{
	// View factor from the receiver lip surface to the ceiling surface
	// Author: Lukas Feierabend
	// Converted from Fortran (sam_lf_pt_viewmod) to c++ in November 2012 by Ty Neises

	// Initialize and size floor
	const int poly_sides = 5;
	polygon floor_polygon; 
	floor_polygon.sizePolygon(poly_sides);
	
	// Define each point of floor polygon
	floor_polygon.SetPoint( 0, point( m_c, 0.0 ) );
	floor_polygon.SetPoint( 1, point( 0.5*m_c + m_r_rec*sin(m_alpha), m_z + m_r_rec*cos(m_alpha) ) );
	floor_polygon.SetPoint( 2, point( 0.5*m_c, m_z+m_r_rec ) );
	floor_polygon.SetPoint( 3, point( m_W*cos(1.5*m_alpha), m_W*sin(1.5*m_alpha) ) );
	floor_polygon.SetPoint( 4, point( 0.0, 0.0 ) );

	// Define connections of vertices
	int vertices[2*poly_sides] = {0,1, 1,2, 2,3, 3,4, 4,0};
	floor_polygon.SetVertices( vertices );

	// Initialize and size bounding box for floor
	polygon floor_box;
	floor_box.sizePolygon( 4 );

	// Define each point of floor bounding box
	floor_box.SetPoint( 0, point( 0.0, 0.0 ) );
	floor_box.SetPoint( 1, point( m_c, 0.0 ) );
	floor_box.SetPoint( 2, point( m_c, m_z + m_r_rec ) );
	floor_box.SetPoint( 3, point( 0.0, m_z + m_r_rec ) );
	
	int hits = 0;

	double Ptheta, Pphi, theta, phi, r1, r2, x, y, y_i, z_i;
	point p;
	int hit;
	int ray_counts;

	// Only set the seed once!
	srand( (unsigned int)time( (time_t*)NULL) ); //Seed with time on init

	for( ray_counts = 1; ray_counts <= m_n_rays; ray_counts++ )
	{		
		Ptheta	= rand() / (double) (RAND_MAX);
		Pphi	= rand() / (double) (RAND_MAX);
		theta	= asin(sqrt(Ptheta));	// determine the polar angle             
		phi		= Pphi*2.0*CSP::pi;		// determine the azimuthal angle
			
		// check if ray goes through floor plane
		if( (CSP::pi/2.0 <= phi && phi <= 3.0/2.0*CSP::pi) || theta == 0.0 )
			hit = 0;
			
		else	// randomly selected ray origin
		{
			r1	= rand() / (double) (RAND_MAX);
			r2	= rand() / (double) (RAND_MAX);
			x	= r1*m_h_lip;
			y	= r2*m_c;

			// determine the intersection point
			y_i = y+tan(phi)*(m_h_lip - x);
			z_i = (m_h_lip-x)/(cos(phi)*tan(theta));

			if( y_i < floor_box.getPoint(0).x )	hit = 0;
			else if( y_i > floor_box.getPoint(1).x ) hit = 0;
			else if( z_i < floor_box.getPoint(0).y ) hit = 0;
			else if( z_i > floor_box.getPoint(3).y ) hit = 0;
			else
			{
				p = point(y_i, z_i);
				if( Point_Is_Inside( p, floor_polygon ) )	
					hit = 1;
				else										
					hit = 0;
			}
		}
		// Hit Counter
		if( hit == 1 )	hits = hits + 1;
	}

	for( int i = 0; i < m_n_nodes; i++ )
		F_LCE = (double) hits / (double) ray_counts;

	return;
}

void Cavity_Calcs::Lip_Floor( double & F_LF )
{
	// View factor from the receiver lip surface to the floor surface
	// Author: Lukas Feierabend
	// Converted from Fortran (sam_lf_pt_viewmod) to c++ in November 2012 by Ty Neises

	// Initialize and size floor
	const int poly_sides = 5;
	polygon floor_polygon; 
	floor_polygon.sizePolygon(poly_sides);
	
	// Define each point of floor polygon
	floor_polygon.SetPoint( 0, point( m_c, 0.0 ) );
	floor_polygon.SetPoint( 1, point( 0.5*m_c + m_r_rec*sin(m_alpha), m_z + m_r_rec*cos(m_alpha) ) );
	floor_polygon.SetPoint( 2, point( 0.5*m_c, m_z+m_r_rec ) );
	floor_polygon.SetPoint( 3, point( m_W*cos(1.5*m_alpha), m_W*sin(1.5*m_alpha) ) );
	floor_polygon.SetPoint( 4, point( 0.0, 0.0 ) );

	// Define connections of vertices
	int vertices[2*poly_sides] = {0,1, 1,2, 2,3, 3,4, 4,0};
	floor_polygon.SetVertices( vertices );

	// Initialize and size bounding box for floor
	polygon floor_box;
	floor_box.sizePolygon( 4 );

	// Define each point of floor bounding box
	floor_box.SetPoint( 0, point( 0.0, 0.0 ) );
	floor_box.SetPoint( 1, point( m_c, 0.0 ) );
	floor_box.SetPoint( 2, point( m_c, m_z + m_r_rec ) );
	floor_box.SetPoint( 3, point( 0.0, m_z + m_r_rec ) );

	int hits = 0;

	double Ptheta, Pphi, theta, phi, r1, r2, x, y, y_i, z_i;
	point p;
	int hit;
	int ray_counts;

	// Only set the seed once!
	srand( (unsigned int)time( (time_t*)NULL) ); //Seed with time on init

	for( ray_counts = 1; ray_counts <= m_n_rays; ray_counts++ )
	{		
		Ptheta	= rand() / (double) (RAND_MAX);
		Pphi	= rand() / (double) (RAND_MAX);
		theta	= asin(sqrt(Ptheta));	// determine the polar angle             
		phi		= Pphi*2.0*CSP::pi;		// determine the azimuthal angle
			
		// check if ray goes through floor plane
		if( (CSP::pi/2.0 <= phi && phi <= 3.0/2.0*CSP::pi) || theta == 0.0 )
			hit = 0;
			
		else	// randomly selected ray origin
		{
			r1	= rand() / (double) (RAND_MAX);
			r2	= rand() / (double) (RAND_MAX);
			x	= r1*m_h_lip;
			y	= r2*m_c;

			// determine the intersection point
			y_i = y+tan(phi)*(m_h_rec - x);
			z_i = (m_h_rec-x)/(cos(phi)*tan(theta));

			if( y_i < floor_box.getPoint(0).x )	hit = 0;
			else if( y_i > floor_box.getPoint(1).x ) hit = 0;
			else if( z_i < floor_box.getPoint(0).y ) hit = 0;
			else if( z_i > floor_box.getPoint(3).y ) hit = 0;
			else
			{
				p = point(y_i, z_i);
				if( Point_Is_Inside( p, floor_polygon ) )	
					hit = 1;
				else										
					hit = 0;
			}
		}
		// Hit Counter
		if( hit == 1 )	hits = hits + 1;
	}

	for( int i = 0; i < m_n_nodes; i++ )
		F_LF = (double) hits / (double) ray_counts;

	return;
}

void Cavity_Calcs::Opening_Ceiling( double & F_OCE )
{
	// View factor from the receiver aperature surface to the ceiling surface
	// Author: Lukas Feierabend
	// Converted from Fortran (sam_lf_pt_viewmod) to c++ in November 2012 by Ty Neises
	
	// Initialize and size floor
	const int poly_sides = 5;
	polygon floor_polygon; 
	floor_polygon.sizePolygon(poly_sides);
	
	// Define each point of floor polygon
	floor_polygon.SetPoint( 0, point( m_c, 0.0 ) );
	floor_polygon.SetPoint( 1, point( 0.5*m_c + m_r_rec*sin(m_alpha), m_z + m_r_rec*cos(m_alpha) ) );
	floor_polygon.SetPoint( 2, point( 0.5*m_c, m_z+m_r_rec ) );
	floor_polygon.SetPoint( 3, point( m_W*cos(1.5*m_alpha), m_W*sin(1.5*m_alpha) ) );
	floor_polygon.SetPoint( 4, point( 0.0, 0.0 ) );

	// Define connections of vertices
	int vertices[2*poly_sides] = {0,1, 1,2, 2,3, 3,4, 4,0};
	floor_polygon.SetVertices( vertices );

	// Initialize and size bounding box for floor
	polygon floor_box;
	floor_box.sizePolygon( 4 );

	// Define each point of floor bounding box
	floor_box.SetPoint( 0, point( 0.0, 0.0 ) );
	floor_box.SetPoint( 1, point( m_c, 0.0 ) );
	floor_box.SetPoint( 2, point( m_c, m_z + m_r_rec ) );
	floor_box.SetPoint( 3, point( 0.0, m_z + m_r_rec ) );

	int hits = 0;

	double Ptheta, Pphi, theta, phi, r1, r2, x, y, y_i, z_i;
	point p;
	int hit;
	int ray_counts;

	// Only set the seed once!
	srand( (unsigned int)time( (time_t*)NULL) ); //Seed with time on init

	for( ray_counts = 1; ray_counts <= m_n_rays; ray_counts++ )
	{		
		Ptheta	= rand() / (double) (RAND_MAX);
		Pphi	= rand() / (double) (RAND_MAX);
		theta	= asin(sqrt(Ptheta));	// determine the polar angle             
		phi		= Pphi*2.0*CSP::pi;		// determine the azimuthal angle
			
		// check if ray goes through floor plane
		if( (CSP::pi/2.0 <= phi && phi <= 3.0/2.0*CSP::pi) || theta == 0.0 )
			hit = 0;
			
		else	// randomly selected ray origin
		{
			r1	= rand() / (double) (RAND_MAX);
			r2	= rand() / (double) (RAND_MAX);
			x	= r1*(m_h_rec - m_h_lip);
			y	= r2*m_c;

			// determine the intersection point
			y_i = y+tan(phi)*(m_h_rec - x);
			z_i = (m_h_rec-x)/(cos(phi)*tan(theta));

			if( y_i < floor_box.getPoint(0).x )	hit = 0;
			else if( y_i > floor_box.getPoint(1).x ) hit = 0;
			else if( z_i < floor_box.getPoint(0).y ) hit = 0;
			else if( z_i > floor_box.getPoint(3).y ) hit = 0;
			else
			{
				p = point(y_i, z_i);
				if( Point_Is_Inside( p, floor_polygon ) )	
					hit = 1;
				else										
					hit = 0;
			}
		}
		// Hit Counter
		if( hit == 1 )	hits = hits + 1;
	}

	for( int i = 0; i < m_n_nodes; i++ )
		F_OCE = (double) hits / (double) ray_counts;

	return;
}

void Cavity_Calcs::Opening_Floor( double & F_OF )
{
	// View factor from the receiver aperature surface to the floor surface
	// Author: Lukas Feierabend
	// Converted from Fortran (sam_lf_pt_viewmod) to c++ in November 2012 by Ty Neises

	// Initialize and size floor
	const int poly_sides = 5;
	polygon floor_polygon; 
	floor_polygon.sizePolygon(poly_sides);
	
	// Define each point of floor polygon
	floor_polygon.SetPoint( 0, point( m_c, 0.0 ) );
	floor_polygon.SetPoint( 1, point( 0.5*m_c + m_r_rec*sin(m_alpha), m_z + m_r_rec*cos(m_alpha) ) );
	floor_polygon.SetPoint( 2, point( 0.5*m_c, m_z+m_r_rec ) );
	floor_polygon.SetPoint( 3, point( m_W*cos(1.5*m_alpha), m_W*sin(1.5*m_alpha) ) );
	floor_polygon.SetPoint( 4, point( 0.0, 0.0 ) );

	// Define connections of vertices
	int vertices[2*poly_sides] = {0,1, 1,2, 2,3, 3,4, 4,0};
	floor_polygon.SetVertices( vertices );

	// Initialize and size bounding box for floor
	polygon floor_box;
	floor_box.sizePolygon( 4 );

	// Define each point of floor bounding box
	floor_box.SetPoint( 0, point( 0.0, 0.0 ) );
	floor_box.SetPoint( 1, point( m_c, 0.0 ) );
	floor_box.SetPoint( 2, point( m_c, m_z + m_r_rec ) );
	floor_box.SetPoint( 3, point( 0.0, m_z + m_r_rec ) );

	int hits = 0;

	double Ptheta, Pphi, theta, phi, r1, r2, x, y, y_i, z_i;
	point p;
	int hit;
	int ray_counts;

	// Only set the seed once!
	srand( (unsigned int)time( (time_t*)NULL) ); //Seed with time on init

	for( ray_counts = 1; ray_counts <= m_n_rays; ray_counts++ )
	{		
		Ptheta	= rand() / (double) (RAND_MAX);
		Pphi	= rand() / (double) (RAND_MAX);
		theta	= asin(sqrt(Ptheta));	// determine the polar angle             
		phi		= Pphi*2.0*CSP::pi;		// determine the azimuthal angle
			
		// check if ray goes through floor plane
		if( (CSP::pi/2.0 <= phi && phi <= 3.0/2.0*CSP::pi) || theta == 0.0 )
			hit = 0;
			
		else	// randomly selected ray origin
		{
			r1	= rand() / (double) (RAND_MAX);
			r2	= rand() / (double) (RAND_MAX);
			x	= m_h_lip + r1*(m_h_rec - m_h_lip);
			y	= r2*m_c;

			// determine the intersection point
			y_i = y+tan(phi)*(m_h_rec - x);
			z_i = (m_h_rec-x)/(cos(phi)*tan(theta));

			if( y_i < floor_box.getPoint(0).x )	hit = 0;
			else if( y_i > floor_box.getPoint(1).x ) hit = 0;
			else if( z_i < floor_box.getPoint(0).y ) hit = 0;
			else if( z_i > floor_box.getPoint(3).y ) hit = 0;
			else
			{
				p = point(y_i, z_i);
				if( Point_Is_Inside( p, floor_polygon ) )	
					hit = 1;
				else										
					hit = 0;
			}
		}
		// Hit Counter
		if( hit == 1 )	hits = hits + 1;
	}

	for( int i = 0; i < m_n_nodes; i++ )
		F_OF = (double) hits / (double) ray_counts;


	return;
}

void Cavity_Calcs::PanelViewFactors( util::matrix_t<double> & F_A_B, util::matrix_t<double> & F_A_C, util::matrix_t<double> & F_A_D, 
									 double * F_A_O, double * F_A_L, double * F_B_O, double * F_B_L )
{
/*
Author: Lukas Feierabend
Converted from Fortran (sam_lf_pt_viewmod) to c++ in November 2012 by Ty Neises

!This programs returns the view factors from each panel 1-4, counted from one outer panel to the other side (assuming symmetric setup), to its surroundings.
!The calculations can only be made for receivers with four panels which are of equal size.

!----------------------------------------------------------------------------------------------------------------------
!-outputs
!   * F_A_B     |   View factors between nodes of panel A and nodes of panel B
!   * F_A_C     |   View factors between nodes of panel A and nodes of panel C
!   * F_A_D     |   View factors between nodes of panel A and nodes of panel D
!   * F_A_O     |   View factors between nodes of panel A and opening (O)
!   * F_A_L     |   View factors between nodes of panel A and lip (L)
!   * F_B_O     |   View factors between nodes of panel B and opening (O)
!   * F_B_L     |   View factors between nodes of panel B and opening (L)
!---------------------------------------------------------------------------------------------------------------------- */

	//theta = m_rec_angle
	//R = m_r_rec
	//N = m_n_nodes

	double phi_1 = CSP::pi - m_alpha;		// angle between two adjacent panels
	double phi_2 = CSP::pi - 2*m_alpha;    // angle between two non-adjacent panels with one panel in between
	double phi_3 = CSP::pi - 3*m_alpha;    // angle between two non-adjacent panels with two panels in between
	double phi_4 = (m_rec_angle - m_alpha)/2.0;	// angle between the aperture plane and an outer panel (1)
	double phi_5 = m_alpha/2.0;			// angle between the aperture plane and an inner panel (2)
	double a_1 = m_W/(2.0*cos(m_alpha));	// distance from inner panel edge to intersection edge of both panel planes for view factor calculation of panels with one panel in between
	double a_2 = m_r_rec*sin(m_alpha)/sin((CSP::pi-3.0*m_alpha)/2.0);	// distance from inner panel edge to intersection edge of both panel planes for view factor calculation of panels with two panels in between
	double a_3 = (m_r_rec+m_z)/sin(m_alpha/2.0)-m_W;	// distance from panel 2 to intersection edge of panel planes 2 and a
	double a_4 = (m_r_rec+m_z)/tan(m_alpha/2.0)-m_c/2.0;	// distance from panel a to intersection edge of panel plane 2 and a

	for(int i = 0; i < m_n_nodes; i++)
	{
		//View factors between nodes of panel A and nodes of panel B
		F_A_B.at(i, 0) = F3D_30(0.0,m_W,0.0,m_h_node,double(i)*m_h_node,(i+1)*m_h_node,0.0,m_W,phi_1);
		
		//View factors between nodes of panel A and nodes of panel C
		F_A_C.at(i, 0) = F3D_30(a_1,a_1+m_W,0.0,m_h_node,double(i)*m_h_node,(i+1)*m_h_node,a_1,a_1+m_W,phi_2);

		//View factors between nodes of panel A and nodes of panel D
		F_A_D.at(i, 0) = F3D_30(a_2,a_2+m_W,0.0,m_h_node,double(i)*m_h_node,(i+1)*m_h_node,a_2,a_2+m_W,phi_3);
		
		// View factors between nodes of panel A and opening (O)
		F_A_O[m_n_nodes-i-1] = F3D_30(0.0,m_W,double(i)*m_h_node,(i+1)*m_h_node,m_h_lip,m_h_rec,0.0,m_c,phi_4);
		
		// View factors between nodes of panel A and lip (m_h_lip)
		F_A_L[m_n_nodes-i-1] = F3D_30(0.0,m_W,double(i)*m_h_node,(i+1)*m_h_node,0.0,m_h_lip,0.0,m_c,phi_4);
		
		// View factors between nodes of panel B and opening (O)
		F_B_O[m_n_nodes-i-1] = F3D_30(a_3,a_3+m_W,double(i)*m_h_node,(i+1)*m_h_node,m_h_lip,m_h_rec,a_4,a_4+m_c,phi_5);
		
		// View factors between nodes of panel B and opening (m_h_lip)
		F_B_L[m_n_nodes-i-1] = F3D_30(a_3,a_3+m_W,double(i)*m_h_node,(i+1)*m_h_node,0.0,m_h_lip,a_4,a_4+m_c,phi_5);
	}
	
	return;
}

double Cavity_Calcs::F3D_30( double x_1, double x_2, double y_1, double y_2, double eta_1, double eta_2,
								double z_1, double z_2, double theta )
{
	/*!Function for calculating the view factor for rectangles with parallel and perpendicular edges and 
	!with an arbitrary angle theta between their intersecting planes, the rectangles can't be flush in 
	!the direction of the intersection line.
	!Reference: http://www.me.utexas.edu/~howell/sectionc/C-17.html. 
	Author: Lukas Feierabend
	Converted from Fortran (sam_lf_pt_viewmod) to c++ in November 2012 by Ty Neises	*/

	double G_1_1_1 = G3D30(x_1,y_1,eta_1,z_1,z_2,theta);
    double G_1_1_2 = G3D30(x_1,y_1,eta_2,z_1,z_2,theta);
    double G_1_2_1 = G3D30(x_1,y_2,eta_1,z_1,z_2,theta);
    double G_1_2_2 = G3D30(x_1,y_2,eta_2,z_1,z_2,theta);
    double G_2_1_1 = G3D30(x_2,y_1,eta_1,z_1,z_2,theta);
    double G_2_1_2 = G3D30(x_2,y_1,eta_2,z_1,z_2,theta);
    double G_2_2_1 = G3D30(x_2,y_2,eta_1,z_1,z_2,theta);
    double G_2_2_2 = G3D30(x_2,y_2,eta_2,z_1,z_2,theta);

	return (-G_1_1_1+G_2_1_1+G_1_2_1-G_2_2_1+G_1_1_2-G_2_1_2-G_1_2_2+G_2_2_2)/((x_2-x_1)*(y_2-y_1));
}


double Cavity_Calcs::G3D30( double x, double y, double eta, double xi_1, double xi_2, double alpha )
{
	/* !Function for calculating the view factor for rectangles with parallel and perpendicular edges and 
	!with an arbitrary angle theta between their intersecting planes, the rectangles can't be flush in 
	!the direction of the intersection line.
	!Reference: http://www.me.utexas.edu/~howell/sectionc/C-17.html. 
	Author: Lukas Feierabend
	Converted from Fortran (sam_lf_pt_viewmod) to c++ in November 2012 by Ty Neises	*/

	if( y==eta )	y = y + 1.E-6;
	if( x==0 && xi_1==0 )	x = 1.E-6;

	double G3D30 = calG( x, y, eta, xi_1, xi_2, alpha );

	return G3D30;
}

double Cavity_Calcs::calG( double x, double y, double eta, double xi_1, double xi_2, double theta )
{
	/* This function integrates the expression for G using a flexible step size. The step size for the next step is based on 
    !the magnitude of the second derivative G''. This is evaluated by considering the difference between the predicted 
    !position of the current value based on the previous 2 values and the actual value obtained from the equation. 
	Author: Lukas Feierabend
	Converted from Fortran (sam_lf_pt_viewmod) to c++ in November 2012 by Ty Neises	*/

	bool recalc = false;
	double tol = 1.E-6;
	double min_step = 1.E-9*(xi_2 - xi_1);

	double step = min_step;

	double xi = xi_1; double G = 0.0; double xi0 = xi;
	double v1, v2, v3, dx32, dx21, dv32, vexp, err;

	int i = 0, n = 0;

	do
	{
        i=i+1; n=n+1;
        
        // Do the following if we aren't recalculating the step    
        if( !recalc && i > 1 )
		{
            v3 = v2;	// v3 is equal to the second to last calculated value of v1
            v2 = v1;	// v2 is equal to the last calculated value of v1
            dx32 = dx21;// The step size from the previous iteration
		}
        dx21 = step;	// The current step size
        
        // Evaluate the expression at xi
        v1 = ((x-xi*cos(theta))*cos(theta)-xi*pow(sin(theta),2)) / 
			 (pow((pow(x,2)-2*x*xi*cos(theta)+pow(xi,2)),0.5) *
			 pow(sin(theta),2))*atan((eta-y)/pow((pow(x,2)-2*x*xi*cos(theta)+pow(xi,2)),0.5)) + 
			 cos(theta)/((eta-y)*pow(sin(theta),2)) *
			 (pow( ((pow(xi,2)*pow(sin(theta),2))+pow((eta-y),2)), 0.5 ) * 
			 atan((x-xi*cos(theta))/pow( (pow(xi,2)*pow(sin(theta),2)+pow((eta-y),2)),0.5)) - 
			 xi*sin(theta)*atan((x-xi*cos(theta))/(sin(theta)))) + 
             xi/(2*(eta-y))*log((pow(x,2)-2*x*xi*cos(theta)+pow(xi,2) + 
			 pow((eta-y),2))/(pow(x,2)-2*x*xi*cos(theta)+pow(xi,2)));

		/*v1 = ((x-xi*cos(theta))*cos(theta)-xi*(sin(theta))**2) / 
			 ((x**2-2*x*xi*cos(theta)+xi**2)**(.5) *
             (sin(theta))**2)*atan((eta-y)/(x**2-2*x*xi*cos(theta)+xi**2)**(.5)) + 
			 cos(theta)/((eta-y)*(sin(theta))**2) *
			 ((xi**2*(sin(theta))**2+(eta-y)**2)**(.5) * 
			 atan((x-xi*cos(theta))/(xi**2*(sin(theta))**2+(eta-y)**2)**(.5)) - 
			 xi*sin(theta)*atan((x-xi*cos(theta))/(sin(theta)))) +
             
			 xi/(2*(eta-y))*log((x**2-2*x*xi*cos(theta)+xi**2 + 
			 
			 (eta-y)**2)/(x**2-2*x*xi*cos(theta)+xi**2))*/
        
        // Do this only on the first iteration
        if( i==1 )
		{
            v2 = v1;
            v3 = v1;
            dx32 = step;
            dx21 = step;
		}
        
        // Calculate the slope of the previous 2 points
        dv32 = (v2 - v3)/dx32;
        // Calculate the predicted current point v1 based on the previous slope
        if( i>2 )	vexp = v2 + dx21*dv32;
        else		vexp = v1;

        // Determine the relative error in the predicted value
        err = fabs( (vexp-v1)/v1 )/tol;
        
        // If the error exceeds a certain value, we need to recalculate
		if( err > 1.0  && step > min_step )
		{
            // Adjust the step size. If the error is greater than 1 (equal to the tolerance), then we need to decrease the step size. 
			if( i>2 )	step = max( step*pow(10,1.0-err), min_step );
			recalc = true;
			xi = min(xi0+step,xi_2);	// go back and reset the current xi based on the previous xi + the adjusted step
            i = i-1;
		}
        else
		{
            // Integrate based on the current iteration. We have to average the value of 'v' over the entire step
            G = G+(v1+v2)/2.0*step;
            
            // Adjust the step size for the next iteration. If the error is greater than 0, then we need to decrease the step size. 
            if( i>2 )	step = max(step*pow(10,(1.0-err)), min_step);        
            recalc = false;
        
            if( xi>=xi_2 ) break;
        
            xi0 = xi;	// keep track of the last xi position
            xi = min(xi+step,xi_2);
		}
        
	}	while(true);
    
    // Use the integral to evaluate calG
    double calG = -(eta-y)*pow(sin(theta),2)/(2*CSP::pi)*G;

	return calG;
}

bool Cavity_Calcs::Ray_Intersects_Seg( point p, point a0, point b0 )
{
	/* Author: Lukas Feierabend
	Converted from Fortran (sam_lf_pt_viewmod) to c++ in November 2012 by Ty Neises	*/

	point a, b;
	double eps = 0.00001;
	if( a0.y > b0.y )
	{
		b = a0;
		a = b0;
	}
	else
	{
		a = a0;
		b = b0;
	}

	if( p.y == a.y || p.y == b.y )	p.y = p.y + eps;
	if( p.y > b.y || p.y < a.y )	return false;
	if( p.x > max(a.x, b.x) )		return false;

	double m_red, m_blue;

	if( p.x < min(a.x, b.x) )
		return true;
	else
	{
		if( fabs(a.x - b.x) > DBL_MIN )	m_red = (b.y - a.y)/(b.x - a.x);
		else							m_red = DBL_MAX;

		if( fabs(a.x - p.x) > DBL_MIN )	m_blue = (p.y - a.y)/(p.x - a.x);
		else							m_blue = DBL_MAX;

		if( m_blue >= m_red )	return true;
		else					return false;
	}
}

bool Cavity_Calcs::Point_Is_Inside( point & p, polygon & pol )
{
	/* Author: Lukas Feierabend
	Converted from Fortran (sam_lf_pt_viewmod) to c++ in November 2012 by Ty Neises	*/

	int count = 0;
	int index_a, index_b;
	int size_pol = pol.n_vertices();
	for( int i = 0; i < size_pol; i = i + 2 )
	{
		index_a = pol.getVertice( i );
		index_b = pol.getVertice( i+1 );

		if( Ray_Intersects_Seg( p, pol.getPoint(index_a), pol.getPoint(index_b) ) )	count = count + 1;
	}

	if( count % 2 == 0 )	return false;
	else					return true;
}

void Cavity_Calcs::ConvectionClausing1983( int n_panels, util::matrix_t<double> & T_s, double T_F, double T_CE,
					double T_L, double T_amb, double P_amb, double A_node,
					double Q_radiation_loss, double & q_convection_Clausing1983, double & h_F, double & h_avg,
					double & h_stag, double & T_stag, double & T_bulk, int & S)
{
	/* Author: Soenke Teichel
	Converted from Fortran (sam_lf_pt_viewmod) to c++ in November 2012 by Ty Neises	*/

	/***********************************************************************************
	!This subroutine calculates the total convective heat losses from the receiver
	!with the correlations presented in Clausing (1983).
	!  The inputs are:
	!    - N_nodes -> number of vertical nodes per receiver panel [-]
	!    - N_panels -> number of receiver panels [-]
	!    - T_F -> the temperature of the receiver FLOOR [K]
	!    - T_amb -> ambient temperature [K]
	!    - P_amb -> ambient pressure [Pa]
	!    - H_rec -> internal receiver height [m]
	!    - H_lip -> height of the upper lip [m]
	!    - R_rec -> internal receiver radius [m]
	!    - alpha -> segment angle [rad]
	!    - W_panel -> width of one receiver panel [m]
	!    - A_node -> area of the active receiver surfaces [m2]
	!    - A_F -> area of the FLOOR surface [m2]
	!    - A_O -> area of the aperture [m2]
	!  The outputs are:
	!    - q_convection_Clausing1983 -> the total convective heat losses through the aperture [W]
	!**********************************************************************************
	!ST - from EES
	W = 2.*R_rec*SIN(alpha/2.)		!panel width if panels have equal size
	c = 2.*R_rec*SIN(PI-2.*alpha)    !distance between the vertical aperture edges if the aperature is considered to be at the outer edges of the outer panels */
	
	double grav = 9.81;

	S = (int) ceil( m_h_lip / (m_h_rec/m_n_nodes) );		// Number of (twn: nodes per panel?) panels that are influenced by the stagnant zone

	double T_F_calc = T_F;

	double T_sum_avg = 0.0;	
	for( int i = 0; i < m_n_nodes - S; i++ )
		for( int j = 0; j < n_panels; j++ )
			T_sum_avg += T_s.at(i,j);

	double T_sum_stag = 0.0;
	for( int i = m_n_nodes - S; i < m_n_nodes; i++ )
		for( int j = 0; j < n_panels; j++ )
			T_sum_stag += T_s.at(i,j);

	double T_avg = T_sum_avg / (n_panels*(m_n_nodes - S));
	T_stag = T_sum_stag / 8.0;
	T_stag = (T_sum_stag + T_CE + T_L)/(double)(S*n_panels + 2);
	
	if( T_F_calc/T_amb > 2.6 )
		T_F_calc = 2.6*T_amb;

	if( T_stag/T_amb > 2.6 )
		T_stag = 2.6*T_amb;

	if( T_avg/T_amb > 2.6 )
		T_avg = 2.6*T_amb;

	// Ambient properties
	double beta_amb = 1.0/T_amb;
	HTFProperties air;
	air.SetFluid( HTFProperties::Air );
	double rho_amb, c_p_amb;
	rho_amb = air.dens( T_amb, P_amb );
	c_p_amb = air.Cp( T_amb )*1000.0;

	double v = 0.0;			// Free stream velocity - Clausing 1983 - forced convection?
	double error = 9999.9;
	int iter = 0;
	double T_c = T_avg;

	double q_convection_Clausing1983X = Q_radiation_loss;
	q_convection_Clausing1983 = 5.0;

	double T_film_F, T_film_stag, T_film_avg, beta_F, beta_stag, beta_avg, k_F, k_stag, k_avg, c_p_F, c_p_stag, c_p_avg, mu_F, mu_stag, mu_avg;
	double Pr_F, Pr_stag, Pr_avg, rho_F, rho_stag, rho_avg, Gr_F, Gr_stag, Gr_avg, Ra_F, Ra_stag, Ra_avg, Nusselt_F, Nusselt_stag, Nusselt_avg;
	double q_conv_1, q_conv_2, q_conv_3, q_conv_4, v_b, v_a;
	while( error > 1.E-12 && iter < 50 )
	{
		iter++;
	    error=fabs((q_convection_Clausing1983X-q_convection_Clausing1983)/q_convection_Clausing1983);
	    q_convection_Clausing1983=q_convection_Clausing1983X;
	 
	    T_bulk=(T_c+T_amb)/2.0;
	
	    // Film temperature for the property evaluation
	    T_film_F	= (T_F_calc+T_bulk)/2.0;	// "Film temperature at the floor"          
	    T_film_stag = (T_stag+T_bulk)/2.0;		// "Film temperature in the stagnant zone"
	    T_film_avg	= (T_avg+T_bulk)/2.0;		// "Average film temperature"
	    // Property evaluation at different locations in the cavity - Floor(F); Stagnant zone(s); Average(avg)
	
	    // Volume expansion coefficient [1/K]
	    beta_F		= 1.0/T_film_F;
	    beta_stag	= 1.0/T_film_stag;
	    beta_avg	= 1.0/T_film_avg;
	
	    // Conductivity [W/m-K]
		k_F		= air.cond( T_film_F );
		k_stag	= air.cond( T_film_stag );
		k_avg	= air.cond( T_film_avg );
	 
	    // Specific heat [J/kg-K]
		c_p_F = air.Cp( T_film_F )*1000.0;
		c_p_stag = air.Cp( T_film_stag )*1000.0;
		c_p_avg = air.Cp( T_film_avg )*1000.0;
	
	    // Viscosity [Pa-s]
		mu_F = air.visc( T_film_F );
		mu_stag = air.visc( T_film_stag );
		mu_avg = air.visc( T_film_avg );
	
	    // Prandtl number
	    Pr_F = (c_p_F*mu_F)/k_F;
	    Pr_stag = (c_p_stag*mu_stag)/k_stag;
	    Pr_avg = (c_p_avg*mu_avg)/k_avg;
	 
	    // Density [kg/m3]
		rho_F = air.dens( T_film_F, P_amb );
		rho_stag = air.dens( T_film_stag, P_amb );
		rho_avg = air.dens( T_film_avg, P_amb );
	  
	    // Grashof number
	    Gr_F=((grav*beta_F*(T_F_calc-T_bulk)*pow((m_A_f/(4*m_W+m_c)),3))/(pow((mu_F/rho_F),2)));
	    Gr_stag=((grav*beta_stag*(T_stag-T_bulk)*pow((m_A_f/(4*m_W+m_c)),3))/(pow((mu_stag/rho_stag),2)));
	    Gr_avg=((grav*beta_avg*(T_avg-T_bulk)*pow((m_h_rec-m_h_lip),3))/(pow((mu_avg/rho_avg),2)));
	
	    // Rayleigh number
	    Ra_F = fabs(Gr_F*Pr_F);
	    Ra_stag = fabs(Gr_stag*Pr_stag);
	    Ra_avg = fabs(Gr_avg*Pr_avg);
	
	    // Nusselt number
	    Nusselt_F=(0.082*pow(Ra_F,(1./3.))*(-0.9+2.4*(T_F_calc/T_amb)-0.5*pow((T_F_calc/T_amb),2)));
	    Nusselt_stag=(2./3.*0.082*pow(Ra_stag,(1./3.))*(-0.9+2.4*(T_stag/T_amb)-0.5*pow((T_stag/T_amb),2)));
	    Nusselt_avg=(0.082*pow(Ra_avg,(1./3.))*(-0.9+2.4*(T_avg/T_amb)-0.5*pow((T_avg/T_amb),2)));		
	
	    h_F=(((4.*m_W+m_c)*k_F)/(m_A_f))*Nusselt_F;
	    h_stag=(((4.*m_W+m_c)*k_stag)/(m_A_f))*Nusselt_stag;
	    h_avg=(k_avg/(m_h_rec-m_h_lip))*Nusselt_avg;
	
		q_conv_1 = 0.0;	
		for( int i = 0; i < m_n_nodes - S; i++ )
			for( int j = 0; j < n_panels; j++ )
				q_conv_1 += h_avg*A_node*(T_s.at(i,j)-T_bulk);		// Convection loss unshaded absorber surfaces
		
		q_conv_2 = 0.0;
		for( int i = m_n_nodes - S; i < m_n_nodes; i++ )
			for( int j = 0; j < n_panels; j++ )
				q_conv_2 += h_avg*(S*A_node - m_W*m_h_lip)*(T_s.at(i,j)-T_bulk);	// Convection loss absorber surfaces partially in stagnant zone

	    q_conv_3 = 0.0;																// convection loss absorber surfaces in stagnant zone
	    q_conv_4=h_F*m_A_f*(T_F_calc-T_bulk)+h_stag*0.3*m_A_f*(T_stag-T_bulk);			// convection loss from the floor and stagnant zone interface area
	    q_convection_Clausing1983X=q_conv_1+q_conv_2+q_conv_3+q_conv_4;
	
	    // Velocity due to bouyant forces
	    v_b=sqrt(grav*beta_amb*(T_c-T_amb)*(m_h_rec-m_h_lip));
	    v_a=0.5*sqrt(pow(v_b,2.0)+pow((v/2.),2.0));
	
	    T_c=q_convection_Clausing1983X/(rho_amb*v_a*m_A_o*0.5*c_p_amb)+T_amb;
	}


	return;
}

void Cavity_Calcs::ConvectionClausing1987( int n_panels, util::matrix_t<double> & T_s, double T_F, double T_amb,
					double P_amb, double & q_convection )
{
	/* Author: Lukas Feierabend
	Converted from Fortran (sam_lf_pt_viewmod) to c++ in November 2012 by Ty Neises	*/

	/* **********************************************************************************
    !This subroutine calculates the total convective heat losses from the receiver
    !with the correlations presented in Clausing (1987).
    !  The inputs are:
    !    - N_nodes -> number of vertical nodes per receiver panel [-]
    !    - N_panels -> number of receiver panels [-]
    !    - T_s -> the array of surface temperature for every active surface node [K]
    !    - T_F -> the temperature of the receiver FLOOR [K]
    !    - T_amb -> ambient temperature [K]
    !    - P_amb -> ambient pressure [Pa]
    !    - H_rec -> internal receiver height [m]
    !    - H_lip -> height of the upper lip [m]
    !    - W_panel -> width of one receiver panel [m]
    !    - A_F -> area of the FLOOR surface [m2]
    !    - A_O -> area of the aperture [m2]
    !  The outputs are:
    !    - q_convection -> the total convective heat losses through the aperture [W]
    !********************************************************************************** */

	double grav = 9.81;

    // Ratio between node height and lip height
    // m_h_node ===    //H_node = H_rec/DBLE(N_nodes)
    double ratio_H = m_h_lip / m_h_node;	//H_lip/H_node
    double CE = ceil(ratio_H);
    double FL = floor(ratio_H);
    double MO = ratio_H - (double) FL;
    double A_node = m_h_node * m_W;      //H_node*W_panel

    // Average wall temperature of the surfaces below the horizontal plane passing through the upper lip
	double sum_T_lower = 0.0;
	for( int i = 0; i < m_n_nodes - CE; i++ )
		for( int j = 0; j < n_panels; j++ )
			sum_T_lower += T_s.at(i,j);

	double sum_T_upper = 0.0;
	for( int i = m_n_nodes - (int)CE; i < m_n_nodes; i++ )
		for( int j = 0; j < n_panels; j++ )
			sum_T_upper += T_s.at(i,j);

	double T_w_ave = (sum_T_lower*A_node + (1.0-MO)*sum_T_upper*A_node + (2.0/3.0)*m_A_f*T_F) / ( ( (double)(m_n_nodes-CE) + 1.0 - MO ) * (double)n_panels*A_node + (2.0/3.0)*m_A_f );

    // In case unrealistic wall temperature values are provided, the convection loss rate is set to 0.
    if( T_w_ave < 250.0 )
	{
        q_convection = 0.0;
        return;
	}
	
    // Film temperature for the property evaluation
    double T_film = (T_w_ave+T_amb)/2.0;
    // Aperture length
    double L_a = m_h_rec - m_h_lip;  // H_rec-H_lip
    // Characteristic length for the dimensionless numbers (reference: Clausing, 1987)
    double L_c = L_a+0.5*m_h_rec; //1.5*H_rec-H_lip;	// ST is that what Clausing did? L_c=Height_Apperature+0.5*Sidelength_of_the_cube_that_resembles_the_cavity
    // Convective area definition for the wall area below the shear layer (reference: Clausing, 1987)
    double A_cz = m_A_f + m_A_o + n_panels*m_W*(m_h_rec-m_h_lip)*CSP::pi/2.0;      // N_panels*W_panel*(H_rec-H_lip)*pi/2.0;

    // Calculate the air properties
	HTFProperties air;
	air.SetFluid( HTFProperties::Air );
    // Specific heat [J/kg-K]
	double c_p_amb = air.Cp( T_amb )*1000.0;			// SpecHeat(1.d0,T_amb,1.d0)*1000.0
    double c_p_film = air.Cp( T_film )*1000.0;			// SpecHeat(1.d0,T_film,1.d0)*1000.0
    // Volume expansion coefficient [1/K]
    double beta_amb = 1./T_amb; 
    double beta_film = 1./T_film;   
    // Conductivity [W/m-K]
	double k_amb = air.cond( T_amb );					// Conductivity(1.d0,T_amb,1.d0)
	double k_film = air.cond( T_film );					// Conductivity(1.d0,T_film,1.d0)
    // Viscosity [Pa-s]
	double mu_amb = air.visc( T_amb );					// Viscosity(1.d0,T_amb,1.d0)
	double mu_film = air.visc( T_film );				// Viscosity(1.d0,T_film,1.d0)
    // Density [kg/m3]
	double rho_amb = air.dens( T_amb, P_amb );			// Density(1.d0,T_amb,P_amb)
	double rho_film = air.dens( T_film, P_amb );		// Density(1.d0,T_film,P_amb)
    // Prandtl number
    double Pr_amb = c_p_amb*mu_amb/k_amb;
    double Pr_film = c_p_film*mu_film/k_film;
    // Rayleigh number
    double Ra_amb = grav*beta_amb*(T_w_ave-T_amb)*pow(L_c,3)*pow( (rho_amb/mu_amb),2 )*Pr_amb;
    double Ra_film = grav*beta_film*(T_w_ave-T_amb)*pow(L_c,3)*pow( (rho_film/mu_film),2 )*Pr_film;

	double g, f;
    // Factor g and f for the Nusselt number correlation
    if( Ra_film < 3.8E+8 )
	{
		g = 0.63*pow(Ra_film,0.25);
        f = 1.0;
	}
    else if( Ra_film < 1.6E+9 )
	{
        g = 0.63*pow(Ra_film,0.25);
		f = (-0.7476 + 0.9163*(T_w_ave/T_amb) - 0.1663*pow((T_w_ave/T_amb),2))*(pow(Ra_film,(1.0/3.0)) - pow(3.8E8,(1.0/3.0)))/(pow(1.69E9,(1.0/3.0)) - pow(3.8E8,(1.0/3.0))) + 1;
		// f = (-0.7476+0.9163*(T_w_avg/T_amb)-0.1663*(T_w_avg/T_amb)**2)*(Ra_film**(1.0/3.0)-3.8E+08**(1.0/3.0))/(1.6E+09**(1.0/3.0)-3.8E+08**(1.0/3.0))+1
	}
    else
	{
        g = 0.108*pow(Ra_film,(1.0/3.0));
        f = 0.2524 + 0.9163*(T_w_ave/T_amb) - 0.1663*pow((T_w_ave/T_amb),2);
	}
	
    // Start value for the iteration of b
    double b = 1.0;
    // Initial high error     
    double error = 9999.0;
    // iteration to obtain result for factor b
	double bX;
	while( error > 1.E-6 )
	{
		bX = 1 - 1.57*pow(((g*f*b*k_film/k_amb)/(pow((Ra_amb*Pr_amb*L_a/L_c),0.5)*m_A_o/A_cz)),(2.0/3.0));
		//bX = 1-1.57*((g*f*b*k_film/k_amb)/((Ra_amb*Pr_amb*L_a/L_c)**0.5*A_O/A_cz))**(2.0/3.0)
		error = fabs(b-bX)/b;
		b = bX;
	}

    // Total convective heat losses throughout the aperture
    q_convection = g*f*b*k_film*A_cz*(T_w_ave-T_amb)/L_c;

	return;
}
