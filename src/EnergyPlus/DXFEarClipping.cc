// EnergyPlus, Copyright (c) 1996-2016, The Board of Trustees of the University of Illinois and
// The Regents of the University of California, through Lawrence Berkeley National Laboratory
// (subject to receipt of any required approvals from the U.S. Dept. of Energy). All rights
// reserved.
//
// If you have questions about your rights to use or distribute this software, please contact
// Berkeley Lab's Innovation & Partnerships Office at IPO@lbl.gov.
//
// NOTICE: This Software was developed under funding from the U.S. Department of Energy and the
// U.S. Government consequently retains certain rights. As such, the U.S. Government has been
// granted for itself and others acting on its behalf a paid-up, nonexclusive, irrevocable,
// worldwide license in the Software to reproduce, distribute copies to the public, prepare
// derivative works, and perform publicly and display publicly, and to permit others to do so.
//
// Redistribution and use in source and binary forms, with or without modification, are permitted
// provided that the following conditions are met:
//
// (1) Redistributions of source code must retain the above copyright notice, this list of
//     conditions and the following disclaimer.
//
// (2) Redistributions in binary form must reproduce the above copyright notice, this list of
//     conditions and the following disclaimer in the documentation and/or other materials
//     provided with the distribution.
//
// (3) Neither the name of the University of California, Lawrence Berkeley National Laboratory,
//     the University of Illinois, U.S. Dept. of Energy nor the names of its contributors may be
//     used to endorse or promote products derived from this software without specific prior
//     written permission.
//
// (4) Use of EnergyPlus(TM) Name. If Licensee (i) distributes the software in stand-alone form
//     without changes from the version obtained under this License, or (ii) Licensee makes a
//     reference solely to the software portion of its product, Licensee must refer to the
//     software as "EnergyPlus version X" software, where "X" is the version number Licensee
//     obtained under this License and may not use a different name for the software. Except as
//     specifically required in this Section (4), Licensee shall not use in a company name, a
//     product name, in advertising, publicity, or other promotional activities any name, trade
//     name, trademark, logo, or other designation of "EnergyPlus", "E+", "e+" or confusingly
//     similar designation, without Lawrence Berkeley National Laboratory's prior written consent.
//
// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR
// IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY
// AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR
// CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
// CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
// SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
// THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
// OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
// POSSIBILITY OF SUCH DAMAGE.
//
// You are under no obligation whatsoever to provide any bug fixes, patches, or upgrades to the
// features, functionality or performance of the source code ("Enhancements") to anyone; however,
// if you choose to make your Enhancements available either publicly, or directly to Lawrence
// Berkeley National Laboratory, without imposing a separate written license agreement for such
// Enhancements, then you hereby grant the following license: a non-exclusive, royalty-free
// perpetual license to install, use, modify, prepare derivative works, incorporate into other
// computer software, distribute, and sublicense such enhancements or derivative works thereof,
// in binary and source code form.

// C++ Headers
#include <cmath>

// ObjexxFCL Headers
#include <ObjexxFCL/Array2D.hh>
#include <ObjexxFCL/Fmath.hh>
#include <ObjexxFCL/gio.hh>

// EnergyPlus Headers
#include <DXFEarClipping.hh>
#include <DataGlobals.hh>
#include <DataPrecisionGlobals.hh>
#include <DataSurfaces.hh>
#include <General.hh>
#include <UtilityRoutines.hh>

namespace EnergyPlus {

namespace DXFEarClipping {

	// Module containing the routines dealing with triangulating a polygon of >4 sides

	// Module information:
	//       Author         Linda Lawrie
	//       Date written   October 2005
	//       Modified       na
	//       Re-engineered  na

	// Purpose of this module:
	// This module provides the techniques and back up procedures for producing a triangulated
	// polygon from a >4 sided figure.  It is only used for DXF output.

	// Methodology employed:
	// Ear clipping has turned out to be the simplest, most robust technique.

	// References:
	// na

	// Other notes:
	// na

	// Use statements:
	// Using/Aliasing
	using namespace DataPrecisionGlobals;
	using namespace DataVectorTypes;
	using DataGlobals::Pi;
	using DataGlobals::TwoPi;
	using DataGlobals::RadToDeg;
	using DataGlobals::OutputFileDebug;

	// Data

	// Derived type definitions:
	// na

	// Module variable declarations:
	// na
	bool trackit( false );
	// Subroutine specifications for module <module_name>:

	// rest of routines are private.

	// Functions

	bool
	InPolygon(
		Vector const & point,
		Array1A< Vector > poly,
		int const nsides
	)
	{
		// this routine is not used in the current scheme

		// Return value
		bool InPolygon;

		// Argument array dimensioning
		poly.dim( nsides );

		// Locals
		//'Return TRUE if the point (xp,yp) lies inside the circumcircle
		//'made up by points (x1,y1) (x2,y2) (x3,y3)
		//'The circumcircle centre is returned in (xc,yc) and the radius r
		//'NOTE: A point on the edge is inside the circumcircle

		Real64 const epsilon( 0.0000001 );
		Real64 anglesum;
		Real64 costheta;
		int vert;
		Real64 m1;
		Real64 m2;
		Real64 acosval;

		// Object Data
		Vector p1;
		Vector p2;

		InPolygon = false;

		anglesum = 0.0;

		for ( vert = 1; vert <= nsides - 1; ++vert ) {

			p1.x = poly( vert ).x - point.x;
			p1.y = poly( vert ).y - point.y;
			p1.z = poly( vert ).z - point.z;

			p2.x = poly( vert + 1 ).x - point.x;
			p2.y = poly( vert + 1 ).y - point.y;
			p2.z = poly( vert + 1 ).z - point.z;

			m1 = Modulus( p1 );
			m2 = Modulus( p2 );

			if ( m1 * m2 <= epsilon ) {
				InPolygon = true;
				break;
			} else {
				costheta = ( p1.x * p2.x + p1.y * p2.y + p1.z * p2.z ) / ( m1 * m2 );
				acosval = std::acos( costheta );
				anglesum += acosval;
			}
		}

		if ( std::abs( anglesum - TwoPi ) <= epsilon ) {
			InPolygon = true;
		}

		return InPolygon;

	}

	Real64
	Modulus( Vector const & point )
	{
		// this routine is not used in the current scheme

		// Return value
		Real64 rModulus;

		rModulus = std::sqrt( point.x * point.x + point.y * point.y + point.z * point.z );

		return rModulus;
	}

	int
	Triangulate(
		int const nsides, // number of sides to polygon
		Array1A< Vector > polygon,
		Array1D< dTriangle > & outtriangles,
		Real64 const surfazimuth, // surface azimuth angle (outward facing normal)
		Real64 const surftilt, // surface tilt angle
		std::string const & surfname, // surface name (for error messages)
		int const surfclass // surface class
	)
	{

		// Subroutine information:
		//       Author         Linda Lawrie
		//       Date written   October 2005
		//       Modified       na
		//       Re-engineered  na

		// Purpose of this subroutine:
		// This routine is a self-contained triangulation calculation from a polygon
		// of 3D vertices, nsides, to a returned set (as possible) of triangles -- noted
		// by vertex numbers.

		// Methodology employed:
		// <Description>

		// References:
		// na

		// Use statements:
		// Using/Aliasing
		using DataGlobals::DisplayExtraWarnings;
		using DataSurfaces::cSurfaceClass;
		using DataSurfaces::SurfaceClass_Floor;
		using DataSurfaces::SurfaceClass_Roof;
		using DataSurfaces::SurfaceClass_Overhang;
		using General::RoundSigDigits;

		// Return value
		int Triangulate;

		// Argument array dimensioning
		polygon.dim( nsides );

		// Locals
		// Subroutine argument definitions:

		// Subroutine parameter definitions:
		Real64 const point_tolerance( 0.00001 );
		static gio::Fmt fmtLD( "*" );

		// Interface block specifications:
		// na

		// Derived type definitions:
		// na

		// Subroutine local variable declarations:
		bool errFlag;
		Array1D_int ears( nsides );
		Array1D_int r_angles( nsides );
		Array1D< Real64 > rangles( nsides );
		Array1D_int c_vertices( nsides );
		Array2D_int earvert( nsides, 3 );
		Array1D_bool removed( nsides );
		//unused  type(Vector_2d), dimension(3) :: testtri
		//unused  type(Vector_2d) :: point
		Array1D_int earverts( 3 );
		Array1D< Real64 > xvt( nsides );
		Array1D< Real64 > yvt( nsides );
		Array1D< Real64 > zvt( nsides );

		//'General Variables
		int i;
		int j;
		//unused  integer k
		int ntri;
		//unused  logical inpoly
		int nvertcur;
		int ncount;
		int svert;
		int mvert;
		int evert;
		//unused  integer tvert
		int nears;
		int nrangles;
		int ncverts;
		//unused  double precision :: ang
		//unused  double precision :: val
		std::string line;
		static int errcount( 0 );

		// Object Data
		Array1D< Vector_2d > vertex( nsides );
		Array1D< dTriangle > Triangle( nsides );

		errFlag = false;
		//  vertex=polygon
		//  if (surfname == 'BOTTOM:OFFICE_E_3') THEN
		//    trackit=.TRUE.
		//  else
		//    trackit=.FALSE.
		//  endif
		if ( surfclass == SurfaceClass_Floor || surfclass == SurfaceClass_Roof || surfclass == SurfaceClass_Overhang ) {
			CalcRfFlrCoordinateTransformation( nsides, polygon, surfazimuth, surftilt, xvt, yvt, zvt );
			for ( svert = 1; svert <= nsides; ++svert ) {
				for ( mvert = svert + 1; mvert <= nsides; ++mvert ) {
					if ( std::abs( xvt( svert ) - xvt( mvert ) ) <= point_tolerance ) xvt( svert ) = xvt( mvert );
					if ( std::abs( zvt( svert ) - zvt( mvert ) ) <= point_tolerance ) zvt( svert ) = zvt( mvert );
				}
			}
			for ( svert = 1; svert <= nsides; ++svert ) {
				vertex( svert ).x = xvt( svert );
				vertex( svert ).y = zvt( svert );
				//      if (trackit) write(outputfiledebug,*) 'x=',xvt(svert),' y=',zvt(svert)
			}
		} else {
			CalcWallCoordinateTransformation( nsides, polygon, surfazimuth, surftilt, xvt, yvt, zvt );
			for ( svert = 1; svert <= nsides; ++svert ) {
				for ( mvert = svert + 1; mvert <= nsides; ++mvert ) {
					if ( std::abs( xvt( svert ) - xvt( mvert ) ) <= point_tolerance ) xvt( svert ) = xvt( mvert );
					if ( std::abs( zvt( svert ) - zvt( mvert ) ) <= point_tolerance ) zvt( svert ) = zvt( mvert );
				}
			}
			for ( svert = 1; svert <= nsides; ++svert ) {
				vertex( svert ).x = xvt( svert );
				vertex( svert ).y = zvt( svert );
			}
		}

		// find ears
		nvertcur = nsides;
		ncount = 0;
		svert = 1;
		mvert = 2;
		evert = 3;
		removed = false;
		while ( nvertcur > 3 ) {
			generate_ears( nsides, vertex, ears, nears, r_angles, nrangles, c_vertices, ncverts, removed, earverts, rangles );
			if ( ! any_gt( ears, 0 ) ) {
				ShowWarningError( "DXFOut: Could not triangulate surface=\"" + surfname + "\", type=\"" + cSurfaceClass( surfclass ) + "\", check surface vertex order(entry)" );
				++errcount;
				if ( errcount == 1 && ! DisplayExtraWarnings ) {
					ShowContinueError( "...use Output:Diagnostics,DisplayExtraWarnings; to show more details on individual surfaces." );
				}
				if ( DisplayExtraWarnings ) {
					gio::write( line, fmtLD ) << "surface=" << surfname << " class=" << cSurfaceClass( surfclass );
					ShowMessage( line );
					for ( j = 1; j <= nsides; ++j ) {
						//          write(line,"(' side=',i2,' (',2(f6.1,','),f6.1,')')") j,polygon(j)
						line = " side=" + RoundSigDigits( j ) + " (" + RoundSigDigits( polygon( j ).x, 1 ) + ',' + RoundSigDigits( polygon( j ).y, 1 ) + ',' + RoundSigDigits( polygon( j ).z, 1 ) + ')';
						ShowMessage( line );
					}
					gio::write( line, fmtLD ) << "number of triangles found=" << ncount;
					ShowMessage( line );
					for ( j = 1; j <= nrangles; ++j ) {
						//          write(line,"(' r angle=',i2,' vert=',i2,' deg=',f6.1)") j,r_angles(j),rangles(j)*RadToDeg
						line = " r angle=" + RoundSigDigits( j ) + " vert=" + RoundSigDigits( r_angles( j ) ) + " deg=" + RoundSigDigits( rangles( j ) * RadToDeg, 1 );
						ShowMessage( line );
					}
				}
				break; // while loop
			}
			if ( nears > 0 ) {
				svert = earverts( 1 );
				mvert = earverts( 2 );
				evert = earverts( 3 );
				// remove ear
				++ncount;
				removed( mvert ) = true;
				earvert( ncount, 1 ) = svert;
				earvert( ncount, 2 ) = mvert;
				earvert( ncount, 3 ) = evert;
				--nvertcur;
			}
			if ( nvertcur == 3 ) {
				j = 1;
				++ncount;
				for ( i = 1; i <= nsides; ++i ) {
					if ( removed( i ) ) continue;
					earvert( ncount, j ) = i;
					++j;
				}
			}
		}

		ntri = ncount;

		for ( i = 1; i <= ntri; ++i ) {
			Triangle( i ).vv0 = earvert( i, 1 );
			Triangle( i ).vv1 = earvert( i, 2 );
			Triangle( i ).vv2 = earvert( i, 3 );
		}

		outtriangles.allocate( ntri );
		for ( i = 1; i <= ntri; ++i ) {
			outtriangles( i ) = Triangle( i );
		}

		Triangulate = ntri;

		return Triangulate;
	}

	Real64
	angle_2dvector(
		Real64 const xa, // vertex coordinate
		Real64 const ya, // vertex coordinate
		Real64 const xb, // vertex coordinate
		Real64 const yb, // vertex coordinate
		Real64 const xc, // vertex coordinate
		Real64 const yc // vertex coordinate
	)
	{

		// Function information:
		//       Author         Linda Lawrie
		//       Date written   October 2005
		//       Modified       na
		//       Re-engineered  na

		// Purpose of this function:
		// This function calculates the angle between two sides of a 2d polygon.
		// It computes the interior angle in radians at vertex
		// (XB,YB) of the chain formed by the directed edges from
		// (XA,YA) to (XB,YB) to (XC,YC).  The interior is to the
		// left of the two directed edges.

		// Methodology employed:
		// <Description>

		// References:
		// Geometry Tools for Computer Graphics

		// Use statements:
		// na

		// Return value
		Real64 angle; // the angle, between 0 and 2*PI.

		// Locals
		// Function argument definitions:
		// angle is set to PI/2 in the degenerate case.

		// Function parameter definitions:
		Real64 const epsilon( 0.0000001 );

		// Interface block specifications:
		// na

		// Derived type definitions:
		// na

		// Function local variable declarations:
		Real64 t;
		Real64 x1;
		Real64 x2;
		Real64 y1;
		Real64 y2;

		x1 = xa - xb;
		y1 = ya - yb;
		x2 = xc - xb;
		y2 = yc - yb;

		t = std::sqrt( ( x1 * x1 + y1 * y1 ) * ( x2 * x2 + y2 * y2 ) );
		if ( t == 0.0E+00 ) t = 1.0E+00;

		t = ( x1 * x2 + y1 * y2 ) / t;

		if ( ( 1.0E+00 - epsilon ) < std::abs( t ) ) {
			t = sign( 1.0E+00, t );
		}

		angle = std::acos( t );

		if ( x2 * y1 - y2 * x1 < 0.0E+00 ) {
			angle = 2.0E+00 * Pi - angle;
		}

		return angle;
	}

	bool
	polygon_contains_point_2d(
		int const nsides, // number of sides (vertices)
		Array1A< Vector_2d > polygon, // points of polygon
		Vector_2d const & point // point to be tested
	)
	{

		// Function information:
		//       Author         Linda Lawrie
		//       Date written   October 2005
		//       Modified       na
		//       Re-engineered  na

		// Purpose of this function:
		// Determine if a point is inside a simple 2d polygon.  For a simple polygon (one whose
		// boundary never crosses itself).  The polygon does not need to be convex.

		// Methodology employed:
		// <Description>

		// References:
		// M Shimrat, Position of Point Relative to Polygon, ACM Algorithm 112,
		// Communications of the ACM, Volume 5, Number 8, page 434, August 1962.

		// Use statements:
		// na

		// Return value
		bool inside; // return value, true=inside, false = not inside

		// Argument array dimensioning
		polygon.dim( nsides );

		// Locals
		// Function argument definitions:

		// Function parameter definitions:

		// Interface block specifications:
		// na

		// Derived type definitions:
		// na

		// Function local variable declarations:
		int i;
		int ip1;

		inside = false;

		for ( i = 1; i <= nsides; ++i ) {

			if ( i < nsides ) {
				ip1 = i + 1;
			} else {
				ip1 = 1;
			}

			if ( ( polygon( i ).y < point.y && point.y <= polygon( ip1 ).y ) || ( point.y <= polygon( i ).y && polygon( ip1 ).y < point.y ) ) {
				if ( ( point.x - polygon( i ).x ) - ( point.y - polygon( i ).y ) * ( polygon( ip1 ).x - polygon( i ).x ) / ( polygon( ip1 ).y - polygon( i ).y ) < 0 ) {
					inside = ! inside;
				}
			}

		}

		return inside;

	}

	void
	generate_ears(
		int const nvert, // number of vertices in polygon
		Array1A< Vector_2d > vertex,
		Array1A_int ears, // number of ears possible (dimensioned to nvert)
		int & nears, // number of ears found
		Array1A_int r_vertices, // number of reflex vertices (>180) possible
		int & nrverts, // number of reflex vertices found (>=180)
		Array1A_int c_vertices, // number of convex vertices
		int & ncverts, // number of convex vertices found (< 180)
		Array1A_bool removed, // array that shows if a vertex has been removed (calling routine)
		Array1A_int earvert, // vertex indicators for first ear
		Array1A< Real64 > rangles
	)
	{

		// Subroutine information:
		//       Author         Linda Lawrie
		//       Date written   October 2005
		//       Modified       na
		//       Re-engineered  na

		// Purpose of this subroutine:
		// This routine generates "ears", "reflex angles" and "convex angles" of the polygon
		// based on the method set for in the reference.

		// Methodology employed:
		// No elegance used here.  Always starts with first vertex in polygon.

		// References:
		// Geometric Tools for Computer Graphics, Philip Schneider, David Eberly. 2003.  Ear
		// clipping for triangulation is described in Chapter 13 on Polygon Partitioning.  Also
		// described in a small article "Triangulation by Ear Clipping", David Eberly, http://www.geometrictools.com

		// Use statements:
		// na

		// Argument array dimensioning
		vertex.dim( nvert );
		ears.dim( nvert );
		r_vertices.dim( nvert );
		c_vertices.dim( nvert );
		removed.dim( nvert );
		earvert.dim( 3 );
		rangles.dim( nvert );

		// Locals
		// Subroutine argument definitions:

		// Subroutine parameter definitions:
		static gio::Fmt fmtLD( "*" );

		// Interface block specifications:
		// na

		// Derived type definitions:
		// na

		// Subroutine local variable declarations:
		int svert; // starting vertex
		int mvert; // "middle" vertex (this will be an ear, if calculated)
		int evert; // ending vertex
		Real64 ang; // ang between
		int tvert; // test vertex, intermediate use
		bool inpoly; // in polygon or not
		int j; // loop counter

		// Object Data
		Vector_2d point; // structure for point
		Array1D< Vector_2d > testtri( 3 ); // structure for triangle

		// initialize, always recalculate
		ears = 0;
		r_vertices = 0;
		rangles = 0.0;
		nears = 0;
		nrverts = 0;
		c_vertices = 0;
		ncverts = 0;

		for ( svert = 1; svert <= nvert; ++svert ) {
			if ( removed( svert ) ) continue;
			//  have starting vertex.  now need middle and end
			mvert = svert + 1;
			for ( j = 1; j <= nvert; ++j ) {
				if ( mvert > nvert ) mvert = 1;
				if ( removed( mvert ) ) {
					++mvert;
					if ( mvert > nvert ) mvert = 1;
				} else {
					break;
				}
			}
			evert = mvert + 1;
			for ( j = 1; j <= nvert; ++j ) {
				if ( evert > nvert ) evert = 1;
				if ( removed( evert ) ) {
					++evert;
					if ( evert > nvert ) evert = 1;
				} else {
					break;
				}
			}

			// have gotten start, middle and ending vertices.  test for reflex angle

			ang = angle_2dvector( vertex( svert ).x, vertex( svert ).y, vertex( mvert ).x, vertex( mvert ).y, vertex( evert ).x, vertex( evert ).y );

			if ( ang > Pi ) { // sufficiently close to 180 degrees.
				++nrverts;
				r_vertices( nrverts ) = mvert;
				rangles( nrverts ) = ang;
				continue;
			} else {
				++ncverts;
				c_vertices( ncverts ) = mvert;
			}

			// convex angle, see if it's an ear
			testtri( 1 ) = vertex( svert );
			testtri( 2 ) = vertex( mvert );
			testtri( 3 ) = vertex( evert );
			tvert = evert;
			for ( j = 4; j <= nvert; ++j ) {
				++tvert;
				if ( tvert > nvert ) tvert = 1;
				if ( removed( tvert ) ) continue;
				point = vertex( tvert );
				inpoly = polygon_contains_point_2d( 3, testtri, point );
				if ( ! inpoly ) continue;
				break;
			}
			//    if (trackit) then
			//      write(outputfiledebug,*) ' triangle=',svert,mvert,evert
			//      write(outputfiledebug,*) ' vertex1=',vertex(svert)%x,vertex(svert)%y
			//      write(outputfiledebug,*) ' vertex2=',vertex(mvert)%x,vertex(mvert)%y
			//      write(outputfiledebug,*) ' vertex3=',vertex(evert)%x,vertex(evert)%y
			//      write(outputfiledebug,*) ' inpoly=',inpoly
			//    endif
			if ( ! inpoly ) {
				// found an ear
				++nears;
				ears( nears ) = mvert;
				if ( nears == 1 ) {
					earvert( 1 ) = svert;
					earvert( 2 ) = mvert;
					earvert( 3 ) = evert;
				}
				if ( trackit ) {
					gio::write( OutputFileDebug, fmtLD ) << "ear=" << nears << " triangle=" << svert << mvert << evert;
				}
			}
		}

	}

	void
	CalcWallCoordinateTransformation(
		int const nsides,
		Array1A< Vector > polygon,
		Real64 const surfazimuth,
		Real64 const EP_UNUSED( surftilt ), // unused1208
		Array1A< Real64 > xvt,
		Array1A< Real64 > yvt,
		Array1A< Real64 > zvt
	)
	{

		// Subroutine information:
		//       Author         Linda Lawrie
		//       Date written   October 2005
		//       Modified       na
		//       Re-engineered  na

		// Purpose of this subroutine:
		// This routine transforms a "wall" (normally vertical polygon) to a south facing (180 deg outward
		// normal) polygon in 2 d (y vertices are then ignored).

		// Methodology employed:
		// Standard angle rotation

		// References:
		// na

		// Use statements:
		// na

		// Argument array dimensioning
		polygon.dim( nsides );
		xvt.dim( nsides );
		yvt.dim( nsides );
		zvt.dim( nsides );

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// Subroutine parameter definitions:
		// na

		// Interface block specifications
		// na

		// Derived type definitions
		// na

		// Subroutine local variable declarations:
		int i; // Loop Control
		Real64 alpha;
		Real64 alphrad;
		Real64 alpha180;

		// convert surface (wall) to facing 180 (outward normal)

		alpha = surfazimuth;

		alpha180 = 180.0 - alpha; // amount to rotate
		alphrad = alpha180 / RadToDeg;

		for ( i = 1; i <= nsides; ++i ) {
			xvt( i ) = std::cos( alphrad ) * polygon( i ).x + std::sin( alphrad ) * polygon( i ).y;
			yvt( i ) = -std::sin( alphrad ) * polygon( i ).x + std::cos( alphrad ) * polygon( i ).y;
			zvt( i ) = polygon( i ).z;
		}

	}

	void
	CalcRfFlrCoordinateTransformation(
		int const nsides,
		Array1A< Vector > polygon,
		Real64 const EP_UNUSED( surfazimuth ), // unused1208
		Real64 const surftilt,
		Array1A< Real64 > xvt,
		Array1A< Real64 > yvt,
		Array1A< Real64 > zvt
	)
	{

		// Subroutine information:
		//       Author         Linda Lawrie
		//       Date written   October 2005
		//       Modified       na
		//       Re-engineered  na

		// Purpose of this subroutine:
		// This routine transforms a roof/floor (normally flat polygon) to a flat
		// polygon in 2 d (z vertices are then ignored).

		// Methodology employed:
		// Standard angle rotation

		// References:
		// na

		// Use statements:
		// na

		// Argument array dimensioning
		polygon.dim( nsides );
		xvt.dim( nsides );
		yvt.dim( nsides );
		zvt.dim( nsides );

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// Subroutine parameter definitions:
		// na

		// Interface block specifications
		// na

		// Derived type definitions
		// na

		// Subroutine local variable declarations:
		int i; // Loop Control
		Real64 alpha;
		Real64 alphrad;

		alpha = -surftilt;

		alphrad = alpha / RadToDeg;

		for ( i = 1; i <= nsides; ++i ) {
			xvt( i ) = polygon( i ).x;
			yvt( i ) = std::cos( alphrad ) * polygon( i ).x + std::sin( alphrad ) * polygon( i ).y;
			zvt( i ) = -std::sin( alphrad ) * polygon( i ).x + std::cos( alphrad ) * polygon( i ).y;
		}

	}

	void
	reorder( int & EP_UNUSED( nvert ) ) // unused1208
	{

		// Locals
		//type (Vector_2d) nvertex(nvert)
		//integer i
		//type (Vector_2d) point
		//integer nrep

		//  Vertex, nverts is in cw order, reorder for calc

		//nrep=1
		//nvertex(1)=vertex(1)
		//do i=nvert,1,-1
		//  nvertex(nrep)=vertex(i)
		//  nrep=nrep+1
		//enddo
		//do i=1,nvert
		//  vertex(i)=nvertex(i)
		//enddo

	}

} // DXFEarClipping

} // EnergyPlus
