//	CoordSys3.cpp
// =================================================================================
// Copyright 1992-2009	Regents of University of California
//						Lawrence Berkeley National Laboratory

//  Authors: W.L. Carroll and R.J. Hitchcock
//           Building Technologies Department
//           Lawrence Berkeley National Laboratory

// This work was supported by the Assistant Secretary for Energy Efficiency 
// and Renewable Energy, Office of Building Technologies, 
// Building Systems and Materials Division of the 
// U.S. Department of Energy under Contract No. DE-AC03-76SF00098.

// NOTICE: The Government is granted for itself and others acting on its behalf 
// a paid-up, nonexclusive, irrevocable worldwide license in this data to reproduce, 
// prepare derivative works, and perform publicly and display publicly. 
// Beginning five (5) years after (date permission to assert copyright was obtained),
// subject to two possible five year renewals, the Government is granted for itself 
// and others acting on its behalf a paid-up, nonexclusive, irrevocable worldwide
// license in this data to reproduce, prepare derivative works, distribute copies to 
// the public, perform publicly and display publicly, and to permit others to do so. 
// NEITHER THE UNITED STATES NOR THE UNITED STATES DEPARTMENT OF ENERGY, NOR ANY OF
// THEIR EMPLOYEES, MAKES ANY WARRANTY, EXPRESS OR IMPLIED, OR ASSUMES ANY LEGAL 
// LIABILITY OR RESPONSIBILITY FOR THE ACCURACY, COMPLETENESS, OR USEFULNESS OF ANY 
// INFORMATION, APPARATUS, PRODUCT, OR PROCESS DISCLOSED, OR REPRESENTS THAT ITS USE 
// WOULD NOT INFRINGE PRIVATELY OWNED RIGHTS.
// =================================================================================

#include <iostream>
#include <sstream>
#include <iomanip>
#include <vector>
using namespace std;

#ifndef INFINITY
extern double INFINITY;
#endif
extern double NaN_QUIET;

// writewndo() Error handler include
#include "DElightManagerC.h"

#include "BGL.h"

namespace BldgGeomLib {

//	RHcoordSys3
	// Constructors
RHCoordSys3::RHCoordSys3() : cs3(3)
{
//	cs3 = [[1 0 0][0 1 0][0 0 1]]
	cs3[0] = vl_x;	//	x
	cs3[1] = vl_y;	//	y
	cs3[2] = vl_z;	//	z
}

RHCoordSys3::RHCoordSys3(const RHCoordSys3 &cs0) : cs3(3)	// copy 
{
	cs3[0] = cs0[0];
	cs3[1] = cs0[1];
	cs3[2] = cs0[2];
}


RHCoordSys3::RHCoordSys3(const vector3& z,const point3& p0,const point3& p1) : cs3(3)
//	z is  vec in normal direction - not reqired to be unit length
//	requires (1) len(z)>0; (2) p0 != p1; (3) z and vector3(p1-p0) are not parallel
{
//	cout << "RHCoordSys3: " << "z: " << z << " p0: " << p0 << " p1: " << p1 << '\n';
	vector3	v1(p1 - p0);
	vector3	v2 = cross(z,v1);
//	cout << "RHCoordSys3: " << "v1: " << v1 << " v2: " << v2 << '\n';
	if (len(z) == 0 || len(v1) == 0 || len(v2) == 0)  {
		cs3[0] = vector3(NaN_QUIET,NaN_QUIET,NaN_QUIET);	//	x
		cs3[1] = vector3(NaN_QUIET,NaN_QUIET,NaN_QUIET);	//	y
		cs3[2] = vector3(NaN_QUIET,NaN_QUIET,NaN_QUIET);	//	z
//		throw string("RHCoordSys3: constructor failed");
	}
	else {
		cs3[2] = norm(z);	//	z
		cs3[1] = norm(v2);	//	y
		cs3[0] = cross(cs3[1],cs3[2]);	//	x
	}
//	cout << "RHCoordSys3: " << "x: " << cs3[0] << " y: " << cs3[1] << " z: " << cs3[2] << '\n';
}


RHCoordSys3::RHCoordSys3(const point3& p0,const point3& p1,const point3& p2) : cs3(3)	
{
	vector3	v0(p1 - p0);
	vector3	v1(p2 - p0);
	vector3	v2 = cross(v0,v1);
	if (!len(v0) && !len(v1) && !len(v2))  {
		cs3[0] = vector3(NaN_QUIET,NaN_QUIET,NaN_QUIET);	//	x
		cs3[1] = vector3(NaN_QUIET,NaN_QUIET,NaN_QUIET);	//	y
		cs3[2] = vector3(NaN_QUIET,NaN_QUIET,NaN_QUIET);	//	z
	}
	else {
		cs3[2] = norm(v2);	//	z
		cs3[0] = norm(v0);	//	x
		cs3[1] = cross(cs3[2],cs3[0]);	//	y
	}
}

RHCoordSys3::RHCoordSys3(Double phi,Double theta,Double zeta) : cs3(3)
{
	RHCoordSys3	csTemp;	//	cs = [[1 0 0][0 1 0][0 0 1]]
	csTemp = csTemp.Rotate3a(phi,theta,zeta);
	cs3[0] = csTemp[0];
	cs3[1] = csTemp[1];
	cs3[2] = csTemp[2];
}

RHCoordSys3::RHCoordSys3(const vector3& x,const vector3& y,const vector3& z) : cs3(3)
// intended to be internally used only - vectors are not checked!!!
{
	cs3[0] = x;
	cs3[1] = y;
	cs3[2] = z;
}


RHCoordSys3::~RHCoordSys3()
{ }

// Other Methods

RHCoordSys3	RHCoordSys3::Rotate1(const vector3& axis, Double angle)
//	spits out a copy, leaving original CS unchanged
//	Rot3 NOTE:  vector3 "axis" MUST BE UNIT LENGTH
{
	RHCoordSys3	csTemp;
//	Rot3 NOTE:  vector3 "axis" MUST BE UNIT LENGTH
	matrix3		R1 = Rot3(axis,angle);
//	cout << R1 << "\n";
	csTemp[0] = R1*cs3[0];
	csTemp[1] = R1*cs3[1];
	csTemp[2] = R1*cs3[2];
	return csTemp;
}

RHCoordSys3	RHCoordSys3::Rotate3(Double Azimuth,Double Tilt,Double AxialRot)
//	spits out a copy, leaving original CS unchanged
{
	//	rotation angles - radians
	//	rotation matrixes R1 etc always left-multiply vector!

	vector3 x1, z2;
	matrix3 R1, R2, R3, Rtot;
	
	// Rotation 1
	if (Azimuth) {
//	Rot3 NOTE:  vector3 "axis" MUST BE UNIT LENGTH
		R1 = Rot3(cs3[2], Azimuth);	// rotation is around ICS_z
		x1 = R1*cs3[0];
		//	z1 = cs3[2]; = ICS_z
	}
	else {
		R1 = vl_I;
		x1 = cs3[0];
	}

	// Rotation 2
	// Tilt = 0, PI are special cases that leave or flip the surface in the x-y plane
	if (Tilt) {
//	Rot3 NOTE:  vector3 "axis" MUST BE UNIT LENGTH
		R2 = Rot3(x1, Tilt);	// rotation is around x1
		z2 = R2*cs3[2];	//z2 = R2*z1; z1 = ICS_z;
	}
	else {
		R2 = vl_I;
		z2 = cs3[2];
	}

	// Rotation 3
	if (AxialRot) {
//	Rot3 NOTE:  vector3 "axis" MUST BE UNIT LENGTH
		R3 = Rot3(z2, AxialRot);	// rotation is around z2 (= surface normal)
	}
	else R3 = vl_I;
	
	// composite rotation
	Rtot = R3*R2*R1;	//order is important

	return	RHCoordSys3(Rtot*cs3[0],Rtot*cs3[1],Rtot*cs3[2]);
}

RHCoordSys3	RHCoordSys3::Rotate3a(Double Azimuth,Double Tilt,Double AxialRot)
//	spits out a copy, leaving original CS unchanged
{
	//	rotation angles - radians
	//	rotation matrixes R1 etc always left-multiply vector!

	vector3 xx = cs3[0], zz = cs3[2];
	
//	Rot3 NOTE:  vector3 "axis" MUST BE UNIT LENGTH
	// Rotation 1
	if (Azimuth) {
		xx = Rot3(zz, Azimuth)*xx;	// rotation is around ICS_z
	}

	// Rotation 2
	// Tilt = 0, PI are special cases that leave or flip the surface in the x-y plane
	if (Tilt) {
		zz = Rot3(xx, Tilt)*zz;	// rotation is around x1
	}

	// Rotation 3
	if (AxialRot) {
		xx = Rot3(zz, AxialRot)*xx;	// rotation is around z2 (= surface normal)
	}

	return	RHCoordSys3(xx,cross(zz,xx),zz);
}

RHCoordSys3	RHCoordSys3::RotateY()
{
	return RHCoordSys3(-cs3[0],cs3[1],-cs3[2]);
}

vector<Double>	RHCoordSys3::RotAngles(RHCoordSys3 Ref_CS) const
{
	vector<Double> RotAng(3);

	vector3	z3 = cs3[2];
	Double	costheta = dot(z3,Ref_CS[2]);

	if ( fabs(costheta) < 1.) {	// normal cases
		//	tilt
		RotAng[1] = acos(costheta );

		// azimuth
		vector3	x2 = cross(Ref_CS[2],z3);	// order is important; x2 = x1
		RotAng[0] = atan2(dot(x2,Ref_CS[1]),dot(x2,Ref_CS[0]));
		
		//	Rot3
		vector3	y2 = cross(z3,x2);	// order is important; z3 = z2
		vector3	x3 = cs3[0];
		RotAng[2] = atan2(dot(x3,y2),dot(x3,x2));

	}
	else {	// special cases: tilt angle theta = 0, PI
		//	tilt
		if (costheta >= 1.) {RotAng[1] = 0.;}
		else {RotAng[1] = PI;}	// i.e., if (costheta <= -1.)

		// azimuth
		RotAng[0] = atan2(dot(cs3[0],Ref_CS[1]),dot(cs3[0],Ref_CS[0]) );
		
		//	Rot3
		RotAng[2] = 0.;
	}

	return RotAng;
}

//	dir3D conversions to/from different RHCoordSyst3's
vector3	dirWCStoLCS(vector3 vDir1, RHCoordSys3 LCS)
{
	normalize(vDir1);
	return vector3(dot(vDir1,LCS[0]),dot(vDir1,LCS[1]),dot(vDir1,LCS[2]));
}

vector3	dirLCStoWCS(vector3 vDir1, RHCoordSys3 LCS)
{
	normalize(vDir1);
	return vDir1[0]*LCS[0] + vDir1[1]*LCS[1] + vDir1[2]*LCS[2];
}

//	convert dir from CS1 to CS2 coords
vector3	dirCS1toCS2(vector3 vDir1, RHCoordSys3 CS1, RHCoordSys3 CS2)
{
	return	dirWCStoLCS(dirLCStoWCS(vDir1,CS1),CS2);
}

}	//	end namespace BldgGeomLib


using namespace BldgGeomLib;

ostream &operator << (ostream &s, const RHCoordSys3 &cs)
{
	Int w = s.width();
	return(s << '[' << cs[0] << setw(w) << cs[1] << setw(w) << cs[2] << ']' );
}

istream &operator >> (istream &s, RHCoordSys3 &cs)
{
    RHCoordSys3	result;
    Char	c;
	std::ostringstream osstream;
	
	// Expected format: [[1 2 3] [4 5 6] [7 8 9]]
	// Each vector is a column[??? ROW!!!] of the matrix.
	
    while (s >> c && isspace(c))		// ignore leading white space
		;
		
    if (c == '[')			
    {
		s >> result[0] >> result[1] >> result[2];

		if (!s)
		{
			osstream << "Expected number while reading RHCoordSys3\n";
			writewndo(osstream.str(),"e");
			return(s);
		}
			
		while (s >> c && isspace(c))
			;
			
		if (c != ']')
    	{
    		s.clear(ios::failbit);
			osstream << "Expected ']' while reading RHCoordSys3\n";
			writewndo(osstream.str(),"e");
	    	return(s);
    	}
	}
    else
	{
	    s.clear(ios::failbit);
		osstream << "Expected '[' while reading RHCoordSys3\n";
		writewndo(osstream.str(),"e");
	    return(s);
	}
	
	cs = result;
    return(s);
}

