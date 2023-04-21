//	CoordSys3.h
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

namespace BldgGeomLib {

class RHCoordSys3
{
public:

	// Constructors
	RHCoordSys3();
	RHCoordSys3(const RHCoordSys3 &cs0);	// copy 
	RHCoordSys3(const vector3& z,const point3& p0,const point3& p1);
	RHCoordSys3(const point3& p1,const point3& p2,const point3& p3);
	RHCoordSys3(Double phi,Double theta,Double zeta);

	~RHCoordSys3();

	// Accessors
	inline vector3    	&operator [] (Int i);	
	inline const vector3	&operator [] (Int i) const;

	// Other Methods
	RHCoordSys3	Rotate1(const vector3& axis, Double angle);
	RHCoordSys3	Rotate3(Double phi,Double theta,Double zeta);
	RHCoordSys3	Rotate3a(Double phi,Double theta,Double zeta);
	RHCoordSys3	RotateY();
	vector<Double>	RotAngles(RHCoordSys3 =RHCoordSys3()) const;	

//private:
	vector<vector3>	cs3;

	// internal Constructor
	RHCoordSys3(const vector3& x,const vector3& y,const vector3& z);
};

//	RHCoordSys	inlines
inline vector3 &RHCoordSys3::operator [] (Int i)
{
    return(cs3[i]);
}

inline const vector3 &RHCoordSys3::operator [] (Int i) const
{
    return(cs3[i]);
}

//	other functions
vector3	dirWCStoLCS(vector3 vDir1, RHCoordSys3 LCS);
vector3	dirLCStoWCS(vector3 vDir1, RHCoordSys3 LCS);
vector3	dirCS1toCS2(vector3 vDir1, RHCoordSys3 CS1, RHCoordSys3 CS2);

}	//	end namespace

std::ostream	&operator << (std::ostream &s, const BldgGeomLib::RHCoordSys3 &cs);
std::istream	&operator >> (std::istream &s, BldgGeomLib::RHCoordSys3 &cs);

