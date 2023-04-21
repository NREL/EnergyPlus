// line3.h
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

class line3 
{
public:

	// Constructors
	
	line3();
	line3(const point3& p1, const vector3& dir);
	line3(const point3& p1, const point3& p2);
//	line3(const line3 &p);			// Copy constructor

	~line3();

	// Accessors
	point3	Origin() const;
	vector3	dir() const;

	// Other Methods
	vector3	normVec() const;
	point3 PointOnLine(double param) const;
	double	DistTo(const point3& pExt) const;

	// Private...
	
protected:

	point3	origin;
	vector3	lDir;
};

//	lineseg3
class lineseg3 : public line3
{
public:

	// Constructors
	lineseg3();
	lineseg3(const point3& p1, const vector3& dir, double length);
	lineseg3(const point3& p1, const point3& p2);
//	LineSeg2(const lineseg3 &ls);			// Copy constructor
	~lineseg3();

	double Length() const;
	point3	end(int ii) const;
	inline double xMax() const;
	inline double xMin() const;
	inline double yMax() const;
	inline double yMin() const;
	inline double zMax() const;
	inline double zMin() const;

protected:
	double length;
};

ostream	&operator << (ostream &s, const lineseg3 &ls);
istream	&operator >> (istream &s, lineseg3 &ls);

//inlines need to be in this file or compiler won't inline them
inline double lineseg3::xMax() const
{
	return (end(2)[0] > origin[0] ? end(2)[0] : origin[0]);
}

inline double lineseg3::xMin() const
{
	return (end(2)[0] < origin[0] ? end(2)[0] : origin[0]);
}

inline double lineseg3::yMax() const
{
	return (end(2)[1] > origin[1] ? end(2)[1] : origin[1]);
}

inline double lineseg3::yMin() const
{
	return (end(2)[1] < origin[1] ? end(2)[1] : origin[1]);
}

inline double lineseg3::zMax() const
{
	return (end(2)[2] > origin[2] ? end(2)[2] : origin[2]);
}

inline double lineseg3::zMin() const
{
	return (end(2)[2] < origin[2] ? end(2)[2] : origin[2]);
}

//	ray3
class plane3;	//	forward declaration
class ray3 : public line3
{
public:

	// Constructors
	ray3();
	ray3(const point3& p1, const vector3& dir);
	ray3(const point3& p1, const point3& p2);
//	ray3(const ray3 &r);			// Copy constructor
	~ray3();

	bool	PointsToward(const plane3& p3) const;
	bool	intersect(const plane3& pl3, double& param) const;
};

}	//	end namespace BldgGeomLib

std::ostream	&operator << (std::ostream &s, const BldgGeomLib::line3 &l);
std::istream	&operator >> (std::istream &s, BldgGeomLib::line3 &l);

std::ostream	&operator << (std::ostream &s, const BldgGeomLib::lineseg3 &ls);
std::istream	&operator >> (std::istream &s, BldgGeomLib::lineseg3 &ls);

std::ostream	&operator << (std::ostream &s, const BldgGeomLib::ray3 &r);
std::istream	&operator >> (std::istream &s, BldgGeomLib::ray3 &r);


