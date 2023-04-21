// line2.h
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

class line2 
{
public:

	// Constructors
	
	line2();
	line2(const point2& p1, const vector2& dir);
	line2(const point2& p1, const point2& p2);
//	line2(const line2 &p);			// Copy constructor

	~line2();

	// Accessors
	point2	Origin() const;
	vector2	dir() const;

	// Other Methods
	vector2	normVec() const;
	Double	xIntercept() const;
	Double	yIntercept() const;
	point2 PointOnLine(Double param) const;
	Double	DistToPoint(const point2& pExt) const;
	int		intersect(const line2& lineExt, Double& param) const;


	// Private...
	
protected:

	point2	origin;
	vector2	lDir;

	Double		intersectG(const line2& lineExt) const;
	Double		intersectH(const line2& lineExt) const;

};

//	lineseg2
class lineseg2 : public line2
{
public:

	// Constructors
	lineseg2();
	lineseg2(const point2& p1, const vector2& dir, Double length);
	lineseg2(const point2& p1, const point2& p2);
//	lineseg2(const ray2 &r);			// Copy constructor
	~lineseg2();

	inline Double Length() const;
	point2	end(int ii) const;
	inline Double xMax() const;
	inline Double xMin() const;
	inline Double yMax() const;
	inline Double yMin() const;

	int	intersect(const lineseg2& lsExt, Double& param) const;

protected:
	Double length;
};

//inlines need to be in this file or compiler won't inline them
inline Double lineseg2::Length() const
{
	return length;
}

inline Double lineseg2::xMax() const
{
	return (end(2)[0] > origin[0] ? end(2)[0] : origin[0]);
}

inline Double lineseg2::xMin() const
{
	return (end(2)[0] < origin[0] ? end(2)[0] : origin[0]);
}

inline Double lineseg2::yMax() const
{
	return (end(2)[1] > origin[1] ? end(2)[1] : origin[1]);
}

inline Double lineseg2::yMin() const
{
	return (end(2)[1] < origin[1] ? end(2)[1] : origin[1]);
}

//	ray2
class ray2 : public line2
{
public:

	// Constructors
	ray2();
	ray2(const point2& p1, const vector2& dir);
	ray2(const point2& p1, const point2& p2);
//	ray2(const ray2 &r);			// Copy constructor
	~ray2();

	bool	PointsTowardLine(const line2& lineExt) const;
	int		intersect(const line2& lineExt, Double& param) const;
	int		intersect(const lineseg2& lsExt, Double& param) const;
};

}	//	end namespace BldgGeomLib

std::ostream	&operator << (std::ostream &s, const BldgGeomLib::line2 &l);
std::istream	&operator >> (std::istream &s, BldgGeomLib::line2 &l);

std::ostream	&operator << (std::ostream &s, const BldgGeomLib::lineseg2 &ls);
std::istream	&operator >> (std::istream &s, BldgGeomLib::lineseg2 &ls);

std::ostream	&operator << (std::ostream &s, const BldgGeomLib::ray2 &r);
std::istream	&operator >> (std::istream &s, BldgGeomLib::ray2 &r);


