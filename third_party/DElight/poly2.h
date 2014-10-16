//	poly2.h
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

class	poly2
{
public:

	// Constructors
	poly2();
	poly2(const vector<point2>& VertList);
	~poly2();

	// Accessors
	inline point2    	&operator [] (Int i);	
	inline const point2	&operator [] (Int i) const;

	//	other...
	int				size() const;
	lineseg2		lsEdge(int ii) const;
	vector2			vEdge(int ii) const;
	Double			ExtAngle(int ii) const;
	Double			TotExtAngle() const;
	inline Double	xMax() const;
	inline Double	xMin() const;
	inline Double	yMax() const;
	inline Double	yMin() const;
	Double			Area() const;
	Double			Circumference() const;		//	length of boundary
	Double			CircumferenceRatio() const; //	ratio with circumference of equal-area circle
	point2			Centroid() const;
	bool			PointInPoly(const point2& p0) const;
	point2			RandInPoly() const;

protected:
	vector<point2>	vPoly;
	Double	vxMax, vxMin, vyMax, vyMin;
	void	vMinMax();
};

//	poly2	inlines
inline point2 &poly2::operator [] (Int i)
{
    return(vPoly[i]);
}

inline const point2 &poly2::operator [] (Int i) const
{
    return(vPoly[i]);
}

inline Double	poly2::xMax() const
{ return vxMax;}

inline Double	poly2::xMin() const
{ return vxMin;}

inline Double	poly2::yMax() const
{ return vyMax;}

inline Double	poly2::yMin() const
{ return vyMin;}

}	//	end namespace BldgGeomLib

std::ostream	&operator << (std::ostream &s, const BldgGeomLib::poly2 &p2);
std::istream	&operator >> (std::istream &s, BldgGeomLib::poly2 &p2);

