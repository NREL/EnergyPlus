//	point3.cpp
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


#include <cctype>
#include <iomanip>
#include <iostream>
#include <sstream>
using namespace std;

#include "DEF.H"
#include "CONST.H"
#include "vector2.h"
#include "point2.h"
#include "vector3.h"
#include "point3.h"
// writewndo() Error handler include
#include "DElightManagerC.h"

using namespace BldgGeomLib;

ostream &operator << (ostream &s, const point3 &p)
{
	Int w = s.width();

	return(s << '[' << p[0] << ' ' << setw(w) << p[1] << ' ' << setw(w) << p[2] << ']');
}

istream &operator >> (istream &s, point3 &p)	//	WLC 06/06/2003 major rewrite
{
    point3	result;
    Char	c;
	std::ostringstream osstream;
	
	// Expected format: [1 2 3]
	
    while (s >> c && isspace(c)) {;} // skip through spaces

	if (s.eof()) return(s);
	if (s.fail()) {
//		cerr << "Point3:ReadError1: unrecoverable failbit\n";
		osstream << "point3:ReadError1: unrecoverable failbit\n";
		writewndo(osstream.str(),"e");
		return(s);
	}
		
    if (c != '[') {
		s.putback(c);
	    s.clear(ios::failbit);
	    return(s);
	}
	//	else ...
	s >> result[0] >> result[1] >> result[2];	

	if (!s)	{
//		cerr << "Point3:ReadError2: Expected number\n";
		osstream << "point3:ReadError2: Expected number\n";
		writewndo(osstream.str(),"e");
		return(s);
	}
		
	while (s >> c && isspace(c)) {;} // skip through spaces

	if (c != ']')	{
    	s.clear(ios::failbit);
//	    cerr << "Point3:ReadError3: Expected ']' - got \'" << c << "\'" << "\n";
		osstream << "point3:ReadError3: Expected ']' - got \'" << c << "\'" << "\n";
		writewndo(osstream.str(),"e");
	    return(s);
    }
	
	p = result;
    return(s);
}

