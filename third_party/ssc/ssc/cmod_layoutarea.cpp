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

#include "core.h"
//#include "lib_weatherfile.h"
#include "lib_util.h"
#include "Toolbox.h"


static var_info _cm_vtab_layoutarea[] = {
/*   VARTYPE           DATATYPE         NAME                         LABEL                                          UNITS     META        GROUP          REQUIRED_IF         CONSTRAINTS         UI_HINTS*/

	
	
	{ SSC_INPUT,        SSC_MATRIX,      "positions",                 "Positions within calculataed area",          "",       "",         "layoutarea",   "*",                "",                "" },        
	
	/* outputs */
	{ SSC_OUTPUT,       SSC_MATRIX,      "convex_hull",               "Convex hull bounding the region",            "",       "",         "layoutarea",   "*",                "",                "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "area",                      "Area inside the convex hull",                "",       "",         "layoutarea",   "*",                "",                "" },

	var_info_invalid };


#ifdef _MSC_VER
#define mysnprintf _snprintf
#else
#define mysnprintf snprintf
#endif

class cm_layoutarea : public compute_module
{
	/*struct Point
	{
		double x, y;
	};*/

public:
	
	cm_layoutarea()
	{
		add_var_info( _cm_vtab_layoutarea );
	}

	void exec( )
	{
		
		util::matrix_t<double> positions;
		//get the matrix of points
		get_matrix("positions", positions);
		//put into an array of points
		std::vector<sp_point> pos_pts;
		pos_pts.reserve( positions.nrows() );

		for(int i=0; i<(int)positions.nrows(); i++){
			pos_pts.push_back( sp_point () );
			pos_pts.back().x = positions.at(i, 0);
			pos_pts.back().y = positions.at(i, 1);
		}


		//Calculate the convex hull surrounding the heliostat positions
		std::vector<sp_point> hull;
		Toolbox::convex_hull(pos_pts, hull);

		//Calculate the area of the convex hull
		double area = Toolbox::area_polygon(hull);


		//return the results
		assign("area", (ssc_number_t)(area*0.000247105));	//acres
		ssc_number_t *hull_t = allocate( "convex_hull", hull.size(), 2);
		for(int i=0; i<(int)hull.size(); i++){
			hull_t[i * 2] = (ssc_number_t)hull.at(i).x;
			hull_t[i * 2 + 1] = (ssc_number_t)hull.at(i).y;
		}
		

	}
};


DEFINE_MODULE_ENTRY( layoutarea, "Layout Area Calculation", 0 )
