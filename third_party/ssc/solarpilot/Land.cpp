
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
#include "Land.h"
#include "math.h"
#include "Toolbox.h"
#include <math.h>
using namespace std;

void Land::Create(var_map &V)
{
    _var_land = &V.land;
    _bound_area = 0.;

    updateCalculatedParameters(V);
}

void Land::updateCalculatedParameters(var_map &V)
{
    //bound_area
    V.land.bound_area.Setval( _bound_area ); //land bound area. Either we have it or we don't. Something will have to have called calcLandArea for this to be set.
    //land_area
    V.land.land_area.Setval( _bound_area * V.land.land_mult.val/4046.86 + V.land.land_const.val );
    //min and max radius
    double exts[2];
    Land::getRadialExtents(V, exts, V.sf.tht.val );
    V.land.radmin_m.Setval( exts[0] );
    V.land.radmax_m.Setval( exts[1] );

}

double Land::getLandBoundArea()
{
    return _bound_area;
}

bool Land::InBounds(var_land &V, sp_point &P, double tht)
{
    //offset the point by tower x,y offset
    sp_point Po(P);
    
	//figure out whether the given point is inside the land area described by _boundary
	bool test = true;
	double prad = sqrt( pow(Po.x, 2) + pow(Po.y, 2) ); //radial position of the point relative to the tower
		
	if(V.is_bounds_scaled.val){	//Does the point lie within the limits scaling with tower height?
		test = (prad >= (tht*V.min_scaled_rad.val) && prad <= (tht*V.max_scaled_rad.val) );
		if(! test) return false;
	}
	if(V.is_bounds_fixed.val){	//Does the point also lie within the fixed limits?
		test = test && (prad >= V.min_fixed_rad.val && prad <= V.max_fixed_rad.val);
		if(! test) return false;
	}
	if(V.is_bounds_array.val)
    {
        if(! V.is_exclusions_relative.val )     //if the exclusions are relative, wait to shift the point until after exclusion tests
        {
            //since we can't offset the inclusions/exclusions polygons, just test with a shifted point
            Po.x += V.tower_offset_x.val;
            Po.y += V.tower_offset_y.val;
        }

		//Test all of the exclusions polygons first
		for(unsigned int i=0; i<V.exclusions.val.size(); i++)
        { 
			if( Toolbox::pointInPolygon(V.exclusions.val.at(i), Po) ) return false;	//if the point is in any exclusion, stop
		}

        if(V.is_exclusions_relative.val )   //if the point hasn't been shifted yet, do so now
        {
            Po.x += V.tower_offset_x.val;
            Po.y += V.tower_offset_y.val;
        }

		//Test all of the inclusions polygons. The point must lie in one or more inclusion.
		bool intest = V.inclusions.val.size() == 0;  //If there aren't any inclusions, all points are included. Otherwise initialize as false

        //check to make sure we aren't missing inclusion regions
        if( intest && !( V.is_bounds_scaled.val || V.is_bounds_fixed.val) )
            throw spexception("The land area in which heliostats may be placed is undefined. "
                              "Please specify the layout bounds where heliostats are allowed.");

		for(unsigned int i=0; i<V.inclusions.val.size(); i++){
			if( Toolbox::pointInPolygon(V.inclusions.val.at(i), Po) ){ intest = true; break;}
		}
		test = test && intest;
	}
	
	return test;
}

void Land::getExtents(var_map &V, double rval[])
{
	/*
	**CALL FOR GUI**
	Take the rval[2] array (output) and the variable map structure from the interface and calculate {radmin, radmax}.

	This returns the min and max radial distance of the heliostat field from the tower location.
	The land bounds can be specified using up to three constraints: 1 - scale with tower height, 
	2 - fixed distances, 3 - user-specified polygon.

	This method considers all methods used and enforces the bounds based on satisfaction of all of
	the active criteria.
	
	This array takes an optional argument "tht" which multiplies by the scaled radii if appropriate.
	By default tht = 1.0
	The method returns an array size=2: {double min, double max}
	*/
	double radmin = 0, radmax = 0;
	double tht = V.sf.tht.val; 
	bool is_bounds_scaled = V.land.is_bounds_scaled.val; 
	bool is_bounds_fixed = V.land.is_bounds_fixed.val; 
	bool is_bounds_array = V.land.is_bounds_array.val;
	
	if(is_bounds_scaled){
		double min_scaled_rad = V.land.min_scaled_rad.val;
		double max_scaled_rad = V.land.max_scaled_rad.val; 
		radmin = min_scaled_rad * tht;
		radmax = max_scaled_rad * tht;
	}
	if(is_bounds_fixed){
		double min_fixed_rad = V.land.min_fixed_rad.val; 
		double max_fixed_rad = V.land.max_fixed_rad.val; 
		if(min_fixed_rad > radmin || radmin == 0) radmin = min_fixed_rad;	//Only change if the fix min radius is larger than the previous bound
		if(max_fixed_rad < radmax || radmax == 0) radmax = max_fixed_rad;	//Only change if the fix max radius is smaller than the previous bound
	}
	if(is_bounds_array){
		
		//Find the maximum radius depending on the inclusions vectors
		double rad, trmax = -1.;
		for(unsigned int i=0; i<V.land.inclusions.val.size(); i++){
			//For each polygon in the inclusions
			for(unsigned int j=0; j<V.land.inclusions.val.at(i).size(); j++){
				rad = sqrt( pow(V.land.inclusions.val.at(i).at(j).x - V.land.tower_offset_x.val, 2) + pow(V.land.inclusions.val.at(i).at(j).y - V.land.tower_offset_y.val, 2) );
				if(fabs(rad) > trmax) trmax = rad;
			}
		}
        if( V.land.inclusions.val.size() > 0 )
        {
		    if(trmax < 0. ) trmax = tht*7.5;	//use the default if nothing is set
		    if(trmax < radmax || radmax == 0) radmax = trmax;
        }
        else
        {
            if( ! (is_bounds_scaled || is_bounds_fixed ) )
                //insufficient info provided
                throw spexception("Insufficient information provided to specify land bounds. At least 1 'inclusion' region must be provided if not specifying fixed or scaled bounds.");
        }

		//Find the minimum radius depending on the exclusions vector
		//values for finding the minimum radius
		double trmin = 9.e9; 
		sp_point T, pt1, N;
		T.Set(V.land.tower_offset_x.val, V.land.tower_offset_y.val, 0.);	//Tower location
		for(unsigned int i=0; i<V.land.inclusions.val.size(); i++){	//For each polygon in the inclusions
			//first check whether the tower lies within the polygon, in which case the minimum radius is zero
			if( Toolbox::pointInPolygon(V.land.inclusions.val.at(i), T) ){
				trmin = 0.;
				break;
			}
					
			int nincpt = (int)V.land.inclusions.val.at(i).size();
			for(int j=0; j<nincpt; j++){
				
				//Find the minimum radius depending on the inclusions vectors
				if(j<nincpt-1){
					pt1.Set(V.land.inclusions.val.at(i).at(j+1));
				}
				else{
					pt1.Set(V.land.inclusions.val.at(i).at(0));
				}

				//Find the closest point on the line defined by pt1 and pt0 to 'T'.
				Toolbox::line_norm_intersect(V.land.inclusions.val.at(i).at(j), pt1, T, N, rad);
				if(fabs(rad) < trmin) trmin = rad;

			}
		}
		
		//Adjust the minimum radius depending on the exclusions 
		sp_point ex1;
		double excheck = 9.e9;
		for(unsigned int i=0; i<V.land.exclusions.val.size(); i++){	//For each polygon in the exclusions
			
			//check whether the tower lies within the polygon. If not, we don't need to adjust
			if(! Toolbox::pointInPolygon(V.land.exclusions.val.at(i), T) ) continue;

			int nex = (int)V.land.exclusions.val.at(i).size();
			for(int j=0; j<nex; j++){
				
				if(j<nex-1) {
					ex1.Set(V.land.exclusions.val.at(i).at(j+1));
				}
				else{
					ex1.Set(V.land.exclusions.val.at(i).at(0));
				}
				//Find the closest point on the line defined by ex0 and ex1 to 'T'. This point
				//is 'N' with a distance 'rad' from T.
				Toolbox::line_norm_intersect(V.land.exclusions.val.at(i).at(j), ex1, T, N, rad);
				if(fabs(rad) < excheck) excheck = rad;
			}
		}
		if(excheck > trmin && excheck < 9.e9) trmin = excheck;		if(trmin > radmax) trmin = 0.001;	//Use a small number larger than zero if nothing is set
		if(trmin > radmin || radmin == 0) radmin = trmin;
	}
	rval[0] = radmin;
	rval[1] = radmax;
	
}

//void Land::getExtents(var_map &V, double rval[], double tht)
//{
//	/*
//	This returns the min and max radial distance of the heliostat field from the tower location.
//	The land bounds can be specified using up to three constraints: 1 - scale with tower height, 
//	2 - fixed distances, 3 - user-specified polygon.
//
//	This method considers all methods used and enforces the bounds based on satisfaction of all of
//	the active criteria.
//	
//	This array takes an optional argument "tht" which multiplies by the scaled radii if appropriate.
//	By default tht = 1.0
//	The method returns an array size=2: {double min, double max}
//	*/
//	double radmin = 0, radmax=0;
//	
//	if(V.land.is_bounds_scaled.val){
//		radmin = V.land.min_scaled_rad.val * tht;
//		radmax = V.land.max_scaled_rad.val * tht;
//	}
//	if(V.land.is_bounds_fixed.val){
//		if(V.land.min_fixed_rad.val > radmin || radmin == 0){radmin = V.land.min_fixed_rad.val;}	//Only change if the fix min radius is larger than the previous bound
//		if(V.land.max_fixed_rad.val < radmax || radmax == 0){radmax = V.land.max_fixed_rad.val;}	//Only change if the fix max radius is smaller than the previous bound
//	}
//	if(V.land.is_bounds_array.val){
//
//		//values for finding the maximum radius
//		double rad, trmax = -1.;
//		for(unsigned int i=0; i<V.land.inclusions.val.size(); i++){	//For each polygon in the inclusions
//			int nincpt = (int)V.land.inclusions.val.at(i).size();
//			for(int j=0; j<nincpt; j++){
//				
//				//Find the maximum radius depending on the inclusions vectors
//				rad = sqrt( pow(V.land.inclusions.val.at(i).at(j).x, 2) + pow(V.land.inclusions.val.at(i).at(j).y, 2) );
//				if(fabs(rad) > trmax) trmax = rad;
//			}
//		}
//		if(trmax < 0.) trmax = tht*7.5;	//use the default if nothing is set
//		if(trmax < radmax || radmax == 0){radmax = trmax;}
//
//		//values for finding the minimum radius
//		double trmin = 9.e9; 
//		sp_point T, pt1, N;
//		T.Set(0.,0.,0.);	//Tower location
//		for(unsigned int i=0; i<V.land.inclusions.val.size(); i++){	//For each polygon in the inclusions
//			//first check whether the tower lies within the polygon, in which case the minimum radius is zero
//			if( Toolbox::pointInPolygon(V.land.inclusions.val.at(i), T) ){
//				trmin = 0.;
//				break;
//			}
//					
//			int nincpt = (int)V.land.inclusions.val.at(i).size();
//			for(int j=0; j<nincpt; j++){
//				
//				//Find the minimum radius depending on the inclusions vectors
//				if(j<nincpt-1){
//					pt1.Set(V.land.inclusions.val.at(i).at(j+1));
//				}
//				else{
//					pt1.Set(V.land.inclusions.val.at(i).at(0));
//				}
//
//				//Find the closest point on the line defined by pt1 and pt0 to 'T'.
//				Toolbox::line_norm_intersect(V.land.inclusions.val.at(i).at(j), pt1, T, N, rad);
//				if(fabs(rad) < trmin) trmin = rad;
//
//			}
//		}
//		
//		//Adjust the minimum radius depending on the exclusions 
//		sp_point ex1;
//		double excheck = 9.e9;
//		for(unsigned int i=0; i<V.land.exclusions.val.size(); i++){	//For each polygon in the exclusions
//			
//			//check whether the tower lies within the polygon. If not, we don't need to adjust
//			if(! Toolbox::pointInPolygon(V.land.exclusions.val.at(i), T) ) continue;
//
//			int nex = (int)V.land.exclusions.val.at(i).size();
//			for(int j=0; j<nex; j++){
//				
//				if(j<nex-1) {
//					ex1.Set(V.land.exclusions.val.at(i).at(j+1));
//				}
//				else{
//					ex1.Set(V.land.exclusions.val.at(i).at(0));
//				}
//				//Find the closest point on the line defined by ex0 and ex1 to 'T'. This point
//				//is 'N' with a distance 'rad' from T.
//				Toolbox::line_norm_intersect(V.land.exclusions.val.at(i).at(j), ex1, T, N, rad);
//				if(fabs(rad) < excheck) excheck = rad;
//			}
//		}
//		if(excheck > trmin && excheck < 9.e9) trmin = excheck;
//
//		if(trmin > radmax) trmin = 0.001;	//Use a small number larger than zero if nothing is set
//		if(trmin > radmin || radmin == 0) radmin = trmin;
//	}
//	rval[0] = radmin;
//	rval[1] = radmax;
//}

void Land::getRadialExtents(var_map &V, double rval[2], double tht){
	/* 
	Sets the values of rval equal to the [min radius, max radius] of the field. This ONLY APPLIES to the 
	radial boundary settings and not to the polygonal boundary settings. If no radial boundaries are used,
	return [-1,-1].
	*/
	double radmin = 0, radmax = 0;
	
	if(V.land.is_bounds_scaled.val){
		radmin = V.land.min_scaled_rad.val * tht;
		radmax = V.land.max_scaled_rad.val * tht;
	}
	if(V.land.is_bounds_fixed.val){
		if(V.land.min_fixed_rad.val > radmin || radmin == 0){radmin = V.land.min_fixed_rad.val;}	//Only change if the fix min radius is larger than the previous bound
		if(V.land.max_fixed_rad.val < radmax || radmax == 0){radmax = V.land.max_fixed_rad.val;}	//Only change if the fix max radius is smaller than the previous bound
	}
	
	rval[0] = radmin;
	rval[1] = radmax;

	if(radmin == 0) rval[0] = -1.;
	if(radmax == 0) rval[1] = -1.;



}

double Land::calcPolyLandArea(var_land &V){
	//Simple summation of polygon inclusions minus exclusions

	//First add all the inclusions together
	double area = 0.;

	for(unsigned int i=0; i<V.inclusions.val.size(); i++){
		int np = (int)V.inclusions.val.at(i).size();
		int j=np-1;
		for(int k=0; k<np; k++){
			sp_point 
				*pj = &V.inclusions.val.at(i).at(j), 
				*pk = &V.inclusions.val.at(i).at(k);
			area += (pj->x + pk->x)*(pj->y - pk->y)/2.;
			j = k;
		}
	}
	area = fabs(area);

	//Now subtract the area of the exclusions
	double excs = 0.;
	for(unsigned int i=0; i<V.exclusions.val.size(); i++){
		int np = (int)V.exclusions.val.at(i).size();
		int j=np-1;
		for(int k=0; k<np; k++){
			sp_point 
				*pj = &V.exclusions.val.at(i).at(j), 
				*pk = &V.exclusions.val.at(i).at(k);
			excs += (pj->x + pk->x)*(pj->y - pk->y)/2.;
			j = k;
		}
	}	
	excs = fabs(excs);

	return area-excs;

}


void Land::calcLandArea(var_land &V, vector<sp_point> &layout)
{
	/* 
	Calculate the land area either using provided polygons, or using the convex hull around the heliostat field. 

	For polygons, assume that the geometry provided accurately represents the land area that is to be accounted for in the cost
	calculations. If only land exclusions are provided, use the convex hull method instead.
	*/

	if(V.is_bounds_array.val && (V.inclusions.val.size() > 0) )
	{
		_bound_area = calcPolyLandArea(V);
	}
	else
	{
		//Calculate the convex hull surrounding the heliostat positions
		std::vector<sp_point> hull;
		Toolbox::convex_hull(layout, hull);

		//Calculate the area of the convex hull
		_bound_area = Toolbox::area_polygon(hull);
	}

}

