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

#ifndef _RECEIVER_H_
#define _RECEIVER_H_ 1


#include <vector>
#include "Toolbox.h"
#include "mod_base.h"
#include "definitions.h"

//Declare referenced classes
class Receiver;
class SolarField;


/*Define a structure that contains information on each mesh point for the receiver surface*/
struct FluxPoint {
	sp_point location;
	Vect normal;
	double 
		/*x, //[m] Node x-position (-East, +West) in global coordinates
		y, //[m] Node y-position (-South, +North) in global coordinates
		z, //[m] Node z-position (+vertical) relative to receiver optical height
		i,	//Normal std::vector to the point, i direction
		j,	//Normal std::vector to the point, j direction
		k,	//Normal std::vector to the point, k direction*/
		maxflux, //[W/m2] Maximum allowable flux on this element
		flux, //[W/m2] Actual flux on this element
		area_factor;	//[0..1] Factor adjusting the area of the point to accommodate edge effects
	bool over_flux;		//Flag indicating whether this element has exceeded its flux limit
	
	FluxPoint();
	
	void Setup(double xloc, double yloc, double zloc, Vect &norm, double flux_max, double Area_factor = 1.);
	void Setup(sp_point &loc, Vect &norm, double flux_max, double Area_factor = 1.);

};

typedef std::vector<std::vector<FluxPoint> > FluxGrid;

//The FluxSurface class
class FluxSurface : public mod_base
{
	/* 
	The FluxSurface class provides a data structure that describes in detail the surface on which 
	flux from the heliostat field will be collected. The class is a set of points with associated 
	normal vectors describing their location in x,y,z space and a unit std::vector that is normal to 
	that point which indicates the orientation of the absorber surface.

	The class contains methods to develop the flux surface from standard receiver geometries as 
	described in DELSOL.
	*/

	int 
		_id, //Absorber surface unique ID
		_type, //Absorber surface type
		/* 
		Types:
		0 | Continuous closed cylinder - external
		1 | Continuous open cylinder - external
		2 | Continuous open cylinder - internal cavity
		3 | Planar rectangle
		4 | Planar ellipse
		5 | Discrete closed N-polygon - external
		6 | Discrete open N-polygon - external
		7 | Discrete open N-polygon - internal cavity
		*/
		_nflux_x,	//Number of flux points horizontally
		_nflux_y;	//Number of flux points vertically
	double
		_width,	
		_height,
		_radius,	//only applies to curved surfaces. If none, should be 0.
		_area,
		_span_ccw,
		_span_cw,
		_max_flux;	//Maximum allowable flux on this surface
	double _max_observed_flux;

	Vect
		_normal;
	sp_point
		_offset;

	FluxGrid _flux_grid; // std::vector containing grid

	Receiver *_rec_parent;

public:
	//----Access functions
	Receiver *getParent();
	int getId();
	FluxGrid *getFluxMap();
	int getFluxNX();
	int getFluxNY();
	sp_point *getSurfaceOffset();
	double getSurfaceWidth();
	double getSurfaceHeight();
	double getSurfaceRadius();
	double getSurfaceArea();
	double getTotalFlux();
	double getMaxObservedFlux();

	void setParent(Receiver *recptr);
	void setFluxPrecision(int nx, int ny);
	void setMaxFlux(double maxflux);
	void setNormalVector(Vect &vect);
	void setSurfaceOffset(sp_point &loc);
	void setSurfaceSpanAngle(double span_min, double span_max);
	void setSurfaceGeometry(double height, double width, double radius = 0.);
	void setMaxObservedFlux(double fmax);
	
	//Declare the scripts
	void DefineFluxPoints(var_receiver &V, int rec_geom, int nx=-1, int ny=-1);
	void Normalize();
	void ClearFluxGrid();
};

typedef std::vector<FluxSurface> FluxSurfaces;


//Create the main receiver class
class Receiver : public mod_base
 {

	double
		_absorber_area,	//Effective area of the receiver absorber panels
		_therm_loss,	//Receiver thermal loss at design
		_piping_loss;	//Thermal loss from non-absorber receiver piping

	bool
		_is_enabled;		//Is template enabled?

	double _thermal_eff;	//An estimate of the thermal efficiency

	PointVect
		_normal; //Unit std::vector of the normal to the reciever
	int
		_rec_geom; //Specific receiver geometry, defined in DefineReceiverGeometry

	FluxSurfaces 
		_surfaces; //A std::vector containing sub-vectors that define the geometry of receiver surfaces
    
    var_receiver *_var_receiver;    //pointer to applicable parameter map

public:	
	
	void Create(var_receiver &V, double tht);	//create from variable map
    void updateCalculatedParameters(var_receiver &V, double tht);
	
	/* Define an enumeration structure for receiver geometry types */
	struct REC_GEOM_TYPE { 
		enum A { CYLINDRICAL_CLOSED, CYLINDRICAL_OPEN, CYLINDRICAL_CAV, PLANE_RECT, 
			PLANE_ELLIPSE, POLYGON_CLOSED, POLYGON_OPEN, POLYGON_CAV}; 
	};

	//Declare "GET" access functions
	static double getReceiverWidth(var_receiver &V); //[m] Returns either receiver width or diameter, depending on configuration
	double getReceiverThermalLoss();
	double getReceiverPipingLoss();
    double getThermalEfficiency();
    double getAbsorberArea();
    int getGeometryType();
    var_receiver* getVarMap();
	void CalculateNormalVector(PointVect &NV);	//Returns the normal std::vector and receiver centroid that represents the optimal optical incidence
	void CalculateNormalVector(sp_point &Hloc, PointVect &NV);	//(Overload) for non-flat receivers, closest normal std::vector given a viewpoint std::vector
	FluxSurfaces *getFluxSurfaces();

    bool isReceiverEnabled();
    void isReceiverEnabled(bool enable);

	//Declare the scripts
	void DefineReceiverGeometry(int nflux_x = 1, int nflux_y = 1);
	void CalculateAbsorberArea();
	void CalculateThermalLoss(double load, double v_wind);
	void CalculateThermalEfficiency(double dni, double dni_des, double v_wind, double q_des);
	double CalculateApparentDiameter(sp_point &Hloc); //[m] Return the apparent receiver diameter given the polygonal structure

 } ;

#endif
