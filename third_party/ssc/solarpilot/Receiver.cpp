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

#include "Receiver.h"
#include <math.h>
#include "exceptions.hpp"
#include <vector>
using namespace std;

//----------------FluxPoint -----------------
FluxPoint::FluxPoint(){over_flux = false; flux = 0.;};
	
void FluxPoint::Setup(double xloc, double yloc, double zloc, Vect &norm, double flux_max, double Area_factor){
	location.x = xloc; location.y = yloc; location.z = zloc;
	normal.i = norm.i; normal.j = norm.j; normal.k = norm.k;
	maxflux = flux_max;
	over_flux = false;
	area_factor = Area_factor;
};
void FluxPoint::Setup(sp_point &loc, Vect &norm, double flux_max, double Area_factor){
	location.x = loc.x; location.y = loc.y; location.z = loc.z;
	normal.i = norm.i; normal.j = norm.j; normal.k = norm.k;
	maxflux = flux_max;
	over_flux = false;
	area_factor = Area_factor;
}


//----------------Absorber surface-----------------
Receiver *FluxSurface::getParent(){return _rec_parent;}
int FluxSurface::getId() {return _id;};
FluxGrid *FluxSurface::getFluxMap(){return &_flux_grid;}
int FluxSurface::getFluxNX(){return _nflux_x;}
int FluxSurface::getFluxNY(){return _nflux_y;}
sp_point *FluxSurface::getSurfaceOffset(){return &_offset;}
double FluxSurface::getSurfaceWidth(){return _width;}
double FluxSurface::getSurfaceHeight(){return _height;}
double FluxSurface::getSurfaceRadius(){return _radius;}
double FluxSurface::getSurfaceArea(){return _area;}
double FluxSurface::getMaxObservedFlux(){return _max_observed_flux;}

void FluxSurface::setParent(Receiver *recptr){_rec_parent = recptr;}
void FluxSurface::setFluxPrecision(int nx, int ny){_nflux_x=nx; _nflux_y = ny;}
void FluxSurface::setMaxFlux(double maxflux){_max_flux = maxflux;}
void FluxSurface::setNormalVector(Vect &vect){
	_normal = vect;
}
void FluxSurface::setSurfaceOffset(sp_point &loc){_offset = loc;}
void FluxSurface::setSurfaceSpanAngle(double span_min, double span_max){_span_ccw = span_min; _span_cw = span_max;}
void FluxSurface::setSurfaceGeometry(double height, double width, double radius){
	_width = width;
	_height = height;
	_radius = radius;	//if radius is 0, assume flat surface.
						//For nonzero radii, the WIDTH WILL BE
}
void FluxSurface::setMaxObservedFlux(double fmax){ _max_observed_flux = fmax; }
	
//Declare the scripts
void FluxSurface::ClearFluxGrid(){
	for(unsigned int i=0; i<_flux_grid.size(); i++){
		for(unsigned int j=0; j<_flux_grid.at(i).size(); j++){
			_flux_grid.at(i).at(j).flux = 0.;
		}
	}
}

void FluxSurface::DefineFluxPoints(var_receiver &V, int rec_geom, int nx, int ny){
	/*
	Given the receiver geometry in "_parent", create a grid of flux hit test points.

	Flux points are in the global coordinate system but do not include receiver offset or tower height.

    */
	if(nx > 0) _nflux_x = nx;
	if(ny > 0) _nflux_y = ny;

	//if(rec_geom > -1) _type = rec_geom;	//Use the argument. Otherwise, use the locally set value

	switch (rec_geom)
	{
	case Receiver::REC_GEOM_TYPE::CYLINDRICAL_CLOSED:
	{			//0 | Continuous closed cylinder - external
		
		//The flux for this geometry assumes that the cylinder is vertical (no zenith displacement)
        //Flux points are stored beginning lower edge, clockwise extent. Final entry upper edge counterclockwise extent

		_area = _height * _radius * PI * 2.;

		//Resize
		_flux_grid.resize(_nflux_x);	//number of rows

		//The azimuthal span taken up by each flux point
		double daz = (_span_cw - _span_ccw)/double(_nflux_x);	

		double faz;
		sp_point floc;
		Vect fnorm;
		double dz = _height/double(_nflux_y);	//height of each flux node
		for(int i=0; i<_nflux_x; i++){
			_flux_grid.at(i).resize(_nflux_y);	//number of columns

            faz = _span_cw - daz*(0.5 + (double)i); //The azimuth angle of the point
			//Calculate the normal vector
			fnorm.i = sin(faz);
			fnorm.j = cos(faz);
			fnorm.k = 0.;
			//Calculate the x-y position
			floc.x = fnorm.i * _radius;
			floc.y = fnorm.j * _radius;
			//Calculate the z position
			for(int j=0; j<_nflux_y; j++){
				floc.z = -_height/2.+dz*(0.5 + (double)j);
				//Set the location
				_flux_grid.at(i).at(j).Setup(floc, fnorm, _max_flux);
			}
		}
		break;
	}
	case Receiver::REC_GEOM_TYPE::CYLINDRICAL_OPEN:
	case Receiver::REC_GEOM_TYPE::CYLINDRICAL_CAV:
	{		
		//1 | Continuous open cylinder - external
		//2 | Continuous open cylinder - internal cavity

		/*
		The flux map for this geometry allows an angling in the zenith direction of the surface.
		The coordinates of the flux map are with respect to the xyz location of the receiver centroid.
		These coordinates account for zenith rotation of the receiver. 
        
        Flux points are stored beginning lower edge, clockwise extent. Final entry upper edge counterclockwise extent
		
        */
        
        double intmult = ( rec_geom == Receiver::REC_GEOM_TYPE::CYLINDRICAL_CAV ? -1. : 1. );   //-1 multiplier for values that are inverted on the internal face of a cylinder
        double spansize = (_span_cw - _span_ccw) * intmult;

		_area = _height * _radius * spansize;

		//Resize
		_flux_grid.resize(_nflux_x); //Number of rows
		
		//The azimuthal span taken up by each point
		double daz = spansize/double(_nflux_x)  ;
		double rec_az = atan2(_normal.i, _normal.j);	//The azimuth angle of the receiver
		double rec_zen = acos(_normal.k);	//The zenith angle of the receiver at rec_az
		double rec_dh = _height/double(_nflux_y);

		double faz, fzen;
		sp_point floc;
		Vect fnorm;
		for(int i=0; i<_nflux_x; i++){
			_flux_grid.at(i).resize(_nflux_y);	//number of columns

			faz = _span_cw - daz*(0.5+double(i));
			fzen = rec_zen*cos(rec_az - faz);	//Local receiver zenith angle
			for(int j=0; j<_nflux_y; j++){
				//Calculate the xyz position assuming no rotation, then rotate into the position of the receiver
				floc.x = _radius*sin(faz);
				floc.y = _radius*cos(faz);
				floc.z = -_height/2.+rec_dh*(0.5 + double(j));
				//Calculate the normal vector
				fnorm.i = sin(faz)*cos(fzen)*intmult;
				fnorm.j = cos(faz)*cos(fzen)*intmult;
				fnorm.k = sin(fzen);

				//rotate about the x axis (zenith)
				Toolbox::rotation(rec_zen, 0, floc);	//Rotate the actual point
				Toolbox::rotation(rec_zen, 0, fnorm);	//rotate the normal vector
				//rotate about the z axis (azimuth)
				Toolbox::rotation(rec_az, 2, floc);     //point
                Toolbox::rotation(rec_az, 2, fnorm);    //normal vector

				//the point "floc" is now rotated into the receiver coordinates
				_flux_grid.at(i).at(j).Setup(floc, fnorm, _max_flux);
			}
		}
		break;
	}
	case Receiver::REC_GEOM_TYPE::PLANE_RECT:
	{		//3 | Planar rectangle
		/* 
		The receiver is a rectangle divided into _nflux_x nodes in the horizontal direction and
		_nflux_y nodes in the vertical direction. Each node is of area A_rec/(_nflux_x * _nflux_y).
		*/

		_area = _height * _width;

		_flux_grid.resize(_nflux_x); //Number of rows
		double rec_az = atan2(_normal.i, _normal.j);	//The azimuth angle of the receiver
		double rec_zen = acos(_normal.k);	//The zenith angle of the receiver at rec_az
		double rec_dh = _height/double(_nflux_y);
		double rec_dw = _width/double(_nflux_x);

		sp_point floc;
		for(int i=0; i<_nflux_x; i++){
			_flux_grid.at(i).resize(_nflux_y);	//number of columns
			for(int j=0; j<_nflux_y; j++){
				//Calculate the position assuming no rotation, then rotate according to the receiver orientation
				floc.x = (-_width + rec_dw)/2. + i*rec_dw;
				floc.y = (-_height + rec_dh)/2. + j*rec_dh;
				floc.z = 0.;
				
				//Rotate about the x axis (zenith)
				Toolbox::rotation(-rec_zen, 0, floc);
				//Rotate about the z axis (azimuth)
				Toolbox::rotation(PI + rec_az, 2, floc);

				//Set up the point
				_flux_grid.at(i).at(j).Setup(floc, _normal, _max_flux);
			}
		}
		break;
	}
	case Receiver::REC_GEOM_TYPE::PLANE_ELLIPSE:
	{		//4 | Planar ellipse
		/* 
		The receiver is a rectangle divided into _nflux_x nodes in the horizontal direction and
		_nflux_y nodes in the vertical direction. Each node is of area A_rec/(_nflux_x * _nflux_y).
		*/

		_area = PI * _width * _height/4.;

		_flux_grid.resize(_nflux_x); //Number of rows
		double rec_az = atan2(_normal.i, _normal.j);	//The azimuth angle of the receiver
		double rec_zen = acos(_normal.k);	//The zenith angle of the receiver at rec_az
		double rec_dh = _height/double(_nflux_y);
		double rec_dw = _width/double(_nflux_x);
		
		sp_point floc;
		for(int i=0; i<_nflux_x; i++){
			_flux_grid.at(i).resize(_nflux_y);	//number of columns
			for(int j=0; j<_nflux_y; j++){
				//Calculate the position assuming no rotation, then rotate according to the receiver orientation
				floc.x = (-_width + rec_dw)/2. + i*rec_dw;
				floc.y = 0.;
				floc.z = (-_height + rec_dh)/2. + j*rec_dh;
				
				//Calculate an area factor to account for incongruence of the rectangular node with the elliptical aperture
				double rect[] = {floc.x, floc.z, rec_dw, rec_dh};
				double ellipse[] = {_width, _height};
				double afactor = fmin(fmax(Toolbox::intersect_ellipse_rect(rect, ellipse)/(rec_dw*rec_dh), 0.), 1.);

				//Rotate about the x axis (zenith)
				Toolbox::rotation(-rec_zen + PI/2., 0, floc);       //unlike plane rect, the points start in X-Z plane
				//Rotate about the z axis (azimuth)
				Toolbox::rotation(PI + rec_az, 2, floc);

				//Set up the point
				_flux_grid.at(i).at(j).Setup(floc, _normal, _max_flux, afactor);
			}
		}
		break;
	}
	case Receiver::REC_GEOM_TYPE::POLYGON_CLOSED:
	{
		
		//The flux for this geometry assumes that the cylinder is vertical (no zenith displacement)

		_area = _height * _radius * PI * 2.;

		//Resize
		_flux_grid.resize(_nflux_x);	//number of rows

		//The azimuthal span taken up by each flux point
		double span = (_span_cw - _span_ccw);
		double daz = span/double(_nflux_x);	//span will always be 2 PI for this

		int npanels = V.n_panels.val; 
		//calculate the angular span each panel occupies
		double panel_az_span = span / (double)npanels;
		
		//pre-calculate normal vectors for each of the panels
		vector<Vect> panel_normals(npanels);
		vector<double> panel_radii(npanels);
		vector<double> panel_azimuths(npanels);
		
        double rec_az = atan2(_normal.i, _normal.j);	//The azimuth angle of the receiver
		double rec_zen = acos(_normal.k);	//The zenith angle of the receiver at rec_az
		
		for(int i=0; i<npanels; i++){
			double paz = _span_cw - (i + 0.5)*panel_az_span;
			double 
				sinpaz = sin(paz),
				cospaz = cos(paz);
			panel_normals.at(i).Set(sinpaz, cospaz, 0.);
			panel_radii.at(i) = _radius*cos(panel_az_span/2.) ;
			panel_azimuths.at(i) = paz + rec_az;

            //rotate panels according to receiver elevation/azimuth
            Toolbox::rotation(rec_zen + PI/2., 0, panel_normals.at(i));
            Toolbox::rotation(rec_az, 2, panel_normals.at(i));

		}

		double faz;
		Vect fnorm;
		for(int i=0; i<_nflux_x; i++){
			_flux_grid.at(i).resize(_nflux_y);	//number of columns

			faz = _span_cw - daz*(0.5 + double(i)) + rec_az;	//The azimuth angle of the point

			//which panel does this flux point belong to?
			int ipanl = (int)(floor(faz / panel_az_span));

			//the normal vector is the same as the panel to which it belongs
			fnorm.Set(panel_normals.at(ipanl));
			
			//Determine the flux point position, which must lie along an existing panel
			
			double h = panel_radii.at(i)/cos( panel_azimuths.at(i) - faz ); //hypotenuse 

			//x-y location of flux point
			sp_point floc;
			floc.x = h * sin(faz - rec_az);
			floc.y = h * cos(faz - rec_az);

			//Calculate the z position
			double dz = _height/double(_nflux_y);	//height of each flux node
			for(int j=0; j<_nflux_y; j++){
				floc.z = -_height/2.+dz*(0.5 + double(j));

                // rotate
                Toolbox::rotation(rec_zen + PI/2., 0, floc);
                Toolbox::rotation(rec_az, 2, floc);

				//Set the location
				_flux_grid.at(i).at(j).Setup(floc, fnorm, _max_flux);
			}
		}
		break;
	}
	case Receiver::REC_GEOM_TYPE::POLYGON_OPEN:
	case Receiver::REC_GEOM_TYPE::POLYGON_CAV:
	default:
		break;
	}

}

double FluxSurface::getTotalFlux(){
	//Determine the total flux on the surface
	double flux_tot=0.;
	for(int i=0; i<_nflux_x; i++){
		for(int j=0; j<_nflux_y; j++){
			flux_tot += _flux_grid.at(i).at(j).flux;
		}
	}
	return flux_tot;
}

void FluxSurface::Normalize(){
	/* 
	Express each node on the flux map as a relative contribution toward the total 
	absorbed flux, which is equal to 1.0. 
	e.g:
	sum_i=0->nfx( sum_j=0->nfy( flux[i][j] )) = 1.0
	*/

	double flux_tot = getTotalFlux();

	//Normalize by the total
	for(int i=0; i<_nflux_x; i++){
		for(int j=0; j<_nflux_y; j++){
			_flux_grid.at(i).at(j).flux *= 1./flux_tot;
		}
	}

}

//-----------------Receiver----------------

void Receiver::Create(var_receiver &V, double tht)
{
    _var_receiver = &V;

    _is_enabled = V.is_enabled.val;
	
	_normal = PointVect(0.,0.,0.,0.,1.,0.); //Unit vector of the normal to the reciever

	DefineReceiverGeometry();

    updateCalculatedParameters(V, tht);

}

void Receiver::updateCalculatedParameters(var_receiver &V, double tht)
{


    //update the receiver geometry type
    switch(_var_receiver->rec_type.mapval())
    {
        case var_receiver::REC_TYPE::EXTERNAL_CYLINDRICAL:
        {
		    if(! _var_receiver->is_open_geom.val)
            {
			    _rec_geom = _var_receiver->is_polygon.val ? 
				    Receiver::REC_GEOM_TYPE::POLYGON_CLOSED : 
				    Receiver::REC_GEOM_TYPE::CYLINDRICAL_CLOSED 
                    ;		/*	0 | Continuous closed cylinder - external	*/

		    }
		    else{
			    _rec_geom = _var_receiver->is_polygon.val ? 
				    Receiver::REC_GEOM_TYPE::POLYGON_OPEN : 
				    Receiver::REC_GEOM_TYPE::CYLINDRICAL_OPEN
                    ;		/*	1 | Continuous open cylinder - external	*/
		    }
			break;
	    }
        //case var_receiver::REC_TYPE::CAVITY: 
        //{
        //	if(! _var_receiver->is_polygon.val){		/*	2 | Continuous open cylinder - internal cavity	*/
	    //		_rec_geom = Receiver::REC_GEOM_TYPE::CYLINDRICAL_CAV ;
	    //	}
	    //	else{
	    //		_rec_geom = Receiver::REC_GEOM_TYPE::POLYGON_CAV;			/*	7 | Discrete open N-polygon - internal cavity	*/
	    //	}
	    //}
        case var_receiver::REC_TYPE::FLAT_PLATE:
        {   //Flat plate
		    if(_var_receiver->aperture_type.mapval() == var_receiver::APERTURE_TYPE::RECTANGULAR){
			    _rec_geom = ( Receiver::REC_GEOM_TYPE::PLANE_RECT );			/*	3 | Planar rectangle	*/
		    }
		    else{
			    _rec_geom = ( Receiver::REC_GEOM_TYPE::PLANE_ELLIPSE );			/* 4 | Planar ellipse		*/
		    }
            break;
	    }
        default:
            break;
    };


	//Receiver area
	CalculateAbsorberArea();

    //aspect
	double height = V.rec_height.val;
	double aspect;
    switch(V.rec_type.mapval() )
    {
    case var_receiver::REC_TYPE::EXTERNAL_CYLINDRICAL:
    {
		//External receiver
		aspect = height/V.rec_diameter.val;
        break;
	}
	//else if(V.rec_type.val == Receiver::REC_TYPE::CAVITY){
		//cavity
		//aspect = height/V.rec_width.val;
	//}
    case var_receiver::REC_TYPE::FLAT_PLATE:
    {
		//flat plate
		aspect = height/V.rec_width.val;
        break;
	}
    //else{
    default:
        throw spexception("Invalid receiver type in UpdateCalculatedMapValues()");
    }

	V.rec_aspect.Setval( aspect ); 
	V.absorber_area.Setval( _absorber_area );   //calculated by CalculateAbsorberArea

	//receiver optical height
	double zoff = V.rec_offset_z.val;
	V.optical_height.Setval( tht + zoff );


	//Estimated heat loss
	double tp = 0.;
	for(int i=0; i<(int)V.therm_loss_load.val.ncells(); i++)
        tp += V.therm_loss_load.val.at(i);

	double therm_loss_base = V.therm_loss_base.val;
	V.therm_loss.Setval( therm_loss_base * _absorber_area/1.e3 * tp);

	//Piping loss
	V.piping_loss.Setval( (V.piping_loss_coef.val * tht + V.piping_loss_const.val)/1.e3 );
		
}


//------------Access functions
double Receiver::getReceiverWidth(var_receiver &V) 
{
    //[m] Returns either receiver width or diameter, depending on configuration
    if(V.rec_type.mapval() == var_receiver::REC_TYPE::EXTERNAL_CYLINDRICAL) 
    {
        return V.rec_diameter.val;
    } 
    else 
    {
        return V.rec_width.val;
    } 
} 

double Receiver::getReceiverThermalLoss()
{
    return _therm_loss;
}

double Receiver::getReceiverPipingLoss()
{
    return _piping_loss;
}

double Receiver::getThermalEfficiency()
{
    return _thermal_eff;
}

double Receiver::getAbsorberArea()
{
    return _absorber_area;
}

int Receiver::getGeometryType()
{
    return _rec_geom;
}
    
FluxSurfaces *Receiver::getFluxSurfaces(){ return &_surfaces; }

var_receiver* Receiver::getVarMap(){return _var_receiver;}

bool Receiver::isReceiverEnabled()
{
    return _is_enabled;
}

void Receiver::isReceiverEnabled(bool enable)
{
    _is_enabled = enable;
}

void Receiver::CalculateNormalVector(PointVect &NV){
	//If no normal vector is supplied, provide the default
	sp_point Vn;
	Vn.Set(0., 0., 0.);
	Receiver::CalculateNormalVector(Vn, NV);
}

void Receiver::CalculateNormalVector(sp_point &Hloc, PointVect &NV){
	/* 
	This subroutine should be used to calculate the normal vector to the receiver for a given heliostat location.
	Ultimately, the optical calculations should not use this method to calculate the normal vector. Instead, use
	the normal vector that is assigned to the receiver surface during setup. 

	In the case of continuous cylindrical surfaces, this method can be called during optical calculations.

	Given a heliostat at point Hloc{x,y,z}, return a normal vector to the receiver absorber surface.
	
	*/
	
    double rec_elevation = _var_receiver->rec_elevation.val * D2R;
    double rec_az = _var_receiver->rec_azimuth.val * D2R;

	//This will have to be modified to allow for multi-surface receivers and polygons. TO DO
	switch(_rec_geom)
	{
	case Receiver::REC_GEOM_TYPE::CYLINDRICAL_CLOSED:
	case Receiver::REC_GEOM_TYPE::POLYGON_CLOSED:

		//External cylinder
		//use the view vector to determine the best normal to report
		
		//Polar coords for azimuth and zenith angles
		double vaz; 
		vaz = atan2(Hloc.x,Hloc.y);
		
		//What is the approximate aim point for the surface?
		NV.z = _var_receiver->optical_height.Val();
		NV.x = _var_receiver->rec_diameter.val/2. * sin(vaz) + _var_receiver->rec_offset_x.val;		//[m] x-location of surface at angle vaz, given radius _var_receiver->rec_diameter.val/2
		NV.y = _var_receiver->rec_diameter.val/2. * cos(vaz) + _var_receiver->rec_offset_y.val;		//[m] y-location "" "" ""

		//calculate the normal vector
        NV.i = sin(vaz)*cos(rec_elevation);
		NV.j = cos(vaz)*cos(rec_elevation);
		NV.k = sin(rec_elevation);
		break;
	case Receiver::REC_GEOM_TYPE::CYLINDRICAL_OPEN:
	case Receiver::REC_GEOM_TYPE::CYLINDRICAL_CAV:
	case Receiver::REC_GEOM_TYPE::PLANE_RECT:
	case Receiver::REC_GEOM_TYPE::PLANE_ELLIPSE:
		//All other types should be simply equal to the user-specified az/zen
		//The approximate aim point is:
		NV.x = _var_receiver->rec_offset_x.val;
		NV.y = _var_receiver->rec_offset_y.val;
		NV.z = _var_receiver->optical_height.Val();
		//Calculate the unit vector
		NV.i = sin(rec_az)*cos(rec_elevation);
		NV.j = cos(rec_az)*cos(rec_elevation);
		NV.k = sin(rec_elevation);
		break;
	default:
		throw spexception("Unsupported receiver type");
	}

	return;

}

//------------------Scripts------------------

//Initialization call to create the receiver surfaces
void Receiver::DefineReceiverGeometry(int nflux_x, int nflux_y) {

	/* 
	The process of defining receiver geometry for each receiver should be:

	1) Indicate which specific geometry type should be used with "_rec_geom"
	2) Calculate and set the number of surfaces used for the recever. Resize "_surfaces".
	3) Calculate and set the normal vector for each surface (if not curved surfaces) with setNormalVector(Vect).
	4) Setup the geometry etc.. including setSurfaceGeometry, setSurfaceOffset, setSurfaceSpanAngle, if applicable.
	5) Define the precision of the flux map.
	6) Define the maximum flux for each panel.
	7) Call the method to set up the flux hit test grid.

	Geometries are:
	0	|	Continuous closed cylinder - external
	1	|	Continuous open cylinder - external	
	2	|	Continuous open cylinder - internal cavity
	3	|	Planar rectangle
	4	|	Planar ellipse
	5	|	Discrete closed N-polygon - external	
	6	|	Discrete open N-polygon - external
	7	|	Discrete open N-polygon - internal cavity
	*/
    int rec_type = _var_receiver->rec_type.mapval();

	if(rec_type == var_receiver::REC_TYPE::EXTERNAL_CYLINDRICAL){ //External
		//if(! _is_polygon){
		/*continuous external cylinders. Setup shares some common features..*/
			
		//this uses a single curved surface
		_surfaces.resize(1);
				
		FluxSurface *S = &_surfaces.at(0);
		S->setParent(this);

		//Do setup
		sp_point loc;
        loc.Set(_var_receiver->rec_offset_x.val, _var_receiver->rec_offset_y.val, _var_receiver->rec_offset_z.val);
		S->setSurfaceGeometry( _var_receiver->rec_height.val, 0., _var_receiver->rec_diameter.val/2. );
		S->setSurfaceOffset( loc );
		//For continuous cylindrical surfaces, the normal vector will define the azimuth and zenith of the receiver surface.
		Vect nv;
        double rec_az = _var_receiver->rec_azimuth.val * D2R;
        double rec_el = _var_receiver->rec_elevation.val * D2R;
		nv.i = sin(rec_az)*cos(rec_el);
		nv.j = cos(rec_az)*cos(rec_el);
		nv.k = sin(rec_el);
		S->setNormalVector(nv); 
						
		if(! _var_receiver->is_open_geom.val){
			//_rec_geom = _var_receiver->is_polygon.val ? 
			//	Receiver::REC_GEOM_TYPE::POLYGON_CLOSED : 
			//	Receiver::REC_GEOM_TYPE::CYLINDRICAL_CLOSED 
   //             ;		/*	0 | Continuous closed cylinder - external	*/

			S->setSurfaceSpanAngle(-PI,PI);	//Full surround
            //_var_receiver->span_min.val = -PI;
            //_var_receiver->span_max.val = PI; //enforce closedness - overwrite any other values
		}
		else{
			//_rec_geom = _var_receiver->is_polygon.val ? 
			//	Receiver::REC_GEOM_TYPE::POLYGON_OPEN : 
			//	Receiver::REC_GEOM_TYPE::CYLINDRICAL_OPEN
   //             ;		/*	1 | Continuous open cylinder - external	*/
			//A curved surface that doesn't form a closed circle. Extents are defined by the span angles.
			S->setSurfaceSpanAngle(_var_receiver->span_min.val*D2R, _var_receiver->span_max.val*D2R);
		}
			
		//Default setup will be for a single flux test point on the surface. In more detailed
		//flux mapping runs, this can be changed to whatever the desired resolution is.
		S->setFluxPrecision(nflux_x,nflux_y);
		S->setMaxFlux(_var_receiver->peak_flux.val);
		S->DefineFluxPoints(*_var_receiver, _rec_geom);

		//}
		//else{
		//	/* Discrete external cylinders of polygonal shape */

		//	//The flux surface of the polygon will still be represented as a single surface
		//	_surfaces.resize(1);

		//	FluxSurface *S = &_surfaces.front();
		//	S->setParent(this);

		//	//Setup the geometry etc.. including setSurfaceGeometry, setSurfaceOffset
		//	double pdaz, wpanel;
		//	if(! _is_open_geom){
		//		_rec_geom = Receiver::REC_GEOM_TYPE::POLYGON_CLOSED;		/*	5 | Discrete closed N-polygon - external	*/

		//		//Calculate the panel width
		//		pdaz = 2.*PI/double(_var_receiver->n_panels.val);	//The azimuthal span of each panel
		//		wpanel = _var_receiver->rec_diameter.val/2.*tan(pdaz); //Width of each panel
		//							
		//	}
		//	else{
		//		_rec_geom = Receiver::REC_GEOM_TYPE::POLYGON_OPEN;		/*	6 | Discrete open N-polygon - external	*/

		//		//Calculate the panel width based on the total span angle. The span angle is defined
		//		//such that the minimum bound of the angle passes through (1) a vector from the center of
		//		//the polygon inscribed circle through the centroid of the farthest panel in the CCW 
		//		//direction, and (2) a vector from the center of teh polygon inscribed circle through 
		//		//the centroid of the farthest panel in the CW direction.
		//		pdaz = (_var_receiver->span_max.val*D2R - _var_receiver->span_min.val*D2R)/double(_var_receiver->n_panels.val-1);
		//		wpanel = _var_receiver->rec_diameter.val/2.*tan(pdaz);	//width of each panel
		//			
		//	}
		//	S->setSurfaceGeometry(_var_receiver->rec_height.val, wpanel);

		//	//Calculate the azimuth angle of the receiver panel
		//	double paz = _var_receiver->panel_rotation.val*D2R + pdaz*double(i);
		//	//Calculate the elevation angle of the panel
		//	double pzen = _rec_elevation*cos(_var_receiver->panel_rotation.val*D2R-paz);
		//	//Set the surface normal vector
		//	Vect nv;
		//	nv.i = sin(paz)*sin(pzen);
		//	nv.j = cos(paz)*sin(pzen);
		//	nv.k = cos(pzen);
		//	S->setNormalVector(nv);

		//	//Calculate the centroid of the panel in global XYZ coords
		//	sp_point pc;
		//	pc.x = nv.i * _var_receiver->rec_diameter.val/2.;
		//	pc.y = nv.j * _var_receiver->rec_diameter.val/2.;
		//	pc.z = nv.k * _var_receiver->rec_diameter.val/2.;
		//	S->setSurfaceOffset(pc);

		//	//Define the precision of the flux map.
		//	S->setFluxPrecision(nflux_x,nflux_y);
		//	S->setMaxFlux(_var_receiver->peak_flux.val);
		//	//Call the method to set up the flux hit test grid.
		//	S->DefineFluxPoints(_rec_geom);

		//}
	}
	//else if(rec_type == var_receiver::REC_TYPE::CAVITY){ //Cavity
	//	if(! _var_receiver->is_polygon.val){		/*	2 | Continuous open cylinder - internal cavity	*/
	//		
	//		//1) Indicate which specific geometry type should be used with "_rec_geom"
	//		_rec_geom = Receiver::REC_GEOM_TYPE::CYLINDRICAL_CAV ;

	//		//2) Calculate and set the number of surfaces used for the recever. Resize "_surfaces".
	//		_surfaces.resize(1);
	//		FluxSurface *S = &_surfaces.at(0);
	//		S->setParent(this);

	//		//3) Calculate and set the normal vector for each surface (if not curved surfaces) with setNormalVector(Vect).

	//		sp_point loc;
 //           loc.Set( _var_receiver->rec_offset_x.val, _var_receiver->rec_offset_y.val, _var_receiver->rec_offset_z.val );
	//		S->setSurfaceGeometry( _var_receiver->rec_height.val, _var_receiver->rec_width.val, _var_receiver->rec_diameter.val/2. );
	//		S->setSurfaceOffset( loc );
	//		//For continuous cylindrical surfaces, the normal vector will define the azimuth and zenith of the receiver surface.
	//		Vect nv;
 //           double rec_az = _var_receiver->rec_azimuth.val * D2R;
 //           double rec_el = _var_receiver->rec_elevation.val * D2R;

	//		nv.i = sin(rec_az)*cos(rec_el);
	//		nv.j = cos(rec_az)*cos(rec_el);
	//		nv.k = sin(rec_el);
	//		S->setNormalVector(nv); 
	//					
	//		//4) Setup the geometry etc.. including setSurfaceGeometry, setSurfaceOffset, setSurfaceSpanAngle, if applicable.
	//		S->setSurfaceSpanAngle(_var_receiver->span_min.val*D2R, _var_receiver->span_max.val*D2R);
	//					
	//		//5) Define the precision of the flux map.
	//		
	//		//Default setup will be for a single flux test point on the surface. In more detailed
	//		//flux mapping runs, this can be changed to whatever the desired resolution is.
	//		S->setFluxPrecision(nflux_x,nflux_y);
	//		
	//		//6) Define the maximum flux for each panel.
	//		S->setMaxFlux(_var_receiver->peak_flux.val);
	//		
	//		//7) Call the method to set up the flux hit test grid.
	//		S->DefineFluxPoints(*_var_receiver, _rec_geom);			

	//	}
	//	else{
	//		_rec_geom = Receiver::REC_GEOM_TYPE::POLYGON_CAV;			/*	7 | Discrete open N-polygon - internal cavity	*/

	//		//Use the number of panels as the number of polygon facets. Each facet is its own surface.
	//		_surfaces.resize(_var_receiver->n_panels.val);

	//		for(int i=0; i<_var_receiver->n_panels.val; i++){
	//			FluxSurface *S = &_surfaces.at(i);
	//			S->setParent(this);

	//			//Setup the geometry etc.. including setSurfaceGeometry, setSurfaceOffset
	//			double pdaz, wpanel;
	//			
	//			/*
	//			Calculate the panel width based on the total span angle. The span angle is defined
	//			such that the minimum bound of the angle passes through (1) a vector from the center of
	//			the polygon inscribed circle through the centroid of the farthest panel in the CCW 
	//			direction, and (2) a vector from the center of teh polygon inscribed circle through 
	//			the centroid of the farthest panel in the CW direction.
	//			*/
	//			pdaz = (_var_receiver->span_max.val*D2R - _var_receiver->span_min.val*D2R)/double(_var_receiver->n_panels.val-1);
	//			wpanel = _var_receiver->rec_diameter.val/2.*tan(pdaz);	//width of each panel
	//				
	//			
	//			S->setSurfaceGeometry(_var_receiver->rec_height.val, wpanel);
 //               
	//			//Calculate the azimuth angle of the receiver panel
	//			double paz = _var_receiver->panel_rotation.val*D2R + pdaz*double(i);
	//			//Calculate the elevation angle of the panel
	//			double pzen = _var_receiver->rec_elevation.val*D2R*cos(_var_receiver->panel_rotation.val*D2R-paz);
	//			//Set the surface normal vector
	//			Vect nv;
	//			nv.i = -sin(paz)*sin(pzen);
	//			nv.j = -cos(paz)*sin(pzen);
	//			nv.k = -cos(pzen);
	//			S->setNormalVector(nv);

	//			//Calculate the centroid of the panel in global XYZ coords
	//			sp_point pc;
	//			pc.x = nv.i * _var_receiver->rec_diameter.val/2.;
	//			pc.y = nv.j * _var_receiver->rec_diameter.val/2.;
	//			pc.z = nv.k * _var_receiver->rec_diameter.val/2.;
	//			S->setSurfaceOffset(pc);

	//			//Define the precision of the flux map.
	//			S->setFluxPrecision(nflux_x,nflux_y);
	//			S->setMaxFlux(_var_receiver->peak_flux.val);
	//			//Call the method to set up the flux hit test grid.
	//			S->DefineFluxPoints(*_var_receiver, _rec_geom);

	//		}

	//	}
	//}
	else if(rec_type == var_receiver::REC_TYPE::FLAT_PLATE){ //Flat plate
		//1) Indicate which specific geometry type should be used with "_rec_geom"
		//if(_var_receiver->aperture_type.mapval() == var_receiver::APERTURE_TYPE::RECTANGULAR){
		//	_rec_geom = ( Receiver::REC_GEOM_TYPE::PLANE_RECT );			/*	3 | Planar rectangle	*/
		//}
		//else{
		//	_rec_geom = ( Receiver::REC_GEOM_TYPE::PLANE_ELLIPSE );			/* 4 | Planar ellipse		*/
		//}
			
		//2) Calculate and set the number of surfaces used for the recever. Resize "_surfaces".
		_surfaces.resize(1);
		FluxSurface *S = &_surfaces.at(0);

		//3) Calculate and set the normal vector for each surface (if not curved surfaces) with setNormalVector(Vect).

		sp_point loc;
        loc.Set( _var_receiver->rec_offset_x.val, _var_receiver->rec_offset_y.val, _var_receiver->rec_offset_z.val );
		S->setSurfaceGeometry( _var_receiver->rec_height.val, _var_receiver->rec_width.val, 0. );
		S->setSurfaceOffset( loc );
		//For continuous cylindrical surfaces, the normal vector will define the azimuth and zenith of the receiver surface.
		Vect nv;
        double rec_az = _var_receiver->rec_azimuth.val *D2R;
        double rec_elevation = _var_receiver->rec_elevation.val *D2R;
		nv.i = sin(rec_az)*cos(rec_elevation);
		nv.j = cos(rec_az)*cos(rec_elevation);
		nv.k = sin(rec_elevation);
		S->setNormalVector(nv);	
						
		//4) Setup the geometry etc.. including setSurfaceGeometry, setSurfaceOffset, setSurfaceSpanAngle, if applicable.
		S->setSurfaceSpanAngle(-PI/2., PI/2.);	
						
		//5) Define the precision of the flux map.
		S->setFluxPrecision(nflux_x,nflux_y);
			
		//6) Define the maximum flux for each panel.
		S->setMaxFlux(_var_receiver->peak_flux.val);
			
		//7) Call the method to set up the flux hit test grid.
		S->DefineFluxPoints(*_var_receiver, _rec_geom);	

	}

	//Set up the absorber panels


}

void Receiver::CalculateAbsorberArea(){

	/* 
	Calculate the receiver absorber surface area based on the geometry type. This doesn't consider
	the area of individual tubes or elements, only the area of the major geometrical surfaces.

	The local variable _absorber_area is set, which can be accessed via
	getReceiverAbsorberArea()
	*/

	int recgeom = _rec_geom; 

	switch (recgeom)
	{
	case Receiver::REC_GEOM_TYPE::CYLINDRICAL_CLOSED:
		_absorber_area = ( _var_receiver->rec_height.val * _var_receiver->rec_diameter.val * PI );
		break;
	case Receiver::REC_GEOM_TYPE::CYLINDRICAL_OPEN:
	case Receiver::REC_GEOM_TYPE::CYLINDRICAL_CAV:
		_absorber_area = ( _var_receiver->rec_height.val * _var_receiver->rec_diameter.val * fabs(_var_receiver->span_max.val*D2R - _var_receiver->span_min.val*D2R)/2. );
		break;
	case Receiver::REC_GEOM_TYPE::PLANE_RECT:
		_absorber_area = ( _var_receiver->rec_height.val * _var_receiver->rec_width.val );
		break;
	case Receiver::REC_GEOM_TYPE::PLANE_ELLIPSE:
		_absorber_area = ( PI * _var_receiver->rec_height.val * _var_receiver->rec_width.val/4. );
		break;
	case Receiver::REC_GEOM_TYPE::POLYGON_CLOSED:
		_absorber_area = ( _var_receiver->rec_height.val * (double)_var_receiver->n_panels.val * _var_receiver->rec_diameter.val/2.*tan(2.*PI/_var_receiver->n_panels.val) );
		break;
	case Receiver::REC_GEOM_TYPE::POLYGON_OPEN:
	case Receiver::REC_GEOM_TYPE::POLYGON_CAV:
		_absorber_area = ( _var_receiver->rec_height.val * (double)_var_receiver->n_panels.val * _var_receiver->rec_diameter.val/2.*tan(fabs(_var_receiver->span_max.val*D2R - _var_receiver->span_min.val*D2R)/(double)(_var_receiver->n_panels.val-1)) );
		break;
	default:
		break;
	}
	
}

void Receiver::CalculateThermalLoss(double load, double v_wind){
	/* 
	Calculate the thermal loss from the receiver. Update the local values of thermal and piping loss.

    _therm_loss [MWt]   Local value updated
    _piping_loss [MWt]   Local value updated

	Load is a normalized thermal load for the receiver. 
	V_wind is m/s.
	*/

	double
		fload = 0.,
		fwind = 0.;
	for(int i=0; i<(int)_var_receiver->therm_loss_load.val.ncells(); i++)
		fload += _var_receiver->therm_loss_load.val.at(i)*pow(load, i);
	for(int i=0; i<(int)_var_receiver->therm_loss_wind.val.ncells(); i++)
		fwind += _var_receiver->therm_loss_wind.val.at(i)*pow(v_wind, i);

	_therm_loss =  _var_receiver->therm_loss_base.val * fload * fwind * _absorber_area *1.e-3 ;    //_therm_loss_base [kWt/m2]

	//piping
	_piping_loss =  (_var_receiver->piping_loss_coef.val * _var_receiver->optical_height.Val() + _var_receiver->piping_loss_const.val)*1.e-3 ;

}

void Receiver::CalculateThermalEfficiency(double dni, double dni_des, double v_wind, double q_des){
    /* 
    Calculate thermal efficiency and update local values.

    Inputs:
        DNI         W/m2    DNI at current time
        dni_des     W/m2    DNI at system design point
        v_wind      m/s     Wind velocity at current time
        q_des       MWt     Design-point receiver output power

    Sets:
        _thermal_eff
        _therm_loss     (via CalculateThermalLoss)
        _piping_loss    (via CalculateThermalLoss)
    */

	double load = dni/dni_des;
	CalculateThermalLoss(load, v_wind);

    _thermal_eff = 1. - _therm_loss / (_therm_loss + q_des ); 

}

double Receiver::CalculateApparentDiameter(sp_point &Hloc)
{ 
	/* 
	[m] Return the apparent receiver diameter given the polygonal structure

	Take the specified heliostat location, the number of receiver panels, and the orientation
	of the primary receiver panel, and calculate the apparent width of the reciever.
	
	This convention assumes that the receiver diameter CIRCUMSCRIBES all panels. That is, 
	the maximum receiver apparent width is the specified receiver diameter.
	*/

	//only valid for cylindrical receivers
	switch (_rec_geom)
	{
	case Receiver::REC_GEOM_TYPE::CYLINDRICAL_CLOSED:
		return _var_receiver->rec_diameter.val;
		break;
	case Receiver::REC_GEOM_TYPE::POLYGON_CLOSED:
	{
		//First determine the azimuthal span between the heliostat location vector and the receiver 
		//main panel normal vector
		double alpha = fabs(atan2(Hloc.x, Hloc.y) - _var_receiver->rec_azimuth.val*D2R);
		//Calculate the difference between the angle and the nearest panel normal
		double theta_hat = fmod(alpha, 2.*PI/_var_receiver->n_panels.val);
		//finally the width is:
		return cos(theta_hat)*_var_receiver->rec_diameter.val;
		break;
	}
	default:
		throw spexception("Attempting to calculate an apparent diameter for an unsupported receiver geometry.");
		break;
	}
}

