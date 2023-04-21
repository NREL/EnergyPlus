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

#ifndef _HELIOSTAT_H_
#define _HELIOSTAT_H_ 1
#include <vector>
#include <math.h>
#include "heliodata.h"
#include "mod_base.h"
#include "definitions.h"

//declare derived classes referenced in the heliostat class
class Flux;
class Reflector;
class SolarField;
class Receiver;
class Ambient;

class Heliostat : public mod_base
 {

	sp_point
		_location, //Location of heliostat in the field (0,0,Zt) is the location of the tower base
		_aim_point,	//constant aim point for the heliostat
		_aim_fluxplane;	//Aim point with respect to the flux plane
	Vect
		_track, //The tracking vector for the heliostat
		_tower_vect,  //Heliostat-to-tower unit vector
		_cant_vect;	//Canting vector (not normalized)
	std::vector<Heliostat*>
		*_neighbors; //pointer to a vector of neighboring heliostats
	matrix_t<Reflector>
		_panels; //Array of cant panels
	std::vector<sp_point>
		_corners,	//Position in global coordinates of the heliostat corners (used for blocking and shadowing)
		_shadow;	//Position in global coordinates of the heliostat shadow
	Heliostat* _master_template;	//Pointer to the template used to create this heliostat
		
	matrix_t<double>
		_mu_MN,		//Normalized mirror shape hermite expansion coefficients (n_terms x n_terms)
		_mu_S,		//Moments of sunshape
		_mu_G,		//Moments of the error distribution
		_mu_M,		//Moments of the mirror shape
		_mu_F,		//Flux moments distrubution - result
		_hcoef,		//Hermite coefficients
		_hc_tht; 		//Hermite coefs depending on tower height - equiv. to mu_F, reused in optimization calcs

	bool
		_in_layout, // Is the heliostat included in the final layout?
		_is_user_canted,	//Are the panels canted according to user-specified values?
		_is_enabled;		//Is template enabled?


	int 
		_id, //Unique ID number for the heliostat/surface
		_group[2];	//Integers {row,col} used to determine which local group the heliostat is in. 

	double
		_xfocal, //[m] focal length in the i^ direction
		_yfocal; //[m] focal length in the j^ direction

	helio_perf_data
		eff_data;
	double
		_slant,		//[m] Heliostat slant range - path length from centroid to receiver aim point
		_zenith,	//[rad] Heliostat tracking zenith angle
		_azimuth,	//[rad] Heliostat tracking azimuth angle
        _r_collision,   //[m] Collision radius of the heliostat
        _area,          //[m2] reflective area of the heliostat
		_image_size_xy[2];	//[m/m] Image size on the receiver plane in {x,y}, normalized by tower height
	std::string
		_helio_name;		//Heliostat template name
	Receiver *_which_rec;	//Which of the receivers is the heliostat pointing at?

    var_heliostat *_var_helio; //pointer to applicable variable map
		
public:

	//Declare other subroutines
	void Create(var_map &V, int htemp_number);
    void updateCalculatedParameters(var_map &V, int htemp_number);

	void installPanels();	//Define the cant panel locations, pointing vectors, and shape
	void updateTrackVector(Vect &sunvect);	//Update the tracking vector for the heliostat
	double calcTotalEfficiency();
    static void calcAndSetAimPointFluxPlane(sp_point &aimpos_abs, Receiver &Rec, Heliostat &H);
	void resetMetrics();
	void CopyImageData(const Heliostat *Hsrc);

	//Declare access functions
	int getId();
	int *getGroupId();		//(row,col) nodes
	bool IsInLayout();
	double getFocalX();
	double getFocalY();
	double getSlantRange();
	Receiver *getWhichReceiver();
	double getRadialPos();
	double getAzimuthalPos();
	Reflector *getPanelById(int id);
	Reflector *getPanel(int row, int col);
	matrix_t<Reflector> *getPanels();
	Vect *getTrackVector();	//return the tracking vector
	Vect *getTowerVector(); // return the helio-tower unit vector
	Vect *getCantVector();	//Return the canting vector (not normalized)
	sp_point *getLocation(); //Get location vector
	sp_point *getAimPoint();	//Get the heliostat aim point on the receiver
	sp_point *getAimPointFluxPlane();	//aim point in the flux plane coordinates 
	helio_perf_data *getEfficiencyObject();
	double getTotalReflectivity();
	double getEfficiencyTotal();
	double getEfficiencyCosine();
	double getEfficiencyAtten();
	double getEfficiencyIntercept();
	double getEfficiencyBlock();
	double getEfficiencyShading();
	double getEfficiencyCloudiness();
	double getPowerToReceiver();
	double getPowerValue();
	double getRankingMetricValue();
	double getAzimuthTrack();
	double getZenithTrack();
    double getArea();
    double getCollisionRadius();
	std::vector<Heliostat*> *getNeighborList();
	std::vector<sp_point> *getCornerCoords();
	std::vector<sp_point> *getShadowCoords();
	matrix_t<double> *getMirrorShapeNormCoefObject();
	matrix_t<double> *getMirrorShapeCoefObject();
	matrix_t<double> *getSunShapeCoefObject();
	matrix_t<double> *getErrorDistCoefObject();
	matrix_t<double> *getFluxMomentsObject();
	matrix_t<double> *getHermiteCoefObject();
	matrix_t<double> *getHermiteNormCoefObject();
	double *getImageSize();
	void getImageSize(double &sigx_n, double &sigy_n);
	std::string *getHeliostatName();
	void getSummaryResults( std::vector<double> &results );
	Heliostat* getMasterTemplate();
    var_heliostat* getVarMap();

	bool IsUserCant(); //Fetch
	void IsUserCant(bool setting);	//Set
    bool IsEnabled(); //fetch
    void IsEnabled(bool enable); //set

	void setId(int id);
	void setGroupId(int row, int col);
	void setInLayout(bool in_layout);
	void setNeighborList(std::vector<Heliostat*> *list);
	void setEfficiencyCosine(double eta_cos);
	void setEfficiencyAtmAtten(double eta_att);
	void setEfficiencyIntercept(double eta_int);
	void setEfficiencyBlocking(double eta_block);
	void setEfficiencyShading(double eta_shadow);
	void setEfficiencyCloudiness(double eta_cloud);
	void setEfficiencyTotal(double eta_tot);
	void setRankingMetricValue(double rval);
	void setLocation(double x, double y, double z);
	void setAimPoint(double x, double y, double z);
	void setAimPoint(sp_point &Aim);
	void setAimPointFluxPlane(sp_point &Aim);
	void setAimPointFluxPlane(double x, double y, double z);
	void setTrackVector(Vect &tr);	//Set the tracking vector
	void setTowerVector(Vect &tow); //Set the helio-tower vector
	void setTrackAngleZenith(double zenith);
	void setTrackAngleAzimuth(double azimuth);
	void setTrackAngles(double azimuth, double zenith);
	void setCantVector(Vect &cant);
	void setCantVector(double cant[3]);
	void setSlantRange(double L);
	void setFocalLengthX(double L);
	void setFocalLengthY(double L);
	void setFocalLength(double L);
	void setWhichReceiver(Receiver *rec);
	void setPowerToReceiver(double P);
	void setPowerValue(double P);
	void setImageSize(double sigx_n, double sigy_n);
	void setMasterTemplate(Heliostat *htemp);

 } ;

class Reflector {
	double 
		_width, //[m] reflector width (type=1)
		_height, //[m] reflector height (type=1)
		_diameter, //[m] reflector diameter (type=2)
		_focal_length; //[m] Focal distance, (if < 0, assume infinite focus)
	int 
		_id,	//Unique identifier for this reflector within a larger group
		_type;	//Which type of reflector (1=rectangular, 2=circular, 3=user-defined)
	matrix_t<PointVect>
		_geometry;	//A vector of points w/ orientation that define the outline of the geometry (type=3)
	PointVect
		_locate_vector;	//PointVect that locates and aims the reflector relative to it's frame of reference, (type=1,2)

public:
	//Constructors
	Reflector();
	
	//Get-Set methods
	int getId();
	double getWidth();
	double getHeight();
	double getDiameter();
	double getFocalLength();
	int getType();
	PointVect *getOrientation();

	void setId(int id);
	void setType(int type);
	void setWidth(double width);
	void setHeight(double height);
	void setDiameter(double diam);
	void setPosition(double x, double y, double z);
	void setAim(double i, double j, double k);
	void setAim( Vect &V);
	void setOrientation(double x, double y, double z, double i, double j, double k);
	void setOrientation(PointVect &PV);
	
	//Define functions
	void setDefaults();
};


class HelioTemplate : public Heliostat {
public:
	HelioTemplate(){};
	~HelioTemplate(){};

	//Add methods here to help the layout tool decide which heliostat to use.
};


#endif
