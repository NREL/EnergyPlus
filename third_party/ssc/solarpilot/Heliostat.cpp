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

#include <vector>
#include "math.h"

#include "Heliostat.h"
#include "SolarField.h"
#include "Flux.h"
#include "heliodata.h"
#include "solpos00.h"
using namespace std;

//declare referenced classes

//Accessors
double Heliostat::calcTotalEfficiency(){ return eff_data.calcTotalEfficiency(); }
int Heliostat::getId(){return _id;}
int *Heliostat::getGroupId(){return _group;}		//(row,col) nodes
double Heliostat::getFocalX(){return _xfocal;}
double Heliostat::getFocalY(){return _yfocal;}
double Heliostat::getSlantRange(){return _slant;}	//[m]
Receiver *Heliostat::getWhichReceiver(){return _which_rec;}
double Heliostat::getRadialPos(){ return sqrt( pow(_location.x, 2) + pow(_location.y, 2) + pow(_location.z, 2) ); }
double Heliostat::getAzimuthalPos(){return atan2(_location.x, _location.y); }	//Radians
matrix_t<Reflector> *Heliostat::getPanels(){return &_panels;}
Vect *Heliostat::getTrackVector(){return &_track;}	//return the tracking vector
Vect *Heliostat::getTowerVector(){return &_tower_vect;} // return the helio-tower unit vector
Vect *Heliostat::getCantVector(){return &_cant_vect;}	//Return the canting vector (not normalized)
sp_point *Heliostat::getLocation(){return &_location;} //Get location vector
sp_point *Heliostat::getAimPoint(){return &_aim_point;}	//Get the heliostat aim point on the receiver
sp_point *Heliostat::getAimPointFluxPlane(){return &_aim_fluxplane;}	//aim point in the flux plane coordinates 
helio_perf_data *Heliostat::getEfficiencyObject(){return &eff_data;}
double Heliostat::getTotalReflectivity(){return eff_data.reflectivity * eff_data.soiling;}
double Heliostat::getEfficiencyTotal(){return eff_data.eta_tot;}
double Heliostat::getEfficiencyCosine(){return eff_data.eta_cos;}
double Heliostat::getEfficiencyAtten(){return eff_data.eta_att;}
double Heliostat::getEfficiencyIntercept(){return eff_data.eta_int;}
double Heliostat::getEfficiencyBlock(){return eff_data.eta_block;}
double Heliostat::getEfficiencyShading(){return eff_data.eta_shadow;}
double Heliostat::getEfficiencyCloudiness(){return eff_data.eta_cloud;}
double Heliostat::getPowerToReceiver(){return eff_data.power_to_rec;}
double Heliostat::getPowerValue(){return eff_data.power_value;}
double Heliostat::getRankingMetricValue(){return eff_data.rank_metric;}
double Heliostat::getAzimuthTrack(){return _azimuth;}
double Heliostat::getZenithTrack(){return _zenith;}
double Heliostat::getCollisionRadius(){return _r_collision;}
double Heliostat::getArea(){return _area;}
vector<Heliostat*> *Heliostat::getNeighborList(){return _neighbors;}
vector<sp_point> *Heliostat::getCornerCoords(){return &_corners;}
vector<sp_point> *Heliostat::getShadowCoords(){return &_shadow;}
matrix_t<double> *Heliostat::getMirrorShapeNormCoefObject(){return &_mu_MN;}
matrix_t<double> *Heliostat::getMirrorShapeCoefObject(){return &_mu_M;}
matrix_t<double> *Heliostat::getSunShapeCoefObject(){return &_mu_S;}
matrix_t<double> *Heliostat::getErrorDistCoefObject(){return &_mu_G;}
matrix_t<double> *Heliostat::getFluxMomentsObject(){return &_mu_F;}
matrix_t<double> *Heliostat::getHermiteCoefObject(){return &_hcoef;}
matrix_t<double> *Heliostat::getHermiteNormCoefObject(){return &_hc_tht;}
double *Heliostat::getImageSize(){return _image_size_xy;}
void Heliostat::getImageSize(double &sigx_n, double &sigy_n){ sigx_n = _image_size_xy[0]; sigy_n = _image_size_xy[1];}
string *Heliostat::getHeliostatName(){return &_helio_name;}
Heliostat* Heliostat::getMasterTemplate(){return _master_template;}
var_heliostat* Heliostat::getVarMap(){return _var_helio;}

bool Heliostat::IsUserCant(){return _is_user_canted;} //Fetch
void Heliostat::IsUserCant(bool setting){_is_user_canted = setting;} //Set
bool Heliostat::IsEnabled(){return _is_enabled;}
bool Heliostat::IsInLayout(){return _in_layout;}
void Heliostat::IsEnabled(bool enable){_is_enabled = enable;}

void Heliostat::setId(int id){_id = id;}
void Heliostat::setGroupId(int row, int col){_group[0] = row; _group[1] = col;}
void Heliostat::setInLayout(bool in_layout){_in_layout = in_layout;}
void Heliostat::setNeighborList(vector<Heliostat*> *list){_neighbors = list;}
void Heliostat::setEfficiencyCosine(double eta_cos){eff_data.eta_cos = fmin(fmax(eta_cos,0.),1.);}
void Heliostat::setEfficiencyAtmAtten(double eta_att){eff_data.eta_att = eta_att;}
void Heliostat::setEfficiencyIntercept(double eta_int){eff_data.eta_int = eta_int;}
void Heliostat::setEfficiencyBlocking(double eta_block){eff_data.eta_block = eta_block;}
void Heliostat::setEfficiencyShading(double eta_shadow){eff_data.eta_shadow = eta_shadow;}
void Heliostat::setEfficiencyCloudiness(double eta_cloud){eff_data.eta_cloud = eta_cloud;}
void Heliostat::setEfficiencyTotal(double eta_tot){eff_data.eta_tot = eta_tot;}
void Heliostat::setRankingMetricValue(double rval){eff_data.rank_metric = rval;}
void Heliostat::setAimPointFluxPlane(sp_point &Aim){_aim_fluxplane.Set( Aim.x, Aim.y, Aim.z );}
void Heliostat::setAimPointFluxPlane(double x, double y, double z){ _aim_fluxplane.Set(x, y, z); }
void Heliostat::setTrackVector(Vect &tr){ _track = tr; }	//Set the tracking vector
void Heliostat::setTowerVector(Vect &tow){ _tower_vect = tow; } //Set the helio-tower vector
void Heliostat::setTrackAngleZenith(double zenith){_zenith = zenith;}
void Heliostat::setTrackAngleAzimuth(double azimuth){_azimuth = azimuth;}
void Heliostat::setTrackAngles(double azimuth, double zenith){_zenith = zenith; _azimuth = azimuth;}
void Heliostat::setCantVector(Vect &cant){_cant_vect.Set(cant.i, cant.j, cant.k);}
void Heliostat::setCantVector(double cant[3]){_cant_vect.Set(cant[0], cant[1], cant[2]);}
void Heliostat::setSlantRange(double L)
{
    _slant = L; 
    if(_var_helio->cant_method.mapval() == var_heliostat::CANT_METHOD::ONAXIS_AT_SLANT)
    {
        _xfocal = L; 
        _yfocal = L;
    }
}

void Heliostat::setFocalLengthX(double L){_xfocal = L;}
void Heliostat::setFocalLengthY(double L){_yfocal = L;}
void Heliostat::setFocalLength(double L){_yfocal = L; _xfocal = L;}
void Heliostat::setWhichReceiver(Receiver *rec){_which_rec = rec;}
void Heliostat::setPowerToReceiver(double P){eff_data.power_to_rec = P;}
void Heliostat::setPowerValue(double P){eff_data.power_value = P;}
void Heliostat::setImageSize(double sigx_n, double sigy_n){ _image_size_xy[0] = sigx_n; _image_size_xy[1] = sigy_n;}
void Heliostat::setMasterTemplate(Heliostat *htemp){_master_template = htemp;}

void Heliostat::resetMetrics(){
	eff_data.resetMetrics();
}

void Heliostat::Create(var_map &V, int htnum)
{
    _var_helio = &V.hels.at(htnum);
	//set some defaults for local values
    _helio_name = _var_helio->helio_name.val;
    _id = _var_helio->id.val;
    _is_enabled = _var_helio->is_enabled.val;
    _location.Set(0., 0., 0.);  //default
    _xfocal = _var_helio->x_focal_length.val;
    _yfocal = _var_helio->y_focal_length.val;
    _cant_vect.Set(0., 0., 1.);

    eff_data.reflectivity = _var_helio->reflectivity.val;
    eff_data.soiling = _var_helio->soiling.val;

	_track = Vect(); //The tracking vector for the heliostat, defaults to 0,0,1
	_tower_vect = Vect();  //Heliostat-to-tower unit vector
	
    //calculate dependent parameters
    updateCalculatedParameters(V, htnum);  //updates var_map
    
	//Now define panel geometry
	installPanels();

}

void Heliostat::updateCalculatedParameters(var_map &Vm, int htnum)
{
    double tht = Vm.sf.tht.val;

    var_heliostat* V = &Vm.hels.at(htnum);
    //Calculate the area and collision radius
	if(V->is_round.mapval() == var_heliostat::IS_ROUND::ROUND){
		_r_collision =  V->diameter.val/2. ;
		_area =  PI*pow(V->diameter.val/2.,2)*V->reflect_ratio.val ;
	}
	else{
        _r_collision =
            sqrt( V->height.val * V->height.val / 4. + V->width.val * V->width.val /4. );
		_area =
            V->width.val * V->height.val * V->reflect_ratio.val              //width * height * structural density is the base area
            - V->x_gap.val * V->height.val * (V->n_cant_x.val - 1) - V->y_gap.val * V->width.val * (V->n_cant_y.val - 1)     //subtract off gap areas
            + (V->n_cant_y.val - 1)*(V->n_cant_x.val - 1)* V->x_gap.val * V->y_gap.val 
            ;        //but don't double-count the little squares in both gaps
	}

    V->area.Setval( _area );
    V->r_collision.Setval( _r_collision );
    
    //calculate the total convolved optical error to report back
	double err_elevation, err_azimuth, err_surface_x, err_surface_y, err_reflect_x, err_reflect_y;
	err_elevation = V->err_elevation.val;
	err_azimuth = V->err_azimuth.val;
	err_surface_x = V->err_surface_x.val;
	err_surface_y = V->err_surface_y.val;
	err_reflect_x = V->err_reflect_x.val;
	err_reflect_y = V->err_reflect_y.val;
	double err_tot = sqrt( pow(2.*err_elevation, 2) + pow(2.*err_azimuth, 2) + pow(2.*err_surface_x, 2) + 
		pow(2.*err_surface_y, 2) + pow(err_reflect_x, 2) + pow(err_reflect_y, 2));
    
    V->err_total.Setval( err_tot );

    //Calculate the total optical reflectivity to report back
	double ref = V->reflectivity.val;
	double soil = V->soiling.val;
    V->ref_total.Setval( ref*soil );

    //Heliostat cant radius
	int cant_method = V->cant_method.mapval(); 
		/* 
		No canting=0
		On-axis at slant=-1
		On-axis, user-defined=1
		Off-axis, day and hour=3
		User-defined vector=4 
		*/
    switch (cant_method)
    {
    //case Heliostat::CANT_TYPE::FLAT:
    //case Heliostat::CANT_TYPE::AT_SLANT:
    case var_heliostat::CANT_METHOD::NO_CANTING:
    case var_heliostat::CANT_METHOD::ONAXIS_AT_SLANT:
        //nothing to calculate
        break;
    //case Heliostat::CANT_TYPE::ON_AXIS_USER:
    case var_heliostat::CANT_METHOD::ONAXIS_USERDEFINED:
    {
        //On-axis, user-defined
		double cant_radius; 
		double cant_rad_scaled = V->cant_rad_scaled.val;
			
		if(V->is_cant_rad_scaled.val){ cant_radius = cant_rad_scaled * tht; }
		else{ cant_radius = cant_rad_scaled; }

        //set the cant radius
		V->cant_radius.Setval( cant_radius );
        break;
    }
    //case Heliostat::CANT_TYPE::AT_DAY_HOUR:
    case var_heliostat::CANT_METHOD::OFFAXIS_DAY_AND_HOUR:
    {
        //Off-axis, day and hour
		/* Calculate the sun position at this day and hour */
		int cant_day =  V->cant_day.val;
		
        double cant_hour = V->cant_hour.val;
		double lat = Vm.amb.latitude.val; 
		double lon = Vm.amb.longitude.val; 
		double tmz = Vm.amb.time_zone.val;

		DateTime DT;
		int month, dom;
		DT.hours_to_date((cant_day-1)*24+cant_hour+12, month, dom);

		//Instantiate the solpos object
		struct posdata SP, *pdat;
		pdat = &SP;	//point to structure for convenience
		S_init(pdat);		//Initialize the values
	
		//get hours and minutes from the cant hour
		int cant_hour_int, cant_min_int;
		cant_hour_int = (int)floor(cant_hour + .001);
		cant_min_int = (int)(floor( (cant_hour - (double)cant_hour_int)*60. ) );
			
		pdat->latitude = float(lat);		//[deg] {float} North is positive
		pdat->longitude = float(lon);		//[deg] {float} Degrees east. West is negative
		pdat->timezone = float(tmz);			//[hr] {float} Time zone, east pos. west negative. Mountain -7, Central -6, etc..
		pdat->year = 2011;		//[year] {int} 4-digit year
		pdat->month = month;	//[mo] {int} (1-12)
		pdat->day = dom;		//[day] {int} Day of the month
		pdat->daynum = cant_day;	//[day] {int} Day of the year
		pdat->hour = cant_hour_int+12;		//[hr] {int} 0-23
		pdat->minute = cant_min_int;	//[min] {int} 0-59
		pdat->second = 0;	//[sec]	{int} 0-59
		pdat->interval = 0;		//[sec] {int} Measurement interval. See solpos documentation.


		long retcode = 0;		//Initialize with no errors
		retcode = S_solpos(pdat);	//Call the solar posotion algorithm
		S_decode(retcode, pdat);	//Check the return code

		/* Check to see if the time/day entered is below sunset. If so, notify the user */
		DT.SetHour(12);
		DT.SetDate(2011,month,dom);
		DT.SetYearDay(cant_day+1);
		double hrs[2];
		//Calculate the daytime hours
		Ambient::calcDaytimeHours(hrs, lat*D2R, lon*D2R, tmz, DT);
		hrs[0] += -12.;
		hrs[1] += -12.;

        //Set the cant sun position
        V->cant_sun_el.Setval( 90. - SP.zenetr );
        V->cant_sun_az.Setval( SP.azim );

        break;
    }
    //case Heliostat::CANT_TYPE::USER_VECTOR:
    case var_heliostat::CANT_METHOD::USERDEFINED_VECTOR:
    {
		//Calculate the magnitude of the vector components
        double i = V->cant_vect_i.val * V->cant_vect_i.val;
        double j = V->cant_vect_j.val * V->cant_vect_j.val;
        double k = V->cant_vect_k.val * V->cant_vect_k.val;
        double cmag = sqrt(i*i + j*j + k*k);
		

        //set normal and cant vector values
        V->cant_norm_i.Setval( i/cmag );
        V->cant_norm_j.Setval( j/cmag );
        V->cant_norm_k.Setval( k/cmag );
		
        double scale = V->cant_vect_scale.val; 
		V->cant_mag_i.Setval( i/cmag*scale );
		V->cant_mag_j.Setval( j/cmag*scale );
		V->cant_mag_k.Setval( k/cmag*scale );
        break;
    }
    default:
        break;
    }



}

void Heliostat::getSummaryResults( vector<double> &results){
	/* 
	Fill the vector "results" with performance metrics of interest
	*/
	results.resize( eff_data.n_metric );
	for(int i=0; i<eff_data.n_metric; i++) results.at(i) = eff_data.getDataByIndex(i); 
	return;
}

void Heliostat::setAimPoint(double x, double y, double z){
	//Set the heliostat aim point
	_aim_point.x = x;
	_aim_point.y = y;
	_aim_point.z = z;
}

void Heliostat::setAimPoint(sp_point &Aim){
	setAimPoint(Aim.x, Aim.y, Aim.z);
}

void Heliostat::installPanels() {
	/*
	This method uses the inputs to define the location and pointing vector of each
	panel on the heliostat. 

	DELSOL3 lines 6494-6520

	Note that in DELSOL3, this originally is part of the flux algorithm. The panel
	arrangement is more conveniently conceptualized as an attribute of the heliostat
	rather than as part of the flux algorithm, so it is placed here instead.
	*/
    var_heliostat *V = _var_helio;

 //   //Calculate the collision radius
	//if(V->is_round.val){
	//	_r_collision =  V->diameter.val/2. ;
	//	_area =  PI*pow(V->diameter.val/2.,2)*V->reflect_ratio.val ;
	//}
	//else{
 //       _r_collision =
 //           sqrt( V->height.val * V->height.val / 4. + V->width.val * V->width.val /4. );
	//	_area =
 //           V->width.val * V->height.val * V->reflect_ratio.val              //width * height * structural density is the base area
 //           - V->x_gap.val * V->height.val * (V->n_cant_x.val - 1) - V->y_gap.val * V->width.val * (V->n_cant_y.val - 1)     //subtract off gap areas
 //           + (V->n_cant_y.val - 1)*(V->n_cant_x.val - 1)* V->x_gap.val * V->y_gap.val 
 //           ;        //but don't double-count the little squares in both gaps
	//}

	//Initialize the image plane image size for this heliostat to zero until it's calculated in the Flux methods
	setImageSize(0.,0.);

	if(V->is_round.mapval() == var_heliostat::IS_ROUND::ROUND){

		/* 
		This configuration allows only 1 facet per heliostat. By default, the canting is normal.
		*/
        _panels.resize(1,1);

		_panels.at(0,0).setId(0);
		_panels.at(0,0).setType(2);	//Circular
		_panels.at(0,0).setDiameter(V->diameter.val);
		_panels.at(0,0).setHeight(V->diameter.val);
		_panels.at(0,0).setWidth(V->diameter.val);
		_panels.at(0,0).setPosition(0.,0.,0.);
		_panels.at(0,0).setAim(0.,0.,1.);

	}
	else{	//Rectangular heliostats

		//Variable declarations
		double dx, dy, x, y;
	
		//--------Calculate canting from other inputs-----------
		//Calculate height and width of each facet
		dx = (V->width.val - V->x_gap.val * (V->n_cant_x.val - 1.) ) / (double)V->n_cant_x.val;	//[m] width of each canting panel
		dy = (V->height.val - V->y_gap.val * (V->n_cant_y.val - 1.) ) / (double)V->n_cant_y.val;	//[m] height of each panel

		int id=0;
		_panels.resize(V->n_cant_y.val, V->n_cant_x.val);
        
		//back-calculate the aim point
        sp_point paim;     //heliostat aimpoint
		paim.x = _location.x + _slant*_tower_vect.i;
		paim.y = _location.y + _slant*_tower_vect.j;
		paim.z = _location.z + _slant*_tower_vect.k;

        //initialize X and Y location of the facet
		y = -V->height.val/2. + dy*0.5;

		for (int j=0; j<V->n_cant_y.val; j++) {
            //initialize the X location of the facet
		    x = -V->width.val/2. + dx*0.5;

			for (int i=0; i<V->n_cant_x.val; i++) {
				//Assign an ID
				_panels.at(j,i).setId(id); id++;
				_panels.at(j,i).setType(1);	//Type=1, rectangular panel
				_panels.at(j,i).setWidth(dx);
				_panels.at(j,i).setHeight(dy);
				//Set the position in the reflector plane. Assume the centroid is in the plane (z=0)
				_panels.at(j,i).setPosition(x, y, 0.0);
				//Determine how each panel is canted
				switch(V->cant_method.mapval())
				{
                //case CANT_TYPE::AT_SLANT:	//Individual on-axis cant at distance equal to the slant range
                case var_heliostat::CANT_METHOD::ONAXIS_AT_SLANT:
                {
					double hyp = sqrt( pow(_slant,2) + pow(x, 2) + pow(y, 2) );	//hypotenuse length
					_panels.at(j,i).setAim(-x/hyp, -y/hyp, 2.*_slant/hyp);
					break;
                }
                //case CANT_TYPE::FLAT:		//no canting
                case var_heliostat::CANT_METHOD::NO_CANTING:
					_panels.at(j,i).setAim(0.,0.,1.);
					break;
                //case CANT_TYPE::ON_AXIS_USER:		//User-defined on-axis canting. Canting specified in array.
                case var_heliostat::CANT_METHOD::ONAXIS_USERDEFINED:
                {
					double hyp = sqrt( V->cant_radius.Val()*V->cant_radius.Val() + x*x + y*y );	//cant focal length
					_panels.at(j,i).setAim(-x/hyp, -y/hyp, 2.*V->cant_radius.Val()/hyp);
					break;
                }
                //case CANT_TYPE::AT_DAY_HOUR:		//Individual off-axis cant at time defined by tracking vector
                case var_heliostat::CANT_METHOD::OFFAXIS_DAY_AND_HOUR:
                {
					//Calculate the tracking azimuth/zenith based on the tracking vector
					double track_az = atan2(_track.i,_track.j);
					double track_zen = acos(_track.k);
				
					//Calculate the panel's actual x-y-z location w/r/t the global coordinates
					double prad = sqrt(pow(x,2)+pow(y*sin(track_zen),2));	//the radius of the panel from the heliostat centroid
					double theta_rot = atan2(x,y);	//angle of rotation of the centroid of the point w/r/t the heliostat coordinates
                    sp_point pg;
					pg.x = _location.x + prad*sin(track_az+theta_rot);
					pg.y = _location.y + prad*cos(track_az+theta_rot);
					pg.z = _location.z + y*sin(track_zen);

					//determine the vector from the panel centroid to the aim point
					double pslant = sqrt( pow(pg.x - paim.x, 2) + pow(pg.y - paim.y, 2) + pow(pg.z - paim.z, 2));
					Vect pref;
                    pref.i = (paim.x - pg.x)/pslant;
					pref.j = (paim.y - pg.y)/pslant;
					pref.k = (paim.z - pg.z)/pslant;

					//back calculate the sun vector from the heliostat normal and receiver vectors
					Vect s_hat;
                    s_hat.i = 2.*_track.i - pref.i;
					s_hat.j = 2.*_track.j - pref.j;
					s_hat.k = 2.*_track.k - pref.k;

					//The canting correction vector is the average of the sun vector and the reflection vector subtracting 
					//the total heliostat tracking vector
					_panels.at(j,i).setAim( (s_hat.i + pref.i)/2. - _track.i, (s_hat.j + pref.j)/2. - _track.j, (s_hat.k + pref.k)/2. - _track.k + 1.);
				
					break;
                }
                //case CANT_TYPE::USER_VECTOR:
                case var_heliostat::CANT_METHOD::USERDEFINED_VECTOR:
                {
                    //throw spexception("The user cant vector option is not correctly implemented in the installPanels() algorithm. Contact support for help resolving this issue.");

                    //The canting correction vector ensures that the user-specified vector is the focus of the canting operation. 
                    Vect paim;
                    double rscale = V->is_cant_rad_scaled.val ? V->cant_vect_scale.val : 1.;

                    /* 
                    coordinate system: looking at the heliostat face on, +x is horizontal, +y vertical. Vector is assumed to come out of the 
                    plane containing the heliostat in the direction of the viewer (i.e. it hits you in the face). This is -z. The if the 
                    vector tilts to the right as viewed, this is +i / +x, up is +j / +y.
                    */
                    paim.Set( _cant_vect.i * rscale - x, _cant_vect.j * rscale - y, _cant_vect.k * rscale );
                    //normalize
                    Toolbox::unitvect( paim );
                    _panels.at(j,i).setAim( paim );

                    break;
                }
				default:
                    throw spexception("The requested canting option is not correctly implemented in the installPanels() algorithm. Contact support for help resolving this issue.");
				}

                //increment the x panel position
                x += dx + V->x_gap.val;
			}
            //increment the y panel position
            y += dy + V->y_gap.val;
		}
	}
	

}

void Heliostat::updateTrackVector(Vect &sunvect) {
	/*
	Calculates the tracking vector given a solar position in "Ambient"
	and a receiver in "Receiver".

	Updates the coordinates of the heliostat corners for shadowing/blocking calculations.

	Do not update the aim point. This method uses the currently assigned aim point.

	This also updates:
	_track			| setTrackVector()		| The tracking vector for the heliostat
	_azimuth		| setTrackAngles()		| The tracking azimuth angle
	_zenith			| setTrackAngles()		| The tracking zenith angle
	_corners		| none					| The location of the heliostat corners in global coordinates (for shadowing and blocking)

	Store the new tracking vector in _track

	From Snell's law, n_hat = (s_hat + t_hat) / mag(s_hat + t_hat)
		where:
		n_hat is the normal tracking vector
		s_hat is the heliostat to sun vector
		t_hat is the helostat to receiver vector
	*/

	Vect 
		n_hat, //tracking vector
		s_hat, //heliostat to sun
		t_hat; //heliostat to receiver
		
	//Get the solar position vector from the ambient class
	s_hat = sunvect;

	//Create a vector between the heliostat and the aim point
    if( _is_enabled )
    {
        t_hat.Set(_aim_point.x - _location.x, _aim_point.y - _location.y, _aim_point.z - _location.z);
	    
        Toolbox::unitvect(t_hat);
        
        //Use the approximate tower vector t_hat to determine the tracking vector
	    Vect ts;
		
	    ts.i = t_hat.i + s_hat.i;
	    ts.j = t_hat.j + s_hat.j;
	    ts.k = t_hat.k + s_hat.k; //break down to save on calculation
	    double ts_mag = sqrt( pow(ts.i, 2) + pow(ts.j, 2) + pow(ts.k, 2) );
	    n_hat.i = ts.i/ts_mag;
	    n_hat.j = ts.j/ts_mag;
	    n_hat.k = ts.k/ts_mag;    

	    //Set the tracking angles
	    setTrackAngles(atan2(n_hat.i,n_hat.j), acos(n_hat.k));
    }
    else
    {
        //aimpoint vector is reflection of sun vector
        t_hat.Set( -sunvect.i, -sunvect.j, sunvect.k );
                
        //normal vector is zenith
        n_hat.Set(0., 0., 1.);

	    //make tracking angles so that heliostat "faces" tower position when in stow
	    setTrackAngles(atan2(_location.x,_location.y), 0.);
    }
			
	
	//Set the Heliostat object tracking vector
	setTrackVector(n_hat);
	//Set the heliostat to tower vector
	setTowerVector(t_hat);

	
	/*Calculate the location in global coordinates of the top two heliostat corners. Note that 
	by the azimuth convention where North is 0deg, the upper edges of the heliostat will begin on
	the southernmost edge of the heliostat.
		
	Assume that the heliostat is starting out facing upward in the z direction with the 
	upper and lower edges parallel to the global x axis (i.e. zenth=0, azimuth=0)
	*/
	if(! ( _var_helio->is_round.mapval() == var_heliostat::IS_ROUND::ROUND)){
        double wm2 = _var_helio->width.val/2.;
        double hm2 = _var_helio->height.val/2.;
		_corners.resize(4);
	
		_corners.at(0).Set(-wm2, -hm2, 0.);	//Upper right corner
		_corners.at(1).Set(wm2, -hm2, 0.);	//upper left corner
		_corners.at(2).Set(wm2, hm2, 0.);	//lower left
		_corners.at(3).Set(-wm2, hm2, 0.);	//lower right
		
		for(int i=0; i<4; i++){ //For each point of interest...
			//Rotate first about the x axis (zenith)
			Toolbox::rotation(_zenith, 0, _corners.at(i)); 
			//Now rotate about the z-axis (azimuth)
			Toolbox::rotation(_azimuth, 2, _corners.at(i));
			//Move from heliostat coordinates to global coordinates
			_corners.at(i).Add(_location.x, _location.y, _location.z); 
		}
	}
	else{ 
		//no corner geometry to consider for round heliostats
	}
	
	return;

}

void Heliostat::calcAndSetAimPointFluxPlane(sp_point &aimpos_abs, Receiver &Rec, Heliostat &H)
{
    /* 
    Given a particular aim point in space, translate the position to an aimpoint on the actual
    flux plane of the receiver. The original aimpoint may not necessarily be on the plane of the 
    receiver, but the final aim point will be.
    */    
    
    sp_point aimpos(aimpos_abs);    
    PointVect NV;
	Rec.CalculateNormalVector(*H.getLocation(), NV);	//Get the receiver normal vector

    double az = atan2(NV.i, NV.j);
    double el = atan2(NV.k*NV.k, NV.i*NV.i + NV.j*NV.j); 

    //rotate into flux plane coordinates
    Toolbox::rotation(PI - az,2,aimpos);
	Toolbox::rotation(PI/2. - el,0,aimpos);
	//The z component should be very small, so zero out
	if( fabs(aimpos.z) < 1.e-6 ) aimpos.z = 0.;
	//The X and Y coordinates now indicate the image plane position
	H.setAimPointFluxPlane(aimpos.x, aimpos.y, aimpos.z);
}

void Heliostat::setLocation(double x, double y, double z)
{
	//Set the location 
	_location.x = x;
	_location.y = y;
	_location.z = z;
}

//--------------Reflector class methods ----------------------

Reflector::Reflector(){
	setDefaults();
}

//Get-Set methods
int Reflector::getId(){return _id;}
double Reflector::getWidth(){return _width;}
double Reflector::getHeight(){return _height;}
double Reflector::getDiameter(){return _diameter;}
double Reflector::getFocalLength(){return _focal_length;}
int Reflector::getType(){return _type;}
PointVect *Reflector::getOrientation(){return &_locate_vector;}
	

void Reflector::setId(int id){_id = id;}
void Reflector::setType(int type){_type = type;}
void Reflector::setWidth(double width){_width = width;}
void Reflector::setHeight(double height){_height = height;}
void Reflector::setDiameter(double diam) {_diameter = diam;}

//Set the default values for the reflector class
void Reflector::setDefaults() {
	//Set default values
	_width = 0.;
	_height = 0.;
	_diameter = 0.;
	_focal_length = 0.;
	_id = -1;
	_type = 1;
	setOrientation(0., 0., 0., 0., 0., 0.);
}

void Reflector::setPosition(double x, double y, double z) {
	_locate_vector.x = x;
	_locate_vector.y = y;
	_locate_vector.z = z;
}

void Reflector::setAim(double i, double j, double k){
	_locate_vector.i = i;
	_locate_vector.j = j;
	_locate_vector.k = k;
}

void Reflector::setAim( Vect &V ){
	_locate_vector.i = V.i;
	_locate_vector.j = V.j;
	_locate_vector.k = V.k;
}

void Reflector::setOrientation(double x, double y, double z, double i, double j, double k){
	setPosition(x,y,z);
	setAim(i,j,k);
}

void Reflector::setOrientation(PointVect &PV){
	setPosition(PV.x, PV.y, PV.z);
	setAim(PV.i, PV.j, PV.k);
}

Reflector *Heliostat::getPanelById(int id){
    size_t ncantx, ncanty;
    _panels.size(ncantx, ncanty);  //is this the right order?

	for (int j=0; j<(int)ncantx; j++) {
		for (int i=0; i<(int)ncanty; i++) {
		  	if (_panels.at(j,i).getId() == id) {
			  	return &_panels.at(j,i);
			}
		}
	}

	//#####call an error here
	return &_panels.at(0, 0);
}

Reflector *Heliostat::getPanel(int row, int col){
	int nr, nc;
	nr = (int)_panels.nrows();
	nc = (int)_panels.ncols();
	if(row < nr && col < nc) {
	  	return &_panels.at(row, col);
	}
	else{
		//FLAG -- this should be an error
		throw spexception("Index out of range in Heliostat::getPanel()");
	}
}

void Heliostat::CopyImageData(const Heliostat *Hsrc){
	/* 
	Copy the image coefficients and data from 'Hsrc' and set as the local heliostat image coefs.

	The following arrays are copied:
	matrix_t<double>
		_mu_MN,		//Normalized mirror shape hermite expansion coefficients (n_terms x n_terms)
		_mu_S,		//Moments of sunshape
		_mu_G,		//Moments of the error distribution
		_mu_M,		//Moments of the mirror shape
		_mu_F,		//Flux moments distrubution - result
		_hcoef,		//Hermite coefficients
		_hc_tht; 		//Hermite coefs depending on tower height - equiv. to mu_F, reused in optimization calcs
	*/
	int nr, nc;

	//_mu_MN
	nr = (int)Hsrc->_mu_MN.nrows();
	nc = (int)Hsrc->_mu_MN.ncols();
	_mu_MN.resize(nr,nc);
	for(int i=0; i<nr; i++)
		for(int j=0; j<nc; j++)
			_mu_MN.at(i,j) = Hsrc->_mu_MN.at(i,j);
	//_mu_S
	nr = (int)Hsrc->_mu_S.nrows();
	nc = (int)Hsrc->_mu_S.ncols();
	_mu_S.resize(nr,nc);
	for(int i=0; i<nr; i++)
		for(int j=0; j<nc; j++)
			_mu_S.at(i,j) = Hsrc->_mu_S.at(i,j);
	//_mu_G
	nr = (int)Hsrc->_mu_G.nrows();
	nc = (int)Hsrc->_mu_G.ncols();
	_mu_G.resize(nr,nc);
	for(int i=0; i<nr; i++)
		for(int j=0; j<nc; j++)
			_mu_G.at(i,j) = Hsrc->_mu_G.at(i,j);
	//_mu_M
	nr = (int)Hsrc->_mu_M.nrows();
	nc = (int)Hsrc->_mu_M.ncols();
	_mu_M.resize(nr,nc);
	for(int i=0; i<nr; i++)
		for(int j=0; j<nc; j++)
			_mu_M.at(i,j) = Hsrc->_mu_M.at(i,j);
	//_mu_F
	nr = (int)Hsrc->_mu_F.nrows();
	nc = (int)Hsrc->_mu_F.ncols();
	_mu_F.resize(nr,nc);
	for(int i=0; i<nr; i++)
		for(int j=0; j<nc; j++)
			_mu_F.at(i,j) = Hsrc->_mu_F.at(i,j);
	//_hcoef
	nc = (int)Hsrc->_hcoef.ncells();
	_hcoef.resize(nc);
	for(int j=0; j<nc; j++)
		_hcoef.at(j) = Hsrc->_hcoef.at(j);
	//_hc_tht
	nr = (int)Hsrc->_hc_tht.nrows();
	nc = (int)Hsrc->_hc_tht.ncols();
	_hc_tht.resize(nr,nc);
	for(int i=0; i<nr; i++)
		for(int j=0; j<nc; j++)
			_hc_tht.at(i,j) = Hsrc->_hc_tht.at(i,j);
}
