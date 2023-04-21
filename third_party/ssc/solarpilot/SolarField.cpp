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

#include <assert.h>
#include <algorithm>
#include <math.h>

#include "exceptions.hpp"
#include "SolarField.h"

#include "sort_method.h"
#include "Heliostat.h"
#include "Receiver.h"
#include "Financial.h"
#include "Ambient.h"
#include "Land.h"
#include "Flux.h"
#include "heliodata.h"

#include "OpticalMesh.h"

using namespace std;

//Sim params
sim_params::sim_params()
{
    dni = 0.; //W/m2
    Tamb = 0.;    //C
    Patm = 0.;    //atm
    Vwind = 0.;   //m/s
    TOUweight = 1.; //-
    Simweight = 1.;
    is_layout = false;
}

//-------Access functions
//"GETS"
SolarField::clouds *SolarField::getCloudObject(){return &_clouds;}
vector<Receiver*> *SolarField::getReceivers(){return &_active_receivers;}
Land *SolarField::getLandObject(){return &_land;}
//Ambient *SolarField::getAmbientObject(){return &_ambient;}
Flux *SolarField::getFluxObject(){return _flux;}
Financial *SolarField::getFinancialObject(){return &_financial;}
FluxSimData *SolarField::getFluxSimObject(){return &_fluxsim;}
htemp_map *SolarField::getHeliostatTemplates(){return &_helio_templates;}
Hvector *SolarField::getHeliostats(){return &_heliostats;}
layout_shell *SolarField::getLayoutShellObject(){return &_layout;}
unordered_map<int,Heliostat*> *SolarField::getHeliostatsByID(){return &_helio_by_id;}
vector<Heliostat> *SolarField::getHeliostatObjects(){return &_helio_objects;}
var_map *SolarField::getVarMap(){return _var_map;}

bool SolarField::getAimpointStatus(){return _is_aimpoints_updated;}
double SolarField::getSimulatedPowerToReceiver(){return _sim_p_to_rec;}
double *SolarField::getHeliostatExtents(){return _helio_extents;}
	
simulation_info *SolarField::getSimInfoObject(){return &_sim_info;}
simulation_error *SolarField::getSimErrorObject(){return &_sim_error;}
optical_hash_tree *SolarField::getOpticalHashTree(){return &_optical_mesh;}

//-------"SETS"
/*min/max field radius.. function sets the value in units of [m]. Can be used as follows:
1) SetMinFieldRadius(_tht, 0.75); 
2) SetMinFieldRadius(100, 1.0); 
*/
void SolarField::setAimpointStatus(bool state){_is_aimpoints_updated = state;}
void SolarField::setSimulatedPowerToReceiver(double val){_sim_p_to_rec = val;}
void SolarField::setHeliostatExtents(double xmax, double xmin, double ymax, double ymin){ 
	_helio_extents[0] = xmax;
	_helio_extents[1] = xmin;
	_helio_extents[2] = ymax;
	_helio_extents[3] = ymin;
};
	
//Scripts
bool SolarField::ErrCheck(){return _sim_error.checkForErrors();}
void SolarField::CancelSimulation(){
	_cancel_flag = true; 
	_sim_error.addSimulationError("Simulation cancelled by user",true,false);
}
bool SolarField::CheckCancelStatus(){ 
	bool stat = _cancel_flag; 
	//_cancel_flag = false; 
	return stat;} 	//any time this is called, reset the flag since the cancellation will be handled immediately
	

//Constructor / destructor
SolarField::SolarField(){
	//_flux = new Flux();
	_flux = 0;
    _var_map = 0;
	_is_created = false;	//The Create() method hasn't been called yet.
	_estimated_annual_power = 0.;
};		

SolarField::~SolarField(){ 
	if(_flux != (Flux*)NULL) delete _flux; //If the flux object is allocated memory, delete
	//Delete receivers
	for(unsigned int i=0; i<_receivers.size(); i++){
		delete _receivers.at(i);
	}
}

//Copy constructor
SolarField::SolarField( const SolarField &sf )
	: 
	_q_to_rec( sf._q_to_rec ),		//[MW] Power to the receiver during performance runs
	_sim_p_to_rec( sf._sim_p_to_rec ),
	_estimated_annual_power( sf._estimated_annual_power ),
	_q_des_withloss( sf._q_des_withloss ),
	_is_aimpoints_updated( sf._is_aimpoints_updated ),
	_cancel_flag( sf._cancel_flag ),
	_is_created( sf._is_created ),
	_layout( sf._layout ),
	_helio_objects( sf._helio_objects ),	//This contains the heliostat objects. The heliostat constructor will handle all internal pointer copy operations
	_helio_template_objects( sf._helio_template_objects ),	//This contains the heliostat template objects.
	_land( sf._land ),
	_financial( sf._financial ),
	_fluxsim( sf._fluxsim ),
	_sim_info( sf._sim_info ),
	_sim_error( sf._sim_error ),
	_var_map( sf._var_map )    //point to original variable map
{
	//------- Reconstruct pointer maps, etc ----------
	for(int i=0; i<4; i++){
		_helio_extents[i] = sf._helio_extents[i]; 
	}
	//Create a map between the original heliostat object addresses and the new address
	unordered_map<Heliostat*, Heliostat*> hp_map;
	for(unsigned int i=0; i<_helio_objects.size(); i++){
		hp_map[ const_cast<Heliostat*>( &sf._helio_objects.at(i) ) ] = &_helio_objects.at(i);
	}

	//Create a temporary pointer map between the old and new heliostat template objects
	unordered_map<Heliostat*,Heliostat*> htemp_ptr_map;
	for(int i=0; i<(int)_helio_template_objects.size(); i++){	
		htemp_ptr_map[ const_cast<Heliostat*>( &(sf._helio_template_objects.at(i)) ) ] = &(_helio_template_objects.at(i));
	}

	//Reassign the integer->Heliostat* map
	
	_helio_templates.clear();
	for(unsigned int i=0; i<_helio_template_objects.size(); i++){
		_helio_templates[i] = &_helio_template_objects.at(i);
	}
	//Set up the heliostat pointer array
	/* 
	The ID's are originally set according to the position in the heliostat object matrix.
	Use this feature to reconstruct the pointer map.
	*/
	int npos = (int)sf._heliostats.size();
	_heliostats.resize(npos);
	for(int i=0; i<npos; i++){
		_heliostats.at(i) = hp_map[ sf._heliostats.at(i) ];
		_heliostats.at(i)->setMasterTemplate( htemp_ptr_map[ _heliostats.at(i)->getMasterTemplate()] );
	}
	_helio_by_id.clear();
	for(unsigned int i=0; i<sf._helio_by_id.size(); i++){	//Do a separate loop for ID's since they are not necessarily assigned right away
		int id = sf._heliostats.at(i)->getId();
		_helio_by_id[ id ] = hp_map[ const_cast<Heliostat*>( const_cast<SolarField*>(&sf)->_helio_by_id[id] ) ];
	}

	//Heliostat groups
	int nr, nc;
	nr = (int)sf._helio_groups.nrows();
	nc = (int)sf._helio_groups.ncols();
	_helio_groups.resize_fill(nr, nc, Hvector()); //NULL));
	for(int i=0; i<nr; i++){
		for(int j=0; j<nc; j++){
			int nh = (int)sf._helio_groups.at(i,j).size();
			_helio_groups.at(i,j).resize(nh);
			for(int k=0; k<nh; k++){
				_helio_groups.at(i,j).at(k) = hp_map[ sf._helio_groups.at(i,j).at(k) ];
			}
		}
	}

	//Layout groups
	_layout_groups.resize(sf._layout_groups.size());
	for(int j=0; j<(int)sf._layout_groups.size(); j++){
		int nh = (int)sf._layout_groups.at(j).size();
		_layout_groups.at(j).resize(nh);
		for(int k=0; k<nh; k++){
			_layout_groups.at(j).at(k) = hp_map[ sf._layout_groups.at(j).at(k) ];
		}
	}
	

	//Neighbors
	nr = (int)sf._neighbors.nrows();
	nc = (int)sf._neighbors.ncols();
	_neighbors.resize_fill(nr, nc, Hvector()); //0, NULL));
	for(int i=0; i<nr; i++){
		for(int j=0; j<nc; j++){
			int nh = (int)sf._neighbors.at(i,j).size();
			_neighbors.at(i,j).resize(nh);
			for(int k=0; k<nh; k++){
				_neighbors.at(i,j).at(k) = hp_map[ sf._neighbors.at(i,j).at(k)];
			}
		}
	}
	for(int i=0; i<npos; i++){
		//Each heliostat needs to have the neighbor group assigned to it
		Heliostat *hptr = _heliostats.at(i);
		hptr->setNeighborList( &_neighbors.at( hptr->getGroupId()[0], hptr->getGroupId()[1] ) );
	}

	//Create receivers
	unordered_map<Receiver*, Receiver*> r_map;	//map old receiver -> new receiver
	int nrec = (int)sf._receivers.size();
	_active_receivers.clear();
	for(int i=0; i<nrec; i++){
		Receiver *rec = new Receiver( *sf._receivers.at(i) );
		_receivers.push_back( rec );
		r_map[ sf._receivers.at(i) ] = rec;		//map
		//When copying the receiver class, update the flux surfaces to point to the correct receiver
		FluxSurfaces *fs = rec->getFluxSurfaces();
		for(unsigned int j=0; j<fs->size(); j++){
			fs->at(j).setParent( rec );
		}
		//Recreate the active receivers list
		if(rec->getVarMap()->is_enabled.val)
			_active_receivers.push_back( rec );
	}


	//Assign the correct receiver to each heliostat
	for(int i=0; i<npos; i++){
		_heliostats.at(i)->setWhichReceiver( r_map[ _heliostats.at(i)->getWhichReceiver() ] );
	}

	//Flux
	//if(_flux != (Flux*)NULL ) delete _flux;
	_flux = new Flux( *sf._flux );

}


//Scripts
void SolarField::Create(var_map &V){
	/*
	Create a solar field instance, set the variable values according to the map provided by the GUI.
	This simply instantiates all the needed objects for the solar field but does not do any analysis.
	*/
	_sim_info.addSimulationNotice("Creating solar field geometry");

    _var_map = &V;  //point to te variables used to create this variable
    //_layout_data = V.sf.layout_data.val;  //copy string layout data



	//Clean out possible existing variables
	Clean();
	
    //Go through and set all of the relevant variables, constructing objects as needed along the way.

    //Ambient
	//_ambient.Create(V);
 
	//Create the flux object
	if(_flux != 0 ){ delete _flux; }	//If the flux object already exists, delete and start over
	_flux = new Flux();
	_flux->Setup();
	
	setAimpointStatus(false);	//the aimpoints have not yet been calculated
	
	//Heliostat variables, Instantiate the template objects in order
    int nh = (int)V.hels.size();
	_helio_template_objects.resize(nh);
    V.sf.temp_which.combo_clear();
    for(int j=0; j<nh; j++)
    {
		_helio_template_objects.at(j).Create( V, j );
		_helio_templates[ j ] = &_helio_template_objects.at(j);
	std::string js = my_to_string(j);	
        V.sf.temp_which.combo_add_choice( V.hels.at(j).helio_name.val, js );
	}
	
	//Land
	_land.Create(V);

	//Parse the layout string into a layout object
	if(! V.sf.layout_data.val.empty() )
    {
        //V.sf.layout_method.combo_select_by_mapval( var_solarfield::LAYOUT_METHOD::USERDEFINED );
		//Convert the string contents to a layout_shell object
		SolarField::parseHeliostatXYZFile( V.sf.layout_data.val, _layout );
        vector<sp_point> lpt;
        for(int i=0; i<(int)_layout.size(); i++)
            lpt.push_back( _layout.at(i).location );
        _land.calcLandArea(V.land, lpt);    //update the land bound area value
        //update the solar field area calculation
        _sf_area = calcHeliostatArea();
	}

	//Receiver variables
	int Nset = (int)V.recs.size();
	_active_receivers.clear();
	
    for(int i=0; i<Nset; i++)
    {
		Receiver *rec = new Receiver();
		_receivers.push_back( rec );
		_receivers.at(i)->Create( V.recs.at(i), V.sf.tht.val );
		if( V.recs.at(i).is_enabled.val )
			_active_receivers.push_back(rec);
	}
	
   
	//Clouds
	double ext[2];
	_land.getExtents(V, ext);
	_clouds.Create(V, ext);

    //Fluxsim
    _fluxsim.Create(V);

    //local solar field parameters
    updateCalculatedParameters(V);
    
    //Cost - financial
	_financial.Create(V);


	_is_created = true;
	
}

void SolarField::updateCalculatedParameters( var_map &V )
{
    /* 
    Update any local values that are reported in the var_map

    This method is not threadsafe unless var_map is threadsafe!
    */

    //sun position at design
    double azzen[2];
    CalcDesignPtSunPosition(V.sf.sun_loc_des.mapval(), azzen[0], azzen[1]);
    V.sf.sun_az_des.Setval( azzen[0] );
    V.sf.sun_el_des.Setval( 90.-azzen[1] );


    //receiver area
    double arec = 0.;
    for(int i=0; i<(int)V.recs.size(); i++)
        arec += V.recs.at(0).absorber_area.Val();

    V.sf.rec_area.Setval( arec );

    //heliostat area
    V.sf.sf_area.Setval( _sf_area );

    //average attenuation
    if(_heliostats.size() > 0 )
    {
        double atten_ave = 0.;
        //calculate for each heliostat
        for(int i=0; i<(int)_heliostats.size(); i++)
        {
            double slant = _heliostats.at(i)->getSlantRange();
            atten_ave += Ambient::calcAttenuation(V,  slant);
        }
        V.amb.atm_atten_est.Setval( 100.*(1. - atten_ave / (double)_heliostats.size() ) );
    }
    else
    {
        //calculate for land area
        //make sure land calculations have been updated already
        double radave = (V.land.radmin_m.Val() + V.land.radmax_m.Val())/2.;
        V.amb.atm_atten_est.Setval( 100.*(1. - Ambient::calcAttenuation(V, radave) ) );
    }



}

void SolarField::updateAllCalculatedParameters(var_map &V)
{
    /* 
    Update all of the calculated values in all of the child classes and in solarfield
    */

    for( int i=0; i<(int)_helio_template_objects.size(); i++)
        _helio_template_objects.at(i).updateCalculatedParameters(V, i);

    _land.updateCalculatedParameters(V);

    for( int i=0; i<(int)_receivers.size(); i++)
        _receivers.at(i)->updateCalculatedParameters(V.recs.at(i), V.sf.tht.val );

    _fluxsim.updateCalculatedParameters(V);

    updateCalculatedParameters(V);

    _financial.updateCalculatedParameters(V);

    //optimization settings
	V.opt.aspect_display.Setval( V.recs[0].rec_aspect.Val() ); 
    V.opt.gs_refine_ratio.Setval( pow(1./1.61803398875, V.opt.max_gs_iter.val ) ); 

}

void SolarField::Clean(){
	/* 
	Clean out existing solar field and subcomponent arrays for a new start. These are the 
	variables that aren't set in the "Create()" methods.
	*/

	for(int i=0; i<4; i++) _helio_extents[i] = 0.;
	_layout.clear();
	_helio_objects.clear();
	_helio_templates.clear();
    _helio_template_objects.clear();
	_heliostats.clear();
	_helio_groups.clear();
	_helio_by_id.clear();
	_neighbors.clear();
	_receivers.clear();
	
	_is_created = false;
	_cancel_flag = false;	//initialize the flag for cancelling the simulation
	_optical_mesh.reset();

    _sf_area = 0.;
}

double SolarField::calcHeliostatArea(){
	/* Sum up the total aperture area of all active heliostats in the solar field */
	int Npos = (int)_heliostats.size();
	double Asf=0.;
	for(int i=0; i<Npos; i++){
		if(_heliostats.at(i)->IsInLayout()) 
            Asf += _heliostats.at(i)->getArea();
	}
	_sf_area = Asf;
	return Asf;
}

double SolarField::calcReceiverTotalArea(){
	/* 
	Sum and return the total absorber surface area for all receivers
	*/

	int nrec = (int)getReceivers()->size();
	double Atot = 0.;
	for(int i=0; i<nrec; i++){ 
        Receiver* Rec = getReceivers()->at(i);
		if(! Rec->isReceiverEnabled() ) continue;
		Atot += Rec->getAbsorberArea(); 
	}
	return Atot;
}

double SolarField::calcAverageAttenuation()
{
    if(_heliostats.size() > 0)
    {
        double att_ave=0;
        for(int i=0; i<(int)_heliostats.size(); i++)
        {
            att_ave += _heliostats.at(i)->getEfficiencyAtten();
        }
        return att_ave / (double)_heliostats.size();
    }
    else
    {
        double r_ave = _var_map->land.radmin_m.Val() + _var_map->land.radmax_m.Val();
        r_ave *= 0.5;

        return Ambient::calcAttenuation(*_var_map, r_ave);
    }
}

bool SolarField::UpdateNeighborList(double lims[4], double zen){
	/* 
	Update the neighbors associated with each heliostat based on the shadow extent. Determine this range
	based on the current sun position

	lims:
	0 : xmin
	1 : xmax
	2 : ymin
	3 : ymax
	*/
	
	double 
		xmax = lims[0],
		xmin = lims[1],
		ymax = lims[2],
		ymin = lims[3];

	//add a little buffer to the min/max extents
	if(xmax>0.) {xmax *= 1.01;} else {xmax *= 0.99;}
	if(xmin<0.) {xmin *= 1.01;} else {xmin *= 0.99;}
	if(ymax>0.) {ymax *= 1.01;} else {ymax *= 0.99;}
	if(ymin<0.) {ymin *= 1.01;} else {ymin *= 0.99;}
	
	//How many nodes should be in the mesh? Determine the shadow length at some low sun position (say 15deg)
	//and use this to size the nodes. Shadowing will be evaluated among heliostats in the current node
	//and surrounding 8 nodes.
	double rcol = 0.;
	double hm = 0.;
	for(htemp_map::iterator it = _helio_templates.begin(); it != _helio_templates.end(); it++){
        var_heliostat* Hv = it->second->getVarMap();
		rcol += it->second->getCollisionRadius();
		hm += Hv->height.val/2.;
	}
	rcol *= 1./(double)_helio_templates.size();
	hm *= 1./(double)_helio_templates.size();

	// This is the shadowing radius at 15deg. Use this as the x and y dimension of the mesh.
	// Multiply the cell width by 0.5 since several adjacent cells will participate in the shadowing. We don't
	// need each cell to be the full shadowing radius wide.
	double r_shad_max = fmax(2.*rcol/tan(PI/2.-zen), 3.*rcol);
	double r_block_max = 10.*hm;	//by similar triangles and assuming the maximum heliostat-to-tower distance is 10 tower heights, calculate max. blocking interaction radius
	double r_interact = max(r_shad_max, r_block_max);
	r_interact = fmin(r_interact, 2.*hm*100);	//limit to a reasonable value
	int ncol, nrow;
	double dcol, drow;
	ncol = max(1, int((xmax - xmin)/r_interact));		//The number of column nodes
	nrow = max(1, int((ymax - ymin)/r_interact));		//The number of row nodes
	dcol = (xmax - xmin)/float(ncol);
	drow = (ymax - ymin)/float(nrow);			//The column and row node width

	//resize the mesh array accordingly
	_helio_groups.resize_fill(nrow, ncol, Hvector());

	int col, row;	//indicates which node the heliostat is in
	int Npos = (int)_helio_objects.size();
	for(int i=0; i<Npos; i++){
		Heliostat *hptr = &_helio_objects.at(i);
		//Find which node to add this heliostat to
		row = (int)(floor((hptr->getLocation()->y - ymin)/drow));
		row = (int)fmax(0., fmin(row, nrow-1));
		col = (int)(floor((hptr->getLocation()->x - xmin)/dcol));
		col = (int)fmax(0., fmin(col, ncol-1));
		//Add the heliostat ID to the mesh node
		_helio_groups.at(row,col).push_back(hptr); 
		//Add the mesh node ID to the heliostat information
		hptr->setGroupId(row,col);
	}

	//Go over each node and compile a list of neighbors from the block of 9 adjacent cells.
	if(CheckCancelStatus()) return false;	//check for cancelled simulation
	int nh;
	_neighbors.resize_fill(nrow, ncol, Hvector());
	for(int i=0; i<nrow; i++){		//Loop over each row
		for(int j=0; j<ncol; j++){	//Loop over each column
			for(int k=i-1;k<i+2;k++){	//At each node position, get the surrounding nodes in the +/-y direction
				if(k<0 || k>nrow-1) continue;	//If the search goes out of bounds in the y direction, loop to the next position
				for(int l=j-1;l<j+2;l++){	//At each node position, get the surrounding nodes in the +/-x direction
					if(l<0 || l>ncol-1) continue;	//If the search goes out of bounds in the x direction, loop to the next position
					nh = (int)_helio_groups.at(k,l).size();	//How many heliostats are in this cell?
					for(int m=0; m<nh; m++){	//Add each heliostat element in the mesh to the list of neighbors for the current cell at (i,j)
						_neighbors.at(i,j).push_back( _helio_groups.at(k,l).at(m) );
					}
				}
			}
		}
	}
	if(CheckCancelStatus()) return false;	//check for cancelled simulation

	//For each heliostat, assign the list of neighbors that are stored in the _neighbors grid
	for(int i=0;i<Npos;i++){
		Heliostat *hptr = &_helio_objects.at(i);
		hptr->setNeighborList( &_neighbors.at( hptr->getGroupId()[0], hptr->getGroupId()[1] ) );	//Set the neighbor list according to the stored _neighbors indices
	}
	return true;

}

bool SolarField::UpdateLayoutGroups(double lims[4]){
	
	/* 
	Create an "optical mesh" of the field based on changes in the local view factor and
	intercept factor (approximate) throughout the field. Certain areas in the heliostat
	field will experience more rapid rates of change of optical performance, and we can
	choose the grouping of heliostats carefully to minimize the number of required zone
	calculations while making sure that we don't group together heliostats that are ac-
	tually different in performance.

	The method here uses the derivative of intercept factor and of view factor to dete-
	rmine the allowable grid size. Each region is evaluated and divided 'n' times until
	the change in the efficiency over the azimuthal and radial spans of the zone falls
	below the allowed tolerance. 

	Zones are assigned a unique binary code that represents their position in the field.
	The code is determined by dividing each variable space in half, and 0 corresponds 
	to the lower region while 1 is the upper. Divisions are made alternatingly between
	radial (first) and azimuthal dimensions. Once the division has reached satisfactory
	resolution in one dimension, subsequent potential divisions are denoted with 'x', as
	no division actually takes place. The remaining dimension continues division until
	the required tolerance is met. At the end of the division chain, the flat 't' indi-
	cates that the node is terminal and contains data.

	For example:
					10001x1xt
	represents:
	[r1]	outer
	[a1]	ccw
	[r2]	inner
	[a2]	ccw
	[r3]	outer
	[a3]	no division
	[r4]	outer
	[a4]	no division
	terminal

	In words, 
		(1) divide a circle into outer and inner radial sections - take the outer,
		(2) divide region 1 into two slices (halves) - take the counterclockwise one,
		(3) divide region 2 into outer and inner radial sections - take the inner,
	....
		the terminal node contains an array of data (heliostat pointers).

	Likewise, each heliostat can be assigned a unique binary tag using this method. 
	Simply take the location of the heliostat and place it within the domain by subse-
	quent division. Continue until both 'r' and 'az' dimensions have sufficient resol-
	ution. The heliostat key will be entirely comprised of 0's and 1's.

	A heliostat can be placed in a zone by comparing the zone and heliostat location keys
	bitwise until the termination flag is reached. Where the zone key has an 'x', the
	zone does not split, and so the heliostat will continue down the mesh tree on the
	only available path. Once the 't' flag is reached, the heliostat is dropped into the
	data container.


	This method has been implemented as a means for quickly sorting a large number of 
	heliostats that are known in x-y coordinates into a grid with variable mesh spacing
	that is defined in cylindrical coordinates. This method can probably be applied to
	sorting in other coordinate systems, but it's application here is specific to this
	problem.
	*/

	double 
		xmax = lims[0],
		xmin = lims[1],
		ymax = lims[2],
		ymin = lims[3];

	//add a little buffer to the min/max extents
	xmax = xmax > 0. ? xmax * 1.01 : xmax * 0.99;
	xmin = xmin > 0. ? xmin * 1.01 : xmin * 0.99;
	ymax = ymax > 0. ? ymax * 1.01 : ymax * 0.99;
	ymin = ymin > 0. ? ymin * 1.01 : ymin * 0.99;

    var_solarfield *Sv = &_var_map->sf;
    var_receiver *Rv = &_var_map->recs.front();
    
	//create objects needed 
	LayoutData mesh_data;
	mesh_data.extents_az[0] = Sv->accept_min.val;
	mesh_data.extents_az[1] = Sv->accept_max.val;
	mesh_data.tht = Sv->tht.val;
	mesh_data.alpha = Rv->rec_azimuth.val*D2R;
	mesh_data.theta = Rv->rec_elevation.val*D2R;
	//double width;
	Receiver *rec = _receivers.front();
	mesh_data.w_rec = Rv->rec_width.val; //sqrt(powf(_receivers.front()->getReceiverWidth(),2) + powf(_receivers.front()->getReceiverHeight(),2));
	mesh_data.flat = rec->getGeometryType() != Receiver::REC_GEOM_TYPE::CYLINDRICAL_CLOSED
					&& rec->getGeometryType() != Receiver::REC_GEOM_TYPE::CYLINDRICAL_OPEN;
	mesh_data.f_tol = Sv->zone_div_tol.val;
	mesh_data.max_zsize_a = Sv->max_zone_size_az.val*Sv->tht.val;
	mesh_data.max_zsize_r = Sv->max_zone_size_rad.val*Sv->tht.val;
	mesh_data.min_zsize_a = Sv->min_zone_size_az.val*Sv->tht.val;
	mesh_data.min_zsize_r = Sv->min_zone_size_rad.val*Sv->tht.val;
	
	//mesh separately for each heliostat template
	int ntemp = (int)_helio_templates.size();
	vector<vector<opt_element> > all_nodes(ntemp);
	_layout_groups.clear();

	for(htemp_map::iterator it=_helio_templates.begin(); it!=_helio_templates.end(); it++){
		
        var_heliostat* Hv = it->second->getVarMap();

		double trange[2], arange[2];
		TemplateRange(it->first, Sv->template_rule.mapval(), trange, arange);
		mesh_data.extents_r[0] = trange[0];
		mesh_data.extents_r[1] = trange[1];
		mesh_data.extents_az[0] = arange[0];
		mesh_data.extents_az[1] = arange[1];
		
		int fmethod = Hv->focus_method.mapval();
		mesh_data.onslant = fmethod == 1;
		switch(fmethod)
		{
		//case 0:
        case var_heliostat::FOCUS_METHOD::FLAT:
			mesh_data.L_f = 1.e9;	//infinite
			break;
		//case 1:
        case var_heliostat::FOCUS_METHOD::AT_SLANT:
			break;	//on slant. this is handled internally
		//case 2:
        case var_heliostat::FOCUS_METHOD::GROUP_AVERAGE:
			//average of group
			mesh_data.L_f = (trange[0] + trange[1])/2.;
			break;
		//case 3:
        case var_heliostat::FOCUS_METHOD::USERDEFINED:
			//user specified
			mesh_data.L_f = sqrt(pow(it->second->getFocalX(),2) + pow(it->second->getFocalY(),2));
			break;
		};
		
		mesh_data.H_h = Hv->height.val;
		mesh_data.H_w = Hv->width.val;
		if( Hv->is_faceted.val ){
			mesh_data.nph = Hv->n_cant_y.val;
			mesh_data.npw = Hv->n_cant_x.val;
		}
		else{
			mesh_data.nph = mesh_data.npw = 1;
		}
		//calculate the total reflected beam error distribution
		double err[2], errnorm=0., errsurf;
        err[0] = Hv->err_azimuth.val;
        err[1] = Hv->err_elevation.val;
		errnorm = err[0]*err[0] + err[1]*err[1];
        err[0] = Hv->err_surface_x.val;
        err[1] = Hv->err_surface_y.val;
		errnorm += err[0]*err[0] + err[1]*err[1];
        err[0] = Hv->err_reflect_x.val;
        err[1] = Hv->err_reflect_y.val;
		errsurf = err[0]*err[0] + err[1]*err[1];	
		mesh_data.s_h = sqrt(4.*errnorm + errsurf);

		//Set the maximum error in the binary tag location
		mesh_data.t_res = fmin(mesh_data.H_h, mesh_data.H_w)/10.;   

		//Create the mesh
		_optical_mesh.reset();
		_optical_mesh.create_mesh(&mesh_data);

		//Add all of the heliostats with this template type to the mesh
		for( vector<Heliostat>::iterator hit = _helio_objects.begin(); hit != _helio_objects.end(); hit++){
			if( hit->getMasterTemplate() != it->second ) continue;
			sp_point *loc = hit->getLocation();
			_optical_mesh.add_object( &(*hit), loc->x, loc->y);
		}

		//Now add all of the layout groups with heliostats to the main array
		vector<vector<void* >* > tgroups = _optical_mesh.get_terminal_data();
		for(int i=0; i<(int)tgroups.size(); i++){
			int ntgroup = (int)tgroups.at(i)->size();
			if(ntgroup == 0) continue;
			_layout_groups.push_back(Hvector());
			for(int j=0; j<ntgroup; j++){
				_layout_groups.back().push_back( (Heliostat*)tgroups.at(i)->at(j) );
			}
		}
	}
	//report to the log window the heliostat simulation reduction ratio
	char msg[200];
	sprintf(msg, "Identified %d optical zones (%.1f avg size)", 
		(int)_layout_groups.size(), (double)_heliostats.size()/(double)_layout_groups.size());
	_sim_info.addSimulationNotice(msg);

	if(CheckCancelStatus()) return false;	//check for cancelled simulation

	return true;

}

bool SolarField::FieldLayout(){
	/* 
	This should only be called by the API. If using the GUI, manually call PrepareFieldLayout(), 
	DoLayout(), and ProcessLayoutResults() from the interface. This call is not capable of 
	multithreading.
	*/
	WeatherData wdata; //Weather data object will be filled in PrepareFieldLayout(...)
	bool needs_sim = PrepareFieldLayout(*this, &wdata);
	
	if( needs_sim){
		//vector<double> results;
		sim_results results;
		int sim_first, sim_last;
		sim_first = 0;
		sim_last = (int)wdata.DNI.size();
		if(! DoLayout(this, &results, &wdata, sim_first, sim_last) )
            return false;

		//For the map-to-annual case, run a simulation here
		if(_var_map->sf.des_sim_detail.mapval() == var_solarfield::DES_SIM_DETAIL::EFFICIENCY_MAP__ANNUAL)
			SolarField::AnnualEfficiencySimulation( _var_map->amb.weather_file.val, this, results); //, (double*)NULL, (double*)NULL, (double*)NULL);


		ProcessLayoutResults(&results, sim_last - sim_first);
	}
    else
    {
        //update the layout data
		ProcessLayoutResultsNoSim();
    }

    return true;

}

bool SolarField::PrepareFieldLayout(SolarField &SF, WeatherData *wdata, bool refresh_only){
	/*
	This algorithm is used to prepare the solar field object for layout simulations.
	A simulation is performed in this algorithm only if loading specified coordinates
	or if the heliostats are not filtered by performance.

	After calling this method, call DoLayout(...) to run the simulation for
	the given weather data steps, then call ProcessLayoutResults(...) to filter the
	heliostats by performance.
	
	This method uses set geometry values (no optimization within this algorithm), 
	so receiver, heliostat, and tower height geometries should be specified a priori.

	This method can also be called in "refresh_only=true" mode, where solar field 
	geometry parameters are updated without recalculating the heliostat positions.


	*****Procedure for laying out the field**************

	-> Choose a tower height and receiver geometry
	-> Determine all of the available positions for heliostats
		- Enforce any land constraints
	-> Create an unordered_map of heliostats to fill these positions
		- Update each heliostat object with the appropriate slant range
		- Update each heliostat's tracking vector to the time/day desired for canting (if applicable)
		- "Install" the panels on each heliostat, assigning canting and focal point
	-> Analyze each heliostat for optical efficiency at a range of timesteps
	-> Apply a filtering algorithm to determine which heliostats should be removed
	-> Do an annual performance run to find total productivity
	-> Calculate the estimated project IRR
		- Include market factors
	-> Adjust independent variables and repeat the process to find optimal system

	*****************************************************
	
	*/
	
    if(!refresh_only && !wdata )
        throw spexception("Prepare field layout called without a weather data object.");

	if(! SF.getSimInfoObject()->addSimulationNotice("Generating solar field heliostat layout") )
    {
        SF.CancelSimulation();
        return false;
    }
	if(SF.CheckCancelStatus()) return false;	//check for cancelled simulation

	//variables
	vector<sp_point> HelPos;		//Vector pointer for heliostat positions
	
    var_map *V = SF.getVarMap();

	//If the analysis method is Hermite Polynomial, initialize polynomial coefs
	SF.getFluxObject()->initHermiteCoefs( *V );
	//Calculate the Hermite geometry coefficients for each template
	for(htemp_map::iterator htemp=SF.getHeliostatTemplates()->begin(); 
                            htemp != SF.getHeliostatTemplates()->end(); 
                            htemp++)
    {
        if( htemp->second->IsEnabled() )
		    SF.getFluxObject()->hermiteMirrorCoefs(*htemp->second, V->sf.tht.val );
	}

	//Calculate available heliostat positions
	SF.getSimInfoObject()->addSimulationNotice("Calculating available heliostat positions");

	if(SF.CheckCancelStatus()) return false;	//check for cancelled simulation

	int layout_method = V->sf.layout_method.mapval();

	layout_shell *layout = SF.getLayoutShellObject();

	if(!refresh_only && layout_method == var_solarfield::LAYOUT_METHOD::RADIAL_STAGGER)
    {		//Radial stagger
		SF.radialStaggerPositions(HelPos);
	}
	else if(!refresh_only && layout_method == var_solarfield::LAYOUT_METHOD::CORNFIELD){	//Cornfield rows (eSolar type)
		SF.cornfieldPositions(HelPos);
	}
	else if(layout_method == var_solarfield::LAYOUT_METHOD::USERDEFINED || refresh_only){	//User-defined field
		/*
		Take a previously defined layout and build up the heliostat objects
		*/
		int nh = (int)layout->size();	//Number of heliostats

		//Resize the HelPos array
		HelPos.resize(nh);

		//Assign the xyz location.. leave assignment of the other information provided in the user
		//data for later.
		for(int i=0; i<nh; i++){
			HelPos.at(i) = layout->at(i).location;
		}

	}
	
	//Weed out the heliostats that lie outside of the land boundary
	//Exclude any points that aren't within the land boundaries
    if(! SF.getSimInfoObject()->addSimulationNotice("Checking for land boundary exclusions") ){
        SF.CancelSimulation();
        return false;
    }
	if(SF.CheckCancelStatus()) return false;	//check for cancelled simulation
	if( V->land.is_bounds_array.val )
    {
		vector<int> dels;
		//Find which points lie outside the bounds
		for(unsigned int j=0; j<HelPos.size(); j++){
			if(! SF.getLandObject()->InBounds(V->land, HelPos.at(j), V->sf.tht.val) ){ dels.push_back(j); }
		}
		//Delete in reverse order
		int nd = (int)dels.size();
		for(int i=0; i<nd; i++){
			HelPos.erase( HelPos.begin()+ dels.at(nd-1-i) );
		}

        //check for problems here
        if(HelPos.size() == 0 )
            throw spexception("The specified land boundaries resulted in an infeasible design. "
                              "No heliostats could be placed in the layout set. Please review "
                              "your settings for land constraints.");
	}
    /* 
    
    enforce other exclusions (receiver acceptance, receiver cylinder
    
    */

    //receiver diameter - delete heliostats that fall within the diameter of the receiver
    //solar field span angles - delete heliostats that are outside of the allowable angular range
    if(SF.getReceivers()->size() == 1 
        && SF.getReceivers()->front()->getVarMap()->rec_type.mapval() == var_receiver::REC_TYPE::EXTERNAL_CYLINDRICAL)
    {
        double azmin = SF.getVarMap()->sf.accept_min.val*D2R;
        double azmax = SF.getVarMap()->sf.accept_max.val*D2R;

        vector<int> dels;
        for(size_t j=0; j<HelPos.size(); j++){
            double x = HelPos.at(j).x;
            double y = HelPos.at(j).y;

            //radial position check
            double h_rad = sqrt(x*x + y*y);

            if( h_rad < SF._var_map->recs.front().rec_diameter.val/2.)
            {
                dels.push_back((int)j);
                continue; //can't delete twice
            }

            //azimuthal position check
            double az = atan2(x,y);
            if( (az > azmax) || (az < azmin) )
                dels.push_back((int)j);

        }
        //Delete in reverse order
		int nd = (int)dels.size();
		for(int i=0; i<nd; i++){
			HelPos.erase( HelPos.begin()+ dels.at(nd-1-i) );
		}
    }
    //Receiver span angles
    if(SF.getReceivers()->size() == 1){
        vector<int> dels;
        //receiver acceptance angle
        Receiver *Rec = SF.getReceivers()->front();
        var_receiver *Rv = Rec->getVarMap();

        int rectype = Rv->rec_type.mapval();
        int j=0;
        for(vector<sp_point>::iterator hpos = HelPos.begin(); hpos != HelPos.end(); hpos++){

            if(rectype == var_receiver::REC_TYPE::FLAT_PLATE){ // Receiver::REC_TYPE::FLAT_PLATE || rectype == Receiver::REC_TYPE::CAVITY){
		        PointVect rnv;
		        Rec->CalculateNormalVector(rnv);

                //calculate the vector from the receiver to the heliostat
                sp_point offset;
                offset.Set(
                    Rv->rec_offset_x.val,
                    Rv->rec_offset_y.val,
                    Rv->rec_offset_z.val);

                double tht = SF.getVarMap()->sf.tht.val; 
                
                Vect hv_r;
                hv_r.i = hpos->x - offset.x;
                hv_r.j = hpos->y - offset.y;
                hv_r.k = hpos->z - tht;
                Toolbox::unitvect(hv_r);

                //Rotate into receiver aperture coordinates
		        double raz = Rv->rec_azimuth.val*R2D;
		        double rel = Rv->rec_elevation.val*R2D;
		        
                Toolbox::rotation(PI - raz, 2, hv_r);
		        Toolbox::rotation(PI - rel, 0, hv_r);

		        double theta_x = atan2(hv_r.i, hv_r.j);
		        double theta_y = atan2(hv_r.k, sqrt(hv_r.i*hv_r.i + hv_r.j*hv_r.j));

		        //check whether the angles are within the allowable range
		        double 
                    acc_x = Rv->accept_ang_x.val*R2D,
                    acc_y = Rv->accept_ang_y.val*R2D;
		        acc_x *= 0.5;
		        acc_y *= 0.5;
		        if(Rv->accept_ang_type.mapval() == var_receiver::ACCEPT_ANG_TYPE::RECTANGULAR){ //Rectangular
			        if(! (fabs(theta_x) < acc_x && fabs(theta_y) < acc_y))
				        dels.push_back(j);
		        }
		        else{	//Elliptical
			        if( (theta_x*theta_x / (acc_x * acc_x) + theta_y*theta_y / (acc_y * acc_y)) > 1. )
				        dels.push_back(j);
		        }
	        }
            j++;
        }
        //Delete in reverse order
		int nd = (int)dels.size();
		for(int i=0; i<nd; i++){
			HelPos.erase( HelPos.begin()+ dels.at(nd-1-i) );
		}
    }

    //--------------------

	if(layout_method == var_solarfield::LAYOUT_METHOD::USERDEFINED || refresh_only)
		//update the layout positions in the land object here since the post-process call isn't made after this
		//SF.getLandObject()->setLayoutPositions(HelPos);
        SF.getLandObject()->calcLandArea( V->land, HelPos );

	//----For all of the heliostat positions, create the heliostat objects and assign the correct canting/focusing
	int Npos = (int)HelPos.size();
	
    if(! SF.getSimInfoObject()->addSimulationNotice("Calculating heliostat canting and aim point information") ){
        SF.CancelSimulation();
        return false;
    }
	if(SF.CheckCancelStatus()) return false;	//check for cancelled simulation

	//Clear out the _heliostats array, we'll reconstruct it here
	Hvector *heliostats = SF.getHeliostats();
	heliostats->clear();
	heliostats->resize(Npos);
	
	//Set up the locations array
	vector<Heliostat> *helio_objects = SF.getHeliostatObjects();
	helio_objects->resize(Npos);
	Heliostat *hptr; //A temporary pointer to avoid retrieving with "at()" over and over
	int focus_method;
	//A temporary point to pass to the template function
	sp_point P; P.x = 0; P.z = 0;
	sp_point Aim;		//The aim point [m]
	//Keep track of the min/max field extents too
	double xmin=9.e99, xmax=-9.e99, ymin=9.e99, ymax=-9.e99;
	double hpx, hpy, hpz;
	
	//For each heliostat position
	for(int i=0; i<Npos; i++){
		hpx = HelPos.at(i).x;
		hpy = HelPos.at(i).y;
		hpz = HelPos.at(i).z;
		//P.y = sqrt(pow(hpx, 2) + pow(hpy, 2));	//Determine the radial position. Set to y.
        Heliostat *htemp;
        if( layout_method == var_solarfield::LAYOUT_METHOD::USERDEFINED )
        {
            try
            {
                htemp = SF.getHeliostatTemplates()->at( layout->at(i).helio_type );
            }
            catch(...)
            {
                htemp = SF.getHeliostatTemplates()->begin()->second;
            }
        }
        else
        {
		    htemp = SF.whichTemplate(V->sf.template_rule.mapval(), HelPos.at(i));
        }
		helio_objects->at(i) = *htemp;	//Copy the template to the heliostat
		hptr = &helio_objects->at(i);	//Save a pointer for future quick reference
		//Save a pointer to the template for future reference
		hptr->setMasterTemplate( htemp );

        var_heliostat *Hv = hptr->getVarMap();

		//Set up the heliostat
		int layout_method = V->sf.layout_method.mapval();
		if(layout_method != var_solarfield::LAYOUT_METHOD::USERDEFINED){	
			//algorithmic layouts (not user defined)
			focus_method = Hv->focus_method.mapval();
		}
		else{
			//User defined layouts - need to check for user defined canting and focusing
			if(layout->at(i).is_user_cant) {
				hptr->IsUserCant( true );
                Vect cant;
                cant.Set( layout->at(i).cant.i, layout->at(i).cant.j, layout->at(i).cant.k );
                hptr->setCantVector( cant );
			}
			else{
				hptr->IsUserCant( false );
			}

			if(layout->at(i).is_user_focus) {
				focus_method = 3;	//user defined
                hptr->setFocalLengthX( layout->at(i).focal_x );
                hptr->setFocalLengthY( layout->at(i).focal_y );
			}
			else{
			    focus_method = hptr->getVarMap()->focus_method.mapval();
			}
		}

		hptr->setLocation(hpx, hpy, hpz);	//Set the position
		if(hpx < xmin){xmin = hpx;}
		if(hpx > xmax){xmax = hpx;}
		if(hpy < ymin){ymin = hpy;}
		if(hpy > ymax){ymax = hpy;}		//Track the min/max field extents

		//Quickly determine the aim point for initial calculations
		if(layout_method == var_solarfield::LAYOUT_METHOD::USERDEFINED || refresh_only){
			//For user defined layouts...
			if(layout->at(i).is_user_aim){	//Check to see if the aim point is also supplied.
				SF.getFluxObject()->simpleAimPoint(*hptr, SF); //Calculate simple aim point first to associate heliostat with receiver.
				Aim = layout->at(i).aim;	//Assign the specified aim point
				hptr->setAimPoint(Aim);		
                //update the image plan aim point
                SF.getFluxObject()->keepExistingAimPoint(*hptr, SF, 0);
			}
			else{	
				SF.getFluxObject()->simpleAimPoint(*hptr, SF);
				Aim = *hptr->getAimPoint();				
			}

			//set status
			hptr->IsEnabled(layout->at(i).is_enabled);
			hptr->setInLayout(layout->at(i).is_in_layout);
		}
		else{	//Otherwise,
			//Determine the simple aim point - doesn't account for flux limitations
			SF.getFluxObject()->simpleAimPoint(*hptr, SF);
			Aim = *hptr->getAimPoint();
		}
		
		//Aim points have been set
		SF.setAimpointStatus(true);

        //calculate and update the heliostat-to-tower vector
        Vect htow;
        htow.Set( Aim.x - hpx, Aim.y - hpy, Aim.z - hpz);
        
		//Calculate the slant range.. This should be the exact slant range.
		//double slant = sqrt(pow(Aim.x - hpx,2) + pow(Aim.y - hpy,2) + pow(Aim.z - hpz,2));
        double slant = Toolbox::vectmag( htow );

        //update the tower vector as a unit vector
        Toolbox::unitvect(htow);
        //Set the tower vector
        hptr->setTowerVector( htow );

		hptr->setSlantRange( slant );
		
		//Choose how to focus the heliostat
		switch(focus_method)
		{
		case 0:	//Flat with no focusing
			hptr->setFocalLength( 1.e9 );
			break;
		case 1:	//Each at a focal length equal to their slant range
			hptr->setFocalLength( hptr->getSlantRange() );

			break;
		case 2:	//Average focal length in template group
			throw spexception("Average template focal length not currently implemented.");
			break;
		case 3:	//User defined focusing method

			break;
		}

        

		//Construct the panel(s) on the heliostat
        hptr->installPanels();

		//Assign a unique ID to the heliostat
		hptr->setId(i);

		//Update progress
		//_sim_info.setSimulationProgress(double(i)/double(Npos));
		if(SF.CheckCancelStatus()) return false;	//check for cancelled simulation
	}
	
	//----Determine nearest neighbors---
	//First go through and put each heliostat into a regional group. The groups are in a cartesian mesh.
	//Save the heliostat field extents [xmax, xmin, ymax, ymin]
	
	SF.setHeliostatExtents(xmax, xmin, ymax, ymin);

    if(! SF.getSimInfoObject()->addSimulationNotice("Determining nearest neighbors for each heliostat") ){
        SF.CancelSimulation();
        return false;
    }
	
	//Update the neighbor list based on zenith. Also initialize the layout groups.
	double *helio_extents = SF.getHeliostatExtents();
	bool isok = SF.UpdateNeighborList(helio_extents, 0. );	//_ambient.getSolarZenith());   don't include shadowing effects in layout (zenith = 0.)
	if(SF.CheckCancelStatus() || !isok) return false;	//check for cancelled simulation
	if(V->sf.is_opt_zoning.val ){
        if(! SF.getSimInfoObject()->addSimulationNotice("Calculating layout optical groups") ){
            SF.CancelSimulation();
            return false;
        }
		isok = SF.UpdateLayoutGroups(helio_extents); //Assign heliostats to groups for simplified intercept factor calculations
		if(SF.CheckCancelStatus() || !isok) return false;
		SF.getSimInfoObject()->addSimulationNotice("Optical group calculations complete");
	}
	
	//----Over each heliostat, calculate optical efficiency for the set of design points
	
	//which simulation criteria should we use to filter the heliostats?
	int des_sim_detail = V->sf.des_sim_detail.mapval();
	unordered_map<int, Heliostat*> *helio_by_id = SF.getHeliostatsByID();

	if(des_sim_detail == var_solarfield::DES_SIM_DETAIL::DO_NOT_FILTER_HELIOSTATS || refresh_only){		//Do not filter any heliostats
		
        
		//Simulate the default design point to ensure equal comparison
        vector<string> vdata = split(Ambient::getDefaultSimStep(), ",");
        int hour, dom, month;
        to_integer(vdata.at(0), &dom);
        to_integer(vdata.at(1), &hour);
        to_integer(vdata.at(2), &month);
        sim_params P;
        //dni, T, P, V, Wt
        to_double(vdata.at(3), &P.dni);
        to_double(vdata.at(4), &P.Tamb);
        to_double(vdata.at(5), &P.Patm);
        to_double(vdata.at(6), &P.Vwind);
        to_double(vdata.at(7), &P.Simweight);

        
        DTobj dt;
        dt.setZero();
        dt._mday = dom;
        dt._hour = hour;
        dt._month = month;

        //Calculate the sun position vector
        double az, zen;
        Ambient::calcSunPosition(*V, dt, &az, &zen);
        Vect sunvect = Ambient::calcSunVectorFromAzZen(az, zen);
 
        //Add all of the heliostat locations to the final vector
		heliostats->resize(Npos);
		for(int i=0; i<Npos; i++){	
			heliostats->at(i) = &helio_objects->at(i);
			//heliostats->at(i)->setInLayout(true);	//All of the heliostats are included in the final layout
			//Update the tracking vector. This defines corner geometry for plotting.
			heliostats->at(i)->updateTrackVector(sunvect);
		}

        SF.SimulateTime(hour, dom, month, P);

		//Save the heliostats in an array by ID#
		helio_by_id->clear();
		for(int i=0; i<Npos; i++){
			(*helio_by_id)[ heliostats->at(i)->getId() ] = heliostats->at(i);
		}
	}
	else if(des_sim_detail != var_solarfield::DES_SIM_DETAIL::DO_NOT_FILTER_HELIOSTATS){	
					//1 :: Subset of days and hours, specified
					//2 :: Subset of days, all hours during the days
					//3 :: Full annual simulation from Weather file
					//4 :: Limited annual simulation from weather file
					//5 :: Representative profiles

		//Copy the previously calculated weather data into the object passed to this method
		SF.copySimulationStepData(*wdata);

		//Check to see if at least one data value is available in the weather data object
		int nsim = wdata->size();
		if( nsim == 0 ){
			SF.getSimErrorObject()->addSimulationError((string)"No design-point data was provided for calculation of the solar field power output. Use setStep(...) to assign DNI, day, and hour info.",true,false);
			return false;
		}
		else{
			//if needed, determine clear sky DNI and replace the weather file DNI
			if (V->amb.insol_type.mapval() != var_ambient::INSOL_TYPE::WEATHER_FILE_DATA){

				//DateTime *dtc = ambient->getDateTimeObj();
				DateTime dt;
				double dom, hour, month, dni, tdb, pres, wind, az, zen, step_weight;
				for(int i=0; i<wdata->size(); i++){
					//Get the design-point day, hour, and DNI
					wdata->getStep(i, dom, hour, month, dni, tdb, pres, wind, step_weight);

					//Convert the day of the month to a day of year
					int doy = dt.GetDayOfYear(2011,int(month),int(dom));
				
					//Calculate the sun position
                    Ambient::setDateTime(dt, hour, doy);
					//latitude, longitude, and elevation should be set in the input file
					Ambient::calcSunPosition(*V, dt, &az, &zen, true); 
					
					//calculate DNI
					double dniclr = Ambient::calcInsolation(*V, az*D2R, zen*D2R, doy);
					//Set to the new value
					wdata->setStep(i, dom, hour, month, dniclr, tdb, pres, wind, step_weight);

				}

				//reset to the old date time
				//ambient->setDateTime(dtc->_hour, dtc->GetDayOfYear());
			}
			
			//Set up the _heliostats array
			for(int i=0; i<Npos; i++){
				heliostats->at(i) = &helio_objects->at(i);		//At first, the heliostats array will contain all of the positions in the field
				heliostats->at(i)->resetMetrics();	//Reset all of the heliostat metrics to the original state 
			}
		}
		SF.getSimInfoObject()->setTotalSimulationCount(nsim);
			
		return true;	//Return a flag indicating that additional simulation is required
	}
	return false;	//if not a detailed design, we don't need additional simulation.

}

bool SolarField::DoLayout( SolarField *SF, sim_results *results, WeatherData *wdata, int sim_first, int sim_last){
	/* 
	This algorithm is static (i.e. it only refers to arguments passed to it and not to the
	"this" SolarField object. 

	The algorithm will simulate the performance of all heliostats in the SF heliostats array for timesteps
	in wdata from sim_first to sim_last. 
	
	If sim_first arg is not set, it will be set to 0.
	if sim_last arg is not set, it will be set to wdata->size()
	
	Threading note:
	This method is duplicated in the LayoutSimThread class. Ensure both methods are substantively consistent
	for predictable simulation behavior.

	*/
    if(! SF->getSimInfoObject()->addSimulationNotice("Simulating design-point conditions") ){
        SF->CancelSimulation();
        return false;
    }
		
	double dni, dom, doy, hour, month, tdb, pres, wind, step_weight;
    int hoy=0;
	bool is_pmt_factors = SF->getVarMap()->fin.is_pmt_factors.val;

    vector<double> *tous = &SF->getVarMap()->fin.pricing_array.Val();

	//int Npos = SF->getHeliostats()->size();

	//Simulate for each time
	//int nsim_actual=0;	//keep track of the day of the year for _des_sim_detail = 3
	//double dni_ave=0.;	//keep track of the average DNI value

	if(SF->CheckCancelStatus()) return false;	//check for cancelled simulation

	if(sim_first < 0) sim_first = 0;
	if(sim_last < 0) sim_last = wdata->size();

	int nsim = sim_last - sim_first + 1;
    
    //reserve memory
    try
    {
        results->reserve(nsim);
    }
    catch(...)
    {
        SF->getSimInfoObject()->addSimulationNotice("Error allocating memory for layout results vector");
    }


    DateTime DT;
	
    SF->getSimInfoObject()->setTotalSimulationCount(nsim);

	for(int i=sim_first; i<sim_last; i++){
		if(! SF->getSimInfoObject()->setCurrentSimulation(i+1) )
            return false;

		//Get the design-point day, hour, and DNI
		wdata->getStep(i, dom, hour, month, dni, tdb, pres, wind, step_weight);

		//Convert the day of the month to a day of year
		doy = DT.GetDayOfYear(2011,int(month),int(dom));
        		
		//Calculate the sun position
		Ambient::setDateTime(DT, hour, doy);
		
        if(is_pmt_factors)
            hoy = DT.GetHourOfYear();
		
        //latitude, longitude, and elevation should be set in the input file
	    double az;
        double zen;
		Ambient::calcSunPosition(*SF->getVarMap(), DT, &az, &zen, true); 
		//If the sun is not above the horizon, don't continue
		if( zen > 90. )
				continue;
		
		az *= D2R;
        zen *= D2R;

		//Simulate field performance
        sim_params P;
        P.dni = dni;
        P.Tamb = tdb;
        P.Vwind = wind;
        P.Patm = pres/1000.;
        P.Simweight = step_weight;
        if(is_pmt_factors)
            P.TOUweight = tous->at(hoy);
        P.is_layout = true;
		SF->Simulate(az, zen, P);
		//nsim_actual ++; dni_ave+=dni;

		//store the results
		results->push_back( sim_result() );
        double azzen[] = {az,zen};
		results->back().process_analytical_simulation( *SF, 0, azzen); //2);

		if(SF->CheckCancelStatus()) return false;	//check for cancelled simulation				
	}

    return true;
}		

void SolarField::ProcessLayoutResultsNoSim()
{
    ProcessLayoutResults(0,0);
}

void SolarField::ProcessLayoutResults( sim_results *results, int nsim_total){
	/*
	The sort metrics are mapped from the GUI in the following order:
	0	|	Power to the receiver
	1	|	Total efficiency
	2	|	Cosine efficiency
	3	|	Attenuation efficiency
	4	|	Intercept efficiency
	5	|	Blocking efficiency
	6	|	Shadowing efficiency
	7	|	Market-weighted power to the receiver

	Default is 7.
	*/
    bool needs_processing = results != 0 && nsim_total > 0;

	int Npos = (int)_heliostats.size();
    int nresults = 0; //initialize

    if( needs_processing )
    {
	    _sim_info.ResetValues();
        if(! _sim_info.addSimulationNotice("Ranking and filtering heliostats") ) {
            CancelSimulation();
            return;
        }

	    //Update each heliostat with the ranking metric and sort the heliostats by the value 
	    vector<double> hsort(Npos);
	    double rmet; //, nsimd=(float)nsim_total;
	
	    //Save the heliostats in an array by ID#
	    _helio_by_id.clear();
	    for(int i=0; i<Npos; i++){
		    _helio_by_id[ _heliostats.at(i)->getId() ] = _heliostats.at(i);
	    }

	    //Of all of the results provided, calculate the average of the ranking metric
	    int rid = _var_map->sf.hsort_method.mapval();
	
	    nresults = (int)results->size();

        //compile the results from each simulation by heliostat
        for(int i=0; i<Npos; i++){
		    int hid = _heliostats.at(i)->getId();	//use the heliostat ID from the first result to collect all of the other results
		    rmet = 0.;      //ranking metric
		    for(int j=0; j<nresults; j++)
                rmet += results->at(j).data_by_helio[ hid ].getDataByIndex( rid );      //accumulate ranking metric as specified in rid

            //normalize for available heliostat power if applicable
            double afact = 1.;
            if( rid == helio_perf_data::PERF_VALUES::POWER_TO_REC || rid == helio_perf_data::PERF_VALUES::POWER_VALUE )
                afact = _heliostats.at(i)->getArea();
        
            double rank_val = rmet / (afact*(float)nresults);       //Calculate the normalized ranking metric. Divide by heliostat area if needed
		
            _helio_by_id[hid]->setRankingMetricValue( rank_val );   //store the value
		    hsort.at(i) = rank_val;                                 //also store in the sort array
		
	    }

        //quicksort by ranking metric value. Correlate _heliostats vector
	    quicksort(hsort, _heliostats, 0, Npos-1);
    }

    //Simulate the default design point to ensure equal comparison
	double az_des, zen_des;
    if(! CalcDesignPtSunPosition(_var_map->sf.sun_loc_des.mapval(), az_des, zen_des) )
        return;
    sim_params P;
    P.dni = _var_map->sf.dni_des.val;
    P.Tamb = 25.;
    P.Patm = 1.;
    P.Vwind = 0.;
    P.Simweight = 1.;
    P.is_layout = true;

	if( zen_des > 90. )
    {
        throw spexception("The design point sun position is invalid. Simulation could not be completed.");
        return;
    }
    else
    {
        Simulate(az_des*D2R, zen_des*D2R, P);
    }
	

	_q_to_rec = 0.;
	for(int i=0; i<Npos; i++){
		_q_to_rec += _heliostats.at(i)->getPowerToReceiver();
	}

	//Calculate the required incident power before thermal losses
	double q_loss_tot = 0.;
	for(int i=0; i<(int)_receivers.size(); i++){
		_receivers.at(i)->CalculateThermalLoss(1., 0.);
		double 
			ql = _receivers.at(i)->getReceiverThermalLoss(),
			qp = _receivers.at(i)->getReceiverPipingLoss();
		q_loss_tot += ql + qp;
	}

	double q_inc_des = _var_map->sf.q_des.val + q_loss_tot;
	_q_des_withloss = q_inc_des; //save this

	//Check that the total power available is at least as much as the design point requirement. If not, notify
	//the user that their specified design power is too high for the layout parameters.
	if(_q_to_rec < q_inc_des*1.e6 && needs_processing)
    {
		string units;
		double 
			xexp = log10(_q_to_rec),
			xmult;
		if(xexp>9.) {
			units = "GW";
			xmult = 0.001;
		}
		else if(xexp>6.) {
			units = "MW";
			xmult = 1.;
		}
		else{
			units = "kW";
			xmult = 1000.;
		}
			
		char msg[1000];
		sprintf(msg, 
			"The maximum available power for this field layout is %.2f %s, and the required design power is %.2f %s."
			"The field cannot generate sufficient power to meet the design requirement. Consider adjusting design point conditions "
			"to generate a satisfactory design.", (float)(_q_to_rec*1.e-6*xmult), units.c_str(), (float)(q_inc_des*xmult), units.c_str());
		_q_des_withloss = q_inc_des;
		_sim_error.addSimulationError(string(msg)); //, true, true);
		//return;
	}

    if( needs_processing )
    {
	    double filter_frac = _var_map->sf.is_prox_filter.val ? _var_map->sf.prox_filter_frac.val : 0.;
	
	    //Determine the heliostats that will be used for the plant
	    double q_cutoff=_q_to_rec;	//[W] countdown variable to decide which heliostats to include
	    int isave;
	    int nfilter=0;
	    double q_replace=0.;
	    for(isave=0; isave<Npos; isave++){
		    //
		    double q = _heliostats.at(isave)->getPowerToReceiver();
		    q_cutoff += -q;
		    if(q_cutoff < q_inc_des*1.e6) {
			    nfilter++;
			    q_replace += q;
		    }
		    if(q_cutoff < q_inc_des*1.e6*(1.-filter_frac)){break;}
	    }
	    //The value of isave is 1 entry more than satisfied the criteria.

	    if(_var_map->sf.is_prox_filter.val){
		    //sort the last nfilter*2 heliostats by proximity to the reciever and use the closest ones. 
		    Hvector hfilter;
		    vector<double> prox;
		    for(int i=max(isave-2*(nfilter-1), 0); i<isave; i++){
			    hfilter.push_back( _heliostats.at(i) );
			    prox.push_back( hfilter.back()->getRadialPos() );
		    }
		    quicksort(prox, hfilter, 0, (int)prox.size()-1);
		    //swap out the heliostats in the main vector with the closer ones.
		    double q_replace_new=0.;
		    int ireplace = isave-1;
		    int nhf = (int)hfilter.size();
		    int irct=0;
		    while(q_replace_new < q_replace && irct < nhf){
			    _heliostats.at(ireplace) = hfilter.at(irct);
			    q_replace_new += _heliostats.at(ireplace)->getPowerToReceiver();
			    ireplace--;
			    irct++;
		    }
		    //update the isave value
		    isave = ireplace+1;
	    }

	    //Delete all of the entries up to isave-1.
	    _heliostats.erase(_heliostats.begin(), _heliostats.begin()+isave);

        //Remove any heliostat in the layout that does not deliver any power to the receiver
        isave = 0;
        for(size_t i=0; i<_heliostats.size(); i++){
            if(_heliostats.at(i)->getPowerToReceiver() > 0.) {
                isave = (int)i;
                break;
            }
        }
        if(isave > 0)
            _heliostats.erase(_heliostats.begin(), _heliostats.begin()+isave);
	    Npos = (int)_heliostats.size();
    }       //end needs_processing


	//Save the heliostats in an array by ID#
	_helio_by_id.clear();
	for(int i=0; i<Npos; i++){
		_helio_by_id[ _heliostats.at(i)->getId() ] = _heliostats.at(i);
	}

    _sf_area = 0.;  //calculate total reflector area
    //Mark these heliostats as contained in the final layout and recalculate the field extents
	_helio_extents[0] = -9e9; //xmax
	_helio_extents[1] = 9e9;	//xmin
	_helio_extents[2] = -9e9;	//ymax
	_helio_extents[3] = 9e9;	//ymin
	for(int i=0; i<Npos; i++){ 
		_heliostats.at(i)->setInLayout(true);	//Set as in the layout
        _sf_area += _heliostats.at(i)->getArea();
		//Refactor the extents
		sp_point *loc = _heliostats.at(i)->getLocation();
		if(loc->x > _helio_extents[0]) _helio_extents[0] = loc->x;
		if(loc->x < _helio_extents[1]) _helio_extents[1] = loc->x;
		if(loc->y > _helio_extents[2]) _helio_extents[2] = loc->y;
		if(loc->y < _helio_extents[3]) _helio_extents[3] = loc->y;
	}
	//Limit the extents to always include the plot origin
	if(_helio_extents[0] < 0.) _helio_extents[0] = 0.;
	if(_helio_extents[1] > 0.) _helio_extents[1] = 0.;
	if(_helio_extents[2] < 0.) _helio_extents[2] = 0.;
	if(_helio_extents[3] > 0.) _helio_extents[3] = 0.;
	
	//Create an estimate of the annual energy output based on the filtered heliostat list
	_estimated_annual_power = 0.;

    if(needs_processing)
    {
	    for(int i=0; i<Npos; i++){
            int hid = _heliostats.at(i)->getId();
	        for(int j=0; j<nresults; j++){
			    _estimated_annual_power += results->at(j).data_by_helio[ hid ].getDataByIndex( helio_perf_data::PERF_VALUES::POWER_VALUE );  //this include receiver efficiency penalty
		    }
	    }
    }
    else
    {
        _estimated_annual_power = _q_to_rec;
    }

	UpdateLayoutAfterChange();

	return;
}

void SolarField::UpdateLayoutAfterChange()
{
	/*
	This method is called upon completion of the layout, and ties up loose ends with:
	- making sure area calculations are complete
	- updating the layout information stored in the variable map
	- updating the layout information stored in the _layout class member
	- calling the method to update all solar field calculated variables
	*/

	//update calculated heliostat area
	calcHeliostatArea();

	//update the layout positions in the land area calculation
	std::vector<sp_point> lpos; 
	lpos.reserve( _heliostats.size() );

	for(int i=0; i<(int)_heliostats.size(); i++)
	{
		if( _heliostats.at(i)->IsInLayout() ) //only include heliostats that are in the layout
			lpos.push_back( *_heliostats.at(i)->getLocation() );
	}
	
	_land.calcLandArea(_var_map->land, lpos );
    
	//update the layout data
    interop::UpdateMapLayoutData(*_var_map, &_heliostats);

    //update the layout shell 
    _layout.clear();
    _layout.reserve( _heliostats.size() );
    for(int i=0; i<(int)_heliostats.size(); i++)
    {
        layout_obj lo;
        Heliostat* H = _heliostats.at(i);
        lo.location = *H->getLocation();
        lo.aim = *H->getAimPoint();
        lo.cant = *H->getCantVector();
        lo.focal_x = H->getFocalX();
        lo.focal_y = H->getFocalY();
        lo.is_user_aim = false;
        lo.is_user_cant = H->IsUserCant();
        lo.is_user_focus = false;
		lo.is_enabled = H->IsEnabled();
		lo.is_in_layout = H->IsInLayout();

        _layout.push_back( lo );
    }
    //update costs
    updateAllCalculatedParameters(*_var_map);

	return;
}

void SolarField::AnnualEfficiencySimulation( SolarField &SF, sim_results &results){ 
	string wf = SF.getVarMap()->amb.weather_file.val;
	
	SolarField::AnnualEfficiencySimulation( wf, &SF, results); 
}

void SolarField::AnnualEfficiencySimulation( string weather_file, SolarField *SF, sim_results &results) //, double *azs, double *zens, double *met)	//overload
{
	/* 
	Take the simulations provided in the "results" array and create an annual simulation from the
	specified weather file. Each heliostat is evaluated based on the efficiency values provided
	in the results along with the associated solar position.
	*/
	//Create arrays of the available sun positions
	int nresult = (int)results.size();
	vector<double>
		solaz(nresult),
		solzen(nresult);
	for(int i=0; i<nresult; i++){
		solaz.at(i) = results.at(i).solar_az;
		solzen.at(i) = results.at(i).solar_zen;
	}

    var_map *V = SF->getVarMap();

	Ambient::readWeatherFile( *V ); //wdannual, weather_file, amb);
	
	int rindex = V->sf.hsort_method.mapval(); 
	int Npos = (int)SF->getHeliostats()->size();
	//Clear the existing ranking metric value from the heliostats
	Hvector *helios = SF->getHeliostats();
	for(int i=0; i<Npos; i++){
		helios->at(i)->setRankingMetricValue(0.);
	}

	vector<double> pfs = V->fin.pmt_factors.val; 
	bool is_pmt_factors = V->fin.is_pmt_factors.val; 
	//Get design point DNI for some simulations
	double dni_des = V->sf.dni_des.val; 

	unordered_map<int, helio_perf_data> *resmap[3];

    WeatherData *wdannual = &V->amb.wf_data.val;
	//Simulate each step
	int nsim = wdannual->size();
	double nsimd = 1./(double)nsim;
	unordered_map<int, double> rank_temp;
	//initialize temp rank value array
	for(int j=0; j<Npos; j++)
		rank_temp[helios->at(j)->getId()] = 0.;

	simulation_info *siminfo = SF->getSimInfoObject();
	siminfo->setTotalSimulationCount(nsim);
	siminfo->setCurrentSimulation(0);
	
    if(! siminfo->addSimulationNotice("Simulating hourly weather profile...") ){
        SF->CancelSimulation();
        return;
    }
	
    DateTime DT;
	for(int i=0; i<nsim; i++){

		//Calculate the solar position based on the time step
		//Convert the day of the month to a day of year
		int month = (int)wdannual->Month.at(i);
		int mday = (int)wdannual->Day.at(i);
		int doy = DateTime::CalculateDayOfYear(2011, month, mday);
		double hour = wdannual->Hour.at(i)+0.5;
		//Calculate the sun position
		Ambient::setDateTime(DT, hour, (double)doy);
		//latitude, longitude, and elevation should be set in the input file
		double az, zen;
		Ambient::calcSunPosition(*V, DT, &az, &zen); //no time correction because 'hour' is adjusted above

		double dni = wdannual->DNI.at(i);
				
		//If the sun is not above the horizon, don't process
		if( zen > PI*0.5) continue;
		
		//Find the 3 efficiency values closest to the current sun position
		int jsave[3]={0,0,0};
		double rdiff[3] = {9.e9, 9.e9, 9.e9};
		for(int j=0; j<nresult; j++){
			double rdiff2 = sqrt( std::pow(az-solaz.at(j),2) + std::pow(zen-solzen.at(j),2) );
			int k=3; 
			while( rdiff2 < rdiff[k-1] && k>0) k+= -1;	//rdiff is in order of closest to farthest. find where the new point should go
			//If a position is found within the array...
			if(k<3){
				//shift values in the arrays
				for(int kk=2; kk>k; kk+= -1){
					if(kk>0){
						rdiff[kk] = rdiff[kk-1];
						jsave[kk] = jsave[kk-1];
					}
				}
				//insert new value
				rdiff[k] = rdiff2;
				jsave[k] = j;
			}
		}
		
		//Calculate the XYZ plane based on cross-product of the two vectors. Normal vector components are multipliers on plane fit.
		Vect t1, t2, cp;
		for(int j=0; j<3; j++)
			resmap[j] = &results.at(jsave[j]).data_by_helio;
		double rm_az[3], rm_zen[3];
		for(int j=0; j<3; j++){
			rm_az[j] = results.at(jsave[j]).solar_az;
			rm_zen[j] = results.at(jsave[j]).solar_zen;
		}
		
		for(unordered_map<int, helio_perf_data>::iterator it = resmap[0]->begin(); it != resmap[0]->end(); it++){ //For each heliostat
			int hid = it->first;
			double zdat[3];
			for(int j=0; j<3; j++)
				zdat[j] = (*resmap[j])[hid].getDataByIndex( rindex );

			t1.Set( rm_az[1] - rm_az[0], rm_zen[1] - rm_zen[0], zdat[1] - zdat[0]); 
			t2.Set( rm_az[2] - rm_az[0], rm_zen[2] - rm_zen[0], zdat[2] - zdat[0]);
			cp = Toolbox::crossprod(t1, t2);

			if(cp.k < 0.){
				cp.i = - cp.i;
				cp.j = - cp.j; 
				cp.k = - cp.k;
			}

			//find min and max
			double zmin = 9.e9, zmax = -9.e9;
			for(int j=0; j<3; j++){
				if(zdat[j] < zmin) zmin = zdat[j];
				if(zdat[j] > zmax) zmax = zdat[j];
			}
						
			//Now based on the location of the point in az,el coordinates, we can calculate an interpolated efficiency
			double z_interp = zdat[0] - (cp.i*(az-rm_az[0]) + cp.j*(zen-rm_zen[0]))/cp.k;

			if(z_interp < zmin) z_interp = zmin;
			if(z_interp > zmax) z_interp = zmax;


			//Calculate the ranking metric.
			double payfactor = 1., zval ;
			switch(rindex)
			{
			case helio_perf_data::PERF_VALUES::POWER_VALUE:
				if(is_pmt_factors)
				  {
				    vector<int> *TOD = SF->getFinancialObject()->getScheduleArray();
				    payfactor = pfs[TOD->at(i)-1];
				  }
			case helio_perf_data::PERF_VALUES::POWER_TO_REC:
				zval = dni/dni_des * payfactor * z_interp * nsimd;
				break;
			default:
				zval = z_interp * nsimd;
			}
			rank_temp[hid] += zval;
			
		}
		
		if(i%200==0){
            if(! siminfo->setCurrentSimulation(i) ) break;
        }
	}
	siminfo->setCurrentSimulation(0);
	//Clear the results array and set up a dummy result that can be used to sort the field
	if(nsim > 1)
		results.erase( results.begin() + 1, results.end());
	for(unordered_map<int,double>::iterator it = rank_temp.begin(); it != rank_temp.end(); it++)
		results.begin()->data_by_helio[it->first].setDataByIndex( rindex, it->second );
		

}

Heliostat *SolarField::whichTemplate(int method, sp_point &pos){
	/*
	This function takes as arguments an integer indicating the method for determining which heliostat 
	template to use (method) and the current point {x,y,z} location of the heliostat. The method
	uses information attributes of the current SolarField object to determine which heliostat template
	is appropriate.

	--Methods description--
	0 = Use single template
	1 = Specified range
	2 = Even radial distribution	

		The heliostat field is broken into equidistant sections depending on the number
		of heliostat templates provided. Heliostats are assigned to templates depending
		on which section they fall into. 

	*/

	int Nht = (int)_helio_templates.size();
    //count the number of enabled templates
    int Nht_active = 0;
    for(int i=0; i<(int)_helio_templates.size(); i++)
        if( _helio_templates.at(i)->IsEnabled() )
            Nht_active++;

	//get the field limits from the land class
	double rad[2];
	
	double 
		rpos = sqrt(pow(pos.x,2) + pow(pos.y,2)), // /_var_map->sf.tht.val, 
		azpos = atan2(pos.x, pos.y);

	_land.getExtents(*_var_map, rad);
	double
		radmin = rad[0],
		radmax = rad[1];
	
	switch(method)
    {
    //case SolarField::TEMPLATE_RULE::SINGLE:
    case var_solarfield::TEMPLATE_RULE::USE_SINGLE_TEMPLATE:
        //Use single template
        return _helio_templates.find( _var_map->sf.temp_which.mapval() )->second;
        /*for(int i=0; i<(int)_helio_templates.size(); i++)
        {
            if( _temp_which == _helio_templates.at(i)->getId() )
                return _helio_templates.at(i);
        }*/
    //case SolarField::TEMPLATE_RULE::SPEC_RANGE:
    case var_solarfield::TEMPLATE_RULE::SPECIFIED_RANGE:
 	{
		double tradmax, tradmin, tazmax, tazmin;
		for(int i=0; i<Nht; i++)
        {
            var_heliostat *Hv = _helio_templates.at(i)->getVarMap();
            tradmin = Hv->temp_rad_min.val;
            tradmax = Hv->temp_rad_max.val;
            tazmin = Hv->temp_az_min.val * D2R;
            tazmax = Hv->temp_az_max.val * D2R;

			if(rpos >= tradmin && rpos < tradmax && azpos >= tazmin && azpos < tazmax)
				return _helio_templates.at(i);
		}
		//none caught.. return the first template
		return _helio_templates.at(0);
		break;
	}
    //case SolarField::TEMPLATE_RULE::EVEN_DIST:
    case var_solarfield::TEMPLATE_RULE::EVEN_RADIAL_DISTRIBUTION:
	{
        //calculate which template is being used by comparing the current position to the radial range of each template.
		int ht = int(floor((rpos - radmin)/( (radmax+0.0001-radmin)/double(Nht_active) )));
        int ht_ct = -1;     //Counter for number of enabled templates
        int ht_save = 0;    //Save the value of the applicable template
        for(int i=0; i<Nht; i++)
        {
            if( _helio_templates.at(i)->IsEnabled() )
                ht_ct++;    //only count enabled templates
            if( ht_ct == ht )
            {               //once the template count matches the calculated number, save and move on
                ht_save = i;
                break;
            }
        }

		return _helio_templates.at(ht_save);
	}
    default:
        break;
    }

    throw spexception("An error occurred while calculating heliostat template placement. Please contact support for debugging help.");

}

void SolarField::TemplateRange(int pos_order, int method, double *rrange, double *arange){
	/* 
	This function is the inverse of 'SolarField::whichTemplate'. Provide a heliostat template
	index within the larger set and a method for dividing the field, and this calculates the 
	valid range of the template.

	>> pos_order = the position of this template in the field. Lower is nearer the receiver.

	<< rrange[2] . Sets the values of len=2 array : {rmin,rmax}
	
	--Methods description--
	0 = Use single template
	1 = Specified range
	2 = Even radial distribution	
	*/

	int Nht = (int)_helio_templates.size();
	//get the field limits from the land class
	double rad[2];
	_land.getExtents(*_var_map, rad);
	double
		radmin = rad[0],
		radmax = rad[1];
	double drad;
	switch(method)
	{
	case 0:	//Use single template
		rrange[0] = radmin;
		rrange[1] = radmax;
		arange[0] = -PI;
		arange[1] = PI;
		return;
		break;
	case 1:	//Specified range
    {
        var_heliostat *Hv = _helio_templates.at(pos_order)->getVarMap();

		rrange[0] = Hv->temp_rad_min.val;
		rrange[1] = Hv->temp_rad_max.val;
		arange[0] = Hv->temp_az_min.val *D2R;
		arange[1] = Hv->temp_az_max.val *D2R;
		return;
		break;
    }
	case 2:	//equal spatial separation
		drad = (radmax - radmin)/float(Nht);
		rrange[0] = radmin + pos_order*drad;
		rrange[1] = rrange[0] + drad;
		arange[0] = -PI;
		arange[1] = PI;
		return;
		break;
	default:
		rrange[0] = radmin;
		rrange[1] = radmax;
		arange[0] = -PI;
		arange[1] = PI;
		
	}


}

void SolarField::radialStaggerPositions(vector<sp_point> &HelPos)
{
	/*
	Calculate the possible heliostat positions in the solar field, given certain minimum/maximum extent
	requirements, heliostat geometry, and tower height. 

	This function requires that the following SolarField attributes be defined:
	-> radmin
	-> radmax
	-> q_des
	-> tht
	-> helio_templates
	-> land (if land filter is used)
	-> spacing_reset
	
	Note that DELSOL uses a variable called "FSLIP" to remove heliostats from the first row when spacing
	is reset. This is based on an empirical relationship that defines the average spacing of a radial "zone"
	and moves inward until heliostat must be removed. This convention is not adopted here. Instead, the
	spacing is determined either by optimization or a user-specified initial spacing factor, and spacing 
	is reset to the initial ratio when the current row azimuth spacing divided by the compressed row spacing 
	exceeds the value "_spacing_reset".

	Radial spacing can be calculated as follows:
	-> Using an empirical relationship from DELSOL
	-> To eliminate blocking but not shading
	-> Optimized? (not yet implemented)

	The heliostat templates will be used according to the following rules:
	-> For 'N' templates in a 1-D array, the field will be broken into N radial sections from radmin to radmax.
	   Each template will be used in its corresponding radial section, with the first template corresponding
	   to the first radial group.
	*/

	//any declarations
	int i,j;
	int N_max;   //Upper estimate for the number of heliostats in the field, sizes the arrays

	//Calculate limits in meters
	//get the field limits from the land class
	double rad[2];
	_land.getExtents(*_var_map, rad);
	double
		radmint = rad[0],
		radmaxt = rad[1];
	
	//Calculate an upper estimate of the size of the heliostat positions array to avoid resizing all the time
    {   //ensure local scope for these temporary variables
	    double r_coll_temp;
	    double r_coll_min = 9.e9; 
	    for(htemp_map::iterator it=_helio_templates.begin(); it != _helio_templates.end(); it++)
        {
		    r_coll_temp = it->second->getCollisionRadius();
		    if(r_coll_temp < r_coll_min) r_coll_min = r_coll_temp; //minimum collision radius in any combination
	    }
	    int nr_max = int((radmaxt - radmint)/(r_coll_min*2.)); 
	    int naz_max = int((radmaxt + radmint)/2.*(_var_map->sf.accept_max.val - _var_map->sf.accept_min.val)*D2R/(r_coll_min*2.));
	    N_max = nr_max * naz_max;  //Estimate the array size
    }

	HelPos.reserve(N_max);

    //choose which (initial) template to use to lay out the positions
    Heliostat *Htemp=0;
    var_heliostat *Htv;
    switch (_var_map->sf.template_rule.mapval())
    {
    //case SolarField::TEMPLATE_RULE::SINGLE:
    case var_solarfield::TEMPLATE_RULE::USE_SINGLE_TEMPLATE:
        Htemp = _helio_templates.find( _var_map->sf.temp_which.mapval() )->second;
        break;
    //case SolarField::TEMPLATE_RULE::SPEC_RANGE:
    //case SolarField::TEMPLATE_RULE::EVEN_DIST:
    case var_solarfield::TEMPLATE_RULE::SPECIFIED_RANGE:
    case var_solarfield::TEMPLATE_RULE::EVEN_RADIAL_DISTRIBUTION:
        // Use the first enabled template in the list
        for(int i=0; i<(int)_helio_templates.size(); i++)
        {
            if( _helio_templates.at(i)->IsEnabled() )
            {
                Htemp = _helio_templates.at(i);
                break;
            }
        }
        break;
    default:
        throw spexception("An invalid heliostat template rule was specified. Please contact support for debugging help.");
    }

    Htv = Htemp->getVarMap();

	//how to calculate radial spacing of the rows?
	if(_var_map->sf.rad_spacing_method.mapval() == var_solarfield::RAD_SPACING_METHOD::DELSOL_EMPIRICAL_FIT)	//use the empirical relationship from delsol
    {
		/* 
		Documentation on these methods is from Kistler (1986), pages 39-41.
		DELSOL3 code lines 1223-1286.

		There are separate relationships depending on surround/cavity receiver, round/rectangular heliostats.

		For this method, only 1 heliostat template can be used.
		*/

		//Check to see if only 1 heliostat template is used.


		/*----------Calculate the row positions----------*/

		int nr = 1; //row counter
		double r_c = radmint; //current row position
		bool is_slip = true;    //initialize

		//Calculate the pointing angle for the first row
		double phi_0 = atan(_var_map->sf.tht.val/r_c);	//Elevation angle from the heliostats in the first row to the receiver
		double tan_phi_0 = _var_map->sf.tht.val/r_c;
		double r_reset = r_c; //Hold on to the radius where the spacing has reset

        //Calculate azimuthal spacing variables
		double daz_init;	//The initial physical spacing between heliostats azimuthally
		double az_ang, azmin, azmid, haz, dr_c;
        int Nhelio = 0;
		
		azmid = (_var_map->sf.accept_max.val + _var_map->sf.accept_min.val)/2.;	//[rad] The midpoint of the acceptance window
        int hpr=-1;  //heliostats per row are calculated after slip planes

		while(r_c < radmaxt){
			/* 
			Calculations in this loop use the current radial position value (r_c), the
			flag for whether the row is a slip plane (is_slip), and the elevation angle 
			phi_0 that is consistent with r_c. These should be initialized and are updated 
			at the end of the loop.
			*/

            //Choose the heliostat template based on the current row position, updating only after slip planes
            if(is_slip)
            {
                sp_point cpos;
                cpos.Set( 0., r_c, 0. );
                Htemp = whichTemplate(_var_map->sf.template_rule.mapval(), cpos );
                Htv = Htemp->getVarMap();
            }

            bool is_round = Htv->is_round.mapval() == var_heliostat::IS_ROUND::ROUND;

		    //Get the minimum separation between heliostats that ensures no collision
		    double r_coll = Htemp->getCollisionRadius();    //Collision radius for current template
		    double hw = Htv->width.val;

			//The radial separation formula is the same for both round and rectangular heliostats
			double rsep = 1.1442399/tan_phi_0-1.093519+3.0683558*phi_0-1.1255617*pow(phi_0,2);
			double asep;

			//Calculate azimuthal separation
			if(is_round){
				//Round
				asep = 1.609666+0.29654848*phi_0+0.019137019/(phi_0-0.012341664);
				asep *= 1./(1.-rsep*hw/(_var_map->sf.tht.val*2. * r_c));	//D1275 is always used.
				rsep *= .5;	//Correct for the half-size convention used
				dr_c = rsep*Htv->width.val;
			}
			else{
				//Rectangular
				asep = (1.7490871+0.63964099*phi_0+0.028726279/(phi_0-0.049023315));
				asep *= 1./(1.-rsep*hw/(_var_map->sf.tht.val*2. * r_c));	//D1275 is always used.
				rsep *= .5;	//Correct for the half-size convention used
				dr_c = rsep*Htv->height.val;
			}
			
			//----Now add heliostats to each row----
			//If the collision radius limit is exceeded, remove heliostats. 
			//daz_init = Azimuthal straight-line distance
			daz_init = hw*asep;	//Normal spacing
			int col_factor = r_coll*2. > hw*asep ? 2 : 1; 
			
			//Should this row reset the spacing? 
			if( is_slip )
            {
				az_ang = 2.*atan2(daz_init, 2.*r_c);	//The angular spacing of the heliostats in the row
				
				//How many heliostats are in this row?
				hpr = int(floor(fmin((_var_map->sf.accept_max.val - _var_map->sf.accept_min.val), 2.*PI)/az_ang));
						
				//-----calculate the position of each heliostat in the row.-----
				//Go min to max around the arc. 
				//Every other row, a heliostat should be at the bisection angle of the acceptance window.
				//In the alternate rows, two heliostats will be evenly centered about the bisecting angle.
		
				//Calculate the starting minimum azimuth angle
				azmin = azmid - az_ang*hpr/2.;
			}

            if( hpr < 0 )
                throw spexception("An algorithmic error occurred during heliostat placement. Please contact support for debugging help.");

			j=0;
			while(j<hpr){	//For the heliostats in the row
				haz = azmin + az_ang*double(j) + az_ang/2.*double(nr%2);	//heliostat azimuth angle
				HelPos.push_back(sp_point());
				HelPos.at(Nhelio).x = r_c*sin(haz);
				HelPos.at(Nhelio).y = r_c*cos(haz);
				HelPos.at(Nhelio).z = 0.;
				Nhelio++;
				j += col_factor;	//If we're skipping heliostats, col_factor will increment by 2
			}

			//Increment to the next row, but make sure there's enough room to avoid collision
			r_c += max(dr_c, r_coll*2.);

			//Is the next row a slip plane?
			if(r_c/r_reset > _var_map->sf.spacing_reset.val){
				is_slip = true;
				r_reset = r_c;
			}
			else{
				is_slip = false;
			}

			nr++;	//Row number, starts at 1
			//now with the solved radius, prepare for the next radius
			phi_0 = atan(_var_map->sf.tht.val/r_c);
			tan_phi_0 = _var_map->sf.tht.val/r_c;
		}


	}
	else if( 
                (_var_map->sf.rad_spacing_method.mapval() == var_solarfield::RAD_SPACING_METHOD::NO_BLOCKINGDENSE 
              || _var_map->sf.rad_spacing_method.mapval() == var_solarfield::RAD_SPACING_METHOD::ELIMINATE_BLOCKING)
            && Htv->is_round.mapval() == var_heliostat::IS_ROUND::ROUND){
		/* 
		Space using radial stagger with the row position chosen for "close packing"
		from the perspective of the receiver.
		*/

		int nr=1;	//row counter
		double r_c = radmint; //Initialize
		bool is_slip = true;


		//For round heliostats, all heliostats must be round (no multiple templates. Macro-level geometry
		//will use the first heliostat (excludes specific canting, aiming, focusing etc.).

		double phi_0 = atan(_var_map->sf.tht.val/r_c);	//elevation angle of the heliostats in the first row

		//Calculate azimuthal spacing variables
		double daz_init;	//The initial physical spacing between heliostats azimuthally
		double az_ang, azmin, azmid, haz, dr_c;
		int Nhelio = 0;
		
		azmid = (_var_map->sf.accept_max.val + _var_map->sf.accept_min.val)/2.;	//[rad] The midpoint of the acceptance window

		//Keep track of the row positions
		vector<double> rowpos; 
        rowpos.push_back(r_c);
		//keep track of whether each row was a slip plane
		vector<bool> slips; 
        slips.push_back(true);


		while(r_c < radmaxt){

            //Choose the heliostat template based on the current row position, updating only after slip planes
            if(is_slip)
            {
                sp_point cpos;
                cpos.Set( 0., r_c, 0. );
                Htemp = whichTemplate(_var_map->sf.template_rule.mapval(), cpos );
                Htv = Htemp->getVarMap();
            }

            double Hd = Htv->width.val;	//Heliostat diameter
		    //double Hrad = Hd/2.;

			//----Add heliostats to this row----
			
			//Should this row reset the spacing? 
            int hpr=-1;  //heliostats per row are calculated after slip planes
			if( is_slip ){
				//daz_init = Azimuthal straight-line distance
				daz_init = Hd*max(1.,_var_map->sf.az_spacing.val);	//Normal spacing
				
				az_ang = 2.*asin(daz_init*.5/r_c);	//The angular spacing of the heliostats in the row
				
				//How many heliostats are in this row?
				hpr = int(floor(fmin((_var_map->sf.accept_max.val - _var_map->sf.accept_min.val), 2.*PI)/az_ang));
						
				//-----calculate the position of each heliostat in the row.-----
				//Go min to max around the arc. 
				//Every other row, a heliostat should be at the bisection angle of the acceptance window.
				//In the alternate rows, two heliostats will be evenly centered about the bisecting angle.
		
				//Calculate the starting minimum azimuth angle
				azmin = azmid - az_ang*hpr/2.;
			}
            if( hpr < 0 )
                throw spexception("An algorithmic error occurred during heliostat placement. Please contact support for debugging help.");

			j=0;
			while(j<hpr){	//For the heliostats in the row
				haz = azmin + az_ang*double(j) + az_ang/2.*double(nr%2);	//heliostat azimuth angle
				HelPos.push_back(sp_point());
				HelPos.at(Nhelio).x = r_c*sin(haz);
				HelPos.at(Nhelio).y = r_c*cos(haz);
				HelPos.at(Nhelio).z = 0.;
				Nhelio++;
				j ++;	//If we're skipping heliostats, col_factor will increment by 2
			}

			//--- Calculate the position of the next row ---
			dr_c = sqrt( pow(Hd,2) - pow(r_c*sin(az_ang/2.),2) )/sin(phi_0);


			//Increment to the next row
			r_c += dr_c;

			//Calculate the spacing between the current row and the previous 2 rows that
			//would result in blocking
			is_slip = false;
			if(nr>2){
				if(! slips.at(nr-2)){
					//If the last row was a slip plane, don't do these calculations
					double drlim = Hd/sin(phi_0);
					if( (r_c - rowpos.at(nr-3)) < drlim || dr_c < Hd/2.){
						is_slip = true;
						r_c += -dr_c + max(drlim, Hd);
					}
				}
			}

			nr++;	//Row number, starts at 1
			//now with the solved radius, prepare for the next radius
			phi_0 = atan(_var_map->sf.tht.val/r_c);
			
			slips.push_back(is_slip);
			rowpos.push_back(r_c);
		}

	}
	else if( 
              (
                 _var_map->sf.rad_spacing_method.mapval() == var_solarfield::RAD_SPACING_METHOD::ELIMINATE_BLOCKING 
              || _var_map->sf.rad_spacing_method.mapval() == var_solarfield::RAD_SPACING_METHOD::NO_BLOCKINGDENSE
              )
            && Htv->is_round.mapval() != var_heliostat::IS_ROUND::ROUND){	//Space to eliminate blocking - rectangular heliostats
		//***calculate the row positions***
		
		int nr=1;	//row counter
		double r_0 = radmint; 
        double r_c = r_0;	//initialize
		
        vector<double> rowpos; 
        vector<bool> slips;
        vector<bool> is_compact;

		rowpos.push_back(r_0);	//add the first row
		slips.push_back(true);
		
		//Calculate the minimum row separation distance, depends on default azimuthal spacing
		//..The azimuthal separation factor - the ratio of the actual separation azimuthally to the collision radius
		double H2 = Htv->height.val/2.;  //Heliostat half-height
		
        

		//calculate pointing angle for the first row
		double phi_0 = atan(_var_map->sf.tht.val/r_0);	//elevation angle of the heliostats in the first row
		
		double r_reset = r_c;	//Hold on to the radius where the spacing has reset
	    
        sp_point hloc;
		while(r_c < radmaxt){
			
            //get the new template
            hloc.Set(r_0,0.,0.);	//Radial position (y-component doesn't matter with this layout in terms of the template to use)
            
            Htemp = whichTemplate(_var_map->sf.template_rule.mapval(), hloc );
            Htv = Htemp->getVarMap();

            H2 = Htv->height.val/2.;  //Heliostat half-height
            double W2 = Htv->width.val/2.;   //heliostat half-width

            double r_coll = Htemp->getCollisionRadius();
            double fr = H2*2.*_var_map->sf.az_spacing.val/r_coll;
		    //double dr_min = 2.*sqrt( pow(2.*r_coll, 2) - pow(r_coll * fr/2.,2) );
            double dr_min = 2. * sqrt( 4 * r_coll * r_coll - pow(_var_map->sf.az_spacing.val*W2, 2) );

			//from pointing angle, calculate other needed info
            {
			    double z_0u = cos(phi_0)*H2;	//height of the upper heliostat edge
			    double r_0u = r_0 + sin(phi_0)*H2;	//Radial position of the upper heliostat edge

                //Calculate the next row position based on similar triangles between tower and upper/lower corners of adjacent heliostats.
                r_c = r_0u * (_var_map->sf.tht.val + z_0u) / (_var_map->sf.tht.val - z_0u) + sin(phi_0)*H2;
            }

            //Is this row in the inner compact region?
            bool row_compact = r_c - r_0 < 2.* r_coll *_var_map->sf.trans_limit_fact.val;
            if( _var_map->sf.rad_spacing_method.mapval() != var_solarfield::RAD_SPACING_METHOD::NO_BLOCKINGDENSE)
                row_compact = false;    //only allow compact layout if requested

            //Has the row compact flag just changed?
            bool row_compact_switch = false;
            
            //retroactively handle the first row
            if( is_compact.empty() ){
                if( row_compact )
                {
                    //The next row is compact, therefore the first must also be
                    is_compact.push_back( true );
                }
                else{
                    is_compact.push_back( false );
                }
            }
            else{
                if( is_compact.back() && !row_compact )
                    row_compact_switch = true;
            }
			
            //Increment to the next row, but make sure there's enough room to avoid collision
			r_c = r_0 + max(r_c - r_0, dr_min);
            
            bool is_slip = (r_c+r_0)/2./r_reset > _var_map->sf.spacing_reset.val || row_compact_switch;

			//check whether the next row is outside the max bounds
			if(r_c > radmaxt) break;

            //manage the next two rows. Call the template check for each subsequent row to ensure
            //the correct template is used for each position.
            sp_point hpos;
            hpos.Set( (r_c + r_0)/2., 0., 0. );
            Heliostat *Htemp_r1 = whichTemplate( _var_map->sf.template_rule.mapval(), hpos );
            hpos.Set( r_c, 0., 0. );
            Heliostat *Htemp_r2 = whichTemplate( _var_map->sf.template_rule.mapval(), hpos );

            //first handle the case where the intermediate row uses a different template
            if( Htemp != Htemp_r1 )
            {
                //the intermediate row causes a template switch. min spacing enforced
                nr++;
                H2 = Htemp_r1->getVarMap()->height.val/2.;  //Heliostat half-height

                //update the calculations
                r_coll = Htemp_r1->getCollisionRadius();
                fr = H2*2.*_var_map->sf.az_spacing.val/r_coll;
			    
                dr_min = sqrt( pow(2.*r_coll, 2) - pow(r_coll * fr/2.,2) ); //update calculation
                //r_c = r_0 + max( (r_c - r_0)/2., dr_min );
                r_c = r_0 + max( (r_c - r_0)/2., r_coll * 2. );
                rowpos.push_back( r_c );
                slips.push_back( true );
                is_compact.push_back( false );  //not sure how else to handle this scenario
                r_reset = r_c;

            }
            //next handle the case where the "current" row uses a different template, but the intermediate row does not
            else if( Htemp != Htemp_r2 )
            {
                //the current row causes a template switch. 
                nr++;	//next row
                double r_half = (r_c+r_0)/2.;
				rowpos.push_back(r_half);	//In radial stagger, there's a shifted row halfway between each aligned row
				slips.push_back(false);
				
                //regular spacing on current row, but maintain at least the collision radius
                r_c = max( r_c, r_half + Htemp_r2->getCollisionRadius() );
                rowpos.push_back(r_c);
				slips.push_back(true);
                is_compact.push_back( false );  //not sure how else to handle this scenario
				r_reset = r_c;
            }
            //Is this an inner compact row?
            else if( row_compact )
            {
                nr++;
                r_c = r_0 + r_coll * 2.;
                rowpos.push_back( r_c );
                slips.push_back(true);
                is_compact.push_back( true );
                r_reset = r_c;

            }
            //Check to see if we have a slip plane at the interediate row
			else if( is_slip )	//The intermediate row incurs a slip plane
            {
               
				if(! row_compact_switch){
                    if( !_var_map->sf.is_sliprow_skipped.val )
                    {
                        nr++; 
                    
                        //make sure the next 2 rows are at least the collision radius away
                        r_c = max(r_0 + 4.*r_coll, r_c);

				        //Make the row as close as possible and remove shadowed/blocked heliostats. 
				        //Multiply by the _slip_offset factor specified by the user
				        rowpos.push_back((r_c+r_0)/2.);
				        slips.push_back(true);
                        is_compact.push_back(false);
                    }
                    else
                    {
                        r_c = r_0 + (r_c - r_0)*(1.-_var_map->sf.slip_plane_blocking.val/2.);
                        r_c = max(r_0 + 2.*r_coll, r_c);
                    }
                }
                else
                {
                    r_c = max(r_0 + 2.*r_coll, r_c );
                }

				nr++;
                rowpos.push_back(r_c);
				slips.push_back(false || row_compact_switch || _var_map->sf.is_sliprow_skipped.val );
                is_compact.push_back(false);
				r_reset = r_c;
			}
            //The intermediate row isn't a slip plane, so add the intermediate row and then check the current row
			else{
                //add intermediate row
				nr++;	
				rowpos.push_back((r_c+r_0)/2.);	//In radial stagger, there's a shifted row halfway between each aligned row
				slips.push_back(false);
                is_compact.push_back(false);
                //determine whether current row is a slip plane
				if( r_c/r_reset > _var_map->sf.spacing_reset.val){	//Does the next inline row incurs a slip plane?
					nr++;
                    if( ! _var_map->sf.is_sliprow_skipped.val )
                    {
					    //Put this row as close as possible to the intermediate row. include _slip_offset factor
                        r_c = max(rowpos.back() + 2.*r_coll, r_c );
                    }
                    else
                    {
                        //from pointing angle, calculate other needed info
			            phi_0 = atan(_var_map->sf.tht.val/rowpos.back());
			            double z_0u = cos(phi_0)*H2*(1.-_var_map->sf.slip_plane_blocking.val);	//height of the upper heliostat edge
			            double r_0u = rowpos.back() + sin(phi_0)*H2;	//Radial position of the upper heliostat edge

                        //Calculate the next row position based on similar triangles between tower and upper/lower corners of adjacent heliostats.
                        r_c = r_0u * (_var_map->sf.tht.val + z_0u) / (_var_map->sf.tht.val - z_0u) + sin(phi_0)*H2;
                        r_c = max(rowpos.back() + 2.*r_coll, r_c);
                    }

                    rowpos.push_back( r_c );
					slips.push_back(true);
					r_reset = r_c;
				}
				else{
                    //current row is not a slip plane, just add as-is
					nr++;
					rowpos.push_back( r_c );
					slips.push_back(false);
				}
                is_compact.push_back(false);
			}
			//now with the solved radius, prepare for the next radius
			phi_0 = atan(_var_map->sf.tht.val/r_c);
			r_0 = r_c;			
		}
	
		//----Now add heliostats to each row----

		double daz_init;	//The initial physical spacing between heliostats azimuthally
		double az_ang, azmin, azmid, haz;
		int Nhelio = 0, hpr = -1;
        //initialize the heliostat template again
        {
            sp_point hpos;
            hpos.Set(0., rowpos.front(), 0. );
            Htemp = whichTemplate( _var_map->sf.template_rule.mapval(), hpos );
            Htv = Htemp->getVarMap();
        }
		double hw = Htv->width.val;
        double r_coll = Htemp->getCollisionRadius();

		r_reset = .001; //rowpos.at(0);	//Hold on to the radius where the spacing has reset
	
		azmid = (_var_map->sf.accept_max.val + _var_map->sf.accept_min.val)/2.;	//[rad] The midpoint of the acceptance window
		
		for(i=0; i<nr; i++){	//For each row
			double r_row = rowpos.at(i);	
			
            //Figure out which template to use
            if( slips.at(i) )
            {
                //update template-dependent values
                hloc.Set(r_row,0.,0.);
                Htemp = whichTemplate(_var_map->sf.template_rule.mapval(), hloc);
                Htv = Htemp->getVarMap();
			    hw = Htv->width.val;	//The heliostat width
                r_coll = Htemp->getCollisionRadius();
            }

            if( is_compact.at(i) ){
                daz_init = r_coll * 2.;     //Compact rows have minimum spacing
            }
            else{
			    daz_init = max(r_coll*2, hw*_var_map->sf.az_spacing.val);	//Azimuthal straight-line distance
            }
			//Should this row reset the spacing? It will reset if beyond the spacing ratio limit or a new heliostat width is used
			if( slips.at(i) ){
				az_ang = 2.*atan2(daz_init, 2.*r_row);	//The angular spacing of the heliostats in the row
		
				//How many heliostats are in this row?
				hpr = int(floor(fmin((_var_map->sf.accept_max.val - _var_map->sf.accept_min.val), 2.*PI)/az_ang));
						
				//-----calculate the position of each heliostat in the row.-----
				//Go min to max around the arc. 
				//Every other row, a heliostat should be at the bisection angle of the acceptance window.
				//In the alternate rows, two heliostats will be evenly centered about the bisecting angle.
		
				//Calculate the starting minimum azimuth angle
				azmin = azmid - az_ang*hpr/2.;
			}

			for(j=0; j<hpr; j++){	//For the heliostats in the row
				haz = azmin + az_ang*double(j) + az_ang/2.*double(i%2);	//heliostat azimuth angle
				HelPos.push_back(sp_point());
				HelPos.at(Nhelio).x = r_row*sin(haz);
				HelPos.at(Nhelio).y = r_row*cos(haz);
				HelPos.at(Nhelio).z = 0.;
				Nhelio++;
			}
		}
	}

	return;
}

void SolarField::cornfieldPositions(vector<sp_point> &HelPos){
	/* 
	Lay out the possible heliostat positions (HelPos) for heliostats arranged in a
	cornfield arrangement. In this configuration, the heliostats are positioned in 
	straight rows, with the positioning staggered between rows. 

	Information for this layout is contained in the following variables:

	_row_spacing_x		//Separation between adjacent heliostats in the X-direction, multiplies heliostat radius
	_row_spacing_y		//Separation between adjacent heliostats in the Y-direction, multiplies heliostat radius
	_xy_field_shape		//Enforced shape of the heliostat field
	_xy_rect_aspect		//Aspect ratio of the rectangular field layout (height in Y / width in X)

	*/

	//get the field limits from the land class
	double rad[2];
	_land.getExtents(*_var_map, rad);
	double
		radmint = rad[0],
		radmaxt = rad[1];

	//Calculate an upper estimate of the size of the heliostat positions array to avoid resizing all the time
	double 
		r_coll_temp = 0,
		r_coll_min = 9.e9; 
	for(htemp_map::iterator it=_helio_templates.begin(); it != _helio_templates.end(); it++){
		r_coll_temp = it->second->getCollisionRadius();
		if(r_coll_temp < r_coll_min) r_coll_min = r_coll_temp; //minimum collision radius in any combination
	}

	//Estimate the total number of heliostats to reserve in the HelPos array
	int N_max, nxmax, nymax;
	double Lx=0., Ly=0.;
	double 
		ry = 2.*r_coll_temp * _var_map->sf.row_spacing_y.val,
		rx = 2.*r_coll_temp * _var_map->sf.row_spacing_x.val;
	switch(_var_map->sf.xy_field_shape.mapval())
	{
	//case 0:	//Hexagon
    case var_solarfield::XY_FIELD_SHAPE::HEXAGON:
		/* 
		The maximum hexagon diameter is governed by the maximum radial value. 

		nx_bar = (radmax*sin(30)+radmax)/2*2
		--> nx_bar = radmax * 1.5 * rx
		ny = 2*radmax*cos(PI/6)/ry
		*/
		N_max = (int)ceil(pow(radmaxt-radmint, 2)*3.*cos(PI/6.)/(rx*ry));
		break;
	//case 1:	//Rectangle
	//case 2:	//Circular
    case var_solarfield::XY_FIELD_SHAPE::RECTANGLE:
    case var_solarfield::XY_FIELD_SHAPE::UNDEFINED:
	{
		/* 
		alpha = _xy_rect_aspect
		theta = atan(alpha)
		Lx = 2*sin(theta)*radmax
		Ly = alpha*Lx
		nx = Lx/rx
		ny = Ly/ry
		*/
		double hyp = sqrt( 1. + _var_map->sf.xy_rect_aspect.val*_var_map->sf.xy_rect_aspect.val );
		//Lx = 2.*sin(atan(1./_xy_rect_aspect))*radmaxt;
		Lx = 2.*radmaxt/hyp;
		Ly = _var_map->sf.xy_rect_aspect.val*Lx;
		nxmax = (int)ceil(Lx/rx);
		nymax = (int)ceil(Ly/ry);
		if(_var_map->sf.xy_field_shape.mapval() == var_solarfield::XY_FIELD_SHAPE::RECTANGLE){ //rectangle
			N_max = nxmax*nymax;
		}
		else{	//circular
			N_max = (int)ceil(PI * nxmax * nymax);
		}
		break;
	}
	default:
		_sim_error.addSimulationError("The specified field shape model does not exist",true);
		return;
	}

	//-- now we know the maximum number of heliostats possible in the field
	HelPos.reserve(N_max);



	/* -----------------  Calculate the positions ----------------*/
	double
		y_loc,	//Current row position in Y
		x_offset,	//Stagger offset in X
		x_max,	//maximum position in X
		//x_min,	//minimum x position for the current row
		x_loc,  //current x position
		y_max,
		hex_factor;	//Constant factor used to calculate x_max in hex's
	int 
		//ind_y, //Current row index in Y
		Nhelio;	//Number of heliostats, total


	
	//Initialize values
	x_offset = 999.;	//initialize to nonzero to force into 0 offset on first pass
	y_loc = 0.;
	hex_factor = 0.57735026919; //1./tan(PI/3.);
	Nhelio = 0;

	var_solarfield::XY_FIELD_SHAPE::EN shape =
	  static_cast<var_solarfield::XY_FIELD_SHAPE::EN>(_var_map->sf.xy_field_shape.mapval());
	assert (shape >= 0 && shape <= var_solarfield::XY_FIELD_SHAPE::UNDEFINED);
	switch (shape)
	{
    case var_solarfield::XY_FIELD_SHAPE::HEXAGON:
	//case 0:	//hex
		y_max = cos(PI/6.)*radmaxt;
		break;
    case var_solarfield::XY_FIELD_SHAPE::RECTANGLE:
    //case 1:	//rectangle
		y_max = Ly/2.;
		break;
    case var_solarfield::XY_FIELD_SHAPE::UNDEFINED:
	//case 2:	//circular
		y_max = radmaxt;
		break;
	}
	

	//loop through Y
	while(y_loc <= y_max)
	{
		//Set the x offset
		if(x_offset > 0.){
			x_offset = 0.;
		}
		else{
			x_offset = rx/2.;
		}

		//Calculate the maximum x position
		var_solarfield::XY_FIELD_SHAPE::EN shape =
		  static_cast<var_solarfield::XY_FIELD_SHAPE::EN>(_var_map->sf.xy_field_shape.mapval());
		assert (shape >= 0 && shape <= var_solarfield::XY_FIELD_SHAPE::UNDEFINED);
		switch (shape)
		{
        case var_solarfield::XY_FIELD_SHAPE::HEXAGON:
		//case 0:	//hex
			x_max = radmaxt - hex_factor*y_loc;
			break;
        case var_solarfield::XY_FIELD_SHAPE::RECTANGLE:
		//case 1:	//rect
			x_max = Lx/2.;
			break;
        case var_solarfield::XY_FIELD_SHAPE::UNDEFINED:
		//case 2:	//circular
			x_max = sqrt( pow(radmaxt, 2) - pow(y_loc, 2) );
			break;
		}

			
		//loop through each X position
		x_loc = x_offset;
		while(x_loc <= x_max){
			//check if the x location is within the exclusion area
			if( sqrt(pow(x_loc, 2) + pow(y_loc, 2)) >= radmint ){

				//Add the heliostat position
				HelPos.push_back(sp_point());
				HelPos.at(Nhelio++).Set(x_loc, y_loc, 0.);
				//Add the -y complement
				if(y_loc>0.){
					HelPos.push_back(sp_point());
					HelPos.at(Nhelio++).Set(x_loc, -y_loc, 0.);
				}
				//Add the x complement
				if(x_loc > 0.){
					HelPos.push_back(sp_point());
					HelPos.at(Nhelio++).Set(-x_loc, y_loc, 0.);
					if(y_loc>0.){
						HelPos.push_back(sp_point());
						HelPos.at(Nhelio++).Set(-x_loc, -y_loc, 0.);
					}
				}
			}
			//increment row X position
			x_loc += rx;
		}


		//increment row Y position
		y_loc += ry;

	}



}

void SolarField::RefactorHeliostatImages(Vect &Sun){
	/* 
	This method:
	1. Recalculates the analytical image properties for each heliostat in the field.
	2. Should only be called after the static Hermite terms have been initialized.
	3. Is a truncated form of the Simulate() call, using only the image intercept method.
	4. Does not set any efficiency properties of the simulated heliostats.
	5. Requires that the tracking vectors be previously updated.
	
	Call this method when an existing field geometry is simulated at a different solar 
	position or when aim points are updated.
	*/

	int nh = (int)_heliostats.size();
	for(int i=0; i<nh; i++){
		_flux->imagePlaneIntercept(*_var_map, *_heliostats.at(i), _heliostats.at(i)->getWhichReceiver(), &Sun);
	}

}

bool SolarField::SimulateTime(int /*hour*/, int day_of_month, int month, sim_params &P){
	/* 
	Simulate a particular date/time for the current solar field geometry.

	hour			|	Hour of the day [0,23)
	day_of_month	|	Day of the month [1,31]
	month			|	Month of the year [1,12]
	args[0]			|	DNI [W/m2]
	args[1]			|	Ambient temperature [C]
	args[2]			|	Atmospheric pressure [atm]
	args[3]			|	Wind velocity [m/s]

	*/

    //update DateTime structure with necessary info
    DateTime DT;
    DT.SetDate(2011, month, day_of_month);  //also updates day of year

	//Calculate the sun position
	double az, zen;
	Ambient::calcSunPosition(*_var_map, DT, &az, &zen);

    //If the sun is not above the horizon plus a very small amount (to avoid infinite shadows), don't continue
	if( zen > 88 )
			return false;
	
	//Simulate field performance
	//args[4] Should contain {dni [W/m2], tdb [C], wind [m/s], pres [mbar], weighting factor [-]};
	Simulate(az, zen, P);
	return true;
}


void SolarField::Simulate(double azimuth, double zenith, sim_params &P)
{
	/*
    azimuth [rad]
    zenith [rad]

	For a given heliostat field and tower/receiver geometry, simulate the performance of the field. Add the efficiency
	attributes to each heliostat. The method requires that:
    * The payment or weighting factors have been set
    * Solar field geometry has been defined

    The procedure is:
    1. Calculate sun vector
    2. Update receiver stuff
    3. Update tracking vectors
    4. Refactor heliostat images
    5. Calculate final aim points
    6. Calculate heliostat performance

	*/
	


    //calculate sun vector
    Vect Sun = Ambient::calcSunVectorFromAzZen(azimuth, zenith);

	for(int i=0; i<(int)_receivers.size(); i++)
    {
	    //Update the estimated receiver thermal efficiency for each receiver
		_receivers.at(i)->CalculateThermalEfficiency(P.dni, _var_map->sf.dni_des.val, P.Vwind, _var_map->sf.q_des.val);
        
        //reset receiver calculated values
    
        //reset the flux grids on all receivers
        for(int j=0; j<(int)_receivers.at(i)->getFluxSurfaces()->size(); j++)
        {
            FluxSurface *fs = &_receivers.at(i)->getFluxSurfaces()->at(j);
            fs->ClearFluxGrid();
            fs->setMaxObservedFlux(0.);
        }
    }

    setSimulatedPowerToReceiver(0.);	//reset the simulated power to the receiver
	
    //tracking
    bool psave = P.is_layout;
    P.is_layout = true; //override for simple right now
    calcAllAimPoints(Sun, P); //true, true);  //update with simple aim points first to get consistent tracking vectors
	updateAllTrackVectors(Sun);
    
    //Calculate aim points
    P.is_layout = psave;
    calcAllAimPoints(Sun, P); //.is_layout, P.is_layout);  // , simple? , quiet?
    
    //Update the heliostat neighbors to include possible shadowers
	UpdateNeighborList(_helio_extents, P.is_layout ? 0. : zenith);		//don't include shadowing effects in layout (zenith = 0.)
	
    //For each heliostat, assess the losses
	//for layout calculations, we can speed things up by only calculating the intercept factor for representative heliostats. (similar to DELSOL).
	int nh = (int)_heliostats.size();
	if(P.is_layout && _var_map->sf.is_opt_zoning.val){
		//The intercept factor is the most time consuming calculation. Simulate just a single heliostat in the 
		//neighboring group and apply it to all the rest.
		
		for(int i=0; i<(int)_layout_groups.size(); i++){
			
			Hvector *hg = &_layout_groups.at(i);

			int ngroup = (int)hg->size();

			if(ngroup == 0) continue;

			Heliostat *helios = hg->front(); // just use the first one
			double eta_int = _flux->imagePlaneIntercept(*_var_map, *helios, helios->getWhichReceiver(), &Sun);
			if( eta_int > 1.) eta_int = 1.;
			helios->setEfficiencyIntercept( fmin(eta_int, 1.) );

			for(int k=1; k<ngroup; k++){
				hg->at(k)->setEfficiencyIntercept( eta_int );
				hg->at(k)->CopyImageData( helios );
			}

			
		}
	}
	
	//Simulate efficiency for all heliostats
	for(int i=0; i<nh; i++)
		SimulateHeliostatEfficiency(this, Sun, _heliostats.at(i), P); 
	
	


}

void SolarField::SimulateHeliostatEfficiency(SolarField *SF, Vect &Sun, Heliostat *helios, sim_params &P)
{
	/*
	Simulate the heliostats in the specified range
	*/
	
    //if a heliostat has been disabled, handle here and return
    if( ! helios->IsEnabled() )
    {
        helios->setEfficiencyCosine( 0. );
        helios->setEfficiencyAtmAtten( 0. );
        helios->setEfficiencyIntercept( 0. );
        helios->setEfficiencyShading( 0. );
        helios->setEfficiencyBlocking( 0. );
        helios->setPowerToReceiver( 0. );
        helios->setPowerValue( 0. );
	    
        helios->calcTotalEfficiency();
        return;
    }

	//Cosine loss
	helios->setEfficiencyCosine( Toolbox::dotprod(Sun, *helios->getTrackVector()) );
	
    var_map *V = SF->getVarMap();

	//Attenuation loss
	double slant = helios->getSlantRange(),
	att = Ambient::calcAttenuation(*V, slant );
	helios->setEfficiencyAtmAtten( att );
	
	Receiver *Rec = helios->getWhichReceiver();

	//Intercept
	if(! (P.is_layout && V->sf.is_opt_zoning.val) ){	//For layout simulations, the simulation method that calls this method handles image intercept
		double eta_int = SF->getFluxObject()->imagePlaneIntercept(*V, *helios, Rec, &Sun);
        if(eta_int != eta_int)
            throw spexception("An error occurred when calculating heliostat intercept factor. Please contact support for help resolving this issue.");
		if(eta_int>1.) eta_int = 1.;
		helios->setEfficiencyIntercept(eta_int);
	}

	//Shadowing and blocking
	double
		shad_tot = 1.,
		block_tot = 1.;
		
    double interaction_limit = V->sf.interaction_limit.val;
	Hvector *neibs = helios->getNeighborList();
	int nn = (int)neibs->size();
	for(int j=0; j<nn; j++){
		if(helios == neibs->at(j) ) continue;	//Don't calculate blocking or shading for the same heliostat
		
        if(!P.is_layout) shad_tot += -SF->calcShadowBlock(helios, neibs->at(j), 0, Sun, interaction_limit);	//Don't calculate shadowing for layout simulations. Cascaded shadowing effects can skew the layout.
		
        block_tot += -SF->calcShadowBlock(helios, neibs->at(j), 1, Sun, interaction_limit);
	}
		
	if(shad_tot < 0.) shad_tot = 0.;
	if(shad_tot > 1.) shad_tot = 1.;
	helios->setEfficiencyShading(shad_tot);

	if(block_tot < 0.) block_tot = 0.;
	if(block_tot > 1.) block_tot = 1.;
	helios->setEfficiencyBlocking(block_tot);
	
	//Soiling, reflectivity, and receiver absorptance factors are included in the total calculation
	double eta_rec_abs = Rec->getVarMap()->absorptance.val; // * eta_rec_acc,
	double eta_total = helios->calcTotalEfficiency();
	double power = eta_total * P.dni * helios->getArea() * eta_rec_abs;
	helios->setPowerToReceiver( power );
	helios->setPowerValue( power * P.Simweight*P.TOUweight * Rec->getThermalEfficiency() );

	return;
	
}

double SolarField::calcShadowBlock(Heliostat *H, Heliostat *HI, int mode, Vect &Sun, double interaction_limit)
{
	/*
	This method takes two heliostats and calculates the interfering or blocking of heliostat 
	"H" by neighbor "HI". 

	interfering is calculated in mode = 0
	Blocking is calculated in mode = 1

	The method assumes that the neighboring heliostats have approximately the same 
	orientation with respect to the sun (i.e. approximately the same tracking vector). This 
	simplifies the calculation.

	The method returns a double value equal to the fraction lost to interference.
	*/
#if 0
	if(HI->IsRound()){	//Remove this for now
		//			Round heliostats

		/* 
		The formula for the area of intersection of 2 circles of equal radius is:
		A = 2 R^2 acos[ d/(2R) ] - 1/2 d sqrt(4 R^2 - d^2)
		where:
			R = radius of the circle
			d = distance separating the centroid of the circles

		The shadowing efficiency is equal to (1 - A_intersect/A_heliostat)
		*/
		sp_point *HIloc, *Hloc;
		//Check to see if the two heliostats are far enough apart that there is no possibility
		//of interfering. This criteria will depend on the solar angle
		Vect *H_inter;
		if(mode == 0){
			//Get the sun vector as the interference direction
			H_inter = &Sun;
		}
		else{
			//Get the tower/receiver vector as the interference direction
			H_inter = H->getTowerVector();
		}

		double zen = acos(H_inter->k);	//The zenith angle for interference

		//Get the interfering heliostat tracking angles
		Vect 
			*HIt = HI->getTrackVector(),	//Interfering heliostat track vector
			*Ht = H->getTrackVector();	//Base heliostat track vector

		//Is the heliostat in a position to shadow/block?
		double Hd = HI->getVarMap()->width.val;	//Diameter
		
		//Get locations
		HIloc = HI->getLocation();
		Hloc = H->getLocation();
		
		//Create a vector pointing from the heliostat to the interfering neighbor
		Vect Hnn;
		Hnn.Set(HIloc->x - Hloc->x, HIloc->y - Hloc->y, HIloc->z - Hloc->z);

		//If the heliostat is not in front of the other with respect to the solar position, it also can't shadow
		if( Toolbox::dotprod(Hnn, *H_inter) < 0.) return 0.; 

		//Find the collision point of the centroid of the shadow
		sp_point hit;
		if( Toolbox::plane_intersect(*Hloc, *Ht, *HIloc, *H_inter, hit) ) {
			//Calculate the distance separating the hit and the heliostat centroid
			Vect vsep;
			vsep.Set( Hloc->x - hit.x, Hloc->y - hit.y, Hloc->z - hit.z );
			double r_sep = Toolbox::vectmag( vsep );
			if(r_sep < Hd){
				//Calculate the shadow overlap area
				double Ax = 2.*pow(Hd/2.,2)*acos(r_sep/Hd) - .5*r_sep*sqrt(pow(Hd,2) - pow(r_sep,2));
				return Ax/(PI*.25*pow(Hd,2));
			}
		}
		return 0.;

		
	}
	else
#endif
    {
		//			rectangular heliostats
		
		sp_point *HIloc, *Hloc;
		//Check to see if the two heliostats are far enough apart that there is no possibility
		//of interfering. This criteria will depend on the solar angle
		Vect *H_inter;
		if(mode == 0){
			//Get the sun vector as the interference direction
			H_inter = &Sun;
		}
		else{
			//Get the tower/receiver vector as the interference direction
			H_inter = H->getTowerVector();
		}
		//double zen = acos(H_inter->k);	//The zenith angle for interference

		//Get the interfering heliostat tracking angles
		Vect 
			*HIt = HI->getTrackVector(),	//Interfering heliostat track vector
			*Ht = H->getTrackVector();	//Base heliostat track vector
		double HIh = HI->getVarMap()->height.val;
		//Interfering heliostat tracking zenith 
		double HIzen = acos(HIt->k);	//zenith angle 
		//double HIaz = atan2(HIt->i,HIt->j);	//azimuth angle

		/*
		The maximum possible extent between heliostats where interference is a possibility..
		Equals the distance in height between the top edge of the interfering heliostat and the bottom edge of the
		blocked/shadowed heliostat, plus any elevation difference between the heliostats, plus the horizontal distance 
		from tilting the heliostats.
		*/
		HIloc = HI->getLocation();
		Hloc = H->getLocation();
		//double l_max = (HIloc->z - Hloc->z + HIh*sin(HIzen))/tan(PI/2.-zen) + HIh*cos(HIzen);
		double tanpi2zen = H_inter->k/sqrt(H_inter->i*H_inter->i + H_inter->j*H_inter->j);
		double l_max = (HIloc->z - Hloc->z + HIh*sin(HIzen))/tanpi2zen + HIh*HIt->k;
		l_max = fmin(l_max, interaction_limit*HIh);	//limit to a reasonable number

		//Create a vector pointing from the heliostat to the interfering neighbor
		Vect Hnn;
		Hnn.Set(HIloc->x - Hloc->x, HIloc->y - Hloc->y, HIloc->z - Hloc->z);

		//How close are the heliostats?
		double hdist = sqrt(Hnn.i*Hnn.i + Hnn.j*Hnn.j + Hnn.k*Hnn.k);
		
		if(hdist > l_max) return 0.;	//No possibility of interfering, return here.
		//Check for collision radius
		double 
			Hh = H->getVarMap()->height.val,	//Shaded heliostat height
			Hw = H->getVarMap()->width.val;	//Shaded heliostat width
		//double Hr = sqrt(pow(Hh/2.,2) + pow(Hw/2.,2));
		//If the heliostat is not in front of the other with respect to the solar position, it also can't shadow
		if( Toolbox::dotprod(Hnn, *H_inter) < 0.) return 0.; 


		//-----------test
		vector<sp_point> 
			*cobj = HI->getCornerCoords(),
			ints(2);	//intersection points
		vector<bool> hits(2, false);	//track whether either point hit
		int i;
		for(i=0; i<2; i++){
			if( Toolbox::plane_intersect(*Hloc, *Ht, cobj->at(i), *H_inter, ints.at(i)) ){
				//An intercept on the plane was detected. Is the intercept within the heliostat area?
				hits.at(i) = Toolbox::pointInPolygon(*H->getCornerCoords(), ints.at(i) );
			}
		}
		//Do either of the corners shadow/block the heliostat?
		if(hits.at(0) || hits.at(1)){
			//interfering detected. 
			double dx_inter, dy_inter;
			
			/*
			To calculate the fraction of energy lost, we first transform the intersection point into heliostat coordinates
			so that it's easier to find how the shadow is cast on the heliostat.
			*/
			vector<sp_point> ints_trans(2);	//Copy of the intersection points for heliostat coordinate transform
			for(i=0; i<2; i++){
				//Express each point of HI in global coords relative to the centroid of H
				//i.e. (ints_trans.x - H->x, ... )
				ints_trans.at(i).Set(ints.at(i).x - Hloc->x, ints.at(i).y - Hloc->y, ints.at(i).z - Hloc->z);

				//First rotate azimuthally back to the north position
				Toolbox::rotation(-H->getAzimuthTrack(), 2, ints_trans.at(i));
				//Next rotate in zenith to get it into heliostat coords. The z component should be very small or zero. 
				//Y components should be relatively close for rectangular heliostats
				Toolbox::rotation(-H->getZenithTrack(), 0, ints_trans.at(i));
			}
			
			//Based on how the image appears, determine interfering. 
			//Recall that after transformation, the positive y edge corresponds to the bottom of the heliostat.
			int which_is_off, which_is_on;
			if(hits.at(0) && hits.at(1)) {
				//Both corners are active in interfering (i.e. the shadow image is contained within the shadowed heliostat
				dy_inter = ( Hh - (ints_trans.at(0).y + ints_trans.at(1).y) ) / (2. * Hh);	//Use the average z position of both points
				dx_inter = fabs(ints_trans.at(0).x - ints_trans.at(1).x ) / Hw;

				return dy_inter * dx_inter;
			}
			else if(hits.at(0)) { 
				//Only the first corner appears in the shadow/blocking image
				which_is_off = 1;
				which_is_on = 0;
			}
			else {
				//Only the second corner appears in the shadow/blocking image
				which_is_off = 0;
				which_is_on = 1;
			}
			
			dy_inter = (Hh/2. - ints_trans.at(which_is_on).y) / Hh;	//The z-interfering component	
			if( ints_trans.at(which_is_off).x > Hw/2. ){ // The shadow image spills off the +x side of the heliostat
				dx_inter = .5 - ints_trans.at(which_is_on).x / Hw;
			}
			else {	//The shadow image spills off the -x side of the heliostat
				dx_inter = ints_trans.at(which_is_on).x / Hw + .5;
			}

			return dy_inter * dx_inter;
		}
		else{	//No interfering
			return 0.;
		}
	}

}

double *SolarField::getPlotBounds(bool /*use_land*/){
	/* 
	Returns the field bound extents for plotting based on the field layout 
	
	"use_land" indicates whether the extents should factor in all available land
	
	*/

	//if(use_land){}
	
	//else{
		//Return the array (size = 4) of heliostat position boundaries [xmax,xmin,ymax,ymin]
		return _helio_extents;
	//}


}



void SolarField::updateAllTrackVectors(Vect &Sun){
    //update all tracking vectors according to the current sun position
    if(_var_map->flux.aim_method.mapval() == var_fluxsim::AIM_METHOD::FREEZE_TRACKING)
        return;
    
    int npos = (int)_heliostats.size();
	for(int i=0; i<npos; i++){
		_heliostats.at(i)->updateTrackVector(Sun);
	}

}

void SolarField::calcHeliostatShadows(Vect &Sun){

	//Calculate the heliostat shadows
	sp_point P;	//sp_point on a plane representing the ground
	Vect Nv;	//Vector normal to the ground surface
	Nv.Set(0., 0., 1.);
	int npos = (int)_heliostats.size();
	//Calculate differently if the heliostats are round
#if 0
	if(_helio_templates.at(0)->getVarMap()->is_round.mapval() == var_heliostat::IS_ROUND::ROUND)
    {

		//This is all wrong


		Vect hvect_xy, hvect_z, svect_xy, svect_z;
		double
			fscale_x, fscale_y;
		double
			hdiam = _heliostats.at(0)->getVarMap()->width.val;
		for(int i=0; i<npos; i++){
			P.Set(0.,0., -hdiam/2.*1.1);
			vector<sp_point> *sc = _heliostats.at(i)->getShadowCoords();
			sc->resize(2);
			/* 
			The shadow coordinates for round heliostats will be:
			0 | x,y,z  of shadow centroid
			1 | <shadow width in x>,<shadow width in y>,0.			
			*/

			Vect *hvect = _heliostats.at(i)->getTrackVector();
			Toolbox::plane_intersect(P, Nv, *_heliostats.at(i)->getLocation(), Sun, sc->at(0) );
			//Get relevant vectors
			hvect_xy.Set(hvect->i, hvect->j, 0.);
			Toolbox::unitvect(hvect_xy);
			svect_xy.Set(Sun.i, Sun.j, 0.);
			Toolbox::unitvect(svect_xy);
			hvect_z.Set( Toolbox::vectmag(hvect->i, hvect->j, 0.), 0., hvect->k);
			Toolbox::unitvect(hvect_z);
			svect_z.Set( Toolbox::vectmag(Sun.i, Sun.j, 0.), 0., Sun.k);
			Toolbox::unitvect(svect_z);
			//dot products determine shadow scaling
			fscale_x = Toolbox::dotprod(hvect_xy, svect_xy);
			fscale_y = Toolbox::dotprod(svect_z, hvect_z);


			//Calculate the shadow width and height
			sc->at(1).Set( fscale_x*hdiam, fscale_y*hdiam, 0 );
			
		}
	}
	else
#endif
    {   //rectangular heliostats
		for(int i=0; i<npos; i++){
			P.Set(0., 0., -_heliostats.at(i)->getVarMap()->height.val/2.*1.1);
			_heliostats.at(i)->getShadowCoords()->resize(4);
			for(int j=0; j<4; j++){
				Toolbox::plane_intersect(P, Nv, _heliostats.at(i)->getCornerCoords()->at(j), Sun, _heliostats.at(i)->getShadowCoords()->at(j) );
			}
		}
	}
}

void SolarField::calcAllAimPoints(Vect &Sun, sim_params &P) //bool force_simple, bool quiet) 
{
	/* 
	Method can be:

	0	|	Simple aim points	|	All heliostats point at vertical centerline of their closest receiver point
	1	|	Sigma aim point		|	Heliostats spread depending on image size (sigmas =std dev of image in x and y)
		-> For this method, args[0] = limiting sigma factor. This determines the distance away from the edge
									  in standard deviations of each image.
							args[1] = alternation flag indicating which receiver edge the offset should reference (+1 or -1)
    2   |   Probability         |   
    3   |   Image SIze          |
        ->  args[0] = limiting sigma factor
            args[1] = limiting sigma factor y
            args[2] = First image flag
    4   |   Keep Existing       |
    5   |   Freeze              |
		
	When calculating the aim points, the heliostat images should be previously updated. Call the flux image updator for methods other
	than simple aim points.

	*/

	int nh = (int)_heliostats.size();

    int method = _var_map->flux.aim_method.mapval();
    if(P.is_layout && method != var_fluxsim::AIM_METHOD::KEEP_EXISTING)
        method = var_fluxsim::AIM_METHOD::SIMPLE_AIM_POINTS;

    //set up args based on the method
    double args[] = {0., 0., 0., 0.};
    switch (method)
    {
    //case FluxSimData::AIM_STRATEGY::EXISTING:
    //case FluxSimData::AIM_STRATEGY::SIMPLE:
    case var_fluxsim::AIM_METHOD::KEEP_EXISTING:
    case var_fluxsim::AIM_METHOD::SIMPLE_AIM_POINTS:
        break;
    //case FluxSimData::AIM_STRATEGY::SIGMA:
    case var_fluxsim::AIM_METHOD::SIGMA_AIMING:
		args[0] = _var_map->flux.sigma_limit_y.val;
        args[1] = 1.;
        break;
    //case FluxSimData::AIM_STRATEGY::PROBABILITY:
    case var_fluxsim::AIM_METHOD::PROBABILITY_SHIFT:
        if(_var_map->flux.flux_dist.mapval() == var_fluxsim::FLUX_DIST::NORMAL){
			//Normal dist, get the standard dev of the sampling distribution
			args[2] = _var_map->flux.norm_dist_sigma.val;
		}
		args[0] = _var_map->flux.sigma_limit_y.val;
		args[1] = _var_map->flux.flux_dist.mapval();	//the distribution type
        break;
    //case FluxSimData::AIM_STRATEGY::IMAGE_SIZE:
    case var_fluxsim::AIM_METHOD::IMAGE_SIZE_PRIORITY:
        args[0] = _var_map->flux.sigma_limit_y.val;
        args[1] = _var_map->flux.sigma_limit_x.val;
        break;
    //case FluxSimData::AIM_STRATEGY::FREEZE:
    case var_fluxsim::AIM_METHOD::FREEZE_TRACKING:
        args[0] = Sun.i;
        args[1] = Sun.j;
        args[2] = Sun.k;
        break;
    default:
        break;
    }


	//for methods that require sorted heliostats, create the sorted data
	Hvector hsort;
	vector<double> ysize;
    int imsize_last_enabled=0;
	if(method == var_fluxsim::AIM_METHOD::IMAGE_SIZE_PRIORITY)
    {
        //update images
        //RefactorHeliostatImages(Sun);

		//Create a list of heliostats sorted by their Y image size
		int nh = (int)_heliostats.size();
		for(int i=0; i<nh; i++)
        {
            //update heliostat efficiency and optical coefficients
            SimulateHeliostatEfficiency(this, Sun, _heliostats.at(i), P);

            hsort.push_back(_heliostats.at(i));
			ysize.push_back(_heliostats.at(i)->getImageSize()[1]);
		}
		quicksort(ysize,hsort,0,nh-1);	//Sorts in ascending order

        //find the first enabled heliostat. This will be the last one called.
        for(size_t i=0; i<hsort.size(); i++)
        {
            if( hsort.at(i)->IsEnabled() )
            {
                imsize_last_enabled=nh - 1 - (int)i;
                break;
            }
        }
	}
	//--
    if(! P.is_layout)
    {
	    _sim_info.Reset();
	    _sim_info.setTotalSimulationCount(nh);
    }
	int update_every = method == var_fluxsim::AIM_METHOD::IMAGE_SIZE_PRIORITY ? max(nh/20,1) : nh+1;
	for(int i=0; i<nh; i++){
		
        int usemethod = method;

        //hande image size priority separately from the main switch structure
        if( method == var_fluxsim::AIM_METHOD::IMAGE_SIZE_PRIORITY )
        {
			try{
                if( hsort.at(nh-i-1)->IsEnabled() )     //is it enabled?
                {
				    args[2] = i == 0 ? 1. : 0.;
				    _flux->imageSizeAimPoint(*hsort.at(nh-i-1), *this, args, i==imsize_last_enabled);	//Send in descending order
                }
                else
                {
                    _flux->zenithAimPoint(*hsort.at(nh-i-1), Sun);
                    usemethod = -1;
                }

			}
			catch(...){
				return;
			}
        }
        else
        {
            //handle all other methods' disabled status here
            if( ! _heliostats.at(i)->IsEnabled() ) 
            {
                //this heliostat is disabled. The aimpoint should point the heliostat to zenith
                _flux->zenithAimPoint(*_heliostats.at(i), Sun);
                usemethod = -1;
            }
        }


		switch(usemethod)
		{
        case var_fluxsim::AIM_METHOD::SIMPLE_AIM_POINTS:
			//Determine the simple aim point - doesn't account for flux limitations
			_flux->simpleAimPoint(*_heliostats.at(i), *this);
			break;
        case var_fluxsim::AIM_METHOD::SIGMA_AIMING:
			args[1] = -args[1];
			_flux->sigmaAimPoint(*_heliostats.at(i), *this, args);
			break;
        case var_fluxsim::AIM_METHOD::PROBABILITY_SHIFT:
			_flux->probabilityShiftAimPoint(*_heliostats.at(i), *this, args);
			break;
        case var_fluxsim::AIM_METHOD::KEEP_EXISTING:
			//Keep existing aim point, but we still need to update the image plane flux point (geometry may have changed)
        {
            _flux->keepExistingAimPoint(*_heliostats.at(i), *this, 0);
			break;
        }
        case var_fluxsim::AIM_METHOD::FREEZE_TRACKING:
            //update the aim point based on the movement of the sun and the resulting shift in the reflected image
            _flux->frozenAimPoint(*_heliostats.at(i), _var_map->sf.tht.val, args);
            break;
        case -1:
        default:
            //nothing
            break;
		}


		//Update the progress bar
        if(! P.is_layout )
        {
		    if(i%update_every==0) {
                if(! _sim_info.setCurrentSimulation(i+1) ) break;
            }
        }
	}
    if(! P.is_layout)
    {
	    _sim_info.Reset();
	    _sim_info.setCurrentSimulation(0);
    }
	setAimpointStatus(true);	//all aimpoints should be up to date

}

int SolarField::getActiveReceiverCount(){
		int n=0;
		for(unsigned int i=0; i<_receivers.size(); i++){ n += _receivers.at(i)->isReceiverEnabled() ? 1 : 0; }
		return n;
}

bool SolarField::parseHeliostatXYZFile(const std::string &filedat, layout_shell &layout ){
	/*
	Take the heliostat layout in text form and parse it into a shell for later use.

	The text lines should be separated by the "\n" newline character, and can be delimited by "," | " " | ";" | "\t"


	Structure:
		0				1			2			3			4			5				6				7			   8		9		10		11		12	     13
	<template (int)> <enabled> <in layout> <location X> <location Y> <location Z> <x focal length> <y focal length> <cant i> <cant j> <cant k> <aim X> <aim Y> <aim Z>

	*/
	layout.clear();
	
	//Split by lines
	vector<string> entries = split(filedat, ";"); 
	//if there's only one entry, we used the wrong delimiter. Try "\n"
	int nlines = (int)entries.size();
	if(nlines < 2){
		entries.clear();
		entries = split(filedat, "\n");
		nlines = (int)entries.size();
	}

	//Resize the heliostat vector
	layout.reserve(nlines);
		
	vector<string> data;
	int i, j;
	double loc[3], focal[2], cant[3], aim[3];
		
	//Find the type of delimiter
	string delim = Toolbox::getDelimiter(entries.at(0));
	data.clear();

	//try to handle old version
	bool is_old_version = false;

	for(i=0; i<nlines; i++){
		data = split(entries.at(i), delim);	
		
		//check for empty lines
		if( data.size() < 2 ) continue;
		layout.push_back(layout_obj());

		//If the number of entries is less than 14 (but must be at least 6), append NULL's
		int dsize = (int)data.size();

		//if the data length is 12, we're loading from an old file
		if (i == 0)
		{
			if (dsize == 12)
				is_old_version = true;
		}


		if(dsize < 6){
			char fmt[] = "Formatting error\nLine %d in the imported layout is incorrectly formatted. The error occurred while parsing the following text:\n\"%s\"";
			char msg[250];
			sprintf(msg, fmt, i+1, entries.at(i).c_str());
			throw(spexception(msg));  //error!
		}
		else if(dsize < 14){
			for(unsigned int k=dsize; k<14; k++){ data.push_back("NULL"); }
		}
		
		int col = 0; // current column
			
		//which template should we use?
		to_integer(data.at(col++), &layout.at(i).helio_type);
			
		//assign enabled and layout status
		if (!is_old_version)
		{
			to_bool(data.at(col++), layout.at(i).is_enabled);
			to_bool(data.at(col++), layout.at(i).is_in_layout);
		}
		else
		{
			layout.at(i).is_enabled = true;
			layout.at(i).is_in_layout = true;
		}

		//Assign the location
		for(j=0; j<3; j++)
		{ 
			to_double(data.at(col++), &loc[j]); 
		}
		layout.at(i).location.Set(loc[0], loc[1], loc[2]);

		//Assign the focal length
		if(data.at(col)!="NULL"){
			for(j=0; j<2; j++)
			{ 
				to_double(data.at(col++), &focal[j]);
			}
			layout.at(i).focal_x = focal[0];
			layout.at(i).focal_y = focal[1];
			layout.at(i).is_user_focus = true;
		}
		else{
			layout.at(i).is_user_focus = false;
		}
			
		//Assign the cant vector unless its null
		if(data.at(col)!="NULL")
		{
			for(j=0; j<3; j++)
			{ 
				to_double(data.at(col++), &cant[j]);
			}
			layout.at(i).cant.Set(cant[0], cant[1], cant[2]);
			layout.at(i).is_user_cant = true;
		}
		else{
			layout.at(i).is_user_cant = false;
		}
			
		//Assign the aim point unless its null
		if(data.at(col)!="NULL")
		{
			for(j=0; j<3; j++)
			{ 
				to_double(data.at(col++), &aim[j]);
			}
			layout.at(i).aim.Set(aim[0], aim[1], aim[2]);
			layout.at(i).is_user_aim = true;
		}
		else{
			layout.at(i).is_user_aim = false;
		}

	}
	return true;
}

int SolarField::calcNumRequiredSimulations(){
	//Based on the solar field settings, how many simulations are impending?
	int nsim;
    int des_sim_detail = _var_map->sf.des_sim_detail.mapval();
	if(des_sim_detail == var_solarfield::DES_SIM_DETAIL::DO_NOT_FILTER_HELIOSTATS){
		nsim = 1;
	}
	else{
		if(des_sim_detail == var_solarfield::DES_SIM_DETAIL::SUBSET_OF_DAYSHOURS){	//Subset of days, all sunlight hours in the selected days
			//Calculate which hours from the days are needed
			//TODO
			throw spexception("Subset hours: Method not currently supported");
		}
		//else if(des_sim_detail == var_solarfield::DES_SIM_DETAIL::ANNUAL_SIMULATION){
			//nsim = _var_map->amb.weather_file.val.size();
		//}
		else{
			nsim = _var_map->sf.sim_step_data.Val().size();	//If other type of simulation, use the data established in the design select method
		}
	}
	return nsim;
}

double SolarField::getReceiverTotalHeatLoss()
{
    double qloss = 0.;

    for(int i=0; i<(int)_receivers.size(); i++)
    {
        qloss = _receivers.at(i)->getReceiverThermalLoss()*1000.;   //kWt
    }

    return qloss;
}

double SolarField::getReceiverPipingHeatLoss()
{
	double qloss = 0.;

    for(int i=0; i<(int)_receivers.size(); i++)
    {
        qloss = _receivers.at(i)->getReceiverPipingLoss()*1000.;   //kWt
    }

    return qloss;
}


void SolarField::HermiteFluxSimulation(Hvector &helios, bool keep_existing_profile)
{
	if( ! keep_existing_profile ) 
		AnalyticalFluxSimulation(helios);
	CalcDimensionalFluxProfiles(helios);
}

void SolarField::AnalyticalFluxSimulation(Hvector &helios)
{
	//Simulate each receiver flux profile (non-dimensional)
	int nrec = (int)_receivers.size();
	for(int n=0; n<nrec; n++){
		if(! _receivers.at(n)->isReceiverEnabled() ) continue;
		FluxSurfaces *surfaces = _receivers.at(n)->getFluxSurfaces();
		for(unsigned int i=0; i<surfaces->size(); i++){
			_flux->fluxDensity(&_sim_info, surfaces->at(i), helios, true, true, true);		
		}
	}

}

void SolarField::CalcDimensionalFluxProfiles(Hvector &helios)
{
	/* 
	Take the existing non-dimensional flux profiles in the Receivers::getFluxSurfaces() 
	and alter the values to indicate total dimensional power on the receiver surfaces
	*/

	//DNI
	double dni = _var_map->flux.flux_dni.val*0.001; // _var_map->sf.dni_des.val*0.001;	 //kW/m2

	//Determine the total power delivered from the heliostats. This serves as a normalizing basis.
	double q_to_rec=0.;
	for(unsigned int i=0; i<helios.size(); i++){
		q_to_rec += helios.at(i)->getEfficiencyTotal()*helios.at(i)->getArea()*dni;	//[kW]
	}
	//Receiver surface area
	double Arec = calcReceiverTotalArea();	//[m2]
	//Convert to kW/m2
	//double q_rec_spec = q_to_rec / Arec;

	//Simulate for each receiver
	int nrec = (int)_receivers.size();
	for(int n=0; n<nrec; n++){
		if(! _receivers.at(n)->isReceiverEnabled() ) continue;
		FluxSurfaces *surfaces = _receivers.at(n)->getFluxSurfaces();
		for(unsigned int i=0; i<surfaces->size(); i++){
			FluxSurface *fs = &surfaces->at(i);
					
			//Take the normalized flux values and multiply to get flux density [kW/m2]
			FluxGrid *grid = fs->getFluxMap();
			double fmax=0.;
            double maxbin=0.;
            double ftot=0.;
            double ftot2 = 0.;
			int nfy = fs->getFluxNY(), nfx = fs->getFluxNX();
			double nfynfx = (double)(nfy*nfx);
            double anode = Arec / nfynfx;
			for(int j=0; j<nfy; j++){
				for(int k=0; k<nfx; k++){
					double *pt = &grid->at(k).at(j).flux;
                    ftot += *pt;
                    if(*pt > maxbin)
                        maxbin = *pt;
                    *pt *= q_to_rec / anode;

					//*pt *= q_rec_spec*nfynfx;
                    ftot2 += *pt;
					if(*pt > fmax)
						fmax = *pt;	
				}
			}
			fs->setMaxObservedFlux(fmax);
		}
	}

}

void SolarField::copySimulationStepData(WeatherData &wdata){
	//the weather data structure contains some pointers that need to be reset. Copy the
	//data and reset the pointers here

	int n = (int)_var_map->sf.sim_step_data.Val().size();
	wdata.resizeAll(n);
	double day, hour, month, dni, tdb, pres, vwind, step_weight;
	for(int i=0; i<n; i++){
		_var_map->sf.sim_step_data.Val().getStep( i, day, hour, month, dni, tdb, pres, vwind, step_weight);
		wdata.setStep(i, day, hour, month, dni, tdb, pres, vwind, step_weight );
	}

}

bool SolarField::CalcDesignPtSunPosition(int sun_loc_des, double &az_des, double &zen_des)
{
    /* 
    Calculate the design-point sun position given a design point specified by the user.

    sun_loc_des:
        0   Summer Solstice (June 21 N, Dec 21 S)
        1   Equinox (March 20, = Sept 20)
        2   Winter solstice (Dec 21 N, June 21 S)
        3   Zenith (180, 90 elev)
        4   User specified

    Sets:
        az_des      [deg]
        zen_des     [deg]

    Returns:   
        Bool (success)
    */

    int month, day;

        
    bool N_hemis = _var_map->amb.latitude.val > 0.;

    switch (sun_loc_des)
    {
    case var_solarfield::SUN_LOC_DES::ZENITH:
        az_des = 180.;
        zen_des = 0.;
        return true;
    case var_solarfield::SUN_LOC_DES::OTHER:
        az_des = _var_map->sf.sun_az_des_user.val;
        zen_des = 90. - _var_map->sf.sun_el_des_user.val;
        return true;

    // ^^^^ these methods are done and have returned without calling sun position 
    case var_solarfield::SUN_LOC_DES::SUMMER_SOLSTICE:
        month = N_hemis ? 6 : 12;
        day = 21;
        break;
    case var_solarfield::SUN_LOC_DES::EQUINOX:
        month = 3;
        day = 20;
        break;
    case var_solarfield::SUN_LOC_DES::WINTER_SOLSTICE:
        month = N_hemis ? 12 : 6;
        day = 21;
        break;
    default:
		_sim_error.addSimulationError("This design-point sun position option is not available", true); 
        return false;;
    }

    //call the sun position algorithm here

    //Convert the day of the month to a day of year
    DateTime DT;
    int doy = DT.GetDayOfYear(2011,month,day);
    Ambient::setDateTime(DT, 12., doy);
	//Calculate the sun position
    Ambient::calcSunPosition(*_var_map, DT, &az_des, &zen_des);

	//If the sun is not above the horizon plus a very small amount, fail
	return zen_des < 90.;
}

double SolarField::getAnnualPowerApproximation()
{
	return _estimated_annual_power;
}

double SolarField::getDesignThermalPowerWithLoss(){ return _q_des_withloss; }

double SolarField::getActualThermalPowerWithLoss(){ return _q_to_rec/1.e6; }
// --- clouds ---

void SolarField::clouds::Create(var_map &V, double extents[2]){

	_all_locs.clear(); //just to be safe...

	//if it's not enabled, we can skip
	if(! V.flux.is_cloudy.val) return;

	//Calculate shadow location(s) here
    int cloud_shape = V.flux.cloud_shape.mapval();
	if(V.flux.is_cloud_pattern.val && cloud_shape == var_fluxsim::CLOUD_SHAPE::FRONT)
    {
		switch (cloud_shape)
		{
        case var_fluxsim::CLOUD_SHAPE::ELLIPTICAL:
        case var_fluxsim::CLOUD_SHAPE::RECTANGULAR:
		{
			
			//create a point for the initial cloud location
			sp_point loc = {V.flux.cloud_loc_x.val, V.flux.cloud_loc_y.val, 0.};
			//rotate into original coordinates
			Toolbox::rotation(-V.flux.cloud_skew.val, 2, loc);
			double rcloud_max = max(V.flux.cloud_depth.val, V.flux.cloud_width.val)/2.;
			double dx = V.flux.cloud_width.val * V.flux.cloud_sep_width.val;
			double dy = V.flux.cloud_depth.val * V.flux.cloud_sep_depth.val;
			double
				rfmax = extents[1],
				xp = rfmax - loc.x + rcloud_max + dx/2.,
				xm = V.flux.is_cloud_symw.val ? rfmax + loc.x + rcloud_max : 0.,
				yp = rfmax - loc.y + rcloud_max,
				ym = V.flux.is_cloud_symd.val ? rfmax + loc.y + rcloud_max : 0.;
			
			//add primary point
			//_all_locs.push_back({_cloud_loc_x, _cloud_loc_y, 0.});
			int nry = (int)ceil( (yp + ym) / dy );
			int nrx = (int)ceil( (xp + xm) / dx );

			int ixs = -(int)ceil( xm / dx );
			int iys = -(int)ceil( ym / dy );

			for(int j = iys; j < nry+1; j++){
				double xoffset = j%2==0 ? 0. : dx/2.;
				for(int i = ixs; i < nrx+1; i++){
					sp_point cloc = {dx * i - xoffset, dy * j, 0.};
					Toolbox::rotation(V.flux.cloud_skew.val*D2R, 2, cloc);
					cloc.Add(V.flux.cloud_loc_x.val, V.flux.cloud_loc_y.val, 0.);
					_all_locs.push_back(cloc);
				}
			}
			
			break;
		}
        case var_fluxsim::CLOUD_SHAPE::FRONT:
			throw spexception("Cannot create a patterned cloud front! Please disable the \"" + V.flux.is_cloud_pattern.short_desc + "\" checkbox.");
			break;
		default:
			break;
		}
	}
	else{
		//Single cloud
		
		sp_point p;
		p.x = V.flux.cloud_loc_x.val;
		p.y = V.flux.cloud_loc_y.val;
		p.z = 0.;
		_all_locs.push_back(p);
	}

}

double SolarField::clouds::ShadowLoss(var_map &V, sp_point &hloc){
	/* 
	Calculate the loss due to cloudiness for this particular location in the field
	*/
	if(! V.flux.is_cloudy.val) return 1.;

	for(vector<sp_point>::iterator cpt = _all_locs.begin(); cpt != _all_locs.end(); cpt ++){
		//express the heliostat location in the coordinate system of the shadow
		sp_point hloc_rot = {hloc.x - cpt->x, hloc.y - cpt->y, 0.};
		Toolbox::rotation(-V.flux.cloud_skew.val*R2D, 2, hloc_rot);

		bool shadowed = false;
		switch (V.flux.cloud_shape.mapval())
		{
        case var_fluxsim::CLOUD_SHAPE::ELLIPTICAL:
		{
			double 
				rx = V.flux.cloud_width.val/2.,
				ry = V.flux.cloud_depth.val/2.;
			if( hloc_rot.x*hloc_rot.x / (rx * rx) + hloc_rot.y*hloc_rot.y / (ry * ry) < 1. )
				shadowed = true;
			break;
		}
        case var_fluxsim::CLOUD_SHAPE::RECTANGULAR:
			if( fabs(hloc_rot.x) < V.flux.cloud_width.val/2. && fabs(hloc_rot.y) < V.flux.cloud_depth.val/2.)
				shadowed = true;

			break;
        case var_fluxsim::CLOUD_SHAPE::FRONT:
			if( hloc_rot.y > 0. )
				shadowed = true;

			break;
		default:
			break;
		}
		if(shadowed)
			return 1. - V.flux.cloud_opacity.val;
	}

	return 1.;
}

