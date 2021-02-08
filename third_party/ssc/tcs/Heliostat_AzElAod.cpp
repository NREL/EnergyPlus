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

#define _TCSTYPEINTERFACE_
#include "tcstype.h"
//#include <shared/lib_util.h>
#include "lib_util.h"
#include <algorithm>

#include "interpolation_routines.h"

using namespace std;

enum{	//Parameters
		P_eta_map,
		P_n_hel,
		P_q_start,
		P_p_run,
		P_v_wind_max,
		P_hel_stow_deploy,

		//Inputs
		I_v_wind,
		I_field_control,
		I_theta,
		I_phi,
		I_AOD,

		//Outputs
		O_pparasi,
		O_eta_field,

		//N_MAX
		N_MAX};

tcsvarinfo Heliostat3DInterp_variables[] = {
	//PARAMETERS
	{TCS_PARAM, TCS_MATRIX, P_eta_map,			"eta_map",			"Field efficiency matrix",             			        "-",        "4 columns (aod, azimuth, zenith, field efficiency)", "", ""},
	{TCS_PARAM, TCS_NUMBER, P_n_hel,            "n_hel",            "Number of heliostats in the field",					"-",		"", "", ""},
	{TCS_PARAM, TCS_NUMBER, P_q_start,			"q_start",			"Electric work for starting up one heliostat",			"kWe-hr",	"", "", ""},
	{TCS_PARAM, TCS_NUMBER, P_p_run,			"p_run",			"Electric power for tracking one heliostat",			"kWe",		"", "", ""},
	{TCS_PARAM, TCS_NUMBER, P_v_wind_max,		"v_wind_max",		"Maximum tolerable wind speed",							"m/s",		"", "", ""},
	{TCS_PARAM, TCS_NUMBER, P_hel_stow_deploy,	"hel_stow_deploy",	"Heliostat field stow/deploy solar elevation angle",	"deg",			"", "", ""},

	//INPUTS
	{TCS_INPUT, TCS_NUMBER, I_v_wind,			"vwind",			"Wind velocity",										"m/s",		"", "", ""},
	{TCS_INPUT, TCS_NUMBER, I_field_control,	"field_control",	"Field defocus control",								"",			"", "", ""},
	{TCS_INPUT, TCS_NUMBER, I_theta,			"theta",			"Solar zenith angle",									"deg",		"", "", ""},
	{TCS_INPUT, TCS_NUMBER, I_phi,				"phi",				"Solar azimuth angle: 0 due north, clockwise to +360",  "deg",		"", "", ""},
	{TCS_INPUT, TCS_NUMBER, I_AOD,				"aod",				"Third dimension interpolation value",					"-",		"", "", ""},

	//OUTPUTS
	{TCS_OUTPUT, TCS_NUMBER, O_pparasi,			"pparasi",			"Parasitic tracking/startup power",						"MWe",		"", "", ""},
	{TCS_OUTPUT, TCS_NUMBER, O_eta_field,		"eta_field",		"Total field efficiency",								"",			"", "", ""},

	//N_MAX
	{TCS_INVALID, TCS_INVALID, N_MAX,			0,					0, 0, 0, 0, 0	} } ;
	
	
class Heliostat3DInterp : public tcstypeinterface
{
private:
	// Class Instances
	Trilinear_Interp field_efficiency_table;

	//Parameters
	int n_hel;
	double q_start;
	double p_run;
	double v_wind_max;	
	double hel_stow_deploy;
	
	//other variables
	int n_zen;
	int n_azi;
	int n_layer;
	

	//Stored Variables
	double eta_prev;
	double v_wind_prev;

public:
	Heliostat3DInterp( tcscontext *cst, tcstypeinfo *ti)
		: tcstypeinterface( cst, ti)
	{
		n_hel = (int)std::numeric_limits<double>::quiet_NaN();
		q_start = std::numeric_limits<double>::quiet_NaN();
		p_run = std::numeric_limits<double>::quiet_NaN();
		v_wind_max = std::numeric_limits<double>::quiet_NaN();
		hel_stow_deploy = std::numeric_limits<double>::quiet_NaN();

		eta_prev = std::numeric_limits<double>::quiet_NaN();
		v_wind_prev = std::numeric_limits<double>::quiet_NaN();
	}

	virtual ~Heliostat3DInterp()
	{
	}

	virtual int init()
	{
		// Read in parameters
		n_hel = (int)value( P_n_hel );					// [-] Number of heliostats
		q_start = value( P_q_start ) * 3600.0;		// [kJ] convert from kWe-hr
		p_run = value( P_p_run ) * 3600.0;			// [kJ/hr] convert from kWe
		v_wind_max = value( P_v_wind_max );			// [m/s] Wind speed at which heliostats are stowed
		hel_stow_deploy = value( P_hel_stow_deploy ); // [deg] Heliostat field stow/deploy solar elevation angle

		/* 
		Process the efficiency matrix

		4 columns by X rows
		0 column: x value - [1..nx, 1..nx, 1..nx, -> repeat ny times ] 
		1 column: y value - [1..1 (nx times), 2..2 (nx times), ... , ny..ny (nx times) ] 
		2 column: z value - [1..1 (nx*ny) times]
		3 column: result value	[(nx*ny) values]
		Repeat list for each layer
		*/
		int rows, cols;
		value( P_eta_map, &rows, &cols );

		//get initial values. Track the change in values over the list
		double
			zen, azi, layer,
			zen0 = TCS_MATRIX_INDEX( var( P_eta_map ), 0, 0),
			azi0 = TCS_MATRIX_INDEX( var( P_eta_map ), 0, 1),
			layer0 =  TCS_MATRIX_INDEX( var( P_eta_map ), 0, 2);
		int nzen0, nzen1, nazi0, nazi1, nlayer0, nlayer1;
		nzen1 = nazi1 = nlayer1 = 1;
		nzen0 = nazi0 = nlayer0 = 0;

		//loop through and check that the changes in variables create a coherent block structure
		bool size_error = false;
		for(int i=0; i<rows; i++){

			zen = TCS_MATRIX_INDEX( var( P_eta_map ), i, 0);
			azi = TCS_MATRIX_INDEX( var( P_eta_map ), i, 1);
			layer = TCS_MATRIX_INDEX( var( P_eta_map ), i, 2);

			if(layer != layer0){
				nlayer1++;
				layer0 = layer;

				//check that both the azimuth and zenth counts are correct
				if(nzen0 > 0 && nzen0 != nzen1){
					size_error = true;
					break;
				}
				if(nazi0 > 0 && nazi0 != nazi1){
					size_error = true;
					break;
				}

				zen0 = zen;
				azi0 = azi;

				nzen0 = nzen1;
				nzen1 = 1;
				nazi0 = nazi1;
				nazi1 = 1;
				continue;

			}	

			if(azi != azi0){
				nazi1++;
				azi0 = azi;

				//check that the zenith count is the same as the last one
				if( nzen0 > 0 && nzen0 != nzen1){
					size_error = true;
					break;
				}

				nzen0 = nzen1;
				nzen1 = 1;
				continue;
			}

			if(zen != zen0){ 
				nzen1++;
				zen0 = zen;
			}

		}

		if(size_error){
			message(TCS_ERROR, "The heliostat efficiency matrix is not properly dimensioned. Please ensure the number of zenith and azimuth values are consistent in all dimensions.");
			return -1;
		}
		
		n_zen = nzen1;
		n_azi = nazi1;
		n_layer = nlayer1;

		//make sure there is sufficient data to interpolate
		if( n_zen < 2 || n_azi < 2 ||n_layer < 2 ){
			message(TCS_ERROR, "The field efficiency matrix contains insufficient data. Each dimension must have at least 2 levels.");
			return -1;
		}

		//transpose data into a block structure
		util::block_t<double> eta_map( n_zen*n_azi, 4, n_layer, 0.0);
		int k=0, kk=0;
		
		//zenith, azimuth, layer, value
		for( int l = 0; l < n_layer; l++){
			for( int r = 0; r < n_zen*n_azi; r++ ){
				for( int c = 0; c < 4; c++ ){
					eta_map.at(k,c,l) = TCS_MATRIX_INDEX( var( P_eta_map ), kk, c );
				}
				k++;
				kk++;
			}
			k=0;
		}
		
		
		// Set up Bilinear Interpolation class for field efficiency data
		if( !field_efficiency_table.Set_3D_Lookup_Table( eta_map ) )
		{
			message( TCS_ERROR, "Initialization of 2D interpolation class failed" );
			return -1;
		}

		
		// Initialize stored variables
		eta_prev = 0.0;
		v_wind_prev = 0.0;

		return 0;
	}

	virtual int call( double /*time*/, double step, int /*ncall*/ )
	{						
		// GET AND CHECK INPUT VALUES
		double v_wind = value( I_v_wind );	// [m/s] wind speed
		double field_control = value( I_field_control ); // Control Parameter ( range from 0 to 1; 0=off, 1=all on)
		if( field_control > 1.0 )
			field_control = 1.0;
		if( field_control < 0.0 )
			field_control = 0.0;
		double theta = value( I_theta );	// solar zenith angle 
		if( theta >= 90.0 )
			field_control = 0.0;		// No tracking before sunrise of after sunset
		double phi = value( I_phi );	
		double layer = value( I_AOD );		//interpolation value in the 3rd dimension
		// Weather reader convention: 0 due north - 360 clockwise
		// solar azimuth angle - Convention used HERE: 0 at south - 360 clockwise
		// Convert
		if(phi <= 180.0)
			phi += 180.0;
		else
			phi -= 180.0;
      
		// Parasitics for startup or shutdown
		double pparasi = 0.0; 
		
		// If starting up or shutting down, calculate parasitics
		if( (field_control > 1.e-4 && eta_prev < 1.e-4) ||		// Startup by setting of control paramter (Field_control 0-> 1)
		(field_control < 1.e-4 && eta_prev >= 1.e-4) ||			// OR Shutdown by setting of control paramter (Field_control 1->0 )
		(field_control > 1.e-4 && v_wind >= v_wind_max ) ||		// OR Shutdown by high wind speed
		(eta_prev > 1.e-4 && v_wind_prev >= v_wind_max && v_wind < v_wind_max)  )	// OR Startup after high wind speed
			pparasi = n_hel * q_start / (step/3600.0);			// kJ/hr 
     
		// Parasitics for tracking      
		if( v_wind < v_wind_max && v_wind_prev < v_wind_max )
				pparasi += n_hel * p_run * field_control;	// kJ/hr

		// Use current solar position to interpolate field efficiency table and fied solar field efficiency
		double eta_field = field_efficiency_table.trilinear_3D_interp( theta, phi, layer );
		eta_field = min( max ( eta_field, 0.0 ), 1.0 );		// Ensure physical behavior 
		
		if( theta >= 90.0 || (90.0-theta) < max( hel_stow_deploy, 0.1 ) )
			eta_field = 1.e-6;

		if( v_wind < v_wind_max )
			eta_field = max( eta_field*field_control, 1.e-6 );
		else
			eta_field = 1.e-6;

		// Set output parameters
		value( O_pparasi, pparasi/3.6e6 );	// [MW], convert from kJ/hr: Parasitic power for tracking
		value( O_eta_field, eta_field );	// [-], field efficiency

		return 0;
	}

	virtual int converged( double /*time*/ )
	{
		eta_prev = value( O_eta_field );
		v_wind_prev = value( I_v_wind );
		
		return 0;
	}

};

TCS_IMPLEMENT_TYPE( Heliostat3DInterp, "Interpolated optical efficiency matrix - 3D", "Mike Wagner", 1, Heliostat3DInterp_variables, NULL, 1 )

