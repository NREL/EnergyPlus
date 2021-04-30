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

#ifndef __pvshade_h
#define __pvshade_h

#include <string>
#include <unordered_map>
#include "lib_util.h"


//   Porting of sam_shading_type241.f90 to new orientation

//  SUPPORTING STRUCTURES

// static self-shading inputs- these do not change with timestep. dynamic inputs are inputs to the ss_exec function
struct ssinputs
{
    int nstrx, nmodx, nmody, nrows;
    double length, width;
    int mod_orient, str_orient;
    double row_space;
    int ndiode;
    double Vmp;
    int mask_angle_calc_method;
    double FF0;						// Fill Factor at STC = Pmp0 / Voc0 / Isc0;

    //constructor for ssarrdat structure- set all values to zero
    ssinputs() : nstrx(0), nmodx(0), nmody(0), nrows(0), length(0), width(0), mod_orient(0), str_orient(0), row_space(0), ndiode(0), Vmp(0), mask_angle_calc_method(0), FF0(0) {}
};

struct ssoutputs	// self-shading outputs
{
    double m_dc_derate;
    double m_reduced_diffuse;
    double m_reduced_reflected;
    double m_diffuse_derate;
    double m_reflected_derate;
    double m_shade_frac_fixed;
};

// look up table for calculating the diffuse reduction due to gcr and tilt of the panels for self-shading
// added to removing duplicate computations for speed up (https://github.com/NREL/ssc/issues/384)
class sssky_diffuse_table
{
    std::unordered_map<std::string, double> derates_table;      // stores pairs of surface tilts and derates
    double gcr;                                                 // 0.01 - 0.99

    double compute(double surface_tilt);

public:
    sssky_diffuse_table(): gcr(0) {}

    // initialize with the ground coverage ratio (fixed per PV simulation) and the starting tilt
    void init(double surface_tilt, double groundCoverageRatio) { gcr = groundCoverageRatio; compute(surface_tilt); }

    // return the sky diffuse derate for the panel at given surface_tilt
    double lookup(double surface_tilt);
};


//	SUPPORTING FUNCTIONS

bool selfshade_simple(

	/* system parameters */
	int ncells, // number of cells in panel
	double area, // panel area in m2
	int orientation, // 0 = landscape, 1 = portrait
	int panels_up, // number of panels along the edge of a row
	double FF_stc, // fill factor @ STC = Pmp0 / Voc0 / Isc0;

	/* current conditions */
	double solzen, // solar zenith angle (deg)
	double solazi, // solar azimuth angle (deg)
	double beam_horiz, // beam irradiance on the horizontal surface (W/m2)
	double diff_poa, // total diffuse irradiance on the tilted surface (W/m2)
	double albedo, // ground reflectance [0..1]

	/* calculated outputs */
	double *dc_derate,
	double *skydiff_derate,
	double *gnddiff_derate );



void diffuse_reduce(
	// inputs (angles in degrees)
	double solzen,
    double stilt,
    double Gb_nor,
    double Gdh,
    double poa_sky,
    double poa_gnd,
    double gcr,
//	double phi0, // mask angle
	double alb,
    double nrows,
    sssky_diffuse_table &skydiffderates,
	// outputs
	double &reduced_skydiff,
    double &Fskydiff,  // derate factor on sky diffuse
	double &reduced_gnddiff,
    double &Fgnddiff); // derate factor on ground diffuse



double selfshade_dc_derate( double X,
						   double S,
						   double FF0, //fill factor
						   double dbh_ratio,
						   double m_d, //number of diodes
						   double Vmp); //module Vmp


void selfshade_xs_horstr( bool landscape, // modules oriented in landscape/portrait on assembly
						   double W,   // module width (short side)
						   double L,   // module length (long side)
						   int r,      // number of rows
						   int m,      // number of modules along row edge (short side of assembly)
						   int n,      // number of modules along (long side of assembly)
						   int ndiode, // number of bypass diodes
						   double Fshad, // Fraction of assembly shaded up from long edge

						   // outputs
						   double &X, double &S);

//	SELF-SHADING CALCULATION FUNCTION

// performs shading calculation and returns outputs
bool ss_exec(
    const ssinputs &inputs,

    double tilt,		            // module tilt (constant for fixed tilt, varies for one-axis)
	double azimuth,		            // module azimuth (constant for fixed tilt, varies for one-axis)
	double solzen,		            // solar zenith (deg)
	double solazi,		            // solar azimuth (deg)
	double Gb_nor,		            // beam normal irradiance (W/m2)
	double Gdh,                     // diffuse horizontal irradiance (W/m2)
	double Gb_poa,		            // POA beam irradiance (W/m2)
	double poa_sky,		            // POA diffuse sky irradiance (W/m2)
	double poa_gnd,                 // POA diffuse gnd irradiance (W/m2)
	double albedo,		            // used to calculate reduced relected irradiance
	bool trackmode,		            // 0 for fixed tilt, 1 for one-axis tracking
	bool linear,		            // 0 for non-linear shading (C. Deline's full algorithm), 1 to stop at linear shading
	double shade_frac_1x,	        // geometric calculation of the fraction of one-axis row that is shaded (0-1), not used if fixed tilt
    sssky_diffuse_table &skydiffs,  // lookup table for sky diffuse derates

    ssoutputs &outputs);

#endif
