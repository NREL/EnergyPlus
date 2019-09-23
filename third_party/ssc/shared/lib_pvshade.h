/*******************************************************************************************************
*  Copyright 2017 Alliance for Sustainable Energy, LLC
*
*  NOTICE: This software was developed at least in part by Alliance for Sustainable Energy, LLC
*  (“Alliance”) under Contract No. DE-AC36-08GO28308 with the U.S. Department of Energy and the U.S.
*  The Government retains for itself and others acting on its behalf a nonexclusive, paid-up,
*  irrevocable worldwide license in the software to reproduce, prepare derivative works, distribute
*  copies to the public, perform publicly and display publicly, and to permit others to do so.
*
*  Redistribution and use in source and binary forms, with or without modification, are permitted
*  provided that the following conditions are met:
*
*  1. Redistributions of source code must retain the above copyright notice, the above government
*  rights notice, this list of conditions and the following disclaimer.
*
*  2. Redistributions in binary form must reproduce the above copyright notice, the above government
*  rights notice, this list of conditions and the following disclaimer in the documentation and/or
*  other materials provided with the distribution.
*
*  3. The entire corresponding source code of any redistribution, with or without modification, by a
*  research entity, including but not limited to any contracting manager/operator of a United States
*  National Laboratory, any institution of higher learning, and any non-profit organization, must be
*  made publicly available under this license for as long as the redistribution is made available by
*  the research entity.
*
*  4. Redistribution of this software, without modification, must refer to the software by the same
*  designation. Redistribution of a modified version of this software (i) may not refer to the modified
*  version by the same designation, or by any confusingly similar designation, and (ii) must refer to
*  the underlying software originally provided by Alliance as “System Advisor Model” or “SAM”. Except
*  to comply with the foregoing, the terms “System Advisor Model”, “SAM”, or any confusingly similar
*  designation may not be used to refer to any modified version of this software or any modified
*  version of the underlying software originally provided by Alliance without the prior written consent
*  of Alliance.
*
*  5. The name of the copyright holder, contributors, the United States Government, the United States
*  Department of Energy, or any of their employees may not be used to endorse or promote products
*  derived from this software without specific prior written permission.
*
*  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR
*  IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND
*  FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER,
*  CONTRIBUTORS, UNITED STATES GOVERNMENT OR UNITED STATES DEPARTMENT OF ENERGY, NOR ANY OF THEIR
*  EMPLOYEES, BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
*  DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
*  DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER
*  IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF
*  THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*******************************************************************************************************/

#ifndef __pvshade_h
#define __pvshade_h

#include <string>

#include "lib_util.h"


//   Porting of sam_shading_type241.f90 to new orientation

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
		double solzen,
		double stilt,
		double Gb_nor,
		double Gd_poa,
		double gcr,
		double phi0, // mask angle (degrees)
		double alb,
		double nrows,
		
		double &reduced_skydiff,
		double &Fskydiff,
		double &reduced_gnddiff,
		double &Fgnddiff );



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

//	SELF-SHADING INPUT AND OUTPUT STRUCTURES AND CALCULATION FUNCTION

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

//performs shading calculation and returns outputs
bool ss_exec(
	const ssinputs &inputs,

	double tilt,		// module tilt (constant for fixed tilt, varies for one-axis)
	double azimuth,		// module azimuth (constant for fixed tilt, varies for one-axis)
	double solzen,		// solar zenith (deg)
	double solazi,		// solar azimuth (deg)
	double Gb_nor,		// beam normal irradiance (W/m2)
	double Gb_poa,		// POA beam irradiance (W/m2)
	double Gd_poa,		// POA diffuse, sky+gnd (W/m2)
	double albedo,		// used to calculate reduced relected irradiance
	bool trackmode,		// 0 for fixed tilt, 1 for one-axis tracking
	bool linear,		// 0 for non-linear shading (C. Deline's full algorithm), 1 to stop at linear shading
	double shade_frac_1x,	// geometric calculation of the fraction of one-axis row that is shaded (0-1), not used if fixed tilt 
	
	ssoutputs &outputs);

#endif