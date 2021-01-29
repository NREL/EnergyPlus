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

#include "lib_snowmodel.h"
#include "lib_util.h"
#include <cmath>
#include <iostream>


#ifndef M_PI
#define M_PI 3.14159265358979323846264338327
#endif

/**********************************************************************************
************************************************************************************
**
**	6 April 2015
**
**	Implementation of Bill Marion's snow model [1] to C++ for use in SAM
**	Original author: David Severin Ryberg
**
**
**********************************************************************************
**  References:
**  1: Marion, Bill, et al. "Measured and modeled photovoltaic system
**     energy losses from snow for Colorado and Wisconsin locations."
**     Solar Energy 97 (2013): 112-121.
**
**********************************************************************************
***********************************************************************************/


pvsnowmodel::pvsnowmodel()
{
	mSlope = -80;
	sSlope = (float)1.97;
	deltaThreshold = 1.00;
	depthThreshold = 1.00;
	previousDepth = 0;
	badValues = 0;
	maxBadValues = 500;
	coverage = 0;
	pCvg = 0;

	good = true;
	msg = "";

}

bool pvsnowmodel::setup(int nmody_in, float baseTilt_in, bool limitTilt){

	nmody = nmody_in;
	baseTilt = baseTilt_in;

	if(limitTilt && (baseTilt>45 || baseTilt < 10)){
		good = true;
		msg = util::format("The snow model is designed to work for PV arrays with a tilt angle between 10 and 45 degrees, but will generate results for tilt angles outside this range. The system you are modeling includes a subarray tilt angle of %f degrees.", baseTilt);
		return false;
	}

	good = true;
	return true;
}


bool pvsnowmodel::getLoss(float poa, float tilt, float , float tdry, float snowDepth, int sunup, float dt, float &returnLoss){

	bool isGood = true;

	// Check if snow depth value is valid
	// * 610 cm ~= 20 ft
	if (snowDepth < 0 || snowDepth > 610 || std::isnan(snowDepth)){
		isGood = false;
		snowDepth = 0;
		badValues++;
		if (badValues == maxBadValues){
			good = false;
			msg = util::format("The weather file contains no snow depth data or the data is not valid. Found (%d) bad snow depth values.", maxBadValues);
			return false;
		} 
	}
		
	/////////////////////////////
	// Step 1
	// look for snow fall and set current snow coverage amount accordingly
	if ((snowDepth - previousDepth) >= deltaThreshold*dt && snowDepth >= depthThreshold){
		coverage = 1;
	}
	else{
		coverage = pCvg;
	}

	// Coverage Override #1:
	//  This override assumes that if the current snow depth is less than the threshold,
	// then the snow coverage on on the PV arrays should be set to zero (even if the model
	// says otherwise).
	if (snowDepth < depthThreshold) coverage = 0;

	/////////////////////////////
	// Step 2
	//
	// Calculating Energy output is done outside this function

	/////////////////////////////
	// Step 3
	//
	// This model is adapted for hourly snow data, and so is not analyzed with
	// with respect to individual days, but instead for all 8760 yearly hours.
	// Hence, this step is skipped

	/////////////////////////////
	// Step 4
	//
	
	// If the day-time flag is not set, assume the tilt value should be equal to the system's base tilt
	//   This will fix the issue in sam where system tilt values during nightime hours is zero (even 
	//   for static systems). This may need to be altered for 1-axis tracking and 2-axis tracking systems
	if (sunup == 0){
		tilt = baseTilt;
	}

	// check if conditions are right for sliding
	if (tdry - poa / mSlope > 0){
		coverage -= (float)(0.1 * sSlope * sin(tilt * M_PI / 180) * dt);
	}

	// Coverage Override #2
	//  This override prevents the snow coverage from going below 0
	if (coverage < 0) coverage = 0;

	returnLoss = 0;
	if (nmody > 0) {
		returnLoss = ((float)ceil(coverage*nmody)) / nmody;
	}
	
	//  Current snow depth and previous snow depth are set to the correct values once
	// this function is run again

	previousDepth = snowDepth;
	pCvg = coverage;

	if (isGood) return true;
	else return false;
}
