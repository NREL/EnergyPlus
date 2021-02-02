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

#ifndef __lib_snowmodel_h
#define __lib_snowmodel_h

#include <string>

class pvsnowmodel
{
public:
	pvsnowmodel();

	// limitTilt requires tilt to be between 10 and 45 degrees
	bool setup(int, float, bool limitTilt = true);

	bool getLoss(float poa, float tilt, float wspd, float tdry, float snowDepth, int sunup, float dt, float &returnLoss);

	float baseTilt,		// The default tilt for 1-axis tracking systems
		mSlope,			// This is a value given by fig. 4 in [1]
		sSlope,			// This is a value given by fig. 7 in [1]
		deltaThreshold,	// The minimum change in snow depth required to trigger a snow fall detection
		depthThreshold,	// The minimum snow depth required to trigger a snow fall detection
		previousDepth,	// The snow depth from the previous iteration
		coverage,		// Snow coverage ( 0...1 )
		pCvg;			// Snow coverage from previous iteration ( 0...1 )

	int nmody,			// number of modules in a row
		badValues,		// keeps track of the number of detected bad snow depth values
		maxBadValues;	// The number of maximum bad snow depth values that is acceptable

	std::string msg;		// This is a string used to return error messages
	bool good;				// This an error flag that will be set to false
							//  if an error has occured
};

#endif