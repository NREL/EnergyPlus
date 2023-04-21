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

#ifndef _AMBIENT_H_
#define _AMBIENT_H_ 1
#include <string>
#include <vector>

#include "mod_base.h"
#include "definitions.h"

/*
Compiler note: If solpos or field_core code is not found, 
Add "solpos.lib;field_core.lib;" to PiLOT_GUI project settings, Linker->Input->Additional Dependencies.  
Add "$(SolutionDir)\solpos;" to field_core project settings, C/C++->Additional Include Directories.

The weather file reader is provided in SAM SIM CORE (SSC).
*/


//------------------
class Ambient : public mod_base
 {
    var_ambient *_amb_map;
 
 public:

	void Create(var_map &V);
    void updateCalculatedParameters(var_map &V);
	void Clean();

	static std::string getDefaultSimStep();	//d.o.m., hour, month, dni, pressure, wind
	
	//Calculation methods
    static void setDateTime(DateTime &DT, double day_hour, double year_day, double year = 2011.);
	static void calcSunPosition(var_map &V, DTobj &DT, double *az, double *zen, bool wf_time_correction=false); //use some local info, some other info
	static void calcSunPosition(double lat, double lon, double timezone, double tstep, const DTobj &dt, double *az, double *zen);	//Calculate with these arguments
    static Vect calcSunVectorFromAzZen(double azimuth, double zenith);	//Calculate sun position given specified az/zen values
	
    static void calcDaytimeHours(double hrs[2], double lat, double lon, double timezone, const DTobj &dt);
	static bool readWeatherFile(var_map &V); 
	static double calcAttenuation(var_map &V, double &len);
	static void calcSpacedDaysHours(double lat, double lon, double tmz, int nday, double delta_hr, std::vector<std::vector<double> > &utime, std::vector<int> &uday); //calculate days and times that produce evenly spaced sun positions over the year
	static double calcInsolation(var_map &V, double azimuth, double zenith, int day_of_year); //calculate clear-sky radiation using one of the DELSOL models

 } ;

#endif
