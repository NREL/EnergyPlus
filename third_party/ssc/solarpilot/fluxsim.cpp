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

#include "fluxsim.h"
#include "solpos00.h"

void FluxSimData::Create(var_map &V)
{
    updateCalculatedParameters(V);
}

void FluxSimData::updateCalculatedParameters(var_map &V)
{
    //may need to calculate the solar position for a particular day/time, otherwise set the solar position 
	//to the input value
	double az,zen;
	if (V.flux.flux_time_type.mapval() == var_fluxsim::FLUX_TIME_TYPE::SUN_POSITION){
		//Sun position are input, just set the corresponding values
		V.flux.flux_solar_az.Setval( V.flux.flux_solar_az_in.val );
		V.flux.flux_solar_el.Setval( V.flux.flux_solar_el_in.val );
	}
	else{
		//hour/day are provided, calculate the solar position
		int flux_day = V.flux.flux_day.val; //Day of the month
		int flux_month = V.flux.flux_month.val; //month of the year
		double flux_hour = V.flux.flux_hour.val; //hour of the day
		double lat = V.amb.latitude.val;
		double lon = V.amb.longitude.val; 
		double tmz = V.amb.time_zone.val; 

		DateTime DT;
		int doy = DT.GetDayOfYear(2011, int(flux_month), int(flux_day));
		
		//Instantiate the solpos object
		struct posdata SP, *pdat;
		pdat = &SP;	//point to structure for convenience
		S_init(pdat);		//Initialize the values

		//Calculate minutes/seconds
		double
			mins = 60.*(flux_hour - floor(flux_hour)),
			secs = 60.*(mins - floor(mins));
	
		pdat->latitude = float(lat);		//[deg] {float} North is positive
		pdat->longitude = float(lon);		//[deg] {float} Degrees east. West is negative
		pdat->timezone = float(tmz);			//[hr] {float} Time zone, east pos. west negative. Mountain -7, Central -6, etc..
		pdat->year = 2011;		//[year] {int} 4-digit year
		pdat->month = int(flux_month);	//[mo] {int} (1-12)
		pdat->day = int(flux_day);		//[day] {int} Day of the month
		pdat->daynum = doy;	//[day] {int} Day of the year
		pdat->hour = int(flux_hour+.0001);		//[hr] {int} 0-23
		pdat->minute = int(mins);	//[min] {int} 0-59
		pdat->second = int(secs);	//[sec]	{int} 0-59
		pdat->interval = 0;		//[sec] {int} Measurement interval. See solpos documentation.


		long retcode = 0;		//Initialize with no errors
		retcode = S_solpos(pdat);	//Call the solar posotion algorithm
		S_decode(retcode, pdat);	//Check the return code

		az = SP.azim;
		zen = SP.zenetr;

		V.flux.flux_solar_az.Setval( az ); 
		V.flux.flux_solar_el.Setval( 90. - zen ); 

	}

}