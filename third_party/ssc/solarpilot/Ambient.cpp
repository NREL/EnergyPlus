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

#include <math.h>
#include <vector>
#include <stdio.h>
#include <math.h>

#include "Ambient.h"
#include "solpos00.h"
#include "Toolbox.h"
#include <shared/lib_weatherfile.h>

#include "definitions.h"

using namespace std;

//-------------------------------AMBIENT CLASS------------------------------------

string Ambient::getDefaultSimStep(){return "20,12,3,950,25,1,0,1";}	//d.o.m., hour, month, dni, pressure, wind, weight factor


//----------Calculation methods----------


void Ambient::Create(var_map &V)
{
	_amb_map = &V.amb;
    
    updateCalculatedParameters(V);
}

void Ambient::Clean(){};

void Ambient::updateCalculatedParameters(var_map &/*V*/)
{

}

void Ambient::setDateTime(DateTime &DT, double day_hour, double year_day, double year){
	DT.setZero();
	double min, sec;
	min = (day_hour - floor(day_hour))*60.;
	sec = (min - floor(min))*60.;
	DT.SetHour(int(floor(day_hour)));
	DT.SetMinute(int(min));
	DT.SetSecond(int(sec));
	DT.SetYearDay(int(year_day));
	DT.SetYear(int(year));
	int month, dom;
	DT.hours_to_date((year_day-1)*24.+day_hour, month, dom);
	DT.SetMonth(int(month));
	DT.SetMonthDay(int(dom));
}

Vect Ambient::calcSunVectorFromAzZen(double azimuth, double zenith) {

	/* 
    azimuth [rad]
    zenith [rad]

    Calculate the unit vector for sun position (i,j,k) relative to 
	the plant location.
	*/

    Vect sp;
	sp.i = sin(azimuth)*sin(zenith);
	sp.j = cos(azimuth)*sin(zenith);
	sp.k = cos(zenith);

	return sp;	
}

void Ambient::calcSunPosition(var_map &V, DTobj &DT, double *az, double *zen, bool wf_time_correction)
{
    //[degr] [degr]
	calcSunPosition( V.amb.latitude.val, V.amb.longitude.val, V.amb.time_zone.val, wf_time_correction ? V.amb.sim_time_step.Val() : 0., DT, az, zen);
}

void Ambient::calcSunPosition(double lat, double lon, double timezone, double tstep, const DTobj &dt, double *az, double *zen){
	/*
    lat [deg]
    lon [deg]
    timezone [hr]
    tstep [sec]

	Use SOLPOS to calculate the sun position.

	The required inputs for SOLPOS are:
		(via posdata)
		year, month, day of month, day of year, hour, minute, second,
		latitude, longitude, timezone, interval
	*/
	
	//Instantiate the solpos object
	struct posdata SP, *pdat;
	pdat = &SP;	//point to structure for convenience
	S_init(pdat);		//Initialize the values
	
	pdat->latitude = float(lat);		//[deg] {float} North is positive
	pdat->longitude = float(lon);		//[deg] {float} Degrees east. West is negative
	pdat->timezone = float(timezone);			//[hr] {float} Time zone, east pos. west negative. Mountain -7, Central -6, etc..
	pdat->year = dt._year;		//[year] {int} 4-digit year
	pdat->month = dt._month+1;	//[mo] {int} (1-12)
	pdat->day = dt._mday;		//[day] {int} Day of the month
	pdat->daynum = dt._yday;	//[day] {int} Day of the year
	pdat->hour = dt._hour;		//[hr] {int} 0-23
	pdat->minute = dt._min;	//[min] {int} 0-59
	pdat->second = dt._sec;	//[sec]	{int} 0-59
	pdat->interval = int(tstep);	//[sec] {int} Measurement interval, should correspond to the duration of the weather file time step. 
		//Note that the interval determines the time at which the sun position is calculated.
		//The sun position is calculated at (hour - interval/2.)


	long retcode = 0;		//Initialize with no errors
	retcode = S_solpos(pdat);	//Call the solar posotion algorithm
	S_decode(retcode, pdat);	//Check the return code

	//set the calculated values
	*az = SP.azim; 
    *zen = SP.zenetr;

    return;
}

void Ambient::calcDaytimeHours(double hrs[2], double lat, double lon, double timezone, const DTobj &dt){
	/* Calculate the limiting hours during which the sun is above the horizon */
	struct posdata SP, *pdat;
	pdat = &SP;	//point to structure for convenience
	S_init(pdat);		//Initialize the values
	
	double r2d = 180./acos(-1);
	pdat->latitude = float(lat*r2d);		//[deg] {float} North is positive
	pdat->longitude = float(lon*r2d);		//[deg] {float} Degrees east. West is negative
	pdat->timezone = float(timezone);			//[hr] {float} Time zone, east pos. west negative. Mountain -7, Central -6, etc..
	pdat->year = dt._year;		//[year] {int} 4-digit year
	pdat->month = dt._month+1;	//[mo] {int} (1-12)
	pdat->day = dt._mday;		//[day] {int} Day of the month
	pdat->daynum = dt._yday;	//[day] {int} Day of the year
	pdat->hour = dt._hour;		//[hr] {int} 0-23
	pdat->minute = dt._min;	//[min] {int} 0-59
	pdat->second = dt._sec;	//[sec]	{int} 0-59
	pdat->interval = 0;		//[sec] {int} Measurement interval. See solpos documentation.


	long retcode = 0;		//Initialize with no errors
	retcode = S_solpos(pdat);	//Call the solar posotion algorithm
	//srss( pdat );
	S_decode(retcode, pdat);	//Check the return code
	hrs[0] = pdat->sretr/60.;
	hrs[1] = pdat->ssetr/60.;

}

bool Ambient::readWeatherFile(var_map &V)
{

    /*

	NOTE: This method does not currently implement psychrometric property algorithms or irradiance correction methods, 
	so data is used "as is" from the weather file. Many weather files do not provide wet bulb temperature directly and
	it must be calculated from dry bulb, relative humidity, and ambient pressure.

	This method takes as inputs:
	A pointer to the data map that will contain the weather file data. This map will have keys (uppercase) 
	that correspond to the data label and an associated vector of the timestep data. The included data streams
	are:

	Key		|	Description						| Units
	-----------------------------------------------------
	DAY		|	Day of the month (1-31)			| days
	MONTH	|	Month of the year (1-12)		| month
	HOUR		|	Hour of the day (1-24)			| hr
	DNI		|	Direct normal irradiation		| W/m2
	T_DB		|	Dry bulb ambient temperature	| C
	V WIND	|	Wind velocity					| m/s


	*/	

	//Open the file
	weatherfile wf_reader;
	if(! wf_reader.open(V.amb.weather_file.val)) return false;	//Error
	//Read the header info
    weather_header wh;
    wf_reader.header(&wh);
	V.amb.latitude.val = wh.lat; //deg
    V.amb.longitude.val = wh.lon; //deg
    V.amb.time_zone.val = wh.tz;
    V.amb.elevation.val = wh.elev;
    
	//Read in the weather data
	int nrec = (int)wf_reader.nrecords();
	V.amb.wf_data.val.resizeAll(nrec);
    weather_record wrec;
	for(int i=0; i<nrec; i++){
		if(! wf_reader.read(&wrec) ) return false; //Error
		V.amb.wf_data.val.Day.at(i) = (double)wrec.day;
		V.amb.wf_data.val.DNI.at(i) = (double)wrec.dn;
		V.amb.wf_data.val.Hour.at(i) = (double)wrec.hour;
		V.amb.wf_data.val.Month.at(i) = (double)wrec.month;
		V.amb.wf_data.val.Pres.at(i) = wrec.pres/1000.;	//bar
		V.amb.wf_data.val.T_db.at(i) = wrec.tdry;		//C
		V.amb.wf_data.val.V_wind.at(i) = wrec.wspd;	//m/s
		V.amb.wf_data.val.Step_weight.at(i) = 1.;	//default step
	}
	return true;

}

double Ambient::calcAttenuation(var_map &V, double &len){
	/*
	Length in units of meters. 
	Atmospheric attenuation model set on Create

	Calculate atmospheric attenuation as a function of slant range. Model options are:
	0:	Barstow 25km (polynomials, DELSOL)
	1:  Barstow 5km visibility (polynomials, DELSOL)
	2:	User defined coefficients (polynomials)
	3:	Sengupta & Wagner model
	*/
	double att=0.0;
		
	//if(V.amb.atm_model.val < 3){ //Barstow 25k or 5km visibility from DELSOL, user coefs
		double rkm = len*.001;
		int nc = (int)V.amb.atm_coefs.val.ncols();
        int atm_sel = V.amb.atm_model.combo_get_current_index();
		for(int i=0; i<nc; i++)
        { 
            att += V.amb.atm_coefs.val.at(atm_sel, i)*pow(rkm, i); 
        }
	//}
	//else if(V.amb.atm_model.val == 3){
	//	//Sengupta model
	//	throw spexception("The Sengupta/Wagner attenuation model is not yet implemented.");
	//}

	return 1.-att;

}

void Ambient::calcSpacedDaysHours(double lat, double lon, double tmz, int nday, double delta_hr, vector<vector<double> > &utime, vector<int> &uday){
	//Method taken from PTGen code (Wagner 2008 thesis)
	double pi = PI;
	uday.resize(nday);
	vector<int> 
		ntstep(nday),
		ntstep_day(nday);

	vector<double> 
		noons(nday),
		hours(nday);
	
	DateTime DT;
	int month, dom;

	for(int i=0; i<nday; i++){
		//Calculate the day number - The days are evenly distributed over the cosine wave of the year
		uday[i] = 355 - (int)floor(acos(-1.+2.*i/(float)(nday-1))/pi*(float)(355-172));
		
		DT.hours_to_date(uday[i]*24 +12., month, dom);
		DT.SetHour(12);
		DT.SetDate(2011, month, dom);
		DT.SetYearDay(uday[i]);
		double hrs[2];
		Ambient::calcDaytimeHours(hrs, lat*D2R, lon*D2R, tmz, DT);

		noons.at(i) = (hrs[0]+hrs[1])/2.;
		//shorten the time by 0.9 so we don't get stuff really close to the horizon
		ntstep[i] = (int)floor( (hrs[1]-noons.at(i))*.9/delta_hr );
		
		//For the calculated day, determine the solar declination angle and the number of daylight hours
		//delta[i] = asin(23.45*D2R*cos((float)(uday[i]-173)*D2R)); 
		//nhour[i] = (int)(floor((2./15.*acos(-tan(phi)*tan(delta[i]))*r2d)/2.));
		ntstep_day[i] = 2*ntstep[i]+1;
	}
	//Store the day and time in the utime array
	utime.clear();
	vector<double> utemp;
	int nflux_sim = 0;
	for(int i=0; i<nday; i++){
		utemp.clear();
		for(int j=0; j<ntstep_day[i]; j++){
			utemp.push_back( noons[i]+(-ntstep[i]+j)*delta_hr -12.);
			nflux_sim++;
		}
		utime.push_back(utemp);
	}
	
}

double Ambient::calcInsolation(var_map &V, double /*azimuth*/, double zenith, int day_of_year) //calculate clear-sky radiation using one of the DELSOL models
{
	/* 
	Inputs:
	azimuth		|	solar azimuth (radians)
	zenith		|	solar zenith angle (radians)
	altitude	|	site elevation / altitude (kilometers)
	model		|	clear sky model { MEINEL, HOTTEL, CONSTANT, MOON, ALLEN }
	solcon		|	*required for CONSTANT*  specified DNI - (kW/m2)

	Delsol 7065-7082ish
	*/

	//int doy = day_of_year; //_date_time.GetDayOfYear();
	double S0 = 1.353*(1.+.0335*cos(2.*PI*(day_of_year+10.)/365.));

	double szen = sin(zenith);
	double czen = cos(zenith);

	double save2 = 90. - atan2(szen, czen)*R2D;
    double save = 1.0/czen;
    if (save2 <= 30.) 
		save=save-41.972213*pow(save2,(-2.0936381-0.04117341*save2+0.000849854*pow(save2,2)));

	double ALT = V.amb.elevation.val / 1000.;
	double dni;

	switch (V.amb.insol_type.mapval())
	{
	//case Ambient::MEINEL:
    case var_ambient::INSOL_TYPE::MEINEL_MODEL:
		dni = (1.-.14*ALT)*exp(-.357/pow(czen,.678))+.14*ALT;
		break;
	//case Ambient::HOTTEL:
    case var_ambient::INSOL_TYPE::HOTTEL_MODEL:
		dni = 0.4237-0.00821*pow(6.-ALT,2)+(0.5055+0.00595*pow(6.5-ALT,2))*exp(-(0.2711+0.01858*pow(2.5-ALT,2))/(czen+.00001));
		break;
	//case Ambient::CONSTANT:
    case var_ambient::INSOL_TYPE::CONSTANT_VALUE:
		dni = V.sf.dni_des.val / (S0 * 1000.);
		break;
	//case Ambient::MOON:
    case var_ambient::INSOL_TYPE::MOON_MODEL:
		dni = 1.0-0.263*((V.amb.del_h2o.val+2.72)/(V.amb.del_h2o.val+5.0))*pow((save*V.amb.dpres.val),(0.367*((V.amb.del_h2o.val+11.53)/(V.amb.del_h2o.val+7.88))) );
		break;
	//case Ambient::ALLEN:
    case var_ambient::INSOL_TYPE::ALLEN_MODEL:
		dni = 0.183*exp(-save*V.amb.dpres.val/0.48)+0.715*exp(-save*V.amb.dpres.val/4.15)+.102;
		break;
	default:
		throw spexception("The specified clear sky DNI model is not available.");
		break;
	}
	return dni * S0 * 1000.;
	
}