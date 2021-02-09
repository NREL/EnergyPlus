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

#include <stdio.h>
#include <algorithm>
#include <sstream>
#include <math.h>

#include "interop.h"
#include "SolarField.h"
#include "STObject.h"
#include "solpos00.h"
#include "sort_method.h"

using namespace std;


//-------------------  arraystring ----------------

ArrayString::ArrayString(){data.clear();}
	
//wxArrayStr &operator=( ArrayString &array );
ArrayString &ArrayString::operator=( vector<string> &array )
{ 
	data = array; 
	return *this;
}
	
int ArrayString::size(){return (int)data.size();}
string ArrayString::operator[](int i){return data.at(i);}
string& ArrayString::at(int i){return data.at(i);}
	
void ArrayString::clear(){data.clear();}
void ArrayString::Clear(){data.clear();}	

void ArrayString::resize(int newsize){data.resize(newsize);}
void ArrayString::push_back(string value){data.push_back(value);}
void ArrayString::Add(string value){data.push_back(value);}

string ArrayString::back(){return data.back();}
int ArrayString::Index(string item){
	for(int i=0; i<(int)data.size(); i++){
		if(item == data.at(i)) return i;
	}
	return -1;
};

vector<string>::iterator ArrayString::erase(vector<string>::iterator position){ return data.erase( position ); }
vector<string>::iterator ArrayString::begin(){return data.begin(); }
//-------------------

//-------------------  par_variable ----------------
par_variable::par_variable()
{ 
    linked = false; 
    layout_required = false;
}
//-------------------


void interop::GenerateSimulationWeatherData(var_map &V, int design_method, ArrayString &wf_entries){
	/* 
	Calculate and fill the weather data steps needed for simulation and the associated time step.

	The weather data is filled in the variable set vset["solarfield"][0]["sim_step_data"].value

	wf_entries consists of a list strings corresponding to each time step. 
	Each string is comma-separated and has the following entries:
	day, hour, month, dn, tdry, pres/1000., wspd
	*/
	
	WeatherData *wdatvar = &V.sf.sim_step_data.Val(); 

	switch (design_method)
	{
    case var_solarfield::DES_SIM_DETAIL::SUBSET_OF_DAYSHOURS:
	//case LAYOUT_DETAIL::SUBSET_HOURS:

	{		//Subset of days/hours
		//Need to add this still
		V.amb.sim_time_step.Setval(0.);
		throw spexception("Simulation with a user-specified list of days/hours is not currently supported. Please use another option.");
		//break;
	}
	//case LAYOUT_DETAIL::SINGLE_POINT:
    case var_solarfield::DES_SIM_DETAIL::SINGLE_SIMULATION_POINT:
	{  //2) Single design point=1;
		V.amb.sim_time_step.Setval(0.);
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
        wdatvar->resizeAll(1);
        wdatvar->setStep(0, dom, hour, month, P.dni, P.Tamb, P.Patm, P.Vwind, P.Simweight);
		break;
	}
	//case LAYOUT_DETAIL::NO_FILTER:
    case var_solarfield::DES_SIM_DETAIL::DO_NOT_FILTER_HELIOSTATS:
	{	//3) Do not filter heliostats=0;
		wdatvar->clear();
		V.amb.sim_time_step.Setval(0.);
		break;
	}
	//case LAYOUT_DETAIL::FULL_ANNUAL:
    case var_solarfield::DES_SIM_DETAIL::ANNUAL_SIMULATION:
	{	//4) Annual simulation=3;
		*wdatvar = WeatherData(V.amb.wf_data.val); //vset["ambient"][0]["wf_data"].value;
        wdatvar->initPointers();

		V.amb.sim_time_step.Setval(3600.);
		break;
	}
	//case LAYOUT_DETAIL::MAP_TO_ANNUAL:
    case var_solarfield::DES_SIM_DETAIL::EFFICIENCY_MAP__ANNUAL:
	{  //Efficiency map + annual simulation
		V.amb.sim_time_step.Setval(3600.);
		wdatvar->clear();
		
		vector<int> uday;
		vector<vector<double> > utime;
		double
			lat = V.amb.latitude.val*D2R,
			lng = V.amb.longitude.val*D2R,
			tmz = V.amb.time_zone.val,
			dni_des = V.sf.dni_des.val;
		int nday = V.sf.des_sim_ndays.val;
		Ambient::calcSpacedDaysHours(lat, lng, tmz, nday, 1., utime, uday);

		int nflux_sim = 0;
		for(int i=0; i<(int)utime.size(); i++)
			nflux_sim += (int)utime.at(i).size();
		
		DateTime dt;
		double hoy, hod;
		int month, dom;
		for(int i=0; i<nday; i++){
			int doy = uday.at(i);	//because of the doy calcluation used before, this is actually [0..364]
			for(int j=0; j<(int)utime.at(i).size(); j++){
				hod = utime.at(i).at(j)+12.;
				hoy = double( doy ) * 24.;
				dt.hours_to_date( hoy, month, dom );	//midnight on the month/day
								
				/*char ts[150];
				std::sprintf(ts,"[P]%d,%f,%d,%f,%f,%f,%f,%f", dom, hod, month, dni_des, 25., 1., 1., 1.);
				wdatvar->append(ts);*/
                wdatvar->append(dom, hod, month, dni_des, 25., 1., 1., 1.0);
			}
		}
		break;

	}
	//case LAYOUT_DETAIL::LIMITED_ANNUAL:
	//case LAYOUT_DETAIL::AVG_PROFILES:
    case var_solarfield::DES_SIM_DETAIL::LIMITED_ANNUAL_SIMULATION:
    case var_solarfield::DES_SIM_DETAIL::REPRESENTATIVE_PROFILES:
	case -1:  //for optimization
	{	//5) Limited annual simulation=4 || Representative profiles=5
		V.amb.sim_time_step.Setval(0.);  //calculate sun position at time specified
		wdatvar->clear();

		//Datetime object
		DateTime dt;
		double
			lat = V.amb.latitude.val,
			lng = V.amb.longitude.val,
			tmz = V.amb.time_zone.val;

		int nday, nskip;
		if( design_method == -1 ){
			nday = 4;
			nskip = 2;
		}
		else{
			nday = V.sf.des_sim_ndays.val;
			nskip = V.sf.des_sim_nhours.val;
		}
		

		double delta_day = 365./float(nday);
		double doffset;
		if(nday%2 == 1){ doffset = 0.; }
		else{ doffset = delta_day/2.; }
		vector<int> simdays(nday,0);
		
		int dinit = 171-(int)Toolbox::round( ( floor(float(nday)/2.) - 1 )*delta_day - doffset );
		int dcalc;
		for(int i=0; i<nday; i++){ 
			dcalc = int( Toolbox::round(dinit + i*delta_day) );
			if(dcalc < 1){ dcalc += 365; }
			else if(dcalc > 365){ dcalc += -365; }
			simdays.at(i) = dcalc - 1;		//Day based on 0 index [0..364]
		}
		//Sort chronologically
		quicksort(simdays, 0, nday-1);

		//Calculate the month and day for each item
		vector<string> tsdat;
		for(int i=0; i<nday; i++){
			int month, dom;
			double hoy;
			int doy = simdays.at(i);	//because of the doy calcluation used before, this is actually [0..364]
			hoy = double( doy ) * 24.;
			dt.hours_to_date( hoy, month, dom );	//midnight on the month/day

			//---Get some info on the weather for each day---
			
			//Calculate the start and end of daytime hours
			dt.SetHour(12);
			dt.SetDate(2011,(int)month,(int)dom);
			doy++;	//Now correct for the doy index
			dt.SetYearDay(doy);	
			double hrs[2];
			Ambient::calcDaytimeHours(hrs, lat*D2R, lng*D2R, tmz, dt);
					

			//Add up the total and average DNI's
			double dni, tdry, pres, wind;
			
			double hrmid = (hrs[0] + hrs[1])/2. + hoy;
			int nhrs = (int)(floor((hrs[1] - hrs[0])/(double)nskip))*nskip;

			//make sure the start and end hours are symmetric about solar noon
			double nmidspan = (double)nhrs/2.;
			//double nmidspan = (double)nskip*floor(nhrs/(2.*(double)nskip));
			double hr_st = hrmid - nmidspan; 
			double hr_end = hrmid + nmidspan;
				

			//Handle the "limited annual.." and "representative profile" options differently
			if(design_method == var_solarfield::DES_SIM_DETAIL::LIMITED_ANNUAL_SIMULATION){	//Limited annual simulation
				
                //weighting fractions for DNI
				double fthis = fmin(0.5, hr_st - floor(hr_st)) + fmin(0.5, ceil(hr_st) - hr_st);
				//double fthis = all_time.front() - floor(all_time.front());
				double fcomp = 1.-fthis;

				//for integration, which way should we go?
				int iind = (hr_st - floor(hr_st)) < 0.5 ? -1 : 1;
				//int iind = fthis < 0.5 ? -1 : 1;



                
                //preprocess the day's weather
				/*vector<double>
					all_time, all_dni, all_tdry, all_pres, all_wind, all_weights;*/
				double jd = hr_st;
				while(jd<hr_end+.001){	//include hr_end
					//index associated with time jd
					int jind = (int)floor(jd);	//the (j-1) originally may have been an error
					tsdat = split(wf_entries.at(jind), ",");

					to_double(tsdat.at(3), &dni);
					to_double(tsdat.at(4), &tdry);
					to_double(tsdat.at(5), &pres);
					to_double(tsdat.at(6), &wind);

					/*all_time.push_back(jd);
					all_dni.push_back(dni);
					all_tdry.push_back(tdry);
					all_pres.push_back(pres);
					all_wind.push_back(wind);*/

					//Calculate weighting factor for this hour
					double hod = fmod(jd,24.);
					double step_weight;
					if(jd==hr_st){
						step_weight = hod - hrs[0] + nskip/2.;
					}
					else if(jd + nskip < hr_end + 0.001){
						step_weight = nskip;
					}
					else{
						step_weight = hrs[1] - hod + nskip/2.;
					}
					step_weight *= Toolbox::round(delta_day);
					//all_weights.push_back( step_weight );

					//jd += (double)nskip;	
				//}

				//int ndatpt = (int)all_time.size();

				
				
				//for(int i=0; i<ndatpt; i++){

					//calculate the adjusted DNI based on the time surrounding the simulation position
					double dnimod, dnicomp;
					if(iind > 0)
					    tsdat = split(wf_entries.at(min(8759,jind+1)), ",");
						//dnicomp = i < ndatpt - 1 ? all_dni.at(i+1) : 0.;
					else
					    tsdat = split(wf_entries.at(max(0,jind-1)), ",");
						//dnicomp = i > 0 ? all_dni.at(i-1) : 0.;
					to_double(tsdat.at(3), &dnicomp);

					//dnimod = all_dni.at(i)*fthis + dnicomp * fcomp;
					dnimod = dni*fthis + dnicomp * fcomp;

					//double hod = fmod(jd,24.);

					//char ts[150];
					//std::sprintf(ts,"[P]%d,%f,%d,%f,%f,%f,%f,%f", int(dom), hod, int(month), dnimod, tdry, pres, wind, step_weight );
					wdatvar->append(int(dom), hod, int(month), dnimod, tdry, pres, wind, step_weight);
					
                    jd += (double)nskip;	

				}

			}
			else	//Representative profile
			{
				//For each base day, loop through the days within the profile range. Average all values within the range.
				
				//Based on the simdays array, calculate the starting and ending day to include in the range
				int simprev;
				if(i==0) simprev = simdays.back();
				else simprev = simdays.at(i-1);
				int simnext;
				if(i==nday-1) simnext = simdays.at(0);
				else simnext = simdays.at(i+1);

				int dprev = 
					simprev > simdays.at(i) ? simdays.at(i)-(simprev-365) : simdays.at(i)-simprev;
				int dnext =
					simnext < simdays.at(i) ? simnext+365 - simdays.at(i) : simnext - simdays.at(i);

				int range_start = -dprev/2; //simdays.at(i)-dprev/2;
				int range_end = dnext/2; //simdays.at(i)+dnext/2;
				int range = range_end - range_start;

				//weighting fractions for DNI
				double fthis = fmin(0.5, hr_st - floor(hr_st)) + fmin(0.5, ceil(hr_st) - hr_st);
				double fcomp = 1.-fthis;

				//for integration, which way should we go?
				int iind = (hr_st - floor(hr_st)) < 0.5 ? -1 : 1;

				int dayind=0;
				int nwf = (int)wf_entries.size();
				double dnicomp;
				double jd=hr_st;
				/*double hr_end_choose;
				if( design_method == LAYOUT_DETAIL::FOR_OPTIMIZATION )
					hr_end_choose = hrmid;
				else
					hr_end_choose = hr_end;*/

				while(jd < hr_end + 0.001){
					double tdry_per = 0., pres_per = 0., wind_per = 0., dni_per = 0., dni_per2 = 0.;
					for(int k=range_start; k<range_end; k++){
						int ind = (int)floor(jd)+k*24;
						if(ind < 0) ind += 8760;
						if(ind > 8759) ind += -8760;

						tsdat = split(wf_entries.at(ind), ",");
						to_double(tsdat.at(3), &dni);
						to_double(tsdat.at(4), &tdry);
						to_double(tsdat.at(5), &pres);
						to_double(tsdat.at(6), &wind);

						//get the complement dni data
						tsdat = split(wf_entries.at( min( max(ind+iind, 0), nwf-1) ), ",");
						to_double(tsdat.at(3), &dnicomp);


						//
						dni_per += dni * fthis + dnicomp * fcomp;
						tdry_per += tdry;
						pres_per += pres;
						wind_per += wind;

						//if it's for optimization, also add the symmetric afternoon hour
						//if(design_method == LAYOUT_DETAIL::FOR_OPTIMIZATION){
						//	double jdhi = hrmid + hrmid - jd;
						//	int indhi = (int)floor(jdhi)+k*24;
						//	if(indhi < 0) indhi += 8760;
						//	if(indhi > 8759) indhi += -8760;

						//	if(indhi == ind)
						//		continue;

						//	tsdat = split(wf_entries.at(indhi), ",");
						//	to_double(tsdat.at(3), &dni);

						//	//get the complement dni data
						//	tsdat = split(wf_entries.at( min( max(ind+iind, 0), nwf-1) ), ",");
						//	to_double(tsdat.at(3), &dnicomp);


						//	//
						//	dni_per2 += dni * fthis + dnicomp * fcomp;							
						//}

					}

					dni_per = (dni_per + dni_per2)/(double)range;
					tdry_per *= 1./(double)range;
					pres_per *= 1./(double)range;
					wind_per *= 1./(double)range;
					//daily peak dni and total daily dni
					//dni_tot += dni_per/1000.;
					//if(dni_per > dni_peak) dni_peak = dni_per;

					//day, hour, month, dni, tdry, pres, wind
					double hod = fmod(jd,24.);

					//Calculate weighting factor for this hour
					double step_weight;
					if(jd==hr_st){
						step_weight = hod - hrs[0] + nskip/2.;
					}
					else if(jd + nskip < hr_end + 0.001){
						step_weight = nskip;
					}
					else{
						step_weight = hrs[1] - hod + nskip/2.;
					}
					step_weight *= (double)range;

					//char ts[150];
					//std::sprintf(ts, "[P]%d,%f,%d,%f,%f,%f,%f,%f", int(dom), hod, int(month), dni_per, tdry_per, pres_per, wind_per, step_weight);
					wdatvar->append(int(dom), hod, int(month), dni_per, tdry_per, pres_per, wind_per, step_weight);

					dayind++;
					jd+=nskip;
				}
			}
		}
		break;
	}
	default:
		break;
	}
}

void interop::GenerateSimulationWeatherData(var_map &vset, int design_method, vector<string> &wf_entries)
{
	/* 
	OVERLOAD to support simple vector<string> type.
	
	Calculate and fill the weather data steps needed for simulation and the associated time step.

	The weather data is filled in the variable set vset["solarfield"][0]["sim_step_data"].value

	wf_entries consists of a list strings corresponding to each time step. 
	Each string is comma-separated and has the following entries:
	day, hour, month,  dni, tdry, pres, wspd
	1..,  0..,  1-12, W/m2,    C,  bar,  m/s
	*/

	//create an array string and call the main method
	ArrayString wfdat;
	for(int i=0; i<(int)wf_entries.size(); i++)
		wfdat.Add( wf_entries.at(i) );

	interop::GenerateSimulationWeatherData(vset, design_method, wfdat);


}

bool interop::parseRange(string &range, int &rangelow, int &rangehi, bool &include_low, bool &include_hi){
	//take range string of form:
	// {dlow,dhi} 
	// where {} can be replaced by ( ), [ ], ( ], [ )

	//parse the string
	vector<string> t1 = split(range, ",");
	if(t1.size()<2) return false;

	string lop, rop, ops, ls, rs;
	ls = t1.at(0);
	rs = t1.at(1);
	lop = ls.at(0);
	rop = rs.at(rs.size()-1);
	//Convert range values to integers
	to_integer(ls.erase(0,1), &rangelow);
	to_integer(rs.erase(rs.size()-1,1), &rangehi);

	ops = lop+rop;
	if(ops == " "){return false;}	//no info, don't check

	if(lop == "(") include_low = false;
	else include_low = true;
	if(rop == ")") include_hi = false;
	else include_hi = true;

	return true;

}

void interop::ticker_initialize(int indices[], int n){
	for(int i=0; i<n; i++)
		indices[i]=0;
	
}

bool interop::ticker_increment(int lengths[], int indices[], bool changed[], int n){
	/* 
	take an index array 'indices[len = n]' with maximum lengths 'lengths[len =n]' and increase
	the indices by '1'. The indices work like a scrolling counter. For example:
	if: lengths = {3,2,2}
	1	|	indices = {0,0,0}
	-> increment
	2	|	indices = {0,0,1}
	-> increment
	3	|	indices = {0,1,0}
	-> increment
	4	|	indices = {0,1,1}
	etc... 

	When initialized, indices are as shown in step 1.

	Returns false if the ticker has been exhausted
	*/

	//initialize 'changed' array
	for(int i=0; i<n; i++) changed[i] = false;

	//increment 
	bool inc_next=true;
	bool complete = false;
	for(int i=n-1; i>-1; i+=-1){
		if(inc_next){
			indices[i]++;
			changed[i] = true;

			//check for completion
			if(i==0)
				complete = indices[0]==lengths[0];
		}
		inc_next = indices[i] > lengths[i]-1;
		if(! inc_next) break;
		indices[i] = 0;
	}

	
	return complete;
}

//Simulation methods

bool interop::PerformanceSimulationPrep(SolarField &SF, Hvector &helios, int /*sim_method*/){
	/* 
	Call this method when setting up a performance simulation (i.e. a flux simulation).

	This method updates the Receiver flux map structure, the heliostat aim points, and the 
	sun position. Once all are updated, a single performance simulation is executed at the 
	specified sun position or hour/day combo (depending on user input).

	AFTER THIS METHOD:
	(A) In the GUI --- Call either HermiteFluxSimulationHandler() or SolTraceFluxSimulation()
		to do a full performance simulation for the system.
	-or-
	(B) Externally --- Call the SolarField::HermiteFluxSimulation() method to simulate performance.
	*/
	
    var_map *V = SF.getVarMap();

	//Update the receiver surface flux densities
    FluxSimData *fd = SF.getFluxSimObject();
    //make sure simulation data is up to date
    fd->Create(*V);
	vector<Receiver*> *recs = SF.getReceivers();
	
	for(unsigned int i=0; i<recs->size(); i++){
		recs->at(i)->DefineReceiverGeometry(V->flux.x_res.val, V->flux.y_res.val);
	}

	//update clouds
	double ext[2];
	SF.getLandObject()->getExtents(*V, ext );
	SF.getCloudObject()->Create(*V, ext);
	//if(SF.getCloudObject()->isCloudy()){
	for(int i=0; i<(int)helios.size(); i++){
		double eta_cloud = SF.getCloudObject()->ShadowLoss(*V,  *helios.at(i)->getLocation() );
		helios.at(i)->setEfficiencyCloudiness( eta_cloud );
		//update overall efficiency
		helios.at(i)->calcTotalEfficiency();
	}
	/*--- calculate aim points ---*/


	//need to update the SF sun position before continuing
    double az, zen;
	if(V->flux.flux_time_type.mapval() == var_fluxsim::FLUX_TIME_TYPE::SUN_POSITION){	//Sun position specified
		az = V->flux.flux_solar_az_in.val;
        zen = 90.0 - V->flux.flux_solar_el_in.val;
	}
	else{
		//day and time specified

		//Run a simulation for the specified conditions
		//<day of the month>, <hour of the day>, <month (1-12)>, <dni [W/m2]>,<amb. temperature [C]>, <atm. pressure [atm]>, <wind velocity [m/s]>
	
		int flux_day = V->flux.flux_day.val;
		double flux_hour = V->flux.flux_hour.val;
		int flux_month = V->flux.flux_month.val;
		DateTime DT;
        Ambient::setDateTime(DT, flux_hour, DT.GetDayOfYear(2011, flux_month, flux_day) );
		Ambient::calcSunPosition(*V, DT, &az, &zen); 
	}
    //update the map sun position
    V->flux.flux_solar_az.Setval( az );
    V->flux.flux_solar_el.Setval( 90. - zen );

    sim_params P;
    P.dni = V->flux.flux_dni.val; 
	P.Tamb = 25.; 
	P.Patm = 1.;

	SF.Simulate(az*D2R, zen*D2R, P);

	return !SF.ErrCheck();
			
}

#ifdef SP_USE_SOLTRACE
bool interop::SolTraceFluxSimulation_ST(st_context_t cxt, int seed, ST_System &ST, 
										int callback(st_uint_t ntracedtotal, st_uint_t ntraced, st_uint_t ntotrace, st_uint_t curstage, st_uint_t nstages, void *data),
										void *par,
                                        vector<vector<double> > *st0data, vector<vector<double> > *st1data, bool save_stage_data, bool load_stage_data)
{
	/* 
	This method requires that a SolTrace context "st_context_t" has already been created and passed to 
	the method. 
	Create by calling:
	st_context_t cxt = st_create_context();

	This method sets up and executes a single-threaded SolTrace simulation. 
	Returns status (error = false, no error = true)

	Passes an optional pointer to a callback function that updates the GUI.
	*/	
	int minrays = ST.sim_raycount;
	int maxrays = ST.sim_raymax;
	
	//simulate, setting the UI callback and a pointer to the UI class
	
	st_sim_params( cxt, minrays, maxrays );
    //if(load_stage_data)
        //return st_sim_run_data(cxt, seed, st0data, st1data, false, callback, par) != -1;
    //else if(save_stage_data)
        //return st_sim_run_data(cxt, seed, st0data, st1data, true, callback, par) != -1;
    //else
	    return st_sim_run(cxt, seed, true, callback, par) != -1;
}


bool interop::SolTraceFluxSimulation_ST(st_context_t cxt, SolarField &SF, Hvector &helios, Vect &sunvect,
							   int callback(st_uint_t ntracedtotal, st_uint_t ntraced, st_uint_t ntotrace, st_uint_t curstage, st_uint_t nstages, void *data),
							   void *par,
                               vector<vector<double> > *st0data, vector<vector<double> > *st1data, bool save_stage_data, bool load_stage_data)
{
	//Overload to be called when maintaining ST_System is not important
	
	ST_System STSim;
	STSim.CreateSTSystem(SF, helios, sunvect);
	ST_System::LoadIntoContext(&STSim, cxt);
	int seed = SF.getFluxObject()->getRandomObject()->integer();
	return SolTraceFluxSimulation_ST(cxt, seed, STSim, callback, par, st0data, st1data, save_stage_data, load_stage_data);
}
#endif

void interop::UpdateMapLayoutData(var_map &V, Hvector *heliostats){
//Fill in the data
	int npos = (int)heliostats->size();
	Heliostat *H;

	string *var = &V.sf.layout_data.val;
	var->clear();
	string sdat; //, sdat2, sdat3, sdat4;
	
	for(int i=0; i<npos; i++){
		/* Fill in the data for each heliostat in the template */


		H = heliostats->at(i);	//shorthand the pointer

		sp_point *loc = H->getLocation();
		Vect *cant = H->getCantVector();
		sp_point *aim = H->getAimPoint();

		//Save the layout to the variable maps
		//Take special care for user-specified values vs. program calculated values.
		char tchar1[300];
		if(H->getVarMap()->focus_method.mapval() == var_heliostat::FOCUS_METHOD::USERDEFINED )
            sprintf(tchar1, "%f,%f", H->getFocalX(), H->getFocalY());
        else 
            sprintf(tchar1, "NULL,NULL");
		//sdat2 = string(tchar1);

		char tchar2[300];
		if(H->IsUserCant()){ sprintf(tchar2, "%f,%f,%f", cant->i, cant->j, cant->k); }
		else{ sprintf(tchar2, "NULL,NULL,NULL"); }
		//sdat3 = string(tchar2);

		char tchar3[300];
		sprintf(tchar3, "%f,%f,%f", aim->x, aim->y, aim->z);
		//sdat4 = string(tchar3);

		char tchar4[300];
		sprintf(tchar4, "%d,%d,%d,%f,%f,%f,%s,%s,%s\n",H->getVarMap()->type.val, H->IsEnabled() ? 1 : 0, H->IsInLayout() ? 1 : 0, loc->x, loc->y, loc->z, tchar1, tchar2, tchar3);
		sdat = string(tchar4);
		var->append(sdat);

	}

}

//-----


void stat_object::initialize(){ //int size){
	min = 9.e99; 
	max = -9.e99;
	sum = 0.;
	stdev = 0.;
	ave = 0.;

}

void stat_object::set(double _min, double _max, double _ave, double _stdev, double _sum, double _wtmean)
{
	min = _min;
	max = _max;
	ave = _ave;
	stdev = _stdev;
	sum = _sum;
    wtmean = _wtmean;
}

//-----------------------
//cost
//cost_categories::cost_categories(){
//	reset();
//}
//
//void cost_categories::reset()
//{
//	//initialize
//	c_heliostat = 0.;
//	c_receiver = 0.;
//	c_tower = 0.;
//	c_tes = 0.;
//	c_pb = 0.;
//	c_other = 0.;
//	c_total = 0.;
//}
//
//void cost_categories::calculateTotalCost(){
//			
//	c_total = c_heliostat + c_receiver + c_tower + c_tes + c_pb + c_other;
//}

//-----------------------
sim_result::sim_result(){
	initialize();
}

void sim_result::initialize(){
	total_heliostat_area = 0.;
	total_receiver_area = 0.;
	total_land_area = 0.;
	power_on_field = 0.;
	power_absorbed = 0.;
    power_thermal_loss = 0.;
    power_piping_loss = 0.;
    power_to_htf = 0.;
	power_to_cycle = 0.;
	power_gross = 0.;
	power_net = 0.;
	num_heliostats_used = 0;
	num_heliostats_avail = 0;
    num_ray_traced = 0;
    num_ray_heliostat = 0;
    num_ray_receiver = 0;
	_q_coe = 0.;

	eff_total_heliostat.initialize();
	eff_total_sf.initialize();
	eff_cosine.initialize();
	eff_attenuation.initialize();
	eff_blocking.initialize();
	eff_shading.initialize();
	eff_reflect.initialize();
	eff_intercept.initialize();
	eff_absorption.initialize();
    eff_cloud.initialize();
	flux_density.initialize();

	flux_surfaces.clear();
	data_by_helio.clear();
}

void sim_result::add_heliostat(Heliostat &H){
	H.getEfficiencyObject()->rec_absorptance = H.getWhichReceiver()->getVarMap()->absorptance.val;
	data_by_helio[ H.getId() ] = *H.getEfficiencyObject();
	num_heliostats_used++;
	total_heliostat_area += H.getArea();
	_q_coe += H.getPowerValue();
}

void sim_result::process_field_stats(){
	//Calculate statistics for all of the heliostats
    if( data_by_helio.size() == 0 )
        return;

	int nm = (data_by_helio.begin()->second).n_metric;
	double 
		*sums = new double[nm],
		*aves = new double[nm],
		*stdevs = new double[nm],
		*mins = new double[nm],
		*maxs = new double[nm],
        *wtmean = new double[nm];
	
	double *aves2 = new double[nm];		//Temporary array for calculating variance

	for(int i=0; i<nm; i++){ 
		sums[i] = 0.;
		stdevs[i] = 0.;
		mins[i] = 9.e9;
		maxs[i] = -9.e9;
		aves[i] = 0.;
		aves2[i] = 0.;
        wtmean[i] = 0.;
	}

	//Calculate metrics
	int n=0;
	double delta;
	for(unordered_map<int, helio_perf_data>::iterator it = data_by_helio.begin(); it != data_by_helio.end(); it++){
		n++;
		for(int j=0; j<nm; j++){
			double v = it->second.getDataByIndex(j);
			sums[j] += v;
			if(v > maxs[j]) maxs[j] = v;
			if(v < mins[j]) mins[j] = v;	

			//Average and variance estimates using Knuth's method
			delta = v - aves[j];
			aves[j] += delta / (double)n;
			aves2[j] += delta*(v-aves[j]);
		}
	}
	//Calculate final standard deviation
	for(int j=0; j<nm; j++)
		stdevs[j] = sqrt(aves2[j]/(double)(n-1));

	delete [] aves2;

    //calculate weighted average efficiency values
    std::vector<int> eff_cascade_indices = {
        helio_perf_data::PERF_VALUES::ETA_CLOUD,
        helio_perf_data::PERF_VALUES::ETA_SHADOW,
        helio_perf_data::PERF_VALUES::ETA_COS,
        helio_perf_data::PERF_VALUES::SOILING,
        helio_perf_data::PERF_VALUES::REFLECTIVITY,
        helio_perf_data::PERF_VALUES::ETA_BLOCK,
        helio_perf_data::PERF_VALUES::ETA_ATT,
        helio_perf_data::PERF_VALUES::ETA_INT,
        helio_perf_data::PERF_VALUES::REC_ABSORPTANCE
    };
    int nh = (int)data_by_helio.size();
    double *rowprod = new double[ nh ];
    for (int i = 0; i < nh; i++)
        rowprod[i] = 1.;    //initialize

    for (size_t i = 0; i < eff_cascade_indices.size(); i++)
    {
        int cascade_index = eff_cascade_indices.at(i); //keep track of current index

        //calculate the running product of losses 'rowprod[j]' for all heliostats j
        int j = 0;
        for (unordered_map<int, helio_perf_data>::iterator it = data_by_helio.begin(); it != data_by_helio.end(); it++)
            rowprod[j++] *= it->second.getDataByIndex(cascade_index);

        //Sum the running product vector and divide by number of heliostats for uncorrected weighted average
        for (j = 0; j < nh; j++)
            wtmean[cascade_index] += rowprod[j];
        wtmean[cascade_index] /= (double)(nh > 0 ? nh : 1);

        //Correct the current efficiency by taking out previous weighted efficiency values
        for (size_t k = 0; k < i; k++)
            wtmean[cascade_index] /= wtmean[eff_cascade_indices.at(k)];
    }

    delete[] rowprod;

	//Assign the named variables
    eff_total_heliostat.set(
        mins[helio_perf_data::PERF_VALUES::ETA_TOT],
        maxs[helio_perf_data::PERF_VALUES::ETA_TOT],
        aves[helio_perf_data::PERF_VALUES::ETA_TOT],
        stdevs[helio_perf_data::PERF_VALUES::ETA_TOT],
        sums[helio_perf_data::PERF_VALUES::ETA_TOT],
        wtmean[helio_perf_data::PERF_VALUES::ETA_TOT]);
    eff_cosine.set(
        mins[helio_perf_data::PERF_VALUES::ETA_COS],
        maxs[helio_perf_data::PERF_VALUES::ETA_COS],
        aves[helio_perf_data::PERF_VALUES::ETA_COS],
        stdevs[helio_perf_data::PERF_VALUES::ETA_COS],
        sums[helio_perf_data::PERF_VALUES::ETA_COS],
        wtmean[helio_perf_data::PERF_VALUES::ETA_COS]);
    eff_attenuation.set(
        mins[helio_perf_data::PERF_VALUES::ETA_ATT],
        maxs[helio_perf_data::PERF_VALUES::ETA_ATT],
        aves[helio_perf_data::PERF_VALUES::ETA_ATT],
        stdevs[helio_perf_data::PERF_VALUES::ETA_ATT],
        sums[helio_perf_data::PERF_VALUES::ETA_ATT],
        wtmean[helio_perf_data::PERF_VALUES::ETA_ATT]);
    eff_blocking.set(
        mins[helio_perf_data::PERF_VALUES::ETA_BLOCK],
        maxs[helio_perf_data::PERF_VALUES::ETA_BLOCK],
        aves[helio_perf_data::PERF_VALUES::ETA_BLOCK],
        stdevs[helio_perf_data::PERF_VALUES::ETA_BLOCK],
        sums[helio_perf_data::PERF_VALUES::ETA_BLOCK],
        wtmean[helio_perf_data::PERF_VALUES::ETA_BLOCK]);
	eff_shading.set(
		mins[ helio_perf_data::PERF_VALUES::ETA_SHADOW ], 
		maxs[ helio_perf_data::PERF_VALUES::ETA_SHADOW ], 
		aves[ helio_perf_data::PERF_VALUES::ETA_SHADOW ], 
		stdevs[ helio_perf_data::PERF_VALUES::ETA_SHADOW ], 
        sums[helio_perf_data::PERF_VALUES::ETA_SHADOW],
        wtmean[helio_perf_data::PERF_VALUES::ETA_SHADOW]);
    eff_intercept.set(
        mins[helio_perf_data::PERF_VALUES::ETA_INT],
        maxs[helio_perf_data::PERF_VALUES::ETA_INT],
        aves[helio_perf_data::PERF_VALUES::ETA_INT],
        stdevs[helio_perf_data::PERF_VALUES::ETA_INT],
        sums[helio_perf_data::PERF_VALUES::ETA_INT],
        wtmean[helio_perf_data::PERF_VALUES::ETA_INT]);
    eff_absorption.set(
        mins[helio_perf_data::PERF_VALUES::REC_ABSORPTANCE],
        maxs[helio_perf_data::PERF_VALUES::REC_ABSORPTANCE],
        aves[helio_perf_data::PERF_VALUES::REC_ABSORPTANCE],
        stdevs[helio_perf_data::PERF_VALUES::REC_ABSORPTANCE],
        sums[helio_perf_data::PERF_VALUES::REC_ABSORPTANCE],
        wtmean[helio_perf_data::PERF_VALUES::REC_ABSORPTANCE]);
    eff_cloud.set(
        mins[helio_perf_data::PERF_VALUES::ETA_CLOUD],
        maxs[helio_perf_data::PERF_VALUES::ETA_CLOUD],
        aves[helio_perf_data::PERF_VALUES::ETA_CLOUD],
        stdevs[helio_perf_data::PERF_VALUES::ETA_CLOUD],
        sums[helio_perf_data::PERF_VALUES::ETA_CLOUD],
        wtmean[helio_perf_data::PERF_VALUES::ETA_CLOUD]);
	eff_reflect.set(
		mins[ helio_perf_data::PERF_VALUES::REFLECTIVITY ]*mins[ helio_perf_data::PERF_VALUES::SOILING ], 
		maxs[ helio_perf_data::PERF_VALUES::REFLECTIVITY ]*maxs[ helio_perf_data::PERF_VALUES::SOILING ], 
		aves[ helio_perf_data::PERF_VALUES::REFLECTIVITY ]*aves[ helio_perf_data::PERF_VALUES::SOILING ], 
		stdevs[ helio_perf_data::PERF_VALUES::REFLECTIVITY ]*stdevs[ helio_perf_data::PERF_VALUES::SOILING ], 
		sums[ helio_perf_data::PERF_VALUES::REFLECTIVITY ]*sums[ helio_perf_data::PERF_VALUES::SOILING ],
        wtmean[helio_perf_data::PERF_VALUES::REFLECTIVITY] * wtmean[helio_perf_data::PERF_VALUES::SOILING]
	);
	eff_total_sf.set(
		mins[ helio_perf_data::PERF_VALUES::ETA_TOT ]* wtmean[ helio_perf_data::PERF_VALUES::REC_ABSORPTANCE ],
		maxs[ helio_perf_data::PERF_VALUES::ETA_TOT ]* wtmean[ helio_perf_data::PERF_VALUES::REC_ABSORPTANCE ],
		aves[ helio_perf_data::PERF_VALUES::ETA_TOT ]* wtmean[ helio_perf_data::PERF_VALUES::REC_ABSORPTANCE ],
		stdevs[ helio_perf_data::PERF_VALUES::ETA_TOT ]* wtmean[ helio_perf_data::PERF_VALUES::REC_ABSORPTANCE ],
		sums[ helio_perf_data::PERF_VALUES::ETA_TOT ]* wtmean[ helio_perf_data::PERF_VALUES::REC_ABSORPTANCE ],
        aves[helio_perf_data::PERF_VALUES::ETA_TOT] * wtmean[helio_perf_data::PERF_VALUES::REC_ABSORPTANCE]
	);
	
	delete [] sums;
	delete [] aves;
	delete [] stdevs;
	delete [] mins; 
	delete [] maxs;
}

void sim_result::process_flux_stats(SolarField &SF){
	//Determine the flux info
	double fave=0., fave2=0., fmax = -9.e9, fmin = 9.e9;
	int nf = 0;
	vector<Receiver*> *recs = SF.getReceivers();
	for( int i=0; i<(int)recs->size(); i++){
		FluxSurfaces *fs = recs->at(i)->getFluxSurfaces();
		for(int j=0; j<(int)fs->size(); j++){
			FluxGrid *fg = fs->at(j).getFluxMap();
			//double ascale = fs->at(j).getSurfaceArea()/atot;
			int 
				nx = fs->at(j).getFluxNX(),
				ny = fs->at(j).getFluxNY();
			double v, delta;
			for(int k=0; k<nx; k++){
				for(int l=0; l<ny; l++){
					v = fg->at(k).at(l).flux;
						
					if(v > fmax) fmax = v;
					if(v < fmin) fmin = v;

					//Estimate variance - use Knuth's one-pass method (http://en.wikipedia.org/wiki/Algorithms_for_calculating_variance) 
					nf++;
					delta = v - fave;
					fave += delta/(double)nf;
					fave2 += delta*(v-fave);

				}
			}
		}
	}
	//Calculate final variance value
	flux_density.stdev = sqrt(fave2/(double)(nf-1));
	flux_density.max = fmax;
	flux_density.min = fmin;
	flux_density.ave = fave;
}

void sim_result::process_analytical_simulation(SolarField &SF, int nsim_type, double sun_az_zen[2], Hvector &helios){
	is_soltrace = false;
	sim_type = nsim_type;

    var_map *V = SF.getVarMap();

	switch (sim_type)
	{
	case sim_result::SIM_TYPE::PARAMETRIC:
	case sim_result::SIM_TYPE::OPTIMIZATION:
	case sim_result::SIM_TYPE::LAYOUT:
	{
		//process only the ranking metric for each heliostat and the field avg. eff
		initialize();
		double effsum = 0.;
		for(unsigned int i=0; i<helios.size(); i++){
			effsum += helios.at(i)->getEfficiencyTotal();
			add_heliostat(*helios.at(i));
		}
		eff_total_sf.ave = effsum / (double)helios.size() ;
		total_receiver_area = V->sf.rec_area.Val(); //SF.calcReceiverTotalArea();
		dni = V->sf.dni_des.val/1000.; // SF.getDesignPointDNI()/1000.;
		power_on_field = total_heliostat_area * dni;	//[kW]
		power_absorbed = power_on_field * eff_total_sf.ave;

        power_thermal_loss = SF.getReceiverTotalHeatLoss();
        power_piping_loss = SF.getReceiverPipingHeatLoss();
        power_to_htf = power_absorbed - (power_thermal_loss + power_piping_loss);

		solar_az = sun_az_zen[0];
		solar_zen = sun_az_zen[1];


		break;
	}
	case sim_result::SIM_TYPE::FLUX_SIMULATION:
	{
		initialize();
		for (unsigned int i = 0; i < helios.size(); i++)
		{
			if( helios.at(i)->IsInLayout() && helios.at(i)->IsEnabled() )
				add_heliostat(*helios.at(i));
		}
		process_field_stats();
		total_receiver_area = SF.calcReceiverTotalArea();
		dni =  SF.getVarMap()->flux.flux_dni.val/1000.;
		power_on_field = total_heliostat_area * dni;	//[kW]
		power_absorbed = power_on_field * eff_total_sf.ave;
        power_thermal_loss = SF.getReceiverTotalHeatLoss();
        power_piping_loss = SF.getReceiverPipingHeatLoss();
        power_to_htf = power_absorbed - (power_thermal_loss + power_piping_loss);

		solar_az = sun_az_zen[0];
		solar_zen = sun_az_zen[1];

		//SF.getFinancialObject()->calcPlantCapitalCost(*SF.getVarMap());	//Always update the plant cost
		total_installed_cost = V->fin.total_installed_cost.Val(); //SF.getFinancialObject()->getTotalInstalledCost();
		coe_metric = total_installed_cost/_q_coe;
		
		process_flux_stats(SF);

		break;
	}
	default:
		break;
	}

}

void sim_result::process_analytical_simulation(SolarField &SF, int sim_type, double sun_az_zen[2]){  /*0=Layout, 1=Optimize, 2=Flux sim, 3=Parametric */
	process_analytical_simulation(SF, sim_type, sun_az_zen, *SF.getHeliostats());
};

void sim_result::process_raytrace_simulation(SolarField &SF, int nsim_type, double sun_az_zen[2], Hvector &helios, double qray, int *emap, int *smap, int *rnum, int ntot, double *boxinfo){
	

	is_soltrace = true;
	/* sim_type: 2=flux simulation, 3=parametric */
	initialize();
	sim_type = nsim_type;
	if(sim_type == 2){

		num_heliostats_used = (int)helios.size();
		for(int i=0; i<num_heliostats_used; i++){
			total_heliostat_area += helios.at(i)->getArea();
		}
		double dni = SF.getVarMap()->sf.dni_des.val/1000.; // SF.getDesignPointDNI()/1000.;


		//Process the ray data
		int st, st0=0, ray, ray0=0, el;
		int nhin=0, nhout=0, nhblock=0, nhabs=0, nrin=0, nrspill=0, nrabs=0;

		for(int i=0; i<ntot; i++){ //for each ray hit
			st = smap[i];	//Stage
			ray = rnum[i];	//Ray number
			el = emap[i];	//Element
			
			//Check first to see if the ray number has changed without logging data
			if((ray != ray0) && (ray0 != 0)){
				if( st0 == 1){ //Heliostat stage
					//Ray was successfully reflected out of the field
					nhin++;
					nhout++;
				}
				else{ //Receiver stage
					//Ray was lost through reflection from the receiver
					nrin++;
				}
				ray0 = 0;
				st0 = 0;
			}

			if(el < 0){
				//Ray was absorbed 
				if(st == 1){ //Heliostat
					nhin++;
					if( ray == ray0 ) //Multiple intersections within heliostat field, meaning blocking occurred
						nhblock++;
					else
						nhabs++; //Single intersection -- reflectivity loss
				}
				else{  //Receiver
					nrin++;
					nrabs++;
				}
				ray0 = 0;
				st0 = 0;
			}
			else if( el == 0 ){ //Element map is 0 - a ray missed the receiver
				nrspill++;

				ray0 = 0;
				st0 = 0;
			}
			else{
				st0 = st;
				ray0 = ray;
			}
		}

		int nsunrays = (int)boxinfo[4];
		double Abox = (boxinfo[0] - boxinfo[1])*(boxinfo[2] - boxinfo[3]);

        num_ray_traced = nsunrays;
        num_ray_heliostat = nhin;
        num_ray_receiver = nrin;

		power_on_field = total_heliostat_area *dni;	//[kW]
		power_absorbed = qray * nrabs;
        power_thermal_loss = SF.getReceiverTotalHeatLoss();
        power_piping_loss = SF.getReceiverPipingHeatLoss();
        power_to_htf = power_absorbed - (power_thermal_loss + power_piping_loss);

        eff_total_sf.set(0,0, 0, 0, 0., power_absorbed / power_on_field);
        eff_cosine.set(0.,0., 0., 0., 0., (double)nhin / (double)nsunrays*Abox / total_heliostat_area);
		eff_blocking.set(0.,0., 0., 0., 0., 1. - (double)nhblock / (double)(nhin - nhabs));
		eff_attenuation.set(0., 0., 0., 0., 0., 1.);	//Not currently accounted for
		eff_reflect.set(0., 0., 0., 0., 0., (double)(nhin - nhabs) / (double)nhin);
		eff_intercept.set(0., 0., 0., 0., 0., (double)nrin / (double)nhout);
		eff_absorption.set(0., 0., 0., 0., 0., (double)nrabs / (double)nrin);
		eff_total_heliostat.set(0., 0., 0., 0., 0., (double)nrabs / (double)nhin);
        eff_cloud.set(1., 1., 1., 0., 1., 1.);

		total_receiver_area = SF.calcReceiverTotalArea();
		solar_az = sun_az_zen[0];
		solar_zen = sun_az_zen[1];

		SF.getFinancialObject()->calcPlantCapitalCost(*SF.getVarMap());	//Always update the plant cost

		total_installed_cost = SF.getVarMap()->fin.total_installed_cost.Val(); //SF.getFinancialObject()->getTotalInstalledCost();
		coe_metric = total_installed_cost/power_absorbed;

		process_flux_stats(SF);

	}
	else{

	}


}

void sim_result::process_flux(SolarField *SF, bool normalize){
	flux_surfaces.clear();
	receiver_names.clear();
	int nr = (int)SF->getReceivers()->size();
	Receiver *rec;
	for(int i=0; i<nr; i++){
		rec = SF->getReceivers()->at(i);
		if(! rec->isReceiverEnabled() ) continue;
		flux_surfaces.push_back( *rec->getFluxSurfaces() );
		if(normalize){
			for(unsigned int j=0; j<rec->getFluxSurfaces()->size(); j++){
				flux_surfaces.back().at(j).Normalize();
			}
		}
		receiver_names.push_back( 
            //*SF->getReceivers()->at(i)->getReceiverName() 
            SF->getReceivers()->at(i)->getVarMap()->rec_name.val
            );
	}
	
}

//------parametric------------------------
multivar::multivar(){wf_are_set = false;}

parametric::parametric(){wf_are_set = false;}

int multivar::size(){ return (int)variables.size(); }

void multivar::clear(){ variables.clear(); current_varpaths.Clear();}

par_variable &multivar::at(int index){ return variables.at(index); }

par_variable &multivar::operator[](int index){ return variables.at(index); }

par_variable &multivar::back(){ return variables.back(); }

void multivar::remove(int index){ 
	variables.erase(variables.begin()+index); 
	current_varpaths.erase(current_varpaths.begin()+index); 
}

void parametric::SetWeatherFileList(ArrayString &list){
	weather_files.clear();
	for(int i=0; i<list.size(); i++){
		weather_files.push_back(list[i]);
	}
	wf_are_set = true;
}


void multivar::addVar(spbase *var)
{
	/* 
	Add a new variable to the parametric analysis. If the variable already exists, overwrite it.
	*/

    
	//check to see whether this variable already exists. If so, overwrite it
	int curind = Index(var->name);
	par_variable *vback;

	if(curind > 0){
		variables.erase(variables.begin()+curind);
		variables.insert(variables.begin()+curind, par_variable());
		vback = &variables.at(curind);
	}
	else
	{
		current_varpaths.Add(var->name);
		variables.push_back(par_variable());
		vback = &variables.back();
	}
	
	vback->varname = var->name;
	vback->display_text = split(var->name, ".").at(0) + ": "+var->short_desc;
	vback->units = var->units;
	vback->selections.clear();
	
	if(var->name == "ambient.0.weather_file"){
		if(! wf_are_set) return;
        string ts;
        var->as_string(ts);
		vback->selections.Add(ts);
		vback->data_type = "location";
		//Load the possible weather files
		vback->choices.clear();
		for(int i=0; i<weather_files.size(); i++){
			vback->choices.push_back(weather_files[i]);
		}
	}
	else if(var->ctype == "combo"){
		vback->selections.Add( var->as_string() );
		vback->data_type = "combo";
		vback->choices.clear();
        vector<string> vchoices = var->combo_get_choices();
        for(int i=0; i<(int)vchoices.size(); i++)
            vback->choices.Add(vchoices.at(i));

	}
	else if(var->ctype == "checkbox"){
		string ts;
        var->as_string(ts);
		vback->selections.Add(ts);
		vback->data_type = "checkbox";
		vback->choices.clear();
		vback->choices.push_back("true");
		vback->choices.push_back("false");
	}
	else if(var->ctype == "bool"){
        string ts;
        var->as_string(ts);
		vback->selections.Add(ts);
		vback->data_type = "bool";
		vback->choices.clear();
		vback->choices.push_back("true");
		vback->choices.push_back("false");
	}
	else if(var->ctype == "int")
    {
		string ts;
        var->as_string(ts);
		vback->selections.Add(ts);
		vback->data_type = "int";
		//int imin, imax;
		//bool withlow, withhi;
		//interop::parseRange(var->range, imin, imax, withlow, withhi);
		/*for(int i=(withlow?imin:imin+1); i<(withhi?imax:imax-1); i++){
			vback->choices.push_back( my_to_string(i) );
		}*/

	}

	else{	//doubles
		string ts;
        var->as_string(ts);
		vback->selections.Add(ts);
		vback->data_type = "double"; //var->dattype;
	}
}

int multivar::Index(string pathname){
	return current_varpaths.Index(pathname);
}

//----- user par table -------------------
ArrayString &simulation_table::operator[](const string &varname){return data[varname];}
ArrayString &simulation_table::at(const string &varname){return data[varname];}
size_t simulation_table::nvar(){return data.size();}
size_t simulation_table::nsim(){return data.size() > 0 ? data.begin()->second.size() : 0;}

void simulation_table::ClearAll(){
	for(unordered_map<string, ArrayString>::iterator it=data.begin(); it != data.end(); it++)
		it->second.Clear();
	data.clear();
}

void simulation_table::getKeys(ArrayString &keys){
	keys.clear();

	for(unordered_map<string, ArrayString>::iterator it = data.begin(); it != data.end(); it++)
		keys.push_back(it->first);
	
}
