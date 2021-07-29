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

#include "LayoutSimulateThread.h"
#include "SolarField.h"

#ifdef SP_USE_THREADS

using namespace std;
	
void LayoutSimThread::Setup(string &tname, SolarField *SF, sim_results *results, WeatherData *wdata, 
	int sim_first, int sim_last, bool is_shadow_detail, bool is_flux_detail)
{
	/* 
	Assign all of the arguments to local memory
	*/
    _thread_id = tname;
	_SF = SF;
	_results = results;
	_wdata = wdata;
	_sol_azzen = 0;
	_sim_first = sim_first;
	_sim_last = sim_last;
	Finished = false;
	CancelFlag = false;
	Nsim_complete = 0;
	Nsim_total = _sim_last - _sim_first;
	_is_user_sun_pos = false;
	_is_shadow_detail = is_shadow_detail;
	_is_flux_detail = is_flux_detail;
	_is_flux_normalized = true;
};

void LayoutSimThread::Setup(string &tname, SolarField *SF, sim_results *results, matrix_t<double> *sol_azzen, 
	sim_params &simpars, int sim_first, int sim_last, bool is_shadow_detail, bool is_flux_detail)
{
	/* 
	overload to allow specification of simulation sun positions. 
	Sun positions provided in a matrix_t
			|	Azimuth	|	Elevation
	Row		|	(rad)	|	(rad)
	--------------------------------
	1		|	az1		|	el1
	2		|	az2		|	el2
	...
	
	Args:
	args[0]	|	DNI		|	W/m2
	args[1]	|	Tdb		|	C
	args[2]	|	Vwind	|	m/s
	args[3]	|	Pres	|	bar

	*/
    _thread_id = tname;
	_SF = SF;
	_results = results;
	_wdata = 0;
	_sol_azzen = sol_azzen;
	_sim_first = sim_first;
	_sim_last = sim_last;
	Finished = false;
	CancelFlag = false;
	Nsim_complete = 0;
	Nsim_total = _sim_last - _sim_first;
	_is_user_sun_pos = true;
	_sim_params = simpars;
	_is_shadow_detail = is_shadow_detail;
	_is_flux_detail = is_flux_detail;
	_is_flux_normalized = true;
};

void LayoutSimThread::IsFluxmapNormalized(bool is_normal)
{
	_is_flux_normalized = is_normal;
}

void LayoutSimThread::CancelSimulation()
{
	CancelLock.lock();
	CancelFlag = true;
	CancelLock.unlock();
}

bool LayoutSimThread::IsSimulationCancelled()
{
	bool r;
	CancelLock.lock();
	r = CancelFlag;
	CancelLock.unlock();
	return r;
}

bool LayoutSimThread::IsFinished()
{
	bool f;
	FinishedLock.lock();
	f = Finished;
	FinishedLock.unlock();
	return f;
}

bool LayoutSimThread::IsFinishedWithErrors()
{
    bool f;
    FinErrLock.lock();
    f = FinishedWithErrors;
    FinErrLock.unlock();
    return f;
}

void LayoutSimThread::UpdateStatus(int nsim_complete, int nsim_total)
{
	StatusLock.lock();
	Nsim_complete = nsim_complete;
	Nsim_total = nsim_total;
	StatusLock.unlock();
}

void LayoutSimThread::GetStatus( int *nsim_complete, int *nsim_total)
{
	StatusLock.lock();
	*nsim_complete = this->Nsim_complete;
	*nsim_total = this->Nsim_total;
	StatusLock.unlock();
}

vector<string> *LayoutSimThread::GetSimMessages()
{
    return &_sim_messages;
}

void LayoutSimThread::StartThread() //Entry()
{
	/* 
	This method duplicates the functionality of SolarField::LayoutSimulate(...)

	This method is intended to be thread safe and can be called by the GUI directly. Each thread must have 
	its own instance of _SF. Before running multiple threads, create a solar field object, prepare it with
	PrepareFieldLayout(...), and use the deep copy constructor in SolarField to create as many duplicate
	objects as there are threads. Call this method for each duplicate object.

	*/
    try{
        
        FinErrLock.lock();
        FinishedWithErrors = false;
        FinErrLock.unlock();
        _sim_messages.clear();

	    double pi = PI;
	    //Run the simulation 
	    double dom, doy, hour, month;
	    double az, zen;
		
	    bool is_pmt_factors = _SF->getVarMap()->fin.is_pmt_factors.val;

        vector<double> *tous = &_SF->getVarMap()->fin.pricing_array.Val();

	    int Npos = (int)_SF->getHeliostats()->size();
				
	    //Simulate for each time
	    StatusLock.lock();
	    bool is_cancel = this->CancelFlag; //check for cancelled simulation
	    StatusLock.unlock();
	    if(is_cancel){
		    FinishedLock.lock();
		    Finished = true;
		    FinishedLock.unlock();
		    return; // (wxThread::ExitCode)-1;
	    }

	    if(_sim_first < 0) _sim_first = 0;
	    if(_sim_last < 0) _sim_last = _wdata->size();

	    int nsim = _sim_last - _sim_first + 1;
	    for(int i=_sim_first; i<_sim_last; i++){
		    //_SF->getSimInfoObject()->setCurrentSimulation(i+1);
		    //double args[5];
            sim_params P;
            DateTime DT;

		    //either calculate the sun position based on weather data steps, or use user-defined values
		    if(! _is_user_sun_pos){

			    //---- Calculate sun positions

			    //Get the design-point day, hour, and DNI
			    _wdata->getStep(i, dom, hour, month, P.dni, P.Tamb, P.Patm, P.Vwind, P.Simweight);
                P.Patm*=.001;

			    //Convert the day of the month to a day of year
				doy = DT.GetDayOfYear(2011,int(month),int(dom));

			    //Calculate the sun position
			    Ambient::setDateTime(DT, hour, doy);
                if(is_pmt_factors)
                    P.TOUweight = tous->at(DT.GetHourOfYear());

			    //latitude, longitude, and elevation should be set in the input file
			    Ambient::calcSunPosition(*_SF->getVarMap(), DT, &az, &zen, true );
		        //If the sun is not above the horizon, don't continue
		        if( zen > 90. )
				        continue;
		
		        az *= D2R;
                zen *= D2R;
			    
		    }
		    else{
			    //set the user-specified values
			    az = _sol_azzen->at(i,0);
			    zen = _sol_azzen->at(i,1);

			    P = _sim_params;
		    }

		    bool is_cancel;

		    StatusLock.lock();
		    is_cancel = this->CancelFlag; 
		    StatusLock.unlock();

		    /*if( (_is_shadow_detail || _is_flux_detail ) && !is_cancel)
			    interop::AimpointUpdateHandler(*_SF);
		
		    StatusLock.lock();
		    is_cancel = this->CancelFlag; 
		    StatusLock.unlock();*/

            P.is_layout = !_is_shadow_detail;

		    if(! is_cancel)
			    _SF->Simulate(az, zen, P); 

		    if((! is_cancel) && _is_flux_detail)
			    _SF->HermiteFluxSimulation( *_SF->getHeliostats() );
							
		    StatusLock.lock();
		    is_cancel = this->CancelFlag; 
		    StatusLock.unlock();

		    //store the _results
            double azzen[] = {az, zen};
		    if(! is_cancel)
			    _results->at(i).process_analytical_simulation(*_SF, _is_flux_detail ? 2 : 0, azzen); //2);
		
		    StatusLock.lock();
		    is_cancel = this->CancelFlag; 
		    StatusLock.unlock();

		    //optionally post-process the flux results as well
		    if(_is_flux_detail && !is_cancel)
			    _results->at(i).process_flux(_SF, _is_flux_normalized);

		    //Update progress
		    UpdateStatus(i-_sim_first+1,nsim);
		    //Check for user cancel
		    StatusLock.lock();
		    is_cancel = this->CancelFlag; 
		    StatusLock.unlock();
		    if(is_cancel){
			    FinishedLock.lock();
			    Finished = true;
			    FinishedLock.unlock();
			    return;
		    }			
	    }
	    FinishedLock.lock();
	    Finished = true;
	    FinishedLock.unlock();

    }
    catch(spexception &e)
    {
        /* Handle exceptions within a thread by adding the exception to a list and returning normally */
        StatusLock.lock();
		this->CancelFlag = true; 
		StatusLock.unlock();
        
        FinishedLock.lock();
		Finished = true;
		FinishedLock.unlock();
        
        FinErrLock.lock();
        FinishedWithErrors = true;
        FinErrLock.unlock();

        _sim_messages.push_back( "Thread " + this->_thread_id + ": " +  e.what() );
    }
    catch(...)
    {
        /* Handle exceptions within a thread by adding the exception to a list and returning normally */
        StatusLock.lock();
		this->CancelFlag = true; 
		StatusLock.unlock();
        
        FinishedLock.lock();
		Finished = true;
		FinishedLock.unlock();
        
        FinErrLock.lock();
        FinishedWithErrors = true;
        FinErrLock.unlock();        
        
        _sim_messages.push_back( "Thread " + this->_thread_id + ": " +  "Caught unspecified error in a simulation thread. Simulation was not successful." );
    }

	return;

};

	
#endif // SP_USE_THREADS
