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

#ifndef _SF_SIMTHREAD_
#define _SF_SIMTHREAD_ 1


#include "SolarField.h"

#ifdef SP_USE_THREADS
#include <thread>
#include <mutex>


class Heliostat;	//Forward declaration
class SolarField;
class WeatherData;
typedef std::vector<Heliostat*> Hvector;	//Needs declaring here


class LayoutSimThread 
{
	bool _is_user_sun_pos;		//Has the user specified sun positions? (opposed to day/time combos)
	bool _is_shadow_detail;		//Include shadowing + blocking in performance calculation?
	bool _is_flux_detail;		//Do post process for flux? (Assumes _is_shadow_detail=true)
	bool _is_flux_normalized;	//normalize the flux maps
	
	bool
		Finished,
		CancelFlag,
        FinishedWithErrors;
    
	int Nsim_complete, Nsim_total;
    std::string _thread_id;

	SolarField *_SF;
	int _sim_first, _sim_last, _sort_metric;
	WeatherData *_wdata;
	sim_results *_results;
	matrix_t<double> *_sol_azzen;
	sim_params _sim_params; 
    std::vector<std::string> _sim_messages;

	//wxMutex
	std::mutex
		StatusLock,
		CancelLock,
		FinishedLock,
        FinErrLock;

public:

	void Setup(std::string &tname, SolarField *SF, sim_results *results, WeatherData *wdata, 
		int sim_first, int sim_last, bool is_shadow_detail, bool is_flux_detail);

	void Setup(std::string &tname, SolarField *SF, sim_results *results, matrix_t<double> *sol_azzen, 
		sim_params &simpars, int sim_first, int sim_last, bool is_shadow_detail, bool is_flux_detail);

	void IsFluxmapNormalized(bool is_normal);	//set whether the fluxmap should be normalized (default TRUE)

	void CancelSimulation();

	bool IsSimulationCancelled();

	bool IsFinished();

    bool IsFinishedWithErrors();

	void UpdateStatus(int nsim_complete, int nsim_total);

	void GetStatus( int *nsim_complete, int *nsim_total);

    std::vector<std::string> *GetSimMessages();    //can be called only after simulation is terminated

	void StartThread();

};


#endif // SP_USE_THREADS

#endif
