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

#ifndef _SOLARFIELD_API_
#define _SOLARFIELD_API_ 1


#include "API_structures.h"
#include "definitions.h"


#if defined(__WINDOWS__)&&defined(__DLL__)
#define SPEXPORT __declspec(dllexport)
#else
#define SPEXPORT
#endif


class simulation_info;
class sim_result;
class SolarField;
class LayoutSimThread;



//-------------------------------------------------------------------------------------------------
//-------------------------------------------------------------------------------------------------

class SPEXPORT AutoPilot 
{

protected:
	bool (*_summary_callback)(simulation_info *siminfo, void *data);
	void *_summary_callback_data;
	bool (*_detail_callback)(simulation_info *siminfo, void *data);
	void *_detail_callback_data;

	SolarField *_SF;
	
	int _sim_total;
	int _sim_complete;
	
	bool _cancel_simulation;	//changing this flag to "true" will cause the current simulation to terminate
	bool _has_summary_callback;			//A callback function has been provided to the API.
	bool _has_detail_callback;
	bool _is_solarfield_external;		//Is the SolarField object provided externally? Otherwise, it will be created and destroyed locally
	bool
		_setup_ok,	//The variable structure has been created
		_simflag;	//add bool flags here to indicate simulation/setup status

    sp_optimize *_opt;

	std::vector<double> interpolate_vectors( std::vector<double> &A, std::vector<double> &B, double alpha);


	void PrepareFluxSimulation(sp_flux_table &fluxtab, int flux_res_x, int flux_res_y, bool is_normalized);
	void PostProcessLayout(sp_layout &layout);
	void PostProcessFlux(sim_result &result, sp_flux_map &fluxmap, int flux_layer = 0);
	

	bool CalculateFluxMapsOV1(std::vector<std::vector<double> > &sunpos, std::vector<std::vector<double> > &fluxtab, std::vector<double> &efficiency, 
		int flux_res_x = 12, int flux_res_y = 10, bool is_normalized = true);

	simulation_info *_summary_siminfo;
	simulation_info *_detail_siminfo;

public:
	AutoPilot();
	virtual ~AutoPilot();
	//Callbacks for progress updates
	void SetSummaryCallback( bool (*callback)(simulation_info* siminfo, void *data), void *cdata);
	void SetDetailCallback( bool (*callback)(simulation_info* siminfo, void *data), void *cdata);
	void SetSummaryCallbackStatus(bool is_enabled);
	void SetDetailCallbackStatus(bool is_enabled);
	//setup
	void PreSimCallbackUpdate();
	void SetExternalSFObject(SolarField *SF);
	bool Setup(var_map &V, bool for_optimize = false);
	//generate weather data
	void GenerateDesignPointSimulations(var_map &V, std::vector<std::string> &hourly_weather_data);
	//Simulation methods
	bool EvaluateDesign(double &obj_metric, double &flux_max, double &tot_cost);
	void PostEvaluationUpdate(int iter, std::vector<double> &pos, double &obj, double &flux, double &cost, std::string *note=0);
	virtual bool CreateLayout(sp_layout &layout, bool do_post_process = true)=0;
	virtual bool CalculateOpticalEfficiencyTable(sp_optical_table &opttab)=0;
	virtual bool CalculateFluxMaps(sp_flux_table &fluxtab, int flux_res_x = 12, int flux_res_y = 10, bool is_normalized = true)=0;
	virtual bool CalculateFluxMaps(std::vector<std::vector<double> > &sunpos, std::vector<std::vector<double> > &fluxtab, std::vector<double> &efficiency, 
		int flux_res_x = 12, int flux_res_y = 10, bool is_normalized = true)=0;
	bool Optimize(int method, std::vector<double*> &optvars, std::vector<double> &upper_range, std::vector<double> &lower_range, std::vector<double> &stepsize, std::vector<std::string> *names=0);
	bool OptimizeRSGS(std::vector<double*> &optvars, std::vector<double> &upper_range, std::vector<double> &lower_range, std::vector<bool> &is_range_constr, std::vector<std::string> *names=0);
    bool OptimizeAuto(std::vector<double*> &optvars, std::vector<double> &upper_range, std::vector<double> &lower_range, std::vector<double> &stepsize, std::vector<std::string> *names=0);
    bool OptimizeSemiAuto(std::vector<double*> &optvars, std::vector<double> &upper_range, std::vector<double> &lower_range, std::vector<bool> &is_range_constr, std::vector<std::string> *names=0);
	//cancellation methods
	void CancelSimulation();
	bool IsSimulationCancelled();
    //other
    sp_optimize *GetOptimizationObject();
    
    struct API_CANT_TYPE { enum A {NONE, ON_AXIS, EQUINOX, SOLSTICE_SUMMER, SOLSTICE_WINTER }; };
	
};


class SPEXPORT AutoPilot_S : public AutoPilot
{
	
	
public:
	//methods
	bool CreateLayout(sp_layout &layout, bool do_post_process = true);
	bool CalculateOpticalEfficiencyTable(sp_optical_table &opttab);
	bool CalculateFluxMaps(sp_flux_table &fluxtab, int flux_res_x = 12, int flux_res_y = 10, bool is_normalized = true);
	bool CalculateFluxMaps(std::vector<std::vector<double> > &sunpos, std::vector<std::vector<double> > &fluxtab, std::vector<double> &efficiency, 
		int flux_res_x = 12, int flux_res_y = 10, bool is_normalized = true);

};

#ifdef SP_USE_THREADS

class SPEXPORT AutoPilot_MT : public AutoPilot
{
	int _n_threads;	//the maximum number of threads to simulate
	int _n_threads_active;	//the number of threads currently used for simulation
	LayoutSimThread *_simthread;
	bool _in_mt_simulation;
	void CancelMTSimulation();

public:
	//constructor
	AutoPilot_MT();

	//methods
	bool CreateLayout(sp_layout &layout, bool do_post_process = true);
	bool CalculateOpticalEfficiencyTable(sp_optical_table &opttab);
	bool CalculateFluxMaps(sp_flux_table &fluxtab, int flux_res_x = 12, int flux_res_y = 10, bool is_normalized = true);
	bool CalculateFluxMaps(std::vector<std::vector<double> > &sunpos, std::vector<std::vector<double> > &fluxtab, std::vector<double> &efficiency, 
		int flux_res_x = 12, int flux_res_y = 10, bool is_normalized = true);

	//other methods
	bool SetMaxThreadCount(int nt);
	void CancelSimulation();
	
};

#endif // SP_USE_THREADS

#endif
