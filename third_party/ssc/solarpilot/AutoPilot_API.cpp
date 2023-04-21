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

#include <sstream>
#include <string>
#include <algorithm>
#include <iomanip> 

#include <nlopt.hpp>

#include "AutoPilot_API.h"
#include "LayoutSimulateThread.h"
#include "IOUtil.h"
#include "SolarField.h"
#include "definitions.h"
#include "mod_base.h"

#ifdef SP_USE_THREADS
#include <thread>
#endif


using namespace std;


class response_surface_data
{
public:
	int N_vars;
	int Order;
	vector<vector<int> > generators;
	vector<double> Y;
	vector<vector<double> > X;
	vector<double> cur_pos;
	vector<double> Beta;
	int ncalls;
	double max_step_size;

	response_surface_data(){
		ncalls = 0;
	}

	int CalcNumberBetas(){
		//calculate the number of BETA terms
		int nbeta = 0;
		for(int i=N_vars+1; i>0; i--)
			nbeta += i;
		return nbeta;
	};

	double EvaluateBiLinearResponse(vector<double> &xpt)
	{
		/* 
		Evaluate a function of the form
		F(x) = beta0 + beta1 * x1 + beta2 * x2 .. + betaN * x1 * x2 + ... + betaP x1^2 ...

		Only cross-terms up to bi-linear form (e.g. x1 * x2) will be included. Quadratic terms
		are also included.
		*/

		
		//is Beta ok?
		/*if(Beta.size() != nbeta)
			Beta.resize(nbeta, 1.);*/

		double yret = 0.;
		int ib=0;
		for(int i=0; i<N_vars+1; i++){
			double xi = i==0 ? 1. : xpt.at(i-1);
			for(int j=i; j<N_vars+1; j++){
				double xj = j==0 ? 1. : xpt.at(j-1);
				yret += xi * xj * Beta.at(ib++);
			}
		}

		return yret;
	};
	
	void AddGenerator(vector<vector<int> > &/*design*/, int a, int b = 0, int c = 0, int d = 0, int e = 0)
	{
		vector<int> newgen;
		if(a>0) newgen.push_back(a);
		if(b>0) newgen.push_back(b);
		if(c>0) newgen.push_back(c);
		if(d>0) newgen.push_back(d);
		if(e>0) newgen.push_back(e);
	}

	void GenerateSurfaceEvalPoints( vector<double> &point, vector<vector<double> > &sim_points, double tolerance)
	{
		/* 
		Take a current point (vector of doubles) that is normalized, and calculate a design of experiments run that 
		would produce a linear estimate of the local surface. Parameters are varied up/down by the tolerance. The
		runs required are returned in the "sim_points" array.

		2-level fractional factorial design information is from Wu & Hamada (2000) "Experiments" Appendix 4a Tab 4A.3
		*/
	
		int nvars = (int)point.size();

		sim_points.clear();

		//allow up to a 32-run table
		int nruns = (int)pow(2., min(nvars,5));

		//values that determine when each variable toggles value
		vector<int> divisors;
		for(int i=0; i<nvars; i++)
			divisors.push_back( (int)pow(2., i) );
		//create a design with either +1 or -1 as high/low value
		vector<vector<int> > design;
		design.push_back( vector<int>(nvars, 1) );
	
		//Create the base table
		for(int i=1; i<nruns; i++){
			vector<int> newline(nvars, 0);
			for(int j=0; j<min(nvars,5); j++)
				newline.at(j) =  design.at(i-1).at(j) * (int)(fmod((double)i, (double)divisors.at(j) ) == 0 ? -1 : 1 ) ;
			design.push_back(newline);
		}
	
		//if there are more than 5 variables, create a fractional factorial design with 32 runs
		generators.clear();
		switch(nvars)
		{
		default:
			if(nvars > 16)
				throw spexception("Optimization is supported for up to 16 independent variables. "
				"Please reduce the number of optimization variables and try again.");
			break;

		case 6:
			AddGenerator( generators, 1,2,3,4,5 );
			break;
		case 7:
			AddGenerator( generators, 1,2,3 );
			AddGenerator( generators, 1,2,4,5 );
			break;
		case 8:
			AddGenerator( generators, 1,2,3 );
			AddGenerator( generators, 1,2,4 );
			AddGenerator( generators, 1,3,4,5 );
			break;
		case 9:
			AddGenerator( generators, 1,2,3 );
			AddGenerator( generators, 1,2,4 );
			AddGenerator( generators, 1,2,5 );
			AddGenerator( generators, 1,3,4,5 );
			break;
		case 10:
			AddGenerator( generators, 1,2,3 );
			AddGenerator( generators, 1,2,4 );
			AddGenerator( generators, 1,2,5 );
			AddGenerator( generators, 1,3,4,5 );
			AddGenerator( generators, 2,3,4,5 );
			break;
		case 11:
		case 12:
		case 13:
		case 14:
		case 15:
		case 16:
			AddGenerator( generators, 1,2,3 );
			AddGenerator( generators, 1,2,4 );
			AddGenerator( generators, 1,3,4 );
			AddGenerator( generators, 2,3,4 );
			AddGenerator( generators, 1,2,5 );
			AddGenerator( generators, 1,3,5 );
			if(nvars == 11) break;
			AddGenerator( generators, 2,3,5 );
			if(nvars == 12) break;
			AddGenerator( generators, 1,4,5 );
			if(nvars == 13) break;
			AddGenerator( generators, 2,4,5 );
			if(nvars == 14) break;
			AddGenerator( generators, 3,4,5 );
			if(nvars == 15) break;
			AddGenerator( generators, 1,2,3,4,5 );
			break;

		}

		//Use the design generators to fill in additional variable columns, if needed
		for(int j=5; j<nvars; j++){
			for(int i=0; i<nruns; i++){

				int val = 1;
				for(int k=0; k<(int)generators.at(j-5).size(); k++)
					val *= design.at(i).at( generators.at(j-5).at(k) - 1 );
				design.at(i).at(j) = val;
			
			}
		}

		//Add central composite runs
		for(int i=0; i<nvars; i++){
			vector<int> row(nvars, 0);
			row.at(i) = 1;
			design.push_back(row);
			row.at(i) = -1;
			design.push_back(row);
		}
		nruns = (int)design.size();

		//create the sim_points array with actual non-dimensionalized simulation values rather than integer surrogates
		for(int i=0; i<nruns; i++){
			sim_points.push_back( vector<double>(nvars) );
			for(int j=0; j<nvars; j++){
				sim_points.at(i).at(j) = point.at(j) * (1. + tolerance * (double)design.at(i).at(j) );
			}
		}
		
	}

};

struct _aof_inst 
{ 
    double obj; 
    double flux; 
	_aof_inst(double o, double f){
        obj=o; flux=f; 
    }; 
	_aof_inst(){};
};

struct AutoOptHelper
{
    int m_iter;
    AutoPilot *m_autopilot;
    vector<vector<double> > m_all_points;
    //vector<double> m_normalizers;
    vector<double> m_objective;
    vector<double> m_flux;
    vector<double*> m_opt_vars;
    vector<string> m_opt_names;
    nlopt::opt *m_opt_obj;
    var_map *m_variables;

    class {
        unordered_map<std::string, _aof_inst> items;

        std::string format(std::vector<double> vars)
        {
            stringstream buf;
            for(int i=0; i<(int)vars.size(); i++)
                buf << setw(8) << vars.at(i) << ",";
            return buf.str();
        };

    public:
        void add_call(std::vector<double> vars, double objective, double flux)
        {
            items[ format(vars) ] = _aof_inst(objective, flux);
        };

        bool check_call(std::vector<double> vars, double* obj, double* flux)
        {
            std::string hash = format(vars);
            if( items.find( hash ) == items.end() )
                return false;

            *obj = items[ hash ].obj;
            *flux = items[ hash ].flux;

            return true;
        };

        size_t size(){
            return items.size();
        };

    } m_history_map;

    void SetObjects( void *autopilot, var_map &V, nlopt::opt *optobj ){
        m_autopilot = static_cast<AutoPilot*>( autopilot );
        m_variables = &V;
        m_opt_obj = optobj;
    };

    void Initialize()
    {
        m_iter = 0;
        m_autopilot = 0;
        m_opt_obj = 0;
        m_all_points.clear();
        //m_normalizers.clear();
        m_objective.clear();
        m_flux.clear();
        m_opt_vars.clear();
        m_opt_names.clear();
    };

    double Simulate(const double *x, int /*n*/, std::string *note=0)
    {
        /* 
        Run a simulation and update points as needed. Report outcome from each step.
        */
        if(m_autopilot->IsSimulationCancelled() )
        {
            m_opt_obj->force_stop();
            return 0.;
        }

        m_iter += 1;

        vector<double> current;

        //update the objective variables
        for(int i=0; i<(int)m_opt_vars.size(); i++)
        {
            current.push_back( x[i] );
            *m_opt_vars.at(i) = current.at(i)/* * m_normalizers.at(i)*/;
        }

        m_all_points.push_back( current );

        double obj, flux, cost;
        
        //Evaluate the objective function value
        if(! 
        m_autopilot->EvaluateDesign( obj, flux, cost) 
            ){
            string errmsg = "Optimization failed at iteration " + my_to_string(m_iter) + ". Terminating simulation.";   
            throw spexception(errmsg.c_str());
        }
        //Update variables as needed
        m_autopilot->PostEvaluationUpdate(m_iter, current/*, m_normalizers*/, obj, flux, cost, note);

        m_objective.push_back(obj);
        m_flux.push_back(flux);
        m_history_map.add_call( current, obj, flux );
                
        return obj;
    };
};

double optimize_leastsq_eval(unsigned n, const double *x, double * /*grad*/, void *data)
{
	/* 
	Evaluate the residual sum of squares
	*/
	
	response_surface_data *D = static_cast<response_surface_data*>( data );
	D->ncalls ++;
	//We are solving for the Beta coefficients (contained in X)
	if(D->Beta.size() != n){
		D->Beta.resize(n,1.);
	}

	for(unsigned i=0; i<n; i++)
		D->Beta.at(i) = x[i];

	double ssres=0.;

	for(int i=0; i<(int)D->X.size(); i++){
		double y = D->EvaluateBiLinearResponse(D->X.at(i));
		double ssrv = (y - D->Y.at(i));
		ssres += ssrv * ssrv;	//residual sum of squares
	}

	return ssres; 

};

double optimize_stdesc_eval(unsigned n, const double *x, double * /*grad*/, void *data)
{
	/* 
	Minimize the response surface value subject to a maximum step size.
	*/

	response_surface_data *D = static_cast<response_surface_data*>(data);
	D->ncalls ++;
	vector<double> xpt;
	//double ssize = 0.;
	for(unsigned i=0; i<n; i++){
		xpt.push_back(x[i]);
		//double xistep = x[i] - D->cur_pos.at(i);
		//D->cur_pos.at(i) = x[i];

		//ssize += xistep*xistep;
	}
	//ssize = sqrt(ssize);

	//return D->EvaluateBiLinearResponse( xpt ) + abs(ssize - D->max_step_size)*D->big_M;
	return D->EvaluateBiLinearResponse( xpt );

};

double optimize_maxstep_eval(unsigned n, const double *x, double * /*grad*/, void *data)
{
	
	response_surface_data *D = static_cast<response_surface_data*>(data);

	vector<double> xpt;
	double ssize = 0.;
	for(unsigned i=0; i<n; i++){
		xpt.push_back(x[i]);
		double xistep = x[i] - D->cur_pos.at(i);
		//D->cur_pos.at(i) = x[i];

		ssize += xistep*xistep;
	}
	ssize = sqrt(ssize);
	return ssize - D->max_step_size;

}

double optimize_auto_eval(unsigned n, const double *x, double * /*grad*/, void *data)
{
    AutoOptHelper *D = static_cast<AutoOptHelper*>( data );
    //Only calls to methods available in AutoPilot base class are allowed!

    return D->Simulate(x, n);
};


double constraint_auto_eval(unsigned n, const double *x, double * /*grad*/, void *data)
{
    AutoOptHelper *D = static_cast<AutoOptHelper*>( data );
    
    std::vector<double> vars;
    for(int i=0; i<(int)n; i++)
        vars.push_back( x[i] );
    double obj, flux;
    if( D->m_history_map.check_call( vars, &obj, &flux ) )
    {
        /*vector<double> empty;
        std::string note = ">>>>   Constraint point";
        D->m_autopilot->PostEvaluationUpdate(-1, empty, empty, flux, D->m_variables->recs.front().peak_flux.val, obj, &note);*/
        return flux - D->m_variables->recs.front().peak_flux.val;
    }
    else
    {
        std::string comment = " >> Checking flux constraint";
        D->Simulate(x, n, &comment);
        return D->m_flux.back() - D->m_variables->recs.front().peak_flux.val;
    }
};


AutoPilot::AutoPilot()
{
	_has_summary_callback = false;
	_has_detail_callback = false;
	_is_solarfield_external = false;
	_SF = 0;
	_summary_callback = 0;
	_detail_callback = 0;
	_summary_callback_data = 0;
	_detail_callback_data = 0;
	_summary_siminfo = 0;
	_detail_siminfo = 0;
    _opt = new sp_optimize();
}

AutoPilot::~AutoPilot()
{
	if( _SF != 0 ){
		//quietly try to delete the solar field
		try{
			delete _SF;
		}
		catch(...){}
	}

	if( _summary_siminfo != 0)
	{
		//quietly try to delete the simulation info object
		try{
			delete _summary_siminfo;
		}
		catch(...){}
	}

    if( _opt != 0 )
        delete _opt;

	return;
}
//
//bool AutoPilot::CreateLayout(sp_layout &/*layout*/, bool /*do_post_process*/)
//{
//	
//	//override in inherited class
//	throw spexception("Virtual method cannot be called directly! Use derived class AutoPilot_S or AutoPilot_MT instead.");
//	return false;
//};
//
//bool AutoPilot::CalculateOpticalEfficiencyTable(sp_optical_table &/*opttab*/)
//{
//	//override in inherited class
//	throw spexception("Virtual method cannot be called directly! Use derived class AutoPilot_S or AutoPilot_MT instead.");
//	return false;
//};
//
//bool AutoPilot::CalculateFluxMaps(sp_flux_table &/*fluxtab*/, int /*flux_res_x*/, int /*flux_res_y*/, bool /*is_normalized*/)
//{
//	//override in inherited class
//	throw spexception("Virtual method cannot be called directly! Use derived class AutoPilot_S or AutoPilot_MT instead.");
//	return false;
//};

void AutoPilot::SetSummaryCallback( bool (*callback)(simulation_info* siminfo, void *data), void *cdata)
{
	_has_summary_callback = true;
	_summary_callback = callback;
	_summary_callback_data = cdata;
}

void AutoPilot::SetDetailCallback( bool (*callback)(simulation_info* siminfo, void *data), void *cdata)
{
	_has_detail_callback = true;
	_detail_callback = callback;
	_detail_callback_data = cdata;
}

void AutoPilot::SetSummaryCallbackStatus(bool is_enabled)
{
	_has_summary_callback = is_enabled;
}

void AutoPilot::SetDetailCallbackStatus(bool is_enabled)
{
	_has_detail_callback = is_enabled;
	if(_SF != 0)
		_SF->getSimInfoObject()->isEnabled(is_enabled);
}

void AutoPilot::SetExternalSFObject( SolarField *SF )
{
	_SF = SF;
	_is_solarfield_external = true;
}

bool AutoPilot::Setup(var_map &V, bool /*for_optimize*/)
{

	/* 
	Using the information provided in the data structures, construct a new SolarField object.
	Structures are declared in "API_structures.h"

	Any messages can be provided back through the optional 'messages' vector, if provided.

	Returns:
	True	-	no errors setting up the field
	False	-	errors setting up the field
	*/
	_cancel_simulation = false;	

	//-----------------------------------------------------------------
	//	Alter settings according to what's provided in the structures
	//-----------------------------------------------------------------

    //need to set up the template combo
    //V.sf.temp_which.combo_clear();
    //std::string name = "Template 1", val = "0";
    //V.sf.temp_which.combo_add_choice(name, val);
    //V.sf.temp_which.combo_select_by_choice_index( 0 ); //use the first heliostat template

	//Dynamically allocate the solar field object, if needed
	if(! _is_solarfield_external ){
		_SF = new SolarField();
	}

	//---Set a couple of parameters here that should be consistent for simple API use
	
	//make sure the aiming strategy is correct
	//if(V.recs.front().rec_type.val == Receiver::REC_TYPE::CYLINDRICAL && !for_optimize)
	//	V.flux.aim_method.val =  FluxSimData::AIM_STRATEGY::SIMPLE ;
	//else
 //   {
	//	V.flux.aim_method.val =  FluxSimData::AIM_STRATEGY::IMAGE_SIZE ;
 //       V.flux.sigma_limit_y.val = 2.5;
 //   }

	////set the receiver flux surfaces to the correct resolution to balance run time with accuracy
	//if( V.recs[0].rec_type.val == 0 ){
	//	//external receiver
	//	V.flux.x_res.val = 12;
	//	V.flux.y_res.val = 20;
	//
	//}
	//else{
	//	//flat plate receiver
	//	V.flux.x_res.val = 15;
	//	V.flux.y_res.val = 15;
	//}
	
	//Create the solar field object
	_SF->Create(V);

	//if a layout is provided in the sp_layout structure, go ahead and create the geometry here.
    if( ! V.sf.layout_data.val.empty() )
    {
		_SF->PrepareFieldLayout(*_SF, 0, true);	//Run the layout method in refresh_only mode
        Vect sun = Ambient::calcSunVectorFromAzZen( _SF->getVarMap()->sf.sun_az_des.Val()*D2R, (90. - _SF->getVarMap()->sf.sun_el_des.Val())*D2R );   
		_SF->calcHeliostatShadows(sun);
        double area = V.land.land_area.Val();  //acre
		V.land.bound_area.Setval( area );
        V.land.land_area.Setval( area );
	}

	
	PreSimCallbackUpdate();
	_setup_ok = true;
	
	return true;
}


vector<double> AutoPilot::interpolate_vectors(vector<double> &A, vector<double> &B, double alpha)
{
	/* 
	Find the point between A and B, where alpha=0 --> A, and alpha=1 --> B
	*/
	if(A.size() != B.size())
		throw spexception("Error (interpolate_vectors): vectors must have the same dimension.");
	
	vector<double> V;
	for(int i=0; i<(int)A.size(); i++)
		V.push_back(A.at(i) + (B.at(i) - A.at(i))*alpha);

	return V;

}

void AutoPilot::GenerateDesignPointSimulations(var_map &V, vector<string> &wdata)
{
	/* 
	Generate the design simulation hours using the SolarPILOT macros. Options must be specified
	directly in the 'variables' data structure. If options are not changed, the default settings
	will be used to generate the simulations (recommended).

	wdata
	------
	This data structure contains a list of all weather entries for the weather file. Each entry is 
	a string formatted with comma separation between weather items. Formatting is:
	day, hour, month,  dni, tdry, pres, wspd
	1..,  0..,  1-12, W/m2,    C,  bar,  m/s
	*/
    
	//amb.weather_data.clear();
	interop::GenerateSimulationWeatherData(V, -1, wdata);	

    /*WeatherData *w = &V.sf.sim_step_data.Val();

    for(int i=0; i<w->size(); i++)
        amb.AddWeatherStep((int)w->Day[i], (int)w->Month[i], w->Hour[i], w->DNI[i], w->T_db[i], w->V_wind[i], w->Pres[i], w->Step_weight[i] );*/

	//vector<double> stepdat;
	//vector<string> sim_step_data = split(variables["solarfield"][0]["sim_step_data"].value, "[P]");
	//for(int i=0; i<(int)sim_step_data.size(); i++){
	//	vector<string> sdata = split(sim_step_data.at(i), ",");
	//	vector<double> fdata;
	//	for(int j=0; j<(int)sdata.size(); j++){
	//		double tt; 
	//		to_double(sdata.at(j), &tt);
	//		fdata.push_back(tt);
	//	}
	//	//add each step
	//	amb.AddWeatherStep((int)fdata.at(0), (int)fdata.at(2), fdata.at(1), fdata.at(3), 
	//		fdata.at(4), fdata.at(6), fdata.at(5), fdata.at(7));
	//}

}

void AutoPilot::PreSimCallbackUpdate()
{
	//pass the callback along to the solar field, if applicable
	if(_has_detail_callback){
		_detail_siminfo = _SF->getSimInfoObject();
		_SF->getSimInfoObject()->setCallbackFunction(_detail_callback, _detail_callback_data);
		_SF->getSimInfoObject()->isEnabled(true);
	}
	if(_has_summary_callback){
		if(! _summary_siminfo )
			_summary_siminfo = new simulation_info();
		_summary_siminfo->ResetValues();
		_summary_siminfo->setCallbackFunction(_summary_callback, _summary_callback_data);
	}
}

void AutoPilot::PostProcessLayout(sp_layout &layout)
{
	/* 
	Layout post-process.. collect the layout results and fill the data into the
	layout structure for later use
	*/


	Hvector *hpos = _SF->getHeliostats();
	layout.heliostat_positions.clear();
	for(int i=0; i<(int)hpos->size(); i++){
		sp_layout::h_position hp;
		hp.location.x = hpos->at(i)->getLocation()->x;
		hp.location.y = hpos->at(i)->getLocation()->y;
		hp.location.z = hpos->at(i)->getLocation()->z;

		hp.cant_vector.i = hpos->at(i)->getCantVector()->i;
        hp.cant_vector.j = hpos->at(i)->getCantVector()->j;
        hp.cant_vector.k = hpos->at(i)->getCantVector()->k;
		
        hp.aimpoint.x = hpos->at(i)->getAimPoint()->x;
		hp.aimpoint.y = hpos->at(i)->getAimPoint()->y;
		hp.aimpoint.z = hpos->at(i)->getAimPoint()->z;

		hp.focal_length = hpos->at(i)->getFocalX();
		hp.template_number = -1;
		//hp.user_optics = false;
		layout.heliostat_positions.push_back( hp );
	}


    var_map *V = _SF->getVarMap();
    _SF->updateAllCalculatedParameters( *V );

	//_layout->land_area = V->land.land_area.Val(); // _SF->getLandObject()->getLandArea() /4046.85642;  //m2->acre
 //   _layout->area_sf = V->sf.sf_area.Val();

 //   _cost->cost_rec_tot = V->fin.rec_cost.Val(); //f->getReceiverCost(); 
 //   _cost->cost_tower_tot = V->fin.tower_cost.Val(); //f->getTowerCost(); 
 //   _cost->cost_land_tot = V->fin.land_cost.Val(); //f->getLandCost(); 
 //   _cost->cost_heliostat_tot = V->fin.heliostat_cost.Val(); // f->getHeliostatCost(); 
 //   _cost->cost_site_tot = V->fin.site_cost.Val(); // f->getSiteCost(); 
 //   _cost->cost_plant_tot = V->fin.plant_cost.Val(); // f->getPlantCost(); 
 //   _cost->cost_tes_tot = V->fin.tes_cost.Val(); // f->getTESCost(); 
 //   _cost->cost_fossil_tot = 0.;
 //   _cost->cost_salestax_tot = V->fin.sales_tax_cost.Val(); // f->getSalesTaxCost();
 //   _cost->cost_direct_tot = V->fin.total_direct_cost.Val(); // f->getTotalDirectCost();
 //   _cost->cost_epc_tot = 0.;
 //   _cost->cost_indirect_tot = V->fin.total_indirect_cost.Val(); //f->getTotalIndirectCost();
 //   _cost->cost_installed_tot = V->fin.total_installed_cost.Val(); // f->getTotalInstalledCost();
    
}

void AutoPilot::PrepareFluxSimulation(sp_flux_table &fluxtab, int flux_res_x, int flux_res_y, bool /*is_normalized*/)
{
	var_map *V = _SF->getVarMap();
    V->amb.sim_time_step.Setval(0.);    //sest the simulation time step for flux

    //simulate flux maps for all of the receivers
	vector<Receiver*> rec_to_sim = *_SF->getReceivers();
	//Get flags and settings
	
	if(flux_res_y > 1)
        V->flux.aim_method.combo_select_by_mapval( var_fluxsim::AIM_METHOD::IMAGE_SIZE_PRIORITY );

	//Shape the flux surface files to match
	for(unsigned int i=0; i<rec_to_sim.size(); i++){
		rec_to_sim.at(i)->DefineReceiverGeometry(flux_res_x, flux_res_y);	//Flux map should match spec
	}
	
	//------------ 
    int nflux_sim;

    if(fluxtab.azimuths.size() == 0)
    {
	    vector<int> uday;
	    vector<vector<double> > utime;
	    //Ambient *amb =_SF->getAmbientObject();

	    if(! fluxtab.is_user_spacing){
		    fluxtab.n_flux_days = 8;
		    fluxtab.delta_flux_hrs = 1.;
	    }

	    Ambient::calcSpacedDaysHours(V->amb.latitude.val, V->amb.longitude.val, V->amb.time_zone.val, 
		    fluxtab.n_flux_days, fluxtab.delta_flux_hrs, utime, uday);
	 
	    nflux_sim = 0;
	    for(int i=0; i<(int)utime.size(); i++)
		    nflux_sim += (int)utime.at(i).size();

	    //Arrays to keep track of input values
	    fluxtab.azimuths.clear();
	    fluxtab.zeniths.clear();

        DateTime DT;
        int nday = (int)uday.size();
        for(int i=0; i<nday; i++){
	        int nhour_day = (int)utime.at(i).size();
	        for(int j=0; j<nhour_day; j++){
			
                Ambient::setDateTime(DT, utime.at(i).at(j)+12, uday[i]);
                double az,zen;
                Ambient::calcSunPosition(*V, DT, &az, &zen);
			
		        //--- keep track of input values
		        fluxtab.azimuths.push_back(az*D2R);
		        fluxtab.zeniths.push_back(zen*D2R);
	        }
        }
    }
    else
    {
        nflux_sim = (int)fluxtab.azimuths.size();
    }

	fluxtab.flux_surfaces.clear();
	//resize the results to accommodate each receiver surface
	int nsurftot=0;
	for(int i=0; i<(int)_SF->getReceivers()->size(); i++){
		for(int j=0; j<(int)_SF->getReceivers()->at(i)->getFluxSurfaces()->size(); j++){
			nsurftot ++;
		}
	}
	fluxtab.flux_surfaces.resize(nsurftot);
	//resize the flux surfaces to match the flux data and the number of annual simulation positions
	for(int i=0; i<nsurftot; i++)
		fluxtab.flux_surfaces.at(i).flux_data.resize(flux_res_y, flux_res_x, nflux_sim);
}

void AutoPilot::PostProcessFlux(sim_result &result, sp_flux_map &fluxmap, int flux_layer)
{
	if(! _cancel_simulation){
				
		int itot=0;
		vector<Receiver*> *Recs = _SF->getReceivers();
		int nrec = (int)Recs->size();
		for(int irec = 0; irec<nrec; irec++){
			//how many surfaces on this receiver?
			Receiver *rec = Recs->at(irec);

			int nrecsurf = (int)rec->getFluxSurfaces()->size();
			for( int isurf=0; isurf<nrecsurf; isurf++){
				//transfer data from the result flux map to the sp_flux_table data structure
				fluxmap.flux_surfaces.at(itot).map_name = rec->getVarMap()->rec_name.val + " surface " + my_to_string(isurf+1);
				FluxSurface *fs = &result.flux_surfaces.at(irec).at(isurf);
				int 
					nflux_x = fs->getFluxNX(),
					nflux_y = fs->getFluxNY();
				FluxGrid *fmap = result.flux_surfaces.at(irec).at(isurf).getFluxMap(); 
				for(int fluxi=0; fluxi<nflux_y; fluxi++){
					for(int fluxj = 0; fluxj < nflux_x; fluxj++){
						//pointers for convenience
						sp_flux_table::sp_flux_stack *fstack = &fluxmap.flux_surfaces.at(itot);
						FluxPoint* fpt = &fmap->at(fluxj).at(nflux_y - fluxi - 1);
						//transfer flux
						fstack->flux_data.at(fluxi, fluxj, flux_layer) = fpt->flux;
						//transfer location
						fstack->xpos.push_back( fpt->location.x );
						fstack->ypos.push_back( fpt->location.y );
					}
				}
					
				itot++;
			}
		}	
	}
}

void AutoPilot::CancelSimulation()
{
	_cancel_simulation = true;
	_SF->CancelSimulation();
}



bool AutoPilot::EvaluateDesign(double &obj_metric, double &flux_max, double &tot_cost)
{
	/* 
	Create a layout and evaluate the optimization objective function value with as little 
	computation as possible. This method is called by the optimization algorithm.

	The 'obj_metric' is evaluated and set in this algorithm. If the simulation fails, the method 
	returns FALSE.
	*/

    var_map *V = _SF->getVarMap();

	//create the solar field object
	if(! _cancel_simulation)
    {
		_SF->Create(*V);	
        if(_SF->ErrCheck()){return false;}
	}
	//Do the layout simulation
	if(! _cancel_simulation)
    {
        sp_layout layout;  //dummy
		if(! CreateLayout(layout, false) )
        {
            CancelSimulation();
            obj_metric = 0.;
            flux_max = 0.;
            return false;
        }
		if(_SF->ErrCheck()){return false;}
	}
	//Do the flux simulation at the design point
	if(! _cancel_simulation){
		//update the flux simulation sun position to match the layout reference point sun position
		//_SF->getVarMap()->flux.flux_time_type.val = var_fluxsim::FLUX_TIME_TYPE::SUN_POSITION;	//sun position specified
        _SF->getVarMap()->flux.flux_time_type.combo_select_by_mapval( var_fluxsim::FLUX_TIME_TYPE::SUN_POSITION );

		//prep for performance simulation (aim points, etc.)
		interop::PerformanceSimulationPrep(*_SF, *_SF->getHeliostats(), 0 /*analytical*/);
		
		//do flux simulation
		_SF->HermiteFluxSimulation( *_SF->getHeliostats(), V->flux.aim_method.mapval() == var_fluxsim::AIM_METHOD::IMAGE_SIZE_PRIORITY);	
		if(_SF->ErrCheck()){return false;}		
	}
	
	//get the annual optical power estimate
	double optical_power = _SF->getAnnualPowerApproximation();

	//get the total plant cost
	tot_cost = V->fin.total_installed_cost.Val();
	
	//Get the maximum flux value
	flux_max=0.;
	for(int i=0; i<(int)_SF->getReceivers()->size(); i++){
		for(int j=0; j<(int)_SF->getReceivers()->at(i)->getFluxSurfaces()->size(); j++){
			double ff = _SF->getReceivers()->at(i)->getFluxSurfaces()->at(j).getMaxObservedFlux();
			if( ff > flux_max )
				flux_max = ff;
		}
	}

	//check to make sure we're producing enough power, otherwise we'll have to penalize
	double qminimum = _SF->getDesignThermalPowerWithLoss();
	double qactual = _SF->getActualThermalPowerWithLoss();
	double power_shortage_ratio = min(qactual/qminimum, 1.);

	//Set the optimization objective value
	//double flux_overage_ratio = max(flux_max/V->recs.front().peak_flux.val, 1.);

	obj_metric = tot_cost/ optical_power *1.e6 //$/MWh
		* (1. + (1. - power_shortage_ratio) * V->opt.power_penalty.val);

	return true;
}

bool AutoPilot::Optimize(int /*method*/, vector<double*> &optvars, vector<double> &upper_range, vector<double> &lower_range, vector<double> &stepsize, vector<string> *names)
{
	/* 
	
	Optimize
	
	*/
    

    //switch(method)     //"BOBYQA=0;COBYLA=1;NEWOUA=2;Nelder-Mead=3;Subplex=4;RSGS=5"
    //{
    //case 5:  //Response surface gradient search - original method
	   // return OptimizeRSGS(optvars, upper_range, lower_range, is_range_constr);
    //    break;
    //case 1: //COBYLA with separate bound constraint
        return OptimizeAuto( optvars, upper_range, lower_range, stepsize, names);        
    //default:
        //return OptimizeSemiAuto( optvars, upper_range, lower_range, is_range_constr, names);
    //}

}

bool AutoPilot::OptimizeRSGS(vector<double*> &optvars, vector<double> &upper_range, vector<double> &lower_range, vector<bool> &/*is_range_constr*/, vector<string> *names)
{
	//Number of variables to be optimized
	int nvars = (int)optvars.size();
	//Store the initial dimensional value of each variable
	/*vector<double> normalizers;
	for(int i=0; i<nvars; i++)
		normalizers.push_back( *optvars.at(i) );*/
	
	//the initial normalized point is '1'
	vector<double> current(nvars, 1.);

	bool converged = false;
	int opt_iter = 0;

    int sim_count = 0;

	vector<vector<double> > all_sim_points; //keep track of all of the positions simulated
	vector<double> objective;
	vector<double> max_flux;
	vector<double> tot_costs;
	double objective_old=9.e22;
	double objective_new=9.e21;
	int sim_count_begin = 0;
    double converge_tol = _SF->getVarMap()->opt.converge_tol.val;
    double max_step =  _SF->getVarMap()->opt.max_step.val;

    //Add a formatted simulation notice
    ostringstream os;
    os << "\n\nBeginning Simulation\nIter ";
    for(int i=0; i<(int)optvars.size(); i++)
        os << setw(9) << (names==0 ? "Var "+my_to_string(i+1) : names->at(i)) << "|";
    os << "| Obj.    | Flux    | Plant cost";

	_summary_siminfo->addSimulationNotice(os.str());
	while( ! converged ){
		sim_count_begin = (int)objective.size() - 1;	//keep track of the simulation number at the beginning of the main iteration

		//Choose the current point to the the best of all simulations in the previous iteration
		if(opt_iter > 0){
			double zbest = 9.e23;
			int ibest = sim_count_begin;
			for(int i=sim_count_begin; i<(int)objective.size(); i++){
				if(objective.at(i) < zbest){
					zbest = objective.at(i);
					ibest = i;
				}
			}
			current = all_sim_points.at(ibest);
		}

		//----------------------
		//	Current point
		//----------------------

		//Start iteration by evaluating the current point
		_summary_siminfo->addSimulationNotice("--- Iteration " + my_to_string(opt_iter+1) + " ---\n...Simulating base point");
		for(int i=0; i<(int)optvars.size(); i++)
			*optvars.at(i) = current.at(i) /** normalizers.at(i)*/;
		all_sim_points.push_back( current );
		double base_obj, base_flux, cost;
		EvaluateDesign(base_obj, base_flux, cost);			
		PostEvaluationUpdate(sim_count++, current, /*normalizers, */base_obj, base_flux, cost);
		if(_cancel_simulation) return false;
		objective.push_back( base_obj );
		max_flux.push_back( base_flux );
		tot_costs.push_back( cost );
		vector<double> surface_objective;
		vector<vector<double> > surface_eval_points;
		surface_objective.push_back( base_obj );
		surface_eval_points.push_back( current );
		//-- 

		//Check to see if no further improvement has been made in the objective function
		objective_new = base_obj;
		if( (objective_old - objective_new)/objective_old < converge_tol ){
			converged = true;

			//Find the best point simulated and return that
			double zbest = 9.e23;
			int ibest = (int)objective.size()-1;	//initialize as the last value just in case
			for(int i=0; i<(int)objective.size(); i++){
				if( objective.at(i) < zbest ){
					zbest = objective.at(i);
					ibest = i;
				}
			}
			for(int i=0; i<(int)optvars.size(); i++)
				*optvars.at(i) = all_sim_points.at(ibest).at(i) /** normalizers.at(i)*/;
			
			break;
		}
		objective_old = objective_new;


		//----------------------
		//	Response surface
		//----------------------
		response_surface_data Reg;

		//Generate the set of points required for the response surface characterization
		vector<vector<double> > runs;
		Reg.GenerateSurfaceEvalPoints( current, runs, max_step );

		//Run the evaluation points
		_summary_siminfo->setTotalSimulationCount((int)runs.size());
		if(! _summary_siminfo->addSimulationNotice("...Creating local response surface") ){
            CancelSimulation();
            return false;
        }
		for(int i=0; i<(int)runs.size(); i++){
            if(! _summary_siminfo->setCurrentSimulation(i) ){
                CancelSimulation();
                return false;
            }

			//update the data structures
			for(int j=0; j<(int)optvars.size(); j++)
				*optvars.at(j) = runs.at(i).at(j) /** normalizers.at(j)*/;
			
			//Evaluate the design
			double obj, flux, cost;
			all_sim_points.push_back( runs.at(i) );
			EvaluateDesign(obj, flux, cost);
			PostEvaluationUpdate(sim_count++, runs.at(i)/*, normalizers*/, obj, flux, cost);
			if(_cancel_simulation) return false;
			surface_objective.push_back(obj);
			surface_eval_points.push_back( runs.at(i) );
			objective.push_back( obj);
			max_flux.push_back(flux);
		    tot_costs.push_back( cost );
		}

		//construct a bilinear regression model
		_summary_siminfo->addSimulationNotice("...Generating regression fit");
		
		//----------------------
		//	Descent vector
		//----------------------

		Reg.N_vars = nvars;
		Reg.Y = surface_objective;
		Reg.X = surface_eval_points;
		int nbeta = Reg.CalcNumberBetas();
		Reg.Beta.resize( nbeta, 1.);
		//calculate the average Y value
		double yave = 0.;
		for(int i=0; i<(int)Reg.Y.size(); i++)
			yave += Reg.Y.at(i);
		yave *= 1./(double)Reg.Y.size();
		//Set the first beta (the constant) to the average as  an initial guess
		Reg.Beta.front() = yave;
		
		nlopt::opt surf(nlopt::LN_NELDERMEAD, nbeta);	//with higher iteration limits, NELDER MEAD does very well compared to sbplex and cobyla
		surf.set_min_objective( optimize_leastsq_eval, &Reg);
		surf.set_xtol_rel(1.e-7);
		surf.set_ftol_rel(1.e-7);
		surf.set_maxtime(5.);
		
		double min_ss;
		try{
			surf.optimize(Reg.Beta, min_ss);
		}
		catch( std::exception &e ){
			_summary_siminfo->addSimulationNotice( e.what() );
			return false;
		}

		//calculate total sum of squares
		double sstot = 0.;
		for(int i=0; i<(int)Reg.Y.size(); i++){
			double ssval = Reg.Y.at(i) - Reg.Beta.front();
			sstot += ssval * ssval;
		}
		//report the coefficient of determination
		_summary_siminfo->addSimulationNotice("... r^2 = " + my_to_string(1.-min_ss/sstot) );
		
		//now we have a response surface described by BETA coefficients. we need to choose the steepest descent
		Reg.ncalls = 0;
		Reg.max_step_size = max_step;
		//nlopt::opt steep(nlopt::LN_COBYLA, nvars);		//optimize with constraint on step size - use COBYLA
		nlopt::opt steep(nlopt::GN_ESCH, nvars);
		steep.set_min_objective( optimize_stdesc_eval, &Reg);
		steep.set_maxtime(2.);
		//add an inequality constraint to find the minimum within a maximum step
		//steep.add_inequality_constraint( optimize_maxstep_eval, &Reg, 1.e-3);
		//add range constraints for the variables
		vector<double>
			range_max, range_min;
		for(int i=0; i<nvars; i++){
			range_max.push_back( fmin(upper_range.at(i), current.at(i) + max_step) );
			range_min.push_back( fmax(lower_range.at(i), current.at(i) - max_step) );
		}

		steep.set_upper_bounds( range_max );
		steep.set_lower_bounds( range_min );

		steep.set_xtol_rel(1.e-4);
		steep.set_ftol_rel(1.e-5);
		
		Reg.cur_pos = current;
		vector<double> stepto(current);
		double min_val;
		try{
			steep.optimize(stepto, min_val);
		}
		catch( std::exception &e )
		{
			_summary_siminfo->addSimulationNotice( e.what() );
			return false;
		}
		
		//Calculate the vector from the current point to the point we'd like to step to
		vector<double> step_vector(stepto);
		for(int i=0; i<(int)step_vector.size(); i++)
			step_vector.at(i) += -current.at(i);


		//is the surface regression optimum value better than the best simulated point in the factorial analyis?
		double best_fact_obj = 9.e9;
		int i_best_fact = 0;
		for(int i=0; i<(int)surface_objective.size(); i++){
			if( surface_objective.at(i) < best_fact_obj ){
				best_fact_obj = surface_objective.at(i);
				i_best_fact = i;
			}
		}
		_summary_siminfo->addSimulationNotice("...Best regression objective value = " + my_to_string(min_val) ); 

		//if so, use the best simulated point
		if(best_fact_obj < min_val ){
			_summary_siminfo->addSimulationNotice("...Correcting step direction to use best response surface point.");

			step_vector.resize( stepto.size() );
			for(int i=0; i<(int)current.size(); i++){
				step_vector.at(i) = surface_eval_points.at(i_best_fact).at(i) - current.at(i);
			}
			//correct the step to maintain the maximum step size
			double step_size = 0.;
			for(int i=0; i<(int)step_vector.size(); i++)
				step_size += step_vector.at(i) * step_vector.at(i);
			step_size = sqrt(step_size);
			for(int i=0; i<(int)step_vector.size(); i++)
				step_vector.at(i) *= Reg.max_step_size / step_size;
			//update the minimum value to be the value at the best point
			min_val = best_fact_obj;
		}


		//Check to see whether the projected minimum is significantly better than the current point
		double checktol = (base_obj - min_val)/base_obj;
		if(fabs(checktol) < converge_tol){
			_summary_siminfo->addSimulationNotice(
				"\nConvergence in the objective function value has been achieved. Final step variation: "
				+ my_to_string(checktol) );
			converged = true;
			break;
		}

		
		//----------------------
		//	Steepest descent
		//----------------------

		//move in max steps along the steepest descent vector until the objective function begins to increase
		_summary_siminfo->ResetValues();
		int minmax_iter = 0;
		bool steep_converged = false;
		double prev_obj = base_obj;
        int max_desc_iter = _SF->getVarMap()->opt.max_desc_iter.val;

		_summary_siminfo->setTotalSimulationCount(max_desc_iter);
		_summary_siminfo->addSimulationNotice("...Moving along steepest descent");
		
		vector<double> start_point = current;
		vector<double> all_steep_objs;
		bool tried_steep_mod = false;
		for(;;)
        {
            if(! _summary_siminfo->setCurrentSimulation(minmax_iter) ){
                CancelSimulation();
                return false;
            }

			//update the variable values
			for(int i=0; i<(int)optvars.size(); i++){
				current.at(i) += step_vector.at(i);
				*optvars.at(i) = current.at(i) /** normalizers.at(i)*/;
			}

			//Evaluate the design
			double obj, flux, cost;
			all_sim_points.push_back( current );
			EvaluateDesign(obj, flux, cost);
			PostEvaluationUpdate(sim_count++, current, /*normalizers,*/ obj, flux, cost);
			if(_cancel_simulation) return false;
			if(minmax_iter > 0)
				prev_obj = objective.back();	//update the latest objective function value
			objective.push_back( obj );
			all_steep_objs.push_back( obj );
			max_flux.push_back( flux );
		    tot_costs.push_back( cost );

			minmax_iter++;
			if(minmax_iter >= max_desc_iter)
				break;

			//break here if the objective has increased
			if(obj > prev_obj){
				if(! tried_steep_mod){
					//did the steepest descent at least do better than any of the response surface simulations?
					double best_steep_obj = 9.e9;
					for(int i=0; i<(int)all_steep_objs.size(); i++)
						if( all_steep_objs.at(i) < best_steep_obj ) best_steep_obj = all_steep_objs.at(i);
					if(best_fact_obj < best_steep_obj)
                    {
						//Calculate a new step vector
						vector<double> new_step_vector( step_vector );

						//go back and try to move in the factorial point direction
						double new_step_size = 0.;
						for(int i=0; i<(int)step_vector.size(); i++){
							double ds = surface_eval_points.at(i_best_fact).at(i) - start_point.at(i);
							new_step_vector.at(i) = ds;
							new_step_size += ds * ds;
						}
						new_step_size = sqrt(new_step_size);

						//check to make sure the new step vector is different from the previous
						double step_diff = 0.;
						for(int i=0; i<(int)step_vector.size(); i++){
							double ds = new_step_vector.at(i) - step_vector.at(i);
							step_diff += ds * ds;
						}
						if( sqrt(step_diff) > max_step/100. && new_step_size > 1.e-8){
							tried_steep_mod = true;
							_summary_siminfo->addSimulationNotice("...Moving back to original point, trying alternate descent direction.");
						
							//correct the step to maintain the maximum step size
							step_vector = new_step_vector;
							double step_size = 0.;
							for(int i=0; i<(int)step_vector.size(); i++)
								step_size += step_vector.at(i) * step_vector.at(i);
							step_size = sqrt(step_size);
							for(int i=0; i<(int)step_vector.size(); i++)
								step_vector.at(i) *= Reg.max_step_size / step_size;
					
							//move back to the starting point
							current = start_point;
							obj = base_obj;
							prev_obj = base_obj;
							continue;
						}
												
					}
				}

				//is the overall steepest descent loop converged?
				if(fabs(obj/prev_obj - 1.) < converge_tol)
					steep_converged = true;
				//Move the current point back to the lowest value
				current = all_sim_points.at( all_sim_points.size() - 2);
				break;
			}

			//break if the step size is very small
			double step_mag = 0.;
			for(int i=0; i<(int)step_vector.size(); i++)
				step_mag += step_vector.at(i) * step_vector.at(i);
			step_mag = sqrt(step_mag);
			if(step_mag < max_step/10.){
				steep_converged = true;
				break;
			}
			
		}
		


		//did we manage to converge the steepest descent in the inner loop? If so, skip the golden section refinement.
		if(steep_converged){
			opt_iter++;
			if( opt_iter >= _SF->getVarMap()->opt.max_iter.val )
				break;
			continue;
		}

		/* 
		Now we have isolated the approximate bottom region of the steepest descent curve. Do a golden section in 
		this region to find the true minimum.
		*/
		double golden_ratio = 1./1.61803398875;
		//double alpha = 0.;
		int nsimpts = (int)all_sim_points.size();
		vector<double>
				lower_gs = all_sim_points.at( nsimpts - 1 - min( 2, minmax_iter ) ),
				upper_gs = all_sim_points.back(),
				site_a_gs, site_b_gs;
		
		_summary_siminfo->setTotalSimulationCount(_SF->getVarMap()->opt.max_gs_iter.val*2);
		_summary_siminfo->addSimulationNotice("...Refining with golden section");

		bool site_a_sim_ok = false;
		bool site_b_sim_ok = false;
		double za = 0., zb = 0.;

		for(int gsiter=0; gsiter<_SF->getVarMap()->opt.max_gs_iter.val; gsiter++)
		{
            if(! _summary_siminfo->setCurrentSimulation(gsiter*2) ){
                CancelSimulation();
                return false;
            }

			//lower and upper points for golden section
			site_a_gs = interpolate_vectors(lower_gs, upper_gs, 1. - golden_ratio);
			site_b_gs = interpolate_vectors(lower_gs, upper_gs, golden_ratio);
				
			double obj, flux, cost;
			//Evaluate at the lower point
			if(! site_a_sim_ok ){
				current = site_a_gs;
				for(int i=0; i<(int)optvars.size(); i++)
					*optvars.at(i) = current.at(i) /** normalizers.at(i)*/;
				all_sim_points.push_back( current );
				EvaluateDesign(obj, flux, cost);			
				PostEvaluationUpdate(sim_count++, current, /*normalizers,*/ obj, flux, cost);
				if(_cancel_simulation) return false;
				za = obj;
				objective.push_back( obj );
				max_flux.push_back( flux );
		        tot_costs.push_back( cost );
			}

            if(! _summary_siminfo->setCurrentSimulation(gsiter*2 + 1) ){
                CancelSimulation();
                return false;
            }

			//Evaluate at the upper point
			if(! site_b_sim_ok){
				current = site_b_gs;
				for(int i=0; i<(int)optvars.size(); i++)
					*optvars.at(i) = current.at(i) /** normalizers.at(i)*/;
				all_sim_points.push_back( current );
				EvaluateDesign(obj, flux, cost);			
				PostEvaluationUpdate(sim_count++, current, /*normalizers,*/ obj, flux, cost);
				if(_cancel_simulation) return false;
				zb = obj;
				objective.push_back( obj );
				max_flux.push_back( flux );
		        tot_costs.push_back( cost );
			}

			//if there's no difference between the two objective functions, don't keep iterating
			if( fabs((za - zb)/za) < _SF->getVarMap()->opt.converge_tol.val)
				break;

			//Decide how to shift the bounds
			if( gsiter == _SF->getVarMap()->opt.max_gs_iter.val -1 ) break;
			if( za > zb ){
				lower_gs = site_a_gs;
				//the lower bound moves up and site a becomes the old site b location
				site_a_sim_ok = true;
				site_b_sim_ok = false;
				za = zb;
			}
			else{
				upper_gs = site_b_gs;
				//the upper bound moves down and site b becomes the old site a location
				site_a_sim_ok = false;
				site_b_sim_ok = true;
				zb = za;
			}
				
		}
		if(_cancel_simulation) return false;
		//update the current point
		current = za < zb ? site_a_gs : site_b_gs;
		opt_iter++;
		if( opt_iter >= _SF->getVarMap()->opt.max_iter.val )
			break;
	}
	
	if(_cancel_simulation) return false;

	////redimensionalize the simulation points
	//for(int i=0; i<(int)all_sim_points.size(); i++){
	//	for(int j=0; j<(int)normalizers.size(); j++){
	//		all_sim_points.at(i).at(j) *= normalizers.at(j);
	//	}
	//}

	_summary_siminfo->ResetValues();


	//Choose the current point to the the best of all simulations in the previous iteration
	double zbest = 9.e23;
	vector<double> best_point;
	int ibest = (int)objective.size()-1;
	for(int i=0; i<(int)objective.size(); i++){
		if(objective.at(i) < zbest){
			zbest = objective.at(i);
			ibest = i;
		}
	}
	best_point = all_sim_points.at(ibest);
	_summary_siminfo->addSimulationNotice("\nBest point found:");
	vector<double> ones(best_point.size(), 1.);
	PostEvaluationUpdate(sim_count++, best_point,/* ones, */zbest, max_flux.at(ibest), tot_costs.at(ibest));

	_summary_siminfo->addSimulationNotice("\n\nOptimization complete!");
	

	//copy the optimization data to the optimization structure
    vector<vector<double> > dimsimpt;
    size_t nr = all_sim_points.size();
    size_t nc = all_sim_points.front().size();

    for(size_t i=0; i<nr; i++){
        if( nc == 0 ) break;
        vector<double> tmp;
        for(size_t j=0; j<nc; j++){
            tmp.push_back( all_sim_points.at(i).at(j) /** normalizers.at(j)*/ );
        }
        dimsimpt.push_back(tmp);
    }
	_opt->setOptimizationSimulationHistory(dimsimpt, objective, max_flux);

	return true;

}

bool AutoPilot::OptimizeAuto(vector<double*> &optvars, vector<double> &upper_range, vector<double> &lower_range, vector<double> &stepsize, vector<string> *names)
{
    /* 
    Use canned algorithm to optimize
    */

    
    //set up NLOPT algorithm
   
    var_map *V = _SF->getVarMap();

    //this always uses COBYLA to handle arbitrary bound constraints
    nlopt::algorithm nlm = nlopt::LN_COBYLA; 

    //flux max is enforced by constraint, so temporarily suspend it
    double flux_penalty_save = V->opt.flux_penalty.val;
    V->opt.flux_penalty.val = 0.;

    nlopt::opt nlobj(nlm, (unsigned int)optvars.size() );
    
    //Create optimization helper class
    AutoOptHelper AO;
    AO.Initialize();
    AO.SetObjects( (void*)this,  *V, &nlobj);
    AO.m_opt_vars = optvars;
    //-------
    nlobj.set_min_objective( optimize_auto_eval, &AO  );
    nlobj.set_xtol_rel(1.e-4);
    nlobj.set_ftol_rel(V->opt.converge_tol.val);
    nlobj.set_initial_step( stepsize );
    nlobj.set_maxeval( V->opt.max_iter.val );

    nlobj.set_lower_bounds(lower_range);
    nlobj.set_upper_bounds(upper_range);
    
    //constraint
    nlobj.add_inequality_constraint( constraint_auto_eval, &AO, 0. );

    //Number of variables to be optimized
	int nvars = (int)optvars.size();
	//Store the initial dimensional value of each variable
	/*for(int i=0; i<nvars; i++)
		AO.m_normalizers.push_back( *optvars.at(i) );*/
	
    //the initial normalized point is '1'
	vector<double> start(nvars);
    for(int i=0; i<(int)optvars.size(); i++)
        start.at(i) = *optvars.at(i);

    //Check feasibility
    unsigned int iht = (unsigned int)(std::find(names->begin(), names->end(), "receiver.0.rec_height") - names->begin());
    if( iht < names->size() )
    {
        double *xtemp = new double[ optvars.size() ]; 
        for(int i=0; i<(int)optvars.size(); i++)
            xtemp[i] = 1.;
        AO.Simulate(xtemp, (int)optvars.size());
        delete [] xtemp;
        double feas_mult = 1.;
        if( AO.m_flux.back() > V->recs.front().peak_flux.val )
        {
            feas_mult += (AO.m_flux.back() / V->recs.front().peak_flux.val - 1. )*3.;
            start.at(iht) *= feas_mult;
            _summary_siminfo->addSimulationNotice( "Modifying initial receiver height for feasibility" );
        }
    }
    	
    
    //Add a formatted simulation notice
    ostringstream os;
    os << "\n\nBeginning Simulation\nIter ";
    for(int i=0; i<(int)optvars.size(); i++)
        os << setw(9) << (names==0 ? "Var "+my_to_string(i+1) : names->at(i)) << "|";
    os << "| Obj.    | Flux    | Plant cost";

    string hmsg = os.str();

    string ol;
    for(int i=0; i<(int)hmsg.size(); i++)
        ol.append("-");

    _summary_siminfo->addSimulationNotice( os.str() );
    _summary_siminfo->addSimulationNotice( ol.c_str() );

    double fmin;
    try{
       nlobj.optimize( start, fmin );
        _summary_siminfo->addSimulationNotice( ol.c_str() );
        
        //int iopt = 0;
        int iopt = (int)AO.m_objective.size()-1;
        /*double objbest = 9.e9;
        for(int i=0; i<(int)AO.m_all_points.size(); i++){
            double obj = AO.m_objective.at(i);
            if( obj < objbest ){
                objbest = obj;
                iopt = i;
            }
        }*/

        //write the optimal point found
        ostringstream oo;
        oo << "Algorithm converged:\n";
        for(int i=0; i<(int)optvars.size(); i++)
            oo << (names == 0 ? "" : names->at(i) + "=" ) << setw(8) << AO.m_all_points.at(iopt).at(i) /** AO.m_normalizers.at(i)*/ << "   ";
        oo << "\nObjective: " << AO.m_objective.back(); //objbest;
        _summary_siminfo->addSimulationNotice(oo.str() );
    }
    catch(...){
        V->opt.flux_penalty.val = flux_penalty_save;
        return false;
    }

    //copy the optimization data to the optimization structure
    vector<vector<double> > dimsimpt;
    size_t nr = AO.m_all_points.size();
    size_t nc = AO.m_all_points.front().size();

    for(size_t i=0; i<nr; i++){
        if( nc == 0 ) break;
        vector<double> tmp;
        for(size_t j=0; j<nc; j++){
            tmp.push_back( AO.m_all_points.at(i).at(j) /** AO.m_normalizers.at(j)*/ );
        }
        dimsimpt.push_back(tmp);
    }
    _opt->setOptimizationSimulationHistory( dimsimpt, AO.m_objective, AO.m_flux );

    V->opt.flux_penalty.val = flux_penalty_save; //reset
    return true;
}


bool AutoPilot::OptimizeSemiAuto(vector<double*> &optvars, vector<double> &/*upper_range*/, vector<double> & /*lower_range*/, vector<bool> & /*is_range_constr*/, vector<string> *names)
{
    /* 
    Use canned algorithm to optimize
    */

    
    //set up NLOPT algorithm
   
    var_map *V = _SF->getVarMap();

    //map the method
    nlopt::algorithm nlm;
    switch(V->opt.algorithm.mapval())
    {
    //case 0: //sp_optimize::METHOD::BOBYQA:
    case var_optimize::ALGORITHM::BOBYQA:
        nlm = nlopt::LN_BOBYQA;
        break;
    //case 1: //sp_optimize::METHOD::COBYLA:
    case var_optimize::ALGORITHM::COBYLA:
        nlm = nlopt::LN_COBYLA;
        break;
    //case 2: //sp_optimize::METHOD::NelderMead:
    case var_optimize::ALGORITHM::NELDERMEAD:
        nlm = nlopt::LN_NELDERMEAD;
        break;
    //case 3: //sp_optimize::METHOD::NEWOUA:
    case var_optimize::ALGORITHM::NEWOUA:
        nlm = nlopt::LN_NEWUOA;
        break;
    //case 4: //sp_optimize::METHOD::Subplex:
    case var_optimize::ALGORITHM::SUBPLEX:
        nlm = nlopt::LN_SBPLX;
        break;
    default:
        nlm = (nlopt::algorithm)-1;
    }

    int tot_max_iter = V->opt.max_iter.val;
    int step_max_iter = tot_max_iter / 3;
    V->opt.max_iter.val = step_max_iter;   //reset at end of run
    double flux_penalty_save = V->opt.flux_penalty.val;

    int iter_counter = 0;
    //------- first optimize the tower height without any flux penalty -------------
    {
        nlopt::opt nlobj(nlm, 1 );
        vector<double*> towvar;
        towvar.push_back(optvars.front());
        V->opt.flux_penalty.val = 0.;

        //Create optimization helper class
        AutoOptHelper AO;
        AO.Initialize();
        AO.SetObjects( (void*)this,  *V, &nlobj);
        AO.m_opt_vars = towvar;
        //-------
        nlobj.set_min_objective( optimize_auto_eval, &AO  );
        nlobj.set_xtol_rel(1.e-4);
        nlobj.set_ftol_rel(V->opt.converge_tol.val);
        nlobj.set_initial_step( vector<double>( 1, V->opt.max_step.val ) );

        nlobj.set_maxeval( V->opt.max_iter.val );

	    //Store the initial dimensional value of each variable
		//AO.m_normalizers.push_back( *towvar.front() );
	
	    //the initial normalized point is '1'
	    vector<double> start(1, 1.);
    
        //Add a formatted simulation notice
        ostringstream os;
        os << "\n\nOptimizing Tower Height\nIter ";
        os << setw(9) << (names==0 ? "Var 1" : names->front()) << "|";
        os << "| Obj.    | Flux    | Plant cost";

        string hmsg = os.str();

        string ol;
        for(int i=0; i<(int)hmsg.size(); i++)
            ol.append("-");

        _summary_siminfo->addSimulationNotice( os.str() );
        _summary_siminfo->addSimulationNotice( ol.c_str() );

        double fmin;
        try{
           nlobj.optimize( start, fmin );
            _summary_siminfo->addSimulationNotice( ol.c_str() );
        
            double objbest = 9.e9;
            for(int i=0; i<(int)AO.m_all_points.size(); i++){
                double obj = AO.m_objective.at(i);
                if( obj < objbest ){
                    objbest = obj;
                }
            }
            iter_counter += (int)AO.m_all_points.size();

        }
        catch(...)
        {
            //reset
            V->opt.max_iter.val = tot_max_iter;   
            V->opt.flux_penalty.val = flux_penalty_save;
            return false;
        }

        V->opt.flux_penalty.val = flux_penalty_save;
    }


    //-------- using optimal tower height, now optimize receiver dimensions -------------


    {
        vector<double*> recvars;
        recvars.push_back(optvars.at(1));
        recvars.push_back(optvars.at(2));
        
        nlopt::opt nlobj(nlm, (unsigned int)recvars.size() );

        //Create optimization helper class
        AutoOptHelper AO;
        AO.Initialize();
        AO.m_iter = iter_counter;
        AO.SetObjects( (void*)this,  *V, &nlobj);
        AO.m_opt_vars = recvars;
        //-------
        nlobj.set_min_objective( optimize_auto_eval, &AO  );
        nlobj.set_xtol_rel(1.e-4);
        nlobj.set_ftol_rel(V->opt.converge_tol.val);
        nlobj.set_initial_step( vector<double>( recvars.size(), V->opt.max_step.val ) );
        nlobj.set_maxeval( V->opt.max_iter.val );

        //Number of variables to be optimized
	    int nvars = (int)recvars.size();
	    //Store the initial dimensional value of each variable
	    /*for(int i=0; i<nvars; i++)
		    AO.m_normalizers.push_back( *recvars.at(i) );*/
	
	    //the initial normalized point is '1'
	    vector<double> start(nvars, 1.);
    
        //Add a formatted simulation notice
        ostringstream os;
        os << "**Optimizing Receiver Dimensions at THT="<< *optvars.front() << "[m]\nIter ";
        for(int i=0; i<(int)recvars.size(); i++)
            os << setw(9) << (names==0 ? "Var "+my_to_string(i+1) : names->at(i+1)) << "|";
        os << "| Obj.    | Flux    | Plant cost";

        string hmsg = os.str();

        string ol;
        for(int i=0; i<(int)hmsg.size(); i++)
            ol.append("-");

        _summary_siminfo->addSimulationNotice( os.str() );
        _summary_siminfo->addSimulationNotice( ol.c_str() );

        double fmin;
        try{
            nlobj.optimize( start, fmin );
            _summary_siminfo->addSimulationNotice( ol.c_str() );
        
            double objbest = 9.e9;
            for(int i=0; i<(int)AO.m_all_points.size(); i++){
                double obj = AO.m_objective.at(i);
                if( obj < objbest ){
                    objbest = obj;
                }
            }
            iter_counter += (int)AO.m_all_points.size();
        }
        catch(...){
            //reset
            V->opt.max_iter.val = tot_max_iter;   
            return false;
        }

    }
    

    //--------- co-optimize all 3 variables ---------------------------------------------
    {
        V->opt.max_iter.val = step_max_iter + (tot_max_iter % 3);   //allow any extra runs here


        nlopt::opt nlobj(nlm, (unsigned int)optvars.size() );
    
        //Create optimization helper class
        AutoOptHelper AO;
        AO.Initialize();
        AO.m_iter = iter_counter;
        AO.SetObjects( (void*)this,  *V, &nlobj);
        AO.m_opt_vars = optvars;
        //-------
        nlobj.set_min_objective( optimize_auto_eval, &AO  );
        nlobj.set_xtol_rel(1.e-4);
        nlobj.set_ftol_rel(V->opt.converge_tol.val);
        nlobj.set_initial_step( vector<double>( optvars.size(), V->opt.max_step.val ) );
        nlobj.set_maxeval( V->opt.max_iter.val );

        //Number of variables to be optimized
	    int nvars = (int)optvars.size();
	    //Store the initial dimensional value of each variable
	    /*for(int i=0; i<nvars; i++)
		    AO.m_normalizers.push_back( *optvars.at(i) );*/
	
	    //the initial normalized point is '1'
	    vector<double> start(nvars, 1.);
    
        //Add a formatted simulation notice
        ostringstream os;
        os << "**Co-optimizing geometry\nIter ";
        for(int i=0; i<(int)optvars.size(); i++)
            os << setw(9) << (names==0 ? "Var "+my_to_string(i+1) : names->at(i)) << "|";
        os << "| Obj.    | Flux    | Plant cost";
        
        string hmsg = os.str();

        string ol;
        for(int i=0; i<(int)hmsg.size(); i++)
            ol.append("-");

        _summary_siminfo->addSimulationNotice( os.str() );
        _summary_siminfo->addSimulationNotice( ol.c_str() );

        double fmin;
        try{
            nlobj.optimize( start, fmin );
            _summary_siminfo->addSimulationNotice( ol.c_str() );
        
            int iopt = 0;
            double objbest = 9.e9;
            for(int i=0; i<(int)AO.m_all_points.size(); i++){
                double obj = AO.m_objective.at(i);
                if( obj < objbest ){
                    objbest = obj;
                    iopt = i;
                }
            }

            //write the optimal point found
            ostringstream oo;
            oo << "Best point found:\n";
            for(int i=0; i<(int)optvars.size(); i++)
                oo << (names == 0 ? "" : names->at(i) + "=" ) << setw(8) << AO.m_all_points.at(iopt).at(i) /** AO.m_normalizers.at(i)*/ << "   ";
            oo << "\nObjective: " << objbest;
            _summary_siminfo->addSimulationNotice(oo.str() );
        }
        catch(...){
            //reset
            V->opt.max_iter.val = tot_max_iter;   
            return false;
        }

        //copy the optimization data to the optimization structure
        vector<vector<double> > dimsimpt;
        size_t nr = AO.m_all_points.size();
        size_t nc = AO.m_all_points.front().size();

        for(size_t i=0; i<nr; i++){
            if( nc == 0 ) break;
            vector<double> tmp;
            for(size_t j=0; j<nc; j++){
                tmp.push_back( AO.m_all_points.at(i).at(j) /** AO.m_normalizers.at(j)*/ );
            }
            dimsimpt.push_back(tmp);
        }
        _opt->setOptimizationSimulationHistory( dimsimpt, AO.m_objective, AO.m_flux );
    }
    //reset
    V->opt.max_iter.val = tot_max_iter;   

    return true;
}



bool AutoPilot::IsSimulationCancelled()
{
	return _cancel_simulation;
}

sp_optimize *AutoPilot::GetOptimizationObject()
{
    return _opt;
}

void AutoPilot::PostEvaluationUpdate(int iter, vector<double> &pos, /*vector<double> &normalizers, */double &obj, double &flux, double &cost, std::string *note)
{
	ostringstream os;
    os << "[" << setw(2) << iter << "] ";
    for(int i=0; i<(int)pos.size(); i++)
        os << setw(8) << pos.at(i) /** normalizers.at(i)*/ << " |";

    os << "|" << setw(8) << obj << " |" << setw(8) << flux << " | $" << setw(8) << cost;

    if( note != 0 )
        os << *note;

    _summary_siminfo->addSimulationNotice( os.str() );

}

bool AutoPilot::CalculateFluxMapsOV1(vector<vector<double> > &sunpos, vector<vector<double> > &fluxtab, vector<double> &efficiency, int flux_res_x, int flux_res_y, bool is_normalized)
{
	/* 
	overload to provide the flux data in a simple 2D vector arrangement. Each flux map is provided 
	in a continuous sequence, and it is up to the user to separate out the data based on knowledge
	of the number of flux maps and dimension of each flux map.
	*/

	//Call the main algorithm
	sp_flux_table fluxtab_s;
	if(! CalculateFluxMaps(fluxtab_s, flux_res_x, flux_res_y, is_normalized) )
		return false;

	block_t<double> *flux_data = &fluxtab_s.flux_surfaces.front().flux_data;

	//convert data structure
	fluxtab.clear();
	efficiency.clear();
	for(int i=0; i<(int)flux_data->nlayers(); i++){
		sunpos.push_back( vector<double>(2) );
		sunpos.back().at(0) = fluxtab_s.azimuths.at(i);
		sunpos.back().at(1) = fluxtab_s.zeniths.at(i);
		efficiency.push_back( fluxtab_s.efficiency.at(i) );

		for(int j=0; j<flux_res_y; j++){
			vector<double> newline;
			for(int k=0; k<flux_res_x; k++){
				newline.push_back(flux_data->at(j, k, i));
			}
			fluxtab.push_back( newline );
		}
	}

	return true;
}

//bool AutoPilot::CalculateFluxMaps(vector<vector<double> > & /*sunpos*/, vector<vector<double> > & /*fluxtab*/, vector<double> & /*efficiency*/, int /*flux_res_x*/, int /*flux_res_y*/, bool /*is_normalized*/)
//{
//	//override in inherited class
//	throw spexception("Virtual method cannot be called directly! Use derived class AutoPilot_S or AutoPilot_MT instead.");
//	return false;
//}

//---------------- API_S --------------------------
bool AutoPilot_S::CreateLayout(sp_layout &layout, bool do_post_process)
{
	/* 
	Create a layout using the variable structure that has been created
	*/
	_cancel_simulation = false;
	PreSimCallbackUpdate();

	//int nsim_req = _SF->calcNumRequiredSimulations();
	//if(! _SF->isSolarFieldCreated()){
		//throw spexception("The solar field Create() method must be called before generating the field layout.");
	//}
	if(! _cancel_simulation){
		bool simok = _SF->FieldLayout();			
        
        if(_SF->ErrCheck() || !simok) return false;
	}
	if(do_post_process){
		if(! _cancel_simulation)
        {
            Vect sun = Ambient::calcSunVectorFromAzZen( _SF->getVarMap()->sf.sun_az_des.Val()*D2R, (90. - _SF->getVarMap()->sf.sun_el_des.Val())*D2R );   

			_SF->calcHeliostatShadows(sun);	if(_SF->ErrCheck()){return false;}
        }
		if(! _cancel_simulation)
			PostProcessLayout(layout);
	}

	return true;

}

bool AutoPilot_S::CalculateOpticalEfficiencyTable(sp_optical_table &opttab)
{
	_cancel_simulation = false;
	PreSimCallbackUpdate();

	//set the solar positions to calculate
	int neff_az;
	int neff_zen;
	
	if(! opttab.is_user_positions){
		//set the solar positions for calculation to the default values
		neff_az = 12;
		opttab.azimuths.clear();
		double eff_az[] = {0.,  30.,  60.,  90., 120., 150., 180., 210., 240., 270., 300., 330.};
		for(int i=0; i<neff_az; i++)
			opttab.azimuths.push_back(eff_az[i]);
		
		neff_zen = 8;
		opttab.zeniths.clear();
		double eff_zen[] = {0.50,   7.,  15.,  30.,  45.,  60.,  75.,  85.};
		for(int i=0; i<neff_zen; i++)
			opttab.zeniths.push_back(eff_zen[i]);
	}
	else{
		neff_az = (int)opttab.azimuths.size();
		neff_zen = (int)opttab.zeniths.size();
	}

	double dni = _SF->getVarMap()->sf.dni_des.val;
	//double args[] = {dni, 25., 1., 0.};		//DNI, Tdb, Pamb, Vwind
    sim_params P;
    P.dni = dni;
    P.Tamb = 25.;
	
	int neff_tot = neff_az * neff_zen;
	
	_sim_total = neff_tot;	//set the total simulation counter

	if(_has_summary_callback){
		_summary_siminfo->ResetValues();
		_summary_siminfo->setTotalSimulationCount(_sim_total);
		_summary_siminfo->addSimulationNotice("Simulating optical efficiency points");
	}
	
	sim_results results;
	results.resize(neff_tot);
	string neff_tot_str = my_to_string(neff_tot);
	int k=0;
	for(int j=0; j<neff_zen; j++){
		for(int i=0; i<neff_az; i++){
			//update the progress counter
			_sim_complete = k;
			
			if(_has_summary_callback)
				if( ! 
					_summary_siminfo->setCurrentSimulation(_sim_complete) 
					) 
					CancelSimulation();
			
			//Update the solar position
			//if(! _cancel_simueclation)
				//_SF->getAmbientObject()->setSolarPosition((opttab.azimuths.at(i)-180.)*D2R, opttab.zeniths.at(j)*D2R);	
			//Update the aim points and images based on the new solar position and tracking angles
			//if(! _cancel_simulation)
				//interop::AimpointUpdateHandler(*_SF);
            double azzen[2];
            azzen[0] = opttab.azimuths.at(i)-180.;
            azzen[1] = opttab.zeniths.at(j) ;
			//Run the performance simulation
			if(! _cancel_simulation)
				_SF->Simulate(azzen[0], azzen[1], P);
			if(! _cancel_simulation)
				results.at(k++).process_analytical_simulation(*_SF, 0, azzen);	


			if(_cancel_simulation)
				return false;

		}
	}
	//collect all of the results and process into the efficiency table data structure
	opttab.eff_data.clear();
	k=0;
	for(int j=0; j<neff_zen; j++){
		vector<double> row;
		for(int i=0; i<neff_az; i++){
			row.push_back( results.at(k++).eff_total_sf.ave );
		}
		opttab.eff_data.push_back(row);
	}
	return true;
}

bool AutoPilot_S::CalculateFluxMaps(sp_flux_table &fluxtab, int flux_res_x, int flux_res_y, bool is_normalized)
{
	/* 
	Calculate the flux incident on the receiver(s) and surface(s) in the solar field. 

	"fluxtab"	An instance of 'sp_flux_table' that contains information on the method and sun positions used to
				calculate flux maps throughout the year. The class member "flux_surfaces" is a vector of 
				"sp_flux_stack"s that contain a list of flux maps, where each sp_flux_map corresponds to a single
				receiver/surface. For systems with one receiver, there will be one sp_flux_map contained in the 
				flux_surfaces member. 

				Within each sp_flux_stack is a 3-dimensional array called "flux_data" that contains a list of flux
				intensities on the surface (1st and 2nd dimensions) at various sun positions throughout the year
				(3rd dimension). The positions included are specified in the "azimuths" and "zeniths" arrays in 
				the parent sp_flux_table.

	"flux_res_x"
	"flux_res_y"
				These optional parameters specify the flux map data resolution in the horizontal (x) and vertical (y)
				dimensions of each receiver surface.
				Default values are (x=12, y=10)

	"is_normalized"
				This optional argument determines whether flux will be reported on a normalized basis or not. 
				Normalized flux indicates the share of the power delivered to the receiver that is contained within
				a single map element. Non-normalized flux data indicates the actual power delivered in kW/m2 
				based on the reference simulation DNI value, which by default is 950 W/m2.

	*/

	PreSimCallbackUpdate();
	_cancel_simulation = false;
	
	PrepareFluxSimulation(fluxtab, flux_res_x, flux_res_y, is_normalized);
	
	//ambient conditions
	double dni = _SF->getVarMap()->sf.dni_des.val;
	
	//double args[] = {dni, 25., 1., 0.};		//DNI, Tdb, Pamb, Vwind
    sim_params P;
    P.dni = dni;
    P.Tamb = 25.;

	_sim_total = (int)fluxtab.azimuths.size();	//update the expected number of simulations
	_sim_complete = 0;

	if(_has_summary_callback){
		_summary_siminfo->ResetValues();
		_summary_siminfo->setTotalSimulationCount(_sim_total);
		_summary_siminfo->addSimulationNotice("Simulating flux maps");
	}

	//From the day and time array, produce an azimuth/zenith array
	fluxtab.efficiency.clear();
	for(int i=0; i<_sim_total; i++){
		_sim_complete++;  //increment

		if(_has_summary_callback)
			if( ! 
				_summary_siminfo->setCurrentSimulation(_sim_complete) 
				) 
				CancelSimulation();

		//if(! _cancel_simulation){
		//	_SF->getAmbientObject()->setSolarPosition( fluxtab.azimuths.at(i), fluxtab.zeniths.at(i) );
		//	interop::AimpointUpdateHandler(*_SF);	//update the aim points and image properties
		//}
        double azzen[2];
        azzen[0] = fluxtab.azimuths.at(i);
        azzen[1] = fluxtab.zeniths.at(i);

		if(! _cancel_simulation)
			_SF->Simulate(azzen[0], azzen[1], P);
		if(! _cancel_simulation)
			_SF->HermiteFluxSimulation( *_SF->getHeliostats() );
			
		sim_result result;
		if(! _cancel_simulation){
			result.process_analytical_simulation(*_SF, 2, azzen);	
			fluxtab.efficiency.push_back( result.eff_total_sf.ave );
		}
						
		//Collect flux results here
		if(! _cancel_simulation)
			result.process_flux( _SF, is_normalized);
						
		//Collect the results for each flux surface

		if(! _cancel_simulation){
			PostProcessFlux(result, fluxtab, i);
				
		} //end cancel 

		if(_cancel_simulation)
				return false;
	}
	
	return true;
}

bool AutoPilot_S::CalculateFluxMaps(vector<vector<double> > &sunpos, vector<vector<double> > &fluxtab, vector<double> &efficiency, int flux_res_x, int flux_res_y, bool is_normalized)
{
	PreSimCallbackUpdate();
	return CalculateFluxMapsOV1(sunpos, fluxtab, efficiency, flux_res_x, flux_res_y, is_normalized);
}

//---------------- API_MT --------------------------

#ifdef SP_USE_THREADS

AutoPilot_MT::AutoPilot_MT()
{
	_in_mt_simulation = false;	//initialize
	_cancel_simulation = false;
	_has_summary_callback = false;
	_has_detail_callback = false;
	_summary_callback = 0;
	_detail_callback = 0;
	_summary_callback_data = 0;
	_detail_callback_data = 0;
	_summary_siminfo = 0;
	_SF = 0;
	//initialize with the maximum number of threads
	SetMaxThreadCount(999999);
}

bool AutoPilot_MT::CreateLayout(sp_layout &layout, bool do_post_process)
{
	/* 
	Create a layout using the variable structure that has been created
	*/
	_cancel_simulation = false;
	_in_mt_simulation = false;
	PreSimCallbackUpdate();
	try
	{
		//Is it possible to run a multithreaded simulation?
		int nsim_req = _SF->calcNumRequiredSimulations();
		if(_has_detail_callback){
			_detail_siminfo->ResetValues();
			_detail_siminfo->setTotalSimulationCount(nsim_req);
			_detail_siminfo->addSimulationNotice("Creating field layout");
		}

		if(_n_threads > 1 && nsim_req > 1){
			//More than 1 thread and more than 1 simulation to run

			//Prepare the master solar field object for layout simulation
			WeatherData wdata;
			bool full_sim = _SF->PrepareFieldLayout(*_SF, &wdata);
		
			//If full simulation is required...
			if(full_sim){

				int nthreads = min(nsim_req, _n_threads);

				//update progress
				if(_has_detail_callback)
					_detail_siminfo->addSimulationNotice("Preparing " + my_to_string(_n_threads) + " threads for simulation");
				
				
				//Duplicate SF objects in memory
				SolarField **SFarr;
				SFarr = new SolarField*[nthreads];
				for(int i=0; i<nthreads; i++){
					SFarr[i] = new SolarField(*_SF);
				}
			
				//Create sufficient results arrays in memory
				sim_results results;
				results.resize(nsim_req);
						
				//Calculate the number of simulations per thread
				int npert = (int)ceil((float)nsim_req/(float)nthreads);

				//Create thread objects
				_simthread = new LayoutSimThread[nthreads];
				_n_threads_active = nthreads;	//Keep track of how many threads are active
				_in_mt_simulation = true;
			
				int
					sim_first = 0,
					sim_last = npert;
				for(int i=0; i<nthreads; i++){
                    std::string istr = my_to_string(i+1);
                    _simthread[i].Setup(istr, SFarr[i], &results, &wdata, sim_first, sim_last, false, false);
					sim_first = sim_last;
					sim_last = min(sim_last+npert, nsim_req);
				}
				
				if(_has_detail_callback){
					_detail_siminfo->setTotalSimulationCount(nsim_req);
					_detail_siminfo->setCurrentSimulation(0);
					_detail_siminfo->addSimulationNotice("Simulating layout design-point hours...");
				}
				
				//Run
				for(int i=0; i<nthreads; i++)
					thread( &LayoutSimThread::StartThread, std::ref( _simthread[i] ) ).detach();
			

				//Wait loop
				while(true){
					int nsim_done = 0, nsim_remain=0, nthread_done=0;
					for(int i=0; i<nthreads; i++){
						if( _simthread[i].IsFinished() )
							nthread_done ++;
					
						int ns, nr;
						_simthread[i].GetStatus(&ns, &nr);
						nsim_done += ns;
						nsim_remain += nr;
					
					
					}
					_sim_total = nsim_req;
					_sim_complete = nsim_done;

					if(_has_detail_callback){
						if(! _detail_siminfo->setCurrentSimulation(nsim_done) )
                            break;
                    }
					
				
					if(nthread_done == nthreads) break;
					std::this_thread::sleep_for(std::chrono::milliseconds(75));

				}

				//Check to see whether the simulation was cancelled
				bool cancelled = false;
				for(int i=0; i<nthreads; i++){
					cancelled = cancelled || _simthread[i].IsSimulationCancelled();
				}
			    //check to see whether simulation errored out
                bool errored_out = false;
                for(int i=0; i<_n_threads; i++){
                    errored_out = errored_out || _simthread[i].IsFinishedWithErrors();
                }
                if( errored_out )
                {
                    CancelSimulation();
                    //Get the error messages, if any
                    string errmsgs;
                    for(int i=0; i<_n_threads; i++){
                        for(int j=0; j<(int)_simthread[i].GetSimMessages()->size(); j++)
                            errmsgs.append( _simthread[i].GetSimMessages()->at(j) + "\n");
                    }
                    //Display error messages
                    if(! errmsgs.empty() && _has_summary_callback)
                        _summary_siminfo->addSimulationNotice( errmsgs.c_str() );
            
                }

	            //Clean up dynamic memory
	            for(int i=0; i<_n_threads; i++){
		            delete SFarr[i];
	            }
	            delete [] SFarr;
	            delete [] _simthread;
	            _simthread = 0;

	            //If the simulation was cancelled per the check above, exit out
	            if(cancelled || errored_out){
		            return false;
	            }
			
				//For the map-to-annual case, run a simulation here
				if(_SF->getVarMap()->sf.des_sim_detail.mapval() == var_solarfield::DES_SIM_DETAIL::EFFICIENCY_MAP__ANNUAL)	
					if(! _cancel_simulation)
						SolarField::AnnualEfficiencySimulation(_SF->getVarMap()->amb.weather_file.val, _SF, results); 

				//Process the results
				if(! _cancel_simulation)
					_SF->ProcessLayoutResults(&results, nsim_req);

			}
		}
		else{
			_n_threads_active = 1;
			_in_mt_simulation = false;

			if(! _cancel_simulation){
				bool simok = _SF->FieldLayout();			
            
                if(_SF->ErrCheck() || !simok) return false;
            }
		}
		if(do_post_process){
            Vect sun = Ambient::calcSunVectorFromAzZen( _SF->getVarMap()->sf.sun_az_des.Val()*D2R, (90. - _SF->getVarMap()->sf.sun_el_des.Val())*D2R );   

			if(! _cancel_simulation)
				_SF->calcHeliostatShadows(sun);	if(_SF->ErrCheck()){return false;}
			if(! _cancel_simulation)
				PostProcessLayout(layout);
		}
	}
	catch(std::exception &e){
		_summary_siminfo->addSimulationNotice(e.what());
		return false;
	}
	catch(...){
		_summary_siminfo->addSimulationNotice("Caught unhandled exception in layout simulation. Simulation unsuccessful.");
		return false;
	}
	return true;
}

bool AutoPilot_MT::SetMaxThreadCount(int nt)
{
	//check to make sure the max number of threads is less
	//than the machine's capacity
	try{
		unsigned int nmax = std::thread::hardware_concurrency();
		_n_threads = min(max(nt,1), (int)nmax);
	}
	catch(...)
	{
		return false;
	}
	return true;
}

bool AutoPilot_MT::CalculateOpticalEfficiencyTable(sp_optical_table &opttab)
{
	
	_cancel_simulation = false;
	PreSimCallbackUpdate();

	//set the solar positions to calculate
	int neff_az;
	int neff_zen;
	
	if(! opttab.is_user_positions){
		//set the solar positions for calculation to the default values
		neff_az = 12;
		opttab.azimuths.clear();
		double eff_az[] = {0.,  30.,  60.,  90., 120., 150., 180., 210., 240., 270., 300., 330.};
		for(int i=0; i<neff_az; i++)
			opttab.azimuths.push_back(eff_az[i]);
		
		neff_zen = 8;
		opttab.zeniths.clear();
		double eff_zen[] = {0.50,   7.,  15.,  30.,  45.,  60.,  75.,  85.};
		for(int i=0; i<neff_zen; i++)
			opttab.zeniths.push_back(eff_zen[i]);
	}
	else{
		neff_az = (int)opttab.azimuths.size();
		neff_zen = (int)opttab.zeniths.size();
	}

    var_map *V = _SF->getVarMap();

	double dni = V->sf.dni_des.val;
	//double args[] = {dni, 25., 1., 0.};		//DNI, Tdb, Pamb, Vwind
    sim_params P;
    P.dni = dni;
    P.Tamb = 25.;
	
	int neff_tot = neff_az * neff_zen;
	
	_sim_total = neff_tot;	//set the total simulation counter

	if(_has_summary_callback){
		_summary_siminfo->ResetValues();
		_summary_siminfo->setTotalSimulationCount(_sim_total);
		_summary_siminfo->addSimulationNotice("Simulating optical efficiency points");
	}

	//load the sun positions into a matrix_t
	matrix_t<double> sunpos(neff_tot, 2);
	int k=0;
	for(int j=0; j<neff_zen; j++){
		for(int i=0; i<neff_az; i++){
			sunpos.at(k,0) = (opttab.azimuths.at(i) - 180.)*D2R;
			sunpos.at(k++,1) = opttab.zeniths.at(j)*D2R;
		}
	}

	//------------do the multithreaded run----------------
	
	//Create copies of the solar field
	SolarField **SFarr;
	SFarr = new SolarField*[_n_threads];
	for(int i=0; i<_n_threads; i++){
		SFarr[i] = new SolarField(*_SF);
	}

	//Create sufficient results arrays in memory
	sim_results results;
	results.resize(_sim_total);
						
	//Calculate the number of simulations per thread
	int npert = (int)ceil((float)_sim_total/(float)_n_threads);

	//Create thread objects
	_simthread = new LayoutSimThread[_n_threads];
	_n_threads_active = _n_threads;	//Keep track of how many threads are active
				
	int
		sim_first = 0,
		sim_last = npert;
	for(int i=0; i<_n_threads; i++){
        std::string istr = my_to_string(i);
		_simthread[i].Setup(istr, SFarr[i], &results, &sunpos, P, sim_first, sim_last, true, false);
		sim_first = sim_last;
		sim_last = min(sim_last+npert, _sim_total);
	}
	//Run
	for(int i=0; i<_n_threads; i++)
		thread( &LayoutSimThread::StartThread, std::ref( _simthread[i] ) ).detach();
			

	//Wait loop
	while(true){
		int nsim_done = 0, nsim_remain=0, nthread_done=0;
		for(int i=0; i<_n_threads; i++){
			if( _simthread[i].IsFinished() )
				nthread_done ++;
					
			int ns, nr;
			_simthread[i].GetStatus(&ns, &nr);
			nsim_done += ns;
			nsim_remain += nr;
					
					
		}
		if(_has_summary_callback){
			if( ! _summary_siminfo->setCurrentSimulation(nsim_done) )
				CancelSimulation();
		}
		if(nthread_done == _n_threads) break;
		std::this_thread::sleep_for(std::chrono::milliseconds(75));
	}

	//Check to see whether the simulation was cancelled
	bool cancelled = false;
	for(int i=0; i<_n_threads; i++){
		cancelled = cancelled || _simthread[i].IsSimulationCancelled();
	}
    
    //check to see whether simulation errored out
    bool errored_out = false;
    for(int i=0; i<_n_threads; i++){
        errored_out = errored_out || _simthread[i].IsFinishedWithErrors();
    }
    if( errored_out )
    {
        CancelSimulation();
        //Get the error messages, if any
        string errmsgs;
        for(int i=0; i<_n_threads; i++){
            for(int j=0; j<(int)_simthread[i].GetSimMessages()->size(); j++)
                errmsgs.append( _simthread[i].GetSimMessages()->at(j) + "\n");
        }
        //Display error messages
        if(! errmsgs.empty() && _has_summary_callback)
            _summary_siminfo->addSimulationNotice( errmsgs.c_str() );
            
    }

	//Clean up dynamic memory
	for(int i=0; i<_n_threads; i++){
		delete SFarr[i];
	}
	delete [] SFarr;
	delete [] _simthread;
	_simthread = 0;

	//If the simulation was cancelled per the check above, exit out
	if(cancelled || errored_out){
		return false;
	}

	//collect all of the results and process into the efficiency table data structure
	opttab.eff_data.clear();
	k=0;
	for(int j=0; j<neff_zen; j++){
		vector<double> row;
		for(int i=0; i<neff_az; i++){
			row.push_back( results.at(k++).eff_total_sf.ave );
		}
		opttab.eff_data.push_back(row);
	}

	return true;
}

bool AutoPilot_MT::CalculateFluxMaps(sp_flux_table &fluxtab, int flux_res_x, int flux_res_y, bool is_normalized)
{
	/* 
	Calculate the flux incident on the receiver(s) and surface(s) in the solar field. 

	"fluxtab"	An instance of 'sp_flux_table' that contains information on the method and sun positions used to
				calculate flux maps throughout the year. The class member "flux_surfaces" is a vector of 
				"sp_flux_stack"s that contain a list of flux maps, where each sp_flux_map corresponds to a single
				receiver/surface. For systems with one receiver, there will be one sp_flux_map contained in the 
				flux_surfaces member. 

				Within each sp_flux_stack is a 3-dimensional array called "flux_data" that contains a list of flux
				intensities on the surface (1st and 2nd dimensions) at various sun positions throughout the year
				(3rd dimension). The positions included are specified in the "azimuths" and "zeniths" arrays in 
				the parent sp_flux_table.

	"flux_res_x"
	"flux_res_y"
				These optional parameters specify the flux map data resolution in the horizontal (x) and vertical (y)
				dimensions of each receiver surface.
				Default values are (x=12, y=10)

	"is_normalized"
				This optional argument determines whether flux will be reported on a normalized basis or not. 
				Normalized flux indicates the share of the power delivered to the receiver that is contained within
				a single map element. Non-normalized flux data indicates the actual power delivered in kW/m2 
				based on the reference simulation DNI value, which by default is 950 W/m2.

	*/


	_cancel_simulation = false;
	PreSimCallbackUpdate();
	PrepareFluxSimulation(fluxtab, flux_res_x, flux_res_y, is_normalized);
	
	//ambient conditions
	double dni = _SF->getVarMap()->sf.dni_des.val;
	//double args[] = {dni, 25., 1., 0.};		//DNI, Tdb, Pamb, Vwind
    sim_params P;
    P.dni = dni;
    P.Tamb = 25.;

	_sim_total = (int)fluxtab.azimuths.size();	//update the expected number of simulations
	_sim_complete = 0;

	//collect the sun positions for the simulations into a single matrix_t
	matrix_t<double> sunpos(_sim_total, 2);
	for(int i=0; i<_sim_total; i++){
		sunpos.at(i,0) = fluxtab.azimuths.at(i);
		sunpos.at(i,1) = fluxtab.zeniths.at(i);
	}
	fluxtab.efficiency.clear();
	fluxtab.efficiency.resize(_sim_total);

	if(_has_summary_callback){
		_summary_siminfo->ResetValues();
		_summary_siminfo->setTotalSimulationCount(_sim_total);
		_summary_siminfo->addSimulationNotice("Simulating flux maps");
	}


	//------------do the multithreaded run----------------
	
	//Create copies of the solar field
	SolarField **SFarr;
	SFarr = new SolarField*[_n_threads];
	for(int i=0; i<_n_threads; i++){
		SFarr[i] = new SolarField(*_SF);
	}

	//Create sufficient results arrays in memory
	sim_results results;
	results.resize(_sim_total);

	//Calculate the number of simulations per thread
	int npert = (int)ceil((float)_sim_total/(float)_n_threads);

	//Create thread objects
	_simthread = new LayoutSimThread[_n_threads];
	_n_threads_active = _n_threads;	//Keep track of how many threads are active
	
	int
		sim_first = 0,
		sim_last = npert;
	for(int i=0; i<_n_threads; i++){
        std::string istr = my_to_string(i);
        _simthread[i].Setup(istr, SFarr[i], &results, &sunpos, P, sim_first, sim_last, true, true);
		_simthread[i].IsFluxmapNormalized(is_normalized);
		sim_first = sim_last;
		sim_last = min(sim_last+npert, _sim_total);
	}
	//Run
	for(int i=0; i<_n_threads; i++)
		thread( &LayoutSimThread::StartThread, std::ref( _simthread[i] ) ).detach();
			

	//Wait loop
	while(true){
		int nsim_done = 0, nsim_remain=0, nthread_done=0;
		for(int i=0; i<_n_threads; i++){
			if( _simthread[i].IsFinished() )
				nthread_done ++;
					
			int ns, nr;
			_simthread[i].GetStatus(&ns, &nr);
			nsim_done += ns;
			nsim_remain += nr;
					
					
		}
		if(_has_summary_callback){
			if( ! _summary_siminfo->setCurrentSimulation(nsim_done) ) 
				CancelSimulation();
		}
		if(nthread_done == _n_threads) break;
		std::this_thread::sleep_for(std::chrono::milliseconds(75));
	}

	//Check to see whether the simulation was cancelled
	bool cancelled = false;
	for(int i=0; i<_n_threads; i++){
		cancelled = cancelled || _simthread[i].IsSimulationCancelled();
	}
	//check to see whether simulation errored out
    bool errored_out = false;
    for(int i=0; i<_n_threads; i++){
        errored_out = errored_out || _simthread[i].IsFinishedWithErrors();
    }
    if( errored_out )
    {
        CancelSimulation();
        //Get the error messages, if any
        string errmsgs;
        for(int i=0; i<_n_threads; i++){
            for(int j=0; j<(int)_simthread[i].GetSimMessages()->size(); j++)
                errmsgs.append( _simthread[i].GetSimMessages()->at(j) + "\n");
        }
        //Display error messages
        if(! errmsgs.empty() && _has_summary_callback)
            _summary_siminfo->addSimulationNotice( errmsgs.c_str() );
            
    }

	//Clean up dynamic memory
	for(int i=0; i<_n_threads; i++){
		delete SFarr[i];
	}
	delete [] SFarr;
	delete [] _simthread;
	_simthread = 0;

	//If the simulation was cancelled per the check above, exit out
	if(cancelled || errored_out){
		return false;
	}

	for(int i=0; i<_sim_total; i++){
		PostProcessFlux(results.at(i), fluxtab, i);
		fluxtab.efficiency.at(i) = results.at(i).eff_total_sf.ave;
	}


	return true;
}

bool AutoPilot_MT::CalculateFluxMaps(vector<vector<double> > &sunpos, vector<vector<double> > &fluxtab, vector<double> &efficiency, int flux_res_x, int flux_res_y, bool is_normalized)
{
	PreSimCallbackUpdate();
	return CalculateFluxMapsOV1(sunpos, fluxtab, efficiency, flux_res_x, flux_res_y, is_normalized);
}

void AutoPilot_MT::CancelSimulation()
{
	CancelMTSimulation();
}

void AutoPilot_MT::CancelMTSimulation()
{
	_cancel_simulation = true;
	if(_in_mt_simulation && _simthread != 0){
		for(int i=0; i<_n_threads_active; i++){
			_simthread[i].CancelSimulation();
		}
	}
}

#endif // SP_USE_THREADS

