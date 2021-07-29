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

#include <algorithm>
#include <stdio.h>
#include <sstream>

#include "mod_base.h"
#include "exceptions.hpp"

using namespace std;

//------ simulation info --------

simulation_info::simulation_info(){ 
	_is_active = false; 
	Reset(); 
	_callback = 0;
	_callback_data = 0;
}
	
bool simulation_info::isEnabled(){return _is_active;}	//Indicates whether any simulation is currently in progress (that's being tracked)
int simulation_info::getCurrentSimulation(){return _is_active ? _current_simulation : 0;}	//Index of the current simulation
int simulation_info::getTotalSimulationCount(){return _is_active ? _total_sim_count : 0;}	//Total number of expected simulations in this batch
double simulation_info::getSimulationProgress(){return _is_active ? _sim_progress : 0.;}	//Fractional progress [0..1] of the simulation
void simulation_info::getSimulationInfo(int &current, int &total, double &progress){ 
	if(!_is_active) return;
	current = _current_simulation;
	total = _total_sim_count;
	progress = _sim_progress;
};
std::string *simulation_info::getSimulationNotices(){return &_sim_notice;}	//Returns a pointer to the vector of simulation notices 

void simulation_info::ResetValues(){
	_current_simulation = 0; 
	_total_sim_count = 0;
	_sim_progress = 0.;
};
void simulation_info::Reset(){
	_current_simulation = 0; 
	_total_sim_count = 0;
	_sim_progress = 0.;
	_sim_notice.clear();
};

//Sets
void simulation_info::setCallbackFunction(bool (*updateFunc)(simulation_info* siminfo, void *data), void *cdata){
	_callback = updateFunc;
	_callback_data = cdata;
	_is_active = true;
}

void *simulation_info::getCallbackData()
{
    return _callback_data;
}

bool simulation_info::setCurrentSimulation(int val)
{
	if(!_is_active) return true; 
	_current_simulation = val; 
	return (*_callback)(this, _callback_data);
	
}

bool simulation_info::setTotalSimulationCount(int val)
{
	if(!_is_active) return true; 
	_total_sim_count = val; 
	return (*_callback)(this, _callback_data);
	
}

void simulation_info::clearSimulationNotices()
{
	if(!_is_active) return; 
	_sim_notice.clear();
}

bool simulation_info::addSimulationNotice(std::string &notice)
{
	if(!_is_active) return true; 
	_sim_notice =  notice; 
	return (*_callback)(this, _callback_data);
	
}

bool simulation_info::addSimulationNotice(std::string notice)
{
	if(!_is_active) return true; 
	_sim_notice = notice; 
	return (*_callback)(this, _callback_data);
	
}

void simulation_info::isEnabled(bool state){_is_active = state;}
//-------------------------------



//
//
//void mod_base::setVar(std::string varname, bounds_array &variable, var_map &V){
//	//Specifically loads the field boundary arrays
//	//The format should be [POLY][P]x1,y1,z1[P]x2,y2,z2...[POLY][P]...
//	if(V.find(varname) != V.end()) {
//		vector<std::string> spolys = split(V[varname].value, "[POLY]");
//		vector<std::string> line, pnt;
//		double x, y, z;
//		int npoly = (int)spolys.size();
//		variable.resize(npoly);
//		for(int i=0; i<npoly; i++){
//			line.clear();
//			line = split(spolys.at(i), "[P]");
//			int npt = (int)line.size();	//The number of points in the polygon
//			variable.at(i).resize(npt);	//Resize the polygon container
//
//			for(int j=0; j<npt; j++){
//				pnt = split(line.at(j), ",");	//Split by comma
//				to_double(pnt.at(0), &x);
//				to_double(pnt.at(1), &y);
//				to_double(pnt.at(2), &z);
//				variable.at(i).at(j).Set(x, y, z);
//			}
//		}
//
//	}
//	else{
//		variable.resize(1);
//		variable.at(0).resize(1);
//		variable.at(0).at(0).Set(0.,0.,0.);
//	}
//
//}

bool mod_base::checkRange(std::string range, double &val, int *flag)
{
	//take range std::string of form:
	// {dlow,dhi} 
	// where {} can be replaced by ( ), [ ], ( ], [ )

	//parse the std::string
	vector<std::string> t1 = split(range, ",");
	if(t1.size()<2) return true;

	std::string lop, rop, ops, ls, rs;
	ls = t1.at(0);
	rs = t1.at(1);
	lop = ls.at(0);
	rop = rs.at(rs.size()-1);
	//Convert range values to doubles
	double lval, rval;
	to_double(ls.erase(0,1), &lval);
	to_double(rs.erase(rs.size()-1,1), &rval);
	//flags for information to return
	int tflag=-1;	//return the type of range applied (i.e. less than, greater than | less than or equal to, greater than, etc)
	bool retflag=false;	//Is the boundary satisfied?

	ops = lop+rop;
	if(ops == " "){return true;}	//no info, don't check
	else if(ops == "()"){if(val > lval && val < rval) {retflag = true; tflag=1;}}
	else if(ops == "[)"){if(val >= lval && val < rval) {retflag = true; tflag=2;}}
	else if(ops == "(]"){if(val > lval && val <= rval) {retflag = true; tflag=3;}}
	else if(ops == "[]"){if(val >= lval && val <= rval) {retflag = true; tflag=4;}}
	else{retflag = true;}

	if(flag != NULL) *flag = tflag;
	return retflag;	//boundary not satisfied by any previous consideration

}

bool mod_base::checkRange(std::string range, int &val, int *flag)
{
	double dval = double(val); 
	return checkRange(range, dval, flag);
};

std::string *mod_base::getWorkingDir(){return &_working_dir;}
void mod_base::setWorkingDir(std::string &dir){_working_dir = dir;}

//-------------------- simulation error ------------
simulation_error::simulation_error(){ 
	_callback = 0; 
	_callback_data = 0;
	_is_fatal = false; 
	_force_display = false;
	_terminate_status = false;
	_is_connected = false;
	_message_log.clear();
}

void simulation_error::setCallbackFunction(void (*errorFunc)(simulation_error* sim_error, void *data), void *cdata){
	_callback = errorFunc;
	_callback_data = cdata;
	_is_connected = true;
}

bool simulation_error::isFatal(){return _is_fatal;}
bool simulation_error::isDisplayNow(){return _force_display;}
std::string *simulation_error::getSimulationErrors(){return &_message_log;}
bool simulation_error::checkForErrors(){return _terminate_status || _is_fatal;}

void simulation_error::Reset(){
	_is_fatal = false; 
	_terminate_status = false;
	_force_display = false;	
	_message_log.clear(); 
}
//If any error has already been recorded, don't reset the error flag.
void simulation_error::addSimulationError(std::string error, bool is_fatal, bool force_display){
	if(! _is_connected ) return;	//only deal with this if the object has been connected to a callback

	_is_fatal = _is_fatal ? true : is_fatal; 
	_force_display = _force_display ? true : force_display;
	_message_log.append(error);
	(*_callback)(this, _callback_data);
	//(*_parent.*_ferror)();
}
void simulation_error::addRangeError(double val, std::string varname, std::string range){
	char fmt[] = "Variable %s is out of range with value %f. The valid range is %s.\n";
	char msg[200];
	sprintf(msg, fmt, varname.c_str(), val, range.c_str());
	addSimulationError(msg, true, true);
}
void simulation_error::setTerminateStatus(bool do_terminate){_terminate_status = do_terminate;}
void simulation_error::clearErrorLog(){_message_log.clear();}
