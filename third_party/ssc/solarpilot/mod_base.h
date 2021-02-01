/*******************************************************************************************************
*  Copyright 2017 Alliance for Sustainable Energy, LLC
*
*  NOTICE: This software was developed at least in part by Alliance for Sustainable Energy, LLC
*  (�Alliance�) under Contract No. DE-AC36-08GO28308 with the U.S. Department of Energy and the U.S.
*  The Government retains for itself and others acting on its behalf a nonexclusive, paid-up,
*  irrevocable worldwide license in the software to reproduce, prepare derivative works, distribute
*  copies to the public, perform publicly and display publicly, and to permit others to do so.
*
*  Redistribution and use in source and binary forms, with or without modification, are permitted
*  provided that the following conditions are met:
*
*  1. Redistributions of source code must retain the above copyright notice, the above government
*  rights notice, this list of conditions and the following disclaimer.
*
*  2. Redistributions in binary form must reproduce the above copyright notice, the above government
*  rights notice, this list of conditions and the following disclaimer in the documentation and/or
*  other materials provided with the distribution.
*
*  3. The entire corresponding source code of any redistribution, with or without modification, by a
*  research entity, including but not limited to any contracting manager/operator of a United States
*  National Laboratory, any institution of higher learning, and any non-profit organization, must be
*  made publicly available under this license for as long as the redistribution is made available by
*  the research entity.
*
*  4. Redistribution of this software, without modification, must refer to the software by the same
*  designation. Redistribution of a modified version of this software (i) may not refer to the modified
*  version by the same designation, or by any confusingly similar designation, and (ii) must refer to
*  the underlying software originally provided by Alliance as �System Advisor Model� or �SAM�. Except
*  to comply with the foregoing, the terms �System Advisor Model�, �SAM�, or any confusingly similar
*  designation may not be used to refer to any modified version of this software or any modified
*  version of the underlying software originally provided by Alliance without the prior written consent
*  of Alliance.
*
*  5. The name of the copyright holder, contributors, the United States Government, the United States
*  Department of Energy, or any of their employees may not be used to endorse or promote products
*  derived from this software without specific prior written permission.
*
*  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR
*  IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND
*  FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER,
*  CONTRIBUTORS, UNITED STATES GOVERNMENT OR UNITED STATES DEPARTMENT OF ENERGY, NOR ANY OF THEIR
*  EMPLOYEES, BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
*  DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
*  DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER
*  IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF
*  THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*******************************************************************************************************/

#ifndef _MOD_BASE_
#define _MOD_BASE_ 1

/*
Forms the base class for the various components of the solar field. 
These components can use the methods and variable declarations provided here.
*/
#include <sstream>

#include <unordered_map>
using std::unordered_map;

#include <map>
#include <vector>
#include <string>
#include <time.h>
#include <algorithm>

#include "Toolbox.h"
#include "string_util.h"

template<typename T> static std::string my_to_string(const T &value) {
	std::ostringstream x;
	x << value;
	return x.str();
}

class simulation_info
{
	/* 
	This object provides information to the calling program on the status of the 
	simulations underway.
	*/
	bool (*_callback)(simulation_info* siminfo, void *data);
	void *_callback_data;

	double
		_sim_progress;
	std::string
		_sim_notice;
	int
		_current_simulation,
		_total_sim_count;
	bool
		_is_active;

public:
	simulation_info();
	
	bool isEnabled();	//Indicates whether any simulation is currently in progress (that's being tracked)
	int getCurrentSimulation();	//Index of the current simulation
	int getTotalSimulationCount();	//Total number of expected simulations in this batch
	double getSimulationProgress();	//Fractional progress [0..1] of the simulation
	void getSimulationInfo(int &current, int &total, double &progress);
	std::string *getSimulationNotices();	//Returns a pointer to the std::vector of simulation notices 

	void ResetValues();
	void Reset();

	//Sets
	void setCallbackFunction(bool (*updateFunc)(simulation_info* siminfo, void *data), void *cdata);
	bool setCurrentSimulation(int val);
	bool setTotalSimulationCount(int val);
	void clearSimulationNotices();
	bool addSimulationNotice(std::string &notice);
	bool addSimulationNotice(std::string notice);
	void isEnabled(bool state);
    void* getCallbackData();
};

class simulation_error
{
	/*
	This class serves as a method for providing the calling program with events and info on 
	simulation (run-time) errors and warnings.
	*/

	void (*_callback)(simulation_error* simerror, void *data);
	void *_callback_data;

	std::string _message_log;
	bool _is_connected;		//has this been tied to a parent error handler?
	bool _is_fatal;
	bool _force_display;	//Force the message to pop 
	bool _terminate_status;	//Set by the calling program. Simulations should terminate if true

public:
	simulation_error();

	void setCallbackFunction(void (*errorFunc)(simulation_error* simerror, void *data), void *cdata);

	bool isFatal();
	bool isDisplayNow();
	std::string *getSimulationErrors();
	bool checkForErrors();

	void Reset();

	//If any error has already been recorded, don't reset the error flag.
	void addSimulationError(std::string error, bool is_fatal=false, bool force_display=false);
	void addRangeError(double val, std::string varname, std::string range);
	void setTerminateStatus(bool do_terminate);
	void clearErrorLog();
	
};

enum SP_DATTYPE { SP_INT, SP_DOUBLE, SP_STRING, SP_BOOL, SP_MATRIX_T, SP_DVEC_POINT, SP_VEC_DOUBLE, SP_VEC_INTEGER, SP_WEATHERDATA, SP_VOIDPTR};

class spbase
{

public:

    static bool _setv(std::string &SV, void *Vp)
    {
        //don't do anything with a void argument
        (void)SV;
        (void)Vp;
        return true;
    }

    static bool _setv(std::string &SV, int &Vp)
    {
        return to_integer(SV, &Vp);
    }; 

    static bool _setv(std::string &SV, double &Vp)
    {
        return to_double(SV, &Vp);
    }; 

    static bool _setv(std::string &SV, std::string &Vp)
    {
        Vp = SV;
        return true;
    };

    static bool _setv(std::string &SV, bool &Vp)
    {
        return to_bool(SV, Vp);
    };

    static bool _setv(std::string &SV, matrix_t<double> &Vp)
    {
        try
        {
            std::vector<std::string> content = split(SV, ";");
		    int nrows = (int)content.size();
		    if(nrows == 0) { Vp.resize_fill(1,2,0.0); return true; }
		    std::vector<std::string> line;
		    line = split(content.at(0), ",");
		    int rowlen = (int)line.size();
		    Vp.resize(nrows, rowlen);
		    for (int i=0; i<nrows; i++){
			    line = split(content.at(i), ",");
			    for (int j=0; j<rowlen; j++){
				    to_double(line.at(j), &Vp.at(i, j));
			    }
		    }
        }
        catch(...)
        {
            return false;
        }
        return true;
    };

    static bool _setv(std::string &SV, std::vector< sp_point > &Vp)
    {
        //splits a set of 3d points into a vector<sp_point>
	    //should be [P]x1,y1,z1[P]x2,y2,z2...
        try
        {
		    std::vector<std::string> content = split(SV, "[P]");
		    std::vector<std::string> line;
		    double x, y, z;
		    int nrows = (int)content.size();
		    Vp.resize(nrows);
		    for(int i=0; i<nrows; i++) {
			    //split each text by comma
			    line = split(content.at(i), ",");
			    to_double(line.at(0), &x);
			    to_double(line.at(1), &y);
			    to_double(line.at(2), &z);
			    Vp.at(i).Set(x, y, z);
		    }
	    }
        catch(...)
        {
            return false;
        }
        return true;
    };

    static bool _setv(std::string &SV, std::vector< double > &Vp )
    {
        try{

            std::vector< std::string > svals = split(SV, ",");

            Vp.resize( svals.size() );

            for(size_t i=0; i<svals.size(); i++)
                to_double( svals.at(i), &Vp.at(i) );
        }
        catch(...)
        {
            return false;
        }
        return true;
    };

    static bool _setv(std::string &SV, std::vector< int > &Vp)
    {
        try
        {
            std::vector< std::string > svals = split(SV, ",");

            Vp.resize( svals.size() );

            for(size_t i=0; i<svals.size(); i++)
                to_integer( svals.at(i), &Vp.at(i) );
        }
        catch(...)
        {
            return false; 
        }
        return true;
    };

    static bool _setv(std::string &SV, WeatherData &Vp)
    {
        try
        {
            std::vector<std::string> vals;
			std::vector<std::string> entries = split(SV, "[P]");

		    int nrows = (int)entries.size();
		    int nv, i, j;
		    Vp.resizeAll(nrows, 0.0);

            //day, hour, month, dni, tdb, pres, vwind, step_weight
		    std::vector<std::vector<double>*> *wdvars = Vp.getEntryPointers();

		    for(i=0; i<nrows; i++){
			    vals = split(entries.at(i), ",");
			    nv = (int)(vals.size() < wdvars->size() ? vals.size() : wdvars->size()); 
			    for(j=0; j<nv; j++){
				    to_double(vals.at(j), &wdvars->at(j)->at(i));
			    }
		    }
        }
        catch(...)
        {
            return false;
        }
        return true;
    };

    static bool _setv(std::string &SV, std::vector< std::vector< sp_point > > &Vp )
    {
        
        /* 
        [POLY] separates entries
        [P] separates points within a polygon
        ',' separates x,y,z within a point
        */

        Vp.clear();

        if( SV.empty() ) return true;

        std::vector< std::string > polys = split(SV, "[POLY]");

        Vp.resize(polys.size() );

        for(size_t i=0; i<polys.size(); i++)
        {
            std::vector< std::string > pts = split(polys.at(i), "[P]");

            Vp.at(i).resize( pts.size(), sp_point() );

            for( size_t j=0; j<pts.size(); j++ )
            {
                std::vector< std::string > vals = split(pts.at(j), ",");

                double x;
                for( size_t k=0; k<vals.size(); k++ )
                {
                    to_double(vals.at(k), &x);
                    Vp.at(i).at(j)[(int)k] = x;
                }
            }
        }

        return true;
    };

protected:
    //----------------------------------------------------------------------------------------
    void _as_str(std::string &vout, void* v)
    {
        //don't do anything with a void argument
        (void)vout;
        (void)v;
    }

    void _as_str(std::string &vout, int &v)
    {
        vout = my_to_string(v);
    };

    void _as_str(std::string &vout, std::string &v)
    {
        vout = v;
    };

    void _as_str(std::string &vout, double &v)
    {
        vout = my_to_string(v);
    };

    void _as_str(std::string &vout, bool &v)
    {
        vout = v ? "true" : "false";
    };

    void _as_str(std::string &vout,  matrix_t<double> &v)
    {
        vout.clear();
        for(size_t i=0; i<v.nrows(); i++)
        {
            for(size_t j=0; j<v.ncols(); j++)
            {
                vout.append( my_to_string(v.at(i,j)) );
                if( j < v.ncols()-1 )
                    vout.append(",");
            }
            vout.append(";");
        }
    };

    void _as_str(std::string &vout, std::vector< sp_point > &v)
    {
        vout.clear();

        for(size_t i=0; i<v.size(); i++)
            vout.append("[P]" + my_to_string(v.at(i).x) + "," + my_to_string(v.at(i).y) + "," + my_to_string(v.at(i).z) );
    };

    void _as_str(std::string &vout, std::vector< double > &v)
    {
        vout.clear();
        for(size_t i=0; i<v.size(); i++)
        {
            vout.append( my_to_string(v.at(i)) );
            if(i<v.size()-1)
                vout.append(",");
        }
    };

    void _as_str(std::string &vout, std::vector< int > &v)
    {
        vout.clear();
        for(size_t i=0; i<v.size(); i++)
        {
            vout.append( my_to_string(v.at(i)) );
            if(i<v.size()-1)
                vout.append(",");
        }
    };

    void _as_str(std::string &vout, WeatherData &v)
    {
        vout.clear();

        std::stringstream S;

        std::vector<std::vector<double>*> *wp = v.getEntryPointers();

        for(size_t i=0; i<wp->front()->size(); i++)
        {
            S << "[P]";

            for(size_t j=0; j<wp->size(); j++)
            {
                S << wp->at(j)->at(i);
                if( j<wp->size()-1 )
                    S << ",";
            }
        }
        vout = S.str();
    };

    void _as_str(std::string &vout, std::vector< std::vector< sp_point > > &v)
    {
        /* 
        [POLY] separates entries
        [P] separates points within a polygon
        ',' separates x,y,z within a point
        */

        vout.clear();

        for( size_t i=0; i<v.size(); i++)
        {
            vout.append("[POLY]");

            for( size_t j=0; j<v.at(i).size(); j++)
            {
                vout.append("[P]");

                for(int k=0; k<3; k++)
                {
                    vout.append(my_to_string(v.at(i).at(j)[k]));
                    if( k<2 )
                        vout.append(",");
                }
            }
        }

        return;
    };

    //int cselect;	//Current selection for a combo, integer corresponding to the mapped options (not the choices vector)


public:

    std::string	name;	    //Formal variable name
	std::string units;	    //units for the variable
	std::string ctype;	    //Control type
	SP_DATTYPE dattype;    //data type DATTYPE enum
	std::string short_desc; //Short description
	std::string long_desc;	//Long description
	
	
    bool is_param;	//Is this variable parameterizable?
	bool is_disabled;	//Is this variable disabled (overridden)?

    //virtual bool set_from_string(std::string &Val){ (void)Val; return false;};
    virtual bool set_from_string(const char* Val){(void)Val; return false;};
    virtual void as_string(std::string &ValAsStr){ (void)ValAsStr; throw spexception("Virtual method as_string cannot be executed in base class");};
    virtual std::string as_string(){throw spexception("Virtual method as_string cannot be executed in base class");};
    virtual bool combo_select(std::string choice){ (void)choice; throw spexception("Virtual method combo_select cannot be executed in base class"); };
    virtual bool combo_select_by_choice_index(int index){ (void)index; throw spexception("Virtual method combo_select_by_choice_index cannot be executed in base class");};
    virtual bool combo_select_by_mapval(int mapval){ (void)mapval; throw spexception("Virtual method combo_select_by_mapval cannot be executed in base class");};
    virtual std::vector<std::string> combo_get_choices(){throw spexception("Virtual method combo_get_choices cannot be executed in base class");};
    virtual int combo_get_count(){throw spexception("Virtual method combo_get_count cannot be executed in base class");};
    virtual int mapval(){throw spexception("Virtual method combo_get_current_mapval cannot be executed in base class");};
    virtual int combo_get_current_index(){throw spexception("Virtual method combo_get_current_index cannot be executed in base class");};
    virtual SP_DATTYPE get_data_type(){ return dattype; }
};

template <typename T>
class spvar : public spbase
{

    struct combo_choices
    {
        std::vector<std::string> _choices;
        std::vector<int> _intvals;

        std::string &at_index(int ind){return _choices.at(ind); };
        int at(std::string choicename){ 
            int ind = index(choicename);
            if(ind < _intvals.size() )
                return _intvals.at( index(choicename) );
            else 
                throw spexception("Could not locate combo value " + choicename);
        };
        int index( std::string choicename) {
            return (int)(find(_choices.begin(), _choices.end(), choicename) - _choices.begin());
        };
        void clear()
        {
            _choices.clear();
            _intvals.clear();
        };
        
    };
    combo_choices choices;

public:
    T val;

    /* ------- combo stuff ------------*/
    void combo_clear()
    {
        choices.clear();
    };
    
    std::vector<std::string> combo_get_choices()
    {
        int nv = (int)choices._choices.size();
        std::vector<std::string> rv(nv);
        for(int i=0; i<nv; i++)
        {
            _as_str(rv.at(i), choices._choices.at(i));
        }

        return rv;
    };
    
    void combo_add_choice(std::string &choicename, std::string &mval)
    {
        int mapint;
        to_integer(mval, &mapint);
        choices._choices.push_back(choicename);
        choices._intvals.push_back(mapint);
    };

    bool combo_select_by_choice_index(int index)
    {
        _setv(choices._choices.at(index), val);
        return true;
    };

    bool combo_select_by_mapval(int mapval)
    {
        int index = (int)(find(choices._intvals.begin(), choices._intvals.end(), mapval) - choices._intvals.begin());
        if( index < (int)choices._intvals.size() )
            _setv(choices._choices.at(index), val);
        else
            return false;
        
        return true;
    };

    bool combo_select(std::string choice)
    {
        int ind = (int)(find(choices._choices.begin(), choices._choices.end(), choice) - choices._choices.begin());
        if( ind < (int)choices._choices.size() )
            _setv(choice, val);
        else
            throw spexception("Invalid combo value specified: " + choice);

        return true;
    };
    
	int mapval()
    {
        std::string valstr; 
        _as_str(valstr, val);
        return choices._intvals.at( choices.index( valstr ) );
    };

    int combo_get_current_index()
    {
        std::string valstr; 
        _as_str(valstr, val);
        return choices.index( valstr );
    };

    int combo_get_count()
    {
        return (int)choices._choices.size();
    };
    /* ------------------------------- */
    
    bool set_from_string(const char* Val)
    {
    	std::string sval = Val;
    	return _setv(sval, val);
    };
    
    void as_string(std::string &ValAsStr)
    {
        _as_str(ValAsStr, val);
    }
    std::string as_string()
    {
        std::string vstr;
        _as_str(vstr, val);
        return vstr;
    }

    void set( 
        std::string Address, 
        SP_DATTYPE Dtype, 
        std::string Value, 
        std::string Units, 
        bool Is_param, 
        std::string Ctrl, 
        std::string Special,
        bool UI_disable,
        std::string Label,
        std::string Description)
    {
        /* 
        Parse and set the variable to it's value from a string argument
        */

        //first set supplementary info
	    name = Address;	//Formal variable name
	    units = Units;	//units for the variable
	    ctype = Ctrl;	//Control type
	    dattype = Dtype; //data type DATTYPE enum
	    short_desc = Label; //Short description
	    long_desc = Description;	//Long description
        is_param = Is_param;	//Is this variable parameterizable?
	    is_disabled = UI_disable;	//Is this variable disabled (overridden)?

        choices.clear();
        if( ctype == "combo" )
        {
            std::vector<std::string> ckeys = split(Special, ";");
            for(int i=0; i<(int)ckeys.size(); i++)
            {
                std::vector<std::string> pair = split(ckeys.at(i), "=");

                combo_add_choice( pair.front(), pair.back() );
            }

            //when loading a combo, the value will be specified by looking for an index in "Value" and setting val equal to the choice at that index
            int val_index;
            to_integer(Value, &val_index);

            if(! Special.empty() )
                combo_select_by_choice_index( val_index );

        }
        else
        {
            //parse value
            bool parseok = _setv(Value, val);

            //check for parsing errors
            if(! parseok)
                throw spexception("An error occurred while assigning input to the internal variable structure. {" + Address + " << " + Value + "}");
        }
        
    };




};

template <typename T>
class spout : public spbase  //need public inheritance of spbase to allow conversion to base class
{
private:
    // override public inheritance of members to be private in spout class
    using spbase::name;	    //Formal variable name
	using spbase::units;	    //units for the variable
	using spbase::ctype;	    //Control type
	using spbase::dattype;    //data type DATTYPE enum
	using spbase::short_desc; //Short description
	using spbase::long_desc;	//Long description
	
    using spbase::is_param;	//Is this variable parameterizable?
	using spbase::is_disabled;	//Is this variable disabled (overridden)?

    T _val;
public:

    bool set_from_string(const char* Val)
    {
        std::string sval = Val;
        return _setv(sval, _val);
    };
    void as_string(std::string &ValAsStr)
    {
        _as_str(ValAsStr, _val);
    }
    std::string as_string()
    {
        std::string vstr;
        _as_str(vstr, _val);
        return vstr;
    }
    void setup( 
        std::string Address, 
        SP_DATTYPE Dtype, 
        std::string Units, 
        bool Is_param, 
        std::string Ctrl, 
        std::string Special,
        bool UI_disable,
        std::string Label,
        std::string Description)
    {
        //first set supplementary info
	    name = Address;	//Formal variable name
	    units = Units;	//units for the variable
	    ctype = Ctrl;	//Control type
	    dattype = Dtype; //data type DATTYPE enum
	    short_desc = Label; //Short description
	    long_desc = Description;	//Long description

        if( ! ctype.empty() )
            throw spexception("Special controls are not allowed for spout objects");

        is_param = Is_param;	//Is this variable parameterizable?
	    is_disabled = UI_disable;	//Is this variable disabled (overridden)?

    }

    //create methods for the variable members to emphasize that these cannot be modified
    T& Val() { return _val; };	//get variable value
    void Setval(T v) { _val = v; };  //set variable value
    std::string	Name() { return name; };	    //Formal variable name
    std::string Units() { return units; };	    //units for the variable
    std::string Ctype() { return ctype; };	    //Control type
    SP_DATTYPE Dattype() { return dattype; };    //data type DATTYPE enum
    std::string Short_desc() { return short_desc; }; //Short description
    std::string Long_desc() { return long_desc; };	//Long description
    bool Is_param() { return is_param; };	//Is this variable parameterizable?
    bool Is_disabled() { return is_disabled; };	//Is this variable disabled (overridden)?
};



class mod_base
{
	
public:
	std::string _working_dir;

	bool checkRange(std::string range, int &val, int *flag = NULL);
	bool checkRange(std::string range, double &val, int *flag = NULL);

	std::string *getWorkingDir();
	void setWorkingDir(std::string &dir);
	
};


#endif
