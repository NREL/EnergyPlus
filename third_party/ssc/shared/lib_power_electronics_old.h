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

#ifndef _power_electronics_h_
#define _power_electronics_h_

#include "lib_battery_dispatch.h"
#include "lib_sandia.h"
#include "lib_pvinv.h"

class inverter
{
public:
	inverter(int inverter_type, int) {
		_inverter_type = inverter_type;
	}

	void set_sandia(sandia_inverter_t * sandia_inverter) { _sandia_inverter = sandia_inverter; }
	void set_partlaod(partload_inverter_t * partload_inverter) { _partload_inverter = partload_inverter; }


	void ac_power(double P_dc, double V_dc, double &P_ac)
	{
		double P_par, P_lr, Eff, P_cliploss, P_soloss, P_ntloss;
		if (_inverter_type == SANDIA_INVERTER)
			_sandia_inverter->acpower(P_dc, V_dc, &P_ac, &P_par, &P_lr, &Eff, &P_cliploss, &P_soloss, &P_ntloss);
		else if (_inverter_type == PARTLOAD_INVERTER)
			_partload_inverter->acpower(P_dc, &P_ac, &P_lr, &P_par, &Eff, &P_cliploss, &P_ntloss);
	}
	enum { SANDIA_INVERTER, DATASHEET_INVERTER, PARTLOAD_INVERTER, COEFFICIENT_GENERATOR, NONE };

protected:
	int _inverter_type;
	int _num_inverters;

	sandia_inverter_t * _sandia_inverter;
	partload_inverter_t * _partload_inverter;
};


class bidirectional_inverter
{
public:

	bidirectional_inverter(double ac_dc_efficiency, double dc_ac_efficiency)
	{
		_dc_ac_efficiency = 0.01*dc_ac_efficiency;
		_ac_dc_efficiency = 0.01*ac_dc_efficiency;
	}

	double dc_ac_efficiency() { return _dc_ac_efficiency; }
	double ac_dc_efficiency() { return _ac_dc_efficiency; }


	// return power loss [kW]
	double convert_to_dc(double P_ac, double * P_dc);
	double convert_to_ac(double P_dc, double * P_ac);

	// return increased power required, i.e 9 kWac may require 10 kWdc
	double compute_dc_from_ac(double P_ac);


protected:
	double _dc_ac_efficiency;
	double _ac_dc_efficiency;

	double _loss_dc_ac;
	double _loss_ac_dc;
};

class dc_dc_charge_controller
{
public:

	dc_dc_charge_controller(double batt_dc_dc_bms_efficiency, double pv_dc_dc_mppt_efficiency)
	{
		_batt_dc_dc_bms_efficiency = 0.01*batt_dc_dc_bms_efficiency;
		_pv_dc_dc_mppt_efficiency = 0.01*pv_dc_dc_mppt_efficiency;

	}

	double batt_dc_dc_bms_efficiency() { return _batt_dc_dc_bms_efficiency; };
	double pv_dc_dc_mppt_efficiency() { return _pv_dc_dc_mppt_efficiency; };


protected:
	double _batt_dc_dc_bms_efficiency;
	double _pv_dc_dc_mppt_efficiency;
	double _loss_dc_dc;
};

class rectifier
{
public:
	// don't know if I need this component in AC or DC charge controllers
	rectifier(double ac_dc_efficiency) { _ac_dc_efficiency = 0.01 * ac_dc_efficiency; }
	double ac_dc_efficiency() { return _ac_dc_efficiency; }

	// return power loss [kW]
	double convert_to_dc(double P_ac, double * P_dc);

protected:
	double _ac_dc_efficiency;
	double _loss_dc_ac;
};
// Charge controller base class
class charge_controller
{
public:
	charge_controller(dispatch_t * dispatch, battery_metrics_t * battery_metrics, double efficiency_1, double efficiency_2);
	virtual ~charge_controller();

	void initialize(double P_pv, double P_load, double P_pv_clipped, size_t index);

	// function to determine appropriate pv and load to send to battery
	virtual void preprocess_pv_load() = 0;

	virtual void run(size_t year, size_t hour_of_year, size_t step_of_hour, size_t index, double P_pv, double P_load, double P_pv_clipped) = 0;
	virtual void compute_to_batt_load_grid(double P_battery_ac_or_dc, double P_pv_ac_or_dc, double P_load_ac, double inverter_efficiency) = 0;

	// return power loss [kW]
	virtual double gen_ac() = 0;
	virtual double update_gen_ac(double P_gen_ac) = 0;

	void finalize();

	// iterative solution required
	bool check_iterate();
	void check_constraints();

	// ac outputs
	double power_tofrom_battery() { return _P_battery; }
	double power_tofrom_grid() { return _P_grid; }
	double power_gen() { return _P_gen; }
	double power_pv_to_load() { return _P_pv_to_load; }
	double power_battery_to_load() { return _P_battery_to_load; }
	double power_grid_to_load() { return _P_grid_to_load; }
	double power_pv_to_batt() { return _P_pv_to_battery; }
	double power_grid_to_batt() { return _P_grid_to_batt; }
	double power_pv_to_grid() { return _P_pv_to_grid; }
	double power_battery_to_grid() { return _P_battery_to_grid; }
	double power_conversion_loss() { return _P_loss; }
	double power_system_loss() { return _P_system_loss; }

	enum { DC_CONNECTED, AC_CONNECTED };

protected:

	dispatch_t * _dispatch;
	dispatch_t * _dispatch_initial;

	battery_metrics_t * _battery_metrics;

	// ac powers to report
	double _P_load;
	double _P_grid;
	double _P_grid_to_load;
	double _P_grid_to_batt;
	double _P_gen;
	double _P_battery_to_load;
	double _P_pv_to_load;
	double _P_pv_to_battery;
	double _P_pv_to_grid;
	double _P_battery_to_grid;
	double _P_battery;
	double _P_system_loss;
	double _P_loss;
	double _P_inverter_draw;

	// ac or dc pv input
	double _P_pv;

	// inputs to battery dispatch
	double _P_pv_dc_discharge_input;
	double _P_pv_dc_charge_input;
	double _P_load_dc_discharge_input;
	double _P_load_dc_charge_input;
	double _P_pv_dc_clipped;

	// does battery need to iterate
	bool _iterate;

	// index of simulation
	size_t _index;

};

class dc_connected_battery_controller : public charge_controller
{
public:
	dc_connected_battery_controller(dispatch_t * dispatch,
		battery_metrics_t * battery_metrics,
		double batt_dc_dc_bms_efficiency,
		double pv_dc_dc_mppt_efficiency,
		double inverter_efficiency);
	~dc_connected_battery_controller();

	void preprocess_pv_load();
	void run(size_t year, size_t hour_of_year, size_t step_of_hour, size_t index, double P_pv, double P_load, double P_pv_clipped = 0);
	void process_dispatch();
	void compute_to_batt_load_grid(double P_battery_ac, double P_pv_dc, double P_load_ac, double inverter_efficiency);
	double gen_ac() { return 0.; };
	double update_gen_ac(double P_gen_ac);


protected:
	dc_dc_charge_controller * _dc_dc_charge_controller;
	double _inverter_efficiency;
};

class ac_connected_battery_controller : public charge_controller
{
public:
	ac_connected_battery_controller(dispatch_t * dispatch, battery_metrics_t * battery_metrics, double ac_dc_efficiency, double dc_ac_efficiency);
	~ac_connected_battery_controller();

	void preprocess_pv_load();
	void run(size_t year, size_t hour_of_year, size_t step_of_hour, size_t index, double P_pv, double P_load, double P_pv_clipped = 0);
	void process_dispatch();
	void compute_to_batt_load_grid(double P_battery_ac, double P_pv_ac, double P_load_ac, double inverter_efficiency = 0.);

	double gen_ac();
	double update_gen_ac(double)
	{
		// nothing to do
		return 0.;
	}

protected:
	bidirectional_inverter * _bidirectional_inverter;
};

#endif