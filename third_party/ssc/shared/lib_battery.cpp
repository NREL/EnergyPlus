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

#include <cmath>
#include <cfloat>
#include <sstream>
#include <algorithm>
#include <numeric>
#include <functional>

#include "6par_newton.h"
#include "lib_battery.h"



/*
Message class
*/
void message::add(std::string message)
{
	std::vector<std::string>::iterator it;
	it = std::find(messages.begin(), messages.end(), message);
	if (it == messages.end())
	{
		messages.push_back(message);
		count.push_back(1);
	}
	else
		count[it - messages.begin()]++;

}
size_t message::total_message_count(){ return messages.size(); }
size_t message::message_count(int index)
{
	if (index < (int)messages.size())
		return count[index];
	else
		return 0;
}
std::string message::get_message(int index)
{
	if (index < (int)messages.size())
		return messages[index];
	else
		return NULL;
}
std::string message::construct_log_count_string(int index)
{
	std::ostringstream oss;
	oss << count[index];

	std::string message_count = oss.str();
	std::string log = messages[index] + " - warning occurred: " + message_count + " times";
	return log;
}

/*
Define Capacity Model
*/
capacity_t::capacity_t() { /* nothing to do */ }
capacity_t::capacity_t(double q, double SOC_init, double SOC_max, double SOC_min)
{
	_q0 = 0.01*SOC_init*q;
	_qmax = q;
	_qmax_thermal = q;
	_qmax0 = q;
	_I = 0.;
	_I_loss = 0.;
	_dt_hour = 0.;

	// Initialize SOC, DOD
	_SOC = SOC_init;
	_SOC_init = SOC_init;
	_SOC_max = SOC_max;
	_SOC_min = SOC_min;
	_DOD = 100. - _SOC;
	_DOD_prev = 0;

	// Initialize charging states
	_prev_charge = DISCHARGE;
	_charge = DISCHARGE;
	_chargeChange = false;
}
void capacity_t::copy(capacity_t * capacity)
{
	_q0 = capacity->_q0;
	_qmax = capacity->_qmax;
	_qmax_thermal = capacity->_qmax_thermal;
	_qmax0 = capacity->_qmax0;
	_I = capacity->_I;
	_I_loss = capacity->_I_loss;
	_SOC = capacity->_SOC;
	_SOC_init = capacity->_SOC_init;
	_SOC_min = capacity->_SOC_min;
	_SOC_max = capacity->_SOC_max;
	_DOD = capacity->_DOD;
	_DOD_prev = capacity->_DOD_prev;
	_dt_hour = capacity->_dt_hour;
	_chargeChange = capacity->_chargeChange;
	_prev_charge = capacity->_prev_charge;
	_charge = capacity->_charge;
}
void capacity_t::check_charge_change()
{
	_charge = NO_CHARGE;

	// charge state
	if (_I < 0)
		_charge = CHARGE;
	else if (_I > 0)
		_charge = DISCHARGE;

	// Check if charge changed
	_chargeChange = false;
	if ((_charge != _prev_charge) && (_charge != NO_CHARGE) && (_prev_charge != NO_CHARGE))
	{
		_chargeChange = true;
		_prev_charge = _charge;
	}
}
int capacity_t::charge_operation(){ return _charge; }
void capacity_t::check_SOC()
{
	double q_upper = _qmax * _SOC_max * 0.01;
	double q_lower = _qmax * _SOC_min * 0.01;
	double I_orig = _I;

	// set capacity to upper thermal limit
	if (q_upper > _qmax_thermal * _SOC_max * 0.01) {
		q_upper = _qmax_thermal * _SOC_max * 0.01;
	}
	// do this so battery can cycle full depth and we calculate correct SOC min
	if (q_lower > _qmax_thermal * _SOC_min * 0.01) {
		q_lower = _qmax_thermal * _SOC_min * 0.01;
	}

	// check if overcharged
	if (_q0 > q_upper )
	{
		if (fabs(_I) > tolerance)
		{
			_I += (_q0 - q_upper) / _dt_hour;
			if (_I / I_orig < 0)
				_I = 0;
		}
		_q0 = q_upper;
	}
	// check if undercharged
	else if (_q0 < q_lower)
	{
		if (fabs(_I) > tolerance)
		{
			_I += (_q0 - q_lower) / _dt_hour;
			if (_I / I_orig < 0)
				_I = 0;
		}
		_q0 = q_lower;
	}
}

void capacity_t::update_SOC()
{
	if (_qmax > 0)
		_SOC = 100.*(_q0 / _qmax_thermal);
	else
		_SOC = 0.;

	// due to dynamics, it's possible SOC could be slightly above 1 or below 0
	if (_SOC > 100.0)
		_SOC = 100.0;
	else if (_SOC < 0.)
		_SOC = 0.;

	_DOD = 100. - _SOC;
}
bool capacity_t::chargeChanged(){return _chargeChange;}
double capacity_t::SOC(){ return _SOC; }
double capacity_t::DOD(){ return _DOD; }
double capacity_t::DOD_max(){ return _SOC_max - _SOC_min; }
double capacity_t::prev_DOD(){ return _DOD_prev; }
double capacity_t::q0(){ return _q0;}
double capacity_t::qmax(){ return _qmax; }
double capacity_t::qmax_thermal(){ return _qmax_thermal; }
double capacity_t::I(){ return _I; }
double capacity_t::I_loss() { return _I_loss; }

/*
Define KiBam Capacity Model
*/
capacity_kibam_t::capacity_kibam_t(){ /* nothing to do */}
capacity_kibam_t::capacity_kibam_t(double q20, double t1, double q1, double q10, double SOC_init, double SOC_max, double SOC_min) :
capacity_t(q20, SOC_init, SOC_max, SOC_min)
{
	_q10 = q10;
	_q20 = q20;
	_I20 = q20/20.;

	// parameters for c, k calculation
	_q1 = q1;
	_q2 = q10;
	_t1 = t1;
	_t2 = 10.;
	_F1 = q1 / q20; // use t1, 20
	_F2 = q1 / q10;  // use t1, 10

	// compute the parameters
	parameter_compute();
	_qmax0 = _qmax;

	// initializes to full battery
	replace_battery(100);
}
capacity_kibam_t * capacity_kibam_t::clone(){ return new capacity_kibam_t(*this); }
void capacity_kibam_t::copy(capacity_t * capacity)
{
	capacity_t::copy(capacity);
	capacity_kibam_t * tmp = dynamic_cast<capacity_kibam_t*>(capacity);

	_t1 = tmp->_t1;
	_t2 = tmp->_t2;
	_q1 = tmp->_q1;
	_q2 = tmp->_q2;
	_F1 = tmp->_F1;
	_F2 = tmp->_F2;
	_c = tmp->_c;
	_k = tmp->_k;
	_q1_0 = tmp->_q1_0;
	_q2_0 = tmp->_q2_0;
	_q10 = tmp->_q10;
	_q20 = tmp->_q20;
	_I20 = tmp->_I20;
}

void capacity_kibam_t::replace_battery(double replacement_percent)
{
	_qmax += replacement_percent * 0.01* _qmax0;
	_qmax = fmin(_qmax, _qmax0);
	_q0 = _qmax*_SOC_init*0.01;
	_q1_0 = _q0*_c;
	_q2_0 = _q0 - _q1_0;
	_SOC = _SOC_init;
}

double capacity_kibam_t::c_compute(double F, double t1, double t2, double k_guess)
{
	double num = F*(1 - exp(-k_guess*t1))*t2 - (1 - exp(-k_guess*t2))*t1;
	double denom = F*(1 - exp(-k_guess*t1))*t2 - (1 - exp(-k_guess*t2))*t1 - k_guess*F*t1*t2 + k_guess*t1*t2;
	return (num / denom);
}

double capacity_kibam_t::q1_compute(double q10, double q0, double dt, double I)
{
	double A = q10*exp(-_k*dt);
	double B = (q0*_k*_c - I)*(1 - exp(-_k*dt)) / _k;
	double C = I*_c*(_k*dt - 1 + exp(-_k*dt)) / _k;
	return (A + B - C);
}

double capacity_kibam_t::q2_compute(double q20, double q0, double dt, double I)
{
	double A = q20*exp(-_k*dt);
	double B = q0*(1 - _c)*(1 - exp(-_k*dt));
	double C = I*(1 - _c)*(_k*dt - 1 + exp(-_k*dt)) / _k;
	return (A + B - C);
}

double capacity_kibam_t::Icmax_compute(double q10, double q0, double dt)
{
	double num = -_k*_c*_qmax + _k*q10*exp(-_k*dt) + q0*_k*_c*(1 - exp(-_k*dt));
	double denom = 1 - exp(-_k*dt) + _c*(_k*dt - 1 + exp(-_k*dt));
	return (num / denom);
}

double capacity_kibam_t::Idmax_compute(double q10, double q0, double dt)
{
	double num = _k*q10*exp(-_k*dt) + q0*_k*_c*(1 - exp(-_k*dt));
	double denom = 1 - exp(-_k*dt) + _c*(_k*dt - 1 + exp(-_k*dt));
	return (num / denom);
}

double capacity_kibam_t::qmax_compute()
{
	double num = _q20*((1 - exp(-_k * 20)) * (1 - _c) + _k*_c * 20);
	double denom = _k*_c * 20;
	return (num / denom);
}

double capacity_kibam_t::qmax_of_i_compute(double T)
{
	return ((_qmax*_k*_c*T) / (1 -exp(-_k*T) + _c*(_k*T - 1 + exp(-_k*T))));
}
void capacity_kibam_t::parameter_compute()
{
	double k_guess = 0.;
	double c1 = 0.;
	double c2 = 0.;
	double minRes = 10000.;

	for (int i = 0; i < 5000; i++)
	{
		k_guess = i*0.001;
		c1 = c_compute(_F1, _t1, 20, k_guess);
		c2 = c_compute(_F2, _t1, _t2, k_guess);

		if (fabs(c1 - c2) < minRes)
		{
			minRes = fabs(c1 - c2);
			_k = k_guess;
			_c = 0.5*(c1 + c2);
		}
	}
	_qmax = qmax_compute();
}

void capacity_kibam_t::updateCapacity(double &I, double dt_hour)
{
	if (fabs(I) < low_tolerance)
		I = 0;

	_DOD_prev = _DOD;
	_I_loss = 0.;
	_I = I;
	_dt_hour = dt_hour;

	double Idmax = 0.;
	double Icmax = 0.;
	double Id = 0.;
	double Ic = 0.;
	double q1 = 0.;
	double q2 = 0.;

	if (_I > 0)
	{
		Idmax = Idmax_compute(_q1_0, _q0, dt_hour);
		Id = fmin(_I, Idmax);
		_I = Id;
	}
	else if (_I < 0)
	{
		Icmax = Icmax_compute(_q1_0, _q0, dt_hour);
		Ic = -fmin(fabs(_I), fabs(Icmax));
		_I = Ic;
	}

	// new charge levels
	q1 = q1_compute(_q1_0, _q0, dt_hour, _I);
	q2 = q2_compute(_q2_0, _q0, dt_hour, _I);

	// Check for thermal effects
	if (q1 + q2 > _qmax_thermal)
	{
		double q0 = q1 + q2;
		double p1 = q1 / q0;
		double p2 = q2 / q0;
		_q0 = _qmax_thermal;
		q1 = _q0*p1;
		q2 = _q0*p2;
	}

	// update internal variables
	_q1_0 = q1;
	_q2_0 = q2;
	_q0 = q1 + q2;

	update_SOC();
	check_charge_change();

	// Pass current out
	I = _I;
}
void capacity_kibam_t::updateCapacityForThermal(double capacity_percent)
{
	// Modify the lifetime degraded capacity by the thermal effect
	_qmax_thermal = _qmax*capacity_percent*0.01;
}
void capacity_kibam_t::updateCapacityForLifetime(double capacity_percent)
{

	if (_qmax0* capacity_percent*0.01 <= _qmax)
		_qmax = _qmax0* capacity_percent*0.01;

	// scale to q0 = qmax if q0 > qmax
	if (_q0 > _qmax)
	{
		double q0_orig = _q0;
		double p = _qmax / _q0;
		_q0 *= p;
		_q1 *= p;
		_q2 *= p;
		_I_loss += (q0_orig - _q0) / _dt_hour;
	}
	update_SOC();
}

double capacity_kibam_t::q1(){ return _q1_0; }
double capacity_kibam_t::q2(){ return _q2_0; }
double capacity_kibam_t::q10(){ return _q10; }
double capacity_kibam_t::q20(){return _q20;}


/*
Define Lithium Ion capacity model
*/
capacity_lithium_ion_t::capacity_lithium_ion_t() { /* nothing to do */ }
capacity_lithium_ion_t::capacity_lithium_ion_t(double q, double SOC_init, double SOC_max, double SOC_min) :capacity_t(q, SOC_init, SOC_max, SOC_min){};
capacity_lithium_ion_t * capacity_lithium_ion_t::clone(){ return new capacity_lithium_ion_t(*this); }
void capacity_lithium_ion_t::copy(capacity_t * capacity){ capacity_t::copy(capacity);}

void capacity_lithium_ion_t::replace_battery(double replacement_percent)
{
	_qmax += _qmax0 * replacement_percent * 0.01;
	_qmax = fmin(_qmax0, _qmax);
	_q0 = _qmax * _SOC_init * 0.01;
	_qmax_thermal = _qmax;
	_SOC = _SOC_init;
}
void capacity_lithium_ion_t::updateCapacity(double &I, double dt)
{
	_DOD_prev = _DOD;
	_I_loss = 0.;
	_dt_hour = dt;
	_I = I;

	// compute charge change ( I > 0 discharging, I < 0 charging)
	_q0 -= _I*dt;

	// check if SOC constraints violated, update q0, I if so
	check_SOC();

	// update SOC, DOD
	update_SOC();
	check_charge_change();

	// Pass current out
	I = _I;
}
void capacity_lithium_ion_t::updateCapacityForThermal(double capacity_percent)
{
	// Modify the lifetime degraded capacity by the thermal effect
	_qmax_thermal = _qmax*capacity_percent*0.01;
}
void capacity_lithium_ion_t::updateCapacityForLifetime(double capacity_percent)
{

	if (_qmax0* capacity_percent*0.01 <= _qmax)
		_qmax = _qmax0* capacity_percent*0.01;

	if (_q0 > _qmax)
	{
		_I_loss += (_q0 - _qmax) / _dt_hour;
		_q0 = _qmax;
	}

	update_SOC();
}
double capacity_lithium_ion_t::q1(){return _q0;}
double capacity_lithium_ion_t::q10(){return _qmax;}


/*
Define Voltage Model
*/
voltage_t::voltage_t(int mode, int num_cells_series, int num_strings, double voltage, double dt_hour)
{
	_mode = mode;
	_num_cells_series = num_cells_series;
	_num_strings = num_strings;
	_cell_voltage = voltage;
	_cell_voltage_nominal = voltage;
	_R = 0.004; // just a default, will get recalculated upon construction
	_R_battery = _R * num_cells_series / num_strings;
    if (dt_hour < 1/60.)
        throw std::runtime_error("Battery time step size must be greater than 1/60th of hour.");
    dt_hr = dt_hour;
}
void voltage_t::copy(voltage_t * voltage)
{
	_mode = voltage->_mode;
	_num_cells_series = voltage->_num_cells_series;
	_num_strings = voltage->_num_strings;
	_cell_voltage = voltage->_cell_voltage;
	_cell_voltage_nominal = voltage->_cell_voltage_nominal;
	_R = voltage->_R;
	_R_battery = voltage->_R_battery;
    dt_hr = voltage->dt_hr;


}
double voltage_t::battery_voltage(){ return _num_cells_series*_cell_voltage; }
double voltage_t::battery_voltage_nominal(){ return _num_cells_series * _cell_voltage_nominal; }
double voltage_t::cell_voltage(){ return _cell_voltage; }
double voltage_t::R_battery(){ return _R_battery; }

// Voltage Table
voltage_table_t::voltage_table_t(int num_cells_series, int num_strings, double voltage, util::matrix_t<double> &voltage_table, double R, double dt_hour) :
        voltage_t(voltage_t::VOLTAGE_TABLE, num_cells_series, num_strings, voltage, dt_hour)
{
	_R = R;

    // extract and sort calendar life info from table
	for (int r = 0; r != (int)voltage_table.nrows(); r++)
		m_voltage_table.emplace_back(std::make_pair(voltage_table.at(r, 0), voltage_table.at(r, 1)));

	std::sort(m_voltage_table.begin(), m_voltage_table.end(), [](std::pair<double, double> a, std::pair<double, double> b) {return a.second > b.second; });

	// save slope and intercept for every set of points to interpolate between
    for (size_t i = 0; i != m_voltage_table.size(); i++) {
        double DOD = m_voltage_table[i].first;
        double V = m_voltage_table[i].second;
        double slope = 0;
        double intercept = V;
        if (i > 0){
            double DOD0 = m_voltage_table[i-1].first;
            double V0 = m_voltage_table[i-1].second;
            slope = (V - V0)/(DOD - DOD0);
            intercept = V0 - (slope * DOD0);
        }
        slopes.emplace_back(slope);
        intercepts.emplace_back(intercept);
    }

    // for extrapolation beyond given points
    slopes.emplace_back(slopes.back());
    intercepts.emplace_back(intercepts.back());

}

voltage_table_t * voltage_table_t::clone(){ return new voltage_table_t(*this); }
void voltage_table_t::copy(voltage_t * voltage)
{
	voltage_t::copy(voltage);

	// doesn't change and may be slow to copy
	/*
	voltage_table_t * tmp = dynamic_cast<voltage_table_t*>(voltage);
	_voltage_table = tmp->_voltage_table;
	*/
}

double voltage_table_t::calculate_voltage(double DOD) {
    DOD = fmax(0., DOD);
    DOD = fmin(DOD, 100.);

    size_t row = 0;
    while (row < m_voltage_table.size() && DOD > m_voltage_table[row].first){
        row++;
    }

    return fmax(slopes[row] * DOD + intercepts[row], 0);
}

double voltage_table_t::calculate_voltage_for_current(double I, double q, double qmax, double) {
    double DOD = (q - I * dt_hr)/qmax * 100.;
    return calculate_voltage(DOD) * _num_cells_series;
}


void voltage_table_t::updateVoltage(capacity_t * capacity, thermal_t * , double )
{
    _cell_voltage = calculate_voltage(capacity->DOD());
}

// helper fx to calculate depth of discharge from current and max capacities
inline double calc_DOD(double q, double qmax) {return (1. - q/qmax) * 100.;}

double voltage_table_t::calculate_max_charge_w(double q, double qmax, double, double *max_current) {
	double current = (q - qmax) / dt_hr;
	if (max_current)
		*max_current = current;
	return calculate_voltage(0.) * current * _num_cells_series;
}

double voltage_table_t::calculate_max_discharge_w(double q, double qmax, double, double *max_current) {
    double DOD0 = calc_DOD(q, qmax);
    double A = q - qmax;
    double B = qmax/100.;

	double max_P = 0;
	double max_I = 0;
	for (size_t i = 0; i < slopes.size(); i++) {
		double dod = -(A * slopes[i] + B * intercepts[i]) / (2 * B * slopes[i]);
		double current = qmax * ((1. - DOD0 / 100.) - (1. - dod / 100.)) / dt_hr;
		double p = calculate_voltage(dod) * current;
		if (p > max_P) {
			max_P = p;
			max_I = current;
		}
	}
	if (max_current)
		*max_current = fmax(0, max_I);
	return max_P * _num_cells_series;
}

double voltage_table_t::calculate_current_for_target_w(double P_watts, double q, double qmax, double) {
    double DOD = calc_DOD(q, qmax);
    double max_p, current;
    if (P_watts == 0)
        return 0.;
    else if (P_watts < 0)
        max_p = calculate_max_charge_w(q, qmax, 0, &current);
    else
        max_p = calculate_max_discharge_w(q, qmax, 0, &current);

    if (fabs(max_p) < fabs(P_watts))
        return current;

	P_watts /= _num_cells_series;
	P_watts *= dt_hr;
    double multiplier = 1.;
    if (P_watts < 0)
        multiplier = -1.;

    size_t row = 0;
    while (row < m_voltage_table.size() && DOD > m_voltage_table[row].first){
        row++;
    }

    double A = q - qmax;
    double B = qmax/100.;

	double DOD_new = 0.;
	double incr = 0;
	// Assume the DOD goes to 0 while charging, 100 while discharging
	double DOD_best = multiplier == -1. ? 0 : 100;
	double P_best = 0;
	while (incr + row < slopes.size() && incr + row >= 0) {
		size_t i = row + (size_t)incr;
		incr += 1 * multiplier;

		double a = B * slopes[i];
		double b = A * slopes[i] + B * intercepts[i];
		double c = A * intercepts[i] - P_watts;

		if (a == 0) {
			continue;
		}

		DOD_new = fabs((-b + sqrt(b * b - 4 * a * c)) / (2 * a));

		auto upper = (size_t)fmin(i, m_voltage_table.size() - 1);
		auto lower = (size_t)fmax(0, i - 1);
		auto DOD_upper = m_voltage_table[upper].first;
		auto DOD_lower = m_voltage_table[lower].first;
		if (DOD_new <= DOD_upper && DOD_new >= DOD_lower) {
			double P = (q - (100. - DOD_new) * qmax / 100) * (a * DOD_new + b);
			if (fabs(P) > fabs(P_best)) {
				P_best = P;
				DOD_best = DOD_new;
			}
		}
	}
	return qmax * ((1. - DOD / 100.) - (1. - DOD_best / 100.)) / dt_hr;
}

// Dynamic voltage model
typedef void (voltage_dynamic_t::*voltage_dynamic_fptr)(const double*, double*);

voltage_dynamic_t::voltage_dynamic_t(int num_cells_series, int num_strings, double voltage, double Vfull,
                                     double Vexp, double Vnom, double Qfull, double Qexp, double Qnom,
                                     double C_rate, double R, double dt_hour) :
        voltage_t(voltage_t::VOLTAGE_MODEL, num_cells_series, num_strings, voltage, dt_hour)
{
	_Vfull = Vfull;
	_Vexp = Vexp;
	_Vnom = Vnom;
	_Qfull = Qfull;
	_Qexp = Qexp;
	_Qnom = Qnom;
	_C_rate = C_rate;
	_R = R;
	_R_battery = _R * num_cells_series / num_strings;

	// assume fully charged, not the nominal value
	_cell_voltage = _Vfull;

	parameter_compute();
}

voltage_dynamic_t * voltage_dynamic_t::clone(){ return new voltage_dynamic_t(*this); }
void voltage_dynamic_t::copy(voltage_t * voltage)
{

	voltage_t::copy(voltage);
	voltage_dynamic_t * tmp = dynamic_cast<voltage_dynamic_t*>(voltage);

	_Vfull = tmp->_Vfull;
	_Vexp = tmp->_Vexp;
	_Vnom = tmp->_Vnom;
	_Qfull = tmp->_Qfull;
	_Qexp = tmp->_Qexp;
	_Qnom = tmp->_Qnom;
	_C_rate = tmp->_C_rate;
	_A = tmp->_A;
	_B0 = tmp->_B0;
	_E0 = tmp->_E0;
	_K = tmp->_K;

	solver_power = tmp->solver_power;
	solver_Q = tmp->solver_Q;
	solver_q = tmp->solver_q;
	solver_cutoff_voltage = tmp->solver_cutoff_voltage;
}
void voltage_dynamic_t::parameter_compute()
{
	// Determines parameters according to page 2 of:
	// Tremblay 2009 "A Generic Bettery Model for the Dynamic Simulation of Hybrid Electric Vehicles"
//	double eta = 0.995;
	double I = _Qfull*_C_rate; // [A]
	//_R = _Vnom*(1. - eta) / (_C_rate*_Qnom); // [Ohm]
	_A = _Vfull - _Vexp; // [V]
	_B0 = 3. / _Qexp;     // [1/Ah]
	_K = ((_Vfull - _Vnom + _A*(std::exp(-_B0*_Qnom) - 1))*(_Qfull - _Qnom)) / (_Qnom); // [V] - polarization voltage
	_E0 = _Vfull + _K + _R*I - _A;

	if (_A < 0 || _B0 < 0 || _K < 0 || _E0 < 0){
	    char err[254];
	    std::sprintf(err, "Error during calculation of battery voltage model parameters: negative value(s) found.\n"
                                  "A: %f, B: %f, K: %f, E0: %f", _A, _B0, _K, _E0);
	    throw std::runtime_error(err);
	}
}

// everything in here is on a per-cell basis
double voltage_dynamic_t::voltage_model_tremblay_hybrid(double Q_cell, double I, double q0_cell)
{
    double it = Q_cell - q0_cell;
    double E = _E0 - _K*(Q_cell / (Q_cell - it)) + _A * exp(-_B0 * it);
    return E - _R*I;
}

double voltage_dynamic_t::calculate_voltage_for_current(double I, double q, double qmax, double)
{
    return _num_cells_series * fmax(voltage_model_tremblay_hybrid(qmax/_num_strings, I/_num_strings , q/_num_strings), 0);
}

// I, Q, q0 are on a per-string basis since adding cells in series does not change current or charge
void voltage_dynamic_t::updateVoltage(capacity_t * capacity, thermal_t * , double )
{
	double Q = capacity->qmax() / _num_strings;
	double I = capacity->I() / _num_strings;
	double q0 = capacity->q0() / _num_strings;

	_cell_voltage = fmax(voltage_model_tremblay_hybrid(Q, I , q0), 0);
}

double voltage_dynamic_t::calculate_max_charge_w(double q, double qmax, double, double *max_current) {
    q /= _num_strings;
    qmax /= _num_strings;
    double current = (q - qmax) / dt_hr;
    if (max_current)
        *max_current = current * _num_strings;
    return current * voltage_model_tremblay_hybrid(qmax, current , qmax) * _num_strings * _num_cells_series;
}

using namespace std::placeholders;
double voltage_dynamic_t::calculate_max_discharge_w(double q, double qmax, double, double *max_current) {
    q /= _num_strings;
    qmax /= _num_strings;

    double current = 0., vol = 0;
    double incr = q / 10;
    double max_p = 0, max_I = 0;
    while (current * dt_hr < q - tolerance && vol >= 0){
        vol = voltage_model_tremblay_hybrid(qmax, current , q - current * dt_hr);
        double p = current * vol;
        if (p > max_p){
            max_p = p;
            max_I = current;
        }
        current += incr;
    }
    current = max_I;

    if (max_current)
        *max_current = current * _num_strings;

    return max_p * _num_strings * _num_cells_series;
}

double voltage_dynamic_t::calculate_current_for_target_w(double P_watts, double q, double qmax, double) {
    if (P_watts == 0) return 0.;

    solver_power = fabs(P_watts) / (_num_cells_series * _num_strings);
    solver_q = q /_num_strings;
    solver_Q = qmax / _num_strings;

    std::function<void(const double*, double*)> f;
    double direction = 1.;
    if (P_watts > 0)
        f = std::bind(&voltage_dynamic_t::solve_current_for_discharge_power, this, _1, _2);
    else{
        f = std::bind(&voltage_dynamic_t::solve_current_for_charge_power, this, _1, _2);
        direction = -1.;
    }

    double x[1], resid[1];
    x[0] = solver_power / _cell_voltage;
    bool check = false;

    newton<double, std::function<void(const double*, double*)>, 1>( x, resid, check, f,
                                                                                100, 1e-6, 1e-6, 0.7);
    return x[0] * _num_strings * direction;
}

void voltage_dynamic_t::solve_current_for_charge_power(const double *x, double *f){
    double I = x[0];
    double V = _E0 - _K*solver_Q/(solver_q+I*dt_hr) + _A*exp(-_B0*(solver_Q-(solver_q+I*dt_hr))) + _R*I;
    f[0] = I*V - solver_power;
}

void voltage_dynamic_t::solve_current_for_discharge_power(const double *x, double *f){
    double I = x[0];
    double V = _E0 - _K*solver_Q/(solver_q-I*dt_hr) + _A*exp(-_B0*(solver_Q-(solver_q-I*dt_hr))) - _R*I;
    f[0] = I*V - solver_power;
}

// Vanadium redox flow model
voltage_vanadium_redox_t::voltage_vanadium_redox_t(int num_cells_series, int num_strings, double V_ref_50, double R,
                                                   double dt_hour) :
        voltage_t(voltage_t::VOLTAGE_MODEL, num_cells_series, num_strings, V_ref_50, dt_hour)
{
	_V_ref_50 = V_ref_50;
	_R = R;
    m_RCF = 8.314 * 1.38 / (26.801 * 3600);
    if (dt_hr < 1/60.)
        throw std::runtime_error("Battery time step size must be greater than 1/60th of hour.");
    dt_hr = dt_hr;
}
voltage_vanadium_redox_t * voltage_vanadium_redox_t::clone(){ return new voltage_vanadium_redox_t(*this); }
void voltage_vanadium_redox_t::copy(voltage_t * voltage)
{
	voltage_t::copy(voltage);
	voltage_vanadium_redox_t * tmp = dynamic_cast<voltage_vanadium_redox_t*>(voltage);

	_V_ref_50 = tmp->_V_ref_50;
	_R = tmp->_R;
	m_RCF = tmp->m_RCF;
	dt_hr = tmp->dt_hr;
	solver_power = tmp->solver_power;
	solver_T_k = tmp->solver_T_k;
	solver_q = tmp->solver_q;
	solver_Q = tmp->solver_Q;
}

double voltage_vanadium_redox_t::calculate_voltage_for_current(double I, double q, double qmax, double T_k)
{
    return voltage_model(q / _num_strings, qmax / _num_strings,
                         I/ _num_strings, T_k) * _num_cells_series;
}

void voltage_vanadium_redox_t::updateVoltage(capacity_t * capacity, thermal_t * thermal, double )
{
	_cell_voltage = voltage_model(capacity->q0() / _num_strings, capacity->qmax() / _num_strings,
                                  capacity->I()/ _num_strings, thermal->T_battery());
}

double voltage_vanadium_redox_t::calculate_max_charge_w(double q, double qmax, double kelvin, double *max_current) {
    qmax /= _num_strings;
    q /= _num_strings;
    double max_I = (q - qmax) / dt_hr;

    if (max_current)
        *max_current = max_I * _num_strings;

    return voltage_model(qmax, qmax, max_I, kelvin) * max_I * _num_strings * _num_cells_series;
}

double voltage_vanadium_redox_t::calculate_max_discharge_w(double q, double qmax, double kelvin, double *max_current) {

    solver_q = q / _num_strings;
    solver_Q = qmax / _num_strings;
    solver_T_k = kelvin;

    std::function<void(const double *, double *)> f = std::bind(&voltage_vanadium_redox_t::solve_max_discharge_power,
                                                                this, _1, _2);

    double x[1], resid[1];
    x[0] = solver_q - tolerance;
    bool check = false;

    newton<double, std::function<void(const double *, double *)>, 1>(x, resid, check, f,
                                                                     100, 1e-6, 1e-6, 0.7);
    double current = x[0];

    double power = current * voltage_model(solver_q - current * dt_hr, solver_Q, current, kelvin) *
                   _num_strings * _num_cells_series;

    if (power < 0) {
        current = 0.;
        power = 0.;
    }
    if (max_current)
        *max_current = current * _num_strings;
    return power;
}

double voltage_vanadium_redox_t::calculate_current_for_target_w(double P_watts, double q, double qmax, double kelvin) {
    if (P_watts == 0) return 0.;

    solver_power = P_watts / (_num_cells_series * _num_strings);
    solver_q = q / _num_strings;
    solver_Q = qmax / _num_strings;
    solver_T_k = kelvin;

    std::function<void(const double *, double *)> f = std::bind(&voltage_vanadium_redox_t::solve_current_for_power,
                                                                this, _1, _2);

    double x[1], resid[1];
    x[0] = solver_power / _cell_voltage;
    bool check = false;

    newton<double, std::function<void(const double *, double *)>, 1>(x, resid, check, f,
                                                                     100, 1e-6, 1e-6, 0.7);
    return x[0] * _num_strings;
}

// I, Q, q0 are on a per-string basis since adding cells in series does not change current or charge
// In constrast to the V_stack + I_stack * R_specific in the paper which follows the convention of negative voltages,
// here the abs(I_stack) is used to allow both terms to move in same direction (https://github.com/NREL/ssc/issues/404)
double voltage_vanadium_redox_t::voltage_model(double q0, double qmax, double I_string, double T)
{
	double SOC_use = q0 / qmax;
	if (SOC_use > 1. - tolerance)
		SOC_use = 1. - tolerance;
	if (SOC_use == 0)
	    SOC_use = 1e-3;

	double A = std::log(std::pow(SOC_use, 2) / std::pow(1 - SOC_use, 2));
	return _V_ref_50 + m_RCF * T * A + fabs(I_string) * _R;
}

void voltage_vanadium_redox_t::solve_current_for_power(const double *x, double *f){
    double I = x[0];
    double SOC = (solver_q - I * dt_hr) / solver_Q;
    f[0] = I * (_V_ref_50 + m_RCF * solver_T_k * std::log(SOC * SOC / std::pow(1. - SOC, 2)) +
                fabs(I) * _R) - solver_power;
}

void voltage_vanadium_redox_t::solve_max_discharge_power(const double *x, double *f){
    double I = fabs(x[0]);
    double SOC = (solver_q - I * dt_hr) / solver_Q;
    f[0] = _V_ref_50 + 2 * I * _R + m_RCF * solver_T_k * (std::log(SOC * SOC / pow(1. - SOC, 2)) -
                                                                2 * I * (1. / SOC - 1. / (1. - SOC)));
}

/*
Define Lifetime Model
*/
lifetime_t::lifetime_t(lifetime_cycle_t * lifetime_cycle, lifetime_calendar_t * lifetime_calendar, const int replacement_option, const double replacement_capacity)
{
	_lifetime_cycle = lifetime_cycle;
	_lifetime_calendar = lifetime_calendar;

	_replacement_option = replacement_option;
	_replacement_capacity = replacement_capacity;
	_replacement_percent = 100;

	// issues as capacity approaches 0%
	if (replacement_capacity == 0.) { _replacement_capacity = 2.; }
	_replacements = 0;
	_replacement_scheduled = false;

	// relative capacity
	_q = 100;
}
lifetime_t * lifetime_t::clone()
{
	lifetime_t * tmp = new lifetime_t(*this);
	tmp->_lifetime_calendar = _lifetime_calendar->clone();
	tmp->_lifetime_cycle = _lifetime_cycle->clone();
	return tmp;
}
void lifetime_t::delete_clone()
{
	if (_lifetime_calendar) delete _lifetime_calendar;
	if (_lifetime_cycle) delete _lifetime_cycle;
}
void lifetime_t::copy(lifetime_t * lifetime)
{
	_lifetime_cycle->copy(lifetime->_lifetime_cycle);
	_lifetime_calendar->copy(lifetime->_lifetime_calendar);

	_replacement_option = lifetime->_replacement_option;
	_replacement_capacity = lifetime->_replacement_capacity;
	_replacements = lifetime->_replacements;
	_replacement_scheduled = lifetime->_replacement_scheduled;
	_q = lifetime->_q;
}
double lifetime_t::capacity_percent(){ return _q; }
double lifetime_t::capacity_percent_cycle() { return _lifetime_cycle->capacity_percent(); }
double lifetime_t::capacity_percent_calendar() { return _lifetime_calendar->capacity_percent(); }

void lifetime_t::runLifetimeModels(size_t idx, capacity_t * capacity, double T_battery)
{
	double q_last = _q;
	double q_cycle = _q;
	double q_calendar = _q;

	if (_q > 0)
	{
		if (capacity->chargeChanged())
			q_cycle = _lifetime_cycle->runCycleLifetime((capacity->prev_DOD()));
		else if (idx==0)
			q_cycle = _lifetime_cycle->runCycleLifetime((capacity->DOD()));

		q_calendar = _lifetime_calendar->runLifetimeCalendarModel(idx, T_battery, capacity->SOC()*0.01);

		// total capacity is min of cycle (Q_neg) and calendar (Q_li) capacity
		_q = fmin(q_cycle, q_calendar);
	}
	if (_q < 0)
		_q = 0;

	// capacity cannot increase
	if (_q > q_last)
		_q = q_last;
}

bool lifetime_t::check_replaced()
{
	bool replaced = false;
	if ((_replacement_option == 1 && (_q - tolerance) <= _replacement_capacity) || _replacement_scheduled)
	{
		_replacements++;

		_q += _replacement_percent;

		// for now, only allow augmenting up to original installed capacity
		_q = fmin(100., _q);

		replaced = true;
		_replacement_scheduled = false;

		_lifetime_cycle->replaceBattery(_replacement_percent);
		_lifetime_calendar->replaceBattery(_replacement_percent);
	}
	return replaced;
}
void lifetime_t::reset_replacements(){ _replacements = 0; }
int lifetime_t::get_replacements(){ return _replacements; }
double lifetime_t::get_replacement_percent() {
	return _replacement_percent;
};

void lifetime_t::set_replacement_option(int option) { _replacement_option = option; }
void lifetime_t::force_replacement(double replacement_percent){
	_replacement_scheduled = true;
	_replacement_percent = replacement_percent;
}

lifetime_cycle_t::lifetime_cycle_t(const util::matrix_t<double> &batt_lifetime_matrix)
{

	_batt_lifetime_matrix = batt_lifetime_matrix;
	for (int i = 0; i <(int)_batt_lifetime_matrix.nrows(); i++)
	{
		_DOD_vect.push_back(batt_lifetime_matrix.at(i,0));
		_cycles_vect.push_back(batt_lifetime_matrix.at(i,1));
		_capacities_vect.push_back(batt_lifetime_matrix.at(i, 2));
	}
	// initialize other member variables
	_nCycles = 0;
	_Dlt = 0;
	_q = bilinear(0.,0);
	_jlt = 0;
	_Xlt = 0;
	_Ylt = 0;
	_Range = 0;
	_average_range = 0;
}

lifetime_cycle_t::~lifetime_cycle_t(){}
lifetime_cycle_t * lifetime_cycle_t::clone(){ return new lifetime_cycle_t(*this); }
void lifetime_cycle_t::copy(lifetime_cycle_t * lifetime_cycle)
{
	// doesn't change (and potentially slow)
	/*
	_cycles_vs_DOD = lifetime_cycle->_cycles_vs_DOD;
	_batt_lifetime_matrix = lifetime_cycle->_batt_lifetime_matrix;
	_DOD_vect = lifetime_cycle->_DOD_vect;
	_cycles_vect = lifetime_cycle->_cycles_vect;
	_capacities_vect = lifetime_cycle->_capacities_vect;
	*/

	_nCycles = lifetime_cycle->_nCycles;
	_q = lifetime_cycle->_q;
	_Dlt = lifetime_cycle->_Dlt;
	_jlt = lifetime_cycle->_jlt;
	_Xlt = lifetime_cycle->_Xlt;
	_Ylt = lifetime_cycle->_Ylt;
	_Peaks = lifetime_cycle->_Peaks;
	_Range = lifetime_cycle->_Range;
	_average_range = lifetime_cycle->_average_range;
}
double lifetime_cycle_t::estimateCycleDamage()
{
	// Initialize assuming 50% DOD
	double DOD = 50;
	if (_average_range > 0){
		DOD = _average_range;
	}
	return(bilinear(DOD, _nCycles+1) - bilinear(DOD, _nCycles + 2));
}
double lifetime_cycle_t::runCycleLifetime(double DOD)
{
	rainflow(DOD);

	// return the effective capacity (Q_neg)
	return _q;
}

void lifetime_cycle_t::rainflow(double DOD)
{
	// initialize return code
	int retCode = LT_GET_DATA;

	// Begin algorithm
	_Peaks.push_back(DOD);
	bool atStepTwo = true;

	// Loop until break
	while (atStepTwo)
	{
		// Rainflow: Step 2: Form ranges X,Y
		if (_jlt >= 2)
			rainflow_ranges();
		else
		{
			// Get more data (Step 1)
			retCode = LT_GET_DATA;
			break;
		}

		// Rainflow: Step 3: Compare ranges
		retCode = rainflow_compareRanges();

		// We break to get more data, or if we are done with step 5
		if (retCode == LT_GET_DATA)
			break;
	}

	if (retCode == LT_GET_DATA)
		_jlt++;
}

void lifetime_cycle_t::rainflow_ranges()
{
	_Ylt = fabs(_Peaks[_jlt - 1] - _Peaks[_jlt - 2]);
	_Xlt = fabs(_Peaks[_jlt] - _Peaks[_jlt - 1]);
}
void lifetime_cycle_t::rainflow_ranges_circular(int index)
{
	size_t end = _Peaks.size() - 1;
	if (index == 0)
	{
		_Xlt = fabs(_Peaks[0] - _Peaks[end]);
		_Ylt = fabs(_Peaks[end] - _Peaks[end - 1]);
	}
	else if (index == 1)
	{
		_Xlt = fabs(_Peaks[1] - _Peaks[0]);
		_Ylt = fabs(_Peaks[0] - _Peaks[end]);
	}
	else
		rainflow_ranges();
}

int lifetime_cycle_t::rainflow_compareRanges()
{
	int retCode = LT_SUCCESS;
	bool contained = true;

	// modified to disregard some of algorithm which doesn't work well
	if (_Xlt < _Ylt)
		retCode = LT_GET_DATA;
	else if (_Xlt >= _Ylt)
		contained = false;

	// Step 5: Count range Y, discard peak & valley of Y, go to Step 2
	if (!contained)
	{
		_Range = _Ylt;
		_average_range = (_average_range*_nCycles + _Range) / (_nCycles + 1);
		_nCycles++;

		// the capacity percent cannot increase
		double dq = bilinear(_average_range, _nCycles) - bilinear(_average_range, _nCycles + 1);
		if (dq > 0)
			_q -= dq;

		if (_q < 0)
			_q = 0.;

		// discard peak & valley of Y
		double save = _Peaks[_jlt];
		_Peaks.pop_back();
		_Peaks.pop_back();
		_Peaks.pop_back();
		_Peaks.push_back(save);
		_jlt -= 2;
		// stay in while loop
		retCode = LT_RERANGE;
	}

	return retCode;
}
void lifetime_cycle_t::replaceBattery(double replacement_percent)
{
	_q += replacement_percent;
	_q = fmin(bilinear(0., 0), _q);
	_Dlt = 0.; // seems unused

	// More work to figure out degradation of multiple-aged battery units
	if (replacement_percent == 100) {
		_nCycles = 0;
	}

	_jlt = 0;
	_Xlt = 0;
	_Ylt = 0;
	_Range = 0;
	_Peaks.clear();
}

int lifetime_cycle_t::cycles_elapsed(){ return _nCycles; }
double lifetime_cycle_t::cycle_range(){ return _Range; }
double lifetime_cycle_t::average_range() { return _average_range; }
double lifetime_cycle_t::capacity_percent() { return _q; }

double lifetime_cycle_t::bilinear(double DOD, int cycle_number)
{
	/*
	Work could be done to make this simpler
	Current idea is to interpolate first along the C = f(n) curves for each DOD to get C_DOD_, C_DOD_+
	Then interpolate C_, C+ to get C at the DOD of interest
	*/

	std::vector<double> D_unique_vect;
	std::vector<double> C_n_low_vect;
	std::vector<double> D_high_vect;
	std::vector<double> C_n_high_vect;
	std::vector<int> low_indices;
	std::vector<int> high_indices;
	double D = 0.;
	size_t n = 0;
	double C = 100;

	// get unique values of D
	D_unique_vect.push_back(_DOD_vect[0]);
	for (int i = 0; i < (int)_DOD_vect.size(); i++){
		bool contained = false;
		for (int j = 0; j < (int)D_unique_vect.size(); j++){
			if (_DOD_vect[i] == D_unique_vect[j]){
				contained = true;
				break;
			}
		}
		if (!contained){
			D_unique_vect.push_back(_DOD_vect[i]);
		}
	}
	n = D_unique_vect.size();

	if (n > 1)
	{
		// get where DOD is bracketed [D_lo, DOD, D_hi]
		double D_lo = 0;
		double D_hi = 100;

		for (int i = 0; i < (int)_DOD_vect.size(); i++)
		{
			D = _DOD_vect[i];
			if (D < DOD && D > D_lo)
				D_lo = D;
			else if (D >= DOD && D < D_hi)
				D_hi = D;
		}

		// Seperate table into bins
		double D_min = 100.;
		double D_max = 0.;

		for (int i = 0; i < (int)_DOD_vect.size(); i++)
		{
			D = _DOD_vect[i];
			if (D == D_lo)
				low_indices.push_back(i);
			else if (D == D_hi)
				high_indices.push_back(i);

			if (D < D_min){ D_min = D; }
			else if (D > D_max){ D_max = D; }
		}

		// if we're out of the bounds, just make the upper bound equal to the highest input
		if (high_indices.size() == 0)
		{
			for (int i = 0; i != (int)_DOD_vect.size(); i++)
			{
				if (_DOD_vect[i] == D_max)
					high_indices.push_back(i);
			}
		}

		size_t n_rows_lo = low_indices.size();
		size_t n_rows_hi = high_indices.size();
		size_t n_cols = 2;

		// If we aren't bounded, fill in values
		if (n_rows_lo == 0)
		{
			// Assumes 0% DOD
			for (int i = 0; i < (int)n_rows_hi; i++)
			{
				C_n_low_vect.push_back(0. + i * 500); // cycles
				C_n_low_vect.push_back(100.); // 100 % capacity
			}
		}

		if (n_rows_lo != 0)
		{
			for (int i = 0; i < (int)n_rows_lo; i++)
			{
				C_n_low_vect.push_back(_cycles_vect[low_indices[i]]);
				C_n_low_vect.push_back(_capacities_vect[low_indices[i]]);
			}
		}
		if (n_rows_hi != 0)
		{
			for (int i = 0; i < (int)n_rows_hi; i++)
			{
				C_n_high_vect.push_back(_cycles_vect[high_indices[i]]);
				C_n_high_vect.push_back(_capacities_vect[high_indices[i]]);
			}
		}
		n_rows_lo = C_n_low_vect.size() / n_cols;
		n_rows_hi = C_n_high_vect.size() / n_cols;

		if (n_rows_lo == 0 || n_rows_hi == 0)
		{
			// need a safeguard here
		}

		util::matrix_t<double> C_n_low(n_rows_lo, n_cols, &C_n_low_vect);
		util::matrix_t<double> C_n_high(n_rows_lo, n_cols, &C_n_high_vect);

		// Compute C(D_lo, n), C(D_hi, n)
		double C_Dlo = util::linterp_col(C_n_low, 0, cycle_number, 1);
		double C_Dhi = util::linterp_col(C_n_high, 0, cycle_number, 1);

		if (C_Dlo < 0.)
			C_Dlo = 0.;
		if (C_Dhi > 100.)
			C_Dhi = 100.;

		// Interpolate to get C(D, n)
		C = util::interpolate(D_lo, C_Dlo, D_hi, C_Dhi, DOD);
	}
	// just have one row, single level interpolation
	else
	{
		C = util::linterp_col(_batt_lifetime_matrix, 1, cycle_number, 2);
	}

	return C;
}

/*
Lifetime Calendar Model
*/
lifetime_calendar_t::lifetime_calendar_t(int calendar_choice, util::matrix_t<double> calendar_matrix, double dt_hour,
	float q0, float a, float b, float c)
{
	_calendar_choice = calendar_choice;

	_day_age_of_battery = 0;
	_last_idx = 0;

	// coefficients based on fractional capacity (0 - 1)
	_dq_old = 0;
	_dq_new = 0;

	_q0 = q0;
	_a = a;
	_b = b;
	_c = c;

	// output based on percentage capacity (0 - 100%)
	_q = _q0 * 100;

	// timestep
	_dt_hour = dt_hour;
	_dt_day = dt_hour / util::hours_per_day;

	// extract and sort calendar life info from table
	if (_calendar_choice == CALENDAR_LOSS_TABLE)
	{
		for (size_t i = 0; i != calendar_matrix.nrows(); i++)
		{
			_calendar_days.push_back((int)calendar_matrix.at(i, 0));
			_calendar_capacity.push_back(calendar_matrix.at(i, 1));
		}
	}
	// Ensure don't accidently initialize to 0 if not using model
	else if (_calendar_choice == NONE) {
		_q0 = 1.0;
	}
}
lifetime_calendar_t * lifetime_calendar_t::clone(){ return new lifetime_calendar_t(*this); }
void lifetime_calendar_t::copy(lifetime_calendar_t * lifetime_calendar)
{
	_calendar_choice = lifetime_calendar->_calendar_choice;
	_calendar_days = lifetime_calendar->_calendar_days;
	_calendar_capacity = lifetime_calendar->_calendar_capacity;
	_day_age_of_battery = lifetime_calendar->_day_age_of_battery;
	_dt_hour = lifetime_calendar->_dt_hour;
	_dt_day = lifetime_calendar->_dt_day;
	_last_idx = lifetime_calendar->_last_idx;
	_q = lifetime_calendar->_q;
	_dq_old = lifetime_calendar->_dq_old;
	_dq_new = lifetime_calendar->_dq_new;
	_q0 = lifetime_calendar->_q0;
	_a = lifetime_calendar->_a;
	_b = lifetime_calendar->_b;
	_c = lifetime_calendar->_c;
}
double lifetime_calendar_t::capacity_percent() { return _q; }
double lifetime_calendar_t::runLifetimeCalendarModel(size_t idx, double T, double SOC)
{
	if (_calendar_choice != lifetime_calendar_t::NONE)
	{
		// only run once per iteration (need to make the last iteration)
		if (idx > _last_idx)
		{
			int steps_per_day = (int) std::round(util::hours_per_day / _dt_hour);
			if (idx % steps_per_day == 0)
				_day_age_of_battery++;

			if (_calendar_choice == lifetime_calendar_t::LITHIUM_ION_CALENDAR_MODEL)
				runLithiumIonModel(T, SOC);
			else if (_calendar_choice == lifetime_calendar_t::CALENDAR_LOSS_TABLE)
				runTableModel();

			_last_idx = idx;
		}
	}
	return _q;
}
void lifetime_calendar_t::runLithiumIonModel(double T, double SOC)
{
	double k_cal = _a * exp(_b * (1. / T - 1. / 296))*exp(_c*(SOC / T - 1. / 296));
	if (_dq_old == 0)
		_dq_new = k_cal * sqrt(_dt_day);
	else
		_dq_new = (0.5 * pow(k_cal, 2) / _dq_old) * _dt_day + _dq_old;
	_dq_old = _dq_new;
	_q = (_q0 - (_dq_new)) * 100;

}
void lifetime_calendar_t::runTableModel()
{
	size_t n = _calendar_days.size() - 1;
	int day_lo = 0;
	int day_hi = _calendar_days[n];
	double capacity_lo = 100;
	double capacity_hi = 0;

	// interpolation mode
	for (int i = 0; i != (int)_calendar_days.size(); i++)
	{
		int day = _calendar_days[i];
		double capacity = _calendar_capacity[i];
		if (day <= _day_age_of_battery)
		{
			day_lo = day;
			capacity_lo = capacity;
		}
		if (day > _day_age_of_battery)
		{
			day_hi = day;
			capacity_hi = capacity;
			break;
		}
	}
	if (day_lo == day_hi)
	{
		day_lo = _calendar_days[n - 1];
		day_hi = _calendar_days[n];
		capacity_lo = _calendar_capacity[n - 1];
		capacity_hi = _calendar_capacity[n];
	}

	_q = util::interpolate(day_lo, capacity_lo, day_hi, capacity_hi, _day_age_of_battery);
}

void lifetime_calendar_t::replaceBattery(double replacement_percent)
{
	_day_age_of_battery = 0;
	_q += replacement_percent;
	_q = fmin(_q0 * 100, _q);
	_dq_new = 0;
	_dq_old = 0;
}

/*
Define Thermal Model
*/
thermal_t::thermal_t() { /* nothing to do */ }
thermal_t::thermal_t(double dt_hour, double mass, double length, double width, double height,
	double Cp,  double h, std::vector<double> T_room,
	const util::matrix_t<double> &c_vs_t ) : _dt_hour(dt_hour), _mass(mass), _length(length), _width(width), _height(height),
	_Cp(Cp), _h(h), _T_room(T_room), _cap_vs_temp(c_vs_t)
{
	_R = 0.004;
	_capacity_percent = 100;

	// assume all surfaces are exposed
	_A = 2 * (length*width + length*height + width*height);

	// initialize to room temperature
	_T_battery = T_room[0];

	//initialize maximum temperature
	_T_max = 400.;

	// curve fit
	size_t n = _cap_vs_temp.nrows();
	for (int i = 0; i < (int)n; i++)
	{
		_cap_vs_temp(i,0) += 273.15; // convert C to K
	}
}
thermal_t * thermal_t::clone(){ return new thermal_t(*this); }
void thermal_t::copy(thermal_t * thermal)
{
	_mass = thermal->_mass;
	_length = thermal->_length;
	_width = thermal->_width;
	_height = thermal->_height;
	_Cp = thermal->_Cp;
	_h = thermal->_h;
	// _T_room = thermal->_T_room;  // don't copy, super slow in subhourly simulations
	_R = thermal->_R;
	_A = thermal->_A;
	_T_battery = thermal->_T_battery;
	_capacity_percent = thermal->_capacity_percent;
	_T_max = thermal->_T_max;
}
void thermal_t::replace_battery(size_t lifetimeIndex)
{
	_T_battery = _T_room[util::yearOneIndex(_dt_hour, lifetimeIndex)];
	_capacity_percent = 100.;
}

#define HR2SEC 3600.0
void thermal_t::updateTemperature(double I, double R, double dt, size_t lifetimeIndex)
{
	_R = R;
	if (trapezoidal(I, dt*HR2SEC, lifetimeIndex) < _T_max && trapezoidal(I, dt*HR2SEC, lifetimeIndex) > 0)
		_T_battery = trapezoidal(I, dt*HR2SEC, lifetimeIndex);
	else if (rk4(I, dt*HR2SEC, lifetimeIndex) < _T_max && rk4(I, dt*HR2SEC, lifetimeIndex) > 0)
		_T_battery = rk4(I, dt*HR2SEC, lifetimeIndex);
	else if (implicit_euler(I, dt*HR2SEC, lifetimeIndex) < _T_max && implicit_euler(I, dt*HR2SEC, lifetimeIndex) > 0)
		_T_battery = implicit_euler(I, dt*HR2SEC, lifetimeIndex);
	else
		_message.add("Computed battery temperature below zero or greater than max allowed, consider reducing C-rate");
	_T_battery = fmax(_T_battery, _T_room[util::yearOneIndex(_dt_hour, lifetimeIndex)]);
}

double thermal_t::f(double T_battery, double I, size_t lifetimeindex)
{
	return (1 / (_mass*_Cp)) * ((_h*(_T_room[util::yearOneIndex(_dt_hour, lifetimeindex)]  - T_battery)*_A) + pow(I, 2)*_R);
}
double thermal_t::rk4( double I, double dt, size_t lifetimeindex)
{
	double k1 = dt*f(_T_battery, I, lifetimeindex);
	double k2 = dt*f(_T_battery + k1 / 2, I, lifetimeindex);
	double k3 = dt*f(_T_battery + k2 / 2, I, lifetimeindex);
	double k4 = dt*f(_T_battery + k3, I, lifetimeindex);
	return (_T_battery + (1. / 6)*(k1 + k4) + (1. / 3.)*(k2 + k3));
}
double thermal_t::trapezoidal(double I, double dt, size_t lifetimeindex)
{
	double B = 1 / (_mass*_Cp); // [K/J]
	double C = _h*_A;			// [W/K]
	double D = pow(I, 2)*_R;	// [Ohm A*A]
	double T_prime = f(_T_battery, I, lifetimeindex);	// [K]

	return (_T_battery + 0.5*dt*(T_prime + B*(C*_T_room[util::yearOneIndex(_dt_hour, lifetimeindex)] + D))) / (1 + 0.5*dt*B*C);
}
double thermal_t::implicit_euler(double I, double dt, size_t lifetimeIndex)
{
	double B = 1 / (_mass*_Cp); // [K/J]
	double C = _h*_A;			// [W/K]
	double D = pow(I, 2)*_R;	// [Ohm A*A]
//	double T_prime = f(_T_battery, I);	// [K]

	return (_T_battery + dt*(B*C*_T_room[util::yearOneIndex(_dt_hour, lifetimeIndex)] + D)) / (1 + dt*B*C);
}
double thermal_t::T_battery(){ return _T_battery; }
double thermal_t::capacity_percent()
{
	double percent = util::linterp_col(_cap_vs_temp, 0, _T_battery, 1);

	if (percent < 0 || percent > 100)
	{
		percent = 100;
		_message.add("Unable to determine capacity adjustment for temperature, ignoring");
	}
	_capacity_percent = percent;
	return _capacity_percent;
}
/*
Define Losses
*/
losses_t::losses_t(double dtHour, lifetime_t * lifetime, thermal_t * thermal, capacity_t* capacity, int loss_choice, double_vec charge_loss, double_vec discharge_loss, double_vec idle_loss, double_vec losses):
_loss_mode(loss_choice)
{
	_dtHour = dtHour;
	_lifetime = lifetime;
	_thermal = thermal;
	_capacity = capacity;

	// User can input vectors of size 1 or size 12
	if (loss_choice == losses_t::MONTHLY)
	{
		if (charge_loss.size() == 1) {
			for (size_t m = 0; m < 12; m++) {
				_charge_loss.push_back(charge_loss[0]);
			}
		}
		else if (charge_loss.size() == 0) {
			for (size_t m = 0; m < 12; m++) {
				_charge_loss.push_back(0);
			}
		}
		else {
			_charge_loss = charge_loss;
		}
		if (discharge_loss.size() == 1) {

			for (size_t m = 0; m < 12; m++) {
				_discharge_loss.push_back(discharge_loss[0]);
			}
		}
		else if (discharge_loss.size() == 0) {

			for (size_t m = 0; m < 12; m++) {
				_discharge_loss.push_back(0);
			}
		}
		else {
			_discharge_loss = discharge_loss;
		}
		if (idle_loss.size() == 1) {
			for (size_t m = 0; m < 12; m++) {
				_idle_loss.push_back(idle_loss[0]);
			}
		}
		else if (idle_loss.size() == 0) {
			for (size_t m = 0; m < 12; m++) {
				_idle_loss.push_back(0);
			}
		}
		else {
			_idle_loss = idle_loss;
		}
		for (size_t i = 0; i < (size_t)(8760 / dtHour); i++) {
			_full_loss.push_back(0);
		}
	}
	// User can input vectors of size 1 or size nrec (first year)
	else {
		if (losses.size() == 1) {
			for (size_t i = 0; i < (size_t)(8760 / dtHour); i++) {
				_full_loss.push_back(losses[0]);
			}
		}
		else {
			_full_loss = losses;
		}

	}
}
void losses_t::set_models(lifetime_t * l, thermal_t * t, capacity_t* c){
    _lifetime = l;
    _thermal = t;
    _capacity = c;
}
void losses_t::copy(losses_t * losses)
{
	_loss_mode = losses->_loss_mode;
	_charge_loss = losses->_charge_loss;
	_discharge_loss = losses->_discharge_loss;
	_idle_loss = losses->_idle_loss;
	_full_loss = losses->_full_loss;
}

double losses_t::getLoss(size_t indexFirstYear) { return _full_loss[indexFirstYear]; }
void losses_t::run_losses(size_t lifetimeIndex)
{
	_capacity->updateCapacityForLifetime(_lifetime->capacity_percent());

	size_t indexYearOne = util::yearOneIndex(_dtHour, lifetimeIndex);
	size_t hourOfYear = (size_t)std::floor(indexYearOne * _dtHour);
	size_t monthIndex = util::month_of((double)(hourOfYear)) - 1;

	// update system losses depending on user input
	if (_loss_mode == losses_t::MONTHLY) {
		if (_capacity->charge_operation() == capacity_t::CHARGE)
			_full_loss[indexYearOne] = _charge_loss[monthIndex];
		if (_capacity->charge_operation() == capacity_t::DISCHARGE)
			_full_loss[indexYearOne] = _discharge_loss[monthIndex];
		if (_capacity->charge_operation() == capacity_t::NO_CHARGE)
			_full_loss[indexYearOne] = _idle_loss[monthIndex];
	}

}
/*
Define Battery
*/
battery_t::battery_t(){};
battery_t::battery_t(double dt_hour, int battery_chemistry)
{
	_dt_hour = dt_hour;
	_dt_min = dt_hour * 60;
	_battery_chemistry = battery_chemistry;
	_last_idx = 0;

	if (battery_chemistry != battery_t::LEAD_ACID) {
		_capacity_initial = new capacity_lithium_ion_t();
	}
	else {
		_capacity_initial = new capacity_kibam_t();
	}
	_thermal_initial = new thermal_t();
}

battery_t::battery_t(const battery_t& battery)
{
	_battery_chemistry = battery._battery_chemistry;
	_dt_hour = battery._dt_hour;
	_dt_min = battery._dt_min;
	_last_idx = battery._last_idx;
	_capacity = battery.capacity_model()->clone();
	_capacity_initial = battery.capacity_initial_model()->clone();
	_voltage = battery.voltage_model()->clone();
	_thermal = battery.thermal_model()->clone();
	_thermal_initial = battery.thermal_initial_model()->clone();
	_lifetime = battery.lifetime_model()->clone();
	_losses = new losses_t(_dt_hour, _lifetime, _thermal, _capacity, 0);
	_losses->copy(battery.losses_model());
}

battery_t::~battery_t()
{
	if (_capacity_initial)
		delete _capacity_initial;
	if (_thermal_initial)
		delete _thermal_initial;
}

// copy from battery to this
void battery_t::copy(const battery_t * battery)
{
	_capacity->copy(battery->capacity_model());
	_capacity_initial->copy(battery->capacity_initial_model());
	_thermal->copy(battery->thermal_model());
	_thermal_initial->copy(battery->thermal_initial_model());
	_lifetime->copy(battery->lifetime_model());
	_voltage->copy(battery->voltage_model());
	_losses->set_models(_lifetime, _thermal, _capacity);

	_battery_chemistry = battery->_battery_chemistry;
	_dt_hour = battery->_dt_hour;
	_dt_min = battery->_dt_min;
	_last_idx = battery->_last_idx;
}

void battery_t::delete_clone()
{
	if (_capacity) delete _capacity;
	if (_voltage) delete _voltage;
	if (_thermal) delete _thermal;
	if (_lifetime)
	{
		_lifetime->delete_clone();
		delete _lifetime;
	}
	if (_losses) delete _losses;
}
void battery_t::initialize(capacity_t *capacity, voltage_t * voltage, lifetime_t * lifetime, thermal_t * thermal, losses_t * losses)
{
	_capacity = capacity;
	_lifetime = lifetime;
	_voltage = voltage;
	_thermal = thermal;
	_losses = losses;

	_capacity_initial->copy(_capacity);
	_thermal_initial->copy(_thermal);
}

double battery_t::calculate_current_for_power_kw(double &P_kw){
    if (P_kw == 0.)
        return 0.;
    double current;
    if (P_kw < 0){
        double max_P = calculate_max_charge_kw(&current);
        if (max_P > P_kw){
            P_kw = max_P;
            return current;
        }
    }
    else{
        double max_P = calculate_max_discharge_kw(&current);
        if (max_P < P_kw){
            P_kw = max_P;
            return current;
        }
    }
    return _voltage->calculate_current_for_target_w(P_kw * 1000., _capacity->q0(), fmin(_capacity->qmax(), _capacity->qmax_thermal()), _thermal->T_battery());
}

double battery_t::calculate_voltage_for_current(double I) {
    // TODO: add looping when this function will actually be used... doesn't work that well atm
    double qmax = fmin(_capacity->qmax(), _capacity->qmax_thermal());
    return voltage_model()->calculate_voltage_for_current(I, battery_charge_total(), qmax, _thermal->T_battery());
}

double battery_t::calculate_max_charge_kw(double *max_current_A) {
    double q = _capacity->q0();
    double qmax = _capacity->qmax_thermal();
    double power_W = 0;
    double current = 0;
    size_t its = 0;
    while (fabs(power_W - _voltage->calculate_max_charge_w(q, qmax, _thermal->T_battery(), &current)) > tolerance
           && its++ < 10){
        power_W = _voltage->calculate_max_charge_w(q, qmax, _thermal->T_battery(), &current);
        _thermal->updateTemperature(current, _voltage->R_battery(), _dt_hour, _last_idx + 1);
        qmax = _capacity->qmax() * _thermal->capacity_percent() / 100.;
    }
    return _voltage->calculate_max_charge_w(q, qmax, _thermal->T_battery(), max_current_A) / 1000.;
}

double battery_t::calculate_max_discharge_kw(double *max_current_A) {
    double q = _capacity->q0();
    double qmax = _capacity->qmax_thermal();
    double power_W = 0;
    double current = 0;
    size_t its = 0;
    while (fabs(power_W - _voltage->calculate_max_discharge_w(q, qmax, _thermal->T_battery(), &current)) > tolerance
        && its++ < 10){
        power_W = _voltage->calculate_max_discharge_w(q, qmax, _thermal->T_battery(), &current);
        _thermal->updateTemperature(current, _voltage->R_battery(), _dt_hour, _last_idx + 1);
        qmax = _capacity->qmax() * _thermal->capacity_percent() / 100.;
    }
    return _voltage->calculate_max_discharge_w(q, qmax, _thermal->T_battery(), max_current_A) / 1000.;
}

double battery_t::run(size_t lifetimeIndex, double &I)
{
	// Temperature affects capacity, but capacity model can reduce current, which reduces temperature, need to iterate
	double I_initial = I;
	size_t iterate_count = 0;
	_capacity_initial->copy(_capacity);
	_thermal_initial->copy(_thermal);

	while (iterate_count < 5)
	{
		runThermalModel(I, lifetimeIndex);
		runCapacityModel(I);

		if (fabs(I - I_initial)/fabs(I_initial) > tolerance)
		{
			_thermal->copy(_thermal_initial);
			_capacity->copy(_capacity_initial);
			I_initial = I;
			iterate_count++;
		}
		else {
			break;
		}

	}
	runVoltageModel();
	runLifetimeModel(lifetimeIndex);
	runLossesModel(lifetimeIndex);

	return I * voltage_model()->battery_voltage() * util::watt_to_kilowatt;
}
void battery_t::runThermalModel(double I, size_t lifetimeIndex)
{
	_thermal->updateTemperature(I, _voltage->R_battery(), _dt_hour, lifetimeIndex);
}

void battery_t::runCapacityModel(double &I)
{
	// Don't update max capacity if the battery is idle
	if (fabs(I) > tolerance) {
		// Need to first update capacity model to ensure temperature accounted for
		_capacity->updateCapacityForThermal(_thermal->capacity_percent());
	}
	_capacity->updateCapacity(I, _dt_hour );
}

void battery_t::runVoltageModel()
{
	_voltage->updateVoltage(_capacity, _thermal, _dt_hour);
}

void battery_t::runLifetimeModel(size_t lifetimeIndex)
{
	_lifetime->runLifetimeModels(lifetimeIndex, capacity_model(), thermal_model()->T_battery());
	if (_lifetime->check_replaced())
	{
		_capacity->replace_battery(_lifetime->get_replacement_percent());
		_thermal->replace_battery(lifetimeIndex);
	}
}
void battery_t::runLossesModel(size_t idx)
{
	if (idx > _last_idx || idx == 0)
	{
		_losses->run_losses(idx);
		_last_idx = idx;
	}
}
capacity_t * battery_t::capacity_model() const { return _capacity; }
capacity_t * battery_t::capacity_initial_model() const { return _capacity_initial; }
voltage_t * battery_t::voltage_model() const { return _voltage; }
lifetime_t * battery_t::lifetime_model() const { return _lifetime; }
thermal_t * battery_t::thermal_model() const { return _thermal; }
thermal_t * battery_t::thermal_initial_model() const { return _thermal_initial; }
losses_t * battery_t::losses_model() const { return _losses; }

double battery_t::battery_charge_needed(double SOC_max)
{
	double charge_needed = _capacity->qmax_thermal() * SOC_max * 0.01 - _capacity->q0();
	if (charge_needed > 0)
		return charge_needed;
	else
		return 0.;
}
double battery_t::battery_energy_to_fill(double SOC_max)
{
	double battery_voltage = this->battery_voltage_nominal(); // [V]
	double charge_needed_to_fill = this->battery_charge_needed(SOC_max); // [Ah] - qmax - q0
	return (charge_needed_to_fill * battery_voltage)*util::watt_to_kilowatt;  // [kWh]
}
double battery_t::battery_energy_nominal()
{
	return battery_voltage_nominal() * _capacity->qmax() * util::watt_to_kilowatt;
}
double battery_t::battery_power_to_fill(double SOC_max)
{
	// in one time step
	return (this->battery_energy_to_fill(SOC_max) / _dt_hour);
}

double battery_t::battery_charge_total(){return _capacity->q0();}
double battery_t::battery_charge_maximum(){ return _capacity->qmax(); }
double battery_t::battery_charge_maximum_thermal() { return _capacity->qmax_thermal(); }
double battery_t::cell_voltage(){ return _voltage->cell_voltage();}
double battery_t::battery_voltage(){ return _voltage->battery_voltage();}
double battery_t::battery_voltage_nominal(){ return _voltage->battery_voltage_nominal(); }
double battery_t::battery_soc(){ return _capacity->SOC(); }
