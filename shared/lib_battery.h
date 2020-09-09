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

#ifndef battery_h
#define battery_h

#include "lib_util.h"

#include <vector>
#include <map>
#include <string>
#include <stdio.h>
#include <algorithm>

// Forward declarations to reduce imports

const double low_tolerance = 0.01;
const double tolerance = 0.001;

// Messages
class message
{
public:
	message(){};
	virtual ~message(){};


	void add(std::string message);
	size_t total_message_count();
	size_t message_count(int index);
	std::string get_message(int index);
	std::string construct_log_count_string(int index);

protected:
	std::vector<std::string> messages;
	std::vector<int> count;
};

/*
Base class from which capacity models derive
Note, all capacity models are based on the capacity of one battery
*/
class voltage_t;
class capacity_t
{
public:

	capacity_t();
	capacity_t(double q, double SOC_init, double SOC_max, double SOC_min);

	// deep copy
	virtual capacity_t * clone() = 0;

	// shallow copy from capacity to this
	virtual void copy(capacity_t *);

	// virtual destructor
	virtual ~capacity_t(){};

	// pure virtual functions (abstract) which need to be defined in derived classes
	virtual void updateCapacity(double &I, double dt) = 0;
	virtual void updateCapacityForThermal(double capacity_percent)=0;
	virtual void updateCapacityForLifetime(double capacity_percent)=0;
	virtual void replace_battery(double replacement_percent)=0;

	void change_SOC_limits(double min, double max){
	    _SOC_min = min;
	    _SOC_max = max;
	}

	virtual double q1() = 0; // available charge
	virtual double q10() = 0; // capacity at 10 hour discharge rate

	void check_charge_change();
	void check_SOC();
	void update_SOC();

	// common outputs
	double SOC();
	double DOD_max();
	double DOD();
	double prev_DOD();
	double q0();
	double qmax();
	double qmax_thermal();
	double I();
	bool chargeChanged();
	double I_loss();
	int charge_operation();

	enum { CHARGE, NO_CHARGE, DISCHARGE };

protected:
	double _q0;  // [Ah] - Total capacity at timestep
	double _qmax; // [Ah] - maximum possible capacity
	double _qmax_thermal; // [Ah] - maximum capacity adjusted for temperature affects
	double _qmax0; // [Ah] - original maximum capacity
	double _I;   // [A]  - Current draw during last step
	double _I_loss; // [A] - Lifetime and thermal losses
	double _SOC; // [%] - State of Charge
	double _SOC_init; // [%] - Initial SOC
	double _SOC_max; // [%] - Maximum SOC
	double _SOC_min; // [%] - Minimum SOC
	double _DOD; // [%] - Depth of Discharge
	double _DOD_prev; // [%] - Depth of Discharge of previous step
	double _dt_hour; // [hr] - Timestep in hours
	bool _chargeChange; // [true/false] - indicates if charging state has changed since last step
	int _prev_charge; // {CHARGE, NO_CHARGE, DISCHARGE}
	int _charge; // {CHARGE, NO_CHARGE, DISCHARGE}
};

/*
KiBaM specific capacity model
*/
class capacity_kibam_t : public capacity_t
{
public:

	// Public APIs
	capacity_kibam_t();
	capacity_kibam_t(double q20, double t1, double q1, double q10, double SOC_init, double SOC_max, double SOC_min);
	~capacity_kibam_t(){}

	// deep copy
	capacity_kibam_t * clone();

	// copy from capacity to this
	void copy(capacity_t *);

	void updateCapacity(double &I, double dt);
	void updateCapacityForThermal(double capacity_percent);
	void updateCapacityForLifetime(double capacity_percent);
	void replace_battery(double replacement_percent);
	double q1(); // Available charge
	double q2(); // Bound charge
	double q10(); // Capacity at 10 hour discharge rate
	double q20(); // Capacity at 20 hour discharge rate

protected:
	// unique to kibam
	double c_compute(double F, double t1, double t2, double k_guess);
	double q1_compute(double q10, double q0, double dt, double I); // may remove some inputs, use class variables
	double q2_compute(double q20, double q0, double dt, double I); // may remove some inputs, use class variables
	double Icmax_compute(double q10, double q0, double dt);
	double Idmax_compute(double q10, double q0, double dt);
	double qmax_compute();
	double qmax_of_i_compute(double T);
	void parameter_compute();

	// parameters for finding c, k, qmax
	double _t1;  // [h] - discharge rate for capacity at _q1
	double _t2;  // [h] - discharge rate for capacity at _q2
	double _q1;  // [Ah]- capacity at discharge rate t1
	double _q2;  // [Ah] - capacity at discharge rate t2
	double _F1;  // [unitless] - internal ratio computation
	double _F2;  // [unitless] - internal ratio computation

	// model parameters
	double _c;  // [0-1] - capacity fraction
	double _k;  // [1/hour] - rate constant

	// charge which changes with time
	double _q1_0; // [Ah] - charge available
	double _q2_0; // [Ah] - charge bound
	double _q10; //  [Ah] - Capacity at 10 hour discharge rate
	double _q20; // [Ah] - Capacity at 20 hour discharge rate
	double _I20; // [A]  - Current at 20 hour discharge rate
};

/*
Lithium Ion specific capacity model
*/
class capacity_lithium_ion_t : public capacity_t
{
public:
	capacity_lithium_ion_t();
	capacity_lithium_ion_t(double q, double SOC_init, double SOC_max, double SOC_min);
	~capacity_lithium_ion_t(){};

	// deep copy
	capacity_lithium_ion_t * clone();

	// copy from capacity to this
	void copy(capacity_t *);

	// override public api
	void updateCapacity(double &I, double dt);
	void updateCapacityForThermal(double capacity_percent);
	void updateCapacityForLifetime(double capacity_percent);
	void replace_battery(double replacement_percent);

	double q1(); // Available charge
	double q10(); // Capacity at 10 hour discharge rate

protected:
};

/*
Voltage Base class.
All voltage models are based on one-cell, but return the voltage for one battery
*/
class thermal_t;
class voltage_t
{
public:
	voltage_t(int mode, int num_cells_series, int num_strings, double voltage, double dt_hour);

	// deep copy
	virtual voltage_t * clone()=0;

	// copy from voltage to this
	virtual void copy(voltage_t *);


	virtual ~voltage_t(){};

    // Returns estimated max charge power over the next timestep (negative)
	virtual double calculate_max_charge_w(double q, double qmax, double kelvin, double *max_current) =0;

	// Returns estimated max discharge power over the next timestep
	virtual double calculate_max_discharge_w(double q, double qmax, double kelvin, double *max_current) =0;

    // Returns current [A] required to dispatch input power [W], only valid if less than max possible
    virtual double calculate_current_for_target_w(double P_watts, double q, double qmax, double kelvin) =0;

    virtual double calculate_voltage_for_current(double I, double q, double qmax, double T_k) =0;

    // runs calculate_voltage_for_current and changes the internal cell voltage
    virtual void updateVoltage(capacity_t * capacity, thermal_t * thermal, double dt)=0;

    virtual double battery_voltage(); // voltage of one battery

	double battery_voltage_nominal(); // nominal voltage of battery
	double cell_voltage(); // voltage of one cell
	double R_battery(); // computed battery resistance

	enum VOLTAGE_CHOICE{VOLTAGE_MODEL, VOLTAGE_TABLE};

protected:
	int _mode;					  // voltage model (0), voltage table (1)
	int _num_cells_series;        // number of cells in series
	int _num_strings;             // addition number in parallel
	double _cell_voltage;         // closed circuit voltage per cell [V]
	double _cell_voltage_nominal; // nominal cell voltage [V]
	double _R;                    // internal cell resistance (Ohm)
	double _R_battery;            // internal battery resistance (Ohm)
	double dt_hr;
};

class voltage_table_t : public voltage_t
{
public:
	voltage_table_t(int num_cells_series, int num_strings, double voltage, util::matrix_t<double> &voltage_table, double R, double dt_hour);

	// deep copy
	voltage_table_t * clone() override;

	// copy from voltage to this
	void copy(voltage_t *) override;

    double calculate_max_charge_w(double q, double qmax, double kelvin, double *max_current) override;

    double calculate_max_discharge_w(double q, double qmax, double kelvin, double *max_current) override;

    // return current for targeted power, or 0 if unable
    double calculate_current_for_target_w(double P_watts, double q, double qmax, double kelvin) override;

    double calculate_voltage_for_current(double I, double q, double qmax, double T_k) override;

    void updateVoltage(capacity_t * capacity, thermal_t * thermal, double dt) override ;

protected:

private:
    //  depth-of-discharge [%] and cell voltage [V] pairs
	std::vector<std::pair<double, double>> m_voltage_table;

    std::vector<double> slopes;
    std::vector<double> intercepts;

    double calculate_voltage(double DOD);
};

// Shepard + Tremblay Model
class voltage_dynamic_t : public voltage_t
{
public:
	voltage_dynamic_t(int num_cells_series, int num_strings, double voltage, double Vfull,
                      double Vexp, double Vnom, double Qfull, double Qexp, double Qnom,
                      double C_rate, double R, double dt_hr=1.);

	// deep copy
	voltage_dynamic_t * clone() override;

	// copy from voltage to this
	void copy(voltage_t *) override;

    double calculate_max_charge_w(double q, double qmax, double kelvin, double *max_current) override;

    double calculate_max_discharge_w(double q, double qmax, double kelvin, double *max_current) override;

	// returns current for power (discharge > 0, charge < 0), use above functions to first check feasibility
    double calculate_current_for_target_w(double P_watts, double q, double qmax, double kelvin) override;

    double calculate_voltage_for_current(double I, double q, double qmax, double T_k) override;

	void updateVoltage(capacity_t * capacity, thermal_t * thermal, double dt) override;


protected:
	double voltage_model_tremblay_hybrid(double Q_cell, double I, double q0_cell);

private:
	double _Vfull;
	double _Vexp;
	double _Vnom;
	double _Qfull;
	double _Qexp;
	double _Qnom;
	double _C_rate;
	double _A;
	double _B0;
	double _E0;
	double _K;

	void parameter_compute();

	// solver quantities
	double solver_Q;
	double solver_q;

    double solver_cutoff_voltage;

    double solver_power;
    void solve_current_for_charge_power(const double *x, double *f);
    void solve_current_for_discharge_power(const double *x, double *f);
};

// D'Agostino Vanadium Redox Flow Model
class voltage_vanadium_redox_t : public voltage_t
{
public:
	voltage_vanadium_redox_t(int num_cells_series, int num_strings, double V_ref_50, double R,
                             double dt_hr=1.);

	// deep copy
	voltage_vanadium_redox_t * clone() override;

	// copy from voltage to this
	void copy(voltage_t *) override;

    double calculate_max_charge_w(double q, double qmax, double kelvin, double *max_current) override;

    double calculate_max_discharge_w(double q, double qmax, double kelvin, double *max_current) override;

    double calculate_current_for_target_w(double P_watts, double q, double qmax, double kelvin) override;

    double calculate_voltage_for_current(double I, double q, double qmax, double T_k) override;

    void updateVoltage(capacity_t * capacity, thermal_t * thermal, double dt) override;

protected:

	// cell voltage model is on a per-cell basis
	double voltage_model(double q, double qmax, double I_string, double T);

private:

    // RC/F: R is Molar gas constant [J/mol/K]^M, R is Faraday constant [As/mol]^M, C is model correction factor^M 1.38
    double m_RCF;

    double _V_ref_50;				// Reference voltage at 50% SOC
    // solver quantities

    double solver_Q;
    double solver_q;
    double solver_T_k;              // temp in Kelvin

    double solver_power;
    void solve_current_for_power(const double *x, double *f);

    void solve_max_discharge_power(const double *x, double *f);
};


/*
Lifetime cycling class.
*/

class lifetime_cycle_t
{

public:
	lifetime_cycle_t(const util::matrix_t<double> &cyles_vs_DOD);
	virtual ~lifetime_cycle_t();

	/// deep copy
	lifetime_cycle_t * clone();

	/// copy from lifetime_cycle to this
	void copy(lifetime_cycle_t *);

	/// return q, the effective capacity percent
	double runCycleLifetime(double DOD);

	/// return hypothetical dq the average cycle
	double estimateCycleDamage();

	/// Return the relative capacity percentage of nominal (%)
	double capacity_percent();

	/// Run the rainflow counting algorithm at the current depth-of-discharge to determine cycle
	void rainflow(double DOD);

	/// Replace or partially replace a batteyr
	void replaceBattery(double replacement_percent);

	/// Return the total cycles elapse
	int cycles_elapsed();

	/// Return the range of the last cycle
	double cycle_range();

	/// Return the average cycle range
	double average_range();

protected:

	void rainflow_ranges();
	void rainflow_ranges_circular(int index);
	int rainflow_compareRanges();

	/// Bilinear interpolation, given the depth-of-discharge and cycle number, return the capacity percent
	double bilinear(double DOD, int cycle_number);

	util::matrix_t<double> _cycles_vs_DOD;
	util::matrix_t<double> _batt_lifetime_matrix;
	std::vector<double> _DOD_vect;
	std::vector<double> _cycles_vect;
	std::vector<double> _capacities_vect;


	int _nCycles;
	double _q;				// relative capacity %
	double _Dlt;			// % damage according to rainflow
	int _jlt;			    // last index in Peaks, i.e, if Peaks = [0,1], then _jlt = 1
	double _Xlt;
	double _Ylt;
	std::vector<double> _Peaks;
	double _Range;
	double _average_range;

	enum RETURN_CODES
	{
		LT_SUCCESS,
		LT_GET_DATA,
		LT_RERANGE
	};
};
/*
Lifetime calendar model
*/
class lifetime_calendar_t
{
public:
	lifetime_calendar_t(int calendar_choice, util::matrix_t<double> calendar_matrix, double dt_hour,
		float q0=1.02, float a=2.66e-3, float b=7280, float c=930);
	virtual ~lifetime_calendar_t(){/* Nothing to do */};

	// deep copy
	lifetime_calendar_t * clone();

	// copy from lifetime_calendar to this
	void copy(lifetime_calendar_t *);

	/// Given the index of the simulation, the tempertature and SOC, return the effective capacity percent
	double runLifetimeCalendarModel(size_t idx, double T, double SOC);

	/// Reset or augment the capacity
	void replaceBattery(double replacement_percent);

	/// Return the relative capacity percentage of nominal (%)
	double capacity_percent();

	enum CALENDAR_LOSS_OPTIONS {NONE, LITHIUM_ION_CALENDAR_MODEL, CALENDAR_LOSS_TABLE};

protected:
	void runLithiumIonModel(double T, double SOC);
	void runTableModel();

private:

	int _calendar_choice;
	std::vector<int> _calendar_days;
	std::vector<double> _calendar_capacity;

	int _day_age_of_battery;

	double _dt_hour; // timestep in hours
	double _dt_day; // timestep in terms of days


	// the last index of the simulation
	size_t _last_idx;

	// relative capacity (0 - 1)
	double _q;
	double _dq_old;
	double _dq_new;

	// K. Smith: Life Prediction model coeffiecients
	float _q0; // unitless
	float _a;  // 1/sqrt(day)
	float _b;  // K
	float _c;  // K
};

/*
Class to encapsulate multiple lifetime models, and linearly combined the associated degradation and handle replacements
*/
class lifetime_t
{
public:
	lifetime_t(lifetime_cycle_t *, lifetime_calendar_t *, const int replacement_option, const double replacement_capacity);
	virtual ~lifetime_t(){};

	/// Deep copy
	lifetime_t * clone();

	/// Delete deep copy
	void delete_clone();

	/// Copy lifetime to this
	void copy(lifetime_t *);

	/// Execute the lifetime models given the current lifetime run index, capacity model, and temperature
	void runLifetimeModels(size_t lifetimeIndex, capacity_t *, double T_battery);

	/// Return the relative capacity percentage of nominal (%)
	double capacity_percent();

	/// Return the relative capacity percentage of nominal caused by cycle damage (%)
	double capacity_percent_cycle();

	/// Return the relative capacity percentage of nominal caused by calendar fade (%)
	double capacity_percent_calendar();

	/// Return pointer to underlying lifetime cycle model
	lifetime_cycle_t * cycleModel() { return _lifetime_cycle; }

	/// Return pointer to underlying lifetime capacity model
	lifetime_calendar_t * calendarModel() { return _lifetime_calendar; }

	/// Check if the battery should be replaced based upon the replacement criteria
	bool check_replaced();

	/// Reset the number of replacements at the year end
	void reset_replacements();

	/// Return the number of total replacements in the year
	int get_replacements();

	/// Return the replacement percent
	double get_replacement_percent();

	/// Set the replacement option
	void set_replacement_option(int option);

	/// Replace the battery and reset the lifetime degradation
	void force_replacement(double replacement_percent);

protected:

	/// Underlying lifetime cycle model
	lifetime_cycle_t * _lifetime_cycle;

	/// Underlying lifetime calendar model
	lifetime_calendar_t * _lifetime_calendar;

	/// Replacement option, 0 = none, 1 = replace at capacity 2 = replace by schedule
	int _replacement_option;

	/// Maximum capacity relative to nameplate at which to replace battery
	double _replacement_capacity;

	/// Number of replacements this year
	int _replacements;

	/// Boolean describing if replacement has been scheduled
	bool _replacement_scheduled;

	/// Percentage of how much capacity to replace (0 - 100%)
	double _replacement_percent;

	/// battery relative capacity (0 - 100%)
	double _q;
};


/*
Thermal classes
*/
class thermal_t
{
public:
	thermal_t();
	thermal_t(double dtHour, double mass, double length, double width, double height,
		double Cp, double h,
		std::vector<double> T_room,
		const util::matrix_t<double> &cap_vs_temp);

	// deep copy
	thermal_t * clone();

	// copy thermal to this
	void copy(thermal_t *);

	void updateTemperature(double I, double R, double dt, size_t lifetimeIndex);
	void replace_battery(size_t lifetimeIndex);

	// outputs
	double T_battery();
	double capacity_percent();
	message get_messages(){ return _message; }

protected:
	double f(double T_battery, double I, size_t lifetimeIndex);
	double rk4(double I, double dt, size_t lifetimeIndex);
	double trapezoidal(double I, double dt, size_t lifetimeIndex);
	double implicit_euler(double I, double dt, size_t lifetimeIndex);

protected:

	util::matrix_t<double> _cap_vs_temp;

	double _dt_hour;    // [hr] - timestep
	double _mass;		// [kg]
	double _length;		// [m]
	double _width;		// [m]
	double _height;		// [m]
	double _Cp;			// [J/KgK] - battery specific heat capacity
	double _h;			// [Wm2K] - general heat transfer coefficient
	std::vector<double> _T_room; // [K] - storage room temperature
	double _R;			// [Ohm] - internal resistance
	double _A;			// [m2] - exposed surface area
	double _T_battery;   // [K]
	double _capacity_percent; //[%]
	double _T_max;		 // [K]
	message _message;

};
/**
* \class losses_t
*
* \brief
*
*  The Battery losses class takes generic losses which occur during charging, discharge, or idle operation modes:
*  The model also accepts a time-series vector of losses defined for every time step of the first year of simulation
*  which may be used in lieu of the losses for operational mode.
*/
class losses_t
{
public:

	/**
	* \function losses_t
	*
	* Construct the losses object
	*
	* \param[in] lifetime_t * pointer to lifetime class
	* \param[in] thermal_t * pointer to thermal class (currently unused)
	* \param[in] capacity_t * pointer to capacity class
	* \param[in] loss_mode 0 for monthy input, 1 for input time series
	* \param[in] batt_loss_charge_kw vector (size 1 for annual or 12 for monthly) containing battery system losses when charging (kW)
	* \param[in] batt_loss_discharge_kw vector (size 1 for annual or 12 for monthly) containing battery system losses when discharge (kW)
	* \param[in] batt_loss_idle_kw vector (size 1 for annual or 12 for monthly) containing battery system losses when idle (kW)
	* \param[in] batt_loss_kw vector (size 1 for annual or 12 for monthly) containing battery system losses when idle (kW)
	*/
	losses_t(double dtHour,
			lifetime_t *,
			thermal_t *,
			capacity_t*,
			const int loss_mode,
			const double_vec batt_loss_charge_kw = std::vector<double>(0),
			const double_vec batt_loss_discharge_kw = std::vector<double>(0),
			const double_vec batt_loss_idle_kw = std::vector<double>(0),
			const double_vec batt_loss_kw=std::vector<double>(0));

	void set_models(lifetime_t *, thermal_t *, capacity_t*);

	/// Copy input losses to this object
	void copy(losses_t *);

	/// Run the losses model at the present simulation index (for year 1 only)
	void run_losses(size_t lifetimeIndex);

	/// Get the loss at the specified simulation index (year 1)
	double getLoss(size_t indexFirstYear);

	/// Options for the loss inputs to use
	enum { MONTHLY, TIMESERIES};

protected:

    int _loss_mode;
	double _dtHour;

	lifetime_t * _lifetime;
	thermal_t * _thermal;
	capacity_t * _capacity;
	double_vec  _charge_loss;
	double_vec  _discharge_loss;
	double_vec  _idle_loss;
	double_vec  _full_loss;
};

/*
Class which encapsulates a battery and all its models
*/

class battery_t
{
public:
	battery_t();
	battery_t(double dt, int battery_chemistry);

	// deep copy constructor (new memory), from battery to this
	battery_t(const battery_t& battery);

	// copy members from battery to this
	void copy(const battery_t * battery);

	// virtual destructor, does nothing as no memory allocated in constructor
	virtual ~battery_t();

	// delete the new submodels that have been allocated
	void delete_clone();

	void initialize(capacity_t *, voltage_t *, lifetime_t *, thermal_t *, losses_t *);

	// Run all for single time step, updating all component model states and return the dispatched power [kW]
	double run(size_t lifetimeIndex, double &I);

	double calculate_voltage_for_current(double I);

	// Return the max charge or discharge power achievable in the next time step, and the required current [A]
    double calculate_max_charge_kw(double *max_current_A = nullptr);
    double calculate_max_discharge_kw(double *max_current_A = nullptr);

	// Returns current [A] required to dispatch input power [kW], or the max power (to which P_kw is set)
	double calculate_current_for_power_kw(double &P_kw);

    // Run a component level model
	void runCapacityModel(double &I);
	void runVoltageModel();
	void runThermalModel(double I, size_t lifetimeIndex);
	void runLifetimeModel(size_t lifetimeIndex);
	void runLossesModel(size_t lifetimeIndex);

	capacity_t * capacity_model() const;
	capacity_t * capacity_initial_model() const;
	voltage_t * voltage_model() const;
	lifetime_t * lifetime_model() const;
	thermal_t * thermal_model() const;
	thermal_t * thermal_initial_model() const;
	losses_t * losses_model() const;

	// Get capacity quantities
	double battery_charge_needed(double SOC_max);
	double battery_charge_total();
	double battery_charge_maximum();
	double battery_charge_maximum_thermal();
	double battery_energy_nominal();
	double battery_energy_to_fill(double SOC_max);
	double battery_power_to_fill(double SOC_max);
	double battery_soc();

	// Get Voltage
	double cell_voltage();
	double battery_voltage(); // the actual battery voltage
	double battery_voltage_nominal(); // the nominal battery voltage

	enum CHEMS{ LEAD_ACID, LITHIUM_ION, VANADIUM_REDOX, IRON_FLOW};
	enum REPLACE{ NO_REPLACEMENTS, REPLACE_BY_CAPACITY, REPLACE_BY_SCHEDULE};


private:
	capacity_t * _capacity;
	capacity_t * _capacity_initial;
	thermal_t * _thermal;
	thermal_t * _thermal_initial;
	lifetime_t * _lifetime;
	voltage_t * _voltage;
	losses_t * _losses;
	int _battery_chemistry;
	double _dt_hour;			// [hr] - timestep
	double _dt_min;				// [min] - timestep
	size_t _last_idx;
};

#endif
