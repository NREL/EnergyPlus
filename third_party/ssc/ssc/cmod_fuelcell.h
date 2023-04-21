/*******************************************************************************************************
*  Copyright 2017 Alliance for Sustainable Energy, LLC
*
*  NOTICE: This software was developed at least in part by Alliance for Sustainable Energy, LLC
*  (Alliance) under Contract No. DE-AC36-08GO28308 with the U.S. Department of Energy and the U.S.
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
*  the underlying software originally provided by Alliance as System Advisor Model or SAM. Except
*  to comply with the foregoing, the terms System Advisor Model, SAM, or any confusingly similar
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
#ifndef _CMOD_FUEL_CELL_H_
#define _CMOD_FUEL_CELL_H_

#include <map>
#include <memory>

#include "core.h"
#include "lib_fuel_cell.h"
#include "lib_fuel_cell_dispatch.h"
#include "lib_util.h"

struct fuelCellVariables
{
public:
	
	fuelCellVariables() {/* nothing to do */ };
	fuelCellVariables(compute_module & cm) :
		systemUseLifetimeOutput(cm.as_boolean("system_use_lifetime_output")),
		unitPowerMax_kW(cm.as_double("fuelcell_unit_max_power")),
		unitPowerMin_kW(cm.as_double("fuelcell_unit_min_power")),
		shutdown_hours(cm.as_double("fuelcell_shutdown_time")),
		startup_hours(cm.as_double("fuelcell_startup_time")),
		is_started(cm.as_double("fuelcell_is_started")),
		dynamicResponseUp_kWperHour(cm.as_double("fuelcell_dynamic_response_up")),
		dynamicResponseDown_kWperHour(cm.as_double("fuelcell_dynamic_response_down")),
		degradation_kWperHour(cm.as_double("fuelcell_degradation")),
		degradationRestart_kW(cm.as_double("fuelcell_degradation_restart")),
		replacementOption(cm.as_unsigned_long("fuelcell_replacement_option")),
		replacement_percent(cm.as_double("fuelcell_replacement_percent")),
		replacementSchedule(cm.as_vector_unsigned_long("fuelcell_replacement_schedule")),
		efficiencyTable(cm.as_matrix("fuelcell_efficiency")),
		efficiencyChoice(cm.as_unsigned_long("fuelcell_efficiency_choice")),
		shutdownTable(cm.as_matrix_unsigned_long("fuelcell_availability_schedule")),
		lowerHeatingValue_BtuPerFt3(cm.as_double("fuelcell_lhv")),
		higherHeatingValue_BtuPerFt3(cm.as_double("fuelcell_lhv")),
		availableFuel_MCf(cm.as_double("fuelcell_fuel_available")),
		shutdownOption(cm.as_integer("fuelcell_operation_options")),
		numberOfUnits(cm.as_integer("fuelcell_number_of_units")),
		dispatchOption(cm.as_integer("fuelcell_dispatch_choice")),
		fixed_percent(cm.as_double("fuelcell_fixed_pct")),
		dispatch_kW(cm.as_vector_double("fuelcell_dispatch")),
		canCharge(cm.as_vector_bool("dispatch_manual_fuelcellcharge")),
		canDischarge(cm.as_vector_bool("dispatch_manual_fuelcelldischarge")),
		discharge_percent(cm.as_vector_double("dispatch_manual_percent_fc_discharge")),
		discharge_units(cm.as_vector_unsigned_long("dispatch_manual_units_fc_discharge")),
		scheduleWeekday(cm.as_matrix_unsigned_long("dispatch_manual_sched")),
		scheduleWeekend(cm.as_matrix_unsigned_long("dispatch_manual_sched_weekend"))
	{
		numberOfYears = 1;
		if (systemUseLifetimeOutput) {
			numberOfYears = cm.as_unsigned_long("analysis_period");
		}

		// Load is always a non-lifetime input
		if (cm.is_assigned("load")) {
			electricLoad_kW = cm.as_vector_double("load");
		}
		
		// Choose between gen and ac, user should only put in one, but will prefer 'gen' if input
		if (cm.is_assigned("gen")) {
			systemGeneration_kW = cm.as_vector_double("gen");
			numberOfRecordsPerYear = systemGeneration_kW.size() / numberOfYears;
		}
		// It's okay if there is no input generation or load, initialize to zero
		else {
			numberOfRecordsPerYear = (size_t)std::fmax(electricLoad_kW.size(), 8760);
			systemGeneration_kW.reserve(numberOfRecordsPerYear * numberOfYears);
			for (size_t j = 0; j < numberOfRecordsPerYear * numberOfYears; j++) {
				systemGeneration_kW.push_back(0.0);
			}
		}

		// Timesteps
		numberOfLifetimeRecords = numberOfRecordsPerYear * numberOfYears;
		stepsPerHour = numberOfRecordsPerYear / (size_t)8760;
		dt_hour = (double)(1.0 / (double)stepsPerHour);

		// Ensure load matches generation size
		std::vector<double> load = electricLoad_kW;
		electricLoad_kW.clear();
		electricLoad_kW.reserve(numberOfLifetimeRecords);

		// Front of meter
		if (load.size() == 0) {
			for (size_t k = 0; k < numberOfLifetimeRecords; k++) {
				electricLoad_kW.push_back(0.0);
			}
		}
		// Behind the meter, load = gen size
		else if (load.size() == numberOfRecordsPerYear) {
			for (size_t y = 0; y < numberOfYears; y++) {
				for (size_t i = 0; i < numberOfRecordsPerYear; i++) {
					electricLoad_kW.push_back(load[i]);
				}
			}
		}
		// Behind the meter, hourly load assumed constant 
		else if (load.size() == 8760) {
			for (size_t y = 0; y < numberOfYears; y++) {
				for (size_t h = 0; h < 8760; h++) {
					double loadHour = load[h];
					for (size_t s = 0; s < stepsPerHour; s++) {
						electricLoad_kW.push_back(loadHour);
					}
				}
			}
		}
		else {
			throw exec_error("fuelcell", "Electric load time steps must equal generation time step or 8760");
		}

		size_t count = 0;
		for (size_t p = 0; p < canDischarge.size(); p++) {
			if (canDischarge[p]) {
				discharge_percentByPeriod[p] = discharge_percent[count];
				discharge_unitsByPeriod[p] = discharge_units[count];
				count++;
			}
		}
	}

	// simulation inputs
	bool systemUseLifetimeOutput;
	size_t numberOfYears;
	size_t numberOfRecordsPerYear;
	size_t numberOfLifetimeRecords;
	size_t stepsPerHour;

	// generation input
	std::vector<double> systemGeneration_kW;

	// electric load input
	std::vector<double> electricLoad_kW;

	// fuel cell
	double dt_hour;
	double unitPowerMax_kW;
	double unitPowerMin_kW;
	double shutdown_hours;
	double startup_hours;
	bool is_started;
	double dynamicResponseUp_kWperHour;
	double dynamicResponseDown_kWperHour;
	double degradation_kWperHour;
	double degradationRestart_kW;
	size_t replacementOption;
	double replacement_percent;
	std::vector<size_t> replacementSchedule;
	util::matrix_t<double> efficiencyTable;
	size_t efficiencyChoice;
	util::matrix_t<size_t> shutdownTable;
	double lowerHeatingValue_BtuPerFt3;
	double higherHeatingValue_BtuPerFt3;
	double availableFuel_MCf;
	int shutdownOption;

	// dispatch
	size_t numberOfUnits;
	int dispatchOption;
	double fixed_percent;
	std::vector<double> dispatch_kW;
	std::vector<bool> canCharge;
	std::vector<bool> canDischarge;
	std::vector<double> discharge_percent;
	std::vector<size_t> discharge_units;
	std::map<size_t, double> discharge_percentByPeriod;
	std::map<size_t, size_t> discharge_unitsByPeriod;
	util::matrix_t<size_t> scheduleWeekday;
	util::matrix_t<size_t> scheduleWeekend;
};

extern var_info vtab_fuelcell_input[];
extern var_info vtab_fuelcell_output[];

class cm_fuelcell : public compute_module 
{
public: 

	/// Default constructor
	cm_fuelcell();

	/// Default destructor
	~cm_fuelcell();

	/// construct since compute_module framework is fundamentally broken
	void construct();

	/// Main execution
	void exec() override;

	/// Allocate Outputs
	void allocateOutputs();

protected:

	// internally allocated
	std::unique_ptr<fuelCellVariables> fcVars;
	std::unique_ptr<FuelCell> fuelCell;
	std::unique_ptr<FuelCellDispatch> fuelCellDispatch;

	// outputs
	ssc_number_t * p_gen_kW;
	ssc_number_t * p_fuelCellPower_kW;
	ssc_number_t * p_fuelCellPowerMaxAvailable_percent;
	ssc_number_t * p_fuelCellLoad_percent;
	ssc_number_t * p_fuelCellElectricalEfficiency_percent;
	ssc_number_t * p_fuelCellPowerThermal_kW;
	ssc_number_t * p_fuelCellConsumption_MCf;
	ssc_number_t * p_fuelCellToGrid_kW;
	ssc_number_t * p_fuelCellToLoad_kW;
	ssc_number_t * p_fuelCellReplacements;
	ssc_number_t * p_fuelCellConsumption_MCf_annual;
};

#endif
