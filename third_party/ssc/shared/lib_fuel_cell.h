#ifndef __LIB_FUEL_CELL__
#define __LIB_FUEL_CELL__

#include <map>
#include "lib_util.h"

const double BTU_PER_KWH = 3412.14163;
const double MMBTU_PER_BTU = 1000000;
const double BTU_PER_MMBTU = 1.0 / MMBTU_PER_BTU;
const double FT3_PER_MCF = 1000;

#ifndef BTU_TO_MCF
#define BTU_TO_MCF(BTU, LHV_BTU_PER_FT3) (BTU / (LHV_BTU_PER_FT3 * FT3_PER_MCF))
#endif

#ifndef MCF_TO_BTU
#define MCF_TO_BTU(MCF, LHV_BTU_PER_FT3) (MCF * LHV_BTU_PER_FT3 * FT3_PER_MCF)
#endif

#ifndef MCF_TO_KWH
#define MCF_TO_KWH(MCF, LHV_BTU_PER_FT3) (MCF_TO_BTU(MCF, LHV_BTU_PER_FT3) / BTU_PER_KWH)
#endif


/**
* \class FuelCell
*
* \brief
*
*  The FuelCell class provides the underlying data and methods required to model fuel cell technology in SAM.
*/
class FuelCell
{
public:
	/// Default FuelCell constructor
	FuelCell();

	/// Construct FuelCell with arguments
	FuelCell(double unitPowerMax_kW, double unitPowerMin_kW, 
		double startup_hours, bool is_started, double shutdown_hours,
		double dynamicResponseUp_kWperMin, double dynamicResponseDown_kWperMin,
		double degradation_kWperHour, double degradationRestart_kW, 
		size_t replacement_option, double replacement_percent, std::vector<size_t> replacementSchedule,
		util::matrix_t<size_t> shutdownTable,
		size_t efficiencyChoice, util::matrix_t<double> efficiencyTable,
		double lowerHeatingValue_BtuPerFt3, double higherHeatingValue_BtuPerFt3, double availableFuel_MCf,
		int shutdownOption,  double dt_hour);

	/// Default destructor
	~FuelCell();

	/// Copy Constructor
	FuelCell(const FuelCell &fuelCell);

	/// Run for single time step
	void runSingleTimeStep(double power_kW);

	/// Initialize hour zero
	void initializeHourZeroPower(double power_kW);

	/// Return true if starting up but not fully running
	bool isStarting();

	/// Return true if operating
	bool isRunning();

	/// Return true if shutting down
	bool isShuttingDown();

	/// Return true if totally shut down
	bool isShutDown();

	/// Return false if hour zero needs initial power from dispatch
	bool isInitialized();

	/// Get original max power kW
	double getMaxPowerOriginal();

	/// Get fuel cell max power kW
	double getMaxPower();

	/// Get fuel cell min power kW
	double getMinPower();

	/// Return the final power kW
	double getPower();

	/// Return the final heat kW
	double getPowerThermal();

	/// Get the maximum power percentage available
	double getPowerMaxPercent();

	/// Return percentage based on requested power (0-100)
	double getPercentLoad();

	/// Return the fractional representation of load (0-1)
	double getLoadFraction();

	/// Return the fuel consumption in MCF
	double getFuelConsumption();

	/// Get efficiency percent
	double getElectricalEfficiency();

	/// Get heat recovery percent
	double getHeatRecoveryEfficiency();

	/// Return the available fuel in MCF
	double getAvailableFuel();

	/// Return the number of replacements
	int getTotalReplacements();

	/// Reset the number of replacements
	void resetReplacements();

	/// Update system properties (for testing)
	void setSystemProperties(double nameplate_kW, double min_kW, double startup_hours, double shutdown_hours,
		double dynamicResponseUp_kWperHour, double dynamicResponseDown_kWperHour);

	/// Update replacement options (for testing)
	void setReplacementOption(size_t replacementOption);
	void setReplacementCapacity(double replacement_percent);

	/// Update restart degradation
	void setDegradationRestartkW(double);

	/// Update degradation (for testing)
	void setDegradationkWPerHour(double degradation_kWPerHour);

	/// Set shutdown option
	void setShutdownOption(int option);

	/// Update restart degradation
	void setScheduledShutdowns(util::matrix_t<size_t> shutdowns);

	/// Set startup hours
	void setStartupHours(double startup_hours, bool is_started);

	/// Calculate fuel consumption at percent load
	void calculateEfficiencyCurve(double percent);

	/// Shutdown option enumerations
	enum FC_SHUTDOWN_OPTION { SHUTDOWN, IDLE };

	/// Fuel cell replacement options
	enum FC_REPLACEMENT_OPTION { NONE, REPLACE_AT_CAPACITY, REPLACE_ON_SCHEDULE };

protected:

	enum FC_EFFICIENCY_COLUMN { PERCENT_MAX, PRECENT_ELECTRICAL_EFFICIENCY, PERCENT_HEAT_RECOVERY };
	enum FC_SHUTDOWN_COLUMN {HOUR, DURATION};
	enum FC_EFFICIENCY_CHOICE { ORIGINAL_NAMEPLATE, DEGRADED_MAX};

	/// Calculate time
	void calculateTime();

	/// Apply degradation 
	void applyDegradation();

	/// Apply degradation 
	void applyEfficiency();

	/// interpolate map
	double interpolateMap(double key, std::map<double, double>);

	/// Check status of fuel cell
	void checkStatus(double power_kW);

	/// Check Min Turndown
	void checkMinTurndown();

	/// Check Max Limit
	void checkMaxLimit();

	/// Check Available Fuel
	void checkAvailableFuel();

	/// Check fuel cell power response conforms to dynamic limits
	void checkPowerResponse();

	// Initialize calculated
	void init();

	// input values
	double dt_hour;
	double m_unitPowerMax_kW;
	double m_unitPowerMin_kW;
	double m_startup_hours;
	bool m_is_started;
	double m_shutdown_hours;

	double m_dynamicResponseUp_kWperHour;
	double m_dynamicResponseDown_kWperHour;
	
	double m_degradation_kWperHour;
	double m_degradationRestart_kW;
	util::matrix_t<size_t> m_scheduledShutdowns;

	size_t m_replacementOption;
	double m_replacement_percent;
	std::vector<size_t> m_replacementSchedule;

	size_t m_efficiencyChoice;
	util::matrix_t<double> m_efficiencyTable;
	double m_lowerHeatingValue_BtuPerFt3;
	double m_higherHeatingValue_BtuPerFt3;
	double m_availableFuel_MCf;
	int m_shutdownOption;

	// calculated
	bool m_initialized;

	bool m_startingUp;
	bool m_startedUp;

	bool m_shuttingDown;
	bool m_shutDown;

	double m_hoursSinceStart;
	double m_hoursSinceStop;
	double m_hoursRampUp;

	double m_powerMax_kW;     ///< Maximum power after degradation
	double m_powerThermal_kW; 
	double m_power_kW;
	double m_powerMaxPercentOfOriginal_percent; ///< The percentage of max power available relative to original
	double m_powerLoad_percent; ///< The power load relative to the degraded maximum

	double m_powerPrevious_kW;
	double m_fuelConsumed_MCf;
	double m_efficiency_percent;
	double m_heatRecovery_percent;
	int m_replacementCount;
	std::map<double, double> m_fuelConsumptionMap_MCf;
	std::map<double, double> m_efficiencyMap;
	std::map<double, double> m_heatRecoveryMap;
	double m_hour;
	size_t m_year;

};

#endif 
