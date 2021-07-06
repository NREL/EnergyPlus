#ifndef _LIB_BATTERY_POWERFLOW_H_
#define _LIB_BATTERY_POWERFLOW_H_

#include <memory>

// Forward declarations
class SharedInverter;

struct BatteryPower;

/**
* \class BatteryPowerFlow
*
* \brief
*
*  The BatteryPowerFlow class provides the calculations for which components of the system power flow to/from the battery,
*  which power components go to meet the electric load, and how the utility grid is used.  It is meant to be shared by
*  the lib_power_electronics charge controllers, which require AC power calculations, and the battery dispatch model, 
*  which require DC power calculations.  The interaction of this model with other's may seem complex, but the general
*  design of the battery model is:
*
*  1. ChargeController - contains information about conversion efficiencies and configuration, which is passed to BatteryPowerFlow
*  2. Dispatch - contains information about the dispatch strategy desired, and constraints around when the battery can
*				 charge or discharge, and constraints around the state-of-charge, power and current throughput limits
*				 Within the Dispatch, the following steps are taken:
*				 a) Calculate battery power given the dispatch strategy and contraints
*				 b) Dispatch the battery with that power (current)
*				 c) Iterate on the current (due to the nonlinear relationship in P = IV) until the constraints are met
*				 d) Calculate the final power flow for the time step.
*/
class BatteryPowerFlow
{
public:
	/// Create a BatteryPowerFlow object
	BatteryPowerFlow(double dtHour);

	/// Perform a deep copy of a BatteryFlow object
	BatteryPowerFlow(const BatteryPowerFlow& powerFlow);

	/// Initialize the power flow for the battery system.  Only needs to be called for manual dispatch control
	void initialize(double stateOfCharge);

	/// Reset the power flow for a new time step
	void reset();

	/// Calculate the power flow for the battery system
	void calculate();

	/// Get Battery Power object
	BatteryPower * getBatteryPower();

private:

	/**
	* \function calculateACConnected
	*
	* Calculate the power flow for an AC connected battery system.  This calculation respects basic constraints about whether
	* a battery is allowed to charge from PV, the Grid, and whether it is allowed to discharge.  The calculation also makes
	* the following assumptions.
	*
	*  0. The BatteryPower contains the powerBattery that the dispatch has determined best fits the constraints
	*  1. Battery is charged from PV before the Grid
	*  2. Battery is charged from the Grid for any remaining power, even if this violates grid charging constraint
	*  3. Battery discharges to the electric load first
	*  4. Any additional battery discharge goes to the Grid.
	*  5. In the event that the battery is allowed to charge and discharge, it will discharge.
	*
	*/
	void calculateACConnected();

	/// Calculate the power flow for an DC connected battery system
	void calculateDCConnected();

	std::unique_ptr<BatteryPower> m_BatteryPower;   /// A structure containing the AC power flow components 
};

/**
* \struct BatteryPower
*
* \brief
*
*  The BatteryPower structure contains all of the power flow components for a battery simulation
*  The structure also contains information about the single point efficiecies required to convert power from one form to another
*  Power quantities in BatteryPower are either all AC or all DC depending on which part of the controller is looking at it
*/
struct BatteryPower
{
public:

	/// Create a BatteryPower object.
	BatteryPower(double dtHour);

	/// Perform a deep copy of a BatteryPower object
	BatteryPower(const BatteryPower& batteryPower);

	/// Register the shared inverter which has previously been defined
	void setSharedInverter(SharedInverter * sharedInverter);

	/// Reset the time-step power quantities to 0
	void reset();

    /*
     * Multiply or divide by ac efficiency for discharging or charging
     * Auxiliary discharging losses are applied to the battery, charging and idle losses to PV or grid
     */
    double adjustForACEfficiencies(double power, double loss);

    /*
     * Multiply or divide by dc efficiency for discharging or charging
     * Auxiliary discharging losses are applied to the battery, chargingand idle losses to PV or grid
     */
    double adjustForDCEfficiencies(double power, double loss);

	/// Copy the enumeration for AC/DC connected systems from ChargeController
	enum CONNECTION { DC_CONNECTED, AC_CONNECTED };


	double dtHour;	   /// The timestep in hours, used for accumulated power losses

	double powerSystem;				   ///< The power production of the renewable system (PV array) (kW)
	double powerSystemThroughSharedInverter; ///< The power going through the shared inverter (kW)
	double powerLoad;			   ///< The power required by the electric load (kW)
	double powerBatteryDC; 	       ///< The power flow to and from the battery (> 0, discharging, < 0 charging) (kWdc)
	double powerBatteryAC; 	       ///< The power flow to and from the battery (> 0, discharging, < 0 charging) (kWac)
	double powerBatteryTarget;	   ///< A user specified or algorithm calculated target dispatch power (kW)
	double powerGrid;              ///< The power flow to and from the grid (> 0, to grid, < 0 from grid) (kW)
	double powerGeneratedBySystem; /// The power generated by the combined power generator and battery (kW)
	double powerSystemToLoad;      ///< The power from the renewable system to the electric load (kW)
	double powerSystemToBattery;   ///< The power from renewable system to the battery (kW)
	double powerSystemToGrid;      ///< The power from renewable system to the grid (kW)
	double powerSystemClipped;	   ///< The power from renewable system that will be clipped if not used in the battery (kW)
	double powerClippedToBattery;  ///< The power from that would otherwise have been clipped to the battery (kW)
	double powerGridToBattery;     ///< The power from the grid to the battery (kW)
	double powerGridToLoad;        ///< The power from the grid to the electric load (kW)
	double powerBatteryToLoad;     ///< The power from the battery to the electric load (kW)
	double powerBatteryToGrid;     ///< The power from the battery to the grid (kW)
	double powerFuelCell;          ///< The power from the fuelcell (kW)
	double powerFuelCellToGrid;    ///< The power from the fuelcell to the grid (kW)
	double powerFuelCellToLoad;    ///< The power from the fuelcell to the load (kW)
	double powerFuelCellToBattery; ///< The power from the fuelcell to the battery (kW)
	double powerPVInverterDraw;	   ///< The power draw from the PV inverter (kW)
	double powerBatteryChargeMaxDC;  ///< The maximum sustained power the battery can charge (kWdc)
	double powerBatteryDischargeMaxDC; ///< The maximum sustained power the battery can discharge (kWdc)
	double powerBatteryChargeMaxAC;   ///< The maximum sustained power the battery can charge (kWac)
	double powerBatteryDischargeMaxAC; ///< The maximum sustained power the battery can discharge (kWac)
	double powerSystemLoss;        ///< The auxiliary power loss in the system (kW)
	double powerConversionLoss;    ///< The power loss due to conversions in the battery power electronics (kW)
	double voltageSystem;		   ///< The system voltage

	//double annualEnergySystemLoss;  /// The total annual loss due to user-specified system losses
	//double annualEnergyConversionLoss;  /// The total annual loss due to power electronic conversions

	int connectionMode;					 ///< 0 if DC-connected, 1 if AC-connected
	int meterPosition;					 ///< 0 if behind-the-meter, 1 if front-of-meter
	double singlePointEfficiencyACToDC;  ///< The conversion efficiency from AC power to DC power within the battery microinverter (0 - 1)
	double singlePointEfficiencyDCToAC;  ///< The conversion efficiency from DC power to AC power within the battery microinverter (0 - 1)
	double singlePointEfficiencyDCToDC;  ///< The conversion efficiency from DC power to DC power within the battery management system (0 - 1)
	SharedInverter * sharedInverter;	 ///< The shared inverter between the PV and battery for a DC-connected system
	double inverterEfficiencyCutoff;     ///< The shared inverter efficiency at which to stop charging or discharging due to inefficient operation (0-1)


	bool canSystemCharge;	///< A boolean specifying whether the battery is allowed to charge from PV in the timestep
	bool canClipCharge;	///< A boolean specifying whether the battery is allowed to charge from otherwise clipped PV in the timestep
	bool canGridCharge; ///< A boolean specifying whether the battery is allowed to charge from the Grid in the timestep
	bool canDischarge;  ///< A boolean specifying whether the battery is allowed to discharge in the timestep
	bool canFuelCellCharge; ///< A boolean specifying whether the battery is allowed to charge from the fuel cell

	double stateOfChargeMax;   ///< The maximum state of charge (0-100)
	double stateOfChargeMin;   ///< The minimum state of charge (0-100)
	double depthOfDischargeMax; ///< The maximum depth of discharge (0-100)

	double currentChargeMax; ///< The maximum sustained current for charging [A]
	double currentDischargeMax; ///< The maximum sustained current for discharging [A]


	double tolerance;  ///< A numerical tolerance. Below this value, zero out the power flow
};


#endif
