#ifndef __LIB_PV_IO_MANAGER_H__
#define __LIB_PV_IO_MANAGER_H__

#include <map>
#include <memory>
#include <math.h>
#include <string>

#include "6par_solve.h"
#include "lib_cec6par.h"
#include "lib_iec61853.h"
#include "lib_irradproc.h"
#include "lib_mlmodel.h"
#include "lib_ondinv.h"
#include "lib_pvinv.h"
#include "lib_pv_incidence_modifier.h"
#include "lib_pvshade.h"
#include "lib_sandia.h"
#include "lib_shared_inverter.h"
#include "lib_snowmodel.h"
#include "lib_util.h"

#include "../ssc/common.h"
#include "../ssc/core.h"

enum modulePowerModelList { MODULE_SIMPLE_EFFICIENCY, MODULE_CEC_DATABASE, MODULE_CEC_USER_INPUT, MODULE_SANDIA, MODULE_IEC61853, MODULE_PVYIELD };
enum inverterTypeList { INVERTER_CEC_DATABASE, INVERTER_DATASHEET, INVERTER_PARTLOAD, INVERTER_COEFFICIENT_GEN, INVERTER_PVYIELD };

/// Structure containing data relevent at the SimulationManager level
struct Simulation_IO;

/// Structure contain data relevent to the Irradiance model
struct Irradiance_IO;

/// Structure containing subarray-level IO information
struct Subarray_IO;

/// Structure containing module-level IO information
struct Module_IO;

/// Structure containing module-level IO information
struct Inverter_IO;

/// Structure containing the aggregate outputs for all subarrays
struct PVSystem_IO;

/**
* \class flag
*
* This class implements error checking for bool or enum flags, such as checking for initialization
* before use in comparisons. Allows only == and !=  operations with integral types and with other flags.
*
*/
class flag
{
	typedef void (flag::*bool_type)() const;
	void safeBool() const {}

public:
	bool init = false;
	int value;				//< for enum or boolean values

	/// set enum or implicit bool value
	void operator =(int setValue) {
		if (setValue <= -1)
			return;
		init = true;
		value = setValue;
	}

	void checkInit() const {
		if (!init)
			throw exec_error("PV IO Manager",
				"Flag used without initialization.");
	}

	/// safe bool conversion operator
	operator bool_type() const {
		checkInit();
		return value ? &flag::safeBool : 0;
	}

	bool operator ==(const int testValue) {
		checkInit();
		return (value == testValue);
	}
	bool operator !=(const int testValue) {
		checkInit();
		return (value != testValue);
	}
};


/**
* \class PVIOManager
*
* This class contains the input and output data needed by all of the submodels in the detailed PV model
* It is intended to be passed as a pointer and modified with the goal of encapsulating all input that comes
* in at the user level and all output that needs to ultimately be passed back to the user.
*
*
* \note The PVIOManager contains structures that contain specific information about each system component
*/
class PVIOManager
{
public:
	/// Create a PVIOManager object by parsing the compute model
	PVIOManager(compute_module* cm, std::string cmName);

	/// Allocate Outputs
	void allocateOutputs(compute_module*  cm);

	/// Return pointer to compute module
	compute_module * getComputeModule();

	/// Return Simulation specific information
	Simulation_IO * getSimulationIO();

	/// Return Irradiance specific information
	Irradiance_IO * getIrradianceIO();

	/// Return Subarray specific information for the given subarray
	Subarray_IO * getSubarrayIO(size_t subarray);

	/// Return all Subarray's
	std::vector<Subarray_IO *> getSubarrays();

	/// Get PVSystem as one object
	PVSystem_IO * getPVSystemIO();

	/// Get Shade Database
	ShadeDB8_mpp * getShadeDatabase();

public:

	/** These structures contain specific IO data for each part of the model
	* They are owned exclusively by the PVIOManager
	*/
	std::unique_ptr<Simulation_IO> m_SimulationIO;
	std::unique_ptr<Irradiance_IO> m_IrradianceIO;
	std::unique_ptr<PVSystem_IO> m_PVSystemIO;
	std::unique_ptr<Inverter_IO> m_InverterIO;
	std::vector<std::unique_ptr<Subarray_IO>> m_SubarraysIO;
	std::unique_ptr<ShadeDB8_mpp> m_shadeDatabase;
	size_t nSubarrays;

private:

	/// A pointer to the underlying compute module object which we don't manage
	compute_module * m_computeModule;

	/// The compute module name
	std::string m_computeModuleName;
};

/**
* \struct Irradiance_IO
*
* This structure contains the input and output data needed by the IrradianceModel
* It is contained within the IOManager.
*
* \note The data contained in Irradiance_IO is primarily the data read in from the weather file
*		In general, the data is independent of a subarray, with the exception of plane-of-array (POA)
*		irradiance data, which may be specified in the weatherfile on a subarray basis.
*
*/
struct Irradiance_IO
{
	/// Construct the Irradiance_IO structure from the compute module input.  This sets up all inputs for the IrradianceModel
	Irradiance_IO(compute_module* cm, std::string cmName);

	/// Check weather file
	void checkWeatherFile(compute_module* cm, std::string cmName);

	/// Allocate the Irradiance_IO outputs
	void AllocateOutputs(compute_module* cm);

	/// Assign outputs from member data after the IrradianceModel has run
	void AssignOutputs(compute_module* cm);

	// Constants

	static const int irradprocNoInterpolateSunriseSunset = -1;    /// Interpolate the sunrise/sunset



	// Irradiance Data Inputs
	std::unique_ptr<weather_data_provider> weatherDataProvider;   /// A class which encapsulates the weather data regardless of input method
	weather_record weatherRecord;								  /// Describes the weather data
	weather_header weatherHeader;								  /// Describes the weather data header
	double tsShiftHours;										  /// Sun position time offset
	flag instantaneous;											  /// Describes whether the weather data is instantaneous (or not)
	size_t numberOfWeatherFileRecords;							  /// The number of records in the weather file
	size_t stepsPerHour;										  /// The number of steps per hour
	size_t numberOfSubarrays;									  /// The number of subarrays (needed if reading POA data from weather file)
	double dtHour;											      /// The timestep in fraction of an hour (e.g. for 15-minute data, dtHour would be 0.25)
	int radiationMode;											  /// Specify which components of radiance should be used: 0=B&D, 1=G&B, 2=G&D, 3=POA-Ref, 4=POA-Pyra
	int skyModel;												  /// Specify which sky diffuse model should be used: 0=isotropic, 1=hdkr, 2=perez
	flag useWeatherFileAlbedo;									  /// Specify whether to use the weather file albedo
	std::vector<double> userSpecifiedMonthlyAlbedo;				  /// User can provide monthly ground albedo values (0-1)

	// Irradiance data Outputs (p_ is just a convention to organize all pointer outputs)
	ssc_number_t * p_weatherFileGHI;			/// The Global Horizonal Irradiance from the weather file [W/m2]
	ssc_number_t * p_weatherFileDNI;			/// The Direct Normal (Beam) Irradiance from the weather file [W/m2]
	ssc_number_t * p_weatherFileDHI;			/// The Direct Normal (Beam) Irradiance from the weather file [W/m2]
	std::vector<ssc_number_t *> p_weatherFilePOA; /// The Plane of Array Irradiance from the weather file [W/m2]
	ssc_number_t * p_sunPositionTime;			/// The hour at which the sun position is calculated [fractional hour 0-23]
	ssc_number_t * p_weatherFileWindSpeed;		/// The Wind Speed from the weather file [m/s]
	ssc_number_t * p_weatherFileAmbientTemp;	/// The ambient temperature from the weather file [C]
	ssc_number_t * p_weatherFileAlbedo;			/// The ground albedo from the weather file
	ssc_number_t * p_weatherFileSnowDepth;		/// The snow depth from the weather file
	ssc_number_t * p_IrradianceCalculated[3];	/// The calculated components of the irradiance [W/m2]
	ssc_number_t * p_sunZenithAngle;			/// The calculate sun zenith angle [degrees]
	ssc_number_t * p_sunAltitudeAngle;			/// The calculated sun altitude angle [degrees]
	ssc_number_t * p_sunAzimuthAngle;			/// The calculated sun azimuth angle [degrees]
	ssc_number_t * p_absoluteAirmass;			/// The calculated absolute airmass
	ssc_number_t * p_sunUpOverHorizon;			/// The calculation of whether the sun is up over the horizon
};

struct Simulation_IO
{
	Simulation_IO(compute_module* cm, Irradiance_IO & IrradianceIO);

	size_t numberOfYears;
	size_t numberOfWeatherFileRecords;
	size_t numberOfSteps;
	size_t stepsPerHour;
	double dtHour;
	flag useLifetimeOutput;
	flag saveLifetimeVars;
	flag annualSimulation; //flag to determine if the simulation is a normal, annual simulation with a single continuous year, or a non-annual/single timestep simulation
};

/***
*
* \struct PVSystem_IO
*
* This structure contains input and output data for the combined PV system (all subarrays)
*/
struct PVSystem_IO
{
	PVSystem_IO(compute_module* cm, std::string cmName, Simulation_IO * SimulationIO, Irradiance_IO * IrradianceIO, std::vector<Subarray_IO*> Subarrays, Inverter_IO * InverterIO);

	void AllocateOutputs(compute_module *cm);
	void AssignOutputs(compute_module *cm);
	void SetupPOAInput();

	size_t numberOfSubarrays;
	size_t numberOfInverters;

	Irradiance_IO * Irradiance;
	Simulation_IO * Simulation;
	std::vector<Subarray_IO*> Subarrays;
	Inverter_IO * Inverter;

	std::unique_ptr<SharedInverter> m_sharedInverter;

	// Inputs assumed to apply to all subarrays
	flag enableDCLifetimeLosses;
	flag enableACLifetimeLosses;
	flag enableSnowModel;

	int stringsInParallel;
	double ratedACOutput;  ///< AC Power rating for whole system (all inverters)

	flag clipMpptWindow;
	std::vector<std::vector<int> > mpptMapping;	///< vector to hold the mapping between subarrays and mppt inputs
	flag enableMismatchVoltageCalc;		///< Whether or not to compute mismatch between multiple subarrays attached to the same mppt input

	std::vector<double> dcDegradationFactor;
	std::vector<double> dcLifetimeLosses;
	std::vector<double> acLifetimeLosses;
	double acDerate;
	double acLossPercent;
	double transmissionDerate;
	double transmissionLossPercent;

	ssc_number_t transformerLoadLossFraction;
	ssc_number_t transformerNoLoadLossFraction;

	// Timeseries Subarray Level Outputs
	std::vector<ssc_number_t *> p_angleOfIncidence; /// The angle of incidence of the subarray [degrees]
	std::vector<ssc_number_t *> p_angleOfIncidenceModifier; /// The weighted angle of incidence modifier for total poa irradiation on subarrray
	std::vector<ssc_number_t *> p_surfaceTilt; ///The tilt of the surface [degrees]
	std::vector<ssc_number_t *> p_surfaceAzimuth; ///The azimuth of the surface [degrees]
	std::vector<ssc_number_t *> p_axisRotation;
	std::vector<ssc_number_t *> p_idealRotation;
	std::vector<ssc_number_t *> p_poaNominalFront;
	std::vector<ssc_number_t *> p_poaShadedFront;
	std::vector<ssc_number_t *> p_poaShadedSoiledFront;
	std::vector<ssc_number_t *> p_poaBeamFront;
	std::vector<ssc_number_t *> p_poaDiffuseFront;
	std::vector<ssc_number_t *> p_poaFront;
	std::vector<ssc_number_t *> p_poaTotal;
	std::vector<ssc_number_t *> p_poaRear;
	std::vector<ssc_number_t *> p_derateSoiling;
	std::vector<ssc_number_t *> p_beamShadingFactor;
	std::vector<ssc_number_t *> p_temperatureCell;
	std::vector<ssc_number_t *> p_temperatureCellSS; // steady state cell temperature
	std::vector<ssc_number_t *> p_moduleEfficiency;
	std::vector<ssc_number_t *> p_dcStringVoltage; /// An output vector containing dc string voltage for each subarray [V]
	std::vector<ssc_number_t *> p_voltageOpenCircuit; /// Open circuit voltage of a string in the subarray [V]
	std::vector<ssc_number_t *> p_currentShortCircuit;
	std::vector<ssc_number_t *> p_dcPowerGross;
	std::vector<ssc_number_t *> p_derateLinear;
	std::vector<ssc_number_t *> p_derateSelfShading;
	std::vector<ssc_number_t *> p_derateSelfShadingDiffuse;
	std::vector<ssc_number_t *> p_derateSelfShadingReflected;
	std::vector<ssc_number_t *> p_shadeDBShadeFraction;

	// MPPT level outputs
	std::vector<ssc_number_t *> p_mpptVoltage; /// An output vector containing input DC voltage in V to each mppt input
	std::vector<ssc_number_t *> p_dcPowerNetPerMppt; /// An output vector containing Net DC Power in W for each mppt input

	// Snow Model outputs
	std::vector<ssc_number_t *> p_snowLoss; /// The angle of incidence of the subarray [degrees]
	std::vector<ssc_number_t *> p_snowCoverage; /// The angle of incidence of the subarray [degrees]

	// Shade Database Validation
	std::vector<ssc_number_t *> p_shadeDB_GPOA; /// The angle of incidence of the subarray [degrees]
	std::vector<ssc_number_t *> p_shadeDB_DPOA; /// The angle of incidence of the subarray [degrees]
	std::vector<ssc_number_t *> p_shadeDB_temperatureCell; /// The angle of incidence of the subarray [degrees]
	std::vector<ssc_number_t *> p_shadeDB_modulesPerString; /// The angle of incidence of the subarray [degrees]
	std::vector<ssc_number_t *> p_shadeDB_voltageMaxPowerSTC; /// The angle of incidence of the subarray [degrees]
	std::vector<ssc_number_t *> p_shadeDB_voltageMPPTLow; /// The angle of incidence of the subarray [degrees]
	std::vector<ssc_number_t *> p_shadeDB_voltageMPPTHigh; /// The angle of incidence of the subarray [degrees]

	// Degradation
	ssc_number_t *p_dcDegradationFactor;

	// transformer loss outputs (single array)
	ssc_number_t *p_transformerNoLoadLoss;
	ssc_number_t *p_transformerLoadLoss;
	ssc_number_t *p_transformerLoss;

	// outputs summed across all subarrays (some could be moved to other structures)
	ssc_number_t *p_poaFrontNominalTotal;
	ssc_number_t *p_poaFrontBeamNominalTotal;
	ssc_number_t *p_poaFrontBeamTotal;
	ssc_number_t *p_poaFrontShadedTotal;
	ssc_number_t *p_poaFrontShadedSoiledTotal;
	ssc_number_t *p_poaRearTotal;
	ssc_number_t *p_poaFrontTotal;
	ssc_number_t *p_poaTotalAllSubarrays;


	ssc_number_t *p_snowLossTotal;
	ssc_number_t *p_inverterEfficiency;
	ssc_number_t *p_inverterClipLoss;
	ssc_number_t *p_inverterMPPTLoss;

	ssc_number_t *p_inverterPowerConsumptionLoss;
	ssc_number_t *p_inverterNightTimeLoss;
	ssc_number_t *p_inverterThermalLoss;
	ssc_number_t *p_inverterTotalLoss;

	ssc_number_t *p_acWiringLoss;
	ssc_number_t *p_transmissionLoss;

	ssc_number_t *p_systemDCPower;
	ssc_number_t *p_systemACPower;
};

/**
* \struct Subarray_IO
*
* This structure contains the input and output data needed for an individual subarray
* It is contained within the IOManager.
*
*/
struct Subarray_IO
{
public:

	/// Construct the Subarray_IO structure from the compute module input.
	Subarray_IO(compute_module* cm, const std::string& cmName, size_t subarrayNumber);

	/// Allocate the Subarray_IO outputs
	void AllocateOutputs(compute_module* cm);

	/// Assign outputs from member data after the PV Model has run
	void AssignOutputs(compute_module* cm);

	std::string prefix;					/// Prefix for extracting variable names

	enum self_shading {NO_SHADING, NON_LINEAR_SHADING, LINEAR_SHADING};

	// Managed by Subarray
	std::unique_ptr<Module_IO> Module; // The PV module for this subarray

	// Inputs
	flag enable;						// Whether or not the subarray is enabled

	// Electrical characteristics
	size_t nStrings;					// Number of strings in the subarray
	int nModulesPerString;				// The number of modules per string
	int mpptInput;						// Which inverter MPPT input this subarray is connected to

	// Physical characteristics
	double groundCoverageRatio;			// The ground coverage ratio [0 - 1]
	double tiltDegrees;					// The surface tilt [degrees]
	double azimuthDegrees;				// The surface azimuth [degrees]
	int trackMode;						// The tracking mode [0 = fixed, 1 = single-axis tracking, 2 = two-axis tracking, 3 = azimuth-axis tracking, 4 = seasonal-tilt
	double trackerRotationLimitDegrees; // The rotational limit of the tracker [degrees]
	flag tiltEqualLatitude;				// Set the tilt equal to the latitude
	std::vector<double> monthlyTiltDegrees; // The seasonal tilt [degrees]
	flag backtrackingEnabled;			// Backtracking enabled or not
	double moduleAspectRatio;			// The aspect ratio of the models used in the subarray
	int nStringsBottom;					// Number of strings along bottom from self-shading

	// Subarray-specific losses
	std::vector<double> monthlySoiling; // The soiling loss by month [%]
	double rearIrradianceLossPercent;
	double dcOptimizerLossPercent;
	double mismatchLossPercent;
	double diodesLossPercent;
	double dcWiringLossPercent;
	double trackingLossPercent;
	double nameplateLossPercent;
	double dcLossTotalPercent;			/// The DC loss due to mismatch, diodes, wiring, tracking, optimizers [%]

    // Shading and snow
    flag enableSelfShadingOutputs;                    // Choose whether additional self-shading outputs are displayed
    int shadeMode;                                    // The shading mode of the subarray [0 = none, 1 = standard (non-linear), 2 = thin film (linear)]
    flag usePOAFromWeatherFile;                       // Flag for whether or not a shading model has been selected that means POA can't be used directly for that subarray
    sssky_diffuse_table selfShadingSkyDiffTable;        // Calculates and stores in a lookup table the self-shading sky diffuse derates
    ssinputs selfShadingInputs;                       // Inputs and calculation methods for self-shading of the subarray
    ssoutputs selfShadingOutputs;                     // Outputs for the self-shading of the subarray
    shading_factor_calculator shadeCalculator;        // The shading calculator model for self-shading
    flag subarrayEnableSnow;                          //a copy of the enableSnowModel flag has to exist in each subarray for setting up snow model inputs specific to each subarray
    pvsnowmodel snowModel;                            // A structure to store the geometry inputs for the snow model for this subarray- even though the snow model is system wide, its effect is subarray-dependent

	/// Calculated plane-of-array (POA) irradiace for the subarray and related geometry
	struct {
		double poaBeamFront;	/// POA due to beam irradiance on the front of the subarray [W/m2]
		double poaDiffuseFront; /// POA due to diffuse irradiance on the front of the subarray [W/m2]
		double poaGroundFront;  /// POA due to ground reflection on the front of the subarray [W/m2]
		double poaRear;			/// POA total irradiance on the back of the subarray if bifacial modules [W/m2]
		double poaTotal;		/// POA total of front and rear side of array [W/m2]
		bool sunUp;				/// Flag indicating whether the sun is up or not
		double angleOfIncidenceDegrees; /// The solar angle of incidence relative to the surface [degrees]
		double surfaceTiltDegrees;  /// The tilt of the subarray after tracking [degrees]
		double surfaceAzimuthDegrees; /// The azimuth of the subarray after tracking [degrees]
		double nonlinearDCShadingDerate; /// The DC loss due to non-linear shading [%]
		bool usePOAFromWF;     /// Flag indicating whether or not to use POA input from the weatherfile
		int poaShadWarningCount; /// A counter to track warnings related to POA
		std::unique_ptr<poaDecompReq> poaAll; /// A structure containing POA decompositions into the three irrradiance components from input POA
	} poa;

	//calculated- subarray power
	double dcPowerSubarray; /// DC power for this subarray [W]

};

/**
* \struct Module_IO
*
* This structure contains the input and output data needed for an individual module
* It is contained within the IOManager.
*
*/
struct Module_IO
{
public:
	/// Construct the Module_IO structure from the compute module input.
	Module_IO(compute_module* cm, std::string cmName, double dcLoss);

	/// Setup the Nominal Operating Cell Temperature (NOCT) model
	void setupNOCTModel(compute_module* cm, const std::string &prefix);

	/// Assign outputs from member data after the PV Model has run
	void AssignOutputs(compute_module* cm);

	enum mountingSpecificConfigurationList {NONE, RACK_MOUNTING, FLUSH_MOUNTING, INTEGRATED_MOUNTING, GAP_MOUNTING};

	int modulePowerModel;						/// The PV module model selected

	double referenceArea;				/// The module area [m2]
	double moduleWattsSTC;				/// The module energy output at STC [W]
	double voltageMaxPower;				/// The voltage at max power [V]
	double selfShadingFillFactor;		/// Self shading fill factor
	flag isConcentratingPV;				/// If the sandia model is being used for CPV
	flag isBifacial;					/// If the model is bifacial
	double bifaciality;					/// The relative efficiency of the rearside to the front side
	double bifacialTransmissionFactor;  /// Factor describing how much light can travel through the module
	double groundClearanceHeight;		/// The ground clearance of a module [m]

	flag simpleEfficiencyForceNoPOA;	/// Flag to avoid calling as_integer(...) repeatedly later on
	flag mountingSpecificCellTemperatureForceNoPOA;

	spe_module_t simpleEfficiencyModel;
	cec6par_module_t cecModel;
	noct_celltemp_t nominalOperatingCellTemp;
	mcsp_celltemp_t mountingSpecificCellTemp;
	mock_celltemp_t mockCellTemp;
	sandia_module_t sandiaModel;
	sandia_celltemp_t sandiaCellTemp;
	iec61853_module_t elevenParamSingleDiodeModel;
	mlmodel_module_t mlModuleModel;
	pvcelltemp_t *cellTempModel;
	pvmodule_t *moduleModel;

	//outputs
	double dcPowerW;			/// The DC power output of one module [W]
	double dcVoltage;			/// The DC voltage of the module [V]
	double voltageOpenCircuit;  /// The DC open circuit voltage of the module [V]
	double currentShortCircuit; /// The DC short circuit current of the module [A]
	double dcEfficiency;		/// The DC conversion efficiency of the module [%]
	double temperatureCellCelcius; /// The weighted moving average  cell temperature of the module [C]
	double temperatureCellCelciusSS; /// The SS average cell temperature of the module [C]
	double angleOfIncidenceModifier; /// The angle of incidence modifier on the total poa front-side irradiance [0-1]

};


/**
* \struct Inverter_IO
*
* This structure contains the input and output data needed for a single inverter
* It is contained within the IOManager.
*
*/
struct Inverter_IO
{
public:
	/// Construct the Inverter_IO structure from the compute module input.
	Inverter_IO(compute_module* cm, std::string cmName);

	/// Setup shared inverter properties
	void setupSharedInverter(compute_module* cm, SharedInverter * a_sharedInverter);

	/// Assign outputs from member data after the PV Model has run
	void AssignOutputs(compute_module* cm);


	int inverterType;		/// From inverterTypeList
	size_t nMpptInputs;        /// Number of maximum power point tracking (MPPT) inputs on one inverter
	double mpptLowVoltage;  /// Lower limit of inverter voltage range for maximum power point tracking (MPPT) per MPPT input
	double mpptHiVoltage;   /// Upper limit of inverter voltage range for maximum power point tracking (MPPT) per MPPT input
	double ratedACOutput;   /// Rated power for one inverter

	::sandia_inverter_t sandiaInverter;
	::partload_inverter_t partloadInverter;
	::ond_inverter ondInverter;

	SharedInverter * sharedInverter;
};

#endif

