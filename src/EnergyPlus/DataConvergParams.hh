#ifndef DataConvergParams_hh_INCLUDED
#define DataConvergParams_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>
#include <DataGlobals.hh>

namespace EnergyPlus {

namespace DataConvergParams {

	// Using/Aliasing

	// Data
	// -only module should be available to other modules and routines.
	// Thus, all variables in this module must be PUBLIC.

	// MODULE PARAMETER DEFINITIONS:

	// Note: Unless otherwise noted, the tolerance parameters listed below were chosen
	// to represent educated guesses at what the tolerances for individual physical
	// parameters should be.
	extern Real64 const HVACEnthalpyToler; // Tolerance for enthalpy comparisons (in kJ/kgK)
	extern Real64 const HVACFlowRateToler; // Tolerance for mass flow rate convergence (in kg/s) [~20 CFM]
	extern Real64 const HVACFlowRateSlopeToler; // Slope tolerance for mass flow, kg/s/iteration
	extern Real64 const HVACFlowRateOscillationToler; // tolerance for detecting duplicate flow rate in stack
	extern Real64 const HVACHumRatToler; // Tolerance for humidity ratio comparisons (kg water/kg dryair)
	extern Real64 const HVACHumRatSlopeToler; // Slope tolerance for humidity ratio, kg water/kg-dryair/iteration
	extern Real64 const HVACHumRatOscillationToler; // tolerance for detecting duplicate humidity ratio in stack
	extern Real64 const HVACQualityToler; // Tolerance for fluid quality comparisons (dimensionless)
	extern Real64 const HVACPressToler; // Tolerance for pressure comparisons (in Pascals)
	extern Real64 const HVACTemperatureToler; // Tolerance for temperature comparisons (in degrees C or K)
	extern Real64 const HVACTemperatureSlopeToler; // Slope tolerance for Temperature, Deg C/iteration
	extern Real64 const HVACTemperatureOscillationToler; // tolerance for detecting duplicate temps in stack
	extern Real64 const HVACEnergyToler; // Tolerance for Energy comparisons (in Watts W)
	// to be consistent, should be 20.d0 (BG Aug 2012)

	extern Real64 const HVACCpApprox; // Air Cp (20C,0.0Kg/Kg) Only for energy Tolerance Calculation
	// Only used to scale the answer for a more intuitive answer for comparison

	extern Real64 const PlantEnthalpyToler; // Tolerance for enthalpy comparisons (in kJ/kgK)
	extern Real64 const PlantFlowRateToler; // Tolerance for mass flow rate convergence (in kg/s) [~2 CFM]
	extern Real64 const PlantLowFlowRateToler; // // Tolerance for low flow rate used for determining when
	//plant pumps can be shut down
	extern Real64 const PlantFlowRateOscillationToler;
	extern Real64 const PlantFlowRateSlopeToler; // Slope tolerance for mass flow, kg/s/iteration

	extern Real64 const PlantPressToler; // Tolerance for pressure comparisons (in Pascals)
	extern Real64 const PlantTemperatureToler; // Tolerance for temperature comparisons (in degrees C or K)
	extern Real64 const PlantTemperatureSlopeToler; // Slope tolerance for Temperature, Deg C/iteration
	extern Real64 const PlantTemperatureOscillationToler; // tolerance for detecting duplicate temps in stack

	extern Real64 const PlantEnergyToler; // Tolerance for Energy comparisons (in Watts W)

	extern Real64 const PlantCpApprox; // Approximate Cp used in Interface manager for
	// Energy Tolerance Calculation, used to scale the answer
	// for a more intuitive answer for comparison
	extern Real64 const PlantFlowFlowRateToler; // Tolerance for mass flow rate convergence (in kg/s)

	extern int const ConvergLogStackDepth;
	extern Array1D< Real64 > const ConvergLogStackARR;
	extern Real64 const sum_ConvergLogStackARR;
	extern Real64 const square_sum_ConvergLogStackARR;
	extern Real64 const sum_square_ConvergLogStackARR;

	extern int const CalledFromAirSystemDemandSide;
	extern int const CalledFromAirSystemSupplySideDeck1;
	extern int const CalledFromAirSystemSupplySideDeck2;
	// DERIVED TYPE DEFINITIONS:
	// na

	// MODULE VARIABLE DECLARATIONS:

	extern int AirLoopConvergFail;

	extern Real64 MinTimeStepSys; // =1 minute
	extern Real64 MinTimeStepTol; // = min allowable for ABS(1.-TimeStepSys/(MinTimeStepSys))
	extern Real64 MaxZoneTempDiff; // 0.3 C = (1% OF 300 C) = max allowable difference between
	//   zone air temp at Time=T and Time=T-1
	extern Real64 MinSysTimeRemaining; // = 1 second
	extern int MaxIter; // maximum number of iterations allowed

	extern int MaxPlantSubIterations; // Iteration Max for Plant Simulation sub iterations
	extern int MinPlantSubIterations; // Iteration Min for Plant Simulation sub iterations

	// Types

	struct HVACNodeConvergLogStruct
	{
		// Members
		int NodeNum;
		bool NotConvergedHumRate;
		bool NotConvergedMassFlow;
		bool NotConvergedTemp;
		Array1D< Real64 > HumidityRatio;
		Array1D< Real64 > MassFlowRate;
		Array1D< Real64 > Temperature;

		// Default Constructor
		HVACNodeConvergLogStruct() :
			HumidityRatio( ConvergLogStackDepth ),
			MassFlowRate( ConvergLogStackDepth ),
			Temperature( ConvergLogStackDepth )
		{}

		// Member Constructor
		HVACNodeConvergLogStruct(
			int const NodeNum,
			bool const NotConvergedHumRate,
			bool const NotConvergedMassFlow,
			bool const NotConvergedTemp,
			Array1< Real64 > const & HumidityRatio,
			Array1< Real64 > const & MassFlowRate,
			Array1< Real64 > const & Temperature
		) :
			NodeNum( NodeNum ),
			NotConvergedHumRate( NotConvergedHumRate ),
			NotConvergedMassFlow( NotConvergedMassFlow ),
			NotConvergedTemp( NotConvergedTemp ),
			HumidityRatio( ConvergLogStackDepth, HumidityRatio ),
			MassFlowRate( ConvergLogStackDepth, MassFlowRate ),
			Temperature( ConvergLogStackDepth, Temperature )
		{}

	};

	struct HVACZoneInletConvergenceStruct
	{
		// Members
		std::string ZoneName;
		int NumInletNodes; // number of inlet nodes for zone
		Array1D< HVACNodeConvergLogStruct > InletNode;

		// Default Constructor
		HVACZoneInletConvergenceStruct() :
			NumInletNodes( 0 )
		{}

		// Member Constructor
		HVACZoneInletConvergenceStruct(
			std::string const & ZoneName,
			int const NumInletNodes, // number of inlet nodes for zone
			Array1< HVACNodeConvergLogStruct > const & InletNode
		) :
			ZoneName( ZoneName ),
			NumInletNodes( NumInletNodes ),
			InletNode( InletNode )
		{}

	};

	struct HVACAirLoopIterationConvergenceStruct
	{
		// Members
		Array1D_bool HVACMassFlowNotConverged; // Flag to show mass flow convergence
		Array1D< Real64 > HVACFlowDemandToSupplyTolValue; // Queue of convergence "results"
		Array1D< Real64 > HVACFlowSupplyDeck1ToDemandTolValue; // Queue of convergence "results"
		Array1D< Real64 > HVACFlowSupplyDeck2ToDemandTolValue; // Queue of convergence "results"
		Array1D_bool HVACHumRatNotConverged; // Flag to show humidity ratio convergence   or failure
		Array1D< Real64 > HVACHumDemandToSupplyTolValue; // Queue of convergence "results"
		Array1D< Real64 > HVACHumSupplyDeck1ToDemandTolValue; // Queue of convergence "results"
		Array1D< Real64 > HVACHumSupplyDeck2ToDemandTolValue; // Queue of convergence "results"
		Array1D_bool HVACTempNotConverged; // Flag to show temperature convergence  or failure
		Array1D< Real64 > HVACTempDemandToSupplyTolValue; // Queue of convergence "results"
		Array1D< Real64 > HVACTempSupplyDeck1ToDemandTolValue; // Queue of convergence "results"
		Array1D< Real64 > HVACTempSupplyDeck2ToDemandTolValue; // Queue of convergence "results"
		Array1D_bool HVACEnergyNotConverged; // Flag to show energy convergence   or failure
		Array1D< Real64 > HVACEnergyDemandToSupplyTolValue; // Queue of convergence "results"
		Array1D< Real64 > HVACEnergySupplyDeck1ToDemandTolValue; // Queue of convergence "results"
		Array1D< Real64 > HVACEnergySupplyDeck2ToDemandTolValue; // Queue of convergence "results"
		Array1D_bool HVACEnthalpyNotConverged; // Flag to show energy convergence   or failure
		Array1D< Real64 > HVACEnthalpyDemandToSupplyTolValue; // Queue of convergence "results"
		Array1D< Real64 > HVACEnthalpySupplyDeck1ToDemandTolValue; // Queue of convergence "results"
		Array1D< Real64 > HVACEnthalpySupplyDeck2ToDemandTolValue; // Queue of convergence "results"
		Array1D_bool HVACPressureNotConverged; // Flag to show energy convergence   or failure
		Array1D< Real64 > HVACPressureDemandToSupplyTolValue; // Queue of convergence "results"
		Array1D< Real64 > HVACPressureSupplyDeck1ToDemandTolValue; // Queue of convergence "results"
		Array1D< Real64 > HVACPressueSupplyDeck2ToDemandTolValue; // Queue of convergence "results"
		Array1D_bool HVACQualityNotConverged; // Flag to show energy convergence   or failure
		Array1D< Real64 > HVACQualityDemandToSupplyTolValue; // Queue of convergence "results"
		Array1D< Real64 > HVACQualitSupplyDeck1ToDemandTolValue; // Queue of convergence "results"
		Array1D< Real64 > HVACQualitySupplyDeck2ToDemandTolValue; // Queue of convergence "results"

		// Default Constructor
		HVACAirLoopIterationConvergenceStruct() :
			HVACMassFlowNotConverged( 3, false ),
			HVACFlowDemandToSupplyTolValue( ConvergLogStackDepth, 0.0 ),
			HVACFlowSupplyDeck1ToDemandTolValue( ConvergLogStackDepth, 0.0 ),
			HVACFlowSupplyDeck2ToDemandTolValue( ConvergLogStackDepth, 0.0 ),
			HVACHumRatNotConverged( 3, false ),
			HVACHumDemandToSupplyTolValue( ConvergLogStackDepth, 0.0 ),
			HVACHumSupplyDeck1ToDemandTolValue( ConvergLogStackDepth, 0.0 ),
			HVACHumSupplyDeck2ToDemandTolValue( ConvergLogStackDepth, 0.0 ),
			HVACTempNotConverged( 3, false ),
			HVACTempDemandToSupplyTolValue( ConvergLogStackDepth, 0.0 ),
			HVACTempSupplyDeck1ToDemandTolValue( ConvergLogStackDepth, 0.0 ),
			HVACTempSupplyDeck2ToDemandTolValue( ConvergLogStackDepth, 0.0 ),
			HVACEnergyNotConverged( 3, false ),
			HVACEnergyDemandToSupplyTolValue( ConvergLogStackDepth, 0.0 ),
			HVACEnergySupplyDeck1ToDemandTolValue( ConvergLogStackDepth, 0.0 ),
			HVACEnergySupplyDeck2ToDemandTolValue( ConvergLogStackDepth, 0.0 ),
			HVACEnthalpyNotConverged( 3, false ),
			HVACEnthalpyDemandToSupplyTolValue( ConvergLogStackDepth, 0.0 ),
			HVACEnthalpySupplyDeck1ToDemandTolValue( ConvergLogStackDepth, 0.0 ),
			HVACEnthalpySupplyDeck2ToDemandTolValue( ConvergLogStackDepth, 0.0 ),
			HVACPressureNotConverged( 3, false ),
			HVACPressureDemandToSupplyTolValue( ConvergLogStackDepth, 0.0 ),
			HVACPressureSupplyDeck1ToDemandTolValue( ConvergLogStackDepth, 0.0 ),
			HVACPressueSupplyDeck2ToDemandTolValue( ConvergLogStackDepth, 0.0 ),
			HVACQualityNotConverged( 3, false ),
			HVACQualityDemandToSupplyTolValue( ConvergLogStackDepth, 0.0 ),
			HVACQualitSupplyDeck1ToDemandTolValue( ConvergLogStackDepth, 0.0 ),
			HVACQualitySupplyDeck2ToDemandTolValue( ConvergLogStackDepth, 0.0 )
		{}

		// Member Constructor
		HVACAirLoopIterationConvergenceStruct(
			Array1D_bool const HVACMassFlowNotConverged, // Flag to show mass flow convergence
			Array1< Real64 > const & HVACFlowDemandToSupplyTolValue, // Queue of convergence "results"
			Array1< Real64 > const & HVACFlowSupplyDeck1ToDemandTolValue, // Queue of convergence "results"
			Array1< Real64 > const & HVACFlowSupplyDeck2ToDemandTolValue, // Queue of convergence "results"
			Array1D_bool const HVACHumRatNotConverged, // Flag to show humidity ratio convergence   or failure
			Array1< Real64 > const & HVACHumDemandToSupplyTolValue, // Queue of convergence "results"
			Array1< Real64 > const & HVACHumSupplyDeck1ToDemandTolValue, // Queue of convergence "results"
			Array1< Real64 > const & HVACHumSupplyDeck2ToDemandTolValue, // Queue of convergence "results"
			Array1D_bool const HVACTempNotConverged, // Flag to show temperature convergence  or failure
			Array1< Real64 > const & HVACTempDemandToSupplyTolValue, // Queue of convergence "results"
			Array1< Real64 > const & HVACTempSupplyDeck1ToDemandTolValue, // Queue of convergence "results"
			Array1< Real64 > const & HVACTempSupplyDeck2ToDemandTolValue, // Queue of convergence "results"
			Array1D_bool const HVACEnergyNotConverged, // Flag to show energy convergence   or failure
			Array1< Real64 > const & HVACEnergyDemandToSupplyTolValue, // Queue of convergence "results"
			Array1< Real64 > const & HVACEnergySupplyDeck1ToDemandTolValue, // Queue of convergence "results"
			Array1< Real64 > const & HVACEnergySupplyDeck2ToDemandTolValue, // Queue of convergence "results"
			Array1D_bool const HVACEnthalpyNotConverged, // Flag to show energy convergence   or failure
			Array1< Real64 > const & HVACEnthalpyDemandToSupplyTolValue, // Queue of convergence "results"
			Array1< Real64 > const & HVACEnthalpySupplyDeck1ToDemandTolValue, // Queue of convergence "results"
			Array1< Real64 > const & HVACEnthalpySupplyDeck2ToDemandTolValue, // Queue of convergence "results"
			Array1D_bool const HVACPressureNotConverged, // Flag to show energy convergence   or failure
			Array1< Real64 > const & HVACPressureDemandToSupplyTolValue, // Queue of convergence "results"
			Array1< Real64 > const & HVACPressureSupplyDeck1ToDemandTolValue, // Queue of convergence "results"
			Array1< Real64 > const & HVACPressueSupplyDeck2ToDemandTolValue, // Queue of convergence "results"
			Array1D_bool const HVACQualityNotConverged, // Flag to show energy convergence   or failure
			Array1< Real64 > const & HVACQualityDemandToSupplyTolValue, // Queue of convergence "results"
			Array1< Real64 > const & HVACQualitSupplyDeck1ToDemandTolValue, // Queue of convergence "results"
			Array1< Real64 > const & HVACQualitySupplyDeck2ToDemandTolValue // Queue of convergence "results"
		) :
			HVACMassFlowNotConverged( 3, HVACMassFlowNotConverged ),
			HVACFlowDemandToSupplyTolValue( ConvergLogStackDepth, HVACFlowDemandToSupplyTolValue ),
			HVACFlowSupplyDeck1ToDemandTolValue( ConvergLogStackDepth, HVACFlowSupplyDeck1ToDemandTolValue ),
			HVACFlowSupplyDeck2ToDemandTolValue( ConvergLogStackDepth, HVACFlowSupplyDeck2ToDemandTolValue ),
			HVACHumRatNotConverged( 3, HVACHumRatNotConverged ),
			HVACHumDemandToSupplyTolValue( ConvergLogStackDepth, HVACHumDemandToSupplyTolValue ),
			HVACHumSupplyDeck1ToDemandTolValue( ConvergLogStackDepth, HVACHumSupplyDeck1ToDemandTolValue ),
			HVACHumSupplyDeck2ToDemandTolValue( ConvergLogStackDepth, HVACHumSupplyDeck2ToDemandTolValue ),
			HVACTempNotConverged( 3, HVACTempNotConverged ),
			HVACTempDemandToSupplyTolValue( ConvergLogStackDepth, HVACTempDemandToSupplyTolValue ),
			HVACTempSupplyDeck1ToDemandTolValue( ConvergLogStackDepth, HVACTempSupplyDeck1ToDemandTolValue ),
			HVACTempSupplyDeck2ToDemandTolValue( ConvergLogStackDepth, HVACTempSupplyDeck2ToDemandTolValue ),
			HVACEnergyNotConverged( 3, HVACEnergyNotConverged ),
			HVACEnergyDemandToSupplyTolValue( ConvergLogStackDepth, HVACEnergyDemandToSupplyTolValue ),
			HVACEnergySupplyDeck1ToDemandTolValue( ConvergLogStackDepth, HVACEnergySupplyDeck1ToDemandTolValue ),
			HVACEnergySupplyDeck2ToDemandTolValue( ConvergLogStackDepth, HVACEnergySupplyDeck2ToDemandTolValue ),
			HVACEnthalpyNotConverged( 3, HVACEnthalpyNotConverged ),
			HVACEnthalpyDemandToSupplyTolValue( ConvergLogStackDepth, HVACEnthalpyDemandToSupplyTolValue ),
			HVACEnthalpySupplyDeck1ToDemandTolValue( ConvergLogStackDepth, HVACEnthalpySupplyDeck1ToDemandTolValue ),
			HVACEnthalpySupplyDeck2ToDemandTolValue( ConvergLogStackDepth, HVACEnthalpySupplyDeck2ToDemandTolValue ),
			HVACPressureNotConverged( 3, HVACPressureNotConverged ),
			HVACPressureDemandToSupplyTolValue( ConvergLogStackDepth, HVACPressureDemandToSupplyTolValue ),
			HVACPressureSupplyDeck1ToDemandTolValue( ConvergLogStackDepth, HVACPressureSupplyDeck1ToDemandTolValue ),
			HVACPressueSupplyDeck2ToDemandTolValue( ConvergLogStackDepth, HVACPressueSupplyDeck2ToDemandTolValue ),
			HVACQualityNotConverged( 3, HVACQualityNotConverged ),
			HVACQualityDemandToSupplyTolValue( ConvergLogStackDepth, HVACQualityDemandToSupplyTolValue ),
			HVACQualitSupplyDeck1ToDemandTolValue( ConvergLogStackDepth, HVACQualitSupplyDeck1ToDemandTolValue ),
			HVACQualitySupplyDeck2ToDemandTolValue( ConvergLogStackDepth, HVACQualitySupplyDeck2ToDemandTolValue )
		{}

	};

	struct PlantIterationConvergenceStruct
	{
		// Members
		bool PlantMassFlowNotConverged; // Flag to show mass flow convergence
		Array1D< Real64 > PlantFlowDemandToSupplyTolValue; // Queue of convergence "results"
		Array1D< Real64 > PlantFlowSupplyToDemandTolValue; // Queue of convergence "results"
		bool PlantTempNotConverged; // Flag to show temperature convergence (0) or failure (1)
		Array1D< Real64 > PlantTempDemandToSupplyTolValue; // Queue of convergence "results"
		Array1D< Real64 > PlantTempSupplyToDemandTolValue; // Queue of convergence "results"

		// Default Constructor
		PlantIterationConvergenceStruct() :
			PlantMassFlowNotConverged( false ),
			PlantFlowDemandToSupplyTolValue( ConvergLogStackDepth, 0.0 ),
			PlantFlowSupplyToDemandTolValue( ConvergLogStackDepth, 0.0 ),
			PlantTempNotConverged( false ),
			PlantTempDemandToSupplyTolValue( ConvergLogStackDepth, 0.0 ),
			PlantTempSupplyToDemandTolValue( ConvergLogStackDepth, 0.0 )
		{}

		// Member Constructor
		PlantIterationConvergenceStruct(
			bool const PlantMassFlowNotConverged, // Flag to show mass flow convergence
			Array1< Real64 > const & PlantFlowDemandToSupplyTolValue, // Queue of convergence "results"
			Array1< Real64 > const & PlantFlowSupplyToDemandTolValue, // Queue of convergence "results"
			bool const PlantTempNotConverged, // Flag to show temperature convergence (0) or failure (1)
			Array1< Real64 > const & PlantTempDemandToSupplyTolValue, // Queue of convergence "results"
			Array1< Real64 > const & PlantTempSupplyToDemandTolValue // Queue of convergence "results"
		) :
			PlantMassFlowNotConverged( PlantMassFlowNotConverged ),
			PlantFlowDemandToSupplyTolValue( ConvergLogStackDepth, PlantFlowDemandToSupplyTolValue ),
			PlantFlowSupplyToDemandTolValue( ConvergLogStackDepth, PlantFlowSupplyToDemandTolValue ),
			PlantTempNotConverged( PlantTempNotConverged ),
			PlantTempDemandToSupplyTolValue( ConvergLogStackDepth, PlantTempDemandToSupplyTolValue ),
			PlantTempSupplyToDemandTolValue( ConvergLogStackDepth, PlantTempSupplyToDemandTolValue )
		{}

	};

	// Object Data
	extern Array1D< HVACZoneInletConvergenceStruct > ZoneInletConvergence;
	extern Array1D< HVACAirLoopIterationConvergenceStruct > AirLoopConvergence;
	extern Array1D< PlantIterationConvergenceStruct > PlantConvergence;

} // DataConvergParams

} // EnergyPlus

#endif
