#ifndef MicroturbineElectricGenerator_hh_INCLUDED
#define MicroturbineElectricGenerator_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>
#include <DataGlobalConstants.hh>
#include <DataGlobals.hh>

namespace EnergyPlus {

namespace MicroturbineElectricGenerator {

	// Using/Aliasing
	using DataGlobalConstants::iGeneratorMicroturbine;

	// Data
	// MODULE PARAMETER DEFINITIONS:
	// na

	// DERIVED TYPE DEFINITIONS:

	// MODULE VARIABLE DECLARATIONS:
	extern int NumMTGenerators; // number of MT Generators specified in input
	extern bool GetMTInput; // then TRUE, calls subroutine to read input file.

	extern Array1D_bool CheckEquipName;

	// SUBROUTINE SPECIFICATIONS FOR MODULE MicroturbineElectricGenerator

	// Types

	struct MTGeneratorSpecs
	{
		// Members
		//      User inputs
		std::string Name; // User identifier (name)
		Real64 RefElecPowerOutput; // Reference Electrical Power Output from generator (W)
		Real64 MinElecPowerOutput; // Minimum Electrical Power Output (W)
		Real64 MaxElecPowerOutput; // Maximum Electrical Power Output (W)
		Real64 RefThermalPowerOutput; // Reference Electrical Power Output from generator (W)
		Real64 MinThermalPowerOutput; // Minimum Electrical Power Output (W)
		Real64 MaxThermalPowerOutput; // Maximum Electrical Power Output (W)
		Real64 RefElecEfficiencyLHV; // Reference Electrical Efficiency based on fuel LHV
		Real64 RefCombustAirInletTemp; // Reference Combustion Air Inlet Temperature (C)
		Real64 RefCombustAirInletHumRat; // Reference Combustion Air Inlet Humidity Ratio (kg/kg)
		Real64 RefElevation; // Reference Elevation (m)
		int ElecPowFTempElevCurveNum; // Curve index for Electrical Power as a function of temp and elev.
		int ElecEffFTempCurveNum; // Curve index for Electrical Efficiency function of temp
		int ElecEffFPLRCurveNum; // Curve index for Electrical Efficiency as a function of PLR
		Real64 FuelHigherHeatingValue; // Higher Heating Value for Fuel (kJ/kg)
		Real64 FuelLowerHeatingValue; // Lower Heating Value for Fuel (kJ/kg)
		Real64 StandbyPower; // Standby Power entered by user (W)
		Real64 AncillaryPower; // Ancillary Power entered by user (W)
		int AncillaryPowerFuelCurveNum; // Index to ancillary power modifer curve (function of fuel input)
		int HeatRecInletNodeNum; // Heat Recovery Water Inlet Node number
		int HeatRecOutletNodeNum; // Heat Recovery Water Outlet Node number
		Real64 RefThermalEffLHV; // Reference Thermal Efficiency (LHV Basis)
		Real64 RefInletWaterTemp; // Reference Inlet Water Temperature for heat recovery (C)
		bool InternalFlowControl; // A9, \field Heat Recovery Water Flow Operating Mode
		bool PlantFlowControl; // Default = Plant Control
		Real64 RefHeatRecVolFlowRate; // Reference Heat Recovery Water Flow Rate (m3/s)
		int HeatRecFlowFTempPowCurveNum; // Curve index for Heat Recovery Water Flow Rate function of temp & power
		int ThermEffFTempElevCurveNum; // Curve index for Thermal Efficiency function of temp & elevation
		int HeatRecRateFPLRCurveNum; // Curve index for Heat Recovery Rate function of part-load ratio
		int HeatRecRateFTempCurveNum; // Curve index for Heat Recovery Rate function of inlet water temp
		int HeatRecRateFWaterFlowCurveNum; // Curve index for Heat Recovery Rate function of water flow rate
		Real64 HeatRecMinVolFlowRate; // Minimum Heat Recovery Water volume Flow Rate (m3/s)
		Real64 HeatRecMaxVolFlowRate; // Maximum Heat Recovery Water volume Flow Rate (m3/s)
		Real64 HeatRecMaxWaterTemp; // Maximum Heat Recovery Water Temperature (C)
		int CombustionAirInletNodeNum; // Combustion Air Inlet Node number
		int CombustionAirOutletNodeNum; // Combustion Air Outlet (Exhaust) Node number
		bool ExhAirCalcsActive; // Flag to enable exhaust air calculations
		Real64 RefExhaustAirMassFlowRate; // Reference Exhaust Air Mass Flow Rate (kg/s)
		Real64 ExhaustAirMassFlowRate; // Actual Exhaust Air Mass Flow Rate (kg/s)
		int ExhFlowFTempCurveNum; // Curve index for Exhaust Air Flow Rate function of inlet air temp
		int ExhFlowFPLRCurveNum; // Curve index for Exhaust Air Flow Rate function of part-load ratio
		Real64 NomExhAirOutletTemp; // Nominal Exhaust Air Outlet Temperature (C)
		int ExhAirTempFTempCurveNum; // Curve index for Exhaust Air Temperature function of inlet air temp
		int ExhAirTempFPLRCurveNum; // Curve index for Exhaust Air Temperature function of part-load ratio
		Real64 ExhaustAirTemperature; // Combustion exhaust air temperature (C)
		Real64 ExhaustAirHumRat; // Combustion exhaust air humidity ratio (kg/kg)
		//      Other required variables/calculated values
		int CompType_Num;
		Real64 RefCombustAirInletDensity; // Reference combustion air inlet density (kg/m3)
		Real64 MinPartLoadRat; // Min allowed operating frac full load
		Real64 MaxPartLoadRat; // Max allowed operating frac full load
		Real64 FuelEnergyUseRateHHV; // Rate of Fuel Energy required to run microturbine, HHV basis (W)
		Real64 FuelEnergyUseRateLHV; // Rate of Fuel Energy required to run microturbine, LHV basis (W)
		Real64 QHeatRecovered; // Recovered exhaust energy rate to heat water  (W)
		Real64 ExhaustEnergyRec; // Recovered exhaust energy to heat water (J)
		Real64 DesignHeatRecMassFlowRate; // Design Water mass flow rate through heat recovery loop (kg/s)
		bool HeatRecActive; // TRUE when heat recovery water inlet and outlet nodes are defined
		Real64 HeatRecInletTemp; // Inlet Temperature of the heat recovery fluid (C)
		Real64 HeatRecOutletTemp; // Outlet Temperature of the heat recovery fluid (C)
		Real64 HeatRecMinMassFlowRate; // Minimum heat recovery water mass flow rate (kg/s)
		Real64 HeatRecMaxMassFlowRate; // Maximum heat recovery water mass flow rate (kg/s)
		Real64 HeatRecMdot; // Heat Recovery Loop Mass flow rate (kg/s)
		int HRLoopNum; // cooling water plant loop index number, for heat recovery
		int HRLoopSideNum; // cooling water plant loop side index, for heat recovery
		int HRBranchNum; // cooling water plant loop branch index, for heat recovery
		int HRCompNum; // cooling water plant loop component index, for heat recovery
		Real64 FuelMdot; // Fuel Amount used (kg/s)
		Real64 ElecPowerGenerated; // Electric power generated (W)
		Real64 StandbyPowerRate; // Standby power rate this time step (W)
		Real64 AncillaryPowerRate; // Ancillary power rate this time step (W)
		//     Warning message variables
		int PowerFTempElevErrorIndex; // Index to power as a function of temp/elevation warning message
		//       INTEGER    :: PowerFTempElevErrorCount     = 0   ! Counter for power as a function of temp/elevation warning messages
		int EffFTempErrorIndex; // Index to efficiency as a function of temperature warning message
		//       INTEGER    :: EffFTempErrorCount           = 0   ! Counter for efficiency as a function of temperature warning messages
		int EffFPLRErrorIndex; // Index to efficiency as a function of PLR warning message
		//       INTEGER    :: EffFPLRErrorCount            = 0   ! Counter for efficiency as a function of PLR warning messages
		int ExhFlowFTempErrorIndex; // Index to exhaust flow as a function of temp warning message
		//       INTEGER    :: ExhFlowFTempErrorCount       = 0   ! Counter for exhaust flow as a function of temp warning messages
		int ExhFlowFPLRErrorIndex; // Index to exhaust flow as a function of PLR warning message
		//       INTEGER    :: ExhFlowFPLRErrorCount        = 0   ! Counter for exhaust flow as a function of PLR warning messages
		int ExhTempFTempErrorIndex; // Index to exhaust temp as a function of temp warning message
		//       INTEGER    :: ExhTempFTempErrorCount       = 0   ! Counter for exhaust temp as a function of temp warning messages
		int ExhTempFPLRErrorIndex; // Index to exhaust temp as a function of PLR warning message
		//       INTEGER    :: ExhTempFPLRErrorCount        = 0   ! Counter for exhaust temp as a function of PLR warning messages
		int HRMinFlowErrorIndex; // Index to reclaim water flow rate warning message
		//       INTEGER    :: HRMinFlowErrorCount          = 0   ! Counter for reclaim water flow rate warning messages
		int HRMaxFlowErrorIndex; // Index to reclaim water flow rate warning message
		//       INTEGER    :: HRMaxFlowErrorCount          = 0   ! Counter for reclaim water flow rate warning messages
		int ExhTempLTInletTempIndex; // Index to exhaust temp < combustion inlet air temp warning messages
		//       INTEGER    :: ExhTempLTInletTempCount      = 0   ! Counter for exhaust temp < combustion inlet air temp warning messages
		int ExhHRLTInletHRIndex; // Index to exhaust hum rat < combustion inlet air hum rat warning messages
		//       INTEGER    :: ExhHRLTInletHRCount          = 0   ! Counter for exhaust hum rat < combustion inlet air hum rat warn messages
		int AnciPowerIterErrorIndex; // Index to Ancillary Power iteration loop warning messages
		//       INTEGER    :: AnciPowerIterErrorCount      = 0   ! Count for Ancillary Power iteration loop warning messages
		int AnciPowerFMdotFuelErrorIndex; // Index to Ancillary Power as a function of fuel input warning messages
		//       INTEGER    :: AnciPowerFMdotFuelErrorCount = 0   ! Count for Ancillary Power as a function of fuel input warning messages
		int HeatRecRateFPLRErrorIndex; // Index to heat recovery rate as a function of PLR warning messages
		//       INTEGER    :: HeatRecRateFPLRErrorCount    = 0   ! Count for heat recovery rate as a function of PLR warning messages
		int HeatRecRateFTempErrorIndex; // Index to heat recovery rate as a function of temp warning messages
		//       INTEGER    :: HeatRecRateFTempErrorCount   = 0   ! Count for heat recovery rate as a function of temp warning messages
		int HeatRecRateFFlowErrorIndex; // Index to heat recovery rate as a function of flow warning messages
		//       INTEGER    :: HeatRecRateFFlowErrorCount   = 0   ! Count for heat recovery rate as a function of flow warning messages
		int ThermEffFTempElevErrorIndex; // Index to thermal efficiency as a function of temp/elevation warnings
		//       INTEGER    :: ThermEffFTempElevErrorCount  = 0   ! Count for thermal efficiency as a function of temp/elevation warnings

		// Default Constructor
		MTGeneratorSpecs() :
			RefElecPowerOutput( 0.0 ),
			MinElecPowerOutput( 0.0 ),
			MaxElecPowerOutput( 0.0 ),
			RefThermalPowerOutput( 0.0 ),
			MinThermalPowerOutput( 0.0 ),
			MaxThermalPowerOutput( 0.0 ),
			RefElecEfficiencyLHV( 0.0 ),
			RefCombustAirInletTemp( 0.0 ),
			RefCombustAirInletHumRat( 0.0 ),
			RefElevation( 0.0 ),
			ElecPowFTempElevCurveNum( 0 ),
			ElecEffFTempCurveNum( 0 ),
			ElecEffFPLRCurveNum( 0 ),
			FuelHigherHeatingValue( 0.0 ),
			FuelLowerHeatingValue( 0.0 ),
			StandbyPower( 0.0 ),
			AncillaryPower( 0.0 ),
			AncillaryPowerFuelCurveNum( 0 ),
			HeatRecInletNodeNum( 0 ),
			HeatRecOutletNodeNum( 0 ),
			RefThermalEffLHV( 0.0 ),
			RefInletWaterTemp( 0.0 ),
			InternalFlowControl( false ),
			PlantFlowControl( true ),
			RefHeatRecVolFlowRate( 0.0 ),
			HeatRecFlowFTempPowCurveNum( 0 ),
			ThermEffFTempElevCurveNum( 0 ),
			HeatRecRateFPLRCurveNum( 0 ),
			HeatRecRateFTempCurveNum( 0 ),
			HeatRecRateFWaterFlowCurveNum( 0 ),
			HeatRecMinVolFlowRate( 0.0 ),
			HeatRecMaxVolFlowRate( 0.0 ),
			HeatRecMaxWaterTemp( 0.0 ),
			CombustionAirInletNodeNum( 0 ),
			CombustionAirOutletNodeNum( 0 ),
			ExhAirCalcsActive( false ),
			RefExhaustAirMassFlowRate( 0.0 ),
			ExhaustAirMassFlowRate( 0.0 ),
			ExhFlowFTempCurveNum( 0 ),
			ExhFlowFPLRCurveNum( 0 ),
			NomExhAirOutletTemp( 0.0 ),
			ExhAirTempFTempCurveNum( 0 ),
			ExhAirTempFPLRCurveNum( 0 ),
			ExhaustAirTemperature( 0.0 ),
			ExhaustAirHumRat( 0.0 ),
			CompType_Num( iGeneratorMicroturbine ),
			RefCombustAirInletDensity( 0.0 ),
			MinPartLoadRat( 0.0 ),
			MaxPartLoadRat( 0.0 ),
			FuelEnergyUseRateHHV( 0.0 ),
			FuelEnergyUseRateLHV( 0.0 ),
			QHeatRecovered( 0.0 ),
			ExhaustEnergyRec( 0.0 ),
			DesignHeatRecMassFlowRate( 0.0 ),
			HeatRecActive( false ),
			HeatRecInletTemp( 0.0 ),
			HeatRecOutletTemp( 0.0 ),
			HeatRecMinMassFlowRate( 0.0 ),
			HeatRecMaxMassFlowRate( 0.0 ),
			HeatRecMdot( 0.0 ),
			HRLoopNum( 0 ),
			HRLoopSideNum( 0 ),
			HRBranchNum( 0 ),
			HRCompNum( 0 ),
			FuelMdot( 0.0 ),
			ElecPowerGenerated( 0.0 ),
			StandbyPowerRate( 0.0 ),
			AncillaryPowerRate( 0.0 ),
			PowerFTempElevErrorIndex( 0 ),
			EffFTempErrorIndex( 0 ),
			EffFPLRErrorIndex( 0 ),
			ExhFlowFTempErrorIndex( 0 ),
			ExhFlowFPLRErrorIndex( 0 ),
			ExhTempFTempErrorIndex( 0 ),
			ExhTempFPLRErrorIndex( 0 ),
			HRMinFlowErrorIndex( 0 ),
			HRMaxFlowErrorIndex( 0 ),
			ExhTempLTInletTempIndex( 0 ),
			ExhHRLTInletHRIndex( 0 ),
			AnciPowerIterErrorIndex( 0 ),
			AnciPowerFMdotFuelErrorIndex( 0 ),
			HeatRecRateFPLRErrorIndex( 0 ),
			HeatRecRateFTempErrorIndex( 0 ),
			HeatRecRateFFlowErrorIndex( 0 ),
			ThermEffFTempElevErrorIndex( 0 )
		{}

		// Member Constructor
		MTGeneratorSpecs(
			std::string const & Name, // User identifier (name)
			Real64 const RefElecPowerOutput, // Reference Electrical Power Output from generator (W)
			Real64 const MinElecPowerOutput, // Minimum Electrical Power Output (W)
			Real64 const MaxElecPowerOutput, // Maximum Electrical Power Output (W)
			Real64 const RefThermalPowerOutput, // Reference Electrical Power Output from generator (W)
			Real64 const MinThermalPowerOutput, // Minimum Electrical Power Output (W)
			Real64 const MaxThermalPowerOutput, // Maximum Electrical Power Output (W)
			Real64 const RefElecEfficiencyLHV, // Reference Electrical Efficiency based on fuel LHV
			Real64 const RefCombustAirInletTemp, // Reference Combustion Air Inlet Temperature (C)
			Real64 const RefCombustAirInletHumRat, // Reference Combustion Air Inlet Humidity Ratio (kg/kg)
			Real64 const RefElevation, // Reference Elevation (m)
			int const ElecPowFTempElevCurveNum, // Curve index for Electrical Power as a function of temp and elev.
			int const ElecEffFTempCurveNum, // Curve index for Electrical Efficiency function of temp
			int const ElecEffFPLRCurveNum, // Curve index for Electrical Efficiency as a function of PLR
			Real64 const FuelHigherHeatingValue, // Higher Heating Value for Fuel (kJ/kg)
			Real64 const FuelLowerHeatingValue, // Lower Heating Value for Fuel (kJ/kg)
			Real64 const StandbyPower, // Standby Power entered by user (W)
			Real64 const AncillaryPower, // Ancillary Power entered by user (W)
			int const AncillaryPowerFuelCurveNum, // Index to ancillary power modifer curve (function of fuel input)
			int const HeatRecInletNodeNum, // Heat Recovery Water Inlet Node number
			int const HeatRecOutletNodeNum, // Heat Recovery Water Outlet Node number
			Real64 const RefThermalEffLHV, // Reference Thermal Efficiency (LHV Basis)
			Real64 const RefInletWaterTemp, // Reference Inlet Water Temperature for heat recovery (C)
			bool const InternalFlowControl, // A9, \field Heat Recovery Water Flow Operating Mode
			bool const PlantFlowControl, // Default = Plant Control
			Real64 const RefHeatRecVolFlowRate, // Reference Heat Recovery Water Flow Rate (m3/s)
			int const HeatRecFlowFTempPowCurveNum, // Curve index for Heat Recovery Water Flow Rate function of temp & power
			int const ThermEffFTempElevCurveNum, // Curve index for Thermal Efficiency function of temp & elevation
			int const HeatRecRateFPLRCurveNum, // Curve index for Heat Recovery Rate function of part-load ratio
			int const HeatRecRateFTempCurveNum, // Curve index for Heat Recovery Rate function of inlet water temp
			int const HeatRecRateFWaterFlowCurveNum, // Curve index for Heat Recovery Rate function of water flow rate
			Real64 const HeatRecMinVolFlowRate, // Minimum Heat Recovery Water volume Flow Rate (m3/s)
			Real64 const HeatRecMaxVolFlowRate, // Maximum Heat Recovery Water volume Flow Rate (m3/s)
			Real64 const HeatRecMaxWaterTemp, // Maximum Heat Recovery Water Temperature (C)
			int const CombustionAirInletNodeNum, // Combustion Air Inlet Node number
			int const CombustionAirOutletNodeNum, // Combustion Air Outlet (Exhaust) Node number
			bool const ExhAirCalcsActive, // Flag to enable exhaust air calculations
			Real64 const RefExhaustAirMassFlowRate, // Reference Exhaust Air Mass Flow Rate (kg/s)
			Real64 const ExhaustAirMassFlowRate, // Actual Exhaust Air Mass Flow Rate (kg/s)
			int const ExhFlowFTempCurveNum, // Curve index for Exhaust Air Flow Rate function of inlet air temp
			int const ExhFlowFPLRCurveNum, // Curve index for Exhaust Air Flow Rate function of part-load ratio
			Real64 const NomExhAirOutletTemp, // Nominal Exhaust Air Outlet Temperature (C)
			int const ExhAirTempFTempCurveNum, // Curve index for Exhaust Air Temperature function of inlet air temp
			int const ExhAirTempFPLRCurveNum, // Curve index for Exhaust Air Temperature function of part-load ratio
			Real64 const ExhaustAirTemperature, // Combustion exhaust air temperature (C)
			Real64 const ExhaustAirHumRat, // Combustion exhaust air humidity ratio (kg/kg)
			int const CompType_Num,
			Real64 const RefCombustAirInletDensity, // Reference combustion air inlet density (kg/m3)
			Real64 const MinPartLoadRat, // Min allowed operating frac full load
			Real64 const MaxPartLoadRat, // Max allowed operating frac full load
			Real64 const FuelEnergyUseRateHHV, // Rate of Fuel Energy required to run microturbine, HHV basis (W)
			Real64 const FuelEnergyUseRateLHV, // Rate of Fuel Energy required to run microturbine, LHV basis (W)
			Real64 const QHeatRecovered, // Recovered exhaust energy rate to heat water  (W)
			Real64 const ExhaustEnergyRec, // Recovered exhaust energy to heat water (J)
			Real64 const DesignHeatRecMassFlowRate, // Design Water mass flow rate through heat recovery loop (kg/s)
			bool const HeatRecActive, // TRUE when heat recovery water inlet and outlet nodes are defined
			Real64 const HeatRecInletTemp, // Inlet Temperature of the heat recovery fluid (C)
			Real64 const HeatRecOutletTemp, // Outlet Temperature of the heat recovery fluid (C)
			Real64 const HeatRecMinMassFlowRate, // Minimum heat recovery water mass flow rate (kg/s)
			Real64 const HeatRecMaxMassFlowRate, // Maximum heat recovery water mass flow rate (kg/s)
			Real64 const HeatRecMdot, // Heat Recovery Loop Mass flow rate (kg/s)
			int const HRLoopNum, // cooling water plant loop index number, for heat recovery
			int const HRLoopSideNum, // cooling water plant loop side index, for heat recovery
			int const HRBranchNum, // cooling water plant loop branch index, for heat recovery
			int const HRCompNum, // cooling water plant loop component index, for heat recovery
			Real64 const FuelMdot, // Fuel Amount used (kg/s)
			Real64 const ElecPowerGenerated, // Electric power generated (W)
			Real64 const StandbyPowerRate, // Standby power rate this time step (W)
			Real64 const AncillaryPowerRate, // Ancillary power rate this time step (W)
			int const PowerFTempElevErrorIndex, // Index to power as a function of temp/elevation warning message
			int const EffFTempErrorIndex, // Index to efficiency as a function of temperature warning message
			int const EffFPLRErrorIndex, // Index to efficiency as a function of PLR warning message
			int const ExhFlowFTempErrorIndex, // Index to exhaust flow as a function of temp warning message
			int const ExhFlowFPLRErrorIndex, // Index to exhaust flow as a function of PLR warning message
			int const ExhTempFTempErrorIndex, // Index to exhaust temp as a function of temp warning message
			int const ExhTempFPLRErrorIndex, // Index to exhaust temp as a function of PLR warning message
			int const HRMinFlowErrorIndex, // Index to reclaim water flow rate warning message
			int const HRMaxFlowErrorIndex, // Index to reclaim water flow rate warning message
			int const ExhTempLTInletTempIndex, // Index to exhaust temp < combustion inlet air temp warning messages
			int const ExhHRLTInletHRIndex, // Index to exhaust hum rat < combustion inlet air hum rat warning messages
			int const AnciPowerIterErrorIndex, // Index to Ancillary Power iteration loop warning messages
			int const AnciPowerFMdotFuelErrorIndex, // Index to Ancillary Power as a function of fuel input warning messages
			int const HeatRecRateFPLRErrorIndex, // Index to heat recovery rate as a function of PLR warning messages
			int const HeatRecRateFTempErrorIndex, // Index to heat recovery rate as a function of temp warning messages
			int const HeatRecRateFFlowErrorIndex, // Index to heat recovery rate as a function of flow warning messages
			int const ThermEffFTempElevErrorIndex // Index to thermal efficiency as a function of temp/elevation warnings
		) :
			Name( Name ),
			RefElecPowerOutput( RefElecPowerOutput ),
			MinElecPowerOutput( MinElecPowerOutput ),
			MaxElecPowerOutput( MaxElecPowerOutput ),
			RefThermalPowerOutput( RefThermalPowerOutput ),
			MinThermalPowerOutput( MinThermalPowerOutput ),
			MaxThermalPowerOutput( MaxThermalPowerOutput ),
			RefElecEfficiencyLHV( RefElecEfficiencyLHV ),
			RefCombustAirInletTemp( RefCombustAirInletTemp ),
			RefCombustAirInletHumRat( RefCombustAirInletHumRat ),
			RefElevation( RefElevation ),
			ElecPowFTempElevCurveNum( ElecPowFTempElevCurveNum ),
			ElecEffFTempCurveNum( ElecEffFTempCurveNum ),
			ElecEffFPLRCurveNum( ElecEffFPLRCurveNum ),
			FuelHigherHeatingValue( FuelHigherHeatingValue ),
			FuelLowerHeatingValue( FuelLowerHeatingValue ),
			StandbyPower( StandbyPower ),
			AncillaryPower( AncillaryPower ),
			AncillaryPowerFuelCurveNum( AncillaryPowerFuelCurveNum ),
			HeatRecInletNodeNum( HeatRecInletNodeNum ),
			HeatRecOutletNodeNum( HeatRecOutletNodeNum ),
			RefThermalEffLHV( RefThermalEffLHV ),
			RefInletWaterTemp( RefInletWaterTemp ),
			InternalFlowControl( InternalFlowControl ),
			PlantFlowControl( PlantFlowControl ),
			RefHeatRecVolFlowRate( RefHeatRecVolFlowRate ),
			HeatRecFlowFTempPowCurveNum( HeatRecFlowFTempPowCurveNum ),
			ThermEffFTempElevCurveNum( ThermEffFTempElevCurveNum ),
			HeatRecRateFPLRCurveNum( HeatRecRateFPLRCurveNum ),
			HeatRecRateFTempCurveNum( HeatRecRateFTempCurveNum ),
			HeatRecRateFWaterFlowCurveNum( HeatRecRateFWaterFlowCurveNum ),
			HeatRecMinVolFlowRate( HeatRecMinVolFlowRate ),
			HeatRecMaxVolFlowRate( HeatRecMaxVolFlowRate ),
			HeatRecMaxWaterTemp( HeatRecMaxWaterTemp ),
			CombustionAirInletNodeNum( CombustionAirInletNodeNum ),
			CombustionAirOutletNodeNum( CombustionAirOutletNodeNum ),
			ExhAirCalcsActive( ExhAirCalcsActive ),
			RefExhaustAirMassFlowRate( RefExhaustAirMassFlowRate ),
			ExhaustAirMassFlowRate( ExhaustAirMassFlowRate ),
			ExhFlowFTempCurveNum( ExhFlowFTempCurveNum ),
			ExhFlowFPLRCurveNum( ExhFlowFPLRCurveNum ),
			NomExhAirOutletTemp( NomExhAirOutletTemp ),
			ExhAirTempFTempCurveNum( ExhAirTempFTempCurveNum ),
			ExhAirTempFPLRCurveNum( ExhAirTempFPLRCurveNum ),
			ExhaustAirTemperature( ExhaustAirTemperature ),
			ExhaustAirHumRat( ExhaustAirHumRat ),
			CompType_Num( CompType_Num ),
			RefCombustAirInletDensity( RefCombustAirInletDensity ),
			MinPartLoadRat( MinPartLoadRat ),
			MaxPartLoadRat( MaxPartLoadRat ),
			FuelEnergyUseRateHHV( FuelEnergyUseRateHHV ),
			FuelEnergyUseRateLHV( FuelEnergyUseRateLHV ),
			QHeatRecovered( QHeatRecovered ),
			ExhaustEnergyRec( ExhaustEnergyRec ),
			DesignHeatRecMassFlowRate( DesignHeatRecMassFlowRate ),
			HeatRecActive( HeatRecActive ),
			HeatRecInletTemp( HeatRecInletTemp ),
			HeatRecOutletTemp( HeatRecOutletTemp ),
			HeatRecMinMassFlowRate( HeatRecMinMassFlowRate ),
			HeatRecMaxMassFlowRate( HeatRecMaxMassFlowRate ),
			HeatRecMdot( HeatRecMdot ),
			HRLoopNum( HRLoopNum ),
			HRLoopSideNum( HRLoopSideNum ),
			HRBranchNum( HRBranchNum ),
			HRCompNum( HRCompNum ),
			FuelMdot( FuelMdot ),
			ElecPowerGenerated( ElecPowerGenerated ),
			StandbyPowerRate( StandbyPowerRate ),
			AncillaryPowerRate( AncillaryPowerRate ),
			PowerFTempElevErrorIndex( PowerFTempElevErrorIndex ),
			EffFTempErrorIndex( EffFTempErrorIndex ),
			EffFPLRErrorIndex( EffFPLRErrorIndex ),
			ExhFlowFTempErrorIndex( ExhFlowFTempErrorIndex ),
			ExhFlowFPLRErrorIndex( ExhFlowFPLRErrorIndex ),
			ExhTempFTempErrorIndex( ExhTempFTempErrorIndex ),
			ExhTempFPLRErrorIndex( ExhTempFPLRErrorIndex ),
			HRMinFlowErrorIndex( HRMinFlowErrorIndex ),
			HRMaxFlowErrorIndex( HRMaxFlowErrorIndex ),
			ExhTempLTInletTempIndex( ExhTempLTInletTempIndex ),
			ExhHRLTInletHRIndex( ExhHRLTInletHRIndex ),
			AnciPowerIterErrorIndex( AnciPowerIterErrorIndex ),
			AnciPowerFMdotFuelErrorIndex( AnciPowerFMdotFuelErrorIndex ),
			HeatRecRateFPLRErrorIndex( HeatRecRateFPLRErrorIndex ),
			HeatRecRateFTempErrorIndex( HeatRecRateFTempErrorIndex ),
			HeatRecRateFFlowErrorIndex( HeatRecRateFFlowErrorIndex ),
			ThermEffFTempElevErrorIndex( ThermEffFTempElevErrorIndex )
		{}

	};

	struct ReportVars
	{
		// Members
		Real64 PowerGen; // Reporting: Electric power produced (W)
		Real64 EnergyGen; // Reporting: Electric energy produced (J)
		Real64 QHeatRecovered; // Reporting: Heat recovered from exhaust to heat water (W)
		Real64 ExhaustEnergyRec; // Reporting: Heat recovered from exhaust to heat water (J)
		Real64 FuelEnergyUseRateHHV; // Reporting: Fuel Energy use rate, HHV basis (W)
		Real64 FuelEnergyHHV; // Reporting: Fuel Energy used (J)
		Real64 FuelMdot; // Reporting: Fuel Amount used (kg/s)
		Real64 ElectricEfficiencyLHV; // Reporting: Electric efficiency LHV (-)
		Real64 ThermalEfficiencyLHV; // Reporting: Thermal (heat recovery to water) efficiency LHV (-)
		Real64 HeatRecInletTemp; // Reporting: Heat Recovery Loop Inlet Temperature (C)
		Real64 HeatRecOutletTemp; // Reporting: Heat Recovery Loop Outlet Temperature (C)
		Real64 HeatRecMdot; // Reporting: Heat Recovery Loop Mass flow rate (kg/s)
		Real64 AncillaryPowerRate; // Reporting: Ancillary power use rate (W)
		Real64 AncillaryEnergy; // Reporting: Ancillary energy use (J)
		Real64 StandbyPowerRate; // Reporting: Standby power use rate (W)
		Real64 StandbyEnergy; // Reporting: Standby energy use (J)
		Real64 ExhAirMassFlowRate; // Actual Exhaust Air Mass Flow Rate (kg/s)
		Real64 ExhAirTemperature; // Combustion exhaust air temperature (C)

		// Default Constructor
		ReportVars() :
			PowerGen( 0.0 ),
			EnergyGen( 0.0 ),
			QHeatRecovered( 0.0 ),
			ExhaustEnergyRec( 0.0 ),
			FuelEnergyUseRateHHV( 0.0 ),
			FuelEnergyHHV( 0.0 ),
			FuelMdot( 0.0 ),
			ElectricEfficiencyLHV( 0.0 ),
			ThermalEfficiencyLHV( 0.0 ),
			HeatRecInletTemp( 0.0 ),
			HeatRecOutletTemp( 0.0 ),
			HeatRecMdot( 0.0 ),
			AncillaryPowerRate( 0.0 ),
			AncillaryEnergy( 0.0 ),
			StandbyPowerRate( 0.0 ),
			StandbyEnergy( 0.0 ),
			ExhAirMassFlowRate( 0.0 ),
			ExhAirTemperature( 0.0 )
		{}

		// Member Constructor
		ReportVars(
			Real64 const PowerGen, // Reporting: Electric power produced (W)
			Real64 const EnergyGen, // Reporting: Electric energy produced (J)
			Real64 const QHeatRecovered, // Reporting: Heat recovered from exhaust to heat water (W)
			Real64 const ExhaustEnergyRec, // Reporting: Heat recovered from exhaust to heat water (J)
			Real64 const FuelEnergyUseRateHHV, // Reporting: Fuel Energy use rate, HHV basis (W)
			Real64 const FuelEnergyHHV, // Reporting: Fuel Energy used (J)
			Real64 const FuelMdot, // Reporting: Fuel Amount used (kg/s)
			Real64 const ElectricEfficiencyLHV, // Reporting: Electric efficiency LHV (-)
			Real64 const ThermalEfficiencyLHV, // Reporting: Thermal (heat recovery to water) efficiency LHV (-)
			Real64 const HeatRecInletTemp, // Reporting: Heat Recovery Loop Inlet Temperature (C)
			Real64 const HeatRecOutletTemp, // Reporting: Heat Recovery Loop Outlet Temperature (C)
			Real64 const HeatRecMdot, // Reporting: Heat Recovery Loop Mass flow rate (kg/s)
			Real64 const AncillaryPowerRate, // Reporting: Ancillary power use rate (W)
			Real64 const AncillaryEnergy, // Reporting: Ancillary energy use (J)
			Real64 const StandbyPowerRate, // Reporting: Standby power use rate (W)
			Real64 const StandbyEnergy, // Reporting: Standby energy use (J)
			Real64 const ExhAirMassFlowRate, // Actual Exhaust Air Mass Flow Rate (kg/s)
			Real64 const ExhAirTemperature // Combustion exhaust air temperature (C)
		) :
			PowerGen( PowerGen ),
			EnergyGen( EnergyGen ),
			QHeatRecovered( QHeatRecovered ),
			ExhaustEnergyRec( ExhaustEnergyRec ),
			FuelEnergyUseRateHHV( FuelEnergyUseRateHHV ),
			FuelEnergyHHV( FuelEnergyHHV ),
			FuelMdot( FuelMdot ),
			ElectricEfficiencyLHV( ElectricEfficiencyLHV ),
			ThermalEfficiencyLHV( ThermalEfficiencyLHV ),
			HeatRecInletTemp( HeatRecInletTemp ),
			HeatRecOutletTemp( HeatRecOutletTemp ),
			HeatRecMdot( HeatRecMdot ),
			AncillaryPowerRate( AncillaryPowerRate ),
			AncillaryEnergy( AncillaryEnergy ),
			StandbyPowerRate( StandbyPowerRate ),
			StandbyEnergy( StandbyEnergy ),
			ExhAirMassFlowRate( ExhAirMassFlowRate ),
			ExhAirTemperature( ExhAirTemperature )
		{}

	};

	// Object Data
	extern Array1D< MTGeneratorSpecs > MTGenerator; // dimension to number of generators
	extern Array1D< ReportVars > MTGeneratorReport;

	// Functions

	void
	SimMTGenerator(
		int const GeneratorType, // Type of generator !unused1208
		std::string const & GeneratorName, // User-specified name of generator
		int & GeneratorIndex, // Index to microturbine generator
		bool const RunFlag, // Simulate generator when TRUE
		Real64 const MyLoad, // Generator demand (W)
		bool const FirstHVACIteration // Simulation flag for First HVAC (system) iteration
	);

	void
	SimMTPlantHeatRecovery(
		std::string const & CompType, // unused1208
		std::string const & CompName,
		int const CompTypeNum, // unused1208
		int & CompNum,
		bool const RunFlag, // unused1208
		bool & InitLoopEquip,
		Real64 & MyLoad, // unused1208
		Real64 & MaxCap,
		Real64 & MinCap,
		Real64 & OptCap,
		bool const FirstHVACIteration // TRUE if First iteration of simulation !unused1208
	);

	// End MT Generator Module Driver Subroutine
	//******************************************************************************

	// Beginning of Microturbine (MT) Generator Module Get Input Subroutine
	//******************************************************************************

	void
	GetMTGeneratorInput();

	// End of Get Input subroutine for the MT Generator Module
	//******************************************************************************

	// Begin MT Generator Module Initialize Subroutine
	// *****************************************************************************

	void
	InitMTGenerators(
		int const GenNum,
		bool const RunFlag,
		Real64 const MyLoad, // electrical load in W
		bool const FirstHVACIteration
	);

	//  End of MT Generator Module Initialize Subroutine
	// *****************************************************************************

	//  Beginning of MT Generator Model Calculation Subroutine
	// *****************************************************************************

	void
	CalcMTGeneratorModel(
		int const GeneratorNum, // Generator number
		bool const RunFlag, // TRUE when generator is being asked to operate
		Real64 const MyLoad, // Generator demand (W)
		bool const FirstHVACIteration // unused1208
	);

	//  End of MT Generator Model Calculation Subroutine
	// *****************************************************************************

	//  Beginning of record keeping subroutine for the MT Generator Module
	// *****************************************************************************

	void
	UpdateMTGeneratorRecords( int const Num ); // Generator number

	void
	GetMTGeneratorResults(
		int const GeneratorType, // type of Generator !unused1208
		int const GeneratorIndex,
		Real64 & GeneratorPower, // electrical power
		Real64 & GeneratorEnergy, // electrical energy
		Real64 & ThermalPower, // heat power
		Real64 & ThermalEnergy // heat energy
	);

	void
	GetMTGeneratorExhaustNode(
		int const CompType,
		std::string const & CompName,
		int & ExhaustOutletNodeNum
	);

	// End of Record Keeping subroutine for the MT Generator Module
	// *****************************************************************************

} // MicroturbineElectricGenerator

} // EnergyPlus

#endif
