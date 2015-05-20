#ifndef CTElectricGenerator_hh_INCLUDED
#define CTElectricGenerator_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>
#include <DataGlobalConstants.hh>
#include <DataGlobals.hh>

namespace EnergyPlus {

namespace CTElectricGenerator {

	// Using/Aliasing
	using DataGlobalConstants::iGeneratorCombTurbine;

	// Data
	// MODULE PARAMETER DEFINITIONS:
	// na

	// DERIVED TYPE DEFINITIONS:

	// MODULE VARIABLE DECLARATIONS:
	extern int NumCTGenerators; // number of CT Generators specified in input
	extern bool GetCTInput; // then TRUE, calls subroutine to read input file.

	extern Array1D_bool CheckEquipName;

	// SUBROUTINE SPECIFICATIONS FOR MODULE PrimaryPlantLoops

	// Types

	struct CTGeneratorSpecs
	{
		// Members
		std::string Name; // user identifier
		std::string TypeOf; // Type of Generator
		int CompType_Num;
		std::string FuelType; // Type of Fuel - DIESEL, GASOLINE, GAS
		Real64 RatedPowerOutput; // W - design nominal capacity of Generator
		int ElectricCircuitNode; // Electric Circuit Node
		Real64 MinPartLoadRat; // (CT MIN) min allowed operating frac full load
		Real64 MaxPartLoadRat; // (CT MAX) max allowed operating frac full load
		Real64 OptPartLoadRat; // (CT BEST) optimal operating frac full load
		Real64 FuelEnergyUseRate; // (EFUEL) rate of Fuel Energy Required to run COMBUSTION turbine (W)
		Real64 FuelEnergy; // Amount of Fuel Energy Required to run COMBUSTION turbine (J)
		int PLBasedFuelInputCurve; // (FUL1GC) Curve Index for Part Load Ratio Based Fuel Input
		// Coefficients Poly Fit
		int TempBasedFuelInputCurve; // (FUL2GC) Curve Index for Ambient Temperature Based Fuel Input
		// Coeff Poly Fit
		Real64 ExhaustFlow; // (FEX) Exhaust Gas Flow Rate cubic meters per second???
		int ExhaustFlowCurve; // (FEXGC) Curve Index for Exhaust Gas Flow Rate Input Coef Poly Fit
		Real64 ExhaustTemp; // (TEX) Exhaust Gas Temperature in C
		int PLBasedExhaustTempCurve; // (TEX1GC) Curve Index for Part Load Ratio Based Exhaust Temp Input
		// Coeffs Poly Fit
		int TempBasedExhaustTempCurve; // (TEX2GC) Curve Index for Ambient Temperature Based Exhaust Gas Temp to
		// Fuel Energy Input Coeffs Poly Fit
		Real64 QLubeOilRecovered; // (ELUBE) Recovered Lube Oil Energy (W)
		Real64 QExhaustRecovered; // (EEX) Recovered Exhaust heat  (W)
		Real64 QTotalHeatRecovered; // total heat recovered (W)
		Real64 LubeOilEnergyRec; // Recovered Lube Oil Energy (J)
		Real64 ExhaustEnergyRec; // Recovered Exhaust heat  (J)
		Real64 TotalHeatEnergyRec; // total heat recovered (J)
		int QLubeOilRecoveredCurve; // (ELUBEGC) Curve Index for Recoverable Lube Oil heat Input Coef Poly Fit
		Real64 UA; // (UACGC) exhaust gas Heat Exchanger UA
		Array1D< Real64 > UACoef; // Heat Exchanger UA  Coeffs Poly Fit
		Real64 MaxExhaustperCTPower; // MAX EXHAUST FLOW PER W POWER OUTPUT COEFF
		Real64 DesignHeatRecVolFlowRate; // m3/s, Design Water mass flow rate through heat recovery loop
		Real64 DesignHeatRecMassFlowRate; // kg/s, Design Water mass flow rate through heat recovery loop
		Real64 DesignMinExitGasTemp; // Steam Saturation Temperature (C)
		Real64 DesignAirInletTemp; // Design Turbine Air Inlet Temperature (C)
		Real64 ExhaustStackTemp; // turbine exhaust gas temp (C)
		bool HeatRecActive; // true when design max flow rate > 0
		int HeatRecInletNodeNum; // Node number on the heat recovery inlet side of the condenser
		int HeatRecOutletNodeNum; // Node number on the heat recovery outlet side of the condenser
		Real64 HeatRecInletTemp; // Inlet Temperature of the heat recovery fluid
		Real64 HeatRecOutletTemp; // Outlet Temperature of the heat recovery fluid
		Real64 HeatRecMdot; // reporting: Heat Recovery Loop Mass flow rate
		int HRLoopNum; // cooling water plant loop index number, for heat recovery
		int HRLoopSideNum; // cooling water plant loop side index, for heat recovery
		int HRBranchNum; // cooling water plant loop branch index, for heat recovery
		int HRCompNum; // cooling water plant loop component index, for heat recovery
		Real64 FuelMdot; // reporting: Fuel Amount used (kg/s)
		Real64 FuelHeatingValue; // Heating Value for Fuel in (kJ/kg)
		Real64 ElecPowerGenerated; // reporting: power generated (W)
		Real64 ElecEnergyGenerated; // reporting: power generated (W)
		Real64 HeatRecMaxTemp; // Max Temp that can be produced in heat recovery
		int OAInletNode; // optional inlet node index pointer for outdoor air for compustion

		// Default Constructor
		CTGeneratorSpecs() :
			TypeOf( "Generator:CombustionTurbine" ),
			CompType_Num( iGeneratorCombTurbine ),
			RatedPowerOutput( 0.0 ),
			ElectricCircuitNode( 0 ),
			MinPartLoadRat( 0.0 ),
			MaxPartLoadRat( 0.0 ),
			OptPartLoadRat( 0.0 ),
			FuelEnergyUseRate( 0.0 ),
			FuelEnergy( 0.0 ),
			PLBasedFuelInputCurve( 0 ),
			TempBasedFuelInputCurve( 0 ),
			ExhaustFlow( 0.0 ),
			ExhaustFlowCurve( 0 ),
			ExhaustTemp( 0.0 ),
			PLBasedExhaustTempCurve( 0 ),
			TempBasedExhaustTempCurve( 0 ),
			QLubeOilRecovered( 0.0 ),
			QExhaustRecovered( 0.0 ),
			QTotalHeatRecovered( 0.0 ),
			LubeOilEnergyRec( 0.0 ),
			ExhaustEnergyRec( 0.0 ),
			TotalHeatEnergyRec( 0.0 ),
			QLubeOilRecoveredCurve( 0 ),
			UA( 0.0 ),
			UACoef( 2, 0.0 ),
			MaxExhaustperCTPower( 0.0 ),
			DesignHeatRecVolFlowRate( 0.0 ),
			DesignHeatRecMassFlowRate( 0.0 ),
			DesignMinExitGasTemp( 0.0 ),
			DesignAirInletTemp( 0.0 ),
			ExhaustStackTemp( 0.0 ),
			HeatRecActive( false ),
			HeatRecInletNodeNum( 0 ),
			HeatRecOutletNodeNum( 0 ),
			HeatRecInletTemp( 0.0 ),
			HeatRecOutletTemp( 0.0 ),
			HeatRecMdot( 0.0 ),
			HRLoopNum( 0 ),
			HRLoopSideNum( 0 ),
			HRBranchNum( 0 ),
			HRCompNum( 0 ),
			FuelMdot( 0.0 ),
			FuelHeatingValue( 0.0 ),
			ElecPowerGenerated( 0.0 ),
			ElecEnergyGenerated( 0.0 ),
			HeatRecMaxTemp( 0.0 ),
			OAInletNode( 0 )
		{}

		// Member Constructor
		CTGeneratorSpecs(
			std::string const & Name, // user identifier
			std::string const & TypeOf, // Type of Generator
			int const CompType_Num,
			std::string const & FuelType, // Type of Fuel - DIESEL, GASOLINE, GAS
			Real64 const RatedPowerOutput, // W - design nominal capacity of Generator
			int const ElectricCircuitNode, // Electric Circuit Node
			Real64 const MinPartLoadRat, // (CT MIN) min allowed operating frac full load
			Real64 const MaxPartLoadRat, // (CT MAX) max allowed operating frac full load
			Real64 const OptPartLoadRat, // (CT BEST) optimal operating frac full load
			Real64 const FuelEnergyUseRate, // (EFUEL) rate of Fuel Energy Required to run COMBUSTION turbine (W)
			Real64 const FuelEnergy, // Amount of Fuel Energy Required to run COMBUSTION turbine (J)
			int const PLBasedFuelInputCurve, // (FUL1GC) Curve Index for Part Load Ratio Based Fuel Input
			int const TempBasedFuelInputCurve, // (FUL2GC) Curve Index for Ambient Temperature Based Fuel Input
			Real64 const ExhaustFlow, // (FEX) Exhaust Gas Flow Rate cubic meters per second???
			int const ExhaustFlowCurve, // (FEXGC) Curve Index for Exhaust Gas Flow Rate Input Coef Poly Fit
			Real64 const ExhaustTemp, // (TEX) Exhaust Gas Temperature in C
			int const PLBasedExhaustTempCurve, // (TEX1GC) Curve Index for Part Load Ratio Based Exhaust Temp Input
			int const TempBasedExhaustTempCurve, // (TEX2GC) Curve Index for Ambient Temperature Based Exhaust Gas Temp to
			Real64 const QLubeOilRecovered, // (ELUBE) Recovered Lube Oil Energy (W)
			Real64 const QExhaustRecovered, // (EEX) Recovered Exhaust heat  (W)
			Real64 const QTotalHeatRecovered, // total heat recovered (W)
			Real64 const LubeOilEnergyRec, // Recovered Lube Oil Energy (J)
			Real64 const ExhaustEnergyRec, // Recovered Exhaust heat  (J)
			Real64 const TotalHeatEnergyRec, // total heat recovered (J)
			int const QLubeOilRecoveredCurve, // (ELUBEGC) Curve Index for Recoverable Lube Oil heat Input Coef Poly Fit
			Real64 const UA, // (UACGC) exhaust gas Heat Exchanger UA
			Array1< Real64 > const & UACoef, // Heat Exchanger UA  Coeffs Poly Fit
			Real64 const MaxExhaustperCTPower, // MAX EXHAUST FLOW PER W POWER OUTPUT COEFF
			Real64 const DesignHeatRecVolFlowRate, // m3/s, Design Water mass flow rate through heat recovery loop
			Real64 const DesignHeatRecMassFlowRate, // kg/s, Design Water mass flow rate through heat recovery loop
			Real64 const DesignMinExitGasTemp, // Steam Saturation Temperature (C)
			Real64 const DesignAirInletTemp, // Design Turbine Air Inlet Temperature (C)
			Real64 const ExhaustStackTemp, // turbine exhaust gas temp (C)
			bool const HeatRecActive, // true when design max flow rate > 0
			int const HeatRecInletNodeNum, // Node number on the heat recovery inlet side of the condenser
			int const HeatRecOutletNodeNum, // Node number on the heat recovery outlet side of the condenser
			Real64 const HeatRecInletTemp, // Inlet Temperature of the heat recovery fluid
			Real64 const HeatRecOutletTemp, // Outlet Temperature of the heat recovery fluid
			Real64 const HeatRecMdot, // reporting: Heat Recovery Loop Mass flow rate
			int const HRLoopNum, // cooling water plant loop index number, for heat recovery
			int const HRLoopSideNum, // cooling water plant loop side index, for heat recovery
			int const HRBranchNum, // cooling water plant loop branch index, for heat recovery
			int const HRCompNum, // cooling water plant loop component index, for heat recovery
			Real64 const FuelMdot, // reporting: Fuel Amount used (kg/s)
			Real64 const FuelHeatingValue, // Heating Value for Fuel in (kJ/kg)
			Real64 const ElecPowerGenerated, // reporting: power generated (W)
			Real64 const ElecEnergyGenerated, // reporting: power generated (W)
			Real64 const HeatRecMaxTemp, // Max Temp that can be produced in heat recovery
			int const OAInletNode // optional inlet node index pointer for outdoor air for compustion
		) :
			Name( Name ),
			TypeOf( TypeOf ),
			CompType_Num( CompType_Num ),
			FuelType( FuelType ),
			RatedPowerOutput( RatedPowerOutput ),
			ElectricCircuitNode( ElectricCircuitNode ),
			MinPartLoadRat( MinPartLoadRat ),
			MaxPartLoadRat( MaxPartLoadRat ),
			OptPartLoadRat( OptPartLoadRat ),
			FuelEnergyUseRate( FuelEnergyUseRate ),
			FuelEnergy( FuelEnergy ),
			PLBasedFuelInputCurve( PLBasedFuelInputCurve ),
			TempBasedFuelInputCurve( TempBasedFuelInputCurve ),
			ExhaustFlow( ExhaustFlow ),
			ExhaustFlowCurve( ExhaustFlowCurve ),
			ExhaustTemp( ExhaustTemp ),
			PLBasedExhaustTempCurve( PLBasedExhaustTempCurve ),
			TempBasedExhaustTempCurve( TempBasedExhaustTempCurve ),
			QLubeOilRecovered( QLubeOilRecovered ),
			QExhaustRecovered( QExhaustRecovered ),
			QTotalHeatRecovered( QTotalHeatRecovered ),
			LubeOilEnergyRec( LubeOilEnergyRec ),
			ExhaustEnergyRec( ExhaustEnergyRec ),
			TotalHeatEnergyRec( TotalHeatEnergyRec ),
			QLubeOilRecoveredCurve( QLubeOilRecoveredCurve ),
			UA( UA ),
			UACoef( 2, UACoef ),
			MaxExhaustperCTPower( MaxExhaustperCTPower ),
			DesignHeatRecVolFlowRate( DesignHeatRecVolFlowRate ),
			DesignHeatRecMassFlowRate( DesignHeatRecMassFlowRate ),
			DesignMinExitGasTemp( DesignMinExitGasTemp ),
			DesignAirInletTemp( DesignAirInletTemp ),
			ExhaustStackTemp( ExhaustStackTemp ),
			HeatRecActive( HeatRecActive ),
			HeatRecInletNodeNum( HeatRecInletNodeNum ),
			HeatRecOutletNodeNum( HeatRecOutletNodeNum ),
			HeatRecInletTemp( HeatRecInletTemp ),
			HeatRecOutletTemp( HeatRecOutletTemp ),
			HeatRecMdot( HeatRecMdot ),
			HRLoopNum( HRLoopNum ),
			HRLoopSideNum( HRLoopSideNum ),
			HRBranchNum( HRBranchNum ),
			HRCompNum( HRCompNum ),
			FuelMdot( FuelMdot ),
			FuelHeatingValue( FuelHeatingValue ),
			ElecPowerGenerated( ElecPowerGenerated ),
			ElecEnergyGenerated( ElecEnergyGenerated ),
			HeatRecMaxTemp( HeatRecMaxTemp ),
			OAInletNode( OAInletNode )
		{}

	};

	struct ReportVars
	{
		// Members
		Real64 PowerGen; // reporting: power (W)
		Real64 EnergyGen; // reporting: power (W)
		Real64 QTotalHeatRecovered; // reporting: total Heat Recovered (W)
		Real64 QLubeOilRecovered; // reporting: Heat Recovered from Lubricant (W)
		Real64 QExhaustRecovered; // reporting: Heat Recovered from exhaust (W)
		Real64 TotalHeatEnergyRec; // reporting: total Heat Recovered (W)
		Real64 LubeOilEnergyRec; // reporting: Heat Recovered from Lubricant (W)
		Real64 ExhaustEnergyRec; // reporting: Heat Recovered from exhaust (W)
		Real64 FuelEnergyUseRate; // reporting: Fuel Energy use rate (W)
		Real64 FuelEnergy; // reporting: Fuel Energy used (J)
		Real64 FuelMdot; // reporting: Fuel Amount used (kg/s)
		Real64 ExhaustStackTemp; // reporting: Exhaust Stack Temperature (C)
		Real64 HeatRecInletTemp; // reporting: Heat Recovery Loop Inlet Temperature (C)
		Real64 HeatRecOutletTemp; // reporting: Heat Recovery Loop Outlet Temperature (C)
		Real64 HeatRecMdot; // reporting: Heat Recovery Loop Mass flow rate (kg/s)

		// Default Constructor
		ReportVars() :
			PowerGen( 0.0 ),
			EnergyGen( 0.0 ),
			QTotalHeatRecovered( 0.0 ),
			QLubeOilRecovered( 0.0 ),
			QExhaustRecovered( 0.0 ),
			TotalHeatEnergyRec( 0.0 ),
			LubeOilEnergyRec( 0.0 ),
			ExhaustEnergyRec( 0.0 ),
			FuelEnergyUseRate( 0.0 ),
			FuelEnergy( 0.0 ),
			FuelMdot( 0.0 ),
			ExhaustStackTemp( 0.0 ),
			HeatRecInletTemp( 0.0 ),
			HeatRecOutletTemp( 0.0 ),
			HeatRecMdot( 0.0 )
		{}

		// Member Constructor
		ReportVars(
			Real64 const PowerGen, // reporting: power (W)
			Real64 const EnergyGen, // reporting: power (W)
			Real64 const QTotalHeatRecovered, // reporting: total Heat Recovered (W)
			Real64 const QLubeOilRecovered, // reporting: Heat Recovered from Lubricant (W)
			Real64 const QExhaustRecovered, // reporting: Heat Recovered from exhaust (W)
			Real64 const TotalHeatEnergyRec, // reporting: total Heat Recovered (W)
			Real64 const LubeOilEnergyRec, // reporting: Heat Recovered from Lubricant (W)
			Real64 const ExhaustEnergyRec, // reporting: Heat Recovered from exhaust (W)
			Real64 const FuelEnergyUseRate, // reporting: Fuel Energy use rate (W)
			Real64 const FuelEnergy, // reporting: Fuel Energy used (J)
			Real64 const FuelMdot, // reporting: Fuel Amount used (kg/s)
			Real64 const ExhaustStackTemp, // reporting: Exhaust Stack Temperature (C)
			Real64 const HeatRecInletTemp, // reporting: Heat Recovery Loop Inlet Temperature (C)
			Real64 const HeatRecOutletTemp, // reporting: Heat Recovery Loop Outlet Temperature (C)
			Real64 const HeatRecMdot // reporting: Heat Recovery Loop Mass flow rate (kg/s)
		) :
			PowerGen( PowerGen ),
			EnergyGen( EnergyGen ),
			QTotalHeatRecovered( QTotalHeatRecovered ),
			QLubeOilRecovered( QLubeOilRecovered ),
			QExhaustRecovered( QExhaustRecovered ),
			TotalHeatEnergyRec( TotalHeatEnergyRec ),
			LubeOilEnergyRec( LubeOilEnergyRec ),
			ExhaustEnergyRec( ExhaustEnergyRec ),
			FuelEnergyUseRate( FuelEnergyUseRate ),
			FuelEnergy( FuelEnergy ),
			FuelMdot( FuelMdot ),
			ExhaustStackTemp( ExhaustStackTemp ),
			HeatRecInletTemp( HeatRecInletTemp ),
			HeatRecOutletTemp( HeatRecOutletTemp ),
			HeatRecMdot( HeatRecMdot )
		{}

	};

	// Object Data
	extern Array1D< CTGeneratorSpecs > CTGenerator; // dimension to number of machines
	extern Array1D< ReportVars > CTGeneratorReport;

	// Functions

	void
	SimCTGenerator(
		int const GeneratorType, // type of Generator
		std::string const & GeneratorName, // user specified name of Generator
		int & GeneratorIndex,
		bool const RunFlag, // simulate Generator when TRUE
		Real64 const MyLoad, // generator demand
		bool const FirstHVACIteration
	);

	void
	SimCTPlantHeatRecovery(
		std::string const & CompType, // unused1208
		std::string const & CompName,
		int const CompTypeNum, // unused1208
		int & CompNum,
		bool const RunFlag,
		bool & InitLoopEquip,
		Real64 & MyLoad,
		Real64 & MaxCap,
		Real64 & MinCap,
		Real64 & OptCap,
		bool const FirstHVACIteration // TRUE if First iteration of simulation
	);

	// End CT Generator Module Driver Subroutines
	//******************************************************************************

	// Beginning of CT Generator Module Get Input subroutines
	//******************************************************************************

	void
	GetCTGeneratorInput();

	// End of Get Input subroutines for the CT Generator Module
	//******************************************************************************

	// Beginning of Generator model Subroutines
	// *****************************************************************************

	void
	CalcCTGeneratorModel(
		int const GeneratorNum, // Generator number
		bool const RunFlag, // TRUE when Generator operating
		Real64 const MyLoad, // Generator demand
		bool const FirstHVACIteration
	);

	// End of CT Generator Module Model Subroutines
	// *****************************************************************************

	// Begin CT Generator Module Utility Subroutines
	// *****************************************************************************

	void
	InitCTGenerators(
		int const GeneratorNum, // Generator number
		bool const RunFlag, // TRUE when Generator operating
		Real64 const MyLoad, // Generator demand
		bool const FirstHVACIteration
	);

	// End CT Generator Module Utility Subroutines
	// *****************************************************************************

	// Beginning of Record Keeping subroutines for the CT Generator Module
	// *****************************************************************************

	void
	UpdateCTGeneratorRecords(
		bool const RunFlag, // TRUE if Generator operating
		int const Num // Generator number
	);

	void
	GetCTGeneratorResults(
		int const GeneratorType, // type of Generator
		int const GeneratorIndex,
		Real64 & GeneratorPower, // electrical power
		Real64 & GeneratorEnergy, // electrical energy
		Real64 & ThermalPower, // heat power
		Real64 & ThermalEnergy // heat energy
	);

	// End of Record Keeping subroutines for the CT Generator Module
	// *****************************************************************************

} // CTElectricGenerator

} // EnergyPlus

#endif
