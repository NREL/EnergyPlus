#ifndef ICEngineElectricGenerator_hh_INCLUDED
#define ICEngineElectricGenerator_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>
#include <DataGlobalConstants.hh>
#include <DataGlobals.hh>

namespace EnergyPlus {

namespace ICEngineElectricGenerator {

	// Using/Aliasing
	using DataGlobalConstants::iGeneratorICEngine;

	// Data
	//MODULE PARAMETER DEFINITIONS
	extern Real64 const ReferenceTemp; // Reference temperature by which lower heating
	// value is reported.  This should be subtracted
	// off of when calculated exhaust energies.

	// DERIVED TYPE DEFINITIONS

	// MODULE VARIABLE DECLARATIONS:
	extern int NumICEngineGenerators; // number of IC ENGINE Generators specified in input
	extern bool GetICEInput; // When TRUE, calls subroutine to read input file.
	extern Array1D_bool CheckEquipName;
	// SUBROUTINE SPECIFICATIONS FOR MODULE IC ENGINEElectricGenerator

	// Types

	struct ICEngineGeneratorSpecs
	{
		// Members
		std::string Name; // user identifier
		std::string TypeOf; // Type of Generator
		int CompType_Num;
		std::string FuelType; // Type of Fuel - DIESEL, GASOLINE, GAS
		Real64 RatedPowerOutput; // W - design nominal capacity of Generator
		int ElectricCircuitNode; // Electric Circuit Node
		Real64 MinPartLoadRat; // (IC ENGINE MIN) min allowed operating frac full load
		Real64 MaxPartLoadRat; // (IC ENGINE MAX) max allowed operating frac full load
		Real64 OptPartLoadRat; // (IC ENGINE BEST) optimal operating frac full load
		Real64 ElecOutputFuelRat; // (RELDC) Ratio of Generator output to Fuel Energy Input
		int ElecOutputFuelCurve; // Curve Index for generator output to Fuel Energy Input Coeff Poly Fit
		Real64 RecJacHeattoFuelRat; // (RJACDC) Ratio of Recoverable Jacket Heat to Fuel Energy Input
		int RecJacHeattoFuelCurve; // Curve Index for Ratio of Recoverable Jacket Heat to
		// Fuel Energy Input Coeff Poly Fit
		Real64 RecLubeHeattoFuelRat; // (RLUBDC) Ratio of Recoverable Lube Oil Heat to Fuel Energy Input
		int RecLubeHeattoFuelCurve; // Curve Index for Ratio of Recoverable Lube Oil Heat to
		// Fuel Energy Input Coef Poly Fit
		Real64 TotExhausttoFuelRat; // (REXDC) Total Exhaust heat Input to Fuel Energy Input
		int TotExhausttoFuelCurve; // Curve Index for Total Exhaust heat Input to Fuel Energy Input
		// Coeffs Poly Fit
		Real64 ExhaustTemp; // (TEXDC) Exhaust Gas Temp to Fuel Energy Input
		int ExhaustTempCurve; // Curve Index for Exhaust Gas Temp to Fuel Energy Input Coeffs Poly Fit
		int ErrExhaustTempIndex; // error index for temp curve
		Real64 UA; // (UACDC) exhaust gas Heat Exchanger UA to Capacity
		Array1D< Real64 > UACoef; // Heat Exchanger UA Coeffs Poly Fit
		Real64 MaxExhaustperPowerOutput; // MAX EXHAUST FLOW PER W DSL POWER OUTPUT COEFF
		Real64 DesignMinExitGasTemp; // Steam Saturation Temperature
		Real64 FuelHeatingValue; // Heating Value of Fuel in kJ/kg
		Real64 DesignHeatRecVolFlowRate; // m3/s, Design Water mass flow rate through heat recovery loop
		Real64 DesignHeatRecMassFlowRate; // kg/s, Design Water mass flow rate through heat recovery loop
		bool HeatRecActive; // True if Heat Rec Design Vol Flow Rate > 0
		int HeatRecInletNodeNum; // Node number on the heat recovery inlet side of the condenser
		int HeatRecOutletNodeNum; // Node number on the heat recovery outlet side of the condenser
		Real64 HeatRecInletTemp; // Inlet Temperature of the heat recovery fluid
		Real64 HeatRecOutletTemp; // Outlet Temperature of the heat recovery fluid
		Real64 HeatRecMdotDesign; // reporting: Heat Recovery Loop Mass flow rate
		Real64 HeatRecMdotActual;
		Real64 QTotalHeatRecovered; // total heat recovered (W)
		Real64 QJacketRecovered; // heat recovered from jacket (W)
		Real64 QLubeOilRecovered; // heat recovered from lube (W)
		Real64 QExhaustRecovered; // exhaust gas heat recovered (W)
		Real64 FuelEnergyUseRate; // Fuel Energy used (W)
		Real64 TotalHeatEnergyRec; // total heat recovered (J)
		Real64 JacketEnergyRec; // heat recovered from jacket (J)
		Real64 LubeOilEnergyRec; // heat recovered from lube (J)
		Real64 ExhaustEnergyRec; // exhaust gas heat recovered (J)
		Real64 FuelEnergy; // Fuel Energy used (J)
		Real64 FuelMdot; // Fuel Amount used (Kg/s)
		Real64 ExhaustStackTemp; // Exhaust Stack Temperature (C)
		Real64 ElecPowerGenerated; // Electric Power Generated (W)
		Real64 ElecEnergyGenerated; // Amount of Electric Energy Generated (J)
		Real64 HeatRecMaxTemp; // Max Temp that can be produced in heat recovery
		int HRLoopNum; // cooling water plant loop index number, for heat recovery
		int HRLoopSideNum; // cooling water plant loop side index, for heat recovery
		int HRBranchNum; // cooling water plant loop branch index, for heat recovery
		int HRCompNum; // cooling water plant loop component index, for heat recovery

		// Default Constructor
		ICEngineGeneratorSpecs() :
			TypeOf( "Generator:InternalCombustionEngine" ),
			CompType_Num( iGeneratorICEngine ),
			RatedPowerOutput( 0.0 ),
			ElectricCircuitNode( 0 ),
			MinPartLoadRat( 0.0 ),
			MaxPartLoadRat( 0.0 ),
			OptPartLoadRat( 0.0 ),
			ElecOutputFuelRat( 0.0 ),
			ElecOutputFuelCurve( 0 ),
			RecJacHeattoFuelRat( 0.0 ),
			RecJacHeattoFuelCurve( 0 ),
			RecLubeHeattoFuelRat( 0.0 ),
			RecLubeHeattoFuelCurve( 0 ),
			TotExhausttoFuelRat( 0.0 ),
			TotExhausttoFuelCurve( 0 ),
			ExhaustTemp( 0.0 ),
			ExhaustTempCurve( 0 ),
			ErrExhaustTempIndex( 0 ),
			UA( 0.0 ),
			UACoef( 2, 0.0 ),
			MaxExhaustperPowerOutput( 0.0 ),
			DesignMinExitGasTemp( 0.0 ),
			FuelHeatingValue( 0.0 ),
			DesignHeatRecVolFlowRate( 0.0 ),
			DesignHeatRecMassFlowRate( 0.0 ),
			HeatRecActive( false ),
			HeatRecInletNodeNum( 0 ),
			HeatRecOutletNodeNum( 0 ),
			HeatRecInletTemp( 0.0 ),
			HeatRecOutletTemp( 0.0 ),
			HeatRecMdotDesign( 0.0 ),
			HeatRecMdotActual( 0.0 ),
			QTotalHeatRecovered( 0.0 ),
			QJacketRecovered( 0.0 ),
			QLubeOilRecovered( 0.0 ),
			QExhaustRecovered( 0.0 ),
			FuelEnergyUseRate( 0.0 ),
			TotalHeatEnergyRec( 0.0 ),
			JacketEnergyRec( 0.0 ),
			LubeOilEnergyRec( 0.0 ),
			ExhaustEnergyRec( 0.0 ),
			FuelEnergy( 0.0 ),
			FuelMdot( 0.0 ),
			ExhaustStackTemp( 0.0 ),
			ElecPowerGenerated( 0.0 ),
			ElecEnergyGenerated( 0.0 ),
			HeatRecMaxTemp( 0.0 ),
			HRLoopNum( 0 ),
			HRLoopSideNum( 0 ),
			HRBranchNum( 0 ),
			HRCompNum( 0 )
		{}

		// Member Constructor
		ICEngineGeneratorSpecs(
			std::string const & Name, // user identifier
			std::string const & TypeOf, // Type of Generator
			int const CompType_Num,
			std::string const & FuelType, // Type of Fuel - DIESEL, GASOLINE, GAS
			Real64 const RatedPowerOutput, // W - design nominal capacity of Generator
			int const ElectricCircuitNode, // Electric Circuit Node
			Real64 const MinPartLoadRat, // (IC ENGINE MIN) min allowed operating frac full load
			Real64 const MaxPartLoadRat, // (IC ENGINE MAX) max allowed operating frac full load
			Real64 const OptPartLoadRat, // (IC ENGINE BEST) optimal operating frac full load
			Real64 const ElecOutputFuelRat, // (RELDC) Ratio of Generator output to Fuel Energy Input
			int const ElecOutputFuelCurve, // Curve Index for generator output to Fuel Energy Input Coeff Poly Fit
			Real64 const RecJacHeattoFuelRat, // (RJACDC) Ratio of Recoverable Jacket Heat to Fuel Energy Input
			int const RecJacHeattoFuelCurve, // Curve Index for Ratio of Recoverable Jacket Heat to
			Real64 const RecLubeHeattoFuelRat, // (RLUBDC) Ratio of Recoverable Lube Oil Heat to Fuel Energy Input
			int const RecLubeHeattoFuelCurve, // Curve Index for Ratio of Recoverable Lube Oil Heat to
			Real64 const TotExhausttoFuelRat, // (REXDC) Total Exhaust heat Input to Fuel Energy Input
			int const TotExhausttoFuelCurve, // Curve Index for Total Exhaust heat Input to Fuel Energy Input
			Real64 const ExhaustTemp, // (TEXDC) Exhaust Gas Temp to Fuel Energy Input
			int const ExhaustTempCurve, // Curve Index for Exhaust Gas Temp to Fuel Energy Input Coeffs Poly Fit
			int const ErrExhaustTempIndex, // error index for temp curve
			Real64 const UA, // (UACDC) exhaust gas Heat Exchanger UA to Capacity
			Array1< Real64 > const & UACoef, // Heat Exchanger UA Coeffs Poly Fit
			Real64 const MaxExhaustperPowerOutput, // MAX EXHAUST FLOW PER W DSL POWER OUTPUT COEFF
			Real64 const DesignMinExitGasTemp, // Steam Saturation Temperature
			Real64 const FuelHeatingValue, // Heating Value of Fuel in kJ/kg
			Real64 const DesignHeatRecVolFlowRate, // m3/s, Design Water mass flow rate through heat recovery loop
			Real64 const DesignHeatRecMassFlowRate, // kg/s, Design Water mass flow rate through heat recovery loop
			bool const HeatRecActive, // True if Heat Rec Design Vol Flow Rate > 0
			int const HeatRecInletNodeNum, // Node number on the heat recovery inlet side of the condenser
			int const HeatRecOutletNodeNum, // Node number on the heat recovery outlet side of the condenser
			Real64 const HeatRecInletTemp, // Inlet Temperature of the heat recovery fluid
			Real64 const HeatRecOutletTemp, // Outlet Temperature of the heat recovery fluid
			Real64 const HeatRecMdotDesign, // reporting: Heat Recovery Loop Mass flow rate
			Real64 const HeatRecMdotActual,
			Real64 const QTotalHeatRecovered, // total heat recovered (W)
			Real64 const QJacketRecovered, // heat recovered from jacket (W)
			Real64 const QLubeOilRecovered, // heat recovered from lube (W)
			Real64 const QExhaustRecovered, // exhaust gas heat recovered (W)
			Real64 const FuelEnergyUseRate, // Fuel Energy used (W)
			Real64 const TotalHeatEnergyRec, // total heat recovered (J)
			Real64 const JacketEnergyRec, // heat recovered from jacket (J)
			Real64 const LubeOilEnergyRec, // heat recovered from lube (J)
			Real64 const ExhaustEnergyRec, // exhaust gas heat recovered (J)
			Real64 const FuelEnergy, // Fuel Energy used (J)
			Real64 const FuelMdot, // Fuel Amount used (Kg/s)
			Real64 const ExhaustStackTemp, // Exhaust Stack Temperature (C)
			Real64 const ElecPowerGenerated, // Electric Power Generated (W)
			Real64 const ElecEnergyGenerated, // Amount of Electric Energy Generated (J)
			Real64 const HeatRecMaxTemp, // Max Temp that can be produced in heat recovery
			int const HRLoopNum, // cooling water plant loop index number, for heat recovery
			int const HRLoopSideNum, // cooling water plant loop side index, for heat recovery
			int const HRBranchNum, // cooling water plant loop branch index, for heat recovery
			int const HRCompNum // cooling water plant loop component index, for heat recovery
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
			ElecOutputFuelRat( ElecOutputFuelRat ),
			ElecOutputFuelCurve( ElecOutputFuelCurve ),
			RecJacHeattoFuelRat( RecJacHeattoFuelRat ),
			RecJacHeattoFuelCurve( RecJacHeattoFuelCurve ),
			RecLubeHeattoFuelRat( RecLubeHeattoFuelRat ),
			RecLubeHeattoFuelCurve( RecLubeHeattoFuelCurve ),
			TotExhausttoFuelRat( TotExhausttoFuelRat ),
			TotExhausttoFuelCurve( TotExhausttoFuelCurve ),
			ExhaustTemp( ExhaustTemp ),
			ExhaustTempCurve( ExhaustTempCurve ),
			ErrExhaustTempIndex( ErrExhaustTempIndex ),
			UA( UA ),
			UACoef( 2, UACoef ),
			MaxExhaustperPowerOutput( MaxExhaustperPowerOutput ),
			DesignMinExitGasTemp( DesignMinExitGasTemp ),
			FuelHeatingValue( FuelHeatingValue ),
			DesignHeatRecVolFlowRate( DesignHeatRecVolFlowRate ),
			DesignHeatRecMassFlowRate( DesignHeatRecMassFlowRate ),
			HeatRecActive( HeatRecActive ),
			HeatRecInletNodeNum( HeatRecInletNodeNum ),
			HeatRecOutletNodeNum( HeatRecOutletNodeNum ),
			HeatRecInletTemp( HeatRecInletTemp ),
			HeatRecOutletTemp( HeatRecOutletTemp ),
			HeatRecMdotDesign( HeatRecMdotDesign ),
			HeatRecMdotActual( HeatRecMdotActual ),
			QTotalHeatRecovered( QTotalHeatRecovered ),
			QJacketRecovered( QJacketRecovered ),
			QLubeOilRecovered( QLubeOilRecovered ),
			QExhaustRecovered( QExhaustRecovered ),
			FuelEnergyUseRate( FuelEnergyUseRate ),
			TotalHeatEnergyRec( TotalHeatEnergyRec ),
			JacketEnergyRec( JacketEnergyRec ),
			LubeOilEnergyRec( LubeOilEnergyRec ),
			ExhaustEnergyRec( ExhaustEnergyRec ),
			FuelEnergy( FuelEnergy ),
			FuelMdot( FuelMdot ),
			ExhaustStackTemp( ExhaustStackTemp ),
			ElecPowerGenerated( ElecPowerGenerated ),
			ElecEnergyGenerated( ElecEnergyGenerated ),
			HeatRecMaxTemp( HeatRecMaxTemp ),
			HRLoopNum( HRLoopNum ),
			HRLoopSideNum( HRLoopSideNum ),
			HRBranchNum( HRBranchNum ),
			HRCompNum( HRCompNum )
		{}

	};

	struct ReportVars
	{
		// Members
		Real64 PowerGen; // reporting: power (W)
		Real64 EnergyGen; // reporting: energy (J)
		Real64 QJacketRecovered; // reporting: Heat Recovered from Jacket (W)
		Real64 QLubeOilRecovered; // reporting: Heat Recovered from Lubricant (W)
		Real64 QExhaustRecovered; // reporting: exhaust gas heat recovered (W)
		Real64 QTotalHeatRecovered; // reporting: Total Heat Recovered (W)
		Real64 TotalHeatEnergyRec; // reporting: total heat recovered (J)
		Real64 JacketEnergyRec; // reporting: heat recovered from jacket (J)
		Real64 LubeOilEnergyRec; // reporting: heat recovered from lube (J)
		Real64 ExhaustEnergyRec; // reporting: exhaust gas heat recovered (J)
		Real64 FuelEnergy; // reporting: Fuel Energy used (J)
		Real64 FuelEnergyUseRate; // reporting: Fuel Energy used (W)
		Real64 FuelMdot; // reporting: Fuel used (Kg/s)
		Real64 ExhaustStackTemp; // reporting: Exhaust Stack Temperature (C)
		Real64 HeatRecInletTemp; // reporting: Heat Recovery Loop Inlet Temperature (C)
		Real64 HeatRecOutletTemp; // reporting: Heat Recovery Loop Outlet Temperature (C)
		Real64 HeatRecMdot; // reporting: Heat Recovery Loop Mass flow rate (kg/s)

		// Default Constructor
		ReportVars() :
			PowerGen( 0.0 ),
			EnergyGen( 0.0 ),
			QJacketRecovered( 0.0 ),
			QLubeOilRecovered( 0.0 ),
			QExhaustRecovered( 0.0 ),
			QTotalHeatRecovered( 0.0 ),
			TotalHeatEnergyRec( 0.0 ),
			JacketEnergyRec( 0.0 ),
			LubeOilEnergyRec( 0.0 ),
			ExhaustEnergyRec( 0.0 ),
			FuelEnergy( 0.0 ),
			FuelEnergyUseRate( 0.0 ),
			FuelMdot( 0.0 ),
			ExhaustStackTemp( 0.0 ),
			HeatRecInletTemp( 0.0 ),
			HeatRecOutletTemp( 0.0 ),
			HeatRecMdot( 0.0 )
		{}

		// Member Constructor
		ReportVars(
			Real64 const PowerGen, // reporting: power (W)
			Real64 const EnergyGen, // reporting: energy (J)
			Real64 const QJacketRecovered, // reporting: Heat Recovered from Jacket (W)
			Real64 const QLubeOilRecovered, // reporting: Heat Recovered from Lubricant (W)
			Real64 const QExhaustRecovered, // reporting: exhaust gas heat recovered (W)
			Real64 const QTotalHeatRecovered, // reporting: Total Heat Recovered (W)
			Real64 const TotalHeatEnergyRec, // reporting: total heat recovered (J)
			Real64 const JacketEnergyRec, // reporting: heat recovered from jacket (J)
			Real64 const LubeOilEnergyRec, // reporting: heat recovered from lube (J)
			Real64 const ExhaustEnergyRec, // reporting: exhaust gas heat recovered (J)
			Real64 const FuelEnergy, // reporting: Fuel Energy used (J)
			Real64 const FuelEnergyUseRate, // reporting: Fuel Energy used (W)
			Real64 const FuelMdot, // reporting: Fuel used (Kg/s)
			Real64 const ExhaustStackTemp, // reporting: Exhaust Stack Temperature (C)
			Real64 const HeatRecInletTemp, // reporting: Heat Recovery Loop Inlet Temperature (C)
			Real64 const HeatRecOutletTemp, // reporting: Heat Recovery Loop Outlet Temperature (C)
			Real64 const HeatRecMdot // reporting: Heat Recovery Loop Mass flow rate (kg/s)
		) :
			PowerGen( PowerGen ),
			EnergyGen( EnergyGen ),
			QJacketRecovered( QJacketRecovered ),
			QLubeOilRecovered( QLubeOilRecovered ),
			QExhaustRecovered( QExhaustRecovered ),
			QTotalHeatRecovered( QTotalHeatRecovered ),
			TotalHeatEnergyRec( TotalHeatEnergyRec ),
			JacketEnergyRec( JacketEnergyRec ),
			LubeOilEnergyRec( LubeOilEnergyRec ),
			ExhaustEnergyRec( ExhaustEnergyRec ),
			FuelEnergy( FuelEnergy ),
			FuelEnergyUseRate( FuelEnergyUseRate ),
			FuelMdot( FuelMdot ),
			ExhaustStackTemp( ExhaustStackTemp ),
			HeatRecInletTemp( HeatRecInletTemp ),
			HeatRecOutletTemp( HeatRecOutletTemp ),
			HeatRecMdot( HeatRecMdot )
		{}

	};

	// Object Data
	extern Array1D< ICEngineGeneratorSpecs > ICEngineGenerator; // dimension to number of machines
	extern Array1D< ReportVars > ICEngineGeneratorReport;

	// Functions

	void
	SimICEngineGenerator(
		int const GeneratorType, // type of Generator
		std::string const & GeneratorName, // user specified name of Generator
		int & GeneratorIndex,
		bool const RunFlag, // simulate Generator when TRUE
		Real64 const MyLoad, // demand on electric generator
		bool const FirstHVACIteration
	);

	void
	GetICEGeneratorResults(
		int const GeneratorType, // type of Generator
		int const GeneratorIndex,
		Real64 & GeneratorPower, // electrical power
		Real64 & GeneratorEnergy, // electrical energy
		Real64 & ThermalPower, // heat power
		Real64 & ThermalEnergy // heat energy
	);

	void
	SimICEPlantHeatRecovery(
		std::string const & CompType,
		std::string const & CompName,
		int const CompTypeNum,
		int & CompNum,
		bool const RunFlag,
		bool & InitLoopEquip,
		Real64 & MyLoad,
		Real64 & MaxCap,
		Real64 & MinCap,
		Real64 & OptCap,
		bool const FirstHVACIteration // TRUE if First iteration of simulation
	);

	// End IC ENGINE Generator Module Driver Subroutines
	//******************************************************************************

	// Beginning of IC ENGINE Generator Module Get Input subroutines
	//******************************************************************************

	void
	GetICEngineGeneratorInput();

	// End of Get Input subroutines for the IC ENGINE Generator Module
	//******************************************************************************

	// Beginning of Generator model Subroutines
	// *****************************************************************************

	void
	CalcICEngineGeneratorModel(
		int const GeneratorNum, // Generator number
		bool const RunFlag, // TRUE when Generator operating
		Real64 const MyLoad, // Generator demand
		bool const FirstHVACIteration
	);

	void
	CalcICEngineGenHeatRecovery(
		int const Num, // HR Component number
		Real64 const EnergyRecovered, // Amount of heat recovered
		Real64 const HeatRecMdot,
		Real64 & HRecRatio // Max Heat recovery ratio
	);

	// End IC ENGINE Generator Module Model Subroutines
	// *****************************************************************************

	// Begin IC ENGINE Generator Module Utility Subroutines
	// *****************************************************************************

	void
	InitICEngineGenerators(
		int const GeneratorNum, // Generator number
		bool const RunFlag, // TRUE when Generator operating
		Real64 const MyLoad, // Generator demand
		bool const FirstHVACIteration
	);

	// End IC ENGINE Generator Module Utility Subroutines
	// *****************************************************************************

	// Beginning of Record Keeping subroutines for the IC ENGINE Generator Module
	// *****************************************************************************

	void
	UpdateICEngineGeneratorRecords(
		bool const RunFlag, // TRUE if Generator operating
		int const Num // Generator number
	);

	// End of Record Keeping subroutines for the IC ENGINE Generator Module
	// *****************************************************************************

} // ICEngineElectricGenerator

} // EnergyPlus

#endif
