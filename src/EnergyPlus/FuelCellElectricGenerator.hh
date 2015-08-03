#ifndef FuelCellElectricGenerator_hh_INCLUDED
#define FuelCellElectricGenerator_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>

namespace EnergyPlus {

namespace FuelCellElectricGenerator {

	// Data
	//MODULE PARAMETER DEFINITIONS

	// DERIVED TYPE DEFINITIONS

	// MODULE VARIABLE DECLARATIONS:
	extern bool GetFuelCellInput; // When TRUE, calls subroutine to read input file.
	extern Array1D_bool CheckEquipName;

	// SUBROUTINE SPECIFICATIONS FOR MODULE FuelCell ElectricGenerator

	//PRIVATE    SetupFuelAndAirConstituentData ! hardwired data for gas phase thermochemistry calcs

	// Functions

	void
	SimFuelCellGenerator(
		int const GeneratorType, // type of Generator
		std::string const & GeneratorName, // user specified name of Generator
		int & GeneratorIndex,
		bool const RunFlag, // simulate Generator when TRUE
		Real64 const MyLoad, // demand on electric generator
		bool const FirstHVACIteration
	);

	// End FuelCell Generator Module Driver Subroutines
	//******************************************************************************

	// Beginning of FuelCell Generator Module Get Input subroutines
	//******************************************************************************

	void
	GetFuelCellGeneratorInput();

	// End of Get Input subroutines for the FuelCell Generator Module

	// Beginning of Generator model Subroutines
	// *****************************************************************************

	void
	CalcFuelCellGeneratorModel(
		int const GeneratorNum, // Generator number
		bool const RunFlag, // TRUE when Generator operating
		Real64 const MyLoad, // Generator demand
		bool const FirstHVACIteration
	);

	void
	ManageElectStorInteractions(
		int const Num, // Generator number, index for structure
		Real64 const Pdemand,
		Real64 const PpcuLosses,
		bool & Constrained,
		Real64 & Pstorage,
		Real64 & PgridOverage // electricity that can't be stored and needs to go out
	);

	Real64
	FuelCellProductGasEnthResidual(
		Real64 const TprodGas, // temperature, this is "x" being searched
		Array1< Real64 > const & Par // par(1) = Generator Number
	);

	void
	FigureAirHeatCap(
		int const GeneratorNum, // ID of generator FuelCell data structure
		Real64 const FluidTemp, // degree C
		Real64 & Cp // (J/mol*K)
	);

	void
	FigureAirEnthalpy(
		int const GeneratorNum, // ID of generator FuelCell data structure
		Real64 const FluidTemp, // degree C
		Real64 & Hair // (kJ/mol)
	);

	void
	FigureFuelHeatCap(
		int const GeneratorNum, // ID of generator FuelCell data structure
		Real64 const FluidTemp, // degree C
		Real64 & Cp // (J/mol*K)
	);

	void
	FigureFuelEnthalpy(
		int const GeneratorNum, // ID of generator FuelCell data structure
		Real64 const FluidTemp, // degree C
		Real64 & Hfuel // kJ/mol
	);

	void
	FigureProductGasesEnthalpy(
		int const GeneratorNum, // ID of generator FuelCell data structure
		Real64 const FluidTemp, // degree C
		Real64 & HProdGases // kJ/mol
	);

	void
	FigureProductGasHeatCap(
		int const GeneratorNum, // ID of generator FuelCell data structure
		Real64 const FluidTemp, // degree C
		Real64 & Cp // (J/mol*K)
	);

	void
	FigureAuxilHeatGasHeatCap(
		int const GeneratorNum, // ID of generator FuelCell data structure
		Real64 const FluidTemp, // degree C
		Real64 & Cp // (J/mol*K)
	);

	void
	FigureHXleavingGasHeatCap(
		int const GeneratorNum, // ID of generator FuelCell data structure
		Real64 const FluidTemp, // degree C
		Real64 & Cp // (J/mol*K)
	);

	void
	FigureGaseousWaterEnthalpy(
		Real64 const FluidTemp, // degree C
		Real64 & HGasWater // kJ/mol
	);

	void
	FigureLiquidWaterEnthalpy(
		Real64 const FluidTemp, // degree C
		Real64 & HLiqWater // kJ/mol
	);

	void
	FigureLiquidWaterHeatCap(
		Real64 const FluidTemp, // degree C
		Real64 & Cp // (J/mol*K)
	);

	void
	FigureLHVofFuel(
		int const Num,
		Real64 const NdotFuel,
		Real64 const NdotCO2,
		Real64 const NdotH20,
		Real64 & LHV
	);

	void
	FigureACAncillaries(
		int const GeneratorNum,
		Real64 & PacAncill
	);

	void
	FigurePowerConditioningLosses(
		int const GeneratorNum,
		Real64 const Pdemand,
		Real64 & PpcuLosses
	);

	void
	FigureTransientConstraints(
		int const GeneratorNum, // index number for accessing correct generator
		Real64 & Pel, // DC power control setting for power module
		bool & Constrained, // true if transient constraints kick in
		Real64 & PelDiff // if constrained then this is the difference, positive
	);

	void
	CalcFuelCellAuxHeater( int const Num ); // Generator number

	void
	CalcFuelCellGenHeatRecovery( int const Num ); // Generator number

	void
	SimFuelCellPlantHeatRecovery(
		std::string const & CompType,
		std::string const & CompName,
		int const CompTypeNum,
		int & CompNum,
		bool const RunFlag,
		bool & InitLoopEquip,
		Real64 & MyLoad, // unused1208
		Real64 & MaxCap,
		Real64 & MinCap,
		Real64 & OptCap,
		bool const FirstHVACIteration // TRUE if First iteration of simulation
	);

	// End FuelCell Generator Module Model Subroutines
	// *****************************************************************************

	// Begin FuelCell Generator Module Utility Subroutines
	// *****************************************************************************

	void
	InitFuelCellGenerators( int const FCnum ); // index to specific fuel cell generator

	// End FuelCell Generator Module Utility Subroutines
	// *****************************************************************************

	// Beginning of Record Keeping subroutines for the FuelCell Generator Module
	// *****************************************************************************

	void
	FigureFuelCellZoneGains();

	void
	UpdateExhaustAirFlows( int const Num ); // generator number

	void
	CalcUpdateHeatRecovery(
		int const Num, // Generator number
		bool const FirstHVACIteration
	);

	void
	UpdateFuelCellGeneratorRecords(
		bool const RunFlag, // TRUE if Generator operating
		int const Num // Generator number
	);

	void
	GetFuelCellGeneratorResults(
		int const GeneratorType, // type of Generator
		int const GeneratorIndex,
		Real64 & GeneratorPower, // electrical power
		Real64 & GeneratorEnergy, // electrical energy
		Real64 & ThermalPower, // heat power
		Real64 & ThermalEnergy // heat energy
	);

} // FuelCellElectricGenerator

} // EnergyPlus

#endif
