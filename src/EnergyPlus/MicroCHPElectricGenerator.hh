#ifndef MicroCHPElectricGenerator_hh_INCLUDED
#define MicroCHPElectricGenerator_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>

namespace EnergyPlus {

namespace MicroCHPElectricGenerator {

	// Data
	//MODULE PARAMETER DEFINITIONS

	// DERIVED TYPE DEFINITIONS
	extern bool GetMicroCHPInput; // When TRUE, calls subroutine to read input file.
	extern Array1D_bool CheckEquipName;
	extern Array1D_bool MySizeFlag;

	// SUBROUTINE SPECIFICATIONS FOR MODULE Combustion ElectricGenerator

	// Functions

	void
	SimMicroCHPGenerator(
		int const GeneratorType, // type of Generator
		std::string const & GeneratorName, // user specified name of Generator
		int & GeneratorIndex,
		bool const RunFlagElectCenter, // simulate Generator when TRUE
		bool const RunFlagPlant, // simulate generator when true.
		Real64 const MyElectricLoad, // demand on electric generator
		Real64 const MyThermalLoad, // thermal demand on cogenerator
		bool const FirstHVACIteration
	);

	// End MicroCHPNoNormalize Generator Module Driver Subroutines
	//******************************************************************************

	// Beginning of Combustion Generator Module Get Input subroutines
	//******************************************************************************

	void
	GetMicroCHPGeneratorInput();

	// PARAMETERS

	void
	InitMicroCHPNoNormalizeGenerators(
		int const GeneratorNum, // Generator number
		bool const FirstHVACIteration
	);

	void
	CalcMicroCHPNoNormalizeGeneratorModel(
		int const GeneratorNum, // Generator number
		bool const RunFlagElectCenter, // TRUE when Generator operating
		bool const RunFlagPlant,
		Real64 const MyElectricLoad, // Generator demand
		Real64 const MyThermalLoad,
		bool const FirstHVACIteration
	);

	Real64
	FuncDetermineEngineTemp(
		Real64 const TcwOut, // hot water leaving temp
		Real64 const MCeng, // Fictitious mass and heat capacity of engine
		Real64 const UAHX, // Heat exchanger UA
		Real64 const UAskin, // Skin losses UA
		Real64 const Troom, // surrounding zone temperature C
		Real64 const Qgenss, // steady state generator heat generation
		Real64 const TengLast, // engine temp at previous time step
		Real64 const time // elapsed time since previous evaluation
	);

	Real64
	FuncDetermineCoolantWaterExitTemp(
		Real64 const TcwIn, // hot water inlet temp
		Real64 const MCcw, // Fictitious mass and heat capacity of coolant hx
		Real64 const UAHX, // Heat exchanger UA
		Real64 const MdotCpcw, // mass flow and specific heat of coolant water
		Real64 const Teng, // engine mass temperature C
		Real64 const TcwoutLast, // coolant water leaving temp at previous time step
		Real64 const time // elapsed time since previous evaluation
	);

	bool
	CheckMicroCHPThermalBalance(
		Real64 const NomHeatGen, // nominal heat generation rate for scaling
		Real64 const TcwIn, // hot water inlet temp
		Real64 const TcwOut, // hot water leaving temp
		Real64 const Teng, // engine mass temperature C
		Real64 const Troom, // surrounding zone temperature C
		Real64 const UAHX, // Heat exchanger UA
		Real64 const UAskin, // Skin losses UA
		Real64 const Qgenss, // steady state generator heat generation
		Real64 const MCeng, // Fictitious mass and heat capacity of engine
		Real64 const MCcw, // Fictitious mass and heat capacity of coolant hx
		Real64 const MdotCpcw // mass flow and specific heat of coolant water
	);

	void
	FigureMicroCHPZoneGains();

	void
	CalcUpdateHeatRecovery(
		int const Num, // Generator number
		bool const FirstHVACIteration
	);

	void
	SimMicroCHPPlantHeatRecovery(
		std::string const & CompType,
		std::string const & CompName,
		int & CompNum,
		bool const RunFlag,
		bool & InitLoopEquip,
		Real64 & MyThermalLoad,
		Real64 & MaxCap,
		Real64 & MinCap,
		Real64 & OptCap,
		bool const FirstHVACIteration // TRUE if First iteration of simulation
	);

	void
	UpdateMicroCHPGeneratorRecords( int const Num ); // Generator number

	void
	GetMicroCHPGeneratorResults(
		int const GeneratorType, // type of Generator
		int const GeneratorIndex,
		Real64 & GeneratorPower, // electrical power
		Real64 & GeneratorEnergy, // electrical energy
		Real64 & ThermalPower, // heat power
		Real64 & ThermalEnergy // heat energy
	);

} // MicroCHPElectricGenerator

} // EnergyPlus

#endif
