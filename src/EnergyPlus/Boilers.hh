#ifndef Boilers_hh_INCLUDED
#define Boilers_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>
#include <DataGlobals.hh>

namespace EnergyPlus {

namespace Boilers {

	// Using/Aliasing

	// Data
	// MODULE PARAMETER DEFINITIONS

	// Boiler normalized efficiency curve types
	extern int const Linear;
	extern int const BiLinear;
	extern int const Quadratic;
	extern int const BiQuadratic;
	extern int const Cubic;
	extern int const QuadraticLinear;
	extern int const BiCubic;
	extern int const TriQuadratic;

	// water temperature evaluation method
	extern int const BoilerTempModeNotSet;
	extern int const EnteringBoilerTemp;
	extern int const LeavingBoilerTemp;

	//Boiler flow modes
	extern int const FlowModeNotSet;
	extern int const ConstantFlow;
	extern int const NotModulated;
	extern int const LeavingSetPointModulated;

	// DERIVED TYPE DEFINITIONS

	// MODULE VARIABLE DECLARATIONS:
	extern int NumBoilers; // Number of boilers
	extern Real64 FuelUsed; // W - Boiler fuel used
	extern Real64 ParasiticElecPower; // W - Parasitic electrical power (e.g. forced draft fan)
	extern Real64 BoilerLoad; // W - Boiler Load
	extern Real64 BoilerMassFlowRate; // kg/s - Boiler mass flow rate
	extern Real64 BoilerOutletTemp; // W - Boiler outlet temperature
	extern Real64 BoilerPLR; // Boiler operating part-load ratio

	extern Array1D_bool CheckEquipName;

	// SUBROUTINE SPECIFICATIONS FOR MODULE Boilers

	// Types

	struct BoilerSpecs
	{
		// Members
		std::string Name; // user identifier
		int FuelType; // resource type assignment
		int TypeNum; // plant loop type identifier
		int LoopNum; // plant loop connection
		int LoopSideNum; // plant loop side connection
		int BranchNum; // plant loop branch connection
		int CompNum; // plant loop component connection
		bool Available; // TRUE if machine available in current time step
		bool ON; // TRUE: simulate the machine at it's operating part load ratio
		Real64 NomCap; // W - design nominal capacity of Boiler
		bool NomCapWasAutoSized; // true if previous was set to autosize input
		Real64 Effic; // boiler efficiency at design conditions
		Real64 TempDesBoilerOut; // C - Boiler design outlet temperature
		int FlowMode; // one of 3 modes for componet flow during operation
		bool ModulatedFlowSetToLoop; // True if the setpoint is missing at the outlet node
		bool ModulatedFlowErrDone; // true if setpoint warning issued
		Real64 VolFlowRate; // m3/s - Boiler water design volumetric flow rate
		bool VolFlowRateWasAutoSized; // true if previous was set to autosize input
		Real64 DesMassFlowRate; // kg/s - Boiler water design mass flow rate
		Real64 MassFlowRate; // kg/s - Boiler water mass flow rate
		Real64 SizFac; // sizing factor
		int BoilerInletNodeNum; // Node number at the boiler inlet
		int BoilerOutletNodeNum; // Node number at the boiler outlet
		Real64 MinPartLoadRat; // Minimum allowed operating part load ratio
		Real64 MaxPartLoadRat; // Maximum allowed operating part load ratio
		Real64 OptPartLoadRat; // Optimal operating part load ratio
		Real64 OperPartLoadRat; // Actual operating part load ratio
		int CurveTempMode; // water temp to use in curve, switch between entering and leaving
		int EfficiencyCurvePtr; // Index to efficiency curve
		int EfficiencyCurveType; // Type of efficiency curve
		Real64 TempUpLimitBoilerOut; // C - Boiler outlet maximum temperature limit
		Real64 ParasiticElecLoad; // W - Parasitic electric power (e.g. forced draft fan)
		int EffCurveOutputError; // efficiency curve output <=0 recurring warning error counter
		int EffCurveOutputIndex; // efficiency curve output <=0 recurring warning error message index
		int CalculatedEffError; // calculated efficiency >1.1 recurring warning error counter
		int CalculatedEffIndex; // calculated efficiency >1.1 recurring warning error message index
		bool IsThisSized; // TRUE if sizing is done

		// Default Constructor
		BoilerSpecs() :
			FuelType( 0 ),
			TypeNum( 0 ),
			LoopNum( 0 ),
			LoopSideNum( 0 ),
			BranchNum( 0 ),
			CompNum( 0 ),
			Available( false ),
			ON( false ),
			NomCap( 0.0 ),
			NomCapWasAutoSized( false ),
			Effic( 0.0 ),
			TempDesBoilerOut( 0.0 ),
			FlowMode( FlowModeNotSet ),
			ModulatedFlowSetToLoop( false ),
			ModulatedFlowErrDone( false ),
			VolFlowRate( 0.0 ),
			VolFlowRateWasAutoSized( false ),
			DesMassFlowRate( 0.0 ),
			MassFlowRate( 0.0 ),
			SizFac( 0.0 ),
			BoilerInletNodeNum( 0 ),
			BoilerOutletNodeNum( 0 ),
			MinPartLoadRat( 0.0 ),
			MaxPartLoadRat( 0.0 ),
			OptPartLoadRat( 0.0 ),
			OperPartLoadRat( 0.0 ),
			CurveTempMode( BoilerTempModeNotSet ),
			EfficiencyCurvePtr( 0 ),
			EfficiencyCurveType( 0 ),
			TempUpLimitBoilerOut( 0.0 ),
			ParasiticElecLoad( 0.0 ),
			EffCurveOutputError( 0 ),
			EffCurveOutputIndex( 0 ),
			CalculatedEffError( 0 ),
			CalculatedEffIndex( 0 ),
			IsThisSized( false )
		{}

	};

	struct ReportVars
	{
		// Members
		Real64 BoilerLoad; // W - Boiler operating load
		Real64 BoilerEnergy; // J - Boiler energy integrated over time
		Real64 FuelUsed; // W - Boiler fuel used
		Real64 FuelConsumed; // J - Boiler Fuel consumed integrated over time
		Real64 BoilerInletTemp; // C - Boiler inlet temperature
		Real64 BoilerOutletTemp; // C - Boiler outlet temperature
		Real64 Mdot; // kg/s - Boiler mass flow rate
		Real64 ParasiticElecPower; // W - Parasitic Electrical Power (e.g. forced draft fan)
		Real64 ParasiticElecConsumption; // J - Parasitic Electrical Consumption (e.g. forced draft fan)
		Real64 BoilerPLR; // Boiler operating part-load ratio

		// Default Constructor
		ReportVars() :
			BoilerLoad( 0.0 ),
			BoilerEnergy( 0.0 ),
			FuelUsed( 0.0 ),
			FuelConsumed( 0.0 ),
			BoilerInletTemp( 0.0 ),
			BoilerOutletTemp( 0.0 ),
			Mdot( 0.0 ),
			ParasiticElecPower( 0.0 ),
			ParasiticElecConsumption( 0.0 ),
			BoilerPLR( 0.0 )
		{}

		// Member Constructor
		ReportVars(
			Real64 const BoilerLoad, // W - Boiler operating load
			Real64 const BoilerEnergy, // J - Boiler energy integrated over time
			Real64 const FuelUsed, // W - Boiler fuel used
			Real64 const FuelConsumed, // J - Boiler Fuel consumed integrated over time
			Real64 const BoilerInletTemp, // C - Boiler inlet temperature
			Real64 const BoilerOutletTemp, // C - Boiler outlet temperature
			Real64 const Mdot, // kg/s - Boiler mass flow rate
			Real64 const ParasiticElecPower, // W - Parasitic Electrical Power (e.g. forced draft fan)
			Real64 const ParasiticElecConsumption, // J - Parasitic Electrical Consumption (e.g. forced draft fan)
			Real64 const BoilerPLR // Boiler operating part-load ratio
		) :
			BoilerLoad( BoilerLoad ),
			BoilerEnergy( BoilerEnergy ),
			FuelUsed( FuelUsed ),
			FuelConsumed( FuelConsumed ),
			BoilerInletTemp( BoilerInletTemp ),
			BoilerOutletTemp( BoilerOutletTemp ),
			Mdot( Mdot ),
			ParasiticElecPower( ParasiticElecPower ),
			ParasiticElecConsumption( ParasiticElecConsumption ),
			BoilerPLR( BoilerPLR )
		{}

	};

	// Object Data
	extern Array1D< BoilerSpecs > Boiler; // boiler data - dimension to number of machines
	extern Array1D< ReportVars > BoilerReport; // report vars - dimension to number of machines

	// Functions

	void
	SimBoiler(
		std::string const & BoilerType, // boiler type (used in CASE statement)
		std::string const & BoilerName, // boiler identifier
		int const EquipFlowCtrl, // Flow control mode for the equipment
		int & CompIndex, // boiler counter/identifier
		bool const RunFlag, // if TRUE run boiler simulation--boiler is ON
		bool & InitLoopEquip, // If not zero, calculate the max load for operating conditions
		Real64 & MyLoad, // W - Actual demand boiler must satisfy--calculated by load dist. routine
		Real64 & MaxCap, // W - maximum boiler operating capacity
		Real64 & MinCap, // W - minimum boiler operating capacity
		Real64 & OptCap, // W - optimal boiler operating capacity
		bool const GetSizingFactor, // TRUE when just the sizing factor is requested
		Real64 & SizingFactor // sizing factor
	);

	void
	GetBoilerInput();

	void
	InitBoiler( int const BoilerNum ); // number of the current boiler being simulated

	void
	SizeBoiler( int const BoilerNum );

	void
	CalcBoilerModel(
		int & BoilerNum, // boiler identifier
		Real64 const MyLoad, // W - hot water demand to be met by boiler
		bool const RunFlag, // TRUE if boiler operating
		int const EquipFlowCtrl // Flow control mode for the equipment
	);

	// Beginning of Record Keeping subroutines for the BOILER:HOTWATER Module
	// *****************************************************************************

	void
	UpdateBoilerRecords(
		Real64 const MyLoad, // boiler operating load
		bool const RunFlag, // boiler on when TRUE
		int const Num // boiler number
	);

	// End of Record Keeping subroutines for the BOILER:HOTWATER Module
	// *****************************************************************************

	//     NOTICE

	//     Copyright (c) 1996-2014 The Board of Trustees of the University of Illinois
	//     and The Regents of the University of California through Ernest Orlando Lawrence
	//     Berkeley National Laboratory.  All rights reserved.

	//     Portions of the EnergyPlus software package have been developed and copyrighted
	//     by other individuals, companies and institutions.  These portions have been
	//     incorporated into the EnergyPlus software package under license.   For a complete
	//     list of contributors, see "Notice" located in main.cc.

	//     NOTICE: The U.S. Government is granted for itself and others acting on its
	//     behalf a paid-up, nonexclusive, irrevocable, worldwide license in this data to
	//     reproduce, prepare derivative works, and perform publicly and display publicly.
	//     Beginning five (5) years after permission to assert copyright is granted,
	//     subject to two possible five year renewals, the U.S. Government is granted for
	//     itself and others acting on its behalf a paid-up, non-exclusive, irrevocable
	//     worldwide license in this data to reproduce, prepare derivative works,
	//     distribute copies to the public, perform publicly and display publicly, and to
	//     permit others to do so.

	//     TRADEMARKS: EnergyPlus is a trademark of the US Department of Energy.

} // Boilers

} // EnergyPlus

#endif
