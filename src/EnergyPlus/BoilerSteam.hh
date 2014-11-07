#ifndef BoilerSteam_hh_INCLUDED
#define BoilerSteam_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/FArray1D.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>
#include <DataGlobals.hh>

namespace EnergyPlus {

namespace BoilerSteam {

	// Using/Aliasing

	// Data
	// MODULE PARAMETER DEFINITIONS
	// na

	// DERIVED TYPE DEFINITIONS

	// MODULE VARIABLE DECLARATIONS:
	extern Real64 FuelUsed; // W - Boiler fuel used
	extern Real64 BoilerLoad; // W - Boiler Load
	extern Real64 BoilerMassFlowRate; // kg/s - Boiler mass flow rate
	extern Real64 BoilerOutletTemp; // W - Boiler outlet temperature
	extern Real64 BoilerMaxPress;
	extern int NumBoilers; // Number of boilers
	extern Real64 BoilerMassFlowMaxAvail; // kg/s - Boiler mass flow rate
	extern Real64 BoilerMassFlowMinAvail; // kg/s - Boiler mass flow rate

	extern FArray1D_bool CheckEquipName;

	// SUBROUTINE SPECIFICATIONS FOR MODULE Boilers

	// Types

	struct BoilerSpecs
	{
		// Members
		std::string Name; // user identifier
		int FuelType; // resource type
		bool Available; // TRUE if machine available in current time step
		bool ON; // TRUE: simulate the machine at it's operating part load ratio
		bool MissingSetPointErrDone; // Missing outlet node setpoint message flag
		bool UseLoopSetPoint; // Flag to use setpoint from loop
		Real64 DesMassFlowRate; // kg/s - Boiler water design mass flow rate
		Real64 MassFlowRate; // kg/s - Boiler water mass flow rate
		Real64 NomCap; // W - design nominal capacity of Boiler
		Real64 Effic; // boiler efficiency at design conditions
		//       REAL(r64)         :: TempDesBoilerOut    =0.0d0      ! C - Boiler design outlet temperature
		Real64 MinPartLoadRat; // Minimum allowed operating part load ratio
		Real64 MaxPartLoadRat; // Maximum allowed operating part load ratio
		Real64 OptPartLoadRat; // Optimal operating part load ratio
		Real64 OperPartLoadRat; // Actual operating part load ratio
		Real64 TempUpLimitBoilerOut; // C - Boiler outlet maximum temperature limit
		Real64 BoilerMaxOperPress; // Max Boiler Pressure
		Real64 BoilerPressCheck; // Boiler Operating Pressure at Saturation Temperature
		Real64 SizFac; // sizing factor
		int BoilerInletNodeNum; // Node number at the boiler inlet
		int BoilerOutletNodeNum; // Node number at the boiler outlet
		FArray1D< Real64 > FullLoadCoef; // Coefficients of the fuel consumption/part load ratio curve
		int TypeNum; // Plant loop type identifier
		int LoopNum; // Plant loop index number
		int LoopSideNum; // Loop side index number
		int BranchNum; // Branch index number
		int CompNum; // Plant loop component index number
		int PressErrIndex; // index pointer for recurring errors
		int FluidIndex; // Steam index
		bool IsThisSized; // TRUE if sizing is done

		// Default Constructor
		BoilerSpecs() :
			FuelType( 0 ),
			Available( false ),
			ON( false ),
			MissingSetPointErrDone( false ),
			UseLoopSetPoint( false ),
			DesMassFlowRate( 0.0 ),
			MassFlowRate( 0.0 ),
			NomCap( 0.0 ),
			Effic( 0.0 ),
			MinPartLoadRat( 0.0 ),
			MaxPartLoadRat( 0.0 ),
			OptPartLoadRat( 0.0 ),
			OperPartLoadRat( 0.0 ),
			TempUpLimitBoilerOut( 0.0 ),
			BoilerMaxOperPress( 0.0 ),
			BoilerPressCheck( 0.0 ),
			SizFac( 0.0 ),
			BoilerInletNodeNum( 0 ),
			BoilerOutletNodeNum( 0 ),
			FullLoadCoef( 3, 0.0 ),
			TypeNum( 0 ),
			LoopNum( 0 ),
			LoopSideNum( 0 ),
			BranchNum( 0 ),
			CompNum( 0 ),
			PressErrIndex( 0 ),
			FluidIndex( 0 ),
			IsThisSized( false )
		{}

		// Member Constructor
		BoilerSpecs(
			std::string const & Name, // user identifier
			int const FuelType, // resource type
			bool const Available, // TRUE if machine available in current time step
			bool const ON, // TRUE: simulate the machine at it's operating part load ratio
			bool const MissingSetPointErrDone, // Missing outlet node setpoint message flag
			bool const UseLoopSetPoint, // Flag to use setpoint from loop
			Real64 const DesMassFlowRate, // kg/s - Boiler water design mass flow rate
			Real64 const MassFlowRate, // kg/s - Boiler water mass flow rate
			Real64 const NomCap, // W - design nominal capacity of Boiler
			Real64 const Effic, // boiler efficiency at design conditions
			Real64 const MinPartLoadRat, // Minimum allowed operating part load ratio
			Real64 const MaxPartLoadRat, // Maximum allowed operating part load ratio
			Real64 const OptPartLoadRat, // Optimal operating part load ratio
			Real64 const OperPartLoadRat, // Actual operating part load ratio
			Real64 const TempUpLimitBoilerOut, // C - Boiler outlet maximum temperature limit
			Real64 const BoilerMaxOperPress, // Max Boiler Pressure
			Real64 const BoilerPressCheck, // Boiler Operating Pressure at Saturation Temperature
			Real64 const SizFac, // sizing factor
			int const BoilerInletNodeNum, // Node number at the boiler inlet
			int const BoilerOutletNodeNum, // Node number at the boiler outlet
			FArray1< Real64 > const & FullLoadCoef, // Coefficients of the fuel consumption/part load ratio curve
			int const TypeNum, // Plant loop type identifier
			int const LoopNum, // Plant loop index number
			int const LoopSideNum, // Loop side index number
			int const BranchNum, // Branch index number
			int const CompNum, // Plant loop component index number
			int const PressErrIndex, // index pointer for recurring errors
			int const FluidIndex, // Steam index
			bool const IsThisSized // TRUE if sizing is done
		) :
			Name( Name ),
			FuelType( FuelType ),
			Available( Available ),
			ON( ON ),
			MissingSetPointErrDone( MissingSetPointErrDone ),
			UseLoopSetPoint( UseLoopSetPoint ),
			DesMassFlowRate( DesMassFlowRate ),
			MassFlowRate( MassFlowRate ),
			NomCap( NomCap ),
			Effic( Effic ),
			MinPartLoadRat( MinPartLoadRat ),
			MaxPartLoadRat( MaxPartLoadRat ),
			OptPartLoadRat( OptPartLoadRat ),
			OperPartLoadRat( OperPartLoadRat ),
			TempUpLimitBoilerOut( TempUpLimitBoilerOut ),
			BoilerMaxOperPress( BoilerMaxOperPress ),
			BoilerPressCheck( BoilerPressCheck ),
			SizFac( SizFac ),
			BoilerInletNodeNum( BoilerInletNodeNum ),
			BoilerOutletNodeNum( BoilerOutletNodeNum ),
			FullLoadCoef( 3, FullLoadCoef ),
			TypeNum( TypeNum ),
			LoopNum( LoopNum ),
			LoopSideNum( LoopSideNum ),
			BranchNum( BranchNum ),
			CompNum( CompNum ),
			PressErrIndex( PressErrIndex ),
			FluidIndex( FluidIndex ),
			IsThisSized( IsThisSized )
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
		Real64 BoilerMaxOperPress;

		// Default Constructor
		ReportVars() :
			BoilerLoad( 0.0 ),
			BoilerEnergy( 0.0 ),
			FuelUsed( 0.0 ),
			FuelConsumed( 0.0 ),
			BoilerInletTemp( 0.0 ),
			BoilerOutletTemp( 0.0 ),
			Mdot( 0.0 ),
			BoilerMaxOperPress( 0.0 )
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
			Real64 const BoilerMaxOperPress
		) :
			BoilerLoad( BoilerLoad ),
			BoilerEnergy( BoilerEnergy ),
			FuelUsed( FuelUsed ),
			FuelConsumed( FuelConsumed ),
			BoilerInletTemp( BoilerInletTemp ),
			BoilerOutletTemp( BoilerOutletTemp ),
			Mdot( Mdot ),
			BoilerMaxOperPress( BoilerMaxOperPress )
		{}

	};

	// Object Data
	extern FArray1D< BoilerSpecs > Boiler; // dimension to number of machines
	extern FArray1D< ReportVars > BoilerReport;

	// Functions

	void
	SimSteamBoiler(
		std::string const & BoilerType, // boiler type (used in CASE statement)
		std::string const & BoilerName, // boiler identifier
		int const EquipFlowCtrl, // Flow control mode for the equipment
		int & CompIndex, // boiler counter/identifier
		bool const RunFlag, // if TRUE run boiler simulation--boiler is ON
		bool const FirstHVACIteration, // TRUE if First iteration of simulation
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
	InitBoiler( int const BoilerNum ); // number of the current electric chiller being simulated

	void
	SizeBoiler( int const BoilerNum );

	void
	CalcBoilerModel(
		int & BoilerNum, // boiler identifier
		Real64 & MyLoad, // W - hot water demand to be met by boiler
		bool const RunFlag, // TRUE if boiler operating
		int const EquipFlowCtrl // Flow control mode for the equipment
	);

	// Beginning of Record Keeping subroutines for the BOILER:SIMPLE Module

	void
	UpdateBoilerRecords(
		Real64 const MyLoad, // boiler operating load
		bool const RunFlag, // boiler on when TRUE
		int const Num, // boiler number
		bool const FirstHVACIteration // TRUE if First iteration of simulation
	);

	// End of Record Keeping subroutines for the BOILER:STEAM Module

	//     NOTICE

	//     Copyright © 1996-2014 The Board of Trustees of the University of Illinois
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

} // BoilerSteam

} // EnergyPlus

#endif
