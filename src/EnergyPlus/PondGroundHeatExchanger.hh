#ifndef PondGroundHeatExchanger_hh_INCLUDED
#define PondGroundHeatExchanger_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>
#include <DataGlobals.hh>

namespace EnergyPlus {

namespace PondGroundHeatExchanger {

	// Using/Aliasing

	// Data
	// MODULE PARAMETER DEFINITIONS
	extern Real64 const SmallNum; // Very small number to avoid div0 errors
	extern Real64 const StefBoltzmann; // Stefan-Boltzmann constant
	//  REAL(r64), PARAMETER :: KelvinConv    = KelvinConv           ! Conversion from Celsius to Kelvin

	// DERIVED TYPE DEFINITIONS

	// MODULE VARIABLE DECLARATIONS:
	// utility variables initialized once
	extern int NumOfPondGHEs; // Number of pond ground heat exchangers
	// Utility variables - initialized for each instance of a pond
	extern Real64 InletTemp; // water inlet temperature
	extern Real64 OutletTemp; // water outlet temperature
	extern Real64 FlowRate; // water mass flow rate
	extern Real64 HeatTransRate; // total heat transfer rate, Watts
	extern Real64 PondTemp; // pond temperature
	extern Real64 PastPondTemp; // past pond temperature
	extern Real64 PondArea; // pond surface area
	extern Real64 PondDepth; // pond depth
	extern Real64 TubeInDiameter; // hydronic tube inside diameter
	extern Real64 TubeOutDiameter; // hydronic tube outside diameter
	extern Real64 TubeConductivity; // hydronic tube thermal conductivity
	extern Real64 GrndConductivity; // ground thermal conductivity
	extern Real64 Concentration; // fluid/glycol concentration 0.0-1.0 proportion.
	extern Real64 CircLength; // length of each circuit
	extern int NumCircuits; // number of circuits in total
	extern int InletNodeNum; // inlet node number
	extern int OutletNodeNum; // oulet node number
	extern int WaterIndex; // Fluid index for pond water
	extern bool NoDeepGroundTempObjWarning; // This will cause a warning to be issued if no "deep" ground
	// temperature object was input.
	extern Array1D_bool CheckEquipName;

	// SUBROUTINE SPECIFICATIONS FOR MODULE PlantPondGroundHeatExchangers

	// Types

	struct PondGroundHeatExchangerData
	{
		// Members
		// Input data
		std::string Name; // name of pond GHE
		std::string InletNode; // pond inlet fluid node
		std::string OutletNode; // pond outlet fluid node
		Real64 DesignMassFlowRate; // design flow rate of circulating fluid
		Real64 DesignCapacity; // design cooling capacity of pond at
		Real64 Depth; // depth of pond
		Real64 Area; // area of pond
		Real64 TubeInDiameter; // hydronic tube inside diameter
		Real64 TubeOutDiameter; // hydronic tube outside diameter
		Real64 TubeConductivity; // hydronic tube thermal conductivity
		Real64 GrndConductivity; // ground thermal conductivity
		Real64 CircuitLength; // length of each circuit
		Real64 BulkTemperature; // current pond bulk temperature
		Real64 PastBulkTemperature; // past pond bulk temperature
		int NumCircuits; // number of circuits in total
		int InletNodeNum; // inlet node number
		int OutletNodeNum; // oulet node number
		int FrozenErrIndex; // for recurring warnings
		int ConsecutiveFrozen; // count of time steps consecutive frozen
		//loop topology variables
		int LoopNum;
		int LoopSideNum;
		int BranchNum;
		int CompNum;

		// Default Constructor
		PondGroundHeatExchangerData() :
			DesignMassFlowRate( 0.0 ),
			DesignCapacity( 0.0 ),
			Depth( 0.0 ),
			Area( 0.0 ),
			TubeInDiameter( 0.0 ),
			TubeOutDiameter( 0.0 ),
			TubeConductivity( 0.0 ),
			GrndConductivity( 0.0 ),
			CircuitLength( 0.0 ),
			BulkTemperature( 0.0 ),
			PastBulkTemperature( 0.0 ),
			NumCircuits( 0 ),
			InletNodeNum( 0 ),
			OutletNodeNum( 0 ),
			FrozenErrIndex( 0 ),
			ConsecutiveFrozen( 0 ),
			LoopNum( 0 ),
			LoopSideNum( 0 ),
			BranchNum( 0 ),
			CompNum( 0 )
		{}

		// Member Constructor
		PondGroundHeatExchangerData(
			std::string const & Name, // name of pond GHE
			std::string const & InletNode, // pond inlet fluid node
			std::string const & OutletNode, // pond outlet fluid node
			Real64 const DesignMassFlowRate, // design flow rate of circulating fluid
			Real64 const DesignCapacity, // design cooling capacity of pond at
			Real64 const Depth, // depth of pond
			Real64 const Area, // area of pond
			Real64 const TubeInDiameter, // hydronic tube inside diameter
			Real64 const TubeOutDiameter, // hydronic tube outside diameter
			Real64 const TubeConductivity, // hydronic tube thermal conductivity
			Real64 const GrndConductivity, // ground thermal conductivity
			Real64 const CircuitLength, // length of each circuit
			Real64 const BulkTemperature, // current pond bulk temperature
			Real64 const PastBulkTemperature, // past pond bulk temperature
			int const NumCircuits, // number of circuits in total
			int const InletNodeNum, // inlet node number
			int const OutletNodeNum, // oulet node number
			int const FrozenErrIndex, // for recurring warnings
			int const ConsecutiveFrozen, // count of time steps consecutive frozen
			int const LoopNum,
			int const LoopSideNum,
			int const BranchNum,
			int const CompNum
		) :
			Name( Name ),
			InletNode( InletNode ),
			OutletNode( OutletNode ),
			DesignMassFlowRate( DesignMassFlowRate ),
			DesignCapacity( DesignCapacity ),
			Depth( Depth ),
			Area( Area ),
			TubeInDiameter( TubeInDiameter ),
			TubeOutDiameter( TubeOutDiameter ),
			TubeConductivity( TubeConductivity ),
			GrndConductivity( GrndConductivity ),
			CircuitLength( CircuitLength ),
			BulkTemperature( BulkTemperature ),
			PastBulkTemperature( PastBulkTemperature ),
			NumCircuits( NumCircuits ),
			InletNodeNum( InletNodeNum ),
			OutletNodeNum( OutletNodeNum ),
			FrozenErrIndex( FrozenErrIndex ),
			ConsecutiveFrozen( ConsecutiveFrozen ),
			LoopNum( LoopNum ),
			LoopSideNum( LoopSideNum ),
			BranchNum( BranchNum ),
			CompNum( CompNum )
		{}

	};

	struct PondGroundHeatExchangerReport
	{
		// Members
		// Report data
		Real64 InletTemp; // fluid inlet temperature
		Real64 OutletTemp; // fluid outlet temperature
		Real64 MassFlowRate; // fluid mass flow rate
		Real64 PondTemp; // pond bulk temperature
		Real64 HeatTransferRate; // total fluid heat transfer rate, Watts
		Real64 Energy; // cumulative energy, Joules

		// Default Constructor
		PondGroundHeatExchangerReport()
		{}

		// Member Constructor
		PondGroundHeatExchangerReport(
			Real64 const InletTemp, // fluid inlet temperature
			Real64 const OutletTemp, // fluid outlet temperature
			Real64 const MassFlowRate, // fluid mass flow rate
			Real64 const PondTemp, // pond bulk temperature
			Real64 const HeatTransferRate, // total fluid heat transfer rate, Watts
			Real64 const Energy // cumulative energy, Joules
		) :
			InletTemp( InletTemp ),
			OutletTemp( OutletTemp ),
			MassFlowRate( MassFlowRate ),
			PondTemp( PondTemp ),
			HeatTransferRate( HeatTransferRate ),
			Energy( Energy )
		{}

	};

	// Object Data
	extern Array1D< PondGroundHeatExchangerData > PondGHE;
	extern Array1D< PondGroundHeatExchangerReport > PondGHEReport;

	// Functions

	void
	SimPondGroundHeatExchanger(
		std::string const & CompName, // name of the pond GHE
		int & CompIndex, // index in local derived types
		bool const FirstHVACIteration, // TRUE if 1st HVAC simulation of system timestep
		bool const RunFlag, // TRUE if equipment turned on by loop operation scheme
		bool & InitLoopEquip,
		Real64 & MaxLoad,
		Real64 & MinLoad,
		Real64 & OptLoad
	);

	//==============================================================================

	void
	GetPondGroundHeatExchanger();

	//==============================================================================

	void
	InitPondGroundHeatExchanger(
		int const PondGHENum, // component number
		bool const FirstHVACIteration, // TRUE if 1st HVAC simulation of system timestep
		bool const RunFlag // TRUE if equipment scheduled to operate
	);

	//==============================================================================

	void
	CalcPondGroundHeatExchanger( int const PondGHENum ); // Number of the Pond GHE

	//==============================================================================

	Real64
	CalcTotalFLux(
		Real64 const PondBulkTemp, // pond temp for this flux calculation
		int const PondGHENum // Number of the Pond GHE
	);

	//==============================================================================

	Real64
	CalcSolarFlux();

	//==============================================================================

	Real64
	CalcEffectiveness(
		Real64 const InsideTemperature, // Temperature of fluid in pipe circuit, in C
		Real64 const PondTemperature, // Temperature of pond water (i.e. outside the pipe), in C
		Real64 const MassFlowRate, // Mass flow rate, in kg/s
		int const PondGHENum // Number of the Pond GHE
	);

	//==============================================================================

	void
	UpdatePondGroundHeatExchanger( int const PondGHENum ); // Index for the pond

	//==============================================================================

	void
	ReportPondGroundHeatExchanger( int const PondGHENum ); // Index for the pond under consideration

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

} // PondGroundHeatExchanger

} // EnergyPlus

#endif
