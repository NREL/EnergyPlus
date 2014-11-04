#ifndef PlantHeatExchangerFluidToFluid_hh_INCLUDED
#define PlantHeatExchangerFluidToFluid_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/FArray1D.hh>
#include <ObjexxFCL/FArray1S.hh>
#include <ObjexxFCL/Optional.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>
#include <DataGlobals.hh>

namespace EnergyPlus {

namespace PlantHeatExchangerFluidToFluid {

	// Using/Aliasing

	// Data
	// MODULE PARAMETER DEFINITIONS:
	extern int const CrossFlowBothUnMixed;
	extern int const CrossFlowBothMixed;
	extern int const CrossFlowSupplyLoopMixedDemandLoopUnMixed;
	extern int const CrossFlowSupplyLoopUnMixedDemandLoopMixed;
	extern int const CounterFlow;
	extern int const ParallelFlow;
	extern int const Ideal;

	extern int const UncontrolledOn;
	extern int const OperationSchemeModulated;
	extern int const OperationSchemeOnOff;
	extern int const HeatingSetPointModulated;
	extern int const HeatingSetPointOnOff;
	extern int const CoolingSetPointModulated;
	extern int const CoolingSetPointOnOff;
	extern int const DualDeadBandSetPointModulated;
	extern int const DualDeadBandSetPointOnOff;
	extern int const CoolingDifferentialOnOff;
	extern int const CoolingSetPointOnOffWithComponentOverride;
	extern int const TrackComponentOnOff;

	extern int const WetBulbTemperature;
	extern int const DryBulbTemperature;
	extern int const LoopTemperature;

	extern int const HeatingSupplySideLoop;
	extern int const CoolingSupplySideLoop;

	// DERIVED TYPE DEFINITIONS:

	// MODULE VARIABLE DECLARATIONS:
	extern std::string ComponentClassName;
	extern int NumberOfPlantFluidHXs;
	extern bool GetInput;
	extern FArray1D_bool CheckFluidHXs;

	// SUBROUTINE SPECIFICATIONS FOR MODULE

	// Types

	struct PlantConnectionStruct
	{
		// Members
		int LoopNum; // plant loop connection index
		int LoopSideNum; // plant loop side connection index
		int BranchNum; // plant loop branch connection index
		int CompNum; // plant loop component connection index
		int InletNodeNum; // plant loop inlet node index
		int OutletNodeNum; // plant loop outlet node index
		Real64 MassFlowRateMin; // minimum (hardware) flow rate for component [kg/s]
		Real64 MassFlowRateMax; // maximum (hardware) flow rate for component [kg/s]
		Real64 DesignVolumeFlowRate; // design flow rate [m3/s]
		Real64 MyLoad; // current load request of supply equip for op scheme control[W]
		Real64 MinLoad; // reports back size for load dispatch routines [W]
		Real64 MaxLoad; // reports back size for load dispatch [W]
		Real64 OptLoad; // reports back size for load dispatch [W]
		Real64 InletTemp; // current inlet fluid temperature [C]
		Real64 InletMassFlowRate; // current inlet mass flow rate [kg/s]
		Real64 OutletTemp; // componenent outlet temperature [C]

		// Default Constructor
		PlantConnectionStruct() :
			LoopNum( 0 ),
			LoopSideNum( 0 ),
			BranchNum( 0 ),
			CompNum( 0 ),
			InletNodeNum( 0 ),
			OutletNodeNum( 0 ),
			MassFlowRateMin( 0.0 ),
			MassFlowRateMax( 0.0 ),
			DesignVolumeFlowRate( 0.0 ),
			MyLoad( 0.0 ),
			MinLoad( 0.0 ),
			MaxLoad( 0.0 ),
			OptLoad( 0.0 ),
			InletTemp( 0.0 ),
			InletMassFlowRate( 0.0 ),
			OutletTemp( 0.0 )
		{}

		// Member Constructor
		PlantConnectionStruct(
			int const LoopNum, // plant loop connection index
			int const LoopSideNum, // plant loop side connection index
			int const BranchNum, // plant loop branch connection index
			int const CompNum, // plant loop component connection index
			int const InletNodeNum, // plant loop inlet node index
			int const OutletNodeNum, // plant loop outlet node index
			Real64 const MassFlowRateMin, // minimum (hardware) flow rate for component [kg/s]
			Real64 const MassFlowRateMax, // maximum (hardware) flow rate for component [kg/s]
			Real64 const DesignVolumeFlowRate, // design flow rate [m3/s]
			Real64 const MyLoad, // current load request of supply equip for op scheme control[W]
			Real64 const MinLoad, // reports back size for load dispatch routines [W]
			Real64 const MaxLoad, // reports back size for load dispatch [W]
			Real64 const OptLoad, // reports back size for load dispatch [W]
			Real64 const InletTemp, // current inlet fluid temperature [C]
			Real64 const InletMassFlowRate, // current inlet mass flow rate [kg/s]
			Real64 const OutletTemp // componenent outlet temperature [C]
		) :
			LoopNum( LoopNum ),
			LoopSideNum( LoopSideNum ),
			BranchNum( BranchNum ),
			CompNum( CompNum ),
			InletNodeNum( InletNodeNum ),
			OutletNodeNum( OutletNodeNum ),
			MassFlowRateMin( MassFlowRateMin ),
			MassFlowRateMax( MassFlowRateMax ),
			DesignVolumeFlowRate( DesignVolumeFlowRate ),
			MyLoad( MyLoad ),
			MinLoad( MinLoad ),
			MaxLoad( MaxLoad ),
			OptLoad( OptLoad ),
			InletTemp( InletTemp ),
			InletMassFlowRate( InletMassFlowRate ),
			OutletTemp( OutletTemp )
		{}

	};

	struct PlantLocatorStruct
	{
		// Members
		int LoopNum; // plant loop connection index
		int LoopSideNum; // plant loop side connection index
		int BranchNum; // plant loop branch connection index
		int CompNum; // plant loop component connection index
		int InletNodeNum; // plant loop inlet node index

		// Default Constructor
		PlantLocatorStruct() :
			LoopNum( 0 ),
			LoopSideNum( 0 ),
			BranchNum( 0 ),
			CompNum( 0 ),
			InletNodeNum( 0 )
		{}

		// Member Constructor
		PlantLocatorStruct(
			int const LoopNum, // plant loop connection index
			int const LoopSideNum, // plant loop side connection index
			int const BranchNum, // plant loop branch connection index
			int const CompNum, // plant loop component connection index
			int const InletNodeNum // plant loop inlet node index
		) :
			LoopNum( LoopNum ),
			LoopSideNum( LoopSideNum ),
			BranchNum( BranchNum ),
			CompNum( CompNum ),
			InletNodeNum( InletNodeNum )
		{}

	};

	struct HeatExchangerStruct
	{
		// Members
		std::string Name;
		int AvailSchedNum;
		int HeatExchangeModelType;
		Real64 UA;
		int ControlMode;
		int SetPointNodeNum;
		Real64 TempControlTol;
		int ControlSignalTemp;
		Real64 MinOperationTemp;
		Real64 MaxOperationTemp;
		PlantConnectionStruct DemandSideLoop; // plant connections and data for the side of HX connected to demand side
		PlantConnectionStruct SupplySideLoop;
		std::string HeatTransferMeteringEndUse;
		std::string ComponentUserName; // user name for control-associated  component
		std::string ComponentClassName; // object class name for control-associated component
		int ComponentTypeOfNum;
		PlantLocatorStruct OtherCompSupplySideLoop;
		PlantLocatorStruct OtherCompDemandSideLoop;
		Real64 SizingFactor;
		Real64 HeatTransferRate;
		Real64 HeatTransferEnergy;
		Real64 Effectiveness;
		Real64 OperationStatus;
		int DmdSideModulatSolvNoConvergeErrorCount;
		int DmdSideModulatSolvNoConvergeErrorIndex;
		int DmdSideModulatSolvFailErrorCount;
		int DmdSideModulatSolvFailErrorIndex;

		// Default Constructor
		HeatExchangerStruct() :
			AvailSchedNum( 0 ),
			HeatExchangeModelType( 0 ),
			UA( 0.0 ),
			ControlMode( 0 ),
			SetPointNodeNum( 0 ),
			TempControlTol( 0.0 ),
			ControlSignalTemp( 0 ),
			MinOperationTemp( -99999.0 ),
			MaxOperationTemp( 99999.0 ),
			ComponentTypeOfNum( 0 ),
			SizingFactor( 1.0 ),
			HeatTransferRate( 0.0 ),
			HeatTransferEnergy( 0.0 ),
			Effectiveness( 0.0 ),
			OperationStatus( 0.0 ),
			DmdSideModulatSolvNoConvergeErrorCount( 0 ),
			DmdSideModulatSolvNoConvergeErrorIndex( 0 ),
			DmdSideModulatSolvFailErrorCount( 0 ),
			DmdSideModulatSolvFailErrorIndex( 0 )
		{}

		// Member Constructor
		HeatExchangerStruct(
			std::string const & Name,
			int const AvailSchedNum,
			int const HeatExchangeModelType,
			Real64 const UA,
			int const ControlMode,
			int const SetPointNodeNum,
			Real64 const TempControlTol,
			int const ControlSignalTemp,
			Real64 const MinOperationTemp,
			Real64 const MaxOperationTemp,
			PlantConnectionStruct const & DemandSideLoop, // plant connections and data for the side of HX connected to demand side
			PlantConnectionStruct const & SupplySideLoop,
			std::string const & HeatTransferMeteringEndUse,
			std::string const & ComponentUserName, // user name for control-associated  component
			std::string const & ComponentClassName, // object class name for control-associated component
			int const ComponentTypeOfNum,
			PlantLocatorStruct const & OtherCompSupplySideLoop,
			PlantLocatorStruct const & OtherCompDemandSideLoop,
			Real64 const SizingFactor,
			Real64 const HeatTransferRate,
			Real64 const HeatTransferEnergy,
			Real64 const Effectiveness,
			Real64 const OperationStatus,
			int const DmdSideModulatSolvNoConvergeErrorCount,
			int const DmdSideModulatSolvNoConvergeErrorIndex,
			int const DmdSideModulatSolvFailErrorCount,
			int const DmdSideModulatSolvFailErrorIndex
		) :
			Name( Name ),
			AvailSchedNum( AvailSchedNum ),
			HeatExchangeModelType( HeatExchangeModelType ),
			UA( UA ),
			ControlMode( ControlMode ),
			SetPointNodeNum( SetPointNodeNum ),
			TempControlTol( TempControlTol ),
			ControlSignalTemp( ControlSignalTemp ),
			MinOperationTemp( MinOperationTemp ),
			MaxOperationTemp( MaxOperationTemp ),
			DemandSideLoop( DemandSideLoop ),
			SupplySideLoop( SupplySideLoop ),
			HeatTransferMeteringEndUse( HeatTransferMeteringEndUse ),
			ComponentUserName( ComponentUserName ),
			ComponentClassName( ComponentClassName ),
			ComponentTypeOfNum( ComponentTypeOfNum ),
			OtherCompSupplySideLoop( OtherCompSupplySideLoop ),
			OtherCompDemandSideLoop( OtherCompDemandSideLoop ),
			SizingFactor( SizingFactor ),
			HeatTransferRate( HeatTransferRate ),
			HeatTransferEnergy( HeatTransferEnergy ),
			Effectiveness( Effectiveness ),
			OperationStatus( OperationStatus ),
			DmdSideModulatSolvNoConvergeErrorCount( DmdSideModulatSolvNoConvergeErrorCount ),
			DmdSideModulatSolvNoConvergeErrorIndex( DmdSideModulatSolvNoConvergeErrorIndex ),
			DmdSideModulatSolvFailErrorCount( DmdSideModulatSolvFailErrorCount ),
			DmdSideModulatSolvFailErrorIndex( DmdSideModulatSolvFailErrorIndex )
		{}

	};

	// Object Data
	extern FArray1D< HeatExchangerStruct > FluidHX;

	// Functions

	void
	SimFluidHeatExchanger(
		int const LoopNum, // plant loop sim call originated from
		int const LoopSideNum, // plant loop side sim call originated from
		std::string const & EquipType, // type of equipment, 'PlantComponent:UserDefined'
		std::string const & EquipName, // user name for component
		int & CompIndex,
		bool & InitLoopEquip,
		Real64 const MyLoad,
		Real64 & MaxCap,
		Real64 & MinCap,
		Real64 & OptCap
	);

	void
	GetFluidHeatExchangerInput();

	void
	InitFluidHeatExchanger(
		int const CompNum,
		int const LoopNum
	);

	void
	SizeFluidHeatExchanger( int const CompNum );

	void
	ControlFluidHeatExchanger(
		int const CompNum,
		int const LoopNum,
		Real64 const MyLoad
	);

	void
	CalcFluidHeatExchanger(
		int const CompNum,
		Real64 const SupSideMdot, // mass flow rate of fluid entering from supply side loop
		Real64 const DmdSideMdot // mass flow rate of fluid entering from demand side loop
	);

	void
	FindHXDemandSideLoopFlow(
		int const CompNum,
		Real64 const TargetSupplySideLoopLeavingTemp,
		int const HXActionMode
	);

	Real64
	HXDemandSideLoopFlowResidual(
		Real64 const DmdSideMassFlowRate,
		Optional< FArray1S< Real64 > const > Par = _ // Par(1) = HX index number
	);

	void
	UpdateFluidHeatExchanger( int const CompNum );

	void
	ReportFluidHeatExchanger( int const CompNum );

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

} // PlantHeatExchangerFluidToFluid

} // EnergyPlus

#endif
