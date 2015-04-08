#ifndef HeatPumpWaterToWaterSimple_hh_INCLUDED
#define HeatPumpWaterToWaterSimple_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>
#include <DataGlobals.hh>

namespace EnergyPlus {

namespace HeatPumpWaterToWaterSimple {

	// Using/Aliasing

	// Data
	// MODULE PARAMETER DEFINITIONS
	extern std::string const HPEqFitHeating;
	extern std::string const HPEqFitHeatingUC;
	extern std::string const HPEqFitCooling;
	extern std::string const HPEqFitCoolingUC;

	// DERIVED TYPE DEFINITIONS

	// Type Description of Heat Pump

	// Output Variables Type definition

	// MODULE VARIABLE DECLARATIONS:
	extern int NumGSHPs; // Number of GSHPs specified in input

	// SUBROUTINE SPECIFICATIONS FOR MODULE

	// Driver/Manager Routines

	// Get Input routines for module

	// Initialization routines for module

	// Computational routines

	// Update routine to check convergence and update nodes

	// Types

	struct GshpSpecs
	{
		// Members
		std::string Name; // user identifier
		std::string WatertoWaterHPType; // Type of WatertoAirHP ie. Heating or Cooling
		int WWHPPlantTypeOfNum; // equipment type num
		bool Available; // need an array of logicals--load identifiers of available equipment
		bool ON; // simulate the machine at it's operating part load ratio
		bool IsOn; // flag that the heat pump is ON during current time step
		bool MustRun; // flag that the heat pump is MUST RUN during current time step
		Real64 SourceSideDesignMassFlow; // Design flow rate (kg/s)
		Real64 LoadSideDesignMassFlow; // Design flow rate (kg/s)
		Real64 RatedLoadVolFlowCool; // Rated Cooling Load Side Volumetric Flow Rate [m3/s]
		Real64 RatedSourceVolFlowCool; // Rated Cooling Source Side Volumetric Flow Rate [m3/s]
		Real64 RatedCapCool; // Rated Cooling Capacity [W]
		Real64 RatedPowerCool; // Rated Cooling Power Consumption[W]
		Real64 CoolCap1; // 1st coefficient of the Cooling capacity performance curve
		Real64 CoolCap2; // 2nd coefficient of the Cooling capacity performance curve
		Real64 CoolCap3; // 3rd coefficient of the Cooling capacity performance curve
		Real64 CoolCap4; // 4th coefficient of the Cooling capacity performance curve
		Real64 CoolCap5; // 5th coefficient of the Cooling capacity performance curve
		Real64 CoolPower1; // 1st coefficient of the Cooling power consumption curve
		Real64 CoolPower2; // 2nd coefficient of the Cooling power consumption curve
		Real64 CoolPower3; // 3rd coefficient of the Cooling power consumption curve
		Real64 CoolPower4; // 4th coefficient of the Cooling power consumption curve
		Real64 CoolPower5; // 5th coefficient of the Cooling power consumption curve
		int CoolCapNegativeCounter; // Counter for number of times cooling capacity curve is <= 0.0
		int CoolCapNegativeIndex; // Index for recurring warning message regarding cooling capacity curve is <= 0.0
		int CoolPowerNegativeCounter; // Counter for number of times cooling power curve is <= 0.0
		int CoolPowerNegativeIndex; // Index for recurring warning message regarding cooling power curve is <= 0.0
		Real64 RatedLoadVolFlowHeat; // Rated Heating Load Side Volumetric Flow Rate [m3/s]
		Real64 RatedSourceVolFlowHeat; // Rated Heating Source Side Volumetric Flow Rate [m3/s]
		Real64 RatedCapHeat; // Rated Heating Capacity [W]
		Real64 RatedPowerHeat; // Rated Heating Compressor Power[W]
		Real64 HeatCap1; // 1st coefficient of the Heating capacity performance curve
		Real64 HeatCap2; // 2nd coefficient of the Heating capacity performance curve
		Real64 HeatCap3; // 3rd coefficient of the Heating capacity performance curve
		Real64 HeatCap4; // 4th coefficient of the Heating capacity performance curve
		Real64 HeatCap5; // 5th coefficient of the Heating capacity performance curve
		Real64 HeatPower1; // 1st coefficient of the Heating power consumption curve
		Real64 HeatPower2; // 2nd coefficient of the Heating power consumption curve
		Real64 HeatPower3; // 3rd coefficient of the Heating power consumption curve
		Real64 HeatPower4; // 4th coefficient of the Heating power consumption curve
		Real64 HeatPower5; // 5th coefficient of the Heating power consumption curve
		int LoadSideInletNodeNum; // Load Side Inlet Node
		int LoadSideOutletNodeNum; // Load Side Outlet Node
		int SourceSideInletNodeNum; // Source Side Inlet Node
		int SourceSideOutletNodeNum; // Source Side Outlet Node
		int HeatCapNegativeCounter; // Counter for number of times heating capacity curve is <= 0.0
		int HeatCapNegativeIndex; // Index for recurring warning message regarding heating capacity curve is <= 0.0
		int HeatPowerNegativeCounter; // Counter for number of times heating power curve is <= 0.0
		int HeatPowerNegativeIndex; // Index for recurring warning message regarding heating power curve is <= 0.0
		//loop topology variables
		int SourceLoopNum; // source side plant loop index number
		int SourceLoopSideNum; // source side plant loop side index
		int SourceBranchNum; // source side plant loop branch index
		int SourceCompNum; // source side plant loop component index
		int LoadLoopNum; // load side plant loop index number
		int LoadLoopSideNum; // load side plant loop side index
		int LoadBranchNum; // load side plant loop branch index
		int LoadCompNum; // load side plant loop component index

		// Default Constructor
		GshpSpecs() :
			WWHPPlantTypeOfNum( 0 ),
			Available( false ),
			ON( false ),
			IsOn( false ),
			MustRun( false ),
			SourceSideDesignMassFlow( 0.0 ),
			LoadSideDesignMassFlow( 0.0 ),
			RatedLoadVolFlowCool( 0.0 ),
			RatedSourceVolFlowCool( 0.0 ),
			RatedCapCool( 0.0 ),
			RatedPowerCool( 0.0 ),
			CoolCap1( 0.0 ),
			CoolCap2( 0.0 ),
			CoolCap3( 0.0 ),
			CoolCap4( 0.0 ),
			CoolCap5( 0.0 ),
			CoolPower1( 0.0 ),
			CoolPower2( 0.0 ),
			CoolPower3( 0.0 ),
			CoolPower4( 0.0 ),
			CoolPower5( 0.0 ),
			CoolCapNegativeCounter( 0 ),
			CoolCapNegativeIndex( 0 ),
			CoolPowerNegativeCounter( 0 ),
			CoolPowerNegativeIndex( 0 ),
			RatedLoadVolFlowHeat( 0.0 ),
			RatedSourceVolFlowHeat( 0.0 ),
			RatedCapHeat( 0.0 ),
			RatedPowerHeat( 0.0 ),
			HeatCap1( 0.0 ),
			HeatCap2( 0.0 ),
			HeatCap3( 0.0 ),
			HeatCap4( 0.0 ),
			HeatCap5( 0.0 ),
			HeatPower1( 0.0 ),
			HeatPower2( 0.0 ),
			HeatPower3( 0.0 ),
			HeatPower4( 0.0 ),
			HeatPower5( 0.0 ),
			LoadSideInletNodeNum( 0 ),
			LoadSideOutletNodeNum( 0 ),
			SourceSideInletNodeNum( 0 ),
			SourceSideOutletNodeNum( 0 ),
			HeatCapNegativeCounter( 0 ),
			HeatCapNegativeIndex( 0 ),
			HeatPowerNegativeCounter( 0 ),
			HeatPowerNegativeIndex( 0 ),
			SourceLoopNum( 0 ),
			SourceLoopSideNum( 0 ),
			SourceBranchNum( 0 ),
			SourceCompNum( 0 ),
			LoadLoopNum( 0 ),
			LoadLoopSideNum( 0 ),
			LoadBranchNum( 0 ),
			LoadCompNum( 0 )
		{}

		// Member Constructor
		GshpSpecs(
			std::string const & Name, // user identifier
			std::string const & WatertoWaterHPType, // Type of WatertoAirHP ie. Heating or Cooling
			int const WWHPPlantTypeOfNum, // equipment type num
			bool const Available, // need an array of logicals--load identifiers of available equipment
			bool const ON, // simulate the machine at it's operating part load ratio
			bool const IsOn, // flag that the heat pump is ON during current time step
			bool const MustRun, // flag that the heat pump is MUST RUN during current time step
			Real64 const SourceSideDesignMassFlow, // Design flow rate (kg/s)
			Real64 const LoadSideDesignMassFlow, // Design flow rate (kg/s)
			Real64 const RatedLoadVolFlowCool, // Rated Cooling Load Side Volumetric Flow Rate [m3/s]
			Real64 const RatedSourceVolFlowCool, // Rated Cooling Source Side Volumetric Flow Rate [m3/s]
			Real64 const RatedCapCool, // Rated Cooling Capacity [W]
			Real64 const RatedPowerCool, // Rated Cooling Power Consumption[W]
			Real64 const CoolCap1, // 1st coefficient of the Cooling capacity performance curve
			Real64 const CoolCap2, // 2nd coefficient of the Cooling capacity performance curve
			Real64 const CoolCap3, // 3rd coefficient of the Cooling capacity performance curve
			Real64 const CoolCap4, // 4th coefficient of the Cooling capacity performance curve
			Real64 const CoolCap5, // 5th coefficient of the Cooling capacity performance curve
			Real64 const CoolPower1, // 1st coefficient of the Cooling power consumption curve
			Real64 const CoolPower2, // 2nd coefficient of the Cooling power consumption curve
			Real64 const CoolPower3, // 3rd coefficient of the Cooling power consumption curve
			Real64 const CoolPower4, // 4th coefficient of the Cooling power consumption curve
			Real64 const CoolPower5, // 5th coefficient of the Cooling power consumption curve
			int const CoolCapNegativeCounter, // Counter for number of times cooling capacity curve is <= 0.0
			int const CoolCapNegativeIndex, // Index for recurring warning message regarding cooling capacity curve is <= 0.0
			int const CoolPowerNegativeCounter, // Counter for number of times cooling power curve is <= 0.0
			int const CoolPowerNegativeIndex, // Index for recurring warning message regarding cooling power curve is <= 0.0
			Real64 const RatedLoadVolFlowHeat, // Rated Heating Load Side Volumetric Flow Rate [m3/s]
			Real64 const RatedSourceVolFlowHeat, // Rated Heating Source Side Volumetric Flow Rate [m3/s]
			Real64 const RatedCapHeat, // Rated Heating Capacity [W]
			Real64 const RatedPowerHeat, // Rated Heating Compressor Power[W]
			Real64 const HeatCap1, // 1st coefficient of the Heating capacity performance curve
			Real64 const HeatCap2, // 2nd coefficient of the Heating capacity performance curve
			Real64 const HeatCap3, // 3rd coefficient of the Heating capacity performance curve
			Real64 const HeatCap4, // 4th coefficient of the Heating capacity performance curve
			Real64 const HeatCap5, // 5th coefficient of the Heating capacity performance curve
			Real64 const HeatPower1, // 1st coefficient of the Heating power consumption curve
			Real64 const HeatPower2, // 2nd coefficient of the Heating power consumption curve
			Real64 const HeatPower3, // 3rd coefficient of the Heating power consumption curve
			Real64 const HeatPower4, // 4th coefficient of the Heating power consumption curve
			Real64 const HeatPower5, // 5th coefficient of the Heating power consumption curve
			int const LoadSideInletNodeNum, // Load Side Inlet Node
			int const LoadSideOutletNodeNum, // Load Side Outlet Node
			int const SourceSideInletNodeNum, // Source Side Inlet Node
			int const SourceSideOutletNodeNum, // Source Side Outlet Node
			int const HeatCapNegativeCounter, // Counter for number of times heating capacity curve is <= 0.0
			int const HeatCapNegativeIndex, // Index for recurring warning message regarding heating capacity curve is <= 0.0
			int const HeatPowerNegativeCounter, // Counter for number of times heating power curve is <= 0.0
			int const HeatPowerNegativeIndex, // Index for recurring warning message regarding heating power curve is <= 0.0
			int const SourceLoopNum, // source side plant loop index number
			int const SourceLoopSideNum, // source side plant loop side index
			int const SourceBranchNum, // source side plant loop branch index
			int const SourceCompNum, // source side plant loop component index
			int const LoadLoopNum, // load side plant loop index number
			int const LoadLoopSideNum, // load side plant loop side index
			int const LoadBranchNum, // load side plant loop branch index
			int const LoadCompNum // load side plant loop component index
		) :
			Name( Name ),
			WatertoWaterHPType( WatertoWaterHPType ),
			WWHPPlantTypeOfNum( WWHPPlantTypeOfNum ),
			Available( Available ),
			ON( ON ),
			IsOn( IsOn ),
			MustRun( MustRun ),
			SourceSideDesignMassFlow( SourceSideDesignMassFlow ),
			LoadSideDesignMassFlow( LoadSideDesignMassFlow ),
			RatedLoadVolFlowCool( RatedLoadVolFlowCool ),
			RatedSourceVolFlowCool( RatedSourceVolFlowCool ),
			RatedCapCool( RatedCapCool ),
			RatedPowerCool( RatedPowerCool ),
			CoolCap1( CoolCap1 ),
			CoolCap2( CoolCap2 ),
			CoolCap3( CoolCap3 ),
			CoolCap4( CoolCap4 ),
			CoolCap5( CoolCap5 ),
			CoolPower1( CoolPower1 ),
			CoolPower2( CoolPower2 ),
			CoolPower3( CoolPower3 ),
			CoolPower4( CoolPower4 ),
			CoolPower5( CoolPower5 ),
			CoolCapNegativeCounter( CoolCapNegativeCounter ),
			CoolCapNegativeIndex( CoolCapNegativeIndex ),
			CoolPowerNegativeCounter( CoolPowerNegativeCounter ),
			CoolPowerNegativeIndex( CoolPowerNegativeIndex ),
			RatedLoadVolFlowHeat( RatedLoadVolFlowHeat ),
			RatedSourceVolFlowHeat( RatedSourceVolFlowHeat ),
			RatedCapHeat( RatedCapHeat ),
			RatedPowerHeat( RatedPowerHeat ),
			HeatCap1( HeatCap1 ),
			HeatCap2( HeatCap2 ),
			HeatCap3( HeatCap3 ),
			HeatCap4( HeatCap4 ),
			HeatCap5( HeatCap5 ),
			HeatPower1( HeatPower1 ),
			HeatPower2( HeatPower2 ),
			HeatPower3( HeatPower3 ),
			HeatPower4( HeatPower4 ),
			HeatPower5( HeatPower5 ),
			LoadSideInletNodeNum( LoadSideInletNodeNum ),
			LoadSideOutletNodeNum( LoadSideOutletNodeNum ),
			SourceSideInletNodeNum( SourceSideInletNodeNum ),
			SourceSideOutletNodeNum( SourceSideOutletNodeNum ),
			HeatCapNegativeCounter( HeatCapNegativeCounter ),
			HeatCapNegativeIndex( HeatCapNegativeIndex ),
			HeatPowerNegativeCounter( HeatPowerNegativeCounter ),
			HeatPowerNegativeIndex( HeatPowerNegativeIndex ),
			SourceLoopNum( SourceLoopNum ),
			SourceLoopSideNum( SourceLoopSideNum ),
			SourceBranchNum( SourceBranchNum ),
			SourceCompNum( SourceCompNum ),
			LoadLoopNum( LoadLoopNum ),
			LoadLoopSideNum( LoadLoopSideNum ),
			LoadBranchNum( LoadBranchNum ),
			LoadCompNum( LoadCompNum )
		{}

	};

	struct ReportVars
	{
		// Members
		Real64 Power; // Power Consumption [W]
		Real64 Energy; // Energy Consumption [J]
		Real64 QLoad; // Load Side Heat Transfer Rate [W]
		Real64 QLoadEnergy; // Load Side Heat Transfer [J]
		Real64 QSource; // Source Side Heat Transfer Rate [W]
		Real64 QSourceEnergy; // Source Side Heat Transfer [J]
		Real64 LoadSideMassFlowRate; // Load side volumetric flow rate m3/s
		Real64 LoadSideInletTemp; // Load Side outlet temperature °C
		Real64 LoadSideOutletTemp; // Load Side outlet temperature °C
		Real64 SourceSideMassFlowRate; // Source side volumetric flow rate m3/s
		Real64 SourceSideInletTemp; // Source Side outlet temperature °C
		Real64 SourceSideOutletTemp; // Source Side outlet temperature °C

		// Default Constructor
		ReportVars() :
			Power( 0.0 ),
			Energy( 0.0 ),
			QLoad( 0.0 ),
			QLoadEnergy( 0.0 ),
			QSource( 0.0 ),
			QSourceEnergy( 0.0 ),
			LoadSideMassFlowRate( 0.0 ),
			LoadSideInletTemp( 0.0 ),
			LoadSideOutletTemp( 0.0 ),
			SourceSideMassFlowRate( 0.0 ),
			SourceSideInletTemp( 0.0 ),
			SourceSideOutletTemp( 0.0 )
		{}

		// Member Constructor
		ReportVars(
			Real64 const Power, // Power Consumption [W]
			Real64 const Energy, // Energy Consumption [J]
			Real64 const QLoad, // Load Side Heat Transfer Rate [W]
			Real64 const QLoadEnergy, // Load Side Heat Transfer [J]
			Real64 const QSource, // Source Side Heat Transfer Rate [W]
			Real64 const QSourceEnergy, // Source Side Heat Transfer [J]
			Real64 const LoadSideMassFlowRate, // Load side volumetric flow rate m3/s
			Real64 const LoadSideInletTemp, // Load Side outlet temperature °C
			Real64 const LoadSideOutletTemp, // Load Side outlet temperature °C
			Real64 const SourceSideMassFlowRate, // Source side volumetric flow rate m3/s
			Real64 const SourceSideInletTemp, // Source Side outlet temperature °C
			Real64 const SourceSideOutletTemp // Source Side outlet temperature °C
		) :
			Power( Power ),
			Energy( Energy ),
			QLoad( QLoad ),
			QLoadEnergy( QLoadEnergy ),
			QSource( QSource ),
			QSourceEnergy( QSourceEnergy ),
			LoadSideMassFlowRate( LoadSideMassFlowRate ),
			LoadSideInletTemp( LoadSideInletTemp ),
			LoadSideOutletTemp( LoadSideOutletTemp ),
			SourceSideMassFlowRate( SourceSideMassFlowRate ),
			SourceSideInletTemp( SourceSideInletTemp ),
			SourceSideOutletTemp( SourceSideOutletTemp )
		{}

	};

	// Object Data
	extern Array1D< GshpSpecs > GSHP;
	extern Array1D< ReportVars > GSHPReport;

	// Functions

	void
	SimHPWatertoWaterSimple(
		std::string const & GSHPType, // Type of GSHP
		int const GSHPTypeNum, // Type of GSHP in Plant equipment
		std::string const & GSHPName, // User Specified Name of GSHP
		int & GSHPNum, // Index of Equipment
		bool const FirstHVACIteration,
		bool & InitLoopEquip, // If not zero, calculate the max load for operating conditions
		Real64 const MyLoad, // Loop demand component will meet
		Real64 & MaxCap, // Maximum operating capacity of GSHP [W]
		Real64 & MinCap, // Minimum operating capacity of GSHP [W]
		Real64 & OptCap, // Optimal operating capacity of GSHP [W]
		int const LoopNum // The calling loop number
	);

	void
	GetWatertoWaterHPInput();

	void
	InitWatertoWaterHP(
		int const GSHPTypeNum, // Type of GSHP
		std::string const & GSHPName, // User Specified Name of GSHP
		int const GSHPNum, // GSHP Number
		bool const FirstHVACIteration,
		Real64 const MyLoad // Demand Load
	);

	void
	CalcWatertoWaterHPCooling(
		int const GSHPNum, // GSHP Number
		Real64 const MyLoad // Operating Load
	);

	void
	CalcWatertoWaterHPHeating(
		int const GSHPNum, // GSHP Number
		Real64 const MyLoad // Operating Load
	);

	void
	UpdateGSHPRecords( int const GSHPNum ); // GSHP number

} // HeatPumpWaterToWaterSimple

} // EnergyPlus

#endif
