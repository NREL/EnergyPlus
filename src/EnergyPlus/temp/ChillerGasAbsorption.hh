#ifndef ChillerGasAbsorption_hh_INCLUDED
#define ChillerGasAbsorption_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/FArray1D.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>
#include <DataGlobals.hh>

namespace EnergyPlus {

namespace ChillerGasAbsorption {

	// Using/Aliasing

	// Data
	//MODULE PARAMETER DEFINITIONS:
	// na

	// MODULE VARIABLE DECLARATIONS:
	extern int NumGasAbsorbers; // number of Absorption Chillers specified in input

	// This type holds the output from the algorithm i.e., the Report Variables

	extern FArray1D_bool CheckEquipName;

	// SUBROUTINE SPECIFICATIONS FOR MODULE PrimaryPlantLoops

	// Types

	struct GasAbsorberSpecs
	{
		// Members
		// Parts of Type that do not correspond with IDD definition
		bool Available; // need an array of logicals--load identifiers of available equipment
		bool ON; // simulate the machine at it's operating part load ratio
		bool InCoolingMode;
		bool InHeatingMode;
		// Part of Type that directly corresponds with IDD definition
		std::string Name; // user identifier
		std::string FuelType; // Type of Fuel - DIESEL, GASOLINE, GAS
		Real64 NomCoolingCap; // W - design nominal capacity of Absorber
		Real64 NomHeatCoolRatio; // ratio of heating to cooling capacity
		Real64 FuelCoolRatio; // ratio of fuel input to cooling output
		Real64 FuelHeatRatio; // ratio of fuel input to heating output
		Real64 ElecCoolRatio; // ratio of electricity input to cooling output
		Real64 ElecHeatRatio; // ratio of electricity input to heating output
		int ChillReturnNodeNum; // Node number on the inlet side of the plant
		int ChillSupplyNodeNum; // Node number on the outlet side of the plant
		bool ChillSetPointErrDone; // flag to report missing setpoint on CW outlet
		bool ChillSetPointSetToLoop; // flag to use overall loop setpoint
		int CondReturnNodeNum; // Node number on the inlet side of the condenser
		int CondSupplyNodeNum; // Node number on the outlet side of the condenser
		int HeatReturnNodeNum; // absorber steam inlet node number, water side
		int HeatSupplyNodeNum; // absorber steam outlet node number, water side
		bool HeatSetPointErrDone; // flag to report missing setpoint on HW outlet
		bool HeatSetPointSetToLoop; // flag to use overall loop setpoint
		Real64 MinPartLoadRat; // min allowed operating frac full load
		Real64 MaxPartLoadRat; // max allowed operating frac full load
		Real64 OptPartLoadRat; // optimal operating frac full load
		Real64 TempDesCondReturn; // design secondary loop fluid temperature at the Absorber condenser side inlet
		Real64 TempDesCHWSupply; // design chilled water supply temperature
		Real64 EvapVolFlowRate; // m**3/s - design nominal water volumetric flow rate through the evaporator
		Real64 CondVolFlowRate; // m**3/s - design nominal water volumetric flow rate through the condenser
		Real64 HeatVolFlowRate; // m**3/s - design nominal water volumetric flow rate through the heater side
		Real64 SizFac; // sizing factor
		int CoolCapFTCurve; // cooling capacity as a function of temperature curve (chilled water temp,
		// condenser water temp)
		int FuelCoolFTCurve; // Fuel-Input-to cooling output Ratio Function of Temperature Curve (chilled
		// water temp, condenser water temp)
		int FuelCoolFPLRCurve; // Fuel-Input-to cooling output Ratio Function of Part Load Ratio Curve
		int ElecCoolFTCurve; // Electric-Input-to cooling output Ratio Function of Temperature Curve
		// (chilled water temp, condenser water temp)
		int ElecCoolFPLRCurve; // Electric-Input-to cooling output Ratio Function of Part Load Ratio Curve
		int HeatCapFCoolCurve; // Heating Capacity Function of Cooling Capacity Curve
		int FuelHeatFHPLRCurve; // Fuel Input to heat output ratio during heating only function
		bool isEnterCondensTemp; // if using entering conderser water temperature is TRUE, exiting is FALSE
		bool isWaterCooled; // if water cooled it is TRUE
		Real64 CHWLowLimitTemp; // Chilled Water Lower Limit Temperature
		Real64 FuelHeatingValue;
		// Calculated design values
		Real64 DesCondMassFlowRate; // design nominal mass flow rate of water through the condenser [kg/s]
		Real64 DesHeatMassFlowRate; // design nominal mass flow rate of water through the hot water side [kg/s]
		Real64 DesEvapMassFlowRate; // design nominal mass flow rate of water through chilled water side [kg/s]
		// other values used during simulation
		int DeltaTempCoolErrCount; // error count for Delta Temp = 0 while cooling
		int DeltaTempHeatErrCount; // error count for Delta Temp = 0 while heating
		int CondErrCount; // error count for poor Condenser Supply Estimate
		bool PossibleSubcooling; // Flag to determine whether plant is overcooled
		//loop topology variables
		int CWLoopNum; // chilled water plant loop index number
		int CWLoopSideNum; // chilled water plant loop side index
		int CWBranchNum; // chilled water plant loop branch index
		int CWCompNum; // chilled water plant loop component index
		int CDLoopNum; // condenser water plant loop index number
		int CDLoopSideNum; // condenser water plant loop side index
		int CDBranchNum; // condenser water plant loop branch index
		int CDCompNum; // condenser water plant loop component index
		int HWLoopNum; // hot water plant loop side index
		int HWLoopSideNum; // hot water plant loop side index
		int HWBranchNum; // hot water plant loop branch index
		int HWCompNum; // hot water plant loop component index
		bool IsThisSized; // TRUE if sizing is done

		// Default Constructor
		GasAbsorberSpecs() :
			Available( false ),
			ON( false ),
			InCoolingMode( false ),
			InHeatingMode( false ),
			NomCoolingCap( 0.0 ),
			NomHeatCoolRatio( 0.0 ),
			FuelCoolRatio( 0.0 ),
			FuelHeatRatio( 0.0 ),
			ElecCoolRatio( 0.0 ),
			ElecHeatRatio( 0.0 ),
			ChillReturnNodeNum( 0 ),
			ChillSupplyNodeNum( 0 ),
			ChillSetPointErrDone( false ),
			ChillSetPointSetToLoop( false ),
			CondReturnNodeNum( 0 ),
			CondSupplyNodeNum( 0 ),
			HeatReturnNodeNum( 0 ),
			HeatSupplyNodeNum( 0 ),
			HeatSetPointErrDone( false ),
			HeatSetPointSetToLoop( false ),
			MinPartLoadRat( 0.0 ),
			MaxPartLoadRat( 0.0 ),
			OptPartLoadRat( 0.0 ),
			TempDesCondReturn( 0.0 ),
			TempDesCHWSupply( 0.0 ),
			EvapVolFlowRate( 0.0 ),
			CondVolFlowRate( 0.0 ),
			HeatVolFlowRate( 0.0 ),
			SizFac( 0.0 ),
			CoolCapFTCurve( 0 ),
			FuelCoolFTCurve( 0 ),
			FuelCoolFPLRCurve( 0 ),
			ElecCoolFTCurve( 0 ),
			ElecCoolFPLRCurve( 0 ),
			HeatCapFCoolCurve( 0 ),
			FuelHeatFHPLRCurve( 0 ),
			isEnterCondensTemp( false ),
			isWaterCooled( false ),
			CHWLowLimitTemp( 0.0 ),
			FuelHeatingValue( 0.0 ),
			DesCondMassFlowRate( 0.0 ),
			DesHeatMassFlowRate( 0.0 ),
			DesEvapMassFlowRate( 0.0 ),
			DeltaTempCoolErrCount( 0 ),
			DeltaTempHeatErrCount( 0 ),
			CondErrCount( 0 ),
			PossibleSubcooling( false ),
			CWLoopNum( 0 ),
			CWLoopSideNum( 0 ),
			CWBranchNum( 0 ),
			CWCompNum( 0 ),
			CDLoopNum( 0 ),
			CDLoopSideNum( 0 ),
			CDBranchNum( 0 ),
			CDCompNum( 0 ),
			HWLoopNum( 0 ),
			HWLoopSideNum( 0 ),
			HWBranchNum( 0 ),
			HWCompNum( 0 ),
			IsThisSized( false )
		{}

		// Member Constructor
		GasAbsorberSpecs(
			bool const Available, // need an array of logicals--load identifiers of available equipment
			bool const ON, // simulate the machine at it's operating part load ratio
			bool const InCoolingMode,
			bool const InHeatingMode,
			std::string const & Name, // user identifier
			std::string const & FuelType, // Type of Fuel - DIESEL, GASOLINE, GAS
			Real64 const NomCoolingCap, // W - design nominal capacity of Absorber
			Real64 const NomHeatCoolRatio, // ratio of heating to cooling capacity
			Real64 const FuelCoolRatio, // ratio of fuel input to cooling output
			Real64 const FuelHeatRatio, // ratio of fuel input to heating output
			Real64 const ElecCoolRatio, // ratio of electricity input to cooling output
			Real64 const ElecHeatRatio, // ratio of electricity input to heating output
			int const ChillReturnNodeNum, // Node number on the inlet side of the plant
			int const ChillSupplyNodeNum, // Node number on the outlet side of the plant
			bool const ChillSetPointErrDone, // flag to report missing setpoint on CW outlet
			bool const ChillSetPointSetToLoop, // flag to use overall loop setpoint
			int const CondReturnNodeNum, // Node number on the inlet side of the condenser
			int const CondSupplyNodeNum, // Node number on the outlet side of the condenser
			int const HeatReturnNodeNum, // absorber steam inlet node number, water side
			int const HeatSupplyNodeNum, // absorber steam outlet node number, water side
			bool const HeatSetPointErrDone, // flag to report missing setpoint on HW outlet
			bool const HeatSetPointSetToLoop, // flag to use overall loop setpoint
			Real64 const MinPartLoadRat, // min allowed operating frac full load
			Real64 const MaxPartLoadRat, // max allowed operating frac full load
			Real64 const OptPartLoadRat, // optimal operating frac full load
			Real64 const TempDesCondReturn, // design secondary loop fluid temperature at the Absorber condenser side inlet
			Real64 const TempDesCHWSupply, // design chilled water supply temperature
			Real64 const EvapVolFlowRate, // m**3/s - design nominal water volumetric flow rate through the evaporator
			Real64 const CondVolFlowRate, // m**3/s - design nominal water volumetric flow rate through the condenser
			Real64 const HeatVolFlowRate, // m**3/s - design nominal water volumetric flow rate through the heater side
			Real64 const SizFac, // sizing factor
			int const CoolCapFTCurve, // cooling capacity as a function of temperature curve (chilled water temp,
			int const FuelCoolFTCurve, // Fuel-Input-to cooling output Ratio Function of Temperature Curve (chilled
			int const FuelCoolFPLRCurve, // Fuel-Input-to cooling output Ratio Function of Part Load Ratio Curve
			int const ElecCoolFTCurve, // Electric-Input-to cooling output Ratio Function of Temperature Curve
			int const ElecCoolFPLRCurve, // Electric-Input-to cooling output Ratio Function of Part Load Ratio Curve
			int const HeatCapFCoolCurve, // Heating Capacity Function of Cooling Capacity Curve
			int const FuelHeatFHPLRCurve, // Fuel Input to heat output ratio during heating only function
			bool const isEnterCondensTemp, // if using entering conderser water temperature is TRUE, exiting is FALSE
			bool const isWaterCooled, // if water cooled it is TRUE
			Real64 const CHWLowLimitTemp, // Chilled Water Lower Limit Temperature
			Real64 const FuelHeatingValue,
			Real64 const DesCondMassFlowRate, // design nominal mass flow rate of water through the condenser [kg/s]
			Real64 const DesHeatMassFlowRate, // design nominal mass flow rate of water through the hot water side [kg/s]
			Real64 const DesEvapMassFlowRate, // design nominal mass flow rate of water through chilled water side [kg/s]
			int const DeltaTempCoolErrCount, // error count for Delta Temp = 0 while cooling
			int const DeltaTempHeatErrCount, // error count for Delta Temp = 0 while heating
			int const CondErrCount, // error count for poor Condenser Supply Estimate
			bool const PossibleSubcooling, // Flag to determine whether plant is overcooled
			int const CWLoopNum, // chilled water plant loop index number
			int const CWLoopSideNum, // chilled water plant loop side index
			int const CWBranchNum, // chilled water plant loop branch index
			int const CWCompNum, // chilled water plant loop component index
			int const CDLoopNum, // condenser water plant loop index number
			int const CDLoopSideNum, // condenser water plant loop side index
			int const CDBranchNum, // condenser water plant loop branch index
			int const CDCompNum, // condenser water plant loop component index
			int const HWLoopNum, // hot water plant loop side index
			int const HWLoopSideNum, // hot water plant loop side index
			int const HWBranchNum, // hot water plant loop branch index
			int const HWCompNum, // hot water plant loop component index
			bool const IsThisSized // TRUE if sizing is done
		) :
			Available( Available ),
			ON( ON ),
			InCoolingMode( InCoolingMode ),
			InHeatingMode( InHeatingMode ),
			Name( Name ),
			FuelType( FuelType ),
			NomCoolingCap( NomCoolingCap ),
			NomHeatCoolRatio( NomHeatCoolRatio ),
			FuelCoolRatio( FuelCoolRatio ),
			FuelHeatRatio( FuelHeatRatio ),
			ElecCoolRatio( ElecCoolRatio ),
			ElecHeatRatio( ElecHeatRatio ),
			ChillReturnNodeNum( ChillReturnNodeNum ),
			ChillSupplyNodeNum( ChillSupplyNodeNum ),
			ChillSetPointErrDone( ChillSetPointErrDone ),
			ChillSetPointSetToLoop( ChillSetPointSetToLoop ),
			CondReturnNodeNum( CondReturnNodeNum ),
			CondSupplyNodeNum( CondSupplyNodeNum ),
			HeatReturnNodeNum( HeatReturnNodeNum ),
			HeatSupplyNodeNum( HeatSupplyNodeNum ),
			HeatSetPointErrDone( HeatSetPointErrDone ),
			HeatSetPointSetToLoop( HeatSetPointSetToLoop ),
			MinPartLoadRat( MinPartLoadRat ),
			MaxPartLoadRat( MaxPartLoadRat ),
			OptPartLoadRat( OptPartLoadRat ),
			TempDesCondReturn( TempDesCondReturn ),
			TempDesCHWSupply( TempDesCHWSupply ),
			EvapVolFlowRate( EvapVolFlowRate ),
			CondVolFlowRate( CondVolFlowRate ),
			HeatVolFlowRate( HeatVolFlowRate ),
			SizFac( SizFac ),
			CoolCapFTCurve( CoolCapFTCurve ),
			FuelCoolFTCurve( FuelCoolFTCurve ),
			FuelCoolFPLRCurve( FuelCoolFPLRCurve ),
			ElecCoolFTCurve( ElecCoolFTCurve ),
			ElecCoolFPLRCurve( ElecCoolFPLRCurve ),
			HeatCapFCoolCurve( HeatCapFCoolCurve ),
			FuelHeatFHPLRCurve( FuelHeatFHPLRCurve ),
			isEnterCondensTemp( isEnterCondensTemp ),
			isWaterCooled( isWaterCooled ),
			CHWLowLimitTemp( CHWLowLimitTemp ),
			FuelHeatingValue( FuelHeatingValue ),
			DesCondMassFlowRate( DesCondMassFlowRate ),
			DesHeatMassFlowRate( DesHeatMassFlowRate ),
			DesEvapMassFlowRate( DesEvapMassFlowRate ),
			DeltaTempCoolErrCount( DeltaTempCoolErrCount ),
			DeltaTempHeatErrCount( DeltaTempHeatErrCount ),
			CondErrCount( CondErrCount ),
			PossibleSubcooling( PossibleSubcooling ),
			CWLoopNum( CWLoopNum ),
			CWLoopSideNum( CWLoopSideNum ),
			CWBranchNum( CWBranchNum ),
			CWCompNum( CWCompNum ),
			CDLoopNum( CDLoopNum ),
			CDLoopSideNum( CDLoopSideNum ),
			CDBranchNum( CDBranchNum ),
			CDCompNum( CDCompNum ),
			HWLoopNum( HWLoopNum ),
			HWLoopSideNum( HWLoopSideNum ),
			HWBranchNum( HWBranchNum ),
			HWCompNum( HWCompNum ),
			IsThisSized( IsThisSized )
		{}

	};

	struct ReportVars
	{
		// Members
		Real64 CoolingLoad; // cooling load on the chiller (previously called QEvap)
		Real64 CoolingEnergy; // variable to track total cooling load for period (was EvapEnergy)
		Real64 HeatingLoad; // heating load on the chiller
		Real64 HeatingEnergy; // heating energy
		Real64 TowerLoad; // load on the cooling tower/condenser (previously called QCond)
		Real64 TowerEnergy; // variable to track total tower load for a period (was CondEnergy)
		Real64 FuelUseRate; // instantaneous use of gas for period
		Real64 FuelEnergy; // variable to track total fuel used for a period
		Real64 CoolFuelUseRate; // instantaneous use of gas for period for cooling
		Real64 CoolFuelEnergy; // variable to track total fuel used for a period for cooling
		Real64 HeatFuelUseRate; // instantaneous use of gas for period for heating
		Real64 HeatFuelEnergy; // variable to track total fuel used for a period for heating
		Real64 ElectricPower; // parasitic electric power used (was PumpingPower)
		Real64 ElectricEnergy; // track the total electricity used for a period (was PumpingEnergy)
		Real64 CoolElectricPower; // parasitic electric power used  for cooling
		Real64 CoolElectricEnergy; // track the total electricity used for a period for cooling
		Real64 HeatElectricPower; // parasitic electric power used  for heating
		Real64 HeatElectricEnergy; // track the total electricity used for a period for heating
		Real64 ChillReturnTemp; // reporting: evaporator inlet temperature (was EvapInletTemp)
		Real64 ChillSupplyTemp; // reporting: evaporator outlet temperature (was EvapOutletTemp)
		Real64 ChillWaterFlowRate; // reporting: evaporator mass flow rate (was Evapmdot)
		Real64 CondReturnTemp; // reporting: condenser inlet temperature (was CondInletTemp)
		Real64 CondSupplyTemp; // reporting: condenser outlet temperature (was CondOutletTemp)
		Real64 CondWaterFlowRate; // reporting: condenser mass flow rate (was Condmdot)
		Real64 HotWaterReturnTemp; // reporting: hot water return (inlet) temperature
		Real64 HotWaterSupplyTemp; // reporting: hot water supply (outlet) temperature
		Real64 HotWaterFlowRate; // reporting: hot water mass flow rate
		Real64 CoolPartLoadRatio; // operating part load ratio (load/capacity for cooling)
		Real64 HeatPartLoadRatio; // operating part load ratio (load/capacity for heating)
		Real64 CoolingCapacity; // current capacity after temperature adjustment
		Real64 HeatingCapacity; // current heating capacity
		Real64 FractionOfPeriodRunning; // fraction of the time period that the unit is operating
		Real64 FuelCOP; // reporting: cooling output/fuel input = CoolingLoad/CoolFuelUseRate

		// Default Constructor
		ReportVars() :
			CoolingLoad( 0.0 ),
			CoolingEnergy( 0.0 ),
			HeatingLoad( 0.0 ),
			HeatingEnergy( 0.0 ),
			TowerLoad( 0.0 ),
			TowerEnergy( 0.0 ),
			FuelUseRate( 0.0 ),
			FuelEnergy( 0.0 ),
			CoolFuelUseRate( 0.0 ),
			CoolFuelEnergy( 0.0 ),
			HeatFuelUseRate( 0.0 ),
			HeatFuelEnergy( 0.0 ),
			ElectricPower( 0.0 ),
			ElectricEnergy( 0.0 ),
			CoolElectricPower( 0.0 ),
			CoolElectricEnergy( 0.0 ),
			HeatElectricPower( 0.0 ),
			HeatElectricEnergy( 0.0 ),
			ChillReturnTemp( 0.0 ),
			ChillSupplyTemp( 0.0 ),
			ChillWaterFlowRate( 0.0 ),
			CondReturnTemp( 0.0 ),
			CondSupplyTemp( 0.0 ),
			CondWaterFlowRate( 0.0 ),
			HotWaterReturnTemp( 0.0 ),
			HotWaterSupplyTemp( 0.0 ),
			HotWaterFlowRate( 0.0 ),
			CoolPartLoadRatio( 0.0 ),
			HeatPartLoadRatio( 0.0 ),
			CoolingCapacity( 0.0 ),
			HeatingCapacity( 0.0 ),
			FractionOfPeriodRunning( 0.0 ),
			FuelCOP( 0.0 )
		{}

		// Member Constructor
		ReportVars(
			Real64 const CoolingLoad, // cooling load on the chiller (previously called QEvap)
			Real64 const CoolingEnergy, // variable to track total cooling load for period (was EvapEnergy)
			Real64 const HeatingLoad, // heating load on the chiller
			Real64 const HeatingEnergy, // heating energy
			Real64 const TowerLoad, // load on the cooling tower/condenser (previously called QCond)
			Real64 const TowerEnergy, // variable to track total tower load for a period (was CondEnergy)
			Real64 const FuelUseRate, // instantaneous use of gas for period
			Real64 const FuelEnergy, // variable to track total fuel used for a period
			Real64 const CoolFuelUseRate, // instantaneous use of gas for period for cooling
			Real64 const CoolFuelEnergy, // variable to track total fuel used for a period for cooling
			Real64 const HeatFuelUseRate, // instantaneous use of gas for period for heating
			Real64 const HeatFuelEnergy, // variable to track total fuel used for a period for heating
			Real64 const ElectricPower, // parasitic electric power used (was PumpingPower)
			Real64 const ElectricEnergy, // track the total electricity used for a period (was PumpingEnergy)
			Real64 const CoolElectricPower, // parasitic electric power used  for cooling
			Real64 const CoolElectricEnergy, // track the total electricity used for a period for cooling
			Real64 const HeatElectricPower, // parasitic electric power used  for heating
			Real64 const HeatElectricEnergy, // track the total electricity used for a period for heating
			Real64 const ChillReturnTemp, // reporting: evaporator inlet temperature (was EvapInletTemp)
			Real64 const ChillSupplyTemp, // reporting: evaporator outlet temperature (was EvapOutletTemp)
			Real64 const ChillWaterFlowRate, // reporting: evaporator mass flow rate (was Evapmdot)
			Real64 const CondReturnTemp, // reporting: condenser inlet temperature (was CondInletTemp)
			Real64 const CondSupplyTemp, // reporting: condenser outlet temperature (was CondOutletTemp)
			Real64 const CondWaterFlowRate, // reporting: condenser mass flow rate (was Condmdot)
			Real64 const HotWaterReturnTemp, // reporting: hot water return (inlet) temperature
			Real64 const HotWaterSupplyTemp, // reporting: hot water supply (outlet) temperature
			Real64 const HotWaterFlowRate, // reporting: hot water mass flow rate
			Real64 const CoolPartLoadRatio, // operating part load ratio (load/capacity for cooling)
			Real64 const HeatPartLoadRatio, // operating part load ratio (load/capacity for heating)
			Real64 const CoolingCapacity, // current capacity after temperature adjustment
			Real64 const HeatingCapacity, // current heating capacity
			Real64 const FractionOfPeriodRunning, // fraction of the time period that the unit is operating
			Real64 const FuelCOP // reporting: cooling output/fuel input = CoolingLoad/CoolFuelUseRate
		) :
			CoolingLoad( CoolingLoad ),
			CoolingEnergy( CoolingEnergy ),
			HeatingLoad( HeatingLoad ),
			HeatingEnergy( HeatingEnergy ),
			TowerLoad( TowerLoad ),
			TowerEnergy( TowerEnergy ),
			FuelUseRate( FuelUseRate ),
			FuelEnergy( FuelEnergy ),
			CoolFuelUseRate( CoolFuelUseRate ),
			CoolFuelEnergy( CoolFuelEnergy ),
			HeatFuelUseRate( HeatFuelUseRate ),
			HeatFuelEnergy( HeatFuelEnergy ),
			ElectricPower( ElectricPower ),
			ElectricEnergy( ElectricEnergy ),
			CoolElectricPower( CoolElectricPower ),
			CoolElectricEnergy( CoolElectricEnergy ),
			HeatElectricPower( HeatElectricPower ),
			HeatElectricEnergy( HeatElectricEnergy ),
			ChillReturnTemp( ChillReturnTemp ),
			ChillSupplyTemp( ChillSupplyTemp ),
			ChillWaterFlowRate( ChillWaterFlowRate ),
			CondReturnTemp( CondReturnTemp ),
			CondSupplyTemp( CondSupplyTemp ),
			CondWaterFlowRate( CondWaterFlowRate ),
			HotWaterReturnTemp( HotWaterReturnTemp ),
			HotWaterSupplyTemp( HotWaterSupplyTemp ),
			HotWaterFlowRate( HotWaterFlowRate ),
			CoolPartLoadRatio( CoolPartLoadRatio ),
			HeatPartLoadRatio( HeatPartLoadRatio ),
			CoolingCapacity( CoolingCapacity ),
			HeatingCapacity( HeatingCapacity ),
			FractionOfPeriodRunning( FractionOfPeriodRunning ),
			FuelCOP( FuelCOP )
		{}

	};

	// Object Data
	extern FArray1D< GasAbsorberSpecs > GasAbsorber; // dimension to number of machines
	extern FArray1D< ReportVars > GasAbsorberReport;

	// Functions

	void
	SimGasAbsorber(
		std::string const & AbsorberType, // type of Absorber
		std::string const & AbsorberName, // user specified name of Absorber
		int const EquipFlowCtrl, // Flow control mode for the equipment
		int & CompIndex, // Absorber number counter
		bool const RunFlag, // simulate Absorber when TRUE
		bool const FirstIteration, // initialize variables when TRUE
		bool & InitLoopEquip, // If not false, calculate the max load for operating conditions
		Real64 & MyLoad, // loop demand component will meet
		int const BranchInletNodeNum, // node number of inlet to calling branch,
		Real64 & MaxCap, // W - maximum operating capacity of Absorber
		Real64 & MinCap, // W - minimum operating capacity of Absorber
		Real64 & OptCap, // W - optimal operating capacity of Absorber
		bool const GetSizingFactor, // TRUE when just the sizing factor is requested
		Real64 & SizingFactor, // sizing factor
		Real64 & TempCondInDesign,
		Real64 & TempEvapOutDesign
	);

	// End Absorption Chiller Module Driver Subroutines
	//******************************************************************************

	// Beginning of Absorption Chiller Module Get Input subroutines
	//******************************************************************************

	void
	GetGasAbsorberInput();

	// End of Get Input subroutines for the Absorption Chiller Module
	//******************************************************************************

	void
	InitGasAbsorber(
		int const ChillNum, // number of the current engine driven chiller being simulated
		bool const RunFlag // TRUE when chiller operating
	);

	void
	SizeGasAbsorber( int const ChillNum );

	// Beginning of Absorber model Subroutines
	// *****************************************************************************

	void
	CalcGasAbsorberChillerModel(
		int & ChillNum, // Absorber number
		Real64 & MyLoad, // operating load
		bool const RunFlag // TRUE when Absorber operating
	);

	void
	CalcGasAbsorberHeaterModel(
		int & ChillNum, // Absorber number
		Real64 & MyLoad, // operating load
		bool const RunFlag // TRUE when Absorber operating
	);

	// End of Absorption Chiller Module Utility Subroutines
	// *****************************************************************************

	// Beginning of Record Keeping subroutines for the Absorption Chiller Module
	// *****************************************************************************

	void
	UpdateGasAbsorberCoolRecords(
		Real64 const MyLoad, // current load
		bool const RunFlag, // TRUE if Absorber operating
		int const ChillNum // Absorber number
	);

	void
	UpdateGasAbsorberHeatRecords(
		Real64 const MyLoad, // current load
		bool const RunFlag, // TRUE if Absorber operating
		int const ChillNum // Absorber number
	);

	// End of Record Keeping subroutines for the Absorption Chiller Module
	// *****************************************************************************

	//                                 COPYRIGHT NOTICE

	//     Portions Copyright � Gas Research Institute 2001.  All rights reserved.

	//     GRI LEGAL NOTICE
	//     Neither GRI, members of GRI nor any person or organization acting on behalf
	//     of either:

	//     A. Makes any warranty of representation, express or implied with respect to
	//        the accuracy, completness, or usefulness of the information contained in
	//        in this program, including any warranty of merchantability or fitness of
	//        any purpose with respoect to the program, or that the use of any
	//        information disclosed in this program may not infringe privately-owned
	//        rights, or

	//     B.  Assumes any liability with respoct to the use of, or for any and all
	//         damages resulting from the use of the program or any portion thereof or
	//         any information disclosed therein.

	//     NOTICE

	//     Copyright � 1996-2014 The Board of Trustees of the University of Illinois
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

} // ChillerGasAbsorption

} // EnergyPlus

#endif
