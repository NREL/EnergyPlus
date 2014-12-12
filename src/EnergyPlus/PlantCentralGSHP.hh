#ifndef PlantCentralGSHP_hh_INCLUDED
#define PlantCentralGSHP_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/FArray1D.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>
#include <DataGlobals.hh>

namespace EnergyPlus {

// Contents:
// CentralHeatPumpSystem (CGSHP) System
// ChillerHeaterPerformance:Electric:EIR

namespace PlantCentralGSHP {

	// Using/Aliasing

	// Data
	// MODULE PARAMETER DEFINITIONS:
	// Chiller type parameters: Only water cooled chiller is allowed
	extern int const WaterCooled;
	extern int const SmartMixing;
	extern int const FullyMixed;
	extern bool SimulClgDominant;
	extern bool SimulHtgDominant;

	// MODULE VARIABLE DECLARATIONS:
	extern bool GetInputWrapper; // When TRUE, calls subroutine to read input file.
	extern int NumWrappers; // Number of Wrappers specified in input
	extern int NumChillerHeaters; // Number of Chiller/heaters specified in input
	extern Real64 CondenserFanPower; // Condenser Fan Power (fan cycles with compressor) [W]
	extern Real64 ChillerCapFT; // Chiller/heater capacity fraction (evaluated as a function of temperature)
	extern Real64 ChillerEIRFT; // Chiller/heater electric input ratio (EIR = 1 / COP) as a function of temperature
	extern Real64 ChillerEIRFPLR; // Chiller/heater EIR as a function of part-load ratio (PLR)
	extern Real64 ChillerPartLoadRatio; // Chiller/heater part-load ratio (PLR)
	extern Real64 ChillerCyclingRatio; // Chiller/heater cycling ratio
	extern Real64 ChillerFalseLoadRate; // Chiller/heater false load over and above the water-side load [W]

	// Type defining the component specifications

	extern FArray1D_bool CheckEquipName;
	extern FArray1D_bool CHCheckEquipName;
	extern FArray1D_bool HPCheckEquipName;

	// SUBROUTINE SPECIFICATIONS FOR MODULE ChillerElectricEIR

	// Types

	struct CGSHPNodeData
	{
		// Members
		Real64 Temp; // {C}
		Real64 TempMin; // {C}
		Real64 TempSetPoint; // SensedNodeFlagValue ! {C}
		Real64 MassFlowRate; // {kg/s}
		Real64 MassFlowRateMin; // {kg/s}
		Real64 MassFlowRateMax; // SensedNodeFlagValue ! {kg/s}
		Real64 MassFlowRateMinAvail; // {kg/s}
		Real64 MassFlowRateMaxAvail; // {kg/s}
		Real64 MassFlowRateSetPoint; // {kg/s}
		Real64 MassFlowRateRequest; // {kg/s}

		// Default Constructor
		CGSHPNodeData() :
			Temp( 0.0 ),
			TempMin( 0.0 ),
			TempSetPoint( 0.0 ),
			MassFlowRate( 0.0 ),
			MassFlowRateMin( 0.0 ),
			MassFlowRateMax( 0.0 ),
			MassFlowRateMinAvail( 0.0 ),
			MassFlowRateMaxAvail( 0.0 ),
			MassFlowRateSetPoint( 0.0 ),
			MassFlowRateRequest( 0.0 )
		{}

		// Member Constructor
		CGSHPNodeData(
			Real64 const Temp, // {C}
			Real64 const TempMin, // {C}
			Real64 const TempSetPoint, // SensedNodeFlagValue ! {C}
			Real64 const MassFlowRate, // {kg/s}
			Real64 const MassFlowRateMin, // {kg/s}
			Real64 const MassFlowRateMax, // SensedNodeFlagValue ! {kg/s}
			Real64 const MassFlowRateMinAvail, // {kg/s}
			Real64 const MassFlowRateMaxAvail, // {kg/s}
			Real64 const MassFlowRateSetPoint, // {kg/s}
			Real64 const MassFlowRateRequest // {kg/s}
		) :
			Temp( Temp ),
			TempMin( TempMin ),
			TempSetPoint( TempSetPoint ),
			MassFlowRate( MassFlowRate ),
			MassFlowRateMin( MassFlowRateMin ),
			MassFlowRateMax( MassFlowRateMax ),
			MassFlowRateMinAvail( MassFlowRateMinAvail ),
			MassFlowRateMaxAvail( MassFlowRateMaxAvail ),
			MassFlowRateSetPoint( MassFlowRateSetPoint ),
			MassFlowRateRequest( MassFlowRateRequest )
		{}

	};

	struct WrapperComponentSpecs
	{
		// Members
		std::string WrapperPerformanceObjectType; // Component type
		std::string WrapperComponentName; // Component name
		int WrapperPerformanceObjectIndex; // Component index in the input array
		std::string WrapperPerformanceObjectSch; // Component operation schedule
		int WrapperIdenticalObjectNum; // Number of identical objects
		int CHSchedPtr; // Index to schedule

		// Default Constructor
		WrapperComponentSpecs() :
			WrapperPerformanceObjectIndex( 0 ),
			WrapperIdenticalObjectNum( 0 ),
			CHSchedPtr( 0 )
		{}

		// Member Constructor
		WrapperComponentSpecs(
			std::string const & WrapperPerformanceObjectType, // Component type
			std::string const & WrapperComponentName, // Component name
			int const WrapperPerformanceObjectIndex, // Component index in the input array
			std::string const & WrapperPerformanceObjectSch, // Component operation schedule
			int const WrapperIdenticalObjectNum, // Number of identical objects
			int const CHSchedPtr // Index to schedule
		) :
			WrapperPerformanceObjectType( WrapperPerformanceObjectType ),
			WrapperComponentName( WrapperComponentName ),
			WrapperPerformanceObjectIndex( WrapperPerformanceObjectIndex ),
			WrapperPerformanceObjectSch( WrapperPerformanceObjectSch ),
			WrapperIdenticalObjectNum( WrapperIdenticalObjectNum ),
			CHSchedPtr( CHSchedPtr )
		{}

	};

	struct ChillerHeaterSpecs
	{
		// Members
		std::string Name; // Name of the Chiller Heater object
		std::string CondModeCooling; // Cooling mode temperature curve input variable
		std::string CondModeHeating; // Clg/Htg mode temperature curve input variable
		std::string CondMode; // Current mode temperature curve input variable
		bool ConstantFlow; // True if this is a Constant Flow Chiller
		bool VariableFlow; // True if this is a Variable Flow Chiller
		bool CoolSetPointSetToLoop; // True if the setpoint is missing at the outlet node
		bool HeatSetPointSetToLoop; // True if the setpoint is missing at the outlet node
		bool CoolSetPointErrDone; // true if setpoint warning issued
		bool HeatSetPointErrDone; // true if setpoint warning issued
		bool PossibleSubcooling; // flag to indicate chiller is doing less cooling that requested
		int ChillerHeaterNum; // Chiller heater number
		int CondenserType; // Type of Condenser - only water cooled is allowed
		int ChillerCapFTCooling; // Cooling capacity function of temperature curve index
		int ChillerEIRFTCooling; // Elec Input to Cooling Output ratio function of temperature curve index
		int ChillerEIRFPLRCooling; // Elec Input to cooling output ratio function of PLR curve index
		int ChillerCapFTHeating; // Clg/Htg capacity function of temperature curve index
		int ChillerEIRFTHeating; // Elec Input to Clg/Htg Output ratio function of temperature curve index
		int ChillerEIRFPLRHeating; // Elec Input to Clg/Htg output ratio function of PLR curve index
		int ChillerCapFT; // Capacity function of temperature curve index
		int ChillerEIRFT; // Elec Input to demand output ratio function of temperature curve index
		int ChillerEIRFPLR; // Elec Input to demand output ratio function of PLR curve index
		int EvapInletNodeNum; // Node number on the inlet side of the plant (evaporator side)
		int EvapOutletNodeNum; // Node number on the outlet side of the plant (evaporator side)
		int CondInletNodeNum; // Node number on the inlet side of the condenser
		int CondOutletNodeNum; // Node number on the outlet side of the condenser
		int ChillerCapFTError; // Used for negative capacity as a function of temp warnings
		int ChillerCapFTErrorIndex; // Used for negative capacity as a function of temp warnings
		int ChillerEIRFTError; // Used for negative EIR as a function of temp warnings
		int ChillerEIRFTErrorIndex; // Used for negative EIR as a function of temp warnings
		int ChillerEIRFPLRError; // Used for negative EIR as a function of PLR warnings
		int ChillerEIRFPLRErrorIndex; // Used for negative EIR as a function of PLR warnings
		int ChillerEIRRefTempErrorIndex; // Used for reference temperature problems
		int DeltaTErrCount; // Evaporator delta T equals 0 for variable flow chiller warning messages
		int DeltaTErrCountIndex; // Index to evaporator delta T = 0 for variable flow chiller warning messages
		int CondMassFlowIndex; // Index to condenser mass flow rate
		Real64 RefCapCooling; // Reference cooling-mode evaporator capacity [W]
		Real64 RefCOPCooling; // Reference cooling-mode COP
		Real64 TempRefEvapOutCooling; // Reference cooling-mode evaporator leaving temperature [C]
		Real64 TempRefCondInCooling; // Reference cooling-mode condenser entering temperature [C]
		Real64 TempRefCondOutCooling; // Reference cooling-mode condenser leaving temperature [C]
		Real64 MaxPartLoadRatCooling; // Maximum Part load ratio in cooling mode
		Real64 OptPartLoadRatCooling; // Optimum Part load ratio in cooling mode
		Real64 MinPartLoadRatCooling; // minimum Part load ratio in cooling mode
		Real64 ClgHtgToCoolingCapRatio; // ratio of clg/htg-mode evaporator capacity to cooling-mode evap. cap
		Real64 ClgHtgtoCogPowerRatio; // ratio of clg/htg-mode evaporator power to cooling-mode evap. power
		Real64 RefCapClgHtg; // Reference clg/htg-mode evaporator capacity [W]
		Real64 RefCOPClgHtg; // Reference clg/htg-mode COP
		Real64 RefPowerClgHtg; // Reference clg/htg-mode evaporator power [W]
		Real64 TempRefEvapOutClgHtg; // Reference clg/htg-mode evaporator leaving temperature [C]
		Real64 TempRefCondInClgHtg; // Reference clg/htg-mode condenser entering temperature [C]
		Real64 TempRefCondOutClgHtg; // Reference clg/htg-mode condenser leaving temperature [C]
		Real64 TempLowLimitEvapOut; // Low temperature shut off [C]
		Real64 MaxPartLoadRatClgHtg; // Maximum Part load ratio in simultaneous heating/cooling mode
		Real64 OptPartLoadRatClgHtg; // Optimum Part load ratio in simultaneous heating/cooling mode
		Real64 MinPartLoadRatClgHtg; // minimum Part load ratio in simultaneous heating/cooling mode
		CGSHPNodeData EvapInletNode; // Chiller heater evaperator inlet node
		CGSHPNodeData EvapOutletNode; // Chiller heater evaperator outlet node
		CGSHPNodeData CondInletNode; // Chiller heater condenser inlet node
		CGSHPNodeData CondOutletNode; // Chiller heater condenser outlet node
		Real64 EvapVolFlowRate; // Reference water volumetric flow rate through the evaporator [m3/s]
		Real64 tmpEvapVolFlowRate; // temporary ref water vol flow rate for intermediate sizing [m3/s]
		Real64 CondVolFlowRate; // Reference water volumetric flow rate through the condenser [m3/s]
		Real64 tmpCondVolFlowRate; // temporary ref water vol flow rate for intermediate sizing [m3/s]
		Real64 CondMassFlowRateMax; // Reference water mass flow rate through condenser [kg/s]
		Real64 EvapMassFlowRateMax; // Reference water mass flow rate through evaporator [kg/s]
		Real64 Evapmdot; // Evaporator mass flow rate [kg/s]
		Real64 Condmdot; // Condenser mass flow rate [kg/s]
		Real64 DesignHotWaterVolFlowRate; // Design hot water volumetric flow rate through the condenser [m3/s]
		Real64 OpenMotorEff; // Open chiller motor efficiency [fraction, 0 to 1]
		Real64 SizFac; // sizing factor
		Real64 RefCap; // Reference evaporator capacity [W]
		Real64 RefCOP; // Reference COP
		Real64 TempRefEvapOut; // Reference evaporator leaving temperature [C]
		Real64 TempRefCondIn; // Reference condenser entering temperature [C]
		Real64 TempRefCondOut; // Reference condenser leaving temperature [C]
		Real64 OptPartLoadRat; // Optimal operating fraction of full load
		Real64 ChillerEIRFPLRMin; // Minimum value of PLR from EIRFPLR curve
		Real64 ChillerEIRFPLRMax; // Maximum value of PLR from EIRFPLR curve

		// Default Constructor
		ChillerHeaterSpecs() :
			ConstantFlow( false ),
			VariableFlow( false ),
			CoolSetPointSetToLoop( false ),
			HeatSetPointSetToLoop( false ),
			CoolSetPointErrDone( false ),
			HeatSetPointErrDone( false ),
			PossibleSubcooling( false ),
			ChillerHeaterNum( 1 ),
			CondenserType( 0 ),
			ChillerCapFTCooling( 0 ),
			ChillerEIRFTCooling( 0 ),
			ChillerEIRFPLRCooling( 0 ),
			ChillerCapFTHeating( 0 ),
			ChillerEIRFTHeating( 0 ),
			ChillerEIRFPLRHeating( 0 ),
			ChillerCapFT( 0 ),
			ChillerEIRFT( 0 ),
			ChillerEIRFPLR( 0 ),
			EvapInletNodeNum( 0 ),
			EvapOutletNodeNum( 0 ),
			CondInletNodeNum( 0 ),
			CondOutletNodeNum( 0 ),
			ChillerCapFTError( 0 ),
			ChillerCapFTErrorIndex( 0 ),
			ChillerEIRFTError( 0 ),
			ChillerEIRFTErrorIndex( 0 ),
			ChillerEIRFPLRError( 0 ),
			ChillerEIRFPLRErrorIndex( 0 ),
			ChillerEIRRefTempErrorIndex( 0 ),
			DeltaTErrCount( 0 ),
			DeltaTErrCountIndex( 0 ),
			CondMassFlowIndex( 0 ),
			RefCapCooling( 0.0 ),
			RefCOPCooling( 0.0 ),
			TempRefEvapOutCooling( 0.0 ),
			TempRefCondInCooling( 0.0 ),
			TempRefCondOutCooling( 0.0 ),
			MaxPartLoadRatCooling( 0.0 ),
			OptPartLoadRatCooling( 0.0 ),
			MinPartLoadRatCooling( 0.0 ),
			ClgHtgToCoolingCapRatio( 0.0 ),
			ClgHtgtoCogPowerRatio( 0.0 ),
			RefCapClgHtg( 0.0 ),
			RefCOPClgHtg( 0.0 ),
			RefPowerClgHtg( 0.0 ),
			TempRefEvapOutClgHtg( 0.0 ),
			TempRefCondInClgHtg( 0.0 ),
			TempRefCondOutClgHtg( 0.0 ),
			TempLowLimitEvapOut( 0.0 ),
			MaxPartLoadRatClgHtg( 0.0 ),
			OptPartLoadRatClgHtg( 0.0 ),
			MinPartLoadRatClgHtg( 0.0 ),
			EvapVolFlowRate( 0.0 ),
			tmpEvapVolFlowRate( 0.0 ),
			CondVolFlowRate( 0.0 ),
			tmpCondVolFlowRate( 0.0 ),
			CondMassFlowRateMax( 0.0 ),
			EvapMassFlowRateMax( 0.0 ),
			Evapmdot( 0.0 ),
			Condmdot( 0.0 ),
			DesignHotWaterVolFlowRate( 0.0 ),
			OpenMotorEff( 0.0 ),
			SizFac( 0.0 ),
			RefCap( 0.0 ),
			RefCOP( 0.0 ),
			TempRefEvapOut( 0.0 ),
			TempRefCondIn( 0.0 ),
			TempRefCondOut( 0.0 ),
			OptPartLoadRat( 0.0 ),
			ChillerEIRFPLRMin( 0.0 ),
			ChillerEIRFPLRMax( 0.0 )
		{}

		// Member Constructor
		ChillerHeaterSpecs(
			std::string const & Name, // Name of the Chiller Heater object
			std::string const & CondModeCooling, // Cooling mode temperature curve input variable
			std::string const & CondModeHeating, // Clg/Htg mode temperature curve input variable
			std::string const & CondMode, // Current mode temperature curve input variable
			bool const ConstantFlow, // True if this is a Constant Flow Chiller
			bool const VariableFlow, // True if this is a Variable Flow Chiller
			bool const CoolSetPointSetToLoop, // True if the setpoint is missing at the outlet node
			bool const HeatSetPointSetToLoop, // True if the setpoint is missing at the outlet node
			bool const CoolSetPointErrDone, // true if setpoint warning issued
			bool const HeatSetPointErrDone, // true if setpoint warning issued
			bool const PossibleSubcooling, // flag to indicate chiller is doing less cooling that requested
			int const ChillerHeaterNum, // Chiller heater number
			int const CondenserType, // Type of Condenser - only water cooled is allowed
			int const ChillerCapFTCooling, // Cooling capacity function of temperature curve index
			int const ChillerEIRFTCooling, // Elec Input to Cooling Output ratio function of temperature curve index
			int const ChillerEIRFPLRCooling, // Elec Input to cooling output ratio function of PLR curve index
			int const ChillerCapFTHeating, // Clg/Htg capacity function of temperature curve index
			int const ChillerEIRFTHeating, // Elec Input to Clg/Htg Output ratio function of temperature curve index
			int const ChillerEIRFPLRHeating, // Elec Input to Clg/Htg output ratio function of PLR curve index
			int const ChillerCapFT, // Capacity function of temperature curve index
			int const ChillerEIRFT, // Elec Input to demand output ratio function of temperature curve index
			int const ChillerEIRFPLR, // Elec Input to demand output ratio function of PLR curve index
			int const EvapInletNodeNum, // Node number on the inlet side of the plant (evaporator side)
			int const EvapOutletNodeNum, // Node number on the outlet side of the plant (evaporator side)
			int const CondInletNodeNum, // Node number on the inlet side of the condenser
			int const CondOutletNodeNum, // Node number on the outlet side of the condenser
			int const ChillerCapFTError, // Used for negative capacity as a function of temp warnings
			int const ChillerCapFTErrorIndex, // Used for negative capacity as a function of temp warnings
			int const ChillerEIRFTError, // Used for negative EIR as a function of temp warnings
			int const ChillerEIRFTErrorIndex, // Used for negative EIR as a function of temp warnings
			int const ChillerEIRFPLRError, // Used for negative EIR as a function of PLR warnings
			int const ChillerEIRFPLRErrorIndex, // Used for negative EIR as a function of PLR warnings
			int const ChillerEIRRefTempErrorIndex, // Used for reference temperature problems
			int const DeltaTErrCount, // Evaporator delta T equals 0 for variable flow chiller warning messages
			int const DeltaTErrCountIndex, // Index to evaporator delta T = 0 for variable flow chiller warning messages
			int const CondMassFlowIndex, // Index to condenser mass flow rate
			Real64 const RefCapCooling, // Reference cooling-mode evaporator capacity [W]
			Real64 const RefCOPCooling, // Reference cooling-mode COP
			Real64 const TempRefEvapOutCooling, // Reference cooling-mode evaporator leaving temperature [C]
			Real64 const TempRefCondInCooling, // Reference cooling-mode condenser entering temperature [C]
			Real64 const TempRefCondOutCooling, // Reference cooling-mode condenser leaving temperature [C]
			Real64 const MaxPartLoadRatCooling, // Maximum Part load ratio in cooling mode
			Real64 const OptPartLoadRatCooling, // Optimum Part load ratio in cooling mode
			Real64 const MinPartLoadRatCooling, // minimum Part load ratio in cooling mode
			Real64 const ClgHtgToCoolingCapRatio, // ratio of clg/htg-mode evaporator capacity to cooling-mode evap. cap
			Real64 const ClgHtgtoCogPowerRatio, // ratio of clg/htg-mode evaporator power to cooling-mode evap. power
			Real64 const RefCapClgHtg, // Reference clg/htg-mode evaporator capacity [W]
			Real64 const RefCOPClgHtg, // Reference clg/htg-mode COP
			Real64 const RefPowerClgHtg, // Reference clg/htg-mode evaporator power [W]
			Real64 const TempRefEvapOutClgHtg, // Reference clg/htg-mode evaporator leaving temperature [C]
			Real64 const TempRefCondInClgHtg, // Reference clg/htg-mode condenser entering temperature [C]
			Real64 const TempRefCondOutClgHtg, // Reference clg/htg-mode condenser leaving temperature [C]
			Real64 const TempLowLimitEvapOut, // Low temperature shut off [C]
			Real64 const MaxPartLoadRatClgHtg, // Maximum Part load ratio in simultaneous heating/cooling mode
			Real64 const OptPartLoadRatClgHtg, // Optimum Part load ratio in simultaneous heating/cooling mode
			Real64 const MinPartLoadRatClgHtg, // minimum Part load ratio in simultaneous heating/cooling mode
			CGSHPNodeData const & EvapInletNode, // Chiller heater evaperator inlet node
			CGSHPNodeData const & EvapOutletNode, // Chiller heater evaperator outlet node
			CGSHPNodeData const & CondInletNode, // Chiller heater condenser inlet node
			CGSHPNodeData const & CondOutletNode, // Chiller heater condenser outlet node
			Real64 const EvapVolFlowRate, // Reference water volumetric flow rate through the evaporator [m3/s]
			Real64 const tmpEvapVolFlowRate, // temporary ref water vol flow rate for intermediate sizing [m3/s]
			Real64 const CondVolFlowRate, // Reference water volumetric flow rate through the condenser [m3/s]
			Real64 const tmpCondVolFlowRate, // temporary ref water vol flow rate for intermediate sizing [m3/s]
			Real64 const CondMassFlowRateMax, // Reference water mass flow rate through condenser [kg/s]
			Real64 const EvapMassFlowRateMax, // Reference water mass flow rate through evaporator [kg/s]
			Real64 const Evapmdot, // Evaporator mass flow rate [kg/s]
			Real64 const Condmdot, // Condenser mass flow rate [kg/s]
			Real64 const DesignHotWaterVolFlowRate, // Design hot water volumetric flow rate through the condenser [m3/s]
			Real64 const OpenMotorEff, // Open chiller motor efficiency [fraction, 0 to 1]
			Real64 const SizFac, // sizing factor
			Real64 const RefCap, // Reference evaporator capacity [W]
			Real64 const RefCOP, // Reference COP
			Real64 const TempRefEvapOut, // Reference evaporator leaving temperature [C]
			Real64 const TempRefCondIn, // Reference condenser entering temperature [C]
			Real64 const TempRefCondOut, // Reference condenser leaving temperature [C]
			Real64 const OptPartLoadRat, // Optimal operating fraction of full load
			Real64 const ChillerEIRFPLRMin, // Minimum value of PLR from EIRFPLR curve
			Real64 const ChillerEIRFPLRMax // Maximum value of PLR from EIRFPLR curve
		) :
			Name( Name ),
			CondModeCooling( CondModeCooling ),
			CondModeHeating( CondModeHeating ),
			CondMode( CondMode ),
			ConstantFlow( ConstantFlow ),
			VariableFlow( VariableFlow ),
			CoolSetPointSetToLoop( CoolSetPointSetToLoop ),
			HeatSetPointSetToLoop( HeatSetPointSetToLoop ),
			CoolSetPointErrDone( CoolSetPointErrDone ),
			HeatSetPointErrDone( HeatSetPointErrDone ),
			PossibleSubcooling( PossibleSubcooling ),
			ChillerHeaterNum( ChillerHeaterNum ),
			CondenserType( CondenserType ),
			ChillerCapFTCooling( ChillerCapFTCooling ),
			ChillerEIRFTCooling( ChillerEIRFTCooling ),
			ChillerEIRFPLRCooling( ChillerEIRFPLRCooling ),
			ChillerCapFTHeating( ChillerCapFTHeating ),
			ChillerEIRFTHeating( ChillerEIRFTHeating ),
			ChillerEIRFPLRHeating( ChillerEIRFPLRHeating ),
			ChillerCapFT( ChillerCapFT ),
			ChillerEIRFT( ChillerEIRFT ),
			ChillerEIRFPLR( ChillerEIRFPLR ),
			EvapInletNodeNum( EvapInletNodeNum ),
			EvapOutletNodeNum( EvapOutletNodeNum ),
			CondInletNodeNum( CondInletNodeNum ),
			CondOutletNodeNum( CondOutletNodeNum ),
			ChillerCapFTError( ChillerCapFTError ),
			ChillerCapFTErrorIndex( ChillerCapFTErrorIndex ),
			ChillerEIRFTError( ChillerEIRFTError ),
			ChillerEIRFTErrorIndex( ChillerEIRFTErrorIndex ),
			ChillerEIRFPLRError( ChillerEIRFPLRError ),
			ChillerEIRFPLRErrorIndex( ChillerEIRFPLRErrorIndex ),
			ChillerEIRRefTempErrorIndex( ChillerEIRRefTempErrorIndex ),
			DeltaTErrCount( DeltaTErrCount ),
			DeltaTErrCountIndex( DeltaTErrCountIndex ),
			CondMassFlowIndex( CondMassFlowIndex ),
			RefCapCooling( RefCapCooling ),
			RefCOPCooling( RefCOPCooling ),
			TempRefEvapOutCooling( TempRefEvapOutCooling ),
			TempRefCondInCooling( TempRefCondInCooling ),
			TempRefCondOutCooling( TempRefCondOutCooling ),
			MaxPartLoadRatCooling( MaxPartLoadRatCooling ),
			OptPartLoadRatCooling( OptPartLoadRatCooling ),
			MinPartLoadRatCooling( MinPartLoadRatCooling ),
			ClgHtgToCoolingCapRatio( ClgHtgToCoolingCapRatio ),
			ClgHtgtoCogPowerRatio( ClgHtgtoCogPowerRatio ),
			RefCapClgHtg( RefCapClgHtg ),
			RefCOPClgHtg( RefCOPClgHtg ),
			RefPowerClgHtg( RefPowerClgHtg ),
			TempRefEvapOutClgHtg( TempRefEvapOutClgHtg ),
			TempRefCondInClgHtg( TempRefCondInClgHtg ),
			TempRefCondOutClgHtg( TempRefCondOutClgHtg ),
			TempLowLimitEvapOut( TempLowLimitEvapOut ),
			MaxPartLoadRatClgHtg( MaxPartLoadRatClgHtg ),
			OptPartLoadRatClgHtg( OptPartLoadRatClgHtg ),
			MinPartLoadRatClgHtg( MinPartLoadRatClgHtg ),
			EvapInletNode( EvapInletNode ),
			EvapOutletNode( EvapOutletNode ),
			CondInletNode( CondInletNode ),
			CondOutletNode( CondOutletNode ),
			EvapVolFlowRate( EvapVolFlowRate ),
			tmpEvapVolFlowRate( tmpEvapVolFlowRate ),
			CondVolFlowRate( CondVolFlowRate ),
			tmpCondVolFlowRate( tmpCondVolFlowRate ),
			CondMassFlowRateMax( CondMassFlowRateMax ),
			EvapMassFlowRateMax( EvapMassFlowRateMax ),
			Evapmdot( Evapmdot ),
			Condmdot( Condmdot ),
			DesignHotWaterVolFlowRate( DesignHotWaterVolFlowRate ),
			OpenMotorEff( OpenMotorEff ),
			SizFac( SizFac ),
			RefCap( RefCap ),
			RefCOP( RefCOP ),
			TempRefEvapOut( TempRefEvapOut ),
			TempRefCondIn( TempRefCondIn ),
			TempRefCondOut( TempRefCondOut ),
			OptPartLoadRat( OptPartLoadRat ),
			ChillerEIRFPLRMin( ChillerEIRFPLRMin ),
			ChillerEIRFPLRMax( ChillerEIRFPLRMax )
		{}

	};

	struct CHReportVars
	{
		// Members
		int CurrentMode; // 0-off; 1-cooling only; 2-heating-only; 3-simutaneouls heat/cool
		Real64 ChillerPartLoadRatio; // Chiller PLR (Load/Capacity)
		Real64 ChillerCyclingRatio; // Chiller cycling ratio (time on/time step)
		Real64 ChillerFalseLoad; // Chiller false load over and above water side load [J]
		Real64 ChillerFalseLoadRate; // Chiller false load rate over and above water side load [W]
		Real64 CoolingPower; // Chiller power, W
		Real64 HeatingPower; // Chiller power, W
		Real64 QEvap; // Evaporator heat transfer rate [W]
		Real64 QCond; // Condenser heat transfer rate [W]
		Real64 CoolingEnergy; // Chiller electric consumption [J]
		Real64 HeatingEnergy; // Chiller electric consumption [J]
		Real64 EvapEnergy; // Evaporator heat transfer energy [J]
		Real64 CondEnergy; // Condenser heat transfer energy [J]
		Real64 CondInletTemp; // Condenser inlet temperature [C]
		Real64 EvapInletTemp; // Evaporator inlet temperature [C]
		Real64 CondOutletTemp; // Condenser outlet temperature [C]
		Real64 EvapOutletTemp; // Evaporator outlet temperature [C]
		Real64 Evapmdot; // Evaporator mass flow rate [kg/s]
		Real64 Condmdot; // Condenser mass flow rate [kg/s]
		Real64 ActualCOP; // Coefficient of performance
		Real64 ChillerCapFT; // Chiller capacity curve output value
		Real64 ChillerEIRFT; // Chiller EIRFT curve output value
		Real64 ChillerEIRFPLR; // Chiller EIRFPLR curve output value
		Real64 CondenserFanPowerUse; // Air-cooled condenser fan power [W]
		Real64 CondenserFanEnergy; // Air-cooled condenser fan energy [J]
		Real64 CondenserFanEnergyConsumption; // ""Should be checked"" For now, leave it
		Real64 ChillerPartLoadRatioSimul; // Chiller PLR (Load/Capacity) for simul clg/htg mode
		Real64 ChillerCyclingRatioSimul; // Chiller cycling ratio (time on/time step) for simul clg/htg mode
		Real64 ChillerFalseLoadSimul; // Chiller false load for simul clg/htg mode [J]
		Real64 ChillerFalseLoadRateSimul; // Chiller false load rate for simul clg/htg mode [W]
		Real64 CoolingPowerSimul; // Chiller power for simul clg/htg mode [W]
		Real64 QEvapSimul; // Evaporator heat transfer rate for simul clg/htg mode [W]
		Real64 QCondSimul; // Evaporator heat transfer rate for simul clg/htg mode [W]
		Real64 CoolingEnergySimul; // Chiller electric consumption for simul clg/htg mode [J]
		Real64 EvapEnergySimul; // Evaporator heat transfer energy for simul clg/htg mode [J]
		Real64 CondEnergySimul; // Condenser heat transfer energy for simul clg/htg mode [J]
		Real64 EvapInletTempSimul; // Evaporator inlet temperature for simul clg/htg mode [C]
		Real64 EvapOutletTempSimul; // Evaporator outlet temperature for simul clg/htg mode [C]
		Real64 EvapmdotSimul; // Evaporator mass flow rate for simul clg/htg mode [kg/s]
		Real64 CondInletTempSimul; // Condenser inlet temperature for simul clg/htg mode [C]
		Real64 CondOutletTempSimul; // Condenser outlet temperature for simul clg/htg mode [C]
		Real64 CondmdotSimul; // Condenser mass flow rate for simul clg/htg mode [kg/s]
		Real64 ChillerCapFTSimul; // Chiller capacity curve output value for simul clg/htg mode
		Real64 ChillerEIRFTSimul; // Chiller EIRFT curve output value for simul clg/htg mode
		Real64 ChillerEIRFPLRSimul; // Chiller EIRFPLR curve output value for simul clg/htg mode

		// Default Constructor
		CHReportVars() :
			CurrentMode( 0 ),
			ChillerPartLoadRatio( 0.0 ),
			ChillerCyclingRatio( 0.0 ),
			ChillerFalseLoad( 0.0 ),
			ChillerFalseLoadRate( 0.0 ),
			CoolingPower( 0.0 ),
			HeatingPower( 0.0 ),
			QEvap( 0.0 ),
			QCond( 0.0 ),
			CoolingEnergy( 0.0 ),
			HeatingEnergy( 0.0 ),
			EvapEnergy( 0.0 ),
			CondEnergy( 0.0 ),
			CondInletTemp( 0.0 ),
			EvapInletTemp( 0.0 ),
			CondOutletTemp( 0.0 ),
			EvapOutletTemp( 0.0 ),
			Evapmdot( 0.0 ),
			Condmdot( 0.0 ),
			ActualCOP( 0.0 ),
			ChillerCapFT( 0.0 ),
			ChillerEIRFT( 0.0 ),
			ChillerEIRFPLR( 0.0 ),
			CondenserFanPowerUse( 0.0 ),
			CondenserFanEnergy( 0.0 ),
			CondenserFanEnergyConsumption( 0.0 ),
			ChillerPartLoadRatioSimul( 0.0 ),
			ChillerCyclingRatioSimul( 0.0 ),
			ChillerFalseLoadSimul( 0.0 ),
			ChillerFalseLoadRateSimul( 0.0 ),
			CoolingPowerSimul( 0.0 ),
			QEvapSimul( 0.0 ),
			QCondSimul( 0.0 ),
			CoolingEnergySimul( 0.0 ),
			EvapEnergySimul( 0.0 ),
			CondEnergySimul( 0.0 ),
			EvapInletTempSimul( 0.0 ),
			EvapOutletTempSimul( 0.0 ),
			EvapmdotSimul( 0.0 ),
			CondInletTempSimul( 0.0 ),
			CondOutletTempSimul( 0.0 ),
			CondmdotSimul( 0.0 ),
			ChillerCapFTSimul( 0.0 ),
			ChillerEIRFTSimul( 0.0 ),
			ChillerEIRFPLRSimul( 0.0 )
		{}

		// Member Constructor
		CHReportVars(
			int const CurrentMode, // 0-off; 1-cooling only; 2-heating-only; 3-simutaneouls heat/cool
			Real64 const ChillerPartLoadRatio, // Chiller PLR (Load/Capacity)
			Real64 const ChillerCyclingRatio, // Chiller cycling ratio (time on/time step)
			Real64 const ChillerFalseLoad, // Chiller false load over and above water side load [J]
			Real64 const ChillerFalseLoadRate, // Chiller false load rate over and above water side load [W]
			Real64 const CoolingPower, // Chiller power, W
			Real64 const HeatingPower, // Chiller power, W
			Real64 const QEvap, // Evaporator heat transfer rate [W]
			Real64 const QCond, // Condenser heat transfer rate [W]
			Real64 const CoolingEnergy, // Chiller electric consumption [J]
			Real64 const HeatingEnergy, // Chiller electric consumption [J]
			Real64 const EvapEnergy, // Evaporator heat transfer energy [J]
			Real64 const CondEnergy, // Condenser heat transfer energy [J]
			Real64 const CondInletTemp, // Condenser inlet temperature [C]
			Real64 const EvapInletTemp, // Evaporator inlet temperature [C]
			Real64 const CondOutletTemp, // Condenser outlet temperature [C]
			Real64 const EvapOutletTemp, // Evaporator outlet temperature [C]
			Real64 const Evapmdot, // Evaporator mass flow rate [kg/s]
			Real64 const Condmdot, // Condenser mass flow rate [kg/s]
			Real64 const ActualCOP, // Coefficient of performance
			Real64 const ChillerCapFT, // Chiller capacity curve output value
			Real64 const ChillerEIRFT, // Chiller EIRFT curve output value
			Real64 const ChillerEIRFPLR, // Chiller EIRFPLR curve output value
			Real64 const CondenserFanPowerUse, // Air-cooled condenser fan power [W]
			Real64 const CondenserFanEnergy, // Air-cooled condenser fan energy [J]
			Real64 const CondenserFanEnergyConsumption, // ""Should be checked"" For now, leave it
			Real64 const ChillerPartLoadRatioSimul, // Chiller PLR (Load/Capacity) for simul clg/htg mode
			Real64 const ChillerCyclingRatioSimul, // Chiller cycling ratio (time on/time step) for simul clg/htg mode
			Real64 const ChillerFalseLoadSimul, // Chiller false load for simul clg/htg mode [J]
			Real64 const ChillerFalseLoadRateSimul, // Chiller false load rate for simul clg/htg mode [W]
			Real64 const CoolingPowerSimul, // Chiller power for simul clg/htg mode [W]
			Real64 const QEvapSimul, // Evaporator heat transfer rate for simul clg/htg mode [W]
			Real64 const QCondSimul, // Evaporator heat transfer rate for simul clg/htg mode [W]
			Real64 const CoolingEnergySimul, // Chiller electric consumption for simul clg/htg mode [J]
			Real64 const EvapEnergySimul, // Evaporator heat transfer energy for simul clg/htg mode [J]
			Real64 const CondEnergySimul, // Condenser heat transfer energy for simul clg/htg mode [J]
			Real64 const EvapInletTempSimul, // Evaporator inlet temperature for simul clg/htg mode [C]
			Real64 const EvapOutletTempSimul, // Evaporator outlet temperature for simul clg/htg mode [C]
			Real64 const EvapmdotSimul, // Evaporator mass flow rate for simul clg/htg mode [kg/s]
			Real64 const CondInletTempSimul, // Condenser inlet temperature for simul clg/htg mode [C]
			Real64 const CondOutletTempSimul, // Condenser outlet temperature for simul clg/htg mode [C]
			Real64 const CondmdotSimul, // Condenser mass flow rate for simul clg/htg mode [kg/s]
			Real64 const ChillerCapFTSimul, // Chiller capacity curve output value for simul clg/htg mode
			Real64 const ChillerEIRFTSimul, // Chiller EIRFT curve output value for simul clg/htg mode
			Real64 const ChillerEIRFPLRSimul // Chiller EIRFPLR curve output value for simul clg/htg mode
		) :
			CurrentMode( CurrentMode ),
			ChillerPartLoadRatio( ChillerPartLoadRatio ),
			ChillerCyclingRatio( ChillerCyclingRatio ),
			ChillerFalseLoad( ChillerFalseLoad ),
			ChillerFalseLoadRate( ChillerFalseLoadRate ),
			CoolingPower( CoolingPower ),
			HeatingPower( HeatingPower ),
			QEvap( QEvap ),
			QCond( QCond ),
			CoolingEnergy( CoolingEnergy ),
			HeatingEnergy( HeatingEnergy ),
			EvapEnergy( EvapEnergy ),
			CondEnergy( CondEnergy ),
			CondInletTemp( CondInletTemp ),
			EvapInletTemp( EvapInletTemp ),
			CondOutletTemp( CondOutletTemp ),
			EvapOutletTemp( EvapOutletTemp ),
			Evapmdot( Evapmdot ),
			Condmdot( Condmdot ),
			ActualCOP( ActualCOP ),
			ChillerCapFT( ChillerCapFT ),
			ChillerEIRFT( ChillerEIRFT ),
			ChillerEIRFPLR( ChillerEIRFPLR ),
			CondenserFanPowerUse( CondenserFanPowerUse ),
			CondenserFanEnergy( CondenserFanEnergy ),
			CondenserFanEnergyConsumption( CondenserFanEnergyConsumption ),
			ChillerPartLoadRatioSimul( ChillerPartLoadRatioSimul ),
			ChillerCyclingRatioSimul( ChillerCyclingRatioSimul ),
			ChillerFalseLoadSimul( ChillerFalseLoadSimul ),
			ChillerFalseLoadRateSimul( ChillerFalseLoadRateSimul ),
			CoolingPowerSimul( CoolingPowerSimul ),
			QEvapSimul( QEvapSimul ),
			QCondSimul( QCondSimul ),
			CoolingEnergySimul( CoolingEnergySimul ),
			EvapEnergySimul( EvapEnergySimul ),
			CondEnergySimul( CondEnergySimul ),
			EvapInletTempSimul( EvapInletTempSimul ),
			EvapOutletTempSimul( EvapOutletTempSimul ),
			EvapmdotSimul( EvapmdotSimul ),
			CondInletTempSimul( CondInletTempSimul ),
			CondOutletTempSimul( CondOutletTempSimul ),
			CondmdotSimul( CondmdotSimul ),
			ChillerCapFTSimul( ChillerCapFTSimul ),
			ChillerEIRFTSimul( ChillerEIRFTSimul ),
			ChillerEIRFPLRSimul( ChillerEIRFPLRSimul )
		{}

	};

	struct WrapperSpecs // This will be used for Wrapper Object. This object will decide the mode of Chiller
	{
		// Members
		std::string Name; // User identifier
		std::string AncilliaryPwSchedule; // Ancilliary Power Schedule Name
		bool VariableFlowCH; // True if all chiller heters are variable flow control
		int SchedPtr; // Schedule value for ancilliar power control
		int CHSchedPtr; // Schedule value for individual chiller heater control
		int ControlMode; // SmartMixing or FullyMixing
		int CHWInletNodeNum; // Node number on the inlet side of the plant (Chilled Water side)
		int CHWOutletNodeNum; // Node number on the outlet side of the plant (Chilled Water side)
		int HWInletNodeNum; // Node number on the inlet side of the plant (Hot Water side)
		int HWOutletNodeNum; // Node number on the outlet side of the plant (Hot Water side)
		int GLHEInletNodeNum; // Node number on the inlet side of the plant (GLHE Water side)
		int GLHEOutletNodeNum; // Node number on the outlet side of the plant (GLHE Water side)
		int NumOfComp; // Number of Components under the wrapper
		Real64 CHWMassFlowRate; // Chilled water mass flow rate
		Real64 HWMassFlowRate; // Hot water mass flow rate
		Real64 GLHEMassFlowRate; // Condenser water mass flow rate
		Real64 CHWMassFlowRateMax; // Maximum chilled water mass flow rate
		Real64 HWMassFlowRateMax; // Maximum hot water mass flow rate
		Real64 GLHEMassFlowRateMax; // Maximum condenser water mass flow rate
		Real64 WrapperCoolingLoad; // Cooling demand for the central heat pump system
		Real64 WrapperHeatingLoad; // Heating demand for the central heat pump system
		Real64 AncilliaryPower; // Wrapper Ancilliary Power
		FArray1D< WrapperComponentSpecs > WrapperComp;
		FArray1D< ChillerHeaterSpecs > ChillerHeater; // Dimension to number of machines
		FArray1D< CHReportVars > ChillerHeaterReport; // Dimension to number of machines
		bool CoolSetPointErrDone; // true if setpoint warning issued
		bool HeatSetPointErrDone; // true if setpoint warning issued
		bool CoolSetPointSetToLoop; // True if the setpoint is missing at the outlet node
		bool HeatSetPointSetToLoop; // True if the setpoint is missing at the outlet node
		int ChillerHeaterNums; // Total number of chiller heater units
		int CWLoopNum; // Chilled water plant loop index number
		int CWLoopSideNum; // Chilled water plant loop side index
		int CWBranchNum; // Chilled water plant loop branch index
		int CWCompNum; // Chilled water plant loop component index
		int HWLoopNum; // Hot water plant loop index number
		int HWLoopSideNum; // Hot water plant loop side index
		int HWBranchNum; // Hot water plant loop branch index
		int HWCompNum; // Hot water plant loop component index
		int GLHELoopNum; // Geo-field water plant loop index number
		int GLHELoopSideNum; // Geo-field water plant loop side index
		int GLHEBranchNum; // Geo-field water plant loop branch index
		int GLHECompNum; // Geo-field water plant loop component index
		int CHWMassFlowIndex; // Chilled water flow index
		int HWMassFlowIndex; // Hot water flow index
		int GLHEMassFlowIndex; // Condenser side flow index
		Real64 SizingFactor; // Sizing factor to adjust the capacity
		Real64 CHWVolFlowRate; // Chilled water volume flow rate [kg/s]
		Real64 HWVolFlowRate; // Hot water volume flow rate [kg/s]
		Real64 GLHEVolFlowRate; // Geo-field volume flow rate [kg/s]

		// Default Constructor
		WrapperSpecs() :
			VariableFlowCH( false ),
			SchedPtr( 0 ),
			CHSchedPtr( 0 ),
			ControlMode( 0 ),
			CHWInletNodeNum( 0 ),
			CHWOutletNodeNum( 0 ),
			HWInletNodeNum( 0 ),
			HWOutletNodeNum( 0 ),
			GLHEInletNodeNum( 0 ),
			GLHEOutletNodeNum( 0 ),
			NumOfComp( 0 ),
			CHWMassFlowRate( 0.0 ),
			HWMassFlowRate( 0.0 ),
			GLHEMassFlowRate( 0.0 ),
			CHWMassFlowRateMax( 0.0 ),
			HWMassFlowRateMax( 0.0 ),
			GLHEMassFlowRateMax( 0.0 ),
			WrapperCoolingLoad( 0.0 ),
			WrapperHeatingLoad( 0.0 ),
			AncilliaryPower( 0.0 ),
			CoolSetPointErrDone( false ),
			HeatSetPointErrDone( false ),
			CoolSetPointSetToLoop( false ),
			HeatSetPointSetToLoop( false ),
			CWLoopNum( 0 ),
			CWLoopSideNum( 0 ),
			CWBranchNum( 0 ),
			CWCompNum( 0 ),
			HWLoopNum( 0 ),
			HWLoopSideNum( 0 ),
			HWBranchNum( 0 ),
			HWCompNum( 0 ),
			GLHELoopNum( 0 ),
			GLHELoopSideNum( 0 ),
			GLHEBranchNum( 0 ),
			GLHECompNum( 0 ),
			CHWMassFlowIndex( 0 ),
			HWMassFlowIndex( 0 ),
			GLHEMassFlowIndex( 0 ),
			SizingFactor( 1.0 ),
			CHWVolFlowRate( 0.0 ),
			HWVolFlowRate( 0.0 ),
			GLHEVolFlowRate( 0.0 )
		{}

		// Member Constructor
		WrapperSpecs(
			std::string const & Name, // User identifier
			std::string const & AncilliaryPwSchedule, // Ancilliary Power Schedule Name
			bool const VariableFlowCH, // True if all chiller heters are variable flow control
			int const SchedPtr, // Schedule value for ancilliar power control
			int const CHSchedPtr, // Schedule value for individual chiller heater control
			int const ControlMode, // SmartMixing or FullyMixing
			int const CHWInletNodeNum, // Node number on the inlet side of the plant (Chilled Water side)
			int const CHWOutletNodeNum, // Node number on the outlet side of the plant (Chilled Water side)
			int const HWInletNodeNum, // Node number on the inlet side of the plant (Hot Water side)
			int const HWOutletNodeNum, // Node number on the outlet side of the plant (Hot Water side)
			int const GLHEInletNodeNum, // Node number on the inlet side of the plant (GLHE Water side)
			int const GLHEOutletNodeNum, // Node number on the outlet side of the plant (GLHE Water side)
			int const NumOfComp, // Number of Components under the wrapper
			Real64 const CHWMassFlowRate, // Chilled water mass flow rate
			Real64 const HWMassFlowRate, // Hot water mass flow rate
			Real64 const GLHEMassFlowRate, // Condenser water mass flow rate
			Real64 const CHWMassFlowRateMax, // Maximum chilled water mass flow rate
			Real64 const HWMassFlowRateMax, // Maximum hot water mass flow rate
			Real64 const GLHEMassFlowRateMax, // Maximum condenser water mass flow rate
			Real64 const WrapperCoolingLoad, // Cooling demand for the central heat pump system
			Real64 const WrapperHeatingLoad, // Heating demand for the central heat pump system
			Real64 const AncilliaryPower, // Wrapper Ancilliary Power
			FArray1< WrapperComponentSpecs > const & WrapperComp,
			FArray1< ChillerHeaterSpecs > const & ChillerHeater, // Dimension to number of machines
			FArray1< CHReportVars > const & ChillerHeaterReport, // Dimension to number of machines
			bool const CoolSetPointErrDone, // true if setpoint warning issued
			bool const HeatSetPointErrDone, // true if setpoint warning issued
			bool const CoolSetPointSetToLoop, // True if the setpoint is missing at the outlet node
			bool const HeatSetPointSetToLoop, // True if the setpoint is missing at the outlet node
			int const ChillerHeaterNums, // Total number of chiller heater units
			int const CWLoopNum, // Chilled water plant loop index number
			int const CWLoopSideNum, // Chilled water plant loop side index
			int const CWBranchNum, // Chilled water plant loop branch index
			int const CWCompNum, // Chilled water plant loop component index
			int const HWLoopNum, // Hot water plant loop index number
			int const HWLoopSideNum, // Hot water plant loop side index
			int const HWBranchNum, // Hot water plant loop branch index
			int const HWCompNum, // Hot water plant loop component index
			int const GLHELoopNum, // Geo-field water plant loop index number
			int const GLHELoopSideNum, // Geo-field water plant loop side index
			int const GLHEBranchNum, // Geo-field water plant loop branch index
			int const GLHECompNum, // Geo-field water plant loop component index
			int const CHWMassFlowIndex, // Chilled water flow index
			int const HWMassFlowIndex, // Hot water flow index
			int const GLHEMassFlowIndex, // Condenser side flow index
			Real64 const SizingFactor, // Sizing factor to adjust the capacity
			Real64 const CHWVolFlowRate, // Chilled water volume flow rate [kg/s]
			Real64 const HWVolFlowRate, // Hot water volume flow rate [kg/s]
			Real64 const GLHEVolFlowRate // Geo-field volume flow rate [kg/s]
		) :
			Name( Name ),
			AncilliaryPwSchedule( AncilliaryPwSchedule ),
			VariableFlowCH( VariableFlowCH ),
			SchedPtr( SchedPtr ),
			CHSchedPtr( CHSchedPtr ),
			ControlMode( ControlMode ),
			CHWInletNodeNum( CHWInletNodeNum ),
			CHWOutletNodeNum( CHWOutletNodeNum ),
			HWInletNodeNum( HWInletNodeNum ),
			HWOutletNodeNum( HWOutletNodeNum ),
			GLHEInletNodeNum( GLHEInletNodeNum ),
			GLHEOutletNodeNum( GLHEOutletNodeNum ),
			NumOfComp( NumOfComp ),
			CHWMassFlowRate( CHWMassFlowRate ),
			HWMassFlowRate( HWMassFlowRate ),
			GLHEMassFlowRate( GLHEMassFlowRate ),
			CHWMassFlowRateMax( CHWMassFlowRateMax ),
			HWMassFlowRateMax( HWMassFlowRateMax ),
			GLHEMassFlowRateMax( GLHEMassFlowRateMax ),
			WrapperCoolingLoad( WrapperCoolingLoad ),
			WrapperHeatingLoad( WrapperHeatingLoad ),
			AncilliaryPower( AncilliaryPower ),
			WrapperComp( WrapperComp ),
			ChillerHeater( ChillerHeater ),
			ChillerHeaterReport( ChillerHeaterReport ),
			CoolSetPointErrDone( CoolSetPointErrDone ),
			HeatSetPointErrDone( HeatSetPointErrDone ),
			CoolSetPointSetToLoop( CoolSetPointSetToLoop ),
			HeatSetPointSetToLoop( HeatSetPointSetToLoop ),
			ChillerHeaterNums( ChillerHeaterNums ),
			CWLoopNum( CWLoopNum ),
			CWLoopSideNum( CWLoopSideNum ),
			CWBranchNum( CWBranchNum ),
			CWCompNum( CWCompNum ),
			HWLoopNum( HWLoopNum ),
			HWLoopSideNum( HWLoopSideNum ),
			HWBranchNum( HWBranchNum ),
			HWCompNum( HWCompNum ),
			GLHELoopNum( GLHELoopNum ),
			GLHELoopSideNum( GLHELoopSideNum ),
			GLHEBranchNum( GLHEBranchNum ),
			GLHECompNum( GLHECompNum ),
			CHWMassFlowIndex( CHWMassFlowIndex ),
			HWMassFlowIndex( HWMassFlowIndex ),
			GLHEMassFlowIndex( GLHEMassFlowIndex ),
			SizingFactor( SizingFactor ),
			CHWVolFlowRate( CHWVolFlowRate ),
			HWVolFlowRate( HWVolFlowRate ),
			GLHEVolFlowRate( GLHEVolFlowRate )
		{}

	};

	struct WrapperReportVars
	{
		// Members
		Real64 Power; // Wrapper power, W
		Real64 QCHW; // Chilled water heat transfer rate [W]
		Real64 QHW; // Hot Water heat transfer rate [W]
		Real64 QGLHE; // Geo-field heat transfer rate [W]
		Real64 TotElecCooling; // Wrapper cooling electric consumption [J]
		Real64 TotElecHeating; // Wrapper heating electric consumption [J]
		Real64 CoolingEnergy; // Chilled water heat transfer energy [J]
		Real64 HeatingEnergy; // Hot Water heat transfer energy [J]
		Real64 GLHEEnergy; // Geo-field heat transfer energy [J]
		Real64 TotElecCoolingPwr; // Wrapper cooling electric consumption rate [W]
		Real64 TotElecHeatingPwr; // Wrapper heating electric consumption rate [W]
		Real64 CoolingRate; // Chilled water heat transfer rate [W]
		Real64 HeatingRate; // Hot Water heat transfer rate [W]
		Real64 GLHERate; // Geo-field heat transfer rate [W]
		Real64 CHWInletTemp; // Chilled water inlet temperature [C]
		Real64 HWInletTemp; // Hot water inlet temperature [C]
		Real64 GLHEInletTemp; // Geo-field inlet temperature [C]
		Real64 CHWOutletTemp; // Chilled water Outlet temperature [C]
		Real64 HWOutletTemp; // Hot water Outlet temperature [C]
		Real64 GLHEOutletTemp; // Geo-field Outlet temperature [C]
		Real64 CHWmdot; // Chilled water mass flow rate [kg/s]
		Real64 HWmdot; // Hot water mass flow rate [kg/s]
		Real64 GLHEmdot; // Geo-field mass flow rate [kg/s]
		Real64 TotElecCoolingSimul; // Wrapper cooling electric consumption [J]
		Real64 CoolingEnergySimul; // Chilled water heat transfer energy [J]
		Real64 TotElecCoolingPwrSimul; // Wrapper cooling electric consumption rate [W]
		Real64 CoolingRateSimul; // Chilled water heat transfer rate [W]
		Real64 CHWInletTempSimul; // Chilled water inlet temperature [C]
		Real64 GLHEInletTempSimul; // Geo-field inlet temperature [C]
		Real64 CHWOutletTempSimul; // Chilled water Outlet temperature [C]
		Real64 GLHEOutletTempSimul; // Geo-field Outlet temperature [C]
		Real64 CHWmdotSimul; // Chilled water mass flow rate [kg/s]
		Real64 GLHEmdotSimul; // Geo-field mass flow rate [kg/s]

		// Default Constructor
		WrapperReportVars() :
			Power( 0.0 ),
			QCHW( 0.0 ),
			QHW( 0.0 ),
			QGLHE( 0.0 ),
			TotElecCooling( 0.0 ),
			TotElecHeating( 0.0 ),
			CoolingEnergy( 0.0 ),
			HeatingEnergy( 0.0 ),
			GLHEEnergy( 0.0 ),
			TotElecCoolingPwr( 0.0 ),
			TotElecHeatingPwr( 0.0 ),
			CoolingRate( 0.0 ),
			HeatingRate( 0.0 ),
			GLHERate( 0.0 ),
			CHWInletTemp( 0.0 ),
			HWInletTemp( 0.0 ),
			GLHEInletTemp( 0.0 ),
			CHWOutletTemp( 0.0 ),
			HWOutletTemp( 0.0 ),
			GLHEOutletTemp( 0.0 ),
			CHWmdot( 0.0 ),
			HWmdot( 0.0 ),
			GLHEmdot( 0.0 ),
			TotElecCoolingSimul( 0.0 ),
			CoolingEnergySimul( 0.0 ),
			TotElecCoolingPwrSimul( 0.0 ),
			CoolingRateSimul( 0.0 ),
			CHWInletTempSimul( 0.0 ),
			GLHEInletTempSimul( 0.0 ),
			CHWOutletTempSimul( 0.0 ),
			GLHEOutletTempSimul( 0.0 ),
			CHWmdotSimul( 0.0 ),
			GLHEmdotSimul( 0.0 )
		{}

		// Member Constructor
		WrapperReportVars(
			Real64 const Power, // Wrapper power, W
			Real64 const QCHW, // Chilled water heat transfer rate [W]
			Real64 const QHW, // Hot Water heat transfer rate [W]
			Real64 const QGLHE, // Geo-field heat transfer rate [W]
			Real64 const TotElecCooling, // Wrapper cooling electric consumption [J]
			Real64 const TotElecHeating, // Wrapper heating electric consumption [J]
			Real64 const CoolingEnergy, // Chilled water heat transfer energy [J]
			Real64 const HeatingEnergy, // Hot Water heat transfer energy [J]
			Real64 const GLHEEnergy, // Geo-field heat transfer energy [J]
			Real64 const TotElecCoolingPwr, // Wrapper cooling electric consumption rate [W]
			Real64 const TotElecHeatingPwr, // Wrapper heating electric consumption rate [W]
			Real64 const CoolingRate, // Chilled water heat transfer rate [W]
			Real64 const HeatingRate, // Hot Water heat transfer rate [W]
			Real64 const GLHERate, // Geo-field heat transfer rate [W]
			Real64 const CHWInletTemp, // Chilled water inlet temperature [C]
			Real64 const HWInletTemp, // Hot water inlet temperature [C]
			Real64 const GLHEInletTemp, // Geo-field inlet temperature [C]
			Real64 const CHWOutletTemp, // Chilled water Outlet temperature [C]
			Real64 const HWOutletTemp, // Hot water Outlet temperature [C]
			Real64 const GLHEOutletTemp, // Geo-field Outlet temperature [C]
			Real64 const CHWmdot, // Chilled water mass flow rate [kg/s]
			Real64 const HWmdot, // Hot water mass flow rate [kg/s]
			Real64 const GLHEmdot, // Geo-field mass flow rate [kg/s]
			Real64 const TotElecCoolingSimul, // Wrapper cooling electric consumption [J]
			Real64 const CoolingEnergySimul, // Chilled water heat transfer energy [J]
			Real64 const TotElecCoolingPwrSimul, // Wrapper cooling electric consumption rate [W]
			Real64 const CoolingRateSimul, // Chilled water heat transfer rate [W]
			Real64 const CHWInletTempSimul, // Chilled water inlet temperature [C]
			Real64 const GLHEInletTempSimul, // Geo-field inlet temperature [C]
			Real64 const CHWOutletTempSimul, // Chilled water Outlet temperature [C]
			Real64 const GLHEOutletTempSimul, // Geo-field Outlet temperature [C]
			Real64 const CHWmdotSimul, // Chilled water mass flow rate [kg/s]
			Real64 const GLHEmdotSimul // Geo-field mass flow rate [kg/s]
		) :
			Power( Power ),
			QCHW( QCHW ),
			QHW( QHW ),
			QGLHE( QGLHE ),
			TotElecCooling( TotElecCooling ),
			TotElecHeating( TotElecHeating ),
			CoolingEnergy( CoolingEnergy ),
			HeatingEnergy( HeatingEnergy ),
			GLHEEnergy( GLHEEnergy ),
			TotElecCoolingPwr( TotElecCoolingPwr ),
			TotElecHeatingPwr( TotElecHeatingPwr ),
			CoolingRate( CoolingRate ),
			HeatingRate( HeatingRate ),
			GLHERate( GLHERate ),
			CHWInletTemp( CHWInletTemp ),
			HWInletTemp( HWInletTemp ),
			GLHEInletTemp( GLHEInletTemp ),
			CHWOutletTemp( CHWOutletTemp ),
			HWOutletTemp( HWOutletTemp ),
			GLHEOutletTemp( GLHEOutletTemp ),
			CHWmdot( CHWmdot ),
			HWmdot( HWmdot ),
			GLHEmdot( GLHEmdot ),
			TotElecCoolingSimul( TotElecCoolingSimul ),
			CoolingEnergySimul( CoolingEnergySimul ),
			TotElecCoolingPwrSimul( TotElecCoolingPwrSimul ),
			CoolingRateSimul( CoolingRateSimul ),
			CHWInletTempSimul( CHWInletTempSimul ),
			GLHEInletTempSimul( GLHEInletTempSimul ),
			CHWOutletTempSimul( CHWOutletTempSimul ),
			GLHEOutletTempSimul( GLHEOutletTempSimul ),
			CHWmdotSimul( CHWmdotSimul ),
			GLHEmdotSimul( GLHEmdotSimul )
		{}

	};

	// Object Data
	extern FArray1D< WrapperSpecs > Wrapper;
	extern FArray1D< ChillerHeaterSpecs > ChillerHeater;
	extern FArray1D< CHReportVars > ChillerHeaterReport;
	extern FArray1D< WrapperReportVars > WrapperReport;

	// Functions

	void
	SimCentralGroundSourceHeatPump(
		std::string const & WrapperName, // User specified name of wrapper
		int const EquipFlowCtrl, // Flow control mode for the equipment
		int & CompIndex, // Chiller number pointer
		int const LoopNum, // plant loop index pointer
		bool const RunFlag, // Simulate chiller when TRUE
		bool const FirstIteration, // Initialize variables when TRUE
		bool & InitLoopEquip, // If not zero, calculate the max load for operating conditions
		Real64 & MyLoad, // Loop demand component will meet [W]
		Real64 & MaxCap, // Maximum operating capacity of chiller [W]
		Real64 & MinCap, // Minimum operating capacity of chiller [W]
		Real64 & OptCap, // Optimal operating capacity of chiller [W]
		bool const GetSizingFactor, // TRUE when just the sizing factor is requested
		Real64 & SizingFactor // sizing factor
	);

	void
	SizeWrapper( int const WrapperNum );

	void
	GetWrapperInput();

	void
	GetChillerHeaterInput();

	void
	InitWrapper(
		int const WrapperNum, // Number of the current wrapper being simulated
		bool const RunFlag, // TRUE when chiller operating
		bool const FirstIteration, // Initialize variables when TRUE
		Real64 const MyLoad, // Demand Load
		int const LoopNum // Loop Number Index
	);

	void
	CalcChillerModel(
		int const WrapperNum, // Number of wrapper
		int const OpMode, // Operation mode
		Real64 & MyLoad, // Operating load
		bool const RunFlag, // TRUE when chiller operating
		bool const FirstIteration, // TRUE when first iteration of timestep
		int const EquipFlowCtrl, // Flow control mode for the equipment
		int const LoopNum // Plant loop number
	);

	void
	CalcChillerHeaterModel(
		int const WrapperNum, // Wrapper number pointor
		int const OpMode, // Operation mode
		Real64 & MyLoad, // Heating load plant should meet
		bool const RunFlag, // TRUE when chiller operating
		bool const FirstIteration, // TRUE when first iteration of timestep
		int const EquipFlowCtrl, // Flow control mode for the equipment
		int const LoopNum // Loop number
	);

	void
	CalcWrapperModel(
		int const WrapperNum,
		Real64 & MyLoad,
		bool const RunFlag,
		bool const FirstIteration,
		int const EquipFlowCtrl,
		int const LoopNum
	);

	void
	UpdateChillerRecords( int const WrapperNum ); // Wrapper number

	void
	UpdateChillerHeaterRecords( int const WrapperNum ); // Wrapper number

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

} // PlantCentralGSHP

} // EnergyPlus

#endif
