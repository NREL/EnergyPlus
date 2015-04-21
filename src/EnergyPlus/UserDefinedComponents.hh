#ifndef UserDefinedComponents_hh_INCLUDED
#define UserDefinedComponents_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>
#include <DataGlobals.hh>
#include <DataPlant.hh>

namespace EnergyPlus {

namespace UserDefinedComponents {

	// Using/Aliasing
	using DataPlant::HowMet_Unknown;
	using DataPlant::LoopFlowStatus_Unknown;

	// Data
	// MODULE PARAMETER DEFINITIONS:
	// na

	// DERIVED TYPE DEFINITIONS:

	// MODULE VARIABLE DECLARATIONS:

	extern int NumUserPlantComps;
	extern int NumUserCoils;
	extern int NumUserZoneAir;
	extern int NumUserAirTerminals;

	extern Array1D_bool CheckUserPlantCompName;
	extern Array1D_bool CheckUserCoilName;
	extern Array1D_bool CheckUserZoneAirName;
	extern Array1D_bool CheckUserAirTerminal;
	extern bool GetInput;

	// SUBROUTINE SPECIFICATIONS FOR MODULE <module_name>:

	// Types

	struct PlantConnectionStruct
	{
		// Members
		int ErlInitProgramMngr; // points to an EMS:ProgramManager to run for setup and sizing
		int ErlSimProgramMngr; // points to an EMS:ProgramManager to run only when this connection is called
		int LoopNum; // plant loop connection index
		int LoopSideNum; // plant loop side connection index
		int BranchNum; // plant loop branch connection index
		int CompNum; // plant loop component connection index
		int InletNodeNum; // plant loop inlet node index
		int OutletNodeNum; // plant loop outlet node index
		int FlowPriority; // how component affects overall loop flow determination
		int HowLoadServed; // nature of component wrt to plant loop's loads
		Real64 LowOutTempLimit; // low limit for outlet temp if MEETSLOADWITHNOMINALCAPACITYLOWOUTLIMIT
		Real64 HiOutTempLimit; // hi limit for outlet temp if MEETSLOADWITHNOMINALCAPACITYHIOUTLIMIT
		Real64 MassFlowRateRequest; // request filled by actuator, might not be satisfied if plant constrained [kg/s]
		Real64 MassFlowRateMin; // filled by actuator, reports minimum (hardware) flow rate for component [kg/s]
		Real64 MassFlowRateMax; // filled by actuator, reports maximum (hardware) flow rate for component [kg/s]
		Real64 DesignVolumeFlowRate; // filled by actuator,
		Real64 MyLoad; // fills internal variable for user's model to know current load request of supply equip [W]
		Real64 MinLoad; // filled by actuator, reports back size for load dispatch routines [W]
		Real64 MaxLoad; // filled by actuator, reports back size for load dispatch [W]
		Real64 OptLoad; // filled by actuator, reports back size for load dispatch [W]
		Real64 InletRho; // fills internal variable, current density for fluid type and inlet temperature [kg/m3]
		Real64 InletCp; // fills internal Varaible, current specific heat for fluid type and inlet temperature [J/kg-C]
		Real64 InletTemp; // fills internal variable, current inlet fluid temperature [C]
		Real64 InletMassFlowRate; // fills internal variable, current inlet mass flow rate [kg/s]
		Real64 OutletTemp; // filled by actuator, componenent outlet temperature [C]

		// Default Constructor
		PlantConnectionStruct() :
			ErlInitProgramMngr( 0 ),
			ErlSimProgramMngr( 0 ),
			LoopNum( 0 ),
			LoopSideNum( 0 ),
			BranchNum( 0 ),
			CompNum( 0 ),
			InletNodeNum( 0 ),
			OutletNodeNum( 0 ),
			FlowPriority( LoopFlowStatus_Unknown ),
			HowLoadServed( HowMet_Unknown ),
			LowOutTempLimit( 0.0 ),
			HiOutTempLimit( 0.0 ),
			MassFlowRateRequest( 0.0 ),
			MassFlowRateMin( 0.0 ),
			MassFlowRateMax( 0.0 ),
			DesignVolumeFlowRate( 0.0 ),
			MyLoad( 0.0 ),
			MinLoad( 0.0 ),
			MaxLoad( 0.0 ),
			OptLoad( 0.0 ),
			InletRho( 0.0 ),
			InletCp( 0.0 ),
			InletTemp( 0.0 ),
			InletMassFlowRate( 0.0 ),
			OutletTemp( 0.0 )
		{}

		// Member Constructor
		PlantConnectionStruct(
			int const ErlInitProgramMngr, // points to an EMS:ProgramManager to run for setup and sizing
			int const ErlSimProgramMngr, // points to an EMS:ProgramManager to run only when this connection is called
			int const LoopNum, // plant loop connection index
			int const LoopSideNum, // plant loop side connection index
			int const BranchNum, // plant loop branch connection index
			int const CompNum, // plant loop component connection index
			int const InletNodeNum, // plant loop inlet node index
			int const OutletNodeNum, // plant loop outlet node index
			int const FlowPriority, // how component affects overall loop flow determination
			int const HowLoadServed, // nature of component wrt to plant loop's loads
			Real64 const LowOutTempLimit, // low limit for outlet temp if MEETSLOADWITHNOMINALCAPACITYLOWOUTLIMIT
			Real64 const HiOutTempLimit, // hi limit for outlet temp if MEETSLOADWITHNOMINALCAPACITYHIOUTLIMIT
			Real64 const MassFlowRateRequest, // request filled by actuator, might not be satisfied if plant constrained [kg/s]
			Real64 const MassFlowRateMin, // filled by actuator, reports minimum (hardware) flow rate for component [kg/s]
			Real64 const MassFlowRateMax, // filled by actuator, reports maximum (hardware) flow rate for component [kg/s]
			Real64 const DesignVolumeFlowRate, // filled by actuator,
			Real64 const MyLoad, // fills internal variable for user's model to know current load request of supply equip [W]
			Real64 const MinLoad, // filled by actuator, reports back size for load dispatch routines [W]
			Real64 const MaxLoad, // filled by actuator, reports back size for load dispatch [W]
			Real64 const OptLoad, // filled by actuator, reports back size for load dispatch [W]
			Real64 const InletRho, // fills internal variable, current density for fluid type and inlet temperature [kg/m3]
			Real64 const InletCp, // fills internal Varaible, current specific heat for fluid type and inlet temperature [J/kg-C]
			Real64 const InletTemp, // fills internal variable, current inlet fluid temperature [C]
			Real64 const InletMassFlowRate, // fills internal variable, current inlet mass flow rate [kg/s]
			Real64 const OutletTemp // filled by actuator, componenent outlet temperature [C]
		) :
			ErlInitProgramMngr( ErlInitProgramMngr ),
			ErlSimProgramMngr( ErlSimProgramMngr ),
			LoopNum( LoopNum ),
			LoopSideNum( LoopSideNum ),
			BranchNum( BranchNum ),
			CompNum( CompNum ),
			InletNodeNum( InletNodeNum ),
			OutletNodeNum( OutletNodeNum ),
			FlowPriority( FlowPriority ),
			HowLoadServed( HowLoadServed ),
			LowOutTempLimit( LowOutTempLimit ),
			HiOutTempLimit( HiOutTempLimit ),
			MassFlowRateRequest( MassFlowRateRequest ),
			MassFlowRateMin( MassFlowRateMin ),
			MassFlowRateMax( MassFlowRateMax ),
			DesignVolumeFlowRate( DesignVolumeFlowRate ),
			MyLoad( MyLoad ),
			MinLoad( MinLoad ),
			MaxLoad( MaxLoad ),
			OptLoad( OptLoad ),
			InletRho( InletRho ),
			InletCp( InletCp ),
			InletTemp( InletTemp ),
			InletMassFlowRate( InletMassFlowRate ),
			OutletTemp( OutletTemp )
		{}

	};

	struct AirConnectionStruct
	{
		// Members
		int InletNodeNum; // air inlet node index
		int OutletNodeNum; // air outlet node index
		Real64 InletRho; // fills internal variable, current inlet air density [kg/m3]
		Real64 InletCp; // fills internal variable, current inlet air specific heat [J/kg-c]
		Real64 InletTemp; // fills internal variable, current inlet air temperature [C]
		Real64 InletHumRat; // fills internal variable, current inlet air humidity ratio [kg/kg]
		Real64 InletMassFlowRate; // fills internal variable, current inlet air mass flow rate [kg/s]
		Real64 OutletTemp; // filled by actuator, component outlet temperature [C]
		Real64 OutletHumRat; // filled by actuator, component outlet humidity ratio [kg/kg]
		Real64 OutletMassFlowRate; // filled by actuator, component outlet mass flow rate [kg/s]

		// Default Constructor
		AirConnectionStruct() :
			InletNodeNum( 0 ),
			OutletNodeNum( 0 ),
			InletRho( 0.0 ),
			InletCp( 0.0 ),
			InletTemp( 0.0 ),
			InletHumRat( 0.0 ),
			InletMassFlowRate( 0.0 ),
			OutletTemp( 0.0 ),
			OutletHumRat( 0.0 ),
			OutletMassFlowRate( 0.0 )
		{}

		// Member Constructor
		AirConnectionStruct(
			int const InletNodeNum, // air inlet node index
			int const OutletNodeNum, // air outlet node index
			Real64 const InletRho, // fills internal variable, current inlet air density [kg/m3]
			Real64 const InletCp, // fills internal variable, current inlet air specific heat [J/kg-c]
			Real64 const InletTemp, // fills internal variable, current inlet air temperature [C]
			Real64 const InletHumRat, // fills internal variable, current inlet air humidity ratio [kg/kg]
			Real64 const InletMassFlowRate, // fills internal variable, current inlet air mass flow rate [kg/s]
			Real64 const OutletTemp, // filled by actuator, component outlet temperature [C]
			Real64 const OutletHumRat, // filled by actuator, component outlet humidity ratio [kg/kg]
			Real64 const OutletMassFlowRate // filled by actuator, component outlet mass flow rate [kg/s]
		) :
			InletNodeNum( InletNodeNum ),
			OutletNodeNum( OutletNodeNum ),
			InletRho( InletRho ),
			InletCp( InletCp ),
			InletTemp( InletTemp ),
			InletHumRat( InletHumRat ),
			InletMassFlowRate( InletMassFlowRate ),
			OutletTemp( OutletTemp ),
			OutletHumRat( OutletHumRat ),
			OutletMassFlowRate( OutletMassFlowRate )
		{}

	};

	struct WaterUseTankConnectionStruct // data for interacting with water use storage system
	{
		// Members
		bool SuppliedByWaterSystem;
		int SupplyTankID; // index "pointer" to WaterStorage structure
		int SupplyTankDemandARRID; // index "pointer" to demand array inside WaterStorage structure
		Real64 SupplyVdotRequest;
		bool CollectsToWaterSystem;
		int CollectionTankID; // index "pointer" to Storage TAnk array WaterStorage
		int CollectionTankSupplyARRID; // index pointe to supply Vdot array in WaterStorage
		Real64 CollectedVdot;

		// Default Constructor
		WaterUseTankConnectionStruct() :
			SuppliedByWaterSystem( false ),
			SupplyTankID( 0 ),
			SupplyTankDemandARRID( 0 ),
			SupplyVdotRequest( 0.0 ),
			CollectsToWaterSystem( false ),
			CollectionTankID( 0 ),
			CollectionTankSupplyARRID( 0 ),
			CollectedVdot( 0.0 )
		{}

		// Member Constructor
		WaterUseTankConnectionStruct(
			bool const SuppliedByWaterSystem,
			int const SupplyTankID, // index "pointer" to WaterStorage structure
			int const SupplyTankDemandARRID, // index "pointer" to demand array inside WaterStorage structure
			Real64 const SupplyVdotRequest,
			bool const CollectsToWaterSystem,
			int const CollectionTankID, // index "pointer" to Storage TAnk array WaterStorage
			int const CollectionTankSupplyARRID, // index pointe to supply Vdot array in WaterStorage
			Real64 const CollectedVdot
		) :
			SuppliedByWaterSystem( SuppliedByWaterSystem ),
			SupplyTankID( SupplyTankID ),
			SupplyTankDemandARRID( SupplyTankDemandARRID ),
			SupplyVdotRequest( SupplyVdotRequest ),
			CollectsToWaterSystem( CollectsToWaterSystem ),
			CollectionTankID( CollectionTankID ),
			CollectionTankSupplyARRID( CollectionTankSupplyARRID ),
			CollectedVdot( CollectedVdot )
		{}

	};

	struct ZoneInternalGainsStruct
	{
		// Members
		bool DeviceHasInternalGains;
		int ZoneNum;
		Real64 ConvectionGainRate;
		Real64 ReturnAirConvectionGainRate;
		Real64 ThermalRadiationGainRate;
		Real64 LatentGainRate;
		Real64 ReturnAirLatentGainRate;
		Real64 CarbonDioxideGainRate;
		Real64 GenericContamGainRate;

		// Default Constructor
		ZoneInternalGainsStruct() :
			DeviceHasInternalGains( false ),
			ZoneNum( 0 ),
			ConvectionGainRate( 0.0 ),
			ReturnAirConvectionGainRate( 0.0 ),
			ThermalRadiationGainRate( 0.0 ),
			LatentGainRate( 0.0 ),
			ReturnAirLatentGainRate( 0.0 ),
			CarbonDioxideGainRate( 0.0 ),
			GenericContamGainRate( 0.0 )
		{}

		// Member Constructor
		ZoneInternalGainsStruct(
			bool const DeviceHasInternalGains,
			int const ZoneNum,
			Real64 const ConvectionGainRate,
			Real64 const ReturnAirConvectionGainRate,
			Real64 const ThermalRadiationGainRate,
			Real64 const LatentGainRate,
			Real64 const ReturnAirLatentGainRate,
			Real64 const CarbonDioxideGainRate,
			Real64 const GenericContamGainRate
		) :
			DeviceHasInternalGains( DeviceHasInternalGains ),
			ZoneNum( ZoneNum ),
			ConvectionGainRate( ConvectionGainRate ),
			ReturnAirConvectionGainRate( ReturnAirConvectionGainRate ),
			ThermalRadiationGainRate( ThermalRadiationGainRate ),
			LatentGainRate( LatentGainRate ),
			ReturnAirLatentGainRate( ReturnAirLatentGainRate ),
			CarbonDioxideGainRate( CarbonDioxideGainRate ),
			GenericContamGainRate( GenericContamGainRate )
		{}

	};

	struct UserPlantComponentStruct
	{
		// Members
		std::string Name; // user identifier
		int ErlSimProgramMngr; // EMS:ProgramManager to always run when this model is called
		int NumPlantConnections; // count of how many plant loop connections there are
		Array1D< PlantConnectionStruct > Loop; // collect data for each plant loop connection
		AirConnectionStruct Air;
		WaterUseTankConnectionStruct Water;
		ZoneInternalGainsStruct Zone;

		// Default Constructor
		UserPlantComponentStruct() :
			ErlSimProgramMngr( 0 ),
			NumPlantConnections( 0 )
		{}

		// Member Constructor
		UserPlantComponentStruct(
			std::string const & Name, // user identifier
			int const ErlSimProgramMngr, // EMS:ProgramManager to always run when this model is called
			int const NumPlantConnections, // count of how many plant loop connections there are
			Array1< PlantConnectionStruct > const & Loop, // collect data for each plant loop connection
			AirConnectionStruct const & Air,
			WaterUseTankConnectionStruct const & Water,
			ZoneInternalGainsStruct const & Zone
		) :
			Name( Name ),
			ErlSimProgramMngr( ErlSimProgramMngr ),
			NumPlantConnections( NumPlantConnections ),
			Loop( Loop ),
			Air( Air ),
			Water( Water ),
			Zone( Zone )
		{}

	};

	struct UserCoilComponentStruct
	{
		// Members
		std::string Name; // user identifier
		int ErlSimProgramMngr; // EMS:ProgramManager to always run when this model is called
		int ErlInitProgramMngr; // EMS:ProgramManager to  run when this model is initialized and setup
		int NumAirConnections; // count of how many air connectiosn there are
		bool PlantIsConnected;
		Array1D< AirConnectionStruct > Air;
		PlantConnectionStruct Loop;
		WaterUseTankConnectionStruct Water;
		ZoneInternalGainsStruct Zone;

		// Default Constructor
		UserCoilComponentStruct() :
			ErlSimProgramMngr( 0 ),
			ErlInitProgramMngr( 0 ),
			NumAirConnections( 0 ),
			PlantIsConnected( false )
		{}

		// Member Constructor
		UserCoilComponentStruct(
			std::string const & Name, // user identifier
			int const ErlSimProgramMngr, // EMS:ProgramManager to always run when this model is called
			int const ErlInitProgramMngr, // EMS:ProgramManager to  run when this model is initialized and setup
			int const NumAirConnections, // count of how many air connectiosn there are
			bool const PlantIsConnected,
			Array1< AirConnectionStruct > const & Air,
			PlantConnectionStruct const & Loop,
			WaterUseTankConnectionStruct const & Water,
			ZoneInternalGainsStruct const & Zone
		) :
			Name( Name ),
			ErlSimProgramMngr( ErlSimProgramMngr ),
			ErlInitProgramMngr( ErlInitProgramMngr ),
			NumAirConnections( NumAirConnections ),
			PlantIsConnected( PlantIsConnected ),
			Air( Air ),
			Loop( Loop ),
			Water( Water ),
			Zone( Zone )
		{}

	};

	struct UserZoneHVACForcedAirComponentStruct
	{
		// Members
		std::string Name; // user identifier
		int ErlSimProgramMngr; // EMS:ProgramManager to always run when this model is called
		int ErlInitProgramMngr; // EMS:ProgramManager to  run when this model is initialized and setup
		AirConnectionStruct ZoneAir;
		AirConnectionStruct SourceAir;
		int NumPlantConnections; // count of how many plant loop (demand) connections there are
		Array1D< PlantConnectionStruct > Loop; // collect data for each plant loop connection
		WaterUseTankConnectionStruct Water;
		ZoneInternalGainsStruct Zone; // for skin losses
		Real64 RemainingOutputToHeatingSP; // sensible load remaining for device, to heating setpoint [W]
		Real64 RemainingOutputToCoolingSP; // sensible load remaining for device, negative means cooling [W]
		Real64 RemainingOutputReqToHumidSP; // latent load remaining for device, to humidification setpoint [kg/s]
		Real64 RemainingOutputReqToDehumidSP; // latent load remaining for device, Negative means dehumidify [kg/s]

		// Default Constructor
		UserZoneHVACForcedAirComponentStruct() :
			ErlSimProgramMngr( 0 ),
			ErlInitProgramMngr( 0 ),
			NumPlantConnections( 0 ),
			RemainingOutputToHeatingSP( 0.0 ),
			RemainingOutputToCoolingSP( 0.0 ),
			RemainingOutputReqToHumidSP( 0.0 ),
			RemainingOutputReqToDehumidSP( 0.0 )
		{}

		// Member Constructor
		UserZoneHVACForcedAirComponentStruct(
			std::string const & Name, // user identifier
			int const ErlSimProgramMngr, // EMS:ProgramManager to always run when this model is called
			int const ErlInitProgramMngr, // EMS:ProgramManager to  run when this model is initialized and setup
			AirConnectionStruct const & ZoneAir,
			AirConnectionStruct const & SourceAir,
			int const NumPlantConnections, // count of how many plant loop (demand) connections there are
			Array1< PlantConnectionStruct > const & Loop, // collect data for each plant loop connection
			WaterUseTankConnectionStruct const & Water,
			ZoneInternalGainsStruct const & Zone, // for skin losses
			Real64 const RemainingOutputToHeatingSP, // sensible load remaining for device, to heating setpoint [W]
			Real64 const RemainingOutputToCoolingSP, // sensible load remaining for device, negative means cooling [W]
			Real64 const RemainingOutputReqToHumidSP, // latent load remaining for device, to humidification setpoint [kg/s]
			Real64 const RemainingOutputReqToDehumidSP // latent load remaining for device, Negative means dehumidify [kg/s]
		) :
			Name( Name ),
			ErlSimProgramMngr( ErlSimProgramMngr ),
			ErlInitProgramMngr( ErlInitProgramMngr ),
			ZoneAir( ZoneAir ),
			SourceAir( SourceAir ),
			NumPlantConnections( NumPlantConnections ),
			Loop( Loop ),
			Water( Water ),
			Zone( Zone ),
			RemainingOutputToHeatingSP( RemainingOutputToHeatingSP ),
			RemainingOutputToCoolingSP( RemainingOutputToCoolingSP ),
			RemainingOutputReqToHumidSP( RemainingOutputReqToHumidSP ),
			RemainingOutputReqToDehumidSP( RemainingOutputReqToDehumidSP )
		{}

	};

	struct UserAirTerminalComponentStruct
	{
		// Members
		std::string Name; // user identifier
		int ActualCtrlZoneNum;
		int ErlSimProgramMngr; // EMS:ProgramManager to always run when this model is called
		int ErlInitProgramMngr; // EMS:ProgramManager to  run when this model is initialized and setup
		AirConnectionStruct AirLoop;
		AirConnectionStruct SourceAir;
		int NumPlantConnections; // count of how many plant loop (demand) connections there are
		Array1D< PlantConnectionStruct > Loop; // collect data for each plant loop connection
		WaterUseTankConnectionStruct Water;
		ZoneInternalGainsStruct Zone; // for skin losses
		Real64 RemainingOutputToHeatingSP; // sensible load remaining for device, to heating setpoint [W]
		Real64 RemainingOutputToCoolingSP; // sensible load remaining for device, negative means cooling [W]
		Real64 RemainingOutputReqToHumidSP; // latent load remaining for device, to humidification setpoint [kg/s]
		Real64 RemainingOutputReqToDehumidSP; // latent load remaining for device, Negative means dehumidify [kg/s]

		// Default Constructor
		UserAirTerminalComponentStruct() :
			ActualCtrlZoneNum( 0 ),
			ErlSimProgramMngr( 0 ),
			ErlInitProgramMngr( 0 ),
			NumPlantConnections( 0 ),
			RemainingOutputToHeatingSP( 0.0 ),
			RemainingOutputToCoolingSP( 0.0 ),
			RemainingOutputReqToHumidSP( 0.0 ),
			RemainingOutputReqToDehumidSP( 0.0 )
		{}

		// Member Constructor
		UserAirTerminalComponentStruct(
			std::string const & Name, // user identifier
			int const ActualCtrlZoneNum,
			int const ErlSimProgramMngr, // EMS:ProgramManager to always run when this model is called
			int const ErlInitProgramMngr, // EMS:ProgramManager to  run when this model is initialized and setup
			AirConnectionStruct const & AirLoop,
			AirConnectionStruct const & SourceAir,
			int const NumPlantConnections, // count of how many plant loop (demand) connections there are
			Array1< PlantConnectionStruct > const & Loop, // collect data for each plant loop connection
			WaterUseTankConnectionStruct const & Water,
			ZoneInternalGainsStruct const & Zone, // for skin losses
			Real64 const RemainingOutputToHeatingSP, // sensible load remaining for device, to heating setpoint [W]
			Real64 const RemainingOutputToCoolingSP, // sensible load remaining for device, negative means cooling [W]
			Real64 const RemainingOutputReqToHumidSP, // latent load remaining for device, to humidification setpoint [kg/s]
			Real64 const RemainingOutputReqToDehumidSP // latent load remaining for device, Negative means dehumidify [kg/s]
		) :
			Name( Name ),
			ActualCtrlZoneNum( ActualCtrlZoneNum ),
			ErlSimProgramMngr( ErlSimProgramMngr ),
			ErlInitProgramMngr( ErlInitProgramMngr ),
			AirLoop( AirLoop ),
			SourceAir( SourceAir ),
			NumPlantConnections( NumPlantConnections ),
			Loop( Loop ),
			Water( Water ),
			Zone( Zone ),
			RemainingOutputToHeatingSP( RemainingOutputToHeatingSP ),
			RemainingOutputToCoolingSP( RemainingOutputToCoolingSP ),
			RemainingOutputReqToHumidSP( RemainingOutputReqToHumidSP ),
			RemainingOutputReqToDehumidSP( RemainingOutputReqToDehumidSP )
		{}

	};

	// Object Data
	extern Array1D< UserPlantComponentStruct > UserPlantComp;
	extern Array1D< UserCoilComponentStruct > UserCoil;
	extern Array1D< UserZoneHVACForcedAirComponentStruct > UserZoneAirHVAC;
	extern Array1D< UserAirTerminalComponentStruct > UserAirTerminal;

	// Functions

	void
	SimUserDefinedPlantComponent(
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
	SimCoilUserDefined(
		std::string const & EquipName, // user name for component
		int & CompIndex,
		int const AirLoopNum,
		bool & HeatingActive,
		bool & CoolingActive
	);

	void
	SimZoneAirUserDefined(
		std::string const & CompName, // name of the packaged terminal heat pump
		int const ZoneNum, // number of zone being served
		Real64 & SensibleOutputProvided, // sensible capacity delivered to zone
		Real64 & LatentOutputProvided, // Latent add/removal  (kg/s), dehumid = negative
		int & CompIndex // index to zone hvac unit
	);

	void
	SimAirTerminalUserDefined(
		std::string const & CompName,
		bool const FirstHVACIteration,
		int const ZoneNum,
		int const ZoneNodeNum,
		int & CompIndex
	);

	void
	GetUserDefinedComponents();

	void
	InitPlantUserComponent(
		int const CompNum,
		int const LoopNum,
		Real64 const MyLoad
	);

	void
	InitCoilUserDefined( int const CompNum );

	void
	InitZoneAirUserDefined(
		int const CompNum,
		int const ZoneNum
	);

	void
	InitAirTerminalUserDefined(
		int const CompNum,
		int const ZoneNum
	);

	void
	ReportPlantUserComponent(
		int const CompNum,
		int const LoopNum
	);

	void
	ReportCoilUserDefined( int const CompNum );

	void
	ReportZoneAirUserDefined( int const CompNum );

	void
	ReportAirTerminalUserDefined( int const CompNum );

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

} // UserDefinedComponents

} // EnergyPlus

#endif
