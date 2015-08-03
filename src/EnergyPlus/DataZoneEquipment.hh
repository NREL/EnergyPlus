#ifndef DataZoneEquipment_hh_INCLUDED
#define DataZoneEquipment_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>
#include <ObjexxFCL/Optional.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>
#include <DataGlobals.hh>

namespace EnergyPlus {

namespace DataZoneEquipment {

	// Using/Aliasing
	using DataGlobals::NumOfZones;

	// Data
	// -only module should be available to other modules and routines.
	// Thus, all variables in this module must be PUBLIC.

	// MODULE PARAMETER DEFINITIONS:
	extern int const PathInlet;
	extern int const CompInlet;
	extern int const Intermediate;
	extern int const Outlet;

	extern int const ZoneSplitter_Type;
	extern int const ZoneSupplyPlenum_Type;
	extern int const ZoneMixer_Type;
	extern int const ZoneReturnPlenum_Type;

	// Start zone equip objects
	// list units that are valid for zone system availability managers first
	extern int const FanCoil4Pipe_Num;
	extern int const PkgTermHPAirToAir_Num;
	extern int const PkgTermACAirToAir_Num;
	extern int const PkgTermHPWaterToAir_Num;
	extern int const WindowAC_Num;
	extern int const UnitHeater_Num;
	extern int const UnitVentilator_Num;
	extern int const ERVStandAlone_Num;
	extern int const VentilatedSlab_Num;
	extern int const OutdoorAirUnit_Num;
	extern int const VRFTerminalUnit_Num;
	extern int const PurchasedAir_Num;
	extern int const ZoneEvaporativeCoolerUnit_Num; // #13, last zone equipment type to use zone availability manager. The above list must not change or NumValidSysAvailZoneComponents(13) must also change.
	extern int const AirDistUnit_Num;
	extern int const DirectAir_Num;
	extern int const BBWaterConvective_Num;
	extern int const BBElectricConvective_Num;
	extern int const HiTempRadiant_Num;
	extern int const LoTempRadiant_Num;
	extern int const ZoneExhaustFan_Num;
	extern int const HeatXchngr_Num;
	extern int const HPWaterHeater_Num;
	extern int const BBWater_Num;
	extern int const ZoneDXDehumidifier_Num;
	extern int const BBSteam_Num;
	extern int const BBElectric_Num;
	extern int const RefrigerationAirChillerSet_Num;
	extern int const UserDefinedZoneHVACForcedAir_Num;
	extern int const ZoneUnitarySystem_Num; // AirloopHVAC:UnitarySystem configured as zone equipment
	extern int const TotalNumZoneEquipType;
	// **NOTE**... if you add another zone equipment object, then increment
	// TotalNumZoneEquipType above to match the total number of zone equipment types
	// End zone equip objects

	extern int const NumValidSysAvailZoneComponents;
	extern Array1D_string const cValidSysAvailManagerCompTypes;

	// DERIVED TYPE DEFINITIONS:

	// MODULE VARIABLE DECLARATIONS:
	extern int NumSupplyAirPaths;
	extern int NumReturnAirPaths;
	extern bool ZoneEquipInputsFilled;
	extern bool ZoneEquipSimulatedOnce;
	extern int NumOfZoneEquipLists; // The Number of Zone Equipment List objects
	extern Array1D_int ZoneEquipAvail;

	// moved from HVACManager.hh to avoid circular call, B Nigusse, 05/14
	extern Array1D_bool CrossMixingReportFlag; // TRUE when Cross Mixing is active based on controls
	extern Array1D_bool MixingReportFlag; // TRUE when Mixing is active based on controls
	extern Array1D< Real64 > VentMCP; // product of mass rate and Cp for each Venitlation object

	// Utility routines for module

	// Types

	struct EquipMeterData
	{
		// Members
		std::string ReportVarName;
		std::string ReportVarUnits;
		int ResourceType;
		std::string EndUse;
		int EndUse_CompMode;
		std::string Group;
		int ReportVarIndex;
		int ReportVarIndexType;
		int ReportVarType;
		Real64 CurMeterReading;

		// Default Constructor
		EquipMeterData() :
			ResourceType( 0 ),
			EndUse_CompMode( 0 ),
			ReportVarIndex( 0 ),
			ReportVarIndexType( 0 ),
			ReportVarType( 0 ),
			CurMeterReading( 0.0 )
		{}

		// Member Constructor
		EquipMeterData(
			std::string const & ReportVarName,
			std::string const & ReportVarUnits,
			int const ResourceType,
			std::string const & EndUse,
			int const EndUse_CompMode,
			std::string const & Group,
			int const ReportVarIndex,
			int const ReportVarIndexType,
			int const ReportVarType,
			Real64 const CurMeterReading
		) :
			ReportVarName( ReportVarName ),
			ReportVarUnits( ReportVarUnits ),
			ResourceType( ResourceType ),
			EndUse( EndUse ),
			EndUse_CompMode( EndUse_CompMode ),
			Group( Group ),
			ReportVarIndex( ReportVarIndex ),
			ReportVarIndexType( ReportVarIndexType ),
			ReportVarType( ReportVarType ),
			CurMeterReading( CurMeterReading )
		{}

	};

	struct SubSubEquipmentData // data for an individual component
	{
		// Members
		std::string TypeOf; // The 'keyWord' identifying  component type
		std::string Name; // Component name
		int EquipIndex; // Component Index for routines
		bool ON; // When true, the designated component or operation scheme is available
		int InletNodeNum;
		int OutletNodeNum;
		int NumMeteredVars;
		Array1D< EquipMeterData > MeteredVar; // Index of energy output report data
		int EnergyTransComp; // 1=EnergyTransfer, 0=No EnergyTransfer  Flag needed for reporting
		int ZoneEqToPlantPtr; // 0=No plant loop connection, >=0 index to ZoneEqToPlant array
		int OpMode;
		Real64 Capacity;
		Real64 Efficiency;
		Real64 TotPlantSupplyElec;
		Real64 PlantSupplyElecEff;
		Real64 PeakPlantSupplyElecEff;
		Real64 TotPlantSupplyGas;
		Real64 PlantSupplyGasEff;
		Real64 PeakPlantSupplyGasEff;
		Real64 TotPlantSupplyPurch;
		Real64 PlantSupplyPurchEff;
		Real64 PeakPlantSupplyPurchEff;
		Real64 TotPlantSupplyOther;
		Real64 PlantSupplyOtherEff;
		Real64 PeakPlantSupplyOtherEff;

		// Default Constructor
		SubSubEquipmentData() :
			EquipIndex( 0 ),
			ON( true ),
			InletNodeNum( 0 ),
			OutletNodeNum( 0 ),
			NumMeteredVars( 0 ),
			EnergyTransComp( 0 ),
			ZoneEqToPlantPtr( 0 ),
			OpMode( 0 ),
			Capacity( 0.0 ),
			Efficiency( 0.0 ),
			TotPlantSupplyElec( 0.0 ),
			PlantSupplyElecEff( 0.0 ),
			PeakPlantSupplyElecEff( 0.0 ),
			TotPlantSupplyGas( 0.0 ),
			PlantSupplyGasEff( 0.0 ),
			PeakPlantSupplyGasEff( 0.0 ),
			TotPlantSupplyPurch( 0.0 ),
			PlantSupplyPurchEff( 0.0 ),
			PeakPlantSupplyPurchEff( 0.0 ),
			TotPlantSupplyOther( 0.0 ),
			PlantSupplyOtherEff( 0.0 ),
			PeakPlantSupplyOtherEff( 0.0 )
		{}

		// Member Constructor
		SubSubEquipmentData(
			std::string const & TypeOf, // The 'keyWord' identifying  component type
			std::string const & Name, // Component name
			int const EquipIndex, // Component Index for routines
			bool const ON, // When true, the designated component or operation scheme is available
			int const InletNodeNum,
			int const OutletNodeNum,
			int const NumMeteredVars,
			Array1< EquipMeterData > const & MeteredVar, // Index of energy output report data
			int const EnergyTransComp, // 1=EnergyTransfer, 0=No EnergyTransfer  Flag needed for reporting
			int const ZoneEqToPlantPtr, // 0=No plant loop connection, >=0 index to ZoneEqToPlant array
			int const OpMode,
			Real64 const Capacity,
			Real64 const Efficiency,
			Real64 const TotPlantSupplyElec,
			Real64 const PlantSupplyElecEff,
			Real64 const PeakPlantSupplyElecEff,
			Real64 const TotPlantSupplyGas,
			Real64 const PlantSupplyGasEff,
			Real64 const PeakPlantSupplyGasEff,
			Real64 const TotPlantSupplyPurch,
			Real64 const PlantSupplyPurchEff,
			Real64 const PeakPlantSupplyPurchEff,
			Real64 const TotPlantSupplyOther,
			Real64 const PlantSupplyOtherEff,
			Real64 const PeakPlantSupplyOtherEff
		) :
			TypeOf( TypeOf ),
			Name( Name ),
			EquipIndex( EquipIndex ),
			ON( ON ),
			InletNodeNum( InletNodeNum ),
			OutletNodeNum( OutletNodeNum ),
			NumMeteredVars( NumMeteredVars ),
			MeteredVar( MeteredVar ),
			EnergyTransComp( EnergyTransComp ),
			ZoneEqToPlantPtr( ZoneEqToPlantPtr ),
			OpMode( OpMode ),
			Capacity( Capacity ),
			Efficiency( Efficiency ),
			TotPlantSupplyElec( TotPlantSupplyElec ),
			PlantSupplyElecEff( PlantSupplyElecEff ),
			PeakPlantSupplyElecEff( PeakPlantSupplyElecEff ),
			TotPlantSupplyGas( TotPlantSupplyGas ),
			PlantSupplyGasEff( PlantSupplyGasEff ),
			PeakPlantSupplyGasEff( PeakPlantSupplyGasEff ),
			TotPlantSupplyPurch( TotPlantSupplyPurch ),
			PlantSupplyPurchEff( PlantSupplyPurchEff ),
			PeakPlantSupplyPurchEff( PeakPlantSupplyPurchEff ),
			TotPlantSupplyOther( TotPlantSupplyOther ),
			PlantSupplyOtherEff( PlantSupplyOtherEff ),
			PeakPlantSupplyOtherEff( PeakPlantSupplyOtherEff )
		{}

	};

	struct SubEquipmentData // data for an individual component
	{
		// Members
		bool Parent; // When true, the designated component is made up of sub-components
		int NumSubSubEquip;
		std::string TypeOf; // The 'keyWord' identifying  component type
		std::string Name; // Component name
		int EquipIndex; // Component Index for routines
		bool ON; // When true, the designated component or operation scheme is available
		int InletNodeNum;
		int OutletNodeNum;
		int NumMeteredVars;
		Array1D< EquipMeterData > MeteredVar; // Index of energy output report data
		Array1D< SubSubEquipmentData > SubSubEquipData; // Component list
		int EnergyTransComp; // 1=EnergyTransfer, 0=No EnergyTransfer  Flag needed for reporting
		int ZoneEqToPlantPtr; // 0=No plant loop connection, >0 index to ZoneEqToPlant array
		int OpMode;
		Real64 Capacity;
		Real64 Efficiency;
		Real64 TotPlantSupplyElec;
		Real64 PlantSupplyElecEff;
		Real64 PeakPlantSupplyElecEff;
		Real64 TotPlantSupplyGas;
		Real64 PlantSupplyGasEff;
		Real64 PeakPlantSupplyGasEff;
		Real64 TotPlantSupplyPurch;
		Real64 PlantSupplyPurchEff;
		Real64 PeakPlantSupplyPurchEff;
		Real64 TotPlantSupplyOther;
		Real64 PlantSupplyOtherEff;
		Real64 PeakPlantSupplyOtherEff;

		// Default Constructor
		SubEquipmentData() :
			Parent( false ),
			NumSubSubEquip( 0 ),
			EquipIndex( 0 ),
			ON( true ),
			InletNodeNum( 0 ),
			OutletNodeNum( 0 ),
			NumMeteredVars( 0 ),
			EnergyTransComp( 0 ),
			ZoneEqToPlantPtr( 0 ),
			OpMode( 0 ),
			Capacity( 0.0 ),
			Efficiency( 0.0 ),
			TotPlantSupplyElec( 0.0 ),
			PlantSupplyElecEff( 0.0 ),
			PeakPlantSupplyElecEff( 0.0 ),
			TotPlantSupplyGas( 0.0 ),
			PlantSupplyGasEff( 0.0 ),
			PeakPlantSupplyGasEff( 0.0 ),
			TotPlantSupplyPurch( 0.0 ),
			PlantSupplyPurchEff( 0.0 ),
			PeakPlantSupplyPurchEff( 0.0 ),
			TotPlantSupplyOther( 0.0 ),
			PlantSupplyOtherEff( 0.0 ),
			PeakPlantSupplyOtherEff( 0.0 )
		{}

		// Member Constructor
		SubEquipmentData(
			bool const Parent, // When true, the designated component is made up of sub-components
			int const NumSubSubEquip,
			std::string const & TypeOf, // The 'keyWord' identifying  component type
			std::string const & Name, // Component name
			int const EquipIndex, // Component Index for routines
			bool const ON, // When true, the designated component or operation scheme is available
			int const InletNodeNum,
			int const OutletNodeNum,
			int const NumMeteredVars,
			Array1< EquipMeterData > const & MeteredVar, // Index of energy output report data
			Array1< SubSubEquipmentData > const & SubSubEquipData, // Component list
			int const EnergyTransComp, // 1=EnergyTransfer, 0=No EnergyTransfer  Flag needed for reporting
			int const ZoneEqToPlantPtr, // 0=No plant loop connection, >0 index to ZoneEqToPlant array
			int const OpMode,
			Real64 const Capacity,
			Real64 const Efficiency,
			Real64 const TotPlantSupplyElec,
			Real64 const PlantSupplyElecEff,
			Real64 const PeakPlantSupplyElecEff,
			Real64 const TotPlantSupplyGas,
			Real64 const PlantSupplyGasEff,
			Real64 const PeakPlantSupplyGasEff,
			Real64 const TotPlantSupplyPurch,
			Real64 const PlantSupplyPurchEff,
			Real64 const PeakPlantSupplyPurchEff,
			Real64 const TotPlantSupplyOther,
			Real64 const PlantSupplyOtherEff,
			Real64 const PeakPlantSupplyOtherEff
		) :
			Parent( Parent ),
			NumSubSubEquip( NumSubSubEquip ),
			TypeOf( TypeOf ),
			Name( Name ),
			EquipIndex( EquipIndex ),
			ON( ON ),
			InletNodeNum( InletNodeNum ),
			OutletNodeNum( OutletNodeNum ),
			NumMeteredVars( NumMeteredVars ),
			MeteredVar( MeteredVar ),
			SubSubEquipData( SubSubEquipData ),
			EnergyTransComp( EnergyTransComp ),
			ZoneEqToPlantPtr( ZoneEqToPlantPtr ),
			OpMode( OpMode ),
			Capacity( Capacity ),
			Efficiency( Efficiency ),
			TotPlantSupplyElec( TotPlantSupplyElec ),
			PlantSupplyElecEff( PlantSupplyElecEff ),
			PeakPlantSupplyElecEff( PeakPlantSupplyElecEff ),
			TotPlantSupplyGas( TotPlantSupplyGas ),
			PlantSupplyGasEff( PlantSupplyGasEff ),
			PeakPlantSupplyGasEff( PeakPlantSupplyGasEff ),
			TotPlantSupplyPurch( TotPlantSupplyPurch ),
			PlantSupplyPurchEff( PlantSupplyPurchEff ),
			PeakPlantSupplyPurchEff( PeakPlantSupplyPurchEff ),
			TotPlantSupplyOther( TotPlantSupplyOther ),
			PlantSupplyOtherEff( PlantSupplyOtherEff ),
			PeakPlantSupplyOtherEff( PeakPlantSupplyOtherEff )
		{}

	};

	struct AirIn
	{
		// Members
		int InNode; // Air distribution unit inlet node
		int OutNode; // Air distribution unit Outlet node
		bool SupplyAirPathExists;
		int MainBranchIndex;
		int SupplyBranchIndex;
		int AirDistUnitIndex; // equipment number in EquipList
		int SupplyAirPathIndex;
		Real64 NetBranchCoilDemand;
		Array1D< SubSubEquipmentData > Coil;

		// Default Constructor
		AirIn() :
			InNode( 0 ),
			OutNode( 0 ),
			SupplyAirPathExists( false ),
			MainBranchIndex( 0 ),
			SupplyBranchIndex( 0 ),
			AirDistUnitIndex( 0 ),
			SupplyAirPathIndex( 0 ),
			NetBranchCoilDemand( 0.0 )
		{}

		// Member Constructor
		AirIn(
			int const InNode, // Air distribution unit inlet node
			int const OutNode, // Air distribution unit Outlet node
			bool const SupplyAirPathExists,
			int const MainBranchIndex,
			int const SupplyBranchIndex,
			int const AirDistUnitIndex, // equipment number in EquipList
			int const SupplyAirPathIndex,
			Real64 const NetBranchCoilDemand,
			Array1< SubSubEquipmentData > const & Coil
		) :
			InNode( InNode ),
			OutNode( OutNode ),
			SupplyAirPathExists( SupplyAirPathExists ),
			MainBranchIndex( MainBranchIndex ),
			SupplyBranchIndex( SupplyBranchIndex ),
			AirDistUnitIndex( AirDistUnitIndex ),
			SupplyAirPathIndex( SupplyAirPathIndex ),
			NetBranchCoilDemand( NetBranchCoilDemand ),
			Coil( Coil )
		{}

	};

	struct EquipConfiguration
	{
		// Members
		std::string ZoneName;
		int ActualZoneNum; // index into the Zone data
		std::string EquipListName;
		int EquipListIndex;
		std::string ControlListName;
		int ZoneNode;
		int ReturnAirNode;
		int NumInletNodes;
		int NumExhaustNodes;
		bool FlowError; // flow error flag
		Array1D_int InletNode; // zone supply air inlet nodes
		Array1D_int ExhaustNode; // zone air exhaust nodes
		int ReturnZonePlenumCondNum; // number of the zone's return air plenum
		int AirLoopNum; // the air loop index for this controlled zone
		int FanOpMode; // =0 if no central sys;
		// -1 if central sys is in cycling fan mode;
		// =2 if central sysis in constant fan mode.
		bool ZonalSystemOnly; // TRUE if served by a zonal system (only)
		bool IsControlled; // True when this is a controlled zone.
		Real64 ZoneExh; // zone exhaust (unbalanced+balanced) mass flow rate [kg/s]
		Real64 ZoneExhBalanced; // balanced zone exhaust mass flow rate [kg/s]
		Real64 PlenumMassFlow; // zone air mass flow rate induced from plenum [kg/s]
		// AirDistUnitCool and AirDistUnitHeat
		// do not correspond with the AIR DISTRIBUTION UNIT object in the zone equipment list.
		// AirDistUnitCool/AirDistUnitHeat, may represent a DIRECT AIR object,
		// or the cold/hot side of AIR DISTRIBUTION
		// UNIT object.  That is both AirDistUnitHeat and AirDistUnitCool are required to describe a dual
		// duct AIR DISTRIBUTION object in the ZoneEquipList.  Although only one AIR DISTRIBUTION UNIT is
		// allowed in ZoneEquipList, two instances of that object may exist in this data structure
		Array1D< AirIn > AirDistUnitHeat; // dimensioned to number of zone inlet nodes
		Array1D< AirIn > AirDistUnitCool; // dimensioned to number of zone inlet nodes.
		bool SupLeakToRetPlen; // True if there is supply duct leak to the
		// plenum (simple duct leakage model)
		bool InFloorActiveElement; // Convection adapation, true if zone has in-floor HVAC
		bool InWallActiveElement; // Convection adapation, true if zone has in-wall HVAC
		bool InCeilingActiveElement; // Convection adapation,
		// true when zone has in-ceiling HVAC

		// Default Constructor
		EquipConfiguration() :
			ZoneName( "Uncontrolled Zone" ),
			ActualZoneNum( 0 ),
			EquipListIndex( 0 ),
			ZoneNode( 0 ),
			ReturnAirNode( 0 ),
			NumInletNodes( 0 ),
			NumExhaustNodes( 0 ),
			FlowError( false ),
			ReturnZonePlenumCondNum( 0 ),
			AirLoopNum( 0 ),
			FanOpMode( 0 ),
			ZonalSystemOnly( false ),
			IsControlled( false ),
			ZoneExh( 0.0 ),
			ZoneExhBalanced( 0.0 ),
			PlenumMassFlow( 0.0 ),
			SupLeakToRetPlen( false ),
			InFloorActiveElement( false ),
			InWallActiveElement( false ),
			InCeilingActiveElement( false )
		{}

		// Member Constructor
		EquipConfiguration(
			std::string const & ZoneName,
			int const ActualZoneNum, // index into the Zone data
			std::string const & EquipListName,
			int const EquipListIndex,
			std::string const & ControlListName,
			int const ZoneNode,
			int const ReturnAirNode,
			int const NumInletNodes,
			int const NumExhaustNodes,
			bool const FlowError, // flow error flag
			Array1_int const & InletNode, // zone supply air inlet nodes
			Array1_int const & ExhaustNode, // zone air exhaust nodes
			int const ReturnZonePlenumCondNum, // number of the zone's return air plenum
			int const AirLoopNum, // the air loop index for this controlled zone
			int const FanOpMode, // =0 if no central sys;
			bool const ZonalSystemOnly, // TRUE if served by a zonal system (only)
			bool const IsControlled, // True when this is a controlled zone.
			Real64 const ZoneExh, // zone exhaust (unbalanced+balanced) mass flow rate [kg/s]
			Real64 const ZoneExhBalanced, // balanced zone exhaust mass flow rate [kg/s]
			Real64 const PlenumMassFlow, // zone air mass flow rate induced from plenum [kg/s]
			Array1< AirIn > const & AirDistUnitHeat, // dimensioned to number of zone inlet nodes
			Array1< AirIn > const & AirDistUnitCool, // dimensioned to number of zone inlet nodes.
			bool const SupLeakToRetPlen, // True if there is supply duct leak to the
			bool const InFloorActiveElement, // Convection adapation, true if zone has in-floor HVAC
			bool const InWallActiveElement, // Convection adapation, true if zone has in-wall HVAC
			bool const InCeilingActiveElement // Convection adapation,
		) :
			ZoneName( ZoneName ),
			ActualZoneNum( ActualZoneNum ),
			EquipListName( EquipListName ),
			EquipListIndex( EquipListIndex ),
			ControlListName( ControlListName ),
			ZoneNode( ZoneNode ),
			ReturnAirNode( ReturnAirNode ),
			NumInletNodes( NumInletNodes ),
			NumExhaustNodes( NumExhaustNodes ),
			FlowError( FlowError ),
			InletNode( InletNode ),
			ExhaustNode( ExhaustNode ),
			ReturnZonePlenumCondNum( ReturnZonePlenumCondNum ),
			AirLoopNum( AirLoopNum ),
			FanOpMode( FanOpMode ),
			ZonalSystemOnly( ZonalSystemOnly ),
			IsControlled( IsControlled ),
			ZoneExh( ZoneExh ),
			ZoneExhBalanced( ZoneExhBalanced ),
			PlenumMassFlow( PlenumMassFlow ),
			AirDistUnitHeat( AirDistUnitHeat ),
			AirDistUnitCool( AirDistUnitCool ),
			SupLeakToRetPlen( SupLeakToRetPlen ),
			InFloorActiveElement( InFloorActiveElement ),
			InWallActiveElement( InWallActiveElement ),
			InCeilingActiveElement( InCeilingActiveElement )
		{}

	};

	struct EquipmentData // data for an individual component
	{
		// Members
		bool Parent; // When true, the designated component is made up of sub-components
		int NumSubEquip;
		std::string TypeOf; // The 'keyWord' identifying  component type
		std::string Name; // Component name
		bool ON; // When true, the designated component or operation scheme is available
		int NumInlets;
		int NumOutlets;
		Array1D_int InletNodeNums;
		Array1D_int OutletNodeNums;
		int NumMeteredVars;
		Array1D< EquipMeterData > MeteredVar; // Index of energy output report data
		Array1D< SubEquipmentData > SubEquipData; // Component list
		int EnergyTransComp; // 1=EnergyTransfer, 0=No EnergyTransfer  Flag needed for reporting
		int ZoneEqToPlantPtr; // 0=No plant loop connection, >0 index to ZoneEqToPlant array
		Real64 TotPlantSupplyElec;
		Real64 PlantSupplyElecEff;
		Real64 PeakPlantSupplyElecEff;
		Real64 TotPlantSupplyGas;
		Real64 PlantSupplyGasEff;
		Real64 PeakPlantSupplyGasEff;
		Real64 TotPlantSupplyPurch;
		Real64 PlantSupplyPurchEff;
		Real64 PeakPlantSupplyPurchEff;
		Real64 TotPlantSupplyOther;
		Real64 PlantSupplyOtherEff;
		Real64 PeakPlantSupplyOtherEff;
		Real64 Capacity;
		int OpMode;

		// Default Constructor
		EquipmentData() :
			Parent( false ),
			NumSubEquip( 0 ),
			ON( true ),
			NumInlets( 0 ),
			NumOutlets( 0 ),
			NumMeteredVars( 0 ),
			EnergyTransComp( 0 ),
			ZoneEqToPlantPtr( 0 ),
			TotPlantSupplyElec( 0.0 ),
			PlantSupplyElecEff( 0.0 ),
			PeakPlantSupplyElecEff( 0.0 ),
			TotPlantSupplyGas( 0.0 ),
			PlantSupplyGasEff( 0.0 ),
			PeakPlantSupplyGasEff( 0.0 ),
			TotPlantSupplyPurch( 0.0 ),
			PlantSupplyPurchEff( 0.0 ),
			PeakPlantSupplyPurchEff( 0.0 ),
			TotPlantSupplyOther( 0.0 ),
			PlantSupplyOtherEff( 0.0 ),
			PeakPlantSupplyOtherEff( 0.0 ),
			Capacity( 0.0 ),
			OpMode( 0 )
		{}

		// Member Constructor
		EquipmentData(
			bool const Parent, // When true, the designated component is made up of sub-components
			int const NumSubEquip,
			std::string const & TypeOf, // The 'keyWord' identifying  component type
			std::string const & Name, // Component name
			bool const ON, // When true, the designated component or operation scheme is available
			int const NumInlets,
			int const NumOutlets,
			Array1_int const & InletNodeNums,
			Array1_int const & OutletNodeNums,
			int const NumMeteredVars,
			Array1< EquipMeterData > const & MeteredVar, // Index of energy output report data
			Array1< SubEquipmentData > const & SubEquipData, // Component list
			int const EnergyTransComp, // 1=EnergyTransfer, 0=No EnergyTransfer  Flag needed for reporting
			int const ZoneEqToPlantPtr, // 0=No plant loop connection, >0 index to ZoneEqToPlant array
			Real64 const TotPlantSupplyElec,
			Real64 const PlantSupplyElecEff,
			Real64 const PeakPlantSupplyElecEff,
			Real64 const TotPlantSupplyGas,
			Real64 const PlantSupplyGasEff,
			Real64 const PeakPlantSupplyGasEff,
			Real64 const TotPlantSupplyPurch,
			Real64 const PlantSupplyPurchEff,
			Real64 const PeakPlantSupplyPurchEff,
			Real64 const TotPlantSupplyOther,
			Real64 const PlantSupplyOtherEff,
			Real64 const PeakPlantSupplyOtherEff,
			Real64 const Capacity,
			int const OpMode
		) :
			Parent( Parent ),
			NumSubEquip( NumSubEquip ),
			TypeOf( TypeOf ),
			Name( Name ),
			ON( ON ),
			NumInlets( NumInlets ),
			NumOutlets( NumOutlets ),
			InletNodeNums( InletNodeNums ),
			OutletNodeNums( OutletNodeNums ),
			NumMeteredVars( NumMeteredVars ),
			MeteredVar( MeteredVar ),
			SubEquipData( SubEquipData ),
			EnergyTransComp( EnergyTransComp ),
			ZoneEqToPlantPtr( ZoneEqToPlantPtr ),
			TotPlantSupplyElec( TotPlantSupplyElec ),
			PlantSupplyElecEff( PlantSupplyElecEff ),
			PeakPlantSupplyElecEff( PeakPlantSupplyElecEff ),
			TotPlantSupplyGas( TotPlantSupplyGas ),
			PlantSupplyGasEff( PlantSupplyGasEff ),
			PeakPlantSupplyGasEff( PeakPlantSupplyGasEff ),
			TotPlantSupplyPurch( TotPlantSupplyPurch ),
			PlantSupplyPurchEff( PlantSupplyPurchEff ),
			PeakPlantSupplyPurchEff( PeakPlantSupplyPurchEff ),
			TotPlantSupplyOther( TotPlantSupplyOther ),
			PlantSupplyOtherEff( PlantSupplyOtherEff ),
			PeakPlantSupplyOtherEff( PeakPlantSupplyOtherEff ),
			Capacity( Capacity ),
			OpMode( OpMode )
		{}

	};

	struct EquipList
	{
		// Members
		std::string Name; // Name of the equipment list
		int NumOfEquipTypes; // Number of items on this list
		Array1D_string EquipType;
		Array1D_int EquipType_Num;
		Array1D_string EquipName;
		Array1D_int EquipIndex;
		Array1D_int CoolingPriority;
		Array1D_int HeatingPriority;
		Array1D< EquipmentData > EquipData; // Index of energy output report data

		// Default Constructor
		EquipList() :
			NumOfEquipTypes( 0 )
		{}

		// Member Constructor
		EquipList(
			std::string const & Name, // Name of the equipment list
			int const NumOfEquipTypes, // Number of items on this list
			Array1_string const & EquipType,
			Array1_int const & EquipType_Num,
			Array1_string const & EquipName,
			Array1_int const & EquipIndex,
			Array1_int const & CoolingPriority,
			Array1_int const & HeatingPriority,
			Array1< EquipmentData > const & EquipData // Index of energy output report data
		) :
			Name( Name ),
			NumOfEquipTypes( NumOfEquipTypes ),
			EquipType( EquipType ),
			EquipType_Num( EquipType_Num ),
			EquipName( EquipName ),
			EquipIndex( EquipIndex ),
			CoolingPriority( CoolingPriority ),
			HeatingPriority( HeatingPriority ),
			EquipData( EquipData )
		{}

	};

	struct ControlList
	{
		// Members
		std::string Name;
		int NumOfControls;
		Array1D_string ControlType;
		Array1D_string ControlName;

		// Default Constructor
		ControlList() :
			NumOfControls( 0 )
		{}

		// Member Constructor
		ControlList(
			std::string const & Name,
			int const NumOfControls,
			Array1_string const & ControlType,
			Array1_string const & ControlName
		) :
			Name( Name ),
			NumOfControls( NumOfControls ),
			ControlType( ControlType ),
			ControlName( ControlName )
		{}

	};

	struct SupplyAir
	{
		// Members
		std::string Name;
		int NumOfComponents;
		int InletNodeNum;
		Array1D_string ComponentType;
		Array1D_int ComponentType_Num;
		Array1D_string ComponentName;
		Array1D_int ComponentIndex;
		Array1D_int SplitterIndex;
		Array1D_int PlenumIndex;
		int NumOutletNodes;
		Array1D_int OutletNode;
		int NumNodes;
		Array1D_int Node;
		Array1D_int NodeType;

		// Default Constructor
		SupplyAir() :
			NumOfComponents( 0 ),
			InletNodeNum( 0 ),
			NumOutletNodes( 0 ),
			NumNodes( 0 )
		{}

		// Member Constructor
		SupplyAir(
			std::string const & Name,
			int const NumOfComponents,
			int const InletNodeNum,
			Array1_string const & ComponentType,
			Array1_int const & ComponentType_Num,
			Array1_string const & ComponentName,
			Array1_int const & ComponentIndex,
			Array1_int const & SplitterIndex,
			Array1_int const & PlenumIndex,
			int const NumOutletNodes,
			Array1_int const & OutletNode,
			int const NumNodes,
			Array1_int const & Node,
			Array1_int const & NodeType
		) :
			Name( Name ),
			NumOfComponents( NumOfComponents ),
			InletNodeNum( InletNodeNum ),
			ComponentType( ComponentType ),
			ComponentType_Num( ComponentType_Num ),
			ComponentName( ComponentName ),
			ComponentIndex( ComponentIndex ),
			SplitterIndex( SplitterIndex ),
			PlenumIndex( PlenumIndex ),
			NumOutletNodes( NumOutletNodes ),
			OutletNode( OutletNode ),
			NumNodes( NumNodes ),
			Node( Node ),
			NodeType( NodeType )
		{}

	};

	struct ReturnAir
	{
		// Members
		std::string Name;
		int NumOfComponents;
		int OutletNodeNum;
		Array1D_string ComponentType;
		Array1D_int ComponentType_Num;
		Array1D_string ComponentName;
		Array1D_int ComponentIndex;

		// Default Constructor
		ReturnAir() :
			NumOfComponents( 0 ),
			OutletNodeNum( 0 )
		{}

		// Member Constructor
		ReturnAir(
			std::string const & Name,
			int const NumOfComponents,
			int const OutletNodeNum,
			Array1_string const & ComponentType,
			Array1_int const & ComponentType_Num,
			Array1_string const & ComponentName,
			Array1_int const & ComponentIndex
		) :
			Name( Name ),
			NumOfComponents( NumOfComponents ),
			OutletNodeNum( OutletNodeNum ),
			ComponentType( ComponentType ),
			ComponentType_Num( ComponentType_Num ),
			ComponentName( ComponentName ),
			ComponentIndex( ComponentIndex )
		{}

	};

	// Object Data
	extern Array1D< EquipConfiguration > ZoneEquipConfig;
	extern Array1D< EquipList > ZoneEquipList;
	extern Array1D< ControlList > HeatingControlList;
	extern Array1D< ControlList > CoolingControlList;
	extern Array1D< SupplyAir > SupplyAirPath;
	extern Array1D< ReturnAir > ReturnAirPath;

	// Functions

	void
	GetZoneEquipmentData();

	void
	GetZoneEquipmentData1();

	void
	SetupZoneEquipmentForConvectionFlowRegime();

	bool
	CheckZoneEquipmentList(
		std::string const & ComponentType, // Type of component
		std::string const & ComponentName, // Name of component
		Optional_int CtrlZoneNum = _
	);

	int
	GetControlledZoneIndex( std::string const & ZoneName ); // Zone name to match into Controlled Zone structure

	int
	FindControlledZoneIndexFromSystemNodeNumberForZone( int const TrialZoneNodeNum ); // Node number to match into Controlled Zone structure

	int
	GetSystemNodeNumberForZone( std::string const & ZoneName ); // Zone name to match into Controlled Zone structure

	int
	GetReturnAirNodeForZone( std::string const & ZoneName ); // Zone name to match into Controlled Zone structure

	Real64
	CalcDesignSpecificationOutdoorAir(
		int const DSOAPtr, // Pointer to DesignSpecification:OutdoorAir object
		int const ActualZoneNum, // Zone index
		bool const UseOccSchFlag, // Zone occupancy schedule will be used instead of using total zone occupancy
		bool const UseMinOASchFlag, // Use min OA schedule in DesignSpecification:OutdoorAir object
		Optional_bool_const PerPersonNotSet = _, // when calculation should not include occupants (e.g., dual duct)
		Optional_bool_const MaxOAVolFlowFlag = _ // TRUE when calculation uses occupancy schedule  (e.g., dual duct)
	);

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

} // DataZoneEquipment

} // EnergyPlus

#endif
