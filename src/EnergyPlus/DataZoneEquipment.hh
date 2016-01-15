// EnergyPlus, Copyright (c) 1996-2016, The Board of Trustees of the University of Illinois and
// The Regents of the University of California, through Lawrence Berkeley National Laboratory
// (subject to receipt of any required approvals from the U.S. Dept. of Energy). All rights
// reserved.
//
// If you have questions about your rights to use or distribute this software, please contact
// Berkeley Lab's Innovation & Partnerships Office at IPO@lbl.gov.
//
// NOTICE: This Software was developed under funding from the U.S. Department of Energy and the
// U.S. Government consequently retains certain rights. As such, the U.S. Government has been
// granted for itself and others acting on its behalf a paid-up, nonexclusive, irrevocable,
// worldwide license in the Software to reproduce, distribute copies to the public, prepare
// derivative works, and perform publicly and display publicly, and to permit others to do so.
//
// Redistribution and use in source and binary forms, with or without modification, are permitted
// provided that the following conditions are met:
//
// (1) Redistributions of source code must retain the above copyright notice, this list of
//     conditions and the following disclaimer.
//
// (2) Redistributions in binary form must reproduce the above copyright notice, this list of
//     conditions and the following disclaimer in the documentation and/or other materials
//     provided with the distribution.
//
// (3) Neither the name of the University of California, Lawrence Berkeley National Laboratory,
//     the University of Illinois, U.S. Dept. of Energy nor the names of its contributors may be
//     used to endorse or promote products derived from this software without specific prior
//     written permission.
//
// (4) Use of EnergyPlus(TM) Name. If Licensee (i) distributes the software in stand-alone form
//     without changes from the version obtained under this License, or (ii) Licensee makes a
//     reference solely to the software portion of its product, Licensee must refer to the
//     software as "EnergyPlus version X" software, where "X" is the version number Licensee
//     obtained under this License and may not use a different name for the software. Except as
//     specifically required in this Section (4), Licensee shall not use in a company name, a
//     product name, in advertising, publicity, or other promotional activities any name, trade
//     name, trademark, logo, or other designation of "EnergyPlus", "E+", "e+" or confusingly
//     similar designation, without Lawrence Berkeley National Laboratory's prior written consent.
//
// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR
// IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY
// AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR
// CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
// CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
// SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
// THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
// OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
// POSSIBILITY OF SUCH DAMAGE.
//
// You are under no obligation whatsoever to provide any bug fixes, patches, or upgrades to the
// features, functionality or performance of the source code ("Enhancements") to anyone; however,
// if you choose to make your Enhancements available either publicly, or directly to Lawrence
// Berkeley National Laboratory, without imposing a separate written license agreement for such
// Enhancements, then you hereby grant the following license: a non-exclusive, royalty-free
// perpetual license to install, use, modify, prepare derivative works, incorporate into other
// computer software, distribute, and sublicense such enhancements or derivative works thereof,
// in binary and source code form.

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
	extern Array1D< Real64 > ZMAT; // Zone air temperature for zone air mixing
	extern Array1D< Real64 > ZHumRat; // Zone air humidity ratio zone air mixing
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
		int NumReturnFlowBasisNodes; // number of return air flow basis nodes
		int ReturnFlowSchedPtrNum; // return air flow fraction schedule pointer
		bool FlowError; // flow error flag
		Array1D_int InletNode; // zone supply air inlet nodes
		Array1D_int ExhaustNode; // zone air exhaust nodes
		Array1D_int ReturnFlowBasisNode; // return air flow basis nodes
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
		int ADUNum; // index of Air Distribution Unit
		int SDUNum; // index of Single Duct Uncontrolled

		// Default Constructor
		EquipConfiguration() :
			ZoneName( "Uncontrolled Zone" ),
			ActualZoneNum( 0 ),
			EquipListIndex( 0 ),
			ZoneNode( 0 ),
			ReturnAirNode( 0 ),
			NumInletNodes( 0 ),
			NumExhaustNodes( 0 ),
			NumReturnFlowBasisNodes( 0 ),
			ReturnFlowSchedPtrNum( 0 ),
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
			InCeilingActiveElement( false ),
			ADUNum( 0 ),
			SDUNum( 0 )
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

	};

	// Object Data
	extern Array1D< EquipConfiguration > ZoneEquipConfig;
	extern Array1D< EquipList > ZoneEquipList;
	extern Array1D< ControlList > HeatingControlList;
	extern Array1D< ControlList > CoolingControlList;
	extern Array1D< SupplyAir > SupplyAirPath;
	extern Array1D< ReturnAir > ReturnAirPath;

	// Functions
	// Clears the global data in DataZoneEquipment.
	// Needed for unit tests, should not be normally called.
	void
	clear_state();

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

} // DataZoneEquipment

} // EnergyPlus

#endif
