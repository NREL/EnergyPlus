// EnergyPlus, Copyright (c) 1996-2022, The Board of Trustees of the University of Illinois,
// The Regents of the University of California, through Lawrence Berkeley National Laboratory
// (subject to receipt of any required approvals from the U.S. Dept. of Energy), Oak Ridge
// National Laboratory, managed by UT-Battelle, Alliance for Sustainable Energy, LLC, and other
// contributors. All rights reserved.
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
//     similar designation, without the U.S. Department of Energy's prior written consent.
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

#ifndef DataZoneEquipment_hh_INCLUDED
#define DataZoneEquipment_hh_INCLUDED

// C++ Headers
#include <unordered_set>

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>
#include <ObjexxFCL/Optional.hh>

// EnergyPlus Headers
#include <EnergyPlus/Data/BaseData.hh>
#include <EnergyPlus/DataGlobals.hh>
#include <EnergyPlus/DataHVACSystems.hh>
#include <EnergyPlus/EnergyPlus.hh>
#include <EnergyPlus/OutputProcessor.hh>
#include <EnergyPlus/SystemReports.hh>

namespace EnergyPlus {

// Forward declarations
struct EnergyPlusData;

namespace DataZoneEquipment {

    // Using/Aliasing
    // Data
    // -only module should be available to other modules and routines.
    // Thus, all variables in this module must be PUBLIC.

    // MODULE PARAMETER DEFINITIONS:
    enum class AirNodeType
    {
        Invalid = -1,
        PathInlet,
        CompInlet,
        Intermediate,
        Outlet,
        Num
    };

    enum class AirLoopHVACZone
    {
        Invalid = -1,
        Splitter,
        SupplyPlenum,
        Mixer,
        ReturnPlenum,
        Num
    };

    constexpr std::array<std::string_view, static_cast<int>(AirLoopHVACZone::Num)> AirLoopHVACTypeNamesCC = {
        "AirLoopHVAC:ZoneSplitter", "AirLoopHVAC:SupplyPlenum", "AirLoopHVAC:ZoneMixer", "AirLoopHVAC:ReturnPlenum"};

    constexpr std::array<std::string_view, static_cast<int>(AirLoopHVACZone::Num)> AirLoopHVACTypeNamesUC = {
        "AIRLOOPHVAC:ZONESPLITTER", "AIRLOOPHVAC:SUPPLYPLENUM", "AIRLOOPHVAC:ZONEMIXER", "AIRLOOPHVAC:RETURNPLENUM"};

    // Start zone equip objects
    // list units that are valid for zone system availability managers first
    enum ZoneEquip
    {
        Invalid = -1,
        FanCoil4Pipe = 1,
        PkgTermHPAirToAir,
        PkgTermACAirToAir,
        PkgTermHPWaterToAir,
        WindowAC,
        UnitHeater,
        UnitVentilator,
        ERVStandAlone,
        VentilatedSlab,
        OutdoorAirUnit,
        VRFTerminalUnit,
        PurchasedAir,
        ZoneEvaporativeCoolerUnit,
        ZoneHybridEvaporativeCooler, // last zone equipment type to use zone availability manager. The above list must not change or
                                     // NumValidSysAvailZoneComponents must also change.
        AirDistUnit,
        BBWaterConvective,
        BBElectricConvective,
        HiTempRadiant,
        LoTempRadiant,
        ZoneExhaustFan,
        HeatXchngr,
        HPWaterHeater,
        BBWater,
        ZoneDXDehumidifier,
        BBSteam,
        BBElectric,
        RefrigerationAirChillerSet,
        UserDefinedZoneHVACForcedAir,
        CoolingPanel,
        ZoneUnitarySys,
        Num
    };

    constexpr int NumValidSysAvailZoneComponents(14);
    extern Array1D_string const cValidSysAvailManagerCompTypes;

    // Per Person Ventilation Rate Mode
    enum class PerPersonVentRateMode
    {
        Invalid = -1,
        DCVByCurrentLevel,
        ByDesignLevel,
        Num
    };

    enum class LoadDist
    {
        Invalid = -1,
        Sequential,
        Uniform,
        UniformPLR,
        SequentialUniformPLR,
        Num
    };

    enum class LightReturnExhaustConfig : int
    {
        Invalid = -1,
        NoExhast = 0, // No exhaust node
        Single = 1,   // One to one configuration
        Multi = 2,    // Multiple return node referred
        Shared = 3,   // Shared exhaust node
        Num
    };

    struct EquipMeterData
    {
        // Members
        std::string ReportVarName;
        OutputProcessor::Unit ReportVarUnits;
        DataGlobalConstants::ResourceType ResourceType;
        std::string EndUse;
        SystemReports::EndUseType EndUse_CompMode;
        std::string Group;
        int ReportVarIndex;
        OutputProcessor::TimeStepType ReportVarIndexType;
        OutputProcessor::VariableType ReportVarType;
        Real64 CurMeterReading;

        // Default Constructor
        EquipMeterData()
            : ReportVarUnits(OutputProcessor::Unit::None), ResourceType(DataGlobalConstants::ResourceType::None),
              EndUse_CompMode(SystemReports::EndUseType::NoHeatNoCool), ReportVarIndex(0), ReportVarIndexType(OutputProcessor::TimeStepType::Zone),
              ReportVarType(OutputProcessor::VariableType::NotFound), CurMeterReading(0.0)
        {
        }
    };

    struct SubSubEquipmentData // data for an individual component
    {
        // Members
        std::string TypeOf; // The 'keyWord' identifying  component type
        std::string Name;   // Component name
        int EquipIndex;     // Component Index for routines
        bool ON;            // When true, the designated component or operation scheme is available
        int InletNodeNum;
        int OutletNodeNum;
        int NumMeteredVars;
        Array1D<EquipMeterData> MeteredVar; // Index of energy output report data
        int EnergyTransComp;                // 1=EnergyTransfer, 0=No EnergyTransfer  Flag needed for reporting
        int ZoneEqToPlantPtr;               // 0=No plant loop connection, >=0 index to ZoneEqToPlant array
        int OpMode;
        Real64 Capacity;
        Real64 Efficiency;
        Real64 TotPlantSupplyElec;
        Real64 TotPlantSupplyGas;
        Real64 TotPlantSupplyPurch;

        // Default Constructor
        SubSubEquipmentData()
            : EquipIndex(0), ON(true), InletNodeNum(0), OutletNodeNum(0), NumMeteredVars(0), EnergyTransComp(0), ZoneEqToPlantPtr(0), OpMode(0),
              Capacity(0.0), Efficiency(0.0), TotPlantSupplyElec(0.0), TotPlantSupplyGas(0.0), TotPlantSupplyPurch(0.0)
        {
        }
    };

    struct SubEquipmentData // data for an individual component
    {
        // Members
        bool Parent; // When true, the designated component is made up of sub-components
        int NumSubSubEquip;
        std::string TypeOf; // The 'keyWord' identifying  component type
        std::string Name;   // Component name
        int EquipIndex;     // Component Index for routines
        bool ON;            // When true, the designated component or operation scheme is available
        int InletNodeNum;
        int OutletNodeNum;
        int NumMeteredVars;
        Array1D<EquipMeterData> MeteredVar;           // Index of energy output report data
        Array1D<SubSubEquipmentData> SubSubEquipData; // Component list
        int EnergyTransComp;                          // 1=EnergyTransfer, 0=No EnergyTransfer  Flag needed for reporting
        int ZoneEqToPlantPtr;                         // 0=No plant loop connection, >0 index to ZoneEqToPlant array
        int OpMode;
        Real64 Capacity;
        Real64 Efficiency;
        Real64 TotPlantSupplyElec;
        Real64 TotPlantSupplyGas;
        Real64 TotPlantSupplyPurch;

        // Default Constructor
        SubEquipmentData()
            : Parent(false), NumSubSubEquip(0), EquipIndex(0), ON(true), InletNodeNum(0), OutletNodeNum(0), NumMeteredVars(0), EnergyTransComp(0),
              ZoneEqToPlantPtr(0), OpMode(0), Capacity(0.0), Efficiency(0.0), TotPlantSupplyElec(0.0), TotPlantSupplyGas(0.0),
              TotPlantSupplyPurch(0.0)
        {
        }
    };

    struct AirIn
    {
        // Members
        int InNode;  // Air distribution unit inlet node
        int OutNode; // Air distribution unit Outlet node
        bool SupplyAirPathExists;
        int MainBranchIndex;
        int SupplyBranchIndex;
        int AirDistUnitIndex;    // equipment number in EquipList
        int TermUnitSizingIndex; // Pointer to TermUnitSizing and TermUnitFinalZoneSizing data for this terminal unit
        int SupplyAirPathIndex;
        Array1D<SubSubEquipmentData> Coil;

        // Default Constructor
        AirIn()
            : InNode(0), OutNode(0), SupplyAirPathExists(false), MainBranchIndex(0), SupplyBranchIndex(0), AirDistUnitIndex(0),
              TermUnitSizingIndex(0), SupplyAirPathIndex(0)
        {
        }
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
        int NumInletNodes;                // number of inlet nodes
        int NumExhaustNodes;              // number of exhaust nodes
        int NumReturnNodes;               // number of return air nodes
        int NumReturnFlowBasisNodes;      // number of return air flow basis nodes
        int ReturnFlowSchedPtrNum;        // return air flow fraction schedule pointer
        bool FlowError;                   // flow error flag
        Array1D_int InletNode;            // zone supply air inlet nodes
        Array1D_int InletNodeAirLoopNum;  // air loop number connected to this inlet node (0 if not an airloop node)
        Array1D_int InletNodeADUNum;      // AirDistUnit connected to this inlet node (0 if not an ADU node, could be zone equip or direct air)
        Array1D_int ExhaustNode;          // zone air exhaust nodes
        Array1D_int ReturnNode;           // zone return air nodes (node numbers)
        Array1D_int ReturnNodeAirLoopNum; // air loop number connected to this return node
        Array1D_int
            ReturnNodeInletNum; // zone supply air inlet index that matched this return node (same zone, same airloop) - not the inlet node number
        Array1D_bool FixedReturnFlow;         // true if return node is fixed and cannot be adjusted in CalcZoneReturnFlows
        Array1D_int ReturnNodePlenumNum;      // number of the return plenum attached to this return node (zero if none)
        Array1D_int ReturnFlowBasisNode;      // return air flow basis nodes
        Array1D_int ReturnNodeExhaustNodeNum; // Exhaust node number flow to a corrsponding return node due to light heat gain
        // Array1D_int SharedExhaustNode;        // Exhaust node number shared by return nodes 0 No exhaust; 1 No share; > 1 shared; -1 use the
        // exhaust node value
        Array1D<LightReturnExhaustConfig>
            SharedExhaustNode; // Exhaust node number shared by return nodes 0 No exhaust; 1 No share; > 1 shared; -1 use the exhaust node value

        bool ZonalSystemOnly;     // TRUE if served by a zonal system (only)
        bool IsControlled;        // True when this is a controlled zone.
        Real64 ZoneExh;           // zone exhaust (unbalanced+balanced) mass flow rate [kg/s]
        Real64 ZoneExhBalanced;   // balanced zone exhaust mass flow rate [kg/s]
        Real64 PlenumMassFlow;    // zone air mass flow rate induced from plenum [kg/s]
        Real64 ExcessZoneExh;     // excess zone exhaust to be balanced by other zones (only used when !ZoneAirMassFlow.EnforceZoneMassBalance) [kg/s]
        Real64 TotAvailAirLoopOA; // total airloop OA available for systems serving this zone (used to apportion excess exhaust) [kg/s}
        Real64 TotInletAirMassFlowRate;   // total inlet node mass flow rate [kg/s]
        Real64 TotExhaustAirMassFlowRate; // total exhaust node mass flow rate [kg/s]
        // AirDistUnitCool and AirDistUnitHeat
        // do not correspond with the AIR DISTRIBUTION UNIT object in the zone equipment list.
        // AirDistUnitCool/AirDistUnitHeat, may represent a DIRECT AIR object,
        // or the cold/hot side of AIR DISTRIBUTION
        // UNIT object.  That is both AirDistUnitHeat and AirDistUnitCool are required to describe a dual
        // duct AIR DISTRIBUTION object in the ZoneEquipList.
        Array1D<AirIn> AirDistUnitHeat; // dimensioned to number of zone inlet nodes
        Array1D<AirIn> AirDistUnitCool; // dimensioned to number of zone inlet nodes.
        bool InFloorActiveElement;      // Convection adapation, true if zone has in-floor HVAC
        bool InWallActiveElement;       // Convection adapation, true if zone has in-wall HVAC
        bool InCeilingActiveElement;    // Convection adapation,
        // true when zone has in-ceiling HVAC
        bool ZoneHasAirFlowWindowReturn; // true if zone has an airflow window (WindowProperty:AirflowControl) with destination=ReturnAir
        bool ZoneHasAirLoopWithOASys;    // true if zone is served by one or more airloops with an outdoor air system
        int ZoneAirDistributionIndex;    // index to DesignSpecification:ZoneAirDistribution object
        int ZoneDesignSpecOAIndex;       // index to DesignSpecification:OutdoorAir object
        Real64 AirLoopDesSupply;         // air lood design supply air flow rate [kg/s]

        // Default Constructor
        EquipConfiguration()
            : ZoneName("Uncontrolled Zone"), ActualZoneNum(0), EquipListIndex(0), ZoneNode(0), NumInletNodes(0), NumExhaustNodes(0),
              NumReturnNodes(0), NumReturnFlowBasisNodes(0), ReturnFlowSchedPtrNum(0), FlowError(false), ZonalSystemOnly(false), IsControlled(false),
              ZoneExh(0.0), ZoneExhBalanced(0.0), PlenumMassFlow(0.0), ExcessZoneExh(0.0), TotAvailAirLoopOA(0.0), TotInletAirMassFlowRate(0.0),
              TotExhaustAirMassFlowRate(0.0), InFloorActiveElement(false), InWallActiveElement(false), InCeilingActiveElement(false),
              ZoneHasAirFlowWindowReturn(false), ZoneHasAirLoopWithOASys(false), ZoneAirDistributionIndex(0), ZoneDesignSpecOAIndex(0),
              AirLoopDesSupply(0.0)
        {
        }
    };

    struct EquipmentData // data for an individual component
    {
        // Members
        bool Parent; // When true, the designated component is made up of sub-components
        int NumSubEquip;
        std::string TypeOf; // The 'keyWord' identifying  component type
        std::string Name;   // Component name
        bool ON;            // When true, the designated component or operation scheme is available
        int NumInlets;
        int NumOutlets;
        Array1D_int InletNodeNums;
        Array1D_int OutletNodeNums;
        int NumMeteredVars;
        Array1D<EquipMeterData> MeteredVar;     // Index of energy output report data
        Array1D<SubEquipmentData> SubEquipData; // Component list
        int EnergyTransComp;                    // 1=EnergyTransfer, 0=No EnergyTransfer  Flag needed for reporting
        int ZoneEqToPlantPtr;                   // 0=No plant loop connection, >0 index to ZoneEqToPlant array
        Real64 TotPlantSupplyElec;
        Real64 TotPlantSupplyGas;
        Real64 TotPlantSupplyPurch;
        int OpMode;

        // Default Constructor
        EquipmentData()
            : Parent(false), NumSubEquip(0), ON(true), NumInlets(0), NumOutlets(0), NumMeteredVars(0), EnergyTransComp(0), ZoneEqToPlantPtr(0),
              TotPlantSupplyElec(0.0), TotPlantSupplyGas(0.0), TotPlantSupplyPurch(0.0), OpMode(0)
        {
        }
    };

    struct EquipList
    {
        // Members
        std::string Name;                           // Name of the equipment list
        DataZoneEquipment::LoadDist LoadDistScheme; // load distribution scheme
        int NumOfEquipTypes;                        // Number of items on this list
        int NumAvailHeatEquip;                      // Number of pieces of equipment available for heating
        int NumAvailCoolEquip;                      // Number of pieces of equipment available for cooling
        Array1D_string EquipType;                   // TODO: Convert this from string to enum and remove EquipTypeEnum below
        Array1D<DataZoneEquipment::ZoneEquip> EquipTypeEnum;
        Array1D_string EquipName;
        Array1D_int EquipIndex;
        std::vector<HVACSystemData *> compPointer;
        Array1D_int CoolingPriority;
        Array1D_int HeatingPriority;
        Array1D_int SequentialCoolingFractionSchedPtr;
        Array1D_int SequentialHeatingFractionSchedPtr;
        Array1D_int CoolingCapacity;      // Current cooling capacity (negative) [W]
        Array1D_int HeatingCapacity;      // Current heating capacity (positive) [W]
        Array1D<EquipmentData> EquipData; // Index of energy output report data

        // Default Constructor
        EquipList() : LoadDistScheme(DataZoneEquipment::LoadDist::Sequential), NumOfEquipTypes(0), NumAvailHeatEquip(0), NumAvailCoolEquip(0)
        {
        }

        void getPrioritiesForInletNode(EnergyPlusData &state,
                                       int inletNodeNum,     // Zone inlet node number to match
                                       int &coolingPriority, // Cooling priority num for matching equipment
                                       int &heatingPriority  // Heating priority num for matching equipment
        );

        Real64 SequentialHeatingFraction(EnergyPlusData &state, int equipNum);

        Real64 SequentialCoolingFraction(EnergyPlusData &state, int equipNum);
    };

    struct ControlList
    {
        // Members
        std::string Name;
        int NumOfControls;
        Array1D_string ControlType;
        Array1D_string ControlName;

        // Default Constructor
        ControlList() : NumOfControls(0)
        {
        }
    };

    struct SupplyAir
    {
        // Members
        std::string Name;
        int NumOfComponents;
        int InletNodeNum;
        Array1D_string ComponentType; // TODO: Convert this from string to enum and remove ComponentTypeEnum below
        Array1D<DataZoneEquipment::AirLoopHVACZone> ComponentTypeEnum;
        Array1D_string ComponentName;
        Array1D_int ComponentIndex;
        Array1D_int SplitterIndex;
        Array1D_int PlenumIndex;
        int NumOutletNodes;
        Array1D_int OutletNode;
        int NumNodes;
        Array1D_int Node;
        Array1D<DataZoneEquipment::AirNodeType> NodeType;

        // Default Constructor
        SupplyAir() : NumOfComponents(0), InletNodeNum(0), NumOutletNodes(0), NumNodes(0)
        {
        }
    };

    struct ReturnAir
    {
        // Members
        std::string Name;
        int NumOfComponents;
        int OutletNodeNum;
        Array1D_string ComponentType; // TODO: Convert this from string to enum and remove ComponentTypeEnum below
        Array1D<DataZoneEquipment::AirLoopHVACZone> ComponentTypeEnum;
        Array1D_string ComponentName;
        Array1D_int ComponentIndex;

        // Default Constructor
        ReturnAir() : NumOfComponents(0), OutletNodeNum(0)
        {
        }
    };

    struct ExhaustAir
    {
        // Members
        std::string Name;
        int NumOfComponents;
        int OutletNodeNum;
        Array1D_string ComponentType; // TODO: Convert this from string to enum and remove ComponentTypeEnum below
        Array1D<DataZoneEquipment::AirLoopHVACZone> ComponentTypeEnum;
        Array1D_string ComponentName;
        Array1D_int ComponentIndex;

        // Default Constructor
        ExhaustAir() : NumOfComponents(0), OutletNodeNum(0)
        {
        }
    };

    void GetZoneEquipmentData(EnergyPlusData &state);

    void SetupZoneEquipmentForConvectionFlowRegime(EnergyPlusData &state);

    bool CheckZoneEquipmentList(EnergyPlusData &state,
                                std::string_view ComponentType, // Type of component
                                std::string_view ComponentName, // Name of component
                                Optional_int CtrlZoneNum = _);

    int GetControlledZoneIndex(EnergyPlusData &state, std::string const &ZoneName); // Zone name to match into Controlled Zone structure

    int FindControlledZoneIndexFromSystemNodeNumberForZone(EnergyPlusData &state,
                                                           int TrialZoneNodeNum); // Node number to match into Controlled Zone structure

    int GetSystemNodeNumberForZone(EnergyPlusData &state, std::string const &ZoneName); // Zone name to match into Controlled Zone structure

    int GetReturnAirNodeForZone(EnergyPlusData &state,
                                std::string const &ZoneName,             // Zone name to match into Controlled Zone structure
                                std::string const &NodeName,             // Return air node name to match (may be blank)
                                std::string const &calledFromDescription // String identifying the calling function and object
    );

    int GetReturnNumForZone(EnergyPlusData &state,
                            std::string const &ZoneName, // Zone name to match into Controlled Zone structure
                            std::string const &NodeName  // Return air node name to match (may be blank)
    );

    int GetZoneEquipControlledZoneNum(EnergyPlusData &state, DataZoneEquipment::ZoneEquip const ZoneEquipTypeNum, std::string const &EquipmentName);

    bool VerifyLightsExhaustNodeForZone(EnergyPlusData &state, int const ZoneNum, int const ZoneExhaustNodeNum);

    void CheckSharedExhaust(EnergyPlusData &state);

} // namespace DataZoneEquipment

struct DataZoneEquipmentData : BaseGlobalStruct
{

    bool GetZoneEquipmentDataErrorsFound = false;
    int GetZoneEquipmentDataFound = 0;
    int NumSupplyAirPaths = 0;
    int NumReturnAirPaths = 0;
    int NumExhaustAirSystems = 0;
    bool ZoneEquipInputsFilled = false;
    bool ZoneEquipSimulatedOnce = false;
    int NumOfZoneEquipLists = 0;
    Array1D_int ZoneEquipAvail;
    Array1D_bool CrossMixingReportFlag; // TRUE when Cross Mixing is active based on controls
    Array1D_bool MixingReportFlag;      // TRUE when Mixing is active based on controls
    Array1D<Real64> VentMCP;            // Product of mass rate and Cp for each Ventilation object
    Array1D<Real64> ZMAT;               // Zone air temperature for zone air mixing
    Array1D<Real64> ZHumRat;            // Zone air humidity ratio zone air mixing
    Array1D<DataZoneEquipment::EquipConfiguration> ZoneEquipConfig;
    std::unordered_set<std::string> UniqueZoneEquipListNames;
    Array1D<DataZoneEquipment::EquipList> ZoneEquipList;
    Array1D<DataZoneEquipment::SupplyAir> SupplyAirPath;
    Array1D<DataZoneEquipment::ReturnAir> ReturnAirPath;
    Array1D<DataZoneEquipment::ExhaustAir> ExhaustAirSystem;

    void clear_state() override
    {
        this->GetZoneEquipmentDataErrorsFound = false;
        this->GetZoneEquipmentDataFound = 0;
        this->NumSupplyAirPaths = 0;
        this->NumReturnAirPaths = 0;
        this->NumExhaustAirSystems = 0;
        this->ZoneEquipInputsFilled = false;
        this->ZoneEquipSimulatedOnce = false;
        this->NumOfZoneEquipLists = 0;
        this->ZoneEquipAvail.deallocate();
        this->CrossMixingReportFlag.deallocate();
        this->MixingReportFlag.deallocate();
        this->VentMCP.deallocate();
        this->ZMAT.deallocate();
        this->ZHumRat.deallocate();
        this->ZoneEquipConfig.deallocate();
        this->UniqueZoneEquipListNames.clear();
        this->ZoneEquipList.deallocate();
        this->SupplyAirPath.deallocate();
        this->ReturnAirPath.deallocate();
        this->ExhaustAirSystem.deallocate();
    }
};

} // namespace EnergyPlus

#endif
