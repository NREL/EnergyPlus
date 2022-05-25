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

#ifndef DataHeatBalance_hh_INCLUDED
#define DataHeatBalance_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>
#include <ObjexxFCL/Array2D.hh>
#include <ObjexxFCL/Array3D.hh>
#include <ObjexxFCL/Array4D.hh>
#include <ObjexxFCL/Optional.hh>
#include <ObjexxFCL/Reference.hh>

// EnergyPlus Headers
#include <EnergyPlus/ConvectionConstants.hh>
#include <EnergyPlus/Data/BaseData.hh>
#include <EnergyPlus/DataComplexFenestration.hh>
#include <EnergyPlus/DataGlobals.hh>
#include <EnergyPlus/DataSurfaces.hh>
#include <EnergyPlus/DataVectorTypes.hh>
#include <EnergyPlus/DataWindowEquivalentLayer.hh>
#include <EnergyPlus/EPVector.hh>
#include <EnergyPlus/EnergyPlus.hh>
#include <EnergyPlus/ExteriorEnergyUse.hh>

namespace EnergyPlus {

// Forward declarations
struct EnergyPlusData;

namespace DataHeatBalance {

    // Using/Aliasing
    using namespace DataComplexFenestration;
    using DataComplexFenestration::GapDeflectionState;
    using DataComplexFenestration::GapSupportPillar;
    using DataComplexFenestration::WindowComplexShade;
    using DataComplexFenestration::WindowThermalModelParams;
    using DataSurfaces::MaxSlatAngs;
    using DataVectorTypes::Vector;

    // Parameters to indicate material group type for use with the Material
    // derived type (see below):
    enum class MaterialGroup
    {
        Invalid = -1,
        RegularMaterial,
        Air,
        Shade,
        WindowGlass,
        WindowGas,
        WindowBlind,
        WindowGasMixture,
        Screen,
        EcoRoof,
        IRTMaterial,
        WindowSimpleGlazing,
        ComplexWindowShade,
        ComplexWindowGap,
        GlassEquivalentLayer,
        ShadeEquivalentLayer,
        DrapeEquivalentLayer,
        BlindEquivalentLayer,
        ScreenEquivalentLayer,
        GapEquivalentLayer,
        Num
    };

    // Parameters for Interior and Exterior Solar Distribution
    enum class Shadowing
    {
        Invalid = -1,
        Minimal,              // all incoming solar hits floor, no exterior shadowing except reveals
        FullExterior,         // all incoming solar hits floor, full exterior shadowing
        FullInteriorExterior, // full interior solar distribution, full exterior solar shadowing
        FullExteriorWithRefl, // all incoming solar hits floor, full exterior shadowing and reflections
        Num
    };

    // Parameters to indicate the zone type for use with the Zone derived
    // type (see below--Zone%OfType):
    constexpr int StandardZone(1);

    // Parameters for WarmupDays
    constexpr int DefaultMaxNumberOfWarmupDays(25); // Default maximum number of warmup days allowed
    constexpr int DefaultMinNumberOfWarmupDays(1);  // Default minimum number of warmup days allowed

    // Parameters for Sky Radiance Distribution
    enum class SkyRadDistribution
    {
        Invalid = -1,
        Isotropic,
        Anisotropic,
        Num
    };

    // Parameters for ZoneAirSolutionAlgo
    enum class SolutionAlgo
    {
        Invalid = -1,
        ThirdOrder,
        AnalyticalSolution,
        EulerMethod,
        Num
    };

    // Parameter for MRT calculation type
    enum class CalcMRT
    {
        Invalid = -1,
        ZoneAveraged,
        SurfaceWeighted,
        AngleFactor,
        Num
    };

    // Parameters for Ventilation
    enum class VentilationType
    {
        Invalid = -1,
        Natural,
        Intake,
        Exhaust,
        Balanced,
        Num
    };

    // Parameters for hybrid ventilation using Ventilation and Mixing objects
    enum class HybridCtrlType
    {
        Invalid = -1,
        Indiv,
        Close,
        Global,
        Num
    };

    // System type, detailed refrigeration or refrigerated case rack
    enum class RefrigSystemType
    {
        Invalid = -1,
        Detailed,
        Rack,
        Num
    };

    // Refrigeration condenser type
    enum class RefrigCondenserType
    {
        Invalid = -1,
        Air,
        Evap,
        Water,
        Cascade,
        WaterHeater,
        Num
    };

    // Parameters for type of infiltration model
    enum class InfiltrationModelType
    {
        Invalid = -1,
        DesignFlowRate,
        ShermanGrimsrud,
        AIM2,
        Num
    };

    // Parameters for type of ventilation model
    enum class VentilationModelType
    {
        Invalid = -1,
        DesignFlowRate,
        WindAndStack,
        Num
    };

    // Parameters for type of zone air balance model
    enum class AirBalance
    {
        Invalid = -1,
        None,
        Quadrature,
        Num
    };

    // Parameter for source zone air flow mass balance infiltration treatment
    enum class InfiltrationFlow
    {
        Invalid = -1,
        No,
        Add,
        Adjust,
        Num
    };

    enum class InfiltrationZoneType
    {
        Invalid = -1,
        MixingSourceZonesOnly,
        AllZones,
        Num
    };

    // zone air flow balancing method
    enum class AdjustmentType
    {
        Invalid = -1,
        AdjustMixingOnly,
        AdjustReturnOnly,
        AdjustMixingThenReturn,
        AdjustReturnThenMixing,
        NoAdjustReturnAndMixing,
        Num
    };

    enum class IntGainType
    {
        Invalid = -1,
        People,
        Lights,
        ElectricEquipment,
        GasEquipment,
        HotWaterEquipment,
        SteamEquipment,
        OtherEquipment,
        ZoneBaseboardOutdoorTemperatureControlled,
        ZoneContaminantSourceAndSinkCarbonDioxide,
        WaterUseEquipment,
        DaylightingDeviceTubular,
        WaterHeaterMixed,
        WaterHeaterStratified,
        ThermalStorageChilledWaterMixed,
        ThermalStorageChilledWaterStratified,
        GeneratorFuelCell,
        GeneratorMicroCHP,
        ElectricLoadCenterTransformer,
        ElectricLoadCenterInverterSimple,
        ElectricLoadCenterInverterFunctionOfPower,
        ElectricLoadCenterInverterLookUpTable,
        ElectricLoadCenterStorageLiIonNmcBattery,
        ElectricLoadCenterStorageBattery,
        ElectricLoadCenterStorageSimple,
        PipeIndoor,
        RefrigerationCase,
        RefrigerationCompressorRack,
        RefrigerationSystemAirCooledCondenser,
        RefrigerationTransSysAirCooledGasCooler,
        RefrigerationSystemSuctionPipe,
        RefrigerationTransSysSuctionPipeMT,
        RefrigerationTransSysSuctionPipeLT,
        RefrigerationSecondaryReceiver,
        RefrigerationSecondaryPipe,
        RefrigerationWalkIn,
        Pump_VarSpeed,
        Pump_ConSpeed,
        Pump_Cond,
        PumpBank_VarSpeed,
        PumpBank_ConSpeed,
        ZoneContaminantSourceAndSinkGenericContam,
        PlantComponentUserDefined,
        CoilUserDefined,
        ZoneHVACForcedAirUserDefined,
        AirTerminalUserDefined,
        PackagedTESCoilTank,
        ElectricEquipmentITEAirCooled,
        SecCoolingDXCoilSingleSpeed,
        SecHeatingDXCoilSingleSpeed,
        SecCoolingDXCoilTwoSpeed,
        SecCoolingDXCoilMultiSpeed,
        SecHeatingDXCoilMultiSpeed,
        ElectricLoadCenterConverter,
        FanSystemModel,
        Num
    };

    static constexpr std::array<std::string_view, static_cast<int>(DataHeatBalance::AirBalance::Num)> AirBalanceTypeNamesUC = {"NONE", "QUADRATURE"};

    static constexpr std::array<std::string_view, static_cast<int>(DataHeatBalance::InfiltrationFlow::Num)> InfiltrationFlowTypeNamesUC = {
        "NONE", "ADDINFILTRATIONFLOW", "ADJUSTINFILTRATIONFLOW"};

    static constexpr std::array<std::string_view, static_cast<int>(DataHeatBalance::InfiltrationFlow::Num)> InfiltrationFlowTypeNamesCC = {
        "None", "AddInfiltrationFlow", "AdjustInfiltrationFlow"};

    static constexpr std::array<std::string_view, static_cast<int>(DataHeatBalance::InfiltrationZoneType::Num)> InfiltrationZoneTypeNamesUC = {
        "MIXINGSOURCEZONESONLY", "ALLZONES"};

    static constexpr std::array<std::string_view, static_cast<int>(DataHeatBalance::InfiltrationZoneType::Num)> InfiltrationZoneTypeNamesCC = {
        "MixingSourceZonesOnly", "AllZones"};

    static constexpr std::array<std::string_view, static_cast<int>(DataHeatBalance::AdjustmentType::Num)> AdjustmentTypeNamesUC = {
        "ADJUSTMIXINGONLY", "ADJUSTRETURNONLY", "ADJUSTMIXINGTHENRETURN", "ADJUSTRETURNTHENMIXING", "NONE"};

    static constexpr std::array<std::string_view, static_cast<int>(DataHeatBalance::AdjustmentType::Num)> AdjustmentTypeNamesCC = {
        "AdjustMixingOnly", "AdjustReturnOnly", "AdjustMixingThenReturn", "AdjustReturnThenMixing", "None"};

    static constexpr std::array<std::string_view, static_cast<int>(DataHeatBalance::IntGainType::Num)> IntGainTypeNamesUC = {
        "PEOPLE",
        "LIGHTS",
        "ELECTRICEQUIPMENT",
        "GASEQUIPMENT",
        "HOTWATEREQUIPMENT",
        "STEAMEQUIPMENT",
        "OTHEREQUIPMENT",
        "ZONEBASEBOARD:OUTDOORTEMPERATURECONTROLLED",
        "ZONECONTAMINANTSOURCEANDSINK:CARBONDIOXIDE",
        "WATERUSE:EQUIPMENT",
        "DAYLIGHTINGDEVICE:TUBULAR",
        "WATERHEATER:MIXED",
        "WATERHEATER:STRATIFIED",
        "THERMALSTORAGE:CHILLEDWATER:MIXED",
        "THERMALSTORAGE:CHILLEDWATER:STRATIFIED",
        "GENERATOR:FUELCELL",
        "GENERATOR:MICROCHP",
        "ELECTRICLOADCENTER:TRANSFORMER",
        "ELECTRICLOADCENTER:INVERTER:SIMPLE",
        "ELECTRICLOADCENTER:INVERTER:FUNCTIONOFPOWER",
        "ELECTRICLOADCENTER:INVERTER:LOOKUPTABLE",
        "ELECTRICLOADCENTER:STORAGE:LIIONNMCBATTERY",
        "ELECTRICLOADCENTER:STORAGE:BATTERY",
        "ELECTRICLOADCENTER:STORAGE:SIMPLE",
        "PIPE:INDOOR",
        "REFRIGERATION:CASE",
        "REFRIGERATION:COMPRESSORRACK",
        "REFRIGERATION:SYSTEM:CONDENSER:AIRCOOLED",
        "REFRIGERATION:TRANSCRITICALSYSTEM:GASCOOLER:AIRCOOLED",
        "REFRIGERATION:SYSTEM:SUCTIONPIPE",
        "REFRIGERATION:TRANSCRITICALSYSTEM:SUCTIONPIPEMT",
        "REFRIGERATION:TRANSCRITICALSYSTEM:SUCTIONPIPELT",
        "REFRIGERATION:SECONDARYSYSTEM:RECEIVER",
        "REFRIGERATION:SECONDARYSYSTEM:PIPE",
        "REFRIGERATION:WALKIN",
        "PUMP:VARIABLESPEED",
        "PUMP:CONSTANTSPEED",
        "PUMP:VARIABLESPEED:CONDENSATE",
        "HEADEREDPUMPS:VARIABLESPEED",
        "HEADEREDPUMPS:CONSTANTSPEED",
        "ZONECONTAMINANTSOURCEANDSINK:GENERICCONTAMINANT",
        "PLANTCOMPONENT:USERDEFINED",
        "COIL:USERDEFINED",
        "ZONEHVAC:FORCEDAIR:USERDEFINED",
        "AIRTERMINAL:SINGLEDUCT:USERDEFINED",
        "COIL:COOLING:DX:SINGLESPEED:THERMALSTORAGE",
        "ELECTRICEQUIPMENT:ITE:AIRCOOLED",
        "COIL:COOLING:DX:SINGLESPEED",
        "COIL:HEATING:DX:SINGLESPEED",
        "COIL:COOLING:DX:TWOSPEED",
        "COIL:COOLING:DX:MULTISPEED",
        "COIL:HEATING:DX:MULTISPEED",
        "ELECTRICLOADCENTER:STORAGE:CONVERTER",
        "FAN:SYSTEMMODEL"};

    static constexpr std::array<std::string_view, static_cast<int>(DataHeatBalance::IntGainType::Num)> IntGainTypeNamesCC = {
        "People",
        "Lights",
        "ElectricEquipment",
        "GasEquipment",
        "HotWaterEquipment",
        "SteamEquipment",
        "OtherEquipment",
        "ZoneBaseboard:OutdoorTemperatureControlled",
        "ZoneContaminantSourceAndSink:CarbonDioxide",
        "WaterUse:Equipment",
        "DaylightingDevice:Tubular",
        "WaterHeater:Mixed",
        "WaterHeater:Stratified",
        "ThermalStorage:ChilledWater:Mixed",
        "ThermalStorage:ChilledWater:Stratified",
        "Generator:FuelCell",
        "Generator:MicroCHP",
        "ElectricLoadCenter:Transformer",
        "ElectricLoadCenter:Inverter:Simple",
        "ElectricLoadCenter:Inverter:FunctionOfPower",
        "ElectricLoadCenter:Inverter:LookUpTable",
        "ElectricLoadCenter:Storage:LiIonNMCBattery",
        "ElectricLoadCenter:Storage:Battery",
        "ElectricLoadCenter:Storage:Simple",
        "Pipe:Indoor",
        "Refrigeration:Case",
        "Refrigeration:CompressorRack",
        "Refrigeration:System:Condenser:AirCooled",
        "Refrigeration:TranscriticalSystem:GasCooler:AirCooled",
        "Refrigeration:System:SuctionPipe",
        "Refrigeration:TranscriticalSystem:SuctionPipeMT",
        "Refrigeration:TranscriticalSystem:SuctionPipeLT",
        "Refrigeration:SecondarySystem:Receiver",
        "Refrigeration:SecondarySystem:Pipe",
        "Refrigeration:WalkIn",
        "Pump:VariableSpeed",
        "Pump:ConstantSpeed",
        "Pump:VariableSpeed:Condensate",
        "HeaderedPumps:VariableSpeed",
        "HeaderedPumps:ConstantSpeed",
        "ZoneContaminantSourceAndSink:GenericContaminant",
        "PlantComponent:UserDefined",
        "Coil:UserDefined",
        "ZoneHVAC:ForcedAir:UserDefined",
        "AirTerminal:SingleDuct:UserDefined",
        "Coil:Cooling:DX:SingleSpeed:ThermalStorage",
        "ElectricEquipment:ITE:AirCooled",
        "Coil:Cooling:DX:SingleSpeed",
        "Coil:Heating:DX:SingleSpeed",
        "Coil:Cooling:DX:TwoSpeed",
        "Coil:Cooling:DX:MultiSpeed",
        "Coil:Heating:DX:MultiSpeed",
        "ElectricLoadCenter:Storage:Converter",
        "Fan:SystemModel"};

    // Parameters for checking surface heat transfer models
    constexpr Real64 HighDiffusivityThreshold(1.e-5);   // used to check if Material properties are out of line.
    constexpr Real64 ThinMaterialLayerThreshold(0.003); // 3 mm lower limit to expected material layers

    // Const for initialization
    constexpr Real64 ZoneInitialTemp(23.0);       // Zone temperature for initialization
    constexpr Real64 SurfInitialTemp(23.0);       // Surface temperature for initialization
    constexpr Real64 SurfInitialConvCoeff(3.076); // Surface convective coefficient for initialization

    // Air       Argon     Krypton   Xenon
    // Gas conductivity coefficients for gases in a mixture
    static constexpr std::array<std::array<Real64, 10>, 3> GasCoeffsCon = {{{2.873e-3, 2.285e-3, 9.443e-4, 4.538e-4, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0},
                                                                            {7.760e-5, 5.149e-5, 2.826e-5, 1.723e-5, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0},
                                                                            {0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0}}};

    // Air       Argon     Krypton   Xenon
    // Gas viscosity coefficients for gases in a mixture
    static constexpr std::array<std::array<Real64, 10>, 3> GasCoeffsVis = {{{3.723e-6, 3.379e-6, 2.213e-6, 1.069e-6, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0},
                                                                            {4.940e-8, 6.451e-8, 7.777e-8, 7.414e-8, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0},
                                                                            {0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0}}};

    // Air       Argon     Krypton   Xenon
    // Gas specific heat coefficients for gases in a mixture
    static constexpr std::array<std::array<Real64, 10>, 3> GasCoeffsCp = {{
        {1002.737, 521.929, 248.091, 158.340, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0},
        {1.2324e-2, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0},
        {0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0},
    }};

    // Air       Argon     Krypton   Xenon
    static constexpr std::array<Real64, 10> GasWght = {
        28.97, 39.948, 83.8, 131.3, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0}; // Gas molecular weights for gases in a mixture

    // Gas specific heat ratios.  Used for gasses in low pressure
    static constexpr std::array<Real64, 10> GasSpecificHeatRatio = {1.4, 1.67, 1.68, 1.66, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0};

    struct TCGlazingsType
    {
        // Members
        std::string Name;         // Name
        int NumGlzMat = 0;        // Number of TC glazing materials
        Array1D_int LayerPoint;   // Layer pointer
        Array1D<Real64> SpecTemp; // Temperature corresponding to the specified TC glazing optical data
        Array1D_string LayerName; // Name of the referenced WindowMaterial:Glazing object
    };

    struct SpectralDataProperties
    {
        // Members
        std::string Name;           // Name of spectral data set
        int NumOfWavelengths = 0;   // Number of wavelengths in the data set
        Array1D<Real64> WaveLength; // Wavelength (microns)
        Array1D<Real64> Trans;      // Transmittance at normal incidence
        Array1D<Real64> ReflFront;  // Front reflectance at normal incidence
        Array1D<Real64> ReflBack;   // Back reflectance at normal incidence
    };

    struct SpaceData
    {
        std::string Name;                                                 // Space name
        int zoneNum = 0;                                                  // Pointer to Zone wich contains this space
        Real64 userEnteredFloorArea = DataGlobalConstants::AutoCalculate; // User input floor area for this space
        std::string spaceType = "General";                                // Space type tag
        int spaceTypeNum = 0;                                             // Points to spaceType for this space
        EPVector<std::string> tags;                                       // Optional tags for reporting
        EPVector<int> surfaces;                                           // Pointers to surfaces in this space
        Real64 calcFloorArea = 0.0;                                       // Calculated floor area used for this space
        Real64 floorArea = 0.0;                                           // Floor area used for this space
        bool hasFloor = false;                                            // Has "Floor" surface
        Real64 extWindowArea = 0.0;                                       // Exterior Window Area for Zone
        Real64 totalSurfArea = 0.0;                                       // Total surface area for Zone
        int radiantEnclosureNum = 0;                                      // Radiant exchange enclosure this space belongs to
        int solarEnclosureNum = 0;                                        // Solar distribution enclosure this space belongs to
        Real64 totOccupants = 0.0;     // total design occupancy (sum of NumberOfPeople for the space People objects, not multiplied)
        Real64 minOccupants = 0.0;     // minimum occupancy (sum of NomMinNumberPeople for the space People objects, not multiplied)
        Real64 maxOccupants = 0.0;     // maximum occupancy (sum of NomMaxNumberPeople for the space People objects, not multiplied)
        bool isRemainderSpace = false; // True if this space is auto-generated "-Remainder" space
        std::vector<ExteriorEnergyUse::ExteriorFuelUsage> otherEquipFuelTypeNums; // List of fuel types used by other equipment in this space
        std::vector<std::string> otherEquipFuelTypeNames;                         // List of fuel types used by other equipment in this space
    };

    struct SpaceListData
    {
        std::string Name;                               // Space List name
        int numListSpaces = 0;                          // Number of spaces in the list
        std::string::size_type maxSpaceNameLength = 0u; // Max Name length of Spaces in the list
        EPVector<int> spaces;                           // Pointers to Spaces in the list
    };

    struct ZoneData
    {
        // Members
        std::string Name;
        int Multiplier = 1;                                        // Used in reporting and for systems calculations
        int ListMultiplier = 1;                                    // For Zone Group object:  used in reporting and systems calculations
        int ListGroup = 0;                                         // used only in Zone Group verification.  and for error message.
        Real64 RelNorth = 0.0;                                     // Relative North (to building north) [Degrees]
        Real64 OriginX = 0.0;                                      // X origin  [m]
        Real64 OriginY = 0.0;                                      // Y origin  [m]
        Real64 OriginZ = 0.0;                                      // Z origin  [m]
        Real64 CeilingHeight = DataGlobalConstants::AutoCalculate; // Ceiling Height entered by user [m] or calculated
        Real64 Volume = DataGlobalConstants::AutoCalculate;        // Volume entered by user [m3] or calculated
        int OfType = 1;                                            // 1=Standard Zone, Not yet used:
        // 2=Plenum Zone, 11=Solar Wall, 12=Roof Pond
        Real64 UserEnteredFloorArea = DataGlobalConstants::AutoCalculate; // User input floor area for this zone
        // Calculated after input
        Real64 FloorArea = 0.0;     // Floor area used for this zone
        Real64 CalcFloorArea = 0.0; // Calculated floor area used for this zone
        Real64 CeilingArea = 0.0;   // Ceiling area for the zone
        bool HasFloor = false;      // Has "Floor" surface
        bool HasRoof = false;       // Has "Roof" or "Ceiling" Surface
        bool HasWindow = false;     // Window(s) present in this zone
        Real64 AirCapacity = 0.0;
        Real64 ExtWindowArea = 0.0;               // Exterior Window Area for Zone
        Real64 ExtGrossWallArea = 0.0;            // Exterior Wall Area for Zone (Gross)
        Real64 ExtWindowArea_Multiplied = 0.0;    // Exterior Window Area for Zone with multipliers
        Real64 ExtGrossWallArea_Multiplied = 0.0; // Exterior Wall Area for Zone (Gross) with multipliers
        Real64 ExtNetWallArea = 0.0;              // Exterior Wall Area for Zone (Net)
        Real64 TotalSurfArea = 0.0;               // Total surface area for Zone
        Real64 ExteriorTotalSurfArea = 0.0;       // Total surface area of all exterior surfaces for Zone
        // (ignoring windows as they will be included in their base surfaces)
        Real64 ExteriorTotalGroundSurfArea = 0.0;       // Total surface area of all surfaces for Zone with ground contact
        Real64 ExtGrossGroundWallArea = 0.0;            // Ground contact Wall Area for Zone (Gross)
        Real64 ExtGrossGroundWallArea_Multiplied = 0.0; // Ground contact Wall Area for Zone (Gross) with multipliers
        int SystemZoneNodeNumber = 0;                   // This is the zone node number for the system for a controlled zone
        bool IsControlled = false;                      // True when this is a controlled zone.
        bool IsSupplyPlenum = false;                    // True when this zone is a supply plenum
        bool IsReturnPlenum = false;                    // True when this zone is a return plenum
        int ZoneEqNum = 0;                              // Controlled zone equip config number
        int PlenumCondNum = 0;                          // Supply or return plenum conditions number, 0 if this is not a plenum zone
        int TempControlledZoneIndex = 0;                // this is the index number for TempControlledZone structure for lookup
        // Pointers to Surface Data Structure
        // |AllSurfF                                                                      |AllSurfL
        // |            |HTSurfF                                                          |HTSurfL
        // |            |OpaqOrWinMassSurfF                              |OpaqOrWinSurfL  |
        // |            |OpaqOrIntMassSurfF      |OpaqOrIntMassSurfL                      |
        // |            |                        ||WindowSurfF           |WindowSurfL     |
        // |            |                        ||                      ||DomeF          |DomeL
        // {[ SurfAir ] [(   SurfOpaqOrIntMass   )( SurfWinOrTDDDiffuser )( TDDDome       )]}
        // HTSurfaceFirst == OpaqOrWinMassSurfaceFirst == OpaqOrIntMassSurfaceFirst
        // WindowSurfaceFirst == OpaqOrIntMassSurfaceLast + 1
        // TDDDomeFirst == OpaqOrWinSurfaceLast + 1 == WindowSurfaceLast + 1
        // AllSurfaceLast == HTSurfaceLast = TDDDomeLast
        int AllSurfaceFirst = 0;           // First surface in zone including air boundaries
        int AllSurfaceLast = -1;           // Last  surface in zone including air boundaries
        int HTSurfaceFirst = 0;            // First Heat Transfer Surface in Zone
        int HTSurfaceLast = -1;            // Last  Heat Transfer Surface in Zone
        int OpaqOrIntMassSurfaceFirst = 0; // First Opaque or Interior Mass Heat Transfer Surface (including opaque doors) in Zone
        int OpaqOrIntMassSurfaceLast = -1; // Last  Opaque or Interior Mass Heat Transfer Surface (including opaque doors) in Zone
        int WindowSurfaceFirst = 0;        // First Window Heat Transfer Surface in Zone
        int WindowSurfaceLast = -1;        // Last  Window Heat Transfer Surface in Zone
        int OpaqOrWinSurfaceFirst = 0;     // First opaque (including IntMass) or window (non TDD Dome) Surface in Zone
        int OpaqOrWinSurfaceLast = -1;     // Last  opaque (including IntMass) or window (non TDD Dome) Surface in Zone
        int TDDDomeFirst = 0;              // First TDD Dome Surface in Zone
        int TDDDomeLast = -1;              // Last  TDD Dome Surface in Zone
        int InsideConvectionAlgo = ConvectionConstants::HcInt_ASHRAESimple; // Ref: appropriate values for Inside Convection solution
        int NumSurfaces = 0;                                                // Number of surfaces for this zone
        int NumSubSurfaces = 0;     // Number of subsurfaces for this zone (windows, doors, tdd dome and diffusers)
        int NumShadingSurfaces = 0; // Number of shading surfaces for this zone
        int OutsideConvectionAlgo = ConvectionConstants::HcExt_ASHRAESimple; // Ref: appropriate values for Outside Convection solution
        Vector Centroid;                                                     // Center of the zone found by averaging wall, floor, and roof centroids
        Real64 MinimumX = 0.0;                                               // Minimum X value for entire zone
        Real64 MaximumX = 0.0;                                               // Maximum X value for entire zone
        Real64 MinimumY = 0.0;                                               // Minimum Y value for entire zone
        Real64 MaximumY = 0.0;                                               // Maximum Y value for entire zone
        Real64 MinimumZ = 0.0;                                               // Minimum Z value for entire zone
        Real64 MaximumZ = 0.0;                                               // Maximum Z value for entire zone
        std::vector<int> ZoneHTSurfaceList;          // List of HT surfaces related to this zone (includes adjacent interzone surfaces)
        std::vector<int> ZoneIZSurfaceList;          // List of interzone surfaces in this zone
        std::vector<int> ZoneHTNonWindowSurfaceList; // List of non-window HT surfaces related to this zone (includes adjacent interzone surfaces)
        std::vector<int> ZoneHTWindowSurfaceList;    // List of window surfaces related to this zone (includes adjacent interzone surfaces)
        std::vector<int> ZoneExtSolarSurfaceList;    // List of exterior solar surfaces in a zone
        int zoneRadEnclosureFirst = -1;              // For Zone resimulation, need a range of enclosures for CalcInteriorRadExchange
        int zoneRadEnclosureLast = -1;               // For Zone resimulation, need a range of enclosures for CalcInteriorRadExchange

        Real64 OutDryBulbTemp = 0.0;                 // Zone outside dry bulb air temperature (C)
        bool OutDryBulbTempEMSOverrideOn = false;    // if true, EMS is calling to override the surface's outdoor air temp
        Real64 OutDryBulbTempEMSOverrideValue = 0.0; // value to use for EMS override of outdoor air drybulb temp (C)
        Real64 OutWetBulbTemp = 0.0;                 // Zone outside wet bulb air temperature (C)
        bool OutWetBulbTempEMSOverrideOn = false;    // if true, EMS is calling to override the surface's outdoor wetbulb
        Real64 OutWetBulbTempEMSOverrideValue = 0.0; // value to use for EMS override of outdoor air wetbulb temp (C)
        Real64 WindSpeed = 0.0;                      // Zone outside wind speed (m/s)
        bool WindSpeedEMSOverrideOn = false;         // if true, EMS is calling to override the surface's outside wind speed
        Real64 WindSpeedEMSOverrideValue = 0.0;      // value to use for EMS override of the surface's outside wind speed
        Real64 WindDir = 0.0;                        // Zone outside wind direction (degree)
        bool WindDirEMSOverrideOn = false;           // if true, EMS is calling to override the surface's outside wind direction
        Real64 WindDirEMSOverrideValue = 0.0;        // value to use for EMS override of the surface's outside wind speed

        bool HasLinkedOutAirNode = false; // true if an OutdoorAir::Node is linked to the surface
        int LinkedOutAirNode = 0;         // Index of the an OutdoorAir:Node

        bool isPartOfTotalArea = true;           // Count the zone area when determining the building total floor area
        bool isNominalOccupied = false;          // has occupancy nominally specified
        bool isNominalControlled = false;        // has Controlled Zone Equip Configuration reference
        Real64 TotOccupants = 0.0;               // total design occupancy (sum of NumberOfPeople for the zone People objects, not multiplied)
        Real64 minOccupants = 0.0;               // minimum occupancy (sum of NomMinNumberPeople for the zone People objects, not multiplied)
        Real64 maxOccupants = 0.0;               // maximum occupancy (sum of NomMaxNumberPeople for the zone People objects, not multiplied)
        int AirHBimBalanceErrIndex = 0;          // error management counter
        bool NoHeatToReturnAir = false;          // TRUE means that heat to return air should be added to the zone load
        bool RefrigCaseRA = false;               // TRUE means there is potentially heat removal from return air
        bool HasAdjustedReturnTempByITE = false; // TRUE means that return temp to return air is adjusted by return temperature of ITE object
        Real64 AdjustedReturnTempByITE = 0.0;    // Diff of the return temp from the zone mixed air temp adjusted by ITE object

        bool HasLtsRetAirGain = false;       // TRUE means that zone lights return air heat > 0.0 calculated from plenum temperature
        bool HasAirFlowWindowReturn = false; // TRUE means that zone has return air flow from windows
        // from refrigeration cases for this zone
        Real64 InternalHeatGains = 0.0;         // internal loads (W)
        Real64 NominalInfilVent = 0.0;          // internal infiltration/ventilation
        Real64 NominalMixing = 0.0;             // internal mixing/cross mixing
        bool TempOutOfBoundsReported = false;   // if any temp out of bounds errors, first will show zone details.
        bool EnforcedReciprocity = false;       // if zone/space required forced reciprocity -- less out of bounds temp errors allowed
        int ZoneMinCO2SchedIndex = 0;           // Index for the schedule the schedule which determines minimum CO2 concentration
        int ZoneMaxCO2SchedIndex = 0;           // Index for the schedule the schedule which determines maximum CO2 concentration
        int ZoneContamControllerSchedIndex = 0; // Index for this schedule
        bool FlagCustomizedZoneCap = false;     // True if customized Zone Capacitance Multiplier is used
        std::vector<ExteriorEnergyUse::ExteriorFuelUsage> otherEquipFuelTypeNums; // List of fuel types used by other equipment in this zone
        std::vector<std::string> otherEquipFuelTypeNames;                         // List of fuel types used by other equipment in this zone

        // Hybrid Modeling
        Real64 ZoneMeasuredTemperature = 0.0;               // Measured zone air temperature input by user
        Real64 ZoneMeasuredHumidityRatio = 0.0;             // Measured zone air humidity ratio by user
        Real64 ZoneMeasuredCO2Concentration = 0.0;          // Measured zone air CO2 concentration input by user
        Real64 ZoneMeasuredSupplyAirTemperature = 0.0;      // Measured zone supply air temperature input by user
        Real64 ZoneMeasuredSupplyAirFlowRate = 0.0;         // Measured zone supply air flow rate input by user
        Real64 ZoneMeasuredSupplyAirHumidityRatio = 0.0;    // Measured zone supply air flow rate input by user
        Real64 ZoneMeasuredSupplyAirCO2Concentration = 0.0; // Measured zone supply air flow rate input by user
        Real64 ZonePeopleActivityLevel = 0.0;               // People activity level input by user
        Real64 ZonePeopleSensibleHeatFraction = 0.0;        // People activity level input by user
        Real64 ZonePeopleRadiantHeatFraction = 0.0;         // People activity level input by user
        Real64 ZoneVolCapMultpSens = 1.0;                   // Zone temperature capacity multiplier, i.e. internal thermal mass multiplier
        Real64 ZoneVolCapMultpMoist = 1.0;                  // Zone humidity capacity multiplier
        Real64 ZoneVolCapMultpCO2 = 1.0;                    // Zone carbon dioxide capacity multiplier
        Real64 ZoneVolCapMultpGenContam = 1.0;              // Zone generic contaminant capacity multiplier
        Real64 ZoneVolCapMultpSensHM = 1.0;                 // Calculated temperature capacity multiplier by hybrid model
        Real64 ZoneVolCapMultpSensHMSum = 0.0;              // for temperature capacity multiplier average calculation
        Real64 ZoneVolCapMultpSensHMCountSum = 0.0;         // for temperature capacity multiplier average calculation
        Real64 ZoneVolCapMultpSensHMAverage = 1.0;          // Temperature capacity multiplier average
        Real64 MCPIHM = 0.0;                                // Calculated mass flow rate by hybrid model
        Real64 InfilOAAirChangeRateHM = 0.0;                // Calculated infiltration air change per hour by hybrid model
        Real64 NumOccHM = 0.0;                              // Inversely solved people count
        Real64 delta_T = 0.0;                               // Indoor and outdoor temperature
        Real64 delta_HumRat = 0.0;                          // Indoor and outdoor humidity ratio delta

        Real64 ZeroSourceSumHATsurf = 0.0; // From Chilled Ceiling Panel, equal to the SumHATsurf for all the walls in a zone with no source

        // Spaces
        bool anySurfacesWithoutSpace = false; // True if any surfaces in a zone do not have a space assigned in input
        bool anySurfacesWithSpace = false;    // True if any surfaces in a zone have a space assigned in input
        EPVector<int> spaceIndexes;           // Indexes to spaces in this zone
        int numSpaces = 0;                    // Number of spaces in this zone

        // Default Constructor
        ZoneData() : Centroid(0.0, 0.0, 0.0)
        {
        }

        void SetOutBulbTempAt(EnergyPlusData &state);

        void SetWindSpeedAt(EnergyPlusData &state, Real64 fac);

        void SetWindDirAt(Real64 fac);
    };

    struct ZoneListData
    {
        // Members
        std::string Name;                         // Zone List name
        int NumOfZones = 0;                       // Number of zones in the list
        std::string::size_type MaxZoneNameLength; // Max Name length of zones in the list
        Array1D_int Zone;                         // Pointers to zones in the list

        // Default Constructor
        ZoneListData() : MaxZoneNameLength(0u)
        {
        }
    };

    struct ZoneGroupData
    {
        // Members
        std::string Name;   // Zone Group name
        int ZoneList = 0;   // Pointer to the zone list
        int Multiplier = 1; // Zone List multiplier
    };

    struct GlobalInternalGainMiscObject
    {
        // Members
        std::string Name;
        int ZoneOrZoneListPtr = 0;
        int NumOfZones = 0;
        int StartPtr = 0;
        bool ZoneListActive = false;
        int spaceOrSpaceListPtr = 0;
        int numOfSpaces = 0;
        int spaceStartPtr = 0;
        bool spaceListActive = false;
        EPVector<int> spaceNums;     // Indexes to spaces associated with this input object
        EPVector<std::string> names; // Names for each instance created from this input object
    };

    enum class ClothingType
    {
        Invalid = -1,
        InsulationSchedule,
        DynamicAshrae55,
        CalculationSchedule,
        Num
    };
    constexpr std::array<std::string_view, static_cast<int>(ClothingType::Num)> clothingTypeNamesUC = {
        "CLOTHINGINSULATIONSCHEDULE", "DYNAMICCLOTHINGMODELASHRAE55", "CALCULATIONMETHODSCHEDULE"};
    constexpr std::array<std::string_view, static_cast<int>(ClothingType::Num)> clothingTypeEIOStrings = {
        "Clothing Insulation Schedule,", "Dynamic Clothing Model ASHRAE55,", "Calculation Method Schedule,"};

    struct PeopleData
    {
        // Members
        std::string Name;               // PEOPLE object name
        int ZonePtr = 0;                // Zone index for this people statement
        int spaceIndex = 0;             // Space index for this people statement
        Real64 NumberOfPeople = 0.0;    // Maximum number of people for this statement
        int NumberOfPeoplePtr = -1;     // Pointer to schedule for number of people
        bool EMSPeopleOn = false;       // EMS actuating number of people if .TRUE.
        Real64 EMSNumberOfPeople = 0.0; // Value EMS is directing to use for override
        // Note that the schedule and maximum number was kept for people since it seemed likely that
        // users would want to assign the same schedule to multiple people statements.
        int ActivityLevelPtr = -1;    // Pointer to schedule for activity level
        Real64 FractionRadiant = 0.0; // Percentage (fraction 0.0-1.0) of sensible heat gain from people
        // that is radiant
        Real64 FractionConvected = 0.0; // Percentage (fraction 0.0-1.0) of sensible heat gain from people
        // that is convective
        Real64 NomMinNumberPeople = 0.0; // Nominal Minimum Number of People (min sch X number of people)
        Real64 NomMaxNumberPeople = 0.0; // Nominal Maximum Number of People (min sch X number of people)
        int WorkEffPtr = -1;             // Pointer to schedule for work efficiency
        int ClothingPtr = -1;            // Pointer to schedule for clothing insulation
        int ClothingMethodPtr = -1;
        ClothingType clothingType = ClothingType::Invalid; // Clothing type
        int AirVelocityPtr = -1;                           // Pointer to schedule for air velocity in zone
        int AnkleAirVelocityPtr = -1;                      // Pointer to schedule for air velocity in zone
        bool Fanger = false;                               // True when Fanger calculation to be performed
        bool Pierce = false;                               // True when Pierce 2-node calculation to be performed
        bool KSU = false;                                  // True when KSU 2-node calculation to be performed
        bool AdaptiveASH55 = false;                        // True when ASHRAE Standard 55 adaptive comfort calculation
        //   to be performed
        bool AdaptiveCEN15251 = false; // True when CEN Standard 15251 adaptive comfort calculation
        //   to be performed
        bool CoolingEffectASH55 = false;                         // True when ASHRAE Standard 55 cooling effect calculation to be performed
        bool AnkleDraftASH55 = false;                            // True when ASHRAE Standard 55 ankle draft calculation to be performed
        CalcMRT MRTCalcType = DataHeatBalance::CalcMRT::Invalid; // MRT calculation type (See MRT Calculation type parameters)
        int SurfacePtr = -1;                                     // Pointer to the name of surface
        std::string AngleFactorListName;                         // Name of angle factor list
        int AngleFactorListPtr = -1;                             // Pointer to the name of angle factor list
        Real64 UserSpecSensFrac = 0.0;                           // User specified sensible fraction
        bool Show55Warning = false;                              // show the warning messages about ASHRAE 55-2004
        Real64 CO2RateFactor = 0.0;                              // Carbon Dioxide Generation Rate [m3/s-W]
        // Report variables
        Real64 NumOcc = 0.0;                 // Number of occupants at current timestep []
        Real64 TemperatureInZone = 0.0;      // Temperature in zone (C)
        Real64 RelativeHumidityInZone = 0.0; // Relative humidity in zone
        Real64 RadGainRate = 0.0;            // Radiant heat gain [W]
        Real64 ConGainRate = 0.0;            // Convective heat gain [W]
        Real64 SenGainRate = 0.0;            // Sensible heat gain [W]
        Real64 LatGainRate = 0.0;            // Latent heat gain [W]
        Real64 TotGainRate = 0.0;            // Total heat gain [W]
        Real64 CO2GainRate = 0.0;            // Carbon Dioxide Gain Rate [m3/s]
        Real64 RadGainEnergy = 0.0;          // Radiant heat gain [J]
        Real64 ConGainEnergy = 0.0;          // Convective heat gain [J]
        Real64 SenGainEnergy = 0.0;          // Sensible heat gain [J]
        Real64 LatGainEnergy = 0.0;          // Latent heat gain [J]
        Real64 TotGainEnergy = 0.0;          // Total heat gain [J]
        // Air velocity check during run time for thermal comfort control
        int AirVelErrIndex = 0; // Air velocity error index
        // For AdaptiveComfort tabular report
        Real64 TimeNotMetASH5580 = 0.0;
        Real64 TimeNotMetASH5590 = 0.0;
        Real64 TimeNotMetCEN15251CatI = 0.0;
        Real64 TimeNotMetCEN15251CatII = 0.0;
        Real64 TimeNotMetCEN15251CatIII = 0.0;
    };

    struct LightsData
    {
        // Members
        std::string Name;                 // LIGHTS object name
        int ZonePtr = 0;                  // Which zone lights are in
        int spaceIndex = 0;               // Space index for this lights instance
        int SchedPtr = -1;                // Schedule for lights
        Real64 DesignLevel = 0.0;         // design level for lights [W]
        bool EMSLightsOn = false;         // EMS actuating Lighting power if .TRUE.
        Real64 EMSLightingPower = 0.0;    // Value EMS is directing to use for override
        Real64 FractionReturnAir = 0.0;   // Percentage (fraction 0.0-1.0) of sensible heat gain that is return air
        Real64 FractionRadiant = 0.0;     // Percentage (fraction 0.0-1.0) of sensible heat gain that is radiant
        Real64 FractionShortWave = 0.0;   // Percentage (fraction 0.0-1.0) of sensible heat gain that is short wave
        Real64 FractionReplaceable = 0.0; // Percentage (fraction 0.0-1.0) of sensible heat gain that is replaceable
        Real64 FractionConvected = 0.0;   // Percentage (fraction 0.0-1.0) of sensible heat gain that is convective
        bool FractionReturnAirIsCalculated = false;
        Real64 FractionReturnAirPlenTempCoeff1 = 0.0;
        Real64 FractionReturnAirPlenTempCoeff2 = 0.0;
        int ZoneReturnNum = 1;          // zone return index (not the node number) for return heat gain
        std::string RetNodeName;        // Zone return node name
        int ZoneExhaustNodeNum = 0;     // Exhaust node number
        Real64 NomMinDesignLevel = 0.0; // Nominal Minimum Design Level (min sch X design level)
        Real64 NomMaxDesignLevel = 0.0; // Nominal Maximum Design Level (max sch X design level)
        bool ManageDemand = false;      // Flag to indicate whether to use demand limiting
        Real64 DemandLimit = 0.0;       // Demand limit set by demand manager [W]
        // Report variables
        Real64 Power = 0.0;              // Electric power [W]
        Real64 RadGainRate = 0.0;        // Radiant heat gain [W]
        Real64 VisGainRate = 0.0;        // Visible heat gain [W]
        Real64 ConGainRate = 0.0;        // Convective heat gain [W]
        Real64 RetAirGainRate = 0.0;     // Return air heat gain [W]
        Real64 TotGainRate = 0.0;        // Total heat gain [W]
        Real64 Consumption = 0.0;        // Electric consumption [J]
        Real64 RadGainEnergy = 0.0;      // Radiant heat gain [J]
        Real64 VisGainEnergy = 0.0;      // Visible heat gain [J]
        Real64 ConGainEnergy = 0.0;      // Convective heat gain [J]
        Real64 RetAirGainEnergy = 0.0;   // Return air heat gain [J]
        Real64 TotGainEnergy = 0.0;      // Total heat gain [J]
        std::string EndUseSubcategory;   // user defined name for the end use category
        Real64 SumConsumption = 0.0;     // sum of electric consumption [J] for reporting
        Real64 SumTimeNotZeroCons = 0.0; // sum of time of positive electric consumption [hr]
    };

    struct ZoneEquipData // Electric, Gas, Other Equipment, CO2
    {
        // Members
        std::string Name;                    // EQUIPMENT object name
        int ZonePtr = 0;                     // Which zone internal gain is in
        int spaceIndex = 0;                  // Space index for this equipment instance
        int SchedPtr = 0;                    // Schedule for internal gain
        Real64 DesignLevel = 0.0;            // design level for internal gain [W]
        bool EMSZoneEquipOverrideOn = false; // EMS actuating equipment power if .TRUE.
        Real64 EMSEquipPower = 0.0;          // Value EMS is directing to use for override
        Real64 FractionLatent = 0.0;         // Percentage (fraction 0.0-1.0) of sensible heat gain that is latent
        Real64 FractionRadiant = 0.0;        // Percentage (fraction 0.0-1.0) of sensible heat gain that is radiant
        Real64 FractionLost = 0.0;           // Percentage (fraction 0.0-1.0) of sensible heat gain that is lost
        Real64 FractionConvected = 0.0;      // Percentage (fraction 0.0-1.0) of sensible heat gain that is convective
        Real64 CO2DesignRate = 0.0;          // CO2 design Rate [m3/s]
        Real64 CO2RateFactor = 0.0;          // CO2 rate factor [m3/s/W]
        Real64 NomMinDesignLevel = 0.0;      // Nominal Minimum Design Level (min sch X design level)
        Real64 NomMaxDesignLevel = 0.0;      // Nominal Maximum Design Level (max sch X design level)
        bool ManageDemand = false;           // Flag to indicate whether to use demand limiting
        Real64 DemandLimit = 0.0;            // Demand limit set by demand manager [W]
        // Report variables
        Real64 Power = 0.0;                   // Electric/Gas/Fuel power [W]
        Real64 RadGainRate = 0.0;             // Radiant heat gain [W]
        Real64 ConGainRate = 0.0;             // Convective heat gain [W]
        Real64 LatGainRate = 0.0;             // Latent heat gain [W]
        Real64 LostRate = 0.0;                // Lost energy (converted to work) [W]
        Real64 TotGainRate = 0.0;             // Total heat gain [W]
        Real64 CO2GainRate = 0.0;             // CO2 gain rate [m3/s]
        Real64 Consumption = 0.0;             // Electric/Gas/Fuel consumption [J]
        Real64 RadGainEnergy = 0.0;           // Radiant heat gain [J]
        Real64 ConGainEnergy = 0.0;           // Convective heat gain [J]
        Real64 LatGainEnergy = 0.0;           // Latent heat gain [J]
        Real64 LostEnergy = 0.0;              // Lost energy (converted to work) [J]
        Real64 TotGainEnergy = 0.0;           // Total heat gain [J]
        std::string EndUseSubcategory;        // user defined name for the end use category
        std::string otherEquipFuelTypeString; // Fuel Type string for Other Equipment
        ExteriorEnergyUse::ExteriorFuelUsage OtherEquipFuelType =
            ExteriorEnergyUse::ExteriorFuelUsage::Invalid; // Fuel Type Number of the Other Equipment (defined in ExteriorEnergyUse.cc)
    };

    struct ITEquipData // IT Equipment
    {
        // Members
        std::string Name;                          // EQUIPMENT object name
        int ZonePtr = 0;                           // Which zone internal gain is in
        int spaceIndex = 0;                        // Space index for this equipment instance
        bool FlowControlWithApproachTemps = false; // True if using supply and return approach temperature for ITE object.
        Real64 DesignTotalPower = 0.0;             // Design level for internal gain [W]
        Real64 NomMinDesignLevel = 0.0;            // Nominal Minimum Design Level (min sch X design level)
        Real64 NomMaxDesignLevel = 0.0;            // Nominal Maximum Design Level (max sch X design level)
        Real64 DesignFanPowerFrac = 0.0;           // Fraction (0.0-1.0) of design power level that is fans
        int OperSchedPtr = 0;                      // Schedule pointer for design power input or operating schedule
        int CPULoadSchedPtr = 0;                   // Schedule pointer for CPU loading schedule
        Real64 SizingTAirIn = 0.0;                 // Entering air dry-bulb temperature at maximum value during sizing[C]
        Real64 DesignTAirIn = 0.0;                 // Design entering air dry-bulb temperature [C]
        Real64 DesignFanPower = 0.0;               // Design fan power input [W]
        Real64 DesignCPUPower = 0.0;               // Design CPU power input [W]
        Real64 DesignAirVolFlowRate = 0.0;         // Design air volume flow rate [m3/s]
        int Class = 0;                             // Environmental class index (A1=1, A2=2, A3=3, A4=4, B=5, C=6)
        int AirFlowFLTCurve = 0;                   // Index for airflow function of CPULoadFrac (x) and TAirIn (y) curve
        int CPUPowerFLTCurve = 0;                  // Index for CPU power function of CPULoadFrac (x) and TAirIn (y) curve
        int FanPowerFFCurve = 0;                   // Index for fan power function of flow fraction curve
        int AirConnectionType = 0;                 // Air connection type (AdjustedSupply, ZoneAirNode, RoomAirModel)
        int InletRoomAirNodeNum = 0;               // Room air model node number for air inlet
        int OutletRoomAirNodeNum = 0;              // Room air model node number for air outlet
        int SupplyAirNodeNum = 0;                  // Node number for supply air inlet
        Real64 DesignRecircFrac = 0.0;             // Design recirculation fraction (0.0-0.5)
        int RecircFLTCurve = 0;                    // Index for recirculation function of CPULoadFrac (x) and TSupply (y) curve
        Real64 DesignUPSEfficiency = 0.0;          // Design power supply efficiency (>0.0 - 1.0)
        int UPSEfficFPLRCurve = 0;                 // Index for recirculation function of part load ratio
        Real64 UPSLossToZoneFrac = 0.0;            // Fraction of UPS power loss to zone (0.0 - 1.0); remainder is lost
        std::string EndUseSubcategoryCPU;          // User defined name for the end use category for the CPU
        std::string EndUseSubcategoryFan;          // User defined name for the end use category for the Fans
        std::string EndUseSubcategoryUPS;          // User defined name for the end use category for the power supply
        bool EMSCPUPowerOverrideOn = false;        // EMS actuating CPU power if .TRUE.
        Real64 EMSCPUPower = 0.0;                  // Value EMS is directing to use for override of CPU power [W]
        bool EMSFanPowerOverrideOn = false;        // EMS actuating Fan power if .TRUE.
        Real64 EMSFanPower = 0.0;                  // Value EMS is directing to use for override of Fan power [W]
        bool EMSUPSPowerOverrideOn = false;        // EMS actuating UPS power if .TRUE.
        Real64 EMSUPSPower = 0.0;                  // Value EMS is directing to use for override of UPS power [W]
        Real64 SupplyApproachTemp = 0.0;           // The difference of the IT inlet temperature from the AHU supply air temperature
        int SupplyApproachTempSch = 0;             // The difference schedule of the IT inlet temperature from the AHU supply air temperature
        Real64 ReturnApproachTemp = 0.0;           // The difference of the unit outlet temperature from the well mixed zone temperature
        int ReturnApproachTempSch = 0;             // The difference schedule of the unit outlet temperature from the well mixed zone temperature
        int zoneEqIndex = 0;                       // index in zone equipment data structure for the zone this IT equipment is in

        // Report variables
        Real64 CPUPower = 0.0;          // ITE CPU Electric Power [W]
        Real64 FanPower = 0.0;          // ITE Fan Electric Power [W]
        Real64 UPSPower = 0.0;          // ITE UPS Electric Power [W]
        Real64 CPUPowerAtDesign = 0.0;  // ITE CPU Electric Power at Design Inlet Conditions [W]
        Real64 FanPowerAtDesign = 0.0;  // ITE Fan Electric Power at Design Inlet Conditions [W]
        Real64 UPSGainRateToZone = 0.0; // ITE UPS Heat Gain to Zone Rate [W] - convective gain
        Real64 ConGainRateToZone = 0.0; // ITE Total Heat Gain to Zone Rate [W] - convective gain - includes heat gain from UPS, plus CPU and Fans if
                                        // room air model not used
        Real64 CPUConsumption = 0.0;    // ITE CPU Electric Energy [J]
        Real64 FanConsumption = 0.0;    // ITE Fan Electric Energy [J]
        Real64 UPSConsumption = 0.0;    // ITE UPS Electric Energy [J]
        Real64 CPUEnergyAtDesign = 0.0; // ITE CPU Electric Energy at Design Inlet Conditions [J]
        Real64 FanEnergyAtDesign = 0.0; // ITE Fan Electric Energy at Design Inlet Conditions [J]
        Real64 UPSGainEnergyToZone = 0.0; // ITE UPS Heat Gain to Zone Energy [J] - convective gain
        Real64 ConGainEnergyToZone = 0.0; // ITE Total Heat Gain to Zone Energy [J] - convective gain - includes heat gain from UPS, plus CPU and Fans
                                          // if room air model not used
        Real64 AirVolFlowStdDensity = 0.0; // Air volume flow rate at standard density [m3/s]
        Real64 AirVolFlowCurDensity = 0.0; // Air volume flow rate at current density [m3/s]
        Real64 AirMassFlow = 0.0;          // Air mass flow rate [kg/s]
        Real64 AirInletDryBulbT = 0.0;     // Air inlet dry-bulb temperature [C]
        Real64 AirInletDewpointT = 0.0;    // Air inlet dewpoint temperature [C]
        Real64 AirInletRelHum = 0.0;       // Air inlet relative humidity [%]
        Real64 AirOutletDryBulbT = 0.0;    // Air outlet dry-bulb temperature [C]
        Real64 SHI = 0.0;                  // Supply Heat Index []
        Real64 TimeOutOfOperRange = 0.0;   // ITE Air Inlet Operating Range Exceeded Time [hr]
        Real64 TimeAboveDryBulbT = 0.0;    // ITE Air Inlet Dry-Bulb Temperature Above Operating Range Time [hr]
        Real64 TimeBelowDryBulbT = 0.0;    // ITE Air Inlet Dry-Bulb Temperature Below Operating Range Time [hr]
        Real64 TimeAboveDewpointT = 0.0;   // ITE Air Inlet Dewpoint Temperature Above Operating Range Time [hr]
        Real64 TimeBelowDewpointT = 0.0;   // ITE Air Inlet Dewpoint Temperature Below Operating Range Time [hr]
        Real64 TimeAboveRH = 0.0;          // ITE Air Inlet Relative Humidity Above Operating Range Time [hr]
        Real64 TimeBelowRH = 0.0;          // ITE Air Inlet Relative Humidity Below Operating Range Time [hr]
        Real64 DryBulbTAboveDeltaT = 0.0;  // ITE Air Inlet Dry-Bulb Temperature Difference Above Operating Range [deltaC]
        Real64 DryBulbTBelowDeltaT = 0.0;  // ITE Air Inlet Dry-Bulb Temperature Difference Below Operating Range [deltaC]
        Real64 DewpointTAboveDeltaT = 0.0; // ITE Air Inlet Dewpoint Temperature Difference Above Operating Range [deltaC]
        Real64 DewpointTBelowDeltaT = 0.0; // ITE Air Inlet Dewpoint Temperature Difference Below Operating Range [deltaC]
        Real64 RHAboveDeltaRH = 0.0;       // ITE Air Inlet Relative Humidity Difference Above Operating Range [%]
        Real64 RHBelowDeltaRH = 0.0;       // ITE Air Inlet Relative Humidity Difference Below Operating Range [%]
    };

    struct BBHeatData
    {
        // Members
        std::string Name; // BASEBOARD HEAT object name
        int ZonePtr = 0;
        int spaceIndex = 0; // Space index for this equipment instance
        int SchedPtr = 0;
        Real64 CapatLowTemperature = 0.0;
        Real64 LowTemperature = 0.0;
        Real64 CapatHighTemperature = 0.0;
        Real64 HighTemperature = 0.0;
        bool EMSZoneBaseboardOverrideOn = false; // EMS actuating equipment power if .TRUE.
        Real64 EMSZoneBaseboardPower = 0.0;      // Value EMS is directing to use for override
        Real64 FractionRadiant = 0.0;
        Real64 FractionConvected = 0.0;
        bool ManageDemand = false; // Flag to indicate whether to use demand limiting
        Real64 DemandLimit = 0.0;  // Demand limit set by demand manager [W]
        // Report variables
        Real64 Power = 0.0;            // Electric power [W]
        Real64 RadGainRate = 0.0;      // Radiant heat gain [W]
        Real64 ConGainRate = 0.0;      // Convective heat gain [W]
        Real64 TotGainRate = 0.0;      // Total heat gain [W]
        Real64 Consumption = 0.0;      // Electric consumption [J]
        Real64 RadGainEnergy = 0.0;    // Radiant heat gain [J]
        Real64 ConGainEnergy = 0.0;    // Convective heat gain [J]
        Real64 TotGainEnergy = 0.0;    // Total heat gain [J]
        std::string EndUseSubcategory; // user defined name for the end use category
    };

    struct InfiltrationData
    {
        // Members
        std::string Name;
        int ZonePtr = 0;                                                  // Which zone infiltration is in
        int SchedPtr = 0;                                                 // Schedule for infiltration
        InfiltrationModelType ModelType = InfiltrationModelType::Invalid; // which model is used for infiltration
        // Design Flow Rate model terms
        Real64 DesignLevel = 0.0;
        Real64 ConstantTermCoef = 0.0;
        Real64 TemperatureTermCoef = 0.0;
        Real64 VelocityTermCoef = 0.0;
        Real64 VelocitySQTermCoef = 0.0;
        // Effective Leakage Area, Sherman Grimsrud terms
        Real64 LeakageArea = 0.0;           // "AL" effective air leakage area
        Real64 BasicStackCoefficient = 0.0; // "Cs" Stack coefficient
        Real64 BasicWindCoefficient = 0.0;  // "Cw" wind coefficient
        // Flow Coefficient, AIM-2, Walker and Wilson terms
        Real64 FlowCoefficient = 0.0;       // "c" Flow coefficient
        Real64 AIM2StackCoefficient = 0.0;  // "Cs" stack coefficient
        Real64 AIM2WindCoefficient = 0.0;   // "Cw" wind coefficient
        Real64 PressureExponent = 0.0;      // "n" pressure power law exponent
        Real64 ShelterFactor = 0.0;         // "s" shelter factor
        bool EMSOverrideOn = false;         // if true then EMS is requesting to override
        Real64 EMSAirFlowRateValue = 0.0;   // value EMS is setting for air flow rate
        bool QuadratureSum = false;         // If quadrature sum of zone air balance method is used
        int OABalancePtr = 0;               // A pointer to ZoneAirBalance If quadrature is true
        Real64 VolumeFlowRate = 0.0;        // infiltration air volume flow rate
        Real64 MassFlowRate = 0.0;          // infiltration air mass flow rate
        Real64 MCpI_temp = 0.0;             // INFILTRATION MASS FLOW * AIR SPECIFIC HEAT
        Real64 InfilHeatGain = 0.0;         // Heat Gain {J} due to infiltration
        Real64 InfilHeatLoss = 0.0;         // Heat Loss {J} due to infiltration
        Real64 InfilLatentGain = 0.0;       // Latent Gain {J} due to infiltration
        Real64 InfilLatentLoss = 0.0;       // Latent Loss {J} due to infiltration
        Real64 InfilTotalGain = 0.0;        // Total Gain {J} due to infiltration (sensible+latent)
        Real64 InfilTotalLoss = 0.0;        // Total Loss {J} due to infiltration (sensible+latent)
        Real64 InfilVolumeCurDensity = 0.0; // Volume of Air {m3} due to infiltration at current zone air density
        Real64 InfilVolumeStdDensity = 0.0; // Volume of Air {m3} due to infiltration at standard density (adjusted for elevation)
        Real64 InfilVdotCurDensity = 0.0;   // Volume flow rate of Air {m3/s} due to infiltration at current zone air density
        Real64 InfilVdotStdDensity = 0.0;   // Volume flow rate of Air {m3/s} due to infiltration standard density (adjusted elevation)
        Real64 InfilMdot = 0.0;             // Mass flow rate {kg/s} due to infiltration for reporting
        Real64 InfilMass = 0.0;             // Mass of Air {kg} due to infiltration
        Real64 InfilAirChangeRate = 0.0;    // Infiltration air change rate {ach}
    };

    struct VentilationData
    {
        // Members
        std::string Name;
        int ZonePtr = 0;
        int SchedPtr = 0;
        VentilationModelType ModelType =
            VentilationModelType::Invalid; // which model is used for ventilation: DesignFlowRate and WindandStackOpenArea
        Real64 DesignLevel = 0.0;
        bool EMSSimpleVentOn = false;      // EMS actuating ventilation flow rate if .TRUE.
        Real64 EMSimpleVentFlowRate = 0.0; // Value EMS is directing to use for override
        Real64 MinIndoorTemperature = -100.0;
        Real64 DelTemperature = 0.0;
        VentilationType FanType = VentilationType::Natural;
        Real64 FanPressure = 0.0;
        Real64 FanEfficiency = 0.0;
        Real64 FanPower = 0.0;
        Real64 AirTemp = 0.0;
        Real64 ConstantTermCoef = 0.0;
        Real64 TemperatureTermCoef = 0.0;
        Real64 VelocityTermCoef = 0.0;
        Real64 VelocitySQTermCoef = 0.0;
        Real64 MaxIndoorTemperature = 100.0;
        Real64 MinOutdoorTemperature = -100.0;
        Real64 MaxOutdoorTemperature = 100.0;
        Real64 MaxWindSpeed = 40.0;
        int MinIndoorTempSchedPtr = 0;                            // Minimum indoor temperature schedule index
        int MaxIndoorTempSchedPtr = 0;                            // Maximum indoor temperature schedule index
        int DeltaTempSchedPtr = 0;                                // Delta temperature schedule index
        int MinOutdoorTempSchedPtr = 0;                           // Minimum outdoor temperature schedule index
        int MaxOutdoorTempSchedPtr = 0;                           // Maximum outdoor temperature schedule index
        int IndoorTempErrCount = 0;                               // Indoor temperature error count
        int OutdoorTempErrCount = 0;                              // Outdoor temperature error count
        int IndoorTempErrIndex = 0;                               // Indoor temperature error Index
        int OutdoorTempErrIndex = 0;                              // Outdoor temperature error Index
        HybridCtrlType HybridControlType = HybridCtrlType::Indiv; // Hybrid ventilation control type: 0 Individual, 1 Close, 2 Global
        int HybridControlMasterNum = 0;                           // Hybrid ventilation control master object number
        bool HybridControlMasterStatus = false;                   // Hybrid ventilation control master object opening status
        bool QuadratureSum = false;                               // If quadrature sum of zone air balance method is used
        int OABalancePtr = 0;                                     // A pointer to ZoneAirBalance
        // WindandStackOpenArea
        Real64 OpenArea = 0.0;    // Opening area [m2]
        int OpenAreaSchedPtr = 0; // Opening area fraction schedule pointer
        Real64 OpenEff = 0.0;     // Opening effectiveness [dimensionless]
        Real64 EffAngle = 0.0;    // Effective angle [degree]
        Real64 DH = 0.0;          // Height difference [m]
        Real64 DiscCoef = 0.0;    // Discharge coefficient
    };

    struct ZoneAirBalanceData
    {
        // Members
        std::string Name;                            // Object name
        std::string ZoneName;                        // Zone name
        int ZonePtr = 0;                             // Zone number
        AirBalance BalanceMethod = AirBalance::None; // Air Balance Method
        Real64 InducedAirRate = 0.0;                 // Induced Outdoor Air Due to Duct Leakage Unbalance [m3/s]
        int InducedAirSchedPtr = 0;                  // Induced Outdoor Air Fraction Schedule
        Real64 BalMassFlowRate = 0.0;                // balanced mass flow rate
        Real64 InfMassFlowRate = 0.0;                // unbalanced mass flow rate from infiltration
        Real64 NatMassFlowRate = 0.0;                // unbalanced mass flow rate from natural ventilation
        Real64 ExhMassFlowRate = 0.0;                // unbalanced mass flow rate from exhaust ventilation
        Real64 IntMassFlowRate = 0.0;                // unbalanced mass flow rate from intake ventilation
        Real64 ERVMassFlowRate = 0.0;                // unbalanced mass flow rate from stand-alone ERV
        bool OneTimeFlag = false;                    // One time flag to get nodes of stand alone ERV
        int NumOfERVs = 0;                           // Number of zone stand alone ERVs
        Array1D_int ERVInletNode;                    // Stand alone ERV supply air inlet nodes
        Array1D_int ERVExhaustNode;                  // Stand alone ERV air exhaust nodes
    };

    struct MixingData
    {
        // Members
        std::string Name;
        int ZonePtr = 0;
        int SchedPtr = 0;
        Real64 DesignLevel = 0.0;
        int FromZone = 0;
        Real64 DeltaTemperature = 0.0;
        Real64 DesiredAirFlowRate = 0.0;
        Real64 DesiredAirFlowRateSaved = 0.0;
        Real64 MixingMassFlowRate = 0.0;
        int DeltaTempSchedPtr = 0;                                // Delta temperature schedule index
        int MinIndoorTempSchedPtr = 0;                            // Minimum indoor temperature schedule index
        int MaxIndoorTempSchedPtr = 0;                            // Maximum indoor temperature schedule index
        int MinSourceTempSchedPtr = 0;                            // Minimum source zone temperature schedule index
        int MaxSourceTempSchedPtr = 0;                            // Maximum source zone temperature schedule index
        int MinOutdoorTempSchedPtr = 0;                           // Minimum outdoor temperature schedule index
        int MaxOutdoorTempSchedPtr = 0;                           // Maximum outdoor temperature schedule index
        int IndoorTempErrCount = 0;                               // Indoor temperature error count
        int SourceTempErrCount = 0;                               // Source zone temperature error count
        int OutdoorTempErrCount = 0;                              // Outdoor temperature error count
        int IndoorTempErrIndex = 0;                               // Indoor temperature error Index
        int SourceTempErrIndex = 0;                               // Source zone temperature error Index
        int OutdoorTempErrIndex = 0;                              // Outdoor temperature error Index
        HybridCtrlType HybridControlType = HybridCtrlType::Indiv; // Hybrid ventilation control type: 0 Individual, 1 Close, 2 Global
        int HybridControlMasterNum = 0;                           // Hybrid ventilation control master ventilation object number
        int NumRefDoorConnections = 0;
        bool EMSSimpleMixingOn = false;      // EMS actuating ventilation flow rate if .TRUE.
        bool RefDoorMixFlag = false;         // Refrigeration door mixing within zone
        Real64 EMSimpleMixingFlowRate = 0.0; // Value EMS is directing to use for override
        Array1D_bool EMSRefDoorMixingOn;
        Array1D<Real64> EMSRefDoorFlowRate;
        Array1D<Real64> VolRefDoorFlowRate;
        Array1D_int OpenSchedPtr;            // Schedule for Refrigeration door open fraction
        Array1D<Real64> DoorHeight;          // Door height for refrigeration door, m
        Array1D<Real64> DoorArea;            // Door area for refrigeration door, m2
        Array1D<Real64> Protection;          // Refrigeration door protection factor, dimensionless
        Array1D_int MateZonePtr;             // Zone connected by refrigeration door (MateZone > ZonePtr)
        Array1D_string DoorMixingObjectName; // Used in one error statement and eio
        Array1D_string DoorProtTypeName;     // Used in eio
                                             // Note, for mixing and crossmixing, this type dimensioned by number of mixing objects.
        // For ref door mixing, dimensioned by number of zones.
    };

    struct ZoneAirMassFlowConservation
    {
        // Members
        bool EnforceZoneMassBalance = false; // flag to enforce zone air mass conservation
        AdjustmentType ZoneFlowAdjustment =
            AdjustmentType::NoAdjustReturnAndMixing; // determines how zone air flow is adjusted (AdjustMixingOnly, AdjustReturnOnly,
                                                     // AdjustMixingThenReturn, AdjustReturnThenMixing, None)        int InfiltrationTreatment;   //
                                                     // determines how infiltration is treated for zone mass balance
        InfiltrationFlow InfiltrationTreatment = InfiltrationFlow::No;             // determines how infiltration is treated for zone mass balance
        InfiltrationZoneType InfiltrationForZones = InfiltrationZoneType::Invalid; // specifies which types of zones allow infiltration to be changed
        bool AdjustZoneMixingFlow = false;                                         // used to adjust zone mixing air flows to enforce air flow balance
        bool AdjustZoneInfiltrationFlow = false; // used to adjust zone infiltration air flows to enforce air flow balance
                                                 // Note, unique global object
    };

    struct ZoneMassConservationData
    {
        // Members
        std::string Name;
        int ZonePtr = 0;                       // pointer to the mixing zone
        Real64 InMassFlowRate = 0.0;           // zone total supply air mass flow rate, kg/s
        Real64 ExhMassFlowRate = 0.0;          // zone exhaust total air mass flow rate, kg/s
        Real64 RetMassFlowRate = 0.0;          // zone return air mass flow rate, kg/s
        Real64 MixingMassFlowRate = 0.0;       // zone mixing air mass flow rate, kg/s
        Real64 MixingSourceMassFlowRate = 0.0; // Zone source mass flow rate for mixing zone, kg/s
        int NumSourceZonesMixingObject = 0;    // number of zone mixing object references as a source zone
        int NumReceivingZonesMixingObject = 0; // number of zone mixing object references as a receiving zone
        bool IsOnlySourceZone = false;         // true only if used only as a source zone in zone mixing object
        bool IsSourceAndReceivingZone = false; // true only if a zone is used as a source and receiving zone in zone mixing objects
        int InfiltrationPtr = 0;               // pointer to infiltration object
        Real64 InfiltrationMassFlowRate = 0.0; // infiltration added to enforced source zone mass balance, kg/s
        int IncludeInfilToZoneMassBal = 0;     // not self-balanced, include infiltration in zone air mass balance
        Array1D_int ZoneMixingSourcesPtr;      // source zones pointer
        Array1D_int ZoneMixingReceivingPtr;    // receiving zones pointer
        Array1D<Real64> ZoneMixingReceivingFr; // receiving zones fraction
                                               // Note, this type dimensioned by number of zones
    };

    struct GenericComponentZoneIntGainStruct
    {
        // Members
        std::string CompObjectType;                   // device object class name
        std::string CompObjectName;                   // device user unique name
        IntGainType CompType = IntGainType::Invalid;  // type of internal gain device identifier
        Real64 spaceGainFrac = 1.0;                   // Fraction of gain value assigned to this Space (because gain rate might be for an entire zone)
        Real64 *PtrConvectGainRate = nullptr;         // POINTER to value of convection heat gain rate for device, watts
        Real64 ConvectGainRate = 0.0;                 // current timestep value of convection heat gain rate for device, watts
        Real64 *PtrReturnAirConvGainRate = nullptr;   // POINTER to value of return air convection heat gain rate for device, W
        Real64 ReturnAirConvGainRate = 0.0;           // current timestep value of return air convection heat gain rate for device, W
        Real64 *PtrRadiantGainRate = nullptr;         // POINTER to value of thermal radiation heat gain rate for device, watts
        Real64 RadiantGainRate = 0.0;                 // current timestep value of thermal radiation heat gain rate for device, watts
        Real64 *PtrLatentGainRate = nullptr;          // POINTER to value of moisture gain rate for device, Watts
        Real64 LatentGainRate = 0.0;                  // current timestep value of moisture gain rate for device, Watts
        Real64 *PtrReturnAirLatentGainRate = nullptr; // POINTER to value of return air moisture gain rate for device, Watts
        Real64 ReturnAirLatentGainRate = 0.0;         // current timestep value of return air moisture gain rate for device, Watts
        Real64 *PtrCarbonDioxideGainRate = nullptr;   // POINTER to value of carbon dioxide gain rate for device
        Real64 CarbonDioxideGainRate = 0.0;           // current timestep value of carbon dioxide gain rate for device
        Real64 *PtrGenericContamGainRate = nullptr;   // POINTER to value of generic contaminant gain rate for device
        Real64 GenericContamGainRate = 0.0;           // current timestep value of generic contaminant gain rate for device
        int ReturnAirNodeNum = 0;                     // return air node number for retrun air convection heat gain
    };

    struct SpaceZoneSimData // Calculated data by Space or Zone during each time step/hour
    {
        // Members
        Real64 NOFOCC = 0.0;  // Number of Occupants
        Real64 QOCTOT = 0.0;  // Total Energy from Occupants
        Real64 QOCSEN = 0.0;  // Sensible Energy from Occupants
        Real64 QOCCON = 0.0;  // ENERGY CONVECTED FROM OCCUPANTS (WH)
        Real64 QOCRAD = 0.0;  // ENERGY RADIATED FROM OCCUPANTS
        Real64 QOCLAT = 0.0;  // LATENT ENERGY FROM OCCUPANTS
        Real64 QLTTOT = 0.0;  // TOTAL ENERGY INTO LIGHTS (WH)
        Real64 QLTCON = 0.0;  // ENERGY CONVECTED TO SPACE AIR FROM LIGHTS
        Real64 QLTRAD = 0.0;  // ENERGY RADIATED TO SPACE FROM LIGHTS
        Real64 QLTCRA = 0.0;  // ENERGY CONVECTED TO RETURN AIR FROM LIGHTS
        Real64 QLTSW = 0.0;   // VISIBLE ENERGY FROM LIGHTS
        Real64 QEECON = 0.0;  // ENERGY CONVECTED FROM ELECTRIC EQUIPMENT
        Real64 QEERAD = 0.0;  // ENERGY RADIATED FROM ELECTRIC EQUIPMENT
        Real64 QEELost = 0.0; // Energy from Electric Equipment (lost)
        Real64 QEELAT = 0.0;  // LATENT ENERGY FROM Electric Equipment
        Real64 QGECON = 0.0;  // ENERGY CONVECTED FROM GAS EQUIPMENT
        Real64 QGERAD = 0.0;  // ENERGY RADIATED FROM GAS EQUIPMENT
        Real64 QGELost = 0.0; // Energy from Gas Equipment (lost)
        Real64 QGELAT = 0.0;  // LATENT ENERGY FROM Gas Equipment
        Real64 QOECON = 0.0;  // ENERGY CONVECTED FROM OTHER EQUIPMENT
        Real64 QOERAD = 0.0;  // ENERGY RADIATED FROM OTHER EQUIPMENT
        Real64 QOELost = 0.0; // Energy from Other Equipment (lost)
        Real64 QOELAT = 0.0;  // LATENT ENERGY FROM Other Equipment
        Real64 QHWCON = 0.0;  // ENERGY CONVECTED FROM Hot Water EQUIPMENT
        Real64 QHWRAD = 0.0;  // ENERGY RADIATED FROM Hot Water EQUIPMENT
        Real64 QHWLost = 0.0; // Energy from Hot Water Equipment (lost)
        Real64 QHWLAT = 0.0;  // LATENT ENERGY FROM Hot Water Equipment
        Real64 QSECON = 0.0;  // ENERGY CONVECTED FROM Steam EQUIPMENT
        Real64 QSERAD = 0.0;  // ENERGY RADIATED FROM Steam EQUIPMENT
        Real64 QSELost = 0.0; // Energy from Steam Equipment (lost)
        Real64 QSELAT = 0.0;  // LATENT ENERGY FROM Steam Equipment
        Real64 QBBCON = 0.0;  // ENERGY CONVECTED FROM BASEBOARD HEATING
        Real64 QBBRAD = 0.0;  // ENERGY RADIATED FROM BASEBOARD HEATING
    };

    struct SpaceIntGainDeviceData
    {
        int numberOfDevices = 0;
        int maxNumberOfDevices = 0;
        Array1D<GenericComponentZoneIntGainStruct> device;
    };

    struct WindowBlindProperties
    {
        // Members
        std::string Name;
        int MaterialNumber = 0; // Material pointer for the blind
        // Input properties
        DataWindowEquivalentLayer::Orientation SlatOrientation = DataWindowEquivalentLayer::Orientation::Invalid; // HORIZONTAL or VERTICAL
        DataWindowEquivalentLayer::AngleType SlatAngleType = DataWindowEquivalentLayer::AngleType::Fixed;         // FIXED or VARIABLE
        Real64 SlatWidth = 0.0;                                                                                   // Slat width (m)
        Real64 SlatSeparation = 0.0;                                                                              // Slat separation (m)
        Real64 SlatThickness = 0.0;                                                                               // Slat thickness (m)
        Real64 SlatCrown = 0.0;        // the height of the slate (length from the chord to the curve)
        Real64 SlatAngle = 0.0;        // Slat angle (deg)
        Real64 MinSlatAngle = 0.0;     // Minimum slat angle for variable-angle slats (deg) (user input)
        Real64 MaxSlatAngle = 0.0;     // Maximum slat angle for variable-angle slats (deg) (user input)
        Real64 SlatConductivity = 0.0; // Slat conductivity (W/m-K)
        // Solar slat properties
        Real64 SlatTransSolBeamDiff = 0.0;     // Slat solar beam-diffuse transmittance
        Real64 SlatFrontReflSolBeamDiff = 0.0; // Slat front solar beam-diffuse reflectance
        Real64 SlatBackReflSolBeamDiff = 0.0;  // Slat back solar beam-diffuse reflectance
        Real64 SlatTransSolDiffDiff = 0.0;     // Slat solar diffuse-diffuse transmittance
        Real64 SlatFrontReflSolDiffDiff = 0.0; // Slat front solar diffuse-diffuse reflectance
        Real64 SlatBackReflSolDiffDiff = 0.0;  // Slat back solar diffuse-diffuse reflectance
        // Visible slat properties
        Real64 SlatTransVisBeamDiff = 0.0;     // Slat visible beam-diffuse transmittance
        Real64 SlatFrontReflVisBeamDiff = 0.0; // Slat front visible beam-diffuse reflectance
        Real64 SlatBackReflVisBeamDiff = 0.0;  // Slat back visible beam-diffuse reflectance
        Real64 SlatTransVisDiffDiff = 0.0;     // Slat visible diffuse-diffuse transmittance
        Real64 SlatFrontReflVisDiffDiff = 0.0; // Slat front visible diffuse-diffuse reflectance
        Real64 SlatBackReflVisDiffDiff = 0.0;  // Slat back visible diffuse-diffuse reflectance
        // Long-wave (IR) slat properties
        Real64 SlatTransIR = 0.0;      // Slat IR transmittance
        Real64 SlatFrontEmissIR = 0.0; // Slat front emissivity
        Real64 SlatBackEmissIR = 0.0;  // Slat back emissivity
        // Some characteristics for blind thermal calculation
        Real64 BlindToGlassDist = 0.0;    // Distance between window shade and adjacent glass (m)
        Real64 BlindTopOpeningMult = 0.0; // Area of air-flow opening at top of blind, expressed as a fraction
        //  of the blind-to-glass opening area at the top of the blind
        Real64 BlindBottomOpeningMult = 0.0; // Area of air-flow opening at bottom of blind, expressed as a fraction
        //  of the blind-to-glass opening area at the bottom of the blind
        Real64 BlindLeftOpeningMult = 0.0; // Area of air-flow opening at left side of blind, expressed as a fraction
        //  of the blind-to-glass opening area at the left side of the blind
        Real64 BlindRightOpeningMult = 0.0; // Area of air-flow opening at right side of blind, expressed as a fraction
        //  of the blind-to-glass opening area at the right side of the blind
        // Calculated blind properties
        // Blind solar properties
        Array2D<Real64> SolFrontBeamBeamTrans; // Blind solar front beam-beam transmittance vs.
        // profile angle, slat angle
        Array2D<Real64> SolFrontBeamBeamRefl; // Blind solar front beam-beam reflectance vs. profile angle,
        // slat angle (zero)
        Array2D<Real64> SolBackBeamBeamTrans; // Blind solar back beam-beam transmittance vs. profile angle,
        // slat angle
        Array2D<Real64> SolBackBeamBeamRefl; // Blind solar back beam-beam reflectance vs. profile angle,
        // slat angle (zero)
        Array2D<Real64> SolFrontBeamDiffTrans; // Blind solar front beam-diffuse transmittance
        // vs. profile angle, slat angle
        Array2D<Real64> SolFrontBeamDiffRefl; // Blind solar front beam-diffuse reflectance
        // vs. profile angle, slat angle
        Array2D<Real64> SolBackBeamDiffTrans; // Blind solar back beam-diffuse transmittance
        // vs. profile angle, slat angle
        Array2D<Real64> SolBackBeamDiffRefl; // Blind solar back beam-diffuse reflectance
        // vs. profile angle, slat angle
        Array1D<Real64> SolFrontDiffDiffTrans; // Blind solar front diffuse-diffuse transmittance
        // vs. slat angle
        Array1D<Real64> SolFrontDiffDiffTransGnd; // Blind ground solar front diffuse-diffuse transmittance
        // vs. slat angle
        Array1D<Real64> SolFrontDiffDiffTransSky; // Blind sky solar front diffuse-diffuse transmittance
        // vs. slat angle
        Array1D<Real64> SolFrontDiffDiffRefl; // Blind solar front diffuse-diffuse reflectance
        // vs. slat angle
        Array1D<Real64> SolFrontDiffDiffReflGnd; // Blind ground solar front diffuse-diffuse reflectance
        // vs. slat angle
        Array1D<Real64> SolFrontDiffDiffReflSky; // Blind sky solar front diffuse-diffuse reflectance
        // vs. slat angle
        Array1D<Real64> SolBackDiffDiffTrans; // Blind solar back diffuse-diffuse transmittance
        // vs. slat angle
        Array1D<Real64> SolBackDiffDiffRefl; // Blind solar back diffuse-diffuse reflectance
        // vs. slat angle
        Array2D<Real64> SolFrontBeamAbs;    // Blind solar front beam absorptance vs. slat angle
        Array2D<Real64> SolBackBeamAbs;     // Blind solar back beam absorptance vs. slat angle
        Array1D<Real64> SolFrontDiffAbs;    // Blind solar front diffuse absorptance vs. slat angle
        Array1D<Real64> SolFrontDiffAbsGnd; // Blind ground solar front diffuse absorptance vs. slat angle
        Array1D<Real64> SolFrontDiffAbsSky; // Blind sky solar front diffuse absorptance vs. slat angle
        Array1D<Real64> SolBackDiffAbs;     // Blind solar back diffuse absorptance vs. slat angle
        // Blind visible properties
        Array2D<Real64> VisFrontBeamBeamTrans; // Blind visible front beam-beam transmittance
        // vs. profile angle, slat angle
        Array2D<Real64> VisFrontBeamBeamRefl; // Blind visible front beam-beam reflectance
        // vs. profile angle, slat angle (zero)
        Array2D<Real64> VisBackBeamBeamTrans; // Blind visible back beam-beam transmittance
        // vs. profile angle, slat angle
        Array2D<Real64> VisBackBeamBeamRefl; // Blind visible back beam-beam reflectance
        // vs. profile angle, slat angle (zero)
        Array2D<Real64> VisFrontBeamDiffTrans; // Blind visible front beam-diffuse transmittance
        // vs. profile angle, slat angle
        Array2D<Real64> VisFrontBeamDiffRefl; // Blind visible front beam-diffuse reflectance
        // vs. profile angle, slat angle
        Array2D<Real64> VisBackBeamDiffTrans; // Blind visible back beam-diffuse transmittance
        // vs. profile angle, slat angle
        Array2D<Real64> VisBackBeamDiffRefl; // Blind visible back beam-diffuse reflectance
        // vs. profile angle, slat angle
        Array1D<Real64> VisFrontDiffDiffTrans; // Blind visible front diffuse-diffuse transmittance
        // vs. slat angle
        Array1D<Real64> VisFrontDiffDiffRefl; // Blind visible front diffuse-diffuse reflectance
        // vs. slat angle
        Array1D<Real64> VisBackDiffDiffTrans; // Blind visible back diffuse-diffuse transmittance
        // vs. slat angle
        Array1D<Real64> VisBackDiffDiffRefl; // Blind visible back diffuse-diffuse reflectance
        // vs. slat angle
        // Long-wave (IR) blind properties
        Array1D<Real64> IRFrontTrans; // Blind IR front transmittance vs. slat angle
        Array1D<Real64> IRFrontEmiss; // Blind IR front emissivity vs. slat angle
        Array1D<Real64> IRBackTrans;  // Blind IR back transmittance vs. slat angle
        Array1D<Real64> IRBackEmiss;  // Blind IR back emissivity vs. slat angle

        // Default Constructor
        WindowBlindProperties()
            : SolFrontBeamBeamTrans(MaxSlatAngs, 37, 0.0), SolFrontBeamBeamRefl(MaxSlatAngs, 37, 0.0), SolBackBeamBeamTrans(MaxSlatAngs, 37, 0.0),
              SolBackBeamBeamRefl(MaxSlatAngs, 37, 0.0), SolFrontBeamDiffTrans(MaxSlatAngs, 37, 0.0), SolFrontBeamDiffRefl(MaxSlatAngs, 37, 0.0),
              SolBackBeamDiffTrans(MaxSlatAngs, 37, 0.0), SolBackBeamDiffRefl(MaxSlatAngs, 37, 0.0), SolFrontDiffDiffTrans(MaxSlatAngs, 0.0),
              SolFrontDiffDiffTransGnd(MaxSlatAngs, 0.0), SolFrontDiffDiffTransSky(MaxSlatAngs, 0.0), SolFrontDiffDiffRefl(MaxSlatAngs, 0.0),
              SolFrontDiffDiffReflGnd(MaxSlatAngs, 0.0), SolFrontDiffDiffReflSky(MaxSlatAngs, 0.0), SolBackDiffDiffTrans(MaxSlatAngs, 0.0),
              SolBackDiffDiffRefl(MaxSlatAngs, 0.0), SolFrontBeamAbs(MaxSlatAngs, 37, 0.0), SolBackBeamAbs(MaxSlatAngs, 37, 0.0),
              SolFrontDiffAbs(MaxSlatAngs, 0.0), SolFrontDiffAbsGnd(MaxSlatAngs, 0.0), SolFrontDiffAbsSky(MaxSlatAngs, 0.0),
              SolBackDiffAbs(MaxSlatAngs, 0.0), VisFrontBeamBeamTrans(MaxSlatAngs, 37, 0.0), VisFrontBeamBeamRefl(MaxSlatAngs, 37, 0.0),
              VisBackBeamBeamTrans(MaxSlatAngs, 37, 0.0), VisBackBeamBeamRefl(MaxSlatAngs, 37, 0.0), VisFrontBeamDiffTrans(MaxSlatAngs, 37, 0.0),
              VisFrontBeamDiffRefl(MaxSlatAngs, 37, 0.0), VisBackBeamDiffTrans(MaxSlatAngs, 37, 0.0), VisBackBeamDiffRefl(MaxSlatAngs, 37, 0.0),
              VisFrontDiffDiffTrans(MaxSlatAngs, 0.0), VisFrontDiffDiffRefl(MaxSlatAngs, 0.0), VisBackDiffDiffTrans(MaxSlatAngs, 0.0),
              VisBackDiffDiffRefl(MaxSlatAngs, 0.0), IRFrontTrans(MaxSlatAngs, 0.0), IRFrontEmiss(MaxSlatAngs, 0.0), IRBackTrans(MaxSlatAngs, 0.0),
              IRBackEmiss(MaxSlatAngs, 0.0)
        {
        }
    };

    struct SurfaceScreenProperties
    {
        // Members
        int MaterialNumber = 0; // Material pointer for the screen
        Real64 BmBmTrans = 0.0; // Beam solar transmittance (dependent on sun angle)
        // (this value can include scattering if the user so chooses)
        Real64 BmBmTransBack = 0.0; // Beam solar transmittance (dependent on sun angle) from back side of screen
        Real64 BmBmTransVis = 0.0;  // Visible solar transmittance (dependent on sun angle)
        // (this value can include visible scattering if the user so chooses)
        Real64 BmDifTrans = 0.0;     // Beam solar transmitted as diffuse radiation (dependent on sun angle)
        Real64 BmDifTransBack = 0.0; // Beam solar transmitted as diffuse radiation (dependent on sun angle) from back side
        Real64 BmDifTransVis = 0.0;  // Visible solar transmitted as diffuse radiation (dependent on sun angle)
        // The following reflectance properties are dependent on sun angle:
        Real64 ReflectSolBeamFront = 0.0;          // Beam solar reflected as diffuse radiation when sun is in front of screen
        Real64 ReflectVisBeamFront = 0.0;          // Visible solar reflected as diffuse radiation when sun is in front of screen
        Real64 ReflectSolBeamBack = 0.0;           // Beam solar reflected as diffuse radiation when sun is in back of screen
        Real64 ReflectVisBeamBack = 0.0;           // Visible solar reflected as diffuse radiation when sun is in back of screen
        Real64 AbsorpSolarBeamFront = 0.0;         // Front surface solar beam absorptance
        Real64 AbsorpSolarBeamBack = 0.0;          // Back surface solar beam absorptance
        Real64 DifDifTrans = 0.0;                  // Back surface diffuse solar transmitted
        Real64 DifDifTransVis = 0.0;               // Back surface diffuse visible solar transmitted
        Real64 DifScreenAbsorp = 0.0;              // Absorption of diffuse radiation
        Real64 DifReflect = 0.0;                   // Back reflection of solar diffuse radiation
        Real64 DifReflectVis = 0.0;                // Back reflection of visible diffuse radiation
        Real64 ReflectScreen = 0.0;                // Screen assembly solar reflectance (user input adjusted for holes in screen)
        Real64 ReflectScreenVis = 0.0;             // Screen assembly visible reflectance (user input adjusted for holes in screen)
        Real64 ReflectCylinder = 0.0;              // Screen material solar reflectance (user input, does not account for holes in screen)
        Real64 ReflectCylinderVis = 0.0;           // Screen material visible reflectance (user input, does not account for holes in screen)
        Real64 ScreenDiameterToSpacingRatio = 0.0; // ratio of screen material diameter to screen material spacing
        int ScreenBeamReflectanceAccounting = 0;   // user specified method of accounting for scattered solar beam
    };

    struct ScreenTransData
    {
        // Members
        Array2D<Real64> Trans;
        Array2D<Real64> Scatt;
    };

    struct ZoneCatEUseData
    {
        // Members
        Array1D<Real64> EEConvected; // Category (0 to 25) Energy Convected from Electric Equipment
        Array1D<Real64> EERadiated;  // Category (0 to 25) Energy Radiated from Electric Equipment
        Array1D<Real64> EELost;      // Category (0 to 25) Energy from Electric Equipment (lost)
        Array1D<Real64> EELatent;    // Category (0 to 25) Latent Energy from Electric Equipment

        // Default Constructor
        ZoneCatEUseData() : EEConvected({0, 25}, 0.0), EERadiated({0, 25}, 0.0), EELost({0, 25}, 0.0), EELatent({0, 25}, 0.0)
        {
        }
    };

    struct RefrigCaseCreditData
    {
        // Members
        Real64 SenCaseCreditToZone = 0.0; // Refrigerated display case sensible energy delivered to zone
        // includes refrigeration pipe and receiver heat exchange with zone
        Real64 LatCaseCreditToZone = 0.0; // Refrigerated display case latent energy delivered to zone
        Real64 SenCaseCreditToHVAC = 0.0; // Refrigerated display case sensible energy delivered to HVAC RA duct
        Real64 LatCaseCreditToHVAC = 0.0; // Refrigerated display case latent energy delivered to HVAC RA duct

        // Reset to Zeros
        void reset()
        {
            SenCaseCreditToZone = 0.0;
            LatCaseCreditToZone = 0.0;
            SenCaseCreditToHVAC = 0.0;
            LatCaseCreditToHVAC = 0.0;
        }
    };

    struct HeatReclaimDataBase
    {
        // Members
        std::string Name;                                         // Name of Coil
        std::string SourceType;                                   // SourceType for Coil
        Real64 AvailCapacity = 0.0;                               // Total available heat reclaim capacity
        Real64 ReclaimEfficiencyTotal = 0.0;                      // Total reclaimed portion
        Real64 WaterHeatingDesuperheaterReclaimedHeatTotal = 0.0; // total reclaimed heat by water heating desuperheater coils
        Real64 HVACDesuperheaterReclaimedHeatTotal = 0.0;         // total reclaimed heat by water heating desuperheater coils
        Array1D<Real64> WaterHeatingDesuperheaterReclaimedHeat;   // heat reclaimed by water heating desuperheater coils
        Array1D<Real64> HVACDesuperheaterReclaimedHeat;           // heat reclaimed by water heating desuperheater coils
    };

    struct HeatReclaimRefrigCondenserData : HeatReclaimDataBase // inherited from base struct
    {
        // Customized Members
        Real64 AvailTemperature = 0.0; // Temperature of heat reclaim source
    };

    struct AirReportVars
    {
        // Members
        Real64 MeanAirTemp = 0.0;            // Mean Air Temperature {C}
        Real64 OperativeTemp = 0.0;          // Average of Mean Air Temperature {C} and Mean Radiant Temperature {C}
        Real64 MeanAirHumRat = 0.0;          // Mean Air Humidity Ratio {kg/kg} (averaged over zone time step)
        Real64 MeanAirDewPointTemp = 0.0;    // Mean Air Dewpoint Temperature {C}
        Real64 ThermOperativeTemp = 0.0;     // Mix or MRT and MAT for Zone Control:Thermostatic:Operative Temperature {C}
        Real64 InfilHeatGain = 0.0;          // Heat Gain {J} due to infiltration
        Real64 InfilHeatLoss = 0.0;          // Heat Loss {J} due to infiltration
        Real64 InfilLatentGain = 0.0;        // Latent Gain {J} due to infiltration
        Real64 InfilLatentLoss = 0.0;        // Latent Loss {J} due to infiltration
        Real64 InfilTotalGain = 0.0;         // Total Gain {J} due to infiltration (sensible+latent)
        Real64 InfilTotalLoss = 0.0;         // Total Loss {J} due to infiltration (sensible+latent)
        Real64 InfilVolumeCurDensity = 0.0;  // Volume of Air {m3} due to infiltration at current zone air density
        Real64 InfilVolumeStdDensity = 0.0;  // Volume of Air {m3} due to infiltration at standard density (adjusted for elevation)
        Real64 InfilVdotCurDensity = 0.0;    // Volume flow rate of Air {m3/s} due to infiltration at current zone air density
        Real64 InfilVdotStdDensity = 0.0;    // Volume flow rate of Air {m3/s} due to infiltration standard density (adjusted elevation)
        Real64 InfilMass = 0.0;              // Mass of Air {kg} due to infiltration
        Real64 InfilMdot = 0.0;              // Mass flow rate of Air (kg/s) due to infiltration
        Real64 InfilAirChangeRate = 0.0;     // Infiltration air change rate {ach}
        Real64 VentilHeatLoss = 0.0;         // Heat Gain {J} due to ventilation
        Real64 VentilHeatGain = 0.0;         // Heat Loss {J} due to ventilation
        Real64 VentilLatentLoss = 0.0;       // Latent Gain {J} due to ventilation
        Real64 VentilLatentGain = 0.0;       // Latent Loss {J} due to ventilation
        Real64 VentilTotalLoss = 0.0;        // Total Gain {J} due to ventilation
        Real64 VentilTotalGain = 0.0;        // Total Loss {J} due to ventilation
        Real64 VentilVolumeCurDensity = 0.0; // Volume of Air {m3} due to ventilation at current zone air density
        Real64 VentilVolumeStdDensity = 0.0; // Volume of Air {m3} due to ventilation at standard density (adjusted for elevation)
        Real64 VentilVdotCurDensity = 0.0;   // Volume flow rate of Air {m3/s} due to ventilation at current zone air density
        Real64 VentilVdotStdDensity = 0.0;   // Volume flow rate of Air {m3/s} due to ventilation at standard density (adjusted elevation)
        Real64 VentilMass = 0.0;             // Mass of Air {kg} due to ventilation
        Real64 VentilMdot = 0.0;             // Mass flow rate of Air {kg/s} due to ventilation
        Real64 VentilAirChangeRate = 0.0;    // Ventilation air change rate (ach)
        Real64 VentilFanElec = 0.0;          // Fan Electricity {W} due to ventilation
        Real64 VentilAirTemp = 0.0;          // Air Temp {C} of ventilation
        Real64 MixVolume = 0.0;              // Mixing volume of Air {m3}
        Real64 MixVdotCurDensity = 0.0;      // Mixing volume flow rate of Air {m3/s} at current zone air density
        Real64 MixVdotStdDensity = 0.0;      // Mixing volume flow rate of Air {m3/s} at standard density (adjusted for elevation)
        Real64 MixMass = 0.0;                // Mixing mass of air {kg}
        Real64 MixMdot = 0.0;                // Mixing mass flow rate of air {kg/s}
        Real64 MixHeatLoss = 0.0;            // Heat Gain {J} due to mixing and cross mixing and refrigeration door mixing
        Real64 MixHeatGain = 0.0;            // Heat Loss {J} due to mixing and cross mixing and refrigeration door mixing
        Real64 MixLatentLoss = 0.0;          // Latent Gain {J} due to mixing and cross mixing and refrigeration door mixing
        Real64 MixLatentGain = 0.0;          // Latent Loss {J} due to mixing and cross mixing and refrigeration door mixing
        Real64 MixTotalLoss = 0.0;           // Total Gain {J} due to mixing and cross mixing and refrigeration door mixing
        Real64 MixTotalGain = 0.0;           // Total Loss {J} due to mixing and cross mixing and refrigeration door mixing
        Real64 SysInletMass = 0.0;           // Total mass of Air {kg} from all system inlets
        Real64 SysOutletMass = 0.0;          // Total mass of Air {kg} from all system outlets
        Real64 ExfilMass = 0.0;              // Mass of Air {kg} due to exfiltration
        Real64 ExfilTotalLoss = 0.0;         // Total Loss rate {W} due to exfiltration (sensible+latent)
        Real64 ExfilSensiLoss = 0.0;         // Sensible Loss rate {W} due to exfiltration
        Real64 ExfilLatentLoss = 0.0;        // Latent Loss rate {W} due to exfiltration
        Real64 ExhTotalLoss = 0.0;           // Total Loss rate {W} due to zone exhaust air (sensible+latent)
        Real64 ExhSensiLoss = 0.0;           // Sensible Loss rate {W} due to zone exhaust air
        Real64 ExhLatentLoss = 0.0;          // Latent Loss rate {W} due to zone exhaust air
        // air heat balance component load summary results
        Real64 SumIntGains = 0.0;     // Zone sum of convective internal gains
        Real64 SumHADTsurfs = 0.0;    // Zone sum of Hc*Area*(Tsurf - Tz)
        Real64 SumMCpDTzones = 0.0;   // zone sum of MassFlowRate*cp*(TremotZone - Tz) transfer air from other zone, Mixing
        Real64 SumMCpDtInfil = 0.0;   // Zone sum of MassFlowRate*Cp*(Tout - Tz) transfer from outside, ventil, earth tube
        Real64 SumMCpDTsystem = 0.0;  // Zone sum of air system MassFlowRate*Cp*(Tsup - Tz)
        Real64 SumNonAirSystem = 0.0; // Zone sum of system convective gains, collected via NonAirSystemResponse
        Real64 CzdTdt = 0.0;          // Zone air energy storage term.
        Real64 imBalance = 0.0;       // put all terms in eq. 5 on RHS , should be zero
        // for ZoneAirBalance:OutdoorAir object Outputs only
        Real64 OABalanceHeatLoss = 0.0;         // Heat Gain {J} due to OA air balance
        Real64 OABalanceHeatGain = 0.0;         // Heat Loss {J} due to OA air balance
        Real64 OABalanceLatentLoss = 0.0;       // Latent Gain {J} due to OA air balance
        Real64 OABalanceLatentGain = 0.0;       // Latent Loss {J} due to OA air balance
        Real64 OABalanceTotalLoss = 0.0;        // Total Gain {J} due to OA air balance
        Real64 OABalanceTotalGain = 0.0;        // Total Loss {J} due to OA air balance
        Real64 OABalanceVolumeCurDensity = 0.0; // Volume of Air {m3} due to OA air balance
        // at current zone air density
        Real64 OABalanceVolumeStdDensity = 0.0; // Volume of Air {m3} due to OA air balance
        // at standard density (adjusted for elevation)
        Real64 OABalanceVdotCurDensity = 0.0; // Volume flow rate of Air {m3/s} due to OA air balance
        // at current zone air density
        Real64 OABalanceVdotStdDensity = 0.0; // Volume flow rate of Air {m3/s} due to OA air balance
        // at standard density (adjusted elevation)
        Real64 OABalanceMass = 0.0;          // Mass of Air {kg} due to OA air balance
        Real64 OABalanceMdot = 0.0;          // Mass flow rate of Air {kg/s} due to OA air balance
        Real64 OABalanceAirChangeRate = 0.0; // OA air balance air change rate (ach)
        Real64 OABalanceFanElec = 0.0;       // Fan Electricity {W} due to OA air balance
        Real64 SumEnthalpyM = 0.0;           // Zone sum of EnthalpyM
        Real64 SumEnthalpyH = 0.0;           // Zone sum of EnthalpyH
    };

    struct ZonePreDefRepType
    {
        // Members
        bool isOccupied = false;      // occupied during the current time step
        Real64 NumOcc = 0.0;          // number of occupants - used in calculating Vbz
        Real64 NumOccAccum = 0.0;     // number of occupants accumulating for entire simulation
        Real64 NumOccAccumTime = 0.0; // time that the number of occupants is accumulating to compute average
        //  - zone time step [hrs]
        Real64 TotTimeOcc = 0.0; // time occupied (and the mechanical ventilation volume is accumulating)
        //  - system time step [hrs]

        // OA Reports - accumulated values
        // All Vol variables are in m3
        Real64 MechVentVolTotalOcc = 0.0;       // volume for mechanical ventilation of outside air for entire simulation during occupied at current
        Real64 MechVentVolMin = 9.9e9;          // a large number since finding minimum volume at current zone air density
        Real64 InfilVolTotalOcc = 0.0;          // volume for infiltration of outside air for entire simulation during occupied at current density
        Real64 InfilVolMin = 9.9e9;             // a large number since finding minimum volume at current zone air density
        Real64 AFNInfilVolTotalOcc = 0.0;       // volume for infiltration of outside air for entire simulation during occupied at zone air density
        Real64 AFNInfilVolMin = 9.9e9;          // a large number since finding minimum volume at current zone air density
        Real64 SimpVentVolTotalOcc = 0.0;       // volume for simple 'ZoneVentilation' of outside air for entire simulation during occupied current
        Real64 SimpVentVolMin = 9.9e9;          // a large number since finding minimum volumeat current zone air density
        Real64 MechVentVolTotalStdDen = 0.0;    // volume for mechanical ventilation of outside air for entire simulation at standard density
        Real64 MechVentVolTotalOccStdDen = 0.0; // volume for mechanical ventilation of outside air for entire simulation during occupied at std
        Real64 InfilVolTotalStdDen = 0.0;       // volume for infiltration of outside air for entire simulation at standard density
        Real64 InfilVolTotalOccStdDen = 0.0;    // volume for infiltration of outside air for entire simulation during occupied standard density
        Real64 AFNInfilVolTotalStdDen = 0.0;    // volume for AFN infiltration of outside air for entire simulation at standard density
        Real64 AFNInfilVolTotalOccStdDen = 0.0; // volume for AFN infiltration of outside air for entire simulation during occupied at std density
        Real64 AFNVentVolStdDen = 0.0;          // volume flow rate for natural ventilation at standard density
        Real64 AFNVentVolTotalStdDen = 0.0;     // volume for natural ventilation for entire simulation at standard density
        Real64 AFNVentVolTotalOccStdDen = 0.0;  // volume for natural ventilatiofor entire simulation n during occupied at standard density
        Real64 SimpVentVolTotalStdDen = 0.0;    // volume for simple 'ZoneVentilation' for entire simulation at standard density
        Real64 SimpVentVolTotalOccStdDen = 0.0; // volume for simple 'ZoneVentilation' for entire simulation during occupied at standard density
        Real64 VozMin = 0.0;                    // minimum outdoor zone ventilation
        Real64 VozTargetTotal = 0.0;            // volume for target Voz-dyn for entire simulation at std density
        Real64 VozTargetTotalOcc = 0.0;         // volume for target Voz-dyn for entire simulation during occupied
        Real64 VozTargetTimeBelow = 0.0;        // time [hrs] that mechanical+natural ventilation is < VozTarget - 1%
        Real64 VozTargetTimeAt = 0.0;           // time [hrs] that mechanical+natural ventilation is = VozTarget within 1% and > zero
        Real64 VozTargetTimeAbove = 0.0;        // time [hrs] that mechanical+natural ventilation is > VozTarget + 1%
        Real64 VozTargetTimeBelowOcc = 0.0;     // time [hrs] that mechanical+natural ventilation is < VozTarget - 1% during occupied
        Real64 VozTargetTimeAtOcc = 0.0;        // time [hrs] that mechanical+natural ventilation is = VozTarget within 1% and > zero during occupied
        Real64 VozTargetTimeAboveOcc = 0.0;     // time [hrs] that mechanical+natural ventilation is > VozTarget + 1% during occupied
        Real64 TotVentTimeNonZeroUnocc = 0.0;   // time [hrs] that mechanical+natural ventilation is > zero during UNoccupied

        // for Sensible Heat Gas Component Report
        // annual
        Real64 SHGSAnZoneEqHt = 0.0;  // Zone Eq heating
        Real64 SHGSAnZoneEqCl = 0.0;  // Zone Eq cooling
        Real64 SHGSAnHvacATUHt = 0.0; // heating by Air Terminal Unit [J]
        Real64 SHGSAnHvacATUCl = 0.0; // cooling by Air Terminal Unit [J]
        Real64 SHGSAnSurfHt = 0.0;    // heated surface heating
        Real64 SHGSAnSurfCl = 0.0;    // cooled surface cooling
        Real64 SHGSAnPeoplAdd = 0.0;  // people additions
        Real64 SHGSAnLiteAdd = 0.0;   // lighting addition
        Real64 SHGSAnEquipAdd = 0.0;  // equipment addition
        Real64 SHGSAnWindAdd = 0.0;   // window addition
        Real64 SHGSAnIzaAdd = 0.0;    // inter zone air addition
        Real64 SHGSAnInfilAdd = 0.0;  // infiltration addition
        Real64 SHGSAnOtherAdd = 0.0;  // opaque surface and other addition
        Real64 SHGSAnEquipRem = 0.0;  // equipment removal
        Real64 SHGSAnWindRem = 0.0;   // window removal
        Real64 SHGSAnIzaRem = 0.0;    // inter-zone air removal
        Real64 SHGSAnInfilRem = 0.0;  // infiltration removal
        Real64 SHGSAnOtherRem = 0.0;  // opaque surface and other removal
        // peak cooling
        int clPtTimeStamp = 0;        // timestamp for the cooling peak
        Real64 clPeak = 0.0;          // cooling peak value (hvac air cooling + cooled surface)
        Real64 SHGSClHvacHt = 0.0;    // hvac air heating
        Real64 SHGSClHvacCl = 0.0;    // hvac air cooling
        Real64 SHGSClHvacATUHt = 0.0; // heating by air terminal unit at cool peak [W]
        Real64 SHGSClHvacATUCl = 0.0; // cooling by air terminal unit at cool peak [W]
        Real64 SHGSClSurfHt = 0.0;    // heated surface heating
        Real64 SHGSClSurfCl = 0.0;    // cooled surface cooling
        Real64 SHGSClPeoplAdd = 0.0;  // people additions
        Real64 SHGSClLiteAdd = 0.0;   // lighting addition
        Real64 SHGSClEquipAdd = 0.0;  // equipment addition
        Real64 SHGSClWindAdd = 0.0;   // window addition
        Real64 SHGSClIzaAdd = 0.0;    // inter zone air addition
        Real64 SHGSClInfilAdd = 0.0;  // infiltration addition
        Real64 SHGSClOtherAdd = 0.0;  // opaque surface and other addition
        Real64 SHGSClEquipRem = 0.0;  // equipment removal
        Real64 SHGSClWindRem = 0.0;   // window removal
        Real64 SHGSClIzaRem = 0.0;    // inter-zone air removal
        Real64 SHGSClInfilRem = 0.0;  // infiltration removal
        Real64 SHGSClOtherRem = 0.0;  // opaque surface and other removal
        // peak heating
        int htPtTimeStamp = 0;        // timestamp for the heating peak
        Real64 htPeak = 0.0;          // heating peak value (hvac air heating + heated surface)
        Real64 SHGSHtHvacHt = 0.0;    // hvac air heating
        Real64 SHGSHtHvacCl = 0.0;    // hvac air cooling
        Real64 SHGSHtHvacATUHt = 0.0; // heating by air terminal unit at heat peak [W]
        Real64 SHGSHtHvacATUCl = 0.0; // cooling by air terminal unit at heat peak [W]
        Real64 SHGSHtSurfHt = 0.0;    // heated surface heating
        Real64 SHGSHtSurfCl = 0.0;    // cooled surface cooling
        Real64 SHGSHtPeoplAdd = 0.0;  // people additions
        Real64 SHGSHtLiteAdd = 0.0;   // lighting addition
        Real64 SHGSHtEquipAdd = 0.0;  // equipment addition
        Real64 SHGSHtWindAdd = 0.0;   // window addition
        Real64 SHGSHtIzaAdd = 0.0;    // inter zone air addition
        Real64 SHGSHtInfilAdd = 0.0;  // infiltration addition
        Real64 SHGSHtOtherAdd = 0.0;  // opaque surface and other addition
        Real64 SHGSHtEquipRem = 0.0;  // equipment removal
        Real64 SHGSHtWindRem = 0.0;   // window removal
        Real64 SHGSHtIzaRem = 0.0;    // inter-zone air removal
        Real64 SHGSHtInfilRem = 0.0;  // infiltration removal
        Real64 SHGSHtOtherRem = 0.0;  // opaque surface and other removal

        // heat emission
        Real64 emiEnvelopConv = 0.0;      // heat emission from envelope convection
        Real64 emiZoneExfiltration = 0.0; // heat emission from zone exfiltration
        Real64 emiZoneExhaust = 0.0;      // heat emission from zone exhaust air
        Real64 emiHVACRelief = 0.0;       // heat emission from HVAC relief air
        Real64 emiHVACReject = 0.0;       // heat emission from HVAC reject air
        Real64 emiTotHeat = 0.0;          // total building heat emission
    };

    struct ZoneLocalEnvironmentData
    {
        // Members
        std::string Name;
        int ZonePtr = 0;           // surface pointer
        int OutdoorAirNodePtr = 0; // schedule pointer
    };

    struct ZoneReportVars // Zone and Space report variables
    {
        // Members
        // People
        Real64 PeopleRadGain = 0.0;
        Real64 PeopleConGain = 0.0;
        Real64 PeopleSenGain = 0.0;
        Real64 PeopleNumOcc = 0.0;
        Real64 PeopleLatGain = 0.0;
        Real64 PeopleTotGain = 0.0;
        Real64 PeopleRadGainRate = 0.0;
        Real64 PeopleConGainRate = 0.0;
        Real64 PeopleSenGainRate = 0.0;
        Real64 PeopleLatGainRate = 0.0;
        Real64 PeopleTotGainRate = 0.0;
        // Lights
        Real64 LtsPower = 0.0;
        Real64 LtsElecConsump = 0.0;
        Real64 LtsRadGain = 0.0;
        Real64 LtsVisGain = 0.0;
        Real64 LtsConGain = 0.0;
        Real64 LtsRetAirGain = 0.0;
        Real64 LtsTotGain = 0.0;
        Real64 LtsRadGainRate = 0.0;
        Real64 LtsVisGainRate = 0.0;
        Real64 LtsConGainRate = 0.0;
        Real64 LtsRetAirGainRate = 0.0;
        Real64 LtsTotGainRate = 0.0;
        // Baseboard Heat
        Real64 BaseHeatPower = 0.0;
        Real64 BaseHeatElecCons = 0.0;
        Real64 BaseHeatRadGain = 0.0;
        Real64 BaseHeatConGain = 0.0;
        Real64 BaseHeatTotGain = 0.0;
        Real64 BaseHeatRadGainRate = 0.0;
        Real64 BaseHeatConGainRate = 0.0;
        Real64 BaseHeatTotGainRate = 0.0;
        // Electric Equipment
        Real64 ElecPower = 0.0;
        Real64 ElecConsump = 0.0;
        Real64 ElecRadGain = 0.0;
        Real64 ElecConGain = 0.0;
        Real64 ElecLatGain = 0.0;
        Real64 ElecLost = 0.0;
        Real64 ElecTotGain = 0.0;
        Real64 ElecRadGainRate = 0.0;
        Real64 ElecConGainRate = 0.0;
        Real64 ElecLatGainRate = 0.0;
        Real64 ElecLostRate = 0.0;
        Real64 ElecTotGainRate = 0.0;
        // Gas Equipment
        Real64 GasPower = 0.0;
        Real64 GasConsump = 0.0;
        Real64 GasRadGain = 0.0;
        Real64 GasConGain = 0.0;
        Real64 GasLatGain = 0.0;
        Real64 GasLost = 0.0;
        Real64 GasTotGain = 0.0;
        Real64 GasRadGainRate = 0.0;
        Real64 GasConGainRate = 0.0;
        Real64 GasLatGainRate = 0.0;
        Real64 GasLostRate = 0.0;
        Real64 GasTotGainRate = 0.0;
        // Hot Water Equipment
        Real64 HWPower = 0.0;
        Real64 HWConsump = 0.0;
        Real64 HWRadGain = 0.0;
        Real64 HWConGain = 0.0;
        Real64 HWLatGain = 0.0;
        Real64 HWLost = 0.0;
        Real64 HWTotGain = 0.0;
        Real64 HWRadGainRate = 0.0;
        Real64 HWConGainRate = 0.0;
        Real64 HWLatGainRate = 0.0;
        Real64 HWLostRate = 0.0;
        Real64 HWTotGainRate = 0.0;
        // Steam Equipment
        Real64 SteamPower = 0.0;
        Real64 SteamConsump = 0.0;
        Real64 SteamRadGain = 0.0;
        Real64 SteamConGain = 0.0;
        Real64 SteamLatGain = 0.0;
        Real64 SteamLost = 0.0;
        Real64 SteamTotGain = 0.0;
        Real64 SteamRadGainRate = 0.0;
        Real64 SteamConGainRate = 0.0;
        Real64 SteamLatGainRate = 0.0;
        Real64 SteamLostRate = 0.0;
        Real64 SteamTotGainRate = 0.0;
        // Other Equipment
        Real64 OtherPower = 0.0;
        Real64 OtherConsump = 0.0;
        Real64 OtherRadGain = 0.0;
        Real64 OtherConGain = 0.0;
        Real64 OtherLatGain = 0.0;
        Real64 OtherLost = 0.0;
        Real64 OtherTotGain = 0.0;
        Real64 OtherRadGainRate = 0.0;
        Real64 OtherConGainRate = 0.0;
        Real64 OtherLatGainRate = 0.0;
        Real64 OtherLostRate = 0.0;
        Real64 OtherTotGainRate = 0.0;
        // IT Equipment
        Real64 ITEqCPUPower = 0.0;          // Zone ITE CPU Electric Power [W]
        Real64 ITEqFanPower = 0.0;          // Zone ITE Fan Electric Power [W]
        Real64 ITEqUPSPower = 0.0;          // Zone ITE UPS Electric Power [W]
        Real64 ITEqCPUPowerAtDesign = 0.0;  // Zone ITE CPU Electric Power at Design Inlet Conditions [W]
        Real64 ITEqFanPowerAtDesign = 0.0;  // Zone ITE Fan Electric Power at Design Inlet Conditions [W]
        Real64 ITEqUPSGainRateToZone = 0.0; // Zone ITE UPS Heat Gain to Zone Rate [W] - convective gain
        Real64 ITEqConGainRateToZone = 0.0; // Zone ITE Total Heat Gain toZone Rate [W] - convective gain - includes heat gain from UPS, plus CPU and
                                            // Fans if room air model not used
        Real64 ITEqCPUConsumption = 0.0;    // Zone ITE CPU Electric Energy [J]
        Real64 ITEqFanConsumption = 0.0;    // Zone ITE Fan Electric Energy [J]
        Real64 ITEqUPSConsumption = 0.0;    // Zone ITE UPS Electric Energy [J]
        Real64 ITEqCPUEnergyAtDesign = 0.0; // Zone ITE CPU Electric Energy at Design Inlet Conditions [J]
        Real64 ITEqFanEnergyAtDesign = 0.0; // Zone ITE Fan Electric Energy at Design Inlet Conditions [J]
        Real64 ITEqUPSGainEnergyToZone = 0.0;  // Zone ITE UPS Heat Gain to Zone Energy [J] - convective gain
        Real64 ITEqConGainEnergyToZone = 0.0;  // Zone ITE Total Heat Gain toZone Energy [J] - convective gain - includes heat gain from UPS, plus CPU
                                               // and Fans if room air model not used
        Real64 ITEqAirVolFlowStdDensity = 0.0; // Zone Air volume flow rate at standard density [m3/s]
        Real64 ITEqAirMassFlow = 0.0;          // Zone Air mass flow rate [kg/s]
        Real64 ITEqSHI = 0.0;                  // Zone Supply Heat Index []
        Real64 ITEqTimeOutOfOperRange = 0.0;   // Zone ITE Air Inlet Operating Range Exceeded Time [hr]
        Real64 ITEqTimeAboveDryBulbT = 0.0;    // Zone ITE Air Inlet Dry-Bulb Temperature Above Operating Range Time [hr]
        Real64 ITEqTimeBelowDryBulbT = 0.0;    // Zone ITE Air Inlet Dry-Bulb Temperature Below Operating Range Time [hr]
        Real64 ITEqTimeAboveDewpointT = 0.0;   // Zone ITE Air Inlet Dewpoint Temperature Above Operating Range Time [hr]
        Real64 ITEqTimeBelowDewpointT = 0.0;   // Zone ITE Air Inlet Dewpoint Temperature Below Operating Range Time [hr]
        Real64 ITEqTimeAboveRH = 0.0;          // Zone ITE Air Inlet Relative Humidity Above Operating Range Time [hr]
        Real64 ITEqTimeBelowRH = 0.0;          // Zone ITE Air Inlet Relative Humidity Below Operating Range Time [hr]
        Real64 ITEAdjReturnTemp = 0.0;         // Zone ITE Adjusted Return Air Temperature
        // Overall Zone Variables
        Real64 TotRadiantGain = 0.0;
        Real64 TotVisHeatGain = 0.0;
        Real64 TotConvectiveGain = 0.0;
        Real64 TotLatentGain = 0.0;
        Real64 TotTotalHeatGain = 0.0;
        Real64 TotRadiantGainRate = 0.0;
        Real64 TotVisHeatGainRate = 0.0;
        Real64 TotConvectiveGainRate = 0.0;
        Real64 TotLatentGainRate = 0.0;
        Real64 TotTotalHeatGainRate = 0.0;
        // Contaminant
        Real64 CO2Rate = 0.0;
        Real64 GCRate = 0.0;

        Real64 SumTinMinusTSup = 0.0;  // Numerator for zone-level sensible heat index (SHI)
        Real64 SumToutMinusTSup = 0.0; // Denominator for zone-level sensible heat index (SHI)
    };

    // Functions

    void SetZoneOutBulbTempAt(EnergyPlusData &state);

    void CheckZoneOutBulbTempAt(EnergyPlusData &state);

    void SetZoneWindSpeedAt(EnergyPlusData &state);

    void SetZoneWindDirAt(EnergyPlusData &state);

    void CheckAndSetConstructionProperties(EnergyPlusData &state,
                                           int ConstrNum,    // Construction number to be set/checked
                                           bool &ErrorsFound // error flag that is set when certain errors have occurred
    );

    int AssignReverseConstructionNumber(EnergyPlusData &state,
                                        int ConstrNum, // Existing Construction number of first surface
                                        bool &ErrorsFound);

    void AddVariableSlatBlind(EnergyPlusData &state,
                              int inBlindNumber,   // current Blind Number/pointer to name
                              int &outBlindNumber, // resultant Blind Number to pass back
                              bool &errFlag        // error flag should one be needed
    );

    void CalcScreenTransmittance(EnergyPlusData &state,
                                 int SurfaceNum,
                                 Optional<Real64 const> Phi = _,     // Optional sun altitude relative to surface outward normal (radians)
                                 Optional<Real64 const> Theta = _,   // Optional sun azimuth relative to surface outward normal (radians)
                                 Optional_int_const ScreenNumber = _ // Optional screen number
    );

    std::string DisplayMaterialRoughness(DataSurfaces::SurfaceRoughness Roughness); // Roughness String

    Real64 ComputeNominalUwithConvCoeffs(EnergyPlusData &state,
                                         int numSurf,  // index for Surface array.
                                         bool &isValid // returns true if result is valid
    );

    void SetFlagForWindowConstructionWithShadeOrBlindLayer(EnergyPlusData &state);

} // namespace DataHeatBalance

struct HeatBalanceData : BaseGlobalStruct
{

    int MaxSolidWinLayers = 0; // Maximum number of solid layers in a window construction

    // SiteData aka building data
    Real64 LowHConvLimit = 0.1; // Lowest allowed convection coefficient for detailed model
    // before reverting to the simple model.  This avoids a
    // divide by zero elsewhere.  Not based on any physical
    // reasoning, just the number that was picked.  It corresponds
    // to a delta T for a vertical surface of 0.000444C.
    //                           ! A lower limit is needed to avoid numerical problems
    //                           ! Natural convection correlations are a function of temperature difference,
    //                           !   there are many times when those temp differences pass through zero leading to non-physical results
    //                           ! Value of 1.0 chosen here is somewhat arbitrary, but based on the following reasons:
    //                           !  1) Low values of HconvIn indicate a layer of high thermal resistance, however
    //                           !       the R-value of a convection film layer should be relatively low (compared to building surfaces)
    //                           !  2) The value of 1.0 corresponds to the thermal resistance of 0.05 m of batt insulation
    //                           !  3) Limit on the order of 1.0 is suggested by the abrupt changes in an inverse relationship
    //                           !  4) A conduction-only analysis can model a limit by considering the thermal performance of
    //                           !       boundary layer to be pure conduction (with no movement to enhance heat transfer);
    //                           !       Taking the still gas thermal conductivity for air at 0.0267 W/m-K (at 300K), then
    //                           !       this limit of 1.0 corresponds to a completely still layer of air that is around 0.025 m thick
    //                           !  5) The previous limit of 0.1 (before ver. 3.1) caused loads initialization problems in test files
    Real64 HighHConvLimit = 1000.0;         // upper limit for HConv, mostly used for user input limits in practice. !W/m2-K
    Real64 MaxAllowedDelTemp = 0.002;       // Convergence criteria for inside surface temperatures
    Real64 MaxAllowedDelTempCondFD = 0.002; // Convergence criteria for inside surface temperatures for CondFD
    std::string BuildingName;               // Name of building
    Real64 BuildingAzimuth = 0.0;           // North Axis of Building
    Real64 LoadsConvergTol = 0.0;           // Tolerance value for Loads Convergence
    Real64 TempConvergTol = 0.0;            // Tolerance value for Temperature Convergence
    int DefaultInsideConvectionAlgo = ConvectionConstants::HcInt_ASHRAESimple;
    int DefaultOutsideConvectionAlgo = ConvectionConstants::HcExt_ASHRAESimple;
    DataHeatBalance::Shadowing SolarDistribution = DataHeatBalance::Shadowing::FullExterior;                // Solar Distribution Algorithm
    int InsideSurfIterations = 0;                                                                           // Counts inside surface iterations
    DataSurfaces::HeatTransferModel OverallHeatTransferSolutionAlgo = DataSurfaces::HeatTransferModel::CTF; // Global HeatBalanceAlgorithm setting
    // Flags for HeatTransfer Algorithms Used
    bool AllCTF = true;                  // CTF used for everything - no EMPD, no CondFD, No HAMT, No Kiva - true until flipped otherwise
    bool AnyCTF = false;                 // CTF used
    bool AnyEMPD = false;                // EMPD used
    bool AnyCondFD = false;              // CondFD used
    bool AnyHAMT = false;                // HAMT used
    bool AnyKiva = false;                // Kiva used
    bool AnyAirBoundary = false;         // Construction:AirBoundary used (implies grouped solar and radiant is present)
    bool AnyBSDF = false;                // True if any WindowModelType == WindowBSDFModel
    int MaxNumberOfWarmupDays = 25;      // Maximum number of warmup days allowed
    int MinNumberOfWarmupDays = 1;       // Minimum number of warmup days allowed
    Real64 CondFDRelaxFactor = 1.0;      // Relaxation factor, for looping across all the surfaces.
    Real64 CondFDRelaxFactorInput = 1.0; // Relaxation factor, for looping across all the surfaces, user input value
    DataHeatBalance::SolutionAlgo ZoneAirSolutionAlgo =
        DataHeatBalance::SolutionAlgo::ThirdOrder; // ThirdOrderBackwardDifference, AnalyticalSolution, and EulerMethod
    bool OverrideZoneAirSolutionAlgo = false;      // Override the zone air solution algorithm in PerformancePrecisionTradeoffs
    Real64 BuildingRotationAppendixG = 0.0;        // Building Rotation for Appendix G
    Real64 ZoneTotalExfiltrationHeatLoss = 0.0;    // Building total heat emission through zone exfiltration;
    Real64 ZoneTotalExhaustHeatLoss = 0.0;         // Building total heat emission through zone air exhaust;
    Real64 SysTotalHVACReliefHeatLoss = 0.0;       // Building total heat emission through HVAC system relief air;
    Real64 SysTotalHVACRejectHeatLoss = 0.0;       // Building total heat emission through HVAC system heat rejection;
    // END SiteData
    int NumOfZoneLists = 0;             // Total number of zone lists
    int NumOfZoneGroups = 0;            // Total number of zone groups
    int NumPeopleStatements = 0;        // Number of People objects in input - possibly global assignments
    int NumLightsStatements = 0;        // Number of Lights objects in input - possibly global assignments
    int NumZoneElectricStatements = 0;  // Number of ZoneElectric objects in input - possibly global assignments
    int NumZoneGasStatements = 0;       // Number of ZoneGas objects in input - possibly global assignments
    int NumInfiltrationStatements = 0;  // Number of Design Flow Infiltration objects in input - possibly global assignments
    int NumVentilationStatements = 0;   // Number of Design Flow Ventilation objects in input - possibly global assignments
    int NumHotWaterEqStatements = 0;    // number of Hot Water Equipment objects in input. - possibly global assignments
    int NumSteamEqStatements = 0;       // number of Steam Equipment objects in input. - possibly global assignments
    int NumOtherEqStatements = 0;       // number of Other Equipment objects in input. - possibly global assignments
    int NumZoneITEqStatements = 0;      // number of Other Equipment objects in input. - possibly global assignments
    int NumZoneBBHeatStatements = 0;    // number of ZoneBaseboard heat objects in input. - possibly global assignments
    int TotPeople = 0;                  // Total People instances after expansion to spaces
    int TotLights = 0;                  // Total Lights instances after expansion to spaces
    int TotElecEquip = 0;               // Total Electric Equipment instances after expansion to spaces
    int TotGasEquip = 0;                // Total Gas Equipment instances after expansion to spaces
    int TotOthEquip = 0;                // Total Other Equipment instances after expansion to spaces
    int TotHWEquip = 0;                 // Total Hot Water Equipment instances after expansion to spaces
    int TotStmEquip = 0;                // Total Steam Equipment instances after expansion to spaces
    int TotITEquip = 0;                 // Total IT Equipment instances after expansion to spaces
    int TotInfiltration = 0;            // Total Infiltration Statements in input and extrapolated from global assignments
    int TotDesignFlowInfiltration = 0;  // number of Design Flow rate ZoneInfiltration in input
    int TotShermGrimsInfiltration = 0;  // number of Sherman Grimsrud (ZoneInfiltration:ResidentialBasic) in input
    int TotAIM2Infiltration = 0;        // number of AIM2 (ZoneInfiltration:ResidentialEnhanced) in input
    int TotVentilation = 0;             // Total Ventilation Statements in input
    int TotDesignFlowVentilation = 0;   // number of Design Flow rate ZoneVentilation in input
    int TotWindAndStackVentilation = 0; // number of wind and stack open area ZoneVentilation in input
    int TotMixing = 0;                  // Total Mixing Statements in input
    int TotCrossMixing = 0;             // Total Cross Mixing Statements in input
    int TotRefDoorMixing = 0;           // Total RefrigerationDoor Mixing Statements in input
    int TotBBHeat = 0;                  // Total BBHeat Statements instances after expansion to spaces
    int TotMaterials = 0;               // Total number of unique materials (layers) in this simulation
    int TotConstructs = 0;              // Total number of unique constructions in this simulation
    int TotSpectralData = 0;            // Total window glass spectral data sets
    int W5GlsMat = 0;                   // Window5 Glass Materials, specified by transmittance and front and back reflectance
    int W5GlsMatAlt = 0;                // Window5 Glass Materials, specified by index of refraction and extinction coeff
    int W5GasMat = 0;                   // Window5 Single-Gas Materials
    int W5GasMatMixture = 0;            // Window5 Gas Mixtures
    int W7SupportPillars = 0;           // Complex fenestration support pillars
    int W7DeflectionStates = 0;         // Complex fenestration deflection states
    int W7MaterialGaps = 0;             // Complex fenestration material gaps
    int TotBlinds = 0;                  // Total number of blind materials
    int TotScreens = 0;                 // Total number of exterior window screen materials
    int TotTCGlazings = 0;              // Number of TC glazing object - WindowMaterial:Glazing:Thermochromic found in the idf file
    int NumSurfaceScreens = 0;          // Total number of screens on exterior windows
    int TotShades = 0;                  // Total number of shade materials
    int TotComplexShades = 0;           // Total number of shading materials for complex fenestrations
    int TotComplexGaps = 0;             // Total number of window gaps for complex fenestrations
    int TotSimpleWindow = 0;            // number of simple window systems.
    int W5GlsMatEQL = 0;                // Window5 Single-Gas Materials for Equivalent Layer window model
    int TotShadesEQL = 0;               // Total number of shade materials for Equivalent Layer window model
    int TotDrapesEQL = 0;               // Total number of drape materials for Equivalent Layer window model
    int TotBlindsEQL = 0;               // Total number of blind materials for Equivalent Layer window model
    int TotScreensEQL = 0;              // Total number of exterior window screen materials for Equivalent Layer window model
    int W5GapMatEQL = 0;                // Window5 Equivalent Layer Single-Gas Materials
    int TotZoneAirBalance = 0;          // Total Zone Air Balance Statements in input
    int TotFrameDivider = 0;            // Total number of window frame/divider objects
    bool AirFlowFlag = false;
    int TotCO2Gen = 0;                       // Total CO2 source and sink statements in input
    bool CalcWindowRevealReflection = false; // True if window reveal reflection is to be calculated for at least one exterior window
    bool StormWinChangeThisDay = false; // True if a storm window has been added or removed from any window during the current day; can only be true
                                        // for first time step of the day.
    bool SimpleCTFOnly = true;          // true if all constructions are simple CTF construction (CTFTimestep = TimeStepZone) with no internal sources
    int MaxCTFTerms = 0;                // Maximum CTF terms to shift for all surfaces in the simulation
    bool AnyInternalHeatSourceInInput = false;        // true if the user has entered any constructions with internal sources
    bool AdaptiveComfortRequested_CEN15251 = false;   // true if people objects have adaptive comfort requests. CEN15251
    bool AdaptiveComfortRequested_ASH55 = false;      // true if people objects have adaptive comfort requests. ASH55
    bool AnyThermalComfortPierceModel = false;        // true if people objects use pierce thermal comfort model
    bool AnyThermalComfortKSUModel = false;           // true if people objects use KSU thermal comfort model
    bool AnyThermalComfortCoolingEffectModel = false; // true if people objects use ASH55 cooling effect adjusted thermal comfort model
    bool AnyThermalComfortAnkleDraftModel = false;    // true if people objects use ASH55 ankle draft thermal comfort model

    bool NoFfactorConstructionsUsed = true;
    bool NoCfactorConstructionsUsed = true;
    bool NoRegularMaterialsUsed = true;

    Array1D<Real64> ZoneSNLoadHeatEnergy;
    Array1D<Real64> ZoneSNLoadCoolEnergy;
    Array1D<Real64> ZoneSNLoadHeatRate;
    Array1D<Real64> ZoneSNLoadCoolRate;
    Array1D<Real64> ZoneSNLoadPredictedRate;
    Array1D<Real64> ZoneSNLoadPredictedHSPRate; // Predicted load to heating setpoint (unmultiplied)
    Array1D<Real64> ZoneSNLoadPredictedCSPRate; // Predicted load to cooling setpoint (unmultiplied)
    Array1D<Real64> ZoneMoisturePredictedRate;
    Array1D<Real64> ZoneMoisturePredictedHumSPRate;   // Predicted latent load to humidification setpoint (unmultiplied)
    Array1D<Real64> ZoneMoisturePredictedDehumSPRate; // Predicted latent load to dehumidification setpoint (unmultiplied)
    Array1D<Real64> ZoneListSNLoadHeatEnergy;
    Array1D<Real64> ZoneListSNLoadCoolEnergy;
    Array1D<Real64> ZoneListSNLoadHeatRate;
    Array1D<Real64> ZoneListSNLoadCoolRate;
    Array1D<Real64> ZoneGroupSNLoadHeatEnergy;
    Array1D<Real64> ZoneGroupSNLoadCoolEnergy;
    Array1D<Real64> ZoneGroupSNLoadHeatRate;
    Array1D<Real64> ZoneGroupSNLoadCoolRate;

    Array1D<Real64> ZoneMRT;        // MEAN RADIANT TEMPERATURE (C)
    Array1D<Real64> ZoneTransSolar; // Exterior beam plus diffuse solar entering zone sum of WinTransSolar for exterior windows in zone (W)
    Array1D<Real64>
        ZoneWinHeatGain; // Heat gain to zone from all exterior windows (includes oneTransSolar); sum of WinHeatGain for exterior windows in zone (W)
    Array1D<Real64> ZoneWinHeatGainRep;             // = ZoneWinHeatGain when ZoneWinHeatGain >= 0
    Array1D<Real64> ZoneWinHeatLossRep;             // = -ZoneWinHeatGain when ZoneWinHeatGain < 0
    Array1D<Real64> ZoneBmSolFrExtWinsRep;          // Beam solar into zone from exterior windows [W]
    Array1D<Real64> ZoneBmSolFrIntWinsRep;          // Beam solar into zone from interior windows [W]
    Array1D<Real64> EnclSolInitialDifSolReflW;      // Initial diffuse solar in zone from ext and int windows reflected from interior surfaces [W]
    Array1D<Real64> ZoneDifSolFrExtWinsRep;         // Diffuse solar into zone from exterior windows [W]
    Array1D<Real64> ZoneDifSolFrIntWinsRep;         // Diffuse solar into zone from interior windows [W]
    Array1D<Real64> ZoneOpaqSurfInsFaceCond;        // Zone inside face opaque surface conduction (W)
    Array1D<Real64> ZoneOpaqSurfInsFaceCondGainRep; // = Zone inside face opaque surface conduction when >= 0
    Array1D<Real64> ZoneOpaqSurfInsFaceCondLossRep; // = -Zone inside face opaque surface conduction when < 0
    Array1D<Real64> ZoneOpaqSurfExtFaceCond;        // Zone outside face opaque surface conduction (W)
    Array1D<Real64> ZoneOpaqSurfExtFaceCondGainRep; // = Zone outside face opaque surface conduction when >= 0
    Array1D<Real64> ZoneOpaqSurfExtFaceCondLossRep; // = -Zone outside face opaque surface conduction when < 0
    Array1D<Real64> ZoneTransSolarEnergy;           // Energy of ZoneTransSolar [J]
    Array1D<Real64> ZoneWinHeatGainRepEnergy;       // Energy of ZoneWinHeatGainRep [J]
    Array1D<Real64> ZoneWinHeatLossRepEnergy;       // Energy of ZoneWinHeatLossRep [J]
    Array1D<Real64> ZoneBmSolFrExtWinsRepEnergy;    // Energy of ZoneBmSolFrExtWinsRep [J]
    Array1D<Real64> ZoneBmSolFrIntWinsRepEnergy;    // Energy of ZoneBmSolFrIntWinsRep [J]
    Array1D<Real64> ZoneDifSolFrExtWinsRepEnergy;   // Energy of ZoneDifSolFrExtWinsRep [J]
    Array1D<Real64> ZoneDifSolFrIntWinsRepEnergy;   // Energy of ZoneDifSolFrIntWinsRep [J]
    Array1D<Real64> ZnOpqSurfInsFaceCondGnRepEnrg;  // Energy of ZoneOpaqSurfInsFaceCondGainRep [J]
    Array1D<Real64> ZnOpqSurfInsFaceCondLsRepEnrg;  // Energy of ZoneOpaqSurfInsFaceCondLossRep [J]
    Array1D<Real64> ZnOpqSurfExtFaceCondGnRepEnrg;  // Energy of ZoneOpaqSurfInsFaceCondGainRep [J]
    Array1D<Real64> ZnOpqSurfExtFaceCondLsRepEnrg;  // Energy of ZoneOpaqSurfInsFaceCondLossRep [J]

    Array1D<Real64> SurfQdotRadIntGainsInPerArea;       // Thermal radiation absorbed on inside surfaces
    Array1D<Real64> SurfQRadSWOutIncident;              // Exterior beam plus diffuse solar incident on surface (W/m2)
    Array1D<Real64> SurfQRadSWOutIncidentBeam;          // Exterior beam solar incident on surface (W/m2)
    Array1D<Real64> SurfBmIncInsSurfIntensRep;          // Beam sol irrad from ext wins on inside of surface (W/m2)
    Array1D<Real64> SurfBmIncInsSurfAmountRep;          // Beam sol amount from ext wins incident on inside of surface (W)
    Array1D<Real64> SurfIntBmIncInsSurfIntensRep;       // Beam sol irrad from int wins on inside of surface (W/m2)
    Array1D<Real64> SurfIntBmIncInsSurfAmountRep;       // Beam sol amount from int wins incident on inside of surface (W)
    Array1D<Real64> SurfQRadSWOutIncidentSkyDiffuse;    // Exterior sky diffuse solar incident on surface (W/m2)
    Array1D<Real64> SurfQRadSWOutIncidentGndDiffuse;    // Exterior ground diffuse solar incident on surface (W/m2)
    Array1D<Real64> SurfQRadSWOutIncBmToDiffReflGnd;    // Exterior diffuse solar incident from beam to diffuse reflection from ground (W/m2)
    Array1D<Real64> SurfQRadSWOutIncSkyDiffReflGnd;     // Exterior diffuse solar incident from sky diffuse reflection from ground (W/m2)
    Array1D<Real64> SurfQRadSWOutIncBmToBmReflObs;      // Exterior beam solar incident from beam-to-beam reflection from obstructions (W/m2)
    Array1D<Real64> SurfQRadSWOutIncBmToDiffReflObs;    // Exterior diffuse solar incident from beam-to-diffuse reflection from obstructions (W/m2)
    Array1D<Real64> SurfQRadSWOutIncSkyDiffReflObs;     // Exterior diffuse solar incident from sky diffuse reflection from obstructions (W/m2)
    Array1D<Real64> SurfSWInAbsTotalReport;             // Report - Total interior/exterior shortwave absorbed on inside of surface (W)
    Array1D<Real64> SurfBmIncInsSurfAmountRepEnergy;    // energy of BmIncInsSurfAmountRep [J]
    Array1D<Real64> SurfIntBmIncInsSurfAmountRepEnergy; // energy of IntBmIncInsSurfAmountRep [J]
    Array1D<Real64> SurfInitialDifSolInAbsReport;       // Report - Initial transmitted diffuse solar absorbed on inside of surface (W)
    Array1D_int SurfWinBSDFBeamDirectionRep;            // BSDF beam direction number for given complex fenestration state (for reporting) []
    Array1D<Real64> SurfWinBSDFBeamThetaRep;            // BSDF beam Theta angle (for reporting) [rad]
    Array1D<Real64> SurfWinBSDFBeamPhiRep;              // BSDF beam Phi angle (for reporting) [rad]
    Array1D<Real64> SurfWinQRadSWwinAbsTot;             // Exterior beam plus diffuse solar absorbed in glass layers of window (W)
    Array2D<Real64> SurfWinQRadSWwinAbsLayer;           // Exterior beam plus diffuse solar absorbed in glass layers of window (W)
    Array2D<Real64> SurfWinFenLaySurfTempFront;         // Front surface temperatures of fenestration layers
    Array2D<Real64> SurfWinFenLaySurfTempBack;          // Back surface temperatures of fenestration layers
    Array1D<Real64> SurfWinQRadSWwinAbsTotEnergy;       // Energy of QRadSWwinAbsTot [J]
    Array1D<Real64> SurfWinSWwinAbsTotalReport;         // Report - Total interior/exterior shortwave absorbed in all glass layers of window (W)
    Array1D<Real64> SurfWinInitialDifSolInTransReport;  // Report - Initial transmitted diffuse solar transmitted out
                                                        // through inside of window surface (W)
    Array2D<Real64> SurfWinQRadSWwinAbs;                // Short wave radiation absorbed in window glass layers
    Array2D<Real64> SurfWinInitialDifSolwinAbs;         // Initial diffuse solar absorbed in window glass layers from inside(W/m2)
    Array1D<Real64> SurfOpaqSWOutAbsTotalReport;        // Report - Total exterior shortwave/solar absorbed on outside of surface (W)
    Array1D<Real64> SurfOpaqSWOutAbsEnergyReport;       // Report - Total exterior shortwave/solar absorbed on outside of surface (j)
    Array1D<Real64> SurfTempEffBulkAir;                 // air temperature adjacent to the surface used for inside surface heat balances

    // Material
    Array1D<Real64> NominalR;                       // Nominal R value of each material -- used in matching interzone surfaces
    Array1D<Real64> NominalRforNominalUCalculation; // Nominal R values are summed to calculate NominalU values for constructions
    Array1D<Real64> NominalU;                       // Nominal U value for each construction -- used in matching interzone surfaces
    Array1D<Real64> NominalUBeforeAdjusted;         // Nominal U value for glazing system only
    Array1D<Real64> CoeffAdjRatio;                  // Conductive coefficient adjustment ratio

    Array1D<Real64>
        EnclSolQSWRad; // Zone short-wave flux density; used to calculate short-wave  radiation absorbed on inside surfaces of zone or enclosure
    Array1D<Real64> EnclSolQSWRadLights; // Like QS, but Lights short-wave only.
    Array1D<Real64> EnclSolDB;           // Factor for diffuse radiation in a zone from beam reflecting from inside surfaces
    Array1D<Real64> EnclSolDBSSG;        // Factor for diffuse radiation in a zone from beam reflecting from inside surfaces.
    // Used only for scheduled surface gains
    Array1D<Real64> EnclSolDBIntWin; // Value of factor for beam solar entering a zone through interior windows
    // (considered to contribute to diffuse in zone)
    Array1D<Real64> EnclSolQSDifSol; // Like QS, but diffuse solar short-wave only.
    Array1D<Real64> EnclSolQD;       // Diffuse solar radiation in a zone from sky and ground diffuse entering
    // through exterior windows and reflecting from interior surfaces,
    // beam from exterior windows reflecting from interior surfaces,
    // and beam entering through interior windows (considered diffuse)
    Array1D<Real64> EnclSolQDforDaylight; // Diffuse solar radiation in a zone from sky and ground diffuse entering
    // through exterior windows, beam from exterior windows reflecting
    // from interior surfaces, and beam entering through interior windows
    // (considered diffuse)
    // Originally QD, now used only for EnclSolQSDifSol calc for daylighting

    Array1D<Real64> EnclSolVMULT;        // 1/(Sum Of A Zone's Inside Surfaces Area*Absorptance)
    Array1D<Real64> EnclRadQThermalRad;  // TOTAL THERMAL RADIATION ADDED TO ZONE or Radiant Enclosure (group of zones)
    Array1D<Real64> EnclRadThermAbsMult; // EnclRadThermAbsMult  - MULTIPLIER TO COMPUTE 'ITABSF'
    Array1D<bool> EnclSolAbsFirstCalc;   // for error message
    Array1D<bool> EnclRadReCalc;         // Enclosure solar or thermal radiation properties needs to be recalc due to window/shading status change

    bool EnclRadAlwaysReCalc = false; // Enclosure solar or thermal radiation properties always needs to be recalc at any time step

    Array1D<Real64> SurfCosIncidenceAngle;       // Cosine of beam solar incidence angle (for reporting)
    Array2D<Real64> SurfSunlitFracHR;            // Hourly fraction of heat transfer surface that is sunlit
    Array2D<Real64> SurfCosIncAngHR;             // Hourly cosine of beam radiation incidence angle on surface
    Array3D<Real64> SurfSunlitFrac;              // TimeStep fraction of heat transfer surface that is sunlit
    Array3D<Real64> SurfSunlitFracWithoutReveal; // For a window with reveal, the sunlit fraction  without shadowing by the reveal
    Array3D<Real64> SurfCosIncAng;               // TimeStep cosine of beam radiation incidence angle on surface
    Array4D_int SurfWinBackSurfaces;     // For a given hour and timestep, a list of up to 20 surfaces receiving  beam solar radiation from a given
                                         // exterior window
    Array4D<Real64> SurfWinOverlapAreas; // For a given hour and timestep, the areas of the exterior window sending beam solar radiation to the
                                         // surfaces listed in BackSurfaces
    Real64 zeroPointerVal = 0.0;
    int NumAirBoundaryMixing = 0;             // Number of air boundary simple mixing objects needed
    std::vector<int> AirBoundaryMixingZone1;  // Air boundary simple mixing zone 1
    std::vector<int> AirBoundaryMixingZone2;  // Air boundary simple mixing zone 2
    std::vector<int> AirBoundaryMixingSched;  // Air boundary simple mixing schedule index
    std::vector<Real64> AirBoundaryMixingVol; // Air boundary simple mixing volume flow rate [m3/s]
    EPVector<DataHeatBalance::ZonePreDefRepType> ZonePreDefRep;
    DataHeatBalance::ZonePreDefRepType BuildingPreDefRep;
    EPVector<DataHeatBalance::SpaceZoneSimData> ZoneIntGain;
    EPVector<DataHeatBalance::SpaceZoneSimData> spaceIntGain;
    EPVector<DataHeatBalance::SpaceIntGainDeviceData> spaceIntGainDevices;
    EPVector<DataHeatBalance::GapSupportPillar> SupportPillar;
    EPVector<DataHeatBalance::GapDeflectionState> DeflectionState;
    EPVector<DataHeatBalance::SpectralDataProperties> SpectralData;
    EPVector<DataHeatBalance::SpaceData> space;
    EPVector<DataHeatBalance::SpaceListData> spaceList;
    EPVector<DataHeatBalance::ZoneData> Zone;
    EPVector<DataHeatBalance::ZoneListData> ZoneList;
    EPVector<DataHeatBalance::ZoneGroupData> ZoneGroup;
    EPVector<DataHeatBalance::PeopleData> People;
    EPVector<DataHeatBalance::LightsData> Lights;
    EPVector<DataHeatBalance::ZoneEquipData> ZoneElectric;
    EPVector<DataHeatBalance::ZoneEquipData> ZoneGas;
    EPVector<DataHeatBalance::ZoneEquipData> ZoneOtherEq;
    EPVector<DataHeatBalance::ZoneEquipData> ZoneHWEq;
    EPVector<DataHeatBalance::ZoneEquipData> ZoneSteamEq;
    EPVector<DataHeatBalance::ITEquipData> ZoneITEq;
    EPVector<DataHeatBalance::BBHeatData> ZoneBBHeat;
    EPVector<DataHeatBalance::InfiltrationData> Infiltration;
    EPVector<DataHeatBalance::VentilationData> Ventilation;
    EPVector<DataHeatBalance::ZoneAirBalanceData> ZoneAirBalance;
    EPVector<DataHeatBalance::MixingData> Mixing;
    EPVector<DataHeatBalance::MixingData> CrossMixing;
    EPVector<DataHeatBalance::MixingData> RefDoorMixing;
    Array1D<DataHeatBalance::WindowBlindProperties> Blind;
    EPVector<DataHeatBalance::WindowComplexShade> ComplexShade;
    EPVector<DataHeatBalance::WindowThermalModelParams> WindowThermalModel;
    EPVector<DataHeatBalance::SurfaceScreenProperties> SurfaceScreens;
    EPVector<DataHeatBalance::ScreenTransData> ScreenTrans;
    EPVector<DataHeatBalance::ZoneCatEUseData> ZoneIntEEuse;
    EPVector<DataHeatBalance::RefrigCaseCreditData> RefrigCaseCredit;
    EPVector<DataHeatBalance::HeatReclaimDataBase> HeatReclaimRefrigeratedRack;
    EPVector<DataHeatBalance::HeatReclaimRefrigCondenserData> HeatReclaimRefrigCondenser;
    EPVector<DataHeatBalance::HeatReclaimDataBase> HeatReclaimDXCoil;
    EPVector<DataHeatBalance::HeatReclaimDataBase> HeatReclaimVS_DXCoil;
    EPVector<DataHeatBalance::HeatReclaimDataBase> HeatReclaimSimple_WAHPCoil;
    EPVector<DataHeatBalance::AirReportVars> ZnAirRpt;
    EPVector<DataHeatBalance::TCGlazingsType> TCGlazings;
    EPVector<DataHeatBalance::ZoneEquipData> ZoneCO2Gen;
    EPVector<DataHeatBalance::GlobalInternalGainMiscObject> PeopleObjects;
    EPVector<DataHeatBalance::GlobalInternalGainMiscObject> LightsObjects;
    EPVector<DataHeatBalance::GlobalInternalGainMiscObject> ZoneElectricObjects;
    EPVector<DataHeatBalance::GlobalInternalGainMiscObject> ZoneGasObjects;
    EPVector<DataHeatBalance::GlobalInternalGainMiscObject> HotWaterEqObjects;
    EPVector<DataHeatBalance::GlobalInternalGainMiscObject> SteamEqObjects;
    EPVector<DataHeatBalance::GlobalInternalGainMiscObject> OtherEqObjects;
    EPVector<DataHeatBalance::GlobalInternalGainMiscObject> ITEqObjects;
    EPVector<DataHeatBalance::GlobalInternalGainMiscObject> ZoneBBHeatObjects;
    EPVector<DataHeatBalance::GlobalInternalGainMiscObject> InfiltrationObjects;
    EPVector<DataHeatBalance::GlobalInternalGainMiscObject> VentilationObjects;
    EPVector<DataHeatBalance::ZoneReportVars> ZnRpt;
    EPVector<DataHeatBalance::ZoneReportVars> spaceRpt;
    EPVector<DataHeatBalance::ZoneMassConservationData> MassConservation;
    DataHeatBalance::ZoneAirMassFlowConservation ZoneAirMassFlow;
    EPVector<DataHeatBalance::ZoneLocalEnvironmentData> ZoneLocalEnvironment;
    bool MundtFirstTimeFlag = true;
    EPVector<std::string> spaceTypes;

    void clear_state() override
    {
        *this = HeatBalanceData();
    }
};

} // namespace EnergyPlus

#endif
