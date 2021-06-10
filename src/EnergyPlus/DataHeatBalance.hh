// EnergyPlus, Copyright (c) 1996-2021, The Board of Trustees of the University of Illinois,
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
#include <EnergyPlus/Data/BaseData.hh>
#include <EnergyPlus/DataComplexFenestration.hh>
#include <EnergyPlus/DataGlobals.hh>
#include <EnergyPlus/DataSurfaces.hh>
#include <EnergyPlus/DataVectorTypes.hh>
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

    constexpr int RegularMaterial(0);
    constexpr int Air(1);
    constexpr int Shade(2);
    constexpr int WindowGlass(3);
    constexpr int WindowGas(4);
    constexpr int WindowBlind(5);
    constexpr int WindowGasMixture(6);
    constexpr int Screen(7);
    constexpr int EcoRoof(8);
    constexpr int IRTMaterial(9);
    constexpr int WindowSimpleGlazing(10);
    constexpr int ComplexWindowShade(11);
    constexpr int ComplexWindowGap(12);

    constexpr int GlassEquivalentLayer(13);
    constexpr int ShadeEquivalentLayer(14);
    constexpr int DrapeEquivalentLayer(15);
    constexpr int BlindEquivalentLayer(16);
    constexpr int ScreenEquivalentLayer(17);
    constexpr int GapEquivalentLayer(18);

    // Parameters to indicate surface roughness for use with the Material
    // derived type (see below):

    constexpr int VeryRough(1);
    constexpr int Rough(2);
    constexpr int MediumRough(3);
    constexpr int MediumSmooth(4);
    constexpr int Smooth(5);
    constexpr int VerySmooth(6);

    // Parameters to indicate blind orientation for use with the Material
    // derived type (see below):

    constexpr int Horizontal(1);
    constexpr int Vertical(2);
    constexpr int FixedSlats(1);
    constexpr int VariableSlats(2);

    // Parameters for Interior and Exterior Solar Distribution

    constexpr int MinimalShadowing(-1);    // all incoming solar hits floor, no exterior shadowing except reveals
    constexpr int FullExterior(0);         // all incoming solar hits floor, full exterior shadowing
    constexpr int FullInteriorExterior(1); // full interior solar distribution, full exterior solar shadowing
    constexpr int FullExteriorWithRefl(2); // all incoming solar hits floor, full exterior shadowing and reflections
    // full exterior shadowing and reflections
    // Parameters to indicate the zone type for use with the Zone derived
    // type (see below--Zone%OfType):

    constexpr int StandardZone(1);

    // Parameters to indicate the convection correlation being used for use with
    // InsideConvectionAlgo and OutsideConvectionAlgo

    constexpr int ASHRAESimple(1);
    constexpr int ASHRAETARP(2);
    constexpr int CeilingDiffuser(3); // Only valid for inside use
    constexpr int TrombeWall(4);      // Only valid for inside use
    constexpr int TarpHcOutside(5);   // Only valid for outside use
    constexpr int MoWiTTHcOutside(6); // Only valid for outside use
    constexpr int DOE2HcOutside(7);   // Only valid for outside use
    constexpr int BLASTHcOutside(8);  // Only valid for outside use
    constexpr int AdaptiveConvectionAlgorithm(9);
    constexpr int ASTMC1340(10);

    // Parameters for WarmupDays
    constexpr int DefaultMaxNumberOfWarmupDays(25); // Default maximum number of warmup days allowed
    constexpr int DefaultMinNumberOfWarmupDays(1);  // Default minimum number of warmup days allowed

    // Parameters for Sky Radiance Distribution
    constexpr int Isotropic(0);
    constexpr int Anisotropic(1);

    // Parameters for ZoneAirSolutionAlgo
    constexpr int Use3rdOrder(0);
    constexpr int UseAnalyticalSolution(1);
    constexpr int UseEulerMethod(2);

    // Parameter for MRT calculation type
    constexpr int ZoneAveraged(1);
    constexpr int SurfaceWeighted(2);
    constexpr int AngleFactor(3);

    // Parameters for Ventilation
    constexpr int NaturalVentilation(0);
    constexpr int IntakeVentilation(1);
    constexpr int ExhaustVentilation(2);
    constexpr int BalancedVentilation(3);

    // Parameters for hybrid ventilation using Ventilation and Mixing objects
    constexpr int HybridControlTypeIndiv(0);
    constexpr int HybridControlTypeClose(1);
    constexpr int HybridControlTypeGlobal(2);

    // System type, detailed refrigeration or refrigerated case rack
    constexpr int RefrigSystemTypeDetailed(1);
    constexpr int RefrigSystemTypeRack(2);

    // Refrigeration condenser type
    constexpr int RefrigCondenserTypeAir(1);
    constexpr int RefrigCondenserTypeEvap(2);
    constexpr int RefrigCondenserTypeWater(3);
    constexpr int RefrigCondenserTypeCascade(4);

    // Parameters for type of infiltration model
    constexpr int InfiltrationDesignFlowRate(1);
    constexpr int InfiltrationShermanGrimsrud(2);
    constexpr int InfiltrationAIM2(3);

    // Parameters for type of ventilation model
    constexpr int VentilationDesignFlowRate(1);
    constexpr int VentilationWindAndStack(2);

    // Parameters for type of zone air balance model
    constexpr int AirBalanceNone(0);
    constexpr int AirBalanceQuadrature(1);

    // Parameter for source zone air flow mass balance infiltration treatment
    constexpr int NoInfiltrationFlow(0);
    constexpr int AddInfiltrationFlow(1);
    constexpr int AdjustInfiltrationFlow(2);
    constexpr int MixingSourceZonesOnly(1);
    constexpr int AllZones(2);

    enum class AdjustmentType
    {
        // zone air flow balancing method
        AdjustMixingOnly,
        AdjustReturnOnly,
        AdjustMixingThenReturn,
        AdjustReturnThenMixing,
        NoAdjustReturnAndMixing
    };
    constexpr int NumZoneIntGainDeviceTypes(54);

    extern Array1D_string const ZoneIntGainDeviceTypes;
    extern Array1D_string const ccZoneIntGainDeviceTypes;

    constexpr int IntGainTypeOf_People(1);
    constexpr int IntGainTypeOf_Lights(2);
    constexpr int IntGainTypeOf_ElectricEquipment(3);
    constexpr int IntGainTypeOf_GasEquipment(4);
    constexpr int IntGainTypeOf_HotWaterEquipment(5);
    constexpr int IntGainTypeOf_SteamEquipment(6);
    constexpr int IntGainTypeOf_OtherEquipment(7);
    constexpr int IntGainTypeOf_ZoneBaseboardOutdoorTemperatureControlled(8);
    constexpr int IntGainTypeOf_ZoneContaminantSourceAndSinkCarbonDioxide(9);
    constexpr int IntGainTypeOf_WaterUseEquipment(10);
    constexpr int IntGainTypeOf_DaylightingDeviceTubular(11);
    constexpr int IntGainTypeOf_WaterHeaterMixed(12);
    constexpr int IntGainTypeOf_WaterHeaterStratified(13);
    constexpr int IntGainTypeOf_ThermalStorageChilledWaterMixed(14);
    constexpr int IntGainTypeOf_ThermalStorageChilledWaterStratified(15);
    constexpr int IntGainTypeOf_GeneratorFuelCell(16);
    constexpr int IntGainTypeOf_GeneratorMicroCHP(17);
    constexpr int IntGainTypeOf_ElectricLoadCenterTransformer(18);
    constexpr int IntGainTypeOf_ElectricLoadCenterInverterSimple(19);
    constexpr int IntGainTypeOf_ElectricLoadCenterInverterFunctionOfPower(20);
    constexpr int IntGainTypeOf_ElectricLoadCenterInverterLookUpTable(21);
    constexpr int IntGainTypeOf_ElectricLoadCenterStorageLiIonNmcBattery(22);
    constexpr int IntGainTypeOf_ElectricLoadCenterStorageBattery(23);
    constexpr int IntGainTypeOf_ElectricLoadCenterStorageSimple(24);
    constexpr int IntGainTypeOf_PipeIndoor(25);
    constexpr int IntGainTypeOf_RefrigerationCase(26);
    constexpr int IntGainTypeOf_RefrigerationCompressorRack(27);
    constexpr int IntGainTypeOf_RefrigerationSystemAirCooledCondenser(28);
    constexpr int IntGainTypeOf_RefrigerationTransSysAirCooledGasCooler(29);
    constexpr int IntGainTypeOf_RefrigerationSystemSuctionPipe(30);
    constexpr int IntGainTypeOf_RefrigerationTransSysSuctionPipeMT(31);
    constexpr int IntGainTypeOf_RefrigerationTransSysSuctionPipeLT(32);
    constexpr int IntGainTypeOf_RefrigerationSecondaryReceiver(33);
    constexpr int IntGainTypeOf_RefrigerationSecondaryPipe(34);
    constexpr int IntGainTypeOf_RefrigerationWalkIn(35);
    constexpr int IntGainTypeOf_Pump_VarSpeed(36);
    constexpr int IntGainTypeOf_Pump_ConSpeed(37);
    constexpr int IntGainTypeOf_Pump_Cond(38);
    constexpr int IntGainTypeOf_PumpBank_VarSpeed(39);
    constexpr int IntGainTypeOf_PumpBank_ConSpeed(40);
    constexpr int IntGainTypeOf_ZoneContaminantSourceAndSinkGenericContam(41);
    constexpr int IntGainTypeOf_PlantComponentUserDefined(42);
    constexpr int IntGainTypeOf_CoilUserDefined(43);
    constexpr int IntGainTypeOf_ZoneHVACForcedAirUserDefined(44);
    constexpr int IntGainTypeOf_AirTerminalUserDefined(45);
    constexpr int IntGainTypeOf_PackagedTESCoilTank(46);
    constexpr int IntGainTypeOf_ElectricEquipmentITEAirCooled(47);
    constexpr int IntGainTypeOf_SecCoolingDXCoilSingleSpeed(48);
    constexpr int IntGainTypeOf_SecHeatingDXCoilSingleSpeed(49);
    constexpr int IntGainTypeOf_SecCoolingDXCoilTwoSpeed(50);
    constexpr int IntGainTypeOf_SecCoolingDXCoilMultiSpeed(51);
    constexpr int IntGainTypeOf_SecHeatingDXCoilMultiSpeed(52);
    constexpr int IntGainTypeOf_ElectricLoadCenterConverter(53);
    constexpr int IntGainTypeOf_FanSystemModel(54);

    // Parameters for checking surface heat transfer models
    constexpr Real64 HighDiffusivityThreshold(1.e-5);   // used to check if Material properties are out of line.
    constexpr Real64 ThinMaterialLayerThreshold(0.003); // 3 mm lower limit to expected material layers

    // Const for initialization
    constexpr Real64 ZoneInitialTemp(23.0);       // Zone temperature for initialization
    constexpr Real64 SurfInitialTemp(23.0);       // Surface temperature for initialization
    constexpr Real64 SurfInitialConvCoeff(3.076); // Surface convective coefficient for initialization

    // Air       Argon     Krypton   Xenon
    extern Array2D<Real64> const GasCoeffsCon; // Gas conductivity coefficients for gases in a mixture

    // Air       Argon     Krypton   Xenon
    extern Array2D<Real64> const GasCoeffsVis; // Gas viscosity coefficients for gases in a mixture

    // Air       Argon     Krypton   Xenon
    extern Array2D<Real64> const GasCoeffsCp; // Gas specific heat coefficients for gases in a mixture

    // Air       Argon     Krypton   Xenon
    extern Array1D<Real64> const GasWght; // Gas molecular weights for gases in a mixture

    // Gas specific heat ratios.  Used for gasses in low pressure
    extern Array1D<Real64> const GasSpecificHeatRatio;

    struct TCGlazingsType
    {
        // Members
        std::string Name;         // Name
        int NumGlzMat;            // Number of TC glazing materials
        Array1D_int LayerPoint;   // Layer pointer
        Array1D<Real64> SpecTemp; // Temperature corresponding to the specified TC glazing optical data
        Array1D_string LayerName; // Name of the referenced WindowMaterial:Glazing object

        // Default Constructor
        TCGlazingsType() : NumGlzMat(0)
        {
        }
    };

    struct SpectralDataProperties
    {
        // Members
        std::string Name;           // Name of spectral data set
        int NumOfWavelengths;       // Number of wavelengths in the data set
        Array1D<Real64> WaveLength; // Wavelength (microns)
        Array1D<Real64> Trans;      // Transmittance at normal incidence
        Array1D<Real64> ReflFront;  // Front reflectance at normal incidence
        Array1D<Real64> ReflBack;   // Back reflectance at normal incidence

        // Default Constructor
        SpectralDataProperties() : NumOfWavelengths(0)
        {
        }
    };

    struct ZoneData
    {
        // Members
        std::string Name;
        int Multiplier;       // Used in reporting and for systems calculations
        int ListMultiplier;   // For Zone Group object:  used in reporting and systems calculations
        int ListGroup;        // used only in Zone Group verification.  and for error message.
        Real64 RelNorth;      // Relative North (to building north) [Degrees]
        Real64 OriginX;       // X origin  [m]
        Real64 OriginY;       // Y origin  [m]
        Real64 OriginZ;       // Z origin  [m]
        Real64 CeilingHeight; // Ceiling Height entered by user [m] or calculated
        Real64 Volume;        // Volume entered by user [m3] or calculated
        int OfType;           // 1=Standard Zone, Not yet used:
        // 2=Plenum Zone, 11=Solar Wall, 12=Roof Pond
        Real64 UserEnteredFloorArea; // User input floor area for this zone
        // Calculated after input
        Real64 FloorArea;        // Floor area used for this zone
        Real64 CalcFloorArea;    // Calculated floor area used for this zone
        Real64 CeilingArea;      // Ceiling area for the zone
        bool HasFloor;           // Has "Floor" surface
        bool HasRoof;            // Has "Roof" or "Ceiling" Surface
        bool HasInterZoneWindow; // Interzone Window(s) present in this zone
        bool HasWindow;          // Window(s) present in this zone
        Real64 AirCapacity;
        Real64 ExtWindowArea;               // Exterior Window Area for Zone
        Real64 ExtGrossWallArea;            // Exterior Wall Area for Zone (Gross)
        Real64 ExtWindowArea_Multiplied;    // Exterior Window Area for Zone with multipliers
        Real64 ExtGrossWallArea_Multiplied; // Exterior Wall Area for Zone (Gross) with multipliers
        Real64 ExtNetWallArea;              // Exterior Wall Area for Zone (Net)
        Real64 TotalSurfArea;               // Total surface area for Zone
        Real64 ExteriorTotalSurfArea;       // Total surface area of all exterior surfaces for Zone
        // (ignoring windows as they will be included in their base surfaces)
        Real64 ExteriorTotalGroundSurfArea;       // Total surface area of all surfaces for Zone with ground contact
        Real64 ExtGrossGroundWallArea;            // Ground contact Wall Area for Zone (Gross)
        Real64 ExtGrossGroundWallArea_Multiplied; // Ground contact Wall Area for Zone (Gross) with multipliers
        int SystemZoneNodeNumber;                 // This is the zone node number for the system for a controlled zone
        bool IsControlled;                        // True when this is a controlled zone.
        bool IsSupplyPlenum;                      // True when this zone is a supply plenum
        bool IsReturnPlenum;                      // True when this zone is a return plenum
        int ZoneEqNum;                            // Controlled zone equip config number
        int PlenumCondNum;                        // Supply or return plenum conditions number, 0 if this is not a plenum zone
        int TempControlledZoneIndex;              // this is the index number for TempControlledZone structure for lookup
        //            Pointers to Surface Data Structure
        int AllSurfaceFirst;                         // First surface in zone including air boundaries
        int AllSurfaceLast;                          // Last  surface in zone including air boundaries
        int HTSurfaceFirst;                          // First Heat Transfer Surface in Zone
        int HTSurfaceLast;                           // Last  Heat Transfer Surface in Zone
        int OpaqOrIntMassSurfaceFirst;               // First Opaque or Interior Mass Heat Transfer Surface (including opaque doors) in Zone
        int OpaqOrIntMassSurfaceLast;                // Last  Opaque or Interior Mass Heat Transfer Surface (including opaque doors) in Zone
        int WindowSurfaceFirst;                      // First Window Heat Transfer Surface in Zone
        int WindowSurfaceLast;                       // Last  Window Heat Transfer Surface in Zone
        int OpaqOrWinSurfaceFirst;                   // First opaque (including IntMass) or window (non TDD Dome) Surface in Zone
        int OpaqOrWinSurfaceLast;                    // Last  opaque (including IntMass) or window (non TDD Dome) Surface in Zone
        int TDDDomeFirst;                            // First TDD Dome Surface in Zone
        int TDDDomeLast;                             // Last  TDD Dome Surface in Zone
        int InsideConvectionAlgo;                    // Ref: appropriate values for Inside Convection solution
        int NumSurfaces;                             // Number of surfaces for this zone
        int NumSubSurfaces;                          // Number of subsurfaces for this zone (windows, doors, tdd dome and diffusers)
        int NumShadingSurfaces;                      // Number of shading surfaces for this zone
        int OutsideConvectionAlgo;                   // Ref: appropriate values for Outside Convection solution
        Vector Centroid;                             // Center of the zone found by averaging wall, floor, and roof centroids
        Real64 MinimumX;                             // Minimum X value for entire zone
        Real64 MaximumX;                             // Maximum X value for entire zone
        Real64 MinimumY;                             // Minimum Y value for entire zone
        Real64 MaximumY;                             // Maximum Y value for entire zone
        Real64 MinimumZ;                             // Minimum Z value for entire zone
        Real64 MaximumZ;                             // Maximum Z value for entire zone
        std::vector<int> ZoneHTSurfaceList;          // List of HT surfaces related to this zone (includes adjacent interzone surfaces)
        std::vector<int> ZoneIZSurfaceList;          // List of interzone surfaces in this zone
        std::vector<int> ZoneHTNonWindowSurfaceList; // List of non-window HT surfaces related to this zone (includes adjacent interzone surfaces)
        std::vector<int> ZoneHTWindowSurfaceList;    // List of window surfaces related to this zone (includes adjacent interzone surfaces)
        std::vector<int> ZoneExtSolarSurfaceList;    // List of exterior solar surfaces in a zone
        int RadiantEnclosureNum;                     // Radiant exchange enclosure this zone belongs to (related to air boundaries)
        int SolarEnclosureNum;                       // Solar distribution enclosure this zone belongs to (related to air boundaries)

        Real64 OutDryBulbTemp;                 // Zone outside dry bulb air temperature (C)
        bool OutDryBulbTempEMSOverrideOn;      // if true, EMS is calling to override the surface's outdoor air temp
        Real64 OutDryBulbTempEMSOverrideValue; // value to use for EMS override of outdoor air drybulb temp (C)
        Real64 OutWetBulbTemp;                 // Zone outside wet bulb air temperature (C)
        bool OutWetBulbTempEMSOverrideOn;      // if true, EMS is calling to override the surface's outdoor wetbulb
        Real64 OutWetBulbTempEMSOverrideValue; // value to use for EMS override of outdoor air wetbulb temp (C)
        Real64 WindSpeed;                      // Zone outside wind speed (m/s)
        bool WindSpeedEMSOverrideOn;           // if true, EMS is calling to override the surface's outside wind speed
        Real64 WindSpeedEMSOverrideValue;      // value to use for EMS override of the surface's outside wind speed
        Real64 WindDir;                        // Zone outside wind direction (degree)
        bool WindDirEMSOverrideOn;             // if true, EMS is calling to override the surface's outside wind direction
        Real64 WindDirEMSOverrideValue;        // value to use for EMS override of the surface's outside wind speed

        bool HasLinkedOutAirNode; // true if an OutdoorAir::Node is linked to the surface
        int LinkedOutAirNode;     // Index of the an OutdoorAir:Node

        bool isPartOfTotalArea;   // Count the zone area when determining the building total floor area
        bool isNominalOccupied;   // has occupancy nominally specified
        bool isNominalControlled; // has Controlled Zone Equip Configuration reference
        Real64 TotOccupants;      // total design occupancy
        // (sum of NumberOfPeople for the zone from People object)
        int AirHBimBalanceErrIndex;      // error management counter
        bool NoHeatToReturnAir;          // TRUE means that heat to return air should be added to the zone load
        bool RefrigCaseRA;               // TRUE means there is potentially heat removal from return air
        bool HasAdjustedReturnTempByITE; // TRUE means that return temp to return air is adjusted by return temperature of ITE object
        Real64 AdjustedReturnTempByITE;  // Diff of the return temp from the zone mixed air temp adjusted by ITE object

        bool HasLtsRetAirGain;       // TRUE means that zone lights return air heat > 0.0 calculated from plenum temperature
        bool HasAirFlowWindowReturn; // TRUE means that zone has return air flow from windows
        // from refrigeration cases for this zone
        Real64 InternalHeatGains;     // internal loads (W)
        Real64 NominalInfilVent;      // internal infiltration/ventilation
        Real64 NominalMixing;         // internal mixing/cross mixing
        bool TempOutOfBoundsReported; // if any temp out of bounds errors, first will show zone details.
        bool EnforcedReciprocity;     // if zone required forced reciprocity --
        //   less out of bounds temperature errors allowed
        int ZoneMinCO2SchedIndex;           // Index for the schedule the schedule which determines minimum CO2 concentration
        int ZoneMaxCO2SchedIndex;           // Index for the schedule the schedule which determines maximum CO2 concentration
        int ZoneContamControllerSchedIndex; // Index for this schedule
        bool FlagCustomizedZoneCap;         // True if customized Zone Capacitance Multiplier is used

        // Hybrid Modeling
        Real64 ZoneMeasuredTemperature;               // Measured zone air temperature input by user
        Real64 ZoneMeasuredHumidityRatio;             // Measured zone air humidity ratio by user
        Real64 ZoneMeasuredCO2Concentration;          // Measured zone air CO2 concentration input by user
        Real64 ZoneMeasuredSupplyAirTemperature;      // Measured zone supply air temperature input by user
        Real64 ZoneMeasuredSupplyAirFlowRate;         // Measured zone supply air flow rate input by user
        Real64 ZoneMeasuredSupplyAirHumidityRatio;    // Measured zone supply air flow rate input by user
        Real64 ZoneMeasuredSupplyAirCO2Concentration; // Measured zone supply air flow rate input by user
        Real64 ZonePeopleActivityLevel;               // People activity level input by user
        Real64 ZonePeopleSensibleHeatFraction;        // People activity level input by user
        Real64 ZonePeopleRadiantHeatFraction;         // People activity level input by user
        Real64 ZoneVolCapMultpSens;                   // Zone temperature capacity multiplier, i.e. internal thermal mass multiplier
        Real64 ZoneVolCapMultpMoist;                  // Zone humidity capacity multiplier
        Real64 ZoneVolCapMultpCO2;                    // Zone carbon dioxide capacity multiplier
        Real64 ZoneVolCapMultpGenContam;              // Zone generic contaminant capacity multiplier
        Real64 ZoneVolCapMultpSensHM;                 // Calculated temperature capacity multiplier by hybrid model
        Real64 ZoneVolCapMultpSensHMSum;              // for temperature capacity multiplier average calculation
        Real64 ZoneVolCapMultpSensHMCountSum;         // for temperature capacity multiplier average calculation
        Real64 ZoneVolCapMultpSensHMAverage;          // Temperature capacity multiplier average
        Real64 MCPIHM;                                // Calculated mass flow rate by hybrid model
        Real64 InfilOAAirChangeRateHM;                // Calculated infiltration air change per hour by hybrid model
        Real64 NumOccHM;                              // Inversely solved people count
        Real64 delta_T;                               // Indoor and outdoor temperature
        Real64 delta_HumRat;                          // Indoor and outdoor humidity ratio delta

        // Default Constructor
        ZoneData()
            : Multiplier(1), ListMultiplier(1), ListGroup(0), RelNorth(0.0), OriginX(0.0), OriginY(0.0), OriginZ(0.0),
              CeilingHeight(DataGlobalConstants::AutoCalculate), Volume(DataGlobalConstants::AutoCalculate), OfType(1),
              UserEnteredFloorArea(DataGlobalConstants::AutoCalculate), FloorArea(0.0), CalcFloorArea(0.0), CeilingArea(0.0), HasFloor(false),
              HasRoof(false), HasInterZoneWindow(false), HasWindow(false), AirCapacity(0.0), ExtWindowArea(0.0), ExtGrossWallArea(0.0),
              ExtWindowArea_Multiplied(0.0), ExtGrossWallArea_Multiplied(0.0), ExtNetWallArea(0.0), TotalSurfArea(0.0), ExteriorTotalSurfArea(0.0),
              ExteriorTotalGroundSurfArea(0.0), ExtGrossGroundWallArea(0.0), ExtGrossGroundWallArea_Multiplied(0.0), SystemZoneNodeNumber(0),
              IsControlled(false), IsSupplyPlenum(false), IsReturnPlenum(false), ZoneEqNum(0), PlenumCondNum(0), TempControlledZoneIndex(0),
              AllSurfaceFirst(0), AllSurfaceLast(-1), HTSurfaceFirst(0), HTSurfaceLast(-1), OpaqOrIntMassSurfaceFirst(0),
              OpaqOrIntMassSurfaceLast(-1), WindowSurfaceFirst(0), WindowSurfaceLast(-1), OpaqOrWinSurfaceFirst(0), OpaqOrWinSurfaceLast(-1),
              TDDDomeFirst(0), TDDDomeLast(-1), InsideConvectionAlgo(ASHRAESimple), NumSurfaces(0), NumSubSurfaces(0), NumShadingSurfaces(0),
              OutsideConvectionAlgo(ASHRAESimple), Centroid(0.0, 0.0, 0.0), MinimumX(0.0), MaximumX(0.0), MinimumY(0.0), MaximumY(0.0), MinimumZ(0.0),
              MaximumZ(0.0), RadiantEnclosureNum(0), SolarEnclosureNum(0),

              OutDryBulbTemp(0.0), OutDryBulbTempEMSOverrideOn(false), OutDryBulbTempEMSOverrideValue(0.0), OutWetBulbTemp(0.0),
              OutWetBulbTempEMSOverrideOn(false), OutWetBulbTempEMSOverrideValue(0.0), WindSpeed(0.0), WindSpeedEMSOverrideOn(false),
              WindSpeedEMSOverrideValue(0.0), WindDir(0.0), WindDirEMSOverrideOn(false), WindDirEMSOverrideValue(0.0), HasLinkedOutAirNode(false),
              LinkedOutAirNode(0.0), isPartOfTotalArea(true), isNominalOccupied(false), isNominalControlled(false), TotOccupants(0.0),
              AirHBimBalanceErrIndex(0), NoHeatToReturnAir(false), RefrigCaseRA(false), HasAdjustedReturnTempByITE(false),
              AdjustedReturnTempByITE(0.0), HasLtsRetAirGain(false), HasAirFlowWindowReturn(false), InternalHeatGains(0.0), NominalInfilVent(0.0),
              NominalMixing(0.0), TempOutOfBoundsReported(false), EnforcedReciprocity(false), ZoneMinCO2SchedIndex(0), ZoneMaxCO2SchedIndex(0),
              ZoneContamControllerSchedIndex(0), FlagCustomizedZoneCap(false),
              // Hybrid Modeling
              ZoneMeasuredTemperature(0.0), ZoneMeasuredHumidityRatio(0.0), ZoneMeasuredCO2Concentration(0.0), ZoneMeasuredSupplyAirTemperature(0.0),
              ZoneMeasuredSupplyAirFlowRate(0.0), ZoneMeasuredSupplyAirHumidityRatio(0.0), ZoneMeasuredSupplyAirCO2Concentration(0.0),
              ZonePeopleActivityLevel(0.0), ZonePeopleSensibleHeatFraction(0.0), ZonePeopleRadiantHeatFraction(0.0), ZoneVolCapMultpSens(1.0),
              ZoneVolCapMultpMoist(1.0), ZoneVolCapMultpCO2(1.0), ZoneVolCapMultpGenContam(1.0), ZoneVolCapMultpSensHM(1.0),
              ZoneVolCapMultpSensHMSum(0.0), ZoneVolCapMultpSensHMCountSum(0.0), ZoneVolCapMultpSensHMAverage(1.0), MCPIHM(0.0),
              InfilOAAirChangeRateHM(0.0), NumOccHM(0.0), delta_T(0.0), delta_HumRat(0.0)
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
        int NumOfZones;                           // Number of zones in the list
        std::string::size_type MaxZoneNameLength; // Max Name length of zones in the list
        Array1D_int Zone;                         // Pointers to zones in the list

        // Default Constructor
        ZoneListData() : NumOfZones(0), MaxZoneNameLength(0u)
        {
        }
    };

    struct ZoneGroupData
    {
        // Members
        std::string Name; // Zone Group name
        int ZoneList;     // Pointer to the zone list
        int Multiplier;   // Zone List multiplier

        // Default Constructor
        ZoneGroupData() : ZoneList(0), Multiplier(1)
        {
        }
    };

    struct GlobalInternalGainMiscObject
    {
        // Members
        std::string Name;
        int ZoneOrZoneListPtr;
        int NumOfZones;
        int StartPtr;
        bool ZoneListActive;

        // Default Constructor
        GlobalInternalGainMiscObject() : ZoneOrZoneListPtr(0), NumOfZones(0), StartPtr(0), ZoneListActive(false)
        {
        }
    };

    struct PeopleData
    {
        // Members
        std::string Name;         // PEOPLE object name
        int ZonePtr;              // Pointer to the zone number for this people statement
        Real64 NumberOfPeople;    // Maximum number of people for this statement
        int NumberOfPeoplePtr;    // Pointer to schedule for number of people
        bool EMSPeopleOn;         // EMS actuating number of people if .TRUE.
        Real64 EMSNumberOfPeople; // Value EMS is directing to use for override
        // Note that the schedule and maximum number was kept for people since it seemed likely that
        // users would want to assign the same schedule to multiple people statements.
        int ActivityLevelPtr;   // Pointer to schedule for activity level
        Real64 FractionRadiant; // Percentage (fraction 0.0-1.0) of sensible heat gain from people
        // that is radiant
        Real64 FractionConvected; // Percentage (fraction 0.0-1.0) of sensible heat gain from people
        // that is convective
        Real64 NomMinNumberPeople; // Nominal Minimum Number of People (min sch X number of people)
        Real64 NomMaxNumberPeople; // Nominal Maximum Number of People (min sch X number of people)
        int WorkEffPtr;            // Pointer to schedule for work efficiency
        int ClothingPtr;           // Pointer to schedule for clothing insulation
        int ClothingMethodPtr;
        int ClothingType;        // Name of clothing type
        int AirVelocityPtr;      // Pointer to schedule for air velocity in zone
        int AnkleAirVelocityPtr; // Pointer to schedule for air velocity in zone
        bool Fanger;             // True when Fanger calculation to be performed
        bool Pierce;             // True when Pierce 2-node calculation to be performed
        bool KSU;                // True when KSU 2-node calculation to be performed
        bool AdaptiveASH55;      // True when ASHRAE Standard 55 adaptive comfort calculation
        //   to be performed
        bool AdaptiveCEN15251; // True when CEN Standard 15251 adaptive comfort calculation
        //   to be performed
        bool CoolingEffectASH55;         // True when ASHRAE Standard 55 cooling effect calculation to be performed
        bool AnkleDraftASH55;            // True when ASHRAE Standard 55 ankle draft calculation to be performed
        int MRTCalcType;                 // MRT calculation type (See MRT Calculation type parameters)
        int SurfacePtr;                  // Pointer to the name of surface
        std::string AngleFactorListName; // Name of angle factor list
        int AngleFactorListPtr;          // Pointer to the name of angle factor list
        Real64 UserSpecSensFrac;         // User specified sensible fraction
        bool Show55Warning;              // show the warning messages about ASHRAE 55-2004
        Real64 CO2RateFactor;            // Carbon Dioxide Generation Rate [m3/s-W]
        // Report variables
        Real64 NumOcc;                 // Number of occupants at current timestep []
        Real64 TemperatureInZone;      // Temperature in zone (C)
        Real64 RelativeHumidityInZone; // Relative humidity in zone
        Real64 RadGainRate;            // Radiant heat gain [W]
        Real64 ConGainRate;            // Convective heat gain [W]
        Real64 SenGainRate;            // Sensible heat gain [W]
        Real64 LatGainRate;            // Latent heat gain [W]
        Real64 TotGainRate;            // Total heat gain [W]
        Real64 CO2GainRate;            // Carbon Dioxide Gain Rate [m3/s]
        Real64 RadGainEnergy;          // Radiant heat gain [J]
        Real64 ConGainEnergy;          // Convective heat gain [J]
        Real64 SenGainEnergy;          // Sensible heat gain [J]
        Real64 LatGainEnergy;          // Latent heat gain [J]
        Real64 TotGainEnergy;          // Total heat gain [J]
        // Air velocity check during run time for thermal comfort control
        int AirVelErrIndex; // Air velocity error index
        // For AdaptiveComfort tabular report
        Real64 TimeNotMetASH5580;
        Real64 TimeNotMetASH5590;
        Real64 TimeNotMetCEN15251CatI;
        Real64 TimeNotMetCEN15251CatII;
        Real64 TimeNotMetCEN15251CatIII;

        // Default Constructor
        PeopleData()
            : ZonePtr(0), NumberOfPeople(0.0), NumberOfPeoplePtr(-1), EMSPeopleOn(false), EMSNumberOfPeople(0.0), ActivityLevelPtr(-1),
              FractionRadiant(0.0), FractionConvected(0.0), NomMinNumberPeople(0.0), NomMaxNumberPeople(0.0), WorkEffPtr(-1), ClothingPtr(-1),
              ClothingMethodPtr(-1), ClothingType(-1), AirVelocityPtr(-1), AnkleAirVelocityPtr(-1), Fanger(false), Pierce(false), KSU(false),
              AdaptiveASH55(false), AdaptiveCEN15251(false), CoolingEffectASH55(false), AnkleDraftASH55(false), MRTCalcType(0), SurfacePtr(-1),
              AngleFactorListPtr(-1), UserSpecSensFrac(0.0), Show55Warning(false), CO2RateFactor(0.0), NumOcc(0.0), TemperatureInZone(0.0),
              RelativeHumidityInZone(0.0), RadGainRate(0.0), ConGainRate(0.0), SenGainRate(0.0), LatGainRate(0.0), TotGainRate(0.0), CO2GainRate(0.0),
              RadGainEnergy(0.0), ConGainEnergy(0.0), SenGainEnergy(0.0), LatGainEnergy(0.0), TotGainEnergy(0.0), AirVelErrIndex(0),
              TimeNotMetASH5580(0.0), TimeNotMetASH5590(0.0), TimeNotMetCEN15251CatI(0.0), TimeNotMetCEN15251CatII(0.0), TimeNotMetCEN15251CatIII(0.0)
        {
        }
    };

    struct LightsData
    {
        // Members
        std::string Name;           // LIGHTS object name
        int ZonePtr;                // Which zone lights are in
        int SchedPtr;               // Schedule for lights
        Real64 DesignLevel;         // design level for lights [W]
        bool EMSLightsOn;           // EMS actuating Lighting power if .TRUE.
        Real64 EMSLightingPower;    // Value EMS is directing to use for override
        Real64 FractionReturnAir;   // Percentage (fraction 0.0-1.0) of sensible heat gain that is return air
        Real64 FractionRadiant;     // Percentage (fraction 0.0-1.0) of sensible heat gain that is radiant
        Real64 FractionShortWave;   // Percentage (fraction 0.0-1.0) of sensible heat gain that is short wave
        Real64 FractionReplaceable; // Percentage (fraction 0.0-1.0) of sensible heat gain that is replaceable
        Real64 FractionConvected;   // Percentage (fraction 0.0-1.0) of sensible heat gain that is convective
        bool FractionReturnAirIsCalculated;
        Real64 FractionReturnAirPlenTempCoeff1;
        Real64 FractionReturnAirPlenTempCoeff2;
        int ZoneReturnNum;        // zone return index (not the node number) for return heat gain
        Real64 NomMinDesignLevel; // Nominal Minimum Design Level (min sch X design level)
        Real64 NomMaxDesignLevel; // Nominal Maximum Design Level (max sch X design level)
        bool ManageDemand;        // Flag to indicate whether to use demand limiting
        Real64 DemandLimit;       // Demand limit set by demand manager [W]
        // Report variables
        Real64 Power;                  // Electric power [W]
        Real64 RadGainRate;            // Radiant heat gain [W]
        Real64 VisGainRate;            // Visible heat gain [W]
        Real64 ConGainRate;            // Convective heat gain [W]
        Real64 RetAirGainRate;         // Return air heat gain [W]
        Real64 TotGainRate;            // Total heat gain [W]
        Real64 Consumption;            // Electric consumption [J]
        Real64 RadGainEnergy;          // Radiant heat gain [J]
        Real64 VisGainEnergy;          // Visible heat gain [J]
        Real64 ConGainEnergy;          // Convective heat gain [J]
        Real64 RetAirGainEnergy;       // Return air heat gain [J]
        Real64 TotGainEnergy;          // Total heat gain [J]
        std::string EndUseSubcategory; // user defined name for the end use category
        Real64 SumConsumption;         // sum of electric consumption [J] for reporting
        Real64 SumTimeNotZeroCons;     // sum of time of positive electric consumption [hr]

        // Default Constructor
        LightsData()
            : ZonePtr(0), SchedPtr(-1), DesignLevel(0.0), EMSLightsOn(false), EMSLightingPower(0.0), FractionReturnAir(0.0), FractionRadiant(0.0),
              FractionShortWave(0.0), FractionReplaceable(0.0), FractionConvected(0.0), FractionReturnAirIsCalculated(false),
              FractionReturnAirPlenTempCoeff1(0.0), FractionReturnAirPlenTempCoeff2(0.0), ZoneReturnNum(1), NomMinDesignLevel(0.0),
              NomMaxDesignLevel(0.0), ManageDemand(false), DemandLimit(0.0), Power(0.0), RadGainRate(0.0), VisGainRate(0.0), ConGainRate(0.0),
              RetAirGainRate(0.0), TotGainRate(0.0), Consumption(0.0), RadGainEnergy(0.0), VisGainEnergy(0.0), ConGainEnergy(0.0),
              RetAirGainEnergy(0.0), TotGainEnergy(0.0), SumConsumption(0.0), SumTimeNotZeroCons(0.0)
        {
        }
    };

    struct ZoneEquipData // Electric, Gas, Other Equipment, CO2
    {
        // Members
        std::string Name;            // EQUIPMENT object name
        int ZonePtr;                 // Which zone internal gain is in
        int SchedPtr;                // Schedule for internal gain
        Real64 DesignLevel;          // design level for internal gain [W]
        bool EMSZoneEquipOverrideOn; // EMS actuating equipment power if .TRUE.
        Real64 EMSEquipPower;        // Value EMS is directing to use for override
        Real64 FractionLatent;       // Percentage (fraction 0.0-1.0) of sensible heat gain that is latent
        Real64 FractionRadiant;      // Percentage (fraction 0.0-1.0) of sensible heat gain that is radiant
        Real64 FractionLost;         // Percentage (fraction 0.0-1.0) of sensible heat gain that is lost
        Real64 FractionConvected;    // Percentage (fraction 0.0-1.0) of sensible heat gain that is convective
        Real64 CO2DesignRate;        // CO2 design Rate [m3/s]
        Real64 CO2RateFactor;        // CO2 rate factor [m3/s/W]
        Real64 NomMinDesignLevel;    // Nominal Minimum Design Level (min sch X design level)
        Real64 NomMaxDesignLevel;    // Nominal Maximum Design Level (max sch X design level)
        bool ManageDemand;           // Flag to indicate whether to use demand limiting
        Real64 DemandLimit;          // Demand limit set by demand manager [W]
        // Report variables
        Real64 Power;                                            // Electric/Gas/Fuel power [W]
        Real64 RadGainRate;                                      // Radiant heat gain [W]
        Real64 ConGainRate;                                      // Convective heat gain [W]
        Real64 LatGainRate;                                      // Latent heat gain [W]
        Real64 LostRate;                                         // Lost energy (converted to work) [W]
        Real64 TotGainRate;                                      // Total heat gain [W]
        Real64 CO2GainRate;                                      // CO2 gain rate [m3/s]
        Real64 Consumption;                                      // Electric/Gas/Fuel consumption [J]
        Real64 RadGainEnergy;                                    // Radiant heat gain [J]
        Real64 ConGainEnergy;                                    // Convective heat gain [J]
        Real64 LatGainEnergy;                                    // Latent heat gain [J]
        Real64 LostEnergy;                                       // Lost energy (converted to work) [J]
        Real64 TotGainEnergy;                                    // Total heat gain [J]
        std::string EndUseSubcategory;                           // user defined name for the end use category
        ExteriorEnergyUse::ExteriorFuelUsage OtherEquipFuelType; // Fuel Type Number of the Other Equipment (defined in ExteriorEnergyUse.cc)

        // Default Constructor
        ZoneEquipData()
            : ZonePtr(0), SchedPtr(0), DesignLevel(0.0), EMSZoneEquipOverrideOn(false), EMSEquipPower(0.0), FractionLatent(0.0), FractionRadiant(0.0),
              FractionLost(0.0), FractionConvected(0.0), CO2DesignRate(0.0), CO2RateFactor(0.0), NomMinDesignLevel(0.0), NomMaxDesignLevel(0.0),
              ManageDemand(false), DemandLimit(0.0), Power(0.0), RadGainRate(0.0), ConGainRate(0.0), LatGainRate(0.0), LostRate(0.0),
              TotGainRate(0.0), CO2GainRate(0.0), Consumption(0.0), RadGainEnergy(0.0), ConGainEnergy(0.0), LatGainEnergy(0.0), LostEnergy(0.0),
              TotGainEnergy(0.0), OtherEquipFuelType(ExteriorEnergyUse::ExteriorFuelUsage::Unknown)
        {
        }
    };

    struct ITEquipData // IT Equipment
    {
        // Members
        std::string Name;                  // EQUIPMENT object name
        int ZonePtr;                       // Which zone internal gain is in
        bool FlowControlWithApproachTemps; // True if using supply and return approach temperature for ITE object.
        Real64 DesignTotalPower;           // Design level for internal gain [W]
        Real64 NomMinDesignLevel;          // Nominal Minimum Design Level (min sch X design level)
        Real64 NomMaxDesignLevel;          // Nominal Maximum Design Level (max sch X design level)
        Real64 DesignFanPowerFrac;         // Fraction (0.0-1.0) of design power level that is fans
        int OperSchedPtr;                  // Schedule pointer for design power input or operating schedule
        int CPULoadSchedPtr;               // Schedule pointer for CPU loading schedule
        Real64 SizingTAirIn;               // Entering air dry-bulb temperature at maximum value during sizing[C]
        Real64 DesignTAirIn;               // Design entering air dry-bulb temperature [C]
        Real64 DesignFanPower;             // Design fan power input [W]
        Real64 DesignCPUPower;             // Design CPU power input [W]
        Real64 DesignAirVolFlowRate;       // Design air volume flow rate [m3/s]
        int Class;                         // Environmental class index (A1=1, A2=2, A3=3, A4=4, B=5, C=6)
        int AirFlowFLTCurve;               // Index for airflow function of CPULoadFrac (x) and TAirIn (y) curve
        int CPUPowerFLTCurve;              // Index for CPU power function of CPULoadFrac (x) and TAirIn (y) curve
        int FanPowerFFCurve;               // Index for fan power function of flow fraction curve
        int AirConnectionType;             // Air connection type (AdjustedSupply, ZoneAirNode, RoomAirModel)
        int InletRoomAirNodeNum;           // Room air model node number for air inlet
        int OutletRoomAirNodeNum;          // Room air model node number for air outlet
        int SupplyAirNodeNum;              // Node number for supply air inlet
        Real64 DesignRecircFrac;           // Design recirculation fraction (0.0-0.5)
        int RecircFLTCurve;                // Index for recirculation function of CPULoadFrac (x) and TSupply (y) curve
        Real64 DesignUPSEfficiency;        // Design power supply efficiency (>0.0 - 1.0)
        int UPSEfficFPLRCurve;             // Index for recirculation function of part load ratio
        Real64 UPSLossToZoneFrac;          // Fraction of UPS power loss to zone (0.0 - 1.0); remainder is lost
        std::string EndUseSubcategoryCPU;  // User defined name for the end use category for the CPU
        std::string EndUseSubcategoryFan;  // User defined name for the end use category for the Fans
        std::string EndUseSubcategoryUPS;  // User defined name for the end use category for the power supply
        bool EMSCPUPowerOverrideOn;        // EMS actuating CPU power if .TRUE.
        Real64 EMSCPUPower;                // Value EMS is directing to use for override of CPU power [W]
        bool EMSFanPowerOverrideOn;        // EMS actuating Fan power if .TRUE.
        Real64 EMSFanPower;                // Value EMS is directing to use for override of Fan power [W]
        bool EMSUPSPowerOverrideOn;        // EMS actuating UPS power if .TRUE.
        Real64 EMSUPSPower;                // Value EMS is directing to use for override of UPS power [W]
        Real64 SupplyApproachTemp;         // The difference of the IT inlet temperature from the AHU supply air temperature
        int SupplyApproachTempSch;         // The difference schedule of the IT inlet temperature from the AHU supply air temperature
        Real64 ReturnApproachTemp;         // The difference of the unit outlet temperature from the well mixed zone temperature
        int ReturnApproachTempSch;         // The difference schedule of the unit outlet temperature from the well mixed zone temperature

        // Report variables
        Real64 CPUPower;            // ITE CPU Electric Power [W]
        Real64 FanPower;            // ITE Fan Electric Power [W]
        Real64 UPSPower;            // ITE UPS Electric Power [W]
        Real64 CPUPowerAtDesign;    // ITE CPU Electric Power at Design Inlet Conditions [W]
        Real64 FanPowerAtDesign;    // ITE Fan Electric Power at Design Inlet Conditions [W]
        Real64 UPSGainRateToZone;   // ITE UPS Heat Gain to Zone Rate [W] - convective gain
        Real64 ConGainRateToZone;   // ITE Total Heat Gain to Zone Rate [W] - convective gain - includes heat gain from UPS, plus CPU and Fans if room
                                    // air model not used
        Real64 CPUConsumption;      // ITE CPU Electric Energy [J]
        Real64 FanConsumption;      // ITE Fan Electric Energy [J]
        Real64 UPSConsumption;      // ITE UPS Electric Energy [J]
        Real64 CPUEnergyAtDesign;   // ITE CPU Electric Energy at Design Inlet Conditions [J]
        Real64 FanEnergyAtDesign;   // ITE Fan Electric Energy at Design Inlet Conditions [J]
        Real64 UPSGainEnergyToZone; // ITE UPS Heat Gain to Zone Energy [J] - convective gain
        Real64 ConGainEnergyToZone; // ITE Total Heat Gain to Zone Energy [J] - convective gain - includes heat gain from UPS, plus CPU and Fans if
                                    // room air model not used
        Real64 AirVolFlowStdDensity; // Air volume flow rate at standard density [m3/s]
        Real64 AirVolFlowCurDensity; // Air volume flow rate at current density [m3/s]
        Real64 AirMassFlow;          // Air mass flow rate [kg/s]
        Real64 AirInletDryBulbT;     // Air inlet dry-bulb temperature [C]
        Real64 AirInletDewpointT;    // Air inlet dewpoint temperature [C]
        Real64 AirInletRelHum;       // Air inlet relative humidity [%]
        Real64 AirOutletDryBulbT;    // Air outlet dry-bulb temperature [C]
        Real64 SHI;                  // Supply Heat Index []
        Real64 TimeOutOfOperRange;   // ITE Air Inlet Operating Range Exceeded Time [hr]
        Real64 TimeAboveDryBulbT;    // ITE Air Inlet Dry-Bulb Temperature Above Operating Range Time [hr]
        Real64 TimeBelowDryBulbT;    // ITE Air Inlet Dry-Bulb Temperature Below Operating Range Time [hr]
        Real64 TimeAboveDewpointT;   // ITE Air Inlet Dewpoint Temperature Above Operating Range Time [hr]
        Real64 TimeBelowDewpointT;   // ITE Air Inlet Dewpoint Temperature Below Operating Range Time [hr]
        Real64 TimeAboveRH;          // ITE Air Inlet Relative Humidity Above Operating Range Time [hr]
        Real64 TimeBelowRH;          // ITE Air Inlet Relative Humidity Below Operating Range Time [hr]
        Real64 DryBulbTAboveDeltaT;  // ITE Air Inlet Dry-Bulb Temperature Difference Above Operating Range [deltaC]
        Real64 DryBulbTBelowDeltaT;  // ITE Air Inlet Dry-Bulb Temperature Difference Below Operating Range [deltaC]
        Real64 DewpointTAboveDeltaT; // ITE Air Inlet Dewpoint Temperature Difference Above Operating Range [deltaC]
        Real64 DewpointTBelowDeltaT; // ITE Air Inlet Dewpoint Temperature Difference Below Operating Range [deltaC]
        Real64 RHAboveDeltaRH;       // ITE Air Inlet Relative Humidity Difference Above Operating Range [%]
        Real64 RHBelowDeltaRH;       // ITE Air Inlet Relative Humidity Difference Below Operating Range [%]

        // Default Constructor
        ITEquipData()
            : ZonePtr(0), FlowControlWithApproachTemps(false), DesignTotalPower(0.0), NomMinDesignLevel(0.0), NomMaxDesignLevel(0.0),
              DesignFanPowerFrac(0.0), OperSchedPtr(0), CPULoadSchedPtr(0), SizingTAirIn(0.0), DesignTAirIn(0.0), DesignFanPower(0.0),
              DesignCPUPower(0.0), DesignAirVolFlowRate(0.0), Class(0), AirFlowFLTCurve(0), CPUPowerFLTCurve(0), FanPowerFFCurve(0),
              AirConnectionType(0), InletRoomAirNodeNum(0), OutletRoomAirNodeNum(0), SupplyAirNodeNum(0), DesignRecircFrac(0.0), RecircFLTCurve(0),
              DesignUPSEfficiency(0.0), UPSEfficFPLRCurve(0), UPSLossToZoneFrac(0.0), EMSCPUPowerOverrideOn(false), EMSCPUPower(0.0),
              EMSFanPowerOverrideOn(false), EMSFanPower(0.0), EMSUPSPowerOverrideOn(false), EMSUPSPower(0.0), SupplyApproachTemp(0.0),
              SupplyApproachTempSch(0), ReturnApproachTemp(0.0), ReturnApproachTempSch(0), CPUPower(0.0), FanPower(0.0), UPSPower(0.0),
              CPUPowerAtDesign(0.0), FanPowerAtDesign(0.0), UPSGainRateToZone(0.0), ConGainRateToZone(0.0), CPUConsumption(0.0), FanConsumption(0.0),
              UPSConsumption(0.0), CPUEnergyAtDesign(0.0), FanEnergyAtDesign(0.0), UPSGainEnergyToZone(0.0), ConGainEnergyToZone(0.0),
              AirVolFlowStdDensity(0.0), AirVolFlowCurDensity(0.0), AirMassFlow(0.0), AirInletDryBulbT(0.0), AirInletDewpointT(0.0),
              AirInletRelHum(0.0), AirOutletDryBulbT(0.0), SHI(0.0), TimeOutOfOperRange(0.0), TimeAboveDryBulbT(0.0), TimeBelowDryBulbT(0.0),
              TimeAboveDewpointT(0.0), TimeBelowDewpointT(0.0), TimeAboveRH(0.0), TimeBelowRH(0.0), DryBulbTAboveDeltaT(0.0),
              DryBulbTBelowDeltaT(0.0), DewpointTAboveDeltaT(0.0), DewpointTBelowDeltaT(0.0), RHAboveDeltaRH(0.0), RHBelowDeltaRH(0.0)
        {
        }
    };

    struct BBHeatData
    {
        // Members
        std::string Name; // BASEBOARD HEAT object name
        int ZonePtr;
        int SchedPtr;
        Real64 CapatLowTemperature;
        Real64 LowTemperature;
        Real64 CapatHighTemperature;
        Real64 HighTemperature;
        bool EMSZoneBaseboardOverrideOn; // EMS actuating equipment power if .TRUE.
        Real64 EMSZoneBaseboardPower;    // Value EMS is directing to use for override
        Real64 FractionRadiant;
        Real64 FractionConvected;
        bool ManageDemand;  // Flag to indicate whether to use demand limiting
        Real64 DemandLimit; // Demand limit set by demand manager [W]
        // Report variables
        Real64 Power;                  // Electric power [W]
        Real64 RadGainRate;            // Radiant heat gain [W]
        Real64 ConGainRate;            // Convective heat gain [W]
        Real64 TotGainRate;            // Total heat gain [W]
        Real64 Consumption;            // Electric consumption [J]
        Real64 RadGainEnergy;          // Radiant heat gain [J]
        Real64 ConGainEnergy;          // Convective heat gain [J]
        Real64 TotGainEnergy;          // Total heat gain [J]
        std::string EndUseSubcategory; // user defined name for the end use category

        // Default Constructor
        BBHeatData()
            : ZonePtr(0), SchedPtr(0), CapatLowTemperature(0.0), LowTemperature(0.0), CapatHighTemperature(0.0), HighTemperature(0.0),
              EMSZoneBaseboardOverrideOn(false), EMSZoneBaseboardPower(0.0), FractionRadiant(0.0), FractionConvected(0.0), ManageDemand(false),
              DemandLimit(0.0), Power(0.0), RadGainRate(0.0), ConGainRate(0.0), TotGainRate(0.0), Consumption(0.0), RadGainEnergy(0.0),
              ConGainEnergy(0.0), TotGainEnergy(0.0)
        {
        }
    };

    struct InfiltrationData
    {
        // Members
        std::string Name;
        int ZonePtr;   // Which zone infiltration is in
        int SchedPtr;  // Schedule for infiltration
        int ModelType; // which model is used for infiltration
        // Design Flow Rate model terms
        Real64 DesignLevel;
        Real64 ConstantTermCoef;
        Real64 TemperatureTermCoef;
        Real64 VelocityTermCoef;
        Real64 VelocitySQTermCoef;
        // Effective Leakage Area, Sherman Grimsrud terms
        Real64 LeakageArea;           // "AL" effective air leakage area
        Real64 BasicStackCoefficient; // "Cs" Stack coefficient
        Real64 BasicWindCoefficient;  // "Cw" wind coefficient
        // Flow Coefficient, AIM-2, Walker and Wilson terms
        Real64 FlowCoefficient;      // "c" Flow coefficient
        Real64 AIM2StackCoefficient; // "Cs" stack coefficient
        Real64 AIM2WindCoefficient;  // "Cw" wind coefficient
        Real64 PressureExponent;     // "n" pressure power law exponent
        Real64 ShelterFactor;        // "s" shelter factor
        bool EMSOverrideOn;          // if true then EMS is requesting to override
        Real64 EMSAirFlowRateValue;  // value EMS is setting for air flow rate
        bool QuadratureSum;          // If quadrature sum of zone air balance method is used
        int OABalancePtr;            // A pointer to ZoneAirBalance If quadrature is true
        Real64 VolumeFlowRate;       // infiltration air volume flow rate
        Real64 MassFlowRate;         // infiltration air mass flow rate

        // Default Constructor
        InfiltrationData()
            : ZonePtr(0), SchedPtr(0), ModelType(0), DesignLevel(0.0), ConstantTermCoef(0.0), TemperatureTermCoef(0.0), VelocityTermCoef(0.0),
              VelocitySQTermCoef(0.0), LeakageArea(0.0), BasicStackCoefficient(0.0), BasicWindCoefficient(0.0), FlowCoefficient(0.0),
              AIM2StackCoefficient(0.0), AIM2WindCoefficient(0.0), PressureExponent(0.0), ShelterFactor(0.0), EMSOverrideOn(false),
              EMSAirFlowRateValue(0.0), QuadratureSum(false), OABalancePtr(0), VolumeFlowRate(0.0), MassFlowRate(0.0)
        {
        }
    };

    struct VentilationData
    {
        // Members
        std::string Name;
        int ZonePtr;
        int SchedPtr;
        int ModelType; // which model is used for ventilation: DesignFlowRate and WindandStackOpenArea
        Real64 DesignLevel;
        bool EMSSimpleVentOn;        // EMS actuating ventilation flow rate if .TRUE.
        Real64 EMSimpleVentFlowRate; // Value EMS is directing to use for override
        Real64 MinIndoorTemperature;
        Real64 DelTemperature;
        int FanType;
        Real64 FanPressure;
        Real64 FanEfficiency;
        Real64 FanPower;
        Real64 AirTemp;
        Real64 ConstantTermCoef;
        Real64 TemperatureTermCoef;
        Real64 VelocityTermCoef;
        Real64 VelocitySQTermCoef;
        Real64 MaxIndoorTemperature;
        Real64 MinOutdoorTemperature;
        Real64 MaxOutdoorTemperature;
        Real64 MaxWindSpeed;
        int MinIndoorTempSchedPtr;      // Minimum indoor temperature schedule index
        int MaxIndoorTempSchedPtr;      // Maximum indoor temperature schedule index
        int DeltaTempSchedPtr;          // Delta temperature schedule index
        int MinOutdoorTempSchedPtr;     // Minimum outdoor temperature schedule index
        int MaxOutdoorTempSchedPtr;     // Maximum outdoor temperature schedule index
        int IndoorTempErrCount;         // Indoor temperature error count
        int OutdoorTempErrCount;        // Outdoor temperature error count
        int IndoorTempErrIndex;         // Indoor temperature error Index
        int OutdoorTempErrIndex;        // Outdoor temperature error Index
        int HybridControlType;          // Hybrid ventilation control type: 0 Individual, 1 Close, 2 Global
        int HybridControlMasterNum;     // Hybrid ventilation control master object number
        bool HybridControlMasterStatus; // Hybrid ventilation control master object opening status
        bool QuadratureSum;             // If quadrature sum of zone air balance method is used
        int OABalancePtr;               // A pointer to ZoneAirBalance
        // WindandStackOpenArea
        Real64 OpenArea;      // Opening area [m2]
        int OpenAreaSchedPtr; // Opening area fraction schedule pointer
        Real64 OpenEff;       // Opening effectiveness [dimensionless]
        Real64 EffAngle;      // Effective angle [degree]
        Real64 DH;            // Height difference [m]
        Real64 DiscCoef;      // Discharge coefficient

        // Default Constructor
        VentilationData()
            : ZonePtr(0), SchedPtr(0), ModelType(0), DesignLevel(0.0), EMSSimpleVentOn(false), EMSimpleVentFlowRate(0.0),
              MinIndoorTemperature(-100.0), DelTemperature(0.0), FanType(0), FanPressure(0.0), FanEfficiency(0.0), FanPower(0.0), AirTemp(0.0),
              ConstantTermCoef(0.0), TemperatureTermCoef(0.0), VelocityTermCoef(0.0), VelocitySQTermCoef(0.0), MaxIndoorTemperature(100.0),
              MinOutdoorTemperature(-100.0), MaxOutdoorTemperature(100.0), MaxWindSpeed(40.0), MinIndoorTempSchedPtr(0), MaxIndoorTempSchedPtr(0),
              DeltaTempSchedPtr(0), MinOutdoorTempSchedPtr(0), MaxOutdoorTempSchedPtr(0), IndoorTempErrCount(0), OutdoorTempErrCount(0),
              IndoorTempErrIndex(0), OutdoorTempErrIndex(0), HybridControlType(0), HybridControlMasterNum(0), HybridControlMasterStatus(false),
              QuadratureSum(false), OABalancePtr(0), OpenArea(0.0), OpenAreaSchedPtr(0), OpenEff(0.0), EffAngle(0.0), DH(0.0), DiscCoef(0.0)
        {
        }
    };

    struct ZoneAirBalanceData
    {
        // Members
        std::string Name;           // Object name
        std::string ZoneName;       // Zone name
        int ZonePtr;                // Zone number
        int BalanceMethod;          // Air Balance Method: None=0, Quadrature = 1
        Real64 InducedAirRate;      // Induced Outdoor Air Due to Duct Leakage Unbalance [m3/s]
        int InducedAirSchedPtr;     // Induced Outdoor Air Fraction Schedule
        Real64 BalMassFlowRate;     // balanced mass flow rate
        Real64 InfMassFlowRate;     // unbalanced mass flow rate from infiltration
        Real64 NatMassFlowRate;     // unbalanced mass flow rate from natural ventilation
        Real64 ExhMassFlowRate;     // unbalanced mass flow rate from exhaust ventilation
        Real64 IntMassFlowRate;     // unbalanced mass flow rate from intake ventilation
        Real64 ERVMassFlowRate;     // unbalanced mass flow rate from stand-alone ERV
        bool OneTimeFlag;           // One time flag to get nodes of stand alone ERV
        int NumOfERVs;              // Number of zone stand alone ERVs
        Array1D_int ERVInletNode;   // Stand alone ERV supply air inlet nodes
        Array1D_int ERVExhaustNode; // Stand alone ERV air exhaust nodes

        // Default Constructor
        ZoneAirBalanceData()
            : ZonePtr(0), BalanceMethod(0), InducedAirRate(0.0), InducedAirSchedPtr(0), BalMassFlowRate(0.0), InfMassFlowRate(0.0),
              NatMassFlowRate(0.0), ExhMassFlowRate(0.0), IntMassFlowRate(0.0), ERVMassFlowRate(0.0), OneTimeFlag(false), NumOfERVs(0)
        {
        }
    };

    struct MixingData
    {
        // Members
        std::string Name;
        int ZonePtr;
        int SchedPtr;
        Real64 DesignLevel;
        int FromZone;
        Real64 DeltaTemperature;
        Real64 DesiredAirFlowRate;
        Real64 DesiredAirFlowRateSaved;
        Real64 MixingMassFlowRate;
        int DeltaTempSchedPtr;      // Delta temperature schedule index
        int MinIndoorTempSchedPtr;  // Minimum indoor temperature schedule index
        int MaxIndoorTempSchedPtr;  // Maximum indoor temperature schedule index
        int MinSourceTempSchedPtr;  // Minimum source zone temperature schedule index
        int MaxSourceTempSchedPtr;  // Maximum source zone temperature schedule index
        int MinOutdoorTempSchedPtr; // Minimum outdoor temperature schedule index
        int MaxOutdoorTempSchedPtr; // Maximum outdoor temperature schedule index
        int IndoorTempErrCount;     // Indoor temperature error count
        int SourceTempErrCount;     // Source zone temperature error count
        int OutdoorTempErrCount;    // Outdoor temperature error count
        int IndoorTempErrIndex;     // Indoor temperature error Index
        int SourceTempErrIndex;     // Source zone temperature error Index
        int OutdoorTempErrIndex;    // Outdoor temperature error Index
        int HybridControlType;      // Hybrid ventilation control type: 0 Individual, 1 Close, 2 Global
        int HybridControlMasterNum; // Hybrid ventilation control master ventilation object number
        int NumRefDoorConnections;
        bool EMSSimpleMixingOn;        // EMS actuating ventilation flow rate if .TRUE.
        bool RefDoorMixFlag;           // Refrigeration door mixing within zone
        Real64 EMSimpleMixingFlowRate; // Value EMS is directing to use for override
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

        // Default Constructor
        MixingData()
            : ZonePtr(0), SchedPtr(0), DesignLevel(0.0), FromZone(0), DeltaTemperature(0.0), DesiredAirFlowRate(0.0), DesiredAirFlowRateSaved(0.0),
              MixingMassFlowRate(0.0), DeltaTempSchedPtr(0), MinIndoorTempSchedPtr(0), MaxIndoorTempSchedPtr(0), MinSourceTempSchedPtr(0),
              MaxSourceTempSchedPtr(0), MinOutdoorTempSchedPtr(0), MaxOutdoorTempSchedPtr(0), IndoorTempErrCount(0), SourceTempErrCount(0),
              OutdoorTempErrCount(0), IndoorTempErrIndex(0), SourceTempErrIndex(0), OutdoorTempErrIndex(0), HybridControlType(0),
              HybridControlMasterNum(0), NumRefDoorConnections(0), EMSSimpleMixingOn(false), RefDoorMixFlag(false), EMSimpleMixingFlowRate(0.0)
        {
        }
    };

    struct ZoneAirMassFlowConservation
    {
        // Members
        bool EnforceZoneMassBalance;       // flag to enforce zone air mass conservation
        AdjustmentType ZoneFlowAdjustment; // determines how zone air flow is adjusted (AdjustMixingOnly, AdjustReturnOnly, AdjustMixingThenReturn,
                                           // AdjustReturnThenMixing, None)        int InfiltrationTreatment;   // determines how infiltration is
                                           // treated for zone mass balance
        int InfiltrationTreatment;         // determines how infiltration is treated for zone mass balance
        int InfiltrationZoneType;          // specifies which types of zones allow infiltration to be changed
        bool AdjustZoneMixingFlow;         // used to adjust zone mixing air flows to enforce air flow balance
        bool AdjustZoneInfiltrationFlow;   // used to adjust zone infiltration air flows to enforce air flow balance
                                           // Note, unique global object

        // Default Constructor
        ZoneAirMassFlowConservation()
            : EnforceZoneMassBalance(false), ZoneFlowAdjustment(AdjustmentType::NoAdjustReturnAndMixing), InfiltrationTreatment(0),
              InfiltrationZoneType(0), AdjustZoneMixingFlow(false), AdjustZoneInfiltrationFlow(false)
        {
        }
    };

    struct ZoneMassConservationData
    {
        // Members
        std::string Name;
        int ZonePtr;                           // pointer to the mixing zone
        Real64 InMassFlowRate;                 // zone total supply air mass flow rate, kg/s
        Real64 ExhMassFlowRate;                // zone exhaust total air mass flow rate, kg/s
        Real64 RetMassFlowRate;                // zone return air mass flow rate, kg/s
        Real64 MixingMassFlowRate;             // zone mixing air mass flow rate, kg/s
        Real64 MixingSourceMassFlowRate;       // Zone source mass flow rate for mixing zone, kg/s
        int NumSourceZonesMixingObject;        // number of zone mixing object references as a source zone
        int NumReceivingZonesMixingObject;     // number of zone mixing object references as a receiving zone
        bool IsOnlySourceZone;                 // true only if used only as a source zone in zone mixing object
        bool IsSourceAndReceivingZone;         // true only if a zone is used as a source and receiving zone in zone mixing objects
        int InfiltrationPtr;                   // pointer to infiltration object
        Real64 InfiltrationMassFlowRate;       // infiltration added to enforced source zone mass balance, kg/s
        int IncludeInfilToZoneMassBal;         // not self-balanced, include infiltration in zone air mass balance
        Array1D_int ZoneMixingSourcesPtr;      // source zones pointer
        Array1D_int ZoneMixingReceivingPtr;    // receiving zones pointer
        Array1D<Real64> ZoneMixingReceivingFr; // receiving zones fraction
                                               // Note, this type dimensioned by number of zones

        // Default Constructor
        ZoneMassConservationData()
            : ZonePtr(0), InMassFlowRate(0.0), ExhMassFlowRate(0.0), RetMassFlowRate(0.0), MixingMassFlowRate(0.0), MixingSourceMassFlowRate(0.0),
              NumSourceZonesMixingObject(0), NumReceivingZonesMixingObject(0), IsOnlySourceZone(false), IsSourceAndReceivingZone(false),
              InfiltrationPtr(0), InfiltrationMassFlowRate(0.0), IncludeInfilToZoneMassBal(0)
        {
        }
    };

    struct GenericComponentZoneIntGainStruct
    {
        // Members
        std::string CompObjectType;         // device object class name
        std::string CompObjectName;         // device user unique name
        int CompTypeOfNum;                  // type of internal gain device identifier
        Real64 *PtrConvectGainRate;         // POINTER to value of convection heat gain rate for device, watts
        Real64 ConvectGainRate;             // current timestep value of convection heat gain rate for device, watts
        Real64 *PtrReturnAirConvGainRate;   // POINTER to value of return air convection heat gain rate for device, W
        Real64 ReturnAirConvGainRate;       // current timestep value of return air convection heat gain rate for device, W
        Real64 *PtrRadiantGainRate;         // POINTER to value of thermal radiation heat gain rate for device, watts
        Real64 RadiantGainRate;             // current timestep value of thermal radiation heat gain rate for device, watts
        Real64 *PtrLatentGainRate;          // POINTER to value of moisture gain rate for device, Watts
        Real64 LatentGainRate;              // current timestep value of moisture gain rate for device, Watts
        Real64 *PtrReturnAirLatentGainRate; // POINTER to value of return air moisture gain rate for device, Watts
        Real64 ReturnAirLatentGainRate;     // current timestep value of return air moisture gain rate for device, Watts
        Real64 *PtrCarbonDioxideGainRate;   // POINTER to value of carbon dioxide gain rate for device
        Real64 CarbonDioxideGainRate;       // current timestep value of carbon dioxide gain rate for device
        Real64 *PtrGenericContamGainRate;   // POINTER to value of generic contaminant gain rate for device
        Real64 GenericContamGainRate;       // current timestep value of generic contaminant gain rate for device
        int ReturnAirNodeNum;               // return air node number for retrun air convection heat gain

        // Default Constructor
        GenericComponentZoneIntGainStruct()
            : CompTypeOfNum(0), PtrConvectGainRate(nullptr), ConvectGainRate(0.0), PtrReturnAirConvGainRate(nullptr), ReturnAirConvGainRate(0.0),
              PtrRadiantGainRate(nullptr), RadiantGainRate(0.0), PtrLatentGainRate(nullptr), LatentGainRate(0.0), PtrReturnAirLatentGainRate(nullptr),
              ReturnAirLatentGainRate(0.0), PtrCarbonDioxideGainRate(nullptr), CarbonDioxideGainRate(0.0), PtrGenericContamGainRate(nullptr),
              GenericContamGainRate(0.0), ReturnAirNodeNum(0)
        {
        }
    };

    struct ZoneSimData // Calculated data by Zone during each time step/hour
    {
        // Members
        Real64 NOFOCC;  // Number of Occupants, zone total
        Real64 QOCTOT;  // Total Energy from Occupants
        Real64 QOCSEN;  // Sensible Energy from Occupants
        Real64 QOCCON;  // ENERGY CONVECTED FROM OCCUPANTS (WH)
        Real64 QOCRAD;  // ENERGY RADIATED FROM OCCUPANTS
        Real64 QOCLAT;  // LATENT ENERGY FROM OCCUPANTS
        Real64 QLTTOT;  // TOTAL ENERGY INTO LIGHTS (WH)
        Real64 QLTCON;  // ENERGY CONVECTED TO SPACE AIR FROM LIGHTS
        Real64 QLTRAD;  // ENERGY RADIATED TO SPACE FROM LIGHTS
        Real64 QLTCRA;  // ENERGY CONVECTED TO RETURN AIR FROM LIGHTS
        Real64 QLTSW;   // VISIBLE ENERGY FROM LIGHTS
        Real64 QEECON;  // ENERGY CONVECTED FROM ELECTRIC EQUIPMENT
        Real64 QEERAD;  // ENERGY RADIATED FROM ELECTRIC EQUIPMENT
        Real64 QEELost; // Energy from Electric Equipment (lost)
        Real64 QEELAT;  // LATENT ENERGY FROM Electric Equipment
        Real64 QGECON;  // ENERGY CONVECTED FROM GAS EQUIPMENT
        Real64 QGERAD;  // ENERGY RADIATED FROM GAS EQUIPMENT
        Real64 QGELost; // Energy from Gas Equipment (lost)
        Real64 QGELAT;  // LATENT ENERGY FROM Gas Equipment
        Real64 QOECON;  // ENERGY CONVECTED FROM OTHER EQUIPMENT
        Real64 QOERAD;  // ENERGY RADIATED FROM OTHER EQUIPMENT
        Real64 QOELost; // Energy from Other Equipment (lost)
        Real64 QOELAT;  // LATENT ENERGY FROM Other Equipment
        Real64 QHWCON;  // ENERGY CONVECTED FROM Hot Water EQUIPMENT
        Real64 QHWRAD;  // ENERGY RADIATED FROM Hot Water EQUIPMENT
        Real64 QHWLost; // Energy from Hot Water Equipment (lost)
        Real64 QHWLAT;  // LATENT ENERGY FROM Hot Water Equipment
        Real64 QSECON;  // ENERGY CONVECTED FROM Steam EQUIPMENT
        Real64 QSERAD;  // ENERGY RADIATED FROM Steam EQUIPMENT
        Real64 QSELost; // Energy from Steam Equipment (lost)
        Real64 QSELAT;  // LATENT ENERGY FROM Steam Equipment
        Real64 QBBCON;  // ENERGY CONVECTED FROM BASEBOARD HEATING
        Real64 QBBRAD;  // ENERGY RADIATED FROM BASEBOARD HEATING
        int NumberOfDevices;
        int MaxNumberOfDevices;
        Array1D<GenericComponentZoneIntGainStruct> Device;

        // Default Constructor
        ZoneSimData()
            : NOFOCC(0.0), QOCTOT(0.0), QOCSEN(0.0), QOCCON(0.0), QOCRAD(0.0), QOCLAT(0.0), QLTTOT(0.0), QLTCON(0.0), QLTRAD(0.0), QLTCRA(0.0),
              QLTSW(0.0), QEECON(0.0), QEERAD(0.0), QEELost(0.0), QEELAT(0.0), QGECON(0.0), QGERAD(0.0), QGELost(0.0), QGELAT(0.0), QOECON(0.0),
              QOERAD(0.0), QOELost(0.0), QOELAT(0.0), QHWCON(0.0), QHWRAD(0.0), QHWLost(0.0), QHWLAT(0.0), QSECON(0.0), QSERAD(0.0), QSELost(0.0),
              QSELAT(0.0), QBBCON(0.0), QBBRAD(0.0), NumberOfDevices(0), MaxNumberOfDevices(0)
        {
        }
    };

    struct WindowBlindProperties
    {
        // Members
        std::string Name;
        int MaterialNumber; // Material pointer for the blind
        // Input properties
        int SlatOrientation;     // HORIZONTAL or VERTICAL
        int SlatAngleType;       // FIXED or VARIABLE
        Real64 SlatWidth;        // Slat width (m)
        Real64 SlatSeparation;   // Slat separation (m)
        Real64 SlatThickness;    // Slat thickness (m)
        Real64 SlatCrown;        // the height of the slate (length from the chord to the curve)
        Real64 SlatAngle;        // Slat angle (deg)
        Real64 MinSlatAngle;     // Minimum slat angle for variable-angle slats (deg) (user input)
        Real64 MaxSlatAngle;     // Maximum slat angle for variable-angle slats (deg) (user input)
        Real64 SlatConductivity; // Slat conductivity (W/m-K)
        // Solar slat properties
        Real64 SlatTransSolBeamDiff;     // Slat solar beam-diffuse transmittance
        Real64 SlatFrontReflSolBeamDiff; // Slat front solar beam-diffuse reflectance
        Real64 SlatBackReflSolBeamDiff;  // Slat back solar beam-diffuse reflectance
        Real64 SlatTransSolDiffDiff;     // Slat solar diffuse-diffuse transmittance
        Real64 SlatFrontReflSolDiffDiff; // Slat front solar diffuse-diffuse reflectance
        Real64 SlatBackReflSolDiffDiff;  // Slat back solar diffuse-diffuse reflectance
        // Visible slat properties
        Real64 SlatTransVisBeamDiff;     // Slat visible beam-diffuse transmittance
        Real64 SlatFrontReflVisBeamDiff; // Slat front visible beam-diffuse reflectance
        Real64 SlatBackReflVisBeamDiff;  // Slat back visible beam-diffuse reflectance
        Real64 SlatTransVisDiffDiff;     // Slat visible diffuse-diffuse transmittance
        Real64 SlatFrontReflVisDiffDiff; // Slat front visible diffuse-diffuse reflectance
        Real64 SlatBackReflVisDiffDiff;  // Slat back visible diffuse-diffuse reflectance
        // Long-wave (IR) slat properties
        Real64 SlatTransIR;      // Slat IR transmittance
        Real64 SlatFrontEmissIR; // Slat front emissivity
        Real64 SlatBackEmissIR;  // Slat back emissivity
        // Some characteristics for blind thermal calculation
        Real64 BlindToGlassDist;    // Distance between window shade and adjacent glass (m)
        Real64 BlindTopOpeningMult; // Area of air-flow opening at top of blind, expressed as a fraction
        //  of the blind-to-glass opening area at the top of the blind
        Real64 BlindBottomOpeningMult; // Area of air-flow opening at bottom of blind, expressed as a fraction
        //  of the blind-to-glass opening area at the bottom of the blind
        Real64 BlindLeftOpeningMult; // Area of air-flow opening at left side of blind, expressed as a fraction
        //  of the blind-to-glass opening area at the left side of the blind
        Real64 BlindRightOpeningMult; // Area of air-flow opening at right side of blind, expressed as a fraction
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
            : MaterialNumber(0), SlatOrientation(0), SlatAngleType(FixedSlats), SlatWidth(0.0), SlatSeparation(0.0), SlatThickness(0.0),
              SlatCrown(0.0), SlatAngle(0.0), MinSlatAngle(0.0), MaxSlatAngle(0.0), SlatConductivity(0.0), SlatTransSolBeamDiff(0.0),
              SlatFrontReflSolBeamDiff(0.0), SlatBackReflSolBeamDiff(0.0), SlatTransSolDiffDiff(0.0), SlatFrontReflSolDiffDiff(0.0),
              SlatBackReflSolDiffDiff(0.0), SlatTransVisBeamDiff(0.0), SlatFrontReflVisBeamDiff(0.0), SlatBackReflVisBeamDiff(0.0),
              SlatTransVisDiffDiff(0.0), SlatFrontReflVisDiffDiff(0.0), SlatBackReflVisDiffDiff(0.0), SlatTransIR(0.0), SlatFrontEmissIR(0.0),
              SlatBackEmissIR(0.0), BlindToGlassDist(0.0), BlindTopOpeningMult(0.0), BlindBottomOpeningMult(0.0), BlindLeftOpeningMult(0.0),
              BlindRightOpeningMult(0.0), SolFrontBeamBeamTrans(MaxSlatAngs, 37, 0.0), SolFrontBeamBeamRefl(MaxSlatAngs, 37, 0.0),
              SolBackBeamBeamTrans(MaxSlatAngs, 37, 0.0), SolBackBeamBeamRefl(MaxSlatAngs, 37, 0.0), SolFrontBeamDiffTrans(MaxSlatAngs, 37, 0.0),
              SolFrontBeamDiffRefl(MaxSlatAngs, 37, 0.0), SolBackBeamDiffTrans(MaxSlatAngs, 37, 0.0), SolBackBeamDiffRefl(MaxSlatAngs, 37, 0.0),
              SolFrontDiffDiffTrans(MaxSlatAngs, 0.0), SolFrontDiffDiffTransGnd(MaxSlatAngs, 0.0), SolFrontDiffDiffTransSky(MaxSlatAngs, 0.0),
              SolFrontDiffDiffRefl(MaxSlatAngs, 0.0), SolFrontDiffDiffReflGnd(MaxSlatAngs, 0.0), SolFrontDiffDiffReflSky(MaxSlatAngs, 0.0),
              SolBackDiffDiffTrans(MaxSlatAngs, 0.0), SolBackDiffDiffRefl(MaxSlatAngs, 0.0), SolFrontBeamAbs(MaxSlatAngs, 37, 0.0),
              SolBackBeamAbs(MaxSlatAngs, 37, 0.0), SolFrontDiffAbs(MaxSlatAngs, 0.0), SolFrontDiffAbsGnd(MaxSlatAngs, 0.0),
              SolFrontDiffAbsSky(MaxSlatAngs, 0.0), SolBackDiffAbs(MaxSlatAngs, 0.0), VisFrontBeamBeamTrans(MaxSlatAngs, 37, 0.0),
              VisFrontBeamBeamRefl(MaxSlatAngs, 37, 0.0), VisBackBeamBeamTrans(MaxSlatAngs, 37, 0.0), VisBackBeamBeamRefl(MaxSlatAngs, 37, 0.0),
              VisFrontBeamDiffTrans(MaxSlatAngs, 37, 0.0), VisFrontBeamDiffRefl(MaxSlatAngs, 37, 0.0), VisBackBeamDiffTrans(MaxSlatAngs, 37, 0.0),
              VisBackBeamDiffRefl(MaxSlatAngs, 37, 0.0), VisFrontDiffDiffTrans(MaxSlatAngs, 0.0), VisFrontDiffDiffRefl(MaxSlatAngs, 0.0),
              VisBackDiffDiffTrans(MaxSlatAngs, 0.0), VisBackDiffDiffRefl(MaxSlatAngs, 0.0), IRFrontTrans(MaxSlatAngs, 0.0),
              IRFrontEmiss(MaxSlatAngs, 0.0), IRBackTrans(MaxSlatAngs, 0.0), IRBackEmiss(MaxSlatAngs, 0.0)
        {
        }
    };

    struct SurfaceScreenProperties
    {
        // Members
        int MaterialNumber; // Material pointer for the screen
        Real64 BmBmTrans;   // Beam solar transmittance (dependent on sun angle)
        // (this value can include scattering if the user so chooses)
        Real64 BmBmTransBack; // Beam solar transmittance (dependent on sun angle) from back side of screen
        Real64 BmBmTransVis;  // Visible solar transmittance (dependent on sun angle)
        // (this value can include visible scattering if the user so chooses)
        Real64 BmDifTrans;     // Beam solar transmitted as diffuse radiation (dependent on sun angle)
        Real64 BmDifTransBack; // Beam solar transmitted as diffuse radiation (dependent on sun angle) from back side
        Real64 BmDifTransVis;  // Visible solar transmitted as diffuse radiation (dependent on sun angle)
        // The following reflectance properties are dependent on sun angle:
        Real64 ReflectSolBeamFront;          // Beam solar reflected as diffuse radiation when sun is in front of screen
        Real64 ReflectVisBeamFront;          // Visible solar reflected as diffuse radiation when sun is in front of screen
        Real64 ReflectSolBeamBack;           // Beam solar reflected as diffuse radiation when sun is in back of screen
        Real64 ReflectVisBeamBack;           // Visible solar reflected as diffuse radiation when sun is in back of screen
        Real64 AbsorpSolarBeamFront;         // Front surface solar beam absorptance
        Real64 AbsorpSolarBeamBack;          // Back surface solar beam absorptance
        Real64 DifDifTrans;                  // Back surface diffuse solar transmitted
        Real64 DifDifTransVis;               // Back surface diffuse visible solar transmitted
        Real64 DifScreenAbsorp;              // Absorption of diffuse radiation
        Real64 DifReflect;                   // Back reflection of solar diffuse radiation
        Real64 DifReflectVis;                // Back reflection of visible diffuse radiation
        Real64 ReflectScreen;                // Screen assembly solar reflectance (user input adjusted for holes in screen)
        Real64 ReflectScreenVis;             // Screen assembly visible reflectance (user input adjusted for holes in screen)
        Real64 ReflectCylinder;              // Screen material solar reflectance (user input, does not account for holes in screen)
        Real64 ReflectCylinderVis;           // Screen material visible reflectance (user input, does not account for holes in screen)
        Real64 ScreenDiameterToSpacingRatio; // ratio of screen material diameter to screen material spacing
        int ScreenBeamReflectanceAccounting; // user specified method of accounting for scattered solar beam

        // Default Constructor
        SurfaceScreenProperties()
            : MaterialNumber(0), BmBmTrans(0.0), BmBmTransBack(0.0), BmBmTransVis(0.0), BmDifTrans(0.0), BmDifTransBack(0.0), BmDifTransVis(0.0),
              ReflectSolBeamFront(0.0), ReflectVisBeamFront(0.0), ReflectSolBeamBack(0.0), ReflectVisBeamBack(0.0), AbsorpSolarBeamFront(0.0),
              AbsorpSolarBeamBack(0.0), DifDifTrans(0.0), DifDifTransVis(0.0), DifScreenAbsorp(0.0), DifReflect(0.0), DifReflectVis(0.0),
              ReflectScreen(0.0), ReflectScreenVis(0.0), ReflectCylinder(0.0), ReflectCylinderVis(0.0), ScreenDiameterToSpacingRatio(0.0),
              ScreenBeamReflectanceAccounting(0)
        {
        }
    };

    struct ScreenTransData
    {
        // Members
        Array2D<Real64> Trans;
        Array2D<Real64> Scatt;

        // Default Constructor
        ScreenTransData() = default;
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
        Real64 SenCaseCreditToZone; // Refrigerated display case sensible energy delivered to zone
        // includes refrigeration pipe and receiver heat exchange with zone
        Real64 LatCaseCreditToZone; // Refrigerated display case latent energy delivered to zone
        Real64 SenCaseCreditToHVAC; // Refrigerated display case sensible energy delivered to HVAC RA duct
        Real64 LatCaseCreditToHVAC; // Refrigerated display case latent energy delivered to HVAC RA duct

        // Default Constructor
        RefrigCaseCreditData() : SenCaseCreditToZone(0.0), LatCaseCreditToZone(0.0), SenCaseCreditToHVAC(0.0), LatCaseCreditToHVAC(0.0)
        {
        }

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
        std::string Name;                                       // Name of Coil
        std::string SourceType;                                 // SourceType for Coil
        Real64 AvailCapacity;                                   // Total available heat reclaim capacity
        Real64 ReclaimEfficiencyTotal;                          // Total reclaimed portion
        Real64 WaterHeatingDesuperheaterReclaimedHeatTotal;     // total reclaimed heat by water heating desuperheater coils
        Real64 HVACDesuperheaterReclaimedHeatTotal;             // total reclaimed heat by water heating desuperheater coils
        Array1D<Real64> WaterHeatingDesuperheaterReclaimedHeat; // heat reclaimed by water heating desuperheater coils
        Array1D<Real64> HVACDesuperheaterReclaimedHeat;         // heat reclaimed by water heating desuperheater coils

        // Default Constructor
        HeatReclaimDataBase()
            : AvailCapacity(0.0), ReclaimEfficiencyTotal(0.0), WaterHeatingDesuperheaterReclaimedHeatTotal(0.0),
              HVACDesuperheaterReclaimedHeatTotal(0.0)
        {
        }
    };

    struct HeatReclaimRefrigCondenserData : HeatReclaimDataBase // inherited from base struct
    {
        // Customized Members
        Real64 AvailTemperature; // Temperature of heat reclaim source

        // Default Constructor
        HeatReclaimRefrigCondenserData() : AvailTemperature(0.0)
        {
        }
    };

    struct AirReportVars
    {
        // Members
        Real64 MeanAirTemp;            // Mean Air Temperature {C}
        Real64 OperativeTemp;          // Average of Mean Air Temperature {C} and Mean Radiant Temperature {C}
        Real64 MeanAirHumRat;          // Mean Air Humidity Ratio {kg/kg} (averaged over zone time step)
        Real64 MeanAirDewPointTemp;    // Mean Air Dewpoint Temperature {C}
        Real64 ThermOperativeTemp;     // Mix or MRT and MAT for Zone Control:Thermostatic:Operative Temperature {C}
        Real64 InfilHeatGain;          // Heat Gain {J} due to infiltration
        Real64 InfilHeatLoss;          // Heat Loss {J} due to infiltration
        Real64 InfilLatentGain;        // Latent Gain {J} due to infiltration
        Real64 InfilLatentLoss;        // Latent Loss {J} due to infiltration
        Real64 InfilTotalGain;         // Total Gain {J} due to infiltration (sensible+latent)
        Real64 InfilTotalLoss;         // Total Loss {J} due to infiltration (sensible+latent)
        Real64 InfilVolumeCurDensity;  // Volume of Air {m3} due to infiltration at current zone air density
        Real64 InfilVolumeStdDensity;  // Volume of Air {m3} due to infiltration at standard density (adjusted for elevation)
        Real64 InfilVdotCurDensity;    // Volume flow rate of Air {m3/s} due to infiltration at current zone air density
        Real64 InfilVdotStdDensity;    // Volume flow rate of Air {m3/s} due to infiltration standard density (adjusted elevation)
        Real64 InfilMass;              // Mass of Air {kg} due to infiltration
        Real64 InfilMdot;              // Mass flow rate of Air (kg/s) due to infiltration
        Real64 InfilAirChangeRate;     // Infiltration air change rate {ach}
        Real64 VentilHeatLoss;         // Heat Gain {J} due to ventilation
        Real64 VentilHeatGain;         // Heat Loss {J} due to ventilation
        Real64 VentilLatentLoss;       // Latent Gain {J} due to ventilation
        Real64 VentilLatentGain;       // Latent Loss {J} due to ventilation
        Real64 VentilTotalLoss;        // Total Gain {J} due to ventilation
        Real64 VentilTotalGain;        // Total Loss {J} due to ventilation
        Real64 VentilVolumeCurDensity; // Volume of Air {m3} due to ventilation at current zone air density
        Real64 VentilVolumeStdDensity; // Volume of Air {m3} due to ventilation at standard density (adjusted for elevation)
        Real64 VentilVdotCurDensity;   // Volume flow rate of Air {m3/s} due to ventilation at current zone air density
        Real64 VentilVdotStdDensity;   // Volume flow rate of Air {m3/s} due to ventilation at standard density (adjusted elevation)
        Real64 VentilMass;             // Mass of Air {kg} due to ventilation
        Real64 VentilMdot;             // Mass flow rate of Air {kg/s} due to ventilation
        Real64 VentilAirChangeRate;    // Ventilation air change rate (ach)
        Real64 VentilFanElec;          // Fan Electricity {W} due to ventilation
        Real64 VentilAirTemp;          // Air Temp {C} of ventilation
        Real64 MixVolume;              // Mixing volume of Air {m3}
        Real64 MixVdotCurDensity;      // Mixing volume flow rate of Air {m3/s} at current zone air density
        Real64 MixVdotStdDensity;      // Mixing volume flow rate of Air {m3/s} at standard density (adjusted for elevation)
        Real64 MixMass;                // Mixing mass of air {kg}
        Real64 MixMdot;                // Mixing mass flow rate of air {kg/s}
        Real64 MixHeatLoss;            // Heat Gain {J} due to mixing and cross mixing and refrigeration door mixing
        Real64 MixHeatGain;            // Heat Loss {J} due to mixing and cross mixing and refrigeration door mixing
        Real64 MixLatentLoss;          // Latent Gain {J} due to mixing and cross mixing and refrigeration door mixing
        Real64 MixLatentGain;          // Latent Loss {J} due to mixing and cross mixing and refrigeration door mixing
        Real64 MixTotalLoss;           // Total Gain {J} due to mixing and cross mixing and refrigeration door mixing
        Real64 MixTotalGain;           // Total Loss {J} due to mixing and cross mixing and refrigeration door mixing
        Real64 SysInletMass;           // Total mass of Air {kg} from all system inlets
        Real64 SysOutletMass;          // Total mass of Air {kg} from all system outlets
        Real64 ExfilMass;              // Mass of Air {kg} due to exfiltration
        Real64 ExfilTotalLoss;         // Total Loss rate {W} due to exfiltration (sensible+latent)
        Real64 ExfilSensiLoss;         // Sensible Loss rate {W} due to exfiltration
        Real64 ExfilLatentLoss;        // Latent Loss rate {W} due to exfiltration
        Real64 ExhTotalLoss;           // Total Loss rate {W} due to zone exhaust air (sensible+latent)
        Real64 ExhSensiLoss;           // Sensible Loss rate {W} due to zone exhaust air
        Real64 ExhLatentLoss;          // Latent Loss rate {W} due to zone exhaust air
        // air heat balance component load summary results
        Real64 SumIntGains;     // Zone sum of convective internal gains
        Real64 SumHADTsurfs;    // Zone sum of Hc*Area*(Tsurf - Tz)
        Real64 SumMCpDTzones;   // zone sum of MassFlowRate*cp*(TremotZone - Tz) transfer air from other zone, Mixing
        Real64 SumMCpDtInfil;   // Zone sum of MassFlowRate*Cp*(Tout - Tz) transfer from outside, ventil, earth tube
        Real64 SumMCpDTsystem;  // Zone sum of air system MassFlowRate*Cp*(Tsup - Tz)
        Real64 SumNonAirSystem; // Zone sum of system convective gains, collected via NonAirSystemResponse
        Real64 CzdTdt;          // Zone air energy storage term.
        Real64 imBalance;       // put all terms in eq. 5 on RHS , should be zero
        // for ZoneAirBalance:OutdoorAir object Outputs only
        Real64 OABalanceHeatLoss;         // Heat Gain {J} due to OA air balance
        Real64 OABalanceHeatGain;         // Heat Loss {J} due to OA air balance
        Real64 OABalanceLatentLoss;       // Latent Gain {J} due to OA air balance
        Real64 OABalanceLatentGain;       // Latent Loss {J} due to OA air balance
        Real64 OABalanceTotalLoss;        // Total Gain {J} due to OA air balance
        Real64 OABalanceTotalGain;        // Total Loss {J} due to OA air balance
        Real64 OABalanceVolumeCurDensity; // Volume of Air {m3} due to OA air balance
        // at current zone air density
        Real64 OABalanceVolumeStdDensity; // Volume of Air {m3} due to OA air balance
        // at standard density (adjusted for elevation)
        Real64 OABalanceVdotCurDensity; // Volume flow rate of Air {m3/s} due to OA air balance
        // at current zone air density
        Real64 OABalanceVdotStdDensity; // Volume flow rate of Air {m3/s} due to OA air balance
        // at standard density (adjusted elevation)
        Real64 OABalanceMass;          // Mass of Air {kg} due to OA air balance
        Real64 OABalanceMdot;          // Mass flow rate of Air {kg/s} due to OA air balance
        Real64 OABalanceAirChangeRate; // OA air balance air change rate (ach)
        Real64 OABalanceFanElec;       // Fan Electricity {W} due to OA air balance
        Real64 SumEnthalpyM = 0.0;     // Zone sum of EnthalpyM
        Real64 SumEnthalpyH = 0.0;     // Zone sum of EnthalpyH
                                       // Default Constructor
        AirReportVars()
            : MeanAirTemp(0.0), OperativeTemp(0.0), MeanAirHumRat(0.0), MeanAirDewPointTemp(0.0), ThermOperativeTemp(0.0), InfilHeatGain(0.0),
              InfilHeatLoss(0.0), InfilLatentGain(0.0), InfilLatentLoss(0.0), InfilTotalGain(0.0), InfilTotalLoss(0.0), InfilVolumeCurDensity(0.0),
              InfilVolumeStdDensity(0.0), InfilVdotCurDensity(0.0), InfilVdotStdDensity(0.0), InfilMass(0.0), InfilMdot(0.0), InfilAirChangeRate(0.0),
              VentilHeatLoss(0.0), VentilHeatGain(0.0), VentilLatentLoss(0.0), VentilLatentGain(0.0), VentilTotalLoss(0.0), VentilTotalGain(0.0),
              VentilVolumeCurDensity(0.0), VentilVolumeStdDensity(0.0), VentilVdotCurDensity(0.0), VentilVdotStdDensity(0.0), VentilMass(0.0),
              VentilMdot(0.0), VentilAirChangeRate(0.0), VentilFanElec(0.0), VentilAirTemp(0.0), MixVolume(0.0), MixVdotCurDensity(0.0),
              MixVdotStdDensity(0.0), MixMass(0.0), MixMdot(0.0), MixHeatLoss(0.0), MixHeatGain(0.0), MixLatentLoss(0.0), MixLatentGain(0.0),
              MixTotalLoss(0.0), MixTotalGain(0.0), SysInletMass(0.0), SysOutletMass(0.0), ExfilMass(0.0), ExfilTotalLoss(0.0), ExfilSensiLoss(0.0),
              ExfilLatentLoss(0.0), ExhTotalLoss(0.0), ExhSensiLoss(0.0), ExhLatentLoss(0.0), SumIntGains(0.0), SumHADTsurfs(0.0), SumMCpDTzones(0.0),
              SumMCpDtInfil(0.0), SumMCpDTsystem(0.0), SumNonAirSystem(0.0), CzdTdt(0.0), imBalance(0.0), OABalanceHeatLoss(0.0),
              OABalanceHeatGain(0.0), OABalanceLatentLoss(0.0), OABalanceLatentGain(0.0), OABalanceTotalLoss(0.0), OABalanceTotalGain(0.0),
              OABalanceVolumeCurDensity(0.0), OABalanceVolumeStdDensity(0.0), OABalanceVdotCurDensity(0.0), OABalanceVdotStdDensity(0.0),
              OABalanceMass(0.0), OABalanceMdot(0.0), OABalanceAirChangeRate(0.0), OABalanceFanElec(0.0), SumEnthalpyM(0.0), SumEnthalpyH(0.0)
        {
        }
    };

    struct ZonePreDefRepType
    {
        // Members
        bool isOccupied;        // occupied during the current time step
        Real64 NumOcc;          // number of occupants - used in calculating Vbz
        Real64 NumOccAccum;     // number of occupants accumulating for entire simulation
        Real64 NumOccAccumTime; // time that the number of occupants is accumulating to compute average
        //  - zone time step [hrs]
        Real64 TotTimeOcc; // time occupied (and the mechanical ventilation volume is accumulating)
        //  - system time step [hrs]

        // OA Reports - accumulated values
        // All Vol variables are in m3
        Real64 MechVentVolTotalOcc;       // volume for mechanical ventilation of outside air for entire simulation during occupied at current
        Real64 MechVentVolMin;            // a large number since finding minimum volume at current zone air density
        Real64 InfilVolTotalOcc;          // volume for infiltration of outside air for entire simulation during occupied at current density
        Real64 InfilVolMin;               // a large number since finding minimum volume at current zone air density
        Real64 AFNInfilVolTotalOcc;       // volume for infiltration of outside air for entire simulation during occupied at zone air density
        Real64 AFNInfilVolMin;            // a large number since finding minimum volume at current zone air density
        Real64 SimpVentVolTotalOcc;       // volume for simple 'ZoneVentilation' of outside air for entire simulation during occupied current
        Real64 SimpVentVolMin;            // a large number since finding minimum volumeat current zone air density
        Real64 MechVentVolTotalStdDen;    // volume for mechanical ventilation of outside air for entire simulation at standard density
        Real64 MechVentVolTotalOccStdDen; // volume for mechanical ventilation of outside air for entire simulation during occupied at std
        Real64 InfilVolTotalStdDen;       // volume for infiltration of outside air for entire simulation at standard density
        Real64 InfilVolTotalOccStdDen;    // volume for infiltration of outside air for entire simulation during occupied standard density
        Real64 AFNInfilVolTotalStdDen;    // volume for AFN infiltration of outside air for entire simulation at standard density
        Real64 AFNInfilVolTotalOccStdDen; // volume for AFN infiltration of outside air for entire simulation during occupied at std density
        Real64 AFNVentVolStdDen;          // volume flow rate for natural ventilation at standard density
        Real64 AFNVentVolTotalStdDen;     // volume for natural ventilation for entire simulation at standard density
        Real64 AFNVentVolTotalOccStdDen;  // volume for natural ventilatiofor entire simulation n during occupied at standard density
        Real64 SimpVentVolTotalStdDen;    // volume for simple 'ZoneVentilation' for entire simulation at standard density
        Real64 SimpVentVolTotalOccStdDen; // volume for simple 'ZoneVentilation' for entire simulation during occupied at standard density
        Real64 VozMin;                    // minimum outdoor zone ventilation
        Real64 VozTargetTotal;            // volume for target Voz-dyn for entire simulation at std density
        Real64 VozTargetTotalOcc;         // volume for target Voz-dyn for entire simulation during occupied
        Real64 VozTargetTimeBelow;        // time [hrs] that mechanical+natural ventilation is < VozTarget - 1%
        Real64 VozTargetTimeAt;           // time [hrs] that mechanical+natural ventilation is = VozTarget within 1% and > zero
        Real64 VozTargetTimeAbove;        // time [hrs] that mechanical+natural ventilation is > VozTarget + 1%
        Real64 VozTargetTimeBelowOcc;     // time [hrs] that mechanical+natural ventilation is < VozTarget - 1% during occupied
        Real64 VozTargetTimeAtOcc;        // time [hrs] that mechanical+natural ventilation is = VozTarget within 1% and > zero during occupied
        Real64 VozTargetTimeAboveOcc;     // time [hrs] that mechanical+natural ventilation is > VozTarget + 1% during occupied
        Real64 TotVentTimeNonZeroUnocc;   // time [hrs] that mechanical+natural ventilation is > zero during UNoccupied

        // for Sensible Heat Gas Component Report
        // annual
        Real64 SHGSAnZoneEqHt;  // Zone Eq heating
        Real64 SHGSAnZoneEqCl;  // Zone Eq cooling
        Real64 SHGSAnHvacATUHt; // heating by Air Terminal Unit [J]
        Real64 SHGSAnHvacATUCl; // cooling by Air Terminal Unit [J]
        Real64 SHGSAnSurfHt;    // heated surface heating
        Real64 SHGSAnSurfCl;    // cooled surface cooling
        Real64 SHGSAnPeoplAdd;  // people additions
        Real64 SHGSAnLiteAdd;   // lighting addition
        Real64 SHGSAnEquipAdd;  // equipment addition
        Real64 SHGSAnWindAdd;   // window addition
        Real64 SHGSAnIzaAdd;    // inter zone air addition
        Real64 SHGSAnInfilAdd;  // infiltration addition
        Real64 SHGSAnOtherAdd;  // opaque surface and other addition
        Real64 SHGSAnEquipRem;  // equipment removal
        Real64 SHGSAnWindRem;   // window removal
        Real64 SHGSAnIzaRem;    // inter-zone air removal
        Real64 SHGSAnInfilRem;  // infiltration removal
        Real64 SHGSAnOtherRem;  // opaque surface and other removal
        // peak cooling
        int clPtTimeStamp;      // timestamp for the cooling peak
        Real64 clPeak;          // cooling peak value (hvac air cooling + cooled surface)
        Real64 SHGSClHvacHt;    // hvac air heating
        Real64 SHGSClHvacCl;    // hvac air cooling
        Real64 SHGSClHvacATUHt; // heating by air terminal unit at cool peak [W]
        Real64 SHGSClHvacATUCl; // cooling by air terminal unit at cool peak [W]
        Real64 SHGSClSurfHt;    // heated surface heating
        Real64 SHGSClSurfCl;    // cooled surface cooling
        Real64 SHGSClPeoplAdd;  // people additions
        Real64 SHGSClLiteAdd;   // lighting addition
        Real64 SHGSClEquipAdd;  // equipment addition
        Real64 SHGSClWindAdd;   // window addition
        Real64 SHGSClIzaAdd;    // inter zone air addition
        Real64 SHGSClInfilAdd;  // infiltration addition
        Real64 SHGSClOtherAdd;  // opaque surface and other addition
        Real64 SHGSClEquipRem;  // equipment removal
        Real64 SHGSClWindRem;   // window removal
        Real64 SHGSClIzaRem;    // inter-zone air removal
        Real64 SHGSClInfilRem;  // infiltration removal
        Real64 SHGSClOtherRem;  // opaque surface and other removal
        // peak heating
        int htPtTimeStamp;      // timestamp for the heating peak
        Real64 htPeak;          // heating peak value (hvac air heating + heated surface)
        Real64 SHGSHtHvacHt;    // hvac air heating
        Real64 SHGSHtHvacCl;    // hvac air cooling
        Real64 SHGSHtHvacATUHt; // heating by air terminal unit at heat peak [W]
        Real64 SHGSHtHvacATUCl; // cooling by air terminal unit at heat peak [W]
        Real64 SHGSHtSurfHt;    // heated surface heating
        Real64 SHGSHtSurfCl;    // cooled surface cooling
        Real64 SHGSHtPeoplAdd;  // people additions
        Real64 SHGSHtLiteAdd;   // lighting addition
        Real64 SHGSHtEquipAdd;  // equipment addition
        Real64 SHGSHtWindAdd;   // window addition
        Real64 SHGSHtIzaAdd;    // inter zone air addition
        Real64 SHGSHtInfilAdd;  // infiltration addition
        Real64 SHGSHtOtherAdd;  // opaque surface and other addition
        Real64 SHGSHtEquipRem;  // equipment removal
        Real64 SHGSHtWindRem;   // window removal
        Real64 SHGSHtIzaRem;    // inter-zone air removal
        Real64 SHGSHtInfilRem;  // infiltration removal
        Real64 SHGSHtOtherRem;  // opaque surface and other removal

        // heat emission
        Real64 emiEnvelopConv;      // heat emission from envelope convection
        Real64 emiZoneExfiltration; // heat emission from zone exfiltration
        Real64 emiZoneExhaust;      // heat emission from zone exhaust air
        Real64 emiHVACRelief;       // heat emission from HVAC relief air
        Real64 emiHVACReject;       // heat emission from HVAC reject air
        Real64 emiTotHeat;          // total building heat emission

        // Default Constructor
        ZonePreDefRepType()
            : isOccupied(false), NumOcc(0.0), NumOccAccum(0.0), NumOccAccumTime(0.0), TotTimeOcc(0.0), MechVentVolTotalOcc(0.0),
              MechVentVolMin(9.9e9), InfilVolTotalOcc(0.0), InfilVolMin(9.9e9), AFNInfilVolTotalOcc(0.0), AFNInfilVolMin(9.9e9),
              SimpVentVolTotalOcc(0.0), SimpVentVolMin(9.9e9), MechVentVolTotalStdDen(0.0), MechVentVolTotalOccStdDen(0.0), InfilVolTotalStdDen(0.0),
              InfilVolTotalOccStdDen(0.0), AFNInfilVolTotalStdDen(0.0), AFNInfilVolTotalOccStdDen(0.0), AFNVentVolStdDen(0.0),
              AFNVentVolTotalStdDen(0.0), AFNVentVolTotalOccStdDen(0.0), SimpVentVolTotalStdDen(0.0), SimpVentVolTotalOccStdDen(0.0), VozMin(0.0),
              VozTargetTotal(0.0), VozTargetTotalOcc(0.0), VozTargetTimeBelow(0.0), VozTargetTimeAt(0.0), VozTargetTimeAbove(0.0),
              VozTargetTimeBelowOcc(0.0), VozTargetTimeAtOcc(0.0), VozTargetTimeAboveOcc(0.0), TotVentTimeNonZeroUnocc(0.0), SHGSAnZoneEqHt(0.0),
              SHGSAnZoneEqCl(0.0), SHGSAnHvacATUHt(0.0), SHGSAnHvacATUCl(0.0), SHGSAnSurfHt(0.0), SHGSAnSurfCl(0.0), SHGSAnPeoplAdd(0.0),
              SHGSAnLiteAdd(0.0), SHGSAnEquipAdd(0.0), SHGSAnWindAdd(0.0), SHGSAnIzaAdd(0.0), SHGSAnInfilAdd(0.0), SHGSAnOtherAdd(0.0),
              SHGSAnEquipRem(0.0), SHGSAnWindRem(0.0), SHGSAnIzaRem(0.0), SHGSAnInfilRem(0.0), SHGSAnOtherRem(0.0), clPtTimeStamp(0), clPeak(0.0),
              SHGSClHvacHt(0.0), SHGSClHvacCl(0.0), SHGSClHvacATUHt(0.0), SHGSClHvacATUCl(0.0), SHGSClSurfHt(0.0), SHGSClSurfCl(0.0),
              SHGSClPeoplAdd(0.0), SHGSClLiteAdd(0.0), SHGSClEquipAdd(0.0), SHGSClWindAdd(0.0), SHGSClIzaAdd(0.0), SHGSClInfilAdd(0.0),
              SHGSClOtherAdd(0.0), SHGSClEquipRem(0.0), SHGSClWindRem(0.0), SHGSClIzaRem(0.0), SHGSClInfilRem(0.0), SHGSClOtherRem(0.0),
              htPtTimeStamp(0), htPeak(0.0), SHGSHtHvacHt(0.0), SHGSHtHvacCl(0.0), SHGSHtHvacATUHt(0.0), SHGSHtHvacATUCl(0.0), SHGSHtSurfHt(0.0),
              SHGSHtSurfCl(0.0), SHGSHtPeoplAdd(0.0), SHGSHtLiteAdd(0.0), SHGSHtEquipAdd(0.0), SHGSHtWindAdd(0.0), SHGSHtIzaAdd(0.0),
              SHGSHtInfilAdd(0.0), SHGSHtOtherAdd(0.0), SHGSHtEquipRem(0.0), SHGSHtWindRem(0.0), SHGSHtIzaRem(0.0), SHGSHtInfilRem(0.0),
              SHGSHtOtherRem(0.0), emiEnvelopConv(0.0), emiZoneExfiltration(0.0), emiZoneExhaust(0.0), emiHVACRelief(0.0), emiHVACReject(0.0),
              emiTotHeat(0.0)
        {
        }
    };

    struct ZoneLocalEnvironmentData
    {
        // Members
        std::string Name;
        int ZonePtr;           // surface pointer
        int OutdoorAirNodePtr; // schedule pointer

        // Default Constructor
        ZoneLocalEnvironmentData() : ZonePtr(0), OutdoorAirNodePtr(0)
        {
        }
    };

    struct ZoneReportVars // Zone level.
    {
        // Members
        // People
        Real64 PeopleRadGain;
        Real64 PeopleConGain;
        Real64 PeopleSenGain;
        Real64 PeopleNumOcc;
        Real64 PeopleLatGain;
        Real64 PeopleTotGain;
        Real64 PeopleRadGainRate;
        Real64 PeopleConGainRate;
        Real64 PeopleSenGainRate;
        Real64 PeopleLatGainRate;
        Real64 PeopleTotGainRate;
        // Lights
        Real64 LtsPower;
        Real64 LtsElecConsump;
        Real64 LtsRadGain;
        Real64 LtsVisGain;
        Real64 LtsConGain;
        Real64 LtsRetAirGain;
        Real64 LtsTotGain;
        Real64 LtsRadGainRate;
        Real64 LtsVisGainRate;
        Real64 LtsConGainRate;
        Real64 LtsRetAirGainRate;
        Real64 LtsTotGainRate;
        // Baseboard Heat
        Real64 BaseHeatPower;
        Real64 BaseHeatElecCons;
        Real64 BaseHeatRadGain;
        Real64 BaseHeatConGain;
        Real64 BaseHeatTotGain;
        Real64 BaseHeatRadGainRate;
        Real64 BaseHeatConGainRate;
        Real64 BaseHeatTotGainRate;
        // Electric Equipment
        Real64 ElecPower;
        Real64 ElecConsump;
        Real64 ElecRadGain;
        Real64 ElecConGain;
        Real64 ElecLatGain;
        Real64 ElecLost;
        Real64 ElecTotGain;
        Real64 ElecRadGainRate;
        Real64 ElecConGainRate;
        Real64 ElecLatGainRate;
        Real64 ElecLostRate;
        Real64 ElecTotGainRate;
        // Gas Equipment
        Real64 GasPower;
        Real64 GasConsump;
        Real64 GasRadGain;
        Real64 GasConGain;
        Real64 GasLatGain;
        Real64 GasLost;
        Real64 GasTotGain;
        Real64 GasRadGainRate;
        Real64 GasConGainRate;
        Real64 GasLatGainRate;
        Real64 GasLostRate;
        Real64 GasTotGainRate;
        // Hot Water Equipment
        Real64 HWPower;
        Real64 HWConsump;
        Real64 HWRadGain;
        Real64 HWConGain;
        Real64 HWLatGain;
        Real64 HWLost;
        Real64 HWTotGain;
        Real64 HWRadGainRate;
        Real64 HWConGainRate;
        Real64 HWLatGainRate;
        Real64 HWLostRate;
        Real64 HWTotGainRate;
        // Steam Equipment
        Real64 SteamPower;
        Real64 SteamConsump;
        Real64 SteamRadGain;
        Real64 SteamConGain;
        Real64 SteamLatGain;
        Real64 SteamLost;
        Real64 SteamTotGain;
        Real64 SteamRadGainRate;
        Real64 SteamConGainRate;
        Real64 SteamLatGainRate;
        Real64 SteamLostRate;
        Real64 SteamTotGainRate;
        // Other Equipment
        Real64 OtherPower;
        Real64 OtherConsump;
        Real64 OtherRadGain;
        Real64 OtherConGain;
        Real64 OtherLatGain;
        Real64 OtherLost;
        Real64 OtherTotGain;
        Real64 OtherRadGainRate;
        Real64 OtherConGainRate;
        Real64 OtherLatGainRate;
        Real64 OtherLostRate;
        Real64 OtherTotGainRate;
        // IT Equipment
        Real64 ITEqCPUPower;            // Zone ITE CPU Electric Power [W]
        Real64 ITEqFanPower;            // Zone ITE Fan Electric Power [W]
        Real64 ITEqUPSPower;            // Zone ITE UPS Electric Power [W]
        Real64 ITEqCPUPowerAtDesign;    // Zone ITE CPU Electric Power at Design Inlet Conditions [W]
        Real64 ITEqFanPowerAtDesign;    // Zone ITE Fan Electric Power at Design Inlet Conditions [W]
        Real64 ITEqUPSGainRateToZone;   // Zone ITE UPS Heat Gain to Zone Rate [W] - convective gain
        Real64 ITEqConGainRateToZone;   // Zone ITE Total Heat Gain toZone Rate [W] - convective gain - includes heat gain from UPS, plus CPU and Fans
                                        // if room air model not used
        Real64 ITEqCPUConsumption;      // Zone ITE CPU Electric Energy [J]
        Real64 ITEqFanConsumption;      // Zone ITE Fan Electric Energy [J]
        Real64 ITEqUPSConsumption;      // Zone ITE UPS Electric Energy [J]
        Real64 ITEqCPUEnergyAtDesign;   // Zone ITE CPU Electric Energy at Design Inlet Conditions [J]
        Real64 ITEqFanEnergyAtDesign;   // Zone ITE Fan Electric Energy at Design Inlet Conditions [J]
        Real64 ITEqUPSGainEnergyToZone; // Zone ITE UPS Heat Gain to Zone Energy [J] - convective gain
        Real64 ITEqConGainEnergyToZone; // Zone ITE Total Heat Gain toZone Energy [J] - convective gain - includes heat gain from UPS, plus CPU and
                                        // Fans if room air model not used
        Real64 ITEqAirVolFlowStdDensity; // Zone Air volume flow rate at standard density [m3/s]
        Real64 ITEqAirMassFlow;          // Zone Air mass flow rate [kg/s]
        Real64 ITEqSHI;                  // Zone Supply Heat Index []
        Real64 ITEqTimeOutOfOperRange;   // Zone ITE Air Inlet Operating Range Exceeded Time [hr]
        Real64 ITEqTimeAboveDryBulbT;    // Zone ITE Air Inlet Dry-Bulb Temperature Above Operating Range Time [hr]
        Real64 ITEqTimeBelowDryBulbT;    // Zone ITE Air Inlet Dry-Bulb Temperature Below Operating Range Time [hr]
        Real64 ITEqTimeAboveDewpointT;   // Zone ITE Air Inlet Dewpoint Temperature Above Operating Range Time [hr]
        Real64 ITEqTimeBelowDewpointT;   // Zone ITE Air Inlet Dewpoint Temperature Below Operating Range Time [hr]
        Real64 ITEqTimeAboveRH;          // Zone ITE Air Inlet Relative Humidity Above Operating Range Time [hr]
        Real64 ITEqTimeBelowRH;          // Zone ITE Air Inlet Relative Humidity Below Operating Range Time [hr]
        Real64 ITEAdjReturnTemp;         // Zone ITE Adjusted Return Air Temperature
        // Overall Zone Variables
        Real64 TotRadiantGain;
        Real64 TotVisHeatGain;
        Real64 TotConvectiveGain;
        Real64 TotLatentGain;
        Real64 TotTotalHeatGain;
        Real64 TotRadiantGainRate;
        Real64 TotVisHeatGainRate;
        Real64 TotConvectiveGainRate;
        Real64 TotLatentGainRate;
        Real64 TotTotalHeatGainRate;
        // Contaminant
        Real64 CO2Rate;
        Real64 GCRate;

        Real64 SumTinMinusTSup;  // Numerator for zone-level sensible heat index (SHI)
        Real64 SumToutMinusTSup; // Denominator for zone-level sensible heat index (SHI)

        // Default Constructor
        ZoneReportVars()
            : PeopleRadGain(0.0), PeopleConGain(0.0), PeopleSenGain(0.0), PeopleNumOcc(0.0), PeopleLatGain(0.0), PeopleTotGain(0.0),
              PeopleRadGainRate(0.0), PeopleConGainRate(0.0), PeopleSenGainRate(0.0), PeopleLatGainRate(0.0), PeopleTotGainRate(0.0), LtsPower(0.0),
              LtsElecConsump(0.0), LtsRadGain(0.0), LtsVisGain(0.0), LtsConGain(0.0), LtsRetAirGain(0.0), LtsTotGain(0.0), LtsRadGainRate(0.0),
              LtsVisGainRate(0.0), LtsConGainRate(0.0), LtsRetAirGainRate(0.0), LtsTotGainRate(0.0), BaseHeatPower(0.0), BaseHeatElecCons(0.0),
              BaseHeatRadGain(0.0), BaseHeatConGain(0.0), BaseHeatTotGain(0.0), BaseHeatRadGainRate(0.0), BaseHeatConGainRate(0.0),
              BaseHeatTotGainRate(0.0), ElecPower(0.0), ElecConsump(0.0), ElecRadGain(0.0), ElecConGain(0.0), ElecLatGain(0.0), ElecLost(0.0),
              ElecTotGain(0.0), ElecRadGainRate(0.0), ElecConGainRate(0.0), ElecLatGainRate(0.0), ElecLostRate(0.0), ElecTotGainRate(0.0),
              GasPower(0.0), GasConsump(0.0), GasRadGain(0.0), GasConGain(0.0), GasLatGain(0.0), GasLost(0.0), GasTotGain(0.0), GasRadGainRate(0.0),
              GasConGainRate(0.0), GasLatGainRate(0.0), GasLostRate(0.0), GasTotGainRate(0.0), HWPower(0.0), HWConsump(0.0), HWRadGain(0.0),
              HWConGain(0.0), HWLatGain(0.0), HWLost(0.0), HWTotGain(0.0), HWRadGainRate(0.0), HWConGainRate(0.0), HWLatGainRate(0.0),
              HWLostRate(0.0), HWTotGainRate(0.0), SteamPower(0.0), SteamConsump(0.0), SteamRadGain(0.0), SteamConGain(0.0), SteamLatGain(0.0),
              SteamLost(0.0), SteamTotGain(0.0), SteamRadGainRate(0.0), SteamConGainRate(0.0), SteamLatGainRate(0.0), SteamLostRate(0.0),
              SteamTotGainRate(0.0), OtherPower(0.0), OtherConsump(0.0), OtherRadGain(0.0), OtherConGain(0.0), OtherLatGain(0.0), OtherLost(0.0),
              OtherTotGain(0.0), OtherRadGainRate(0.0), OtherConGainRate(0.0), OtherLatGainRate(0.0), OtherLostRate(0.0), OtherTotGainRate(0.0),
              ITEqCPUPower(0.0), ITEqFanPower(0.0), ITEqUPSPower(0.0), ITEqCPUPowerAtDesign(0.0), ITEqFanPowerAtDesign(0.0),
              ITEqUPSGainRateToZone(0.0), ITEqConGainRateToZone(0.0), ITEqCPUConsumption(0.0), ITEqFanConsumption(0.0), ITEqUPSConsumption(0.0),
              ITEqCPUEnergyAtDesign(0.0), ITEqFanEnergyAtDesign(0.0), ITEqUPSGainEnergyToZone(0.0), ITEqConGainEnergyToZone(0.0),
              ITEqAirVolFlowStdDensity(0.0), ITEqAirMassFlow(0.0), ITEqSHI(0.0), ITEqTimeOutOfOperRange(0.0), ITEqTimeAboveDryBulbT(0.0),
              ITEqTimeBelowDryBulbT(0.0), ITEqTimeAboveDewpointT(0.0), ITEqTimeBelowDewpointT(0.0), ITEqTimeAboveRH(0.0), ITEqTimeBelowRH(0.0),
              ITEAdjReturnTemp(0.0), TotRadiantGain(0.0), TotVisHeatGain(0.0), TotConvectiveGain(0.0), TotLatentGain(0.0), TotTotalHeatGain(0.0),
              TotRadiantGainRate(0.0), TotVisHeatGainRate(0.0), TotConvectiveGainRate(0.0), TotLatentGainRate(0.0), TotTotalHeatGainRate(0.0),
              CO2Rate(0.0), GCRate(0.0), SumTinMinusTSup(0.0), SumToutMinusTSup(0.0)
        {
        }
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

    std::string DisplayMaterialRoughness(int Roughness); // Roughness String

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
    int DefaultInsideConvectionAlgo = 1;    // 1 = simple (ASHRAE); 2 = detailed (ASHRAE); 3 = ceiling diffuser; 4 = trombe wall
    int DefaultOutsideConvectionAlgo = 1;   // 1 = simple (ASHRAE); 2 = detailed; etc (BLAST, TARP, MOWITT, DOE-2)
    int SolarDistribution = 0;              // Solar Distribution Algorithm
    int InsideSurfIterations = 0;           // Counts inside surface iterations
    DataSurfaces::iHeatTransferModel OverallHeatTransferSolutionAlgo = DataSurfaces::iHeatTransferModel::CTF; // Global HeatBalanceAlgorithm setting
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
    int ZoneAirSolutionAlgo = DataHeatBalance::Use3rdOrder; // ThirdOrderBackwardDifference, AnalyticalSolution, and EulerMethod
    bool OverrideZoneAirSolutionAlgo = false;               // Override the zone air solution algorithm in PerformancePrecisionTradeoffs
    Real64 BuildingRotationAppendixG = 0.0;                 // Building Rotation for Appendix G
    Real64 ZoneTotalExfiltrationHeatLoss = 0.0;             // Building total heat emission through zone exfiltration;
    Real64 ZoneTotalExhaustHeatLoss = 0.0;                  // Building total heat emission through zone air exhaust;
    Real64 SysTotalHVACReliefHeatLoss = 0.0;                // Building total heat emission through HVAC system relief air;
    Real64 SysTotalHVACRejectHeatLoss = 0.0;                // Building total heat emission through HVAC system heat rejection;
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
    int TotPeople = 0;                  // Total People Statements in input and extrapolated from global assignments
    int TotLights = 0;                  // Total Lights Statements in input and extrapolated from global assignments
    int TotElecEquip = 0;               // Total Electric Equipment Statements in input and extrapolated from global assignments
    int TotGasEquip = 0;                // Total Gas Equipment Statements in input
    int TotOthEquip = 0;                // Total Other Equipment Statements in input
    int TotHWEquip = 0;                 // Total Hot Water Equipment Statements in input
    int TotStmEquip = 0;                // Total Steam Equipment Statements in input
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
    int TotBBHeat = 0;                  // Total BBHeat Statements in input
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

    Array1D<Real64> SNLoadHeatEnergy;
    Array1D<Real64> SNLoadCoolEnergy;
    Array1D<Real64> SNLoadHeatRate;
    Array1D<Real64> SNLoadCoolRate;
    Array1D<Real64> SNLoadPredictedRate;
    Array1D<Real64> SNLoadPredictedHSPRate; // Predicted load to heating setpoint (unmultiplied)
    Array1D<Real64> SNLoadPredictedCSPRate; // Predicted load to cooling setpoint (unmultiplied)
    Array1D<Real64> MoisturePredictedRate;
    Array1D<Real64> MoisturePredictedHumSPRate;   // Predicted latent load to humidification setpoint (unmultiplied)
    Array1D<Real64> MoisturePredictedDehumSPRate; // Predicted latent load to dehumidification setpoint (unmultiplied)
    Array1D<Real64> ListSNLoadHeatEnergy;
    Array1D<Real64> ListSNLoadCoolEnergy;
    Array1D<Real64> ListSNLoadHeatRate;
    Array1D<Real64> ListSNLoadCoolRate;
    Array1D<Real64> GroupSNLoadHeatEnergy;
    Array1D<Real64> GroupSNLoadCoolEnergy;
    Array1D<Real64> GroupSNLoadHeatRate;
    Array1D<Real64> GroupSNLoadCoolRate;

    Array1D<Real64> ZoneMRT;        // MEAN RADIANT TEMPERATURE (C)
    Array1D<Real64> ZoneTransSolar; // Exterior beam plus diffuse solar entering zone sum of WinTransSolar for exterior windows in zone (W)
    Array1D<Real64>
        ZoneWinHeatGain; // Heat gain to zone from all exterior windows (includes oneTransSolar); sum of WinHeatGain for exterior windows in zone (W)
    Array1D<Real64> ZoneWinHeatGainRep;             // = ZoneWinHeatGain when ZoneWinHeatGain >= 0
    Array1D<Real64> ZoneWinHeatLossRep;             // = -ZoneWinHeatGain when ZoneWinHeatGain < 0
    Array1D<Real64> ZoneBmSolFrExtWinsRep;          // Beam solar into zone from exterior windows [W]
    Array1D<Real64> ZoneBmSolFrIntWinsRep;          // Beam solar into zone from interior windows [W]
    Array1D<Real64> ZoneInitialDifSolReflW;         // Initial diffuse solar in zone from ext and int windows reflected from interior surfaces [W]
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

    Array1D<Real64> SurfQRadThermInAbs;                 // Thermal radiation absorbed on inside surfaces
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
    Array1D<Real64> SurfCosIncidenceAngle;              // Cosine of beam solar incidence angle (for reporting)
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

    // Material
    Array1D<Real64> NominalR;                       // Nominal R value of each material -- used in matching interzone surfaces
    Array1D<Real64> NominalRforNominalUCalculation; // Nominal R values are summed to calculate NominalU values for constructions
    Array1D<Real64> NominalU;                       // Nominal U value for each construction -- used in matching interzone surfaces

    // todo - rename and reordering
    Array1D<Real64> SurfTempEffBulkAir;     // air temperature adjacent to the surface used for inside surface heat balances
    Array1D<Real64> HConvIn;                // INSIDE CONVECTION COEFFICIENT
    Array1D<Real64> SurfAnisoSkyMult;       // Multiplier on exterior-surface sky view factor to account for
                                            // anisotropy of sky radiance; = 1.0 for for isotropic sky
    Array1D<Real64> DifShdgRatioIsoSky;     // Diffuse shading ratio (WithShdgIsoSky/WoShdgIsoSky)
    Array3D<Real64> DifShdgRatioIsoSkyHRTS; // Diffuse shading ratio (WithShdgIsoSky/WoShdgIsoSky)
    Array1D<Real64> curDifShdgRatioIsoSky;  // Diffuse shading ratio (WithShdgIsoSky/WoShdgIsoSky)
    Array1D<Real64> DifShdgRatioHoriz;      // Horizon shading ratio (WithShdgHoriz/WoShdgHoriz)
    Array3D<Real64> DifShdgRatioHorizHRTS;  // Horizon shading ratio (WithShdgHoriz/WoShdgHoriz)
    Array1D<Real64> WithShdgIsoSky;         // Diffuse solar irradiance from sky on surface, with shading
    Array1D<Real64> WoShdgIsoSky;           // Diffuse solar from sky on surface, without shading
    Array1D<Real64> WithShdgHoriz;          // Diffuse solar irradiance from horizon portion of sky on surface, with shading
    Array1D<Real64> WoShdgHoriz;            // Diffuse solar irradiance from horizon portion of sky on surface, without shading
    Array1D<Real64> MultIsoSky;             // Contribution to eff sky view factor from isotropic sky
    Array1D<Real64> MultCircumSolar;        // Contribution to eff sky view factor from circumsolar brightening
    Array1D<Real64> MultHorizonZenith;      // Contribution to eff sky view factor from horizon or zenith brightening

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
    Array1D<bool> ZoneSolAbsFirstCalc;   // for error message
    Array1D<bool> EnclRadReCalc;         // Enclosure solar or thermal radiation properties needs to be recalc due to window/shading status change
    bool EnclRadAlwaysReCalc = false;    // Enclosure solar or thermal radiation properties always needs to be recalc at any time step
    // todo - the following in absorptance branch
    Array2D<Real64> SunlitFracHR;            // Hourly fraction of heat transfer surface that is sunlit
    Array2D<Real64> CosIncAngHR;             // Hourly cosine of beam radiation incidence angle on surface
    Array3D<Real64> SunlitFrac;              // TimeStep fraction of heat transfer surface that is sunlit
    Array3D<Real64> SunlitFracWithoutReveal; // For a window with reveal, the sunlit fraction  without shadowing by the reveal
    Array3D<Real64> CosIncAng;               // TimeStep cosine of beam radiation incidence angle on surface
    Array4D_int
        BackSurfaces; // For a given hour and timestep, a list of up to 20 surfaces receiving  beam solar radiation from a given exterior window
    Array4D<Real64> OverlapAreas; // For a given hour and timestep, the areas of the exterior window sending beam solar radiation to the surfaces
                                  // listed in BackSurfaces
    Real64 zeroPointerVal = 0.0;
    int NumAirBoundaryMixing = 0;             // Number of air boundary simple mixing objects needed
    std::vector<int> AirBoundaryMixingZone1;  // Air boundary simple mixing zone 1
    std::vector<int> AirBoundaryMixingZone2;  // Air boundary simple mixing zone 2
    std::vector<int> AirBoundaryMixingSched;  // Air boundary simple mixing schedule index
    std::vector<Real64> AirBoundaryMixingVol; // Air boundary simple mixing volume flow rate [m3/s]
    EPVector<DataHeatBalance::ZonePreDefRepType> ZonePreDefRep;
    DataHeatBalance::ZonePreDefRepType BuildingPreDefRep;
    EPVector<DataHeatBalance::ZoneSimData> ZoneIntGain;
    EPVector<DataHeatBalance::GapSupportPillar> SupportPillar;
    EPVector<DataHeatBalance::GapDeflectionState> DeflectionState;
    EPVector<DataHeatBalance::SpectralDataProperties> SpectralData;
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
    EPVector<DataHeatBalance::GlobalInternalGainMiscObject> InfiltrationObjects;
    EPVector<DataHeatBalance::GlobalInternalGainMiscObject> VentilationObjects;
    EPVector<DataHeatBalance::ZoneReportVars> ZnRpt;
    EPVector<DataHeatBalance::ZoneMassConservationData> MassConservation;
    DataHeatBalance::ZoneAirMassFlowConservation ZoneAirMassFlow;
    EPVector<DataHeatBalance::ZoneLocalEnvironmentData> ZoneLocalEnvironment;
    bool MundtFirstTimeFlag = true;

    void clear_state() override
    {
        this->MaxSolidWinLayers = 0;
        this->LowHConvLimit = 0.1;
        this->HighHConvLimit = 1000.0;
        this->MaxAllowedDelTemp = 0.002;
        this->MaxAllowedDelTempCondFD = 0.002;
        this->BuildingName.clear();
        this->BuildingAzimuth = 0.0;
        this->LoadsConvergTol = 0.0;
        this->TempConvergTol = 0.0;
        this->DefaultInsideConvectionAlgo = 1;
        this->DefaultOutsideConvectionAlgo = 1;
        this->SolarDistribution = 0;
        this->InsideSurfIterations = 0;
        this->OverallHeatTransferSolutionAlgo = DataSurfaces::iHeatTransferModel::CTF;
        this->AllCTF = true;
        this->AnyCTF = false;
        this->AnyEMPD = false;
        this->AnyCondFD = false;
        this->AnyHAMT = false;
        this->AnyKiva = false;
        this->AnyAirBoundary = false;
        this->AnyBSDF = false;
        this->MaxNumberOfWarmupDays = 25;
        this->MinNumberOfWarmupDays = 1;
        this->CondFDRelaxFactor = 1.0;
        this->CondFDRelaxFactorInput = 1.0;
        this->ZoneAirSolutionAlgo = DataHeatBalance::Use3rdOrder;
        this->OverrideZoneAirSolutionAlgo = false;
        this->BuildingRotationAppendixG = 0.0;
        this->ZoneTotalExfiltrationHeatLoss = 0.0;
        this->ZoneTotalExhaustHeatLoss = 0.0;
        this->SysTotalHVACReliefHeatLoss = 0.0;
        this->SysTotalHVACRejectHeatLoss = 0.0;
        this->NumOfZoneLists = 0;
        this->NumOfZoneGroups = 0;
        this->NumPeopleStatements = 0;
        this->NumLightsStatements = 0;
        this->NumZoneElectricStatements = 0;
        this->NumZoneGasStatements = 0;
        this->NumInfiltrationStatements = 0;
        this->NumVentilationStatements = 0;
        this->NumHotWaterEqStatements = 0;
        this->NumSteamEqStatements = 0;
        this->NumOtherEqStatements = 0;
        this->NumZoneITEqStatements = 0;
        this->TotPeople = 0;
        this->TotLights = 0;
        this->TotElecEquip = 0;
        this->TotGasEquip = 0;
        this->TotOthEquip = 0;
        this->TotHWEquip = 0;
        this->TotStmEquip = 0;
        this->TotInfiltration = 0;
        this->TotDesignFlowInfiltration = 0;
        this->TotShermGrimsInfiltration = 0;
        this->TotAIM2Infiltration = 0;
        this->TotVentilation = 0;
        this->TotDesignFlowVentilation = 0;
        this->TotWindAndStackVentilation = 0;
        this->TotMixing = 0;
        this->TotCrossMixing = 0;
        this->TotRefDoorMixing = 0;
        this->TotBBHeat = 0;
        this->TotMaterials = 0;
        this->TotConstructs = 0;
        this->TotSpectralData = 0;
        this->W5GlsMat = 0;
        this->W5GlsMatAlt = 0;
        this->W5GasMat = 0;
        this->W5GasMatMixture = 0;
        this->W7SupportPillars = 0;
        this->W7DeflectionStates = 0;
        this->W7MaterialGaps = 0;
        this->TotBlinds = 0;
        this->TotScreens = 0;
        this->TotTCGlazings = 0;
        this->NumSurfaceScreens = 0;
        this->TotShades = 0;
        this->TotComplexShades = 0;
        this->TotComplexGaps = 0;
        this->TotSimpleWindow = 0;
        this->W5GlsMatEQL = 0;
        this->TotShadesEQL = 0;
        this->TotDrapesEQL = 0;
        this->TotBlindsEQL = 0;
        this->TotScreensEQL = 0;
        this->W5GapMatEQL = 0;
        this->TotZoneAirBalance = 0;
        this->TotFrameDivider = 0;
        this->AirFlowFlag = false;
        this->TotCO2Gen = 0;
        this->CalcWindowRevealReflection = false;
        this->StormWinChangeThisDay = false;
        this->AnyInternalHeatSourceInInput = false;
        this->AdaptiveComfortRequested_CEN15251 = false;
        this->AdaptiveComfortRequested_ASH55 = false;
        this->AnyThermalComfortPierceModel = false;
        this->AnyThermalComfortKSUModel = false;
        this->AnyThermalComfortCoolingEffectModel = false;
        this->AnyThermalComfortAnkleDraftModel = false;
        this->NoFfactorConstructionsUsed = true;
        this->NoCfactorConstructionsUsed = true;
        this->NoRegularMaterialsUsed = true;
        this->EnclRadAlwaysReCalc = false;
        this->SNLoadHeatEnergy.deallocate();
        this->SNLoadCoolEnergy.deallocate();
        this->SNLoadHeatRate.deallocate();
        this->SNLoadCoolRate.deallocate();
        this->SNLoadPredictedRate.deallocate();
        this->SNLoadPredictedHSPRate.deallocate();
        this->SNLoadPredictedCSPRate.deallocate();
        this->MoisturePredictedRate.deallocate();
        this->MoisturePredictedHumSPRate.deallocate();
        this->MoisturePredictedDehumSPRate.deallocate();
        this->ListSNLoadHeatEnergy.deallocate();
        this->ListSNLoadCoolEnergy.deallocate();
        this->ListSNLoadHeatRate.deallocate();
        this->ListSNLoadCoolRate.deallocate();
        this->GroupSNLoadHeatEnergy.deallocate();
        this->GroupSNLoadCoolEnergy.deallocate();
        this->GroupSNLoadHeatRate.deallocate();
        this->GroupSNLoadCoolRate.deallocate();
        this->ZoneMRT.deallocate();
        this->ZoneTransSolar.deallocate();
        this->ZoneWinHeatGain.deallocate();
        this->ZoneWinHeatGainRep.deallocate();
        this->ZoneWinHeatLossRep.deallocate();
        this->ZoneBmSolFrExtWinsRep.deallocate();
        this->ZoneBmSolFrIntWinsRep.deallocate();
        this->ZoneInitialDifSolReflW.deallocate();
        this->ZoneDifSolFrExtWinsRep.deallocate();
        this->ZoneDifSolFrIntWinsRep.deallocate();
        this->ZoneOpaqSurfInsFaceCond.deallocate();
        this->ZoneOpaqSurfInsFaceCondGainRep.deallocate();
        this->ZoneOpaqSurfInsFaceCondLossRep.deallocate();
        this->ZoneOpaqSurfExtFaceCond.deallocate();
        this->ZoneOpaqSurfExtFaceCondGainRep.deallocate();
        this->ZoneOpaqSurfExtFaceCondLossRep.deallocate();
        this->ZoneTransSolarEnergy.deallocate();
        this->ZoneWinHeatGainRepEnergy.deallocate();
        this->ZoneWinHeatLossRepEnergy.deallocate();
        this->ZoneBmSolFrExtWinsRepEnergy.deallocate();
        this->ZoneBmSolFrIntWinsRepEnergy.deallocate();
        this->ZoneDifSolFrExtWinsRepEnergy.deallocate();
        this->ZoneDifSolFrIntWinsRepEnergy.deallocate();
        this->ZnOpqSurfInsFaceCondGnRepEnrg.deallocate();
        this->ZnOpqSurfInsFaceCondLsRepEnrg.deallocate();
        this->ZnOpqSurfExtFaceCondGnRepEnrg.deallocate();
        this->ZnOpqSurfExtFaceCondLsRepEnrg.deallocate();
        this->SurfQRadThermInAbs.deallocate();
        this->SurfQRadSWOutIncident.deallocate();
        this->SurfQRadSWOutIncidentBeam.deallocate();
        this->SurfBmIncInsSurfIntensRep.deallocate();
        this->SurfBmIncInsSurfAmountRep.deallocate();
        this->SurfIntBmIncInsSurfIntensRep.deallocate();
        this->SurfIntBmIncInsSurfAmountRep.deallocate();
        this->SurfQRadSWOutIncidentSkyDiffuse.deallocate();
        this->SurfQRadSWOutIncidentGndDiffuse.deallocate();
        this->SurfQRadSWOutIncBmToDiffReflGnd.deallocate();
        this->SurfQRadSWOutIncSkyDiffReflGnd.deallocate();
        this->SurfQRadSWOutIncBmToBmReflObs.deallocate();
        this->SurfQRadSWOutIncBmToDiffReflObs.deallocate();
        this->SurfQRadSWOutIncSkyDiffReflObs.deallocate();
        this->SurfCosIncidenceAngle.deallocate();
        this->SurfSWInAbsTotalReport.deallocate();
        this->SurfBmIncInsSurfAmountRepEnergy.deallocate();
        this->SurfIntBmIncInsSurfAmountRepEnergy.deallocate();
        this->SurfInitialDifSolInAbsReport.deallocate();
        this->SurfWinBSDFBeamDirectionRep.deallocate();
        this->SurfWinBSDFBeamThetaRep.deallocate();
        this->SurfWinBSDFBeamPhiRep.deallocate();
        this->SurfWinQRadSWwinAbsTot.deallocate();
        this->SurfWinQRadSWwinAbsLayer.deallocate();
        this->SurfWinFenLaySurfTempFront.deallocate();
        this->SurfWinFenLaySurfTempBack.deallocate();
        this->SurfWinQRadSWwinAbsTotEnergy.deallocate();
        this->SurfWinSWwinAbsTotalReport.deallocate();
        this->SurfWinInitialDifSolInTransReport.deallocate();
        this->SurfWinQRadSWwinAbs.deallocate();
        this->SurfWinInitialDifSolwinAbs.deallocate();
        this->SurfOpaqSWOutAbsTotalReport.deallocate();
        this->SurfOpaqSWOutAbsEnergyReport.deallocate();
        this->NominalR.deallocate();
        this->NominalRforNominalUCalculation.deallocate();
        this->NominalU.deallocate();
        this->SurfTempEffBulkAir.deallocate();
        this->HConvIn.deallocate();
        this->SurfAnisoSkyMult.deallocate();
        this->DifShdgRatioIsoSky.deallocate();
        this->DifShdgRatioIsoSkyHRTS.deallocate();
        this->curDifShdgRatioIsoSky.deallocate();
        this->DifShdgRatioHoriz.deallocate();
        this->DifShdgRatioHorizHRTS.deallocate();
        this->WithShdgIsoSky.deallocate();
        this->WoShdgIsoSky.deallocate();
        this->WithShdgHoriz.deallocate();
        this->WoShdgHoriz.deallocate();
        this->MultIsoSky.deallocate();
        this->MultCircumSolar.deallocate();
        this->MultHorizonZenith.deallocate();

        this->EnclSolQSWRad.deallocate();
        this->EnclSolQSWRadLights.deallocate();
        this->EnclSolDB.deallocate();
        this->EnclSolDBSSG.deallocate();
        this->EnclSolDBIntWin.deallocate();
        this->EnclSolQSDifSol.deallocate();
        this->EnclSolQD.deallocate();
        this->EnclSolQDforDaylight.deallocate();
        this->EnclSolVMULT.deallocate();
        this->EnclRadQThermalRad.deallocate();
        this->EnclRadThermAbsMult.deallocate();
        this->ZoneSolAbsFirstCalc.deallocate();
        this->EnclRadReCalc.deallocate();
        this->SunlitFracHR.deallocate();
        this->CosIncAngHR.deallocate();
        this->SunlitFrac.deallocate();
        this->SunlitFracWithoutReveal.deallocate();
        this->CosIncAng.deallocate();
        this->BackSurfaces.deallocate();
        this->OverlapAreas.deallocate();
        this->zeroPointerVal = 0.0;
        this->NumAirBoundaryMixing = 0;
        this->AirBoundaryMixingZone1.clear();
        this->AirBoundaryMixingZone2.clear();
        this->AirBoundaryMixingSched.clear();
        this->AirBoundaryMixingVol.clear();
        this->ZonePreDefRep.deallocate();
        this->BuildingPreDefRep = DataHeatBalance::ZonePreDefRepType();
        this->ZoneIntGain.deallocate();
        this->SupportPillar.deallocate();
        this->DeflectionState.deallocate();
        this->SpectralData.deallocate();
        this->Zone.deallocate();
        this->ZoneList.deallocate();
        this->ZoneGroup.deallocate();
        this->People.deallocate();
        this->Lights.deallocate();
        this->ZoneElectric.deallocate();
        this->ZoneGas.deallocate();
        this->ZoneOtherEq.deallocate();
        this->ZoneHWEq.deallocate();
        this->ZoneSteamEq.deallocate();
        this->ZoneITEq.deallocate();
        this->ZoneBBHeat.deallocate();
        this->Infiltration.deallocate();
        this->Ventilation.deallocate();
        this->ZoneAirBalance.deallocate();
        this->Mixing.deallocate();
        this->CrossMixing.deallocate();
        this->RefDoorMixing.deallocate();
        this->Blind.deallocate();
        this->ComplexShade.deallocate();
        this->WindowThermalModel.deallocate();
        this->SurfaceScreens.deallocate();
        this->ScreenTrans.deallocate();
        this->ZoneIntEEuse.deallocate();
        this->RefrigCaseCredit.deallocate();
        this->HeatReclaimRefrigeratedRack.deallocate();
        this->HeatReclaimRefrigCondenser.deallocate();
        this->HeatReclaimDXCoil.deallocate();
        this->HeatReclaimVS_DXCoil.deallocate();
        this->HeatReclaimSimple_WAHPCoil.deallocate();
        this->ZnAirRpt.deallocate();
        this->TCGlazings.deallocate();
        this->ZoneCO2Gen.deallocate();
        this->PeopleObjects.deallocate();
        this->LightsObjects.deallocate();
        this->ZoneElectricObjects.deallocate();
        this->ZoneGasObjects.deallocate();
        this->HotWaterEqObjects.deallocate();
        this->SteamEqObjects.deallocate();
        this->OtherEqObjects.deallocate();
        this->InfiltrationObjects.deallocate();
        this->VentilationObjects.deallocate();
        this->ZnRpt.deallocate();
        this->MassConservation.deallocate();
        this->ZoneAirMassFlow = DataHeatBalance::ZoneAirMassFlowConservation();
        this->ZoneLocalEnvironment.deallocate();
        this->MundtFirstTimeFlag = true;
    }
};

} // namespace EnergyPlus

#endif
