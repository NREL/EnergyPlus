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

// C++ Headers
#include <cmath>

// ObjexxFCL Headers
#include <ObjexxFCL/Array.functions.hh>
#include <ObjexxFCL/Fmath.hh>

// EnergyPlus Headers
#include <EnergyPlus/Construction.hh>
#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataHeatBalance.hh>
#include <EnergyPlus/DataSurfaces.hh>
#include <EnergyPlus/Material.hh>
#include <EnergyPlus/UtilityRoutines.hh>

namespace EnergyPlus::DataHeatBalance {

// MODULE INFORMATION:
//       AUTHOR         Rick Strand
//       DATE WRITTEN   August 1997 (rewritten)
//       MODIFIED       Aug-Oct 1997 RKS (added derived types)
//       MODIFIED       Feb 1999 (FW) Added visible radiation parameters,
//                      WindowShadingControl type and SurfaceWindowCalc type
//                      Sep 1999 (FW) Added/modified Window4 gas variables
//                      Jul 2003 (CC) Added reference temperature variable for air models
//                      Aug 2003 (FW) Added FractionReturnAirPlenTempCoeff1 and
//                      FractionReturnAirPlenTempCoeff2 to Type LightsData
//                      Nov 2003 (FW) Add FullExteriorWithRefl and FullInteriorExteriorWithRefl
//                       as SolarDistribution values
//                      Dec 2003 (PGE) Added Zone List and Zone Group; added SNLoad variables
//                      August 2006 (COP) Added variable k coefficient and PCM enthalpy.
//                      Dec 2006 (DJS-PSU) Added ecoroof material
//                      Dec 2008 TH added new properties to MaterialProperties and
//                              ConstructionData for thermochromic windows
//       RE-ENGINEERED  na

// PURPOSE OF THIS MODULE:
// This module should contain the information that is needed to pass
// from the Heat Balance Module and all of the Zone Initializations
// such as ConductionTransferFunction, GlassCalculation,
// SolarShading, etc. Modules.

// Using/Aliasing
using DataSurfaces::MaxSlatAngs;
using namespace DataVectorTypes;
using DataBSDFWindow::BSDFLayerAbsorpStruct;
using DataBSDFWindow::BSDFWindowInputStruct;

Array1D_string const ZoneIntGainDeviceTypes(NumZoneIntGainDeviceTypes,
                                            {"PEOPLE",
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
                                             "FAN:SYSTEMMODEL"});

Array1D_string const ccZoneIntGainDeviceTypes(NumZoneIntGainDeviceTypes,
                                              {"People",
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
                                               "Fan:SystemModel"});

// Air       Argon     Krypton   Xenon
Array2D<Real64> const GasCoeffsCon(
    3,
    10,
    reshape2<Real64, int>(
        {2.873e-3, 2.285e-3, 9.443e-4, 4.538e-4, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 7.760e-5, 5.149e-5, 2.826e-5, 1.723e-5, 0.0,
         0.0,      0.0,      0.0,      0.0,      0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,      0.0,      0.0,      0.0,      0.0},
        {3, 10})); // Gas conductivity coefficients for gases in a mixture // Explicit reshape2 template args are work-around for VC++2013 bug

// Air       Argon     Krypton   Xenon
Array2D<Real64> const GasCoeffsVis(
    3,
    10,
    reshape2<Real64, int>(
        {3.723e-6, 3.379e-6, 2.213e-6, 1.069e-6, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 4.940e-8, 6.451e-8, 7.777e-8, 7.414e-8, 0.0,
         0.0,      0.0,      0.0,      0.0,      0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,      0.0,      0.0,      0.0,      0.0},
        {3, 10})); // Gas viscosity coefficients for gases in a mixture // Explicit reshape2 template args are work-around for VC++2013 bug

// Air       Argon     Krypton   Xenon
Array2D<Real64> const GasCoeffsCp(
    3,
    10,
    reshape2<Real64, int>(
        {1002.737, 521.929, 248.091, 158.340, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.2324e-2, 0.0, 0.0, 0.0, 0.0,
         0.0,      0.0,     0.0,     0.0,     0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,       0.0, 0.0, 0.0, 0.0},
        {3, 10})); // Gas specific heat coefficients for gases in a mixture // Explicit reshape2 template args are work-around for VC++2013 bug

// Air       Argon     Krypton   Xenon
Array1D<Real64> const GasWght(10, {28.97, 39.948, 83.8, 131.3, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0}); // Gas molecular weights for gases in a mixture

// Gas specific heat ratios.  Used for gasses in low pressure
Array1D<Real64> const GasSpecificHeatRatio(10, {1.4, 1.67, 1.68, 1.66, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0});

// Functions

void ZoneData::SetOutBulbTempAt(EnergyPlusData &state)
{
    // SUBROUTINE INFORMATION:
    //       AUTHOR         Noel Keen (LBL)/Linda Lawrie
    //       DATE WRITTEN   August 2010
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // Routine provides facility for doing bulk Set Temperature at Height.

    if (state.dataEnvrn->SiteTempGradient == 0.0) {
        OutDryBulbTemp = state.dataEnvrn->OutDryBulbTemp;
        OutWetBulbTemp = state.dataEnvrn->OutWetBulbTemp;
    } else {
        // Base temperatures at Z = 0 (C)
        Real64 const BaseDryTemp(state.dataEnvrn->OutDryBulbTemp + state.dataEnvrn->WeatherFileTempModCoeff);
        Real64 const BaseWetTemp(state.dataEnvrn->OutWetBulbTemp + state.dataEnvrn->WeatherFileTempModCoeff);

        Real64 const Z(Centroid.z); // Centroid value
        if (Z <= 0.0) {
            OutDryBulbTemp = BaseDryTemp;
            OutWetBulbTemp = BaseWetTemp;
        } else {
            OutDryBulbTemp = BaseDryTemp - state.dataEnvrn->SiteTempGradient * DataEnvironment::EarthRadius * Z / (DataEnvironment::EarthRadius + Z);
            OutWetBulbTemp = BaseWetTemp - state.dataEnvrn->SiteTempGradient * DataEnvironment::EarthRadius * Z / (DataEnvironment::EarthRadius + Z);
        }
    }
}

void ZoneData::SetWindSpeedAt(EnergyPlusData &state, Real64 const fac)
{
    // SUBROUTINE INFORMATION:
    //       AUTHOR         Linda Lawrie
    //       DATE WRITTEN   June 2013
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // Routine provides facility for doing bulk Set Windspeed at Height.

    if (state.dataEnvrn->SiteWindExp == 0.0) {
        WindSpeed = state.dataEnvrn->WindSpeed;
    } else {
        Real64 const Z(Centroid.z); // Centroid value
        if (Z <= 0.0) {
            WindSpeed = 0.0;
        } else {
            //  [Met] - at meterological Station, Height of measurement is usually 10m above ground
            //  LocalWindSpeed = Windspeed [Met] * (Wind Boundary LayerThickness [Met]/Height [Met])**Wind Exponent[Met] &
            //                     * (Height above ground / Site Wind Boundary Layer Thickness) ** Site Wind Exponent
            WindSpeed = fac * std::pow(Z, state.dataEnvrn->SiteWindExp);
        }
    }
}

void ZoneData::SetWindDirAt(Real64 const fac)
{
    WindDir = fac;
}

void SetZoneOutBulbTempAt(EnergyPlusData &state)
{
    for (auto &zone : state.dataHeatBal->Zone) {
        zone.SetOutBulbTempAt(state);
    }
}

void CheckZoneOutBulbTempAt(EnergyPlusData &state)
{
    // Using/Aliasing
    using DataEnvironment::SetOutBulbTempAt_error;

    Real64 minBulb = 0.0;
    for (auto &zone : state.dataHeatBal->Zone) {
        minBulb = min(minBulb, zone.OutDryBulbTemp, zone.OutWetBulbTemp);
        if (minBulb < -100.0) SetOutBulbTempAt_error(state, "Zone", zone.Centroid.z, zone.Name);
    }
}

void SetZoneWindSpeedAt(EnergyPlusData &state)
{
    Real64 const fac(state.dataEnvrn->WindSpeed * state.dataEnvrn->WeatherFileWindModCoeff *
                     std::pow(state.dataEnvrn->SiteWindBLHeight, -state.dataEnvrn->SiteWindExp));
    for (auto &zone : state.dataHeatBal->Zone) {
        zone.SetWindSpeedAt(state, fac);
    }
}

void SetZoneWindDirAt(EnergyPlusData &state)
{
    // Using/Aliasing
    Real64 const fac(state.dataEnvrn->WindDir);
    for (auto &zone : state.dataHeatBal->Zone) {
        zone.SetWindDirAt(fac);
    }
}

void CheckAndSetConstructionProperties(EnergyPlusData &state,
                                       int const ConstrNum, // Construction number to be set/checked
                                       bool &ErrorsFound    // error flag that is set when certain errors have occurred
)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Linda Lawrie
    //       DATE WRITTEN   December 2006

    // This routine checks some properties of entered constructions; sets some properties; and sets
    // an error flag for certain error conditions.

    int InsideLayer;             // Inside Layer of Construct; for window construct, layer no. of inside glass
    int MaterNum;                // Counters to keep track of the material number for a layer
    int OutsideMaterNum;         // Material "number" of the Outside layer
    int InsideMaterNum;          // Material "number" of the Inside layer
    int Layer;                   // loop index for each of the construction layers
    int TotLayers;               // Number of layers in a construction
    int TotGlassLayers;          // Number of glass layers in a construction
    int TotShadeLayers;          // Number of shade layers in a construction
    int TotGasLayers;            // Number of gas layers in a construction
    bool WrongMaterialsMix;      // True if window construction has a layer that is not glass, gas or shade
    bool WrongWindowLayering;    // True if error in layering of a window construction
    int MaterNumNext;            // Next material number in the layer sequence
    int IGas;                    // Index for gases in a mixture of gases in a window gap
    int LayNumSh;                // Number of shade/blind layer in a construction
    int MatSh;                   // Material number of a shade/blind layer
    int MatGapL;                 // Material number of the gas layer to the left (outer side) of a shade/blind layer
    int MatGapR;                 // Material number of the gas layer to the right (innner side) of a shade/blind layer
    int BlNum;                   // Blind number
    bool ValidBGShadeBlindConst; // True if a valid window construction with between-glass shade/blind
    int GlassLayNum;             // Glass layer number

    TotLayers = state.dataConstruction->Construct(ConstrNum).TotLayers;
    if (TotLayers == 0) return; // error condition, hopefully caught elsewhere
    InsideLayer = TotLayers;
    if (state.dataConstruction->Construct(ConstrNum).LayerPoint(InsideLayer) <= 0) return; // Error condition

    //   window screen is not allowed on inside layer

    state.dataConstruction->Construct(ConstrNum).DayltPropPtr = 0;
    InsideMaterNum = state.dataConstruction->Construct(ConstrNum).LayerPoint(InsideLayer);
    if (InsideMaterNum != 0) {
        state.dataConstruction->Construct(ConstrNum).InsideAbsorpVis = state.dataMaterial->Material(InsideMaterNum).AbsorpVisible;
        state.dataConstruction->Construct(ConstrNum).InsideAbsorpSolar = state.dataMaterial->Material(InsideMaterNum).AbsorpSolar;

        // Following line applies only to opaque surfaces; it is recalculated later for windows.
        state.dataConstruction->Construct(ConstrNum).ReflectVisDiffBack = 1.0 - state.dataMaterial->Material(InsideMaterNum).AbsorpVisible;
    }

    OutsideMaterNum = state.dataConstruction->Construct(ConstrNum).LayerPoint(1);
    if (OutsideMaterNum != 0) {
        state.dataConstruction->Construct(ConstrNum).OutsideAbsorpVis = state.dataMaterial->Material(OutsideMaterNum).AbsorpVisible;
        state.dataConstruction->Construct(ConstrNum).OutsideAbsorpSolar = state.dataMaterial->Material(OutsideMaterNum).AbsorpSolar;
    }

    state.dataConstruction->Construct(ConstrNum).TotSolidLayers = 0;
    state.dataConstruction->Construct(ConstrNum).TotGlassLayers = 0;
    state.dataConstruction->Construct(ConstrNum).AbsDiffShade = 0.0;

    // Check if any layer is glass, gas, shade, screen or blind; if so it is considered a window construction for
    // purposes of error checking.

    state.dataConstruction->Construct(ConstrNum).TypeIsWindow = false;
    for (Layer = 1; Layer <= TotLayers; ++Layer) {
        MaterNum = state.dataConstruction->Construct(ConstrNum).LayerPoint(Layer);
        if (MaterNum == 0) continue; // error -- has been caught will stop program later
        switch (state.dataMaterial->Material(MaterNum).Group) {
        case WindowGlass:
        case WindowGas:
        case WindowGasMixture:
        case Shade:
        case WindowBlind:
        case Screen:
        case WindowSimpleGlazing:
        case ComplexWindowShade:
        case ComplexWindowGap:
        case GlassEquivalentLayer:
        case ShadeEquivalentLayer:
        case DrapeEquivalentLayer:
        case ScreenEquivalentLayer:
        case BlindEquivalentLayer:
        case GapEquivalentLayer:
            state.dataConstruction->Construct(ConstrNum).TypeIsWindow = true;
        }
    }

    if (InsideMaterNum == 0) return;
    if (OutsideMaterNum == 0) return;

    if (state.dataConstruction->Construct(ConstrNum).TypeIsWindow) {

        state.dataConstruction->Construct(ConstrNum).NumCTFTerms = 0;
        state.dataConstruction->Construct(ConstrNum).NumHistories = 0;
        WrongMaterialsMix = false;
        WrongWindowLayering = false;
        for (Layer = 1; Layer <= TotLayers; ++Layer) {
            MaterNum = state.dataConstruction->Construct(ConstrNum).LayerPoint(Layer);
            if (MaterNum == 0) continue; // error -- has been caught will stop program later
            switch (state.dataMaterial->Material(MaterNum).Group) {
            case WindowGlass:
            case WindowGas:
            case WindowGasMixture:
            case Shade:
            case WindowBlind:
            case Screen:
            case WindowSimpleGlazing:
            case ComplexWindowShade:
            case ComplexWindowGap:
            case GlassEquivalentLayer:
            case ShadeEquivalentLayer:
            case DrapeEquivalentLayer:
            case ScreenEquivalentLayer:
            case BlindEquivalentLayer:
            case GapEquivalentLayer:
                break; // everything is OK
            default:
                WrongMaterialsMix = true; // found a bad one
            }
        }

        if (WrongMaterialsMix) { // Illegal material for a window construction
            ShowSevereError(state,
                            "Error: Window construction=" + state.dataConstruction->Construct(ConstrNum).Name +
                                " has materials other than glass, gas, shade, screen, blind, complex shading, complex gap, or simple system.");
            ErrorsFound = true;
            // Do not check number of layers for BSDF type of window since that can be handled
        } else if ((TotLayers > 8) && (!state.dataConstruction->Construct(ConstrNum).WindowTypeBSDF) &&
                   (!state.dataConstruction->Construct(ConstrNum).WindowTypeEQL)) { // Too many layers for a window construction
            ShowSevereError(state,
                            "CheckAndSetConstructionProperties: Window construction=" + state.dataConstruction->Construct(ConstrNum).Name +
                                " has too many layers (max of 8 allowed -- 4 glass + 3 gap + 1 shading device).");
            ErrorsFound = true;

        } else if (TotLayers == 1) {

            if (state.dataMaterial->Material(state.dataConstruction->Construct(ConstrNum).LayerPoint(1)).Group == Shade ||
                state.dataMaterial->Material(state.dataConstruction->Construct(ConstrNum).LayerPoint(1)).Group == WindowGas ||
                state.dataMaterial->Material(state.dataConstruction->Construct(ConstrNum).LayerPoint(1)).Group == WindowGasMixture ||
                state.dataMaterial->Material(state.dataConstruction->Construct(ConstrNum).LayerPoint(1)).Group == WindowBlind ||
                state.dataMaterial->Material(state.dataConstruction->Construct(ConstrNum).LayerPoint(1)).Group == Screen ||
                state.dataMaterial->Material(state.dataConstruction->Construct(ConstrNum).LayerPoint(1)).Group == ComplexWindowShade ||
                state.dataMaterial->Material(state.dataConstruction->Construct(ConstrNum).LayerPoint(1)).Group == ComplexWindowGap) {
                ShowSevereError(
                    state,
                    "CheckAndSetConstructionProperties: The single-layer window construction=" + state.dataConstruction->Construct(ConstrNum).Name +
                        " has a gas, complex gap, shade, complex shade, screen or blind material; it should be glass of simple glazing system.");
                ErrorsFound = true;
            }
        }

        // Find total glass layers, total shade/blind layers and total gas layers in a window construction

        TotGlassLayers = 0;
        TotShadeLayers = 0; // Includes shades, blinds, and screens
        TotGasLayers = 0;
        for (Layer = 1; Layer <= TotLayers; ++Layer) {
            MaterNum = state.dataConstruction->Construct(ConstrNum).LayerPoint(Layer);
            if (MaterNum == 0) continue; // error -- has been caught will stop program later
            if (state.dataMaterial->Material(MaterNum).Group == WindowGlass) ++TotGlassLayers;
            if (state.dataMaterial->Material(MaterNum).Group == WindowSimpleGlazing) ++TotGlassLayers;
            if (state.dataMaterial->Material(MaterNum).Group == Shade || state.dataMaterial->Material(MaterNum).Group == WindowBlind ||
                state.dataMaterial->Material(MaterNum).Group == Screen || state.dataMaterial->Material(MaterNum).Group == ComplexWindowShade)
                ++TotShadeLayers;
            if (state.dataMaterial->Material(MaterNum).Group == WindowGas || state.dataMaterial->Material(MaterNum).Group == WindowGasMixture ||
                state.dataMaterial->Material(MaterNum).Group == ComplexWindowGap)
                ++TotGasLayers;
            if (Layer < TotLayers) {
                MaterNumNext = state.dataConstruction->Construct(ConstrNum).LayerPoint(Layer + 1);
                // Adjacent layers of same type not allowed
                if (MaterNumNext == 0) continue;
                if (state.dataMaterial->Material(MaterNum).Group == state.dataMaterial->Material(MaterNumNext).Group) WrongWindowLayering = true;
            }
        }

        // It is not necessary to check rest of BSDF window structure since that is performed inside TARCOG90 routine.
        // That routine also allow structures which are not allowed in rest of this routine
        if (state.dataConstruction->Construct(ConstrNum).WindowTypeBSDF) {
            state.dataConstruction->Construct(ConstrNum).TotGlassLayers = TotGlassLayers;
            state.dataConstruction->Construct(ConstrNum).TotSolidLayers = TotGlassLayers + TotShadeLayers;
            state.dataConstruction->Construct(ConstrNum).InsideAbsorpThermal =
                state.dataMaterial->Material(state.dataConstruction->Construct(ConstrNum).LayerPoint(InsideLayer)).AbsorpThermalBack;
            state.dataConstruction->Construct(ConstrNum).OutsideAbsorpThermal =
                state.dataMaterial->Material(state.dataConstruction->Construct(ConstrNum).LayerPoint(1)).AbsorpThermalFront;
            return;
        }

        if (state.dataConstruction->Construct(ConstrNum).WindowTypeEQL) {
            state.dataConstruction->Construct(ConstrNum).InsideAbsorpThermal =
                state.dataMaterial->Material(state.dataConstruction->Construct(ConstrNum).LayerPoint(InsideLayer)).AbsorpThermalBack;
            state.dataConstruction->Construct(ConstrNum).OutsideAbsorpThermal =
                state.dataMaterial->Material(state.dataConstruction->Construct(ConstrNum).LayerPoint(1)).AbsorpThermalFront;
            return;
        }

        if (state.dataMaterial->Material(state.dataConstruction->Construct(ConstrNum).LayerPoint(1)).Group == WindowGas ||
            state.dataMaterial->Material(state.dataConstruction->Construct(ConstrNum).LayerPoint(1)).Group == WindowGasMixture ||
            state.dataMaterial->Material(state.dataConstruction->Construct(ConstrNum).LayerPoint(TotLayers)).Group == WindowGas ||
            state.dataMaterial->Material(state.dataConstruction->Construct(ConstrNum).LayerPoint(TotLayers)).Group == WindowGasMixture)
            WrongWindowLayering = true;                     // Gas cannot be first or last layer
        if (TotShadeLayers > 1) WrongWindowLayering = true; // At most one shade, screen or blind allowed

        // If there is a diffusing glass layer no shade, screen or blind is allowed
        for (Layer = 1; Layer <= TotLayers; ++Layer) {
            MaterNum = state.dataConstruction->Construct(ConstrNum).LayerPoint(Layer);
            if (MaterNum == 0) continue; // error -- has been caught will stop program later
            if (state.dataMaterial->Material(MaterNum).SolarDiffusing && TotShadeLayers > 0) {
                ErrorsFound = true;
                ShowSevereError(state, "CheckAndSetConstructionProperties: Window construction=" + state.dataConstruction->Construct(ConstrNum).Name);
                ShowContinueError(state,
                                  "has diffusing glass=" + state.dataMaterial->Material(MaterNum).Name + " and a shade, screen or blind layer.");
                break;
            }
        }

        // If there is a diffusing glass layer it must be the innermost layer
        if (TotGlassLayers > 1) {
            GlassLayNum = 0;
            for (Layer = 1; Layer <= TotLayers; ++Layer) {
                MaterNum = state.dataConstruction->Construct(ConstrNum).LayerPoint(Layer);
                if (MaterNum == 0) continue; // error -- has been caught will stop program later
                if (state.dataMaterial->Material(MaterNum).Group == WindowGlass) {
                    ++GlassLayNum;
                    if (GlassLayNum < TotGlassLayers && state.dataMaterial->Material(MaterNum).SolarDiffusing) {
                        ErrorsFound = true;
                        ShowSevereError(
                            state, "CheckAndSetConstructionProperties: Window construction=" + state.dataConstruction->Construct(ConstrNum).Name);
                        ShowContinueError(
                            state, "has diffusing glass=" + state.dataMaterial->Material(MaterNum).Name + " that is not the innermost glass layer.");
                    }
                }
            }
        }

        // interior window screen is not allowed. Check for invalid between-glass screen is checked below.
        if (TotShadeLayers == 1 && state.dataMaterial->Material(state.dataConstruction->Construct(ConstrNum).LayerPoint(TotLayers)).Group == Screen &&
            TotLayers != 1) {
            WrongWindowLayering = true;
        }

        // Consistency checks for a construction with a between-glass shade or blind

        if (TotShadeLayers == 1 && state.dataMaterial->Material(state.dataConstruction->Construct(ConstrNum).LayerPoint(1)).Group != Shade &&
            state.dataMaterial->Material(state.dataConstruction->Construct(ConstrNum).LayerPoint(1)).Group != WindowBlind &&
            state.dataMaterial->Material(state.dataConstruction->Construct(ConstrNum).LayerPoint(1)).Group != Screen &&
            state.dataMaterial->Material(state.dataConstruction->Construct(ConstrNum).LayerPoint(TotLayers)).Group != Shade &&
            state.dataMaterial->Material(state.dataConstruction->Construct(ConstrNum).LayerPoint(TotLayers)).Group != WindowBlind &&
            state.dataMaterial->Material(state.dataConstruction->Construct(ConstrNum).LayerPoint(TotLayers)).Group != ComplexWindowShade &&
            !WrongWindowLayering) {

            // This is a construction with a between-glass shade or blind

            if (TotGlassLayers >= 4) {
                // Quadruple pane not allowed.
                WrongWindowLayering = true;
            } else if (TotGlassLayers == 2 || TotGlassLayers == 3) {
                ValidBGShadeBlindConst = false;
                if (TotGlassLayers == 2) {
                    if (TotLayers != 5) {
                        WrongWindowLayering = true;
                    } else {
                        if (state.dataMaterial->Material(state.dataConstruction->Construct(ConstrNum).LayerPoint(1)).Group == WindowGlass &&
                            (state.dataMaterial->Material(state.dataConstruction->Construct(ConstrNum).LayerPoint(2)).Group == WindowGas ||
                             state.dataMaterial->Material(state.dataConstruction->Construct(ConstrNum).LayerPoint(2)).Group == WindowGasMixture) &&
                            ((state.dataMaterial->Material(state.dataConstruction->Construct(ConstrNum).LayerPoint(3)).Group == Shade ||
                              state.dataMaterial->Material(state.dataConstruction->Construct(ConstrNum).LayerPoint(3)).Group == WindowBlind) &&
                             state.dataMaterial->Material(state.dataConstruction->Construct(ConstrNum).LayerPoint(3)).Group != Screen) &&
                            (state.dataMaterial->Material(state.dataConstruction->Construct(ConstrNum).LayerPoint(4)).Group == WindowGas ||
                             state.dataMaterial->Material(state.dataConstruction->Construct(ConstrNum).LayerPoint(4)).Group == WindowGasMixture) &&
                            state.dataMaterial->Material(state.dataConstruction->Construct(ConstrNum).LayerPoint(5)).Group == WindowGlass)
                            ValidBGShadeBlindConst = true;
                    }
                } else { // TotGlassLayers = 3
                    if (TotLayers != 7) {
                        WrongWindowLayering = true;
                    } else {
                        if (state.dataMaterial->Material(state.dataConstruction->Construct(ConstrNum).LayerPoint(1)).Group == WindowGlass &&
                            (state.dataMaterial->Material(state.dataConstruction->Construct(ConstrNum).LayerPoint(2)).Group == WindowGas ||
                             state.dataMaterial->Material(state.dataConstruction->Construct(ConstrNum).LayerPoint(2)).Group == WindowGasMixture) &&
                            state.dataMaterial->Material(state.dataConstruction->Construct(ConstrNum).LayerPoint(3)).Group == WindowGlass &&
                            (state.dataMaterial->Material(state.dataConstruction->Construct(ConstrNum).LayerPoint(4)).Group == WindowGas ||
                             state.dataMaterial->Material(state.dataConstruction->Construct(ConstrNum).LayerPoint(4)).Group == WindowGasMixture) &&
                            ((state.dataMaterial->Material(state.dataConstruction->Construct(ConstrNum).LayerPoint(5)).Group == Shade ||
                              state.dataMaterial->Material(state.dataConstruction->Construct(ConstrNum).LayerPoint(5)).Group == WindowBlind) &&
                             state.dataMaterial->Material(state.dataConstruction->Construct(ConstrNum).LayerPoint(5)).Group != Screen) &&
                            (state.dataMaterial->Material(state.dataConstruction->Construct(ConstrNum).LayerPoint(6)).Group == WindowGas ||
                             state.dataMaterial->Material(state.dataConstruction->Construct(ConstrNum).LayerPoint(6)).Group == WindowGasMixture) &&
                            state.dataMaterial->Material(state.dataConstruction->Construct(ConstrNum).LayerPoint(7)).Group == WindowGlass)
                            ValidBGShadeBlindConst = true;
                    }
                } // End of check if TotGlassLayers = 2 or 3
                if (!ValidBGShadeBlindConst) WrongWindowLayering = true;
                if (!WrongWindowLayering) {
                    LayNumSh = 2 * TotGlassLayers - 1;
                    MatSh = state.dataConstruction->Construct(ConstrNum).LayerPoint(LayNumSh);
                    // For double pane, shade/blind must be layer #3.
                    // For triple pane, it must be layer #5 (i.e., between two inner panes).
                    if (state.dataMaterial->Material(MatSh).Group != Shade && state.dataMaterial->Material(MatSh).Group != WindowBlind)
                        WrongWindowLayering = true;
                    if (TotLayers != 2 * TotGlassLayers + 1) WrongWindowLayering = true;
                    if (!WrongWindowLayering) {
                        // Gas on either side of a between-glass shade/blind must be the same
                        MatGapL = state.dataConstruction->Construct(ConstrNum).LayerPoint(LayNumSh - 1);
                        MatGapR = state.dataConstruction->Construct(ConstrNum).LayerPoint(LayNumSh + 1);
                        for (IGas = 1; IGas <= 5; ++IGas) {
                            if ((state.dataMaterial->Material(MatGapL).GasType(IGas) != state.dataMaterial->Material(MatGapR).GasType(IGas)) ||
                                (state.dataMaterial->Material(MatGapL).GasFract(IGas) != state.dataMaterial->Material(MatGapR).GasFract(IGas)))
                                WrongWindowLayering = true;
                        }
                        // Gap width on either side of a between-glass shade/blind must be the same
                        if (std::abs(state.dataMaterial->Material(MatGapL).Thickness - state.dataMaterial->Material(MatGapR).Thickness) > 0.0005)
                            WrongWindowLayering = true;
                        if (state.dataMaterial->Material(MatSh).Group == WindowBlind) {
                            BlNum = state.dataMaterial->Material(MatSh).BlindDataPtr;
                            if (BlNum > 0) {
                                if ((state.dataMaterial->Material(MatGapL).Thickness + state.dataMaterial->Material(MatGapR).Thickness) <
                                    state.dataHeatBal->Blind(BlNum).SlatWidth) {
                                    ErrorsFound = true;
                                    ShowSevereError(state,
                                                    "CheckAndSetConstructionProperties: For window construction " +
                                                        state.dataConstruction->Construct(ConstrNum).Name);
                                    ShowContinueError(state, "the slat width of the between-glass blind is greater than");
                                    ShowContinueError(state, "the sum of the widths of the gas layers adjacent to the blind.");
                                }
                            } // End of check if BlNum > 0
                        }     // End of check if material is window blind
                    }         // End of check if WrongWindowLayering
                }             // End of check if WrongWindowLayering
            }                 // End of check on total glass layers
        }                     // End of check if construction has between-glass shade/blind

        // Check Simple Windows,
        if (state.dataMaterial->Material(state.dataConstruction->Construct(ConstrNum).LayerPoint(1)).Group == WindowSimpleGlazing) {
            if (TotLayers > 1) {
                // check that none of the other layers are glazing or gas
                for (Layer = 1; Layer <= TotLayers; ++Layer) {
                    MaterNum = state.dataConstruction->Construct(ConstrNum).LayerPoint(Layer);
                    if (MaterNum == 0) continue; // error -- has been caught will stop program later
                    if (state.dataMaterial->Material(MaterNum).Group == WindowGlass) {
                        ErrorsFound = true;
                        ShowSevereError(state,
                                        "CheckAndSetConstructionProperties: Error in window construction " +
                                            state.dataConstruction->Construct(ConstrNum).Name + "--");
                        ShowContinueError(state, "For simple window constructions, no other glazing layers are allowed.");
                    }
                    if (state.dataMaterial->Material(MaterNum).Group == WindowGas) {
                        ErrorsFound = true;
                        ShowSevereError(state,
                                        "CheckAndSetConstructionProperties: Error in window construction " +
                                            state.dataConstruction->Construct(ConstrNum).Name + "--");
                        ShowContinueError(state, "For simple window constructions, no other gas layers are allowed.");
                    }
                }
            }
        }

        if (WrongWindowLayering) {
            ShowSevereError(
                state, "CheckAndSetConstructionProperties: Error in window construction " + state.dataConstruction->Construct(ConstrNum).Name + "--");
            ShowContinueError(state, "  For multi-layer window constructions the following rules apply:");
            ShowContinueError(state, "    --The first and last layer must be a solid layer (glass or shade/screen/blind),");
            ShowContinueError(state, "    --Adjacent glass layers must be separated by one and only one gas layer,");
            ShowContinueError(state, "    --Adjacent layers must not be of the same type,");
            ShowContinueError(state, "    --Only one shade/screen/blind layer is allowed,");
            ShowContinueError(state, "    --An exterior shade/screen/blind must be the first layer,");
            ShowContinueError(state, "    --An interior shade/blind must be the last layer,");
            ShowContinueError(state, "    --An interior screen is not allowed,");
            ShowContinueError(state, "    --For an exterior shade/screen/blind or interior shade/blind, there should not be a gas layer");
            ShowContinueError(state, "    ----between the shade/screen/blind and adjacent glass,");
            ShowContinueError(state, "    --A between-glass screen is not allowed,");
            ShowContinueError(state, "    --A between-glass shade/blind is allowed only for double and triple glazing,");
            ShowContinueError(state, "    --A between-glass shade/blind must have adjacent gas layers of the same type and width,");
            ShowContinueError(state, "    --For triple glazing the between-glass shade/blind must be between the two inner glass layers,");
            ShowContinueError(state, "    --The slat width of a between-glass blind must be less than the sum of the widths");
            ShowContinueError(state, "    ----of the gas layers adjacent to the blind.");
            ErrorsFound = true;
        }

        state.dataConstruction->Construct(ConstrNum).TotGlassLayers = TotGlassLayers;
        state.dataConstruction->Construct(ConstrNum).TotSolidLayers = TotGlassLayers + TotShadeLayers;

        // In following, InsideLayer is layer number of inside glass and InsideAbsorpThermal applies
        // only to inside glass; it is corrected later in InitGlassOpticalCalculations
        // if construction has inside shade or blind.
        if (state.dataMaterial->Material(state.dataConstruction->Construct(ConstrNum).LayerPoint(InsideLayer)).Group == Shade ||
            state.dataMaterial->Material(state.dataConstruction->Construct(ConstrNum).LayerPoint(InsideLayer)).Group == WindowBlind) {
            --InsideLayer;
        }
        if (InsideLayer > 0) {
            InsideMaterNum = state.dataConstruction->Construct(ConstrNum).LayerPoint(InsideLayer);
            state.dataConstruction->Construct(ConstrNum).InsideAbsorpThermal =
                state.dataMaterial->Material(state.dataConstruction->Construct(ConstrNum).LayerPoint(InsideLayer)).AbsorpThermalBack;
        }
        if (InsideMaterNum != 0) {
            state.dataConstruction->Construct(ConstrNum).InsideAbsorpVis = state.dataMaterial->Material(InsideMaterNum).AbsorpVisible;
            state.dataConstruction->Construct(ConstrNum).InsideAbsorpSolar = state.dataMaterial->Material(InsideMaterNum).AbsorpSolar;
        }

        if ((state.dataMaterial->Material(state.dataConstruction->Construct(ConstrNum).LayerPoint(1)).Group == WindowGlass) ||
            (state.dataMaterial->Material(state.dataConstruction->Construct(ConstrNum).LayerPoint(1)).Group == WindowSimpleGlazing)) { // Glass
            state.dataConstruction->Construct(ConstrNum).OutsideAbsorpThermal =
                state.dataMaterial->Material(state.dataConstruction->Construct(ConstrNum).LayerPoint(1)).AbsorpThermalFront;
        } else { // Exterior shade, blind or screen
            state.dataConstruction->Construct(ConstrNum).OutsideAbsorpThermal =
                state.dataMaterial->Material(state.dataConstruction->Construct(ConstrNum).LayerPoint(1)).AbsorpThermal;
        }

    } else { // Opaque surface
        state.dataConstruction->Construct(ConstrNum).InsideAbsorpThermal =
            state.dataMaterial->Material(state.dataConstruction->Construct(ConstrNum).LayerPoint(InsideLayer)).AbsorpThermal;
        state.dataConstruction->Construct(ConstrNum).OutsideAbsorpThermal =
            state.dataMaterial->Material(state.dataConstruction->Construct(ConstrNum).LayerPoint(1)).AbsorpThermal;
    }

    state.dataConstruction->Construct(ConstrNum).OutsideRoughness =
        state.dataMaterial->Material(state.dataConstruction->Construct(ConstrNum).LayerPoint(1)).Roughness;

    if (state.dataMaterial->Material(state.dataConstruction->Construct(ConstrNum).LayerPoint(1)).Group == Air) {
        ShowSevereError(
            state, "CheckAndSetConstructionProperties: Outside Layer is Air for construction " + state.dataConstruction->Construct(ConstrNum).Name);
        ShowContinueError(state,
                          "  Error in material " + state.dataMaterial->Material(state.dataConstruction->Construct(ConstrNum).LayerPoint(1)).Name);
        ErrorsFound = true;
    }
    if (InsideLayer > 0) {
        if (state.dataMaterial->Material(state.dataConstruction->Construct(ConstrNum).LayerPoint(InsideLayer)).Group == Air) {
            ShowSevereError(state,
                            "CheckAndSetConstructionProperties: Inside Layer is Air for construction " +
                                state.dataConstruction->Construct(ConstrNum).Name);
            ShowContinueError(state,
                              "  Error in material " +
                                  state.dataMaterial->Material(state.dataConstruction->Construct(ConstrNum).LayerPoint(InsideLayer)).Name);
            ErrorsFound = true;
        }
    }

    if (state.dataMaterial->Material(state.dataConstruction->Construct(ConstrNum).LayerPoint(1)).Group == EcoRoof) {
        state.dataConstruction->Construct(ConstrNum).TypeIsEcoRoof = true;
        // need to check EcoRoof is not non-outside layer
        for (Layer = 2; Layer <= TotLayers; ++Layer) {
            if (state.dataMaterial->Material(state.dataConstruction->Construct(ConstrNum).LayerPoint(Layer)).Group == EcoRoof) {
                ShowSevereError(state,
                                "CheckAndSetConstructionProperties: Interior Layer is EcoRoof for construction " +
                                    state.dataConstruction->Construct(ConstrNum).Name);
                ShowContinueError(state,
                                  "  Error in material " +
                                      state.dataMaterial->Material(state.dataConstruction->Construct(ConstrNum).LayerPoint(Layer)).Name);
                ErrorsFound = true;
            }
        }
    }

    if (state.dataMaterial->Material(state.dataConstruction->Construct(ConstrNum).LayerPoint(1)).Group == IRTMaterial) {
        state.dataConstruction->Construct(ConstrNum).TypeIsIRT = true;
        if (state.dataConstruction->Construct(ConstrNum).TotLayers != 1) {
            ShowSevereError(state,
                            "CheckAndSetConstructionProperties: Infrared Transparent (IRT) Construction is limited to 1 layer " +
                                state.dataConstruction->Construct(ConstrNum).Name);
            ShowContinueError(state, "  Too many layers in referenced construction.");
            ErrorsFound = true;
        }
    }
}

int AssignReverseConstructionNumber(EnergyPlusData &state,
                                    int const ConstrNum, // Existing Construction number of first surface
                                    bool &ErrorsFound)
{

    // FUNCTION INFORMATION:
    //       AUTHOR         Linda Lawrie
    //       DATE WRITTEN   December 2006
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS FUNCTION:
    // For interzone, unentered surfaces, we need to have "reverse" constructions
    // assigned to the created surfaces.  These need to be the reverse (outside to inside layer)
    // of existing surfaces.  Plus, there may be one already in the data structure so this is looked for as well.

    // METHODOLOGY EMPLOYED:
    // Create reverse layers.  Look in current constructions to see if match.  If no match, create a new one.

    // Return value
    int NewConstrNum; // Reverse Construction Number

    // FUNCTION LOCAL VARIABLE DECLARATIONS:
    int nLayer;
    int Loop;
    bool Found;

    if (ConstrNum == 0) {
        // error caught elsewhere
        NewConstrNum = 0;
        return NewConstrNum;
    }

    state.dataConstruction->Construct(ConstrNum).IsUsed = true;
    nLayer = 0;
    state.dataConstruction->LayerPoint = 0;
    for (Loop = state.dataConstruction->Construct(ConstrNum).TotLayers; Loop >= 1; --Loop) {
        ++nLayer;
        state.dataConstruction->LayerPoint(nLayer) = state.dataConstruction->Construct(ConstrNum).LayerPoint(Loop);
    }

    // now, got thru and see if there is a match already....
    NewConstrNum = 0;
    for (Loop = 1; Loop <= state.dataHeatBal->TotConstructs; ++Loop) {
        Found = true;
        for (nLayer = 1; nLayer <= Construction::MaxLayersInConstruct; ++nLayer) {
            if (state.dataConstruction->Construct(Loop).LayerPoint(nLayer) != state.dataConstruction->LayerPoint(nLayer)) {
                Found = false;
                break;
            }
        }
        if (Found) {
            NewConstrNum = Loop;
            break;
        }
    }

    // if need new one, bunch o stuff
    if (NewConstrNum == 0) {
        ++state.dataHeatBal->TotConstructs;
        state.dataConstruction->Construct.redimension(state.dataHeatBal->TotConstructs);
        state.dataHeatBal->NominalRforNominalUCalculation.redimension(state.dataHeatBal->TotConstructs);
        state.dataHeatBal->NominalRforNominalUCalculation(state.dataHeatBal->TotConstructs) = 0.0;
        state.dataHeatBal->NominalU.redimension(state.dataHeatBal->TotConstructs);
        state.dataHeatBal->NominalU(state.dataHeatBal->TotConstructs) = 0.0;
        //  Put in new attributes
        NewConstrNum = state.dataHeatBal->TotConstructs;
        state.dataConstruction->Construct(NewConstrNum).IsUsed = true;
        state.dataConstruction->Construct(state.dataHeatBal->TotConstructs) =
            state.dataConstruction->Construct(ConstrNum); // preserve some of the attributes.
        // replace others...
        state.dataConstruction->Construct(state.dataHeatBal->TotConstructs).Name = "iz-" + state.dataConstruction->Construct(ConstrNum).Name;
        state.dataConstruction->Construct(state.dataHeatBal->TotConstructs).TotLayers = state.dataConstruction->Construct(ConstrNum).TotLayers;
        for (nLayer = 1; nLayer <= Construction::MaxLayersInConstruct; ++nLayer) {
            state.dataConstruction->Construct(state.dataHeatBal->TotConstructs).LayerPoint(nLayer) = state.dataConstruction->LayerPoint(nLayer);
            if (state.dataConstruction->LayerPoint(nLayer) != 0) {
                state.dataHeatBal->NominalRforNominalUCalculation(state.dataHeatBal->TotConstructs) +=
                    state.dataHeatBal->NominalR(state.dataConstruction->LayerPoint(nLayer));
            }
        }

        // no error if zero -- that will have been caught with earlier construction
        // the following line was changed to fix CR7601
        if (state.dataHeatBal->NominalRforNominalUCalculation(state.dataHeatBal->TotConstructs) != 0.0) {
            state.dataHeatBal->NominalU(state.dataHeatBal->TotConstructs) =
                1.0 / state.dataHeatBal->NominalRforNominalUCalculation(state.dataHeatBal->TotConstructs);
        }

        CheckAndSetConstructionProperties(state, state.dataHeatBal->TotConstructs, ErrorsFound);
    }

    return NewConstrNum;
}

void AddVariableSlatBlind(EnergyPlusData &state,
                          int const inBlindNumber, // current Blind Number/pointer to name
                          int &outBlindNumber,     // resultant Blind Number to pass back
                          bool &errFlag            // error flag should one be needed
)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Linda Lawrie
    //       DATE WRITTEN   September 2009
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // Window Blinds are presented as "fixed" slat blinds.  However for certain Window Shading Controls,
    // the program needs to set the property to "variable"/movable slats.  Since a blind could be in use
    // elsewhere with "fixed", a material needs to be added with variable properties -- having most of the
    // "fixed" properties in tact.

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int Found;
    Real64 MinSlatAngGeom;
    Real64 MaxSlatAngGeom;

    // Object Data

    // maybe it's already there
    errFlag = false;
    Found = UtilityRoutines::FindItemInList("~" + state.dataHeatBal->Blind(inBlindNumber).Name, state.dataHeatBal->Blind);
    if (Found == 0) {
        // Add a new blind
        state.dataHeatBal->Blind.redimension(++state.dataHeatBal->TotBlinds);
        state.dataHeatBal->Blind(state.dataHeatBal->TotBlinds) = state.dataHeatBal->Blind(inBlindNumber);
        state.dataHeatBal->Blind(state.dataHeatBal->TotBlinds).Name = "~" + state.dataHeatBal->Blind(inBlindNumber).Name;
        outBlindNumber = state.dataHeatBal->TotBlinds;
        state.dataHeatBal->Blind(state.dataHeatBal->TotBlinds).SlatAngleType = VariableSlats;

        // Minimum and maximum slat angles allowed by slat geometry
        if (state.dataHeatBal->Blind(state.dataHeatBal->TotBlinds).SlatWidth >
            state.dataHeatBal->Blind(state.dataHeatBal->TotBlinds).SlatSeparation) {
            MinSlatAngGeom = std::asin(state.dataHeatBal->Blind(state.dataHeatBal->TotBlinds).SlatThickness /
                                       (state.dataHeatBal->Blind(state.dataHeatBal->TotBlinds).SlatThickness +
                                        state.dataHeatBal->Blind(state.dataHeatBal->TotBlinds).SlatSeparation)) /
                             DataGlobalConstants::DegToRadians;
        } else {
            MinSlatAngGeom = 0.0;
        }
        MaxSlatAngGeom = 180.0 - MinSlatAngGeom;

        // Error if maximum slat angle less than minimum

        if (state.dataHeatBal->Blind(state.dataHeatBal->TotBlinds).MaxSlatAngle <
            state.dataHeatBal->Blind(state.dataHeatBal->TotBlinds).MinSlatAngle) {
            errFlag = true;
            ShowSevereError(state, "WindowMaterial:Blind=\"" + state.dataHeatBal->Blind(inBlindNumber).Name + "\", Illegal value combination.");
            ShowContinueError(state,
                              format("Minimum Slat Angle=[{:.1R}], is greater than Maximum Slat Angle=[{:.1R}] deg.",
                                     state.dataHeatBal->Blind(state.dataHeatBal->TotBlinds).MinSlatAngle,
                                     state.dataHeatBal->Blind(state.dataHeatBal->TotBlinds).MaxSlatAngle));
        }

        // Error if input slat angle not in input min/max range

        if (state.dataHeatBal->Blind(state.dataHeatBal->TotBlinds).MaxSlatAngle >
                state.dataHeatBal->Blind(state.dataHeatBal->TotBlinds).MinSlatAngle &&
            (state.dataHeatBal->Blind(state.dataHeatBal->TotBlinds).SlatAngle < state.dataHeatBal->Blind(state.dataHeatBal->TotBlinds).MinSlatAngle ||
             state.dataHeatBal->Blind(state.dataHeatBal->TotBlinds).SlatAngle >
                 state.dataHeatBal->Blind(state.dataHeatBal->TotBlinds).MaxSlatAngle)) {
            errFlag = true;
            ShowSevereError(state, "WindowMaterial:Blind=\"" + state.dataHeatBal->Blind(inBlindNumber).Name + "\", Illegal value combination.");
            ShowContinueError(state,
                              format("Slat Angle=[{:.1R}] is outside of the input min/max range, min=[{:.1R}], max=[{:.1R}] deg.",
                                     state.dataHeatBal->Blind(state.dataHeatBal->TotBlinds).SlatAngle,
                                     state.dataHeatBal->Blind(state.dataHeatBal->TotBlinds).MinSlatAngle,
                                     state.dataHeatBal->Blind(state.dataHeatBal->TotBlinds).MaxSlatAngle));
        }

        // Warning if input minimum slat angle is less than that allowed by slat geometry

        if (state.dataHeatBal->Blind(state.dataHeatBal->TotBlinds).MinSlatAngle < MinSlatAngGeom) {
            ShowWarningError(state, "WindowMaterial:Blind=\"" + state.dataHeatBal->Blind(inBlindNumber).Name + "\", Illegal value combination.");
            ShowContinueError(
                state,
                format("Minimum Slat Angle=[{:.1R}] is less than the smallest allowed by slat dimensions and spacing, min=[{:.1R}] deg.",
                       state.dataHeatBal->Blind(state.dataHeatBal->TotBlinds).MinSlatAngle,
                       MinSlatAngGeom));
            ShowContinueError(state, format("Minimum Slat Angle will be set to {:.1R} deg.", MinSlatAngGeom));
            state.dataHeatBal->Blind(state.dataHeatBal->TotBlinds).MinSlatAngle = MinSlatAngGeom;
        }

        // Warning if input maximum slat angle is greater than that allowed by slat geometry

        if (state.dataHeatBal->Blind(state.dataHeatBal->TotBlinds).MaxSlatAngle > MaxSlatAngGeom) {
            ShowWarningError(state, "WindowMaterial:Blind=\"" + state.dataHeatBal->Blind(inBlindNumber).Name + "\", Illegal value combination.");
            ShowContinueError(state,
                              format("Maximum Slat Angle=[{:.1R}] is greater than the largest allowed by slat dimensions and spacing, [{:.1R}] deg.",
                                     state.dataHeatBal->Blind(state.dataHeatBal->TotBlinds).MaxSlatAngle,
                                     MaxSlatAngGeom));
            ShowContinueError(state, format("Maximum Slat Angle will be set to {:.1R} deg.", MaxSlatAngGeom));
            state.dataHeatBal->Blind(state.dataHeatBal->TotBlinds).MaxSlatAngle = MaxSlatAngGeom;
        }
    } else {
        outBlindNumber = Found;
    }
}

void CalcScreenTransmittance(EnergyPlusData &state,
                             int const SurfaceNum,
                             Optional<Real64 const> Phi,     // Optional sun altitude relative to surface outward normal (radians)
                             Optional<Real64 const> Theta,   // Optional sun azimuth relative to surface outward normal (radians)
                             Optional_int_const ScreenNumber // Optional screen number
)
{

    // FUNCTION INFORMATION:
    //       AUTHOR         Richard Raustad
    //       DATE WRITTEN   May 2006
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS FUNCTION:
    //  Calculate transmittance of window screen given azimuth and altitude angle
    //  of sun and surface orientation.

    // METHODOLOGY EMPLOYED:
    //  Window screen solar beam transmittance varies as the sun moves across the sky
    //  due to the geometry of the screen material and the angle of incidence
    //  of the solar beam. Azimuth and altitude angle are calculated with respect
    //  to the surface outward normal. Solar beam reflectance and absorptance are also
    //  accounted for.

    //  CALLs to CalcScreenTransmittance are primarily based on surface index. A typical call is:
    //  CALL CalcScreenTransmittance(SurfaceNum)
    //  Since a single Material:WindowScreen object may be used for multiple windows, the
    //  screen's direct beam properties are calculated for the screen material attached to this surface.
    //  If a single Material:WindowScreen object is used for 3 windows then SurfaceScreens(3) is allocated.

    //  CALLs to CalcScreenTransmittance may be done by using the optional arguments as follows:
    //  CALLs to CalcScreenTransmittance at normal incidence are:
    //  CALL with a screen number and relative azimuth and altitude angles
    //  CALL CalcScreenTransmittance(0, Phi=0.0, Theta=0.0, ScreenNumber=ScNum)
    //   -OR-
    //  CALL same as above using the material structure
    //  CALL CalcScreenTransmittance(0, Phi=0.0, Theta=0.0, ScreenNumber=dataMaterial.Material(MatShade)%ScreenDataPtr)
    //   -OR-
    //  CALL with the surface number and relative azimuth and altitude angles
    //  CALL CalcScreenTransmittance(SurfaceNum, Phi=0.0, Theta=0.0)

    //  CALL's passing the screen number without the relative azimuth and altitude angles is not allowed
    //  CALL CalcScreenTransmittance(0, ScreenNumber=ScNum) ! DO NOT use this syntax

    // Using/Aliasing
    using DataSurfaces::DoNotModel;
    using DataSurfaces::ModelAsDiffuse;
    using DataSurfaces::ModelAsDirectBeam;

    // Locals
    // FUNCTION ARGUMENT DEFINITIONS:
    // The optional arguments Phi and Theta are used to integrate over a hemisphere and are passed as pairs
    // The optional argument ScreenNumber is used during CalcWindowScreenProperties to integrate over a quarter hemispere
    // "before" the surface # is known. Theta and Phi can be passed without ScreenNumber, but DO NOT pass ScreenNumber
    // without Theta and Phi.

    // FUNCTION PARAMETER DEFINITIONS:
    Real64 const Small(1.E-9); // Small Number used to approximate zero

    // FUNCTION PARAMETER DEFINITIONS:
    int ScNum;                        // Index to screen data
    Real64 Tdirect;                   // Beam solar transmitted through screen (dependent on sun angle)
    Real64 Tscattered;                // Beam solar reflected through screen (dependent on sun angle)
    Real64 TscatteredVis;             // Visible beam solar reflected through screen (dependent on sun angle)
    Real64 SunAzimuth;                // Solar azimuth angle from north (rad)
    Real64 SunAltitude;               // Solar altitude angle from horizon (rad)
    Real64 SurfaceAzimuth;            // Surface azimuth angle from north (rad)
    Real64 SurfaceTilt;               // Surface tilt angle from vertical (rad)
    Real64 SunAzimuthToScreenNormal;  // Relative solar azimuth (sun angle from screen normal, 0 to PiOvr2, rad)
    Real64 SunAltitudeToScreenNormal; // Relative solar altitude (sun angle from screen normal, -PiOvr2 to PiOvr2, rad)
    Real64 Beta;                      // Compliment of relative solar azimuth (rad)
    Real64 TransXDir;                 // Horizontal component of direct beam transmittance
    Real64 TransYDir;                 // Vertical component of direct beam transmittance
    Real64 Delta;                     // Intermediate variable used for Tscatter calculation (deg)
    Real64 DeltaMax;                  // Intermediate variable used for Tscatter calculation (deg)
    Real64 Tscattermax;               // Maximum solar beam  scattered transmittance
    Real64 TscattermaxVis;            // Maximum visible beam scattered transmittance
    Real64 ExponentInterior;          // Exponent used in scattered transmittance calculation
    // when Delta < DeltaMax (0,0 to peak)
    Real64 ExponentExterior; // Exponent used in scattered transmittance calculation
    // when Delta > DeltaMax (peak to max)
    Real64 AlphaDblPrime; // Intermediate variables (used in Eng. Doc.)
    Real64 COSMu;
    Real64 Epsilon;
    Real64 Eta;
    Real64 MuPrime;
    Real64 Gamma;
    Real64 NormalAltitude; // Actual altitude angle of sun wrt surface outward normal (rad)
    Real64 NormalAzimuth;  // Actual azimuth angle of sun wrt surface outward normal (rad)
    Real64 IncidentAngle;  // Solar angle wrt surface outward normal to determine
    // if sun is in front of screen (rad)
    Real64 PeakToPlateauRatio;    // Ratio of peak scattering to plateau at 0,0 incident angle
    Real64 PeakToPlateauRatioVis; // Ratio of peak visible scattering to plateau at 0,0 incident angle
    Real64 ReflectCyl;            // Screen material reflectance
    Real64 ReflectCylVis;         // Screen material visible reflectance

    // SurfaceScreens structure may be accessed using either the surface or screen index
    // The screen index is based on the number of Surface:HeatTransfer:Sub objects using any Material:WindowScreen object
    if (present(ScreenNumber)) {
        ScNum = ScreenNumber;
        if (!present(Theta) || !present(Phi)) {
            ShowFatalError(state, "Syntax error, optional arguments Theta and Phi must be present when optional ScreenNumber is used.");
        }
    } else {
        ScNum = state.dataSurface->SurfWinScreenNumber(SurfaceNum);
    }

    if (present(Theta)) {
        SunAzimuthToScreenNormal = std::abs(Theta);
        if (SunAzimuthToScreenNormal > DataGlobalConstants::Pi) {
            SunAzimuthToScreenNormal = 0.0;
        } else {
            if (SunAzimuthToScreenNormal > DataGlobalConstants::PiOvr2) {
                SunAzimuthToScreenNormal = DataGlobalConstants::Pi - SunAzimuthToScreenNormal;
            }
        }
        NormalAzimuth = SunAzimuthToScreenNormal;
    } else {
        SunAzimuth = std::atan2(state.dataEnvrn->SOLCOS(1), state.dataEnvrn->SOLCOS(2));
        if (SunAzimuth < 0.0) SunAzimuth += 2.0 * DataGlobalConstants::Pi;
        SurfaceAzimuth = state.dataSurface->Surface(SurfaceNum).Azimuth * DataGlobalConstants::DegToRadians;
        NormalAzimuth = SunAzimuth - SurfaceAzimuth;
        //   Calculate the transmittance whether sun is in front of or behind screen, place result in BmBmTrans or BmBmTransBack
        if (std::abs(SunAzimuth - SurfaceAzimuth) > DataGlobalConstants::PiOvr2) {
            SunAzimuthToScreenNormal = std::abs(SunAzimuth - SurfaceAzimuth) - DataGlobalConstants::PiOvr2;
        } else {
            SunAzimuthToScreenNormal = std::abs(SunAzimuth - SurfaceAzimuth);
        }
    }

    if (present(Phi)) {
        SunAltitudeToScreenNormal = std::abs(Phi);
        if (SunAltitudeToScreenNormal > DataGlobalConstants::PiOvr2) {
            SunAltitudeToScreenNormal = DataGlobalConstants::Pi - SunAltitudeToScreenNormal;
        }
        SunAltitude = SunAltitudeToScreenNormal;
    } else {
        SunAltitude = (DataGlobalConstants::PiOvr2 - std::acos(state.dataEnvrn->SOLCOS(3)));
        SurfaceTilt = state.dataSurface->Surface(SurfaceNum).Tilt * DataGlobalConstants::DegToRadians;
        SunAltitudeToScreenNormal = std::abs(SunAltitude + (SurfaceTilt - DataGlobalConstants::PiOvr2));
        if (SunAltitudeToScreenNormal > DataGlobalConstants::PiOvr2) {
            SunAltitudeToScreenNormal -= DataGlobalConstants::PiOvr2;
        }
    }

    if (SurfaceNum == 0 || !present(ScreenNumber)) {
        NormalAltitude = SunAltitude;
    } else {
        NormalAltitude = SunAltitude + (SurfaceTilt - DataGlobalConstants::PiOvr2);
    }

    if (NormalAltitude != 0.0 && NormalAzimuth != 0.0) {
        IncidentAngle = std::acos(std::sin(NormalAltitude) / (std::tan(NormalAzimuth) * std::tan(NormalAltitude) / std::sin(NormalAzimuth)));
    } else if (NormalAltitude != 0.0 && NormalAzimuth == 0.0) {
        IncidentAngle = NormalAltitude;
    } else if (NormalAltitude == 0.0 && NormalAzimuth != 0.0) {
        IncidentAngle = NormalAzimuth;
    } else {
        IncidentAngle = 0.0;
    }

    // ratio of screen material diameter to screen material spacing
    Gamma = state.dataHeatBal->SurfaceScreens(ScNum).ScreenDiameterToSpacingRatio;

    // ************************************************************************************************
    // * calculate transmittance of totally absorbing screen material (beam passing through open area)*
    // ************************************************************************************************

    // calculate compliment of relative solar azimuth
    Beta = DataGlobalConstants::PiOvr2 - SunAzimuthToScreenNormal;

    // Catch all divide by zero instances
    if (Beta > Small) {
        if (std::abs(SunAltitudeToScreenNormal - DataGlobalConstants::PiOvr2) > Small) {
            AlphaDblPrime = std::atan(std::tan(SunAltitudeToScreenNormal) / std::cos(SunAzimuthToScreenNormal));
            TransYDir = 1.0 - Gamma * (std::cos(AlphaDblPrime) +
                                       std::sin(AlphaDblPrime) * std::tan(SunAltitudeToScreenNormal) * std::sqrt(1.0 + pow_2(1.0 / std::tan(Beta))));
            TransYDir = max(0.0, TransYDir);
        } else {
            TransYDir = 0.0;
        }
    } else {
        TransYDir = 0.0;
    }

    COSMu = std::sqrt(pow_2(std::cos(SunAltitudeToScreenNormal)) * pow_2(std::cos(SunAzimuthToScreenNormal)) +
                      pow_2(std::sin(SunAltitudeToScreenNormal)));
    if (COSMu > Small) {
        Epsilon = std::acos(std::cos(SunAltitudeToScreenNormal) * std::cos(SunAzimuthToScreenNormal) / COSMu);
        Eta = DataGlobalConstants::PiOvr2 - Epsilon;
        if (std::cos(Epsilon) != 0.0) {
            MuPrime = std::atan(std::tan(std::acos(COSMu)) / std::cos(Epsilon));
            if (Eta != 0.0) {
                TransXDir =
                    1.0 - Gamma * (std::cos(MuPrime) + std::sin(MuPrime) * std::tan(std::acos(COSMu)) * std::sqrt(1.0 + pow_2(1.0 / std::tan(Eta))));
                TransXDir = max(0.0, TransXDir);
            } else {
                TransXDir = 0.0;
            }
        } else {
            TransXDir = 0.0;
        }
    } else {
        TransXDir = 1.0 - Gamma;
    }
    Tdirect = max(0.0, TransXDir * TransYDir);

    // *******************************************************************************
    // * calculate transmittance of scattered beam due to reflecting screen material *
    // *******************************************************************************

    ReflectCyl = state.dataHeatBal->SurfaceScreens(ScNum).ReflectCylinder;
    ReflectCylVis = state.dataHeatBal->SurfaceScreens(ScNum).ReflectCylinderVis;

    if (std::abs(SunAzimuthToScreenNormal - DataGlobalConstants::PiOvr2) < Small ||
        std::abs(SunAltitudeToScreenNormal - DataGlobalConstants::PiOvr2) < Small) {
        Tscattered = 0.0;
        TscatteredVis = 0.0;
    } else {
        //   DeltaMax and Delta are in degrees
        DeltaMax = 89.7 - (10.0 * Gamma / 0.16);
        Delta = std::sqrt(pow_2(SunAzimuthToScreenNormal / DataGlobalConstants::DegToRadians) +
                          pow_2(SunAltitudeToScreenNormal / DataGlobalConstants::DegToRadians));

        //   Use empirical model to determine maximum (peak) scattering
        Tscattermax = 0.0229 * Gamma + 0.2971 * ReflectCyl - 0.03624 * pow_2(Gamma) + 0.04763 * pow_2(ReflectCyl) - 0.44416 * Gamma * ReflectCyl;
        TscattermaxVis =
            0.0229 * Gamma + 0.2971 * ReflectCylVis - 0.03624 * pow_2(Gamma) + 0.04763 * pow_2(ReflectCylVis) - 0.44416 * Gamma * ReflectCylVis;

        //   Vary slope of interior and exterior surface of scattering model
        ExponentInterior = -pow_2(Delta - DeltaMax) / 600.0;
        ExponentExterior = -std::pow(std::abs(Delta - DeltaMax), 2.5) / 600.0;

        //   Determine ratio of scattering at 0,0 incident angle to maximum (peak) scattering
        PeakToPlateauRatio = 1.0 / (0.2 * (1 - Gamma) * ReflectCyl);
        PeakToPlateauRatioVis = 1.0 / (0.2 * (1 - Gamma) * ReflectCylVis);

        if (Delta > DeltaMax) {
            //     Apply offset for plateau and use exterior exponential function to simulate actual scattering as a function of solar angles
            Tscattered = 0.2 * (1.0 - Gamma) * ReflectCyl * Tscattermax * (1.0 + (PeakToPlateauRatio - 1.0) * std::exp(ExponentExterior));
            TscatteredVis = 0.2 * (1.0 - Gamma) * ReflectCylVis * TscattermaxVis * (1.0 + (PeakToPlateauRatioVis - 1.0) * std::exp(ExponentExterior));
            //     Trim off offset if solar angle (delta) is greater than maximum (peak) scattering angle
            Tscattered -= (0.2 * (1.0 - Gamma) * ReflectCyl * Tscattermax) * max(0.0, (Delta - DeltaMax) / (90.0 - DeltaMax));
            TscatteredVis -= (0.2 * (1.0 - Gamma) * ReflectCylVis * TscattermaxVis) * max(0.0, (Delta - DeltaMax) / (90.0 - DeltaMax));
        } else {
            //     Apply offset for plateau and use interior exponential function to simulate actual scattering as a function of solar angles
            Tscattered = 0.2 * (1.0 - Gamma) * ReflectCyl * Tscattermax * (1.0 + (PeakToPlateauRatio - 1.0) * std::exp(ExponentInterior));
            TscatteredVis = 0.2 * (1.0 - Gamma) * ReflectCylVis * TscattermaxVis * (1.0 + (PeakToPlateauRatioVis - 1.0) * std::exp(ExponentInterior));
        }
    }
    Tscattered = max(0.0, Tscattered);
    TscatteredVis = max(0.0, TscatteredVis);

    if (state.dataHeatBal->SurfaceScreens(ScNum).ScreenBeamReflectanceAccounting == DoNotModel) {
        if (std::abs(IncidentAngle) <= DataGlobalConstants::PiOvr2) {
            state.dataHeatBal->SurfaceScreens(ScNum).BmBmTrans = Tdirect;
            state.dataHeatBal->SurfaceScreens(ScNum).BmBmTransVis = Tdirect;
            state.dataHeatBal->SurfaceScreens(ScNum).BmBmTransBack = 0.0;
        } else {
            state.dataHeatBal->SurfaceScreens(ScNum).BmBmTrans = 0.0;
            state.dataHeatBal->SurfaceScreens(ScNum).BmBmTransVis = 0.0;
            state.dataHeatBal->SurfaceScreens(ScNum).BmBmTransBack = Tdirect;
        }
        Tscattered = 0.0;
        TscatteredVis = 0.0;
    } else if (state.dataHeatBal->SurfaceScreens(ScNum).ScreenBeamReflectanceAccounting == ModelAsDirectBeam) {
        if (std::abs(IncidentAngle) <= DataGlobalConstants::PiOvr2) {
            state.dataHeatBal->SurfaceScreens(ScNum).BmBmTrans = Tdirect + Tscattered;
            state.dataHeatBal->SurfaceScreens(ScNum).BmBmTransVis = Tdirect + TscatteredVis;
            state.dataHeatBal->SurfaceScreens(ScNum).BmBmTransBack = 0.0;
        } else {
            state.dataHeatBal->SurfaceScreens(ScNum).BmBmTrans = 0.0;
            state.dataHeatBal->SurfaceScreens(ScNum).BmBmTransVis = 0.0;
            state.dataHeatBal->SurfaceScreens(ScNum).BmBmTransBack = Tdirect + Tscattered;
        }
        Tscattered = 0.0;
        TscatteredVis = 0.0;
    } else if (state.dataHeatBal->SurfaceScreens(ScNum).ScreenBeamReflectanceAccounting == ModelAsDiffuse) {
        if (std::abs(IncidentAngle) <= DataGlobalConstants::PiOvr2) {
            state.dataHeatBal->SurfaceScreens(ScNum).BmBmTrans = Tdirect;
            state.dataHeatBal->SurfaceScreens(ScNum).BmBmTransVis = Tdirect;
            state.dataHeatBal->SurfaceScreens(ScNum).BmBmTransBack = 0.0;
        } else {
            state.dataHeatBal->SurfaceScreens(ScNum).BmBmTrans = 0.0;
            state.dataHeatBal->SurfaceScreens(ScNum).BmBmTransVis = 0.0;
            state.dataHeatBal->SurfaceScreens(ScNum).BmBmTransBack = Tdirect;
        }
    }

    if (std::abs(IncidentAngle) <= DataGlobalConstants::PiOvr2) {
        state.dataHeatBal->SurfaceScreens(ScNum).BmDifTrans = Tscattered;
        state.dataHeatBal->SurfaceScreens(ScNum).BmDifTransVis = TscatteredVis;
        state.dataHeatBal->SurfaceScreens(ScNum).BmDifTransBack = 0.0;
        state.dataHeatBal->SurfaceScreens(ScNum).ReflectSolBeamFront = max(0.0, ReflectCyl * (1.0 - Tdirect) - Tscattered);
        state.dataHeatBal->SurfaceScreens(ScNum).ReflectVisBeamFront = max(0.0, ReflectCylVis * (1.0 - Tdirect) - TscatteredVis);
        state.dataHeatBal->SurfaceScreens(ScNum).AbsorpSolarBeamFront = max(0.0, (1.0 - Tdirect) * (1.0 - ReflectCyl));
        state.dataHeatBal->SurfaceScreens(ScNum).ReflectSolBeamBack = 0.0;
        state.dataHeatBal->SurfaceScreens(ScNum).ReflectVisBeamBack = 0.0;
        state.dataHeatBal->SurfaceScreens(ScNum).AbsorpSolarBeamBack = 0.0;
    } else {
        state.dataHeatBal->SurfaceScreens(ScNum).BmDifTrans = 0.0;
        state.dataHeatBal->SurfaceScreens(ScNum).BmDifTransVis = 0.0;
        state.dataHeatBal->SurfaceScreens(ScNum).BmDifTransBack = Tscattered;
        state.dataHeatBal->SurfaceScreens(ScNum).ReflectSolBeamBack = max(0.0, ReflectCyl * (1.0 - Tdirect) - Tscattered);
        state.dataHeatBal->SurfaceScreens(ScNum).ReflectVisBeamBack = max(0.0, ReflectCylVis * (1.0 - Tdirect) - TscatteredVis);
        state.dataHeatBal->SurfaceScreens(ScNum).AbsorpSolarBeamBack = max(0.0, (1.0 - Tdirect) * (1.0 - ReflectCyl));
        state.dataHeatBal->SurfaceScreens(ScNum).ReflectSolBeamFront = 0.0;
        state.dataHeatBal->SurfaceScreens(ScNum).ReflectVisBeamFront = 0.0;
        state.dataHeatBal->SurfaceScreens(ScNum).AbsorpSolarBeamFront = 0.0;
    }
}

std::string DisplayMaterialRoughness(int const Roughness) // Roughness String
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Linda K. Lawrie
    //       DATE WRITTEN   October 2005
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine is given a roughness value and returns the character representation.

    // Return value
    std::string cRoughness; // Character representation of Roughness

    // Select the correct Number for the associated ascii name for the roughness type
    {
        auto const SELECT_CASE_var(Roughness);
        if (SELECT_CASE_var == VeryRough) {
            cRoughness = "VeryRough";
        } else if (SELECT_CASE_var == Rough) {
            cRoughness = "Rough";
        } else if (SELECT_CASE_var == MediumRough) {
            cRoughness = "MediumRough";
        } else if (SELECT_CASE_var == MediumSmooth) {
            cRoughness = "MediumSmooth";
        } else if (SELECT_CASE_var == Smooth) {
            cRoughness = "Smooth";
        } else if (SELECT_CASE_var == VerySmooth) {
            cRoughness = "VerySmooth";
        } else {
            cRoughness = "";
        }
    }

    return cRoughness;
}

Real64 ComputeNominalUwithConvCoeffs(EnergyPlusData &state,
                                     int const numSurf, // index for Surface array.
                                     bool &isValid      // returns true if result is valid
)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Jason Glazer
    //       DATE WRITTEN   September 2013
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // Calculate Nominal U-value with convection/film coefficients for reporting by
    // adding on prescribed R-values for interior and exterior convection coefficients
    // as found in ASHRAE 90.1-2004, Appendix A. Used in EIO and tabular reports.
    // ASHRAE 90.1-2004 Section A9.4.1 shows the following:
    //      R-value Condition
    //      All exterior conditions                        IP: 0.17  SI: 0.0299
    //      All semi-exterior surfaces                     IP: 0.46  SI: 0.0810
    //      Interior horizontal surfaces, heat flow up     IP: 0.61  SI: 0.1074
    //      Interior horizontal surfaces, heat flow down   IP: 0.92  SI: 0.1620
    //      Interior vertical surfaces                     IP: 0.68  SI: 0.1198
    // This section shows the same value in 90.1-2007 and 90.2-2010

    // Using/Aliasing
    using DataSurfaces::ExternalEnvironment;
    using DataSurfaces::Ground;
    using DataSurfaces::GroundFCfactorMethod;
    using DataSurfaces::SurfaceClass;

    // Return value
    Real64 NominalUwithConvCoeffs; // return value

    Real64 insideFilm;
    Real64 outsideFilm;

    isValid = true;
    // exterior conditions
    {
        auto const SELECT_CASE_var(state.dataSurface->Surface(numSurf).ExtBoundCond);
        if (SELECT_CASE_var == ExternalEnvironment) {
            outsideFilm = 0.0299387; // All exterior conditions
        } else if ((SELECT_CASE_var == Ground) || (SELECT_CASE_var == GroundFCfactorMethod)) {
            outsideFilm = 0.0; // No outside film when underground
        } else {
            if (state.dataSurface->Surface(numSurf).ExtBoundCond > 0) { // interzone partition
                // use companion surface in adjacent zone
                {
                    auto const SELECT_CASE_var1(state.dataSurface->Surface(state.dataSurface->Surface(numSurf).ExtBoundCond).Class);
                    if ((SELECT_CASE_var1 == SurfaceClass::Wall) ||
                        (SELECT_CASE_var1 == SurfaceClass::Door)) { // Interior:  vertical, still air, Rcin = 0.68 ft2-F-hr/BTU
                        outsideFilm = 0.1197548;
                    } else if (SELECT_CASE_var1 ==
                               SurfaceClass::Floor) { // Interior:  horizontal, still air, heat flow downward, Rcin = 0.92 ft2-F-hr/BTU
                        outsideFilm = 0.1620212;
                    } else if (SELECT_CASE_var1 ==
                               SurfaceClass::Roof) { // Interior:  horizontal, still air, heat flow upward, Rcin = 0.61 ft2-F-hr/BTU
                        outsideFilm = 0.1074271;
                    } else {
                        outsideFilm = 0.0810106; // All semi-exterior surfaces
                    }
                }
            } else {
                outsideFilm = 0.0810106; // All semi-exterior surfaces
            }
        }
    }
    // interior conditions
    if (state.dataHeatBal->NominalU(state.dataSurface->Surface(numSurf).Construction) > 0.0) {
        {
            auto const SELECT_CASE_var(state.dataSurface->Surface(numSurf).Class);
            if ((SELECT_CASE_var == SurfaceClass::Wall) ||
                (SELECT_CASE_var == SurfaceClass::Door)) { // Interior:  vertical, still air, Rcin = 0.68 ft2-F-hr/BTU
                insideFilm = 0.1197548;
            } else if (SELECT_CASE_var == SurfaceClass::Floor) { // Interior:  horizontal, still air, heat flow downward, Rcin = 0.92 ft2-F-hr/BTU
                insideFilm = 0.1620212;
            } else if (SELECT_CASE_var == SurfaceClass::Roof) { // Interior:  horizontal, still air, heat flow upward, Rcin = 0.61 ft2-F-hr/BTU
                insideFilm = 0.1074271;
            } else {
                insideFilm = 0.0;
                outsideFilm = 0.0;
            }
        }
        NominalUwithConvCoeffs =
            1.0 / (insideFilm + (1.0 / state.dataHeatBal->NominalU(state.dataSurface->Surface(numSurf).Construction)) + outsideFilm);
    } else {
        isValid = false;
        NominalUwithConvCoeffs = state.dataHeatBal->NominalU(state.dataSurface->Surface(numSurf).Construction);
    }

    return NominalUwithConvCoeffs;
}

void SetFlagForWindowConstructionWithShadeOrBlindLayer(EnergyPlusData &state)
{

    // PURPOSE OF THIS SUBROUTINE:
    // check fenestrations with shading control and set a flag to true if its construction has
    // either shade or blind material layer

    // METHODOLOGY EMPLOYED:
    // Loop through Surface and register any shading controls, and loop through the construction
    // material layer

    // Using/Aliasing
    using DataSurfaces::ExternalEnvironment;

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int loopSurfNum(0); // surface index
    int ConstrNum(0);   // construction index
    int NumLayers(0);   // number of material layers in a construction
    int Layer(0);       // construction material layer index
    int MaterNum(0);    // construction material index

    for (loopSurfNum = 1; loopSurfNum <= state.dataSurface->TotSurfaces; ++loopSurfNum) {

        if (state.dataSurface->Surface(loopSurfNum).Class != DataSurfaces::SurfaceClass::Window) continue;
        if (state.dataSurface->Surface(loopSurfNum).ExtBoundCond != ExternalEnvironment) continue;
        if (!state.dataSurface->Surface(loopSurfNum).HasShadeControl) continue;
        if (state.dataSurface->Surface(loopSurfNum).activeShadedConstruction == 0) continue;

        ConstrNum = state.dataSurface->Surface(loopSurfNum).activeShadedConstruction;
        if (state.dataConstruction->Construct(ConstrNum).TypeIsWindow) {
            NumLayers = state.dataConstruction->Construct(ConstrNum).TotLayers;
            for (Layer = 1; Layer <= NumLayers; ++Layer) {
                MaterNum = state.dataConstruction->Construct(ConstrNum).LayerPoint(Layer);
                if (MaterNum == 0) continue;
                if (state.dataMaterial->Material(MaterNum).Group == Shade || state.dataMaterial->Material(MaterNum).Group == WindowBlind)
                    state.dataSurface->SurfWinHasShadeOrBlindLayer(loopSurfNum) = true;
            }
        }
    }
}

} // namespace EnergyPlus::DataHeatBalance
