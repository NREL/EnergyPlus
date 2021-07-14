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

#ifndef DataRoomAirModel_hh_INCLUDED
#define DataRoomAirModel_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>
#include <ObjexxFCL/Array2D.hh>

// EnergyPlus Headers
#include <EnergyPlus/Data/BaseData.hh>
#include <EnergyPlus/DataGlobals.hh>
#include <EnergyPlus/EnergyPlus.hh>

namespace EnergyPlus {

namespace DataRoomAirModel {

    auto constexpr cUserDefinedControlObject("RoomAir:TemperaturePattern:UserDefined");
    auto constexpr cTempPatternConstGradientObject("RoomAir:TemperaturePattern:ConstantGradient");
    auto constexpr cTempPatternTwoGradientObject("RoomAir:TemperaturePattern:TwoGradient");
    auto constexpr cTempPatternNDHeightObject("RoomAir:TemperaturePattern:NondimensionalHeight");
    auto constexpr cTempPatternSurfMapObject("RoomAir:TemperaturePattern:SurfaceMapping");

    // Parameters to indicate room air model selected
    enum class RoomAirModel : int
    {
        Unassigned = -1,
        UserDefined,   // user defined patterns
        Mixing,        // mixing air model
        Mundt,         // Mundt nodal model
        UCSDDV,        // UCSD Displacement Ventilation model
        UCSDCV,        // UCSD-CV
        UCSDUFI,       // UCSD UFAD interior zone model
        UCSDUFE,       // UCSD UFAD exterior zone model
        AirflowNetwork // RoomAirModel_AirflowNetwork interior zone model
    };
    constexpr const char *ChAirModel[] = {
        "*Invalid*", "UserDefined", "Mixing", "Mundt", "UCSD_DV", "UCSD_CV", "UCSD_UFI", "UCSD_UFE", "AirflowNetwork"};

    // Parameters to indicate air temperature coupling scheme
    enum class CouplingScheme : int
    {
        Unassigned = -1,
        Direct,
        Indirect
    };

    // Parameters to indicate type of air node, which is dependent on air models
    enum class AirNodeType
    {
        Unassigned = -1,
        InletAirNode,              // air node at inlet (for Mundt and Rees&Haves Models)
        FloorAirNode,              // air node at floor (for Mundt and Rees&Haves Models)
        ControlAirNode,            // air node at control point (for Mundt Model)
        CeilingAirNode,            // air node at ceiling (for Mundt Model)
        MundtRoomAirNode,          // air node for vertical walls (for Mundt Model)
        ReturnAirNode,             // air node for return (for Mundt and Rees&Haves Models)
        AirflowNetworkRoomAirNode, // air node for airflow network based room air model
        PlumeAirNode,              // air node for plume load (for Rees&Haves Model)
        RoomAirNode                // air node for vertical walls (for Rees&Haves Model)
    };

    // user-defined pattern two gradient interpolation modes
    enum class UserDefinedPatternMode
    {
        Unassigned,
        OutdoorDryBulbMode,  // by outdoor air bulb.
        SensibleCoolingMode, // by sensible cooling load
        SensibleHeatingMode, // by sensible heating load
        ZoneAirTempMode,     // by zone air temperature
        DeltaOutdoorZone     // by difference between zone and outdoor
    };

    // user defined temperature pattern types
    enum class UserDefinedPatternType
    {
        Unassigned,
        ConstGradTempPattern,  // constant gradient in vertical direction
        TwoGradInterpPattern,  // two gradient interpolation
        NonDimenHeightPattern, // non-dimensionalized height
        SurfMapTempPattern     // arbitrary surface mappings
    };

    // parameters to indicate diffuser type
    enum class Diffuser
    {
        Unassigned,
        Swirl,
        VarArea,
        DisplVent,
        LinBarGrille,
        Custom
    };

    enum class Comfort
    {
        VComfort_Invalid,
        VComfort_Jet,
        VComfort_Recirculation
    };

    struct AirModelData
    {
        // Members
        std::string AirModelName;
        std::string ZoneName;
        int ZonePtr;               // Pointer to the zone number for this statement
        RoomAirModel AirModelType; // 1 = Mixing, 2 = Mundt, 3 = Rees and Haves,
        // 4 = UCSDDV, 5 = UCSDCV, -1 = user defined
        // 6 = UCSDUFI, 7 = UCSDUFE, 8 = AirflowNetwork
        CouplingScheme TempCoupleScheme; // 1 = absolute (direct),
        // 2 = relative air model temperature passing scheme (indirect)
        bool SimAirModel; // FALSE if Mixing air model is currently used and
        // TRUE if other air models are currently used

        // Default Constructor
        AirModelData() : ZonePtr(0), AirModelType(RoomAirModel::Mixing), TempCoupleScheme(CouplingScheme::Direct), SimAirModel(false)
        {
        }
    };

    struct AirNodeData
    {
        // Members
        std::string Name; // name
        std::string ZoneName;
        int ZonePtr;               // Pointer to the zone number for this statement
        AirNodeType ClassType;     // depending on type of model
        Real64 Height;             // height
        Real64 ZoneVolumeFraction; // portion of zone air volume associated with this node
        Array1D_bool SurfMask;     // limit of 60 surfaces at current sizing
        bool IsZone;               // TRUE if this node is zone node

        // Default Constructor
        AirNodeData() : ZonePtr(0), ClassType(AirNodeType::Unassigned), Height(0.0), ZoneVolumeFraction(0), IsZone(false)
        {
        }
    };

    struct DVData
    {
        // Members
        std::string ZoneName;       // Name of zone
        int ZonePtr;                // Pointer to the zone number for this statement
        int SchedGainsPtr;          // Schedule for internal gain fraction to occupied zone
        std::string SchedGainsName; // Gains Schedule name
        Real64 NumPlumesPerOcc;     // Effective number of plumes per occupant
        Real64 ThermostatHeight;    // Height of thermostat/ temperature control sensor
        Real64 ComfortHeight;       // Height at which air temperature is measured for comfort purposes
        Real64 TempTrigger;         // Minimum temperature difference between TOC TMX for stratification

        // Default Constructor
        DVData() : ZonePtr(0), SchedGainsPtr(-1), NumPlumesPerOcc(1.0), ThermostatHeight(0.0), ComfortHeight(0.0), TempTrigger(0.0)
        {
        }
    };

    struct CVData
    {
        // Members
        std::string ZoneName;       // Name of zone
        int ZonePtr;                // Pointer to the zone number for this statement
        int SchedGainsPtr;          // Schedule for internal gain fraction to occupied zone
        std::string SchedGainsName; // Gains Schedule name
        Comfort VforComfort;        // Use Recirculation or Jet velocity and temperatures
        // for comfort models

        // Default Constructor
        CVData() : ZonePtr(-1), SchedGainsPtr(-1), VforComfort(Comfort::VComfort_Invalid)
        {
        }
    };

    struct CVFlow
    {
        // Members
        int FlowFlag; // Equal to 1 if the opening has inflow, else equal to 0.
        Real64 Width; // Width of the opening [m]
        Real64 Area;  // Area of the opening [m2]
        Real64 Fin;   // Inflow volume flux through the opening [m3/s]
        Real64 Uin;   // Inflow air velocity through the opening [m/s]
        Real64 Vjet;  // Average maximum jet velocity for the opening [m/s]
        Real64 Yjet;  // Y in "Y = aX + b" formula
        Real64 Ujet;  // Volume average jet region velocity [m/s]
        Real64 Yrec;  // Y in "Y = aX + b" formula
        Real64 Urec;  // Area-averaged velocity in the y-z plane with maximum flow [m/s]
        Real64 YQrec; // Y in "Y = aX + b" formula
        Real64 Qrec;  // Total flow rate for the recirculation regions in the plane of maximum flow [m3/s]

        // Default Constructor
        CVFlow()
            : FlowFlag(0), Width(0.0), Area(0.0), Fin(0.0), Uin(0.0), Vjet(0.0), Yjet(0.0), Ujet(0.0), Yrec(0.0), Urec(0.0), YQrec(0.0), Qrec(0.0)
        {
        }
    };

    struct CVDVParameters
    {
        // Members
        Real64 Width;
        Real64 Height;
        int Shadow;
        Real64 Zmin;
        Real64 Zmax;

        // Default Constructor
        CVDVParameters() : Width(0.0), Height(0.0), Shadow(0), Zmin(0.0), Zmax(0.0)
        {
        }
    };

    struct UFIData
    {
        // Members
        std::string ZoneName;    // Name of zone
        int ZonePtr;             // Pointer to the zone number for this statement
        int ZoneEquipPtr;        // Pointer to zone equip for this UFAD zone
        Real64 DiffusersPerZone; // Number of diffusers in this zone
        Real64 PowerPerPlume;    // Power in each plume [W]
        Real64 DiffArea;         // Effective area of a diffuser [m2]
        Real64 DiffAngle;        // angle between diffuser slots and vertical (degrees)
        Real64 HeatSrcHeight;    // height of heat source above floor [m]
        Real64 ThermostatHeight; // Height of thermostat/ temperature control sensor [m]
        Real64 ComfortHeight;    // Height at which air temperature is measured for
        // comfort purposes [m]
        Real64 TempTrigger; // Minimum temperature difference between TOC TMX
        // for stratification [deltaC]
        Diffuser DiffuserType; // 1=Swirl, 2=variable area, 3=displacement, 4=linear bar grille, 5=custom
        Real64 TransHeight;    // user specified transition height [m]
        bool CalcTransHeight;  // flag to calc trans height or use user specified input
        Real64 A_Kc;           // Coefficient A in Formula Kc = A*Gamma**B + C + D*Gamma + E*Gamma**2
        Real64 B_Kc;           // Coefficient A in Formula Kc = A*Gamma**B + C + D*Gamma + E*Gamma**2
        Real64 C_Kc;           // Coefficient A in Formula Kc = A*Gamma**B + C + D*Gamma + E*Gamma**2
        Real64 D_Kc;           // Coefficient A in Formula Kc = A*Gamma**B + C + D*Gamma + E*Gamma**2
        Real64 E_Kc;           // Coefficient A in Formula Kc = A*Gamma**B + C + D*Gamma + E*Gamma**2

        // Default Constructor
        UFIData()
            : ZonePtr(0), ZoneEquipPtr(0), DiffusersPerZone(0.0), PowerPerPlume(0.0), DiffArea(0.0), DiffAngle(0.0), HeatSrcHeight(0.0),
              ThermostatHeight(0.0), ComfortHeight(0.0), TempTrigger(0.0), DiffuserType(Diffuser::Unassigned), TransHeight(0.0),
              CalcTransHeight(false), A_Kc(0.0), B_Kc(0.0), C_Kc(0.0), D_Kc(0.0), E_Kc(0.0)
        {
        }
    };

    struct UFEData
    {
        // Members
        std::string ZoneName;    // Name of zone
        int ZonePtr;             // Pointer to the zone number for this statement
        int ZoneEquipPtr;        // Pointer to zone equip for this UFAD zone
        Real64 DiffusersPerZone; // Number of diffusers in this zone
        Real64 PowerPerPlume;    // Power in each plume [W]
        Real64 DiffArea;         // Effective area of a diffuser [m2]
        Real64 DiffAngle;        // angle between diffuser slots and vertical (degrees)
        Real64 HeatSrcHeight;    // height of heat source above floor [m]
        Real64 ThermostatHeight; // Height of thermostat/ temperature control sensor [m]
        Real64 ComfortHeight;    // Height at which air temperature is measured for
        // comfort purposes [m]
        Real64 TempTrigger; // Minimum temperature difference between TOC TMX
        // for stratification [deltaC]
        Diffuser DiffuserType; // 1=Swirl, 2=variable area, 3=displacement, 4=linear bar grille, 5=custom
        Real64 TransHeight;    // user specified transition height [m]
        bool CalcTransHeight;  // flag to calc trans height or use user specified input
        Real64 A_Kc;           // Coefficient A in Formula Kc = A*Gamma**B + C + D*Gamma + E*Gamma**2
        Real64 B_Kc;           // Coefficient A in Formula Kc = A*Gamma**B + C + D*Gamma + E*Gamma**2
        Real64 C_Kc;           // Coefficient A in Formula Kc = A*Gamma**B + C + D*Gamma + E*Gamma**2
        Real64 D_Kc;           // Coefficient A in Formula Kc = A*Gamma**B + C + D*Gamma + E*Gamma**2
        Real64 E_Kc;           // Coefficient A in Formula Kc = A*Gamma**B + C + D*Gamma + E*Gamma**2
        Real64 WinWidth;       // sum of widths of exterior windows in zone
        Real64 NumExtWin;      // number of exterior windows in the zone
        bool ShadeDown;        // signals shade up or down

        // Default Constructor
        UFEData()
            : ZonePtr(0), ZoneEquipPtr(0), DiffusersPerZone(0.0), PowerPerPlume(0.0), DiffArea(0.0), DiffAngle(0.0), HeatSrcHeight(0.0),
              ThermostatHeight(0.0), ComfortHeight(0.0), TempTrigger(0.0), DiffuserType(Diffuser::Unassigned), TransHeight(0.0),
              CalcTransHeight(false), A_Kc(0.0), B_Kc(0.0), C_Kc(0.0), D_Kc(0.0), E_Kc(0.0), WinWidth(0.0), NumExtWin(0.0), ShadeDown(true)
        {
        }
    };

    struct SurfMapPattern // nested structure in RoomAirPattern
    {
        // Members
        // user variables
        Array1D_string SurfName;  // user defined name
        Array1D<Real64> DeltaTai; // (Tai - MAT ) offset from mean air temp
        int NumSurfs;             // number of surfaces in this pattern
        // calculated and from elsewhere
        Array1D_int SurfID; // index in HB surface structure array

        // Default Constructor
        SurfMapPattern() : NumSurfs(0)
        {
        }
    };

    struct ConstGradPattern // nested structure in RoomAirPattern
    {
        // Members
        // user variables
        std::string Name; // name
        Real64 Gradient;  // value of vertical gradient [C/m]

        // Default Constructor
        ConstGradPattern() : Gradient(0.0)
        {
        }
    };

    struct TwoVertGradInterpolPattern // nested structure in RoomAirPattern
    {
        // Members
        // user variables
        std::string Name;                                           // name
        Real64 TstatHeight;                                         // Height of thermostat/ temperature control sensor
        Real64 TleavingHeight;                                      // height of return air node where leaving zone
        Real64 TexhaustHeight;                                      // height of exhaust air node where leaving zone
        Real64 LowGradient;                                         // lower value of vertical gradient [C/m]
        Real64 HiGradient;                                          // upper value of vertical gradient [C/m]
        DataRoomAirModel::UserDefinedPatternMode InterpolationMode; // control for interpolation mode
        Real64 UpperBoundTempScale;                                 // temperature value for HiGradient
        Real64 LowerBoundTempScale;                                 // temperature value for LowGradient
        Real64 UpperBoundHeatRateScale;                             // load value for HiGradient
        Real64 LowerBoundHeatRateScale;                             // load value for lowGradient

        // Default Constructor
        TwoVertGradInterpolPattern()
            : TstatHeight(0.0), TleavingHeight(0.0), TexhaustHeight(0.0), LowGradient(0.0), HiGradient(0.0),
              InterpolationMode(DataRoomAirModel::UserDefinedPatternMode::Unassigned), UpperBoundTempScale(0.0), LowerBoundTempScale(0.0),
              UpperBoundHeatRateScale(0.0), LowerBoundHeatRateScale(0.0)
        {
        }
    };

    struct TempVsHeightPattern // to be used as nested structure in RoomAirPattern
    {
        // Members
        Array1D<Real64> ZetaPatrn;     // non dimensional height from floor,
        Array1D<Real64> DeltaTaiPatrn; // Tai- MAT (TODO, check sign)

        // Default Constructor
        TempVsHeightPattern()
        {
        }
    };

    struct TemperaturePatternStruct // RoomAirPattern
    {
        // Members
        std::string Name;                        // unique identifier
        int PatrnID;                             // control ID for referencing in Schedules
        UserDefinedPatternType PatternMode;      // Control for what type of calcs in this pattern
        ConstGradPattern GradPatrn;              // Constant gradient pattern
        TwoVertGradInterpolPattern TwoGradPatrn; // Two gradient interpolation pattern
        TempVsHeightPattern VertPatrn;           // Vertical gradient profile pattern
        SurfMapPattern MapPatrn;                 // Generic Surface map pattern
        Real64 DeltaTstat;                       // (Tstat - MAT) offset   deg C
        Real64 DeltaTleaving;                    // (Tleaving - MAT) deg C
        Real64 DeltaTexhaust;                    // (Texhaust - MAT) deg C

        // Default Constructor
        TemperaturePatternStruct()
            : PatrnID(0), PatternMode(UserDefinedPatternType::Unassigned), DeltaTstat(0.0), DeltaTleaving(0.0), DeltaTexhaust(0.0)
        {
        }
    };

    struct SurfaceAssocNestedStruct
    {
        // Members
        std::string Name;    // unique identifier
        int SurfID;          // id in HB surface structs
        Real64 TadjacentAir; // place to put resulting temperature value
        Real64 Zeta;         // non-dimensional height in zone ot

        // Default Constructor
        SurfaceAssocNestedStruct() : SurfID(0), TadjacentAir(23.0), Zeta(0.0)
        {
        }
    };

    struct AirPatternInfobyZoneStruct // becomes AirPatternZoneInfo
    {
        // Members
        // user variables
        bool IsUsed;                   // .TRUE. if user-defined patterns used in zone
        std::string Name;              // Name
        std::string ZoneName;          // Zone name in building
        int ZoneID;                    // Index of Zone in Heat Balance
        std::string AvailSched;        // Name of availability schedule
        int AvailSchedID;              // index of availability schedule
        std::string PatternCntrlSched; // name of schedule that selects pattern
        int PatternSchedID;            // index of pattern selecting schedule
        // calculated and from elsewhere
        Real64 ZoneHeight;                      // in meters, from Zone%CeilingHeight
        int ZoneNodeID;                         // index in Node array for this zone
        Array1D_int ExhaustAirNodeID;           // indexes in Node array
        Real64 TairMean;                        // comes from MAT
        Real64 Tstat;                           // temperature for thermostat
        Real64 Tleaving;                        // temperature for return air node
        Real64 Texhaust;                        // temperature for exhaust air node
        Array1D<SurfaceAssocNestedStruct> Surf; // nested struct w/ surface info
        int totNumSurfs;                        // total surfs for this zone
        int firstSurfID;                        // Index of first surface
        // report
        Real64 Gradient; // result for modeled gradient if using two-gradient interpolation

        // Default Constructor
        AirPatternInfobyZoneStruct()
            : IsUsed(false), ZoneID(0), AvailSchedID(0), PatternSchedID(0), ZoneHeight(0.0), ZoneNodeID(0), TairMean(23.0), Tstat(23.0),
              Tleaving(23.0), Texhaust(23.0), totNumSurfs(0), firstSurfID(0), Gradient(0.0)
        {
        }
    };

    struct AirflowLinkagesInfoNestedStruct // becomes link
    {
        // Members
        // user variables
        int AirflowNetworkLinkSimuID;    // point to this linkage in AirflowNetworkLinkSimu structure
        int AirflowNetworkLinkageDataID; // point to this linkage in AirflowNetworkLinkageData structure
        int AirflowNetworkLinkReportID;  // point to this linkage in AirflowNetworkLinkReport structure
        Real64 MdotIn;                   // mass flow rate of air into control volume(neg means leaving control volume) (kg / s)
        Real64 TempIn;                   // drybulb temperature of air into control volume
        Real64 HumRatIn;                 // humidity ratio of air into control volume

        // Default Constructor
        AirflowLinkagesInfoNestedStruct()
            : AirflowNetworkLinkSimuID(0), AirflowNetworkLinkageDataID(0), AirflowNetworkLinkReportID(0), MdotIn(0.0), TempIn(0.0), HumRatIn(0.0)
        {
        }
    };

    struct RoomAirflowNetworkNodeInternalGainsStruct // becomes IntGain
    {
        // Members
        // user variables
        int TypeOfNum;                    // Internal type
        std::string Name;                 // Intenral gain name
        bool UseRoomAirModelTempForGains; // TRUE if user inputs temp for gains
        bool FractionCheck;               // TRUE if a fraction of internal gain for each object is checked

        // Default Constructor
        RoomAirflowNetworkNodeInternalGainsStruct() : TypeOfNum(0), UseRoomAirModelTempForGains(false), FractionCheck(false)
        {
        }
    };

    struct RoomAirflowNetworkHVACStruct // becomes HVAC
    {
        // Members
        // user variables
        std::string Name;           // HVAC system name
        std::string ObjectTypeName; // HVAC object type name
        std::string SupplyNodeName; // HVAC system supply node name
        std::string ReturnNodeName; // HVAC system return node name
        int TypeOfNum;              // HVAC type num
        Real64 SupplyFraction;      // Supply flow fraction
        Real64 ReturnFraction;      // Return flow fraction
        int EquipConfigIndex;       // HVAC equipment configuration index
        int SupNodeNum;             // HVAC supply node number
        int RetNodeNum;             // HVAC return node number
        int CompIndex;              // Component index

        // Default Constructor
        RoomAirflowNetworkHVACStruct()
            : TypeOfNum(0),        // HVAC type num
              SupplyFraction(0),   // Supply flow fraction
              ReturnFraction(0),   // Return flow fraction
              EquipConfigIndex(0), // HVAC equipment configuration index
              SupNodeNum(0),       // HVAC supply node number
              RetNodeNum(0),       // HVAC return node number
              CompIndex(0)         // Component index
        {
        }
    };

    struct RoomAirflowNetworkAirNodeNestedStruct // becomes Node
    {
        // Members
        // user variables
        std::string Name;                                           // name of the node itself
        Real64 ZoneVolumeFraction;                                  // Zone volume fraction applied to this specific node
        std::string NodeSurfListName;                               // name of nodes' adjacent surface list
        bool HasSurfacesAssigned;                                   // True if this node has surfaces assigned
        Array1D<bool> SurfMask;                                     // Sized to num of surfs in Zone, true if surface is associated with this node
        std::string NodeIntGainsListName;                           // name of node's internal gains list
        bool HasIntGainsAssigned;                                   // True if this node has internal gain assigned
        int NumIntGains;                                            // Number of internal gain objects
        Array1D<int> IntGainsDeviceIndices;                         // sized to NumIntGains, index pointers to internal gains struct
        Array1D<Real64> IntGainsFractions;                          // sized to NumIntGains, gain fractions to this node
        Array1D<RoomAirflowNetworkNodeInternalGainsStruct> IntGain; // Internal gain struct
        std::string NodeHVACListName;                               // name of node's HVAC list
        bool HasHVACAssigned;                                       // True if HVAC systems are assigned to this node
        int NumHVACs;                                               // Number of HVAC systems
        Array1D<RoomAirflowNetworkHVACStruct> HVAC;                 // HVAC struct
        int AirflowNetworkNodeID;                                   // pointer to AirflowNetworkNodeData structure
        int NumOfAirflowLinks;                                      // Number of intra zone links
        Array1D<AirflowLinkagesInfoNestedStruct> Link;              // Linkage struct
        Real64 AirVolume;                                           // air volume in control volume associated with this node(m3 / s)
        Real64 RhoAir;                                              // current density of air for nodal control volume
        Real64 CpAir;                                               // current heat capacity of air for nodal control volume

        Real64 AirTemp;     // node air temperature
        Real64 AirTempX1;   // node air temperature at t minus 1 zone timestep
        Real64 AirTempX2;   // node air temperature at t minus 2 zone timestep
        Real64 AirTempX3;   // node air temperature at t minus 3 zone timestep
        Real64 AirTempX4;   // node air temperature at t minus 4 zone timestep
        Real64 AirTempDSX1; // node air temperature at t minus 1 system timestep
        Real64 AirTempDSX2; // node air temperature at t minus 2 system timestep
        Real64 AirTempDSX3; // node air temperature at t minus 3 system timestep
        Real64 AirTempDSX4; // node air temperature at t minus 4 system timestep
        Real64 AirTempT1;   // node air temperature at the previous time step used in Exact and Euler method
        Real64 AirTempTMX;  // temporary node air temperature to test convergence used in Exact and Euler method
        Real64 AirTempTM2;  // node air temperature at time step t-2 used in Exact and Euler method

        Real64 HumRat;     // node air humidity ratio
        Real64 HumRatX1;   // node air humidity ratio at t minus 1 zone timestep
        Real64 HumRatX2;   // node air humidity ratio at t minus 2 zone timestep
        Real64 HumRatX3;   // node air humidity ratio at t minus 3 zone timestep
        Real64 HumRatX4;   // node air humidity ratio at t minus 4 zone timestep
        Real64 HumRatDSX1; // node air humidity ratio at t minus 1 system timestep
        Real64 HumRatDSX2; // node air humidity ratio at t minus 2 system timestep
        Real64 HumRatDSX3; // node air humidity ratio at t minus 3 system timestep
        Real64 HumRatDSX4; // node air humidity ratio at t minus 4 system timestep
        Real64 HumRatW1;   // node air humidity ratio at the previous time step used in Exact and Euler method
        Real64 HumRatWMX;  // temporary node air humidity ratio to test convergence used in Exact and Euler method
        Real64 HumRatWM2;  // node air humidity ratio at time step t-2 used in Exact and Euler method

        Real64 RelHumidity; // node air relative humidity

        // sensible heat balance terms for node
        Real64 SumIntSensibleGain; // rate of heat gain from internal sensible gains(after fraction)
        Real64 SumHA;              // sum of Hc * Area for surfaces associated with this node(surface convection sensible gain term)
        Real64 SumHATsurf;         // sum of Hc * Area * Temp for surfaces associated with this node for convective heat transfer
        Real64 SumHATref;          // sum of Hc * Area * Temp for surfaces associated with this node for radiation exchange
        Real64 SumLinkMCp;         // sum of mdor*Cp for incoming airflows for this node derived from the AirflowNetwork model
        Real64 SumLinkMCpT; // sum of mdor*Cp*T for incoming airflows and source temperature for this node derived from the AirflowNetwork model
        Real64 SumSysMCp;   // sum of mdor*Cp for incoming supply airflows for this node
        Real64 SumSysMCpT;  // sum of mdor*Cp*T for incoming supply airflows and temperature for this node
        Real64 SumSysM;     // sum of mdot for incoming supply airflows for this node
        Real64 SumSysMW;    // sum of mdot*W for incoming supply airflows and temperature for this node
        Real64 NonAirSystemResponse;     // sum of convective system load
        Real64 SysDepZoneLoadsLagged;    // sum of system lagged load
        Real64 SysDepZoneLoadsLaggedOld; // sum of system lagged load
        Real64 AirCap;                   // Air storage term for energy balalce at each node
        Real64 AirHumRat;                // Air storage term for moisture balalce at each node
        // latent moisture balance terms for node
        Real64 SumIntLatentGain; // rate of heat gain form internal latent gains(after fraction)
        Real64 SumHmAW;          // sum of AREA*Moist CONVECTION COEFF*INSIDE Humidity Ratio
        Real64 SumHmARa;         // SUM OF ZONE AREA*Moist CONVECTION COEFF*Rho Air
        Real64 SumHmARaW;        // SUM OF ZONE AREA*Moist CONVECTION COEFF*Rho Air* Inside Humidity Ratio
        Real64 SumLinkM;         // sum of mdor for incoming airflows for this node derived from the AirflowNetwork model
        Real64 SumLinkMW; // sum of mdor*Cp*T for incoming airflows and source humidity ratio for this node derived from the AirflowNetwork model

        // Default Constructor
        RoomAirflowNetworkAirNodeNestedStruct()
            : ZoneVolumeFraction(0.0), HasSurfacesAssigned(false), HasIntGainsAssigned(false), NumIntGains(0), HasHVACAssigned(false), NumHVACs(0),
              AirflowNetworkNodeID(0),              // pointer to AirflowNetworkNodeData structure
              NumOfAirflowLinks(0), AirVolume(0.0), // air volume in control volume associated with this node(m3 / s)
              RhoAir(0.0),                          // current density of air for nodal control volume
              CpAir(0.0),                           // current heat capacity of air for nodal control volume
              AirTemp(0.0),                         // node air temperature
              AirTempX1(0.0),                       // node air temperature at t minus 1 zone timestep
              AirTempX2(0.0),                       // node air temperature at t minus 2 zone timestep
              AirTempX3(0.0),                       // node air temperature at t minus 3 zone timestep
              AirTempX4(0.0),                       // node air temperature at t minus 4 zone timestep
              AirTempDSX1(0.0),                     // node air temperature at t minus 1 system timestep
              AirTempDSX2(0.0),                     // node air temperature at t minus 2 system timestep
              AirTempDSX3(0.0),                     // node air temperature at t minus 3 system timestep
              AirTempDSX4(0.0),                     // node air temperature at t minus 4 system timestep
              AirTempT1(0.0),                       // node air temperature at the previous time step used in Exact and Euler method
              AirTempTMX(0.0),                      // temporary node air temperature to test convergence used in Exact and Euler method
              AirTempTM2(0.0),                      // node air temperature at time step t-2 used in Exact and Euler method
              HumRat(0.0),                          // node air humidity ratio
              HumRatX1(0.0),                        // node air humidity ratio at t minus 1 zone timestep
              HumRatX2(0.0),                        // node air humidity ratio at t minus 2 zone timestep
              HumRatX3(0.0),                        // node air humidity ratio at t minus 3 zone timestep
              HumRatX4(0.0),                        // node air humidity ratio at t minus 4 zone timestep
              HumRatDSX1(0.0),                      // node air humidity ratio at t minus 1 system timestep
              HumRatDSX2(0.0),                      // node air humidity ratio at t minus 2 system timestep
              HumRatDSX3(0.0),                      // node air humidity ratio at t minus 3 system timestep
              HumRatDSX4(0.0),                      // node air humidity ratio at t minus 4 system timestep
              HumRatW1(0.0),                        // node air humidity ratio at the previous time step used in Exact and Euler method
              HumRatWMX(0.0),                       // temporary node air humidity ratio to test convergence used in Exact and Euler method
              HumRatWM2(0.0),                       // node air humidity ratio at time step t-2 used in Exact and Euler method
              RelHumidity(0.0),                     // node air relative humidity
              // sensible heat balance terms for node
              SumIntSensibleGain(0.0),         // rate of heat gain from internal sensible gains(after fraction)
              SumHA(0.0),                      // sum of Hc * Area for surfaces associated with this node(surface convection sensible gain term)
              SumHATsurf(0.0), SumHATref(0.0), // sum of Hc * Area * Temp for surfaces associated with this node
              SumLinkMCp(0.0), SumLinkMCpT(0.0), SumSysMCp(0.0), SumSysMCpT(0.0), SumSysM(0.0), SumSysMW(0.0), NonAirSystemResponse(0.0),
              SysDepZoneLoadsLagged(0.0), SysDepZoneLoadsLaggedOld(0.0), AirCap(0.0), AirHumRat(0.0),
              // latent moisture balance terms for node
              SumIntLatentGain(0.0), // rate of heat gain form internal latent gains(after fraction)
              SumHmAW(0.0),          // sum of AREA*Moist CONVECTION COEFF*INSIDE Humidity Ratio
              SumHmARa(0.0),         // SUM OF ZONE AREA*Moist CONVECTION COEFF*Rho Air
              SumHmARaW(0.0),        // SUM OF ZONE AREA*Moist CONVECTION COEFF*Rho Air* Inside Humidity Ratio
              SumLinkM(0.0), SumLinkMW(0.0)
        {
        }
    };

    struct RoomAirflowNetworkInfoByZoneStruct // becomes RoomAirflowNetworkZoneInfo
    {
        // Members
        // user variables
        bool IsUsed;                                         // true. if RoomAirflowNetwork model used in zone
        std::string Name;                                    // Name
        std::string ZoneName;                                // Zone name in building
        int ZoneID;                                          // Index of Zone in Heat Balance
        int ActualZoneID;                                    // Index of controlled zones in ZoneCOnfigure
        std::string AvailSched;                              // Name of availability schedule
        int AvailSchedID;                                    // index of availability schedule
        int ControlAirNodeID;                                // index of roomair node that is HVAC control sensor location
        int NumOfAirNodes;                                   // Number of air nodes
        Array1D<RoomAirflowNetworkAirNodeNestedStruct> Node; // Node struct
        int ZoneNodeID;                                      // index in system Node array for this zone
        Real64 TairMean;                                     // comes from MAT
        Real64 Tstat;                                        // temperature for thermostat
        Real64 Tleaving;                                     // temperature for return air node
        Real64 Texhaust;                                     // temperature for exhaust air node
        int totNumSurfs;                                     // total surfs for this zone
        int firstSurfID;                                     // Index of first surface
        int RAFNNum;                                         // RAFN number

        // Default Constructor
        RoomAirflowNetworkInfoByZoneStruct()
            : IsUsed(false),       // true. if RoomAirflowNetwork model used in zone
              ZoneID(0),           // Index of Zone in Heat Balance
              ActualZoneID(0),     // Index of controlled zones in ZoneCOnfigure
              AvailSchedID(0),     // index of availability schedule
              ControlAirNodeID(0), // index of roomair node that is HVAC control sensor location
              NumOfAirNodes(0),    // Number of air nodes
              ZoneNodeID(0),       // index in system Node array for this zone
              TairMean(23.0),      // comes from MAT
              Tstat(23.0),         // temperature for thermostat
              Tleaving(23.0),      // temperature for return air node
              Texhaust(23.0),      // temperature for exhaust air node
              totNumSurfs(0),      // total surfs for this zone
              firstSurfID(0),      // Index of first surface
              RAFNNum(0)           // RAFN number
        {
        }
    };

} // namespace DataRoomAirModel

struct RoomAirModelData : BaseGlobalStruct
{

    int TotNumOfAirNodes = 0;
    int TotNumOfRoomAFNNodes = 0;
    Array1D_int TotNumOfZoneAirNodes;
    Array1D<Real64> ConvectiveFloorSplit;
    Array1D<Real64> InfiltratFloorSplit;
    // UCSD
    int TotUCSDDV = 0; // Total number of UCSDDV zones
    Array1D<Real64> DVHcIn;
    Array1D_bool IsZoneDV;        // Is the air model for the zone UCSDDV?
    Array1D<Real64> ZTOC;         // Temperature of occupied (lower) zone
    Array1D<Real64> AvgTempGrad;  // vertical Average Temperature Gradient in the room
    Array1D<Real64> ZTMX;         // Temperature of the mixing(upper) layer
    Array1D<Real64> MaxTempGrad;  // maximum Average Temperature Gradient in the room
    Array1D<Real64> HVACAirTemp;  // HVAC system temperature (DEG C)
    Array1D<Real64> HVACMassFlow; // HVAC system mass flow rate (KG/S)
    Array1D<Real64> ZTFloor;
    Array1D<Real64> HeightTransition;
    Array1D<Real64> FracMinFlow;
    Array1D_int ZoneDVMixedFlag;
    Array1D<Real64> ZoneDVMixedFlagRep;
    Array1D_bool ZoneAirSystemON;
    Array1D<Real64> TCMF; // comfort temperature
    Array1D<Real64> ZoneCeilingHeight;
    Array1D<Real64> MATFloor;    // [C] floor level mean air temp
    Array1D<Real64> XMATFloor;   // [C] floor level mean air temp at t minus 1 zone time step
    Array1D<Real64> XM2TFloor;   // [C] floor level mean air temp at t minus 2 zone time step
    Array1D<Real64> XM3TFloor;   // [C] floor level mean air temp at t minus 3 zone time step
    Array1D<Real64> XM4TFloor;   // [C] floor level mean air temp at t minus 4 zone time step
    Array1D<Real64> DSXMATFloor; // [C] floor level mean air temp at t minus 1 system time step
    Array1D<Real64> DSXM2TFloor; // [C] floor level mean air temp at t minus 2 system time step
    Array1D<Real64> DSXM3TFloor; // [C] floor level mean air temp at t minus 3 system time step
    Array1D<Real64> DSXM4TFloor; // [C] floor level mean air temp at t minus 4 system time step
    Array1D<Real64> MATOC;       // [C] occupied mean air temp
    Array1D<Real64> XMATOC;      // [C] occupied mean air temp at t minus 1 zone time step
    Array1D<Real64> XM2TOC;      // [C] occupied mean air temp at t minus 2 zone time step
    Array1D<Real64> XM3TOC;      // [C] occupied mean air temp at t minus 3 zone time step
    Array1D<Real64> XM4TOC;      // [C] occupied mean air temp at t minus 4 zone time step
    Array1D<Real64> DSXMATOC;    // [C] occupied mean air temp at t minus 1 system time step
    Array1D<Real64> DSXM2TOC;    // [C] occupied mean air temp at t minus 2 system time step
    Array1D<Real64> DSXM3TOC;    // [C] occupied mean air temp at t minus 3 system time step
    Array1D<Real64> DSXM4TOC;    // [C] occupied mean air temp at t minus 4 system time step
    Array1D<Real64> MATMX;       // [C] mixed (upper) mean air temp
    Array1D<Real64> XMATMX;      // [C] mixed (upper) mean air temp at t minus 1 zone time step
    Array1D<Real64> XM2TMX;      // [C] mixed (upper) mean air temp at t minus 2 zone time step
    Array1D<Real64> XM3TMX;      // [C] mixed (upper) mean air temp at t minus 3 zone time step
    Array1D<Real64> XM4TMX;      // [C] mixed (upper) mean air temp at t minus 4 zone time step
    Array1D<Real64> DSXMATMX;    // [C] mixed  mean air temp at t minus 1 system time step
    Array1D<Real64> DSXM2TMX;    // [C] mixed  mean air temp at t minus 2 system time step
    Array1D<Real64> DSXM3TMX;    // [C] mixed  mean air temp at t minus 3 system time step
    Array1D<Real64> DSXM4TMX;    // [C] mixed  mean air temp at t minus 4 system time step
    Array1D<Real64> ZTM1Floor;   // [C] difference equation's Floor air temp at t minus 1
    Array1D<Real64> ZTM2Floor;   // [C] difference equation's Floor air temp at t minus 2
    Array1D<Real64> ZTM3Floor;   // [C] difference equation's Floor air temp at t minus 3
    Array1D<Real64> ZTM1OC;      // [C] difference equation's Occupied air temp at t minus 1
    Array1D<Real64> ZTM2OC;      // [C] difference equation's Occupied air temp at t minus 2
    Array1D<Real64> ZTM3OC;      // [C] difference equation's Occupied air temp at t minus 3
    Array1D<Real64> ZTM1MX;      // [C] difference equation's Mixed  air temp at t minus 1
    Array1D<Real64> ZTM2MX;      // [C] difference equation's Mixed  air temp at t minus 1
    Array1D<Real64> ZTM3MX;      // [C] difference equation's Mixed  air temp at t minus 1
    Array1D<Real64> AIRRATFloor;
    Array1D<Real64> AIRRATOC;
    Array1D<Real64> AIRRATMX;
    // Euler and Exact solution algorithms
    Array1D<Real64> Zone1Floor;  // [C] difference equation's Floor air temp at previous dt
    Array1D<Real64> ZoneMXFloor; // [C] difference equation's Floor air temp at t minus 1
    Array1D<Real64> ZoneM2Floor; // [C] difference equation's Floor air temp at t minus 2
    Array1D<Real64> Zone1OC;     // [C] difference equation's Occupied air temp at previous dt
    Array1D<Real64> ZoneMXOC;    // [C] difference equation's Occupied air temp at t minus 1
    Array1D<Real64> ZoneM2OC;    // [C] difference equation's Occupied air temp at t minus 2
    Array1D<Real64> Zone1MX;     // [C] difference equation's Mixed  air temp at previous dt
    Array1D<Real64> ZoneMXMX;    // [C] difference equation's Mixed  air temp at t minus 1
    Array1D<Real64> ZoneM2MX;    // [C] difference equation's Mixed  air temp at t minus 2
    // UCSD-CV
    int TotUCSDCV = 0;                   // Total number of UCSDDV zones
    int CVNumAirflowNetworkSurfaces = 0; // total number of AirFlowNetwork surfaces.
    Array1D<Real64> CVHcIn;
    Array1D_bool IsZoneCV;           // Is the air model for the zone UCSDDV?
    Array1D<Real64> ZoneCVisMixing;  // Zone set to CV is actually using a mixing model
    Array1D<Real64> ZTJET;           // Jet Temperatures
    Array1D<Real64> ZTREC;           // Recirculation Temperatures
    Array1D<Real64> RoomOutflowTemp; // Temperature of air flowing out of the room
    Array1D<Real64> JetRecAreaRatio;
    Array1D<Real64> Urec;           // Recirculation region average velocity
    Array1D<Real64> Ujet;           // Jet region average velocity
    Array1D<Real64> Qrec;           // Recirculation zone total flow rate
    Array1D<Real64> Qtot;           // Total volumetric inflow rate through all active aperatures [m3/s]
    Array1D<Real64> RecInflowRatio; // Ratio of the recirculation volumetric flow rate to the total inflow flow rate []
    Array1D<Real64> Uhc;
    Array1D<Real64> Ain;                     // Inflow aperture area
    Array1D<Real64> Droom;                   // CV Zone average length
    Array1D<Real64> Dstar;                   // CV Zone average length, wind direction corrected
    Array1D<Real64> Tin;                     // Inflow air temperature
    Array1D<Real64> TotArea;                 // Sum of the areas of all apertures in the zone
    Array2D_int AirflowNetworkSurfaceUCSDCV; // table for AirflowNetwork surfaces organization
    // Interzone surfaces counts twice.
    Array1D<Real64> Rfr;          // Ration between inflow and recirculation air flows
    Array1D<Real64> ZoneCVhasREC; // Airflow pattern is C = 0, CR(1)
    bool UCSDModelUsed = false;
    bool MundtModelUsed = false;
    // UCSD-UF
    int TotUCSDUI = 0;     // total number of UCSDUI zones
    int TotUCSDUE = 0;     // total number of UCSDUE zones
    Array1D_bool IsZoneUI; // controls program flow, for interior or exterior UFAD model
    Array1D_int ZoneUFPtr;
    Array1D<Real64> UFHcIn;
    Array1D_int ZoneUFMixedFlag;
    Array1D<Real64> ZoneUFMixedFlagRep;
    Array1D<Real64> ZoneUFGamma;
    Array1D<Real64> ZoneUFPowInPlumes;            // [W]
    Array1D<Real64> ZoneUFPowInPlumesfromWindows; // [W]
    Array1D<Real64> Phi;                          // dimensionless measure of occupied subzone temperature
    // END UCSD
    // Begin NREL User-defined patterns
    int numTempDistContrldZones = 0; // count of zones with user-defined patterns
    int NumAirTempPatterns = 0;      // count of all different patterns in input file
    int NumConstantGradient = 0;     // count of constant gradient patterns in input
    int NumTwoGradientInterp = 0;    // count of two gradient interp patterns in input
    int NumNonDimensionalHeight = 0; // count of ND height profile patterns in input
    int NumSurfaceMapping = 0;       // count of generic surface map patterns in input
    bool UserDefinedUsed = false;    // true if user-defined model used anywhere
    // End User-defined patterns

    // RoomAirflowNetwork
    int NumOfRoomAirflowNetControl = 0; // count of RoomAirSettings:AirflowNetwork

    // Object Data
    Array1D<DataRoomAirModel::AirModelData> AirModel;
    Array1D<DataRoomAirModel::AirNodeData> AirNode;
    Array1D<DataRoomAirModel::DVData> ZoneUCSDDV; // UCSD
    Array1D<DataRoomAirModel::CVData> ZoneUCSDCV;
    Array1D<DataRoomAirModel::UFIData> ZoneUCSDUI;
    Array1D<DataRoomAirModel::UFEData> ZoneUCSDUE;
    Array2D<DataRoomAirModel::CVFlow> CVJetRecFlows;                                          // Jet and recirculation zone flows and properties
    Array1D<DataRoomAirModel::CVDVParameters> SurfParametersCVDV;                             // Surface parameters
    Array1D<DataRoomAirModel::TemperaturePatternStruct> RoomAirPattern;                       // user defined patterns ,various types
    Array1D<DataRoomAirModel::AirPatternInfobyZoneStruct> AirPatternZoneInfo;                 // added zone information for user defined patterns
    Array1D<DataRoomAirModel::RoomAirflowNetworkInfoByZoneStruct> RoomAirflowNetworkZoneInfo; // added zone info

    void clear_state() override
    {
        TotNumOfAirNodes = 0;
        TotNumOfRoomAFNNodes = 0;
        TotNumOfZoneAirNodes.clear();
        ConvectiveFloorSplit.clear();
        InfiltratFloorSplit.clear();
        // UCSD
        TotUCSDDV = 0; // Total number of UCSDDV zones
        DVHcIn.clear();
        IsZoneDV.clear();     // Is the air model for the zone UCSDDV?
        ZTOC.clear();         // Temperature of occupied (lower) zone
        AvgTempGrad.clear();  // vertical Average Temperature Gradient in the room
        ZTMX.clear();         // Temperature of the mixing(upper) layer
        MaxTempGrad.clear();  // maximum Average Temperature Gradient in the room
        HVACAirTemp.clear();  // HVAC system temperature (DEG C)
        HVACMassFlow.clear(); // HVAC system mass flow rate (KG/S)
        ZTFloor.clear();
        HeightTransition.clear();
        FracMinFlow.clear();
        ZoneDVMixedFlag.clear();
        ZoneDVMixedFlagRep.clear();
        ZoneAirSystemON.clear();
        TCMF.clear(); // comfort temperature
        ZoneCeilingHeight.clear();
        MATFloor.clear();    // [C] floor level mean air temp
        XMATFloor.clear();   // [C] floor level mean air temp at t minus 1 zone time step
        XM2TFloor.clear();   // [C] floor level mean air temp at t minus 2 zone time step
        XM3TFloor.clear();   // [C] floor level mean air temp at t minus 3 zone time step
        XM4TFloor.clear();   // [C] floor level mean air temp at t minus 4 zone time step
        DSXMATFloor.clear(); // [C] floor level mean air temp at t minus 1 system time step
        DSXM2TFloor.clear(); // [C] floor level mean air temp at t minus 2 system time step
        DSXM3TFloor.clear(); // [C] floor level mean air temp at t minus 3 system time step
        DSXM4TFloor.clear(); // [C] floor level mean air temp at t minus 4 system time step
        MATOC.clear();       // [C] occupied mean air temp
        XMATOC.clear();      // [C] occupied mean air temp at t minus 1 zone time step
        XM2TOC.clear();      // [C] occupied mean air temp at t minus 2 zone time step
        XM3TOC.clear();      // [C] occupied mean air temp at t minus 3 zone time step
        XM4TOC.clear();      // [C] occupied mean air temp at t minus 4 zone time step
        DSXMATOC.clear();    // [C] occupied mean air temp at t minus 1 system time step
        DSXM2TOC.clear();    // [C] occupied mean air temp at t minus 2 system time step
        DSXM3TOC.clear();    // [C] occupied mean air temp at t minus 3 system time step
        DSXM4TOC.clear();    // [C] occupied mean air temp at t minus 4 system time step
        MATMX.clear();       // [C] mixed (upper) mean air temp
        XMATMX.clear();      // [C] mixed (upper) mean air temp at t minus 1 zone time step
        XM2TMX.clear();      // [C] mixed (upper) mean air temp at t minus 2 zone time step
        XM3TMX.clear();      // [C] mixed (upper) mean air temp at t minus 3 zone time step
        XM4TMX.clear();      // [C] mixed (upper) mean air temp at t minus 4 zone time step
        DSXMATMX.clear();    // [C] mixed  mean air temp at t minus 1 system time step
        DSXM2TMX.clear();    // [C] mixed  mean air temp at t minus 2 system time step
        DSXM3TMX.clear();    // [C] mixed  mean air temp at t minus 3 system time step
        DSXM4TMX.clear();    // [C] mixed  mean air temp at t minus 4 system time step
        ZTM1Floor.clear();   // [C] difference equation's Floor air temp at t minus 1
        ZTM2Floor.clear();   // [C] difference equation's Floor air temp at t minus 2
        ZTM3Floor.clear();   // [C] difference equation's Floor air temp at t minus 3
        ZTM1OC.clear();      // [C] difference equation's Occupied air temp at t minus 1
        ZTM2OC.clear();      // [C] difference equation's Occupied air temp at t minus 2
        ZTM3OC.clear();      // [C] difference equation's Occupied air temp at t minus 3
        ZTM1MX.clear();      // [C] difference equation's Mixed  air temp at t minus 1
        ZTM2MX.clear();      // [C] difference equation's Mixed  air temp at t minus 1
        ZTM3MX.clear();      // [C] difference equation's Mixed  air temp at t minus 1
        AIRRATFloor.clear();
        AIRRATOC.clear();
        AIRRATMX.clear();
        // Euler and Exact solution algorithms
        Zone1Floor.clear();  // [C] difference equation's Floor air temp at previous dt
        ZoneMXFloor.clear(); // [C] difference equation's Floor air temp at t minus 1
        ZoneM2Floor.clear(); // [C] difference equation's Floor air temp at t minus 2
        Zone1OC.clear();     // [C] difference equation's Occupied air temp at previous dt
        ZoneMXOC.clear();    // [C] difference equation's Occupied air temp at t minus 1
        ZoneM2OC.clear();    // [C] difference equation's Occupied air temp at t minus 2
        Zone1MX.clear();     // [C] difference equation's Mixed  air temp at previous dt
        ZoneMXMX.clear();    // [C] difference equation's Mixed  air temp at t minus 1
        ZoneM2MX.clear();    // [C] difference equation's Mixed  air temp at t minus 2
        // UCSD-CV
        TotUCSDCV = 0;                   // Total number of UCSDDV zones
        CVNumAirflowNetworkSurfaces = 0; // total number of AirFlowNetwork surfaces.
        CVHcIn.clear();
        IsZoneCV.clear();        // Is the air model for the zone UCSDDV?
        ZoneCVisMixing.clear();  // Zone set to CV is actually using a mixing model
        ZTJET.clear();           // Jet Temperatures
        ZTREC.clear();           // Recirculation Temperatures
        RoomOutflowTemp.clear(); // Temperature of air flowing out of the room
        JetRecAreaRatio.clear();
        Urec.clear();           // Recirculation region average velocity
        Ujet.clear();           // Jet region average velocity
        Qrec.clear();           // Recirculation zone total flow rate
        Qtot.clear();           // Total volumetric inflow rate through all active aperatures [m3/s]
        RecInflowRatio.clear(); // Ratio of the recirculation volumetric flow rate to the total inflow flow rate []
        Uhc.clear();
        Ain.clear();                         // Inflow aperture area
        Droom.clear();                       // CV Zone average length
        Dstar.clear();                       // CV Zone average length, wind direction corrected
        Tin.clear();                         // Inflow air temperature
        TotArea.clear();                     // Sum of the areas of all apertures in the zone
        AirflowNetworkSurfaceUCSDCV.clear(); // table for AirflowNetwork surfaces organization
        // Interzone surfaces counts twice.
        Rfr.clear();          // Ration between inflow and recirculation air flows
        ZoneCVhasREC.clear(); // Airflow pattern is C = 0, CR(1)
        UCSDModelUsed = false;
        MundtModelUsed = false;
        // UCSD-UF
        TotUCSDUI = 0;    // total number of UCSDUI zones
        TotUCSDUE = 0;    // total number of UCSDUE zones
        IsZoneUI.clear(); // controls program flow, for interior or exterior UFAD model
        ZoneUFPtr.clear();
        UFHcIn.clear();
        ZoneUFMixedFlag.clear();
        ZoneUFMixedFlagRep.clear();
        ZoneUFGamma.clear();
        ZoneUFPowInPlumes.clear();            // [W]
        ZoneUFPowInPlumesfromWindows.clear(); // [W]
        Phi.clear();                          // dimensionless measure of occupied subzone temperature
        // END UCSD
        // Begin NREL User-defined patterns
        numTempDistContrldZones = 0; // count of zones with user-defined patterns
        NumAirTempPatterns = 0;      // count of all different patterns in input file
        NumConstantGradient = 0;     // count of constant gradient patterns in input
        NumTwoGradientInterp = 0;    // count of two gradient interp patterns in input
        NumNonDimensionalHeight = 0; // count of ND height profile patterns in input
        NumSurfaceMapping = 0;       // count of generic surface map patterns in input
        UserDefinedUsed = false;     // true if user-defined model used anywhere
        // End User-defined patterns

        // RoomAirflowNetwork
        NumOfRoomAirflowNetControl = 0; // count of RoomAirSettings:AirflowNetwork

        // Object Data
        AirModel.clear();
        AirNode.clear();
        ZoneUCSDDV.clear(); // UCSD
        ZoneUCSDCV.clear();
        ZoneUCSDUI.clear();
        ZoneUCSDUE.clear();
        CVJetRecFlows.clear();              // Jet and recirculation zone flows and properties
        SurfParametersCVDV.clear();         // Surface parameters
        RoomAirPattern.clear();             // user defined patterns ,various types
        AirPatternZoneInfo.clear();         // added zone information for user defined patterns
        RoomAirflowNetworkZoneInfo.clear(); // added zone info
    }
};

} // namespace EnergyPlus

#endif
