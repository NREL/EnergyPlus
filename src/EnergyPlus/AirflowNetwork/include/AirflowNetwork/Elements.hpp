// EnergyPlus, Copyright (c) 1996-2020, The Board of Trustees of the University of Illinois,
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

#ifndef ELEMENTS_HPP
#define ELEMENTS_HPP

#include "AirflowNetwork/Solver.hpp"
#include "AirflowNetwork/Properties.hpp"

namespace EnergyPlus {

namespace AirflowNetwork {

    // Using/Aliasing

    // Data
    // module should be available to other modules and routines.  Thus,
    // all variables in this module must be PUBLIC.

    // MODULE PARAMETER DEFINITIONS:
    extern int const CompTypeNum_DOP; // Detailed large opening component
    extern int const CompTypeNum_SOP; // Simple opening component
    extern int const CompTypeNum_SCR; // Surface crack component
    extern int const CompTypeNum_SEL; // Surface effective leakage ratio component
    extern int const CompTypeNum_PLR; // Distribution system crack component
    extern int const CompTypeNum_DWC; // Distribution system duct component
    extern int const CompTypeNum_CVF; // Distribution system constant volume fan component
    extern int const CompTypeNum_FAN; // Distribution system detailed fan component
    extern int const CompTypeNum_MRR; // Distribution system multiple curve fit power law resistant flow component
    extern int const CompTypeNum_DMP; // Distribution system damper component
    extern int const CompTypeNum_ELR; // Distribution system effective leakage ratio component
    extern int const CompTypeNum_CPD; // Distribution system constant pressure drop component
    extern int const CompTypeNum_COI; // Distribution system coil component
    extern int const CompTypeNum_TMU; // Distribution system terminal unit component
    extern int const CompTypeNum_EXF; // Zone exhaust fan
    extern int const CompTypeNum_HEX; // Distribution system heat exchanger
    extern int const CompTypeNum_HOP; // Horizontal opening component
    extern int const CompTypeNum_RVD; // Reheat VAV terminal damper
    extern int const CompTypeNum_OAF; // Distribution system OA
    extern int const CompTypeNum_REL; // Distribution system relief air

    // EPlus component Type
    extern int const EPlusTypeNum_SCN; // Supply connection
    extern int const EPlusTypeNum_RCN; // Return connection
    extern int const EPlusTypeNum_RHT; // Reheat terminal
    extern int const EPlusTypeNum_FAN; // Fan
    extern int const EPlusTypeNum_COI; // Heating or cooling coil
    extern int const EPlusTypeNum_HEX; // Heat exchanger
    extern int const EPlusTypeNum_RVD; // Reheat VAV terminal damper

    // EPlus node type
    extern int const EPlusTypeNum_ZIN; // Zone inlet node
    extern int const EPlusTypeNum_ZOU; // Zone outlet node
    extern int const EPlusTypeNum_SPL; // Splitter node
    extern int const EPlusTypeNum_MIX; // Mixer node
    extern int const EPlusTypeNum_OAN; // Outside air system node
    extern int const EPlusTypeNum_EXT; // OA system inlet node
    extern int const EPlusTypeNum_FIN; // Fan Inlet node
    extern int const EPlusTypeNum_FOU; // Fan Outlet Node
    extern int const EPlusTypeNum_COU; // Coil Outlet Node
    extern int const EPlusTypeNum_HXO; // Heat exchanger Outlet Node
    extern int const EPlusTypeNum_DIN; // Damper Inlet node
    extern int const EPlusTypeNum_DOU; // Damper Outlet Node
    extern int const EPlusTypeNum_SPI; // Splitter inlet Node
    extern int const EPlusTypeNum_SPO; // Splitter Outlet Node

    extern int const iWPCCntr_Input;
    extern int const iWPCCntr_SurfAvg;

    extern int const PressureCtrlExhaust;
    extern int const PressureCtrlRelief;

    // DERIVED TYPE DEFINITIONS:

    // MODULE VARIABLE DECLARATIONS:
    // Node simulation variable in air distribution system
    // Link simulation variable in air distribution system
    // Sensible and latent exchange variable in air distribution system

    extern int SimulateAirflowNetwork;
    // Vent Control  DistSys Control  Flag    Description
    //  NONE           NONE           0      No AirflowNetwork and SIMPLE
    //  SIMPLE         NONE           1      Simple calculations only
    //  MULTIZONE      NONE           2      Perform multizone calculations only
    //  NONE           DISTSYS        3      Perform distribution system during system on time only
    //  SIMPLE         DISTSYS        4      Perform distribution system during system on time and simple calculations during off time
    //  MULTIZONE      DISTSYS        5      Perform distribution system during system on time and multizone calculations during off time

    extern int const AirflowNetworkControlSimple;    // Simple calculations only
    extern int const AirflowNetworkControlMultizone; // Perform multizone calculations only
    extern int const AirflowNetworkControlSimpleADS; // Perform distribution system during system
    // on time and simple calculations during off time
    extern int const AirflowNetworkControlMultiADS; // Perform distribution system during system on time
    // and multizone calculations during off time

    extern Array1D_bool AirflowNetworkZoneFlag;

    extern int NumOfNodesMultiZone;    // Number of nodes for multizone calculation
    extern int NumOfNodesDistribution; // Number of nodes for distribution system calculation
    extern int NumOfLinksMultiZone;    // Number of links for multizone calculation
    extern int NumOfLinksDistribution; // Number of links for distribution system calculation
    extern int NumOfNodesIntraZone;    // Number of nodes for intrazone calculation
    extern int NumOfLinksIntraZone;    // Number of links for intrazone calculation

    extern int AirflowNetworkNumOfNodes; // Number of nodes for AirflowNetwork calculation
    // = NumOfNodesMultiZone+NumOfNodesDistribution
    extern int AirflowNetworkNumOfComps; // Number of components for AirflowNetwork calculation
    extern int AirflowNetworkNumOfLinks; // Number of links for AirflowNetwork calculation
    // = NumOfLinksMultiZone+NumOfLinksDistribution
    // RoomAirManager use
    extern int AirflowNetworkNumOfSurfaces; // The number of surfaces for multizone calculation
    extern int AirflowNetworkNumOfZones;    // The number of zones for multizone calculation

    extern bool RollBackFlag;                         // Roll back flag when system time step down shifting
    extern Array1D<Real64> ANZT;                      // Local zone air temperature for roll back use
    extern Array1D<Real64> ANZW;                      // Local zone air humidity ratio for roll back use
    extern Array1D<Real64> ANCO;                      // Local zone air CO2 for roll back use
    extern Array1D<Real64> ANGC;                      // Local zone air generic contaminant for roll back use
    extern int AirflowNetworkNumOfExhFan;             // Number of zone exhaust fans
    extern Array1D_bool AirflowNetworkZoneExhaustFan; // Logical to use zone exhaust fans
    extern bool AirflowNetworkFanActivated;           // Supply fan activation flag
    extern bool AirflowNetworkUnitarySystem;          // set to TRUE for unitary systems (to make answers equal, will remove eventually)
    // Multispeed HP only
    extern int MultiSpeedHPIndicator; // Indicator for multispeed heat pump use
    // Addiitonal airflow needed for an VAV fan to compensate the leakage losses and supply pathway pressure losses [kg/s]
    extern Real64 VAVTerminalRatio;       // The terminal flow ratio when a supply VAV fan reach its max flow rate
    extern bool VAVSystem;                // This flag is used to represent a VAV system
    extern Real64 ExhaustFanMassFlowRate; // Exhaust fan flow rate used in PressureStat
    extern int PressureSetFlag;           // PressureSet flag
    extern Real64 ReliefMassFlowRate;     // OA Mixer relief node flow rate used in PressureStat

    // Types

    struct AirflowNetworkSimuProp // Basic parameters for AirflowNetwork simulation
    {
        enum class Solver
        {
            SkylineLU,
            ConjugateGradient
        };
        // Members
        std::string AirflowNetworkSimuName; // Provide a unique object name
        std::string Control;                // AirflowNetwork control: MULTIZONE WITH DISTRIBUTION,
        // MULTIZONE WITHOUT DISTRIBUTION
        // MULTIZONE WITH DISTRIBUTION ONLY DURING FAN OPERATION,
        // and NO MULTIZONE OR DISTRIBUTION
        std::string WPCCntr;      // Wind pressure coefficient input control: "SURFACE-AVERAGE CALCULATION", or "INPUT"
        int iWPCCntr;             // Integer equivalent for WPCCntr field
        std::string BldgType;     // Building type: "LOWRISE" or "HIGHRISE" at WPCCntr = "SURFACE-AVERAGE CALCULATIO"
        std::string HeightOption; // Height Selection: "ExternalNode" or "OpeningHeight" at WPCCntr = "INPUT"
        int MaxIteration;         // Maximum number of iteration, default 500
        int InitFlag;             // Initialization flag
        Solver solver;
        Real64 RelTol;               // Relative airflow convergence
        Real64 AbsTol;               // Absolute airflow convergence
        Real64 ConvLimit;            // Convergence acceleration limit
        Real64 MaxPressure;          // Maximum pressure change in an element [Pa]
        Real64 Azimuth;              // Azimuth Angle of Long Axis of Building, not used at WPCCntr = "INPUT"
        Real64 AspectRatio;          // Ratio of Building Width Along Short Axis to Width Along Long Axis
        Real64 DiffP;                // Minimum pressure difference
        int ExtLargeOpeningErrCount; // Exterior large opening error count during HVAC system operation
        int ExtLargeOpeningErrIndex; // Exterior large opening error index during HVAC system operation
        int OpenFactorErrCount;      // Large opening error count at Open factor > 1.0
        int OpenFactorErrIndex;      // Large opening error error index at Open factor > 1.0
        std::string InitType;        // Initialization flag type:
        bool TExtHeightDep;          // Choice of height dependence of external node temperature
        // "ZeroNodePressures", or "LinearInitializationMethod"

        // Default Constructor
        AirflowNetworkSimuProp()
            : Control("NoMultizoneOrDistribution"), WPCCntr("Input"), MaxIteration(500), InitFlag(0), solver(Solver::SkylineLU), RelTol(1.0e-5),
              AbsTol(1.0e-5), ConvLimit(-0.5), MaxPressure(500.0), Azimuth(0.0), AspectRatio(1.0), DiffP(1.0e-4), ExtLargeOpeningErrCount(0),
              ExtLargeOpeningErrIndex(0), OpenFactorErrCount(0), OpenFactorErrIndex(0), InitType("ZeroNodePressures"), TExtHeightDep(false)
        {
        }

        // Member Constructor
        AirflowNetworkSimuProp(std::string const &AirflowNetworkSimuName, // Provide a unique object name
                               std::string const &Control,                // AirflowNetwork control: MULTIZONE WITH DISTRIBUTION,
                               std::string const &WPCCntr,      // Wind pressure coefficient input control: "SURFACE-AVERAGE CALCULATION", or "INPUT"
                               int const iWPCCntr,              // Integer equivalent for WPCCntr field
                               std::string const &BldgType,     // Building type: "LOWRISE" or "HIGHRISE" at WPCCntr = "SURFACE-AVERAGE CALCULATION"
                               std::string const &HeightOption, // Height Selection: "ExternalNode" or "OpeningHeight" at WPCCntr = "INPUT"
                               int const MaxIteration,          // Maximum number of iteration, default 500
                               int const InitFlag,              // Initialization flag
                               Real64 const RelTol,             // Relative airflow convergence
                               Real64 const AbsTol,             // Absolute airflow convergence
                               Real64 const ConvLimit,          // Convergence acceleration limit
                               Real64 const MaxPressure,        // Maximum pressure change in an element [Pa]
                               Real64 const Azimuth,            // Azimuth Angle of Long Axis of Building, not used at WPCCntr = "INPUT"
                               Real64 const AspectRatio,        // Ratio of Building Width Along Short Axis to Width Along Long Axis
                               Real64 const DiffP,              // Minimum pressure difference
                               int const ExtLargeOpeningErrCount, // Exterior large opening error count during HVAC system operation
                               int const ExtLargeOpeningErrIndex, // Exterior large opening error index during HVAC system operation
                               int const OpenFactorErrCount,      // Large opening error count at Open factor > 1.0
                               int const OpenFactorErrIndex,      // Large opening error error index at Open factor > 1.0
                               std::string const &InitType,       // Initialization flag type:
                               Solver solver,                     // Solver type
                               bool const TExtHeightDep           // Choice of height dependence of external node temperature
                               )
            : AirflowNetworkSimuName(AirflowNetworkSimuName), Control(Control), WPCCntr(WPCCntr), iWPCCntr(iWPCCntr), BldgType(BldgType),
              HeightOption(HeightOption), MaxIteration(MaxIteration), InitFlag(InitFlag), solver(solver), RelTol(RelTol), AbsTol(AbsTol),
              ConvLimit(ConvLimit), MaxPressure(MaxPressure), Azimuth(Azimuth), AspectRatio(AspectRatio), DiffP(DiffP),
              ExtLargeOpeningErrCount(ExtLargeOpeningErrCount), ExtLargeOpeningErrIndex(ExtLargeOpeningErrIndex),
              OpenFactorErrCount(OpenFactorErrCount), OpenFactorErrIndex(OpenFactorErrIndex), InitType(InitType), TExtHeightDep(TExtHeightDep)
        {
        }
    };

    struct MultizoneZoneProp // Zone information
    {
        // Members
        std::string ZoneName;    // Name of Associated EnergyPlus Thermal Zone
        std::string VentControl; // Ventilation Control Mode: "TEMPERATURE", "ENTHALPIC", "CONSTANT", or "NOVENT"
        std::string VentSchName; // Name of ventilation temperature control schedule
        Real64 Height;           // Nodal height
        Real64 OpenFactor;       // Limit Value on Multiplier for Modulating Venting Open Factor,
        // Not applicable if Vent Control Mode = CONSTANT or NOVENT
        Real64 LowValueTemp; // Lower Value on Inside/Outside Temperature Difference for
        // Modulating the Venting Open Factor with temp control
        Real64 UpValueTemp; // Upper Value on Inside/Outside Temperature Difference for
        // Modulating the Venting Open Factor with temp control
        Real64 LowValueEnth; // Lower Value on Inside/Outside Temperature Difference for
        // Modulating the Venting Open Factor with Enthalpic control
        Real64 UpValueEnth; // Upper Value on Inside/Outside Temperature Difference for
        // Modulating the Venting Open Factor with Enthalpic control
        int ZoneNum;                                // Zone number associated with ZoneName
        int VentSchNum;                             // Zone ventilation schedule number associated with ventilation schedule name
        int VentCtrNum;                             // Ventilation control mode number: 1 "Temperature", 2 "ENTHALPIC", 3 "CONSTANT", 4 "NOVENT"
        std::string VentingSchName;                 // Name of ventilation temperature control schedule
        int VentingSchNum;                          // Ventilation schedule number
        std::string SingleSidedCpType;              // Type of calculation method for single sided wind pressure coefficients
        Real64 BuildWidth;                          // The width of the building along the facade that contains this zone.
        int ASH55PeopleInd;                         // Index of people object with ASH55 comfort calcs for ventilation control
        int CEN15251PeopleInd;                      // Index of people object with CEN15251 comfort calcs for ventilation control
        std::string OccupantVentilationControlName; // Occupant ventilation control name
        int OccupantVentilationControlNum;          // Occupant ventilation control number
        int RAFNNodeNum;                            // Index of RAFN node number

        // Default Constructor
        MultizoneZoneProp()
            : VentControl("NoVent"), Height(0.0), OpenFactor(1.0), LowValueTemp(0.0), UpValueTemp(100.0), LowValueEnth(0.0), UpValueEnth(300000.0),
              ZoneNum(0), VentSchNum(0), VentCtrNum(0), VentingSchNum(0), SingleSidedCpType("STANDARD"), BuildWidth(10.0), ASH55PeopleInd(0),
              CEN15251PeopleInd(0), OccupantVentilationControlNum(0), RAFNNodeNum(0)
        {
        }
    };

    struct MultizoneSurfaceProp // Surface information
    {
        // Members
        std::string SurfName;         // Name of Associated EnergyPlus surface
        std::string OpeningName;      // Name of opening component, either simple or detailed large opening
        std::string ExternalNodeName; // Name of external node, but not used at WPC="INPUT"
        Real64 Factor;                // Crack Actual Value or Window Open Factor for Ventilation
        int SurfNum;                  // Surface number
        std::array<int, 2> NodeNums;  // Positive: Zone numbers; 0: External
        Real64 OpenFactor;            // Surface factor
        Real64 OpenFactorLast;        // Surface factor at previous time step
        bool EMSOpenFactorActuated;   // True if EMS actuation is on
        Real64 EMSOpenFactor;         // Surface factor value from EMS for override
        Real64 Height;                // Surface Height
        Real64 Width;                 // Surface width
        Real64 CHeight;               // Surface central height in z direction
        std::string VentControl;      // Ventilation Control Mode: TEMPERATURE, ENTHALPIC, CONSTANT, ZONELEVEL or NOVENT
        std::string VentSchName;      // ! Name of ventilation temperature control schedule
        Real64 ModulateFactor;        // Limit Value on Multiplier for Modulating Venting Open Factor
        Real64 LowValueTemp;          // Lower Value on Inside/Outside Temperature Difference for
        // Modulating the Venting Open Factor with temp control
        Real64 UpValueTemp; // Upper Value on Inside/Outside Temperature Difference for
        // Modulating the Venting Open Factor with temp control
        Real64 LowValueEnth; // Lower Value on Inside/Outside Temperature Difference for
        // Modulating the Venting Open Factor with Enthalpic control
        Real64 UpValueEnth; // Upper Value on Inside/Outside Temperature Difference for
        // Modulating the Venting Open Factor with Enthalpic control
        std::string VentingSchName;                 // Name of ventilation temperature control schedule
        int VentSchNum;                             // Zone ventilation schedule number associated with ventilation schedule name
        int VentSurfCtrNum;                         // Ventilation control mode number: 1 "Temperature", 2 "ENTHALPIC", 3 "CONSTANT", 4 "NOVENT"
        int VentingSchNum;                          // Ventilation schedule number
        int ZonePtr;                                // Pointer to inside face zone
        bool IndVentControl;                        // Individual surface venting control
        int ExtLargeOpeningErrCount;                // Exterior large opening error count during HVAC system operation
        int ExtLargeOpeningErrIndex;                // Exterior large opening error index during HVAC system operation
        int OpenFactorErrCount;                     // Large opening error count at Open factor > 1.0
        int OpenFactorErrIndex;                     // Large opening error error index at Open factor > 1.0
        Real64 Multiplier;                          // Window multiplier
        bool HybridVentClose;                       // Hybrid ventilation window close control logical
        bool HybridCtrlGlobal;                      // Hybrid ventilation global control logical
        bool HybridCtrlMaster;                      // Hybrid ventilation global control master
        Real64 WindModifier;                        // Wind modifier from hybrid ventilation control
        std::string OccupantVentilationControlName; // Occupant ventilation control name
        int OccupantVentilationControlNum;          // Occupant ventilation control number
        int OpeningStatus;                          // Open status at current time step
        int PrevOpeningstatus;                      // Open status at previous time step
        Real64 CloseElapsedTime;                    // Elapsed time during closing (min)
        Real64 OpenElapsedTime;                     // Elapsed time during closing (min)
        int ClosingProbStatus;                      // Closing probability status
        int OpeningProbStatus;                      // Opening probability status
        bool RAFNflag;                              // True if this surface is used in AirflowNetwork:IntraZone:Linkage
        bool NonRectangular;                        // True if this surface is not rectangular
        int EquivRecMethod;             // Equivalent Rectangle Method input: 1 Height; 2 Base surface aspect ratio; 3 User input aspect ratio
        Real64 EquivRecUserAspectRatio; // user input value when EquivRecMethod = 3

        // Default Constructor
        MultizoneSurfaceProp()
            : Factor(0.0), SurfNum(0), NodeNums{{0, 0}}, OpenFactor(0.0), OpenFactorLast(0.0), EMSOpenFactorActuated(false), EMSOpenFactor(0.0),
              Height(0.0), Width(0.0), CHeight(0.0), VentControl("ZONELEVEL"), ModulateFactor(0.0), LowValueTemp(0.0), UpValueTemp(100.0),
              LowValueEnth(0.0), UpValueEnth(300000.0), VentSchNum(0), VentSurfCtrNum(0), VentingSchNum(0), ZonePtr(0), IndVentControl(false),
              ExtLargeOpeningErrCount(0), ExtLargeOpeningErrIndex(0), OpenFactorErrCount(0), OpenFactorErrIndex(0), Multiplier(1.0),
              HybridVentClose(false), HybridCtrlGlobal(false), HybridCtrlMaster(false), WindModifier(1.0), OccupantVentilationControlNum(0),
              OpeningStatus(0), PrevOpeningstatus(0), CloseElapsedTime(0.0), OpenElapsedTime(0.0), ClosingProbStatus(0), OpeningProbStatus(0),
              RAFNflag(false), NonRectangular(false), EquivRecMethod(1), EquivRecUserAspectRatio(1.0)
        {
        }
    };

    struct AirflowElement
    {
    };

    struct DetailedOpening : public AirflowElement // Large detailed opening component
    {
        // Members
        std::string Name;     // Name of large detailed opening component
        Real64 FlowCoef;      // Air Mass Flow Coefficient When Window or Door Is Closed [kg/s at 1Pa]
        Real64 FlowExpo;      // Air Mass Flow exponent When Window or Door Is Closed [dimensionless]
        std::string TypeName; // Name of Large vertical opening type
        int LVOType;          // Large vertical opening type number
        Real64 LVOValue;      // Extra crack length for LVO type 1 with multiple operable parts,
        // or Height of pivoting axis for LVO type 2
        int NumFac;         // Number of Opening Factor Values
        Real64 OpenFac1;    // Opening factor #1
        Real64 DischCoeff1; // Discharge coefficient for opening factor #1
        Real64 WidthFac1;   // Width factor for for Opening factor #1
        Real64 HeightFac1;  // Height factor for opening factor #1
        Real64 StartHFac1;  // Start height factor for opening factor #1
        Real64 OpenFac2;    // Opening factor #2
        Real64 DischCoeff2; // Discharge coefficient for opening factor #2
        Real64 WidthFac2;   // Width factor for for Opening factor #2
        Real64 HeightFac2;  // Height factor for opening factor #2
        Real64 StartHFac2;  // Start height factor for opening factor #2
        Real64 OpenFac3;    // Opening factor #3
        Real64 DischCoeff3; // Discharge coefficient for opening factor #3
        Real64 WidthFac3;   // Width factor for for Opening factor #3
        Real64 HeightFac3;  // Height factor for opening factor #3
        Real64 StartHFac3;  // Start height factor for opening factor #3
        Real64 OpenFac4;    // Opening factor #4
        Real64 DischCoeff4; // Discharge coefficient for opening factor #4
        Real64 WidthFac4;   // Width factor for for Opening factor #4
        Real64 HeightFac4;  // Height factor for opening factor #4
        Real64 StartHFac4;  // Start height factor for opening factor #4
        Real64 OpenFactor;  // Opening factor
        int WidthErrCount;  // Width error count
        int WidthErrIndex;  // Width error index
        int HeightErrCount; // Height error count
        int HeightErrIndex; // Height error index

        // Default Constructor
        DetailedOpening()
            : FlowCoef(0.0), FlowExpo(0.0), TypeName("NONPIVOTED"), LVOType(0), LVOValue(0.0), NumFac(0), OpenFac1(0.0), DischCoeff1(0.0),
              WidthFac1(0.0), HeightFac1(0.0), StartHFac1(0.0), OpenFac2(0.0), DischCoeff2(0.0), WidthFac2(0.0), HeightFac2(0.0), StartHFac2(0.0),
              OpenFac3(0.0), DischCoeff3(0.0), WidthFac3(0.0), HeightFac3(0.0), StartHFac3(0.0), OpenFac4(0.0), DischCoeff4(0.0), WidthFac4(0.0),
              HeightFac4(0.0), StartHFac4(0.0), OpenFactor(0.0), WidthErrCount(0), WidthErrIndex(0), HeightErrCount(0), HeightErrIndex(0)
        {
        }

        int calculate(bool const LFLAG,           // Initialization flag.If = 1, use laminar relationship
                      Real64 const PDROP,         // Total pressure drop across a component (P1 - P2) [Pa]
                      int const EP_UNUSED(i),     // Linkage number
                      const AirProperties &propN, // Node 1 properties
                      const AirProperties &propM, // Node 2 properties
                      std::array<Real64, 2> &F,   // Airflow through the component [kg/s]
                      std::array<Real64, 2> &DF   // Partial derivative:  DF/DP
        );
    };

    struct SimpleOpening : public AirflowElement // Large simple opening component
    {
        // Members
        std::string Name;  // Name of large simple opening component
        Real64 FlowCoef;   // Air Mass Flow Coefficient When Window or Door Is Closed [kg/s at 1Pa]
        Real64 FlowExpo;   // Air Mass Flow exponent When Window or Door Is Closed [dimensionless]
        Real64 MinRhoDiff; // Minimum density difference for two-way flow
        Real64 DischCoeff; // Discharge coefficient at full opening
        Real64 OpenFactor; // Opening factor

        // Default Constructor
        SimpleOpening() : FlowCoef(0.0), FlowExpo(0.0), MinRhoDiff(0.0), DischCoeff(0.0), OpenFactor(0.0)
        {
        }

        int calculate(bool const LFLAG,           // Initialization flag.If = 1, use laminar relationship
                      Real64 const PDROP,         // Total pressure drop across a component (P1 - P2) [Pa]
                      int const i,                // Linkage number
                      const AirProperties &propN, // Node 1 properties
                      const AirProperties &propM, // Node 2 properties
                      std::array<Real64, 2> &F,   // Airflow through the component [kg/s]
                      std::array<Real64, 2> &DF   // Partial derivative:  DF/DP
        );
    };

    struct HorizontalOpening : public AirflowElement // Large horizontal opening component
    {
        // Members
        std::string Name;  // Name of large horizontal opening component
        Real64 FlowCoef;   // Air Mass Flow Coefficient When Window or Door Is Closed [kg/s at 1Pa]
        Real64 FlowExpo;   // Air Mass Flow exponent When Window or Door Is Closed [dimensionless]
        Real64 Slope;      // Sloping plane angle
        Real64 DischCoeff; // Discharge coefficient at full opening

        // Default Constructor
        HorizontalOpening() : FlowCoef(0.0), FlowExpo(0.0), Slope(0.0), DischCoeff(0.0)
        {
        }

        int calculate(bool const LFLAG,           // Initialization flag.If = 1, use laminar relationship
                      Real64 const PDROP,         // Total pressure drop across a component (P1 - P2) [Pa]
                      int const i,                // Linkage number
                      const AirProperties &propN, // Node 1 properties
                      const AirProperties &propM, // Node 2 properties
                      std::array<Real64, 2> &F,   // Airflow through the component [kg/s]
                      std::array<Real64, 2> &DF   // Partial derivative:  DF/DP
        );
    };

    struct ReferenceConditions // Surface crack standard conditions
    {
        // Members
        std::string name;     // Name of standard conditions component
        Real64 temperature;   // Standard temperature for crack data
        Real64 pressure;      // Standard barometric pressure for crack data
        Real64 humidityRatio; // Standard humidity ratio for crack data

        ReferenceConditions(const std::string &name, Real64 temperature = 20.0, Real64 pressure = 101325.0, Real64 humidityRatio = 0.0)
            : name(name), temperature(temperature), pressure(pressure), humidityRatio(humidityRatio)
        {
        }

        // ReferenceConditions(Real64 temperature = 20.0, Real64 pressure = 101325.0, Real64 humidityRatio = 0.0)
        //    : temperature(temperature), pressure(pressure), humidityRatio(humidityRatio)
        //{
        //}
    };

    struct SurfaceCrack : public AirflowElement // Surface crack component
    {
        // Members
        std::string Name; // Name of crack component
        // std::string ExternalNodeNames; // Name of external node.Not required for internal surface
        Real64 FlowCoef;  // Air Mass Flow Coefficient When Window or Door Is Closed [kg/s at 1Pa]
        Real64 FlowExpo;  // Air Mass Flow exponent When Window or Door Is Closed [dimensionless]
        Real64 StandardT; // Standard temperature for crack data
        Real64 StandardP; // Standard barometric pressure for crack data
        Real64 StandardW; // Standard humidity ratio for crack data

        // Default Constructor
        SurfaceCrack() : FlowCoef(0.0), FlowExpo(0.0), StandardT(0.0), StandardP(0.0), StandardW(0.0)
        {
        }

        int calculate(bool const LFLAG,           // Initialization flag.If = 1, use laminar relationship
                      Real64 const PDROP,         // Total pressure drop across a component (P1 - P2) [Pa]
                      int const i,                // Linkage number
                      const AirProperties &propN, // Node 1 properties
                      const AirProperties &propM, // Node 2 properties
                      std::array<Real64, 2> &F,   // Airflow through the component [kg/s]
                      std::array<Real64, 2> &DF   // Partial derivative:  DF/DP
        );
    };

    struct EffectiveLeakageArea : public AirflowElement // Surface effective leakage area component
    {
        // Members
        std::string Name;   // Name of effective leakage area component
        Real64 ELA;         // Effective leakage area
        Real64 DischCoeff;  // Discharge coefficient
        Real64 RefDeltaP;   // Reference pressure difference
        Real64 FlowExpo;    // Air Mass Flow exponent When Window or Door Is Closed
        Real64 TestDeltaP;  // Testing pressure difference
        Real64 TestDisCoef; // Testing Discharge coefficient

        // Default Constructor
        EffectiveLeakageArea() : ELA(0.0), DischCoeff(0.0), RefDeltaP(0.0), FlowExpo(0.0), TestDeltaP(0.0), TestDisCoef(0.0)
        {
        }

        int calculate(bool const LFLAG,           // Initialization flag.If = 1, use laminar relationship
                      Real64 const PDROP,         // Total pressure drop across a component (P1 - P2) [Pa]
                      int const i,                // Linkage number
                      const AirProperties &propN, // Node 1 properties
                      const AirProperties &propM, // Node 2 properties
                      std::array<Real64, 2> &F,   // Airflow through the component [kg/s]
                      std::array<Real64, 2> &DF   // Partial derivative:  DF/DP
        );
    };

    struct ZoneExhaustFan : public AirflowElement // Zone exhaust fan component
    {
        // Members
        std::string Name; // Name of exhaust fan component
        Real64 FlowRate;  // mass flow rate
        int SchedPtr;     // Schedule pointer
        Real64 FlowCoef;  // Air Mass Flow Coefficient [kg/s at 1Pa]
        Real64 FlowExpo;  // Air Mass Flow exponent [dimensionless]
        Real64 StandardT; // Standard temperature for crack data
        Real64 StandardP; // Standard barometric pressure for crack data
        Real64 StandardW; // Standard humidity ratio for crack data
        int InletNode;    // Inlet node number
        int OutletNode;   // Outlet node number
        int EPlusZoneNum; // Zone number
        int PressCtrlNum; // pressure control number

        // Default Constructor
        ZoneExhaustFan()
            : FlowRate(0.0), SchedPtr(0), FlowCoef(0.0), FlowExpo(0.0), StandardT(0.0), StandardP(0.0), StandardW(0.0), InletNode(0), OutletNode(0),
              EPlusZoneNum(0), PressCtrlNum(0)
        {
        }

        int calculate(bool const LFLAG,           // Initialization flag.If = 1, use laminar relationship
                      Real64 const PDROP,         // Total pressure drop across a component (P1 - P2) [Pa]
                      int const i,                // Linkage number
                      const AirProperties &propN, // Node 1 properties
                      const AirProperties &propM, // Node 2 properties
                      std::array<Real64, 2> &F,   // Airflow through the component [kg/s]
                      std::array<Real64, 2> &DF   // Partial derivative:  DF/DP
        );
    };

    struct MultizoneExternalNodeProp // External node properties
    {
        // Members
        std::string Name;      // Name of external node
        Real64 azimuth;        // Azimuthal angle of the associated surface
        Real64 height;         // Nodal height
        int ExtNum;            // External node number
        int OutAirNodeNum;     // Outdoor air node number
        int facadeNum;         // Facade number
        int curve;             // Curve ID, replace with pointer after curve refactor
        bool symmetricCurve;   // Symmtric curves are evaluated from 0 to 180, others are evaluated from 0 to 360
        bool useRelativeAngle; // Determines whether the wind angle is relative to the surface or absolute

        // Default Constructor
        MultizoneExternalNodeProp()
            : azimuth(0.0), height(0.0), ExtNum(0), OutAirNodeNum(0), facadeNum(0), curve(0), symmetricCurve(false), useRelativeAngle(false)
        {
        }
    };

    struct DeltaCpProp
    {
        // Members
        Array1D<Real64> WindDir; // Wind direction

        // Default Constructor
        DeltaCpProp()
        {
        }
    };

    struct IntraZoneNodeProp // Intra zone node data
    {
        // Members
        std::string Name;         // Name of node
        std::string RAFNNodeName; // RoomAir model node name
        Real64 Height;            // Nodal height
        int RAFNNodeNum;          // RoomAir model node number
        int ZoneNum;              // Zone number
        int AFNZoneNum;           // MultiZone number

        // Default Constructor
        IntraZoneNodeProp() : Height(0.0), RAFNNodeNum(0), ZoneNum(0), AFNZoneNum(0)
        {
        }
    };

    struct AirflowNetworkLinkage // AirflowNetwork linkage data base class
    {
        // Members
        std::string Name;                     // Provide a unique linkage name
        std::array<std::string, 2> NodeNames; // Names of nodes (limited to 2)
        std::array<Real64, 2> NodeHeights;    // Node heights
        std::string CompName;                 // Name of element
        int CompNum;                          // Element Number
        std::array<int, 2> NodeNums;          // Node numbers
        int LinkNum;                          // Linkage number

        // Default Constructor
        AirflowNetworkLinkage() : NodeHeights{{0.0, 0.0}}, CompNum(0), NodeNums{{0, 0}}, LinkNum(0)
        {
        }
    };

    struct IntraZoneLinkageProp : public AirflowNetworkLinkage // Intra zone linkage data
    {
        // Members
        std::string SurfaceName; // Connection Surface Name

        // Default Constructor
        IntraZoneLinkageProp() : AirflowNetworkLinkage()
        {
        }
    };

    struct DisSysNodeProp // CP Value
    {
        // Members
        std::string Name;      // Name of node
        std::string EPlusName; // EnergyPlus node name
        std::string EPlusType; // EnergyPlus node type
        Real64 Height;         // Nodal height
        int EPlusNodeNum;      // EPlus node number
        int AirLoopNum;        // AirLoop number

        // Default Constructor
        DisSysNodeProp() : Height(0.0), EPlusNodeNum(0), AirLoopNum(0)
        {
        }
    };

    struct DuctLeak : public AirflowElement // duct leak component
    {
        // Members
        std::string Name; // Name of component leak
        Real64 FlowCoef;  // Air Mass Flow Coefficient [kg/s at 1Pa]
        Real64 FlowExpo;  // Air Mass Flow exponent [dimensionless]

        // Default Constructor
        DuctLeak() : FlowCoef(0.0), FlowExpo(0.0)
        {
        }

        int calculate(bool const LFLAG,           // Initialization flag.If = 1, use laminar relationship
                      Real64 const PDROP,         // Total pressure drop across a component (P1 - P2) [Pa]
                      int const i,                // Linkage number
                      const AirProperties &propN, // Node 1 properties
                      const AirProperties &propM, // Node 2 properties
                      std::array<Real64, 2> &F,   // Airflow through the component [kg/s]
                      std::array<Real64, 2> &DF   // Partial derivative:  DF/DP
        );
    };

    struct EffectiveLeakageRatio : public AirflowElement // effective leakage ratio component
    {
        // Members
        std::string Name; // Name of component leak
        Real64 ELR;       // Value of effective leakage ratio
        Real64 FlowRate;  // Maximum airflow rate
        Real64 RefPres;   // Reference pressure difference
        Real64 FlowExpo;  // Air Mass Flow exponent

        // Default Constructor
        EffectiveLeakageRatio() : ELR(0.0), FlowRate(0.0), RefPres(0.0), FlowExpo(0.0)
        {
        }

        int calculate(bool const LFLAG,           // Initialization flag.If = 1, use laminar relationship
                      Real64 const PDROP,         // Total pressure drop across a component (P1 - P2) [Pa]
                      int const EP_UNUSED(i),     // Linkage number
                      const AirProperties &propN, // Node 1 properties
                      const AirProperties &propM, // Node 2 properties
                      std::array<Real64, 2> &F,   // Airflow through the component [kg/s]
                      std::array<Real64, 2> &DF   // Partial derivative:  DF/DP
        );
    };

    struct Duct : public AirflowElement // Duct component
    {
        // Members
        std::string Name;         // Name of duct component
        Real64 L;                 // Duct length [m]
        Real64 hydraulicDiameter; // Hydraulic diameter [m]
        Real64 A;                 // Cross section area [m2]
        Real64 roughness;         // Surface roughness [m]
        Real64 TurDynCoef;        // Turbulent dynamic loss coefficient
        Real64 UThermConduct;     // Conduction heat transmittance [W/m2-K]
        Real64 UMoisture;         // Overall moisture transmittance [kg/m2]
        Real64 InsideConvCoeff;   // Inside convection coefficient [W/m2-K]
        Real64 OutsideConvCoeff;  // Outside convection coefficient [W/m2-K]
        Real64 MThermal;          // Thermal capacity [J/K]
        Real64 MMoisture;         // Moisture capacity [kg]
        Real64 LamDynCoef;        // Laminar dynamic loss coefficient
        Real64 LamFriCoef;        // Laminar friction loss coefficient
        Real64 InitLamCoef;       // Coefficient of linear initialization
        Real64 RelRough;          // e/D: relative roughness,
        Real64 RelL;              // L/D: relative length,
        Real64 g;                 // 1/sqrt(Darcy friction factor),
        Real64 A1;                // 1.14 - 0.868589*ln(e/D),

        // Default Constructor
        Duct()
            : L(0.0), hydraulicDiameter(0.0), A(0.0), roughness(0.0), TurDynCoef(0.0), UThermConduct(0.0), UMoisture(0.0), InsideConvCoeff(0.0),
              OutsideConvCoeff(0.0), MThermal(0.0), MMoisture(0.0), LamDynCoef(0.0), LamFriCoef(0.0), InitLamCoef(0.0), RelRough(0.0), RelL(0.0),
              g(0.0), A1(0.0)
        {
        }

        int calculate(bool const LFLAG,           // Initialization flag.If = 1, use laminar relationship
                      Real64 const PDROP,         // Total pressure drop across a component (P1 - P2) [Pa]
                      int const i,                // Linkage number
                      const AirProperties &propN, // Node 1 properties
                      const AirProperties &propM, // Node 2 properties
                      std::array<Real64, 2> &F,   // Airflow through the component [kg/s]
                      std::array<Real64, 2> &DF   // Partial derivative:  DF/DP
        );
    };

    struct Damper : public AirflowElement // Damper component
    {
        // Members
        std::string Name; // Name of damper component
        Real64 LTP;       // Value for laminar turbulent transition
        Real64 LamFlow;   // Laminar flow coefficient
        Real64 TurFlow;   // Turbulent flow coefficient
        Real64 FlowExpo;  // Air Mass Flow exponent
        Real64 FlowMin;   // Minimum control air mass rate
        Real64 FlowMax;   // Maximum control air mass rate
        Real64 A0;        // First polynomial coefficient of the control variable (constant coefficient)
        Real64 A1;        // Second polynomial coefficient of the control variable (linear coefficient)
        Real64 A2;        // Third polynomial coefficient of the control variable (quadratic coefficient)
        Real64 A3;        // Fourth polynomial coefficient of the control variable (cubic coefficient)

        // Default Constructor
        Damper() : LTP(0.0), LamFlow(0.0), TurFlow(0.0), FlowExpo(0.0), FlowMin(0.0), FlowMax(0.0), A0(0.0), A1(0.0), A2(0.0), A3(0.0)
        {
        }

        int calculate(bool const LFLAG,           // Initialization flag.If = 1, use laminar relationship
                      Real64 const PDROP,         // Total pressure drop across a component (P1 - P2) [Pa]
                      int const i,                // Linkage number
                      const AirProperties &propN, // Node 1 properties
                      const AirProperties &propM, // Node 2 properties
                      std::array<Real64, 2> &F,   // Airflow through the component [kg/s]
                      std::array<Real64, 2> &DF   // Partial derivative:  DF/DP
        );
    };

    struct ConstantVolumeFan : public AirflowElement // Constant volume fan component
    {
        // Members
        std::string Name;          // Name of detailed fan component
        Real64 FlowRate;           // Air volume flow rate
        Real64 Ctrl;               // Control ratio
        int FanTypeNum;            // Fan type: Constant volume or ONOFF
        int FanIndex;              // Fan index
        int InletNode;             // Inlet node number
        int OutletNode;            // Outlet node number
        Real64 MaxAirMassFlowRate; // Max Specified MAss Flow Rate of Damper [kg/s]
        int AirLoopNum;            // Air loop number
        bool FanModelFlag;         // True, this fan is FAN:SYSTEMMODEL

        // Default Constructor
        ConstantVolumeFan()
            : FlowRate(0.0), Ctrl(0.0), FanTypeNum(0), FanIndex(0), InletNode(0), OutletNode(0), MaxAirMassFlowRate(0.0), AirLoopNum(0), FanModelFlag(false)
        {
        }

        int calculate(bool const LFLAG,           // Initialization flag.If = 1, use laminar relationship
                      Real64 const PDROP,         // Total pressure drop across a component (P1 - P2) [Pa]
                      int const i,                // Linkage number
                      const AirProperties &propN, // Node 1 properties
                      const AirProperties &propM, // Node 2 properties
                      std::array<Real64, 2> &F,   // Airflow through the component [kg/s]
                      std::array<Real64, 2> &DF   // Partial derivative:  DF/DP
        );
    };

    struct DetailedFan : public AirflowElement // Detailed fan component
    {
        // Members
        std::string Name;      // Name of constant volume fan component
        Real64 FlowCoef;       // Coefficient for linear initialization [kg/s at 1Pa]
        Real64 FlowExpo;       // Turbulent flow coefficient [dimensionless]
        Real64 RhoAir;         // Reference air density
        Real64 Qfree;          // Free delivery flow at P=0
        Real64 Pshut;          // Shutoff pressure at Q=0
        Real64 TranRat;        // Flow coefficient at laminar/turbulent transition
        int n;                 // Number of ranges for fan performance curve
        Array1D<Real64> Coeff; // Coefficients of fan performance curve.
        // Each range has a min flow rate and 4 coefficients

        // Default Constructor
        DetailedFan() : FlowCoef(0.0), FlowExpo(0.0), RhoAir(0.0), Qfree(0.0), Pshut(0.0), TranRat(0.0)
        {
        }

        int calculate(bool const LFLAG,           // Initialization flag.If = 1, use laminar relationship
                      Real64 const PDROP,         // Total pressure drop across a component (P1 - P2) [Pa]
                      int const i,                // Linkage number
                      const AirProperties &propN, // Node 1 properties
                      const AirProperties &propM, // Node 2 properties
                      std::array<Real64, 2> &F,   // Airflow through the component [kg/s]
                      std::array<Real64, 2> &DF   // Partial derivative:  DF/DP
        );
    };

    struct DisSysCompCoilProp : public AirflowElement // Coil component
    {
        // Members
        std::string Name;         // Name of coil component
        std::string EPlusType;    // EnergyPlus coil type
        Real64 L;                 // Air path length
        Real64 hydraulicDiameter; // Air path hydraulic diameter
        int AirLoopNum;           // AirLoop number

        // Default Constructor
        DisSysCompCoilProp() : L(0.0), hydraulicDiameter(0.0), AirLoopNum(0)
        {
        }

        int calculate(bool const LFLAG,           // Initialization flag.If = 1, use laminar relationship
                      Real64 const PDROP,         // Total pressure drop across a component (P1 - P2) [Pa]
                      int const i,                // Linkage number
                      const AirProperties &propN, // Node 1 properties
                      const AirProperties &propM, // Node 2 properties
                      std::array<Real64, 2> &F,   // Airflow through the component [kg/s]
                      std::array<Real64, 2> &DF   // Partial derivative:  DF/DP
        );
    };

    struct DisSysCompHXProp : public AirflowElement // Coil component
    {
        // Members
        std::string Name;         // Name of coil component
        std::string EPlusType;    // EnergyPlus coil type
        Real64 L;                 // Air path length
        Real64 hydraulicDiameter; // Air path hydraulic diameter
        bool CoilParentExists;    // Is a coil component

        // Default Constructor
        DisSysCompHXProp() : L(0.0), hydraulicDiameter(0.0), CoilParentExists(false)
        {
        }

        int calculate(bool const LFLAG,           // Initialization flag.If = 1, use laminar relationship
                      Real64 const PDROP,         // Total pressure drop across a component (P1 - P2) [Pa]
                      int const i,                // Linkage number
                      const AirProperties &propN, // Node 1 properties
                      const AirProperties &propM, // Node 2 properties
                      std::array<Real64, 2> &F,   // Airflow through the component [kg/s]
                      std::array<Real64, 2> &DF   // Partial derivative:  DF/DP
        );
    };

    struct DisSysCompTermUnitProp : public AirflowElement // Terminal unit component
    {
        // Members
        std::string Name;         // Name of coil component
        std::string EPlusType;    // EnergyPlus coil type
        Real64 L;                 // Air path length
        Real64 hydraulicDiameter; // Air path hydraulic diameter
        int DamperInletNode;      // Damper inlet node number
        int DamperOutletNode;     // Damper outlet node number
        int AirLoopNum;           // AirLoop number

        // Default Constructor
        DisSysCompTermUnitProp() : L(0.0), hydraulicDiameter(0.0), DamperInletNode(0), DamperOutletNode(0), AirLoopNum(0)
        {
        }

        int calculate(bool const LFLAG,           // Initialization flag.If = 1, use laminar relationship
                      Real64 const PDROP,         // Total pressure drop across a component (P1 - P2) [Pa]
                      int const i,                // Linkage number
                      const AirProperties &propN, // Node 1 properties
                      const AirProperties &propM, // Node 2 properties
                      std::array<Real64, 2> &F,   // Airflow through the component [kg/s]
                      std::array<Real64, 2> &DF   // Partial derivative:  DF/DP
        );
    };

    struct ConstantPressureDrop : public AirflowElement // Constant pressure drop component
    {
        // Members
        std::string Name; // Name of constant pressure drop component
        Real64 A;         // cross section area
        Real64 DP;        // Pressure difference across the component

        // Default Constructor
        ConstantPressureDrop() : A(0.0), DP(0.0)
        {
        }

        int calculate(bool const LFLAG,           // Initialization flag.If = 1, use laminar relationship
                      const Real64 PDROP,         // Total pressure drop across a component (P1 - P2) [Pa]
                      int const i,                // Linkage number
                      const AirProperties &propN, // Node 1 properties
                      const AirProperties &propM, // Node 2 properties
                      std::array<Real64, 2> &F,   // Airflow through the component [kg/s]
                      std::array<Real64, 2> &DF   // Partial derivative:  DF/DP
        );
    };

    struct DisSysLinkageProp : public AirflowNetworkLinkage // Distribution system linkage data
    {
        // Members
        std::string ZoneName; // Name of zone
        int ZoneNum;          // Zone Number

        // Default Constructor
        DisSysLinkageProp() : AirflowNetworkLinkage(), ZoneNum(0)
        {
        }
    };

    struct AirflowNetworkNodeProp // AirflowNetwork nodal data
    {
        // Members
        std::string Name;      // Provide a unique node name
        std::string NodeType;  // Provide node type "External", "Thermal Zone" or "Other"
        std::string EPlusNode; // EnergyPlus node name
        Real64 NodeHeight;     // Node height [m]
        int NodeNum;           // Node number
        int NodeTypeNum;       // Node type with integer number
        // 0: Calculated, 1: Given pressure;
        std::string EPlusZoneName; // EnergyPlus node name
        int EPlusZoneNum;          // E+ zone number
        int EPlusNodeNum;
        int ExtNodeNum;
        int OutAirNodeNum;
        int EPlusTypeNum;
        int RAFNNodeNum; // RoomAir model node number
        int NumOfLinks;  // Number of links for RoomAir model
        int AirLoopNum;  // AirLoop number

        // Default Constructor
        AirflowNetworkNodeProp()
            : NodeHeight(0.0), NodeNum(0), NodeTypeNum(0), EPlusZoneNum(0), EPlusNodeNum(0), ExtNodeNum(0), OutAirNodeNum(0), EPlusTypeNum(0),
              RAFNNodeNum(0), NumOfLinks(0), AirLoopNum(0)
        {
        }
    };

    struct AirflowNetworkCompProp // AirflowNetwork element data
    {
        // Members
        std::string Name;          // Provide a unique element name
        int CompTypeNum;           // Provide numeric equivalent for AirflowNetworkCompType
        int TypeNum;               // Component number under same component type
        int CompNum;               // General component number
        std::string EPlusName;     // Provide a unique element name
        std::string EPlusCompName; // Provide EPlus component name or Other
        std::string EPlusType;     // Provide EPlus type, such as terminal reheat, coil, etc. 9/30/03 or Other
        int EPlusTypeNum;          // Provide EPlus component type

        // Default Constructor
        AirflowNetworkCompProp() : CompTypeNum(0), TypeNum(0), CompNum(0), EPlusTypeNum(0)
        {
        }
    };

    struct AirflowNetworkLinkageProp : public AirflowNetworkLinkage // AirflowNetwork linkage data
    {
        // Members
        std::string ZoneName; // Name of zone
        int ZoneNum;          // Zone Number
        int DetOpenNum;       // Large Opening number
        int ConnectionFlag;   // Return and supply connection flag
        bool VAVTermDamper;   // True if this component is a damper for a VAV terminal
        int LinkageViewFactorObjectNum;
        int AirLoopNum; // Airloop number

        // Default Constructor
        AirflowNetworkLinkageProp()
            : AirflowNetworkLinkage(), ZoneNum(0), DetOpenNum(0), ConnectionFlag(0), VAVTermDamper(false), LinkageViewFactorObjectNum(0),
              AirLoopNum(0)
        {
        }
    };

    struct PressureControllerProp
    {
        // Members
        std::string Name;              // Provide a unique object name
        std::string ZoneName;          // Name of the zone that is being controlled
        int ZoneNum;                   // Zone number
        int AFNNodeNum;                // AFN node number
        std::string ControlObjectType; // The control type to be used for pressure control
        std::string ControlObjectName; // Corresponding control type name
        int ControlTypeSet;            // Control type set to be used for pressure control
        int AvailSchedPtr;             // Availability schedule pointer
        int PresSetpointSchedPtr;      // Pressure setpoint schedule pointer
        int AirLoopNum;                // Air loop number
        int OANodeNum;                 // outdoor air node number
        bool bypass;                   // Can not perform pressure control as true
        Real64 PresCtrlMassRate;

        // Default Constructor
        PressureControllerProp()
            : ZoneNum(0), AFNNodeNum(0), ControlTypeSet(0), AvailSchedPtr(0), PresSetpointSchedPtr(0), AirLoopNum(0), OANodeNum(0), bypass(false),
              PresCtrlMassRate(0.0)
        {
        }
    };

    struct OutdoorAirFan : public AirflowElement // OA fan component
    {
        // Members
        std::string Name; // Name of exhaust fan component
        int SchedPtr;     // Schedule pointer
        Real64 FlowCoef;  // Air Mass Flow Coefficient [kg/s at 1Pa]
        Real64 FlowExpo;  // Air Mass Flow exponent [dimensionless]
        Real64 StandardT; // Standard temperature for crack data [C]
        Real64 StandardP; // Standard barometric pressure for crack data [Pa]
        Real64 StandardW; // Standard humidity ratio for crack data [kg/kg]
        int InletNode;    // Inlet node number
        int OutletNode;   // Outlet node number
        int OAMixerNum;   // OA Mixer number
        int PressCtrlNum; // Pressure control number

        // Default Constructor
        OutdoorAirFan()
            : SchedPtr(0), FlowCoef(0.0), FlowExpo(0.0), StandardT(0.0), StandardP(0.0), StandardW(0.0), InletNode(0), OutletNode(0), OAMixerNum(0),
              PressCtrlNum(0)
        {
        }

        virtual ~OutdoorAirFan()
        {
        }

        virtual int calculate(bool const LFLAG,           // Initialization flag.If = 1, use laminar relationship
                              const Real64 PDROP,         // Total pressure drop across a component (P1 - P2) [Pa]
                              int const i,                // Linkage number
                              const AirProperties &propN, // Node 1 properties
                              const AirProperties &propM, // Node 2 properties
                              std::array<Real64, 2> &F,   // Airflow through the component [kg/s]
                              std::array<Real64, 2> &DF   // Partial derivative:  DF/DP
        );
    };

    struct ReliefFlow : public OutdoorAirFan // OA fan component
    {

        // Default Constructor
        ReliefFlow() : OutdoorAirFan()
        {
        }

        virtual int calculate(bool const LFLAG,           // Initialization flag.If = 1, use laminar relationship
                              const Real64 PDROP,         // Total pressure drop across a component (P1 - P2) [Pa]
                              int const i,                // Linkage number
                              const AirProperties &propN, // Node 1 properties
                              const AirProperties &propM, // Node 2 properties
                              std::array<Real64, 2> &F,   // Airflow through the component [kg/s]
                              std::array<Real64, 2> &DF   // Partial derivative:  DF/DP
        );
    };

    struct AirflowNetworkNodeSimuData // Node variable for simulation
    {
        // Members
        Real64 TZ;       // Temperature [C]
        Real64 WZ;       // Humidity ratio [kg/kg]
        Real64 PZ;       // Pressure [Pa]
        Real64 CO2Z;     // CO2 [ppm]
        Real64 GCZ;      // Generic contaminant [ppm]
        Real64 TZlast;   // Temperature [C] at previous time step
        Real64 WZlast;   // Humidity ratio [kg/kg] at previous time step
        Real64 CO2Zlast; // CO2 [ppm] at previous time step
        Real64 GCZlast;  // Generic contaminant [ppm] at previous time step

        // Default Constructor
        AirflowNetworkNodeSimuData() : TZ(0.0), WZ(0.0), PZ(0.0), CO2Z(0.0), GCZ(0.0), TZlast(0.0), WZlast(0.0), CO2Zlast(0.0), GCZlast(0.0)
        {
        }
    };

    struct AirflowNetworkLinkSimuData
    {
        // Members
        Real64 FLOW;     // Mass flow rate [kg/s]
        Real64 FLOW2;    // Mass flow rate [kg/s] for two way flow
        Real64 DP;       // Pressure difference across a component
        Real64 VolFLOW;  // Mass flow rate [m3/s]
        Real64 VolFLOW2; // Mass flow rate [m3/s] for two way flow
        Real64 DP1;

        // Default Constructor
        AirflowNetworkLinkSimuData() : FLOW(0.0), FLOW2(0.0), DP(0.0), VolFLOW(0.0), VolFLOW2(0.0), DP1(0.0)
        {
        }
    };

    struct AirflowNetworkLinkReportData
    {
        // Members
        Real64 FLOW;        // Mass flow rate [kg/s]
        Real64 FLOW2;       // Mass flow rate [kg/s] for two way flow
        Real64 VolFLOW;     // Mass flow rate [m^3/s]
        Real64 VolFLOW2;    // Mass flow rate [m^3/s] for two way flow
        Real64 FLOWOFF;     // Mass flow rate during OFF cycle [kg/s]
        Real64 FLOW2OFF;    // Mass flow rate during OFF cycle [kg/s] for two way flow
        Real64 VolFLOWOFF;  // Mass flow rate during OFF cycle [m^3/s]
        Real64 VolFLOW2OFF; // Mass flow rate during OFF cycle [m^3/s] for two way flow
        Real64 DP;          // Average Pressure difference across a component
        Real64 DPON;        // Pressure difference across a component with fan on
        Real64 DPOFF;       // Pressure difference across a component with fan off

        // Default Constructor
        AirflowNetworkLinkReportData()
            : FLOW(0.0), FLOW2(0.0), VolFLOW(0.0), VolFLOW2(0.0), FLOWOFF(0.0), FLOW2OFF(0.0), VolFLOWOFF(0.0), VolFLOW2OFF(0.0), DP(0.0), DPON(0.0),
              DPOFF(0.0)
        {
        }
    };

    struct AirflowNetworkNodeReportData // Node variable for simulation
    {
        // Members
        Real64 PZ;    // Average Pressure [Pa]
        Real64 PZON;  // Pressure with fan on [Pa]
        Real64 PZOFF; // Pressure with fan off [Pa]

        // Default Constructor
        AirflowNetworkNodeReportData() : PZ(0.0), PZON(0.0), PZOFF(0.0)
        {
        }
    };

    struct AirflowNetworkExchangeProp
    {
        // Members
        Real64 MultiZoneSen;
        Real64 MultiZoneLat;
        Real64 LeakSen;
        Real64 LeakLat;
        Real64 CondSen;
        Real64 DiffLat;
        Real64 RadGain;
        Real64 TotalSen;
        Real64 TotalLat;
        Real64 SumMCp;
        Real64 SumMCpT;
        Real64 SumMHr;
        Real64 SumMHrW;
        Real64 SumMMCp;
        Real64 SumMMCpT;
        Real64 SumMMHr;
        Real64 SumMMHrW;
        Real64 SumMHrCO;
        Real64 SumMMHrCO;
        Real64 TotalCO2;
        Real64 SumMHrGC;
        Real64 SumMMHrGC;
        Real64 TotalGC;

        // Default Constructor
        AirflowNetworkExchangeProp()
            : MultiZoneSen(0.0), MultiZoneLat(0.0), LeakSen(0.0), LeakLat(0.0), CondSen(0.0), DiffLat(0.0), RadGain(0.0), TotalSen(0.0),
              TotalLat(0.0), SumMCp(0.0), SumMCpT(0.0), SumMHr(0.0), SumMHrW(0.0), SumMMCp(0.0), SumMMCpT(0.0), SumMMHr(0.0), SumMMHrW(0.0),
              SumMHrCO(0.0), SumMMHrCO(0.0), TotalCO2(0.0), SumMHrGC(0.0), SumMMHrGC(0.0), TotalGC(0.0)
        {
        }
    };

    struct AiflowNetworkReportProp
    {
        // Members
        Real64 MultiZoneInfiSenGainW;
        Real64 MultiZoneInfiSenGainJ;
        Real64 MultiZoneInfiSenLossW;
        Real64 MultiZoneInfiSenLossJ;
        Real64 MultiZoneMixSenGainW;
        Real64 MultiZoneMixSenGainJ;
        Real64 MultiZoneMixSenLossW;
        Real64 MultiZoneMixSenLossJ;
        Real64 MultiZoneInfiLatGainW;
        Real64 MultiZoneInfiLatGainJ;
        Real64 MultiZoneInfiLatLossW;
        Real64 MultiZoneInfiLatLossJ;
        Real64 MultiZoneMixLatGainW;
        Real64 MultiZoneMixLatGainJ;
        Real64 MultiZoneMixLatLossW;
        Real64 MultiZoneMixLatLossJ;
        Real64 LeakSenGainW;
        Real64 LeakSenGainJ;
        Real64 LeakSenLossW;
        Real64 LeakSenLossJ;
        Real64 LeakLatGainW;
        Real64 LeakLatGainJ;
        Real64 LeakLatLossW;
        Real64 LeakLatLossJ;
        Real64 CondSenGainW;
        Real64 CondSenGainJ;
        Real64 CondSenLossW;
        Real64 CondSenLossJ;
        Real64 DiffLatGainW;
        Real64 DiffLatGainJ;
        Real64 DiffLatLossW;
        Real64 DiffLatLossJ;
        Real64 RadGainW;
        Real64 RadGainJ;
        Real64 RadLossW;
        Real64 RadLossJ;
        Real64 TotalSenGainW;
        Real64 TotalSenGainJ;
        Real64 TotalSenLossW;
        Real64 TotalSenLossJ;
        Real64 TotalLatGainW;
        Real64 TotalLatGainJ;
        Real64 TotalLatLossW;
        Real64 TotalLatLossJ;
        bool OnOffFlag;

        // Default Constructor
        AiflowNetworkReportProp()
            : MultiZoneInfiSenGainW(0.0), MultiZoneInfiSenGainJ(0.0), MultiZoneInfiSenLossW(0.0), MultiZoneInfiSenLossJ(0.0),
              MultiZoneMixSenGainW(0.0), MultiZoneMixSenGainJ(0.0), MultiZoneMixSenLossW(0.0), MultiZoneMixSenLossJ(0.0), MultiZoneInfiLatGainW(0.0),
              MultiZoneInfiLatGainJ(0.0), MultiZoneInfiLatLossW(0.0), MultiZoneInfiLatLossJ(0.0), MultiZoneMixLatGainW(0.0),
              MultiZoneMixLatGainJ(0.0), MultiZoneMixLatLossW(0.0), MultiZoneMixLatLossJ(0.0), LeakSenGainW(0.0), LeakSenGainJ(0.0),
              LeakSenLossW(0.0), LeakSenLossJ(0.0), LeakLatGainW(0.0), LeakLatGainJ(0.0), LeakLatLossW(0.0), LeakLatLossJ(0.0), CondSenGainW(0.0),
              CondSenGainJ(0.0), CondSenLossW(0.0), CondSenLossJ(0.0), DiffLatGainW(0.0), DiffLatGainJ(0.0), DiffLatLossW(0.0), DiffLatLossJ(0.0),
              RadGainW(0.0), RadGainJ(0.0), RadLossW(0.0), RadLossJ(0.0), TotalSenGainW(0.0), TotalSenGainJ(0.0), TotalSenLossW(0.0),
              TotalSenLossJ(0.0), TotalLatGainW(0.0), TotalLatGainJ(0.0), TotalLatLossW(0.0), TotalLatLossJ(0.0), OnOffFlag(false)
        {
        }
    };

    struct LinkageSurfaceProp
    {
        // Members
        std::string SurfaceName;
        int SurfaceNum;                 // Name of surface referenced by view factor
        Real64 ViewFactor;              // View factor
        Real64 SurfaceResistanceFactor; // Total radiation heat transfer resistance factor
        Real64 SurfaceRadLoad;          // Duct radiation load from surface [W]

        // Default constructor
        LinkageSurfaceProp() : SurfaceNum(0), ViewFactor(0.0), SurfaceResistanceFactor(0.0), SurfaceRadLoad(0.0)
        {
        }
    };

    struct AirflowNetworkLinkageViewFactorProp
    {
        // Members
        std::string LinkageName;
        Real64 DuctExposureFraction;
        Real64 DuctEmittance;
        Array1D<LinkageSurfaceProp> LinkageSurfaceData;
        int ObjectNum;
        Real64 QRad;
        Real64 QConv;

        AirflowNetworkLinkageViewFactorProp() : DuctExposureFraction(0.0), DuctEmittance(0.0), ObjectNum(0), QRad(0.0), QConv(0.0)
        {
        }
    };

    // Object Data
    extern Array1D<AirflowNetworkNodeSimuData> AirflowNetworkNodeSimu;
    extern Array1D<AirflowNetworkLinkSimuData> AirflowNetworkLinkSimu;
    extern Array1D<AirflowNetworkExchangeProp> AirflowNetworkExchangeData;
    extern Array1D<AirflowNetworkExchangeProp> AirflowNetworkMultiExchangeData;
    extern Array1D<AirflowNetworkLinkReportData> AirflowNetworkLinkReport;
    extern Array1D<AirflowNetworkNodeReportData> AirflowNetworkNodeReport;
    extern Array1D<AirflowNetworkLinkReportData> AirflowNetworkLinkReport1;
    extern AirflowNetworkSimuProp
        AirflowNetworkSimu; // unique object name | AirflowNetwork control | Wind pressure coefficient input control | Integer equivalent for WPCCntr
                            // field | CP Array name at WPCCntr = "INPUT" | Building type | Height Selection | Maximum number of iteration |
                            // Initialization flag | Relative airflow convergence | Absolute airflow convergence | Convergence acceleration limit |
                            // Maximum pressure change in an element [Pa] | Azimuth Angle of Long Axis of Building | Ratio of Building Width Along
                            // Short Axis to Width Along Long Axis | Number of wind directions | Minimum pressure difference | Exterior large opening
                            // error count during HVAC system operation | Exterior large opening error index during HVAC system operation | Large
                            // opening error count at Open factor > 1.0 | Large opening error error index at Open factor > 1.0 | Initialization flag
                            // type
    extern Array1D<AirflowNetworkNodeProp> AirflowNetworkNodeData;
    extern Array1D<AirflowNetworkCompProp> AirflowNetworkCompData;
    extern Array1D<AirflowNetworkLinkageProp> AirflowNetworkLinkageData;
    extern Array1D<MultizoneZoneProp> MultizoneZoneData;
    extern Array1D<MultizoneSurfaceProp> MultizoneSurfaceData;
    extern Array1D<DetailedOpening> MultizoneCompDetOpeningData;
    extern Array1D<SimpleOpening> MultizoneCompSimpleOpeningData;
    extern Array1D<HorizontalOpening> MultizoneCompHorOpeningData;
    // extern Array1D<ReferenceConditions> MultizoneSurfaceStdConditionsCrackData;
    extern Array1D<SurfaceCrack> MultizoneSurfaceCrackData;
    extern Array1D<EffectiveLeakageArea> MultizoneSurfaceELAData;
    extern Array1D<MultizoneExternalNodeProp> MultizoneExternalNodeData;
    extern Array1D<DeltaCpProp> DeltaCp;
    extern Array1D<DeltaCpProp> EPDeltaCP;
    extern Array1D<ZoneExhaustFan> MultizoneCompExhaustFanData;
    extern Array1D<IntraZoneNodeProp> IntraZoneNodeData;       // Intra zone data set
    extern Array1D<IntraZoneLinkageProp> IntraZoneLinkageData; // Intra zone linkage adat set
    extern Array1D<DisSysNodeProp> DisSysNodeData;
    extern Array1D<DuctLeak> DisSysCompLeakData;
    extern Array1D<EffectiveLeakageRatio> DisSysCompELRData;
    extern Array1D<Duct> DisSysCompDuctData;
    extern Array1D<Damper> DisSysCompDamperData;
    extern Array1D<ConstantVolumeFan> DisSysCompCVFData;
    extern Array1D<DetailedFan> DisSysCompDetFanData;
    extern Array1D<DisSysCompCoilProp> DisSysCompCoilData;
    extern Array1D<DisSysCompHXProp> DisSysCompHXData;
    extern Array1D<DisSysCompTermUnitProp> DisSysCompTermUnitData;
    extern Array1D<ConstantPressureDrop> DisSysCompCPDData;
    extern Array1D<AiflowNetworkReportProp> AirflowNetworkReportData;
    extern Array1D<PressureControllerProp> PressureControllerData;
    extern Array1D<OutdoorAirFan> DisSysCompOutdoorAirData;
    extern Array1D<ReliefFlow> DisSysCompReliefAirData;
    extern Array1D<AirflowNetworkLinkageViewFactorProp> AirflowNetworkLinkageViewFactorData;

    void clear_state();

} // namespace AirflowNetwork

} // namespace EnergyPlus

#endif
