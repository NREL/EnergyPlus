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

// C++ Headers
#include <cassert>
#include <cmath>

// ObjexxFCL Headers
#include <ObjexxFCL/Array.functions.hh>
#include <ObjexxFCL/Fmath.hh>

// EnergyPlus Headers
#include <EnergyPlus/BranchNodeConnections.hh>
#include <EnergyPlus/Construction.hh>
#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/DataBranchAirLoopPlant.hh>
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataHVACGlobals.hh>
#include <EnergyPlus/DataHeatBalFanSys.hh>
#include <EnergyPlus/DataHeatBalSurface.hh>
#include <EnergyPlus/DataHeatBalance.hh>
#include <EnergyPlus/DataLoopNode.hh>
#include <EnergyPlus/DataSizing.hh>
#include <EnergyPlus/DataSurfaceLists.hh>
#include <EnergyPlus/DataSurfaces.hh>
#include <EnergyPlus/DataZoneEquipment.hh>
#include <EnergyPlus/EMSManager.hh>
#include <EnergyPlus/FluidProperties.hh>
#include <EnergyPlus/General.hh>
#include <EnergyPlus/GeneralRoutines.hh>
#include <EnergyPlus/GlobalNames.hh>
#include <EnergyPlus/HeatBalanceSurfaceManager.hh>
#include <EnergyPlus/InputProcessing/InputProcessor.hh>
#include <EnergyPlus/LowTempRadiantSystem.hh>
#include <EnergyPlus/NodeInputManager.hh>
#include <EnergyPlus/OutputProcessor.hh>
#include <EnergyPlus/Plant/DataPlant.hh>
#include <EnergyPlus/PlantUtilities.hh>
#include <EnergyPlus/Psychrometrics.hh>
#include <EnergyPlus/ReportSizingManager.hh>
#include <EnergyPlus/ScheduleManager.hh>
#include <EnergyPlus/UtilityRoutines.hh>
#include <EnergyPlus/WeatherManager.hh>
#include <EnergyPlus/ZoneTempPredictorCorrector.hh>

namespace EnergyPlus {

namespace LowTempRadiantSystem {

    // Module containing the routines dealing with the low temperature radiant systems

    // MODULE INFORMATION:
    //       AUTHOR         Rick Strand
    //       DATE WRITTEN   November 2000
    //       MODIFIED       Rick Strand March 2001 (additional controls, etc.)
    //                      Rick Strand July 2003 (added constant flow hydronic system)
    //                      B. Griffith Sept 2010, plant upgrades, generalize fluid properties
    //                      Rick Strand August 2011 (improved condensation handling)

    // PURPOSE OF THIS MODULE:
    // The purpose of this module is to simulate low temperature radiant systems.
    // It is the intention of this module to cover all types of low temperature
    // radiant systems: wall, ceiling, floor, heating, cooling, panels, etc.

    // METHODOLOGY EMPLOYED:
    // Based on work done in IBLAST, this model has been revised for the structure
    // of EnergyPlus.  It is still based on the QTF formulation of heat transfer
    // through building elements with embedded heat sources/sinks.  Note that due
    // to the fact that a radiant system is both a building heat transfer element
    // and a controllable system that some iteration between the system and the
    // surface heat balance routine is necessary.
    // REFERENCES:
    // IBLAST-QTF research program, completed in January 1995 (unreleased)
    // Strand, R.K. 1995. "Heat Source Transfer Functions and Their Application to
    //   Low Temperature Radiant Heating Systems", Ph.D. dissertation, University
    //   of Illinois at Urbana-Champaign, Department of Mechanical and Industrial
    //   Engineering.
    // Seem, J.E. 1986. "Heat Transfer in Buildings", Ph.D. dissertation, University
    //   of Wisconsin-Madison.

    // OTHER NOTES: This module contains three different types of radiant system
    // models: (a) variable flow hydronic heating/cooling radiant system;
    // (b) constant flow, variable controlled temperature heating/cooling radiant
    // system; (c) electric resistance heating radiant system.  Systems (a) and
    // (b) are hydronic systems--one which varies hydronic flow as the key control
    // paramter (a) and one which varies the inlet hydronic temperature while
    // keeping the flow rate through the radiant system constant (b).  In system
    // (b), the injection rate from the main water loop is varied to obtain the
    // proper inlet temperature.

    // USE STATEMENTS:
    // Use statements for data only modules
    // Using/Aliasing
    using DataGlobals::BeginTimeStepFlag;
    using DataGlobals::DisplayExtraWarnings;
    using DataGlobals::SysSizingCalc;
    using DataGlobals::WarmupFlag;
    using DataHeatBalance::Air;
    using DataHeatBalance::RegularMaterial;
    using DataHeatBalance::TotConstructs;
    using DataHeatBalance::TotMaterials;
    using DataHeatBalFanSys::QRadSysSource; // Heat source/sink value & temperature for CondFD algo.
    using DataHeatBalFanSys::TCondFDSourceNode;
    using DataHVACGlobals::SmallLoad;
    using DataSurfaces::HeatTransferModel_CTF;
    using DataSurfaces::Surface;
    using DataSurfaces::TotSurfaces;
    using Psychrometrics::PsyTdpFnWPb;

    // Data
    // MODULE PARAMETER DEFINITIONS:
    // System types:
    int const HydronicSystem(1);     // Variable flow hydronic radiant system
    int const ConstantFlowSystem(2); // Constant flow, variable (controlled) temperature radiant system
    int const ElectricSystem(3);     // Electric resistance radiant heating system
    std::string const cHydronicSystem("ZoneHVAC:LowTemperatureRadiant:VariableFlow");
    std::string const cConstantFlowSystem("ZoneHVAC:LowTemperatureRadiant:ConstantFlow");
    std::string const cElectricSystem("ZoneHVAC:LowTemperatureRadiant:Electric");
    // Operating modes:
    int const NotOperating(0); // Parameter for use with OperatingMode variable, set for heating
    int const HeatingMode(1);  // Parameter for use with OperatingMode variable, set for heating
    int const CoolingMode(-1);  // Parameter for use with OperatingMode variable, set for cooling
    // Condensation control types:
    int const CondCtrlNone(0);      // Condensation control--none, so system never shuts down
    int const CondCtrlSimpleOff(1); // Condensation control--simple off, system shuts off when condensation predicted
    int const CondCtrlVariedOff(2); // Condensation control--variable off, system modulates to keep running if possible
    // Number of Circuits per Surface Calculation Method
    int const OneCircuit(1);          // there is 1 circuit per surface
    int const CalculateFromLength(2); // The number of circuits is TubeLength*SurfaceFrac / CircuitLength
    std::string const OnePerSurf("OnePerSurface");
    std::string const CalcFromLength("CalculateFromCircuitLength");
    // Limit temperatures to indicate that a system cannot heat or cannot cool
    Real64 LowTempHeating(-200.0); // Used to indicate that a user does not have a heating control temperature
    Real64 HighTempCooling(200.0); // Used to indicate that a user does not have a cooling control temperature

    static std::string const fluidNameWater("WATER");
    static std::string const BlankString;

    // DERIVED TYPE DEFINITIONS:

    // MODULE VARIABLE DECLARATIONS:
    // Standard, run-of-the-mill variables...
    bool GetInputFlag = true;
    int NumOfHydrLowTempRadSys(0);        // Number of hydronic low tempererature radiant systems
    int NumOfCFloLowTempRadSys(0);        // Number of constant flow (hydronic) low tempererature radiant systems
    int NumOfElecLowTempRadSys(0);        // Number of electric low tempererature radiant systems
    int CFloCondIterNum(0);               // Number of iterations for a constant flow radiant system--controls variable cond sys ctrl
    int TotalNumOfRadSystems(0);          // Total number of low temperature radiant systems
    int MaxCloNumOfSurfaces(0);           // Used to set allocate size in CalcClo routine
    bool VarOffCond(false);               // Set to true when in cooling for constant flow system + variable off condensation predicted
    bool FirstTimeInit(true);             // Set to true for first pass through init routine then set to false
    bool anyRadiantSystemUsingRunningMeanAverage(false);    // Set to true when there is at least one constant flow radiant system that uses the running mean average
    Real64 LoopReqTemp(0.0);              // Temperature required at the inlet of the pump (from the loop) to meet control logic
    Array1D<Real64> QRadSysSrcAvg;        // Average source over the time step for a particular radiant surface
    Array1D<Real64> ZeroSourceSumHATsurf; // Equal to SumHATsurf for all the walls in a zone with no source
    // Record keeping variables used to calculate QRadSysSrcAvg locally
    Array1D<Real64> LastQRadSysSrc;     // Need to keep the last value in case we are still iterating
    Array1D<Real64> LastSysTimeElapsed; // Need to keep the last value in case we are still iterating
    Array1D<Real64> LastTimeStepSys;    // Need to keep the last value in case we are still iterating
    // Autosizing variables
    Array1D_bool MySizeFlagHydr;
    Array1D_bool MySizeFlagCFlo;
    Array1D_bool MySizeFlagElec;
    Array1D_bool CheckEquipName;

    // Object Data
    Array1D<VariableFlowRadiantSystemData> HydrRadSys;
    Array1D<ConstantFlowRadiantSystemData> CFloRadSys;
    Array1D<ElectricRadiantSystemData> ElecRadSys;
    Array1D<RadSysTypeData> RadSysTypes;
    std::unordered_map<std::string, std::string> LowTempRadUniqueNames;
    Array1D<ElecRadSysNumericFieldData> ElecRadSysNumericFields;
    Array1D<HydronicRadiantSysNumericFieldData> HydronicRadiantSysNumericFields;

    bool FirstTimeFlag = true; // for setting size of Ckj, Cmj, WaterTempOut arrays
    bool MyEnvrnFlagGeneral = true;
    bool ZoneEquipmentListChecked = false; // True after the Zone Equipment List has been checked for items
    bool MyOneTimeFlag = true; // Initialization flag
    bool warnTooLow = false;
    bool warnTooHigh = false;

    void clear_state()
    {
        LowTempHeating = -200.0;
        HighTempCooling = 200.0;
        NumOfHydrLowTempRadSys = 0;
        NumOfCFloLowTempRadSys = 0;
        NumOfElecLowTempRadSys = 0;
        CFloCondIterNum = 0;
        TotalNumOfRadSystems = 0;
        MaxCloNumOfSurfaces = 0;
        VarOffCond = false;
        FirstTimeInit = true;
        anyRadiantSystemUsingRunningMeanAverage = false;
        LoopReqTemp = 0.0;
        QRadSysSrcAvg.deallocate();
        ZeroSourceSumHATsurf.deallocate();
        LastQRadSysSrc.deallocate();
        LastSysTimeElapsed.deallocate();
        LastTimeStepSys.deallocate();
        MySizeFlagHydr.deallocate();
        MySizeFlagCFlo.deallocate();
        MySizeFlagElec.deallocate();
        CheckEquipName.deallocate();
        HydrRadSys.deallocate();
        CFloRadSys.deallocate();
        ElecRadSys.deallocate();
        RadSysTypes.deallocate();
        ElecRadSysNumericFields.deallocate();
        HydronicRadiantSysNumericFields.deallocate();
        LowTempRadUniqueNames.clear();
        GetInputFlag = true;
        FirstTimeFlag = true;
        MyEnvrnFlagGeneral = true;
        ZoneEquipmentListChecked = false;
        MyOneTimeFlag = true;
        warnTooLow = false;
        warnTooHigh = false;
    }

    void SimLowTempRadiantSystem(EnergyPlusData &state, std::string const &CompName,   // name of the low temperature radiant system
                                 bool const FirstHVACIteration, // TRUE if 1st HVAC simulation of system timestep
                                 Real64 &LoadMet,               // load met by the radiant system, in Watts
                                 int &CompIndex)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Rick Strand
        //       DATE WRITTEN   November 2000

        // Using/Aliasing
        using General::TrimSigDigits;

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int RadSysNum;  // Radiant system number/index in local derived types
        int SystemType; // Type of radiant system: hydronic, constant flow, or electric
        bool InitErrorFound(false);

        // FLOW:
        if (GetInputFlag) {
            GetLowTempRadiantSystem();
            GetInputFlag = false;
        }

        // Find the correct Low Temp Radiant System
        if (CompIndex == 0) {
            RadSysNum = UtilityRoutines::FindItemInList(CompName, RadSysTypes);
            if (RadSysNum == 0) {
                ShowFatalError("SimLowTempRadiantSystem: Unit not found=" + CompName);
            }
            CompIndex = RadSysNum;
            SystemType = RadSysTypes(RadSysNum).SystemType;
            {
                auto const SELECT_CASE_var(SystemType);
                if (SELECT_CASE_var == HydronicSystem) {
                    RadSysTypes(RadSysNum).CompIndex = UtilityRoutines::FindItemInList(CompName, HydrRadSys);
                } else if (SELECT_CASE_var == ConstantFlowSystem) {
                    RadSysTypes(RadSysNum).CompIndex = UtilityRoutines::FindItemInList(CompName, CFloRadSys);
                } else if (SELECT_CASE_var == ElectricSystem) {
                    RadSysTypes(RadSysNum).CompIndex = UtilityRoutines::FindItemInList(CompName, ElecRadSys);
                }
            }
        } else {
            RadSysNum = CompIndex;
            SystemType = RadSysTypes(RadSysNum).SystemType;
            if (RadSysNum > TotalNumOfRadSystems || RadSysNum < 1) {
                ShowFatalError("SimLowTempRadiantSystem:  Invalid CompIndex passed=" + TrimSigDigits(RadSysNum) +
                               ", Number of Units=" + TrimSigDigits(TotalNumOfRadSystems) + ", Entered Unit name=" + CompName);
            }
            if (CheckEquipName(RadSysNum)) {
                if (CompName != RadSysTypes(RadSysNum).Name) {
                    ShowFatalError("SimLowTempRadiantSystem: Invalid CompIndex passed=" + TrimSigDigits(RadSysNum) + ", Unit name=" + CompName +
                                   ", stored Unit Name for that index=" + RadSysTypes(RadSysNum).Name);
                }
                CheckEquipName(RadSysNum) = false;
            }
        }

        InitLowTempRadiantSystem(state, FirstHVACIteration, RadSysTypes(RadSysNum).CompIndex, SystemType, InitErrorFound);
        if (InitErrorFound) {
            ShowFatalError("InitLowTempRadiantSystem: Preceding error is not allowed to proceed with the simulation.  Correct this input problem.");
        }

        // Simulate, update, and report based on the type of radiant system
        {
            RadiantSystemBaseData * baseSystem;
            if (SystemType == HydronicSystem) {
                baseSystem = &HydrRadSys(RadSysTypes(RadSysNum).CompIndex);
            } else if (SystemType == ConstantFlowSystem) {
                baseSystem = &CFloRadSys(RadSysTypes(RadSysNum).CompIndex);
            } else if (SystemType == ElectricSystem) {
                baseSystem = &ElecRadSys(RadSysTypes(RadSysNum).CompIndex);
            } else {
                ShowFatalError("SimLowTempRadiantSystem: Illegal system type for system " + CompName);
            }

            if ((SystemType == HydronicSystem) || (SystemType == ConstantFlowSystem) || (SystemType == ElectricSystem) ) {
                baseSystem->calculateLowTemperatureRadiantSystem(state, LoadMet);
                baseSystem->updateLowTemperatureRadiantSystemSurfaces();
                baseSystem->updateLowTemperatureRadiantSystem(); // Nothing to update for electric systems
                baseSystem->reportLowTemperatureRadiantSystem();
            }
        }

    }

    void GetLowTempRadiantSystem()
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Rick Strand
        //       DATE WRITTEN   November 2000
        //       MODIFIED       August 2003 (added constant flow system, made input extensible)

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine reads the input for low temperature radiant systems
        // from the user input file.  This will contain all of the information
        // needed to simulate a low temperature radiant system.

        // Using/Aliasing
        using BranchNodeConnections::TestCompSet;
        using DataGlobals::AnyEnergyManagementSystemInModel;
        using DataGlobals::ScheduleAlwaysOn;
        using DataHeatBalance::Zone;
        using DataSizing::AutoSize;
        using DataSizing::CapacityPerFloorArea;
        using DataSizing::CoolingDesignCapacity;
        using DataSizing::FractionOfAutosizedCoolingCapacity;
        using DataSizing::FractionOfAutosizedHeatingCapacity;
        using DataSizing::HeatingDesignCapacity;
        using FluidProperties::FindGlycol;
        using General::TrimSigDigits;
        using NodeInputManager::GetOnlySingleNode;
        using ScheduleManager::GetScheduleIndex;
        using namespace DataLoopNode;
        using namespace DataSurfaceLists;

        // SUBROUTINE PARAMETER DEFINITIONS:
        static std::string const RoutineName("GetLowTempRadiantSystem: "); // include trailing blank space
        static std::string const Off("Off");
        static std::string const SimpleOff("SimpleOff");
        static std::string const VariableOff("VariableOff");
        int const iHeatCAPMAlphaNum(5);             // get input index to Low Temperature Radiant system heating capacity sizing method
        int const iHeatDesignCapacityNumericNum(1); // get input index to Low Temperature Radiant system electric heating capacity
        int const iHeatCapacityPerFloorAreaNumericNum(
            2); // get input index to Low Temperature Radiant system electric heating capacity per floor area sizing
        int const iHeatFracOfAutosizedCapacityNumericNum(
            3); //  get input index to Low Temperature Radiant system electric heating capacity sizing as fraction of autozized heating capacity

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        std::string CurrentModuleObject;       // for ease in getting objects
        Array1D_string Alphas;                 // Alpha items for object
        Array1D_string cAlphaFields;           // Alpha field names
        Array1D_string cNumericFields;         // Numeric field names
        Array1D_bool AssignedAsRadiantSurface; // Set to true when a surface is part of a radiant system
        int CheckSurfNum;                      // Surface number to check to see if it has already been used by a radiant system
        bool ErrorsFound(false);        // Set to true if errors in input, fatal at end of routine
        int GlycolIndex;                       // Index of 'Water' in glycol data structure
        int IOStatus;                          // Used in GetObjectItem
        int Item;                              // Item to be "gotten"
        int MaxAlphas;                         // Maximum number of alphas for these input keywords
        int MaxNumbers;                        // Maximum number of numbers for these input keywords
        Array1D<Real64> Numbers; // Numeric items for object
        int NumAlphas;           // Number of Alphas for each GetObjectItem call
        int NumArgs;             // Unused variable that is part of a subroutine call
        int NumNumbers;          // Number of Numbers for each GetObjectItem call
        int SurfListNum; // Index within the SurfList derived type for a surface list name
        int SurfNum;     // DO loop counter for surfaces
        int BaseNum;                 // Temporary number for creating RadiantSystemTypes structure
        Array1D_bool lAlphaBlanks;   // Logical array, alpha field input BLANK = .TRUE.
        Array1D_bool lNumericBlanks; // Logical array, numeric field input BLANK = .TRUE.

        MaxAlphas = 0;
        MaxNumbers = 0;

        inputProcessor->getObjectDefMaxArgs("ZoneHVAC:LowTemperatureRadiant:VariableFlow", NumArgs, NumAlphas, NumNumbers);
        MaxAlphas = max(MaxAlphas, NumAlphas);
        MaxNumbers = max(MaxNumbers, NumNumbers);

        inputProcessor->getObjectDefMaxArgs("ZoneHVAC:LowTemperatureRadiant:ConstantFlow", NumArgs, NumAlphas, NumNumbers);
        MaxAlphas = max(MaxAlphas, NumAlphas);
        MaxNumbers = max(MaxNumbers, NumNumbers);

        inputProcessor->getObjectDefMaxArgs("ZoneHVAC:LowTemperatureRadiant:Electric", NumArgs, NumAlphas, NumNumbers);
        MaxAlphas = max(MaxAlphas, NumAlphas);
        MaxNumbers = max(MaxNumbers, NumNumbers);

        Alphas.allocate(MaxAlphas);
        Numbers.dimension(MaxNumbers, 0.0);
        cAlphaFields.allocate(MaxAlphas);
        cNumericFields.allocate(MaxNumbers);
        lAlphaBlanks.dimension(MaxAlphas, true);
        lNumericBlanks.dimension(MaxNumbers, true);

        NumOfHydrLowTempRadSys = inputProcessor->getNumObjectsFound("ZoneHVAC:LowTemperatureRadiant:VariableFlow");
        NumOfCFloLowTempRadSys = inputProcessor->getNumObjectsFound("ZoneHVAC:LowTemperatureRadiant:ConstantFlow");
        NumOfElecLowTempRadSys = inputProcessor->getNumObjectsFound("ZoneHVAC:LowTemperatureRadiant:Electric");

        TotalNumOfRadSystems = NumOfHydrLowTempRadSys + NumOfElecLowTempRadSys + NumOfCFloLowTempRadSys;
        RadSysTypes.allocate(TotalNumOfRadSystems);
        LowTempRadUniqueNames.reserve(static_cast<unsigned>(TotalNumOfRadSystems));
        CheckEquipName.dimension(TotalNumOfRadSystems, true);

        HydrRadSys.allocate(NumOfHydrLowTempRadSys);
        if (NumOfHydrLowTempRadSys > 0) {
            GlycolIndex = FindGlycol(fluidNameWater);
            for (auto &e : HydrRadSys)
                e.GlycolIndex = GlycolIndex;
            if (GlycolIndex == 0) {
                ShowSevereError("Hydronic radiant systems: no water property data found in input");
                ErrorsFound = true;
            }
        } else {
            for (auto &e : HydrRadSys)
                e.GlycolIndex = 0;
        }

        CFloRadSys.allocate(NumOfCFloLowTempRadSys);
        if (NumOfCFloLowTempRadSys > 0) {
            GlycolIndex = FindGlycol(fluidNameWater);
            for (auto &e : CFloRadSys)
                e.GlycolIndex = GlycolIndex;
            if (GlycolIndex == 0) {
                ShowSevereError("Constant flow radiant systems: no water property data found in input");
                ErrorsFound = true;
            }
        } else {
            for (auto &e : CFloRadSys)
                e.GlycolIndex = 0;
        }

        ElecRadSys.allocate(NumOfElecLowTempRadSys);
        ElecRadSysNumericFields.allocate(NumOfElecLowTempRadSys);
        HydronicRadiantSysNumericFields.allocate(NumOfHydrLowTempRadSys);

        // make sure data is gotten for surface lists
        GetNumberOfSurfaceLists();

        // Obtain all of the user data related to hydronic low temperature radiant systems...
        BaseNum = 0;
        CurrentModuleObject = "ZoneHVAC:LowTemperatureRadiant:VariableFlow";
        for (Item = 1; Item <= NumOfHydrLowTempRadSys; ++Item) {

            inputProcessor->getObjectItem(CurrentModuleObject,
                                          Item,
                                          Alphas,
                                          NumAlphas,
                                          Numbers,
                                          NumNumbers,
                                          IOStatus,
                                          lNumericBlanks,
                                          lAlphaBlanks,
                                          cAlphaFields,
                                          cNumericFields);

            HydronicRadiantSysNumericFields(Item).FieldNames.allocate(NumNumbers);
            HydronicRadiantSysNumericFields(Item).FieldNames = "";
            HydronicRadiantSysNumericFields(Item).FieldNames = cNumericFields;
            GlobalNames::VerifyUniqueInterObjectName(LowTempRadUniqueNames, Alphas(1), CurrentModuleObject, cAlphaFields(1), ErrorsFound);

            ++BaseNum;
            RadSysTypes(BaseNum).Name = Alphas(1);
            RadSysTypes(BaseNum).SystemType = HydronicSystem;

            auto &thisRadSys (HydrRadSys(Item));

            // General user input data
            thisRadSys.Name = Alphas(1);

            thisRadSys.SchedName = Alphas(2);
            if (lAlphaBlanks(2)) {
                thisRadSys.SchedPtr = ScheduleAlwaysOn;
            } else {
                thisRadSys.SchedPtr = GetScheduleIndex(Alphas(2));
                if (thisRadSys.SchedPtr == 0) {
                    ShowSevereError(cAlphaFields(2) + " not found for " + Alphas(1));
                    ShowContinueError("Missing " + cAlphaFields(2) + " is " + Alphas(2));
                    ErrorsFound = true;
                }
            }

            thisRadSys.ZoneName = Alphas(3);
            thisRadSys.ZonePtr = UtilityRoutines::FindItemInList(Alphas(3), Zone);
            if (thisRadSys.ZonePtr == 0) {
                ShowSevereError(RoutineName + "Invalid " + cAlphaFields(3) + " = " + Alphas(3));
                ShowContinueError("Occurs in " + CurrentModuleObject + " = " + Alphas(1));
                ErrorsFound = true;
            }

            thisRadSys.SurfListName = Alphas(4);
            SurfListNum = 0;
            if (NumOfSurfaceLists > 0) SurfListNum = UtilityRoutines::FindItemInList(thisRadSys.SurfListName, SurfList);
            if (SurfListNum > 0) { // Found a valid surface list
                thisRadSys.NumOfSurfaces = SurfList(SurfListNum).NumOfSurfaces;
                thisRadSys.SurfacePtr.allocate(thisRadSys.NumOfSurfaces);
                thisRadSys.SurfaceName.allocate(thisRadSys.NumOfSurfaces);
                thisRadSys.SurfaceFrac.allocate(thisRadSys.NumOfSurfaces);
                thisRadSys.NumCircuits.allocate(thisRadSys.NumOfSurfaces);
                for (SurfNum = 1; SurfNum <= SurfList(SurfListNum).NumOfSurfaces; ++SurfNum) {
                    thisRadSys.SurfacePtr(SurfNum) = SurfList(SurfListNum).SurfPtr(SurfNum);
                    thisRadSys.SurfaceName(SurfNum) = SurfList(SurfListNum).SurfName(SurfNum);
                    thisRadSys.SurfaceFrac(SurfNum) = SurfList(SurfListNum).SurfFlowFrac(SurfNum);
                    if (thisRadSys.SurfacePtr(SurfNum) > 0) {
                        Surface(thisRadSys.SurfacePtr(SurfNum)).IntConvSurfHasActiveInIt = true;
                    }
                }
            } else { // User entered a single surface name rather than a surface list
                thisRadSys.NumOfSurfaces = 1;
                thisRadSys.SurfacePtr.allocate(thisRadSys.NumOfSurfaces);
                thisRadSys.SurfaceName.allocate(thisRadSys.NumOfSurfaces);
                thisRadSys.SurfaceFrac.allocate(thisRadSys.NumOfSurfaces);
                thisRadSys.NumCircuits.allocate(thisRadSys.NumOfSurfaces);
                thisRadSys.SurfaceName(1) = thisRadSys.SurfListName;
                thisRadSys.SurfacePtr(1) = UtilityRoutines::FindItemInList(thisRadSys.SurfaceName(1), Surface);
                thisRadSys.SurfaceFrac(1) = 1.0;
                thisRadSys.NumCircuits(1) = 0.0;
                // Error checking for single surfaces
                if (thisRadSys.SurfacePtr(1) == 0) {
                    ShowSevereError(RoutineName + "Invalid " + cAlphaFields(4) + " = " + Alphas(4));
                    ShowContinueError("Occurs in " + CurrentModuleObject + " = " + Alphas(1));
                    ErrorsFound = true;
                } else if (Surface(thisRadSys.SurfacePtr(1)).IsRadSurfOrVentSlabOrPool) {
                    ShowSevereError(RoutineName + CurrentModuleObject + "=\"" + Alphas(1) + "\", Invalid Surface");
                    ShowContinueError(cAlphaFields(4) + "=\"" + Alphas(4) + "\" has been used in another radiant system or ventilated slab.");
                    ErrorsFound = true;
                }
                if (thisRadSys.SurfacePtr(1) != 0) {
                    Surface(thisRadSys.SurfacePtr(1)).IntConvSurfHasActiveInIt = true;
                    Surface(thisRadSys.SurfacePtr(1)).IsRadSurfOrVentSlabOrPool = true;
                }
            }

            // Error checking for zones and construction information
            thisRadSys.errorCheckZonesAndConstructions(ErrorsFound);

            thisRadSys.FluidToSlabHeatTransfer = thisRadSys.getFluidToSlabHeatTransferInput(Alphas(5));
            thisRadSys.TubeDiameterInner = Numbers(1);
            thisRadSys.TubeDiameterOuter = Numbers(2);
            thisRadSys.TubeLength = Numbers(3);
            thisRadSys.TubeConductivity = Numbers(4);

            // Process the temperature control type
            thisRadSys.ControlType = thisRadSys.processRadiantSystemControlInput(Alphas(6),cAlphaFields(6),HydronicSystem);

            // Process the setpoint type
            thisRadSys.SetpointType = thisRadSys.processRadiantSystemSetpointInput(Alphas(7),cAlphaFields(7));

            // Determine Low Temp Radiant heating design capacity sizing method
            if (UtilityRoutines::SameString(Alphas(8), "HeatingDesignCapacity")) {
                thisRadSys.HeatingCapMethod = HeatingDesignCapacity;
                if (!lNumericBlanks(5)) {
                    thisRadSys.ScaledHeatingCapacity = Numbers(5);
                    if (thisRadSys.ScaledHeatingCapacity < 0.0 && thisRadSys.ScaledHeatingCapacity != AutoSize) {
                        ShowSevereError(CurrentModuleObject + " = " + thisRadSys.Name);
                        ShowContinueError("Illegal " + cNumericFields(5) + " = " + TrimSigDigits(Numbers(5), 7));
                        ErrorsFound = true;
                    }
                } else {
                    if ((!lAlphaBlanks(9)) || (!lAlphaBlanks(10))) {
                        ShowSevereError(CurrentModuleObject + " = " + thisRadSys.Name);
                        ShowContinueError("Input for " + cAlphaFields(8) + " = " + Alphas(8));
                        ShowContinueError("Blank field not allowed for " + cNumericFields(5));
                        ErrorsFound = true;
                    }
                }
            } else if (UtilityRoutines::SameString(Alphas(8), "CapacityPerFloorArea")) {
                thisRadSys.HeatingCapMethod = CapacityPerFloorArea;
                if (!lNumericBlanks(6)) {
                    thisRadSys.ScaledHeatingCapacity = Numbers(6);
                    if (thisRadSys.ScaledHeatingCapacity <= 0.0) {
                        ShowSevereError(CurrentModuleObject + " = " + thisRadSys.Name);
                        ShowContinueError("Input for " + cAlphaFields(8) + " = " + Alphas(8));
                        ShowContinueError("Illegal " + cNumericFields(6) + " = " + TrimSigDigits(Numbers(6), 7));
                        ErrorsFound = true;
                    } else if (thisRadSys.ScaledHeatingCapacity == AutoSize) {
                        ShowSevereError(CurrentModuleObject + " = " + thisRadSys.Name);
                        ShowContinueError("Input for " + cAlphaFields(8) + " = " + Alphas(8));
                        ShowContinueError("Illegal " + cNumericFields(6) + " = Autosize");
                        ErrorsFound = true;
                    }
                } else {
                    ShowSevereError(CurrentModuleObject + " = " + thisRadSys.Name);
                    ShowContinueError("Input for " + cAlphaFields(8) + " = " + Alphas(8));
                    ShowContinueError("Blank field not allowed for " + cNumericFields(6));
                    ErrorsFound = true;
                }
            } else if (UtilityRoutines::SameString(Alphas(8), "FractionOfAutosizedHeatingCapacity")) {
                thisRadSys.HeatingCapMethod = FractionOfAutosizedHeatingCapacity;
                if (!lNumericBlanks(7)) {
                    thisRadSys.ScaledHeatingCapacity = Numbers(7);
                    if (thisRadSys.ScaledHeatingCapacity < 0.0) {
                        ShowSevereError(CurrentModuleObject + " = " + thisRadSys.Name);
                        ShowContinueError("Illegal " + cNumericFields(7) + " = " + TrimSigDigits(Numbers(7), 7));
                        ErrorsFound = true;
                    }
                } else {
                    ShowSevereError(CurrentModuleObject + " = " + thisRadSys.Name);
                    ShowContinueError("Input for " + cAlphaFields(8) + " = " + Alphas(8));
                    ShowContinueError("Blank field not allowed for " + cNumericFields(7));
                    ErrorsFound = true;
                }
            } else {
                ShowSevereError(CurrentModuleObject + " = " + thisRadSys.Name);
                ShowContinueError("Illegal " + cAlphaFields(8) + " = " + Alphas(8));
                ErrorsFound = true;
            }

            // Heating user input data
            thisRadSys.WaterVolFlowMaxHeat = Numbers(8);

            thisRadSys.HotWaterInNode = GetOnlySingleNode(
                Alphas(9), ErrorsFound, CurrentModuleObject, Alphas(1), NodeType_Water, NodeConnectionType_Inlet, 1, ObjectIsNotParent);

            thisRadSys.HotWaterOutNode = GetOnlySingleNode(
                Alphas(10), ErrorsFound, CurrentModuleObject, Alphas(1), NodeType_Water, NodeConnectionType_Outlet, 1, ObjectIsNotParent);

            if ((!lAlphaBlanks(9)) || (!lAlphaBlanks(10))) {
                TestCompSet(CurrentModuleObject, Alphas(1), Alphas(9), Alphas(10), "Hot Water Nodes");
            }

            thisRadSys.HotThrottlRange = Numbers(9);

            thisRadSys.HotSetptSched = Alphas(11);
            thisRadSys.HotSetptSchedPtr = GetScheduleIndex(Alphas(11));
            if ((thisRadSys.HotSetptSchedPtr == 0) && (!lAlphaBlanks(11))) {
                ShowSevereError(cAlphaFields(11) + " not found: " + Alphas(11));
                ShowContinueError("Occurs in " + CurrentModuleObject + " = " + Alphas(1));
                ErrorsFound = true;
            }

            if ((thisRadSys.WaterVolFlowMaxHeat == AutoSize) &&
                (lAlphaBlanks(9) || lAlphaBlanks(10) || lAlphaBlanks(11) || (thisRadSys.HotWaterInNode <= 0) ||
                 (thisRadSys.HotWaterOutNode <= 0) || (thisRadSys.HotSetptSchedPtr == 0))) {
                ShowSevereError("Hydronic radiant systems may not be autosized without specification of nodes or schedules.");
                ShowContinueError("Occurs in " + CurrentModuleObject + " (heating input) = " + Alphas(1));
                ErrorsFound = true;
            }

            // Determine Low Temp Radiant cooling design capacity sizing method
            if (UtilityRoutines::SameString(Alphas(12), "CoolingDesignCapacity")) {
                thisRadSys.CoolingCapMethod = CoolingDesignCapacity;
                if (!lNumericBlanks(10)) {
                    thisRadSys.ScaledCoolingCapacity = Numbers(10);
                    if (thisRadSys.ScaledCoolingCapacity < 0.0 && thisRadSys.ScaledCoolingCapacity != AutoSize) {
                        ShowSevereError(CurrentModuleObject + " = " + thisRadSys.Name);
                        ShowContinueError("Illegal " + cNumericFields(10) + " = " + TrimSigDigits(Numbers(10), 7));
                        ErrorsFound = true;
                    }
                } else {
                    if ((!lAlphaBlanks(13)) || (!lAlphaBlanks(14))) {
                        ShowSevereError(CurrentModuleObject + " = " + thisRadSys.Name);
                        ShowContinueError("Input for " + cAlphaFields(12) + " = " + Alphas(12));
                        ShowContinueError("Blank field not allowed for " + cNumericFields(10));
                        ErrorsFound = true;
                    }
                }
            } else if (UtilityRoutines::SameString(Alphas(12), "CapacityPerFloorArea")) {
                thisRadSys.CoolingCapMethod = CapacityPerFloorArea;
                if (!lNumericBlanks(9)) {
                    thisRadSys.ScaledCoolingCapacity = Numbers(11);
                    if (thisRadSys.CoolingCapMethod <= 0.0) {
                        ShowSevereError(CurrentModuleObject + " = " + thisRadSys.Name);
                        ShowContinueError("Input for " + cAlphaFields(12) + " = " + Alphas(12));
                        ShowContinueError("Illegal " + cNumericFields(11) + " = " + TrimSigDigits(Numbers(11), 7));
                        ErrorsFound = true;
                    } else if (thisRadSys.ScaledCoolingCapacity == AutoSize) {
                        ShowSevereError(CurrentModuleObject + " = " + thisRadSys.Name);
                        ShowContinueError("Input for " + cAlphaFields(12) + " = " + Alphas(12));
                        ShowContinueError("Illegal " + cNumericFields(11) + " = Autosize");
                        ErrorsFound = true;
                    }
                } else {
                    ShowSevereError(CurrentModuleObject + " = " + thisRadSys.Name);
                    ShowContinueError("Input for " + cAlphaFields(12) + " = " + Alphas(12));
                    ShowContinueError("Blank field not allowed for " + cNumericFields(11));
                    ErrorsFound = true;
                }
            } else if (UtilityRoutines::SameString(Alphas(12), "FractionOfAutosizedCoolingCapacity")) {
                thisRadSys.CoolingCapMethod = FractionOfAutosizedCoolingCapacity;
                if (!lNumericBlanks(12)) {
                    thisRadSys.ScaledCoolingCapacity = Numbers(12);
                    if (thisRadSys.ScaledCoolingCapacity < 0.0) {
                        ShowSevereError(CurrentModuleObject + " = " + thisRadSys.Name);
                        ShowContinueError("Illegal " + cNumericFields(12) + " = " + TrimSigDigits(Numbers(12), 7));
                        ErrorsFound = true;
                    }
                } else {
                    ShowSevereError(CurrentModuleObject + " = " + thisRadSys.Name);
                    ShowContinueError("Input for " + cAlphaFields(12) + " = " + Alphas(12));
                    ShowContinueError("Blank field not allowed for " + cNumericFields(12));
                    ErrorsFound = true;
                }
            } else {
                ShowSevereError(CurrentModuleObject + " = " + thisRadSys.Name);
                ShowContinueError("Illegal " + cAlphaFields(12) + " = " + Alphas(12));
                ErrorsFound = true;
            }

            // Cooling user input data
            thisRadSys.WaterVolFlowMaxCool = Numbers(13);

            thisRadSys.ColdWaterInNode = GetOnlySingleNode(
                Alphas(13), ErrorsFound, CurrentModuleObject, Alphas(1), NodeType_Water, NodeConnectionType_Inlet, 2, ObjectIsNotParent);

            thisRadSys.ColdWaterOutNode = GetOnlySingleNode(
                Alphas(14), ErrorsFound, CurrentModuleObject, Alphas(1), NodeType_Water, NodeConnectionType_Outlet, 2, ObjectIsNotParent);

            if ((!lAlphaBlanks(13)) || (!lAlphaBlanks(14))) {
                TestCompSet(CurrentModuleObject, Alphas(1), Alphas(13), Alphas(14), "Chilled Water Nodes");
            }

            thisRadSys.ColdThrottlRange = Numbers(14);

            thisRadSys.ColdSetptSched = Alphas(15);
            thisRadSys.ColdSetptSchedPtr = GetScheduleIndex(Alphas(15));
            if ((thisRadSys.ColdSetptSchedPtr == 0) && (!lAlphaBlanks(15))) {
                ShowSevereError(cAlphaFields(15) + " not found: " + Alphas(15));
                ShowContinueError("Occurs in " + CurrentModuleObject + " = " + Alphas(1));
                ErrorsFound = true;
            }

            if (UtilityRoutines::SameString(Alphas(16), Off)) {
                thisRadSys.CondCtrlType = CondCtrlNone;
            } else if (UtilityRoutines::SameString(Alphas(16), SimpleOff)) {
                thisRadSys.CondCtrlType = CondCtrlSimpleOff;
            } else if (UtilityRoutines::SameString(Alphas(16), VariableOff)) {
                thisRadSys.CondCtrlType = CondCtrlVariedOff;
            } else {
                thisRadSys.CondCtrlType = CondCtrlSimpleOff;
            }

            thisRadSys.CondDewPtDeltaT = Numbers(15);

            if (UtilityRoutines::SameString(Alphas(17), OnePerSurf)) {
                thisRadSys.NumCircCalcMethod = OneCircuit;
            } else if (UtilityRoutines::SameString(Alphas(17), CalcFromLength)) {
                thisRadSys.NumCircCalcMethod = CalculateFromLength;
            } else {
                thisRadSys.NumCircCalcMethod = OneCircuit;
            }

            thisRadSys.CircLength = Numbers(16);

            thisRadSys.schedNameChangeoverDelay = Alphas(18);
            if (!lAlphaBlanks(18)) {
                thisRadSys.schedPtrChangeoverDelay = GetScheduleIndex(Alphas(18));
                if (thisRadSys.schedPtrChangeoverDelay == 0) {
                    ShowWarningError(cAlphaFields(18) + " not found for " + Alphas(18));
                    ShowContinueError("This occurs for " + cAlphaFields(1) + " = " + Alphas(1));
                    ShowContinueError("As a result, no changeover delay will be used for this radiant system.");
                }
            }

            if ((thisRadSys.WaterVolFlowMaxCool == AutoSize) &&
                (lAlphaBlanks(12) || lAlphaBlanks(13) || lAlphaBlanks(14) || (thisRadSys.ColdWaterInNode <= 0) ||
                 (thisRadSys.ColdWaterOutNode <= 0) || (thisRadSys.ColdSetptSchedPtr == 0))) {
                ShowSevereError("Hydronic radiant systems may not be autosized without specification of nodes or schedules");
                ShowContinueError("Occurs in " + CurrentModuleObject + " (cooling input) =" + Alphas(1));
                ErrorsFound = true;
            }
        }

        // Obtain all of the user data related to constant flow (hydronic) low temperature radiant systems...

        CurrentModuleObject = "ZoneHVAC:LowTemperatureRadiant:ConstantFlow";

        for (Item = 1; Item <= NumOfCFloLowTempRadSys; ++Item) {

            inputProcessor->getObjectItem(CurrentModuleObject,
                                          Item,
                                          Alphas,
                                          NumAlphas,
                                          Numbers,
                                          NumNumbers,
                                          IOStatus,
                                          lNumericBlanks,
                                          lAlphaBlanks,
                                          cAlphaFields,
                                          cNumericFields);
            GlobalNames::VerifyUniqueInterObjectName(LowTempRadUniqueNames, Alphas(1), CurrentModuleObject, cAlphaFields(1), ErrorsFound);
            ++BaseNum;
            RadSysTypes(BaseNum).Name = Alphas(1);
            RadSysTypes(BaseNum).SystemType = ConstantFlowSystem;

            // General user input data
            auto &thisCFloSys (CFloRadSys(Item));

            thisCFloSys.Name = Alphas(1);

            thisCFloSys.SchedName = Alphas(2);
            if (lAlphaBlanks(2)) {
                thisCFloSys.SchedPtr = ScheduleAlwaysOn;
            } else {
                thisCFloSys.SchedPtr = GetScheduleIndex(Alphas(2));
                if (thisCFloSys.SchedPtr == 0) {
                    ShowSevereError(cAlphaFields(2) + " not found for " + Alphas(1));
                    ShowContinueError("Missing " + cAlphaFields(2) + " is " + Alphas(2));
                    ErrorsFound = true;
                }
            }

            thisCFloSys.ZoneName = Alphas(3);
            thisCFloSys.ZonePtr = UtilityRoutines::FindItemInList(Alphas(3), Zone);
            if (thisCFloSys.ZonePtr == 0) {
                ShowSevereError(RoutineName + "Invalid " + cAlphaFields(3) + " = " + Alphas(3));
                ShowContinueError("Occurs in " + CurrentModuleObject + " = " + Alphas(1));
                ErrorsFound = true;
            }

            thisCFloSys.SurfListName = Alphas(4);
            SurfListNum = 0;
            if (NumOfSurfaceLists > 0) SurfListNum = UtilityRoutines::FindItemInList(thisCFloSys.SurfListName, SurfList);
            if (SurfListNum > 0) { // Found a valid surface list
                thisCFloSys.NumOfSurfaces = SurfList(SurfListNum).NumOfSurfaces;
                thisCFloSys.SurfacePtr.allocate(thisCFloSys.NumOfSurfaces);
                thisCFloSys.SurfaceName.allocate(thisCFloSys.NumOfSurfaces);
                thisCFloSys.SurfaceFrac.allocate(thisCFloSys.NumOfSurfaces);
                thisCFloSys.NumCircuits.allocate(thisCFloSys.NumOfSurfaces);
                MaxCloNumOfSurfaces = max(MaxCloNumOfSurfaces, thisCFloSys.NumOfSurfaces);
                for (SurfNum = 1; SurfNum <= SurfList(SurfListNum).NumOfSurfaces; ++SurfNum) {
                    thisCFloSys.SurfacePtr(SurfNum) = SurfList(SurfListNum).SurfPtr(SurfNum);
                    thisCFloSys.SurfaceName(SurfNum) = SurfList(SurfListNum).SurfName(SurfNum);
                    thisCFloSys.SurfaceFrac(SurfNum) = SurfList(SurfListNum).SurfFlowFrac(SurfNum);
                    thisCFloSys.NumCircuits(SurfNum) = 0.0;
                    if (thisCFloSys.SurfacePtr(SurfNum) != 0) {
                        Surface(thisCFloSys.SurfacePtr(SurfNum)).IntConvSurfHasActiveInIt = true;
                    }
                }
            } else { // User entered a single surface name rather than a surface list
                thisCFloSys.NumOfSurfaces = 1;
                thisCFloSys.SurfacePtr.allocate(thisCFloSys.NumOfSurfaces);
                thisCFloSys.SurfaceName.allocate(thisCFloSys.NumOfSurfaces);
                thisCFloSys.SurfaceFrac.allocate(thisCFloSys.NumOfSurfaces);
                thisCFloSys.NumCircuits.allocate(thisCFloSys.NumOfSurfaces);
                MaxCloNumOfSurfaces = max(MaxCloNumOfSurfaces, thisCFloSys.NumOfSurfaces);
                thisCFloSys.SurfaceName(1) = thisCFloSys.SurfListName;
                thisCFloSys.SurfacePtr(1) = UtilityRoutines::FindItemInList(thisCFloSys.SurfaceName(1), Surface);
                thisCFloSys.SurfaceFrac(1) = 1.0;
                thisCFloSys.NumCircuits(1) = 0.0;
                // Error checking for single surfaces
                if (thisCFloSys.SurfacePtr(1) == 0) {
                    ShowSevereError(RoutineName + "Invalid " + cAlphaFields(4) + " = " + Alphas(4));
                    ShowContinueError("Occurs in " + CurrentModuleObject + " = " + Alphas(1));
                    ErrorsFound = true;
                } else if (Surface(thisCFloSys.SurfacePtr(1)).IsRadSurfOrVentSlabOrPool) {
                    ShowSevereError(RoutineName + CurrentModuleObject + "=\"" + Alphas(1) + "\", Invalid Surface");
                    ShowContinueError(cAlphaFields(4) + "=\"" + Alphas(4) + "\" has been used in another radiant system or ventilated slab.");
                    ErrorsFound = true;
                }
                if (thisCFloSys.SurfacePtr(1) != 0) {
                    Surface(thisCFloSys.SurfacePtr(1)).IntConvSurfHasActiveInIt = true;
                    Surface(thisCFloSys.SurfacePtr(1)).IsRadSurfOrVentSlabOrPool = true;
                }
            }

            // Error checking for zones and construction information
            thisCFloSys.errorCheckZonesAndConstructions(ErrorsFound);

            thisCFloSys.FluidToSlabHeatTransfer = thisCFloSys.getFluidToSlabHeatTransferInput(Alphas(5));
            thisCFloSys.TubeDiameterInner = Numbers(1);
            thisCFloSys.TubeDiameterOuter = Numbers(2);
            thisCFloSys.TubeLength = Numbers(3);
            thisCFloSys.TubeConductivity = Numbers(4);

            // Process the temperature control type
            thisCFloSys.ControlType = thisCFloSys.processRadiantSystemControlInput(Alphas(6),cAlphaFields(6),ConstantFlowSystem);
            thisCFloSys.runningMeanOutdoorAirTemperatureWeightingFactor = Numbers(5);

            // Process pump input for constant flow (hydronic) radiant system
            thisCFloSys.WaterVolFlowMax = Numbers(6);
            thisCFloSys.VolFlowSched = Alphas(7);
            thisCFloSys.VolFlowSchedPtr = GetScheduleIndex(Alphas(7));
            if ((thisCFloSys.VolFlowSchedPtr == 0) && (!lAlphaBlanks(7))) {
                ShowSevereError(cAlphaFields(7) + " not found: " + Alphas(7));
                ShowContinueError("Occurs in " + CurrentModuleObject + " = " + Alphas(1));
                ErrorsFound = true;
            }
            thisCFloSys.NomPumpHead = Numbers(7);
            thisCFloSys.NomPowerUse = Numbers(8);
            thisCFloSys.MotorEffic = Numbers(9);
            thisCFloSys.FracMotorLossToFluid = Numbers(10);

            // Heating user input data
            thisCFloSys.HotWaterInNode = GetOnlySingleNode(
                Alphas(8), ErrorsFound, CurrentModuleObject, Alphas(1), NodeType_Water, NodeConnectionType_Inlet, 1, ObjectIsNotParent);

            thisCFloSys.HotWaterOutNode = GetOnlySingleNode(
                Alphas(9), ErrorsFound, CurrentModuleObject, Alphas(1), NodeType_Water, NodeConnectionType_Outlet, 1, ObjectIsNotParent);

            if ((!lAlphaBlanks(8)) || (!lAlphaBlanks(9))) {
                TestCompSet(CurrentModuleObject, Alphas(1), Alphas(8), Alphas(9), "Hot Water Nodes");
            }

            thisCFloSys.HotWaterHiTempSched = Alphas(10);
            thisCFloSys.HotWaterHiTempSchedPtr = GetScheduleIndex(Alphas(10));
            if ((thisCFloSys.HotWaterHiTempSchedPtr == 0) && (!lAlphaBlanks(10))) {
                ShowSevereError(cAlphaFields(10) + " not found: " + Alphas(10));
                ShowContinueError("Occurs in " + CurrentModuleObject + " = " + Alphas(1));
                ErrorsFound = true;
            }

            thisCFloSys.HotWaterLoTempSched = Alphas(11);
            thisCFloSys.HotWaterLoTempSchedPtr = GetScheduleIndex(Alphas(11));
            if ((thisCFloSys.HotWaterLoTempSchedPtr == 0) && (!lAlphaBlanks(11))) {
                ShowSevereError(cAlphaFields(11) + " not found: " + Alphas(11));
                ShowContinueError("Occurs in " + CurrentModuleObject + " = " + Alphas(1));
                ErrorsFound = true;
            }

            thisCFloSys.HotCtrlHiTempSched = Alphas(12);
            thisCFloSys.HotCtrlHiTempSchedPtr = GetScheduleIndex(Alphas(12));
            if ((thisCFloSys.HotCtrlHiTempSchedPtr == 0) && (!lAlphaBlanks(12))) {
                ShowSevereError(cAlphaFields(12) + " not found: " + Alphas(12));
                ShowContinueError("Occurs in " + CurrentModuleObject + " = " + Alphas(1));
                ErrorsFound = true;
            }

            thisCFloSys.HotCtrlLoTempSched = Alphas(13);
            thisCFloSys.HotCtrlLoTempSchedPtr = GetScheduleIndex(Alphas(13));
            if ((thisCFloSys.HotCtrlLoTempSchedPtr == 0) && (!lAlphaBlanks(13))) {
                ShowSevereError(cAlphaFields(13) + " not found: " + Alphas(13));
                ShowContinueError("Occurs in " + CurrentModuleObject + " = " + Alphas(1));
                ErrorsFound = true;
            }

            // Cooling user input data
            thisCFloSys.ColdWaterInNode = GetOnlySingleNode(
                Alphas(14), ErrorsFound, CurrentModuleObject, Alphas(1), NodeType_Water, NodeConnectionType_Inlet, 2, ObjectIsNotParent);

            thisCFloSys.ColdWaterOutNode = GetOnlySingleNode(
                Alphas(15), ErrorsFound, CurrentModuleObject, Alphas(1), NodeType_Water, NodeConnectionType_Outlet, 2, ObjectIsNotParent);

            if ((!lAlphaBlanks(14)) || (!lAlphaBlanks(15))) {
                TestCompSet(CurrentModuleObject, Alphas(1), Alphas(14), Alphas(15), "Chilled Water Nodes");
            }

            thisCFloSys.ColdWaterHiTempSched = Alphas(16);
            thisCFloSys.ColdWaterHiTempSchedPtr = GetScheduleIndex(Alphas(16));
            if ((thisCFloSys.ColdWaterHiTempSchedPtr == 0) && (!lAlphaBlanks(16))) {
                ShowSevereError(cAlphaFields(16) + " not found: " + Alphas(16));
                ShowContinueError("Occurs in " + CurrentModuleObject + " = " + Alphas(1));
                ErrorsFound = true;
            }

            thisCFloSys.ColdWaterLoTempSched = Alphas(17);
            thisCFloSys.ColdWaterLoTempSchedPtr = GetScheduleIndex(Alphas(17));
            if ((thisCFloSys.ColdWaterLoTempSchedPtr == 0) && (!lAlphaBlanks(17))) {
                ShowSevereError(cAlphaFields(17) + " not found: " + Alphas(17));
                ShowContinueError("Occurs in " + CurrentModuleObject + " = " + Alphas(1));
                ErrorsFound = true;
            }

            thisCFloSys.ColdCtrlHiTempSched = Alphas(18);
            thisCFloSys.ColdCtrlHiTempSchedPtr = GetScheduleIndex(Alphas(18));
            if ((thisCFloSys.ColdCtrlHiTempSchedPtr == 0) && (!lAlphaBlanks(18))) {
                ShowSevereError(cAlphaFields(18) + " not found: " + Alphas(18));
                ShowContinueError("Occurs in " + CurrentModuleObject + " = " + Alphas(1));
                ErrorsFound = true;
            }

            thisCFloSys.ColdCtrlLoTempSched = Alphas(19);
            thisCFloSys.ColdCtrlLoTempSchedPtr = GetScheduleIndex(Alphas(19));
            if ((thisCFloSys.ColdCtrlLoTempSchedPtr == 0) && (!lAlphaBlanks(19))) {
                ShowSevereError(cAlphaFields(19) + " not found: " + Alphas(19));
                ShowContinueError("Occurs in " + CurrentModuleObject + " = " + Alphas(1));
                ErrorsFound = true;
            }

            if (UtilityRoutines::SameString(Alphas(20), Off)) {
                thisCFloSys.CondCtrlType = CondCtrlNone;
            } else if (UtilityRoutines::SameString(Alphas(20), SimpleOff)) {
                thisCFloSys.CondCtrlType = CondCtrlSimpleOff;
            } else if (UtilityRoutines::SameString(Alphas(20), VariableOff)) {
                thisCFloSys.CondCtrlType = CondCtrlVariedOff;
            } else {
                thisCFloSys.CondCtrlType = CondCtrlSimpleOff;
            }

            thisCFloSys.CondDewPtDeltaT = Numbers(11);

            if (UtilityRoutines::SameString(Alphas(21), OnePerSurf)) {
                thisCFloSys.NumCircCalcMethod = OneCircuit;
            } else if (UtilityRoutines::SameString(Alphas(21), CalcFromLength)) {
                thisCFloSys.NumCircCalcMethod = CalculateFromLength;
            } else {
                thisCFloSys.NumCircCalcMethod = OneCircuit;
            }

            thisCFloSys.CircLength = Numbers(12);

            thisCFloSys.schedNameChangeoverDelay = Alphas(22);
            if (!lAlphaBlanks(22)) {
                thisCFloSys.schedPtrChangeoverDelay = GetScheduleIndex(Alphas(22));
                if (thisCFloSys.schedPtrChangeoverDelay == 0) {
                    ShowWarningError(cAlphaFields(22) + " not found for " + Alphas(22));
                    ShowContinueError("This occurs for " + cAlphaFields(1) + " = " + Alphas(1));
                    ShowContinueError("As a result, no changeover delay will be used for this radiant system.");
                }
            }
        }

        // Obtain all of the user data related to electric low temperature radiant systems...
        CurrentModuleObject = "ZoneHVAC:LowTemperatureRadiant:Electric";

        for (Item = 1; Item <= NumOfElecLowTempRadSys; ++Item) {

            inputProcessor->getObjectItem(CurrentModuleObject,
                                          Item,
                                          Alphas,
                                          NumAlphas,
                                          Numbers,
                                          NumNumbers,
                                          IOStatus,
                                          lNumericBlanks,
                                          lAlphaBlanks,
                                          cAlphaFields,
                                          cNumericFields);

            ElecRadSysNumericFields(Item).FieldNames.allocate(NumNumbers);
            ElecRadSysNumericFields(Item).FieldNames = "";
            ElecRadSysNumericFields(Item).FieldNames = cNumericFields;

            GlobalNames::VerifyUniqueInterObjectName(LowTempRadUniqueNames, Alphas(1), CurrentModuleObject, cAlphaFields(1), ErrorsFound);
            ++BaseNum;
            RadSysTypes(BaseNum).Name = Alphas(1);
            RadSysTypes(BaseNum).SystemType = ElectricSystem;

            // General user input data
            auto &thisElecSys (ElecRadSys(Item));

            thisElecSys.Name = Alphas(1);

            thisElecSys.SchedName = Alphas(2);
            if (lAlphaBlanks(2)) {
                thisElecSys.SchedPtr = ScheduleAlwaysOn;
            } else {
                thisElecSys.SchedPtr = GetScheduleIndex(Alphas(2));
                if (thisElecSys.SchedPtr == 0) {
                    ShowSevereError(cAlphaFields(2) + " not found for" + Alphas(1));
                    ShowContinueError("Incorrect " + cAlphaFields(2) + " = " + Alphas(2));
                    ErrorsFound = true;
                }
            }

            thisElecSys.ZoneName = Alphas(3);
            thisElecSys.ZonePtr = UtilityRoutines::FindItemInList(Alphas(3), Zone);
            if (thisElecSys.ZonePtr == 0) {
                ShowSevereError(RoutineName + "Invalid " + cAlphaFields(3) + " = " + Alphas(3));
                ShowContinueError("Occurs in " + CurrentModuleObject + " = " + Alphas(1));
                ErrorsFound = true;
            }

            thisElecSys.SurfListName = Alphas(4);
            SurfListNum = 0;
            if (NumOfSurfaceLists > 0) SurfListNum = UtilityRoutines::FindItemInList(thisElecSys.SurfListName, SurfList);
            if (SurfListNum > 0) { // Found a valid surface list
                thisElecSys.NumOfSurfaces = SurfList(SurfListNum).NumOfSurfaces;
                thisElecSys.SurfacePtr.allocate(thisElecSys.NumOfSurfaces);
                thisElecSys.SurfaceName.allocate(thisElecSys.NumOfSurfaces);
                thisElecSys.SurfaceFrac.allocate(thisElecSys.NumOfSurfaces);
                for (SurfNum = 1; SurfNum <= SurfList(SurfListNum).NumOfSurfaces; ++SurfNum) {
                    thisElecSys.SurfacePtr(SurfNum) = SurfList(SurfListNum).SurfPtr(SurfNum);
                    thisElecSys.SurfaceName(SurfNum) = SurfList(SurfListNum).SurfName(SurfNum);
                    thisElecSys.SurfaceFrac(SurfNum) = SurfList(SurfListNum).SurfFlowFrac(SurfNum);
                }
            } else { // User entered a single surface name rather than a surface list
                thisElecSys.NumOfSurfaces = 1;
                thisElecSys.SurfacePtr.allocate(thisElecSys.NumOfSurfaces);
                thisElecSys.SurfaceName.allocate(thisElecSys.NumOfSurfaces);
                thisElecSys.SurfaceFrac.allocate(thisElecSys.NumOfSurfaces);
                thisElecSys.SurfaceName(1) = thisElecSys.SurfListName;
                thisElecSys.SurfacePtr(1) = UtilityRoutines::FindItemInList(thisElecSys.SurfaceName(1), Surface);
                thisElecSys.SurfaceFrac(1) = 1.0;
                // Error checking for single surfaces
                if (thisElecSys.SurfacePtr(1) == 0) {
                    ShowSevereError(RoutineName + "Invalid " + cAlphaFields(4) + " = " + Alphas(4));
                    ShowContinueError("Occurs in " + CurrentModuleObject + " = " + Alphas(1));
                    ErrorsFound = true;
                } else if (Surface(thisElecSys.SurfacePtr(1)).IsRadSurfOrVentSlabOrPool) {
                    ShowSevereError(RoutineName + CurrentModuleObject + "=\"" + Alphas(1) + "\", Invalid Surface");
                    ShowContinueError(cAlphaFields(4) + "=\"" + Alphas(4) + "\" has been used in another radiant system or ventilated slab.");
                    ErrorsFound = true;
                }
                if (thisElecSys.SurfacePtr(1) != 0) {
                    Surface(ElecRadSys(Item).SurfacePtr(1)).IsRadSurfOrVentSlabOrPool = true;
                }
            }

            // Error checking for zones and construction information
            thisElecSys.errorCheckZonesAndConstructions(ErrorsFound);

            // Heating user input data
            // Determine Low Temp Radiant heating design capacity sizing method
            if (UtilityRoutines::SameString(Alphas(iHeatCAPMAlphaNum), "HeatingDesignCapacity")) {
                thisElecSys.HeatingCapMethod = HeatingDesignCapacity;
                if (!lNumericBlanks(iHeatDesignCapacityNumericNum)) {
                    thisElecSys.ScaledHeatingCapacity = Numbers(iHeatDesignCapacityNumericNum);
                    thisElecSys.MaxElecPower = thisElecSys.ScaledHeatingCapacity;
                    if (thisElecSys.ScaledHeatingCapacity < 0.0 && thisElecSys.ScaledHeatingCapacity != AutoSize) {
                        ShowSevereError(CurrentModuleObject + " = " + thisElecSys.Name);
                        ShowContinueError("Illegal " + cNumericFields(iHeatDesignCapacityNumericNum) + " = " +
                                          TrimSigDigits(Numbers(iHeatDesignCapacityNumericNum), 7));
                        ErrorsFound = true;
                    }
                } else {
                    ShowSevereError(CurrentModuleObject + " = " + thisElecSys.Name);
                    ShowContinueError("Input for " + cAlphaFields(iHeatCAPMAlphaNum) + " = " + Alphas(iHeatCAPMAlphaNum));
                    ShowContinueError("Blank field not allowed for " + cNumericFields(iHeatDesignCapacityNumericNum));
                    ErrorsFound = true;
                }
            } else if (UtilityRoutines::SameString(Alphas(iHeatCAPMAlphaNum), "CapacityPerFloorArea")) {
                thisElecSys.HeatingCapMethod = CapacityPerFloorArea;
                if (!lNumericBlanks(iHeatCapacityPerFloorAreaNumericNum)) {
                    thisElecSys.ScaledHeatingCapacity = Numbers(iHeatCapacityPerFloorAreaNumericNum);
                    thisElecSys.MaxElecPower = thisElecSys.ScaledHeatingCapacity;
                    if (thisElecSys.ScaledHeatingCapacity <= 0.0) {
                        ShowSevereError(CurrentModuleObject + " = " + thisElecSys.Name);
                        ShowContinueError("Input for " + cAlphaFields(iHeatCAPMAlphaNum) + " = " + Alphas(iHeatCAPMAlphaNum));
                        ShowContinueError("Illegal " + cNumericFields(iHeatCapacityPerFloorAreaNumericNum) + " = " +
                                          TrimSigDigits(Numbers(iHeatCapacityPerFloorAreaNumericNum), 7));
                        ErrorsFound = true;
                    } else if (thisElecSys.ScaledHeatingCapacity == AutoSize) {
                        ShowSevereError(CurrentModuleObject + " = " + thisElecSys.Name);
                        ShowContinueError("Input for " + cAlphaFields(iHeatCAPMAlphaNum) + " = " + Alphas(iHeatCAPMAlphaNum));
                        ShowContinueError("Illegal " + cNumericFields(iHeatCapacityPerFloorAreaNumericNum) + " = Autosize");
                        ErrorsFound = true;
                    }
                } else {
                    ShowSevereError(CurrentModuleObject + " = " + thisElecSys.Name);
                    ShowContinueError("Input for " + cAlphaFields(iHeatCAPMAlphaNum) + " = " + Alphas(iHeatCAPMAlphaNum));
                    ShowContinueError("Blank field not allowed for " + cNumericFields(iHeatCapacityPerFloorAreaNumericNum));
                    ErrorsFound = true;
                }
            } else if (UtilityRoutines::SameString(Alphas(iHeatCAPMAlphaNum), "FractionOfAutosizedHeatingCapacity")) {
                thisElecSys.HeatingCapMethod = FractionOfAutosizedHeatingCapacity;
                if (!lNumericBlanks(iHeatFracOfAutosizedCapacityNumericNum)) {
                    thisElecSys.ScaledHeatingCapacity = Numbers(iHeatFracOfAutosizedCapacityNumericNum);
                    thisElecSys.MaxElecPower = thisElecSys.ScaledHeatingCapacity;
                    if (thisElecSys.ScaledHeatingCapacity < 0.0) {
                        ShowSevereError(CurrentModuleObject + " = " + thisElecSys.Name);
                        ShowContinueError("Illegal " + cNumericFields(iHeatFracOfAutosizedCapacityNumericNum) + " = " +
                                          TrimSigDigits(Numbers(iHeatFracOfAutosizedCapacityNumericNum), 7));
                        ErrorsFound = true;
                    }
                } else {
                    ShowSevereError(CurrentModuleObject + " = " + thisElecSys.Name);
                    ShowContinueError("Input for " + cAlphaFields(iHeatCAPMAlphaNum) + " = " + Alphas(iHeatCAPMAlphaNum));
                    ShowContinueError("Blank field not allowed for " + cNumericFields(iHeatFracOfAutosizedCapacityNumericNum));
                    ErrorsFound = true;
                }
            } else {
                ShowSevereError(CurrentModuleObject + " = " + thisElecSys.Name);
                ShowContinueError("Illegal " + cAlphaFields(iHeatCAPMAlphaNum) + " = " + Alphas(iHeatCAPMAlphaNum));
                ErrorsFound = true;
            }

            // Process the temperature control type
            thisElecSys.ControlType = thisElecSys.processRadiantSystemControlInput(Alphas(6),cAlphaFields(6),ElectricSystem);

            // Process the setpoint type
            thisElecSys.SetpointType = thisElecSys.processRadiantSystemSetpointInput(Alphas(7),cAlphaFields(7));

            thisElecSys.ThrottlRange = Numbers(4);

            thisElecSys.SetptSched = Alphas(8);
            thisElecSys.SetptSchedPtr = GetScheduleIndex(Alphas(8));
            if (thisElecSys.SetptSchedPtr == 0) {
                if (lAlphaBlanks(8)) {
                    ShowSevereError(cAlphaFields(8) + " must be input, missing for " + Alphas(1));
                } else {
                    ShowSevereError(cAlphaFields(8) + " not found for " + Alphas(8));
                    ShowContinueError("Incorrect " + cAlphaFields(8) + " = " + Alphas(8));
                }
                ErrorsFound = true;
            }
        }

        // Check to see if any surface is included in more than one radiant system.  This is not allowed
        // and thus indicative that there is an error in the input file.  This is to make sure that two
        // different radiant systems are competing for the same surface.  Allowing this to happen would
        // result in lost energy somewhere and the situation really is not physically possible anyway.
        AssignedAsRadiantSurface.dimension(TotSurfaces, false);

        for (Item = 1; Item <= NumOfHydrLowTempRadSys; ++Item) {
            for (SurfNum = 1; SurfNum <= HydrRadSys(Item).NumOfSurfaces; ++SurfNum) {
                CheckSurfNum = HydrRadSys(Item).SurfacePtr(SurfNum);
                if (CheckSurfNum == 0) continue;
                if (AssignedAsRadiantSurface(CheckSurfNum)) {
                    ShowSevereError("Surface " + Surface(CheckSurfNum).Name + " is referenced by more than one radiant system--this is not allowed");
                    ErrorsFound = true;
                } else {
                    AssignedAsRadiantSurface(CheckSurfNum) = true;
                }
                // Also check the other side of interzone partitions
                if ((Surface(CheckSurfNum).ExtBoundCond > 0) && (Surface(CheckSurfNum).ExtBoundCond != CheckSurfNum)) {
                    if (AssignedAsRadiantSurface(Surface(CheckSurfNum).ExtBoundCond)) {
                        ShowSevereError("Interzone surface " + Surface(Surface(CheckSurfNum).ExtBoundCond).Name +
                                        " is referenced by more than one radiant system--this is not allowed");
                        ErrorsFound = true;
                    } else {
                        AssignedAsRadiantSurface(Surface(CheckSurfNum).ExtBoundCond) = true;
                    }
                }
            }
        }

        for (Item = 1; Item <= NumOfCFloLowTempRadSys; ++Item) {
            for (SurfNum = 1; SurfNum <= CFloRadSys(Item).NumOfSurfaces; ++SurfNum) {
                CheckSurfNum = CFloRadSys(Item).SurfacePtr(SurfNum);
                if (CheckSurfNum == 0) continue;
                if (AssignedAsRadiantSurface(CheckSurfNum)) {
                    ShowSevereError("Surface " + Surface(CheckSurfNum).Name + " is referenced by more than one radiant system--this is not allowed");
                    ErrorsFound = true;
                } else {
                    AssignedAsRadiantSurface(CheckSurfNum) = true;
                }
                // Also check the other side of interzone partitions
                if ((Surface(CheckSurfNum).ExtBoundCond > 0) && (Surface(CheckSurfNum).ExtBoundCond != CheckSurfNum)) {
                    if (AssignedAsRadiantSurface(Surface(CheckSurfNum).ExtBoundCond)) {
                        ShowSevereError("Interzone surface " + Surface(Surface(CheckSurfNum).ExtBoundCond).Name +
                                        " is referenced by more than one radiant system--this is not allowed");
                        ErrorsFound = true;
                    } else {
                        AssignedAsRadiantSurface(Surface(CheckSurfNum).ExtBoundCond) = true;
                    }
                }
            }
        }

        for (Item = 1; Item <= NumOfElecLowTempRadSys; ++Item) {
            for (SurfNum = 1; SurfNum <= ElecRadSys(Item).NumOfSurfaces; ++SurfNum) {
                CheckSurfNum = ElecRadSys(Item).SurfacePtr(SurfNum);
                if (CheckSurfNum == 0) continue;
                if (AssignedAsRadiantSurface(CheckSurfNum)) {
                    ShowSevereError("Surface " + Surface(CheckSurfNum).Name + " is referenced by more than one radiant system--this is not allowed");
                    ErrorsFound = true;
                } else {
                    AssignedAsRadiantSurface(CheckSurfNum) = true;
                }
                // Also check the other side of interzone partitions
                if ((Surface(CheckSurfNum).ExtBoundCond > 0) && (Surface(CheckSurfNum).ExtBoundCond != CheckSurfNum)) {
                    if (AssignedAsRadiantSurface(Surface(CheckSurfNum).ExtBoundCond)) {
                        ShowSevereError("Interzone surface " + Surface(Surface(CheckSurfNum).ExtBoundCond).Name +
                                        " is referenced by more than one radiant system--this is not allowed");
                        ErrorsFound = true;
                    } else {
                        AssignedAsRadiantSurface(Surface(CheckSurfNum).ExtBoundCond) = true;
                    }
                }
            }
        }

        AssignedAsRadiantSurface.deallocate();
        Alphas.deallocate();
        Numbers.deallocate();
        cAlphaFields.deallocate();
        cNumericFields.deallocate();
        lAlphaBlanks.deallocate();
        lNumericBlanks.deallocate();

        if (ErrorsFound) {
            ShowFatalError(RoutineName + "Errors found in input. Preceding conditions cause termination.");
        }

        // Set up the output variables for low temperature radiant systems
        // ZoneHVAC:LowTemperatureRadiant:VariableFlow (HydrRadSys)
        for (Item = 1; Item <= NumOfHydrLowTempRadSys; ++Item) {

            auto &thisHydrSys (HydrRadSys(Item));

            SetupOutputVariable(
                "Zone Radiant HVAC Heating Rate", OutputProcessor::Unit::W, thisHydrSys.HeatPower, "System", "Average", thisHydrSys.Name);
            SetupOutputVariable("Zone Radiant HVAC Heating Energy",
                                OutputProcessor::Unit::J,
                                thisHydrSys.HeatEnergy,
                                "System",
                                "Sum",
                                thisHydrSys.Name,
                                _,
                                "ENERGYTRANSFER",
                                "HEATINGCOILS",
                                _,
                                "System");
            SetupOutputVariable("Zone Radiant HVAC Heating Fluid Energy",
                                OutputProcessor::Unit::J,
                                thisHydrSys.HeatEnergy,
                                "System",
                                "Sum",
                                thisHydrSys.Name,
                                _,
                                "PLANTLOOPHEATINGDEMAND",
                                "HEATINGCOILS",
                                _,
                                "System");
            SetupOutputVariable(
                "Zone Radiant HVAC Cooling Rate", OutputProcessor::Unit::W, thisHydrSys.CoolPower, "System", "Average", thisHydrSys.Name);

            SetupOutputVariable("Zone Radiant HVAC Cooling Energy",
                                OutputProcessor::Unit::J,
                                thisHydrSys.CoolEnergy,
                                "System",
                                "Sum",
                                thisHydrSys.Name,
                                _,
                                "ENERGYTRANSFER",
                                "COOLINGCOILS",
                                _,
                                "System");
            SetupOutputVariable("Zone Radiant HVAC Cooling Fluid Energy",
                                OutputProcessor::Unit::J,
                                thisHydrSys.CoolEnergy,
                                "System",
                                "Sum",
                                thisHydrSys.Name,
                                _,
                                "PLANTLOOPCOOLINGDEMAND",
                                "COOLINGCOILS",
                                _,
                                "System");
            SetupOutputVariable("Zone Radiant HVAC Mass Flow Rate",
                                OutputProcessor::Unit::kg_s,
                                thisHydrSys.WaterMassFlowRate,
                                "System",
                                "Average",
                                thisHydrSys.Name);
            SetupOutputVariable("Zone Radiant HVAC Inlet Temperature",
                                OutputProcessor::Unit::C,
                                thisHydrSys.WaterInletTemp,
                                "System",
                                "Average",
                                thisHydrSys.Name);
            SetupOutputVariable("Zone Radiant HVAC Outlet Temperature",
                                OutputProcessor::Unit::C,
                                thisHydrSys.WaterOutletTemp,
                                "System",
                                "Average",
                                thisHydrSys.Name);
            SetupOutputVariable("Zone Radiant HVAC Moisture Condensation Time",
                                OutputProcessor::Unit::s,
                                thisHydrSys.CondCausedTimeOff,
                                "System",
                                "Sum",
                                thisHydrSys.Name);
            SetupOutputVariable("Zone Radiant HVAC Operation Mode",
                                OutputProcessor::Unit::None,
                                thisHydrSys.OperatingMode,
                                "System",
                                "Average",
                                thisHydrSys.Name);
            if (AnyEnergyManagementSystemInModel) {
                SetupEMSInternalVariable("Hydronic Low Temp Radiant Design Water Volume Flow Rate for Heating",
                                         thisHydrSys.Name,
                                         "[m3/s]",
                                         thisHydrSys.WaterVolFlowMaxHeat);
                SetupEMSInternalVariable("Hydronic Low Temp Radiant Design Water Volume Flow Rate for Cooling",
                                         thisHydrSys.Name,
                                         "[m3/s]",
                                         thisHydrSys.WaterVolFlowMaxCool);
                SetupEMSActuator("Hydronic Low Temp Radiant",
                                 thisHydrSys.Name,
                                 "Water Mass Flow Rate",
                                 "[kg/s]",
                                 thisHydrSys.EMSOverrideOnWaterMdot,
                                 thisHydrSys.EMSWaterMdotOverrideValue);
            }
        }

        // Set up the output variables for low temperature radiant systems
        // ZoneHVAC:LowTemperatureRadiant:ConstantFlow (CFloRadSys)
        for (Item = 1; Item <= NumOfCFloLowTempRadSys; ++Item) {

            auto &thisCFloSys (CFloRadSys(Item));

            SetupOutputVariable(
                "Zone Radiant HVAC Heating Rate", OutputProcessor::Unit::W, thisCFloSys.HeatPower, "System", "Average", thisCFloSys.Name);
            SetupOutputVariable("Zone Radiant HVAC Heating Energy",
                                OutputProcessor::Unit::J,
                                thisCFloSys.HeatEnergy,
                                "System",
                                "Sum",
                                thisCFloSys.Name,
                                _,
                                "ENERGYTRANSFER",
                                "HEATINGCOILS",
                                _,
                                "System");
            SetupOutputVariable("Zone Radiant HVAC Heating Fluid Heat Transfer Energy",
                                OutputProcessor::Unit::J,
                                thisCFloSys.HeatEnergy,
                                "System",
                                "Sum",
                                thisCFloSys.Name,
                                _,
                                "PLANTLOOPHEATINGDEMAND",
                                "HEATINGCOILS",
                                _,
                                "System");
            SetupOutputVariable(
                "Zone Radiant HVAC Cooling Rate", OutputProcessor::Unit::W, thisCFloSys.CoolPower, "System", "Average", thisCFloSys.Name);
            SetupOutputVariable("Zone Radiant HVAC Cooling Energy",
                                OutputProcessor::Unit::J,
                                thisCFloSys.CoolEnergy,
                                "System",
                                "Sum",
                                thisCFloSys.Name,
                                _,
                                "ENERGYTRANSFER",
                                "COOLINGCOILS",
                                _,
                                "System");
            SetupOutputVariable("Zone Radiant HVAC Cooling Fluid Heat Transfer Energy",
                                OutputProcessor::Unit::J,
                                thisCFloSys.CoolEnergy,
                                "System",
                                "Sum",
                                thisCFloSys.Name,
                                _,
                                "PLANTLOOPCOOLINGDEMAND",
                                "COOLINGCOILS",
                                _,
                                "System");
            SetupOutputVariable("Zone Radiant HVAC Mass Flow Rate",
                                OutputProcessor::Unit::kg_s,
                                thisCFloSys.WaterMassFlowRate,
                                "System",
                                "Average",
                                thisCFloSys.Name);
            SetupOutputVariable("Zone Radiant HVAC Injection Mass Flow Rate",
                                OutputProcessor::Unit::kg_s,
                                thisCFloSys.WaterInjectionRate,
                                "System",
                                "Average",
                                thisCFloSys.Name);
            SetupOutputVariable("Zone Radiant HVAC Recirculation Mass Flow Rate",
                                OutputProcessor::Unit::kg_s,
                                thisCFloSys.WaterRecircRate,
                                "System",
                                "Average",
                                thisCFloSys.Name);
            SetupOutputVariable("Zone Radiant HVAC Inlet Temperature",
                                OutputProcessor::Unit::C,
                                thisCFloSys.WaterInletTemp,
                                "System",
                                "Average",
                                thisCFloSys.Name);
            SetupOutputVariable("Zone Radiant HVAC Outlet Temperature",
                                OutputProcessor::Unit::C,
                                thisCFloSys.WaterOutletTemp,
                                "System",
                                "Average",
                                thisCFloSys.Name);
            SetupOutputVariable("Zone Radiant HVAC Pump Inlet Temperature",
                                OutputProcessor::Unit::C,
                                thisCFloSys.PumpInletTemp,
                                "System",
                                "Average",
                                thisCFloSys.Name);
            SetupOutputVariable("Zone Radiant HVAC Pump Electricity Rate",
                                OutputProcessor::Unit::W,
                                thisCFloSys.PumpPower,
                                "System",
                                "Average",
                                thisCFloSys.Name);
            SetupOutputVariable("Zone Radiant HVAC Pump Electricity Energy",
                                OutputProcessor::Unit::J,
                                thisCFloSys.PumpEnergy,
                                "System",
                                "Sum",
                                thisCFloSys.Name,
                                _,
                                "Electricity",
                                "Pumps",
                                _,
                                "Plant");
            SetupOutputVariable("Zone Radiant HVAC Pump Mass Flow Rate",
                                OutputProcessor::Unit::kg_s,
                                thisCFloSys.PumpMassFlowRate,
                                "System",
                                "Average",
                                thisCFloSys.Name);
            SetupOutputVariable("Zone Radiant HVAC Pump Fluid Heat Gain Rate",
                                OutputProcessor::Unit::W,
                                thisCFloSys.PumpHeattoFluid,
                                "System",
                                "Average",
                                thisCFloSys.Name);
            SetupOutputVariable("Zone Radiant HVAC Pump Fluid Heat Gain Energy",
                                OutputProcessor::Unit::J,
                                thisCFloSys.PumpHeattoFluidEnergy,
                                "System",
                                "Sum",
                                thisCFloSys.Name);
            SetupOutputVariable("Zone Radiant HVAC Moisture Condensation Time",
                                OutputProcessor::Unit::s,
                                thisCFloSys.CondCausedTimeOff,
                                "System",
                                "Sum",
                                thisCFloSys.Name);
            SetupOutputVariable("Zone Radiant HVAC Operation Mode",
                                OutputProcessor::Unit::None,
                                thisCFloSys.OperatingMode,
                                "System",
                                "Average",
                                thisCFloSys.Name);
            if (anyRadiantSystemUsingRunningMeanAverage) {
                SetupOutputVariable("Zone Radiant HVAC Running Mean Outdoor Dry-Bulb Temperature",
                                    OutputProcessor::Unit::C,
                                    thisCFloSys.todayRunningMeanOutdoorDryBulbTemperature,
                                    "System",
                                    "Average",
                                    thisCFloSys.Name);
                SetupOutputVariable("Zone Radiant HVAC Previous Day Running Mean Outdoor Dry-Bulb Temperature",
                                    OutputProcessor::Unit::C,
                                    thisCFloSys.yesterdayRunningMeanOutdoorDryBulbTemperature,
                                    "System",
                                    "Average",
                                    thisCFloSys.Name);
                SetupOutputVariable("Zone Radiant HVAC Previous Day Average Outdoor Dry-Bulb Temperature",
                                    OutputProcessor::Unit::C,
                                    thisCFloSys.yesterdayAverageOutdoorDryBulbTemperature,
                                    "System",
                                    "Average",
                                    thisCFloSys.Name);
            }
            if (AnyEnergyManagementSystemInModel) {
                SetupEMSInternalVariable(
                    "Constant Flow Low Temp Radiant Design Water Mass Flow Rate", thisCFloSys.Name, "[m3/s]", thisCFloSys.WaterVolFlowMax);
                SetupEMSActuator("Constant Flow Low Temp Radiant",
                                 thisCFloSys.Name,
                                 "Water Mass Flow Rate",
                                 "[kg/s]",
                                 thisCFloSys.EMSOverrideOnWaterMdot,
                                 thisCFloSys.EMSWaterMdotOverrideValue);
            }
        }

        for (Item = 1; Item <= NumOfElecLowTempRadSys; ++Item) {
            // Set up the output variables for low temperature radiant systems
            // ZoneHVAC:LowTemperatureRadiant:Electric (ElecRadSys)

            auto &thisElecSys (ElecRadSys(Item));

            SetupOutputVariable(
                "Zone Radiant HVAC Electricity Rate", OutputProcessor::Unit::W, thisElecSys.ElecPower, "System", "Average", thisElecSys.Name);
            SetupOutputVariable("Zone Radiant HVAC Electricity Energy",
                                OutputProcessor::Unit::J,
                                thisElecSys.ElecEnergy,
                                "System",
                                "Sum",
                                thisElecSys.Name,
                                _,
                                "ELECTRICITY",
                                "Heating",
                                _,
                                "System");
            SetupOutputVariable(
                "Zone Radiant HVAC Heating Rate", OutputProcessor::Unit::W, thisElecSys.HeatPower, "System", "Average", thisElecSys.Name);
            SetupOutputVariable("Zone Radiant HVAC Heating Energy",
                                OutputProcessor::Unit::J,
                                thisElecSys.HeatEnergy,
                                "System",
                                "Sum",
                                thisElecSys.Name,
                                _,
                                "ENERGYTRANSFER",
                                "HEATINGCOILS",
                                _,
                                "System");
        }
    }

    FluidToSlabHeatTransferTypes HydronicSystemBaseData::getFluidToSlabHeatTransferInput(std::string const userInput)
    {
        if (UtilityRoutines::SameString(userInput, "ConvectionOnly")) {
            return FluidToSlabHeatTransferTypes::ConvectionOnly;
        } else if (UtilityRoutines::SameString(userInput, "ISOStandard")) {
            return FluidToSlabHeatTransferTypes::ISOStandard;
        } else {
            ShowWarningError("Invalid Fluid to Slab Heat Transfer Model Input = " + userInput);
            ShowContinueError("Occurs in Low Temperature Radiant System = " + this->Name);
            ShowContinueError("Heat transfer model reset to convection only for this Low Temperature Radiant System.");
            return FluidToSlabHeatTransferTypes::ConvectionOnly;
        }
    }

    LowTempRadiantControlTypes RadiantSystemBaseData::processRadiantSystemControlInput(std::string const& controlInput,
                                                                std::string const& controlInputField,
                                                                int const& typeOfRadiantSystem)
    {
        if (UtilityRoutines::SameString(controlInput, "MeanAirTemperature")) {
            return LowTempRadiantControlTypes::MATControl;
        } else if (UtilityRoutines::SameString(controlInput, "MeanRadiantTemperature")) {
            return LowTempRadiantControlTypes::MRTControl;
        } else if (UtilityRoutines::SameString(controlInput, "OperativeTemperature")) {
            return LowTempRadiantControlTypes::OperativeControl;
        } else if (UtilityRoutines::SameString(controlInput, "OutdoorDryBulbTemperature")) {
            return LowTempRadiantControlTypes::ODBControl;
        } else if (UtilityRoutines::SameString(controlInput, "OutdoorWetBulbTemperature")) {
            return LowTempRadiantControlTypes::OWBControl;
        } else if (UtilityRoutines::SameString(controlInput, "SurfaceFaceTemperature")) {
            return LowTempRadiantControlTypes::SurfFaceTempControl;
        } else if (UtilityRoutines::SameString(controlInput, "SurfaceInteriorTemperature")) {
             return LowTempRadiantControlTypes::SurfIntTempControl;
        } else if (UtilityRoutines::SameString(controlInput, "RunningMeanOutdoorDryBulbTemperature") && typeOfRadiantSystem == ConstantFlowSystem) {
            anyRadiantSystemUsingRunningMeanAverage = true;
            return LowTempRadiantControlTypes::RunningMeanODBControl;
        } else {
            ShowWarningError("Invalid " + controlInputField + " = " + controlInput);
            ShowContinueError("Occurs in Low Temperature Radiant System = " + this->Name);
            ShowContinueError("Control reset to MAT control for this Low Temperature Radiant System.");
            return LowTempRadiantControlTypes::MATControl;
        }
    }

    LowTempRadiantSetpointTypes RadiantSystemBaseData::processRadiantSystemSetpointInput(std::string const& controlInput,
                                                                                         std::string const& controlInputField)
    {
        if (UtilityRoutines::SameString(controlInput, "HalfFlowPower")) {
            return LowTempRadiantSetpointTypes::halfFlowPower;
        } else if (UtilityRoutines::SameString(controlInput, "ZeroFlowPower")) {
            return LowTempRadiantSetpointTypes::zeroFlowPower;
        } else {
            ShowWarningError("Invalid " + controlInputField + " = " + controlInput);
            ShowContinueError("Occurs in Low Temperature Radiant System = " + this->Name);
            ShowContinueError("Setpoint type reset to HalfFlowPower for this Low Temperature Radiant System.");
            return LowTempRadiantSetpointTypes::halfFlowPower;
        }

    }

    void RadiantSystemBaseData::errorCheckZonesAndConstructions(bool &errorsFound)
    {
        Real64 zoneMultipliers = 0.0;
        Real64 zoneMultipliersSurface = 0.0;
        Real64 zoneMultiplersTolerance = 0.001;
        for (int SurfNum = 1; SurfNum <= this->NumOfSurfaces; ++SurfNum) {

            if (this->SurfacePtr(SurfNum) == 0) continue; // invalid surface -- detected earlier

            if (DataGlobals::DisplayExtraWarnings) {
                // check zone numbers--ok if they are not the same
                // group warning issued earlier, show detailed warning here
                if (Surface(this->SurfacePtr(SurfNum)).Zone != this->ZonePtr) {
                    ShowWarningError("A surface referenced in a Low Temperature Radiant System is not in same zone as the radiant system itself");
                    ShowContinueError("Surface = " + Surface(this->SurfacePtr(SurfNum)).Name);
                    ShowContinueError("Surface in Zone = " + DataHeatBalance::Zone(Surface(this->SurfacePtr(SurfNum)).Zone).Name +
                        ". Radiant System in Zone = " + this->ZoneName);
                    ShowContinueError("Occurs in Low Temperature Radiant System = " + this->Name);
                    ShowContinueError("If this is intentionally a radiant system with surfaces in more than one thermal zone,");
                    ShowContinueError("then ignore this warning message.  Otherwise, check the surfaces in this radiant system.");
                }
            }

            // check zone multipliers--these must be the same
            if (SurfNum == 1) zoneMultipliers = double(DataHeatBalance::Zone(this->ZonePtr).Multiplier) *double(DataHeatBalance::Zone(this->ZonePtr).ListMultiplier);
            zoneMultipliersSurface = double(DataHeatBalance::Zone(Surface(this->SurfacePtr(SurfNum)).Zone).Multiplier)
                                            * double(DataHeatBalance::Zone(Surface(this->SurfacePtr(SurfNum)).Zone).ListMultiplier);
            if (std::abs(zoneMultipliers-zoneMultipliersSurface)>zoneMultiplersTolerance) {
                ShowSevereError("The zone multipliers are not the same for all surfaces contained in this radiant system");
                ShowContinueError("This is not allowed and must be fixed for the simulation to run.");
                ShowContinueError("Occurs in Low Temperature Radiant System = " + this->Name);
                errorsFound = true;
            }

            // make sure that this construction is defined with a source/sink--this must be the case or it can't serve as a radiant system surface
            if (!dataConstruction.Construct(Surface(this->SurfacePtr(SurfNum)).Construction).SourceSinkPresent) {
                ShowSevereError("Construction referenced in Radiant System Surface does not have a source/sink present");
                ShowContinueError("Surface name= " + Surface(this->SurfacePtr(SurfNum)).Name +
                                  "  Construction name = " + dataConstruction.Construct(Surface(this->SurfacePtr(SurfNum)).Construction).Name);
                ShowContinueError("Construction needs to be defined with a \"Construction:InternalSource\" object.");
                errorsFound = true;
            }
        }
    }

    void InitLowTempRadiantSystem(EnergyPlusData &state, bool const FirstHVACIteration, // TRUE if 1st HVAC simulation of system timestep
                                  int const RadSysNum,  // Index for the low temperature radiant system under consideration within the derived types
                                  int const SystemType, // Type of radiant system: hydronic, constant flow, or electric
                                  bool &InitErrorsFound)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Rick Strand
        //       DATE WRITTEN   November 2000

        // Using/Aliasing
        using DataGlobals::AnyPlantInModel;
        using DataGlobals::BeginEnvrnFlag;
        using DataGlobals::NumOfZones;
        using DataPlant::PlantLoop;
        using DataPlant::TypeOf_LowTempRadiant_ConstFlow;
        using DataPlant::TypeOf_LowTempRadiant_VarFlow;
        using DataSizing::AutoSize;
        using DataZoneEquipment::CheckZoneEquipmentList;
        using DataZoneEquipment::ZoneEquipInputsFilled;
        using FluidProperties::GetDensityGlycol;
        using General::RoundSigDigits;
        using PlantUtilities::InitComponentNodes;
        using PlantUtilities::ScanPlantLoopsForObject;
        using PlantUtilities::SetComponentFlowRate;
        using ScheduleManager::GetCurrentScheduleValue;

        // SUBROUTINE PARAMETER DEFINITIONS:
        Real64 const ZeroTol(0.0000001); // Smallest non-zero value allowed
        static std::string const RoutineName("InitLowTempRadiantSystem");


        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        Real64 CurrentFlowSchedule; // Schedule value for flow fraction in a constant flow radiant system
        int RadNum;                 // Number of the radiant system (DO loop counter)
        int RadSurfNum;             // Number of the radiant system surface (DO loop counter)
        int SurfNum;                // Intermediate variable for keeping track of the surface number
        Real64 TotalEffic;          // Intermediate calculation variable for total pump efficiency
        int ZoneNum;                // Intermediate variable for keeping track of the zone number
        static Array1D_bool MyEnvrnFlagHydr;
        static Array1D_bool MyEnvrnFlagCFlo;
        static Array1D_bool MyEnvrnFlagElec;
        int Loop;
        static Array1D_bool MyPlantScanFlagHydr;
        static Array1D_bool MyPlantScanFlagCFlo;
        Real64 mdot; // local fluid mass flow rate
        Real64 rho;  // local fluid density
        bool errFlag;

        InitErrorsFound = false;

        if (MyOneTimeFlag) {
            MyEnvrnFlagHydr.allocate(NumOfHydrLowTempRadSys);
            MyEnvrnFlagCFlo.allocate(NumOfCFloLowTempRadSys);
            MyEnvrnFlagElec.allocate(NumOfElecLowTempRadSys);
            MyPlantScanFlagHydr.allocate(NumOfHydrLowTempRadSys);
            MyPlantScanFlagCFlo.allocate(NumOfCFloLowTempRadSys);
            MyPlantScanFlagHydr = true;
            MyPlantScanFlagCFlo = true;
            MyEnvrnFlagHydr = true;
            MyEnvrnFlagCFlo = true;
            MyEnvrnFlagElec = true;
            MyOneTimeFlag = false;
        }

        if (FirstTimeInit) {

            ZeroSourceSumHATsurf.dimension(NumOfZones, 0.0);
            QRadSysSrcAvg.dimension(TotSurfaces, 0.0);
            LastQRadSysSrc.dimension(TotSurfaces, 0.0);
            LastSysTimeElapsed.dimension(TotSurfaces, 0.0);
            LastTimeStepSys.dimension(TotSurfaces, 0.0);
            MySizeFlagHydr.allocate(NumOfHydrLowTempRadSys);
            MySizeFlagCFlo.allocate(NumOfCFloLowTempRadSys);
            MySizeFlagElec.allocate(NumOfElecLowTempRadSys);
            MySizeFlagHydr = true;
            MySizeFlagCFlo = true;
            MySizeFlagElec = true;

            // Initialize total areas for all radiant systems
            for (RadNum = 1; RadNum <= NumOfHydrLowTempRadSys; ++RadNum) {
                HydrRadSys(RadNum).TotalSurfaceArea = 0.0;
                for (SurfNum = 1; SurfNum <= HydrRadSys(RadNum).NumOfSurfaces; ++SurfNum) {
                    HydrRadSys(RadNum).TotalSurfaceArea += Surface(HydrRadSys(RadNum).SurfacePtr(SurfNum)).Area;
                }
            }
            for (RadNum = 1; RadNum <= NumOfCFloLowTempRadSys; ++RadNum) {
                CFloRadSys(RadNum).TotalSurfaceArea = 0.0;
                for (SurfNum = 1; SurfNum <= CFloRadSys(RadNum).NumOfSurfaces; ++SurfNum) {
                    CFloRadSys(RadNum).TotalSurfaceArea += Surface(CFloRadSys(RadNum).SurfacePtr(SurfNum)).Area;
                }
            }
            for (RadNum = 1; RadNum <= NumOfElecLowTempRadSys; ++RadNum) {
                ElecRadSys(RadNum).TotalSurfaceArea = 0.0;
                for (SurfNum = 1; SurfNum <= ElecRadSys(RadNum).NumOfSurfaces; ++SurfNum) {
                    ElecRadSys(RadNum).TotalSurfaceArea += Surface(ElecRadSys(RadNum).SurfacePtr(SurfNum)).Area;
                }
            }

            // Check pump parameters for constant flow hydronic radiant systems
            for (RadNum = 1; RadNum <= NumOfCFloLowTempRadSys; ++RadNum) {
                // Calculate the efficiency for each pump: The calculation
                // is based on the PMPSIM code in the ASHRAE Secondary Toolkit
                if ((CFloRadSys(RadNum).NomPowerUse > ZeroTol) && (CFloRadSys(RadNum).MotorEffic > ZeroTol) &&
                    (CFloRadSys(RadNum).WaterVolFlowMax != AutoSize)) {
                    TotalEffic = CFloRadSys(RadNum).WaterVolFlowMax * CFloRadSys(RadNum).NomPumpHead / CFloRadSys(RadNum).NomPowerUse;
                    CFloRadSys(RadNum).PumpEffic = TotalEffic / CFloRadSys(RadNum).MotorEffic;
                    static constexpr auto fmt = "Check input.  Calc Pump Efficiency={:.5R}% {}, for pump in radiant system {}";
                    Real64 pumpEfficiency = CFloRadSys(RadNum).PumpEffic * 100.0;
                    if (CFloRadSys(RadNum).PumpEffic < 0.50) {
                        ShowWarningError(format(fmt, pumpEfficiency, "which is less than 50%", CFloRadSys(RadNum).Name));
                    } else if ((CFloRadSys(RadNum).PumpEffic > 0.95) && (CFloRadSys(RadNum).PumpEffic <= 1.0)) {
                        ShowWarningError(format(fmt, pumpEfficiency, "is approaching 100%", CFloRadSys(RadNum).Name));
                    } else if (CFloRadSys(RadNum).PumpEffic > 1.0) {
                        ShowSevereError(format(fmt, pumpEfficiency, "which is bigger than 100%", CFloRadSys(RadNum).Name));
                        InitErrorsFound = true;
                    }
                } else {
                    if (CFloRadSys(RadNum).WaterVolFlowMax !=
                        AutoSize) { // Autosize is not an error but it does not need to check pump efficiency here
                        ShowSevereError("Check input.  Pump nominal power and motor efficiency cannot be 0, for pump=" + CFloRadSys(RadNum).Name);
                        InitErrorsFound = true;
                    }
                }
            }

            FirstTimeInit = false;
        }

        if (SystemType == HydronicSystem) {
            if (MyPlantScanFlagHydr(RadSysNum) && allocated(PlantLoop)) {
                errFlag = false;
                if (HydrRadSys(RadSysNum).HotWaterInNode > 0) {
                    ScanPlantLoopsForObject(state,
                                            HydrRadSys(RadSysNum).Name,
                                            TypeOf_LowTempRadiant_VarFlow,
                                            HydrRadSys(RadSysNum).HWLoopNum,
                                            HydrRadSys(RadSysNum).HWLoopSide,
                                            HydrRadSys(RadSysNum).HWBranchNum,
                                            HydrRadSys(RadSysNum).HWCompNum,
                                            errFlag,
                                            _,
                                            _,
                                            _,
                                            HydrRadSys(RadSysNum).HotWaterInNode,
                                            _);
                    if (errFlag) {
                        ShowFatalError("InitLowTempRadiantSystem: Program terminated due to previous condition(s).");
                    }
                }
                if (HydrRadSys(RadSysNum).ColdWaterInNode > 0) {
                    ScanPlantLoopsForObject(state,
                                            HydrRadSys(RadSysNum).Name,
                                            TypeOf_LowTempRadiant_VarFlow,
                                            HydrRadSys(RadSysNum).CWLoopNum,
                                            HydrRadSys(RadSysNum).CWLoopSide,
                                            HydrRadSys(RadSysNum).CWBranchNum,
                                            HydrRadSys(RadSysNum).CWCompNum,
                                            errFlag,
                                            _,
                                            _,
                                            _,
                                            HydrRadSys(RadSysNum).ColdWaterInNode,
                                            _);
                    if (errFlag) {
                        ShowFatalError("InitLowTempRadiantSystem: Program terminated due to previous condition(s).");
                    }
                }
                MyPlantScanFlagHydr(RadSysNum) = false;
            } else if (MyPlantScanFlagHydr(RadSysNum) && !AnyPlantInModel) {
                MyPlantScanFlagHydr(RadSysNum) = false;
            }
        }

        if (SystemType == ConstantFlowSystem) {
            if (MyPlantScanFlagCFlo(RadSysNum) && allocated(PlantLoop)) {
                errFlag = false;
                if (CFloRadSys(RadSysNum).HotWaterInNode > 0) {
                    ScanPlantLoopsForObject(state,
                                            CFloRadSys(RadSysNum).Name,
                                            TypeOf_LowTempRadiant_ConstFlow,
                                            CFloRadSys(RadSysNum).HWLoopNum,
                                            CFloRadSys(RadSysNum).HWLoopSide,
                                            CFloRadSys(RadSysNum).HWBranchNum,
                                            CFloRadSys(RadSysNum).HWCompNum,
                                            errFlag,
                                            _,
                                            _,
                                            _,
                                            CFloRadSys(RadSysNum).HotWaterInNode,
                                            _);
                    if (errFlag) {
                        ShowFatalError("InitLowTempRadiantSystem: Program terminated due to previous condition(s).");
                    }
                }
                if (CFloRadSys(RadSysNum).ColdWaterInNode > 0) {
                    ScanPlantLoopsForObject(state,
                                            CFloRadSys(RadSysNum).Name,
                                            TypeOf_LowTempRadiant_ConstFlow,
                                            CFloRadSys(RadSysNum).CWLoopNum,
                                            CFloRadSys(RadSysNum).CWLoopSide,
                                            CFloRadSys(RadSysNum).CWBranchNum,
                                            CFloRadSys(RadSysNum).CWCompNum,
                                            errFlag,
                                            _,
                                            _,
                                            _,
                                            CFloRadSys(RadSysNum).ColdWaterInNode,
                                            _);
                    if (errFlag) {
                        ShowFatalError("InitLowTempRadiantSystem: Program terminated due to previous condition(s).");
                    }
                }
                MyPlantScanFlagCFlo(RadSysNum) = false;
            } else if (MyPlantScanFlagCFlo(RadSysNum) && !AnyPlantInModel) {
                MyPlantScanFlagCFlo(RadSysNum) = false;
            }
        }

        // need to check all units to see if they are on Zone Equipment List or issue warning
        if (!ZoneEquipmentListChecked && ZoneEquipInputsFilled) {
            ZoneEquipmentListChecked = true;
            for (Loop = 1; Loop <= TotalNumOfRadSystems; ++Loop) {
                {
                    auto const SELECT_CASE_var(RadSysTypes(Loop).SystemType);

                    if (SELECT_CASE_var == HydronicSystem) {
                        if (CheckZoneEquipmentList("ZoneHVAC:LowTemperatureRadiant:VariableFlow", RadSysTypes(Loop).Name)) continue;
                        ShowSevereError("InitLowTempRadiantSystem: Unit=[ZoneHVAC:LowTemperatureRadiant:VariableFlow," + RadSysTypes(Loop).Name +
                                        "] is not on any ZoneHVAC:EquipmentList.  It will not be simulated.");
                    } else if (SELECT_CASE_var == ConstantFlowSystem) {
                        if (CheckZoneEquipmentList("ZoneHVAC:LowTemperatureRadiant:ConstantFlow", RadSysTypes(Loop).Name)) continue;
                        ShowSevereError("InitLowTempRadiantSystem: Unit=[ZoneHVAC:LowTemperatureRadiant:ConstantFlow," + RadSysTypes(Loop).Name +
                                        "] is not on any ZoneHVAC:EquipmentList.  It will not be simulated.");
                    } else if (SELECT_CASE_var == ElectricSystem) {
                        if (CheckZoneEquipmentList("ZoneHVAC:LowTemperatureRadiant:Electric", RadSysTypes(Loop).Name)) continue;
                        ShowSevereError("InitLowTempRadiantSystem: Unit=[ZoneHVAC:LowTemperatureRadiant:Electric," + RadSysTypes(Loop).Name +
                                        "] is not on any ZoneHVAC:EquipmentList.  It will not be simulated.");
                    } else { // Illegal system, but checked earlier
                    }
                }
            }
        }

        if (!SysSizingCalc && (SystemType == HydronicSystem)) {
            if (MySizeFlagHydr(RadSysNum) && !MyPlantScanFlagHydr(RadSysNum)) {
                // for each radiant system do the sizing once.
                SizeLowTempRadiantSystem(state, RadSysNum, SystemType);
                MySizeFlagHydr(RadSysNum) = false;

                // Can this system actually do cooling?
                if ((HydrRadSys(RadSysNum).WaterVolFlowMaxCool > 0.0) && (HydrRadSys(RadSysNum).ColdWaterInNode > 0) &&
                    (HydrRadSys(RadSysNum).ColdWaterOutNode > 0) && (HydrRadSys(RadSysNum).ColdSetptSchedPtr > 0)) {
                    HydrRadSys(RadSysNum).CoolingSystem = true;
                }

                // Can this system actually do heating?
                if ((HydrRadSys(RadSysNum).WaterVolFlowMaxHeat > 0.0) && (HydrRadSys(RadSysNum).HotWaterInNode > 0) &&
                    (HydrRadSys(RadSysNum).HotWaterOutNode > 0) && (HydrRadSys(RadSysNum).HotSetptSchedPtr > 0)) {
                    HydrRadSys(RadSysNum).HeatingSystem = true;
                }

                // set design mass flow rates
                if (HydrRadSys(RadSysNum).HotWaterInNode > 0) {
                    rho = GetDensityGlycol(PlantLoop(HydrRadSys(RadSysNum).HWLoopNum).FluidName,
                                           DataGlobals::HWInitConvTemp,
                                           PlantLoop(HydrRadSys(RadSysNum).HWLoopNum).FluidIndex,
                                           RoutineName);
                    HydrRadSys(RadSysNum).WaterFlowMaxHeat = rho * HydrRadSys(RadSysNum).WaterVolFlowMaxHeat;
                    InitComponentNodes(0.0,
                                       HydrRadSys(RadSysNum).WaterFlowMaxHeat,
                                       HydrRadSys(RadSysNum).HotWaterInNode,
                                       HydrRadSys(RadSysNum).HotWaterOutNode,
                                       HydrRadSys(RadSysNum).HWLoopNum,
                                       HydrRadSys(RadSysNum).HWLoopSide,
                                       HydrRadSys(RadSysNum).HWBranchNum,
                                       HydrRadSys(RadSysNum).HWCompNum);
                }
                if (HydrRadSys(RadSysNum).ColdWaterInNode > 0) {
                    rho = GetDensityGlycol(PlantLoop(HydrRadSys(RadSysNum).CWLoopNum).FluidName,
                                           DataGlobals::CWInitConvTemp,
                                           PlantLoop(HydrRadSys(RadSysNum).CWLoopNum).FluidIndex,
                                           RoutineName);
                    HydrRadSys(RadSysNum).WaterFlowMaxCool = rho * HydrRadSys(RadSysNum).WaterVolFlowMaxCool;
                    InitComponentNodes(0.0,
                                       HydrRadSys(RadSysNum).WaterFlowMaxCool,
                                       HydrRadSys(RadSysNum).ColdWaterInNode,
                                       HydrRadSys(RadSysNum).ColdWaterOutNode,
                                       HydrRadSys(RadSysNum).CWLoopNum,
                                       HydrRadSys(RadSysNum).CWLoopSide,
                                       HydrRadSys(RadSysNum).CWBranchNum,
                                       HydrRadSys(RadSysNum).CWCompNum);
                }
            }
        }

        if (!SysSizingCalc && (SystemType == ConstantFlowSystem)) {
            if (MySizeFlagCFlo(RadSysNum) && !MyPlantScanFlagCFlo(RadSysNum)) {
                // for each radiant system do the sizing once.
                SizeLowTempRadiantSystem(state, RadSysNum, SystemType);

                // set design mass flow rates
                if (CFloRadSys(RadSysNum).HotWaterInNode > 0) {
                    rho = GetDensityGlycol(PlantLoop(CFloRadSys(RadSysNum).HWLoopNum).FluidName,
                                           DataGlobals::HWInitConvTemp,
                                           PlantLoop(CFloRadSys(RadSysNum).HWLoopNum).FluidIndex,
                                           RoutineName);
                    CFloRadSys(RadSysNum).HotDesignWaterMassFlowRate = rho * CFloRadSys(RadSysNum).WaterVolFlowMax;
                    InitComponentNodes(0.0,
                                       CFloRadSys(RadSysNum).HotDesignWaterMassFlowRate,
                                       CFloRadSys(RadSysNum).HotWaterInNode,
                                       CFloRadSys(RadSysNum).HotWaterOutNode,
                                       CFloRadSys(RadSysNum).HWLoopNum,
                                       CFloRadSys(RadSysNum).HWLoopSide,
                                       CFloRadSys(RadSysNum).HWBranchNum,
                                       CFloRadSys(RadSysNum).HWCompNum);
                }
                if (CFloRadSys(RadSysNum).ColdWaterInNode > 0) {
                    rho = GetDensityGlycol(PlantLoop(CFloRadSys(RadSysNum).CWLoopNum).FluidName,
                                           DataGlobals::CWInitConvTemp,
                                           PlantLoop(CFloRadSys(RadSysNum).CWLoopNum).FluidIndex,
                                           RoutineName);
                    CFloRadSys(RadSysNum).ColdDesignWaterMassFlowRate = rho * CFloRadSys(RadSysNum).WaterVolFlowMax;
                    InitComponentNodes(0.0,
                                       CFloRadSys(RadSysNum).ColdDesignWaterMassFlowRate,
                                       CFloRadSys(RadSysNum).ColdWaterInNode,
                                       CFloRadSys(RadSysNum).ColdWaterOutNode,
                                       CFloRadSys(RadSysNum).CWLoopNum,
                                       CFloRadSys(RadSysNum).CWLoopSide,
                                       CFloRadSys(RadSysNum).CWBranchNum,
                                       CFloRadSys(RadSysNum).CWCompNum);
                }
                MySizeFlagCFlo(RadSysNum) = false;
            }
        }

        if (!SysSizingCalc && (SystemType == ElectricSystem)) {
            if (MySizeFlagElec(RadSysNum)) {
                // for each radiant system do the sizing once.
                SizeLowTempRadiantSystem(state, RadSysNum, SystemType);
                MySizeFlagElec(RadSysNum) = false;
            }
        }

        if (BeginEnvrnFlag && MyEnvrnFlagGeneral) {
            ZeroSourceSumHATsurf = 0.0;
            QRadSysSrcAvg = 0.0;
            LastQRadSysSrc = 0.0;
            LastSysTimeElapsed = 0.0;
            LastTimeStepSys = 0.0;
            MyEnvrnFlagGeneral = false;
        }
        if (!BeginEnvrnFlag) MyEnvrnFlagGeneral = true;

        // If we are at the beginning of a new environment OR the warmup period is done and the simulation is starting,
        // then the various changeover variables need to be reset so that we are starting from scratch.
        if ( (BeginEnvrnFlag && FirstHVACIteration) ||
             (!WarmupFlag && DataGlobals::BeginDayFlag && FirstHVACIteration && DataGlobals::DayOfSim == 1) ) {
            // Reset values related to changeover
            if (SystemType == HydronicSystem) {
                HydrRadSys(RadSysNum).lastOperatingMode = NotOperating;
                HydrRadSys(RadSysNum).lastDayOfSim = 0;
                HydrRadSys(RadSysNum).lastHourOfDay = 0;
                HydrRadSys(RadSysNum).lastTimeStep = 0;
            }
            if (SystemType == ConstantFlowSystem) {
                CFloRadSys(RadSysNum).lastOperatingMode = NotOperating;
                CFloRadSys(RadSysNum).lastDayOfSim = 0;
                CFloRadSys(RadSysNum).lastHourOfDay = 0;
                CFloRadSys(RadSysNum).lastTimeStep = 0;
            }
        }

        if (SystemType == HydronicSystem) {
            if (BeginEnvrnFlag && MyEnvrnFlagHydr(RadSysNum)) {
                HydrRadSys(RadSysNum).HeatPower = 0.0;
                HydrRadSys(RadSysNum).HeatEnergy = 0.0;
                HydrRadSys(RadSysNum).CoolPower = 0.0;
                HydrRadSys(RadSysNum).CoolEnergy = 0.0;
                HydrRadSys(RadSysNum).WaterInletTemp = 0.0;
                HydrRadSys(RadSysNum).WaterOutletTemp = 0.0;
                HydrRadSys(RadSysNum).WaterMassFlowRate = 0.0;

                if (!MyPlantScanFlagHydr(RadSysNum)) {
                    if (HydrRadSys(RadSysNum).HotWaterInNode > 0) {
                        InitComponentNodes(0.0,
                                           HydrRadSys(RadSysNum).WaterFlowMaxHeat,
                                           HydrRadSys(RadSysNum).HotWaterInNode,
                                           HydrRadSys(RadSysNum).HotWaterOutNode,
                                           HydrRadSys(RadSysNum).HWLoopNum,
                                           HydrRadSys(RadSysNum).HWLoopSide,
                                           HydrRadSys(RadSysNum).HWBranchNum,
                                           HydrRadSys(RadSysNum).HWCompNum);
                    }
                    if (HydrRadSys(RadSysNum).ColdWaterInNode > 0) {
                        InitComponentNodes(0.0,
                                           HydrRadSys(RadSysNum).WaterFlowMaxCool,
                                           HydrRadSys(RadSysNum).ColdWaterInNode,
                                           HydrRadSys(RadSysNum).ColdWaterOutNode,
                                           HydrRadSys(RadSysNum).CWLoopNum,
                                           HydrRadSys(RadSysNum).CWLoopSide,
                                           HydrRadSys(RadSysNum).CWBranchNum,
                                           HydrRadSys(RadSysNum).CWCompNum);
                    }
                }
                MyEnvrnFlagHydr(RadSysNum) = false;
            }
        } // NumOfHydrLowTempRadSys > 0
        if (!BeginEnvrnFlag && SystemType == HydronicSystem) MyEnvrnFlagHydr(RadSysNum) = true;

        if (SystemType == ConstantFlowSystem) {
            if (BeginEnvrnFlag && MyEnvrnFlagCFlo(RadSysNum)) {
                CFloRadSys(RadSysNum).WaterInletTemp = 0.0;
                CFloRadSys(RadSysNum).WaterOutletTemp = 0.0;
                CFloRadSys(RadSysNum).PumpInletTemp = 0.0;
                CFloRadSys(RadSysNum).WaterMassFlowRate = 0.0;
                CFloRadSys(RadSysNum).WaterInjectionRate = 0.0;
                CFloRadSys(RadSysNum).WaterRecircRate = 0.0;
                CFloRadSys(RadSysNum).HeatPower = 0.0;
                CFloRadSys(RadSysNum).HeatEnergy = 0.0;
                CFloRadSys(RadSysNum).CoolPower = 0.0;
                CFloRadSys(RadSysNum).CoolEnergy = 0.0;
                CFloRadSys(RadSysNum).PumpPower = 0.0;
                CFloRadSys(RadSysNum).PumpMassFlowRate = 0.0;
                CFloRadSys(RadSysNum).PumpHeattoFluid = 0.0;

                if (!MyPlantScanFlagCFlo(RadSysNum)) {
                    if (CFloRadSys(RadSysNum).HotWaterInNode > 0) {
                        InitComponentNodes(0.0,
                                           CFloRadSys(RadSysNum).HotDesignWaterMassFlowRate,
                                           CFloRadSys(RadSysNum).HotWaterInNode,
                                           CFloRadSys(RadSysNum).HotWaterOutNode,
                                           CFloRadSys(RadSysNum).HWLoopNum,
                                           CFloRadSys(RadSysNum).HWLoopSide,
                                           CFloRadSys(RadSysNum).HWBranchNum,
                                           CFloRadSys(RadSysNum).HWCompNum);
                    }
                    if (CFloRadSys(RadSysNum).ColdWaterInNode > 0) {
                        InitComponentNodes(0.0,
                                           CFloRadSys(RadSysNum).ColdDesignWaterMassFlowRate,
                                           CFloRadSys(RadSysNum).ColdWaterInNode,
                                           CFloRadSys(RadSysNum).ColdWaterOutNode,
                                           CFloRadSys(RadSysNum).CWLoopNum,
                                           CFloRadSys(RadSysNum).CWLoopSide,
                                           CFloRadSys(RadSysNum).CWBranchNum,
                                           CFloRadSys(RadSysNum).CWCompNum);
                    }
                }
                MyEnvrnFlagCFlo(RadSysNum) = false;
            }

            if (anyRadiantSystemUsingRunningMeanAverage) {
                if (DataGlobals::BeginDayFlag && CFloRadSys(RadSysNum).setRunningMeanValuesAtBeginningOfDay) {
                    CFloRadSys(RadSysNum).calculateRunningMeanAverageTemperature();
                    CFloRadSys(RadSysNum).setRunningMeanValuesAtBeginningOfDay = false; // only set these once per system
                } else if (!DataGlobals::BeginDayFlag && !CFloRadSys(RadSysNum).setRunningMeanValuesAtBeginningOfDay) {
                    CFloRadSys(RadSysNum).setRunningMeanValuesAtBeginningOfDay = true;  // reset so that the next time BeginDayFlag is true this can get set
                }
            }

        } // NumOfCFloLowTempRadSys > 0
        if (!BeginEnvrnFlag && SystemType == ConstantFlowSystem) MyEnvrnFlagCFlo(RadSysNum) = true;

        if (SystemType == ElectricSystem) {
            if (BeginEnvrnFlag && MyEnvrnFlagElec(RadSysNum)) {
                ElecRadSys(RadSysNum).HeatPower = 0.0;
                ElecRadSys(RadSysNum).HeatEnergy = 0.0;
                ElecRadSys(RadSysNum).ElecPower = 0.0;
                ElecRadSys(RadSysNum).ElecEnergy = 0.0;
            }
            MyEnvrnFlagElec(RadSysNum) = false;
        }
        if (!BeginEnvrnFlag && SystemType == ElectricSystem) MyEnvrnFlagElec(RadSysNum) = true;

        if (SystemType == ConstantFlowSystem) {

            // Can this system actually do heating?
            if ((CFloRadSys(RadSysNum).WaterVolFlowMax > 0.0) && (CFloRadSys(RadSysNum).HotWaterInNode > 0) &&
                (CFloRadSys(RadSysNum).HotWaterOutNode > 0) && (CFloRadSys(RadSysNum).HotWaterHiTempSchedPtr > 0) &&
                (CFloRadSys(RadSysNum).HotWaterLoTempSchedPtr > 0) && (CFloRadSys(RadSysNum).HotCtrlHiTempSchedPtr > 0) &&
                (CFloRadSys(RadSysNum).HotCtrlLoTempSchedPtr > 0)) {
                CFloRadSys(RadSysNum).HeatingSystem = true;
            }

            // Can this system actually do cooling?
            if ((CFloRadSys(RadSysNum).WaterVolFlowMax > 0.0) && (CFloRadSys(RadSysNum).ColdWaterInNode > 0) &&
                (CFloRadSys(RadSysNum).ColdWaterOutNode > 0) && (CFloRadSys(RadSysNum).ColdWaterHiTempSchedPtr > 0) &&
                (CFloRadSys(RadSysNum).ColdWaterLoTempSchedPtr > 0) && (CFloRadSys(RadSysNum).ColdCtrlHiTempSchedPtr > 0) &&
                (CFloRadSys(RadSysNum).ColdCtrlLoTempSchedPtr > 0)) {
                CFloRadSys(RadSysNum).CoolingSystem = true;
            }
        }

        if (BeginTimeStepFlag && FirstHVACIteration) { // This is the first pass through in a particular time step

            {
                auto const SELECT_CASE_var(SystemType);

                if (SELECT_CASE_var == HydronicSystem) {

                    ZoneNum = HydrRadSys(RadSysNum).ZonePtr;
                    ZeroSourceSumHATsurf(ZoneNum) = SumHATsurf(ZoneNum); // Set this to figure what part of the load the radiant system meets
                    for (RadSurfNum = 1; RadSurfNum <= HydrRadSys(RadSysNum).NumOfSurfaces; ++RadSurfNum) {
                        SurfNum = HydrRadSys(RadSysNum).SurfacePtr(RadSurfNum);
                        QRadSysSrcAvg(SurfNum) = 0.0;      // Initialize this variable to zero (radiant system defaults to off)
                        LastQRadSysSrc(SurfNum) = 0.0;     // At the start of a time step, reset to zero so average calculation can begin again
                        LastSysTimeElapsed(SurfNum) = 0.0; // At the start of a time step, reset to zero so average calculation can begin again
                        LastTimeStepSys(SurfNum) = 0.0;    // At the start of a time step, reset to zero so average calculation can begin again
                    }

                } else if (SELECT_CASE_var == ConstantFlowSystem) {

                    ZoneNum = CFloRadSys(RadSysNum).ZonePtr;
                    ZeroSourceSumHATsurf(ZoneNum) = SumHATsurf(ZoneNum); // Set this to figure what part of the load the radiant system meets
                    for (RadSurfNum = 1; RadSurfNum <= CFloRadSys(RadSysNum).NumOfSurfaces; ++RadSurfNum) {
                        SurfNum = CFloRadSys(RadSysNum).SurfacePtr(RadSurfNum);
                        QRadSysSrcAvg(SurfNum) = 0.0;      // Initialize this variable to zero (radiant system defaults to off)
                        LastQRadSysSrc(SurfNum) = 0.0;     // At the start of a time step, reset to zero so average calculation can begin again
                        LastSysTimeElapsed(SurfNum) = 0.0; // At the start of a time step, reset to zero so average calculation can begin again
                        LastTimeStepSys(SurfNum) = 0.0;    // At the start of a time step, reset to zero so average calculation can begin again
                    }

                } else if (SELECT_CASE_var == ElectricSystem) {

                    ZoneNum = ElecRadSys(RadSysNum).ZonePtr;
                    ZeroSourceSumHATsurf(ZoneNum) = SumHATsurf(ZoneNum); // Set this to figure what part of the load the radiant system meets
                    for (RadSurfNum = 1; RadSurfNum <= ElecRadSys(RadSysNum).NumOfSurfaces; ++RadSurfNum) {
                        SurfNum = ElecRadSys(RadSysNum).SurfacePtr(RadSurfNum);
                        QRadSysSrcAvg(SurfNum) = 0.0;      // Initialize this variable to zero (radiant system defaults to off)
                        LastQRadSysSrc(SurfNum) = 0.0;     // At the start of a time step, reset to zero so average calculation can begin again
                        LastSysTimeElapsed(SurfNum) = 0.0; // At the start of a time step, reset to zero so average calculation can begin again
                        LastTimeStepSys(SurfNum) = 0.0;    // At the start of a time step, reset to zero so average calculation can begin again
                    }

                } else {

                    ShowSevereError("Radiant system entered without specification of type: electric, constant flow, or hydronic?");
                    ShowContinueError("Occurs in Radiant System=" + HydrRadSys(RadSysNum).Name);
                    ShowFatalError("Preceding condition causes termination.");
                }
            }

        } // ...for first pass through in a particular time step.

        {
            auto const SELECT_CASE_var(SystemType);

            if (SELECT_CASE_var == HydronicSystem) {

                // Initialize the appropriate node data
                if (HydrRadSys(RadSysNum).HeatingSystem) {
                    mdot = 0.0;
                    SetComponentFlowRate(mdot,
                                         HydrRadSys(RadSysNum).HotWaterInNode,
                                         HydrRadSys(RadSysNum).HotWaterOutNode,
                                         HydrRadSys(RadSysNum).HWLoopNum,
                                         HydrRadSys(RadSysNum).HWLoopSide,
                                         HydrRadSys(RadSysNum).HWBranchNum,
                                         HydrRadSys(RadSysNum).HWCompNum);
                }
                if (HydrRadSys(RadSysNum).CoolingSystem) {
                    mdot = 0.0;
                    SetComponentFlowRate(mdot,
                                         HydrRadSys(RadSysNum).ColdWaterInNode,
                                         HydrRadSys(RadSysNum).ColdWaterOutNode,
                                         HydrRadSys(RadSysNum).CWLoopNum,
                                         HydrRadSys(RadSysNum).CWLoopSide,
                                         HydrRadSys(RadSysNum).CWBranchNum,
                                         HydrRadSys(RadSysNum).CWCompNum);
                }
                if (HydrRadSys(RadSysNum).OperatingMode != NotOperating && FirstHVACIteration) HydrRadSys(RadSysNum).updateOperatingModeHistory();

            } else if (SELECT_CASE_var == ConstantFlowSystem) {
                CFloRadSys(RadSysNum).WaterMassFlowRate = 0.0;
                // Initialize the appropriate node data
                if (CFloRadSys(RadSysNum).HeatingSystem) {
                    if (CFloRadSys(RadSysNum).VolFlowSchedPtr > 0) {
                        CurrentFlowSchedule = GetCurrentScheduleValue(CFloRadSys(RadSysNum).VolFlowSchedPtr);
                    } else {
                        CurrentFlowSchedule = 1.0; // Allow user to avoid putting in a schedule (defaults to constant flow at all times)
                    }
                    if (CurrentFlowSchedule > 1.0) CurrentFlowSchedule = 1.0; // Do not allow more flow than design maximum
                    if (CurrentFlowSchedule < 0.0) CurrentFlowSchedule = 0.0; // Do not allow negative flow

                    CFloRadSys(RadSysNum).HotWaterMassFlowRate = CFloRadSys(RadSysNum).HotDesignWaterMassFlowRate * CurrentFlowSchedule;

                    if (CFloRadSys(RadSysNum).EMSOverrideOnWaterMdot)
                        CFloRadSys(RadSysNum).HotWaterMassFlowRate = CFloRadSys(RadSysNum).EMSWaterMdotOverrideValue;

                    if (CFloRadSys(RadSysNum).HotWaterInNode > 0)
                        SetComponentFlowRate(CFloRadSys(RadSysNum).HotWaterMassFlowRate,
                                             CFloRadSys(RadSysNum).HotWaterInNode,
                                             CFloRadSys(RadSysNum).HotWaterOutNode,
                                             CFloRadSys(RadSysNum).HWLoopNum,
                                             CFloRadSys(RadSysNum).HWLoopSide,
                                             CFloRadSys(RadSysNum).HWBranchNum,
                                             CFloRadSys(RadSysNum).HWCompNum);
                }
                if (CFloRadSys(RadSysNum).CoolingSystem) {
                    if (CFloRadSys(RadSysNum).VolFlowSchedPtr > 0) {
                        CurrentFlowSchedule = GetCurrentScheduleValue(CFloRadSys(RadSysNum).VolFlowSchedPtr);
                    } else {
                        CurrentFlowSchedule = 1.0; // Allow user to avoid putting in a schedule (defaults to constant flow at all times)
                    }
                    if (CurrentFlowSchedule > 1.0) CurrentFlowSchedule = 1.0; // Do not allow more flow than design maximum
                    if (CurrentFlowSchedule < 0.0) CurrentFlowSchedule = 0.0; // Do not allow negative flow
                    CFloRadSys(RadSysNum).ChWaterMassFlowRate = CFloRadSys(RadSysNum).ColdDesignWaterMassFlowRate * CurrentFlowSchedule;

                    if (CFloRadSys(RadSysNum).EMSOverrideOnWaterMdot)
                        CFloRadSys(RadSysNum).ChWaterMassFlowRate = CFloRadSys(RadSysNum).EMSWaterMdotOverrideValue;

                    if (CFloRadSys(RadSysNum).ColdWaterInNode > 0)
                        SetComponentFlowRate(CFloRadSys(RadSysNum).ChWaterMassFlowRate,
                                             CFloRadSys(RadSysNum).ColdWaterInNode,
                                             CFloRadSys(RadSysNum).ColdWaterOutNode,
                                             CFloRadSys(RadSysNum).CWLoopNum,
                                             CFloRadSys(RadSysNum).CWLoopSide,
                                             CFloRadSys(RadSysNum).CWBranchNum,
                                             CFloRadSys(RadSysNum).CWCompNum);
                }
                if (CFloRadSys(RadSysNum).OperatingMode != NotOperating && FirstHVACIteration) CFloRadSys(RadSysNum).updateOperatingModeHistory();

            } else if (SELECT_CASE_var == ElectricSystem) {

                ElecRadSys(RadSysNum).OperatingMode = NotOperating;

            }
        }

    }

    void HydronicSystemBaseData::updateOperatingModeHistory()
    {
        // Since this is only called when the operating mode is something other than "not operating",
        // the status from the previous system time step is what it did in the last or previous time step.
        // So, we can update the last status of the system using this information before reseting things
        // to "not operating".
        this->lastOperatingMode = this->OperatingMode;

        if (DataGlobals::BeginDayFlag) {
            // The begin day flag is set which mean this is the first time step of the day.
            // This also means that the previous time step was the last time step of yesterday.
            // So, the day should be the previous day, the hour should bethe last hour of the
            // day, and the time step should be the last time step.
            this->lastDayOfSim  = DataGlobals::DayOfSim - 1;
            this->lastHourOfDay = int(DataGlobals::HoursInDay);
            this->lastTimeStep  = DataGlobals::NumOfTimeStepInHour;
        } else if (DataGlobals::BeginHourFlag) {
            // It's not the beginning of the day but it is the beginning of an hour other than
            // the first hour.  This means that the previous time step was the previous hour of
            // today in the last time step.  So, the day should be the current day, the hour should
            // be the previous hour, and the time step should be the last time step.
            this->lastDayOfSim  = DataGlobals::DayOfSim;
            this->lastHourOfDay = DataGlobals::HourOfDay - 1;
            this->lastTimeStep  = DataGlobals::NumOfTimeStepInHour;
        } else if (DataGlobals::BeginTimeStepFlag) {
            // It's neither the beginning of the day nor the beginning of an hour but it is the start
            // of a time step other than the first time step in the hour.  So, the day should be the
            // current day, the hour should be the current hour, and the time step should be the
            // previous time step.
            this->lastDayOfSim  = DataGlobals::DayOfSim;
            this->lastHourOfDay = DataGlobals::HourOfDay;
            this->lastTimeStep  = DataGlobals::TimeStep-1;
        } else {
            // It's not the beginning of the day, hour, or time step so the "last" value is simply the
            // same as the current value.  Note that these parameters only track down to the zone time
            // step level and will make decisions based on that.
            this->lastDayOfSim  = DataGlobals::DayOfSim;
            this->lastHourOfDay = DataGlobals::HourOfDay;
            this->lastTimeStep  = DataGlobals::TimeStep;
        }

        // Now go ahead and reset the operating mode (this will be set to something else if the system is running)
        this->OperatingMode = NotOperating;
    }

    void HydronicSystemBaseData::setOperatingModeBasedOnChangeoverDelay()
    {
        if (this->lastOperatingMode == NotOperating) return; // this should only happen at the beginning of a simulation (at the start of warmup and the actual simulation)
                                                             // so let things proceed with whatever the system wants to do

        if (this->OperatingMode == NotOperating) return;  // always let it turn off

        if (this->OperatingMode == this->lastOperatingMode) return; // always let it continue to operating in the same mode

        if (this->schedPtrChangeoverDelay == 0) return; // user not requesting any delays (no schedule entered) so let it do whatever is requested

        Real64 currentChangeoverDelay = ScheduleManager::GetCurrentScheduleValue(this->schedPtrChangeoverDelay);
        if (currentChangeoverDelay <= 0.0) return;  // delay is zero so let it do whatever it requested

        // At this point, the radiant system is trying to switch modes from the previous time step, the user is requesting a delay in the changeover,
        // and the requested delay is greater than zero.  Calculate what the current time is in hours from the start of the simulation
        Real64 timeCurrent = 24.0*float(DataGlobals::DayOfSim-1) + float(DataGlobals::HourOfDay-1) + float(DataGlobals::TimeStep-1)/float(DataGlobals::NumOfTimeStepInHour);
        Real64 timeLast = 24.0*float(this->lastDayOfSim-1) + float(this->lastHourOfDay-1) + float(this->lastTimeStep-1)/float(DataGlobals::NumOfTimeStepInHour);
        Real64 actualTimeDifference = timeCurrent - timeLast;

        // If the time difference is not longer than the user delay, then the system should not switch modes and needs to be turned off.
        if (actualTimeDifference <= currentChangeoverDelay) this->OperatingMode = NotOperating;

        // Note: if the time difference is greater than the user delay request, then go ahead and keep the operating mode needed (don't do anything).

    }


    void SizeLowTempRadiantSystem(EnergyPlusData &state, int const RadSysNum, // Index for the low temperature radiant system under consideration within the derived types
                                  int const SystemType // Type of radiant system: hydronic, constant flow, or electric
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Fred Buhl
        //       DATE WRITTEN   February 2002
        //       MODIFIED       August 2013 Daeho Kang, add component sizing table entries
        //                      August 2014 Bereket Nigusse, added scalable sizing
        //                      March 2014 Daeho Kang, add constant flow system autosizing

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine is for sizing low temperature radiant components for which flow rates
        // and tube length or max electric power have not been specified in the input

        // METHODOLOGY EMPLOYED:
        // Obtains flow rates from the zone sizing arrays and plant sizing data. Maximum electric
        // power is set to the design heat load. Tube length is calculated by rule-of-thumb from
        // the surface area.

        // Using/Aliasing
        using namespace DataSizing;
        using DataHeatBalance::Zone;
        using DataHVACGlobals::AutoCalculateSizing;
        using DataHVACGlobals::CoolingCapacitySizing;
        using DataHVACGlobals::HeatingCapacitySizing;
        using DataPlant::PlantLoop;
        using FluidProperties::GetDensityGlycol;
        using FluidProperties::GetSpecificHeatGlycol;
        using General::RoundSigDigits;
        using PlantUtilities::MyPlantSizingIndex;
        using PlantUtilities::RegisterPlantCompDesignFlow;
        using ReportSizingManager::ReportSizingOutput;
        using ReportSizingManager::RequestSizing;

        // SUBROUTINE PARAMETER DEFINITIONS:
        static std::string const RoutineName("SizeLowTempRadiantSystem");
        static int const OFF = 0;
        static int const ClgHtg = 1;
        static int const ClgOnly = 2;
        static int const HtgOnly = 3;

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int PltSizHeatNum(0);    // index of plant sizing object for 1st heating loop
        int PltSizCoolNum(0);    // index of plant sizing object for 1st cooling loop
        int SurfNum;             // surface index in radiant system data structure
        bool ErrorsFound(false); // If errors detected in input
        Real64 rho;
        Real64 Cp;
        bool IsAutoSize(false); // Indicator to autosize
        Real64 WaterVolFlowMaxHeatDes(0.0);  // Design hot water flow for reproting
        Real64 WaterVolFlowMaxHeatUser(0.0); // User hard-sized hot water flow for
        Real64 WaterVolFlowMaxCoolDes(0.0);  // Design chilled water flow for reproting
        Real64 WaterVolFlowMaxCoolUser(0.0); // User hard-sized chilled water flow for reproting
        Real64 TubeLengthDes(0.0);           // Design tube length for reproting
        Real64 TubeLengthUser(0.0);          // User hard-sized tube length for reproting
        std::string CompName;                // component name
        std::string CompType;                // component type
        std::string SizingString;            // input field sizing description (e.g., Nominal Capacity)
        Real64 TempSize;                     // autosized value of coil input field
        int FieldNum = 1;                    // IDD numeric field number where input field description is found
        int SizingMethod;                    // Integer representation of sizing method name (e.g. CoolingCapacitySizing, HeatingCapacitySizing)
        bool PrintFlag;                      // TRUE when sizing information is reported in the eio file
        int CapSizingMethod(0);     // capacity sizing methods (HeatingDesignCapacity, CapacityPerFloorArea, FractionOfAutosizedCoolingCapacity, and
                                    // FractionOfAutosizedHeatingCapacity )
        Real64 DesCoilLoad;         // design autosized or user specified capacity
        int OpMode(1);              // System operating mode
        int HeatNode;               // Hot water inlet node to determine system operating mode
        int CoolNode;               // Chilled water inlet node to determine system operating mode
        Real64 WaterVolFlowMaxDes;  // Design water volume flow rate for reproting
        Real64 WaterVolFlowMaxUser; // User hard-sized water volume flow rate for reproting

        DesCoilLoad = 0.0;
        DataScalableCapSizingON = false;
        DataFracOfAutosizedHeatingCapacity = 1.0;

        if (SystemType == ElectricSystem) {

            if (ElecRadSys(RadSysNum).MaxElecPower == AutoSize) {
                IsAutoSize = true;
            }

            if (CurZoneEqNum > 0) {

                CompType = "ZoneHVAC:LowTemperatureRadiant:Electric";
                CompName = ElecRadSys(RadSysNum).Name;
                SizingMethod = HeatingCapacitySizing;
                FieldNum = 1;
                PrintFlag = true;
                SizingString = ElecRadSysNumericFields(RadSysNum).FieldNames(FieldNum) + " [W]";
                CapSizingMethod = ElecRadSys(RadSysNum).HeatingCapMethod;
                ZoneEqSizing(CurZoneEqNum).SizingMethod(SizingMethod) = CapSizingMethod;

                if (!IsAutoSize && !ZoneSizingRunDone) { // simulation continue
                    if (CapSizingMethod == HeatingDesignCapacity && ElecRadSys(RadSysNum).ScaledHeatingCapacity > 0.0) {
                        TempSize = ElecRadSys(RadSysNum).ScaledHeatingCapacity;
                        RequestSizing(state, CompType, CompName, SizingMethod, SizingString, TempSize, PrintFlag, RoutineName);
                        DesCoilLoad = TempSize;
                    } else if (CapSizingMethod == CapacityPerFloorArea) {
                        DataScalableCapSizingON = true;
                        TempSize = ElecRadSys(RadSysNum).ScaledHeatingCapacity * Zone(ElecRadSys(RadSysNum).ZonePtr).FloorArea;
                        RequestSizing(state, CompType, CompName, SizingMethod, SizingString, TempSize, PrintFlag, RoutineName);
                        DesCoilLoad = TempSize;
                        DataScalableCapSizingON = false;
                        ElecRadSys(RadSysNum).MaxElecPower = TempSize;
                    } else if (CapSizingMethod == FractionOfAutosizedHeatingCapacity) {
                        ShowSevereError(RoutineName + ": auto-sizing cannot be done for " + CompType + " = " + ElecRadSys(RadSysNum).Name + "\".");
                        ShowContinueError("The \"SimulationControl\" object must have the field \"Do Zone Sizing Calculation\" set to Yes when the "
                                          "Heating Design Capacity Method = \"FractionOfAutosizedHeatingCapacity\".");
                        ErrorsFound = true;
                    }
                } else {
                    if (CapSizingMethod == HeatingDesignCapacity || CapSizingMethod == CapacityPerFloorArea ||
                        CapSizingMethod == FractionOfAutosizedHeatingCapacity) {
                        if (CapSizingMethod == HeatingDesignCapacity) {
                            if (ZoneSizingRunDone) {
                                CheckZoneSizing(CompType, CompName);
                                SizingMethod = AutoCalculateSizing;
                                DataConstantUsedForSizing = FinalZoneSizing(CurZoneEqNum).NonAirSysDesHeatLoad;
                                DataFractionUsedForSizing = 1.0;
                            }
                            if (ElecRadSys(RadSysNum).ScaledHeatingCapacity == AutoSize) {
                                TempSize = AutoSize;
                            } else {
                                TempSize = ElecRadSys(RadSysNum).ScaledHeatingCapacity;
                            }
                        } else if (CapSizingMethod == CapacityPerFloorArea) {
                            if (ZoneSizingRunDone) {
                                CheckZoneSizing(CompType, CompName);
                                ZoneEqSizing(CurZoneEqNum).HeatingCapacity = true;
                                ZoneEqSizing(CurZoneEqNum).DesHeatingLoad = FinalZoneSizing(CurZoneEqNum).NonAirSysDesHeatLoad;
                            }
                            TempSize = ElecRadSys(RadSysNum).ScaledHeatingCapacity * Zone(ElecRadSys(RadSysNum).ZonePtr).FloorArea;
                            DataScalableCapSizingON = true;

                        } else if (CapSizingMethod == FractionOfAutosizedHeatingCapacity) {
                            CheckZoneSizing(CompType, CompName);
                            ZoneEqSizing(CurZoneEqNum).HeatingCapacity = true;
                            ZoneEqSizing(CurZoneEqNum).DesHeatingLoad = FinalZoneSizing(CurZoneEqNum).NonAirSysDesHeatLoad;
                            TempSize = ZoneEqSizing(CurZoneEqNum).DesHeatingLoad * ElecRadSys(RadSysNum).ScaledHeatingCapacity;
                            DataScalableCapSizingON = true;
                        } else {
                            TempSize = ElecRadSys(RadSysNum).ScaledHeatingCapacity;
                        }
                        RequestSizing(state, CompType, CompName, SizingMethod, SizingString, TempSize, PrintFlag, RoutineName);
                        ElecRadSys(RadSysNum).MaxElecPower = TempSize;
                        DataConstantUsedForSizing = 0.0;
                        DataFractionUsedForSizing = 0.0;
                        DataScalableCapSizingON = false;
                    }
                }
            }
        }

        if (SystemType == HydronicSystem) {

            CompType = "ZoneHVAC:LowTemperatureRadiant:VariableFlow";
            CompName = HydrRadSys(RadSysNum).Name;

            IsAutoSize = false;
            if (HydrRadSys(RadSysNum).ScaledHeatingCapacity == AutoSize) {
                IsAutoSize = true;
            }

            if (CurZoneEqNum > 0) {

                SizingMethod = HeatingCapacitySizing;
                FieldNum = 5;
                PrintFlag = true;
                SizingString = HydronicRadiantSysNumericFields(RadSysNum).FieldNames(FieldNum) + " [W]";
                CapSizingMethod = HydrRadSys(RadSysNum).HeatingCapMethod;
                ZoneEqSizing(CurZoneEqNum).SizingMethod(SizingMethod) = CapSizingMethod;

                if (!IsAutoSize && !ZoneSizingRunDone) { // simulation continue
                    if (CapSizingMethod == HeatingDesignCapacity && HydrRadSys(RadSysNum).ScaledHeatingCapacity > 0.0) {
                        TempSize = HydrRadSys(RadSysNum).ScaledHeatingCapacity;
                        RequestSizing(state, CompType, CompName, SizingMethod, SizingString, TempSize, PrintFlag, RoutineName);
                        DesCoilLoad = TempSize;
                    } else if (CapSizingMethod == CapacityPerFloorArea) {
                        DataScalableCapSizingON = true;
                        TempSize = HydrRadSys(RadSysNum).ScaledHeatingCapacity * Zone(HydrRadSys(RadSysNum).ZonePtr).FloorArea;
                        RequestSizing(state, CompType, CompName, SizingMethod, SizingString, TempSize, PrintFlag, RoutineName);
                        DesCoilLoad = TempSize;
                        DataScalableCapSizingON = false;
                    } else if (CapSizingMethod == FractionOfAutosizedHeatingCapacity) {
                        if (HydrRadSys(RadSysNum).WaterVolFlowMaxHeat == AutoSize) {
                            ShowSevereError(RoutineName + ": auto-sizing cannot be done for " + CompType + " = " + HydrRadSys(RadSysNum).Name +
                                            "\".");
                            ShowContinueError("The \"SimulationControl\" object must have the field \"Do Zone Sizing Calculation\" set to Yes when "
                                              "the Heating Design Capacity Method = \"FractionOfAutosizedHeatingCapacity\".");
                            ErrorsFound = true;
                        }
                    }
                } else { // Autosize or hard-size with sizing run
                    if (CapSizingMethod == HeatingDesignCapacity || CapSizingMethod == CapacityPerFloorArea ||
                        CapSizingMethod == FractionOfAutosizedHeatingCapacity) {
                        if (CapSizingMethod == HeatingDesignCapacity) {
                            if (ZoneSizingRunDone) {
                                CheckZoneSizing(CompType, CompName);
                                SizingMethod = AutoCalculateSizing;
                                DataConstantUsedForSizing = FinalZoneSizing(CurZoneEqNum).NonAirSysDesHeatLoad;
                                DataFractionUsedForSizing = 1.0;
                            }
                            if (HydrRadSys(RadSysNum).ScaledHeatingCapacity == AutoSize) {
                                TempSize = AutoSize;
                            } else {
                                TempSize = HydrRadSys(RadSysNum).ScaledHeatingCapacity;
                            }
                        } else if (CapSizingMethod == CapacityPerFloorArea) {
                            if (ZoneSizingRunDone) {
                                CheckZoneSizing(CompType, CompName);
                                ZoneEqSizing(CurZoneEqNum).HeatingCapacity = true;
                                ZoneEqSizing(CurZoneEqNum).DesHeatingLoad = FinalZoneSizing(CurZoneEqNum).NonAirSysDesHeatLoad;
                            }
                            TempSize = HydrRadSys(RadSysNum).ScaledHeatingCapacity * Zone(HydrRadSys(RadSysNum).ZonePtr).FloorArea;
                            DataScalableCapSizingON = true;
                        } else if (CapSizingMethod == FractionOfAutosizedHeatingCapacity) {
                            CheckZoneSizing(CompType, CompName);
                            ZoneEqSizing(CurZoneEqNum).HeatingCapacity = true;
                            ZoneEqSizing(CurZoneEqNum).DesHeatingLoad = FinalZoneSizing(CurZoneEqNum).NonAirSysDesHeatLoad;
                            TempSize = ZoneEqSizing(CurZoneEqNum).DesHeatingLoad * HydrRadSys(RadSysNum).ScaledHeatingCapacity;
                            DataScalableCapSizingON = true;
                        } else {
                            TempSize = HydrRadSys(RadSysNum).ScaledHeatingCapacity;
                        }
                        RequestSizing(state, CompType, CompName, SizingMethod, SizingString, TempSize, PrintFlag, RoutineName);
                        DesCoilLoad = TempSize;
                        DataConstantUsedForSizing = 0.0;
                        DataFractionUsedForSizing = 0.0;
                        DataScalableCapSizingON = false;
                    } else {
                        DesCoilLoad = 0.0;
                    }
                }
                // finally heating capacity is saved in this variable
                HydrRadSys(RadSysNum).ScaledHeatingCapacity = DesCoilLoad;
            }

            IsAutoSize = false;
            if (HydrRadSys(RadSysNum).WaterVolFlowMaxHeat == AutoSize) {
                IsAutoSize = true;
            }

            if (CurZoneEqNum > 0) {
                if (!IsAutoSize && !ZoneSizingRunDone) { // simulation continue
                    if (HydrRadSys(RadSysNum).WaterVolFlowMaxHeat > 0.0) {
                        ReportSizingOutput(CompType,
                                           HydrRadSys(RadSysNum).Name,
                                           "User-Specified Maximum Hot Water Flow [m3/s]",
                                           HydrRadSys(RadSysNum).WaterVolFlowMaxHeat);
                    }
                } else { // Autosize or hard-size with sizing run
                    if (HydrRadSys(RadSysNum).HotWaterInNode > 0 && HydrRadSys(RadSysNum).HotWaterOutNode > 0) {
                        PltSizHeatNum = MyPlantSizingIndex(CompType,
                                                           HydrRadSys(RadSysNum).Name,
                                                           HydrRadSys(RadSysNum).HotWaterInNode,
                                                           HydrRadSys(RadSysNum).HotWaterOutNode,
                                                           ErrorsFound);
                        if (PltSizHeatNum > 0) {
                            if (DesCoilLoad >= SmallLoad) {
                                rho = GetDensityGlycol(PlantLoop(HydrRadSys(RadSysNum).HWLoopNum).FluidName,
                                                       DataGlobals::HWInitConvTemp,
                                                       PlantLoop(HydrRadSys(RadSysNum).HWLoopNum).FluidIndex,
                                                       RoutineName);
                                Cp = GetSpecificHeatGlycol(PlantLoop(HydrRadSys(RadSysNum).HWLoopNum).FluidName,
                                                           DataGlobals::HWInitConvTemp,
                                                           PlantLoop(HydrRadSys(RadSysNum).HWLoopNum).FluidIndex,
                                                           RoutineName);
                                WaterVolFlowMaxHeatDes = DesCoilLoad / (PlantSizData(PltSizHeatNum).DeltaT * Cp * rho);
                            } else {
                                WaterVolFlowMaxHeatDes = 0.0;
                            }
                        } else {
                            ShowSevereError("Autosizing of water flow requires a heating loop Sizing:Plant object");
                            ShowContinueError("Occurs in ZoneHVAC:LowTemperatureRadiant:VariableFlow Object=" + HydrRadSys(RadSysNum).Name);
                            ErrorsFound = true;
                        }
                    }

                    if (IsAutoSize) {
                        HydrRadSys(RadSysNum).WaterVolFlowMaxHeat = WaterVolFlowMaxHeatDes;
                        ReportSizingOutput(CompType, HydrRadSys(RadSysNum).Name, "Design Size Maximum Hot Water Flow [m3/s]", WaterVolFlowMaxHeatDes);
                    } else { // hard-size with sizing data
                        if (HydrRadSys(RadSysNum).WaterVolFlowMaxHeat > 0.0 && WaterVolFlowMaxHeatDes > 0.0) {
                            WaterVolFlowMaxHeatUser = HydrRadSys(RadSysNum).WaterVolFlowMaxHeat;
                            ReportSizingOutput(CompType,
                                               HydrRadSys(RadSysNum).Name,
                                               "Design Size Maximum Hot Water Flow [m3/s]",
                                               WaterVolFlowMaxHeatDes,
                                               "User-Specified Maximum Hot Water Flow [m3/s]",
                                               WaterVolFlowMaxHeatUser);
                            if (DisplayExtraWarnings) {
                                if ((std::abs(WaterVolFlowMaxHeatDes - WaterVolFlowMaxHeatUser) / WaterVolFlowMaxHeatUser) >
                                    AutoVsHardSizingThreshold) {
                                    ShowMessage("SizeLowTempRadiantSystem: Potential issue with equipment sizing for "
                                                "ZoneHVAC:LowTemperatureRadiant:VariableFlow = \"" +
                                                HydrRadSys(RadSysNum).Name + "\".");
                                    ShowContinueError("User-Specified Maximum Hot Water Flow of " + RoundSigDigits(WaterVolFlowMaxHeatUser, 5) +
                                                      " [m3/s]");
                                    ShowContinueError("differs from Design Size Maximum Hot Water Flow of " +
                                                      RoundSigDigits(WaterVolFlowMaxHeatDes, 5) + " [m3/s]");
                                    ShowContinueError("This may, or may not, indicate mismatched component sizes.");
                                    ShowContinueError("Verify that the value entered is intended and is consistent with other components.");
                                }
                            }
                        }
                    }
                }
            }

            IsAutoSize = false;
            if (HydrRadSys(RadSysNum).ScaledCoolingCapacity == AutoSize) {
                IsAutoSize = true;
            }

            if (CurZoneEqNum > 0) {

                SizingMethod = CoolingCapacitySizing;
                FieldNum = 10;
                PrintFlag = true;
                SizingString = HydronicRadiantSysNumericFields(RadSysNum).FieldNames(FieldNum) + " [W]";
                CapSizingMethod = HydrRadSys(RadSysNum).CoolingCapMethod;
                ZoneEqSizing(CurZoneEqNum).SizingMethod(SizingMethod) = CapSizingMethod;

                if (!IsAutoSize && !ZoneSizingRunDone) { // simulation continue
                    if (CapSizingMethod == CoolingDesignCapacity && HydrRadSys(RadSysNum).ScaledCoolingCapacity > 0.0) {
                        TempSize = HydrRadSys(RadSysNum).ScaledCoolingCapacity;
                        RequestSizing(state, CompType, CompName, SizingMethod, SizingString, TempSize, PrintFlag, RoutineName);
                        DesCoilLoad = TempSize;
                    } else if (CapSizingMethod == CapacityPerFloorArea) {
                        DataScalableCapSizingON = true;
                        TempSize = HydrRadSys(RadSysNum).ScaledCoolingCapacity * Zone(HydrRadSys(RadSysNum).ZonePtr).FloorArea;
                        RequestSizing(state, CompType, CompName, SizingMethod, SizingString, TempSize, PrintFlag, RoutineName);
                        DesCoilLoad = TempSize;
                        DataScalableCapSizingON = false;
                    } else if (CapSizingMethod == FractionOfAutosizedCoolingCapacity) {
                        if (HydrRadSys(RadSysNum).WaterVolFlowMaxCool == AutoSize) {
                            ShowSevereError(RoutineName + ": auto-sizing cannot be done for " + CompType + " = " + HydrRadSys(RadSysNum).Name +
                                            "\".");
                            ShowContinueError("The \"SimulationControl\" object must have the field \"Do Zone Sizing Calculation\" set to Yes when "
                                              "the Cooling Design Capacity Method = \"FractionOfAutosizedCoolingCapacity\".");
                            ErrorsFound = true;
                        }
                    }
                } else { // Autosize or hard-size with sizing run
                    if (CapSizingMethod == CoolingDesignCapacity || CapSizingMethod == CapacityPerFloorArea ||
                        CapSizingMethod == FractionOfAutosizedCoolingCapacity) {
                        if (CapSizingMethod == CoolingDesignCapacity) {
                            if (ZoneSizingRunDone) {
                                CheckZoneSizing(CompType, CompName);
                                SizingMethod = AutoCalculateSizing;
                                DataConstantUsedForSizing = FinalZoneSizing(CurZoneEqNum).NonAirSysDesCoolLoad;
                                DataFractionUsedForSizing = 1.0;
                            }
                            if (HydrRadSys(RadSysNum).ScaledCoolingCapacity == AutoSize) {
                                TempSize = AutoSize;
                            } else {
                                TempSize = HydrRadSys(RadSysNum).ScaledCoolingCapacity;
                            }
                        } else if (CapSizingMethod == CapacityPerFloorArea) {
                            if (ZoneSizingRunDone) {
                                CheckZoneSizing(CompType, CompName);
                                ZoneEqSizing(CurZoneEqNum).CoolingCapacity = true;
                                ZoneEqSizing(CurZoneEqNum).DesCoolingLoad = FinalZoneSizing(CurZoneEqNum).NonAirSysDesCoolLoad;
                            }
                            TempSize = HydrRadSys(RadSysNum).ScaledCoolingCapacity * Zone(HydrRadSys(RadSysNum).ZonePtr).FloorArea;
                            DataScalableCapSizingON = true;
                        } else if (CapSizingMethod == FractionOfAutosizedCoolingCapacity) {
                            CheckZoneSizing(CompType, CompName);
                            ZoneEqSizing(CurZoneEqNum).CoolingCapacity = true;
                            ZoneEqSizing(CurZoneEqNum).DesCoolingLoad = FinalZoneSizing(CurZoneEqNum).NonAirSysDesCoolLoad;
                            TempSize = ZoneEqSizing(CurZoneEqNum).DesCoolingLoad * HydrRadSys(RadSysNum).ScaledCoolingCapacity;
                            DataScalableCapSizingON = true;

                        } else {
                            TempSize = HydrRadSys(RadSysNum).ScaledCoolingCapacity;
                        }
                        RequestSizing(state, CompType, CompName, SizingMethod, SizingString, TempSize, PrintFlag, RoutineName);
                        DesCoilLoad = TempSize;
                        DataConstantUsedForSizing = 0.0;
                        DataFractionUsedForSizing = 0.0;
                        DataScalableCapSizingON = false;
                    } else {
                        DesCoilLoad = 0.0;
                    }
                }
                // finally cooling capacity is saved in this variable
                HydrRadSys(RadSysNum).ScaledCoolingCapacity = DesCoilLoad;
            }

            IsAutoSize = false;
            if (HydrRadSys(RadSysNum).WaterVolFlowMaxCool == AutoSize) {
                IsAutoSize = true;
            }
            if (CurZoneEqNum > 0) {
                if (!IsAutoSize && !ZoneSizingRunDone) { // simulation continue
                    if (HydrRadSys(RadSysNum).WaterVolFlowMaxCool > 0.0) {
                        ReportSizingOutput(CompType,
                                           HydrRadSys(RadSysNum).Name,
                                           "User-Specified Maximum Cold Water Flow [m3/s]",
                                           HydrRadSys(RadSysNum).WaterVolFlowMaxCool);
                    }
                } else { // Autosize or hard-size with sizing run
                    if (HydrRadSys(RadSysNum).ColdWaterInNode > 0 && HydrRadSys(RadSysNum).ColdWaterOutNode > 0) {
                        PltSizCoolNum = MyPlantSizingIndex(CompType,
                                                           HydrRadSys(RadSysNum).Name,
                                                           HydrRadSys(RadSysNum).ColdWaterInNode,
                                                           HydrRadSys(RadSysNum).ColdWaterOutNode,
                                                           ErrorsFound);
                        if (PltSizCoolNum > 0) {
                            if (DesCoilLoad >= SmallLoad) {
                                rho = GetDensityGlycol(PlantLoop(HydrRadSys(RadSysNum).CWLoopNum).FluidName,
                                                       DataGlobals::CWInitConvTemp,
                                                       PlantLoop(HydrRadSys(RadSysNum).CWLoopNum).FluidIndex,
                                                       RoutineName);
                                Cp = GetSpecificHeatGlycol(PlantLoop(HydrRadSys(RadSysNum).CWLoopNum).FluidName,
                                                           DataGlobals::CWInitConvTemp,
                                                           PlantLoop(HydrRadSys(RadSysNum).CWLoopNum).FluidIndex,
                                                           RoutineName);
                                WaterVolFlowMaxCoolDes = DesCoilLoad / (PlantSizData(PltSizCoolNum).DeltaT * Cp * rho);
                            } else {
                                WaterVolFlowMaxCoolDes = 0.0;
                            }
                        } else {
                            ShowSevereError("Autosizing of water flow requires a cooling loop Sizing:Plant object");
                            ShowContinueError("Occurs in ZoneHVAC:LowTemperatureRadiant:VariableFlow Object=" + HydrRadSys(RadSysNum).Name);
                            ErrorsFound = true;
                        }
                    }

                    if (IsAutoSize) {
                        HydrRadSys(RadSysNum).WaterVolFlowMaxCool = WaterVolFlowMaxCoolDes;
                        ReportSizingOutput(
                            CompType, HydrRadSys(RadSysNum).Name, "Design Size Maximum Cold Water Flow [m3/s]", WaterVolFlowMaxCoolDes);
                    } else { // hard-size with sizing data
                        if (HydrRadSys(RadSysNum).WaterVolFlowMaxCool > 0.0 && WaterVolFlowMaxCoolDes > 0.0) {
                            WaterVolFlowMaxCoolUser = HydrRadSys(RadSysNum).WaterVolFlowMaxCool;
                            ReportSizingOutput(CompType,
                                               HydrRadSys(RadSysNum).Name,
                                               "Design Size Maximum Cold Water Flow [m3/s]",
                                               WaterVolFlowMaxCoolDes,
                                               "User-Specified Maximum Cold Water Flow [m3/s]",
                                               WaterVolFlowMaxCoolUser);
                            if (DisplayExtraWarnings) {
                                if ((std::abs(WaterVolFlowMaxCoolDes - WaterVolFlowMaxCoolUser) / WaterVolFlowMaxCoolUser) >
                                    AutoVsHardSizingThreshold) {
                                    ShowMessage("SizeLowTempRadiantSystem: Potential issue with equipment sizing for "
                                                "ZoneHVAC:LowTemperatureRadiant:VariableFlow = \"" +
                                                HydrRadSys(RadSysNum).Name + "\".");
                                    ShowContinueError("User-Specified Maximum Cool Water Flow of " + RoundSigDigits(WaterVolFlowMaxCoolUser, 5) +
                                                      " [m3/s]");
                                    ShowContinueError("differs from Design Size Maximum Cool Water Flow of " +
                                                      RoundSigDigits(WaterVolFlowMaxCoolDes, 5) + " [m3/s]");
                                    ShowContinueError("This may, or may not, indicate mismatched component sizes.");
                                    ShowContinueError("Verify that the value entered is intended and is consistent with other components.");
                                }
                            }
                        }
                    }
                }
            }

            IsAutoSize = false;
            if (HydrRadSys(RadSysNum).TubeLength == AutoSize) {
                IsAutoSize = true;
            }
            if (CurZoneEqNum > 0) {
                if (!IsAutoSize && !ZoneSizingRunDone) { // simulation continue
                    if (HydrRadSys(RadSysNum).TubeLength > 0.0) {
                        ReportSizingOutput(
                            CompType, HydrRadSys(RadSysNum).Name, "User-Specified Hydronic Tubing Length [m]", HydrRadSys(RadSysNum).TubeLength);
                    }
                } else { // Autosize or hard-size with sizing run
                    // CheckZoneSizing is not required here because the tube length calculation is not dependent on zone sizing calculation results
                    TubeLengthDes = HydrRadSys(RadSysNum).sizeRadiantSystemTubeLength();
                    if (IsAutoSize) {
                        HydrRadSys(RadSysNum).TubeLength = TubeLengthDes;
                        ReportSizingOutput(CompType, HydrRadSys(RadSysNum).Name, "Design Size Hydronic Tubing Length [m]", TubeLengthDes);
                    } else { // hard-size with sizing data
                        if (HydrRadSys(RadSysNum).TubeLength > 0.0 && TubeLengthDes > 0.0) {
                            TubeLengthUser = HydrRadSys(RadSysNum).TubeLength;
                            ReportSizingOutput(CompType,
                                               HydrRadSys(RadSysNum).Name,
                                               "Design Size Hydronic Tubing Length [m]",
                                               TubeLengthDes,
                                               "User-Specified Hydronic Tubing Length [m]",
                                               TubeLengthUser);
                            if (DisplayExtraWarnings) {
                                if ((std::abs(TubeLengthDes - TubeLengthUser) / TubeLengthUser) > AutoVsHardSizingThreshold) {
                                    ShowMessage("SizeLowTempRadiantSystem: Potential issue with equipment sizing for "
                                                "ZoneHVAC:LowTemperatureRadiant:VariableFlow = \"" +
                                                HydrRadSys(RadSysNum).Name + "\".");
                                    ShowContinueError("User-Specified Hydronic Tubing Length of " + RoundSigDigits(TubeLengthUser, 5) + " [m]");
                                    ShowContinueError("differs from Design Size Hydronic Tubing Length of " + RoundSigDigits(TubeLengthDes, 5) +
                                                      " [m]");
                                    ShowContinueError("This may, or may not, indicate mismatched component sizes.");
                                    ShowContinueError("Verify that the value entered is intended and is consistent with other components.");
                                }
                            }
                        }
                    }
                }
            }

            for (SurfNum = 1; SurfNum <= HydrRadSys(RadSysNum).NumOfSurfaces; ++SurfNum) {
                if (HydrRadSys(RadSysNum).NumCircCalcMethod == CalculateFromLength) {
                    HydrRadSys(RadSysNum).NumCircuits(SurfNum) =
                        (HydrRadSys(RadSysNum).SurfaceFrac(SurfNum) * HydrRadSys(RadSysNum).TubeLength) / HydrRadSys(RadSysNum).CircLength;
                    HydrRadSys(RadSysNum).NumCircuits(SurfNum) = max(HydrRadSys(RadSysNum).NumCircuits(SurfNum), 1.0);
                } else {
                    HydrRadSys(RadSysNum).NumCircuits(SurfNum) = 1.0;
                }
            }

            RegisterPlantCompDesignFlow(HydrRadSys(RadSysNum).HotWaterInNode, HydrRadSys(RadSysNum).WaterVolFlowMaxHeat);
            RegisterPlantCompDesignFlow(HydrRadSys(RadSysNum).ColdWaterInNode, HydrRadSys(RadSysNum).WaterVolFlowMaxCool);
        }

        if (SystemType == ConstantFlowSystem) {

            CompType = "ZoneHVAC:LowTemperatureRadiant:ConstantFlow";
            CompName = CFloRadSys(RadSysNum).Name;

            // Check which operating system it is
            HeatNode = CFloRadSys(RadSysNum).HotWaterInNode;
            CoolNode = CFloRadSys(RadSysNum).ColdWaterInNode;
            if (HeatNode > 0 && CoolNode > 0) {
                OpMode = ClgHtg;
            } else if (HeatNode > 0 && CoolNode <= 0) {
                OpMode = HtgOnly;
            } else if (CoolNode > 0 && HeatNode <= 0) {
                OpMode = ClgOnly;
            } else {
                OpMode = OFF; // It shouldn't happen here
            }

            if (CFloRadSys(RadSysNum).WaterVolFlowMax == AutoSize) {
                IsAutoSize = true;
            }

            if (CurZoneEqNum > 0) {
                if (!IsAutoSize && !ZoneSizingRunDone) { // simulation continue
                    if (CFloRadSys(RadSysNum).WaterVolFlowMax > 0.0) {
                        ReportSizingOutput(
                            CompType, CFloRadSys(RadSysNum).Name, "User-Specified Maximum Water Flow [m3/s]", CFloRadSys(RadSysNum).WaterVolFlowMax);
                    }
                } else { // Autosize or hard-size with sizing run
                    CheckZoneSizing(CompType, CFloRadSys(RadSysNum).Name);
                    // Estimate hot water and chilled water flows
                    // Index only if it provides heating to avoid severe error
                    if (OpMode == ClgHtg || OpMode == HtgOnly) {
                        PltSizHeatNum = MyPlantSizingIndex(CompType,
                                                           CFloRadSys(RadSysNum).Name,
                                                           CFloRadSys(RadSysNum).HotWaterInNode,
                                                           CFloRadSys(RadSysNum).HotWaterOutNode,
                                                           ErrorsFound);
                    }
                    if (PltSizHeatNum > 0) {
                        if (FinalZoneSizing(CurZoneEqNum).NonAirSysDesHeatLoad >= SmallLoad) {
                            rho = GetDensityGlycol(PlantLoop(CFloRadSys(RadSysNum).HWLoopNum).FluidName,
                                                   DataGlobals::HWInitConvTemp,
                                                   PlantLoop(CFloRadSys(RadSysNum).HWLoopNum).FluidIndex,
                                                   "SizeLowTempRadiantSystem");
                            Cp = GetSpecificHeatGlycol(PlantLoop(CFloRadSys(RadSysNum).HWLoopNum).FluidName,
                                                       DataGlobals::HWInitConvTemp,
                                                       PlantLoop(CFloRadSys(RadSysNum).HWLoopNum).FluidIndex,
                                                       "SizeLowTempRadiantSystem");
                            WaterVolFlowMaxHeatDes =
                                FinalZoneSizing(CurZoneEqNum).NonAirSysDesHeatLoad / (PlantSizData(PltSizHeatNum).DeltaT * Cp * rho);
                        } else {
                            WaterVolFlowMaxHeatDes = 0.0;
                        }
                    } else {
                        if (OpMode == ClgHtg || OpMode == HtgOnly) {
                            ShowSevereError("Autosizing of water flow requires a heating loop Sizing:Plant object");
                            ShowContinueError("Occurs in ZoneHVAC:LowTemperatureRadiant:ConstantFlow Object=" + CFloRadSys(RadSysNum).Name);
                            ErrorsFound = true;
                        }
                    }

                    // Index only if it provides cooling system to avoid severe error
                    if (OpMode == ClgHtg || OpMode == ClgOnly) {
                        PltSizCoolNum = MyPlantSizingIndex(CompType,
                                                           CFloRadSys(RadSysNum).Name,
                                                           CFloRadSys(RadSysNum).ColdWaterInNode,
                                                           CFloRadSys(RadSysNum).ColdWaterOutNode,
                                                           ErrorsFound);
                    }
                    if (PltSizCoolNum > 0) {
                        if (FinalZoneSizing(CurZoneEqNum).NonAirSysDesCoolLoad >= SmallLoad) {
                            rho = GetDensityGlycol(PlantLoop(CFloRadSys(RadSysNum).CWLoopNum).FluidName,
                                                   DataGlobals::CWInitConvTemp,
                                                   PlantLoop(CFloRadSys(RadSysNum).CWLoopNum).FluidIndex,
                                                   "SizeLowTempRadiantSystem");
                            Cp = GetSpecificHeatGlycol(PlantLoop(CFloRadSys(RadSysNum).CWLoopNum).FluidName,
                                                       DataGlobals::CWInitConvTemp,
                                                       PlantLoop(CFloRadSys(RadSysNum).CWLoopNum).FluidIndex,
                                                       "SizeLowTempRadiantSystem");
                            WaterVolFlowMaxCoolDes =
                                FinalZoneSizing(CurZoneEqNum).NonAirSysDesCoolLoad / (PlantSizData(PltSizCoolNum).DeltaT * Cp * rho);
                        } else {
                            WaterVolFlowMaxCoolDes = 0.0;
                        }
                    } else {
                        if (OpMode == ClgHtg || OpMode == ClgOnly) {
                            ShowSevereError("Autosizing of water flow requires a cooling loop Sizing:Plant object");
                            ShowContinueError("Occurs in ZoneHVAC:LowTemperatureRadiant:ConstantFlow Object=" + CFloRadSys(RadSysNum).Name);
                            ErrorsFound = true;
                        }
                    }

                    // Determine maximum water flow rate depending upon system type
                    if (OpMode == ClgHtg) {
                        WaterVolFlowMaxDes = std::max(WaterVolFlowMaxHeatDes, WaterVolFlowMaxCoolDes);
                    } else if (OpMode == ClgOnly) {
                        WaterVolFlowMaxDes = WaterVolFlowMaxCoolDes;
                    } else if (OpMode == HtgOnly) {
                        WaterVolFlowMaxDes = WaterVolFlowMaxHeatDes;
                    } else {
                        WaterVolFlowMaxDes = 0.0;
                    }

                    if (IsAutoSize) {
                        CFloRadSys(RadSysNum).WaterVolFlowMax = WaterVolFlowMaxDes;
                        ReportSizingOutput(CompType, CFloRadSys(RadSysNum).Name, "Design Size Maximum Water Flow [m3/s]", WaterVolFlowMaxDes);
                    } else { // hard-size with sizing data
                        if (CFloRadSys(RadSysNum).WaterVolFlowMax > 0.0 && WaterVolFlowMaxDes > 0.0) {
                            WaterVolFlowMaxUser = CFloRadSys(RadSysNum).WaterVolFlowMax;
                            ReportSizingOutput(CompType,
                                               CFloRadSys(RadSysNum).Name,
                                               "Design Size Maximum Water Flow [m3/s]",
                                               WaterVolFlowMaxDes,
                                               "User-Specified Maximum Water Flow [m3/s]",
                                               WaterVolFlowMaxUser);
                            if (DisplayExtraWarnings) {
                                if ((std::abs(WaterVolFlowMaxDes - WaterVolFlowMaxUser) / WaterVolFlowMaxUser) > AutoVsHardSizingThreshold) {
                                    ShowMessage("SizeLowTempRadiantSystem: Potential issue with equipment sizing for "
                                                "ZoneHVAC:LowTemperatureRadiant:ConstantFlow = \" " +
                                                CFloRadSys(RadSysNum).Name + "\".");
                                    ShowContinueError("User-Specified Maximum Water Flow of " + RoundSigDigits(WaterVolFlowMaxUser, 5) + " [m3/s]");
                                    ShowContinueError("differs from Design Size Maximum Water Flow of " + RoundSigDigits(WaterVolFlowMaxDes, 5) +
                                                      " [m3/s]");
                                    ShowContinueError("This may, or may not, indicate mismatched component sizes.");
                                    ShowContinueError("Verify that the value entered is intended and is consistent with other components.");
                                }
                            }
                        }
                    }
                }
            }

            IsAutoSize = false;
            if (CFloRadSys(RadSysNum).TubeLength == AutoSize) {
                IsAutoSize = true;
            }

            if (CurZoneEqNum > 0) {
                if (!IsAutoSize && !ZoneSizingRunDone) { // simulation continue
                    if (CFloRadSys(RadSysNum).TubeLength > 0.0) {
                        ReportSizingOutput("ZoneHVAC:LowTemperatureRadiant:ConstantFlow",
                                           CFloRadSys(RadSysNum).Name,
                                           "User-Specified Hydronic Tubing Length [m]",
                                           CFloRadSys(RadSysNum).TubeLength);
                    }
                } else { // Autosize or hard-size with sizing run
                    // CheckZoneSizing is not required here because the tube length calculation is not dependent on zone sizing calculation results
                    TubeLengthDes = CFloRadSys(RadSysNum).sizeRadiantSystemTubeLength();
                    if (IsAutoSize) {
                        CFloRadSys(RadSysNum).TubeLength = TubeLengthDes;
                        ReportSizingOutput("ZoneHVAC:LowTemperatureRadiant:ConstantFlow",
                                           CFloRadSys(RadSysNum).Name,
                                           "Design Size Hydronic Tubing Length [m]",
                                           TubeLengthDes);
                    } else { // hard-size with sizing data
                        if (CFloRadSys(RadSysNum).TubeLength > 0.0 && TubeLengthDes > 0.0) {
                            TubeLengthUser = CFloRadSys(RadSysNum).TubeLength;
                            ReportSizingOutput("ZoneHVAC:LowTemperatureRadiant:ConstantFlow",
                                               CFloRadSys(RadSysNum).Name,
                                               "Design Size Hydronic Tubing Length [m]",
                                               TubeLengthDes,
                                               "User-Specified Hydronic Tubing Length [m]",
                                               TubeLengthUser);
                            if (DisplayExtraWarnings) {
                                if ((std::abs(TubeLengthDes - TubeLengthUser) / TubeLengthUser) > AutoVsHardSizingThreshold) {
                                    ShowMessage("SizeLowTempRadiantSystem: Potential issue with equipment sizing for "
                                                "ZoneHVAC:LowTemperatureRadiant:ConstantFlow = \" " +
                                                CFloRadSys(RadSysNum).Name + "\".");
                                    ShowContinueError("User-Specified Hydronic Tubing Length of " + RoundSigDigits(TubeLengthUser, 5) + " [m]");
                                    ShowContinueError("differs from Design Size Hydronic Tubing Length of " + RoundSigDigits(TubeLengthDes, 5) +
                                                      " [m]");
                                    ShowContinueError("This may, or may not, indicate mismatched component sizes.");
                                    ShowContinueError("Verify that the value entered is intended and is consistent with other components.");
                                }
                            }
                        }
                    }
                }
            }

            for (SurfNum = 1; SurfNum <= CFloRadSys(RadSysNum).NumOfSurfaces; ++SurfNum) {
                if (CFloRadSys(RadSysNum).NumCircCalcMethod == CalculateFromLength) {
                    CFloRadSys(RadSysNum).NumCircuits(SurfNum) =
                        (CFloRadSys(RadSysNum).SurfaceFrac(SurfNum) * CFloRadSys(RadSysNum).TubeLength) / CFloRadSys(RadSysNum).CircLength;
                    CFloRadSys(RadSysNum).NumCircuits(SurfNum) = max(CFloRadSys(RadSysNum).NumCircuits(SurfNum), 1.0);
                } else {
                    CFloRadSys(RadSysNum).NumCircuits(SurfNum) = 1.0;
                }
            }
            if (CFloRadSys(RadSysNum).HotWaterInNode > 0) {
                RegisterPlantCompDesignFlow(CFloRadSys(RadSysNum).HotWaterInNode, CFloRadSys(RadSysNum).WaterVolFlowMax);
            }
            if (CFloRadSys(RadSysNum).ColdWaterInNode > 0) {
                RegisterPlantCompDesignFlow(CFloRadSys(RadSysNum).ColdWaterInNode, CFloRadSys(RadSysNum).WaterVolFlowMax);
            }
        }

        if (ErrorsFound) {
            ShowFatalError("Preceding sizing errors cause program termination");
        }
    }

    Real64 HydronicSystemBaseData::sizeRadiantSystemTubeLength()
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Rick Strand
        //       DATE WRITTEN   August 2017

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine figures out the tube length based on the spacing of tubes.
        // For single surface systems, this is fairly easy as there is only one spacing
        // to deal with.  For multi-surface systems, more work is necessary because each
        // surface could use a different spacing.

        // Return value
        Real64 sizeRadiantSystemTubeLength;

        Real64 tubeLength (0.0); // temporary holding place for the function calculation

        for (int surfNum = 1; surfNum <= this->NumOfSurfaces; ++surfNum) {
            auto &thisHydrSysSurf(Surface(this->SurfacePtr(surfNum)));
            auto &thisHydrSpacing(dataConstruction.Construct(thisHydrSysSurf.Construction).ThicknessPerpend);
            if ((thisHydrSpacing > 0.005) && (thisHydrSpacing < 0.5)) { // limit allowable spacing to between 1cm and 1m
                tubeLength += thisHydrSysSurf.Area / (2.0 * thisHydrSpacing);
            } else { // if not in allowable limit, default back to 0.15m (15cm or 6 inches)
                tubeLength += thisHydrSysSurf.Area / 0.15;
            }
        }

        sizeRadiantSystemTubeLength = tubeLength;
        return sizeRadiantSystemTubeLength;
    }

    void VariableFlowRadiantSystemData::calculateLowTemperatureRadiantSystem(EnergyPlusData &state, Real64 &LoadMet)   // load met by the radiant system, in Watts
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Rick Strand
        //       DATE WRITTEN   November 2000

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine does all of the stuff that is necessary to simulate
        // a low temperature hydronic radiant heating/cooling system.  Calls are
        // made to appropriate subroutines either in this module or outside of it.

        // METHODOLOGY EMPLOYED:
        // Follows the methods used by many other pieces of zone equipment.
        // Much like a water coil, a hydronic system will use the ControlCompOutput
        // routine to determine what fraction of capacity the unit should be
        // functioning at by controlling the flow rate of water to the element.

        // REFERENCES:
        // Other EnergyPlus modules
        // IBLAST-QTF research program, completed in January 1995 (unreleased)
        // Strand, R.K. 1995. "Heat Source Transfer Functions and Their Application to
        //   Low Temperature Radiant Heating Systems", Ph.D. dissertation, University
        //   of Illinois at Urbana-Champaign, Department of Mechanical and Industrial
        //   Engineering.
        // Seem, J.E. 1986. "Heat Transfer in Buildings", Ph.D. dissertation, University
        //   of Wisconsin-Madison.

        // Using/Aliasing
        using DataBranchAirLoopPlant::MassFlowTolerance;
        using DataHeatBalance::Zone;
        using DataHeatBalance::ZoneData;
        using DataHVACGlobals::SmallLoad;
        using PlantUtilities::SetComponentFlowRate;
        using ScheduleManager::GetCurrentScheduleValue;

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        Real64 ActWaterFlow; // actual water flow for heating or cooling [kg/sec]
        int ControlNode;     // the hot water or cold water inlet node
        Real64 ControlTemp;  // temperature of whatever is controlling the radiant system
        Real64 MassFlowFrac; // fraction of the maximum water flow rate as determined by the control algorithm
        Real64 MaxWaterFlow; // maximum water flow for heating or cooling [kg/sec]
        Real64 OffTempCool;  // temperature at which the flow rate throttles back to zero for cooling
        Real64 OffTempHeat;  // temperature at which the flow rate throttles back to zero for heating
        int SurfNum;         // Surface number in the Surface derived type for a radiant system surface
        int SurfNum2;        // Surface number in the Surface derived type for a radiant system surface
        int ZoneNum;         // number of zone being served
        Real64 mdot;         // local temporary for fluid mass flow rate
        bool SysRunning;     // True when system is running

        ControlNode = 0;
        MaxWaterFlow = 0.0;
        ActWaterFlow = 0.0;
        ZoneNum = this->ZonePtr;
        this->OperatingMode = NotOperating;
        SysRunning = true;

        if (GetCurrentScheduleValue(this->SchedPtr) <= 0) {

            // Unit is off or has no load upon it; set the flow rates to zero and then
            // simulate the components with the no flow conditions
            for (SurfNum = 1; SurfNum <= this->NumOfSurfaces; ++SurfNum) {
                SurfNum2 = this->SurfacePtr(SurfNum);
                QRadSysSource(SurfNum2) = 0.0;
                if (Surface(SurfNum2).ExtBoundCond > 0 && Surface(SurfNum2).ExtBoundCond != SurfNum2)
                    QRadSysSource(Surface(SurfNum2).ExtBoundCond) = 0.0; // Also zero the other side of an interzone
            }
            if (this->HeatingSystem) {
                mdot = 0.0;
                SetComponentFlowRate(mdot,
                                     this->HotWaterInNode,
                                     this->HotWaterOutNode,
                                     this->HWLoopNum,
                                     this->HWLoopSide,
                                     this->HWBranchNum,
                                     this->HWCompNum);
            }
            if (this->CoolingSystem) {
                mdot = 0.0;
                SetComponentFlowRate(mdot,
                                     this->ColdWaterInNode,
                                     this->ColdWaterOutNode,
                                     this->CWLoopNum,
                                     this->CWLoopSide,
                                     this->CWBranchNum,
                                     this->CWCompNum);
            }
        } else { // Unit might be on-->this section is intended to control the water mass flow rate being
            // sent to the radiant system

            ControlTemp = this->setRadiantSystemControlTemperature();

            if (this->HotSetptSchedPtr > 0) {
                OffTempHeat = this->setOffTemperatureLowTemperatureRadiantSystem(this->HotSetptSchedPtr,this->HotThrottlRange);
            } else { // This system is not capable of heating, set OffTempHeat to something really low
                OffTempHeat = LowTempHeating;
            }
            if (this->ColdSetptSchedPtr > 0) {
                OffTempCool = this->setOffTemperatureLowTemperatureRadiantSystem(this->ColdSetptSchedPtr,-this->ColdThrottlRange);
            } else { // This system is not capable of cooling, set OffTempCool to something really high
                OffTempCool = HighTempCooling;
            }

            // Check for an illogical condition where a user enters controls that could
            // potentially be heating or cooling at a particular control temperature
            if (OffTempHeat > OffTempCool) {
                MassFlowFrac = 0.0;
                ShowSevereError("Overlapping heating and cooling control temps in radiant system: " + this->Name);
                ShowFatalError("Preceding condition causes termination.");

            } else { // Temperatures for heating and cooling do not overlap--calculate the mass flow fraction

                if (ControlTemp < OffTempHeat && this->HeatingSystem) { // Heating mode
                    this->OperatingMode = HeatingMode;
                } else if (ControlTemp > OffTempCool && this->CoolingSystem) { // Cooling mode
                    this->OperatingMode = CoolingMode;
                }

                this->setOperatingModeBasedOnChangeoverDelay();

                if (this->OperatingMode == HeatingMode) {
                    ControlNode = this->HotWaterInNode;
                    MaxWaterFlow = this->WaterFlowMaxHeat;
                    MassFlowFrac = this->calculateOperationalFraction(OffTempHeat, ControlTemp, this->HotThrottlRange);
                } else if (this->OperatingMode == CoolingMode) {
                    ControlNode = this->ColdWaterInNode;
                    MaxWaterFlow = this->WaterFlowMaxCool;
                    MassFlowFrac = this->calculateOperationalFraction(OffTempCool, ControlTemp, this->ColdThrottlRange);
                } else {
                    MassFlowFrac = 0.0;
                }
            }

            // Calculate and limit the water flow rate
            ActWaterFlow = MassFlowFrac * MaxWaterFlow;
            if (ActWaterFlow < MassFlowTolerance) ActWaterFlow = 0.0;
            if (this->EMSOverrideOnWaterMdot) ActWaterFlow = this->EMSWaterMdotOverrideValue;

            if (this->OperatingMode == HeatingMode) {
                if (this->HeatingSystem) {
                    SetComponentFlowRate(ActWaterFlow,
                                         this->HotWaterInNode,
                                         this->HotWaterOutNode,
                                         this->HWLoopNum,
                                         this->HWLoopSide,
                                         this->HWBranchNum,
                                         this->HWCompNum);
                } else { // not heating system
                    SysRunning = false;
                }
            } else if (this->OperatingMode == CoolingMode) {
                if (this->CoolingSystem) {
                    SetComponentFlowRate(ActWaterFlow,
                                         this->ColdWaterInNode,
                                         this->ColdWaterOutNode,
                                         this->CWLoopNum,
                                         this->CWLoopSide,
                                         this->CWBranchNum,
                                         this->CWCompNum);
                } else { // not cooling system
                    SysRunning = false;
                }
            }

            // Now simulate the system...
            if ( ((this->OperatingMode == HeatingMode) || (this->OperatingMode == CoolingMode))
                 && SysRunning ) this->calculateLowTemperatureRadiantSystemComponents(state, LoadMet);
        }
    }

    void VariableFlowRadiantSystemData::calculateLowTemperatureRadiantSystemComponents(EnergyPlusData &state, Real64 &LoadMet) // Load met by the low temperature radiant system, in Watts
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Rick Strand
        //       DATE WRITTEN   November 2000
        //       MODIFIED       Sep 2011 LKL/BG - resimulate only zones needing it for Radiant systems

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine solves the radiant system based on how much water is (and
        // the conditions of the water) supplied to the radiant system.

        // METHODOLOGY EMPLOYED:
        // Use heat exchanger formulas to obtain the heat source/sink for the radiant
        // system based on the inlet conditions and flow rate of water.  Once that is
        // determined, recalculate the surface heat balances to reflect this heat
        // addition/subtraction.  The load met by the system is determined by the
        // difference between the convection from all surfaces in the zone when
        // there was no radiant system output and with a source/sink added.

        // REFERENCES:
        // IBLAST-QTF research program, completed in January 1995 (unreleased)
        // Strand, R.K. 1995. "Heat Source Transfer Functions and Their Application to
        //   Low Temperature Radiant Heating Systems", Ph.D. dissertation, University
        //   of Illinois at Urbana-Champaign, Department of Mechanical and Industrial
        //   Engineering.

        // Using/Aliasing
        using DataEnvironment::OutBaroPress;
        using DataHeatBalance::Zone;
        using DataHeatBalFanSys::CTFTsrcConstPart;
        using DataHeatBalFanSys::RadSysTiHBConstCoef;
        using DataHeatBalFanSys::RadSysTiHBQsrcCoef;
        using DataHeatBalFanSys::RadSysTiHBToutCoef;
        using DataHeatBalFanSys::RadSysToHBConstCoef;
        using DataHeatBalFanSys::RadSysToHBQsrcCoef;
        using DataHeatBalFanSys::RadSysToHBTinCoef;
        using DataHeatBalFanSys::ZoneAirHumRat;
        using DataHeatBalSurface::TH;
        using DataLoopNode::Node;
        using DataSurfaces::HeatTransferModel_CondFD;
        using DataSurfaces::HeatTransferModel_CTF;
        using DataSurfaces::Surface;
        using General::RoundSigDigits;
        using PlantUtilities::SetComponentFlowRate;

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int CondSurfNum;          // Surface number (in radiant array) of
        int ConstrNum;            // Index for construction number in Construct derived type
        Real64 DewPointTemp;      // Dew-point temperature based on the zone air conditions
        Real64 EpsMdotCp;         // Epsilon (heat exchanger terminology) times water mass flow rate times water specific heat
        Real64 FullWaterMassFlow; // Original water mass flow rate before reducing the flow for condensation concerns
        Real64 LowestRadSurfTemp; // Lowest surface temperature of a radiant system (when condensation is a concern)
        Real64 PredictedCondTemp; // Temperature at which condensation is predicted (includes user parameter)
        int RadSurfNum;           // DO loop counter for the surfaces that comprise a particular radiant system
        int RadSurfNum2;          // DO loop counter for the surfaces that comprise a particular radiant system
        int RadSurfNum3;          // DO loop counter for the surfaces that comprise a particular radiant system
        Real64 ReductionFrac;     // Fraction that the flow should be reduced to avoid condensation
        int SurfNum;              // Index for radiant surface in Surface derived type
        int SurfNum2;             // Index for radiant surface in Surface derived type
        Real64 SysWaterMassFlow;  // System level water mass flow rate (includes effect of zone multiplier)
        Real64 WaterMassFlow;     // Water mass flow rate in the radiant system, kg/s
        int WaterNodeIn;          // Node number of the water entering the radiant system
        Real64 WaterTempIn;       // Temperature of the water entering the radiant system, in C
        Real64 ZeroFlowSurfTemp;  // Temperature of radiant surface when flow is zero
        int ZoneNum;              // Zone pointer for this radiant system

        Real64 Ca; // Coefficients to relate the inlet water temperature to the heat source
        Real64 Cb;
        Real64 Cc;
        Real64 Cd;
        Real64 Ce;
        Real64 Cf;
        Real64 Cg;
        Real64 Ch;
        Real64 Ci;
        Real64 Cj;
        Real64 Ck;
        Real64 Cl;
        // For more info on Ca through Cl, see comments below

        // First, apply heat exchanger logic to find the heat source/sink to the system.
        // This involves finding out the heat transfer characteristics of the hydronic
        // loop and then applying the equations derived on pp. 113-118 of the dissertation.

        // Set the conditions on the water side inlet
        {
            auto const SELECT_CASE_var(this->OperatingMode);
            if (SELECT_CASE_var == HeatingMode) {
                WaterNodeIn = this->HotWaterInNode;
            } else if (SELECT_CASE_var == CoolingMode) {
                WaterNodeIn = this->ColdWaterInNode;
            } else {
                WaterNodeIn = 0; // Suppress uninitialized warning
                ShowSevereError("Illegal low temperature radiant system operating mode");
                ShowContinueError("Occurs in Radiant System=" + this->Name);
                ShowFatalError("Preceding condition causes termination.");
            }
        }
        ZoneNum = this->ZonePtr;
        SysWaterMassFlow = Node(WaterNodeIn).MassFlowRate;
        WaterMassFlow = Node(WaterNodeIn).MassFlowRate / double(Zone(ZoneNum).Multiplier * Zone(ZoneNum).ListMultiplier);
        WaterTempIn = Node(WaterNodeIn).Temp;

        if (WaterMassFlow <= 0.0) {
            // No flow or below minimum allowed so there is no heat source/sink
            // This is possible with a mismatch between system and plant operation
            // or a slight mismatch between zone and system controls.  This is not
            // necessarily a "problem" so this exception is necessary in the code.
            for (RadSurfNum = 1; RadSurfNum <= this->NumOfSurfaces; ++RadSurfNum) {
                SurfNum = this->SurfacePtr(RadSurfNum);
                QRadSysSource(SurfNum) = 0.0;
                if (Surface(SurfNum).ExtBoundCond > 0 && Surface(SurfNum).ExtBoundCond != SurfNum)
                    QRadSysSource(Surface(SurfNum).ExtBoundCond) = 0.0; // Also zero the other side of an interzone
            }

        } else {

            for (RadSurfNum = 1; RadSurfNum <= this->NumOfSurfaces; ++RadSurfNum) {

                SurfNum = this->SurfacePtr(RadSurfNum);
                // Determine the heat exchanger "effectiveness" term

                EpsMdotCp = calculateHXEffectivenessTerm(SurfNum, WaterTempIn, WaterMassFlow, this->SurfaceFrac(RadSurfNum), this->NumCircuits(RadSurfNum));

                // Obtain the heat balance coefficients and calculate the intermediate coefficients
                // linking the inlet water temperature to the heat source/sink to the radiant system.
                // The coefficients are based on the following development...
                // The heat balance equations at the outside and inside surfaces are of the form:
                //   Tinside  = Ca + Cb*Toutside + Cc*q"
                //   Toutside = Cd + Ce*Tinside  + Cf*q"
                //   Tsource  = Cg + Ch*q"       + Ci*Tinside + Cj*Toutside
                // where:
                //   Tinside is the temperature at the inside surface
                //   Toutside is the temperature at the outside surface
                //   Tsource is the temperature within the radiant system at the location of the source/sink
                //   Ca is all of the other terms in the inside heat balance (solar, LW exchange, conduction history terms, etc.)
                //   Cb is the current cross CTF term
                //   Cc is the QTF inside term for the current heat source/sink
                //   Cd is all of the other terms in the outside heat balance (solar, LW exchange, conduction history terms, etc.)
                //   Ce is the current cross CTF term (should be equal to Cb)
                //   Cf is the QTF outside term for the current heat source/sink
                //   Cg is the summation of all temperature and source history terms at the source/sink location
                //   Ch is the QTF term at the source/sink location for the current heat source/sink
                //   Ci is the CTF inside term for the current inside surface temperature
                //   Cj is the CTF outside term for the current outside surface temperature
                // Note that it is necessary to not use "slow conduction" assumptions because the
                // source/sink has an impact on BOTH the inside and outside surface heat balances.
                // Hence the more general formulation.
                // The first two T equations above can be solved to remove the other surface temperature.
                // This results in the following equations:
                //   Tinside  = Ca + Cb*(Cd + Ce*Tinside + Cf*q") + Cc*q"   or...
                //   Tinside  = (Ca + Cb*Cd + (Cc+Cb*Cf)*q") / (1 - Ce*Cb)
                //   Toutside = Cd + Ce*(Ca + Cb*Toutside + Cc*q") + Cf*q"  or...
                //   Toutside = (Cd + Ce*Ca + (Cf+Ce*Cc)*q") / (1 - Ce*Cb)
                // Substituting the new equations for Tinside and Toutside as a function of C and q"
                // into the equation for Tsource...
                //   Tsource  = Cg + Ch*q" + Ci*((Ca + Cb*Cd + (Cc+Cb*Cf)*q") / (1 - Ce*Cb)) &
                //                         + Cj*((Cd + Ce*Ca + (Cf+Ce*Cc)*q") / (1 - Ce*Cb))
                // Or rearranging this to get Tsource as a function of q", we get...
                //   Tsource  =  Cg + ((Ci*(Ca + Cb*Cd) + Cj*(Cd + Ce*Ca))/(1-Ce*Cb)) &
                //             +(Ch + ((Ci*(Cc + Cb*Cf) + Cj*(Cf + Ce*Cc))/(1-Ce*Cb)))*q"
                // Or in a slightly simpler form...
                //   Tsource  = Ck + Cl*q"
                // where:
                //   Ck = Cg + ((Ci*(Ca + Cb*Cd) + Cj*(Cd + Ce*Ca))/(1-Ce*Cb))
                //   Cl = Ch + ((Ci*(Cc + Cb*Cf) + Cj*(Cf + Ce*Cc))/(1-Ce*Cb))
                // Note also that from heat exchanger "algebra", we have:
                //   q = epsilon*qmax    and    qmax = Mdot*Cp*(Twaterin-Tsource)
                // So...
                //   q" = q/Area = (epsilon*Mdot*Cp/Area)*(Twaterin-Tsource)
                // Or rearranging this equation:
                //   Tsource = -(q"*A/(epsilon*Mdot*Cp)) + Twaterin
                // Setting this equation equal to the other equation for Tsource a couple lines up
                // and rearranging to solve for q"...
                //   q" = (Twaterin - Ck) / (Cl + (A/(epsilon*Mdot*Cp))
                // or
                //   q  = (Twaterin - Ck) / ((Cl/A) + (1/epsilon*Mdot*Cp))
                // or
                //   q  = epsilon*Mdot*Cp*(Twaterin - Ck) / (1+(epsilon*Mdot*Cp*Cl/A))
                // which is the desired result, that is the heat source or sink to the radiant
                // system as a function of the water inlet temperature (flow rate is also in there
                // as well as all of the heat balance terms "hidden" in Ck and Cl).
                ConstrNum = Surface(SurfNum).Construction;

                if (Surface(SurfNum).HeatTransferAlgorithm == HeatTransferModel_CTF) {

                    Ca = RadSysTiHBConstCoef(SurfNum);
                    Cb = RadSysTiHBToutCoef(SurfNum);
                    Cc = RadSysTiHBQsrcCoef(SurfNum);

                    Cd = RadSysToHBConstCoef(SurfNum);
                    Ce = RadSysToHBTinCoef(SurfNum);
                    Cf = RadSysToHBQsrcCoef(SurfNum);

                    Cg = CTFTsrcConstPart(SurfNum);
                    Ch = dataConstruction.Construct(ConstrNum).CTFTSourceQ(0);
                    Ci = dataConstruction.Construct(ConstrNum).CTFTSourceIn(0);
                    Cj = dataConstruction.Construct(ConstrNum).CTFTSourceOut(0);

                    Ck = Cg + ((Ci * (Ca + Cb * Cd) + Cj * (Cd + Ce * Ca)) / (1.0 - Ce * Cb));
                    Cl = Ch + ((Ci * (Cc + Cb * Cf) + Cj * (Cf + Ce * Cc)) / (1.0 - Ce * Cb));

                    QRadSysSource(SurfNum) = EpsMdotCp * (WaterTempIn - Ck) / (1.0 + (EpsMdotCp * Cl / Surface(SurfNum).Area));

                } else if (Surface(SurfNum).HeatTransferAlgorithm == HeatTransferModel_CondFD) {

                    QRadSysSource(SurfNum) = EpsMdotCp * (WaterTempIn - TCondFDSourceNode(SurfNum));
                }

                if (Surface(SurfNum).ExtBoundCond > 0 && Surface(SurfNum).ExtBoundCond != SurfNum)
                    QRadSysSource(Surface(SurfNum).ExtBoundCond) = QRadSysSource(SurfNum); // Also set the other side of an interzone
            }

            // "Temperature Comparison" Cut-off:
            for (RadSurfNum = 1; RadSurfNum <= this->NumOfSurfaces; ++RadSurfNum) {
                // Check to see whether or not the system should really be running.  If
                // QRadSysSource is negative when we are in heating mode or QRadSysSource
                // is positive when we are in cooling mode, then the radiant system will
                // be doing the opposite of its intention.  In this case, the flow rate
                // is set to zero to avoid heating in cooling mode or cooling in heating
                // mode.
                SurfNum = this->SurfacePtr(RadSurfNum);

                if (((this->OperatingMode == HeatingMode) && (QRadSysSource(SurfNum) <= 0.0)) ||
                    ((this->OperatingMode == CoolingMode) && (QRadSysSource(SurfNum) >= 0.0))) {
                    WaterMassFlow = 0.0;
                    if (this->OperatingMode == HeatingMode) {
                        SetComponentFlowRate(WaterMassFlow,
                                             this->HotWaterInNode,
                                             this->HotWaterOutNode,
                                             this->HWLoopNum,
                                             this->HWLoopSide,
                                             this->HWBranchNum,
                                             this->HWCompNum);

                    } else if (this->OperatingMode == CoolingMode) {
                        SetComponentFlowRate(WaterMassFlow,
                                             this->ColdWaterInNode,
                                             this->ColdWaterOutNode,
                                             this->CWLoopNum,
                                             this->CWLoopSide,
                                             this->CWBranchNum,
                                             this->CWCompNum);
                    }
                    this->WaterMassFlowRate = WaterMassFlow;
                    this->OperatingMode = NotOperating;

                    for (RadSurfNum2 = 1; RadSurfNum2 <= this->NumOfSurfaces; ++RadSurfNum2) {
                        SurfNum2 = this->SurfacePtr(RadSurfNum2);
                        QRadSysSource(SurfNum2) = 0.0;
                        if (Surface(SurfNum2).ExtBoundCond > 0 && Surface(SurfNum2).ExtBoundCond != SurfNum2)
                            QRadSysSource(Surface(SurfNum2).ExtBoundCond) = 0.0; // Also zero the other side of an interzone
                    }
                    break; // outer do loop
                }
            }

            // Condensation Cut-off:
            // Check to see whether there are any surface temperatures within the radiant system that have
            // dropped below the dew-point temperature.  If so, we need to shut off this radiant system.
            // A safety parameter is added (hardwired parameter) to avoid getting too close to condensation
            // conditions.
            this->CondCausedShutDown = false;
            DewPointTemp = PsyTdpFnWPb(ZoneAirHumRat(ZoneNum), OutBaroPress);

            if ((this->OperatingMode == CoolingMode) && (this->CondCtrlType == CondCtrlSimpleOff)) {

                for (RadSurfNum2 = 1; RadSurfNum2 <= this->NumOfSurfaces; ++RadSurfNum2) {
                    if (TH(2, 1, this->SurfacePtr(RadSurfNum2)) < (DewPointTemp + this->CondDewPtDeltaT)) {
                        // Condensation warning--must shut off radiant system
                        this->CondCausedShutDown = true;
                        WaterMassFlow = 0.0;
                        this->OperatingMode = NotOperating;
                        SetComponentFlowRate(WaterMassFlow,
                                             this->ColdWaterInNode,
                                             this->ColdWaterOutNode,
                                             this->CWLoopNum,
                                             this->CWLoopSide,
                                             this->CWBranchNum,
                                             this->CWCompNum);
                        this->WaterMassFlowRate = WaterMassFlow;
                        for (RadSurfNum3 = 1; RadSurfNum3 <= this->NumOfSurfaces; ++RadSurfNum3) {
                            SurfNum2 = this->SurfacePtr(RadSurfNum3);
                            QRadSysSource(SurfNum2) = 0.0;
                            if (Surface(SurfNum2).ExtBoundCond > 0 && Surface(SurfNum2).ExtBoundCond != SurfNum2)
                                QRadSysSource(Surface(SurfNum2).ExtBoundCond) = 0.0; // Also zero the other side of an interzone
                        }
                        // Produce a warning message so that user knows the system was shut-off due to potential for condensation
                        if (!WarmupFlag) {
                            if (this->CondErrIndex == 0) { // allow errors up to number of radiant systems
                                ShowWarningMessage(cHydronicSystem + " [" + this->Name + ']');
                                ShowContinueError("Surface [" + Surface(this->SurfacePtr(RadSurfNum2)).Name +
                                                  "] temperature below dew-point temperature--potential for condensation exists");
                                ShowContinueError("Flow to the radiant system will be shut-off to avoid condensation");
                                ShowContinueError("Predicted radiant system surface temperature = " +
                                                  RoundSigDigits(TH(2, 1, this->SurfacePtr(RadSurfNum2)), 2));
                                ShowContinueError("Zone dew-point temperature + safety delta T= " +
                                                  RoundSigDigits(DewPointTemp + this->CondDewPtDeltaT, 2));
                                ShowContinueErrorTimeStamp("");
                                ShowContinueError("Note that a " + RoundSigDigits(this->CondDewPtDeltaT, 4) +
                                                  " C safety was chosen in the input for the shut-off criteria");
                                ShowContinueError("Note also that this affects all surfaces that are part of this radiant system");
                            }
                            ShowRecurringWarningErrorAtEnd(cHydronicSystem + " [" + this->Name +
                                                               "] condensation shut-off occurrence continues.",
                                                           this->CondErrIndex,
                                                           DewPointTemp,
                                                           DewPointTemp,
                                                           _,
                                                           "C",
                                                           "C");
                        }
                        break; // outer do loop
                    }
                }

            } else if ((this->OperatingMode == CoolingMode) && (this->CondCtrlType == CondCtrlNone)) {

                for (RadSurfNum2 = 1; RadSurfNum2 <= this->NumOfSurfaces; ++RadSurfNum2) {
                    if (TH(2, 1, this->SurfacePtr(RadSurfNum2)) < DewPointTemp) {
                        // Condensation occurring but user does not want to shut radiant system off ever
                        this->CondCausedShutDown = true;
                    }
                }

            } else if ((this->OperatingMode == CoolingMode) && (this->CondCtrlType == CondCtrlVariedOff)) {

                LowestRadSurfTemp = 999.9;
                CondSurfNum = 0;
                for (RadSurfNum2 = 1; RadSurfNum2 <= this->NumOfSurfaces; ++RadSurfNum2) {
                    if (TH(2, 1, this->SurfacePtr(RadSurfNum2)) < (DewPointTemp + this->CondDewPtDeltaT)) {
                        if (TH(2, 1, this->SurfacePtr(RadSurfNum2)) < LowestRadSurfTemp) {
                            LowestRadSurfTemp = TH(2, 1, this->SurfacePtr(RadSurfNum2));
                            CondSurfNum = RadSurfNum2;
                        }
                    }
                }

                if (CondSurfNum > 0) { // Condensation predicted so let's deal with it
                    // Process here is: turn everything off and see what the resulting surface temperature is for
                    // the surface that was causing the lowest temperature.  Then, interpolate to find the flow that
                    // would still allow the system to operate without producing condensation.  Rerun the heat balance
                    // and recheck for condensation.  If condensation still exists, shut everything down.  This avoids
                    // excessive iteration and still makes an attempt to vary the flow rate.
                    // First, shut everything off...
                    FullWaterMassFlow = WaterMassFlow;
                    WaterMassFlow = 0.0;
                    SetComponentFlowRate(WaterMassFlow,
                                         this->ColdWaterInNode,
                                         this->ColdWaterOutNode,
                                         this->CWLoopNum,
                                         this->CWLoopSide,
                                         this->CWBranchNum,
                                         this->CWCompNum);
                    this->WaterMassFlowRate = WaterMassFlow;
                    for (RadSurfNum3 = 1; RadSurfNum3 <= this->NumOfSurfaces; ++RadSurfNum3) {
                        SurfNum2 = this->SurfacePtr(RadSurfNum3);
                        QRadSysSource(SurfNum2) = 0.0;
                        if (Surface(SurfNum2).ExtBoundCond > 0 && Surface(SurfNum2).ExtBoundCond != SurfNum2)
                            QRadSysSource(Surface(SurfNum2).ExtBoundCond) = 0.0; // Also zero the other side of an interzone
                    }
                    // Redo the heat balances since we have changed the heat source (set it to zero)
                    HeatBalanceSurfaceManager::CalcHeatBalanceOutsideSurf(state, state.dataConvectionCoefficients, state.files, ZoneNum);
                    HeatBalanceSurfaceManager::CalcHeatBalanceInsideSurf(state, ZoneNum);
                    // Now check all of the surface temperatures.  If any potentially have condensation, leave the system off.
                    for (RadSurfNum2 = 1; RadSurfNum2 <= this->NumOfSurfaces; ++RadSurfNum2) {
                        if (TH(2, 1, this->SurfacePtr(RadSurfNum2)) < (DewPointTemp + this->CondDewPtDeltaT)) {
                            this->CondCausedShutDown = true;
                        }
                    }
                    // If the system does not need to be shut down, then let's see if we can vary the flow based
                    // on the lowest temperature surface from before.  This will use interpolation to try a new
                    // flow rate.
                    if (!this->CondCausedShutDown) {
                        PredictedCondTemp = DewPointTemp + this->CondDewPtDeltaT;
                        ZeroFlowSurfTemp = TH(2, 1, this->SurfacePtr(CondSurfNum));
                        ReductionFrac = (ZeroFlowSurfTemp - PredictedCondTemp) / std::abs(ZeroFlowSurfTemp - LowestRadSurfTemp);
                        if (ReductionFrac < 0.0) ReductionFrac = 0.0; // Shouldn't happen as the above check should have screened this out
                        if (ReductionFrac > 1.0) ReductionFrac = 1.0; // Shouldn't happen either because condensation doesn't exist then
                        WaterMassFlow = ReductionFrac * FullWaterMassFlow;
                        SysWaterMassFlow = double(Zone(ZoneNum).Multiplier * Zone(ZoneNum).ListMultiplier) * WaterMassFlow;
                        // Got a new reduced flow rate that should work...reset loop variable and resimulate the system
                        SetComponentFlowRate(SysWaterMassFlow,
                                             this->ColdWaterInNode,
                                             this->ColdWaterOutNode,
                                             this->CWLoopNum,
                                             this->CWLoopSide,
                                             this->CWBranchNum,
                                             this->CWCompNum);
                        this->WaterMassFlowRate = SysWaterMassFlow;

                        // Go through all of the surfaces again with the new flow rate...
                        for (RadSurfNum3 = 1; RadSurfNum3 <= this->NumOfSurfaces; ++RadSurfNum3) {
                            SurfNum = this->SurfacePtr(RadSurfNum3);
                            // Determine the heat exchanger "effectiveness" term

                            EpsMdotCp = calculateHXEffectivenessTerm(SurfNum, WaterTempIn, WaterMassFlow, this->SurfaceFrac(RadSurfNum3), this->NumCircuits(RadSurfNum3));

                            if (Surface(SurfNum).HeatTransferAlgorithm == HeatTransferModel_CTF) {
                                // For documentation on coefficients, see code earlier in this subroutine
                                Ca = RadSysTiHBConstCoef(SurfNum);
                                Cb = RadSysTiHBToutCoef(SurfNum);
                                Cc = RadSysTiHBQsrcCoef(SurfNum);
                                Cd = RadSysToHBConstCoef(SurfNum);
                                Ce = RadSysToHBTinCoef(SurfNum);
                                Cf = RadSysToHBQsrcCoef(SurfNum);
                                Cg = CTFTsrcConstPart(SurfNum);
                                Ch = dataConstruction.Construct(ConstrNum).CTFTSourceQ(0);
                                Ci = dataConstruction.Construct(ConstrNum).CTFTSourceIn(0);
                                Cj = dataConstruction.Construct(ConstrNum).CTFTSourceOut(0);
                                Ck = Cg + ((Ci * (Ca + Cb * Cd) + Cj * (Cd + Ce * Ca)) / (1.0 - Ce * Cb));
                                Cl = Ch + ((Ci * (Cc + Cb * Cf) + Cj * (Cf + Ce * Cc)) / (1.0 - Ce * Cb));
                                QRadSysSource(SurfNum) = EpsMdotCp * (WaterTempIn - Ck) / (1.0 + (EpsMdotCp * Cl / Surface(SurfNum).Area));
                            } else if (Surface(SurfNum).HeatTransferAlgorithm == HeatTransferModel_CondFD) {
                                QRadSysSource(SurfNum) = EpsMdotCp * (WaterTempIn - TCondFDSourceNode(SurfNum));
                            }
                            if (Surface(SurfNum).ExtBoundCond > 0 && Surface(SurfNum).ExtBoundCond != SurfNum)
                                QRadSysSource(Surface(SurfNum).ExtBoundCond) = QRadSysSource(SurfNum); // Also set the other side of an interzone
                        }

                        // Redo the heat balances since we have changed the heat source
                        HeatBalanceSurfaceManager::CalcHeatBalanceOutsideSurf(state, state.dataConvectionCoefficients, state.files, ZoneNum);
                        HeatBalanceSurfaceManager::CalcHeatBalanceInsideSurf(state, ZoneNum);

                        // Check for condensation one more time.  If no condensation, we are done.  If there is
                        // condensation, shut things down and be done.
                        for (RadSurfNum2 = 1; RadSurfNum2 <= this->NumOfSurfaces; ++RadSurfNum2) {
                            if (this->CondCausedShutDown) break;
                            if (TH(2, 1, this->SurfacePtr(RadSurfNum2)) < (PredictedCondTemp)) {
                                // Condensation still present--must shut off radiant system
                                this->CondCausedShutDown = true;
                                WaterMassFlow = 0.0;
                                this->OperatingMode = NotOperating;
                                RadSurfNum = RadSurfNum2;
                                SetComponentFlowRate(WaterMassFlow,
                                                     this->ColdWaterInNode,
                                                     this->ColdWaterOutNode,
                                                     this->CWLoopNum,
                                                     this->CWLoopSide,
                                                     this->CWBranchNum,
                                                     this->CWCompNum);
                                this->WaterMassFlowRate = WaterMassFlow;
                                for (RadSurfNum3 = 1; RadSurfNum3 <= this->NumOfSurfaces; ++RadSurfNum3) {
                                    SurfNum2 = this->SurfacePtr(RadSurfNum3);
                                    QRadSysSource(SurfNum2) = 0.0;
                                    if (Surface(SurfNum2).ExtBoundCond > 0 && Surface(SurfNum2).ExtBoundCond != SurfNum2)
                                        QRadSysSource(Surface(SurfNum2).ExtBoundCond) = 0.0; // Also zero the other side of an interzone
                                }
                            }
                        }
                    }

                    if (this->CondCausedShutDown) {
                        // Produce a warning message so that user knows the system was shut-off due to potential for condensation
                        if (!WarmupFlag) {
                            if (this->CondErrIndex == 0) { // allow errors up to number of radiant systems
                                ShowWarningMessage(cHydronicSystem + " [" + this->Name + ']');
                                ShowContinueError("Surface [" + Surface(this->SurfacePtr(CondSurfNum)).Name +
                                                  "] temperature below dew-point temperature--potential for condensation exists");
                                ShowContinueError("Flow to the radiant system will be shut-off to avoid condensation");
                                ShowContinueError("Predicted radiant system surface temperature = " +
                                                  RoundSigDigits(TH(2, 1, this->SurfacePtr(CondSurfNum)), 2));
                                ShowContinueError("Zone dew-point temperature + safety delta T= " +
                                                  RoundSigDigits(DewPointTemp + this->CondDewPtDeltaT, 2));
                                ShowContinueErrorTimeStamp("");
                                ShowContinueError("Note that a " + RoundSigDigits(this->CondDewPtDeltaT, 4) +
                                                  " C safety was chosen in the input for the shut-off criteria");
                                ShowContinueError("Note also that this affects all surfaces that are part of this radiant system");
                            }
                            ShowRecurringWarningErrorAtEnd(cHydronicSystem + " [" + this->Name +
                                                               "] condensation shut-off occurrence continues.",
                                                           this->CondErrIndex,
                                                           DewPointTemp,
                                                           DewPointTemp,
                                                           _,
                                                           "C",
                                                           "C");
                        }
                    }
                } // Condensation Predicted in Variable Shut-Off Control Type
            }     // In cooling mode and one of the condensation control types
        }         // There was a non-zero flow

        // Now that we have the source/sink term, we must redo the heat balances to obtain
        // the new SumHATsurf value for the zone.  Note that the difference between the new
        // SumHATsurf and the value originally calculated by the heat balance with a zero
        // source for all radiant systems in the zone is the load met by the system (approximately).
        HeatBalanceSurfaceManager::CalcHeatBalanceOutsideSurf(state, state.dataConvectionCoefficients, state.files, ZoneNum);
        HeatBalanceSurfaceManager::CalcHeatBalanceInsideSurf(state, ZoneNum);

        LoadMet = SumHATsurf(ZoneNum) - ZeroSourceSumHATsurf(ZoneNum);
    }

    void ConstantFlowRadiantSystemData::calculateLowTemperatureRadiantSystem(EnergyPlusData &state, Real64 &LoadMet)      // load met by the radiant system, in Watts
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Rick Strand
        //       DATE WRITTEN   August 2003

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine does all of the stuff that is necessary to simulate
        // a constant flow low temperature hydronic radiant heating/cooling system.
        // Calls are made to appropriate subroutines either in this module or
        // outside of it.

        // METHODOLOGY EMPLOYED:
        // Similar in many aspects to the hydronic (variable flow) radiant system
        // except that flow rate through the radiant system is constant (based on
        // the user schedule) and the inlet temperature is varied by injecting
        // more or less fluid from the main loop to achieve the desired inlet
        // temperature.

        // REFERENCES:
        // Other EnergyPlus modules
        // IBLAST-QTF research program, completed in January 1995 (unreleased)
        // Strand, R.K. 1995. "Heat Source Transfer Functions and Their Application to
        //   Low Temperature Radiant Heating Systems", Ph.D. dissertation, University
        //   of Illinois at Urbana-Champaign, Department of Mechanical and Industrial
        //   Engineering.
        // Seem, J.E. 1986. "Heat Transfer in Buildings", Ph.D. dissertation, University
        //   of Wisconsin-Madison.

        // Using/Aliasing
        using DataBranchAirLoopPlant::MassFlowTolerance;
        using DataEnvironment::CurMnDy;
        using DataEnvironment::EnvironmentName;
        using DataHeatBalance::MRT;
        using DataHeatBalance::Zone;
        using DataHeatBalance::ZoneData;
        using DataHeatBalFanSys::MAT;
        using DataHVACGlobals::SmallLoad;
        using DataLoopNode::Node;
        using FluidProperties::GetSpecificHeatGlycol;
        using General::TrimSigDigits;
        using PlantUtilities::SetComponentFlowRate;
        using ScheduleManager::GetCurrentScheduleValue;

        // SUBROUTINE PARAMETER DEFINITIONS:
        Real64 const LowCpFluidValue(100.0); // lowest allowed Cp fluid value (to avoid dividing by zero) [J/kg-K]
        static std::string const RoutineName("CalcLowTempCFloRadiantSystem");

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        Real64 CpFluid;         // Specific heat of the fluid in the radiant system
        Real64 InjectFlowRate;  // Calculated injection flow rate that will meet the inlet temperature requirement
        bool Iteration;         // FALSE when a normal solution, TRUE when it is a solution where we must also find the inlet temp
        int LoopInNode;         // Node on the loop that is the inlet to the constant flow radiant system
        Real64 OffTempCool;     // temperature at which the cooling shuts down
        Real64 OffTempHeat;     // temperature at which the heating shuts down
        Real64 PumpPartLoadRat; // Pump part load ratio (based on user schedule, or 1.0 for no schedule)
        Real64 PumpTempRise;    // Temperature rise of the fluid as it passes through the pump
        Real64 RadInTemp;       // "Desired" radiant system water inlet temperature [Celsius]
        Real64 SetPointTemp;    // temperature that will be used to control the radiant system [Celsius]
        Real64 SetPointTempHi;  // Current high point in setpoint temperature range
        Real64 SetPointTempLo;  // Current low point in setpoint temperature range
        Real64 ShaftPower;      // Amount of power expended at the pump shaft
        int SurfNum;            // Surface number in the Surface derived type for a radiant system surface
        int SurfNum2;           // Surface number in the Surface derived type for a radiant system surface
        bool SysRunning;        // TRUE when the system is running
        Real64 SysWaterInTemp;  // Fluid temperature supplied from the loop
        Real64 WaterTempHi;     // Current high point in water temperature range
        Real64 WaterTempLo;     // Current low point in water temperature range
        int ZoneNum;            // number of zone being served
        Real64 mdot;            // local temporary for water mass flow rate kg/s

        // initialize local variables
        ZoneNum = this->ZonePtr;
        SysRunning = true; // default to running and turn off only if not running
        VarOffCond = false;

        if (GetCurrentScheduleValue(this->SchedPtr) <= 0) SysRunning = false;

        if (SysRunning) { // Unit is probably on-->this section is intended to control the water
            // mass flow rate being sent to the radiant system

            // Set the current setpoint temperature (same procedure for either heating or cooling)

            SetPointTemp = this->setRadiantSystemControlTemperature();

            // Avoid problems when there is no heating or cooling control because the system only cools or heats
            if (this->HotCtrlHiTempSchedPtr > 0) {
                OffTempHeat = GetCurrentScheduleValue(this->HotCtrlHiTempSchedPtr);
            } else {
                OffTempHeat = LowTempHeating;
            }
            if (this->ColdCtrlLoTempSchedPtr > 0) {
                OffTempCool = GetCurrentScheduleValue(this->ColdCtrlLoTempSchedPtr);
            } else {
                OffTempCool = HighTempCooling;
            }

            if (SetPointTemp < OffTempHeat && this->HeatingSystem) { // Heating mode
                this->OperatingMode = HeatingMode;
            } else if (SetPointTemp > OffTempCool && this->CoolingSystem) { // Cooling mode
                this->OperatingMode = CoolingMode;
            }

            this->setOperatingModeBasedOnChangeoverDelay();

            // Now actually decide what to do based on the setpoint temperature in relation to the control temperatures
            if (this->OperatingMode == HeatingMode) { // HEATING MODE

                this->WaterMassFlowRate = this->HotWaterMassFlowRate;

                if (!this->HeatingSystem) {

                    SysRunning = false; // Can't heat unless it's a heating system

                } else { // It is a heating system so set all of the values for controls

                    SetPointTempHi = GetCurrentScheduleValue(this->HotCtrlHiTempSchedPtr);
                    SetPointTempLo = GetCurrentScheduleValue(this->HotCtrlLoTempSchedPtr);
                    if (SetPointTempHi < SetPointTempLo) {
                        ShowSevereError("Heating setpoint temperature mismatch in" + this->Name);
                        ShowContinueError("High setpoint temperature is less than low setpoint temperature--check your schedule input");
                        ShowFatalError("Preceding condition causes termination.");
                    }

                    WaterTempHi = GetCurrentScheduleValue(this->HotWaterHiTempSchedPtr);
                    WaterTempLo = GetCurrentScheduleValue(this->HotWaterLoTempSchedPtr);
                    if (WaterTempHi < WaterTempLo) {
                        ShowSevereError("Heating water temperature mismatch in" + this->Name);
                        ShowContinueError("High water temperature is less than low water temperature--check your schedule input");
                        ShowFatalError("Preceding condition causes termination.");
                    }

                    if (SetPointTemp >= SetPointTempHi) {
                        // System is above high heating setpoint so we should be able to turn the system off
                        RadInTemp = WaterTempLo;
                        SysRunning = false;
                    } else if (SetPointTemp <= SetPointTempLo) {
                        // System is running with its highest inlet temperature
                        RadInTemp = WaterTempHi;
                    } else {
                        // Interpolate to obtain the current radiant system inlet temperature
                        RadInTemp = WaterTempHi - (WaterTempHi - WaterTempLo) * (SetPointTemp - SetPointTempLo) / (SetPointTempHi - SetPointTempLo);
                    }
                }

            } else if (this->OperatingMode == CoolingMode) { // COOLING MODE

                this->WaterMassFlowRate = this->ChWaterMassFlowRate;

                if (!this->CoolingSystem) {

                    SysRunning = false; // Can't cool unless it's a cooling system

                } else { // It is a cooling system so set all of the values for controls

                    SetPointTempHi = GetCurrentScheduleValue(this->ColdCtrlHiTempSchedPtr);
                    SetPointTempLo = GetCurrentScheduleValue(this->ColdCtrlLoTempSchedPtr);
                    if (SetPointTempHi < SetPointTempLo) {
                        ShowSevereError("Cooling setpoint temperature mismatch in" + this->Name);
                        ShowContinueError("High setpoint temperature is less than low setpoint temperature--check your schedule input");
                        ShowFatalError("Preceding condition causes termination.");
                    }

                    WaterTempHi = GetCurrentScheduleValue(this->ColdWaterHiTempSchedPtr);
                    WaterTempLo = GetCurrentScheduleValue(this->ColdWaterLoTempSchedPtr);
                    if (WaterTempHi < WaterTempLo) {
                        ShowSevereError("Cooling water temperature mismatch in" + this->Name);
                        ShowContinueError("High water temperature is less than low water temperature--check your schedule input");
                        ShowFatalError("Preceding condition causes termination.");
                    }

                    if (SetPointTemp <= SetPointTempLo) {
                        // System is below low cooling setpoint so we should be able to turn the system off
                        RadInTemp = WaterTempHi;
                        SysRunning = false;
                    } else if (SetPointTemp >= SetPointTempHi) {
                        // System is running with its lowest inlet temperature
                        RadInTemp = WaterTempLo;
                    } else {
                        // Interpolate to obtain the current radiant system inlet temperature
                        RadInTemp = WaterTempHi - (WaterTempHi - WaterTempLo) * (SetPointTemp - SetPointTempLo) / (SetPointTempHi - SetPointTempLo);
                    }
                }

            } else { // System is not running because the setpoint temperature is in the "deadband"

                RadInTemp = SetPointTemp;
                SysRunning = false;
            }
        }

        if (SysRunning) {
            CpFluid = GetSpecificHeatGlycol(fluidNameWater, RadInTemp, this->GlycolIndex, RoutineName);
        }

        if ((!SysRunning) || (CpFluid < LowCpFluidValue)) {
            // Unit is off or has no load upon it OR CpFluid value is "zero" so
            // set the flow rates to zero and then simulate the components with
            // the no flow conditions
            this->OperatingMode = NotOperating;
            this->WaterMassFlowRate = 0.0;
            this->WaterInjectionRate = 0.0;
            this->WaterRecircRate = 0.0;
            this->HeatPower = 0.0;
            this->CoolPower = 0.0;
            this->PumpPower = 0.0;
            this->PumpMassFlowRate = 0.0;
            this->PumpHeattoFluid = 0.0;

            for (SurfNum = 1; SurfNum <= this->NumOfSurfaces; ++SurfNum) {
                SurfNum2 = this->SurfacePtr(SurfNum);
                QRadSysSource(SurfNum2) = 0.0;
                if (Surface(SurfNum2).ExtBoundCond > 0 && Surface(SurfNum2).ExtBoundCond != SurfNum2)
                    QRadSysSource(Surface(SurfNum2).ExtBoundCond) = 0.0; // Also zero the other side of an interzone
            }

            // turn off flow requests made during init because it is not actually running
            if (this->CWLoopNum > 0) {
                mdot = 0.0;
                SetComponentFlowRate(mdot,
                                     this->ColdWaterInNode,
                                     this->ColdWaterOutNode,
                                     this->CWLoopNum,
                                     this->CWLoopSide,
                                     this->CWBranchNum,
                                     this->CWCompNum);
            }
            if (this->HWLoopNum > 0) {
                mdot = 0.0;
                SetComponentFlowRate(mdot,
                                     this->HotWaterInNode,
                                     this->HotWaterOutNode,
                                     this->HWLoopNum,
                                     this->HWLoopSide,
                                     this->HWBranchNum,
                                     this->HWCompNum);
            }
        } else { // (SysRunning) so simulate the system...

            // Determine pump flow rate and pump heat addition
            this->PumpMassFlowRate = this->WaterMassFlowRate; // Set in InitLowTempRadiantSystem
            if (this->VolFlowSchedPtr > 0) {
                PumpPartLoadRat = GetCurrentScheduleValue(this->VolFlowSchedPtr);
            } else {
                PumpPartLoadRat = 1.0;
            }
            this->PumpPower = PumpPartLoadRat * this->NomPowerUse;
            ShaftPower = this->PumpPower * this->MotorEffic;
            // This adds the pump heat based on User input for the pump (same as in Pump module)
            // We assume that all of the heat ends up in the fluid eventually since this is a closed loop.
            this->PumpHeattoFluid =
                ShaftPower + ((this->PumpPower - ShaftPower) * this->FracMotorLossToFluid);
            if (this->PumpMassFlowRate > 0.0) {
                PumpTempRise = this->PumpHeattoFluid / (this->PumpMassFlowRate * CpFluid);
            } else {
                PumpTempRise = 0.0;
            }

            LoopReqTemp = RadInTemp - PumpTempRise; // Temperature required at the inlet of the pump to meet the temperature request

            if (this->OperatingMode == HeatingMode) {

                // in heating mode so shut down cold water flow request
                if (this->CWLoopNum > 0) {
                    mdot = 0.0;
                    SetComponentFlowRate(mdot,
                                         this->ColdWaterInNode,
                                         this->ColdWaterOutNode,
                                         this->CWLoopNum,
                                         this->CWLoopSide,
                                         this->CWBranchNum,
                                         this->CWCompNum);
                }
                LoopInNode = this->HotWaterInNode;
                SysWaterInTemp = Node(LoopInNode).Temp;
                Iteration = false;

                if ((SysWaterInTemp >= LoopReqTemp) && (Node(LoopInNode).MassFlowRateMaxAvail >= this->WaterMassFlowRate)) {
                    // Case 1: Adequate temperature and flow
                    // Best condition--loop inlet temperature greater than requested and we have enough flow.
                    // So, proceed assuming the RadInTemp requested by the controls and then figure out the
                    // mixing after the outlet radiant temperature is calculated.
                    this->WaterInletTemp = RadInTemp;
                    this->calculateLowTemperatureRadiantSystemComponents(state, LoopInNode, Iteration, LoadMet);

                    // We now have inlet and outlet temperatures--we still need to set the flow rates
                    if ((SysWaterInTemp - this->WaterOutletTemp) != 0.0) { // protect divide by zero
                        this->WaterInjectionRate =
                            (this->WaterMassFlowRate *
                             (this->WaterInletTemp - this->WaterOutletTemp) /
                             (SysWaterInTemp - this->WaterOutletTemp)) -
                            (this->PumpHeattoFluid / (CpFluid * (SysWaterInTemp - this->WaterOutletTemp)));
                    } else {
                        this->WaterInjectionRate = this->WaterMassFlowRate;
                    }
                    this->WaterRecircRate = this->WaterMassFlowRate - this->WaterInjectionRate;

                } else if ((SysWaterInTemp < LoopReqTemp) && (Node(LoopInNode).MassFlowRateMaxAvail >= this->WaterMassFlowRate)) {
                    // Case 2: Adequate flow but temperature too low
                    // Only thing to do is to reset the inlet temperature and assume that the loop will supply
                    // the entire flow to the component (no recirculation but potentially some bypass for the
                    // overall loop).  There is no way we can meet the control temperature so don't even try.
                    this->WaterInletTemp = SysWaterInTemp + PumpTempRise;
                    this->calculateLowTemperatureRadiantSystemComponents(state, LoopInNode, Iteration, LoadMet);

                    // We now have inlet and outlet temperatures--we still need to set the flow rates
                    if ((SysWaterInTemp - this->WaterOutletTemp) != 0.0) { // protect divide by zero
                        this->WaterInjectionRate =
                            (this->WaterMassFlowRate *
                             (this->WaterInletTemp - this->WaterOutletTemp) /
                             (SysWaterInTemp - this->WaterOutletTemp)) -
                            (this->PumpHeattoFluid / (CpFluid * (SysWaterInTemp - this->WaterOutletTemp)));
                    } else {
                        this->WaterInjectionRate = this->WaterMassFlowRate;
                    }
                    if (this->WaterInjectionRate > this->WaterMassFlowRate)
                        this->WaterInjectionRate = this->WaterMassFlowRate;
                    this->WaterRecircRate = 0.0; // by definition

                } else if ((SysWaterInTemp >= LoopReqTemp) && (Node(LoopInNode).MassFlowRateMaxAvail < this->WaterMassFlowRate)) {
                    // Case 3: Adequate temperature but loop flow is less than component flow
                    // This case might work out, but there is no guarantee that there is enough loop flow to
                    // mix with the recirculation flow and still provide a high enough temperature.  First
                    // step is to try the inlet temperature and flow rate as in Case 1.  If we can obtain
                    // the proper temperature inlet to the radiant system, then we are done.  If not, we
                    // have to repeat the solution for an unknown inlet temperature and a known recirculation
                    // rate.
                    this->WaterInletTemp = RadInTemp;
                    this->calculateLowTemperatureRadiantSystemComponents(state, LoopInNode, Iteration, LoadMet);

                    // Now see if we can really get that desired into temperature (RadInTemp) by solving
                    // for the flow that is injected from the loop.  A heat balance for the mixer that relates
                    // the important quantities is:
                    //   Mdotradsys*Cp*Tradsysin = Mdotloop*Cp*Tloop + (Mdotradsys-Mdotloop)*Cp*Tradsysout + PumpHeat
                    // or rearranging to get the injection flow (Mdotloop):
                    //   Mdotloop = Mdotcomp*(Tradsysin-Tradsysout)/(Tloop-Tradsysout) - PumpHeat/(Cp*(Tloop-Tradsysout))
                    // If Mdotloop from this equation is greater that the loop flow rate (Node%MassFlowRate),
                    // then we cannot meet the inlet temperature and we have to "iterate" through the
                    // alternate solution.
                    if ((SysWaterInTemp - this->WaterOutletTemp) != 0.0) { // protect divide by zero
                        InjectFlowRate =
                            (this->WaterMassFlowRate * (this->WaterInletTemp - this->WaterOutletTemp) /
                             (SysWaterInTemp - this->WaterOutletTemp)) -
                            (this->PumpHeattoFluid / (CpFluid * (SysWaterInTemp - this->WaterOutletTemp)));
                    } else {
                        InjectFlowRate = this->WaterMassFlowRate;
                    }
                    if (InjectFlowRate > Node(LoopInNode).MassFlowRateMaxAvail) {
                        // We didn't have enough flow from the loop to meet our inlet temperature request.
                        // So, set the injection rate to the loop flow and calculate the recirculation flow.
                        // Then, resimulate the radiant system using these values (it will obtain the actual
                        // inlet temperature that results from this).
                        this->WaterInjectionRate = Node(LoopInNode).MassFlowRateMaxAvail;
                        this->WaterRecircRate = this->WaterMassFlowRate - this->WaterInjectionRate;
                        this->WaterInletTemp = SysWaterInTemp + PumpTempRise;
                        Iteration = true;
                        this->calculateLowTemperatureRadiantSystemComponents(state, LoopInNode, Iteration, LoadMet);
                    } else {
                        this->WaterInjectionRate = InjectFlowRate;
                        this->WaterRecircRate = this->WaterMassFlowRate - this->WaterInjectionRate;
                    }

                } else if ((SysWaterInTemp < LoopReqTemp) && (Node(LoopInNode).MassFlowRateMaxAvail < this->WaterMassFlowRate)) {
                    // Case 4: Temperature too low and loop flow is less than component flow
                    // Worst condition--can't meet the temperature request at all.  Only thing to do is to
                    // set the loop flow and recirculation rate (known) and solve for the inlet temperature
                    // using the "iteration" solution scheme from "Case 3B" above
                    this->WaterInjectionRate = Node(LoopInNode).MassFlowRateMaxAvail;
                    this->WaterRecircRate = this->WaterMassFlowRate - this->WaterInjectionRate;
                    this->WaterInletTemp = SysWaterInTemp + PumpTempRise;
                    Iteration = true;
                    this->calculateLowTemperatureRadiantSystemComponents(state, LoopInNode, Iteration, LoadMet);
               }

            } else if (this->OperatingMode == CoolingMode) {

                // in cooling mode so shut down heating water flow request
                if (this->HWLoopNum > 0) {
                    mdot = 0.0;
                    SetComponentFlowRate(mdot,
                                         this->HotWaterInNode,
                                         this->HotWaterOutNode,
                                         this->HWLoopNum,
                                         this->HWLoopSide,
                                         this->HWBranchNum,
                                         this->HWCompNum);
                }
                LoopInNode = this->ColdWaterInNode;
                SysWaterInTemp = Node(LoopInNode).Temp;
                CFloCondIterNum = 1;
                while ((CFloCondIterNum <= 1) ||
                       ((CFloCondIterNum <= 2) && (this->CondCtrlType == CondCtrlVariedOff) && (VarOffCond))) {
                    Iteration = false;

                    if ((SysWaterInTemp <= LoopReqTemp) && (Node(LoopInNode).MassFlowRateMaxAvail >= this->WaterMassFlowRate)) {
                        // Case 1: Adequate temperature and flow
                        // Best condition--loop inlet temperature lower than requested and we have enough flow.
                        // So, proceed assuming the RadInTemp requested by the controls and then figure out the
                        // mixing after the outlet radiant temperature is calculated.

                        // This condition can also happen when LoopReqTemp has been reset  to dewpoint for condensation control
                        if (!VarOffCond) {
                            this->WaterInletTemp = RadInTemp;
                        } else {
                            this->WaterInletTemp = LoopReqTemp;
                        }
                        this->calculateLowTemperatureRadiantSystemComponents(state, LoopInNode, Iteration, LoadMet);

                        // We now have inlet and outlet temperatures--we still need to set the flow rates
                        if ((SysWaterInTemp - this->WaterOutletTemp) != 0.0) { // protect div by zero
                            this->WaterInjectionRate =
                                (this->WaterMassFlowRate *
                                 (this->WaterInletTemp - this->WaterOutletTemp) /
                                 (SysWaterInTemp - this->WaterOutletTemp)) -
                                (this->PumpHeattoFluid / (CpFluid * (SysWaterInTemp - this->WaterOutletTemp)));
                        } else {
                            this->WaterInjectionRate = this->WaterMassFlowRate;
                        }
                        this->WaterRecircRate = this->WaterMassFlowRate - this->WaterInjectionRate;

                    } else if ((SysWaterInTemp > LoopReqTemp) && (Node(LoopInNode).MassFlowRateMaxAvail >= this->WaterMassFlowRate)) {
                        // Case 2: Adequate flow but temperature too high
                        // Only thing to do is to reset the inlet temperature and assume that the loop will supply
                        // the entire flow to the component (no recirculation but potentially some bypass for the
                        // overall loop).  There is no way we can meet the control temperature so don't even try.
                        this->WaterInletTemp = SysWaterInTemp + PumpTempRise;
                        this->calculateLowTemperatureRadiantSystemComponents(state, LoopInNode, Iteration, LoadMet);

                        // We now have inlet and outlet temperatures--we still need to set the flow rates
                        if ((SysWaterInTemp - this->WaterOutletTemp) != 0.0) { // protect div by zero
                            this->WaterInjectionRate =
                                (this->WaterMassFlowRate *
                                 (this->WaterInletTemp - this->WaterOutletTemp) /
                                 (SysWaterInTemp - this->WaterOutletTemp)) -
                                (this->PumpHeattoFluid / (CpFluid * (SysWaterInTemp - this->WaterOutletTemp)));
                        } else { // no temp change present, set injection rate to full flow
                            this->WaterInjectionRate = this->WaterMassFlowRate;
                        }
                        if (this->WaterInjectionRate > this->WaterMassFlowRate)
                            this->WaterInjectionRate = this->WaterMassFlowRate;
                        this->WaterRecircRate = 0.0; // by definition

                    } else if ((SysWaterInTemp <= LoopReqTemp) && (Node(LoopInNode).MassFlowRateMaxAvail < this->WaterMassFlowRate)) {
                        // Case 3: Adequate temperature but loop flow is less than component flow
                        // This case might work out, but there is no guarantee that there is enough loop flow to
                        // mix with the recirculation flow and still provide a high enough temperature.  First
                        // step is to try the inlet temperature and flow rate as in Case 1.  If we can obtain
                        // the proper temperature inlet to the radiant system, then we are done.  If not, we
                        // have to repeat the solution for an unknown inlet temperature and a known recirculation
                        // rate.
                        // This condition might happen when LoopReqTemp has been reset  to dewpoint for condensation control
                        if (!VarOffCond) {
                            this->WaterInletTemp = RadInTemp;
                        } else {
                            this->WaterInletTemp = LoopReqTemp;
                        }
                        this->calculateLowTemperatureRadiantSystemComponents(state, LoopInNode, Iteration, LoadMet);

                        // Now see if we can really get that desired into temperature (RadInTemp) by solving
                        // for the flow that is injected from the loop.  A heat balance for the mixer that relates
                        // the important quantities is:
                        //   Mdotradsys*Cp*Tradsysin = Mdotloop*Cp*Tloop + (Mdotradsys-Mdotloop)*Cp*Tradsysout + PumpHeat
                        // or rearranging to get the injection flow (Mdotloop):
                        //   Mdotloop = Mdotcomp*(Tradsysin-Tradsysout)/(Tloop-Tradsysout) - PumpHeat/(Cp*(Tloop-Tradsysout))
                        // If Mdotloop from this equation is greater that the loop flow rate (Node%MassFlowRate),
                        // then we cannot meet the inlet temperature and we have to "iterate" through the
                        // alternate solution.
                        if ((SysWaterInTemp - this->WaterOutletTemp) != 0.0) { // protect div by zero
                            InjectFlowRate =
                                (this->WaterMassFlowRate *
                                 (this->WaterInletTemp - this->WaterOutletTemp) /
                                 (SysWaterInTemp - this->WaterOutletTemp)) -
                                (this->PumpHeattoFluid / (CpFluid * (SysWaterInTemp - this->WaterOutletTemp)));
                        } else {
                            InjectFlowRate = this->WaterMassFlowRate;
                        }
                        if (InjectFlowRate > Node(LoopInNode).MassFlowRateMaxAvail) {
                            // We didn't have enough flow from the loop to meet our inlet temperature request.
                            // So, set the injection rate to the loop flow and calculate the recirculation flow.
                            // Then, resimulate the radiant system using these values (it will obtain the actual
                            // inlet temperature that results from this).
                            this->WaterInjectionRate = Node(LoopInNode).MassFlowRateMaxAvail;
                            this->WaterRecircRate = this->WaterMassFlowRate - this->WaterInjectionRate;
                            this->WaterInletTemp = SysWaterInTemp + PumpTempRise;
                            Iteration = true;
                            this->calculateLowTemperatureRadiantSystemComponents(state, LoopInNode, Iteration, LoadMet);
                        } else {
                            this->WaterInjectionRate = InjectFlowRate;
                            this->WaterRecircRate = this->WaterMassFlowRate - this->WaterInjectionRate;
                        }

                    } else if ((SysWaterInTemp > LoopReqTemp) && (Node(LoopInNode).MassFlowRateMaxAvail < this->WaterMassFlowRate)) {
                        // Case 4: Temperature too low and loop flow is less than component flow
                        // Worst condition--can't meet the temperature request at all.  Only thing to do is to
                        // set the loop flow and recirculation rate (known) and solve for the inlet temperature
                        // using the "iteration" solution scheme from "Case 3B" above
                        this->WaterInjectionRate = Node(LoopInNode).MassFlowRateMaxAvail;
                        this->WaterRecircRate = this->WaterMassFlowRate - this->WaterInjectionRate;
                        this->WaterInletTemp = SysWaterInTemp + PumpTempRise;
                        Iteration = true;
                        this->calculateLowTemperatureRadiantSystemComponents(state, LoopInNode, Iteration, LoadMet);
                    }

                    ++CFloCondIterNum;
                }

            } // Operating mode (heating or cooling)

            // Case when system has been shut down because of condensation issues or other limitations:
            if (this->WaterMassFlowRate < MassFlowTolerance) {
                this->WaterMassFlowRate = 0.0;
                this->WaterInjectionRate = 0.0;
                this->WaterRecircRate = 0.0;
                this->PumpMassFlowRate = 0.0;
                this->OperatingMode = NotOperating;
            }

            // There are some cases when the pump heat is actually enough to provide all the heating that the system needs.
            // In this case, the water injection flow rate will come back as a slightly negative number.  Reset it to zero
            // and just recirculate all the flow through the local loop.
            if (this->WaterInjectionRate < 0.0) {
                this->WaterInjectionRate = 0.0;
                this->WaterRecircRate = this->WaterMassFlowRate;
            }

            // Error check, just in case
            if (this->WaterRecircRate < 0.0) {
                ShowWarningError("Flow mismatch in radiant system--result will be an energy imbalance--should not get this error");
                ShowContinueErrorTimeStamp("WaterRecircRate=" + TrimSigDigits(this->WaterRecircRate, 2) +
                                           ", in Radiant System=" + this->Name + ',');
                this->WaterRecircRate = 0.0;
                this->WaterInjectionRate = this->WaterMassFlowRate;
            }

        } // System running mode (yes or no)
    }

    void ConstantFlowRadiantSystemData::calculateLowTemperatureRadiantSystemComponents
                                            (EnergyPlusData &state, int const MainLoopNodeIn, // Node number on main loop of the inlet node to the radiant system
                                             bool const Iteration,     // FALSE for the regular solution, TRUE when we had to loop back
                                             Real64 &LoadMet           // Load met by the low temperature radiant system, in Watts
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Rick Strand
        //       DATE WRITTEN   August 2003
        //       MODIFIED       Sep 2011 LKL/BG - resimulate only zones needing it for Radiant systems

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine solves the radiant system based on how much water is (and
        // the conditions of the water) supplied to the radiant system.  The purpose
        // of this subroutine is similar to CalcLowTempHydrRadSysComps except that
        // it solves this for a constant flow hydronic radiant system.

        // METHODOLOGY EMPLOYED:
        // Use heat exchanger formulas to obtain the heat source/sink for the radiant
        // system based on the inlet conditions and flow rate of water.  Once that is
        // determined, recalculate the surface heat balances to reflect this heat
        // addition/subtraction.  The load met by the system is determined by the
        // difference between the convection from all surfaces in the zone when
        // there was no radiant system output and with a source/sink added.

        // REFERENCES:
        // IBLAST-QTF research program, completed in January 1995 (unreleased)
        // Strand, R.K. 1995. "Heat Source Transfer Functions and Their Application to
        //   Low Temperature Radiant Heating Systems", Ph.D. dissertation, University
        //   of Illinois at Urbana-Champaign, Department of Mechanical and Industrial
        //   Engineering.

        // Using/Aliasing
        using DataEnvironment::OutBaroPress;
        using DataHeatBalance::Zone;
        using DataHeatBalFanSys::CTFTsrcConstPart;
        using DataHeatBalFanSys::RadSysTiHBConstCoef;
        using DataHeatBalFanSys::RadSysTiHBQsrcCoef;
        using DataHeatBalFanSys::RadSysTiHBToutCoef;
        using DataHeatBalFanSys::RadSysToHBConstCoef;
        using DataHeatBalFanSys::RadSysToHBQsrcCoef;
        using DataHeatBalFanSys::RadSysToHBTinCoef;
        using DataHeatBalFanSys::ZoneAirHumRat;
        using DataHeatBalSurface::TH;
        using DataLoopNode::Node;
        using DataSurfaces::HeatTransferModel_CondFD;
        using DataSurfaces::HeatTransferModel_CTF;
        using DataSurfaces::Surface;
        using FluidProperties::GetSpecificHeatGlycol;
        using General::RoundSigDigits;
        using PlantUtilities::SetComponentFlowRate;

        // SUBROUTINE PARAMETER DEFINITIONS:
        Real64 const TempCheckLimit(0.1); // Maximum allowed temperature difference between outlet temperature calculations
        Real64 const ZeroSystemResp(0.1); // Response below which the system response is really zero
        static std::string const RoutineName("CalcLowTempCFloRadSysComps");

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int ConstrNum;                // Index for construction number in Construct derived type
        Real64 Cp;                    // Intermediate calculational variable for specific heat of water
        Real64 DewPointTemp;          // Dew-point temperature based on the zone air conditions
        Real64 EpsMdotCp;             // Epsilon (heat exchanger terminology) times water mass flow rate times water specific heat
        Real64 LoopTerm;              // Intermeidate calculation variable for determining the water inlet temperature
        Real64 Mdot;                  // Intermediate calculation variable for mass flow rate in a surface within the radiant system
        int RadSurfNum;               // DO loop counter for the surfaces that comprise a particular radiant system
        int RadSurfNum2;              // DO loop counter for the surfaces that comprise a particular radiant system
        int RadSurfNum3;              // DO loop counter for the surfaces that comprise a particular radiant system
        Real64 RecircTerm;            // Intermeidate calculation variable for determining the water inlet temperature
        Real64 SumFlowFracCkCm;       // Summation of surface flow fraction, Ck, and Cm product for each surface in the system
        Real64 SumFlowFracOneMinusCm; // Summation of surface flow fraction times (1-Cm) for each surface in the radiant system
        int SurfNum;                  // Index for radiant surface in Surface derived type
        int SurfNum2;                 // Index for radiant surface in Surface derived type
        Real64 TotalRadSysPower;      // Total heat source/sink to radiant system
        Real64 TwiCoeff;              // Intermeidate calculation variable for determining the water inlet temperature
        Real64 WaterMassFlow;         // Water mass flow rate in the radiant system, kg/s
        int WaterNodeIn;              // Node number of the water entering the radiant system
        Real64 WaterOutletTempCheck;  // Radiant system water outlet temperature (calculated from mixing all outlet streams together)
        Real64 WaterTempIn;           // Temperature of the water entering the radiant system, in C
        int ZoneNum;                  // number of zone being served
        Real64 ZoneMult;              // Zone multiplier for this system

        Real64 Ca; // Coefficients to relate the inlet water temperature to the heat source
        Real64 Cb;
        Real64 Cc;
        Real64 Cd;
        Real64 Ce;
        Real64 Cf;
        Real64 Cg;
        Real64 Ch;
        Real64 Ci;
        Real64 Cj;
        Real64 Ck;
        Real64 Cl;
        // For more info on Ca through Cl, see comments below

        static Array1D<Real64> Ckj; // Coefficients for individual surfaces within a radiant system
        static Array1D<Real64> Cmj;
        static Array1D<Real64> WaterTempOut; // Array of outlet water temperatures for
                                             // each surface in the radiant system

        // First, apply heat exchanger logic to find the heat source/sink to the system.
        // This involves finding out the heat transfer characteristics of the hydronic
        // loop and then applying the equations derived on pp. 113-118 of the dissertation.
        if (FirstTimeFlag) {
            Ckj.allocate(MaxCloNumOfSurfaces);
            Cmj.allocate(MaxCloNumOfSurfaces);
            WaterTempOut.allocate(MaxCloNumOfSurfaces);
            FirstTimeFlag = false;
        }

        Ckj = 0.0;
        Cmj = 0.0;
        WaterTempOut = this->WaterInletTemp;

        // Set the conditions on the water side inlet
        {
            auto const SELECT_CASE_var(this->OperatingMode);
            if (SELECT_CASE_var == HeatingMode) {
                WaterNodeIn = this->HotWaterInNode;
            } else if (SELECT_CASE_var == CoolingMode) {
                WaterNodeIn = this->ColdWaterInNode;
            } else {
                ShowSevereError("Illegal low temperature radiant system operating mode");
                ShowContinueError("Occurs in Radiant System=" + this->Name);
                ShowFatalError("Preceding condition causes termination.");
            }
        }
        ZoneNum = this->ZonePtr;
        ZoneMult = double(Zone(ZoneNum).Multiplier * Zone(ZoneNum).ListMultiplier);
        WaterMassFlow = this->WaterMassFlowRate / ZoneMult;
        WaterTempIn = this->WaterInletTemp;

        if (WaterMassFlow <= 0.0) {
            // No flow or below minimum allowed so there is no heat source/sink
            // This is possible with a mismatch between system and plant operation
            // or a slight mismatch between zone and system controls.  This is not
            // necessarily a "problem" so this exception is necessary in the code.
            for (RadSurfNum = 1; RadSurfNum <= this->NumOfSurfaces; ++RadSurfNum) {
                SurfNum = this->SurfacePtr(RadSurfNum);
                QRadSysSource(SurfNum) = 0.0;
                if (Surface(SurfNum).ExtBoundCond > 0 && Surface(SurfNum).ExtBoundCond != SurfNum)
                    QRadSysSource(Surface(SurfNum).ExtBoundCond) = 0.0; // Also zero the other side of an interzone
            }

            this->WaterOutletTemp = this->WaterInletTemp;

        } else {

            for (RadSurfNum = 1; RadSurfNum <= this->NumOfSurfaces; ++RadSurfNum) {
                SurfNum = this->SurfacePtr(RadSurfNum);
                // Determine the heat exchanger "effectiveness" term

                EpsMdotCp = calculateHXEffectivenessTerm(SurfNum, WaterTempIn, WaterMassFlow, this->SurfaceFrac(RadSurfNum), this->NumCircuits(RadSurfNum));

                // Obtain the heat balance coefficients and calculate the intermediate coefficients
                // linking the inlet water temperature to the heat source/sink to the radiant system.
                // The coefficients are based on the following development...
                // The heat balance equations at the outside and inside surfaces are of the form:
                //   Tinside  = Ca + Cb*Toutside + Cc*q"
                //   Toutside = Cd + Ce*Tinside  + Cf*q"
                //   Tsource  = Cg + Ch*q"       + Ci*Tinside + Cj*Toutside
                // where:
                //   Tinside is the temperature at the inside surface
                //   Toutside is the temperature at the outside surface
                //   Tsource is the temperature within the radiant system at the location of the source/sink
                //   Ca is all of the other terms in the inside heat balance (solar, LW exchange, conduction history terms, etc.)
                //   Cb is the current cross CTF term
                //   Cc is the QTF inside term for the current heat source/sink
                //   Cd is all of the other terms in the outside heat balance (solar, LW exchange, conduction history terms, etc.)
                //   Ce is the current cross CTF term (should be equal to Cb)
                //   Cf is the QTF outside term for the current heat source/sink
                //   Cg is the summation of all temperature and source history terms at the source/sink location
                //   Ch is the QTF term at the source/sink location for the current heat source/sink
                //   Ci is the CTF inside term for the current inside surface temperature
                //   Cj is the CTF outside term for the current outside surface temperature
                // Note that it is necessary to not use "slow conduction" assumptions because the
                // source/sink has an impact on BOTH the inside and outside surface heat balances.
                // Hence the more general formulation.
                // The first two T equations above can be solved to remove the other surface temperature.
                // This results in the following equations:
                //   Tinside  = Ca + Cb*(Cd + Ce*Tinside + Cf*q") + Cc*q"   or...
                //   Tinside  = (Ca + Cb*Cd + (Cc+Cb*Cf)*q") / (1 - Ce*Cb)
                //   Toutside = Cd + Ce*(Ca + Cb*Toutside + Cc*q") + Cf*q"  or...
                //   Toutside = (Cd + Ce*Ca + (Cf+Ce*Cc)*q") / (1 - Ce*Cb)
                // Substituting the new equations for Tinside and Toutside as a function of C and q"
                // into the equation for Tsource...
                //   Tsource  = Cg + Ch*q" + Ci*((Ca + Cb*Cd + (Cc+Cb*Cf)*q") / (1 - Ce*Cb)) &
                //                         + Cj*((Cd + Ce*Ca + (Cf+Ce*Cc)*q") / (1 - Ce*Cb))
                // Or rearranging this to get Tsource as a function of q", we get...
                //   Tsource  =  Cg + ((Ci*(Ca + Cb*Cd) + Cj*(Cd + Ce*Ca))/(1-Ce*Cb)) &
                //             +(Ch + ((Ci*(Cc + Cb*Cf) + Cj*(Cf + Ce*Cc))/(1-Ce*Cb)))*q"
                // Or in a slightly simpler form...
                //   Tsource  = Ck + Cl*q"
                // where:
                //   Ck = Cg + ((Ci*(Ca + Cb*Cd) + Cj*(Cd + Ce*Ca))/(1-Ce*Cb))
                //   Cl = Ch + ((Ci*(Cc + Cb*Cf) + Cj*(Cf + Ce*Cc))/(1-Ce*Cb))
                // Note also that from heat exchanger "algebra", we have:
                //   q = epsilon*qmax    and    qmax = Mdot*Cp*(Twaterin-Tsource)
                // So...
                //   q" = q/Area = (epsilon*Mdot*Cp/Area)*(Twaterin-Tsource)
                // Or rearranging this equation:
                //   Tsource = -(q"*A/(epsilon*Mdot*Cp)) + Twaterin
                // Setting this equation equal to the other equation for Tsource a couple lines up
                // and rearranging to solve for q"...
                //   q" = (Twaterin - Ck) / (Cl + (A/(epsilon*Mdot*Cp))
                // or
                //   q  = (Twaterin - Ck) / ((Cl/A) + (1/epsilon*Mdot*Cp))
                // or
                //   q  = epsilon*Mdot*Cp*(Twaterin - Ck) / (1+(epsilon*Mdot*Cp*Cl/A))
                // which is the desired result, that is the heat source or sink to the radiant
                // system as a function of the water inlet temperature (flow rate is also in there
                // as well as all of the heat balance terms "hidden" in Ck and Cl).

                ConstrNum = Surface(SurfNum).Construction;

                Ca = RadSysTiHBConstCoef(SurfNum);
                Cb = RadSysTiHBToutCoef(SurfNum);
                Cc = RadSysTiHBQsrcCoef(SurfNum);

                Cd = RadSysToHBConstCoef(SurfNum);
                Ce = RadSysToHBTinCoef(SurfNum);
                Cf = RadSysToHBQsrcCoef(SurfNum);

                Cg = CTFTsrcConstPart(SurfNum);
                Ch = dataConstruction.Construct(ConstrNum).CTFTSourceQ(0);
                Ci = dataConstruction.Construct(ConstrNum).CTFTSourceIn(0);
                Cj = dataConstruction.Construct(ConstrNum).CTFTSourceOut(0);

                Ck = Cg + ((Ci * (Ca + Cb * Cd) + Cj * (Cd + Ce * Ca)) / (1.0 - Ce * Cb));
                Cl = Ch + ((Ci * (Cc + Cb * Cf) + Cj * (Cf + Ce * Cc)) / (1.0 - Ce * Cb));

                Mdot = WaterMassFlow * this->SurfaceFrac(RadSurfNum);
                Cp = GetSpecificHeatGlycol(fluidNameWater, WaterTempIn, this->GlycolIndex, RoutineName);

                if (!Iteration) {

                    if (Surface(SurfNum).HeatTransferAlgorithm == HeatTransferModel_CTF)
                        QRadSysSource(SurfNum) = EpsMdotCp * (WaterTempIn - Ck) / (1.0 + (EpsMdotCp * Cl / Surface(SurfNum).Area));

                    if (Surface(SurfNum).HeatTransferAlgorithm == HeatTransferModel_CondFD)
                        QRadSysSource(SurfNum) = EpsMdotCp * (WaterTempIn - TCondFDSourceNode(SurfNum));

                    if (Surface(SurfNum).ExtBoundCond > 0 && Surface(SurfNum).ExtBoundCond != SurfNum)
                        QRadSysSource(Surface(SurfNum).ExtBoundCond) = QRadSysSource(SurfNum); // Also set the other side of an interzone
                    WaterTempOut(RadSurfNum) = WaterTempIn - (QRadSysSource(SurfNum) / (Mdot * Cp));
                } else { // (Iteration)
                    // In this case, we did not know the inlet temperature directly and have
                    // to figure it out as part of the solution.  Thus, we have to do a little
                    // more algebra.
                    // The last equation in the previous block was:
                    //   q = epsilon*Mdot*Cp*(Twaterin - Ck) / (1+(epsilon*Mdot*Cp*Cl/A))
                    // which combines with:
                    //   q = Mdot*Cp*(Twaterin - Twaterout,j)
                    // so that:
                    //   (Twaterin - Twaterout.j) = epsilon*(Twaterin - Ck) / (1+(epsilon*Mdot*Cp*Cl/A))
                    // Let:
                    //   Cm = epsilonj / (1+(epsilonj*Mdot,j*Cp*Cl,j/A))
                    // for each surface in the radiant system.  This results in:
                    //   (Twaterin - Twaterout,j) = Cm,j*(Twaterin - Ck,j)
                    // Or:
                    //   Twaterout,j = (1 - Cm,j)*Twaterin + Cm,j*Ck,j
                    // This holds for each surface that is part of the radiant system (j).  To get the
                    // overall outlet temperature, we have to do a mixing calculation after all of the
                    // surfaces have been simulated:
                    //   Twaterout = SUM(Fractionj*Twaterout,j)
                    // We also have to solve an energy balance at the mixing valve and add in pump heat.
                    // The energy balance at the mixing valve relates the loop inlet temperature (Tloopin)
                    // and the overall outlet temperature (Twaterout):
                    //   Tpumpin = (Mdotloop/Mdotradsys)*Tloopin + (Mdotrecirc/Mdotradsys)*Twaterout
                    // This can then be related to the inlet water temperature to the radiant system
                    // after pump heat has been taken into account:
                    //   Twaterin = (Mdotloop/Mdotradsys)*Tloopin + (Mdotrecirc/Mdotradsys)*Twaterout + PumpHeat/(Mdotradsys*Cp)
                    // Pluggin in the definition of Twaterout (sum equation above) and then the definition
                    // of each individual Twaterout,j equation (which is solely a function of Twaterin
                    // and coefficients), we can obtain an equation for Twaterin that consists of all
                    // known quantities.  This requires us to calculate Ck,j and Cm,j for all the radiant
                    // surfaces in the system first and then coming up with a calculation for Twaterin.
                    // After than, individual Twaterout,j can be calculated along with QRadSysSource.
                    Ckj(RadSurfNum) = Ck;
                    Cmj(RadSurfNum) = (EpsMdotCp / (Mdot * Cp)) / (1.0 + (EpsMdotCp * Cl / Surface(SurfNum).Area));

                    if (RadSurfNum == this->NumOfSurfaces) { // Last one so we can now do the other calculations
                        // Equation for Twaterin is:
                        //   Twaterin = (LoopTerm + RecircTerm)/(TwiCoeff)
                        // where:
                        //   LoopTerm   = (Mdotloop/Mdotradsys)*Tloopin + PumpHeat/(Mdotradsys*Cp)
                        //   RecircTerm = (Mdotrecirc/Mdotradsys)*SUM(FlowFracj*Ck,j*Cm,j)
                        //   TwiCoeff   = 1 - (Mdotrecirc/Mdotradsys)*SUM(FlowFracj*(1 - Cm,j))
                        SumFlowFracCkCm = 0.0;
                        SumFlowFracOneMinusCm = 0.0;
                        for (RadSurfNum2 = 1; RadSurfNum2 <= this->NumOfSurfaces; ++RadSurfNum2) {
                            SumFlowFracCkCm += (this->SurfaceFrac(RadSurfNum2) * Ckj(RadSurfNum) * Cmj(RadSurfNum2));
                            SumFlowFracOneMinusCm += (this->SurfaceFrac(RadSurfNum2) * (1.0 - Cmj(RadSurfNum2)));
                        }

                        LoopTerm = (this->WaterInjectionRate / this->WaterMassFlowRate) * Node(MainLoopNodeIn).Temp +
                                   (this->PumpHeattoFluid / (this->WaterMassFlowRate * Cp));

                        RecircTerm = (this->WaterRecircRate / this->WaterMassFlowRate) * SumFlowFracCkCm;

                        TwiCoeff = 1.0 - (this->WaterRecircRate / this->WaterMassFlowRate) * SumFlowFracOneMinusCm;

                        WaterTempIn = (LoopTerm + RecircTerm) / (TwiCoeff);

                        this->WaterInletTemp = WaterTempIn;

                        for (RadSurfNum2 = 1; RadSurfNum2 <= this->NumOfSurfaces; ++RadSurfNum2) {
                            WaterTempOut(RadSurfNum2) = WaterTempIn * (1.0 - Cmj(RadSurfNum2)) + (Ckj(RadSurfNum2) * Cmj(RadSurfNum2));
                            Mdot = WaterMassFlow * this->SurfaceFrac(RadSurfNum2);
                            SurfNum = this->SurfacePtr(RadSurfNum2);
                            QRadSysSource(SurfNum) = Mdot * Cp * (WaterTempIn - WaterTempOut(RadSurfNum2));
                            if (Surface(SurfNum).ExtBoundCond > 0 && Surface(SurfNum).ExtBoundCond != SurfNum)
                                QRadSysSource(Surface(SurfNum).ExtBoundCond) = QRadSysSource(SurfNum); // Also set the other side of an interzone
                        }
                    }
                }
            }

            for (RadSurfNum = 1; RadSurfNum <= this->NumOfSurfaces; ++RadSurfNum) {
                SurfNum = this->SurfacePtr(RadSurfNum);
                // "Temperature Comparison" Cut-off:
                // Check to see whether or not the system should really be running.  If
                // QRadSysSource is negative when we are in heating mode or QRadSysSource
                // is positive when we are in cooling mode, then the radiant system will
                // be doing the opposite of its intention.  In this case, the flow rate
                // is set to zero to avoid heating in cooling mode or cooling in heating
                // mode.
                if (((this->OperatingMode == HeatingMode) && (QRadSysSource(SurfNum) <= 0.0)) ||
                    ((this->OperatingMode == CoolingMode) && (QRadSysSource(SurfNum) >= 0.0))) {
                    WaterMassFlow = 0.0;
                    if (this->OperatingMode == HeatingMode) {
                        SetComponentFlowRate(WaterMassFlow,
                                             this->HotWaterInNode,
                                             this->HotWaterOutNode,
                                             this->HWLoopNum,
                                             this->HWLoopSide,
                                             this->HWBranchNum,
                                             this->HWCompNum);
                    } else if (this->OperatingMode == CoolingMode) {
                        SetComponentFlowRate(WaterMassFlow,
                                             this->ColdWaterInNode,
                                             this->ColdWaterOutNode,
                                             this->CWLoopNum,
                                             this->CWLoopSide,
                                             this->CWBranchNum,
                                             this->CWCompNum);
                    }
                    this->WaterMassFlowRate = WaterMassFlow;
                    this->OperatingMode = NotOperating;
                    for (RadSurfNum2 = 1; RadSurfNum2 <= this->NumOfSurfaces; ++RadSurfNum2) {
                        SurfNum2 = this->SurfacePtr(RadSurfNum2);
                        QRadSysSource(SurfNum2) = 0.0;
                        if (Surface(SurfNum2).ExtBoundCond > 0 && Surface(SurfNum2).ExtBoundCond != SurfNum2)
                            QRadSysSource(Surface(SurfNum2).ExtBoundCond) = 0.0; // Also zero the other side of an interzone
                    }
                    break; // outer do loop
                }
            }
            // Condensation Cut-off:
            // Check to see whether there are any surface temperatures within the radiant system that have
            // dropped below the dew-point temperature.  If so, we need to shut off this radiant system.
            // A safety parameter is added (hardwired parameter) to avoid getting too close to condensation
            // conditions.
            this->CondCausedShutDown = false;
            DewPointTemp = PsyTdpFnWPb(ZoneAirHumRat(this->ZonePtr), OutBaroPress);

            if ((this->OperatingMode == CoolingMode) && (this->CondCtrlType == CondCtrlSimpleOff)) {

                for (RadSurfNum2 = 1; RadSurfNum2 <= this->NumOfSurfaces; ++RadSurfNum2) {
                    if (TH(2, 1, this->SurfacePtr(RadSurfNum2)) < (DewPointTemp + this->CondDewPtDeltaT)) {
                        // Condensation warning--must shut off radiant system
                        this->CondCausedShutDown = true;
                        WaterMassFlow = 0.0;
                        this->OperatingMode = NotOperating;
                        SetComponentFlowRate(WaterMassFlow,
                                             this->ColdWaterInNode,
                                             this->ColdWaterOutNode,
                                             this->CWLoopNum,
                                             this->CWLoopSide,
                                             this->CWBranchNum,
                                             this->CWCompNum);
                        this->WaterMassFlowRate = WaterMassFlow;
                        for (RadSurfNum3 = 1; RadSurfNum3 <= this->NumOfSurfaces; ++RadSurfNum3) {
                            SurfNum2 = this->SurfacePtr(RadSurfNum3);
                            QRadSysSource(SurfNum2) = 0.0;
                            if (Surface(SurfNum2).ExtBoundCond > 0 && Surface(SurfNum2).ExtBoundCond != SurfNum2)
                                QRadSysSource(Surface(SurfNum2).ExtBoundCond) = 0.0; // Also zero the other side of an interzone
                        }
                        // Produce a warning message so that user knows the system was shut-off due to potential for condensation
                        if (!WarmupFlag) {
                            if (this->CondErrIndex == 0) { // allow errors up to number of radiant systems
                                ShowWarningMessage(cConstantFlowSystem + " [" + this->Name + ']');
                                ShowContinueError("Surface [" + Surface(this->SurfacePtr(RadSurfNum2)).Name +
                                                  "] temperature below dew-point temperature--potential for condensation exists");
                                ShowContinueError("Flow to the radiant system will be shut-off to avoid condensation");
                                ShowContinueError("Predicted radiant system surface temperature = " +
                                                  RoundSigDigits(TH(2, 1, this->SurfacePtr(RadSurfNum2)), 2));
                                ShowContinueError("Zone dew-point temperature + safety delta T= " +
                                                  RoundSigDigits(DewPointTemp + this->CondDewPtDeltaT, 2));
                                ShowContinueErrorTimeStamp("");
                                ShowContinueError("Note that a " + RoundSigDigits(this->CondDewPtDeltaT, 4) +
                                                  " C safety was chosen in the input for the shut-off criteria");
                                ShowContinueError("Note also that this affects all surfaces that are part of this radiant system");
                            }
                            ShowRecurringWarningErrorAtEnd(cConstantFlowSystem + " [" + this->Name +
                                                               "] condensation shut-off occurrence continues.",
                                                           this->CondErrIndex,
                                                           DewPointTemp,
                                                           DewPointTemp,
                                                           _,
                                                           "C",
                                                           "C");
                        }
                        break; // outer do loop
                    }
                }

            } else if ((this->OperatingMode == CoolingMode) && (this->CondCtrlType == CondCtrlNone)) {

                for (RadSurfNum2 = 1; RadSurfNum2 <= this->NumOfSurfaces; ++RadSurfNum2) {
                    if (TH(2, 1, this->SurfacePtr(RadSurfNum2)) < DewPointTemp) {
                        // Condensation occurring but user does not want to shut radiant system off ever
                        this->CondCausedShutDown = true;
                    }
                }

            } else if ((this->OperatingMode == CoolingMode) && (this->CondCtrlType == CondCtrlVariedOff)) {

                for (RadSurfNum2 = 1; RadSurfNum2 <= this->NumOfSurfaces; ++RadSurfNum2) {
                    if (TH(2, 1, this->SurfacePtr(RadSurfNum2)) < (DewPointTemp + this->CondDewPtDeltaT)) {
                        VarOffCond = true;
                        if (CFloCondIterNum >= 2) {
                            // We have already iterated once so now we must shut off radiant system
                            this->CondCausedShutDown = true;
                            WaterMassFlow = 0.0;
                            this->OperatingMode = NotOperating;
                            SetComponentFlowRate(WaterMassFlow,
                                                 this->ColdWaterInNode,
                                                 this->ColdWaterOutNode,
                                                 this->CWLoopNum,
                                                 this->CWLoopSide,
                                                 this->CWBranchNum,
                                                 this->CWCompNum);
                            this->WaterMassFlowRate = WaterMassFlow;
                            for (RadSurfNum3 = 1; RadSurfNum3 <= this->NumOfSurfaces; ++RadSurfNum3) {
                                SurfNum2 = this->SurfacePtr(RadSurfNum3);
                                QRadSysSource(SurfNum2) = 0.0;
                                if (Surface(SurfNum2).ExtBoundCond > 0 && Surface(SurfNum2).ExtBoundCond != SurfNum2)
                                    QRadSysSource(Surface(SurfNum2).ExtBoundCond) = 0.0; // Also zero the other side of an interzone
                            }
                            // Produce a warning message so that user knows the system was shut-off due to potential for condensation
                            if (!WarmupFlag) {
                                if (this->CondErrIndex == 0) { // allow errors up to number of radiant systems
                                    ShowWarningMessage(cConstantFlowSystem + " [" + this->Name + ']');
                                    ShowContinueError("Surface [" + Surface(this->SurfacePtr(RadSurfNum2)).Name +
                                                      "] temperature below dew-point temperature--potential for condensation exists");
                                    ShowContinueError("Flow to the radiant system will be shut-off to avoid condensation");
                                    ShowContinueError("Predicted radiant system surface temperature = " +
                                                      RoundSigDigits(TH(2, 1, this->SurfacePtr(RadSurfNum2)), 2));
                                    ShowContinueError("Zone dew-point temperature + safety delta T= " +
                                                      RoundSigDigits(DewPointTemp + this->CondDewPtDeltaT, 2));
                                    ShowContinueErrorTimeStamp("");
                                    ShowContinueError("Note that a " + RoundSigDigits(this->CondDewPtDeltaT, 4) +
                                                      " C safety was chosen in the input for the shut-off criteria");
                                    ShowContinueError("Note also that this affects all surfaces that are part of this radiant system");
                                }
                                ShowRecurringWarningErrorAtEnd(cConstantFlowSystem + " [" + this->Name +
                                                                   "] condensation shut-off occurrence continues.",
                                                               this->CondErrIndex,
                                                               DewPointTemp,
                                                               DewPointTemp,
                                                               _,
                                                               "C",
                                                               "C");
                            }
                            break; // outer do loop
                        } else {   // (First iteration--reset loop required temperature and try again to avoid condensation)
                            LoopReqTemp = DewPointTemp + this->CondDewPtDeltaT;
                        }
                    }
                }
            }

            // Determine radiant system outlet temperature (two ways to calculate--use as a check)
            WaterOutletTempCheck = 0.0;
            TotalRadSysPower = 0.0;
            for (RadSurfNum = 1; RadSurfNum <= this->NumOfSurfaces; ++RadSurfNum) {
                SurfNum = this->SurfacePtr(RadSurfNum);
                TotalRadSysPower += QRadSysSource(SurfNum);
                WaterOutletTempCheck += (this->SurfaceFrac(RadSurfNum) * WaterTempOut(RadSurfNum));
            }
            TotalRadSysPower *= ZoneMult;

            if (this->WaterMassFlowRate > 0.0) {
                Cp = GetSpecificHeatGlycol(fluidNameWater, WaterTempIn, this->GlycolIndex, RoutineName);
                this->WaterOutletTemp = this->WaterInletTemp - (TotalRadSysPower / (this->WaterMassFlowRate * Cp));
                if ((std::abs(this->WaterOutletTemp - WaterOutletTempCheck) > TempCheckLimit) &&
                    (std::abs(TotalRadSysPower) > ZeroSystemResp)) {
                    // If the total system power is zero, that means we have shut down and the temperatures won't match because of that
                    ShowWarningError("Radiant system water outlet temperature calculation mismatch--this should not happen");
                }
            } else {
                this->WaterOutletTemp = this->WaterInletTemp;
            }
        }

        // Now that we have the source/sink term(s), we must redo the heat balances to obtain
        // the new SumHATsurf value for the zone.  Note that the difference between the new
        // SumHATsurf and the value originally calculated by the heat balance with a zero
        // source for all radiant systems in the zone is the load met by the system (approximately).
        HeatBalanceSurfaceManager::CalcHeatBalanceOutsideSurf(state, state.dataConvectionCoefficients, state.files, ZoneNum);
        HeatBalanceSurfaceManager::CalcHeatBalanceInsideSurf(state, ZoneNum);

        LoadMet = SumHATsurf(this->ZonePtr) - ZeroSourceSumHATsurf(this->ZonePtr);

    }

    void ConstantFlowRadiantSystemData::calculateRunningMeanAverageTemperature()
    {
        // This routine grabs the current weather data since it is currently available at this point in the simulation.  Note, however,
        // that the formula that calculates the running mean average (dry-bulb) temperature uses the values from "yesterday".  So, today's
        // values are calculated and then shifted at the beginning of the next day to the tomorrow variables.  It is these tomorrow variables
        // that are then used in the formula.  So, that is why some of the assignments are done in the order that they are in below.
        if (DataGlobals::DayOfSim == 1 && DataGlobals::WarmupFlag) {
            // there is no "history" here--assume everything that came before was the same (this applies to design days also--weather is always the same
            this->todayAverageOutdoorDryBulbTemperature = this->calculateCurrentDailyAverageODB();
            this->yesterdayAverageOutdoorDryBulbTemperature = this->todayAverageOutdoorDryBulbTemperature;
            this->todayRunningMeanOutdoorDryBulbTemperature = this->todayAverageOutdoorDryBulbTemperature;
            this->yesterdayRunningMeanOutdoorDryBulbTemperature = this->todayAverageOutdoorDryBulbTemperature;
        } else if (!DataGlobals::WarmupFlag && DataGlobals::NumOfDayInEnvrn > 1) {
            // This is an environment with more than one day (non-design day) so...
            // First update yesterday's information using what was previously calculated for "today"
            this->yesterdayAverageOutdoorDryBulbTemperature = this->todayAverageOutdoorDryBulbTemperature;
            this->yesterdayRunningMeanOutdoorDryBulbTemperature = this->todayRunningMeanOutdoorDryBulbTemperature;
            // Now update the running mean and average outdoor air temperatures
            this->todayRunningMeanOutdoorDryBulbTemperature = (1.0 - this->runningMeanOutdoorAirTemperatureWeightingFactor) * this->yesterdayAverageOutdoorDryBulbTemperature
                                                              + this->runningMeanOutdoorAirTemperatureWeightingFactor * this->yesterdayRunningMeanOutdoorDryBulbTemperature;
            this->todayAverageOutdoorDryBulbTemperature = this->calculateCurrentDailyAverageODB();
        }
    }

    Real64 ConstantFlowRadiantSystemData::calculateCurrentDailyAverageODB()
    {
        Real64 sum = 0.0;
        for (int hourNumber = 1; hourNumber <= DataGlobals::HoursInDay; ++hourNumber) {
            for (int timeStepNumber = 1; timeStepNumber <= DataGlobals::NumOfTimeStepInHour; ++timeStepNumber) {
                sum += WeatherManager::TodayOutDryBulbTemp(timeStepNumber,hourNumber);
            }
        }
        return sum/double(DataGlobals::HoursInDay*DataGlobals::NumOfTimeStepInHour);
    }


    void ElectricRadiantSystemData::calculateLowTemperatureRadiantSystem(EnergyPlusData &state, Real64 &LoadMet)  // load met by the radiant system, in Watts
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Rick Strand
        //       DATE WRITTEN   November 2000
        //       MODIFIED       Sep 2011 LKL/BG - resimulate only zones needing it for Radiant systems

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine does all of the stuff that is necessary to simulate
        // a low temperature electric radiant heating system.  Calls are made to
        // appropriate subroutines either in this module or outside of it.

        // METHODOLOGY EMPLOYED:
        // Follows the methods used by many other pieces of zone equipment except
        // that we are controlling the electrical input to the building element's
        // resistance heating wires.  Note that cooling is not allowed for such
        // a system.

        // REFERENCES:
        // Other EnergyPlus modules
        // IBLAST-QTF research program, completed in January 1995 (unreleased)
        // Strand, R.K. 1995. "Heat Source Transfer Functions and Their Application to
        //   Low Temperature Radiant Heating Systems", Ph.D. dissertation, University
        //   of Illinois at Urbana-Champaign, Department of Mechanical and Industrial
        //   Engineering.
        // Seem, J.E. 1986. "Heat Transfer in Buildings", Ph.D. dissertation, University
        //   of Wisconsin-Madison.

        // Using/Aliasing
        using DataHeatBalance::MRT;
        using DataHeatBalance::Zone;
        using DataHeatBalance::ZoneData;
        using DataHeatBalFanSys::MAT;
        using DataHVACGlobals::SmallLoad;
        using ScheduleManager::GetCurrentScheduleValue;

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        Real64 ControlTemp; // Temperature of the parameter that is controlling the radiant system
        Real64 HeatFrac;    // fraction of maximum electrical heat input to radiant system [dimensionless]
        Real64 OffTemp;     // Temperature above which the radiant system should be completely off [C]
        int RadSurfNum;     // number of surface that is the radiant system
        int SurfNum;        // intermediate variable for surface number in Surface derived type
        int ZoneNum;        // number of zone being served

        // initialize local variables
        ZoneNum = this->ZonePtr;
        HeatFrac = 0.0;

        if (GetCurrentScheduleValue(this->SchedPtr) <= 0.0) {

            // Unit is off; set the heat source terms to zero
            for (RadSurfNum = 1; RadSurfNum <= this->NumOfSurfaces; ++RadSurfNum) {
                SurfNum = this->SurfacePtr(RadSurfNum);
                QRadSysSource(SurfNum) = 0.0;
                if (Surface(SurfNum).ExtBoundCond > 0 && Surface(SurfNum).ExtBoundCond != SurfNum)
                    QRadSysSource(Surface(SurfNum).ExtBoundCond) = 0.0; // Also zero the other side of an interzone
            }

        } else { // Unit might be on-->this section is intended to determine whether the controls say
            // that the unit should be on or not

            // Determine the current setpoint temperature and the temperature at which the unit should be completely off
            OffTemp = this->setOffTemperatureLowTemperatureRadiantSystem(this->SetptSchedPtr,this->ThrottlRange);

            // Determine the control temperature--what the setpoint/offtemp is being compared to for unit operation

            ControlTemp = this->setRadiantSystemControlTemperature();

            if (ControlTemp < OffTemp) { // HEATING MODE

                this->OperatingMode = HeatingMode;

                HeatFrac = this->calculateOperationalFraction(OffTemp, ControlTemp, this->ThrottlRange);
                if (HeatFrac > 1.0) HeatFrac = 1.0;

                // Set the heat source for the low temperature electric radiant system
                for (RadSurfNum = 1; RadSurfNum <= this->NumOfSurfaces; ++RadSurfNum) {
                    SurfNum = this->SurfacePtr(RadSurfNum);
                    QRadSysSource(SurfNum) = HeatFrac * this->MaxElecPower * this->SurfaceFrac(RadSurfNum);
                    if (Surface(SurfNum).ExtBoundCond > 0 && Surface(SurfNum).ExtBoundCond != SurfNum)
                        QRadSysSource(Surface(SurfNum).ExtBoundCond) = QRadSysSource(SurfNum); // Also set the other side of an interzone
                }

                // Now "simulate" the system by recalculating the heat balances
                HeatBalanceSurfaceManager::CalcHeatBalanceOutsideSurf(state, state.dataConvectionCoefficients, state.files, ZoneNum);
                HeatBalanceSurfaceManager::CalcHeatBalanceInsideSurf(state, ZoneNum);

                LoadMet = SumHATsurf(ZoneNum) - ZeroSourceSumHATsurf(ZoneNum);

            } else { //  OFF or COOLING MODE (not allowed for an electric low temperature radiant system), turn it off

                for (RadSurfNum = 1; RadSurfNum <= this->NumOfSurfaces; ++RadSurfNum) {
                    SurfNum = this->SurfacePtr(RadSurfNum);
                    QRadSysSource(SurfNum) = 0.0;
                    if (Surface(SurfNum).ExtBoundCond > 0 && Surface(SurfNum).ExtBoundCond != SurfNum)
                        QRadSysSource(Surface(SurfNum).ExtBoundCond) = 0.0; // Also zero the other side of an interzone
                }
            }
        }
    }

   void RadiantSystemBaseData::updateLowTemperatureRadiantSystemSurfaces()
   {

       // The purpose of this routine is to update the average heat source/sink for a particular system over the various system time
       // steps that make up the zone time step.  For hydronic systems, this routine must also set the outlet water conditions.
       // For the source/sink average update, if the system time step elapsed is still what it used to be, then either we are still
       // iterating orwe had to go back and shorten the time step.  As a result, we have to subtract out the previous value that we
       // added.  If the system time step elapsed is different, then we just need to add the new values to the running average.

       // Using/Aliasing
       using DataGlobals::TimeStepZone;
       using DataHeatBalance::Zone;
       using DataHVACGlobals::SysTimeElapsed;
       using DataHVACGlobals::TimeStepSys;

       // SUBROUTINE PARAMETER DEFINITIONS:
       static std::string const RoutineName("UpdateLowTempRadiantSystem");

       for (int radSurfNum = 1; radSurfNum <= this->NumOfSurfaces; ++radSurfNum) {

           int surfNum = this->SurfacePtr(radSurfNum);

           if (LastSysTimeElapsed(surfNum) == SysTimeElapsed) {
               // Still iterating or reducing system time step, so subtract old values which were
               // not valid
               QRadSysSrcAvg(surfNum) -= LastQRadSysSrc(surfNum) * LastTimeStepSys(surfNum) / TimeStepZone;
           }

           // Update the running average and the "last" values with the current values of the appropriate variables
           QRadSysSrcAvg(surfNum) += QRadSysSource(surfNum) * TimeStepSys / TimeStepZone;

           LastQRadSysSrc(surfNum) = QRadSysSource(surfNum);
           LastSysTimeElapsed(surfNum) = SysTimeElapsed;
           LastTimeStepSys(surfNum) = TimeStepSys;
       }
   }

    void VariableFlowRadiantSystemData::updateLowTemperatureRadiantSystem()
    {

        // Using/Aliasing
        using DataHeatBalance::Zone;
        using DataLoopNode::Node;
        using DataPlant::PlantLoop;
        using FluidProperties::GetSpecificHeatGlycol;
        using PlantUtilities::SafeCopyPlantNode;
        using PlantUtilities::SetComponentFlowRate;

        // SUBROUTINE PARAMETER DEFINITIONS:
        static std::string const RoutineName("UpdateVariableFlowSystem");

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        Real64 cpWater;         // Specific heat of water
        int waterInletNode;     // Node number for the water side inlet of the radiant system
        Real64 waterMassFlow;   // Flow rate of water in the radiant system
        int waterOutletNode;    // Node number for the water side outlet of the radiant system

        // For a hydronic system, calculate the water side outlet conditions and set the
        // appropriate conditions on the correct HVAC node.

        // First sum up all of the heat sources/sinks associated with this system
        Real64 TotalHeatSource(0.0); // Total heat source or sink for a particular radiant system (sum of all surface source/sinks)
        for (int radSurfNum = 1; radSurfNum <= this->NumOfSurfaces; ++radSurfNum) {
            TotalHeatSource += QRadSysSource(this->SurfacePtr(radSurfNum));
        }
        TotalHeatSource *= double(Zone(this->ZonePtr).Multiplier * Zone(this->ZonePtr).ListMultiplier);

        // Update the heating side of things
        if (this->HeatingSystem) {

            waterInletNode = this->HotWaterInNode;
            waterOutletNode = this->HotWaterOutNode;
            waterMassFlow = Node(waterInletNode).MassFlowRate;

            cpWater = GetSpecificHeatGlycol(PlantLoop(this->HWLoopNum).FluidName,Node(waterInletNode).Temp,
                                            PlantLoop(this->HWLoopNum).FluidIndex,RoutineName);

            if (this->OperatingMode == HeatingMode) {
                if ((cpWater > 0.0) && (waterMassFlow > 0.0)) {
                    SafeCopyPlantNode(waterInletNode, waterOutletNode);
                    Node(waterOutletNode).Temp = Node(waterInletNode).Temp - TotalHeatSource / waterMassFlow / cpWater;
                } else {
                    SafeCopyPlantNode(waterInletNode, waterOutletNode);
                }

            } else { // CoolingMode or not on
                SafeCopyPlantNode(waterInletNode, waterOutletNode);
            }

            this->checkForOutOfRangeTemperatureResult(Node(waterOutletNode).Temp, Node(waterInletNode).Temp);
        }

        if (this->CoolingSystem) {

            waterInletNode = this->ColdWaterInNode;
            waterOutletNode = this->ColdWaterOutNode;
            waterMassFlow = Node(waterInletNode).MassFlowRate;

            cpWater = GetSpecificHeatGlycol(PlantLoop(this->CWLoopNum).FluidName,Node(waterInletNode).Temp,
                                            PlantLoop(this->CWLoopNum).FluidIndex,RoutineName);

            if (this->OperatingMode == CoolingMode) {
                if ((cpWater > 0.0) && (waterMassFlow > 0.0)) {
                    SafeCopyPlantNode(waterInletNode, waterOutletNode);
                    Node(waterOutletNode).Temp = Node(waterInletNode).Temp - TotalHeatSource / waterMassFlow / cpWater;
                } else {
                    SafeCopyPlantNode(waterInletNode, waterOutletNode);
                }

            } else { // HeatingMode or not on
                SafeCopyPlantNode(waterInletNode, waterOutletNode);
            }

            this->checkForOutOfRangeTemperatureResult(Node(waterOutletNode).Temp, Node(waterInletNode).Temp);
        }

    }

    void ConstantFlowRadiantSystemData::updateLowTemperatureRadiantSystem()
    {

        // Using/Aliasing
        using DataHeatBalance::Zone;
        using DataLoopNode::Node;
        using DataPlant::PlantLoop;
        using FluidProperties::GetSpecificHeatGlycol;
        using PlantUtilities::SafeCopyPlantNode;
        using PlantUtilities::SetComponentFlowRate;

        Real64 bypassMassFlow;  // Local bypass for a constant flow radiant system (could have recirculation and/or bypass)
        int waterInletNode;     // Node number for the water side inlet of the radiant system
        int waterOutletNode;    // Node number for the water side outlet of the radiant system

        // For a constant flow system, calculate the water side outlet conditions
        // and set the appropriate conditions on the correct HVAC node.  This may
        // require mixing if the main system does not provide all of the flow that
        // the local radiant system circulates.

        // Update the heating side of things
        if (this->HeatingSystem) {

            waterInletNode = this->HotWaterInNode;
            waterOutletNode = this->HotWaterOutNode;
            SafeCopyPlantNode(waterInletNode, waterOutletNode);

            if (this->OperatingMode == HeatingMode) {

                // Leave the inlet and outlet flow alone (if high enough) and perform a bypass if more flow than needed
                if (Node(waterInletNode).MassFlowRate <= this->WaterInjectionRate) {
                    // Note that the water injection rate has already been restricted to the maximum available flow
                    Node(waterOutletNode).Temp = this->WaterOutletTemp;
                } else {
                    // Loop is providing more flow than needed so perform a local bypass and
                    // mix the flows to obtain the proper outlet temperature.  In this case,
                    // the mass flow rates on the loop are left alone and the outlet temperature
                    // is calculated from a simple steady-steady, steady-flow energy balance.
                    bypassMassFlow = Node(waterInletNode).MassFlowRate - this->WaterInjectionRate;
                    Node(waterOutletNode).Temp = ((bypassMassFlow * Node(waterInletNode).Temp) + (this->WaterInjectionRate * this->WaterOutletTemp)) /
                                                 (Node(waterOutletNode).MassFlowRate);
                }
            }
            this->checkForOutOfRangeTemperatureResult(Node(waterOutletNode).Temp, Node(waterInletNode).Temp);
        }

        if (this->CoolingSystem) {

            waterInletNode = this->ColdWaterInNode;
            waterOutletNode = this->ColdWaterOutNode;
            SafeCopyPlantNode(waterInletNode, waterOutletNode);

            if (this->OperatingMode == CoolingMode) {

                if (Node(waterInletNode).MassFlowRate <= this->WaterInjectionRate) {
                    // Note that the water injection rate has already been restricted to the maximum available flow

                    Node(waterOutletNode).Temp = this->WaterOutletTemp;
                } else {
                    // Loop is providing more flow than needed so perform a local bypass and
                    // mix the flows to obtain the proper outlet temperature.  In this case,
                    // the mass flow rates on the loop are left alone and the outlet temperature
                    // is calculated from a simple steady-steady, steady-flow energy balance.
                    bypassMassFlow = Node(waterInletNode).MassFlowRate - this->WaterInjectionRate;
                    Node(waterOutletNode).Temp = ((bypassMassFlow * Node(waterInletNode).Temp) + (this->WaterInjectionRate * this->WaterOutletTemp)) /
                                                 (Node(waterOutletNode).MassFlowRate);
                }

                this->checkForOutOfRangeTemperatureResult(Node(waterOutletNode).Temp, Node(waterInletNode).Temp);
            }
        }

    }

    void ElectricRadiantSystemData::updateLowTemperatureRadiantSystem()
    {   // Dummy routine: no updates are needed for electric radiant systems
    }

    void HydronicSystemBaseData::checkForOutOfRangeTemperatureResult(Real64 const outletTemp, Real64 const inletTemp)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         B. Griffith
        //       DATE WRITTEN   March 2013

        // PURPOSE OF THIS SUBROUTINE:
        // check for crazy, out of range temperature results for fluid leaving radiant system

        // Using/Aliasing
        using General::RoundSigDigits;

        Real64 const upperRangeLimit(500.0);  // high error trigger limit for when model is not working
        Real64 const lowerRangeLimit(-300.0); // Low error trigger limit for when model is not working

        if (outletTemp < lowerRangeLimit) {
            warnTooLow = true;
        }

        if (outletTemp > upperRangeLimit) {
            warnTooHigh = true;
        }

        if (warnTooLow || warnTooHigh) {
            if (warnTooLow) {
                if (this->OutRangeLoErrorCount == 0) {
                    ShowSevereMessage("UpdateLowTempRadiantSystem: model result for fluid outlet temperature is not physical.");
                    ShowContinueError("Occurs for radiant system name = " + this->Name);
                    ShowContinueError("Calculated radiant system outlet temperature = " + RoundSigDigits(outletTemp, 3) + " [C]");
                    ShowContinueError("Radiant system inlet temperature = " + RoundSigDigits(inletTemp, 3) + " [C]");
                    ShowContinueError(
                        "A possible cause is that the materials used in the internal source construction are not compatible with the model.");
                }
                ShowRecurringSevereErrorAtEnd(
                    "UpdateLowTempRadiantSystem: Detected low out of range outlet temperature result for radiant system name =" +
                        this->Name,
                    this->OutRangeLoErrorCount,
                    outletTemp,
                    outletTemp);
            }

            if (warnTooHigh) {
                if (this->OutRangeHiErrorCount == 0) {
                    ShowSevereMessage("UpdateLowTempRadiantSystem: model result for fluid outlet temperature is not physical.");
                    ShowContinueError("Occurs for radiant system name = " + this->Name);
                    ShowContinueError("Calculated radiant system outlet temperature = " + RoundSigDigits(outletTemp, 3) + " [C]");
                    ShowContinueError("Radiant system inlet temperature = " + RoundSigDigits(inletTemp, 3) + " [C]");
                    ShowContinueError(
                        "A possible cause is that the materials used in the internal source construction are not compatible with the model.");
                }
                ShowRecurringSevereErrorAtEnd(
                    "UpdateLowTempRadiantSystem: Detected high out of range outlet temperature result radiant system name =" +
                        this->Name,
                    this->OutRangeHiErrorCount,
                    outletTemp,
                    outletTemp);
            }

        }
    }

    Real64 RadiantSystemBaseData::setRadiantSystemControlTemperature()
    {
        switch (this->ControlType) {
        case LowTempRadiantControlTypes::MATControl:
            return DataHeatBalFanSys::MAT(this->ZonePtr);
        case LowTempRadiantControlTypes::MRTControl:
            return DataHeatBalance::MRT(this->ZonePtr);
        case LowTempRadiantControlTypes::OperativeControl:
            return 0.5 * (DataHeatBalFanSys::MAT(this->ZonePtr) + DataHeatBalance::MRT(this->ZonePtr));
        case LowTempRadiantControlTypes::ODBControl:
            return DataHeatBalance::Zone(this->ZonePtr).OutDryBulbTemp;
        case LowTempRadiantControlTypes::OWBControl:
            return DataHeatBalance::Zone(this->ZonePtr).OutWetBulbTemp;
        case LowTempRadiantControlTypes::SurfFaceTempControl:
            return DataHeatBalSurface::TempSurfIn(this->SurfacePtr(1));   // Grabs the inside face temperature of the first surface in the list
        case LowTempRadiantControlTypes::SurfIntTempControl:
            return DataHeatBalSurface::TempUserLoc(this->SurfacePtr(1));   // Grabs the temperature inside the slab at the location specified by the user
        case LowTempRadiantControlTypes::RunningMeanODBControl:
            return this->todayRunningMeanOutdoorDryBulbTemperature;
        default:
            ShowSevereError("Illegal control type in low temperature radiant system: " + this->Name);
            ShowFatalError("Preceding condition causes termination.");
            return 0.0; // hush the compiler
        }
    }

    Real64 RadiantSystemBaseData::calculateOperationalFraction(Real64 const offTemperature, Real64 const controlTemperature, Real64 const throttlingRange)
    {
        Real64 temperatureDifference = std::abs(offTemperature - controlTemperature);
        if (temperatureDifference <= 0.0) {
            return 0.0; // No temperature difference--turn things off (set to zero); technically shouldn't happen
        } else if (throttlingRange < 0.001) {
            return 1.0; // Throttling range is essentially zero and there is a temperature difference--turn it full on
        } else {
            return temperatureDifference/throttlingRange;   // Temperature difference is non-zero and less than the throttling range--calculate the operation fraction
        }
    }

    Real64 RadiantSystemBaseData::setOffTemperatureLowTemperatureRadiantSystem(const int scheduleIndex, const Real64 throttlingRange)
    {
        Real64 scheduleValue = ScheduleManager::GetCurrentScheduleValue(scheduleIndex);
        switch (this->SetpointType) {
            case LowTempRadiantSetpointTypes::halfFlowPower:
                return scheduleValue + 0.5 *throttlingRange;
            case LowTempRadiantSetpointTypes::zeroFlowPower:
                return scheduleValue;
            default:
                ShowSevereError("Illegal setpoint type in low temperature radiant system: " + this->Name);
                ShowFatalError("Preceding condition causes termination.");
                return scheduleValue + 0.5 * throttlingRange; // hush the compiler
        }

    }

    Real64 HydronicSystemBaseData::calculateHXEffectivenessTerm(int const SurfNum,          // Surface number for this particular part of the radiant system
                                                                Real64 const Temperature,   // Temperature of water entering the radiant system, in C
                                                                Real64 const WaterMassFlow, // Mass flow rate of water in the radiant system, in kg/s
                                                                Real64 const FlowFraction,  // Mass flow rate fraction for this surface in the radiant system
                                                                Real64 const NumCircs       // Number of fluid circuits in this surface
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Rick Strand
        //       DATE WRITTEN   December 2000

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine calculates the radiant system "heat exchanger"
        // effectiveness term.  This is equal to the mass flow rate of water
        // times the specific heat of water times the effectiveness of
        // the heat exchanger (radiant system "coil").

        // METHODOLOGY EMPLOYED:
        // Assumes that the only real heat transfer term that we have to
        // deal with is the convection from the water to the tube.  The
        // other assumptions are that the tube inside surface temperature
        // is equal to the "source location temperature" and that it is
        // a CONSTANT throughout the radiant system.  This is to make
        // the problem more tractable and to fit with other system assumptions
        // that were made elsewhere in the radiant system model.

        // REFERENCES:
        // Property data for water shown below as parameters taken from
        //   Incropera and DeWitt, Introduction to Heat Transfer, Table A.6.
        // Heat exchanger information also from Incropera and DeWitt.
        // Code based loosely on code from IBLAST program (research version)

        // Using/Aliasing
        using DataGlobals::Pi;
        using DataPlant::PlantLoop;
        using FluidProperties::GetSpecificHeatGlycol;

        // Return value
        Real64 calculateHXEffectivenessTerm;

        // SUBROUTINE PARAMETER DEFINITIONS:
        Real64 const MaxLaminarRe(2300.0); // Maximum Reynolds number for laminar flow
        int const NumOfPropDivisions(13);
        Real64 const MaxExpPower(50.0); // Maximum power after which EXP argument would be zero for DP variables
        static Array1D<Real64> const Temps(
            NumOfPropDivisions, {1.85, 6.85, 11.85, 16.85, 21.85, 26.85, 31.85, 36.85, 41.85, 46.85, 51.85, 56.85, 61.85}); // Temperature, in C
        static Array1D<Real64> const Mu(NumOfPropDivisions,
                                        {0.001652,
                                         0.001422,
                                         0.001225,
                                         0.00108,
                                         0.000959,
                                         0.000855,
                                         0.000769,
                                         0.000695,
                                         0.000631,
                                         0.000577,
                                         0.000528,
                                         0.000489,
                                         0.000453}); // Viscosity, in Ns/m2
        static Array1D<Real64> const Conductivity(
            NumOfPropDivisions, {0.574, 0.582, 0.590, 0.598, 0.606, 0.613, 0.620, 0.628, 0.634, 0.640, 0.645, 0.650, 0.656}); // Conductivity, in W/mK
        static Array1D<Real64> const Pr(
            NumOfPropDivisions, {12.22, 10.26, 8.81, 7.56, 6.62, 5.83, 5.20, 4.62, 4.16, 3.77, 3.42, 3.15, 2.88}); // Prandtl number (dimensionless)
        static std::string const RoutineName("calculateHXEffectivenessTerm");

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int Index;
        Real64 InterpFrac;
        Real64 NuD;
        Real64 ReD;
        Real64 NTU;
        Real64 CpWater(0.0);
        Real64 Kactual;
        Real64 MUactual;
        Real64 PRactual;
        Real64 Eff; // HX effectiveness

        // First find out where we are in the range of temperatures
        Index = 1;
        while (Index <= NumOfPropDivisions) {
            if (Temperature < Temps(Index)) break; // DO loop
            ++Index;
        }

        // Initialize thermal properties of water
        if (Index == 1) {
            MUactual = Mu(Index);
            Kactual = Conductivity(Index);
            PRactual = Pr(Index);
        } else if (Index > NumOfPropDivisions) {
            Index = NumOfPropDivisions;
            MUactual = Mu(Index);
            Kactual = Conductivity(Index);
            PRactual = Pr(Index);
        } else {
            InterpFrac = (Temperature - Temps(Index - 1)) / (Temps(Index) - Temps(Index - 1));
            MUactual = Mu(Index - 1) + InterpFrac * (Mu(Index) - Mu(Index - 1));
            Kactual = Conductivity(Index - 1) + InterpFrac * (Conductivity(Index) - Conductivity(Index - 1));
            PRactual = Pr(Index - 1) + InterpFrac * (Pr(Index) - Pr(Index - 1));
        }
        // arguments are glycol name, temperature, and concentration
        {
            auto const SELECT_CASE_var1(this->OperatingMode);
            if (SELECT_CASE_var1 == HeatingMode) {
                CpWater = GetSpecificHeatGlycol(PlantLoop(this->HWLoopNum).FluidName,
                                                Temperature,
                                                PlantLoop(this->HWLoopNum).FluidIndex,
                                                RoutineName);
            } else if (SELECT_CASE_var1 == CoolingMode) {
                CpWater = GetSpecificHeatGlycol(PlantLoop(this->CWLoopNum).FluidName,
                                                Temperature,
                                                PlantLoop(this->CWLoopNum).FluidIndex,
                                                RoutineName);
            } else {
                assert(false);
            }
        }

        // Calculate NTU based on the heat transfer model

        if (this->FluidToSlabHeatTransfer == FluidToSlabHeatTransferTypes::ISOStandard) {

            Real64 U = this->calculateUFromISOStandard(SurfNum, WaterMassFlow*FlowFraction);

            // Calculate the NTU parameter
            // NTU = UA/[(Mdot*Cp)min]
            // where: U = h (convection coefficient) and h = (k)(Nu)/D
            //        A = Pi*D*TubeLength
            NTU = U * Pi * this->TubeDiameterOuter * this->TubeLength / (WaterMassFlow * CpWater); // FlowFraction cancels out here

        } else {    // (this->FluidToSlabHeatTransfer == FluidToSlabHeatTransferTypes::ConvectionOnly)

            // Calculate the Reynold's number from RE=(4*Mdot)/(Pi*Mu*Diameter)
            ReD = 4.0 * WaterMassFlow * FlowFraction / (Pi * MUactual * this->TubeDiameterInner * NumCircs);

            // Calculate the Nusselt number based on what flow regime one is in
            if (ReD >= MaxLaminarRe) { // Turbulent flow --> use Colburn equation

                NuD = 0.023 * std::pow(ReD, 0.8) * std::pow(PRactual, 1.0 / 3.0);

            } else { // Laminar flow --> use constant surface temperature relation

                NuD = 3.66;
            }

            // Calculate the NTU parameter
            // NTU = UA/[(Mdot*Cp)min]
            // where: U = h (convection coefficient) and h = (k)(Nu)/D
            //        A = Pi*D*TubeLength
            NTU = Pi * Kactual * NuD * this->TubeLength / (WaterMassFlow * CpWater); // FlowFraction cancels out here

        }

        // Calculate Epsilon*MassFlowRate*Cp
        if (NTU > MaxExpPower) {
            Eff = 1.0;
            calculateHXEffectivenessTerm = FlowFraction * WaterMassFlow * CpWater;
        } else {
            Eff = 1.0 - std::exp(-NTU);
            calculateHXEffectivenessTerm = Eff * FlowFraction * WaterMassFlow * CpWater;
        }

        return calculateHXEffectivenessTerm;
    }

    Real64 HydronicSystemBaseData::calculateUFromISOStandard(int const SurfNum,
                                                             Real64 const WaterMassFlow)
    {
        // Calculates the U-value for a pipe embedded in a radiant system using the information
        // from ISO Standard 11855, Part 2 (2012): "Building environment design — Design, dimensioning,
        // installation and control of embedded radiant heating and cooling systems — Part 2:
        // Determination of the design heating and cooling capacity."  This looks exclusively at the heat transfer
        // between the fluid and the inner side of the pipe and heat conduction through the pipe.  The remainder
        // of the ISO calculation relates to the slab itself which is modeled using transient heat conduction here
        // in EnergyPlus.

        // Return value
        Real64 calculateUFromISOStandard;

        int constructionNumber = DataSurfaces::Surface(SurfNum).Construction;

        // Fluid resistance to heat transfer, assumes turbulent flow (Equation B5, p. 38 of ISO Standard 11855-2)
        Real64 distanceBetweenPipes = 2.0 * dataConstruction.Construct(constructionNumber).ThicknessPerpend;
        Real64 ratioDiameterToMassFlowLength = this->TubeDiameterInner / WaterMassFlow / this->TubeLength;
        Real64 rFluid = 0.125 / DataGlobals::Pi * std::pow(distanceBetweenPipes, 0.13) * std::pow(ratioDiameterToMassFlowLength,0.87);

        // Resistance to heat transfer (conduction through the piping material, Equation B6, p. 38 of ISO Standard 11855-2)
        Real64 rTube = 0.5 * distanceBetweenPipes * std::log(this->TubeDiameterOuter/this->TubeDiameterInner) / DataGlobals::Pi / this->TubeConductivity;

        calculateUFromISOStandard = 1.0 / (rFluid + rTube);

        return calculateUFromISOStandard;
    }

    void UpdateRadSysSourceValAvg(bool &LowTempRadSysOn) // .TRUE. if the radiant system has run this zone time step
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Rick Strand
        //       DATE WRITTEN   November 2000

        // PURPOSE OF THIS SUBROUTINE:
        // To transfer the average value of the heat source/sink over the entire
        // zone time step back to the heat balance routines so that the heat
        // balance algorithms can simulate one last time with the average source
        // to maintain some reasonable amount of continuity and energy balance
        // in the temperature and flux histories.

        // METHODOLOGY EMPLOYED:
        // All of the record keeping for the average term is done in the Update
        // routine so the only other thing that this subroutine does is check to
        // see if the system was even on.  If any average term is non-zero, then
        // one or more of the radiant systems was running.

        // SUBROUTINE PARAMETER DEFINITIONS:
        Real64 const CloseEnough(0.01); // Some arbitrarily small value to avoid zeros and numbers that are almost the same

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int SurfNum; // DO loop counter for surface index

        LowTempRadSysOn = false;

        // If this was never allocated, then there are no radiant systems in this input file (just RETURN)
        if (!allocated(QRadSysSrcAvg)) return;

        // If it was allocated, then we have to check to see if this was running at all...
        for (SurfNum = 1; SurfNum <= TotSurfaces; ++SurfNum) {
            if (QRadSysSrcAvg(SurfNum) != 0.0) {
                LowTempRadSysOn = true;
                break; // DO loop
            }
        }

        QRadSysSource = QRadSysSrcAvg;

        // For interzone surfaces, QRadSysSrcAvg was only updated for the "active" side.  The active side
        // would have a non-zero value at this point.  If the numbers differ, then we have to manually update.
        for (SurfNum = 1; SurfNum <= TotSurfaces; ++SurfNum) {
            if (Surface(SurfNum).ExtBoundCond > 0 && Surface(SurfNum).ExtBoundCond != SurfNum) {
                if (std::abs(QRadSysSource(SurfNum) - QRadSysSource(Surface(SurfNum).ExtBoundCond)) > CloseEnough) { // numbers differ
                    if (std::abs(QRadSysSource(SurfNum)) > std::abs(QRadSysSource(Surface(SurfNum).ExtBoundCond))) {
                        QRadSysSource(Surface(SurfNum).ExtBoundCond) = QRadSysSource(SurfNum);
                    } else {
                        QRadSysSource(SurfNum) = QRadSysSource(Surface(SurfNum).ExtBoundCond);
                    }
                }
            }
        }
    }

    Real64 SumHATsurf(int const ZoneNum) // Zone number
    {

        // FUNCTION INFORMATION:
        //       AUTHOR         Peter Graham Ellis
        //       DATE WRITTEN   July 2003

        // PURPOSE OF THIS FUNCTION:
        // This function calculates the zone sum of Hc*Area*Tsurf.  It replaces the old SUMHAT.
        // The SumHATsurf code below is also in the CalcZoneSums subroutine in ZoneTempPredictorCorrector
        // and should be updated accordingly.

        // Using/Aliasing
        using namespace DataSurfaces;
        using namespace DataHeatBalance;
        using namespace DataHeatBalSurface;

        // Return value
        Real64 sumHATsurf(0.0);

        for (int surfNum = Zone(ZoneNum).SurfaceFirst; surfNum <= Zone(ZoneNum).SurfaceLast; ++surfNum) {
            if (!Surface(surfNum).HeatTransSurf) continue; // Skip non-heat transfer surfaces

            Real64 Area = Surface(surfNum).Area;

            if (Surface(surfNum).Class == SurfaceClass_Window) {
                if (SurfWinShadingFlag(surfNum) == IntShadeOn || SurfWinShadingFlag(surfNum) == IntBlindOn) {
                    // The area is the shade or blind are = sum of the glazing area and the divider area (which is zero if no divider)
                    Area += SurfWinDividerArea(surfNum);
                }

                if (SurfWinFrameArea(surfNum) > 0.0) {
                    // Window frame contribution
                    sumHATsurf += HConvIn(surfNum) * SurfWinFrameArea(surfNum) * (1.0 + SurfWinProjCorrFrIn(surfNum)) *
                                  SurfWinFrameTempSurfIn(surfNum);
                }

                if (SurfWinDividerArea(surfNum) > 0.0 && SurfWinShadingFlag(surfNum) != IntShadeOn &&
                    SurfWinShadingFlag(surfNum) != IntBlindOn) {
                    // Window divider contribution (only from shade or blind for window with divider and interior shade or blind)
                    sumHATsurf += HConvIn(surfNum) * SurfWinDividerArea(surfNum) * (1.0 + 2.0 * SurfWinProjCorrDivIn(surfNum)) *
                                  SurfWinDividerTempSurfIn(surfNum);
                }
            }

            sumHATsurf += HConvIn(surfNum) * Area * TempSurfInTmp(surfNum);
        }

        return sumHATsurf;
    }

    void VariableFlowRadiantSystemData::reportLowTemperatureRadiantSystem()
    {

        // Using/Aliasing
        using DataGlobals::SecInHour;
        using DataHeatBalance::Zone;
        using DataHVACGlobals::TimeStepSys;
        using DataLoopNode::Node;

        Real64 totalRadSysPower(0.0); // Total source/sink power for the radiant system (sum of all surfaces of the system)

        for (int radSurfNum = 1; radSurfNum <= this->NumOfSurfaces; ++radSurfNum) {
            totalRadSysPower += QRadSysSource(this->SurfacePtr(radSurfNum));
        }

        totalRadSysPower *= double(Zone(this->ZonePtr).Multiplier * Zone(this->ZonePtr).ListMultiplier);

        this->HeatPower = 0.0;
        this->CoolPower = 0.0;

        if (this->OperatingMode == HeatingMode) {
            this->WaterInletTemp = Node(this->HotWaterInNode).Temp;
            this->WaterOutletTemp = Node(this->HotWaterOutNode).Temp;
            this->WaterMassFlowRate = Node(this->HotWaterInNode).MassFlowRate;
            this->HeatPower = totalRadSysPower;

        } else if (this->OperatingMode == CoolingMode) {
            this->WaterInletTemp = Node(this->ColdWaterInNode).Temp;
            this->WaterOutletTemp = Node(this->ColdWaterOutNode).Temp;
            this->WaterMassFlowRate = Node(this->ColdWaterInNode).MassFlowRate;
            this->CoolPower = -totalRadSysPower;

        } else { // Not Operating: Leave temperatures at previous values
            this->WaterMassFlowRate = 0.0;
            this->WaterOutletTemp = this->WaterInletTemp;
        }

        this->HeatEnergy = this->HeatPower * TimeStepSys * SecInHour;
        this->CoolEnergy = this->CoolPower * TimeStepSys * SecInHour;

        if (this->CondCausedShutDown) {
            this->CondCausedTimeOff = TimeStepSys * SecInHour;
        } else {
            this->CondCausedTimeOff = 0.0;
        }

    }

    void ConstantFlowRadiantSystemData::reportLowTemperatureRadiantSystem()
    {

        // Using/Aliasing
        using DataGlobals::SecInHour;
        using DataHeatBalance::Zone;
        using DataHVACGlobals::TimeStepSys;
        using DataLoopNode::Node;
        using DataPlant::PlantLoop;
        using DataSurfaces::Surface;
        using FluidProperties::GetSpecificHeatGlycol;

        static std::string const routineName("ReportConstantFlowSystem");
        Real64 cpFluid;          // Specific heat of the fluid in the radiant system
        Real64 totalRadSysPower(0.0); // Total source/sink power for the radiant system (sum of all surfaces of the system)

        for (int radSurfNum = 1; radSurfNum <= this->NumOfSurfaces; ++radSurfNum) {
            totalRadSysPower += QRadSysSource(this->SurfacePtr(radSurfNum));
        }

        totalRadSysPower *= double(Zone(this->ZonePtr).Multiplier * Zone(this->ZonePtr).ListMultiplier);

        this->HeatPower = 0.0;
        this->CoolPower = 0.0;

        // Note that temperatures have already been set as part of the simulation
        // step.  So, they do not need to be calculated here except for the pump
        // inlet temperature which was not calculated elsewhere.  If the system is
        // not operating, leave the temperatures with their previous values but
        // zero out the flow and power quantities (should have already been done
        // in another routine, but just in case...).

        if (this->OperatingMode == HeatingMode) {
            cpFluid = GetSpecificHeatGlycol(PlantLoop(this->HWLoopNum).FluidName,Node(this->HotWaterInNode).Temp,
                                            PlantLoop(this->HWLoopNum).FluidIndex,routineName);

            this->HeatPower = totalRadSysPower;
            if (this->PumpMassFlowRate > 0.0) {
                this->PumpInletTemp = this->WaterInletTemp - (this->PumpHeattoFluid / (this->PumpMassFlowRate * cpFluid));
            } else {
                this->PumpInletTemp = this->WaterInletTemp;
            }

        } else if (this->OperatingMode == CoolingMode) {
            cpFluid = GetSpecificHeatGlycol(PlantLoop(this->CWLoopNum).FluidName,Node(this->ColdWaterInNode).Temp,
                                            PlantLoop(this->CWLoopNum).FluidIndex,routineName);

            this->CoolPower = -totalRadSysPower;
            this->PumpInletTemp = this->WaterInletTemp - (this->PumpHeattoFluid / (this->PumpMassFlowRate * cpFluid));

        } else { // Not Operating
            this->WaterOutletTemp = this->WaterInletTemp;
            this->PumpInletTemp = this->WaterInletTemp;
            this->WaterMassFlowRate = 0.0;
            this->WaterInjectionRate = 0.0;
            this->WaterRecircRate = 0.0;
            this->HeatPower = 0.0;
            this->CoolPower = 0.0;
            this->PumpPower = 0.0;
            this->PumpMassFlowRate = 0.0;
            this->PumpHeattoFluid = 0.0;
        }

        this->HeatEnergy = this->HeatPower * TimeStepSys * SecInHour;
        this->CoolEnergy = this->CoolPower * TimeStepSys * SecInHour;
        this->PumpEnergy = this->PumpPower * TimeStepSys * SecInHour;
        this->PumpHeattoFluidEnergy = this->PumpHeattoFluid * TimeStepSys * SecInHour;

        if (this->CondCausedShutDown) {
            this->CondCausedTimeOff = TimeStepSys * SecInHour;
        } else {
            this->CondCausedTimeOff = 0.0;
        }

    }

    void ElectricRadiantSystemData::reportLowTemperatureRadiantSystem()
    {

        // Using/Aliasing
        using DataGlobals::SecInHour;
        using DataHeatBalance::Zone;
        using DataHVACGlobals::TimeStepSys;

        Real64 totalRadSysPower(0.0); // Total source/sink power for the radiant system (sum of all surfaces of the system)

        for (int radSurfNum = 1; radSurfNum <= this->NumOfSurfaces; ++radSurfNum) {
            totalRadSysPower += QRadSysSource(this->SurfacePtr(radSurfNum));
        }

        totalRadSysPower *= double(Zone(this->ZonePtr).Multiplier * Zone(this->ZonePtr).ListMultiplier);

        this->ElecPower = totalRadSysPower;
        this->ElecEnergy = this->ElecPower * TimeStepSys * SecInHour;
        this->HeatPower = this->ElecPower;
        this->HeatEnergy = this->ElecEnergy;

    }

} // namespace LowTempRadiantSystem

} // namespace EnergyPlus
