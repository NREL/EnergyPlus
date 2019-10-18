// EnergyPlus, Copyright (c) 1996-2019, The Board of Trustees of the University of Illinois,
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
#include <EnergyPlus/BranchNodeConnections.hh>
#include <EnergyPlus/CurveManager.hh>
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataGenerators.hh>
#include <EnergyPlus/DataGlobalConstants.hh>
#include <EnergyPlus/DataGlobals.hh>
#include <EnergyPlus/DataHVACGlobals.hh>
#include <EnergyPlus/DataHeatBalFanSys.hh>
#include <EnergyPlus/DataHeatBalance.hh>
#include <EnergyPlus/DataIPShortCuts.hh>
#include <EnergyPlus/DataLoopNode.hh>
#include <EnergyPlus/DataPlant.hh>
#include <EnergyPlus/DataSizing.hh>
#include <EnergyPlus/FluidProperties.hh>
#include <EnergyPlus/General.hh>
#include <EnergyPlus/GeneratorDynamicsManager.hh>
#include <EnergyPlus/GeneratorFuelSupply.hh>
#include <EnergyPlus/HeatBalanceInternalHeatGains.hh>
#include <EnergyPlus/InputProcessing/InputProcessor.hh>
#include <EnergyPlus/MicroCHPElectricGenerator.hh>
#include <EnergyPlus/NodeInputManager.hh>
#include <EnergyPlus/OutputProcessor.hh>
#include <EnergyPlus/PlantUtilities.hh>
#include <EnergyPlus/ScheduleManager.hh>
#include <EnergyPlus/UtilityRoutines.hh>

namespace EnergyPlus {

namespace MicroCHPElectricGenerator {

    // MODULE INFORMATION:
    //       AUTHOR         Brent Griffth
    //       DATE WRITTEN   June 2006
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS MODULE:
    // This module simulates the operation of Internal Combustion and Stirling Engine
    //  residential-scale generators for combined heat and power.

    // METHODOLOGY EMPLOYED:
    // Once the ElectricPowerManager determines that the Combustion Generator
    // is available to meet an electric load demand, it calls SimCombustionGenerator
    // which in turn calls the Combustion model.
    // See DataFuelCells.cc for structures and variables

    // REFERENCES:
    // IEA/ECBCS Annex 42 model specification titled: " A Generic Model Specification for
    // Combustion-based Residential CHP Devices"  Alex Ferguson, Nick Kelly, IEA Annex 42.
    // Module developed from

    int NumMicroCHPs;
    int NumMicroCHPParams;
    
    Array1D<MicroCHPDataStruct> MicroCHP;
    Array1D<MicroCHPParamsNonNormalized> MicroCHPParamInput;

    bool GetMicroCHPInput(true);

    void SimMicroCHPGenerator(int const EP_UNUSED(GeneratorType), // type of Generator
                              std::string const &GeneratorName,   // user specified name of Generator
                              int &GeneratorIndex,
                              bool const RunFlagElectCenter, // simulate Generator when TRUE
                              bool const RunFlagPlant,       // simulate generator when true.
                              Real64 const MyElectricLoad,   // demand on electric generator
                              Real64 const MyThermalLoad,    // thermal demand on cogenerator
                              bool const FirstHVACIteration)
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR         Brent Griffith
        //       DATE WRITTEN   July 2006
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE: This is the main driver for the model for
        // internal combustion engine and
        // Stirling cycle engine model from IEA/ECBCS Annex 42 r.  It
        // gets the input for the models, initializes simulation variables, call
        // the appropriate model and sets up reporting variables.

        int GenNum; // Generator number counter

        // Get Generator data from input file
        if (GetMicroCHPInput) {
            GetMicroCHPGeneratorInput();
            GetMicroCHPInput = false;
        }

        if (GeneratorIndex == 0) {
            GenNum = UtilityRoutines::FindItemInList(GeneratorName, MicroCHP);
            if (GenNum == 0) ShowFatalError("SimMicroCHPGenerator: Specified Generator not one of Valid Micro CHP Generators " + GeneratorName);
            GeneratorIndex = GenNum;
        } else {
            GenNum = GeneratorIndex;
            if (GenNum > NumMicroCHPs || GenNum < 1) {
                ShowFatalError("SimMicroCHPGenerator: Invalid GeneratorIndex passed=" + General::TrimSigDigits(GenNum) +
                               ", Number of Micro CHP Generators=" + General::TrimSigDigits(NumMicroCHPs) + ", Generator name=" + GeneratorName);
            }
            if (MicroCHP(GenNum).CheckEquipName) {
                if (GeneratorName != MicroCHP(GenNum).Name) {
                    ShowFatalError("SimMicroCHPNoNormalizeGenerator: Invalid GeneratorIndex passed=" + General::TrimSigDigits(GenNum) +
                                   ", Generator name=" + GeneratorName + ", stored Generator Name for that index=" + MicroCHP(GenNum).Name);
                }
                MicroCHP(GenNum).CheckEquipName = false;
            }
        }

        if (MicroCHP(GenNum).ModelTypeAnnex42) { // call the non normalize calc routines (set for future extension to normalize ones)

            InitMicroCHPNoNormalizeGenerators(GenNum, FirstHVACIteration);
            if (!DataPlant::PlantFirstSizeCompleted) return;
            CalcMicroCHPNoNormalizeGeneratorModel(GenNum, RunFlagElectCenter, RunFlagPlant, MyElectricLoad, MyThermalLoad, FirstHVACIteration);

            CalcUpdateHeatRecovery(GenNum, FirstHVACIteration);

            UpdateMicroCHPGeneratorRecords(GenNum);
        }
    }

    void GetMicroCHPGeneratorInput()
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR:          Brent Griffith
        //       DATE WRITTEN:    July 2005

        // PURPOSE OF THIS SUBROUTINE:
        // This routine will get the input
        // required by the Micro CHP Generator models.

        // METHODOLOGY EMPLOYED:
        // EnergyPlus input processor

        int GeneratorNum;               // Generator counter
        int NumAlphas;                  // Number of elements in the alpha array
        int NumNums;                    // Number of elements in the numeric array
        int IOStat;                     // IO Status when calling get input subroutine
        Array1D_string AlphArray(25);   // character string data
        Array1D<Real64> NumArray(200);  // numeric data TODO deal with allocatable for extensible
        static bool ErrorsFound(false); // error flag
        static bool MyOneTimeFlag(true);

        if (MyOneTimeFlag) {

            // call to Fuel supply module to set up data there.
            GeneratorFuelSupply::GetGeneratorFuelSupplyInput();

            // First get the Micro CHP Parameters so they can be nested in structure later
            DataIPShortCuts::cCurrentModuleObject = "Generator:MicroCHP:NonNormalizedParameters";
            NumMicroCHPParams = inputProcessor->getNumObjectsFound(DataIPShortCuts::cCurrentModuleObject);

            if (NumMicroCHPParams <= 0) {
                ShowSevereError("No " + DataIPShortCuts::cCurrentModuleObject + " equipment specified in input file");
                ErrorsFound = true;
            }

            MicroCHPParamInput.allocate(NumMicroCHPParams);

            for (int CHPParamNum = 1; CHPParamNum <= NumMicroCHPParams; ++CHPParamNum) {
                inputProcessor->getObjectItem(DataIPShortCuts::cCurrentModuleObject,
                                              CHPParamNum,
                                              AlphArray,
                                              NumAlphas,
                                              NumArray,
                                              NumNums,
                                              IOStat,
                                              _,
                                              DataIPShortCuts::lAlphaFieldBlanks,
                                              DataIPShortCuts::cAlphaFieldNames,
                                              DataIPShortCuts::cNumericFieldNames);
                // Can't validate this name
                // UtilityRoutines::IsNameEmpty(AlphArray(1),DataIPShortCuts::cCurrentModuleObject, ErrorsFound);

                std::string ObjMSGName = DataIPShortCuts::cCurrentModuleObject + " Named " + AlphArray(1);

                MicroCHPParamInput(CHPParamNum).Name = AlphArray(1);        // A1 name
                MicroCHPParamInput(CHPParamNum).MaxElecPower = NumArray(1); // N1 Maximum Electric Power [W]
                MicroCHPParamInput(CHPParamNum).MinElecPower = NumArray(2); // N2 Minimum Electric Power [W]
                MicroCHPParamInput(CHPParamNum).MinWaterMdot = NumArray(3); // N3 Minimum Cooling Water Flow Rate [kg/s]
                MicroCHPParamInput(CHPParamNum).MaxWaterTemp = NumArray(4); // N3 Maximum Cooling Water Inlet Temp [C]
                MicroCHPParamInput(CHPParamNum).ElecEffCurveID = CurveManager::GetCurveCheck(AlphArray(2), ErrorsFound, ObjMSGName);    // Electrical Eff. ID
                MicroCHPParamInput(CHPParamNum).ThermalEffCurveID = CurveManager::GetCurveCheck(AlphArray(3), ErrorsFound, ObjMSGName); // Thermal Efficiency

                if (UtilityRoutines::SameString(AlphArray(4), "InternalControl")) {
                    MicroCHPParamInput(CHPParamNum).InternalFlowControl = true; //  A4, \field Cooling Water Flow Rate Mode
                    MicroCHPParamInput(CHPParamNum).PlantFlowControl = false;
                }
                if ((!(UtilityRoutines::SameString(AlphArray(4), "InternalControl"))) &&
                    (!(UtilityRoutines::SameString(AlphArray(4), "PlantControl")))) {
                    ShowSevereError("Invalid, " + DataIPShortCuts::cAlphaFieldNames(4) + " = " + AlphArray(4));
                    ShowContinueError("Entered in " + DataIPShortCuts::cCurrentModuleObject + '=' + AlphArray(1));
                    ErrorsFound = true;
                }
                if (MicroCHPParamInput(CHPParamNum).InternalFlowControl) { // get the curve
                    MicroCHPParamInput(CHPParamNum).WaterFlowCurveID = CurveManager::GetCurveCheck(AlphArray(5), ErrorsFound, ObjMSGName);
                    //  Curve for Cooling Water Flow Rate
                }
                MicroCHPParamInput(CHPParamNum).AirFlowCurveID = CurveManager::GetCurveCheck(AlphArray(6), ErrorsFound, ObjMSGName);
                //  Name of Curve for Air Flow Rate
                MicroCHPParamInput(CHPParamNum).DeltaPelMax = NumArray(5);       // N5 Maximum rate of change in net electrical power [W/s]
                MicroCHPParamInput(CHPParamNum).DeltaFuelMdotMax = NumArray(6);  // N6 Maximum Rate of change in fuel flow rate [kg/s2]
                MicroCHPParamInput(CHPParamNum).UAhx = NumArray(7);              // N7 Heat Exchanger UA_hx
                MicroCHPParamInput(CHPParamNum).UAskin = NumArray(8);            // N8 Skin Loss UA_loss
                MicroCHPParamInput(CHPParamNum).RadiativeFraction = NumArray(9); // N9 radiative fraction for skin losses
                MicroCHPParamInput(CHPParamNum).MCeng = NumArray(10);            // N10 Aggregated Thermal Mass of Generator MC_eng
                if (MicroCHPParamInput(CHPParamNum).MCeng <= 0.0) {
                    ShowSevereError("Invalid, " + DataIPShortCuts::cNumericFieldNames(10) + " = " + General::RoundSigDigits(NumArray(10), 5));
                    ShowContinueError("Entered in " + DataIPShortCuts::cCurrentModuleObject + '=' + AlphArray(1));
                    ShowContinueError("Thermal mass must be greater than zero");
                    ErrorsFound = true;
                }
                MicroCHPParamInput(CHPParamNum).MCcw = NumArray(11); // Aggregated Thermal Mass of Heat Recovery MC_cw
                if (MicroCHPParamInput(CHPParamNum).MCcw <= 0.0) {
                    ShowSevereError("Invalid, " + DataIPShortCuts::cNumericFieldNames(11) + " = " + General::RoundSigDigits(NumArray(11), 5));
                    ShowContinueError("Entered in " + DataIPShortCuts::cCurrentModuleObject + '=' + AlphArray(1));
                    ShowContinueError("Thermal mass must be greater than zero");
                    ErrorsFound = true;
                }
                MicroCHPParamInput(CHPParamNum).Pstandby = NumArray(12); // N12 Standby Power [W]

                if (UtilityRoutines::SameString(AlphArray(7), "TimeDelay")) {
                    MicroCHPParamInput(CHPParamNum).WarmUpByTimeDelay = true;
                    MicroCHPParamInput(CHPParamNum).WarmUpByEngineTemp = false;
                }
                if ((!(UtilityRoutines::SameString(AlphArray(7), "NominalEngineTemperature"))) &&
                    (!(UtilityRoutines::SameString(AlphArray(7), "TimeDelay")))) {
                    ShowSevereError("Invalid, " + DataIPShortCuts::cAlphaFieldNames(7) + " = " + AlphArray(7));
                    ShowContinueError("Entered in " + DataIPShortCuts::cCurrentModuleObject + '=' + AlphArray(1));
                    ErrorsFound = true;
                }
                MicroCHPParamInput(CHPParamNum).kf = NumArray(13);          // N13 Warmup Fuel Flow Rate Coefficient k_f
                MicroCHPParamInput(CHPParamNum).TnomEngOp = NumArray(14);   // N14 Nominal Engine Operating Temperature [C]
                MicroCHPParamInput(CHPParamNum).kp = NumArray(15);          // N15 Warmup Power Coefficient k_p
                MicroCHPParamInput(CHPParamNum).Rfuelwarmup = NumArray(16); // N16 Warm Up Fuel Flow Rate Limit Ratio
                MicroCHPParamInput(CHPParamNum).WarmUpDelay = NumArray(17); // N17 Warm Up Delay Time

                MicroCHPParamInput(CHPParamNum).PcoolDown = NumArray(18); // N18 Cool Down Power

                MicroCHPParamInput(CHPParamNum).CoolDownDelay = NumArray(19); // N19 Cool Down Delay Time in seconds

                if (UtilityRoutines::SameString(AlphArray(8), "MandatoryCoolDown")) {
                    MicroCHPParamInput(CHPParamNum).MandatoryFullCoolDown = true;
                    MicroCHPParamInput(CHPParamNum).WarmRestartOkay = false;
                }
                if ((!(UtilityRoutines::SameString(AlphArray(8), "MandatoryCoolDown"))) &&
                    (!(UtilityRoutines::SameString(AlphArray(8), "OptionalCoolDown")))) {
                    ShowSevereError("Invalid, " + DataIPShortCuts::cAlphaFieldNames(8) + " = " + AlphArray(8));
                    ShowContinueError("Entered in " + DataIPShortCuts::cCurrentModuleObject + '=' + AlphArray(1));
                    ErrorsFound = true;
                }
            }

            DataIPShortCuts::cCurrentModuleObject = "Generator:MicroCHP";
            NumMicroCHPs = inputProcessor->getNumObjectsFound(DataIPShortCuts::cCurrentModuleObject);

            if (NumMicroCHPs <= 0) {
                // shouldn't ever come here?
                ShowSevereError("No " + DataIPShortCuts::cCurrentModuleObject + " equipment specified in input file");
                ErrorsFound = true;
            }

            // ALLOCATE ARRAYS

            if (!(allocated(MicroCHP))) {
                MicroCHP.allocate(NumMicroCHPs); // inits handeled in derived type definitions
            }

            // load in Micro CHPs
            for (GeneratorNum = 1; GeneratorNum <= NumMicroCHPs; ++GeneratorNum) {
                inputProcessor->getObjectItem(DataIPShortCuts::cCurrentModuleObject,
                                              GeneratorNum,
                                              AlphArray,
                                              NumAlphas,
                                              NumArray,
                                              NumNums,
                                              IOStat,
                                              _,
                                              DataIPShortCuts::lAlphaFieldBlanks,
                                              DataIPShortCuts::cAlphaFieldNames,
                                              DataIPShortCuts::cNumericFieldNames);
                UtilityRoutines::IsNameEmpty(AlphArray(1), DataIPShortCuts::cCurrentModuleObject, ErrorsFound);

                // GENERATOR:MICRO CHP,
                MicroCHP(GeneratorNum).Name = AlphArray(1); //  A1 Generator name
                MicroCHP(GeneratorNum).ParamObjName = AlphArray(2); //  A2 Micro CHP Parameter Object Name
                // find input structure
                int thisParamID = UtilityRoutines::FindItemInList(AlphArray(2), MicroCHPParamInput);
                if (thisParamID != 0) {
                    MicroCHP(GeneratorNum).A42Model = MicroCHPParamInput(thisParamID); // entire structure of input data assigned here!
                } else {
                    ShowSevereError("Invalid, " + DataIPShortCuts::cAlphaFieldNames(2) + " = " + AlphArray(2));
                    ShowContinueError("Entered in " + DataIPShortCuts::cCurrentModuleObject + '=' + AlphArray(1));
                    ErrorsFound = true;
                }

                if (!DataIPShortCuts::lAlphaFieldBlanks(3)) {
                    MicroCHP(GeneratorNum).ZoneName = AlphArray(3); //  A3 Zone Name
                    MicroCHP(GeneratorNum).ZoneID = UtilityRoutines::FindItemInList(MicroCHP(GeneratorNum).ZoneName, DataHeatBalance::Zone);
                    if (MicroCHP(GeneratorNum).ZoneID == 0) {
                        ShowSevereError("Invalid, " + DataIPShortCuts::cAlphaFieldNames(3) + " = " + AlphArray(3));
                        ShowContinueError("Entered in " + DataIPShortCuts::cCurrentModuleObject + '=' + AlphArray(1));
                        ErrorsFound = true;
                    }
                } else {
                    MicroCHP(GeneratorNum).ZoneID = 0;
                }
                MicroCHP(GeneratorNum).PlantInletNodeName = AlphArray(4);  //  A4 Cooling Water Inlet Node Name
                MicroCHP(GeneratorNum).PlantOutletNodeName = AlphArray(5); //  A5 Cooling Water Outlet Node Name
                // find node ids for water path
                MicroCHP(GeneratorNum).PlantInletNodeID = NodeInputManager::GetOnlySingleNode(
                    AlphArray(4), ErrorsFound, DataIPShortCuts::cCurrentModuleObject, AlphArray(1), DataLoopNode::NodeType_Water, DataLoopNode::NodeConnectionType_Inlet, 1, DataLoopNode::ObjectIsNotParent);
                MicroCHP(GeneratorNum).PlantOutletNodeID = NodeInputManager::GetOnlySingleNode(
                    AlphArray(5), ErrorsFound, DataIPShortCuts::cCurrentModuleObject, AlphArray(1), DataLoopNode::NodeType_Water, DataLoopNode::NodeConnectionType_Outlet, 1, DataLoopNode::ObjectIsNotParent);
                BranchNodeConnections::TestCompSet(DataIPShortCuts::cCurrentModuleObject, AlphArray(1), AlphArray(4), AlphArray(5), "Heat Recovery Nodes");

                MicroCHP(GeneratorNum).AirInletNodeName = AlphArray(6); //  A6 Air Inlet Node Name
                // check the node connections
                MicroCHP(GeneratorNum).AirInletNodeID = NodeInputManager::GetOnlySingleNode(
                    AlphArray(6), ErrorsFound, DataIPShortCuts::cCurrentModuleObject, AlphArray(1), DataLoopNode::NodeType_Air, DataLoopNode::NodeConnectionType_Inlet, 2, DataLoopNode::ObjectIsNotParent);

                MicroCHP(GeneratorNum).AirOutletNodeName = AlphArray(7); //  A7 Air Outlet Node Name
                MicroCHP(GeneratorNum).AirOutletNodeID = NodeInputManager::GetOnlySingleNode(
                    AlphArray(7), ErrorsFound, DataIPShortCuts::cCurrentModuleObject, AlphArray(1), DataLoopNode::NodeType_Air, DataLoopNode::NodeConnectionType_Outlet, 2, DataLoopNode::ObjectIsNotParent);

                MicroCHP(GeneratorNum).FuelSupplyID = UtilityRoutines::FindItemInList(AlphArray(8), DataGenerators::FuelSupply); // Fuel Supply ID
                if (MicroCHP(GeneratorNum).FuelSupplyID == 0) {
                    ShowSevereError("Invalid, " + DataIPShortCuts::cAlphaFieldNames(8) + " = " + AlphArray(8));
                    ShowContinueError("Entered in " + DataIPShortCuts::cCurrentModuleObject + '=' + AlphArray(1));
                    ErrorsFound = true;
                }

                if (DataIPShortCuts::lAlphaFieldBlanks(9)) {
                    MicroCHP(GeneratorNum).AvailabilitySchedID = DataGlobals::ScheduleAlwaysOn;
                } else {
                    MicroCHP(GeneratorNum).AvailabilitySchedID = ScheduleManager::GetScheduleIndex(AlphArray(9));
                    if (MicroCHP(GeneratorNum).AvailabilitySchedID == 0) {
                        ShowSevereError("Invalid, " + DataIPShortCuts::cAlphaFieldNames(9) + " = " + AlphArray(9));
                        ShowContinueError("Entered in " + DataIPShortCuts::cCurrentModuleObject + '=' + AlphArray(1));
                        ErrorsFound = true;
                    }
                }
                MicroCHP(GeneratorNum).A42Model.TengLast = 20.0;      // inits
                MicroCHP(GeneratorNum).A42Model.TempCWOutLast = 20.0; // inits
            }

            if (ErrorsFound) {
                ShowFatalError("Errors found in processing input for " + DataIPShortCuts::cCurrentModuleObject);
            }

            // setup report variables
            for (GeneratorNum = 1; GeneratorNum <= NumMicroCHPs; ++GeneratorNum) {

                SetupOutputVariable("Generator Off Mode Time",
                                    OutputProcessor::Unit::s,
                                    MicroCHP(GeneratorNum).Report.OffModeTime,
                                    "System",
                                    "Sum",
                                    MicroCHP(GeneratorNum).Name);
                SetupOutputVariable("Generator Standby Mode Time",
                                    OutputProcessor::Unit::s,
                                    MicroCHP(GeneratorNum).Report.StandyByModeTime,
                                    "System",
                                    "Sum",
                                    MicroCHP(GeneratorNum).Name);
                SetupOutputVariable("Generator Warm Up Mode Time",
                                    OutputProcessor::Unit::s,
                                    MicroCHP(GeneratorNum).Report.WarmUpModeTime,
                                    "System",
                                    "Sum",
                                    MicroCHP(GeneratorNum).Name);
                SetupOutputVariable("Generator Normal Operating Mode Time",
                                    OutputProcessor::Unit::s,
                                    MicroCHP(GeneratorNum).Report.NormalModeTime,
                                    "System",
                                    "Sum",
                                    MicroCHP(GeneratorNum).Name);
                SetupOutputVariable("Generator Cool Down Mode Time",
                                    OutputProcessor::Unit::s,
                                    MicroCHP(GeneratorNum).Report.CoolDownModeTime,
                                    "System",
                                    "Sum",
                                    MicroCHP(GeneratorNum).Name);

                SetupOutputVariable("Generator Produced Electric Power",
                                    OutputProcessor::Unit::W,
                                    MicroCHP(GeneratorNum).Report.ACPowerGen,
                                    "System",
                                    "Average",
                                    MicroCHP(GeneratorNum).Name);
                SetupOutputVariable("Generator Produced Electric Energy",
                                    OutputProcessor::Unit::J,
                                    MicroCHP(GeneratorNum).Report.ACEnergyGen,
                                    "System",
                                    "Sum",
                                    MicroCHP(GeneratorNum).Name,
                                    _,
                                    "ElectricityProduced",
                                    "COGENERATION",
                                    _,
                                    "Plant");
                SetupOutputVariable("Generator Produced Thermal Rate",
                                    OutputProcessor::Unit::W,
                                    MicroCHP(GeneratorNum).Report.QdotHR,
                                    "system",
                                    "Average",
                                    MicroCHP(GeneratorNum).Name);
                SetupOutputVariable("Generator Produced Thermal Energy",
                                    OutputProcessor::Unit::J,
                                    MicroCHP(GeneratorNum).Report.TotalHeatEnergyRec,
                                    "system",
                                    "Sum",
                                    MicroCHP(GeneratorNum).Name,
                                    _,
                                    "ENERGYTRANSFER",
                                    "COGENERATION",
                                    _,
                                    "Plant");

                SetupOutputVariable("Generator Electric Efficiency",
                                    OutputProcessor::Unit::None,
                                    MicroCHP(GeneratorNum).Report.ElectEfficiency,
                                    "System",
                                    "Average",
                                    MicroCHP(GeneratorNum).Name);
                SetupOutputVariable("Generator Thermal Efficiency",
                                    OutputProcessor::Unit::None,
                                    MicroCHP(GeneratorNum).Report.ThermalEfficiency,
                                    "System",
                                    "Average",
                                    MicroCHP(GeneratorNum).Name);
                SetupOutputVariable("Generator Gross Input Heat Rate",
                                    OutputProcessor::Unit::W,
                                    MicroCHP(GeneratorNum).Report.QdotGross,
                                    "system",
                                    "Average",
                                    MicroCHP(GeneratorNum).Name);
                SetupOutputVariable("Generator Steady State Engine Heat Generation Rate",
                                    OutputProcessor::Unit::W,
                                    MicroCHP(GeneratorNum).Report.Qgenss,
                                    "system",
                                    "Average",
                                    MicroCHP(GeneratorNum).Name);

                SetupOutputVariable("Generator Engine Heat Exchange Rate",
                                    OutputProcessor::Unit::W,
                                    MicroCHP(GeneratorNum).Report.QdotHX,
                                    "system",
                                    "Average",
                                    MicroCHP(GeneratorNum).Name);
                SetupOutputVariable("Generator Air Mass Flow Rate",
                                    OutputProcessor::Unit::kg_s,
                                    MicroCHP(GeneratorNum).Report.MdotAir,
                                    "System",
                                    "Average",
                                    MicroCHP(GeneratorNum).Name);
                SetupOutputVariable("Generator Fuel Molar Flow Rate",
                                    OutputProcessor::Unit::kmol_s,
                                    MicroCHP(GeneratorNum).Report.NdotFuel,
                                    "System",
                                    "Average",
                                    MicroCHP(GeneratorNum).Name);
                SetupOutputVariable("Generator Fuel Mass Flow Rate",
                                    OutputProcessor::Unit::kg_s,
                                    MicroCHP(GeneratorNum).Report.MdotFuel,
                                    "System",
                                    "Average",
                                    MicroCHP(GeneratorNum).Name);

                SetupOutputVariable("Generator Engine Temperature",
                                    OutputProcessor::Unit::C,
                                    MicroCHP(GeneratorNum).Report.Tengine,
                                    "System",
                                    "Average",
                                    MicroCHP(GeneratorNum).Name);
                SetupOutputVariable("Generator Coolant Inlet Temperature",
                                    OutputProcessor::Unit::C,
                                    MicroCHP(GeneratorNum).Report.HeatRecInletTemp,
                                    "System",
                                    "Average",
                                    MicroCHP(GeneratorNum).Name);
                SetupOutputVariable("Generator Coolant Outlet Temperature",
                                    OutputProcessor::Unit::C,
                                    MicroCHP(GeneratorNum).Report.HeatRecOutletTemp,
                                    "System",
                                    "Average",
                                    MicroCHP(GeneratorNum).Name);

                // this next one needs to be reconciled with non-gas fuel constituents.
                //   need custom resourceTypeKey or something for user defined fuel compositions.
                SetupOutputVariable("Generator Fuel HHV Basis Energy",
                                    OutputProcessor::Unit::J,
                                    MicroCHP(GeneratorNum).Report.FuelEnergyHHV,
                                    "System",
                                    "Sum",
                                    MicroCHP(GeneratorNum).Name,
                                    _,
                                    "Gas",
                                    "COGENERATION",
                                    _,
                                    "Plant");

                SetupOutputVariable("Generator Fuel HHV Basis Rate",
                                    OutputProcessor::Unit::W,
                                    MicroCHP(GeneratorNum).Report.FuelEnergyUseRateHHV,
                                    "System",
                                    "Average",
                                    MicroCHP(GeneratorNum).Name);

                SetupOutputVariable("Generator Fuel LHV Basis Energy",
                                    OutputProcessor::Unit::J,
                                    MicroCHP(GeneratorNum).Report.FuelEnergyLHV,
                                    "System",
                                    "Sum",
                                    MicroCHP(GeneratorNum).Name);
                SetupOutputVariable("Generator Fuel LHV Basis Rate",
                                    OutputProcessor::Unit::W,
                                    MicroCHP(GeneratorNum).Report.FuelEnergyUseRateLHV,
                                    "System",
                                    "Average",
                                    MicroCHP(GeneratorNum).Name);

                SetupOutputVariable("Generator Fuel Compressor Electric Power",
                                    OutputProcessor::Unit::W,
                                    MicroCHP(GeneratorNum).Report.FuelCompressPower,
                                    "System",
                                    "Average",
                                    MicroCHP(GeneratorNum).Name);
                SetupOutputVariable("Generator Fuel Compressor Electric Energy",
                                    OutputProcessor::Unit::J,
                                    MicroCHP(GeneratorNum).Report.FuelCompressEnergy,
                                    "System",
                                    "Sum",
                                    MicroCHP(GeneratorNum).Name);
                SetupOutputVariable("Generator Fuel Compressor Skin Heat Loss Rate",
                                    OutputProcessor::Unit::W,
                                    MicroCHP(GeneratorNum).Report.FuelCompressSkinLoss,
                                    "System",
                                    "Average",
                                    MicroCHP(GeneratorNum).Name);

                SetupOutputVariable("Generator Zone Sensible Heat Transfer Rate",
                                    OutputProcessor::Unit::W,
                                    MicroCHP(GeneratorNum).Report.SkinLossPower,
                                    "System",
                                    "Average",
                                    MicroCHP(GeneratorNum).Name);
                SetupOutputVariable("Generator Zone Sensible Heat Transfer Energy",
                                    OutputProcessor::Unit::J,
                                    MicroCHP(GeneratorNum).Report.SkinLossEnergy,
                                    "System",
                                    "Sum",
                                    MicroCHP(GeneratorNum).Name);
                SetupOutputVariable("Generator Zone Convection Heat Transfer Rate",
                                    OutputProcessor::Unit::W,
                                    MicroCHP(GeneratorNum).Report.SkinLossConvect,
                                    "System",
                                    "Average",
                                    MicroCHP(GeneratorNum).Name);
                SetupOutputVariable("Generator Zone Radiation Heat Transfer Rate",
                                    OutputProcessor::Unit::W,
                                    MicroCHP(GeneratorNum).Report.SkinLossRadiat,
                                    "System",
                                    "Average",
                                    MicroCHP(GeneratorNum).Name);

                if (MicroCHP(GeneratorNum).ZoneID > 0) {
                    SetupZoneInternalGain(MicroCHP(GeneratorNum).ZoneID,
                                          "Generator:MicroCHP",
                                          MicroCHP(GeneratorNum).Name,
                                          DataHeatBalance::IntGainTypeOf_GeneratorMicroCHP,
                                          MicroCHP(GeneratorNum).Report.SkinLossConvect,
                                          _,
                                          MicroCHP(GeneratorNum).Report.SkinLossRadiat);
                }
            }

            MyOneTimeFlag = false;
        }
    }

    // PARAMETERS

    void InitMicroCHPNoNormalizeGenerators(int const GeneratorNum, // Generator number
                                           bool const EP_UNUSED(FirstHVACIteration))
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR         BGriffith
        //       DATE WRITTEN   March 2007
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        static std::string const RoutineName("InitMicroCHPNoNormalizeGenerators");

        bool errFlag;

        if (MicroCHP(GeneratorNum).MyPlantScanFlag && allocated(DataPlant::PlantLoop)) {
            errFlag = false;
            PlantUtilities::ScanPlantLoopsForObject(MicroCHP(GeneratorNum).Name,
                                    DataPlant::TypeOf_Generator_MicroCHP,
                                    MicroCHP(GeneratorNum).CWLoopNum,
                                    MicroCHP(GeneratorNum).CWLoopSideNum,
                                    MicroCHP(GeneratorNum).CWBranchNum,
                                    MicroCHP(GeneratorNum).CWCompNum,
                                    errFlag,
                                    _,
                                    _,
                                    _,
                                    _,
                                    _);

            if (errFlag) {
                ShowFatalError("InitMicroCHPNoNormalizeGenerators: Program terminated for previous conditions.");
            }

            if (!MicroCHP(GeneratorNum).A42Model.InternalFlowControl) {
                // IF this is on the supply side and not internal flow control then reset flow priority to lower
                if (MicroCHP(GeneratorNum).CWLoopSideNum == DataPlant::SupplySide) {
                    DataPlant::PlantLoop(MicroCHP(GeneratorNum).CWLoopNum)
                        .LoopSide(MicroCHP(GeneratorNum).CWLoopSideNum)
                        .Branch(MicroCHP(GeneratorNum).CWBranchNum)
                        .Comp(MicroCHP(GeneratorNum).CWCompNum)
                        .FlowPriority = DataPlant::LoopFlowStatus_TakesWhatGets;
                }
            }

            MicroCHP(GeneratorNum).MyPlantScanFlag = false;
        }

        if (!DataGlobals::SysSizingCalc && MicroCHP(GeneratorNum).MySizeFlag && !MicroCHP(GeneratorNum).MyPlantScanFlag && (DataPlant::PlantFirstSizesOkayToFinalize)) {
            Real64 rho = FluidProperties::GetDensityGlycol(DataPlant::PlantLoop(MicroCHP(GeneratorNum).CWLoopNum).FluidName,
                                   DataLoopNode::Node(MicroCHP(GeneratorNum).PlantInletNodeID).Temp,
                                   DataPlant::PlantLoop(MicroCHP(GeneratorNum).CWLoopNum).FluidIndex,
                                   RoutineName);
            if (MicroCHP(GeneratorNum).A42Model.InternalFlowControl) { // got a curve
                MicroCHP(GeneratorNum).PlantMassFlowRateMax = 2.0 * CurveManager::CurveValue(MicroCHP(GeneratorNum).A42Model.WaterFlowCurveID,
                                                                               MicroCHP(GeneratorNum).A42Model.MaxElecPower,
                                                                               DataLoopNode::Node(MicroCHP(GeneratorNum).PlantInletNodeID).Temp);
            } else if (MicroCHP(GeneratorNum).CWLoopSideNum == DataPlant::SupplySide) {
                if (DataPlant::PlantLoop(MicroCHP(GeneratorNum).CWLoopNum).MaxMassFlowRate > 0.0) {
                    MicroCHP(GeneratorNum).PlantMassFlowRateMax = DataPlant::PlantLoop(MicroCHP(GeneratorNum).CWLoopNum).MaxMassFlowRate;
                } else if (DataPlant::PlantLoop(MicroCHP(GeneratorNum).CWLoopNum).PlantSizNum > 0) {
                    MicroCHP(GeneratorNum).PlantMassFlowRateMax = DataSizing::PlantSizData(MicroCHP(GeneratorNum).CWLoopNum).DesVolFlowRate * rho;
                } else {
                    MicroCHP(GeneratorNum).PlantMassFlowRateMax = 2.0;
                }

            } else if (MicroCHP(GeneratorNum).CWLoopSideNum == DataPlant::DemandSide) {
                MicroCHP(GeneratorNum).PlantMassFlowRateMax = 2.0; // would like to use plant loop max but not ready yet
            }

            PlantUtilities::RegisterPlantCompDesignFlow(MicroCHP(GeneratorNum).PlantInletNodeID, MicroCHP(GeneratorNum).PlantMassFlowRateMax / rho);

            MicroCHP(GeneratorNum).A42Model.ElecEff = CurveManager::CurveValue(MicroCHP(GeneratorNum).A42Model.ElecEffCurveID,
                                                                 MicroCHP(GeneratorNum).A42Model.MaxElecPower,
                                                                 MicroCHP(GeneratorNum).PlantMassFlowRateMax,
                                                                 DataLoopNode::Node(MicroCHP(GeneratorNum).PlantInletNodeID).Temp);

            MicroCHP(GeneratorNum).A42Model.ThermEff = CurveManager::CurveValue(MicroCHP(GeneratorNum).A42Model.ThermalEffCurveID,
                                                                  MicroCHP(GeneratorNum).A42Model.MaxElecPower,
                                                                  MicroCHP(GeneratorNum).PlantMassFlowRateMax,
                                                                  DataLoopNode::Node(MicroCHP(GeneratorNum).PlantInletNodeID).Temp);

            GeneratorDynamicsManager::SetupGeneratorControlStateManager(GeneratorNum);
            MicroCHP(GeneratorNum).MySizeFlag = false;
        }

        if (MicroCHP(GeneratorNum).MySizeFlag) return;

        int DynaCntrlNum = MicroCHP(GeneratorNum).DynamicsControlID;

        if (DataGlobals::BeginEnvrnFlag && MicroCHP(GeneratorNum).MyEnvrnFlag) {
            // reset to starting condition for different environment runperiods, design days
            MicroCHP(GeneratorNum).A42Model.TengLast = 20.0;
            MicroCHP(GeneratorNum).A42Model.TempCWOutLast = 20.0;
            MicroCHP(GeneratorNum).A42Model.TimeElapsed = 0.0;
            MicroCHP(GeneratorNum).A42Model.OpMode = 0;
            MicroCHP(GeneratorNum).A42Model.OffModeTime = 0.0;
            MicroCHP(GeneratorNum).A42Model.StandyByModeTime = 0.0;
            MicroCHP(GeneratorNum).A42Model.WarmUpModeTime = 0.0;
            MicroCHP(GeneratorNum).A42Model.NormalModeTime = 0.0;
            MicroCHP(GeneratorNum).A42Model.CoolDownModeTime = 0.0;
            MicroCHP(GeneratorNum).A42Model.Pnet = 0.0;
            MicroCHP(GeneratorNum).A42Model.ElecEff = 0.0;
            MicroCHP(GeneratorNum).A42Model.Qgross = 0.0;
            MicroCHP(GeneratorNum).A42Model.ThermEff = 0.0;
            MicroCHP(GeneratorNum).A42Model.Qgenss = 0.0;
            MicroCHP(GeneratorNum).A42Model.NdotFuel = 0.0;
            MicroCHP(GeneratorNum).A42Model.MdotFuel = 0.0;
            MicroCHP(GeneratorNum).A42Model.Teng = 20.0;
            MicroCHP(GeneratorNum).A42Model.TcwIn = 20.0;
            MicroCHP(GeneratorNum).A42Model.TcwOut = 20.0;
            MicroCHP(GeneratorNum).A42Model.MdotAir = 0.0;
            MicroCHP(GeneratorNum).A42Model.QdotSkin = 0.0;
            MicroCHP(GeneratorNum).A42Model.QdotConvZone = 0.0;
            MicroCHP(GeneratorNum).A42Model.QdotRadZone = 0.0;
            DataGenerators::GeneratorDynamics(DynaCntrlNum).LastOpMode = DataGenerators::OpModeOff;
            DataGenerators::GeneratorDynamics(DynaCntrlNum).CurrentOpMode = DataGenerators::OpModeOff;
            DataGenerators::GeneratorDynamics(DynaCntrlNum).FractionalDayofLastShutDown = 0.0;
            DataGenerators::GeneratorDynamics(DynaCntrlNum).FractionalDayofLastStartUp = 0.0;
            DataGenerators::GeneratorDynamics(DynaCntrlNum).HasBeenOn = false;
            DataGenerators::GeneratorDynamics(DynaCntrlNum).DuringStartUp = false;
            DataGenerators::GeneratorDynamics(DynaCntrlNum).DuringShutDown = false;
            DataGenerators::GeneratorDynamics(DynaCntrlNum).FuelMdotLastTimestep = 0.0;
            DataGenerators::GeneratorDynamics(DynaCntrlNum).PelLastTimeStep = 0.0;
            DataGenerators::GeneratorDynamics(DynaCntrlNum).NumCycles = 0;

            DataGenerators::FuelSupply(MicroCHP(GeneratorNum).FuelSupplyID).QskinLoss = 0.0;

            PlantUtilities::InitComponentNodes(0.0,
                               MicroCHP(GeneratorNum).PlantMassFlowRateMax,
                               MicroCHP(GeneratorNum).PlantInletNodeID,
                               MicroCHP(GeneratorNum).PlantOutletNodeID,
                               MicroCHP(GeneratorNum).CWLoopNum,
                               MicroCHP(GeneratorNum).CWLoopSideNum,
                               MicroCHP(GeneratorNum).CWBranchNum,
                               MicroCHP(GeneratorNum).CWCompNum);
        }

        if (!DataGlobals::BeginEnvrnFlag) {
            MicroCHP(GeneratorNum).MyEnvrnFlag = true;
        }

        Real64 TimeElapsed = DataGlobals::HourOfDay + DataGlobals::TimeStep * DataGlobals::TimeStepZone + DataHVACGlobals::SysTimeElapsed;
        if (MicroCHP(GeneratorNum).A42Model.TimeElapsed != TimeElapsed) {
            // The simulation has advanced to the next system timestep.  Save conditions from the end of the previous system
            // timestep for use as the initial conditions of each iteration that does not advance the system timestep.
            MicroCHP(GeneratorNum).A42Model.TengLast = MicroCHP(GeneratorNum).A42Model.Teng;
            MicroCHP(GeneratorNum).A42Model.TempCWOutLast = MicroCHP(GeneratorNum).A42Model.TcwOut;
            MicroCHP(GeneratorNum).A42Model.TimeElapsed = TimeElapsed;
            DataGenerators::GeneratorDynamics(DynaCntrlNum).LastOpMode = DataGenerators::GeneratorDynamics(DynaCntrlNum).CurrentOpMode;
            DataGenerators::GeneratorDynamics(DynaCntrlNum).FuelMdotLastTimestep = MicroCHP(GeneratorNum).A42Model.MdotFuel;
            DataGenerators::GeneratorDynamics(DynaCntrlNum).PelLastTimeStep = MicroCHP(GeneratorNum).A42Model.Pnet;
        }

        if (!MicroCHP(GeneratorNum).A42Model.InternalFlowControl) {

            Real64 mdot = MicroCHP(GeneratorNum).PlantMassFlowRateMax;
            PlantUtilities::SetComponentFlowRate(mdot,
                                 MicroCHP(GeneratorNum).PlantInletNodeID,
                                 MicroCHP(GeneratorNum).PlantOutletNodeID,
                                 MicroCHP(GeneratorNum).CWLoopNum,
                                 MicroCHP(GeneratorNum).CWLoopSideNum,
                                 MicroCHP(GeneratorNum).CWBranchNum,
                                 MicroCHP(GeneratorNum).CWCompNum);
            MicroCHP(GeneratorNum).PlantMassFlowRate = mdot;
        }
    }

    void CalcMicroCHPNoNormalizeGeneratorModel(int const GeneratorNum,        // Generator number
                                               bool const RunFlagElectCenter, // TRUE when Generator operating
                                               bool const RunFlagPlant,
                                               Real64 const MyElectricLoad, // Generator demand
                                               Real64 const MyThermalLoad,
                                               bool const FirstHVACIteration)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR        B Griffith
        //       DATE WRITTEN   July 2006
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Main calculation subroutine for the IEA Annex 42 model

        // METHODOLOGY EMPLOYED:
        // curve fit, dynamic control limits,

        // REFERENCES:
        // IEA Annex 42 FC-COGEN-SIM "A Generic Model Specification for Combustion-based Residential CHP Devices"
        // Alex Ferguson, Nick Kelly, Version 3, June 26, 2006

        static std::string const RoutineName("CalcMicroCHPNoNormalizeGeneratorModel");

        int CurrentOpMode = 0;
        Real64 AllowedLoad = 0.0;
        Real64 PLRforSubtimestepStartUp(1.0);
        Real64 PLRforSubtimestepShutDown(0.0);
        bool RunFlag(false);

        GeneratorDynamicsManager::ManageGeneratorControlState(DataGlobalConstants::iGeneratorMicroCHP,
                                    MicroCHP(GeneratorNum).Name,
                                    GeneratorNum,
                                    RunFlagElectCenter,
                                    RunFlagPlant,
                                    MyElectricLoad,
                                    MyThermalLoad,
                                    AllowedLoad,
                                    CurrentOpMode,
                                    PLRforSubtimestepStartUp,
                                    PLRforSubtimestepShutDown,
                                    FirstHVACIteration);

        if (RunFlagElectCenter || RunFlagPlant) RunFlag = true;

        Real64 Teng = MicroCHP(GeneratorNum).A42Model.Teng;
        Real64 TcwOut = MicroCHP(GeneratorNum).A42Model.TcwOut;

        Real64 thisAmbientTemp;
        if (MicroCHP(GeneratorNum).ZoneID > 0) {
            thisAmbientTemp = DataHeatBalFanSys::MAT(MicroCHP(GeneratorNum).ZoneID);
        } else { // outdoor location, no zone
            thisAmbientTemp = DataEnvironment::OutDryBulbTemp;
        }

        Real64 Pnetss = 0.0;
        Real64 Pstandby = 0.0; // power draw during standby, positive here means negative production
        Real64 Pcooler = 0.0;  // power draw during cool down, positive here means negative production
        Real64 NdotFuel = 0.0;
        Real64 ElecEff = 0.0;
        Real64 MdotAir = 0.0;
        Real64 Qgenss = 0.0;
        Real64 MdotCW = 0.0;
        Real64 TcwIn = 0.0;
        Real64 MdotFuel = 0.0;
        Real64 Qgross = 0.0;
        Real64 ThermEff = 0.0;

        {
            auto const SELECT_CASE_var(CurrentOpMode);

            if (SELECT_CASE_var == DataGenerators::OpModeOff) { // same as standby in model spec but no Pnet standby electicity losses.

                Qgenss = 0.0;
                MdotCW = DataLoopNode::Node(MicroCHP(GeneratorNum).PlantInletNodeID).MassFlowRate; // kg/s
                TcwIn = DataLoopNode::Node(MicroCHP(GeneratorNum).PlantInletNodeID).Temp;          // C
                Pnetss = 0.0;
                Pstandby = 0.0;
                Pcooler = MicroCHP(GeneratorNum).A42Model.PcoolDown * PLRforSubtimestepShutDown;
                ElecEff = 0.0;
                ThermEff = 0.0;
                Qgross = 0.0;
                NdotFuel = 0.0;
                MdotFuel = 0.0;
                MdotAir = 0.0;

                MdotCW = 0.0;
                PlantUtilities::SetComponentFlowRate(MdotCW,
                                     MicroCHP(GeneratorNum).PlantInletNodeID,
                                     MicroCHP(GeneratorNum).PlantOutletNodeID,
                                     MicroCHP(GeneratorNum).CWLoopNum,
                                     MicroCHP(GeneratorNum).CWLoopSideNum,
                                     MicroCHP(GeneratorNum).CWBranchNum,
                                     MicroCHP(GeneratorNum).CWCompNum);
                MicroCHP(GeneratorNum).PlantMassFlowRate = MdotCW;

            } else if (SELECT_CASE_var == DataGenerators::OpModeStandby) {
                Qgenss = 0.0;
                MdotCW = DataLoopNode::Node(MicroCHP(GeneratorNum).PlantInletNodeID).MassFlowRate; // kg/s
                TcwIn = DataLoopNode::Node(MicroCHP(GeneratorNum).PlantInletNodeID).Temp;          // C
                Pnetss = 0.0;
                Pstandby = MicroCHP(GeneratorNum).A42Model.Pstandby * (1.0 - PLRforSubtimestepShutDown);
                Pcooler = MicroCHP(GeneratorNum).A42Model.PcoolDown * PLRforSubtimestepShutDown;
                ElecEff = 0.0;
                ThermEff = 0.0;
                Qgross = 0.0;
                NdotFuel = 0.0;
                MdotFuel = 0.0;
                MdotAir = 0.0;

                MdotCW = 0.0;
                PlantUtilities::SetComponentFlowRate(MdotCW,
                                     MicroCHP(GeneratorNum).PlantInletNodeID,
                                     MicroCHP(GeneratorNum).PlantOutletNodeID,
                                     MicroCHP(GeneratorNum).CWLoopNum,
                                     MicroCHP(GeneratorNum).CWLoopSideNum,
                                     MicroCHP(GeneratorNum).CWBranchNum,
                                     MicroCHP(GeneratorNum).CWCompNum);
                MicroCHP(GeneratorNum).PlantMassFlowRate = MdotCW;

            } else if (SELECT_CASE_var == DataGenerators::OpModeWarmUp) {

                if (MicroCHP(GeneratorNum).A42Model.WarmUpByTimeDelay) {
                    // Internal combustion engine.  This is just like normal  operation but no net power yet.
                    Pnetss = MyElectricLoad; // W
                    Pstandby = 0.0;
                    Pcooler = MicroCHP(GeneratorNum).A42Model.PcoolDown * PLRforSubtimestepShutDown;
                    TcwIn = DataLoopNode::Node(MicroCHP(GeneratorNum).PlantInletNodeID).Temp;          // C
                    MdotCW = DataLoopNode::Node(MicroCHP(GeneratorNum).PlantInletNodeID).MassFlowRate; // kg/s
                    if (MicroCHP(GeneratorNum).A42Model.InternalFlowControl) {
                        MdotCW = GeneratorDynamicsManager::FuncDetermineCWMdotForInternalFlowControl(GeneratorNum, Pnetss, TcwIn);
                    }
                    ElecEff = CurveManager::CurveValue(MicroCHP(GeneratorNum).A42Model.ElecEffCurveID, Pnetss, MdotCW, TcwIn);
                    ElecEff = max(0.0, ElecEff); // protect against bad curve result

                    if (ElecEff > 0.0) {           // trap divide by bad thing
                        Qgross = Pnetss / ElecEff; // W
                    } else {
                        Qgross = 0.0;
                    }
                    ThermEff = CurveManager::CurveValue(MicroCHP(GeneratorNum).A42Model.ThermalEffCurveID, Pnetss, MdotCW, TcwIn);
                    ThermEff = max(0.0, ThermEff); // protect against bad curve result

                    Qgenss = ThermEff * Qgross; // W

                    MdotFuel = Qgross / (DataGenerators::FuelSupply(MicroCHP(GeneratorNum).FuelSupplyID).LHV * 1000.0 * 1000.0) *
                               DataGenerators::FuelSupply(MicroCHP(GeneratorNum).FuelSupplyID).KmolPerSecToKgPerSec;
                    //  kMol/s = (J/s) /(KJ/mol * 1000 J/KJ * 1000 mol/kmol)

                    bool ConstrainedIncreasingNdot(false);
                    bool ConstrainedDecreasingNdot(false);
                    Real64 MdotFuelAllowed = 0.0;

                    GeneratorDynamicsManager::ManageGeneratorFuelFlow(DataGlobalConstants::iGeneratorMicroCHP,
                                            MicroCHP(GeneratorNum).Name,
                                            GeneratorNum,
                                            RunFlag,
                                            MdotFuel,
                                            MdotFuelAllowed,
                                            ConstrainedIncreasingNdot,
                                            ConstrainedDecreasingNdot);

                    if (ConstrainedIncreasingNdot || ConstrainedDecreasingNdot) { // recalculate Pnetss with new NdotFuel with iteration
                        MdotFuel = MdotFuelAllowed;
                        NdotFuel = MdotFuel / DataGenerators::FuelSupply(MicroCHP(GeneratorNum).FuelSupplyID).KmolPerSecToKgPerSec;
                        Qgross = NdotFuel * (DataGenerators::FuelSupply(MicroCHP(GeneratorNum).FuelSupplyID).LHV * 1000.0 * 1000.0);

                        for (int i = 1; i <= 20; ++i) { // iterating here  could add use of seach method
                            Pnetss = Qgross * ElecEff;
                            if (MicroCHP(GeneratorNum).A42Model.InternalFlowControl) {
                                MdotCW = GeneratorDynamicsManager::FuncDetermineCWMdotForInternalFlowControl(GeneratorNum, Pnetss, TcwIn);
                            }
                            ElecEff = CurveManager::CurveValue(MicroCHP(GeneratorNum).A42Model.ElecEffCurveID, Pnetss, MdotCW, TcwIn);
                            ElecEff = max(0.0, ElecEff); // protect against bad curve result
                        }

                        ThermEff = CurveManager::CurveValue(MicroCHP(GeneratorNum).A42Model.ThermalEffCurveID, Pnetss, MdotCW, TcwIn);
                        ThermEff = max(0.0, ThermEff); // protect against bad curve result
                        Qgenss = ThermEff * Qgross;    // W
                    }
                    Pnetss = 0.0; // no actually power produced here.
                    NdotFuel = MdotFuel / DataGenerators::FuelSupply(MicroCHP(GeneratorNum).FuelSupplyID).KmolPerSecToKgPerSec;
                    MdotAir = CurveManager::CurveValue(MicroCHP(GeneratorNum).A42Model.AirFlowCurveID, MdotFuel);
                    MdotAir = max(0.0, MdotAir); // protect against bad curve result

                } else if (MicroCHP(GeneratorNum).A42Model.WarmUpByEngineTemp) {
                    // Stirling engine mode warm up
                    //   find MdotFuelMax
                    Real64 Pmax = MicroCHP(GeneratorNum).A42Model.MaxElecPower;
                    Pstandby = 0.0;
                    Pcooler = MicroCHP(GeneratorNum).A42Model.PcoolDown * PLRforSubtimestepShutDown; // could be here with part load in cool down
                    TcwIn = DataLoopNode::Node(MicroCHP(GeneratorNum).PlantInletNodeID).Temp;                      // C
                    MdotCW = DataLoopNode::Node(MicroCHP(GeneratorNum).PlantInletNodeID).MassFlowRate;             // kg/s
                    ElecEff = CurveManager::CurveValue(MicroCHP(GeneratorNum).A42Model.ElecEffCurveID, Pmax, MdotCW, TcwIn);
                    ElecEff = max(0.0, ElecEff); // protect against bad curve result
                    if (ElecEff > 0.0) {         // trap divide by bad thing
                        Qgross = Pmax / ElecEff; // W
                    } else {
                        Qgross = 0.0;
                    }
                    NdotFuel = Qgross / (DataGenerators::FuelSupply(MicroCHP(GeneratorNum).FuelSupplyID).LHV * 1000.0 * 1000.0);
                    //  kMol/s = (J/s) /(KJ/mol * 1000 J/KJ * 1000 mol/kmol)
                    Real64 MdotFuelMax = NdotFuel * DataGenerators::FuelSupply(MicroCHP(GeneratorNum).FuelSupplyID).KmolPerSecToKgPerSec;

                    Real64 MdotFuelWarmup;
                    if (Teng > thisAmbientTemp) {
                        MdotFuelWarmup = MdotFuelMax + MicroCHP(GeneratorNum).A42Model.kf * MdotFuelMax *
                                                           ((MicroCHP(GeneratorNum).A42Model.TnomEngOp - thisAmbientTemp) / (Teng - thisAmbientTemp));
                        // check that numerical answer didn't blow up beyond limit, and reset if it did
                        if (MdotFuelWarmup > MicroCHP(GeneratorNum).A42Model.Rfuelwarmup * MdotFuelMax) {
                            MdotFuelWarmup = MicroCHP(GeneratorNum).A42Model.Rfuelwarmup * MdotFuelMax;
                        }
                    } else { // equal would divide by zero
                        MdotFuelWarmup = MicroCHP(GeneratorNum).A42Model.Rfuelwarmup * MdotFuelMax;
                    }

                    if (MicroCHP(GeneratorNum).A42Model.TnomEngOp > thisAmbientTemp) {
                        Pnetss = Pmax * MicroCHP(GeneratorNum).A42Model.kp *
                                 ((Teng - thisAmbientTemp) / (MicroCHP(GeneratorNum).A42Model.TnomEngOp - thisAmbientTemp));
                    } else if (MicroCHP(GeneratorNum).A42Model.TnomEngOp < thisAmbientTemp) {
                        Pnetss = Pmax;
                    } else { // equal would divide by zero
                        Pnetss = Pmax;
                    }

                    MdotFuel = MdotFuelWarmup;
                    NdotFuel = MdotFuel / DataGenerators::FuelSupply(MicroCHP(GeneratorNum).FuelSupplyID).KmolPerSecToKgPerSec;
                    MdotAir = CurveManager::CurveValue(MicroCHP(GeneratorNum).A42Model.AirFlowCurveID, MdotFuelWarmup);
                    MdotAir = max(0.0, MdotAir); // protect against bad curve result
                    Qgross = NdotFuel * (DataGenerators::FuelSupply(MicroCHP(GeneratorNum).FuelSupplyID).LHV * 1000.0 * 1000.0);
                    ThermEff = CurveManager::CurveValue(MicroCHP(GeneratorNum).A42Model.ThermalEffCurveID, Pmax, MdotCW, TcwIn);
                    Qgenss = ThermEff * Qgross; // W
                }
                NdotFuel = MdotFuel / DataGenerators::FuelSupply(MicroCHP(GeneratorNum).FuelSupplyID).KmolPerSecToKgPerSec;

            } else if (SELECT_CASE_var == DataGenerators::OpModeNormal) {
                if (PLRforSubtimestepStartUp < 1.0) {
                    if (RunFlagElectCenter) Pnetss = MyElectricLoad; // W
                    if (RunFlagPlant) Pnetss = AllowedLoad;
                } else {
                    Pnetss = AllowedLoad;
                }
                Pstandby = 0.0;
                Pcooler = 0.0;
                TcwIn = DataLoopNode::Node(MicroCHP(GeneratorNum).PlantInletNodeID).Temp;          // C
                MdotCW = DataLoopNode::Node(MicroCHP(GeneratorNum).PlantInletNodeID).MassFlowRate; // kg/s
                if (MicroCHP(GeneratorNum).A42Model.InternalFlowControl) {
                    MdotCW = GeneratorDynamicsManager::FuncDetermineCWMdotForInternalFlowControl(GeneratorNum, Pnetss, TcwIn);
                }

                ElecEff = CurveManager::CurveValue(MicroCHP(GeneratorNum).A42Model.ElecEffCurveID, Pnetss, MdotCW, TcwIn);
                ElecEff = max(0.0, ElecEff); // protect against bad curve result

                if (ElecEff > 0.0) {           // trap divide by bad thing
                    Qgross = Pnetss / ElecEff; // W
                } else {
                    Qgross = 0.0;
                }

                ThermEff = CurveManager::CurveValue(MicroCHP(GeneratorNum).A42Model.ThermalEffCurveID, Pnetss, MdotCW, TcwIn);
                ThermEff = max(0.0, ThermEff); // protect against bad curve result
                Qgenss = ThermEff * Qgross;    // W
                MdotFuel = Qgross / (DataGenerators::FuelSupply(MicroCHP(GeneratorNum).FuelSupplyID).LHV * 1000.0 * 1000.0) *
                           DataGenerators::FuelSupply(MicroCHP(GeneratorNum).FuelSupplyID).KmolPerSecToKgPerSec;
                //  kMol/s = (J/s) /(KJ/mol * 1000 J/KJ * 1000 mol/kmol)

                bool ConstrainedIncreasingNdot(false);
                bool ConstrainedDecreasingNdot(false);
                Real64 MdotFuelAllowed = 0.0;

                GeneratorDynamicsManager::ManageGeneratorFuelFlow(DataGlobalConstants::iGeneratorMicroCHP,
                                        MicroCHP(GeneratorNum).Name,
                                        GeneratorNum,
                                        RunFlag,
                                        MdotFuel,
                                        MdotFuelAllowed,
                                        ConstrainedIncreasingNdot,
                                        ConstrainedDecreasingNdot);

                if (ConstrainedIncreasingNdot || ConstrainedDecreasingNdot) { // recalculate Pnetss with new NdotFuel with iteration
                    MdotFuel = MdotFuelAllowed;
                    NdotFuel = MdotFuel / DataGenerators::FuelSupply(MicroCHP(GeneratorNum).FuelSupplyID).KmolPerSecToKgPerSec;
                    Qgross = NdotFuel * (DataGenerators::FuelSupply(MicroCHP(GeneratorNum).FuelSupplyID).LHV * 1000.0 * 1000.0);

                    for (int i = 1; i <= 20; ++i) { // iterating here,  could add use of seach method error signal
                        Pnetss = Qgross * ElecEff;
                        if (MicroCHP(GeneratorNum).A42Model.InternalFlowControl) {
                            MdotCW = GeneratorDynamicsManager::FuncDetermineCWMdotForInternalFlowControl(GeneratorNum, Pnetss, TcwIn);
                        }
                        ElecEff = CurveManager::CurveValue(MicroCHP(GeneratorNum).A42Model.ElecEffCurveID, Pnetss, MdotCW, TcwIn);
                        ElecEff = max(0.0, ElecEff); // protect against bad curve result
                    }

                    ThermEff = CurveManager::CurveValue(MicroCHP(GeneratorNum).A42Model.ThermalEffCurveID, Pnetss, MdotCW, TcwIn);
                    ThermEff = max(0.0, ThermEff); // protect against bad curve result
                    Qgenss = ThermEff * Qgross;    // W
                }

                NdotFuel = MdotFuel / DataGenerators::FuelSupply(MicroCHP(GeneratorNum).FuelSupplyID).KmolPerSecToKgPerSec;
                MdotAir = CurveManager::CurveValue(MicroCHP(GeneratorNum).A42Model.AirFlowCurveID, MdotFuel);
                MdotAir = max(0.0, MdotAir); // protect against bad curve result
                if (PLRforSubtimestepStartUp < 1.0) {
                    Pnetss = AllowedLoad;
                }

            } else if (SELECT_CASE_var == DataGenerators::OpModeCoolDown) {

                Pnetss = 0.0;
                Pstandby = 0.0;
                Pcooler = MicroCHP(GeneratorNum).A42Model.PcoolDown;
                TcwIn = DataLoopNode::Node(MicroCHP(GeneratorNum).PlantInletNodeID).Temp;          // C
                MdotCW = DataLoopNode::Node(MicroCHP(GeneratorNum).PlantInletNodeID).MassFlowRate; // kg/s
                if (MicroCHP(GeneratorNum).A42Model.InternalFlowControl) {
                    MdotCW = GeneratorDynamicsManager::FuncDetermineCWMdotForInternalFlowControl(GeneratorNum, Pnetss, TcwIn);
                }
                NdotFuel = 0.0;
                MdotFuel = 0.0;
                MdotAir = 0.0;
                ElecEff = 0.0;
                ThermEff = 0.0;
                Qgross = 0.0;
                Qgenss = 0.0;
            }
        }

        for (int i = 1; i <= 20; ++i) { // sequential search with exit criteria
            // calculate new value for engine temperature
            // for Stirling in warmup, need to include dependency of Qgness on Teng
            if ((MicroCHP(GeneratorNum).A42Model.WarmUpByEngineTemp) && (CurrentOpMode == DataGenerators::OpModeWarmUp)) {

                Real64 Pmax = MicroCHP(GeneratorNum).A42Model.MaxElecPower;
                TcwIn = DataLoopNode::Node(MicroCHP(GeneratorNum).PlantInletNodeID).Temp;          // C
                MdotCW = DataLoopNode::Node(MicroCHP(GeneratorNum).PlantInletNodeID).MassFlowRate; // kg/s
                ElecEff = CurveManager::CurveValue(MicroCHP(GeneratorNum).A42Model.ElecEffCurveID, Pmax, MdotCW, TcwIn);
                ElecEff = max(0.0, ElecEff); // protect against bad curve result
                if (ElecEff > 0.0) {         // trap divide by bad thing
                    Qgross = Pmax / ElecEff; // W
                } else {
                    Qgross = 0.0;
                }
                NdotFuel = Qgross / (DataGenerators::FuelSupply(MicroCHP(GeneratorNum).FuelSupplyID).LHV * 1000.0 * 1000.0);
                //  kMol/s = (J/s) /(KJ/mol * 1000 J/KJ * 1000 mol/kmol)
                Real64 MdotFuelMax = NdotFuel * DataGenerators::FuelSupply(MicroCHP(GeneratorNum).FuelSupplyID).KmolPerSecToKgPerSec;

                Real64 MdotFuelWarmup;
                if (Teng > thisAmbientTemp) {
                    MdotFuelWarmup = MdotFuelMax + MicroCHP(GeneratorNum).A42Model.kf * MdotFuelMax *
                                                       ((MicroCHP(GeneratorNum).A42Model.TnomEngOp - thisAmbientTemp) / (Teng - thisAmbientTemp));

                    // check that numerical answer didn't blow up beyond limit, and reset if it did
                    if (MdotFuelWarmup > MicroCHP(GeneratorNum).A42Model.Rfuelwarmup * MdotFuelMax) {
                        MdotFuelWarmup = MicroCHP(GeneratorNum).A42Model.Rfuelwarmup * MdotFuelMax;
                    }
                    if (MicroCHP(GeneratorNum).A42Model.TnomEngOp > thisAmbientTemp) {
                        Pnetss = Pmax * MicroCHP(GeneratorNum).A42Model.kp *
                                 ((Teng - thisAmbientTemp) / (MicroCHP(GeneratorNum).A42Model.TnomEngOp - thisAmbientTemp));
                    } else if (MicroCHP(GeneratorNum).A42Model.TnomEngOp < thisAmbientTemp) {
                        Pnetss = Pmax;
                    } else { // equal would divide by zero
                        Pnetss = Pmax;
                    }
                } else { // equal would divide by zero
                    MdotFuelWarmup = MicroCHP(GeneratorNum).A42Model.Rfuelwarmup * MdotFuelMax;
                }
                MdotFuel = MdotFuelWarmup;
                NdotFuel = MdotFuel / DataGenerators::FuelSupply(MicroCHP(GeneratorNum).FuelSupplyID).KmolPerSecToKgPerSec;
                MdotAir = CurveManager::CurveValue(MicroCHP(GeneratorNum).A42Model.AirFlowCurveID, MdotFuelWarmup);
                MdotAir = max(0.0, MdotAir); // protect against bad curve result
                Qgross = NdotFuel * (DataGenerators::FuelSupply(MicroCHP(GeneratorNum).FuelSupplyID).LHV * 1000.0 * 1000.0);
                ThermEff = CurveManager::CurveValue(MicroCHP(GeneratorNum).A42Model.ThermalEffCurveID, Pmax, MdotCW, TcwIn);
                ThermEff = max(0.0, ThermEff); // protect against bad curve result
                Qgenss = ThermEff * Qgross;    // W
            }

            Real64 dt = DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour;

            Teng = FuncDetermineEngineTemp(TcwOut,
                                           MicroCHP(GeneratorNum).A42Model.MCeng,
                                           MicroCHP(GeneratorNum).A42Model.UAhx,
                                           MicroCHP(GeneratorNum).A42Model.UAskin,
                                           thisAmbientTemp,
                                           Qgenss,
                                           MicroCHP(GeneratorNum).A42Model.TengLast,
                                           dt);

            Real64 Cp = FluidProperties::GetSpecificHeatGlycol(
                DataPlant::PlantLoop(MicroCHP(GeneratorNum).CWLoopNum).FluidName, TcwIn, DataPlant::PlantLoop(MicroCHP(GeneratorNum).CWLoopNum).FluidIndex, RoutineName);

            TcwOut = FuncDetermineCoolantWaterExitTemp(TcwIn,
                                                       MicroCHP(GeneratorNum).A42Model.MCcw,
                                                       MicroCHP(GeneratorNum).A42Model.UAhx,
                                                       MdotCW * Cp,
                                                       Teng,
                                                       MicroCHP(GeneratorNum).A42Model.TempCWOutLast,
                                                       dt);

            // form balance and exit once met.
            bool EnergyBalOK = CheckMicroCHPThermalBalance(MicroCHP(GeneratorNum).A42Model.MaxElecPower,
                                                      TcwIn,
                                                      TcwOut,
                                                      Teng,
                                                      thisAmbientTemp,
                                                      MicroCHP(GeneratorNum).A42Model.UAhx,
                                                      MicroCHP(GeneratorNum).A42Model.UAskin,
                                                      Qgenss,
                                                      MicroCHP(GeneratorNum).A42Model.MCeng,
                                                      MicroCHP(GeneratorNum).A42Model.MCcw,
                                                      MdotCW * Cp);

            if (EnergyBalOK && (i > 4)) break;
        }

        MicroCHP(GeneratorNum).PlantMassFlowRate = MdotCW;
        MicroCHP(GeneratorNum).A42Model.Pnet = Pnetss - Pcooler - Pstandby;
        MicroCHP(GeneratorNum).A42Model.ElecEff = ElecEff;
        MicroCHP(GeneratorNum).A42Model.Qgross = Qgross;
        MicroCHP(GeneratorNum).A42Model.ThermEff = ThermEff;
        MicroCHP(GeneratorNum).A42Model.Qgenss = Qgenss;
        MicroCHP(GeneratorNum).A42Model.NdotFuel = NdotFuel;
        MicroCHP(GeneratorNum).A42Model.MdotFuel = MdotFuel;
        MicroCHP(GeneratorNum).A42Model.Teng = Teng;
        MicroCHP(GeneratorNum).A42Model.TcwOut = TcwOut;
        MicroCHP(GeneratorNum).A42Model.TcwIn = TcwIn;
        MicroCHP(GeneratorNum).A42Model.MdotAir = MdotAir;
        MicroCHP(GeneratorNum).A42Model.QdotSkin = MicroCHP(GeneratorNum).A42Model.UAskin * (Teng - thisAmbientTemp);

        MicroCHP(GeneratorNum).A42Model.OpMode = CurrentOpMode;
    }

    Real64 FuncDetermineEngineTemp(Real64 const TcwOut,   // hot water leaving temp
                                   Real64 const MCeng,    // Fictitious mass and heat capacity of engine
                                   Real64 const UAHX,     // Heat exchanger UA
                                   Real64 const UAskin,   // Skin losses UA
                                   Real64 const Troom,    // surrounding zone temperature C
                                   Real64 const Qgenss,   // steady state generator heat generation
                                   Real64 const TengLast, // engine temp at previous time step
                                   Real64 const time      // elapsed time since previous evaluation
    )
    {

        // FUNCTION INFORMATION:
        //       AUTHOR         B. Griffith
        //       DATE WRITTEN   Feb. 2007
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS FUNCTION:
        // Calculate engine temperaure,

        // METHODOLOGY EMPLOYED:
        // model is dynamic in that previous condition affects current timestep
        //  solve ode for engine temp using analytical solution

        Real64 a = ((UAHX * TcwOut / MCeng) + (UAskin * Troom / MCeng) + (Qgenss / MCeng));
        Real64 b = ((-1.0 * UAHX / MCeng) + (-1.0 * UAskin / MCeng));

        return (TengLast + a / b) * std::exp(b * time) - a / b;
    }

    Real64 FuncDetermineCoolantWaterExitTemp(Real64 const TcwIn,      // hot water inlet temp
                                             Real64 const MCcw,       // Fictitious mass and heat capacity of coolant hx
                                             Real64 const UAHX,       // Heat exchanger UA
                                             Real64 const MdotCpcw,   // mass flow and specific heat of coolant water
                                             Real64 const Teng,       // engine mass temperature C
                                             Real64 const TcwoutLast, // coolant water leaving temp at previous time step
                                             Real64 const time        // elapsed time since previous evaluation
    )
    {

        // FUNCTION INFORMATION:
        //       AUTHOR         B. Griffith
        //       DATE WRITTEN   Feb. 2007
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS FUNCTION:
        // Calculate coolan water leaving temperaure,

        // METHODOLOGY EMPLOYED:
        // model is dynamic in that previous condition affects current timestep
        //  solve ode for coolant water outlet temp using analytical solution

        Real64 a = (MdotCpcw * TcwIn / MCcw) + (UAHX * Teng / MCcw);
        Real64 b = ((-1.0 * MdotCpcw / MCcw) + (-1.0 * UAHX / MCcw));

        if (b * time < (-1.0 * DataGlobals::MaxEXPArg)) {
            return -a / b;
        } else {
            return (TcwoutLast + a / b) * std::exp(b * time) - a / b;
        }
    }

    bool CheckMicroCHPThermalBalance(Real64 const NomHeatGen, // nominal heat generation rate for scaling
                                     Real64 const TcwIn,      // hot water inlet temp
                                     Real64 const TcwOut,     // hot water leaving temp
                                     Real64 const Teng,       // engine mass temperature C
                                     Real64 const Troom,      // surrounding zone temperature C
                                     Real64 const UAHX,       // Heat exchanger UA
                                     Real64 const UAskin,     // Skin losses UA
                                     Real64 const Qgenss,     // steady state generator heat generation
                                     Real64 const MCeng,      // Fictitious mass and heat capacity of engine
                                     Real64 const MCcw,       // Fictitious mass and heat capacity of coolant hx
                                     Real64 const MdotCpcw    // mass flow and specific heat of coolant water
    )
    {

        // FUNCTION INFORMATION:
        //       AUTHOR         B. Griffith
        //       DATE WRITTEN   Feb. 2007
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS FUNCTION:
        // Check for energy balance to test if can exit iteration loop

        // METHODOLOGY EMPLOYED:
        // put all terms of dynamic energy balances on RHS and compute magnitude of imbalance
        //  compare imbalance to scalable thresholds and make a boolean conclusion.

        // first compute derivatives using a + bT
        // derivative of engine temp wrt time
        Real64 a = ((UAHX * TcwOut / MCeng) + (UAskin * Troom / MCeng) + (Qgenss / MCeng));
        Real64 b = ((-1.0 * UAHX / MCeng) + (-1.0 * UAskin / MCeng));
        Real64 DTengDTime = a + b * Teng;

        // derivative of coolant exit temp wrt time
        Real64 c = (MdotCpcw * TcwIn / MCcw) + (UAHX * Teng / MCcw);
        Real64 d = ((-1.0 * MdotCpcw / MCcw) + (-1.0 * UAHX / MCcw));
        Real64 DCoolOutTDtime = c + d * TcwOut;

        // energy imbalance for engine control volume
        Real64 magImbalEng = UAHX * (TcwOut - Teng) + UAskin * (Troom - Teng) + Qgenss - MCeng * DTengDTime;

        // energy imbalance for coolant control volume
        Real64 magImbalCooling = MdotCpcw * (TcwIn - TcwOut) + UAHX * (Teng - TcwOut) - MCcw * DCoolOutTDtime;

        // criteria for when to call energy balance okay
        Real64 threshold = NomHeatGen / 10000000.0;

        if ((threshold > magImbalEng) && (threshold > magImbalCooling)) {
            return true;
        } else {
            return false;
        }
    }

    void FigureMicroCHPZoneGains()
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         B. Griffith
        //       DATE WRITTEN   July 2006
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Couple equiment skin losses to the Zone Heat Balance

        // METHODOLOGY EMPLOYED:
        // This routine adds up the various skin losses and then
        //  sets the values in the ZoneIntGain structure

        static bool MyEnvrnFlag(true);

        if (NumMicroCHPs == 0) return;

        if (DataGlobals::BeginEnvrnFlag && MyEnvrnFlag) {
            for (auto &e : DataGenerators::FuelSupply)
                e.QskinLoss = 0.0;
            for (auto &e : MicroCHP) {
                e.A42Model.QdotSkin = 0.0;
                e.Report.SkinLossConvect = 0.0;
                e.Report.SkinLossRadiat = 0.0;
            }
            MyEnvrnFlag = false;
        }

        if (!DataGlobals::BeginEnvrnFlag) MyEnvrnFlag = true;

        for (int CHPnum = 1; CHPnum <= NumMicroCHPs; ++CHPnum) {
            Real64 TotalZoneHeatGain = DataGenerators::FuelSupply(MicroCHP(CHPnum).FuelSupplyID).QskinLoss + MicroCHP(CHPnum).A42Model.QdotSkin;

            MicroCHP(CHPnum).A42Model.QdotConvZone = TotalZoneHeatGain * (1 - MicroCHP(CHPnum).A42Model.RadiativeFraction);
            MicroCHP(CHPnum).Report.SkinLossConvect = MicroCHP(CHPnum).A42Model.QdotConvZone;
            MicroCHP(CHPnum).A42Model.QdotRadZone = TotalZoneHeatGain * MicroCHP(CHPnum).A42Model.RadiativeFraction;
            MicroCHP(CHPnum).Report.SkinLossRadiat = MicroCHP(CHPnum).A42Model.QdotRadZone;
        }
    }

    void CalcUpdateHeatRecovery(int const Num, // Generator number
                                bool const EP_UNUSED(FirstHVACIteration))
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         B Griffith
        //       DATE WRITTEN   Aug 2006
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // update plant loop interactions, do any calcs needed

        static std::string const RoutineName("CalcUpdateHeatRecovery");

        PlantUtilities::SafeCopyPlantNode(MicroCHP(Num).PlantInletNodeID,  MicroCHP(Num).PlantOutletNodeID);

        DataLoopNode::Node(MicroCHP(Num).PlantOutletNodeID).Temp = MicroCHP(Num).A42Model.TcwOut;

        Real64 Cp = FluidProperties::GetSpecificHeatGlycol(
            DataPlant::PlantLoop(MicroCHP(Num).CWLoopNum).FluidName, MicroCHP(Num).A42Model.TcwIn, DataPlant::PlantLoop(MicroCHP(Num).CWLoopNum).FluidIndex, RoutineName);

        DataLoopNode::Node(MicroCHP(Num).PlantOutletNodeID).Enthalpy = MicroCHP(Num).A42Model.TcwOut * Cp;
    }

    void SimMicroCHPPlantHeatRecovery(std::string const &EP_UNUSED(CompType),
                                      std::string const &CompName,
                                      int &CompNum,
                                      bool const EP_UNUSED(RunFlag),
                                      bool &InitLoopEquip,
                                      Real64 &EP_UNUSED(MyThermalLoad),
                                      Real64 &MaxCap,
                                      Real64 &MinCap,
                                      Real64 &OptCap,
                                      bool const FirstHVACIteration // TRUE if First iteration of simulation
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         B. Griffith
        //       DATE WRITTEN   Jan 2006
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // makes sure input are gotten and setup from Plant loop perspective.
        // does not (re)simulate entire MicroCHP model

        if (GetMicroCHPInput) {
            GetMicroCHPGeneratorInput();
            GetMicroCHPInput = false;
        }

        if (InitLoopEquip) {
            CompNum = UtilityRoutines::FindItemInList(CompName, MicroCHP);
            if (CompNum == 0) {
                ShowFatalError("SimMicroCHPPlantHeatRecovery: MicroCHP Generator Unit not found=" + CompName);
                return;
            }
            InitMicroCHPNoNormalizeGenerators(CompNum, FirstHVACIteration);
            if (MicroCHP(CompNum).MySizeFlag) return;
            MinCap = DataGenerators::GeneratorDynamics(MicroCHP(CompNum).DynamicsControlID).QdotHXMin;
            MaxCap = DataGenerators::GeneratorDynamics(MicroCHP(CompNum).DynamicsControlID).QdotHXMax;
            OptCap = DataGenerators::GeneratorDynamics(MicroCHP(CompNum).DynamicsControlID).QdotHXOpt;
            return;
        } // End Of InitLoopEquip

        PlantUtilities::UpdateComponentHeatRecoverySide(MicroCHP(CompNum).CWLoopNum,
                                        MicroCHP(CompNum).CWLoopSideNum,
                                        DataPlant::TypeOf_Generator_MicroCHP,
                                        MicroCHP(CompNum).PlantInletNodeID,
                                        MicroCHP(CompNum).PlantOutletNodeID,
                                        MicroCHP(CompNum).Report.QdotHR,
                                        MicroCHP(CompNum).Report.HeatRecInletTemp,
                                        MicroCHP(CompNum).Report.HeatRecOutletTemp,
                                        MicroCHP(CompNum).Report.HeatRecMdot,
                                        FirstHVACIteration);
    }

    void UpdateMicroCHPGeneratorRecords(int const Num) // Generator number
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         B. Griffith
        //       DATE WRITTEN   July 2006
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // update variables in structures linked to output reports

        static std::string const RoutineName("UpdateMicroCHPGeneratorRecords");

        MicroCHP(Num).Report.Mode = MicroCHP(Num).A42Model.OpMode;
        MicroCHP(Num).Report.OffModeTime = MicroCHP(Num).A42Model.OffModeTime;
        MicroCHP(Num).Report.StandyByModeTime = MicroCHP(Num).A42Model.StandyByModeTime;
        MicroCHP(Num).Report.WarmUpModeTime = MicroCHP(Num).A42Model.WarmUpModeTime;
        MicroCHP(Num).Report.NormalModeTime = MicroCHP(Num).A42Model.NormalModeTime;
        MicroCHP(Num).Report.CoolDownModeTime = MicroCHP(Num).A42Model.CoolDownModeTime;

        MicroCHP(Num).Report.ACPowerGen = MicroCHP(Num).A42Model.Pnet;                            // electrical power produced [W]
        MicroCHP(Num).Report.ACEnergyGen = MicroCHP(Num).A42Model.Pnet * DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour; // energy produced (J)
        MicroCHP(Num).Report.QdotGross = MicroCHP(Num).A42Model.Qgross;
        MicroCHP(Num).Report.Qgenss = MicroCHP(Num).A42Model.Qgenss;
        MicroCHP(Num).Report.QdotHX =
            MicroCHP(Num).A42Model.UAhx * (MicroCHP(Num).A42Model.Teng - MicroCHP(Num).A42Model.TcwOut); //  heat recovered rate (W)

        Real64 Cp = FluidProperties::GetSpecificHeatGlycol(
            DataPlant::PlantLoop(MicroCHP(Num).CWLoopNum).FluidName, MicroCHP(Num).A42Model.TcwIn, DataPlant::PlantLoop(MicroCHP(Num).CWLoopNum).FluidIndex, RoutineName);

        MicroCHP(Num).Report.QdotHR = MicroCHP(Num).PlantMassFlowRate * Cp * (MicroCHP(Num).A42Model.TcwOut - MicroCHP(Num).A42Model.TcwIn);
        MicroCHP(Num).Report.TotalHeatEnergyRec = MicroCHP(Num).Report.QdotHR * DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour; // heat recovered energy (J)

        MicroCHP(Num).Report.HeatRecInletTemp = MicroCHP(Num).A42Model.TcwIn;   // Heat Recovery Loop Inlet Temperature (C)
        MicroCHP(Num).Report.HeatRecOutletTemp = MicroCHP(Num).A42Model.TcwOut; // Heat Recovery Loop Outlet Temperature (C)
        MicroCHP(Num).Report.HeatRecMdot = MicroCHP(Num).PlantMassFlowRate;     // Heat Recovery Loop Mass flow rate (kg/s)
        MicroCHP(Num).Report.Tengine = MicroCHP(Num).A42Model.Teng;
        MicroCHP(Num).Report.ElectEfficiency = MicroCHP(Num).A42Model.ElecEff;
        MicroCHP(Num).Report.ThermalEfficiency = MicroCHP(Num).A42Model.ThermEff;

        MicroCHP(Num).Report.OverallEfficiency = MicroCHP(Num).A42Model.ElecEff + MicroCHP(Num).A42Model.ThermEff;

        MicroCHP(Num).Report.MdotAir = MicroCHP(Num).A42Model.MdotAir; // air flow in kg/sec

        MicroCHP(Num).Report.NdotFuel = MicroCHP(Num).A42Model.NdotFuel; // fuel flow in kmol/sec
        MicroCHP(Num).Report.MdotFuel = MicroCHP(Num).A42Model.MdotFuel; // fuel flow in kg/sec

        MicroCHP(Num).Report.FuelCompressPower = DataGenerators::FuelSupply(MicroCHP(Num).FuelSupplyID).PfuelCompEl;
        // electrical power used by fuel supply compressor [W]
        MicroCHP(Num).Report.FuelCompressEnergy = DataGenerators::FuelSupply(MicroCHP(Num).FuelSupplyID).PfuelCompEl * DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour; // elect energy
        MicroCHP(Num).Report.FuelCompressSkinLoss = DataGenerators::FuelSupply(MicroCHP(Num).FuelSupplyID).QskinLoss;
        // heat rate of losses.by fuel supply compressor [W]
        MicroCHP(Num).Report.FuelEnergyHHV = MicroCHP(Num).A42Model.NdotFuel * DataGenerators::FuelSupply(MicroCHP(Num).FuelSupplyID).HHV *
                                             DataGenerators::FuelSupply(MicroCHP(Num).FuelSupplyID).KmolPerSecToKgPerSec * DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour;
        // reporting: Fuel Energy used (W)
        MicroCHP(Num).Report.FuelEnergyUseRateHHV = MicroCHP(Num).A42Model.NdotFuel * DataGenerators::FuelSupply(MicroCHP(Num).FuelSupplyID).HHV *
                                                    DataGenerators::FuelSupply(MicroCHP(Num).FuelSupplyID).KmolPerSecToKgPerSec;
        // reporting: Fuel Energy used (J)
        MicroCHP(Num).Report.FuelEnergyLHV =
            MicroCHP(Num).A42Model.NdotFuel * DataGenerators::FuelSupply(MicroCHP(Num).FuelSupplyID).LHV * 1000000.0 * DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour;
        // reporting: Fuel Energy used (W)
        MicroCHP(Num).Report.FuelEnergyUseRateLHV = MicroCHP(Num).A42Model.NdotFuel * DataGenerators::FuelSupply(MicroCHP(Num).FuelSupplyID).LHV * 1000000.0;

        MicroCHP(Num).Report.SkinLossPower = MicroCHP(Num).A42Model.QdotConvZone + MicroCHP(Num).A42Model.QdotRadZone;
        MicroCHP(Num).Report.SkinLossEnergy = (MicroCHP(Num).A42Model.QdotConvZone + MicroCHP(Num).A42Model.QdotRadZone) * DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour;
        MicroCHP(Num).Report.SkinLossConvect = MicroCHP(Num).A42Model.QdotConvZone;
        MicroCHP(Num).Report.SkinLossRadiat = MicroCHP(Num).A42Model.QdotRadZone;

        // update node data for air inlet (and outlet)
        if (MicroCHP(Num).AirInletNodeID > 0) {
            DataLoopNode::Node(MicroCHP(Num).AirInletNodeID).MassFlowRate = MicroCHP(Num).Report.MdotAir;
        }
        if (MicroCHP(Num).AirOutletNodeID > 0) {
            DataLoopNode::Node(MicroCHP(Num).AirOutletNodeID).MassFlowRate = MicroCHP(Num).Report.MdotAir;
            DataLoopNode::Node(MicroCHP(Num).AirOutletNodeID).Temp = MicroCHP(Num).A42Model.Teng;
        }
    }

    void GetMicroCHPGeneratorResults(int const EP_UNUSED(GeneratorType), // type of Generator
                                     int const GeneratorIndex,
                                     Real64 &GeneratorPower,  // electrical power
                                     Real64 &GeneratorEnergy, // electrical energy
                                     Real64 &ThermalPower,    // heat power
                                     Real64 &ThermalEnergy    // heat energy
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         B. Griffith
        //       DATE WRITTEN   March 2008
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // provide a get method to collect results at the load center level

        GeneratorPower = MicroCHP(GeneratorIndex).Report.ACPowerGen;
        GeneratorEnergy = MicroCHP(GeneratorIndex).Report.ACEnergyGen;
        ThermalPower = MicroCHP(GeneratorIndex).Report.QdotHR;
        ThermalEnergy = MicroCHP(GeneratorIndex).Report.TotalHeatEnergyRec;
    }

} // namespace MicroCHPElectricGenerator

} // namespace EnergyPlus
