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
#include <cmath>

// ObjexxFCL Headers
#include <ObjexxFCL/Array.functions.hh>
#include <ObjexxFCL/Array1D.hh>
#include <ObjexxFCL/Fmath.hh>

// EnergyPlus Headers
#include <EnergyPlus/Construction.hh>
#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/DataAirLoop.hh>
#include <EnergyPlus/DataAirSystems.hh>
#include <EnergyPlus/DataGlobals.hh>
#include <EnergyPlus/DataHeatBalance.hh>
#include <EnergyPlus/DataLoopNode.hh>
#include <EnergyPlus/DataPrecisionGlobals.hh>
#include <EnergyPlus/DataRuntimeLanguage.hh>
#include <EnergyPlus/DataSurfaces.hh>
#include <EnergyPlus/DataZoneControls.hh>
#include <EnergyPlus/EMSManager.hh>
#include <EnergyPlus/General.hh>
#include <EnergyPlus/InputProcessing/InputProcessor.hh>
#include <EnergyPlus/OutAirNodeManager.hh>
#include <EnergyPlus/OutputProcessor.hh>
#include <EnergyPlus/PluginManager.hh>
#include <EnergyPlus/RuntimeLanguageProcessor.hh>
#include <EnergyPlus/ScheduleManager.hh>
#include <EnergyPlus/UtilityRoutines.hh>

namespace EnergyPlus {

// note there are routines that lie outside of the Module at the end of this file

namespace EMSManager {

    // MODULE INFORMATION:
    //       AUTHOR         Peter Graham Ellis
    //       DATE WRITTEN   June 2006
    //       MODIFIED       Brent Griffith
    //                      May - August 2009
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS MODULE:
    // This module manages the programmable energy management system(EMS).

    // METHODOLOGY EMPLOYED:

    // Using/Aliasing
    using namespace DataPrecisionGlobals;
    using namespace DataRuntimeLanguage;

    // Data
    // MODULE PARAMETER DEFINITIONS
    int const iTemperatureSetPoint(101);      // integer for node setpoint control type
    int const iTemperatureMinSetPoint(102);   // integer for node setpoint control type
    int const iTemperatureMaxSetPoint(103);   // integer for node setpoint control type
    int const iHumidityRatioSetPoint(104);    // integer for node setpoint control type
    int const iHumidityRatioMinSetPoint(105); // integer for node setpoint control type
    int const iHumidityRatioMaxSetPoint(106); // integer for node setpoint control type
    int const iMassFlowRateSetPoint(107);     // integer for node setpoint control type
    int const iMassFlowRateMinSetPoint(108);  // integer for node setpoint control type
    int const iMassFlowRateMaxSetPoint(109);  // integer for node setpoint control type

    static std::string const BlankString;

    // DERIVED TYPE DEFINITIONS:

    // MODULE VARIABLE TYPE DECLARATIONS:

    // MODULE VARIABLE DECLARATIONS:
    bool GetEMSUserInput(true); // Flag to prevent input from being read multiple times
    bool ZoneThermostatActuatorsHaveBeenSetup(false);
    bool FinishProcessingUserInput(true); // Flag to indicate still need to process input

    // SUBROUTINE SPECIFICATIONS:

    // Functions
    void clear_state()
    {
        GetEMSUserInput = true;
        ZoneThermostatActuatorsHaveBeenSetup = false;
        FinishProcessingUserInput = true;
    }

    void CheckIfAnyEMS(EnergyPlusData &state)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Brent Griffith
        //       DATE WRITTEN   April 2009
        //       MODIFIED       Rui Zhang February 2010
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Determine if EMS is used in model and set flag
        // This needs to be checked early so calls to SetupEMSActuator
        // can be avoided if there is no EMS in model.
        // We cannot do error checking during the full get input until later in the simulation.

        // METHODOLOGY EMPLOYED:
        // Get number of EMS-related input objects and set
        // global logical AnyEnergyManagementSystemInModel

        // Using/Aliasing
        using DataGlobals::AnyEnergyManagementSystemInModel;
        using General::ScanForReports;

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:

        std::string cCurrentModuleObject;

        cCurrentModuleObject = "EnergyManagementSystem:Sensor";
        NumSensors = inputProcessor->getNumObjectsFound(cCurrentModuleObject);

        cCurrentModuleObject = "EnergyManagementSystem:Actuator";
        numActuatorsUsed = inputProcessor->getNumObjectsFound(cCurrentModuleObject);

        cCurrentModuleObject = "EnergyManagementSystem:ProgramCallingManager";
        NumProgramCallManagers = inputProcessor->getNumObjectsFound(cCurrentModuleObject);

        cCurrentModuleObject = "EnergyManagementSystem:Program";
        NumErlPrograms = inputProcessor->getNumObjectsFound(cCurrentModuleObject);

        cCurrentModuleObject = "EnergyManagementSystem:Subroutine";
        NumErlSubroutines = inputProcessor->getNumObjectsFound(cCurrentModuleObject);

        cCurrentModuleObject = "EnergyManagementSystem:GlobalVariable";
        NumUserGlobalVariables = inputProcessor->getNumObjectsFound(cCurrentModuleObject);

        cCurrentModuleObject = "EnergyManagementSystem:OutputVariable";
        NumEMSOutputVariables = inputProcessor->getNumObjectsFound(cCurrentModuleObject);

        cCurrentModuleObject = "EnergyManagementSystem:MeteredOutputVariable";
        NumEMSMeteredOutputVariables = inputProcessor->getNumObjectsFound(cCurrentModuleObject);

        cCurrentModuleObject = "EnergyManagementSystem:CurveOrTableIndexVariable";
        NumEMSCurveIndices = inputProcessor->getNumObjectsFound(cCurrentModuleObject);

        cCurrentModuleObject = "ExternalInterface:Variable";
        NumExternalInterfaceGlobalVariables = inputProcessor->getNumObjectsFound(cCurrentModuleObject);

        // added for FMUImport
        cCurrentModuleObject = "ExternalInterface:FunctionalMockupUnitImport:To:Variable";
        NumExternalInterfaceFunctionalMockupUnitImportGlobalVariables = inputProcessor->getNumObjectsFound(cCurrentModuleObject);

        // added for FMUExport
        cCurrentModuleObject = "ExternalInterface:FunctionalMockupUnitExport:To:Variable";
        NumExternalInterfaceFunctionalMockupUnitExportGlobalVariables = inputProcessor->getNumObjectsFound(cCurrentModuleObject);

        cCurrentModuleObject = "ExternalInterface:Actuator";
        NumExternalInterfaceActuatorsUsed = inputProcessor->getNumObjectsFound(cCurrentModuleObject);

        // added for FMUImport
        cCurrentModuleObject = "ExternalInterface:FunctionalMockupUnitImport:To:Actuator";
        NumExternalInterfaceFunctionalMockupUnitImportActuatorsUsed = inputProcessor->getNumObjectsFound(cCurrentModuleObject);

        // added for FMUExport
        cCurrentModuleObject = "ExternalInterface:FunctionalMockupUnitExport:To:Actuator";
        NumExternalInterfaceFunctionalMockupUnitExportActuatorsUsed = inputProcessor->getNumObjectsFound(cCurrentModuleObject);

        cCurrentModuleObject = "EnergyManagementSystem:ConstructionIndexVariable";
        NumEMSConstructionIndices = inputProcessor->getNumObjectsFound(cCurrentModuleObject);

        cCurrentModuleObject = "Output:EnergyManagementSystem";
        int NumOutputEMSs = inputProcessor->getNumObjectsFound(cCurrentModuleObject);

        // Python plugin instances also count since actuators need to be set up for them
        int numPythonPlugins = inputProcessor->getNumObjectsFound("PythonPlugin:Instance");
        int numActiveCallbacks = PluginManagement::PluginManager::numActiveCallbacks();

        // added for FMU
        if ((NumSensors + numActuatorsUsed + NumProgramCallManagers + NumErlPrograms + NumErlSubroutines + NumUserGlobalVariables +
             NumEMSOutputVariables + NumEMSCurveIndices + NumExternalInterfaceGlobalVariables + NumExternalInterfaceActuatorsUsed +
             NumEMSConstructionIndices + NumEMSMeteredOutputVariables + NumExternalInterfaceFunctionalMockupUnitImportActuatorsUsed +
             NumExternalInterfaceFunctionalMockupUnitImportGlobalVariables + NumExternalInterfaceFunctionalMockupUnitExportActuatorsUsed +
             NumExternalInterfaceFunctionalMockupUnitExportGlobalVariables + NumOutputEMSs + numPythonPlugins + numActiveCallbacks) > 0) {
            AnyEnergyManagementSystemInModel = true;
        } else {
            AnyEnergyManagementSystemInModel = false;
        }

        AnyEnergyManagementSystemInModel = AnyEnergyManagementSystemInModel || state.dataGlobal->externalHVACManager;

        if (AnyEnergyManagementSystemInModel) {

            ScanForReports(state, "EnergyManagementSystem", OutputEDDFile);
            if (OutputEDDFile) {
                // open up output file for EMS EDD file  EMS Data and Debug
                state.files.edd.ensure_open("CheckIFAnyEMS", state.files.outputControl.edd);
            }
        } else {
            ScanForReports(state, "EnergyManagementSystem", OutputEDDFile);
            if (OutputEDDFile) {
                ShowWarningError("CheckIFAnyEMS: No EnergyManagementSystem has been set up in the input file but output is requested.");
                ShowContinueError("No EDD file will be produced. Refer to EMS Application Guide and/or InputOutput Reference to set up your "
                                  "EnergyManagementSystem.");
            }
        }
    }

    // MODULE SUBROUTINES:

    void ManageEMS(EnergyPlusData &state,
                   int const iCalledFrom,                 // indicates where subroutine was called from, parameters in DataGlobals.
                   bool &anyProgramRan,                   // true if any Erl programs ran for this call
                   Optional_int_const ProgramManagerToRun // specific program manager to run
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Peter Graham Ellis
        //       DATE WRITTEN   June 2006
        //       MODIFIED       na
        //       RE-ENGINEERED  Brent Griffith, April 2009
        //                      added calling point argument and logic.
        //                      Collapsed SimulateEMS into this routine

        // PURPOSE OF THIS SUBROUTINE:

        // METHODOLOGY EMPLOYED:
        // Standard EnergyPlus methodology.

        // Using/Aliasing
        using DataGlobals::AnyEnergyManagementSystemInModel;
        using DataGlobals::emsCallFromExternalInterface;
        using DataGlobals::emsCallFromSetupSimulation;
        using DataGlobals::emsCallFromUserDefinedComponentModel;
        using OutputProcessor::MeterType;
        using OutputProcessor::RealVariables;
        using OutputProcessor::RealVariableType;
        using OutputProcessor::RVariableTypes;
        using RuntimeLanguageProcessor::BeginEnvrnInitializeRuntimeLanguage;
        using RuntimeLanguageProcessor::EvaluateStack;

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:

        int ErlVariableNum;    // local index
        int ProgramManagerNum; // local index and loop
        int ErlProgramNum;     // local index
        int ActuatorUsedLoop;  // local loop
        int EMSActuatorVariableNum;

        int tmpInteger;
        //  INTEGER  :: ProgramNum

        // FLOW:
        anyProgramRan = false;
        if (!AnyEnergyManagementSystemInModel) return; // quick return if nothing to do

        if (iCalledFrom == DataGlobals::emsCallFromBeginNewEvironment) {
            BeginEnvrnInitializeRuntimeLanguage();
            PluginManagement::onBeginEnvironment();
        }

        InitEMS(state, iCalledFrom);

        // also call plugins and callbacks here for convenience
        bool anyPluginsOrCallbacksRan = false;
        if (iCalledFrom != DataGlobals::emsCallFromUserDefinedComponentModel) { // don't run user-defined component plugins this way
            PluginManagement::runAnyRegisteredCallbacks(state, iCalledFrom, anyPluginsOrCallbacksRan);
            if (anyPluginsOrCallbacksRan) {
                anyProgramRan = true;
            }
        }

        if (iCalledFrom == emsCallFromSetupSimulation) {
            ProcessEMSInput(state, true);
            return;
        }

        // Run the Erl programs depending on calling point.

        if (iCalledFrom != emsCallFromUserDefinedComponentModel) {
            for (ProgramManagerNum = 1; ProgramManagerNum <= NumProgramCallManagers; ++ProgramManagerNum) {

                if (EMSProgramCallManager(ProgramManagerNum).CallingPoint == iCalledFrom) {
                    for (ErlProgramNum = 1; ErlProgramNum <= EMSProgramCallManager(ProgramManagerNum).NumErlPrograms; ++ErlProgramNum) {
                        EvaluateStack(state, EMSProgramCallManager(ProgramManagerNum).ErlProgramARR(ErlProgramNum));
                        anyProgramRan = true;
                    }
                }
            }
        } else { // call specific program manager
            if (present(ProgramManagerToRun)) {
                for (ErlProgramNum = 1; ErlProgramNum <= EMSProgramCallManager(ProgramManagerToRun).NumErlPrograms; ++ErlProgramNum) {
                    EvaluateStack(state, EMSProgramCallManager(ProgramManagerToRun).ErlProgramARR(ErlProgramNum));
                    anyProgramRan = true;
                }
            }
        }

        if (iCalledFrom == emsCallFromExternalInterface) {
            anyProgramRan = true;
        }

        if (!anyProgramRan) return;

        // Set actuated variables with new values
        for (ActuatorUsedLoop = 1;
             ActuatorUsedLoop <= numActuatorsUsed + NumExternalInterfaceActuatorsUsed + NumExternalInterfaceFunctionalMockupUnitImportActuatorsUsed +
                                     NumExternalInterfaceFunctionalMockupUnitExportActuatorsUsed;
             ++ActuatorUsedLoop) {
            ErlVariableNum = EMSActuatorUsed(ActuatorUsedLoop).ErlVariableNum;
            if (ErlVariableNum <= 0) continue; // this can happen for good reason during sizing

            EMSActuatorVariableNum = EMSActuatorUsed(ActuatorUsedLoop).ActuatorVariableNum;
            if (EMSActuatorVariableNum <= 0) continue; // this can happen for good reason during sizing

            if (ErlVariable(ErlVariableNum).Value.Type == ValueNull) {
                *EMSActuatorAvailable(EMSActuatorVariableNum).Actuated = false;
            } else {
                // Set the value and the actuated flag remotely on the actuated object via the pointer
                {
                    auto const SELECT_CASE_var(EMSActuatorAvailable(EMSActuatorVariableNum).PntrVarTypeUsed);

                    if (SELECT_CASE_var == PntrReal) {
                        *EMSActuatorAvailable(EMSActuatorVariableNum).Actuated = true;
                        *EMSActuatorAvailable(EMSActuatorVariableNum).RealValue = ErlVariable(ErlVariableNum).Value.Number;
                    } else if (SELECT_CASE_var == PntrInteger) {
                        *EMSActuatorAvailable(EMSActuatorVariableNum).Actuated = true;
                        tmpInteger = std::floor(ErlVariable(ErlVariableNum).Value.Number);
                        *EMSActuatorAvailable(EMSActuatorVariableNum).IntValue = tmpInteger;
                    } else if (SELECT_CASE_var == PntrLogical) {
                        *EMSActuatorAvailable(EMSActuatorVariableNum).Actuated = true;
                        if (ErlVariable(ErlVariableNum).Value.Number == 0.0) {
                            *EMSActuatorAvailable(EMSActuatorVariableNum).LogValue = false;
                        } else if (ErlVariable(ErlVariableNum).Value.Number == 1.0) {
                            *EMSActuatorAvailable(EMSActuatorVariableNum).LogValue = true;
                        } else {
                            *EMSActuatorAvailable(EMSActuatorVariableNum).LogValue = false;
                        }

                    } else {
                    }
                }
            }
        }

        ReportEMS();
    }

    void InitEMS(EnergyPlusData &state, int const iCalledFrom) // indicates where subroutine was called from, parameters in DataGlobals.
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Brent Griffith
        //       DATE WRITTEN   May 2009
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // collect routines needed to initialize EMS

        // METHODOLOGY EMPLOYED:
        // <description>

        // REFERENCES:
        // na

        // Using/Aliasing
        using DataGlobals::BeginEnvrnFlag;
        using DataGlobals::DoingSizing;
        using DataGlobals::emsCallFromSystemSizing;
        using DataGlobals::emsCallFromUserDefinedComponentModel;
        using DataGlobals::emsCallFromZoneSizing;
        using DataGlobals::KickOffSimulation;
        using DataZoneControls::GetZoneAirStatsInputFlag;
        using RuntimeLanguageProcessor::InitializeRuntimeLanguage;
        using RuntimeLanguageProcessor::SetErlValueNumber;
        using ScheduleManager::GetCurrentScheduleValue;

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:

        // SUBROUTINE PARAMETER DEFINITIONS:
        // na

        // INTERFACE BLOCK SPECIFICATIONS:
        // na

        // DERIVED TYPE DEFINITIONS:
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:

        int InternalVarUsedNum; // local index and loop
        int InternVarAvailNum;  // local index
        int SensorNum;          // local loop and index
        int ErlVariableNum;     // local index
        Real64 tmpReal;         // temporary local integer

        if (GetEMSUserInput) {
            SetupZoneInfoAsInternalDataAvail();
            SetupWindowShadingControlActuators(state);
            SetupSurfaceConvectionActuators();
            SetupSurfaceConstructionActuators();
            SetupSurfaceOutdoorBoundaryConditionActuators();
            SetupZoneOutdoorBoundaryConditionActuators();
            GetEMSInput(state);
            GetEMSUserInput = false;
        }

        if (!GetZoneAirStatsInputFlag && !ZoneThermostatActuatorsHaveBeenSetup) {
            SetupThermostatActuators();
            ZoneThermostatActuatorsHaveBeenSetup = true;
        }

        // need to delay setup of HVAC actuator until after the systems input has been processed (if present)
        if (FinishProcessingUserInput && !DoingSizing && !KickOffSimulation) {
            SetupNodeSetPointsAsActuators();
            SetupPrimaryAirSystemAvailMgrAsActuators(state);
            //    CALL SetupWindowShadingControlActuators !this is too late for including in sizing, moved to GetEMSUserInput
            //    CALL SetupThermostatActuators !this is too late for including in sizing, moved to GetEMSUserInput
            //    CALL SetupSurfaceConvectionActuators !this is too late for including in sizing, moved to GetEMSUserInput
            FinishProcessingUserInput = false;
        }

        InitializeRuntimeLanguage(state);

        if ((BeginEnvrnFlag) || (iCalledFrom == emsCallFromZoneSizing) || (iCalledFrom == emsCallFromSystemSizing) ||
            (iCalledFrom == emsCallFromUserDefinedComponentModel)) {

            // another pass at trying to setup input data.
            if (FinishProcessingUserInput) {
                ProcessEMSInput(state, false);
            }

            // update internal data variables being used by Erl
            for (InternalVarUsedNum = 1; InternalVarUsedNum <= NumInternalVariablesUsed; ++InternalVarUsedNum) {
                ErlVariableNum = EMSInternalVarsUsed(InternalVarUsedNum).ErlVariableNum;
                InternVarAvailNum = EMSInternalVarsUsed(InternalVarUsedNum).InternVarNum;
                if (!(InternVarAvailNum > 0)) continue; // sometimes executes before completely finished setting up.
                if (!(ErlVariableNum > 0)) continue;

                {
                    auto const SELECT_CASE_var(EMSInternalVarsAvailable(InternVarAvailNum).PntrVarTypeUsed);

                    if (SELECT_CASE_var == PntrReal) {

                        ErlVariable(ErlVariableNum).Value = SetErlValueNumber(*EMSInternalVarsAvailable(InternVarAvailNum).RealValue);

                    } else if (SELECT_CASE_var == PntrInteger) {

                        tmpReal = double(*EMSInternalVarsAvailable(InternVarAvailNum).IntValue);
                        ErlVariable(ErlVariableNum).Value = SetErlValueNumber(tmpReal);
                    }
                }
            }
        }

        // Update sensors with current data
        for (SensorNum = 1; SensorNum <= NumSensors; ++SensorNum) {
            ErlVariableNum = Sensor(SensorNum).VariableNum;
            if ((ErlVariableNum > 0) && (Sensor(SensorNum).Index > 0)) {
                if (Sensor(SensorNum).SchedNum == 0) { // not a schedule so get from output processor

                    ErlVariable(ErlVariableNum).Value = SetErlValueNumber(GetInternalVariableValue(Sensor(SensorNum).Type, Sensor(SensorNum).Index),
                                                                          ErlVariable(ErlVariableNum).Value);
                } else { // schedule so use schedule service

                    ErlVariable(ErlVariableNum).Value =
                        SetErlValueNumber(GetCurrentScheduleValue(Sensor(SensorNum).SchedNum), ErlVariable(ErlVariableNum).Value);
                }
            }
        }
    }

    void ReportEMS()
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Peter Graham Ellis
        //       DATE WRITTEN   June 2006
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Calculates report variables.

        // METHODOLOGY EMPLOYED:
        // Standard EnergyPlus methodology.

        // Using/Aliasing
        using RuntimeLanguageProcessor::ReportRuntimeLanguage;

        // SUBROUTINE ARGUMENT DEFINITIONS:
        // INTEGER, INTENT(IN) :: ListNum

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:

        // FLOW:
        ReportRuntimeLanguage();
    }

    void GetEMSInput(EnergyPlusData &state)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Peter Graham Ellis
        //       DATE WRITTEN   June 2006
        //       MODIFIED       BG April 2009, finishing, renaming, etc.
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Gets the EMS input from the input file.

        // METHODOLOGY EMPLOYED:
        // Standard EnergyPlus methodology.

        // Using/Aliasing
        using DataGlobals::AnyEnergyManagementSystemInModel;
        using DataGlobals::emsCallFromAfterHVACManagers;
        using DataGlobals::emsCallFromBeforeHVACManagers;
        using DataGlobals::emsCallFromBeginNewEvironmentAfterWarmUp;
        using DataGlobals::emsCallFromBeginZoneTimestepBeforeInitHeatBalance;
        using DataGlobals::emsCallFromBeginZoneTimestepAfterInitHeatBalance;
        using DataGlobals::emsCallFromBeginTimestepBeforePredictor;
        using DataGlobals::emsCallFromComponentGetInput;
        using DataGlobals::emsCallFromEndSystemTimestepAfterHVACReporting;
        using DataGlobals::emsCallFromEndSystemTimestepBeforeHVACReporting;
        using DataGlobals::emsCallFromEndZoneTimestepAfterZoneReporting;
        using DataGlobals::emsCallFromEndZoneTimestepBeforeZoneReporting;
        using DataGlobals::emsCallFromHVACIterationLoop;
        using DataGlobals::emsCallFromSystemSizing;
        using DataGlobals::emsCallFromUnitarySystemSizing;
        using DataGlobals::emsCallFromUserDefinedComponentModel;
        using DataGlobals::emsCallFromZoneSizing;
        using RuntimeLanguageProcessor::ExternalInterfaceInitializeErlVariable;
        using RuntimeLanguageProcessor::FindEMSVariable;
        using RuntimeLanguageProcessor::InitializeRuntimeLanguage;
        using RuntimeLanguageProcessor::NewEMSVariable;
        using RuntimeLanguageProcessor::SetErlValueNumber;

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:

        int StackNum;
        int SensorNum;
        int ActuatorNum;
        int ActuatorVariableNum;
        int VariableNum; // local do loop index
        int NumAlphas;   // Number of elements in the alpha array
        int NumNums;     // Number of elements in the numeric array
        int AlphaNum;
        int IOStat; // IO Status when calling get input subroutine
        static bool ErrorsFound(false);
        Array1D_string cAlphaFieldNames;
        Array1D_string cNumericFieldNames;
        Array1D_bool lNumericFieldBlanks;
        Array1D_bool lAlphaFieldBlanks;
        Array1D_string cAlphaArgs;
        Array1D<Real64> rNumericArgs;
        std::string cCurrentModuleObject;
        int VarType;
        int VarIndex;
        bool FoundObjectType;
        bool FoundObjectName;
        bool FoundActuatorName;
        int NumErlProgramsThisManager; // temporary size of Erl programs in EMSProgramCallManager
        int ManagerProgramNum;         // index counter for Erl programs inside EMSProgramCallManager
        int CallManagerNum;            // loop counter for EMSProgramCallManager structure
        int InternVarNum;              // do loop counter for internal variables used (outer)
        int InternalVarAvailNum;       // do loop counter for internal variables available (inner)
        int Loop;                      // do loop counter
        static int MaxNumAlphas(0);    // argument for call to GetObjectDefMaxArgs
        static int MaxNumNumbers(0);   // argument for call to GetObjectDefMaxArgs
        static int TotalArgs(0);       // argument for call to GetObjectDefMaxArgs
        bool errFlag;

        // FLOW:
        cCurrentModuleObject = "EnergyManagementSystem:Sensor";
        inputProcessor->getObjectDefMaxArgs(cCurrentModuleObject, TotalArgs, NumAlphas, NumNums);
        MaxNumNumbers = NumNums;
        MaxNumAlphas = NumAlphas;
        cCurrentModuleObject = "EnergyManagementSystem:Actuator";
        inputProcessor->getObjectDefMaxArgs(cCurrentModuleObject, TotalArgs, NumAlphas, NumNums);
        MaxNumNumbers = max(MaxNumNumbers, NumNums);
        MaxNumAlphas = max(MaxNumAlphas, NumAlphas);
        cCurrentModuleObject = "EnergyManagementSystem:ProgramCallingManager";
        inputProcessor->getObjectDefMaxArgs(cCurrentModuleObject, TotalArgs, NumAlphas, NumNums);
        MaxNumNumbers = max(MaxNumNumbers, NumNums);
        MaxNumAlphas = max(MaxNumAlphas, NumAlphas);
        cCurrentModuleObject = "EnergyManagementSystem:Program";
        inputProcessor->getObjectDefMaxArgs(cCurrentModuleObject, TotalArgs, NumAlphas, NumNums);
        MaxNumNumbers = max(MaxNumNumbers, NumNums);
        MaxNumAlphas = max(MaxNumAlphas, NumAlphas);
        cCurrentModuleObject = "EnergyManagementSystem:Subroutine";
        inputProcessor->getObjectDefMaxArgs(cCurrentModuleObject, TotalArgs, NumAlphas, NumNums);
        MaxNumNumbers = max(MaxNumNumbers, NumNums);
        MaxNumAlphas = max(MaxNumAlphas, NumAlphas);
        cCurrentModuleObject = "EnergyManagementSystem:OutputVariable";
        inputProcessor->getObjectDefMaxArgs(cCurrentModuleObject, TotalArgs, NumAlphas, NumNums);
        MaxNumNumbers = max(MaxNumNumbers, NumNums);
        MaxNumAlphas = max(MaxNumAlphas, NumAlphas);
        cCurrentModuleObject = "ExternalInterface:Variable";
        inputProcessor->getObjectDefMaxArgs(cCurrentModuleObject, TotalArgs, NumAlphas, NumNums);
        MaxNumNumbers = max(MaxNumNumbers, NumNums);
        MaxNumAlphas = max(MaxNumAlphas, NumAlphas);
        cCurrentModuleObject = "ExternalInterface:Actuator";
        inputProcessor->getObjectDefMaxArgs(cCurrentModuleObject, TotalArgs, NumAlphas, NumNums);
        MaxNumNumbers = max(MaxNumNumbers, NumNums);
        MaxNumAlphas = max(MaxNumAlphas, NumAlphas);
        cCurrentModuleObject = "ExternalInterface:FunctionalMockupUnitImport:To:Variable";
        inputProcessor->getObjectDefMaxArgs(cCurrentModuleObject, TotalArgs, NumAlphas, NumNums);
        MaxNumNumbers = max(MaxNumNumbers, NumNums);
        MaxNumAlphas = max(MaxNumAlphas, NumAlphas);
        cCurrentModuleObject = "ExternalInterface:FunctionalMockupUnitImport:To:Actuator";
        inputProcessor->getObjectDefMaxArgs(cCurrentModuleObject, TotalArgs, NumAlphas, NumNums);
        MaxNumNumbers = max(MaxNumNumbers, NumNums);
        MaxNumAlphas = max(MaxNumAlphas, NumAlphas);
        cCurrentModuleObject = "ExternalInterface:FunctionalMockupUnitExport:To:Variable";
        inputProcessor->getObjectDefMaxArgs(cCurrentModuleObject, TotalArgs, NumAlphas, NumNums);
        MaxNumNumbers = max(MaxNumNumbers, NumNums);
        MaxNumAlphas = max(MaxNumAlphas, NumAlphas);
        cCurrentModuleObject = "ExternalInterface:FunctionalMockupUnitExport:To:Actuator";
        inputProcessor->getObjectDefMaxArgs(cCurrentModuleObject, TotalArgs, NumAlphas, NumNums);
        MaxNumNumbers = max(MaxNumNumbers, NumNums);
        MaxNumAlphas = max(MaxNumAlphas, NumAlphas);
        //  cCurrentModuleObject = 'EnergyManagementSystem:Sensor'
        //  CALL inputProcessor->getObjectDefMaxArgs(cCurrentModuleObject,TotalArgs,NumAlphas,NumNums)
        //  MaxNumNumbers=MAX(MaxNumNumbers,NumNums)
        //  MaxNumAlphas=MAX(MaxNumAlphas,NumAlphas)
        cCurrentModuleObject = "EnergyManagementSystem:GlobalVariable";
        inputProcessor->getObjectDefMaxArgs(cCurrentModuleObject, TotalArgs, NumAlphas, NumNums);
        MaxNumNumbers = max(MaxNumNumbers, NumNums);
        MaxNumAlphas = max(MaxNumAlphas, NumAlphas);

        cAlphaFieldNames.allocate(MaxNumAlphas);
        cAlphaArgs.allocate(MaxNumAlphas);
        lAlphaFieldBlanks.dimension(MaxNumAlphas, false);
        cNumericFieldNames.allocate(MaxNumNumbers);
        rNumericArgs.dimension(MaxNumNumbers, 0.0);
        lNumericFieldBlanks.dimension(MaxNumNumbers, false);

        cCurrentModuleObject = "EnergyManagementSystem:Sensor";
        if (NumSensors > 0) {
            Sensor.allocate(NumSensors);

            for (SensorNum = 1; SensorNum <= NumSensors; ++SensorNum) {
                inputProcessor->getObjectItem(state,
                                              cCurrentModuleObject,
                                              SensorNum,
                                              cAlphaArgs,
                                              NumAlphas,
                                              rNumericArgs,
                                              NumNums,
                                              IOStat,
                                              lNumericFieldBlanks,
                                              lAlphaFieldBlanks,
                                              cAlphaFieldNames,
                                              cNumericFieldNames);
                UtilityRoutines::IsNameEmpty(cAlphaArgs(1), cCurrentModuleObject, ErrorsFound);
                ValidateEMSVariableName(cCurrentModuleObject, cAlphaArgs(1), cAlphaFieldNames(1), errFlag, ErrorsFound);
                if (!errFlag) {
                    Sensor(SensorNum).Name = cAlphaArgs(1);

                    // really needs to check for conflicts with program and function names too...done later
                    VariableNum = FindEMSVariable(cAlphaArgs(1), 0);

                    if (VariableNum > 0) {
                        ShowSevereError("Invalid " + cAlphaFieldNames(1) + '=' + cAlphaArgs(1));
                        ShowContinueError("Entered in " + cCurrentModuleObject + '=' + cAlphaArgs(1));
                        ShowContinueError("Object name conflicts with a global variable name in EMS");
                        ErrorsFound = true;
                    } else {
                        VariableNum = NewEMSVariable(cAlphaArgs(1), 0);
                        Sensor(SensorNum).VariableNum = VariableNum;
                        ErlVariable(VariableNum).Value.initialized = true;
                    }
                }

                if (cAlphaArgs(2) == "*") cAlphaArgs(2).clear();
                Sensor(SensorNum).UniqueKeyName = cAlphaArgs(2);
                Sensor(SensorNum).OutputVarName = cAlphaArgs(3);

                VarIndex = GetMeterIndex(cAlphaArgs(3));
                if (VarIndex > 0) {
                    if (!lAlphaFieldBlanks(2)) {
                        ShowWarningError("Unused" + cAlphaFieldNames(2) + '=' + cAlphaArgs(2));
                        ShowContinueError("Entered in " + cCurrentModuleObject + '=' + cAlphaArgs(1));
                        ShowContinueError("Meter Name found; Key Name will be ignored"); // why meters have no keys..
                    } else {
                        Sensor(SensorNum).Type = 3;
                        Sensor(SensorNum).Index = VarIndex;
                        Sensor(SensorNum).CheckedOkay = true;
                    }
                } else {
                    // Search for variable names
                    GetVariableTypeAndIndex(state,cAlphaArgs(3), cAlphaArgs(2), VarType, VarIndex);
                    if (VarType != 0) {
                        Sensor(SensorNum).Type = VarType;
                        if (VarIndex != 0) {
                            Sensor(SensorNum).Index = VarIndex;
                            Sensor(SensorNum).CheckedOkay = true;
                        }
                    }
                }

            } // SensorNum
        }

        cCurrentModuleObject = "EnergyManagementSystem:Actuator";

        if (numActuatorsUsed + NumExternalInterfaceActuatorsUsed + NumExternalInterfaceFunctionalMockupUnitImportActuatorsUsed +
                NumExternalInterfaceFunctionalMockupUnitExportActuatorsUsed >
            0) {
            EMSActuatorUsed.allocate(numActuatorsUsed + NumExternalInterfaceActuatorsUsed +
                                     NumExternalInterfaceFunctionalMockupUnitImportActuatorsUsed +
                                     NumExternalInterfaceFunctionalMockupUnitExportActuatorsUsed);
            for (ActuatorNum = 1;
                 ActuatorNum <= numActuatorsUsed + NumExternalInterfaceActuatorsUsed + NumExternalInterfaceFunctionalMockupUnitImportActuatorsUsed +
                                    NumExternalInterfaceFunctionalMockupUnitExportActuatorsUsed;
                 ++ActuatorNum) {
                // If we process the ExternalInterface actuators, all we need to do is to change the
                // name of the module object, and shift the ActuatorNum in GetObjectItem
                if (ActuatorNum <= numActuatorsUsed) {
                    inputProcessor->getObjectItem(state,
                                                  cCurrentModuleObject,
                                                  ActuatorNum,
                                                  cAlphaArgs,
                                                  NumAlphas,
                                                  rNumericArgs,
                                                  NumNums,
                                                  IOStat,
                                                  lNumericFieldBlanks,
                                                  lAlphaFieldBlanks,
                                                  cAlphaFieldNames,
                                                  cNumericFieldNames);
                } else if (ActuatorNum > numActuatorsUsed && ActuatorNum <= numActuatorsUsed + NumExternalInterfaceActuatorsUsed) {
                    cCurrentModuleObject = "ExternalInterface:Actuator";
                    inputProcessor->getObjectItem(state,
                                                  cCurrentModuleObject,
                                                  ActuatorNum - numActuatorsUsed,
                                                  cAlphaArgs,
                                                  NumAlphas,
                                                  rNumericArgs,
                                                  NumNums,
                                                  IOStat,
                                                  lNumericFieldBlanks,
                                                  lAlphaFieldBlanks,
                                                  cAlphaFieldNames,
                                                  cNumericFieldNames);
                } else if (ActuatorNum > numActuatorsUsed + NumExternalInterfaceActuatorsUsed &&
                           ActuatorNum <=
                               (numActuatorsUsed + NumExternalInterfaceActuatorsUsed + NumExternalInterfaceFunctionalMockupUnitImportActuatorsUsed)) {
                    cCurrentModuleObject = "ExternalInterface:FunctionalMockupUnitImport:To:Actuator";
                    inputProcessor->getObjectItem(state,
                                                  cCurrentModuleObject,
                                                  ActuatorNum - numActuatorsUsed - NumExternalInterfaceActuatorsUsed,
                                                  cAlphaArgs,
                                                  NumAlphas,
                                                  rNumericArgs,
                                                  NumNums,
                                                  IOStat,
                                                  lNumericFieldBlanks,
                                                  lAlphaFieldBlanks,
                                                  cAlphaFieldNames,
                                                  cNumericFieldNames);
                } else if (ActuatorNum >
                               numActuatorsUsed + NumExternalInterfaceActuatorsUsed + NumExternalInterfaceFunctionalMockupUnitImportActuatorsUsed &&
                           ActuatorNum <= numActuatorsUsed + NumExternalInterfaceActuatorsUsed +
                                              NumExternalInterfaceFunctionalMockupUnitImportActuatorsUsed +
                                              NumExternalInterfaceFunctionalMockupUnitExportActuatorsUsed) {
                    cCurrentModuleObject = "ExternalInterface:FunctionalMockupUnitExport:To:Actuator";
                    inputProcessor->getObjectItem(state,
                                                  cCurrentModuleObject,
                                                  ActuatorNum - numActuatorsUsed - NumExternalInterfaceActuatorsUsed -
                                                      NumExternalInterfaceFunctionalMockupUnitImportActuatorsUsed,
                                                  cAlphaArgs,
                                                  NumAlphas,
                                                  rNumericArgs,
                                                  NumNums,
                                                  IOStat,
                                                  lNumericFieldBlanks,
                                                  lAlphaFieldBlanks,
                                                  cAlphaFieldNames,
                                                  cNumericFieldNames);
                }

                UtilityRoutines::IsNameEmpty(cAlphaArgs(1), cCurrentModuleObject, ErrorsFound);
                ValidateEMSVariableName(cCurrentModuleObject, cAlphaArgs(1), cAlphaFieldNames(1), errFlag, ErrorsFound);
                if (!errFlag) {
                    EMSActuatorUsed(ActuatorNum).Name = cAlphaArgs(1);

                    // really needs to check for conflicts with program and function names too...
                    VariableNum = FindEMSVariable(cAlphaArgs(1), 0);

                    if (VariableNum > 0) {
                        ShowSevereError("Invalid " + cAlphaFieldNames(1) + '=' + cAlphaArgs(1));
                        ShowContinueError("Entered in " + cCurrentModuleObject + '=' + cAlphaArgs(1));
                        ShowContinueError("Object name conflicts with a global variable name in EMS");
                        ErrorsFound = true;
                    } else {
                        VariableNum = NewEMSVariable(cAlphaArgs(1), 0);
                        EMSActuatorUsed(ActuatorNum).ErlVariableNum = VariableNum;
                        // initialize Erl variable for actuator to null
                        ErlVariable(VariableNum).Value = Null;
                        if (ActuatorNum > numActuatorsUsed) {
                            // Initialize variables for the ExternalInterface variables
                            ExternalInterfaceInitializeErlVariable(VariableNum, SetErlValueNumber(rNumericArgs(1)), lNumericFieldBlanks(1));
                        }
                    }
                }

                // need to store characters to finish processing later (once available Actuators have all been setup)
                EMSActuatorUsed(ActuatorNum).ComponentTypeName = cAlphaArgs(3);
                EMSActuatorUsed(ActuatorNum).UniqueIDName = cAlphaArgs(2);
                EMSActuatorUsed(ActuatorNum).ControlTypeName = cAlphaArgs(4);

                FoundObjectType = false;
                FoundObjectName = false;
                FoundActuatorName = false;
                for (ActuatorVariableNum = 1; ActuatorVariableNum <= numEMSActuatorsAvailable; ++ActuatorVariableNum) {
                    if (UtilityRoutines::SameString(EMSActuatorAvailable(ActuatorVariableNum).ComponentTypeName, cAlphaArgs(3))) {
                        FoundObjectType = true;
                        if (UtilityRoutines::SameString(EMSActuatorAvailable(ActuatorVariableNum).UniqueIDName, cAlphaArgs(2))) {
                            FoundObjectName = true;
                            if (UtilityRoutines::SameString(EMSActuatorAvailable(ActuatorVariableNum).ControlTypeName, cAlphaArgs(4))) {
                                FoundActuatorName = true;
                                break;
                            }
                        }
                    }
                }

                if (FoundActuatorName) {
                    // SetupNodeSetPointAsActuators has NOT been called yet at this point
                    EMSActuatorUsed(ActuatorNum).ActuatorVariableNum = ActuatorVariableNum;
                    EMSActuatorUsed(ActuatorNum).CheckedOkay = true;

                    int nHandle = EMSActuatorAvailable(ActuatorVariableNum).handleCount;
                    if (nHandle > 0) {
                        EnergyPlus::ShowWarningError("Seems like you already tried to get a Handle on this Actuator " + std::to_string(nHandle) + "times.");
                        EnergyPlus::ShowContinueError("Occurred for componentType='" +  EMSActuatorUsed(ActuatorNum).ComponentTypeName
                                + "', controlType='" + EMSActuatorUsed(ActuatorNum).ControlTypeName
                                + "', uniqueKey='" + EMSActuatorUsed(ActuatorNum).UniqueIDName + "'.");
                        EnergyPlus::ShowContinueError("You should take note that there is a risk of overwritting.");
                    }
                    ++EMSActuatorAvailable(ActuatorVariableNum).handleCount;
                }
            } // ActuatorNum
        }

        cCurrentModuleObject = "EnergyManagementSystem:InternalVariable";
        NumInternalVariablesUsed = inputProcessor->getNumObjectsFound(cCurrentModuleObject);
        if (NumInternalVariablesUsed > 0) {
            EMSInternalVarsUsed.allocate(NumInternalVariablesUsed);

            for (InternVarNum = 1; InternVarNum <= NumInternalVariablesUsed; ++InternVarNum) {
                inputProcessor->getObjectItem(state,
                                              cCurrentModuleObject,
                                              InternVarNum,
                                              cAlphaArgs,
                                              NumAlphas,
                                              rNumericArgs,
                                              NumNums,
                                              IOStat,
                                              lNumericFieldBlanks,
                                              lAlphaFieldBlanks,
                                              cAlphaFieldNames,
                                              cNumericFieldNames);

                UtilityRoutines::IsNameEmpty(cAlphaArgs(1), cCurrentModuleObject, ErrorsFound);
                ValidateEMSVariableName(cCurrentModuleObject, cAlphaArgs(1), cAlphaFieldNames(1), errFlag, ErrorsFound);
                if (!errFlag) {
                    EMSInternalVarsUsed(InternVarNum).Name = cAlphaArgs(1);
                    VariableNum = FindEMSVariable(cAlphaArgs(1), 0);
                    if (VariableNum > 0) {
                        ShowSevereError("Invalid " + cAlphaFieldNames(1) + '=' + cAlphaArgs(1));
                        ShowContinueError("Entered in " + cCurrentModuleObject + '=' + cAlphaArgs(1));
                        ShowContinueError("Object name conflicts with a global variable name in EMS");
                        ErrorsFound = true;
                    } else {
                        VariableNum = NewEMSVariable(cAlphaArgs(1), 0);
                        EMSInternalVarsUsed(InternVarNum).ErlVariableNum = VariableNum;
                    }

                    EMSInternalVarsUsed(InternVarNum).UniqueIDName = cAlphaArgs(2);
                    EMSInternalVarsUsed(InternVarNum).InternalDataTypeName = cAlphaArgs(3);

                    FoundObjectType = false;
                    FoundObjectName = false;
                    for (InternalVarAvailNum = 1; InternalVarAvailNum <= numEMSInternalVarsAvailable; ++InternalVarAvailNum) {
                        if (UtilityRoutines::SameString(EMSInternalVarsAvailable(InternalVarAvailNum).DataTypeName, cAlphaArgs(3))) {
                            FoundObjectType = true;
                            if (UtilityRoutines::SameString(EMSInternalVarsAvailable(InternalVarAvailNum).UniqueIDName, cAlphaArgs(2))) {
                                FoundObjectName = true;
                                break; // InternalVarAvailNum now holds needed index pointer
                            }
                        }
                    }

                    if (FoundObjectName) {
                        EMSInternalVarsUsed(InternVarNum).InternVarNum = InternalVarAvailNum;
                        EMSInternalVarsUsed(InternVarNum).CheckedOkay = true;
                    }
                }
            }
        }

        InitializeRuntimeLanguage(state); // Loads built-in globals and functions, then performs GetInput for runtime language objects

        if (NumProgramCallManagers > 0) {
            cCurrentModuleObject = "EnergyManagementSystem:ProgramCallingManager";
            EMSProgramCallManager.allocate(NumProgramCallManagers);

            for (CallManagerNum = 1; CallManagerNum <= NumProgramCallManagers; ++CallManagerNum) {

                inputProcessor->getObjectItem(state,
                                              cCurrentModuleObject,
                                              CallManagerNum,
                                              cAlphaArgs,
                                              NumAlphas,
                                              rNumericArgs,
                                              NumNums,
                                              IOStat,
                                              lNumericFieldBlanks,
                                              lAlphaFieldBlanks,
                                              cAlphaFieldNames,
                                              cNumericFieldNames);

                UtilityRoutines::IsNameEmpty(cAlphaArgs(1), cCurrentModuleObject, ErrorsFound);
                EMSProgramCallManager(CallManagerNum).Name = cAlphaArgs(1);

                {
                    auto const SELECT_CASE_var(cAlphaArgs(2));

                    if (SELECT_CASE_var == "BEGINNEWENVIRONMENT") {
                        EMSProgramCallManager(CallManagerNum).CallingPoint = DataGlobals::emsCallFromBeginNewEvironment;
                    } else if (SELECT_CASE_var == "BEGINZONETIMESTEPBEFORESETCURRENTWEATHER") {
                        EMSProgramCallManager(CallManagerNum).CallingPoint = DataGlobals::emsCallFromBeginZoneTimestepBeforeSetCurrentWeather;
                    } else if (SELECT_CASE_var == "AFTERNEWENVIRONMENTWARMUPISCOMPLETE") {
                        EMSProgramCallManager(CallManagerNum).CallingPoint = emsCallFromBeginNewEvironmentAfterWarmUp;
                    } else if (SELECT_CASE_var == "BEGINZONETIMESTEPBEFOREINITHEATBALANCE") {
                        EMSProgramCallManager(CallManagerNum).CallingPoint = emsCallFromBeginZoneTimestepBeforeInitHeatBalance;
                    } else if (SELECT_CASE_var == "BEGINZONETIMESTEPAFTERINITHEATBALANCE") {
                        EMSProgramCallManager(CallManagerNum).CallingPoint = emsCallFromBeginZoneTimestepAfterInitHeatBalance;
                    } else if (SELECT_CASE_var == "BEGINTIMESTEPBEFOREPREDICTOR") {
                        EMSProgramCallManager(CallManagerNum).CallingPoint = emsCallFromBeginTimestepBeforePredictor;
                    } else if (SELECT_CASE_var == "AFTERPREDICTORBEFOREHVACMANAGERS") {
                        EMSProgramCallManager(CallManagerNum).CallingPoint = emsCallFromBeforeHVACManagers;
                    } else if (SELECT_CASE_var == "AFTERPREDICTORAFTERHVACMANAGERS") {
                        EMSProgramCallManager(CallManagerNum).CallingPoint = emsCallFromAfterHVACManagers;
                    } else if (SELECT_CASE_var == "INSIDEHVACSYSTEMITERATIONLOOP") {
                        EMSProgramCallManager(CallManagerNum).CallingPoint = emsCallFromHVACIterationLoop;
                    } else if (SELECT_CASE_var == "ENDOFZONETIMESTEPBEFOREZONEREPORTING") {
                        EMSProgramCallManager(CallManagerNum).CallingPoint = emsCallFromEndZoneTimestepBeforeZoneReporting;
                    } else if (SELECT_CASE_var == "ENDOFZONETIMESTEPAFTERZONEREPORTING") {
                        EMSProgramCallManager(CallManagerNum).CallingPoint = emsCallFromEndZoneTimestepAfterZoneReporting;
                    } else if (SELECT_CASE_var == "ENDOFSYSTEMTIMESTEPBEFOREHVACREPORTING") {
                        EMSProgramCallManager(CallManagerNum).CallingPoint = emsCallFromEndSystemTimestepBeforeHVACReporting;
                    } else if (SELECT_CASE_var == "ENDOFSYSTEMTIMESTEPAFTERHVACREPORTING") {
                        EMSProgramCallManager(CallManagerNum).CallingPoint = emsCallFromEndSystemTimestepAfterHVACReporting;
                    } else if (SELECT_CASE_var == "ENDOFZONESIZING") {
                        EMSProgramCallManager(CallManagerNum).CallingPoint = emsCallFromZoneSizing;
                    } else if (SELECT_CASE_var == "ENDOFSYSTEMSIZING") {
                        EMSProgramCallManager(CallManagerNum).CallingPoint = emsCallFromSystemSizing;
                    } else if (SELECT_CASE_var == "AFTERCOMPONENTINPUTREADIN") {
                        EMSProgramCallManager(CallManagerNum).CallingPoint = emsCallFromComponentGetInput;
                    } else if (SELECT_CASE_var == "USERDEFINEDCOMPONENTMODEL") {
                        EMSProgramCallManager(CallManagerNum).CallingPoint = emsCallFromUserDefinedComponentModel;
                    } else if (SELECT_CASE_var == "UNITARYSYSTEMSIZING") {
                        EMSProgramCallManager(CallManagerNum).CallingPoint = emsCallFromUnitarySystemSizing;
                    } else {
                        ShowSevereError("Invalid " + cAlphaFieldNames(2) + '=' + cAlphaArgs(2));
                        ShowContinueError("Entered in " + cCurrentModuleObject + '=' + cAlphaArgs(1));
                        ErrorsFound = true;
                    }
                }

                NumErlProgramsThisManager = NumAlphas - 2;
                EMSProgramCallManager(CallManagerNum).NumErlPrograms = NumErlProgramsThisManager;
                EMSProgramCallManager(CallManagerNum).ErlProgramARR.allocate(NumErlProgramsThisManager);
                ManagerProgramNum = 0;
                for (AlphaNum = 3; AlphaNum <= NumAlphas; ++AlphaNum) {
                    // find program name in Stack structure
                    if (lAlphaFieldBlanks(AlphaNum)) { // throw error
                        ShowSevereError("Invalid " + cAlphaFieldNames(AlphaNum) + '=' + cAlphaArgs(AlphaNum));
                        ShowContinueError("Entered in " + cCurrentModuleObject + '=' + cAlphaArgs(1));
                        ShowContinueError("Program names cannot be blank");
                        ErrorsFound = true;
                    }

                    StackNum = UtilityRoutines::FindItemInList(cAlphaArgs(AlphaNum), ErlStack);

                    if (StackNum > 0) { // found it
                        // check for duplicate and warn.
                        for (Loop = 1; Loop <= ManagerProgramNum; ++Loop) {
                            if (EMSProgramCallManager(CallManagerNum).ErlProgramARR(Loop) == StackNum) {
                                ShowWarningError("Duplicate " + cAlphaFieldNames(AlphaNum) + '=' + cAlphaArgs(AlphaNum));
                                ShowContinueError("Entered in " + cCurrentModuleObject + '=' + cAlphaArgs(1));
                                ShowContinueError("Erl program appears more than once, and the simulation continues.");
                            }
                        }

                        ++ManagerProgramNum;

                        EMSProgramCallManager(CallManagerNum).ErlProgramARR(ManagerProgramNum) = StackNum;

                    } else {
                        ShowSevereError("Invalid " + cAlphaFieldNames(AlphaNum) + '=' + cAlphaArgs(AlphaNum));
                        ShowContinueError("Entered in " + cCurrentModuleObject + '=' + cAlphaArgs(1));
                        ShowContinueError("Program Name not found.");
                        ErrorsFound = true;
                    }
                } // AlphaNum
            }

        } else { // no program calling manager in input
            if (NumErlPrograms > 0) {
                cCurrentModuleObject = "EnergyManagementSystem:ProgramCallingManager";
                ShowWarningError("Energy Management System is missing input object " + cCurrentModuleObject);
                ShowContinueError("EnergyPlus Runtime Language programs need a calling manager to control when they get executed");
            }
        }

        cAlphaFieldNames.deallocate();
        cAlphaArgs.deallocate();
        lAlphaFieldBlanks.deallocate();
        cNumericFieldNames.deallocate();
        rNumericArgs.deallocate();
        lNumericFieldBlanks.deallocate();

        if (ErrorsFound) {
            ShowFatalError("Errors found in getting Energy Management System input. Preceding condition causes termination.");
        }
    }

    void ProcessEMSInput(EnergyPlusData &state, bool const reportErrors) // .  If true, then report out errors ,otherwise setup what we can
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         B. Griffith
        //       DATE WRITTEN   May 2009
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // contains Some input checks that need to be deferred until later in the simulation

        // METHODOLOGY EMPLOYED:
        // Loop over objects doing input checks.
        // Had to break up get user input into two phases because
        // the actuators can't be set up until all the HVAC systems are read in, sized, etc.
        // but we also want to allow customizing sizing calcs which occur much earlier in the simulation.
        //  so here we do a final pass and throw the errors that would usually occur during get input.

        // Using/Aliasing
        using RuntimeLanguageProcessor::BeginEnvrnInitializeRuntimeLanguage;
        using ScheduleManager::GetScheduleIndex;

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int SensorNum; // local loop
        //  INTEGER :: VariableNum  ! local do loop index
        int VarIndex;
        int VarType;
        bool ErrorsFound(false);
        int ActuatorNum;
        bool FoundObjectType;
        bool FoundObjectName;
        bool FoundActuatorName;
        int ActuatorVariableNum;
        int InternVarNum;        // local do loop index
        int InternalVarAvailNum; // local do loop index
        std::string cCurrentModuleObject;

        cCurrentModuleObject = "EnergyManagementSystem:Sensor";
        for (SensorNum = 1; SensorNum <= NumSensors; ++SensorNum) {
            if (Sensor(SensorNum).CheckedOkay) continue;

            // try again to process sensor.
            VarIndex = GetMeterIndex(Sensor(SensorNum).OutputVarName);
            if (VarIndex > 0) {

                Sensor(SensorNum).Type = 3;
                Sensor(SensorNum).Index = VarIndex;

            } else {
                // Search for variable names
                GetVariableTypeAndIndex(state, Sensor(SensorNum).OutputVarName, Sensor(SensorNum).UniqueKeyName, VarType, VarIndex);
                if (VarType == 0) {
                    if (reportErrors) {
                        ShowSevereError("Invalid Output:Variable or Output:Meter Name =" + Sensor(SensorNum).OutputVarName);
                        ShowContinueError("Entered in " + cCurrentModuleObject + '=' + Sensor(SensorNum).Name);
                        ShowContinueError("Output:Variable Name not found");
                        ErrorsFound = true;
                    }
                } else if (VarIndex == 0) {
                    if (reportErrors) {
                        ShowSevereError("Invalid Output:Variable or Output:Meter Index Key Name =" + Sensor(SensorNum).UniqueKeyName);
                        ShowContinueError("For Output:Variable or Output:Meter = " + Sensor(SensorNum).OutputVarName);
                        ShowContinueError("Entered in " + cCurrentModuleObject + '=' + Sensor(SensorNum).Name);
                        ShowContinueError("Unique Key Name not found.");
                        ErrorsFound = true;
                    }
                } else {
                    Sensor(SensorNum).Type = VarType;
                    Sensor(SensorNum).Index = VarIndex;
                    Sensor(SensorNum).CheckedOkay = true;
                    // If variable is Schedule Value, then get the schedule id to register it as being used
                    if (UtilityRoutines::SameString(Sensor(SensorNum).OutputVarName, "Schedule Value")) {
                        Sensor(SensorNum).SchedNum = GetScheduleIndex(state, Sensor(SensorNum).UniqueKeyName);
                        if (Sensor(SensorNum).SchedNum == 0) {
                            Sensor(SensorNum).CheckedOkay = false;
                            if (reportErrors) {
                                ShowSevereError("Invalid Output:Variable or Output:Meter Index Key Name =" + Sensor(SensorNum).UniqueKeyName);
                                ShowContinueError("For Output:Variable or Output:Meter = " + Sensor(SensorNum).OutputVarName);
                                ShowContinueError("Entered in " + cCurrentModuleObject + '=' + Sensor(SensorNum).Name);
                                ShowContinueError("Schedule Name not found.");
                                ErrorsFound = true;
                            }
                        }
                    }
                }
            }

        } // SensorNum

        // added for FMU
        for (ActuatorNum = 1;
             ActuatorNum <= numActuatorsUsed + NumExternalInterfaceActuatorsUsed + NumExternalInterfaceFunctionalMockupUnitImportActuatorsUsed +
                                NumExternalInterfaceFunctionalMockupUnitExportActuatorsUsed;
             ++ActuatorNum) {
            // If we process the ExternalInterface actuators, all we need to do is to change the

            if (ActuatorNum <= numActuatorsUsed) {
                cCurrentModuleObject = "EnergyManagementSystem:Actuator";
            } else if (ActuatorNum > numActuatorsUsed && ActuatorNum <= numActuatorsUsed + NumExternalInterfaceActuatorsUsed) {
                cCurrentModuleObject = "ExternalInterface:Actuator";
            } else if (ActuatorNum > numActuatorsUsed + NumExternalInterfaceActuatorsUsed &&
                       ActuatorNum <=
                           numActuatorsUsed + NumExternalInterfaceActuatorsUsed + NumExternalInterfaceFunctionalMockupUnitImportActuatorsUsed) {
                cCurrentModuleObject = "ExternalInterface:FunctionalMockupUnitImport:To:Actuator";
            } else if (ActuatorNum >
                           numActuatorsUsed + NumExternalInterfaceActuatorsUsed + NumExternalInterfaceFunctionalMockupUnitImportActuatorsUsed &&
                       ActuatorNum <= numActuatorsUsed + NumExternalInterfaceActuatorsUsed +
                                          NumExternalInterfaceFunctionalMockupUnitImportActuatorsUsed +
                                          NumExternalInterfaceFunctionalMockupUnitExportActuatorsUsed) {
                cCurrentModuleObject = "ExternalInterface:FunctionalMockupUnitExport:To:Actuator";
            }

            if (EMSActuatorUsed(ActuatorNum).CheckedOkay) continue;
            FoundObjectType = false;
            FoundObjectName = false;
            FoundActuatorName = false;
            for (ActuatorVariableNum = 1; ActuatorVariableNum <= numEMSActuatorsAvailable; ++ActuatorVariableNum) {
                if (UtilityRoutines::SameString(EMSActuatorAvailable(ActuatorVariableNum).ComponentTypeName,
                                                EMSActuatorUsed(ActuatorNum).ComponentTypeName)) {
                    FoundObjectType = true;
                    if (UtilityRoutines::SameString(EMSActuatorAvailable(ActuatorVariableNum).UniqueIDName,
                                                    EMSActuatorUsed(ActuatorNum).UniqueIDName)) {
                        FoundObjectName = true;
                        if (UtilityRoutines::SameString(EMSActuatorAvailable(ActuatorVariableNum).ControlTypeName,
                                                        EMSActuatorUsed(ActuatorNum).ControlTypeName)) {
                            FoundActuatorName = true;
                            break;
                        }
                    }
                }
            }

            if (!FoundObjectType) {
                if (reportErrors) {
                    ShowSevereError("Invalid Actuated Component Type =" + EMSActuatorUsed(ActuatorNum).ComponentTypeName);
                    ShowContinueError("Entered in " + cCurrentModuleObject + '=' + EMSActuatorUsed(ActuatorNum).Name);
                    ShowContinueError("Component Type not found");
                    if (OutputEDDFile) {
                        ShowContinueError("Review .edd file for valid component types.");
                    } else {
                        ShowContinueError("Use Output:EnergyManagementSystem object to create .edd file for valid component types.");
                    }
                    ErrorsFound = true;
                }
            }

            if (!FoundObjectName) {
                if (reportErrors) {
                    ShowSevereError("Invalid Actuated Component Unique Name =" + EMSActuatorUsed(ActuatorNum).UniqueIDName);
                    ShowContinueError("Entered in " + cCurrentModuleObject + '=' + EMSActuatorUsed(ActuatorNum).Name);
                    ShowContinueError("Component Unique key name not found ");
                    if (OutputEDDFile) {
                        ShowContinueError("Review edd file for valid component names.");
                    } else {
                        ShowContinueError("Use Output:EnergyManagementSystem object to create .edd file for valid component names.");
                    }
                    ErrorsFound = true;
                }
            }

            if (!FoundActuatorName) {
                if (reportErrors) {
                    ShowSevereError("Invalid Actuated Component Control Type =" + EMSActuatorUsed(ActuatorNum).ControlTypeName);
                    ShowContinueError("Entered in " + cCurrentModuleObject + '=' + EMSActuatorUsed(ActuatorNum).Name);
                    ShowContinueError("Control Type not found");
                    if (OutputEDDFile) {
                        ShowContinueError("Review edd file for valid component control types.");
                    } else {
                        ShowContinueError("Use Output:EnergyManagementSystem object to create .edd file for valid component control types.");
                    }
                    ErrorsFound = true;
                }
            } else {
                EMSActuatorUsed(ActuatorNum).ActuatorVariableNum = ActuatorVariableNum;
                EMSActuatorUsed(ActuatorNum).CheckedOkay = true;
                int nHandle = EMSActuatorAvailable(ActuatorVariableNum).handleCount;
                if (nHandle > 0) {
                    EnergyPlus::ShowWarningError("Seems like you already tried to get a Handle on this Actuator " + std::to_string(nHandle) + "times.");
                    EnergyPlus::ShowContinueError("Occurred for componentType='" +  EMSActuatorUsed(ActuatorNum).ComponentTypeName
                            + "', controlType='" + EMSActuatorUsed(ActuatorNum).ControlTypeName
                            + "', uniqueKey='" + EMSActuatorUsed(ActuatorNum).UniqueIDName + "'.");
                    EnergyPlus::ShowContinueError("You should take note that there is a risk of overwritting.");
                }
                ++EMSActuatorAvailable(ActuatorVariableNum).handleCount;

                // Warn if actuator applied to an air boundary surface
                if (UtilityRoutines::SameString(EMSActuatorUsed(ActuatorNum).ComponentTypeName, "AIRFLOW NETWORK WINDOW/DOOR OPENING")) {
                    int actuatedSurfNum = UtilityRoutines::FindItemInList(EMSActuatorUsed(ActuatorNum).UniqueIDName, DataSurfaces::Surface);
                    if (actuatedSurfNum > 0) {
                        if (DataSurfaces::Surface(actuatedSurfNum).IsAirBoundarySurf) {
                            ShowWarningError(
                                "GetEMSInput: EnergyManagementSystem:Actuator=" + EMSActuatorUsed(ActuatorNum).Name +
                                " actuates an opening attached to an air boundary surface.");
                        }
                    }
                }
            }
        } // ActuatorNum

        cCurrentModuleObject = "EnergyManagementSystem:InternalVariable";
        for (InternVarNum = 1; InternVarNum <= NumInternalVariablesUsed; ++InternVarNum) {
            if (EMSInternalVarsUsed(InternVarNum).CheckedOkay) continue;
            FoundObjectType = false;
            FoundObjectName = false;
            for (InternalVarAvailNum = 1; InternalVarAvailNum <= numEMSInternalVarsAvailable; ++InternalVarAvailNum) {
                if (UtilityRoutines::SameString(EMSInternalVarsAvailable(InternalVarAvailNum).DataTypeName,
                                                EMSInternalVarsUsed(InternVarNum).InternalDataTypeName)) {
                    FoundObjectType = true;
                    if (UtilityRoutines::SameString(EMSInternalVarsAvailable(InternalVarAvailNum).UniqueIDName,
                                                    EMSInternalVarsUsed(InternVarNum).UniqueIDName)) {
                        FoundObjectName = true;
                        break; // InternalVarAvailNum now holds needed index pointer
                    }
                }
            }

            if (!FoundObjectType) {
                if (reportErrors) {
                    ShowSevereError("Invalid Internal Data Type =" + EMSInternalVarsUsed(InternVarNum).InternalDataTypeName);
                    ShowContinueError("Entered in " + cCurrentModuleObject + '=' + EMSInternalVarsUsed(InternVarNum).Name);
                    ShowContinueError("Internal data type name not found");
                    ErrorsFound = true;
                }
            }

            if (!FoundObjectName) {
                if (reportErrors) {
                    ShowSevereError("Invalid Internal Data Index Key Name =" + EMSInternalVarsUsed(InternVarNum).UniqueIDName);
                    ShowContinueError("Entered in " + cCurrentModuleObject + '=' + EMSInternalVarsUsed(InternVarNum).Name);
                    ShowContinueError("Internal data unique identifier not found");
                    ErrorsFound = true;
                }
            } else {
                EMSInternalVarsUsed(InternVarNum).InternVarNum = InternalVarAvailNum;
                EMSInternalVarsUsed(InternVarNum).CheckedOkay = true;
            }
        }
        if (reportErrors) {
            EchoOutActuatorKeyChoices(state);
            EchoOutInternalVariableChoices(state);
        }

        if (ErrorsFound) {
            ShowFatalError("Errors found in processing Energy Management System input. Preceding condition causes termination.");
        }

        if (reportErrors) {
            BeginEnvrnInitializeRuntimeLanguage();
        }
    }

    void GetVariableTypeAndIndex(EnergyPlusData &state, std::string const &VarName, std::string const &VarKeyName, int &VarType, int &VarIndex)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Peter Graham Ellis
        //       DATE WRITTEN   June 2006
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // local helper routine intended to lookup report variables only.
        //    Use GetMeterIndex for meters.

        // METHODOLOGY EMPLOYED:
        // make calls to OutputProcessor methods GetVariableKeyCountandType and GetVariableKeys

        // USE STATEMENTS:

        // Using/Aliasing
        using RuntimeLanguageProcessor::EvaluateStack;

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int NumKeys;
        int KeyNum;
        OutputProcessor::StoreType AvgOrSum;
        OutputProcessor::TimeStepType StepType;
        OutputProcessor::Unit Units(OutputProcessor::Unit::None);
        Array1D_string KeyName;
        Array1D_int KeyIndex;
        bool Found;

        // FLOW:
        VarType = 0;
        VarIndex = 0;
        Found = false;
        GetVariableKeyCountandType(state, VarName, NumKeys, VarType, AvgOrSum, StepType, Units);

        // note that schedules are not getting VarType set right...

        if (NumKeys > 0) {
            KeyName.allocate(NumKeys);
            KeyIndex.allocate(NumKeys);
            GetVariableKeys(state, VarName, VarType, KeyName, KeyIndex);

            if (KeyName(1) == "ENVIRONMENT") {
                VarIndex = KeyIndex(1);
            } else {
                for (KeyNum = 1; KeyNum <= NumKeys; ++KeyNum) {
                    if (KeyName(KeyNum) == VarKeyName) {
                        Found = true;
                        break;
                    }
                }
                if (Found) VarIndex = KeyIndex(KeyNum);
            }

            KeyName.deallocate();
            KeyIndex.deallocate();
        }
    }

    void EchoOutActuatorKeyChoices(EnergyPlusData &state)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Brent Griffith
        //       DATE WRITTEN   April 2009
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // echo out actuators registered with SetupEMSActuator for user access

        // METHODOLOGY EMPLOYED:
        // mine structure and write to edd file
        // note this executes after final processing and sizing-related calling points may already execute Erl programs

        // SUBROUTINE PARAMETER DEFINITIONS:

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:

        if (OutputEMSActuatorAvailFull) {

            print(state.files.edd, "! <EnergyManagementSystem:Actuator Available>, Component Unique Name, Component Type,  Control Type, Units\n");
            for (int ActuatorLoop = 1; ActuatorLoop <= numEMSActuatorsAvailable; ++ActuatorLoop) {
                print(state.files.edd,
                      "EnergyManagementSystem:Actuator Available,{},{},{},{}\n",
                      EMSActuatorAvailable(ActuatorLoop).UniqueIDName,
                      EMSActuatorAvailable(ActuatorLoop).ComponentTypeName,
                      EMSActuatorAvailable(ActuatorLoop).ControlTypeName,
                      EMSActuatorAvailable(ActuatorLoop).Units);
            }
        } else if (OutputEMSActuatorAvailSmall) {
            print(state.files.edd, "! <EnergyManagementSystem:Actuator Available>, *, Component Type, Control Type, Units\n");
            int FoundTypeName;
            int FoundControlType;
            for (int ActuatorLoop = 1; ActuatorLoop <= numEMSActuatorsAvailable; ++ActuatorLoop) {
                if (ActuatorLoop + 1 <= numEMSActuatorsAvailable) {
                    FoundTypeName = UtilityRoutines::FindItemInList(EMSActuatorAvailable(ActuatorLoop).ComponentTypeName,
                                                                    EMSActuatorAvailable({ActuatorLoop + 1, numEMSActuatorsAvailable}),
                                                                    &EMSActuatorAvailableType::ComponentTypeName,
                                                                    numEMSActuatorsAvailable - (ActuatorLoop + 1));
                    FoundControlType = UtilityRoutines::FindItemInList(EMSActuatorAvailable(ActuatorLoop).ControlTypeName,
                                                                       EMSActuatorAvailable({ActuatorLoop + 1, numEMSActuatorsAvailable}),
                                                                       &EMSActuatorAvailableType::ControlTypeName,
                                                                       numEMSActuatorsAvailable - (ActuatorLoop + 1));
                } else {
                    FoundTypeName = 1;
                    FoundControlType = 1;
                }
                if ((FoundTypeName == 0) || (FoundControlType == 0)) {
                    print(state.files.edd,
                          "EnergyManagementSystem:Actuator Available, *,{},{},{}\n",
                          EMSActuatorAvailable(ActuatorLoop).ComponentTypeName,
                          EMSActuatorAvailable(ActuatorLoop).ControlTypeName,
                          EMSActuatorAvailable(ActuatorLoop).Units);
                }
            }
        }
    }

    void EchoOutInternalVariableChoices(EnergyPlusData &state)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Brent Griffith
        //       DATE WRITTEN   April 2009
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // echo out actuators registered with SetupEMSActuator for user access

        // METHODOLOGY EMPLOYED:
        // mine structure and write to eio file

        // SUBROUTINE PARAMETER DEFINITIONS:

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:

        if (OutputEMSInternalVarsFull) {

            print(state.files.edd, "! <EnergyManagementSystem:InternalVariable Available>, Unique Name, Internal Data Type, Units \n");
            for (int InternalDataLoop = 1; InternalDataLoop <= numEMSInternalVarsAvailable; ++InternalDataLoop) {
                print(state.files.edd,
                      "EnergyManagementSystem:InternalVariable Available,{},{},{}\n",
                      EMSInternalVarsAvailable(InternalDataLoop).UniqueIDName,
                      EMSInternalVarsAvailable(InternalDataLoop).DataTypeName,
                      EMSInternalVarsAvailable(InternalDataLoop).Units);
            }

        } else if (OutputEMSInternalVarsSmall) {
            print(state.files.edd, "! <EnergyManagementSystem:InternalVariable Available>, *, Internal Data Type\n");
            for (int InternalDataLoop = 1; InternalDataLoop <= numEMSInternalVarsAvailable; ++InternalDataLoop) {
                int Found(0);
                if (InternalDataLoop + 1 <= numEMSInternalVarsAvailable) {
                    Found = UtilityRoutines::FindItemInList(EMSInternalVarsAvailable(InternalDataLoop).DataTypeName,
                                                            EMSInternalVarsAvailable({InternalDataLoop + 1, numEMSInternalVarsAvailable}),
                                                            &InternalVarsAvailableType::DataTypeName,
                                                            numEMSInternalVarsAvailable - (InternalDataLoop + 1));
                }
                if (Found == 0) {
                    print(state.files.edd,
                          "EnergyManagementSystem:InternalVariable Available, *,{},{}\n",
                          EMSInternalVarsAvailable(InternalDataLoop).DataTypeName,
                          EMSInternalVarsAvailable(InternalDataLoop).Units);
                }
            }
        }
    }

    void SetupNodeSetPointsAsActuators()
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Brent Griffith
        //       DATE WRITTEN   May 2009
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // make system nodes in model available for EMS control

        // METHODOLOGY EMPLOYED:
        // Loop over node structures and make calls to SetupEMSActuator
        // the pattern for the basic node setpoints is a little different in that the actuators directly
        // affect the node variables, rather than using seperate logical override flag and ems values

        // REFERENCES:
        // na

        // Using/Aliasing
        using DataLoopNode::Node;
        using DataLoopNode::NodeID;
        using DataLoopNode::NumOfNodes;
        using OutAirNodeManager::NumOutsideAirNodes;
        using OutAirNodeManager::OutsideAirNodeList;

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:
        // na

        // SUBROUTINE PARAMETER DEFINITIONS:
        // na

        // INTERFACE BLOCK SPECIFICATIONS:
        // na

        // DERIVED TYPE DEFINITIONS:
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int LoopNode;       // local do loop index
        static bool lDummy; // not going to setup a pointer to logical control //Fix Changed to static: Passed to SetupEMSActuator as source of
                            // persistent Reference
        // (could this ever cause a fault?) // It caused illegal memory access/corruption
        // make it optional in Setup call?
        int OutsideAirNodeNum; // local do loop index
        int NodeNum;           // local index.

        lDummy = false;

        if (NumOfNodes > 0) {

            for (LoopNode = 1; LoopNode <= NumOfNodes; ++LoopNode) {
                // setup the setpoint for each type of variable that can be controlled
                SetupEMSActuator("System Node Setpoint", NodeID(LoopNode), "Temperature Setpoint", "[C]", lDummy, Node(LoopNode).TempSetPoint);
                SetupEMSActuator(
                    "System Node Setpoint", NodeID(LoopNode), "Temperature Minimum Setpoint", "[C]", lDummy, Node(LoopNode).TempSetPointLo);
                SetupEMSActuator(
                    "System Node Setpoint", NodeID(LoopNode), "Temperature Maximum Setpoint", "[C]", lDummy, Node(LoopNode).TempSetPointHi);
                SetupEMSActuator(
                    "System Node Setpoint", NodeID(LoopNode), "Humidity Ratio Setpoint", "[kgWater/kgDryAir]", lDummy, Node(LoopNode).HumRatSetPoint);
                SetupEMSActuator("System Node Setpoint",
                                 NodeID(LoopNode),
                                 "Humidity Ratio Maximum Setpoint",
                                 "[kgWater/kgDryAir]",
                                 lDummy,
                                 Node(LoopNode).HumRatMax);
                SetupEMSActuator("System Node Setpoint",
                                 NodeID(LoopNode),
                                 "Humidity Ratio Minimum Setpoint",
                                 "[kgWater/kgDryAir]",
                                 lDummy,
                                 Node(LoopNode).HumRatMin);
                SetupEMSActuator(
                    "System Node Setpoint", NodeID(LoopNode), "Mass Flow Rate Setpoint", "[kg/s]", lDummy, Node(LoopNode).MassFlowRateSetPoint);
                SetupEMSActuator("System Node Setpoint",
                                 NodeID(LoopNode),
                                 "Mass Flow Rate Maximum Available Setpoint",
                                 "[kg/s]",
                                 lDummy,
                                 Node(LoopNode).MassFlowRateMaxAvail);
                SetupEMSActuator("System Node Setpoint",
                                 NodeID(LoopNode),
                                 "Mass Flow Rate Minimum Available Setpoint",
                                 "[kg/s]",
                                 lDummy,
                                 Node(LoopNode).MassFlowRateMinAvail);
            }

        } // NumOfNodes > 0

        if (NumOutsideAirNodes > 0) {
            for (OutsideAirNodeNum = 1; OutsideAirNodeNum <= NumOutsideAirNodes; ++OutsideAirNodeNum) {
                NodeNum = OutsideAirNodeList(OutsideAirNodeNum);
                SetupEMSActuator("Outdoor Air System Node",
                                 NodeID(NodeNum),
                                 "Drybulb Temperature",
                                 "[C]",
                                 Node(NodeNum).EMSOverrideOutAirDryBulb,
                                 Node(NodeNum).EMSValueForOutAirDryBulb);
                SetupEMSActuator("Outdoor Air System Node",
                                 NodeID(NodeNum),
                                 "Wetbulb Temperature",
                                 "[C]",
                                 Node(NodeNum).EMSOverrideOutAirWetBulb,
                                 Node(NodeNum).EMSValueForOutAirWetBulb);
                SetupEMSActuator("Outdoor Air System Node",
                                 NodeID(NodeNum),
                                 "Wind Speed",
                                 "[m/s]",
                                 Node(NodeNum).EMSOverrideOutAirWindSpeed,
                                 Node(NodeNum).EMSValueForOutAirWindSpeed);
                SetupEMSActuator("Outdoor Air System Node",
                                 NodeID(NodeNum),
                                 "Wind Direction",
                                 "[degree]",
                                 Node(NodeNum).EMSOverrideOutAirWindDir,
                                 Node(NodeNum).EMSValueForOutAirWindDir);
                for (int ActuatorUsedLoop = 1; ActuatorUsedLoop <= numActuatorsUsed; ActuatorUsedLoop++) {
                    if (UtilityRoutines::SameString(EMSActuatorUsed(ActuatorUsedLoop).ComponentTypeName, "Outdoor Air System Node") &&
                        UtilityRoutines::SameString(EMSActuatorUsed(ActuatorUsedLoop).UniqueIDName,NodeID(NodeNum))) {
                        Node(NodeNum).IsLocalNode = true;
                        break;
                    }
                }
            }
        }
    }

    void UpdateEMSTrendVariables()
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Brent Griffith
        //       DATE WRITTEN   May 2009
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Logged trend data

        // METHODOLOGY EMPLOYED:
        // Store current value of Erl Variable in Trend stack
        // Trend arrays are pushed so that the latest value is
        //  always at index 1.  old values get lost.

        // REFERENCES:
        // na

        // Using/Aliasing
        using DataGlobals::AnyEnergyManagementSystemInModel;

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:
        // na

        // SUBROUTINE PARAMETER DEFINITIONS:
        // na

        // INTERFACE BLOCK SPECIFICATIONS:
        // na

        // DERIVED TYPE DEFINITIONS:
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        // na
        static int TrendNum(0); // local loop counter
        static int ErlVarNum(0);
        static int TrendDepth(0);
        static Real64 currentVal(0.0);

        // checks with quick return if no updates needed.
        if (!AnyEnergyManagementSystemInModel) return;
        if (NumErlTrendVariables == 0) return;

        for (TrendNum = 1; TrendNum <= NumErlTrendVariables; ++TrendNum) {
            ErlVarNum = TrendVariable(TrendNum).ErlVariablePointer;
            TrendDepth = TrendVariable(TrendNum).LogDepth;
            if ((ErlVarNum > 0) && (TrendDepth > 0)) {
                currentVal = ErlVariable(ErlVarNum).Value.Number;
                // push into trend
                TrendVariable(TrendNum).tempTrendARR = TrendVariable(TrendNum).TrendValARR;
                TrendVariable(TrendNum).TrendValARR(1) = currentVal;
                TrendVariable(TrendNum).TrendValARR({2, TrendDepth}) = TrendVariable(TrendNum).tempTrendARR({1, TrendDepth - 1});
            }
        }
    }

    std::string controlTypeName(int const SetPointType) {
        std::string cControlTypeName;

        auto const SELECT_CASE_var(SetPointType);

        if (SELECT_CASE_var == iTemperatureSetPoint) {
            cControlTypeName = "Temperature Setpoint";
        } else if (SELECT_CASE_var == iTemperatureMinSetPoint) {
            cControlTypeName = "Temperature Minimum Setpoint";
        } else if (SELECT_CASE_var == iTemperatureMaxSetPoint) {
            cControlTypeName = "Temperature Maximum Setpoint";
        } else if (SELECT_CASE_var == iHumidityRatioSetPoint) {
            cControlTypeName = "Humidity Ratio Setpoint";
        } else if (SELECT_CASE_var == iHumidityRatioMinSetPoint) {
            cControlTypeName = "Humidity Ratio Minimum Setpoint";
        } else if (SELECT_CASE_var == iHumidityRatioMaxSetPoint) {
            cControlTypeName = "Humidity Ratio Maximum Setpoint";
        } else if (SELECT_CASE_var == iMassFlowRateSetPoint) {
            cControlTypeName = "Mass Flow Rate Setpoint";
        } else if (SELECT_CASE_var == iMassFlowRateMinSetPoint) {
            cControlTypeName = "Mass Flow Rate Minimum Available Setpoint";
        } else if (SELECT_CASE_var == iMassFlowRateMaxSetPoint) {
            cControlTypeName = "Mass Flow Rate Maximum Available Setpoint";
        }

        return cControlTypeName;

    }

    bool CheckIfNodeSetPointManaged(int const NodeNum, int const SetPointType, bool byHandle) {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Brent Griffith
        //       DATE WRITTEN   May 2009
        //       MODIFIED       July 2020, Julien Marrec of EffiBEM: added option to check by handle (for API)
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Provide method to verify that a specific node is (probably) managed by EMS

        // Using/Aliasing
        using DataLoopNode::NodeID;

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        bool FoundControl(false);

        std::string cNodeName = NodeID(NodeNum);
        std::string cComponentTypeName = "System Node Setpoint";
        std::string cControlTypeName = controlTypeName(SetPointType);


        if (byHandle) {
            for (int Loop = 1; Loop <= numEMSActuatorsAvailable; ++Loop) {
                if ((EMSActuatorAvailable(Loop).handleCount > 0) &&
                    (UtilityRoutines::SameString(EMSActuatorAvailable(Loop).ComponentTypeName, cComponentTypeName)) &&
                    (UtilityRoutines::SameString(EMSActuatorAvailable(Loop).UniqueIDName, cNodeName)) &&
                    (UtilityRoutines::SameString(EMSActuatorAvailable(Loop).ControlTypeName, cControlTypeName)))
                {
                    FoundControl = true;
                    break;
                }
            }
            if (!FoundControl) {
                ShowWarningError("Missing '" + controlTypeName(SetPointType) + "' for node named named '" + NodeID(NodeNum) + "'.");
            }
        } else {
            for (int Loop = 1; Loop <= numActuatorsUsed + NumExternalInterfaceActuatorsUsed; ++Loop) {
                if ((UtilityRoutines::SameString(EMSActuatorUsed(Loop).ComponentTypeName, cComponentTypeName)) &&
                        (UtilityRoutines::SameString(EMSActuatorUsed(Loop).UniqueIDName, cNodeName)) &&
                        (UtilityRoutines::SameString(EMSActuatorUsed(Loop).ControlTypeName, cControlTypeName))) {
                    FoundControl = true;
                    break;
                }
            }
        }

        return FoundControl;
    }

    bool CheckIfNodeSetPointManagedByEMS(int const NodeNum, // index of node being checked.
                                         int const SetPointType,
                                         bool &ErrorFlag)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Brent Griffith
        //       DATE WRITTEN   May 2009
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Provide method to verify that a specific node is (probably) managed by EMS

        // Using/Aliasing
        using DataLoopNode::NodeID;

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        std::string cControlTypeName;
        std::string cComponentTypeName;
        std::string cNodeName;
        bool FoundControl = CheckIfNodeSetPointManaged(NodeNum, SetPointType, false);

        if ((!ErrorFlag) && (!FoundControl)) {
            int numPythonPlugins = inputProcessor->getNumObjectsFound("PythonPlugin:Instance");
            int numActiveCallbacks = PluginManagement::PluginManager::numActiveCallbacks(); // EnergyPlus::DataGlobals::eplusRunningViaAPI;
            if ((numPythonPlugins + numActiveCallbacks) == 0) {
                ErrorFlag = true;
            } else {
                // We'll defer to checking at the end whether a Plugin / API called getActuatorHandle on it
                auto &nodeSetpointCheck = DataLoopNode::NodeSetpointCheck(NodeNum);
                nodeSetpointCheck.needsSetpointChecking = true;

                auto const SELECT_CASE_var(SetPointType);
                if (SELECT_CASE_var == iTemperatureSetPoint) {
                    nodeSetpointCheck.checkTemperatureSetPoint = true;
                } else if (SELECT_CASE_var == iTemperatureMinSetPoint) {
                    nodeSetpointCheck.checkTemperatureMinSetPoint = true;
                } else if (SELECT_CASE_var == iTemperatureMaxSetPoint) {
                    nodeSetpointCheck.checkTemperatureMaxSetPoint = true;
                } else if (SELECT_CASE_var == iHumidityRatioSetPoint) {
                    nodeSetpointCheck.checkHumidityRatioSetPoint = true;
                } else if (SELECT_CASE_var == iHumidityRatioMinSetPoint) {
                    nodeSetpointCheck.checkHumidityRatioMinSetPoint = true;
                } else if (SELECT_CASE_var == iHumidityRatioMaxSetPoint) {
                    nodeSetpointCheck.checkHumidityRatioMaxSetPoint = true;
                } else if (SELECT_CASE_var == iMassFlowRateSetPoint) {
                    nodeSetpointCheck.checkMassFlowRateSetPoint = true;
                } else if (SELECT_CASE_var == iMassFlowRateMinSetPoint) {
                    nodeSetpointCheck.checkMassFlowRateMinSetPoint = true;
                } else if (SELECT_CASE_var == iMassFlowRateMaxSetPoint) {
                    nodeSetpointCheck.checkMassFlowRateMaxSetPoint = true;
                }

            }
        }

        return FoundControl;
    }

    void checkSetpointNodesAtEnd()
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Julien Marrec of EffiBEM
        //       DATE WRITTEN   July 2020
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine checks any nodes where we couldn't find a Setpoint in EMS, after the PythonPlugin / API have been called
        // so we can check if getActuatorHandle was ever actually called for that node.

        bool FatalErrorFlag = false;

        for (int NodeNum = 1; NodeNum <= DataLoopNode::NumOfNodes; ++NodeNum) {
            auto & nodeSetpointCheck = DataLoopNode::NodeSetpointCheck(NodeNum);

            if (nodeSetpointCheck.needsSetpointChecking) {
                // Start by setting it to false (assume matched)
                nodeSetpointCheck.needsSetpointChecking = false;

                if (nodeSetpointCheck.checkTemperatureSetPoint) {
                    nodeSetpointCheck.needsSetpointChecking |= !CheckIfNodeSetPointManaged(NodeNum, EMSManager::iTemperatureSetPoint, true);
                }
                if (nodeSetpointCheck.checkTemperatureMinSetPoint) {
                    nodeSetpointCheck.needsSetpointChecking |= !CheckIfNodeSetPointManaged(NodeNum, EMSManager::iTemperatureMinSetPoint, true);
                }
                if (nodeSetpointCheck.checkTemperatureMaxSetPoint) {
                    nodeSetpointCheck.needsSetpointChecking |= !CheckIfNodeSetPointManaged(NodeNum, EMSManager::iTemperatureMaxSetPoint, true);
                }
                if (nodeSetpointCheck.checkHumidityRatioSetPoint) {
                    nodeSetpointCheck.needsSetpointChecking |= !CheckIfNodeSetPointManaged(NodeNum, EMSManager::iHumidityRatioSetPoint, true);
                }
                if (nodeSetpointCheck.checkHumidityRatioMinSetPoint) {
                    nodeSetpointCheck.needsSetpointChecking |= !CheckIfNodeSetPointManaged(NodeNum, EMSManager::iHumidityRatioMinSetPoint, true);
                }
                if (nodeSetpointCheck.checkHumidityRatioMaxSetPoint) {
                    nodeSetpointCheck.needsSetpointChecking |= !CheckIfNodeSetPointManaged(NodeNum, EMSManager::iHumidityRatioMaxSetPoint, true);
                }
                if (nodeSetpointCheck.checkMassFlowRateSetPoint) {
                    nodeSetpointCheck.needsSetpointChecking |= !CheckIfNodeSetPointManaged(NodeNum, EMSManager::iMassFlowRateSetPoint, true);
                }
                if (nodeSetpointCheck.checkMassFlowRateMinSetPoint) {
                    nodeSetpointCheck.needsSetpointChecking |= !CheckIfNodeSetPointManaged(NodeNum, EMSManager::iMassFlowRateMinSetPoint, true);
                }
                if (nodeSetpointCheck.checkMassFlowRateMaxSetPoint) {
                    nodeSetpointCheck.needsSetpointChecking |= !CheckIfNodeSetPointManaged(NodeNum, EMSManager::iMassFlowRateMaxSetPoint, true);
                }

                if (nodeSetpointCheck.needsSetpointChecking) {
                    FatalErrorFlag = true;
                }
            }
        }

        if (FatalErrorFlag) {
            ShowFatalError("checkSetpointNodesAtEnd: At least one node does not have a setpoint attached, "
                           "neither via a SetpointManager, EMS:Actuator, or API");
        }
    }


    bool CheckIfNodeMoreInfoSensedByEMS(int const nodeNum, // index of node being checked.
                                        std::string const &varName)
    {
        bool returnValue;

        returnValue = false;
        for (auto loop = 1; loop <= NumSensors; ++loop) {
            if (Sensor(loop).UniqueKeyName == DataLoopNode::NodeID(nodeNum) && UtilityRoutines::SameString(Sensor(loop).OutputVarName, varName)) {
                returnValue = true;
            }
        }

        return returnValue;
    }

    void SetupPrimaryAirSystemAvailMgrAsActuators(EnergyPlusData &state)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Brent Griffith
        //       DATE WRITTEN   May 2009
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // make air system status available as EMS actuator

        // METHODOLOGY EMPLOYED:
        // <description>

        // REFERENCES:
        // na

        // Using/Aliasing
        using DataAirSystems::PrimaryAirSystem;

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:
        // na

        // SUBROUTINE PARAMETER DEFINITIONS:
        // na

        // INTERFACE BLOCK SPECIFICATIONS:
        // na

        // DERIVED TYPE DEFINITIONS:
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        static int numAirLoops(0);
        static int Loop(0);
        static bool lDummy; // Fix Changed to static: Passed to SetupEMSActuator as source of persistent Reference

        lDummy = false;

        if (allocated(state.dataAirLoop->PriAirSysAvailMgr)) {
            numAirLoops = isize(state.dataAirLoop->PriAirSysAvailMgr);
            for (Loop = 1; Loop <= numAirLoops; ++Loop) {
                SetupEMSActuator(
                    "AirLoopHVAC", PrimaryAirSystem(Loop).Name, "Availability Status", "[ ]", lDummy, state.dataAirLoop->PriAirSysAvailMgr(Loop).AvailStatus);
            }

        } else {
        }
    }

    void SetupWindowShadingControlActuators(EnergyPlusData &state)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Brent Griffith
        //       DATE WRITTEN   May 2009
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // make calls to SetupEMSactuator for public data for Window Shades

        // METHODOLOGY EMPLOYED:
        // Loop thru SurfaceWindow and register any shading controls

        // REFERENCES:
        // na

        // Using/Aliasing
        using DataSurfaces::ExternalEnvironment;
        using DataSurfaces::Surface;
        using DataSurfaces::SurfaceClass_Window;
        using DataSurfaces::TotSurfaces;
        using DataSurfaces::WindowShadingControl;
        using DataSurfaces::WSC_ST_SwitchableGlazing;
        using DataSurfaces::WSC_ST_ExteriorScreen;

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:
        // na

        // SUBROUTINE PARAMETER DEFINITIONS:
        // na

        // INTERFACE BLOCK SPECIFICATIONS:
        // na

        // DERIVED TYPE DEFINITIONS:
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        static int loopSurfNum(0); // local do loop index

        for (loopSurfNum = 1; loopSurfNum <= TotSurfaces; ++loopSurfNum) {

            if (Surface(loopSurfNum).Class != SurfaceClass_Window) continue;
            if (Surface(loopSurfNum).ExtBoundCond != ExternalEnvironment) continue;
            if (!Surface(loopSurfNum).HasShadeControl) continue;

            if (DataSurfaces::SurfWinHasShadeOrBlindLayer(loopSurfNum)) {
                SetupEMSActuator("Window Shading Control",
                                 Surface(loopSurfNum).Name,
                                 "Control Status",
                                 "[ShadeStatus]",
                                 DataSurfaces::SurfWinShadingFlagEMSOn(loopSurfNum),
                                 DataSurfaces::SurfWinShadingFlagEMSValue(loopSurfNum));
                if (DataSurfaces::SurfWinMovableSlats(loopSurfNum)) {
                    SetupEMSActuator("Window Shading Control",
                                     Surface(loopSurfNum).Name,
                                     "Slat Angle",
                                     "[degrees]",
                                     DataSurfaces::SurfWinSlatAngThisTSDegEMSon(loopSurfNum),
                                     DataSurfaces::SurfWinSlatAngThisTSDegEMSValue(loopSurfNum));
                }
            } else if (WindowShadingControl(Surface(loopSurfNum).activeWindowShadingControl).ShadingType == WSC_ST_ExteriorScreen) {
                SetupEMSActuator("Window Shading Control",
                                 Surface(loopSurfNum).Name,
                                 "Control Status",
                                 "[ShadeStatus]",
                                 DataSurfaces::SurfWinShadingFlagEMSOn(loopSurfNum),
                                 DataSurfaces::SurfWinShadingFlagEMSValue(loopSurfNum));
            } else {
                if (WindowShadingControl(Surface(loopSurfNum).activeWindowShadingControl).ShadingType != WSC_ST_SwitchableGlazing) {
                    ShowSevereError("Missing shade or blind layer in window construction name = '" +
                                    state.dataConstruction->Construct(Surface(loopSurfNum).activeShadedConstruction).Name + "', surface name = '" +
                                    Surface(loopSurfNum).Name + "'.");
                    ShowContinueError("...'Control Status' or 'Slat Angle' EMS Actuator cannot be set for a construction that does not have a shade "
                                      "or a blind layer.");
                    ShowContinueError("...Add shade or blind layer to this construction in order to be able to apply EMS Actuator.");
                }
            }
        }
    }

    void SetupThermostatActuators()
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Brent Griffith
        //       DATE WRITTEN   May 2009
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Make zone thermostats, humidistats, and comfort controls available to EMS

        // METHODOLOGY EMPLOYED:
        // Loop over structures and call SetupEMSactuator for public data in DataZoneControls.

        // REFERENCES:
        // na

        // Using/Aliasing
        using namespace DataZoneControls;

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:
        // na

        // SUBROUTINE PARAMETER DEFINITIONS:
        // na

        // INTERFACE BLOCK SPECIFICATIONS:
        // na

        // DERIVED TYPE DEFINITIONS:
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        static int Loop(0); // local do loop index

        for (Loop = 1; Loop <= NumTempControlledZones; ++Loop) {
            SetupEMSActuator("Zone Temperature Control",
                             TempControlledZone(Loop).ZoneName,
                             "Heating Setpoint",
                             "[C]",
                             TempControlledZone(Loop).EMSOverrideHeatingSetPointOn,
                             TempControlledZone(Loop).EMSOverrideHeatingSetPointValue);
            SetupEMSActuator("Zone Temperature Control",
                             TempControlledZone(Loop).ZoneName,
                             "Cooling Setpoint",
                             "[C]",
                             TempControlledZone(Loop).EMSOverrideCoolingSetPointOn,
                             TempControlledZone(Loop).EMSOverrideCoolingSetPointValue);
        }

        for (Loop = 1; Loop <= NumHumidityControlZones; ++Loop) {
            SetupEMSActuator("Zone Humidity Control",
                             HumidityControlZone(Loop).ZoneName,
                             "Relative Humidity Humidifying Setpoint",
                             "[%]",
                             HumidityControlZone(Loop).EMSOverrideHumidifySetPointOn,
                             HumidityControlZone(Loop).EMSOverrideHumidifySetPointValue);
            SetupEMSActuator("Zone Humidity Control",
                             HumidityControlZone(Loop).ZoneName,
                             "Relative Humidity Dehumidifying Setpoint",
                             "[%]",
                             HumidityControlZone(Loop).EMSOverrideDehumidifySetPointOn,
                             HumidityControlZone(Loop).EMSOverrideDehumidifySetPointValue);
        }

        for (Loop = 1; Loop <= NumComfortControlledZones; ++Loop) {
            SetupEMSActuator("Zone Comfort Control",
                             ComfortControlledZone(Loop).ZoneName,
                             "Heating Setpoint",
                             "[]",
                             ComfortControlledZone(Loop).EMSOverrideHeatingSetPointOn,
                             ComfortControlledZone(Loop).EMSOverrideHeatingSetPointValue);
            SetupEMSActuator("Zone Comfort Control",
                             ComfortControlledZone(Loop).ZoneName,
                             "Cooling Setpoint",
                             "[]",
                             ComfortControlledZone(Loop).EMSOverrideCoolingSetPointOn,
                             ComfortControlledZone(Loop).EMSOverrideCoolingSetPointValue);
        }
    }

    void SetupSurfaceConvectionActuators()
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Brent Griffith
        //       DATE WRITTEN   May 2009
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Setup EMS actuators available for surface convection coefficients

        // METHODOLOGY EMPLOYED:
        // access public data and loop over it.

        // REFERENCES:
        // na

        // Using/Aliasing
        using DataSurfaces::Surface;
        using DataSurfaces::TotSurfaces;

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:
        // na

        // SUBROUTINE PARAMETER DEFINITIONS:
        // na

        // INTERFACE BLOCK SPECIFICATIONS:
        // na

        // DERIVED TYPE DEFINITIONS:
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int SurfNum; // local loop index.

        for (SurfNum = 1; SurfNum <= TotSurfaces; ++SurfNum) {
            SetupEMSActuator("Surface",
                             Surface(SurfNum).Name,
                             "Interior Surface Convection Heat Transfer Coefficient",
                             "[W/m2-K]",
                             Surface(SurfNum).EMSOverrideIntConvCoef,
                             Surface(SurfNum).EMSValueForIntConvCoef);
            SetupEMSActuator("Surface",
                             Surface(SurfNum).Name,
                             "Exterior Surface Convection Heat Transfer Coefficient",
                             "[W/m2-K]",
                             Surface(SurfNum).EMSOverrideExtConvCoef,
                             Surface(SurfNum).EMSValueForExtConvCoef);
        }
    }

    void SetupSurfaceConstructionActuators()
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         B. Griffith
        //       DATE WRITTEN   Jan 2012
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // setup EMS actuators available for surface construction

        // METHODOLOGY EMPLOYED:
        // <description>

        // REFERENCES:
        // na

        // Using/Aliasing
        using DataHeatBalance::TotConstructs;
        using DataSurfaces::Surface;
        using DataSurfaces::TotSurfaces;

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:
        // na

        // SUBROUTINE PARAMETER DEFINITIONS:
        // na

        // INTERFACE BLOCK SPECIFICATIONS:
        // na

        // DERIVED TYPE DEFINITIONS:
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int SurfNum; // local loop index.

        for (SurfNum = 1; SurfNum <= TotSurfaces; ++SurfNum) {

            if (!Surface(SurfNum).HeatTransSurf) continue;

            SetupEMSActuator("Surface",
                             Surface(SurfNum).Name,
                             "Construction State",
                             "[ ]",
                             Surface(SurfNum).EMSConstructionOverrideON,
                             Surface(SurfNum).EMSConstructionOverrideValue);
        }

        // Setup error checking storage

        if (!allocated(EMSConstructActuatorChecked)) EMSConstructActuatorChecked.allocate(TotConstructs, TotSurfaces);
        EMSConstructActuatorChecked = false;

        if (!allocated(EMSConstructActuatorIsOkay)) EMSConstructActuatorIsOkay.allocate(TotConstructs, TotSurfaces);
        EMSConstructActuatorIsOkay = false;
    }

    void SetupSurfaceOutdoorBoundaryConditionActuators()
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         B. Griffith
        //       DATE WRITTEN   May 2013
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // setup EMS actuators for outside boundary conditions by surface

        // METHODOLOGY EMPLOYED:
        // loop through all surfaces, cycle if not heat transfer or outdoors BC

        // REFERENCES:
        // na

        // Using/Aliasing
        using DataSurfaces::ExternalEnvironment;
        using DataSurfaces::Surface;
        using DataSurfaces::TotSurfaces;

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:
        // na

        // SUBROUTINE PARAMETER DEFINITIONS:
        // na

        // INTERFACE BLOCK SPECIFICATIONS:
        // na

        // DERIVED TYPE DEFINITIONS:
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int SurfNum; // local loop index.

        for (SurfNum = 1; SurfNum <= TotSurfaces; ++SurfNum) {

            if (!Surface(SurfNum).HeatTransSurf) continue;
            if (!(Surface(SurfNum).ExtBoundCond == ExternalEnvironment)) continue;

            SetupEMSActuator("Surface",
                             Surface(SurfNum).Name,
                             "Outdoor Air Drybulb Temperature",
                             "[C]",
                             Surface(SurfNum).OutDryBulbTempEMSOverrideOn,
                             Surface(SurfNum).OutDryBulbTempEMSOverrideValue);

            SetupEMSActuator("Surface",
                             Surface(SurfNum).Name,
                             "Outdoor Air Wetbulb Temperature",
                             "[C]",
                             Surface(SurfNum).OutWetBulbTempEMSOverrideOn,
                             Surface(SurfNum).OutWetBulbTempEMSOverrideValue);
            if (Surface(SurfNum).ExtWind) {
                SetupEMSActuator("Surface",
                                 Surface(SurfNum).Name,
                                 "Outdoor Air Wind Speed",
                                 "[m/s]",
                                 Surface(SurfNum).WindSpeedEMSOverrideOn,
                                 Surface(SurfNum).WindSpeedEMSOverrideValue);
                SetupEMSActuator("Surface",
                                 Surface(SurfNum).Name,
                                 "Outdoor Air Wind Direction",
                                 "[degree]",
                                 Surface(SurfNum).WindDirEMSOverrideOn,
                                 Surface(SurfNum).WindDirEMSOverrideValue);
            }
        }
    }

    void SetupZoneInfoAsInternalDataAvail()
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Brent Griffith
        //       DATE WRITTEN   May 2009
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // set up zone-related info as internal data

        // METHODOLOGY EMPLOYED:
        // <description>

        // REFERENCES:
        // na

        // Using/Aliasing
        using DataGlobals::NumOfZones;
        using DataHeatBalance::Zone;

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:
        // na

        // SUBROUTINE PARAMETER DEFINITIONS:
        // na

        // INTERFACE BLOCK SPECIFICATIONS:
        // na

        // DERIVED TYPE DEFINITIONS:
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:

        int ZoneNum;

        if (allocated(Zone)) {
            for (ZoneNum = 1; ZoneNum <= NumOfZones; ++ZoneNum) {

                SetupEMSInternalVariable("Zone Floor Area", Zone(ZoneNum).Name, "[m2]", Zone(ZoneNum).FloorArea);
                SetupEMSInternalVariable("Zone Air Volume", Zone(ZoneNum).Name, "[m3]", Zone(ZoneNum).Volume);
                SetupEMSInternalVariable("Zone Multiplier", Zone(ZoneNum).Name, "[ ]", Zone(ZoneNum).Multiplier);
                SetupEMSInternalVariable("Zone List Multiplier", Zone(ZoneNum).Name, "[ ]", Zone(ZoneNum).ListMultiplier);
            }
        }
    }

    void SetupZoneOutdoorBoundaryConditionActuators()
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         X Luo
        //       DATE WRITTEN   July 2017
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // setup EMS actuators for outside boundary conditions by surface

        // METHODOLOGY EMPLOYED:
        // loop through all surfaces, cycle if not heat transfer or outdoors BC

        // REFERENCES:
        // na

        // Using/Aliasing
        using DataGlobals::NumOfZones;
        using DataHeatBalance::Zone;

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int ZoneNum; // local loop index.

        for (ZoneNum = 1; ZoneNum <= NumOfZones; ++ZoneNum) {

            SetupEMSActuator("Zone",
                             Zone(ZoneNum).Name,
                             "Outdoor Air Drybulb Temperature",
                             "[C]",
                             Zone(ZoneNum).OutDryBulbTempEMSOverrideOn,
                             Zone(ZoneNum).OutDryBulbTempEMSOverrideValue);
            SetupEMSActuator("Zone",
                             Zone(ZoneNum).Name,
                             "Outdoor Air Wetbulb Temperature",
                             "[C]",
                             Zone(ZoneNum).OutWetBulbTempEMSOverrideOn,
                             Zone(ZoneNum).OutWetBulbTempEMSOverrideValue);
            SetupEMSActuator("Zone",
                             Zone(ZoneNum).Name,
                             "Outdoor Air Wind Speed",
                             "[m/s]",
                             Zone(ZoneNum).WindSpeedEMSOverrideOn,
                             Zone(ZoneNum).WindSpeedEMSOverrideValue);
            SetupEMSActuator("Zone",
                             Zone(ZoneNum).Name,
                             "Outdoor Air Wind Direction",
                             "[degree]",
                             Zone(ZoneNum).WindDirEMSOverrideOn,
                             Zone(ZoneNum).WindDirEMSOverrideValue);
        }
    }

    void checkForUnusedActuatorsAtEnd()
    {
        // call at end of simulation to check if any of the user's actuators were never initialized.
        // Could be a mistake we want to help users catch // Issue #4404.
        for (int actuatorUsedLoop = 1; actuatorUsedLoop <= numActuatorsUsed; ++actuatorUsedLoop) {
            if (!ErlVariable(EMSActuatorUsed(actuatorUsedLoop).ErlVariableNum).Value.initialized) {
                ShowWarningError("checkForUnusedActuatorsAtEnd: Unused EMS Actuator detected, suggesting possible unintended programming error or "
                                 "spelling mistake.");
                ShowContinueError("Check Erl programs related to EMS actuator variable name = " + EMSActuatorUsed(actuatorUsedLoop).Name);
                ShowContinueError("EMS Actuator type name = " + EMSActuatorUsed(actuatorUsedLoop).ComponentTypeName);
                ShowContinueError("EMS Actuator unique component name = " + EMSActuatorUsed(actuatorUsedLoop).UniqueIDName);
                ShowContinueError("EMS Actuator control type = " + EMSActuatorUsed(actuatorUsedLoop).ControlTypeName);
            }
        }
    }

} // namespace EMSManager

// Moved these setup EMS actuator routines out of module to solve circular use problems between
//  ScheduleManager and OutputProcessor. Followed pattern used for SetupOutputVariable

void SetupEMSActuator(std::string const &cComponentTypeName,
                      std::string const &cUniqueIDName,
                      std::string const &cControlTypeName,
                      std::string const &cUnits,
                      bool &lEMSActuated,
                      Real64 &rValue)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Peter Graham Ellis
    //       DATE WRITTEN   June 2006
    //       MODIFIED       Brent Griffith April 2009,
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // register a new actuator for EMS
    //   set  up pointer to logical and real value

    // METHODOLOGY EMPLOYED:
    // push size of ActuatorVariable and add a new one.
    //  check for duplicates.

    // Using/Aliasing
    using namespace DataPrecisionGlobals;
    using namespace DataRuntimeLanguage;

    std::string const UpperCaseObjectType(UtilityRoutines::MakeUPPERCase(cComponentTypeName));
    std::string const UpperCaseObjectName(UtilityRoutines::MakeUPPERCase(cUniqueIDName));
    std::string const UpperCaseActuatorName(UtilityRoutines::MakeUPPERCase(cControlTypeName));

    EMSActuatorKey const key(UpperCaseObjectType, UpperCaseObjectName, UpperCaseActuatorName);

    if (EMSActuator_lookup.find(key) == EMSActuator_lookup.end()) {
        if (numEMSActuatorsAvailable == 0) {
            EMSActuatorAvailable.allocate(varsAvailableAllocInc);
            numEMSActuatorsAvailable = 1;
            maxEMSActuatorsAvailable = varsAvailableAllocInc;
        } else {
            if (numEMSActuatorsAvailable + 1 > maxEMSActuatorsAvailable) {
                EMSActuatorAvailable.redimension(maxEMSActuatorsAvailable *= 2);
            }
            ++numEMSActuatorsAvailable;
        }

        auto &actuator(EMSActuatorAvailable(numEMSActuatorsAvailable));
        actuator.ComponentTypeName = cComponentTypeName;
        actuator.UniqueIDName = cUniqueIDName;
        actuator.ControlTypeName = cControlTypeName;
        actuator.Units = cUnits;
        actuator.Actuated = &lEMSActuated; // Pointer assigment
        actuator.RealValue = &rValue;      // Pointer assigment
        actuator.PntrVarTypeUsed = PntrReal;
        EMSActuator_lookup.insert(key);
    }
}

void SetupEMSActuator(std::string const &cComponentTypeName,
                      std::string const &cUniqueIDName,
                      std::string const &cControlTypeName,
                      std::string const &cUnits,
                      bool &lEMSActuated,
                      int &iValue)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Brent Griffith
    //       DATE WRITTEN   May 2009
    //       MODIFIED
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // register a new actuator for EMS
    //   set  up pointer to logical and integer value

    // METHODOLOGY EMPLOYED:
    // push size of ActuatorVariable and add a new one.
    //  check for duplicates.

    // Using/Aliasing
    using namespace DataPrecisionGlobals;
    using namespace DataRuntimeLanguage;

    std::string const UpperCaseObjectType(UtilityRoutines::MakeUPPERCase(cComponentTypeName));
    std::string const UpperCaseObjectName(UtilityRoutines::MakeUPPERCase(cUniqueIDName));
    std::string const UpperCaseActuatorName(UtilityRoutines::MakeUPPERCase(cControlTypeName));

    EMSActuatorKey const key(UpperCaseObjectType, UpperCaseObjectName, UpperCaseActuatorName);

    if (EMSActuator_lookup.find(key) == EMSActuator_lookup.end()) {
        if (numEMSActuatorsAvailable == 0) {
            EMSActuatorAvailable.allocate(varsAvailableAllocInc);
            numEMSActuatorsAvailable = 1;
            maxEMSActuatorsAvailable = varsAvailableAllocInc;
        } else {
            if (numEMSActuatorsAvailable + 1 > maxEMSActuatorsAvailable) {
                EMSActuatorAvailable.redimension(maxEMSActuatorsAvailable *= 2);
            }
            ++numEMSActuatorsAvailable;
        }

        auto &actuator(EMSActuatorAvailable(numEMSActuatorsAvailable));
        actuator.ComponentTypeName = cComponentTypeName;
        actuator.UniqueIDName = cUniqueIDName;
        actuator.ControlTypeName = cControlTypeName;
        actuator.Units = cUnits;
        actuator.Actuated = &lEMSActuated; // Pointer assigment
        actuator.IntValue = &iValue;       // Pointer assigment
        actuator.PntrVarTypeUsed = PntrInteger;
        EMSActuator_lookup.insert(key);
    }
}

void SetupEMSActuator(std::string const &cComponentTypeName,
                      std::string const &cUniqueIDName,
                      std::string const &cControlTypeName,
                      std::string const &cUnits,
                      bool &lEMSActuated,
                      bool &lValue)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Brent Griffith
    //       DATE WRITTEN   August 2009
    //       MODIFIED
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // register a new actuator for EMS
    //   set  up pointer to logical and logical value

    // METHODOLOGY EMPLOYED:
    // push size of ActuatorVariable and add a new one.
    //  check for duplicates.

    // Using/Aliasing
    using namespace DataPrecisionGlobals;
    using namespace DataRuntimeLanguage;

    std::string const UpperCaseObjectType(UtilityRoutines::MakeUPPERCase(cComponentTypeName));
    std::string const UpperCaseObjectName(UtilityRoutines::MakeUPPERCase(cUniqueIDName));
    std::string const UpperCaseActuatorName(UtilityRoutines::MakeUPPERCase(cControlTypeName));

    EMSActuatorKey const key(UpperCaseObjectType, UpperCaseObjectName, UpperCaseActuatorName);

    if (EMSActuator_lookup.find(key) == EMSActuator_lookup.end()) {
        if (numEMSActuatorsAvailable == 0) {
            EMSActuatorAvailable.allocate(varsAvailableAllocInc);
            numEMSActuatorsAvailable = 1;
            maxEMSActuatorsAvailable = varsAvailableAllocInc;
        } else {
            if (numEMSActuatorsAvailable + 1 > maxEMSActuatorsAvailable) {
                EMSActuatorAvailable.redimension(maxEMSActuatorsAvailable *= 2);
            }
            ++numEMSActuatorsAvailable;
        }

        auto &actuator(EMSActuatorAvailable(numEMSActuatorsAvailable));
        actuator.ComponentTypeName = cComponentTypeName;
        actuator.UniqueIDName = cUniqueIDName;
        actuator.ControlTypeName = cControlTypeName;
        actuator.Units = cUnits;
        actuator.Actuated = &lEMSActuated; // Pointer assigment
        actuator.LogValue = &lValue;       // Pointer assigment
        actuator.PntrVarTypeUsed = PntrLogical;
        EMSActuator_lookup.insert(key);
    }
}

void SetupEMSInternalVariable(std::string const &cDataTypeName, std::string const &cUniqueIDName, std::string const &cUnits, Real64 &rValue)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Brent Griffith
    //       DATE WRITTEN   May 2009
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // Setup internal data source and make available to EMS

    // Using/Aliasing
    using namespace DataPrecisionGlobals;
    using namespace DataRuntimeLanguage;

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int InternalVarAvailNum; // loop index
    bool FoundInternalDataType;
    bool FoundDuplicate;

    // Object Data

    FoundInternalDataType = false;
    FoundDuplicate = false;

    for (InternalVarAvailNum = 1; InternalVarAvailNum <= numEMSInternalVarsAvailable; ++InternalVarAvailNum) {
        if ((UtilityRoutines::SameString(cDataTypeName, EMSInternalVarsAvailable(InternalVarAvailNum).DataTypeName)) &&
            (UtilityRoutines::SameString(cUniqueIDName, EMSInternalVarsAvailable(InternalVarAvailNum).UniqueIDName))) {
            FoundDuplicate = true;
            break;
        }
    }

    if (FoundDuplicate) {
        ShowSevereError("Duplicate internal variable was sent to SetupEMSInternalVariable.");
        ShowContinueError("Internal variable type = " + cDataTypeName + " ; name = " + cUniqueIDName);
        ShowContinueError("Called from SetupEMSInternalVariable.");
    } else {
        // add new internal data variable
        if (numEMSInternalVarsAvailable == 0) {
            EMSInternalVarsAvailable.allocate(varsAvailableAllocInc);
            numEMSInternalVarsAvailable = 1;
            maxEMSInternalVarsAvailable = varsAvailableAllocInc;
        } else {
            if (numEMSInternalVarsAvailable + 1 > maxEMSInternalVarsAvailable) {
                EMSInternalVarsAvailable.redimension(maxEMSInternalVarsAvailable += varsAvailableAllocInc);
            }
            ++numEMSInternalVarsAvailable;
        }

        InternalVarAvailNum = numEMSInternalVarsAvailable;
        EMSInternalVarsAvailable(InternalVarAvailNum).DataTypeName = cDataTypeName;
        EMSInternalVarsAvailable(InternalVarAvailNum).UniqueIDName = cUniqueIDName;
        EMSInternalVarsAvailable(InternalVarAvailNum).Units = cUnits;
        EMSInternalVarsAvailable(InternalVarAvailNum).RealValue = &rValue;
        EMSInternalVarsAvailable(InternalVarAvailNum).PntrVarTypeUsed = PntrReal;
    }
}

void SetupEMSInternalVariable(std::string const &cDataTypeName, std::string const &cUniqueIDName, std::string const &cUnits, int &iValue)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Brent Griffith
    //       DATE WRITTEN   May 2009
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // Setup internal data source and make available to EMS

    // Using/Aliasing
    using namespace DataPrecisionGlobals;
    using namespace DataRuntimeLanguage;

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int InternalVarAvailNum; // loop index
    bool FoundInternalDataType;
    bool FoundDuplicate;

    // Object Data

    FoundInternalDataType = false;
    FoundDuplicate = false;

    for (InternalVarAvailNum = 1; InternalVarAvailNum <= numEMSInternalVarsAvailable; ++InternalVarAvailNum) {
        if ((UtilityRoutines::SameString(cDataTypeName, EMSInternalVarsAvailable(InternalVarAvailNum).DataTypeName)) &&
            (UtilityRoutines::SameString(cUniqueIDName, EMSInternalVarsAvailable(InternalVarAvailNum).UniqueIDName))) {
            FoundDuplicate = true;
            break;
        }
    }

    if (FoundDuplicate) {
        ShowSevereError("Duplicate internal variable was sent to SetupEMSInternalVariable.");
        ShowContinueError("Internal variable type = " + cDataTypeName + " ; name = " + cUniqueIDName);
        ShowContinueError("called from SetupEMSInternalVariable");
    } else {
        // add new internal data variable
        if (numEMSInternalVarsAvailable == 0) {
            EMSInternalVarsAvailable.allocate(varsAvailableAllocInc);
            numEMSInternalVarsAvailable = 1;
            maxEMSInternalVarsAvailable = varsAvailableAllocInc;
        } else {
            if (numEMSInternalVarsAvailable + 1 > maxEMSInternalVarsAvailable) {
                EMSInternalVarsAvailable.redimension(maxEMSInternalVarsAvailable += varsAvailableAllocInc);
            }
            ++numEMSInternalVarsAvailable;
        }

        InternalVarAvailNum = numEMSInternalVarsAvailable;
        EMSInternalVarsAvailable(InternalVarAvailNum).DataTypeName = cDataTypeName;
        EMSInternalVarsAvailable(InternalVarAvailNum).UniqueIDName = cUniqueIDName;
        EMSInternalVarsAvailable(InternalVarAvailNum).Units = cUnits;
        EMSInternalVarsAvailable(InternalVarAvailNum).IntValue = &iValue;
        EMSInternalVarsAvailable(InternalVarAvailNum).PntrVarTypeUsed = PntrInteger;
    }
}

} // namespace EnergyPlus
