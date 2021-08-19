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

// EnergyPlus Headers
#include <EnergyPlus/Autosizing/SystemAirFlowSizing.hh>
#include <EnergyPlus/BranchNodeConnections.hh>
#include <EnergyPlus/CurveManager.hh>
#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/DataContaminantBalance.hh>
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataHVACGlobals.hh>
#include <EnergyPlus/DataHeatBalance.hh>
#include <EnergyPlus/DataLoopNode.hh>
#include <EnergyPlus/DataPrecisionGlobals.hh>
#include <EnergyPlus/DataSizing.hh>
#include <EnergyPlus/EMSManager.hh>
#include <EnergyPlus/EnergyPlus.hh>
#include <EnergyPlus/Fans.hh>
#include <EnergyPlus/FaultsManager.hh>
#include <EnergyPlus/HVACFan.hh>
#include <EnergyPlus/HeatBalanceInternalHeatGains.hh>
#include <EnergyPlus/InputProcessing/InputProcessor.hh>
#include <EnergyPlus/NodeInputManager.hh>
#include <EnergyPlus/OutputProcessor.hh>
#include <EnergyPlus/OutputReportPredefined.hh>
#include <EnergyPlus/Psychrometrics.hh>
#include <EnergyPlus/ScheduleManager.hh>
#include <ObjexxFCL/Optional.hh>

namespace EnergyPlus {

namespace HVACFan {

    int getFanObjectVectorIndex( // lookup vector index for fan object name in object array EnergyPlus::HVACFan::fanObjs
        EnergyPlusData &state,
        std::string const &objectName, // IDF name in input
        bool const ErrorCheck)
    {
        int index = -1;
        bool found = false;
        for (std::size_t loop = 0; loop < state.dataHVACFan->fanObjs.size(); ++loop) {
            if (objectName == state.dataHVACFan->fanObjs[loop]->name) {
                if (!found) {
                    index = loop;
                    found = true;
                } else { // found duplicate
                    // TODO throw warning?
                    index = -1;
                    ShowSevereError(state,
                                    "getFanObjectVectorIndex: Found duplicate Fan:SystemModel inputs of name =" + objectName + ". Check inputs");
                }
            }
        }
        if (!found && ErrorCheck) {
            ShowSevereError(state, "getFanObjectVectorIndex: did not find Fan:SystemModel name =" + objectName + ". Check inputs");
        }
        return index;
    }

    bool checkIfFanNameIsAFanSystem( // look up to see if input contains a Fan:SystemModel with the name (for use before object construction
        EnergyPlusData &state,
        std::string const &objectName)
    {

        int testNum = state.dataInputProcessing->inputProcessor->getObjectItemNum(state, "Fan:SystemModel", objectName);
        if (testNum > 0) {
            return true;
        } else {
            return false;
        }
    }

    void FanSystem::simulate(
        EnergyPlusData &state,
        Optional<Real64 const> flowFraction, // when used, this directs the fan to set the flow at this flow fraction = current flow/ max design flow
                                             // rate.  It is not exactly the same as the legacy speed ratio that was used with SimulateFanComponents.
        Optional_bool_const zoneCompTurnFansOn,  // can be used as turn fans ON signal from ZoneHVAC component
        Optional_bool_const zoneCompTurnFansOff, // can be used as turn Fans OFF signal from ZoneHVAC component
        Optional<Real64 const>
            pressureRise, // Pressure difference to use for DeltaPress, for rating DX coils at a different pressure without entire duct system
        Optional<Real64 const> massFlowRate1,    // Mass flow rate in operating mode 1 [kg/s]
        Optional<Real64 const> runTimeFraction1, // Run time fraction in operating mode 1
        Optional<Real64 const> massFlowRate2,    // Mass flow rate in operating mode 2 [kg/s]
        Optional<Real64 const> runTimeFraction2, // Run time fraction in opearating mode 2
        Optional<Real64 const> pressureRise2     // Pressure difference for operating mode 2
    )
    {

        m_objTurnFansOn = false;
        m_objTurnFansOff = false;

        init(state);

        if (m_objSizingFlag) {
            return; // can't run calculations until sizing is completed
        }

        if (present(zoneCompTurnFansOn) && present(zoneCompTurnFansOff)) {
            // Set module-level logic flags equal to ZoneCompTurnFansOn and ZoneCompTurnFansOff values passed into this routine
            // for ZoneHVAC components with system availability managers defined.
            // The module-level flags get used in the other subroutines (e.g., SimSimpleFan,SimVariableVolumeFan and SimOnOffFan)
            m_objTurnFansOn = zoneCompTurnFansOn;
            m_objTurnFansOff = zoneCompTurnFansOff;
        } else {
            // Set module-level logic flags equal to the global LocalTurnFansOn and LocalTurnFansOff variables for all other cases.
            m_objTurnFansOn = state.dataHVACGlobal->TurnFansOn;
            m_objTurnFansOff = state.dataHVACGlobal->TurnFansOff;
        }
        if (present(pressureRise) && present(massFlowRate1) && present(runTimeFraction1) && present(massFlowRate2) && present(runTimeFraction2) &&
            present(pressureRise2)) {
            Real64 flowRatio1 = massFlowRate1 / m_maxAirMassFlowRate;
            Real64 flowRatio2 = massFlowRate2 / m_maxAirMassFlowRate;
            calcSimpleSystemFan(state, _, pressureRise, flowRatio1, runTimeFraction1, flowRatio2, runTimeFraction2, pressureRise2);
        } else if (!present(pressureRise) && present(massFlowRate1) && present(runTimeFraction1) && present(massFlowRate2) &&
                   present(runTimeFraction2) && !present(pressureRise2)) {
            Real64 flowRatio1 = massFlowRate1 / m_maxAirMassFlowRate;
            Real64 flowRatio2 = massFlowRate2 / m_maxAirMassFlowRate;
            calcSimpleSystemFan(state, flowFraction, _, flowRatio1, runTimeFraction1, flowRatio2, runTimeFraction2, _);
        } else if (present(pressureRise) && present(flowFraction)) {
            calcSimpleSystemFan(state, flowFraction, pressureRise, _, _, _, _, _);
        } else if (present(pressureRise) && !present(flowFraction)) {
            calcSimpleSystemFan(state, _, pressureRise, _, _, _, _, _);
        } else if (!present(pressureRise) && present(flowFraction)) {
            calcSimpleSystemFan(state, flowFraction, _, _, _, _, _, _);
        } else {
            calcSimpleSystemFan(state, _, _, _, _, _, _, _);
        }

        update(state);

        report(state);
    }

    void FanSystem::init(EnergyPlusData &state)
    {
        if (!state.dataGlobal->SysSizingCalc && m_objSizingFlag) {
            set_size(state);
            m_objSizingFlag = false;
        }

        if (state.dataGlobal->BeginEnvrnFlag && m_objEnvrnFlag) {

            // Currently, fan does not force minimum mass flow, only used for power calculation
            // m_minAirFlowRate = designAirVolFlowRate * m_minPowerFlowFrac;
            // m_minAirMassFlowRate = m_minAirFlowRate * m_rhoAirStdInit;

            // Init the Node Control variables
            state.dataLoopNodes->Node(outletNodeNum).MassFlowRateMax = m_maxAirMassFlowRate;
            // Currently, fan does not force minimum mass flow, only used for power calculation
            // DataLoopNode::Node( outletNodeNum ).MassFlowRateMin = m_minAirMassFlowRate;

            // Initialize all report variables to a known state at beginning of simulation
            m_fanPower = 0.0;
            m_deltaTemp = 0.0;
            m_powerLossToAir = 0.0;
            m_fanEnergy = 0.0;
            for (auto loop = 0; loop < m_numSpeeds; ++loop) {
                m_fanRunTimeFractionAtSpeed[loop] = 0.0;
            }
            m_objEnvrnFlag = false;
        }

        if (!state.dataGlobal->BeginEnvrnFlag) {
            m_objEnvrnFlag = true;
        }

        m_massFlowRateMaxAvail =
            min(state.dataLoopNodes->Node(outletNodeNum).MassFlowRateMax, state.dataLoopNodes->Node(inletNodeNum).MassFlowRateMaxAvail);
        m_massFlowRateMinAvail =
            min(max(state.dataLoopNodes->Node(outletNodeNum).MassFlowRateMin, state.dataLoopNodes->Node(inletNodeNum).MassFlowRateMinAvail),
                state.dataLoopNodes->Node(inletNodeNum).MassFlowRateMaxAvail);

        // Load the node data in this section for the component simulation
        // First need to make sure that the MassFlowRate is between the max and min avail.
        m_inletAirMassFlowRate = min(state.dataLoopNodes->Node(inletNodeNum).MassFlowRate, m_massFlowRateMaxAvail);
        m_inletAirMassFlowRate = max(m_inletAirMassFlowRate, m_massFlowRateMinAvail);

        // Then set the other conditions
        m_inletAirTemp = state.dataLoopNodes->Node(inletNodeNum).Temp;
        m_inletAirHumRat = state.dataLoopNodes->Node(inletNodeNum).HumRat;
        m_inletAirEnthalpy = state.dataLoopNodes->Node(inletNodeNum).Enthalpy;
    }

    void FanSystem::set_size(EnergyPlusData &state)
    {
        static constexpr std::string_view routineName = "FanSystem::set_size ";

        Real64 tempFlow = designAirVolFlowRate;
        bool bPRINT = true;
        state.dataSize->DataAutosizable = true;
        state.dataSize->DataEMSOverrideON = m_maxAirFlowRateEMSOverrideOn;
        state.dataSize->DataEMSOverride = m_maxAirFlowRateEMSOverrideValue;

        bool errorsFound = false;
        SystemAirFlowSizer sizerSystemAirFlow;
        sizerSystemAirFlow.initializeWithinEP(state, m_fanType, name, bPRINT, routineName);
        designAirVolFlowRate = sizerSystemAirFlow.size(state, tempFlow, errorsFound);

        state.dataSize->DataAutosizable = true; // should be false?
        state.dataSize->DataEMSOverrideON = false;
        state.dataSize->DataEMSOverride = 0.0;

        if (m_designElecPowerWasAutosized) {

            switch (m_powerSizingMethod) {

            case PowerSizingMethod::powerPerFlow: {
                designElecPower = designAirVolFlowRate * m_elecPowerPerFlowRate;
                break;
            }
            case PowerSizingMethod::powerPerFlowPerPressure: {
                designElecPower = designAirVolFlowRate * deltaPress * m_elecPowerPerFlowRatePerPressure;
                break;
            }
            case PowerSizingMethod::totalEfficiencyAndPressure: {
                designElecPower = designAirVolFlowRate * deltaPress / m_fanTotalEff;
                break;
            }
            case PowerSizingMethod::powerSizingMethodNotSet: {
                // do nothing
                break;
            }

            } // end switch

            // report design power
            BaseSizer::reportSizerOutput(state, m_fanType, name, "Design Electric Power Consumption [W]", designElecPower);

        } // end if power was autosized

        m_rhoAirStdInit = state.dataEnvrn->StdRhoAir;
        m_maxAirMassFlowRate = designAirVolFlowRate * m_rhoAirStdInit;

        // calculate total fan system efficiency at design, else set to 1 to avoid div by zero
        if (designElecPower > 0.0) {
            m_fanTotalEff = designAirVolFlowRate * deltaPress / designElecPower;
        } else {
            m_fanTotalEff = 1.0;
        }

        if (speedControl == SpeedControlMethod::Discrete && m_numSpeeds > 1) { // set up values at speeds
            m_massFlowAtSpeed.resize(m_numSpeeds, 0.0);
            m_totEfficAtSpeed.resize(m_numSpeeds, 0.0);
            for (auto loop = 0; loop < m_numSpeeds; ++loop) {
                m_massFlowAtSpeed[loop] = m_maxAirMassFlowRate * m_flowFractionAtSpeed[loop];
                if (m_powerFractionInputAtSpeed[loop]) { // use speed power fraction
                    if (designElecPower > 0.0) {
                        m_totEfficAtSpeed[loop] =
                            m_flowFractionAtSpeed[loop] * designAirVolFlowRate * deltaPress / (designElecPower * m_powerFractionAtSpeed[loop]);
                    } else {
                        m_totEfficAtSpeed[loop] = 1.0;
                    }
                } else { // use power curve
                    m_totEfficAtSpeed[loop] =
                        m_flowFractionAtSpeed[loop] * designAirVolFlowRate * deltaPress /
                        (designElecPower * CurveManager::CurveValue(state, powerModFuncFlowFractionCurveIndex, m_flowFractionAtSpeed[loop]));
                    m_powerFractionAtSpeed[loop] = CurveManager::CurveValue(state, powerModFuncFlowFractionCurveIndex, m_flowFractionAtSpeed[loop]);
                }
            }
        }
        Real64 rhoAir = Psychrometrics::PsyRhoAirFnPbTdbW(state, state.dataLoopNodes->Node(inletNodeNum).Press, m_inletAirTemp, m_inletAirHumRat);
        m_designPointFEI = report_fei(state, designAirVolFlowRate, designElecPower, deltaPress, rhoAir);

        OutputReportPredefined::PreDefTableEntry(state, state.dataOutRptPredefined->pdchFanType, name, m_fanType);
        OutputReportPredefined::PreDefTableEntry(state, state.dataOutRptPredefined->pdchFanTotEff, name, m_fanTotalEff);
        OutputReportPredefined::PreDefTableEntry(state, state.dataOutRptPredefined->pdchFanDeltaP, name, deltaPress);
        OutputReportPredefined::PreDefTableEntry(state, state.dataOutRptPredefined->pdchFanVolFlow, name, designAirVolFlowRate);

        OutputReportPredefined::PreDefTableEntry(state, state.dataOutRptPredefined->pdchFanPwr, name, designElecPower);
        if (designAirVolFlowRate != 0.0) {
            OutputReportPredefined::PreDefTableEntry(
                state, state.dataOutRptPredefined->pdchFanPwrPerFlow, name, designElecPower / designAirVolFlowRate);
        }
        OutputReportPredefined::PreDefTableEntry(state, state.dataOutRptPredefined->pdchFanMotorIn, name, m_motorInAirFrac);
        OutputReportPredefined::PreDefTableEntry(state, state.dataOutRptPredefined->pdchFanEnergyIndex, name, m_designPointFEI);

        OutputReportPredefined::PreDefTableEntry(state, state.dataOutRptPredefined->pdchFanEndUse, name, m_endUseSubcategoryName);

        m_objSizingFlag = false;
    }

    Real64 FanSystem::report_fei(
        EnergyPlusData &state, Real64 const designFlowRate, Real64 const designElecPower, Real64 const designDeltaPress, Real64 inletRhoAir)
    {
        // PURPOSE OF THIS SUBROUTINE:
        // Calculate the Fan Energy Index

        // REFERENCES:
        // ANSI/AMCA Standard 207-17: Fan System Efficiency and Fan System Input Power Calculation, 2017.
        // AANSI / AMCA Standard 208 - 18: Calculation of the Fan Energy Index, 2018.

        assert(state.dataEnvrn->StdRhoAir > 0.0);
        // Calculate reference fan shaft power
        Real64 refFanShaftPower = (designFlowRate + 0.118) * (designDeltaPress + 100 * inletRhoAir / state.dataEnvrn->StdRhoAir) / (1000 * 0.66);

        // Calculate reference reference fan transmission efficiency
        Real64 refFanTransEff = 0.96 * pow((refFanShaftPower / (refFanShaftPower + 1.64)), 0.05);

        // Calculate reference reference fan motor efficiency
        Real64 refFanMotorOutput = refFanShaftPower / refFanTransEff;

        Real64 refFanMotorEff;
        if (refFanMotorOutput < 185.0) {
            refFanMotorEff = -0.003812 * pow(std::log10(refFanMotorOutput), 4) + 0.025834 * pow(std::log10(refFanMotorOutput), 3) -
                             0.072577 * pow(std::log10(refFanMotorOutput), 2) + 0.125559 * std::log10(refFanMotorOutput) + 0.850274;
        } else {
            refFanMotorEff = 0.962;
        }

        // Calculate reference reference fan motor controller  efficiency
        Real64 refFanMotorCtrlEff = 1;

        Real64 refFanElecPower = refFanShaftPower / (refFanTransEff * refFanMotorEff * refFanMotorCtrlEff);

        if (designElecPower > 0.0) {
            return refFanElecPower * 1000 / designElecPower;
        } else {
            return 0.0;
        }
    }

    FanSystem::FanSystem(EnergyPlusData &state, std::string const &objectName)
        : availSchedIndex(0), inletNodeNum(0), outletNodeNum(0), designAirVolFlowRate(0.0), speedControl(SpeedControlMethod::NotSet), deltaPress(0.0),
          designElecPower(0.0), powerModFuncFlowFractionCurveIndex(0), AirLoopNum(0), AirPathFlag(false), fanIsSecondaryDriver(false),
          m_fanType_Num(0), m_designAirVolFlowRateWasAutosized(false), m_minPowerFlowFrac(0.0), m_motorEff(0.0), m_motorInAirFrac(0.0),
          m_designElecPowerWasAutosized(false), m_powerSizingMethod(PowerSizingMethod::powerSizingMethodNotSet), m_elecPowerPerFlowRate(0.0),
          m_elecPowerPerFlowRatePerPressure(0.0), m_fanTotalEff(0.0), m_nightVentPressureDelta(0.0), m_nightVentFlowFraction(0.0), m_zoneNum(0),
          m_zoneRadFract(0.0), m_heatLossesDestination(ThermalLossDestination::heatLossNotDetermined), m_qdotConvZone(0.0), m_qdotRadZone(0.0),
          m_numSpeeds(0), m_inletAirMassFlowRate(0.0), m_outletAirMassFlowRate(0.0), m_maxAirMassFlowRate(0.0), m_inletAirTemp(0.0),
          m_outletAirTemp(0.0), m_inletAirHumRat(0.0), m_outletAirHumRat(0.0), m_inletAirEnthalpy(0.0), m_outletAirEnthalpy(0.0),
          m_objTurnFansOn(false), m_objTurnFansOff(false), m_objEnvrnFlag(true), m_objSizingFlag(true), m_fanPower(0.0), m_fanEnergy(0.0),
          m_maxAirFlowRateEMSOverrideOn(false), m_maxAirFlowRateEMSOverrideValue(0.0), m_eMSFanPressureOverrideOn(false), m_eMSFanPressureValue(0.0),
          m_eMSFanEffOverrideOn(false), m_eMSFanEffValue(0.0), m_eMSMaxMassFlowOverrideOn(false), m_eMSAirMassFlowValue(0.0),
          m_faultyFilterFlag(false), m_faultyFilterIndex(0),

          m_massFlowRateMaxAvail(0.0), m_massFlowRateMinAvail(0.0), m_rhoAirStdInit(0.0), m_designPointFEI(0.0)
    // oneTimePowerCurveCheck_( true )
    {

        static constexpr std::string_view routineName = "HVACFan constructor ";
        int numAlphas;    // Number of elements in the alpha array
        int numNums;      // Number of elements in the numeric array
        int numTotFields; // Total number of alpha and numeric fields
        int IOStat;       // IO Status when calling get input subroutine
        bool errorsFound = false;
        std::string locCurrentModuleObject = "Fan:SystemModel";
        Array1D_string alphaArgs;
        Array1D_string alphaFieldNames;
        Array1D_bool isAlphaFieldBlank;
        Array1D<Real64> numericArgs;
        Array1D_string numericFieldNames;
        Array1D_bool isNumericFieldBlank;
        int objectNum = state.dataInputProcessing->inputProcessor->getObjectItemNum(state, locCurrentModuleObject, objectName);
        state.dataInputProcessing->inputProcessor->getObjectDefMaxArgs(state, locCurrentModuleObject, numTotFields, numAlphas, numNums);
        if (numAlphas > 0) {
            alphaArgs.allocate(numAlphas);
            alphaFieldNames.allocate(numAlphas);
            isAlphaFieldBlank.allocate(numAlphas);
        }
        if (numNums > 0) {
            numericArgs.allocate(numNums);
            numericFieldNames.allocate(numNums);
            isNumericFieldBlank.allocate(numNums);
        }

        state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                 locCurrentModuleObject,
                                                                 objectNum,
                                                                 alphaArgs,
                                                                 numAlphas,
                                                                 numericArgs,
                                                                 numNums,
                                                                 IOStat,
                                                                 isNumericFieldBlank,
                                                                 isAlphaFieldBlank,
                                                                 alphaFieldNames,
                                                                 numericFieldNames);

        name = alphaArgs(1);
        // TODO how to check for unique names across objects during get input?
        m_fanType = locCurrentModuleObject;
        m_fanType_Num = DataHVACGlobals::FanType_SystemModelObject;
        if (isAlphaFieldBlank(2)) {
            availSchedIndex = DataGlobalConstants::ScheduleAlwaysOn;
        } else {
            availSchedIndex = ScheduleManager::GetScheduleIndex(state, alphaArgs(2));
            if (availSchedIndex == 0) {
                ShowSevereError(state, std::string{routineName} + locCurrentModuleObject + "=\"" + alphaArgs(1) + "\", invalid entry.");
                ShowContinueError(state, "Invalid " + alphaFieldNames(2) + " = " + alphaArgs(2));
                errorsFound = true;
            }
        }
        inletNodeNum = NodeInputManager::GetOnlySingleNode(state,
                                                           alphaArgs(3),
                                                           errorsFound,
                                                           locCurrentModuleObject,
                                                           alphaArgs(1),
                                                           DataLoopNode::NodeFluidType::Air,
                                                           DataLoopNode::NodeConnectionType::Inlet,
                                                           NodeInputManager::compFluidStream::Primary,
                                                           DataLoopNode::ObjectIsNotParent);
        outletNodeNum = NodeInputManager::GetOnlySingleNode(state,
                                                            alphaArgs(4),
                                                            errorsFound,
                                                            locCurrentModuleObject,
                                                            alphaArgs(1),
                                                            DataLoopNode::NodeFluidType::Air,
                                                            DataLoopNode::NodeConnectionType::Outlet,
                                                            NodeInputManager::compFluidStream::Primary,
                                                            DataLoopNode::ObjectIsNotParent);

        BranchNodeConnections::TestCompSet(state, locCurrentModuleObject, alphaArgs(1), alphaArgs(3), alphaArgs(4), "Air Nodes");

        designAirVolFlowRate = numericArgs(1);
        if (designAirVolFlowRate == DataSizing::AutoSize) {
            m_designAirVolFlowRateWasAutosized = true;
        }

        if (isAlphaFieldBlank(5)) {
            speedControl = SpeedControlMethod::Discrete;
        } else if (UtilityRoutines::SameString(alphaArgs(5), "Continuous")) {
            speedControl = SpeedControlMethod::Continuous;
        } else if (UtilityRoutines::SameString(alphaArgs(5), "Discrete")) {
            speedControl = SpeedControlMethod::Discrete;
        } else {
            ShowSevereError(state, std::string{routineName} + locCurrentModuleObject + "=\"" + alphaArgs(1) + "\", invalid entry.");
            ShowContinueError(state, "Invalid " + alphaFieldNames(5) + " = " + alphaArgs(5));
            errorsFound = true;
        }

        m_minPowerFlowFrac = numericArgs(2);
        deltaPress = numericArgs(3);
        if (deltaPress <= 0.0) {
            ShowSevereError(state, std::string{routineName} + locCurrentModuleObject + " zero or negative, invalid entry in " + numericFieldNames(3));
            errorsFound = true;
        }
        m_motorEff = numericArgs(4);
        m_motorInAirFrac = numericArgs(5);
        designElecPower = numericArgs(6);
        if (designElecPower == DataSizing::AutoSize) {
            m_designElecPowerWasAutosized = true;
        }
        if (m_designElecPowerWasAutosized) {
            if (isAlphaFieldBlank(6)) {
                m_powerSizingMethod = PowerSizingMethod::powerPerFlowPerPressure;
            } else if (UtilityRoutines::SameString(alphaArgs(6), "PowerPerFlow")) {
                m_powerSizingMethod = PowerSizingMethod::powerPerFlow;
            } else if (UtilityRoutines::SameString(alphaArgs(6), "PowerPerFlowPerPressure")) {
                m_powerSizingMethod = PowerSizingMethod::powerPerFlowPerPressure;
            } else if (UtilityRoutines::SameString(alphaArgs(6), "TotalEfficiencyAndPressure")) {
                m_powerSizingMethod = PowerSizingMethod::totalEfficiencyAndPressure;
            } else {
                ShowSevereError(state, std::string{routineName} + locCurrentModuleObject + "=\"" + alphaArgs(1) + "\", invalid entry.");
                ShowContinueError(state, "Invalid " + alphaFieldNames(6) + " = " + alphaArgs(6));
                errorsFound = true;
            }
            m_elecPowerPerFlowRate = numericArgs(7);
            m_elecPowerPerFlowRatePerPressure = numericArgs(8);
            m_fanTotalEff = numericArgs(9);
        }
        if (!isAlphaFieldBlank(7)) {
            powerModFuncFlowFractionCurveIndex = CurveManager::GetCurveIndex(state, alphaArgs(7));
            if (powerModFuncFlowFractionCurveIndex == 0) {
                ShowWarningError(state, std::string{routineName} + locCurrentModuleObject + "=\"" + alphaArgs(1) + "\", invalid entry.");
                ShowContinueError(state, "Invalid " + alphaFieldNames(7) + " = " + alphaArgs(7));
                ShowContinueError(state, "Curve not found.");
                if (speedControl == SpeedControlMethod::Continuous) {
                    errorsFound = true;
                }
            }
        } else { // blank
            if (speedControl == SpeedControlMethod::Continuous) {
                ShowWarningError(state, std::string{routineName} + locCurrentModuleObject + "=\"" + alphaArgs(1) + "\", invalid entry.");
                ShowContinueError(state, "Continuous speed control requires a fan power curve in " + alphaFieldNames(7) + " = " + alphaArgs(7));
                errorsFound = true;
            }
        }
        m_nightVentPressureDelta = numericArgs(10);
        m_nightVentFlowFraction = numericArgs(11); // not used
        m_zoneNum = UtilityRoutines::FindItemInList(alphaArgs(8), state.dataHeatBal->Zone);
        if (m_zoneNum > 0) m_heatLossesDestination = ThermalLossDestination::zoneGains;
        if (m_zoneNum == 0) {
            if (isAlphaFieldBlank(8)) {
                m_heatLossesDestination = ThermalLossDestination::lostToOutside;
            } else {
                m_heatLossesDestination = ThermalLossDestination::lostToOutside;
                ShowWarningError(state, std::string{routineName} + locCurrentModuleObject + "=\"" + alphaArgs(1) + "\", invalid entry.");
                ShowContinueError(state, "Invalid " + alphaFieldNames(8) + " = " + alphaArgs(8));
                ShowContinueError(state, "Zone name not found. Fan motor heat losses will not be added to a zone");
                // continue with simulation but motor losses not sent to a zone.
            }
        }
        m_zoneRadFract = numericArgs(12);
        if (!isAlphaFieldBlank(9)) {
            m_endUseSubcategoryName = alphaArgs(9);
        } else {
            m_endUseSubcategoryName = "General";
        }

        if (!isNumericFieldBlank(13)) {
            m_numSpeeds = numericArgs(13);
        } else {
            m_numSpeeds = 1;
        }
        m_fanRunTimeFractionAtSpeed.resize(m_numSpeeds, 0.0);
        if (speedControl == SpeedControlMethod::Discrete && m_numSpeeds > 1) {
            // should have field sets
            m_flowFractionAtSpeed.resize(m_numSpeeds, 0.0);
            m_powerFractionAtSpeed.resize(m_numSpeeds, 0.0);
            m_powerFractionInputAtSpeed.resize(m_numSpeeds, false);
            if (m_numSpeeds == ((numNums - 13) / 2) || m_numSpeeds == ((numNums + 1 - 13) / 2)) {
                for (auto loopSet = 0; loopSet < m_numSpeeds; ++loopSet) {
                    m_flowFractionAtSpeed[loopSet] = numericArgs(13 + loopSet * 2 + 1);
                    if (!isNumericFieldBlank(13 + loopSet * 2 + 2)) {
                        m_powerFractionAtSpeed[loopSet] = numericArgs(13 + loopSet * 2 + 2);
                        m_powerFractionInputAtSpeed[loopSet] = true;
                    } else {
                        m_powerFractionInputAtSpeed[loopSet] = false;
                    }
                }
            } else {
                // field set input does not match number of speeds, throw warning
                ShowSevereError(state, std::string{routineName} + locCurrentModuleObject + "=\"" + alphaArgs(1) + "\", invalid entry.");
                ShowContinueError(state, "Fan with Discrete speed control does not have input for speed data that matches the number of speeds.");
                errorsFound = true;
            }
            // check that flow fractions are increasing
            bool increasingOrderError = false;
            for (auto loop = 0; loop < (m_numSpeeds - 1); ++loop) {
                if (m_flowFractionAtSpeed[loop] > m_flowFractionAtSpeed[loop + 1]) {
                    increasingOrderError = true;
                }
            }
            if (increasingOrderError) {
                ShowSevereError(state, std::string{routineName} + locCurrentModuleObject + "=\"" + alphaArgs(1) + "\", invalid entry.");
                ShowContinueError(state,
                                  "Fan with Discrete speed control and multiple speed levels does not have input with flow fractions arranged in "
                                  "increasing order.");
                errorsFound = true;
            }
        }

        // check if power curve present when any speeds have no power fraction
        if (speedControl == SpeedControlMethod::Discrete && m_numSpeeds > 1 && powerModFuncFlowFractionCurveIndex == 0) {
            bool foundMissingPowerFraction = false;
            for (auto loop = 0; loop < m_numSpeeds; ++loop) {
                if (!m_powerFractionInputAtSpeed[loop]) {
                    foundMissingPowerFraction = true;
                }
            }
            if (foundMissingPowerFraction) {
                // field set input does not match number of speeds, throw warning
                ShowSevereError(state, std::string{routineName} + locCurrentModuleObject + "=\"" + alphaArgs(1) + "\", invalid entry.");
                ShowContinueError(
                    state,
                    "Fan with Discrete speed control does not have input for power fraction at all speed levels and does not have a power curve.");
                errorsFound = true;
            }
        }

        if (errorsFound) {
            ShowFatalError(state, std::string{routineName} + "Errors found in input for fan name = " + name + ".  Program terminates.");
        }

        SetupOutputVariable(state,
                            "Fan Electricity Rate",
                            OutputProcessor::Unit::W,
                            m_fanPower,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Average,
                            name);
        SetupOutputVariable(state,
                            "Fan Rise in Air Temperature",
                            OutputProcessor::Unit::deltaC,
                            m_deltaTemp,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Average,
                            name);
        SetupOutputVariable(state,
                            "Fan Heat Gain to Air",
                            OutputProcessor::Unit::W,
                            m_powerLossToAir,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Average,
                            name);
        SetupOutputVariable(state,
                            "Fan Electricity Energy",
                            OutputProcessor::Unit::J,
                            m_fanEnergy,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Summed,
                            name,
                            _,
                            "Electricity",
                            "Fans",
                            m_endUseSubcategoryName,
                            "System");
        SetupOutputVariable(state,
                            "Fan Air Mass Flow Rate",
                            OutputProcessor::Unit::kg_s,
                            m_outletAirMassFlowRate,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Average,
                            name);
        if (speedControl == SpeedControlMethod::Discrete && m_numSpeeds == 1) {
            SetupOutputVariable(state,
                                "Fan Runtime Fraction",
                                OutputProcessor::Unit::None,
                                m_fanRunTimeFractionAtSpeed[0],
                                OutputProcessor::SOVTimeStepType::System,
                                OutputProcessor::SOVStoreType::Average,
                                name);
        } else if (speedControl == SpeedControlMethod::Discrete && m_numSpeeds > 1) {
            for (auto speedLoop = 0; speedLoop < m_numSpeeds; ++speedLoop) {
                SetupOutputVariable(state,
                                    "Fan Runtime Fraction Speed " + fmt::to_string(speedLoop + 1),
                                    OutputProcessor::Unit::None,
                                    m_fanRunTimeFractionAtSpeed[speedLoop],
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::Average,
                                    name);
            }
        }

        if (state.dataGlobal->AnyEnergyManagementSystemInModel) {
            SetupEMSInternalVariable(state, "Fan Maximum Mass Flow Rate", name, "[kg/s]", m_maxAirMassFlowRate);
            SetupEMSActuator(state, "Fan", name, "Fan Air Mass Flow Rate", "[kg/s]", m_eMSMaxMassFlowOverrideOn, m_eMSAirMassFlowValue);
            SetupEMSInternalVariable(state, "Fan Nominal Pressure Rise", name, "[Pa]", deltaPress);
            SetupEMSActuator(state, "Fan", name, "Fan Pressure Rise", "[Pa]", m_eMSFanPressureOverrideOn, m_eMSFanPressureValue);
            SetupEMSInternalVariable(state, "Fan Nominal Total Efficiency", name, "[fraction]", m_fanTotalEff);
            SetupEMSActuator(state, "Fan", name, "Fan Total Efficiency", "[fraction]", m_eMSFanEffOverrideOn, m_eMSFanEffValue);
            SetupEMSActuator(
                state, "Fan", name, "Fan Autosized Air Flow Rate", "[m3/s]", m_maxAirFlowRateEMSOverrideOn, m_maxAirFlowRateEMSOverrideValue);
        }

        if (m_heatLossesDestination == ThermalLossDestination::zoneGains) {
            SetupZoneInternalGain(
                state, m_zoneNum, "Fan:SystemModel", name, DataHeatBalance::IntGainTypeOf_FanSystemModel, &m_qdotConvZone, nullptr, &m_qdotRadZone);
        }

        alphaArgs.deallocate();
        alphaFieldNames.deallocate();
        isAlphaFieldBlank.deallocate();
        numericArgs.deallocate();
        numericFieldNames.deallocate();
        isNumericFieldBlank.deallocate();

        bool anyEMSRan = false;
        EMSManager::ManageEMS(state, EMSManager::EMSCallFrom::ComponentGetInput, anyEMSRan, ObjexxFCL::Optional_int_const());
    }

    void
    FanSystem::calcSimpleSystemFan(EnergyPlusData &state,
                                   Optional<Real64 const> flowFraction, // Flow fraction for entire timestep (not used if flow ratios are present)
                                   Optional<Real64 const> pressureRise, // Pressure difference to use for DeltaPress
                                   Optional<Real64 const> flowRatio1,   // Flow ratio in operating mode 1
                                   Optional<Real64 const> runTimeFrac1, // Run time fraction in operating mode 1
                                   Optional<Real64 const> flowRatio2,   // Flow ratio in operating mode 2
                                   Optional<Real64 const> runTimeFrac2, // Run time fraction in operating mode 2
                                   Optional<Real64 const> pressureRise2 // Pressure difference to use for operating mode 2
    )
    {
        std::vector<Real64> localPressureRise; // [0] is operating mode 1, [1] is operating mode 2
        Real64 localFlowFraction;
        Real64 localFanTotEff;
        std::vector<Real64> localAirMassFlow;
        std::vector<Real64> localFlowRatio;
        std::vector<Real64> localRunTimeFrac;
        bool localUseFlowRatiosAndRunTimeFracs = false;

        int localNumModes = 1; // Number of operating modes, 1 or 2 ( e.g. heating, ventilating, cooling)
        if (present(flowRatio2) && present(runTimeFrac2)) localNumModes = 2;
        localPressureRise.resize(2, 0.0);
        localAirMassFlow.resize(2, 0.0);
        localFlowRatio.resize(2, 0.0);
        localRunTimeFrac.resize(2, 1.0);

        if (state.dataHVACGlobal->NightVentOn) {
            // assume if non-zero inputs for night data then this fan is to be used with that data
            if (m_nightVentPressureDelta > 0.0) {
                localPressureRise[0] = m_nightVentPressureDelta;
                localPressureRise[1] = m_nightVentPressureDelta;
            }

            if (m_maxAirMassFlowRate > 0.0) { // protect div by 0
                localFlowFraction = m_inletAirMassFlowRate / m_maxAirMassFlowRate;
            } else {
                localFlowFraction = 1.0;
            }
            localAirMassFlow[0] = m_inletAirMassFlowRate;

        } else { // not in night mode
            if (present(pressureRise)) {
                localPressureRise[0] = pressureRise;
            } else {
                localPressureRise[0] = deltaPress;
            }
            if (present(pressureRise2)) {
                localPressureRise[1] = pressureRise2;
            } else {
                localPressureRise[1] = deltaPress;
            }
            if (present(flowFraction)) {
                localFlowFraction = flowFraction;
                localAirMassFlow[0] = localFlowFraction * m_maxAirMassFlowRate;
            } else {
                if (m_maxAirMassFlowRate > 0.0) { // protect div by 0
                    localFlowFraction = m_inletAirMassFlowRate / m_maxAirMassFlowRate;
                } else {
                    localFlowFraction = 1.0;
                }
                localAirMassFlow[0] = m_inletAirMassFlowRate;
            }
            if (present(flowRatio1) && present(flowRatio2) && present(runTimeFrac1) && present(runTimeFrac2)) {
                localUseFlowRatiosAndRunTimeFracs = true;
                localRunTimeFrac[0] = runTimeFrac1;
                localRunTimeFrac[1] = runTimeFrac2;
                localFlowRatio[0] = flowRatio1;
                localAirMassFlow[0] = localFlowRatio[0] * m_maxAirMassFlowRate * localRunTimeFrac[0];
                localFlowRatio[1] = flowRatio2;
                localAirMassFlow[1] = localFlowRatio[1] * m_maxAirMassFlowRate * localRunTimeFrac[1];
            } else {
                localRunTimeFrac[0] = 1.0; // if runTimeFracs are not present, assume single-mode operation
                localRunTimeFrac[1] = 0.0; // if runTimeFracs are not present, assume single-mode operation
            }
        }

        Real64 localFaultMaxAirMassFlow = 0.0;
        bool faultActive = false;
        Real64 localFaultPressureRise = 0.0;
        if (m_faultyFilterFlag && (state.dataFaultsMgr->NumFaultyAirFilter > 0) && (!state.dataGlobal->WarmupFlag) &&
            (!state.dataGlobal->DoingSizing) && state.dataGlobal->DoWeathSim && (!m_eMSMaxMassFlowOverrideOn) && (!m_eMSFanPressureOverrideOn)) {
            if (ScheduleManager::GetCurrentScheduleValue(state, state.dataFaultsMgr->FaultsFouledAirFilters(m_faultyFilterIndex).AvaiSchedPtr) > 0) {
                faultActive = true;
                Real64 FanDesignFlowRateDec = 0; // Decrease of the Fan Design Volume Flow Rate [m3/sec]
                FanDesignFlowRateDec = Fans::CalFaultyFanAirFlowReduction(
                    state,
                    name,
                    designAirVolFlowRate,
                    deltaPress,
                    (ScheduleManager::GetCurrentScheduleValue(
                         state, state.dataFaultsMgr->FaultsFouledAirFilters(m_faultyFilterIndex).FaultyAirFilterPressFracSchePtr) -
                     1) *
                        deltaPress,
                    state.dataFaultsMgr->FaultsFouledAirFilters(m_faultyFilterIndex).FaultyAirFilterFanCurvePtr);

                localFaultMaxAirMassFlow = m_maxAirMassFlowRate - FanDesignFlowRateDec * m_rhoAirStdInit;

                localFaultPressureRise =
                    ScheduleManager::GetCurrentScheduleValue(
                        state, state.dataFaultsMgr->FaultsFouledAirFilters(m_faultyFilterIndex).FaultyAirFilterPressFracSchePtr) *
                    deltaPress;
            }
        }

        for (int mode = 0; mode < localNumModes; ++mode) {
            // EMS override MassFlow, DeltaPress, and FanEff
            if (m_eMSFanPressureOverrideOn) localPressureRise[mode] = m_eMSFanPressureValue;
            if (m_eMSFanEffOverrideOn) localFanTotEff = m_eMSFanEffValue;
            if (m_eMSMaxMassFlowOverrideOn) {
                localAirMassFlow[mode] = m_eMSAirMassFlowValue;
            }

            localAirMassFlow[mode] = min(localAirMassFlow[mode], m_maxAirMassFlowRate);
            if (faultActive) {
                localAirMassFlow[mode] = min(localAirMassFlow[mode], localFaultMaxAirMassFlow);
                localPressureRise[mode] = localFaultPressureRise;
            }
            localFlowFraction = localAirMassFlow[0] / m_maxAirMassFlowRate;
            localFlowFraction = min(1.0, localFlowFraction);

            if (localRunTimeFrac[mode] > 0.0) {
                localFlowRatio[mode] = localAirMassFlow[mode] / (m_maxAirMassFlowRate * localRunTimeFrac[mode]);
            }
            localFlowRatio[mode] = min(1.0, localFlowRatio[mode]);
        }

        // zero these now, because the may accumulate across multiple operating modes
        m_powerLossToAir = 0.0;
        m_fanPower = 0.0;
        m_outletAirMassFlowRate = 0.0;
        if (speedControl == SpeedControlMethod::Discrete) {
            for (auto loop = 0; loop < m_numSpeeds; ++loop) {
                m_fanRunTimeFractionAtSpeed[loop] = 0.0;
            }
        }

        if ((ScheduleManager::GetCurrentScheduleValue(state, availSchedIndex) > 0.0 || m_objTurnFansOn) && !m_objTurnFansOff &&
            ((localAirMassFlow[0] + localAirMassFlow[1]) > 0.0)) {
            // fan is running

            for (int mode = 0; mode < localNumModes; ++mode) {

                // if no flow for this mode then continue to the next mode
                if (localAirMassFlow[mode] == 0.0) continue;

                switch (speedControl) {

                case SpeedControlMethod::Discrete: {
                    //
                    if (state.dataHVACGlobal->OnOffFanPartLoadFraction <= 0.0) {
                        state.dataHVACGlobal->OnOffFanPartLoadFraction = 1.0;
                    }
                    if (state.dataHVACGlobal->OnOffFanPartLoadFraction < 0.7) {
                        state.dataHVACGlobal->OnOffFanPartLoadFraction =
                            0.7; // a warning message is already issued from the DX coils or gas heating coil
                    }
                    if (localUseFlowRatiosAndRunTimeFracs) {
                        // Use flow ratios and runtimefractions pass from parent (allows fan to cycle at a specified speed)
                        Real64 locRunTimeFraction(0.0);
                        if (state.dataHVACGlobal->OnOffFanPartLoadFraction >= 1.0) {
                            locRunTimeFraction = localRunTimeFrac[mode];
                        } else {
                            locRunTimeFraction = max(0.0, min(1.0, localRunTimeFrac[mode] / state.dataHVACGlobal->OnOffFanPartLoadFraction));
                        }
                        Real64 locFlowRatio = localFlowRatio[mode]; // Current mode flow rate / max flow rate
                        Real64 locLowSpeedFanRunTimeFrac = 0.0;
                        Real64 locHiSpeedFanRunTimeFrac = 0.0;
                        if (m_numSpeeds == 1) { // CV or OnOff
                            localFanTotEff = m_fanTotalEff;
                            locHiSpeedFanRunTimeFrac = locRunTimeFraction * locFlowRatio;
                            m_fanRunTimeFractionAtSpeed[0] += locHiSpeedFanRunTimeFrac;
                            m_fanPower += max(
                                0.0, locHiSpeedFanRunTimeFrac * m_maxAirMassFlowRate * localPressureRise[mode] / (localFanTotEff * m_rhoAirStdInit));
                        } else if (m_numSpeeds > 1) { // multi speed

                            // find which two speed levels bracket flow ratios and calculate runtimefraction at each speed
                            // ideally the flow ratios passed in will match one of the fan m_flowFractionAtSpeed but it is not required
                            int lowSideSpeed = -1;
                            int hiSideSpeed = -1;

                            if (locFlowRatio <= m_flowFractionAtSpeed[0]) { // on/off at lowest speed
                                hiSideSpeed = 0;
                                locHiSpeedFanRunTimeFrac = locFlowRatio * locRunTimeFraction / m_flowFractionAtSpeed[0];
                                m_fanRunTimeFractionAtSpeed[0] += locHiSpeedFanRunTimeFrac;
                            } else {
                                lowSideSpeed = 0; // hush up cppcheck
                                hiSideSpeed = 0;  // hush up cppcheck
                                for (auto loop = 0; loop < m_numSpeeds - 1; ++loop) {
                                    if ((m_flowFractionAtSpeed[loop] <= locFlowRatio) && (locFlowRatio <= m_flowFractionAtSpeed[loop + 1])) {
                                        lowSideSpeed = loop;
                                        hiSideSpeed = loop + 1;
                                        break;
                                    }
                                }
                                Real64 locLowSpeedTimeFrac = (m_flowFractionAtSpeed[hiSideSpeed] - locFlowRatio) /
                                                             (m_flowFractionAtSpeed[hiSideSpeed] - m_flowFractionAtSpeed[lowSideSpeed]);
                                locLowSpeedFanRunTimeFrac = locLowSpeedTimeFrac * localRunTimeFrac[mode];
                                locHiSpeedFanRunTimeFrac = (1 - locLowSpeedTimeFrac) * localRunTimeFrac[mode];
                                m_fanRunTimeFractionAtSpeed[lowSideSpeed] += locLowSpeedFanRunTimeFrac;
                                m_fanRunTimeFractionAtSpeed[hiSideSpeed] += locHiSpeedFanRunTimeFrac;
                            }
                            if (lowSideSpeed != -1 && hiSideSpeed != -1) {
                                m_fanPower += max(0.0,
                                                  locLowSpeedFanRunTimeFrac * m_massFlowAtSpeed[lowSideSpeed] * localPressureRise[mode] /
                                                          (m_totEfficAtSpeed[lowSideSpeed] * m_rhoAirStdInit) +
                                                      locHiSpeedFanRunTimeFrac * m_massFlowAtSpeed[hiSideSpeed] * localPressureRise[mode] /
                                                          (m_totEfficAtSpeed[hiSideSpeed] * m_rhoAirStdInit));
                            } else if (lowSideSpeed == -1 && hiSideSpeed == 0) {
                                m_fanPower += max(0.0,
                                                  locHiSpeedFanRunTimeFrac * m_massFlowAtSpeed[hiSideSpeed] * localPressureRise[mode] /
                                                      (m_totEfficAtSpeed[hiSideSpeed] * m_rhoAirStdInit));
                            }
                        }
                    } else {
                        // Use localFlowFraction which is not locked at a particular flow ratio (legacy method for fan:onoff)
                        Real64 locFanRunTimeFraction(0.0);
                        Real64 locLowSpeedFanRunTimeFrac = 0.0;
                        Real64 locHiSpeedFanRunTimeFrac = 0.0;
                        if (state.dataHVACGlobal->OnOffFanPartLoadFraction >= 1.0) {
                            locFanRunTimeFraction = localFlowFraction;
                        } else {
                            locFanRunTimeFraction = max(0.0, min(1.0, localFlowFraction / state.dataHVACGlobal->OnOffFanPartLoadFraction));
                        }
                        if (m_numSpeeds == 1) { // CV or OnOff
                            localFanTotEff = m_fanTotalEff;
                            locHiSpeedFanRunTimeFrac = locFanRunTimeFraction;
                            m_fanRunTimeFractionAtSpeed[0] += locHiSpeedFanRunTimeFrac;
                            m_fanPower += max(
                                0.0, locHiSpeedFanRunTimeFrac * m_maxAirMassFlowRate * localPressureRise[mode] / (localFanTotEff * m_rhoAirStdInit));
                        } else if (m_numSpeeds > 1) { // multi speed

                            // find which two speed levels bracket flow fraction and calculate runtimefraction
                            int lowSideSpeed = -1;
                            int hiSideSpeed = -1;

                            if (locFanRunTimeFraction < m_flowFractionAtSpeed[0]) { // on/off between zero and lowest speed
                                hiSideSpeed = 0;
                                locHiSpeedFanRunTimeFrac = locFanRunTimeFraction / m_flowFractionAtSpeed[0];
                                m_fanRunTimeFractionAtSpeed[0] += locHiSpeedFanRunTimeFrac;
                            } else {
                                lowSideSpeed = 0; // hush up cppcheck
                                hiSideSpeed = 0;  // hush up cppcheck
                                for (auto loop = 0; loop < m_numSpeeds - 1; ++loop) {
                                    if ((m_flowFractionAtSpeed[loop] <= locFanRunTimeFraction) &&
                                        (locFanRunTimeFraction <= m_flowFractionAtSpeed[loop + 1])) {
                                        lowSideSpeed = loop;
                                        hiSideSpeed = loop + 1;
                                        break;
                                    }
                                }
                                locLowSpeedFanRunTimeFrac = (m_flowFractionAtSpeed[hiSideSpeed] - locFanRunTimeFraction) /
                                                            (m_flowFractionAtSpeed[hiSideSpeed] - m_flowFractionAtSpeed[lowSideSpeed]);
                                locHiSpeedFanRunTimeFrac = (locFanRunTimeFraction - m_flowFractionAtSpeed[lowSideSpeed]) /
                                                           (m_flowFractionAtSpeed[hiSideSpeed] - m_flowFractionAtSpeed[lowSideSpeed]);
                                m_fanRunTimeFractionAtSpeed[lowSideSpeed] += locLowSpeedFanRunTimeFrac;
                                m_fanRunTimeFractionAtSpeed[hiSideSpeed] += locHiSpeedFanRunTimeFrac;
                            }
                            if (lowSideSpeed != -1 && hiSideSpeed != -1) {
                                m_fanPower += max(0.0,
                                                  locLowSpeedFanRunTimeFrac * m_massFlowAtSpeed[lowSideSpeed] * localPressureRise[mode] /
                                                          (m_totEfficAtSpeed[lowSideSpeed] * m_rhoAirStdInit) +
                                                      locHiSpeedFanRunTimeFrac * m_massFlowAtSpeed[hiSideSpeed] * localPressureRise[mode] /
                                                          (m_totEfficAtSpeed[hiSideSpeed] * m_rhoAirStdInit));
                            } else if (lowSideSpeed == -1 && hiSideSpeed == 0) {
                                m_fanPower += max(0.0,
                                                  locHiSpeedFanRunTimeFrac * m_massFlowAtSpeed[hiSideSpeed] * localPressureRise[mode] /
                                                      (m_totEfficAtSpeed[hiSideSpeed] * m_rhoAirStdInit));
                            }
                        }
                    }
                    localFanTotEff = m_fanTotalEff;
                    break;
                }
                case SpeedControlMethod::Continuous: {
                    localFanTotEff = m_fanTotalEff;
                    Real64 locFlowRatio(0.0);
                    Real64 locFanRunTimeFraction(0.0);
                    if (localUseFlowRatiosAndRunTimeFracs) {
                        locFlowRatio = localFlowRatio[mode];
                        locFanRunTimeFraction = localRunTimeFrac[mode];
                    } else {
                        locFlowRatio = localFlowFraction;
                        locFanRunTimeFraction = 1.0;
                    }

                    Real64 localFlowFractionForPower = max(m_minPowerFlowFrac, locFlowRatio);
                    Real64 localPowerFraction(0.0);
                    if (state.dataHVACGlobal->NightVentOn) {
                        localPowerFraction = 1.0; // not sure why, but legacy fan had this for night ventilation
                    } else {
                        localPowerFraction = CurveManager::CurveValue(state, powerModFuncFlowFractionCurveIndex, localFlowFractionForPower);
                    }
                    Real64 localfanPower = max(0.0,
                                               locFanRunTimeFraction * localPowerFraction * m_maxAirMassFlowRate * localPressureRise[mode] /
                                                   (localFanTotEff * m_rhoAirStdInit));
                    Real64 fanShaftPower = m_motorEff * localfanPower;
                    Real64 localpowerLossToAir = fanShaftPower + (localfanPower - fanShaftPower) * m_motorInAirFrac;
                    m_outletAirEnthalpy = m_inletAirEnthalpy + localpowerLossToAir / localAirMassFlow[mode]; // this will get revised later
                    m_outletAirHumRat = m_inletAirHumRat;                                                    // this will get revised later
                    m_outletAirTemp = Psychrometrics::PsyTdbFnHW(m_outletAirEnthalpy, m_outletAirHumRat);    // this will get revised later
                    // When fan air flow is less than 10%, the fan power curve is linearized between the 10% to 0% to
                    //  avoid the unrealistic high temperature rise across the fan.
                    Real64 deltaTAcrossFan = m_outletAirTemp - m_inletAirTemp;
                    if (deltaTAcrossFan > 20.0) {
                        Real64 minFlowFracLimitFanHeat = 0.10;
                        Real64 powerFractionAtLowMin = 0.0;
                        Real64 fanPoweratLowMinimum = 0.0;
                        if (localFlowFractionForPower < minFlowFracLimitFanHeat) {
                            powerFractionAtLowMin = CurveManager::CurveValue(state, powerModFuncFlowFractionCurveIndex, minFlowFracLimitFanHeat);
                            fanPoweratLowMinimum =
                                powerFractionAtLowMin * m_maxAirMassFlowRate * localPressureRise[mode] / (localFanTotEff * m_rhoAirStdInit);
                            localfanPower = max(0.0, localFlowFractionForPower * fanPoweratLowMinimum / minFlowFracLimitFanHeat);
                        } else if (locFlowRatio < minFlowFracLimitFanHeat) {
                            powerFractionAtLowMin = CurveManager::CurveValue(state, powerModFuncFlowFractionCurveIndex, minFlowFracLimitFanHeat);
                            fanPoweratLowMinimum =
                                powerFractionAtLowMin * m_maxAirMassFlowRate * localPressureRise[mode] / (localFanTotEff * m_rhoAirStdInit);
                            localfanPower = max(0.0, locFlowRatio * fanPoweratLowMinimum / minFlowFracLimitFanHeat);
                        }
                    }
                    m_fanPower += localfanPower;
                    break;
                } // continuous speed control case
                case SpeedControlMethod::NotSet: {
                    // do nothing
                    break;
                }
                } // end switch
                m_outletAirMassFlowRate += localAirMassFlow[mode];

            } // end of operating mode loop

            if (m_outletAirMassFlowRate > 0.0) {
                Real64 fanShaftPower = m_motorEff * m_fanPower; // power delivered to shaft
                m_powerLossToAir = fanShaftPower + (m_fanPower - fanShaftPower) * m_motorInAirFrac;
                m_outletAirEnthalpy = m_inletAirEnthalpy + m_powerLossToAir / m_outletAirMassFlowRate;
                // This fan does not change the moisture or Mass Flow across the component
                m_outletAirHumRat = m_inletAirHumRat;
                m_outletAirTemp = Psychrometrics::PsyTdbFnHW(m_outletAirEnthalpy, m_outletAirHumRat);
            } else {
                m_fanPower = 0.0;
                m_powerLossToAir = 0.0;
                m_outletAirHumRat = m_inletAirHumRat;
                m_outletAirEnthalpy = m_inletAirEnthalpy;
                m_outletAirTemp = m_inletAirTemp;
                m_massFlowRateMaxAvail = 0.0;
                m_massFlowRateMinAvail = 0.0;
            }

        } else { // fan is off
            // Fan is off and not operating no power consumed and mass flow rate.
            m_fanPower = 0.0;
            m_powerLossToAir = 0.0;
            m_outletAirHumRat = m_inletAirHumRat;
            m_outletAirEnthalpy = m_inletAirEnthalpy;
            m_outletAirTemp = m_inletAirTemp;
            // Set the Control Flow variables to 0.0 flow when OFF.
            if (fanIsSecondaryDriver) {
                m_outletAirMassFlowRate =
                    localAirMassFlow[0] +
                    localAirMassFlow[1]; // sometimes the air is moving with the fan off, eg. AirTerminal:SingleDuct:VAV:Reheat:VariableSpeedFan
                if (m_outletAirMassFlowRate == 0.0) {
                    m_massFlowRateMaxAvail = 0.0;
                    m_massFlowRateMinAvail = 0.0;
                }
            } else {
                m_outletAirMassFlowRate = 0.0;
                m_massFlowRateMaxAvail = 0.0;
                m_massFlowRateMinAvail = 0.0;
            }
        }

        if (m_heatLossesDestination == ThermalLossDestination::zoneGains) {
            Real64 powerLossToZone = m_fanPower - m_powerLossToAir;
            m_qdotConvZone = powerLossToZone * (1.0 - m_zoneRadFract);
            m_qdotRadZone = powerLossToZone * m_zoneRadFract;
        }
        state.dataHVACGlobal->OnOffFanPartLoadFraction = 1.0; // reset to 1
    }

    void FanSystem::update(EnergyPlusData &state) const // does not change state of object, only update elsewhere
    {
        // Set the outlet air node of the fan
        state.dataLoopNodes->Node(outletNodeNum).MassFlowRate = m_outletAirMassFlowRate;
        state.dataLoopNodes->Node(outletNodeNum).Temp = m_outletAirTemp;
        state.dataLoopNodes->Node(outletNodeNum).HumRat = m_outletAirHumRat;
        state.dataLoopNodes->Node(outletNodeNum).Enthalpy = m_outletAirEnthalpy;
        // Set the outlet nodes for properties that just pass through & not used
        state.dataLoopNodes->Node(outletNodeNum).Quality = state.dataLoopNodes->Node(inletNodeNum).Quality;
        state.dataLoopNodes->Node(outletNodeNum).Press = state.dataLoopNodes->Node(inletNodeNum).Press;

        // Set the Node Flow Control Variables from the Fan Control Variables
        state.dataLoopNodes->Node(outletNodeNum).MassFlowRateMaxAvail = m_massFlowRateMaxAvail;
        state.dataLoopNodes->Node(outletNodeNum).MassFlowRateMinAvail = m_massFlowRateMinAvail;

        // make sure inlet has the same mass flow
        state.dataLoopNodes->Node(inletNodeNum).MassFlowRate = m_outletAirMassFlowRate;

        if (state.dataContaminantBalance->Contaminant.CO2Simulation) {
            state.dataLoopNodes->Node(outletNodeNum).CO2 = state.dataLoopNodes->Node(inletNodeNum).CO2;
        }
        if (state.dataContaminantBalance->Contaminant.GenericContamSimulation) {
            state.dataLoopNodes->Node(outletNodeNum).GenContam = state.dataLoopNodes->Node(inletNodeNum).GenContam;
        }

        if (speedControl == SpeedControlMethod::Continuous) {
            if (AirLoopNum > 0) {
                state.dataAirLoop->AirLoopAFNInfo(AirLoopNum).AFNLoopOnOffFanRTF = m_fanRunTimeFractionAtSpeed[0];
            }
        } else {
            if (AirLoopNum > 0) {
                if (m_numSpeeds == 1) {
                    state.dataAirLoop->AirLoopAFNInfo(AirLoopNum).AFNLoopOnOffFanRTF = m_outletAirMassFlowRate / m_maxAirMassFlowRate;
                } else {
                    if (m_outletAirMassFlowRate <= m_massFlowAtSpeed[0]) {
                        state.dataAirLoop->AirLoopAFNInfo(AirLoopNum).AFNLoopOnOffFanRTF = m_outletAirMassFlowRate / m_massFlowAtSpeed[0];
                    } else {
                        state.dataAirLoop->AirLoopAFNInfo(AirLoopNum).AFNLoopOnOffFanRTF = 1.0;
                    }
                }
            }
        }
    }

    void FanSystem::report(EnergyPlusData &state)
    {
        m_fanEnergy = m_fanPower * state.dataHVACGlobal->TimeStepSys * DataGlobalConstants::SecInHour;
        m_deltaTemp = m_outletAirTemp - m_inletAirTemp;
    }

    Real64 FanSystem::fanPower() const
    {
        return m_fanPower;
    }

    Real64 FanSystem::powerLossToAir() const
    {
        return m_powerLossToAir;
    }

    Real64 FanSystem::maxAirMassFlowRate() const
    {
        return m_maxAirMassFlowRate;
    }

    Real64 FanSystem::getFanDesignTemperatureRise(EnergyPlusData &state) const
    {
        if (!m_objSizingFlag) {
            Real64 cpAir = Psychrometrics::PsyCpAirFnW(DataPrecisionGlobals::constant_zero);
            Real64 designDeltaT = (deltaPress / (m_rhoAirStdInit * cpAir * m_fanTotalEff)) * (m_motorEff + m_motorInAirFrac * (1.0 - m_motorEff));
            return designDeltaT;
        } else {
            // TODO throw warning, exception, call sizing?
            ShowWarningError(state, "FanSystem::getFanDesignTemperatureRise called before fan sizing completed ");
            return 0.0;
        }
    }

    Real64 FanSystem::getFanDesignHeatGain(EnergyPlusData &state, Real64 const FanVolFlow // fan volume flow rate [m3/s]
    )
    {
        if (!m_objSizingFlag) {
            Real64 fanPowerTot = (FanVolFlow * deltaPress) / m_fanTotalEff;
            Real64 designHeatGain = m_motorEff * fanPowerTot + (fanPowerTot - m_motorEff * fanPowerTot) * m_motorInAirFrac;
            return designHeatGain;
        } else {
            set_size(state);
            Real64 fanPowerTot = (FanVolFlow * deltaPress) / m_fanTotalEff;
            Real64 designHeatGain = m_motorEff * fanPowerTot + (fanPowerTot - m_motorEff * fanPowerTot) * m_motorInAirFrac;
            return designHeatGain;
        }
    }

    void FanSystem::FanInputsForDesignHeatGain(EnergyPlusData &state, Real64 &deltaP, Real64 &motEff, Real64 &totEff, Real64 &motInAirFrac)
    {
        if (!m_objSizingFlag) {
            deltaP = deltaPress;
            motEff = m_motorEff;
            totEff = m_fanTotalEff;
            motInAirFrac = m_motorInAirFrac;
            return;
        } else {
            set_size(state);
            deltaP = deltaPress;
            motEff = m_motorEff;
            totEff = m_fanTotalEff;
            motInAirFrac = m_motorInAirFrac;
            return;
        }
    }

    // void
    // FanSystem::fanIsSecondaryDriver()
    //{
    //    // this concept is used when the fan may be operating in a situation where there is airflow without it running at all
    //    // call this when some other fan is feeding the device containing this fan, making it a secondary fan.
    //    // example is the fan in a VS VAV air terminal used for UFAD.
    //    fanIsSecondaryDriver = true;
    //}

    // void
    // FanSystem::setFaultyFilterOn()
    //{
    //    // call this to set flag to direct model to use fault for filter
    //    faultyFilterFlag_ = true;
    //}

    // void
    // FanSystem::setFaultyFilterIndex( int const faultyAirFilterIndex  )
    //{
    //    // this is the index in the FaultsFouledAirFilters structure array in FaultsManager
    //    m_faultyFilterIndex = faultyAirFilterIndex;
    //}

} // namespace HVACFan

} // namespace EnergyPlus
