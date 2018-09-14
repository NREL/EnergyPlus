// EnergyPlus, Copyright (c) 1996-2018, The Board of Trustees of the University of Illinois,
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

// EnergyPlus Headers
#include <BranchNodeConnections.hh>
#include <DataHVACGlobals.hh>
#include <DataIPShortCuts.hh>
#include <DataLoopNode.hh>
#include <DataPlant.hh>
#include <DataPrecisionGlobals.hh>
#include <EMSManager.hh>
#include <FluidProperties.hh>
#include <InputProcessing/InputProcessor.hh>
#include <NodeInputManager.hh>
#include <OutputProcessor.hh>
#include <PlantLoadProfile.hh>
#include <PlantUtilities.hh>
#include <ScheduleManager.hh>
#include <UtilityRoutines.hh>

namespace EnergyPlus {

namespace PlantLoadProfile {
    // MODULE INFORMATION:
    //       AUTHOR         Peter Graham Ellis
    //       DATE WRITTEN   January 2004
    //       MODIFIED       Brent Griffith, plant rewrite, general fluid types
    //                      allow flow requests with out load requests
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS MODULE:
    // This module simulates a scheduled load profile on the demand side of the plant loop.

    // METHODOLOGY EMPLOYED:
    // The plant load profile object provides a scheduled load on the plant loop.  Unlike most plant equipment
    // on the demand side, i.e. zone equipment, this object does not have a zone associated with it.
    // For this reason the plant load profile can only be called for simulation by the non-zone equipment
    // manager (see NonZoneEquipmentManager.cc).

    // Using/Aliasing
    using namespace DataPrecisionGlobals;
    using DataGlobals::BeginEnvrnFlag;
    using DataPlant::PlantLoop;
    using DataPlant::TypeOf_PlantLoadProfile;
    using PlantUtilities::InitComponentNodes;
    using PlantUtilities::ScanPlantLoopsForObject;
    using PlantUtilities::SetComponentFlowRate;

    // MODULE VARIABLE DECLARATIONS:
    bool GetPlantLoadProfileInputFlag(true);
    int NumOfPlantProfile;

    // Object Data
    Array1D<PlantProfileData> PlantProfile;

    PlantComponent *PlantProfileData::factory(std::string objectName)
    {
        if (GetPlantLoadProfileInputFlag) {
            getPlantProfileInput();
            GetPlantLoadProfileInputFlag = false;
        }
        // Now look for this particular plant profile in the list
        for (auto &plp : PlantProfile) {
            if (plp.Name == objectName) {
                return &plp;
            }
        }
        // If we didn't find it, fatal
        ShowFatalError("PlantLoadProfile::factory: Error getting inputs for pipe named: " + objectName);
        return nullptr;
    }

    void PlantProfileData::onInitLoopEquip(const PlantLocation &EP_UNUSED(calledFromLocation))
    {
        this->initialize();
    }

    void PlantProfileData::clearOperatingVariables()
    {
        m_operatingVolumeFlowRate = 0.0;
        m_operatingMassFlowRate = 0.0;
        m_operatingPower = 0.0;
        m_operatingEnergy = 0.0;
        m_operatingCoolingEnergy = 0.0;
        m_operatingHeatingEnergy = 0.0;
        m_operatingInletTemperature = 0.0;
        m_operatingOutletTemperature = 0.0;
    }

    void PlantProfileData::simulate(const PlantLocation &EP_UNUSED(calledFromLocation),
                                    bool const EP_UNUSED(FirstHVACIteration),
                                    Real64 &EP_UNUSED(CurLoad),
                                    bool const EP_UNUSED(RunFlag))
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Peter Graham Ellis
        //       DATE WRITTEN   January 2004
        //       MODIFIED       Brent Griffith, generalize fluid cp
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Simulates the plant load profile object.

        // METHODOLOGY EMPLOYED:
        // This is a very simple simulation.  InitPlantProfile does the work of getting the scheduled load and flow rate.
        // Flow is requested and the actual available flow is set.  The outlet temperature is calculated.

        // USE STATEMENTS:

        // Using/Aliasing
        using FluidProperties::GetSpecificHeatGlycol;

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        static std::string const RoutineName("SimulatePlantProfile");
        Real64 DeltaTemp;

        this->initialize();
        this->calculate();
        this->update();
        this->report();

    } // simulate()

    void PlantProfileData::initialize()
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Peter Graham Ellis
        //       DATE WRITTEN   January 2004
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Initializes the plant load profile object during the plant simulation.

        // METHODOLOGY EMPLOYED:
        // Inlet and outlet nodes are initialized.  The scheduled load and flow rate is obtained, flow is requested, and the
        // actual available flow is set.

        // Using/Aliasing
        using DataGlobals::SysSizingCalc;
        using DataLoopNode::Node;
        using FluidProperties::GetDensityGlycol;
        using PlantUtilities::RegisterPlantCompDesignFlow;
        using ScheduleManager::GetCurrentScheduleValue;
        using ScheduleManager::GetScheduleMaxValue;

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        static std::string const RoutineName("InitPlantProfile");
        Real64 FluidDensityInit;
        bool errFlag;

        // clear all operating variables to ensure they are updated following the initialization
        clearOperatingVariables();

        // Do the one time initializations
        if (m_doOneTimeInitialization) {
            if (allocated(PlantLoop)) { // TODO: is this check needed?
                errFlag = false;
                ScanPlantLoopsForObject(
                    Name, m_plantProfileType, m_loopIndex, m_loopSideIndex, m_branchIndex, m_componentIndex, _, _, _, _, _, errFlag);
                if (errFlag) {
                    ShowFatalError("InitPlantProfile: Program terminated for previous conditions.");
                }

                m_doOneTimeInitialization = false;
            }
        }

        if (!SysSizingCalc && m_doInitSizing) {
            RegisterPlantCompDesignFlow(m_nodeInletIndex, m_peakVolumeFlowRate);
            m_doInitSizing = false;
        }

        if (m_doEnvironmentInitialization && BeginEnvrnFlag) {
            // Clear node initial conditions
            // DSU? can we centralize these temperature inits
            //    Node(InletNode)%Temp = 0.0
            Node(m_nodeOutletIndex).Temp = 0.0;

            FluidDensityInit =
                GetDensityGlycol(PlantLoop(m_loopIndex).FluidName, DataGlobals::InitConvTemp, PlantLoop(m_loopIndex).FluidIndex, RoutineName);

            Real64 const maxFlowMultiplier(GetScheduleMaxValue(m_flowRateFractionScheduleIndex));

            InitComponentNodes(0.0,
                               m_peakVolumeFlowRate * FluidDensityInit * maxFlowMultiplier,
                               m_nodeInletIndex,
                               m_nodeOutletIndex,
                               m_loopIndex,
                               m_loopSideIndex,
                               m_branchIndex,
                               m_componentIndex);

            m_emsHasMassFlowRateOverride = false;
            m_emsMassFlowRateOverride = 0.0;
            m_emsHasPowerOverride = false;
            m_emsPowerOverride = 0.0;
            m_doEnvironmentInitialization = false;
        }

        if (!BeginEnvrnFlag) {
            m_doEnvironmentInitialization = true;
        }

        // every iteration will initialize, set operating conditions
        m_operatingInletTemperature = Node(m_nodeInletIndex).Temp;

        if (m_emsHasPowerOverride) {
            m_operatingPower = m_emsPowerOverride;
        } else {
            m_operatingPower = GetCurrentScheduleValue(m_loadScheduleIndex);
        }

        FluidDensityInit =
            GetDensityGlycol(PlantLoop(m_loopIndex).FluidName, m_operatingInletTemperature, PlantLoop(m_loopIndex).FluidIndex, RoutineName);

        // Get the scheduled volume flow rate
        m_operatingVolumeFlowRate = m_peakVolumeFlowRate * GetCurrentScheduleValue(m_flowRateFractionScheduleIndex);

        if (m_emsHasMassFlowRateOverride) {
            m_operatingMassFlowRate = m_emsMassFlowRateOverride;
        } else {
            m_operatingMassFlowRate = m_operatingVolumeFlowRate * FluidDensityInit;
        }

        // Request the mass flow rate from the plant component flow utility routine
        SetComponentFlowRate(
            m_operatingMassFlowRate, m_nodeInletIndex, m_nodeOutletIndex, m_loopIndex, m_loopSideIndex, m_branchIndex, m_componentIndex);

        // back calculate the volume flow in case EMS has overridden mass flow rate
        m_operatingVolumeFlowRate = m_operatingMassFlowRate / FluidDensityInit;
    }

    void PlantProfileData::calculate()
    {
        static std::string const RoutineName("CalculatePlantProfile");

        // set the outlet temperature to the inlet temperature
        m_operatingOutletTemperature = m_operatingInletTemperature;

        if (m_operatingMassFlowRate > 0.0) {
            Real64 deltaTemperature;
            Real64 const Cp =
                GetSpecificHeatGlycol(PlantLoop(m_loopIndex).FluidName, m_operatingInletTemperature, PlantLoop(m_loopIndex).FluidIndex, RoutineName);

            deltaTemperature = m_operatingPower / (m_operatingMassFlowRate * Cp);
            m_operatingOutletTemperature -= deltaTemperature;
        } else {
            m_operatingPower = 0.0;
        }
    }

    void PlantProfileData::update()
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Peter Graham Ellis
        //       DATE WRITTEN   January 2004
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Updates the node variables with local variables.

        // METHODOLOGY EMPLOYED:
        // Standard EnergyPlus methodology.

        // Using/Aliasing
        using DataLoopNode::Node;
        using PlantUtilities::SafeCopyPlantNode;

        // copy data from inlet to outlet
        SafeCopyPlantNode(m_nodeInletIndex, m_nodeOutletIndex);

        // update the outlet temperature
        Node(m_nodeOutletIndex).Temp = m_operatingOutletTemperature;
    }

    void PlantProfileData::report()
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR         Peter Graham Ellis
        //       DATE WRITTEN   January 2004
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Calculates report variables.

        // METHODOLOGY EMPLOYED:
        // Standard EnergyPlus methodology.

        // Using/Aliasing
        using DataGlobals::SecInHour;
        using DataHVACGlobals::TimeStepSys;

        Real64 const reportingConstant(TimeStepSys * SecInHour); // constant for converting power to energy

        // update integrated values based on the operating conditions
        m_operatingEnergy = m_operatingPower * reportingConstant;

        if (m_operatingEnergy >= 0.0) {
            m_operatingHeatingEnergy = m_operatingEnergy;
            m_operatingCoolingEnergy = 0.0;
        } else {
            m_operatingHeatingEnergy = 0.0;
            m_operatingCoolingEnergy = std::abs(m_operatingEnergy);
        }
    }

    // Functions
    void getPlantProfileInput()
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Peter Graham Ellis
        //       DATE WRITTEN   January 2004
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Gets the plant load profile input from the input file and sets up the objects.

        // METHODOLOGY EMPLOYED:
        // Standard EnergyPlus methodology.

        // Using/Aliasing
        using BranchNodeConnections::TestCompSet;
        using DataGlobals::AnyEnergyManagementSystemInModel;
        using NodeInputManager::GetOnlySingleNode;
        using ScheduleManager::GetScheduleIndex;
        using namespace DataLoopNode;
        using namespace DataIPShortCuts; // Data for field names, blank numerics

        // Locals
        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        static bool ErrorsFound(false); // Set to true if errors in input, fatal at end of routine
        int IOStatus;                   // Used in GetObjectItem
        int NumAlphas;                  // Number of Alphas for each GetObjectItem call
        int NumNumbers;                 // Number of Numbers for each GetObjectItem call
        int ProfileNum;                 // PLANT LOAD PROFILE (PlantProfile) object number
        //  CHARACTER(len=MaxNameLength)   :: FoundBranchName
        //  INTEGER                        :: BranchControlType

        // FLOW:
        cCurrentModuleObject = "LoadProfile:Plant";
        NumOfPlantProfile = inputProcessor->getNumObjectsFound(cCurrentModuleObject);

        if (NumOfPlantProfile > 0) {
            PlantProfile.allocate(NumOfPlantProfile);

            for (ProfileNum = 1; ProfileNum <= NumOfPlantProfile; ++ProfileNum) {
                inputProcessor->getObjectItem(cCurrentModuleObject,
                                              ProfileNum,
                                              cAlphaArgs,
                                              NumAlphas,
                                              rNumericArgs,
                                              NumNumbers,
                                              IOStatus,
                                              lNumericFieldBlanks,
                                              _,
                                              cAlphaFieldNames,
                                              cNumericFieldNames);
                UtilityRoutines::IsNameEmpty(cAlphaArgs(1), cCurrentModuleObject, ErrorsFound);

                PlantProfile(ProfileNum).Name = cAlphaArgs(1);
                PlantProfile(ProfileNum).TypeNum = TypeOf_PlantLoadProfile; // parameter assigned in DataPlant !DSU

                PlantProfile(ProfileNum).InletNode = GetOnlySingleNode(
                    cAlphaArgs(2), ErrorsFound, cCurrentModuleObject, cAlphaArgs(1), NodeType_Water, NodeConnectionType_Inlet, 1, ObjectIsNotParent);
                PlantProfile(ProfileNum).OutletNode = GetOnlySingleNode(
                    cAlphaArgs(3), ErrorsFound, cCurrentModuleObject, cAlphaArgs(1), NodeType_Water, NodeConnectionType_Outlet, 1, ObjectIsNotParent);

                PlantProfile(ProfileNum).LoadSchedule = GetScheduleIndex(cAlphaArgs(4));

                if (PlantProfile(ProfileNum).LoadSchedule == 0) {
                    ShowSevereError(cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\"  The Schedule for " + cAlphaFieldNames(4) + " called " +
                                    cAlphaArgs(4) + " was not found.");
                    ErrorsFound = true;
                }

                PlantProfile(ProfileNum).PeakVolFlowRate = rNumericArgs(1);

                PlantProfile(ProfileNum).FlowRateFracSchedule = GetScheduleIndex(cAlphaArgs(5));

                if (PlantProfile(ProfileNum).FlowRateFracSchedule == 0) {
                    ShowSevereError(cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\"  The Schedule for " + cAlphaFieldNames(5) + " called " +
                                    cAlphaArgs(5) + " was not found.");

                    ErrorsFound = true;
                }

                // Check plant connections
                TestCompSet(cCurrentModuleObject, cAlphaArgs(1), cAlphaArgs(2), cAlphaArgs(3), cCurrentModuleObject + " Nodes");

                // Setup report variables
                SetupOutputVariable("Plant Load Profile Mass Flow Rate",
                                    OutputProcessor::Unit::kg_s,
                                    PlantProfile(ProfileNum).MassFlowRate,
                                    "System",
                                    "Average",
                                    PlantProfile(ProfileNum).Name);

                SetupOutputVariable("Plant Load Profile Heat Transfer Rate",
                                    OutputProcessor::Unit::W,
                                    PlantProfile(ProfileNum).Power,
                                    "System",
                                    "Average",
                                    PlantProfile(ProfileNum).Name);

                SetupOutputVariable("Plant Load Profile Heat Transfer Energy",
                                    OutputProcessor::Unit::J,
                                    PlantProfile(ProfileNum).Energy,
                                    "System",
                                    "Sum",
                                    PlantProfile(ProfileNum).Name,
                                    _,
                                    "ENERGYTRANSFER",
                                    "Heating",
                                    _,
                                    "Plant"); // is EndUseKey right?

                SetupOutputVariable("Plant Load Profile Heating Energy",
                                    OutputProcessor::Unit::J,
                                    PlantProfile(ProfileNum).HeatingEnergy,
                                    "System",
                                    "Sum",
                                    PlantProfile(ProfileNum).Name,
                                    _,
                                    "PLANTLOOPHEATINGDEMAND",
                                    "Heating",
                                    _,
                                    "Plant");

                SetupOutputVariable("Plant Load Profile Cooling Energy",
                                    OutputProcessor::Unit::J,
                                    PlantProfile(ProfileNum).CoolingEnergy,
                                    "System",
                                    "Sum",
                                    PlantProfile(ProfileNum).Name,
                                    _,
                                    "PLANTLOOPCOOLINGDEMAND",
                                    "Cooling",
                                    _,
                                    "Plant");

                if (AnyEnergyManagementSystemInModel) {
                    SetupEMSActuator("Plant Load Profile",
                                     PlantProfile(ProfileNum).Name,
                                     "Mass Flow Rate",
                                     "[kg/s]",
                                     PlantProfile(ProfileNum).EMSOverrideMassFlow,
                                     PlantProfile(ProfileNum).EMSMassFlowValue);
                    SetupEMSActuator("Plant Load Profile",
                                     PlantProfile(ProfileNum).Name,
                                     "Power",
                                     "[W]",
                                     PlantProfile(ProfileNum).EMSOverridePower,
                                     PlantProfile(ProfileNum).EMSPowerValue);
                }

                if (ErrorsFound) ShowFatalError("Errors in " + cCurrentModuleObject + " input.");

            } // ProfileNum
        }
    }

    void clear_state()
    {
        NumOfPlantProfile = 0;
        GetPlantLoadProfileInputFlag = true;
        PlantProfile.deallocate();
    }

} // namespace PlantLoadProfile

} // namespace EnergyPlus
