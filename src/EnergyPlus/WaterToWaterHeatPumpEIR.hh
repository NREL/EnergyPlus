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

#ifndef ENERGYPLUS_WATERTOWATERHEATPUMPEIR_HH
#define ENERGYPLUS_WATERTOWATERHEATPUMPEIR_HH

#include <functional>
#include <string>
#include <vector>

#include <PlantComponent.hh>
#include <Plant/PlantLocation.hh>
#include <WaterToWaterHeatPumps.hh>

namespace EnergyPlus {

    namespace EIRWaterToWaterHeatPumps {

        struct InOutNodePair {
            int inlet;
            int outlet;

            InOutNodePair() : inlet(0), outlet(0) {
            }
        };

        struct EIRWaterToWaterHeatPump : public EnergyPlus::BaseWaterToWaterHeatPump {

            // fixed configuration parameters
            std::string name = "";
            int plantTypeOfNum = -1;
            std::string companionCoilName = "";
            EIRWaterToWaterHeatPump *companionHeatPumpCoil = nullptr;
            Real64 sizingFactor = 1.0;

            // reference data
            Real64 referenceCapacity = 0.0;
            bool referenceCapacityWasAutoSized = false;
            Real64 referenceCOP = 0.0;

            // curve references
            int capFuncTempCurveIndex = 0;
            int powerRatioFuncTempCurveIndex = 0;
            int powerRatioFuncPLRCurveIndex = 0;

            // flow rate terms
            Real64 loadSideDesignVolFlowRate = 0.0;
            bool loadSideDesignVolFlowRateWasAutoSized = false;
            Real64 sourceSideDesignVolFlowRate = 0.0;
            bool sourceSideDesignVolFlowRateWasAutoSized = false;
            Real64 loadSideDesignMassFlowRate = 0.0;
            Real64 sourceSideDesignMassFlowRate = 0.0;
            Real64 loadSideMassFlowRate = 0.0;
            Real64 sourceSideMassFlowRate = 0.0;

            // simulation variables
            Real64 loadSideHeatTransfer = 0.0;
            Real64 sourceSideHeatTransfer = 0.0;
            Real64 loadSideInletTemp = 0.0;
            Real64 loadSideOutletTemp = 0.0;
            Real64 sourceSideInletTemp = 0.0;
            Real64 sourceSideOutletTemp = 0.0;
            Real64 powerUsage = 0.0;
            Real64 loadSideEnergy = 0.0;
            Real64 sourceSideEnergy = 0.0;
            Real64 powerEnergy = 0.0;
            bool running = false;

            // topology variables
            PlantLocation loadSideLocation;
            PlantLocation sourceSideLocation;
            InOutNodePair loadSideNodes;
            InOutNodePair sourceSideNodes;

            // counters and indexes
            int condMassFlowRateTriggerIndex = 0;
            int recurringConcurrentOperationWarningIndex = 0;

            // logic flags
            bool oneTimeInit = true;
            bool envrnInit = true;

            // a couple worker functions to easily allow merging of cooling and heating operations
            std::function<Real64 (Real64, Real64)> calcLoadOutletTemp;
            std::function<Real64 (Real64, Real64)> calcQsource;
            std::function<Real64 (Real64, Real64)> calcSourceOutletTemp;

            virtual ~EIRWaterToWaterHeatPump() = default;

            EIRWaterToWaterHeatPump() = default;

            void simulate(const PlantLocation &calledFromLocation, bool FirstHVACIteration, Real64 &CurLoad,
                          bool RunFlag) override;

            void onInitLoopEquip(const PlantLocation &EP_UNUSED(calledFromLocation)) override;

            void getDesignCapacities(const PlantLocation &EP_UNUSED(calledFromLocation),
                                     Real64 &EP_UNUSED(MaxLoad),
                                     Real64 &EP_UNUSED(MinLoad),
                                     Real64 &EP_UNUSED(OptLoad)) override;

            void doPhysics(Real64 currentLoad);

            void size();

            Real64 getLoadSideOutletSetPointTemp();

            void setOperatingFlowRates();

            void resetReportingVariables();

            static PlantComponent *factory(int wwhp_type_of_num, std::string eir_wwhp_name);

            static void pairUpCompanionCoils();

            static void processInputForEIRWWHP();

            static void clear_state();

            static void checkConcurrentOperation();

            static Real64 add(Real64 const a, Real64 const b) {return a + b;}

            static Real64 subtract(Real64 const a, Real64 const b) {return a - b;}
        };

        extern std::vector<EIRWaterToWaterHeatPump> eir_wwhp;
    } // namespace EIRWaterToWaterHeatPumps
} // namespace EnergyPlus

#endif // ENERGYPLUS_WATERTOWATERHEATPUMPEIR_HH
