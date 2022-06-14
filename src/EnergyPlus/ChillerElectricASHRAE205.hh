// EnergyPlus, Copyright (c) 1996-2022, The Board of Trustees of the University of Illinois,
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

#ifndef ChillerElectricASHRAE205_hh_INCLUDED
#define ChillerElectricASHRAE205_hh_INCLUDED

#include <ObjexxFCL/Array1D.hh>

#include <EnergyPlus/Data/BaseData.hh>
#include <EnergyPlus/DataGlobals.hh>
#include <EnergyPlus/EnergyPlus.hh>
#include <EnergyPlus/PlantChillers.hh>
#include <EnergyPlus/Plant/DataPlant.hh>

namespace EnergyPlus {

    struct EnergyPlusData;

    namespace ChillerElectricASHRAE205 {
        struct ASHRAE205ChillerSpecs : PlantChillers::BaseChillerSpecs {
            // Members
            Real64 TempLowLimitEvapOut;                // C - low temperature shut off
            Real64 DesignHeatRecVolFlowRate;           // m3/s, Design Water mass flow rate through heat recovery loop
            bool DesignHeatRecVolFlowRateWasAutoSized; // true if previous was input autosize.
            Real64 DesignHeatRecMassFlowRate;          // kg/s, Design Water mass flow rate through heat recovery loop
            bool HeatRecActive;                        // True entered Heat Rec Vol Flow Rate >0
            int HeatRecInletNodeNum;                   // Node number on the heat recovery inlet side of the condenser
            int HeatRecOutletNodeNum;                  // Node number on the heat recovery outlet side of the condenser
            Real64 HeatRecCapacityFraction;            // user input for heat recovery capacity fraction []
            Real64 HeatRecMaxCapacityLimit;            // Capacity limit for Heat recovery, one time calc [W]
            int HeatRecSetPointNodeNum;                // index for system node with the heat recover leaving setpoint
            int HeatRecInletLimitSchedNum;             // index for schedule for the inlet high limit for heat recovery operation
            PlantLocation HRPlantLoc;                  // heat recovery water plant loop component index
            std::string EndUseSubcategory;             // identifier use for the end use subcategory
            Real64 CondOutletHumRat;                   // kg/kg - condenser outlet humditiy ratio, air side
            Real64 ActualCOP;
            Real64 HeatRecInletTemp;
            Real64 HeatRecOutletTemp;
            Real64 HeatRecMdot;
            Real64 ChillerCondAvgTemp; // the effective condenser temperature for chiller performance [C]

            // Default Constructor
            ASHRAE205ChillerSpecs()
                    : TempLowLimitEvapOut(0.0), DesignHeatRecVolFlowRate(0.0),
                      DesignHeatRecVolFlowRateWasAutoSized(false), DesignHeatRecMassFlowRate(0.0), HeatRecActive(false),
                      HeatRecInletNodeNum(0),
                      HeatRecOutletNodeNum(0), HeatRecCapacityFraction(0.0), HeatRecMaxCapacityLimit(0.0),
                      HeatRecSetPointNodeNum(0),
                      HeatRecInletLimitSchedNum(0), HRPlantLoc{}, CondOutletHumRat(0.0), ActualCOP(0.0),
                      HeatRecInletTemp(0.0), HeatRecOutletTemp(0.0), HeatRecMdot(0.0), ChillerCondAvgTemp(0.0) {
            }

            static void getInput(EnergyPlusData &state);

            void setOutputVariables(EnergyPlusData &state);

            static ASHRAE205ChillerSpecs *factory(EnergyPlusData &state, std::string const &chillerName);

            void simulate([[maybe_unused]] EnergyPlusData &state,
                          const PlantLocation &calledFromLocation,
                          bool FirstHVACIteration,
                          Real64 &CurLoad,
                          bool RunFlag) override;

            void initialize(EnergyPlusData &state, bool RunFlag, Real64 MyLoad) override;

            void size(EnergyPlusData &state) override;

            void calculate(EnergyPlusData &state,
                           Real64 &MyLoad,                                   // operating load
                           bool RunFlag                                      // TRUE when chiller operating
            );

            void update(EnergyPlusData &state,
                        Real64 MyLoad, // current load
                        bool RunFlag   // TRUE if chiller operating
            );

            void oneTimeInit(EnergyPlusData &state) override;
        };
    } // namespace ChillerElectricASHRAE205

    struct ChillerElectricASHRAE205Data : BaseGlobalStruct
    {
        bool getInputFlag = true;
        Array1D<ChillerElectricASHRAE205::ASHRAE205ChillerSpecs> Electric205Chiller;

        void clear_state() override
        {
            *this = ChillerElectricASHRAE205Data();
        }
    };

} // namespace EnergyPlus
#endif //ChillerElectricASHRAE205_hh_INCLUDED
