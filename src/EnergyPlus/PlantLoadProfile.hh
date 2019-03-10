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

#ifndef PlantLoadProfile_hh_INCLUDED
#define PlantLoadProfile_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>

// EnergyPlus Headers
#include <DataGlobals.hh>
#include <EnergyPlus.hh>
#include <PlantComponent.hh>

namespace EnergyPlus {

namespace PlantLoadProfile {
    // MODULE VARIABLE DECLARATIONS:
    extern int NumOfPlantProfile;

    // Types
    class PlantProfileObject : public PlantComponent
    {
        // Members
    public:
        virtual ~PlantProfileObject()
        {
        }

        std::string Name;                    // Name of Plant Load Profile object
        bool m_emsHasPowerOverride;    // if true, then EMS is calling to override power level
        Real64 m_emsPowerOverride;     // value EMS is directing to use for power [W]
        bool m_emsHasMassFlowRateOverride;
        Real64 m_emsMassFlowRateOverride;
        Real64 m_operatingPower;             // Power required to meet the load (W)
        Real64 m_operatingEnergy;            // Energy required to meet the load (J)
        Real64 m_operatingHeatingEnergy;     // Heating Energy required to meet the load (J)
        Real64 m_operatingCoolingEnergy;     // Cooling Energy required to meet the load (J)
        Real64 m_operatingInletTemperature;  // Inlet temperature (C)
        Real64 m_operatingOutletTemperature; // Outlet temperature (C)
        Real64 m_operatingVolumeFlowRate;    // Volumetric flow rate (m3/s)
        Real64 m_operatingMassFlowRate;      // Mass flow rate (kg/s)

        // member functions
        static PlantComponent *factory(std::string objectName);
        static void getPlantProfileInput();

        void simulate(const PlantLocation &calledFromLocation, bool const FirstHVACIteration, Real64 &CurLoad, bool const RunFlag) override;

        void onInitLoopEquip(const PlantLocation &calledFromLocation) override;

        // Default Constructor
        PlantProfileObject()
            : m_emsHasPowerOverride(false), m_emsPowerOverride(0.0), m_emsHasMassFlowRateOverride(false), m_emsMassFlowRateOverride(0.0),
              m_operatingPower(0.0), m_operatingEnergy(0.0), m_operatingHeatingEnergy(0.0), m_operatingCoolingEnergy(0.0),
              m_operatingInletTemperature(0.0), m_operatingOutletTemperature(0.0), m_operatingVolumeFlowRate(0.0), m_operatingMassFlowRate(0.0),
              m_loopIndex(0), m_loopSideIndex(0), m_branchIndex(0), m_componentIndex(0), m_doOneTimeInitialization(true),
              m_doEnvironmentInitialization(true), m_doInitSizing(true), m_nodeInletIndex(0), m_nodeOutletIndex(0), m_loadScheduleIndex(0),
              m_peakVolumeFlowRate(0.0), m_flowRateFractionScheduleIndex(0)
        {
        }

    private:
        int m_plantProfileType;        // Plant Side Connection: 'TypeOf_Num' assigned in DataPlant  !DSU
        int m_loopIndex;       // water plant loop index number                      !DSU
        int m_loopSideIndex;   // water plant loop side index                        !DSU
        int m_branchIndex; // water plant loop branch index                      !DSU
        int m_componentIndex;   // water plant loop component index                   !DSU
        bool m_doOneTimeInitialization;
        bool m_doEnvironmentInitialization;          // Flag for initialization:  TRUE means do the init
        bool m_doInitSizing;    // Flag for initialization of plant sizing
        int m_nodeInletIndex;
        int m_nodeOutletIndex;
        int m_loadScheduleIndex;         // Pointer to schedule object
        Real64 m_peakVolumeFlowRate;   // Peak volumetric flow rate, also water consumption rate (m3/s)
        int m_flowRateFractionScheduleIndex; // Pointer to schedule object

        void initialize();

        void update();

        void report();

        void clearOperatingVariables();

        void calculate();
    };

    // Object Data
    extern Array1D<PlantProfileObject> PlantProfile;

    void clear_state();

} // namespace PlantLoadProfile

} // namespace EnergyPlus

#endif
