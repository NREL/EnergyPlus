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

#ifndef Boilers_hh_INCLUDED
#define Boilers_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>

// EnergyPlus Headers
#include <DataGlobals.hh>
#include <EnergyPlus.hh>
#include <PlantComponent.hh>

namespace EnergyPlus {

namespace Boilers {

    // Using/Aliasing

    // Data
    // MODULE PARAMETER DEFINITIONS

    // Boiler normalized efficiency curve types
    enum class EfficiencyCurveType
    {
        NotSet,
        Linear,
        BiLinear,
        Quadratic,
        BiQuadratic,
        Cubic,
        QuadraticLinear,
        BiCubic,
        Default = NotSet,
    };

    // water temperature evaluation method
    enum class TemperatureEvaluationModeType
    {
        NotSet,
        Entering,
        Leaving,
        Default = NotSet,
    };

    // Boiler flow modes
    enum class FlowModeType
    {
        NotSet,
        Constant,
        NotModulated,
        LeavingSetPointModulated,
        Default = NotSet,
    };

    // DERIVED TYPE DEFINITIONS

    // MODULE VARIABLE DECLARATIONS:
    extern int NumBoilers; // Number of boilers

    // SUBROUTINE SPECIFICATIONS FOR MODULE Boilers

    // Types
    class BoilerObject : public PlantComponent
    {
        // Members
    public:
        virtual ~BoilerObject()
        {
        }

        std::string Name; // user identifier
        Real64 m_operatingLoad;                                         // W - Boiler Load
        Real64 m_operatingHeatingEnergy;                                // J - Boiler energy integrated over time
        Real64 m_operatingParasiticElectricalPower;                     // W - Parasitic electrical power (e.g. forced draft fan)
        Real64 m_operatingParasiticElectricalConsumption;               // J - Parasitic Electrical Consumption (e.g. forced draft fan)
        Real64 m_operatingMassFlowRate;                                 // kg/s - Boiler mass flow rate
        Real64 m_operatingInletTemperature;                             // C - Boiler inlet temperature
        Real64 m_operatingOutletTemperature;                            // C - Boiler outlet temperature
        Real64 m_operatingPartLoadRatio;                                // Boiler operating part-load ratio
        Real64 m_operatingFuelUseRate;                                  // W - Boiler fuel used
        Real64 m_operatingFuelUse;                                      // J - Boiler Fuel consumed integrated over time

        // member functions
        static PlantComponent *factory(std::string objectName);
        static void getBoilerInput();

        void simulate(const PlantLocation &calledFromLocation,
                      bool const FirstHVACIteration,
                      Real64 &CurLoad,
                      bool const RunFlag,
                      int const EquipFlowCtrl) override;

        void getDesignCapacities(const PlantLocation &EP_UNUSED(calledFromLocation), Real64 &MaxLoad, Real64 &MinLoad, Real64 &OptLoad) override;

        void onInitLoopEquip(const PlantLocation &EP_UNUSED(calledFromLocation)) override;

        void getSizingFactor(Real64 &SizFac) override;

        void doSizing();

        // public getters/setters
        Real64 getDesignNominalCapacity();
        Real64 getDesignVolumeFlowRate();
        Real64 getFaultyBoilerFoulingFactor();

        void setDesignNominalCapacity(Real64 const capacity);
        void setDesignVolumeFlowRate(Real64 const flowRate);

        void setFaultyBoilerFoulingFactorIndex(Real64 const index);

        // set the sizing factor for the boiler, if sizingFactor <= 0.0, it is set to 1.0
        void setDesignSizingFactor(Real64 const sizingFactor);
        void setLoopNumber(int const loopNumber);
        void setDesignOutletTemperature(Real64 const temperature);

        // set the outlet limit temperature, if temperature <= 0.0, set it to 99.9
        void setDesignOutletTemperatureLimit(Real64 const temperature);

        // Default Constructor
        BoilerObject()
            : m_fuelType(0), m_boilerTypeEnumerator(0), m_loopIndex(0), m_loopSideIndex(0), m_branchIndex(0), m_componentIndex(0),
              m_designNominalCapacity(0.0), m_designNominalCapacityWasAutoSized(false), m_designEfficiency(0.0),
              m_designFlowMode(FlowModeType::Default), m_outletSetpointMissingError(false), m_outletSetpointMissingErrorDone(false),
              m_designVolumeFlowRate(0.0), m_designVolumeFlowRateWasAutoSized(false), m_designMassFlowRate(0.0), m_designSizingFactor(0.0),
              m_nodeHotWaterInletIndex(0), m_nodeHotWaterOutletIndex(0), m_designMinPartLoadRatio(0.0), m_designMaxPartLoadRatio(0.0),
              m_designOptimalPartLoadRatio(0.0), m_efficiencyCurveTemperatureMode(TemperatureEvaluationModeType::Default), m_curveEfficiencyIndex(0),
              m_efficiencyCurveType(EfficiencyCurveType::Default), m_designOutletTemperatureLimit(0.0), m_designParasiticElectricalLoad(0.0),
              m_efficiencyCurveOutputError(0), m_efficiencyCurveOutputErrorIndex(0), m_calculatedEfficiencyError(0),
              m_calculatedEfficiencyErrorIndex(0), m_doOneTimeInitialisation(true), m_doEnvironmentInitialisation(true),
              m_hasFaultyBoilerFoulingFactor(false), m_faultyBoilerFoulingFactorIndex(0), m_operatingLoad(0.0), m_operatingHeatingEnergy(0.0),
              m_operatingParasiticElectricalPower(0.0), m_operatingParasiticElectricalConsumption(0.0), m_operatingMassFlowRate(0.0),
              m_operatingInletTemperature(0.0), m_operatingPartLoadRatio(0.0), m_operatingFuelUseRate(0.0), m_operatingFuelUse(0.0)
        {
        }

    private:
        int m_fuelType;                                                 // resource type assignment
        int m_boilerTypeEnumerator;                                     // plant loop type identifier
        int m_loopIndex;                                                // plant loop connection
        int m_loopSideIndex;                                            // plant loop side connection
        int m_branchIndex;                                              // plant loop branch connection
        int m_componentIndex;                                           // plant loop component connection
        Real64 m_designNominalCapacity;                                 // W - design nominal capacity of Boiler
        bool m_designNominalCapacityWasAutoSized;                       // true if previous was set to autosize input
        Real64 m_designEfficiency;                                      // boiler efficiency at design conditions
        FlowModeType m_designFlowMode;                                  // one of 3 modes for componet flow during operation
        bool m_outletSetpointMissingError;                              // True if the setpoint is missing at the outlet node
        bool m_outletSetpointMissingErrorDone;                          // true if setpoint warning issued
        Real64 m_designVolumeFlowRate;                                  // m3/s - Boiler water design volumetric flow rate
        bool m_designVolumeFlowRateWasAutoSized;                        // true if previous was set to autosize input
        Real64 m_designMassFlowRate;                                    // kg/s - Boiler water design mass flow rate
        Real64 m_designSizingFactor;                                    // sizing factor
        int m_nodeHotWaterInletIndex;                                   // Node number at the boiler inlet
        int m_nodeHotWaterOutletIndex;                                  // Node number at the boiler outlet
        Real64 m_designMinPartLoadRatio;                                // Minimum allowed operating part load ratio
        Real64 m_designMaxPartLoadRatio;                                // Maximum allowed operating part load ratio
        Real64 m_designOptimalPartLoadRatio;                            // Optimal operating part load ratio
        TemperatureEvaluationModeType m_efficiencyCurveTemperatureMode; // water temp to use in curve, switch between entering and leaving
        int m_curveEfficiencyIndex;                                     // Index to efficiency curve
        EfficiencyCurveType m_efficiencyCurveType;                      // Type of efficiency curve
        Real64 m_designOutletTemperatureLimit;                          // C - Boiler outlet maximum temperature limit
        Real64 m_designParasiticElectricalLoad;                         // W - Parasitic electric power (e.g. forced draft fan)
        int m_efficiencyCurveOutputError;                               // efficiency curve output <=0 recurring warning error counter
        int m_efficiencyCurveOutputErrorIndex;                          // efficiency curve output <=0 recurring warning error message index
        int m_calculatedEfficiencyError;                                // calculated efficiency >1.1 recurring warning error counter
        int m_calculatedEfficiencyErrorIndex;                           // calculated efficiency >1.1 recurring warning error message index
        std::string m_endUseSubcategory;                                // identifier use for the end use subcategory
        bool m_doOneTimeInitialisation;                                 // do the one time initialisation, i.e. locate on plantloops etc.
        bool m_doEnvironmentInitialisation;                             // do the environment initialisation, i.e. get inlet conditions etc.
        bool m_hasFaultyBoilerFoulingFactor;                            // True if the boiler has fouling fault
        int m_faultyBoilerFoulingFactorIndex;                           // Index of the fault object corresponding to the boiler

        void initialise(); // number of the current boiler being simulated

        void calculate(Real64 const MyLoad,    // W - hot water demand to be met by boiler
                       bool const RunFlag,     // TRUE if boiler operating
                       int const EquipFlowCtrl // Flow control mode for the equipment
        );

        bool hasTwoVariableEfficiencyCurve();

        void clearOperatingVariables();

        // transfer data from the boiler to outlet nodes
        void update();

        // update all reporting variables
        void report();
    };

    // Object Data
    extern Array1D<BoilerObject> Boiler; // boiler data - dimension to number of machines

    void clear_state();

} // namespace Boilers

} // namespace EnergyPlus

#endif
