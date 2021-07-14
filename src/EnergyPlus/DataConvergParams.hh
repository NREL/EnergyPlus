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

#ifndef DataConvergParams_hh_INCLUDED
#define DataConvergParams_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>

// EnergyPlus Headers
#include <EnergyPlus/Data/BaseData.hh>
#include <EnergyPlus/DataGlobals.hh>
#include <EnergyPlus/EnergyPlus.hh>

namespace EnergyPlus {

namespace DataConvergParams {

    // Note: Unless otherwise noted, the tolerance parameters listed below were chosen
    // to represent educated guesses at what the tolerances for individual physical
    // parameters should be.
    constexpr Real64 HVACEnthalpyToler(260.0);                  // Tolerance for enthalpy comparisons (in kJ/kgK)
    constexpr Real64 HVACFlowRateToler(0.01);                   // Tolerance for mass flow rate convergence (in kg/s) [~20 CFM]
    constexpr Real64 HVACFlowRateSlopeToler(0.001);             // Slope tolerance for mass flow, kg/s/iteration
    constexpr Real64 HVACFlowRateOscillationToler(0.0000001);   // tolerance for detecting duplicate flow rate in stack
    constexpr Real64 HVACHumRatToler(0.0001);                   // Tolerance for humidity ratio comparisons (kg water/kg dryair)
    constexpr Real64 HVACHumRatSlopeToler(0.00001);             // Slope tolerance for humidity ratio, kg water/kg-dryair/iteration
    constexpr Real64 HVACHumRatOscillationToler(0.00000001);    // tolerance for detecting duplicate humidity ratio in stack
    constexpr Real64 HVACQualityToler(0.01);                    // Tolerance for fluid quality comparisons (dimensionless)
    constexpr Real64 HVACPressToler(10.0);                      // Tolerance for pressure comparisons (in Pascals)
    constexpr Real64 HVACTemperatureToler(0.01);                // Tolerance for temperature comparisons (in degrees C or K)
    constexpr Real64 HVACTemperatureSlopeToler(0.001);          // Slope tolerance for Temperature, Deg C/iteration
    constexpr Real64 HVACTemperatureOscillationToler(0.000001); // tolerance for detecting duplicate temps in stack
    constexpr Real64 HVACEnergyToler(10.0);                     // Tolerance for Energy comparisons (in Watts W)
    // to be consistent, should be 20.d0 (BG Aug 2012)

    constexpr Real64 HVACCpApprox(1004.844); // Air Cp (20C,0.0Kg/Kg) Only for energy Tolerance Calculation
    // Only used to scale the answer for a more intuitive answer for comparison

    constexpr Real64 PlantEnthalpyToler(0.10);  // Tolerance for enthalpy comparisons (in kJ/kgK)
    constexpr Real64 PlantFlowRateToler(0.001); // Tolerance for mass flow rate convergence (in kg/s) [~2 CFM]
    constexpr Real64 PlantFlowRateOscillationToler(0.0000001);
    constexpr Real64 PlantFlowRateSlopeToler(0.0001); // Slope tolerance for mass flow, kg/s/iteration

    constexpr Real64 PlantPressToler(10.0);                      // Tolerance for pressure comparisons (in Pascals)
    constexpr Real64 PlantTemperatureToler(0.01);                // Tolerance for temperature comparisons (in degrees C or K)
    constexpr Real64 PlantTemperatureSlopeToler(0.001);          // Slope tolerance for Temperature, Deg C/iteration
    constexpr Real64 PlantTemperatureOscillationToler(0.000001); // tolerance for detecting duplicate temps in stack

    constexpr Real64 PlantEnergyToler(10.0); // Tolerance for Energy comparisons (in Watts W)

    constexpr Real64 PlantCpApprox(4180.0); // Approximate Cp used in Interface manager for
    // Energy Tolerance Calculation, used to scale the answer
    // for a more intuitive answer for comparison
    constexpr Real64 PlantFlowFlowRateToler(0.01);    // Tolerance for mass flow rate convergence (in kg/s)
    constexpr Real64 PlantLowFlowRateToler(0.000001); // Tolerance for low flow rate used for determining when
    // plant pumps can be shut down

    constexpr int ConvergLogStackDepth(10);

    enum class iCalledFrom
    {
        AirSystemDemandSide,
        AirSystemSupplySideDeck1,
        AirSystemSupplySideDeck2
    };

    struct HVACNodeConvergLogStruct
    {
        // Members
        int NodeNum;
        Array1D<Real64> HumidityRatio;
        Array1D<Real64> MassFlowRate;
        Array1D<Real64> Temperature;

        // Default Constructor
        HVACNodeConvergLogStruct()
            : NodeNum(0), HumidityRatio(ConvergLogStackDepth), MassFlowRate(ConvergLogStackDepth), Temperature(ConvergLogStackDepth)
        {
        }
    };

    struct HVACZoneInletConvergenceStruct
    {
        // Members
        std::string ZoneName;
        int NumInletNodes; // number of inlet nodes for zone
        Array1D<HVACNodeConvergLogStruct> InletNode;

        // Default Constructor
        HVACZoneInletConvergenceStruct() : NumInletNodes(0)
        {
        }
    };

    struct HVACAirLoopIterationConvergenceStruct
    {
        // Members
        Array1D_bool HVACMassFlowNotConverged;                   // Flag to show mass flow convergence
        Array1D<Real64> HVACFlowDemandToSupplyTolValue;          // Queue of convergence "results"
        Array1D<Real64> HVACFlowSupplyDeck1ToDemandTolValue;     // Queue of convergence "results"
        Array1D<Real64> HVACFlowSupplyDeck2ToDemandTolValue;     // Queue of convergence "results"
        Array1D_bool HVACHumRatNotConverged;                     // Flag to show humidity ratio convergence   or failure
        Array1D<Real64> HVACHumDemandToSupplyTolValue;           // Queue of convergence "results"
        Array1D<Real64> HVACHumSupplyDeck1ToDemandTolValue;      // Queue of convergence "results"
        Array1D<Real64> HVACHumSupplyDeck2ToDemandTolValue;      // Queue of convergence "results"
        Array1D_bool HVACTempNotConverged;                       // Flag to show temperature convergence  or failure
        Array1D<Real64> HVACTempDemandToSupplyTolValue;          // Queue of convergence "results"
        Array1D<Real64> HVACTempSupplyDeck1ToDemandTolValue;     // Queue of convergence "results"
        Array1D<Real64> HVACTempSupplyDeck2ToDemandTolValue;     // Queue of convergence "results"
        Array1D_bool HVACEnergyNotConverged;                     // Flag to show energy convergence   or failure
        Array1D<Real64> HVACEnergyDemandToSupplyTolValue;        // Queue of convergence "results"
        Array1D<Real64> HVACEnergySupplyDeck1ToDemandTolValue;   // Queue of convergence "results"
        Array1D<Real64> HVACEnergySupplyDeck2ToDemandTolValue;   // Queue of convergence "results"
        Array1D_bool HVACEnthalpyNotConverged;                   // Flag to show energy convergence   or failure
        Array1D<Real64> HVACEnthalpyDemandToSupplyTolValue;      // Queue of convergence "results"
        Array1D<Real64> HVACEnthalpySupplyDeck1ToDemandTolValue; // Queue of convergence "results"
        Array1D<Real64> HVACEnthalpySupplyDeck2ToDemandTolValue; // Queue of convergence "results"
        Array1D_bool HVACPressureNotConverged;                   // Flag to show energy convergence   or failure
        Array1D<Real64> HVACPressureDemandToSupplyTolValue;      // Queue of convergence "results"
        Array1D<Real64> HVACPressureSupplyDeck1ToDemandTolValue; // Queue of convergence "results"
        Array1D<Real64> HVACPressueSupplyDeck2ToDemandTolValue;  // Queue of convergence "results"
        Array1D_bool HVACQualityNotConverged;                    // Flag to show energy convergence   or failure
        Array1D<Real64> HVACQualityDemandToSupplyTolValue;       // Queue of convergence "results"
        Array1D<Real64> HVACQualitSupplyDeck1ToDemandTolValue;   // Queue of convergence "results"
        Array1D<Real64> HVACQualitySupplyDeck2ToDemandTolValue;  // Queue of convergence "results"

        // Default Constructor
        HVACAirLoopIterationConvergenceStruct()
            : HVACMassFlowNotConverged(3, false), HVACFlowDemandToSupplyTolValue(ConvergLogStackDepth, 0.0),
              HVACFlowSupplyDeck1ToDemandTolValue(ConvergLogStackDepth, 0.0), HVACFlowSupplyDeck2ToDemandTolValue(ConvergLogStackDepth, 0.0),
              HVACHumRatNotConverged(3, false), HVACHumDemandToSupplyTolValue(ConvergLogStackDepth, 0.0),
              HVACHumSupplyDeck1ToDemandTolValue(ConvergLogStackDepth, 0.0), HVACHumSupplyDeck2ToDemandTolValue(ConvergLogStackDepth, 0.0),
              HVACTempNotConverged(3, false), HVACTempDemandToSupplyTolValue(ConvergLogStackDepth, 0.0),
              HVACTempSupplyDeck1ToDemandTolValue(ConvergLogStackDepth, 0.0), HVACTempSupplyDeck2ToDemandTolValue(ConvergLogStackDepth, 0.0),
              HVACEnergyNotConverged(3, false), HVACEnergyDemandToSupplyTolValue(ConvergLogStackDepth, 0.0),
              HVACEnergySupplyDeck1ToDemandTolValue(ConvergLogStackDepth, 0.0), HVACEnergySupplyDeck2ToDemandTolValue(ConvergLogStackDepth, 0.0),
              HVACEnthalpyNotConverged(3, false), HVACEnthalpyDemandToSupplyTolValue(ConvergLogStackDepth, 0.0),
              HVACEnthalpySupplyDeck1ToDemandTolValue(ConvergLogStackDepth, 0.0), HVACEnthalpySupplyDeck2ToDemandTolValue(ConvergLogStackDepth, 0.0),
              HVACPressureNotConverged(3, false), HVACPressureDemandToSupplyTolValue(ConvergLogStackDepth, 0.0),
              HVACPressureSupplyDeck1ToDemandTolValue(ConvergLogStackDepth, 0.0), HVACPressueSupplyDeck2ToDemandTolValue(ConvergLogStackDepth, 0.0),
              HVACQualityNotConverged(3, false), HVACQualityDemandToSupplyTolValue(ConvergLogStackDepth, 0.0),
              HVACQualitSupplyDeck1ToDemandTolValue(ConvergLogStackDepth, 0.0), HVACQualitySupplyDeck2ToDemandTolValue(ConvergLogStackDepth, 0.0)
        {
        }
    };

    struct PlantIterationConvergenceStruct
    {
        // Members
        bool PlantMassFlowNotConverged;                  // Flag to show mass flow convergence
        Array1D<Real64> PlantFlowDemandToSupplyTolValue; // Queue of convergence "results"
        Array1D<Real64> PlantFlowSupplyToDemandTolValue; // Queue of convergence "results"
        bool PlantTempNotConverged;                      // Flag to show temperature convergence (0) or failure (1)
        Array1D<Real64> PlantTempDemandToSupplyTolValue; // Queue of convergence "results"
        Array1D<Real64> PlantTempSupplyToDemandTolValue; // Queue of convergence "results"

        // Default Constructor
        PlantIterationConvergenceStruct()
            : PlantMassFlowNotConverged(false), PlantFlowDemandToSupplyTolValue(ConvergLogStackDepth, 0.0),
              PlantFlowSupplyToDemandTolValue(ConvergLogStackDepth, 0.0), PlantTempNotConverged(false),
              PlantTempDemandToSupplyTolValue(ConvergLogStackDepth, 0.0), PlantTempSupplyToDemandTolValue(ConvergLogStackDepth, 0.0)
        {
        }
    };

} // namespace DataConvergParams

struct ConvergParamsData : BaseGlobalStruct
{

    int AirLoopConvergFail = 0;

    Real64 MinTimeStepSys = (1.0 / 60.0); // =1 minute
    Real64 MinTimeStepTol = 1.0e-4;       // = min allowable for ABS(1.-TimeStepSys/(MinTimeStepSys))
    Real64 MaxZoneTempDiff = 0.3;         // 0.3 C = (1% OF 300 C) = max allowable difference between
    int MaxIter = 20;                     // maximum number of iterations allowed
    int MaxPlantSubIterations = 8;        // Iteration Max for Plant Simulation sub iterations
    int MinPlantSubIterations = 2;        // Iteration Min for Plant Simulation sub iterations

    // Object Data
    Array1D<DataConvergParams::HVACZoneInletConvergenceStruct> ZoneInletConvergence;
    Array1D<DataConvergParams::HVACAirLoopIterationConvergenceStruct> AirLoopConvergence;
    Array1D<DataConvergParams::PlantIterationConvergenceStruct> PlantConvergence;

    void clear_state() override
    {
        this->AirLoopConvergFail = 0;
        this->MinTimeStepSys = (1.0 / 60.0); // =1 minute
        this->MinTimeStepTol = 1.0e-4;       // = min allowable for ABS(1.-TimeStepSys/(MinTimeStepSys))
        this->MaxZoneTempDiff = 0.3;         // 0.3 C = (1% OF 300 C) = max allowable difference between
        this->MaxIter = 20;                  // maximum number of iterations allowed
        this->MaxPlantSubIterations = 8;     // Iteration Max for Plant Simulation sub iterations
        this->MinPlantSubIterations = 2;     // Iteration Min for Plant Simulation sub iterations
        this->ZoneInletConvergence.deallocate();
        this->AirLoopConvergence.deallocate();
        this->PlantConvergence.deallocate();
    }
};

} // namespace EnergyPlus

#endif
