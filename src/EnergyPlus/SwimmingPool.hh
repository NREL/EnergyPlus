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

#ifndef SwimmingPool_hh_INCLUDED
#define SwimmingPool_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>

// EnergyPlus Headers
#include <EnergyPlus/DataGlobals.hh>
#include <EnergyPlus/EnergyPlus.hh>
#include <EnergyPlus/PlantComponent.hh>

namespace EnergyPlus {

namespace SwimmingPool {

    extern int NumSwimmingPools; // Number of swimming pools

    struct SwimmingPoolData : PlantComponent
    {
        // Members
        // Input data
        std::string Name;                // name of swimming pool
        std::string SurfaceName;         // surface name of pool
        int SurfacePtr;                  // index to surface array
        std::string ZoneName;            // Name of zone the pool is in
        int ZonePtr;                     // Pointer to this zone in the Zone derived type
        std::string WaterInletNodeName;  // water inlet node name
        int WaterInletNode;              // water inlet node number
        std::string WaterOutletNodeName; // water outlet node name
        int WaterOutletNode;             // water outlet node number
        int HWLoopNum;
        int HWLoopSide;
        int HWBranchNum;
        int HWCompNum;
        Real64 WaterVolFlowMax;                 // maximum water flow rate for pool, m3/s
        Real64 WaterMassFlowRateMax;            // maximum water mass flow rate for pool, kg/s
        Real64 AvgDepth;                        // average depth of the pool, m
        Real64 ActivityFactor;                  // Activity factor for the pool
        std::string ActivityFactorSchedName;    // Activity factor schedule name
        int ActivityFactorSchedPtr;             // Activity factor schedule pointer
        Real64 CurActivityFactor;               // Current activity factor value
        std::string MakeupWaterSupplyName;      // Name of make-up water source
        std::string MakeupWaterSupplySchedName; // Name of make-up water supply schedule
        int MakeupWaterSupplySchedPtr;          // Index to schedule for make-up water
        Real64 CurMakeupWaterTemp;              // Current makeup water temperature
        std::string CoverSchedName;             // Pool cover schedule name
        int CoverSchedPtr;                      // Index to pool cover schedule
        Real64 CurCoverSchedVal;                // Current cover schedule value based on schedule
        Real64 CoverEvapFactor;                 // Pool cover evaporation factor
        Real64 CoverConvFactor;                 // Pool cover convective factor
        Real64 CoverSWRadFactor;                // Pool cover short-wavelength radiation factor
        Real64 CoverLWRadFactor;                // Pool cover long-wavelength radiation factor
        Real64 CurCoverEvapFac;                 // Current pool cover evaporation factor
        Real64 CurCoverConvFac;                 // Current pool cover convective factor
        Real64 CurCoverSWRadFac;                // Current pool cover short-wavelength radiation factor
        Real64 CurCoverLWRadFac;                // Current pool cover long-wavelength radiation factor
        Real64 RadConvertToConvect;             // LW and SW radiation converted to convective gain by pool cover in W/m2
        Real64 MiscPowerFactor;                 // Pool miscellaneous power equipment consumption coefficient in W/(kg/s)
        std::string SetPtTempSchedName;         // Schedule name for water setpoint temperature
        int SetPtTempSchedPtr;                  // Schedule pointer for water setpoint temperature
        Real64 CurSetPtTemp;                    // Current water setpoint temperature
        Real64 MaxNumOfPeople;                  // Number of people in the pool as defined by user input
        std::string PeopleSchedName;            // Name of people schedule
        int PeopleSchedPtr;                     // People schedule index
        std::string PeopleHeatGainSchedName;    // Name of people heat gain schedule
        int PeopleHeatGainSchedPtr;             // People heat gain schedule index
        Real64 PeopleHeatGain;                  // Current heat gain from people
        int GlycolIndex;                        // index in fluid property routines for water
        Real64 WaterMass;                       // pool water mass
        Real64 SatPressPoolWaterTemp;           // Saturation pressure at the pool water temperature
        Real64 PartPressZoneAirTemp;            // Partial pressure of water vapor in the air
        // Report data
        Real64 PoolWaterTemp;           // Average pool water temperature
        Real64 WaterInletTemp;          // water inlet temperature
        Real64 WaterOutletTemp;         // water outlet temperature
        Real64 WaterMassFlowRate;       // water mass flow rate
        Real64 MakeUpWaterMassFlowRate; // makeup water flow rate (addition to the pool)
        Real64 MakeUpWaterMass;         // makeup water mass added to pool
        Real64 MakeUpWaterVolFlowRate;  // makeup water volume flow rate
        Real64 MakeUpWaterVol;          // makeup water volume added to pool
        Real64 HeatPower;               // heating sent to pool in Watts
        Real64 HeatEnergy;              // heating sent to pool in Joules
        Real64 MiscEquipPower;          // power for miscellaneous pool equipment in Watts
        Real64 MiscEquipEnergy;         // energy for miscellaneous pool equipment in Joules
        Real64 RadConvertToConvectRep;  // LW and SW radiation converted to convective gain by pool cover (reporting) in W
        Real64 EvapHeatLossRate;        // Heat lost due to evaporation of pool water as a rate in Watts
        Real64 EvapEnergyLoss;          // Energy lost due to evaporation in Joules
        bool MyOneTimeFlag;
        bool MyEnvrnFlagGeneral;
        bool MyPlantScanFlagPool;
        Array1D_int SurfaceToPoolIndex;
        Array1D<Real64> QPoolSrcAvg;          // Average source over the time step for a particular radiant surface
        Array1D<Real64> HeatTransCoefsAvg;    // Average denominator term over the time step for a particular pool
        Array1D<Real64> ZeroSourceSumHATsurf; // Equal to SumHATsurf for all the walls in a zone with no source
        // Record keeping variables used to calculate QRadSysSrcAvg locally
        Array1D<Real64> LastQPoolSrc;       // Need to keep the last value in case we are still iterating
        Array1D<Real64> LastHeatTransCoefs; // Need to keep the last value in case we are still iterating
        Array1D<Real64> LastSysTimeElapsed; // Need to keep the last value in case we are still iterating
        Array1D<Real64> LastTimeStepSys;    // Need to keep the last value in case we are still iterating

        // Default Constructor
        SwimmingPoolData()
            : SurfacePtr(0), ZonePtr(0), WaterInletNode(0), WaterOutletNode(0), HWLoopNum(0), HWLoopSide(0), HWBranchNum(0), HWCompNum(0),
              WaterVolFlowMax(0.0), WaterMassFlowRateMax(0.0), AvgDepth(0.0), ActivityFactor(0.0), ActivityFactorSchedPtr(0), CurActivityFactor(0.0),
              MakeupWaterSupplySchedPtr(0), CurMakeupWaterTemp(0.0), CoverSchedPtr(0), CurCoverSchedVal(0.0), CoverEvapFactor(0.0),
              CoverConvFactor(0.0), CoverSWRadFactor(0.0), CoverLWRadFactor(0.0), CurCoverEvapFac(0.0), CurCoverConvFac(0.0), CurCoverSWRadFac(0.0),
              CurCoverLWRadFac(0.0), RadConvertToConvect(0.0), MiscPowerFactor(0.0), SetPtTempSchedPtr(0), CurSetPtTemp(23.0), MaxNumOfPeople(0.0),
              PeopleSchedPtr(0), PeopleHeatGainSchedPtr(0), PeopleHeatGain(0.0), GlycolIndex(0), WaterMass(0.0), SatPressPoolWaterTemp(0.0),
              PartPressZoneAirTemp(0.0), PoolWaterTemp(23.0), WaterInletTemp(0.0), WaterOutletTemp(0.0), WaterMassFlowRate(0.0),
              MakeUpWaterMassFlowRate(0.0), MakeUpWaterMass(0.0), MakeUpWaterVolFlowRate(0.0), MakeUpWaterVol(0.0), HeatPower(0.0), HeatEnergy(0.0),
              MiscEquipPower(0.0), MiscEquipEnergy(0.0), RadConvertToConvectRep(0.0), EvapHeatLossRate(0.0), EvapEnergyLoss(0.0), MyOneTimeFlag(true),
              MyEnvrnFlagGeneral(true), MyPlantScanFlagPool(true)
        {
        }

        void simulate(const PlantLocation &calledFromLocation, bool FirstHVACIteration, Real64 &CurLoad, bool RunFlag) override;

        void initialize(bool FirstHVACIteration // true during the first HVAC iteration
        );

        void setupOutputVars();

        void initSwimmingPoolPlantLoopIndex();

        void initSwimmingPoolPlantNodeFlow(bool MyPlantScanFlagPool // logical flag true when plant index has not yet been set
        );

        void calculate();

        void calcSwimmingPoolEvap(Real64 &EvapRate, // Evaporation rate
                                  int SurfNum,      // Surface index
                                  Real64 MAT,       // mean air temperature
                                  Real64 HumRat     // zone air humidity ratio
        );

        void update();
    };

    // Object Data
    extern Array1D<SwimmingPoolData> Pool;

    void clear_state();

    void GetSwimmingPool();

    void SimSwimmingPool(bool FirstHVACIteration);

    void UpdatePoolSourceValAvg(bool &SwimmingPoolOn); // .TRUE. if the swimming pool has "run" this zone time step

    Real64 SumHATsurf(int ZoneNum); // Zone number

    void ReportSwimmingPool();

    Real64 MakeUpWaterVolFlowFunct(Real64 MakeUpWaterMassFlowRate, Real64 Density);

    Real64 MakeUpWaterVolFunct(Real64 MakeUpWaterMass, Real64 Density);

} // namespace SwimmingPool

} // namespace EnergyPlus

#endif
