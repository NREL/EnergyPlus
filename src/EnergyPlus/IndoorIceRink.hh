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

#ifndef IceRink_hh_INCLUDED
#define IceRink_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>

// EnergyPlus Headers
#include <EnergyPlus/Data/BaseData.hh>
#include <EnergyPlus/DataGlobals.hh>
#include <EnergyPlus/EnergyPlus.hh>
#include <EnergyPlus/Plant/DataPlant.hh>
#include <EnergyPlus/PlantComponent.hh>

namespace EnergyPlus {

struct EnergyPlusData;

namespace IceRink {

    enum class SystemType
    {
        Direct,
        Indirect
    };
    enum class FluidType
    {
        CaCl2 [[maybe_unused]],
        E6
    };
    enum class ControlType
    {
        SurfaceTemp,
        BrineOutletTemp
    };

    struct Coefficients
    {
        Real64 Ca = 0.0;
        Real64 Cb = 0.0;
        Real64 Cc = 0.0;
        Real64 Cd = 0.0;
        Real64 Ce = 0.0;
        Real64 Cf = 0.0;
        Real64 Cg = 0.0;
        Real64 Ch = 0.0;
        Real64 Ci = 0.0;
        Real64 Cj = 0.0;
        Real64 Cl = 0.0;
        Real64 Ck = 0.0;
        Real64 Area = 0.0;
    };

    struct IceRinkData : PlantComponent
    {
        Coefficients coeffs;
        std::string Name; // User identifier
        int RinkType_Num = DataPlant::TypeOf_IceRink;
        std::string SchedName;          // availability schedule
        int SchedPtr = 0;               // index to schedule
        std::string ZoneName;           // Name of zone the system is present
        int ZonePtr = 0;                // Pointer to the zone in which the floor radiant system is present
        std::string SurfaceName;        // surface name of rink
        int SurfacePtr = 0;             // index to surface array
        int PeopleHeatGainSchedPtr = 0; // People schedule index
        std::string PeopleSchedName;    // Name of people schedule
        int PeopleSchedPtr = 0;         // People schedule index
        Real64 PeopleHeatGain = 0.0;    // Current heat gain from people
        Real64 NumOfPeople = 0.0;
        Real64 MaxNumOfPeople; // Number of people in the rink as defined by user input
        Real64 FloodWaterTemp;
        Real64 IceTemp;
        int NumOfSurfaces = 0;     // Total number of surfaces in the ice rink arena
        Real64 DesignMassFlowRate; // Design flow rate through the rink HX [kg/s]

        Real64 hrstofreeze;                                         // Desired hours to freeze the water of the ice rink
        Real64 deltatemp;                                           // Design delta T for heat exchanger
        Real64 maxmdot;                                             // maximum mass flowrate
        ControlType ControlStrategy = ControlType::BrineOutletTemp; // Control strategy for the ice rink (BOTC or STC)

        // Rink geometry details
        Real64 LengthRink;
        Real64 WidthRink;
        Real64 WaterTemp; // water temperature before freezing to ice rink
        Real64 IceThickness;
        Real64 TubeLength;       // Length of the piping used in the floor radiant system
        Real64 TubeDiameter;     // Diameter of the piping used in the floor radiant system
        int NumCircuits = 1;     // Number of fluid circuits in the floor radiant system / a value of 1 is only for testing
        Real64 TotalSurfaceArea; // Surface area of the rink

        // loop topology variables
        int LoopNum = 0;
        int LoopSide = 0;
        int BranchNum = 0;
        int CompNum = 0;

        std::string RefrigInNode;  // Inlet node for the cold refrigerant entring the floor radiant system
        int InNode = 0;            // Refrigerant inlet node number
        int OutNode = 0;           // Refrigerant outlet node number
        int IceSetptSchedPtr = 0;  // Pointer to set point temperature of ice surface
        std::string IceSetptSched; // Set point schedule name for STC

        // Data specific to direct systems
        int RefrigIndex = 0; // Index to refrigerant properties used in direct system

        // Data specific to indirect systems
        int GlycolIndex = 0; // Index to secondary refrigerant used in indirect system
        Real64 RefrigConc;

        bool MyFlag = true;
        bool MyEnvrnFlag = true;
        Real64 IceTemperature = 0.0;
        Real64 Tsurfin1 = 0.0;           // inside surface temperature in previous timestep.
        Real64 Qsource2 = 0.0;           // Q used to report data after calling heat balance. This is used to find any irregularities.
        Real64 Qsetpoint = 0.0;          // Required Q to bring the inside surface temperature to ice setpoint.
        Real64 Qsrcmax = 0.0;            // maximum Q to freeze water. This is used to calculate the design load and maximum mass flowrate.
        Real64 Qsrcmax2 = 0.0;           // maximum Q based on CTFs
        Real64 Tsrc = 0.0;               // Source Temperature. Only used to track simulation results.
        Real64 Effectiveness = 0.0;      // effectiveness of HX
        Real64 ReqMassFlow = 0.0;        // Required mass flowarate
        Real64 CpRefrig = 0.0;           // Specific heat of refrigerant
        Real64 RhoWater = 0.0;           // density of water
        Real64 CpWater = 0.0;            // Specific heat of water
        Real64 Q = 0.0;                  // Final Q to be reported to EnergyPlus - using eqn. from low radiant temp system
        Real64 Q2 = 0.0;                 // using eqn. 16
        Real64 Q3 = 0.0;                 // using eqn. 11
        Real64 TRefigOutCheck = 0.0;     // Outlet temperature of refrigerant
        Real64 RefrigTempIn = 0.0;       // Inlet temperature of refrigerant
        Real64 RefrigMassFlow = 0.0;     // Refrigerant mass flow rate
        Real64 PastRefrigMassFlow = 0.0; // to keep track of previous mass flow rate
        Real64 LoadMet = 0.0;
        Real64 COP = 0.0;             // hardwired COP. This will be changed later. default is 2.5
        Real64 IceSetPointTemp = 0.0; // Ice rink setpoint
        Real64 operation = 0.0;       // used to determine whether the ice rink is ON or OFF. 1: ON; 0: OFF
        Real64 circuits = 0.0;
        Real64 HXSpacing = 0.0;
        Real64 DesignSetPoint = 0.0;
        Real64 ResurfWaterTemp = 0.0; // in C and from ASHRAE - 60 default
        Real64 InitialWaterTemp = 0.0;
        Real64 ResurfTank = 0.0; // in m3 - 0.55m3 default is from ASHRAE capacity range
        Real64 QResurface = 0.0;
        Real64 HeatingWater = 0.0;

        Real64 FreezingLoad = 0.0;

        // ReportData
        Real64 CoolPower = 0.0;       // Cooling sent to rink floor in Watts
        Real64 CoolEnergy = 0.0;      // Cooling sent to rink floor in Joules

        // Members
        std::string NameR; // Resurfacer Name
        int CompIndex = 0;
        int WaterIndex = 0;
        std::string ResurfacingSchedName;
        int ResurfacingSchedPtr = 0;
        int ResurfaceON; // TODO: Should be bool?

        Real64 InletTemp;
        Real64 OutletTemp;
        Real64 FlowRate;

        // Report Data
        Real64 ResurfacingHeatLoad = 0.0;
        Real64 EHeatingWater = 0.0;
        Real64 ResurfacingWaterTemp = 0.0;
        Real64 TankCapacity = 0.0;

        void
        simulate(EnergyPlusData &state, const PlantLocation &calledFromLocation, bool FirstHVACIteration, Real64 &CurLoad, bool RunFlag) override;

        void oneTimeInit(EnergyPlusData &state) override;

        void initialize(EnergyPlusData &state);

        void setupOutputVariables(EnergyPlusData &state);

        Real64 IceRinkFreezing(EnergyPlusData &state);

        Real64 calcEffectiveness(EnergyPlusData &state, Real64 Temperature, Real64 lRefrigMassFlow) const;

        void calculateIceRink(EnergyPlusData &state, Real64 &LoadMet);

        void report(EnergyPlusData &state);

        static PlantComponent *factory(EnergyPlusData &state, std::string const &objectName);
    };

    void GetIndoorIceRink(EnergyPlusData &state);

} // namespace IceRink

struct IceRinkData : BaseGlobalStruct
{
    Array1D<IceRink::IceRinkData> Rink;
    int NumOfRinks = 0;
    int NumOfResurfacers = 0;
    bool FirstTimeInit = true;
    bool GetInput = true;
    Array1D<Real64> QRadSysSrcAvg;      // Average source over the time step for a particular radiant surface
    Array1D<Real64> LastQRadSysSrc;     // Need to keep the last value in case we are still iterating
    Array1D<Real64> LastSysTimeElapsed; // Need to keep the last value in case we are still iterating
    Array1D<Real64> LastTimeStepSys;    // Need to keep the last value in case we are still iterating

    void clear_state() override
    {
        *this = IceRinkData();
    }
};
} // namespace EnergyPlus

#endif
