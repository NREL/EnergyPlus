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
#include <EnergyPlus/DataGlobals.hh>
#include <EnergyPlus/EnergyPlus.hh>
#include <EnergyPlus/PlantComponent.hh>
#include <EnergyPlus/Data/EnergyPlusData.hh>

namespace EnergyPlus {

namespace IceRink {

    // Data
    // MODULE PARAMETER DEFINITIONS:
    // System types:
    extern int const DirectSystem;
    extern int const IndirectSystem;
    extern std::string const cDRink;
    extern std::string const cIRink;

    // Fluid types in indirect refrigeration system
    extern int const CaCl2;
    extern int const EG;

    // Control types:
    extern int const SurfaceTempControl;
    extern int const BrineOutletTempControl;

    // Operating Mode:
    extern int NotOperating; // Parameter for use with OperatingMode variable, set for not operating
    extern int CoolingMode;  // Parameter for use with OperatingMode variable, set for cooling

    extern int NumOfRinks;
    extern int NumOfResurfacers;

    struct IceRinkData : PlantComponent
    {
        std::string Name; // User identifier
        int RinkType_Num;
        std::string SchedName;               // availability schedule
        int SchedPtr;                        // index to schedule
        std::string ZoneName;                // Name of zone the system is present
        int ZonePtr;                         // Pointer to the zone in which the floor radiant system is present
        std::string SurfaceName;             // surface name of rink
        int SurfacePtr;                      // index to surface array
        int PeopleHeatGainSchedPtr;          // People schedule index
        std::string PeopleHeatGainSchedName; // Name of people heat gain schedule
        std::string PeopleSchedName;         // Name of people schedule
        int PeopleSchedPtr;                  // People schedule index
        Real64 PeopleHeatGain;               // Current heat gain from people
        Real64 NumOfPeople;
        Real64 TotalPeopleHG;
        Real64 MaxNumOfPeople; // Number of people in the rink as defined by user input
        int WaterIndex;
        Real64 FloodWaterTemp;
        int NumOfSurfaces;         // Total number of surfaces in the ice rink arena
        Real64 DesignMassFlowRate; // Design flow rate through the rink HX [kg/s]
        Real64 DesignCapacity;     // design cooling capacity
        Real64 MinRefrigMassFlow;  // Minimum mass flow rate of refrigerant allowed in the floor radiant system(kg/s)
        Real64 MaxRefrigMassFlow;  // Miximum mass flow rate of refrigerant allowed in the floor radiant system(kg/s)
        int ControlStrategy;       // Control strategy for the ice rink (BOTC or STC)

        // Rink geometry details
        Real64 LengthRink;
        Real64 WidthRink;
        Real64 DepthRink;
        Real64 IceThickness;
        Real64 TubeLength;       // Length of the piping used in the floor radiant system
        Real64 TubeDiameter;     // Diameter of the piping used in the floor radiant system
        int NumCircuits;         // Number of fluid circuits in the floor radiant system
        Real64 TotalSurfaceArea; // Surface area of the rink

        // loop topology variables
        int LoopNum;
        int LoopSide;
        int BranchNum;
        int CompNum;

        std::string RefrigInNode;     // Inlet node for the cold refrigerant entring the floor radiant system
        std::string RefrigOutNode;    // Outlet node for the hot refrigerant exiting the floor radiant system
        int InNode;                   // Refrigerant inlet node number
        int OutNode;                  // Refrigerant outlet node number
        int RefrigSetptSchedPtr;      // Pointer to set point temperature of refrigerant outlet
        int IceSetptSchedPtr;         // Pointer to set point temperature of ice surface
        Real64 RefrigSetptTemp;       // Set point temperature for refrigerant outlet
        Real64 IceSetptTemp;          // Set point temperature for ice surface
        std::string RefrigSetptSched; // Set point schedule name for BOTC
        std::string IceSetptSched;    // Set point schedule name for STC

        // Data specific to direct systems
        int RefrigIndex; // Index to refrigerant properties used in direct system

        // Data specific to indirect systems
        int GlycolIndex; // Index to secondary refrigerant used in indirect system
        int RefrigType;
        Real64 RefrigConc;

        bool MyFlag;
        bool MyEnvrnFlag;
        Real64 QSrc;
        Real64 QSrcAvg;
        Real64 LastQSrc;
        Real64 LastSysTimeElapsed;
        Real64 LastTimeStepSys;

        // ReportData
        Real64 RefrigInletTemp;  // Refrigerant inlet temperature
        Real64 RefrigOutletTemp; // Refrigerant outlet temperature
        Real64 RefrigMassFlow;   // Refrigerant mass flow rate in the floor radiant system(Kg/s)
        Real64 CoolPower;        // Cooling sent to rink floor in Watts
        Real64 CoolEnergy;       // Cooling sent to rink floor in Joules

        // Default Constructor
        IceRinkData() : RinkType_Num(0)
        {
        }
        //void simulate(const PlantLocation &calledFromLocation, bool FirstHVACIteration, Real64 &CurLoad, bool RunFlag);
        void simulate(EnergyPlusData &EP_UNUSED(state), const PlantLocation &calledFromLocation, bool FirstHVACIteration, Real64 &CurLoad, bool RunFlag) override;
        //void simulate(EnergyPlusData &EP_UNUSED(state), const PlantLocation &calledFromLocation, bool FirstHVACIteration, Real64 &CurLoad, bool RunFlag) override;
        //void simulate(EnergyPlusData &EP_UNUSED(state), const PlantLocation &calledFromLocation, bool FirstHVACIteration, Real64 &CurLoad, bool RunFlag) override;
        void initialize();

        void setupOutputVariables();

        Real64 PeopleHG();

        Real64 IceRinkFreezing(Real64 &FreezingLoad);

        Real64 calcEffectiveness(Real64 const Temperature, Real64 const RefrigMassFlow);

        void calculateIceRink(Real64 &LoadMet);

        void update();

        void report(bool RunFlag);

        static PlantComponent *factory(std::string const &objectName);
    };

    struct ResurfacerData
    {
        // Members
        std::string Name;
        int CompIndex;
        int WaterIndex;
        std::string ResurfacingSchedName;
        int ResurfacingSchedPtr;
        int NoOfResurfEvents;
        Real64 ResurfacingWaterTemp;
        Real64 InitWaterTemp;
        Real64 TankCapacity; // in m3
        Real64 IceTemperature;

        // Report Data
        Real64 ResurfacingHeatLoad;
        Real64 QResurfacing;
        Real64 EHeatingWater;
        // Default Constructor
        ResurfacerData()
            : CompIndex(0), WaterIndex(0), ResurfacingSchedPtr(0), NoOfResurfEvents(0), ResurfacingWaterTemp(0.0), InitWaterTemp(0.0),
              TankCapacity(0.0), QResurfacing(0.0), EHeatingWater(0.0), ResurfacingHeatLoad(0.0)
        {
        }

        // Function
        Real64 RinkResurfacer(Real64 &ResurfacingLoad);
    };

    // Object Data:
    extern Array1D<IceRinkData> Rink;
    extern Array1D<ResurfacerData> Resurfacer;

    // Functions:

    void clear_state();

    void GetIndoorIceRink();

    void GetResurfacer();

} // namespace IceRink
} // namespace EnergyPlus

#endif
