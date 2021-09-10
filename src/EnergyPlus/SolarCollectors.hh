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

#ifndef SolarCollectors_hh_INCLUDED
#define SolarCollectors_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>
#include <ObjexxFCL/Optional.hh>

// EnergyPlus Headers
#include <EnergyPlus/DataGlobals.hh>
#include <EnergyPlus/EnergyPlus.hh>
#include <EnergyPlus/PlantComponent.hh>

namespace EnergyPlus {

// Forward declarations
struct EnergyPlusData;

namespace SolarCollectors {

    enum struct FluidEnum
    {
        WATER,
        AIR
    };

    enum struct TestTypeEnum
    {
        INLET,
        AVERAGE,
        OUTLET
    };

    enum struct TankTypeEnum
    {
        ICSRectangularTank
    };

    struct ParametersData
    {
        // Members
        std::string Name;                      // Name of solar collector parameters
        Real64 Area;                           // Gross area of collector (m2)
        FluidEnum TestFluid;                   // Test fluid (only WATER for now)
        Real64 TestMassFlowRate;               // Test volumetric flow rate (m3/s)
        TestTypeEnum TestType;                 // Test correlation type (INLET | AVERAGE | OUTLET)
        Real64 eff0;                           // Coefficient 1 of efficiency equation (Y-intercept)
        Real64 eff1;                           // Coefficient 2 of efficiency equation (1st order)
        Real64 eff2;                           // Coefficient 3 of efficiency equation (2nd order)
        Real64 iam1;                           // Coefficient 2 of incident angle modifier (1st order)
        Real64 iam2;                           // Coefficient 3 of incident angle modifier (2nd order)
        TankTypeEnum ICSType_Num;              // ICS collector type
        Real64 Volume;                         // collector water net volume (m3)
        Real64 SideHeight;                     // collector side height (m)
        Real64 ThermalMass;                    // thermal mass of the absorber plate (J/m2C)
        Real64 ULossSide;                      // heat loss conductance for collector side (W/m2C)
        Real64 ULossBottom;                    // heat loss conductance for collector bottom (W/m2C)
        Real64 AspectRatio;                    // collector aspect ratio (dimensionless)
        int NumOfCovers;                       // number of transparent collector covers
        Real64 CoverSpacing;                   // collector cover spacings (m)
        Array1D<Real64> RefractiveIndex;       // refractive idex of inner and outer covers (dimensionless)
        Array1D<Real64> ExtCoefTimesThickness; // extinction coefficient times thickness of covers (dimensionless)
        Array1D<Real64> EmissOfCover;          // emissivity of inner and outer covers (dimensionless)
        Real64 EmissOfAbsPlate;                // emissivity Of absorber plate (dimensionless)
        Real64 AbsorOfAbsPlate;                // absorptance of the absorber plate (dimensionless)

        // Default Constructor
        ParametersData()
            : Area(0.0), TestFluid(FluidEnum::WATER), TestMassFlowRate(0.0), TestType(TestTypeEnum::INLET), eff0(0.0), eff1(0.0), eff2(0.0),
              iam1(0.0), iam2(0.0), ICSType_Num(TankTypeEnum::ICSRectangularTank), Volume(0.0), SideHeight(0.0), ThermalMass(0.0), ULossSide(0.0),
              ULossBottom(0.0), AspectRatio(0.0), NumOfCovers(0), CoverSpacing(0.0), RefractiveIndex(2, 0.0), ExtCoefTimesThickness(2, 0.0),
              EmissOfCover(2, 0.0), EmissOfAbsPlate(0.0), AbsorOfAbsPlate(0.0)
        {
        }

        Real64 IAM(EnergyPlusData &state, Real64 IncidentAngle // Angle of incidence (radians)
        );
    };

    struct CollectorData : PlantComponent
    {
        // Members
        std::string Name;         // Name of solar collector
        std::string BCType;       // Boundary condition Type
        std::string OSCMName;     // OtherSideConditionsModel
        int VentCavIndex;         // index of ventilated cavity object
        TankTypeEnum ICSType_Num; // ICS collector type number
        int TypeNum;              // Plant Side Connection: 'TypeOf_Num' assigned in DataPlant
        int WLoopNum;             // Water plant loop index number
        int WLoopSideNum;         // Water plant loop side index
        int WLoopBranchNum;       // Water plant loop branch index
        int WLoopCompNum;         // Water plant loop component index
        bool Init;                // Flag for initialization:  TRUE means do the init
        bool InitSizing;          // Flag for initialization of plant sizing
        int Parameters;           // Parameters object number
        int Surface;              // Surface object number
        int InletNode;            // Inlet node
        Real64 InletTemp;         // Inlet temperature from plant (C)
        int OutletNode;           // Outlet node
        Real64 OutletTemp;        // Outlet temperature or stagnation temperature in the collector (C)
        Real64 MassFlowRate;      // Mass flow rate through the collector (kg/s)
        Real64 MassFlowRateMax;   // Maximum mass flow rate through the collector (kg/s)
        Real64 VolFlowRateMax;    // Maximum volumetric flow rate through the collector (m3/s)
        int ErrIndex;             // Error index for recurring error
        int IterErrIndex;         // Error index for recurring error (iteration - did not converge)
        // Report variables
        Real64 IncidentAngleModifier; // Net incident angle modifier
        Real64 Efficiency;            // Thermal efficiency of solar energy conversion
        Real64 Power;                 // Heat gain or loss to collector fluid (W)
        Real64 HeatGain;              // Heat gain to collector fluid (W)
        Real64 HeatLoss;              // Heat loss from collector fluid (W)
        Real64 Energy;                // Energy gained (or lost) to collector fluid (J)
        // Report variables
        Real64 HeatRate;           // Collector useful Heat gain rate [W]
        Real64 HeatEnergy;         // Collector useful Heat gain energy [J]
        Real64 StoredHeatRate;     // net heat gain or loss rate of the collector fluid [W]
        Real64 StoredHeatEnergy;   // net heat gain or loss energy of the collector fluid [J]
        Real64 HeatGainRate;       // Collector useful Heat gain rate [W]
        Real64 HeatGainEnergy;     // Collector useful Heat gain energy (J)
        Real64 HeatLossRate;       // collector useful heat loss rate [W]
        Real64 HeatLossEnergy;     // Collector useful Heat loss energy [J]
        Real64 SkinHeatLossRate;   // collector skin heat loss rate [W]
        Real64 CollHeatLossEnergy; // collector skin heat loss energy[J]
        Real64 TauAlpha;           // Transmittance-absorptance product total radiation
        Real64 UTopLoss;           // Over all top loss coefficient [W/m2.C]
        Real64 TempOfWater;        // average temperature of the collector water [C]
        Real64 TempOfAbsPlate;     // average temperature of the abs plate [C]
        Real64 TempOfInnerCover;   // temperature of the collector inner cover [C]
        Real64 TempOfOuterCover;   // temperature of the collector inner cover [C]
        // Data from elsewhere and calculated
        Real64 TauAlphaNormal;               // Transmittance-absorptance product normal radiation
        Real64 TauAlphaSkyDiffuse;           // Transmittance-absorptance product sky diffuse radiation
        Real64 TauAlphaGndDiffuse;           // Transmittance-absorptance product grn diffuse radiation
        Real64 TauAlphaBeam;                 // Transmittance-absorptance product beam radiation
        Array1D<Real64> CoversAbsSkyDiffuse; // sky diffuse solar absorptance of cover
        Array1D<Real64> CoversAbsGndDiffuse; // ground diffuse solar absorptance of cover
        Array1D<Real64> CoverAbs;            // solar rad weighted covers absorptance
        Real64 TimeElapsed;                  // Fraction of the current hour that has elapsed (h)
        // Saved in order to identify the beginning of a new system time
        Real64 UbLoss;                 // Over all bottom loss coefficient [W/m2C]
        Real64 UsLoss;                 // Over all side loss coefficient [W/m2C]
        Real64 AreaRatio;              // Side area to collector area ratio [-]
        Real64 RefSkyDiffInnerCover;   // Sky diffuse refl of inner cover (cover 1)
        Real64 RefGrnDiffInnerCover;   // ground diffuse refl of inner cover (cover 1)
        Real64 RefDiffInnerCover;      // diffuse reflectance of the inner cover (cover 1) from bottom
        Real64 SavedTempOfWater;       // water temp carried from time step to time step [C]
        Real64 SavedTempOfAbsPlate;    // abs plate temp carried from time step to time step [C]
        Real64 SavedTempOfInnerCover;  // inner cover temp carried from time step to time step [C]
        Real64 SavedTempOfOuterCover;  // outer cover temp carried from time step to time step [C]
        Real64 SavedTempCollectorOSCM; // Temperature of collector back from OSCM at previous time step [C]
        Real64 Length;                 // characteristic length of the abs plate
        Real64 TiltR2V;                // collector tilt angle from the vertical [degree]
        Real64 Tilt;                   // collector tilt angle from the horizontal [degree]
        Real64 CosTilt;                // cosine of colector tilt angle [-]
        Real64 SinTilt;                // sine of 1.8 times colector tilt angle [-]
        Real64 SideArea;               // weighted collector side area (m2)
        Real64 Area;                   // collector area (m2)
        Real64 Volume;                 // collector net volume (m3)
        bool OSCM_ON;                  // Boundary condition is OSCM
        bool InitICS;                  // used to initialize ICS variables only
        bool SetLoopIndexFlag;
        bool SetDiffRadFlag;

        // Default Constructor
        CollectorData()
            : VentCavIndex(0), ICSType_Num(TankTypeEnum::ICSRectangularTank), TypeNum(0), WLoopNum(0), WLoopSideNum(0), WLoopBranchNum(0),
              WLoopCompNum(0), Init(true), InitSizing(true), Parameters(0), Surface(0), InletNode(0), InletTemp(0.0), OutletNode(0), OutletTemp(0.0),
              MassFlowRate(0.0), MassFlowRateMax(0.0), VolFlowRateMax(0.0), ErrIndex(0), IterErrIndex(0), IncidentAngleModifier(0.0), Efficiency(0.0),
              Power(0.0), HeatGain(0.0), HeatLoss(0.0), Energy(0.0), HeatRate(0.0), HeatEnergy(0.0), StoredHeatRate(0.0), StoredHeatEnergy(0.0),
              HeatGainRate(0.0), HeatGainEnergy(0.0), HeatLossRate(0.0), HeatLossEnergy(0.0), SkinHeatLossRate(0.0), CollHeatLossEnergy(0.0),
              TauAlpha(0.0), UTopLoss(0.0), TempOfWater(0.0), TempOfAbsPlate(0.0), TempOfInnerCover(0.0), TempOfOuterCover(0.0), TauAlphaNormal(0.0),
              TauAlphaSkyDiffuse(0.0), TauAlphaGndDiffuse(0.0), TauAlphaBeam(0.0), CoversAbsSkyDiffuse(2, 0.0), CoversAbsGndDiffuse(2, 0.0),
              CoverAbs(2, 0.0), TimeElapsed(0.0), UbLoss(0.0), UsLoss(0.0), AreaRatio(0.0), RefSkyDiffInnerCover(0.0), RefGrnDiffInnerCover(0.0),
              RefDiffInnerCover(0.0), SavedTempOfWater(0.0), SavedTempOfAbsPlate(0.0), SavedTempOfInnerCover(0.0), SavedTempOfOuterCover(0.0),
              SavedTempCollectorOSCM(0.0), Length(1.0), TiltR2V(0.0), Tilt(0.0), CosTilt(0.0), SinTilt(0.0), SideArea(0.0), Area(0.0), Volume(0.0),
              OSCM_ON(false), InitICS(false), SetLoopIndexFlag(true), SetDiffRadFlag(true)
        {
        }

        static PlantComponent *factory(EnergyPlusData &state, std::string const &objectName);

        void setupOutputVars(EnergyPlusData &state);

        void initialize(EnergyPlusData &state);

        void simulate([[maybe_unused]] EnergyPlusData &state,
                      const PlantLocation &calledFromLocation,
                      bool FirstHVACIteration,
                      Real64 &CurLoad,
                      bool RunFlag) override;

        void CalcTransRefAbsOfCover(EnergyPlusData &state,
                                    Real64 IncidentAngle,              // Angle of incidence (radians)
                                    Real64 &TransSys,                  // cover system solar transmittance
                                    Real64 &ReflSys,                   // cover system solar reflectance
                                    Real64 &AbsCover1,                 // Inner cover solar absorbtance
                                    Real64 &AbsCover2,                 // Outer cover solar absorbtance
                                    Optional_bool_const InOUTFlag = _, // flag for calc. diffuse solar refl of cover from inside out
                                    Optional<Real64> RefSysDiffuse = _ // cover system solar reflectance from inner to outer cover
        );

        void CalcSolarCollector(EnergyPlusData &state);

        void CalcICSSolarCollector(EnergyPlusData &state);

        void CalcTransAbsorProduct(EnergyPlusData &state, Real64 IncidAngle);

        void CalcHeatTransCoeffAndCoverTemp(EnergyPlusData &state);

        static void ICSCollectorAnalyticalSolution(EnergyPlusData &state,
                                                   Real64 SecInTimeStep,     // seconds in a time step
                                                   Real64 a1,                // coefficient of ODE for Tp
                                                   Real64 a2,                // coefficient of ODE for Tp
                                                   Real64 a3,                // coefficient of ODE for Tp
                                                   Real64 b1,                // coefficient of ODE for TW
                                                   Real64 b2,                // coefficient of ODE for TW
                                                   Real64 b3,                // coefficient of ODE for TW
                                                   Real64 TempAbsPlateOld,   // absorber plate temperature at previous time step [C]
                                                   Real64 TempWaterOld,      // collector water temperature at previous time step [C]
                                                   Real64 &TempAbsPlate,     // absorber plate temperature at current time step [C]
                                                   Real64 &TempWater,        // collector water temperature at current time step [C]
                                                   bool AbsorberPlateHasMass // flag for absorber thermal mass
        );

        static Real64 CalcConvCoeffBetweenPlates(Real64 TempSurf1, // temperature of surface 1
                                                 Real64 TempSurf2, // temperature of surface 1
                                                 Real64 AirGap,    // characteristic length [m]
                                                 Real64 CosTilt,   // cosine of surface tilt angle relative to the horizontal
                                                 Real64 SinTilt    // sine of surface tilt angle relative to the horizontal
        );

        static Real64 CalcConvCoeffAbsPlateAndWater(EnergyPlusData &state,
                                                    Real64 TAbsorber, // temperature of absorber plate [C]
                                                    Real64 TWater,    // temperature of water [C]
                                                    Real64 Lc,        // characteristic length [m]
                                                    Real64 TiltR2V    // collector tilt angle relative to the vertical [degree]
        );

        static void GetExtVentedCavityIndex(EnergyPlusData &state, int SurfacePtr, int &VentCavIndex);

        void update(EnergyPlusData &state);

        void report(EnergyPlusData &state);

        void oneTimeInit_new(EnergyPlusData &state) override;

        void oneTimeInit(EnergyPlusData &state) override;
    };

    void GetSolarCollectorInput(EnergyPlusData &state);

} // namespace SolarCollectors

struct SolarCollectorsData : BaseGlobalStruct
{

    Array1D_bool CheckEquipName;
    int NumOfCollectors = 0;
    int NumOfParameters = 0;
    bool GetInputFlag = true;

    Array1D<SolarCollectors::ParametersData> Parameters;
    Array1D<SolarCollectors::CollectorData> Collector;
    std::unordered_map<std::string, std::string> UniqueParametersNames;
    std::unordered_map<std::string, std::string> UniqueCollectorNames;

    void clear_state() override
    {
        NumOfCollectors = 0;
        NumOfParameters = 0;
        GetInputFlag = true;
        Parameters.deallocate();
        Collector.deallocate();
        UniqueCollectorNames.clear();
        UniqueParametersNames.clear();
    }

    // Default Constructor
    SolarCollectorsData() = default;
};
} // namespace EnergyPlus

#endif
