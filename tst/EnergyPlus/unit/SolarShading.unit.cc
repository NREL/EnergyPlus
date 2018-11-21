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

// EnergyPlus::SolarShading Unit Tests

// Google Test Headers
#include <gtest/gtest.h>

// EnergyPlus Headers
#include <EnergyPlus/DataBSDFWindow.hh>
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataGlobals.hh>
#include <EnergyPlus/DataHeatBalance.hh>
#include <EnergyPlus/DataShadowingCombinations.hh>
#include <EnergyPlus/DataSurfaces.hh>
#include <EnergyPlus/DataSystemVariables.hh>
#include <EnergyPlus/DataVectorTypes.hh>
#include <EnergyPlus/HeatBalanceManager.hh>
#include <EnergyPlus/ScheduleManager.hh>
#include <EnergyPlus/SimulationManager.hh>
#include <EnergyPlus/SizingManager.hh>
#include <EnergyPlus/SolarShading.hh>
#include <EnergyPlus/SurfaceGeometry.hh>
#include <EnergyPlus/UtilityRoutines.hh>

#include "Fixtures/EnergyPlusFixture.hh"

using namespace EnergyPlus;
using namespace EnergyPlus::SolarShading;
using namespace EnergyPlus::DataSurfaces;
using namespace EnergyPlus::DataGlobals;
using namespace EnergyPlus::DataSystemVariables;
using namespace EnergyPlus::DataHeatBalance;
using namespace EnergyPlus::DataBSDFWindow;
using namespace EnergyPlus::DataVectorTypes;
using namespace EnergyPlus::DataShadowingCombinations;
using namespace ObjexxFCL;

TEST_F(EnergyPlusFixture, SolarShadingTest_CalcPerSolarBeamTest)
{
    // Test inits for integrated and non-integrated shading calcs

    //	static bool ErrorsFound( false ); // If errors detected in input
    //	static int ZoneNum( 0 ); // Zone number
    //	int NumAlphas( 2 );
    //	int NumNumbers( 9 );
    Real64 AvgEqOfTime(0.0);       // Average value of Equation of Time for period
    Real64 AvgSinSolarDeclin(1.0); // Average value of Sine of Solar Declination for period
    Real64 AvgCosSolarDeclin(0.0); // Average value of Cosine of Solar Declination for period
    int NumTimeSteps(6);

    TimeStep = 1;
    TotSurfaces = 3;
    MaxBkSurf = 3;
    SurfaceWindow.allocate(TotSurfaces);
    SunlitFracHR.allocate(24, TotSurfaces);
    SunlitFrac.allocate(NumTimeSteps, 24, TotSurfaces);
    SunlitFracWithoutReveal.allocate(NumTimeSteps, 24, TotSurfaces);
    CTHETA.allocate(TotSurfaces);
    CosIncAngHR.allocate(24, TotSurfaces);
    CosIncAng.allocate(NumTimeSteps, 24, TotSurfaces);
    AOSurf.allocate(TotSurfaces);
    BackSurfaces.allocate(NumTimeSteps, 24, MaxBkSurf, TotSurfaces);
    OverlapAreas.allocate(NumTimeSteps, 24, MaxBkSurf, TotSurfaces);

    // Test non-integrated option first, CalcPerSolarBeam should set OutProjSLFracMult and InOutProjSLFracMult to 1.0 for all hours
    for (int SurfNum = 1; SurfNum <= TotSurfaces; ++SurfNum) {
        for (int Hour = 1; Hour <= 24; ++Hour) {
            SurfaceWindow(SurfNum).OutProjSLFracMult(Hour) = 999.0;
            SurfaceWindow(SurfNum).InOutProjSLFracMult(Hour) = 888.0;
        }
    }

    DetailedSolarTimestepIntegration = false;
    CalcPerSolarBeam(AvgEqOfTime, AvgSinSolarDeclin, AvgCosSolarDeclin);

    for (int SurfNum = 1; SurfNum <= TotSurfaces; ++SurfNum) {
        for (int Hour = 1; Hour <= 24; ++Hour) {
            EXPECT_EQ(1.0, SurfaceWindow(SurfNum).OutProjSLFracMult(Hour));
            EXPECT_EQ(1.0, SurfaceWindow(SurfNum).InOutProjSLFracMult(Hour));
        }
    }

    // Test integrated option, CalcPerSolarBeam should set OutProjSLFracMult and InOutProjSLFracMult to 1.0 only for the specified hour
    // Re-initialize to new values
    for (int SurfNum = 1; SurfNum <= TotSurfaces; ++SurfNum) {
        for (int Hour = 1; Hour <= 24; ++Hour) {
            SurfaceWindow(SurfNum).OutProjSLFracMult(Hour) = 555.0;
            SurfaceWindow(SurfNum).InOutProjSLFracMult(Hour) = 444.0;
        }
    }

    DetailedSolarTimestepIntegration = true;
    HourOfDay = 23;
    CalcPerSolarBeam(AvgEqOfTime, AvgSinSolarDeclin, AvgCosSolarDeclin);

    for (int SurfNum = 1; SurfNum <= TotSurfaces; ++SurfNum) {
        for (int Hour = 1; Hour <= 24; ++Hour) {
            if (Hour == HourOfDay) {
                EXPECT_EQ(1.0, SurfaceWindow(SurfNum).OutProjSLFracMult(Hour));
                EXPECT_EQ(1.0, SurfaceWindow(SurfNum).InOutProjSLFracMult(Hour));
            } else {
                EXPECT_EQ(555.0, SurfaceWindow(SurfNum).OutProjSLFracMult(Hour));
                EXPECT_EQ(444.0, SurfaceWindow(SurfNum).InOutProjSLFracMult(Hour));
            }
        }
    }

    // Clean up
    SurfaceWindow.deallocate();
    SunlitFracHR.deallocate();
    SunlitFrac.deallocate();
    SunlitFracWithoutReveal.deallocate();
    CTHETA.deallocate();
    CosIncAngHR.deallocate();
    CosIncAng.deallocate();
    AOSurf.deallocate();
    BackSurfaces.deallocate();
    OverlapAreas.deallocate();
}

TEST_F(EnergyPlusFixture, SolarShadingTest_SurfaceScheduledSolarInc)
{
    int SurfSolIncPtr;
    TotSurfIncSolSSG = 4;
    SurfIncSolSSG.allocate(TotSurfIncSolSSG);
    SurfIncSolSSG(1).SurfPtr = 1;
    SurfIncSolSSG(1).ConstrPtr = 1;
    SurfIncSolSSG(2).SurfPtr = 1;
    SurfIncSolSSG(2).ConstrPtr = 2;
    SurfIncSolSSG(3).SurfPtr = 4;
    SurfIncSolSSG(3).ConstrPtr = 10;
    SurfIncSolSSG(4).SurfPtr = 5;
    SurfIncSolSSG(4).ConstrPtr = 1;

    // Test retrieving pointer for surface incident solar schedule

    SurfSolIncPtr = -99;
    SurfSolIncPtr = SurfaceScheduledSolarInc(1, 1);
    EXPECT_EQ(1, SurfSolIncPtr);

    SurfSolIncPtr = -99;
    SurfSolIncPtr = SurfaceScheduledSolarInc(1, 2);
    EXPECT_EQ(2, SurfSolIncPtr);

    SurfSolIncPtr = -99;
    SurfSolIncPtr = SurfaceScheduledSolarInc(1, 3);
    EXPECT_EQ(0, SurfSolIncPtr);

    SurfSolIncPtr = -99;
    SurfSolIncPtr = SurfaceScheduledSolarInc(5, 1);
    EXPECT_EQ(4, SurfSolIncPtr);

    SurfSolIncPtr = -99;
    SurfSolIncPtr = SurfaceScheduledSolarInc(5, 10);
    EXPECT_EQ(0, SurfSolIncPtr);

    SurfIncSolSSG.deallocate();
}

TEST_F(EnergyPlusFixture, SolarShadingTest_polygon_contains_point)
{
    unsigned int numSides = 4;
    Array1D<Vector> Rectangle3d;

    Rectangle3d.allocate(numSides);

    Rectangle3d(1).x = 0.;
    Rectangle3d(1).y = 0.;
    Rectangle3d(1).z = 0.;

    Rectangle3d(2).x = 10.;
    Rectangle3d(2).y = 0.;
    Rectangle3d(2).z = 0.;

    Rectangle3d(3).x = 10.;
    Rectangle3d(3).y = 10.;
    Rectangle3d(3).z = 0.;

    Rectangle3d(4).x = 0.;
    Rectangle3d(4).y = 10.;
    Rectangle3d(4).z = 0.;

    Vector PointInside;

    PointInside.x = 5.;
    PointInside.y = 5.;
    PointInside.z = 0.;

    Vector PointOutside;

    PointOutside.x = 20.;
    PointOutside.y = 20.;
    PointOutside.z = 0.;

    EXPECT_TRUE(polygon_contains_point(numSides, Rectangle3d, PointInside, false, false, true));
    EXPECT_FALSE(polygon_contains_point(numSides, Rectangle3d, PointOutside, false, false, true));

    Rectangle3d(1).x = 0.;
    Rectangle3d(1).y = 0.;
    Rectangle3d(1).z = 0.;

    Rectangle3d(2).x = 10.;
    Rectangle3d(2).y = 0.;
    Rectangle3d(2).z = 0.;

    Rectangle3d(3).x = 10.;
    Rectangle3d(3).y = 0.;
    Rectangle3d(3).z = 10.;

    Rectangle3d(4).x = 0.;
    Rectangle3d(4).y = 0.;
    Rectangle3d(4).z = 10.;

    PointInside.x = 5.;
    PointInside.y = 0.;
    PointInside.z = 5.;

    PointOutside.x = 20.;
    PointOutside.y = 0.;
    PointOutside.z = 20.;

    EXPECT_TRUE(polygon_contains_point(numSides, Rectangle3d, PointInside, false, true, false));
    EXPECT_FALSE(polygon_contains_point(numSides, Rectangle3d, PointOutside, false, true, false));

    Rectangle3d(1).x = 0.;
    Rectangle3d(1).y = 0.;
    Rectangle3d(1).z = 0.;

    Rectangle3d(2).x = 0.;
    Rectangle3d(2).y = 10.;
    Rectangle3d(2).z = 0.;

    Rectangle3d(3).x = 0.;
    Rectangle3d(3).y = 10.;
    Rectangle3d(3).z = 10.;

    Rectangle3d(4).x = 0.;
    Rectangle3d(4).y = 0.;
    Rectangle3d(4).z = 10.;

    PointInside.x = 0.;
    PointInside.y = 5.;
    PointInside.z = 5.;

    PointOutside.x = 0.;
    PointOutside.y = 20.;
    PointOutside.z = 20.;

    EXPECT_TRUE(polygon_contains_point(numSides, Rectangle3d, PointInside, true, false, false));
    EXPECT_FALSE(polygon_contains_point(numSides, Rectangle3d, PointOutside, true, false, false));
}

TEST_F(EnergyPlusFixture, SolarShadingTest_FigureSolarBeamAtTimestep)
{
    std::string const idf_objects = delimited_string({"  Version,9.0;                                                                       ",
                                                      "  Building,                                                                          ",
                                                      "    DemoFDT,                 !- Name                                                 ",
                                                      "    0,                       !- North Axis {deg}                                     ",
                                                      "    Suburbs,                 !- Terrain                                              ",
                                                      "    3.9999999E-02,           !- Loads Convergence Tolerance Value                    ",
                                                      "    4.0000002E-03,           !- Temperature Convergence Tolerance Value {deltaC}     ",
                                                      "    FullExterior,            !- Solar Distribution                                   ",
                                                      "    ,                        !- Maximum Number of Warmup Days                        ",
                                                      "    6;                       !- Minimum Number of Warmup Days                        ",
                                                      "  ShadowCalculation,                                                                 ",
                                                      "    TimestepFrequency,       !- Calculation Method                                   ",
                                                      "    ,                        !- Calculation Frequency                                ",
                                                      "    ,                        !- Maximum Figures in Shadow Overlap Calculations       ",
                                                      "    ,                        !- Polygon Clipping Algorithm                           ",
                                                      "    DetailedSkyDiffuseModeling;  !- Sky Diffuse Modeling Algorithm                   ",
                                                      "  SurfaceConvectionAlgorithm:Inside,TARP;                                            ",
                                                      "  SurfaceConvectionAlgorithm:Outside,TARP;                                           ",
                                                      "  HeatBalanceAlgorithm,ConductionTransferFunction;                                   ",
                                                      "  Timestep,6;                                                                        ",
                                                      "  RunPeriod,                                                                         ",
                                                      "    ,                        !- Name                                                 ",
                                                      "    1,                       !- Begin Month                                          ",
                                                      "    1,                       !- Begin Day of Month                                   ",
                                                      "    ,                        !- Begin Year                                           ",
                                                      "    12,                      !- End Month                                            ",
                                                      "    31,                      !- End Day of Month                                     ",
                                                      "    ,                        !- End Year                                             ",
                                                      "    ,                        !- Day of Week for Start Day                            ",
                                                      "    ,                        !- Use Weather File Holidays and Special Days           ",
                                                      "    ,                        !- Use Weather File Daylight Saving Period              ",
                                                      "    ,                        !- Apply Weekend Holiday Rule                           ",
                                                      "    ,                        !- Use Weather File Rain Indicators                     ",
                                                      "    ;                        !- Use Weather File Snow Indicators                     ",
                                                      "  ScheduleTypeLimits,                                                                ",
                                                      "    Fraction,                !- Name                                                 ",
                                                      "    0.0,                     !- Lower Limit Value                                    ",
                                                      "    1.0,                     !- Upper Limit Value                                    ",
                                                      "    Continuous;              !- Numeric Type                                         ",
                                                      "  ScheduleTypeLimits,                                                                ",
                                                      "    ON/OFF,                  !- Name                                                 ",
                                                      "    0,                       !- Lower Limit Value                                    ",
                                                      "    1,                       !- Upper Limit Value                                    ",
                                                      "    Discrete;                !- Numeric Type                                         ",
                                                      "  Schedule:Compact,                                                                  ",
                                                      "    SunShading,              !- Name                                                 ",
                                                      "    ON/OFF,                  !- Schedule Type Limits Name                            ",
                                                      "    Through: 4/30,           !- Field 1                                              ",
                                                      "    For: AllDays,            !- Field 2                                              ",
                                                      "    until: 24:00,1,          !- Field 3                                              ",
                                                      "    Through: 10/31,          !- Field 5                                              ",
                                                      "    For: AllDays,            !- Field 6                                              ",
                                                      "    until: 24:00,0,          !- Field 7                                              ",
                                                      "    Through: 12/31,          !- Field 9                                              ",
                                                      "    For: AllDays,            !- Field 10                                             ",
                                                      "    until: 24:00,1;          !- Field 11                                             ",
                                                      "  Material,                                                                          ",
                                                      "    A2 - 4 IN DENSE FACE BRICK,  !- Name                                             ",
                                                      "    Rough,                   !- Roughness                                            ",
                                                      "    0.1014984,               !- Thickness {m}                                        ",
                                                      "    1.245296,                !- Conductivity {W/m-K}                                 ",
                                                      "    2082.400,                !- Density {kg/m3}                                      ",
                                                      "    920.4800,                !- Specific Heat {J/kg-K}                               ",
                                                      "    0.9000000,               !- Thermal Absorptance                                  ",
                                                      "    0.9300000,               !- Solar Absorptance                                    ",
                                                      "    0.9300000;               !- Visible Absorptance                                  ",
                                                      "  Material,                                                                          ",
                                                      "    E1 - 3 / 4 IN PLASTER OR GYP BOARD,  !- Name                                     ",
                                                      "    Smooth,                  !- Roughness                                            ",
                                                      "    1.9050000E-02,           !- Thickness {m}                                        ",
                                                      "    0.7264224,               !- Conductivity {W/m-K}                                 ",
                                                      "    1601.846,                !- Density {kg/m3}                                      ",
                                                      "    836.8000,                !- Specific Heat {J/kg-K}                               ",
                                                      "    0.9000000,               !- Thermal Absorptance                                  ",
                                                      "    0.9200000,               !- Solar Absorptance                                    ",
                                                      "    0.9200000;               !- Visible Absorptance                                  ",
                                                      "  Material,                                                                          ",
                                                      "    E2 - 1 / 2 IN SLAG OR STONE,  !- Name                                            ",
                                                      "    Rough,                   !- Roughness                                            ",
                                                      "    1.2710161E-02,           !- Thickness {m}                                        ",
                                                      "    1.435549,                !- Conductivity {W/m-K}                                 ",
                                                      "    881.0155,                !- Density {kg/m3}                                      ",
                                                      "    1673.600,                !- Specific Heat {J/kg-K}                               ",
                                                      "    0.9000000,               !- Thermal Absorptance                                  ",
                                                      "    0.5500000,               !- Solar Absorptance                                    ",
                                                      "    0.5500000;               !- Visible Absorptance                                  ",
                                                      "  Material,                                                                          ",
                                                      "    C12 - 2 IN HW CONCRETE,  !- Name                                                 ",
                                                      "    MediumRough,             !- Roughness                                            ",
                                                      "    5.0901599E-02,           !- Thickness {m}                                        ",
                                                      "    1.729577,                !- Conductivity {W/m-K}                                 ",
                                                      "    2242.585,                !- Density {kg/m3}                                      ",
                                                      "    836.8000,                !- Specific Heat {J/kg-K}                               ",
                                                      "    0.9000000,               !- Thermal Absorptance                                  ",
                                                      "    0.6500000,               !- Solar Absorptance                                    ",
                                                      "    0.6500000;               !- Visible Absorptance                                  ",
                                                      "  Material:NoMass,                                                                   ",
                                                      "    R13LAYER,                !- Name                                                 ",
                                                      "    Rough,                   !- Roughness                                            ",
                                                      "    2.290965,                !- Thermal Resistance {m2-K/W}                          ",
                                                      "    0.9000000,               !- Thermal Absorptance                                  ",
                                                      "    0.7500000,               !- Solar Absorptance                                    ",
                                                      "    0.7500000;               !- Visible Absorptance                                  ",
                                                      "  WindowMaterial:Glazing,                                                            ",
                                                      "    GLASS - CLEAR PLATE 1 / 4 IN,  !- Name                                           ",
                                                      "    SpectralAverage,         !- Optical Data Type                                    ",
                                                      "    ,                        !- Window Glass Spectral Data Set Name                  ",
                                                      "    0.006,                   !- Thickness {m}                                        ",
                                                      "    0.80,                    !- Solar Transmittance at Normal Incidence              ",
                                                      "    0.10,                    !- Front Side Solar Reflectance at Normal Incidence     ",
                                                      "    0.10,                    !- Back Side Solar Reflectance at Normal Incidence      ",
                                                      "    0.80,                    !- Visible Transmittance at Normal Incidence            ",
                                                      "    0.10,                    !- Front Side Visible Reflectance at Normal Incidence   ",
                                                      "    0.10,                    !- Back Side Visible Reflectance at Normal Incidence    ",
                                                      "    0.0,                     !- Infrared Transmittance at Normal Incidence           ",
                                                      "    0.84,                    !- Front Side Infrared Hemispherical Emissivity         ",
                                                      "    0.84,                    !- Back Side Infrared Hemispherical Emissivity          ",
                                                      "    0.9;                     !- Conductivity {W/m-K}                                 ",
                                                      "  WindowMaterial:Gas,                                                                ",
                                                      "    AIRGAP,                  !- Name                                                 ",
                                                      "    AIR,                     !- Gas Type                                             ",
                                                      "    0.0125;                  !- Thickness {m}                                        ",
                                                      "  Construction,                                                                      ",
                                                      "    R13WALL,                 !- Name                                                 ",
                                                      "    R13LAYER;                !- Outside Layer                                        ",
                                                      "  Construction,                                                                      ",
                                                      "    EXTWALL09,               !- Name                                                 ",
                                                      "    A2 - 4 IN DENSE FACE BRICK,  !- Outside Layer                                    ",
                                                      "    E1 - 3 / 4 IN PLASTER OR GYP BOARD;  !- Layer 4                                  ",
                                                      "  Construction,                                                                      ",
                                                      "    INTERIOR,                !- Name                                                 ",
                                                      "    C12 - 2 IN HW CONCRETE;  !- Layer 4                                              ",
                                                      "  Construction,                                                                      ",
                                                      "    SLAB FLOOR,              !- Name                                                 ",
                                                      "    C12 - 2 IN HW CONCRETE;  !- Layer 4                                              ",
                                                      "  Construction,                                                                      ",
                                                      "    ROOF31,                  !- Name                                                 ",
                                                      "    E2 - 1 / 2 IN SLAG OR STONE,  !- Outside Layer                                   ",
                                                      "    C12 - 2 IN HW CONCRETE;  !- Layer 4                                              ",
                                                      "  Construction,                                                                      ",
                                                      "    DOUBLE PANE HW WINDOW,   !- Name                                                 ",
                                                      "    GLASS - CLEAR PLATE 1 / 4 IN,  !- Outside Layer                                  ",
                                                      "    AIRGAP,                  !- Layer 2                                              ",
                                                      "    GLASS - CLEAR PLATE 1 / 4 IN;  !- Layer 3                                        ",
                                                      "  Construction,                                                                      ",
                                                      "    PARTITION02,             !- Name                                                 ",
                                                      "    E1 - 3 / 4 IN PLASTER OR GYP BOARD,  !- Outside Layer                            ",
                                                      "    C12 - 2 IN HW CONCRETE,  !- Layer 4                                              ",
                                                      "    E1 - 3 / 4 IN PLASTER OR GYP BOARD;  !- Layer 3                                  ",
                                                      "  Construction,                                                                      ",
                                                      "    single PANE HW WINDOW,   !- Name                                                 ",
                                                      "    GLASS - CLEAR PLATE 1 / 4 IN;  !- Outside Layer                                  ",
                                                      "  Construction,                                                                      ",
                                                      "    EXTWALLdemo,             !- Name                                                 ",
                                                      "    A2 - 4 IN DENSE FACE BRICK,  !- Outside Layer                                    ",
                                                      "    E1 - 3 / 4 IN PLASTER OR GYP BOARD;  !- Layer 4                                  ",
                                                      "  GlobalGeometryRules,                                                               ",
                                                      "    UpperLeftCorner,         !- Starting Vertex Position                             ",
                                                      "    Counterclockwise,        !- Vertex Entry Direction                               ",
                                                      "    Relative;                !- Coordinate System                                    ",
                                                      "  Zone,                                                                              ",
                                                      "    ZONE ONE,                !- Name                                                 ",
                                                      "    0,                       !- Direction of Relative North {deg}                    ",
                                                      "    0,                       !- X Origin {m}                                         ",
                                                      "    0,                       !- Y Origin {m}                                         ",
                                                      "    0,                       !- Z Origin {m}                                         ",
                                                      "    1,                       !- Type                                                 ",
                                                      "    1,                       !- Multiplier                                           ",
                                                      "    0,                       !- Ceiling Height {m}                                   ",
                                                      "    0;                       !- Volume {m3}                                          ",
                                                      "  BuildingSurface:Detailed,                                                          ",
                                                      "    Zn001:Wall-North,        !- Name                                                 ",
                                                      "    Wall,                    !- Surface Type                                         ",
                                                      "    EXTWALLdemo,             !- Construction Name                                    ",
                                                      "    ZONE ONE,                !- Zone Name                                            ",
                                                      "    Outdoors,                !- Outside Boundary Condition                           ",
                                                      "    ,                        !- Outside Boundary Condition Object                    ",
                                                      "    SunExposed,              !- Sun Exposure                                         ",
                                                      "    WindExposed,             !- Wind Exposure                                        ",
                                                      "    0.5000000,               !- View Factor to Ground                                ",
                                                      "    4,                       !- Number of Vertices                                   ",
                                                      "    5,5,3,  !- X,Y,Z ==> Vertex 1 {m}                                                ",
                                                      "    5,5,0,  !- X,Y,Z ==> Vertex 2 {m}                                                ",
                                                      "    -5,5,0,  !- X,Y,Z ==> Vertex 3 {m}                                               ",
                                                      "    -5,5,3;  !- X,Y,Z ==> Vertex 4 {m}                                               ",
                                                      "  BuildingSurface:Detailed,                                                          ",
                                                      "    Zn001:Wall-East,         !- Name                                                 ",
                                                      "    Wall,                    !- Surface Type                                         ",
                                                      "    EXTWALL09,               !- Construction Name                                    ",
                                                      "    ZONE ONE,                !- Zone Name                                            ",
                                                      "    Outdoors,                !- Outside Boundary Condition                           ",
                                                      "    ,                        !- Outside Boundary Condition Object                    ",
                                                      "    SunExposed,              !- Sun Exposure                                         ",
                                                      "    WindExposed,             !- Wind Exposure                                        ",
                                                      "    0.5000000,               !- View Factor to Ground                                ",
                                                      "    4,                       !- Number of Vertices                                   ",
                                                      "    5,-5,3,  !- X,Y,Z ==> Vertex 1 {m}                                               ",
                                                      "    5,-5,0,  !- X,Y,Z ==> Vertex 2 {m}                                               ",
                                                      "    5,5,0,  !- X,Y,Z ==> Vertex 3 {m}                                                ",
                                                      "    5,5,3;  !- X,Y,Z ==> Vertex 4 {m}                                                ",
                                                      "  BuildingSurface:Detailed,                                                          ",
                                                      "    Zn001:Wall-South,        !- Name                                                 ",
                                                      "    Wall,                    !- Surface Type                                         ",
                                                      "    R13WALL,                 !- Construction Name                                    ",
                                                      "    ZONE ONE,                !- Zone Name                                            ",
                                                      "    Outdoors,                !- Outside Boundary Condition                           ",
                                                      "    ,                        !- Outside Boundary Condition Object                    ",
                                                      "    SunExposed,              !- Sun Exposure                                         ",
                                                      "    WindExposed,             !- Wind Exposure                                        ",
                                                      "    0.5000000,               !- View Factor to Ground                                ",
                                                      "    4,                       !- Number of Vertices                                   ",
                                                      "    -5,-5,3,  !- X,Y,Z ==> Vertex 1 {m}                                              ",
                                                      "    -5,-5,0,  !- X,Y,Z ==> Vertex 2 {m}                                              ",
                                                      "    5,-5,0,  !- X,Y,Z ==> Vertex 3 {m}                                               ",
                                                      "    5,-5,3;  !- X,Y,Z ==> Vertex 4 {m}                                               ",
                                                      "  BuildingSurface:Detailed,                                                          ",
                                                      "    Zn001:Wall-West,         !- Name                                                 ",
                                                      "    Wall,                    !- Surface Type                                         ",
                                                      "    EXTWALL09,               !- Construction Name                                    ",
                                                      "    ZONE ONE,                !- Zone Name                                            ",
                                                      "    Outdoors,                !- Outside Boundary Condition                           ",
                                                      "    ,                        !- Outside Boundary Condition Object                    ",
                                                      "    SunExposed,              !- Sun Exposure                                         ",
                                                      "    WindExposed,             !- Wind Exposure                                        ",
                                                      "    0.5000000,               !- View Factor to Ground                                ",
                                                      "    4,                       !- Number of Vertices                                   ",
                                                      "    -5,5,3,  !- X,Y,Z ==> Vertex 1 {m}                                               ",
                                                      "    -5,5,0,  !- X,Y,Z ==> Vertex 2 {m}                                               ",
                                                      "    -5,-5,0,  !- X,Y,Z ==> Vertex 3 {m}                                              ",
                                                      "    -5,-5,3;  !- X,Y,Z ==> Vertex 4 {m}                                              ",
                                                      "  BuildingSurface:Detailed,                                                          ",
                                                      "    Zn001:roof,              !- Name                                                 ",
                                                      "    Roof,                    !- Surface Type                                         ",
                                                      "    ROOF31,                  !- Construction Name                                    ",
                                                      "    ZONE ONE,                !- Zone Name                                            ",
                                                      "    Outdoors,                !- Outside Boundary Condition                           ",
                                                      "    ,                        !- Outside Boundary Condition Object                    ",
                                                      "    SunExposed,              !- Sun Exposure                                         ",
                                                      "    WindExposed,             !- Wind Exposure                                        ",
                                                      "    0.0000000,               !- View Factor to Ground                                ",
                                                      "    4,                       !- Number of Vertices                                   ",
                                                      "    -5,-5,3,  !- X,Y,Z ==> Vertex 1 {m}                                              ",
                                                      "    5,-5,3,  !- X,Y,Z ==> Vertex 2 {m}                                               ",
                                                      "    5,5,3,  !- X,Y,Z ==> Vertex 3 {m}                                                ",
                                                      "    -5,5,3;  !- X,Y,Z ==> Vertex 4 {m}                                               ",
                                                      "  BuildingSurface:Detailed,                                                          ",
                                                      "    Zn001:floor,             !- Name                                                 ",
                                                      "    Floor,                   !- Surface Type                                         ",
                                                      "    SLAB FLOOR,              !- Construction Name                                    ",
                                                      "    ZONE ONE,                !- Zone Name                                            ",
                                                      "    Outdoors,                !- Outside Boundary Condition                           ",
                                                      "    ,                        !- Outside Boundary Condition Object                    ",
                                                      "    SunExposed,              !- Sun Exposure                                         ",
                                                      "    WindExposed,             !- Wind Exposure                                        ",
                                                      "    0.0000000,               !- View Factor to Ground                                ",
                                                      "    4,                       !- Number of Vertices                                   ",
                                                      "    -5,5,0,  !- X,Y,Z ==> Vertex 1 {m}                                               ",
                                                      "    5,5,0,  !- X,Y,Z ==> Vertex 2 {m}                                                ",
                                                      "    5,-5,0,  !- X,Y,Z ==> Vertex 3 {m}                                               ",
                                                      "    -5,-5,0;  !- X,Y,Z ==> Vertex 4 {m}                                              ",
                                                      "  FenestrationSurface:Detailed,                                                      ",
                                                      "    Zn001:Wall-South:Win001, !- Name                                                 ",
                                                      "    Window,                  !- Surface Type                                         ",
                                                      "    DOUBLE PANE HW WINDOW,   !- Construction Name                                    ",
                                                      "    Zn001:Wall-South,        !- Building Surface Name                                ",
                                                      "    ,                        !- Outside Boundary Condition Object                    ",
                                                      "    0.5000000,               !- View Factor to Ground                                ",
                                                      "    TestFrameAndDivider,     !- Frame and Divider Name                               ",
                                                      "    1.0,                     !- Multiplier                                           ",
                                                      "    4,                       !- Number of Vertices                                   ",
                                                      "    -3,-5,2.5,  !- X,Y,Z ==> Vertex 1 {m}                                            ",
                                                      "    -3,-5,0.5,  !- X,Y,Z ==> Vertex 2 {m}                                            ",
                                                      "    3,-5,0.5,  !- X,Y,Z ==> Vertex 3 {m}                                             ",
                                                      "    3,-5,2.5;  !- X,Y,Z ==> Vertex 4 {m}                                             ",
                                                      "  WindowProperty:FrameAndDivider,                                                    ",
                                                      "    TestFrameAndDivider,     !- Name                                                 ",
                                                      "    0.05,                    !- Frame Width {m}                                      ",
                                                      "    0.05,                    !- Frame Outside Projection {m}                         ",
                                                      "    0.05,                    !- Frame Inside Projection {m}                          ",
                                                      "    5.0,                     !- Frame Conductance {W/m2-K}                           ",
                                                      "    1.2,                     !- Ratio of Frame-Edge Glass Conductance to Center-Of-Gl",
                                                      "    0.8,                     !- Frame Solar Absorptance                              ",
                                                      "    0.8,                     !- Frame Visible Absorptance                            ",
                                                      "    0.9,                     !- Frame Thermal Hemispherical Emissivity               ",
                                                      "    DividedLite,             !- Divider Type                                         ",
                                                      "    0.02,                    !- Divider Width {m}                                    ",
                                                      "    2,                       !- Number of Horizontal Dividers                        ",
                                                      "    2,                       !- Number of Vertical Dividers                          ",
                                                      "    0.02,                    !- Divider Outside Projection {m}                       ",
                                                      "    0.02,                    !- Divider Inside Projection {m}                        ",
                                                      "    5.0,                     !- Divider Conductance {W/m2-K}                         ",
                                                      "    1.2,                     !- Ratio of Divider-Edge Glass Conductance to Center-Of-",
                                                      "    0.8,                     !- Divider Solar Absorptance                            ",
                                                      "    0.8,                     !- Divider Visible Absorptance                          ",
                                                      "    0.9;                     !- Divider Thermal Hemispherical Emissivity             ",
                                                      "  Shading:Zone:Detailed,                                                             ",
                                                      "    Zn001:Wall-South:Shade001,  !- Name                                              ",
                                                      "    Zn001:Wall-South,        !- Base Surface Name                                    ",
                                                      "    SunShading,              !- Transmittance Schedule Name                          ",
                                                      "    4,                       !- Number of Vertices                                   ",
                                                      "    -3,-5,2.5,  !- X,Y,Z ==> Vertex 1 {m}                                            ",
                                                      "    -3,-6,2.5,  !- X,Y,Z ==> Vertex 2 {m}                                            ",
                                                      "    3,-6,2.5,  !- X,Y,Z ==> Vertex 3 {m}                                             ",
                                                      "    3,-5,2.5;  !- X,Y,Z ==> Vertex 4 {m}                                             ",
                                                      "  ShadingProperty:Reflectance,                                                       ",
                                                      "    Zn001:Wall-South:Shade001,  !- Shading Surface Name                              ",
                                                      "    0.2,                     !- Diffuse Solar Reflectance of Unglazed Part of Shading",
                                                      "    0.2;                     !- Diffuse Visible Reflectance of Unglazed Part of Shadi"});

    ASSERT_TRUE(process_idf(idf_objects));

    SimulationManager::GetProjectData();
    bool FoundError = false;

    HeatBalanceManager::GetProjectControlData(FoundError); // read project control data
    EXPECT_FALSE(FoundError);                              // expect no errors

    HeatBalanceManager::SetPreConstructionInputParameters();
    ScheduleManager::ProcessScheduleInput(); // read schedules

    HeatBalanceManager::GetMaterialData(FoundError);
    EXPECT_FALSE(FoundError);

    HeatBalanceManager::GetFrameAndDividerData(FoundError);
    EXPECT_FALSE(FoundError);

    HeatBalanceManager::GetConstructData(FoundError);
    EXPECT_FALSE(FoundError);

    HeatBalanceManager::GetZoneData(FoundError); // Read Zone data from input file
    EXPECT_FALSE(FoundError);

    SurfaceGeometry::GetGeometryParameters(FoundError);
    EXPECT_FALSE(FoundError);

    SurfaceGeometry::CosZoneRelNorth.allocate(1);
    SurfaceGeometry::SinZoneRelNorth.allocate(1);

    SurfaceGeometry::CosZoneRelNorth(1) = std::cos(-Zone(1).RelNorth * DegToRadians);
    SurfaceGeometry::SinZoneRelNorth(1) = std::sin(-Zone(1).RelNorth * DegToRadians);
    SurfaceGeometry::CosBldgRelNorth = 1.0;
    SurfaceGeometry::SinBldgRelNorth = 0.0;

    SurfaceGeometry::GetSurfaceData(FoundError); // setup zone geometry and get zone data
    EXPECT_FALSE(FoundError);                    // expect no errors

    //	compare_err_stream( "" ); // just for debugging

    SurfaceGeometry::SetupZoneGeometry(FoundError); // this calls GetSurfaceData()
    EXPECT_FALSE(FoundError);

    SolarShading::AllocateModuleArrays();
    SolarShading::DetermineShadowingCombinations();
    DataEnvironment::DayOfYear_Schedule = 168;
    DataEnvironment::DayOfWeek = 6;
    DataGlobals::TimeStep = 4;
    DataGlobals::HourOfDay = 9;

    //	compare_err_stream( "" ); // just for debugging

    DataSurfaces::ShadingTransmittanceVaries = true;
    DataSystemVariables::DetailedSkyDiffuseAlgorithm = true;
    SolarDistribution = FullExterior;

    CalcSkyDifShading = true;
    SolarShading::InitSolarCalculations();
    SolarShading::SkyDifSolarShading();
    CalcSkyDifShading = false;

    FigureSolarBeamAtTimestep(DataGlobals::HourOfDay, DataGlobals::TimeStep);

    EXPECT_NEAR(0.6504, DifShdgRatioIsoSkyHRTS(4, 9, 6), 0.0001);
    EXPECT_NEAR(0.9152, DifShdgRatioHorizHRTS(4, 9, 6), 0.0001);
}

TEST_F(EnergyPlusFixture, SolarShadingTest_ExternalShadingIO)
{
    std::string const idf_objects = delimited_string({"  Version,9.0;                                                                       ",
                                                      "  Building,                                                                          ",
                                                      "    DemoFDT,                 !- Name                                                 ",
                                                      "    0,                       !- North Axis {deg}                                     ",
                                                      "    Suburbs,                 !- Terrain                                              ",
                                                      "    3.9999999E-02,           !- Loads Convergence Tolerance Value                    ",
                                                      "    4.0000002E-03,           !- Temperature Convergence Tolerance Value {deltaC}     ",
                                                      "    FullExterior,            !- Solar Distribution                                   ",
                                                      "    ,                        !- Maximum Number of Warmup Days                        ",
                                                      "    6;                       !- Minimum Number of Warmup Days                        ",
                                                      "  ShadowCalculation,                                                                 ",
                                                      "    TimestepFrequency,          !- Calculation Method                                ",
                                                      "    ,                           !- Calculation Frequency                             ",
                                                      "    ,                           !- Maximum Figures in Shadow Overlap Calculations    ",
                                                      "    ,                           !- Polygon Clipping Algorithm                        ",
                                                      "    DetailedSkyDiffuseModeling, !- Sky Diffuse Modeling Algorithm                    ",
                                                      "    ScheduledShading,           !- External Shading Calculation Method               ",
                                                      "    Yes;                        !- Output External Shading Calculation Results       ",
                                                      "  SurfaceConvectionAlgorithm:Inside,TARP;                                            ",
                                                      "  SurfaceConvectionAlgorithm:Outside,TARP;                                           ",
                                                      "  HeatBalanceAlgorithm,ConductionTransferFunction;                                   ",
                                                      "  Timestep,6;                                                                        ",
                                                      "  RunPeriod,                                                                         ",
                                                      "    ,                        !- Name                                                 ",
                                                      "    1,                       !- Begin Month                                          ",
                                                      "    1,                       !- Begin Day of Month                                   ",
                                                      "    ,                        !- Begin Year                                           ",
                                                      "    12,                      !- End Month                                            ",
                                                      "    31,                      !- End Day of Month                                     ",
                                                      "    ,                        !- End Year                                             ",
                                                      "    ,                        !- Day of Week for Start Day                            ",
                                                      "    ,                        !- Use Weather File Holidays and Special Days           ",
                                                      "    ,                        !- Use Weather File Daylight Saving Period              ",
                                                      "    ,                        !- Apply Weekend Holiday Rule                           ",
                                                      "    ,                        !- Use Weather File Rain Indicators                     ",
                                                      "    ;                        !- Use Weather File Snow Indicators                     ",
                                                      "  ScheduleTypeLimits,                                                                ",
                                                      "    Fraction,                !- Name                                                 ",
                                                      "    0.0,                     !- Lower Limit Value                                    ",
                                                      "    1.0,                     !- Upper Limit Value                                    ",
                                                      "    Continuous;              !- Numeric Type                                         ",
                                                      "  ScheduleTypeLimits,                                                                ",
                                                      "    ON/OFF,                  !- Name                                                 ",
                                                      "    0,                       !- Lower Limit Value                                    ",
                                                      "    1,                       !- Upper Limit Value                                    ",
                                                      "    Discrete;                !- Numeric Type                                         ",
                                                      "  Schedule:Compact,                                                                  ",
                                                      "    SunShading,              !- Name                                                 ",
                                                      "    ON/OFF,                  !- Schedule Type Limits Name                            ",
                                                      "    Through: 4/30,           !- Field 1                                              ",
                                                      "    For: AllDays,            !- Field 2                                              ",
                                                      "    until: 24:00,1,          !- Field 3                                              ",
                                                      "    Through: 10/31,          !- Field 5                                              ",
                                                      "    For: AllDays,            !- Field 6                                              ",
                                                      "    until: 24:00,0,          !- Field 7                                              ",
                                                      "    Through: 12/31,          !- Field 9                                              ",
                                                      "    For: AllDays,            !- Field 10                                             ",
                                                      "    until: 24:00,1;          !- Field 11                                             ",
                                                      "  Material,                                                                          ",
                                                      "    A2 - 4 IN DENSE FACE BRICK,  !- Name                                             ",
                                                      "    Rough,                   !- Roughness                                            ",
                                                      "    0.1014984,               !- Thickness {m}                                        ",
                                                      "    1.245296,                !- Conductivity {W/m-K}                                 ",
                                                      "    2082.400,                !- Density {kg/m3}                                      ",
                                                      "    920.4800,                !- Specific Heat {J/kg-K}                               ",
                                                      "    0.9000000,               !- Thermal Absorptance                                  ",
                                                      "    0.9300000,               !- Solar Absorptance                                    ",
                                                      "    0.9300000;               !- Visible Absorptance                                  ",
                                                      "  Material,                                                                          ",
                                                      "    E1 - 3 / 4 IN PLASTER OR GYP BOARD,  !- Name                                     ",
                                                      "    Smooth,                  !- Roughness                                            ",
                                                      "    1.9050000E-02,           !- Thickness {m}                                        ",
                                                      "    0.7264224,               !- Conductivity {W/m-K}                                 ",
                                                      "    1601.846,                !- Density {kg/m3}                                      ",
                                                      "    836.8000,                !- Specific Heat {J/kg-K}                               ",
                                                      "    0.9000000,               !- Thermal Absorptance                                  ",
                                                      "    0.9200000,               !- Solar Absorptance                                    ",
                                                      "    0.9200000;               !- Visible Absorptance                                  ",
                                                      "  Material,                                                                          ",
                                                      "    E2 - 1 / 2 IN SLAG OR STONE,  !- Name                                            ",
                                                      "    Rough,                   !- Roughness                                            ",
                                                      "    1.2710161E-02,           !- Thickness {m}                                        ",
                                                      "    1.435549,                !- Conductivity {W/m-K}                                 ",
                                                      "    881.0155,                !- Density {kg/m3}                                      ",
                                                      "    1673.600,                !- Specific Heat {J/kg-K}                               ",
                                                      "    0.9000000,               !- Thermal Absorptance                                  ",
                                                      "    0.5500000,               !- Solar Absorptance                                    ",
                                                      "    0.5500000;               !- Visible Absorptance                                  ",
                                                      "  Material,                                                                          ",
                                                      "    C12 - 2 IN HW CONCRETE,  !- Name                                                 ",
                                                      "    MediumRough,             !- Roughness                                            ",
                                                      "    5.0901599E-02,           !- Thickness {m}                                        ",
                                                      "    1.729577,                !- Conductivity {W/m-K}                                 ",
                                                      "    2242.585,                !- Density {kg/m3}                                      ",
                                                      "    836.8000,                !- Specific Heat {J/kg-K}                               ",
                                                      "    0.9000000,               !- Thermal Absorptance                                  ",
                                                      "    0.6500000,               !- Solar Absorptance                                    ",
                                                      "    0.6500000;               !- Visible Absorptance                                  ",
                                                      "  Material:NoMass,                                                                   ",
                                                      "    R13LAYER,                !- Name                                                 ",
                                                      "    Rough,                   !- Roughness                                            ",
                                                      "    2.290965,                !- Thermal Resistance {m2-K/W}                          ",
                                                      "    0.9000000,               !- Thermal Absorptance                                  ",
                                                      "    0.7500000,               !- Solar Absorptance                                    ",
                                                      "    0.7500000;               !- Visible Absorptance                                  ",
                                                      "  WindowMaterial:Glazing,                                                            ",
                                                      "    GLASS - CLEAR PLATE 1 / 4 IN,  !- Name                                           ",
                                                      "    SpectralAverage,         !- Optical Data Type                                    ",
                                                      "    ,                        !- Window Glass Spectral Data Set Name                  ",
                                                      "    0.006,                   !- Thickness {m}                                        ",
                                                      "    0.80,                    !- Solar Transmittance at Normal Incidence              ",
                                                      "    0.10,                    !- Front Side Solar Reflectance at Normal Incidence     ",
                                                      "    0.10,                    !- Back Side Solar Reflectance at Normal Incidence      ",
                                                      "    0.80,                    !- Visible Transmittance at Normal Incidence            ",
                                                      "    0.10,                    !- Front Side Visible Reflectance at Normal Incidence   ",
                                                      "    0.10,                    !- Back Side Visible Reflectance at Normal Incidence    ",
                                                      "    0.0,                     !- Infrared Transmittance at Normal Incidence           ",
                                                      "    0.84,                    !- Front Side Infrared Hemispherical Emissivity         ",
                                                      "    0.84,                    !- Back Side Infrared Hemispherical Emissivity          ",
                                                      "    0.9;                     !- Conductivity {W/m-K}                                 ",
                                                      "  WindowMaterial:Gas,                                                                ",
                                                      "    AIRGAP,                  !- Name                                                 ",
                                                      "    AIR,                     !- Gas Type                                             ",
                                                      "    0.0125;                  !- Thickness {m}                                        ",
                                                      "  Construction,                                                                      ",
                                                      "    R13WALL,                 !- Name                                                 ",
                                                      "    R13LAYER;                !- Outside Layer                                        ",
                                                      "  Construction,                                                                      ",
                                                      "    EXTWALL09,               !- Name                                                 ",
                                                      "    A2 - 4 IN DENSE FACE BRICK,  !- Outside Layer                                    ",
                                                      "    E1 - 3 / 4 IN PLASTER OR GYP BOARD;  !- Layer 4                                  ",
                                                      "  Construction,                                                                      ",
                                                      "    INTERIOR,                !- Name                                                 ",
                                                      "    C12 - 2 IN HW CONCRETE;  !- Layer 4                                              ",
                                                      "  Construction,                                                                      ",
                                                      "    SLAB FLOOR,              !- Name                                                 ",
                                                      "    C12 - 2 IN HW CONCRETE;  !- Layer 4                                              ",
                                                      "  Construction,                                                                      ",
                                                      "    ROOF31,                  !- Name                                                 ",
                                                      "    E2 - 1 / 2 IN SLAG OR STONE,  !- Outside Layer                                   ",
                                                      "    C12 - 2 IN HW CONCRETE;  !- Layer 4                                              ",
                                                      "  Construction,                                                                      ",
                                                      "    DOUBLE PANE HW WINDOW,   !- Name                                                 ",
                                                      "    GLASS - CLEAR PLATE 1 / 4 IN,  !- Outside Layer                                  ",
                                                      "    AIRGAP,                  !- Layer 2                                              ",
                                                      "    GLASS - CLEAR PLATE 1 / 4 IN;  !- Layer 3                                        ",
                                                      "  Construction,                                                                      ",
                                                      "    PARTITION02,             !- Name                                                 ",
                                                      "    E1 - 3 / 4 IN PLASTER OR GYP BOARD,  !- Outside Layer                            ",
                                                      "    C12 - 2 IN HW CONCRETE,  !- Layer 4                                              ",
                                                      "    E1 - 3 / 4 IN PLASTER OR GYP BOARD;  !- Layer 3                                  ",
                                                      "  Construction,                                                                      ",
                                                      "    single PANE HW WINDOW,   !- Name                                                 ",
                                                      "    GLASS - CLEAR PLATE 1 / 4 IN;  !- Outside Layer                                  ",
                                                      "  Construction,                                                                      ",
                                                      "    EXTWALLdemo,             !- Name                                                 ",
                                                      "    A2 - 4 IN DENSE FACE BRICK,  !- Outside Layer                                    ",
                                                      "    E1 - 3 / 4 IN PLASTER OR GYP BOARD;  !- Layer 4                                  ",
                                                      "  GlobalGeometryRules,                                                               ",
                                                      "    UpperLeftCorner,         !- Starting Vertex Position                             ",
                                                      "    Counterclockwise,        !- Vertex Entry Direction                               ",
                                                      "    Relative;                !- Coordinate System                                    ",
                                                      "  Zone,                                                                              ",
                                                      "    ZONE ONE,                !- Name                                                 ",
                                                      "    0,                       !- Direction of Relative North {deg}                    ",
                                                      "    0,                       !- X Origin {m}                                         ",
                                                      "    0,                       !- Y Origin {m}                                         ",
                                                      "    0,                       !- Z Origin {m}                                         ",
                                                      "    1,                       !- Type                                                 ",
                                                      "    1,                       !- Multiplier                                           ",
                                                      "    0,                       !- Ceiling Height {m}                                   ",
                                                      "    0;                       !- Volume {m3}                                          ",
                                                      "  BuildingSurface:Detailed,                                                          ",
                                                      "    Zn001:Wall-North,        !- Name                                                 ",
                                                      "    Wall,                    !- Surface Type                                         ",
                                                      "    EXTWALLdemo,             !- Construction Name                                    ",
                                                      "    ZONE ONE,                !- Zone Name                                            ",
                                                      "    Outdoors,                !- Outside Boundary Condition                           ",
                                                      "    ,                        !- Outside Boundary Condition Object                    ",
                                                      "    SunExposed,              !- Sun Exposure                                         ",
                                                      "    WindExposed,             !- Wind Exposure                                        ",
                                                      "    0.5000000,               !- View Factor to Ground                                ",
                                                      "    4,                       !- Number of Vertices                                   ",
                                                      "    5,5,3,  !- X,Y,Z ==> Vertex 1 {m}                                                ",
                                                      "    5,5,0,  !- X,Y,Z ==> Vertex 2 {m}                                                ",
                                                      "    -5,5,0,  !- X,Y,Z ==> Vertex 3 {m}                                               ",
                                                      "    -5,5,3;  !- X,Y,Z ==> Vertex 4 {m}                                               ",
                                                      "  BuildingSurface:Detailed,                                                          ",
                                                      "    Zn001:Wall-East,         !- Name                                                 ",
                                                      "    Wall,                    !- Surface Type                                         ",
                                                      "    EXTWALL09,               !- Construction Name                                    ",
                                                      "    ZONE ONE,                !- Zone Name                                            ",
                                                      "    Outdoors,                !- Outside Boundary Condition                           ",
                                                      "    ,                        !- Outside Boundary Condition Object                    ",
                                                      "    SunExposed,              !- Sun Exposure                                         ",
                                                      "    WindExposed,             !- Wind Exposure                                        ",
                                                      "    0.5000000,               !- View Factor to Ground                                ",
                                                      "    4,                       !- Number of Vertices                                   ",
                                                      "    5,-5,3,  !- X,Y,Z ==> Vertex 1 {m}                                               ",
                                                      "    5,-5,0,  !- X,Y,Z ==> Vertex 2 {m}                                               ",
                                                      "    5,5,0,  !- X,Y,Z ==> Vertex 3 {m}                                                ",
                                                      "    5,5,3;  !- X,Y,Z ==> Vertex 4 {m}                                                ",
                                                      "  BuildingSurface:Detailed,                                                          ",
                                                      "    Zn001:Wall-South,        !- Name                                                 ",
                                                      "    Wall,                    !- Surface Type                                         ",
                                                      "    R13WALL,                 !- Construction Name                                    ",
                                                      "    ZONE ONE,                !- Zone Name                                            ",
                                                      "    Outdoors,                !- Outside Boundary Condition                           ",
                                                      "    ,                        !- Outside Boundary Condition Object                    ",
                                                      "    SunExposed,              !- Sun Exposure                                         ",
                                                      "    WindExposed,             !- Wind Exposure                                        ",
                                                      "    0.5000000,               !- View Factor to Ground                                ",
                                                      "    4,                       !- Number of Vertices                                   ",
                                                      "    -5,-5,3,  !- X,Y,Z ==> Vertex 1 {m}                                              ",
                                                      "    -5,-5,0,  !- X,Y,Z ==> Vertex 2 {m}                                              ",
                                                      "    5,-5,0,  !- X,Y,Z ==> Vertex 3 {m}                                               ",
                                                      "    5,-5,3;  !- X,Y,Z ==> Vertex 4 {m}                                               ",
                                                      "  BuildingSurface:Detailed,                                                          ",
                                                      "    Zn001:Wall-West,         !- Name                                                 ",
                                                      "    Wall,                    !- Surface Type                                         ",
                                                      "    EXTWALL09,               !- Construction Name                                    ",
                                                      "    ZONE ONE,                !- Zone Name                                            ",
                                                      "    Outdoors,                !- Outside Boundary Condition                           ",
                                                      "    ,                        !- Outside Boundary Condition Object                    ",
                                                      "    SunExposed,              !- Sun Exposure                                         ",
                                                      "    WindExposed,             !- Wind Exposure                                        ",
                                                      "    0.5000000,               !- View Factor to Ground                                ",
                                                      "    4,                       !- Number of Vertices                                   ",
                                                      "    -5,5,3,  !- X,Y,Z ==> Vertex 1 {m}                                               ",
                                                      "    -5,5,0,  !- X,Y,Z ==> Vertex 2 {m}                                               ",
                                                      "    -5,-5,0,  !- X,Y,Z ==> Vertex 3 {m}                                              ",
                                                      "    -5,-5,3;  !- X,Y,Z ==> Vertex 4 {m}                                              ",
                                                      "  BuildingSurface:Detailed,                                                          ",
                                                      "    Zn001:roof,              !- Name                                                 ",
                                                      "    Roof,                    !- Surface Type                                         ",
                                                      "    ROOF31,                  !- Construction Name                                    ",
                                                      "    ZONE ONE,                !- Zone Name                                            ",
                                                      "    Outdoors,                !- Outside Boundary Condition                           ",
                                                      "    ,                        !- Outside Boundary Condition Object                    ",
                                                      "    SunExposed,              !- Sun Exposure                                         ",
                                                      "    WindExposed,             !- Wind Exposure                                        ",
                                                      "    0.0000000,               !- View Factor to Ground                                ",
                                                      "    4,                       !- Number of Vertices                                   ",
                                                      "    -5,-5,3,  !- X,Y,Z ==> Vertex 1 {m}                                              ",
                                                      "    5,-5,3,  !- X,Y,Z ==> Vertex 2 {m}                                               ",
                                                      "    5,5,3,  !- X,Y,Z ==> Vertex 3 {m}                                                ",
                                                      "    -5,5,3;  !- X,Y,Z ==> Vertex 4 {m}                                               ",
                                                      "  SurfaceProperty:LocalEnvironment,                                                  ",
                                                      "    LocEnv:Zn001:roof,                                                               ",
                                                      "    Zn001:roof,                                                                      ",
                                                      "    ExtShadingSch:Zn001:roof,                                                        ",
                                                      "    ,                                                                                ",
                                                      "    ;                                                                                ",
                                                      "  Schedule:Compact,                                                                  ",
                                                      "    ExtShadingSch:Zn001:roof,                                                        ",
                                                      "    Fraction,                !- Schedule Type Limits Name                            ",
                                                      "    Through: 12/31,          !- Field 1                                              ",
                                                      "    For: AllDays,            !- Field 2                                              ",
                                                      "    Until: 24:00,            !- Field 3                                              ",
                                                      "    0.5432;                  !- Field 4                                              ",
                                                      "  BuildingSurface:Detailed,                                                          ",
                                                      "    Zn001:floor,             !- Name                                                 ",
                                                      "    Floor,                   !- Surface Type                                         ",
                                                      "    SLAB FLOOR,              !- Construction Name                                    ",
                                                      "    ZONE ONE,                !- Zone Name                                            ",
                                                      "    Outdoors,                !- Outside Boundary Condition                           ",
                                                      "    ,                        !- Outside Boundary Condition Object                    ",
                                                      "    SunExposed,              !- Sun Exposure                                         ",
                                                      "    WindExposed,             !- Wind Exposure                                        ",
                                                      "    0.0000000,               !- View Factor to Ground                                ",
                                                      "    4,                       !- Number of Vertices                                   ",
                                                      "    -5,5,0,  !- X,Y,Z ==> Vertex 1 {m}                                               ",
                                                      "    5,5,0,  !- X,Y,Z ==> Vertex 2 {m}                                                ",
                                                      "    5,-5,0,  !- X,Y,Z ==> Vertex 3 {m}                                               ",
                                                      "    -5,-5,0;  !- X,Y,Z ==> Vertex 4 {m}                                              ",
                                                      "  FenestrationSurface:Detailed,                                                      ",
                                                      "    Zn001:Wall-South:Win001, !- Name                                                 ",
                                                      "    Window,                  !- Surface Type                                         ",
                                                      "    DOUBLE PANE HW WINDOW,   !- Construction Name                                    ",
                                                      "    Zn001:Wall-South,        !- Building Surface Name                                ",
                                                      "    ,                        !- Outside Boundary Condition Object                    ",
                                                      "    0.5000000,               !- View Factor to Ground                                ",
                                                      "    TestFrameAndDivider,     !- Frame and Divider Name                               ",
                                                      "    1.0,                     !- Multiplier                                           ",
                                                      "    4,                       !- Number of Vertices                                   ",
                                                      "    -3,-5,2.5,  !- X,Y,Z ==> Vertex 1 {m}                                            ",
                                                      "    -3,-5,0.5,  !- X,Y,Z ==> Vertex 2 {m}                                            ",
                                                      "    3,-5,0.5,  !- X,Y,Z ==> Vertex 3 {m}                                             ",
                                                      "    3,-5,2.5;  !- X,Y,Z ==> Vertex 4 {m}                                             ",
                                                      "  WindowProperty:FrameAndDivider,                                                    ",
                                                      "    TestFrameAndDivider,     !- Name                                                 ",
                                                      "    0.05,                    !- Frame Width {m}                                      ",
                                                      "    0.05,                    !- Frame Outside Projection {m}                         ",
                                                      "    0.05,                    !- Frame Inside Projection {m}                          ",
                                                      "    5.0,                     !- Frame Conductance {W/m2-K}                           ",
                                                      "    1.2,                     !- Ratio of Frame-Edge Glass Conductance to Center-Of-Gl",
                                                      "    0.8,                     !- Frame Solar Absorptance                              ",
                                                      "    0.8,                     !- Frame Visible Absorptance                            ",
                                                      "    0.9,                     !- Frame Thermal Hemispherical Emissivity               ",
                                                      "    DividedLite,             !- Divider Type                                         ",
                                                      "    0.02,                    !- Divider Width {m}                                    ",
                                                      "    2,                       !- Number of Horizontal Dividers                        ",
                                                      "    2,                       !- Number of Vertical Dividers                          ",
                                                      "    0.02,                    !- Divider Outside Projection {m}                       ",
                                                      "    0.02,                    !- Divider Inside Projection {m}                        ",
                                                      "    5.0,                     !- Divider Conductance {W/m2-K}                         ",
                                                      "    1.2,                     !- Ratio of Divider-Edge Glass Conductance to Center-Of-",
                                                      "    0.8,                     !- Divider Solar Absorptance                            ",
                                                      "    0.8,                     !- Divider Visible Absorptance                          ",
                                                      "    0.9;                     !- Divider Thermal Hemispherical Emissivity             ",
                                                      "  Shading:Zone:Detailed,                                                             ",
                                                      "    Zn001:Wall-South:Shade001,  !- Name                                              ",
                                                      "    Zn001:Wall-South,        !- Base Surface Name                                    ",
                                                      "    SunShading,              !- Transmittance Schedule Name                          ",
                                                      "    4,                       !- Number of Vertices                                   ",
                                                      "    -3,-5,2.5,  !- X,Y,Z ==> Vertex 1 {m}                                            ",
                                                      "    -3,-6,2.5,  !- X,Y,Z ==> Vertex 2 {m}                                            ",
                                                      "    3,-6,2.5,  !- X,Y,Z ==> Vertex 3 {m}                                             ",
                                                      "    3,-5,2.5;  !- X,Y,Z ==> Vertex 4 {m}                                             ",
                                                      "  ShadingProperty:Reflectance,                                                       ",
                                                      "    Zn001:Wall-South:Shade001,  !- Shading Surface Name                              ",
                                                      "    0.2,                     !- Diffuse Solar Reflectance of Unglazed Part of Shading",
                                                      "    0.2;                     !- Diffuse Visible Reflectance of Unglazed Part of Shadi"});

    ASSERT_TRUE(process_idf(idf_objects));

    SolarShading::clear_state();

    SimulationManager::GetProjectData();
    bool FoundError = false;

    HeatBalanceManager::GetProjectControlData(FoundError); // read project control data
    EXPECT_FALSE(FoundError);                              // expect no errors

    HeatBalanceManager::SetPreConstructionInputParameters();
    ScheduleManager::ProcessScheduleInput(); // read schedules

    HeatBalanceManager::GetMaterialData(FoundError);
    EXPECT_FALSE(FoundError);

    HeatBalanceManager::GetFrameAndDividerData(FoundError);
    EXPECT_FALSE(FoundError);

    HeatBalanceManager::GetConstructData(FoundError);
    EXPECT_FALSE(FoundError);

    HeatBalanceManager::GetZoneData(FoundError); // Read Zone data from input file
    EXPECT_FALSE(FoundError);

    SurfaceGeometry::GetGeometryParameters(FoundError);
    EXPECT_FALSE(FoundError);

    SurfaceGeometry::CosZoneRelNorth.allocate(1);
    SurfaceGeometry::SinZoneRelNorth.allocate(1);

    SurfaceGeometry::CosZoneRelNorth(1) = std::cos(-Zone(1).RelNorth * DegToRadians);
    SurfaceGeometry::SinZoneRelNorth(1) = std::sin(-Zone(1).RelNorth * DegToRadians);
    SurfaceGeometry::CosBldgRelNorth = 1.0;
    SurfaceGeometry::SinBldgRelNorth = 0.0;

    compare_err_stream("");                         // just for debugging
    SurfaceGeometry::SetupZoneGeometry(FoundError); // this calls GetSurfaceData()
    EXPECT_FALSE(FoundError);

    SolarShading::AllocateModuleArrays();
    SolarShading::DetermineShadowingCombinations();
    DataEnvironment::DayOfYear_Schedule = 168;
    DataEnvironment::DayOfWeek = 6;
    DataGlobals::TimeStep = 4;
    DataGlobals::HourOfDay = 9;

    compare_err_stream(""); // just for debugging

    DataSurfaces::ShadingTransmittanceVaries = true;
    DataSystemVariables::DetailedSkyDiffuseAlgorithm = true;
    DataSystemVariables::UseScheduledSunlitFrac = true;
    SolarDistribution = FullExterior;

    CalcSkyDifShading = true;
    SolarShading::InitSolarCalculations();
    SolarShading::SkyDifSolarShading();
    CalcSkyDifShading = false;

    ScheduleManager::UpdateScheduleValues();
    DataBSDFWindow::SUNCOSTS(4, 9, 1) = 0.1;
    DataBSDFWindow::SUNCOSTS(4, 9, 2) = 0.1;
    DataBSDFWindow::SUNCOSTS(4, 9, 3) = 0.1;
    FigureSolarBeamAtTimestep(DataGlobals::HourOfDay, DataGlobals::TimeStep);

    EXPECT_TRUE(UseScheduledSunlitFrac);
    EXPECT_DOUBLE_EQ(0.5432, ScheduleManager::LookUpScheduleValue(2, 9, 4));
    EXPECT_FALSE(SolarShading::SUNCOS(3) < 0.00001);
    EXPECT_DOUBLE_EQ(0.00001, DataEnvironment::SunIsUpValue);
    ;
    EXPECT_FALSE(SolarShading::SUNCOS(3) < DataEnvironment::SunIsUpValue);

    EXPECT_DOUBLE_EQ(1, SunlitFrac(4, 9, 3));
    EXPECT_DOUBLE_EQ(1, SunlitFrac(4, 9, 6));
    EXPECT_DOUBLE_EQ(0.5432, SunlitFrac(4, 9, 9));
    SolarShading::clear_state();
}

TEST_F(EnergyPlusFixture, SolarShadingTest_DisableGroupSelfShading)
{
    std::string const idf_objects = delimited_string({"  Version,9.0;                                                                       ",
                                                      "  Building,                                                                          ",
                                                      "    DemoFDT,                 !- Name                                                 ",
                                                      "    0,                       !- North Axis {deg}                                     ",
                                                      "    Suburbs,                 !- Terrain                                              ",
                                                      "    3.9999999E-02,           !- Loads Convergence Tolerance Value                    ",
                                                      "    4.0000002E-03,           !- Temperature Convergence Tolerance Value {deltaC}     ",
                                                      "    FullExterior,            !- Solar Distribution                                   ",
                                                      "    ,                        !- Maximum Number of Warmup Days                        ",
                                                      "    6;                       !- Minimum Number of Warmup Days                        ",
                                                      "  ShadowCalculation,                                                                 ",
                                                      "    TimestepFrequency,          !- Calculation Method                                ",
                                                      "    ,                           !- Calculation Frequency                             ",
                                                      "    ,                           !- Maximum Figures in Shadow Overlap Calculations    ",
                                                      "    ,                           !- Polygon Clipping Algorithm                        ",
                                                      "    ,                           !- Sky Diffuse Modeling Algorithm                    ",
                                                      "    ,                           !- External Shading Calculation Method               ",
                                                      "    ,                           !- Output External Shading Calculation Results       ",
                                                      "    Yes,                        !- Disable Shading within A Zone Group               ",
                                                      "    ,                           !- Disable Shading between Zone Groups               ",
                                                      "    ShadingZoneGroup;           !- Shading Group 1 ZoneList Name                     ",
                                                      "  SurfaceConvectionAlgorithm:Inside,TARP;                                            ",
                                                      "  SurfaceConvectionAlgorithm:Outside,TARP;                                           ",
                                                      "  HeatBalanceAlgorithm,ConductionTransferFunction;                                   ",
                                                      "  Timestep,6;                                                                        ",
                                                      "  RunPeriod,                                                                         ",
                                                      "    ,                        !- Name                                                 ",
                                                      "    1,                       !- Begin Month                                          ",
                                                      "    1,                       !- Begin Day of Month                                   ",
                                                      "    ,                        !- Begin Year",
                                                      "    12,                      !- End Month                                            ",
                                                      "    31,                      !- End Day of Month                                     ",
                                                      "    ,                        !- End Year",
                                                      "    ,                        !- Day of Week for Start Day                            ",
                                                      "    ,                        !- Use Weather File Holidays and Special Days           ",
                                                      "    ,                        !- Use Weather File Daylight Saving Period              ",
                                                      "    ,                        !- Apply Weekend Holiday Rule                           ",
                                                      "    ,                        !- Use Weather File Rain Indicators                     ",
                                                      "    ;                        !- Use Weather File Snow Indicators                     ",
                                                      "  ScheduleTypeLimits,                                                                ",
                                                      "    Fraction,                !- Name                                                 ",
                                                      "    0.0,                     !- Lower Limit Value                                    ",
                                                      "    1.0,                     !- Upper Limit Value                                    ",
                                                      "    Continuous;              !- Numeric Type                                         ",
                                                      "  ScheduleTypeLimits,                                                                ",
                                                      "    ON/OFF,                  !- Name                                                 ",
                                                      "    0,                       !- Lower Limit Value                                    ",
                                                      "    1,                       !- Upper Limit Value                                    ",
                                                      "    Discrete;                !- Numeric Type                                         ",
                                                      "  Schedule:Compact,                                                                  ",
                                                      "    SunShading,              !- Name                                                 ",
                                                      "    ON/OFF,                  !- Schedule Type Limits Name                            ",
                                                      "    Through: 4/30,           !- Field 1                                              ",
                                                      "    For: AllDays,            !- Field 2                                              ",
                                                      "    until: 24:00,1,          !- Field 3                                              ",
                                                      "    Through: 10/31,          !- Field 5                                              ",
                                                      "    For: AllDays,            !- Field 6                                              ",
                                                      "    until: 24:00,0,          !- Field 7                                              ",
                                                      "    Through: 12/31,          !- Field 9                                              ",
                                                      "    For: AllDays,            !- Field 10                                             ",
                                                      "    until: 24:00,1;          !- Field 11                                             ",
                                                      "  Material,                                                                          ",
                                                      "    A2 - 4 IN DENSE FACE BRICK,  !- Name                                             ",
                                                      "    Rough,                   !- Roughness                                            ",
                                                      "    0.1014984,               !- Thickness {m}                                        ",
                                                      "    1.245296,                !- Conductivity {W/m-K}                                 ",
                                                      "    2082.400,                !- Density {kg/m3}                                      ",
                                                      "    920.4800,                !- Specific Heat {J/kg-K}                               ",
                                                      "    0.9000000,               !- Thermal Absorptance                                  ",
                                                      "    0.9300000,               !- Solar Absorptance                                    ",
                                                      "    0.9300000;               !- Visible Absorptance                                  ",
                                                      "  Material,                                                                          ",
                                                      "    E1 - 3 / 4 IN PLASTER OR GYP BOARD,  !- Name                                     ",
                                                      "    Smooth,                  !- Roughness                                            ",
                                                      "    1.9050000E-02,           !- Thickness {m}                                        ",
                                                      "    0.7264224,               !- Conductivity {W/m-K}                                 ",
                                                      "    1601.846,                !- Density {kg/m3}                                      ",
                                                      "    836.8000,                !- Specific Heat {J/kg-K}                               ",
                                                      "    0.9000000,               !- Thermal Absorptance                                  ",
                                                      "    0.9200000,               !- Solar Absorptance                                    ",
                                                      "    0.9200000;               !- Visible Absorptance                                  ",
                                                      "  Material,                                                                          ",
                                                      "    E2 - 1 / 2 IN SLAG OR STONE,  !- Name                                            ",
                                                      "    Rough,                   !- Roughness                                            ",
                                                      "    1.2710161E-02,           !- Thickness {m}                                        ",
                                                      "    1.435549,                !- Conductivity {W/m-K}                                 ",
                                                      "    881.0155,                !- Density {kg/m3}                                      ",
                                                      "    1673.600,                !- Specific Heat {J/kg-K}                               ",
                                                      "    0.9000000,               !- Thermal Absorptance                                  ",
                                                      "    0.5500000,               !- Solar Absorptance                                    ",
                                                      "    0.5500000;               !- Visible Absorptance                                  ",
                                                      "  Material,                                                                          ",
                                                      "    C12 - 2 IN HW CONCRETE,  !- Name                                                 ",
                                                      "    MediumRough,             !- Roughness                                            ",
                                                      "    5.0901599E-02,           !- Thickness {m}                                        ",
                                                      "    1.729577,                !- Conductivity {W/m-K}                                 ",
                                                      "    2242.585,                !- Density {kg/m3}                                      ",
                                                      "    836.8000,                !- Specific Heat {J/kg-K}                               ",
                                                      "    0.9000000,               !- Thermal Absorptance                                  ",
                                                      "    0.6500000,               !- Solar Absorptance                                    ",
                                                      "    0.6500000;               !- Visible Absorptance                                  ",
                                                      "  Material:NoMass,                                                                   ",
                                                      "    R13LAYER,                !- Name                                                 ",
                                                      "    Rough,                   !- Roughness                                            ",
                                                      "    2.290965,                !- Thermal Resistance {m2-K/W}                          ",
                                                      "    0.9000000,               !- Thermal Absorptance                                  ",
                                                      "    0.7500000,               !- Solar Absorptance                                    ",
                                                      "    0.7500000;               !- Visible Absorptance                                  ",
                                                      "  WindowMaterial:Glazing,                                                            ",
                                                      "    GLASS - CLEAR PLATE 1 / 4 IN,  !- Name                                           ",
                                                      "    SpectralAverage,         !- Optical Data Type                                    ",
                                                      "    ,                        !- Window Glass Spectral Data Set Name                  ",
                                                      "    0.006,                   !- Thickness {m}                                        ",
                                                      "    0.80,                    !- Solar Transmittance at Normal Incidence              ",
                                                      "    0.10,                    !- Front Side Solar Reflectance at Normal Incidence     ",
                                                      "    0.10,                    !- Back Side Solar Reflectance at Normal Incidence      ",
                                                      "    0.80,                    !- Visible Transmittance at Normal Incidence            ",
                                                      "    0.10,                    !- Front Side Visible Reflectance at Normal Incidence   ",
                                                      "    0.10,                    !- Back Side Visible Reflectance at Normal Incidence    ",
                                                      "    0.0,                     !- Infrared Transmittance at Normal Incidence           ",
                                                      "    0.84,                    !- Front Side Infrared Hemispherical Emissivity         ",
                                                      "    0.84,                    !- Back Side Infrared Hemispherical Emissivity          ",
                                                      "    0.9;                     !- Conductivity {W/m-K}                                 ",
                                                      "  WindowMaterial:Gas,                                                                ",
                                                      "    AIRGAP,                  !- Name                                                 ",
                                                      "    AIR,                     !- Gas Type                                             ",
                                                      "    0.0125;                  !- Thickness {m}                                        ",
                                                      "  Construction,                                                                      ",
                                                      "    R13WALL,                 !- Name                                                 ",
                                                      "    R13LAYER;                !- Outside Layer                                        ",
                                                      "  Construction,                                                                      ",
                                                      "    EXTWALL09,               !- Name                                                 ",
                                                      "    A2 - 4 IN DENSE FACE BRICK,  !- Outside Layer                                    ",
                                                      "    E1 - 3 / 4 IN PLASTER OR GYP BOARD;  !- Layer 4                                  ",
                                                      "  Construction,                                                                      ",
                                                      "    INTERIOR,                !- Name                                                 ",
                                                      "    C12 - 2 IN HW CONCRETE;  !- Layer 4                                              ",
                                                      "  Construction,                                                                      ",
                                                      "    SLAB FLOOR,              !- Name                                                 ",
                                                      "    C12 - 2 IN HW CONCRETE;  !- Layer 4                                              ",
                                                      "  Construction,                                                                      ",
                                                      "    ROOF31,                  !- Name                                                 ",
                                                      "    E2 - 1 / 2 IN SLAG OR STONE,  !- Outside Layer                                   ",
                                                      "    C12 - 2 IN HW CONCRETE;  !- Layer 4                                              ",
                                                      "  Construction,                                                                      ",
                                                      "    DOUBLE PANE HW WINDOW,   !- Name                                                 ",
                                                      "    GLASS - CLEAR PLATE 1 / 4 IN,  !- Outside Layer                                  ",
                                                      "    AIRGAP,                  !- Layer 2                                              ",
                                                      "    GLASS - CLEAR PLATE 1 / 4 IN;  !- Layer 3                                        ",
                                                      "  Construction,                                                                      ",
                                                      "    PARTITION02,             !- Name                                                 ",
                                                      "    E1 - 3 / 4 IN PLASTER OR GYP BOARD,  !- Outside Layer                            ",
                                                      "    C12 - 2 IN HW CONCRETE,  !- Layer 4                                              ",
                                                      "    E1 - 3 / 4 IN PLASTER OR GYP BOARD;  !- Layer 3                                  ",
                                                      "  Construction,                                                                      ",
                                                      "    single PANE HW WINDOW,   !- Name                                                 ",
                                                      "    GLASS - CLEAR PLATE 1 / 4 IN;  !- Outside Layer                                  ",
                                                      "  Construction,                                                                      ",
                                                      "    EXTWALLdemo,             !- Name                                                 ",
                                                      "    A2 - 4 IN DENSE FACE BRICK,  !- Outside Layer                                    ",
                                                      "    E1 - 3 / 4 IN PLASTER OR GYP BOARD;  !- Layer 4                                  ",
                                                      "  GlobalGeometryRules,                                                               ",
                                                      "    UpperLeftCorner,         !- Starting Vertex Position                             ",
                                                      "    Counterclockwise,        !- Vertex Entry Direction                               ",
                                                      "    Relative;                !- Coordinate System                                    ",
                                                      "  Zone,                                                                              ",
                                                      "    ZONE ONE,                !- Name                                                 ",
                                                      "    0,                       !- Direction of Relative North {deg}                    ",
                                                      "    0,                       !- X Origin {m}                                         ",
                                                      "    0,                       !- Y Origin {m}                                         ",
                                                      "    0,                       !- Z Origin {m}                                         ",
                                                      "    1,                       !- Type                                                 ",
                                                      "    1,                       !- Multiplier                                           ",
                                                      "    0,                       !- Ceiling Height {m}                                   ",
                                                      "    0;                       !- Volume {m3}                                          ",
                                                      "  ZoneList,                                                                          ",
                                                      "    ShadingZoneGroup,                                                                ",
                                                      "    ZONE ONE;                                                                        ",
                                                      "  BuildingSurface:Detailed,                                                          ",
                                                      "    Zn001:Wall-North,        !- Name                                                 ",
                                                      "    Wall,                    !- Surface Type                                         ",
                                                      "    EXTWALLdemo,             !- Construction Name                                    ",
                                                      "    ZONE ONE,                !- Zone Name                                            ",
                                                      "    Outdoors,                !- Outside Boundary Condition                           ",
                                                      "    ,                        !- Outside Boundary Condition Object                    ",
                                                      "    SunExposed,              !- Sun Exposure                                         ",
                                                      "    WindExposed,             !- Wind Exposure                                        ",
                                                      "    0.5000000,               !- View Factor to Ground                                ",
                                                      "    4,                       !- Number of Vertices                                   ",
                                                      "    5,5,3,  !- X,Y,Z ==> Vertex 1 {m}                                                ",
                                                      "    5,5,0,  !- X,Y,Z ==> Vertex 2 {m}                                                ",
                                                      "    -5,5,0,  !- X,Y,Z ==> Vertex 3 {m}                                               ",
                                                      "    -5,5,3;  !- X,Y,Z ==> Vertex 4 {m}                                               ",
                                                      "  BuildingSurface:Detailed,                                                          ",
                                                      "    Zn001:Wall-East,         !- Name                                                 ",
                                                      "    Wall,                    !- Surface Type                                         ",
                                                      "    EXTWALL09,               !- Construction Name                                    ",
                                                      "    ZONE ONE,                !- Zone Name                                            ",
                                                      "    Outdoors,                !- Outside Boundary Condition                           ",
                                                      "    ,                        !- Outside Boundary Condition Object                    ",
                                                      "    SunExposed,              !- Sun Exposure                                         ",
                                                      "    WindExposed,             !- Wind Exposure                                        ",
                                                      "    0.5000000,               !- View Factor to Ground                                ",
                                                      "    4,                       !- Number of Vertices                                   ",
                                                      "    5,-5,3,  !- X,Y,Z ==> Vertex 1 {m}                                               ",
                                                      "    5,-5,0,  !- X,Y,Z ==> Vertex 2 {m}                                               ",
                                                      "    5,5,0,  !- X,Y,Z ==> Vertex 3 {m}                                                ",
                                                      "    5,5,3;  !- X,Y,Z ==> Vertex 4 {m}                                                ",
                                                      "  BuildingSurface:Detailed,                                                          ",
                                                      "    Zn001:Wall-South,        !- Name                                                 ",
                                                      "    Wall,                    !- Surface Type                                         ",
                                                      "    R13WALL,                 !- Construction Name                                    ",
                                                      "    ZONE ONE,                !- Zone Name                                            ",
                                                      "    Outdoors,                !- Outside Boundary Condition                           ",
                                                      "    ,                        !- Outside Boundary Condition Object                    ",
                                                      "    SunExposed,              !- Sun Exposure                                         ",
                                                      "    WindExposed,             !- Wind Exposure                                        ",
                                                      "    0.5000000,               !- View Factor to Ground                                ",
                                                      "    4,                       !- Number of Vertices                                   ",
                                                      "    -5,-5,3,  !- X,Y,Z ==> Vertex 1 {m}                                              ",
                                                      "    -5,-5,0,  !- X,Y,Z ==> Vertex 2 {m}                                              ",
                                                      "    5,-5,0,  !- X,Y,Z ==> Vertex 3 {m}                                               ",
                                                      "    5,-5,3;  !- X,Y,Z ==> Vertex 4 {m}                                               ",
                                                      "  BuildingSurface:Detailed,                                                          ",
                                                      "    Zn001:Wall-West,         !- Name                                                 ",
                                                      "    Wall,                    !- Surface Type                                         ",
                                                      "    EXTWALL09,               !- Construction Name                                    ",
                                                      "    ZONE ONE,                !- Zone Name                                            ",
                                                      "    Outdoors,                !- Outside Boundary Condition                           ",
                                                      "    ,                        !- Outside Boundary Condition Object                    ",
                                                      "    SunExposed,              !- Sun Exposure                                         ",
                                                      "    WindExposed,             !- Wind Exposure                                        ",
                                                      "    0.5000000,               !- View Factor to Ground                                ",
                                                      "    4,                       !- Number of Vertices                                   ",
                                                      "    -5,5,3,  !- X,Y,Z ==> Vertex 1 {m}                                               ",
                                                      "    -5,5,0,  !- X,Y,Z ==> Vertex 2 {m}                                               ",
                                                      "    -5,-5,0,  !- X,Y,Z ==> Vertex 3 {m}                                              ",
                                                      "    -5,-5,3;  !- X,Y,Z ==> Vertex 4 {m}                                              ",
                                                      "  BuildingSurface:Detailed,                                                          ",
                                                      "    Zn001:roof,              !- Name                                                 ",
                                                      "    Roof,                    !- Surface Type                                         ",
                                                      "    ROOF31,                  !- Construction Name                                    ",
                                                      "    ZONE ONE,                !- Zone Name                                            ",
                                                      "    Outdoors,                !- Outside Boundary Condition                           ",
                                                      "    ,                        !- Outside Boundary Condition Object                    ",
                                                      "    SunExposed,              !- Sun Exposure                                         ",
                                                      "    WindExposed,             !- Wind Exposure                                        ",
                                                      "    0.0000000,               !- View Factor to Ground                                ",
                                                      "    4,                       !- Number of Vertices                                   ",
                                                      "    -5,-5,3,  !- X,Y,Z ==> Vertex 1 {m}                                              ",
                                                      "    5,-5,3,  !- X,Y,Z ==> Vertex 2 {m}                                               ",
                                                      "    5,5,3,  !- X,Y,Z ==> Vertex 3 {m}                                                ",
                                                      "    -5,5,3;  !- X,Y,Z ==> Vertex 4 {m}                                               ",
                                                      "  Schedule:Compact,                                                                  ",
                                                      "    ExtShadingSch:Zn001:roof,                                                        ",
                                                      "    Fraction,                !- Schedule Type Limits Name                            ",
                                                      "    Through: 12/31,          !- Field 1                                              ",
                                                      "    For: AllDays,            !- Field 2                                              ",
                                                      "    Until: 24:00,            !- Field 3                                              ",
                                                      "    0.5432;                  !- Field 4                                              ",
                                                      "  BuildingSurface:Detailed,                                                          ",
                                                      "    Zn001:floor,             !- Name                                                 ",
                                                      "    Floor,                   !- Surface Type                                         ",
                                                      "    SLAB FLOOR,              !- Construction Name                                    ",
                                                      "    ZONE ONE,                !- Zone Name                                            ",
                                                      "    Outdoors,                !- Outside Boundary Condition                           ",
                                                      "    ,                        !- Outside Boundary Condition Object                    ",
                                                      "    SunExposed,              !- Sun Exposure                                         ",
                                                      "    WindExposed,             !- Wind Exposure                                        ",
                                                      "    0.0000000,               !- View Factor to Ground                                ",
                                                      "    4,                       !- Number of Vertices                                   ",
                                                      "    -5,5,0,  !- X,Y,Z ==> Vertex 1 {m}                                               ",
                                                      "    5,5,0,  !- X,Y,Z ==> Vertex 2 {m}                                                ",
                                                      "    5,-5,0,  !- X,Y,Z ==> Vertex 3 {m}                                               ",
                                                      "    -5,-5,0;  !- X,Y,Z ==> Vertex 4 {m}                                              ",
                                                      "  FenestrationSurface:Detailed,                                                      ",
                                                      "    Zn001:Wall-South:Win001, !- Name                                                 ",
                                                      "    Window,                  !- Surface Type                                         ",
                                                      "    DOUBLE PANE HW WINDOW,   !- Construction Name                                    ",
                                                      "    Zn001:Wall-South,        !- Building Surface Name                                ",
                                                      "    ,                        !- Outside Boundary Condition Object                    ",
                                                      "    0.5000000,               !- View Factor to Ground                                ",
                                                      "    TestFrameAndDivider,     !- Frame and Divider Name                               ",
                                                      "    1.0,                     !- Multiplier                                           ",
                                                      "    4,                       !- Number of Vertices                                   ",
                                                      "    -3,-5,2.5,  !- X,Y,Z ==> Vertex 1 {m}                                            ",
                                                      "    -3,-5,0.5,  !- X,Y,Z ==> Vertex 2 {m}                                            ",
                                                      "    3,-5,0.5,  !- X,Y,Z ==> Vertex 3 {m}                                             ",
                                                      "    3,-5,2.5;  !- X,Y,Z ==> Vertex 4 {m}                                             ",
                                                      "  WindowProperty:FrameAndDivider,                                                    ",
                                                      "    TestFrameAndDivider,     !- Name                                                 ",
                                                      "    0.05,                    !- Frame Width {m}                                      ",
                                                      "    0.05,                    !- Frame Outside Projection {m}                         ",
                                                      "    0.05,                    !- Frame Inside Projection {m}                          ",
                                                      "    5.0,                     !- Frame Conductance {W/m2-K}                           ",
                                                      "    1.2,                     !- Ratio of Frame-Edge Glass Conductance to Center-Of-Gl",
                                                      "    0.8,                     !- Frame Solar Absorptance                              ",
                                                      "    0.8,                     !- Frame Visible Absorptance                            ",
                                                      "    0.9,                     !- Frame Thermal Hemispherical Emissivity               ",
                                                      "    DividedLite,             !- Divider Type                                         ",
                                                      "    0.02,                    !- Divider Width {m}                                    ",
                                                      "    2,                       !- Number of Horizontal Dividers                        ",
                                                      "    2,                       !- Number of Vertical Dividers                          ",
                                                      "    0.02,                    !- Divider Outside Projection {m}                       ",
                                                      "    0.02,                    !- Divider Inside Projection {m}                        ",
                                                      "    5.0,                     !- Divider Conductance {W/m2-K}                         ",
                                                      "    1.2,                     !- Ratio of Divider-Edge Glass Conductance to Center-Of-",
                                                      "    0.8,                     !- Divider Solar Absorptance                            ",
                                                      "    0.8,                     !- Divider Visible Absorptance                          ",
                                                      "    0.9;                     !- Divider Thermal Hemispherical Emissivity             ",
                                                      "  Shading:Zone:Detailed,                                                             ",
                                                      "    Zn001:Wall-South:Shade001,  !- Name                                              ",
                                                      "    Zn001:Wall-South,        !- Base Surface Name                                    ",
                                                      "    SunShading,              !- Transmittance Schedule Name                          ",
                                                      "    4,                       !- Number of Vertices                                   ",
                                                      "    -3,-5,2.5,  !- X,Y,Z ==> Vertex 1 {m}                                            ",
                                                      "    -3,-6,2.5,  !- X,Y,Z ==> Vertex 2 {m}                                            ",
                                                      "    3,-6,2.5,  !- X,Y,Z ==> Vertex 3 {m}                                             ",
                                                      "    3,-5,2.5;  !- X,Y,Z ==> Vertex 4 {m}                                             ",
                                                      "  ShadingProperty:Reflectance,                                                       ",
                                                      "    Zn001:Wall-South:Shade001,  !- Shading Surface Name                              ",
                                                      "    0.2,                     !- Diffuse Solar Reflectance of Unglazed Part of Shading",
                                                      "    0.2;                     !- Diffuse Visible Reflectance of Unglazed Part of Shadi"});

    ASSERT_TRUE(process_idf(idf_objects));

    SolarShading::clear_state();

    SimulationManager::GetProjectData();
    bool FoundError = false;

    HeatBalanceManager::GetProjectControlData(FoundError); // read project control data
    EXPECT_FALSE(FoundError);                              // expect no errors

    HeatBalanceManager::SetPreConstructionInputParameters();
    ScheduleManager::ProcessScheduleInput(); // read schedules

    HeatBalanceManager::GetMaterialData(FoundError);
    EXPECT_FALSE(FoundError);

    HeatBalanceManager::GetFrameAndDividerData(FoundError);
    EXPECT_FALSE(FoundError);

    HeatBalanceManager::GetConstructData(FoundError);
    EXPECT_FALSE(FoundError);

    HeatBalanceManager::GetZoneData(FoundError); // Read Zone data from input file
    EXPECT_FALSE(FoundError);

    SurfaceGeometry::GetGeometryParameters(FoundError);
    EXPECT_FALSE(FoundError);

    SizingManager::GetZoneSizingInput();

    SurfaceGeometry::CosZoneRelNorth.allocate(1);
    SurfaceGeometry::SinZoneRelNorth.allocate(1);

    SurfaceGeometry::CosZoneRelNorth(1) = std::cos(-Zone(1).RelNorth * DegToRadians);
    SurfaceGeometry::SinZoneRelNorth(1) = std::sin(-Zone(1).RelNorth * DegToRadians);
    SurfaceGeometry::CosBldgRelNorth = 1.0;
    SurfaceGeometry::SinBldgRelNorth = 0.0;

    compare_err_stream("");                         // just for debugging
    SurfaceGeometry::SetupZoneGeometry(FoundError); // this calls GetSurfaceData()
    EXPECT_FALSE(FoundError);

    compare_err_stream(""); // just for debugging

    SolarShading::GetShadowingInput();

    for (int SurfNum = 1; SurfNum <= TotSurfaces; SurfNum++) {
        if (Surface(SurfNum).ExtBoundCond == 0 && Surface(SurfNum).Zone != 0) {
            int ZoneSize = Surface(SurfNum).DisabledShadowingZoneList.size();
            EXPECT_EQ(1, ZoneSize);
            std::vector<int> DisabledZones = Surface(SurfNum).DisabledShadowingZoneList;
            for (int i : DisabledZones) {
                EXPECT_EQ(1, i);
            }
        }
    }
}
