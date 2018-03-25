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

// EnergyPlus::MoistureBalanceEMPD Tests

// Google Test Headers
#include <gtest/gtest.h>

// EnergyPlus Headers
#include "Fixtures/EnergyPlusFixture.hh"
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataGlobals.hh>
#include <EnergyPlus/DataHeatBalFanSys.hh>
#include <EnergyPlus/DataHeatBalance.hh>
#include <EnergyPlus/DataMoistureBalance.hh>
#include <EnergyPlus/DataMoistureBalanceEMPD.hh>
#include <EnergyPlus/DataSurfaces.hh>
#include <EnergyPlus/HeatBalanceManager.hh>
#include <EnergyPlus/InputProcessing/InputProcessor.hh>
#include <EnergyPlus/MoistureBalanceEMPDManager.hh>

using namespace EnergyPlus;

TEST_F(EnergyPlusFixture, CheckEMPDCalc)
{
    std::string const idf_objects =
        delimited_string({"Version, 8.6;",
                          "Material,",
                          "Concrete,                !- Name",
                          "Rough,                   !- Roughness",
                          "0.152,                   !- Thickness {m}",
                          "0.3,                     !- Conductivity {W/m-K}",
                          "1000,                    !- Density {kg/m3}",
                          "950,                     !- Specific Heat {J/kg-K}",
                          "0.900000,                !- Thermal Absorptance",
                          "0.600000,                !- Solar Absorptance",
                          "0.600000;                !- Visible Absorptance",
                          "MaterialProperty:MoisturePenetrationDepth:Settings,",
                          "Concrete,                !- Name",
                          "6.554,                     !- Water Vapor Diffusion Resistance Factor {dimensionless} (mu)",
                          "0.0661,                   !- Moisture Equation Coefficient a {dimensionless} (MoistACoeff)",
                          "1,                       !- Moisture Equation Coefficient b {dimensionless} (MoistBCoeff)",
                          "0,                       !- Moisture Equation Coefficient c {dimensionless} (MoistCCoeff)",
                          "1,                       !- Moisture Equation Coefficient d {dimensionless} (MoistDCoeff)",
                          "0.006701,                    !- Surface-layer penetrtion depth {m} (dEMPD)",
                          "0.013402,                    !- Deep-layer penetration depth {m} (dEPMDdeep)",
                          "0,                       !- Coating layer permability {m} (CoatingThickness)",
                          "1;                       !- Coating layer water vapor diffusion resistance factor {dimensionless} (muCoating)"});

    ASSERT_TRUE(process_idf(idf_objects));

    bool errors_found(false);
    HeatBalanceManager::GetMaterialData(errors_found);
    ASSERT_FALSE(errors_found) << "Errors in GetMaterialData";

    // Surface
    using DataSurfaces::TotSurfaces;
    TotSurfaces = 1;
    DataSurfaces::Surface.allocate(TotSurfaces);
    DataSurfaces::SurfaceData &surface = DataSurfaces::Surface(1);
    surface.Name = "Surface1";
    surface.Area = 1.0;
    surface.HeatTransSurf = true;

    // Zone
    surface.Zone = 1;
    DataHeatBalFanSys::ZoneAirHumRat.allocate(1);
    DataMoistureBalance::RhoVaporAirIn.allocate(1);
    DataMoistureBalance::HMassConvInFD.allocate(1);
    DataHeatBalFanSys::MAT.allocate(1);
    DataHeatBalFanSys::MAT(1) = 20.0;
    DataHeatBalFanSys::ZoneAirHumRat(1) = 0.0061285406810457849;

    // Construction
    surface.Construction = 1;
    DataHeatBalance::Construct.allocate(1);
    DataHeatBalance::ConstructionData &construction = DataHeatBalance::Construct(1);
    construction.TotLayers = 1;
    construction.LayerPoint(construction.TotLayers) = UtilityRoutines::FindItemInList("CONCRETE", DataHeatBalance::Material);

    // Initialize and get inputs
    MoistureBalanceEMPDManager::InitMoistureBalanceEMPD();

    // Set up conditions
    DataGlobals::TimeStepZone = 0.25;
    DataEnvironment::OutBaroPress = 101325.;
    DataMoistureBalanceEMPD::RVSurface(1) = 0.007077173214149593;
    DataMoistureBalanceEMPD::RVSurfaceOld(1) = DataMoistureBalanceEMPD::RVSurface(1);
    DataMoistureBalance::HMassConvInFD(1) = 0.0016826898264131584;
    DataMoistureBalance::RhoVaporAirIn(1) = 0.0073097913062508896;
    DataMoistureBalanceEMPD::RVSurfLayer(1) = 0.007038850125652322;
    DataMoistureBalanceEMPD::RVDeepLayer(1) = 0.0051334905162138695;
    DataMoistureBalanceEMPD::RVdeepOld(1) = 0.0051334905162138695;
    DataMoistureBalanceEMPD::RVSurfLayerOld(1) = 0.007038850125652322;

    // Do calcs
    Real64 Tsat(0.0);
    MoistureBalanceEMPDManager::CalcMoistureBalanceEMPD(1, 19.907302679986064, 19.901185713164697, Tsat);

    auto const &report_vars = MoistureBalanceEMPDManager::EMPDReportVars(1);
    EXPECT_DOUBLE_EQ(6.3445188238394508, Tsat);
    EXPECT_DOUBLE_EQ(0.0071762141417078054, DataMoistureBalanceEMPD::RVSurface(1));
    EXPECT_DOUBLE_EQ(0.00000076900234067835945, report_vars.mass_flux_deep);
    EXPECT_DOUBLE_EQ(-0.00000019077843350248091, report_vars.mass_flux_zone);
    EXPECT_DOUBLE_EQ(0.0070186500259181136, DataMoistureBalanceEMPD::RVSurfLayer(1));
    EXPECT_DOUBLE_EQ(0.0051469229632164605, DataMoistureBalanceEMPD::RVDeepLayer(1));
    EXPECT_DOUBLE_EQ(-0.47694608375620229, DataMoistureBalanceEMPD::HeatFluxLatent(1));

    // Clean up
    DataHeatBalFanSys::ZoneAirHumRat.deallocate();
    DataMoistureBalance::RhoVaporAirIn.deallocate();
}

TEST_F(EnergyPlusFixture, EMPDAutocalcDepth)
{
    std::string const idf_objects =
        delimited_string({"Version, 8.6;",
                          "Material,",
                          "Concrete,                !- Name",
                          "Rough,                   !- Roughness",
                          "0.152,                   !- Thickness {m}",
                          "0.3,                     !- Conductivity {W/m-K}",
                          "850,                     !- Density {kg/m3}",
                          "950,                     !- Specific Heat {J/kg-K}",
                          "0.900000,                !- Thermal Absorptance",
                          "0.600000,                !- Solar Absorptance",
                          "0.600000;                !- Visible Absorptance",
                          "MaterialProperty:MoisturePenetrationDepth:Settings,",
                          "Concrete,                !- Name",
                          "8,                     !- Water Vapor Diffusion Resistance Factor {dimensionless} (mu)",
                          "0.012,                   !- Moisture Equation Coefficient a {dimensionless} (MoistACoeff)",
                          "1,                       !- Moisture Equation Coefficient b {dimensionless} (MoistBCoeff)",
                          "0,                       !- Moisture Equation Coefficient c {dimensionless} (MoistCCoeff)",
                          "1,                       !- Moisture Equation Coefficient d {dimensionless} (MoistDCoeff)",
                          ",                    !- Surface-layer penetrtion depth {m} (dEMPD)",
                          "autocalculate,                    !- Deep-layer penetration depth {m} (dEPMDdeep)",
                          "0,                       !- Coating layer permability {m} (CoatingThickness)",
                          "1;                       !- Coating layer water vapor diffusion resistance factor {dimensionless} (muCoating)"});

    ASSERT_TRUE(process_idf(idf_objects));

    bool errors_found(false);
    HeatBalanceManager::GetMaterialData(errors_found);
    ASSERT_FALSE(errors_found) << "Errors in GetMaterialData";
    MoistureBalanceEMPDManager::GetMoistureBalanceEMPDInput();

    const DataHeatBalance::MaterialProperties &material = DataHeatBalance::Material(1);
    ASSERT_NEAR(material.EMPDSurfaceDepth, 0.014143, 0.000001);
    ASSERT_NEAR(material.EMPDDeepDepth, 0.064810, 0.000001);
}

TEST_F(EnergyPlusFixture, EMPDRcoating)
{
    std::string const idf_objects =
        delimited_string({"Version, 8.6;",
                          "Material,",
                          "Concrete,                !- Name",
                          "Rough,                   !- Roughness",
                          "0.152,                   !- Thickness {m}",
                          "0.3,                     !- Conductivity {W/m-K}",
                          "1000,                    !- Density {kg/m3}",
                          "950,                     !- Specific Heat {J/kg-K}",
                          "0.900000,                !- Thermal Absorptance",
                          "0.600000,                !- Solar Absorptance",
                          "0.600000;                !- Visible Absorptance",
                          "MaterialProperty:MoisturePenetrationDepth:Settings,",
                          "Concrete,                !- Name",
                          "6.554,                     !- Water Vapor Diffusion Resistance Factor {dimensionless} (mu)",
                          "0.0661,                   !- Moisture Equation Coefficient a {dimensionless} (MoistACoeff)",
                          "1,                       !- Moisture Equation Coefficient b {dimensionless} (MoistBCoeff)",
                          "0,                       !- Moisture Equation Coefficient c {dimensionless} (MoistCCoeff)",
                          "1,                       !- Moisture Equation Coefficient d {dimensionless} (MoistDCoeff)",
                          "0.006701,                    !- Surface-layer penetrtion depth {m} (dEMPD)",
                          "0.013402,                    !- Deep-layer penetration depth {m} (dEPMDdeep)",
                          "0.002,                       !- Coating layer permability {m} (CoatingThickness)",
                          "1;                       !- Coating layer water vapor diffusion resistance factor {dimensionless} (muCoating)"});

    ASSERT_TRUE(process_idf(idf_objects));

    bool errors_found(false);
    HeatBalanceManager::GetMaterialData(errors_found);
    ASSERT_FALSE(errors_found) << "Errors in GetMaterialData";

    // Surface
    using DataSurfaces::TotSurfaces;
    TotSurfaces = 1;
    DataSurfaces::Surface.allocate(TotSurfaces);
    DataSurfaces::SurfaceData &surface = DataSurfaces::Surface(1);
    surface.Name = "Surface1";
    surface.Area = 1.0;
    surface.HeatTransSurf = true;

    // Zone
    surface.Zone = 1;
    DataHeatBalFanSys::ZoneAirHumRat.allocate(1);
    DataMoistureBalance::RhoVaporAirIn.allocate(1);
    DataMoistureBalance::HMassConvInFD.allocate(1);
    DataHeatBalFanSys::MAT.allocate(1);
    DataHeatBalFanSys::MAT(1) = 20.0;
    DataHeatBalFanSys::ZoneAirHumRat(1) = 0.0061285406810457849;

    // Construction
    surface.Construction = 1;
    DataHeatBalance::Construct.allocate(1);
    DataHeatBalance::ConstructionData &construction = DataHeatBalance::Construct(1);
    construction.TotLayers = 1;
    construction.LayerPoint(construction.TotLayers) = UtilityRoutines::FindItemInList("CONCRETE", DataHeatBalance::Material);

    // Initialize and get inputs
    MoistureBalanceEMPDManager::InitMoistureBalanceEMPD();

    // Set up conditions
    DataGlobals::TimeStepZone = 0.25;
    DataEnvironment::OutBaroPress = 101325.;
    DataMoistureBalanceEMPD::RVSurface(1) = 0.007077173214149593;
    DataMoistureBalanceEMPD::RVSurfaceOld(1) = DataMoistureBalanceEMPD::RVSurface(1);
    DataMoistureBalance::HMassConvInFD(1) = 0.0016826898264131584;
    DataMoistureBalance::RhoVaporAirIn(1) = 0.0073097913062508896;
    DataMoistureBalanceEMPD::RVSurfLayer(1) = 0.007038850125652322;
    DataMoistureBalanceEMPD::RVDeepLayer(1) = 0.0051334905162138695;
    DataMoistureBalanceEMPD::RVdeepOld(1) = 0.0051334905162138695;
    DataMoistureBalanceEMPD::RVSurfLayerOld(1) = 0.007038850125652322;

    // Do calcs
    Real64 Tsat(0.0);
    MoistureBalanceEMPDManager::CalcMoistureBalanceEMPD(1, 19.907302679986064, 19.901185713164697, Tsat);

    auto const &report_vars = MoistureBalanceEMPDManager::EMPDReportVars(1);
    EXPECT_DOUBLE_EQ(6.3445188238394508, Tsat);
    EXPECT_DOUBLE_EQ(0.0071815819413115663, DataMoistureBalanceEMPD::RVSurface(1));
    EXPECT_DOUBLE_EQ(0.00000076900234067835945, report_vars.mass_flux_deep);
    EXPECT_DOUBLE_EQ(-1.8118197009111738e-07, report_vars.mass_flux_zone);
    EXPECT_DOUBLE_EQ(0.0070183147759991828, DataMoistureBalanceEMPD::RVSurfLayer(1));
    EXPECT_DOUBLE_EQ(0.0051469229632164605, DataMoistureBalanceEMPD::RVDeepLayer(1));
    EXPECT_DOUBLE_EQ(-0.45295492522779346, DataMoistureBalanceEMPD::HeatFluxLatent(1));

    // Clean up
    DataHeatBalFanSys::ZoneAirHumRat.deallocate();
    DataMoistureBalance::RhoVaporAirIn.deallocate();
}
