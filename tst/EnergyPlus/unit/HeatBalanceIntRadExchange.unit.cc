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

// EnergyPlus::HeatBalFiniteDiffManager Unit Tests

// Google Test Headers
#include <gtest/gtest.h>

// EnergyPlus Headers
#include <EnergyPlus/Construction.hh>
#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/DataHeatBalSurface.hh>
#include <EnergyPlus/DataHeatBalance.hh>
#include <EnergyPlus/DataSurfaces.hh>
#include <EnergyPlus/DataViewFactorInformation.hh>
#include <EnergyPlus/HeatBalanceIntRadExchange.hh>
#include <EnergyPlus/HeatBalanceManager.hh>
#include <EnergyPlus/Material.hh>

#include "Fixtures/EnergyPlusFixture.hh"

using namespace EnergyPlus::HeatBalanceIntRadExchange;

namespace EnergyPlus {

TEST_F(EnergyPlusFixture, HeatBalanceIntRadExchange_CarrollMRT)
{
    int N;                 // NUMBER OF SURFACES
    Array1D<Real64> A;     // AREA VECTOR- ASSUMED,BE N ELEMENTS LONG
    Array1D<Real64> FMRT;  // MRT "VIEW FACTORS"
    Array1D<Real64> EMISS; // Gray body emissivities
    Array1D<Real64> Fp;    // Gray body radiative resistance

    // Three surfaces of equal size
    N = 3;

    A.allocate(N);
    A(1) = 1.0;
    A(2) = 1.0;
    A(3) = 1.0;

    FMRT.allocate(N);
    CalcFMRT(*state, N, A, FMRT);

    EMISS.allocate(N);
    EMISS(1) = 1.0;
    EMISS(2) = 1.0;
    EMISS(3) = 1.0;

    Fp.allocate(N);
    CalcFp(N, EMISS, FMRT, Fp);

    EXPECT_NEAR(FMRT(1), 1.5, 0.001);
    EXPECT_NEAR(FMRT(2), 1.5, 0.001);
    EXPECT_NEAR(FMRT(3), 1.5, 0.001);

    // Special case where surfaces are equal area (each 50% of total).
    N = 2;

    A.redimension(N);
    A(1) = 1.0;
    A(2) = 1.0;

    FMRT.redimension(N);

    CalcFMRT(*state, N, A, FMRT);

    EXPECT_NEAR(FMRT(1), 2.0, 0.001);
    EXPECT_NEAR(FMRT(2), 2.0, 0.001);

    EMISS.redimension(N);
    EMISS(1) = 1.0;
    EMISS(2) = 1.0;

    Fp.redimension(N);
    CalcFp(N, EMISS, FMRT, Fp);

    // Imbalanced areas
    A(1) = 2.0;
    A(2) = 1.0;

    CalcFMRT(*state, N, A, FMRT);

    std::string const error_string = delimited_string({"   ** Severe  ** Geometry not compatible with Carroll MRT Zone Radiant Exchange method."});
    EXPECT_TRUE(compare_err_stream(error_string, true));

    CalcFp(N, EMISS, FMRT, Fp);
}

TEST_F(EnergyPlusFixture, HeatBalanceIntRadExchange_FixViewFactorsTest)
{

    int N;                     // NUMBER OF SURFACES
    Array1D<Real64> A;         // AREA VECTOR- ASSUMED,BE N ELEMENTS LONG
    Array2D<Real64> F;         // APPROXIMATE DIRECT VIEW FACTOR MATRIX (N X N)
    int ZoneNum;               // Zone number being fixed
    Real64 OriginalCheckValue; // check of SUM(F) - N
    Real64 FixedCheckValue;    // check after fixed of SUM(F) - N
    Real64 FinalCheckValue;    // the one to go with
    int NumIterations;         // number of iterations to fixed
    Real64 RowSum;             // RowSum of Fixed
    bool anyIntMassInZone;

    anyIntMassInZone = false;
    N = 3;

    A.allocate(N);
    F.allocate(N, N);

    A(1) = 1.0;
    A(2) = 1.0;
    A(3) = 1.0;

    F(1, 1) = 0.0;
    F(1, 2) = 0.5;
    F(1, 3) = 0.5;
    F(2, 1) = 0.5;
    F(2, 2) = 0.0;
    F(2, 3) = 0.5;
    F(3, 1) = 0.5;
    F(3, 2) = 0.5;
    F(3, 3) = 0.0;

    ZoneNum = 1;

    state->dataHeatBal->Zone.allocate(ZoneNum);
    state->dataHeatBal->Zone(ZoneNum).Name = "Test";
    state->dataViewFactor->ZoneRadiantInfo.allocate(ZoneNum);
    state->dataViewFactor->ZoneRadiantInfo(ZoneNum).Name = state->dataHeatBal->Zone(ZoneNum).Name;
    state->dataViewFactor->ZoneRadiantInfo(ZoneNum).ZoneNums.push_back(ZoneNum);

    FixViewFactors(*state,
                   N,
                   A,
                   F,
                   state->dataViewFactor->ZoneRadiantInfo(ZoneNum).Name,
                   state->dataViewFactor->ZoneRadiantInfo(ZoneNum).ZoneNums,
                   OriginalCheckValue,
                   FixedCheckValue,
                   FinalCheckValue,
                   NumIterations,
                   RowSum,
                   anyIntMassInZone);

    std::string const error_string = delimited_string({
        "   ** Warning ** Surfaces in Zone/Enclosure=\"Test\" do not define an enclosure.",
        "   **   ~~~   ** Number of surfaces <= 3, view factors are set to force reciprocity but may not fulfill completeness.",
        "   **   ~~~   ** Reciprocity means that radiant exchange between two surfaces will match and not lead to an energy loss.",
        "   **   ~~~   ** Completeness means that all of the view factors between a surface and the other surfaces in a zone add up to unity.",
        "   **   ~~~   ** So, when there are three or less surfaces in a zone, EnergyPlus will make sure there are no losses of energy but",
        "   **   ~~~   ** it will not exchange the full amount of radiation with the rest of the zone as it would if there was a completed "
        "enclosure.",
    });

    EXPECT_TRUE(compare_err_stream(error_string, true));

    // Tests for correction of view factors based on GitHub Issue #5772

    A(1) = 20.0;
    A(2) = 180.0;
    A(3) = 180.0;
    F(1, 1) = 0.0;
    F(1, 2) = 0.5;
    F(1, 3) = 0.5;
    F(2, 1) = 0.1;
    F(2, 2) = 0.0;
    F(2, 3) = 0.9;
    F(3, 1) = 0.1;
    F(3, 2) = 0.9;
    F(3, 3) = 0.0;

    FixViewFactors(*state,
                   N,
                   A,
                   F,
                   state->dataViewFactor->ZoneRadiantInfo(ZoneNum).Name,
                   state->dataViewFactor->ZoneRadiantInfo(ZoneNum).ZoneNums,
                   OriginalCheckValue,
                   FixedCheckValue,
                   FinalCheckValue,
                   NumIterations,
                   RowSum,
                   anyIntMassInZone);

    EXPECT_NEAR(F(1, 2), 0.07986, 0.001);
    EXPECT_NEAR(F(2, 1), 0.71875, 0.001);
    EXPECT_NEAR(F(3, 2), 0.28125, 0.001);

    A(1) = 100.0;
    A(2) = 100.0;
    A(3) = 200.0;
    F(1, 1) = 0.0;
    F(1, 2) = 1.0 / 3.0;
    F(1, 3) = 2.0 / 3.0;
    F(2, 1) = 1.0 / 3.0;
    F(2, 2) = 0.0;
    F(2, 3) = 2.0 / 3.0;
    F(3, 1) = 0.5;
    F(3, 2) = 0.5;
    F(3, 3) = 0.0;

    FixViewFactors(*state,
                   N,
                   A,
                   F,
                   state->dataViewFactor->ZoneRadiantInfo(ZoneNum).Name,
                   state->dataViewFactor->ZoneRadiantInfo(ZoneNum).ZoneNums,
                   OriginalCheckValue,
                   FixedCheckValue,
                   FinalCheckValue,
                   NumIterations,
                   RowSum,
                   anyIntMassInZone);

    EXPECT_NEAR(F(1, 2), 0.181818, 0.001);
    EXPECT_NEAR(F(2, 3), 0.25, 0.001);
    EXPECT_NEAR(F(3, 2), 0.5, 0.001);

    A(1) = 100.0;
    A(2) = 150.0;
    A(3) = 200.0;
    F(1, 1) = 0.0;
    F(1, 2) = 150.0 / 350.0;
    F(1, 3) = 200.0 / 350.0;
    F(2, 1) = 1.0 / 3.0;
    F(2, 2) = 0.0;
    F(2, 3) = 2.0 / 3.0;
    F(3, 1) = 0.4;
    F(3, 2) = 0.6;
    F(3, 3) = 0.0;

    FixViewFactors(*state,
                   N,
                   A,
                   F,
                   state->dataViewFactor->ZoneRadiantInfo(ZoneNum).Name,
                   state->dataViewFactor->ZoneRadiantInfo(ZoneNum).ZoneNums,
                   OriginalCheckValue,
                   FixedCheckValue,
                   FinalCheckValue,
                   NumIterations,
                   RowSum,
                   anyIntMassInZone);

    EXPECT_NEAR(F(1, 2), 0.21466, 0.001);
    EXPECT_NEAR(F(1, 3), 0.25445, 0.001);
    EXPECT_NEAR(F(2, 1), 0.32199, 0.001);
    EXPECT_NEAR(F(2, 3), 0.36832, 0.001);
    EXPECT_NEAR(F(3, 1), 0.50890, 0.001);
    EXPECT_NEAR(F(3, 2), 0.49110, 0.001);

    A.deallocate();
    F.deallocate();
    // Tests for correction of view factors based on GitHub Issue #8700 (when one
    // surface is much larger than other surfaces, N > 3) The following is a new
    // test that demonstrates the new correction when one surface is almost as large
    // as everything else. It helps with arriving at a stable group of view factors
    // that do not cause odd fluctuations in the results.
    N = 4;

    A.allocate(N);
    F.allocate(N, N);

    A(1) = 100.0;
    A(2) = 50.0;
    A(3) = 25.0;
    A(4) = 25.0;
    F(1, 1) = 0.0;
    F(1, 2) = 0.5;
    F(1, 3) = 0.25;
    F(1, 4) = 0.25;
    F(2, 1) = 2.0 / 3.0;
    F(2, 2) = 0.0;
    F(2, 3) = 1.0 / 6.0;
    F(2, 4) = 1.0 / 6.0;
    F(3, 1) = 4.0 / 7.0;
    F(3, 2) = 2.0 / 7.0;
    F(3, 3) = 0.0;
    F(3, 4) = 1.0 / 7.0;
    F(4, 1) = 4.0 / 7.0;
    F(4, 2) = 2.0 / 7.0;
    F(4, 3) = 1.0 / 7.0;
    F(4, 4) = 0.0;

    FixViewFactors(*state,
                   N,
                   A,
                   F,
                   state->dataViewFactor->ZoneRadiantInfo(ZoneNum).Name,
                   state->dataViewFactor->ZoneRadiantInfo(ZoneNum).ZoneNums,
                   OriginalCheckValue,
                   FixedCheckValue,
                   FinalCheckValue,
                   NumIterations,
                   RowSum,
                   anyIntMassInZone);

    EXPECT_NEAR(F(1, 1), 0.31747, 0.001);
    EXPECT_NEAR(F(1, 2), 0.71788, 0.001);
    EXPECT_NEAR(F(1, 3), 0.64862, 0.001);
    EXPECT_NEAR(F(1, 4), 0.64862, 0.001);
    EXPECT_NEAR(F(2, 1), 0.35894, 0.001);
    EXPECT_NEAR(F(2, 2), 0.00000, 0.001);
    EXPECT_NEAR(F(2, 3), 0.28073, 0.001);
    EXPECT_NEAR(F(2, 4), 0.28073, 0.001);
    EXPECT_NEAR(F(3, 1), 0.16215, 0.001);
    EXPECT_NEAR(F(3, 2), 0.14036, 0.001);
    EXPECT_NEAR(F(3, 3), 0.00000, 0.001);
    EXPECT_NEAR(F(3, 4), 0.07060, 0.001);
    EXPECT_NEAR(F(4, 1), 0.16215, 0.001);
    EXPECT_NEAR(F(4, 2), 0.14036, 0.001);
    EXPECT_NEAR(F(4, 3), 0.07060, 0.001);
    EXPECT_NEAR(F(4, 4), 0.00000, 0.001);
}

TEST_F(EnergyPlusFixture, HeatBalanceIntRadExchange_DoesZoneHaveInternalMassTest)
{

    int numOfZoneSurfaces;
    Array1D_int surfPointers;
    bool functionReturnValue;

    numOfZoneSurfaces = 7;
    surfPointers.allocate(numOfZoneSurfaces);
    state->dataSurface->Surface.allocate(numOfZoneSurfaces);

    for (int i = 1; i <= numOfZoneSurfaces; ++i) {
        surfPointers(i) = i;
        state->dataSurface->Surface(i).Class = DataSurfaces::SurfaceClass::Wall;
    }

    // Test 1: Nothing is an internal mass--function should return "false"
    functionReturnValue = DoesZoneHaveInternalMass(*state, numOfZoneSurfaces, surfPointers);
    EXPECT_FALSE(functionReturnValue);

    // Test 2: Set one of the surfaces to internal mass--function should return "true"
    state->dataSurface->Surface(7).Class = DataSurfaces::SurfaceClass::IntMass;
    functionReturnValue = DoesZoneHaveInternalMass(*state, numOfZoneSurfaces, surfPointers);
    EXPECT_TRUE(functionReturnValue);
}

TEST_F(EnergyPlusFixture, HeatBalanceIntRadExchange_UpdateMovableInsulationFlagTest)
{

    bool DidMIChange;
    int SurfNum;

    state->dataConstruction->Construct.allocate(1);
    state->dataMaterial->Material.allocate(1);
    state->dataSurface->Surface.allocate(1);
    state->dataSurface->SurfMaterialMovInsulInt.allocate(1);
    state->dataHeatBalSurf->SurfMovInsulIntPresent.allocate(1);
    state->dataHeatBalSurf->SurfMovInsulIntPresentPrevTS.allocate(1);
    state->dataHeatBalSurf->SurfMovInsulIndexList.push_back(1);

    SurfNum = 1;
    state->dataHeatBalSurf->SurfMovInsulIntPresent(1) = false;
    state->dataHeatBalSurf->SurfMovInsulIntPresentPrevTS(1) = false;
    state->dataSurface->Surface(1).Construction = 1;
    state->dataSurface->SurfMaterialMovInsulInt(1) = 1;

    state->dataConstruction->Construct(1).InsideAbsorpThermal = 0.9;
    state->dataMaterial->Material(1).AbsorpThermal = 0.5;
    state->dataMaterial->Material(1).Resistance = 1.25;
    state->dataMaterial->Material(1).AbsorpSolar = 0.25;

    // Test 1: Movable insulation present but wasn't in previous time step, also movable insulation emissivity different than base construction
    //         This should result in a true value from the algorithm which will cause interior radiant exchange matrices to be recalculated
    HeatBalanceIntRadExchange::UpdateMovableInsulationFlag(*state, DidMIChange, SurfNum);
    EXPECT_TRUE(!DidMIChange);

    // Test 2: Movable insulation present and was also present in previous time step.  This should result in a false value since nothing has changed.
    state->dataHeatBalSurf->SurfMovInsulIntPresentPrevTS(1) = true;
    HeatBalanceIntRadExchange::UpdateMovableInsulationFlag(*state, DidMIChange, SurfNum);
    EXPECT_TRUE(DidMIChange);

    // Test 2: Movable insulation present but wasn't in previous time step.  However, the emissivity of the movable insulation and that of the
    // 		   construction are the same so nothing has actually changed.  This should result in a false value.
    state->dataHeatBalSurf->SurfMovInsulIntPresentPrevTS(1) = true;
    state->dataMaterial->Material(1).AbsorpThermal = state->dataConstruction->Construct(1).InsideAbsorpThermal;
    HeatBalanceIntRadExchange::UpdateMovableInsulationFlag(*state, DidMIChange, SurfNum);
    EXPECT_TRUE(!DidMIChange);
}

TEST_F(EnergyPlusFixture, HeatBalanceIntRadExchange_AlignInputViewFactorsTest)
{

    std::string const idf_objects = delimited_string({
        "Zone,",
        "Zone 1;             !- Name",

        "Zone,",
        "Zone 2;             !- Name",

        "Zone,",
        "Zone 3;             !- Name",

        "Zone,",
        "Zone 4;             !- Name",

        "Zone,",
        "Zone 5;             !- Name",

        "ZoneProperty:UserViewFactors:BySurfaceName,",
        "Zone 3,",
        "SB51,SB51,0.000000,",
        "SB51,SB52,2.672021E-002,",
        "SB51,SB53,8.311358E-002,",
        "SB51,SB54,2.672021E-002;",

        "ZoneProperty:UserViewFactors:BySurfaceName,",
        "Perimeter Zones,",
        "SB51,SB51,0.000000,",
        "SB51,SB52,2.672021E-002,",
        "SB51,SB53,8.311358E-002,",
        "SB51,SB54,2.672021E-002;",

        "ZoneList,",
        "Perimeter Zones, !- Name",
        "Zone 5, !- Zone 1 Name",
        "Zone 2; !- Zone 2 Name",

        "ZoneProperty:UserViewFactors:BySurfaceName,",
        "Zone 6,",
        "SB51,SB51,0.000000,",
        "SB51,SB52,2.672021E-002,",
        "SB51,SB53,8.311358E-002,",
        "SB51,SB54,2.672021E-002;",
    });
    ASSERT_TRUE(process_idf(idf_objects));
    bool ErrorsFound = false;
    HeatBalanceManager::GetZoneData(*state, ErrorsFound);
    EXPECT_FALSE(ErrorsFound);

    state->dataViewFactor->NumOfRadiantEnclosures = 3;
    state->dataViewFactor->ZoneRadiantInfo.allocate(3);
    state->dataViewFactor->ZoneRadiantInfo(1).Name = "Enclosure 1";
    state->dataViewFactor->ZoneRadiantInfo(1).ZoneNums.push_back(
        UtilityRoutines::FindItemInList(UtilityRoutines::MakeUPPERCase("Zone 2"), state->dataHeatBal->Zone, state->dataGlobal->NumOfZones));
    state->dataViewFactor->ZoneRadiantInfo(1).ZoneNums.push_back(
        UtilityRoutines::FindItemInList(UtilityRoutines::MakeUPPERCase("Zone 1"), state->dataHeatBal->Zone, state->dataGlobal->NumOfZones));
    state->dataViewFactor->ZoneRadiantInfo(2).Name = "Enclosure 2";
    state->dataViewFactor->ZoneRadiantInfo(2).ZoneNums.push_back(
        UtilityRoutines::FindItemInList(UtilityRoutines::MakeUPPERCase("Zone 4"), state->dataHeatBal->Zone, state->dataGlobal->NumOfZones));
    state->dataViewFactor->ZoneRadiantInfo(2).ZoneNums.push_back(
        UtilityRoutines::FindItemInList(UtilityRoutines::MakeUPPERCase("Zone 5"), state->dataHeatBal->Zone, state->dataGlobal->NumOfZones));
    state->dataViewFactor->ZoneRadiantInfo(3).Name = "Zone 3";
    state->dataViewFactor->ZoneRadiantInfo(3).ZoneNums.push_back(
        UtilityRoutines::FindItemInList(UtilityRoutines::MakeUPPERCase("Zone 3"), state->dataHeatBal->Zone, state->dataGlobal->NumOfZones));

    ErrorsFound = false;
    HeatBalanceIntRadExchange::AlignInputViewFactors(*state, "ZoneProperty:UserViewFactors:BySurfaceName", ErrorsFound);
    EXPECT_TRUE(ErrorsFound);
    std::string const error_string =
        delimited_string({"   ** Severe  ** AlignInputViewFactors: ZoneProperty:UserViewFactors:BySurfaceName=\"Perimeter Zones\" found a matching "
                          "ZoneList, but did not find a matching radiant or solar enclosure with the same zones.",
                          "   ** Severe  ** AlignInputViewFactors: ZoneProperty:UserViewFactors:BySurfaceName=\"Zone 6\" did not find a matching "
                          "radiant or solar enclosure name."});
    EXPECT_TRUE(compare_err_stream(error_string, true));

    EXPECT_EQ(state->dataViewFactor->ZoneRadiantInfo(1).Name, "Enclosure 1");
    EXPECT_EQ(state->dataViewFactor->ZoneRadiantInfo(2).Name, "Enclosure 2");
    EXPECT_EQ(state->dataViewFactor->ZoneRadiantInfo(3).Name, "Zone 3");
}

TEST_F(EnergyPlusFixture, HeatBalanceIntRadExchange_AlignInputViewFactorsTest2)
{

    std::string const idf_objects = delimited_string({
        "Zone,",
        "Zone 1;             !- Name",

        "Zone,",
        "Zone 2;             !- Name",

        "Zone,",
        "Zone 3;             !- Name",

        "Zone,",
        "Zone 4;             !- Name",

        "Zone,",
        "Zone 5;             !- Name",

        "ZoneProperty:UserViewFactors:BySurfaceName,",
        "Zone 3,",
        "SB51,SB51,0.000000,",
        "SB51,SB52,2.672021E-002,",
        "SB51,SB53,8.311358E-002,",
        "SB51,SB54,2.672021E-002;",

        "ZoneProperty:UserViewFactors:BySurfaceName,",
        "Perimeter Zones,",
        "SB51,SB51,0.000000,",
        "SB51,SB52,2.672021E-002,",
        "SB51,SB53,8.311358E-002,",
        "SB51,SB54,2.672021E-002;",

        "ZoneList,",
        "Perimeter Zones, !- Name",
        "Zone 5, !- Zone 1 Name",
        "Zone 2; !- Zone 2 Name",

        "ZoneProperty:UserViewFactors:BySurfaceName,",
        "Zone 6,",
        "SB51,SB51,0.000000,",
        "SB51,SB52,2.672021E-002,",
        "SB51,SB53,8.311358E-002,",
        "SB51,SB54,2.672021E-002;",
    });
    ASSERT_TRUE(process_idf(idf_objects));
    bool ErrorsFound = false;
    HeatBalanceManager::GetZoneData(*state, ErrorsFound);
    EXPECT_FALSE(ErrorsFound);

    state->dataViewFactor->NumOfSolarEnclosures = 3;
    state->dataViewFactor->ZoneSolarInfo.allocate(3);
    state->dataViewFactor->ZoneSolarInfo(1).Name = "Enclosure 1";
    state->dataViewFactor->ZoneSolarInfo(1).ZoneNums.push_back(
        UtilityRoutines::FindItemInList(UtilityRoutines::MakeUPPERCase("Zone 2"), state->dataHeatBal->Zone, state->dataGlobal->NumOfZones));
    state->dataViewFactor->ZoneSolarInfo(1).ZoneNums.push_back(
        UtilityRoutines::FindItemInList(UtilityRoutines::MakeUPPERCase("Zone 5"), state->dataHeatBal->Zone, state->dataGlobal->NumOfZones));
    state->dataViewFactor->ZoneSolarInfo(2).Name = "Enclosure 2";
    state->dataViewFactor->ZoneSolarInfo(2).ZoneNums.push_back(
        UtilityRoutines::FindItemInList(UtilityRoutines::MakeUPPERCase("Zone 4"), state->dataHeatBal->Zone, state->dataGlobal->NumOfZones));
    state->dataViewFactor->ZoneSolarInfo(2).ZoneNums.push_back(
        UtilityRoutines::FindItemInList(UtilityRoutines::MakeUPPERCase("Zone 5"), state->dataHeatBal->Zone, state->dataGlobal->NumOfZones));
    state->dataViewFactor->ZoneSolarInfo(3).Name = "Zone 3";
    state->dataViewFactor->ZoneSolarInfo(3).ZoneNums.push_back(
        UtilityRoutines::FindItemInList(UtilityRoutines::MakeUPPERCase("Zone 3"), state->dataHeatBal->Zone, state->dataGlobal->NumOfZones));

    ErrorsFound = false;
    HeatBalanceIntRadExchange::AlignInputViewFactors(*state, "ZoneProperty:UserViewFactors:BySurfaceName", ErrorsFound);
    EXPECT_TRUE(ErrorsFound);
    std::string const error_string = delimited_string({"   ** Severe  ** AlignInputViewFactors: ZoneProperty:UserViewFactors:BySurfaceName=\"Zone "
                                                       "6\" did not find a matching radiant or solar enclosure name."});
    EXPECT_TRUE(compare_err_stream(error_string, true));

    EXPECT_EQ(state->dataViewFactor->ZoneSolarInfo(1).Name, "Perimeter Zones");
    EXPECT_EQ(state->dataViewFactor->ZoneSolarInfo(2).Name, "Enclosure 2");
    EXPECT_EQ(state->dataViewFactor->ZoneSolarInfo(3).Name, "Zone 3");
}

TEST_F(EnergyPlusFixture, HeatBalanceIntRadExchange_AlignInputViewFactorsTest3)
{

    std::string const idf_objects = delimited_string({
        "Zone,",
        "Zone 1;             !- Name",

        "Zone,",
        "Zone 2;             !- Name",

        "Zone,",
        "Zone 3;             !- Name",

        "Zone,",
        "Zone 4;             !- Name",

        "Zone,",
        "Zone 5;             !- Name",

        "ZoneProperty:UserViewFactors:BySurfaceName,",
        "Zone 3,",
        "SB51,SB51,0.000000,",
        "SB51,SB52,2.672021E-002,",
        "SB51,SB53,8.311358E-002,",
        "SB51,SB54,2.672021E-002;",

        "ZoneProperty:UserViewFactors:BySurfaceName,",
        "Perimeter Zones,",
        "SB51,SB51,0.000000,",
        "SB51,SB52,2.672021E-002,",
        "SB51,SB53,8.311358E-002,",
        "SB51,SB54,2.672021E-002;",

        "ZoneList,",
        "Perimeter Zones, !- Name",
        "Zone 5, !- Zone 1 Name",
        "Zone 2; !- Zone 2 Name",

        "ZoneProperty:UserViewFactors:BySurfaceName,",
        "Zone 6,",
        "SB51,SB51,0.000000,",
        "SB51,SB52,2.672021E-002,",
        "SB51,SB53,8.311358E-002,",
        "SB51,SB54,2.672021E-002;",
    });
    ASSERT_TRUE(process_idf(idf_objects));
    bool ErrorsFound = false;
    HeatBalanceManager::GetZoneData(*state, ErrorsFound);
    EXPECT_FALSE(ErrorsFound);

    state->dataViewFactor->NumOfSolarEnclosures = 3;
    state->dataViewFactor->ZoneSolarInfo.allocate(3);
    state->dataViewFactor->ZoneSolarInfo(1).Name = "Enclosure 1";
    state->dataViewFactor->ZoneSolarInfo(1).ZoneNums.push_back(
        UtilityRoutines::FindItemInList(UtilityRoutines::MakeUPPERCase("Zone 2"), state->dataHeatBal->Zone, state->dataGlobal->NumOfZones));
    state->dataViewFactor->ZoneSolarInfo(1).ZoneNums.push_back(
        UtilityRoutines::FindItemInList(UtilityRoutines::MakeUPPERCase("Zone 1"), state->dataHeatBal->Zone, state->dataGlobal->NumOfZones));
    state->dataViewFactor->ZoneSolarInfo(2).Name = "Enclosure 2";
    state->dataViewFactor->ZoneSolarInfo(2).ZoneNums.push_back(
        UtilityRoutines::FindItemInList(UtilityRoutines::MakeUPPERCase("Zone 4"), state->dataHeatBal->Zone, state->dataGlobal->NumOfZones));
    state->dataViewFactor->ZoneSolarInfo(2).ZoneNums.push_back(
        UtilityRoutines::FindItemInList(UtilityRoutines::MakeUPPERCase("Zone 5"), state->dataHeatBal->Zone, state->dataGlobal->NumOfZones));
    state->dataViewFactor->ZoneSolarInfo(3).Name = "Zone 3";
    state->dataViewFactor->ZoneSolarInfo(3).ZoneNums.push_back(
        UtilityRoutines::FindItemInList(UtilityRoutines::MakeUPPERCase("Zone 3"), state->dataHeatBal->Zone, state->dataGlobal->NumOfZones));

    ErrorsFound = false;
    HeatBalanceIntRadExchange::AlignInputViewFactors(*state, "ZoneProperty:UserViewFactors:BySurfaceName", ErrorsFound);
    EXPECT_TRUE(ErrorsFound);
    std::string const error_string =
        delimited_string({"   ** Severe  ** AlignInputViewFactors: ZoneProperty:UserViewFactors:BySurfaceName=\"Perimeter Zones\" found a matching "
                          "ZoneList, but did not find a matching radiant or solar enclosure with the same zones.",
                          "   ** Severe  ** AlignInputViewFactors: ZoneProperty:UserViewFactors:BySurfaceName=\"Zone 6\" did not find a matching "
                          "radiant or solar enclosure name."});
    EXPECT_TRUE(compare_err_stream(error_string, true));

    EXPECT_EQ(state->dataViewFactor->ZoneSolarInfo(1).Name, "Enclosure 1");
    EXPECT_EQ(state->dataViewFactor->ZoneSolarInfo(2).Name, "Enclosure 2");
    EXPECT_EQ(state->dataViewFactor->ZoneSolarInfo(3).Name, "Zone 3");
}

TEST_F(EnergyPlusFixture, HeatBalanceIntRadExchange_AlignInputViewFactorsTest4)
{

    std::string const idf_objects = delimited_string({
        "Zone,",
        "Zone 1;             !- Name",

        "Zone,",
        "Zone 2;             !- Name",

        "Zone,",
        "Zone 3;             !- Name",

        "Zone,",
        "Zone 4;             !- Name",

        "Zone,",
        "Zone 5;             !- Name",

        "ZoneProperty:UserViewFactors:BySurfaceName,",
        "Zone 3,",
        "SB51,SB51,0.000000,",
        "SB51,SB52,2.672021E-002,",
        "SB51,SB53,8.311358E-002,",
        "SB51,SB54,2.672021E-002;",

        "ZoneProperty:UserViewFactors:BySurfaceName,",
        "Perimeter Zones,",
        "SB51,SB51,0.000000,",
        "SB51,SB52,2.672021E-002,",
        "SB51,SB53,8.311358E-002,",
        "SB51,SB54,2.672021E-002;",

        "ZoneList,",
        "Perimeter Zones, !- Name",
        "Zone 5, !- Zone 1 Name",
        "Zone 2; !- Zone 2 Name",

        "ZoneProperty:UserViewFactors:BySurfaceName,",
        "Zone 6,",
        "SB51,SB51,0.000000,",
        "SB51,SB52,2.672021E-002,",
        "SB51,SB53,8.311358E-002,",
        "SB51,SB54,2.672021E-002;",
    });
    ASSERT_TRUE(process_idf(idf_objects));
    bool ErrorsFound = false;
    HeatBalanceManager::GetZoneData(*state, ErrorsFound);
    EXPECT_FALSE(ErrorsFound);

    state->dataViewFactor->NumOfRadiantEnclosures = 3;
    state->dataViewFactor->ZoneRadiantInfo.allocate(3);
    state->dataViewFactor->ZoneRadiantInfo(1).Name = "Enclosure 1";
    state->dataViewFactor->ZoneRadiantInfo(1).ZoneNums.push_back(
        UtilityRoutines::FindItemInList(UtilityRoutines::MakeUPPERCase("Zone 2"), state->dataHeatBal->Zone, state->dataGlobal->NumOfZones));
    state->dataViewFactor->ZoneRadiantInfo(1).ZoneNums.push_back(
        UtilityRoutines::FindItemInList(UtilityRoutines::MakeUPPERCase("Zone 5"), state->dataHeatBal->Zone, state->dataGlobal->NumOfZones));
    state->dataViewFactor->ZoneRadiantInfo(2).Name = "Enclosure 2";
    state->dataViewFactor->ZoneRadiantInfo(2).ZoneNums.push_back(
        UtilityRoutines::FindItemInList(UtilityRoutines::MakeUPPERCase("Zone 4"), state->dataHeatBal->Zone, state->dataGlobal->NumOfZones));
    state->dataViewFactor->ZoneRadiantInfo(2).ZoneNums.push_back(
        UtilityRoutines::FindItemInList(UtilityRoutines::MakeUPPERCase("Zone 5"), state->dataHeatBal->Zone, state->dataGlobal->NumOfZones));
    state->dataViewFactor->ZoneRadiantInfo(3).Name = "Zone 3";
    state->dataViewFactor->ZoneRadiantInfo(3).ZoneNums.push_back(
        UtilityRoutines::FindItemInList(UtilityRoutines::MakeUPPERCase("Zone 3"), state->dataHeatBal->Zone, state->dataGlobal->NumOfZones));

    ErrorsFound = false;
    HeatBalanceIntRadExchange::AlignInputViewFactors(*state, "ZoneProperty:UserViewFactors:BySurfaceName", ErrorsFound);
    EXPECT_TRUE(ErrorsFound);
    std::string const error_string = delimited_string({"   ** Severe  ** AlignInputViewFactors: ZoneProperty:UserViewFactors:BySurfaceName=\"Zone "
                                                       "6\" did not find a matching radiant or solar enclosure name."});
    EXPECT_TRUE(compare_err_stream(error_string, true));

    EXPECT_EQ(state->dataViewFactor->ZoneRadiantInfo(1).Name, "Perimeter Zones");
    EXPECT_EQ(state->dataViewFactor->ZoneRadiantInfo(2).Name, "Enclosure 2");
    EXPECT_EQ(state->dataViewFactor->ZoneRadiantInfo(3).Name, "Zone 3");
}

TEST_F(EnergyPlusFixture, HeatBalanceIntRadExchange_ViewFactorAngleLimitTest)
{

    int N;             // NUMBER OF SURFACES
    Array1D<Real64> A; // AREA VECTOR- ASSUMED,BE N ELEMENTS LONG
    Array2D<Real64> F; // APPROXIMATE DIRECT VIEW FACTOR MATRIX (N X N)
    int ZoneNum;       // Zone number being fixed

    // number of surfaces
    N = 9;
    A.allocate(N);
    F.allocate(N, N);

    // initialize view factors
    F = 0.0;

    // set surface areas to 1.0 for view factor calculations
    A = 1.0;

    ZoneNum = 1;
    state->dataHeatBal->Zone.allocate(ZoneNum);
    state->dataHeatBal->Zone(ZoneNum).Name = "Test";
    state->dataViewFactor->ZoneRadiantInfo.allocate(ZoneNum);
    state->dataViewFactor->ZoneRadiantInfo(ZoneNum).Name = state->dataHeatBal->Zone(ZoneNum).Name;
    state->dataViewFactor->ZoneRadiantInfo(ZoneNum).ZoneNums.push_back(ZoneNum);
    state->dataViewFactor->ZoneRadiantInfo(ZoneNum).Azimuth.allocate(N);
    state->dataViewFactor->ZoneRadiantInfo(ZoneNum).Tilt.allocate(N);
    state->dataViewFactor->ZoneRadiantInfo(ZoneNum).SurfacePtr.allocate(N);
    state->dataViewFactor->ZoneRadiantInfo(ZoneNum).SurfacePtr(1) = 1;
    state->dataViewFactor->ZoneRadiantInfo(ZoneNum).SurfacePtr(2) = 2;
    state->dataViewFactor->ZoneRadiantInfo(ZoneNum).SurfacePtr(3) = 3;
    state->dataViewFactor->ZoneRadiantInfo(ZoneNum).SurfacePtr(4) = 4;
    state->dataViewFactor->ZoneRadiantInfo(ZoneNum).SurfacePtr(5) = 5;
    state->dataViewFactor->ZoneRadiantInfo(ZoneNum).SurfacePtr(6) = 6;
    state->dataViewFactor->ZoneRadiantInfo(ZoneNum).SurfacePtr(7) = 7;
    state->dataViewFactor->ZoneRadiantInfo(ZoneNum).SurfacePtr(8) = 8;
    state->dataViewFactor->ZoneRadiantInfo(ZoneNum).SurfacePtr(9) = 9;
    state->dataSurface->Surface.allocate(N);

    // wall
    state->dataSurface->Surface(1).Class = DataSurfaces::SurfaceClass::Wall;
    state->dataViewFactor->ZoneRadiantInfo(ZoneNum).Azimuth(1) = 0;
    state->dataViewFactor->ZoneRadiantInfo(ZoneNum).Tilt(1) = 90;
    // opposite wall
    state->dataSurface->Surface(2).Class = DataSurfaces::SurfaceClass::Wall;
    state->dataViewFactor->ZoneRadiantInfo(ZoneNum).Tilt(2) = 90;
    state->dataViewFactor->ZoneRadiantInfo(ZoneNum).Azimuth(2) = 180;
    // floor
    state->dataSurface->Surface(3).Class = DataSurfaces::SurfaceClass::Floor;
    state->dataViewFactor->ZoneRadiantInfo(ZoneNum).Azimuth(3) = 0;
    state->dataViewFactor->ZoneRadiantInfo(ZoneNum).Tilt(3) = 180;
    // ceiling/roof
    state->dataSurface->Surface(4).Class = DataSurfaces::SurfaceClass::Roof;
    state->dataViewFactor->ZoneRadiantInfo(ZoneNum).Azimuth(4) = 0;
    state->dataViewFactor->ZoneRadiantInfo(ZoneNum).Tilt(4) = 0;
    // additional floor with different tilt
    state->dataSurface->Surface(5).Class = DataSurfaces::SurfaceClass::Floor;
    state->dataViewFactor->ZoneRadiantInfo(ZoneNum).Azimuth(5) = 0;
    state->dataViewFactor->ZoneRadiantInfo(ZoneNum).Tilt(5) = 270;
    // additional wall with slight difference in azimuth (<10 degrees)
    state->dataSurface->Surface(6).Class = DataSurfaces::SurfaceClass::Wall;
    state->dataViewFactor->ZoneRadiantInfo(ZoneNum).Azimuth(6) = 358;
    state->dataViewFactor->ZoneRadiantInfo(ZoneNum).Tilt(6) = 90;
    // ceiling/roof with slight difference in azimuth (<10 degrees)
    state->dataSurface->Surface(7).Class = DataSurfaces::SurfaceClass::Roof;
    state->dataViewFactor->ZoneRadiantInfo(ZoneNum).Azimuth(7) = 5;
    state->dataViewFactor->ZoneRadiantInfo(ZoneNum).Tilt(7) = 0;
    // ceiling/roof with large difference in azimuth
    state->dataSurface->Surface(8).Class = DataSurfaces::SurfaceClass::Roof;
    state->dataViewFactor->ZoneRadiantInfo(ZoneNum).Azimuth(8) = 90;
    state->dataViewFactor->ZoneRadiantInfo(ZoneNum).Tilt(8) = 0;
    // internal mass
    state->dataSurface->Surface(9).Class = DataSurfaces::SurfaceClass::IntMass;
    state->dataViewFactor->ZoneRadiantInfo(ZoneNum).Azimuth(9) = 0;
    state->dataViewFactor->ZoneRadiantInfo(ZoneNum).Tilt(9) = 0;

    CalcApproximateViewFactors(*state,
                               N,
                               A,
                               state->dataViewFactor->ZoneRadiantInfo(ZoneNum).Azimuth,
                               state->dataViewFactor->ZoneRadiantInfo(ZoneNum).Tilt,
                               F,
                               state->dataViewFactor->ZoneRadiantInfo(ZoneNum).SurfacePtr);

    // surfaces can't see themselves, no view factor
    EXPECT_EQ(F(1, 1), 0.0);
    EXPECT_EQ(F(2, 2), 0.0);
    EXPECT_EQ(F(3, 3), 0.0);
    EXPECT_EQ(F(4, 4), 0.0);
    EXPECT_EQ(F(5, 5), 0.0);
    EXPECT_EQ(F(6, 6), 0.0);
    EXPECT_EQ(F(7, 7), 0.0);
    EXPECT_EQ(F(8, 8), 0.0);
    EXPECT_EQ(F(9, 9), 0.0);

    // two floors should NOT see each other, even with different tilts (180 and 270), no view factor
    EXPECT_EQ(F(3, 5), 0.0);
    EXPECT_EQ(F(5, 3), 0.0);

    // all surfaces see internal mass
    EXPECT_GT(F(9, 1), 0.0);
    EXPECT_GT(F(9, 2), 0.0);
    EXPECT_GT(F(9, 3), 0.0);
    EXPECT_GT(F(9, 4), 0.0);
    EXPECT_GT(F(9, 5), 0.0);
    EXPECT_GT(F(9, 6), 0.0);
    EXPECT_GT(F(9, 7), 0.0);
    EXPECT_GT(F(1, 9), 0.0);
    EXPECT_GT(F(2, 9), 0.0);
    EXPECT_GT(F(3, 9), 0.0);
    EXPECT_GT(F(4, 9), 0.0);
    EXPECT_GT(F(5, 9), 0.0);
    EXPECT_GT(F(6, 9), 0.0);
    EXPECT_GT(F(7, 9), 0.0);

    // all non-floor surfaces see floors
    EXPECT_GT(F(3, 1), 0.0);
    EXPECT_GT(F(3, 2), 0.0);
    EXPECT_GT(F(3, 4), 0.0);
    EXPECT_GT(F(3, 6), 0.0);
    EXPECT_GT(F(3, 7), 0.0);
    EXPECT_GT(F(3, 9), 0.0);
    EXPECT_GT(F(1, 3), 0.0);
    EXPECT_GT(F(2, 3), 0.0);
    EXPECT_GT(F(4, 3), 0.0);
    EXPECT_GT(F(6, 3), 0.0);
    EXPECT_GT(F(7, 3), 0.0);
    EXPECT_GT(F(9, 3), 0.0);
    EXPECT_GT(F(5, 1), 0.0);
    EXPECT_GT(F(5, 2), 0.0);
    EXPECT_GT(F(5, 4), 0.0);
    EXPECT_GT(F(5, 6), 0.0);
    EXPECT_GT(F(5, 7), 0.0);
    EXPECT_GT(F(5, 9), 0.0);
    EXPECT_GT(F(1, 5), 0.0);
    EXPECT_GT(F(2, 5), 0.0);
    EXPECT_GT(F(4, 5), 0.0);
    EXPECT_GT(F(6, 5), 0.0);
    EXPECT_GT(F(7, 5), 0.0);
    EXPECT_GT(F(9, 5), 0.0);

    // all floors see ceilings/roofs and result in a view factor
    EXPECT_GT(F(4, 3), 0.0);
    EXPECT_GT(F(4, 5), 0.0);
    EXPECT_GT(F(3, 4), 0.0);
    EXPECT_GT(F(5, 4), 0.0);
    EXPECT_GT(F(7, 3), 0.0);
    EXPECT_GT(F(7, 5), 0.0);
    EXPECT_GT(F(3, 7), 0.0);
    EXPECT_GT(F(5, 7), 0.0);

    // two walls should see each other and result in a view factor
    EXPECT_GT(F(1, 2), 0.0);
    EXPECT_GT(F(2, 1), 0.0);

    // ceiling/roof and wall should see each other and result in a view factor
    EXPECT_GT(F(1, 4), 0.0);
    EXPECT_GT(F(2, 4), 0.0);
    EXPECT_GT(F(6, 4), 0.0);
    EXPECT_GT(F(4, 1), 0.0);
    EXPECT_GT(F(4, 2), 0.0);
    EXPECT_GT(F(4, 6), 0.0);
    EXPECT_GT(F(1, 7), 0.0);
    EXPECT_GT(F(2, 7), 0.0);
    EXPECT_GT(F(6, 7), 0.0);
    EXPECT_GT(F(7, 1), 0.0);
    EXPECT_GT(F(7, 2), 0.0);
    EXPECT_GT(F(7, 6), 0.0);
    EXPECT_GT(F(1, 8), 0.0);
    EXPECT_GT(F(2, 8), 0.0);
    EXPECT_GT(F(6, 8), 0.0);
    EXPECT_GT(F(8, 1), 0.0);
    EXPECT_GT(F(8, 2), 0.0);
    EXPECT_GT(F(8, 6), 0.0);

    // two walls with azimuths of 0 and 358 (<10 degrees), should NOT see each other, no view factor
    EXPECT_EQ(F(1, 6), 0.0);
    EXPECT_EQ(F(6, 1), 0.0);

    // two roofs with azimuths of 0 and 5 (<10 degrees), should NOT see each other, no view factor
    EXPECT_EQ(F(4, 7), 0.0);
    EXPECT_EQ(F(7, 4), 0.0);

    // two roofs with azimuths of 0 and 90 (>10 degrees), should see each other and result in a view factor
    // this goes against common sense and why we might want to change to comparing surface normals instead of azimuth/tilt
    EXPECT_GT(F(4, 8), 0.0);
    EXPECT_GT(F(8, 4), 0.0);
}
} // namespace EnergyPlus
