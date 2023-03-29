// EnergyPlus, Copyright (c) 1996-2023, The Board of Trustees of the University of Illinois,
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

// C++ Headers
#include <cmath>
#include <string>

// ObjexxFCL Headers
#include <ObjexxFCL/Fmath.hh>
#include <ObjexxFCL/member.functions.hh>

// EnergyPlus Headers
#include <EnergyPlus/Construction.hh>
#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataHeatBalSurface.hh>
#include <EnergyPlus/DataHeatBalance.hh>
#include <EnergyPlus/DataIPShortCuts.hh>
#include <EnergyPlus/DataMoistureBalance.hh>
#include <EnergyPlus/DataSurfaces.hh>
#include <EnergyPlus/DisplayRoutines.hh>
#include <EnergyPlus/General.hh>
#include <EnergyPlus/HeatBalanceHAMTManager.hh>
#include <EnergyPlus/InputProcessing/InputProcessor.hh>
#include <EnergyPlus/Material.hh>
#include <EnergyPlus/OutputProcessor.hh>
#include <EnergyPlus/Psychrometrics.hh>
#include <EnergyPlus/UtilityRoutines.hh>
#include <EnergyPlus/ZoneTempPredictorCorrector.hh>

namespace EnergyPlus {

namespace HeatBalanceHAMTManager {

    // MODULE INFORMATION:
    //       AUTHOR      Phillip Biddulph
    //       DATE WRITTEN   June 2008
    //       MODIFIED
    //       Bug fixes to make sure HAMT can cope with data limits  ! PDB August 2009
    //       RE-ENGINEERED

    // PURPOSE OF THIS MODULE:
    // Calculate, record and report the one dimentional heat and moisture transfer
    // through a surface given the material composition of the building surface and
    // the external and internal Temperatures and Relative Humidities.

    // METHODOLOGY EMPLOYED:
    // Each surface is split into "cells", where all characteristics are initiallised.
    // Cells are matched and links created in the initialisation routine.
    // The internal and external "surfaces" of the surface are virtual cells to allow for the
    // input of heat and vapor via heat transfer coefficients, radiation,
    // and vapor transfer coefficients
    // Uses Forward (implicit) finite difference alogorithm. Heat transfer is caclulated first,
    // with the option of including the latent heat, then liquid and vapor transfer. The process is ittereated.
    // Once the temperatures have converged the internal surface
    // temperature and vapor densities are passed back to EnergyPlus.

    // Temperatures and relative humidities are updated once EnergyPlus has checked that
    // the zone temperatures have converged.

    // REFERENCES:
    // K?zel, H.M. (1995) Simultaneous Heat and Moisture Transport in Building Components.
    // One- and two-dimensional calculation using simple parameters. IRB Verlag 1995
    // Holman, J.P. (2002) Heat Transfer, Ninth Edition. McGraw-Hill
    // Winterton, R.H.S. (1997) Heat Transfer. (Oxford Chemistry Primers; 50) Oxford University Press
    // Kumar Kumaran, M. (1996) IEA ANNEX 24, Final Report, Volume 3

    // USE STATEMENTS:

    // Using/Aliasing
    using namespace DataSurfaces;
    using DataHeatBalSurface::MinSurfaceTempLimit;
    using DataHeatBalSurface::MinSurfaceTempLimitBeforeFatal;
    using namespace DataHeatBalance;
    using namespace Psychrometrics;

    void ManageHeatBalHAMT(EnergyPlusData &state, int const SurfNum, Real64 &SurfTempInTmp, Real64 &TempSurfOutTmp)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Phillip Biddulph
        //       DATE WRITTEN   June 2008
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Manages the Heat and Moisture Transfer calculations.

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        if (state.dataHeatBalHAMTMgr->OneTimeFlag) {
            state.dataHeatBalHAMTMgr->OneTimeFlag = false;
            DisplayString(state, "Initialising Heat and Moisture Transfer Model");
            GetHeatBalHAMTInput(state);
            InitHeatBalHAMT(state);
        }

        CalcHeatBalHAMT(state, SurfNum, SurfTempInTmp, TempSurfOutTmp);
    }

    void GetHeatBalHAMTInput(EnergyPlusData &state)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Phillip Biddulph
        //       DATE WRITTEN   June 2008
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // gets input for the HAMT model

        // SUBROUTINE PARAMETER DEFINITIONS:
        static std::string const cHAMTObject1("MaterialProperty:HeatAndMoistureTransfer:Settings");
        static std::string const cHAMTObject2("MaterialProperty:HeatAndMoistureTransfer:SorptionIsotherm");
        static std::string const cHAMTObject3("MaterialProperty:HeatAndMoistureTransfer:Suction");
        static std::string const cHAMTObject4("MaterialProperty:HeatAndMoistureTransfer:Redistribution");
        static std::string const cHAMTObject5("MaterialProperty:HeatAndMoistureTransfer:Diffusion");
        static std::string const cHAMTObject6("MaterialProperty:HeatAndMoistureTransfer:ThermalConductivity");
        static std::string const cHAMTObject7("SurfaceProperties:VaporCoefficients");

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:

        Array1D_string AlphaArray;
        Array1D_string cAlphaFieldNames;
        Array1D_string cNumericFieldNames;

        Array1D_bool lAlphaBlanks;
        Array1D_bool lNumericBlanks;

        Array1D<Real64> NumArray;

        Real64 dumrh;
        Real64 dumdata;
        Real64 avdata;

        int MaxNums;
        int MaxAlphas;
        int NumParams;
        int NumNums;
        int NumAlphas;
        int status;
        int matid;
        int iso;
        int Numid;
        int suc;
        int red;
        int mu;
        int tc;

        int HAMTitems;
        int item;
        int ii;
        int jj;
        int vtcsid;

        bool avflag;
        bool isoerrrise;
        bool ErrorsFound;

        state.dataHeatBalHAMTMgr->watertot.allocate(state.dataSurface->TotSurfaces);
        state.dataHeatBalHAMTMgr->surfrh.allocate(state.dataSurface->TotSurfaces);
        state.dataHeatBalHAMTMgr->surfextrh.allocate(state.dataSurface->TotSurfaces);
        state.dataHeatBalHAMTMgr->surftemp.allocate(state.dataSurface->TotSurfaces);
        state.dataHeatBalHAMTMgr->surfexttemp.allocate(state.dataSurface->TotSurfaces);
        state.dataHeatBalHAMTMgr->surfvp.allocate(state.dataSurface->TotSurfaces);

        state.dataHeatBalHAMTMgr->firstcell.allocate(state.dataSurface->TotSurfaces);
        state.dataHeatBalHAMTMgr->lastcell.allocate(state.dataSurface->TotSurfaces);
        state.dataHeatBalHAMTMgr->Extcell.allocate(state.dataSurface->TotSurfaces);
        state.dataHeatBalHAMTMgr->ExtRadcell.allocate(state.dataSurface->TotSurfaces);
        state.dataHeatBalHAMTMgr->ExtConcell.allocate(state.dataSurface->TotSurfaces);
        state.dataHeatBalHAMTMgr->ExtSkycell.allocate(state.dataSurface->TotSurfaces);
        state.dataHeatBalHAMTMgr->ExtGrncell.allocate(state.dataSurface->TotSurfaces);
        state.dataHeatBalHAMTMgr->Intcell.allocate(state.dataSurface->TotSurfaces);
        state.dataHeatBalHAMTMgr->IntConcell.allocate(state.dataSurface->TotSurfaces);

        state.dataHeatBalHAMTMgr->extvtc.allocate(state.dataSurface->TotSurfaces);
        state.dataHeatBalHAMTMgr->intvtc.allocate(state.dataSurface->TotSurfaces);
        state.dataHeatBalHAMTMgr->extvtcflag.allocate(state.dataSurface->TotSurfaces);
        state.dataHeatBalHAMTMgr->intvtcflag.allocate(state.dataSurface->TotSurfaces);
        state.dataHeatBalHAMTMgr->MyEnvrnFlag.allocate(state.dataSurface->TotSurfaces);

        state.dataHeatBalHAMTMgr->extvtc = -1.0;
        state.dataHeatBalHAMTMgr->intvtc = -1.0;
        state.dataHeatBalHAMTMgr->extvtcflag = false;
        state.dataHeatBalHAMTMgr->intvtcflag = false;
        state.dataHeatBalHAMTMgr->MyEnvrnFlag = true;

        state.dataHeatBalHAMTMgr->latswitch = true;
        state.dataHeatBalHAMTMgr->rainswitch = true;

        MaxAlphas = 0;
        MaxNums = 0;
        state.dataInputProcessing->inputProcessor->getObjectDefMaxArgs(state, cHAMTObject1, NumParams, NumAlphas, NumNums);
        MaxAlphas = max(MaxAlphas, NumAlphas);
        MaxNums = max(MaxNums, NumNums);
        state.dataInputProcessing->inputProcessor->getObjectDefMaxArgs(state, cHAMTObject2, NumParams, NumAlphas, NumNums);
        MaxAlphas = max(MaxAlphas, NumAlphas);
        MaxNums = max(MaxNums, NumNums);
        state.dataInputProcessing->inputProcessor->getObjectDefMaxArgs(state, cHAMTObject3, NumParams, NumAlphas, NumNums);
        MaxAlphas = max(MaxAlphas, NumAlphas);
        MaxNums = max(MaxNums, NumNums);
        state.dataInputProcessing->inputProcessor->getObjectDefMaxArgs(state, cHAMTObject4, NumParams, NumAlphas, NumNums);
        MaxAlphas = max(MaxAlphas, NumAlphas);
        MaxNums = max(MaxNums, NumNums);
        state.dataInputProcessing->inputProcessor->getObjectDefMaxArgs(state, cHAMTObject5, NumParams, NumAlphas, NumNums);
        MaxAlphas = max(MaxAlphas, NumAlphas);
        MaxNums = max(MaxNums, NumNums);
        state.dataInputProcessing->inputProcessor->getObjectDefMaxArgs(state, cHAMTObject6, NumParams, NumAlphas, NumNums);
        MaxAlphas = max(MaxAlphas, NumAlphas);
        MaxNums = max(MaxNums, NumNums);
        state.dataInputProcessing->inputProcessor->getObjectDefMaxArgs(state, cHAMTObject7, NumParams, NumAlphas, NumNums);
        MaxAlphas = max(MaxAlphas, NumAlphas);
        MaxNums = max(MaxNums, NumNums);

        ErrorsFound = false;

        AlphaArray.allocate(MaxAlphas);
        cAlphaFieldNames.allocate(MaxAlphas);
        cNumericFieldNames.allocate(MaxNums);
        NumArray.dimension(MaxNums, 0.0);
        lAlphaBlanks.dimension(MaxAlphas, false);
        lNumericBlanks.dimension(MaxNums, false);

        HAMTitems =
            state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, cHAMTObject1); // MaterialProperty:HeatAndMoistureTransfer:Settings
        for (item = 1; item <= HAMTitems; ++item) {
            state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                     cHAMTObject1,
                                                                     item,
                                                                     AlphaArray,
                                                                     NumAlphas,
                                                                     NumArray,
                                                                     NumNums,
                                                                     status,
                                                                     lNumericBlanks,
                                                                     lAlphaBlanks,
                                                                     cAlphaFieldNames,
                                                                     cNumericFieldNames);

            matid = UtilityRoutines::FindItemInPtrList(AlphaArray(1), state.dataMaterial->Material);

            if (matid == 0) {
                ShowSevereError(state, format("{} {}=\"{}\" is invalid (undefined).", cHAMTObject1, cAlphaFieldNames(1), AlphaArray(1)));
                ShowContinueError(state, "The basic material must be defined in addition to specifying HeatAndMoistureTransfer properties.");
                ErrorsFound = true;
                continue;
            }
            auto *thisMaterial = dynamic_cast<Material::MaterialChild *>(state.dataMaterial->Material(matid));
            assert(thisMaterial != nullptr);
            if (thisMaterial->ROnly) {
                ShowWarningError(state,
                                 format("{} {}=\"{}\" is defined as an R-only value material.", cHAMTObject1, cAlphaFieldNames(1), AlphaArray(1)));
                continue;
            }

            thisMaterial->Porosity = NumArray(1);
            thisMaterial->iwater = NumArray(2);
        }

        HAMTitems = state.dataInputProcessing->inputProcessor->getNumObjectsFound(
            state, cHAMTObject2); // MaterialProperty:HeatAndMoistureTransfer:SorptionIsotherm
        for (item = 1; item <= HAMTitems; ++item) {
            state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                     cHAMTObject2,
                                                                     item,
                                                                     AlphaArray,
                                                                     NumAlphas,
                                                                     NumArray,
                                                                     NumNums,
                                                                     status,
                                                                     lNumericBlanks,
                                                                     lAlphaBlanks,
                                                                     cAlphaFieldNames,
                                                                     cNumericFieldNames);

            matid = UtilityRoutines::FindItemInPtrList(AlphaArray(1), state.dataMaterial->Material);

            if (matid == 0) {
                ShowSevereError(state, format("{} {}=\"{}\" is invalid (undefined).", cHAMTObject2, cAlphaFieldNames(1), AlphaArray(1)));
                ShowContinueError(state, "The basic material must be defined in addition to specifying HeatAndMoistureTransfer properties.");
                ErrorsFound = true;
                continue;
            }
            auto *thisMaterial = dynamic_cast<Material::MaterialChild *>(state.dataMaterial->Material(matid));
            assert(thisMaterial != nullptr);
            if (thisMaterial->ROnly) {
                ShowWarningError(state,
                                 format("{} {}=\"{}\" is defined as an R-only value material.", cHAMTObject2, cAlphaFieldNames(1), AlphaArray(1)));
                continue;
            }

            Numid = 1;

            thisMaterial->niso = int(NumArray(Numid));

            for (iso = 1; iso <= thisMaterial->niso; ++iso) {
                ++Numid;
                thisMaterial->isorh(iso) = NumArray(Numid);
                ++Numid;
                thisMaterial->isodata(iso) = NumArray(Numid);
            }

            ++thisMaterial->niso;
            thisMaterial->isorh(thisMaterial->niso) = rhmax;
            thisMaterial->isodata(thisMaterial->niso) = thisMaterial->Porosity * wdensity;

            ++thisMaterial->niso;
            thisMaterial->isorh(thisMaterial->niso) = 0.0;
            thisMaterial->isodata(thisMaterial->niso) = 0.0;
        }

        // check the isotherm
        for (matid = 1; matid <= state.dataMaterial->TotMaterials; ++matid) {
            auto *thisMaterial = dynamic_cast<Material::MaterialChild *>(state.dataMaterial->Material(matid));
            assert(thisMaterial != nullptr);
            if (thisMaterial->niso > 0) {
                // - First sort
                for (jj = 1; jj <= thisMaterial->niso - 1; ++jj) {
                    for (ii = jj + 1; ii <= thisMaterial->niso; ++ii) {
                        if (thisMaterial->isorh(jj) > thisMaterial->isorh(ii)) {

                            dumrh = thisMaterial->isorh(jj);
                            dumdata = thisMaterial->isodata(jj);

                            thisMaterial->isorh(jj) = thisMaterial->isorh(ii);
                            thisMaterial->isodata(jj) = thisMaterial->isodata(ii);

                            thisMaterial->isorh(ii) = dumrh;
                            thisMaterial->isodata(ii) = dumdata;
                        }
                    }
                }
                //- Now make sure the data rises
                isoerrrise = false;
                for (ii = 1; ii <= 100; ++ii) {
                    avflag = true;
                    for (jj = 1; jj <= thisMaterial->niso - 1; ++jj) {
                        if (thisMaterial->isodata(jj) > thisMaterial->isodata(jj + 1)) {
                            isoerrrise = true;
                            avdata = (thisMaterial->isodata(jj) + thisMaterial->isodata(jj + 1)) / 2.0;
                            thisMaterial->isodata(jj) = avdata;
                            thisMaterial->isodata(jj + 1) = avdata;
                            avflag = false;
                        }
                    }
                    if (avflag) break;
                }
                if (isoerrrise) {
                    ShowWarningError(state, format("{} data not rising - Check material {}", cHAMTObject2, thisMaterial->Name));
                    ShowContinueError(state, "Isotherm data has been fixed, and the simulation continues.");
                }
            }
        }

        HAMTitems =
            state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, cHAMTObject3); // MaterialProperty:HeatAndMoistureTransfer:Suction
        for (item = 1; item <= HAMTitems; ++item) {
            state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                     cHAMTObject3,
                                                                     item,
                                                                     AlphaArray,
                                                                     NumAlphas,
                                                                     NumArray,
                                                                     NumNums,
                                                                     status,
                                                                     lNumericBlanks,
                                                                     lAlphaBlanks,
                                                                     cAlphaFieldNames,
                                                                     cNumericFieldNames);

            matid = UtilityRoutines::FindItemInPtrList(AlphaArray(1), state.dataMaterial->Material);

            if (matid == 0) {
                ShowSevereError(state, format("{} {}=\"{}\" is invalid (undefined).", cHAMTObject3, cAlphaFieldNames(1), AlphaArray(1)));
                ShowContinueError(state, "The basic material must be defined in addition to specifying HeatAndMoistureTransfer properties.");
                ErrorsFound = true;
                continue;
            }
            auto *thisMaterial = dynamic_cast<Material::MaterialChild *>(state.dataMaterial->Material(matid));
            assert(thisMaterial != nullptr);
            if (thisMaterial->ROnly) {
                ShowWarningError(state,
                                 format("{} {}=\"{}\" is defined as an R-only value material.", cHAMTObject3, cAlphaFieldNames(1), AlphaArray(1)));
                continue;
            }

            Numid = 1;

            thisMaterial->nsuc = NumArray(Numid);
            for (suc = 1; suc <= thisMaterial->nsuc; ++suc) {
                ++Numid;
                thisMaterial->sucwater(suc) = NumArray(Numid);
                ++Numid;
                thisMaterial->sucdata(suc) = NumArray(Numid);
            }

            ++thisMaterial->nsuc;
            thisMaterial->sucwater(thisMaterial->nsuc) = thisMaterial->isodata(thisMaterial->niso);
            thisMaterial->sucdata(thisMaterial->nsuc) = thisMaterial->sucdata(thisMaterial->nsuc - 1);
        }

        HAMTitems = state.dataInputProcessing->inputProcessor->getNumObjectsFound(
            state, cHAMTObject4); // MaterialProperty:HeatAndMoistureTransfer:Redistribution
        for (item = 1; item <= HAMTitems; ++item) {
            state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                     cHAMTObject4,
                                                                     item,
                                                                     AlphaArray,
                                                                     NumAlphas,
                                                                     NumArray,
                                                                     NumNums,
                                                                     status,
                                                                     lNumericBlanks,
                                                                     lAlphaBlanks,
                                                                     cAlphaFieldNames,
                                                                     cNumericFieldNames);

            matid = UtilityRoutines::FindItemInPtrList(AlphaArray(1), state.dataMaterial->Material);
            if (matid == 0) {
                ShowSevereError(state, format("{} {}=\"{}\" is invalid (undefined).", cHAMTObject4, cAlphaFieldNames(1), AlphaArray(1)));
                ShowContinueError(state, "The basic material must be defined in addition to specifying HeatAndMoistureTransfer properties.");
                ErrorsFound = true;
                continue;
            }
            auto *thisMaterial = dynamic_cast<Material::MaterialChild *>(state.dataMaterial->Material(matid));
            assert(thisMaterial != nullptr);
            if (thisMaterial->ROnly) {
                ShowWarningError(state,
                                 format("{} {}=\"{}\" is defined as an R-only value material.", cHAMTObject4, cAlphaFieldNames(1), AlphaArray(1)));
                continue;
            }
            Numid = 1;

            thisMaterial->nred = NumArray(Numid);
            for (red = 1; red <= thisMaterial->nred; ++red) {
                ++Numid;
                thisMaterial->redwater(red) = NumArray(Numid);
                ++Numid;
                thisMaterial->reddata(red) = NumArray(Numid);
            }

            ++thisMaterial->nred;
            thisMaterial->redwater(thisMaterial->nred) = thisMaterial->isodata(thisMaterial->niso);
            thisMaterial->reddata(thisMaterial->nred) = thisMaterial->reddata(thisMaterial->nred - 1);
        }

        HAMTitems =
            state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, cHAMTObject5); // MaterialProperty:HeatAndMoistureTransfer:Diffusion
        for (item = 1; item <= HAMTitems; ++item) {
            state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                     cHAMTObject5,
                                                                     item,
                                                                     AlphaArray,
                                                                     NumAlphas,
                                                                     NumArray,
                                                                     NumNums,
                                                                     status,
                                                                     lNumericBlanks,
                                                                     lAlphaBlanks,
                                                                     cAlphaFieldNames,
                                                                     cNumericFieldNames);

            matid = UtilityRoutines::FindItemInPtrList(AlphaArray(1), state.dataMaterial->Material);
            if (matid == 0) {
                ShowSevereError(state, format("{} {}=\"{}\" is invalid (undefined).", cHAMTObject5, cAlphaFieldNames(1), AlphaArray(1)));
                ShowContinueError(state, "The basic material must be defined in addition to specifying HeatAndMoistureTransfer properties.");
                ErrorsFound = true;
                continue;
            }
            auto *thisMaterial = dynamic_cast<Material::MaterialChild *>(state.dataMaterial->Material(matid));
            assert(thisMaterial != nullptr);
            if (thisMaterial->ROnly) {
                ShowWarningError(state,
                                 format("{} {}=\"{}\" is defined as an R-only value material.", cHAMTObject5, cAlphaFieldNames(1), AlphaArray(1)));
                continue;
            }

            Numid = 1;

            thisMaterial->nmu = NumArray(Numid);
            if (thisMaterial->nmu > 0) {
                for (mu = 1; mu <= thisMaterial->nmu; ++mu) {
                    ++Numid;
                    thisMaterial->murh(mu) = NumArray(Numid);
                    ++Numid;
                    thisMaterial->mudata(mu) = NumArray(Numid);
                }

                ++thisMaterial->nmu;
                thisMaterial->murh(thisMaterial->nmu) = thisMaterial->isorh(thisMaterial->niso);
                thisMaterial->mudata(thisMaterial->nmu) = thisMaterial->mudata(thisMaterial->nmu - 1);
            }
        }

        HAMTitems = state.dataInputProcessing->inputProcessor->getNumObjectsFound(
            state, cHAMTObject6); // MaterialProperty:HeatAndMoistureTransfer:ThermalConductivity
        for (item = 1; item <= HAMTitems; ++item) {
            state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                     cHAMTObject6,
                                                                     item,
                                                                     AlphaArray,
                                                                     NumAlphas,
                                                                     NumArray,
                                                                     NumNums,
                                                                     status,
                                                                     lNumericBlanks,
                                                                     lAlphaBlanks,
                                                                     cAlphaFieldNames,
                                                                     cNumericFieldNames);

            matid = UtilityRoutines::FindItemInPtrList(AlphaArray(1), state.dataMaterial->Material);
            if (matid == 0) {
                ShowSevereError(state, format("{} {}=\"{}\" is invalid (undefined).", cHAMTObject6, cAlphaFieldNames(1), AlphaArray(1)));
                ShowContinueError(state, "The basic material must be defined in addition to specifying HeatAndMoistureTransfer properties.");
                ErrorsFound = true;
                continue;
            }
            auto *thisMaterial = dynamic_cast<Material::MaterialChild *>(state.dataMaterial->Material(matid));
            assert(thisMaterial != nullptr);
            if (thisMaterial->ROnly) {
                ShowWarningError(state,
                                 format("{} {}=\"{}\" is defined as an R-only value material.", cHAMTObject6, cAlphaFieldNames(1), AlphaArray(1)));
                continue;
            }
            Numid = 1;

            thisMaterial->ntc = NumArray(Numid);
            if (thisMaterial->ntc > 0) {
                for (tc = 1; tc <= thisMaterial->ntc; ++tc) {
                    ++Numid;
                    thisMaterial->tcwater(tc) = NumArray(Numid);
                    ++Numid;
                    thisMaterial->tcdata(tc) = NumArray(Numid);
                }

                ++thisMaterial->ntc;
                thisMaterial->tcwater(thisMaterial->ntc) = thisMaterial->isodata(thisMaterial->niso);
                thisMaterial->tcdata(thisMaterial->ntc) = thisMaterial->tcdata(thisMaterial->ntc - 1);
            }
        }

        // Vapor Transfer coefficients
        HAMTitems = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, cHAMTObject7); // SurfaceProperties:VaporCoefficients
        for (item = 1; item <= HAMTitems; ++item) {
            state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                     cHAMTObject7,
                                                                     item,
                                                                     AlphaArray,
                                                                     NumAlphas,
                                                                     NumArray,
                                                                     NumNums,
                                                                     status,
                                                                     lNumericBlanks,
                                                                     lAlphaBlanks,
                                                                     cAlphaFieldNames,
                                                                     cNumericFieldNames);

            vtcsid = UtilityRoutines::FindItemInList(AlphaArray(1), state.dataSurface->Surface);
            if (vtcsid == 0) {
                ShowSevereError(state, format("{} {}=\"{}\" is invalid (undefined).", cHAMTObject7, cAlphaFieldNames(1), AlphaArray(1)));
                ShowContinueError(state, "The basic material must be defined in addition to specifying HeatAndMoistureTransfer properties.");
                ErrorsFound = true;
                continue;
            }

            if (AlphaArray(2) == "YES") {
                state.dataHeatBalHAMTMgr->extvtcflag(vtcsid) = true;
                state.dataHeatBalHAMTMgr->extvtc(vtcsid) = NumArray(1);
            }

            if (AlphaArray(3) == "YES") {
                state.dataHeatBalHAMTMgr->intvtcflag(vtcsid) = true;
                state.dataHeatBalHAMTMgr->intvtc(vtcsid) = NumArray(2);
            }
        }

        AlphaArray.deallocate();
        cAlphaFieldNames.deallocate();
        cNumericFieldNames.deallocate();
        NumArray.deallocate();
        lAlphaBlanks.deallocate();
        lNumericBlanks.deallocate();

        if (ErrorsFound) {
            ShowFatalError(state, "GetHeatBalHAMTInput: Errors found getting input.  Program terminates.");
        }
    }

    void InitHeatBalHAMT(EnergyPlusData &state)
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR         Phillip Biddulph
        //       DATE WRITTEN   June 2008
        //       MODIFIED       B. Griffith, Aug 2012 for surface-specific algorithms
        //       RE-ENGINEERED  na

        // Using/Aliasing
        using General::ScanForReports;

        // Locals
        // SUBROUTINE PARAMETER DEFINITIONS:
        Real64 constexpr adjdist(0.00005); // Allowable distance between two cells, also used as limit on cell length
        static constexpr std::string_view RoutineName("InitCombinedHeatAndMoistureFiniteElement: ");

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int ii;
        int cid;
        int cid1;
        int cid2;
        int sid;
        int conid;
        int lid;
        int matid;
        int did;
        int adj1;
        int adj2;
        int errorCount;
        int MaterNum;

        Real64 runor;
        Real64 high1;
        Real64 low2;
        Real64 testlen;
        Real64 waterd; // water density
        bool DoReport;

        auto &cells(state.dataHeatBalHAMTMgr->cells);

        state.dataHeatBalHAMTMgr->deltat = state.dataGlobal->TimeStepZone * 3600.0;

        // Check the materials information and work out how many cells are required.
        errorCount = 0;
        state.dataHeatBalHAMTMgr->TotCellsMax = 0;
        for (sid = 1; sid <= state.dataSurface->TotSurfaces; ++sid) {
            if (state.dataSurface->Surface(sid).Class == SurfaceClass::Window) continue;
            if (state.dataSurface->Surface(sid).HeatTransferAlgorithm != DataSurfaces::HeatTransferModel::HAMT) continue;
            conid = state.dataSurface->Surface(sid).Construction;
            if (conid == 0) continue;
            for (lid = 1; lid <= state.dataConstruction->Construct(conid).TotLayers; ++lid) {
                matid = state.dataConstruction->Construct(conid).LayerPoint(lid);
                auto *thisMaterial = dynamic_cast<Material::MaterialChild *>(state.dataMaterial->Material(matid));
                assert(thisMaterial != nullptr);
                if (thisMaterial->ROnly) {
                    ShowSevereError(state,
                                    format("{}Construction={} cannot contain R-only value materials.",
                                           RoutineName,
                                           state.dataConstruction->Construct(conid).Name));
                    ShowContinueError(state, format("Reference Material=\"{}\".", thisMaterial->Name));
                    ++errorCount;
                    continue;
                }

                if (thisMaterial->nmu < 0) {
                    ShowSevereError(state, format("{}Construction={}", RoutineName, state.dataConstruction->Construct(conid).Name));
                    ShowContinueError(state,
                                      format("Reference Material=\"{}\" does not have required Water Vapor Diffusion Resistance Factor (mu) data.",
                                             thisMaterial->Name));
                    ++errorCount;
                }

                if (thisMaterial->niso < 0) {
                    ShowSevereError(state, format("{}Construction={}", RoutineName, state.dataConstruction->Construct(conid).Name));
                    ShowContinueError(state, format("Reference Material=\"{}\" does not have required isotherm data.", thisMaterial->Name));
                    ++errorCount;
                }
                if (thisMaterial->nsuc < 0) {
                    ShowSevereError(state, format("{}Construction={}", RoutineName, state.dataConstruction->Construct(conid).Name));
                    ShowContinueError(
                        state,
                        format("Reference Material=\"{}\" does not have required liquid transport coefficient (suction) data.", thisMaterial->Name));
                    ++errorCount;
                }
                if (thisMaterial->nred < 0) {
                    ShowSevereError(state, format("{}Construction={}", RoutineName, state.dataConstruction->Construct(conid).Name));
                    ShowContinueError(state,
                                      format("Reference Material=\"{}\" does not have required liquid transport coefficient (redistribution) data.",
                                             thisMaterial->Name));
                    ++errorCount;
                }
                if (thisMaterial->ntc < 0) {
                    if (thisMaterial->Conductivity > 0) {
                        ShowWarningError(state, format("{}Construction={}", RoutineName, state.dataConstruction->Construct(conid).Name));
                        ShowContinueError(
                            state,
                            format("Reference Material=\"{}\" does not have thermal conductivity data. Using fixed value.", thisMaterial->Name));
                        thisMaterial->ntc = 2;
                        thisMaterial->tcwater(1) = 0.0;
                        thisMaterial->tcdata(1) = thisMaterial->Conductivity;
                        thisMaterial->tcwater(2) = thisMaterial->isodata(thisMaterial->niso);
                        thisMaterial->tcdata(2) = thisMaterial->Conductivity;
                    } else {
                        ShowSevereError(state, format("{}Construction={}", RoutineName, state.dataConstruction->Construct(conid).Name));
                        ShowContinueError(state,
                                          format("Reference Material=\"{}\" does not have required thermal conductivity data.", thisMaterial->Name));
                        ++errorCount;
                    }
                }

                // convert material water content to RH

                waterd = thisMaterial->iwater * thisMaterial->Density;
                interp(thisMaterial->niso, thisMaterial->isodata, thisMaterial->isorh, waterd, thisMaterial->irh);

                thisMaterial->divs = int(thisMaterial->Thickness / thisMaterial->divsize) + thisMaterial->divmin;
                if (thisMaterial->divs > thisMaterial->divmax) {
                    thisMaterial->divs = thisMaterial->divmax;
                }
                // Check length of cell - reduce number of divisions if necessary
                Real64 const sin_negPIOvr2 = std::sin(-DataGlobalConstants::Pi / 2.0);
                while (true) {
                    testlen = thisMaterial->Thickness *
                              ((std::sin(DataGlobalConstants::Pi * (-1.0 / double(thisMaterial->divs)) - DataGlobalConstants::Pi / 2.0) / 2.0) -
                               (sin_negPIOvr2 / 2.0));
                    if (testlen > adjdist) break;
                    --thisMaterial->divs;
                    if (thisMaterial->divs < 1) {
                        ShowSevereError(state, format("{}Construction={}", RoutineName, state.dataConstruction->Construct(conid).Name));
                        ShowContinueError(state, format("Reference Material=\"{}\" is too thin.", thisMaterial->Name));
                        ++errorCount;
                        break;
                    }
                }
                state.dataHeatBalHAMTMgr->TotCellsMax += thisMaterial->divs;
            }
            state.dataHeatBalHAMTMgr->TotCellsMax += 7;
        }

        if (errorCount > 0) {
            ShowFatalError(state, "CombinedHeatAndMoistureFiniteElement: Incomplete data to start solution, program terminates.");
        }

        // Make the cells and initialize
        cells.allocate(state.dataHeatBalHAMTMgr->TotCellsMax);
        for (auto &e : cells) {
            e.adjs = -1;
            e.adjsl = -1;
        }

        cid = 0;

        // Set up surface cell structure
        for (sid = 1; sid <= state.dataSurface->TotSurfaces; ++sid) {
            if (!state.dataSurface->Surface(sid).HeatTransSurf) continue;
            if (state.dataSurface->Surface(sid).Class == SurfaceClass::Window) continue;
            if (state.dataSurface->Surface(sid).HeatTransferAlgorithm != DataSurfaces::HeatTransferModel::HAMT) continue;
            // Boundary Cells
            runor = -0.02;
            // Air Convection Cell
            ++cid;
            state.dataHeatBalHAMTMgr->firstcell(sid) = cid;
            state.dataHeatBalHAMTMgr->ExtConcell(sid) = cid;
            cells(cid).rh = 0.0;
            cells(cid).sid = sid;
            cells(cid).length(1) = 0.01;
            cells(cid).origin(1) = cells(cid).length(1) / 2.0 + runor;

            // Air Radiation Cell
            ++cid;
            state.dataHeatBalHAMTMgr->ExtRadcell(sid) = cid;
            cells(cid).rh = 0.0;
            cells(cid).sid = sid;
            cells(cid).length(1) = 0.01;
            cells(cid).origin(1) = cells(cid).length(1) / 2.0 + runor;

            // Sky Cell
            ++cid;
            state.dataHeatBalHAMTMgr->ExtSkycell(sid) = cid;
            cells(cid).rh = 0.0;
            cells(cid).sid = sid;
            cells(cid).length(1) = 0.01;
            cells(cid).origin(1) = cells(cid).length(1) / 2.0 + runor;

            // Ground Cell
            ++cid;
            state.dataHeatBalHAMTMgr->ExtGrncell(sid) = cid;
            cells(cid).rh = 0.0;
            cells(cid).sid = sid;
            cells(cid).length(1) = 0.01;
            cells(cid).origin(1) = cells(cid).length(1) / 2.0 + runor;
            runor += cells(cid).length(1);

            // External Virtual Cell
            ++cid;
            state.dataHeatBalHAMTMgr->Extcell(sid) = cid;
            cells(cid).rh = 0.0;
            cells(cid).sid = sid;
            cells(cid).length(1) = 0.01;
            cells(cid).origin(1) = cells(cid).length(1) / 2.0 + runor;
            runor += cells(cid).length(1);

            // Material Cells
            conid = state.dataSurface->Surface(sid).Construction;
            for (lid = 1; lid <= state.dataConstruction->Construct(conid).TotLayers; ++lid) {
                matid = state.dataConstruction->Construct(conid).LayerPoint(lid);
                auto const *thisMaterial = dynamic_cast<const Material::MaterialChild *>(state.dataMaterial->Material(matid));
                assert(thisMaterial != nullptr);

                for (did = 1; did <= thisMaterial->divs; ++did) {
                    ++cid;

                    cells(cid).matid = matid;
                    cells(cid).sid = sid;

                    cells(cid).temp = thisMaterial->itemp;
                    cells(cid).tempp1 = thisMaterial->itemp;
                    cells(cid).tempp2 = thisMaterial->itemp;

                    cells(cid).rh = thisMaterial->irh;
                    cells(cid).rhp1 = thisMaterial->irh;
                    cells(cid).rhp2 = thisMaterial->irh;

                    cells(cid).density = thisMaterial->Density;
                    cells(cid).spech = thisMaterial->SpecHeat;

                    // Make cells smaller near the surface
                    cells(cid).length(1) =
                        thisMaterial->Thickness *
                        ((std::sin(DataGlobalConstants::Pi * (-double(did) / double(thisMaterial->divs)) - DataGlobalConstants::Pi / 2.0) / 2.0) -
                         (std::sin(DataGlobalConstants::Pi * (-double(did - 1) / double(thisMaterial->divs)) - DataGlobalConstants::Pi / 2.0) / 2.0));

                    cells(cid).origin(1) = runor + cells(cid).length(1) / 2.0;
                    runor += cells(cid).length(1);

                    cells(cid).volume = cells(cid).length(1) * state.dataSurface->Surface(sid).Area;
                }
            }

            // Interior Virtual Cell
            ++cid;
            state.dataHeatBalHAMTMgr->Intcell(sid) = cid;
            cells(cid).sid = sid;
            cells(cid).rh = 0.0;
            cells(cid).length(1) = 0.01;
            cells(cid).origin(1) = cells(cid).length(1) / 2.0 + runor;
            runor += cells(cid).length(1);

            // Air Convection Cell
            ++cid;
            state.dataHeatBalHAMTMgr->lastcell(sid) = cid;
            state.dataHeatBalHAMTMgr->IntConcell(sid) = cid;
            cells(cid).rh = 0.0;
            cells(cid).sid = sid;
            cells(cid).length(1) = 0.01;
            cells(cid).origin(1) = cells(cid).length(1) / 2.0 + runor;
        }

        // Find adjacent cells.
        for (cid1 = 1; cid1 <= state.dataHeatBalHAMTMgr->TotCellsMax; ++cid1) {
            for (cid2 = 1; cid2 <= state.dataHeatBalHAMTMgr->TotCellsMax; ++cid2) {
                if ((cid1 != cid2) && (cells(cid1).sid == cells(cid2).sid)) {
                    high1 = cells(cid1).origin(1) + cells(cid1).length(1) / 2.0;
                    low2 = cells(cid2).origin(1) - cells(cid2).length(1) / 2.0;
                    if (std::abs(low2 - high1) < adjdist) {
                        adj1 = 0;
                        for (ii = 1; ii <= adjmax; ++ii) {
                            ++adj1;
                            if (cells(cid1).adjs(adj1) == -1) break;
                        }
                        adj2 = 0;
                        for (ii = 1; ii <= adjmax; ++ii) {
                            ++adj2;
                            if (cells(cid2).adjs(adj2) == -1) break;
                        }
                        cells(cid1).adjs(adj1) = cid2;
                        cells(cid2).adjs(adj2) = cid1;

                        cells(cid1).adjsl(adj1) = adj2;
                        cells(cid2).adjsl(adj2) = adj1;

                        sid = cells(cid1).sid;
                        cells(cid1).overlap(adj1) = state.dataSurface->Surface(sid).Area;
                        cells(cid2).overlap(adj2) = state.dataSurface->Surface(sid).Area;
                        cells(cid1).dist(adj1) = cells(cid1).length(1) / 2.0;
                        cells(cid2).dist(adj2) = cells(cid2).length(1) / 2.0;
                    }
                }
            }
        }

        // Reset surface virtual cell origins and volumes. Initialize report variables.
        static constexpr std::string_view Format_1966("! <HAMT cells>, Surface Name, Construction Name, Cell Numbers\n");
        print(state.files.eio, Format_1966);
        static constexpr std::string_view Format_1965("! <HAMT origins>, Surface Name, Construction Name, Cell origins (m) \n");
        print(state.files.eio, Format_1965);
        // cCurrentModuleObject='MaterialProperty:HeatAndMoistureTransfer:*'
        for (sid = 1; sid <= state.dataSurface->TotSurfaces; ++sid) {
            if (!state.dataSurface->Surface(sid).HeatTransSurf) continue;
            if (state.dataSurface->Surface(sid).Class == SurfaceClass::Window) continue;
            if (state.dataSurface->Surface(sid).HeatTransferAlgorithm != DataSurfaces::HeatTransferModel::HAMT) continue;
            cells(state.dataHeatBalHAMTMgr->Extcell(sid)).origin(1) += cells(state.dataHeatBalHAMTMgr->Extcell(sid)).length(1) / 2.0;
            cells(state.dataHeatBalHAMTMgr->Intcell(sid)).origin(1) -= cells(state.dataHeatBalHAMTMgr->Intcell(sid)).length(1) / 2.0;
            cells(state.dataHeatBalHAMTMgr->Extcell(sid)).volume = 0.0;
            cells(state.dataHeatBalHAMTMgr->Intcell(sid)).volume = 0.0;
            state.dataHeatBalHAMTMgr->watertot(sid) = 0.0;
            state.dataHeatBalHAMTMgr->surfrh(sid) = 0.0;
            state.dataHeatBalHAMTMgr->surfextrh(sid) = 0.0;
            state.dataHeatBalHAMTMgr->surftemp(sid) = 0.0;
            state.dataHeatBalHAMTMgr->surfexttemp(sid) = 0.0;
            state.dataHeatBalHAMTMgr->surfvp(sid) = 0.0;
            SetupOutputVariable(state,
                                "HAMT Surface Average Water Content Ratio",
                                OutputProcessor::Unit::kg_kg,
                                state.dataHeatBalHAMTMgr->watertot(sid),
                                OutputProcessor::SOVTimeStepType::Zone,
                                OutputProcessor::SOVStoreType::State,
                                state.dataSurface->Surface(sid).Name);
            SetupOutputVariable(state,
                                "HAMT Surface Inside Face Temperature",
                                OutputProcessor::Unit::C,
                                state.dataHeatBalHAMTMgr->surftemp(sid),
                                OutputProcessor::SOVTimeStepType::Zone,
                                OutputProcessor::SOVStoreType::State,
                                state.dataSurface->Surface(sid).Name);
            SetupOutputVariable(state,
                                "HAMT Surface Inside Face Relative Humidity",
                                OutputProcessor::Unit::Perc,
                                state.dataHeatBalHAMTMgr->surfrh(sid),
                                OutputProcessor::SOVTimeStepType::Zone,
                                OutputProcessor::SOVStoreType::State,
                                state.dataSurface->Surface(sid).Name);
            SetupOutputVariable(state,
                                "HAMT Surface Inside Face Vapor Pressure",
                                OutputProcessor::Unit::Pa,
                                state.dataHeatBalHAMTMgr->surfvp(sid),
                                OutputProcessor::SOVTimeStepType::Zone,
                                OutputProcessor::SOVStoreType::State,
                                state.dataSurface->Surface(sid).Name);
            SetupOutputVariable(state,
                                "HAMT Surface Outside Face Temperature",
                                OutputProcessor::Unit::C,
                                state.dataHeatBalHAMTMgr->surfexttemp(sid),
                                OutputProcessor::SOVTimeStepType::Zone,
                                OutputProcessor::SOVStoreType::State,
                                state.dataSurface->Surface(sid).Name);
            SetupOutputVariable(state,
                                "HAMT Surface Outside Face Relative Humidity",
                                OutputProcessor::Unit::Perc,
                                state.dataHeatBalHAMTMgr->surfextrh(sid),
                                OutputProcessor::SOVTimeStepType::Zone,
                                OutputProcessor::SOVStoreType::State,
                                state.dataSurface->Surface(sid).Name);

            // write cell origins to initialization output file
            conid = state.dataSurface->Surface(sid).Construction;
            print(state.files.eio, "HAMT cells, {},{}", state.dataSurface->Surface(sid).Name, state.dataConstruction->Construct(conid).Name);
            for (int concell = 1, concell_end = state.dataHeatBalHAMTMgr->Intcell(sid) - state.dataHeatBalHAMTMgr->Extcell(sid) + 1;
                 concell <= concell_end;
                 ++concell) {
                print(state.files.eio, ",{:4}", concell);
            }
            print(state.files.eio, "\n");
            print(state.files.eio, "HAMT origins,{},{}", state.dataSurface->Surface(sid).Name, state.dataConstruction->Construct(conid).Name);
            for (int cellid = state.dataHeatBalHAMTMgr->Extcell(sid); cellid <= state.dataHeatBalHAMTMgr->Intcell(sid); ++cellid) {
                print(state.files.eio, ",{:10.7F}", cells(cellid).origin(1));
            }
            print(state.files.eio, "\n");

            for (int cellid = state.dataHeatBalHAMTMgr->Extcell(sid), concell = 1; cellid <= state.dataHeatBalHAMTMgr->Intcell(sid);
                 ++cellid, ++concell) {
                SetupOutputVariable(state,
                                    format("HAMT Surface Temperature Cell {}", concell),
                                    OutputProcessor::Unit::C,
                                    cells(cellid).temp,
                                    OutputProcessor::SOVTimeStepType::Zone,
                                    OutputProcessor::SOVStoreType::State,
                                    state.dataSurface->Surface(sid).Name);
            }
            for (int cellid = state.dataHeatBalHAMTMgr->Extcell(sid), concell = 1; cellid <= state.dataHeatBalHAMTMgr->Intcell(sid);
                 ++cellid, ++concell) {
                SetupOutputVariable(state,
                                    format("HAMT Surface Water Content Cell {}", concell),
                                    OutputProcessor::Unit::kg_kg,
                                    cells(cellid).wreport,
                                    OutputProcessor::SOVTimeStepType::Zone,
                                    OutputProcessor::SOVStoreType::State,
                                    state.dataSurface->Surface(sid).Name);
            }
            for (int cellid = state.dataHeatBalHAMTMgr->Extcell(sid), concell = 1; cellid <= state.dataHeatBalHAMTMgr->Intcell(sid);
                 ++cellid, ++concell) {
                SetupOutputVariable(state,
                                    format("HAMT Surface Relative Humidity Cell {}", concell),
                                    OutputProcessor::Unit::Perc,
                                    cells(cellid).rhp,
                                    OutputProcessor::SOVTimeStepType::Zone,
                                    OutputProcessor::SOVStoreType::State,
                                    state.dataSurface->Surface(sid).Name);
            }
        }

        ScanForReports(state, "Constructions", DoReport, "Constructions");
        if (DoReport) {

            static constexpr std::string_view Format_108("! <Material Nominal Resistance>, Material Name,  Nominal R\n");
            print(state.files.eio, Format_108);

            for (MaterNum = 1; MaterNum <= state.dataMaterial->TotMaterials; ++MaterNum) {

                static constexpr std::string_view Format_111("Material Nominal Resistance,{},{:.4R}\n");
                print(state.files.eio, Format_111, state.dataMaterial->Material(MaterNum)->Name, state.dataHeatBal->NominalR(MaterNum));
            }
        }
    }

    void CalcHeatBalHAMT(EnergyPlusData &state, int const sid, Real64 &SurfTempInTmp, Real64 &TempSurfOutTmp)
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR         Phillip Biddulph
        //       DATE WRITTEN   June 2008
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // To calculate the heat and moisture transfer through the surface

        // Using/Aliasing
        using DataSurfaces::OtherSideCondModeledExt;

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:

        // SUBROUTINE PARAMETER DEFINITIONS:
        static std::string const HAMTExt("HAMT-Ext");
        static std::string const HAMTInt("HAMT-Int");

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        Real64 SurfTempInP;
        Real64 RhoIn;
        Real64 RhoOut;
        Real64 torsum;
        Real64 oorsum;
        Real64 phioosum;
        Real64 phiorsum;
        Real64 vpoosum;
        Real64 vporsum;
        Real64 rhr1;
        Real64 rhr2;
        Real64 wcap;
        Real64 thermr1;
        Real64 thermr2;
        Real64 tcap;
        Real64 qvp;
        Real64 vaporr1;
        Real64 vaporr2;
        Real64 vpdiff;
        Real64 sumtp1;
        Real64 tempmax;
        Real64 tempmin;

        int ii;
        int matid;
        int itter;
        int cid;
        int adj;
        int adjl;

        Real64 denominator;

        auto &cells(state.dataHeatBalHAMTMgr->cells);
        auto &Extcell(state.dataHeatBalHAMTMgr->Extcell);
        auto &Intcell(state.dataHeatBalHAMTMgr->Intcell);

        if (state.dataGlobal->BeginEnvrnFlag && state.dataHeatBalHAMTMgr->MyEnvrnFlag(sid)) {
            cells(Extcell(sid)).rh = 0.0;
            cells(Extcell(sid)).rhp1 = 0.0;
            cells(Extcell(sid)).rhp2 = 0.0;

            cells(Extcell(sid)).temp = 10.0;
            cells(Extcell(sid)).tempp1 = 10.0;
            cells(Extcell(sid)).tempp2 = 10.0;

            cells(Intcell(sid)).rh = 0.0;
            cells(Intcell(sid)).rhp1 = 0.0;
            cells(Intcell(sid)).rhp2 = 0.0;

            cells(Intcell(sid)).temp = 10.0;
            cells(Intcell(sid)).tempp1 = 10.0;
            cells(Intcell(sid)).tempp2 = 10.0;

            for (cid = Extcell(sid) + 1; cid <= Intcell(sid) - 1; ++cid) {
                matid = cells(cid).matid;

                auto const *thisMaterial = dynamic_cast<const Material::MaterialChild *>(state.dataMaterial->Material(matid));
                assert(thisMaterial != nullptr);
                cells(cid).temp = thisMaterial->itemp;
                cells(cid).tempp1 = thisMaterial->itemp;
                cells(cid).tempp2 = thisMaterial->itemp;

                cells(cid).rh = thisMaterial->irh;
                cells(cid).rhp1 = thisMaterial->irh;
                cells(cid).rhp2 = thisMaterial->irh;
            }
            state.dataHeatBalHAMTMgr->MyEnvrnFlag(sid) = false;
        }
        if (!state.dataGlobal->BeginEnvrnFlag) {
            state.dataHeatBalHAMTMgr->MyEnvrnFlag(sid) = true;
        }

        // Set all the boundary values
        cells(state.dataHeatBalHAMTMgr->ExtRadcell(sid)).temp = state.dataMstBal->TempOutsideAirFD(sid);
        cells(state.dataHeatBalHAMTMgr->ExtConcell(sid)).temp = state.dataMstBal->TempOutsideAirFD(sid);
        auto &thisZoneHB = state.dataZoneTempPredictorCorrector->zoneHeatBalance(state.dataSurface->Surface(sid).Zone);
        if (state.dataSurface->Surface(sid).ExtBoundCond == OtherSideCondModeledExt) {
            // CR8046 switch modeled rad temp for sky temp.
            cells(state.dataHeatBalHAMTMgr->ExtSkycell(sid)).temp = state.dataSurface->OSCM(state.dataSurface->Surface(sid).OSCMPtr).TRad;
            cells(Extcell(sid)).Qadds = 0.0; // eliminate incident shortwave on underlying surface
        } else {
            cells(state.dataHeatBalHAMTMgr->ExtSkycell(sid)).temp = state.dataEnvrn->SkyTemp;

            cells(Extcell(sid)).Qadds = state.dataSurface->Surface(sid).Area * state.dataHeatBalSurf->SurfOpaqQRadSWOutAbs(sid);
        }

        cells(state.dataHeatBalHAMTMgr->ExtGrncell(sid)).temp = state.dataMstBal->TempOutsideAirFD(sid);
        RhoOut = state.dataMstBal->RhoVaporAirOut(sid);

        // Special case when the surface is an internal mass
        if (state.dataSurface->Surface(sid).ExtBoundCond == sid) {
            cells(state.dataHeatBalHAMTMgr->ExtConcell(sid)).temp = thisZoneHB.MAT;
            RhoOut = state.dataMstBal->RhoVaporAirIn(sid);
        }

        RhoIn = state.dataMstBal->RhoVaporAirIn(sid);

        cells(state.dataHeatBalHAMTMgr->ExtRadcell(sid)).htc = state.dataMstBal->HAirFD(sid);
        cells(state.dataHeatBalHAMTMgr->ExtConcell(sid)).htc = state.dataMstBal->HConvExtFD(sid);
        cells(state.dataHeatBalHAMTMgr->ExtSkycell(sid)).htc = state.dataMstBal->HSkyFD(sid);
        cells(state.dataHeatBalHAMTMgr->ExtGrncell(sid)).htc = state.dataMstBal->HGrndFD(sid);

        cells(state.dataHeatBalHAMTMgr->IntConcell(sid)).temp = thisZoneHB.MAT;

        cells(state.dataHeatBalHAMTMgr->IntConcell(sid)).htc = state.dataMstBal->HConvInFD(sid);

        cells(Intcell(sid)).Qadds = state.dataSurface->Surface(sid).Area *
                                    (state.dataHeatBalSurf->SurfOpaqQRadSWInAbs(sid) + state.dataHeatBalSurf->SurfQdotRadNetLWInPerArea(sid) +
                                     state.dataHeatBalSurf->SurfQdotRadHVACInPerArea(sid) + state.dataHeatBal->SurfQdotRadIntGainsInPerArea(sid) +
                                     state.dataHeatBalSurf->SurfQAdditionalHeatSourceInside(sid));

        cells(state.dataHeatBalHAMTMgr->ExtConcell(sid)).rh =
            PsyRhFnTdbRhov(state, cells(state.dataHeatBalHAMTMgr->ExtConcell(sid)).temp, RhoOut, HAMTExt);
        cells(state.dataHeatBalHAMTMgr->IntConcell(sid)).rh =
            PsyRhFnTdbRhov(state, cells(state.dataHeatBalHAMTMgr->IntConcell(sid)).temp, RhoIn, HAMTInt);

        if (cells(state.dataHeatBalHAMTMgr->ExtConcell(sid)).rh > rhmax) {
            cells(state.dataHeatBalHAMTMgr->ExtConcell(sid)).rh = rhmax;
        }
        if (cells(state.dataHeatBalHAMTMgr->IntConcell(sid)).rh > rhmax) {
            cells(state.dataHeatBalHAMTMgr->IntConcell(sid)).rh = rhmax;
        }

        // PDB August 2009 Start! Correction for when no vapour transfer coefficient have been defined.
        if (state.dataHeatBalHAMTMgr->extvtcflag(sid)) {
            cells(state.dataHeatBalHAMTMgr->ExtConcell(sid)).vtc = state.dataHeatBalHAMTMgr->extvtc(sid);
        } else {
            if (cells(state.dataHeatBalHAMTMgr->ExtConcell(sid)).rh > 0) {
                cells(state.dataHeatBalHAMTMgr->ExtConcell(sid)).vtc =
                    state.dataMstBal->HMassConvExtFD(sid) * RhoOut /
                    (PsyPsatFnTemp(state, state.dataMstBal->TempOutsideAirFD(sid)) * cells(state.dataHeatBalHAMTMgr->ExtConcell(sid)).rh);
            } else {
                cells(state.dataHeatBalHAMTMgr->ExtConcell(sid)).vtc = 10000.0;
            }
        }

        if (state.dataHeatBalHAMTMgr->intvtcflag(sid)) {
            cells(state.dataHeatBalHAMTMgr->IntConcell(sid)).vtc = state.dataHeatBalHAMTMgr->intvtc(sid);
            state.dataMstBal->HMassConvInFD(sid) = cells(state.dataHeatBalHAMTMgr->IntConcell(sid)).vtc * PsyPsatFnTemp(state, thisZoneHB.MAT) *
                                                   cells(state.dataHeatBalHAMTMgr->IntConcell(sid)).rh / RhoIn;
        } else {
            if (cells(state.dataHeatBalHAMTMgr->IntConcell(sid)).rh > 0) {
                cells(state.dataHeatBalHAMTMgr->IntConcell(sid)).vtc =
                    state.dataMstBal->HMassConvInFD(sid) * RhoIn /
                    (PsyPsatFnTemp(state, thisZoneHB.MAT) * cells(state.dataHeatBalHAMTMgr->IntConcell(sid)).rh);
            } else {
                cells(state.dataHeatBalHAMTMgr->IntConcell(sid)).vtc = 10000.0;
            }
        }
        // PDB August 2009 End

        // Initialise
        for (cid = state.dataHeatBalHAMTMgr->firstcell(sid); cid <= Extcell(sid) - 1; ++cid) {
            cells(cid).tempp1 = cells(cid).temp;
            cells(cid).tempp2 = cells(cid).temp;
            cells(cid).rhp1 = cells(cid).rh;
            cells(cid).rhp2 = cells(cid).rh;
        }
        for (cid = Intcell(sid) + 1; cid <= state.dataHeatBalHAMTMgr->lastcell(sid); ++cid) {
            cells(cid).tempp1 = cells(cid).temp;
            cells(cid).tempp2 = cells(cid).temp;
            cells(cid).rhp1 = cells(cid).rh;
            cells(cid).rhp2 = cells(cid).rh;
        }

        itter = 0;
        while (true) {
            ++itter;
            // Update Moisture values

            for (cid = state.dataHeatBalHAMTMgr->firstcell(sid); cid <= state.dataHeatBalHAMTMgr->lastcell(sid); ++cid) {
                matid = cells(cid).matid;
                cells(cid).vp = RHtoVP(state, cells(cid).rh, cells(cid).temp);
                cells(cid).vpp1 = RHtoVP(state, cells(cid).rhp1, cells(cid).tempp1);
                cells(cid).vpsat = PsyPsatFnTemp(state, cells(cid).tempp1);
                if (matid > 0) {
                    auto const *thisMaterial = dynamic_cast<const Material::MaterialChild *>(state.dataMaterial->Material(matid));
                    assert(thisMaterial != nullptr);
                    interp(thisMaterial->niso, thisMaterial->isorh, thisMaterial->isodata, cells(cid).rhp1, cells(cid).water, cells(cid).dwdphi);
                    if (state.dataEnvrn->IsRain && state.dataHeatBalHAMTMgr->rainswitch) {
                        interp(thisMaterial->nsuc, thisMaterial->sucwater, thisMaterial->sucdata, cells(cid).water, cells(cid).dw);
                    } else {
                        interp(thisMaterial->nred, thisMaterial->redwater, thisMaterial->reddata, cells(cid).water, cells(cid).dw);
                    }
                    interp(thisMaterial->nmu, thisMaterial->murh, thisMaterial->mudata, cells(cid).rhp1, cells(cid).mu);
                    interp(thisMaterial->ntc, thisMaterial->tcwater, thisMaterial->tcdata, cells(cid).water, cells(cid).wthermalc);
                }
            }

            // Calculate Heat and Vapor resistances,
            for (cid = Extcell(sid); cid <= Intcell(sid); ++cid) {
                torsum = 0.0;
                oorsum = 0.0;
                vpdiff = 0.0;
                for (ii = 1; ii <= adjmax; ++ii) {
                    adj = cells(cid).adjs(ii);
                    adjl = cells(cid).adjsl(ii);
                    if (adj == -1) break;

                    if (cells(cid).htc > 0) {
                        thermr1 = 1.0 / (cells(cid).overlap(ii) * cells(cid).htc);
                    } else if (cells(cid).matid > 0) {
                        thermr1 = cells(cid).dist(ii) / (cells(cid).overlap(ii) * cells(cid).wthermalc);
                    } else {
                        thermr1 = 0.0;
                    }

                    if (cells(cid).vtc > 0) {
                        vaporr1 = 1.0 / (cells(cid).overlap(ii) * cells(cid).vtc);
                    } else if (cells(cid).matid > 0) {
                        vaporr1 =
                            (cells(cid).dist(ii) * cells(cid).mu) / (cells(cid).overlap(ii) * WVDC(cells(cid).tempp1, state.dataEnvrn->OutBaroPress));
                    } else {
                        vaporr1 = 0.0;
                    }

                    if (cells(adj).htc > 0) {
                        thermr2 = 1.0 / (cells(cid).overlap(ii) * cells(adj).htc);
                    } else if (cells(adj).matid > 0) {
                        thermr2 = cells(adj).dist(adjl) / (cells(cid).overlap(ii) * cells(adj).wthermalc);
                    } else {
                        thermr2 = 0.0;
                    }

                    if (cells(adj).vtc > 0) {
                        vaporr2 = 1.0 / (cells(cid).overlap(ii) * cells(adj).vtc);
                    } else if (cells(adj).matid > 0) {
                        vaporr2 =
                            cells(adj).mu * cells(adj).dist(adjl) / (WVDC(cells(adj).tempp1, state.dataEnvrn->OutBaroPress) * cells(cid).overlap(ii));
                    } else {
                        vaporr2 = 0.0;
                    }

                    if (thermr1 + thermr2 > 0) {
                        oorsum += 1.0 / (thermr1 + thermr2);
                        torsum += cells(adj).tempp1 / (thermr1 + thermr2);
                    }
                    if (vaporr1 + vaporr2 > 0) {
                        vpdiff += (cells(adj).vp - cells(cid).vp) / (vaporr1 + vaporr2);
                    }
                }

                // Calculate Heat Capacitance
                tcap = ((cells(cid).density * cells(cid).spech + cells(cid).water * wspech) * cells(cid).volume);

                // calculate the latent heat if wanted and check for divergence
                qvp = 0.0;
                if ((cells(cid).matid > 0) && (state.dataHeatBalHAMTMgr->latswitch)) {
                    qvp = vpdiff * whv;
                }
                if (std::abs(qvp) > qvplim) {
                    if (!state.dataGlobal->WarmupFlag) {
                        ++state.dataHeatBalHAMTMgr->qvpErrCount;
                        if (state.dataHeatBalHAMTMgr->qvpErrCount < 16) {
                            ShowWarningError(
                                state, format("HeatAndMoistureTransfer: Large Latent Heat for Surface {}", state.dataSurface->Surface(sid).Name));
                        } else {
                            ShowRecurringWarningErrorAtEnd(
                                state, "HeatAndMoistureTransfer: Large Latent Heat Errors ", state.dataHeatBalHAMTMgr->qvpErrReport);
                        }
                    }
                    qvp = 0.0;
                }

                // Calculate the temperature for the next time step
                cells(cid).tempp1 = (torsum + qvp + cells(cid).Qadds + (tcap * cells(cid).temp / state.dataHeatBalHAMTMgr->deltat)) /
                                    (oorsum + (tcap / state.dataHeatBalHAMTMgr->deltat));
            }

            // Check for silly temperatures
            tempmax = maxval(cells, &subcell::tempp1);
            tempmin = minval(cells, &subcell::tempp1);
            if (tempmax > state.dataHeatBalSurf->MaxSurfaceTempLimit) {
                if (!state.dataGlobal->WarmupFlag) {
                    if (state.dataSurface->SurfHighTempErrCount(sid) == 0) {
                        ShowSevereMessage(
                            state,
                            format("HAMT: Temperature (high) out of bounds ({:.2R}) for surface={}", tempmax, state.dataSurface->Surface(sid).Name));
                        ShowContinueErrorTimeStamp(state, "");
                    }
                    ShowRecurringWarningErrorAtEnd(state,
                                                   "HAMT: Temperature Temperature (high) out of bounds; Surface=" +
                                                       state.dataSurface->Surface(sid).Name,
                                                   state.dataSurface->SurfHighTempErrCount(sid),
                                                   tempmax,
                                                   tempmax,
                                                   _,
                                                   "C",
                                                   "C");
                }
            }
            if (tempmax > state.dataHeatBalSurf->MaxSurfaceTempLimitBeforeFatal) {
                if (!state.dataGlobal->WarmupFlag) {
                    ShowSevereError(state,
                                    format("HAMT: HAMT: Temperature (high) out of bounds ( {:.2R}) for surface={}",
                                           tempmax,
                                           state.dataSurface->Surface(sid).Name));
                    ShowContinueErrorTimeStamp(state, "");
                    ShowFatalError(state, "Program terminates due to preceding condition.");
                }
            }
            if (tempmin < MinSurfaceTempLimit) {
                if (!state.dataGlobal->WarmupFlag) {
                    if (state.dataSurface->SurfHighTempErrCount(sid) == 0) {
                        ShowSevereMessage(
                            state,
                            format("HAMT: Temperature (low) out of bounds ({:.2R}) for surface={}", tempmin, state.dataSurface->Surface(sid).Name));
                        ShowContinueErrorTimeStamp(state, "");
                    }
                    ShowRecurringWarningErrorAtEnd(state,
                                                   "HAMT: Temperature Temperature (high) out of bounds; Surface=" +
                                                       state.dataSurface->Surface(sid).Name,
                                                   state.dataSurface->SurfHighTempErrCount(sid),
                                                   tempmin,
                                                   tempmin,
                                                   _,
                                                   "C",
                                                   "C");
                }
            }
            if (tempmin < MinSurfaceTempLimitBeforeFatal) {
                if (!state.dataGlobal->WarmupFlag) {
                    ShowSevereError(state,
                                    format("HAMT: HAMT: Temperature (low) out of bounds ( {:.2R}) for surface={}",
                                           tempmin,
                                           state.dataSurface->Surface(sid).Name));
                    ShowContinueErrorTimeStamp(state, "");
                    ShowFatalError(state, "Program terminates due to preceding condition.");
                }
            }

            // Calculate the liquid and vapor resisitances
            for (cid = Extcell(sid); cid <= Intcell(sid); ++cid) {
                phioosum = 0.0;
                phiorsum = 0.0;
                vpoosum = 0.0;
                vporsum = 0.0;

                for (ii = 1; ii <= adjmax; ++ii) {
                    adj = cells(cid).adjs(ii);
                    adjl = cells(cid).adjsl(ii);
                    if (adj == -1) break;

                    if (cells(cid).vtc > 0) {
                        vaporr1 = 1.0 / (cells(cid).overlap(ii) * cells(cid).vtc);
                    } else if (cells(cid).matid > 0) {
                        vaporr1 =
                            (cells(cid).dist(ii) * cells(cid).mu) / (cells(cid).overlap(ii) * WVDC(cells(cid).tempp1, state.dataEnvrn->OutBaroPress));
                    } else {
                        vaporr1 = 0.0;
                    }

                    if (cells(adj).vtc > 0) {
                        vaporr2 = 1.0 / (cells(cid).overlap(ii) * cells(adj).vtc);
                    } else if (cells(adj).matid > 0) {
                        vaporr2 = (cells(adj).dist(adjl) * cells(adj).mu) /
                                  (cells(cid).overlap(ii) * WVDC(cells(adj).tempp1, state.dataEnvrn->OutBaroPress));
                    } else {
                        vaporr2 = 0.0;
                    }
                    if (vaporr1 + vaporr2 > 0) {
                        vpoosum += 1.0 / (vaporr1 + vaporr2);
                        vporsum += (cells(adj).vpp1 / (vaporr1 + vaporr2));
                    }

                    if ((cells(cid).dw > 0) && (cells(cid).dwdphi > 0)) {
                        rhr1 = cells(cid).dist(ii) / (cells(cid).overlap(ii) * cells(cid).dw * cells(cid).dwdphi);
                    } else {
                        rhr1 = 0.0;
                    }
                    if ((cells(adj).dw > 0) && (cells(adj).dwdphi > 0)) {
                        rhr2 = cells(adj).dist(adjl) / (cells(cid).overlap(ii) * cells(adj).dw * cells(adj).dwdphi);
                    } else {
                        rhr2 = 0.0;
                    }

                    //             IF(rhr1+rhr2>0)THEN
                    if (rhr1 * rhr2 > 0) {
                        phioosum += 1.0 / (rhr1 + rhr2);
                        phiorsum += (cells(adj).rhp1 / (rhr1 + rhr2));
                    }
                }

                // Moisture Capacitance
                if (cells(cid).dwdphi > 0.0) {
                    wcap = cells(cid).dwdphi * cells(cid).volume;
                } else {
                    wcap = 0.0;
                }

                // Calculate the RH for the next time step
                denominator = (phioosum + vpoosum * cells(cid).vpsat + wcap / state.dataHeatBalHAMTMgr->deltat);
                if (denominator != 0.0) {
                    cells(cid).rhp1 = (phiorsum + vporsum + (wcap * cells(cid).rh) / state.dataHeatBalHAMTMgr->deltat) / denominator;
                } else {
                    ShowSevereError(state, "CalcHeatBalHAMT: demoninator in calculating RH is zero.  Check material properties for accuracy.");
                    ShowContinueError(state, format("...Problem occurs in Material=\"{}\".", state.dataMaterial->Material(cells(cid).matid)->Name));
                    ShowFatalError(state, "Program terminates due to preceding condition.");
                }

                if (cells(cid).rhp1 > rhmax) {
                    cells(cid).rhp1 = rhmax;
                }
            }

            // Check for convergence or too many itterations
            sumtp1 = 0.0;
            for (cid = Extcell(sid); cid <= Intcell(sid); ++cid) {
                if (sumtp1 < std::abs(cells(cid).tempp2 - cells(cid).tempp1)) {
                    sumtp1 = std::abs(cells(cid).tempp2 - cells(cid).tempp1);
                }
            }
            if (sumtp1 < convt) {
                break;
            }
            if (itter > ittermax) {
                break;
            }
            for (cid = state.dataHeatBalHAMTMgr->firstcell(sid); cid <= state.dataHeatBalHAMTMgr->lastcell(sid); ++cid) {
                cells(cid).tempp2 = cells(cid).tempp1;
                cells(cid).rhp2 = cells(cid).rhp1;
            }
        }

        // report back to CalcHeatBalanceInsideSurf
        TempSurfOutTmp = cells(Extcell(sid)).tempp1;
        SurfTempInTmp = cells(Intcell(sid)).tempp1;

        SurfTempInP = cells(Intcell(sid)).rhp1 * PsyPsatFnTemp(state, cells(Intcell(sid)).tempp1);

        state.dataMstBal->RhoVaporSurfIn(sid) = SurfTempInP / (461.52 * (thisZoneHB.MAT + DataGlobalConstants::KelvinConv));
    }

    void UpdateHeatBalHAMT(EnergyPlusData &state, int const sid)
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR         Phillip Biddulph
        //       DATE WRITTEN   June 2008
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // The zone heat balance equation has converged, so now the HAMT values are to be fixed
        // ready for the next itteration.
        // Fill all the report variables

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int cid;
        Real64 watermass;
        Real64 matmass;
        // unused1208    REAL(r64), SAVE :: InOld=0.0D0
        // unused1208    REAL(r64), SAVE :: OutOld=0.0D0

        // Update Temperatures and RHs. Calculate report variables
        matmass = 0.0;
        watermass = 0.0;
        for (cid = state.dataHeatBalHAMTMgr->firstcell(sid); cid <= state.dataHeatBalHAMTMgr->lastcell(sid); ++cid) {
            // fix HAMT values for this surface
            state.dataHeatBalHAMTMgr->cells(cid).temp = state.dataHeatBalHAMTMgr->cells(cid).tempp1;
            state.dataHeatBalHAMTMgr->cells(cid).rh = state.dataHeatBalHAMTMgr->cells(cid).rhp1;
            state.dataHeatBalHAMTMgr->cells(cid).rhp = state.dataHeatBalHAMTMgr->cells(cid).rh * 100.0;
            if (state.dataHeatBalHAMTMgr->cells(cid).density > 0.0) {
                state.dataHeatBalHAMTMgr->cells(cid).wreport =
                    state.dataHeatBalHAMTMgr->cells(cid).water / state.dataHeatBalHAMTMgr->cells(cid).density;
                watermass += (state.dataHeatBalHAMTMgr->cells(cid).water * state.dataHeatBalHAMTMgr->cells(cid).volume);
                matmass += (state.dataHeatBalHAMTMgr->cells(cid).density * state.dataHeatBalHAMTMgr->cells(cid).volume);
            }
        }

        state.dataHeatBalHAMTMgr->watertot(sid) = 0.0;
        if (matmass > 0) state.dataHeatBalHAMTMgr->watertot(sid) = watermass / matmass;

        state.dataHeatBalHAMTMgr->surfrh(sid) = 100.0 * state.dataHeatBalHAMTMgr->cells(state.dataHeatBalHAMTMgr->Intcell(sid)).rh;
        state.dataHeatBalHAMTMgr->surfextrh(sid) = 100.0 * state.dataHeatBalHAMTMgr->cells(state.dataHeatBalHAMTMgr->Extcell(sid)).rh;
        state.dataHeatBalHAMTMgr->surftemp(sid) = state.dataHeatBalHAMTMgr->cells(state.dataHeatBalHAMTMgr->Intcell(sid)).temp;
        state.dataHeatBalHAMTMgr->surfexttemp(sid) = state.dataHeatBalHAMTMgr->cells(state.dataHeatBalHAMTMgr->Extcell(sid)).temp;
        state.dataHeatBalHAMTMgr->surfvp(sid) = RHtoVP(state,
                                                       state.dataHeatBalHAMTMgr->cells(state.dataHeatBalHAMTMgr->Intcell(sid)).rh,
                                                       state.dataHeatBalHAMTMgr->cells(state.dataHeatBalHAMTMgr->Intcell(sid)).temp);
    }

    void interp(int const ndata,
                const Array1D<Real64> &xx,
                const Array1D<Real64> &yy,
                Real64 const invalue,
                Real64 &outvalue,
                ObjexxFCL::Optional<Real64> outgrad)
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR         Phillip Biddulph
        //       DATE WRITTEN   June 2008
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // To find a value by searching an array and interpolating between two coordinates
        // Also returns the gradient if required.

        // METHODOLOGY EMPLOYED:
        // Simple search

        // Argument array dimensioning
        EP_SIZE_CHECK(xx, ndata);
        EP_SIZE_CHECK(yy, ndata);

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        Real64 xxlow;
        Real64 xxhigh;
        Real64 yylow;
        Real64 yyhigh;
        Real64 mygrad;
        int step;

        mygrad = 0.0;
        outvalue = 0.0;

        if (ndata > 1) {
            xxlow = xx(1);
            yylow = yy(1);
            for (step = 2; step <= ndata; ++step) {
                xxhigh = xx(step);
                yyhigh = yy(step);
                if (invalue <= xxhigh) break;
                xxlow = xxhigh;
                yylow = yyhigh;
            }

            if (xxhigh > xxlow) {
                mygrad = (yyhigh - yylow) / (xxhigh - xxlow);
                outvalue = (invalue - xxlow) * mygrad + yylow;
                // PDB August 2009 bug fix
            } else if (std::abs(xxhigh - xxlow) < 0.0000000001) {
                outvalue = yylow;
            }
        }

        if (present(outgrad)) {
            // return gradient if required
            outgrad = mygrad;
        }
    }

    Real64 RHtoVP(EnergyPlusData &state, Real64 const RH, Real64 const Temperature)
    {
        // FUNCTION INFORMATION:
        //       AUTHOR         Phillip Biddulph
        //       DATE WRITTEN   June 2008
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS FUNCTION:
        // Convert Relative Humidity and Temperature to Vapor Pressure

        // Return value
        Real64 RHtoVP;

        // FUNCTION LOCAL VARIABLE DECLARATIONS:
        Real64 VPSat;

        VPSat = PsyPsatFnTemp(state, Temperature);

        RHtoVP = RH * VPSat;

        return RHtoVP;
    }

    Real64 WVDC(Real64 const Temperature, Real64 const ambp)
    {
        // FUNCTION INFORMATION:
        //       AUTHOR         Phillip Biddulph
        //       DATE WRITTEN   June 2008
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS FUNCTION:
        // To calculate the Water Vapor Diffusion Coefficient in air
        // using the temperature and ambient atmospheric pressor

        // REFERENCES:
        // K?zel, H.M. (1995) Simultaneous Heat and Moisture Transport in Building Components.
        // One- and two-dimensional calculation using simple parameters. IRB Verlag 1995

        // Return value
        Real64 WVDC;

        WVDC = (2.e-7 * std::pow(Temperature + DataGlobalConstants::KelvinConv, 0.81)) / ambp;

        return WVDC;
    }

    //                                 COPYRIGHT NOTICE

    //     Portions Copyright (c) University College London 2007.  All rights
    //     reserved.

    //     UCL LEGAL NOTICE
    //     Neither UCL, members of UCL nor any person or organisation acting on
    //     behalf of either:

    //     A. Makes any warranty of representation, express or implied with
    //        respect to the accuracy, completeness, or usefulness of the
    //        information contained in this program, including any warranty of
    //        merchantability or fitness of any purpose with respect to the
    //        program, or that the use of any information disclosed in this
    //        program may not infringe privately-owned rights, or

    //     B. Assumes any liability with respect to the use of, or for any and
    //        all damages resulting from the use of the program or any portion
    //        thereof or any information disclosed therein.

} // namespace HeatBalanceHAMTManager

} // namespace EnergyPlus
