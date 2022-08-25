// EnergyPlus, Copyright (c) 1996-2022, The Board of Trustees of the University of Illinois,
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

// EnergyPlus::DXCoils unit tests
// DX heating coil defrost capacity with electric resistance

// Google test headers
#include <gtest/gtest.h>

// EnergyPlus Headers
#include "Fixtures/EnergyPlusFixture.hh"
#include "Fixtures/SQLiteFixture.hh"
#include <EnergyPlus/CurveManager.hh>
#include <EnergyPlus/DXCoils.hh>
#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/DataAirLoop.hh>
#include <EnergyPlus/DataAirSystems.hh>
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataHeatBalance.hh>
#include <EnergyPlus/DataSizing.hh>
#include <EnergyPlus/Fans.hh>
#include <EnergyPlus/IOFiles.hh>
#include <EnergyPlus/NodeInputManager.hh>
#include <EnergyPlus/OutAirNodeManager.hh>
#include <EnergyPlus/OutputProcessor.hh>
#include <EnergyPlus/OutputReportPredefined.hh>
#include <EnergyPlus/Psychrometrics.hh>
#include <EnergyPlus/ScheduleManager.hh>

using namespace EnergyPlus;
using namespace DXCoils;
using namespace DataAirLoop;
using namespace DataAirSystems;
using namespace DataHVACGlobals;
using namespace DataSizing;
using namespace CurveManager;
using namespace OutputReportPredefined;
using namespace ScheduleManager;
using namespace DataEnvironment;

namespace EnergyPlus {

TEST_F(EnergyPlusFixture, DXCoils_Test1)
{
    using Psychrometrics::PsyRhFnTdbWPb;
    using Psychrometrics::PsyTdbFnHW;
    using Psychrometrics::PsyTsatFnHPb;
    using Psychrometrics::PsyWFnTdbH;
    int DXCoilNum;
    int CurveNum;

    state->dataDXCoils->NumDXCoils = 2;
    state->dataDXCoils->DXCoil.allocate(state->dataDXCoils->NumDXCoils);
    state->dataDXCoils->DXCoil(1).DXCoilType_Num = CoilDX_MultiSpeedCooling;
    state->dataDXCoils->DXCoil(1).DXCoilType = "Coil:Cooling:DX:MultiSpeed";
    state->dataDXCoils->DXCoil(2).DXCoilType_Num = CoilDX_MultiSpeedHeating;
    state->dataDXCoils->DXCoil(2).DXCoilType = "Coil:Heating:DX:MultiSpeed";
    state->dataDXCoils->DXCoil(1).MSRatedTotCap.allocate(2);
    state->dataDXCoils->DXCoil(2).MSRatedTotCap.allocate(2);
    state->dataDXCoils->DXCoil(2).CompanionUpstreamDXCoil = 1;

    state->dataDXCoils->DXCoilNumericFields.allocate(state->dataDXCoils->NumDXCoils);
    state->dataDXCoils->DXCoilNumericFields(2).PerfMode.allocate(1);
    state->dataDXCoils->DXCoilNumericFields(2).PerfMode(1).FieldNames.allocate(17);
    state->dataDXCoils->DXCoil(2).DefrostStrategy = StandardRatings::DefrostStrat::Resistive;
    state->dataDXCoils->DXCoil(2).DefrostCapacity = 5000.0;
    state->dataDXCoils->DXCoil(2).Name = "DX Heating coil";
    state->dataDXCoils->DXCoil(1).NumOfSpeeds = 2;
    state->dataDXCoils->DXCoil(2).NumOfSpeeds = 2;

    for (DXCoilNum = 1; DXCoilNum <= 2; ++DXCoilNum) {
        state->dataDXCoils->DXCoil(DXCoilNum).MSRatedTotCap.allocate(state->dataDXCoils->DXCoil(DXCoilNum).NumOfSpeeds);
        state->dataDXCoils->DXCoil(DXCoilNum).MSRatedSHR.allocate(state->dataDXCoils->DXCoil(DXCoilNum).NumOfSpeeds);
        state->dataDXCoils->DXCoil(DXCoilNum).MSRatedCOP.allocate(state->dataDXCoils->DXCoil(DXCoilNum).NumOfSpeeds);
        state->dataDXCoils->DXCoil(DXCoilNum).MSRatedAirVolFlowRate.allocate(state->dataDXCoils->DXCoil(DXCoilNum).NumOfSpeeds);
        state->dataDXCoils->DXCoil(DXCoilNum).MSRatedAirMassFlowRate.allocate(state->dataDXCoils->DXCoil(DXCoilNum).NumOfSpeeds);
        state->dataDXCoils->DXCoil(DXCoilNum).MSCCapFTemp.allocate(state->dataDXCoils->DXCoil(DXCoilNum).NumOfSpeeds);
        state->dataDXCoils->DXCoil(DXCoilNum).MSCCapFFlow.allocate(state->dataDXCoils->DXCoil(DXCoilNum).NumOfSpeeds);
        state->dataDXCoils->DXCoil(DXCoilNum).MSEIRFTemp.allocate(state->dataDXCoils->DXCoil(DXCoilNum).NumOfSpeeds);
        state->dataDXCoils->DXCoil(DXCoilNum).MSEIRFFlow.allocate(state->dataDXCoils->DXCoil(DXCoilNum).NumOfSpeeds);
        state->dataDXCoils->DXCoil(DXCoilNum).MSWasteHeat.allocate(state->dataDXCoils->DXCoil(DXCoilNum).NumOfSpeeds);
        state->dataDXCoils->DXCoil(DXCoilNum).MSEvapCondEffect.allocate(state->dataDXCoils->DXCoil(DXCoilNum).NumOfSpeeds);
        state->dataDXCoils->DXCoil(DXCoilNum).MSEvapCondAirFlow.allocate(state->dataDXCoils->DXCoil(DXCoilNum).NumOfSpeeds);
        state->dataDXCoils->DXCoil(DXCoilNum).MSEvapCondPumpElecNomPower.allocate(state->dataDXCoils->DXCoil(DXCoilNum).NumOfSpeeds);
        state->dataDXCoils->DXCoil(DXCoilNum).MSRatedCBF.allocate(state->dataDXCoils->DXCoil(DXCoilNum).NumOfSpeeds);
        state->dataDXCoils->DXCoil(DXCoilNum).MSWasteHeatFrac.allocate(state->dataDXCoils->DXCoil(DXCoilNum).NumOfSpeeds);
        state->dataDXCoils->DXCoil(DXCoilNum).MSPLFFPLR.allocate(state->dataDXCoils->DXCoil(DXCoilNum).NumOfSpeeds);
        state->dataDXCoils->DXCoil(DXCoilNum).MSTwet_Rated.allocate(state->dataDXCoils->DXCoil(DXCoilNum).NumOfSpeeds);
        state->dataDXCoils->DXCoil(DXCoilNum).MSGamma_Rated.allocate(state->dataDXCoils->DXCoil(DXCoilNum).NumOfSpeeds);
        state->dataDXCoils->DXCoil(DXCoilNum).MSMaxONOFFCyclesperHour.allocate(state->dataDXCoils->DXCoil(DXCoilNum).NumOfSpeeds);
        state->dataDXCoils->DXCoil(DXCoilNum).MSLatentCapacityTimeConstant.allocate(state->dataDXCoils->DXCoil(DXCoilNum).NumOfSpeeds);
        state->dataDXCoils->DXCoil(DXCoilNum).MSFanPowerPerEvapAirFlowRate.allocate(state->dataDXCoils->DXCoil(DXCoilNum).NumOfSpeeds);
        state->dataDXCoils->DXCoil(DXCoilNum).MSFanPowerPerEvapAirFlowRate_2023.allocate(state->dataDXCoils->DXCoil(DXCoilNum).NumOfSpeeds);
    }

    state->dataDXCoils->DXCoil(1).MSRatedTotCap(1) = 4455.507579219055;
    state->dataDXCoils->DXCoil(1).MSRatedTotCap(2) = 6188.507579219055;
    state->dataDXCoils->DXCoil(1).MSCCapFFlow = 1;
    state->dataDXCoils->DXCoil(1).MSCCapFTemp = 3;
    state->dataDXCoils->DXCoil(1).MSEIRFFlow = 1;
    state->dataDXCoils->DXCoil(1).MSEIRFTemp = 3;
    state->dataDXCoils->DXCoil(1).MSPLFFPLR = 2;

    DXCoilNum = 2;
    state->dataDXCoils->DXCoil(DXCoilNum).MSRatedTotCap(1) = 4455.507579219055;
    state->dataDXCoils->DXCoil(DXCoilNum).MSRatedTotCap(2) = 6188.204971137576;
    state->dataDXCoils->DXCoil(DXCoilNum).MSRatedCOP(1) = 4.03;
    state->dataDXCoils->DXCoil(DXCoilNum).MSRatedCOP(2) = 3.53;

    state->dataDXCoils->DXCoil(DXCoilNum).MSCCapFFlow = 1;
    state->dataDXCoils->DXCoil(DXCoilNum).MSCCapFTemp = 3;
    state->dataDXCoils->DXCoil(DXCoilNum).MSEIRFFlow = 1;
    state->dataDXCoils->DXCoil(DXCoilNum).MSEIRFTemp = 3;
    state->dataDXCoils->DXCoil(DXCoilNum).MSPLFFPLR = 2;
    state->dataDXCoils->DXCoil(DXCoilNum).MSRatedAirVolFlowRate(1) = 0.2339;
    state->dataDXCoils->DXCoil(DXCoilNum).MSRatedAirVolFlowRate(2) = 0.2924;
    state->dataDXCoils->DXCoil(DXCoilNum).MSFanPowerPerEvapAirFlowRate = 0.0;
    state->dataDXCoils->DXCoil(DXCoilNum).MSFanPowerPerEvapAirFlowRate_2023 = 0.0;
    state->dataDXCoils->DXCoil(DXCoilNum).RegionNum = 4;
    state->dataDXCoils->DXCoil(DXCoilNum).MinOATCompressor = -17.78;

    state->dataCurveManager->NumCurves = 3;
    state->dataCurveManager->PerfCurve.allocate(state->dataCurveManager->NumCurves);

    CurveNum = 1;
    state->dataCurveManager->PerfCurve(CurveNum).curveType = CurveType::Quadratic;
    state->dataCurveManager->PerfCurve(CurveNum).ObjectType = "Curve:Quadratic";
    state->dataCurveManager->PerfCurve(CurveNum).InterpolationType = InterpType::EvaluateCurveToLimits;
    state->dataCurveManager->PerfCurve(CurveNum).Coeff1 = 1;
    state->dataCurveManager->PerfCurve(CurveNum).Coeff2 = 0.0;
    state->dataCurveManager->PerfCurve(CurveNum).Coeff3 = 0.0;
    state->dataCurveManager->PerfCurve(CurveNum).Coeff4 = 0.0;
    state->dataCurveManager->PerfCurve(CurveNum).Coeff5 = 0.0;
    state->dataCurveManager->PerfCurve(CurveNum).Coeff6 = 0.0;
    state->dataCurveManager->PerfCurve(CurveNum).Var1Min = 0.0;
    state->dataCurveManager->PerfCurve(CurveNum).Var1Max = 2.0;
    state->dataCurveManager->PerfCurve(CurveNum).Var2Min = 0.0;
    state->dataCurveManager->PerfCurve(CurveNum).Var2Max = 2.0;

    CurveNum = 2;
    state->dataCurveManager->PerfCurve(CurveNum).curveType = CurveType::Quadratic;
    state->dataCurveManager->PerfCurve(CurveNum).ObjectType = "Curve:Quadratic";
    state->dataCurveManager->PerfCurve(CurveNum).InterpolationType = InterpType::EvaluateCurveToLimits;
    state->dataCurveManager->PerfCurve(CurveNum).Coeff1 = 1;
    state->dataCurveManager->PerfCurve(CurveNum).Coeff2 = 0.0;
    state->dataCurveManager->PerfCurve(CurveNum).Coeff3 = 0.0;
    state->dataCurveManager->PerfCurve(CurveNum).Coeff4 = 0.0;
    state->dataCurveManager->PerfCurve(CurveNum).Coeff5 = 0.0;
    state->dataCurveManager->PerfCurve(CurveNum).Coeff6 = 0.0;
    state->dataCurveManager->PerfCurve(CurveNum).Var1Min = 0.0;
    state->dataCurveManager->PerfCurve(CurveNum).Var1Max = 1.0;
    state->dataCurveManager->PerfCurve(CurveNum).Var2Min = 0.7;
    state->dataCurveManager->PerfCurve(CurveNum).Var2Max = 1.0;

    CurveNum = 3;
    state->dataCurveManager->PerfCurve(CurveNum).curveType = CurveType::BiQuadratic;
    state->dataCurveManager->PerfCurve(CurveNum).ObjectType = "Curve:Biquadratic";
    state->dataCurveManager->PerfCurve(CurveNum).InterpolationType = InterpType::EvaluateCurveToLimits;
    state->dataCurveManager->PerfCurve(CurveNum).Coeff1 = 1;
    state->dataCurveManager->PerfCurve(CurveNum).Coeff2 = 0.0;
    state->dataCurveManager->PerfCurve(CurveNum).Coeff3 = 0.0;
    state->dataCurveManager->PerfCurve(CurveNum).Coeff4 = 0.0;
    state->dataCurveManager->PerfCurve(CurveNum).Coeff5 = 0.0;
    state->dataCurveManager->PerfCurve(CurveNum).Coeff6 = 0.0;
    state->dataCurveManager->PerfCurve(CurveNum).Var1Min = -100.0;
    state->dataCurveManager->PerfCurve(CurveNum).Var1Max = 100.0;
    state->dataCurveManager->PerfCurve(CurveNum).Var2Min = -100.0;
    state->dataCurveManager->PerfCurve(CurveNum).Var2Max = 100.0;

    SetPredefinedTables(*state);
    SizeDXCoil(*state, 2);
    EXPECT_DOUBLE_EQ(5000.0, state->dataDXCoils->DXCoil(2).DefrostCapacity);

    EXPECT_TRUE(has_eio_output());

    // fails on windows due to endline issue... this outputs /r/n on Windows but it is outputting /n on Windows for some reason...
    // EXPECT_TRUE( compare_eio_stream( delimited_string( {
    // 	"! <DX Heating Coil Standard Rating Information>, Component Type, Component Name, High Temperature Heating (net) Rating Capacity {W}, Low
    // Temperature Heating (net) Rating Capacity {W}, HSPF {Btu/W-h}, Region Number", 	" DX Heating Coil Standard Rating Information, , DX Heating
    // coil, 6414.3, 6414.3, 6.58, 4" } ) ) );

    // set up coil operating conditions (replicates first occurrence of RH > 1 warning in HVACTemplate_UnitarySytsem annual run)
    state->dataEnvrn->OutDryBulbTemp = 16.1;
    state->dataEnvrn->OutHumRat = 0.0114507065;
    state->dataEnvrn->OutBaroPress = 98200.0;
    state->dataEnvrn->StdRhoAir = 1.2;
    state->dataHVACGlobal->MSHPMassFlowRateLow = 0.2339 * state->dataEnvrn->StdRhoAir;
    state->dataHVACGlobal->MSHPMassFlowRateHigh = 0.2924 * state->dataEnvrn->StdRhoAir;

    int CoilIndex = 1;
    state->dataHeatBal->HeatReclaimDXCoil.allocate(2);
    state->dataDXCoils->DXCoil(CoilIndex).InletAirMassFlowRate = state->dataHVACGlobal->MSHPMassFlowRateLow;
    state->dataDXCoils->DXCoil(CoilIndex).MSRatedAirMassFlowRate(1) = state->dataHVACGlobal->MSHPMassFlowRateLow;
    state->dataDXCoils->DXCoil(CoilIndex).MSRatedAirMassFlowRate(2) = state->dataHVACGlobal->MSHPMassFlowRateHigh;
    state->dataDXCoils->DXCoil(CoilIndex).InletAirTemp = 16.1; // 97% RH - inlet is right up against the saturation curve
    state->dataDXCoils->DXCoil(CoilIndex).InletAirEnthalpy = 45158.16;
    state->dataDXCoils->DXCoil(CoilIndex).InletAirHumRat = 0.01145065;
    state->dataDXCoils->DXCoil(CoilIndex).MSRatedCBF(1) = 0.0107723;
    state->dataDXCoils->DXCoil(CoilIndex).MSRatedCBF(2) = 0.0107723;
    state->dataDXCoils->DXCoil(CoilIndex).MSWasteHeat(1) = 0;
    state->dataDXCoils->DXCoil(CoilIndex).MSWasteHeat(2) = 0;
    state->dataDXCoils->DXCoil(CoilIndex).MSWasteHeatFrac(1) = 0;
    state->dataDXCoils->DXCoil(CoilIndex).MSWasteHeatFrac(2) = 0;
    state->dataDXCoils->DXCoil(CoilIndex).SchedPtr = 1;
    state->dataScheduleMgr->Schedule.allocate(1);
    state->dataScheduleMgr->Schedule(1).CurrentValue = 1.0;
    state->dataDXCoils->DXCoilOutletTemp.allocate(1);
    state->dataDXCoils->DXCoilOutletHumRat.allocate(1);
    state->dataDXCoils->DXCoilPartLoadRatio.allocate(1);
    state->dataDXCoils->DXCoilFanOpMode.allocate(1);

    state->dataLoopNodes->Node.allocate(1);
    state->dataDXCoils->DXCoil(CoilIndex).AirOutNode = 1;

    Real64 SpeedRatio = 0.0;
    Real64 CycRatio = 1.0;
    int SpeedNum = 2;
    int FanOpMode = 1;
    DataHVACGlobals::CompressorOperation CompressorOp = DataHVACGlobals::CompressorOperation::On;
    int SingleMode = 0;
    CalcMultiSpeedDXCoilCooling(*state, CoilIndex, SpeedRatio, CycRatio, SpeedNum, FanOpMode, CompressorOp, SingleMode);

    Real64 TdbAtOutlet = PsyTdbFnHW(state->dataDXCoils->DXCoil(CoilIndex).OutletAirEnthalpy, state->dataDXCoils->DXCoil(CoilIndex).OutletAirHumRat);
    Real64 tSatAtOutlet = PsyTsatFnHPb(*state, state->dataDXCoils->DXCoil(CoilIndex).OutletAirEnthalpy, state->dataEnvrn->OutBaroPress);
    Real64 rhAtOutlet = PsyRhFnTdbWPb(*state,
                                      state->dataDXCoils->DXCoil(CoilIndex).OutletAirTemp,
                                      state->dataDXCoils->DXCoil(CoilIndex).OutletAirHumRat,
                                      state->dataEnvrn->OutBaroPress);

    // air outlet condition is right next to the saturation curve
    EXPECT_DOUBLE_EQ(TdbAtOutlet, tSatAtOutlet); // Tdb higher than TSat by 1.8E-15 C
    EXPECT_GT(TdbAtOutlet, tSatAtOutlet);        // Tdb higher than TSat by 1.8E-15 C
    EXPECT_NEAR(1.0, rhAtOutlet, 0.00001);       // 99.9995% RH (i.e., it's not 100% as PsyRhFnTdbWPb would have reported previously)
    EXPECT_LT(rhAtOutlet, 1.0);                  // just to the right of saturation curve

    // TODO: FIXME: This now outputs a warning...?
    // EXPECT_FALSE( has_cerr_output() ); // old warning no longer reported
}

TEST_F(EnergyPlusFixture, DXCoils_Test2)
{
    int DXCoilNum;
    int CurveNum;

    state->dataGlobal->DisplayExtraWarnings = true;
    state->dataSize->SysSizingRunDone = true;
    state->dataSize->FinalSysSizing.allocate(1);
    state->dataAirSystemsData->PrimaryAirSystems.allocate(1);
    state->dataAirLoop->AirLoopControlInfo.allocate(1);
    state->dataSize->CurSysNum = 1;
    state->dataDXCoils->NumDXCoils = 2;
    DXCoilNum = 2;
    state->dataSize->UnitarySysEqSizing.allocate(1);
    state->dataDXCoils->DXCoil.allocate(state->dataDXCoils->NumDXCoils);
    state->dataDXCoils->DXCoil(1).DXCoilType_Num = CoilDX_CoolingSingleSpeed;
    state->dataDXCoils->DXCoil(2).DXCoilType_Num = CoilDX_HeatingEmpirical;
    state->dataDXCoils->DXCoil(DXCoilNum).DXCoilType = "Coil:Heating:DX:SingleSpeed";
    state->dataDXCoils->DXCoil(2).CompanionUpstreamDXCoil = 1;

    state->dataDXCoils->DXCoilNumericFields.allocate(state->dataDXCoils->NumDXCoils);
    state->dataDXCoils->DXCoilNumericFields(2).PerfMode.allocate(1);
    state->dataDXCoils->DXCoilNumericFields(2).PerfMode(1).FieldNames.allocate(20);
    state->dataDXCoils->DXCoil(2).DefrostStrategy = StandardRatings::DefrostStrat::Resistive;
    state->dataDXCoils->DXCoil(2).DefrostCapacity = 5000.0;
    state->dataDXCoils->DXCoil(2).Name = "DX Heating coil";

    state->dataDXCoils->DXCoil(1).RatedTotCap(1) = AutoSize;
    state->dataDXCoils->DXCoil(1).RatedTotCap(2) = AutoSize;
    state->dataDXCoils->DXCoil(2).RatedTotCap(1) = AutoSize;
    state->dataDXCoils->DXCoil(DXCoilNum).RegionNum = 4;
    state->dataDXCoils->DXCoil(DXCoilNum).MinOATCompressor = -17.78;
    state->dataDXCoils->DXCoil(DXCoilNum).CCapFFlow(1) = 1;
    state->dataDXCoils->DXCoil(DXCoilNum).CCapFTemp(1) = 1;
    state->dataDXCoils->DXCoil(DXCoilNum).EIRFFlow(1) = 1;
    state->dataDXCoils->DXCoil(DXCoilNum).EIRFTemp(1) = 1;
    state->dataDXCoils->DXCoil(DXCoilNum).PLFFPLR(1) = 1;
    state->dataCurveManager->NumCurves = 3;
    state->dataCurveManager->PerfCurve.allocate(state->dataCurveManager->NumCurves);

    CurveNum = 1;
    state->dataCurveManager->PerfCurve(CurveNum).curveType = CurveType::Quadratic;
    state->dataCurveManager->PerfCurve(CurveNum).ObjectType = "Curve:Quadratic";
    state->dataCurveManager->PerfCurve(CurveNum).InterpolationType = InterpType::EvaluateCurveToLimits;
    state->dataCurveManager->PerfCurve(CurveNum).Coeff1 = 1;
    state->dataCurveManager->PerfCurve(CurveNum).Coeff2 = 0.0;
    state->dataCurveManager->PerfCurve(CurveNum).Coeff3 = 0.0;
    state->dataCurveManager->PerfCurve(CurveNum).Coeff4 = 0.0;
    state->dataCurveManager->PerfCurve(CurveNum).Coeff5 = 0.0;
    state->dataCurveManager->PerfCurve(CurveNum).Coeff6 = 0.0;
    state->dataCurveManager->PerfCurve(CurveNum).Var1Min = 0.0;
    state->dataCurveManager->PerfCurve(CurveNum).Var1Max = 2.0;
    state->dataCurveManager->PerfCurve(CurveNum).Var2Min = 0.0;
    state->dataCurveManager->PerfCurve(CurveNum).Var2Max = 2.0;

    CurveNum = 2;
    state->dataCurveManager->PerfCurve(CurveNum).curveType = CurveType::Quadratic;
    state->dataCurveManager->PerfCurve(CurveNum).ObjectType = "Curve:Quadratic";
    state->dataCurveManager->PerfCurve(CurveNum).InterpolationType = InterpType::EvaluateCurveToLimits;
    state->dataCurveManager->PerfCurve(CurveNum).Coeff1 = 1;
    state->dataCurveManager->PerfCurve(CurveNum).Coeff2 = 0.0;
    state->dataCurveManager->PerfCurve(CurveNum).Coeff3 = 0.0;
    state->dataCurveManager->PerfCurve(CurveNum).Coeff4 = 0.0;
    state->dataCurveManager->PerfCurve(CurveNum).Coeff5 = 0.0;
    state->dataCurveManager->PerfCurve(CurveNum).Coeff6 = 0.0;
    state->dataCurveManager->PerfCurve(CurveNum).Var1Min = 0.0;
    state->dataCurveManager->PerfCurve(CurveNum).Var1Max = 1.0;
    state->dataCurveManager->PerfCurve(CurveNum).Var2Min = 0.7;
    state->dataCurveManager->PerfCurve(CurveNum).Var2Max = 1.0;

    CurveNum = 3;
    state->dataCurveManager->PerfCurve(CurveNum).curveType = CurveType::BiQuadratic;
    state->dataCurveManager->PerfCurve(CurveNum).ObjectType = "Curve:Biquadratic";
    state->dataCurveManager->PerfCurve(CurveNum).InterpolationType = InterpType::EvaluateCurveToLimits;
    state->dataCurveManager->PerfCurve(CurveNum).Coeff1 = 1;
    state->dataCurveManager->PerfCurve(CurveNum).Coeff2 = 0.0;
    state->dataCurveManager->PerfCurve(CurveNum).Coeff3 = 0.0;
    state->dataCurveManager->PerfCurve(CurveNum).Coeff4 = 0.0;
    state->dataCurveManager->PerfCurve(CurveNum).Coeff5 = 0.0;
    state->dataCurveManager->PerfCurve(CurveNum).Coeff6 = 0.0;
    state->dataCurveManager->PerfCurve(CurveNum).Var1Min = -100.0;
    state->dataCurveManager->PerfCurve(CurveNum).Var1Max = 100.0;
    state->dataCurveManager->PerfCurve(CurveNum).Var2Min = -100.0;
    state->dataCurveManager->PerfCurve(CurveNum).Var2Max = 100.0;

    SetPredefinedTables(*state);
    SizeDXCoil(*state, 2);
    EXPECT_DOUBLE_EQ(0.0, state->dataDXCoils->DXCoil(2).RatedTotCap(1));

    EXPECT_TRUE(has_eio_output());

    // EXPECT_TRUE( compare_eio_stream( delimited_string( {
    // 	"! <Component Sizing Information>, Component Type, Component Name, Input Field Description, Value",
    // 	" Component Sizing Information, Coil:Heating:DX:SingleSpeed, DX Heating coil, Design Size  [W], 0.00000",
    // 	" Component Sizing Information, Coil:Heating:DX:SingleSpeed, DX Heating coil, User-Specified  [W], 5000.00000",
    // 	" DX Heating Coil Standard Rating Information, Coil:Heating:DX:SingleSpeed, DX Heating coil, 0.0, 0.0, 3.51, 4"} ) ) );

    // Output from CI, I don't know why it is different than above...

    // "! , Component Type, Component Name, Input Field Description, Value",
    // " Component Sizing Information, Coil:Heating:DX:SingleSpeed, DX Heating coil, Design Size [W], 0.00000",
    // " Component Sizing Information, Coil:Heating:DX:SingleSpeed, DX Heating coil, User-Specified [W], 5000.00000",
    // "! , Component Type, Component Name, High Temperature Heating (net) Rating Capacity {W}, Low Temperature Heating (net) Rating Capacity {W},
    // HSPF {Btu/W-h}, Region Number", " DX Heating Coil Standard Rating Information, Coil:Heating:DX:SingleSpeed, DX Heating coil, 0.0, 0.0, 3.51, 4"

    // Clean up
    state->dataSize->UnitarySysEqSizing.deallocate();
    state->dataSize->FinalSysSizing.deallocate();
    state->dataAirSystemsData->PrimaryAirSystems.deallocate();
    state->dataAirLoop->AirLoopControlInfo.deallocate();
}

TEST_F(EnergyPlusFixture, TestMultiSpeedDefrostCOP)
{
    // Test that the COP calculation is correct when the defrost is on. #4973

    using DXCoils::CalcMultiSpeedDXCoilHeating;
    using Psychrometrics::PsyHFnTdbW;
    using Psychrometrics::PsyRhoAirFnPbTdbW;
    int DXCoilNum;

    // Set up heating coil and curves.

    state->dataDXCoils->NumDXCoils = 1;
    DXCoilNum = 1;
    state->dataDXCoils->DXCoil.allocate(state->dataDXCoils->NumDXCoils);
    DXCoilData &Coil = state->dataDXCoils->DXCoil(DXCoilNum);

    Coil.DXCoilType = "Coil:Heating:DX:MultiSpeed";
    Coil.DXCoilType_Num = CoilDX_MultiSpeedHeating;
    Coil.SchedPtr = DataGlobalConstants::ScheduleAlwaysOn;

    state->dataDXCoils->DXCoilNumericFields.allocate(state->dataDXCoils->NumDXCoils);
    state->dataHeatBal->HeatReclaimDXCoil.allocate(state->dataDXCoils->NumDXCoils);
    state->dataDXCoils->DXCoilOutletTemp.allocate(state->dataDXCoils->NumDXCoils);
    state->dataDXCoils->DXCoilOutletHumRat.allocate(state->dataDXCoils->NumDXCoils);
    state->dataDXCoils->DXCoilFanOpMode.allocate(state->dataDXCoils->NumDXCoils);
    state->dataDXCoils->DXCoilPartLoadRatio.allocate(state->dataDXCoils->NumDXCoils);
    state->dataDXCoils->DXCoilNumericFields(DXCoilNum).PerfMode.allocate(1);
    state->dataDXCoils->DXCoilNumericFields(DXCoilNum).PerfMode(1).FieldNames.allocate(15);
    Coil.DefrostStrategy = StandardRatings::DefrostStrat::Resistive;
    Coil.Name = "DX Heating coil";
    Coil.NumOfSpeeds = 2;
    state->dataLoopNodes->Node.allocate(1);
    Coil.AirOutNode = 1;

    Coil.MSRatedTotCap.allocate(Coil.NumOfSpeeds);
    Coil.MSRatedSHR.allocate(Coil.NumOfSpeeds);
    Coil.MSRatedCOP.allocate(Coil.NumOfSpeeds);
    Coil.MSRatedAirVolFlowRate.allocate(Coil.NumOfSpeeds);
    Coil.MSRatedAirMassFlowRate.allocate(Coil.NumOfSpeeds);
    Coil.MSCCapFTemp.allocate(Coil.NumOfSpeeds);
    Coil.MSCCapFFlow.allocate(Coil.NumOfSpeeds);
    Coil.MSEIRFTemp.allocate(Coil.NumOfSpeeds);
    Coil.MSEIRFFlow.allocate(Coil.NumOfSpeeds);
    Coil.MSWasteHeat.allocate(Coil.NumOfSpeeds);
    Coil.MSEvapCondEffect.allocate(Coil.NumOfSpeeds);
    Coil.MSEvapCondAirFlow.allocate(Coil.NumOfSpeeds);
    Coil.MSEvapCondPumpElecNomPower.allocate(Coil.NumOfSpeeds);
    Coil.MSRatedCBF.allocate(Coil.NumOfSpeeds);
    Coil.MSWasteHeatFrac.allocate(Coil.NumOfSpeeds);
    Coil.MSPLFFPLR.allocate(Coil.NumOfSpeeds);
    Coil.MSTwet_Rated.allocate(Coil.NumOfSpeeds);
    Coil.MSGamma_Rated.allocate(Coil.NumOfSpeeds);
    Coil.MSMaxONOFFCyclesperHour.allocate(Coil.NumOfSpeeds);
    Coil.MSLatentCapacityTimeConstant.allocate(Coil.NumOfSpeeds);
    Coil.MSFanPowerPerEvapAirFlowRate.allocate(Coil.NumOfSpeeds);

    Coil.MinOATCompressor = -73.27777777777779;
    Coil.CrankcaseHeaterCapacity = 0.0;
    Coil.MaxOATDefrost = 0.0;
    Coil.DefrostStrategy = StandardRatings::DefrostStrat::Resistive;
    Coil.DefrostControl = StandardRatings::HPdefrostControl::Timed;
    Coil.DefrostTime = 0.058333;
    Coil.DefrostCapacity = 1000;
    Coil.PLRImpact = false;
    Coil.FuelType = "Electricity";
    Coil.FuelTypeNum = DataGlobalConstants::ResourceType::Electricity;
    Coil.RegionNum = 4;
    Coil.MSRatedTotCap(1) = 2202.5268975202675;
    Coil.MSRatedCOP(1) = 4.200635910578916;
    Coil.MSRatedAirVolFlowRate(1) = 0.087746133503702;
    Coil.MSFanPowerPerEvapAirFlowRate(1) = 773.3;
    Coil.MSWasteHeatFrac(1) = 0.2;
    Coil.MSRatedTotCap(2) = 11012.634487601337;
    Coil.MSRatedCOP(2) = 4.200635910578916;
    Coil.MSRatedAirVolFlowRate(2) = 0.43873066751851;
    Coil.MSFanPowerPerEvapAirFlowRate(2) = 773.3;
    Coil.MSWasteHeatFrac(2) = 0.2;
    Coil.RatedSHR(1) = 1.0;

    for (int mode = 1; mode <= Coil.NumOfSpeeds; ++mode) {
        Coil.MSRatedAirMassFlowRate(mode) =
            Coil.MSRatedAirVolFlowRate(mode) * PsyRhoAirFnPbTdbW(*state, state->dataEnvrn->StdBaroPress, 21.11, 0.00881, "InitDXCoil");
    }

    state->dataCurveManager->NumCurves = 11;
    state->dataCurveManager->PerfCurve.allocate(state->dataCurveManager->NumCurves);

    PerformanceCurveData *pCurve;

    int constexpr nCapfT1 = 1;
    pCurve = &state->dataCurveManager->PerfCurve(nCapfT1);
    pCurve->curveType = CurveType::BiQuadratic;
    pCurve->Name = "HP_Heat-Cap-fT1";
    pCurve->Coeff1 = 0.95624428;
    pCurve->Coeff2 = 0;
    pCurve->Coeff3 = 0;
    pCurve->Coeff4 = 0.005999544;
    pCurve->Coeff5 = -0.0000900072;
    pCurve->Coeff6 = 0;
    pCurve->Var1Min = -100;
    pCurve->Var1Max = 100;
    pCurve->Var2Min = -100;
    pCurve->Var2Max = 100;

    Coil.MSCCapFTemp(1) = nCapfT1;

    int constexpr nCapfFF1 = 2;
    pCurve = &state->dataCurveManager->PerfCurve(nCapfFF1);
    pCurve->curveType = CurveType::Quadratic;
    pCurve->Name = "HP_Heat-Cap-fFF1";
    pCurve->Coeff1 = 1;
    pCurve->Coeff2 = 0;
    pCurve->Coeff3 = 0;
    pCurve->Var1Min = 0;
    pCurve->Var1Max = 2;
    pCurve->CurveMin = 0;
    pCurve->CurveMax = 2;

    Coil.MSCCapFFlow(1) = nCapfFF1;

    int constexpr nEIRfT1 = 3;
    pCurve = &state->dataCurveManager->PerfCurve(nEIRfT1);
    pCurve->curveType = CurveType::BiQuadratic;
    pCurve->Name = "HP_Heat-EIR-fT1";
    pCurve->Coeff1 = 1.065476178;
    pCurve->Coeff2 = 0;
    pCurve->Coeff3 = 0;
    pCurve->Coeff4 = -0.0085714308;
    pCurve->Coeff5 = 0.0000857142;
    pCurve->Coeff6 = 0;
    pCurve->Var1Min = -100;
    pCurve->Var1Max = 100;
    pCurve->Var2Min = -100;
    pCurve->Var2Max = 100;

    Coil.MSEIRFTemp(1) = nEIRfT1;

    int constexpr nEIRfFF1 = 4;
    pCurve = &state->dataCurveManager->PerfCurve(nEIRfFF1);
    pCurve->curveType = CurveType::Quadratic;
    pCurve->Name = "HP_Heat-EIR-fFF1";
    pCurve->Coeff1 = 1;
    pCurve->Coeff2 = 0;
    pCurve->Coeff3 = 0;
    pCurve->Var1Min = 0;
    pCurve->Var1Max = 2;
    pCurve->CurveMin = 0;
    pCurve->CurveMax = 2;

    Coil.MSEIRFFlow(1) = nEIRfFF1;

    int constexpr nPLFfPLR1 = 5;
    pCurve = &state->dataCurveManager->PerfCurve(nPLFfPLR1);
    pCurve->curveType = CurveType::Quadratic;
    pCurve->Name = "HP_Heat-PLF-fPLR1";
    pCurve->Coeff1 = 1;
    pCurve->Coeff2 = 0;
    pCurve->Coeff3 = 0;
    pCurve->Var1Min = 0;
    pCurve->Var1Max = 1;
    pCurve->CurveMin = 0.7;
    pCurve->CurveMax = 1;

    Coil.MSPLFFPLR(1) = nPLFfPLR1;

    int constexpr nConstantBiquadratic = 6;
    pCurve = &state->dataCurveManager->PerfCurve(nConstantBiquadratic);
    pCurve->curveType = CurveType::BiQuadratic;
    pCurve->Name = "ConstantBiquadratic";
    pCurve->Coeff1 = 1;
    pCurve->Coeff2 = 0;
    pCurve->Coeff3 = 0;
    pCurve->Coeff4 = 0;
    pCurve->Coeff5 = 0;
    pCurve->Coeff6 = 0;
    pCurve->Var1Min = -100;
    pCurve->Var1Max = 100;
    pCurve->Var2Min = -100;
    pCurve->Var2Max = 100;

    Coil.MSWasteHeat(1) = nConstantBiquadratic;
    Coil.MSWasteHeat(2) = nConstantBiquadratic;

    int constexpr nCapfT2 = 7;
    pCurve = &state->dataCurveManager->PerfCurve(nCapfT2);
    pCurve->curveType = CurveType::BiQuadratic;
    pCurve->Name = "HP_Heat-Cap-fT2";
    pCurve->Coeff1 = 0.95624428;
    pCurve->Coeff2 = 0;
    pCurve->Coeff3 = 0;
    pCurve->Coeff4 = 0.005999544;
    pCurve->Coeff5 = -0.0000900072;
    pCurve->Coeff6 = 0;
    pCurve->Var1Min = -100;
    pCurve->Var1Max = 100;
    pCurve->Var2Min = -100;
    pCurve->Var2Max = 100;

    Coil.MSCCapFTemp(2) = nCapfT2;

    int constexpr nCapfFF2 = 8;
    pCurve = &state->dataCurveManager->PerfCurve(nCapfFF2);
    pCurve->curveType = CurveType::Quadratic;
    pCurve->Name = "HP_Heat-Cap-fFF2";
    pCurve->Coeff1 = 1;
    pCurve->Coeff2 = 0;
    pCurve->Coeff3 = 0;
    pCurve->Var1Min = 0;
    pCurve->Var1Max = 2;
    pCurve->CurveMin = 0;
    pCurve->CurveMax = 2;

    Coil.MSCCapFFlow(2) = nCapfFF2;

    int constexpr nEIRfT2 = 9;
    pCurve = &state->dataCurveManager->PerfCurve(nEIRfT2);
    pCurve->curveType = CurveType::BiQuadratic;
    pCurve->Name = "HP_Heat-EIR-fT2";
    pCurve->Coeff1 = 1.065476178;
    pCurve->Coeff2 = 0;
    pCurve->Coeff3 = 0;
    pCurve->Coeff4 = -0.0085714308;
    pCurve->Coeff5 = 0.0000857142;
    pCurve->Coeff6 = 0;
    pCurve->Var1Min = -100;
    pCurve->Var1Max = 100;
    pCurve->Var2Min = -100;
    pCurve->Var2Max = 100;

    Coil.MSEIRFTemp(2) = nEIRfT2;

    int constexpr nEIRfFF2 = 10;
    pCurve = &state->dataCurveManager->PerfCurve(nEIRfFF2);
    pCurve->curveType = CurveType::Quadratic;
    pCurve->Name = "HP_Heat-EIR-fFF2";
    pCurve->Coeff1 = 1;
    pCurve->Coeff2 = 0;
    pCurve->Coeff3 = 0;
    pCurve->Var1Min = 0;
    pCurve->Var1Max = 2;
    pCurve->CurveMin = 0;
    pCurve->CurveMax = 2;

    Coil.MSEIRFFlow(2) = nEIRfFF2;

    int constexpr nPLFfPLR2 = 11;
    pCurve = &state->dataCurveManager->PerfCurve(nPLFfPLR2);
    pCurve->curveType = CurveType::Quadratic;
    pCurve->Name = "HP_Heat-PLF-fPLR2";
    pCurve->Coeff1 = 1;
    pCurve->Coeff2 = 0;
    pCurve->Coeff3 = 0;
    pCurve->Var1Min = 0;
    pCurve->Var1Max = 1;
    pCurve->CurveMin = 0.7;
    pCurve->CurveMax = 1;

    Coil.MSPLFFPLR(2) = nPLFfPLR2;

    for (int CurveNum = 1; CurveNum <= state->dataCurveManager->NumCurves; ++CurveNum) {
        PerformanceCurveData &rCurve = state->dataCurveManager->PerfCurve(CurveNum);
        if (rCurve.curveType == CurveType::BiQuadratic) {
            rCurve.ObjectType = "Curve:Biquadratic";
            rCurve.InterpolationType = InterpType::EvaluateCurveToLimits;
        } else if (rCurve.curveType == CurveType::Quadratic) {
            rCurve.ObjectType = "Curve:Quadratic";
            rCurve.InterpolationType = InterpType::EvaluateCurveToLimits;
        }
    }

    // Set up inlet air conditions.
    Coil.InletAirMassFlowRate = Coil.MSRatedAirMassFlowRate(1);
    state->dataHVACGlobal->MSHPMassFlowRateLow = Coil.MSRatedAirMassFlowRate(1);
    state->dataHVACGlobal->MSHPMassFlowRateHigh = Coil.MSRatedAirMassFlowRate(2);
    state->dataEnvrn->OutHumRat = 0.002;
    state->dataEnvrn->OutBaroPress = 101325; // sea level
    Coil.InletAirTemp = 20;
    Coil.InletAirHumRat = 0.008;
    Coil.InletAirEnthalpy = PsyHFnTdbW(Coil.InletAirTemp, Coil.InletAirHumRat);

    // Test high speed
    Real64 SpeedRatio = 1.0;
    Real64 CycRatio = 1.0;
    int SpeedNum = 2;
    int const FanOpMode = ContFanCycCoil;

    // Defroster on
    state->dataEnvrn->OutDryBulbTemp = -5.0; // cold
    CalcMultiSpeedDXCoilHeating(*state, DXCoilNum, SpeedRatio, CycRatio, SpeedNum, FanOpMode, 0);
    Real64 COPwoDefrost = Coil.MSRatedCOP(SpeedNum) /
                          (CurveValue(*state, nEIRfT2, Coil.InletAirTemp, state->dataEnvrn->OutDryBulbTemp) * CurveValue(*state, nEIRfFF2, 1));
    Real64 COPwDefrost = Coil.TotalHeatingEnergyRate / Coil.ElecHeatingPower;
    EXPECT_LT(COPwDefrost, COPwoDefrost);

    // Defroster off
    state->dataEnvrn->OutDryBulbTemp = 5.0; // not cold enough for defroster
    CalcMultiSpeedDXCoilHeating(*state, DXCoilNum, SpeedRatio, CycRatio, SpeedNum, FanOpMode, 0);
    COPwoDefrost = Coil.MSRatedCOP(SpeedNum) /
                   (CurveValue(*state, nEIRfT2, Coil.InletAirTemp, state->dataEnvrn->OutDryBulbTemp) * CurveValue(*state, nEIRfFF2, 1));
    COPwDefrost = Coil.TotalHeatingEnergyRate / Coil.ElecHeatingPower;
    EXPECT_DOUBLE_EQ(COPwoDefrost, COPwDefrost);

    // Test low speed
    SpeedNum = 1;

    // Defroster on
    state->dataEnvrn->OutDryBulbTemp = -5.0; // cold
    CalcMultiSpeedDXCoilHeating(*state, DXCoilNum, SpeedRatio, CycRatio, SpeedNum, FanOpMode, 0);
    COPwoDefrost = Coil.MSRatedCOP(SpeedNum) /
                   (CurveValue(*state, nEIRfT1, Coil.InletAirTemp, state->dataEnvrn->OutDryBulbTemp) * CurveValue(*state, nEIRfFF1, 1));
    COPwDefrost = Coil.TotalHeatingEnergyRate / Coil.ElecHeatingPower;
    EXPECT_LT(COPwDefrost, COPwoDefrost);

    // Defroster off
    state->dataEnvrn->OutDryBulbTemp = 5.0; // not cold enough for defroster
    CalcMultiSpeedDXCoilHeating(*state, DXCoilNum, SpeedRatio, CycRatio, SpeedNum, FanOpMode, 0);
    COPwoDefrost = Coil.MSRatedCOP(SpeedNum) /
                   (CurveValue(*state, nEIRfT1, Coil.InletAirTemp, state->dataEnvrn->OutDryBulbTemp) * CurveValue(*state, nEIRfFF1, 1));
    COPwDefrost = Coil.TotalHeatingEnergyRate / Coil.ElecHeatingPower;
    EXPECT_DOUBLE_EQ(COPwoDefrost, COPwDefrost);

    // Now test that coil output at Speed = 1, CyclingRatio = 1 is the same as Speed = 2 and SpeedRatio = 0
    Real64 DXCoilOutletNodeTemp = Coil.OutletAirTemp;
    Real64 DXCoilOutletNodeHumRat = Coil.OutletAirHumRat;
    Real64 DXCoilOutletNodeEnthalpy = Coil.OutletAirEnthalpy;
    Real64 DXCoilHeatingCapacity = Coil.TotalHeatingEnergyRate;

    SpeedRatio = 0.0;
    CycRatio = 1.0;
    SpeedNum = 2;

    CalcMultiSpeedDXCoilHeating(*state, DXCoilNum, SpeedRatio, CycRatio, SpeedNum, FanOpMode, 0);

    Real64 DXCoilOutletNodeTemp2 = Coil.OutletAirTemp;
    Real64 DXCoilOutletNodeHumRat2 = Coil.OutletAirHumRat;
    Real64 DXCoilOutletNodeEnthalpy2 = Coil.OutletAirEnthalpy;
    Real64 DXCoilHeatingCapacity2 = Coil.TotalHeatingEnergyRate;

    EXPECT_DOUBLE_EQ(DXCoilOutletNodeTemp, DXCoilOutletNodeTemp2);
    EXPECT_DOUBLE_EQ(DXCoilOutletNodeHumRat, DXCoilOutletNodeHumRat2);
    EXPECT_DOUBLE_EQ(DXCoilOutletNodeEnthalpy, DXCoilOutletNodeEnthalpy2);
    EXPECT_DOUBLE_EQ(DXCoilHeatingCapacity, DXCoilHeatingCapacity2);

    // Defroster on
    state->dataEnvrn->OutDryBulbTemp = -5.0; // cold

    SpeedRatio = 0.0;
    CycRatio = 1.0;
    SpeedNum = 1;

    CalcMultiSpeedDXCoilHeating(*state, DXCoilNum, SpeedRatio, CycRatio, SpeedNum, FanOpMode, 0);

    DXCoilOutletNodeTemp = Coil.OutletAirTemp;
    DXCoilOutletNodeHumRat = Coil.OutletAirHumRat;
    DXCoilOutletNodeEnthalpy = Coil.OutletAirEnthalpy;
    DXCoilHeatingCapacity = Coil.TotalHeatingEnergyRate;

    SpeedRatio = 0.0;
    CycRatio = 1.0;
    SpeedNum = 2;

    CalcMultiSpeedDXCoilHeating(*state, DXCoilNum, SpeedRatio, CycRatio, SpeedNum, FanOpMode, 0);

    DXCoilOutletNodeTemp2 = Coil.OutletAirTemp;
    DXCoilOutletNodeHumRat2 = Coil.OutletAirHumRat;
    DXCoilOutletNodeEnthalpy2 = Coil.OutletAirEnthalpy;
    DXCoilHeatingCapacity2 = Coil.TotalHeatingEnergyRate;

    EXPECT_DOUBLE_EQ(DXCoilOutletNodeTemp, DXCoilOutletNodeTemp2);
    EXPECT_DOUBLE_EQ(DXCoilOutletNodeHumRat, DXCoilOutletNodeHumRat2);
    EXPECT_DOUBLE_EQ(DXCoilOutletNodeEnthalpy, DXCoilOutletNodeEnthalpy2);
    EXPECT_DOUBLE_EQ(DXCoilHeatingCapacity, DXCoilHeatingCapacity2);
}

TEST_F(EnergyPlusFixture, TestSingleSpeedDefrostCOP)
{
    // Test that the COP calculation is correct when the defrost is on. #4973

    using DXCoils::CalcMultiSpeedDXCoilHeating;
    using Psychrometrics::PsyHFnTdbW;
    using Psychrometrics::PsyRhoAirFnPbTdbW;
    int DXCoilNum;

    // Set up heating coil and curves.

    state->dataDXCoils->NumDXCoils = 1;
    DXCoilNum = 1;
    state->dataDXCoils->DXCoil.allocate(state->dataDXCoils->NumDXCoils);
    state->dataDXCoils->DXCoilNumericFields.allocate(1);
    state->dataDXCoils->DXCoilOutletTemp.allocate(state->dataDXCoils->NumDXCoils);
    state->dataDXCoils->DXCoilOutletHumRat.allocate(state->dataDXCoils->NumDXCoils);
    state->dataDXCoils->DXCoilFanOpMode.allocate(state->dataDXCoils->NumDXCoils);
    state->dataDXCoils->DXCoilPartLoadRatio.allocate(state->dataDXCoils->NumDXCoils);
    state->dataDXCoils->DXCoilTotalHeating.allocate(state->dataDXCoils->NumDXCoils);
    state->dataDXCoils->DXCoilHeatInletAirDBTemp.allocate(state->dataDXCoils->NumDXCoils);
    state->dataDXCoils->DXCoilHeatInletAirWBTemp.allocate(state->dataDXCoils->NumDXCoils);
    DXCoilData &Coil = state->dataDXCoils->DXCoil(DXCoilNum);

    Coil.Name = "DX Single Speed Heating Coil";
    Coil.DXCoilType = "Coil:Heating:DX:SingleSpeed";
    Coil.DXCoilType_Num = CoilDX_HeatingEmpirical;
    Coil.SchedPtr = DataGlobalConstants::ScheduleAlwaysOn;
    state->dataLoopNodes->Node.allocate(1);
    Coil.AirOutNode = 1;

    Coil.RatedSHR(1) = 1.0;
    Coil.RatedTotCap(1) = 11012.634487601337;
    Coil.RatedCOP(1) = 4.200635910578916;
    Coil.RatedEIR(1) = 1 / Coil.RatedCOP(1);
    Coil.RatedAirVolFlowRate(1) = 0.43873066751851;
    Coil.RatedAirMassFlowRate(1) =
        Coil.RatedAirVolFlowRate(1) * PsyRhoAirFnPbTdbW(*state, state->dataEnvrn->StdBaroPress, 21.11, 0.00881, "InitDXCoil");
    Coil.FanPowerPerEvapAirFlowRate(1) = 773.3;
    Coil.FanPowerPerEvapAirFlowRate_2023(1) = 934.3;
    Coil.MinOATCompressor = -73.27777777777779;
    Coil.CrankcaseHeaterCapacity = 0.0;
    Coil.MaxOATDefrost = 0.0;
    Coil.DefrostStrategy = StandardRatings::DefrostStrat::Resistive;
    Coil.DefrostControl = StandardRatings::HPdefrostControl::Timed;
    Coil.DefrostTime = 0.058333;
    Coil.DefrostCapacity = 1000;
    Coil.PLRImpact = false;
    Coil.FuelType = "Electricity";
    Coil.FuelTypeNum = DataGlobalConstants::ResourceType::Electricity;
    Coil.RegionNum = 4;

    state->dataCurveManager->NumCurves = 5;
    state->dataCurveManager->PerfCurve.allocate(state->dataCurveManager->NumCurves);

    PerformanceCurveData *pCurve;

    int constexpr nCapfT2 = 1;
    pCurve = &state->dataCurveManager->PerfCurve(nCapfT2);
    pCurve->curveType = CurveType::BiQuadratic;
    pCurve->Name = "HP_Heat-Cap-fT2";
    pCurve->Coeff1 = 0.95624428;
    pCurve->Coeff2 = 0;
    pCurve->Coeff3 = 0;
    pCurve->Coeff4 = 0.005999544;
    pCurve->Coeff5 = -0.0000900072;
    pCurve->Coeff6 = 0;
    pCurve->Var1Min = -100;
    pCurve->Var1Max = 100;
    pCurve->Var2Min = -100;
    pCurve->Var2Max = 100;

    Coil.CCapFTemp(1) = nCapfT2;

    int constexpr nCapfFF2 = 2;
    pCurve = &state->dataCurveManager->PerfCurve(nCapfFF2);
    pCurve->curveType = CurveType::Quadratic;
    pCurve->Name = "HP_Heat-Cap-fFF2";
    pCurve->Coeff1 = 1;
    pCurve->Coeff2 = 0;
    pCurve->Coeff3 = 0;
    pCurve->Var1Min = 0;
    pCurve->Var1Max = 2;
    pCurve->CurveMin = 0;
    pCurve->CurveMax = 2;

    Coil.CCapFFlow(1) = nCapfFF2;

    int constexpr nEIRfT2 = 3;
    pCurve = &state->dataCurveManager->PerfCurve(nEIRfT2);
    pCurve->curveType = CurveType::BiQuadratic;
    pCurve->Name = "HP_Heat-EIR-fT2";
    pCurve->Coeff1 = 1.065476178;
    pCurve->Coeff2 = 0;
    pCurve->Coeff3 = 0;
    pCurve->Coeff4 = -0.0085714308;
    pCurve->Coeff5 = 0.0000857142;
    pCurve->Coeff6 = 0;
    pCurve->Var1Min = -100;
    pCurve->Var1Max = 100;
    pCurve->Var2Min = -100;
    pCurve->Var2Max = 100;

    Coil.EIRFTemp(1) = nEIRfT2;

    int constexpr nEIRfFF2 = 4;
    pCurve = &state->dataCurveManager->PerfCurve(nEIRfFF2);
    pCurve->curveType = CurveType::Quadratic;
    pCurve->Name = "HP_Heat-EIR-fFF2";
    pCurve->Coeff1 = 1;
    pCurve->Coeff2 = 0;
    pCurve->Coeff3 = 0;
    pCurve->Var1Min = 0;
    pCurve->Var1Max = 2;
    pCurve->CurveMin = 0;
    pCurve->CurveMax = 2;

    Coil.EIRFFlow(1) = nEIRfFF2;

    int constexpr nPLFfPLR2 = 5;
    pCurve = &state->dataCurveManager->PerfCurve(nPLFfPLR2);
    pCurve->curveType = CurveType::Quadratic;
    pCurve->Name = "HP_Heat-PLF-fPLR2";
    pCurve->Coeff1 = 1;
    pCurve->Coeff2 = 0;
    pCurve->Coeff3 = 0;
    pCurve->Var1Min = 0;
    pCurve->Var1Max = 1;
    pCurve->CurveMin = 0.7;
    pCurve->CurveMax = 1;

    Coil.PLFFPLR(1) = nPLFfPLR2;

    for (int CurveNum = 1; CurveNum <= state->dataCurveManager->NumCurves; ++CurveNum) {
        PerformanceCurveData &rCurve = state->dataCurveManager->PerfCurve(CurveNum);
        if (rCurve.curveType == CurveType::BiQuadratic) {
            rCurve.ObjectType = "Curve:Biquadratic";
            rCurve.InterpolationType = InterpType::EvaluateCurveToLimits;
        } else if (rCurve.curveType == CurveType::Quadratic) {
            rCurve.ObjectType = "Curve:Quadratic";
            rCurve.InterpolationType = InterpType::EvaluateCurveToLimits;
        }
    }

    // Set up inlet air conditions.
    Coil.InletAirMassFlowRate = Coil.RatedAirMassFlowRate(1);
    state->dataEnvrn->OutHumRat = 0.002;
    state->dataEnvrn->OutBaroPress = 101325; // sea level
    Coil.InletAirTemp = 20;
    Coil.InletAirHumRat = 0.008;
    Coil.InletAirEnthalpy = PsyHFnTdbW(Coil.InletAirTemp, Coil.InletAirHumRat);

    int const FanOpMode = ContFanCycCoil;
    Real64 constexpr PLR = 1.0;

    // Defrost Off
    state->dataEnvrn->OutDryBulbTemp = -5.0; // cold
    CalcDXHeatingCoil(*state, DXCoilNum, PLR, FanOpMode);
    Real64 COPwoDefrost =
        Coil.RatedCOP(1) / (CurveValue(*state, nEIRfT2, Coil.InletAirTemp, state->dataEnvrn->OutDryBulbTemp) * CurveValue(*state, nEIRfFF2, 1));
    Real64 COPwDefrost = Coil.TotalHeatingEnergyRate / Coil.ElecHeatingPower;
    EXPECT_LT(COPwDefrost, COPwoDefrost);

    // Defrost On
    state->dataEnvrn->OutDryBulbTemp = 5.0; // not as cold
    CalcDXHeatingCoil(*state, DXCoilNum, PLR, FanOpMode);
    COPwoDefrost =
        Coil.RatedCOP(1) / (CurveValue(*state, nEIRfT2, Coil.InletAirTemp, state->dataEnvrn->OutDryBulbTemp) * CurveValue(*state, nEIRfFF2, 1));
    COPwDefrost = Coil.TotalHeatingEnergyRate / Coil.ElecHeatingPower;
    EXPECT_DOUBLE_EQ(COPwoDefrost, COPwDefrost);
}

TEST_F(EnergyPlusFixture, TestCalcCBF)
{
    using DataEnvironment::StdPressureSeaLevel;
    const std::string CoilType("Coil:WaterHeating:AirToWaterHeatPump:Wrapped");
    const std::string CoilName("The Coil");
    Real64 InletDBTemp(19.722222222222221);
    Real64 InletWBTemp(13.078173565729553);
    Real64 InletAirHumRat;
    constexpr Real64 TotalCap(1303.5987246916557);
    constexpr Real64 AirVolFlowRate(0.085422486640000003);
    constexpr Real64 SHR(0.88);
    Real64 AirPressure;
    Real64 CBF_expected;
    Real64 CBF_calculated;

    AirPressure = StdPressureSeaLevel;
    InletAirHumRat = Psychrometrics::PsyWFnTdbTwbPb(*state, InletDBTemp, InletWBTemp, AirPressure);
    CBF_calculated = CalcCBF(*state, CoilType, CoilName, InletDBTemp, InletAirHumRat, TotalCap, AirVolFlowRate, SHR, true);
    CBF_expected = 0.17268167698750708;
    EXPECT_DOUBLE_EQ(CBF_calculated, CBF_expected);

    // push inlet condition towards saturation curve to test CBF calculation robustness
    InletWBTemp = 19.7; // 19.72 DB / 19.7 WB
    InletAirHumRat = Psychrometrics::PsyWFnTdbTwbPb(*state, InletDBTemp, InletWBTemp, AirPressure);
    CBF_calculated = CalcCBF(*state, CoilType, CoilName, InletDBTemp, InletAirHumRat, TotalCap, AirVolFlowRate, SHR, true);
    EXPECT_NEAR(CBF_calculated, 0.00020826, 0.0000001);

    InletDBTemp = 13.1;  // colder and much less likely inlet air temperature
    InletWBTemp = 13.08; // 13.1 DB / 13.08 WB - hard to find ADP (needed mod to CalcCBF function)
    InletAirHumRat = Psychrometrics::PsyWFnTdbTwbPb(*state, InletDBTemp, InletWBTemp, AirPressure);
    CBF_calculated = CalcCBF(*state, CoilType, CoilName, InletDBTemp, InletAirHumRat, TotalCap, AirVolFlowRate, SHR, true);
    EXPECT_NEAR(CBF_calculated, 0.0001572, 0.0000001);
}

TEST_F(EnergyPlusFixture, DXCoilEvapCondPumpSizingTest)
{

    // tests autosizing evaporatively cooled condenser pump #4802

    std::string const idf_objects = delimited_string({
        "	Schedule:Compact,",
        "	FanAndCoilAvailSched, !- Name",
        "	Fraction,             !- Schedule Type Limits Name",
        "	Through: 12/31,       !- Field 1",
        "	For: AllDays,         !- Field 2",
        "	Until: 24:00, 1.0;    !- Field 3",
        "Curve:Biquadratic,",
        "	WindACCoolCapFT, !- Name",
        "	0.942587793,     !- Coefficient1 Constant",
        "	0.009543347,     !- Coefficient2 x",
        "	0.000683770,     !- Coefficient3 x**2",
        "	-0.011042676,    !- Coefficient4 y",
        "	0.000005249,     !- Coefficient5 y**2",
        "	-0.000009720,    !- Coefficient6 x*y",
        "	12.77778,        !- Minimum Value of x",
        "	23.88889,        !- Maximum Value of x",
        "	18.0,            !- Minimum Value of y",
        "	46.11111,        !- Maximum Value of y",
        "	,                !- Minimum Curve Output",
        "	,                !- Maximum Curve Output",
        "	Temperature,     !- Input Unit Type for X",
        "	Temperature,     !- Input Unit Type for Y",
        "	Dimensionless;   !- Output Unit Type",
        "Curve:Biquadratic,",
        "	WindACEIRFT,   !- Name",
        "	0.342414409,   !- Coefficient1 Constant",
        "	0.034885008,   !- Coefficient2 x",
        "	-0.000623700,  !- Coefficient3 x**2",
        "	0.004977216,   !- Coefficient4 y",
        "	0.000437951,   !- Coefficient5 y**2",
        "	-0.000728028,  !- Coefficient6 x*y",
        "	12.77778,      !- Minimum Value of x",
        "	23.88889,      !- Maximum Value of x",
        "	18.0,          !- Minimum Value of y",
        "	46.11111,      !- Maximum Value of y",
        "	,              !- Minimum Curve Output",
        "	,              !- Maximum Curve Output",
        "	Temperature,   !- Input Unit Type for X",
        "	Temperature,   !- Input Unit Type for Y",
        "	Dimensionless; !- Output Unit Type",
        "Curve:Quadratic,",
        "	WindACCoolCapFFF, !- Name",
        "	0.8,              !- Coefficient1 Constant",
        "	0.2,              !- Coefficient2 x",
        "	0.0,              !- Coefficient3 x**2",
        "	0.5,              !- Minimum Value of x",
        "	1.5;              !- Maximum Value of x",
        "Curve:Quadratic,",
        "	WindACEIRFFF, !- Name",
        "	1.1552,       !- Coefficient1 Constant",
        "  -0.1808,       !- Coefficient2 x",
        "	0.0256,       !- Coefficient3 x**2",
        "	0.5,          !- Minimum Value of x",
        "	1.5;          !- Maximum Value of x",
        "Curve:Quadratic,",
        "	WindACPLFFPLR, !- Name",
        "	0.85,          !- Coefficient1 Constant",
        "	0.15,          !- Coefficient2 x",
        "	0.0,           !- Coefficient3 x**2",
        "	0.0,           !- Minimum Value of x",
        "	1.0;           !- Maximum Value of x",
        "Coil:Cooling:DX:SingleSpeed,",
        "	Furnace ACDXCoil 1,   !- Name",
        " 	FanAndCoilAvailSched, !- Availability Schedule Name",
        "	25000.0,              !- Gross Rated Total Cooling Capacity { W }",
        "	0.75,                 !- Gross Rated Sensible Heat Ratio",
        "	4.40,                 !- Gross Rated Cooling COP { W / W }",
        "	1.30,                 !- Rated Air Flow Rate { m3 / s }",
        "	,                     !- 2017 Rated Evaporator Fan Power Per Volume Flow Rate {W/(m3/s)}",
        "   ,                     !- 2023 Rated Evaporator Fan Power Per Volume Flow Rate {W/(m3/s)}",
        "	DX Cooling Coil Air Inlet Node, !- Air Inlet Node Name",
        "	Heating Coil Air Inlet Node,    !- Air Outlet Node Name",
        "	WindACCoolCapFT,      !- Total Cooling Capacity Function of Temperature Curve Name",
        "	WindACCoolCapFFF,     !- Total Cooling Capacity Function of Flow Fraction Curve Name",
        "	WindACEIRFT,          !- Energy Input Ratio Function of Temperature Curve Name",
        "	WindACEIRFFF,         !- Energy Input Ratio Function of Flow Fraction Curve Name",
        "	WindACPLFFPLR,        !- Part Load Fraction Correlation Curve Name",
        "	,                     !- Minimum Outdoor Dry-Bulb Temperature for Compressor Operation {C}",
        "	0.0,                  !- Nominal Time for Condensate Removal to Begin",
        "	0.0,                  !- Ratio of Initial Moisture Evaporation Rate and Steady State Latent Capacity",
        "	0.0,                  !- Maximum Cycling Rate",
        "	0.0,                  !- Latent Capacity Time Constant",
        "	Split TSW Cooling Coil Condenser Inlet, !- Condenser Air Inlet Node Name",
        "	EvaporativelyCooled,  !- Condenser Type",
        "	0.0,                  !- Evaporative Condenser Effectiveness",
        "	,                     !- Evaporative Condenser Air Flow Rate",
        "	autosize,             !- Evaporative Condenser Pump Rated Power Consumption",
        "	0.0,                  !- Crankcase Heater Capacity",
        "	10.0;                 !- Maximum Outdoor DryBulb Temperature for Crankcase Heater Operation",
    });

    ASSERT_TRUE(process_idf(idf_objects));

    ProcessScheduleInput(*state);
    GetCurveInput(*state);
    GetDXCoils(*state);

    ASSERT_EQ(1, state->dataDXCoils->NumDXCoils);
    EXPECT_EQ(DataSizing::AutoSize, state->dataDXCoils->DXCoil(1).EvapCondPumpElecNomPower(1));

    SetPredefinedTables(*state);

    SizeDXCoil(*state, 1);
    EXPECT_EQ(25000.0, state->dataDXCoils->DXCoil(1).RatedTotCap(1));
    EXPECT_EQ(state->dataDXCoils->DXCoil(1).RatedTotCap(1) * 0.004266, state->dataDXCoils->DXCoil(1).EvapCondPumpElecNomPower(1));
    // Minimum Outdoor Temperature for Compressor Operation defaults to -25.0 C
    EXPECT_EQ(state->dataDXCoils->DXCoil(1).MinOATCompressor, -25.0);
}

TEST_F(EnergyPlusFixture, TestDXCoilIndoorOrOutdoor)
{

    // Test whether the coil is placed indoor or outdoor, by checking the air inlet node location

    using namespace DXCoils;
    using OutAirNodeManager::CheckOutAirNodeNumber;

    // Common Inputs
    int NumCoils;  // total number of coils
    int DXCoilNum; // index to the current coil

    // Allocate
    NumCoils = 3;
    state->dataDXCoils->DXCoil.allocate(NumCoils);

    // IDF snippets
    std::string const idf_objects = delimited_string({
        "OutdoorAir:Node,                                      ",
        "   Outside Air Inlet Node 1; !- Name                  ",
        "OutdoorAir:NodeList,                                  ",
        "   OutsideAirInletNodes;    !- Node or NodeList Name 1",
        "NodeList,                                             ",
        "   OutsideAirInletNodes,    !- Name                   ",
        "   Outside Air Inlet Node 2;!- Node 1 Name            ",
    });

    ASSERT_TRUE(process_idf(idf_objects));

    // Run
    DXCoilNum = 1;
    state->dataDXCoils->DXCoil(DXCoilNum).AirInNode = 1; // "Outside Air Inlet Node 1"
    state->dataDXCoils->DXCoil(DXCoilNum).IsDXCoilInZone = !CheckOutAirNodeNumber(*state, state->dataDXCoils->DXCoil(DXCoilNum).AirInNode);

    DXCoilNum = 2;
    state->dataDXCoils->DXCoil(DXCoilNum).AirInNode = 2; // "Outside Air Inlet Node 2"
    state->dataDXCoils->DXCoil(DXCoilNum).IsDXCoilInZone = !CheckOutAirNodeNumber(*state, state->dataDXCoils->DXCoil(DXCoilNum).AirInNode);

    DXCoilNum = 3; // "Inside Air Inlet Node"
    state->dataDXCoils->DXCoil(DXCoilNum).IsDXCoilInZone = !CheckOutAirNodeNumber(*state, state->dataDXCoils->DXCoil(DXCoilNum).AirInNode);

    // Check
    EXPECT_FALSE(state->dataDXCoils->DXCoil(1).IsDXCoilInZone);
    EXPECT_FALSE(state->dataDXCoils->DXCoil(2).IsDXCoilInZone);
    EXPECT_TRUE(state->dataDXCoils->DXCoil(3).IsDXCoilInZone);
}

TEST_F(EnergyPlusFixture, TestMultiSpeedWasteHeat)
{
    // Test the waste heat function #4536

    using Psychrometrics::PsyHFnTdbW;
    using Psychrometrics::PsyTwbFnTdbWPb;

    std::string const idf_objects = delimited_string({
        " Schedule:Compact,",
        "	FanAndCoilAvailSched, !- Name",
        "	Fraction,             !- Schedule Type Limits Name",
        "	Through: 12/31,       !- Field 1",
        "	For: AllDays,         !- Field 2",
        "	Until: 24:00, 1.0;    !- Field 3",
        " OutdoorAir:Node,",
        "	Outdoor Condenser Air Node, !- Name",
        "	1.0;                     !- Height Above Ground{ m }",
        " Coil:Cooling:DX:MultiSpeed,",
        "  Heat Pump ACDXCoil 1, !- Name",
        "  FanAndCoilAvailSched, !- Availability Schedule Name",
        "  DX Cooling Coil Air Inlet Node, !- Air Inlet Node Name",
        "  Heating Coil Air Inlet Node, !- Air Outlet Node Name",
        "  Outdoor Condenser Air Node, !- Condenser Air Inlet Node Name",
        "  AirCooled, !- Condenser Type",
        "  , !- Minimum Outdoor Dry-Bulb Temperature for Compressor Operation {C}",
        "  , !- Supply Water Storage Tank Name",
        "  , !- Condensate Collection Water Storage Tank Name",
        "  No, !- Apply Part Load Fraction to Speeds Greater than 1",
        "  No, !- Apply Latent Degradation to Speeds Greater than 1",
        "  200.0, !- Crankcase Heater Capacity{ W }",
        "  10.0, !- Maximum Outdoor Dry - Bulb Temperature for Crankcase Heater Operation{ C }",
        "  , !- Basin Heater Capacity{ W / K }",
        "  , !- Basin Heater Setpoint Temperature{ C }",
        "  , !- Basin Heater Operating Schedule Name",
        "  Electricity, !- Fuel Type",
        "  4, !- Number of Speeds",
        "  7500, !- Speed 1 Gross Rated Total Cooling Capacity{ W }",
        "  0.75, !- Speed 1 Gross Rated Sensible Heat Ratio",
        "  3.0, !- Speed 1 Gross Rated Cooling COP{ W / W }",
        "  0.40, !- Speed 1 Rated Air Flow Rate{ m3 / s }",
        "  453.3, !- 2017 Speed 1 Rated Evaporator Fan Power Per Volume Flow Rate {W/(m3/s)}",
        "  453.3, !- 2023 Speed 1 Rated Evaporator Fan Power Per Volume Flow Rate {W/(m3/s)}", //??TBD:BPS
        "  HPACCoolCapFT Speed, !- Speed 1 Total Cooling Capacity Function of Temperature Curve Name",
        "  HPACCoolCapFF Speed, !- Speed 1 Total Cooling Capacity Function of Flow Fraction Curve Name",
        "  HPACCOOLEIRFT Speed, !- Speed 1 Energy Input Ratio Function of Temperature Curve Name",
        "  HPACCOOLEIRFF Speed, !- Speed 1 Energy Input Ratio Function of Flow Fraction Curve Name",
        "  HPACCOOLPLFFPLR Speed, !- Speed 1 Part Load Fraction Correlation Curve Name",
        "  1000.0, !- Speed 1 Nominal Time for Condensate Removal to Begin{ s }",
        "  1.5, !- Speed 1 Ratio of Initial Moisture Evaporation Rate and Steady State Latent Capacity{ dimensionless }",
        "  3.0, !- Speed 1 Maximum Cycling Rate{ cycles / hr }",
        "  45.0, !- Speed 1 Latent Capacity Time Constant{ s }",
        "  0.2, !- Speed 1 Rated Waste Heat Fraction of Power Input{ dimensionless }",
        "  , !- Speed 1 Waste Heat Function of Temperature Curve Name",
        "  0.9, !- Speed 1 Evaporative Condenser Effectiveness{ dimensionless }",
        "  0.05, !- Speed 1 Evaporative Condenser Air Flow Rate{ m3 / s }",
        "  50, !- Speed 1 Rated Evaporative Condenser Pump Power Consumption{ W }",
        "  17500, !- Speed 2 Gross Rated Total Cooling Capacity{ W }",
        "  0.75, !- Speed 2 Gross Rated Sensible Heat Ratio",
        "  3.0, !- Speed 2 Gross Rated Cooling COP{ W / W }",
        "  0.85, !- Speed 2 Rated Air Flow Rate{ m3 / s }",
        "  523.3, !- 2017 Speed 2 Rated Evaporator Fan Power Per Volume Flow Rate {W/(m3/s)}",
        "  523.3, !- 2023 Speed 2 Rated Evaporator Fan Power Per Volume Flow Rate {W/(m3/s)}", //??TBD:BPS
        "  HPACCoolCapFT Speed, !- Speed 2 Total Cooling Capacity Function of Temperature Curve Name",
        "  HPACCoolCapFF Speed, !- Speed 2 Total Cooling Capacity Function of Flow Fraction Curve Name",
        "  HPACCOOLEIRFT Speed, !- Speed 2 Energy Input Ratio Function of Temperature Curve Name",
        "  HPACCOOLEIRFF Speed, !- Speed 2 Energy Input Ratio Function of Flow Fraction Curve Name",
        "  HPACCOOLPLFFPLR Speed, !- Speed 2 Part Load Fraction Correlation Curve Name",
        "  1000.0, !- Speed 2 Nominal Time for Condensate Removal to Begin{ s }",
        "  1.5, !- Speed 2 Ratio of Initial Moisture Evaporation Rate and steady state Latent Capacity{ dimensionless }",
        "  3.0, !- Speed 2 Maximum Cycling Rate{ cycles / hr }",
        "  45.0, !- Speed 2 Latent Capacity Time Constant{ s }",
        "  0.2, !- Speed 2 Rated Waste Heat Fraction of Power Input{ dimensionless }",
        "  , !- Speed 2 Waste Heat Function of Temperature Curve Name",
        "  0.9, !- Speed 2 Evaporative Condenser Effectiveness{ dimensionless }",
        "  0.1, !- Speed 2 Evaporative Condenser Air Flow Rate{ m3 / s }",
        "  60, !- Speed 2 Rated Evaporative Condenser Pump Power Consumption{ W }",
        "  25500, !- Speed 3 Gross Rated Total Cooling Capacity{ W }",
        "  0.75, !- Speed 3 Gross Rated Sensible Heat Ratio",
        "  3.0, !- Speed 3 Gross Rated Cooling COP{ W / W }",
        "  1.25, !- Speed 3 Rated Air Flow Rate{ m3 / s }",
        "  573.3, !- 2017 Speed 3 Rated Evaporator Fan Power Per Volume Flow Rate {W/(m3/s)}",
        "  573.3, !- 2023 Speed 3 Rated Evaporator Fan Power Per Volume Flow Rate {W/(m3/s)}", //??TBD:BPS
        "  HPACCoolCapFT Speed, !- Speed 3 Total Cooling Capacity Function of Temperature Curve Name",
        "  HPACCoolCapFF Speed, !- Speed 3 Total Cooling Capacity Function of Flow Fraction Curve Name",
        "  HPACCOOLEIRFT Speed, !- Speed 3 Energy Input Ratio Function of Temperature Curve Name",
        "  HPACCOOLEIRFF Speed, !- Speed 3 Energy Input Ratio Function of Flow Fraction Curve Name",
        "  HPACCOOLPLFFPLR Speed, !- Speed 3 Part Load Fraction Correlation Curve Name",
        "  1000.0, !- Speed 3 Nominal Time for Condensate Removal to Begin{ s }",
        "  1.5, !- Speed 3 Ratio of Initial Moisture Evaporation Rate and steady state Latent Capacity{ dimensionless }",
        "  3.0, !- Speed 3 Maximum Cycling Rate{ cycles / hr }",
        "  45.0, !- Speed 3 Latent Capacity Time Constant{ s }",
        "  0.2, !- Speed 3 Rated Waste Heat Fraction of Power Input{ dimensionless }",
        "  , !- Speed 3 Waste Heat Function of Temperature Curve Name",
        "  0.9, !- Speed 3 Evaporative Condenser Effectiveness{ dimensionless }",
        "  0.2, !- Speed 3 Evaporative Condenser Air Flow Rate{ m3 / s }",
        "  80, !- Speed 3 Rated Evaporative Condenser Pump Power Consumption{ W }",
        "  35500, !- Speed 4 Gross Rated Total Cooling Capacity{ W }",
        "  0.75, !- Speed 4 Gross Rated Sensible Heat Ratio",
        "  3.0, !- Speed 4 Gross Rated Cooling COP{ W / W }",
        "  1.75, !- Speed 4 Rated Air Flow Rate{ m3 / s }",
        "  673.3, !- 2017 Speed 4 Rated Evaporator Fan Power Per Volume Flow Rate {W/(m3/s)}",
        "  673.3, !- 2023 Speed 4 Rated Evaporator Fan Power Per Volume Flow Rate {W/(m3/s)}", //??TBD:BPS
        "  HPACCoolCapFT Speed, !- Speed 4 Total Cooling Capacity Function of Temperature Curve Name",
        "  HPACCoolCapFF Speed, !- Speed 4 Total Cooling Capacity Function of Flow Fraction Curve Name",
        "  HPACCOOLEIRFT Speed, !- Speed 4 Energy Input Ratio Function of Temperature Curve Name",
        "  HPACCOOLEIRFF Speed, !- Speed 4 Energy Input Ratio Function of Flow Fraction Curve Name",
        "  HPACCOOLPLFFPLR Speed, !- Speed 4 Part Load Fraction Correlation Curve Name",
        "  1000.0, !- Speed 4 Nominal Time for Condensate Removal to Begin{ s }",
        "  1.5, !- Speed 4 Ratio of Initial Moisture Evaporation Rate and steady state Latent Capacity{ dimensionless }",
        "  3.0, !- Speed 4 Maximum Cycling Rate{ cycles / hr }",
        "  45.0, !- Speed 4 Latent Capacity Time Constant{ s }",
        "  0.2, !- Speed 4 Rated Waste Heat Fraction of Power Input{ dimensionless }",
        "  , !- Speed 4 Waste Heat Function of Temperature Curve Name",
        "  0.9, !- Speed 4 Evaporative Condenser Effectiveness{ dimensionless }",
        "  0.3, !- Speed 4 Evaporative Condenser Air Flow Rate{ m3 / s }",
        " 100;                     !- Speed 4 Rated Evaporative Condenser Pump Power Consumption{ W }",
        " Curve:Biquadratic,",
        "  HPACCoolCapFT Speed, !- Name",
        "  1.0, !- Coefficient1 Constant",
        "  0.0, !- Coefficient2 x",
        "  0.0, !- Coefficient3 x**2",
        "  0.0, !- Coefficient4 y",
        "  0.0, !- Coefficient5 y**2",
        "  0.0, !- Coefficient6 x*y",
        "  12.77778, !- Minimum Value of x",
        "  23.88889, !- Maximum Value of x",
        "  23.88889, !- Minimum Value of y",
        "  46.11111, !- Maximum Value of y",
        "  , !- Minimum Curve Output",
        "  , !- Maximum Curve Output",
        "  Temperature, !- Input Unit Type for X",
        "  Temperature, !- Input Unit Type for Y",
        "  Dimensionless;           !- Output Unit Type",
        " Curve:Cubic,",
        "  HPACCoolCapFF Speed, !- Name",
        "  .47278589, !- Coefficient1 Constant",
        "  1.2433415, !- Coefficient2 x",
        "  -1.0387055, !- Coefficient3 x**2",
        "  .32257813, !- Coefficient4 x**3",
        "  0.5, !- Minimum Value of x",
        "  1.5;                   !- Maximum Value of x",
        " Curve:Biquadratic,",
        "  HPACCOOLEIRFT Speed, !- Name",
        "  0.632475E+00, !- Coefficient1 Constant",
        "  -0.121321E-01, !- Coefficient2 x",
        "  0.507773E-03, !- Coefficient3 x**2",
        "  0.155377E-01, !- Coefficient4 y",
        "  0.272840E-03, !- Coefficient5 y**2",
        "  -0.679201E-03, !- Coefficient6 x*y",
        "  12.77778, !- Minimum Value of x",
        "  23.88889, !- Maximum Value of x",
        "  23.88889, !- Minimum Value of y",
        "  46.11111, !- Maximum Value of y",
        "  , !- Minimum Curve Output",
        "  , !- Maximum Curve Output",
        "  Temperature, !- Input Unit Type for X",
        "  Temperature, !- Input Unit Type for Y",
        "  Dimensionless;           !- Output Unit Type",
        "Curve:Cubic,",
        "  HPACCOOLEIRFF Speed, !- Name",
        "  .47278589, !- Coefficient1 Constant",
        "  1.2433415, !- Coefficient2 x",
        "  -1.0387055, !- Coefficient3 x**2",
        "  .32257813, !- Coefficient4 x**3",
        "  0.5, !- Minimum Value of x",
        "  1.5;                     !- Maximum Value of x",
        "Curve:Quadratic,",
        "  HPACCOOLPLFFPLR Speed, !- Name",
        "  0.85, !- Coefficient1 Constant",
        "  0.15, !- Coefficient2 x",
        "  0.0, !- Coefficient3 x**2",
        "  0.0, !- Minimum Value of x",
        "  1.0;                     !- Maximum Value of x",
    });

    ASSERT_TRUE(process_idf(idf_objects));

    // Case 1 test
    GetDXCoils(*state);

    EXPECT_EQ("Electricity", state->dataDXCoils->DXCoil(1).FuelType); // it also covers a test for fuel type input
    EXPECT_TRUE(compare_enums(DataGlobalConstants::ResourceType::Electricity, state->dataDXCoils->DXCoil(1).FuelTypeNum));
    EXPECT_EQ(0, state->dataDXCoils->DXCoil(1).MSWasteHeat(2));

    // Test calculations of the waste heat function #5162

    // Case 2 test waste heat is zero when the parent has not heat recovery inputs
    state->dataDXCoils->DXCoil(1).FuelType = "NaturalGas";
    state->dataDXCoils->DXCoil(1).FuelTypeNum = DataGlobalConstants::ResourceType::Natural_Gas;
    state->dataDXCoils->DXCoil(1).MSHPHeatRecActive = false;

    state->dataEnvrn->OutDryBulbTemp = 35;
    state->dataEnvrn->OutHumRat = 0.0128;
    state->dataEnvrn->OutBaroPress = 101325;
    state->dataEnvrn->OutWetBulbTemp =
        PsyTwbFnTdbWPb(*state, state->dataEnvrn->OutDryBulbTemp, state->dataEnvrn->OutHumRat, state->dataEnvrn->OutBaroPress);

    state->dataDXCoils->DXCoil(1).MSRatedAirMassFlowRate(1) = state->dataDXCoils->DXCoil(1).MSRatedAirVolFlowRate(1) * 1.2;
    state->dataDXCoils->DXCoil(1).MSRatedAirMassFlowRate(2) = state->dataDXCoils->DXCoil(1).MSRatedAirVolFlowRate(2) * 1.2;
    state->dataDXCoils->DXCoil(1).InletAirMassFlowRate = state->dataDXCoils->DXCoil(1).MSRatedAirMassFlowRate(2);
    state->dataHVACGlobal->MSHPMassFlowRateLow = state->dataDXCoils->DXCoil(1).MSRatedAirMassFlowRate(1);
    state->dataHVACGlobal->MSHPMassFlowRateHigh = state->dataDXCoils->DXCoil(1).MSRatedAirMassFlowRate(2);

    state->dataDXCoils->DXCoil(1).InletAirTemp = 25.0;
    state->dataDXCoils->DXCoil(1).InletAirHumRat = 0.005;
    state->dataDXCoils->DXCoil(1).InletAirEnthalpy = PsyHFnTdbW(25.0, 0.005);

    state->dataDXCoils->DXCoil(1).SchedPtr = 1;
    state->dataScheduleMgr->Schedule(state->dataDXCoils->DXCoil(1).SchedPtr).CurrentValue = 1.0; // enable the VRF condenser
    state->dataDXCoils->DXCoil(1).MSRatedCBF(1) = 0.1262;
    state->dataDXCoils->DXCoil(1).MSRatedCBF(2) = 0.0408;

    CalcMultiSpeedDXCoilCooling(*state, 1, 1, 1, 2, 1, DataHVACGlobals::CompressorOperation::On, 0);
    EXPECT_EQ(0, state->dataHVACGlobal->MSHPWasteHeat);

    // Case 3 heat recovery is true and no waste heat function cuvre
    state->dataDXCoils->DXCoil(1).MSWasteHeat(1) = 0;
    state->dataDXCoils->DXCoil(1).MSWasteHeat(2) = 0;
    state->dataDXCoils->DXCoil(1).MSHPHeatRecActive = true;

    CalcMultiSpeedDXCoilCooling(*state, 1, 1, 1, 2, 1, DataHVACGlobals::CompressorOperation::On, 0);

    EXPECT_NEAR(1302.748, state->dataHVACGlobal->MSHPWasteHeat, 0.001);
}

TEST_F(EnergyPlusFixture, DXCoil_ValidateADPFunction)
{

    using Psychrometrics::PsyRhoAirFnPbTdbW;

    // tests autosizing DX coil SHR #4853

    std::string const idf_objects = delimited_string({
        "	Schedule:Compact,",
        "	FanAndCoilAvailSched, !- Name",
        "	Fraction,             !- Schedule Type Limits Name",
        "	Through: 12/31,       !- Field 1",
        "	For: AllDays,         !- Field 2",
        "	Until: 24:00, 1.0;    !- Field 3",
        "Curve:Biquadratic,",
        "	WindACCoolCapFT, !- Name",
        "	0.942587793,     !- Coefficient1 Constant",
        "	0.009543347,     !- Coefficient2 x",
        "	0.000683770,     !- Coefficient3 x**2",
        "	-0.011042676,    !- Coefficient4 y",
        "	0.000005249,     !- Coefficient5 y**2",
        "	-0.000009720,    !- Coefficient6 x*y",
        "	12.77778,        !- Minimum Value of x",
        "	23.88889,        !- Maximum Value of x",
        "	18.0,            !- Minimum Value of y",
        "	46.11111,        !- Maximum Value of y",
        "	,                !- Minimum Curve Output",
        "	,                !- Maximum Curve Output",
        "	Temperature,     !- Input Unit Type for X",
        "	Temperature,     !- Input Unit Type for Y",
        "	Dimensionless;   !- Output Unit Type",
        "Curve:Biquadratic,",
        "	WindACEIRFT,   !- Name",
        "	0.342414409,   !- Coefficient1 Constant",
        "	0.034885008,   !- Coefficient2 x",
        "	-0.000623700,  !- Coefficient3 x**2",
        "	0.004977216,   !- Coefficient4 y",
        "	0.000437951,   !- Coefficient5 y**2",
        "	-0.000728028,  !- Coefficient6 x*y",
        "	12.77778,      !- Minimum Value of x",
        "	23.88889,      !- Maximum Value of x",
        "	18.0,          !- Minimum Value of y",
        "	46.11111,      !- Maximum Value of y",
        "	,              !- Minimum Curve Output",
        "	,              !- Maximum Curve Output",
        "	Temperature,   !- Input Unit Type for X",
        "	Temperature,   !- Input Unit Type for Y",
        "	Dimensionless; !- Output Unit Type",
        "Curve:Quadratic,",
        "	WindACCoolCapFFF, !- Name",
        "	0.8,              !- Coefficient1 Constant",
        "	0.2,              !- Coefficient2 x",
        "	0.0,              !- Coefficient3 x**2",
        "	0.5,              !- Minimum Value of x",
        "	1.5;              !- Maximum Value of x",
        "Curve:Quadratic,",
        "	WindACEIRFFF, !- Name",
        "	1.1552,       !- Coefficient1 Constant",
        "  -0.1808,       !- Coefficient2 x",
        "	0.0256,       !- Coefficient3 x**2",
        "	0.5,          !- Minimum Value of x",
        "	1.5;          !- Maximum Value of x",
        "Curve:Quadratic,",
        "	WindACPLFFPLR, !- Name",
        "	0.85,          !- Coefficient1 Constant",
        "	0.15,          !- Coefficient2 x",
        "	0.0,           !- Coefficient3 x**2",
        "	0.0,           !- Minimum Value of x",
        "	1.0;           !- Maximum Value of x",
        "Coil:Cooling:DX:SingleSpeed,",
        "	Furnace ACDXCoil 1,   !- Name",
        " 	FanAndCoilAvailSched, !- Availability Schedule Name",
        "	25000.0,              !- Gross Rated Total Cooling Capacity { W }",
        "	autosize,             !- Gross Rated Sensible Heat Ratio",
        "	4.40,                 !- Gross Rated Cooling COP { W / W }",
        "	1.30,                 !- Rated Air Flow Rate { m3 / s }",
        "	,                     !- 2017 Rated Evaporator Fan Power Per Volume Flow Rate {W/(m3/s)}",
        "   ,                     !- 2023 Rated Evaporator Fan Power Per Volume Flow Rate {W/(m3/s)}",
        "	DX Cooling Coil Air Inlet Node, !- Air Inlet Node Name",
        "	Heating Coil Air Inlet Node,    !- Air Outlet Node Name",
        "	WindACCoolCapFT,      !- Total Cooling Capacity Function of Temperature Curve Name",
        "	WindACCoolCapFFF,     !- Total Cooling Capacity Function of Flow Fraction Curve Name",
        "	WindACEIRFT,          !- Energy Input Ratio Function of Temperature Curve Name",
        "	WindACEIRFFF,         !- Energy Input Ratio Function of Flow Fraction Curve Name",
        "	WindACPLFFPLR,        !- Part Load Fraction Correlation Curve Name",
        "	,                     !- Minimum Outdoor Dry-Bulb Temperature for Compressor Operation {C}",
        "	0.0,                  !- Nominal Time for Condensate Removal to Begin",
        "	0.0,                  !- Ratio of Initial Moisture Evaporation Rate and Steady State Latent Capacity",
        "	0.0,                  !- Maximum Cycling Rate",
        "	0.0,                  !- Latent Capacity Time Constant",
        "	Split TSW Cooling Coil Condenser Inlet, !- Condenser Air Inlet Node Name",
        "	EvaporativelyCooled,  !- Condenser Type",
        "	0.0,                  !- Evaporative Condenser Effectiveness",
        "	,                     !- Evaporative Condenser Air Flow Rate",
        "	autosize,             !- Evaporative Condenser Pump Rated Power Consumption",
        "	0.0,                  !- Crankcase Heater Capacity",
        "	10.0;                 !- Maximum Outdoor DryBulb Temperature for Crankcase Heater Operation",
    });

    ASSERT_TRUE(process_idf(idf_objects));

    ProcessScheduleInput(*state);
    GetCurveInput(*state);
    GetDXCoils(*state);
    SetPredefinedTables(*state);
    state->dataSize->CurZoneEqNum = 1;

    // Need this to prevent crash in Sizers
    state->dataSize->FinalZoneSizing.allocate(1);
    state->dataSize->FinalZoneSizing(state->dataSize->CurZoneEqNum).DesCoolVolFlow = 0.1;
    state->dataSize->FinalZoneSizing(state->dataSize->CurZoneEqNum).DesHeatVolFlow = 0.1;
    state->dataSize->DataFlowUsedForSizing = 0.1;
    state->dataSize->ZoneEqSizing.allocate(1);
    state->dataSize->ZoneEqSizing(state->dataSize->CurZoneEqNum).CoolingCapacity = true;
    state->dataSize->ZoneEqSizing(state->dataSize->CurZoneEqNum).DesCoolingLoad = state->dataDXCoils->DXCoil(1).RatedTotCap(1);
    state->dataSize->ZoneEqSizing(state->dataSize->CurZoneEqNum).DesignSizeFromParent = false;
    state->dataSize->ZoneEqSizing(state->dataSize->CurZoneEqNum).SizingMethod.allocate(25);
    state->dataSize->ZoneEqSizing(state->dataSize->CurZoneEqNum).SizingMethod(DataHVACGlobals::SystemAirflowSizing) = DataSizing::SupplyAirFlowRate;
    state->dataSize->ZoneSizingInput.allocate(1);
    state->dataSize->ZoneSizingInput(1).ZoneNum = 1;
    state->dataSize->NumZoneSizingInput = 1;
    state->dataSize->ZoneSizingRunDone = true;
    state->dataEnvrn->StdBaroPress = 101325.0;

    SizeDXCoil(*state, 1); // normal sizing

    Real64 constexpr RatedInletAirTemp(26.6667);   // 26.6667C or 80F
    Real64 constexpr RatedInletAirHumRat(0.01125); // Humidity ratio corresponding to 80F dry bulb/67F wet bulb
    std::string const CallingRoutine("DXCoil_ValidateADPFunction");

    Real64 CBF_calculated = CalcCBF(*state,
                                    state->dataDXCoils->DXCoil(1).DXCoilType,
                                    state->dataDXCoils->DXCoil(1).Name,
                                    RatedInletAirTemp,
                                    RatedInletAirHumRat,
                                    state->dataDXCoils->DXCoil(1).RatedTotCap(1),
                                    state->dataDXCoils->DXCoil(1).RatedAirVolFlowRate(1),
                                    state->dataDXCoils->DXCoil(1).RatedSHR(1),
                                    true);

    EXPECT_NEAR(0.792472, state->dataDXCoils->DXCoil(1).RatedSHR(1), 0.0000001);
    EXPECT_NEAR(0.00213735, CBF_calculated, 0.0000001);

    state->dataDXCoils->DXCoil(1).RatedTotCap(1) = 35000.0; // simulate outlet condition right at the saturation curve
    state->dataDXCoils->DXCoil(1).RatedSHR(1) = AutoSize;

    SizeDXCoil(*state, 1);
    CBF_calculated = CalcCBF(*state,
                             state->dataDXCoils->DXCoil(1).DXCoilType,
                             state->dataDXCoils->DXCoil(1).Name,
                             RatedInletAirTemp,
                             RatedInletAirHumRat,
                             state->dataDXCoils->DXCoil(1).RatedTotCap(1),
                             state->dataDXCoils->DXCoil(1).RatedAirVolFlowRate(1),
                             state->dataDXCoils->DXCoil(1).RatedSHR(1),
                             true);

    EXPECT_NEAR(0.67908322, state->dataDXCoils->DXCoil(1).RatedSHR(1), 0.0000001);
    EXPECT_NEAR(0.00298921, CBF_calculated, 0.0000001);

    state->dataDXCoils->DXCoil(1).RatedTotCap(1) = 40000.0; // reverse perturb SHR (i.e., decrease SHR), CalcCBF would have failed with RH >= 1.0
    state->dataDXCoils->DXCoil(1).RatedSHR(1) = AutoSize;

    SizeDXCoil(*state, 1);
    CBF_calculated = CalcCBF(*state,
                             state->dataDXCoils->DXCoil(1).DXCoilType,
                             state->dataDXCoils->DXCoil(1).Name,
                             RatedInletAirTemp,
                             RatedInletAirHumRat,
                             state->dataDXCoils->DXCoil(1).RatedTotCap(1),
                             state->dataDXCoils->DXCoil(1).RatedAirVolFlowRate(1),
                             state->dataDXCoils->DXCoil(1).RatedSHR(1),
                             true);

    EXPECT_NEAR(0.64708322, state->dataDXCoils->DXCoil(1).RatedSHR(1), 0.0000001);
    EXPECT_NEAR(0.00252307, CBF_calculated, 0.0000001);
}

TEST_F(EnergyPlusFixture, TestMultiSpeedCoolingCrankcaseOutput)
{
    // Test the crankcase heat for Coil:Cooling:DX:MultiSpeed #5659

    std::string const idf_objects = delimited_string({
        " Schedule:Compact,",
        "	FanAndCoilAvailSched, !- Name",
        "	Fraction,             !- Schedule Type Limits Name",
        "	Through: 12/31,       !- Field 1",
        "	For: AllDays,         !- Field 2",
        "	Until: 24:00, 1.0;    !- Field 3",
        " OutdoorAir:Node,",
        "	Outdoor Condenser Air Node, !- Name",
        "	1.0;                     !- Height Above Ground{ m }",
        " Coil:Cooling:DX:MultiSpeed,",
        "  Heat Pump ACDXCoil 1, !- Name",
        "  FanAndCoilAvailSched, !- Availability Schedule Name",
        "  DX Cooling Coil Air Inlet Node, !- Air Inlet Node Name",
        "  Heating Coil Air Inlet Node, !- Air Outlet Node Name",
        "  Outdoor Condenser Air Node, !- Condenser Air Inlet Node Name",
        "  AirCooled, !- Condenser Type",
        "  , !- Minimum Outdoor Dry-Bulb Temperature for Compressor Operation {C}",
        "  , !- Supply Water Storage Tank Name",
        "  , !- Condensate Collection Water Storage Tank Name",
        "  No, !- Apply Part Load Fraction to Speeds Greater than 1",
        "  No, !- Apply Latent Degradation to Speeds Greater than 1",
        "  200.0, !- Crankcase Heater Capacity{ W }",
        "  10.0, !- Maximum Outdoor Dry - Bulb Temperature for Crankcase Heater Operation{ C }",
        "  , !- Basin Heater Capacity{ W / K }",
        "  , !- Basin Heater Setpoint Temperature{ C }",
        "  , !- Basin Heater Operating Schedule Name",
        "  Electricity, !- Fuel Type",
        "  4, !- Number of Speeds",
        "  7500, !- Speed 1 Gross Rated Total Cooling Capacity{ W }",
        "  0.75, !- Speed 1 Gross Rated Sensible Heat Ratio",
        "  3.0, !- Speed 1 Gross Rated Cooling COP{ W / W }",
        "  0.40, !- Speed 1 Rated Air Flow Rate{ m3 / s }",
        "  453.3, !- 2017 Speed 1 Rated Evaporator Fan Power Per Volume Flow Rate {W/(m3/s)}",
        "  453.3, !- 2023 Speed 1 Rated Evaporator Fan Power Per Volume Flow Rate {W/(m3/s)}", //??TBD:BPS
        "  HPACCoolCapFT Speed, !- Speed 1 Total Cooling Capacity Function of Temperature Curve Name",
        "  HPACCoolCapFF Speed, !- Speed 1 Total Cooling Capacity Function of Flow Fraction Curve Name",
        "  HPACCOOLEIRFT Speed, !- Speed 1 Energy Input Ratio Function of Temperature Curve Name",
        "  HPACCOOLEIRFF Speed, !- Speed 1 Energy Input Ratio Function of Flow Fraction Curve Name",
        "  HPACCOOLPLFFPLR Speed, !- Speed 1 Part Load Fraction Correlation Curve Name",
        "  1000.0, !- Speed 1 Nominal Time for Condensate Removal to Begin{ s }",
        "  1.5, !- Speed 1 Ratio of Initial Moisture Evaporation Rate and Steady State Latent Capacity{ dimensionless }",
        "  3.0, !- Speed 1 Maximum Cycling Rate{ cycles / hr }",
        "  45.0, !- Speed 1 Latent Capacity Time Constant{ s }",
        "  0.2, !- Speed 1 Rated Waste Heat Fraction of Power Input{ dimensionless }",
        "  , !- Speed 1 Waste Heat Function of Temperature Curve Name",
        "  0.9, !- Speed 1 Evaporative Condenser Effectiveness{ dimensionless }",
        "  0.05, !- Speed 1 Evaporative Condenser Air Flow Rate{ m3 / s }",
        "  50, !- Speed 1 Rated Evaporative Condenser Pump Power Consumption{ W }",
        "  17500, !- Speed 2 Gross Rated Total Cooling Capacity{ W }",
        "  0.75, !- Speed 2 Gross Rated Sensible Heat Ratio",
        "  3.0, !- Speed 2 Gross Rated Cooling COP{ W / W }",
        "  0.85, !- Speed 2 Rated Air Flow Rate{ m3 / s }",
        "  523.3, !- 2017 Speed 2 Rated Evaporator Fan Power Per Volume Flow Rate {W/(m3/s)}",
        "  523.3, !- 2023 Speed 2 Rated Evaporator Fan Power Per Volume Flow Rate {W/(m3/s)}", //??TBD:BPS
        "  HPACCoolCapFT Speed, !- Speed 2 Total Cooling Capacity Function of Temperature Curve Name",
        "  HPACCoolCapFF Speed, !- Speed 2 Total Cooling Capacity Function of Flow Fraction Curve Name",
        "  HPACCOOLEIRFT Speed, !- Speed 2 Energy Input Ratio Function of Temperature Curve Name",
        "  HPACCOOLEIRFF Speed, !- Speed 2 Energy Input Ratio Function of Flow Fraction Curve Name",
        "  HPACCOOLPLFFPLR Speed, !- Speed 2 Part Load Fraction Correlation Curve Name",
        "  1000.0, !- Speed 2 Nominal Time for Condensate Removal to Begin{ s }",
        "  1.5, !- Speed 2 Ratio of Initial Moisture Evaporation Rate and steady state Latent Capacity{ dimensionless }",
        "  3.0, !- Speed 2 Maximum Cycling Rate{ cycles / hr }",
        "  45.0, !- Speed 2 Latent Capacity Time Constant{ s }",
        "  0.2, !- Speed 2 Rated Waste Heat Fraction of Power Input{ dimensionless }",
        "  , !- Speed 2 Waste Heat Function of Temperature Curve Name",
        "  0.9, !- Speed 2 Evaporative Condenser Effectiveness{ dimensionless }",
        "  0.1, !- Speed 2 Evaporative Condenser Air Flow Rate{ m3 / s }",
        "  60, !- Speed 2 Rated Evaporative Condenser Pump Power Consumption{ W }",
        "  25500, !- Speed 3 Gross Rated Total Cooling Capacity{ W }",
        "  0.75, !- Speed 3 Gross Rated Sensible Heat Ratio",
        "  3.0, !- Speed 3 Gross Rated Cooling COP{ W / W }",
        "  1.25, !- Speed 3 Rated Air Flow Rate{ m3 / s }",
        "  573.3, !- 2017 Speed 3 Rated Evaporator Fan Power Per Volume Flow Rate {W/(m3/s)}",
        "  573.3, !- 2023 Speed 3 Rated Evaporator Fan Power Per Volume Flow Rate {W/(m3/s)}", //??TBD:BPS
        "  HPACCoolCapFT Speed, !- Speed 3 Total Cooling Capacity Function of Temperature Curve Name",
        "  HPACCoolCapFF Speed, !- Speed 3 Total Cooling Capacity Function of Flow Fraction Curve Name",
        "  HPACCOOLEIRFT Speed, !- Speed 3 Energy Input Ratio Function of Temperature Curve Name",
        "  HPACCOOLEIRFF Speed, !- Speed 3 Energy Input Ratio Function of Flow Fraction Curve Name",
        "  HPACCOOLPLFFPLR Speed, !- Speed 3 Part Load Fraction Correlation Curve Name",
        "  1000.0, !- Speed 3 Nominal Time for Condensate Removal to Begin{ s }",
        "  1.5, !- Speed 3 Ratio of Initial Moisture Evaporation Rate and steady state Latent Capacity{ dimensionless }",
        "  3.0, !- Speed 3 Maximum Cycling Rate{ cycles / hr }",
        "  45.0, !- Speed 3 Latent Capacity Time Constant{ s }",
        "  0.2, !- Speed 3 Rated Waste Heat Fraction of Power Input{ dimensionless }",
        "  , !- Speed 3 Waste Heat Function of Temperature Curve Name",
        "  0.9, !- Speed 3 Evaporative Condenser Effectiveness{ dimensionless }",
        "  0.2, !- Speed 3 Evaporative Condenser Air Flow Rate{ m3 / s }",
        "  80, !- Speed 3 Rated Evaporative Condenser Pump Power Consumption{ W }",
        "  35500, !- Speed 4 Gross Rated Total Cooling Capacity{ W }",
        "  0.75, !- Speed 4 Gross Rated Sensible Heat Ratio",
        "  3.0, !- Speed 4 Gross Rated Cooling COP{ W / W }",
        "  1.75, !- Speed 4 Rated Air Flow Rate{ m3 / s }",
        "  673.3, !- 2017 Speed 4 Rated Evaporator Fan Power Per Volume Flow Rate {W/(m3/s)}",
        "  673.3, !- 2023 Speed 4 Rated Evaporator Fan Power Per Volume Flow Rate {W/(m3/s)}", //??TBD:BPS
        "  HPACCoolCapFT Speed, !- Speed 4 Total Cooling Capacity Function of Temperature Curve Name",
        "  HPACCoolCapFF Speed, !- Speed 4 Total Cooling Capacity Function of Flow Fraction Curve Name",
        "  HPACCOOLEIRFT Speed, !- Speed 4 Energy Input Ratio Function of Temperature Curve Name",
        "  HPACCOOLEIRFF Speed, !- Speed 4 Energy Input Ratio Function of Flow Fraction Curve Name",
        "  HPACCOOLPLFFPLR Speed, !- Speed 4 Part Load Fraction Correlation Curve Name",
        "  1000.0, !- Speed 4 Nominal Time for Condensate Removal to Begin{ s }",
        "  1.5, !- Speed 4 Ratio of Initial Moisture Evaporation Rate and steady state Latent Capacity{ dimensionless }",
        "  3.0, !- Speed 4 Maximum Cycling Rate{ cycles / hr }",
        "  45.0, !- Speed 4 Latent Capacity Time Constant{ s }",
        "  0.2, !- Speed 4 Rated Waste Heat Fraction of Power Input{ dimensionless }",
        "  , !- Speed 4 Waste Heat Function of Temperature Curve Name",
        "  0.9, !- Speed 4 Evaporative Condenser Effectiveness{ dimensionless }",
        "  0.3, !- Speed 4 Evaporative Condenser Air Flow Rate{ m3 / s }",
        " 100;                     !- Speed 4 Rated Evaporative Condenser Pump Power Consumption{ W }",
        " Curve:Biquadratic,",
        "  HPACCoolCapFT Speed, !- Name",
        "  1.0, !- Coefficient1 Constant",
        "  0.0, !- Coefficient2 x",
        "  0.0, !- Coefficient3 x**2",
        "  0.0, !- Coefficient4 y",
        "  0.0, !- Coefficient5 y**2",
        "  0.0, !- Coefficient6 x*y",
        "  12.77778, !- Minimum Value of x",
        "  23.88889, !- Maximum Value of x",
        "  23.88889, !- Minimum Value of y",
        "  46.11111, !- Maximum Value of y",
        "  , !- Minimum Curve Output",
        "  , !- Maximum Curve Output",
        "  Temperature, !- Input Unit Type for X",
        "  Temperature, !- Input Unit Type for Y",
        "  Dimensionless;           !- Output Unit Type",
        " Curve:Cubic,",
        "  HPACCoolCapFF Speed, !- Name",
        "  .47278589, !- Coefficient1 Constant",
        "  1.2433415, !- Coefficient2 x",
        "  -1.0387055, !- Coefficient3 x**2",
        "  .32257813, !- Coefficient4 x**3",
        "  0.5, !- Minimum Value of x",
        "  1.5;                   !- Maximum Value of x",
        " Curve:Biquadratic,",
        "  HPACCOOLEIRFT Speed, !- Name",
        "  0.632475E+00, !- Coefficient1 Constant",
        "  -0.121321E-01, !- Coefficient2 x",
        "  0.507773E-03, !- Coefficient3 x**2",
        "  0.155377E-01, !- Coefficient4 y",
        "  0.272840E-03, !- Coefficient5 y**2",
        "  -0.679201E-03, !- Coefficient6 x*y",
        "  12.77778, !- Minimum Value of x",
        "  23.88889, !- Maximum Value of x",
        "  23.88889, !- Minimum Value of y",
        "  46.11111, !- Maximum Value of y",
        "  , !- Minimum Curve Output",
        "  , !- Maximum Curve Output",
        "  Temperature, !- Input Unit Type for X",
        "  Temperature, !- Input Unit Type for Y",
        "  Dimensionless;           !- Output Unit Type",
        "Curve:Cubic,",
        "  HPACCOOLEIRFF Speed, !- Name",
        "  .47278589, !- Coefficient1 Constant",
        "  1.2433415, !- Coefficient2 x",
        "  -1.0387055, !- Coefficient3 x**2",
        "  .32257813, !- Coefficient4 x**3",
        "  0.5, !- Minimum Value of x",
        "  1.5;                     !- Maximum Value of x",
        "Curve:Quadratic,",
        "  HPACCOOLPLFFPLR Speed, !- Name",
        "  0.85, !- Coefficient1 Constant",
        "  0.15, !- Coefficient2 x",
        "  0.0, !- Coefficient3 x**2",
        "  0.0, !- Minimum Value of x",
        "  1.0;                     !- Maximum Value of x",
    });

    ASSERT_TRUE(process_idf(idf_objects));

    // Case 1 test
    GetDXCoils(*state);

    state->dataAirLoop->AirLoopInputsFilled = true;

    state->dataGlobal->SysSizingCalc = true;

    InitDXCoil(*state, 1);

    EXPECT_FALSE(state->dataDXCoils->DXCoil(1).ReportCoolingCoilCrankcasePower);
    // These two output variables are listed in rdd for Coil:Cooling:DX:MultiSpeed used for AC only
    EXPECT_EQ("Cooling Coil Crankcase Heater Electricity Rate", state->dataOutputProcessor->DDVariableTypes(10).VarNameOnly);
    EXPECT_EQ("Cooling Coil Crankcase Heater Electricity Energy", state->dataOutputProcessor->DDVariableTypes(11).VarNameOnly);

    state->dataGlobal->SysSizingCalc = false;
    state->dataAirLoop->AirLoopInputsFilled = false;
}

TEST_F(EnergyPlusFixture, BlankDefrostEIRCurveInput)
{

    // tests autosizing evaporatively cooled condenser pump #4802

    std::string const idf_objects = delimited_string({
        "	Schedule:Compact,",
        "	Always On,            !- Name",
        "	Fraction,             !- Schedule Type Limits Name",
        "	Through: 12/31,       !- Field 1",
        "	For: AllDays,         !- Field 2",
        "	Until: 24:00, 1.0;    !- Field 3",

        "Curve:Biquadratic,",
        "	Biquadratic,     !- Name",
        "	1.0,             !- Coefficient1 Constant",
        "	0.0,             !- Coefficient2 x",
        "	0.0,             !- Coefficient3 x**2",
        "	0.0,             !- Coefficient4 y",
        "	0.0,             !- Coefficient5 y**2",
        "	0.0,             !- Coefficient6 x*y",
        "	12.0,            !- Minimum Value of x",
        "	23.9,            !- Maximum Value of x",
        "	18.0,            !- Minimum Value of y",
        "	46.1,            !- Maximum Value of y",
        "	,                !- Minimum Curve Output",
        "	,                !- Maximum Curve Output",
        "	Temperature,     !- Input Unit Type for X",
        "	Temperature,     !- Input Unit Type for Y",
        "	Dimensionless;   !- Output Unit Type",

        "Curve:Quadratic,",
        "	Quadratic, !- Name",
        "	0.8,              !- Coefficient1 Constant",
        "	0.2,              !- Coefficient2 x",
        "	0.0,              !- Coefficient3 x**2",
        "	0.5,              !- Minimum Value of x",
        "	1.5;              !- Maximum Value of x",

        "Coil:Heating:DX:SingleSpeed,",
        "	BC Heating Coil System HC,  !- Name",
        "	Always On,               !- Availability Schedule Name",
        "	autosize,                !- Gross Rated Heating Capacity {W}",
        "	3.03,                    !- Gross Rated Heating COP {W/W}",
        "	autosize,                !- Rated Air Flow Rate {m3/s}",
        "	773.3,                   !- 2017 Rated Supply Fan Power Per Volume Flow Rate {W/(m3/s)}",
        "   934.3,                   !- 2023 Rated Supply Fan Power Per Volume Flow Rate {W/(m3/s)}", //??TBD:BPS
        "	CoilSystem_Cooling_DX 1 DX Cooling Coil System Outlet Node Name,  !- Air Inlet Node Name",
        "	CoilSystem_Heating_DX 1 Air Outlet,  !- Air Outlet Node Name",
        "	Biquadratic,             !- Heating Capacity Function of Temperature Curve Name",
        "	Quadratic,               !- Heating Capacity Function of Flow Fraction Curve Name",
        "	Biquadratic,             !- Energy Input Ratio Function of Temperature Curve Name",
        "	Quadratic,               !- Energy Input Ratio Function of Flow Fraction Curve Name",
        "	Quadratic,               !- Part Load Fraction Correlation Curve Name",
        "	Biquadratic,             !- Defrost Energy Input Ratio Function of Temperature Curve Name",
        "	-8.0,                    !- Minimum Outdoor Dry-Bulb Temperature for Compressor Operation {C}",
        "	,                        !- Outdoor Dry-Bulb Temperature to Turn On Compressor {C}",
        "	5.0,                     !- Maximum Outdoor Dry-Bulb Temperature for Defrost Operation {C}",
        "	0.0,                     !- Crankcase Heater Capacity {W}",
        "	10.0,                    !- Maximum Outdoor Dry-Bulb Temperature for Crankcase Heater Operation {C}",
        "	,                        !- Defrost Strategy",
        "	,                        !- Defrost Control",
        "	0.058333,                !- Defrost Time Period Fraction",
        "	autosize,                !- Resistive Defrost Heater Capacity {W}",
        "	4;                       !- Region number for calculating HSPF",
    });

    ASSERT_TRUE(process_idf(idf_objects));

    ProcessScheduleInput(*state);
    GetCurveInput(*state);
    GetDXCoils(*state);

    ASSERT_EQ(1, state->dataDXCoils->NumDXCoils);
    ASSERT_TRUE(compare_enums(state->dataDXCoils->DXCoil(1).DefrostStrategy, StandardRatings::DefrostStrat::ReverseCycle));
    ASSERT_TRUE(compare_enums(state->dataDXCoils->DXCoil(1).DefrostControl, StandardRatings::HPdefrostControl::Timed));
    ASSERT_EQ(state->dataDXCoils->DXCoil(1).DefrostEIRFT, 1);
    ASSERT_EQ(state->dataDXCoils->DXCoil(1).MaxOATDefrost, 5.0);
    ASSERT_EQ(state->dataDXCoils->DXCoil(1).DefrostTime, 0.058333);
}

TEST_F(EnergyPlusFixture, CurveOutputLimitWarning)
{

    // tests performance curves reports warning if rating point results is not near 1.0

    std::string const idf_objects = delimited_string({
        "	Schedule:Compact,",
        "	Always On,            !- Name",
        "	Fraction,             !- Schedule Type Limits Name",
        "	Through: 12/31,       !- Field 1",
        "	For: AllDays,         !- Field 2",
        "	Until: 24:00, 1.0;    !- Field 3",

        "Curve:Biquadratic,",
        "	Biquadratic,     !- Name",
        "	1.1001,          !- Coefficient1 Constant",
        "	0.0,             !- Coefficient2 x",
        "	0.0,             !- Coefficient3 x**2",
        "	0.0,             !- Coefficient4 y",
        "	0.0,             !- Coefficient5 y**2",
        "	0.0,             !- Coefficient6 x*y",
        "	12.0,            !- Minimum Value of x",
        "	23.9,            !- Maximum Value of x",
        "	18.0,            !- Minimum Value of y",
        "	46.1,            !- Maximum Value of y",
        "	,                !- Minimum Curve Output",
        "	,                !- Maximum Curve Output",
        "	Temperature,     !- Input Unit Type for X",
        "	Temperature,     !- Input Unit Type for Y",
        "	Dimensionless;   !- Output Unit Type",

        "Curve:Quadratic,",
        "	Quadratic, !- Name",
        "	0.8,              !- Coefficient1 Constant",
        "	0.2,              !- Coefficient2 x",
        "	0.0,              !- Coefficient3 x**2",
        "	0.5,              !- Minimum Value of x",
        "	1.5;              !- Maximum Value of x",

        "  COIL:Cooling:DX:VariableRefrigerantFlow,",
        "    TU1 VRF DX Cooling Coil, !- Name",
        "    Always On,           !- Availability Schedule Name",
        "    6600.0,                  !- Gross Rated Total Cooling Capacity {W}",
        "    0.75,                    !- Gross Rated Sensible Heat Ratio",
        "    0.500,                   !- Rated Air Flow Rate {m3/s}",
        "    Biquadratic,             !- Cooling Capacity Ratio Modifier Function of Temperature Curve Name",
        "    Quadratic,               !- Cooling Capacity Modifier Curve Function of Flow Fraction Name",
        "    TU1 Inlet Node,          !- Coil Air Inlet Node",
        "    TU1 VRF DX CCoil Outlet Node,  !- Coil Air Outlet Node",
        "    ;                        !- Name of Water Storage Tank for Condensate Collection",

    });

    ASSERT_TRUE(process_idf(idf_objects));

    ProcessScheduleInput(*state);
    GetCurveInput(*state);
    GetDXCoils(*state);

    // TODO: FIXME: Should this still have cerr output?
    // EXPECT_TRUE( has_cerr_output() ); // capacity as a function of temperature inputs will give output above 1.0 +- 10% and trip warning message

    Real64 CurveVal = CurveValue(*state, state->dataDXCoils->DXCoil(1).CCapFTemp(1), RatedInletWetBulbTemp, RatedOutdoorAirTemp);
    ASSERT_EQ(CurveVal, 1.1001); // anything over 1.1 will trip warning message for capacity as a function of temperature
}

TEST_F(EnergyPlusFixture, CoilHeatingDXSingleSpeed_MinOADBTempCompOperLimit)
{

    // tests minimum limits of Minimum Outdoor Drybulb Temperature for Compressor Operation

    std::string const idf_objects = delimited_string({

        "  Schedule:Compact,",
        "    FanAvailSched,           !- Name",
        "    Fraction,                !- Schedule Type Limits Name",
        "    Through: 12/31,          !- Field 1",
        "    For: AllDays,            !- Field 2",
        "    Until: 24:00,1.0;        !- Field 3",

        "  Curve:Cubic,",
        "    HPACHeatCapFT,           !- Name",
        "    0.758746,                !- Coefficient1 Constant",
        "    0.027626,                !- Coefficient2 x",
        "    0.000148716,             !- Coefficient3 x**2",
        "    0.0000034992,            !- Coefficient4 x**3",
        "    -20.0,                   !- Minimum Value of x",
        "    20.0,                    !- Maximum Value of x",
        "    ,                        !- Minimum Curve Output",
        "    ,                        !- Maximum Curve Output",
        "    Temperature,             !- Input Unit Type for X",
        "    Dimensionless;           !- Output Unit Type",

        "  Curve:Cubic,",
        "    HPACHeatCapFFF,          !- Name",
        "    0.84,                    !- Coefficient1 Constant",
        "    0.16,                    !- Coefficient2 x",
        "    0.0,                     !- Coefficient3 x**2",
        "    0.0,                     !- Coefficient4 x**3",
        "    0.5,                     !- Minimum Value of x",
        "    1.5;                     !- Maximum Value of x",

        "  Curve:Cubic,",
        "    HPACHeatEIRFT,           !- Name",
        "    1.19248,                 !- Coefficient1 Constant",
        "    -0.0300438,              !- Coefficient2 x",
        "    0.00103745,              !- Coefficient3 x**2",
        "    -0.000023328,            !- Coefficient4 x**3",
        "    -20.0,                   !- Minimum Value of x",
        "    20.0,                    !- Maximum Value of x",
        "    ,                        !- Minimum Curve Output",
        "    ,                        !- Maximum Curve Output",
        "    Temperature,             !- Input Unit Type for X",
        "    Dimensionless;           !- Output Unit Type",

        "  Curve:Quadratic,",
        "    HPACHeatEIRFFF,          !- Name",
        "    1.3824,                  !- Coefficient1 Constant",
        "    -0.4336,                 !- Coefficient2 x",
        "    0.0512,                  !- Coefficient3 x**2",
        "    0.0,                     !- Minimum Value of x",
        "    1.0;                     !- Maximum Value of x",

        "  Curve:Quadratic,",
        "    HPACHeatPLFFPLR,         !- Name",
        "    0.75,                    !- Coefficient1 Constant",
        "    0.25,                    !- Coefficient2 x",
        "    0.0,                     !- Coefficient3 x**2",
        "    0.0,                     !- Minimum Value of x",
        "    1.0;                     !- Maximum Value of x",

        "  Coil:Heating:DX:SingleSpeed,",
        "    Heating Coil SingleSpeed,!- Name",
        "    FanAvailSched,           !- Availability Schedule Name",
        "    Autosize,                !- Gross Rated Heating Capacity {W}",
        "    3.75,                    !- Gross Rated Heating COP {W/W}",
        "    Autosize,                !- Rated Air Flow Rate {m3/s}",
        "    ,                        !- 2017 Rated Supply Fan Power Per Volume Flow Rate {W/(m3/s)}",
        "    ,                        !- 2023 Rated Supply Fan Power Per Volume Flow Rate {W/(m3/s)}",
        "    SPACE1-1 Cooling Coil Outlet,  !- Air Inlet Node Name",
        "    SPACE1-1 Heating Coil Outlet,  !- Air Outlet Node Name",
        "    HPACHeatCapFT,           !- Heating Capacity Function of Temperature Curve Name",
        "    HPACHeatCapFFF,          !- Heating Capacity Function of Flow Fraction Curve Name",
        "    HPACHeatEIRFT,           !- Energy Input Ratio Function of Temperature Curve Name",
        "    HPACHeatEIRFFF,          !- Energy Input Ratio Function of Flow Fraction Curve Name",
        "    HPACHeatPLFFPLR,         !- Part Load Fraction Correlation Curve Name",
        "    ,                        !- Defrost Energy Input Ratio Function of Temperature Curve Name",
        "   -30.0,                    !- Minimum Outdoor Dry-Bulb Temperature for Compressor Operation {C}",
        "    ,                        !- Outdoor Dry-Bulb Temperature to Turn On Compressor {C}",
        "    5.0,                     !- Maximum Outdoor Dry-Bulb Temperature for Defrost Operation {C}",
        "    200.0,                   !- Crankcase Heater Capacity {W}",
        "    10.0,                    !- Maximum Outdoor Dry-Bulb Temperature for Crankcase Heater Operation {C}",
        "    Resistive,               !- Defrost Strategy",
        "    TIMED,                   !- Defrost Control",
        "    0.166667,                !- Defrost Time Period Fraction",
        "    Autosize;                !- Resistive Defrost Heater Capacity {W}",

    });

    ASSERT_TRUE(process_idf(idf_objects));

    ProcessScheduleInput(*state);
    GetDXCoils(*state);

    ASSERT_EQ("HEATING COIL SINGLESPEED", state->dataDXCoils->DXCoil(1).Name); // Heating Coil Single Speed
    ASSERT_EQ(-30.0, state->dataDXCoils->DXCoil(1).MinOATCompressor);          // removed the minimum limit of -20.0C
}

TEST_F(EnergyPlusFixture, CoilCoolingDXTwoSpeed_MinOADBTempCompOperLimit)
{

    // tests minimum limits of Minimum Outdoor Drybulb Temperature for Compressor Operation #6507

    std::string const idf_objects = delimited_string({

        "  Schedule:Compact,",
        "    FanAvailSched,           !- Name",
        "    Fraction,                !- Schedule Type Limits Name",
        "    Through: 12/31,          !- Field 1",
        "    For: AllDays,            !- Field 2",
        "    Until: 24:00,1.0;        !- Field 3",

        "  Curve:Biquadratic,",
        "    WindACCoolCapFT,         !- Name",
        "    0.942587793,             !- Coefficient1 Constant",
        "  0.009543347,             !- Coefficient2 x",
        "  0.000683770,             !- Coefficient3 x**2",
        "  -0.011042676,            !- Coefficient4 y",
        "  0.000005249,             !- Coefficient5 y**2",
        "  -0.000009720,            !- Coefficient6 x*y",
        "  12.77778,                !- Minimum Value of x",
        "  23.88889,                !- Maximum Value of x",
        "  23.88889,                !- Minimum Value of y",
        "  46.11111,                !- Maximum Value of y",
        "  ,                        !- Minimum Curve Output",
        "  ,                        !- Maximum Curve Output",
        "  Temperature,             !- Input Unit Type for X",
        "  Temperature,             !- Input Unit Type for Y",
        "  Dimensionless;           !- Output Unit Type",

        "  Curve:Cubic,",
        "    RATED - CCAP - FFLOW,          !- Name",
        "    0.84,                    !- Coefficient1 Constant",
        "    0.16,                    !- Coefficient2 x",
        "    0.0,                     !- Coefficient3 x**2",
        "    0.0,                     !- Coefficient4 x**3",
        "    0.5,                     !- Minimum Value of x",
        "    1.5;                     !- Maximum Value of x",

        "  Curve:Biquadratic,",
        "    WindACEIRFT,         !- Name",
        "    0.942587793,             !- Coefficient1 Constant",
        "  0.009543347,             !- Coefficient2 x",
        "  0.000683770,             !- Coefficient3 x**2",
        "  -0.011042676,            !- Coefficient4 y",
        "  0.000005249,             !- Coefficient5 y**2",
        "  -0.000009720,            !- Coefficient6 x*y",
        "  12.77778,                !- Minimum Value of x",
        "  23.88889,                !- Maximum Value of x",
        "  23.88889,                !- Minimum Value of y",
        "  46.11111,                !- Maximum Value of y",
        "  ,                        !- Minimum Curve Output",
        "  ,                        !- Maximum Curve Output",
        "  Temperature,             !- Input Unit Type for X",
        "  Temperature,             !- Input Unit Type for Y",
        "  Dimensionless;           !- Output Unit Type",

        "  Curve:Quadratic,",
        "    RATED - CEIR - FFLOW,          !- Name",
        "    1.3824,                  !- Coefficient1 Constant",
        "    -0.4336,                 !- Coefficient2 x",
        "    0.0512,                  !- Coefficient3 x**2",
        "    0.0,                     !- Minimum Value of x",
        "    1.0;                     !- Maximum Value of x",

        "  Curve:Quadratic,",
        "    WindACPLFFPLR,         !- Name",
        "    0.75,                    !- Coefficient1 Constant",
        "    0.25,                    !- Coefficient2 x",
        "    0.0,                     !- Coefficient3 x**2",
        "    0.0,                     !- Minimum Value of x",
        "    1.0;                     !- Maximum Value of x",

        "  Coil:Cooling:DX:TwoSpeed,",
        "    Main Cooling Coil 1,     !- Name",
        "    FanAvailSched,   !- Availability Schedule Name",
        "    autosize,                !- High Speed Gross Rated Total Cooling Capacity{ W }",
        "    0.8,                     !- High Speed Rated Sensible Heat Ratio",
        "    3.0,                     !- High Speed Gross Rated Cooling COP{ W / W }",
        "    autosize,                !- High Speed Rated Air Flow Rate{ m3 / s }",
        "    ,                        !- Unit Internal Static Air Pressure{ Pa }",
        "    Mixed Air Node 1,        !- Air Inlet Node Name",
        "    Main Cooling Coil 1 Outlet Node,  !- Air Outlet Node Name",
        "    WindACCoolCapFT,         !- Total Cooling Capacity Function of Temperature Curve Name",
        "    RATED - CCAP - FFLOW,        !- Total Cooling Capacity Function of Flow Fraction Curve Name",
        "    WindACEIRFT,             !- Energy Input Ratio Function of Temperature Curve Name",
        "    RATED - CEIR - FFLOW,        !- Energy Input Ratio Function of Flow Fraction Curve Name",
        "    WindACPLFFPLR,           !- Part Load Fraction Correlation Curve Name",
        "    autosize,                !- Low Speed Gross Rated Total Cooling Capacity{ W }",
        "    0.8,                     !- Low Speed Gross Rated Sensible Heat Ratio",
        "    4.2,                     !- Low Speed Gross Rated Cooling COP{ W / W }",
        "    autosize,                !- Low Speed Rated Air Flow Rate{ m3 / s }",
        "    WindACCoolCapFT,         !- Low Speed Total Cooling Capacity Function of Temperature Curve Name",
        "    WindACEIRFT,             !- Low Speed Energy Input Ratio Function of Temperature Curve Name",
        "    ;  !- Condenser Air Inlet Node Name",

    });

    ASSERT_TRUE(process_idf(idf_objects));

    ProcessScheduleInput(*state);
    GetDXCoils(*state);

    ASSERT_EQ("MAIN COOLING COIL 1", state->dataDXCoils->DXCoil(1).Name); // Cooling Coil Two Speed
    ASSERT_EQ(-25.0, state->dataDXCoils->DXCoil(1).MinOATCompressor);     // use default value at -25C
}

TEST_F(SQLiteFixture, DXCoils_TestComponentSizingOutput_TwoSpeed)
{
    state->dataSQLiteProcedures->sqlite->sqliteBegin();
    state->dataSQLiteProcedures->sqlite->createSQLiteSimulationsRecord(1, "EnergyPlus Version", "Current Time");

    std::string const idf_objects = delimited_string({

        "Schedule:Compact,",
        "  FanAvailSched,           !- Name",
        "  Fraction,                !- Schedule Type Limits Name",
        "  Through: 12/31,          !- Field 1",
        "  For: AllDays,            !- Field 2",
        "  Until: 24:00,1.0;        !- Field 3",

        "Curve:Biquadratic,",
        "  WindACCoolCapFT,         !- Name",
        "  0.942587793,             !- Coefficient1 Constant",
        "0.009543347,             !- Coefficient2 x",
        "0.000683770,             !- Coefficient3 x**2",
        "-0.011042676,            !- Coefficient4 y",
        "0.000005249,             !- Coefficient5 y**2",
        "-0.000009720,            !- Coefficient6 x*y",
        "12.77778,                !- Minimum Value of x",
        "23.88889,                !- Maximum Value of x",
        "23.88889,                !- Minimum Value of y",
        "46.11111,                !- Maximum Value of y",
        ",                        !- Minimum Curve Output",
        ",                        !- Maximum Curve Output",
        "Temperature,             !- Input Unit Type for X",
        "Temperature,             !- Input Unit Type for Y",
        "Dimensionless;           !- Output Unit Type",

        "Curve:Cubic,",
        "  RATED - CCAP - FFLOW,          !- Name",
        "  0.84,                    !- Coefficient1 Constant",
        "  0.16,                    !- Coefficient2 x",
        "  0.0,                     !- Coefficient3 x**2",
        "  0.0,                     !- Coefficient4 x**3",
        "  0.5,                     !- Minimum Value of x",
        "  1.5;                     !- Maximum Value of x",

        "Curve:Biquadratic,",
        "  WindACEIRFT,         !- Name",
        "  0.942587793,             !- Coefficient1 Constant",
        "  0.009543347,             !- Coefficient2 x",
        "  0.000683770,             !- Coefficient3 x**2",
        "  -0.011042676,            !- Coefficient4 y",
        "  0.000005249,             !- Coefficient5 y**2",
        "  -0.000009720,            !- Coefficient6 x*y",
        "  12.77778,                !- Minimum Value of x",
        "  23.88889,                !- Maximum Value of x",
        "  23.88889,                !- Minimum Value of y",
        "  46.11111,                !- Maximum Value of y",
        "  ,                        !- Minimum Curve Output",
        "  ,                        !- Maximum Curve Output",
        "  Temperature,             !- Input Unit Type for X",
        "  Temperature,             !- Input Unit Type for Y",
        "  Dimensionless;           !- Output Unit Type",

        "Curve:Quadratic,",
        "  RATED - CEIR - FFLOW,          !- Name",
        "  1.3824,                  !- Coefficient1 Constant",
        "  -0.4336,                 !- Coefficient2 x",
        "  0.0512,                  !- Coefficient3 x**2",
        "  0.0,                     !- Minimum Value of x",
        "  1.0;                     !- Maximum Value of x",

        "Curve:Quadratic,",
        "  WindACPLFFPLR,         !- Name",
        "  0.75,                    !- Coefficient1 Constant",
        "  0.25,                    !- Coefficient2 x",
        "  0.0,                     !- Coefficient3 x**2",
        "  0.0,                     !- Minimum Value of x",
        "  1.0;                     !- Maximum Value of x",

        "Coil:Cooling:DX:TwoSpeed,",
        "  Main Cooling Coil 1,     !- Name",
        "  FanAvailSched,           !- Availability Schedule Name",
        "  autosize,                !- High Speed Gross Rated Total Cooling Capacity{ W }",
        "  0.8,                     !- High Speed Rated Sensible Heat Ratio",
        "  3.0,                     !- High Speed Gross Rated Cooling COP{ W / W }",
        "  autosize,                !- High Speed Rated Air Flow Rate{ m3 / s }",
        "  ,                        !- Unit Internal Static Air Pressure{ Pa }",
        "  Mixed Air Node 1,        !- Air Inlet Node Name",
        "  Main Cooling Coil 1 Outlet Node,  !- Air Outlet Node Name",
        "  WindACCoolCapFT,         !- Total Cooling Capacity Function of Temperature Curve Name",
        "  RATED - CCAP - FFLOW,    !- Total Cooling Capacity Function of Flow Fraction Curve Name",
        "  WindACEIRFT,             !- Energy Input Ratio Function of Temperature Curve Name",
        "  RATED - CEIR - FFLOW,    !- Energy Input Ratio Function of Flow Fraction Curve Name",
        "  WindACPLFFPLR,           !- Part Load Fraction Correlation Curve Name",
        "  autosize,                !- Low Speed Gross Rated Total Cooling Capacity{ W }",
        "  0.8,                     !- Low Speed Gross Rated Sensible Heat Ratio",
        "  4.2,                     !- Low Speed Gross Rated Cooling COP{ W / W }",
        "  autosize,                !- Low Speed Rated Air Flow Rate{ m3 / s }",
        "  WindACCoolCapFT,         !- Low Speed Total Cooling Capacity Function of Temperature Curve Name",
        "  WindACEIRFT,             !- Low Speed Energy Input Ratio Function of Temperature Curve Name",
        "  Main Cooling Coil 1 Condenser Node,  !- Condenser Air Inlet Node Name",
        "  EvaporativelyCooled,     !- Condenser Type",
        "  ,                        !- Minimum Outdoor Dry-Bulb Temperature for Compressor Operation {C}",
        "  ,                        !- High Speed Evaporative Condenser Effectiveness {dimensionless}",
        "  autosize,                !- High Speed Evaporative Condenser Air Flow Rate {m3/s}",
        "  autosize,                !- High Speed Evaporative Condenser Pump Rated Power Consumption {W}",
        "  ,                        !- Low Speed Evaporative Condenser Effectiveness {dimensionless}",
        "  autosize,                !- Low Speed Evaporative Condenser Air Flow Rate {m3/s}",
        "  autosize,                !- Low Speed Evaporative Condenser Pump Rated Power Consumption {W}",
        "  ,                        !- Supply Water Storage Tank Name",
        "  ,                        !- Condensate Collection Water Storage Tank Name",
        "  200,                     !- Basin Heater Capacity {W/K}",
        "  2,                       !- Basin Heater Setpoint Temperature {C}",
        "  BasinHeaterSched;        !- Basin Heater Operating Schedule Name",
    });

    ASSERT_TRUE(process_idf(idf_objects));

    ScheduleManager::ProcessScheduleInput(*state);
    DXCoils::GetDXCoils(*state);
    EXPECT_EQ(1, state->dataDXCoils->NumDXCoils);

    state->dataSize->CurZoneEqNum = 0;
    state->dataSize->CurOASysNum = 0;
    state->dataSize->CurSysNum = 1;
    state->dataSize->FinalSysSizing.allocate(1);
    state->dataSize->FinalSysSizing(state->dataSize->CurSysNum).CoolSupTemp = 12.0;
    state->dataSize->FinalSysSizing(state->dataSize->CurSysNum).CoolSupHumRat = 0.0085;
    state->dataSize->FinalSysSizing(state->dataSize->CurSysNum).MixTempAtCoolPeak = 28.0;
    state->dataSize->FinalSysSizing(state->dataSize->CurSysNum).MixHumRatAtCoolPeak = 0.0075;
    state->dataSize->FinalSysSizing(state->dataSize->CurSysNum).DesCoolVolFlow = 1.00;
    state->dataSize->FinalSysSizing(state->dataSize->CurSysNum).DesOutAirVolFlow = 0.2;

    state->dataAirSystemsData->PrimaryAirSystems.allocate(1);
    state->dataAirSystemsData->PrimaryAirSystems(state->dataSize->CurSysNum).NumOACoolCoils = 0;
    state->dataAirSystemsData->PrimaryAirSystems(state->dataSize->CurSysNum).SupFanNum = 0;
    state->dataAirSystemsData->PrimaryAirSystems(state->dataSize->CurSysNum).RetFanNum = 0;

    state->dataSize->SysSizingRunDone = true;
    state->dataSize->SysSizInput.allocate(1);
    state->dataSize->SysSizInput(1).AirLoopNum = state->dataSize->CurSysNum;
    state->dataSize->NumSysSizInput = 1;

    state->dataEnvrn->StdBaroPress = 101325.0;
    Psychrometrics::InitializePsychRoutines(*state);

    // Need this to prevent crash in Sizers
    state->dataSize->UnitarySysEqSizing.allocate(1);
    state->dataSize->OASysEqSizing.allocate(1);

    // Fake having a parent coil setting the size
    // UnitarySysEqSizing(DXCoilNum).CoolingCapacity = true;
    state->dataSize->CurDuctType = DataHVACGlobals::Cooling;

    // We aim to test resulting values that are in this report, so request it
    // We actually don't need this because ReportSizerOutput also outputs to the "ComponentSizes" table
    // OutputReportTabular::displayEioSummary = true;

    // Setting predefined tables is needed though
    OutputReportPredefined::SetPredefinedTables(*state);

    // SizeDXCoil is the one doing the sizing AND the reporting
    DXCoils::SizeDXCoil(*state, 1);
    // Ensure we have a RatedTotCap size to begin with
    Real64 ratedTotCap = state->dataDXCoils->DXCoil(1).RatedTotCap(1);
    EXPECT_GT(ratedTotCap, 0.0);

    // High Speed Condenser Air Flow = RatedTotCap * 0.000114 m3/s/W (850 CFM/ton)
    Real64 highSpeedCondAirFlow = state->dataDXCoils->DXCoil(1).RatedTotCap(1) * 0.000114;
    EXPECT_NEAR(highSpeedCondAirFlow, state->dataDXCoils->DXCoil(1).EvapCondAirFlow(1), 0.1);

    // Low Speed Condenser Air Flow: 1/3 Total Capacity * 0.000114 m3/s/w (850 cfm/ton)
    Real64 lowSpeedCondAirFlow = state->dataDXCoils->DXCoil(1).RatedTotCap(1) * 0.000114 * 0.3333;
    EXPECT_NEAR(lowSpeedCondAirFlow, state->dataDXCoils->DXCoil(1).EvapCondAirFlow2, 0.1);

    // High Speed Condenser Pump Power = Total Capacity * 0.004266 W/W (15 W/ton)
    Real64 highSpeedCondPumpPower = state->dataDXCoils->DXCoil(1).RatedTotCap(1) * 0.004266;
    EXPECT_NEAR(highSpeedCondPumpPower, state->dataDXCoils->DXCoil(1).EvapCondPumpElecNomPower(1), 0.1);

    // Low Speed Condenser Pump Power = Total Capacity * 0.004266 W/W (15 W/ton) * 1/3
    Real64 lowSpeedCondPumpPower = state->dataDXCoils->DXCoil(1).RatedTotCap(1) * 0.004266 * 0.3333;
    EXPECT_NEAR(lowSpeedCondPumpPower, state->dataDXCoils->DXCoil(1).EvapCondPumpElecNomPower2, 0.1);

    // Write the EIO Table we need
    // We actually don't need this because ReportSizerOutput also outputs to the "ComponentSizes" table
    // OutputReportTabular::WriteEioTables();

    // Now check output tables / EIO
    const std::string compType = state->dataDXCoils->DXCoil(1).DXCoilType;
    EXPECT_EQ(compType, "Coil:Cooling:DX:TwoSpeed");
    const std::string compName = state->dataDXCoils->DXCoil(1).Name;
    EXPECT_EQ(compName, "MAIN COOLING COIL 1");

    struct TestQuery
    {
        TestQuery(std::string t_description, std::string t_units, Real64 t_value)
            : description(t_description), units(t_units), expectedValue(t_value),
              displayString("Description='" + description + "'; Units='" + units + "'"){};

        const std::string description;
        const std::string units;
        const Real64 expectedValue;
        const std::string displayString;
    };

    std::vector<TestQuery> testQueries({
        TestQuery("Design Size High Speed Gross Rated Total Cooling Capacity", "W", ratedTotCap),
        TestQuery("Design Size High Speed Evaporative Condenser Air Flow Rate", "m3/s", highSpeedCondAirFlow),
        TestQuery("Design Size Low Speed Evaporative Condenser Air Flow Rate", "m3/s", lowSpeedCondAirFlow),
        TestQuery("Design Size High Speed Evaporative Condenser Pump Rated Power Consumption", "W", highSpeedCondPumpPower),
        TestQuery("Design Size Low Speed Evaporative Condenser Pump Rated Power Consumption", "W", lowSpeedCondPumpPower),
    });

    for (auto &testQuery : testQueries) {

        std::string query("SELECT Value From ComponentSizes"
                          "  WHERE CompType = '" +
                          compType +
                          "'"
                          "  AND CompName = '" +
                          compName +
                          "'"
                          "  AND Description = '" +
                          testQuery.description + "'" + "  AND Units = '" + testQuery.units + "'");

        // execAndReturnFirstDouble returns -10000.0 if not found
        Real64 return_val = SQLiteFixture::execAndReturnFirstDouble(query);

        if (return_val < 0) {
            EXPECT_TRUE(false) << "Query returned nothing for " << testQuery.displayString;
        } else {
            EXPECT_NEAR(testQuery.expectedValue, return_val, 0.01) << "Failed for " << testQuery.displayString;
        }
    }

    state->dataSQLiteProcedures->sqlite->sqliteCommit();
}

TEST_F(SQLiteFixture, DXCoils_TestComponentSizingOutput_SingleSpeed)
{
    state->dataSQLiteProcedures->sqlite->sqliteBegin();
    state->dataSQLiteProcedures->sqlite->createSQLiteSimulationsRecord(1, "EnergyPlus Version", "Current Time");

    std::string const idf_objects = delimited_string({

        "Schedule:Compact,",
        "  FanAndCoilAvailSched, !- Name",
        "  Fraction,             !- Schedule Type Limits Name",
        "  Through: 12/31,       !- Field 1",
        "  For: AllDays,         !- Field 2",
        "  Until: 24:00, 1.0;    !- Field 3",

        "Curve:Biquadratic,",
        "  WindACCoolCapFT, !- Name",
        "  0.942587793,     !- Coefficient1 Constant",
        "  0.009543347,     !- Coefficient2 x",
        "  0.000683770,     !- Coefficient3 x**2",
        "  -0.011042676,    !- Coefficient4 y",
        "  0.000005249,     !- Coefficient5 y**2",
        "  -0.000009720,    !- Coefficient6 x*y",
        "  12.77778,        !- Minimum Value of x",
        "  23.88889,        !- Maximum Value of x",
        "  18.0,            !- Minimum Value of y",
        "  46.11111,        !- Maximum Value of y",
        "  ,                !- Minimum Curve Output",
        "  ,                !- Maximum Curve Output",
        "  Temperature,     !- Input Unit Type for X",
        "  Temperature,     !- Input Unit Type for Y",
        "  Dimensionless;   !- Output Unit Type",

        "Curve:Biquadratic,",
        "  WindACEIRFT,   !- Name",
        "  0.342414409,   !- Coefficient1 Constant",
        "  0.034885008,   !- Coefficient2 x",
        "  -0.000623700,  !- Coefficient3 x**2",
        "  0.004977216,   !- Coefficient4 y",
        "  0.000437951,   !- Coefficient5 y**2",
        "  -0.000728028,  !- Coefficient6 x*y",
        "  12.77778,      !- Minimum Value of x",
        "  23.88889,      !- Maximum Value of x",
        "  18.0,          !- Minimum Value of y",
        "  46.11111,      !- Maximum Value of y",
        "  ,              !- Minimum Curve Output",
        "  ,              !- Maximum Curve Output",
        "  Temperature,   !- Input Unit Type for X",
        "  Temperature,   !- Input Unit Type for Y",
        "  Dimensionless; !- Output Unit Type",

        "Curve:Quadratic,",
        "  WindACCoolCapFFF, !- Name",
        "  0.8,              !- Coefficient1 Constant",
        "  0.2,              !- Coefficient2 x",
        "  0.0,              !- Coefficient3 x**2",
        "  0.5,              !- Minimum Value of x",
        "  1.5;              !- Maximum Value of x",

        "Curve:Quadratic,",
        "  WindACEIRFFF, !- Name",
        "  1.1552,       !- Coefficient1 Constant",
        " -0.1808,       !- Coefficient2 x",
        "  0.0256,       !- Coefficient3 x**2",
        "  0.5,          !- Minimum Value of x",
        "  1.5;          !- Maximum Value of x",

        "Curve:Quadratic,",
        "  WindACPLFFPLR, !- Name",
        "  0.85,          !- Coefficient1 Constant",
        "  0.15,          !- Coefficient2 x",
        "  0.0,           !- Coefficient3 x**2",
        "  0.0,           !- Minimum Value of x",
        "  1.0;           !- Maximum Value of x",

        "Coil:Cooling:DX:SingleSpeed,",
        "  Furnace ACDXCoil 1,   !- Name",
        "  FanAndCoilAvailSched, !- Availability Schedule Name",
        "  autosize,             !- Gross Rated Total Cooling Capacity { W }",
        "  0.75,                 !- Gross Rated Sensible Heat Ratio",
        "  4.40,                 !- Gross Rated Cooling COP { W / W }",
        "  1.30,                 !- Rated Air Flow Rate { m3 / s }",
        "  ,                     !- 2017 Rated Evaporator Fan Power Per Volume Flow Rate {W/(m3/s)}",
        "  ,                     !- 2023 Rated Evaporator Fan Power Per Volume Flow Rate {W/(m3/s)}",
        "  DX Cooling Coil Air Inlet Node, !- Air Inlet Node Name",
        "  Heating Coil Air Inlet Node,    !- Air Outlet Node Name",
        "  WindACCoolCapFT,      !- Total Cooling Capacity Function of Temperature Curve Name",
        "  WindACCoolCapFFF,     !- Total Cooling Capacity Function of Flow Fraction Curve Name",
        "  WindACEIRFT,          !- Energy Input Ratio Function of Temperature Curve Name",
        "  WindACEIRFFF,         !- Energy Input Ratio Function of Flow Fraction Curve Name",
        "  WindACPLFFPLR,        !- Part Load Fraction Correlation Curve Name",
        "  ,                     !- Minimum Outdoor Dry-Bulb Temperature for Compressor Operation {C}",
        "  0.0,                  !- Nominal Time for Condensate Removal to Begin",
        "  0.0,                  !- Ratio of Initial Moisture Evaporation Rate and Steady State Latent Capacity",
        "  0.0,                  !- Maximum Cycling Rate",
        "  0.0,                  !- Latent Capacity Time Constant",
        "  Split TSW Cooling Coil Condenser Inlet, !- Condenser Air Inlet Node Name",
        "  EvaporativelyCooled,  !- Condenser Type",
        "  ,                     !- Evaporative Condenser Effectiveness",
        "  autosize,             !- Evaporative Condenser Air Flow Rate",
        "  autosize,             !- Evaporative Condenser Pump Rated Power Consumption",
        "  0.0,                  !- Crankcase Heater Capacity",
        "  10.0;                 !- Maximum Outdoor DryBulb Temperature for Crankcase Heater Operation",
    });

    ASSERT_TRUE(process_idf(idf_objects));

    ScheduleManager::ProcessScheduleInput(*state);
    DXCoils::GetDXCoils(*state);
    EXPECT_EQ(1, state->dataDXCoils->NumDXCoils);

    // All of this is to basically manage to get RatedTotCap to be autosized
    state->dataSize->CurZoneEqNum = 0;
    state->dataSize->CurOASysNum = 0;
    state->dataSize->CurSysNum = 1;
    state->dataSize->FinalSysSizing.allocate(1);
    state->dataSize->FinalSysSizing(state->dataSize->CurSysNum).CoolSupTemp = 12.0;
    state->dataSize->FinalSysSizing(state->dataSize->CurSysNum).CoolSupHumRat = 0.0085;
    state->dataSize->FinalSysSizing(state->dataSize->CurSysNum).MixTempAtCoolPeak = 28.0;
    state->dataSize->FinalSysSizing(state->dataSize->CurSysNum).MixHumRatAtCoolPeak = 0.0075;
    state->dataSize->FinalSysSizing(state->dataSize->CurSysNum).DesCoolVolFlow = 1.00;
    state->dataSize->FinalSysSizing(state->dataSize->CurSysNum).DesOutAirVolFlow = 0.2;

    state->dataAirSystemsData->PrimaryAirSystems.allocate(1);
    state->dataAirSystemsData->PrimaryAirSystems(state->dataSize->CurSysNum).NumOACoolCoils = 0;
    state->dataAirSystemsData->PrimaryAirSystems(state->dataSize->CurSysNum).SupFanNum = 0;
    state->dataAirSystemsData->PrimaryAirSystems(state->dataSize->CurSysNum).RetFanNum = 0;

    state->dataSize->SysSizingRunDone = true;
    state->dataSize->SysSizInput.allocate(1);
    state->dataSize->SysSizInput(1).AirLoopNum = state->dataSize->CurSysNum;
    state->dataSize->NumSysSizInput = 1;

    state->dataEnvrn->StdBaroPress = 101325.0;
    Psychrometrics::InitializePsychRoutines(*state);

    // Need this to prevent crash in Sizers
    state->dataSize->UnitarySysEqSizing.allocate(1);
    state->dataSize->OASysEqSizing.allocate(1);

    // Get into a block so that it sets the RatedTotCap
    state->dataSize->CurDuctType = DataHVACGlobals::Cooling;

    // We aim to test resulting values that are in this report, so request it
    // We actually don't need this because ReportSizerOutput also outputs to the "ComponentSizes" table
    // OutputReportTabular::displayEioSummary = true;

    // Setting predefined tables is needed though
    OutputReportPredefined::SetPredefinedTables(*state);

    // SizeDXCoil is the one doing the sizing AND the reporting
    DXCoils::SizeDXCoil(*state, 1);
    // Ensure we have a RatedTotCap size to begin with
    Real64 ratedTotCap = state->dataDXCoils->DXCoil(1).RatedTotCap(1);
    EXPECT_GT(ratedTotCap, 0.0);

    // Condenser Air Flow = RatedTotCap * 0.000114 m3/s/W (850 CFM/ton)
    Real64 condAirFlow = state->dataDXCoils->DXCoil(1).RatedTotCap(1) * 0.000114;
    EXPECT_NEAR(condAirFlow, state->dataDXCoils->DXCoil(1).EvapCondAirFlow(1), 0.1);

    // Condenser Pump Power = Total Capacity * 0.004266 W/W (15 W/ton)
    Real64 condPumpPower = state->dataDXCoils->DXCoil(1).RatedTotCap(1) * 0.004266;
    EXPECT_NEAR(condPumpPower, state->dataDXCoils->DXCoil(1).EvapCondPumpElecNomPower(1), 0.1);

    // Write the EIO Table we need
    // We actually don't need this because ReportSizerOutput also outputs to the "ComponentSizes" table
    // OutputReportTabular::WriteEioTables();

    // Now check output tables / EIO
    const std::string compType = state->dataDXCoils->DXCoil(1).DXCoilType;
    EXPECT_EQ(compType, "Coil:Cooling:DX:SingleSpeed");
    const std::string compName = state->dataDXCoils->DXCoil(1).Name;
    EXPECT_EQ(compName, "FURNACE ACDXCOIL 1");

    struct TestQuery
    {
        TestQuery(std::string t_description, std::string t_units, Real64 t_value)
            : description(t_description), units(t_units), expectedValue(t_value),
              displayString("Description='" + description + "'; Units='" + units + "'"){};

        const std::string description;
        const std::string units;
        const Real64 expectedValue;
        const std::string displayString;
    };

    std::vector<TestQuery> testQueries({
        TestQuery("Design Size Gross Rated Total Cooling Capacity", "W", ratedTotCap),
        TestQuery("Design Size Evaporative Condenser Air Flow Rate", "m3/s", condAirFlow),
        TestQuery("Design Size Evaporative Condenser Pump Rated Power Consumption", "W", condPumpPower),
    });

    for (auto &testQuery : testQueries) {

        std::string query("SELECT Value From ComponentSizes"
                          "  WHERE CompType = '" +
                          compType +
                          "'"
                          "  AND CompName = '" +
                          compName +
                          "'"
                          "  AND Description = '" +
                          testQuery.description + "'" + "  AND Units = '" + testQuery.units + "'");

        // execAndReturnFirstDouble returns -10000.0 if not found
        Real64 return_val = SQLiteFixture::execAndReturnFirstDouble(query);

        if (return_val < 0) {
            EXPECT_TRUE(false) << "Query returned nothing for " << testQuery.displayString;
        } else {
            EXPECT_NEAR(testQuery.expectedValue, return_val, 0.01) << "Failed for " << testQuery.displayString;
        }
    }

    state->dataSQLiteProcedures->sqlite->sqliteCommit();
}

TEST_F(EnergyPlusFixture, TestMultiSpeedHeatingCoilSizingOutput)
{
    // Test rated heating capacity for Coil:Heating:DX:MultiSpeed #7381

    std::string const idf_objects = delimited_string({

        " Coil:Cooling:DX:MultiSpeed,",
        "   ashp clg coil,                          !- Name",
        "   ,                                       !- Availability Schedule Name",
        "   ashp unitary system Fan - Cooling Coil Node, !- Air Inlet Node Name",
        "   ashp unitary system Cooling Coil - Heating Coil Node, !- Air Outlet Node Name",
        "   ,                                       !- Condenser Air Inlet Node Name",
        "   AirCooled,                              !- Condenser Type",
        "   ,                                       !- Minimum Outdoor Dry-Bulb Temperature for Compressor Operation {C}",
        "   ,                                       !- Supply Water Storage Tank Name",
        "   ,                                       !- Condensate Collection Water Storage Tank Name",
        "   No,                                     !- Apply Part Load Fraction to Speeds Greater than 1",
        "   No,                                     !- Apply Latent Degradation to Speeds Greater than 1",
        "   0,                                      !- Crankcase Heater Capacity {W}",
        "   10,                                     !- Maximum Outdoor Dry-Bulb Temperature for Crankcase Heater Operation {C}",
        "   0,                                      !- Basin Heater Capacity {W/K}",
        "   2,                                      !- Basin Heater Setpoint Temperature {C}",
        "   ,                                       !- Basin Heater Operating Schedule Name",
        "   Electricity,                            !- Fuel Type",
        "   2,                                      !- Number of Speeds",
        "   10128.5361851424,                       !- Speed Gross Rated Total Cooling Capacity 1 {W}",
        "   0.714668466400895,                      !- Speed Gross Rated Sensible Heat Ratio 1",
        "   4.77462180051141,                       !- Speed Gross Rated Cooling COP 1 {W/W}",
        "   0.558646076305085,                      !- Speed Rated Air Flow Rate 1 {m3/s}",
        "   773.3,                                  !- 2017 Speed 1 Rated Evaporator Fan Power Per Volume Flow Rate {W/(m3/s)}",
        "   934.4,                                  !- 2023 Speed 1 Rated Evaporator Fan Power Per Volume Flow Rate {W/(m3/s)}", //??TBD:BPS
        "   Cool-Cap-fT1,                           !- Speed Total Cooling Capacity Function of Temperature Curve Name 1",
        "   Cool-Cap-fFF1,                          !- Speed Total Cooling Capacity Function of Flow Fraction Curve Name 1",
        "   Cool-EIR-fT1,                           !- Speed Energy Input Ratio Function of Temperature Curve Name 1",
        "   Cool-EIR-fFF1,                          !- Speed Energy Input Ratio Function of Flow Fraction Curve Name 1",
        "   Cool-PLF-fPLR1,                         !- Speed Part Load Fraction Correlation Curve Name 1",
        "   1000,                                   !- Speed Nominal Time for Condensate Removal to Begin 1 {s}",
        "   1.5,                                    !- Speed Ratio of Initial Moisture Evaporation Rate and Steady State Latent Capacity 1 "
        "{dimensionless}",
        "   3,                                      !- Speed Maximum Cycling Rate 1 {cycles/hr}",
        "   45,                                     !- Speed Latent Capacity Time Constant 1 {s}",
        "   0.2,                                    !- Speed Rated Waste Heat Fraction of Power Input 1 {dimensionless}",
        "   ConstantBiquadratic 1,                  !- Speed Waste Heat Function of Temperature Curve Name 1",
        "   0.9,                                    !- Speed Evaporative Condenser Effectiveness 1 {dimensionless}",
        "   AutoSize,                               !- Speed Evaporative Condenser Air Flow Rate 1 {m3/s}",
        "   AutoSize,                               !- Speed Rated Evaporative Condenser Pump Power Consumption 1 {W}",
        "   14067.4113682534,                       !- Speed Gross Rated Total Cooling Capacity 2 {W}",
        "   0.727729571918817,                      !- Speed Gross Rated Sensible Heat Ratio 2",
        "   4.41853147094111,                       !- Speed Gross Rated Cooling COP 2 {W/W}",
        "   0.649588460819866,                      !- Speed Rated Air Flow Rate 2 {m3/s}",
        "   773.3,                                  !- 2017 Speed 2 Rated Evaporator Fan Power Per Volume Flow Rate {W/(m3/s)}",
        "   934.4,                                  !- 2023 Speed 2 Rated Evaporator Fan Power Per Volume Flow Rate {W/(m3/s)}", //??TBD:BPS
        "   Cool-Cap-fT2,                           !- Speed Total Cooling Capacity Function of Temperature Curve Name 2",
        "   Cool-Cap-fFF2,                          !- Speed Total Cooling Capacity Function of Flow Fraction Curve Name 2",
        "   Cool-EIR-fT2,                           !- Speed Energy Input Ratio Function of Temperature Curve Name 2",
        "   Cool-EIR-fFF2,                          !- Speed Energy Input Ratio Function of Flow Fraction Curve Name 2",
        "   Cool-PLF-fPLR2,                         !- Speed Part Load Fraction Correlation Curve Name 2",
        "   1000,                                   !- Speed Nominal Time for Condensate Removal to Begin 2 {s}",
        "   1.5,                                    !- Speed Ratio of Initial Moisture Evaporation Rate and Steady State Latent Capacity 2 "
        "{dimensionless}",
        "   3,                                      !- Speed Maximum Cycling Rate 2 {cycles/hr}",
        "   45,                                     !- Speed Latent Capacity Time Constant 2 {s}",
        "   0.2,                                    !- Speed Rated Waste Heat Fraction of Power Input 2 {dimensionless}",
        "   ConstantBiquadratic 1,                  !- Speed Waste Heat Function of Temperature Curve Name 2",
        "   0.9,                                    !- Speed Evaporative Condenser Effectiveness 2 {dimensionless}",
        "   AutoSize,                               !- Speed Evaporative Condenser Air Flow Rate 2 {m3/s}",
        "   AutoSize;                               !- Speed Rated Evaporative Condenser Pump Power Consumption 2 {W}",

        " Curve:Biquadratic,",
        "   Cool-Cap-fT1,                           !- Name",
        "   1.658788451,                            !- Coefficient1 Constant",
        "   -0.0834530076,                          !- Coefficient2 x",
        "   0.00342409032,                          !- Coefficient3 x**2",
        "   0.0024332436,                           !- Coefficient4 y",
        "   -4.5036e-005,                           !- Coefficient5 y**2",
        "   -0.00053367984,                         !- Coefficient6 x*y",
        "   13.88,                                  !- Minimum Value of x {BasedOnField A2}",
        "   23.88,                                  !- Maximum Value of x {BasedOnField A2}",
        "   18.33,                                  !- Minimum Value of y {BasedOnField A3}",
        "   51.66;                                  !- Maximum Value of y {BasedOnField A3}",

        " Curve:Quadratic,",
        "   Cool-Cap-fFF1,                          !- Name",
        "   0.655239515,                            !- Coefficient1 Constant",
        "   0.511655216,                            !- Coefficient2 x",
        "   -0.166894731,                           !- Coefficient3 x**2",
        "   0,                                      !- Minimum Value of x {BasedOnField A2}",
        "   2,                                      !- Maximum Value of x {BasedOnField A2}",
        "   0,                                      !- Minimum Curve Output {BasedOnField A3}",
        "   2;                                      !- Maximum Curve Output {BasedOnField A3}",

        " Curve:Biquadratic,",
        "   Cool-EIR-fT1,                           !- Name",
        "   -0.582915701,                           !- Coefficient1 Constant",
        "   0.1581006726,                           !- Coefficient2 x",
        "   -0.00439794684,                         !- Coefficient3 x**2",
        "   -0.020335122,                           !- Coefficient4 y",
        "   0.00107983368,                          !- Coefficient5 y**2",
        "   -0.0006395922,                          !- Coefficient6 x*y",
        "   13.88,                                  !- Minimum Value of x {BasedOnField A2}",
        "   23.88,                                  !- Maximum Value of x {BasedOnField A2}",
        "   18.33,                                  !- Minimum Value of y {BasedOnField A3}",
        "   51.66;                                  !- Maximum Value of y {BasedOnField A3}",

        " Curve:Quadratic,",
        "   Cool-EIR-fFF1,                          !- Name",
        "   1.639108268,                            !- Coefficient1 Constant",
        "   -0.998953996,                           !- Coefficient2 x",
        "   0.359845728,                            !- Coefficient3 x**2",
        "   0,                                      !- Minimum Value of x {BasedOnField A2}",
        "   2,                                      !- Maximum Value of x {BasedOnField A2}",
        "   0,                                      !- Minimum Curve Output {BasedOnField A3}",
        "   2;                                      !- Maximum Curve Output {BasedOnField A3}",

        " Curve:Quadratic,",
        "   Cool-PLF-fPLR1,                         !- Name",
        "   0.89,                                   !- Coefficient1 Constant",
        "   0.11,                                   !- Coefficient2 x",
        "   0,                                      !- Coefficient3 x**2",
        "   0,                                      !- Minimum Value of x {BasedOnField A2}",
        "   1,                                      !- Maximum Value of x {BasedOnField A2}",
        "   0.7,                                    !- Minimum Curve Output {BasedOnField A3}",
        "   1;                                      !- Maximum Curve Output {BasedOnField A3}",

        " Curve:Biquadratic,",
        "   ConstantBiquadratic 1,                  !- Name",
        "   1,                                      !- Coefficient1 Constant",
        "   0,                                      !- Coefficient2 x",
        "   0,                                      !- Coefficient3 x**2",
        "   0,                                      !- Coefficient4 y",
        "   0,                                      !- Coefficient5 y**2",
        "   0,                                      !- Coefficient6 x*y",
        "   -100,                                   !- Minimum Value of x {BasedOnField A2}",
        "   100,                                    !- Maximum Value of x {BasedOnField A2}",
        "   -100,                                   !- Minimum Value of y {BasedOnField A3}",
        "   100;                                    !- Maximum Value of y {BasedOnField A3}",

        " Curve:Biquadratic,",
        "   Cool-Cap-fT2,                           !- Name",
        "   1.472738138,                            !- Coefficient1 Constant",
        "   -0.0672218352,                          !- Coefficient2 x",
        "   0.0029199042,                           !- Coefficient3 x**2",
        "   5.16005999999982e-005,                  !- Coefficient4 y",
        "   -2.97756e-005,                          !- Coefficient5 y**2",
        "   -0.00035908596,                         !- Coefficient6 x*y",
        "   13.88,                                  !- Minimum Value of x {BasedOnField A2}",
        "   23.88,                                  !- Maximum Value of x {BasedOnField A2}",
        "   18.33,                                  !- Minimum Value of y {BasedOnField A3}",
        "   51.66;                                  !- Maximum Value of y {BasedOnField A3}",

        " Curve:Quadratic,",
        "   Cool-Cap-fFF2,                          !- Name",
        "   0.618281092,                            !- Coefficient1 Constant",
        "   0.569060264,                            !- Coefficient2 x",
        "   -0.187341356,                           !- Coefficient3 x**2",
        "   0,                                      !- Minimum Value of x {BasedOnField A2}",
        "   2,                                      !- Maximum Value of x {BasedOnField A2}",
        "   0,                                      !- Minimum Curve Output {BasedOnField A3}",
        "   2;                                      !- Maximum Curve Output {BasedOnField A3}",

        " Curve:Biquadratic,",
        "   Cool-EIR-fT2,                           !- Name",
        "   -0.488195597,                           !- Coefficient1 Constant",
        "   0.0991621818,                           !- Coefficient2 x",
        "   -0.00236967444,                         !- Coefficient3 x**2",
        "   0.019503441,                            !- Coefficient4 y",
        "   0.0004297698,                           !- Coefficient5 y**2",
        "   -0.00109743984,                         !- Coefficient6 x*y",
        "   13.88,                                  !- Minimum Value of x {BasedOnField A2}",
        "   23.88,                                  !- Maximum Value of x {BasedOnField A2}",
        "   18.33,                                  !- Minimum Value of y {BasedOnField A3}",
        "   51.66;                                  !- Maximum Value of y {BasedOnField A3}",

        " Curve:Quadratic,",
        "   Cool-EIR-fFF2,                          !- Name",
        "   1.570774717,                            !- Coefficient1 Constant",
        "   -0.914152018,                           !- Coefficient2 x",
        "   0.343377302,                            !- Coefficient3 x**2",
        "   0,                                      !- Minimum Value of x {BasedOnField A2}",
        "   2,                                      !- Maximum Value of x {BasedOnField A2}",
        "   0,                                      !- Minimum Curve Output {BasedOnField A3}",
        "   2;                                      !- Maximum Curve Output {BasedOnField A3}",

        " Curve:Quadratic,",
        "   Cool-PLF-fPLR2,                         !- Name",
        "   0.89,                                   !- Coefficient1 Constant",
        "   0.11,                                   !- Coefficient2 x",
        "   0,                                      !- Coefficient3 x**2",
        "   0,                                      !- Minimum Value of x {BasedOnField A2}",
        "   1,                                      !- Maximum Value of x {BasedOnField A2}",
        "   0.7,                                    !- Minimum Curve Output {BasedOnField A3}",
        "   1;                                      !- Maximum Curve Output {BasedOnField A3}",

        " Coil:Heating:DX:MultiSpeed,",
        "   ashp htg coil,                          !- Name",
        "   ,                                       !- Availability Schedule Name",
        "   ashp unitary system Cooling Coil - Heating Coil Node, !- Air Inlet Node Name",
        "   ashp unitary system Heating Coil - Supplemental Coil Node, !- Air Outlet Node Name",
        "   -17.7777777777778,                      !- Minimum Outdoor Dry-Bulb Temperature for Compressor Operation {C}",
        "   ,                                       !- Outdoor Dry-Bulb Temperature to Turn On Compressor {C}",
        "   50,                                     !- Crankcase Heater Capacity {W}",
        "   10,                                     !- Maximum Outdoor Dry-Bulb Temperature for Crankcase Heater Operation {C}",
        "   DefrostEIR,                             !- Defrost Energy Input Ratio Function of Temperature Curve Name",
        "   4.44444444444444,                       !- Maximum Outdoor Dry-Bulb Temperature for Defrost Operation {C}",
        "   ReverseCycle,                           !- Defrost Strategy",
        "   OnDemand,                               !- Defrost Control",
        "   0.058333,                               !- Defrost Time Period Fraction",
        "   AutoSize,                               !- Resistive Defrost Heater Capacity {W}",
        "   No,                                     !- Apply Part Load Fraction to Speeds Greater than 1",
        "   Electricity,                            !- Fuel Type",
        "   4,                                      !- Region number for Calculating HSPF",
        "   2,                                      !- Number of Speeds",
        "   10128.5361851424,                       !- Speed Gross Rated Heating Capacity 1 {W}",
        "   4.4518131589158,                        !- Speed Gross Rated Heating COP 1 {W/W}",
        "   0.531903646383625,                      !- Speed Rated Air Flow Rate 1 {m3/s}",
        "   773.3,                                  !- 2017 Speed 1 Rated Supply Air Fan Power Per Volume Flow Rate {W/(m3/s)}",
        "   934.3,                                  !- 2023 Speed 1 Rated Supply Air Fan Power Per Volume Flow Rate {W/(m3/s)}", //??
        "   HP_Heat-Cap-fT1,                        !- Speed Heating Capacity Function of Temperature Curve Name 1",
        "   HP_Heat-CAP-fFF1,                       !- Speed Heating Capacity Function of Flow Fraction Curve Name 1",
        "   HP_Heat-EIR-fT1,                        !- Speed Energy Input Ratio Function of Temperature Curve Name 1",
        "   HP_Heat-EIR-fFF1,                       !- Speed Energy Input Ratio Function of Flow Fraction Curve Name 1",
        "   HP_Heat-PLF-fPLR1,                      !- Speed Part Load Fraction Correlation Curve Name 1",
        "   0.2,                                    !- Speed Rated Waste Heat Fraction of Power Input 1 {dimensionless}",
        "   ConstantBiquadratic,                    !- Speed Waste Heat Function of Temperature Curve Name 1",
        "   14067.4113682534,                       !- Speed Gross Rated Heating Capacity 2 {W}",
        "   3.9871749697327,                        !- Speed Gross Rated Heating COP 2 {W/W}",
        "   0.664879557979531,                      !- Speed Rated Air Flow Rate 2 {m3/s}",
        "   773.3,                                  !- 2017 Speed 2 Rated Supply Air Fan Power Per Volume Flow Rate {W/(m3/s)}",
        "   934.3,                                  !- 2023 Speed 2 Rated Supply Air Fan Power Per Volume Flow Rate {W/(m3/s)}",
        "   HP_Heat-Cap-fT2,                        !- Speed Heating Capacity Function of Temperature Curve Name 2",
        "   HP_Heat-CAP-fFF2,                       !- Speed Heating Capacity Function of Flow Fraction Curve Name 2",
        "   HP_Heat-EIR-fT2,                        !- Speed Energy Input Ratio Function of Temperature Curve Name 2",
        "   HP_Heat-EIR-fFF2,                       !- Speed Energy Input Ratio Function of Flow Fraction Curve Name 2",
        "   HP_Heat-PLF-fPLR2,                      !- Speed Part Load Fraction Correlation Curve Name 2",
        "   0.2,                                    !- Speed Rated Waste Heat Fraction of Power Input 2 {dimensionless}",
        "   ConstantBiquadratic;                    !- Speed Waste Heat Function of Temperature Curve Name 2",

        " Curve:Biquadratic,",
        "   DefrostEIR,                             !- Name",
        "   0.1528,                                 !- Coefficient1 Constant",
        "   0,                                      !- Coefficient2 x",
        "   0,                                      !- Coefficient3 x**2",
        "   0,                                      !- Coefficient4 y",
        "   0,                                      !- Coefficient5 y**2",
        "   0,                                      !- Coefficient6 x*y",
        "   -100,                                   !- Minimum Value of x {BasedOnField A2}",
        "   100,                                    !- Maximum Value of x {BasedOnField A2}",
        "   -100,                                   !- Minimum Value of y {BasedOnField A3}",
        "   100;                                    !- Maximum Value of y {BasedOnField A3}",

        " Curve:Biquadratic,",
        "   HP_Heat-Cap-fT1,                        !- Name",
        "   0.84077409,                             !- Coefficient1 Constant",
        "   -0.0014336586,                          !- Coefficient2 x",
        "   -0.000150336,                           !- Coefficient3 x**2",
        "   0.029628603,                            !- Coefficient4 y",
        "   0.000161676,                            !- Coefficient5 y**2",
        "   -2.349e-005,                            !- Coefficient6 x*y",
        "   -100,                                   !- Minimum Value of x {BasedOnField A2}",
        "   100,                                    !- Maximum Value of x {BasedOnField A2}",
        "   -100,                                   !- Minimum Value of y {BasedOnField A3}",
        "   100;                                    !- Maximum Value of y {BasedOnField A3}",

        " Curve:Quadratic,",
        "   HP_Heat-CAP-fFF1,                       !- Name",
        "   0.741466907,                            !- Coefficient1 Constant",
        "   0.378645444,                            !- Coefficient2 x",
        "   -0.119754733,                           !- Coefficient3 x**2",
        "   0,                                      !- Minimum Value of x {BasedOnField A2}",
        "   2,                                      !- Maximum Value of x {BasedOnField A2}",
        "   0,                                      !- Minimum Curve Output {BasedOnField A3}",
        "   2;                                      !- Maximum Curve Output {BasedOnField A3}",

        " Curve:Biquadratic,",
        "   HP_Heat-EIR-fT1,                        !- Name",
        "   0.539472334,                            !- Coefficient1 Constant",
        "   0.0165103146,                           !- Coefficient2 x",
        "   0.00083874528,                          !- Coefficient3 x**2",
        "   -0.00403234020000001,                   !- Coefficient4 y",
        "   0.00142404156,                          !- Coefficient5 y**2",
        "   -0.00211806252,                         !- Coefficient6 x*y",
        "   -100,                                   !- Minimum Value of x {BasedOnField A2}",
        "   100,                                    !- Maximum Value of x {BasedOnField A2}",
        "   -100,                                   !- Minimum Value of y {BasedOnField A3}",
        "   100;                                    !- Maximum Value of y {BasedOnField A3}",

        " Curve:Quadratic,",
        "   HP_Heat-EIR-fFF1,                       !- Name",
        "   2.153618211,                            !- Coefficient1 Constant",
        "   -1.737190609,                           !- Coefficient2 x",
        "   0.584269478,                            !- Coefficient3 x**2",
        "   0,                                      !- Minimum Value of x {BasedOnField A2}",
        "   2,                                      !- Maximum Value of x {BasedOnField A2}",
        "   0,                                      !- Minimum Curve Output {BasedOnField A3}",
        "   2;                                      !- Maximum Curve Output {BasedOnField A3}",

        " Curve:Quadratic,",
        "   HP_Heat-PLF-fPLR1,                      !- Name",
        "   0.89,                                   !- Coefficient1 Constant",
        "   0.11,                                   !- Coefficient2 x",
        "   0,                                      !- Coefficient3 x**2",
        "   0,                                      !- Minimum Value of x {BasedOnField A2}",
        "   1,                                      !- Maximum Value of x {BasedOnField A2}",
        "   0.7,                                    !- Minimum Curve Output {BasedOnField A3}",
        "   1;                                      !- Maximum Curve Output {BasedOnField A3}",

        " Curve:Biquadratic,",
        "   ConstantBiquadratic,                    !- Name",
        "   1,                                      !- Coefficient1 Constant",
        "   0,                                      !- Coefficient2 x",
        "   0,                                      !- Coefficient3 x**2",
        "   0,                                      !- Coefficient4 y",
        "   0,                                      !- Coefficient5 y**2",
        "   0,                                      !- Coefficient6 x*y",
        "   -100,                                   !- Minimum Value of x {BasedOnField A2}",
        "   100,                                    !- Maximum Value of x {BasedOnField A2}",
        "   -100,                                   !- Minimum Value of y {BasedOnField A3}",
        "   100;                                    !- Maximum Value of y {BasedOnField A3}",

        " Curve:Biquadratic,",
        "   HP_Heat-Cap-fT2,                        !- Name",
        "   0.831506971,                            !- Coefficient1 Constant",
        "   0.0018392166,                           !- Coefficient2 x",
        "   -0.000187596,                           !- Coefficient3 x**2",
        "   0.0266002056,                           !- Coefficient4 y",
        "   0.000191484,                            !- Coefficient5 y**2",
        "   -6.5772e-005,                           !- Coefficient6 x*y",
        "   -100,                                   !- Minimum Value of x {BasedOnField A2}",
        "   100,                                    !- Maximum Value of x {BasedOnField A2}",
        "   -100,                                   !- Minimum Value of y {BasedOnField A3}",
        "   100;                                    !- Maximum Value of y {BasedOnField A3}",

        " Curve:Quadratic,",
        "   HP_Heat-CAP-fFF2,                       !- Name",
        "   0.76634609,                             !- Coefficient1 Constant",
        "   0.32840943,                             !- Coefficient2 x",
        "   -0.094701495,                           !- Coefficient3 x**2",
        "   0,                                      !- Minimum Value of x {BasedOnField A2}",
        "   2,                                      !- Maximum Value of x {BasedOnField A2}",
        "   0,                                      !- Minimum Curve Output {BasedOnField A3}",
        "   2;                                      !- Maximum Curve Output {BasedOnField A3}",

        " Curve:Biquadratic,",
        "   HP_Heat-EIR-fT2,                        !- Name",
        "   0.787746797,                            !- Coefficient1 Constant",
        "   -0.000652314599999999,                  !- Coefficient2 x",
        "   0.00078866784,                          !- Coefficient3 x**2",
        "   -0.0023209056,                          !- Coefficient4 y",
        "   0.00074760408,                          !- Coefficient5 y**2",
        "   -0.00109173096,                         !- Coefficient6 x*y",
        "   -100,                                   !- Minimum Value of x {BasedOnField A2}",
        "   100,                                    !- Maximum Value of x {BasedOnField A2}",
        "   -100,                                   !- Minimum Value of y {BasedOnField A3}",
        "   100;                                    !- Maximum Value of y {BasedOnField A3}",

        " Curve:Quadratic,",
        "   HP_Heat-EIR-fFF2,                       !- Name",
        "   2.001041353,                            !- Coefficient1 Constant",
        "   -1.58869128,                            !- Coefficient2 x",
        "   0.587593517,                            !- Coefficient3 x**2",
        "   0,                                      !- Minimum Value of x {BasedOnField A2}",
        "   2,                                      !- Maximum Value of x {BasedOnField A2}",
        "   0,                                      !- Minimum Curve Output {BasedOnField A3}",
        "   2;                                      !- Maximum Curve Output {BasedOnField A3}",

        " Curve:Quadratic,",
        "   HP_Heat-PLF-fPLR2,                      !- Name",
        "   0.89,                                   !- Coefficient1 Constant",
        "   0.11,                                   !- Coefficient2 x",
        "   0,                                      !- Coefficient3 x**2",
        "   0,                                      !- Minimum Value of x {BasedOnField A2}",
        "   1,                                      !- Maximum Value of x {BasedOnField A2}",
        "   0.7,                                    !- Minimum Curve Output {BasedOnField A3}",
        "   1;                                      !- Maximum Curve Output {BasedOnField A3}",
    });

    ASSERT_TRUE(process_idf(idf_objects));

    // get input
    GetDXCoils(*state);
    SetPredefinedTables(*state);
    // check multi-speed DX cooling coil
    EXPECT_EQ("ASHP CLG COIL", state->dataDXCoils->DXCoil(1).Name);
    EXPECT_EQ("Coil:Cooling:DX:MultiSpeed", state->dataDXCoils->DXCoil(1).DXCoilType);
    SizeDXCoil(*state, 1);
    EXPECT_EQ(14067.4113682534, state->dataDXCoils->DXCoil(1).MSRatedTotCap(2));
    EXPECT_EQ(10128.5361851424, state->dataDXCoils->DXCoil(1).MSRatedTotCap(1));
    EXPECT_EQ(0.649588460819866, state->dataDXCoils->DXCoil(1).MSRatedAirVolFlowRate(2));
    EXPECT_EQ(0.558646076305085, state->dataDXCoils->DXCoil(1).MSRatedAirVolFlowRate(1));

    // check multi-speed DX heating coil
    EXPECT_EQ("ASHP HTG COIL", state->dataDXCoils->DXCoil(2).Name);
    EXPECT_EQ("Coil:Heating:DX:MultiSpeed", state->dataDXCoils->DXCoil(2).DXCoilType);
    SizeDXCoil(*state, 2);
    EXPECT_EQ(14067.4113682534, state->dataDXCoils->DXCoil(2).MSRatedTotCap(2));
    EXPECT_EQ(10128.5361851424, state->dataDXCoils->DXCoil(2).MSRatedTotCap(1));
    EXPECT_EQ(0.664879557979531, state->dataDXCoils->DXCoil(2).MSRatedAirVolFlowRate(2));
    EXPECT_EQ(0.531903646383625, state->dataDXCoils->DXCoil(2).MSRatedAirVolFlowRate(1));
}
TEST_F(EnergyPlusFixture, TestMultiSpeedCoolingCoilTabularReporting)
{
    // Test rated sensible cooling capacity reporting for Coil:Cooling:DX:MultiSpeed #7381

    std::string const idf_objects = delimited_string({

        " Coil:Cooling:DX:MultiSpeed,",
        "   ashp clg coil,                          !- Name",
        "   ,                                       !- Availability Schedule Name",
        "   ashp unitary system Fan - Cooling Coil Node, !- Air Inlet Node Name",
        "   ashp unitary system Cooling Coil - Heating Coil Node, !- Air Outlet Node Name",
        "   ,                                       !- Condenser Air Inlet Node Name",
        "   AirCooled,                              !- Condenser Type",
        "   ,                                       !- Minimum Outdoor Dry-Bulb Temperature for Compressor Operation {C}",
        "   ,                                       !- Supply Water Storage Tank Name",
        "   ,                                       !- Condensate Collection Water Storage Tank Name",
        "   No,                                     !- Apply Part Load Fraction to Speeds Greater than 1",
        "   No,                                     !- Apply Latent Degradation to Speeds Greater than 1",
        "   0,                                      !- Crankcase Heater Capacity {W}",
        "   10,                                     !- Maximum Outdoor Dry-Bulb Temperature for Crankcase Heater Operation {C}",
        "   0,                                      !- Basin Heater Capacity {W/K}",
        "   2,                                      !- Basin Heater Setpoint Temperature {C}",
        "   ,                                       !- Basin Heater Operating Schedule Name",
        "   Electricity,                            !- Fuel Type",
        "   2,                                      !- Number of Speeds",
        "   10128.5361851424,                       !- Speed Gross Rated Total Cooling Capacity 1 {W}",
        "   0.714668466400895,                      !- Speed Gross Rated Sensible Heat Ratio 1",
        "   4.77462180051141,                       !- Speed Gross Rated Cooling COP 1 {W/W}",
        "   0.558646076305085,                      !- Speed Rated Air Flow Rate 1 {m3/s}",
        "   773.3,                                  !- 2017 Speed 1 Rated Evaporator Fan Power Per Volume Flow Rate {W/(m3/s)}",
        "   934.4,                                  !- 2023 Speed 1 Rated Evaporator Fan Power Per Volume Flow Rate {W/(m3/s)}", //??TBD:BPS
        "   Cool-Cap-fT1,                           !- Speed Total Cooling Capacity Function of Temperature Curve Name 1",
        "   Cool-Cap-fFF1,                          !- Speed Total Cooling Capacity Function of Flow Fraction Curve Name 1",
        "   Cool-EIR-fT1,                           !- Speed Energy Input Ratio Function of Temperature Curve Name 1",
        "   Cool-EIR-fFF1,                          !- Speed Energy Input Ratio Function of Flow Fraction Curve Name 1",
        "   Cool-PLF-fPLR1,                         !- Speed Part Load Fraction Correlation Curve Name 1",
        "   1000,                                   !- Speed Nominal Time for Condensate Removal to Begin 1 {s}",
        "   1.5,                                    !- Speed Ratio of Initial Moisture Evaporation Rate and Steady State Latent Capacity 1 "
        "{dimensionless}",
        "   3,                                      !- Speed Maximum Cycling Rate 1 {cycles/hr}",
        "   45,                                     !- Speed Latent Capacity Time Constant 1 {s}",
        "   0.2,                                    !- Speed Rated Waste Heat Fraction of Power Input 1 {dimensionless}",
        "   ConstantBiquadratic 1,                  !- Speed Waste Heat Function of Temperature Curve Name 1",
        "   0.9,                                    !- Speed Evaporative Condenser Effectiveness 1 {dimensionless}",
        "   AutoSize,                               !- Speed Evaporative Condenser Air Flow Rate 1 {m3/s}",
        "   AutoSize,                               !- Speed Rated Evaporative Condenser Pump Power Consumption 1 {W}",
        "   14067.4113682534,                       !- Speed Gross Rated Total Cooling Capacity 2 {W}",
        "   0.727729571918817,                      !- Speed Gross Rated Sensible Heat Ratio 2",
        "   4.41853147094111,                       !- Speed Gross Rated Cooling COP 2 {W/W}",
        "   0.649588460819866,                      !- Speed Rated Air Flow Rate 2 {m3/s}",
        "   773.3,                                  !- 2017 Speed 2 Rated Evaporator Fan Power Per Volume Flow Rate 2 {W/(m3/s)}",
        "   934.4,                                  !- 2023 Speed 2 Rated Evaporator Fan Power Per Volume Flow Rate 2 {W/(m3/s)}", //??TBD:BPS
        "   Cool-Cap-fT2,                           !- Speed Total Cooling Capacity Function of Temperature Curve Name 2",
        "   Cool-Cap-fFF2,                          !- Speed Total Cooling Capacity Function of Flow Fraction Curve Name 2",
        "   Cool-EIR-fT2,                           !- Speed Energy Input Ratio Function of Temperature Curve Name 2",
        "   Cool-EIR-fFF2,                          !- Speed Energy Input Ratio Function of Flow Fraction Curve Name 2",
        "   Cool-PLF-fPLR2,                         !- Speed Part Load Fraction Correlation Curve Name 2",
        "   1000,                                   !- Speed Nominal Time for Condensate Removal to Begin 2 {s}",
        "   1.5,                                    !- Speed Ratio of Initial Moisture Evaporation Rate and Steady State Latent Capacity 2 "
        "{dimensionless}",
        "   3,                                      !- Speed Maximum Cycling Rate 2 {cycles/hr}",
        "   45,                                     !- Speed Latent Capacity Time Constant 2 {s}",
        "   0.2,                                    !- Speed Rated Waste Heat Fraction of Power Input 2 {dimensionless}",
        "   ConstantBiquadratic 1,                  !- Speed Waste Heat Function of Temperature Curve Name 2",
        "   0.9,                                    !- Speed Evaporative Condenser Effectiveness 2 {dimensionless}",
        "   AutoSize,                               !- Speed Evaporative Condenser Air Flow Rate 2 {m3/s}",
        "   AutoSize;                               !- Speed Rated Evaporative Condenser Pump Power Consumption 2 {W}",

        " Curve:Biquadratic,",
        "   Cool-Cap-fT1,                           !- Name",
        "   1.658788451,                            !- Coefficient1 Constant",
        "   -0.0834530076,                          !- Coefficient2 x",
        "   0.00342409032,                          !- Coefficient3 x**2",
        "   0.0024332436,                           !- Coefficient4 y",
        "   -4.5036e-005,                           !- Coefficient5 y**2",
        "   -0.00053367984,                         !- Coefficient6 x*y",
        "   13.88,                                  !- Minimum Value of x {BasedOnField A2}",
        "   23.88,                                  !- Maximum Value of x {BasedOnField A2}",
        "   18.33,                                  !- Minimum Value of y {BasedOnField A3}",
        "   51.66;                                  !- Maximum Value of y {BasedOnField A3}",

        " Curve:Quadratic,",
        "   Cool-Cap-fFF1,                          !- Name",
        "   0.655239515,                            !- Coefficient1 Constant",
        "   0.511655216,                            !- Coefficient2 x",
        "   -0.166894731,                           !- Coefficient3 x**2",
        "   0,                                      !- Minimum Value of x {BasedOnField A2}",
        "   2,                                      !- Maximum Value of x {BasedOnField A2}",
        "   0,                                      !- Minimum Curve Output {BasedOnField A3}",
        "   2;                                      !- Maximum Curve Output {BasedOnField A3}",

        " Curve:Biquadratic,",
        "   Cool-EIR-fT1,                           !- Name",
        "   -0.582915701,                           !- Coefficient1 Constant",
        "   0.1581006726,                           !- Coefficient2 x",
        "   -0.00439794684,                         !- Coefficient3 x**2",
        "   -0.020335122,                           !- Coefficient4 y",
        "   0.00107983368,                          !- Coefficient5 y**2",
        "   -0.0006395922,                          !- Coefficient6 x*y",
        "   13.88,                                  !- Minimum Value of x {BasedOnField A2}",
        "   23.88,                                  !- Maximum Value of x {BasedOnField A2}",
        "   18.33,                                  !- Minimum Value of y {BasedOnField A3}",
        "   51.66;                                  !- Maximum Value of y {BasedOnField A3}",

        " Curve:Quadratic,",
        "   Cool-EIR-fFF1,                          !- Name",
        "   1.639108268,                            !- Coefficient1 Constant",
        "   -0.998953996,                           !- Coefficient2 x",
        "   0.359845728,                            !- Coefficient3 x**2",
        "   0,                                      !- Minimum Value of x {BasedOnField A2}",
        "   2,                                      !- Maximum Value of x {BasedOnField A2}",
        "   0,                                      !- Minimum Curve Output {BasedOnField A3}",
        "   2;                                      !- Maximum Curve Output {BasedOnField A3}",

        " Curve:Quadratic,",
        "   Cool-PLF-fPLR1,                         !- Name",
        "   0.89,                                   !- Coefficient1 Constant",
        "   0.11,                                   !- Coefficient2 x",
        "   0,                                      !- Coefficient3 x**2",
        "   0,                                      !- Minimum Value of x {BasedOnField A2}",
        "   1,                                      !- Maximum Value of x {BasedOnField A2}",
        "   0.7,                                    !- Minimum Curve Output {BasedOnField A3}",
        "   1;                                      !- Maximum Curve Output {BasedOnField A3}",

        " Curve:Biquadratic,",
        "   ConstantBiquadratic 1,                  !- Name",
        "   1,                                      !- Coefficient1 Constant",
        "   0,                                      !- Coefficient2 x",
        "   0,                                      !- Coefficient3 x**2",
        "   0,                                      !- Coefficient4 y",
        "   0,                                      !- Coefficient5 y**2",
        "   0,                                      !- Coefficient6 x*y",
        "   -100,                                   !- Minimum Value of x {BasedOnField A2}",
        "   100,                                    !- Maximum Value of x {BasedOnField A2}",
        "   -100,                                   !- Minimum Value of y {BasedOnField A3}",
        "   100;                                    !- Maximum Value of y {BasedOnField A3}",

        " Curve:Biquadratic,",
        "   Cool-Cap-fT2,                           !- Name",
        "   1.472738138,                            !- Coefficient1 Constant",
        "   -0.0672218352,                          !- Coefficient2 x",
        "   0.0029199042,                           !- Coefficient3 x**2",
        "   5.16005999999982e-005,                  !- Coefficient4 y",
        "   -2.97756e-005,                          !- Coefficient5 y**2",
        "   -0.00035908596,                         !- Coefficient6 x*y",
        "   13.88,                                  !- Minimum Value of x {BasedOnField A2}",
        "   23.88,                                  !- Maximum Value of x {BasedOnField A2}",
        "   18.33,                                  !- Minimum Value of y {BasedOnField A3}",
        "   51.66;                                  !- Maximum Value of y {BasedOnField A3}",

        " Curve:Quadratic,",
        "   Cool-Cap-fFF2,                          !- Name",
        "   0.618281092,                            !- Coefficient1 Constant",
        "   0.569060264,                            !- Coefficient2 x",
        "   -0.187341356,                           !- Coefficient3 x**2",
        "   0,                                      !- Minimum Value of x {BasedOnField A2}",
        "   2,                                      !- Maximum Value of x {BasedOnField A2}",
        "   0,                                      !- Minimum Curve Output {BasedOnField A3}",
        "   2;                                      !- Maximum Curve Output {BasedOnField A3}",

        " Curve:Biquadratic,",
        "   Cool-EIR-fT2,                           !- Name",
        "   -0.488195597,                           !- Coefficient1 Constant",
        "   0.0991621818,                           !- Coefficient2 x",
        "   -0.00236967444,                         !- Coefficient3 x**2",
        "   0.019503441,                            !- Coefficient4 y",
        "   0.0004297698,                           !- Coefficient5 y**2",
        "   -0.00109743984,                         !- Coefficient6 x*y",
        "   13.88,                                  !- Minimum Value of x {BasedOnField A2}",
        "   23.88,                                  !- Maximum Value of x {BasedOnField A2}",
        "   18.33,                                  !- Minimum Value of y {BasedOnField A3}",
        "   51.66;                                  !- Maximum Value of y {BasedOnField A3}",

        " Curve:Quadratic,",
        "   Cool-EIR-fFF2,                          !- Name",
        "   1.570774717,                            !- Coefficient1 Constant",
        "   -0.914152018,                           !- Coefficient2 x",
        "   0.343377302,                            !- Coefficient3 x**2",
        "   0,                                      !- Minimum Value of x {BasedOnField A2}",
        "   2,                                      !- Maximum Value of x {BasedOnField A2}",
        "   0,                                      !- Minimum Curve Output {BasedOnField A3}",
        "   2;                                      !- Maximum Curve Output {BasedOnField A3}",

        " Curve:Quadratic,",
        "   Cool-PLF-fPLR2,                         !- Name",
        "   0.89,                                   !- Coefficient1 Constant",
        "   0.11,                                   !- Coefficient2 x",
        "   0,                                      !- Coefficient3 x**2",
        "   0,                                      !- Minimum Value of x {BasedOnField A2}",
        "   1,                                      !- Maximum Value of x {BasedOnField A2}",
        "   0.7,                                    !- Minimum Curve Output {BasedOnField A3}",
        "   1;                                      !- Maximum Curve Output {BasedOnField A3}",
    });

    ASSERT_TRUE(process_idf(idf_objects));

    // get input
    GetDXCoils(*state);
    // Setup the predefined tables
    EnergyPlus::OutputReportPredefined::SetPredefinedTables(*state);
    // check multi-speed DX cooling coil
    EXPECT_EQ("ASHP CLG COIL", state->dataDXCoils->DXCoil(1).Name);
    EXPECT_EQ("Coil:Cooling:DX:MultiSpeed", state->dataDXCoils->DXCoil(1).DXCoilType);
    // coils are in an airloop
    state->dataSize->CurSysNum = 1;
    state->dataSize->UnitarySysEqSizing.allocate(state->dataSize->CurSysNum);
    state->dataSize->UnitarySysEqSizing(state->dataSize->CurSysNum).CoolingCapacity = false;
    state->dataSize->UnitarySysEqSizing(state->dataSize->CurSysNum).HeatingCapacity = false;
    // coil sizing
    SizeDXCoil(*state, 1);
    EXPECT_EQ(14067.4113682534, state->dataDXCoils->DXCoil(1).MSRatedTotCap(2));
    EXPECT_EQ(10128.5361851424, state->dataDXCoils->DXCoil(1).MSRatedTotCap(1));
    EXPECT_EQ(0.649588460819866, state->dataDXCoils->DXCoil(1).MSRatedAirVolFlowRate(2));
    EXPECT_EQ(0.558646076305085, state->dataDXCoils->DXCoil(1).MSRatedAirVolFlowRate(1));
    // check multi-speed DX cooling coil rated capacity
    EXPECT_EQ(14067.4113682534, state->dataDXCoils->DXCoil(1).RatedTotCap(1));
    EXPECT_EQ(0.727729571918817, state->dataDXCoils->DXCoil(1).RatedSHR(1));
    Real64 RatedSensCapacity = state->dataDXCoils->DXCoil(1).RatedTotCap(1) * state->dataDXCoils->DXCoil(1).RatedSHR(1);
    EXPECT_EQ(10237.271253024948, RatedSensCapacity);
    // check tabular outputs
    PreDefTableEntry(*state,
                     state->dataOutRptPredefined->pdch2CoilFinalTotalCap,
                     "Coil Final Gross Total Capacity [W]",
                     state->dataDXCoils->DXCoil(1).RatedTotCap(1),
                     3);
    PreDefTableEntry(*state,
                     state->dataOutRptPredefined->pdch2CoilFinalSensCap,
                     "Coil Final Gross Sensible Capacity [W]",
                     state->dataDXCoils->DXCoil(1).RatedTotCap(1) * state->dataDXCoils->DXCoil(1).RatedSHR(1),
                     3);
    EXPECT_EQ("14067.411",
              RetrievePreDefTableEntry(*state, state->dataOutRptPredefined->pdch2CoilFinalTotalCap, "Coil Final Gross Total Capacity [W]"));
    EXPECT_EQ("10237.271",
              RetrievePreDefTableEntry(*state, state->dataOutRptPredefined->pdch2CoilFinalSensCap, "Coil Final Gross Sensible Capacity [W]"));
}

TEST_F(EnergyPlusFixture, TestMultiSpeedCoilsAutoSizingOutput)
{
    // Test rated heating capacity for Coil:Heating:DX:MultiSpeed #7477

    std::string const idf_objects = delimited_string({

        " Coil:Cooling:DX:MultiSpeed,",
        "   ashp clg coil,                          !- Name",
        "   ,                                       !- Availability Schedule Name",
        "   ashp unitary system Fan - Cooling Coil Node, !- Air Inlet Node Name",
        "   ashp unitary system Cooling Coil - Heating Coil Node, !- Air Outlet Node Name",
        "   ,                                       !- Condenser Air Inlet Node Name",
        "   AirCooled,                              !- Condenser Type",
        "   ,                                       !- Minimum Outdoor Dry-Bulb Temperature for Compressor Operation {C}",
        "   ,                                       !- Supply Water Storage Tank Name",
        "   ,                                       !- Condensate Collection Water Storage Tank Name",
        "   No,                                     !- Apply Part Load Fraction to Speeds Greater than 1",
        "   No,                                     !- Apply Latent Degradation to Speeds Greater than 1",
        "   0,                                      !- Crankcase Heater Capacity {W}",
        "   10,                                     !- Maximum Outdoor Dry-Bulb Temperature for Crankcase Heater Operation {C}",
        "   0,                                      !- Basin Heater Capacity {W/K}",
        "   2,                                      !- Basin Heater Setpoint Temperature {C}",
        "   ,                                       !- Basin Heater Operating Schedule Name",
        "   Electricity,                            !- Fuel Type",
        "   2,                                      !- Number of Speeds",
        "   AutoSize,                               !- Speed Gross Rated Total Cooling Capacity 1 {W}",
        "   AutoSize,                               !- Speed Gross Rated Sensible Heat Ratio 1",
        "   4.80,                                   !- Speed Gross Rated Cooling COP 1 {W/W}",
        "   AutoSize,                               !- Speed Rated Air Flow Rate 1 {m3/s}",
        "   773.3,                                  !- 2017 Speed 1 Rated Evaporator Fan Power Per Volume Flow Rate {W/(m3/s)}",
        "   934.4,                                  !- 2023 Speed 1 Rated Evaporator Fan Power Per Volume Flow Rate {W/(m3/s)}", //??TBD:BPS
        "   Cool-Cap-fT1,                           !- Speed Total Cooling Capacity Function of Temperature Curve Name 1",
        "   Cool-Cap-fFF1,                          !- Speed Total Cooling Capacity Function of Flow Fraction Curve Name 1",
        "   Cool-EIR-fT1,                           !- Speed Energy Input Ratio Function of Temperature Curve Name 1",
        "   Cool-EIR-fFF1,                          !- Speed Energy Input Ratio Function of Flow Fraction Curve Name 1",
        "   Cool-PLF-fPLR1,                         !- Speed Part Load Fraction Correlation Curve Name 1",
        "   1000,                                   !- Speed Nominal Time for Condensate Removal to Begin 1 {s}",
        "   1.5,                                    !- Speed Ratio of Initial Moisture Evaporation Rate and Steady State Latent Capacity 1 "
        "{dimensionless}",
        "   3,                                      !- Speed Maximum Cycling Rate 1 {cycles/hr}",
        "   45,                                     !- Speed Latent Capacity Time Constant 1 {s}",
        "   0.2,                                    !- Speed Rated Waste Heat Fraction of Power Input 1 {dimensionless}",
        "   ConstantBiquadratic 1,                  !- Speed Waste Heat Function of Temperature Curve Name 1",
        "   0.9,                                    !- Speed Evaporative Condenser Effectiveness 1 {dimensionless}",
        "   AutoSize,                               !- Speed Evaporative Condenser Air Flow Rate 1 {m3/s}",
        "   AutoSize,                               !- Speed Rated Evaporative Condenser Pump Power Consumption 1 {W}",
        "   AutoSize,                               !- Speed Gross Rated Total Cooling Capacity 2 {W}",
        "   AutoSize,                               !- Speed Gross Rated Sensible Heat Ratio 2",
        "   4.80,                                   !- Speed Gross Rated Cooling COP 2 {W/W}",
        "   AutoSize,                               !- Speed Rated Air Flow Rate 2 {m3/s}",
        "   773.3,                                  !- 2017 Speed 2 Rated Evaporator Fan Power Per Volume Flow Rate {W/(m3/s)}",
        "   934.4,                                  !- 2023 Speed 2 Rated Evaporator Fan Power Per Volume Flow Rate {W/(m3/s)}", //??TBD:BPS
        "   Cool-Cap-fT2,                           !- Speed Total Cooling Capacity Function of Temperature Curve Name 2",
        "   Cool-Cap-fFF2,                          !- Speed Total Cooling Capacity Function of Flow Fraction Curve Name 2",
        "   Cool-EIR-fT2,                           !- Speed Energy Input Ratio Function of Temperature Curve Name 2",
        "   Cool-EIR-fFF2,                          !- Speed Energy Input Ratio Function of Flow Fraction Curve Name 2",
        "   Cool-PLF-fPLR2,                         !- Speed Part Load Fraction Correlation Curve Name 2",
        "   1000,                                   !- Speed Nominal Time for Condensate Removal to Begin 2 {s}",
        "   1.5,                                    !- Speed Ratio of Initial Moisture Evaporation Rate and Steady State Latent Capacity 2 "
        "{dimensionless}",
        "   3,                                      !- Speed Maximum Cycling Rate 2 {cycles/hr}",
        "   45,                                     !- Speed Latent Capacity Time Constant 2 {s}",
        "   0.2,                                    !- Speed Rated Waste Heat Fraction of Power Input 2 {dimensionless}",
        "   ConstantBiquadratic 1,                  !- Speed Waste Heat Function of Temperature Curve Name 2",
        "   0.9,                                    !- Speed Evaporative Condenser Effectiveness 2 {dimensionless}",
        "   AutoSize,                               !- Speed Evaporative Condenser Air Flow Rate 2 {m3/s}",
        "   AutoSize;                               !- Speed Rated Evaporative Condenser Pump Power Consumption 2 {W}",

        " Curve:Biquadratic,",
        "   Cool-Cap-fT1,                           !- Name",
        "   1.658788451,                            !- Coefficient1 Constant",
        "   -0.0834530076,                          !- Coefficient2 x",
        "   0.00342409032,                          !- Coefficient3 x**2",
        "   0.0024332436,                           !- Coefficient4 y",
        "   -4.5036e-005,                           !- Coefficient5 y**2",
        "   -0.00053367984,                         !- Coefficient6 x*y",
        "   13.88,                                  !- Minimum Value of x {BasedOnField A2}",
        "   23.88,                                  !- Maximum Value of x {BasedOnField A2}",
        "   18.33,                                  !- Minimum Value of y {BasedOnField A3}",
        "   51.66;                                  !- Maximum Value of y {BasedOnField A3}",

        " Curve:Quadratic,",
        "   Cool-Cap-fFF1,                          !- Name",
        "   0.655239515,                            !- Coefficient1 Constant",
        "   0.511655216,                            !- Coefficient2 x",
        "   -0.166894731,                           !- Coefficient3 x**2",
        "   0,                                      !- Minimum Value of x {BasedOnField A2}",
        "   2,                                      !- Maximum Value of x {BasedOnField A2}",
        "   0,                                      !- Minimum Curve Output {BasedOnField A3}",
        "   2;                                      !- Maximum Curve Output {BasedOnField A3}",

        " Curve:Biquadratic,",
        "   Cool-EIR-fT1,                           !- Name",
        "   -0.582915701,                           !- Coefficient1 Constant",
        "   0.1581006726,                           !- Coefficient2 x",
        "   -0.00439794684,                         !- Coefficient3 x**2",
        "   -0.020335122,                           !- Coefficient4 y",
        "   0.00107983368,                          !- Coefficient5 y**2",
        "   -0.0006395922,                          !- Coefficient6 x*y",
        "   13.88,                                  !- Minimum Value of x {BasedOnField A2}",
        "   23.88,                                  !- Maximum Value of x {BasedOnField A2}",
        "   18.33,                                  !- Minimum Value of y {BasedOnField A3}",
        "   51.66;                                  !- Maximum Value of y {BasedOnField A3}",

        " Curve:Quadratic,",
        "   Cool-EIR-fFF1,                          !- Name",
        "   1.639108268,                            !- Coefficient1 Constant",
        "   -0.998953996,                           !- Coefficient2 x",
        "   0.359845728,                            !- Coefficient3 x**2",
        "   0,                                      !- Minimum Value of x {BasedOnField A2}",
        "   2,                                      !- Maximum Value of x {BasedOnField A2}",
        "   0,                                      !- Minimum Curve Output {BasedOnField A3}",
        "   2;                                      !- Maximum Curve Output {BasedOnField A3}",

        " Curve:Quadratic,",
        "   Cool-PLF-fPLR1,                         !- Name",
        "   0.89,                                   !- Coefficient1 Constant",
        "   0.11,                                   !- Coefficient2 x",
        "   0,                                      !- Coefficient3 x**2",
        "   0,                                      !- Minimum Value of x {BasedOnField A2}",
        "   1,                                      !- Maximum Value of x {BasedOnField A2}",
        "   0.7,                                    !- Minimum Curve Output {BasedOnField A3}",
        "   1;                                      !- Maximum Curve Output {BasedOnField A3}",

        " Curve:Biquadratic,",
        "   ConstantBiquadratic 1,                  !- Name",
        "   1,                                      !- Coefficient1 Constant",
        "   0,                                      !- Coefficient2 x",
        "   0,                                      !- Coefficient3 x**2",
        "   0,                                      !- Coefficient4 y",
        "   0,                                      !- Coefficient5 y**2",
        "   0,                                      !- Coefficient6 x*y",
        "   -100,                                   !- Minimum Value of x {BasedOnField A2}",
        "   100,                                    !- Maximum Value of x {BasedOnField A2}",
        "   -100,                                   !- Minimum Value of y {BasedOnField A3}",
        "   100;                                    !- Maximum Value of y {BasedOnField A3}",

        " Curve:Biquadratic,",
        "   Cool-Cap-fT2,                           !- Name",
        "   1.472738138,                            !- Coefficient1 Constant",
        "   -0.0672218352,                          !- Coefficient2 x",
        "   0.0029199042,                           !- Coefficient3 x**2",
        "   5.16005999999982e-005,                  !- Coefficient4 y",
        "   -2.97756e-005,                          !- Coefficient5 y**2",
        "   -0.00035908596,                         !- Coefficient6 x*y",
        "   13.88,                                  !- Minimum Value of x {BasedOnField A2}",
        "   23.88,                                  !- Maximum Value of x {BasedOnField A2}",
        "   18.33,                                  !- Minimum Value of y {BasedOnField A3}",
        "   51.66;                                  !- Maximum Value of y {BasedOnField A3}",

        " Curve:Quadratic,",
        "   Cool-Cap-fFF2,                          !- Name",
        "   0.618281092,                            !- Coefficient1 Constant",
        "   0.569060264,                            !- Coefficient2 x",
        "   -0.187341356,                           !- Coefficient3 x**2",
        "   0,                                      !- Minimum Value of x {BasedOnField A2}",
        "   2,                                      !- Maximum Value of x {BasedOnField A2}",
        "   0,                                      !- Minimum Curve Output {BasedOnField A3}",
        "   2;                                      !- Maximum Curve Output {BasedOnField A3}",

        " Curve:Biquadratic,",
        "   Cool-EIR-fT2,                           !- Name",
        "   -0.488195597,                           !- Coefficient1 Constant",
        "   0.0991621818,                           !- Coefficient2 x",
        "   -0.00236967444,                         !- Coefficient3 x**2",
        "   0.019503441,                            !- Coefficient4 y",
        "   0.0004297698,                           !- Coefficient5 y**2",
        "   -0.00109743984,                         !- Coefficient6 x*y",
        "   13.88,                                  !- Minimum Value of x {BasedOnField A2}",
        "   23.88,                                  !- Maximum Value of x {BasedOnField A2}",
        "   18.33,                                  !- Minimum Value of y {BasedOnField A3}",
        "   51.66;                                  !- Maximum Value of y {BasedOnField A3}",

        " Curve:Quadratic,",
        "   Cool-EIR-fFF2,                          !- Name",
        "   1.570774717,                            !- Coefficient1 Constant",
        "   -0.914152018,                           !- Coefficient2 x",
        "   0.343377302,                            !- Coefficient3 x**2",
        "   0,                                      !- Minimum Value of x {BasedOnField A2}",
        "   2,                                      !- Maximum Value of x {BasedOnField A2}",
        "   0,                                      !- Minimum Curve Output {BasedOnField A3}",
        "   2;                                      !- Maximum Curve Output {BasedOnField A3}",

        " Curve:Quadratic,",
        "   Cool-PLF-fPLR2,                         !- Name",
        "   0.89,                                   !- Coefficient1 Constant",
        "   0.11,                                   !- Coefficient2 x",
        "   0,                                      !- Coefficient3 x**2",
        "   0,                                      !- Minimum Value of x {BasedOnField A2}",
        "   1,                                      !- Maximum Value of x {BasedOnField A2}",
        "   0.7,                                    !- Minimum Curve Output {BasedOnField A3}",
        "   1;                                      !- Maximum Curve Output {BasedOnField A3}",

        " Coil:Heating:DX:MultiSpeed,",
        "   ashp htg coil,                          !- Name",
        "   ,                                       !- Availability Schedule Name",
        "   ashp unitary system Cooling Coil - Heating Coil Node, !- Air Inlet Node Name",
        "   ashp unitary system Heating Coil - Supplemental Coil Node, !- Air Outlet Node Name",
        "   -17.7777777777778,                      !- Minimum Outdoor Dry-Bulb Temperature for Compressor Operation {C}",
        "   ,                                       !- Outdoor Dry-Bulb Temperature to Turn On Compressor {C}",
        "   50,                                     !- Crankcase Heater Capacity {W}",
        "   10,                                     !- Maximum Outdoor Dry-Bulb Temperature for Crankcase Heater Operation {C}",
        "   DefrostEIR,                             !- Defrost Energy Input Ratio Function of Temperature Curve Name",
        "   4.44444444444444,                       !- Maximum Outdoor Dry-Bulb Temperature for Defrost Operation {C}",
        "   ReverseCycle,                           !- Defrost Strategy",
        "   OnDemand,                               !- Defrost Control",
        "   0.058333,                               !- Defrost Time Period Fraction",
        "   AutoSize,                               !- Resistive Defrost Heater Capacity {W}",
        "   No,                                     !- Apply Part Load Fraction to Speeds Greater than 1",
        "   Electricity,                            !- Fuel Type",
        "   4,                                      !- Region number for Calculating HSPF",
        "   2,                                      !- Number of Speeds",
        "   AutoSize,                               !- Speed Gross Rated Heating Capacity 1 {W}",
        "   4.60,                                   !- Speed Gross Rated Heating COP 1 {W/W}",
        "   AutoSize,                               !- Speed Rated Air Flow Rate 1 {m3/s}",
        "   773.3,                                  !- 2017 Speed 1 Rated Supply Air Fan Power Per Volume Flow Rate {W/(m3/s)}",
        "   934.3,                                  !- 2023 Speed 1 Rated Supply Air Fan Power Per Volume Flow Rate {W/(m3/s)}", //??
        "   HP_Heat-Cap-fT1,                        !- Speed Heating Capacity Function of Temperature Curve Name 1",
        "   HP_Heat-CAP-fFF1,                       !- Speed Heating Capacity Function of Flow Fraction Curve Name 1",
        "   HP_Heat-EIR-fT1,                        !- Speed Energy Input Ratio Function of Temperature Curve Name 1",
        "   HP_Heat-EIR-fFF1,                       !- Speed Energy Input Ratio Function of Flow Fraction Curve Name 1",
        "   HP_Heat-PLF-fPLR1,                      !- Speed Part Load Fraction Correlation Curve Name 1",
        "   0.2,                                    !- Speed Rated Waste Heat Fraction of Power Input 1 {dimensionless}",
        "   ConstantBiquadratic,                    !- Speed Waste Heat Function of Temperature Curve Name 1",
        "   AutoSize,                               !- Speed Gross Rated Heating Capacity 2 {W}",
        "   4.40,                                   !- Speed Gross Rated Heating COP 2 {W/W}",
        "   AutoSize,                               !- Speed Rated Air Flow Rate 2 {m3/s}",
        "   773.3,                                  !- 2017 Speed 2 Rated Supply Air Fan Power Per Volume Flow Rate {W/(m3/s)}",
        "   934.3,                                  !- 2023 Speed 2 Rated Supply Air Fan Power Per Volume Flow Rate {W/(m3/s)}", //??
        "   HP_Heat-Cap-fT2,                        !- Speed Heating Capacity Function of Temperature Curve Name 2",
        "   HP_Heat-CAP-fFF2,                       !- Speed Heating Capacity Function of Flow Fraction Curve Name 2",
        "   HP_Heat-EIR-fT2,                        !- Speed Energy Input Ratio Function of Temperature Curve Name 2",
        "   HP_Heat-EIR-fFF2,                       !- Speed Energy Input Ratio Function of Flow Fraction Curve Name 2",
        "   HP_Heat-PLF-fPLR2,                      !- Speed Part Load Fraction Correlation Curve Name 2",
        "   0.2,                                    !- Speed Rated Waste Heat Fraction of Power Input 2 {dimensionless}",
        "   ConstantBiquadratic;                    !- Speed Waste Heat Function of Temperature Curve Name 2",

        " Curve:Biquadratic,",
        "   DefrostEIR,                             !- Name",
        "   0.1528,                                 !- Coefficient1 Constant",
        "   0,                                      !- Coefficient2 x",
        "   0,                                      !- Coefficient3 x**2",
        "   0,                                      !- Coefficient4 y",
        "   0,                                      !- Coefficient5 y**2",
        "   0,                                      !- Coefficient6 x*y",
        "   -100,                                   !- Minimum Value of x {BasedOnField A2}",
        "   100,                                    !- Maximum Value of x {BasedOnField A2}",
        "   -100,                                   !- Minimum Value of y {BasedOnField A3}",
        "   100;                                    !- Maximum Value of y {BasedOnField A3}",

        " Curve:Biquadratic,",
        "   HP_Heat-Cap-fT1,                        !- Name",
        "   0.84077409,                             !- Coefficient1 Constant",
        "   -0.0014336586,                          !- Coefficient2 x",
        "   -0.000150336,                           !- Coefficient3 x**2",
        "   0.029628603,                            !- Coefficient4 y",
        "   0.000161676,                            !- Coefficient5 y**2",
        "   -2.349e-005,                            !- Coefficient6 x*y",
        "   -100,                                   !- Minimum Value of x {BasedOnField A2}",
        "   100,                                    !- Maximum Value of x {BasedOnField A2}",
        "   -100,                                   !- Minimum Value of y {BasedOnField A3}",
        "   100;                                    !- Maximum Value of y {BasedOnField A3}",

        " Curve:Quadratic,",
        "   HP_Heat-CAP-fFF1,                       !- Name",
        "   0.741466907,                            !- Coefficient1 Constant",
        "   0.378645444,                            !- Coefficient2 x",
        "   -0.119754733,                           !- Coefficient3 x**2",
        "   0,                                      !- Minimum Value of x {BasedOnField A2}",
        "   2,                                      !- Maximum Value of x {BasedOnField A2}",
        "   0,                                      !- Minimum Curve Output {BasedOnField A3}",
        "   2;                                      !- Maximum Curve Output {BasedOnField A3}",

        " Curve:Biquadratic,",
        "   HP_Heat-EIR-fT1,                        !- Name",
        "   0.539472334,                            !- Coefficient1 Constant",
        "   0.0165103146,                           !- Coefficient2 x",
        "   0.00083874528,                          !- Coefficient3 x**2",
        "   -0.00403234020000001,                   !- Coefficient4 y",
        "   0.00142404156,                          !- Coefficient5 y**2",
        "   -0.00211806252,                         !- Coefficient6 x*y",
        "   -100,                                   !- Minimum Value of x {BasedOnField A2}",
        "   100,                                    !- Maximum Value of x {BasedOnField A2}",
        "   -100,                                   !- Minimum Value of y {BasedOnField A3}",
        "   100;                                    !- Maximum Value of y {BasedOnField A3}",

        " Curve:Quadratic,",
        "   HP_Heat-EIR-fFF1,                       !- Name",
        "   2.153618211,                            !- Coefficient1 Constant",
        "   -1.737190609,                           !- Coefficient2 x",
        "   0.584269478,                            !- Coefficient3 x**2",
        "   0,                                      !- Minimum Value of x {BasedOnField A2}",
        "   2,                                      !- Maximum Value of x {BasedOnField A2}",
        "   0,                                      !- Minimum Curve Output {BasedOnField A3}",
        "   2;                                      !- Maximum Curve Output {BasedOnField A3}",

        " Curve:Quadratic,",
        "   HP_Heat-PLF-fPLR1,                      !- Name",
        "   0.89,                                   !- Coefficient1 Constant",
        "   0.11,                                   !- Coefficient2 x",
        "   0,                                      !- Coefficient3 x**2",
        "   0,                                      !- Minimum Value of x {BasedOnField A2}",
        "   1,                                      !- Maximum Value of x {BasedOnField A2}",
        "   0.7,                                    !- Minimum Curve Output {BasedOnField A3}",
        "   1;                                      !- Maximum Curve Output {BasedOnField A3}",

        " Curve:Biquadratic,",
        "   ConstantBiquadratic,                    !- Name",
        "   1,                                      !- Coefficient1 Constant",
        "   0,                                      !- Coefficient2 x",
        "   0,                                      !- Coefficient3 x**2",
        "   0,                                      !- Coefficient4 y",
        "   0,                                      !- Coefficient5 y**2",
        "   0,                                      !- Coefficient6 x*y",
        "   -100,                                   !- Minimum Value of x {BasedOnField A2}",
        "   100,                                    !- Maximum Value of x {BasedOnField A2}",
        "   -100,                                   !- Minimum Value of y {BasedOnField A3}",
        "   100;                                    !- Maximum Value of y {BasedOnField A3}",

        " Curve:Biquadratic,",
        "   HP_Heat-Cap-fT2,                        !- Name",
        "   0.831506971,                            !- Coefficient1 Constant",
        "   0.0018392166,                           !- Coefficient2 x",
        "   -0.000187596,                           !- Coefficient3 x**2",
        "   0.0266002056,                           !- Coefficient4 y",
        "   0.000191484,                            !- Coefficient5 y**2",
        "   -6.5772e-005,                           !- Coefficient6 x*y",
        "   -100,                                   !- Minimum Value of x {BasedOnField A2}",
        "   100,                                    !- Maximum Value of x {BasedOnField A2}",
        "   -100,                                   !- Minimum Value of y {BasedOnField A3}",
        "   100;                                    !- Maximum Value of y {BasedOnField A3}",

        " Curve:Quadratic,",
        "   HP_Heat-CAP-fFF2,                       !- Name",
        "   0.76634609,                             !- Coefficient1 Constant",
        "   0.32840943,                             !- Coefficient2 x",
        "   -0.094701495,                           !- Coefficient3 x**2",
        "   0,                                      !- Minimum Value of x {BasedOnField A2}",
        "   2,                                      !- Maximum Value of x {BasedOnField A2}",
        "   0,                                      !- Minimum Curve Output {BasedOnField A3}",
        "   2;                                      !- Maximum Curve Output {BasedOnField A3}",

        " Curve:Biquadratic,",
        "   HP_Heat-EIR-fT2,                        !- Name",
        "   0.787746797,                            !- Coefficient1 Constant",
        "   -0.000652314599999999,                  !- Coefficient2 x",
        "   0.00078866784,                          !- Coefficient3 x**2",
        "   -0.0023209056,                          !- Coefficient4 y",
        "   0.00074760408,                          !- Coefficient5 y**2",
        "   -0.00109173096,                         !- Coefficient6 x*y",
        "   -100,                                   !- Minimum Value of x {BasedOnField A2}",
        "   100,                                    !- Maximum Value of x {BasedOnField A2}",
        "   -100,                                   !- Minimum Value of y {BasedOnField A3}",
        "   100;                                    !- Maximum Value of y {BasedOnField A3}",

        " Curve:Quadratic,",
        "   HP_Heat-EIR-fFF2,                       !- Name",
        "   2.001041353,                            !- Coefficient1 Constant",
        "   -1.58869128,                            !- Coefficient2 x",
        "   0.587593517,                            !- Coefficient3 x**2",
        "   0,                                      !- Minimum Value of x {BasedOnField A2}",
        "   2,                                      !- Maximum Value of x {BasedOnField A2}",
        "   0,                                      !- Minimum Curve Output {BasedOnField A3}",
        "   2;                                      !- Maximum Curve Output {BasedOnField A3}",

        " Curve:Quadratic,",
        "   HP_Heat-PLF-fPLR2,                      !- Name",
        "   0.89,                                   !- Coefficient1 Constant",
        "   0.11,                                   !- Coefficient2 x",
        "   0,                                      !- Coefficient3 x**2",
        "   0,                                      !- Minimum Value of x {BasedOnField A2}",
        "   1,                                      !- Maximum Value of x {BasedOnField A2}",
        "   0.7,                                    !- Minimum Curve Output {BasedOnField A3}",
        "   1;                                      !- Maximum Curve Output {BasedOnField A3}",
    });

    ASSERT_TRUE(process_idf(idf_objects));

    // get input
    GetDXCoils(*state);
    SetPredefinedTables(*state);
    // check multi-speed DX cooling coil
    EXPECT_EQ("ASHP CLG COIL", state->dataDXCoils->DXCoil(1).Name);
    EXPECT_EQ("Coil:Cooling:DX:MultiSpeed", state->dataDXCoils->DXCoil(1).DXCoilType);

    state->dataEnvrn->StdBaroPress = 101325.0;
    state->dataEnvrn->StdRhoAir = 1.2;
    Psychrometrics::InitializePsychRoutines(*state);

    // set system sizing parameters
    state->dataSize->CurZoneEqNum = 0;
    state->dataSize->CurSysNum = 1;
    state->dataSize->FinalSysSizing.allocate(1);

    state->dataSize->SysSizingRunDone = true;
    state->dataSize->FinalSysSizing(state->dataSize->CurSysNum).DesMainVolFlow = 1.75;
    state->dataSize->FinalSysSizing(state->dataSize->CurSysNum).CoolSupTemp = 13.0;
    state->dataSize->FinalSysSizing(state->dataSize->CurSysNum).CoolSupHumRat = 0.0080;
    state->dataSize->FinalSysSizing(state->dataSize->CurSysNum).MixTempAtCoolPeak = 24.290004300002032;
    state->dataSize->FinalSysSizing(state->dataSize->CurSysNum).MixHumRatAtCoolPeak = 0.0095218208835786931;
    state->dataSize->FinalSysSizing(state->dataSize->CurSysNum).OutTempAtCoolPeak = 28.244709704058657;

    state->dataAirSystemsData->PrimaryAirSystems.allocate(1);
    state->dataAirSystemsData->PrimaryAirSystems(state->dataSize->CurSysNum).NumOACoolCoils = 0;
    state->dataAirSystemsData->PrimaryAirSystems(state->dataSize->CurSysNum).SupFanNum = 0;
    state->dataAirSystemsData->PrimaryAirSystems(state->dataSize->CurSysNum).RetFanNum = 0;

    state->dataSize->SysSizInput.allocate(1);
    state->dataSize->SysSizInput(1).AirLoopNum = state->dataSize->CurSysNum;
    state->dataSize->NumSysSizInput = 1;
    // Need this to prevent crash in Sizers
    state->dataSize->UnitarySysEqSizing.allocate(1);

    SizeDXCoil(*state, 1);
    // Design flow rate at speed 2 and speed 1
    EXPECT_EQ(1.75, state->dataDXCoils->DXCoil(1).MSRatedAirVolFlowRate(2));
    EXPECT_EQ(0.875, state->dataDXCoils->DXCoil(1).MSRatedAirVolFlowRate(2) * 0.5);
    EXPECT_EQ(0.875, state->dataDXCoils->DXCoil(1).MSRatedAirVolFlowRate(1));
    // Design Capacity at speed 2 and speed 1
    EXPECT_NEAR(32731.91, state->dataDXCoils->DXCoil(1).MSRatedTotCap(2), 0.01);
    EXPECT_NEAR(16365.95, state->dataDXCoils->DXCoil(1).MSRatedTotCap(1), 0.01);

    // check multi-speed DX heating coil
    EXPECT_EQ("ASHP HTG COIL", state->dataDXCoils->DXCoil(2).Name);
    EXPECT_EQ("Coil:Heating:DX:MultiSpeed", state->dataDXCoils->DXCoil(2).DXCoilType);
    // set companion dx cooling coil
    state->dataDXCoils->DXCoil(2).CompanionUpstreamDXCoil = 1;
    SizeDXCoil(*state, 2);
    EXPECT_EQ(1.75, state->dataDXCoils->DXCoil(2).MSRatedAirVolFlowRate(2));
    EXPECT_EQ(0.875, state->dataDXCoils->DXCoil(2).MSRatedAirVolFlowRate(2) * 0.5);
    EXPECT_EQ(0.875, state->dataDXCoils->DXCoil(2).MSRatedAirVolFlowRate(1));
    EXPECT_NEAR(32731.91, state->dataDXCoils->DXCoil(2).MSRatedTotCap(2), 0.01);
    EXPECT_NEAR(16365.95, state->dataDXCoils->DXCoil(2).MSRatedTotCap(1), 0.01);
}

TEST_F(EnergyPlusFixture, TestMultiSpeedCoolingCoilPartialAutoSizeOutput)
{
    // Test partial autosize Coil:Cooling:DX:MultiSpeed #7477

    std::string const idf_objects = delimited_string({

        " Coil:Cooling:DX:MultiSpeed,",
        "   ashp clg coil,                          !- Name",
        "   ,                                       !- Availability Schedule Name",
        "   ashp unitary system Fan - Cooling Coil Node, !- Air Inlet Node Name",
        "   ashp unitary system Cooling Coil - Heating Coil Node, !- Air Outlet Node Name",
        "   ,                                       !- Condenser Air Inlet Node Name",
        "   AirCooled,                              !- Condenser Type",
        "   ,                                       !- Minimum Outdoor Dry-Bulb Temperature for Compressor Operation {C}",
        "   ,                                       !- Supply Water Storage Tank Name",
        "   ,                                       !- Condensate Collection Water Storage Tank Name",
        "   No,                                     !- Apply Part Load Fraction to Speeds Greater than 1",
        "   No,                                     !- Apply Latent Degradation to Speeds Greater than 1",
        "   0,                                      !- Crankcase Heater Capacity {W}",
        "   10,                                     !- Maximum Outdoor Dry-Bulb Temperature for Crankcase Heater Operation {C}",
        "   0,                                      !- Basin Heater Capacity {W/K}",
        "   2,                                      !- Basin Heater Setpoint Temperature {C}",
        "   ,                                       !- Basin Heater Operating Schedule Name",
        "   Electricity,                            !- Fuel Type",
        "   2,                                      !- Number of Speeds",
        "   AutoSize,                               !- Speed Gross Rated Total Cooling Capacity 1 {W}",
        "   AutoSize,                               !- Speed Gross Rated Sensible Heat Ratio 1",
        "   4.80,                                   !- Speed Gross Rated Cooling COP 1 {W/W}",
        "   AutoSize,                               !- Speed Rated Air Flow Rate 1 {m3/s}",
        "   773.3,                                  !- 2017 Speed1 Rated Evaporator Fan Power Per Volume Flow Rate {W/(m3/s)}",
        "   934.4,                                  !- 2023 Speed 1 Rated Evaporator Fan Power Per Volume Flow Rate {W/(m3/s)}", //??TBD:BPS
        "   Cool-Cap-fT1,                           !- Speed Total Cooling Capacity Function of Temperature Curve Name 1",
        "   Cool-Cap-fFF1,                          !- Speed Total Cooling Capacity Function of Flow Fraction Curve Name 1",
        "   Cool-EIR-fT1,                           !- Speed Energy Input Ratio Function of Temperature Curve Name 1",
        "   Cool-EIR-fFF1,                          !- Speed Energy Input Ratio Function of Flow Fraction Curve Name 1",
        "   Cool-PLF-fPLR1,                         !- Speed Part Load Fraction Correlation Curve Name 1",
        "   1000,                                   !- Speed Nominal Time for Condensate Removal to Begin 1 {s}",
        "   1.5,                                    !- Speed Ratio of Initial Moisture Evaporation Rate and Steady State Latent Capacity 1 "
        "{dimensionless}",
        "   3,                                      !- Speed Maximum Cycling Rate 1 {cycles/hr}",
        "   45,                                     !- Speed Latent Capacity Time Constant 1 {s}",
        "   0.2,                                    !- Speed Rated Waste Heat Fraction of Power Input 1 {dimensionless}",
        "   ConstantBiquadratic 1,                  !- Speed Waste Heat Function of Temperature Curve Name 1",
        "   0.9,                                    !- Speed Evaporative Condenser Effectiveness 1 {dimensionless}",
        "   AutoSize,                               !- Speed Evaporative Condenser Air Flow Rate 1 {m3/s}",
        "   AutoSize,                               !- Speed Rated Evaporative Condenser Pump Power Consumption 1 {W}",
        "   AutoSize,                               !- Speed Gross Rated Total Cooling Capacity 2 {W}",
        "   AutoSize,                               !- Speed Gross Rated Sensible Heat Ratio 2",
        "   4.80,                                   !- Speed Gross Rated Cooling COP 2 {W/W}",
        "   AutoSize,                               !- Speed Rated Air Flow Rate 2 {m3/s}",
        "   773.3,                                  !- 2017 Speed 2 Rated Evaporator Fan Power Per Volume Flow Rate {W/(m3/s)}",
        "   934.4,                                  !- 2023 Speed 2 Rated Evaporator Fan Power Per Volume Flow Rate {W/(m3/s)}", //??TBD:BPS
        "   Cool-Cap-fT2,                           !- Speed Total Cooling Capacity Function of Temperature Curve Name 2",
        "   Cool-Cap-fFF2,                          !- Speed Total Cooling Capacity Function of Flow Fraction Curve Name 2",
        "   Cool-EIR-fT2,                           !- Speed Energy Input Ratio Function of Temperature Curve Name 2",
        "   Cool-EIR-fFF2,                          !- Speed Energy Input Ratio Function of Flow Fraction Curve Name 2",
        "   Cool-PLF-fPLR2,                         !- Speed Part Load Fraction Correlation Curve Name 2",
        "   1000,                                   !- Speed Nominal Time for Condensate Removal to Begin 2 {s}",
        "   1.5,                                    !- Speed Ratio of Initial Moisture Evaporation Rate and Steady State Latent Capacity 2 "
        "{dimensionless}",
        "   3,                                      !- Speed Maximum Cycling Rate 2 {cycles/hr}",
        "   45,                                     !- Speed Latent Capacity Time Constant 2 {s}",
        "   0.2,                                    !- Speed Rated Waste Heat Fraction of Power Input 2 {dimensionless}",
        "   ConstantBiquadratic 1,                  !- Speed Waste Heat Function of Temperature Curve Name 2",
        "   0.9,                                    !- Speed Evaporative Condenser Effectiveness 2 {dimensionless}",
        "   AutoSize,                               !- Speed Evaporative Condenser Air Flow Rate 2 {m3/s}",
        "   AutoSize;                               !- Speed Rated Evaporative Condenser Pump Power Consumption 2 {W}",

        " Curve:Biquadratic,",
        "   Cool-Cap-fT1,                           !- Name",
        "   1.658788451,                            !- Coefficient1 Constant",
        "   -0.0834530076,                          !- Coefficient2 x",
        "   0.00342409032,                          !- Coefficient3 x**2",
        "   0.0024332436,                           !- Coefficient4 y",
        "   -4.5036e-005,                           !- Coefficient5 y**2",
        "   -0.00053367984,                         !- Coefficient6 x*y",
        "   13.88,                                  !- Minimum Value of x {BasedOnField A2}",
        "   23.88,                                  !- Maximum Value of x {BasedOnField A2}",
        "   18.33,                                  !- Minimum Value of y {BasedOnField A3}",
        "   51.66;                                  !- Maximum Value of y {BasedOnField A3}",

        " Curve:Quadratic,",
        "   Cool-Cap-fFF1,                          !- Name",
        "   0.655239515,                            !- Coefficient1 Constant",
        "   0.511655216,                            !- Coefficient2 x",
        "   -0.166894731,                           !- Coefficient3 x**2",
        "   0,                                      !- Minimum Value of x {BasedOnField A2}",
        "   2,                                      !- Maximum Value of x {BasedOnField A2}",
        "   0,                                      !- Minimum Curve Output {BasedOnField A3}",
        "   2;                                      !- Maximum Curve Output {BasedOnField A3}",

        " Curve:Biquadratic,",
        "   Cool-EIR-fT1,                           !- Name",
        "   -0.582915701,                           !- Coefficient1 Constant",
        "   0.1581006726,                           !- Coefficient2 x",
        "   -0.00439794684,                         !- Coefficient3 x**2",
        "   -0.020335122,                           !- Coefficient4 y",
        "   0.00107983368,                          !- Coefficient5 y**2",
        "   -0.0006395922,                          !- Coefficient6 x*y",
        "   13.88,                                  !- Minimum Value of x {BasedOnField A2}",
        "   23.88,                                  !- Maximum Value of x {BasedOnField A2}",
        "   18.33,                                  !- Minimum Value of y {BasedOnField A3}",
        "   51.66;                                  !- Maximum Value of y {BasedOnField A3}",

        " Curve:Quadratic,",
        "   Cool-EIR-fFF1,                          !- Name",
        "   1.639108268,                            !- Coefficient1 Constant",
        "   -0.998953996,                           !- Coefficient2 x",
        "   0.359845728,                            !- Coefficient3 x**2",
        "   0,                                      !- Minimum Value of x {BasedOnField A2}",
        "   2,                                      !- Maximum Value of x {BasedOnField A2}",
        "   0,                                      !- Minimum Curve Output {BasedOnField A3}",
        "   2;                                      !- Maximum Curve Output {BasedOnField A3}",

        " Curve:Quadratic,",
        "   Cool-PLF-fPLR1,                         !- Name",
        "   0.89,                                   !- Coefficient1 Constant",
        "   0.11,                                   !- Coefficient2 x",
        "   0,                                      !- Coefficient3 x**2",
        "   0,                                      !- Minimum Value of x {BasedOnField A2}",
        "   1,                                      !- Maximum Value of x {BasedOnField A2}",
        "   0.7,                                    !- Minimum Curve Output {BasedOnField A3}",
        "   1;                                      !- Maximum Curve Output {BasedOnField A3}",

        " Curve:Biquadratic,",
        "   ConstantBiquadratic 1,                  !- Name",
        "   1,                                      !- Coefficient1 Constant",
        "   0,                                      !- Coefficient2 x",
        "   0,                                      !- Coefficient3 x**2",
        "   0,                                      !- Coefficient4 y",
        "   0,                                      !- Coefficient5 y**2",
        "   0,                                      !- Coefficient6 x*y",
        "   -100,                                   !- Minimum Value of x {BasedOnField A2}",
        "   100,                                    !- Maximum Value of x {BasedOnField A2}",
        "   -100,                                   !- Minimum Value of y {BasedOnField A3}",
        "   100;                                    !- Maximum Value of y {BasedOnField A3}",

        " Curve:Biquadratic,",
        "   Cool-Cap-fT2,                           !- Name",
        "   1.472738138,                            !- Coefficient1 Constant",
        "   -0.0672218352,                          !- Coefficient2 x",
        "   0.0029199042,                           !- Coefficient3 x**2",
        "   5.16005999999982e-005,                  !- Coefficient4 y",
        "   -2.97756e-005,                          !- Coefficient5 y**2",
        "   -0.00035908596,                         !- Coefficient6 x*y",
        "   13.88,                                  !- Minimum Value of x {BasedOnField A2}",
        "   23.88,                                  !- Maximum Value of x {BasedOnField A2}",
        "   18.33,                                  !- Minimum Value of y {BasedOnField A3}",
        "   51.66;                                  !- Maximum Value of y {BasedOnField A3}",

        " Curve:Quadratic,",
        "   Cool-Cap-fFF2,                          !- Name",
        "   0.618281092,                            !- Coefficient1 Constant",
        "   0.569060264,                            !- Coefficient2 x",
        "   -0.187341356,                           !- Coefficient3 x**2",
        "   0,                                      !- Minimum Value of x {BasedOnField A2}",
        "   2,                                      !- Maximum Value of x {BasedOnField A2}",
        "   0,                                      !- Minimum Curve Output {BasedOnField A3}",
        "   2;                                      !- Maximum Curve Output {BasedOnField A3}",

        " Curve:Biquadratic,",
        "   Cool-EIR-fT2,                           !- Name",
        "   -0.488195597,                           !- Coefficient1 Constant",
        "   0.0991621818,                           !- Coefficient2 x",
        "   -0.00236967444,                         !- Coefficient3 x**2",
        "   0.019503441,                            !- Coefficient4 y",
        "   0.0004297698,                           !- Coefficient5 y**2",
        "   -0.00109743984,                         !- Coefficient6 x*y",
        "   13.88,                                  !- Minimum Value of x {BasedOnField A2}",
        "   23.88,                                  !- Maximum Value of x {BasedOnField A2}",
        "   18.33,                                  !- Minimum Value of y {BasedOnField A3}",
        "   51.66;                                  !- Maximum Value of y {BasedOnField A3}",

        " Curve:Quadratic,",
        "   Cool-EIR-fFF2,                          !- Name",
        "   1.570774717,                            !- Coefficient1 Constant",
        "   -0.914152018,                           !- Coefficient2 x",
        "   0.343377302,                            !- Coefficient3 x**2",
        "   0,                                      !- Minimum Value of x {BasedOnField A2}",
        "   2,                                      !- Maximum Value of x {BasedOnField A2}",
        "   0,                                      !- Minimum Curve Output {BasedOnField A3}",
        "   2;                                      !- Maximum Curve Output {BasedOnField A3}",

        " Curve:Quadratic,",
        "   Cool-PLF-fPLR2,                         !- Name",
        "   0.89,                                   !- Coefficient1 Constant",
        "   0.11,                                   !- Coefficient2 x",
        "   0,                                      !- Coefficient3 x**2",
        "   0,                                      !- Minimum Value of x {BasedOnField A2}",
        "   1,                                      !- Maximum Value of x {BasedOnField A2}",
        "   0.7,                                    !- Minimum Curve Output {BasedOnField A3}",
        "   1;                                      !- Maximum Curve Output {BasedOnField A3}",

    });

    ASSERT_TRUE(process_idf(idf_objects));

    // get input
    GetDXCoils(*state);
    SetPredefinedTables(*state);
    // check multi-speed DX cooling coil
    EXPECT_EQ("ASHP CLG COIL", state->dataDXCoils->DXCoil(1).Name);
    EXPECT_EQ("Coil:Cooling:DX:MultiSpeed", state->dataDXCoils->DXCoil(1).DXCoilType);

    state->dataEnvrn->StdBaroPress = 101325.0;
    state->dataEnvrn->StdRhoAir = 1.2;
    Psychrometrics::InitializePsychRoutines(*state);

    // set system sizing parameters
    state->dataSize->CurZoneEqNum = 0;
    state->dataSize->CurSysNum = 1;
    state->dataSize->FinalSysSizing.allocate(1);

    state->dataSize->SysSizingRunDone = true;
    state->dataSize->FinalSysSizing(state->dataSize->CurSysNum).DesMainVolFlow = 1.75;
    state->dataSize->FinalSysSizing(state->dataSize->CurSysNum).CoolSupTemp = 13.0;
    state->dataSize->FinalSysSizing(state->dataSize->CurSysNum).CoolSupHumRat = 0.0080;
    state->dataSize->FinalSysSizing(state->dataSize->CurSysNum).MixTempAtCoolPeak = 24.290004300002032;
    state->dataSize->FinalSysSizing(state->dataSize->CurSysNum).MixHumRatAtCoolPeak = 0.0095218208835786931;
    state->dataSize->FinalSysSizing(state->dataSize->CurSysNum).OutTempAtCoolPeak = 28.244709704058657;

    state->dataAirSystemsData->PrimaryAirSystems.allocate(1);
    state->dataAirSystemsData->PrimaryAirSystems(state->dataSize->CurSysNum).NumOACoolCoils = 0;
    state->dataAirSystemsData->PrimaryAirSystems(state->dataSize->CurSysNum).SupFanNum = 0;
    state->dataAirSystemsData->PrimaryAirSystems(state->dataSize->CurSysNum).RetFanNum = 0;

    state->dataSize->SysSizInput.allocate(1);
    state->dataSize->SysSizInput(1).AirLoopNum = state->dataSize->CurSysNum;
    state->dataSize->NumSysSizInput = 1;
    // Need this to prevent crash in Sizers
    state->dataSize->UnitarySysEqSizing.allocate(1);

    // test SHR design size when all autosized
    SizeDXCoil(*state, 1);
    // Design flow rate at speed 2 and speed 1
    EXPECT_EQ(1.75, state->dataDXCoils->DXCoil(1).MSRatedAirVolFlowRate(2));
    EXPECT_EQ(0.875, state->dataDXCoils->DXCoil(1).MSRatedAirVolFlowRate(2) * 0.5);
    EXPECT_EQ(0.875, state->dataDXCoils->DXCoil(1).MSRatedAirVolFlowRate(1));
    // Design Capacity at speed 2 and speed 1
    EXPECT_NEAR(32731.91, state->dataDXCoils->DXCoil(1).MSRatedTotCapDes(2), 0.01);
    EXPECT_NEAR(32731.91, state->dataDXCoils->DXCoil(1).MSRatedTotCap(2), 0.01);
    EXPECT_NEAR(16365.95, state->dataDXCoils->DXCoil(1).MSRatedTotCap(1), 0.01);
    // Design SHR at speed 2 and speed 1
    EXPECT_NEAR(0.80038, state->dataDXCoils->DXCoil(1).MSRatedSHR(2), 0.00001);
    EXPECT_NEAR(0.80038, state->dataDXCoils->DXCoil(1).MSRatedSHR(1), 0.00001);

    // test SHR design size when partial autosizing (capacity is hardsized)
    state->dataDXCoils->DXCoil(1).MSRatedTotCap(1) = 17500.0; // DataSizing::AutoSize;
    state->dataDXCoils->DXCoil(1).MSRatedTotCap(2) = 35000.0; // DataSizing::AutoSize;

    SizeDXCoil(*state, 1);
    // Design size SHR at speed 2 and speed 1
    EXPECT_NEAR(0.80038, state->dataDXCoils->DXCoil(1).MSRatedSHR(2), 0.00001);
    EXPECT_NEAR(0.80038, state->dataDXCoils->DXCoil(1).MSRatedSHR(1), 0.00001);
    // Design Capacity at speed 2 and speed 1
    EXPECT_NEAR(32731.91, state->dataDXCoils->DXCoil(1).MSRatedTotCapDes(2), 0.01);
    EXPECT_EQ(35000.0, state->dataDXCoils->DXCoil(1).MSRatedTotCap(2));
    EXPECT_EQ(35000.0 * 0.5, state->dataDXCoils->DXCoil(1).MSRatedTotCap(1));
    // Design flow rate at speed 2 and speed 1
    EXPECT_EQ(1.75, state->dataDXCoils->DXCoil(1).MSRatedAirVolFlowRate(2));
    EXPECT_EQ(0.875, state->dataDXCoils->DXCoil(1).MSRatedAirVolFlowRate(1));
}

TEST_F(EnergyPlusFixture, DXCoils_GetDXCoilCapFTCurveIndexTest)
{
    int DXCoilNum;
    int CurveNum;

    state->dataDXCoils->NumDXCoils = 2;
    state->dataDXCoils->DXCoil.allocate(state->dataDXCoils->NumDXCoils);
    state->dataDXCoils->DXCoil(1).DXCoilType_Num = CoilDX_MultiSpeedCooling;
    state->dataDXCoils->DXCoil(1).DXCoilType = "Coil:Cooling:DX:MultiSpeed";
    state->dataDXCoils->DXCoil(2).DXCoilType_Num = CoilDX_MultiSpeedHeating;
    state->dataDXCoils->DXCoil(2).DXCoilType = "Coil:Heating:DX:MultiSpeed";

    for (DXCoilNum = 1; DXCoilNum <= 2; ++DXCoilNum) {
        state->dataDXCoils->DXCoil(DXCoilNum).NumOfSpeeds = 2;
        state->dataDXCoils->DXCoil(DXCoilNum).MSRatedTotCap.allocate(state->dataDXCoils->DXCoil(DXCoilNum).NumOfSpeeds);
        state->dataDXCoils->DXCoil(DXCoilNum).MSCCapFTemp.allocate(state->dataDXCoils->DXCoil(DXCoilNum).NumOfSpeeds);
    }

    state->dataCurveManager->NumCurves = 4;
    state->dataCurveManager->PerfCurve.allocate(state->dataCurveManager->NumCurves);

    CurveNum = 1;
    state->dataCurveManager->PerfCurve(CurveNum).Name = "HP_Cool-Cap-fT-SP1";
    state->dataCurveManager->PerfCurve(CurveNum).curveType = CurveType::BiQuadratic;
    state->dataCurveManager->PerfCurve(CurveNum).ObjectType = "Curve:Biquadratic";
    state->dataCurveManager->PerfCurve(CurveNum).InterpolationType = InterpType::EvaluateCurveToLimits;
    state->dataCurveManager->PerfCurve(CurveNum).Coeff1 = 1.658788451;
    state->dataCurveManager->PerfCurve(CurveNum).Coeff2 = -0.0834530076;
    state->dataCurveManager->PerfCurve(CurveNum).Coeff3 = 0.00342409032;
    state->dataCurveManager->PerfCurve(CurveNum).Coeff4 = 0.0024332436;
    state->dataCurveManager->PerfCurve(CurveNum).Coeff5 = -4.5036e-005;
    state->dataCurveManager->PerfCurve(CurveNum).Coeff6 = -0.00053367984;
    state->dataCurveManager->PerfCurve(CurveNum).Var1Min = 13.88;
    state->dataCurveManager->PerfCurve(CurveNum).Var1Max = 23.88;
    state->dataCurveManager->PerfCurve(CurveNum).Var2Min = 18.33;
    state->dataCurveManager->PerfCurve(CurveNum).Var2Max = 51.66;

    CurveNum = 2;
    state->dataCurveManager->PerfCurve(CurveNum).Name = "HP_Cool-Cap-fT-SP2";
    state->dataCurveManager->PerfCurve(CurveNum).curveType = CurveType::BiQuadratic;
    state->dataCurveManager->PerfCurve(CurveNum).ObjectType = "Curve:Biquadratic";
    state->dataCurveManager->PerfCurve(CurveNum).InterpolationType = InterpType::EvaluateCurveToLimits;
    state->dataCurveManager->PerfCurve(CurveNum).Coeff1 = 1.472738138;
    state->dataCurveManager->PerfCurve(CurveNum).Coeff2 = -0.0672218352;
    state->dataCurveManager->PerfCurve(CurveNum).Coeff3 = 0.0029199042;
    state->dataCurveManager->PerfCurve(CurveNum).Coeff4 = 5.16005999999982e-005;
    state->dataCurveManager->PerfCurve(CurveNum).Coeff5 = -2.97756e-005;
    state->dataCurveManager->PerfCurve(CurveNum).Coeff6 = -0.00035908596;
    state->dataCurveManager->PerfCurve(CurveNum).Var1Min = 13.88;
    state->dataCurveManager->PerfCurve(CurveNum).Var1Max = 23.88;
    state->dataCurveManager->PerfCurve(CurveNum).Var2Min = 18.33;
    state->dataCurveManager->PerfCurve(CurveNum).Var2Max = 51.66;

    CurveNum = 3;
    state->dataCurveManager->PerfCurve(CurveNum).Name = "HP_Heat-Cap-fT-SP1";
    state->dataCurveManager->PerfCurve(CurveNum).curveType = CurveType::BiQuadratic;
    state->dataCurveManager->PerfCurve(CurveNum).ObjectType = "Curve:Biquadratic";
    state->dataCurveManager->PerfCurve(CurveNum).InterpolationType = InterpType::EvaluateCurveToLimits;
    state->dataCurveManager->PerfCurve(CurveNum).Coeff1 = 0.84077409;
    state->dataCurveManager->PerfCurve(CurveNum).Coeff2 = -0.0014336586;
    state->dataCurveManager->PerfCurve(CurveNum).Coeff3 = -0.000150336;
    state->dataCurveManager->PerfCurve(CurveNum).Coeff4 = 0.029628603;
    state->dataCurveManager->PerfCurve(CurveNum).Coeff5 = 0.000161676;
    state->dataCurveManager->PerfCurve(CurveNum).Coeff6 = -2.349e-005;
    state->dataCurveManager->PerfCurve(CurveNum).Var1Min = -100.0;
    state->dataCurveManager->PerfCurve(CurveNum).Var1Max = 100.0;
    state->dataCurveManager->PerfCurve(CurveNum).Var2Min = -100.0;
    state->dataCurveManager->PerfCurve(CurveNum).Var2Max = 100.0;

    CurveNum = 4;
    state->dataCurveManager->PerfCurve(CurveNum).Name = "HP_Heat-Cap-fT-SP2";
    state->dataCurveManager->PerfCurve(CurveNum).curveType = CurveType::BiQuadratic;
    state->dataCurveManager->PerfCurve(CurveNum).ObjectType = "Curve:Biquadratic";
    state->dataCurveManager->PerfCurve(CurveNum).InterpolationType = InterpType::EvaluateCurveToLimits;
    state->dataCurveManager->PerfCurve(CurveNum).Coeff1 = 0.831506971;
    state->dataCurveManager->PerfCurve(CurveNum).Coeff2 = 0.0018392166;
    state->dataCurveManager->PerfCurve(CurveNum).Coeff3 = -0.000187596;
    state->dataCurveManager->PerfCurve(CurveNum).Coeff4 = 0.0266002056;
    state->dataCurveManager->PerfCurve(CurveNum).Coeff5 = 0.000191484;
    state->dataCurveManager->PerfCurve(CurveNum).Coeff6 = -6.5772e-005;
    state->dataCurveManager->PerfCurve(CurveNum).Var1Min = -100.0;
    state->dataCurveManager->PerfCurve(CurveNum).Var1Max = 100.0;
    state->dataCurveManager->PerfCurve(CurveNum).Var2Min = -100.0;
    state->dataCurveManager->PerfCurve(CurveNum).Var2Max = 100.0;

    state->dataDXCoils->DXCoil(1).MSCCapFTemp(1) = 1;
    state->dataDXCoils->DXCoil(1).MSCCapFTemp(2) = 2;

    DXCoilNum = 2;
    state->dataDXCoils->DXCoil(DXCoilNum).MSCCapFTemp(1) = 3;
    state->dataDXCoils->DXCoil(DXCoilNum).MSCCapFTemp(2) = 4;

    bool ErrorsFound;
    int DataTotCapCurveIndex = 0;

    state->dataDXCoils->GetCoilsInputFlag = false;

    // dx cooling coil
    int CoilIndex = 1;
    EXPECT_EQ(state->dataDXCoils->DXCoil(CoilIndex).DXCoilType, "Coil:Cooling:DX:MultiSpeed");
    DataTotCapCurveIndex = DXCoils::GetDXCoilCapFTCurveIndex(*state, CoilIndex, ErrorsFound);
    EXPECT_EQ(2, DataTotCapCurveIndex);
    // evaluate dx cooling coil curves to show impacts of incorrect curve index
    Real64 TotCapTempModFac_lowestSpeed = CurveValue(*state, 1, 19.4, 30.0);
    Real64 TotCapTempModFac_designSpeed = CurveValue(*state, DataTotCapCurveIndex, 19.4, 30.0);
    EXPECT_DOUBLE_EQ(1.0503539775151995, TotCapTempModFac_lowestSpeed);
    EXPECT_DOUBLE_EQ(1.0333316291120003, TotCapTempModFac_designSpeed);
    // apply dx cooling coil capacity curve correction
    Real64 PeakCoilCoolingLoad = 10000.0;
    Real64 NominalCoolingDesignCapacity_lowestSpeed = PeakCoilCoolingLoad / TotCapTempModFac_lowestSpeed;
    Real64 NominalCoolingDesignCapacity_designSpeed = PeakCoilCoolingLoad / TotCapTempModFac_designSpeed;
    EXPECT_DOUBLE_EQ(9520.5999254239905, NominalCoolingDesignCapacity_lowestSpeed);
    EXPECT_DOUBLE_EQ(9677.4353153145621, NominalCoolingDesignCapacity_designSpeed);

    // dx heating coil
    CoilIndex = 2;
    EXPECT_EQ(state->dataDXCoils->DXCoil(CoilIndex).DXCoilType, "Coil:Heating:DX:MultiSpeed");
    DataTotCapCurveIndex = DXCoils::GetDXCoilCapFTCurveIndex(*state, CoilIndex, ErrorsFound);
    EXPECT_EQ(4, DataTotCapCurveIndex);
    // evaluate dx heating coil curves to show impacts of incorrect curve index
    TotCapTempModFac_lowestSpeed = CurveValue(*state, 3, 5.0, 10.0);
    TotCapTempModFac_designSpeed = CurveValue(*state, DataTotCapCurveIndex, 5.0, 10.0);
    EXPECT_DOUBLE_EQ(1.1411265269999999, TotCapTempModFac_lowestSpeed);
    EXPECT_DOUBLE_EQ(1.1178750099999999, TotCapTempModFac_designSpeed);
    // apply dx heating coil capacity curve correction
    Real64 PeakCoilHeatingLoad = 10000.0;
    Real64 NominalHeatingDesignCapacity_lowestSpeed = PeakCoilHeatingLoad / TotCapTempModFac_lowestSpeed;
    Real64 NominalHeatingDesignCapacity_designSpeed = PeakCoilHeatingLoad / TotCapTempModFac_designSpeed;
    EXPECT_DOUBLE_EQ(8763.2701224550547, NominalHeatingDesignCapacity_lowestSpeed);
    EXPECT_DOUBLE_EQ(8945.5439208717980, NominalHeatingDesignCapacity_designSpeed);
}
TEST_F(EnergyPlusFixture, DXCoils_RatedInletAirWTest)
{

    Real64 Tdb = 26.6667;
    Real64 Twet = 19.4444;
    Real64 RatedW = Psychrometrics::PsyWFnTdbTwbPb(*state, Tdb, Twet, 101325.0);
    EXPECT_NEAR(RatedInletAirHumRat, RatedW, 0.000001);
}

TEST_F(EnergyPlusFixture, SingleSpeedDXCoolingCoilOutputTest)
{
    int DXCoilNum(1);
    state->dataDXCoils->NumDXCoils = 1;
    state->dataCurveManager->NumCurves = 2;
    state->dataDXCoils->DXCoil.allocate(state->dataDXCoils->NumDXCoils);
    state->dataLoopNodes->Node.allocate(2);
    state->dataDXCoils->DXCoilNumericFields.allocate(state->dataDXCoils->NumDXCoils);
    state->dataDXCoils->DXCoilNumericFields(state->dataDXCoils->NumDXCoils).PerfMode.allocate(1);
    state->dataDXCoils->DXCoilNumericFields(state->dataDXCoils->NumDXCoils).PerfMode(1).FieldNames.allocate(20);
    state->dataHeatBal->HeatReclaimDXCoil.allocate(state->dataDXCoils->NumDXCoils);
    state->dataDXCoils->DXCoilOutletTemp.allocate(state->dataDXCoils->NumDXCoils);
    state->dataDXCoils->DXCoilOutletHumRat.allocate(state->dataDXCoils->NumDXCoils);
    state->dataDXCoils->DXCoilFullLoadOutAirTemp.allocate(state->dataDXCoils->NumDXCoils);
    state->dataDXCoils->DXCoilFullLoadOutAirHumRat.allocate(state->dataDXCoils->NumDXCoils);
    state->dataDXCoils->DXCoilPartLoadRatio.allocate(state->dataDXCoils->NumDXCoils);
    state->dataDXCoils->DXCoilFanOpMode.allocate(state->dataDXCoils->NumDXCoils);
    state->dataCurveManager->PerfCurve.allocate(state->dataCurveManager->NumCurves);
    auto &Coil = state->dataDXCoils->DXCoil(DXCoilNum);
    auto &constantcurve1 = state->dataCurveManager->PerfCurve(1);
    auto &constantcurve2 = state->dataCurveManager->PerfCurve(2);
    auto &AirInletNode = state->dataLoopNodes->Node(1);
    auto &AirOutletNode = state->dataLoopNodes->Node(2);
    // set coil parameters
    Coil.DXCoilType_Num = CoilDX_CoolingSingleSpeed;
    Coil.SchedPtr = DataGlobalConstants::ScheduleAlwaysOn;
    Coil.RatedTotCap(1) = 17580.0;
    Coil.RatedCOP(1) = 3.0;
    Coil.RatedEIR(1) = 1.0 / Coil.RatedCOP(1);
    Coil.RatedAirMassFlowRate = 1.0;
    Coil.MinOATCompressor = -17.78;
    Coil.CCapFTemp(1) = 1;
    Coil.CCapFFlow(1) = 2;
    Coil.EIRFTemp(1) = 1;
    Coil.EIRFFlow(1) = 2;
    Coil.PLFFPLR(1) = 2;
    Coil.AirOutNode = 2;
    Coil.AirInNode = 1;
    // biquadratic curve
    constantcurve1.Name = "constant biquadratic curve";
    constantcurve1.curveType = CurveType::BiQuadratic;
    constantcurve1.ObjectType = "Curve:Biquadratic";
    constantcurve1.InterpolationType = InterpType::EvaluateCurveToLimits;
    constantcurve1.Coeff1 = 1.0;
    constantcurve1.Coeff2 = 0.0;
    constantcurve1.Coeff3 = 0.0;
    constantcurve1.Coeff4 = 0.0;
    constantcurve1.Coeff5 = 0.0;
    constantcurve1.Coeff6 = 0.0;
    constantcurve1.Var1Min = 10.0;
    constantcurve1.Var1Max = 25.0;
    constantcurve1.Var2Min = 0.0;
    constantcurve1.Var2Max = 100.0;
    constantcurve1.CurveMin = 1.0;
    constantcurve1.CurveMax = 1.0;
    // quadratic curve
    constantcurve2.Name = "constant quadratic curve";
    constantcurve2.curveType = CurveType::Quadratic;
    constantcurve2.ObjectType = "Curve:Quadratic";
    constantcurve2.InterpolationType = InterpType::EvaluateCurveToLimits;
    constantcurve2.Coeff1 = 1.0;
    constantcurve2.Coeff2 = 0.0;
    constantcurve2.Coeff3 = 0.0;
    constantcurve2.Var1Min = 0.0;
    constantcurve2.Var1Max = 1.0;
    constantcurve2.CurveMin = 1.0;
    constantcurve2.CurveMax = 1.0;
    // test 1: dry cooling
    Coil.BypassedFlowFrac(1) = 0.0;
    Coil.InletAirMassFlowRate = 1.0;
    Coil.InletAirTemp = 30.0;
    Coil.InletAirHumRat = 0.0075;
    Coil.InletAirEnthalpy = Psychrometrics::PsyHFnTdbW(Coil.InletAirTemp, Coil.InletAirHumRat);
    // set coil inlet and outlet node condition
    AirInletNode.Temp = Coil.InletAirTemp;
    AirInletNode.HumRat = Coil.InletAirHumRat;
    AirInletNode.Enthalpy = Coil.InletAirEnthalpy;
    AirOutletNode.Temp = Coil.InletAirTemp;
    AirOutletNode.HumRat = Coil.InletAirHumRat;
    AirOutletNode.Enthalpy = Coil.InletAirEnthalpy;
    // outside air condition
    state->dataEnvrn->OutBaroPress = 101325.0;
    state->dataEnvrn->OutDryBulbTemp = 38.0;
    state->dataEnvrn->OutHumRat = 0.0120;
    state->dataEnvrn->WindSpeed = 5.0;
    state->dataEnvrn->WindDir = 0.0;
    // run coil at full capacity
    Real64 PartLoadRatio(1.0);
    Real64 AirFlowRatio(1.0);
    int FanOpMode(2);
    CompressorOperation CompressorOp = DataHVACGlobals::CompressorOperation::On;
    CalcDoe2DXCoil(*state, DXCoilNum, CompressorOp, true, PartLoadRatio, FanOpMode, _, AirFlowRatio);
    EXPECT_NEAR(17580.0, Coil.TotalCoolingEnergyRate, 0.0001);   // equals fully capacity
    EXPECT_NEAR(17580.0, Coil.SensCoolingEnergyRate, 0.0001);    // sensible cooling only
    EXPECT_NEAR(0.0, Coil.LatCoolingEnergyRate, 1.0E-11);        // zero latent cooling rate
    EXPECT_DOUBLE_EQ(AirInletNode.HumRat, AirOutletNode.HumRat); // dry cooling only
    ;
    // check against local calculation
    Real64 results_totaloutput = Coil.InletAirMassFlowRate * (Psychrometrics::PsyHFnTdbW(AirInletNode.Temp, AirInletNode.HumRat) -
                                                              Psychrometrics::PsyHFnTdbW(AirOutletNode.Temp, AirOutletNode.HumRat));
    Real64 results_sensibleoutput = Coil.InletAirMassFlowRate * (1.00484e3 + min(AirInletNode.HumRat, AirOutletNode.HumRat) * 1.85895e3) *
                                    (AirInletNode.Temp - AirOutletNode.Temp);
    Real64 results_latentoutput = results_totaloutput - results_sensibleoutput;
    EXPECT_NEAR(results_totaloutput, Coil.TotalCoolingEnergyRate, 0.0001);
    EXPECT_NEAR(results_sensibleoutput, Coil.SensCoolingEnergyRate, 0.0001);
    EXPECT_NEAR(results_latentoutput, Coil.LatCoolingEnergyRate, 1.0E-11);

    // test 1: wet cooling
    Coil.BypassedFlowFrac(1) = 0.0;
    Coil.InletAirMassFlowRate = 1.0;
    Coil.InletAirTemp = 24.0;
    Coil.InletAirHumRat = 0.0100;
    Coil.InletAirEnthalpy = Psychrometrics::PsyHFnTdbW(Coil.InletAirTemp, Coil.InletAirHumRat);
    // set coil inlet and outlet node condition
    AirInletNode.Temp = Coil.InletAirTemp;
    AirInletNode.HumRat = Coil.InletAirHumRat;
    AirInletNode.Enthalpy = Coil.InletAirEnthalpy;
    // run coil at full capacity
    CalcDoe2DXCoil(*state, DXCoilNum, CompressorOp, true, PartLoadRatio, FanOpMode, _, AirFlowRatio);
    EXPECT_NEAR(17580.0, Coil.TotalCoolingEnergyRate, 0.0001);           // equals fully capacity
    EXPECT_NEAR(13104.577807007219, Coil.SensCoolingEnergyRate, 0.0001); // sensible cooling rate
    EXPECT_NEAR(4475.4221929927808, Coil.LatCoolingEnergyRate, 0.0001);  // latent cooling rate
    EXPECT_DOUBLE_EQ(0.0100, AirInletNode.HumRat);                       // input check
    EXPECT_NEAR(0.0082418676694790537, AirOutletNode.HumRat, 0.00001);   // cooling and dehumidification
    ;
    // check against hand calculation
    results_totaloutput = Coil.InletAirMassFlowRate * (Psychrometrics::PsyHFnTdbW(AirInletNode.Temp, AirInletNode.HumRat) -
                                                       Psychrometrics::PsyHFnTdbW(AirOutletNode.Temp, AirOutletNode.HumRat));
    results_sensibleoutput = Coil.InletAirMassFlowRate * (1.00484e3 + min(AirInletNode.HumRat, AirOutletNode.HumRat) * 1.85895e3) *
                             (AirInletNode.Temp - AirOutletNode.Temp);
    results_latentoutput = results_totaloutput - results_sensibleoutput;
    EXPECT_NEAR(results_totaloutput, Coil.TotalCoolingEnergyRate, 0.0001);
    EXPECT_NEAR(results_sensibleoutput, Coil.SensCoolingEnergyRate, 0.0001);
    EXPECT_NEAR(results_latentoutput, Coil.LatCoolingEnergyRate, 1.0E-11);
}

TEST_F(EnergyPlusFixture, MultiSpeedDXCoolingCoilOutputTest)
{

    int DXCoilNum(1);
    state->dataDXCoils->NumDXCoils = 1;
    state->dataCurveManager->NumCurves = 2;
    state->dataHVACGlobal->MSHPMassFlowRateLow = 0.6;
    state->dataHVACGlobal->MSHPMassFlowRateHigh = 1.0;
    state->dataDXCoils->DXCoil.allocate(state->dataDXCoils->NumDXCoils);
    state->dataLoopNodes->Node.allocate(2);
    state->dataDXCoils->DXCoilNumericFields.allocate(state->dataDXCoils->NumDXCoils);
    state->dataDXCoils->DXCoilNumericFields(1).PerfMode.allocate(1);
    state->dataDXCoils->DXCoilNumericFields(1).PerfMode(1).FieldNames.allocate(17);
    state->dataHeatBal->HeatReclaimDXCoil.allocate(2);
    state->dataDXCoils->DXCoilOutletTemp.allocate(1);
    state->dataDXCoils->DXCoilOutletHumRat.allocate(1);
    state->dataDXCoils->DXCoilPartLoadRatio.allocate(1);
    state->dataDXCoils->DXCoilFanOpMode.allocate(1);
    state->dataCurveManager->PerfCurve.allocate(state->dataCurveManager->NumCurves);

    auto &Coil = state->dataDXCoils->DXCoil(1);
    auto &constantcurve1 = state->dataCurveManager->PerfCurve(1);
    auto &constantcurve2 = state->dataCurveManager->PerfCurve(2);
    auto &AirInletNode = state->dataLoopNodes->Node(1);
    auto &AirOutletNode = state->dataLoopNodes->Node(2);

    Coil.DXCoilType_Num = CoilDX_MultiSpeedCooling;
    Coil.DXCoilType = "Coil:Cooling:DX:MultiSpeed";
    Coil.SchedPtr = DataGlobalConstants::ScheduleAlwaysOn;
    Coil.NumOfSpeeds = 2;
    Coil.MSRatedTotCap.allocate(Coil.NumOfSpeeds);
    Coil.MSRatedSHR.allocate(Coil.NumOfSpeeds);
    Coil.MSRatedCOP.allocate(Coil.NumOfSpeeds);
    Coil.MSRatedAirVolFlowRate.allocate(Coil.NumOfSpeeds);
    Coil.MSRatedAirMassFlowRate.allocate(Coil.NumOfSpeeds);
    Coil.MSCCapFTemp.allocate(Coil.NumOfSpeeds);
    Coil.MSCCapFFlow.allocate(Coil.NumOfSpeeds);
    Coil.MSEIRFTemp.allocate(Coil.NumOfSpeeds);
    Coil.MSEIRFFlow.allocate(Coil.NumOfSpeeds);
    Coil.MSWasteHeat.allocate(Coil.NumOfSpeeds);
    Coil.MSEvapCondEffect.allocate(Coil.NumOfSpeeds);
    Coil.MSEvapCondAirFlow.allocate(Coil.NumOfSpeeds);
    Coil.MSEvapCondPumpElecNomPower.allocate(Coil.NumOfSpeeds);
    Coil.MSRatedCBF.allocate(Coil.NumOfSpeeds);
    Coil.MSWasteHeatFrac.allocate(Coil.NumOfSpeeds);
    Coil.MSPLFFPLR.allocate(Coil.NumOfSpeeds);
    Coil.MSTwet_Rated.allocate(Coil.NumOfSpeeds);
    Coil.MSGamma_Rated.allocate(Coil.NumOfSpeeds);
    Coil.MSMaxONOFFCyclesperHour.allocate(Coil.NumOfSpeeds);
    Coil.MSLatentCapacityTimeConstant.allocate(Coil.NumOfSpeeds);
    Coil.MSFanPowerPerEvapAirFlowRate.allocate(Coil.NumOfSpeeds);
    Coil.MSCCapFTemp = 1;
    Coil.MSCCapFFlow = 2;
    Coil.MSEIRFTemp = 1;
    Coil.MSEIRFFlow = 2;
    Coil.MSPLFFPLR = 2;
    Coil.AirOutNode = 2;
    Coil.AirInNode = 1;
    // biquadratic curve
    constantcurve1.Name = "constant biquadratic curve";
    constantcurve1.curveType = CurveType::BiQuadratic;
    constantcurve1.ObjectType = "Curve:Biquadratic";
    constantcurve1.InterpolationType = InterpType::EvaluateCurveToLimits;
    constantcurve1.Coeff1 = 1.0;
    constantcurve1.Coeff2 = 0.0;
    constantcurve1.Coeff3 = 0.0;
    constantcurve1.Coeff4 = 0.0;
    constantcurve1.Coeff5 = 0.0;
    constantcurve1.Coeff6 = 0.0;
    constantcurve1.Var1Min = 10.0;
    constantcurve1.Var1Max = 25.0;
    constantcurve1.Var2Min = 0.0;
    constantcurve1.Var2Max = 100.0;
    constantcurve1.CurveMin = 1.0;
    constantcurve1.CurveMax = 1.0;
    // quadratic curve
    constantcurve2.Name = "constant quadratic curve";
    constantcurve2.curveType = CurveType::Quadratic;
    constantcurve2.ObjectType = "Curve:Quadratic";
    constantcurve2.InterpolationType = InterpType::EvaluateCurveToLimits;
    constantcurve2.Coeff1 = 1.0;
    constantcurve2.Coeff2 = 0.0;
    constantcurve2.Coeff3 = 0.0;
    constantcurve2.Var1Min = 0.0;
    constantcurve2.Var1Max = 1.0;
    constantcurve2.CurveMin = 1.0;
    constantcurve2.CurveMax = 1.0;
    // set coil parameter
    Coil.MSRatedTotCap(1) = 10710.0; // 60 % of full capacity
    Coil.MSRatedTotCap(2) = 17850.0; // 5 ton capcity
    Coil.InletAirMassFlowRate = state->dataHVACGlobal->MSHPMassFlowRateHigh;
    Coil.MSRatedAirMassFlowRate(1) = state->dataHVACGlobal->MSHPMassFlowRateLow;
    Coil.MSRatedAirMassFlowRate(2) = state->dataHVACGlobal->MSHPMassFlowRateHigh;
    Coil.MSRatedCBF(1) = 0.0;
    Coil.MSRatedCBF(2) = 0.0;
    Coil.MSWasteHeat(1) = 0;
    Coil.MSWasteHeat(2) = 0;
    Coil.MSWasteHeatFrac(1) = 0;
    Coil.MSWasteHeatFrac(2) = 0;
    Coil.MSRatedSHR(1) = 0.65;
    Coil.MSRatedSHR(2) = 0.75;
    Coil.MSRatedCOP(1) = 3.0;
    Coil.MSRatedCOP(2) = 3.0;

    // test 1: dry cooling
    Coil.InletAirTemp = 30.0;
    Coil.InletAirHumRat = 0.0075;
    Coil.InletAirEnthalpy = Psychrometrics::PsyHFnTdbW(Coil.InletAirTemp, Coil.InletAirHumRat);
    // set coil inlet and outlet node condition
    AirInletNode.Temp = Coil.InletAirTemp;
    AirInletNode.HumRat = Coil.InletAirHumRat;
    AirInletNode.Enthalpy = Coil.InletAirEnthalpy;
    AirOutletNode.Temp = Coil.InletAirTemp;
    AirOutletNode.HumRat = Coil.InletAirHumRat;
    AirOutletNode.Enthalpy = Coil.InletAirEnthalpy;
    // outside air condition
    state->dataEnvrn->OutBaroPress = 101325.0;
    state->dataEnvrn->OutDryBulbTemp = 30.0;
    state->dataEnvrn->OutHumRat = 0.0120;
    state->dataEnvrn->WindSpeed = 5.0;
    state->dataEnvrn->WindDir = 0.0;
    int SpeedNum = 2;
    int FanOpMode = 1;
    CompressorOperation CompressorOp = DataHVACGlobals::CompressorOperation::On;
    int SingleMode = 0;
    // run the coil at low speed
    Real64 SpeedRatio = 0.0;
    Real64 CycRatio = 1.0;
    CalcMultiSpeedDXCoilCooling(*state, DXCoilNum, SpeedRatio, CycRatio, SpeedNum, FanOpMode, CompressorOp, SingleMode);
    EXPECT_NEAR(10710.0, Coil.TotalCoolingEnergyRate, 0.0001);   // equals low speed capacity
    EXPECT_NEAR(10710.0, Coil.SensCoolingEnergyRate, 0.0001);    // sensible cooling rate at low speed
    EXPECT_NEAR(0.0, Coil.LatCoolingEnergyRate, 1.0E-11);        // zero latent cooling rate at low speed
    EXPECT_DOUBLE_EQ(0.0075, AirInletNode.HumRat);               // input check
    EXPECT_DOUBLE_EQ(AirInletNode.HumRat, AirOutletNode.HumRat); // dry cooling only
    ;
    // check against hand calculation at low speed
    Real64 results_totaloutput = state->dataHVACGlobal->MSHPMassFlowRateLow * (Psychrometrics::PsyHFnTdbW(AirInletNode.Temp, AirInletNode.HumRat) -
                                                                               Psychrometrics::PsyHFnTdbW(AirOutletNode.Temp, AirOutletNode.HumRat));
    Real64 results_sensibleoutput = state->dataHVACGlobal->MSHPMassFlowRateLow *
                                    (1.00484e3 + min(AirInletNode.HumRat, AirOutletNode.HumRat) * 1.85895e3) *
                                    (AirInletNode.Temp - AirOutletNode.Temp);
    Real64 results_latentoutput = results_totaloutput - results_sensibleoutput;
    EXPECT_NEAR(results_totaloutput, Coil.TotalCoolingEnergyRate, 0.0001);
    EXPECT_NEAR(results_sensibleoutput, Coil.SensCoolingEnergyRate, 0.0001);
    EXPECT_NEAR(results_latentoutput, Coil.LatCoolingEnergyRate, 1.0E-11);
    // run the coil at high speed
    SpeedRatio = 1.0;
    CalcMultiSpeedDXCoilCooling(*state, DXCoilNum, SpeedRatio, CycRatio, SpeedNum, FanOpMode, CompressorOp, SingleMode);
    EXPECT_NEAR(17850.0, Coil.TotalCoolingEnergyRate, 0.0001);   // total capacity at high speed
    EXPECT_NEAR(17850.0, Coil.SensCoolingEnergyRate, 0.0001);    // sensible cooling rate at high speed
    EXPECT_NEAR(0.0, Coil.LatCoolingEnergyRate, 1.0E-11);        // zero latent cooling rate at high speed
    EXPECT_DOUBLE_EQ(0.0075, AirInletNode.HumRat);               // input check
    EXPECT_DOUBLE_EQ(AirInletNode.HumRat, AirOutletNode.HumRat); // dry cooling only
    ;
    // check against hand calculation
    results_totaloutput = state->dataHVACGlobal->MSHPMassFlowRateHigh * (Psychrometrics::PsyHFnTdbW(AirInletNode.Temp, AirInletNode.HumRat) -
                                                                         Psychrometrics::PsyHFnTdbW(AirOutletNode.Temp, AirOutletNode.HumRat));
    results_sensibleoutput = state->dataHVACGlobal->MSHPMassFlowRateHigh * (1.00484e3 + min(AirInletNode.HumRat, AirOutletNode.HumRat) * 1.85895e3) *
                             (AirInletNode.Temp - AirOutletNode.Temp);
    results_latentoutput = results_totaloutput - results_sensibleoutput;
    EXPECT_NEAR(results_totaloutput, Coil.TotalCoolingEnergyRate, 0.0001);
    EXPECT_NEAR(results_sensibleoutput, Coil.SensCoolingEnergyRate, 0.0001);
    EXPECT_NEAR(results_latentoutput, Coil.LatCoolingEnergyRate, 1.0E-11);

    // test 2: wet cooling
    Coil.InletAirTemp = 24.0;
    Coil.InletAirHumRat = 0.0100;
    Coil.InletAirEnthalpy = Psychrometrics::PsyHFnTdbW(Coil.InletAirTemp, Coil.InletAirHumRat);
    // set coil inlet and outlet node condition
    AirInletNode.Temp = Coil.InletAirTemp;
    AirInletNode.HumRat = Coil.InletAirHumRat;
    AirInletNode.Enthalpy = Coil.InletAirEnthalpy;
    // run coil at low speed
    SpeedRatio = 0.0;
    CycRatio = 1.0;
    CalcMultiSpeedDXCoilCooling(*state, DXCoilNum, SpeedRatio, CycRatio, SpeedNum, FanOpMode, CompressorOp, SingleMode);
    EXPECT_NEAR(10710.0, Coil.TotalCoolingEnergyRate, 0.0001);           // equals low speed cooling capacity
    EXPECT_NEAR(7930.3412059184047, Coil.SensCoolingEnergyRate, 0.0001); // sensible cooling rate at low speed
    EXPECT_NEAR(2779.6587940815953, Coil.LatCoolingEnergyRate, 0.0001);  // latent cooling rate at low speed
    EXPECT_DOUBLE_EQ(0.0100, AirInletNode.HumRat);                       // input check
    EXPECT_NEAR(0.0081800569931542392, AirOutletNode.HumRat, 0.00001);   // cooling and dehumidification
    ;
    // check against hand calculation at low speed
    results_totaloutput = state->dataHVACGlobal->MSHPMassFlowRateLow * (Psychrometrics::PsyHFnTdbW(AirInletNode.Temp, AirInletNode.HumRat) -
                                                                        Psychrometrics::PsyHFnTdbW(AirOutletNode.Temp, AirOutletNode.HumRat));
    results_sensibleoutput = state->dataHVACGlobal->MSHPMassFlowRateLow * (1.00484e3 + min(AirInletNode.HumRat, AirOutletNode.HumRat) * 1.85895e3) *
                             (AirInletNode.Temp - AirOutletNode.Temp);
    results_latentoutput = results_totaloutput - results_sensibleoutput;
    EXPECT_NEAR(results_totaloutput, Coil.TotalCoolingEnergyRate, 0.0001);
    EXPECT_NEAR(results_sensibleoutput, Coil.SensCoolingEnergyRate, 0.0001);
    EXPECT_NEAR(results_latentoutput, Coil.LatCoolingEnergyRate, 1.0E-11);

    // run the coil at high speed
    SpeedRatio = 1.0;
    CalcMultiSpeedDXCoilCooling(*state, DXCoilNum, SpeedRatio, CycRatio, SpeedNum, FanOpMode, CompressorOp, SingleMode);
    EXPECT_NEAR(17850.0, Coil.TotalCoolingEnergyRate, 0.0001);           // total capacity at high speed
    EXPECT_NEAR(13217.235343197342, Coil.SensCoolingEnergyRate, 0.0001); // sensible cooling rate at high speed
    EXPECT_NEAR(4632.7646568026576, Coil.LatCoolingEnergyRate, 0.0001);  // latent cooling rate at high speed
    EXPECT_DOUBLE_EQ(0.0100, AirInletNode.HumRat);                       // input check
    EXPECT_NEAR(0.0081800569931542392, AirOutletNode.HumRat, 0.00001);   // cooling and dehumidification
    ;
    // check against hand calculation
    results_totaloutput = state->dataHVACGlobal->MSHPMassFlowRateHigh * (Psychrometrics::PsyHFnTdbW(AirInletNode.Temp, AirInletNode.HumRat) -
                                                                         Psychrometrics::PsyHFnTdbW(AirOutletNode.Temp, AirOutletNode.HumRat));
    results_sensibleoutput = state->dataHVACGlobal->MSHPMassFlowRateHigh * (1.00484e3 + min(AirInletNode.HumRat, AirOutletNode.HumRat) * 1.85895e3) *
                             (AirInletNode.Temp - AirOutletNode.Temp);
    results_latentoutput = results_totaloutput - results_sensibleoutput;
    EXPECT_NEAR(results_totaloutput, Coil.TotalCoolingEnergyRate, 0.0001);
    EXPECT_NEAR(results_sensibleoutput, Coil.SensCoolingEnergyRate, 0.0001);
    EXPECT_NEAR(results_latentoutput, Coil.LatCoolingEnergyRate, 1.0E-11);
}

TEST_F(EnergyPlusFixture, TwoSpeedDXCoilStandardRatingsTest)
{

    std::string const idf_objects = delimited_string({

        "ScheduleTypeLimits,",
        "    OnOff,                   !- Name",
        "    0,                       !- Lower Limit Value",
        "    1,                       !- Upper Limit Value",
        "    Discrete,                !- Numeric Type",
        "    availability;            !- Unit Type",

        "Schedule:Constant,",
        "    Always On Discrete,      !- Name",
        "    OnOff,                   !- Schedule Type Limits Name",
        "    1;                       !- Hourly Value",

        "Fan:VariableVolume,",
        "    Fan Variable Volume,     !- Name",
        "    Always On Discrete,      !- Availability Schedule Name",
        "    0.60,                    !- Fan Total Efficiency",
        "    500,                     !- Pressure Rise {Pa}",
        "    1.0,                     !- Maximum Flow Rate {m3/s}",
        "    FixedFlowRate,           !- Fan Power Minimum Flow Rate Input Method",
        "    0,                       !- Fan Power Minimum Flow Fraction",
        "    0,                       !- Fan Power Minimum Air Flow Rate {m3/s}",
        "    0.90,                    !- Motor Efficiency",
        "    1,                       !- Motor In Airstream Fraction",
        "    0.040759894,             !- Fan Power Coefficient 1",
        "    0.08804497,              !- Fan Power Coefficient 2",
        "    -0.07292612,             !- Fan Power Coefficient 3",
        "    0.943739823,             !- Fan Power Coefficient 4",
        "    0,                       !- Fan Power Coefficient 5",
        "    Node 11,                 !- Air Inlet Node Name",
        "    Node 3;                  !- Air Outlet Node Name",

        "Coil:Cooling:DX:TwoSpeed,",
        "    CCooling DX Two Speed,   !- Name",
        "    Always On Discrete,      !- Availability Schedule Name",
        "    17580.0,                 !- High Speed Gross Rated Total Cooling Capacity {W}",
        "    0.75,                    !- High Speed Rated Sensible Heat Ratio",
        "    3,                       !- High Speed Gross Rated Cooling COP {W/W}",
        "    1.0,                     !- High Speed Rated Air Flow Rate {m3/s}",
        "    400,                     !- Unit Internal Static Air Pressure {Pa}",
        "    Node 9,                  !- Air Inlet Node Name",
        "    Node 10,                 !- Air Outlet Node Name",
        "    CoolCAPFT,               !- Total Cooling Capacity Function of Temperature Curve Name",
        "    CoolCAPFFF,              !- Total Cooling Capacity Function of Flow Fraction Curve Name",
        "    CoolEIRFT,               !- Energy Input Ratio Function of Temperature Curve Name",
        "    CoolEIRFFF,              !- Energy Input Ratio Function of Flow Fraction Curve Name",
        "    CoolPLFFPLR,             !- Part Load Fraction Correlation Curve Name",
        "    4300.0,                  !- Low Speed Gross Rated Total Cooling Capacity {W}",
        "    0.70,                    !- Low Speed Gross Rated Sensible Heat Ratio",
        "    3,                       !- Low Speed Gross Rated Cooling COP {W/W}",
        "    0.30,                    !- Low Speed Rated Air Flow Rate {m3/s}",
        "    LSCoolCAPFT,             !- Low Speed Total Cooling Capacity Function of Temperature Curve Name",
        "    LSCoolEIRFT,             !- Low Speed Energy Input Ratio Function of Temperature Curve Name",
        "    ,                        !- Condenser Air Inlet Node Name",
        "    AirCooled,               !- Condenser Type",
        "    ,                        !- Minimum Outdoor Dry-Bulb Temperature for Compressor Operation {C}",
        "    ,                        !- High Speed Evaporative Condenser Effectiveness {dimensionless}",
        "    ,                        !- High Speed Evaporative Condenser Air Flow Rate {m3/s}",
        "    ,                        !- High Speed Evaporative Condenser Pump Rated Power Consumption {W}",
        "    ,                        !- Low Speed Evaporative Condenser Effectiveness {dimensionless}",
        "    ,                        !- Low Speed Evaporative Condenser Air Flow Rate {m3/s}",
        "    ,                        !- Low Speed Evaporative Condenser Pump Rated Power Consumption {W}",
        "    ,                        !- Supply Water Storage Tank Name",
        "    ,                        !- Condensate Collection Water Storage Tank Name",
        "    0,                       !- Basin Heater Capacity {W/K}",
        "    2;                       !- Basin Heater Setpoint Temperature {C}",

        "Curve:Quadratic,CoolCAPFFF,0.77136,0.34053,-0.11088,0.75918,1.13877, 0.0, 2.0, Dimensionless, Dimensionless; ",
        "Curve:Quadratic,CoolEIRFFF,1.2055,-0.32953,0.12308,0.75918,1.13877, 0.0, 2.0, Dimensionless, Dimensionless; ",
        "Curve:Quadratic,CoolPLFFPLR,0.771,0.229, 0.0, 0.0, 1.0, 0.0, 1.0, Dimensionless, Dimensionless; ",
        "Curve:Biquadratic,CoolCAPFT,0.42415,0.04426,-0.00042,0.00333,-8e-005,-0.00021,17,22,13,46, , , Temperature, Temperature, Dimensionless;",
        "Curve:Biquadratic,CoolEIRFT,1.23649,-0.02431,0.00057,-0.01434,0.00063,-0.00038,17,22,13,46, , , Temperature, Temperature, Dimensionless;",
        "Curve:Biquadratic,LSCoolCAPFT,0.42415,0.04426,-0.00042,0.00333,-8e-005,-0.00021,17,22,13,46, , , Temperature, Temperature, Dimensionless;",
        "Curve:Biquadratic,LSCoolEIRFT,1.23649,-0.02431,0.00057,-0.01434,0.00063,-0.00038,17,22,13,46, , , Temperature, Temperature, Dimensionless;",
    });

    ASSERT_TRUE(process_idf(idf_objects));

    state->dataGlobal->NumOfTimeStepInHour = 1;
    state->dataGlobal->MinutesPerTimeStep = 60;
    ScheduleManager::ProcessScheduleInput(*state);
    state->dataEnvrn->StdRhoAir = 1.0;
    state->dataEnvrn->OutBaroPress = 101325.0;
    GetCurveInput(*state);
    Fans::GetFanInput(*state);
    GetDXCoils(*state);
    int dXCoilIndex = UtilityRoutines::FindItemInList("CCOOLING DX TWO SPEED", state->dataDXCoils->DXCoil);
    int fanIndex = UtilityRoutines::FindItemInList("FAN VARIABLE VOLUME", state->dataFans->Fan, &Fans::FanEquipConditions::FanName);
    auto &coolcoilTwoSpeed = state->dataDXCoils->DXCoil(dXCoilIndex);
    auto &supplyFan = state->dataFans->Fan(fanIndex);
    coolcoilTwoSpeed.SupplyFanIndex = fanIndex;
    coolcoilTwoSpeed.SupplyFanName = supplyFan.FanName;
    coolcoilTwoSpeed.SupplyFan_TypeNum = supplyFan.FanType_Num;
    state->dataGlobal->SysSizingCalc = true;
    coolcoilTwoSpeed.RatedAirMassFlowRate(1) = coolcoilTwoSpeed.RatedAirVolFlowRate(1) * state->dataEnvrn->StdRhoAir;
    coolcoilTwoSpeed.RatedAirMassFlowRate2 = coolcoilTwoSpeed.RatedAirVolFlowRate2 * state->dataEnvrn->StdRhoAir;
    supplyFan.MaxAirMassFlowRate = supplyFan.MaxAirFlowRate * state->dataEnvrn->StdRhoAir;
    supplyFan.RhoAirStdInit = state->dataEnvrn->StdRhoAir;
    auto &InletNode = state->dataLoopNodes->Node(supplyFan.InletNodeNum);
    auto &OutletNode = state->dataLoopNodes->Node(supplyFan.OutletNodeNum);
    InletNode.MassFlowRate = 1.0;
    InletNode.MassFlowRateMax = 1.0;
    InletNode.MassFlowRateMaxAvail = 1.0;
    InletNode.MassFlowRateMinAvail = 0.0;
    OutletNode.MassFlowRate = 1.0;
    OutletNode.MassFlowRateMax = 1.0;
    OutletNode.MassFlowRateMaxAvail = 1.0;
    OutletNode.MassFlowRateMinAvail = 0.0;
    OutputReportPredefined::SetPredefinedTables(*state);
    // test 1: using internal static and fan pressure rise
    CalcTwoSpeedDXCoilStandardRating(*state, dXCoilIndex);
    EXPECT_EQ(coolcoilTwoSpeed.Name, "CCOOLING DX TWO SPEED");
    EXPECT_EQ(coolcoilTwoSpeed.DXCoilType, "Coil:Cooling:DX:TwoSpeed");
    EXPECT_EQ(coolcoilTwoSpeed.DXCoilType_Num, CoilDX_CoolingTwoSpeed);
    EXPECT_EQ(coolcoilTwoSpeed.InternalStaticPressureDrop, 400.0);
    EXPECT_TRUE(coolcoilTwoSpeed.RateWithInternalStaticAndFanObject);
    EXPECT_EQ("8.77", RetrievePreDefTableEntry(*state, state->dataOutRptPredefined->pdchDXCoolCoilEERIP, coolcoilTwoSpeed.Name));
    EXPECT_EQ("11.25", RetrievePreDefTableEntry(*state, state->dataOutRptPredefined->pdchDXCoolCoilIEERIP, coolcoilTwoSpeed.Name));
    // test 2: using default fan power per evap air flow rate, 365 W/1000 scfm or 773.3 W/(m3/s)
    coolcoilTwoSpeed.RateWithInternalStaticAndFanObject = false;
    OutputReportPredefined::SetPredefinedTables(*state);
    CalcTwoSpeedDXCoilStandardRating(*state, dXCoilIndex);
    EXPECT_EQ("8.72", RetrievePreDefTableEntry(*state, state->dataOutRptPredefined->pdchDXCoolCoilEERIP, coolcoilTwoSpeed.Name));
    EXPECT_EQ("10.15", RetrievePreDefTableEntry(*state, state->dataOutRptPredefined->pdchDXCoolCoilIEERIP, coolcoilTwoSpeed.Name));
}

} // namespace EnergyPlus
