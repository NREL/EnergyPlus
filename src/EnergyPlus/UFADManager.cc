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

// C++ Headers
#include <cmath>

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>
#include <ObjexxFCL/Fmath.hh>
#include <ObjexxFCL/member.functions.hh>

// EnergyPlus Headers
#include <EnergyPlus/Autosizing/Base.hh>
#include <EnergyPlus/ConvectionCoefficients.hh>
#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataHVACGlobals.hh>
#include <EnergyPlus/DataHeatBalFanSys.hh>
#include <EnergyPlus/DataHeatBalSurface.hh>
#include <EnergyPlus/DataHeatBalance.hh>
#include <EnergyPlus/DataLoopNode.hh>
#include <EnergyPlus/DataRoomAirModel.hh>
#include <EnergyPlus/DataSizing.hh>
#include <EnergyPlus/DataSurfaces.hh>
#include <EnergyPlus/DataUCSDSharedData.hh>
#include <EnergyPlus/DataZoneEquipment.hh>
#include <EnergyPlus/General.hh>
#include <EnergyPlus/InternalHeatGains.hh>
#include <EnergyPlus/Psychrometrics.hh>
#include <EnergyPlus/UFADManager.hh>
#include <EnergyPlus/UtilityRoutines.hh>

namespace EnergyPlus::UFADManager {

// Module containing the routines dealing with the UnderFloor Air
// Distribution zone model

// MODULE INFORMATION:
//       AUTHOR         Fred Buhl
//       DATE WRITTEN   August 2005
//       MODIFIED       na
//       RE-ENGINEERED  na

// PURPOSE OF THIS MODULE:
// Encapsulate the routines that do the simulation of the UCSD UFAD non-uniform
// zone models

// METHODOLOGY EMPLOYED:
// 2-node zone model with the node heights varying as a function of internal loads
// and supply air flow (and other factors)

// REFERENCES:
// See the EnergyPlus Engineering Reference and the PhD thesis of Anna Liu, UC San Diego

// OTHER NOTES:
// na

// Using/Aliasing
using namespace DataLoopNode;
using namespace DataEnvironment;
using namespace DataHeatBalance;
using namespace DataHeatBalSurface;
using namespace DataSurfaces;
using namespace DataRoomAirModel;
using ConvectionCoefficients::CalcDetailedHcInForDVModel;

void ManageUCSDUFModels(EnergyPlusData &state,
                        int const ZoneNum,                                 // index number for the specified zone
                        DataRoomAirModel::RoomAirModel const ZoneModelType // type of zone model; UCSDUFI = 6
)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Fred Buhl
    //       DATE WRITTEN   August, 2005
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // Manages the simulation of the 2-node nonuniform zone models for underfloor air
    // distribution systems (UFAD). Called from RoomAirManager, ManageAirModel

    // METHODOLOGY EMPLOYED:
    // uses Init and Calc routines in the standard EPlus manner to manage the calculation
    // Note that much of the initialization is done in RoomAirManager, SharedDVCVUFDataInit

    // Using/Aliasing
    using namespace DataLoopNode;
    using namespace DataEnvironment;
    using namespace DataHeatBalance;
    using namespace DataHeatBalSurface;
    using namespace DataSurfaces;
    using namespace DataRoomAirModel;
    using ConvectionCoefficients::CalcDetailedHcInForDVModel;

    // input was obtained in RoomAirManager, GetUFADIntZoneData

    InitUCSDUF(state, ZoneNum, ZoneModelType); // initialize some module variables

    {
        auto const SELECT_CASE_var(ZoneModelType);

        if (SELECT_CASE_var == DataRoomAirModel::RoomAirModel::UCSDUFI) { // UCSD UFAD interior zone model
            // simulate room airflow using the UCSDUFI model
            CalcUCSDUI(state, ZoneNum);

        } else if (SELECT_CASE_var == DataRoomAirModel::RoomAirModel::UCSDUFE) { // UCSD UFAD interior zone model
            // simulate room airflow using the UCSDUFI model
            CalcUCSDUE(state, ZoneNum);
        }
    }
}

void InitUCSDUF(EnergyPlusData &state,
                int const ZoneNum,
                DataRoomAirModel::RoomAirModel const ZoneModelType // type of zone model; UCSDUFI = 6
)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Fred Buhl
    //       DATE WRITTEN   August 2005
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // initialize arrays & variables used by the UCSD UFAD zone models

    // METHODOLOGY EMPLOYED:
    // Note that much of the initialization is done in RoomAirManager, SharedDVCVUFDataInit

    Real64 NumShadesDown(0.0);
    int UINum; // index to underfloor interior zone model data

    // Do the one time initializations
    if (state.dataUFADManager->MyOneTimeFlag) {
        state.dataUFADManager->HeightFloorSubzoneTop = 0.2;
        state.dataUFADManager->ThickOccupiedSubzoneMin = 0.2;
        state.dataUFADManager->HeightIntMassDefault = 2.0;
        state.dataUFADManager->MyOneTimeFlag = false;
        state.dataUFADManager->MySizeFlag.dimension(state.dataGlobal->NumOfZones, true);
    }

    if (state.dataUFADManager->MySizeFlag(ZoneNum)) {
        SizeUCSDUF(state, ZoneNum, ZoneModelType);
        state.dataUFADManager->MySizeFlag(ZoneNum) = false;
    }

    // initialize these variables every timestep

    state.dataUFADManager->HeightIntMass = state.dataUFADManager->HeightIntMassDefault;
    state.dataRoomAirMod->ZoneUFGamma(ZoneNum) = 0.0;
    state.dataRoomAirMod->ZoneUFPowInPlumes(ZoneNum) = 0.0;
    NumShadesDown = 0.0;
    for (int Ctd = state.dataUCSDShared->PosZ_Window((ZoneNum - 1) * 2 + 1); Ctd <= state.dataUCSDShared->PosZ_Window((ZoneNum - 1) * 2 + 2); ++Ctd) {
        int SurfNum = state.dataUCSDShared->APos_Window(Ctd);
        if (SurfNum == 0) continue;
        if (state.dataSurface->Surface(SurfNum).ExtBoundCond == ExternalEnvironment ||
            state.dataSurface->Surface(SurfNum).ExtBoundCond == OtherSideCoefNoCalcExt ||
            state.dataSurface->Surface(SurfNum).ExtBoundCond == OtherSideCoefCalcExt ||
            state.dataSurface->Surface(SurfNum).ExtBoundCond == OtherSideCondModeledExt) {
            if (ANY_INTERIOR_SHADE_BLIND(state.dataSurface->SurfWinShadingFlag(SurfNum))) {
                ++NumShadesDown;
            }
        }
    }
    if (ZoneModelType == DataRoomAirModel::RoomAirModel::UCSDUFE) {
        UINum = state.dataRoomAirMod->ZoneUFPtr(ZoneNum);
        if (state.dataRoomAirMod->ZoneUCSDUE(UINum).NumExtWin > 1.0) {
            if (NumShadesDown / state.dataRoomAirMod->ZoneUCSDUE(UINum).NumExtWin >= 0.5) {
                state.dataRoomAirMod->ZoneUCSDUE(UINum).ShadeDown = true;
            } else {
                state.dataRoomAirMod->ZoneUCSDUE(UINum).ShadeDown = false;
            }
        } else {
            state.dataRoomAirMod->ZoneUCSDUE(UINum).ShadeDown = false;
        }
    }
}

void SizeUCSDUF(EnergyPlusData &state,
                int const ZoneNum,
                DataRoomAirModel::RoomAirModel const ZoneModelType // type of zone model; UCSDUFI = 6
)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Fred Buhl
    //       DATE WRITTEN   August 2005
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // set some smart defaults for UFAD systems

    // METHODOLOGY EMPLOYED:
    // use data from Center for Built Environment

    using DataSizing::AutoSize;

    int UINum;                     // index to underfloor interior zone model data
    Real64 NumberOfOccupants(0.0); // design number of occupants in the zone
    Real64 NumberOfPlumes(0.0);    // design number of plumes in the zone
    Real64 ZoneElecConv(0.0);      // zone elec equip design convective gain [W]
    Real64 ZoneGasConv(0.0);       // zone gas equip design convective gain [W]
    Real64 ZoneOthEqConv(0.0);     // zone other equip design convective gain [W]
    Real64 ZoneHWEqConv(0.0);      // zone hot water equip design convective gain [W]
    Real64 ZoneSteamEqConv(0.0);   // zone steam equip design convective gain [W]

    if (ZoneModelType == DataRoomAirModel::RoomAirModel::UCSDUFI) {
        UINum = state.dataRoomAirMod->ZoneUFPtr(ZoneNum);
        NumberOfOccupants = 0.0;
        for (int Ctd = 1; Ctd <= state.dataHeatBal->TotPeople; ++Ctd) {
            if (state.dataHeatBal->People(Ctd).ZonePtr == ZoneNum) {
                NumberOfOccupants += state.dataHeatBal->People(Ctd).NumberOfPeople;
            }
        }
        if (state.dataRoomAirMod->ZoneUCSDUI(UINum).DiffArea == AutoSize) {
            if (state.dataRoomAirMod->ZoneUCSDUI(UINum).DiffuserType == Diffuser::Swirl) {
                state.dataRoomAirMod->ZoneUCSDUI(UINum).DiffArea = 0.0075;
            } else if (state.dataRoomAirMod->ZoneUCSDUI(UINum).DiffuserType == Diffuser::VarArea) {
                state.dataRoomAirMod->ZoneUCSDUI(UINum).DiffArea = 0.035;
            } else if (state.dataRoomAirMod->ZoneUCSDUI(UINum).DiffuserType == Diffuser::DisplVent) {
                state.dataRoomAirMod->ZoneUCSDUI(UINum).DiffArea = 0.0060;
            } else if (state.dataRoomAirMod->ZoneUCSDUI(UINum).DiffuserType == Diffuser::LinBarGrille) {
                // 4 ft x 4 inches; 75 cfm per linear foot; area is .025 m2/m
                state.dataRoomAirMod->ZoneUCSDUI(UINum).DiffArea = 0.03;
            } else {
                state.dataRoomAirMod->ZoneUCSDUI(UINum).DiffArea = 0.0075;
            }
            BaseSizer::reportSizerOutput(state,
                                         "RoomAirSettings:UnderFloorAirDistributionInterior",
                                         state.dataRoomAirMod->ZoneUCSDUI(UINum).ZoneName,
                                         "Design effective area of diffuser",
                                         state.dataRoomAirMod->ZoneUCSDUI(UINum).DiffArea);
        }
        if (state.dataRoomAirMod->ZoneUCSDUI(UINum).DiffAngle == AutoSize) {
            if (state.dataRoomAirMod->ZoneUCSDUI(UINum).DiffuserType == Diffuser::Swirl) {
                state.dataRoomAirMod->ZoneUCSDUI(UINum).DiffAngle = 28.0;
            } else if (state.dataRoomAirMod->ZoneUCSDUI(UINum).DiffuserType == Diffuser::VarArea) {
                state.dataRoomAirMod->ZoneUCSDUI(UINum).DiffAngle = 45.0;
            } else if (state.dataRoomAirMod->ZoneUCSDUI(UINum).DiffuserType == Diffuser::DisplVent) {
                state.dataRoomAirMod->ZoneUCSDUI(UINum).DiffAngle = 73.0;
            } else if (state.dataRoomAirMod->ZoneUCSDUI(UINum).DiffuserType == Diffuser::LinBarGrille) {
                state.dataRoomAirMod->ZoneUCSDUI(UINum).DiffAngle = 15.0;
            } else {
                state.dataRoomAirMod->ZoneUCSDUI(UINum).DiffAngle = 28.0;
            }
            BaseSizer::reportSizerOutput(state,
                                         "RoomAirSettings:UnderFloorAirDistributionInterior",
                                         state.dataRoomAirMod->ZoneUCSDUI(UINum).ZoneName,
                                         "Angle between diffuser slots and the vertical",
                                         state.dataRoomAirMod->ZoneUCSDUI(UINum).DiffAngle);
        }
        if (state.dataRoomAirMod->ZoneUCSDUI(UINum).TransHeight == AutoSize) {
            state.dataRoomAirMod->ZoneUCSDUI(UINum).CalcTransHeight = true;
            state.dataRoomAirMod->ZoneUCSDUI(UINum).TransHeight = 0.0;
        } else {
            state.dataRoomAirMod->ZoneUCSDUI(UINum).CalcTransHeight = false;
        }
        if (state.dataRoomAirMod->ZoneUCSDUI(UINum).DiffuserType == Diffuser::Swirl) {
            if (state.dataRoomAirMod->ZoneUCSDUI(UINum).A_Kc != DataGlobalConstants::AutoCalculate ||
                state.dataRoomAirMod->ZoneUCSDUI(UINum).B_Kc != DataGlobalConstants::AutoCalculate ||
                state.dataRoomAirMod->ZoneUCSDUI(UINum).C_Kc != DataGlobalConstants::AutoCalculate ||
                state.dataRoomAirMod->ZoneUCSDUI(UINum).D_Kc != DataGlobalConstants::AutoCalculate ||
                state.dataRoomAirMod->ZoneUCSDUI(UINum).E_Kc != DataGlobalConstants::AutoCalculate) {
                ShowWarningError(state,
                                 "For RoomAirSettings:UnderFloorAirDistributionInterior for Zone " +
                                     state.dataRoomAirMod->ZoneUCSDUI(UINum).ZoneName +
                                     ", input for Coefficients A - E will be ignored when Floor Diffuser Type = Swirl.");
                ShowContinueError(state, "  To input these Coefficients, use Floor Diffuser Type = Custom.");
            }
            state.dataRoomAirMod->ZoneUCSDUI(UINum).A_Kc = 0.0;
            state.dataRoomAirMod->ZoneUCSDUI(UINum).B_Kc = 0.0;
            state.dataRoomAirMod->ZoneUCSDUI(UINum).C_Kc = 0.6531;
            state.dataRoomAirMod->ZoneUCSDUI(UINum).D_Kc = 0.0069;
            state.dataRoomAirMod->ZoneUCSDUI(UINum).E_Kc = -0.00004;
        } else if (state.dataRoomAirMod->ZoneUCSDUI(UINum).DiffuserType == Diffuser::VarArea) {
            if (state.dataRoomAirMod->ZoneUCSDUI(UINum).A_Kc != DataGlobalConstants::AutoCalculate ||
                state.dataRoomAirMod->ZoneUCSDUI(UINum).B_Kc != DataGlobalConstants::AutoCalculate ||
                state.dataRoomAirMod->ZoneUCSDUI(UINum).C_Kc != DataGlobalConstants::AutoCalculate ||
                state.dataRoomAirMod->ZoneUCSDUI(UINum).D_Kc != DataGlobalConstants::AutoCalculate ||
                state.dataRoomAirMod->ZoneUCSDUI(UINum).E_Kc != DataGlobalConstants::AutoCalculate) {
                ShowWarningError(state,
                                 "For RoomAirSettings:UnderFloorAirDistributionInterior for Zone " +
                                     state.dataRoomAirMod->ZoneUCSDUI(UINum).ZoneName +
                                     ", input for Coefficients A - E will be ignored when Floor Diffuser Type = VariableArea.");
                ShowContinueError(state, "  To input these Coefficients, use Floor Diffuser Type = Custom.");
            }
            state.dataRoomAirMod->ZoneUCSDUI(UINum).A_Kc = 0.0;
            state.dataRoomAirMod->ZoneUCSDUI(UINum).B_Kc = 0.0;
            state.dataRoomAirMod->ZoneUCSDUI(UINum).C_Kc = 0.88;
            state.dataRoomAirMod->ZoneUCSDUI(UINum).D_Kc = 0.0;
            state.dataRoomAirMod->ZoneUCSDUI(UINum).E_Kc = 0.0;
        } else if (state.dataRoomAirMod->ZoneUCSDUI(UINum).DiffuserType == Diffuser::DisplVent) {
            if (state.dataRoomAirMod->ZoneUCSDUI(UINum).A_Kc != DataGlobalConstants::AutoCalculate ||
                state.dataRoomAirMod->ZoneUCSDUI(UINum).B_Kc != DataGlobalConstants::AutoCalculate ||
                state.dataRoomAirMod->ZoneUCSDUI(UINum).C_Kc != DataGlobalConstants::AutoCalculate ||
                state.dataRoomAirMod->ZoneUCSDUI(UINum).D_Kc != DataGlobalConstants::AutoCalculate ||
                state.dataRoomAirMod->ZoneUCSDUI(UINum).E_Kc != DataGlobalConstants::AutoCalculate) {
                ShowWarningError(state,
                                 "For RoomAirSettings:UnderFloorAirDistributionInterior for Zone " +
                                     state.dataRoomAirMod->ZoneUCSDUI(UINum).ZoneName +
                                     ", input for Coefficients A - E will be ignored when Floor Diffuser Type = HorizontalDisplacement.");
                ShowContinueError(state, "  To input these Coefficients, use Floor Diffuser Type = Custom.");
            }
            state.dataRoomAirMod->ZoneUCSDUI(UINum).A_Kc = 0.0;
            state.dataRoomAirMod->ZoneUCSDUI(UINum).B_Kc = 0.0;
            state.dataRoomAirMod->ZoneUCSDUI(UINum).C_Kc = 0.67;
            state.dataRoomAirMod->ZoneUCSDUI(UINum).D_Kc = 0.0;
            state.dataRoomAirMod->ZoneUCSDUI(UINum).E_Kc = 0.0;
        } else if (state.dataRoomAirMod->ZoneUCSDUI(UINum).DiffuserType == Diffuser::LinBarGrille) {
            if (state.dataRoomAirMod->ZoneUCSDUI(UINum).A_Kc != DataGlobalConstants::AutoCalculate ||
                state.dataRoomAirMod->ZoneUCSDUI(UINum).B_Kc != DataGlobalConstants::AutoCalculate ||
                state.dataRoomAirMod->ZoneUCSDUI(UINum).C_Kc != DataGlobalConstants::AutoCalculate ||
                state.dataRoomAirMod->ZoneUCSDUI(UINum).D_Kc != DataGlobalConstants::AutoCalculate ||
                state.dataRoomAirMod->ZoneUCSDUI(UINum).E_Kc != DataGlobalConstants::AutoCalculate) {
                ShowWarningError(state,
                                 "For RoomAirSettings:UnderFloorAirDistributionInterior for Zone " +
                                     state.dataRoomAirMod->ZoneUCSDUI(UINum).ZoneName +
                                     ", input for Coefficients A - E will be ignored when Floor Diffuser Type = LinearBarGrille.");
                ShowContinueError(state, "  To input these Coefficients, use Floor Diffuser Type = Custom.");
            }
            state.dataRoomAirMod->ZoneUCSDUI(UINum).A_Kc = 0.0;
            state.dataRoomAirMod->ZoneUCSDUI(UINum).B_Kc = 0.0;
            state.dataRoomAirMod->ZoneUCSDUI(UINum).C_Kc = 0.8;
            state.dataRoomAirMod->ZoneUCSDUI(UINum).D_Kc = 0.0;
            state.dataRoomAirMod->ZoneUCSDUI(UINum).E_Kc = 0.0;
        } else {
            if (state.dataRoomAirMod->ZoneUCSDUI(UINum).A_Kc == DataGlobalConstants::AutoCalculate ||
                state.dataRoomAirMod->ZoneUCSDUI(UINum).B_Kc == DataGlobalConstants::AutoCalculate ||
                state.dataRoomAirMod->ZoneUCSDUI(UINum).C_Kc == DataGlobalConstants::AutoCalculate ||
                state.dataRoomAirMod->ZoneUCSDUI(UINum).D_Kc == DataGlobalConstants::AutoCalculate ||
                state.dataRoomAirMod->ZoneUCSDUI(UINum).E_Kc == DataGlobalConstants::AutoCalculate) {
                ShowFatalError(state,
                               "For RoomAirSettings:UnderFloorAirDistributionInterior for Zone " + state.dataRoomAirMod->ZoneUCSDUI(UINum).ZoneName +
                                   ", input for Coefficients A - E must be specified when Floor Diffuser Type = Custom.");
            }
        }
        if (state.dataRoomAirMod->ZoneUCSDUI(UINum).PowerPerPlume == DataGlobalConstants::AutoCalculate) {
            NumberOfPlumes = 0.0;
            if (NumberOfOccupants > 0.0) {
                NumberOfPlumes = NumberOfOccupants;
            } else {
                NumberOfPlumes = 1.0;
            }
            ZoneElecConv = 0.0;
            for (int Ctd = 1; Ctd <= state.dataHeatBal->TotElecEquip; ++Ctd) {
                if (state.dataHeatBal->ZoneElectric(Ctd).ZonePtr == ZoneNum) {
                    ZoneElecConv += state.dataHeatBal->ZoneElectric(Ctd).DesignLevel * state.dataHeatBal->ZoneElectric(Ctd).FractionConvected;
                }
            }
            ZoneGasConv = 0.0;
            for (int Ctd = 1; Ctd <= state.dataHeatBal->TotGasEquip; ++Ctd) {
                if (state.dataHeatBal->ZoneGas(Ctd).ZonePtr == ZoneNum) {
                    ZoneGasConv += state.dataHeatBal->ZoneGas(Ctd).DesignLevel * state.dataHeatBal->ZoneGas(Ctd).FractionConvected;
                }
            }
            ZoneOthEqConv = 0.0;
            for (int Ctd = 1; Ctd <= state.dataHeatBal->TotOthEquip; ++Ctd) {
                if (state.dataHeatBal->ZoneOtherEq(Ctd).ZonePtr == ZoneNum) {
                    ZoneOthEqConv += state.dataHeatBal->ZoneOtherEq(Ctd).DesignLevel * state.dataHeatBal->ZoneOtherEq(Ctd).FractionConvected;
                }
            }
            ZoneHWEqConv = 0.0;
            for (int Ctd = 1; Ctd <= state.dataHeatBal->TotHWEquip; ++Ctd) {
                if (state.dataHeatBal->ZoneHWEq(Ctd).ZonePtr == ZoneNum) {
                    ZoneHWEqConv += state.dataHeatBal->ZoneHWEq(Ctd).DesignLevel * state.dataHeatBal->ZoneHWEq(Ctd).FractionConvected;
                }
            }
            for (int Ctd = 1; Ctd <= state.dataHeatBal->TotStmEquip; ++Ctd) {
                ZoneSteamEqConv = 0.0;
                if (state.dataHeatBal->ZoneSteamEq(Ctd).ZonePtr == ZoneNum) {
                    ZoneSteamEqConv += state.dataHeatBal->ZoneSteamEq(Ctd).DesignLevel * state.dataHeatBal->ZoneSteamEq(Ctd).FractionConvected;
                }
            }
            state.dataRoomAirMod->ZoneUCSDUI(UINum).PowerPerPlume =
                (NumberOfOccupants * 73.0 + ZoneElecConv + ZoneGasConv + ZoneOthEqConv + ZoneHWEqConv + ZoneSteamEqConv) / NumberOfPlumes;
            BaseSizer::reportSizerOutput(state,
                                         "RoomAirSettings:UnderFloorAirDistributionInterior",
                                         state.dataRoomAirMod->ZoneUCSDUI(UINum).ZoneName,
                                         "Power per plume [W]",
                                         state.dataRoomAirMod->ZoneUCSDUI(UINum).PowerPerPlume);
        }
        if (state.dataRoomAirMod->ZoneUCSDUI(UINum).DiffusersPerZone == AutoSize) {
            if (NumberOfOccupants > 0.0) {
                state.dataRoomAirMod->ZoneUCSDUI(UINum).DiffusersPerZone = NumberOfOccupants;
            } else {
                state.dataRoomAirMod->ZoneUCSDUI(UINum).DiffusersPerZone = 1.0;
            }
            BaseSizer::reportSizerOutput(state,
                                         "RoomAirSettings:UnderFloorAirDistributionInterior",
                                         state.dataRoomAirMod->ZoneUCSDUI(UINum).ZoneName,
                                         "Number of diffusers per zone",
                                         state.dataRoomAirMod->ZoneUCSDUI(UINum).DiffusersPerZone);
        }
    }

    if (ZoneModelType == DataRoomAirModel::RoomAirModel::UCSDUFE) {
        UINum = state.dataRoomAirMod->ZoneUFPtr(ZoneNum);
        // calculate total window width in zone
        for (int Ctd = state.dataUCSDShared->PosZ_Window((ZoneNum - 1) * 2 + 1); Ctd <= state.dataUCSDShared->PosZ_Window((ZoneNum - 1) * 2 + 2);
             ++Ctd) {
            int SurfNum = state.dataUCSDShared->APos_Window(Ctd);
            if (SurfNum == 0) continue;
            if (state.dataSurface->Surface(SurfNum).ExtBoundCond == ExternalEnvironment ||
                state.dataSurface->Surface(SurfNum).ExtBoundCond == OtherSideCoefNoCalcExt ||
                state.dataSurface->Surface(SurfNum).ExtBoundCond == OtherSideCoefCalcExt ||
                state.dataSurface->Surface(SurfNum).ExtBoundCond == OtherSideCondModeledExt) {
                state.dataRoomAirMod->ZoneUCSDUE(UINum).WinWidth += state.dataSurface->Surface(SurfNum).Width;
                ++state.dataRoomAirMod->ZoneUCSDUE(UINum).NumExtWin;
            }
        }
        if (state.dataRoomAirMod->ZoneUCSDUE(UINum).WinWidth <= 0.0) {
            ShowWarningError(state,
                             "For RoomAirSettings:UnderFloorAirDistributionExterior for Zone " + state.dataRoomAirMod->ZoneUCSDUE(UINum).ZoneName +
                                 " there are no exterior windows.");
            ShowContinueError(state, "  The zone will be treated as a UFAD interior zone");
        }
        NumberOfOccupants = 0.0;
        for (int Ctd = 1; Ctd <= state.dataHeatBal->TotPeople; ++Ctd) {
            if (state.dataHeatBal->People(Ctd).ZonePtr == ZoneNum) {
                NumberOfOccupants += state.dataHeatBal->People(Ctd).NumberOfPeople;
            }
        }
        if (state.dataRoomAirMod->ZoneUCSDUE(UINum).DiffArea == AutoSize) {
            if (state.dataRoomAirMod->ZoneUCSDUE(UINum).DiffuserType == Diffuser::Swirl) {
                state.dataRoomAirMod->ZoneUCSDUE(UINum).DiffArea = 0.0075;
            } else if (state.dataRoomAirMod->ZoneUCSDUE(UINum).DiffuserType == Diffuser::VarArea) {
                state.dataRoomAirMod->ZoneUCSDUE(UINum).DiffArea = 0.035;
            } else if (state.dataRoomAirMod->ZoneUCSDUE(UINum).DiffuserType == Diffuser::DisplVent) {
                state.dataRoomAirMod->ZoneUCSDUE(UINum).DiffArea = 0.0060;
            } else if (state.dataRoomAirMod->ZoneUCSDUE(UINum).DiffuserType == Diffuser::LinBarGrille) {
                // 4 ft x 4 inches; eff area is 50% of total area; 75 cfm per linear foot.
                state.dataRoomAirMod->ZoneUCSDUE(UINum).DiffArea = 0.03;
            } else {
                state.dataRoomAirMod->ZoneUCSDUE(UINum).DiffArea = 0.0075;
            }
            BaseSizer::reportSizerOutput(state,
                                         "RoomAirSettings:UnderFloorAirDistributionExterior",
                                         state.dataRoomAirMod->ZoneUCSDUE(UINum).ZoneName,
                                         "Design effective area of diffuser",
                                         state.dataRoomAirMod->ZoneUCSDUE(UINum).DiffArea);
        }
        if (state.dataRoomAirMod->ZoneUCSDUE(UINum).DiffAngle == AutoSize) {
            if (state.dataRoomAirMod->ZoneUCSDUE(UINum).DiffuserType == Diffuser::Swirl) {
                state.dataRoomAirMod->ZoneUCSDUE(UINum).DiffAngle = 28.0;
            } else if (state.dataRoomAirMod->ZoneUCSDUE(UINum).DiffuserType == Diffuser::VarArea) {
                state.dataRoomAirMod->ZoneUCSDUE(UINum).DiffAngle = 45.0;
            } else if (state.dataRoomAirMod->ZoneUCSDUE(UINum).DiffuserType == Diffuser::DisplVent) {
                state.dataRoomAirMod->ZoneUCSDUE(UINum).DiffAngle = 73.0;
            } else if (state.dataRoomAirMod->ZoneUCSDUE(UINum).DiffuserType == Diffuser::LinBarGrille) {
                state.dataRoomAirMod->ZoneUCSDUE(UINum).DiffAngle = 15.0;
            } else {
                state.dataRoomAirMod->ZoneUCSDUE(UINum).DiffAngle = 28.0;
            }
            BaseSizer::reportSizerOutput(state,
                                         "RoomAirSettings:UnderFloorAirDistributionExterior",
                                         state.dataRoomAirMod->ZoneUCSDUE(UINum).ZoneName,
                                         "Angle between diffuser slots and the vertical",
                                         state.dataRoomAirMod->ZoneUCSDUE(UINum).DiffAngle);
        }
        if (state.dataRoomAirMod->ZoneUCSDUE(UINum).TransHeight == AutoSize) {
            state.dataRoomAirMod->ZoneUCSDUE(UINum).CalcTransHeight = true;
            state.dataRoomAirMod->ZoneUCSDUE(UINum).TransHeight = 0.0;
        } else {
            state.dataRoomAirMod->ZoneUCSDUE(UINum).CalcTransHeight = false;
        }
        if (state.dataRoomAirMod->ZoneUCSDUE(UINum).DiffuserType == Diffuser::Swirl) {
            if (state.dataRoomAirMod->ZoneUCSDUE(UINum).A_Kc != DataGlobalConstants::AutoCalculate ||
                state.dataRoomAirMod->ZoneUCSDUE(UINum).B_Kc != DataGlobalConstants::AutoCalculate ||
                state.dataRoomAirMod->ZoneUCSDUE(UINum).C_Kc != DataGlobalConstants::AutoCalculate ||
                state.dataRoomAirMod->ZoneUCSDUE(UINum).D_Kc != DataGlobalConstants::AutoCalculate ||
                state.dataRoomAirMod->ZoneUCSDUE(UINum).E_Kc != DataGlobalConstants::AutoCalculate) {
                ShowWarningError(state,
                                 "For RoomAirSettings:UnderFloorAirDistributionExterior for Zone " +
                                     state.dataRoomAirMod->ZoneUCSDUE(UINum).ZoneName +
                                     ", input for Coefficients A - E will be ignored when Floor Diffuser Type = Swirl.");
                ShowContinueError(state, "  To input these Coefficients, use Floor Diffuser Type = Custom.");
            }
            state.dataRoomAirMod->ZoneUCSDUE(UINum).A_Kc = 0.0;
            state.dataRoomAirMod->ZoneUCSDUE(UINum).B_Kc = 0.0;
            state.dataRoomAirMod->ZoneUCSDUE(UINum).C_Kc = 0.6531;
            state.dataRoomAirMod->ZoneUCSDUE(UINum).D_Kc = 0.0069;
            state.dataRoomAirMod->ZoneUCSDUE(UINum).E_Kc = -0.00004;
        } else if (state.dataRoomAirMod->ZoneUCSDUE(UINum).DiffuserType == Diffuser::VarArea) {
            if (state.dataRoomAirMod->ZoneUCSDUE(UINum).A_Kc != DataGlobalConstants::AutoCalculate ||
                state.dataRoomAirMod->ZoneUCSDUE(UINum).B_Kc != DataGlobalConstants::AutoCalculate ||
                state.dataRoomAirMod->ZoneUCSDUE(UINum).C_Kc != DataGlobalConstants::AutoCalculate ||
                state.dataRoomAirMod->ZoneUCSDUE(UINum).D_Kc != DataGlobalConstants::AutoCalculate ||
                state.dataRoomAirMod->ZoneUCSDUE(UINum).E_Kc != DataGlobalConstants::AutoCalculate) {
                ShowWarningError(state,
                                 "For RoomAirSettings:UnderFloorAirDistributionExterior for Zone " +
                                     state.dataRoomAirMod->ZoneUCSDUE(UINum).ZoneName +
                                     ", input for Coefficients A - E will be ignored when Floor Diffuser Type = VariableArea.");
                ShowContinueError(state, "  To input these Coefficients, use Floor Diffuser Type = Custom.");
            }
            state.dataRoomAirMod->ZoneUCSDUE(UINum).A_Kc = 0.0;
            state.dataRoomAirMod->ZoneUCSDUE(UINum).B_Kc = 0.0;
            state.dataRoomAirMod->ZoneUCSDUE(UINum).C_Kc = 0.83;
            state.dataRoomAirMod->ZoneUCSDUE(UINum).D_Kc = 0.0;
            state.dataRoomAirMod->ZoneUCSDUE(UINum).E_Kc = 0.0;
        } else if (state.dataRoomAirMod->ZoneUCSDUE(UINum).DiffuserType == Diffuser::DisplVent) {
            if (state.dataRoomAirMod->ZoneUCSDUE(UINum).A_Kc != DataGlobalConstants::AutoCalculate ||
                state.dataRoomAirMod->ZoneUCSDUE(UINum).B_Kc != DataGlobalConstants::AutoCalculate ||
                state.dataRoomAirMod->ZoneUCSDUE(UINum).C_Kc != DataGlobalConstants::AutoCalculate ||
                state.dataRoomAirMod->ZoneUCSDUE(UINum).D_Kc != DataGlobalConstants::AutoCalculate ||
                state.dataRoomAirMod->ZoneUCSDUE(UINum).E_Kc != DataGlobalConstants::AutoCalculate) {
                ShowWarningError(state,
                                 "For RoomAirSettings:UnderFloorAirDistributionExterior for Zone " +
                                     state.dataRoomAirMod->ZoneUCSDUE(UINum).ZoneName +
                                     ", input for Coefficients A - E will be ignored when Floor Diffuser Type = HorizontalDisplacement.");
                ShowContinueError(state, "  To input these Coefficients, use Floor Diffuser Type = Custom.");
            }
            state.dataRoomAirMod->ZoneUCSDUE(UINum).A_Kc = 0.0;
            state.dataRoomAirMod->ZoneUCSDUE(UINum).B_Kc = 0.0;
            state.dataRoomAirMod->ZoneUCSDUE(UINum).C_Kc = 0.67;
            state.dataRoomAirMod->ZoneUCSDUE(UINum).D_Kc = 0.0;
            state.dataRoomAirMod->ZoneUCSDUE(UINum).E_Kc = 0.0;
        } else if (state.dataRoomAirMod->ZoneUCSDUE(UINum).DiffuserType == Diffuser::LinBarGrille) {
            if (state.dataRoomAirMod->ZoneUCSDUE(UINum).A_Kc != DataGlobalConstants::AutoCalculate ||
                state.dataRoomAirMod->ZoneUCSDUE(UINum).B_Kc != DataGlobalConstants::AutoCalculate ||
                state.dataRoomAirMod->ZoneUCSDUE(UINum).C_Kc != DataGlobalConstants::AutoCalculate ||
                state.dataRoomAirMod->ZoneUCSDUE(UINum).D_Kc != DataGlobalConstants::AutoCalculate ||
                state.dataRoomAirMod->ZoneUCSDUE(UINum).E_Kc != DataGlobalConstants::AutoCalculate) {
                ShowWarningError(state,
                                 "For RoomAirSettings:UnderFloorAirDistributionExterior for Zone " +
                                     state.dataRoomAirMod->ZoneUCSDUE(UINum).ZoneName +
                                     ", input for Coefficients A - E will be ignored when Floor Diffuser Type = LinearBarGrille.");
                ShowContinueError(state, "  To input these Coefficients, use Floor Diffuser Type = Custom.");
            }
            state.dataRoomAirMod->ZoneUCSDUE(UINum).A_Kc = 0.0;
            state.dataRoomAirMod->ZoneUCSDUE(UINum).B_Kc = 0.0;
            state.dataRoomAirMod->ZoneUCSDUE(UINum).C_Kc = 0.8214;
            state.dataRoomAirMod->ZoneUCSDUE(UINum).D_Kc = -0.0263;
            state.dataRoomAirMod->ZoneUCSDUE(UINum).E_Kc = 0.0014;
        } else {
            if (state.dataRoomAirMod->ZoneUCSDUE(UINum).A_Kc == DataGlobalConstants::AutoCalculate ||
                state.dataRoomAirMod->ZoneUCSDUE(UINum).B_Kc == DataGlobalConstants::AutoCalculate ||
                state.dataRoomAirMod->ZoneUCSDUE(UINum).C_Kc == DataGlobalConstants::AutoCalculate ||
                state.dataRoomAirMod->ZoneUCSDUE(UINum).D_Kc == DataGlobalConstants::AutoCalculate ||
                state.dataRoomAirMod->ZoneUCSDUE(UINum).E_Kc == DataGlobalConstants::AutoCalculate) {
                ShowFatalError(state,
                               "For RoomAirSettings:UnderFloorAirDistributionExterior for Zone " + state.dataRoomAirMod->ZoneUCSDUE(UINum).ZoneName +
                                   ", input for Coefficients A - E must be specified when Floor Diffuser Type = Custom.");
            }
        }
        if (state.dataRoomAirMod->ZoneUCSDUE(UINum).PowerPerPlume == DataGlobalConstants::AutoCalculate) {
            if (NumberOfOccupants > 0) {
                NumberOfPlumes = NumberOfOccupants;
            } else {
                NumberOfPlumes = 1.0;
            }
            ZoneElecConv = 0.0;
            for (int Ctd = 1; Ctd <= state.dataHeatBal->TotElecEquip; ++Ctd) {
                if (state.dataHeatBal->ZoneElectric(Ctd).ZonePtr == ZoneNum) {
                    ZoneElecConv += state.dataHeatBal->ZoneElectric(Ctd).DesignLevel;
                }
            }
            ZoneGasConv = 0.0;
            for (int Ctd = 1; Ctd <= state.dataHeatBal->TotGasEquip; ++Ctd) {
                if (state.dataHeatBal->ZoneGas(Ctd).ZonePtr == ZoneNum) {
                    ZoneGasConv += state.dataHeatBal->ZoneGas(Ctd).DesignLevel;
                }
            }
            ZoneOthEqConv = 0.0;
            for (int Ctd = 1; Ctd <= state.dataHeatBal->TotOthEquip; ++Ctd) {
                if (state.dataHeatBal->ZoneOtherEq(Ctd).ZonePtr == ZoneNum) {
                    ZoneOthEqConv += state.dataHeatBal->ZoneOtherEq(Ctd).DesignLevel;
                }
            }
            ZoneHWEqConv = 0.0;
            for (int Ctd = 1; Ctd <= state.dataHeatBal->TotHWEquip; ++Ctd) {
                if (state.dataHeatBal->ZoneHWEq(Ctd).ZonePtr == ZoneNum) {
                    ZoneHWEqConv += state.dataHeatBal->ZoneHWEq(Ctd).DesignLevel;
                }
            }
            for (int Ctd = 1; Ctd <= state.dataHeatBal->TotStmEquip; ++Ctd) {
                ZoneSteamEqConv = 0.0;
                if (state.dataHeatBal->ZoneSteamEq(Ctd).ZonePtr == ZoneNum) {
                    ZoneSteamEqConv += state.dataHeatBal->ZoneSteamEq(Ctd).DesignLevel;
                }
            }
            state.dataRoomAirMod->ZoneUCSDUE(UINum).PowerPerPlume =
                (NumberOfOccupants * 73.0 + ZoneElecConv + ZoneGasConv + ZoneOthEqConv + ZoneHWEqConv + ZoneSteamEqConv) / NumberOfPlumes;
            BaseSizer::reportSizerOutput(state,
                                         "RoomAirSettings:UnderFloorAirDistributionExterior",
                                         state.dataRoomAirMod->ZoneUCSDUE(UINum).ZoneName,
                                         "Power per plume [W]",
                                         state.dataRoomAirMod->ZoneUCSDUE(UINum).PowerPerPlume);
        }
        if (state.dataRoomAirMod->ZoneUCSDUE(UINum).DiffusersPerZone == AutoSize) {
            if (NumberOfOccupants > 0.0) {
                state.dataRoomAirMod->ZoneUCSDUE(UINum).DiffusersPerZone = NumberOfOccupants;
            } else {
                state.dataRoomAirMod->ZoneUCSDUE(UINum).DiffusersPerZone = 1.0;
            }
            BaseSizer::reportSizerOutput(state,
                                         "RoomAirSettings:UnderFloorAirDistributionExterior",
                                         state.dataRoomAirMod->ZoneUCSDUE(UINum).ZoneName,
                                         "Number of diffusers per zone",
                                         state.dataRoomAirMod->ZoneUCSDUE(UINum).DiffusersPerZone);
        }
    }
}

void HcUCSDUF(EnergyPlusData &state, int const ZoneNum, Real64 const FractionHeight)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         G. Carrilho da Graca
    //       DATE WRITTEN   February 2004
    //       MODIFIED       -
    //       RE-ENGINEERED  -

    // PURPOSE OF THIS SUBROUTINE:
    // Main subroutine for convection calculation in the UCSD Displacement Ventilation model.
    // It calls CalcDetailedHcInForDVModel for convection coefficient
    // initial calculations and averages the final result comparing the position of the surface with
    // the interface subzone height.

    // Using/Aliasing
    using namespace DataHeatBalFanSys;
    using namespace DataEnvironment;
    using namespace DataHeatBalance;

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int Ctd;         // DO loop counter for surfaces
    Real64 HLD;      // Convection coefficient for the lower area of surface
    Real64 TmedDV;   // Average temperature for DV
    Real64 Z1;       // auxiliary var for lowest height
    Real64 Z2;       // auxiliary var for highest height
    Real64 ZSupSurf; // highest height for this surface
    Real64 ZInfSurf; // lowest height for this surface
    Real64 HLU;      // Convection coefficient for the upper area of surface
    Real64 LayH;     // Height of the Occupied/Mixed subzone interface
    Real64 LayFrac;  // Fraction height of the Occupied/Mixed subzone interface
    int SurfNum;     // Surface number
    // Initialize HAT and HA

    state.dataUFADManager->HAT_MX = 0.0;
    state.dataUFADManager->HAT_OC = 0.0;
    state.dataUFADManager->HA_MX = 0.0;
    state.dataUFADManager->HA_OC = 0.0;
    state.dataUFADManager->HAT_FLOOR = 0.0;
    state.dataUFADManager->HA_FLOOR = 0.0;
    state.dataUFADManager->HAT_MXWin = 0.0;
    state.dataUFADManager->HAT_OCWin = 0.0;
    state.dataUFADManager->HA_MXWin = 0.0;
    state.dataUFADManager->HA_OCWin = 0.0;

    // Is the air flow model for this zone set to UCSDDV Displacement Ventilation?
    if (state.dataRoomAirMod->IsZoneUI(ZoneNum)) {
        LayFrac = FractionHeight;
        LayH = FractionHeight *
               (state.dataRoomAirMod->ZoneCeilingHeight((ZoneNum - 1) * 2 + 2) - state.dataRoomAirMod->ZoneCeilingHeight((ZoneNum - 1) * 2 + 1));
        // WALL Hc, HA and HAT calculation
        for (Ctd = state.dataUCSDShared->PosZ_Wall((ZoneNum - 1) * 2 + 1); Ctd <= state.dataUCSDShared->PosZ_Wall((ZoneNum - 1) * 2 + 2); ++Ctd) {
            SurfNum = state.dataUCSDShared->APos_Wall(Ctd);
            state.dataSurface->SurfTAirRef(SurfNum) = AdjacentAirTemp;
            if (SurfNum == 0) continue;
            Z1 = minval(state.dataSurface->Surface(SurfNum).Vertex({1, state.dataSurface->Surface(SurfNum).Sides}), &Vector::z);
            Z2 = maxval(state.dataSurface->Surface(SurfNum).Vertex({1, state.dataSurface->Surface(SurfNum).Sides}), &Vector::z);
            ZSupSurf = Z2 - state.dataRoomAirMod->ZoneCeilingHeight((ZoneNum - 1) * 2 + 1);
            ZInfSurf = Z1 - state.dataRoomAirMod->ZoneCeilingHeight((ZoneNum - 1) * 2 + 1);

            // The Wall surface is in the upper subzone
            if (ZInfSurf > LayH) {
                state.dataHeatBal->SurfTempEffBulkAir(SurfNum) = state.dataRoomAirMod->ZTMX(ZoneNum);
                CalcDetailedHcInForDVModel(state, SurfNum, state.dataHeatBalSurf->TempSurfIn, state.dataRoomAirMod->UFHcIn);
                state.dataUCSDShared->HWall(Ctd) = state.dataRoomAirMod->UFHcIn(SurfNum);
                state.dataUFADManager->HAT_MX +=
                    state.dataSurface->Surface(SurfNum).Area * state.dataHeatBalSurf->TempSurfIn(SurfNum) * state.dataUCSDShared->HWall(Ctd);
                state.dataUFADManager->HA_MX += state.dataSurface->Surface(SurfNum).Area * state.dataUCSDShared->HWall(Ctd);
            }

            // The Wall surface is in the lower subzone
            if (ZSupSurf < LayH) {
                state.dataHeatBal->SurfTempEffBulkAir(SurfNum) = state.dataRoomAirMod->ZTOC(ZoneNum);
                CalcDetailedHcInForDVModel(state, SurfNum, state.dataHeatBalSurf->TempSurfIn, state.dataRoomAirMod->UFHcIn);
                state.dataUCSDShared->HWall(Ctd) = state.dataRoomAirMod->UFHcIn(SurfNum);
                state.dataUFADManager->HAT_OC +=
                    state.dataSurface->Surface(SurfNum).Area * state.dataHeatBalSurf->TempSurfIn(SurfNum) * state.dataUCSDShared->HWall(Ctd);
                state.dataUFADManager->HA_OC += state.dataSurface->Surface(SurfNum).Area * state.dataUCSDShared->HWall(Ctd);
            }

            if (std::abs(ZInfSurf - ZSupSurf) < 1.e-10) {
                ShowSevereError(state, "RoomAirModelUFAD:HcUCSDUF: Surface values will cause divide by zero.");
                ShowContinueError(state,
                                  "Zone=\"" + state.dataHeatBal->Zone(state.dataSurface->Surface(SurfNum).Zone).Name + "\", Surface=\"" +
                                      state.dataSurface->Surface(SurfNum).Name + "\".");
                ShowContinueError(state, format("ZInfSurf=[{:.4R}], LayH=[{:.4R}].", ZInfSurf, LayH));
                ShowContinueError(state, format("ZSupSurf=[{:.4R}], LayH=[{:.4R}].", ZSupSurf, LayH));
                ShowFatalError(state, "...Previous condition causes termination.");
            }

            // The Wall surface is partially in upper and partially in lower subzone
            if (ZInfSurf <= LayH && ZSupSurf >= LayH) {
                state.dataHeatBal->SurfTempEffBulkAir(SurfNum) = state.dataRoomAirMod->ZTMX(ZoneNum);
                CalcDetailedHcInForDVModel(state, SurfNum, state.dataHeatBalSurf->TempSurfIn, state.dataRoomAirMod->UFHcIn);
                HLU = state.dataRoomAirMod->UFHcIn(SurfNum);
                state.dataHeatBal->SurfTempEffBulkAir(SurfNum) = state.dataRoomAirMod->ZTOC(ZoneNum);
                CalcDetailedHcInForDVModel(state, SurfNum, state.dataHeatBalSurf->TempSurfIn, state.dataRoomAirMod->UFHcIn);
                HLD = state.dataRoomAirMod->UFHcIn(SurfNum);
                TmedDV = ((ZSupSurf - LayH) * state.dataRoomAirMod->ZTMX(ZoneNum) + (LayH - ZInfSurf) * state.dataRoomAirMod->ZTOC(ZoneNum)) /
                         (ZSupSurf - ZInfSurf);
                state.dataUCSDShared->HWall(Ctd) = ((LayH - ZInfSurf) * HLD + (ZSupSurf - LayH) * HLU) / (ZSupSurf - ZInfSurf);
                state.dataUFADManager->HAT_MX += state.dataSurface->Surface(SurfNum).Area * (ZSupSurf - LayH) / (ZSupSurf - ZInfSurf) *
                                                 state.dataHeatBalSurf->TempSurfIn(SurfNum) * HLU;
                state.dataUFADManager->HA_MX += state.dataSurface->Surface(SurfNum).Area * (ZSupSurf - LayH) / (ZSupSurf - ZInfSurf) * HLU;
                state.dataUFADManager->HAT_OC += state.dataSurface->Surface(SurfNum).Area * (LayH - ZInfSurf) / (ZSupSurf - ZInfSurf) *
                                                 state.dataHeatBalSurf->TempSurfIn(SurfNum) * HLD;
                state.dataUFADManager->HA_OC += state.dataSurface->Surface(SurfNum).Area * (LayH - ZInfSurf) / (ZSupSurf - ZInfSurf) * HLD;
                state.dataHeatBal->SurfTempEffBulkAir(SurfNum) = TmedDV;
            }

            state.dataRoomAirMod->UFHcIn(SurfNum) = state.dataUCSDShared->HWall(Ctd);

        } // END WALL

        // WINDOW Hc, HA and HAT CALCULATION
        for (Ctd = state.dataUCSDShared->PosZ_Window((ZoneNum - 1) * 2 + 1); Ctd <= state.dataUCSDShared->PosZ_Window((ZoneNum - 1) * 2 + 2); ++Ctd) {
            SurfNum = state.dataUCSDShared->APos_Window(Ctd);
            state.dataSurface->SurfTAirRef(SurfNum) = AdjacentAirTemp;
            if (SurfNum == 0) continue;
            if (state.dataSurface->Surface(SurfNum).Tilt > 10.0 && state.dataSurface->Surface(SurfNum).Tilt < 170.0) { // Window Wall
                Z1 = minval(state.dataSurface->Surface(SurfNum).Vertex({1, state.dataSurface->Surface(SurfNum).Sides}), &Vector::z);
                Z2 = maxval(state.dataSurface->Surface(SurfNum).Vertex({1, state.dataSurface->Surface(SurfNum).Sides}), &Vector::z);
                ZSupSurf = Z2 - state.dataRoomAirMod->ZoneCeilingHeight((ZoneNum - 1) * 2 + 1);
                ZInfSurf = Z1 - state.dataRoomAirMod->ZoneCeilingHeight((ZoneNum - 1) * 2 + 1);

                if (ZInfSurf > LayH) {
                    state.dataHeatBal->SurfTempEffBulkAir(SurfNum) = state.dataRoomAirMod->ZTMX(ZoneNum);
                    CalcDetailedHcInForDVModel(state, SurfNum, state.dataHeatBalSurf->TempSurfIn, state.dataRoomAirMod->UFHcIn);
                    state.dataUCSDShared->HWindow(Ctd) = state.dataRoomAirMod->UFHcIn(SurfNum);
                    state.dataUFADManager->HAT_MX +=
                        state.dataSurface->Surface(SurfNum).Area * state.dataHeatBalSurf->TempSurfIn(SurfNum) * state.dataUCSDShared->HWindow(Ctd);
                    state.dataUFADManager->HA_MX += state.dataSurface->Surface(SurfNum).Area * state.dataUCSDShared->HWindow(Ctd);
                    state.dataUFADManager->HAT_MXWin +=
                        state.dataSurface->Surface(SurfNum).Area * state.dataHeatBalSurf->TempSurfIn(SurfNum) * state.dataUCSDShared->HWindow(Ctd);
                    state.dataUFADManager->HA_MXWin += state.dataSurface->Surface(SurfNum).Area * state.dataUCSDShared->HWindow(Ctd);
                }

                if (ZSupSurf < LayH) {
                    state.dataHeatBal->SurfTempEffBulkAir(SurfNum) = state.dataRoomAirMod->ZTOC(ZoneNum);
                    CalcDetailedHcInForDVModel(state, SurfNum, state.dataHeatBalSurf->TempSurfIn, state.dataRoomAirMod->UFHcIn);
                    state.dataUCSDShared->HWindow(Ctd) = state.dataRoomAirMod->UFHcIn(SurfNum);
                    state.dataUFADManager->HAT_OC +=
                        state.dataSurface->Surface(SurfNum).Area * state.dataHeatBalSurf->TempSurfIn(SurfNum) * state.dataUCSDShared->HWindow(Ctd);
                    state.dataUFADManager->HA_OC += state.dataSurface->Surface(SurfNum).Area * state.dataUCSDShared->HWindow(Ctd);
                    state.dataUFADManager->HAT_OCWin +=
                        state.dataSurface->Surface(SurfNum).Area * state.dataHeatBalSurf->TempSurfIn(SurfNum) * state.dataUCSDShared->HWindow(Ctd);
                    state.dataUFADManager->HA_OCWin += state.dataSurface->Surface(SurfNum).Area * state.dataUCSDShared->HWindow(Ctd);
                }

                if (ZInfSurf <= LayH && ZSupSurf >= LayH) {
                    state.dataHeatBal->SurfTempEffBulkAir(SurfNum) = state.dataRoomAirMod->ZTMX(ZoneNum);
                    CalcDetailedHcInForDVModel(state, SurfNum, state.dataHeatBalSurf->TempSurfIn, state.dataRoomAirMod->UFHcIn);
                    HLU = state.dataRoomAirMod->UFHcIn(SurfNum);
                    state.dataHeatBal->SurfTempEffBulkAir(SurfNum) = state.dataRoomAirMod->ZTOC(ZoneNum);
                    CalcDetailedHcInForDVModel(state, SurfNum, state.dataHeatBalSurf->TempSurfIn, state.dataRoomAirMod->UFHcIn);
                    HLD = state.dataRoomAirMod->UFHcIn(SurfNum);
                    TmedDV = ((ZSupSurf - LayH) * state.dataRoomAirMod->ZTMX(ZoneNum) + (LayH - ZInfSurf) * state.dataRoomAirMod->ZTOC(ZoneNum)) /
                             (ZSupSurf - ZInfSurf);
                    state.dataUCSDShared->HWindow(Ctd) = ((LayH - ZInfSurf) * HLD + (ZSupSurf - LayH) * HLU) / (ZSupSurf - ZInfSurf);
                    state.dataUFADManager->HAT_MX += state.dataSurface->Surface(SurfNum).Area * (ZSupSurf - LayH) / (ZSupSurf - ZInfSurf) *
                                                     state.dataHeatBalSurf->TempSurfIn(SurfNum) * HLU;
                    state.dataUFADManager->HA_MX += state.dataSurface->Surface(SurfNum).Area * (ZSupSurf - LayH) / (ZSupSurf - ZInfSurf) * HLU;
                    state.dataUFADManager->HAT_MXWin += state.dataSurface->Surface(SurfNum).Area * (ZSupSurf - LayH) / (ZSupSurf - ZInfSurf) *
                                                        state.dataHeatBalSurf->TempSurfIn(SurfNum) * HLU;
                    state.dataUFADManager->HA_MXWin += state.dataSurface->Surface(SurfNum).Area * (ZSupSurf - LayH) / (ZSupSurf - ZInfSurf) * HLU;
                    state.dataUFADManager->HAT_OC += state.dataSurface->Surface(SurfNum).Area * (LayH - ZInfSurf) / (ZSupSurf - ZInfSurf) *
                                                     state.dataHeatBalSurf->TempSurfIn(SurfNum) * HLD;
                    state.dataUFADManager->HA_OC += state.dataSurface->Surface(SurfNum).Area * (LayH - ZInfSurf) / (ZSupSurf - ZInfSurf) * HLD;
                    state.dataUFADManager->HAT_OCWin += state.dataSurface->Surface(SurfNum).Area * (LayH - ZInfSurf) / (ZSupSurf - ZInfSurf) *
                                                        state.dataHeatBalSurf->TempSurfIn(SurfNum) * HLD;
                    state.dataUFADManager->HA_OCWin += state.dataSurface->Surface(SurfNum).Area * (LayH - ZInfSurf) / (ZSupSurf - ZInfSurf) * HLD;
                    state.dataHeatBal->SurfTempEffBulkAir(SurfNum) = TmedDV;
                }
            }

            if (state.dataSurface->Surface(SurfNum).Tilt <= 10.0) { // Window Ceiling
                state.dataHeatBal->SurfTempEffBulkAir(SurfNum) = state.dataRoomAirMod->ZTMX(ZoneNum);
                CalcDetailedHcInForDVModel(state, SurfNum, state.dataHeatBalSurf->TempSurfIn, state.dataRoomAirMod->UFHcIn);
                state.dataUCSDShared->HWindow(Ctd) = state.dataRoomAirMod->UFHcIn(SurfNum);
                state.dataUFADManager->HAT_MX +=
                    state.dataSurface->Surface(SurfNum).Area * state.dataHeatBalSurf->TempSurfIn(SurfNum) * state.dataUCSDShared->HWindow(Ctd);
                state.dataUFADManager->HA_MX += state.dataSurface->Surface(SurfNum).Area * state.dataUCSDShared->HWindow(Ctd);
            }

            if (state.dataSurface->Surface(SurfNum).Tilt >= 170.0) { // Window Floor
                state.dataHeatBal->SurfTempEffBulkAir(SurfNum) = state.dataRoomAirMod->ZTOC(ZoneNum);
                CalcDetailedHcInForDVModel(state, SurfNum, state.dataHeatBalSurf->TempSurfIn, state.dataRoomAirMod->UFHcIn);
                state.dataUCSDShared->HWindow(Ctd) = state.dataRoomAirMod->UFHcIn(SurfNum);
                state.dataUFADManager->HAT_OC +=
                    state.dataSurface->Surface(SurfNum).Area * state.dataHeatBalSurf->TempSurfIn(SurfNum) * state.dataUCSDShared->HWindow(Ctd);
                state.dataUFADManager->HA_OC += state.dataSurface->Surface(SurfNum).Area * state.dataUCSDShared->HWindow(Ctd);
            }

            state.dataRoomAirMod->UFHcIn(SurfNum) = state.dataUCSDShared->HWindow(Ctd);

        } // END WINDOW

        // DOOR Hc, HA and HAT CALCULATION
        for (Ctd = state.dataUCSDShared->PosZ_Door((ZoneNum - 1) * 2 + 1); Ctd <= state.dataUCSDShared->PosZ_Door((ZoneNum - 1) * 2 + 2);
             ++Ctd) { // DOOR
            SurfNum = state.dataUCSDShared->APos_Door(Ctd);
            state.dataSurface->SurfTAirRef(SurfNum) = AdjacentAirTemp;
            if (SurfNum == 0) continue;
            Z1 = minval(state.dataSurface->Surface(SurfNum).Vertex({1, state.dataSurface->Surface(SurfNum).Sides}), &Vector::z);
            Z2 = maxval(state.dataSurface->Surface(SurfNum).Vertex({1, state.dataSurface->Surface(SurfNum).Sides}), &Vector::z);
            ZSupSurf = Z2 - state.dataRoomAirMod->ZoneCeilingHeight((ZoneNum - 1) * 2 + 1);
            ZInfSurf = Z1 - state.dataRoomAirMod->ZoneCeilingHeight((ZoneNum - 1) * 2 + 1);

            if (ZInfSurf > LayH) {
                state.dataHeatBal->SurfTempEffBulkAir(SurfNum) = state.dataRoomAirMod->ZTMX(ZoneNum);
                CalcDetailedHcInForDVModel(state, SurfNum, state.dataHeatBalSurf->TempSurfIn, state.dataRoomAirMod->UFHcIn);
                state.dataUCSDShared->HDoor(Ctd) = state.dataRoomAirMod->UFHcIn(SurfNum);
                state.dataUFADManager->HAT_MX +=
                    state.dataSurface->Surface(SurfNum).Area * state.dataHeatBalSurf->TempSurfIn(SurfNum) * state.dataUCSDShared->HDoor(Ctd);
                state.dataUFADManager->HA_MX += state.dataSurface->Surface(SurfNum).Area * state.dataUCSDShared->HDoor(Ctd);
            }

            if (ZSupSurf < LayH) {
                state.dataHeatBal->SurfTempEffBulkAir(SurfNum) = state.dataRoomAirMod->ZTOC(ZoneNum);
                CalcDetailedHcInForDVModel(state, SurfNum, state.dataHeatBalSurf->TempSurfIn, state.dataRoomAirMod->UFHcIn);
                state.dataUCSDShared->HDoor(Ctd) = state.dataRoomAirMod->UFHcIn(SurfNum);
                state.dataUFADManager->HAT_OC +=
                    state.dataSurface->Surface(SurfNum).Area * state.dataHeatBalSurf->TempSurfIn(SurfNum) * state.dataUCSDShared->HDoor(Ctd);
                state.dataUFADManager->HA_OC += state.dataSurface->Surface(SurfNum).Area * state.dataUCSDShared->HDoor(Ctd);
            }

            if (ZInfSurf <= LayH && ZSupSurf >= LayH) {
                state.dataHeatBal->SurfTempEffBulkAir(SurfNum) = state.dataRoomAirMod->ZTMX(ZoneNum);
                CalcDetailedHcInForDVModel(state, SurfNum, state.dataHeatBalSurf->TempSurfIn, state.dataRoomAirMod->UFHcIn);
                HLU = state.dataRoomAirMod->UFHcIn(SurfNum);
                state.dataHeatBal->SurfTempEffBulkAir(SurfNum) = state.dataRoomAirMod->ZTOC(ZoneNum);
                CalcDetailedHcInForDVModel(state, SurfNum, state.dataHeatBalSurf->TempSurfIn, state.dataRoomAirMod->UFHcIn);
                HLD = state.dataRoomAirMod->UFHcIn(SurfNum);
                TmedDV = ((ZSupSurf - LayH) * state.dataRoomAirMod->ZTMX(ZoneNum) + (LayH - ZInfSurf) * state.dataRoomAirMod->ZTOC(ZoneNum)) /
                         (ZSupSurf - ZInfSurf);
                state.dataUCSDShared->HDoor(Ctd) = ((LayH - ZInfSurf) * HLD + (ZSupSurf - LayH) * HLU) / (ZSupSurf - ZInfSurf);
                state.dataUFADManager->HAT_MX += state.dataSurface->Surface(SurfNum).Area * (ZSupSurf - LayH) / (ZSupSurf - ZInfSurf) *
                                                 state.dataHeatBalSurf->TempSurfIn(SurfNum) * HLU;
                state.dataUFADManager->HA_MX += state.dataSurface->Surface(SurfNum).Area * (ZSupSurf - LayH) / (ZSupSurf - ZInfSurf) * HLU;
                state.dataUFADManager->HAT_OC += state.dataSurface->Surface(SurfNum).Area * (LayH - ZInfSurf) / (ZSupSurf - ZInfSurf) *
                                                 state.dataHeatBalSurf->TempSurfIn(SurfNum) * HLD;
                state.dataUFADManager->HA_OC += state.dataSurface->Surface(SurfNum).Area * (LayH - ZInfSurf) / (ZSupSurf - ZInfSurf) * HLD;
                state.dataHeatBal->SurfTempEffBulkAir(SurfNum) = TmedDV;
            }

            state.dataRoomAirMod->UFHcIn(SurfNum) = state.dataUCSDShared->HDoor(Ctd);

        } // END DOOR

        // INTERNAL Hc, HA and HAT CALCULATION
        state.dataUFADManager->HeightIntMass =
            min(state.dataUFADManager->HeightIntMassDefault,
                (state.dataRoomAirMod->ZoneCeilingHeight((ZoneNum - 1) * 2 + 2) - state.dataRoomAirMod->ZoneCeilingHeight((ZoneNum - 1) * 2 + 1)));
        for (Ctd = state.dataUCSDShared->PosZ_Internal((ZoneNum - 1) * 2 + 1); Ctd <= state.dataUCSDShared->PosZ_Internal((ZoneNum - 1) * 2 + 2);
             ++Ctd) {
            SurfNum = state.dataUCSDShared->APos_Internal(Ctd);
            state.dataSurface->SurfTAirRef(SurfNum) = AdjacentAirTemp;
            if (SurfNum == 0) continue;
            ZSupSurf = state.dataUFADManager->HeightIntMass;
            ZInfSurf = 0.0;

            if (ZSupSurf < LayH) {
                state.dataHeatBal->SurfTempEffBulkAir(SurfNum) = state.dataRoomAirMod->ZTOC(ZoneNum);
                CalcDetailedHcInForDVModel(state, SurfNum, state.dataHeatBalSurf->TempSurfIn, state.dataRoomAirMod->UFHcIn);
                state.dataUCSDShared->HInternal(Ctd) = state.dataRoomAirMod->UFHcIn(SurfNum);
                state.dataUFADManager->HAT_OC +=
                    state.dataSurface->Surface(SurfNum).Area * state.dataHeatBalSurf->TempSurfIn(SurfNum) * state.dataUCSDShared->HInternal(Ctd);
                state.dataUFADManager->HA_OC += state.dataSurface->Surface(SurfNum).Area * state.dataUCSDShared->HInternal(Ctd);
            }

            if (ZInfSurf <= LayH && ZSupSurf >= LayH) {
                state.dataHeatBal->SurfTempEffBulkAir(SurfNum) = state.dataRoomAirMod->ZTMX(ZoneNum);
                CalcDetailedHcInForDVModel(state, SurfNum, state.dataHeatBalSurf->TempSurfIn, state.dataRoomAirMod->UFHcIn);
                HLU = state.dataRoomAirMod->UFHcIn(SurfNum);
                state.dataHeatBal->SurfTempEffBulkAir(SurfNum) = state.dataRoomAirMod->ZTOC(ZoneNum);
                CalcDetailedHcInForDVModel(state, SurfNum, state.dataHeatBalSurf->TempSurfIn, state.dataRoomAirMod->UFHcIn);
                HLD = state.dataRoomAirMod->UFHcIn(SurfNum);
                TmedDV = ((ZSupSurf - LayH) * state.dataRoomAirMod->ZTMX(ZoneNum) + (LayH - ZInfSurf) * state.dataRoomAirMod->ZTOC(ZoneNum)) /
                         (ZSupSurf - ZInfSurf);
                state.dataUCSDShared->HInternal(Ctd) = ((LayH - ZInfSurf) * HLD + (ZSupSurf - LayH) * HLU) / (ZSupSurf - ZInfSurf);
                state.dataUFADManager->HAT_MX += state.dataSurface->Surface(SurfNum).Area * (ZSupSurf - LayH) / (ZSupSurf - ZInfSurf) *
                                                 state.dataHeatBalSurf->TempSurfIn(SurfNum) * HLU;
                state.dataUFADManager->HA_MX += state.dataSurface->Surface(SurfNum).Area * (ZSupSurf - LayH) / (ZSupSurf - ZInfSurf) * HLU;
                state.dataUFADManager->HAT_OC += state.dataSurface->Surface(SurfNum).Area * (LayH - ZInfSurf) / (ZSupSurf - ZInfSurf) *
                                                 state.dataHeatBalSurf->TempSurfIn(SurfNum) * HLD;
                state.dataUFADManager->HA_OC += state.dataSurface->Surface(SurfNum).Area * (LayH - ZInfSurf) / (ZSupSurf - ZInfSurf) * HLD;
                state.dataHeatBal->SurfTempEffBulkAir(SurfNum) = TmedDV;
            }

            state.dataRoomAirMod->UFHcIn(SurfNum) = state.dataUCSDShared->HInternal(Ctd);
        } // END INTERNAL

        // CEILING Hc, HA and HAT CALCULATION
        for (Ctd = state.dataUCSDShared->PosZ_Ceiling((ZoneNum - 1) * 2 + 1); Ctd <= state.dataUCSDShared->PosZ_Ceiling((ZoneNum - 1) * 2 + 2);
             ++Ctd) {
            SurfNum = state.dataUCSDShared->APos_Ceiling(Ctd);
            state.dataSurface->SurfTAirRef(SurfNum) = AdjacentAirTemp;
            if (SurfNum == 0) continue;
            state.dataHeatBal->SurfTempEffBulkAir(SurfNum) = state.dataRoomAirMod->ZTMX(ZoneNum);
            CalcDetailedHcInForDVModel(state, SurfNum, state.dataHeatBalSurf->TempSurfIn, state.dataRoomAirMod->UFHcIn);
            state.dataUCSDShared->HCeiling(Ctd) = state.dataRoomAirMod->UFHcIn(SurfNum);
            state.dataUFADManager->HAT_MX +=
                state.dataSurface->Surface(SurfNum).Area * state.dataHeatBalSurf->TempSurfIn(SurfNum) * state.dataUCSDShared->HCeiling(Ctd);
            state.dataUFADManager->HA_MX += state.dataSurface->Surface(SurfNum).Area * state.dataUCSDShared->HCeiling(Ctd);
            state.dataRoomAirMod->UFHcIn(SurfNum) = state.dataUCSDShared->HCeiling(Ctd);
        } // END CEILING

        // FLOOR Hc, HA and HAT CALCULATION
        for (Ctd = state.dataUCSDShared->PosZ_Floor((ZoneNum - 1) * 2 + 1); Ctd <= state.dataUCSDShared->PosZ_Floor((ZoneNum - 1) * 2 + 2); ++Ctd) {
            SurfNum = state.dataUCSDShared->APos_Floor(Ctd);
            state.dataSurface->SurfTAirRef(SurfNum) = AdjacentAirTemp;
            if (SurfNum == 0) continue;
            state.dataHeatBal->SurfTempEffBulkAir(SurfNum) = state.dataRoomAirMod->ZTFloor(ZoneNum);
            CalcDetailedHcInForDVModel(state, SurfNum, state.dataHeatBalSurf->TempSurfIn, state.dataRoomAirMod->UFHcIn);
            state.dataUCSDShared->HFloor(Ctd) = state.dataRoomAirMod->UFHcIn(SurfNum);
            state.dataUFADManager->HAT_OC +=
                state.dataSurface->Surface(SurfNum).Area * state.dataHeatBalSurf->TempSurfIn(SurfNum) * state.dataUCSDShared->HFloor(Ctd);
            state.dataUFADManager->HA_OC += state.dataSurface->Surface(SurfNum).Area * state.dataUCSDShared->HFloor(Ctd);
            state.dataHeatBal->SurfTempEffBulkAir(SurfNum) = state.dataRoomAirMod->ZTFloor(ZoneNum);
            state.dataRoomAirMod->UFHcIn(SurfNum) = state.dataUCSDShared->HFloor(Ctd);
        } // END FLOOR
    }
}

void CalcUCSDUI(EnergyPlusData &state, int const ZoneNum) // index number for the specified zone
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Fred Buhl
    //       DATE WRITTEN   August 2005
    //       MODIFIED       Brent Griffith June 2008 for new interpolation and time history
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // Using the UCSD UFAD interior zone model, this subroutine calculates the  occupied subzone height,
    // surface heat transfer coefficients, the occupied subzone temperature, and the upper subzone temperature.

    // METHODOLOGY EMPLOYED:
    // The zone is divided into 2 subzones with a variable transition height.

    // REFERENCES:
    // The model is described in the EnergyPlus Engineering Reference in Anna Liu's UCSD PhD thesis.

    // Using/Aliasing
    using Psychrometrics::PsyCpAirFnW;
    using Psychrometrics::PsyRhoAirFnPbTdbW;
    using namespace DataHeatBalFanSys;
    auto &TimeStepSys = state.dataHVACGlobal->TimeStepSys;
    using InternalHeatGains::SumInternalConvectionGainsByTypes;
    using InternalHeatGains::SumReturnAirConvectionGainsByTypes;

    // Locals
    // SUBROUTINE ARGUMENT DEFINITIONS:

    // SUBROUTINE PARAMETER DEFINITIONS:
    // na

    // INTERFACE BLOCK SPECIFICATIONS:
    // na

    // DERIVED TYPE DEFINITIONS:
    // na

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    bool MIXFLAG(false);  // if true treat as a mixed zone
    Real64 CeilingHeight; // zone ceiling height above floor [m]
    int UINum;            // index to underfloor interior zone model data
    Real64 GainsFrac;     // fraction of occupied subzone heat gains that remain in the subzone;
    // that is, don't go into the plumes
    // REAL(r64)   :: NumPLPP            ! number of plumes per person
    Real64 HeightThermostat;    // height of the thermostat above the floor [m]
    Real64 HeightComfort;       // height at which comfort temperature is calculated
    Real64 TempDiffCritRep;     // Minimum temperature difference between upper and occupied subzones for reporting
    Real64 ConvGainsOccSubzone; // convective heat gains into the lower (occupied) subzone [W]
    Real64 ConvGainsUpSubzone;  // convective heat gains into the upper subzone [W]
    Real64 ConvGains;           // total zone convective gains (excluding surfaces) [W]
    int ZoneEquipConfigNum;     // ZoneEquipConfig index for this UFAD zone
    Real64 SumSysMCp;           // Sum of system mass flow rate * specific heat for this zone [W/K]
    Real64 SumSysMCpT;          // Sum of system mass flow rate * specific heat * temperature for this zone [W]
    Real64 SumSysM;             // Sum of systems mass flow rate [kg/s]
    Real64 NodeTemp;            // inlet node temperature [K]
    Real64 MassFlowRate;        // system mass flow rate [kg/s]
    Real64 CpAir;               // specific heat of air [J/kgK]
    int InNodeIndex;            // inlet node index in ZoneEquipConfig
    Real64 SumMCp;              // mass flow rate * specific heat for this zone for infiltration, ventilation, mixing [W/K]
    Real64 SumMCpT;             // mass flow rate * specific heat* temp for this zone for infiltration, ventilation, mixing [W]
    Real64 MCp_Total;           // total mass flow rate * specific heat for this zone [W/K]
    Real64 MCpT_Total;          // total mass flow rate * specific heat* temp for this zone [W]
    Real64 NumberOfPlumes;
    Real64 PowerInPlumes;      // [W]
    Real64 PowerPerPlume(0.0); // power generating each plume [W]
    Real64 HeightFrac;         // Fractional height of transition between occupied and upper subzones
    Real64 TotSysFlow;         // [m3/s]
    Real64 NumDiffusersPerPlume;
    Real64 NumDiffusers;
    Real64 TSupK; // supply yemperature [K]
    Real64 Gamma; // dimensionless height parameter; higher gamma means interface height will be
    // higher, smaller gamma means interface height will be lower.
    Real64 DiffArea;     // diffuser effective area [m2]
    Real64 ThrowAngle;   // diffuser slot angle relative to vertical [radians]
    Real64 SourceHeight; // height of plume sources above the floor [m]
    int Ctd;
    Real64 AirCap;
    Real64 TempHistTerm;
    Real64 ZTAveraged;
    Real64 HeightUpSubzoneAve;       // Height of center of upper air subzone
    Real64 HeightOccupiedSubzoneAve; // Height of center of occupied air subzone
    Real64 ZoneMult;                 // total zone multiplier
    int ZoneNodeNum;                 // node number of the HVAC zone node
    Array1D_int IntGainTypesOccupied(30,
                                     {IntGainTypeOf_People,
                                      IntGainTypeOf_WaterHeaterMixed,
                                      IntGainTypeOf_WaterHeaterStratified,
                                      IntGainTypeOf_ThermalStorageChilledWaterMixed,
                                      IntGainTypeOf_ThermalStorageChilledWaterStratified,
                                      IntGainTypeOf_ElectricEquipment,
                                      IntGainTypeOf_ElectricEquipmentITEAirCooled,
                                      IntGainTypeOf_GasEquipment,
                                      IntGainTypeOf_HotWaterEquipment,
                                      IntGainTypeOf_SteamEquipment,
                                      IntGainTypeOf_OtherEquipment,
                                      IntGainTypeOf_ZoneBaseboardOutdoorTemperatureControlled,
                                      IntGainTypeOf_GeneratorFuelCell,
                                      IntGainTypeOf_WaterUseEquipment,
                                      IntGainTypeOf_GeneratorMicroCHP,
                                      IntGainTypeOf_ElectricLoadCenterTransformer,
                                      IntGainTypeOf_ElectricLoadCenterInverterSimple,
                                      IntGainTypeOf_ElectricLoadCenterInverterFunctionOfPower,
                                      IntGainTypeOf_ElectricLoadCenterInverterLookUpTable,
                                      IntGainTypeOf_ElectricLoadCenterStorageLiIonNmcBattery,
                                      IntGainTypeOf_ElectricLoadCenterStorageBattery,
                                      IntGainTypeOf_ElectricLoadCenterStorageSimple,
                                      IntGainTypeOf_PipeIndoor,
                                      IntGainTypeOf_RefrigerationCase,
                                      IntGainTypeOf_RefrigerationCompressorRack,
                                      IntGainTypeOf_RefrigerationSystemAirCooledCondenser,
                                      IntGainTypeOf_RefrigerationSystemSuctionPipe,
                                      IntGainTypeOf_RefrigerationSecondaryReceiver,
                                      IntGainTypeOf_RefrigerationSecondaryPipe,
                                      IntGainTypeOf_RefrigerationWalkIn});

    Array1D_int IntGainTypesUpSubzone(2, {IntGainTypeOf_DaylightingDeviceTubular, IntGainTypeOf_Lights});
    Real64 RetAirGains;

    // Exact solution or Euler method
    if (state.dataHeatBal->ZoneAirSolutionAlgo != Use3rdOrder) {
        if (state.dataHVACGlobal->ShortenTimeStepSysRoomAir && TimeStepSys < state.dataGlobal->TimeStepZone) {
            if (state.dataHVACGlobal->PreviousTimeStep < state.dataGlobal->TimeStepZone) {
                state.dataRoomAirMod->Zone1OC(ZoneNum) = state.dataRoomAirMod->ZoneM2OC(ZoneNum);
                state.dataRoomAirMod->Zone1MX(ZoneNum) = state.dataRoomAirMod->ZoneM2MX(ZoneNum);
            } else {
                state.dataRoomAirMod->Zone1OC(ZoneNum) = state.dataRoomAirMod->ZoneMXOC(ZoneNum);
                state.dataRoomAirMod->Zone1MX(ZoneNum) = state.dataRoomAirMod->ZoneMXMX(ZoneNum);
            }
        } else {
            state.dataRoomAirMod->Zone1OC(ZoneNum) = state.dataRoomAirMod->ZTOC(ZoneNum);
            state.dataRoomAirMod->Zone1MX(ZoneNum) = state.dataRoomAirMod->ZTMX(ZoneNum);
        }
    }

    MIXFLAG = false;
    state.dataRoomAirMod->UFHcIn = state.dataHeatBal->HConvIn;
    SumSysMCp = 0.0;
    SumSysMCpT = 0.0;
    TotSysFlow = 0.0;
    TSupK = 0.0;
    SumSysM = 0.0;
    ZoneMult = state.dataHeatBal->Zone(ZoneNum).Multiplier * state.dataHeatBal->Zone(ZoneNum).ListMultiplier;
    CeilingHeight = state.dataRoomAirMod->ZoneCeilingHeight((ZoneNum - 1) * 2 + 2) - state.dataRoomAirMod->ZoneCeilingHeight((ZoneNum - 1) * 2 + 1);
    UINum = state.dataRoomAirMod->ZoneUFPtr(ZoneNum);
    HeightThermostat = state.dataRoomAirMod->ZoneUCSDUI(UINum).ThermostatHeight;
    HeightComfort = state.dataRoomAirMod->ZoneUCSDUI(UINum).ComfortHeight;
    TempDiffCritRep = state.dataRoomAirMod->ZoneUCSDUI(UINum).TempTrigger;
    DiffArea = state.dataRoomAirMod->ZoneUCSDUI(UINum).DiffArea;
    ThrowAngle = DataGlobalConstants::DegToRadians * state.dataRoomAirMod->ZoneUCSDUI(UINum).DiffAngle;
    SourceHeight = 0.0;
    NumDiffusers = state.dataRoomAirMod->ZoneUCSDUI(UINum).DiffusersPerZone;
    PowerPerPlume = state.dataRoomAirMod->ZoneUCSDUI(UINum).PowerPerPlume;
    // gains from occupants, task lighting, elec equip, gas equip, other equip, hot water equip, steam equip,
    // baseboards (nonthermostatic), water heater skin loss
    SumInternalConvectionGainsByTypes(state, ZoneNum, IntGainTypesOccupied, ConvGainsOccSubzone);

    // Add heat to return air if zonal system (no return air) or cycling system (return air frequently very
    // low or zero)
    if (state.dataHeatBal->Zone(ZoneNum).NoHeatToReturnAir) {
        SumReturnAirConvectionGainsByTypes(state, ZoneNum, IntGainTypesOccupied, RetAirGains);
        ConvGainsOccSubzone += RetAirGains;
    }

    // Add convection from pool cover to occupied region
    ConvGainsOccSubzone += state.dataHeatBalFanSys->SumConvPool(ZoneNum);

    // gains from lights (ceiling), tubular daylighting devices, high temp radiant heaters

    SumInternalConvectionGainsByTypes(state, ZoneNum, IntGainTypesUpSubzone, ConvGainsUpSubzone);
    ConvGainsUpSubzone += state.dataHeatBalFanSys->SumConvHTRadSys(ZoneNum);
    if (state.dataHeatBal->Zone(ZoneNum).NoHeatToReturnAir) {
        SumReturnAirConvectionGainsByTypes(state, ZoneNum, IntGainTypesUpSubzone, RetAirGains);
        ConvGainsUpSubzone += RetAirGains;
    }
    ConvGains = ConvGainsOccSubzone + ConvGainsUpSubzone + state.dataHeatBalFanSys->SysDepZoneLoadsLagged(ZoneNum);
    ZoneEquipConfigNum = state.dataRoomAirMod->ZoneUCSDUI(UINum).ZoneEquipPtr;
    if (ZoneEquipConfigNum > 0) {
        for (InNodeIndex = 1; InNodeIndex <= state.dataZoneEquip->ZoneEquipConfig(ZoneEquipConfigNum).NumInletNodes; ++InNodeIndex) {
            NodeTemp = state.dataLoopNodes->Node(state.dataZoneEquip->ZoneEquipConfig(ZoneEquipConfigNum).InletNode(InNodeIndex)).Temp;
            MassFlowRate = state.dataLoopNodes->Node(state.dataZoneEquip->ZoneEquipConfig(ZoneEquipConfigNum).InletNode(InNodeIndex)).MassFlowRate;
            CpAir = PsyCpAirFnW(state.dataHeatBalFanSys->ZoneAirHumRat(ZoneNum));
            SumSysMCp += MassFlowRate * CpAir;
            SumSysMCpT += MassFlowRate * CpAir * NodeTemp;
            TotSysFlow +=
                MassFlowRate / PsyRhoAirFnPbTdbW(state, state.dataEnvrn->OutBaroPress, NodeTemp, state.dataHeatBalFanSys->ZoneAirHumRat(ZoneNum));
            TSupK += MassFlowRate * NodeTemp;
            SumSysM += MassFlowRate;
        }
        if (TotSysFlow > 0.0) {
            TSupK = TSupK / SumSysM + DataGlobalConstants::KelvinConv;
        } else {
            TSupK = 0.0;
        }
    }
    // mass flow times specific heat for infiltration, ventilation, mixing, earth tube
    SumMCp = state.dataHeatBalFanSys->MCPI(ZoneNum) + state.dataHeatBalFanSys->MCPV(ZoneNum) + state.dataHeatBalFanSys->MCPM(ZoneNum) +
             state.dataHeatBalFanSys->MCPE(ZoneNum) + state.dataHeatBalFanSys->MCPC(ZoneNum) + state.dataHeatBalFanSys->MDotCPOA(ZoneNum);
    // mass flow times specific heat times temperature for infiltration, ventilation, mixing, earth tube
    SumMCpT = state.dataHeatBalFanSys->MCPTI(ZoneNum) + state.dataHeatBalFanSys->MCPTV(ZoneNum) + state.dataHeatBalFanSys->MCPTM(ZoneNum) +
              state.dataHeatBalFanSys->MCPTE(ZoneNum) + state.dataHeatBalFanSys->MCPTC(ZoneNum) +
              state.dataHeatBalFanSys->MDotCPOA(ZoneNum) * state.dataHeatBal->Zone(ZoneNum).OutDryBulbTemp;
    MCp_Total = SumMCp + SumSysMCp;
    MCpT_Total = SumMCpT + SumSysMCpT;
    // For the York MIT diffusers (variable area) the area varies with the flow rate. Assume 400 ft/min velocity
    // at the diffuser, and a design flow rate of 150 cfm (.0708 m3/s). Then the design area for each diffuser is
    // 150 ft3/min / 400 ft/min = .375 ft2 = .035 m2. This is adjusted each time step by
    //               (TotSysFlow/(NumDiffusers*.0708))*.035
    if (state.dataRoomAirMod->ZoneUCSDUI(UINum).DiffuserType == Diffuser::VarArea) {
        DiffArea = 0.035 * TotSysFlow / (0.0708 * NumDiffusers);
    }
    // initial estimate of convective transfer from surfaces; assume HeightFrac is 0.5.
    HcUCSDUF(state, ZoneNum, 0.5);
    PowerInPlumes = ConvGains + state.dataUFADManager->HAT_OC - state.dataUFADManager->HA_OC * state.dataRoomAirMod->ZTOC(ZoneNum) +
                    state.dataUFADManager->HAT_MX - state.dataUFADManager->HA_MX * state.dataRoomAirMod->ZTMX(ZoneNum);
    if (PowerPerPlume > 0.0 && PowerInPlumes > 0.0) {
        NumberOfPlumes = PowerInPlumes / PowerPerPlume;
        NumDiffusersPerPlume = NumDiffusers / NumberOfPlumes;
    } else {
        NumberOfPlumes = 1.0;
        NumDiffusersPerPlume = 1.0;
    }
    if ((PowerInPlumes <= 0.0) || (TotSysFlow == 0.0) || (TSupK - DataGlobalConstants::KelvinConv) > state.dataHeatBalFanSys->MAT(ZoneNum)) {
        // The system will mix
        HeightFrac = 0.0;
    } else {
        Gamma = std::pow(TotSysFlow * std::cos(ThrowAngle), 1.5) /
                (NumberOfPlumes * std::pow(NumDiffusersPerPlume * DiffArea, 1.25) * std::sqrt(0.0281 * 0.001 * PowerInPlumes));
        if (state.dataRoomAirMod->ZoneUCSDUI(UINum).CalcTransHeight) {
            HeightFrac = (std::sqrt(NumDiffusersPerPlume * DiffArea) * (7.43 * std::log(Gamma) - 1.35) + 0.5 * SourceHeight) / CeilingHeight;
        } else {
            HeightFrac = state.dataRoomAirMod->ZoneUCSDUI(UINum).TransHeight / CeilingHeight;
        }
        HeightFrac = max(0.0, min(1.0, HeightFrac));
        for (Ctd = 1; Ctd <= 4; ++Ctd) {
            HcUCSDUF(state, ZoneNum, HeightFrac);
            PowerInPlumes = ConvGains + state.dataUFADManager->HAT_OC - state.dataUFADManager->HA_OC * state.dataRoomAirMod->ZTOC(ZoneNum) +
                            state.dataUFADManager->HAT_MX - state.dataUFADManager->HA_MX * state.dataRoomAirMod->ZTMX(ZoneNum);
            if (PowerPerPlume > 0.0 && PowerInPlumes > 0.0) {
                NumberOfPlumes = PowerInPlumes / PowerPerPlume;
                NumDiffusersPerPlume = NumDiffusers / NumberOfPlumes;
            } else {
                NumberOfPlumes = 1.0;
                NumDiffusersPerPlume = 1.0;
            }
            if (PowerInPlumes <= 0.0) break;
            Gamma = std::pow(TotSysFlow * std::cos(ThrowAngle), 1.5) /
                    (NumberOfPlumes * std::pow(NumDiffusersPerPlume * DiffArea, 1.25) * std::sqrt(0.0281 * 0.001 * PowerInPlumes));
            if (state.dataRoomAirMod->ZoneUCSDUI(UINum).CalcTransHeight) {
                HeightFrac = (std::sqrt(NumDiffusersPerPlume * DiffArea) * (7.43 * std::log(Gamma) - 1.35) + 0.5 * SourceHeight) / CeilingHeight;
            } else {
                HeightFrac = state.dataRoomAirMod->ZoneUCSDUI(UINum).TransHeight / CeilingHeight;
            }
            HeightFrac = max(0.0, min(1.0, HeightFrac));
            state.dataRoomAirMod->HeightTransition(ZoneNum) = HeightFrac * CeilingHeight;
            GainsFrac = state.dataRoomAirMod->ZoneUCSDUI(UINum).A_Kc * std::pow(Gamma, state.dataRoomAirMod->ZoneUCSDUI(UINum).B_Kc) +
                        state.dataRoomAirMod->ZoneUCSDUI(UINum).C_Kc + state.dataRoomAirMod->ZoneUCSDUI(UINum).D_Kc * Gamma +
                        state.dataRoomAirMod->ZoneUCSDUI(UINum).E_Kc * pow_2(Gamma);
            GainsFrac = max(0.6, min(GainsFrac, 1.0));
            state.dataRoomAirMod->AIRRATOC(ZoneNum) =
                state.dataHeatBal->Zone(ZoneNum).Volume *
                (state.dataRoomAirMod->HeightTransition(ZoneNum) - min(state.dataRoomAirMod->HeightTransition(ZoneNum), 0.2)) / CeilingHeight *
                state.dataHeatBal->Zone(ZoneNum).ZoneVolCapMultpSens *
                PsyRhoAirFnPbTdbW(
                    state, state.dataEnvrn->OutBaroPress, state.dataRoomAirMod->MATOC(ZoneNum), state.dataHeatBalFanSys->ZoneAirHumRat(ZoneNum)) *
                PsyCpAirFnW(state.dataHeatBalFanSys->ZoneAirHumRat(ZoneNum)) / (TimeStepSys * DataGlobalConstants::SecInHour);
            state.dataRoomAirMod->AIRRATMX(ZoneNum) =
                state.dataHeatBal->Zone(ZoneNum).Volume * (CeilingHeight - state.dataRoomAirMod->HeightTransition(ZoneNum)) / CeilingHeight *
                state.dataHeatBal->Zone(ZoneNum).ZoneVolCapMultpSens *
                PsyRhoAirFnPbTdbW(
                    state, state.dataEnvrn->OutBaroPress, state.dataRoomAirMod->MATMX(ZoneNum), state.dataHeatBalFanSys->ZoneAirHumRat(ZoneNum)) *
                PsyCpAirFnW(state.dataHeatBalFanSys->ZoneAirHumRat(ZoneNum)) / (TimeStepSys * DataGlobalConstants::SecInHour);

            if (state.dataHVACGlobal->UseZoneTimeStepHistory) {
                state.dataRoomAirMod->ZTM3OC(ZoneNum) = state.dataRoomAirMod->XM3TOC(ZoneNum);
                state.dataRoomAirMod->ZTM2OC(ZoneNum) = state.dataRoomAirMod->XM2TOC(ZoneNum);
                state.dataRoomAirMod->ZTM1OC(ZoneNum) = state.dataRoomAirMod->XMATOC(ZoneNum);

                state.dataRoomAirMod->ZTM3MX(ZoneNum) = state.dataRoomAirMod->XM3TMX(ZoneNum);
                state.dataRoomAirMod->ZTM2MX(ZoneNum) = state.dataRoomAirMod->XM2TMX(ZoneNum);
                state.dataRoomAirMod->ZTM1MX(ZoneNum) = state.dataRoomAirMod->XMATMX(ZoneNum);

            } else {
                state.dataRoomAirMod->ZTM3OC(ZoneNum) = state.dataRoomAirMod->DSXM3TOC(ZoneNum);
                state.dataRoomAirMod->ZTM2OC(ZoneNum) = state.dataRoomAirMod->DSXM2TOC(ZoneNum);
                state.dataRoomAirMod->ZTM1OC(ZoneNum) = state.dataRoomAirMod->DSXMATOC(ZoneNum);

                state.dataRoomAirMod->ZTM3MX(ZoneNum) = state.dataRoomAirMod->DSXM3TMX(ZoneNum);
                state.dataRoomAirMod->ZTM2MX(ZoneNum) = state.dataRoomAirMod->DSXM2TMX(ZoneNum);
                state.dataRoomAirMod->ZTM1MX(ZoneNum) = state.dataRoomAirMod->DSXMATMX(ZoneNum);
            }

            AirCap = state.dataRoomAirMod->AIRRATOC(ZoneNum);
            TempHistTerm = AirCap * (3.0 * state.dataRoomAirMod->ZTM1OC(ZoneNum) - (3.0 / 2.0) * state.dataRoomAirMod->ZTM2OC(ZoneNum) +
                                     (1.0 / 3.0) * state.dataRoomAirMod->ZTM3OC(ZoneNum));
            // Formerly CoefSumha, coef in zone temp equation with dimensions of h*A
            Real64 TempDepCoef = GainsFrac * state.dataUFADManager->HA_OC + MCp_Total;
            Real64 TempIndCoef = GainsFrac * (ConvGains + state.dataUFADManager->HAT_OC + state.dataUFADManager->HAT_MX -
                                              state.dataUFADManager->HA_MX * state.dataRoomAirMod->ZTMX(ZoneNum)) +
                                 MCpT_Total + state.dataHeatBalFanSys->NonAirSystemResponse(ZoneNum) / ZoneMult;
            {
                auto const SELECT_CASE_var(state.dataHeatBal->ZoneAirSolutionAlgo);
                if (SELECT_CASE_var == Use3rdOrder) {
                    state.dataRoomAirMod->ZTOC(ZoneNum) = (TempHistTerm +
                                                           GainsFrac * (ConvGains + state.dataUFADManager->HAT_OC + state.dataUFADManager->HAT_MX -
                                                                        state.dataUFADManager->HA_MX * state.dataRoomAirMod->ZTMX(ZoneNum)) +
                                                           MCpT_Total + state.dataHeatBalFanSys->NonAirSystemResponse(ZoneNum) / ZoneMult) /
                                                          ((11.0 / 6.0) * AirCap + GainsFrac * state.dataUFADManager->HA_OC + MCp_Total);
                } else if (SELECT_CASE_var == UseAnalyticalSolution) {
                    if (TempDepCoef == 0.0) { // B=0
                        state.dataRoomAirMod->ZTOC(ZoneNum) = state.dataRoomAirMod->Zone1OC(ZoneNum) + TempIndCoef / AirCap;
                    } else {
                        state.dataRoomAirMod->ZTOC(ZoneNum) =
                            (state.dataRoomAirMod->Zone1OC(ZoneNum) - TempIndCoef / TempDepCoef) * std::exp(min(700.0, -TempDepCoef / AirCap)) +
                            TempIndCoef / TempDepCoef;
                    }
                } else if (SELECT_CASE_var == UseEulerMethod) {
                    state.dataRoomAirMod->ZTOC(ZoneNum) = (AirCap * state.dataRoomAirMod->Zone1OC(ZoneNum) + TempIndCoef) / (AirCap + TempDepCoef);
                }
            }
            AirCap = state.dataRoomAirMod->AIRRATMX(ZoneNum);
            TempHistTerm = AirCap * (3.0 * state.dataRoomAirMod->ZTM1MX(ZoneNum) - (3.0 / 2.0) * state.dataRoomAirMod->ZTM2MX(ZoneNum) +
                                     (1.0 / 3.0) * state.dataRoomAirMod->ZTM3MX(ZoneNum));
            TempDepCoef = (1.0 - GainsFrac) * state.dataUFADManager->HA_MX + MCp_Total;
            TempIndCoef = (1.0 - GainsFrac) * (ConvGains + state.dataUFADManager->HAT_OC + state.dataUFADManager->HAT_MX -
                                               state.dataUFADManager->HA_OC * state.dataRoomAirMod->ZTOC(ZoneNum)) +
                          state.dataRoomAirMod->ZTOC(ZoneNum) * MCp_Total;
            {
                auto const SELECT_CASE_var(state.dataHeatBal->ZoneAirSolutionAlgo);
                if (SELECT_CASE_var == Use3rdOrder) {
                    state.dataRoomAirMod->ZTMX(ZoneNum) =
                        (TempHistTerm +
                         (1.0 - GainsFrac) * (ConvGains + state.dataUFADManager->HAT_OC + state.dataUFADManager->HAT_MX -
                                              state.dataUFADManager->HA_OC * state.dataRoomAirMod->ZTOC(ZoneNum)) +
                         state.dataRoomAirMod->ZTOC(ZoneNum) * MCp_Total) /
                        ((11.0 / 6.0) * AirCap + (1.0 - GainsFrac) * state.dataUFADManager->HA_MX + MCp_Total);
                } else if (SELECT_CASE_var == UseAnalyticalSolution) {
                    if (TempDepCoef == 0.0) { // B=0
                        state.dataRoomAirMod->ZTMX(ZoneNum) = state.dataRoomAirMod->Zone1MX(ZoneNum) + TempIndCoef / AirCap;
                    } else {
                        state.dataRoomAirMod->ZTMX(ZoneNum) =
                            (state.dataRoomAirMod->Zone1MX(ZoneNum) - TempIndCoef / TempDepCoef) * std::exp(min(700.0, -TempDepCoef / AirCap)) +
                            TempIndCoef / TempDepCoef;
                    }
                } else if (SELECT_CASE_var == UseEulerMethod) {
                    state.dataRoomAirMod->ZTMX(ZoneNum) = (AirCap * state.dataRoomAirMod->Zone1MX(ZoneNum) + TempIndCoef) / (AirCap + TempDepCoef);
                }
            }
            state.dataRoomAirMod->ZTFloor(ZoneNum) = state.dataRoomAirMod->ZTOC(ZoneNum);
        }
        if (PowerInPlumes <= 0.0) {
            HeightFrac = 0.0;
            state.dataRoomAirMod->AirModel(ZoneNum).SimAirModel = false;
            state.dataRoomAirMod->ZoneUFGamma(ZoneNum) = 0.0;
            state.dataRoomAirMod->ZoneUFPowInPlumes(ZoneNum) = 0.0;
        } else {
            state.dataRoomAirMod->AirModel(ZoneNum).SimAirModel = true;
            state.dataRoomAirMod->ZoneUFGamma(ZoneNum) = Gamma;
            state.dataRoomAirMod->ZoneUFPowInPlumes(ZoneNum) = PowerInPlumes;
        }
    }

    //=============================== M I X E D  Calculation ==============================================
    if (state.dataRoomAirMod->ZTMX(ZoneNum) < state.dataRoomAirMod->ZTOC(ZoneNum) || MCp_Total <= 0.0 ||
        HeightFrac * CeilingHeight < state.dataUFADManager->ThickOccupiedSubzoneMin) {
        MIXFLAG = true;
        HeightFrac = 0.0;
        state.dataRoomAirMod->AvgTempGrad(ZoneNum) = 0.0;
        state.dataRoomAirMod->MaxTempGrad(ZoneNum) = 0.0;
        state.dataRoomAirMod->AirModel(ZoneNum).SimAirModel = false;
        AirCap = state.dataHeatBalFanSys->AIRRAT(ZoneNum);
        TempHistTerm = AirCap * (3.0 * state.dataHeatBalFanSys->ZTM1(ZoneNum) - (3.0 / 2.0) * state.dataHeatBalFanSys->ZTM2(ZoneNum) +
                                 (1.0 / 3.0) * state.dataHeatBalFanSys->ZTM3(ZoneNum));

        for (Ctd = 1; Ctd <= 3; ++Ctd) {
            Real64 TempDepCoef = state.dataUFADManager->HA_MX + state.dataUFADManager->HA_OC + MCp_Total;
            // Formerly CoefSumhat, coef in zone temp equation with dimensions of h*A(T1
            Real64 TempIndCoef = ConvGains + state.dataUFADManager->HAT_MX + state.dataUFADManager->HAT_OC + MCpT_Total;
            {
                auto const SELECT_CASE_var(state.dataHeatBal->ZoneAirSolutionAlgo);
                if (SELECT_CASE_var == Use3rdOrder) {
                    ZTAveraged = (TempHistTerm + ConvGains + state.dataUFADManager->HAT_MX + state.dataUFADManager->HAT_OC + MCpT_Total) /
                                 ((11.0 / 6.0) * AirCap + state.dataUFADManager->HA_MX + state.dataUFADManager->HA_OC + MCp_Total);
                } else if (SELECT_CASE_var == UseAnalyticalSolution) {
                    if (TempDepCoef == 0.0) { // B=0
                        ZTAveraged = state.dataHeatBalFanSys->ZoneT1(ZoneNum) + TempIndCoef / AirCap;
                    } else {
                        ZTAveraged =
                            (state.dataHeatBalFanSys->ZoneT1(ZoneNum) - TempIndCoef / TempDepCoef) * std::exp(min(700.0, -TempDepCoef / AirCap)) +
                            TempIndCoef / TempDepCoef;
                    }
                } else if (SELECT_CASE_var == UseEulerMethod) {
                    ZTAveraged = (AirCap * state.dataHeatBalFanSys->ZoneT1(ZoneNum) + TempIndCoef) / (AirCap + TempDepCoef);
                }
            }
            state.dataRoomAirMod->ZTOC(ZoneNum) = ZTAveraged;
            state.dataRoomAirMod->ZTMX(ZoneNum) = ZTAveraged;
            state.dataRoomAirMod->ZTFloor(ZoneNum) = ZTAveraged;
            HcUCSDUF(state, ZoneNum, HeightFrac);
            TempDepCoef = state.dataUFADManager->HA_MX + state.dataUFADManager->HA_OC + MCp_Total;
            TempIndCoef = ConvGains + state.dataUFADManager->HAT_MX + state.dataUFADManager->HAT_OC + MCpT_Total;
            {
                auto const SELECT_CASE_var(state.dataHeatBal->ZoneAirSolutionAlgo);
                if (SELECT_CASE_var == Use3rdOrder) {
                    ZTAveraged = (TempHistTerm + ConvGains + state.dataUFADManager->HAT_MX + state.dataUFADManager->HAT_OC + MCpT_Total) /
                                 ((11.0 / 6.0) * AirCap + state.dataUFADManager->HA_MX + state.dataUFADManager->HA_OC + MCp_Total);
                } else if (SELECT_CASE_var == UseAnalyticalSolution) {
                    if (TempDepCoef == 0.0) { // B=0
                        ZTAveraged = state.dataHeatBalFanSys->ZoneT1(ZoneNum) + TempIndCoef / AirCap;
                    } else {
                        ZTAveraged =
                            (state.dataHeatBalFanSys->ZoneT1(ZoneNum) - TempIndCoef / TempDepCoef) * std::exp(min(700.0, -TempDepCoef / AirCap)) +
                            TempIndCoef / TempDepCoef;
                    }
                } else if (SELECT_CASE_var == UseEulerMethod) {
                    ZTAveraged = (AirCap * state.dataHeatBalFanSys->ZoneT1(ZoneNum) + TempIndCoef) / (AirCap + TempDepCoef);
                }
            }
            state.dataRoomAirMod->ZTOC(ZoneNum) = ZTAveraged;
            state.dataRoomAirMod->ZTMX(ZoneNum) = ZTAveraged;
            state.dataRoomAirMod->ZTFloor(ZoneNum) = ZTAveraged;
        }
    }
    //=========================================================================================

    // Comfort temperature and temperature at the thermostat/temperature control sensor

    state.dataRoomAirMod->HeightTransition(ZoneNum) = HeightFrac * CeilingHeight;
    HeightUpSubzoneAve = (CeilingHeight + state.dataRoomAirMod->HeightTransition(ZoneNum)) / 2.0;
    HeightOccupiedSubzoneAve = state.dataRoomAirMod->HeightTransition(ZoneNum) / 2.0;
    // Comfort temperature

    if (MIXFLAG) {
        state.dataRoomAirMod->TCMF(ZoneNum) = ZTAveraged;
    } else {
        if (HeightComfort < HeightOccupiedSubzoneAve) {
            state.dataRoomAirMod->TCMF(ZoneNum) = state.dataRoomAirMod->ZTOC(ZoneNum);
        } else if (HeightComfort >= HeightOccupiedSubzoneAve && HeightComfort < HeightUpSubzoneAve) {
            state.dataRoomAirMod->TCMF(ZoneNum) = (state.dataRoomAirMod->ZTOC(ZoneNum) * (HeightUpSubzoneAve - HeightComfort) +
                                                   state.dataRoomAirMod->ZTMX(ZoneNum) * (HeightComfort - HeightOccupiedSubzoneAve)) /
                                                  (HeightUpSubzoneAve - HeightOccupiedSubzoneAve);
        } else if (HeightComfort >= HeightUpSubzoneAve && HeightComfort <= CeilingHeight) {
            state.dataRoomAirMod->TCMF(ZoneNum) = state.dataRoomAirMod->ZTMX(ZoneNum);
        } else {
            ShowFatalError(state, "UFAD comfort height is above ceiling or below floor in Zone: " + state.dataHeatBal->Zone(ZoneNum).Name);
        }
    }

    // Temperature at the thermostat/temperature control sensor

    if (MIXFLAG) {
        state.dataHeatBalFanSys->TempTstatAir(ZoneNum) = ZTAveraged;
    } else {
        if (HeightThermostat < HeightOccupiedSubzoneAve) {
            state.dataHeatBalFanSys->TempTstatAir(ZoneNum) = state.dataRoomAirMod->ZTOC(ZoneNum);
        } else if (HeightThermostat >= HeightOccupiedSubzoneAve && HeightThermostat < HeightUpSubzoneAve) {
            state.dataHeatBalFanSys->TempTstatAir(ZoneNum) = (state.dataRoomAirMod->ZTOC(ZoneNum) * (HeightUpSubzoneAve - HeightThermostat) +
                                                              state.dataRoomAirMod->ZTMX(ZoneNum) * (HeightThermostat - HeightOccupiedSubzoneAve)) /
                                                             (HeightUpSubzoneAve - HeightOccupiedSubzoneAve);
        } else if (HeightThermostat >= HeightUpSubzoneAve && HeightThermostat <= CeilingHeight) {
            state.dataHeatBalFanSys->TempTstatAir(ZoneNum) = state.dataRoomAirMod->ZTMX(ZoneNum);
        } else {
            ShowFatalError(state,
                           "Underfloor air distribution thermostat height is above ceiling or below floor in Zone: " +
                               state.dataHeatBal->Zone(ZoneNum).Name);
        }
    }

    // Temperature gradients
    if ((HeightUpSubzoneAve - HeightOccupiedSubzoneAve) > 0.1) {
        state.dataRoomAirMod->AvgTempGrad(ZoneNum) =
            (state.dataRoomAirMod->ZTMX(ZoneNum) - state.dataRoomAirMod->ZTOC(ZoneNum)) / (HeightUpSubzoneAve - HeightOccupiedSubzoneAve);
    } else {
        state.dataRoomAirMod->AvgTempGrad(ZoneNum) = 0.0;
    }

    if (MIXFLAG) {
        state.dataRoomAirMod->ZoneUFMixedFlag(ZoneNum) = 1;
        state.dataRoomAirMod->AirModel(ZoneNum).SimAirModel = false;
    } else {
        state.dataRoomAirMod->ZoneUFMixedFlag(ZoneNum) = 0;
        state.dataRoomAirMod->AirModel(ZoneNum).SimAirModel = true;
    }

    if (ZoneEquipConfigNum > 0) {
        ZoneNodeNum = state.dataHeatBal->Zone(ZoneNum).SystemZoneNodeNumber;
        state.dataLoopNodes->Node(ZoneNodeNum).Temp = state.dataRoomAirMod->ZTMX(ZoneNum);
    }

    if (MIXFLAG) {
        state.dataRoomAirMod->Phi(ZoneNum) = 1.0;
    } else {
        state.dataRoomAirMod->Phi(ZoneNum) = (state.dataRoomAirMod->ZTOC(ZoneNum) - (TSupK - DataGlobalConstants::KelvinConv)) /
                                             (state.dataRoomAirMod->ZTMX(ZoneNum) - (TSupK - DataGlobalConstants::KelvinConv));
    }

    // Mixed for reporting purposes
    if ((MIXFLAG) || ((state.dataRoomAirMod->ZTMX(ZoneNum) - state.dataRoomAirMod->ZTOC(ZoneNum)) < TempDiffCritRep)) {
        state.dataRoomAirMod->ZoneUFMixedFlagRep(ZoneNum) = 1.0;
        state.dataRoomAirMod->HeightTransition(ZoneNum) = 0.0;
        state.dataRoomAirMod->AvgTempGrad(ZoneNum) = 0.0;
    } else {
        state.dataRoomAirMod->ZoneUFMixedFlagRep(ZoneNum) = 0.0;
    }
}

void CalcUCSDUE(EnergyPlusData &state, int const ZoneNum) // index number for the specified zone
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Fred Buhl
    //       DATE WRITTEN   January 2006
    //       MODIFIED       Brent Griffith June 2008 for new interpolation and time history
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // Using the UCSD UFAD exterior zone model, this subroutine calculates the  occupied subzone height,
    // surface heat transfer coefficients, the occupied subzone temperature, and the upper subzone temperature.

    // METHODOLOGY EMPLOYED:
    // The zone is divided into 2 subzones with a variable transition height.

    // REFERENCES:
    // The model is described in the EnergyPlus Engineering Reference in Anna Liu's UCSD PhD thesis.

    // Using/Aliasing
    using Psychrometrics::PsyCpAirFnW;
    using Psychrometrics::PsyRhoAirFnPbTdbW;
    using namespace DataHeatBalFanSys;
    auto &TimeStepSys = state.dataHVACGlobal->TimeStepSys;
    using InternalHeatGains::SumInternalConvectionGainsByTypes;
    using InternalHeatGains::SumReturnAirConvectionGainsByTypes;

    // Locals
    // SUBROUTINE ARGUMENT DEFINITIONS:

    // SUBROUTINE PARAMETER DEFINITIONS:
    // na

    // INTERFACE BLOCK SPECIFICATIONS:
    // na

    // DERIVED TYPE DEFINITIONS:
    // na

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    bool MIXFLAG(false);  // if true treat as a mixed zone
    Real64 CeilingHeight; // zone ceiling height above floor [m]
    int UINum;            // index to underfloor interior zone model data
    Real64 GainsFrac;     // fraction of occupied subzone heat gains that remain in the subzone;
    // that is, don't go into the plumes
    Real64 HeightThermostat;    // height of the thermostat above the floor [m]
    Real64 HeightComfort;       // height at which comfort temperature is calculated
    Real64 TempDiffCritRep;     // Minimum temperature difference between upper and occupied subzones for reporting
    Real64 ConvGainsOccSubzone; // convective heat gains into the lower (occupied) subzone [W]
    Real64 ConvGainsUpSubzone;  // convective heat gains into the upper subzone [W]
    Real64 ConvGains;           // total zone convective gains (excluding surfaces) [W]
    Real64 ConvGainsWindows;    // convective gain from windows [W]
    int ZoneEquipConfigNum;     // ZoneEquipConfig index for this UFAD zone
    Real64 SumSysMCp;           // Sum of system mass flow rate * specific heat for this zone [W/K]
    Real64 SumSysMCpT;          // Sum of system mass flow rate * specific heat * temperature for this zone [W]
    Real64 SumSysM;             // Sum of systems mass flow rate [kg/s]
    Real64 NodeTemp;            // inlet node temperature [K]
    Real64 MassFlowRate;        // system mass flow rate [kg/s]
    Real64 CpAir;               // specific heat of air [J/kgK]
    int InNodeIndex;            // inlet node index in ZoneEquipConfig
    Real64 SumMCp;              // mass flow rate * specific heat for this zone for infiltration, ventilation, mixing [W/K]
    Real64 SumMCpT;             // mass flow rate * specific heat* temp for this zone for infiltration, ventilation, mixing [W]
    Real64 MCp_Total;           // total mass flow rate * specific heat for this zone [W/K]
    Real64 MCpT_Total;          // total mass flow rate * specific heat* temp for this zone [W]
    Real64 NumberOfPlumes;
    Real64 PowerInPlumes;         // [W]
    Real64 PowerPerPlume(0.0);    // power carried by each plume [W]
    Real64 PowerInPlumesPerMeter; // Power in Plumes per meter of window length [W/m]
    Real64 NumDiffusersPerPlume(0.0);
    Real64 HeightFrac; // Fractional height of transition between occupied and upper subzones
    Real64 TotSysFlow; // [m3/s]
    Real64 NumDiffusers;
    Real64 TSupK; // supply yemperature [K]
    Real64 Gamma; // dimensionless height parameter; higher gamma means interface height will be
    // higher, smaller gamma means interface height will be lower.
    Real64 DiffArea;     // diffuser effective area [m2]
    Real64 ThrowAngle;   // diffuser slot angle relative to vertical [radians]
    Real64 SourceHeight; // height of plume sources above the floor [m]
    int Ctd;
    Real64 AirCap;
    Real64 TempHistTerm;
    Real64 ZTAveraged;
    Real64 HeightUpSubzoneAve;       // Height of center of upper air subzone
    Real64 HeightOccupiedSubzoneAve; // Height of center of occupied air subzone
    Real64 ZoneMult;                 // total zone multiplier
    int ZoneNodeNum;                 // node number of the HVAC zone node
    Real64 TempDepCoef(0.0);         // Formerly CoefSumha, coef in zone temp equation with dimensions of h*A
    Real64 TempIndCoef(0.0);         // Formerly CoefSumhat, coef in zone temp equation with dimensions of h*A(T1
    static const Array1D_int IntGainTypesOccupied(30,
                                                  {IntGainTypeOf_People,
                                                   IntGainTypeOf_WaterHeaterMixed,
                                                   IntGainTypeOf_WaterHeaterStratified,
                                                   IntGainTypeOf_ThermalStorageChilledWaterMixed,
                                                   IntGainTypeOf_ThermalStorageChilledWaterStratified,
                                                   IntGainTypeOf_ElectricEquipment,
                                                   IntGainTypeOf_ElectricEquipmentITEAirCooled,
                                                   IntGainTypeOf_GasEquipment,
                                                   IntGainTypeOf_HotWaterEquipment,
                                                   IntGainTypeOf_SteamEquipment,
                                                   IntGainTypeOf_OtherEquipment,
                                                   IntGainTypeOf_ZoneBaseboardOutdoorTemperatureControlled,
                                                   IntGainTypeOf_GeneratorFuelCell,
                                                   IntGainTypeOf_WaterUseEquipment,
                                                   IntGainTypeOf_GeneratorMicroCHP,
                                                   IntGainTypeOf_ElectricLoadCenterTransformer,
                                                   IntGainTypeOf_ElectricLoadCenterInverterSimple,
                                                   IntGainTypeOf_ElectricLoadCenterInverterFunctionOfPower,
                                                   IntGainTypeOf_ElectricLoadCenterInverterLookUpTable,
                                                   IntGainTypeOf_ElectricLoadCenterStorageBattery,
                                                   IntGainTypeOf_ElectricLoadCenterStorageLiIonNmcBattery,
                                                   IntGainTypeOf_ElectricLoadCenterStorageSimple,
                                                   IntGainTypeOf_PipeIndoor,
                                                   IntGainTypeOf_RefrigerationCase,
                                                   IntGainTypeOf_RefrigerationCompressorRack,
                                                   IntGainTypeOf_RefrigerationSystemAirCooledCondenser,
                                                   IntGainTypeOf_RefrigerationSystemSuctionPipe,
                                                   IntGainTypeOf_RefrigerationSecondaryReceiver,
                                                   IntGainTypeOf_RefrigerationSecondaryPipe,
                                                   IntGainTypeOf_RefrigerationWalkIn});

    static const Array1D_int IntGainTypesUpSubzone(2, {IntGainTypeOf_DaylightingDeviceTubular, IntGainTypeOf_Lights});
    Real64 RetAirGains;

    // Exact solution or Euler method
    if (state.dataHeatBal->ZoneAirSolutionAlgo != Use3rdOrder) {
        if (state.dataHVACGlobal->ShortenTimeStepSysRoomAir && TimeStepSys < state.dataGlobal->TimeStepZone) {
            if (state.dataHVACGlobal->PreviousTimeStep < state.dataGlobal->TimeStepZone) {
                state.dataRoomAirMod->Zone1OC(ZoneNum) = state.dataRoomAirMod->ZoneM2OC(ZoneNum);
                state.dataRoomAirMod->Zone1MX(ZoneNum) = state.dataRoomAirMod->ZoneM2MX(ZoneNum);
            } else {
                state.dataRoomAirMod->Zone1OC(ZoneNum) = state.dataRoomAirMod->ZoneMXOC(ZoneNum);
                state.dataRoomAirMod->Zone1MX(ZoneNum) = state.dataRoomAirMod->ZoneMXMX(ZoneNum);
            }
        } else {
            state.dataRoomAirMod->Zone1OC(ZoneNum) = state.dataRoomAirMod->ZTOC(ZoneNum);
            state.dataRoomAirMod->Zone1MX(ZoneNum) = state.dataRoomAirMod->ZTMX(ZoneNum);
        }
    }

    HeightFrac = 0.0;
    MIXFLAG = false;
    state.dataRoomAirMod->UFHcIn = state.dataHeatBal->HConvIn;
    SumSysMCp = 0.0;
    SumSysMCpT = 0.0;
    TotSysFlow = 0.0;
    TSupK = 0.0;
    SumSysM = 0.0;
    PowerInPlumes = 0.0;
    ConvGainsWindows = 0.0;
    Gamma = 0.0;
    ZoneMult = state.dataHeatBal->Zone(ZoneNum).Multiplier * state.dataHeatBal->Zone(ZoneNum).ListMultiplier;
    CeilingHeight = state.dataRoomAirMod->ZoneCeilingHeight((ZoneNum - 1) * 2 + 2) - state.dataRoomAirMod->ZoneCeilingHeight((ZoneNum - 1) * 2 + 1);
    UINum = state.dataRoomAirMod->ZoneUFPtr(ZoneNum);
    HeightThermostat = state.dataRoomAirMod->ZoneUCSDUE(UINum).ThermostatHeight;
    HeightComfort = state.dataRoomAirMod->ZoneUCSDUE(UINum).ComfortHeight;
    TempDiffCritRep = state.dataRoomAirMod->ZoneUCSDUE(UINum).TempTrigger;
    DiffArea = state.dataRoomAirMod->ZoneUCSDUE(UINum).DiffArea;
    ThrowAngle = DataGlobalConstants::DegToRadians * state.dataRoomAirMod->ZoneUCSDUE(UINum).DiffAngle;
    SourceHeight = state.dataRoomAirMod->ZoneUCSDUE(UINum).HeatSrcHeight;
    NumDiffusers = state.dataRoomAirMod->ZoneUCSDUE(UINum).DiffusersPerZone;
    PowerPerPlume = state.dataRoomAirMod->ZoneUCSDUE(UINum).PowerPerPlume;
    // gains from occupants, task lighting, elec equip, gas equip, other equip, hot water equip, steam equip,
    // baseboards (nonthermostatic), water heater skin loss
    SumInternalConvectionGainsByTypes(state, ZoneNum, IntGainTypesOccupied, ConvGainsOccSubzone);

    // Add heat to return air if zonal system (no return air) or cycling system (return air frequently very
    // low or zero)
    if (state.dataHeatBal->Zone(ZoneNum).NoHeatToReturnAir) {
        SumReturnAirConvectionGainsByTypes(state, ZoneNum, IntGainTypesOccupied, RetAirGains);
        ConvGainsOccSubzone += RetAirGains;
    }

    // Add convection from pool cover to occupied region
    ConvGainsOccSubzone += state.dataHeatBalFanSys->SumConvPool(ZoneNum);

    // gains from lights (ceiling), tubular daylighting devices, high temp radiant heaters
    SumInternalConvectionGainsByTypes(state, ZoneNum, IntGainTypesUpSubzone, ConvGainsUpSubzone);
    ConvGainsUpSubzone += state.dataHeatBalFanSys->SumConvHTRadSys(ZoneNum);
    if (state.dataHeatBal->Zone(ZoneNum).NoHeatToReturnAir) {
        SumReturnAirConvectionGainsByTypes(state, ZoneNum, IntGainTypesUpSubzone, RetAirGains);
        ConvGainsUpSubzone += RetAirGains;
    }
    ConvGains = ConvGainsOccSubzone + ConvGainsUpSubzone + state.dataHeatBalFanSys->SysDepZoneLoadsLagged(ZoneNum);
    ZoneEquipConfigNum = state.dataRoomAirMod->ZoneUCSDUE(UINum).ZoneEquipPtr;
    if (ZoneEquipConfigNum > 0) {
        for (InNodeIndex = 1; InNodeIndex <= state.dataZoneEquip->ZoneEquipConfig(ZoneEquipConfigNum).NumInletNodes; ++InNodeIndex) {
            NodeTemp = state.dataLoopNodes->Node(state.dataZoneEquip->ZoneEquipConfig(ZoneEquipConfigNum).InletNode(InNodeIndex)).Temp;
            MassFlowRate = state.dataLoopNodes->Node(state.dataZoneEquip->ZoneEquipConfig(ZoneEquipConfigNum).InletNode(InNodeIndex)).MassFlowRate;
            CpAir = PsyCpAirFnW(state.dataHeatBalFanSys->ZoneAirHumRat(ZoneNum));
            SumSysMCp += MassFlowRate * CpAir;
            SumSysMCpT += MassFlowRate * CpAir * NodeTemp;
            TotSysFlow +=
                MassFlowRate / PsyRhoAirFnPbTdbW(state, state.dataEnvrn->OutBaroPress, NodeTemp, state.dataHeatBalFanSys->ZoneAirHumRat(ZoneNum));
            TSupK += MassFlowRate * NodeTemp;
            SumSysM += MassFlowRate;
        }
        if (TotSysFlow > 0.0) {
            TSupK = TSupK / SumSysM + DataGlobalConstants::KelvinConv;
        } else {
            TSupK = 0.0;
        }
    }
    // mass flow times specific heat for infiltration, ventilation, mixing
    SumMCp = state.dataHeatBalFanSys->MCPI(ZoneNum) + state.dataHeatBalFanSys->MCPV(ZoneNum) + state.dataHeatBalFanSys->MCPM(ZoneNum) +
             state.dataHeatBalFanSys->MDotCPOA(ZoneNum);
    // mass flow times specific heat times temperature for infiltration, ventilation, mixing
    SumMCpT = state.dataHeatBalFanSys->MCPTI(ZoneNum) + state.dataHeatBalFanSys->MCPTV(ZoneNum) + state.dataHeatBalFanSys->MCPTM(ZoneNum) +
              state.dataHeatBalFanSys->MDotCPOA(ZoneNum) * state.dataHeatBal->Zone(ZoneNum).OutDryBulbTemp;

    MCp_Total = SumMCp + SumSysMCp;
    MCpT_Total = SumMCpT + SumSysMCpT;

    // For the York MIT diffusers (variable area) the area varies with the flow rate. Assume 400 ft/min velocity
    // at the diffuser, and a design flow rate of 150 cfm (.0708 m3/s). Then the design area for each diffuser is
    // 150 ft3/min / 400 ft/min = .375 ft2 = .035 m2. This is adjusted each time step by
    //               (TotSysFlow/(NumDiffusers*.0708))*.035
    if (state.dataRoomAirMod->ZoneUCSDUE(UINum).DiffuserType == Diffuser::VarArea) {
        DiffArea = 0.035 * TotSysFlow / (0.0708 * NumDiffusers);
    }
    // initial estimate of convective transfer from surfaces; assume HeightFrac is 0.5.
    HcUCSDUF(state, ZoneNum, 0.5);
    ConvGainsWindows = state.dataUFADManager->HAT_MXWin + state.dataUFADManager->HAT_OCWin -
                       state.dataUFADManager->HA_MXWin * state.dataRoomAirMod->ZTMX(ZoneNum) -
                       state.dataUFADManager->HA_OCWin * state.dataRoomAirMod->ZTOC(ZoneNum);
    PowerInPlumes = ConvGains + state.dataUFADManager->HAT_OC - state.dataUFADManager->HA_OC * state.dataRoomAirMod->ZTOC(ZoneNum) +
                    state.dataUFADManager->HAT_MX - state.dataUFADManager->HA_MX * state.dataRoomAirMod->ZTMX(ZoneNum);
    // NumberOfPlumes = PowerInPlumes / PowerPerPlume
    if (PowerPerPlume > 0.0 && PowerInPlumes > 0.0) {
        NumberOfPlumes = PowerInPlumes / PowerPerPlume;
        NumDiffusersPerPlume = NumDiffusers / NumberOfPlumes;
    } else {
        NumberOfPlumes = 1.0;
        NumDiffusersPerPlume = 1.0;
    }
    if ((PowerInPlumes <= 0.0) || (TotSysFlow == 0.0) || (TSupK - DataGlobalConstants::KelvinConv) > state.dataHeatBalFanSys->MAT(ZoneNum)) {
        // The system will mix
        HeightFrac = 0.0;
    } else {
        if (PowerInPlumes > 0.0) {
            if (state.dataRoomAirMod->ZoneUCSDUE(UINum).WinWidth > 0.0) { // exterior zone formula
                PowerInPlumesPerMeter = PowerInPlumes / state.dataRoomAirMod->ZoneUCSDUE(UINum).WinWidth;
                Gamma = (TotSysFlow * std::cos(ThrowAngle)) / (NumDiffusers * DiffArea * std::pow(0.0281 * 0.001 * PowerInPlumesPerMeter, 0.333333));
            } else { // interior zone formula
                Gamma = std::pow(TotSysFlow * std::cos(ThrowAngle), 1.5) /
                        (NumberOfPlumes * std::pow(NumDiffusersPerPlume * DiffArea, 1.25) * std::sqrt(0.0281 * 0.001 * PowerInPlumes));
            }
        } else {
            Gamma = 1000.0;
        }
        if (state.dataRoomAirMod->ZoneUCSDUE(UINum).CalcTransHeight) {
            if (state.dataRoomAirMod->ZoneUCSDUE(UINum).WinWidth > 0.0) { // use exterior zone formula
                HeightFrac = (std::sqrt(DiffArea) * (11.03 * std::log(Gamma) - 10.73) + 0.5 * SourceHeight) / CeilingHeight;
            } else { // use interior zone formula
                HeightFrac = (std::sqrt(NumDiffusersPerPlume * DiffArea) * (7.43 * std::log(Gamma) - 1.35) + 0.5 * SourceHeight) / CeilingHeight;
            }
        } else {
            HeightFrac = state.dataRoomAirMod->ZoneUCSDUE(UINum).TransHeight / CeilingHeight;
        }
        HeightFrac = max(0.0, min(1.0, HeightFrac));
        GainsFrac = state.dataRoomAirMod->ZoneUCSDUE(UINum).A_Kc * std::pow(Gamma, state.dataRoomAirMod->ZoneUCSDUE(UINum).B_Kc) +
                    state.dataRoomAirMod->ZoneUCSDUE(UINum).C_Kc + state.dataRoomAirMod->ZoneUCSDUE(UINum).D_Kc * Gamma +
                    state.dataRoomAirMod->ZoneUCSDUE(UINum).E_Kc * pow_2(Gamma);
        GainsFrac = max(0.7, min(GainsFrac, 1.0));
        if (state.dataRoomAirMod->ZoneUCSDUE(UINum).ShadeDown) {
            GainsFrac -= 0.2;
        }
        state.dataRoomAirMod->ZoneUFPowInPlumes(ZoneNum) = PowerInPlumes;
        for (Ctd = 1; Ctd <= 4; ++Ctd) {
            HcUCSDUF(state, ZoneNum, HeightFrac);
            ConvGainsWindows = state.dataUFADManager->HAT_MXWin + state.dataUFADManager->HAT_OCWin -
                               state.dataUFADManager->HA_MXWin * state.dataRoomAirMod->ZTMX(ZoneNum) -
                               state.dataUFADManager->HA_OCWin * state.dataRoomAirMod->ZTOC(ZoneNum);
            ConvGainsWindows = max(ConvGainsWindows, 0.0);
            PowerInPlumes = ConvGains + state.dataUFADManager->HAT_OC - state.dataUFADManager->HA_OC * state.dataRoomAirMod->ZTOC(ZoneNum) +
                            state.dataUFADManager->HAT_MX - state.dataUFADManager->HA_MX * state.dataRoomAirMod->ZTMX(ZoneNum);
            // NumberOfPlumes = PowerInPlumes / PowerPerPlume
            NumberOfPlumes = 1.0;
            if (PowerInPlumes <= 0.0) break;
            if (state.dataRoomAirMod->ZoneUCSDUE(UINum).WinWidth > 0.0) { // use exterior zone formula
                PowerInPlumesPerMeter = PowerInPlumes / state.dataRoomAirMod->ZoneUCSDUE(UINum).WinWidth;
                Gamma = (TotSysFlow * std::cos(ThrowAngle)) / (NumDiffusers * DiffArea * std::pow(0.0281 * 0.001 * PowerInPlumesPerMeter, 0.333333));
            } else { // use interior zone formula
                Gamma = std::pow(TotSysFlow * std::cos(ThrowAngle), 1.5) /
                        (NumberOfPlumes * std::pow(NumDiffusersPerPlume * DiffArea, 1.25) * std::sqrt(0.0281 * 0.001 * PowerInPlumes));
            }
            if (state.dataRoomAirMod->ZoneUCSDUE(UINum).CalcTransHeight) {
                if (state.dataRoomAirMod->ZoneUCSDUE(UINum).WinWidth > 0.0) { // exterior zone formula
                    HeightFrac = (std::sqrt(DiffArea) * (11.03 * std::log(Gamma) - 10.73) + 0.5 * SourceHeight) / CeilingHeight;
                } else { // interior zone formula
                    HeightFrac = (std::sqrt(NumDiffusersPerPlume * DiffArea) * (7.43 * std::log(Gamma) - 1.35) + 0.5 * SourceHeight) / CeilingHeight;
                }
            } else {
                HeightFrac = state.dataRoomAirMod->ZoneUCSDUE(UINum).TransHeight / CeilingHeight;
            }
            HeightFrac = min(1.0, HeightFrac);
            state.dataRoomAirMod->HeightTransition(ZoneNum) = HeightFrac * CeilingHeight;
            GainsFrac = state.dataRoomAirMod->ZoneUCSDUE(UINum).A_Kc * std::pow(Gamma, state.dataRoomAirMod->ZoneUCSDUE(UINum).B_Kc) +
                        state.dataRoomAirMod->ZoneUCSDUE(UINum).C_Kc + state.dataRoomAirMod->ZoneUCSDUE(UINum).D_Kc * Gamma +
                        state.dataRoomAirMod->ZoneUCSDUE(UINum).E_Kc * pow_2(Gamma);
            GainsFrac = max(0.7, min(GainsFrac, 1.0));
            if (state.dataRoomAirMod->ZoneUCSDUE(UINum).ShadeDown) {
                GainsFrac -= 0.2;
            }
            state.dataRoomAirMod->AIRRATOC(ZoneNum) =
                state.dataHeatBal->Zone(ZoneNum).Volume *
                (state.dataRoomAirMod->HeightTransition(ZoneNum) - min(state.dataRoomAirMod->HeightTransition(ZoneNum), 0.2)) / CeilingHeight *
                state.dataHeatBal->Zone(ZoneNum).ZoneVolCapMultpSens *
                PsyRhoAirFnPbTdbW(
                    state, state.dataEnvrn->OutBaroPress, state.dataRoomAirMod->MATOC(ZoneNum), state.dataHeatBalFanSys->ZoneAirHumRat(ZoneNum)) *
                PsyCpAirFnW(state.dataHeatBalFanSys->ZoneAirHumRat(ZoneNum)) / (TimeStepSys * DataGlobalConstants::SecInHour);
            state.dataRoomAirMod->AIRRATMX(ZoneNum) =
                state.dataHeatBal->Zone(ZoneNum).Volume * (CeilingHeight - state.dataRoomAirMod->HeightTransition(ZoneNum)) / CeilingHeight *
                state.dataHeatBal->Zone(ZoneNum).ZoneVolCapMultpSens *
                PsyRhoAirFnPbTdbW(
                    state, state.dataEnvrn->OutBaroPress, state.dataRoomAirMod->MATMX(ZoneNum), state.dataHeatBalFanSys->ZoneAirHumRat(ZoneNum)) *
                PsyCpAirFnW(state.dataHeatBalFanSys->ZoneAirHumRat(ZoneNum)) / (TimeStepSys * DataGlobalConstants::SecInHour);

            if (state.dataHVACGlobal->UseZoneTimeStepHistory) {
                state.dataRoomAirMod->ZTM3OC(ZoneNum) = state.dataRoomAirMod->XM3TOC(ZoneNum);
                state.dataRoomAirMod->ZTM2OC(ZoneNum) = state.dataRoomAirMod->XM2TOC(ZoneNum);
                state.dataRoomAirMod->ZTM1OC(ZoneNum) = state.dataRoomAirMod->XMATOC(ZoneNum);

                state.dataRoomAirMod->ZTM3MX(ZoneNum) = state.dataRoomAirMod->XM3TMX(ZoneNum);
                state.dataRoomAirMod->ZTM2MX(ZoneNum) = state.dataRoomAirMod->XM2TMX(ZoneNum);
                state.dataRoomAirMod->ZTM1MX(ZoneNum) = state.dataRoomAirMod->XMATMX(ZoneNum);

            } else {
                state.dataRoomAirMod->ZTM3OC(ZoneNum) = state.dataRoomAirMod->DSXM3TOC(ZoneNum);
                state.dataRoomAirMod->ZTM2OC(ZoneNum) = state.dataRoomAirMod->DSXM2TOC(ZoneNum);
                state.dataRoomAirMod->ZTM1OC(ZoneNum) = state.dataRoomAirMod->DSXMATOC(ZoneNum);

                state.dataRoomAirMod->ZTM3MX(ZoneNum) = state.dataRoomAirMod->DSXM3TMX(ZoneNum);
                state.dataRoomAirMod->ZTM2MX(ZoneNum) = state.dataRoomAirMod->DSXM2TMX(ZoneNum);
                state.dataRoomAirMod->ZTM1MX(ZoneNum) = state.dataRoomAirMod->DSXMATMX(ZoneNum);
            }

            AirCap = state.dataRoomAirMod->AIRRATOC(ZoneNum);
            TempHistTerm = AirCap * (3.0 * state.dataRoomAirMod->ZTM1OC(ZoneNum) - (3.0 / 2.0) * state.dataRoomAirMod->ZTM2OC(ZoneNum) +
                                     (1.0 / 3.0) * state.dataRoomAirMod->ZTM3OC(ZoneNum));
            TempDepCoef = GainsFrac * state.dataUFADManager->HA_OC + MCp_Total;
            TempIndCoef = GainsFrac * (ConvGains + state.dataUFADManager->HAT_OC + state.dataUFADManager->HAT_MX -
                                       state.dataUFADManager->HA_MX * state.dataRoomAirMod->ZTMX(ZoneNum)) +
                          MCpT_Total + state.dataHeatBalFanSys->NonAirSystemResponse(ZoneNum) / ZoneMult;
            {
                auto const SELECT_CASE_var(state.dataHeatBal->ZoneAirSolutionAlgo);
                if (SELECT_CASE_var == Use3rdOrder) {
                    state.dataRoomAirMod->ZTOC(ZoneNum) = (TempHistTerm +
                                                           GainsFrac * (ConvGains + state.dataUFADManager->HAT_OC + state.dataUFADManager->HAT_MX -
                                                                        state.dataUFADManager->HA_MX * state.dataRoomAirMod->ZTMX(ZoneNum)) +
                                                           MCpT_Total + state.dataHeatBalFanSys->NonAirSystemResponse(ZoneNum) / ZoneMult) /
                                                          ((11.0 / 6.0) * AirCap + GainsFrac * state.dataUFADManager->HA_OC + MCp_Total);
                } else if (SELECT_CASE_var == UseAnalyticalSolution) {
                    if (TempDepCoef == 0.0) { // B=0
                        state.dataRoomAirMod->ZTOC(ZoneNum) = state.dataRoomAirMod->Zone1OC(ZoneNum) + TempIndCoef / AirCap;
                    } else {
                        state.dataRoomAirMod->ZTOC(ZoneNum) =
                            (state.dataRoomAirMod->Zone1OC(ZoneNum) - TempIndCoef / TempDepCoef) * std::exp(min(700.0, -TempDepCoef / AirCap)) +
                            TempIndCoef / TempDepCoef;
                    }
                } else if (SELECT_CASE_var == UseEulerMethod) {
                    state.dataRoomAirMod->ZTOC(ZoneNum) = (AirCap * state.dataRoomAirMod->Zone1OC(ZoneNum) + TempIndCoef) / (AirCap + TempDepCoef);
                }
            }
            AirCap = state.dataRoomAirMod->AIRRATMX(ZoneNum);
            TempHistTerm = AirCap * (3.0 * state.dataRoomAirMod->ZTM1MX(ZoneNum) - (3.0 / 2.0) * state.dataRoomAirMod->ZTM2MX(ZoneNum) +
                                     (1.0 / 3.0) * state.dataRoomAirMod->ZTM3MX(ZoneNum));
            TempDepCoef = (1.0 - GainsFrac) * state.dataUFADManager->HA_MX + MCp_Total;
            TempIndCoef = (1.0 - GainsFrac) * (ConvGains + state.dataUFADManager->HAT_OC + state.dataUFADManager->HAT_MX -
                                               state.dataUFADManager->HA_OC * state.dataRoomAirMod->ZTOC(ZoneNum)) +
                          state.dataRoomAirMod->ZTOC(ZoneNum) * MCp_Total;
            {
                auto const SELECT_CASE_var(state.dataHeatBal->ZoneAirSolutionAlgo);
                if (SELECT_CASE_var == Use3rdOrder) {
                    state.dataRoomAirMod->ZTMX(ZoneNum) =
                        (TempHistTerm +
                         (1.0 - GainsFrac) * (ConvGains + state.dataUFADManager->HAT_OC + state.dataUFADManager->HAT_MX -
                                              state.dataUFADManager->HA_OC * state.dataRoomAirMod->ZTOC(ZoneNum)) +
                         state.dataRoomAirMod->ZTOC(ZoneNum) * MCp_Total) /
                        ((11.0 / 6.0) * AirCap + (1.0 - GainsFrac) * state.dataUFADManager->HA_MX + MCp_Total);
                } else if (SELECT_CASE_var == UseAnalyticalSolution) {
                    if (TempDepCoef == 0.0) { // B=0
                        state.dataRoomAirMod->ZTMX(ZoneNum) = state.dataRoomAirMod->Zone1MX(ZoneNum) + TempIndCoef / AirCap;
                    } else {
                        state.dataRoomAirMod->ZTMX(ZoneNum) =
                            (state.dataRoomAirMod->Zone1MX(ZoneNum) - TempIndCoef / TempDepCoef) * std::exp(min(700.0, -TempDepCoef / AirCap)) +
                            TempIndCoef / TempDepCoef;
                    }
                } else if (SELECT_CASE_var == UseEulerMethod) {
                    state.dataRoomAirMod->ZTMX(ZoneNum) = (AirCap * state.dataRoomAirMod->Zone1MX(ZoneNum) + TempIndCoef) / (AirCap + TempDepCoef);
                }
            }
            state.dataRoomAirMod->ZTFloor(ZoneNum) = state.dataRoomAirMod->ZTOC(ZoneNum);
        }
        if (PowerInPlumes <= 0.0) {
            HeightFrac = 0.0;
            state.dataRoomAirMod->AirModel(ZoneNum).SimAirModel = false;
            state.dataRoomAirMod->ZoneUFGamma(ZoneNum) = 0.0;
            state.dataRoomAirMod->ZoneUFPowInPlumes(ZoneNum) = 0.0;
            state.dataRoomAirMod->ZoneUFPowInPlumesfromWindows(ZoneNum) = 0.0;
        } else {
            state.dataRoomAirMod->AirModel(ZoneNum).SimAirModel = true;
            state.dataRoomAirMod->ZoneUFGamma(ZoneNum) = Gamma;
            state.dataRoomAirMod->ZoneUFPowInPlumes(ZoneNum) = PowerInPlumes;
            state.dataRoomAirMod->ZoneUFPowInPlumesfromWindows(ZoneNum) = ConvGainsWindows;
        }
    }

    //=============================== M I X E D  Calculation ==============================================
    if (state.dataRoomAirMod->ZTMX(ZoneNum) < state.dataRoomAirMod->ZTOC(ZoneNum) || MCp_Total <= 0.0 ||
        HeightFrac * CeilingHeight < state.dataUFADManager->ThickOccupiedSubzoneMin) {
        MIXFLAG = true;
        HeightFrac = 0.0;

        state.dataRoomAirMod->AvgTempGrad(ZoneNum) = 0.0;
        state.dataRoomAirMod->MaxTempGrad(ZoneNum) = 0.0;
        state.dataRoomAirMod->AirModel(ZoneNum).SimAirModel = false;
        AirCap = state.dataHeatBalFanSys->AIRRAT(ZoneNum);
        TempHistTerm = AirCap * (3.0 * state.dataHeatBalFanSys->ZTM1(ZoneNum) - (3.0 / 2.0) * state.dataHeatBalFanSys->ZTM2(ZoneNum) +
                                 (1.0 / 3.0) * state.dataHeatBalFanSys->ZTM3(ZoneNum));

        for (Ctd = 1; Ctd <= 3; ++Ctd) {
            TempDepCoef = state.dataUFADManager->HA_MX + state.dataUFADManager->HA_OC + MCp_Total;
            TempIndCoef = ConvGains + state.dataUFADManager->HAT_MX + state.dataUFADManager->HAT_OC + MCpT_Total;
            {
                auto const SELECT_CASE_var(state.dataHeatBal->ZoneAirSolutionAlgo);
                if (SELECT_CASE_var == Use3rdOrder) {
                    ZTAveraged = (TempHistTerm + ConvGains + state.dataUFADManager->HAT_MX + state.dataUFADManager->HAT_OC + MCpT_Total) /
                                 ((11.0 / 6.0) * AirCap + state.dataUFADManager->HA_MX + state.dataUFADManager->HA_OC + MCp_Total);
                } else if (SELECT_CASE_var == UseAnalyticalSolution) {
                    if (TempDepCoef == 0.0) { // B=0
                        ZTAveraged = state.dataHeatBalFanSys->ZoneT1(ZoneNum) + TempIndCoef / AirCap;
                    } else {
                        ZTAveraged =
                            (state.dataHeatBalFanSys->ZoneT1(ZoneNum) - TempIndCoef / TempDepCoef) * std::exp(min(700.0, -TempDepCoef / AirCap)) +
                            TempIndCoef / TempDepCoef;
                    }
                } else if (SELECT_CASE_var == UseEulerMethod) {
                    ZTAveraged = (AirCap * state.dataHeatBalFanSys->ZoneT1(ZoneNum) + TempIndCoef) / (AirCap + TempDepCoef);
                }
            }
            state.dataRoomAirMod->ZTOC(ZoneNum) = ZTAveraged;
            state.dataRoomAirMod->ZTMX(ZoneNum) = ZTAveraged;
            state.dataRoomAirMod->ZTFloor(ZoneNum) = ZTAveraged;
            HcUCSDUF(state, ZoneNum, HeightFrac);
            TempDepCoef = state.dataUFADManager->HA_MX + state.dataUFADManager->HA_OC + MCp_Total;
            TempIndCoef = ConvGains + state.dataUFADManager->HAT_MX + state.dataUFADManager->HAT_OC + MCpT_Total;
            {
                auto const SELECT_CASE_var(state.dataHeatBal->ZoneAirSolutionAlgo);
                if (SELECT_CASE_var == Use3rdOrder) {
                    ZTAveraged = (TempHistTerm + ConvGains + state.dataUFADManager->HAT_MX + state.dataUFADManager->HAT_OC + MCpT_Total) /
                                 ((11.0 / 6.0) * AirCap + state.dataUFADManager->HA_MX + state.dataUFADManager->HA_OC + MCp_Total);
                } else if (SELECT_CASE_var == UseAnalyticalSolution) {
                    if (TempDepCoef == 0.0) { // B=0
                        ZTAveraged = state.dataHeatBalFanSys->ZoneT1(ZoneNum) + TempIndCoef / AirCap;
                    } else {
                        ZTAveraged =
                            (state.dataHeatBalFanSys->ZoneT1(ZoneNum) - TempIndCoef / TempDepCoef) * std::exp(min(700.0, -TempDepCoef / AirCap)) +
                            TempIndCoef / TempDepCoef;
                    }
                } else if (SELECT_CASE_var == UseEulerMethod) {
                    ZTAveraged = (AirCap * state.dataHeatBalFanSys->ZoneT1(ZoneNum) + TempIndCoef) / (AirCap + TempDepCoef);
                }
            }
            state.dataRoomAirMod->ZTOC(ZoneNum) = ZTAveraged;
            state.dataRoomAirMod->ZTMX(ZoneNum) = ZTAveraged;
            state.dataRoomAirMod->ZTFloor(ZoneNum) = ZTAveraged;
        }
    }
    //=========================================================================================

    // Comfort temperature and temperature at the thermostat/temperature control sensor

    HeightUpSubzoneAve = (CeilingHeight + state.dataRoomAirMod->HeightTransition(ZoneNum)) / 2.0;
    HeightOccupiedSubzoneAve = state.dataRoomAirMod->HeightTransition(ZoneNum) / 2.0;
    // Comfort temperature

    if (MIXFLAG) {
        state.dataRoomAirMod->TCMF(ZoneNum) = ZTAveraged;
    } else {
        if (HeightComfort < HeightOccupiedSubzoneAve) {
            state.dataRoomAirMod->TCMF(ZoneNum) = state.dataRoomAirMod->ZTOC(ZoneNum);
        } else if (HeightComfort >= HeightOccupiedSubzoneAve && HeightComfort < HeightUpSubzoneAve) {
            state.dataRoomAirMod->TCMF(ZoneNum) = (state.dataRoomAirMod->ZTOC(ZoneNum) * (HeightUpSubzoneAve - HeightComfort) +
                                                   state.dataRoomAirMod->ZTMX(ZoneNum) * (HeightComfort - HeightOccupiedSubzoneAve)) /
                                                  (HeightUpSubzoneAve - HeightOccupiedSubzoneAve);
        } else if (HeightComfort >= HeightUpSubzoneAve && HeightComfort <= CeilingHeight) {
            state.dataRoomAirMod->TCMF(ZoneNum) = state.dataRoomAirMod->ZTMX(ZoneNum);
        } else {
            ShowFatalError(state, "UFAD comfort height is above ceiling or below floor in Zone: " + state.dataHeatBal->Zone(ZoneNum).Name);
        }
    }

    // Temperature at the thermostat/temperature control sensor

    if (MIXFLAG) {
        state.dataHeatBalFanSys->TempTstatAir(ZoneNum) = ZTAveraged;
    } else {
        if (HeightThermostat < HeightOccupiedSubzoneAve) {
            state.dataHeatBalFanSys->TempTstatAir(ZoneNum) = state.dataRoomAirMod->ZTOC(ZoneNum);
        } else if (HeightThermostat >= HeightOccupiedSubzoneAve && HeightThermostat < HeightUpSubzoneAve) {
            state.dataHeatBalFanSys->TempTstatAir(ZoneNum) = (state.dataRoomAirMod->ZTOC(ZoneNum) * (HeightUpSubzoneAve - HeightThermostat) +
                                                              state.dataRoomAirMod->ZTMX(ZoneNum) * (HeightThermostat - HeightOccupiedSubzoneAve)) /
                                                             (HeightUpSubzoneAve - HeightOccupiedSubzoneAve);
        } else if (HeightThermostat >= HeightUpSubzoneAve && HeightThermostat <= CeilingHeight) {
            state.dataHeatBalFanSys->TempTstatAir(ZoneNum) = state.dataRoomAirMod->ZTMX(ZoneNum);
        } else {
            ShowFatalError(state,
                           "Underfloor air distribution thermostat height is above ceiling or below floor in Zone: " +
                               state.dataHeatBal->Zone(ZoneNum).Name);
        }
    }

    // Temperature gradients
    if ((HeightUpSubzoneAve - HeightOccupiedSubzoneAve) > 0.1) {
        state.dataRoomAirMod->AvgTempGrad(ZoneNum) =
            (state.dataRoomAirMod->ZTMX(ZoneNum) - state.dataRoomAirMod->ZTOC(ZoneNum)) / (HeightUpSubzoneAve - HeightOccupiedSubzoneAve);
    } else {
        state.dataRoomAirMod->AvgTempGrad(ZoneNum) = 0.0;
    }

    if (MIXFLAG) {
        state.dataRoomAirMod->ZoneUFMixedFlag(ZoneNum) = 1;
        state.dataRoomAirMod->AirModel(ZoneNum).SimAirModel = false;
    } else {
        state.dataRoomAirMod->ZoneUFMixedFlag(ZoneNum) = 0;
        state.dataRoomAirMod->AirModel(ZoneNum).SimAirModel = true;
    }

    if (ZoneEquipConfigNum > 0) {
        ZoneNodeNum = state.dataHeatBal->Zone(ZoneNum).SystemZoneNodeNumber;
        state.dataLoopNodes->Node(ZoneNodeNum).Temp = state.dataRoomAirMod->ZTMX(ZoneNum);
    }

    if (MIXFLAG) {
        state.dataRoomAirMod->Phi(ZoneNum) = 1.0;
    } else {
        state.dataRoomAirMod->Phi(ZoneNum) = (state.dataRoomAirMod->ZTOC(ZoneNum) - (TSupK - DataGlobalConstants::KelvinConv)) /
                                             (state.dataRoomAirMod->ZTMX(ZoneNum) - (TSupK - DataGlobalConstants::KelvinConv));
    }

    // Mixed for reporting purposes
    if ((MIXFLAG) || ((state.dataRoomAirMod->ZTMX(ZoneNum) - state.dataRoomAirMod->ZTOC(ZoneNum)) < TempDiffCritRep)) {
        state.dataRoomAirMod->ZoneUFMixedFlagRep(ZoneNum) = 1.0;
        state.dataRoomAirMod->HeightTransition(ZoneNum) = 0.0;
        state.dataRoomAirMod->AvgTempGrad(ZoneNum) = 0.0;
    } else {
        state.dataRoomAirMod->ZoneUFMixedFlagRep(ZoneNum) = 0.0;
    }
}

} // namespace EnergyPlus::UFADManager
