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
#include <algorithm>
#include <cmath>
#include <limits>

// ObjexxFCL Headers
#include <ObjexxFCL/Array.functions.hh>
#include <ObjexxFCL/Array1D.hh>
#include <ObjexxFCL/Array2D.hh>
#include <ObjexxFCL/Fmath.hh>

// EnergyPlus Headers
#include <AirflowNetwork/Solver.hpp>
#include <EnergyPlus/CrossVentMgr.hh>
#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataErrorTracking.hh>
#include <EnergyPlus/DataHVACGlobals.hh>
#include <EnergyPlus/DataHeatBalFanSys.hh>
#include <EnergyPlus/DataHeatBalance.hh>
#include <EnergyPlus/DataIPShortCuts.hh>
#include <EnergyPlus/DataLoopNode.hh>
#include <EnergyPlus/DataRoomAirModel.hh>
#include <EnergyPlus/DataSurfaces.hh>
#include <EnergyPlus/DataZoneEquipment.hh>
#include <EnergyPlus/DisplacementVentMgr.hh>
#include <EnergyPlus/Fans.hh>
#include <EnergyPlus/General.hh>
#include <EnergyPlus/InputProcessing/InputProcessor.hh>
#include <EnergyPlus/InternalHeatGains.hh>
#include <EnergyPlus/MundtSimMgr.hh>
#include <EnergyPlus/OutputProcessor.hh>
#include <EnergyPlus/Psychrometrics.hh>
#include <EnergyPlus/RoomAirModelAirflowNetwork.hh>
#include <EnergyPlus/RoomAirModelManager.hh>
#include <EnergyPlus/RoomAirModelUserTempPattern.hh>
#include <EnergyPlus/ScheduleManager.hh>
#include <EnergyPlus/UFADManager.hh>
#include <EnergyPlus/UtilityRoutines.hh>
#include <EnergyPlus/ZoneTempPredictorCorrector.hh>

namespace EnergyPlus {

namespace RoomAir {

    // MODULE INFORMATION
    //       AUTHOR         Weixiu Kong
    //       DATE WRITTEN   March 2003
    //       MODIFIED       July 2003, CC
    //                      Aug, 2005, BG

    // PURPOSE OF THIS MODULE:
    // Contains subroutines for managing the room air models

    constexpr std::array<std::string_view, (int)RoomAirModel::Num> roomAirModelNamesUC =
        { "USERDEFINED", "MIXING", "MUNDT", "UCSD_DV", "UCSD_CV", "UCSD_UFI", "UCSD_UFE", "AIRFLOWNETWORK"};

    constexpr std::array<std::string_view, (int)AirNodeType::Num> airNodeTypeNamesUC =
        {"INLET", "FLOOR", "CONTROL", "CEILING", "MUNDTROOM", "RETURN", "AIRFLOWNETWORK", "PLUME", "REESROOM"};

    constexpr std::array<std::string_view, (int)Comfort::Num> comfortNamesUC =
        {"JET", "RECIRCULATION"};

    constexpr std::array<std::string_view, (int)Diffuser::Num> diffuserNamesUC =
        {"SWIRL", "VARIABLEAREA", "HORIZONTALSWIRL", "LINEARBARGRILLE", "CUSTOM"};
        
    constexpr std::array<std::string_view, (int)UserDefinedPatternMode::Num> userDefinedPatternModeNamesUC =
        {"OUTDOORDRYBULBTEMPERATURE", "SENSIBLECOOLINGLOAD", "SENSIBLEHEATINGLOAD", "ZONEDRYBULBTEMPERATURE", "ZONEANDOUTDOORTEMPERATUREDIFFERENCE"};

    void ManageAirModel(EnergyPlusData &state, int const ZoneNum)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Weixiu Kong
        //       DATE WRITTEN   April 2003
        //       MODIFIED       July 2003, CC
        //                      Jan 2004, CC

        // PURPOSE OF THIS SUBROUTINE:
        //     manage room air models.

        if (state.dataRoomAirModelMgr->GetAirModelData) {
            GetAirModelDatas(state);
            state.dataRoomAirModelMgr->GetAirModelData = false;
        }

        if (!state.dataRoomAirMod->anyNonMixingRoomAirModel) return;

        if (state.dataRoomAirMod->UCSDModelUsed) {
            SharedDVCVUFDataInit(state, ZoneNum);
        }

        switch (state.dataRoomAirMod->AirModel(ZoneNum).AirModel) {
        case RoomAirModel::UserDefined:
            ManageUserDefinedPatterns(state, ZoneNum);
            break;

        case RoomAirModel::Mixing: // Mixing air model
            break;                                   // do nothing

        case RoomAirModel::DispVent1Node: // Mundt air model
            // simulate room airflow using Mundt model
            ManageMundtModel(state, ZoneNum);
            break;

        case RoomAirModel::DispVent3Node: // UCDV Displacement Ventilation model
            // simulate room airflow using UCSDDV model
            ManageUCSDDVModel(state, ZoneNum);
            break;

        case RoomAirModel::CrossVent: // UCSD Cross Ventilation model
            // simulate room airflow using UCSDDV model
            ManageUCSDCVModel(state, ZoneNum);
            break;

        case RoomAirModel::UFADInt: // UCSD UFAD interior zone model
            // simulate room airflow using the UCSDUFI model
            ManageUCSDUFModels(state, ZoneNum, RoomAirModel::UFADInt);
            break;

        case RoomAirModel::UFADExt: // UCSD UFAD exterior zone model
            // simulate room airflow using the UCSDUFE model
            ManageUCSDUFModels(state, ZoneNum, RoomAirModel::UFADExt);
            break;

        case RoomAirModel::AirflowNetwork: // RoomAirflowNetwork zone model
            // simulate room airflow using the AirflowNetwork - based model
            SimRoomAirModelAirflowNetwork(state, ZoneNum);
            break;

        default:   // mixing air model
            break; // do nothing
        }
    }

    //*****************************************************************************************

    void GetAirModelDatas(EnergyPlusData &state)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Linda Lawrie
        //       DATE WRITTEN   March 2005
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This routine "gets" all the data for the "RoomAir" models by calling individual
        // routines.

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        bool ErrorsFound;

        ErrorsFound = false;
        // get air node input data for all zones
        GetAirNodeData(state, ErrorsFound);

        // get mundt model controls for all zones
        GetMundtData(state, ErrorsFound);

        // get airflow network model info for all zones
        GetRoomAirflowNetworkData(state, ErrorsFound);

        // get UCSDDV model controls for all zones
        GetDisplacementVentData(state, ErrorsFound);

        // get UCSDCV model controls for all zones
        GetCrossVentData(state, ErrorsFound);

        // get BTG's user-defined patterns for all zones
        GetUserDefinedPatternData(state, ErrorsFound);

        // get UCSD UFAD interior zone model controls for all zones
        // get UCSD UFAD exterior zone model controls for all zones
        GetUFADZoneData(state, ErrorsFound);

        if (ErrorsFound) {
            ShowFatalError(state, "GetAirModelData: Errors found getting air model input.  Program terminates.");
        }
    }

    void GetUserDefinedPatternData(EnergyPlusData &state, bool &ErrorsFound) // True if errors found during this get input routine
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR         Brent Griffith
        //       DATE WRITTEN   Aug 2005
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This routine "gets" all the data for the "User-Defined RoomAir"

        // METHODOLOGY EMPLOYED:
        // usual energyplus input routines
        // for the actual patterns, a single structure array holds
        // different patterns in nested derived types.

        // Using/Aliasing
        using DataZoneEquipment::EquipConfiguration;

        using ScheduleManager::GetScheduleIndex;

        // SUBROUTINE PARAMETER DEFINITIONS:
        static constexpr std::string_view routineName = "GetUserDefinedPatternData: ";

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int NumAlphas;  // number of alphas
        int NumNumbers; // Number of numbers encountered
        int Status;     // Notes if there was an error in processing the input

        auto &ipsc = state.dataIPShortCut;
        
        // access input file and setup
        state.dataRoomAirMod->numTempDistContrldZones =
            state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, cUserDefinedControlObject);

        state.dataRoomAirMod->NumConstantGradient =
            state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, cTempPatternConstGradientObject);
        state.dataRoomAirMod->NumTwoGradientInterp =
            state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, cTempPatternTwoGradientObject);
        state.dataRoomAirMod->NumNonDimensionalHeight =
            state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, cTempPatternNDHeightObject);
        state.dataRoomAirMod->NumSurfaceMapping = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, cTempPatternSurfMapObject);

        state.dataRoomAirMod->NumAirTempPatterns = state.dataRoomAirMod->NumConstantGradient + state.dataRoomAirMod->NumTwoGradientInterp +
                                                   state.dataRoomAirMod->NumNonDimensionalHeight + state.dataRoomAirMod->NumSurfaceMapping;
        ipsc->cCurrentModuleObject = cUserDefinedControlObject;
        if (state.dataRoomAirMod->numTempDistContrldZones == 0) {
            if (state.dataRoomAirMod->NumAirTempPatterns != 0) { // user may have missed control object
                ShowWarningError(state, format("Missing {} object needed to use roomair temperature patterns", ipsc->cCurrentModuleObject));
                // ErrorsFound = .TRUE.
            }
            return;
        }

        // now allocate AirPatternZoneInfo to length of all zones for easy indexing
        if (!allocated(state.dataRoomAirMod->AirPatternZoneInfo)) {
            state.dataRoomAirMod->AirPatternZoneInfo.allocate(state.dataGlobal->NumOfZones);
        }

        for (int ObjNum = 1; ObjNum <= state.dataRoomAirMod->numTempDistContrldZones; ++ObjNum) {

            state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                     ipsc->cCurrentModuleObject,
                                                                     ObjNum,
                                                                     ipsc->cAlphaArgs,
                                                                     NumAlphas,
                                                                     ipsc->rNumericArgs,
                                                                     NumNumbers,
                                                                     Status,
                                                                     _,
                                                                     ipsc->lAlphaFieldBlanks,
                                                                     ipsc->cAlphaFieldNames,
                                                                     ipsc->cNumericFieldNames);

            ErrorObjectHeader eoh{routineName, ipsc->cCurrentModuleObject, ipsc->cAlphaArgs(1)};
            // first get zone ID
            int ZoneNum = UtilityRoutines::FindItemInList(ipsc->cAlphaArgs(2), state.dataHeatBal->Zone);
            if (ZoneNum == 0) { // throw error
                ShowSevereItemNotFound(state, eoh, ipsc->cAlphaFieldNames(2), ipsc->cAlphaArgs(2));
                ErrorsFound = true;
                return; // halt to avoid hard crash
            }

            auto &airPatternZoneInfo = state.dataRoomAirMod->AirPatternZoneInfo(ZoneNum);
            airPatternZoneInfo.IsUsed = true;
            airPatternZoneInfo.Name = ipsc->cAlphaArgs(1);     // Name of this Control Object
            airPatternZoneInfo.ZoneName = ipsc->cAlphaArgs(2); // Zone Name

            airPatternZoneInfo.AvailSched = ipsc->cAlphaArgs(3);
            if (ipsc->lAlphaFieldBlanks(3)) {
                airPatternZoneInfo.AvailSchedID = ScheduleManager::ScheduleAlwaysOn;
            } else {
                airPatternZoneInfo.AvailSchedID = GetScheduleIndex(state, ipsc->cAlphaArgs(3));
                if (airPatternZoneInfo.AvailSchedID == 0) {
                    ShowSevereItemNotFound(state, eoh, ipsc->cAlphaFieldNames(3), ipsc->cAlphaArgs(3));
                    ErrorsFound = true;
                }
            }

            airPatternZoneInfo.PatternCntrlSched =
                ipsc->cAlphaArgs(4); // Schedule Name for Leading Pattern Control for this Zone
            airPatternZoneInfo.PatternSchedID = GetScheduleIndex(state, ipsc->cAlphaArgs(4));
            if (airPatternZoneInfo.PatternSchedID == 0) {
                ShowSevereItemNotFound(state, eoh, ipsc->cAlphaFieldNames(4), ipsc->cAlphaArgs(4));
                ErrorsFound = true;
            }

            airPatternZoneInfo.ZoneID = ZoneNum;

            //   figure number of surfaces for this zone
            airPatternZoneInfo.totNumSurfs = 0;
            for (int spaceNum : state.dataHeatBal->Zone(ZoneNum).spaceIndexes) {
                auto &thisSpace = state.dataHeatBal->space(spaceNum);
                airPatternZoneInfo.totNumSurfs += thisSpace.HTSurfaceLast - thisSpace.HTSurfaceFirst + 1;
            }
            //   allocate nested derived type for surface info
            airPatternZoneInfo.Surf.allocate(airPatternZoneInfo.totNumSurfs);

            //   Fill in what we know for nested structure for surfaces
            int thisSurfinZone = 0;
            for (int spaceNum : state.dataHeatBal->Zone(ZoneNum).spaceIndexes) {
                auto &thisSpace = state.dataHeatBal->space(spaceNum);
                for (int thisHBsurfID = thisSpace.HTSurfaceFirst; thisHBsurfID <= thisSpace.HTSurfaceLast; ++thisHBsurfID) {
                    ++thisSurfinZone;
                    if (state.dataSurface->Surface(thisHBsurfID).Class == DataSurfaces::SurfaceClass::IntMass) {
                        airPatternZoneInfo.Surf(thisSurfinZone).SurfID = thisHBsurfID;
                        airPatternZoneInfo.Surf(thisSurfinZone).Zeta = 0.5;
                        continue;
                    }

                    airPatternZoneInfo.Surf(thisSurfinZone).SurfID = thisHBsurfID;

                    airPatternZoneInfo.Surf(thisSurfinZone).Zeta = FigureNDheightInZone(state, thisHBsurfID);
                }
            } // loop through surfaces in this zone

        } // loop through number of 'RoomAir:TemperaturePattern:UserDefined' objects

        // Check against AirModel.  Make sure there is a match here.
        for (int iZone = 1; iZone <= state.dataGlobal->NumOfZones; ++iZone) {
            if (state.dataRoomAirMod->AirModel(iZone).AirModel != RoomAirModel::UserDefined) continue;
            if (state.dataRoomAirMod->AirPatternZoneInfo(iZone).IsUsed) continue; // There is a Room Air Temperatures object for this zone
            ShowSevereError(state,
                            format("{}AirModel for Zone=[{}] is indicated as \"User Defined\".", routineName, state.dataHeatBal->Zone(iZone).Name));
            ShowContinueError(state, format("...but missing a {} object for control.", ipsc->cCurrentModuleObject));
            ErrorsFound = true;
        }

        // now get user defined temperature patterns
        if (!allocated(state.dataRoomAirMod->AirPattern)) {
            state.dataRoomAirMod->AirPattern.allocate(state.dataRoomAirMod->NumAirTempPatterns);
        }

        // Four different objects to get
        ipsc->cCurrentModuleObject = cTempPatternConstGradientObject;
        for (int ObjNum = 1; ObjNum <= state.dataRoomAirMod->NumConstantGradient; ++ObjNum) {
            int thisPattern = ObjNum;
            state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                     ipsc->cCurrentModuleObject,
                                                                     ObjNum,
                                                                     ipsc->cAlphaArgs,
                                                                     NumAlphas,
                                                                     ipsc->rNumericArgs,
                                                                     NumNumbers,
                                                                     Status,
                                                                     _,
                                                                     _,
                                                                     ipsc->cAlphaFieldNames,
                                                                     ipsc->cNumericFieldNames);

            auto &roomAirPattern = state.dataRoomAirMod->AirPattern(thisPattern);
            roomAirPattern.Name = ipsc->cAlphaArgs(1);
            roomAirPattern.PatrnID = ipsc->rNumericArgs(1);
            roomAirPattern.PatternMode = UserDefinedPatternType::ConstGradTemp;
            roomAirPattern.DeltaTstat = ipsc->rNumericArgs(2);
            roomAirPattern.DeltaTleaving = ipsc->rNumericArgs(3);
            roomAirPattern.DeltaTexhaust = ipsc->rNumericArgs(4);
            roomAirPattern.GradPatrn.Gradient = ipsc->rNumericArgs(5);
        }

        ipsc->cCurrentModuleObject = cTempPatternTwoGradientObject;
        for (int ObjNum = 1; ObjNum <= state.dataRoomAirMod->NumTwoGradientInterp; ++ObjNum) {
            int thisPattern = state.dataRoomAirMod->NumConstantGradient + ObjNum;
            state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                     ipsc->cCurrentModuleObject,
                                                                     ObjNum,
                                                                     ipsc->cAlphaArgs,
                                                                     NumAlphas,
                                                                     ipsc->rNumericArgs,
                                                                     NumNumbers,
                                                                     Status,
                                                                     _,
                                                                     _,
                                                                     ipsc->cAlphaFieldNames,
                                                                     ipsc->cNumericFieldNames);


            auto &roomAirPattern = state.dataRoomAirMod->AirPattern(thisPattern);
            ErrorObjectHeader eoh{routineName, ipsc->cCurrentModuleObject, ipsc->cAlphaArgs(1)};
            roomAirPattern.PatternMode = UserDefinedPatternType::TwoGradInterp;
            roomAirPattern.Name = ipsc->cAlphaArgs(1);
            roomAirPattern.PatrnID = ipsc->rNumericArgs(1);
            roomAirPattern.TwoGradPatrn.TstatHeight = ipsc->rNumericArgs(2);
            roomAirPattern.TwoGradPatrn.TleavingHeight = ipsc->rNumericArgs(3);
            roomAirPattern.TwoGradPatrn.TexhaustHeight = ipsc->rNumericArgs(4);
            roomAirPattern.TwoGradPatrn.LowGradient = ipsc->rNumericArgs(5);
            roomAirPattern.TwoGradPatrn.HiGradient = ipsc->rNumericArgs(6);

            roomAirPattern.TwoGradPatrn.InterpolationMode =
                static_cast<UserDefinedPatternMode>(getEnumValue(userDefinedPatternModeNamesUC, UtilityRoutines::makeUPPER(ipsc->cAlphaArgs(2))));
            if (roomAirPattern.TwoGradPatrn.InterpolationMode == UserDefinedPatternMode::Invalid) {
                ShowSevereInvalidKey(state, eoh, ipsc->cAlphaFieldNames(2), ipsc->cAlphaArgs(2));
                ErrorsFound = true;
            }

            roomAirPattern.TwoGradPatrn.UpperBoundTempScale = ipsc->rNumericArgs(7);
            roomAirPattern.TwoGradPatrn.LowerBoundTempScale = ipsc->rNumericArgs(8);

            roomAirPattern.TwoGradPatrn.UpperBoundHeatRateScale = ipsc->rNumericArgs(9);
            roomAirPattern.TwoGradPatrn.LowerBoundHeatRateScale = ipsc->rNumericArgs(10);

            // now test the input some
            if (roomAirPattern.TwoGradPatrn.HiGradient ==
                roomAirPattern.TwoGradPatrn.LowGradient) {
                ShowWarningError(state, format("Upper and lower gradients equal, use {} instead ", cTempPatternConstGradientObject));
                ShowContinueError(state, format("Entered in {} = {}", ipsc->cCurrentModuleObject, ipsc->cAlphaArgs(1)));
            }
            if ((roomAirPattern.TwoGradPatrn.UpperBoundTempScale == roomAirPattern.TwoGradPatrn.LowerBoundTempScale) &&
                ((roomAirPattern.TwoGradPatrn.InterpolationMode == UserDefinedPatternMode::OutdoorDryBulb) ||
                 (roomAirPattern.TwoGradPatrn.InterpolationMode == UserDefinedPatternMode::ZoneAirTemp) ||
                 (roomAirPattern.TwoGradPatrn.InterpolationMode == UserDefinedPatternMode::DeltaOutdoorZone))) {
                // throw error, will cause divide by zero when used for scaling
                ShowSevereError(state, format("Error in temperature scale in {}: {}", ipsc->cCurrentModuleObject, ipsc->cAlphaArgs(1)));
                ErrorsFound = true;
            }
            if ((roomAirPattern.TwoGradPatrn.HiGradient == roomAirPattern.TwoGradPatrn.LowGradient) &&
                ((roomAirPattern.TwoGradPatrn.InterpolationMode == UserDefinedPatternMode::SensibleCooling) ||
                 (roomAirPattern.TwoGradPatrn.InterpolationMode == UserDefinedPatternMode::SensibleHeating))) {
                // throw error, will cause divide by zero when used for scaling
                ShowSevereError(state, format("Error in load scale in {}: {}", ipsc->cCurrentModuleObject, ipsc->cAlphaArgs(1)));
                ErrorsFound = true;
            }
        }

        ipsc->cCurrentModuleObject = cTempPatternNDHeightObject;
        for (int ObjNum = 1; ObjNum <= state.dataRoomAirMod->NumNonDimensionalHeight; ++ObjNum) {
            int thisPattern = state.dataRoomAirMod->NumConstantGradient + state.dataRoomAirMod->NumTwoGradientInterp + ObjNum;
            state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                     ipsc->cCurrentModuleObject,
                                                                     ObjNum,
                                                                     ipsc->cAlphaArgs,
                                                                     NumAlphas,
                                                                     ipsc->rNumericArgs,
                                                                     NumNumbers,
                                                                     Status,
                                                                     _,
                                                                     _,
                                                                     ipsc->cAlphaFieldNames,
                                                                     ipsc->cNumericFieldNames);
            auto &roomAirPattern = state.dataRoomAirMod->AirPattern(thisPattern);
            roomAirPattern.PatternMode = UserDefinedPatternType::NonDimenHeight;

            roomAirPattern.Name = ipsc->cAlphaArgs(1);
            roomAirPattern.PatrnID = ipsc->rNumericArgs(1);
            roomAirPattern.DeltaTstat = ipsc->rNumericArgs(2);
            roomAirPattern.DeltaTleaving = ipsc->rNumericArgs(3);
            roomAirPattern.DeltaTexhaust = ipsc->rNumericArgs(4);

            int NumPairs = std::floor((double(NumNumbers) - 4.0) / 2.0);

            // TODO error checking

            roomAirPattern.VertPatrn.ZetaPatrn.allocate(NumPairs);
            roomAirPattern.VertPatrn.DeltaTaiPatrn.allocate(NumPairs);

            // init these since they can't be in derived type
            roomAirPattern.VertPatrn.ZetaPatrn = 0.0;
            roomAirPattern.VertPatrn.DeltaTaiPatrn = 0.0;

            for (int i = 0; i <= NumPairs - 1; ++i) {

                roomAirPattern.VertPatrn.ZetaPatrn(i + 1) = ipsc->rNumericArgs(2 * i + 5);
                roomAirPattern.VertPatrn.DeltaTaiPatrn(i + 1) = ipsc->rNumericArgs(2 * i + 6);
            }

            // TODO  check order (TODO sort ? )
            for (int i = 2; i <= NumPairs; ++i) {
                if (roomAirPattern.VertPatrn.ZetaPatrn(i) < roomAirPattern.VertPatrn.ZetaPatrn(i - 1)) {
                    ShowSevereError(
                        state, format("Zeta values not in increasing order in {}: {}", ipsc->cCurrentModuleObject, ipsc->cAlphaArgs(1)));
                    ErrorsFound = true;
                }
            }
        }

        ipsc->cCurrentModuleObject = cTempPatternSurfMapObject;
        for (int ObjNum = 1; ObjNum <= state.dataRoomAirMod->NumSurfaceMapping; ++ObjNum) {
            int thisPattern = state.dataRoomAirMod->NumConstantGradient + state.dataRoomAirMod->NumTwoGradientInterp +
                          state.dataRoomAirMod->NumNonDimensionalHeight + ObjNum;

            state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                     ipsc->cCurrentModuleObject,
                                                                     ObjNum,
                                                                     ipsc->cAlphaArgs,
                                                                     NumAlphas,
                                                                     ipsc->rNumericArgs,
                                                                     NumNumbers,
                                                                     Status,
                                                                     _,
                                                                     _,
                                                                     ipsc->cAlphaFieldNames,
                                                                     ipsc->cNumericFieldNames);

            auto &roomAirPattern = state.dataRoomAirMod->AirPattern(thisPattern);
            ErrorObjectHeader eoh{routineName, ipsc->cCurrentModuleObject, ipsc->cAlphaArgs(1)};
            
            roomAirPattern.PatternMode = UserDefinedPatternType::SurfMapTemp;
            roomAirPattern.Name = ipsc->cAlphaArgs(1);
            roomAirPattern.PatrnID = ipsc->rNumericArgs(1);
            roomAirPattern.DeltaTstat = ipsc->rNumericArgs(2);
            roomAirPattern.DeltaTleaving = ipsc->rNumericArgs(3);
            roomAirPattern.DeltaTexhaust = ipsc->rNumericArgs(4);

            int NumPairs = NumNumbers - 4;

            if (NumPairs != (NumAlphas - 1)) {
                ShowSevereError(state,
                                format("Error in number of entries in {} object: {}", ipsc->cCurrentModuleObject, ipsc->cAlphaArgs(1)));
                ErrorsFound = true;
            }
            roomAirPattern.MapPatrn.SurfName.allocate(NumPairs);
            roomAirPattern.MapPatrn.DeltaTai.allocate(NumPairs);
            roomAirPattern.MapPatrn.SurfID.allocate(NumPairs);

            // init just allocated
            roomAirPattern.MapPatrn.SurfName = "";
            roomAirPattern.MapPatrn.DeltaTai = 0.0;
            roomAirPattern.MapPatrn.SurfID = 0;

            for (int i = 1; i <= NumPairs; ++i) {
                roomAirPattern.MapPatrn.SurfName(i) = ipsc->cAlphaArgs(i + 1);
                roomAirPattern.MapPatrn.DeltaTai(i) = ipsc->rNumericArgs(i + 4);
                roomAirPattern.MapPatrn.SurfID(i) = UtilityRoutines::FindItemInList(ipsc->cAlphaArgs(i + 1), state.dataSurface->Surface);
                if (roomAirPattern.MapPatrn.SurfID(i) == 0) {
                    ShowSevereItemNotFound(state, eoh, ipsc->cAlphaFieldNames(i+1), ipsc->cAlphaArgs(i+1));
                    ErrorsFound = true;
                }
            }
            roomAirPattern.MapPatrn.NumSurfs = NumPairs;
        }

        if (state.dataErrTracking->TotalRoomAirPatternTooLow > 0) {
            ShowWarningError(state,
                             format("GetUserDefinedPatternData: RoomAirModelUserTempPattern: {} problem(s) in non-dimensional height calculations, "
                                    "too low surface height(s) in relation to floor height of zone(s).",
                                    state.dataErrTracking->TotalRoomAirPatternTooLow));
            ShowContinueError(state, "...Use OutputDiagnostics,DisplayExtraWarnings; to see details.");
            state.dataErrTracking->TotalWarningErrors += state.dataErrTracking->TotalRoomAirPatternTooLow;
        }
        if (state.dataErrTracking->TotalRoomAirPatternTooHigh > 0) {
            ShowWarningError(state,
                             format("GetUserDefinedPatternData: RoomAirModelUserTempPattern: {} problem(s) in non-dimensional height calculations, "
                                    "too high surface height(s) in relation to ceiling height of zone(s).",
                                    state.dataErrTracking->TotalRoomAirPatternTooHigh));
            ShowContinueError(state, "...Use OutputDiagnostics,DisplayExtraWarnings; to see details.");
            state.dataErrTracking->TotalWarningErrors += state.dataErrTracking->TotalRoomAirPatternTooHigh;
        }

        // now do one time setups from and checks on user data

        // Find and set return and exhaust node ids

        for (int i = 1; i <= state.dataGlobal->NumOfZones; ++i) {
            if (state.dataRoomAirMod->AirPatternZoneInfo(i).IsUsed) {
                // first get return and exhaust air node index
                int found = UtilityRoutines::FindItemInList(
                    state.dataRoomAirMod->AirPatternZoneInfo(i).ZoneName, state.dataZoneEquip->ZoneEquipConfig, &EquipConfiguration::ZoneName);
                if (found != 0) {

                    state.dataRoomAirMod->AirPatternZoneInfo(i).ZoneNodeID = state.dataZoneEquip->ZoneEquipConfig(found).ZoneNode;
                    if (allocated(state.dataZoneEquip->ZoneEquipConfig(found).ExhaustNode)) {
                        state.dataRoomAirMod->AirPatternZoneInfo(i).ExhaustAirNodeID.allocate(
                            state.dataZoneEquip->ZoneEquipConfig(found).NumExhaustNodes);
                        state.dataRoomAirMod->AirPatternZoneInfo(i).ExhaustAirNodeID = state.dataZoneEquip->ZoneEquipConfig(found).ExhaustNode;
                    } // exhaust nodes present
                }     // found ZoneEquipConf

                // second get zone height values
                state.dataRoomAirMod->AirPatternZoneInfo(i).ZoneHeight = state.dataHeatBal->Zone(i).CeilingHeight;

            } // air pattern is used
        }
    }

    void GetAirNodeData(EnergyPlusData &state, bool &ErrorsFound) // True if errors found during this get input routine
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Brent Griffith
        //       DATE WRITTEN   August 2001
        //       RE-ENGINEERED  April 2003, Weixiu Kong
        //       MODIFIED       July 2003, CC
        //                      Jan 2004, CC

        // PURPOSE OF THIS SUBROUTINE:
        //     Get AirNode data for all zones at once

        constexpr std::string_view routineName = "GetAirNodeData";
        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int NumAlphas; // States which alpha value to read from a
        // "Number" line
        int NumNumbers;       // Number of numbers encountered
        int Status;           // Notes if there was an error in processing the input

        if (!state.dataRoomAirMod->DispVent1NodeModelUsed) return;

        auto &ipsc = state.dataIPShortCut;
        
        // Initialize default values for air nodes
        state.dataRoomAirMod->TotNumOfZoneAirNodes.allocate(state.dataGlobal->NumOfZones);
        state.dataRoomAirMod->TotNumOfAirNodes = 0;
        state.dataRoomAirMod->TotNumOfZoneAirNodes = 0;
        ipsc->cCurrentModuleObject = "RoomAir:Node";
        state.dataRoomAirMod->TotNumOfAirNodes = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, ipsc->cCurrentModuleObject);

        if (state.dataRoomAirMod->TotNumOfAirNodes <= 0) {
            // no air node object is found, terminate the program
            ShowSevereError(state, format("No {} objects found in input.", ipsc->cCurrentModuleObject));
            ShowContinueError(state, format("The OneNodeDisplacementVentilation model requires {} objects", ipsc->cCurrentModuleObject));
            ErrorsFound = true;
            return;
        } else {
            // air node objects are found so allocate airnode variable
            state.dataRoomAirMod->AirNode.allocate(state.dataRoomAirMod->TotNumOfAirNodes);
        }

        for (int AirNodeNum = 1; AirNodeNum <= state.dataRoomAirMod->TotNumOfAirNodes; ++AirNodeNum) {

            // get air node objects
            state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                     ipsc->cCurrentModuleObject,
                                                                     AirNodeNum,
                                                                     ipsc->cAlphaArgs,
                                                                     NumAlphas,
                                                                     ipsc->rNumericArgs,
                                                                     NumNumbers,
                                                                     Status,
                                                                     _,
                                                                     _,
                                                                     ipsc->cAlphaFieldNames,
                                                                     ipsc->cNumericFieldNames);

            auto &airNode = state.dataRoomAirMod->AirNode(AirNodeNum);
            airNode.Name = ipsc->cAlphaArgs(1);

            ErrorObjectHeader eoh{routineName, ipsc->cCurrentModuleObject, airNode.Name};

            airNode.ZoneName = ipsc->cAlphaArgs(3); // Zone name
            airNode.ZonePtr = UtilityRoutines::FindItemInList(airNode.ZoneName, state.dataHeatBal->Zone);
            if (airNode.ZonePtr == 0) {
                ShowSevereItemNotFound(state, eoh, ipsc->cAlphaFieldNames(3), ipsc->cAlphaArgs(3));
                ErrorsFound = true;
            } else {
                int NumOfSurfs = 0;
                for (int spaceNum : state.dataHeatBal->Zone(airNode.ZonePtr).spaceIndexes) {
                    auto &thisSpace = state.dataHeatBal->space(spaceNum);
                    NumOfSurfs += thisSpace.HTSurfaceLast - thisSpace.HTSurfaceFirst + 1;
                }
                airNode.SurfMask.allocate(NumOfSurfs);
            }

            airNode.ClassType = static_cast<AirNodeType>(getEnumValue(airNodeTypeNamesUC, UtilityRoutines::makeUPPER(ipsc->cAlphaArgs(2))));
            if (airNode.ClassType == AirNodeType::Invalid) {
                ShowSevereInvalidKey(state, eoh, ipsc->cAlphaFieldNames(2), ipsc->cAlphaArgs(2));
                ErrorsFound = true;
            }

            airNode.Height = ipsc->rNumericArgs(1); // Air node height
            int NumSurfsInvolved = NumAlphas - 3;                                                         // Number of surfaces involved with air nodes

            // Initialize
            airNode.SurfMask = false;

            if (NumSurfsInvolved <= 0) {

                // report severe error since the following air nodes require surfaces associated with them
                if (airNode.ClassType == AirNodeType::Floor || airNode.ClassType == AirNodeType::Ceiling ||
                    airNode.ClassType == AirNodeType::Mundt || airNode.ClassType == AirNodeType::Plume ||
                    airNode.ClassType == AirNodeType::Rees) { // Are there really Rees 1-4?
                    // terminate the program due to a severe error in the specified input
                    ShowSevereError(state, format("GetAirNodeData: {}=\"{}\" invalid air node specification.", ipsc->cCurrentModuleObject, airNode.Name));
                    ShowContinueError(state, format("Mundt Room Air Model: No surface names specified.  Air node=\"{} requires surfaces associated with it.",
                                                    airNode.Name));
                    ErrorsFound = true;
                } 
                continue;
            }

            // report warning error since the following air nodes do not require surfaces associated with them
            // and assign .FALSE. to 'SurfNeeded'
            if (airNode.ClassType == AirNodeType::Inlet || airNode.ClassType == AirNodeType::Control ||
                airNode.ClassType == AirNodeType::Return || airNode.ClassType == AirNodeType::Plume) {
                ShowWarningError(state, format("GetAirNodeData: {}=\"{}\" invalid linkage", ipsc->cCurrentModuleObject, airNode.Name));
                ShowContinueError(state, format("Mundt Room Air Model: No surface names needed.  Air node=\"{} does not relate to any surfaces.",
                                                airNode.Name));
                continue;
            } 

            // this air node is in this zone; hence, first get name of all surfaces in this zone
            auto const &zone = state.dataHeatBal->Zone(airNode.ZonePtr);
            int NumOfSurfs = 0;
            for (int spaceNum : zone.spaceIndexes) {
                auto &thisSpace = state.dataHeatBal->space(spaceNum);
                NumOfSurfs += thisSpace.HTSurfaceLast - thisSpace.HTSurfaceFirst + 1;
            }

            // terminate the program due to a severe error in the specified input
            if (NumSurfsInvolved > NumOfSurfs) {
                ShowFatalError(state,
                               format("GetAirNodeData: Mundt Room Air Model: Number of surfaces connected to {} is greater than number of surfaces in {}",
                                      airNode.Name,
                                      zone.Name));
                return;
            }

            // relate surfaces to this air node and check to see whether surface names are specified correctly or not
            int SurfCount = 0;
            for (int ListSurfNum = 4; ListSurfNum <= NumAlphas; ++ListSurfNum) {
                int thisSurfinZone = 0;
                for (int spaceNum : zone.spaceIndexes) {
                    auto &thisSpace = state.dataHeatBal->space(spaceNum);
                    for (int SurfNum = thisSpace.HTSurfaceFirst; SurfNum <= thisSpace.HTSurfaceLast; ++SurfNum) {
                        ++thisSurfinZone;
                        if (ipsc->cAlphaArgs(ListSurfNum) == state.dataSurface->Surface(SurfNum).Name) {
                            airNode.SurfMask(thisSurfinZone) = true;
                            ++SurfCount;
                            break;
                        }
                    }
                    if (SurfCount > 0) break;
                }
            }

            // report warning error since surface names are specified correctly
            if ((NumSurfsInvolved) != SurfCount) {
                ShowWarningError(state,
                                 format("GetAirNodeData: Mundt Room Air Model: Some surface names specified for {} are not in {}",
                                        airNode.Name,
                                        zone.Name));
            }
        } // for (AirNodeNum)

        // get number of air nodes in each zone
        for (int AirNodeNum = 1; AirNodeNum <= state.dataRoomAirMod->TotNumOfAirNodes; ++AirNodeNum) {
            auto const &airNode = state.dataRoomAirMod->AirNode(AirNodeNum);
            // this zone uses other air model so skip the rest
            if (state.dataRoomAirMod->AirModel(airNode.ZonePtr).AirModel == RoomAirModel::DispVent1Node) 
                ++state.dataRoomAirMod->TotNumOfZoneAirNodes(airNode.ZonePtr);
        }
    }

    //*****************************************************************************************

    void GetMundtData(EnergyPlusData &state, bool &ErrorsFound) // True if errors found during this get input routine
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Brent Griffith
        //       DATE WRITTEN   August 2001
        //       MODIFIED       na
        //       RE-ENGINEERED  April 2003, Weixiu Kong
        //                      July 2003, CC

        // PURPOSE OF THIS SUBROUTINE:
        //     Get Mundt model controls for all zones at once

        // METHODOLOGY EMPLOYED:
        //     Use input processer to get input from idf file

        // Using/Aliasing
        constexpr std::string_view routineName = "GetMundtData";
        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int NumAlphas;
        int NumNumbers;       // Number of numbers encountered
        int Status;           // Notes if there was an error in processing the input
        int NumOfMundtContrl; // Number of Mundt Model Controls

        auto &ipsc = state.dataIPShortCut;
        
        if (!state.dataRoomAirMod->DispVent1NodeModelUsed) return;

        // Initialize default values for Mundt model controls
        state.dataRoomAirMod->ConvectiveFloorSplit.allocate(state.dataGlobal->NumOfZones);
        state.dataRoomAirMod->InfiltratFloorSplit.allocate(state.dataGlobal->NumOfZones);
        state.dataRoomAirMod->ConvectiveFloorSplit = 0.0;
        state.dataRoomAirMod->InfiltratFloorSplit = 0.0;
        auto &cCurrentModuleObject = ipsc->cCurrentModuleObject;
        cCurrentModuleObject = "RoomAirSettings:OneNodeDisplacementVentilation";
        NumOfMundtContrl = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, cCurrentModuleObject);
        if (NumOfMundtContrl > state.dataGlobal->NumOfZones) {
            ShowSevereError(state, format("Too many {} objects in input file", cCurrentModuleObject));
            ShowContinueError(state, format("There cannot be more {} objects than number of zones.", cCurrentModuleObject));
            ErrorsFound = true;
        }

        if (NumOfMundtContrl == 0) {
            ShowWarningError(state,
                             format("No {} objects found, program assumes no convection or infiltration gains near floors", cCurrentModuleObject));
            return;
        }

        // this zone uses Mundt model so get Mundt Model Control
        // loop through all 'RoomAirSettings:OneNodeDisplacementVentilation' objects
        for (int ControlNum = 1; ControlNum <= NumOfMundtContrl; ++ControlNum) {
            state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                     ipsc->cCurrentModuleObject,
                                                                     ControlNum,
                                                                     ipsc->cAlphaArgs,
                                                                     NumAlphas,
                                                                     ipsc->rNumericArgs,
                                                                     NumNumbers,
                                                                     Status,
                                                                     _,
                                                                     _,
                                                                     ipsc->cAlphaFieldNames,
                                                                     ipsc->cNumericFieldNames);


            ErrorObjectHeader eoh{routineName, ipsc->cCurrentModuleObject, ""};
            int ZoneNum = UtilityRoutines::FindItemInList(ipsc->cAlphaArgs(1), state.dataHeatBal->Zone);
            if (ZoneNum == 0) {
                ShowSevereItemNotFound(state, eoh, ipsc->cAlphaFieldNames(1), ipsc->cAlphaArgs(1));
                ErrorsFound = true;
                continue;
            }
            if (state.dataRoomAirMod->AirModel(ZoneNum).AirModel != RoomAirModel::DispVent1Node) {
                ShowSevereError(
                    state,
                    format("Zone specified=\"{}\", Air Model type is not OneNodeDisplacementVentilation.", ipsc->cAlphaArgs(1)));
                ShowContinueError(
                                  state, format("Air Model Type for zone={}", roomAirModelNamesUC[(int)state.dataRoomAirMod->AirModel(ZoneNum).AirModel]));
                ErrorsFound = true;
                continue;
            }
            state.dataRoomAirMod->ConvectiveFloorSplit(ZoneNum) = ipsc->rNumericArgs(1);
            state.dataRoomAirMod->InfiltratFloorSplit(ZoneNum) = ipsc->rNumericArgs(2);
        }
    }

    void GetDisplacementVentData(EnergyPlusData &state, bool &ErrorsFound) // True if errors found during this get input routine
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         G. Carrilho da Graca
        //       DATE WRITTEN   January 2004

        // PURPOSE OF THIS SUBROUTINE:
        //  Get UCSD Displacement ventilation model controls for all zones at once

        // METHODOLOGY EMPLOYED:
        // Use input processor to get input from idf file

        // Using/Aliasing
        using namespace ScheduleManager;

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        constexpr std::string_view routineName = "GetDisplacementVentData";
        int IOStat;
        int NumAlpha;
        int NumNumber;

        auto &ipsc = state.dataIPShortCut;

        if (!state.dataRoomAirMod->UCSDModelUsed) return;
        ipsc->cCurrentModuleObject = "RoomAirSettings:ThreeNodeDisplacementVentilation";
        state.dataRoomAirMod->TotDispVent3Node = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, ipsc->cCurrentModuleObject);

        if (state.dataRoomAirMod->TotDispVent3Node <= 0) return;

        state.dataRoomAirMod->ZoneDispVent3Node.allocate(state.dataRoomAirMod->TotDispVent3Node);

        for (int Loop = 1; Loop <= state.dataRoomAirMod->TotDispVent3Node; ++Loop) {

            state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                     ipsc->cCurrentModuleObject,
                                                                     Loop,
                                                                     ipsc->cAlphaArgs,
                                                                     NumAlpha,
                                                                     ipsc->rNumericArgs,
                                                                     NumNumber,
                                                                     IOStat,
                                                                     _,
                                                                     ipsc->lAlphaFieldBlanks,
                                                                     ipsc->cAlphaFieldNames,
                                                                     ipsc->cNumericFieldNames);

            auto &zoneDV3N = state.dataRoomAirMod->ZoneDispVent3Node(Loop);
            ErrorObjectHeader eoh{routineName, ipsc->cCurrentModuleObject, ipsc->cAlphaArgs(1)};
            // First is Zone Name
            // zoneUCSDDV.ZoneName = ipsc->cAlphaArgs(1);
            zoneDV3N.ZonePtr = UtilityRoutines::FindItemInList(ipsc->cAlphaArgs(1), state.dataHeatBal->Zone);
            if (zoneDV3N.ZonePtr == 0) {
                ShowSevereItemNotFound(state, eoh, ipsc->cAlphaFieldNames(1), ipsc->cAlphaArgs(1));
                ErrorsFound = true;
            } else {
                state.dataRoomAirMod->IsZoneDispVent3Node(zoneDV3N.ZonePtr) = true;
            }
            // Second Alpha is Schedule Name
            // zoneUCSDDV.SchedGainsName = ipsc->cAlphaArgs(2);
            if (ipsc->lAlphaFieldBlanks(2)) {
                ShowSevereEmptyField(state, eoh, ipsc->cAlphaFieldNames(2));
                ErrorsFound = true;
            } else if ((zoneDV3N.SchedGainsPtr = GetScheduleIndex(state, ipsc->cAlphaArgs(2))) == 0) {
                ShowSevereItemNotFound(state, eoh, ipsc->cAlphaFieldNames(2), ipsc->cAlphaArgs(2));
                ErrorsFound = true;
            } 

            zoneDV3N.NumPlumesPerOcc = ipsc->rNumericArgs(1);
            zoneDV3N.ThermostatHeight = ipsc->rNumericArgs(2);
            zoneDV3N.ComfortHeight = ipsc->rNumericArgs(3);
            zoneDV3N.TempTrigger = ipsc->rNumericArgs(4);
        }
    }

    void GetCrossVentData(EnergyPlusData &state, bool &ErrorsFound) // True if errors found during this get input routine
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         G. Carrilho da Graca
        //       DATE WRITTEN   October 2004

        // PURPOSE OF THIS SUBROUTINE:
        //  Get UCSD Cross ventilation model controls for all zones at once

        // METHODOLOGY EMPLOYED:
        // Use input processor to get input from idf file

        // Using/Aliasing
        using namespace ScheduleManager;

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        constexpr std::string_view routineName = "GetCrossVentData";
        
        int IOStat;
        int NumAlpha;
        int NumNumber;

        auto &ipsc = state.dataIPShortCut;
        if (!state.dataRoomAirMod->UCSDModelUsed) return;
        ipsc->cCurrentModuleObject = "RoomAirSettings:CrossVentilation";
        state.dataRoomAirMod->TotCrossVent = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, ipsc->cCurrentModuleObject);

        if (state.dataRoomAirMod->TotCrossVent <= 0) return;

        state.dataRoomAirMod->ZoneCrossVent.allocate(state.dataRoomAirMod->TotCrossVent);

        for (int Loop = 1; Loop <= state.dataRoomAirMod->TotCrossVent; ++Loop) {

            state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                     ipsc->cCurrentModuleObject,
                                                                     Loop,
                                                                     ipsc->cAlphaArgs,
                                                                     NumAlpha,
                                                                     ipsc->rNumericArgs,
                                                                     NumNumber,
                                                                     IOStat,
                                                                     _,
                                                                     ipsc->lAlphaFieldBlanks,
                                                                     ipsc->cAlphaFieldNames,
                                                                     ipsc->cNumericFieldNames);

            auto &zoneCV = state.dataRoomAirMod->ZoneCrossVent(Loop);
            ErrorObjectHeader eoh{routineName, ipsc->cCurrentModuleObject, ipsc->cAlphaArgs(1)};
            // First is Zone Name
            // state.dataRoomAirMod->ZoneUCSDCV(Loop).ZoneName = ipsc->cAlphaArgs(1);
            if ((zoneCV.ZonePtr = UtilityRoutines::FindItemInList(ipsc->cAlphaArgs(1), state.dataHeatBal->Zone)) == 0) {
                ShowSevereItemNotFound(state, eoh, ipsc->cAlphaFieldNames(1), ipsc->cAlphaArgs(1));
                ErrorsFound = true;
            } else {
                state.dataRoomAirMod->IsZoneCrossVent(zoneCV.ZonePtr) = true;
            }
            // Second Alpha is Schedule Name
            // zoneUCSDCV.SchedGainsName = ipsc->cAlphaArgs(2);
            if (ipsc->lAlphaFieldBlanks(2)) {
                ShowSevereEmptyField(state, eoh, ipsc->cAlphaFieldNames(2));
                ErrorsFound = true;
            } else if ((zoneCV.SchedGainsPtr = GetScheduleIndex(state, ipsc->cAlphaArgs(2))) == 0) {
                ShowSevereItemNotFound(state, eoh, ipsc->cAlphaFieldNames(2), ipsc->cAlphaArgs(2));
                ErrorsFound = true;
            }

            // Third Alpha is a string: JET or RECIRCULATION
            if (ipsc->lAlphaFieldBlanks(3)) {
                for (int Loop2 = 1; Loop2 <= state.dataHeatBal->TotPeople; ++Loop2) {
                     if (state.dataHeatBal->People(Loop2).ZonePtr != zoneCV.ZonePtr) continue;
                     if (!state.dataHeatBal->People(Loop2).Fanger) continue;
                     ShowSevereEmptyField(state, eoh, ipsc->cAlphaFieldNames(3));
                     ErrorsFound = true;
                }
            } else if ((zoneCV.VforComfort = static_cast<Comfort>(getEnumValue(comfortNamesUC, UtilityRoutines::makeUPPER(ipsc->cAlphaArgs(3))))) ==
                       Comfort::Invalid) {
                ShowSevereInvalidKey(state, eoh, ipsc->cAlphaFieldNames(3), ipsc->cAlphaArgs(3));
                ErrorsFound = true;
            }

            if (zoneCV.ZonePtr == 0) continue;

            // Following depend on valid zone

            if (UtilityRoutines::FindItemInList(state.dataHeatBal->Zone(zoneCV.ZonePtr).Name,
                                                state.afn->MultizoneZoneData,
                                                &AirflowNetwork::MultizoneZoneProp::ZoneName) == 0) {
                ShowSevereError(state, format("Problem with {} = {}", ipsc->cCurrentModuleObject, ipsc->cAlphaArgs(1)));
                ShowContinueError(state, "AirflowNetwork airflow model must be active in this zone");
                ErrorsFound = true;
            }

            // If a crack is used it must have an air flow coefficient = 0.5
            for (int iLink = 1; iLink <= state.afn->NumOfLinksMultiZone; ++iLink) {
                auto const &mzSurfaceData = state.afn->MultizoneSurfaceData(iLink);
                int nodeNum1 = mzSurfaceData.NodeNums[0];
                int nodeNum2 = mzSurfaceData.NodeNums[1];
                if (state.dataSurface->Surface(mzSurfaceData.SurfNum).Zone == zoneCV.ZonePtr ||
                    (state.afn->AirflowNetworkNodeData(nodeNum2).EPlusZoneNum == zoneCV.ZonePtr &&
                     state.afn->AirflowNetworkNodeData(nodeNum1).EPlusZoneNum > 0) ||
                    (state.afn->AirflowNetworkNodeData(nodeNum2).EPlusZoneNum > 0 &&
                     state.afn->AirflowNetworkNodeData(nodeNum1).EPlusZoneNum == zoneCV.ZonePtr)) {
                    int compNum = state.afn->AirflowNetworkLinkageData(iLink).CompNum;
                    int typeNum = state.afn->AirflowNetworkCompData(compNum).TypeNum;
                    if (state.afn->AirflowNetworkCompData(compNum).CompTypeNum == AirflowNetwork::iComponentTypeNum::SCR) {
                        if (state.afn->MultizoneSurfaceCrackData(typeNum).exponent != 0.50) {
                            state.dataRoomAirMod->AirModel(zoneCV.ZonePtr).AirModel = RoomAirModel::Mixing;
                            ShowWarningError(state, format("Problem with {} = {}", ipsc->cCurrentModuleObject, ipsc->cAlphaArgs(1)));
                            ShowWarningError(state, format("Roomair model will not be applied for Zone={}.", ipsc->cAlphaArgs(1)));
                            ShowContinueError(
                                state,
                                format("AirflowNetwrok:Multizone:Surface crack object must have an air flow coefficient = 0.5, value was={:.2R}",
                                       state.afn->MultizoneSurfaceCrackData(typeNum).exponent));
                        }
                    }
                } // if 
            } // for (iLink)
        } // for (Loop)
    }

    void GetUFADZoneData(EnergyPlusData &state, bool &ErrorsFound) // True if errors found during this get input routine
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Fred Buhl
        //       DATE WRITTEN   August 2005

        // PURPOSE OF THIS SUBROUTINE:
        //  Get UCSD UFAD interior zone model controls for all zones at once

        // METHODOLOGY EMPLOYED:
        // Use input processor to get input from idf file

        // Using/Aliasing
        using namespace ScheduleManager;

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        constexpr std::string_view routineName = "GetUFADZoneData";
        
        int IOStat;
        int NumAlpha;
        int NumNumber;

        if (!state.dataRoomAirMod->UCSDModelUsed) {
            state.dataRoomAirMod->TotUFADInt = 0;
            state.dataRoomAirMod->TotUFADExt = 0;
            return;
        }

        auto &ipsc = state.dataIPShortCut;
        
        ipsc->cCurrentModuleObject = "RoomAirSettings:UnderFloorAirDistributionInterior";
        state.dataRoomAirMod->TotUFADInt = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, ipsc->cCurrentModuleObject);
        ipsc->cCurrentModuleObject = "RoomAirSettings:UnderFloorAirDistributionExterior";
        state.dataRoomAirMod->TotUFADExt = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, ipsc->cCurrentModuleObject);

        if (state.dataRoomAirMod->TotUFADInt <= 0 && state.dataRoomAirMod->TotUFADExt <= 0) return;

        state.dataRoomAirMod->ZoneUFADInt.allocate(state.dataRoomAirMod->TotUFADInt);
        state.dataRoomAirMod->ZoneUFADExt.allocate(state.dataRoomAirMod->TotUFADExt);
        state.dataRoomAirMod->ZoneUFADPtr.dimension(state.dataGlobal->NumOfZones, 0);

        ipsc->cCurrentModuleObject = "RoomAirSettings:UnderFloorAirDistributionInterior";
        for (int Loop = 1; Loop <= state.dataRoomAirMod->TotUFADInt; ++Loop) {
            state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                     ipsc->cCurrentModuleObject,
                                                                     Loop,
                                                                     ipsc->cAlphaArgs,
                                                                     NumAlpha,
                                                                     ipsc->rNumericArgs,
                                                                     NumNumber,
                                                                     IOStat,
                                                                     _,
                                                                     ipsc->lAlphaFieldBlanks,
                                                                     ipsc->cAlphaFieldNames,
                                                                     ipsc->cNumericFieldNames);
            // First is Zone Name
            auto &zoneUI = state.dataRoomAirMod->ZoneUFADInt(Loop);
            ErrorObjectHeader eoh{routineName, ipsc->cCurrentModuleObject, ipsc->cAlphaArgs(1)};
            
            zoneUI.ZoneName = ipsc->cAlphaArgs(1);
            zoneUI.ZonePtr = UtilityRoutines::FindItemInList(ipsc->cAlphaArgs(1), state.dataHeatBal->Zone);
            state.dataRoomAirMod->ZoneUFADPtr(zoneUI.ZonePtr) = Loop;
            if (zoneUI.ZonePtr == 0) {
                ShowSevereItemNotFound(state, eoh, ipsc->cAlphaFieldNames(1), ipsc->cAlphaArgs(1));
                ErrorsFound = true;
            } else {
                state.dataRoomAirMod->IsZoneUI(zoneUI.ZonePtr) = true;
            }

            // 2nd alpha is diffuser type
            zoneUI.DiffuserType = static_cast<Diffuser>(getEnumValue(diffuserNamesUC, UtilityRoutines::makeUPPER(ipsc->cAlphaArgs(2))));
            if (zoneUI.DiffuserType == Diffuser::Invalid) {
                ShowSevereInvalidKey(state, eoh, ipsc->cAlphaFieldNames(2), ipsc->cAlphaArgs(2));
                ErrorsFound = true;
            }
            // 1st number is Number of Diffusers per Zone
            zoneUI.DiffusersPerZone = ipsc->rNumericArgs(1);
            // 2nd number is Power per Plume
            zoneUI.PowerPerPlume = ipsc->rNumericArgs(2);
            // 3rd number is Design Effective Area of Diffuser
            zoneUI.DiffArea = ipsc->rNumericArgs(3);
            // 4th number is Diffuser Slot Angle from Vertical
            zoneUI.DiffAngle = ipsc->rNumericArgs(4);
            // 5th number is Thermostat Height
            zoneUI.ThermostatHeight = ipsc->rNumericArgs(5);
            // 6th number is Comfort Height
            zoneUI.ComfortHeight = ipsc->rNumericArgs(6);
            // 7th number is Temperature Difference Threshold for Reporting
            zoneUI.TempTrigger = ipsc->rNumericArgs(7);
            // 8th number user-specified transition height
            zoneUI.TransHeight = ipsc->rNumericArgs(8);
            // 9th number is Coefficient A in formula Kc = A*Gamma**B + C + D*Gamma + E*Gamma**2
            zoneUI.A_Kc = ipsc->rNumericArgs(9);
            // 10th number is Coefficient B in formula Kc = A*Gamma**B + C + D*Gamma + E*Gamma**2
            zoneUI.B_Kc = ipsc->rNumericArgs(10);
            // 11th number is Coefficient C in formula Kc = A*Gamma**B + C + D*Gamma + E*Gamma**2
            zoneUI.C_Kc = ipsc->rNumericArgs(11);
            // 12th number is Coefficient D in formula Kc = A*Gamma**B + C + D*Gamma + E*Gamma**2
            zoneUI.D_Kc = ipsc->rNumericArgs(12);
            // 13th number is Coefficient E in formula Kc = A*Gamma**B + C + D*Gamma + E*Gamma**2
            zoneUI.E_Kc = ipsc->rNumericArgs(13);
        }

        ipsc->cCurrentModuleObject = "RoomAirSettings:UnderFloorAirDistributionExterior";
        for (int Loop = 1; Loop <= state.dataRoomAirMod->TotUFADExt; ++Loop) {
            state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                     ipsc->cCurrentModuleObject,
                                                                     Loop,
                                                                     ipsc->cAlphaArgs,
                                                                     NumAlpha,
                                                                     ipsc->rNumericArgs,
                                                                     NumNumber,
                                                                     IOStat,
                                                                     _,
                                                                     ipsc->lAlphaFieldBlanks,
                                                                     ipsc->cAlphaFieldNames,
                                                                     ipsc->cNumericFieldNames);
            // First is Zone Name
            auto &zoneUE = state.dataRoomAirMod->ZoneUFADExt(Loop);
            ErrorObjectHeader eoh{routineName, ipsc->cCurrentModuleObject, ipsc->cAlphaArgs(1)};
            zoneUE.ZoneName = ipsc->cAlphaArgs(1);
            zoneUE.ZonePtr = UtilityRoutines::FindItemInList(ipsc->cAlphaArgs(1), state.dataHeatBal->Zone);
            if (zoneUE.ZonePtr == 0) {
                ShowSevereItemNotFound(state, eoh, ipsc->cAlphaFieldNames(1), ipsc->cAlphaArgs(1));
                ErrorsFound = true;
            } else {
                state.dataRoomAirMod->IsZoneUI(zoneUE.ZonePtr) = true;
                state.dataRoomAirMod->ZoneUFADPtr(zoneUE.ZonePtr) = Loop;
            }

            zoneUE.DiffuserType = static_cast<Diffuser>(getEnumValue(diffuserNamesUC, UtilityRoutines::makeUPPER(ipsc->cAlphaArgs(2))));
            if (zoneUE.DiffuserType == Diffuser::Invalid) {
                ShowSevereInvalidKey(state, eoh, ipsc->cAlphaFieldNames(2), ipsc->cAlphaArgs(2));
                ErrorsFound = true;
            }
            // 1st number is Number of Diffusers per Zone
            zoneUE.DiffusersPerZone = ipsc->rNumericArgs(1);
            // 2nd number is Power per Plume
            zoneUE.PowerPerPlume = ipsc->rNumericArgs(2);
            // 3rd number is Design Effective Area of Diffuser
            zoneUE.DiffArea = ipsc->rNumericArgs(3);
            // 4th number is Diffuser Slot Angle from Vertical
            zoneUE.DiffAngle = ipsc->rNumericArgs(4);
            // 5th number is Thermostat Height
            zoneUE.ThermostatHeight = ipsc->rNumericArgs(5);
            // 6th number is Comfort Height
            zoneUE.ComfortHeight = ipsc->rNumericArgs(6);
            // 7th number is Temperature Difference Threshold for Reporting
            zoneUE.TempTrigger = ipsc->rNumericArgs(7);
            // 8th number user-specified transition height
            zoneUE.TransHeight = ipsc->rNumericArgs(8);
            // 9th number is Coefficient A in formula Kc = A*Gamma**B + C + D*Gamma + E*Gamma**2
            zoneUE.A_Kc = ipsc->rNumericArgs(9);
            // 10th number is Coefficient B in formula Kc = A*Gamma**B + C + D*Gamma + E*Gamma**2
            zoneUE.B_Kc = ipsc->rNumericArgs(10);
            // 11th number is Coefficient C in formula Kc = A*Gamma**B + C + D*Gamma + E*Gamma**2
            zoneUE.C_Kc = ipsc->rNumericArgs(11);
            // 12th number is Coefficient D in formula Kc = A*Gamma**B + C + D*Gamma + E*Gamma**2
            zoneUE.D_Kc = ipsc->rNumericArgs(12);
            // 13th number is Coefficient E in formula Kc = A*Gamma**B + C + D*Gamma + E*Gamma**2
            zoneUE.E_Kc = ipsc->rNumericArgs(13);
        }
    }

    void GetRoomAirflowNetworkData(EnergyPlusData &state, bool &ErrorsFound) // True if errors found during this get input routine
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Brent Griffith
        //       DATE WRITTEN   November 2009

        // PURPOSE OF THIS SUBROUTINE:
        //  Get RoomAirflowNetwork data for all zones at once

        // METHODOLOGY EMPLOYED:
        // Use input processor to get input from idf file

        // Using/Aliasing
        using InternalHeatGains::GetInternalGainDeviceIndex;
        using ScheduleManager::GetScheduleIndex;

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        constexpr std::string_view routineName = "GetRoomAirflowNetworkData";
        int NumAlphas;
        int NumNumbers;
        int status;
        int TotNumOfRAFNNodeSurfLists;
        int TotNumOfRAFNNodeGainsLists;
        int TotNumOfRAFNNodeHVACLists;
        int TotNumEquip;
        bool IntEquipError;

        auto &ipsc = state.dataIPShortCut;
        ipsc->cCurrentModuleObject = "RoomAirSettings:AirflowNetwork";
        state.dataRoomAirMod->NumOfRoomAirflowNetControl = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, ipsc->cCurrentModuleObject);
        if (state.dataRoomAirMod->NumOfRoomAirflowNetControl == 0) return;
        if (state.dataRoomAirMod->NumOfRoomAirflowNetControl > state.dataGlobal->NumOfZones) {
            ShowSevereError(state, format("Too many {} objects in input file", ipsc->cCurrentModuleObject));
            ShowContinueError(state, format("There cannot be more {} objects than number of zones.", ipsc->cCurrentModuleObject));
            ErrorsFound = true;
        }

        if (!allocated(state.dataRoomAirMod->AFNZoneInfo)) {
            state.dataRoomAirMod->AFNZoneInfo.allocate(state.dataGlobal->NumOfZones);
        }

        for (int Loop = 1; Loop <= state.dataRoomAirMod->NumOfRoomAirflowNetControl; ++Loop) {
            state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                     ipsc->cCurrentModuleObject,
                                                                     Loop,
                                                                     ipsc->cAlphaArgs,
                                                                     NumAlphas,
                                                                     ipsc->rNumericArgs,
                                                                     NumNumbers,
                                                                     status,
                                                                     _,
                                                                     ipsc->lAlphaFieldBlanks,
                                                                     ipsc->cAlphaFieldNames,
                                                                     ipsc->cNumericFieldNames);


            ErrorObjectHeader eoh{routineName, ipsc->cCurrentModuleObject, ipsc->cAlphaArgs(1)};
            int ZoneNum = UtilityRoutines::FindItemInList(ipsc->cAlphaArgs(2), state.dataHeatBal->Zone, state.dataGlobal->NumOfZones);
            if (ZoneNum == 0) {
                ShowSevereItemNotFound(state, eoh, ipsc->cAlphaFieldNames(2), ipsc->cAlphaArgs(2));
                ErrorsFound = true;
                continue;
            }
            if (state.dataRoomAirMod->AirModel(ZoneNum).AirModel != RoomAirModel::AirflowNetwork) {
                ShowSevereError(state,
                                format("GetRoomAirflowNetworkData: Zone specified='{}', Air Model type is not AirflowNetwork.",
                                       ipsc->cAlphaArgs(1)));
                ShowContinueError(
                                  state, format("Air Model Type for zone ={}", roomAirModelNamesUC[(int)state.dataRoomAirMod->AirModel(ZoneNum).AirModel]));
                ErrorsFound = true;
                continue;
            }

            auto &roomAFNZoneInfo = state.dataRoomAirMod->AFNZoneInfo(ZoneNum);
            roomAFNZoneInfo.ZoneID = ZoneNum;
            roomAFNZoneInfo.RAFNNum = Loop;
            roomAFNZoneInfo.IsUsed = true;
            roomAFNZoneInfo.Name = ipsc->cAlphaArgs(1);
            roomAFNZoneInfo.ZoneName = ipsc->cAlphaArgs(2); // Zone Name

            roomAFNZoneInfo.NumOfAirNodes = (NumAlphas - 3);

            if (roomAFNZoneInfo.NumOfAirNodes > 0) {
                roomAFNZoneInfo.Node.allocate(roomAFNZoneInfo.NumOfAirNodes);
            } else {
                ShowSevereError(
                    state,
                    format("GetRoomAirflowNetworkData: Incomplete input in {} = {}", ipsc->cCurrentModuleObject, ipsc->cAlphaArgs(1)));
                ErrorsFound = true;
            }

            for (int iAirNode = 1; iAirNode <= roomAFNZoneInfo.NumOfAirNodes; ++iAirNode) {
                roomAFNZoneInfo.Node(iAirNode).Name = ipsc->cAlphaArgs(iAirNode+3);
            }
            // control point node

            roomAFNZoneInfo.ControlAirNodeID = UtilityRoutines::FindItemInList(ipsc->cAlphaArgs(3), roomAFNZoneInfo.Node, roomAFNZoneInfo.NumOfAirNodes);
            if (roomAFNZoneInfo.ControlAirNodeID == 0) {
                ShowSevereItemNotFound(state, eoh, ipsc->cAlphaFieldNames(3), ipsc->cAlphaArgs(3));
                ErrorsFound = true;
                continue;
            }
            
            roomAFNZoneInfo.totNumSurfs = 0;
            for (int spaceNum : state.dataHeatBal->Zone(ZoneNum).spaceIndexes) {
                auto &thisSpace = state.dataHeatBal->space(spaceNum);
                roomAFNZoneInfo.totNumSurfs += thisSpace.HTSurfaceLast - thisSpace.HTSurfaceFirst + 1;
            }
        } // for (Loop)
        
        ipsc->cCurrentModuleObject = "RoomAir:Node:AirflowNetwork";
        state.dataRoomAirMod->TotNumOfRoomAFNNodes = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, ipsc->cCurrentModuleObject);
        for (int Loop = 1; Loop <= state.dataRoomAirMod->TotNumOfRoomAFNNodes; ++Loop) {
            state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                     ipsc->cCurrentModuleObject,
                                                                     Loop,
                                                                     ipsc->cAlphaArgs,
                                                                     NumAlphas,
                                                                     ipsc->rNumericArgs,
                                                                     NumNumbers,
                                                                     status,
                                                                     _,
                                                                     ipsc->lAlphaFieldBlanks,
                                                                     ipsc->cAlphaFieldNames,
                                                                     ipsc->cNumericFieldNames);

            ErrorObjectHeader eoh{routineName, ipsc->cCurrentModuleObject, ipsc->cAlphaArgs(1)};
            int ZoneNum = UtilityRoutines::FindItemInList(ipsc->cAlphaArgs(2), state.dataHeatBal->Zone, state.dataGlobal->NumOfZones);
            if (ZoneNum == 0) {
                ShowSevereItemNotFound(state, eoh, ipsc->cAlphaFieldNames(2), ipsc->cAlphaArgs(2));
                ErrorsFound = true;
                continue;
            }

            auto &roomAFNZoneInfo = state.dataRoomAirMod->AFNZoneInfo(ZoneNum);
            int RAFNNodeNum = UtilityRoutines::FindItemInList(ipsc->cAlphaArgs(1), roomAFNZoneInfo.Node, roomAFNZoneInfo.NumOfAirNodes);
            if (RAFNNodeNum == 0) {
                ShowSevereItemNotFound(state, eoh, ipsc->cAlphaFieldNames(1), ipsc->cAlphaArgs(1));
                ErrorsFound = true;
                continue;
            }

            auto &roomAFNZoneNode = roomAFNZoneInfo.Node(RAFNNodeNum);
            roomAFNZoneNode.ZoneVolumeFraction = ipsc->rNumericArgs(1);
            if (!ipsc->lAlphaFieldBlanks(3)) {
                roomAFNZoneNode.NodeSurfListName = ipsc->cAlphaArgs(3);
            } else {
                roomAFNZoneNode.HasSurfacesAssigned = false;
            }
            if (!ipsc->lAlphaFieldBlanks(4)) {
                roomAFNZoneNode.NodeIntGainsListName = ipsc->cAlphaArgs(4);
            } else {
                roomAFNZoneNode.HasIntGainsAssigned = false;
            }
            if (!ipsc->lAlphaFieldBlanks(5)) {
                roomAFNZoneNode.NodeHVACListName = ipsc->cAlphaArgs(5);
            } else {
                roomAFNZoneNode.HasHVACAssigned = false;
            }

        } // loop thru TotNumOfRoomAFNNodes

        ipsc->cCurrentModuleObject = "RoomAir:Node:AirflowNetwork:AdjacentSurfaceList";
        TotNumOfRAFNNodeSurfLists = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, ipsc->cCurrentModuleObject);
        for (int Loop = 1; Loop <= TotNumOfRAFNNodeSurfLists; ++Loop) {
            bool foundList = false;
            state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                     ipsc->cCurrentModuleObject,
                                                                     Loop,
                                                                     ipsc->cAlphaArgs,
                                                                     NumAlphas,
                                                                     ipsc->rNumericArgs,
                                                                     NumNumbers,
                                                                     status,
                                                                     _,
                                                                     _,
                                                                     ipsc->cAlphaFieldNames,
                                                                     ipsc->cNumericFieldNames);

            
            for (int iZone = 1; iZone <= state.dataGlobal->NumOfZones; ++iZone) {
                // find surface list
                int RAFNNodeNum = 0;
                auto &roomAFNZoneInfo = state.dataRoomAirMod->AFNZoneInfo(iZone);
                if (roomAFNZoneInfo.NumOfAirNodes > 0) {
                    RAFNNodeNum = UtilityRoutines::FindItemInList(ipsc->cAlphaArgs(1),
                                                                  roomAFNZoneInfo.Node,
                                                                  &AFNAirNodeNested::NodeSurfListName,
                                                                  roomAFNZoneInfo.NumOfAirNodes);
                }
                
                if (RAFNNodeNum == 0) continue;

                // found it
                foundList = true;
                int NumSurfsThisNode = NumAlphas - 1;
                int NumOfSurfs = 0; // What is this used for?
                for (int spaceNum : state.dataHeatBal->Zone(iZone).spaceIndexes) {
                    auto &thisSpace = state.dataHeatBal->space(spaceNum);
                    NumOfSurfs += thisSpace.HTSurfaceLast - thisSpace.HTSurfaceFirst + 1;
                }

                auto &roomAFNZoneNode = roomAFNZoneInfo.Node(RAFNNodeNum);
                if (allocated(roomAFNZoneNode.SurfMask)) {
                    // throw error found twice
                    ShowSevereError(state, format("GetRoomAirflowNetworkData: Invalid {} = {}", ipsc->cAlphaFieldNames(1), ipsc->cAlphaArgs(1)));
                    ShowContinueError(state, format("Entered in {} = {}", ipsc->cCurrentModuleObject, ipsc->cAlphaArgs(1)));
                    ShowContinueError(state, "Duplicate RoomAir:Node:AirflowNetwork:AdjacentSurfaceList name.");
                    ErrorsFound = true;
                    continue;
                }

                roomAFNZoneNode.SurfMask.allocate(roomAFNZoneInfo.totNumSurfs);
                roomAFNZoneNode.SurfMask = false; // init
                roomAFNZoneNode.HasSurfacesAssigned = true;
                // relate surfaces to this air node and check to see whether surface names are specified correctly or not
                int SurfCount = 0;
                int thisSurfinZone = 0;
                for (int ListSurfNum = 2; ListSurfNum <= NumAlphas; ++ListSurfNum) {
                    for (int spaceNum : state.dataHeatBal->Zone(iZone).spaceIndexes) {
                        auto &thisSpace = state.dataHeatBal->space(spaceNum);
                        for (int SurfNum = thisSpace.HTSurfaceFirst; SurfNum <= thisSpace.HTSurfaceLast; ++SurfNum) {
                            ++thisSurfinZone;
                            if (ipsc->cAlphaArgs(ListSurfNum) == state.dataSurface->Surface(SurfNum).Name) {
                                roomAFNZoneNode.SurfMask(thisSurfinZone) = true;
                                ++SurfCount;
                                break;
                            }
                        }
                        if (SurfCount > 0) break;
                    }
                }
                if (NumSurfsThisNode != SurfCount) {
                    ShowSevereError(state,
                                    format("GetRoomAirflowNetworkData: Invalid {} = {}",
                                           ipsc->cAlphaFieldNames(1),
                                           ipsc->cAlphaArgs(1)));
                    ShowContinueError(state, format("Entered in {} = {}", ipsc->cCurrentModuleObject, ipsc->cAlphaArgs(1)));
                    ShowContinueError(state, "Some surface names were not found in the zone");
                    ErrorsFound = true;
                }
            } // for (iZone)

            if (!foundList) { // throw error
                ShowSevereError(state,
                                format("GetRoomAirflowNetworkData: Invalid {} = {}",
                                       ipsc->cAlphaFieldNames(1),
                                       ipsc->cAlphaArgs(1)));
                ShowContinueError(state, format("Entered in {} = {}", ipsc->cCurrentModuleObject, ipsc->cAlphaArgs(1)));
                ShowContinueError(state, "Did not find a RoomAir:Node:AirflowNetwork object that references this object");
                ErrorsFound = true;
            }
        } // loop thru TotNumOfRAFNNodeSurfLists

        ipsc->cCurrentModuleObject = "RoomAir:Node:AirflowNetwork:InternalGains";
        TotNumOfRAFNNodeGainsLists = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, ipsc->cCurrentModuleObject);
        for (int Loop = 1; Loop <= TotNumOfRAFNNodeGainsLists; ++Loop) {
            int foundList = false;
            state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                     ipsc->cCurrentModuleObject,
                                                                     Loop,
                                                                     ipsc->cAlphaArgs,
                                                                     NumAlphas,
                                                                     ipsc->rNumericArgs,
                                                                     NumNumbers,
                                                                     status,
                                                                     _,
                                                                     _,
                                                                     ipsc->cAlphaFieldNames,
                                                                     ipsc->cNumericFieldNames);

            ErrorObjectHeader eoh{routineName, ipsc->cCurrentModuleObject, ipsc->cAlphaArgs(1)};
            
            if (mod((NumAlphas + NumNumbers - 1), 3) != 0) {
                ShowSevereError(state, format("GetRoomAirflowNetworkData: For {}: {}", ipsc->cCurrentModuleObject, ipsc->cAlphaArgs(1)));
                ShowContinueError(state,
                                  format("Extensible field set are not evenly divisable by 3. Number of data entries = {}",
                                         NumAlphas + NumNumbers - 1));
                ErrorsFound = true;
                break;
            }

            for (int iZone = 1; iZone <= state.dataGlobal->NumOfZones; ++iZone) {
                auto &roomAFNZoneInfo = state.dataRoomAirMod->AFNZoneInfo(iZone);
                // find surface list
                int RAFNNodeNum = 0;
                if (roomAFNZoneInfo.NumOfAirNodes > 0) {
                    RAFNNodeNum = UtilityRoutines::FindItemInList(ipsc->cAlphaArgs(1),
                                                                  roomAFNZoneInfo.Node,
                                                                  &AFNAirNodeNested::NodeIntGainsListName,
                                                                  roomAFNZoneInfo.NumOfAirNodes);
                }
                if (RAFNNodeNum == 0) continue;

                // found it
                foundList = true;
                int numInputGains = (NumAlphas + NumNumbers - 1) / 3;
                int numSpacesInZone = state.dataHeatBal->Zone(iZone).numSpaces;
                int maxNumGains = numInputGains * numSpacesInZone;
                auto &roomAFNZoneNode = roomAFNZoneInfo.Node(RAFNNodeNum);
                if (allocated(roomAFNZoneNode.IntGain)) {
                    ShowSevereError(state,
                                    format("GetRoomAirflowNetworkData: Invalid {} = {}",
                                           ipsc->cAlphaFieldNames(1),
                                           ipsc->cAlphaArgs(1)));
                    ShowContinueError(state, format("Entered in {} = {}", ipsc->cCurrentModuleObject, ipsc->cAlphaArgs(1)));
                    ShowContinueError(state, format("Duplicate {} name.", ipsc->cCurrentModuleObject));
                    ErrorsFound = true;
                    continue;
                }
                    
                roomAFNZoneNode.IntGain.allocate(maxNumGains);
                roomAFNZoneNode.IntGainsDeviceIndices.allocate(maxNumGains);
                roomAFNZoneNode.intGainsDeviceSpaces.allocate(maxNumGains);
                roomAFNZoneNode.IntGainsFractions.allocate(maxNumGains);
                roomAFNZoneNode.HasIntGainsAssigned = true;
                int numGainsFound = 0;
                for (int gainsLoop = 1; gainsLoop <= numInputGains; ++gainsLoop) {
                    auto &intGain = roomAFNZoneNode.IntGain(gainsLoop);
                    intGain.type = static_cast<DataHeatBalance::IntGainType>(getEnumValue(DataHeatBalance::IntGainTypeNamesUC,
                                                                                          UtilityRoutines::makeUPPER(ipsc->cAlphaArgs(gainsLoop * 2))));
                    
                    if (intGain.type == DataHeatBalance::IntGainType::Invalid) {
                        ShowSevereInvalidKey(state, eoh, ipsc->cAlphaFieldNames(gainsLoop * 2), ipsc->cAlphaArgs(gainsLoop * 2));
                        ErrorsFound = true;
                        continue;
                    }
                    intGain.Name = ipsc->cAlphaArgs(gainsLoop * 2 + 1);
                    
                    bool gainFound = false;
                    // check all spaces in this zone for matching gains
                    for (int spaceNum : state.dataHeatBal->Zone(iZone).spaceIndexes) {
                        // verify type and name and get pointer to device in internal gains structure array
                        int intGainIndex = GetInternalGainDeviceIndex(state, spaceNum, intGain.type, intGain.Name);
                        if (intGainIndex >= 0) {
                                gainFound = true;
                                ++numGainsFound;
                                roomAFNZoneNode.intGainsDeviceSpaces(numGainsFound) = spaceNum;
                                roomAFNZoneNode.IntGainsDeviceIndices(numGainsFound) = intGainIndex;
                                roomAFNZoneNode.IntGainsFractions(numGainsFound) = ipsc->rNumericArgs(gainsLoop);
                        }
                    }
                    if (gainFound) {
                        roomAFNZoneNode.NumIntGains = numGainsFound;
                    } else {
                        ShowSevereError(state,
                                        format("GetRoomAirflowNetworkData: Invalid {} = {}",
                                               ipsc->cAlphaFieldNames(gainsLoop * 2 + 1),
                                               ipsc->cAlphaArgs(gainsLoop * 2 + 1)));
                        ShowContinueError(state, format("Entered in {} = {}", ipsc->cCurrentModuleObject, ipsc->cAlphaArgs(1)));
                        ShowContinueError(state, "Internal gain did not match correctly");
                        ErrorsFound = true;
                    }
                } // for (gainsLoop)
            } //for (iZone)
        } // loop thru TotNumOfRAFNNodeGainsLists

        // Get data of HVAC equipment
        ipsc->cCurrentModuleObject = "RoomAir:Node:AirflowNetwork:HVACEquipment";
        TotNumOfRAFNNodeHVACLists = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, ipsc->cCurrentModuleObject);
        for (int Loop = 1; Loop <= TotNumOfRAFNNodeHVACLists; ++Loop) {
            state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                     ipsc->cCurrentModuleObject,
                                                                     Loop,
                                                                     ipsc->cAlphaArgs,
                                                                     NumAlphas,
                                                                     ipsc->rNumericArgs,
                                                                     NumNumbers,
                                                                     status,
                                                                     _,
                                                                     _,
                                                                     ipsc->cAlphaFieldNames,
                                                                     ipsc->cNumericFieldNames);

            ErrorObjectHeader eoh{routineName, ipsc->cCurrentModuleObject, ipsc->cAlphaArgs(1)};
            
            if (mod((NumAlphas + NumNumbers - 1), 4) != 0) {
                ShowSevereError(state, format("GetRoomAirflowNetworkData: For {}: {}", ipsc->cCurrentModuleObject, ipsc->cAlphaArgs(1)));
                ShowContinueError(state,
                                  format("Extensible field set are not evenly divisable by 4. Number of data entries = {}",
                                         fmt::to_string(NumAlphas + NumNumbers - 1)));
                ErrorsFound = true;
                break;
            }
            
            for (int iZone = 1; iZone <= state.dataGlobal->NumOfZones; ++iZone) {
                auto &roomAFNZoneInfo = state.dataRoomAirMod->AFNZoneInfo(iZone);
                // find surface list
                int RAFNNodeNum = 0;    
                if (roomAFNZoneInfo.NumOfAirNodes > 0) {
                    RAFNNodeNum = UtilityRoutines::FindItemInList(ipsc->cAlphaArgs(1),
                                                                  roomAFNZoneInfo.Node,
                                                                  &AFNAirNodeNested::NodeHVACListName,
                                                                  roomAFNZoneInfo.NumOfAirNodes);
                }

                if (RAFNNodeNum == 0) continue;

                // found it
                auto &roomAFNNode = roomAFNZoneInfo.Node(RAFNNodeNum);
                if (allocated(roomAFNNode.HVAC)) {
                    ShowSevereError(state, format("GetRoomAirflowNetworkData: Invalid {} = {}", ipsc->cAlphaFieldNames(1), ipsc->cAlphaArgs(1)));
                    ShowContinueError(state, format("Entered in {} = {}", ipsc->cCurrentModuleObject, ipsc->cAlphaArgs(1)));
                    ShowContinueError(state, format("Duplicate {} name.", ipsc->cCurrentModuleObject));
                    ErrorsFound = true;
                    continue;
                }

                roomAFNNode.NumHVACs = (NumAlphas + NumNumbers - 1) / 4;
                roomAFNNode.HVAC.allocate(roomAFNNode.NumHVACs);
                roomAFNNode.HasHVACAssigned = true;
                for (int iEquip = 1; iEquip <= roomAFNNode.NumHVACs; ++iEquip) {
                    int iEquipArg = 2 + (iEquip - 1) * 2;
                    auto &roomAFNNodeHVAC = roomAFNNode.HVAC(iEquip);
                    roomAFNNodeHVAC.zoneEquipType =
                        static_cast<DataZoneEquipment::ZoneEquipType>(getEnumValue(DataZoneEquipment::zoneEquipTypeNamesUC, ipsc->cAlphaArgs(iEquipArg)));
                    if (roomAFNNodeHVAC.zoneEquipType == DataZoneEquipment::ZoneEquipType::Invalid) {
                        ShowSevereInvalidKey(state, eoh, ipsc->cAlphaFieldNames(iEquipArg), ipsc->cAlphaArgs(iEquipArg));
                        ErrorsFound = true;
                    }
                    roomAFNNodeHVAC.Name = ipsc->cAlphaArgs(3 + (iEquip - 1) * 2);
                        
                    // verify type and name and get pointer to device in HVAC equipment type and name structure array
                    TotNumEquip = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, ipsc->cAlphaArgs(iEquipArg));
                    if (TotNumEquip == 0) {
                        ShowSevereItemNotFound(state, eoh, ipsc->cAlphaFieldNames(iEquipArg), ipsc->cAlphaArgs(iEquipArg));
                        ErrorsFound = true;
                    }
                    roomAFNNodeHVAC.SupplyFraction = ipsc->rNumericArgs(iEquipArg);
                    roomAFNNodeHVAC.ReturnFraction = ipsc->rNumericArgs(iEquipArg);
                    
                    IntEquipError = CheckEquipName(
                                                   state,
                                                   roomAFNNodeHVAC.Name,
                                                   roomAFNNodeHVAC.SupplyNodeName,
                                                   roomAFNNodeHVAC.ReturnNodeName,
                                                   TotNumEquip,
                                                   roomAFNNodeHVAC.zoneEquipType);
                    
                    if (!IntEquipError) {
                        ShowSevereError(state,
                                        format("GetRoomAirflowNetworkData: Invalid {} = {}",
                                               ipsc->cAlphaFieldNames(3 + (iEquip - 1) * 2),
                                               ipsc->cAlphaArgs(2 + (iEquip - 1) * 2)));
                        ShowContinueError(state, format("Entered in {} = {}", ipsc->cCurrentModuleObject, ipsc->cAlphaArgs(1)));
                        ShowContinueError(state, "Internal gain did not match correctly");
                        ErrorsFound = true;
                    }
                    //!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                    // TYPE RoomAirflowNetworkHVACStruct
                    // INTEGER::EquipConfigIndex = 0
                } // for (iEquip)
            } // for (Zone)
        } // loop thru TotNumOfRAFNNodeHVACLists

        // do some checks on input data
        for (int iZone = 1; iZone <= state.dataGlobal->NumOfZones; ++iZone) {
            auto &roomAFNZoneInfo = state.dataRoomAirMod->AFNZoneInfo(iZone);
            if (roomAFNZoneInfo.NumOfAirNodes == 0)
                continue;

            // Check zone volume fraction
            Real64 SumFraction = 0.0;
            for (int iRoomAFNNode = 1; iRoomAFNNode <= roomAFNZoneInfo.NumOfAirNodes; ++iRoomAFNNode) {
                SumFraction += roomAFNZoneInfo.Node(iRoomAFNNode).ZoneVolumeFraction;
            }
            if (std::abs(SumFraction - 1.0) > 0.001) {
                ShowSevereError(state, "GetRoomAirflowNetworkData: Invalid, zone volume fractions do not sum to 1.0");
                ShowContinueError(state,
                                  format("Entered in RoomAir:Node:AirflowNetwork with Zone Name = {}", state.dataHeatBal->Zone(iZone).Name));
                ShowContinueError(state, "The Fraction of Zone Air Volume values across all the nodes needs to sum to 1.0.");
                ShowContinueError(state, format("The sum of fractions entered = {:.3R}", SumFraction));
                ErrorsFound = true;
            }

            // Check internal gain fraction
            for (int iRoomAFNNode = 1; iRoomAFNNode <= roomAFNZoneInfo.NumOfAirNodes; ++iRoomAFNNode) {
                auto &roomAFNNode = roomAFNZoneInfo.Node(iRoomAFNNode);
                for (int iGain = 1; iGain <= roomAFNNode.NumIntGains; ++iGain) {
                    auto &intGain = roomAFNNode.IntGain(iGain);
                    if (intGain.FractionCheck) continue;
                    Real64 SumFraction = roomAFNNode.IntGainsFractions(iGain);
                    intGain.FractionCheck = true;

                    for (int iRoomAFNNode2 = 1; iRoomAFNNode2 <= roomAFNZoneInfo.NumOfAirNodes; ++iRoomAFNNode2) {
                        auto &roomAFNNode2 = roomAFNZoneInfo.Node(iRoomAFNNode2);
                        for (int iGain2 = 1; iGain2 <= roomAFNNode2.NumIntGains; ++iGain2) {
                            auto &intGain2 = roomAFNNode2.IntGain(iGain2);
                            if (intGain2.FractionCheck) continue;
                            if (intGain.type == intGain2.type && UtilityRoutines::SameString(intGain.Name, intGain2.Name)) {
                                SumFraction += roomAFNNode2.IntGainsFractions(iGain2);
                                intGain2.FractionCheck = true;
                            }
                        }
                    }
                    if (std::abs(SumFraction - 1.0) > 0.001) {
                        ShowSevereError(state, "GetRoomAirflowNetworkData: Invalid, internal gain fractions do not sum to 1.0");
                        ShowContinueError(state,
                                          format("Entered in RoomAir:Node:AirflowNetwork with Zone Name = {}, Intrnal gain name = {}",
                                                 state.dataHeatBal->Zone(iZone).Name,
                                                 intGain.Name));
                        ShowContinueError(state, "The Fraction of internal gain across all the nodes needs to sum to 1.0.");
                        ShowContinueError(state, format("The sum of fractions entered = {:.3R}", SumFraction));
                        ErrorsFound = true;
                    }
                } // for (iGain)
            } // for (iRoomAFNNode)
        } // for (iZone)

        if (ErrorsFound)
            return;
        
        for (int iZone = 1; iZone <= state.dataGlobal->NumOfZones; ++iZone) {
            auto &roomAFNZoneInfo = state.dataRoomAirMod->AFNZoneInfo(iZone);
            if (!roomAFNZoneInfo.IsUsed || roomAFNZoneInfo.NumOfAirNodes == 0) continue;
            
            for (int iAirNode = 1; iAirNode <= roomAFNZoneInfo.NumOfAirNodes; ++iAirNode) {
                auto &roomAFNZoneNode = roomAFNZoneInfo.Node(iAirNode);
                SetupOutputVariable(state,
                                    "RoomAirflowNetwork Node Temperature",
                                    OutputProcessor::Unit::C,
                                    roomAFNZoneNode.AirTemp,
                                    OutputProcessor::SOVTimeStepType::HVAC,
                                    OutputProcessor::SOVStoreType::Average,
                                    roomAFNZoneNode.Name);
                SetupOutputVariable(state,
                                    "RoomAirflowNetwork Node Humidity Ratio",
                                    OutputProcessor::Unit::kgWater_kgDryAir,
                                    roomAFNZoneNode.HumRat,
                                    OutputProcessor::SOVTimeStepType::HVAC,
                                    OutputProcessor::SOVStoreType::Average,
                                    roomAFNZoneNode.Name);
                SetupOutputVariable(state,
                                    "RoomAirflowNetwork Node Relative Humidity",
                                    OutputProcessor::Unit::Perc,
                                    roomAFNZoneNode.RelHumidity,
                                    OutputProcessor::SOVTimeStepType::HVAC,
                                    OutputProcessor::SOVStoreType::Average,
                                    roomAFNZoneNode.Name);
            } // for (iAirNodE)
        } // for (iZone)
    }

    void SharedDVCVUFDataInit(EnergyPlusData &state, int const ZoneNum)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Linda Lawrie
        //       DATE WRITTEN   March 2005
        //       MODIFIED       Aug, 2013, Sam Brunswick -- for RoomAirCrossCrossVent modifications

        // PURPOSE OF THIS SUBROUTINE:
        // This routine allocates and initializes(?) the data that is shared between the
        // UCSD models (DV and CV)

        // Using/Aliasing
        using namespace DataEnvironment;
        using namespace DataSurfaces;
        using Psychrometrics::PsyRhoAirFnPbTdbW;

        // SUBROUTINE PARAMETER DEFINITIONS:
        Real64 constexpr BaseDischargeCoef(0.62);

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        bool SetZoneAux;
        Array1D_int AuxSurf;
        int MaxSurf;
        Array2D_int AuxAirflowNetworkSurf;
        int ZoneEquipConfigNum; // counter

        // Do the one time initializations
        if (state.dataRoomAirModelMgr->MyOneTimeFlag) {

            state.dataRoomAirModelMgr->MyEnvrnFlag.allocate(state.dataGlobal->NumOfZones);

            state.dataRoomAirMod->APos_Wall.allocate(state.dataSurface->TotSurfaces);
            state.dataRoomAirMod->APos_Floor.allocate(state.dataSurface->TotSurfaces);
            state.dataRoomAirMod->APos_Ceiling.allocate(state.dataSurface->TotSurfaces);
            state.dataRoomAirMod->PosZ_Wall.allocate(state.dataGlobal->NumOfZones);
            state.dataRoomAirMod->PosZ_Floor.allocate(state.dataGlobal->NumOfZones);
            state.dataRoomAirMod->PosZ_Ceiling.allocate(state.dataGlobal->NumOfZones);
            state.dataRoomAirMod->APos_Window.allocate(state.dataSurface->TotSurfaces);
            state.dataRoomAirMod->APos_Door.allocate(state.dataSurface->TotSurfaces);
            state.dataRoomAirMod->APos_Internal.allocate(state.dataSurface->TotSurfaces);
            state.dataRoomAirMod->PosZ_Window.allocate(state.dataGlobal->NumOfZones);
            state.dataRoomAirMod->PosZ_Door.allocate(state.dataGlobal->NumOfZones);
            state.dataRoomAirMod->PosZ_Internal.allocate(state.dataGlobal->NumOfZones);
            state.dataRoomAirMod->HCeiling.allocate(state.dataSurface->TotSurfaces);
            state.dataRoomAirMod->HWall.allocate(state.dataSurface->TotSurfaces);
            state.dataRoomAirMod->HFloor.allocate(state.dataSurface->TotSurfaces);
            state.dataRoomAirMod->HInternal.allocate(state.dataSurface->TotSurfaces);
            state.dataRoomAirMod->HWindow.allocate(state.dataSurface->TotSurfaces);
            state.dataRoomAirMod->HDoor.allocate(state.dataSurface->TotSurfaces);

            AuxSurf.allocate(state.dataGlobal->NumOfZones);

            state.dataRoomAirMod->ZoneCeilingHeight1.allocate(state.dataGlobal->NumOfZones);
            state.dataRoomAirMod->ZoneCeilingHeight2.allocate(state.dataGlobal->NumOfZones);
            state.dataRoomAirMod->ZoneCeilingHeight1 = 0.0;
            state.dataRoomAirMod->ZoneCeilingHeight2 = 0.0;

            // Arrays initializations
            state.dataRoomAirMod->APos_Wall = 0;
            state.dataRoomAirMod->APos_Floor = 0;
            state.dataRoomAirMod->APos_Ceiling = 0;
            std::fill(state.dataRoomAirMod->PosZ_Wall.begin(),state.dataRoomAirMod->PosZ_Wall.end(), BegEnd());
            std::fill(state.dataRoomAirMod->PosZ_Floor.begin(),state.dataRoomAirMod->PosZ_Floor.end(), BegEnd());
            std::fill(state.dataRoomAirMod->PosZ_Ceiling.begin(),state.dataRoomAirMod->PosZ_Ceiling.end(), BegEnd());
            state.dataRoomAirMod->APos_Window = 0;
            state.dataRoomAirMod->APos_Door = 0;
            state.dataRoomAirMod->APos_Internal = 0;
            std::fill(state.dataRoomAirMod->PosZ_Window.begin(),state.dataRoomAirMod->PosZ_Window.end(), BegEnd());
            std::fill(state.dataRoomAirMod->PosZ_Door.begin(),state.dataRoomAirMod->PosZ_Door.end(), BegEnd());
            std::fill(state.dataRoomAirMod->PosZ_Internal.begin(),state.dataRoomAirMod->PosZ_Internal.end(), BegEnd());
            state.dataRoomAirMod->HCeiling = 0.0;
            state.dataRoomAirMod->HWall = 0.0;
            state.dataRoomAirMod->HFloor = 0.0;
            state.dataRoomAirMod->HInternal = 0.0;
            state.dataRoomAirMod->HWindow = 0.0;
            state.dataRoomAirMod->HDoor = 0.0;

            int contWall = 0, contFloor = 0, contCeiling = 0, contWindow = 0, contInternal = 0, contDoor = 0;
            int contWallBeg = 0, contFloorBeg = 0, contCeilingBeg = 0, contWindowBeg = 0, contInternalBeg = 0, contDoorBeg = 0;
            int contWallLast = 0, contFloorLast = 0, contCeilingLast = 0, contWindowLast = 0, contInternalLast = 0, contDoorLast = 0;
            
            // Put the surface and zone information in Apos and PosZ arrays
            for (int ZNum = 1; ZNum <= state.dataGlobal->NumOfZones; ++ZNum) {
                // advance ONE position in the arrays PosZ because this is a new zone
                contWallBeg = contWall + 1;
                contFloorBeg = contFloor + 1;
                contCeilingBeg = contCeiling + 1;
                contWindowBeg = contWindow + 1;
                contInternalBeg = contInternal + 1;
                contDoorBeg = contDoor + 1;
                SetZoneAux = true;

                Real64 Z1ofZone = std::numeric_limits<Real64>::max();
                Real64 Z2ofZone = std::numeric_limits<Real64>::lowest();

                // cycle in this zone for all the surfaces
                for (int spaceNum : state.dataHeatBal->Zone(ZNum).spaceIndexes) {
                    auto &thisSpace = state.dataHeatBal->space(spaceNum);

                    for (int SurfNum = thisSpace.HTSurfaceFirst; SurfNum <= thisSpace.HTSurfaceLast; ++SurfNum) {
                        auto const &surf = state.dataSurface->Surface(SurfNum);
                        if (surf.Class != DataSurfaces::SurfaceClass::IntMass) {
                            // Recalculate lowest and highest height for the zone
                            for (int i = 1, u = surf.Sides; i <= u; ++i) {
                                Real64 const z_i = surf.Vertex(i).z;
                                if (z_i < Z1ofZone) { Z1ofZone = z_i; };
                                if (z_i > Z2ofZone) { Z2ofZone = z_i; };
                            }
                        }

                        // Put the reference to this surface in the appropriate array
                        if (surf.Class == SurfaceClass::Floor) {
                            ++contFloor;
                            state.dataRoomAirMod->APos_Floor(contFloor) = SurfNum;
                        } else if (surf.Class == SurfaceClass::Wall) {
                            ++contWall;
                            state.dataRoomAirMod->APos_Wall(contWall) = SurfNum;
                        } else if (surf.Class == SurfaceClass::Window) {
                            ++contWindow;
                            state.dataRoomAirMod->APos_Window(contWindow) = SurfNum;
                        } else if (surf.Class == SurfaceClass::IntMass) {
                            ++contInternal;
                            state.dataRoomAirMod->APos_Internal(contInternal) = SurfNum;
                        } else if (surf.Class == SurfaceClass::Door) {
                            ++contDoor;
                            state.dataRoomAirMod->APos_Door(contDoor) = SurfNum;
                        } else {
                            ++contCeiling;
                            state.dataRoomAirMod->APos_Ceiling(contCeiling) = SurfNum;
                        }
                    }
                } // for (SurfNum)

                contWallLast = contWall;
                contFloorLast = contFloor;
                contCeilingLast = contCeiling;
                contWindowLast = contWindow;
                contDoorLast = contDoor;
                contInternalLast = contInternal;
                // PosZ_Wall (... + 1) has the Begin Wall reference in Apos_Wall for the ZNum
                // PosZ_Wall (... + 2) has the End Wall reference in Apos_Wall for the ZNum
                state.dataRoomAirMod->PosZ_Wall(ZNum).beg = contWallBeg;
                state.dataRoomAirMod->PosZ_Wall(ZNum).end = contWallLast;
                state.dataRoomAirMod->PosZ_Floor(ZNum).beg = contFloorBeg;
                state.dataRoomAirMod->PosZ_Floor(ZNum).end = contFloorLast;
                state.dataRoomAirMod->PosZ_Ceiling(ZNum).beg = contCeilingBeg;
                state.dataRoomAirMod->PosZ_Ceiling(ZNum).end = contCeilingLast;
                state.dataRoomAirMod->PosZ_Window(ZNum).beg = contWindowBeg;
                state.dataRoomAirMod->PosZ_Window(ZNum).end = contWindowLast;
                state.dataRoomAirMod->PosZ_Door(ZNum).beg = contDoorBeg;
                state.dataRoomAirMod->PosZ_Door(ZNum).end = contDoorLast;
                state.dataRoomAirMod->PosZ_Internal(ZNum).beg = contInternalBeg;
                state.dataRoomAirMod->PosZ_Internal(ZNum).end = contInternalLast;
                // Save the highest and lowest height for this zone
                state.dataRoomAirMod->ZoneCeilingHeight1(ZNum) = Z1ofZone;
                state.dataRoomAirMod->ZoneCeilingHeight2(ZNum) = Z2ofZone;

                constexpr Real64 CeilingHeightDiffMaximum = 0.1;
                if (std::abs((Z2ofZone - Z1ofZone) - state.dataHeatBal->Zone(ZNum).CeilingHeight) > CeilingHeightDiffMaximum) {
                    ShowWarningError(state, format("RoomAirManager: Inconsistent ceiling heights in Zone: {}", state.dataHeatBal->Zone(ZNum).Name));
                    ShowContinueError(state, format("Lowest height=[{:.3R}].", Z1ofZone));
                    ShowContinueError(state, format("Highest height=[{:.3R}].", Z2ofZone));
                    ShowContinueError(state, format("Ceiling height=[{:.3R}].", state.dataHeatBal->Zone(ZNum).CeilingHeight));
                }
            } // for (ZoneNum)

            AuxSurf = 0;
            state.dataRoomAirMod->CrossVentNumAFNSurfaces = 0;

            // calculate maximum number of airflow network surfaces in each zone
            for (int iMzLink = 1; iMzLink <= state.afn->NumOfLinksMultiZone; ++iMzLink) {
                auto const &mzSurf = state.dataSurface->Surface(state.afn->MultizoneSurfaceData(iMzLink).SurfNum);
                ++AuxSurf(mzSurf.Zone);
                ++state.dataRoomAirMod->CrossVentNumAFNSurfaces;
                // Check if this is an interzone airflow network surface
                if (mzSurf.ExtBoundCond > 0 &&
                    (state.afn->MultizoneSurfaceData(iMzLink).SurfNum != mzSurf.ExtBoundCond)) {
                    ++AuxSurf(state.dataSurface->Surface(mzSurf.ExtBoundCond).Zone);
                    ++state.dataRoomAirMod->CrossVentNumAFNSurfaces;
                }
            }
            // calculate maximum number of airflow network surfaces in a single zone
            MaxSurf = AuxSurf(1);
            for (int iZone = 2; iZone <= state.dataGlobal->NumOfZones; ++iZone) {
                if (AuxSurf(iZone) > MaxSurf) MaxSurf = AuxSurf(iZone);
            }

            if (!allocated(state.dataRoomAirMod->AFNSurfaceCrossVent)) {
                state.dataRoomAirMod->AFNSurfaceCrossVent.allocate({0, MaxSurf}, state.dataGlobal->NumOfZones);
            }
            if (!allocated(state.dataRoomAirMod->CrossVentJetRecFlows)) {
                state.dataRoomAirMod->CrossVentJetRecFlows.allocate({0, MaxSurf}, state.dataGlobal->NumOfZones);
            }
            AuxAirflowNetworkSurf.allocate({0, MaxSurf}, state.dataGlobal->NumOfZones);
            // Width and Height for airflow network surfaces
            if (!allocated(state.dataRoomAirMod->SurfParametersCrossDispVent)) {
                state.dataRoomAirMod->SurfParametersCrossDispVent.allocate(state.afn->NumOfLinksMultiZone);
            }

            state.dataRoomAirMod->AFNSurfaceCrossVent = 0;
            // Organize surfaces in vector AirflowNetworkSurfaceUCSDCV(Zone, surface indexes)
            for (int iZone = 1; iZone <= state.dataGlobal->NumOfZones; ++iZone) {
                // the 0 component of the array has the number of relevant AirflowNetwork surfaces for the zone
                state.dataRoomAirMod->AFNSurfaceCrossVent(0, iZone) = AuxSurf(iZone);
                if (AuxSurf(iZone) == 0)
                        continue;
                
                Real64 const ceilingHeight = state.dataRoomAirMod->ZoneCeilingHeight1(iZone);
                int SurfNum = 1;

                for (int iMzLink = 1; iMzLink <= state.afn->NumOfLinksMultiZone; ++iMzLink) {
                    auto const &mzSurf = state.dataSurface->Surface(state.afn->MultizoneSurfaceData(iMzLink).SurfNum);
                    auto &surfParams = state.dataRoomAirMod->SurfParametersCrossDispVent(iMzLink);

                    if (mzSurf.Zone == iZone) {
                        // SurfNum has the reference surface number relative to AirflowNetworkSurfaceData
                        state.dataRoomAirMod->AFNSurfaceCrossVent(SurfNum, iZone) = iMzLink;
                        // calculate the surface width and height
                        int compNum = state.afn->AirflowNetworkLinkageData(iMzLink).CompNum;
                        int typeNum = state.afn->AirflowNetworkCompData(compNum).TypeNum;
                        if (state.afn->AirflowNetworkCompData(compNum).CompTypeNum == AirflowNetwork::iComponentTypeNum::DOP) {
                            Real64 WidthFactMax = 0.0;
                            Real64 HeightFactMax = 0.0;

                            Real64 WidthFact = 0.0;
                            Real64 HeightFact = 0.0;
                            auto const &mzCompDetOpening = state.afn->MultizoneCompDetOpeningData(typeNum);
                            for (int Loop3 = 1; Loop3 <= mzCompDetOpening.NumFac; ++Loop3) {
                                if (Loop3 == 1) {
                                    WidthFact = mzCompDetOpening.WidthFac1;
                                    HeightFact = mzCompDetOpening.HeightFac1;
                                } else if (Loop3 == 2) {
                                    WidthFact = mzCompDetOpening.WidthFac2;
                                    HeightFact = mzCompDetOpening.HeightFac2;
                                } else if (Loop3 == 3) {
                                    WidthFact = mzCompDetOpening.WidthFac3;
                                    HeightFact = mzCompDetOpening.HeightFac3;
                                } else if (Loop3 == 4) {
                                    WidthFact = mzCompDetOpening.WidthFac4;
                                    HeightFact = mzCompDetOpening.HeightFac4;
                                }
                                if (WidthFact > WidthFactMax) {
                                    WidthFactMax = WidthFact;
                                }
                                if (HeightFact > HeightFactMax) {
                                    HeightFactMax = HeightFact;
                                }
                            }
                            surfParams.Width = WidthFactMax * mzSurf.Width;
                            surfParams.Height = HeightFactMax * mzSurf.Height;
                            
                        } else if (state.afn->AirflowNetworkCompData(compNum).CompTypeNum == AirflowNetwork::iComponentTypeNum::SCR) { // surface type = CRACK
                            surfParams.Width = mzSurf.Width / 2;
                            auto const &zoneHeatBal = state.dataZoneTempPredictorCorrector->zoneHeatBalance(iZone);
                            Real64 AinCV =
                                state.afn->MultizoneSurfaceCrackData(typeNum).coefficient /
                                    (BaseDischargeCoef *
                                     std::sqrt(2.0 / PsyRhoAirFnPbTdbW(state, state.dataEnvrn->OutBaroPress, zoneHeatBal.MAT, zoneHeatBal.ZoneAirHumRat)));
                            surfParams.Height = AinCV / surfParams.Width;
                        }

                        // calculate the surface Zmin and Zmax
                        if (state.afn->AirflowNetworkCompData(compNum).CompTypeNum == AirflowNetwork::iComponentTypeNum::DOP || 
                            state.afn->AirflowNetworkCompData(compNum).CompTypeNum == AirflowNetwork::iComponentTypeNum::SCR) { // surface type = CRACK
                            Real64 z_min(std::numeric_limits<Real64>::max()), z_max(std::numeric_limits<Real64>::lowest());
                            for (int i = 1; i <= mzSurf.Sides; ++i) {
                                Real64 const z_i = mzSurf.Vertex(i).z;
                                z_min = std::min(z_min, z_i);
                                z_max = std::max(z_max, z_i);
                            }
                            surfParams.Zmin = z_min - ceilingHeight;
                            surfParams.Zmax = z_max - ceilingHeight;
                        }

                        ++SurfNum;
                        // Check if airflow network Surface is an interzone surface:

                    } else { // if (mzSurf.Zone == iZone)
                        int nodeNum1 = state.afn->MultizoneSurfaceData(iMzLink).NodeNums[0];
                        int nodeNum2 = state.afn->MultizoneSurfaceData(iMzLink).NodeNums[1];
                        if ((state.afn->AirflowNetworkNodeData(nodeNum2).EPlusZoneNum == iZone &&
                             state.afn->AirflowNetworkNodeData(nodeNum1).EPlusZoneNum > 0) ||
                            (state.afn->AirflowNetworkNodeData(nodeNum2).EPlusZoneNum > 0 &&
                             state.afn->AirflowNetworkNodeData(nodeNum1).EPlusZoneNum == iZone)) {
                            state.dataRoomAirMod->AFNSurfaceCrossVent(SurfNum, iZone) = iMzLink;
                            ++SurfNum;
                        }
                    }
                } // for (Loop2)
            } // for (iZone)

            AuxSurf.deallocate();

            if (any(state.dataRoomAirMod->IsZoneDispVent3Node) || any(state.dataRoomAirMod->IsZoneUI)) {
                state.dataRoomAirMod->MaxTempGrad.allocate(state.dataGlobal->NumOfZones);
                state.dataRoomAirMod->AvgTempGrad.allocate(state.dataGlobal->NumOfZones);
                state.dataRoomAirMod->TCMF.allocate(state.dataGlobal->NumOfZones);
                state.dataRoomAirMod->FracMinFlow.allocate(state.dataGlobal->NumOfZones);
                state.dataRoomAirMod->ZoneAirSystemON.allocate(state.dataGlobal->NumOfZones);
                // Allocate histories of displacement ventilation temperatures PH 3/5/04
                state.dataRoomAirMod->MATFloor.allocate(state.dataGlobal->NumOfZones);
                state.dataRoomAirMod->XMATFloor.allocate(state.dataGlobal->NumOfZones);
#ifdef GET_OUT
                state.dataRoomAirMod->XM2TFloor.allocate(state.dataGlobal->NumOfZones);
                state.dataRoomAirMod->XM3TFloor.allocate(state.dataGlobal->NumOfZones);
                state.dataRoomAirMod->XM4TFloor.allocate(state.dataGlobal->NumOfZones);
#endif //                
                state.dataRoomAirMod->DSXMATFloor.allocate(state.dataGlobal->NumOfZones);
#ifdef GET_OUT
                state.dataRoomAirMod->DSXM2TFloor.allocate(state.dataGlobal->NumOfZones);
                state.dataRoomAirMod->DSXM3TFloor.allocate(state.dataGlobal->NumOfZones);
                state.dataRoomAirMod->DSXM4TFloor.allocate(state.dataGlobal->NumOfZones);
#endif //                
                state.dataRoomAirMod->MATOC.allocate(state.dataGlobal->NumOfZones);
                state.dataRoomAirMod->XMATOC.allocate(state.dataGlobal->NumOfZones);
#ifdef GET_OUT
                state.dataRoomAirMod->XM2TOC.allocate(state.dataGlobal->NumOfZones);
                state.dataRoomAirMod->XM3TOC.allocate(state.dataGlobal->NumOfZones);
                state.dataRoomAirMod->XM4TOC.allocate(state.dataGlobal->NumOfZones);
#endif //                
                state.dataRoomAirMod->DSXMATOC.allocate(state.dataGlobal->NumOfZones);
#ifdef GET_OUT
                state.dataRoomAirMod->DSXM2TOC.allocate(state.dataGlobal->NumOfZones);
                state.dataRoomAirMod->DSXM3TOC.allocate(state.dataGlobal->NumOfZones);
                state.dataRoomAirMod->DSXM4TOC.allocate(state.dataGlobal->NumOfZones);
#endif //                
                state.dataRoomAirMod->MATMX.allocate(state.dataGlobal->NumOfZones);
                state.dataRoomAirMod->XMATMX.allocate(state.dataGlobal->NumOfZones);
#ifdef GET_OUT
                state.dataRoomAirMod->XM2TMX.allocate(state.dataGlobal->NumOfZones);
                state.dataRoomAirMod->XM3TMX.allocate(state.dataGlobal->NumOfZones);
                state.dataRoomAirMod->XM4TMX.allocate(state.dataGlobal->NumOfZones);
#endif //                
                state.dataRoomAirMod->DSXMATMX.allocate(state.dataGlobal->NumOfZones);
#ifdef GET_OUT
                state.dataRoomAirMod->DSXM2TMX.allocate(state.dataGlobal->NumOfZones);
                state.dataRoomAirMod->DSXM3TMX.allocate(state.dataGlobal->NumOfZones);
                state.dataRoomAirMod->DSXM4TMX.allocate(state.dataGlobal->NumOfZones);
#endif //                
                state.dataRoomAirMod->ZTMFloor.allocate(state.dataGlobal->NumOfZones);
#ifdef GET_OUT
                state.dataRoomAirMod->ZTM2Floor.allocate(state.dataGlobal->NumOfZones);
                state.dataRoomAirMod->ZTM3Floor.allocate(state.dataGlobal->NumOfZones);
#endif //                
                state.dataRoomAirMod->ZTMOC.allocate(state.dataGlobal->NumOfZones);
#ifdef GET_OUT
                state.dataRoomAirMod->ZTM2OC.allocate(state.dataGlobal->NumOfZones);
                state.dataRoomAirMod->ZTM3OC.allocate(state.dataGlobal->NumOfZones);
#endif //                
                state.dataRoomAirMod->ZTMMX.allocate(state.dataGlobal->NumOfZones);
#ifdef GET_OUT
                state.dataRoomAirMod->ZTM2MX.allocate(state.dataGlobal->NumOfZones);
                state.dataRoomAirMod->ZTM3MX.allocate(state.dataGlobal->NumOfZones);
#endif //                
                state.dataRoomAirMod->AIRRATFloor.allocate(state.dataGlobal->NumOfZones);
                state.dataRoomAirMod->AIRRATOC.allocate(state.dataGlobal->NumOfZones);
                state.dataRoomAirMod->AIRRATMX.allocate(state.dataGlobal->NumOfZones);
                state.dataRoomAirMod->ZTOC.allocate(state.dataGlobal->NumOfZones);
                state.dataRoomAirMod->ZTMX.allocate(state.dataGlobal->NumOfZones);
                state.dataRoomAirMod->ZTFloor.allocate(state.dataGlobal->NumOfZones);
                state.dataRoomAirMod->HeightTransition.allocate(state.dataGlobal->NumOfZones);
                state.dataRoomAirMod->Phi.allocate(state.dataGlobal->NumOfZones);
                state.dataRoomAirMod->Zone1Floor.allocate(state.dataGlobal->NumOfZones);
                state.dataRoomAirMod->ZoneMXFloor.allocate(state.dataGlobal->NumOfZones);
                state.dataRoomAirMod->ZoneM2Floor.allocate(state.dataGlobal->NumOfZones);
                state.dataRoomAirMod->Zone1OC.allocate(state.dataGlobal->NumOfZones);
                state.dataRoomAirMod->ZoneMXOC.allocate(state.dataGlobal->NumOfZones);
                state.dataRoomAirMod->ZoneM2OC.allocate(state.dataGlobal->NumOfZones);
                state.dataRoomAirMod->Zone1MX.allocate(state.dataGlobal->NumOfZones);
                state.dataRoomAirMod->ZoneMXMX.allocate(state.dataGlobal->NumOfZones);
                state.dataRoomAirMod->ZoneM2MX.allocate(state.dataGlobal->NumOfZones);

                state.dataRoomAirMod->MaxTempGrad = 0.0;
                state.dataRoomAirMod->AvgTempGrad = 0.0;
                state.dataRoomAirMod->TCMF = 23.0;
                state.dataRoomAirMod->FracMinFlow = 0.0;
                //      ZoneDVMixedFlagRep    = 0.0
                state.dataRoomAirMod->ZoneAirSystemON = false;
                //      ZoneDVMixedFlag=0
                state.dataRoomAirMod->MATFloor = 23.0;
                state.dataRoomAirMod->XMATFloor = {23.0, 23.0, 23.0, 23.0};
#ifdef GET_OUT
                state.dataRoomAirMod->XM2TFloor = 23.0;
                state.dataRoomAirMod->XM3TFloor = 23.0;
                state.dataRoomAirMod->XM4TFloor = 23.0;
#endif //                
                state.dataRoomAirMod->DSXMATFloor = {23.0, 23.0, 23.0, 23.0};
#ifdef GET_OUT
                state.dataRoomAirMod->DSXM2TFloor = 23.0;
                state.dataRoomAirMod->DSXM3TFloor = 23.0;
                state.dataRoomAirMod->DSXM4TFloor = 23.0;
#endif //                
                state.dataRoomAirMod->MATOC = {23.0, 23.0, 23.0, 23.0};
                state.dataRoomAirMod->XMATOC = {23.0, 23.0, 23.0, 23.0};
#ifdef GET_OUT
                state.dataRoomAirMod->XM2TOC = 23.0;
                state.dataRoomAirMod->XM3TOC = 23.0;
                state.dataRoomAirMod->XM4TOC = 23.0;
#endif //                
                state.dataRoomAirMod->DSXMATOC = {23.0, 23.0, 23.0, 23.0};
#ifdef GET_OUT
                state.dataRoomAirMod->DSXM2TOC = 23.0;
                state.dataRoomAirMod->DSXM3TOC = 23.0;
                state.dataRoomAirMod->DSXM4TOC = 23.0;
#endif //                
                state.dataRoomAirMod->MATMX = {23.0, 23.0, 23.0, 23.0};
                state.dataRoomAirMod->XMATMX = {23.0, 23.0, 23.0, 23.0};
#ifdef GET_OUT
                state.dataRoomAirMod->XM2TMX = 23.0;
                state.dataRoomAirMod->XM3TMX = 23.0;
                state.dataRoomAirMod->XM4TMX = 23.0;
#endif //                
                state.dataRoomAirMod->DSXMATMX = {23.0, 23.0, 23.0, 23.0};
#ifdef GET_OUT
                state.dataRoomAirMod->DSXM2TMX = 23.0;
                state.dataRoomAirMod->DSXM3TMX = 23.0;
                state.dataRoomAirMod->DSXM4TMX = 23.0;
#endif //                
                state.dataRoomAirMod->ZTMFloor = {23.0, 23.0, 23.0};
#ifdef GET_OUT
                state.dataRoomAirMod->ZTM2Floor = 23.0;
                state.dataRoomAirMod->ZTM3Floor = 23.0;
#endif //
                state.dataRoomAirMod->ZTMOC = {23.0, 23.0, 23.0};
#ifdef GET_OUT
                state.dataRoomAirMod->ZTM2OC = 23.0;
                state.dataRoomAirMod->ZTM3OC = 23.0;
#endif //                
                state.dataRoomAirMod->ZTMMX = {23.0, 23.0, 23.0};
#ifdef GET_OUT
                state.dataRoomAirMod->ZTM2MX = 23.0;
                state.dataRoomAirMod->ZTM3MX = 23.0;
#endif //                
                state.dataRoomAirMod->Zone1Floor = 23.0;
                state.dataRoomAirMod->ZoneMXFloor = 23.0;
                state.dataRoomAirMod->ZoneM2Floor = 23.0;
                state.dataRoomAirMod->Zone1OC = 23.0;
                state.dataRoomAirMod->ZoneMXOC = 23.0;
                state.dataRoomAirMod->ZoneM2OC = 23.0;
                state.dataRoomAirMod->Zone1MX = 23.0;
                state.dataRoomAirMod->ZoneMXMX = 23.0;
                state.dataRoomAirMod->ZoneM2MX = 23.0;
                state.dataRoomAirMod->AIRRATFloor = 0.0;
                state.dataRoomAirMod->AIRRATOC = 0.0;
                state.dataRoomAirMod->AIRRATMX = 0.0;
                state.dataRoomAirMod->ZTOC = 23.0;
                state.dataRoomAirMod->ZTMX = 23.0;
                state.dataRoomAirMod->ZTFloor = 23.0;
                state.dataRoomAirMod->HeightTransition = 0.0;
                state.dataRoomAirMod->Phi = 0.0;
                state.dataRoomAirMod->HCeiling = 0.0;
                state.dataRoomAirMod->HWall = 0.0;
                state.dataRoomAirMod->HFloor = 0.0;
                state.dataRoomAirMod->HInternal = 0.0;
                state.dataRoomAirMod->HWindow = 0.0;
                state.dataRoomAirMod->HDoor = 0.0;
            }

            if (any(state.dataRoomAirMod->IsZoneDispVent3Node)) {

                state.dataRoomAirMod->DispVent3NodeHcIn.allocate(state.dataSurface->TotSurfaces);
                state.dataRoomAirMod->ZoneDispVent3NodeMixedFlagRep.allocate(state.dataGlobal->NumOfZones);
                state.dataRoomAirMod->ZoneDispVent3NodeMixedFlag.allocate(state.dataGlobal->NumOfZones);
                state.dataRoomAirMod->DispVent3NodeHcIn = 0.0;
                state.dataRoomAirMod->ZoneDispVent3NodeMixedFlagRep = 0.0;
                state.dataRoomAirMod->ZoneDispVent3NodeMixedFlag = 0;
                // Output variables and DV zone flag
                for (int iZone = 1; iZone <= state.dataGlobal->NumOfZones; ++iZone) {
                    if (state.dataRoomAirMod->AirModel(iZone).AirModel != RoomAirModel::DispVent3Node)
                        continue; // don't set these up if they don't make sense
                    // CurrentModuleObject='RoomAirSettings:ThreeNodeDisplacementVentilation'
                    SetupOutputVariable(state,
                                        "Room Air Zone Mixed Subzone Temperature",
                                        OutputProcessor::Unit::C,
                                        state.dataRoomAirMod->ZTMX(iZone),
                                        OutputProcessor::SOVTimeStepType::HVAC,
                                        OutputProcessor::SOVStoreType::State,
                                        state.dataHeatBal->Zone(iZone).Name);
                    SetupOutputVariable(state,
                                        "Room Air Zone Occupied Subzone Temperature",
                                        OutputProcessor::Unit::C,
                                        state.dataRoomAirMod->ZTOC(iZone),
                                        OutputProcessor::SOVTimeStepType::HVAC,
                                        OutputProcessor::SOVStoreType::State,
                                        state.dataHeatBal->Zone(iZone).Name);
                    SetupOutputVariable(state,
                                        "Room Air Zone Floor Subzone Temperature",
                                        OutputProcessor::Unit::C,
                                        state.dataRoomAirMod->ZTFloor(iZone),
                                        OutputProcessor::SOVTimeStepType::HVAC,
                                        OutputProcessor::SOVStoreType::State,
                                        state.dataHeatBal->Zone(iZone).Name);
                    SetupOutputVariable(state,
                                        "Room Air Zone Transition Height",
                                        OutputProcessor::Unit::m,
                                        state.dataRoomAirMod->HeightTransition(iZone),
                                        OutputProcessor::SOVTimeStepType::HVAC,
                                        OutputProcessor::SOVStoreType::State,
                                        state.dataHeatBal->Zone(iZone).Name);
                    SetupOutputVariable(state,
                                        "Room Air Zone Recommended Minimum Flow Fraction",
                                        OutputProcessor::Unit::None,
                                        state.dataRoomAirMod->FracMinFlow(iZone),
                                        OutputProcessor::SOVTimeStepType::HVAC,
                                        OutputProcessor::SOVStoreType::State,
                                        state.dataHeatBal->Zone(iZone).Name);
                    SetupOutputVariable(state,
                                        "Room Air Zone Is Mixed Status",
                                        OutputProcessor::Unit::None,
                                        state.dataRoomAirMod->ZoneDispVent3NodeMixedFlagRep(iZone),
                                        OutputProcessor::SOVTimeStepType::HVAC,
                                        OutputProcessor::SOVStoreType::State,
                                        state.dataHeatBal->Zone(iZone).Name);
                    SetupOutputVariable(state,
                                        "Room Air Zone Average Temperature Gradient",
                                        OutputProcessor::Unit::K_m,
                                        state.dataRoomAirMod->AvgTempGrad(iZone),
                                        OutputProcessor::SOVTimeStepType::HVAC,
                                        OutputProcessor::SOVStoreType::State,
                                        state.dataHeatBal->Zone(iZone).Name);
                    SetupOutputVariable(state,
                                        "Room Air Zone Maximum Temperature Gradient",
                                        OutputProcessor::Unit::K_m,
                                        state.dataRoomAirMod->MaxTempGrad(iZone),
                                        OutputProcessor::SOVTimeStepType::HVAC,
                                        OutputProcessor::SOVStoreType::State,
                                        state.dataHeatBal->Zone(iZone).Name);
                    SetupOutputVariable(state,
                                        "Room Air Zone Thermal Comfort Effective Air Temperature",
                                        OutputProcessor::Unit::C,
                                        state.dataRoomAirMod->TCMF(iZone),
                                        OutputProcessor::SOVTimeStepType::HVAC,
                                        OutputProcessor::SOVStoreType::State,
                                        state.dataHeatBal->Zone(iZone).Name);
                    SetupOutputVariable(state,
                                        "Room Air Zone Thermostat Temperature",
                                        OutputProcessor::Unit::C,
                                        state.dataHeatBalFanSys->TempTstatAir(iZone),
                                        OutputProcessor::SOVTimeStepType::HVAC,
                                        OutputProcessor::SOVStoreType::State,
                                        state.dataHeatBal->Zone(iZone).Name);
                } // for (iZone)
            } // if (any(IsZoneDV))

            if (any(state.dataRoomAirMod->IsZoneUI)) {
                state.dataRoomAirMod->ZoneUFADMixedFlag.allocate(state.dataGlobal->NumOfZones);
                state.dataRoomAirMod->ZoneUFADMixedFlagRep.allocate(state.dataGlobal->NumOfZones);
                state.dataRoomAirMod->UFADHcIn.allocate(state.dataSurface->TotSurfaces);
                state.dataRoomAirMod->ZoneUFADGamma.allocate(state.dataGlobal->NumOfZones);
                state.dataRoomAirMod->ZoneUFADPowInPlumes.allocate(state.dataGlobal->NumOfZones);
                state.dataRoomAirMod->ZoneUFADPowInPlumesfromWindows.allocate(state.dataGlobal->NumOfZones);
                state.dataRoomAirMod->ZoneUFADMixedFlag = 0;
                state.dataRoomAirMod->ZoneUFADMixedFlagRep = 0.0;
                state.dataRoomAirMod->UFADHcIn = 0.0;
                state.dataRoomAirMod->ZoneUFADGamma = 0.0;
                state.dataRoomAirMod->ZoneUFADPowInPlumes = 0.0;
                state.dataRoomAirMod->ZoneUFADPowInPlumesfromWindows = 0.0;
                
                // Output variables and UF zone flag
                for (int iZone = 1; iZone <= state.dataGlobal->NumOfZones; ++iZone) {
                    if (state.dataRoomAirMod->AirModel(iZone).AirModel != RoomAirModel::UFADInt)
                        continue; // don't set these up if they don't make sense

                    auto &zone = state.dataHeatBal->Zone(iZone);
                    // CurrentModuleObject='RoomAirSettings:UnderFloorAirDistributionInterior'
                    SetupOutputVariable(state,
                                        "Room Air Zone Mixed Subzone Temperature",
                                        OutputProcessor::Unit::C,
                                        state.dataRoomAirMod->ZTMX(iZone),
                                        OutputProcessor::SOVTimeStepType::HVAC,
                                        OutputProcessor::SOVStoreType::State,
                                        zone.Name);
                    SetupOutputVariable(state,
                                        "Room Air Zone Occupied Subzone Temperature",
                                        OutputProcessor::Unit::C,
                                        state.dataRoomAirMod->ZTOC(iZone),
                                        OutputProcessor::SOVTimeStepType::HVAC,
                                        OutputProcessor::SOVStoreType::State,
                                        zone.Name);
                    SetupOutputVariable(state,
                                        "Room Air Zone Transition Height",
                                        OutputProcessor::Unit::m,
                                        state.dataRoomAirMod->HeightTransition(iZone),
                                        OutputProcessor::SOVTimeStepType::HVAC,
                                        OutputProcessor::SOVStoreType::State,
                                        zone.Name);
                    SetupOutputVariable(state,
                                        "Room Air Zone Is Mixed Status",
                                        OutputProcessor::Unit::None,
                                        state.dataRoomAirMod->ZoneUFADMixedFlagRep(iZone),
                                        OutputProcessor::SOVTimeStepType::HVAC,
                                        OutputProcessor::SOVStoreType::State,
                                        zone.Name);
                    SetupOutputVariable(state,
                                        "Room Air Zone Average Temperature Gradient",
                                        OutputProcessor::Unit::K_m,
                                        state.dataRoomAirMod->AvgTempGrad(iZone),
                                        OutputProcessor::SOVTimeStepType::HVAC,
                                        OutputProcessor::SOVStoreType::State,
                                        zone.Name);
                    SetupOutputVariable(state,
                                        "Room Air Zone Effective Comfort Air Temperature",
                                        OutputProcessor::Unit::C,
                                        state.dataRoomAirMod->TCMF(iZone),
                                        OutputProcessor::SOVTimeStepType::HVAC,
                                        OutputProcessor::SOVStoreType::State,
                                        zone.Name);
                    SetupOutputVariable(state,
                                        "Room Air Zone Thermostat Temperature",
                                        OutputProcessor::Unit::C,
                                        state.dataHeatBalFanSys->TempTstatAir(iZone),
                                        OutputProcessor::SOVTimeStepType::HVAC,
                                        OutputProcessor::SOVStoreType::State,
                                        zone.Name);
                    SetupOutputVariable(state,
                                        "Room Air Zone Transition Height Gamma Value",
                                        OutputProcessor::Unit::None,
                                        state.dataRoomAirMod->ZoneUFADGamma(iZone),
                                        OutputProcessor::SOVTimeStepType::HVAC,
                                        OutputProcessor::SOVStoreType::State,
                                        zone.Name);
                    SetupOutputVariable(state,
                                        "Room Air Zone Plume Heat Transfer Rate",
                                        OutputProcessor::Unit::W,
                                        state.dataRoomAirMod->ZoneUFADPowInPlumes(iZone),
                                        OutputProcessor::SOVTimeStepType::HVAC,
                                        OutputProcessor::SOVStoreType::State,
                                        zone.Name);
                    SetupOutputVariable(state,
                                        "Room Air Zone Temperature Stratification Fraction",
                                        OutputProcessor::Unit::None,
                                        state.dataRoomAirMod->Phi(iZone),
                                        OutputProcessor::SOVTimeStepType::HVAC,
                                        OutputProcessor::SOVStoreType::State,
                                        zone.Name);

                    // set zone equip pointer in the UCSDUI data structure
                    state.dataRoomAirMod->ZoneUFADInt(state.dataRoomAirMod->ZoneUFADPtr(iZone)).ZoneEquipPtr = iZone;
                } // for (iZone)

                
                for (int iZone = 1; iZone <= state.dataGlobal->NumOfZones; ++iZone) {
                    if (state.dataRoomAirMod->AirModel(iZone).AirModel != RoomAirModel::UFADExt)
                        continue; // don't set these up if they don't make sense
                    // CurrentModuleObject='RoomAirSettings:UnderFloorAirDistributionExterior'
                    auto const &zone = state.dataHeatBal->Zone(iZone);
                    SetupOutputVariable(state,
                                        "Room Air Zone Mixed Subzone Temperature",
                                        OutputProcessor::Unit::C,
                                        state.dataRoomAirMod->ZTMX(iZone),
                                        OutputProcessor::SOVTimeStepType::HVAC,
                                        OutputProcessor::SOVStoreType::State,
                                        zone.Name);
                    SetupOutputVariable(state,
                                        "Room Air Zone Occupied Subzone Temperature",
                                        OutputProcessor::Unit::C,
                                        state.dataRoomAirMod->ZTOC(iZone),
                                        OutputProcessor::SOVTimeStepType::HVAC,
                                        OutputProcessor::SOVStoreType::State,
                                        zone.Name);
                    SetupOutputVariable(state,
                                        "Room Air Zone Transition Height",
                                        OutputProcessor::Unit::m,
                                        state.dataRoomAirMod->HeightTransition(iZone),
                                        OutputProcessor::SOVTimeStepType::HVAC,
                                        OutputProcessor::SOVStoreType::State,
                                        zone.Name);
                    SetupOutputVariable(state,
                                        "Room Air Zone Is Mixed Status",
                                        OutputProcessor::Unit::None,
                                        state.dataRoomAirMod->ZoneUFADMixedFlagRep(iZone),
                                        OutputProcessor::SOVTimeStepType::HVAC,
                                        OutputProcessor::SOVStoreType::State,
                                        zone.Name);
                    SetupOutputVariable(state,
                                        "Room Air Zone Average Temperature Gradient",
                                        OutputProcessor::Unit::K_m,
                                        state.dataRoomAirMod->AvgTempGrad(iZone),
                                        OutputProcessor::SOVTimeStepType::HVAC,
                                        OutputProcessor::SOVStoreType::State,
                                        zone.Name);
                    SetupOutputVariable(state,
                                        "Room Air Zone Effective Comfort Air Temperature",
                                        OutputProcessor::Unit::C,
                                        state.dataRoomAirMod->TCMF(iZone),
                                        OutputProcessor::SOVTimeStepType::HVAC,
                                        OutputProcessor::SOVStoreType::State,
                                        zone.Name);
                    SetupOutputVariable(state,
                                        "Room Air Zone Thermostat Temperature",
                                        OutputProcessor::Unit::C,
                                        state.dataHeatBalFanSys->TempTstatAir(iZone),
                                        OutputProcessor::SOVTimeStepType::HVAC,
                                        OutputProcessor::SOVStoreType::State,
                                        zone.Name);
                    SetupOutputVariable(state,
                                        "Room Air Zone Transition Height Gamma Value",
                                        OutputProcessor::Unit::None,
                                        state.dataRoomAirMod->ZoneUFADGamma(iZone),
                                        OutputProcessor::SOVTimeStepType::HVAC,
                                        OutputProcessor::SOVStoreType::State,
                                        zone.Name);
                    SetupOutputVariable(state,
                                        "Room Air Zone Plume Heat Transfer Rate",
                                        OutputProcessor::Unit::W,
                                        state.dataRoomAirMod->ZoneUFADPowInPlumes(iZone),
                                        OutputProcessor::SOVTimeStepType::HVAC,
                                        OutputProcessor::SOVStoreType::State,
                                        zone.Name);
                    SetupOutputVariable(state,
                                        "Room Air Zone Window Plume Heat Transfer Rate",
                                        OutputProcessor::Unit::W,
                                        state.dataRoomAirMod->ZoneUFADPowInPlumesfromWindows(iZone),
                                        OutputProcessor::SOVTimeStepType::HVAC,
                                        OutputProcessor::SOVStoreType::State,
                                        zone.Name);
                    SetupOutputVariable(state,
                                        "Room Air Zone Temperature Stratification Fraction",
                                        OutputProcessor::Unit::None,
                                        state.dataRoomAirMod->Phi(iZone),
                                        OutputProcessor::SOVTimeStepType::HVAC,
                                        OutputProcessor::SOVStoreType::State,
                                        zone.Name);
                    // set zone equip pointer in the UCSDUE data structure
                    state.dataRoomAirMod->ZoneUFADExt(state.dataRoomAirMod->ZoneUFADPtr(iZone)).ZoneEquipPtr = iZone;
                }
            }

            if (any(state.dataRoomAirMod->IsZoneCrossVent)) {
                state.dataRoomAirMod->CrossVentHcIn.allocate(state.dataSurface->TotSurfaces);
                state.dataRoomAirMod->ZTJET.allocate(state.dataGlobal->NumOfZones);
                // Most ZTJet takes defaults
                state.dataRoomAirMod->ZTREC.allocate(state.dataGlobal->NumOfZones);
                state.dataRoomAirMod->RoomOutflowTemp.allocate(state.dataGlobal->NumOfZones);
                // Most ZTREC takes defaults
                state.dataRoomAirMod->JetRecAreaRatio.allocate(state.dataGlobal->NumOfZones);
                state.dataRoomAirMod->Urec.allocate(state.dataGlobal->NumOfZones);
                state.dataRoomAirMod->Ujet.allocate(state.dataGlobal->NumOfZones);
                state.dataRoomAirMod->Qrec.allocate(state.dataGlobal->NumOfZones);
                state.dataRoomAirMod->Qtot.allocate(state.dataGlobal->NumOfZones);
                state.dataRoomAirMod->RecInflowRatio.allocate(state.dataGlobal->NumOfZones);
                state.dataRoomAirMod->Uhc.allocate(state.dataGlobal->NumOfZones);
                state.dataRoomAirMod->Ain.allocate(state.dataGlobal->NumOfZones);
                state.dataRoomAirMod->Tin.allocate(state.dataGlobal->NumOfZones);
                state.dataRoomAirMod->Droom.allocate(state.dataGlobal->NumOfZones);
                state.dataRoomAirMod->Dstar.allocate(state.dataGlobal->NumOfZones);
                state.dataRoomAirMod->ZoneCrossVentIsMixing.allocate(state.dataGlobal->NumOfZones);
                state.dataRoomAirMod->Rfr.allocate(state.dataGlobal->NumOfZones);
                state.dataRoomAirMod->ZoneCrossVentHasREC.allocate(state.dataGlobal->NumOfZones);

                state.dataRoomAirMod->ZTJET = 23.0;
                state.dataRoomAirMod->RoomOutflowTemp = 23.0;
                state.dataRoomAirMod->ZTREC = 23.0;
                state.dataRoomAirMod->CrossVentHcIn = 0.0;
                state.dataRoomAirMod->JetRecAreaRatio = 0.2;
                state.dataRoomAirMod->Urec = 0.2;
                state.dataRoomAirMod->Ujet = 0.2;
                state.dataRoomAirMod->Qrec = 0.2;
                state.dataRoomAirMod->Uhc = 0.2;
                state.dataRoomAirMod->Ain = 1.0;
                state.dataRoomAirMod->Tin = 23.0;
                state.dataRoomAirMod->Droom = 6.0;
                state.dataRoomAirMod->ZoneCrossVentIsMixing = 0.0;
                state.dataRoomAirMod->Rfr = 10.0;
                state.dataRoomAirMod->ZoneCrossVentHasREC = 1.0;
                state.dataRoomAirMod->HCeiling = 0.0;
                state.dataRoomAirMod->HWall = 0.0;
                state.dataRoomAirMod->HFloor = 0.0;
                state.dataRoomAirMod->HInternal = 0.0;
                state.dataRoomAirMod->HWindow = 0.0;
                state.dataRoomAirMod->HDoor = 0.0;

                for (int iZone = 1; iZone <= state.dataGlobal->NumOfZones; ++iZone) {
                    if (state.dataRoomAirMod->AirModel(iZone).AirModel != RoomAirModel::CrossVent)
                        continue; // don't set these up if they don't make sense

                    ZoneEquipConfigNum = ZoneNum; // Where does this ZoneNum come from?

                    auto const &zone = state.dataHeatBal->Zone(iZone);
                    // check whether this zone is a controlled zone or not
                    if (state.dataZoneEquip->ZoneEquipConfig(ZoneEquipConfigNum).IsControlled) {
                        state.dataRoomAirMod->IsZoneCrossVent(iZone) = false;
                        state.dataRoomAirMod->AirModel(iZone).SimAirModel = false;
                        ShowSevereError(state,
                                        format("Unmixed Flow: Cross Ventilation cannot be applied for Zone={}", zone.Name));
                        ShowContinueError(state,
                                          format("An HVAC system is present in the zone. Fully mixed airflow model will be used for Zone={}", zone.Name));
                        continue;
                    }
                    // CurrentModuleObject='RoomAirSettings:CrossVentilation'
                    SetupOutputVariable(state,
                                        "Room Air Zone Jet Region Temperature",
                                        OutputProcessor::Unit::C,
                                        state.dataRoomAirMod->ZTJET(iZone),
                                        OutputProcessor::SOVTimeStepType::Zone,
                                        OutputProcessor::SOVStoreType::Average,
                                        zone.Name);
                    SetupOutputVariable(state,
                                        "Room Air Zone Recirculation Region Temperature",
                                        OutputProcessor::Unit::C,
                                        state.dataRoomAirMod->ZTREC(iZone),
                                        OutputProcessor::SOVTimeStepType::Zone,
                                        OutputProcessor::SOVStoreType::Average,
                                        zone.Name);
                    SetupOutputVariable(state,
                                        "Room Air Zone Jet Region Average Air Velocity",
                                        OutputProcessor::Unit::m_s,
                                        state.dataRoomAirMod->Ujet(iZone),
                                        OutputProcessor::SOVTimeStepType::Zone,
                                        OutputProcessor::SOVStoreType::Average,
                                        zone.Name);
                    SetupOutputVariable(state,
                                        "Room Air Zone Recirculation Region Average Air Velocity",
                                        OutputProcessor::Unit::m_s,
                                        state.dataRoomAirMod->Urec(iZone),
                                        OutputProcessor::SOVTimeStepType::Zone,
                                        OutputProcessor::SOVStoreType::Average,
                                        zone.Name);
                    SetupOutputVariable(state,
                                        "Room Air Zone Recirculation and Inflow Rate Ratio",
                                        OutputProcessor::Unit::None,
                                        state.dataRoomAirMod->RecInflowRatio(iZone),
                                        OutputProcessor::SOVTimeStepType::Zone,
                                        OutputProcessor::SOVStoreType::Average,
                                        zone.Name);
                    SetupOutputVariable(state,
                                        "Room Air Zone Inflow Opening Area",
                                        OutputProcessor::Unit::m2,
                                        state.dataRoomAirMod->Ain(iZone),
                                        OutputProcessor::SOVTimeStepType::Zone,
                                        OutputProcessor::SOVStoreType::Average,
                                        zone.Name);
                    SetupOutputVariable(state,
                                        "Room Air Zone Room Length",
                                        OutputProcessor::Unit::m,
                                        state.dataRoomAirMod->Dstar(iZone),
                                        OutputProcessor::SOVTimeStepType::Zone,
                                        OutputProcessor::SOVStoreType::Average,
                                        zone.Name);
                    SetupOutputVariable(state,
                                        "Room Air Zone Is Mixing Status",
                                        OutputProcessor::Unit::None,
                                        state.dataRoomAirMod->ZoneCrossVentIsMixing(iZone),
                                        OutputProcessor::SOVTimeStepType::Zone,
                                        OutputProcessor::SOVStoreType::State,
                                        zone.Name);
                    SetupOutputVariable(state,
                                        "Room Air Zone Is Recirculating Status",
                                        OutputProcessor::Unit::None,
                                        state.dataRoomAirMod->ZoneCrossVentHasREC(iZone),
                                        OutputProcessor::SOVTimeStepType::Zone,
                                        OutputProcessor::SOVStoreType::State,
                                        zone.Name);
                    for (int i = 1; i <= state.dataRoomAirMod->AFNSurfaceCrossVent(0, ZoneNum); ++i) {
                        int N = state.afn->AirflowNetworkLinkageData(i).CompNum;
                        if (state.afn->AirflowNetworkCompData(N).CompTypeNum == AirflowNetwork::iComponentTypeNum::DOP) {
                            SetupOutputVariable(
                                state,
                                "Room Air Window Jet Region Average Air Velocity",
                                OutputProcessor::Unit::m_s,
                                state.dataRoomAirMod->CrossVentJetRecFlows(i, iZone).Ujet,
                                OutputProcessor::SOVTimeStepType::Zone,
                                OutputProcessor::SOVStoreType::Average,
                                state.afn->MultizoneSurfaceData(i).SurfName);
                        }
                    }
                } // for (iZone)
            } // if (any(isZoneCV))

            state.dataRoomAirModelMgr->MyEnvrnFlag = true;

            state.dataRoomAirModelMgr->MyOneTimeFlag = false;
        } // if (myOneTimeFlag)

        // Do the Begin Environment initializations
        if (state.dataGlobal->BeginEnvrnFlag && state.dataRoomAirModelMgr->MyEnvrnFlag(ZoneNum)) {

            if (state.dataRoomAirMod->IsZoneDispVent3Node(ZoneNum) || state.dataRoomAirMod->IsZoneUI(ZoneNum)) {

                state.dataRoomAirMod->MaxTempGrad(ZoneNum) = 0.0;
                state.dataRoomAirMod->AvgTempGrad(ZoneNum) = 0.0;
                state.dataRoomAirMod->TCMF(ZoneNum) = 23.0;
                state.dataRoomAirMod->FracMinFlow(ZoneNum) = 0.0;
                state.dataRoomAirMod->ZoneAirSystemON(ZoneNum) = false;
                state.dataRoomAirMod->MATFloor(ZoneNum) = 23.0;
                state.dataRoomAirMod->XMATFloor(ZoneNum) = {23.0, 23.0, 23.0, 23.0};
#ifdef GET_OUT
                state.dataRoomAirMod->XM2TFloor(ZoneNum) = 23.0;
                state.dataRoomAirMod->XM3TFloor(ZoneNum) = 23.0;
                state.dataRoomAirMod->XM4TFloor(ZoneNum) = 23.0;
#endif //                
                state.dataRoomAirMod->DSXMATFloor(ZoneNum) = {23.0, 23.0, 23.0, 23.0};
#ifdef GET_OUT
                state.dataRoomAirMod->DSXM2TFloor(ZoneNum) = 23.0;
                state.dataRoomAirMod->DSXM3TFloor(ZoneNum) = 23.0;
                state.dataRoomAirMod->DSXM4TFloor(ZoneNum) = 23.0;
#endif //                
                state.dataRoomAirMod->MATOC(ZoneNum) = 23.0;
                state.dataRoomAirMod->XMATOC(ZoneNum) = {23.0, 23.0, 23.0, 23.0};
#ifdef GET_OUT
                state.dataRoomAirMod->XM2TOC(ZoneNum) = 23.0;
                state.dataRoomAirMod->XM3TOC(ZoneNum) = 23.0;
                state.dataRoomAirMod->XM4TOC(ZoneNum) = 23.0;
#endif //                
                state.dataRoomAirMod->DSXMATOC(ZoneNum) = {23.0, 23.0, 23.0, 23.0};
#ifdef GET_OUT
                state.dataRoomAirMod->DSXM2TOC(ZoneNum) = 23.0;
                state.dataRoomAirMod->DSXM3TOC(ZoneNum) = 23.0;
                state.dataRoomAirMod->DSXM4TOC(ZoneNum) = 23.0;
#endif //                
                state.dataRoomAirMod->MATMX(ZoneNum) = 23.0;
                state.dataRoomAirMod->XMATMX(ZoneNum) = {23.0, 23.0, 23.0, 23.0};
#ifdef GET_OUT
                state.dataRoomAirMod->XM2TMX(ZoneNum) = 23.0;
                state.dataRoomAirMod->XM3TMX(ZoneNum) = 23.0;
                state.dataRoomAirMod->XM4TMX(ZoneNum) = 23.0;
#endif //                
                state.dataRoomAirMod->DSXMATMX(ZoneNum) = {23.0, 23.0, 23.0, 23.0};
#ifdef GET_OUT
                state.dataRoomAirMod->DSXM2TMX(ZoneNum) = 23.0;
                state.dataRoomAirMod->DSXM3TMX(ZoneNum) = 23.0;
                state.dataRoomAirMod->DSXM4TMX(ZoneNum) = 23.0;
#endif //                
                state.dataRoomAirMod->ZTMFloor(ZoneNum) = {23.0, 23.0, 23.0};
#ifdef GET_OUT
                state.dataRoomAirMod->ZTM2Floor(ZoneNum) = 23.0;
                state.dataRoomAirMod->ZTM3Floor(ZoneNum) = 23.0;
#endif //                
                state.dataRoomAirMod->Zone1Floor(ZoneNum) = 23.0;
                state.dataRoomAirMod->ZoneMXFloor(ZoneNum) = 23.0;
                state.dataRoomAirMod->ZoneM2Floor(ZoneNum) = 23.0;
                state.dataRoomAirMod->ZTMOC(ZoneNum) = {23.0, 23.0, 23.0};
#ifdef GET_OUT
                state.dataRoomAirMod->ZTM2OC(ZoneNum) = 23.0;
                state.dataRoomAirMod->ZTM3OC(ZoneNum) = 23.0;
#endif //                
                state.dataRoomAirMod->Zone1OC(ZoneNum) = 23.0;
                state.dataRoomAirMod->ZoneMXOC(ZoneNum) = 23.0;
                state.dataRoomAirMod->ZoneM2OC(ZoneNum) = 23.0;
                state.dataRoomAirMod->ZTMMX(ZoneNum) = {23.0, 23.0, 23.0};
#ifdef GET_OUT
                state.dataRoomAirMod->ZTM2MX(ZoneNum) = 23.0;
                state.dataRoomAirMod->ZTM3MX(ZoneNum) = 23.0;
#endif //                
                state.dataRoomAirMod->Zone1MX(ZoneNum) = 23.0;
                state.dataRoomAirMod->ZoneMXMX(ZoneNum) = 23.0;
                state.dataRoomAirMod->ZoneM2MX(ZoneNum) = 23.0;
                state.dataRoomAirMod->AIRRATFloor(ZoneNum) = 0.0;
                state.dataRoomAirMod->AIRRATOC(ZoneNum) = 0.0;
                state.dataRoomAirMod->AIRRATMX(ZoneNum) = 0.0;
                state.dataRoomAirMod->ZTOC(ZoneNum) = 23.0;
                state.dataRoomAirMod->ZTMX(ZoneNum) = 23.0;
                state.dataRoomAirMod->ZTFloor(ZoneNum) = 23.0;
                state.dataRoomAirMod->HeightTransition(ZoneNum) = 0.0;
                state.dataRoomAirMod->Phi(ZoneNum) = 0.0;
                state.dataRoomAirMod->HCeiling = 0.0;
                state.dataRoomAirMod->HWall = 0.0;
                state.dataRoomAirMod->HFloor = 0.0;
                state.dataRoomAirMod->HInternal = 0.0;
                state.dataRoomAirMod->HWindow = 0.0;
                state.dataRoomAirMod->HDoor = 0.0;
            }

            if (state.dataRoomAirMod->IsZoneDispVent3Node(ZoneNum)) {

                state.dataRoomAirMod->DispVent3NodeHcIn = 0.0;
                state.dataRoomAirMod->ZoneDispVent3NodeMixedFlagRep(ZoneNum) = 0.0;
                state.dataRoomAirMod->ZoneDispVent3NodeMixedFlag(ZoneNum) = 0;
            }

            if (state.dataRoomAirMod->IsZoneUI(ZoneNum)) {

                state.dataRoomAirMod->UFADHcIn = 0.0;
                state.dataRoomAirMod->ZoneUFADMixedFlag(ZoneNum) = 0;
                state.dataRoomAirMod->ZoneUFADMixedFlagRep(ZoneNum) = 0.0;
                state.dataRoomAirMod->ZoneUFADGamma(ZoneNum) = 0.0;
                state.dataRoomAirMod->ZoneUFADPowInPlumes(ZoneNum) = 0.0;
                state.dataRoomAirMod->ZoneUFADPowInPlumesfromWindows(ZoneNum) = 0.0;
            }

            if (state.dataRoomAirMod->IsZoneCrossVent(ZoneNum)) {
                state.dataRoomAirMod->ZTJET(ZoneNum) = 23.0;
                state.dataRoomAirMod->RoomOutflowTemp(ZoneNum) = 23.0;
                state.dataRoomAirMod->ZTREC(ZoneNum) = 23.0;
                state.dataRoomAirMod->CrossVentHcIn = 0.0;
                state.dataRoomAirMod->JetRecAreaRatio(ZoneNum) = 0.2;
                state.dataRoomAirMod->Urec(ZoneNum) = 0.2;
                state.dataRoomAirMod->Ujet(ZoneNum) = 0.2;
                state.dataRoomAirMod->Uhc(ZoneNum) = 0.2;
                state.dataRoomAirMod->Ain(ZoneNum) = 1.0;
                state.dataRoomAirMod->Tin(ZoneNum) = 23.0;
                state.dataRoomAirMod->Droom(ZoneNum) = 6.0;
                state.dataRoomAirMod->Dstar(ZoneNum) = 6.0;
                state.dataRoomAirMod->ZoneCrossVentIsMixing(ZoneNum) = 0.0;
                state.dataRoomAirMod->Rfr(ZoneNum) = 10.0;
                state.dataRoomAirMod->ZoneCrossVentHasREC(ZoneNum) = 1.0;
                state.dataRoomAirMod->HCeiling = 0.0;
                state.dataRoomAirMod->HWall = 0.0;
                state.dataRoomAirMod->HFloor = 0.0;
                state.dataRoomAirMod->HInternal = 0.0;
                state.dataRoomAirMod->HWindow = 0.0;
                state.dataRoomAirMod->HDoor = 0.0;
            }

            state.dataRoomAirModelMgr->MyEnvrnFlag(ZoneNum) = false;
        } // end one time inits

        if (!state.dataGlobal->BeginEnvrnFlag) {
            state.dataRoomAirModelMgr->MyEnvrnFlag(ZoneNum) = true;
        }
    }

    void GetRAFNNodeNum(EnergyPlusData &state,
                        std::string const &RAFNNodeName, // Name of RoomAir:Node:AirflowNetwork
                        int &ZoneNum,                    // The zone number associate with the node name
                        int &RAFNNodeNum,                // RoomAir:Node:AirflowNetwork Number
                        bool &Errorfound                 // true if an error is found (TODO: Useless, RAFNodeNum is 0 when Errorfound is true)
    )
    {

        // FUNCTION INFORMATION:
        //       AUTHOR         Lixing Gu
        //       DATE WRITTEN   November 2014

        // PURPOSE OF THIS FUNCTION:
        // This function looks up the given RoomAirNode name and returns the Zone number and RoomAir node
        // number. If incorrect name is given, errorsfound is returned as true and value is returned
        // as zero.

        // Obtains and Allocates RoomAirSettings : AirflowNetwork
        if (state.dataRoomAirModelMgr->GetAirModelData) {
            GetAirModelDatas(state);
            state.dataRoomAirModelMgr->GetAirModelData = false;
        }

        Errorfound = false;
        RAFNNodeNum = 0;
        for (int I = 1; I <= state.dataGlobal->NumOfZones; ++I) {
            auto const &afnZoneInfo = state.dataRoomAirMod->AFNZoneInfo(I);
            if (afnZoneInfo.NumOfAirNodes > 0) {
                RAFNNodeNum = UtilityRoutines::FindItemInList(RAFNNodeName, afnZoneInfo.Node, afnZoneInfo.NumOfAirNodes);
                if (RAFNNodeNum > 0) {
                    ZoneNum = I;
                    break;
                }
            }
        }

        if (RAFNNodeNum == 0) {
            Errorfound = true;
            ShowSevereError(state,
                            format("Could not find RoomAir:Node:AirflowNetwork number with AirflowNetwork:IntraZone:Node Name='{}", RAFNNodeName));
        }
    }

    bool CheckEquipName(EnergyPlusData &state,
                        std::string const &EquipName, // Equipment Name
                        std::string &SupplyNodeName,  // Supply node name
                        std::string &ReturnNodeName,  // Return node name
                        int TotNumEquip,              // how many of this equipment type
                        DataZoneEquipment::ZoneEquipType zoneEquipType
    )
    {

        // FUNCTION INFORMATION:
        //       AUTHOR         Lixing Gu
        //       DATE WRITTEN   March 2014

        // PURPOSE OF THIS FUNCTION:
        // This function looks up the given RoomAirNode name and returns the Zone number and RoomAir node
        // number.If incorrect name is given, errorsfound is returned as true and value is returned
        // as zero.

        // Using/Aliasing
        using Fans::GetFanOutletNode;

        // Return value
        bool EquipFind; // True if an error is found

        // FUNCTION LOCAL VARIABLE DECLARATIONS:
        int NumAlphas;
        int NumNumbers;
        int Status;              // Used in GetObjectItem
        int MaxNums = 0;         // Maximum number of numeric input fields
        int MaxAlphas = 0;       // Maximum number of alpha input fields
        int TotalArgs = 0;       // Total number of alpha and numeric arguments(max) for a
        Array1D_string Alphas;   // Alpha input items for object
        Array1D<Real64> Numbers; // Numeric input items for object
        bool errorfound;

        NumAlphas = 1;
        NumNumbers = 1;
        EquipFind = false;

        SupplyNodeName = "";

        if (zoneEquipType == DataZoneEquipment::ZoneEquipType::Invalid) return EquipFind;

        std::string_view equipTypeName = DataZoneEquipment::zoneEquipTypeNamesUC[(int)zoneEquipType];
        state.dataInputProcessing->inputProcessor->getObjectDefMaxArgs(state, equipTypeName, TotalArgs, NumAlphas, NumNumbers);

        MaxNums = max(MaxNums, NumNumbers);
        MaxAlphas = max(MaxAlphas, NumAlphas);

        if (MaxNums > NumNumbers) {
            Numbers.allocate(MaxNums);
            Numbers = 0.0;
        } else if (!allocated(Numbers)) {
            Numbers.allocate(MaxNums);
        }

        if (MaxAlphas > NumAlphas) {
            Alphas.allocate(MaxAlphas);
            Alphas = "";
        } else if (!allocated(Alphas)) {
            Alphas.allocate(NumAlphas);
        }

        for (int I = 1; I <= TotNumEquip; ++I) {
            state.dataInputProcessing->inputProcessor->getObjectItem(state, equipTypeName, I, Alphas, NumAlphas, Numbers, NumNumbers, Status);
            if (UtilityRoutines::SameString(Alphas(1), EquipName)) {
                EquipFind = true;
                break;
            }
        }

        switch (zoneEquipType) {
        case DataZoneEquipment::ZoneEquipType::VariableRefrigerantFlowTerminal: { // ZoneHVAC:TerminalUnit : VariableRefrigerantFlow
            SupplyNodeName = Alphas(4);
            ReturnNodeName = "";                                                           // Zone return node
        } break;
        case DataZoneEquipment::ZoneEquipType::EnergyRecoveryVentilator: { // ZoneHVAC : EnergyRecoveryVentilator
            int nodeNum = GetFanOutletNode(state, "Fan:OnOff", Alphas(4), errorfound);
            if (errorfound) {
            }
            SupplyNodeName = state.dataLoopNodes->NodeID(nodeNum);                      // ?????
            ReturnNodeName = "";                                                  // Zone exhaust node
        } break;
        case DataZoneEquipment::ZoneEquipType::FourPipeFanCoil: { // ZoneHVAC : FourPipeFanCoil
            SupplyNodeName = Alphas(6);
            ReturnNodeName = Alphas(5);
        } break;
        case DataZoneEquipment::ZoneEquipType::OutdoorAirUnit: { // ZoneHVAC : OutdoorAirUnit
            SupplyNodeName = Alphas(13);
            ReturnNodeName = Alphas(14);
        } break;
        case DataZoneEquipment::ZoneEquipType::PackagedTerminalAirConditioner: { // ZoneHVAC : PackagedTerminalAirConditioner
            SupplyNodeName = Alphas(4);
            ReturnNodeName = Alphas(3);
        } break;
        case DataZoneEquipment::ZoneEquipType::PackagedTerminalHeatPump: { // ZoneHVAC : PackagedTerminalHeatPump
            SupplyNodeName = Alphas(4);
            ReturnNodeName = Alphas(3);
        } break;
        case DataZoneEquipment::ZoneEquipType::UnitHeater: { // ZoneHVAC : UnitHeater
            SupplyNodeName = Alphas(4);
            ReturnNodeName = Alphas(3);
        } break;
        case DataZoneEquipment::ZoneEquipType::UnitVentilator: { // ZoneHVAC : UnitVentilator
            SupplyNodeName = Alphas(7);
            ReturnNodeName = Alphas(6);
        } break;
        case DataZoneEquipment::ZoneEquipType::VentilatedSlab: { // ZoneHVAC : VentilatedSlab
            SupplyNodeName = Alphas(20);
            ReturnNodeName = Alphas(18);
        } break;
        case DataZoneEquipment::ZoneEquipType::PackagedTerminalHeatPumpWaterToAir: { // ZoneHVAC : WaterToAirHeatPump
            SupplyNodeName = Alphas(4);
            ReturnNodeName = Alphas(3);
        } break;
        case DataZoneEquipment::ZoneEquipType::WindowAirConditioner: { // ZoneHVAC : WindowAirConditioner
            SupplyNodeName = Alphas(4);
            ReturnNodeName = Alphas(3);
        } break;
        case DataZoneEquipment::ZoneEquipType::BaseboardElectric: { // ZoneHVAC : Baseboard : RadiantConvective : Electric
            SupplyNodeName = "";                                                          // convection only
        } break;
        case DataZoneEquipment::ZoneEquipType::BaseboardWater: { // ZoneHVAC : Baseboard : RadiantConvective : Water
            SupplyNodeName = "";
        } break;
        case DataZoneEquipment::ZoneEquipType::BaseboardSteam: { // ZoneHVAC : Baseboard : RadiantConvective : Steam
            SupplyNodeName = "";
        } break;
        case DataZoneEquipment::ZoneEquipType::BaseboardConvectiveElectric: { // ZoneHVAC : Baseboard : Convective : Electric
            SupplyNodeName = "";
        } break;
        case DataZoneEquipment::ZoneEquipType::BaseboardConvectiveWater: { // ZoneHVAC : Baseboard : Convective : Water
            SupplyNodeName = "";
        } break;
        case DataZoneEquipment::ZoneEquipType::HighTemperatureRadiant: { // ZoneHVAC : HighTemperatureRadiant
            SupplyNodeName = "";
        } break;
        case DataZoneEquipment::ZoneEquipType::DehumidifierDX: { // ZoneHVAC : Dehumidifier : DX
            SupplyNodeName = Alphas(4);
            ReturnNodeName = Alphas(3);
        } break;
        case DataZoneEquipment::ZoneEquipType::PurchasedAir: { // ZoneHVAC : IdealLoadsAirSystem
            SupplyNodeName = Alphas(3);
            ReturnNodeName = Alphas(4);
        } break;
        case DataZoneEquipment::ZoneEquipType::RefrigerationChillerSet: { // ZoneHVAC : RefrigerationChillerSet
            SupplyNodeName = Alphas(5);
            ReturnNodeName = Alphas(4);
        } break;
        case DataZoneEquipment::ZoneEquipType::HybridEvaporativeCooler: { // ZoneHVAC : HybridUnitaryAirConditioners
            SupplyNodeName = Alphas(11);
            ReturnNodeName = Alphas(9);
        } break;
        case DataZoneEquipment::ZoneEquipType::ExhaustFan: {      // Fan : ZoneExhaust
            SupplyNodeName = "";                                                      // ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? May not use
        } break;
        case DataZoneEquipment::ZoneEquipType::HeatPumpWaterHeater: { // WaterHeater : HeatPump
            SupplyNodeName = Alphas(8);
            ReturnNodeName = Alphas(7);
            // For AirTerminals, find matching return node later
        } break;
        case DataZoneEquipment::ZoneEquipType::AirTerminalDualDuctConstantVolume: { // AirTerminal : DualDuct : ConstantVolume
            SupplyNodeName = Alphas(1);
            ReturnNodeName = "";
        } break;
        case DataZoneEquipment::ZoneEquipType::AirTerminalDualDuctVAV: { // AirTerminal : DualDuct : VAV
            SupplyNodeName = Alphas(1);
            ReturnNodeName = "";
        } break;
        case DataZoneEquipment::ZoneEquipType::AirTerminalSingleDuctConstantVolumeReheat: { // AirTerminal : SingleDuct : ConstantVolume : Reheat
            SupplyNodeName = Alphas(1);
            ReturnNodeName = "";
        } break;
        case DataZoneEquipment::ZoneEquipType::AirTerminalSingleDuctConstantVolumeNoReheat: { // AirTerminal : SingleDuct :
                                                                                                              // ConstantVolume : NoReheat
            SupplyNodeName = Alphas(4);
            ReturnNodeName = "";
        } break;
        case DataZoneEquipment::ZoneEquipType::AirTerminalSingleDuctVAVReheat: { // AirTerminal : SingleDuct : VAV : Reheat
            SupplyNodeName = Alphas(1);
            ReturnNodeName = "";
        } break;
        case DataZoneEquipment::ZoneEquipType::AirTerminalSingleDuctVAVNoReheat: { // AirTerminal : SingleDuct : VAV : NoReheat
            SupplyNodeName = Alphas(1);
            ReturnNodeName = "";
        } break;
        case DataZoneEquipment::ZoneEquipType::AirTerminalSingleDuctSeriesPIUReheat: { // AirTerminal : SingleDuct : SeriesPIU : Reheat
            SupplyNodeName = Alphas(1);
            ReturnNodeName = "";
        } break;
        case DataZoneEquipment::ZoneEquipType::AirTerminalSingleDuctParallelPIUReheat: { // AirTerminal : SingleDuct : ParallelPIU : Reheat
            SupplyNodeName = Alphas(1);
            ReturnNodeName = "";
        } break;
        case DataZoneEquipment::ZoneEquipType::AirTerminalSingleDuctCAVFourPipeInduction: { // AirTerminal : SingleDuct :
                                                                                                            // ConstantVolume : FourPipeInduction
            SupplyNodeName = Alphas(1);
            ReturnNodeName = "";
        } break;
        case DataZoneEquipment::ZoneEquipType::AirTerminalSingleDuctVAVReheatVariableSpeedFan: { // AirTerminal : SingleDuct : VAV
                                                                                                                 // : Reheat : VariableSpeedFan
            SupplyNodeName = Alphas(1);
            ReturnNodeName = "";
        } break;
        case DataZoneEquipment::ZoneEquipType::AirTerminalSingleDuctVAVHeatAndCoolReheat: { // AirTerminal : SingleDuct : VAV :
                                                                                                            // HeatAndCool : Reheat
            SupplyNodeName = Alphas(1);
            ReturnNodeName = "";
        } break;
        case DataZoneEquipment::ZoneEquipType::AirTerminalSingleDuctVAVHeatAndCoolNoReheat: { // AirTerminal : SingleDuct : VAV :
                                                                                                              // HeatAndCool : NoReheat
            SupplyNodeName = Alphas(1);
            ReturnNodeName = "";
        } break;
        case DataZoneEquipment::ZoneEquipType::AirTerminalSingleDuctConstantVolumeCooledBeam: { // AirTerminal : SingleDuct :
                                                                                                                // ConstantVolume : CooledBeam
            SupplyNodeName = Alphas(5);
            ReturnNodeName = "";
        } break;
        case DataZoneEquipment::ZoneEquipType::AirTerminalDualDuctVAVOutdoorAir: { // AirTerminal : DualDuct : VAV : OutdoorAir
            SupplyNodeName = Alphas(3);
            ReturnNodeName = "";
        } break;
        case DataZoneEquipment::ZoneEquipType::AirLoopHVACReturnAir: { // AirLoopHVACReturnAir
            SupplyNodeName = Alphas(4);                                                //
            ReturnNodeName = "";                                                       //
        } break;
        default: {
            assert(false);
        } break;
                
        } // switch 

        // Need to find a better to handle allocate and deallocate
        if (MaxAlphas > NumAlphas) {
            Alphas.deallocate();
        }
        if (MaxNums > NumNumbers) {
            Numbers.deallocate();
        }

        return EquipFind;
    }

    //*****************************************************************************************

} // namespace RoomAir

} // namespace EnergyPlus
