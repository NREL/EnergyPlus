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
#include <algorithm>
#include <cmath>
#include <limits>

// ObjexxFCL Headers
#include <ObjexxFCL/Array.functions.hh>
#include <ObjexxFCL/Array1D.hh>
#include <ObjexxFCL/Array2D.hh>
#include <ObjexxFCL/Fmath.hh>

// EnergyPlus Headers
#include <AirflowNetwork/Elements.hpp>
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
#include <EnergyPlus/DataUCSDSharedData.hh>
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

namespace EnergyPlus {

namespace RoomAirModelManager {

    // MODULE INFORMATION
    //       AUTHOR         Weixiu Kong
    //       DATE WRITTEN   March 2003
    //       MODIFIED       July 2003, CC
    //                      Aug, 2005, BG
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS MODULE:
    // Contains subroutines for managing the room air models

    // Using/Aliasing
    using namespace DataRoomAirModel;

    bool GetUCSDDVDataFlag(true); // UCSD
    bool GetAirModelData(true);   // Used to "get" all air model data
    bool MyOneTimeFlag(true);

    void clear_state()
    {
        GetUCSDDVDataFlag = true;
        GetAirModelData = true;
        MyOneTimeFlag = true;
    }

    void ManageAirModel(EnergyPlusData &state, int &ZoneNum)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Weixiu Kong
        //       DATE WRITTEN   April 2003
        //       MODIFIED       July 2003, CC
        //                      Jan 2004, CC
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        //     manage room air models.

        // Using/Aliasing
        using CrossVentMgr::ManageUCSDCVModel;
        using DisplacementVentMgr::ManageUCSDDVModel;
        using MundtSimMgr::ManageMundtModel;
        using RoomAirModelAirflowNetwork::SimRoomAirModelAirflowNetwork;
        using RoomAirModelUserTempPattern::ManageUserDefinedPatterns;
        using UFADManager::ManageUCSDUFModels;


        if (GetAirModelData) {
            GetAirModelDatas(state);
            GetAirModelData = false;
        }

        if (state.dataRoomAirMod->UCSDModelUsed) {
            SharedDVCVUFDataInit(state,ZoneNum);
        }

        {
            auto const SELECT_CASE_var(state.dataRoomAirMod->AirModel(ZoneNum).AirModelType);

            if (SELECT_CASE_var == DataRoomAirModel::RoomAirModel::UserDefined) {

                ManageUserDefinedPatterns(state, ZoneNum);

            } else if (SELECT_CASE_var == DataRoomAirModel::RoomAirModel::Mixing) { // Mixing air model
                                                                 // do nothing

            } else if (SELECT_CASE_var == DataRoomAirModel::RoomAirModel::Mundt) { // Mundt air model
                // simulate room airflow using Mundt model
                ManageMundtModel(state, ZoneNum);

            } else if (SELECT_CASE_var == DataRoomAirModel::RoomAirModel::UCSDDV) { // UCDV Displacement Ventilation model
                // simulate room airflow using UCSDDV model
                ManageUCSDDVModel(state, ZoneNum);

            } else if (SELECT_CASE_var == DataRoomAirModel::RoomAirModel::UCSDCV) { // UCSD Cross Ventilation model
                // simulate room airflow using UCSDDV model
                ManageUCSDCVModel(state, ZoneNum);

            } else if (SELECT_CASE_var == DataRoomAirModel::RoomAirModel::UCSDUFI) { // UCSD UFAD interior zone model
                // simulate room airflow using the UCSDUFI model
                ManageUCSDUFModels(state, ZoneNum, DataRoomAirModel::RoomAirModel::UCSDUFI);

            } else if (SELECT_CASE_var == DataRoomAirModel::RoomAirModel::UCSDUFE) { // UCSD UFAD exterior zone model
                // simulate room airflow using the UCSDUFE model
                ManageUCSDUFModels(state, ZoneNum, DataRoomAirModel::RoomAirModel::UCSDUFE);

            } else if (SELECT_CASE_var == DataRoomAirModel::RoomAirModel::AirflowNetwork) { // RoomAirflowNetwork zone model
                // simulate room airflow using the AirflowNetwork - based model
                SimRoomAirModelAirflowNetwork(state, ZoneNum);

            } else { // mixing air model
                     // do nothing
            }
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
        GetCrossVentData(state,ErrorsFound);

        // get BTG's user-defined patterns for all zones
        GetUserDefinedPatternData(state, ErrorsFound);

        // get UCSD UFAD interior zone model controls for all zones
        // get UCSD UFAD exterior zone model controls for all zones
        GetUFADZoneData(state,ErrorsFound);

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
        using namespace DataIPShortCuts;
        using DataHeatBalance::Zone;
        using DataSurfaces::Surface;
        using DataZoneEquipment::EquipConfiguration;

        using RoomAirModelUserTempPattern::FigureNDheightInZone;
        using ScheduleManager::GetScheduleIndex;

        // SUBROUTINE PARAMETER DEFINITIONS:
        static std::string const RoutineName("GetUserDefinedPatternData: ");

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int NumAlphas;  // number of alphas
        int NumNumbers; // Number of numbers encountered
        int Status;     // Notes if there was an error in processing the input

        int thisSurfinZone; // working variable for indexing surfaces within a ZoneRadiantInfo structure
        int thisHBsurfID;   // working variable for indexing surfaces in main Surface structure
        int thisPattern;

        int i;        // do loop indexer
        int NumPairs; // number of zeta/deltaTai pairs
        int ObjNum;   // loop indexer of input objects if the same type
        int ZoneNum;  // zone number in heat balance domain
        int found;    // test for UtilityRoutines::FindItemInList(

        // access input file and setup
        state.dataRoomAirMod->numTempDistContrldZones = inputProcessor->getNumObjectsFound(state, cUserDefinedControlObject);

        state.dataRoomAirMod->NumConstantGradient = inputProcessor->getNumObjectsFound(state, cTempPatternConstGradientObject);
        state.dataRoomAirMod->NumTwoGradientInterp = inputProcessor->getNumObjectsFound(state, cTempPatternTwoGradientObject);
        state.dataRoomAirMod->NumNonDimensionalHeight = inputProcessor->getNumObjectsFound(state, cTempPatternNDHeightObject);
        state.dataRoomAirMod->NumSurfaceMapping = inputProcessor->getNumObjectsFound(state, cTempPatternSurfMapObject);

        state.dataRoomAirMod->NumAirTempPatterns = state.dataRoomAirMod->NumConstantGradient + state.dataRoomAirMod->NumTwoGradientInterp + state.dataRoomAirMod->NumNonDimensionalHeight + state.dataRoomAirMod->NumSurfaceMapping;

        cCurrentModuleObject = cUserDefinedControlObject;
        if (state.dataRoomAirMod->numTempDistContrldZones == 0) {
            if (state.dataRoomAirMod->NumAirTempPatterns != 0) { // user may have missed control object
                ShowWarningError(state, "Missing " + cCurrentModuleObject + " object needed to use roomair temperature patterns");
                // ErrorsFound = .TRUE.
            }
            return;
        }

        // now allocate AirPatternZoneInfo to length of all zones for easy indexing
        if (!allocated(state.dataRoomAirMod->AirPatternZoneInfo)) {
            state.dataRoomAirMod->AirPatternZoneInfo.allocate(state.dataGlobal->NumOfZones);
        }

        for (ObjNum = 1; ObjNum <= state.dataRoomAirMod->numTempDistContrldZones; ++ObjNum) {

            inputProcessor->getObjectItem(state,
                                          cCurrentModuleObject,
                                          ObjNum,
                                          cAlphaArgs,
                                          NumAlphas,
                                          rNumericArgs,
                                          NumNumbers,
                                          Status,
                                          _,
                                          lAlphaFieldBlanks,
                                          cAlphaFieldNames,
                                          cNumericFieldNames);
            // first get zone ID
            ZoneNum = UtilityRoutines::FindItemInList(cAlphaArgs(2), Zone);
            if (ZoneNum == 0) { // throw error
                ShowSevereError(state, RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\", invalid data.");
                ShowContinueError(state, "Invalid-not found " + cAlphaFieldNames(2) + "=\"" + cAlphaArgs(2) + "\".");
                ErrorsFound = true;
                return; // halt to avoid hard crash
            }
            state.dataRoomAirMod->AirPatternZoneInfo(ZoneNum).IsUsed = true;
            state.dataRoomAirMod->AirPatternZoneInfo(ZoneNum).Name = cAlphaArgs(1);     // Name of this Control Object
            state.dataRoomAirMod->AirPatternZoneInfo(ZoneNum).ZoneName = cAlphaArgs(2); // Zone Name

            state.dataRoomAirMod->AirPatternZoneInfo(ZoneNum).AvailSched = cAlphaArgs(3);
            if (lAlphaFieldBlanks(3)) {
                state.dataRoomAirMod->AirPatternZoneInfo(ZoneNum).AvailSchedID = DataGlobalConstants::ScheduleAlwaysOn;
            } else {
                state.dataRoomAirMod->AirPatternZoneInfo(ZoneNum).AvailSchedID = GetScheduleIndex(state, cAlphaArgs(3));
                if (state.dataRoomAirMod->AirPatternZoneInfo(ZoneNum).AvailSchedID == 0) {
                    ShowSevereError(state, RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\", invalid data.");
                    ShowContinueError(state, "Invalid-not found " + cAlphaFieldNames(3) + "=\"" + cAlphaArgs(3) + "\".");
                    ErrorsFound = true;
                }
            }

            state.dataRoomAirMod->AirPatternZoneInfo(ZoneNum).PatternCntrlSched = cAlphaArgs(4); // Schedule Name for Leading Pattern Control for this Zone
            state.dataRoomAirMod->AirPatternZoneInfo(ZoneNum).PatternSchedID = GetScheduleIndex(state, cAlphaArgs(4));
            if (state.dataRoomAirMod->AirPatternZoneInfo(ZoneNum).PatternSchedID == 0) {
                ShowSevereError(state, RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\", invalid data.");
                ShowContinueError(state, "Invalid-not found " + cAlphaFieldNames(4) + "=\"" + cAlphaArgs(4) + "\".");
                ErrorsFound = true;
            }

            state.dataRoomAirMod->AirPatternZoneInfo(ZoneNum).ZoneID = ZoneNum;

            //   figure number of surfaces for this zone
            state.dataRoomAirMod->AirPatternZoneInfo(ZoneNum).totNumSurfs = Zone(ZoneNum).SurfaceLast - Zone(ZoneNum).SurfaceFirst + 1;
            //   allocate nested derived type for surface info
            state.dataRoomAirMod->AirPatternZoneInfo(ZoneNum).Surf.allocate(state.dataRoomAirMod->AirPatternZoneInfo(ZoneNum).totNumSurfs);

            //   Fill in what we know for nested structure for surfaces
            for (thisSurfinZone = 1; thisSurfinZone <= state.dataRoomAirMod->AirPatternZoneInfo(ZoneNum).totNumSurfs; ++thisSurfinZone) {
                thisHBsurfID = Zone(ZoneNum).SurfaceFirst + thisSurfinZone - 1;
                if (Surface(thisHBsurfID).Class == DataSurfaces::SurfaceClass::IntMass) {
                    state.dataRoomAirMod->AirPatternZoneInfo(ZoneNum).Surf(thisSurfinZone).SurfID = thisHBsurfID;
                    state.dataRoomAirMod->AirPatternZoneInfo(ZoneNum).Surf(thisSurfinZone).Zeta = 0.5;
                    continue;
                }

                state.dataRoomAirMod->AirPatternZoneInfo(ZoneNum).Surf(thisSurfinZone).SurfID = thisHBsurfID;

                state.dataRoomAirMod->AirPatternZoneInfo(ZoneNum).Surf(thisSurfinZone).Zeta = FigureNDheightInZone(state, thisHBsurfID);

            } // loop through surfaces in this zone

        } // loop through number of 'RoomAir:TemperaturePattern:UserDefined' objects

        // Check against AirModel.  Make sure there is a match here.
        for (ZoneNum = 1; ZoneNum <= state.dataGlobal->NumOfZones; ++ZoneNum) {
            if (state.dataRoomAirMod->AirModel(ZoneNum).AirModelType != DataRoomAirModel::RoomAirModel::UserDefined) continue;
            if (state.dataRoomAirMod->AirPatternZoneInfo(ZoneNum).IsUsed) continue; // There is a Room Air Temperatures object for this zone
            ShowSevereError(state, RoutineName + "AirModel for Zone=[" + Zone(ZoneNum).Name + "] is indicated as \"User Defined\".");
            ShowContinueError(state, "...but missing a " + cCurrentModuleObject + " object for control.");
            ErrorsFound = true;
        }

        // now get user defined temperature patterns
        if (!allocated(state.dataRoomAirMod->RoomAirPattern)) {
            state.dataRoomAirMod->RoomAirPattern.allocate(state.dataRoomAirMod->NumAirTempPatterns);
        }

        // Four different objects to get
        cCurrentModuleObject = cTempPatternConstGradientObject;
        for (ObjNum = 1; ObjNum <= state.dataRoomAirMod->NumConstantGradient; ++ObjNum) {
            thisPattern = ObjNum;
            inputProcessor->getObjectItem(
                state, cCurrentModuleObject, ObjNum, cAlphaArgs, NumAlphas, rNumericArgs, NumNumbers, Status, _, _, cAlphaFieldNames, cNumericFieldNames);

            state.dataRoomAirMod->RoomAirPattern(thisPattern).Name = cAlphaArgs(1);
            state.dataRoomAirMod->RoomAirPattern(thisPattern).PatrnID = rNumericArgs(1);
            state.dataRoomAirMod->RoomAirPattern(thisPattern).PatternMode = DataRoomAirModel::UserDefinedPatternType::ConstGradTempPattern;
            state.dataRoomAirMod->RoomAirPattern(thisPattern).DeltaTstat = rNumericArgs(2);
            state.dataRoomAirMod->RoomAirPattern(thisPattern).DeltaTleaving = rNumericArgs(3);
            state.dataRoomAirMod->RoomAirPattern(thisPattern).DeltaTexhaust = rNumericArgs(4);
            state.dataRoomAirMod->RoomAirPattern(thisPattern).GradPatrn.Gradient = rNumericArgs(5);
        }

        cCurrentModuleObject = cTempPatternTwoGradientObject;
        for (ObjNum = 1; ObjNum <= state.dataRoomAirMod->NumTwoGradientInterp; ++ObjNum) {
            thisPattern = state.dataRoomAirMod->NumConstantGradient + ObjNum;
            inputProcessor->getObjectItem(
                state, cCurrentModuleObject, ObjNum, cAlphaArgs, NumAlphas, rNumericArgs, NumNumbers, Status, _, _, cAlphaFieldNames, cNumericFieldNames);
            state.dataRoomAirMod->RoomAirPattern(thisPattern).PatternMode = DataRoomAirModel::UserDefinedPatternType::TwoGradInterpPattern;
            state.dataRoomAirMod->RoomAirPattern(thisPattern).Name = cAlphaArgs(1);
            state.dataRoomAirMod->RoomAirPattern(thisPattern).PatrnID = rNumericArgs(1);
            state.dataRoomAirMod->RoomAirPattern(thisPattern).TwoGradPatrn.TstatHeight = rNumericArgs(2);
            state.dataRoomAirMod->RoomAirPattern(thisPattern).TwoGradPatrn.TleavingHeight = rNumericArgs(3);
            state.dataRoomAirMod->RoomAirPattern(thisPattern).TwoGradPatrn.TexhaustHeight = rNumericArgs(4);
            state.dataRoomAirMod->RoomAirPattern(thisPattern).TwoGradPatrn.LowGradient = rNumericArgs(5);
            state.dataRoomAirMod->RoomAirPattern(thisPattern).TwoGradPatrn.HiGradient = rNumericArgs(6);

            if (UtilityRoutines::SameString(cAlphaArgs(2), "OutdoorDryBulbTemperature")) {
                state.dataRoomAirMod->RoomAirPattern(thisPattern).TwoGradPatrn.InterpolationMode = DataRoomAirModel::UserDefinedPatternMode::OutdoorDryBulbMode;
            } else if (UtilityRoutines::SameString(cAlphaArgs(2), "ZoneDryBulbTemperature")) {
                state.dataRoomAirMod->RoomAirPattern(thisPattern).TwoGradPatrn.InterpolationMode = DataRoomAirModel::UserDefinedPatternMode::ZoneAirTempMode;
            } else if (UtilityRoutines::SameString(cAlphaArgs(2), "ZoneAndOutdoorTemperatureDifference")) {
                state.dataRoomAirMod->RoomAirPattern(thisPattern).TwoGradPatrn.InterpolationMode = DataRoomAirModel::UserDefinedPatternMode::DeltaOutdoorZone;
            } else if (UtilityRoutines::SameString(cAlphaArgs(2), "SensibleCoolingLoad")) {
                state.dataRoomAirMod->RoomAirPattern(thisPattern).TwoGradPatrn.InterpolationMode = DataRoomAirModel::UserDefinedPatternMode::SensibleCoolingMode;
            } else if (UtilityRoutines::SameString(cAlphaArgs(2), "SensibleHeatingLoad")) {
                state.dataRoomAirMod->RoomAirPattern(thisPattern).TwoGradPatrn.InterpolationMode = DataRoomAirModel::UserDefinedPatternMode::SensibleHeatingMode;
            } else {
                ShowSevereError(state, "Invalid " + cAlphaFieldNames(2) + " = " + cAlphaArgs(2));
                ShowContinueError(state, "Entered in " + cCurrentModuleObject + " = " + cAlphaArgs(1));
                ErrorsFound = true;
            }

            state.dataRoomAirMod->RoomAirPattern(thisPattern).TwoGradPatrn.UpperBoundTempScale = rNumericArgs(7);
            state.dataRoomAirMod->RoomAirPattern(thisPattern).TwoGradPatrn.LowerBoundTempScale = rNumericArgs(8);

            state.dataRoomAirMod->RoomAirPattern(thisPattern).TwoGradPatrn.UpperBoundHeatRateScale = rNumericArgs(9);
            state.dataRoomAirMod->RoomAirPattern(thisPattern).TwoGradPatrn.LowerBoundHeatRateScale = rNumericArgs(10);

            // now test the input some
            if (state.dataRoomAirMod->RoomAirPattern(thisPattern).TwoGradPatrn.HiGradient == state.dataRoomAirMod->RoomAirPattern(thisPattern).TwoGradPatrn.LowGradient) {
                ShowWarningError(state, format("Upper and lower gradients equal, use {} instead ", cTempPatternConstGradientObject));
                ShowContinueError(state, "Entered in " + cCurrentModuleObject + " = " + cAlphaArgs(1));
            }
            if ((state.dataRoomAirMod->RoomAirPattern(thisPattern).TwoGradPatrn.UpperBoundTempScale == state.dataRoomAirMod->RoomAirPattern(thisPattern).TwoGradPatrn.LowerBoundTempScale) &&
                ((state.dataRoomAirMod->RoomAirPattern(thisPattern).TwoGradPatrn.InterpolationMode == DataRoomAirModel::UserDefinedPatternMode::OutdoorDryBulbMode) ||
                 (state.dataRoomAirMod->RoomAirPattern(thisPattern).TwoGradPatrn.InterpolationMode == DataRoomAirModel::UserDefinedPatternMode::ZoneAirTempMode) ||
                 (state.dataRoomAirMod->RoomAirPattern(thisPattern).TwoGradPatrn.InterpolationMode == DataRoomAirModel::UserDefinedPatternMode::DeltaOutdoorZone))) {
                // throw error, will cause divide by zero when used for scaling
                ShowSevereError(state, "Error in temperature scale in " + cCurrentModuleObject + ": " + cAlphaArgs(1));
                ErrorsFound = true;
            }
            if ((state.dataRoomAirMod->RoomAirPattern(thisPattern).TwoGradPatrn.HiGradient == state.dataRoomAirMod->RoomAirPattern(thisPattern).TwoGradPatrn.LowGradient) &&
                ((state.dataRoomAirMod->RoomAirPattern(thisPattern).TwoGradPatrn.InterpolationMode == DataRoomAirModel::UserDefinedPatternMode::SensibleCoolingMode) ||
                 (state.dataRoomAirMod->RoomAirPattern(thisPattern).TwoGradPatrn.InterpolationMode == DataRoomAirModel::UserDefinedPatternMode::SensibleHeatingMode))) {
                // throw error, will cause divide by zero when used for scaling
                ShowSevereError(state, "Error in load scale in " + cCurrentModuleObject + ": " + cAlphaArgs(1));
                ErrorsFound = true;
            }
        }

        cCurrentModuleObject = cTempPatternNDHeightObject;
        for (ObjNum = 1; ObjNum <= state.dataRoomAirMod->NumNonDimensionalHeight; ++ObjNum) {
            thisPattern = state.dataRoomAirMod->NumConstantGradient + state.dataRoomAirMod->NumTwoGradientInterp + ObjNum;
            state.dataRoomAirMod->RoomAirPattern(thisPattern).PatternMode = DataRoomAirModel::UserDefinedPatternType::NonDimenHeightPattern;

            inputProcessor->getObjectItem(
                state, cCurrentModuleObject, ObjNum, cAlphaArgs, NumAlphas, rNumericArgs, NumNumbers, Status, _, _, cAlphaFieldNames, cNumericFieldNames);
            state.dataRoomAirMod->RoomAirPattern(thisPattern).Name = cAlphaArgs(1);
            state.dataRoomAirMod->RoomAirPattern(thisPattern).PatrnID = rNumericArgs(1);
            state.dataRoomAirMod->RoomAirPattern(thisPattern).DeltaTstat = rNumericArgs(2);
            state.dataRoomAirMod->RoomAirPattern(thisPattern).DeltaTleaving = rNumericArgs(3);
            state.dataRoomAirMod->RoomAirPattern(thisPattern).DeltaTexhaust = rNumericArgs(4);

            NumPairs = std::floor((double(NumNumbers) - 4.0) / 2.0);

            // TODO error checking

            state.dataRoomAirMod->RoomAirPattern(thisPattern).VertPatrn.ZetaPatrn.allocate(NumPairs);
            state.dataRoomAirMod->RoomAirPattern(thisPattern).VertPatrn.DeltaTaiPatrn.allocate(NumPairs);

            // init these since they can't be in derived type
            state.dataRoomAirMod->RoomAirPattern(thisPattern).VertPatrn.ZetaPatrn = 0.0;
            state.dataRoomAirMod->RoomAirPattern(thisPattern).VertPatrn.DeltaTaiPatrn = 0.0;

            for (i = 0; i <= NumPairs - 1; ++i) {

                state.dataRoomAirMod->RoomAirPattern(thisPattern).VertPatrn.ZetaPatrn(i + 1) = rNumericArgs(2 * i + 5);
                state.dataRoomAirMod->RoomAirPattern(thisPattern).VertPatrn.DeltaTaiPatrn(i + 1) = rNumericArgs(2 * i + 6);
            }

            // TODO  check order (TODO sort ? )
            for (i = 2; i <= NumPairs; ++i) {
                if (state.dataRoomAirMod->RoomAirPattern(thisPattern).VertPatrn.ZetaPatrn(i) < state.dataRoomAirMod->RoomAirPattern(thisPattern).VertPatrn.ZetaPatrn(i - 1)) {
                    ShowSevereError(state, "Zeta values not in increasing order in " + cCurrentModuleObject + ": " + cAlphaArgs(1));
                    ErrorsFound = true;
                }
            }
        }

        cCurrentModuleObject = cTempPatternSurfMapObject;
        for (ObjNum = 1; ObjNum <= state.dataRoomAirMod->NumSurfaceMapping; ++ObjNum) {
            thisPattern = state.dataRoomAirMod->NumConstantGradient + state.dataRoomAirMod->NumTwoGradientInterp + state.dataRoomAirMod->NumNonDimensionalHeight + ObjNum;
            state.dataRoomAirMod->RoomAirPattern(thisPattern).PatternMode = DataRoomAirModel::UserDefinedPatternType::SurfMapTempPattern;

            inputProcessor->getObjectItem(
                state, cCurrentModuleObject, ObjNum, cAlphaArgs, NumAlphas, rNumericArgs, NumNumbers, Status, _, _, cAlphaFieldNames, cNumericFieldNames);
            state.dataRoomAirMod->RoomAirPattern(thisPattern).Name = cAlphaArgs(1);
            state.dataRoomAirMod->RoomAirPattern(thisPattern).PatrnID = rNumericArgs(1);
            state.dataRoomAirMod->RoomAirPattern(thisPattern).DeltaTstat = rNumericArgs(2);
            state.dataRoomAirMod->RoomAirPattern(thisPattern).DeltaTleaving = rNumericArgs(3);
            state.dataRoomAirMod->RoomAirPattern(thisPattern).DeltaTexhaust = rNumericArgs(4);

            NumPairs = NumNumbers - 4;

            if (NumPairs != (NumAlphas - 1)) {
                ShowSevereError(state, "Error in number of entries in " + cCurrentModuleObject + " object: " + cAlphaArgs(1));
                ErrorsFound = true;
            }
            state.dataRoomAirMod->RoomAirPattern(thisPattern).MapPatrn.SurfName.allocate(NumPairs);
            state.dataRoomAirMod->RoomAirPattern(thisPattern).MapPatrn.DeltaTai.allocate(NumPairs);
            state.dataRoomAirMod->RoomAirPattern(thisPattern).MapPatrn.SurfID.allocate(NumPairs);

            // init just allocated
            state.dataRoomAirMod->RoomAirPattern(thisPattern).MapPatrn.SurfName = "";
            state.dataRoomAirMod->RoomAirPattern(thisPattern).MapPatrn.DeltaTai = 0.0;
            state.dataRoomAirMod->RoomAirPattern(thisPattern).MapPatrn.SurfID = 0;

            for (i = 1; i <= NumPairs; ++i) {
                state.dataRoomAirMod->RoomAirPattern(thisPattern).MapPatrn.SurfName(i) = cAlphaArgs(i + 1);
                state.dataRoomAirMod->RoomAirPattern(thisPattern).MapPatrn.DeltaTai(i) = rNumericArgs(i + 4);
                found = UtilityRoutines::FindItemInList(cAlphaArgs(i + 1), Surface);
                if (found != 0) {
                    state.dataRoomAirMod->RoomAirPattern(thisPattern).MapPatrn.SurfID(i) = found;
                } else {
                    ShowSevereError(state, "Surface name not found in " + cCurrentModuleObject + " object: " + cAlphaArgs(1));
                    ErrorsFound = true;
                }
            }
            state.dataRoomAirMod->RoomAirPattern(thisPattern).MapPatrn.NumSurfs = NumPairs;
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

        for (i = 1; i <= state.dataGlobal->NumOfZones; ++i) {
            if (state.dataRoomAirMod->AirPatternZoneInfo(i).IsUsed) {
                // first get return and exhaust air node index
                found = UtilityRoutines::FindItemInList(state.dataRoomAirMod->AirPatternZoneInfo(i).ZoneName, state.dataZoneEquip->ZoneEquipConfig, &EquipConfiguration::ZoneName);
                if (found != 0) {

                    state.dataRoomAirMod->AirPatternZoneInfo(i).ZoneNodeID = state.dataZoneEquip->ZoneEquipConfig(found).ZoneNode;
                    if (allocated(state.dataZoneEquip->ZoneEquipConfig(found).ExhaustNode)) {
                        state.dataRoomAirMod->AirPatternZoneInfo(i).ExhaustAirNodeID.allocate(state.dataZoneEquip->ZoneEquipConfig(found).NumExhaustNodes);
                        state.dataRoomAirMod->AirPatternZoneInfo(i).ExhaustAirNodeID = state.dataZoneEquip->ZoneEquipConfig(found).ExhaustNode;
                    } // exhaust nodes present
                }     // found ZoneEquipConf

                // second get zone height values
                state.dataRoomAirMod->AirPatternZoneInfo(i).ZoneHeight = Zone(i).CeilingHeight;

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

        // METHODOLOGY EMPLOYED:
        //     Use input processer to get input from idf file

        // Using/Aliasing
        using namespace DataIPShortCuts;
        using DataHeatBalance::Zone;
        using DataSurfaces::Surface;

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int NumAlphas; // States which alpha value to read from a
        // "Number" line
        int NumNumbers;       // Number of numbers encountered
        int Status;           // Notes if there was an error in processing the input
        int AirNodeNum;       // Index number for air nodes
        int ZoneNum;          // Index number for zones
        int NumSurfsInvolved; // Number of surfaces involved with air nodes
        int SurfCount;        // Number of surfaces involved with air nodes
        // (used for checking error)
        int SurfNum;     // Index number for surfaces
        int SurfFirst;   // Index number for first surface of zones
        int NumOfSurfs;  // Index number for last surface of zones
        int ListSurfNum; // Index number of surfaces listed in the air node object
        bool SurfNeeded;



        if (!state.dataRoomAirMod->MundtModelUsed) return;

        // Initialize default values for air nodes
        state.dataRoomAirMod->TotNumOfZoneAirNodes.allocate(state.dataGlobal->NumOfZones);
        state.dataRoomAirMod->TotNumOfAirNodes = 0;
        state.dataRoomAirMod->TotNumOfZoneAirNodes = 0;

        cCurrentModuleObject = "RoomAir:Node";
        state.dataRoomAirMod->TotNumOfAirNodes = inputProcessor->getNumObjectsFound(state, cCurrentModuleObject);

        if (state.dataRoomAirMod->TotNumOfAirNodes <= 0) {
            // no air node object is found, terminate the program
            ShowSevereError(state, "No " + cCurrentModuleObject + " objects found in input.");
            ShowContinueError(state, "The OneNodeDisplacementVentilation model requires " + cCurrentModuleObject + " objects");
            ErrorsFound = true;
            return;
        } else {
            // air node objects are found so allocate airnode variable
            state.dataRoomAirMod->AirNode.allocate(state.dataRoomAirMod->TotNumOfAirNodes);
        }

        for (AirNodeNum = 1; AirNodeNum <= state.dataRoomAirMod->TotNumOfAirNodes; ++AirNodeNum) {

            // get air node objects
            inputProcessor->getObjectItem(state,
                                          cCurrentModuleObject,
                                          AirNodeNum,
                                          cAlphaArgs,
                                          NumAlphas,
                                          rNumericArgs,
                                          NumNumbers,
                                          Status,
                                          _,
                                          _,
                                          cAlphaFieldNames,
                                          cNumericFieldNames);
            UtilityRoutines::IsNameEmpty(state, cAlphaArgs(1), cCurrentModuleObject, ErrorsFound);

            state.dataRoomAirMod->AirNode(AirNodeNum).Name = cAlphaArgs(1);

            state.dataRoomAirMod->AirNode(AirNodeNum).ZoneName = cAlphaArgs(3); // Zone name
            state.dataRoomAirMod->AirNode(AirNodeNum).ZonePtr = UtilityRoutines::FindItemInList(state.dataRoomAirMod->AirNode(AirNodeNum).ZoneName, Zone);
            if (state.dataRoomAirMod->AirNode(AirNodeNum).ZonePtr == 0) {
                ShowSevereError(state, "Invalid " + cAlphaFieldNames(3) + " = " + cAlphaArgs(3));
                ShowContinueError(state, "Entered in " + cCurrentModuleObject + " = " + cAlphaArgs(1));
                ErrorsFound = true;
            } else {
                ZoneNum = state.dataRoomAirMod->AirNode(AirNodeNum).ZonePtr;
                NumOfSurfs = Zone(ZoneNum).SurfaceLast - Zone(ZoneNum).SurfaceFirst + 1;
                state.dataRoomAirMod->AirNode(AirNodeNum).SurfMask.allocate(NumOfSurfs);
            }

            {
                auto const nodeType(cAlphaArgs(2));
                if (nodeType == "INLET") {
                    state.dataRoomAirMod->AirNode(AirNodeNum).ClassType = InletAirNode;
                } else if (nodeType == "FLOOR") {
                    state.dataRoomAirMod->AirNode(AirNodeNum).ClassType = FloorAirNode;
                } else if (nodeType == "CONTROL") {
                    state.dataRoomAirMod->AirNode(AirNodeNum).ClassType = ControlAirNode;
                } else if (nodeType == "CEILING") {
                    state.dataRoomAirMod->AirNode(AirNodeNum).ClassType = CeilingAirNode;
                } else if (nodeType == "MUNDTROOM") {
                    state.dataRoomAirMod->AirNode(AirNodeNum).ClassType = MundtRoomAirNode;
                } else if (nodeType == "RETURN") {
                    state.dataRoomAirMod->AirNode(AirNodeNum).ClassType = ReturnAirNode;
                    //            CASE ('PLUME1')
                    //                AirNode(AirNodeNum)%ClassType   = PlumeAirNode1
                    //            CASE ('PLUME2')
                    //                AirNode(AirNodeNum)%ClassType   = PlumeAirNode2
                    //            CASE ('PLUME3')
                    //                AirNode(AirNodeNum)%ClassType   = PlumeAirNode3
                    //            CASE ('PLUME4')
                    //                AirNode(AirNodeNum)%ClassType   = PlumeAirNode4
                    //            CASE ('REESROOM1')
                    //                AirNode(AirNodeNum)%ClassType   = RoomAirNode1
                    //            CASE ('REESROOM2')
                    //                AirNode(AirNodeNum)%ClassType   = RoomAirNode2
                    //            CASE ('REESROOM3')
                    //                AirNode(AirNodeNum)%ClassType   = RoomAirNode3
                    //            CASE ('REESROOM4')
                    //                AirNode(AirNodeNum)%ClassType   = RoomAirNode4
                } else {
                    ShowSevereError(state, "Invalid " + cAlphaFieldNames(2) + " = " + cAlphaArgs(2));
                    ShowContinueError(state, "Entered in " + cCurrentModuleObject + " = " + cAlphaArgs(1));
                    ErrorsFound = true;
                }
            }

            state.dataRoomAirMod->AirNode(AirNodeNum).Height = rNumericArgs(1); // Air node height
            NumSurfsInvolved = NumAlphas - 3;             // Number of surfaces involved with air nodes

            // Initialize
            state.dataRoomAirMod->AirNode(AirNodeNum).SurfMask = false;

            if (NumSurfsInvolved <= 0) {

                // report severe error since the following air nodes require surfaces associated with them
                {
                    auto const nodeType(cAlphaArgs(2));
                    if (nodeType == "FLOOR" || nodeType == "CEILING" || nodeType == "MUNDTROOM" || nodeType == "PLUME4" || nodeType == "REESROOM1" ||
                        nodeType == "REESROOM2" || nodeType == "REESROOM3" || nodeType == "REESROOM4") {
                        // terminate the program due to a severe error in the specified input
                        ShowSevereError(state, "GetAirNodeData: " + cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\" invalid air node specification.");
                        ShowContinueError(state, "Mundt Room Air Model: No surface names specified.  Air node=\"" + state.dataRoomAirMod->AirNode(AirNodeNum).Name +
                                          " requires name of surfaces associated with it.");
                        ErrorsFound = true;
                    } else {
                    }
                }

            } else {

                // initialize
                SurfNeeded = true;

                // report warning error since the following air nodes do not require surfaces associated with them
                // and assign .FALSE. to 'SurfNeeded'
                {
                    auto const nodeType(cAlphaArgs(2));
                    if (nodeType == "INLET" || nodeType == "CONTROL" || nodeType == "RETURN" || nodeType == "PLUME1" || nodeType == "PLUME2" ||
                        nodeType == "PLUME3") {
                        ShowWarningError(state, "GetAirNodeData: " + cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\" invalid linkage");
                        ShowContinueError(state, "Mundt Room Air Model: No surface names needed.  Air node=\"" + state.dataRoomAirMod->AirNode(AirNodeNum).Name +
                                          " does not relate to any surfaces.");
                        SurfNeeded = false;
                    } else {
                    }
                }

                if (SurfNeeded) {

                    // this air node is in this zone; hence, first get name of all surfaces in this zone
                    ZoneNum = state.dataRoomAirMod->AirNode(AirNodeNum).ZonePtr;
                    SurfFirst = Zone(ZoneNum).SurfaceFirst;
                    NumOfSurfs = Zone(ZoneNum).SurfaceLast - Zone(ZoneNum).SurfaceFirst + 1;

                    // terminate the program due to a severe error in the specified input
                    if ((NumSurfsInvolved) > NumOfSurfs) {
                        ShowFatalError(state, "GetAirNodeData: Mundt Room Air Model: Number of surfaces connected to " + state.dataRoomAirMod->AirNode(AirNodeNum).Name +
                                       " is greater than number of surfaces in " + Zone(ZoneNum).Name);
                        return;
                    }

                    // relate surfaces to this air node and check to see whether surface names are specified correctly or not
                    SurfCount = 0;
                    --SurfFirst;
                    for (ListSurfNum = 4; ListSurfNum <= NumAlphas; ++ListSurfNum) {
                        for (SurfNum = 1; SurfNum <= NumOfSurfs; ++SurfNum) {
                            if (cAlphaArgs(ListSurfNum) == Surface(SurfFirst + SurfNum).Name) {
                                state.dataRoomAirMod->AirNode(AirNodeNum).SurfMask(SurfNum) = true;
                                ++SurfCount;
                            }
                        }
                    }

                    // report warning error since surface names are specified correctly
                    if ((NumSurfsInvolved) != SurfCount) {
                        ShowWarningError(state, "GetAirNodeData: Mundt Room Air Model: Some surface names specified for " + state.dataRoomAirMod->AirNode(AirNodeNum).Name +
                                         " are not in " + Zone(ZoneNum).Name);
                    }
                }
            }
        }

        // get number of air nodes in each zone
        for (ZoneNum = 1; ZoneNum <= state.dataGlobal->NumOfZones; ++ZoneNum) {

            // this zone uses other air model so skip the rest
            if (state.dataRoomAirMod->AirModel(ZoneNum).AirModelType != DataRoomAirModel::RoomAirModel::Mundt) continue;

            // this zone uses a nodal air model so get number of air nodes in each zone
            for (AirNodeNum = 1; AirNodeNum <= state.dataRoomAirMod->TotNumOfAirNodes; ++AirNodeNum) {
                if (UtilityRoutines::SameString(state.dataRoomAirMod->AirNode(AirNodeNum).ZoneName, Zone(ZoneNum).Name)) {
                    ++state.dataRoomAirMod->TotNumOfZoneAirNodes(ZoneNum);
                }
            }
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
        using namespace DataIPShortCuts;
        using DataHeatBalance::Zone;

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int NumAlphas;
        int NumNumbers;       // Number of numbers encountered
        int Status;           // Notes if there was an error in processing the input
        int ControlNum;       // Index number
        int NumOfMundtContrl; // Number of Mundt Model Controls
        int ZoneNum;          // Index number for zones



        if (!state.dataRoomAirMod->MundtModelUsed) return;

        // Initialize default values for Mundt model controls
        state.dataRoomAirMod->ConvectiveFloorSplit.allocate(state.dataGlobal->NumOfZones);
        state.dataRoomAirMod->InfiltratFloorSplit.allocate(state.dataGlobal->NumOfZones);
        state.dataRoomAirMod->ConvectiveFloorSplit = 0.0;
        state.dataRoomAirMod->InfiltratFloorSplit = 0.0;

        cCurrentModuleObject = "RoomAirSettings:OneNodeDisplacementVentilation";
        NumOfMundtContrl = inputProcessor->getNumObjectsFound(state, cCurrentModuleObject);
        if (NumOfMundtContrl > state.dataGlobal->NumOfZones) {
            ShowSevereError(state, "Too many " + cCurrentModuleObject + " objects in input file");
            ShowContinueError(state, "There cannot be more " + cCurrentModuleObject + " objects than number of zones.");
            ErrorsFound = true;
        }

        if (NumOfMundtContrl == 0) {
            ShowWarningError(state, "No " + cCurrentModuleObject + " objects found, program assumes no convection or infiltration gains near floors");
            return;
        }

        // this zone uses Mundt model so get Mundt Model Control
        // loop through all 'RoomAirSettings:OneNodeDisplacementVentilation' objects
        for (ControlNum = 1; ControlNum <= NumOfMundtContrl; ++ControlNum) {
            inputProcessor->getObjectItem(state,
                                          cCurrentModuleObject,
                                          ControlNum,
                                          cAlphaArgs,
                                          NumAlphas,
                                          rNumericArgs,
                                          NumNumbers,
                                          Status,
                                          _,
                                          _,
                                          cAlphaFieldNames,
                                          cNumericFieldNames);
            ZoneNum = UtilityRoutines::FindItemInList(cAlphaArgs(1), Zone);
            if (ZoneNum == 0) {
                ShowSevereError(state, "Invalid " + cAlphaFieldNames(1) + " = " + cAlphaArgs(1));
                ShowContinueError(state, "Entered in " + cCurrentModuleObject + " = " + cAlphaArgs(1));
                ShowContinueError(state, "Not a valid zone name.");
                ErrorsFound = true;
                continue;
            }
            if (state.dataRoomAirMod->AirModel(ZoneNum).AirModelType != DataRoomAirModel::RoomAirModel::Mundt) {
                ShowSevereError(state, "Zone specified=\"" + cAlphaArgs(1) + "\", Air Model type is not OneNodeDisplacementVentilation.");
                ShowContinueError(state, format("Air Model Type for zone={}", ChAirModel[static_cast<int>(state.dataRoomAirMod->AirModel(ZoneNum).AirModelType)]));
                ErrorsFound = true;
                continue;
            }
            state.dataRoomAirMod->ConvectiveFloorSplit(ZoneNum) = rNumericArgs(1);
            state.dataRoomAirMod->InfiltratFloorSplit(ZoneNum) = rNumericArgs(2);
        }
    }

    void GetDisplacementVentData(EnergyPlusData &state, bool &ErrorsFound) // True if errors found during this get input routine
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         G. Carrilho da Graca
        //       DATE WRITTEN   January 2004
        //       MODIFIED
        //       RE-ENGINEERED

        // PURPOSE OF THIS SUBROUTINE:
        //  Get UCSD Displacement ventilation model controls for all zones at once

        // METHODOLOGY EMPLOYED:
        // Use input processor to get input from idf file

        // Using/Aliasing
        using namespace DataIPShortCuts;
        using DataHeatBalance::Zone;
        using namespace ScheduleManager;

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int IOStat;
        int NumAlpha;
        int NumNumber;
        int Loop;

        if (!state.dataRoomAirMod->UCSDModelUsed) return;
        cCurrentModuleObject = "RoomAirSettings:ThreeNodeDisplacementVentilation";
        state.dataRoomAirMod->TotUCSDDV = inputProcessor->getNumObjectsFound(state, cCurrentModuleObject);

        if (state.dataRoomAirMod->TotUCSDDV <= 0) return;

        state.dataRoomAirMod->ZoneUCSDDV.allocate(state.dataRoomAirMod->TotUCSDDV);

        for (Loop = 1; Loop <= state.dataRoomAirMod->TotUCSDDV; ++Loop) {

            inputProcessor->getObjectItem(state,
                                          cCurrentModuleObject,
                                          Loop,
                                          cAlphaArgs,
                                          NumAlpha,
                                          rNumericArgs,
                                          NumNumber,
                                          IOStat,
                                          _,
                                          lAlphaFieldBlanks,
                                          cAlphaFieldNames,
                                          cNumericFieldNames);
            // First is Zone Name
            state.dataRoomAirMod->ZoneUCSDDV(Loop).ZoneName = cAlphaArgs(1);
            state.dataRoomAirMod->ZoneUCSDDV(Loop).ZonePtr = UtilityRoutines::FindItemInList(cAlphaArgs(1), Zone);
            if (state.dataRoomAirMod->ZoneUCSDDV(Loop).ZonePtr == 0) {
                ShowSevereError(state, "Invalid " + cAlphaFieldNames(1) + " = " + cAlphaArgs(1));
                ShowContinueError(state, "Entered in " + cCurrentModuleObject + " = " + cAlphaArgs(1));
                ShowContinueError(state, "Zone Name not found.");
                ErrorsFound = true;
            } else {
                state.dataRoomAirMod->IsZoneDV(state.dataRoomAirMod->ZoneUCSDDV(Loop).ZonePtr) = true;
            }
            // Second Alpha is Schedule Name
            state.dataRoomAirMod->ZoneUCSDDV(Loop).SchedGainsName = cAlphaArgs(2);
            state.dataRoomAirMod->ZoneUCSDDV(Loop).SchedGainsPtr = GetScheduleIndex(state, cAlphaArgs(2));
            if (state.dataRoomAirMod->ZoneUCSDDV(Loop).SchedGainsPtr == 0) {
                if (lAlphaFieldBlanks(2)) {
                    ShowSevereError(state, "Invalid " + cAlphaFieldNames(2) + " = " + cAlphaArgs(2));
                    ShowContinueError(state, "Entered in " + cCurrentModuleObject + " = " + cAlphaArgs(1));
                    ShowContinueError(state, " Schedule name must be input.");
                    ErrorsFound = true;
                } else {
                    ShowSevereError(state, "Invalid " + cAlphaFieldNames(2) + " = " + cAlphaArgs(2));
                    ShowContinueError(state, "Entered in " + cCurrentModuleObject + " = " + cAlphaArgs(1));
                    ShowContinueError(state, "Schedule name was not found.");
                    ErrorsFound = true;
                }
            }

            state.dataRoomAirMod->ZoneUCSDDV(Loop).NumPlumesPerOcc = rNumericArgs(1);
            state.dataRoomAirMod->ZoneUCSDDV(Loop).ThermostatHeight = rNumericArgs(2);
            state.dataRoomAirMod->ZoneUCSDDV(Loop).ComfortHeight = rNumericArgs(3);
            state.dataRoomAirMod->ZoneUCSDDV(Loop).TempTrigger = rNumericArgs(4);
        }
    }

    void GetCrossVentData(EnergyPlusData &state, bool &ErrorsFound) // True if errors found during this get input routine
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         G. Carrilho da Graca
        //       DATE WRITTEN   October 2004
        //       MODIFIED
        //       RE-ENGINEERED

        // PURPOSE OF THIS SUBROUTINE:
        //  Get UCSD Cross ventilation model controls for all zones at once

        // METHODOLOGY EMPLOYED:
        // Use input processor to get input from idf file

        // Using/Aliasing
        using namespace DataIPShortCuts;
        using DataHeatBalance::Zone;
        using namespace ScheduleManager;
        using DataSurfaces::Surface;
        using DataHeatBalance::People;
        using DataHeatBalance::TotPeople;

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int IOStat;
        int NumAlpha;
        int NumNumber;
        int Loop;
        int Loop2;
        int ThisZone;
        static int CompNum(0);
        static int TypeNum(0);
        static int NodeNum1(0);
        static int NodeNum2(0);

        if (!state.dataRoomAirMod->UCSDModelUsed) return;
        cCurrentModuleObject = "RoomAirSettings:CrossVentilation";
        state.dataRoomAirMod->TotUCSDCV = inputProcessor->getNumObjectsFound(state, cCurrentModuleObject);

        if (state.dataRoomAirMod->TotUCSDCV <= 0) return;

        state.dataRoomAirMod->ZoneUCSDCV.allocate(state.dataRoomAirMod->TotUCSDCV);

        for (Loop = 1; Loop <= state.dataRoomAirMod->TotUCSDCV; ++Loop) {

            inputProcessor->getObjectItem(state,
                                          cCurrentModuleObject,
                                          Loop,
                                          cAlphaArgs,
                                          NumAlpha,
                                          rNumericArgs,
                                          NumNumber,
                                          IOStat,
                                          _,
                                          lAlphaFieldBlanks,
                                          cAlphaFieldNames,
                                          cNumericFieldNames);
            // First is Zone Name
            state.dataRoomAirMod->ZoneUCSDCV(Loop).ZoneName = cAlphaArgs(1);
            state.dataRoomAirMod->ZoneUCSDCV(Loop).ZonePtr = UtilityRoutines::FindItemInList(cAlphaArgs(1), Zone);
            if (state.dataRoomAirMod->ZoneUCSDCV(Loop).ZonePtr == 0) {
                ShowSevereError(state, "Invalid " + cAlphaFieldNames(1) + " = " + cAlphaArgs(1));
                ShowContinueError(state, "Entered in " + cCurrentModuleObject + " = " + cAlphaArgs(1));
                ShowContinueError(state, "Zone name was not found.");
                ErrorsFound = true;
            } else {
                state.dataRoomAirMod->IsZoneCV(state.dataRoomAirMod->ZoneUCSDCV(Loop).ZonePtr) = true;
            }
            // Second Alpha is Schedule Name
            state.dataRoomAirMod->ZoneUCSDCV(Loop).SchedGainsName = cAlphaArgs(2);
            state.dataRoomAirMod->ZoneUCSDCV(Loop).SchedGainsPtr = GetScheduleIndex(state, cAlphaArgs(2));
            if (state.dataRoomAirMod->ZoneUCSDCV(Loop).SchedGainsPtr == 0) {
                if (lAlphaFieldBlanks(2)) {
                    ShowSevereError(state, "Invalid " + cAlphaFieldNames(2) + " = " + cAlphaArgs(2));
                    ShowContinueError(state, "Entered in " + cCurrentModuleObject + " = " + cAlphaArgs(1));
                    ShowContinueError(state, "Schedule name field is blank.");
                    ErrorsFound = true;
                } else {
                    ShowSevereError(state, "Invalid " + cAlphaFieldNames(2) + " = " + cAlphaArgs(2));
                    ShowContinueError(state, "Entered in " + cCurrentModuleObject + " = " + cAlphaArgs(1));
                    ShowContinueError(state, "Schedule name was not found.");
                    ErrorsFound = true;
                }
            }

            // Third Alpha is a string: JET or RECIRCULATION
            if (UtilityRoutines::SameString(cAlphaArgs(3), "Jet")) {
                state.dataRoomAirMod->ZoneUCSDCV(Loop).VforComfort = Comfort::VComfort_Jet;
            } else if (UtilityRoutines::SameString(cAlphaArgs(3), "Recirculation")) {
                state.dataRoomAirMod->ZoneUCSDCV(Loop).VforComfort = Comfort::VComfort_Recirculation;
            } else {
                state.dataRoomAirMod->ZoneUCSDCV(Loop).VforComfort = Comfort::VComfort_Invalid;
            }

            for (Loop2 = 1; Loop2 <= TotPeople; ++Loop2) {
                if (People(Loop2).ZonePtr != state.dataRoomAirMod->ZoneUCSDCV(Loop).ZonePtr) continue;
                if (People(Loop2).Fanger) {
                    if (state.dataRoomAirMod->ZoneUCSDCV(Loop).VforComfort == Comfort::VComfort_Invalid) {
                        if (lAlphaFieldBlanks(3)) {
                            ShowSevereError(state, "Invalid " + cAlphaFieldNames(3) + " = " + cAlphaArgs(3));
                            ShowContinueError(state, "Entered in " + cCurrentModuleObject + " = " + cAlphaArgs(1));
                            ShowContinueError(state, "Airflow region used for thermal comfort evaluation is required for Zone=" + cAlphaArgs(1));
                            ShowContinueError(state, "Field is blank, please choose Jet or Recirculation.");
                            ErrorsFound = true;
                        } else {
                            ShowSevereError(state, "Invalid " + cAlphaFieldNames(3) + " = " + cAlphaArgs(3));
                            ShowContinueError(state, "Entered in " + cCurrentModuleObject + " = " + cAlphaArgs(1));
                            ShowContinueError(state, "Airflow region used for thermal comfort evaluation is required for Zone=" + cAlphaArgs(1));
                            ShowContinueError(state, "Please choose Jet or Recirculation.");
                            ErrorsFound = true;
                        }
                    }
                }
            }

            ThisZone = state.dataRoomAirMod->ZoneUCSDCV(Loop).ZonePtr;
            if (ThisZone == 0) continue;

            // Following depend on valid zone

            Loop2 = UtilityRoutines::FindItemInList(
                Zone(state.dataRoomAirMod->ZoneUCSDCV(Loop).ZonePtr).Name, AirflowNetwork::MultizoneZoneData, &AirflowNetwork::MultizoneZoneProp::ZoneName);
            if (Loop2 == 0) {
                ShowSevereError(state, "Problem with " + cCurrentModuleObject + " = " + cAlphaArgs(1));
                ShowContinueError(state, "AirflowNetwork airflow model must be active in this zone");
                ErrorsFound = true;
            }

            // If a crack is used it must have an air flow coefficient = 0.5
            for (Loop2 = 1; Loop2 <= AirflowNetwork::NumOfLinksMultiZone; ++Loop2) {
                NodeNum1 = AirflowNetwork::MultizoneSurfaceData(Loop2).NodeNums[0];
                NodeNum2 = AirflowNetwork::MultizoneSurfaceData(Loop2).NodeNums[1];
                if (Surface(AirflowNetwork::MultizoneSurfaceData(Loop2).SurfNum).Zone == ThisZone ||
                    (AirflowNetwork::AirflowNetworkNodeData(NodeNum2).EPlusZoneNum == ThisZone &&
                     AirflowNetwork::AirflowNetworkNodeData(NodeNum1).EPlusZoneNum > 0) ||
                    (AirflowNetwork::AirflowNetworkNodeData(NodeNum2).EPlusZoneNum > 0 &&
                     AirflowNetwork::AirflowNetworkNodeData(NodeNum1).EPlusZoneNum == ThisZone)) {
                    CompNum = AirflowNetwork::AirflowNetworkLinkageData(Loop2).CompNum;
                    TypeNum = AirflowNetwork::AirflowNetworkCompData(CompNum).TypeNum;
                    if (AirflowNetwork::AirflowNetworkCompData(CompNum).CompTypeNum == AirflowNetwork::CompTypeNum_SCR) {
                        if (AirflowNetwork::MultizoneSurfaceCrackData(TypeNum).FlowExpo != 0.50) {
                            state.dataRoomAirMod->AirModel(ThisZone).AirModelType = DataRoomAirModel::RoomAirModel::Mixing;
                            ShowWarningError(state, "Problem with " + cCurrentModuleObject + " = " + cAlphaArgs(1));
                            ShowWarningError(state, "Roomair model will not be applied for Zone=" + cAlphaArgs(1) + '.');
                            ShowContinueError(
                                state,
                                format("AirflowNetwrok:Multizone:Surface crack object must have an air flow coefficient = 0.5, value was={:.2R}",
                                       AirflowNetwork::MultizoneSurfaceCrackData(TypeNum).FlowExpo));
                        }
                    }
                }
            }
        }
    }

    void GetUFADZoneData(EnergyPlusData &state, bool &ErrorsFound) // True if errors found during this get input routine
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Fred Buhl
        //       DATE WRITTEN   August 2005
        //       MODIFIED
        //       RE-ENGINEERED

        // PURPOSE OF THIS SUBROUTINE:
        //  Get UCSD UFAD interior zone model controls for all zones at once

        // METHODOLOGY EMPLOYED:
        // Use input processor to get input from idf file

        // Using/Aliasing
        using namespace DataIPShortCuts;
        using DataHeatBalance::Zone;
        using namespace ScheduleManager;

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int IOStat;
        int NumAlpha;
        int NumNumber;
        int Loop;

        if (!state.dataRoomAirMod->UCSDModelUsed) {
            state.dataRoomAirMod->TotUCSDUI = 0;
            state.dataRoomAirMod->TotUCSDUE = 0;
            return;
        }
        cCurrentModuleObject = "RoomAirSettings:UnderFloorAirDistributionInterior";
        state.dataRoomAirMod->TotUCSDUI = inputProcessor->getNumObjectsFound(state, cCurrentModuleObject);
        cCurrentModuleObject = "RoomAirSettings:UnderFloorAirDistributionExterior";
        state.dataRoomAirMod->TotUCSDUE = inputProcessor->getNumObjectsFound(state, cCurrentModuleObject);

        if (state.dataRoomAirMod->TotUCSDUI <= 0 && state.dataRoomAirMod->TotUCSDUE <= 0) return;

        state.dataRoomAirMod->ZoneUCSDUI.allocate(state.dataRoomAirMod->TotUCSDUI);
        state.dataRoomAirMod->ZoneUCSDUE.allocate(state.dataRoomAirMod->TotUCSDUE);
        state.dataRoomAirMod->ZoneUFPtr.dimension(state.dataGlobal->NumOfZones, 0);

        cCurrentModuleObject = "RoomAirSettings:UnderFloorAirDistributionInterior";
        for (Loop = 1; Loop <= state.dataRoomAirMod->TotUCSDUI; ++Loop) {
            inputProcessor->getObjectItem(state,
                                          cCurrentModuleObject,
                                          Loop,
                                          cAlphaArgs,
                                          NumAlpha,
                                          rNumericArgs,
                                          NumNumber,
                                          IOStat,
                                          _,
                                          lAlphaFieldBlanks,
                                          cAlphaFieldNames,
                                          cNumericFieldNames);
            // First is Zone Name
            state.dataRoomAirMod->ZoneUCSDUI(Loop).ZoneName = cAlphaArgs(1);
            state.dataRoomAirMod->ZoneUCSDUI(Loop).ZonePtr = UtilityRoutines::FindItemInList(cAlphaArgs(1), Zone);
            state.dataRoomAirMod->ZoneUFPtr(state.dataRoomAirMod->ZoneUCSDUI(Loop).ZonePtr) = Loop;
            if (state.dataRoomAirMod->ZoneUCSDUI(Loop).ZonePtr == 0) {
                ShowSevereError(state, "Invalid " + cAlphaFieldNames(1) + " = " + cAlphaArgs(1));
                ShowContinueError(state, "Entered in " + cCurrentModuleObject + " = " + cAlphaArgs(1));
                ShowContinueError(state, "Zone name was not found.");
                ErrorsFound = true;
            } else {
                state.dataRoomAirMod->IsZoneUI(state.dataRoomAirMod->ZoneUCSDUI(Loop).ZonePtr) = true;
            }
            // 2nd alpha is diffuser type
            if (UtilityRoutines::SameString(cAlphaArgs(2), "Swirl")) {
                state.dataRoomAirMod->ZoneUCSDUI(Loop).DiffuserType = Diffuser::Swirl;
            } else if (UtilityRoutines::SameString(cAlphaArgs(2), "VariableArea")) {
                state.dataRoomAirMod->ZoneUCSDUI(Loop).DiffuserType = Diffuser::VarArea;
            } else if (UtilityRoutines::SameString(cAlphaArgs(2), "HorizontalSwirl")) {
                state.dataRoomAirMod->ZoneUCSDUI(Loop).DiffuserType = Diffuser::DisplVent;
            } else if (UtilityRoutines::SameString(cAlphaArgs(2), "Custom")) {
                state.dataRoomAirMod->ZoneUCSDUI(Loop).DiffuserType = Diffuser::Custom;
            } else if (UtilityRoutines::SameString(cAlphaArgs(2), "LinearBarGrille")) {
                state.dataRoomAirMod->ZoneUCSDUI(Loop).DiffuserType = Diffuser::LinBarGrille;
            } else {
                ShowSevereError(state, "Invalid " + cAlphaFieldNames(2) + " = " + cAlphaArgs(2));
                ShowContinueError(state, "Entered in " + cCurrentModuleObject + " = " + cAlphaArgs(1));
                ErrorsFound = true;
            }
            // 1st number is Number of Diffusers per Zone
            state.dataRoomAirMod->ZoneUCSDUI(Loop).DiffusersPerZone = rNumericArgs(1);
            // 2nd number is Power per Plume
            state.dataRoomAirMod->ZoneUCSDUI(Loop).PowerPerPlume = rNumericArgs(2);
            // 3rd number is Design Effective Area of Diffuser
            state.dataRoomAirMod->ZoneUCSDUI(Loop).DiffArea = rNumericArgs(3);
            // 4th number is Diffuser Slot Angle from Vertical
            state.dataRoomAirMod->ZoneUCSDUI(Loop).DiffAngle = rNumericArgs(4);
            // 5th number is Thermostat Height
            state.dataRoomAirMod->ZoneUCSDUI(Loop).ThermostatHeight = rNumericArgs(5);
            // 6th number is Comfort Height
            state.dataRoomAirMod->ZoneUCSDUI(Loop).ComfortHeight = rNumericArgs(6);
            // 7th number is Temperature Difference Threshold for Reporting
            state.dataRoomAirMod->ZoneUCSDUI(Loop).TempTrigger = rNumericArgs(7);
            // 8th number user-specified transition height
            state.dataRoomAirMod->ZoneUCSDUI(Loop).TransHeight = rNumericArgs(8);
            // 9th number is Coefficient A in formula Kc = A*Gamma**B + C + D*Gamma + E*Gamma**2
            state.dataRoomAirMod->ZoneUCSDUI(Loop).A_Kc = rNumericArgs(9);
            // 10th number is Coefficient B in formula Kc = A*Gamma**B + C + D*Gamma + E*Gamma**2
            state.dataRoomAirMod->ZoneUCSDUI(Loop).B_Kc = rNumericArgs(10);
            // 11th number is Coefficient C in formula Kc = A*Gamma**B + C + D*Gamma + E*Gamma**2
            state.dataRoomAirMod->ZoneUCSDUI(Loop).C_Kc = rNumericArgs(11);
            // 12th number is Coefficient D in formula Kc = A*Gamma**B + C + D*Gamma + E*Gamma**2
            state.dataRoomAirMod->ZoneUCSDUI(Loop).D_Kc = rNumericArgs(12);
            // 13th number is Coefficient E in formula Kc = A*Gamma**B + C + D*Gamma + E*Gamma**2
            state.dataRoomAirMod->ZoneUCSDUI(Loop).E_Kc = rNumericArgs(13);
        }

        cCurrentModuleObject = "RoomAirSettings:UnderFloorAirDistributionExterior";
        for (Loop = 1; Loop <= state.dataRoomAirMod->TotUCSDUE; ++Loop) {
            inputProcessor->getObjectItem(state,
                                          cCurrentModuleObject,
                                          Loop,
                                          cAlphaArgs,
                                          NumAlpha,
                                          rNumericArgs,
                                          NumNumber,
                                          IOStat,
                                          _,
                                          lAlphaFieldBlanks,
                                          cAlphaFieldNames,
                                          cNumericFieldNames);
            // First is Zone Name
            state.dataRoomAirMod->ZoneUCSDUE(Loop).ZoneName = cAlphaArgs(1);
            state.dataRoomAirMod->ZoneUCSDUE(Loop).ZonePtr = UtilityRoutines::FindItemInList(cAlphaArgs(1), Zone);
            state.dataRoomAirMod->ZoneUFPtr(state.dataRoomAirMod->ZoneUCSDUE(Loop).ZonePtr) = Loop;
            if (state.dataRoomAirMod->ZoneUCSDUE(Loop).ZonePtr == 0) {
                ShowSevereError(state, "Invalid " + cAlphaFieldNames(1) + " = " + cAlphaArgs(1));
                ShowContinueError(state, "Entered in " + cCurrentModuleObject + " = " + cAlphaArgs(1));
                ShowContinueError(state, "Zone name was not found.");
                ErrorsFound = true;
            } else {
                state.dataRoomAirMod->IsZoneUI(state.dataRoomAirMod->ZoneUCSDUE(Loop).ZonePtr) = true;
            }
            // 2nd alpha is diffuser type
            if (UtilityRoutines::SameString(cAlphaArgs(2), "Swirl")) {
                state.dataRoomAirMod->ZoneUCSDUE(Loop).DiffuserType = Diffuser::Swirl;
            } else if (UtilityRoutines::SameString(cAlphaArgs(2), "VariableArea")) {
                state.dataRoomAirMod->ZoneUCSDUE(Loop).DiffuserType = Diffuser::VarArea;
            } else if (UtilityRoutines::SameString(cAlphaArgs(2), "HorizontalSwirl")) {
                state.dataRoomAirMod->ZoneUCSDUE(Loop).DiffuserType = Diffuser::DisplVent;
            } else if (UtilityRoutines::SameString(cAlphaArgs(2), "Custom")) {
                state.dataRoomAirMod->ZoneUCSDUE(Loop).DiffuserType = Diffuser::Custom;
            } else if (UtilityRoutines::SameString(cAlphaArgs(2), "LinearBarGrille")) {
                state.dataRoomAirMod->ZoneUCSDUE(Loop).DiffuserType = Diffuser::LinBarGrille;
            } else {
                ShowSevereError(state, "Invalid " + cAlphaFieldNames(2) + " = " + cAlphaArgs(2));
                ShowContinueError(state, "Entered in " + cCurrentModuleObject + " = " + cAlphaArgs(1));
                ErrorsFound = true;
            }
            // 1st number is Number of Diffusers per Zone
            state.dataRoomAirMod->ZoneUCSDUE(Loop).DiffusersPerZone = rNumericArgs(1);
            // 2nd number is Power per Plume
            state.dataRoomAirMod->ZoneUCSDUE(Loop).PowerPerPlume = rNumericArgs(2);
            // 3rd number is Design Effective Area of Diffuser
            state.dataRoomAirMod->ZoneUCSDUE(Loop).DiffArea = rNumericArgs(3);
            // 4th number is Diffuser Slot Angle from Vertical
            state.dataRoomAirMod->ZoneUCSDUE(Loop).DiffAngle = rNumericArgs(4);
            // 5th number is Thermostat Height
            state.dataRoomAirMod->ZoneUCSDUE(Loop).ThermostatHeight = rNumericArgs(5);
            // 6th number is Comfort Height
            state.dataRoomAirMod->ZoneUCSDUE(Loop).ComfortHeight = rNumericArgs(6);
            // 7th number is Temperature Difference Threshold for Reporting
            state.dataRoomAirMod->ZoneUCSDUE(Loop).TempTrigger = rNumericArgs(7);
            // 8th number user-specified transition height
            state.dataRoomAirMod->ZoneUCSDUE(Loop).TransHeight = rNumericArgs(8);
            // 9th number is Coefficient A in formula Kc = A*Gamma**B + C + D*Gamma + E*Gamma**2
            state.dataRoomAirMod->ZoneUCSDUE(Loop).A_Kc = rNumericArgs(9);
            // 10th number is Coefficient B in formula Kc = A*Gamma**B + C + D*Gamma + E*Gamma**2
            state.dataRoomAirMod->ZoneUCSDUE(Loop).B_Kc = rNumericArgs(10);
            // 11th number is Coefficient C in formula Kc = A*Gamma**B + C + D*Gamma + E*Gamma**2
            state.dataRoomAirMod->ZoneUCSDUE(Loop).C_Kc = rNumericArgs(11);
            // 12th number is Coefficient D in formula Kc = A*Gamma**B + C + D*Gamma + E*Gamma**2
            state.dataRoomAirMod->ZoneUCSDUE(Loop).D_Kc = rNumericArgs(12);
            // 13th number is Coefficient E in formula Kc = A*Gamma**B + C + D*Gamma + E*Gamma**2
            state.dataRoomAirMod->ZoneUCSDUE(Loop).E_Kc = rNumericArgs(13);
        }
    }

    void GetRoomAirflowNetworkData(EnergyPlusData &state, bool &ErrorsFound) // True if errors found during this get input routine
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Brent Griffith
        //       DATE WRITTEN   November 2009
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        //  Get RoomAirflowNetwork data for all zones at once

        // METHODOLOGY EMPLOYED:
        // Use input processor to get input from idf file

        // Using/Aliasing
        using namespace DataIPShortCuts;
        using DataHeatBalance::NumZoneIntGainDeviceTypes;
        using DataHeatBalance::Zone;
        using DataHeatBalance::ZoneIntGainDeviceTypes;
        using DataHVACGlobals::NumZoneHVACTerminalTypes;
        using DataHVACGlobals::ZoneHVACTerminalTypes;
        using DataSurfaces::Surface;
        using InternalHeatGains::GetInternalGainDeviceIndex;
        using ScheduleManager::GetScheduleIndex;

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int Loop; // local do loop index
        int NumAlphas;
        int NumNumbers;
        int status;
        int ZoneNum;
        int thisAirNodeinZone;
        int AlphaArgNum;
        int AirCntrlNodeNum;
        int TotNumOfRAFNNodeSurfLists;
        int TotNumOfRAFNNodeGainsLists;
        int TotNumOfRAFNNodeHVACLists;
        bool IntGainError;
        int RAFNNodeNum;
        bool foundList;
        int NumSurfsThisNode;
        int NumOfSurfs;
        int SurfCount;
        int SurfFirst;
        int ListSurfNum;
        int SurfNum;
        int numGains;
        int gainsLoop;
        int TypeNum;
        int numEquip;
        int EquipLoop;
        int TotNumEquip;
        bool IntEquipError;
        Real64 SumFraction;
        std::string Name;
        int GainNum;
        int RAFNNum;

        cCurrentModuleObject = "RoomAirSettings:AirflowNetwork";
        state.dataRoomAirMod->NumOfRoomAirflowNetControl = inputProcessor->getNumObjectsFound(state, cCurrentModuleObject);
        if (state.dataRoomAirMod->NumOfRoomAirflowNetControl == 0) return;
        if (state.dataRoomAirMod->NumOfRoomAirflowNetControl > state.dataGlobal->NumOfZones) {
            ShowSevereError(state, "Too many " + cCurrentModuleObject + " objects in input file");
            ShowContinueError(state, "There cannot be more " + cCurrentModuleObject + " objects than number of zones.");
            ErrorsFound = true;
        }

        if (!allocated(state.dataRoomAirMod->RoomAirflowNetworkZoneInfo)) {
            state.dataRoomAirMod->RoomAirflowNetworkZoneInfo.allocate(state.dataGlobal->NumOfZones);
        }

        for (Loop = 1; Loop <= state.dataRoomAirMod->NumOfRoomAirflowNetControl; ++Loop) {
            inputProcessor->getObjectItem(state,
                                          cCurrentModuleObject,
                                          Loop,
                                          cAlphaArgs,
                                          NumAlphas,
                                          rNumericArgs,
                                          NumNumbers,
                                          status,
                                          _,
                                          lAlphaFieldBlanks,
                                          cAlphaFieldNames,
                                          cNumericFieldNames);
            ZoneNum = UtilityRoutines::FindItemInList(cAlphaArgs(2), Zone, state.dataGlobal->NumOfZones);
            if (ZoneNum == 0) {
                ShowSevereError(state, "GetRoomAirflowNetworkData: Invalid " + cAlphaFieldNames(2) + " = " + cAlphaArgs(2));
                ShowContinueError(state, "Entered in " + cCurrentModuleObject + " = " + cAlphaArgs(2));
                ShowContinueError(state, "Not a valid zone name.");
                ErrorsFound = true;
                continue;
            }
            if (state.dataRoomAirMod->AirModel(ZoneNum).AirModelType != DataRoomAirModel::RoomAirModel::AirflowNetwork) {
                ShowSevereError(state, "GetRoomAirflowNetworkData: Zone specified='" + cAlphaArgs(1) + "', Air Model type is not AirflowNetwork.");
                ShowContinueError(state, format("Air Model Type for zone ={}", ChAirModel[static_cast<int>(state.dataRoomAirMod->AirModel(ZoneNum).AirModelType)]));
                ErrorsFound = true;
                continue;
            }
            state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).ZoneID = ZoneNum;
            state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).RAFNNum = Loop;
            state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).IsUsed = true;
            state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Name = cAlphaArgs(1);
            state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).ZoneName = cAlphaArgs(2); // Zone Name

            state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).NumOfAirNodes = (NumAlphas - 3);

            if (state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).NumOfAirNodes > 0) {
                state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node.allocate(state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).NumOfAirNodes);
            } else {
                ShowSevereError(state, "GetRoomAirflowNetworkData: Incomplete input in " + cCurrentModuleObject + " = " + cAlphaArgs(1));
                ErrorsFound = true;
            }

            for (thisAirNodeinZone = 1; thisAirNodeinZone <= state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).NumOfAirNodes; ++thisAirNodeinZone) {
                AlphaArgNum = thisAirNodeinZone + 3;
                state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(thisAirNodeinZone).Name = cAlphaArgs(AlphaArgNum);
            }
            // control point node
            AirCntrlNodeNum = UtilityRoutines::FindItemInList(
                cAlphaArgs(3), state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node, state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).NumOfAirNodes);
            if (AirCntrlNodeNum == 0) {
                ShowSevereError(state, "GetRoomAirflowNetworkData: Invalid " + cAlphaFieldNames(3) + " = " + cAlphaArgs(3));
                ShowContinueError(state, "Entered in " + cCurrentModuleObject + " = " + cAlphaArgs(1));
                ShowContinueError(state, "Not a valid RoomAir:Node:AirflowNetwork name for this zone.");
                ErrorsFound = true;
                continue;
            } else {
                state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).ControlAirNodeID = AirCntrlNodeNum;
            }
            state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).totNumSurfs = Zone(ZoneNum).SurfaceLast - Zone(ZoneNum).SurfaceFirst + 1;
        } // loop thru NumOfRoomAirflowNetControl

        cCurrentModuleObject = "RoomAir:Node:AirflowNetwork";
        state.dataRoomAirMod->TotNumOfRoomAFNNodes = inputProcessor->getNumObjectsFound(state, cCurrentModuleObject);
        for (Loop = 1; Loop <= state.dataRoomAirMod->TotNumOfRoomAFNNodes; ++Loop) {
            inputProcessor->getObjectItem(state,
                                          cCurrentModuleObject,
                                          Loop,
                                          cAlphaArgs,
                                          NumAlphas,
                                          rNumericArgs,
                                          NumNumbers,
                                          status,
                                          _,
                                          lAlphaFieldBlanks,
                                          cAlphaFieldNames,
                                          cNumericFieldNames);
            ZoneNum = UtilityRoutines::FindItemInList(cAlphaArgs(2), Zone, state.dataGlobal->NumOfZones);
            if (ZoneNum == 0) {
                ShowSevereError(state, "GetRoomAirflowNetworkData: Invalid " + cAlphaFieldNames(2) + " = " + cAlphaArgs(2));
                ShowContinueError(state, "Entered in " + cCurrentModuleObject + " = " + cAlphaArgs(1));
                ShowContinueError(state, "Not a valid zone name.");
                ErrorsFound = true;
                continue;
            }

            RAFNNodeNum = UtilityRoutines::FindItemInList(
                cAlphaArgs(1), state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node, state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).NumOfAirNodes);
            if (RAFNNodeNum == 0) {
                ShowSevereError(state, "GetRoomAirflowNetworkData: Invalid " + cAlphaFieldNames(2) + " = " + cAlphaArgs(2));
                ShowContinueError(state, "Entered in " + cCurrentModuleObject + " = " + cAlphaArgs(1));
                ShowContinueError(state, "Not a valid RoomAir:Node:AirflowNetwork name.");
                ErrorsFound = true;
                continue;
            }

            state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(RAFNNodeNum).ZoneVolumeFraction = rNumericArgs(1);
            if (!lAlphaFieldBlanks(3)) {
                state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(RAFNNodeNum).NodeSurfListName = cAlphaArgs(3);
            } else {
                state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(RAFNNodeNum).HasSurfacesAssigned = false;
            }
            if (!lAlphaFieldBlanks(4)) {
                state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(RAFNNodeNum).NodeIntGainsListName = cAlphaArgs(4);
            } else {
                state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(RAFNNodeNum).HasIntGainsAssigned = false;
            }
            if (!lAlphaFieldBlanks(5)) {
                state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(RAFNNodeNum).NodeHVACListName = cAlphaArgs(5);
            } else {
                state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(RAFNNodeNum).HasHVACAssigned = false;
            }

        } // loop thru TotNumOfRoomAFNNodes

        cCurrentModuleObject = "RoomAir:Node:AirflowNetwork:AdjacentSurfaceList";
        TotNumOfRAFNNodeSurfLists = inputProcessor->getNumObjectsFound(state, cCurrentModuleObject);
        for (Loop = 1; Loop <= TotNumOfRAFNNodeSurfLists; ++Loop) {
            foundList = false;
            inputProcessor->getObjectItem(
                state, cCurrentModuleObject, Loop, cAlphaArgs, NumAlphas, rNumericArgs, NumNumbers, status, _, _, cAlphaFieldNames, cNumericFieldNames);
            for (ZoneNum = 1; ZoneNum <= state.dataGlobal->NumOfZones; ++ZoneNum) {
                // find surface list
                if (state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).NumOfAirNodes > 0) {
                    RAFNNodeNum = UtilityRoutines::FindItemInList(cAlphaArgs(1),
                                                                  state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node,
                                                                  &RoomAirflowNetworkAirNodeNestedStruct::NodeSurfListName,
                                                                  state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).NumOfAirNodes);
                } else {
                    RAFNNodeNum = 0;
                }
                if (RAFNNodeNum > 0) { // found it
                    foundList = true;
                    NumSurfsThisNode = NumAlphas - 1;
                    NumOfSurfs = Zone(ZoneNum).SurfaceLast - Zone(ZoneNum).SurfaceFirst + 1;
                    if (allocated(state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(RAFNNodeNum).SurfMask)) {
                        // throw error found twice
                        ShowSevereError(state, "GetRoomAirflowNetworkData: Invalid " + cAlphaFieldNames(1) + " = " + cAlphaArgs(1));
                        ShowContinueError(state, "Entered in " + cCurrentModuleObject + " = " + cAlphaArgs(1));
                        ShowContinueError(state, "Duplicate RoomAir:Node:AirflowNetwork:AdjacentSurfaceList name.");
                        ErrorsFound = true;
                    } else {
                        state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(RAFNNodeNum).SurfMask.allocate(state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).totNumSurfs);
                        state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(RAFNNodeNum).SurfMask = false; // init
                        state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(RAFNNodeNum).HasSurfacesAssigned = true;
                        // relate surfaces to this air node and check to see whether surface names are specified correctly or not
                        SurfCount = 0;
                        SurfFirst = Zone(ZoneNum).SurfaceFirst - 1;
                        for (ListSurfNum = 2; ListSurfNum <= NumAlphas; ++ListSurfNum) {
                            for (SurfNum = 1; SurfNum <= NumOfSurfs; ++SurfNum) {
                                // IF( cAlphaArgs( ListSurfNum ) == Surface( SurfFirst + SurfNum ).Name ) THEN
                                if (UtilityRoutines::SameString(cAlphaArgs(ListSurfNum), Surface(SurfFirst + SurfNum).Name)) {
                                    state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(RAFNNodeNum).SurfMask(SurfNum) = true;
                                    SurfCount = SurfCount + 1;
                                }
                            }
                        }
                        if (NumSurfsThisNode != SurfCount) {
                            ShowSevereError(state, "GetRoomAirflowNetworkData: Invalid " + cAlphaFieldNames(1) + " = " + cAlphaArgs(1));
                            ShowContinueError(state, "Entered in " + cCurrentModuleObject + " = " + cAlphaArgs(1));
                            ShowContinueError(state, "Some surface names were not found in the zone");
                            ErrorsFound = true;
                        }
                    }
                    break;
                }
            }                 // loop over zones
            if (!foundList) { // throw error
                ShowSevereError(state, "GetRoomAirflowNetworkData: Invalid " + cAlphaFieldNames(1) + " = " + cAlphaArgs(1));
                ShowContinueError(state, "Entered in " + cCurrentModuleObject + " = " + cAlphaArgs(1));
                ShowContinueError(state, "Did not find a RoomAir:Node:AirflowNetwork object that references this object");
                ErrorsFound = true;
            }
        } // loop thru TotNumOfRAFNNodeSurfLists

        cCurrentModuleObject = "RoomAir:Node:AirflowNetwork:InternalGains";
        TotNumOfRAFNNodeGainsLists = inputProcessor->getNumObjectsFound(state, cCurrentModuleObject);
        for (Loop = 1; Loop <= TotNumOfRAFNNodeGainsLists; ++Loop) {
            foundList = false;
            inputProcessor->getObjectItem(
                state, cCurrentModuleObject, Loop, cAlphaArgs, NumAlphas, rNumericArgs, NumNumbers, status, _, _, cAlphaFieldNames, cNumericFieldNames);
            if (mod((NumAlphas + NumNumbers - 1), 3) != 0) {
                ShowSevereError(state, "GetRoomAirflowNetworkData: For " + cCurrentModuleObject + ": " + cAlphaArgs(1));
                ShowContinueError(state,
                                  "Extensible field set are not evenly divisable by 3. Number of data entries = " +
                                      fmt::to_string(NumAlphas + NumNumbers - 1));
                ErrorsFound = true;
                break;
            }
            for (ZoneNum = 1; ZoneNum <= state.dataGlobal->NumOfZones; ++ZoneNum) {
                // find surface list
                if (state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).NumOfAirNodes > 0) {
                    RAFNNodeNum = UtilityRoutines::FindItemInList(cAlphaArgs(1),
                                                                  state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node,
                                                                  &RoomAirflowNetworkAirNodeNestedStruct::NodeIntGainsListName,
                                                                  state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).NumOfAirNodes);
                } else {
                    RAFNNodeNum = 0;
                }
                if (RAFNNodeNum > 0) { // found it
                    foundList = true;
                    numGains = (NumAlphas + NumNumbers - 1) / 3;
                    if (allocated(state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(RAFNNodeNum).IntGain)) {
                        ShowSevereError(state, "GetRoomAirflowNetworkData: Invalid " + cAlphaFieldNames(1) + " = " + cAlphaArgs(1));
                        ShowContinueError(state, "Entered in " + cCurrentModuleObject + " = " + cAlphaArgs(1));
                        ShowContinueError(state, "Duplicate " + cCurrentModuleObject + " name.");
                        ErrorsFound = true;
                    } else {
                        state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(RAFNNodeNum).IntGain.allocate(numGains);
                        state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(RAFNNodeNum).IntGainsDeviceIndices.allocate(numGains);
                        state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(RAFNNodeNum).IntGainsFractions.allocate(numGains);
                        state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(RAFNNodeNum).HasIntGainsAssigned = true;
                        state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(RAFNNodeNum).NumIntGains = numGains;
                        for (gainsLoop = 1; gainsLoop <= numGains; ++gainsLoop) {
                            TypeNum = UtilityRoutines::FindItemInList(cAlphaArgs(gainsLoop * 2), ZoneIntGainDeviceTypes, NumZoneIntGainDeviceTypes);
                            if (TypeNum > 0) {
                                state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(RAFNNodeNum).IntGain(gainsLoop).TypeOfNum = TypeNum;
                            } else {
                                ShowSevereError(state, "GetRoomAirflowNetworkData: Invalid " + cAlphaFieldNames(gainsLoop * 2) + " = " +
                                                cAlphaArgs(gainsLoop * 2));
                                ShowContinueError(state, "Entered in " + cCurrentModuleObject + " = " + cAlphaArgs(1));
                                ShowContinueError(state, "incorrect type of internal gain");
                                ErrorsFound = true;
                            }
                            state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(RAFNNodeNum).IntGain(gainsLoop).Name = cAlphaArgs(gainsLoop * 2 + 1);

                            // verify type and name and get pointer to device in internal gains structure array
                            IntGainError = false;
                            GetInternalGainDeviceIndex(ZoneNum,
                                                       state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(RAFNNodeNum).IntGain(gainsLoop).TypeOfNum,
                                                       state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(RAFNNodeNum).IntGain(gainsLoop).Name,
                                                       state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(RAFNNodeNum).IntGainsDeviceIndices(gainsLoop),
                                                       IntGainError);
                            if (IntGainError) {
                                ShowSevereError(state, "GetRoomAirflowNetworkData: Invalid " + cAlphaFieldNames(gainsLoop * 2 + 1) + " = " +
                                                cAlphaArgs(gainsLoop * 2 + 1));
                                ShowContinueError(state, "Entered in " + cCurrentModuleObject + " = " + cAlphaArgs(1));
                                ShowContinueError(state, "Internal gain did not match correctly");
                                ErrorsFound = true;
                            }
                            state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(RAFNNodeNum).IntGainsFractions(gainsLoop) = rNumericArgs(gainsLoop);
                        }
                    }
                }
            }
        } // loop thru TotNumOfRAFNNodeGainsLists

        // Get data of HVAC equipment
        cCurrentModuleObject = "RoomAir:Node:AirflowNetwork:HVACEquipment";
        TotNumOfRAFNNodeHVACLists = inputProcessor->getNumObjectsFound(state, cCurrentModuleObject);
        for (Loop = 1; Loop <= TotNumOfRAFNNodeHVACLists; ++Loop) {
            inputProcessor->getObjectItem(
                state, cCurrentModuleObject, Loop, cAlphaArgs, NumAlphas, rNumericArgs, NumNumbers, status, _, _, cAlphaFieldNames, cNumericFieldNames);
            if (mod((NumAlphas + NumNumbers - 1), 4) != 0) {
                ShowSevereError(state, "GetRoomAirflowNetworkData: For " + cCurrentModuleObject + ": " + cAlphaArgs(1));
                ShowContinueError(state,
                                  "Extensible field set are not evenly divisable by 4. Number of data entries = " +
                                      fmt::to_string(NumAlphas + NumNumbers - 1));
                ErrorsFound = true;
                break;
            }
            for (ZoneNum = 1; ZoneNum <= state.dataGlobal->NumOfZones; ++ZoneNum) {
                // find surface list
                if (state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).NumOfAirNodes > 0) {
                    RAFNNodeNum = UtilityRoutines::FindItemInList(cAlphaArgs(1),
                                                                  state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node,
                                                                  &RoomAirflowNetworkAirNodeNestedStruct::NodeHVACListName,
                                                                  state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).NumOfAirNodes);
                } else {
                    RAFNNodeNum = 0;
                }
                if (RAFNNodeNum > 0) { // found it
                    foundList = true;
                    numEquip = (NumAlphas + NumNumbers - 1) / 4;
                    state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(RAFNNodeNum).NumHVACs = numEquip;
                    if (allocated(state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(RAFNNodeNum).HVAC)) {
                        ShowSevereError(state, "GetRoomAirflowNetworkData: Invalid " + cAlphaFieldNames(1) + " = " + cAlphaArgs(1));
                        ShowContinueError(state, "Entered in " + cCurrentModuleObject + " = " + cAlphaArgs(1));
                        ShowContinueError(state, "Duplicate " + cCurrentModuleObject + " name.");
                        ErrorsFound = true;
                    } else {
                        state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(RAFNNodeNum).HVAC.allocate(numEquip);
                        state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(RAFNNodeNum).HasHVACAssigned = true;
                        for (EquipLoop = 1; EquipLoop <= numEquip; ++EquipLoop) {
                            TypeNum =
                                UtilityRoutines::FindItemInList(cAlphaArgs(2 + (EquipLoop - 1) * 2), ZoneHVACTerminalTypes, NumZoneHVACTerminalTypes);
                            if (TypeNum > 0) {
                                state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(RAFNNodeNum).HVAC(EquipLoop).TypeOfNum = TypeNum;
                            } else {
                                ShowSevereError(state, "GetRoomAirflowNetworkData: Invalid " + cAlphaFieldNames(2 + (EquipLoop - 1) * 2) + " = " +
                                                cAlphaArgs(2 + (EquipLoop - 1) * 2));
                                ShowContinueError(state, "Entered in " + cCurrentModuleObject + " = " + cAlphaArgs(1));
                                ShowContinueError(state, "incorrect type of HVACEquipment");
                                ErrorsFound = true;
                            }
                            state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(RAFNNodeNum).HVAC(EquipLoop).ObjectTypeName =
                                cAlphaArgs(2 + (EquipLoop - 1) * 2);
                            state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(RAFNNodeNum).HVAC(EquipLoop).Name = cAlphaArgs(3 + (EquipLoop - 1) * 2);

                            // verify type and name and get pointer to device in HVAC equipment type and name structure array
                            TotNumEquip = inputProcessor->getNumObjectsFound(state, ZoneHVACTerminalTypes(TypeNum));
                            if (TotNumEquip == 0) {
                                ShowSevereError(state, "GetRoomAirflowNetworkData: No such " + cAlphaFieldNames(2 + (EquipLoop - 1) * 2) + " = " +
                                                cAlphaArgs(2 + (EquipLoop - 1) * 2));
                                ShowContinueError(state, "is available in the input file in " + cCurrentModuleObject + " = " + cAlphaArgs(1));
                                ErrorsFound = true;
                            }
                            state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(RAFNNodeNum).HVAC(EquipLoop).SupplyFraction =
                                rNumericArgs(1 + (EquipLoop - 1) * 2);
                            state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(RAFNNodeNum).HVAC(EquipLoop).ReturnFraction =
                                rNumericArgs(2 + (EquipLoop - 1) * 2);

                            IntEquipError = CheckEquipName(state, state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(RAFNNodeNum).HVAC(EquipLoop).ObjectTypeName,
                                                           state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(RAFNNodeNum).HVAC(EquipLoop).Name,
                                                           state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(RAFNNodeNum).HVAC(EquipLoop).SupplyNodeName,
                                                           state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(RAFNNodeNum).HVAC(EquipLoop).ReturnNodeName,
                                                           TotNumEquip,
                                                           TypeNum);

                            if (!IntEquipError) {
                                ShowSevereError(state, "GetRoomAirflowNetworkData: Invalid " + cAlphaFieldNames(3 + (EquipLoop - 1) * 2) + " = " +
                                                cAlphaArgs(2 + (EquipLoop - 1) * 2));
                                ShowContinueError(state, "Entered in " + cCurrentModuleObject + " = " + cAlphaArgs(1));
                                ShowContinueError(state, "Internal gain did not match correctly");
                                ErrorsFound = true;
                            }
                            //!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                            // TYPE RoomAirflowNetworkHVACStruct
                            // INTEGER::EquipConfigIndex = 0
                        }
                    }
                }
            }
        } // loop thru TotNumOfRAFNNodeHVACLists

        // do some checks on input data
        for (ZoneNum = 1; ZoneNum <= state.dataGlobal->NumOfZones; ++ZoneNum) {
            if (state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).NumOfAirNodes > 0) {
                // Check zone volume fraction
                SumFraction = 0.0;
                for (RAFNNodeNum = 1; RAFNNodeNum <= state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).NumOfAirNodes; ++RAFNNodeNum) {
                    SumFraction = SumFraction + state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(RAFNNodeNum).ZoneVolumeFraction;
                }
                if (std::abs(SumFraction - 1.0) > 0.001) {
                    ShowSevereError(state, "GetRoomAirflowNetworkData: Invalid, zone volume fractions do not sum to 1.0");
                    ShowContinueError(state, "Entered in RoomAir:Node:AirflowNetwork with Zone Name = " + Zone(ZoneNum).Name);
                    ShowContinueError(state, "The Fraction of Zone Air Volume values across all the nodes needs to sum to 1.0.");
                    ShowContinueError(state, format("The sum of fractions entered = {:.3R}", SumFraction));
                    ErrorsFound = true;
                }
                // Check internal gain fraction
                for (RAFNNodeNum = 1; RAFNNodeNum <= state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).NumOfAirNodes; ++RAFNNodeNum) {
                    for (gainsLoop = 1; gainsLoop <= state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(RAFNNodeNum).NumIntGains; ++gainsLoop) {
                        if (state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(RAFNNodeNum).IntGain(gainsLoop).FractionCheck) continue;
                        SumFraction = state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(RAFNNodeNum).IntGainsFractions(gainsLoop);
                        TypeNum = state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(RAFNNodeNum).IntGain(gainsLoop).TypeOfNum;
                        Name = state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(RAFNNodeNum).IntGain(gainsLoop).Name;
                        state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(RAFNNodeNum).IntGain(gainsLoop).FractionCheck = true;
                        for (RAFNNum = 1; RAFNNum <= state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).NumOfAirNodes; ++RAFNNum) {
                            for (GainNum = 1; GainNum <= state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(RAFNNum).NumIntGains; ++GainNum) {
                                if (state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(RAFNNum).IntGain(GainNum).FractionCheck) continue;
                                if (TypeNum == state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(RAFNNum).IntGain(GainNum).TypeOfNum &&
                                    UtilityRoutines::SameString(Name, state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(RAFNNum).IntGain(GainNum).Name)) {
                                    SumFraction += state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(RAFNNum).IntGainsFractions(GainNum);
                                    state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(RAFNNum).IntGain(GainNum).FractionCheck = true;
                                }
                            }
                        }
                        if (std::abs(SumFraction - 1.0) > 0.001) {
                            ShowSevereError(state, "GetRoomAirflowNetworkData: Invalid, internal gain fractions do not sum to 1.0");
                            ShowContinueError(state, "Entered in RoomAir:Node:AirflowNetwork with Zone Name = " + Zone(ZoneNum).Name +
                                              ", Intrnal gain name = " + Name);
                            ShowContinueError(state, "The Fraction of internal gain across all the nodes needs to sum to 1.0.");
                            ShowContinueError(state, format("The sum of fractions entered = {:.3R}", SumFraction));
                            ErrorsFound = true;
                        }
                    }
                }
            }
        }

        if (!ErrorsFound) {
            for (ZoneNum = 1; ZoneNum <= state.dataGlobal->NumOfZones; ++ZoneNum) {
                if (state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).IsUsed) {
                    if (state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).NumOfAirNodes > 0) {
                        for (Loop = 1; Loop <= state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).NumOfAirNodes; ++Loop) {
                            SetupOutputVariable(state, "RoomAirflowNetwork Node Temperature",
                                                OutputProcessor::Unit::C,
                                                state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(Loop).AirTemp,
                                                "HVAC",
                                                "Average",
                                                state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(Loop).Name);
                            SetupOutputVariable(state, "RoomAirflowNetwork Node Humidity Ratio",
                                                OutputProcessor::Unit::kgWater_kgDryAir,
                                                state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(Loop).HumRat,
                                                "HVAC",
                                                "Average",
                                                state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(Loop).Name);
                            SetupOutputVariable(state, "RoomAirflowNetwork Node Relative Humidity",
                                                OutputProcessor::Unit::Perc,
                                                state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(Loop).RelHumidity,
                                                "HVAC",
                                                "Average",
                                                state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(ZoneNum).Node(Loop).Name);
                        }
                    }
                }
            }
        }
    }

    void SharedDVCVUFDataInit(EnergyPlusData &state, int &ZoneNum)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Linda Lawrie
        //       DATE WRITTEN   March 2005
        //       MODIFIED       Aug, 2013, Sam Brunswick -- for RoomAirCrossCrossVent modifications
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This routine allocates and initializes(?) the data that is shared between the
        // UCSD models (DV and CV)

        // METHODOLOGY EMPLOYED:
        // na

        // REFERENCES:
        // na

        // Using/Aliasing
        using namespace DataUCSDSharedData;
        using namespace DataEnvironment;
        using namespace DataHeatBalFanSys;
        using namespace DataSurfaces;
        using DataHeatBalance::Zone;
        using Psychrometrics::PsyRhoAirFnPbTdbW;

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:

        // SUBROUTINE PARAMETER DEFINITIONS:
        Real64 const BaseDischargeCoef(0.62);

        // INTERFACE BLOCK SPECIFICATIONS:
        // na

        // DERIVED TYPE DEFINITIONS:
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int SurfNum;                             // DO loop counter for surfaces
        int ZNum;                                // DO loop counter for zones
        static int contFloorBegin(0);            // counter
        static int contFloorLast(0);             // counter
        static int contFloor(0);                 // counter
        static int contCeilingBegin(0);          // counter
        static int contCeilingLast(0);           // counter
        static int contCeiling(0);               // counter
        static int contWallBegin(0);             // counter
        static int contWallLast(0);              // counter
        static int contWall(0);                  // counter
        static int contWindowBegin(0);           // counter
        static int contWindowLast(0);            // counter
        static int contWindow(0);                // counter
        static int contInternalBegin(0);         // counter
        static int contInternalLast(0);          // counter
        static int contInternal(0);              // counter
        static int contDoorBegin(0);             // counter
        static int contDoorLast(0);              // counter
        static int contDoor(0);                  // counter
        static int Loop(0);                      // counter
        static int Loop2(0);                     // counter
        static int i(0);                         // counter
        static int N(0);                         // counter
        static Real64 Z1ZoneAux(0.0);            // Auxiliary variables
        static Real64 Z2ZoneAux(0.0);            // Auxiliary variables
        static Real64 Z1Zone(0.0);               // Auxiliary variables
        static Real64 Z2Zone(0.0);               // Auxiliary variables
        static Real64 CeilingHeightDiffMax(0.1); // Maximum difference between wall height and ceiling height
        bool SetZoneAux;
        Array1D_int AuxSurf;
        int MaxSurf;
        Array2D_int AuxAirflowNetworkSurf;
        Real64 WidthFactMax;
        Real64 HeightFactMax;
        Real64 WidthFact;
        Real64 HeightFact;
        static int Loop3(0);    // counter
        int ZoneEquipConfigNum; // counter
        Real64 AinCV;
        int AirflowNetworkSurfPtr;
        int NSides;
        static Array1D_bool MyEnvrnFlag;

        static int CompNum(0);  // AirflowNetwork Component number
        static int TypeNum(0);  // Airflownetwork Type Number within a component
        static int NodeNum1(0); // The first node number in an AirflowNetwork linkage data
        static int NodeNum2(0); // The Second node number in an AirflowNetwork linkage data

        // Do the one time initializations
        if (MyOneTimeFlag) {

            MyEnvrnFlag.allocate(state.dataGlobal->NumOfZones);

            APos_Wall.allocate(TotSurfaces);
            APos_Floor.allocate(TotSurfaces);
            APos_Ceiling.allocate(TotSurfaces);
            PosZ_Wall.allocate(state.dataGlobal->NumOfZones * 2);
            PosZ_Floor.allocate(state.dataGlobal->NumOfZones * 2);
            PosZ_Ceiling.allocate(state.dataGlobal->NumOfZones * 2);
            APos_Window.allocate(TotSurfaces);
            APos_Door.allocate(TotSurfaces);
            APos_Internal.allocate(TotSurfaces);
            PosZ_Window.allocate(state.dataGlobal->NumOfZones * 2);
            PosZ_Door.allocate(state.dataGlobal->NumOfZones * 2);
            PosZ_Internal.allocate(state.dataGlobal->NumOfZones * 2);
            HCeiling.allocate(TotSurfaces);
            HWall.allocate(TotSurfaces);
            HFloor.allocate(TotSurfaces);
            HInternal.allocate(TotSurfaces);
            HWindow.allocate(TotSurfaces);
            HDoor.allocate(TotSurfaces);

            AuxSurf.allocate(state.dataGlobal->NumOfZones);

            state.dataRoomAirMod->ZoneCeilingHeight.allocate(state.dataGlobal->NumOfZones * 2);
            state.dataRoomAirMod->ZoneCeilingHeight = 0.0;

            // Arrays initializations
            APos_Wall = 0;
            APos_Floor = 0;
            APos_Ceiling = 0;
            PosZ_Wall = 0;
            PosZ_Floor = 0;
            PosZ_Ceiling = 0;
            APos_Window = 0;
            APos_Door = 0;
            APos_Internal = 0;
            PosZ_Window = 0;
            PosZ_Door = 0;
            PosZ_Internal = 0;
            HCeiling = 0.0;
            HWall = 0.0;
            HFloor = 0.0;
            HInternal = 0.0;
            HWindow = 0.0;
            HDoor = 0.0;

            // Put the surface and zone information in Apos and PosZ arrays
            for (ZNum = 1; ZNum <= state.dataGlobal->NumOfZones; ++ZNum) {
                // advance ONE position in the arrays PosZ because this is a new zone
                contWallBegin = contWall + 1;
                contFloorBegin = contFloor + 1;
                contCeilingBegin = contCeiling + 1;
                contWindowBegin = contWindow + 1;
                contInternalBegin = contInternal + 1;
                contDoorBegin = contDoor + 1;
                SetZoneAux = true;

                // cycle in this zone for all the surfaces
                for (SurfNum = Zone(ZNum).SurfaceFirst; SurfNum <= Zone(ZNum).SurfaceLast; ++SurfNum) {
                    if (Surface(SurfNum).Class != DataSurfaces::SurfaceClass::IntMass) {
                        // Recalculate lowest and highest height for the zone
                        Z1Zone = std::numeric_limits<Real64>::max();
                        Z2Zone = std::numeric_limits<Real64>::lowest();
                        for (int i = 1, u = Surface(SurfNum).Sides; i <= u; ++i) {
                            Real64 const z_i(Surface(SurfNum).Vertex(i).z);
                            Z1Zone = std::min(Z1Zone, z_i);
                            Z2Zone = std::max(Z2Zone, z_i);
                        }
                    }

                    if (SetZoneAux) {
                        // lowest height for the zone (for the first surface of the zone)
                        Z1ZoneAux = Z1Zone;
                        // highest height for the zone (for the first surface of the zone)
                        Z2ZoneAux = Z2Zone;
                        SetZoneAux = false;
                    }

                    if (Z1Zone < Z1ZoneAux) {
                        Z1ZoneAux = Z1Zone;
                    }
                    if (Z2Zone > Z2ZoneAux) {
                        Z2ZoneAux = Z2Zone;
                    }
                    Z1Zone = Z1ZoneAux;
                    Z2Zone = Z2ZoneAux;

                    // Put the reference to this surface in the appropriate array
                    if (Surface(SurfNum).Class == SurfaceClass::Floor) {
                        ++contFloor;
                        APos_Floor(contFloor) = SurfNum;
                    } else if (Surface(SurfNum).Class == SurfaceClass::Wall) {
                        ++contWall;
                        APos_Wall(contWall) = SurfNum;
                    } else if (Surface(SurfNum).Class == SurfaceClass::Window) {
                        ++contWindow;
                        APos_Window(contWindow) = SurfNum;
                    } else if (Surface(SurfNum).Class == SurfaceClass::IntMass) {
                        ++contInternal;
                        APos_Internal(contInternal) = SurfNum;
                    } else if (Surface(SurfNum).Class == SurfaceClass::Door) {
                        ++contDoor;
                        APos_Door(contDoor) = SurfNum;
                    } else {
                        ++contCeiling;
                        APos_Ceiling(contCeiling) = SurfNum;
                    }
                } // Surfaces

                contWallLast = contWall;
                contFloorLast = contFloor;
                contCeilingLast = contCeiling;
                contWindowLast = contWindow;
                contDoorLast = contDoor;
                contInternalLast = contInternal;
                // PosZ_Wall (... + 1) has the Begin Wall reference in Apos_Wall for the ZNum
                // PosZ_Wall (... + 2) has the End Wall reference in Apos_Wall for the ZNum
                PosZ_Wall((ZNum - 1) * 2 + 1) = contWallBegin;
                PosZ_Wall((ZNum - 1) * 2 + 2) = contWallLast;
                PosZ_Floor((ZNum - 1) * 2 + 1) = contFloorBegin;
                PosZ_Floor((ZNum - 1) * 2 + 2) = contFloorLast;
                PosZ_Ceiling((ZNum - 1) * 2 + 1) = contCeilingBegin;
                PosZ_Ceiling((ZNum - 1) * 2 + 2) = contCeilingLast;
                PosZ_Window((ZNum - 1) * 2 + 1) = contWindowBegin;
                PosZ_Window((ZNum - 1) * 2 + 2) = contWindowLast;
                PosZ_Door((ZNum - 1) * 2 + 1) = contDoorBegin;
                PosZ_Door((ZNum - 1) * 2 + 2) = contDoorLast;
                PosZ_Internal((ZNum - 1) * 2 + 1) = contInternalBegin;
                PosZ_Internal((ZNum - 1) * 2 + 2) = contInternalLast;
                // Save the highest and lowest height for this zone
                state.dataRoomAirMod->ZoneCeilingHeight((ZNum - 1) * 2 + 1) = Z1Zone;
                state.dataRoomAirMod->ZoneCeilingHeight((ZNum - 1) * 2 + 2) = Z2Zone;

                if (std::abs((Z2Zone - Z1Zone) - Zone(ZNum).CeilingHeight) > CeilingHeightDiffMax) {
                    ShowWarningError(state, "RoomAirManager: Inconsistent ceiling heights in Zone: " + Zone(ZNum).Name);
                    ShowContinueError(state, format("Lowest height=[{:.3R}].", Z1Zone));
                    ShowContinueError(state, format("Highest height=[{:.3R}].", Z2Zone));
                    ShowContinueError(state, format("Ceiling height=[{:.3R}].", Zone(ZNum).CeilingHeight));
                }
            } // Zones

            AuxSurf = 0;
            state.dataRoomAirMod->CVNumAirflowNetworkSurfaces = 0;

            // calculate maximum number of airflow network surfaces in each zone
            for (Loop = 1; Loop <= AirflowNetwork::NumOfLinksMultiZone; ++Loop) {
                ++AuxSurf(Surface(AirflowNetwork::MultizoneSurfaceData(Loop).SurfNum).Zone);
                ++state.dataRoomAirMod->CVNumAirflowNetworkSurfaces;
                // Check if this is an interzone airflow network surface
                if (Surface(AirflowNetwork::MultizoneSurfaceData(Loop).SurfNum).ExtBoundCond > 0 &&
                    (AirflowNetwork::MultizoneSurfaceData(Loop).SurfNum !=
                     Surface(AirflowNetwork::MultizoneSurfaceData(Loop).SurfNum).ExtBoundCond)) {
                    ++AuxSurf(Surface(Surface(AirflowNetwork::MultizoneSurfaceData(Loop).SurfNum).ExtBoundCond).Zone);
                    ++state.dataRoomAirMod->CVNumAirflowNetworkSurfaces;
                }
            }
            // calculate maximum number of airflow network surfaces in a single zone
            MaxSurf = AuxSurf(1);
            for (Loop = 2; Loop <= state.dataGlobal->NumOfZones; ++Loop) {
                if (AuxSurf(Loop) > MaxSurf) MaxSurf = AuxSurf(Loop);
            }

            if (!allocated(state.dataRoomAirMod->AirflowNetworkSurfaceUCSDCV)) {
                state.dataRoomAirMod->AirflowNetworkSurfaceUCSDCV.allocate({0, MaxSurf}, state.dataGlobal->NumOfZones);
            }
            if (!allocated(state.dataRoomAirMod->CVJetRecFlows)) {
                state.dataRoomAirMod->CVJetRecFlows.allocate({0, MaxSurf}, state.dataGlobal->NumOfZones);
            }
            AuxAirflowNetworkSurf.allocate({0, MaxSurf}, state.dataGlobal->NumOfZones);
            // Width and Height for airflow network surfaces
            if (!allocated(state.dataRoomAirMod->SurfParametersCVDV)) {
                state.dataRoomAirMod->SurfParametersCVDV.allocate(AirflowNetwork::NumOfLinksMultiZone);
            }

            state.dataRoomAirMod->AirflowNetworkSurfaceUCSDCV = 0;
            // Organize surfaces in vector AirflowNetworkSurfaceUCSDCV(Zone, surface indexes)
            for (Loop = 1; Loop <= state.dataGlobal->NumOfZones; ++Loop) {
                // the 0 component of the array has the number of relevant AirflowNetwork surfaces for the zone
                state.dataRoomAirMod->AirflowNetworkSurfaceUCSDCV(0, Loop) = AuxSurf(Loop);
                if (AuxSurf(Loop) != 0) {
                    Real64 const ceilingHeight(state.dataRoomAirMod->ZoneCeilingHeight((Loop - 1) * 2 + 1));
                    SurfNum = 1;
                    for (Loop2 = 1; Loop2 <= AirflowNetwork::NumOfLinksMultiZone; ++Loop2) {
                        if (Surface(AirflowNetwork::MultizoneSurfaceData(Loop2).SurfNum).Zone == Loop) {
                            // SurfNum has the reference surface number relative to AirflowNetworkSurfaceData
                            state.dataRoomAirMod->AirflowNetworkSurfaceUCSDCV(SurfNum, Loop) = Loop2;
                            // calculate the surface width and height
                            CompNum = AirflowNetwork::AirflowNetworkLinkageData(Loop2).CompNum;
                            TypeNum = AirflowNetwork::AirflowNetworkCompData(CompNum).TypeNum;
                            if (AirflowNetwork::AirflowNetworkCompData(CompNum).CompTypeNum == AirflowNetwork::CompTypeNum_DOP) {
                                WidthFactMax = 0.0;
                                HeightFactMax = 0.0;
                                for (Loop3 = 1; Loop3 <= AirflowNetwork::MultizoneCompDetOpeningData(TypeNum).NumFac; ++Loop3) {
                                    if (Loop3 == 1) {
                                        WidthFact = AirflowNetwork::MultizoneCompDetOpeningData(TypeNum).WidthFac1;
                                        HeightFact = AirflowNetwork::MultizoneCompDetOpeningData(TypeNum).HeightFac1;
                                    }
                                    if (Loop3 == 2) {
                                        WidthFact = AirflowNetwork::MultizoneCompDetOpeningData(TypeNum).WidthFac2;
                                        HeightFact = AirflowNetwork::MultizoneCompDetOpeningData(TypeNum).HeightFac2;
                                    }
                                    if (Loop3 == 3) {
                                        WidthFact = AirflowNetwork::MultizoneCompDetOpeningData(TypeNum).WidthFac3;
                                        HeightFact = AirflowNetwork::MultizoneCompDetOpeningData(TypeNum).HeightFac3;
                                    }
                                    if (Loop3 == 4) {
                                        WidthFact = AirflowNetwork::MultizoneCompDetOpeningData(TypeNum).WidthFac4;
                                        HeightFact = AirflowNetwork::MultizoneCompDetOpeningData(TypeNum).HeightFac4;
                                    }
                                    if (WidthFact > WidthFactMax) {
                                        WidthFactMax = WidthFact;
                                    }
                                    if (HeightFact > HeightFactMax) {
                                        HeightFactMax = HeightFact;
                                    }
                                }
                                state.dataRoomAirMod->SurfParametersCVDV(Loop2).Width = WidthFactMax * Surface(AirflowNetwork::MultizoneSurfaceData(Loop2).SurfNum).Width;
                                state.dataRoomAirMod->SurfParametersCVDV(Loop2).Height =
                                    HeightFactMax * Surface(AirflowNetwork::MultizoneSurfaceData(Loop2).SurfNum).Height;
                            } else if (AirflowNetwork::AirflowNetworkCompData(CompNum).CompTypeNum ==
                                       AirflowNetwork::CompTypeNum_SCR) { // surface type = CRACK
                                state.dataRoomAirMod->SurfParametersCVDV(Loop2).Width = Surface(AirflowNetwork::MultizoneSurfaceData(Loop2).SurfNum).Width / 2;
                                AinCV = AirflowNetwork::MultizoneSurfaceCrackData(TypeNum).FlowCoef /
                                        (BaseDischargeCoef * std::sqrt(2.0 / PsyRhoAirFnPbTdbW(state, state.dataEnvrn->OutBaroPress, MAT(Loop), ZoneAirHumRat(Loop))));
                                state.dataRoomAirMod->SurfParametersCVDV(Loop2).Height = AinCV / state.dataRoomAirMod->SurfParametersCVDV(Loop2).Width;
                            }
                            // calculate the surface Zmin and Zmax
                            if (AirflowNetwork::AirflowNetworkCompData(CompNum).CompTypeNum == AirflowNetwork::CompTypeNum_DOP) {
                                AirflowNetworkSurfPtr = AirflowNetwork::MultizoneSurfaceData(Loop2).SurfNum;
                                NSides = Surface(AirflowNetwork::MultizoneSurfaceData(Loop2).SurfNum).Sides;
                                Real64 z_min(std::numeric_limits<Real64>::max()), z_max(std::numeric_limits<Real64>::lowest());
                                for (int i = 1; i <= NSides; ++i) {
                                    Real64 const z_i(Surface(AirflowNetworkSurfPtr).Vertex(i).z);
                                    z_min = std::min(z_min, z_i);
                                    z_max = std::max(z_max, z_i);
                                }
                                state.dataRoomAirMod->SurfParametersCVDV(Loop2).Zmin = z_min - ceilingHeight;
                                state.dataRoomAirMod->SurfParametersCVDV(Loop2).Zmax = z_max - ceilingHeight;
                            } else if (AirflowNetwork::AirflowNetworkCompData(CompNum).CompTypeNum ==
                                       AirflowNetwork::CompTypeNum_SCR) { // surface type = CRACK
                                AirflowNetworkSurfPtr = AirflowNetwork::MultizoneSurfaceData(Loop2).SurfNum;
                                NSides = Surface(AirflowNetwork::MultizoneSurfaceData(Loop2).SurfNum).Sides;
                                Real64 z_min(std::numeric_limits<Real64>::max()), z_max(std::numeric_limits<Real64>::lowest());
                                for (int i = 1; i <= NSides; ++i) {
                                    Real64 const z_i(Surface(AirflowNetworkSurfPtr).Vertex(i).z);
                                    z_min = std::min(z_min, z_i);
                                    z_max = std::max(z_max, z_i);
                                }
                                state.dataRoomAirMod->SurfParametersCVDV(Loop2).Zmin = z_min - ceilingHeight;
                                state.dataRoomAirMod->SurfParametersCVDV(Loop2).Zmax = z_max - ceilingHeight;
                            }

                            ++SurfNum;
                            // Check if airflow network Surface is an interzone surface:
                        } else {
                            NodeNum1 = AirflowNetwork::MultizoneSurfaceData(Loop2).NodeNums[0];
                            NodeNum2 = AirflowNetwork::MultizoneSurfaceData(Loop2).NodeNums[1];
                            if ((AirflowNetwork::AirflowNetworkNodeData(NodeNum2).EPlusZoneNum == Loop &&
                                 AirflowNetwork::AirflowNetworkNodeData(NodeNum1).EPlusZoneNum > 0) ||
                                (AirflowNetwork::AirflowNetworkNodeData(NodeNum2).EPlusZoneNum > 0 &&
                                 AirflowNetwork::AirflowNetworkNodeData(NodeNum1).EPlusZoneNum == Loop)) {
                                state.dataRoomAirMod->AirflowNetworkSurfaceUCSDCV(SurfNum, Loop) = Loop2;
                                ++SurfNum;
                            }
                        }
                    }
                }
            }

            AuxSurf.deallocate();

            if (any(state.dataRoomAirMod->IsZoneDV) || any(state.dataRoomAirMod->IsZoneUI)) {
                state.dataRoomAirMod->MaxTempGrad.allocate(state.dataGlobal->NumOfZones);
                state.dataRoomAirMod->AvgTempGrad.allocate(state.dataGlobal->NumOfZones);
                state.dataRoomAirMod->TCMF.allocate(state.dataGlobal->NumOfZones);
                state.dataRoomAirMod->FracMinFlow.allocate(state.dataGlobal->NumOfZones);
                state.dataRoomAirMod->ZoneAirSystemON.allocate(state.dataGlobal->NumOfZones);
                // Allocate histories of displacement ventilation temperatures PH 3/5/04
                state.dataRoomAirMod->MATFloor.allocate(state.dataGlobal->NumOfZones);
                state.dataRoomAirMod->XMATFloor.allocate(state.dataGlobal->NumOfZones);
                state.dataRoomAirMod->XM2TFloor.allocate(state.dataGlobal->NumOfZones);
                state.dataRoomAirMod->XM3TFloor.allocate(state.dataGlobal->NumOfZones);
                state.dataRoomAirMod->XM4TFloor.allocate(state.dataGlobal->NumOfZones);
                state.dataRoomAirMod->DSXMATFloor.allocate(state.dataGlobal->NumOfZones);
                state.dataRoomAirMod->DSXM2TFloor.allocate(state.dataGlobal->NumOfZones);
                state.dataRoomAirMod->DSXM3TFloor.allocate(state.dataGlobal->NumOfZones);
                state.dataRoomAirMod->DSXM4TFloor.allocate(state.dataGlobal->NumOfZones);
                state.dataRoomAirMod->MATOC.allocate(state.dataGlobal->NumOfZones);
                state.dataRoomAirMod->XMATOC.allocate(state.dataGlobal->NumOfZones);
                state.dataRoomAirMod->XM2TOC.allocate(state.dataGlobal->NumOfZones);
                state.dataRoomAirMod->XM3TOC.allocate(state.dataGlobal->NumOfZones);
                state.dataRoomAirMod->XM4TOC.allocate(state.dataGlobal->NumOfZones);
                state.dataRoomAirMod->DSXMATOC.allocate(state.dataGlobal->NumOfZones);
                state.dataRoomAirMod->DSXM2TOC.allocate(state.dataGlobal->NumOfZones);
                state.dataRoomAirMod->DSXM3TOC.allocate(state.dataGlobal->NumOfZones);
                state.dataRoomAirMod->DSXM4TOC.allocate(state.dataGlobal->NumOfZones);
                state.dataRoomAirMod->MATMX.allocate(state.dataGlobal->NumOfZones);
                state.dataRoomAirMod->XMATMX.allocate(state.dataGlobal->NumOfZones);
                state.dataRoomAirMod->XM2TMX.allocate(state.dataGlobal->NumOfZones);
                state.dataRoomAirMod->XM3TMX.allocate(state.dataGlobal->NumOfZones);
                state.dataRoomAirMod->XM4TMX.allocate(state.dataGlobal->NumOfZones);
                state.dataRoomAirMod->DSXMATMX.allocate(state.dataGlobal->NumOfZones);
                state.dataRoomAirMod->DSXM2TMX.allocate(state.dataGlobal->NumOfZones);
                state.dataRoomAirMod->DSXM3TMX.allocate(state.dataGlobal->NumOfZones);
                state.dataRoomAirMod->DSXM4TMX.allocate(state.dataGlobal->NumOfZones);
                state.dataRoomAirMod->ZTM1Floor.allocate(state.dataGlobal->NumOfZones);
                state.dataRoomAirMod->ZTM2Floor.allocate(state.dataGlobal->NumOfZones);
                state.dataRoomAirMod->ZTM3Floor.allocate(state.dataGlobal->NumOfZones);
                state.dataRoomAirMod->ZTM1OC.allocate(state.dataGlobal->NumOfZones);
                state.dataRoomAirMod->ZTM2OC.allocate(state.dataGlobal->NumOfZones);
                state.dataRoomAirMod->ZTM3OC.allocate(state.dataGlobal->NumOfZones);
                state.dataRoomAirMod->ZTM1MX.allocate(state.dataGlobal->NumOfZones);
                state.dataRoomAirMod->ZTM2MX.allocate(state.dataGlobal->NumOfZones);
                state.dataRoomAirMod->ZTM3MX.allocate(state.dataGlobal->NumOfZones);
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
                state.dataRoomAirMod->XMATFloor = 23.0;
                state.dataRoomAirMod->XM2TFloor = 23.0;
                state.dataRoomAirMod->XM3TFloor = 23.0;
                state.dataRoomAirMod->XM4TFloor = 23.0;
                state.dataRoomAirMod->DSXMATFloor = 23.0;
                state.dataRoomAirMod->DSXM2TFloor = 23.0;
                state.dataRoomAirMod->DSXM3TFloor = 23.0;
                state.dataRoomAirMod->DSXM4TFloor = 23.0;
                state.dataRoomAirMod->MATOC = 23.0;
                state.dataRoomAirMod->XMATOC = 23.0;
                state.dataRoomAirMod->XM2TOC = 23.0;
                state.dataRoomAirMod->XM3TOC = 23.0;
                state.dataRoomAirMod->XM4TOC = 23.0;
                state.dataRoomAirMod->DSXMATOC = 23.0;
                state.dataRoomAirMod->DSXM2TOC = 23.0;
                state.dataRoomAirMod->DSXM3TOC = 23.0;
                state.dataRoomAirMod->DSXM4TOC = 23.0;
                state.dataRoomAirMod->MATMX = 23.0;
                state.dataRoomAirMod->XMATMX = 23.0;
                state.dataRoomAirMod->XM2TMX = 23.0;
                state.dataRoomAirMod->XM3TMX = 23.0;
                state.dataRoomAirMod->XM4TMX = 23.0;
                state.dataRoomAirMod->DSXMATMX = 23.0;
                state.dataRoomAirMod->DSXM2TMX = 23.0;
                state.dataRoomAirMod->DSXM3TMX = 23.0;
                state.dataRoomAirMod->DSXM4TMX = 23.0;
                state.dataRoomAirMod->ZTM1Floor = 23.0;
                state.dataRoomAirMod->ZTM2Floor = 23.0;
                state.dataRoomAirMod->ZTM3Floor = 23.0;
                state.dataRoomAirMod->ZTM1OC = 23.0;
                state.dataRoomAirMod->ZTM2OC = 23.0;
                state.dataRoomAirMod->ZTM3OC = 23.0;
                state.dataRoomAirMod->ZTM1MX = 23.0;
                state.dataRoomAirMod->ZTM2MX = 23.0;
                state.dataRoomAirMod->ZTM3MX = 23.0;
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
                HCeiling = 0.0;
                HWall = 0.0;
                HFloor = 0.0;
                HInternal = 0.0;
                HWindow = 0.0;
                HDoor = 0.0;
            }

            if (any(state.dataRoomAirMod->IsZoneDV)) {

                state.dataRoomAirMod->DVHcIn.allocate(TotSurfaces);
                state.dataRoomAirMod->ZoneDVMixedFlagRep.allocate(state.dataGlobal->NumOfZones);
                state.dataRoomAirMod->ZoneDVMixedFlag.allocate(state.dataGlobal->NumOfZones);
                state.dataRoomAirMod->DVHcIn = 0.0;
                state.dataRoomAirMod->ZoneDVMixedFlagRep = 0.0;
                state.dataRoomAirMod->ZoneDVMixedFlag = 0;
                // Output variables and DV zone flag
                for (Loop = 1; Loop <= state.dataGlobal->NumOfZones; ++Loop) {
                    if (state.dataRoomAirMod->AirModel(Loop).AirModelType != DataRoomAirModel::RoomAirModel::UCSDDV) continue; // don't set these up if they don't make sense
                    // CurrentModuleObject='RoomAirSettings:ThreeNodeDisplacementVentilation'
                    SetupOutputVariable(state,
                        "Room Air Zone Mixed Subzone Temperature", OutputProcessor::Unit::C, state.dataRoomAirMod->ZTMX(Loop), "HVAC", "State", Zone(Loop).Name);
                    SetupOutputVariable(state,
                        "Room Air Zone Occupied Subzone Temperature", OutputProcessor::Unit::C, state.dataRoomAirMod->ZTOC(Loop), "HVAC", "State", Zone(Loop).Name);
                    SetupOutputVariable(state,
                        "Room Air Zone Floor Subzone Temperature", OutputProcessor::Unit::C, state.dataRoomAirMod->ZTFloor(Loop), "HVAC", "State", Zone(Loop).Name);
                    SetupOutputVariable(state,
                        "Room Air Zone Transition Height", OutputProcessor::Unit::m, state.dataRoomAirMod->HeightTransition(Loop), "HVAC", "State", Zone(Loop).Name);
                    SetupOutputVariable(state, "Room Air Zone Recommended Minimum Flow Fraction",
                                        OutputProcessor::Unit::None,
                                        state.dataRoomAirMod->FracMinFlow(Loop),
                                        "HVAC",
                                        "State",
                                        Zone(Loop).Name);
                    SetupOutputVariable(state,
                        "Room Air Zone Is Mixed Status", OutputProcessor::Unit::None, state.dataRoomAirMod->ZoneDVMixedFlagRep(Loop), "HVAC", "State", Zone(Loop).Name);
                    SetupOutputVariable(state, "Room Air Zone Average Temperature Gradient",
                                        OutputProcessor::Unit::K_m,
                                        state.dataRoomAirMod->AvgTempGrad(Loop),
                                        "HVAC",
                                        "State",
                                        Zone(Loop).Name);
                    SetupOutputVariable(state, "Room Air Zone Maximum Temperature Gradient",
                                        OutputProcessor::Unit::K_m,
                                        state.dataRoomAirMod->MaxTempGrad(Loop),
                                        "HVAC",
                                        "State",
                                        Zone(Loop).Name);
                    SetupOutputVariable(state, "Room Air Zone Thermal Comfort Effective Air Temperature",
                                        OutputProcessor::Unit::C,
                                        state.dataRoomAirMod->TCMF(Loop),
                                        "HVAC",
                                        "State",
                                        Zone(Loop).Name);
                    SetupOutputVariable(state,
                        "Room Air Zone Thermostat Temperature", OutputProcessor::Unit::C, TempTstatAir(Loop), "HVAC", "State", Zone(Loop).Name);
                }
            }

            if (any(state.dataRoomAirMod->IsZoneUI)) {
                state.dataRoomAirMod->ZoneUFMixedFlag.allocate(state.dataGlobal->NumOfZones);
                state.dataRoomAirMod->ZoneUFMixedFlagRep.allocate(state.dataGlobal->NumOfZones);
                state.dataRoomAirMod->UFHcIn.allocate(TotSurfaces);
                state.dataRoomAirMod->ZoneUFGamma.allocate(state.dataGlobal->NumOfZones);
                state.dataRoomAirMod->ZoneUFPowInPlumes.allocate(state.dataGlobal->NumOfZones);
                state.dataRoomAirMod->ZoneUFPowInPlumesfromWindows.allocate(state.dataGlobal->NumOfZones);
                state.dataRoomAirMod->ZoneUFMixedFlag = 0;
                state.dataRoomAirMod->ZoneUFMixedFlagRep = 0.0;
                state.dataRoomAirMod->UFHcIn = 0.0;
                state.dataRoomAirMod->ZoneUFGamma = 0.0;
                state.dataRoomAirMod->ZoneUFPowInPlumes = 0.0;
                state.dataRoomAirMod->ZoneUFPowInPlumesfromWindows = 0.0;
                // Output variables and UF zone flag
                for (Loop = 1; Loop <= state.dataGlobal->NumOfZones; ++Loop) {
                    if (state.dataRoomAirMod->AirModel(Loop).AirModelType != DataRoomAirModel::RoomAirModel::UCSDUFI) continue; // don't set these up if they don't make sense
                    // CurrentModuleObject='RoomAirSettings:UnderFloorAirDistributionInterior'
                    SetupOutputVariable(state,
                        "Room Air Zone Mixed Subzone Temperature", OutputProcessor::Unit::C, state.dataRoomAirMod->ZTMX(Loop), "HVAC", "State", Zone(Loop).Name);
                    SetupOutputVariable(state,
                        "Room Air Zone Occupied Subzone Temperature", OutputProcessor::Unit::C, state.dataRoomAirMod->ZTOC(Loop), "HVAC", "State", Zone(Loop).Name);
                    SetupOutputVariable(state,
                        "Room Air Zone Transition Height", OutputProcessor::Unit::m, state.dataRoomAirMod->HeightTransition(Loop), "HVAC", "State", Zone(Loop).Name);
                    SetupOutputVariable(state,
                        "Room Air Zone Is Mixed Status", OutputProcessor::Unit::None, state.dataRoomAirMod->ZoneUFMixedFlagRep(Loop), "HVAC", "State", Zone(Loop).Name);
                    SetupOutputVariable(state, "Room Air Zone Average Temperature Gradient",
                                        OutputProcessor::Unit::K_m,
                                        state.dataRoomAirMod->AvgTempGrad(Loop),
                                        "HVAC",
                                        "State",
                                        Zone(Loop).Name);
                    SetupOutputVariable(state,
                        "Room Air Zone Effective Comfort Air Temperature", OutputProcessor::Unit::C, state.dataRoomAirMod->TCMF(Loop), "HVAC", "State", Zone(Loop).Name);
                    SetupOutputVariable(state,
                        "Room Air Zone Thermostat Temperature", OutputProcessor::Unit::C, TempTstatAir(Loop), "HVAC", "State", Zone(Loop).Name);
                    SetupOutputVariable(state, "Room Air Zone Transition Height Gamma Value",
                                        OutputProcessor::Unit::None,
                                        state.dataRoomAirMod->ZoneUFGamma(Loop),
                                        "HVAC",
                                        "State",
                                        Zone(Loop).Name);
                    SetupOutputVariable(state, "Room Air Zone Plume Heat Transfer Rate",
                                        OutputProcessor::Unit::W,
                                        state.dataRoomAirMod->ZoneUFPowInPlumes(Loop),
                                        "HVAC",
                                        "State",
                                        Zone(Loop).Name);
                    SetupOutputVariable(state, "Room Air Zone Temperature Stratification Fraction",
                                        OutputProcessor::Unit::None,
                                        state.dataRoomAirMod->Phi(Loop),
                                        "HVAC",
                                        "State",
                                        Zone(Loop).Name);

                    // set zone equip pointer in the UCSDUI data structure
                    for (ZoneEquipConfigNum = 1; ZoneEquipConfigNum <= state.dataGlobal->NumOfZones; ++ZoneEquipConfigNum) {
                        if (state.dataZoneEquip->ZoneEquipConfig(ZoneEquipConfigNum).ActualZoneNum == Loop) {
                            state.dataRoomAirMod->ZoneUCSDUI(state.dataRoomAirMod->ZoneUFPtr(Loop)).ZoneEquipPtr = ZoneEquipConfigNum;
                            break;
                        }
                    } // ZoneEquipConfigNum
                }
                for (Loop = 1; Loop <= state.dataGlobal->NumOfZones; ++Loop) {
                    if (state.dataRoomAirMod->AirModel(Loop).AirModelType != DataRoomAirModel::RoomAirModel::UCSDUFE) continue; // don't set these up if they don't make sense
                    // CurrentModuleObject='RoomAirSettings:UnderFloorAirDistributionExterior'
                    SetupOutputVariable(state,
                        "Room Air Zone Mixed Subzone Temperature", OutputProcessor::Unit::C, state.dataRoomAirMod->ZTMX(Loop), "HVAC", "State", Zone(Loop).Name);
                    SetupOutputVariable(state,
                        "Room Air Zone Occupied Subzone Temperature", OutputProcessor::Unit::C, state.dataRoomAirMod->ZTOC(Loop), "HVAC", "State", Zone(Loop).Name);
                    SetupOutputVariable(state,
                        "Room Air Zone Transition Height", OutputProcessor::Unit::m, state.dataRoomAirMod->HeightTransition(Loop), "HVAC", "State", Zone(Loop).Name);
                    SetupOutputVariable(state,
                        "Room Air Zone Is Mixed Status", OutputProcessor::Unit::None, state.dataRoomAirMod->ZoneUFMixedFlagRep(Loop), "HVAC", "State", Zone(Loop).Name);
                    SetupOutputVariable(state, "Room Air Zone Average Temperature Gradient",
                                        OutputProcessor::Unit::K_m,
                                        state.dataRoomAirMod->AvgTempGrad(Loop),
                                        "HVAC",
                                        "State",
                                        Zone(Loop).Name);
                    SetupOutputVariable(state,
                        "Room Air Zone Effective Comfort Air Temperature", OutputProcessor::Unit::C, state.dataRoomAirMod->TCMF(Loop), "HVAC", "State", Zone(Loop).Name);
                    SetupOutputVariable(state,
                        "Room Air Zone Thermostat Temperature", OutputProcessor::Unit::C, TempTstatAir(Loop), "HVAC", "State", Zone(Loop).Name);
                    SetupOutputVariable(state, "Room Air Zone Transition Height Gamma Value",
                                        OutputProcessor::Unit::None,
                                        state.dataRoomAirMod->ZoneUFGamma(Loop),
                                        "HVAC",
                                        "State",
                                        Zone(Loop).Name);
                    SetupOutputVariable(state, "Room Air Zone Plume Heat Transfer Rate",
                                        OutputProcessor::Unit::W,
                                        state.dataRoomAirMod->ZoneUFPowInPlumes(Loop),
                                        "HVAC",
                                        "State",
                                        Zone(Loop).Name);
                    SetupOutputVariable(state, "Room Air Zone Window Plume Heat Transfer Rate",
                                        OutputProcessor::Unit::W,
                                        state.dataRoomAirMod->ZoneUFPowInPlumesfromWindows(Loop),
                                        "HVAC",
                                        "State",
                                        Zone(Loop).Name);
                    SetupOutputVariable(state, "Room Air Zone Temperature Stratification Fraction",
                                        OutputProcessor::Unit::None,
                                        state.dataRoomAirMod->Phi(Loop),
                                        "HVAC",
                                        "State",
                                        Zone(Loop).Name);
                    // set zone equip pointer in the UCSDUE data structure
                    for (ZoneEquipConfigNum = 1; ZoneEquipConfigNum <= state.dataGlobal->NumOfZones; ++ZoneEquipConfigNum) {
                        if (state.dataZoneEquip->ZoneEquipConfig(ZoneEquipConfigNum).ActualZoneNum == Loop) {
                            state.dataRoomAirMod->ZoneUCSDUE(state.dataRoomAirMod->ZoneUFPtr(Loop)).ZoneEquipPtr = ZoneEquipConfigNum;
                            break;
                        }
                    } // ZoneEquipConfigNum
                }
            }

            if (any(state.dataRoomAirMod->IsZoneCV)) {
                state.dataRoomAirMod->CVHcIn.allocate(TotSurfaces);
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
                state.dataRoomAirMod->ZoneCVisMixing.allocate(state.dataGlobal->NumOfZones);
                state.dataRoomAirMod->Rfr.allocate(state.dataGlobal->NumOfZones);
                state.dataRoomAirMod->ZoneCVhasREC.allocate(state.dataGlobal->NumOfZones);

                state.dataRoomAirMod->ZTJET = 23.0;
                state.dataRoomAirMod->RoomOutflowTemp = 23.0;
                state.dataRoomAirMod->ZTREC = 23.0;
                state.dataRoomAirMod->CVHcIn = 0.0;
                state.dataRoomAirMod->JetRecAreaRatio = 0.2;
                state.dataRoomAirMod->Urec = 0.2;
                state.dataRoomAirMod->Ujet = 0.2;
                state.dataRoomAirMod->Qrec = 0.2;
                state.dataRoomAirMod->Uhc = 0.2;
                state.dataRoomAirMod->Ain = 1.0;
                state.dataRoomAirMod->Tin = 23.0;
                state.dataRoomAirMod->Droom = 6.0;
                state.dataRoomAirMod->ZoneCVisMixing = 0.0;
                state.dataRoomAirMod->Rfr = 10.0;
                state.dataRoomAirMod->ZoneCVhasREC = 1.0;
                HCeiling = 0.0;
                HWall = 0.0;
                HFloor = 0.0;
                HInternal = 0.0;
                HWindow = 0.0;
                HDoor = 0.0;

                for (Loop = 1; Loop <= state.dataGlobal->NumOfZones; ++Loop) {
                    if (state.dataRoomAirMod->AirModel(Loop).AirModelType != DataRoomAirModel::RoomAirModel::UCSDCV) continue; // don't set these up if they don't make sense
                    ZoneEquipConfigNum = ZoneNum;
                    // check whether this zone is a controlled zone or not
                    if (state.dataZoneEquip->ZoneEquipConfig(ZoneEquipConfigNum).IsControlled) {
                        state.dataRoomAirMod->IsZoneCV(Loop) = false;
                        state.dataRoomAirMod->AirModel(Loop).SimAirModel = false;
                        ShowSevereError(state, "Unmixed Flow: Cross Ventilation cannot be applied for Zone=" + Zone(Loop).Name);
                        ShowContinueError(state, "An HVAC system is present in the zone. Fully mixed airflow model will be used for Zone=" +
                                          Zone(Loop).Name);
                        continue;
                    }
                    // CurrentModuleObject='RoomAirSettings:CrossVentilation'
                    SetupOutputVariable(state,
                        "Room Air Zone Jet Region Temperature", OutputProcessor::Unit::C, state.dataRoomAirMod->ZTJET(Loop), "Zone", "Average", Zone(Loop).Name);
                    SetupOutputVariable(state,
                        "Room Air Zone Recirculation Region Temperature", OutputProcessor::Unit::C, state.dataRoomAirMod->ZTREC(Loop), "Zone", "Average", Zone(Loop).Name);
                    SetupOutputVariable(state,
                        "Room Air Zone Jet Region Average Air Velocity", OutputProcessor::Unit::m_s, state.dataRoomAirMod->Ujet(Loop), "Zone", "Average", Zone(Loop).Name);
                    SetupOutputVariable(state, "Room Air Zone Recirculation Region Average Air Velocity",
                                        OutputProcessor::Unit::m_s,
                                        state.dataRoomAirMod->Urec(Loop),
                                        "Zone",
                                        "Average",
                                        Zone(Loop).Name);
                    SetupOutputVariable(state, "Room Air Zone Recirculation and Inflow Rate Ratio",
                                        OutputProcessor::Unit::None,
                                        state.dataRoomAirMod->RecInflowRatio(Loop),
                                        "Zone",
                                        "Average",
                                        Zone(Loop).Name);
                    SetupOutputVariable(state,
                        "Room Air Zone Inflow Opening Area", OutputProcessor::Unit::m2, state.dataRoomAirMod->Ain(Loop), "Zone", "Average", Zone(Loop).Name);
                    SetupOutputVariable(state, "Room Air Zone Room Length", OutputProcessor::Unit::m, state.dataRoomAirMod->Dstar(Loop), "Zone", "Average", Zone(Loop).Name);
                    SetupOutputVariable(state,
                        "Room Air Zone Is Mixing Status", OutputProcessor::Unit::None, state.dataRoomAirMod->ZoneCVisMixing(Loop), "Zone", "State", Zone(Loop).Name);
                    SetupOutputVariable(state,
                        "Room Air Zone Is Recirculating Status", OutputProcessor::Unit::None, state.dataRoomAirMod->ZoneCVhasREC(Loop), "Zone", "State", Zone(Loop).Name);
                    for (i = 1; i <= state.dataRoomAirMod->AirflowNetworkSurfaceUCSDCV(0, ZoneNum); ++i) {
                        N = AirflowNetwork::AirflowNetworkLinkageData(i).CompNum;
                        if (AirflowNetwork::AirflowNetworkCompData(N).CompTypeNum == AirflowNetwork::CompTypeNum_DOP) {
                            SurfNum = AirflowNetwork::MultizoneSurfaceData(i).SurfNum;
                            SetupOutputVariable(state, "Room Air Window Jet Region Average Air Velocity",
                                                OutputProcessor::Unit::m_s,
                                                state.dataRoomAirMod->CVJetRecFlows(i, Loop).Ujet,
                                                "Zone",
                                                "Average",
                                                AirflowNetwork::MultizoneSurfaceData(i).SurfName);
                        }
                    }
                }
            }

            MyEnvrnFlag = true;

            MyOneTimeFlag = false;
        }

        // Do the Begin Environment initializations
        if (state.dataGlobal->BeginEnvrnFlag && MyEnvrnFlag(ZoneNum)) {

            if (state.dataRoomAirMod->IsZoneDV(ZoneNum) || state.dataRoomAirMod->IsZoneUI(ZoneNum)) {

                state.dataRoomAirMod->MaxTempGrad(ZoneNum) = 0.0;
                state.dataRoomAirMod->AvgTempGrad(ZoneNum) = 0.0;
                state.dataRoomAirMod->TCMF(ZoneNum) = 23.0;
                state.dataRoomAirMod->FracMinFlow(ZoneNum) = 0.0;
                state.dataRoomAirMod->ZoneAirSystemON(ZoneNum) = false;
                state.dataRoomAirMod->MATFloor(ZoneNum) = 23.0;
                state.dataRoomAirMod->XMATFloor(ZoneNum) = 23.0;
                state.dataRoomAirMod->XM2TFloor(ZoneNum) = 23.0;
                state.dataRoomAirMod->XM3TFloor(ZoneNum) = 23.0;
                state.dataRoomAirMod->XM4TFloor(ZoneNum) = 23.0;
                state.dataRoomAirMod->DSXMATFloor(ZoneNum) = 23.0;
                state.dataRoomAirMod->DSXM2TFloor(ZoneNum) = 23.0;
                state.dataRoomAirMod->DSXM3TFloor(ZoneNum) = 23.0;
                state.dataRoomAirMod->DSXM4TFloor(ZoneNum) = 23.0;
                state.dataRoomAirMod->MATOC(ZoneNum) = 23.0;
                state.dataRoomAirMod->XMATOC(ZoneNum) = 23.0;
                state.dataRoomAirMod->XM2TOC(ZoneNum) = 23.0;
                state.dataRoomAirMod->XM3TOC(ZoneNum) = 23.0;
                state.dataRoomAirMod->XM4TOC(ZoneNum) = 23.0;
                state.dataRoomAirMod->DSXMATOC(ZoneNum) = 23.0;
                state.dataRoomAirMod->DSXM2TOC(ZoneNum) = 23.0;
                state.dataRoomAirMod->DSXM3TOC(ZoneNum) = 23.0;
                state.dataRoomAirMod->DSXM4TOC(ZoneNum) = 23.0;
                state.dataRoomAirMod->MATMX(ZoneNum) = 23.0;
                state.dataRoomAirMod->XMATMX(ZoneNum) = 23.0;
                state.dataRoomAirMod->XM2TMX(ZoneNum) = 23.0;
                state.dataRoomAirMod->XM3TMX(ZoneNum) = 23.0;
                state.dataRoomAirMod->XM4TMX(ZoneNum) = 23.0;
                state.dataRoomAirMod->DSXMATMX(ZoneNum) = 23.0;
                state.dataRoomAirMod->DSXM2TMX(ZoneNum) = 23.0;
                state.dataRoomAirMod->DSXM3TMX(ZoneNum) = 23.0;
                state.dataRoomAirMod->DSXM4TMX(ZoneNum) = 23.0;
                state.dataRoomAirMod->ZTM1Floor(ZoneNum) = 23.0;
                state.dataRoomAirMod->ZTM2Floor(ZoneNum) = 23.0;
                state.dataRoomAirMod->ZTM3Floor(ZoneNum) = 23.0;
                state.dataRoomAirMod->Zone1Floor(ZoneNum) = 23.0;
                state.dataRoomAirMod->ZoneMXFloor(ZoneNum) = 23.0;
                state.dataRoomAirMod->ZoneM2Floor(ZoneNum) = 23.0;
                state.dataRoomAirMod->ZTM1OC(ZoneNum) = 23.0;
                state.dataRoomAirMod->ZTM2OC(ZoneNum) = 23.0;
                state.dataRoomAirMod->ZTM3OC(ZoneNum) = 23.0;
                state.dataRoomAirMod->Zone1OC(ZoneNum) = 23.0;
                state.dataRoomAirMod->ZoneMXOC(ZoneNum) = 23.0;
                state.dataRoomAirMod->ZoneM2OC(ZoneNum) = 23.0;
                state.dataRoomAirMod->ZTM1MX(ZoneNum) = 23.0;
                state.dataRoomAirMod->ZTM2MX(ZoneNum) = 23.0;
                state.dataRoomAirMod->ZTM3MX(ZoneNum) = 23.0;
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
                HCeiling = 0.0;
                HWall = 0.0;
                HFloor = 0.0;
                HInternal = 0.0;
                HWindow = 0.0;
                HDoor = 0.0;
            }

            if (state.dataRoomAirMod->IsZoneDV(ZoneNum)) {

                state.dataRoomAirMod->DVHcIn = 0.0;
                state.dataRoomAirMod->ZoneDVMixedFlagRep(ZoneNum) = 0.0;
                state.dataRoomAirMod->ZoneDVMixedFlag(ZoneNum) = 0;
            }

            if (state.dataRoomAirMod->IsZoneUI(ZoneNum)) {

                state.dataRoomAirMod->UFHcIn = 0.0;
                state.dataRoomAirMod->ZoneUFMixedFlag(ZoneNum) = 0;
                state.dataRoomAirMod->ZoneUFMixedFlagRep(ZoneNum) = 0.0;
                state.dataRoomAirMod->ZoneUFGamma(ZoneNum) = 0.0;
                state.dataRoomAirMod->ZoneUFPowInPlumes(ZoneNum) = 0.0;
                state.dataRoomAirMod->ZoneUFPowInPlumesfromWindows(ZoneNum) = 0.0;
            }

            if (state.dataRoomAirMod->IsZoneCV(ZoneNum)) {
                state.dataRoomAirMod->ZTJET(ZoneNum) = 23.0;
                state.dataRoomAirMod->RoomOutflowTemp(ZoneNum) = 23.0;
                state.dataRoomAirMod->ZTREC(ZoneNum) = 23.0;
                state.dataRoomAirMod->CVHcIn = 0.0;
                state.dataRoomAirMod->JetRecAreaRatio(ZoneNum) = 0.2;
                state.dataRoomAirMod->Urec(ZoneNum) = 0.2;
                state.dataRoomAirMod->Ujet(ZoneNum) = 0.2;
                state.dataRoomAirMod->Uhc(ZoneNum) = 0.2;
                state.dataRoomAirMod->Ain(ZoneNum) = 1.0;
                state.dataRoomAirMod->Tin(ZoneNum) = 23.0;
                state.dataRoomAirMod->Droom(ZoneNum) = 6.0;
                state.dataRoomAirMod->Dstar(ZoneNum) = 6.0;
                state.dataRoomAirMod->ZoneCVisMixing(ZoneNum) = 0.0;
                state.dataRoomAirMod->Rfr(ZoneNum) = 10.0;
                state.dataRoomAirMod->ZoneCVhasREC(ZoneNum) = 1.0;
                HCeiling = 0.0;
                HWall = 0.0;
                HFloor = 0.0;
                HInternal = 0.0;
                HWindow = 0.0;
                HDoor = 0.0;
            }

            MyEnvrnFlag(ZoneNum) = false;
        } // end one time inits

        if (!state.dataGlobal->BeginEnvrnFlag) {
            MyEnvrnFlag(ZoneNum) = true;
        }
    }

    void GetRAFNNodeNum(EnergyPlusData &state, std::string const &RAFNNodeName, // Name of RoomAir:Node:AirflowNetwork
                        int &ZoneNum,                    // The zone number associate with the node name
                        int &RAFNNodeNum,                // RoomAir:Node:AirflowNetwork Number
                        bool &Errorfound                 // true if an error is found (TODO: Useless, RAFNodeNum is 0 when Errorfound is true)
    )
    {

        // FUNCTION INFORMATION:
        //       AUTHOR         Lixing Gu
        //       DATE WRITTEN   November 2014
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS FUNCTION:
        // This function looks up the given RoomAirNode name and returns the Zone number and RoomAir node
        // number. If incorrect name is given, errorsfound is returned as true and value is returned
        // as zero.

        // FUNCTION LOCAL VARIABLE DECLARATIONS:
        int I; // Zone index

        // Obtains and Allocates RoomAirSettings : AirflowNetwork
        if (GetAirModelData) {
            GetAirModelDatas(state);
            GetAirModelData = false;
        }

        Errorfound = false;
        RAFNNodeNum = 0;
        for (I = 1; I <= state.dataGlobal->NumOfZones; ++I) {
            if (state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(I).NumOfAirNodes > 0) {
                RAFNNodeNum =
                    UtilityRoutines::FindItemInList(RAFNNodeName, state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(I).Node, state.dataRoomAirMod->RoomAirflowNetworkZoneInfo(I).NumOfAirNodes);
                if (RAFNNodeNum > 0) {
                    ZoneNum = I;
                    break;
                }
            }
        }

        if (RAFNNodeNum == 0) {
            Errorfound = true;
            ShowSevereError(state, "Could not find RoomAir:Node:AirflowNetwork number with AirflowNetwork:IntraZone:Node Name='" + RAFNNodeName);
        }
    }

    bool CheckEquipName(EnergyPlusData &state, std::string const &EquipType, // Equipment type
                        std::string const &EquipName, // Equipment Name
                        std::string &SupplyNodeName,  // Supply node name
                        std::string &ReturnNodeName,  // Return node name
                        int TotNumEquip,              // how many of this equipment type
                        int TypeNum                   // equipment type number
    )
    {

        // FUNCTION INFORMATION:
        //       AUTHOR         Lixing Gu
        //       DATE WRITTEN   March 2014
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS FUNCTION:
        // This function looks up the given RoomAirNode name and returns the Zone number and RoomAir node
        // number.If incorrect name is given, errorsfound is returned as true and value is returned
        // as zero.

        // Using/Aliasing
        using DataLoopNode::NodeID;
        using namespace DataIPShortCuts;
        using Fans::GetFanOutletNode;

        // Return value
        bool EquipFind; // True if an error is found

        // FUNCTION LOCAL VARIABLE DECLARATIONS:
        int NumAlphas;
        int NumNumbers;
        int I;
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

        if (TypeNum == 0) return EquipFind;

        inputProcessor->getObjectDefMaxArgs(state, EquipType, TotalArgs, NumAlphas, NumNumbers);

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

        for (I = 1; I <= TotNumEquip; ++I) {
            inputProcessor->getObjectItem(state, EquipType, I, Alphas, NumAlphas, Numbers, NumNumbers, Status);
            if (UtilityRoutines::SameString(Alphas(1), EquipName)) {
                EquipFind = true;
                break;
            }
        }

        if (TypeNum == DataHVACGlobals::ZoneEquipTypeOf_VariableRefrigerantFlow) { // ZoneHVAC:TerminalUnit : VariableRefrigerantFlow
            SupplyNodeName = Alphas(4);
            ReturnNodeName = "";   // Zone return node
        } else if (TypeNum == DataHVACGlobals::ZoneEquipTypeOf_EnergyRecoveryVentilator) { // ZoneHVAC : EnergyRecoveryVentilator
            I = GetFanOutletNode(state, "Fan:OnOff", Alphas(4), errorfound);
            if (errorfound) {
            }
            SupplyNodeName = NodeID(I); // ?????
            ReturnNodeName = "";        // Zone exhaust node
        } else if (TypeNum == DataHVACGlobals::ZoneEquipTypeOf_FourPipeFanCoil) {      // ZoneHVAC : FourPipeFanCoil
            SupplyNodeName = Alphas(6);
            ReturnNodeName = Alphas(5);
        } else if (TypeNum == DataHVACGlobals::ZoneEquipTypeOf_OutdoorAirUnit) { // ZoneHVAC : OutdoorAirUnit
            SupplyNodeName = Alphas(13);
            ReturnNodeName = Alphas(14);
        } else if (TypeNum == DataHVACGlobals::ZoneEquipTypeOf_PackagedTerminalAirConditioner) { // ZoneHVAC : PackagedTerminalAirConditioner
            SupplyNodeName = Alphas(4);
            ReturnNodeName = Alphas(3);
        } else if (TypeNum == DataHVACGlobals::ZoneEquipTypeOf_PackagedTerminalHeatPump) { // ZoneHVAC : PackagedTerminalHeatPump
            SupplyNodeName = Alphas(4);
            ReturnNodeName = Alphas(3);
        } else if (TypeNum == DataHVACGlobals::ZoneEquipTypeOf_UnitHeater) { // ZoneHVAC : UnitHeater
            SupplyNodeName = Alphas(4);
            ReturnNodeName = Alphas(3);
        } else if (TypeNum == DataHVACGlobals::ZoneEquipTypeOf_UnitVentilator) { // ZoneHVAC : UnitVentilator
            SupplyNodeName = Alphas(7);
            ReturnNodeName = Alphas(6);
        } else if (TypeNum == DataHVACGlobals::ZoneEquipTypeOf_VentilatedSlab) { // ZoneHVAC : VentilatedSlab
            SupplyNodeName = Alphas(20);
            ReturnNodeName = Alphas(18);
        } else if (TypeNum == DataHVACGlobals::ZoneEquipTypeOf_WaterToAirHeatPump) { // ZoneHVAC : WaterToAirHeatPump
            SupplyNodeName = Alphas(4);
            ReturnNodeName = Alphas(3);
        } else if (TypeNum == DataHVACGlobals::ZoneEquipTypeOf_WindowAirConditioner) { // ZoneHVAC : WindowAirConditioner
            SupplyNodeName = Alphas(4);
            ReturnNodeName = Alphas(3);
        } else if (TypeNum == DataHVACGlobals::ZoneEquipTypeOf_BaseboardRadiantConvectiveElectric) { // ZoneHVAC : Baseboard : RadiantConvective : Electric
            SupplyNodeName = "";    // convection only
        } else if (TypeNum == DataHVACGlobals::ZoneEquipTypeOf_BaseboardRadiantConvectiveWater) { // ZoneHVAC : Baseboard : RadiantConvective : Water
            SupplyNodeName = "";
        } else if (TypeNum == DataHVACGlobals::ZoneEquipTypeOf_BaseboardRadiantConvectiveSteam) { // ZoneHVAC : Baseboard : RadiantConvective : Steam
            SupplyNodeName = "";
        } else if (TypeNum == DataHVACGlobals::ZoneEquipTypeOf_BaseboardConvectiveElectric) { // ZoneHVAC : Baseboard : Convective : Electric
            SupplyNodeName = "";
        } else if (TypeNum == DataHVACGlobals::ZoneEquipTypeOf_BaseboardConvectiveWater) { // ZoneHVAC : Baseboard : Convective : Water
            SupplyNodeName = "";
        } else if (TypeNum == DataHVACGlobals::ZoneEquipTypeOf_HighTemperatureRadiant) { // ZoneHVAC : HighTemperatureRadiant
            SupplyNodeName = "";
        } else if (TypeNum == DataHVACGlobals::ZoneEquipTypeOf_DehumidifierDX) { // ZoneHVAC : Dehumidifier : DX
            SupplyNodeName = Alphas(4);
            ReturnNodeName = Alphas(3);
        } else if (TypeNum == DataHVACGlobals::ZoneEquipTypeOf_IdealLoadsAirSystem) { // ZoneHVAC : IdealLoadsAirSystem
            SupplyNodeName = Alphas(3);
            ReturnNodeName = Alphas(4);
        } else if (TypeNum == DataHVACGlobals::ZoneEquipTypeOf_RefrigerationChillerSet) { // ZoneHVAC : RefrigerationChillerSet
            SupplyNodeName = Alphas(5);
            ReturnNodeName = Alphas(4);
        } else if (TypeNum == DataHVACGlobals::ZoneEquipTypeOf_HybridUnitaryAirConditioners) { // ZoneHVAC : HybridUnitaryAirConditioners
            SupplyNodeName = Alphas(11);
            ReturnNodeName = Alphas(9);
        } else if (TypeNum == DataHVACGlobals::ZoneEquipTypeOf_FanZoneExhaust) { // Fan : ZoneExhaust
            SupplyNodeName = "";    // ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? May not use
        } else if (TypeNum == DataHVACGlobals::ZoneEquipTypeOf_WaterHeaterHeatPump) { // WaterHeater : HeatPump
            SupplyNodeName = Alphas(8);
            ReturnNodeName = Alphas(7);
            // For AirTerminals, find matching return node later
        } else if (TypeNum == DataHVACGlobals::ZoneEquipTypeOf_AirTerminalDualDuctConstantVolume) { // AirTerminal : DualDuct : ConstantVolume
            SupplyNodeName = Alphas(1);
            ReturnNodeName = "";
        } else if (TypeNum == DataHVACGlobals::ZoneEquipTypeOf_AirTerminalDualDuctVAV) { // AirTerminal : DualDuct : VAV
            SupplyNodeName = Alphas(1);
            ReturnNodeName = "";
        } else if (TypeNum == DataHVACGlobals::ZoneEquipTypeOf_AirTerminalSingleDuctConstantVolumeReheat) { // AirTerminal : SingleDuct : ConstantVolume : Reheat
            SupplyNodeName = Alphas(1);
            ReturnNodeName = "";
        } else if (TypeNum == DataHVACGlobals::ZoneEquipTypeOf_AirTerminalSingleDuctConstantVolumeNoReheat) { // AirTerminal : SingleDuct : ConstantVolume : NoReheat
            SupplyNodeName = Alphas(4);
            ReturnNodeName = "";
        } else if (TypeNum == DataHVACGlobals::ZoneEquipTypeOf_AirTerminalSingleDuctVAVReheat) { // AirTerminal : SingleDuct : VAV : Reheat
            SupplyNodeName = Alphas(1);
            ReturnNodeName = "";
        } else if (TypeNum == DataHVACGlobals::ZoneEquipTypeOf_AirTerminalSingleDuctVAVNoReheat) { // AirTerminal : SingleDuct : VAV : NoReheat
            SupplyNodeName = Alphas(1);
            ReturnNodeName = "";
        } else if (TypeNum == DataHVACGlobals::ZoneEquipTypeOf_AirTerminalSingleDuctSeriesPIUReheat) { // AirTerminal : SingleDuct : SeriesPIU : Reheat
            SupplyNodeName = Alphas(1);
            ReturnNodeName = "";
        } else if (TypeNum == DataHVACGlobals::ZoneEquipTypeOf_AirTerminalSingleDuctParallelPIUReheat) { // AirTerminal : SingleDuct : ParallelPIU : Reheat
            SupplyNodeName = Alphas(1);
            ReturnNodeName = "";
        } else if (TypeNum == DataHVACGlobals::ZoneEquipTypeOf_AirTerminalSingleDuctCAVFourPipeInduction) { // AirTerminal : SingleDuct : ConstantVolume : FourPipeInduction
            SupplyNodeName = Alphas(1);
            ReturnNodeName = "";
        } else if (TypeNum == DataHVACGlobals::ZoneEquipTypeOf_AirTerminalSingleDuctVAVReheatVariableSpeedFan) { // AirTerminal : SingleDuct : VAV : Reheat : VariableSpeedFan
            SupplyNodeName = Alphas(1);
            ReturnNodeName = "";
        } else if (TypeNum == DataHVACGlobals::ZoneEquipTypeOf_AirTerminalSingleDuctVAVHeatAndCoolReheat) { // AirTerminal : SingleDuct : VAV : HeatAndCool : Reheat
            SupplyNodeName = Alphas(1);
            ReturnNodeName = "";
        } else if (TypeNum == DataHVACGlobals::ZoneEquipTypeOf_AirTerminalSingleDuctVAVHeatAndCoolNoReheat) { // AirTerminal : SingleDuct : VAV : HeatAndCool : NoReheat
            SupplyNodeName = Alphas(1);
            ReturnNodeName = "";
        } else if (TypeNum == DataHVACGlobals::ZoneEquipTypeOf_AirTerminalSingleDuctConstantVolumeCooledBeam) { // AirTerminal : SingleDuct : ConstantVolume : CooledBeam
            SupplyNodeName = Alphas(5);
            ReturnNodeName = "";
        } else if (TypeNum == DataHVACGlobals::ZoneEquipTypeOf_AirTerminalDualDuctVAVOutdoorAir) { // AirTerminal : DualDuct : VAV : OutdoorAir
            SupplyNodeName = Alphas(3);
            ReturnNodeName = "";
        } else if (TypeNum == DataHVACGlobals::ZoneEquipTypeOf_AirLoopHVACReturnAir) {     // AirLoopHVACReturnAir
            SupplyNodeName = Alphas(4); //
            ReturnNodeName = "";        //
        }

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

} // namespace RoomAirModelManager

} // namespace EnergyPlus
