// EnergyPlus, Copyright (c) 1996-2020, The Board of Trustees of the University of Illinois,
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

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>
#include <ObjexxFCL/Fmath.hh>

// EnergyPlus Headers
#include <EnergyPlus/AirTerminalUnit.hh>
#include <EnergyPlus/BranchNodeConnections.hh>
#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/DataAirLoop.hh>
#include <EnergyPlus/DataDefineEquip.hh>
#include <EnergyPlus/DataHeatBalance.hh>
#include <EnergyPlus/DataHVACGlobals.hh>
#include <EnergyPlus/DataLoopNode.hh>
#include <EnergyPlus/DataSizing.hh>
#include <EnergyPlus/DataZoneEquipment.hh>
#include <EnergyPlus/DualDuct.hh>
#include <EnergyPlus/General.hh>
#include <EnergyPlus/GeneralRoutines.hh>
#include <EnergyPlus/HVACCooledBeam.hh>
#include <EnergyPlus/HVACFourPipeBeam.hh>
#include <EnergyPlus/HVACSingleDuctInduc.hh>
#include <EnergyPlus/InputProcessing/InputProcessor.hh>
#include <EnergyPlus/NodeInputManager.hh>
#include <EnergyPlus/OutputProcessor.hh>
#include <EnergyPlus/PoweredInductionUnits.hh>
#include <EnergyPlus/Psychrometrics.hh>
#include <EnergyPlus/SingleDuct.hh>
#include <EnergyPlus/UserDefinedComponents.hh>
#include <EnergyPlus/UtilityRoutines.hh>
#include <EnergyPlus/ZoneAirLoopEquipmentManager.hh>

namespace EnergyPlus {

namespace ZoneAirLoopEquipmentManager {
    // Module containing the routines dealing with the ZoneAirLoopEquipmentManager

    // MODULE INFORMATION:
    //       AUTHOR         Russ Taylor
    //       DATE WRITTEN   May 1997

    using DataGlobals::BeginDayFlag;
    using DataGlobals::BeginEnvrnFlag;
    using DataGlobals::BeginHourFlag;
    using DataGlobals::BeginTimeStepFlag;
    using DataGlobals::NumOfZones;
    using DataHVACGlobals::FirstTimeStepSysFlag;
    using namespace DataDefineEquip;

    void ManageZoneAirLoopEquipment(EnergyPlusData &state, std::string const &ZoneAirLoopEquipName,
                                    bool const FirstHVACIteration,
                                    Real64 &SysOutputProvided,
                                    Real64 &NonAirSysOutput,
                                    Real64 &LatOutputProvided, // Latent add/removal supplied by air dist unit (kg/s), dehumid = negative
                                    int const ActualZoneNum,
                                    int &ControlledZoneNum,
                                    int &CompIndex)
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR         Russ Taylor
        //       DATE WRITTEN   May 1997
        //       MODIFIED       Don Shirey, Aug 2009 (LatOutputProvided)

        // PURPOSE OF THIS SUBROUTINE:
        // Calls the zone thermal control simulations and the interfaces
        // (water-air, refrigerant-air, steam-air, electric-electric,
        // water-water, etc)

        using General::TrimSigDigits;

        int AirDistUnitNum;

        // Beginning of Code

        GetZoneAirLoopEquipment(state);

        // Find the correct Zone Air Distribution Unit Equipment
        if (CompIndex == 0) {
            AirDistUnitNum = UtilityRoutines::FindItemInList(ZoneAirLoopEquipName, AirDistUnit);
            if (AirDistUnitNum == 0) {
                ShowFatalError("ManageZoneAirLoopEquipment: Unit not found=" + ZoneAirLoopEquipName);
            }
            CompIndex = AirDistUnitNum;
        } else {
            AirDistUnitNum = CompIndex;
            if (AirDistUnitNum > NumAirDistUnits || AirDistUnitNum < 1) {
                ShowFatalError("ManageZoneAirLoopEquipment:  Invalid CompIndex passed=" + TrimSigDigits(AirDistUnitNum) +
                               ", Number of Units=" + TrimSigDigits(NumAirDistUnits) + ", Entered Unit name=" + ZoneAirLoopEquipName);
            }
            if (ZoneAirLoopEquipName != AirDistUnit(AirDistUnitNum).Name) {
                ShowFatalError("ManageZoneAirLoopEquipment: Invalid CompIndex passed=" + TrimSigDigits(AirDistUnitNum) +
                               ", Unit name=" + ZoneAirLoopEquipName + ", stored Unit Name for that index=" + AirDistUnit(AirDistUnitNum).Name);
            }
        }
        DataSizing::CurTermUnitSizingNum = AirDistUnit(AirDistUnitNum).TermUnitSizingNum;
        InitZoneAirLoopEquipment(state, AirDistUnitNum, ControlledZoneNum, ActualZoneNum);
        InitZoneAirLoopEquipmentTimeStep(AirDistUnitNum);

        SimZoneAirLoopEquipment(state,
            AirDistUnitNum, SysOutputProvided, NonAirSysOutput, LatOutputProvided, FirstHVACIteration, ControlledZoneNum, ActualZoneNum);

        // Call one-time init to fill termunit sizing and other data for the ADU - can't do this until the actual terminal unit nodes have been
        // matched to zone euqip config nodes
        InitZoneAirLoopEquipment(state, AirDistUnitNum, ControlledZoneNum, ActualZoneNum);
    }

    void GetZoneAirLoopEquipment(EnergyPlusData &state)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Russ Taylor
        //       DATE WRITTEN   June 1997
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Get all the system related equipment which may be attached to
        // a zone

        // METHODOLOGY EMPLOYED:
        // Needs description, as appropriate.

        // REFERENCES:
        // na

        // Using/Aliasing
        using NodeInputManager::GetOnlySingleNode;
        using namespace DataLoopNode;
        using BranchNodeConnections::SetUpCompSets;
        using DualDuct::GetDualDuctOutdoorAirRecircUse;

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:
        // na

        // SUBROUTINE PARAMETER DEFINITIONS:
        static std::string const RoutineName("GetZoneAirLoopEquipment: ");            // include trailing blank space
        static std::string const CurrentModuleObject("ZoneHVAC:AirDistributionUnit"); // Object type for getting and error messages

        // INTERFACE BLOCK SPECIFICATIONS
        // na

        // DERIVED TYPE DEFINITIONS
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int AirDistUnitNum;
        int AirDistCompUnitNum;
        int NumAlphas;
        int NumNums;
        int IOStat;
        static Array1D_string AlphArray(5);      // Tuned Made static
        static Array1D<Real64> NumArray(2);      // Tuned Made static
        bool ErrorsFound(false);          // If errors detected in input
        bool IsNotOK;                            // Flag to verify name
        static Array1D_string cAlphaFields(5);   // Alpha field names //Tuned Made static
        static Array1D_string cNumericFields(2); // Numeric field names //Tuned Made static
        static Array1D_bool lAlphaBlanks(5);     // Logical array, alpha field input BLANK = .TRUE. //Tuned Made static
        static Array1D_bool lNumericBlanks(2);   // Logical array, numeric field input BLANK = .TRUE. //Tuned Made static
        bool DualDuctRecircIsUsed;               // local temporary for deciding if recirc side used by dual duct terminal
        // make sure the input data is read in only once
        if (!state.dataZoneAirLoopEquipmentManager->GetAirDistUnitsFlag) {
            return;
        } else {
            state.dataZoneAirLoopEquipmentManager->GetAirDistUnitsFlag = false;
        }

        NumAirDistUnits = inputProcessor->getNumObjectsFound(CurrentModuleObject);

        AirDistUnit.allocate(NumAirDistUnits);
        state.dataZoneAirLoopEquipmentManager->EachOnceFlag.allocate(NumAirDistUnits);
        state.dataZoneAirLoopEquipmentManager->EachOnceFlag = true;

        if (NumAirDistUnits > 0) {

            for (AirDistUnitNum = 1; AirDistUnitNum <= NumAirDistUnits; ++AirDistUnitNum) {
                inputProcessor->getObjectItem(state,
                                              CurrentModuleObject,
                                              AirDistUnitNum,
                                              AlphArray,
                                              NumAlphas,
                                              NumArray,
                                              NumNums,
                                              IOStat,
                                              lNumericBlanks,
                                              lAlphaBlanks,
                                              cAlphaFields,
                                              cNumericFields); //  data for one zone
                UtilityRoutines::IsNameEmpty(AlphArray(1), CurrentModuleObject, ErrorsFound);

                AirDistUnit(AirDistUnitNum).Name = AlphArray(1);
                // Input Outlet Node Num
                AirDistUnit(AirDistUnitNum).OutletNodeNum = GetOnlySingleNode(state,
                    AlphArray(2), ErrorsFound, CurrentModuleObject, AlphArray(1), NodeType_Air, NodeConnectionType_Outlet, 1, ObjectIsParent);
                AirDistUnit(AirDistUnitNum).InletNodeNum = 0;
                AirDistUnit(AirDistUnitNum).NumComponents = 1;
                AirDistCompUnitNum = 1;
                // Load the air Distribution Unit Equip and Name
                AirDistUnit(AirDistUnitNum).EquipType(AirDistCompUnitNum) = AlphArray(3);
                AirDistUnit(AirDistUnitNum).EquipName(AirDistCompUnitNum) = AlphArray(4);
                ValidateComponent(state, AlphArray(3), AlphArray(4), IsNotOK, CurrentModuleObject);
                if (IsNotOK) {
                    ShowContinueError("In " + CurrentModuleObject + " = " + AlphArray(1));
                    ErrorsFound = true;
                }
                AirDistUnit(AirDistUnitNum).UpStreamLeakFrac = NumArray(1);
                AirDistUnit(AirDistUnitNum).DownStreamLeakFrac = NumArray(2);
                if (AirDistUnit(AirDistUnitNum).DownStreamLeakFrac <= 0.0) {
                    AirDistUnit(AirDistUnitNum).LeakLoadMult = 1.0;
                } else if (AirDistUnit(AirDistUnitNum).DownStreamLeakFrac < 1.0 && AirDistUnit(AirDistUnitNum).DownStreamLeakFrac > 0.0) {
                    AirDistUnit(AirDistUnitNum).LeakLoadMult = 1.0 / (1.0 - AirDistUnit(AirDistUnitNum).DownStreamLeakFrac);
                } else {
                    ShowSevereError("Error found in " + CurrentModuleObject + " = " + AirDistUnit(AirDistUnitNum).Name);
                    ShowContinueError(cNumericFields(2) + " must be less than 1.0");
                    ErrorsFound = true;
                }
                if (AirDistUnit(AirDistUnitNum).UpStreamLeakFrac > 0.0) {
                    AirDistUnit(AirDistUnitNum).UpStreamLeak = true;
                } else {
                    AirDistUnit(AirDistUnitNum).UpStreamLeak = false;
                }
                if (AirDistUnit(AirDistUnitNum).DownStreamLeakFrac > 0.0) {
                    AirDistUnit(AirDistUnitNum).DownStreamLeak = true;
                } else {
                    AirDistUnit(AirDistUnitNum).DownStreamLeak = false;
                }

                // DesignSpecification:AirTerminal:Sizing name
                AirDistUnit(AirDistUnitNum).AirTerminalSizingSpecIndex = 0;
                if (!lAlphaBlanks(5)) {
                    AirDistUnit(AirDistUnitNum).AirTerminalSizingSpecIndex =
                        UtilityRoutines::FindItemInList(AlphArray(5), DataSizing::AirTerminalSizingSpec);
                    if (AirDistUnit(AirDistUnitNum).AirTerminalSizingSpecIndex == 0) {
                        ShowSevereError(cAlphaFields(5) + " = " + AlphArray(5) + " not found.");
                        ShowContinueError("Occurs in " + CurrentModuleObject + " = " + AirDistUnit(AirDistUnitNum).Name);
                        ErrorsFound = true;
                    }
                }
                // Validate EquipType for Air Distribution Unit
                if (UtilityRoutines::SameString(AirDistUnit(AirDistUnitNum).EquipType(AirDistCompUnitNum), "AirTerminal:DualDuct:ConstantVolume")) {
                    AirDistUnit(AirDistUnitNum).EquipType_Num(AirDistCompUnitNum) = DualDuctConstVolume;
                    if (AirDistUnit(AirDistUnitNum).UpStreamLeak || AirDistUnit(AirDistUnitNum).DownStreamLeak) {
                        ShowSevereError("Error found in " + CurrentModuleObject + " = " + AirDistUnit(AirDistUnitNum).Name);
                        ShowContinueError("Simple duct leakage model not available for " + cAlphaFields(3) + " = " +
                                          AirDistUnit(AirDistUnitNum).EquipType(AirDistCompUnitNum));
                        ErrorsFound = true;
                    }
                } else if (UtilityRoutines::SameString(AirDistUnit(AirDistUnitNum).EquipType(AirDistCompUnitNum), "AirTerminal:DualDuct:VAV")) {
                    AirDistUnit(AirDistUnitNum).EquipType_Num(AirDistCompUnitNum) = DualDuctVAV;
                    if (AirDistUnit(AirDistUnitNum).UpStreamLeak || AirDistUnit(AirDistUnitNum).DownStreamLeak) {
                        ShowSevereError("Error found in " + CurrentModuleObject + " = " + AirDistUnit(AirDistUnitNum).Name);
                        ShowContinueError("Simple duct leakage model not available for " + cAlphaFields(3) + " = " +
                                          AirDistUnit(AirDistUnitNum).EquipType(AirDistCompUnitNum));
                        ErrorsFound = true;
                    }
                } else if (UtilityRoutines::SameString(AirDistUnit(AirDistUnitNum).EquipType(AirDistCompUnitNum),
                                                       "AirTerminal:DualDuct:VAV:OutdoorAir")) {
                    AirDistUnit(AirDistUnitNum).EquipType_Num(AirDistCompUnitNum) = DualDuctVAVOutdoorAir;
                    if (AirDistUnit(AirDistUnitNum).UpStreamLeak || AirDistUnit(AirDistUnitNum).DownStreamLeak) {
                        ShowSevereError("Error found in " + CurrentModuleObject + " = " + AirDistUnit(AirDistUnitNum).Name);
                        ShowContinueError("Simple duct leakage model not available for " + cAlphaFields(3) + " = " +
                                          AirDistUnit(AirDistUnitNum).EquipType(AirDistCompUnitNum));
                        ErrorsFound = true;
                    }
                } else if (UtilityRoutines::SameString(AirDistUnit(AirDistUnitNum).EquipType(AirDistCompUnitNum),
                                                       "AirTerminal:SingleDuct:ConstantVolume:Reheat")) {
                    AirDistUnit(AirDistUnitNum).EquipType_Num(AirDistCompUnitNum) = SingleDuctConstVolReheat;
                } else if (UtilityRoutines::SameString(AirDistUnit(AirDistUnitNum).EquipType(AirDistCompUnitNum),
                                                       "AirTerminal:SingleDuct:ConstantVolume:NoReheat")) {
                    AirDistUnit(AirDistUnitNum).EquipType_Num(AirDistCompUnitNum) = SingleDuctConstVolNoReheat;
                } else if (UtilityRoutines::SameString(AirDistUnit(AirDistUnitNum).EquipType(AirDistCompUnitNum),
                                                       "AirTerminal:SingleDuct:VAV:Reheat")) {
                    AirDistUnit(AirDistUnitNum).EquipType_Num(AirDistCompUnitNum) = SingleDuctVAVReheat;
                } else if (UtilityRoutines::SameString(AirDistUnit(AirDistUnitNum).EquipType(AirDistCompUnitNum),
                                                       "AirTerminal:SingleDuct:VAV:NoReheat")) {
                    AirDistUnit(AirDistUnitNum).EquipType_Num(AirDistCompUnitNum) = SingleDuctVAVNoReheat;
                } else if (UtilityRoutines::SameString(AirDistUnit(AirDistUnitNum).EquipType(AirDistCompUnitNum),
                                                       "AirTerminal:SingleDuct:VAV:HeatAndCool:Reheat")) {
                    AirDistUnit(AirDistUnitNum).EquipType_Num(AirDistCompUnitNum) = SingleDuctCBVAVReheat;
                } else if (UtilityRoutines::SameString(AirDistUnit(AirDistUnitNum).EquipType(AirDistCompUnitNum),
                                                       "AirTerminal:SingleDuct:VAV:HeatAndCool:NoReheat")) {
                    AirDistUnit(AirDistUnitNum).EquipType_Num(AirDistCompUnitNum) = SingleDuctCBVAVNoReheat;
                } else if (UtilityRoutines::SameString(AirDistUnit(AirDistUnitNum).EquipType(AirDistCompUnitNum),
                                                       "AirTerminal:SingleDuct:SeriesPIU:Reheat")) {
                    AirDistUnit(AirDistUnitNum).EquipType_Num(AirDistCompUnitNum) = SingleDuct_SeriesPIU_Reheat;
                    if (AirDistUnit(AirDistUnitNum).UpStreamLeak || AirDistUnit(AirDistUnitNum).DownStreamLeak) {
                        ShowSevereError("Error found in " + CurrentModuleObject + " = " + AirDistUnit(AirDistUnitNum).Name);
                        ShowContinueError("Simple duct leakage model not available for " + cAlphaFields(3) + " = " +
                                          AirDistUnit(AirDistUnitNum).EquipType(AirDistCompUnitNum));
                        ErrorsFound = true;
                    }
                } else if (UtilityRoutines::SameString(AirDistUnit(AirDistUnitNum).EquipType(AirDistCompUnitNum),
                                                       "AirTerminal:SingleDuct:ParallelPIU:Reheat")) {
                    AirDistUnit(AirDistUnitNum).EquipType_Num(AirDistCompUnitNum) = SingleDuct_ParallelPIU_Reheat;
                    if (AirDistUnit(AirDistUnitNum).UpStreamLeak || AirDistUnit(AirDistUnitNum).DownStreamLeak) {
                        ShowSevereError("Error found in " + CurrentModuleObject + " = " + AirDistUnit(AirDistUnitNum).Name);
                        ShowContinueError("Simple duct leakage model not available for " + cAlphaFields(3) + " = " +
                                          AirDistUnit(AirDistUnitNum).EquipType(AirDistCompUnitNum));
                        ErrorsFound = true;
                    }
                } else if (UtilityRoutines::SameString(AirDistUnit(AirDistUnitNum).EquipType(AirDistCompUnitNum),
                                                       "AirTerminal:SingleDuct:ConstantVolume:FourPipeInduction")) {
                    AirDistUnit(AirDistUnitNum).EquipType_Num(AirDistCompUnitNum) = SingleDuct_ConstVol_4PipeInduc;
                    if (AirDistUnit(AirDistUnitNum).UpStreamLeak || AirDistUnit(AirDistUnitNum).DownStreamLeak) {
                        ShowSevereError("Error found in " + CurrentModuleObject + " = " + AirDistUnit(AirDistUnitNum).Name);
                        ShowContinueError("Simple duct leakage model not available for " + cAlphaFields(3) + " = " +
                                          AirDistUnit(AirDistUnitNum).EquipType(AirDistCompUnitNum));
                        ErrorsFound = true;
                    }
                } else if (UtilityRoutines::SameString(AirDistUnit(AirDistUnitNum).EquipType(AirDistCompUnitNum),
                                                       "AirTerminal:SingleDuct:VAV:Reheat:VariableSpeedFan")) {
                    AirDistUnit(AirDistUnitNum).EquipType_Num(AirDistCompUnitNum) = SingleDuctVAVReheatVSFan;
                    if (AirDistUnit(AirDistUnitNum).UpStreamLeak || AirDistUnit(AirDistUnitNum).DownStreamLeak) {
                        ShowSevereError("Error found in " + CurrentModuleObject + " = " + AirDistUnit(AirDistUnitNum).Name);
                        ShowContinueError("Simple duct leakage model not available for " + cAlphaFields(3) + " = " +
                                          AirDistUnit(AirDistUnitNum).EquipType(AirDistCompUnitNum));
                        ErrorsFound = true;
                    }
                } else if (UtilityRoutines::SameString(AirDistUnit(AirDistUnitNum).EquipType(AirDistCompUnitNum),
                                                       "AirTerminal:SingleDuct:ConstantVolume:CooledBeam")) {
                    AirDistUnit(AirDistUnitNum).EquipType_Num(AirDistCompUnitNum) = SingleDuctConstVolCooledBeam;
                    if (AirDistUnit(AirDistUnitNum).UpStreamLeak || AirDistUnit(AirDistUnitNum).DownStreamLeak) {
                        ShowSevereError("Error found in " + CurrentModuleObject + " = " + AirDistUnit(AirDistUnitNum).Name);
                        ShowContinueError("Simple duct leakage model not available for " + cAlphaFields(3) + " = " +
                                          AirDistUnit(AirDistUnitNum).EquipType(AirDistCompUnitNum));
                        ErrorsFound = true;
                    }
                } else if (UtilityRoutines::SameString(AirDistUnit(AirDistUnitNum).EquipType(AirDistCompUnitNum),
                                                       "AirTerminal:SingleDuct:ConstantVolume:FourPipeBeam")) {
                    AirDistUnit(AirDistUnitNum).EquipType_Num(AirDistCompUnitNum) = SingleDuctConstVolFourPipeBeam;
                    AirDistUnit(AirDistUnitNum).airTerminalPtr =
                        FourPipeBeam::HVACFourPipeBeam::fourPipeBeamFactory(state, AirDistUnit(AirDistUnitNum).EquipName(1));
                    if (AirDistUnit(AirDistUnitNum).UpStreamLeak || AirDistUnit(AirDistUnitNum).DownStreamLeak) {
                        ShowSevereError("Error found in " + CurrentModuleObject + " = " + AirDistUnit(AirDistUnitNum).Name);
                        ShowContinueError("Simple duct leakage model not available for " + cAlphaFields(3) + " = " +
                                          AirDistUnit(AirDistUnitNum).EquipType(AirDistCompUnitNum));
                        ErrorsFound = true;
                    }
                } else if (UtilityRoutines::SameString(AirDistUnit(AirDistUnitNum).EquipType(AirDistCompUnitNum),
                                                       "AirTerminal:SingleDuct:UserDefined")) {
                    AirDistUnit(AirDistUnitNum).EquipType_Num(AirDistCompUnitNum) = SingleDuctUserDefined;
                } else if (UtilityRoutines::SameString(AirDistUnit(AirDistUnitNum).EquipType(AirDistCompUnitNum), "AirTerminal:SingleDuct:Mixer")) {
                    AirDistUnit(AirDistUnitNum).EquipType_Num(AirDistCompUnitNum) = SingleDuctATMixer;
                    if (AirDistUnit(AirDistUnitNum).UpStreamLeak || AirDistUnit(AirDistUnitNum).DownStreamLeak) {
                        ShowSevereError("Error found in " + CurrentModuleObject + " = " + AirDistUnit(AirDistUnitNum).Name);
                        ShowContinueError("Simple duct leakage model not available for " + cAlphaFields(3) + " = " +
                                          AirDistUnit(AirDistUnitNum).EquipType(AirDistCompUnitNum));
                        ErrorsFound = true;
                    }
                } else {
                    ShowSevereError("Error found in " + CurrentModuleObject + " = " + AirDistUnit(AirDistUnitNum).Name);
                    ShowContinueError("Invalid " + cAlphaFields(3) + " = " + AirDistUnit(AirDistUnitNum).EquipType(AirDistCompUnitNum));
                    ErrorsFound = true;
                }

                // Set up component set for air terminal unit
                if ((AirDistUnit(AirDistUnitNum).EquipType_Num(AirDistCompUnitNum) == DualDuctConstVolume) ||
                    (AirDistUnit(AirDistUnitNum).EquipType_Num(AirDistCompUnitNum) == DualDuctVAV)) {
                    //  For dual duct units, set up two component sets, one for heat and one for cool
                    SetUpCompSets(CurrentModuleObject,
                                  AirDistUnit(AirDistUnitNum).Name,
                                  AirDistUnit(AirDistUnitNum).EquipType(AirDistCompUnitNum) + ":HEAT",
                                  AirDistUnit(AirDistUnitNum).EquipName(AirDistCompUnitNum),
                                  "UNDEFINED",
                                  AlphArray(2));
                    SetUpCompSets(CurrentModuleObject,
                                  AirDistUnit(AirDistUnitNum).Name,
                                  AirDistUnit(AirDistUnitNum).EquipType(AirDistCompUnitNum) + ":COOL",
                                  AirDistUnit(AirDistUnitNum).EquipName(AirDistCompUnitNum),
                                  "UNDEFINED",
                                  AlphArray(2));
                    //  For dual duct units with decoupled OA and RA, set up two component sets, one for OA (Outdoor Air)
                    //  and one for RA (Recirculated Air)
                } else if (AirDistUnit(AirDistUnitNum).EquipType_Num(AirDistCompUnitNum) == DualDuctVAVOutdoorAir) {
                    SetUpCompSets(CurrentModuleObject,
                                  AirDistUnit(AirDistUnitNum).Name,
                                  AirDistUnit(AirDistUnitNum).EquipType(AirDistCompUnitNum) + ":OutdoorAir",
                                  AirDistUnit(AirDistUnitNum).EquipName(AirDistCompUnitNum),
                                  "UNDEFINED",
                                  AlphArray(2));
                    GetDualDuctOutdoorAirRecircUse(state,
                                                   AirDistUnit(AirDistUnitNum).EquipType(AirDistCompUnitNum),
                                                   AirDistUnit(AirDistUnitNum).EquipName(AirDistCompUnitNum),
                                                   DualDuctRecircIsUsed);
                    if (DualDuctRecircIsUsed) {
                        SetUpCompSets(CurrentModuleObject,
                                      AirDistUnit(AirDistUnitNum).Name,
                                      AirDistUnit(AirDistUnitNum).EquipType(AirDistCompUnitNum) + ":RecirculatedAir",
                                      AirDistUnit(AirDistUnitNum).EquipName(AirDistCompUnitNum),
                                      "UNDEFINED",
                                      AlphArray(2));
                    }
                } else {
                    SetUpCompSets(CurrentModuleObject,
                                  AirDistUnit(AirDistUnitNum).Name,
                                  AirDistUnit(AirDistUnitNum).EquipType(AirDistCompUnitNum),
                                  AirDistUnit(AirDistUnitNum).EquipName(AirDistCompUnitNum),
                                  "UNDEFINED",
                                  AlphArray(2));
                }

            } // End of Air Dist Do Loop
            for (AirDistUnitNum = 1; AirDistUnitNum <= NumAirDistUnits; ++AirDistUnitNum) {
                SetupOutputVariable(state, "Zone Air Terminal Sensible Heating Energy",
                                    OutputProcessor::Unit::J,
                                    AirDistUnit(AirDistUnitNum).HeatGain,
                                    "System",
                                    "Sum",
                                    AirDistUnit(AirDistUnitNum).Name);
                SetupOutputVariable(state, "Zone Air Terminal Sensible Cooling Energy",
                                    OutputProcessor::Unit::J,
                                    AirDistUnit(AirDistUnitNum).CoolGain,
                                    "System",
                                    "Sum",
                                    AirDistUnit(AirDistUnitNum).Name);
                SetupOutputVariable(state, "Zone Air Terminal Sensible Heating Rate",
                                    OutputProcessor::Unit::W,
                                    AirDistUnit(AirDistUnitNum).HeatRate,
                                    "System",
                                    "Average",
                                    AirDistUnit(AirDistUnitNum).Name);
                SetupOutputVariable(state, "Zone Air Terminal Sensible Cooling Rate",
                                    OutputProcessor::Unit::W,
                                    AirDistUnit(AirDistUnitNum).CoolRate,
                                    "System",
                                    "Average",
                                    AirDistUnit(AirDistUnitNum).Name);
            }
        }
        if (ErrorsFound) {
            ShowFatalError(RoutineName + "Errors found in getting " + CurrentModuleObject + " Input");
        }
    }

    void InitZoneAirLoopEquipment(EnergyPlusData &state, int const AirDistUnitNum, int const ControlledZoneNum, int const ActualZoneNum)
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR         Russ Taylor
        //       DATE WRITTEN   Nov 1997

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine is left for Module format consistency -- not needed in this module.

        // Do the Begin Simulation initializations
        if (!state.dataZoneAirLoopEquipmentManager->InitAirDistUnitsFlag) {
            return;
        }
        if (state.dataZoneAirLoopEquipmentManager->EachOnceFlag(AirDistUnitNum) && (AirDistUnit(AirDistUnitNum).TermUnitSizingNum > 0)) {

            {
                auto &thisADU(AirDistUnit(AirDistUnitNum));
                {
                    auto &thisZoneEqConfig(DataZoneEquipment::ZoneEquipConfig(ControlledZoneNum));
                    thisADU.ZoneNum = ActualZoneNum;
                    for (int inletNum = 1; inletNum <= thisZoneEqConfig.NumInletNodes; ++inletNum) {
                        if (thisZoneEqConfig.InletNode(inletNum) == thisADU.OutletNodeNum)
                            thisZoneEqConfig.InletNodeADUNum(inletNum) = AirDistUnitNum;
                    }
                }

                // Fill TermUnitSizing with specs from DesignSpecification:AirTerminal:Sizing
                {
                    auto &thisTermUnitSizingData(DataSizing::TermUnitSizing(thisADU.TermUnitSizingNum));
                    thisTermUnitSizingData.ADUName = thisADU.Name;
                    if (thisADU.AirTerminalSizingSpecIndex > 0) {
                        {
                            auto const &thisAirTermSizingSpec(DataSizing::AirTerminalSizingSpec(thisADU.AirTerminalSizingSpecIndex));
                            thisTermUnitSizingData.SpecDesCoolSATRatio = thisAirTermSizingSpec.DesCoolSATRatio;
                            thisTermUnitSizingData.SpecDesHeatSATRatio = thisAirTermSizingSpec.DesHeatSATRatio;
                            thisTermUnitSizingData.SpecDesSensCoolingFrac = thisAirTermSizingSpec.DesSensCoolingFrac;
                            thisTermUnitSizingData.SpecDesSensHeatingFrac = thisAirTermSizingSpec.DesSensHeatingFrac;
                            thisTermUnitSizingData.SpecMinOAFrac = thisAirTermSizingSpec.MinOAFrac;
                        }
                    }
                }
            }

            if (AirDistUnit(AirDistUnitNum).ZoneNum != 0 && DataHeatBalance::Zone(AirDistUnit(AirDistUnitNum).ZoneNum).HasAdjustedReturnTempByITE) {
                for (int AirDistCompNum = 1; AirDistCompNum <= AirDistUnit(AirDistUnitNum).NumComponents; ++AirDistCompNum) {
                    if (AirDistUnit(AirDistUnitNum).EquipType_Num(AirDistCompNum) != SingleDuctVAVReheat &&
                        AirDistUnit(AirDistUnitNum).EquipType_Num(AirDistCompNum) != SingleDuctVAVNoReheat) {
                        ShowSevereError("The FlowControlWithApproachTemperatures only works with ITE zones with single duct VAV terminal unit.");
                        ShowContinueError("The return air temperature of the ITE will not be overwritten.");
                        ShowFatalError("Preceding condition causes termination.");
                    }
                }
            }
            state.dataZoneAirLoopEquipmentManager->EachOnceFlag(AirDistUnitNum) = false;
            ++state.dataZoneAirLoopEquipmentManager->numADUInitialized;
            if (state.dataZoneAirLoopEquipmentManager->numADUInitialized == NumAirDistUnits) {
                // If all ADUs are initialized, set InitAirDistUnitsFlag to false
                state.dataZoneAirLoopEquipmentManager->InitAirDistUnitsFlag = false;
            }
        }
    }

    void InitZoneAirLoopEquipmentTimeStep(int const AirDistUnitNum)
    {
        // every time step
        AirDistUnit(AirDistUnitNum).MassFlowRateDnStrLk = 0.0;
        AirDistUnit(AirDistUnitNum).MassFlowRateUpStrLk = 0.0;
        AirDistUnit(AirDistUnitNum).MassFlowRateTU = 0.0;
        AirDistUnit(AirDistUnitNum).MassFlowRateZSup = 0.0;
        AirDistUnit(AirDistUnitNum).MassFlowRateSup = 0.0;
        AirDistUnit(AirDistUnitNum).HeatRate = 0.0;
        AirDistUnit(AirDistUnitNum).CoolRate = 0.0;
        AirDistUnit(AirDistUnitNum).HeatGain = 0.0;
        AirDistUnit(AirDistUnitNum).CoolGain = 0.0;
    }

    void SimZoneAirLoopEquipment(EnergyPlusData &state, int const AirDistUnitNum,
                                 Real64 &SysOutputProvided,
                                 Real64 &NonAirSysOutput,
                                 Real64 &LatOutputProvided, // Latent add/removal provided by this unit (kg/s), dehumidify = negative
                                 bool const FirstHVACIteration,
                                 int const ControlledZoneNum,
                                 int const ActualZoneNum)
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR         Russ Taylor
        //       DATE WRITTEN   May 1997
        //       MODIFIED       Don Shirey, Aug 2009 (LatOutputProvided)

        // PURPOSE OF THIS SUBROUTINE:
        // Simulates primary system air supplied to a zone and calculates
        // airflow requirements

        using DataLoopNode::Node;
        using DataZoneEquipment::ZoneEquipConfig;
        using DualDuct::SimulateDualDuct;
        using HVACCooledBeam::SimCoolBeam;
        using HVACSingleDuctInduc::SimIndUnit;
        using PoweredInductionUnits::SimPIU;
        using Psychrometrics::PsyCpAirFnW;
        using SingleDuct::GetATMixers;
        using SingleDuct::SimulateSingleDuct;
        using UserDefinedComponents::SimAirTerminalUserDefined;

        bool ProvideSysOutput;

        int AirDistCompNum;
        int InNodeNum;                      // air distribution unit inlet node
        int OutNodeNum;                     // air distribution unit outlet node
        static int AirLoopNum(0);           // index of air loop
        Real64 MassFlowRateMaxAvail;        // max avail mass flow rate excluding leaks [kg/s]
        Real64 MassFlowRateMinAvail;        // min avail mass flow rate excluding leaks [kg/s]
        Real64 MassFlowRateUpStreamLeakMax; // max upstream leak flow rate [kg/s]
        static Real64 DesFlowRatio(0.0);    // ratio of system to sum of zones design flow rate
        static Real64 SpecHumOut(0.0);      // Specific humidity ratio of outlet air (kg moisture / kg moist air)
        static Real64 SpecHumIn(0.0);       // Specific humidity ratio of inlet air (kg moisture / kg moist air)

        ProvideSysOutput = true;
        for (AirDistCompNum = 1; AirDistCompNum <= AirDistUnit(AirDistUnitNum).NumComponents; ++AirDistCompNum) {
            NonAirSysOutput = 0.0;
            InNodeNum = AirDistUnit(AirDistUnitNum).InletNodeNum;
            OutNodeNum = AirDistUnit(AirDistUnitNum).OutletNodeNum;
            MassFlowRateMaxAvail = 0.0;
            MassFlowRateMinAvail = 0.0;
            // check for no plenum
            // set the max and min avail flow rates taking into acount the upstream leak
            if (AirDistUnit(AirDistUnitNum).UpStreamLeak) {
                if (InNodeNum > 0) {
                    MassFlowRateMaxAvail = Node(InNodeNum).MassFlowRateMaxAvail;
                    MassFlowRateMinAvail = Node(InNodeNum).MassFlowRateMinAvail;
                    AirLoopNum = AirDistUnit(AirDistUnitNum).AirLoopNum;
                    if (AirLoopNum > 0) {
                        DesFlowRatio = state.dataAirLoop->AirLoopFlow(AirLoopNum).SysToZoneDesFlowRatio;
                    } else {
                        DesFlowRatio = 1.0;
                    }
                    MassFlowRateUpStreamLeakMax =
                        max(AirDistUnit(AirDistUnitNum).UpStreamLeakFrac * Node(InNodeNum).MassFlowRateMax * DesFlowRatio, 0.0);
                    if (MassFlowRateMaxAvail > MassFlowRateUpStreamLeakMax) {
                        AirDistUnit(AirDistUnitNum).MassFlowRateUpStrLk = MassFlowRateUpStreamLeakMax;
                        Node(InNodeNum).MassFlowRateMaxAvail = MassFlowRateMaxAvail - MassFlowRateUpStreamLeakMax;
                    } else {
                        AirDistUnit(AirDistUnitNum).MassFlowRateUpStrLk = MassFlowRateMaxAvail;
                        Node(InNodeNum).MassFlowRateMaxAvail = 0.0;
                    }
                    Node(InNodeNum).MassFlowRateMinAvail = max(0.0, MassFlowRateMinAvail - AirDistUnit(AirDistUnitNum).MassFlowRateUpStrLk);
                }
            }

            {
                auto const SELECT_CASE_var(AirDistUnit(AirDistUnitNum).EquipType_Num(AirDistCompNum));

                if (SELECT_CASE_var == DualDuctConstVolume) {
                    SimulateDualDuct(state, AirDistUnit(AirDistUnitNum).EquipName(AirDistCompNum),
                                     FirstHVACIteration,
                                     ActualZoneNum,
                                     ZoneEquipConfig(ControlledZoneNum).ZoneNode,
                                     AirDistUnit(AirDistUnitNum).EquipIndex(AirDistCompNum));

                } else if (SELECT_CASE_var == DualDuctVAV) {
                    SimulateDualDuct(state, AirDistUnit(AirDistUnitNum).EquipName(AirDistCompNum),
                                     FirstHVACIteration,
                                     ActualZoneNum,
                                     ZoneEquipConfig(ControlledZoneNum).ZoneNode,
                                     AirDistUnit(AirDistUnitNum).EquipIndex(AirDistCompNum));

                } else if (SELECT_CASE_var == DualDuctVAVOutdoorAir) {
                    SimulateDualDuct(state, AirDistUnit(AirDistUnitNum).EquipName(AirDistCompNum),
                                     FirstHVACIteration,
                                     ActualZoneNum,
                                     ZoneEquipConfig(ControlledZoneNum).ZoneNode,
                                     AirDistUnit(AirDistUnitNum).EquipIndex(AirDistCompNum));

                } else if (SELECT_CASE_var == SingleDuctVAVReheat) {
                    SimulateSingleDuct(state, AirDistUnit(AirDistUnitNum).EquipName(AirDistCompNum),
                                       FirstHVACIteration,
                                       ActualZoneNum,
                                       ZoneEquipConfig(ControlledZoneNum).ZoneNode,
                                       AirDistUnit(AirDistUnitNum).EquipIndex(AirDistCompNum));

                } else if (SELECT_CASE_var == SingleDuctCBVAVReheat) {
                    SimulateSingleDuct(state, AirDistUnit(AirDistUnitNum).EquipName(AirDistCompNum),
                                       FirstHVACIteration,
                                       ActualZoneNum,
                                       ZoneEquipConfig(ControlledZoneNum).ZoneNode,
                                       AirDistUnit(AirDistUnitNum).EquipIndex(AirDistCompNum));

                } else if (SELECT_CASE_var == SingleDuctVAVNoReheat) {
                    SimulateSingleDuct(state, AirDistUnit(AirDistUnitNum).EquipName(AirDistCompNum),
                                       FirstHVACIteration,
                                       ActualZoneNum,
                                       ZoneEquipConfig(ControlledZoneNum).ZoneNode,
                                       AirDistUnit(AirDistUnitNum).EquipIndex(AirDistCompNum));

                } else if (SELECT_CASE_var == SingleDuctCBVAVNoReheat) {
                    SimulateSingleDuct(state, AirDistUnit(AirDistUnitNum).EquipName(AirDistCompNum),
                                       FirstHVACIteration,
                                       ActualZoneNum,
                                       ZoneEquipConfig(ControlledZoneNum).ZoneNode,
                                       AirDistUnit(AirDistUnitNum).EquipIndex(AirDistCompNum));

                } else if (SELECT_CASE_var == SingleDuctConstVolReheat) {
                    SimulateSingleDuct(state, AirDistUnit(AirDistUnitNum).EquipName(AirDistCompNum),
                                       FirstHVACIteration,
                                       ActualZoneNum,
                                       ZoneEquipConfig(ControlledZoneNum).ZoneNode,
                                       AirDistUnit(AirDistUnitNum).EquipIndex(AirDistCompNum));

                } else if (SELECT_CASE_var == SingleDuctConstVolNoReheat) {
                    SimulateSingleDuct(state, AirDistUnit(AirDistUnitNum).EquipName(AirDistCompNum),
                                       FirstHVACIteration,
                                       ActualZoneNum,
                                       ZoneEquipConfig(ControlledZoneNum).ZoneNode,
                                       AirDistUnit(AirDistUnitNum).EquipIndex(AirDistCompNum));

                } else if (SELECT_CASE_var == SingleDuct_SeriesPIU_Reheat) {
                    SimPIU(state, AirDistUnit(AirDistUnitNum).EquipName(AirDistCompNum),
                           FirstHVACIteration,
                           ActualZoneNum,
                           ZoneEquipConfig(ControlledZoneNum).ZoneNode,
                           AirDistUnit(AirDistUnitNum).EquipIndex(AirDistCompNum));

                } else if (SELECT_CASE_var == SingleDuct_ParallelPIU_Reheat) {
                    SimPIU(state, AirDistUnit(AirDistUnitNum).EquipName(AirDistCompNum),
                           FirstHVACIteration,
                           ActualZoneNum,
                           ZoneEquipConfig(ControlledZoneNum).ZoneNode,
                           AirDistUnit(AirDistUnitNum).EquipIndex(AirDistCompNum));

                } else if (SELECT_CASE_var == SingleDuct_ConstVol_4PipeInduc) {
                    SimIndUnit(state, AirDistUnit(AirDistUnitNum).EquipName(AirDistCompNum),
                               FirstHVACIteration,
                               ActualZoneNum,
                               ZoneEquipConfig(ControlledZoneNum).ZoneNode,
                               AirDistUnit(AirDistUnitNum).EquipIndex(AirDistCompNum));

                } else if (SELECT_CASE_var == SingleDuctVAVReheatVSFan) {
                    SimulateSingleDuct(state, AirDistUnit(AirDistUnitNum).EquipName(AirDistCompNum),
                                       FirstHVACIteration,
                                       ActualZoneNum,
                                       ZoneEquipConfig(ControlledZoneNum).ZoneNode,
                                       AirDistUnit(AirDistUnitNum).EquipIndex(AirDistCompNum));

                } else if (SELECT_CASE_var == SingleDuctConstVolCooledBeam) {
                    SimCoolBeam(state,
                                AirDistUnit(AirDistUnitNum).EquipName(AirDistCompNum),
                                FirstHVACIteration,
                                ActualZoneNum,
                                ZoneEquipConfig(ControlledZoneNum).ZoneNode,
                                AirDistUnit(AirDistUnitNum).EquipIndex(AirDistCompNum),
                                NonAirSysOutput);

                } else if (SELECT_CASE_var == SingleDuctConstVolFourPipeBeam) {
                    AirDistUnit(AirDistUnitNum).airTerminalPtr->simulate(state, FirstHVACIteration, NonAirSysOutput);

                } else if (SELECT_CASE_var == SingleDuctUserDefined) {
                    SimAirTerminalUserDefined(state,
                                              AirDistUnit(AirDistUnitNum).EquipName(AirDistCompNum),
                                              FirstHVACIteration,
                                              ActualZoneNum,
                                              ZoneEquipConfig(ControlledZoneNum).ZoneNode,
                                              AirDistUnit(AirDistUnitNum).EquipIndex(AirDistCompNum));

                } else if (SELECT_CASE_var == SingleDuctATMixer) {
                    GetATMixers(state); // Needed here if mixer used only with unitarysystem which gets its input late
                    ProvideSysOutput = false;

                } else {
                    ShowSevereError("Error found in ZoneHVAC:AirDistributionUnit=" + AirDistUnit(AirDistUnitNum).Name);
                    ShowContinueError("Invalid Component=" + AirDistUnit(AirDistUnitNum).EquipType(AirDistCompNum));
                    ShowFatalError("Preceding condition causes termination.");
                }
            }

            // do leak mass flow calcs
            if (InNodeNum > 0) { // InNodeNum is not always known when this is called, eg FPIU
                InNodeNum = AirDistUnit(AirDistUnitNum).InletNodeNum;
                if (AirDistUnit(AirDistUnitNum).UpStreamLeak) {
                    Node(InNodeNum).MassFlowRateMaxAvail = MassFlowRateMaxAvail;
                    Node(InNodeNum).MassFlowRateMinAvail = MassFlowRateMinAvail;
                }
                if ((AirDistUnit(AirDistUnitNum).UpStreamLeak || AirDistUnit(AirDistUnitNum).DownStreamLeak) && MassFlowRateMaxAvail > 0.0) {
                    AirDistUnit(AirDistUnitNum).MassFlowRateTU = Node(InNodeNum).MassFlowRate;
                    AirDistUnit(AirDistUnitNum).MassFlowRateZSup =
                        AirDistUnit(AirDistUnitNum).MassFlowRateTU * (1.0 - AirDistUnit(AirDistUnitNum).DownStreamLeakFrac);
                    AirDistUnit(AirDistUnitNum).MassFlowRateDnStrLk =
                        AirDistUnit(AirDistUnitNum).MassFlowRateTU * AirDistUnit(AirDistUnitNum).DownStreamLeakFrac;
                    AirDistUnit(AirDistUnitNum).MassFlowRateSup =
                        AirDistUnit(AirDistUnitNum).MassFlowRateTU + AirDistUnit(AirDistUnitNum).MassFlowRateUpStrLk;
                    Node(InNodeNum).MassFlowRate = AirDistUnit(AirDistUnitNum).MassFlowRateSup;
                    Node(OutNodeNum).MassFlowRate = AirDistUnit(AirDistUnitNum).MassFlowRateZSup;
                    Node(OutNodeNum).MassFlowRateMaxAvail =
                        max(0.0,
                            MassFlowRateMaxAvail - AirDistUnit(AirDistUnitNum).MassFlowRateDnStrLk - AirDistUnit(AirDistUnitNum).MassFlowRateUpStrLk);
                    Node(OutNodeNum).MassFlowRateMinAvail =
                        max(0.0,
                            MassFlowRateMinAvail - AirDistUnit(AirDistUnitNum).MassFlowRateDnStrLk - AirDistUnit(AirDistUnitNum).MassFlowRateUpStrLk);
                    AirDistUnit(AirDistUnitNum).MaxAvailDelta = MassFlowRateMaxAvail - Node(OutNodeNum).MassFlowRateMaxAvail;
                    AirDistUnit(AirDistUnitNum).MinAvailDelta = MassFlowRateMinAvail - Node(OutNodeNum).MassFlowRateMinAvail;
                } else {
                    // if no leaks, or a terminal unit type not supported for leaks
                    int termUnitType = AirDistUnit(AirDistUnitNum).EquipType_Num(AirDistCompNum);
                    if ((termUnitType == DualDuctConstVolume) || (termUnitType == DualDuctVAV) || (termUnitType == DualDuctVAVOutdoorAir)) {
                        // Use ADU outlet node flow for dual duct terminal units (which don't support leaks)
                        AirDistUnit(AirDistUnitNum).MassFlowRateTU = Node(OutNodeNum).MassFlowRate;
                        AirDistUnit(AirDistUnitNum).MassFlowRateZSup = Node(OutNodeNum).MassFlowRate;
                        AirDistUnit(AirDistUnitNum).MassFlowRateSup = Node(OutNodeNum).MassFlowRate;
                    } else {
                        AirDistUnit(AirDistUnitNum).MassFlowRateTU = Node(InNodeNum).MassFlowRate;
                        AirDistUnit(AirDistUnitNum).MassFlowRateZSup = Node(InNodeNum).MassFlowRate;
                        AirDistUnit(AirDistUnitNum).MassFlowRateSup = Node(InNodeNum).MassFlowRate;
                    }
                }
            }
        }
        if (ProvideSysOutput) {
            int OutletNodeNum = AirDistUnit(AirDistUnitNum).OutletNodeNum;
            int ZoneAirNode = ZoneEquipConfig(ControlledZoneNum).ZoneNode;
            SpecHumOut = Node(OutletNodeNum).HumRat;
            SpecHumIn = Node(ZoneAirNode).HumRat;
            // Sign convention: SysOutputProvided <0 Zone is cooled
            //                  SysOutputProvided >0 Zone is heated
            SysOutputProvided =
                Node(OutletNodeNum).MassFlowRate *
                Psychrometrics::PsyDeltaHSenFnTdb2W2Tdb1W1(Node(OutletNodeNum).Temp, SpecHumOut, Node(ZoneAirNode).Temp, SpecHumIn); // sensible {W};
            // Sign convention: LatOutputProvided <0 Zone is dehumidified
            //                  LatOutputProvided >0 Zone is humidified
            // CR9155 Remove specific humidity calculations
            LatOutputProvided = Node(OutletNodeNum).MassFlowRate * (SpecHumOut - SpecHumIn); // Latent rate (kg/s), dehumid = negative
        } else {
            SysOutputProvided = 0.0;
            LatOutputProvided = 0.0;
        }
    }

} // namespace ZoneAirLoopEquipmentManager

} // namespace EnergyPlus
