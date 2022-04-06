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

// FMI-Related Headers
extern "C" {
#include <BCVTB/utilSocket.h>
#include <BCVTB/utilXml.h>
#include <FMI/main.h>
}

// C++ Headers
#include <string>
#include <vector>

// ObjexxFCL Headers
#include <ObjexxFCL/Array.functions.hh>
#include <ObjexxFCL/string.functions.hh>

// EnergyPlus Headers
#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataIPShortCuts.hh>
#include <EnergyPlus/DataStringGlobals.hh>
#include <EnergyPlus/DataSystemVariables.hh>
#include <EnergyPlus/DisplayRoutines.hh>
#include <EnergyPlus/EMSManager.hh>
#include <EnergyPlus/ExternalInterface.hh>
#include <EnergyPlus/FileSystem.hh>
#include <EnergyPlus/GlobalNames.hh>
#include <EnergyPlus/InputProcessing/InputProcessor.hh>
#include <EnergyPlus/OutputProcessor.hh>
#include <EnergyPlus/RuntimeLanguageProcessor.hh>
#include <EnergyPlus/ScheduleManager.hh>
#include <EnergyPlus/UtilityRoutines.hh>

namespace EnergyPlus::ExternalInterface {

// Module containing the routines dealing with the BCVTB interface

// MODULE INFORMATION:
//       AUTHOR         Michael Wetter
//       DATE WRITTEN   2Dec2007
//       MODIFIED       Rui Zhang July 2009
//       MODIFIED       Thierry S. Nouidui 2011
//       RE-ENGINEERED  na

// PURPOSE OF THIS MODULE:
// To encapsulate the data and routines required to interface
// the Building Controls Virtual Test Bed (BCVTB) and FunctionalMockupUnits (FMU)

// REFERENCES:
// http://simulationresearch.lbl.gov/bcvtb
// http://www.modelisar.com

void ExternalInterfaceExchangeVariables(EnergyPlusData &state)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Michael Wetter
    //       DATE WRITTEN   2Dec2007
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // Exchanges variables between EnergyPlus and the BCVTB socket.

    // Using/Aliasing
    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    std::string errorMessage; // Error message
    int retValErrMsg;

    if (state.dataExternalInterface->GetInputFlag) {
        GetExternalInterfaceInput(state);
        state.dataExternalInterface->GetInputFlag = false;
    }

    if (state.dataExternalInterface->haveExternalInterfaceBCVTB || state.dataExternalInterface->haveExternalInterfaceFMUExport) {
        InitExternalInterface(state);
        // Exchange data only after sizing and after warm-up.
        // Note that checking for ZoneSizingCalc SysSizingCalc does not work here, hence we
        // use the KindOfSim flag
        if (!state.dataGlobal->WarmupFlag && (state.dataGlobal->KindOfSim == DataGlobalConstants::KindOfSim::RunPeriodWeather)) {
            CalcExternalInterface(state);
        }
    }

    if (state.dataExternalInterface->haveExternalInterfaceFMUImport) {
        char *errorMessagePtr(&errorMessage[0]);
        retValErrMsg = checkOperatingSystem(errorMessagePtr);
        if (retValErrMsg != 0) {
            ShowSevereError(state, "ExternalInterface/ExternalInterfaceExchangeVariables:" + std::string(errorMessagePtr));
            state.dataExternalInterface->ErrorsFound = true;
            StopExternalInterfaceIfError(state);
        }
        // initialize the FunctionalMockupUnitImport interface
        InitExternalInterfaceFMUImport(state);
        // No Data exchange during design days
        // Data Exchange data during warmup and after warmup
        CalcExternalInterfaceFMUImport(state);
    }
}

void GetExternalInterfaceInput(EnergyPlusData &state)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Michael Wetter
    //       DATE WRITTEN   2Dec2007
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // Obtains input data for ExternalInterface

    // METHODOLOGY EMPLOYED:
    // Uses InputProcessor "Get" routines to obtain data.

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int NumAlphas;  // Number of Alphas for each GetObjectItem call
    int NumNumbers; // Number of Numbers for each GetObjectItem call
    int IOStatus;   // Used in GetObjectItem
    int Loop;       // Loop counter
    auto &cCurrentModuleObject = state.dataIPShortCut->cCurrentModuleObject;
    cCurrentModuleObject = "ExternalInterface";
    state.dataExternalInterface->NumExternalInterfaces = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, cCurrentModuleObject);

    for (Loop = 1; Loop <= state.dataExternalInterface->NumExternalInterfaces;
         ++Loop) { // This loop determines whether the external interface is for FMU or BCVTB
        state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                 cCurrentModuleObject,
                                                                 Loop,
                                                                 state.dataIPShortCut->cAlphaArgs,
                                                                 NumAlphas,
                                                                 state.dataIPShortCut->rNumericArgs,
                                                                 NumNumbers,
                                                                 IOStatus,
                                                                 _,
                                                                 _,
                                                                 state.dataIPShortCut->cAlphaFieldNames,
                                                                 state.dataIPShortCut->cNumericFieldNames);
        if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(1), "PtolemyServer")) { // The BCVTB interface is activated.
            ++state.dataExternalInterface->NumExternalInterfacesBCVTB;
        } else if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(1),
                                               "FunctionalMockupUnitImport")) { // The functional mock up unit import interface is activated.
            ++state.dataExternalInterface->NumExternalInterfacesFMUImport;
        } else if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(1),
                                               "FunctionalMockupUnitExport")) { // The functional mock up unit export interface is activated.
            ++state.dataExternalInterface->NumExternalInterfacesFMUExport;
        }
    }

    // Check if objects are used although BCVTB interface object is not defined
    if (state.dataExternalInterface->NumExternalInterfacesBCVTB == 0) {
        WarnIfExternalInterfaceObjectsAreUsed(state, "ExternalInterface:Schedule");
        WarnIfExternalInterfaceObjectsAreUsed(state, "ExternalInterface:Variable");
        WarnIfExternalInterfaceObjectsAreUsed(state, "ExternalInterface:Actuator");
    }

    // Check if objects are used although FMUExport interface is not defined
    if (state.dataExternalInterface->NumExternalInterfacesFMUExport == 0) {
        WarnIfExternalInterfaceObjectsAreUsed(state, "ExternalInterface:FunctionalMockupUnitExport:To:Schedule");
        WarnIfExternalInterfaceObjectsAreUsed(state, "ExternalInterface:FunctionalMockupUnitExport:To:Variable");
        WarnIfExternalInterfaceObjectsAreUsed(state, "ExternalInterface:FunctionalMockupUnitExport:To:Actuator");
    }

    // Check if objects are used although FMU Import interface is not defined
    if (state.dataExternalInterface->NumExternalInterfacesFMUImport == 0) {
        WarnIfExternalInterfaceObjectsAreUsed(state, "ExternalInterface:FunctionalMockupUnitImport:To:Schedule");
        WarnIfExternalInterfaceObjectsAreUsed(state, "ExternalInterface:FunctionalMockupUnitImport:To:Variable");
        WarnIfExternalInterfaceObjectsAreUsed(state, "ExternalInterface:FunctionalMockupUnitImport:To:Actuator");
    }

    if ((state.dataExternalInterface->NumExternalInterfacesBCVTB == 1) && (state.dataExternalInterface->NumExternalInterfacesFMUExport == 0)) {
        state.dataExternalInterface->haveExternalInterfaceBCVTB = true;
        DisplayString(state, "Instantiating Building Controls Virtual Test Bed");
        state.dataExternalInterface->varKeys.allocate(maxVar);         // Keys of report variables used for data exchange
        state.dataExternalInterface->varNames.allocate(maxVar);        // Names of report variables used for data exchange
        state.dataExternalInterface->inpVarTypes.dimension(maxVar, 0); // Names of report variables used for data exchange
        state.dataExternalInterface->inpVarNames.allocate(maxVar);     // Names of report variables used for data exchange
        VerifyExternalInterfaceObject(state);
    } else if ((state.dataExternalInterface->NumExternalInterfacesBCVTB == 0) && (state.dataExternalInterface->NumExternalInterfacesFMUExport == 1)) {
        state.dataExternalInterface->haveExternalInterfaceFMUExport = true;
        state.dataExternalInterface->FMUExportActivate = 1;
        DisplayString(state, "Instantiating FunctionalMockupUnitExport interface");
        state.dataExternalInterface->varKeys.allocate(maxVar);         // Keys of report variables used for data exchange
        state.dataExternalInterface->varNames.allocate(maxVar);        // Names of report variables used for data exchange
        state.dataExternalInterface->inpVarTypes.dimension(maxVar, 0); // Names of report variables used for data exchange
        state.dataExternalInterface->inpVarNames.allocate(maxVar);     // Names of report variables used for data exchange
        VerifyExternalInterfaceObject(state);
    } else if ((state.dataExternalInterface->NumExternalInterfacesBCVTB == 1) && (state.dataExternalInterface->NumExternalInterfacesFMUExport != 0)) {
        ShowSevereError(state, "GetExternalInterfaceInput: Cannot have Ptolemy and FMU-Export interface simultaneously.");
        state.dataExternalInterface->ErrorsFound = true;
    }

    if ((state.dataExternalInterface->NumExternalInterfacesFMUImport == 1) && (state.dataExternalInterface->NumExternalInterfacesFMUExport == 0)) {
        state.dataExternalInterface->haveExternalInterfaceFMUImport = true;
        DisplayString(state, "Instantiating FunctionalMockupUnitImport interface");
        cCurrentModuleObject = "ExternalInterface:FunctionalMockupUnitImport";
        state.dataExternalInterface->NumFMUObjects = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, cCurrentModuleObject);
        VerifyExternalInterfaceObject(state);
    } else if ((state.dataExternalInterface->NumExternalInterfacesFMUImport == 1) &&
               (state.dataExternalInterface->NumExternalInterfacesFMUExport != 0)) {
        ShowSevereError(state, "GetExternalInterfaceInput: Cannot have FMU-Import and FMU-Export interface simultaneously.");
        state.dataExternalInterface->ErrorsFound = true;
    }

    if (state.dataExternalInterface->NumExternalInterfacesBCVTB > 1) {
        ShowSevereError(state, "GetExternalInterfaceInput: Cannot have more than one Ptolemy interface.");
        ShowContinueError(state, "GetExternalInterfaceInput: Errors found in input.");
        state.dataExternalInterface->ErrorsFound = true;
    }

    if (state.dataExternalInterface->NumExternalInterfacesFMUExport > 1) {
        ShowSevereError(state, "GetExternalInterfaceInput: Cannot have more than one FMU-Export interface.");
        ShowContinueError(state, "Errors found in input.");
        state.dataExternalInterface->ErrorsFound = true;
    }

    if (state.dataExternalInterface->NumExternalInterfacesFMUImport > 1) {
        ShowSevereError(state, "GetExternalInterfaceInput: Cannot have more than one FMU-Import interface.");
        ShowContinueError(state, "Errors found in input.");
        state.dataExternalInterface->ErrorsFound = true;
    }

    if (state.dataExternalInterface->ErrorsFound) {
        ShowFatalError(state, "GetExternalInterfaceInput: preceding conditions cause termination.");
    }

    StopExternalInterfaceIfError(state);
}

void StopExternalInterfaceIfError(EnergyPlusData &state)
{
    // SUBROUTINE INFORMATION:
    //       AUTHOR         Michael Wetter
    //       DATE WRITTEN   9Jan2008
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine gracefully stops the ExternalInterface if an error has been found.
    // It sends an appropriate message to the ExternalInterface
    // and then calls a fatal error to stop EnergyPlus.

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int retVal; // Return value, needed to catch return value of function call
    int constexpr flag1(-10);
    int constexpr flag2(-20);

    if ((state.dataExternalInterface->NumExternalInterfacesBCVTB != 0) || (state.dataExternalInterface->NumExternalInterfacesFMUExport != 0)) {
        if (state.dataExternalInterface->ErrorsFound) {
            // Check if the socket is open
            if (state.dataExternalInterface->socketFD >= 0) {
                // Socket is open
                if (state.dataExternalInterface->simulationStatus == 1) {
                    retVal = sendclientmessage(&state.dataExternalInterface->socketFD, &flag1);
                } else {
                    retVal = sendclientmessage(&state.dataExternalInterface->socketFD, &flag2);
                }
            }
            ShowFatalError(state, "Error in ExternalInterface: Check EnergyPlus *.err file.");
        }
    }
    if (state.dataExternalInterface->NumExternalInterfacesFMUImport != 0) {
        if (state.dataExternalInterface->ErrorsFound) {
            ShowFatalError(state, "ExternalInterface/StopExternalInterfaceIfError: Error in ExternalInterface: Check EnergyPlus *.err file.");
        }
    }
}

void CloseSocket(EnergyPlusData &state, int const FlagToWriteToSocket)
{
    // SUBROUTINE INFORMATION:
    //       AUTHOR         Michael Wetter
    //       DATE WRITTEN   December 2008
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine tries to write the optional error code to the
    // socket and then closes the socket

    // SUBROUTINE ARGUMENT DEFINITIONS:
    // +1: E+ reached final time
    // -1: E+ had some error

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int retVal;     // Return value, needed to catch return value of function call
    bool fileExist; // Set to true if file exists

    // Try to establish socket connection. This is needed if Ptolemy started E+,
    //  but E+ had an error before the call to InitExternalInterface.

    fileExist = FileSystem::fileExists(state.dataExternalInterface->socCfgFilPath);

    if ((state.dataExternalInterface->socketFD == -1) && fileExist) {
        state.dataExternalInterface->socketFD = establishclientsocket(state.dataExternalInterface->socCfgFilPath.string().c_str());
    }

    if (state.dataExternalInterface->socketFD >= 0) {
        // TODO: use retVal?
        retVal = sendclientmessage(&state.dataExternalInterface->socketFD, &FlagToWriteToSocket);
        // Don't close socket as this may give sometimes an IOException in Windows
        // This problem seems to affect only Windows but not Mac
        //     close(state.dataExternalInterface->socketFD)
    }
}

void ParseString(std::string const &str, // The string, with all elements separated by ';'
                 Array1D_string &ele,    // The elements
                 int const nEle          // The number of elements
)
{
    // SUBROUTINE INFORMATION:
    //       AUTHOR         Michael Wetter
    //       DATE WRITTEN   8Jan2008
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine parses the semicolon separated string xmlStr
    // and assigns each element to ele

    // SUBROUTINE VARIABLE DEFINITIONS:
    int i;                         // Counter
    std::string::size_type iSta;   // Start of substring
    std::string::size_type iEnd;   // End of substring
    std::string::size_type iCol;   // Index of ;
    std::string::size_type lenStr; // Length of string

    lenStr = len(str);
    iEnd = 0;
    for (i = 1; i <= nEle; ++i) {
        iSta = iEnd; // add one to skip ';'
        iCol = str.find(';', iSta);
        if (iCol != std::string::npos) {
            iEnd = iCol + 1;
        } else { // Use rest of string
            iEnd = lenStr;
        }
        ele(i) = UtilityRoutines::MakeUPPERCase(str.substr(iSta, iEnd - iSta - 1));
    }
}

void InitExternalInterface(EnergyPlusData &state)
{
    // SUBROUTINE INFORMATION:
    //       AUTHOR         Michael Wetter
    //       DATE WRITTEN   2Dec2007
    //       MODIFIED       Rui Zhang Aug 2009
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine is for initializations of the ExternalInterface

    // Using/Aliasing

    using RuntimeLanguageProcessor::FindEMSVariable;
    using RuntimeLanguageProcessor::isExternalInterfaceErlVariable;
    using ScheduleManager::GetDayScheduleIndex;

    // SUBROUTINE PARAMETER DEFINITIONS:

    std::string const simCfgFilNam("variables.cfg");               // Configuration file
    std::string const xmlStrInKey("schedule,variable,actuator\0"); // xml values in string, separated by ','

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int i;                    // loop counters
    std::string xmlStrOut;    // xml values in string, separated by ';'
    std::string xmlStrOutTyp; // xml values in string, separated by ';'
    std::string xmlStrIn;     // xml values in string, separated by ';'
    int retVal;               // Return value of function call, used for error handling
    int mainVersion;          // The version number

    if (state.dataExternalInterface->InitExternalInterfacefirstCall) {
        DisplayString(state, "ExternalInterface initializes.");
        // do one time initializations

        if (state.dataExternalInterface->haveExternalInterfaceBCVTB) {
            // Check version number
            mainVersion = getmainversionnumber();
            if (mainVersion < 0) {
                ShowSevereError(state, "ExternalInterface: BCVTB is not installed in this version.");
                state.dataExternalInterface->ErrorsFound = true;
                StopExternalInterfaceIfError(state);
            }
        }

        // Get port number
        if (FileSystem::fileExists(state.dataExternalInterface->socCfgFilPath)) {
            state.dataExternalInterface->socketFD = establishclientsocket(state.dataExternalInterface->socCfgFilPath.string().c_str());
            if (state.dataExternalInterface->socketFD < 0) {
                ShowSevereError(state,
                                format("ExternalInterface: Could not open socket. File descriptor = {}.", state.dataExternalInterface->socketFD));
                state.dataExternalInterface->ErrorsFound = true;
            }
        } else {
            ShowSevereError(state, "ExternalInterface: Did not find file \"" + state.dataExternalInterface->socCfgFilPath.string() + "\".");
            ShowContinueError(state, "This file needs to be in same directory as in.idf.");
            ShowContinueError(state, "Check the documentation for the ExternalInterface.");
            state.dataExternalInterface->ErrorsFound = true;
        }

        // Make sure that idf file specified a run period other than
        // design day and system sizing.
        ValidateRunControl(state);

        StopExternalInterfaceIfError(state);

        // make a single length here for all strings to be passed to getepvariables
        size_t lenXmlStr(maxVar * DataGlobalConstants::MaxNameLength); // Length of strings being passed to getepvariables

        // initialize all the strings to this length with blanks
        xmlStrOut = std::string(lenXmlStr, ' ');
        xmlStrOutTyp = std::string(lenXmlStr, ' ');
        xmlStrIn = std::string(lenXmlStr, ' ');

        // Get input and output variables for EnergyPlus in sequence
        // Check if simCfgFilNam exists.
        if (FileSystem::fileExists(simCfgFilNam)) {

            // preprocess the strings into char vectors before making the library call
            auto xmlStrOutTypArr(getCharArrayFromString(xmlStrOutTyp));
            auto xmlStrOutArr(getCharArrayFromString(xmlStrOut));
            auto xmlStrInArr(getCharArrayFromString(xmlStrIn));

            // now make the library call
            if (state.dataExternalInterface->haveExternalInterfaceBCVTB) {
                retVal = getepvariables(simCfgFilNam.c_str(),
                                        &xmlStrOutTypArr[0],
                                        &xmlStrOutArr[0],
                                        &state.dataExternalInterface->nOutVal,
                                        xmlStrInKey.c_str(),
                                        &state.dataExternalInterface->nInKeys,
                                        &xmlStrInArr[0],
                                        &state.dataExternalInterface->nInpVar,
                                        state.dataExternalInterface->inpVarTypes.data(),
                                        &lenXmlStr);
            } else if (state.dataExternalInterface->haveExternalInterfaceFMUExport) {
                retVal = getepvariablesFMU(simCfgFilNam.c_str(),
                                           &xmlStrOutTypArr[0],
                                           &xmlStrOutArr[0],
                                           &state.dataExternalInterface->nOutVal,
                                           xmlStrInKey.c_str(),
                                           &state.dataExternalInterface->nInKeys,
                                           &xmlStrInArr[0],
                                           &state.dataExternalInterface->nInpVar,
                                           state.dataExternalInterface->inpVarTypes.data(),
                                           &lenXmlStr);
            } else {
                // there should be no else condition at this point, however we'll still assign the error value for completeness
                retVal = -1;
            }

            // then postprocess the char vectors in case they are used after the fact
            xmlStrOutTyp = getStringFromCharArray(xmlStrOutTypArr);
            xmlStrOut = getStringFromCharArray(xmlStrOutArr);
            xmlStrIn = getStringFromCharArray(xmlStrInArr);

            xmlStrOutTypArr.clear();
            xmlStrOutArr.clear();
            xmlStrInArr.clear();

            // handle errors when reading variables.cfg file
            if (retVal < 0) {
                ShowSevereError(state, "ExternalInterface: Error when getting input and output variables for EnergyPlus,");
                ShowContinueError(state, "check simulation.log for error message.");
                state.dataExternalInterface->ErrorsFound = true;
            }

        } else {

            ShowSevereError(state, "ExternalInterface: Did not find file \"" + simCfgFilNam + "\".");
            ShowContinueError(state, "This file needs to be in same directory as in.idf.");
            ShowContinueError(state, "Check the documentation for the ExternalInterface.");
            state.dataExternalInterface->ErrorsFound = true;
        }
        StopExternalInterfaceIfError(state);

        if (state.dataExternalInterface->nOutVal + state.dataExternalInterface->nInpVar > maxVar) {
            ShowSevereError(state, "ExternalInterface: Too many variables to be exchanged.");
            ShowContinueError(state, format("Attempted to exchange {} outputs", state.dataExternalInterface->nOutVal));
            ShowContinueError(state, format("plus {} inputs.", state.dataExternalInterface->nOutVal));
            ShowContinueError(state, format("Maximum allowed is sum is {}.", maxVar));
            ShowContinueError(state, "To fix, increase maxVar in ExternalInterface.cc");
            state.dataExternalInterface->ErrorsFound = true;
        }
        StopExternalInterfaceIfError(state);

        if (state.dataExternalInterface->nOutVal < 0) {
            ShowSevereError(state, "ExternalInterface: Error when getting number of xml values for outputs.");
            state.dataExternalInterface->ErrorsFound = true;
        } else {
            ParseString(xmlStrOut, state.dataExternalInterface->varNames, state.dataExternalInterface->nOutVal);
            ParseString(xmlStrOutTyp, state.dataExternalInterface->varKeys, state.dataExternalInterface->nOutVal);
        }
        StopExternalInterfaceIfError(state);

        if (state.dataExternalInterface->nInpVar < 0) {
            ShowSevereError(state, "ExternalInterface: Error when getting number of xml values for inputs.");
            state.dataExternalInterface->ErrorsFound = true;
        } else {
            ParseString(xmlStrIn, state.dataExternalInterface->inpVarNames, state.dataExternalInterface->nInpVar);
        }
        StopExternalInterfaceIfError(state);

        DisplayString(state, format("Number of outputs in ExternalInterface = {}", state.dataExternalInterface->nOutVal));
        DisplayString(state, format("Number of inputs  in ExternalInterface = {}", state.dataExternalInterface->nInpVar));

        state.dataExternalInterface->InitExternalInterfacefirstCall = false;

    } else if (!state.dataExternalInterface->configuredControlPoints) {
        state.dataExternalInterface->keyVarIndexes.allocate(state.dataExternalInterface->nOutVal);
        state.dataExternalInterface->varTypes.allocate(state.dataExternalInterface->nOutVal);
        GetReportVariableKey(state,
                             state.dataExternalInterface->varKeys,
                             state.dataExternalInterface->nOutVal,
                             state.dataExternalInterface->varNames,
                             state.dataExternalInterface->keyVarIndexes,
                             state.dataExternalInterface->varTypes);
        state.dataExternalInterface->varInd.allocate(state.dataExternalInterface->nInpVar);
        for (i = 1; i <= state.dataExternalInterface->nInpVar; ++i) {
            if (state.dataExternalInterface->inpVarTypes(i) == indexSchedule) {
                state.dataExternalInterface->varInd(i) = GetDayScheduleIndex(state, state.dataExternalInterface->inpVarNames(i));
            } else if (state.dataExternalInterface->inpVarTypes(i) == indexVariable) {
                state.dataExternalInterface->varInd(i) = FindEMSVariable(state, state.dataExternalInterface->inpVarNames(i), 0);
            } else if (state.dataExternalInterface->inpVarTypes(i) == indexActuator) {
                state.dataExternalInterface->varInd(i) = FindEMSVariable(state, state.dataExternalInterface->inpVarNames(i), 0);
            }
            if (state.dataExternalInterface->varInd(i) <= 0) {
                ShowSevereError(state,
                                "ExternalInterface: Error, xml file \"" + simCfgFilNam + "\" declares variable \"" +
                                    state.dataExternalInterface->inpVarNames(i) + "\",");
                ShowContinueError(state, "but variable was not found in idf file.");
                state.dataExternalInterface->ErrorsFound = true;
            }
        }
        StopExternalInterfaceIfError(state);
        // Configure Erl variables
        for (i = 1; i <= state.dataExternalInterface->nInpVar; ++i) {
            if (state.dataExternalInterface->inpVarTypes(i) == indexVariable) { // ems-globalvariable
                state.dataExternalInterface->useEMS = true;
                if (!isExternalInterfaceErlVariable(state, state.dataExternalInterface->varInd(i))) {
                    ShowSevereError(state,
                                    "ExternalInterface: Error, xml file \"" + simCfgFilNam + "\" declares variable \"" +
                                        state.dataExternalInterface->inpVarNames(i) + "\",");
                    ShowContinueError(state, "But this variable is an ordinary Erl variable, not an ExternalInterface variable.");
                    ShowContinueError(state, "You must specify a variable of type \"ExternalInterface:Variable\".");
                    state.dataExternalInterface->ErrorsFound = true;
                }
            } else if (state.dataExternalInterface->inpVarTypes(i) == indexActuator) { // ems-actuator
                state.dataExternalInterface->useEMS = true;
                if (!isExternalInterfaceErlVariable(state, state.dataExternalInterface->varInd(i))) {
                    ShowSevereError(state,
                                    "ExternalInterface: Error, xml file \"" + simCfgFilNam + "\" declares variable \"" +
                                        state.dataExternalInterface->inpVarNames(i) + "\",");
                    ShowContinueError(state, "But this variable is an ordinary Erl actuator, not an ExternalInterface actuator.");
                    ShowContinueError(state, "You must specify a variable of type \"ExternalInterface:Actuator\".");
                    state.dataExternalInterface->ErrorsFound = true;
                }
            }
        }
        state.dataExternalInterface->configuredControlPoints = true;
    }
    StopExternalInterfaceIfError(state);
}

void GetSetVariablesAndDoStepFMUImport(EnergyPlusData &state)
{
    // SUBROUTINE INFORMATION:
    //       AUTHOR         Thierry S. Nouidui, Michael Wetter, Wangda Zuo
    //       DATE WRITTEN   08Aug2011
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // This routine gets, sets and does the time integration in FMUs.

    // Using/Aliasing
    using EMSManager::ManageEMS;

    using RuntimeLanguageProcessor::ExternalInterfaceSetErlVariable;
    using RuntimeLanguageProcessor::FindEMSVariable;
    using RuntimeLanguageProcessor::isExternalInterfaceErlVariable;
    using ScheduleManager::ExternalInterfaceSetSchedule;

    // SUBROUTINE PARAMETER DEFINITIONS:

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int i, j, k; // Loop counters

    for (i = 1; i <= state.dataExternalInterface->NumFMUObjects; ++i) {
        for (j = 1; j <= state.dataExternalInterface->FMU(i).NumInstances; ++j) {
            if (state.dataExternalInterface->FlagReIni) {
                // Get from FMUs, values that will be set in EnergyPlus (Schedule)
                for (k = 1; k <= state.dataExternalInterface->FMUTemp(i).Instance(j).NumOutputVariablesSchedule; ++k) {
                    state.dataExternalInterface->FMU(i).Instance(j).fmuOutputVariableSchedule(k).RealVarValue =
                        state.dataExternalInterface->FMUTemp(i).Instance(j).fmuOutputVariableSchedule(k).RealVarValue;
                }

                // Get from FMUs, values that will be set in EnergyPlus (Variable)
                for (k = 1; k <= state.dataExternalInterface->FMUTemp(i).Instance(j).NumOutputVariablesVariable; ++k) {
                    state.dataExternalInterface->FMU(i).Instance(j).fmuOutputVariableVariable(k).RealVarValue =
                        state.dataExternalInterface->FMUTemp(i).Instance(j).fmuOutputVariableVariable(k).RealVarValue;
                }

                // Get from FMUs, values that will be set in EnergyPlus (Actuator)
                for (k = 1; k <= state.dataExternalInterface->FMUTemp(i).Instance(j).NumOutputVariablesActuator; ++k) {
                    state.dataExternalInterface->FMU(i).Instance(j).fmuOutputVariableActuator(k).RealVarValue =
                        state.dataExternalInterface->FMUTemp(i).Instance(j).fmuOutputVariableActuator(k).RealVarValue;
                }
            } else {
                // Get from FMUs, values that will be set in EnergyPlus (Schedule)

                if (size(state.dataExternalInterface->FMU(i).Instance(j).fmuOutputVariableSchedule) > 0) {

                    // generate vectors here first
                    std::vector<unsigned int> valueReferenceVec;
                    std::vector<Real64> realVarValueVec;
                    for (unsigned long x = 1; x <= size(state.dataExternalInterface->FMU(i).Instance(j).fmuOutputVariableSchedule); ++x) {
                        valueReferenceVec.push_back(state.dataExternalInterface->FMU(i).Instance(j).fmuOutputVariableSchedule(x).ValueReference);
                        realVarValueVec.push_back(state.dataExternalInterface->FMU(i).Instance(j).fmuOutputVariableSchedule(x).RealVarValue);
                    }

                    // pass in the vectors as pointers to the first member of the vector
                    state.dataExternalInterface->FMU(i).Instance(j).fmistatus =
                        fmiEPlusGetReal(&state.dataExternalInterface->FMU(i).Instance(j).fmicomponent,
                                        &valueReferenceVec[0],
                                        &realVarValueVec[0],
                                        &state.dataExternalInterface->FMU(i).Instance(j).NumOutputVariablesSchedule,
                                        &state.dataExternalInterface->FMU(i).Instance(j).Index);

                    for (unsigned long x = 1; x <= size(state.dataExternalInterface->FMU(i).Instance(j).fmuOutputVariableSchedule); ++x) {
                        state.dataExternalInterface->FMU(i).Instance(j).fmuOutputVariableSchedule(x).ValueReference = valueReferenceVec[x - 1];
                        state.dataExternalInterface->FMU(i).Instance(j).fmuOutputVariableSchedule(x).RealVarValue = realVarValueVec[x - 1];
                    }

                    if (state.dataExternalInterface->FMU(i).Instance(j).fmistatus != fmiOK) {
                        ShowSevereError(state, "ExternalInterface/GetSetVariablesAndDoStepFMUImport: Error when trying to get outputs");
                        ShowContinueError(state,
                                          "in instance \"" + state.dataExternalInterface->FMU(i).Instance(j).Name + "\" of FMU \"" +
                                              state.dataExternalInterface->FMU(i).Name + "\"");
                        ShowContinueError(state, format("Error Code = \"{}\"", state.dataExternalInterface->FMU(i).Instance(j).fmistatus));
                        state.dataExternalInterface->ErrorsFound = true;
                        StopExternalInterfaceIfError(state);
                    }
                }

                // generate vectors here first
                if (size(state.dataExternalInterface->FMU(i).Instance(j).fmuOutputVariableVariable) > 0) {

                    std::vector<unsigned int> valueReferenceVec2;
                    std::vector<Real64> realVarValueVec2;
                    for (unsigned long x = 1; x <= size(state.dataExternalInterface->FMU(i).Instance(j).fmuOutputVariableVariable); ++x) {
                        valueReferenceVec2.push_back(state.dataExternalInterface->FMU(i).Instance(j).fmuOutputVariableVariable(x).ValueReference);
                        realVarValueVec2.push_back(state.dataExternalInterface->FMU(i).Instance(j).fmuOutputVariableVariable(x).RealVarValue);
                    }

                    // pass in the vectors as pointers to the first member of the vector
                    state.dataExternalInterface->FMU(i).Instance(j).fmistatus =
                        fmiEPlusGetReal(&state.dataExternalInterface->FMU(i).Instance(j).fmicomponent,
                                        &valueReferenceVec2[0],
                                        &realVarValueVec2[0],
                                        &state.dataExternalInterface->FMU(i).Instance(j).NumOutputVariablesVariable,
                                        &state.dataExternalInterface->FMU(i).Instance(j).Index);

                    for (unsigned long x = 1; x <= size(state.dataExternalInterface->FMU(i).Instance(j).fmuOutputVariableVariable); ++x) {
                        state.dataExternalInterface->FMU(i).Instance(j).fmuOutputVariableVariable(x).ValueReference = valueReferenceVec2[x - 1];
                        state.dataExternalInterface->FMU(i).Instance(j).fmuOutputVariableVariable(x).RealVarValue = realVarValueVec2[x - 1];
                    }

                    if (state.dataExternalInterface->FMU(i).Instance(j).fmistatus != fmiOK) {
                        ShowSevereError(state, "ExternalInterface/GetSetVariablesAndDoStepFMUImport: Error when trying to get outputs");
                        ShowContinueError(state,
                                          "in instance \"" + state.dataExternalInterface->FMU(i).Instance(j).Name + "\" of FMU \"" +
                                              state.dataExternalInterface->FMU(i).Name + "\"");
                        ShowContinueError(state, format("Error Code = \"{}\"", state.dataExternalInterface->FMU(i).Instance(j).fmistatus));
                        state.dataExternalInterface->ErrorsFound = true;
                        StopExternalInterfaceIfError(state);
                    }
                }

                if (size(state.dataExternalInterface->FMU(i).Instance(j).fmuOutputVariableActuator) > 0) {

                    // generate vectors here first
                    std::vector<unsigned int> valueReferenceVec3;
                    std::vector<Real64> realVarValueVec3;
                    for (unsigned long x = 1; x <= size(state.dataExternalInterface->FMU(i).Instance(j).fmuOutputVariableActuator); ++x) {
                        valueReferenceVec3.push_back(state.dataExternalInterface->FMU(i).Instance(j).fmuOutputVariableActuator(x).ValueReference);
                        realVarValueVec3.push_back(state.dataExternalInterface->FMU(i).Instance(j).fmuOutputVariableActuator(x).RealVarValue);
                    }

                    // pass in the vectors as pointers to the first member of the vector
                    state.dataExternalInterface->FMU(i).Instance(j).fmistatus =
                        fmiEPlusGetReal(&state.dataExternalInterface->FMU(i).Instance(j).fmicomponent,
                                        &valueReferenceVec3[0],
                                        &realVarValueVec3[0],
                                        &state.dataExternalInterface->FMU(i).Instance(j).NumOutputVariablesActuator,
                                        &state.dataExternalInterface->FMU(i).Instance(j).Index);

                    for (unsigned long x = 1; x <= size(state.dataExternalInterface->FMU(i).Instance(j).fmuOutputVariableActuator); ++x) {
                        state.dataExternalInterface->FMU(i).Instance(j).fmuOutputVariableActuator(x).ValueReference = valueReferenceVec3[x - 1];
                        state.dataExternalInterface->FMU(i).Instance(j).fmuOutputVariableActuator(x).RealVarValue = realVarValueVec3[x - 1];
                    }

                    if (state.dataExternalInterface->FMU(i).Instance(j).fmistatus != fmiOK) {
                        ShowSevereError(state, "ExternalInterface/GetSetVariablesAndDoStepFMUImport: Error when trying to get outputs");
                        ShowContinueError(state,
                                          "in instance \"" + state.dataExternalInterface->FMU(i).Instance(j).Name + "\" of FMU \"" +
                                              state.dataExternalInterface->FMU(i).Name + "\"");
                        ShowContinueError(state, format("Error Code = \"{}\"", state.dataExternalInterface->FMU(i).Instance(j).fmistatus));
                        state.dataExternalInterface->ErrorsFound = true;
                        StopExternalInterfaceIfError(state);
                    }
                }
            }

            // Set in EnergyPlus the values of the schedules
            for (k = 1; k <= state.dataExternalInterface->FMU(i).Instance(j).NumOutputVariablesSchedule; ++k) {
                ExternalInterfaceSetSchedule(state,
                                             state.dataExternalInterface->FMU(i).Instance(j).eplusInputVariableSchedule(k).VarIndex,
                                             state.dataExternalInterface->FMU(i).Instance(j).fmuOutputVariableSchedule(k).RealVarValue);
            }

            // Set in EnergyPlus the values of the variables
            for (k = 1; k <= state.dataExternalInterface->FMU(i).Instance(j).NumOutputVariablesVariable; ++k) {
                ExternalInterfaceSetErlVariable(state,
                                                state.dataExternalInterface->FMU(i).Instance(j).eplusInputVariableVariable(k).VarIndex,
                                                state.dataExternalInterface->FMU(i).Instance(j).fmuOutputVariableVariable(k).RealVarValue);
            }

            // Set in EnergyPlus the values of the actuators
            for (k = 1; k <= state.dataExternalInterface->FMU(i).Instance(j).NumOutputVariablesActuator; ++k) {
                ExternalInterfaceSetErlVariable(state,
                                                state.dataExternalInterface->FMU(i).Instance(j).eplusInputVariableActuator(k).VarIndex,
                                                state.dataExternalInterface->FMU(i).Instance(j).fmuOutputVariableActuator(k).RealVarValue);
            }

            if (state.dataExternalInterface->FirstCallGetSetDoStep) {
                // Get from EnergyPlus, values that will be set in fmus
                for (k = 1; k <= state.dataExternalInterface->FMU(i).Instance(j).NumInputVariablesInIDF; ++k) {
                    // This make sure that the variables are updated at the Zone Time Step
                    state.dataExternalInterface->FMU(i).Instance(j).eplusOutputVariable(k).RTSValue =
                        GetInternalVariableValue(state,
                                                 state.dataExternalInterface->FMU(i).Instance(j).eplusOutputVariable(k).VarType,
                                                 state.dataExternalInterface->FMU(i).Instance(j).eplusOutputVariable(k).VarIndex);
                }
            } else {
                // Get from EnergyPlus, values that will be set in fmus
                for (k = 1; k <= state.dataExternalInterface->FMU(i).Instance(j).NumInputVariablesInIDF; ++k) {
                    // This make sure that the variables are updated at the Zone Time Step
                    state.dataExternalInterface->FMU(i).Instance(j).eplusOutputVariable(k).RTSValue =
                        GetInternalVariableValueExternalInterface(state,
                                                                  state.dataExternalInterface->FMU(i).Instance(j).eplusOutputVariable(k).VarType,
                                                                  state.dataExternalInterface->FMU(i).Instance(j).eplusOutputVariable(k).VarIndex);
                }
            }

            if (!state.dataExternalInterface->FlagReIni) {

                // generate vectors here first
                std::vector<unsigned int> valueReferenceVec4;
                for (unsigned long x = 1; x <= size(state.dataExternalInterface->FMU(i).Instance(j).fmuInputVariable); ++x) {
                    valueReferenceVec4.push_back(state.dataExternalInterface->FMU(i).Instance(j).fmuInputVariable(x).ValueReference);
                }

                std::vector<Real64> rtsValueVec4;
                for (unsigned long x = 1; x <= size(state.dataExternalInterface->FMU(i).Instance(j).eplusOutputVariable); ++x) {
                    rtsValueVec4.push_back(state.dataExternalInterface->FMU(i).Instance(j).eplusOutputVariable(x).RTSValue);
                }

                state.dataExternalInterface->FMU(i).Instance(j).fmistatus =
                    fmiEPlusSetReal(&state.dataExternalInterface->FMU(i).Instance(j).fmicomponent,
                                    &valueReferenceVec4[0],
                                    &rtsValueVec4[0],
                                    &state.dataExternalInterface->FMU(i).Instance(j).NumInputVariablesInIDF,
                                    &state.dataExternalInterface->FMU(i).Instance(j).Index);

                if (state.dataExternalInterface->FMU(i).Instance(j).fmistatus != fmiOK) {
                    ShowSevereError(state, "ExternalInterface/GetSetVariablesAndDoStepFMUImport: Error when trying to set inputs");
                    ShowContinueError(state,
                                      "in instance \"" + state.dataExternalInterface->FMU(i).Instance(j).Name + "\" of FMU \"" +
                                          state.dataExternalInterface->FMU(i).Name + "\"");
                    ShowContinueError(state, format("Error Code = \"{}\"", state.dataExternalInterface->FMU(i).Instance(j).fmistatus));
                    state.dataExternalInterface->ErrorsFound = true;
                    StopExternalInterfaceIfError(state);
                }
            }
            int localfmitrue(fmiTrue);
            // Call and simulate the FMUs to get values at the corresponding timestep.
            state.dataExternalInterface->FMU(i).Instance(j).fmistatus = fmiEPlusDoStep(&state.dataExternalInterface->FMU(i).Instance(j).fmicomponent,
                                                                                       &state.dataExternalInterface->tComm,
                                                                                       &state.dataExternalInterface->hStep,
                                                                                       &localfmitrue,
                                                                                       &state.dataExternalInterface->FMU(i).Instance(j).Index);
            if (state.dataExternalInterface->FMU(i).Instance(j).fmistatus != fmiOK) {
                ShowSevereError(state, "ExternalInterface/GetSetVariablesAndDoStepFMUImport: Error when trying to");
                ShowContinueError(state, "do the coSimulation with instance \"" + state.dataExternalInterface->FMU(i).Instance(j).Name + "\"");
                ShowContinueError(state, "of FMU \"" + state.dataExternalInterface->FMU(i).Name + "\"");
                ShowContinueError(state, format("Error Code = \"{}\"", state.dataExternalInterface->FMU(i).Instance(j).fmistatus));
                state.dataExternalInterface->ErrorsFound = true;
                StopExternalInterfaceIfError(state);
            }
        }
    }

    // If we have Erl variables, we need to call ManageEMS so that they get updated in the Erl data structure
    if (state.dataExternalInterface->useEMS) {
        bool anyRan;
        ManageEMS(state, EMSManager::EMSCallFrom::ExternalInterface, anyRan, ObjexxFCL::Optional_int_const());
    }

    state.dataExternalInterface->FirstCallGetSetDoStep = false;
}

void InstantiateInitializeFMUImport(EnergyPlusData &state)
{
    // SUBROUTINE INFORMATION:
    //       AUTHOR         Thierry S. Nouidui, Michael Wetter, Wangda Zuo
    //       DATE WRITTEN   08Aug2011
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // This routine instantiates and initializes FMUs.

    // Using/Aliasing

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:

    // Instantiate FMUs
    for (int i = 1; i <= state.dataExternalInterface->NumFMUObjects; ++i) {
        for (int j = 1; j <= state.dataExternalInterface->FMU(i).NumInstances; ++j) {
            auto folderStr = state.dataExternalInterface->FMU(i).Instance(j).WorkingFolder.string();
            state.dataExternalInterface->FMU(i).Instance(j).fmicomponent =
                fmiEPlusInstantiateSlave((char *)folderStr.c_str(),
                                         &state.dataExternalInterface->FMU(i).Instance(j).LenWorkingFolder,
                                         &state.dataExternalInterface->FMU(i).TimeOut,
                                         &state.dataExternalInterface->FMU(i).Visible,
                                         &state.dataExternalInterface->FMU(i).Interactive,
                                         &state.dataExternalInterface->FMU(i).LoggingOn,
                                         &state.dataExternalInterface->FMU(i).Instance(j).Index);
            // TODO: This is doing a null pointer check; OK?
            if (!state.dataExternalInterface->FMU(i).Instance(j).fmicomponent) {
                ShowSevereError(state, "ExternalInterface/CalcExternalInterfaceFMUImport: Error when trying to instantiate");
                ShowContinueError(state,
                                  "instance \"" + state.dataExternalInterface->FMU(i).Instance(j).Name + "\" of FMU \"" +
                                      state.dataExternalInterface->FMU(i).Name + "\"");
                state.dataExternalInterface->ErrorsFound = true;
                StopExternalInterfaceIfError(state);
            }
        }
    }

    // Initialize FMUs
    int localfmiTrue(fmiTrue);
    for (int i = 1; i <= state.dataExternalInterface->NumFMUObjects; ++i) {
        for (int j = 1; j <= state.dataExternalInterface->FMU(i).NumInstances; ++j) {
            state.dataExternalInterface->FMU(i).Instance(j).fmistatus =
                fmiEPlusInitializeSlave(&state.dataExternalInterface->FMU(i).Instance(j).fmicomponent,
                                        &state.dataExternalInterface->tStart,
                                        &localfmiTrue,
                                        &state.dataExternalInterface->tStop,
                                        &state.dataExternalInterface->FMU(i).Instance(j).Index);
            if (state.dataExternalInterface->FMU(i).Instance(j).fmistatus != fmiOK) {
                ShowSevereError(state, "ExternalInterface/CalcExternalInterfaceFMUImport: Error when trying to initialize");
                ShowContinueError(state,
                                  "instance \"" + state.dataExternalInterface->FMU(i).Instance(j).Name + "\" of FMU \"" +
                                      state.dataExternalInterface->FMU(i).Name + "\"");
                ShowContinueError(state, format("Error Code = \"{}\"", state.dataExternalInterface->FMU(i).Instance(j).fmistatus));
                state.dataExternalInterface->ErrorsFound = true;
                StopExternalInterfaceIfError(state);
            }
        }
    }
}

void InitializeFMU(EnergyPlusData &state)
{
    // SUBROUTINE INFORMATION:
    //       AUTHOR         Thierry S. Nouidui, Michael Wetter, Wangda Zuo
    //       DATE WRITTEN   08Aug2011
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // This routine reinitializes FMUs.

    // Using/Aliasing

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int i, j; // Loop counters
    int localfmiTrue(fmiTrue);

    // Initialize FMUs
    for (i = 1; i <= state.dataExternalInterface->NumFMUObjects; ++i) {
        for (j = 1; j <= state.dataExternalInterface->FMU(i).NumInstances; ++j) {
            state.dataExternalInterface->FMU(i).Instance(j).fmistatus =
                fmiEPlusInitializeSlave(&state.dataExternalInterface->FMU(i).Instance(j).fmicomponent,
                                        &state.dataExternalInterface->tStart,
                                        &localfmiTrue,
                                        &state.dataExternalInterface->tStop,
                                        &state.dataExternalInterface->FMU(i).Instance(j).Index);
            if (state.dataExternalInterface->FMU(i).Instance(j).fmistatus != fmiOK) {
                ShowSevereError(state, "ExternalInterface/CalcExternalInterfaceFMUImport: Error when trying to initialize");
                ShowContinueError(state,
                                  "instance \"" + state.dataExternalInterface->FMU(i).Instance(j).Name + "\" of FMU \"" +
                                      state.dataExternalInterface->FMU(i).Name + "\"");
                ShowContinueError(state, format("Error Code = \"{}\"", state.dataExternalInterface->FMU(i).Instance(j).fmistatus));
                state.dataExternalInterface->ErrorsFound = true;
                StopExternalInterfaceIfError(state);
            }
        }
    }
}

void TerminateResetFreeFMUImport(EnergyPlusData &state, int fmiEndSimulation)
{
    // SUBROUTINE INFORMATION:
    //       AUTHOR         Thierry S. Nouidui, Michael Wetter, Wangda Zuo
    //       DATE WRITTEN   08Aug2011
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // This routine terminates the FMUs instances

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int i, j; // Loop counter

    //----Needs to have function that allows to terminates FMU. Was not defined in version 1.0 -- fixme
    for (i = 1; i <= state.dataExternalInterface->NumFMUObjects; ++i) {
        for (j = 1; j <= state.dataExternalInterface->FMU(i).NumInstances; ++j) {
            if (state.dataExternalInterface->FMU(i).Instance(j).fmistatus != fmiFatal) {
                // Cleanup slaves
                state.dataExternalInterface->FMU(i).Instance(j).fmistatus =
                    fmiEPlusFreeSlave(&state.dataExternalInterface->FMU(i).Instance(j).fmicomponent,
                                      &state.dataExternalInterface->FMU(i).Instance(j).Index,
                                      &fmiEndSimulation);
            }
            // check if fmiComponent has been freed
            if (!state.dataExternalInterface->FMU(i).Instance(j).fmicomponent) {
                ShowSevereError(state, "ExternalInterface/TerminateResetFreeFMUImport: Error when trying to terminate");
                ShowContinueError(state,
                                  "instance \"" + state.dataExternalInterface->FMU(i).Instance(j).Name + "\" of FMU \"" +
                                      state.dataExternalInterface->FMU(i).Name + "\"");
                state.dataExternalInterface->ErrorsFound = true;
                StopExternalInterfaceIfError(state);
            }
        }
    }
}

void InitExternalInterfaceFMUImport(EnergyPlusData &state)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Thierry S. Nouidui, Michael Wetter, Wangda Zuo
    //       DATE WRITTEN   08Aug2011
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // This routine initializes the input and outputs variables used for the co-simulation with FMUs.

    // Using/Aliasing
    using DataStringGlobals::altpathChar;
    using DataStringGlobals::pathChar;
    using DataSystemVariables::CheckForActualFilePath;

    using RuntimeLanguageProcessor::FindEMSVariable;
    using RuntimeLanguageProcessor::isExternalInterfaceErlVariable;
    using ScheduleManager::GetDayScheduleIndex;

    // Locals
    int i, j, k, l, Loop;        // Loop counters
    int retVal;                  // Return value of function call, used for error handling
    int NumAlphas(0);            // Number of Alphas for each GetObjectItem call
    int NumNumbers(0);           // Number of Numbers for each GetObjectItem call
    int IOStatus(0);             // Used in GetObjectItem
    int NumFMUInputVariables(0); // Number of FMU input variables
    std::string Name_NEW;        // Units sting, may be blank
    std::string Name_OLD;        // Units sting, may be blank

    Array1D_int keyIndexes(1);                          // Array index for
    Array1D<OutputProcessor::VariableType> varTypes(1); // Array index for
    Array1D_string NamesOfKeys(1);                      // Specific key name
    int retValfmiVersion;
    int retValfmiPathLib;
    Array1D_string NameListInstances(5);
    fs::path tempFullFilePath;

    Array1D_string strippedFileName; // remove path from entered file name
    Array1D_string fullFileName;     // entered file name/found

    std::string::size_type pos;
    int FOUND;

    if (state.dataExternalInterface->FirstCallIni) {
        DisplayString(state, "Initializing FunctionalMockupUnitImport interface");
        // do one time initializations
        ValidateRunControl(state);
        state.dataExternalInterface->FMU.allocate(state.dataExternalInterface->NumFMUObjects);

        // there used to be code in here to apply the root working folder to create an absolute path
        // however, this wasn't working, as the root working folder was coming back empty
        // in any case, the relative paths work fine here

        // post process as needed in case these are used later
        state.dataExternalInterface->FMURootWorkingFolder = fs::path("tmp-fmus"); // getStringFromCharArray( FMUWorkingFolderCharArr );

        // Get and store the names of all FMUs in EnergyPlus data structure
        strippedFileName.allocate(state.dataExternalInterface->NumFMUObjects);
        fullFileName.allocate(state.dataExternalInterface->NumFMUObjects);

        auto &cCurrentModuleObject = state.dataIPShortCut->cCurrentModuleObject;
        cCurrentModuleObject = "ExternalInterface:FunctionalMockupUnitImport";
        for (Loop = 1; Loop <= state.dataExternalInterface->NumFMUObjects; ++Loop) {
            state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                     cCurrentModuleObject,
                                                                     Loop,
                                                                     state.dataIPShortCut->cAlphaArgs,
                                                                     NumAlphas,
                                                                     state.dataIPShortCut->rNumericArgs,
                                                                     NumNumbers,
                                                                     IOStatus,
                                                                     _,
                                                                     _,
                                                                     state.dataIPShortCut->cAlphaFieldNames,
                                                                     state.dataIPShortCut->cNumericFieldNames);
            // Get the FMU name
            state.dataExternalInterface->FMU(Loop).Name = state.dataIPShortCut->cAlphaArgs(1);

            fs::path inputPath = FileSystem::makeNativePath(state.dataExternalInterface->FMU(Loop).Name);

            std::string contextString = cCurrentModuleObject + ", " + state.dataIPShortCut->cAlphaFieldNames(1) + ": ";

            tempFullFilePath = CheckForActualFilePath(state, inputPath, contextString);
            if (!tempFullFilePath.empty()) {

                // TODO: eliminate this old block once confident
                pos = index(state.dataExternalInterface->FMU(Loop).Name, pathChar, true); // look backwards
                if (pos != std::string::npos) {
                    strippedFileName(Loop) = state.dataExternalInterface->FMU(Loop).Name.substr(pos + 1);
                } else {                                                                         // pos == 0, look for alt path char
                    pos = index(state.dataExternalInterface->FMU(Loop).Name, altpathChar, true); // look backwards
                    if (pos != std::string::npos) {
                        strippedFileName(Loop) = state.dataExternalInterface->FMU(Loop).Name.substr(pos + 1);
                    } else {
                        strippedFileName(Loop) = state.dataExternalInterface->FMU(Loop).Name;
                    }
                }
                fullFileName(Loop) = tempFullFilePath.string();
            } else {
                state.dataExternalInterface->ErrorsFound = true;
            }
            // Get fmu time out
            state.dataExternalInterface->FMU(Loop).TimeOut = state.dataIPShortCut->rNumericArgs(1);
            // Get fmu logging on
            state.dataExternalInterface->FMU(Loop).LoggingOn = state.dataIPShortCut->rNumericArgs(2);
        }

        // check for dups that aren't the same file
        // this is windows code...
        // So this check that if I entered two different things and get the same end filename, then it's wrong?
        for (j = 1; j <= state.dataExternalInterface->NumFMUObjects; ++j) {
            for (k = 2; k <= state.dataExternalInterface->NumFMUObjects; ++k) {
                if (!UtilityRoutines::SameString(strippedFileName(j), strippedFileName(k))) continue;
                // base file names are the same
                if (UtilityRoutines::SameString(fullFileName(j), fullFileName(k))) continue;
                ShowSevereError(state, "ExternalInterface/InitExternalInterfaceFMUImport:");
                ShowContinueError(state, "duplicate file names (but not same file) entered.");
                ShowContinueError(state, "...entered file name=\"" + state.dataExternalInterface->FMU(j).Name + "\"");
                ShowContinueError(state, "...   full file name=\"" + fullFileName(j) + "\"");
                ShowContinueError(state, "...entered file name=\"" + state.dataExternalInterface->FMU(k).Name + "\"");
                ShowContinueError(state, "...   full file name=\"" + fullFileName(k) + "\"");
                ShowContinueError(state, "...name collision but not same file name.");
                state.dataExternalInterface->ErrorsFound = true;
            }
        }

        if (state.dataExternalInterface->ErrorsFound) {
            strippedFileName.deallocate();
            fullFileName.deallocate();
            StopExternalInterfaceIfError(state);
        }

        // get the names of the input variables each state.dataExternalInterface->FMU(and the names of the
        // corresponding output variables in EnergyPlus --).
        cCurrentModuleObject = "ExternalInterface:FunctionalMockupUnitImport:From:Variable";
        NumFMUInputVariables = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, cCurrentModuleObject);
        // Determine the number of instances for each FMUs
        for (i = 1; i <= state.dataExternalInterface->NumFMUObjects; ++i) {
            Name_NEW = "";
            Name_OLD = "";
            j = 1;
            k = 1;
            state.dataExternalInterface->FMU(i).Instance.allocate(NumFMUInputVariables);
            state.dataExternalInterface->checkInstanceName.allocate(NumFMUInputVariables);
            for (l = 1; l <= NumFMUInputVariables; ++l) {
                state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                         cCurrentModuleObject,
                                                                         l,
                                                                         state.dataIPShortCut->cAlphaArgs,
                                                                         NumAlphas,
                                                                         state.dataIPShortCut->rNumericArgs,
                                                                         NumNumbers,
                                                                         IOStatus,
                                                                         _,
                                                                         _,
                                                                         state.dataIPShortCut->cAlphaFieldNames,
                                                                         state.dataIPShortCut->cNumericFieldNames);
                if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(3), state.dataExternalInterface->FMU(i).Name)) {
                    Name_NEW = state.dataIPShortCut->cAlphaArgs(4);
                    if (!UtilityRoutines::SameString(Name_OLD, Name_NEW)) {
                        FOUND = UtilityRoutines::FindItem(Name_NEW, state.dataExternalInterface->checkInstanceName);
                        if (FOUND == 0) {
                            state.dataExternalInterface->checkInstanceName(l).Name = Name_NEW;
                            state.dataExternalInterface->FMU(i).NumInstances = j;
                            state.dataExternalInterface->FMU(i).Instance(j).Name = Name_NEW;
                            ++j;
                            Name_OLD = Name_NEW;
                        }
                    }
                    state.dataExternalInterface->FMU(i).TotNumInputVariablesInIDF = k;
                    ++k;
                }
            }
            state.dataExternalInterface->checkInstanceName.deallocate();
        }

        for (i = 1; i <= state.dataExternalInterface->NumFMUObjects; ++i) {
            if (state.dataExternalInterface->FMU(i).NumInstances == 0) {
                ShowSevereError(
                    state, "ExternalInterface/InitExternalInterfaceFMUImport: The FMU \"" + state.dataExternalInterface->FMU(i).Name + "\" does");
                ShowContinueError(state, "not have any instances or any input variable. An FMU should have at least one instance");
                ShowContinueError(state, "or one input variable defined in input file. Check FMU object in the input file.");
                state.dataExternalInterface->ErrorsFound = true;
                StopExternalInterfaceIfError(state);
            }
            if (NumFMUInputVariables > 0 && state.dataExternalInterface->FMU(i).TotNumInputVariablesInIDF == 0) {
                ShowWarningError(state, "InitExternalInterfaceFMUImport: The FMU \"" + state.dataExternalInterface->FMU(i).Name + "\"");
                ShowContinueError(state, "is defined but has no input variables.");
                ShowContinueError(state, "Check the input field of the corresponding object");
                ShowContinueError(state, "ExternalInterface:FunctionalMockupUnitImport:From:Variable.");
            }
        }

        // write output folder where FMUs will be unpacked later on.
        for (i = 1; i <= state.dataExternalInterface->NumFMUObjects; ++i) {
            for (j = 1; j <= state.dataExternalInterface->FMU(i).NumInstances; ++j) {
                state.dataExternalInterface->FMU(i).Instance(j).WorkingFolder =
                    state.dataExternalInterface->FMURootWorkingFolder /
                    fs::path(strippedFileName(i) + '_' + state.dataExternalInterface->FMU(i).Instance(j).Name);
            }
        }

        // parse the fmu defined in the idf using the fmuUnpack.
        for (i = 1; i <= state.dataExternalInterface->NumFMUObjects; ++i) {
            for (j = 1; j <= state.dataExternalInterface->FMU(i).NumInstances; ++j) {
                // get the length of working folder trimmed
                state.dataExternalInterface->FMU(i).Instance(j).LenWorkingFolder =
                    state.dataExternalInterface->FMU(i).Instance(j).WorkingFolder.string().length();
                // unpack fmus
                // preprocess arguments for library call
                {
                    auto fullFileNameArr(getCharArrayFromString(fullFileName(i)));
                    auto workingFolderArr(getCharArrayFromString(state.dataExternalInterface->FMU(i).Instance(j).WorkingFolder.string()));
                    int lenFileName(len(fullFileName(i)));

                    // make the library call
                    retVal = fmiEPlusUnpack(
                        &fullFileNameArr[0], &workingFolderArr[0], &lenFileName, &state.dataExternalInterface->FMU(i).Instance(j).LenWorkingFolder);

                    if (retVal != 0) {
                        ShowSevereError(state, "ExternalInterface/InitExternalInterfaceFMUImport: Error when trying to");
                        ShowContinueError(state, "unpack the FMU \"" + state.dataExternalInterface->FMU(i).Name + "\".");
                        ShowContinueError(state, "Check if the FMU exists. Also check if the FMU folder is not write protected.");
                        state.dataExternalInterface->ErrorsFound = true;
                        StopExternalInterfaceIfError(state);
                    }
                }

                {
                    // determine modelID and modelGUID of all FMU instances
                    // preprocess arguments for library call
                    auto workingFolderArr(getCharArrayFromString(state.dataExternalInterface->FMU(i).Instance(j).WorkingFolder.string()));

                    // make the library call
                    state.dataExternalInterface->FMU(i).Instance(j).Index =
                        model_ID_GUID((char *)state.dataExternalInterface->FMU(i).Instance(j).Name.c_str(),
                                      &workingFolderArr[0],
                                      &state.dataExternalInterface->FMU(i).Instance(j).LenWorkingFolder,
                                      &state.dataExternalInterface->FMU(i).Instance(j).NumInputVariablesInFMU,
                                      &state.dataExternalInterface->FMU(i).Instance(j).NumOutputVariablesInFMU);

                    if (state.dataExternalInterface->FMU(i).Instance(j).Index < 0) {
                        ShowSevereError(state, "ExternalInterface/InitExternalInterfaceFMUImport: Error when trying to");
                        ShowContinueError(state, "get the model ID and model GUID");
                        ShowContinueError(state,
                                          "of instance \"" + state.dataExternalInterface->FMU(i).Instance(j).Name + "\" of FMU \"" +
                                              state.dataExternalInterface->FMU(i).Name + "\".");
                        ShowContinueError(state, "Check if modelDescription.xml exists in the folder where the FMU has been unpacked.");
                        state.dataExternalInterface->ErrorsFound = true;
                        StopExternalInterfaceIfError(state);
                    }
                }

                {
                    // get the path to the binaries
                    // preprocess args for library call
                    auto workingFolderArr(getCharArrayFromString(state.dataExternalInterface->FMU(i).Instance(j).WorkingFolder.string()));
                    // Reserve some space in the string, becasue addLibPathCurrentWorkflowFolder doesn't allocate memory for the
                    // workingFolderWithLibArr Note: you can't call str.resize(str.length() + 91) because the conversion to std::vector<char> will
                    // find the null terminator and so it will have no effect
                    std::string reservedString = state.dataExternalInterface->FMU(i).Instance(j).WorkingFolder.string() +
                                                 "                                                                                           ";
                    auto workingFolderWithLibArr(getCharArrayFromString(reservedString));

                    // make the library call
                    retValfmiPathLib = addLibPathCurrentWorkingFolder(&workingFolderWithLibArr[0],
                                                                      &workingFolderArr[0],
                                                                      &state.dataExternalInterface->FMU(i).Instance(j).LenWorkingFolder,
                                                                      &state.dataExternalInterface->FMU(i).Instance(j).Index);

                    // post process args in case they are used later
                    state.dataExternalInterface->FMU(i).Instance(j).WorkingFolder_wLib =
                        fs::path(trim(getStringFromCharArray(workingFolderWithLibArr)));

                    if (retValfmiPathLib != 0) {
                        ShowSevereError(state, "ExternalInterface/InitExternalInterfaceFMUImport: Error when trying to");
                        ShowContinueError(state, "get the path to the binaries of instance");
                        ShowContinueError(state,
                                          "\"" + state.dataExternalInterface->FMU(i).Instance(j).Name + "\" of FMU \"" +
                                              state.dataExternalInterface->FMU(i).Name + "\".");
                        ShowContinueError(state, "Check if binaries folder exists where the FMU has been unpacked.");
                        state.dataExternalInterface->ErrorsFound = true;
                        StopExternalInterfaceIfError(state);
                    }

                    // get the length of the working folder with libraries
                    state.dataExternalInterface->FMU(i).Instance(j).LenWorkingFolder_wLib =
                        state.dataExternalInterface->FMU(i).Instance(j).WorkingFolder_wLib.string().length();
                }

                {
                    // determine the FMI version
                    // preprocess args for library call
                    auto workingFolderWithLibArr(getCharArrayFromString(state.dataExternalInterface->FMU(i).Instance(j).WorkingFolder_wLib.string()));
                    auto VersionNumArr(
                        getCharArrayFromString("    ")); // the version should only be 3 characters long, since for now we only handle "1.0"

                    // make the library call
                    retValfmiVersion = getfmiEPlusVersion(&workingFolderWithLibArr[0],
                                                          &state.dataExternalInterface->FMU(i).Instance(j).LenWorkingFolder_wLib,
                                                          &VersionNumArr[0],
                                                          &state.dataExternalInterface->FMU(i).Instance(j).Index);

                    // post process in case args are used later
                    state.dataExternalInterface->FMU(i).Instance(j).fmiVersionNumber = getStringFromCharArray(VersionNumArr);

                    if (retValfmiVersion != 0) {
                        ShowSevereError(state, "ExternalInterface/InitExternalInterfaceFMUImport: Error when trying to");
                        ShowContinueError(state, "load FMI functions library of instance");
                        ShowContinueError(state,
                                          "\"" + state.dataExternalInterface->FMU(i).Instance(j).Name + "\" of FMU \"" +
                                              state.dataExternalInterface->FMU(i).Name + "\".");
                        ShowContinueError(state, "\"" + state.dataExternalInterface->FMU(i).Instance(j).fmiVersionNumber + "\".");
                        state.dataExternalInterface->ErrorsFound = true;
                        StopExternalInterfaceIfError(state);
                    }

                    if (state.dataExternalInterface->FMU(i).Instance(j).fmiVersionNumber.substr(0, 3) != "1.0") {
                        ShowSevereError(state, "ExternalInterface/InitExternalInterfaceFMUImport: Error when getting version");
                        ShowContinueError(state, "number of instance \"" + state.dataExternalInterface->FMU(i).Instance(j).Name + "\"");
                        ShowContinueError(state, "of FMU \"" + state.dataExternalInterface->FMU(i).Name + "\".");
                        ShowContinueError(state,
                                          "The version number found (\"" +
                                              state.dataExternalInterface->FMU(i).Instance(j).fmiVersionNumber.substr(0, 3) + "\")");
                        ShowContinueError(state, "differs from version 1.0 which is currently supported.");
                        state.dataExternalInterface->ErrorsFound = true;
                        StopExternalInterfaceIfError(state);
                    }
                }
            }
        }

        strippedFileName.deallocate();
        fullFileName.deallocate();

        state.dataExternalInterface->UniqueFMUInputVarNames.reserve(static_cast<unsigned>(NumFMUInputVariables));
        for (i = 1; i <= state.dataExternalInterface->NumFMUObjects; ++i) {
            for (j = 1; j <= state.dataExternalInterface->FMU(i).NumInstances; ++j) {
                state.dataExternalInterface->FMU(i).Instance(j).fmuInputVariable.allocate(NumFMUInputVariables);
                state.dataExternalInterface->FMU(i).Instance(j).checkfmuInputVariable.allocate(NumFMUInputVariables);
                state.dataExternalInterface->UniqueFMUInputVarNames.clear();
                state.dataExternalInterface->FMU(i).Instance(j).eplusOutputVariable.allocate(NumFMUInputVariables);
                k = 1;
                for (l = 1; l <= NumFMUInputVariables; ++l) {
                    state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                             cCurrentModuleObject,
                                                                             l,
                                                                             state.dataIPShortCut->cAlphaArgs,
                                                                             NumAlphas,
                                                                             state.dataIPShortCut->rNumericArgs,
                                                                             NumNumbers,
                                                                             IOStatus,
                                                                             _,
                                                                             _,
                                                                             state.dataIPShortCut->cAlphaFieldNames,
                                                                             state.dataIPShortCut->cNumericFieldNames);
                    if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(3), state.dataExternalInterface->FMU(i).Name) &&
                        UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(4), state.dataExternalInterface->FMU(i).Instance(j).Name)) {
                        state.dataExternalInterface->FMU(i).Instance(j).fmuInputVariable(k).Name = state.dataIPShortCut->cAlphaArgs(5);
                        state.dataExternalInterface->FMU(i).Instance(j).eplusOutputVariable(k).VarKey = state.dataIPShortCut->cAlphaArgs(1);
                        state.dataExternalInterface->FMU(i).Instance(j).eplusOutputVariable(k).Name = state.dataIPShortCut->cAlphaArgs(2);
                        // verify whether we have duplicate FMU input variables in the idf
                        GlobalNames::VerifyUniqueInterObjectName(state,
                                                                 state.dataExternalInterface->UniqueFMUInputVarNames,
                                                                 state.dataExternalInterface->FMU(i).Instance(j).fmuInputVariable(k).Name,
                                                                 cCurrentModuleObject,
                                                                 state.dataExternalInterface->FMU(i).Instance(j).Name,
                                                                 state.dataExternalInterface->ErrorsFound);
                        //                            UtilityRoutines::VerifyName( state.dataExternalInterface->FMU( i ).Instance( j
                        //                            ).fmuInputVariable(
                        // k
                        //).Name,  state.dataExternalInterface->FMU(
                        // i
                        //).Instance(
                        // j
                        //).checkfmuInputVariable, NumFMUInputVariables, IsNotOK, IsBlank, "The FMU input variable \"" +
                        // state.dataExternalInterface->FMU( i ).Instance( j
                        //).fmuInputVariable( k ).Name + "\" of instance \"" + state.dataExternalInterface->FMU( i ).Instance( j ).Name + "\" of FMU
                        //\"" + state.dataExternalInterface->FMU( i ).Name + "\"
                        // has duplicates. Please check the input file again and delete duplicated entries." );
                        if (state.dataExternalInterface->ErrorsFound) {
                            StopExternalInterfaceIfError(state);
                        } else {
                            state.dataExternalInterface->FMU(i).Instance(j).checkfmuInputVariable(k).Name =
                                state.dataExternalInterface->FMU(i).Instance(j).fmuInputVariable(k).Name;
                        }

                        // preprocess args for library call
                        auto inputVarNameArr(getCharArrayFromString(state.dataExternalInterface->FMU(i).Instance(j).fmuInputVariable(k).Name));
                        int inputVarNameLen(len(state.dataExternalInterface->FMU(i).Instance(j).fmuInputVariable(k).Name));

                        // make the library call
                        state.dataExternalInterface->FMU(i).Instance(j).fmuInputVariable(k).ValueReference = getValueReferenceByNameFMUInputVariables(
                            &inputVarNameArr[0], &inputVarNameLen, &state.dataExternalInterface->FMU(i).Instance(j).Index);

                        // postprocess args in case they are used later
                        state.dataExternalInterface->FMU(i).Instance(j).fmuInputVariable(k).Name = getStringFromCharArray(inputVarNameArr);

                        if (state.dataExternalInterface->FMU(i).Instance(j).fmuInputVariable(k).ValueReference == -999) {
                            ShowSevereError(state, "ExternalInterface/InitExternalInterfaceFMUImport: Error when trying to");
                            ShowContinueError(state, "get the value reference of FMU input variable");
                            ShowContinueError(state,
                                              "\"" + state.dataExternalInterface->FMU(i).Instance(j).fmuInputVariable(k).Name + "\" of instance \"" +
                                                  state.dataExternalInterface->FMU(i).Instance(j).Name + "\" of FMU");
                            ShowContinueError(state,
                                              "of FMU \"" + state.dataExternalInterface->FMU(i).Name + "\". Please check the name of input variable");
                            ShowContinueError(state, "in the input file and in the modelDescription file.");
                            state.dataExternalInterface->ErrorsFound = true;
                            StopExternalInterfaceIfError(state);
                        }

                        if (state.dataExternalInterface->FMU(i).Instance(j).fmuInputVariable(k).ValueReference == -1) {
                            ShowSevereError(state, "ExternalInterface/InitExternalInterfaceFMUImport: Error when trying to");
                            ShowContinueError(state, "get the value reference of FMU input variable");
                            ShowContinueError(state,
                                              "\"" + state.dataExternalInterface->FMU(i).Instance(j).fmuInputVariable(k).Name + "\" of instance \"" +
                                                  state.dataExternalInterface->FMU(i).Instance(j).Name + "\" of FMU");
                            ShowContinueError(state,
                                              "\"" + state.dataExternalInterface->FMU(i).Name + "\". This variable is not an FMU input variable.");
                            ShowContinueError(state, "Please check the causality of the variable in the modelDescription file.");
                            state.dataExternalInterface->ErrorsFound = true;
                            StopExternalInterfaceIfError(state);
                        }

                        // The next call expects an array, but a single item is passed
                        // Therefore create a single item array here first
                        Array1D_string tempSingleStringA(1, state.dataExternalInterface->FMU(i).Instance(j).eplusOutputVariable(k).VarKey);
                        Array1D_string tempSingleStringB(1, state.dataExternalInterface->FMU(i).Instance(j).eplusOutputVariable(k).Name);

                        // Make the call with arrays
                        GetReportVariableKey(state, tempSingleStringA, 1, tempSingleStringB, keyIndexes, varTypes);

                        // Then postprocess the array items back in case they changed
                        state.dataExternalInterface->FMU(i).Instance(j).eplusOutputVariable(k).VarKey = tempSingleStringA(1);
                        state.dataExternalInterface->FMU(i).Instance(j).eplusOutputVariable(k).Name = tempSingleStringB(1);

                        state.dataExternalInterface->FMU(i).Instance(j).eplusOutputVariable(k).VarIndex = keyIndexes(1);
                        state.dataExternalInterface->FMU(i).Instance(j).eplusOutputVariable(k).VarType = varTypes(1);
                        state.dataExternalInterface->FMU(i).Instance(j).NumInputVariablesInIDF = k;
                        ++k;
                    }
                }

                if (NumFMUInputVariables > 0 && state.dataExternalInterface->FMU(i).Instance(j).NumInputVariablesInIDF == 0) {
                    ShowWarningError(state,
                                     "InitExternalInterfaceFMUImport: The instance \"" + state.dataExternalInterface->FMU(i).Instance(j).Name +
                                         "\" of FMU \"" + state.dataExternalInterface->FMU(i).Name + "\"");
                    ShowContinueError(state, "is defined but has no input variables. Check the input field of the");
                    ShowContinueError(state, "corresponding object: ExternalInterface:FunctionalMockupUnitImport:From:Variable.");
                }
            }
        }

        for (i = 1; i <= state.dataExternalInterface->NumFMUObjects; ++i) {
            for (j = 1; j <= state.dataExternalInterface->FMU(i).NumInstances; ++j) {
                // check whether the number of input variables in fmu is bigger than in the idf
                if (state.dataExternalInterface->FMU(i).Instance(j).NumInputVariablesInFMU >
                    state.dataExternalInterface->FMU(i).Instance(j).NumInputVariablesInIDF) {
                    ShowWarningError(state,
                                     format("InitExternalInterfaceFMUImport: The number of input variables defined in input file ({})",
                                            state.dataExternalInterface->FMU(i).Instance(j).NumInputVariablesInIDF));
                    ShowContinueError(state,
                                      "of instance \"" + state.dataExternalInterface->FMU(i).Instance(j).Name + "\" of FMU \"" +
                                          state.dataExternalInterface->FMU(i).Name + "\" is less than the number of input variables");
                    ShowContinueError(
                        state, format("in the modelDescription file ({}).", state.dataExternalInterface->FMU(i).Instance(j).NumInputVariablesInFMU));
                    ShowContinueError(state, "Check the input file and the modelDescription file again.");
                }
                // check whether the number of input variables in fmu is less than in the idf
                if (state.dataExternalInterface->FMU(i).Instance(j).NumInputVariablesInFMU <
                    state.dataExternalInterface->FMU(i).Instance(j).NumInputVariablesInIDF) {
                    ShowWarningError(state,
                                     format("InitExternalInterfaceFMUImport: The number of input variables defined in input file ({})",
                                            state.dataExternalInterface->FMU(i).Instance(j).NumInputVariablesInIDF));
                    ShowContinueError(state,
                                      "of instance \"" + state.dataExternalInterface->FMU(i).Instance(j).Name + "\" of FMU \"" +
                                          state.dataExternalInterface->FMU(i).Name + "\" is bigger than the number of input variables");
                    ShowContinueError(
                        state, format("in the modelDescription file ({}).", state.dataExternalInterface->FMU(i).Instance(j).NumInputVariablesInFMU));
                    ShowContinueError(state, "Check the input file and the modelDescription file again.");
                }
            }
        }

        // get the names of the output variables each fmu (and the names of the
        // corresponding input variables in EnergyPlus -- schedule).
        cCurrentModuleObject = "ExternalInterface:FunctionalMockupUnitImport:To:Schedule";
        NumFMUInputVariables = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, cCurrentModuleObject);

        for (i = 1; i <= state.dataExternalInterface->NumFMUObjects; ++i) {
            j = 1;
            for (k = 1; k <= NumFMUInputVariables; ++k) {
                state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                         cCurrentModuleObject,
                                                                         k,
                                                                         state.dataIPShortCut->cAlphaArgs,
                                                                         NumAlphas,
                                                                         state.dataIPShortCut->rNumericArgs,
                                                                         NumNumbers,
                                                                         IOStatus,
                                                                         _,
                                                                         _,
                                                                         state.dataIPShortCut->cAlphaFieldNames,
                                                                         state.dataIPShortCut->cNumericFieldNames);
                if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(3), state.dataExternalInterface->FMU(i).Name)) {
                    state.dataExternalInterface->FMU(i).TotNumOutputVariablesSchedule = j;
                    ++j;
                }
            }
        }

        for (i = 1; i <= state.dataExternalInterface->NumFMUObjects; ++i) {
            for (j = 1; j <= state.dataExternalInterface->FMU(i).NumInstances; ++j) {
                state.dataExternalInterface->FMU(i).Instance(j).fmuOutputVariableSchedule.allocate(NumFMUInputVariables);
                state.dataExternalInterface->FMU(i).Instance(j).eplusInputVariableSchedule.allocate(NumFMUInputVariables);
                k = 1;
                for (l = 1; l <= NumFMUInputVariables; ++l) {
                    state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                             cCurrentModuleObject,
                                                                             l,
                                                                             state.dataIPShortCut->cAlphaArgs,
                                                                             NumAlphas,
                                                                             state.dataIPShortCut->rNumericArgs,
                                                                             NumNumbers,
                                                                             IOStatus,
                                                                             _,
                                                                             _,
                                                                             state.dataIPShortCut->cAlphaFieldNames,
                                                                             state.dataIPShortCut->cNumericFieldNames);
                    if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(3), state.dataExternalInterface->FMU(i).Name) &&
                        UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(4), state.dataExternalInterface->FMU(i).Instance(j).Name)) {
                        state.dataExternalInterface->FMU(i).Instance(j).fmuOutputVariableSchedule(k).Name = state.dataIPShortCut->cAlphaArgs(5);
                        state.dataExternalInterface->FMU(i).Instance(j).eplusInputVariableSchedule(k).Name = state.dataIPShortCut->cAlphaArgs(1);
                        state.dataExternalInterface->FMU(i).Instance(j).eplusInputVariableSchedule(k).InitialValue =
                            state.dataIPShortCut->rNumericArgs(1);

                        // get the value reference by using the FMU name and the variable name.

                        // preprocess the arguments before the following library call
                        auto NameCharArr(getCharArrayFromString(state.dataExternalInterface->FMU(i).Instance(j).fmuOutputVariableSchedule(k).Name));
                        int lengthVar(len(state.dataExternalInterface->FMU(i).Instance(j).fmuOutputVariableSchedule(k).Name));

                        // make the library call
                        state.dataExternalInterface->FMU(i).Instance(j).fmuOutputVariableSchedule(k).ValueReference =
                            getValueReferenceByNameFMUOutputVariables(
                                &NameCharArr[0], &lengthVar, &state.dataExternalInterface->FMU(i).Instance(j).Index);

                        // postprocess the arguments after the library call in case they are changed and used later
                        state.dataExternalInterface->FMU(i).Instance(j).fmuOutputVariableSchedule(k).Name = getStringFromCharArray(NameCharArr);

                        if (state.dataExternalInterface->FMU(i).Instance(j).fmuOutputVariableSchedule(k).ValueReference == -999) {
                            ShowSevereError(state,
                                            "ExternalInterface/InitExternalInterfaceFMUImport: Error when trying to get the value reference of "
                                            "the FMU output variable");
                            ShowContinueError(state,
                                              "\"" + state.dataExternalInterface->FMU(i).Instance(j).fmuOutputVariableSchedule(k).Name +
                                                  "\" of instance \"" + state.dataExternalInterface->FMU(i).Instance(j).Name + "\"");
                            ShowContinueError(state,
                                              "of FMU \"" + state.dataExternalInterface->FMU(i).Name + "\" that will be mapped to a schedule.");
                            ShowContinueError(state, "Please check the name of output variables in the input file and");
                            ShowContinueError(state, "in the modelDescription file.");
                            state.dataExternalInterface->ErrorsFound = true;
                            StopExternalInterfaceIfError(state);
                        }

                        if (state.dataExternalInterface->FMU(i).Instance(j).fmuOutputVariableSchedule(k).ValueReference == -1) {
                            ShowSevereError(state,
                                            "ExternalInterface/InitExternalInterfaceFMUImport: Error when trying to get the value reference of "
                                            "the FMU output variable");
                            ShowContinueError(state,
                                              "\"" + state.dataExternalInterface->FMU(i).Instance(j).fmuOutputVariableSchedule(k).Name +
                                                  "\" of instance \"" + state.dataExternalInterface->FMU(i).Instance(j).Name + "\"");
                            ShowContinueError(state,
                                              "of FMU \"" + state.dataExternalInterface->FMU(i).Name + "\" that will be mapped to a schedule.");
                            ShowContinueError(state, "This variable is not an FMU output variable.");
                            ShowContinueError(state, "Please check the causality of the variable in the modelDescription file.");
                            state.dataExternalInterface->ErrorsFound = true;
                            StopExternalInterfaceIfError(state);
                        }

                        state.dataExternalInterface->FMU(i).Instance(j).eplusInputVariableSchedule(k).VarIndex =
                            GetDayScheduleIndex(state, state.dataExternalInterface->FMU(i).Instance(j).eplusInputVariableSchedule(k).Name);
                        state.dataExternalInterface->FMU(i).Instance(j).NumOutputVariablesSchedule = k;
                        if (state.dataExternalInterface->FMU(i).Instance(j).eplusInputVariableSchedule(k).VarIndex <= 0) {
                            ShowSevereError(state,
                                            "ExternalInterface/InitExternalInterfaceFMUImport:declares variable \"" +
                                                state.dataExternalInterface->FMU(i).Instance(j).eplusInputVariableSchedule(k).Name + "\",");
                            ShowContinueError(state, "but variable is not a schedule variable.");
                            state.dataExternalInterface->ErrorsFound = true;
                            StopExternalInterfaceIfError(state);
                        }
                        ++k;
                    }
                }
            }
        }

        // get the names of the output variables each fmu (and the names of the
        // corresponding input variables in EnergyPlus -- variable).
        cCurrentModuleObject = "ExternalInterface:FunctionalMockupUnitImport:To:Variable";
        NumFMUInputVariables = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, cCurrentModuleObject);

        for (i = 1; i <= state.dataExternalInterface->NumFMUObjects; ++i) {
            j = 1;
            for (k = 1; k <= NumFMUInputVariables; ++k) {
                state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                         cCurrentModuleObject,
                                                                         k,
                                                                         state.dataIPShortCut->cAlphaArgs,
                                                                         NumAlphas,
                                                                         state.dataIPShortCut->rNumericArgs,
                                                                         NumNumbers,
                                                                         IOStatus,
                                                                         _,
                                                                         _,
                                                                         state.dataIPShortCut->cAlphaFieldNames,
                                                                         state.dataIPShortCut->cNumericFieldNames);
                if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(2), state.dataExternalInterface->FMU(i).Name)) {
                    state.dataExternalInterface->FMU(i).TotNumOutputVariablesVariable = j;
                    ++j;
                }
            }
        }

        for (i = 1; i <= state.dataExternalInterface->NumFMUObjects; ++i) {
            for (j = 1; j <= state.dataExternalInterface->FMU(i).NumInstances; ++j) {
                state.dataExternalInterface->FMU(i).Instance(j).fmuOutputVariableVariable.allocate(NumFMUInputVariables);
                state.dataExternalInterface->FMU(i).Instance(j).eplusInputVariableVariable.allocate(NumFMUInputVariables);
                k = 1;
                for (l = 1; l <= NumFMUInputVariables; ++l) {
                    state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                             cCurrentModuleObject,
                                                                             l,
                                                                             state.dataIPShortCut->cAlphaArgs,
                                                                             NumAlphas,
                                                                             state.dataIPShortCut->rNumericArgs,
                                                                             NumNumbers,
                                                                             IOStatus,
                                                                             _,
                                                                             _,
                                                                             state.dataIPShortCut->cAlphaFieldNames,
                                                                             state.dataIPShortCut->cNumericFieldNames);
                    if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(2), state.dataExternalInterface->FMU(i).Name) &&
                        UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(3), state.dataExternalInterface->FMU(i).Instance(j).Name)) {
                        state.dataExternalInterface->FMU(i).Instance(j).fmuOutputVariableVariable(k).Name = state.dataIPShortCut->cAlphaArgs(4);
                        state.dataExternalInterface->FMU(i).Instance(j).eplusInputVariableVariable(k).Name = state.dataIPShortCut->cAlphaArgs(1);

                        // get the value reference by using the FMU name and the variable name.
                        auto NameCharArr(getCharArrayFromString(state.dataExternalInterface->FMU(i).Instance(j).fmuOutputVariableVariable(k).Name));
                        int tempLength(len(state.dataExternalInterface->FMU(i).Instance(j).fmuOutputVariableVariable(k).Name));
                        state.dataExternalInterface->FMU(i).Instance(j).fmuOutputVariableVariable(k).ValueReference =
                            getValueReferenceByNameFMUOutputVariables(
                                &NameCharArr[0], &tempLength, &state.dataExternalInterface->FMU(i).Instance(j).Index);
                        // state.dataExternalInterface->FMU( i ).Instance( j ).fmuOutputVariableVariable( k ).Name = getStringFromCharArray(
                        // NameCharArr );

                        if (state.dataExternalInterface->FMU(i).Instance(j).fmuOutputVariableVariable(k).ValueReference == -999) {
                            ShowSevereError(state,
                                            "ExternalInterface/InitExternalInterfaceFMUImport: Error when trying to get the value reference of "
                                            "the FMU output variable");
                            ShowContinueError(state,
                                              "\"" + state.dataExternalInterface->FMU(i).Instance(j).fmuOutputVariableVariable(k).Name +
                                                  "\" of instance \"" + state.dataExternalInterface->FMU(i).Instance(j).Name + "\"");
                            ShowContinueError(state,
                                              "of FMU \"" + state.dataExternalInterface->FMU(i).Name + "\" that will be mapped to a variable.");
                            ShowContinueError(state, "Please check the name of output variables in the input file and in the modelDescription file.");
                            state.dataExternalInterface->ErrorsFound = true;
                            StopExternalInterfaceIfError(state);
                        }

                        if (state.dataExternalInterface->FMU(i).Instance(j).fmuOutputVariableVariable(k).ValueReference == -1) {
                            ShowSevereError(state,
                                            "ExternalInterface/InitExternalInterfaceFMUImport: Error when trying to get the value reference of "
                                            "the FMU output variable");
                            ShowContinueError(state,
                                              "\"" + state.dataExternalInterface->FMU(i).Instance(j).fmuOutputVariableVariable(k).Name +
                                                  "\" of instance \"" + state.dataExternalInterface->FMU(i).Instance(j).Name + "\"");
                            ShowContinueError(state,
                                              "of FMU \"" + state.dataExternalInterface->FMU(i).Name + "\" that will be mapped to a variable.");
                            ShowContinueError(state,
                                              "This variable is not an FMU output variable. Please check the causality of the variable in the "
                                              "modelDescription file.");
                            state.dataExternalInterface->ErrorsFound = true;
                            StopExternalInterfaceIfError(state);
                        }

                        state.dataExternalInterface->FMU(i).Instance(j).eplusInputVariableVariable(k).VarIndex =
                            FindEMSVariable(state, state.dataExternalInterface->FMU(i).Instance(j).eplusInputVariableVariable(k).Name, 0);
                        state.dataExternalInterface->FMU(i).Instance(j).NumOutputVariablesVariable = k;
                        if (state.dataExternalInterface->FMU(i).Instance(j).eplusInputVariableVariable(k).VarIndex <= 0) {
                            ShowSevereError(state,
                                            "ExternalInterface/InitExternalInterfaceFMUImport:declares variable \"" +
                                                state.dataExternalInterface->FMU(i).Instance(j).eplusInputVariableVariable(k).Name + "\",");
                            ShowContinueError(state, "but variable is not an EMS variable.");
                            state.dataExternalInterface->ErrorsFound = true;
                            StopExternalInterfaceIfError(state);
                        }
                        ++k;
                    }
                }
                if (state.dataExternalInterface->FMU(i).Instance(j).NumOutputVariablesVariable >= 1) {
                    state.dataExternalInterface->useEMS = true;
                }
            }
        }

        // get the names of the output variables each fmu (and the names of the
        // corresponding input variables in EnergyPlus -- actuator).
        cCurrentModuleObject = "ExternalInterface:FunctionalMockupUnitImport:To:Actuator";
        NumFMUInputVariables = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, cCurrentModuleObject);

        for (i = 1; i <= state.dataExternalInterface->NumFMUObjects; ++i) {
            j = 1;
            for (k = 1; k <= NumFMUInputVariables; ++k) {
                state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                         cCurrentModuleObject,
                                                                         k,
                                                                         state.dataIPShortCut->cAlphaArgs,
                                                                         NumAlphas,
                                                                         state.dataIPShortCut->rNumericArgs,
                                                                         NumNumbers,
                                                                         IOStatus,
                                                                         _,
                                                                         _,
                                                                         state.dataIPShortCut->cAlphaFieldNames,
                                                                         state.dataIPShortCut->cNumericFieldNames);
                if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(5), state.dataExternalInterface->FMU(i).Name)) {
                    state.dataExternalInterface->FMU(i).TotNumOutputVariablesActuator = j;
                    ++j;
                }
            }
        }

        for (i = 1; i <= state.dataExternalInterface->NumFMUObjects; ++i) {
            for (j = 1; j <= state.dataExternalInterface->FMU(i).NumInstances; ++j) {
                state.dataExternalInterface->FMU(i).Instance(j).fmuOutputVariableActuator.allocate(NumFMUInputVariables);
                state.dataExternalInterface->FMU(i).Instance(j).eplusInputVariableActuator.allocate(NumFMUInputVariables);
                k = 1;
                for (l = 1; l <= NumFMUInputVariables; ++l) {
                    state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                             cCurrentModuleObject,
                                                                             l,
                                                                             state.dataIPShortCut->cAlphaArgs,
                                                                             NumAlphas,
                                                                             state.dataIPShortCut->rNumericArgs,
                                                                             NumNumbers,
                                                                             IOStatus,
                                                                             _,
                                                                             _,
                                                                             state.dataIPShortCut->cAlphaFieldNames,
                                                                             state.dataIPShortCut->cNumericFieldNames);
                    if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(5), state.dataExternalInterface->FMU(i).Name) &&
                        UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(6), state.dataExternalInterface->FMU(i).Instance(j).Name)) {
                        state.dataExternalInterface->FMU(i).Instance(j).fmuOutputVariableActuator(k).Name = state.dataIPShortCut->cAlphaArgs(7);
                        state.dataExternalInterface->FMU(i).Instance(j).eplusInputVariableActuator(k).Name = state.dataIPShortCut->cAlphaArgs(1);

                        // get the value reference by using the FMU name and the variable name.
                        auto tempNameArr(getCharArrayFromString(state.dataExternalInterface->FMU(i).Instance(j).fmuOutputVariableActuator(k).Name));
                        int tempLength(len(state.dataExternalInterface->FMU(i).Instance(j).fmuOutputVariableActuator(k).Name));
                        state.dataExternalInterface->FMU(i).Instance(j).fmuOutputVariableActuator(k).ValueReference =
                            getValueReferenceByNameFMUOutputVariables(
                                &tempNameArr[0], &tempLength, &state.dataExternalInterface->FMU(i).Instance(j).Index);
                        // state.dataExternalInterface->FMU( i ).Instance( j ).fmuOutputVariableActuator( k ).Name = getStringFromCharArray(
                        // tempNameArr );

                        if (state.dataExternalInterface->FMU(i).Instance(j).fmuOutputVariableActuator(k).ValueReference == -999) {
                            ShowSevereError(state,
                                            "ExternalInterface/InitExternalInterfaceFMUImport: Error when trying to get the value reference of "
                                            "the FMU output variable");
                            ShowContinueError(state,
                                              "\"" + state.dataExternalInterface->FMU(i).Instance(j).fmuOutputVariableActuator(k).Name +
                                                  "\" of instance \"" + state.dataExternalInterface->FMU(i).Instance(j).Name + "\"");
                            ShowContinueError(state,
                                              "of FMU \"" + state.dataExternalInterface->FMU(i).Name + "\" that will be mapped to an actuator.");
                            ShowContinueError(state, "Please check the name of output variables in the input file and in the modelDescription file.");
                            state.dataExternalInterface->ErrorsFound = true;
                            StopExternalInterfaceIfError(state);
                        }

                        if (state.dataExternalInterface->FMU(i).Instance(j).fmuOutputVariableActuator(k).ValueReference == -1) {
                            ShowSevereError(state,
                                            "ExternalInterface/InitExternalInterfaceFMUImport: Error when trying to get the value reference of "
                                            "the FMU output variable");
                            ShowContinueError(state,
                                              "\"" + state.dataExternalInterface->FMU(i).Instance(j).fmuOutputVariableActuator(k).Name +
                                                  "\" of instance \"" + state.dataExternalInterface->FMU(i).Instance(j).Name + "\"");
                            ShowContinueError(state,
                                              "of FMU \"" + state.dataExternalInterface->FMU(i).Name + "\" that will be mapped to an actuator.");
                            ShowContinueError(state,
                                              "This variable is not an FMU output variable. Please check the causality of the variable in the "
                                              "modelDescription file.");
                            state.dataExternalInterface->ErrorsFound = true;
                            StopExternalInterfaceIfError(state);
                        }

                        state.dataExternalInterface->FMU(i).Instance(j).eplusInputVariableActuator(k).VarIndex =
                            FindEMSVariable(state, state.dataExternalInterface->FMU(i).Instance(j).eplusInputVariableActuator(k).Name, 0);
                        state.dataExternalInterface->FMU(i).Instance(j).NumOutputVariablesActuator = k;
                        if (state.dataExternalInterface->FMU(i).Instance(j).eplusInputVariableActuator(k).VarIndex <= 0) {
                            ShowSevereError(state,
                                            "ExternalInterface/InitExternalInterfaceFMUImport:declares variable \"" +
                                                state.dataExternalInterface->FMU(i).Instance(j).eplusInputVariableActuator(k).Name + "\",");
                            ShowContinueError(state, "but variable is not an EMS variable.");
                            state.dataExternalInterface->ErrorsFound = true;
                            StopExternalInterfaceIfError(state);
                        }
                        ++k;
                    }
                }
                // set the flag state.dataExternalInterface->useEMS to true. This will be used then to update the erl variables in erl data structure
                if (state.dataExternalInterface->FMU(i).Instance(j).NumOutputVariablesActuator >= 1) {
                    state.dataExternalInterface->useEMS = true;
                }
            }
        }

        // parse the fmu defined in the idf using the fmuUnpack with the flag --unpack.
        for (i = 1; i <= state.dataExternalInterface->NumFMUObjects; ++i) {
            for (j = 1; j <= state.dataExternalInterface->FMU(i).NumInstances; ++j) {
                state.dataExternalInterface->FMU(i).Instance(j).NumOutputVariablesInIDF =
                    state.dataExternalInterface->FMU(i).Instance(j).NumOutputVariablesSchedule +
                    state.dataExternalInterface->FMU(i).Instance(j).NumOutputVariablesVariable +
                    state.dataExternalInterface->FMU(i).Instance(j).NumOutputVariablesActuator;
                // check whether the number of output variables in fmu is bigger than in the idf
                if (state.dataExternalInterface->FMU(i).Instance(j).NumOutputVariablesInFMU >
                    state.dataExternalInterface->FMU(i).Instance(j).NumOutputVariablesInIDF) {
                    ShowWarningError(state,
                                     format("InitExternalInterfaceFMUImport: The number of output variables defined in input file ({})",
                                            state.dataExternalInterface->FMU(i).Instance(j).NumOutputVariablesInIDF));
                    ShowContinueError(state,
                                      "of instance \"" + state.dataExternalInterface->FMU(i).Instance(j).Name + "\" of FMU \"" +
                                          state.dataExternalInterface->FMU(i).Name + "\" is less than the number of output variables");
                    ShowContinueError(
                        state, format("in the modelDescription file ({}).", state.dataExternalInterface->FMU(i).Instance(j).NumOutputVariablesInFMU));
                    ShowContinueError(state, "Check the input file and the modelDescription file again.");
                }
                // check whether the number of output variables in fmu is less than in the idf
                if (state.dataExternalInterface->FMU(i).Instance(j).NumOutputVariablesInFMU <
                    state.dataExternalInterface->FMU(i).Instance(j).NumOutputVariablesInIDF) {
                    ShowWarningError(state,
                                     format("InitExternalInterfaceFMUImport: The number of output variables defined in input file ({})",
                                            state.dataExternalInterface->FMU(i).Instance(j).NumOutputVariablesInIDF));
                    ShowContinueError(state,
                                      "of instance \"" + state.dataExternalInterface->FMU(i).Instance(j).Name + "\" of FMU \"" +
                                          state.dataExternalInterface->FMU(i).Name + "\" is bigger than the number of output variables");
                    ShowContinueError(
                        state, format("in the modelDescription file ({}).", state.dataExternalInterface->FMU(i).Instance(j).NumOutputVariablesInFMU));
                    ShowContinueError(state, "Check the input file and the modelDescription file again.");
                }

                DisplayString(state,
                              format("Number of inputs in instance \"{}\" of FMU \"{}\" = \"{}\".",
                                     state.dataExternalInterface->FMU(i).Instance(j).Name,
                                     state.dataExternalInterface->FMU(i).Name,
                                     state.dataExternalInterface->FMU(i).Instance(j).NumInputVariablesInIDF));
                DisplayString(state,
                              format("Number of outputs in instance \"{}\" of FMU \"{}\" = \"{}\".",
                                     state.dataExternalInterface->FMU(i).Instance(j).Name,
                                     state.dataExternalInterface->FMU(i).Name,
                                     state.dataExternalInterface->FMU(i).Instance(j).NumOutputVariablesInIDF));
            }
        }
        StopExternalInterfaceIfError(state);
        state.dataExternalInterface->FirstCallIni = false;
    }
}

std::string trim(std::string const &str)
{
    std::size_t first = str.find_first_not_of(' ');
    std::size_t last = str.find_last_not_of(' ');
    return str.substr(first, last - first + 1);
}

Real64 GetCurSimStartTimeSeconds(EnergyPlusData &state)
{
    // FUNCTION INFORMATION:
    //       AUTHOR         Thierry S. Nouidui, Michael Wetter, Wangda Zuo
    //       DATE WRITTEN   August 2011
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS FUNCTION:
    //  Get the current month and day in the runperiod and convert
    //  it into seconds.

    // Locals
    Real64 simtime;

    if (!state.dataEnvrn->CurrentYearIsLeapYear) {
        switch (state.dataEnvrn->Month) {
        case 1:
            simtime = 0;
            break;
        case 2:
            simtime = 31;
            break;
        case 3:
            simtime = 59;
            break;
        case 4:
            simtime = 90;
            break;
        case 5:
            simtime = 120;
            break;
        case 6:
            simtime = 151;
            break;
        case 7:
            simtime = 181;
            break;
        case 8:
            simtime = 212;
            break;
        case 9:
            simtime = 243;
            break;
        case 10:
            simtime = 273;
            break;
        case 11:
            simtime = 304;
            break;
        case 12:
            simtime = 334;
            break;
        default:
            simtime = 0;
        }
    } else {
        switch (state.dataEnvrn->Month) {
        case 1:
            simtime = 0;
            break;
        case 2:
            simtime = 31;
            break;
        case 3:
            simtime = 59 + 1;
            break;
        case 4:
            simtime = 90 + 1;
            break;
        case 5:
            simtime = 120 + 1;
            break;
        case 6:
            simtime = 151 + 1;
            break;
        case 7:
            simtime = 181 + 1;
            break;
        case 8:
            simtime = 212 + 1;
            break;
        case 9:
            simtime = 243 + 1;
            break;
        case 10:
            simtime = 273 + 1;
            break;
        case 11:
            simtime = 304 + 1;
            break;
        case 12:
            simtime = 334 + 1;
            break;
        default:
            simtime = 0;
        }
    }

    simtime = 24 * (simtime + (state.dataEnvrn->DayOfMonth - 1)); // day of month does not need to be stubtracted??
    simtime = 60 * (simtime + (state.dataGlobal->HourOfDay - 1)); // hours to minutes
    simtime = 60 * (simtime);                                     // minutes to seconds

    return simtime;
}

void CalcExternalInterfaceFMUImport(EnergyPlusData &state)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Thierry S. Nouidui, Michael Wetter, Wangda Zuo
    //       DATE WRITTEN   08Aug2011
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine organizes the data exchange between FMU and EnergyPlus.

    // Using/Aliasing
    using EMSManager::ManageEMS;

    using RuntimeLanguageProcessor::ExternalInterfaceSetErlVariable;
    using RuntimeLanguageProcessor::FindEMSVariable;
    using RuntimeLanguageProcessor::isExternalInterfaceErlVariable;
    using ScheduleManager::ExternalInterfaceSetSchedule;
    using ScheduleManager::GetDayScheduleIndex;

    // SUBROUTINE PARAMETER DEFINITIONS:

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int i, j, k; // Loop counter

    Array1D_string Alphas(5);
    Array1D_int keyIndexes(1);     // Array index for
    Array1D_string NamesOfKeys(1); // Specific key name

    if (state.dataGlobal->WarmupFlag &&
        (state.dataGlobal->KindOfSim != DataGlobalConstants::KindOfSim::RunPeriodWeather)) { // No data exchange during design days
        if (state.dataExternalInterface->FirstCallDesignDays) {
            ShowWarningError(state, "ExternalInterface/CalcExternalInterfaceFMUImport: ExternalInterface does not exchange data during design days.");
        }
        state.dataExternalInterface->FirstCallDesignDays = false;
    }
    if (state.dataGlobal->WarmupFlag &&
        (state.dataGlobal->KindOfSim == DataGlobalConstants::KindOfSim::RunPeriodWeather)) { // Data exchange after design days
        if (state.dataExternalInterface->FirstCallWUp) {
            // set the report during warmup to true so that variables are also updated during the warmup
            // UpdateDataDuringWarmupExternalInterface = true;
            state.dataExternalInterface->hStep = (60.0 * state.dataGlobal->TimeStepZone) * 60.0;
            state.dataExternalInterface->tStart = GetCurSimStartTimeSeconds(state);
            state.dataExternalInterface->tStop = state.dataExternalInterface->tStart + 24.0 * 3600.0;
            state.dataExternalInterface->tComm = state.dataExternalInterface->tStart;

            // instantiate and initialize the unpack fmus
            InstantiateInitializeFMUImport(state);

            // allocate memory for a temporary FMU that will be used at the end of the warmup
            state.dataExternalInterface->FMUTemp.allocate(state.dataExternalInterface->NumFMUObjects);
            for (i = 1; i <= state.dataExternalInterface->NumFMUObjects; ++i) {
                state.dataExternalInterface->FMUTemp(i).Instance.allocate(state.dataExternalInterface->FMU(i).NumInstances);
            }
            for (i = 1; i <= state.dataExternalInterface->NumFMUObjects; ++i) {
                for (j = 1; j <= state.dataExternalInterface->FMU(i).NumInstances; ++j) {
                    state.dataExternalInterface->FMUTemp(i).Instance(j).fmuInputVariable.allocate(
                        state.dataExternalInterface->FMU(i).Instance(j).NumInputVariablesInIDF);
                    state.dataExternalInterface->FMUTemp(i).Instance(j).eplusOutputVariable.allocate(
                        state.dataExternalInterface->FMU(i).Instance(j).NumInputVariablesInIDF);
                    state.dataExternalInterface->FMUTemp(i).Instance(j).fmuOutputVariableSchedule.allocate(
                        state.dataExternalInterface->FMU(i).Instance(j).NumOutputVariablesSchedule);
                    state.dataExternalInterface->FMUTemp(i).Instance(j).fmuOutputVariableVariable.allocate(
                        state.dataExternalInterface->FMU(i).Instance(j).NumOutputVariablesVariable);
                    state.dataExternalInterface->FMUTemp(i).Instance(j).fmuOutputVariableActuator.allocate(
                        state.dataExternalInterface->FMU(i).Instance(j).NumOutputVariablesActuator);
                }
            }

            GetSetVariablesAndDoStepFMUImport(state);
            state.dataExternalInterface->tComm += state.dataExternalInterface->hStep;
            state.dataExternalInterface->FirstCallWUp = false;

        } else {
            if (state.dataExternalInterface->tComm < state.dataExternalInterface->tStop) {
                GetSetVariablesAndDoStepFMUImport(state);
                // Advance the communication time step
                state.dataExternalInterface->tComm += state.dataExternalInterface->hStep;
            } else {
                for (i = 1; i <= state.dataExternalInterface->NumFMUObjects; ++i) {
                    for (j = 1; j <= state.dataExternalInterface->FMU(i).NumInstances; ++j) {

                        state.dataExternalInterface->FMUTemp(i).Instance(j).NumInputVariablesInIDF =
                            state.dataExternalInterface->FMU(i).Instance(j).NumInputVariablesInIDF;
                        for (k = 1; k <= state.dataExternalInterface->FMU(i).Instance(j).NumInputVariablesInIDF; ++k) {
                            state.dataExternalInterface->FMUTemp(i).Instance(j).fmuInputVariable(k).ValueReference =
                                state.dataExternalInterface->FMU(i).Instance(j).fmuInputVariable(k).ValueReference;
                            state.dataExternalInterface->FMUTemp(i).Instance(j).eplusOutputVariable(k).RTSValue =
                                state.dataExternalInterface->FMU(i).Instance(j).eplusOutputVariable(k).RTSValue;
                            state.dataExternalInterface->FMUTemp(i).Instance(j).eplusOutputVariable(k).ITSValue =
                                state.dataExternalInterface->FMU(i).Instance(j).eplusOutputVariable(k).ITSValue;
                            state.dataExternalInterface->FMUTemp(i).Instance(j).eplusOutputVariable(k).VarType =
                                state.dataExternalInterface->FMU(i).Instance(j).eplusOutputVariable(k).VarType;
                        }

                        // save values that will be set in EnergyPlus (Schedule)
                        state.dataExternalInterface->FMUTemp(i).Instance(j).NumOutputVariablesSchedule =
                            state.dataExternalInterface->FMU(i).Instance(j).NumOutputVariablesSchedule;
                        for (k = 1; k <= state.dataExternalInterface->FMU(i).Instance(j).NumOutputVariablesSchedule; ++k) {
                            state.dataExternalInterface->FMUTemp(i).Instance(j).fmuOutputVariableSchedule(k).RealVarValue =
                                state.dataExternalInterface->FMU(i).Instance(j).fmuOutputVariableSchedule(k).RealVarValue;
                        }

                        // save values that will be set in EnergyPlus (Variable)
                        state.dataExternalInterface->FMUTemp(i).Instance(j).NumOutputVariablesVariable =
                            state.dataExternalInterface->FMU(i).Instance(j).NumOutputVariablesVariable;
                        for (k = 1; k <= state.dataExternalInterface->FMU(i).Instance(j).NumOutputVariablesVariable; ++k) {
                            state.dataExternalInterface->FMUTemp(i).Instance(j).fmuOutputVariableVariable(k).RealVarValue =
                                state.dataExternalInterface->FMU(i).Instance(j).fmuOutputVariableVariable(k).RealVarValue;
                        }

                        // save values that will be set in EnergyPlus (Actuator)
                        state.dataExternalInterface->FMUTemp(i).Instance(j).NumOutputVariablesActuator =
                            state.dataExternalInterface->FMU(i).Instance(j).NumOutputVariablesActuator;
                        for (k = 1; k <= state.dataExternalInterface->FMU(i).Instance(j).NumOutputVariablesActuator; ++k) {
                            state.dataExternalInterface->FMUTemp(i).Instance(j).fmuOutputVariableActuator(k).RealVarValue =
                                state.dataExternalInterface->FMU(i).Instance(j).fmuOutputVariableActuator(k).RealVarValue;
                        }
                    }
                }

                StopExternalInterfaceIfError(state);

                // Terminate all FMUs
                TerminateResetFreeFMUImport(state, state.dataExternalInterface->fmiEndSimulation);

                // Reset the communication time step
                state.dataExternalInterface->tComm = state.dataExternalInterface->tStart;

                // Reinstantiate and reinitialize the FMUs
                InstantiateInitializeFMUImport(state);

                // Set the values that have been saved in the FMUs-- saveFMUStateVariables ()
                for (i = 1; i <= state.dataExternalInterface->NumFMUObjects; ++i) {
                    for (j = 1; j <= state.dataExternalInterface->FMU(i).NumInstances; ++j) {

                        std::vector<unsigned int> valRefVec;
                        for (unsigned long x = 1; x <= size(state.dataExternalInterface->FMU(i).Instance(j).fmuInputVariable); ++x) {
                            valRefVec.push_back(state.dataExternalInterface->FMU(i).Instance(j).fmuInputVariable(x).ValueReference);
                        }

                        std::vector<Real64> rtsValVec;
                        for (unsigned long x = 1; x <= size(state.dataExternalInterface->FMU(i).Instance(j).eplusOutputVariable); ++x) {
                            rtsValVec.push_back(state.dataExternalInterface->FMU(i).Instance(j).eplusOutputVariable(x).RTSValue);
                        }

                        // make the library call
                        state.dataExternalInterface->FMU(i).Instance(j).fmistatus =
                            fmiEPlusSetReal(&state.dataExternalInterface->FMU(i).Instance(j).fmicomponent,
                                            &valRefVec[0],
                                            &rtsValVec[0],
                                            &state.dataExternalInterface->FMUTemp(i).Instance(j).NumInputVariablesInIDF,
                                            &state.dataExternalInterface->FMU(i).Instance(j).Index);

                        if (state.dataExternalInterface->FMU(i).Instance(j).fmistatus != fmiOK) {
                            ShowSevereError(
                                state,
                                "ExternalInterface/CalcExternalInterfaceFMUImport: Error when trying to set an input value in instance \"" +
                                    state.dataExternalInterface->FMU(i).Instance(j).Name + "\"");
                            ShowContinueError(state,
                                              format("of FMU \"{}\"; Error Code = \"{}\"",
                                                     state.dataExternalInterface->FMU(i).Name,
                                                     state.dataExternalInterface->FMU(i).Instance(j).fmistatus));
                            state.dataExternalInterface->ErrorsFound = true;
                            StopExternalInterfaceIfError(state);
                        }
                    }
                }
                // set the flag to reinitialize states to be true
                state.dataExternalInterface->FlagReIni = true;
                GetSetVariablesAndDoStepFMUImport(state);
                state.dataExternalInterface->FlagReIni = false;
                // advance one time step ahead for the next calculation
                state.dataExternalInterface->tComm += state.dataExternalInterface->hStep;
            }
        }
    }
    // BeginSimulation
    if (!state.dataGlobal->WarmupFlag && (state.dataGlobal->KindOfSim == DataGlobalConstants::KindOfSim::RunPeriodWeather)) {

        if (state.dataExternalInterface->FirstCallTStep) {
            // reset the UpdateDataDuringWarmupExternalInterface to be false.
            state.dataSysVars->UpdateDataDuringWarmupExternalInterface = false;
            // The time is computed in seconds for FMU
            state.dataExternalInterface->tStart = GetCurSimStartTimeSeconds(state);
            state.dataExternalInterface->tStop =
                state.dataExternalInterface->tStart + (state.dataEnvrn->TotalOverallSimDays - state.dataEnvrn->TotDesDays) * 24.0 * 3600.0;
            state.dataExternalInterface->tComm = state.dataExternalInterface->tStart;

            // Terminate all FMUs
            TerminateResetFreeFMUImport(state, state.dataExternalInterface->fmiEndSimulation);

            // Reinstantiate and reinitialize the FMUs
            InstantiateInitializeFMUImport(state);

            // Set the values that have been saved in the FMUs-- saveFMUStateVariables ()
            for (i = 1; i <= state.dataExternalInterface->NumFMUObjects; ++i) {
                for (j = 1; j <= state.dataExternalInterface->FMU(i).NumInstances; ++j) {

                    // make vectors first
                    std::vector<unsigned int> valRefVec;
                    for (unsigned long x = 1; x <= size(state.dataExternalInterface->FMUTemp(i).Instance(j).fmuInputVariable); ++x) {
                        valRefVec.push_back(state.dataExternalInterface->FMUTemp(i).Instance(j).fmuInputVariable(x).ValueReference);
                    }
                    std::vector<Real64> rtsValVec;
                    for (unsigned long x = 1; x <= size(state.dataExternalInterface->FMUTemp(i).Instance(j).eplusOutputVariable); ++x) {
                        rtsValVec.push_back(state.dataExternalInterface->FMUTemp(i).Instance(j).eplusOutputVariable(x).RTSValue);
                    }

                    // make the library call
                    state.dataExternalInterface->FMU(i).Instance(j).fmistatus =
                        fmiEPlusSetReal(&state.dataExternalInterface->FMU(i).Instance(j).fmicomponent,
                                        &valRefVec[0],
                                        &rtsValVec[0],
                                        &state.dataExternalInterface->FMUTemp(i).Instance(j).NumInputVariablesInIDF,
                                        &state.dataExternalInterface->FMU(i).Instance(j).Index);

                    if (state.dataExternalInterface->FMU(i).Instance(j).fmistatus != fmiOK) {
                        ShowSevereError(state, "ExternalInterface/CalcExternalInterfaceFMUImport: ");
                        ShowContinueError(state, "Error when trying to set inputs in instance");
                        ShowContinueError(state,
                                          "\"" + state.dataExternalInterface->FMU(i).Instance(j).Name + "\" of FMU \"" +
                                              state.dataExternalInterface->FMU(i).Name + "\"");
                        ShowContinueError(state, format("Error Code = \"{}\"", state.dataExternalInterface->FMU(i).Instance(j).fmistatus));
                        state.dataExternalInterface->ErrorsFound = true;
                        StopExternalInterfaceIfError(state);
                    }
                }
            }
            // set the flag to reinitialize states to be true
            state.dataExternalInterface->FlagReIni = true;
            GetSetVariablesAndDoStepFMUImport(state);
            state.dataExternalInterface->FlagReIni = false;
            // advance one time step ahead for the next calculation
            state.dataExternalInterface->tComm += state.dataExternalInterface->hStep;
            state.dataExternalInterface->FirstCallTStep = false;
        } else {
            if (state.dataExternalInterface->tComm != state.dataExternalInterface->tStop) {
                GetSetVariablesAndDoStepFMUImport(state);
                state.dataExternalInterface->tComm += state.dataExternalInterface->hStep;
            } else {
                // Terminate reset and free Slaves
                state.dataExternalInterface->fmiEndSimulation = 1;
                TerminateResetFreeFMUImport(state, state.dataExternalInterface->fmiEndSimulation);
                for (i = 1; i <= state.dataExternalInterface->NumFMUObjects; ++i) {
                    for (j = 1; j <= state.dataExternalInterface->FMU(i).NumInstances; ++j) {
                        // Deallocate used objects
                        state.dataExternalInterface->FMUTemp(i).Instance(j).fmuInputVariable.deallocate();
                        state.dataExternalInterface->FMUTemp(i).Instance(j).eplusOutputVariable.deallocate();
                        state.dataExternalInterface->FMUTemp(i).Instance(j).fmuOutputVariableSchedule.deallocate();
                        state.dataExternalInterface->FMUTemp(i).Instance(j).fmuOutputVariableVariable.deallocate();
                        state.dataExternalInterface->FMUTemp(i).Instance(j).fmuOutputVariableActuator.deallocate();
                    }
                }

                for (i = 1; i <= state.dataExternalInterface->NumFMUObjects; ++i) {
                    state.dataExternalInterface->FMUTemp(i).Instance.deallocate();
                }

                state.dataExternalInterface->FMUTemp.deallocate();

                for (i = 1; i <= state.dataExternalInterface->NumFMUObjects; ++i) {
                    for (j = 1; j <= state.dataExternalInterface->FMU(i).NumInstances; ++j) {
                        state.dataExternalInterface->FMU(i).Instance(j).eplusInputVariableSchedule.deallocate();
                        state.dataExternalInterface->FMU(i).Instance(j).fmuOutputVariableSchedule.deallocate();
                        state.dataExternalInterface->FMU(i).Instance(j).eplusInputVariableVariable.deallocate();
                        state.dataExternalInterface->FMU(i).Instance(j).fmuOutputVariableVariable.deallocate();
                        state.dataExternalInterface->FMU(i).Instance(j).eplusInputVariableActuator.deallocate();
                        state.dataExternalInterface->FMU(i).Instance(j).fmuOutputVariableActuator.deallocate();
                        state.dataExternalInterface->FMU(i).Instance(j).fmuInputVariable.deallocate();
                        state.dataExternalInterface->FMU(i).Instance(j).checkfmuInputVariable.deallocate();
                    }
                }

                for (i = 1; i <= state.dataExternalInterface->NumFMUObjects; ++i) {
                    state.dataExternalInterface->FMU(i).Instance.deallocate();
                }
                state.dataExternalInterface->FMU.deallocate();
            }
        }
    }
}

void ValidateRunControl(EnergyPlusData &state)
{
    // SUBROUTINE INFORMATION:
    //       AUTHOR         Michael Wetter
    //       DATE WRITTEN   December 2009
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine ensures that the RunControl object is valid.

    // METHODOLOGY EMPLOYED:
    // Use GetObjectItem from the Input Processor

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int NumAlphas(0);  // Number of Alphas for each GetObjectItem call
    int NumNumbers(0); // Number of Numbers for each GetObjectItem call
    int IOStatus(0);   // Used in GetObjectItem
    auto &cCurrentModuleObject = state.dataIPShortCut->cCurrentModuleObject;

    cCurrentModuleObject = "SimulationControl";
    int const NumRunControl = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, cCurrentModuleObject);
    if (NumRunControl > 0) {
        state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                 cCurrentModuleObject,
                                                                 1,
                                                                 state.dataIPShortCut->cAlphaArgs,
                                                                 NumAlphas,
                                                                 state.dataIPShortCut->rNumericArgs,
                                                                 NumNumbers,
                                                                 IOStatus,
                                                                 _,
                                                                 _,
                                                                 state.dataIPShortCut->cAlphaFieldNames,
                                                                 state.dataIPShortCut->cNumericFieldNames);
        if (state.dataIPShortCut->cAlphaArgs(5) == "NO") { // This run does not have a weather file simulation.
            ShowSevereError(state, "ExternalInterface: Error in idf file, section SimulationControl:");
            ShowContinueError(state, "When using the ExternalInterface, a run period from the weather file must be specified");
            ShowContinueError(state, "in the idf file, because the ExternalInterface interface is not active during");
            ShowContinueError(state, "warm-up and during sizing.");
            state.dataExternalInterface->ErrorsFound = true;
        }
    }
}

void CalcExternalInterface(EnergyPlusData &state)
{
    // SUBROUTINE INFORMATION:
    //       AUTHOR         Michael Wetter
    //       DATE WRITTEN   2Dec2007
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // Using/Aliasing
    using EMSManager::ManageEMS;

    using RuntimeLanguageProcessor::ExternalInterfaceSetErlVariable;
    using ScheduleManager::ExternalInterfaceSetSchedule;
    // using DataPrecisionGlobals;

    // SUBROUTINE PARAMETER DEFINITIONS:
    int constexpr nDblMax(1024); // Maximum number of doubles

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int i;      // Loop counter
    int retVal; // Return value from socket

    int flaWri;       // flag to write to the socket
    int flaRea;       // flag read from the socket
    int nDblWri;      // number of doubles to write to socket
    int nDblRea;      // number of doubles to read from socket
    Real64 curSimTim; // current simulation time
    Real64 preSimTim; // previous time step's simulation time

    Array1D<Real64> dblValWri(nDblMax);
    Array1D<Real64> dblValRea(nDblMax);
    bool continueSimulation; // Flag, true if simulation should continue

    if (state.dataExternalInterface->firstCall) {
        DisplayString(state, "ExternalInterface starts first data exchange.");
        state.dataExternalInterface->simulationStatus = 2;
        preSimTim = 0; // In the first call, E+ did not reset SimTimeSteps to zero
    } else {
        preSimTim = state.dataGlobal->SimTimeSteps * state.dataGlobal->MinutesPerTimeStep * 60.0;
    }

    // Socket asked to terminate simulation, but simulation continues
    if (state.dataExternalInterface->noMoreValues && state.dataExternalInterface->showContinuationWithoutUpdate) {
        if (state.dataExternalInterface->haveExternalInterfaceBCVTB) {
            ShowWarningError(
                state, format("ExternalInterface: Continue simulation without updated values from server at t ={:.2T} hours", preSimTim / 3600.0));
        }
        state.dataExternalInterface->showContinuationWithoutUpdate = false;
    }

    // Usual branch, control is configured and simulation should continue
    if (state.dataExternalInterface->configuredControlPoints && (!state.dataExternalInterface->noMoreValues)) {
        // Data to be exchanged
        nDblWri = size(state.dataExternalInterface->varTypes);
        nDblRea = 0;
        flaWri = 0;

        // Get EnergyPlus variables
        if (state.dataExternalInterface->firstCall) { // bug fix causing external interface to send zero at the beginning of sim, Thierry Nouidui
            for (i = 1; i <= nDblWri; ++i) {
                dblValWri(i) =
                    GetInternalVariableValue(state, state.dataExternalInterface->varTypes(i), state.dataExternalInterface->keyVarIndexes(i));
            }
        } else {
            for (i = 1; i <= nDblWri; ++i) {
                dblValWri(i) = GetInternalVariableValueExternalInterface(
                    state, state.dataExternalInterface->varTypes(i), state.dataExternalInterface->keyVarIndexes(i));
            }
        }

        // Exchange data with socket
        retVal = 0;
        flaRea = 0;
        if (state.dataExternalInterface->haveExternalInterfaceBCVTB) {
            retVal = exchangedoubleswithsocket(&state.dataExternalInterface->socketFD,
                                               &flaWri,
                                               &flaRea,
                                               &nDblWri,
                                               &nDblRea,
                                               &preSimTim,
                                               dblValWri.data(),
                                               &curSimTim,
                                               dblValRea.data());
        } else if (state.dataExternalInterface->haveExternalInterfaceFMUExport) {
            retVal = exchangedoubleswithsocketFMU(&state.dataExternalInterface->socketFD,
                                                  &flaWri,
                                                  &flaRea,
                                                  &nDblWri,
                                                  &nDblRea,
                                                  &preSimTim,
                                                  dblValWri.data(),
                                                  &curSimTim,
                                                  dblValRea.data(),
                                                  &state.dataExternalInterface->FMUExportActivate);
        }
        continueSimulation = true;

        // Check for errors, in which case we terminate the simulation loop
        // Added a check since the FMUExport is terminated with the flaRea set to 1.
        if (state.dataExternalInterface->haveExternalInterfaceBCVTB ||
            (state.dataExternalInterface->haveExternalInterfaceFMUExport && (flaRea == 0))) {
            if (retVal != 0) {
                continueSimulation = false;
                ShowSevereError(state,
                                format("ExternalInterface: Socket communication received error value \"{:2}\" at time = {:.2T} hours.",
                                       retVal,
                                       preSimTim / 3600));
                ShowContinueError(state, format("ExternalInterface: Flag from server \"{:2}\".", flaRea));
                state.dataExternalInterface->ErrorsFound = true;
                StopExternalInterfaceIfError(state);
            }
        }

        // Check communication flag
        if (flaRea != 0) {
            // No more values will be received in future steps
            // Added a check since the FMUExport  is terminated with the flaRea set to 1.
            state.dataExternalInterface->noMoreValues = true;
            if (state.dataExternalInterface->haveExternalInterfaceBCVTB) {
                ShowSevereError(state, format("ExternalInterface: Received end of simulation flag at time = {:.2T} hours.", preSimTim / 3600));
                StopExternalInterfaceIfError(state);
            }
        }

        // Make sure we get the right number of double values, unless retVal != 0
        if ((flaRea == 0) && (!state.dataExternalInterface->ErrorsFound) && continueSimulation &&
            (nDblRea != isize(state.dataExternalInterface->varInd))) {
            ShowSevereError(
                state,
                format("ExternalInterface: Received \"{}\" double values, expected \"{}\".", nDblRea, size(state.dataExternalInterface->varInd)));
            state.dataExternalInterface->ErrorsFound = true;
            StopExternalInterfaceIfError(state);
        }

        // No errors found. Assign exchanged variables
        if ((flaRea == 0) && continueSimulation) {
            for (i = 1; i <= isize(state.dataExternalInterface->varInd); ++i) {
                if (state.dataExternalInterface->inpVarTypes(i) == indexSchedule) {
                    ExternalInterfaceSetSchedule(state, state.dataExternalInterface->varInd(i), dblValRea(i));
                } else if ((state.dataExternalInterface->inpVarTypes(i) == indexVariable) ||
                           (state.dataExternalInterface->inpVarTypes(i) == indexActuator)) {
                    ExternalInterfaceSetErlVariable(state, state.dataExternalInterface->varInd(i), dblValRea(i));
                } else {
                    ShowContinueError(state, "ExternalInterface: Error in finding the type of the input variable for EnergyPlus");
                    ShowContinueError(state, format("variable index: {}. Variable will not be updated.", i));
                }
            }
        }
    }

    // If we have Erl variables, we need to call ManageEMS so that they get updated in the Erl data structure
    if (state.dataExternalInterface->useEMS) {
        bool anyRan;
        ManageEMS(state, EMSManager::EMSCallFrom::ExternalInterface, anyRan, ObjexxFCL::Optional_int_const());
    }

    state.dataExternalInterface->firstCall = false; // bug fix causing external interface to send zero at the beginning of sim, Thierry Nouidui
}

void GetReportVariableKey(
    EnergyPlusData &state,
    const Array1D_string &varKeys,                   // Standard variable name
    int const numberOfKeys,                          // Number of keys=size(state.dataExternalInterface->varKeys)
    const Array1D_string &VarNames,                  // Standard variable name
    Array1D_int &keyVarIndexes,                      // Array index
    Array1D<OutputProcessor::VariableType> &varTypes // Types of variables in state.dataExternalInterface->keystate.dataExternalInterface->varIndexes
)
{
    // SUBROUTINE INFORMATION:
    //       AUTHOR         Michael Wetter
    //       DATE WRITTEN   2Dec2007
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // Gets the sensor key index and type for the specified variable key and name

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    OutputProcessor::VariableType varType(OutputProcessor::VariableType::NotFound); // 0=not found, 1=integer, 2=real, 3=meter
    int numKeys(0);                                                                 // Number of keys found
    OutputProcessor::StoreType varAvgSum(OutputProcessor::StoreType::Averaged);     // Variable  is Averaged=1 or Summed=2
    OutputProcessor::TimeStepType varStepType(OutputProcessor::TimeStepType::Zone); // Variable time step is Zone=1 or HVAC=2
    OutputProcessor::Unit varUnits(OutputProcessor::Unit::None);                    // Units sting, may be blank
    Array1D_int keyIndexes;                                                         // Array index for
    Array1D_string NamesOfKeys;                                                     // Specific key name
    int Loop, iKey;                                                                 // Loop counters

    // Get pointers for variables to be sent to Ptolemy
    for (Loop = 1; Loop <= numberOfKeys; ++Loop) {
        GetVariableKeyCountandType(state, VarNames(Loop), numKeys, varType, varAvgSum, varStepType, varUnits);
        if (varType != OutputProcessor::VariableType::NotFound) {
            NamesOfKeys.allocate(numKeys);
            keyIndexes.allocate(numKeys);
            GetVariableKeys(state, VarNames(Loop), varType, NamesOfKeys, keyIndexes);
            // Find key index whose keyName is equal to keyNames(Loop)
            int max(NamesOfKeys.size());
            for (iKey = 1; iKey <= max; ++iKey) {
                if (NamesOfKeys(iKey) == varKeys(Loop)) {
                    keyVarIndexes(Loop) = keyIndexes(iKey);
                    varTypes(Loop) = varType;
                    break;
                }
            }
            keyIndexes.deallocate();
            NamesOfKeys.deallocate();
        }
        if ((varType == OutputProcessor::VariableType::NotFound) || (iKey > numKeys)) {
            ShowSevereError(state,
                            "ExternalInterface: Simulation model has no variable \"" + VarNames(Loop) + "\" with key \"" + varKeys(Loop) + "\".");
            state.dataExternalInterface->ErrorsFound = true;
        }
    }
}

void WarnIfExternalInterfaceObjectsAreUsed(EnergyPlusData &state, std::string const &ObjectWord)
{
    // SUBROUTINE INFORMATION:
    //       AUTHOR         Michael Wetter
    //       DATE WRITTEN   December 2009
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine writes a warning if ExternalInterface objects are used in the
    // idf file, but the ExternalInterface link is not specified.

    int const NumObjects = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, ObjectWord);
    if (NumObjects > 0) {
        ShowWarningError(state, "IDF file contains object \"" + ObjectWord + "\",");
        ShowContinueError(state, "but object \"ExternalInterface\" with appropriate key entry is not specified. Values will not be updated.");
    }
}

void VerifyExternalInterfaceObject(EnergyPlusData &state)
{
    // SUBROUTINE INFORMATION:
    //       AUTHOR         Michael Wetter
    //       DATE WRITTEN   12Dec2009
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine verifies the correctness of the fields of
    // the ExternalInterface object in the idf file

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int NumAlphas(0);  // Number of Alphas for each GetObjectItem call
    int NumNumbers(0); // Number of Numbers for each GetObjectItem call
    int IOStatus(0);   // Used in GetObjectItem
    auto &cCurrentModuleObject = state.dataIPShortCut->cCurrentModuleObject;

    cCurrentModuleObject = "ExternalInterface";
    state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                             cCurrentModuleObject,
                                                             1,
                                                             state.dataIPShortCut->cAlphaArgs,
                                                             NumAlphas,
                                                             state.dataIPShortCut->rNumericArgs,
                                                             NumNumbers,
                                                             IOStatus,
                                                             _,
                                                             _,
                                                             state.dataIPShortCut->cAlphaFieldNames,
                                                             state.dataIPShortCut->cNumericFieldNames);
    if ((!UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(1), "PtolemyServer")) &&
        (!UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(1), "FunctionalMockupUnitImport")) &&
        (!UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(1), "FunctionalMockupUnitExport"))) {
        ShowSevereError(state,
                        "VerifyExternalInterfaceObject: " + cCurrentModuleObject + ", invalid " + state.dataIPShortCut->cAlphaFieldNames(1) + "=\"" +
                            state.dataIPShortCut->cAlphaArgs(1) + "\".");
        ShowContinueError(state, "only \"PtolemyServer or FunctionalMockupUnitImport or FunctionalMockupUnitExport\" allowed.");
        state.dataExternalInterface->ErrorsFound = true;
    }
}

std::vector<char> getCharArrayFromString(std::string const &originalString)
{
    // c_str returns null terminated, so we don't need a +1?
    return std::vector<char>(originalString.c_str(), originalString.c_str() + originalString.size());
}

std::string getStringFromCharArray(std::vector<char> originalCharArray)
{
    originalCharArray.push_back('\0');
    return std::string(&originalCharArray.front());
}

} // namespace EnergyPlus::ExternalInterface
