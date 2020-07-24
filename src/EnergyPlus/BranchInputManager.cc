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

// C++ Headers
#include <string>

// ObjexxFCL Headers
#include <ObjexxFCL/Array.functions.hh>
#include <ObjexxFCL/Fmath.hh>
#include <ObjexxFCL/string.functions.hh>

// EnergyPlus Headers
#include <EnergyPlus/BranchInputManager.hh>
#include <EnergyPlus/BranchNodeConnections.hh>
#include <EnergyPlus/CurveManager.hh>
#include <EnergyPlus/DataBranchAirLoopPlant.hh>
#include <EnergyPlus/DataErrorTracking.hh>
#include <EnergyPlus/General.hh>
#include <EnergyPlus/GeneralRoutines.hh>
#include <EnergyPlus/InputProcessing/InputProcessor.hh>
#include <EnergyPlus/NodeInputManager.hh>
#include <EnergyPlus/UtilityRoutines.hh>

namespace EnergyPlus {

namespace BranchInputManager {

    // Module containing the routines dealing with the BRANCH and CONNECTOR
    // lists input.

    // MODULE INFORMATION:
    //       AUTHOR         Linda Lawrie
    //       DATE WRITTEN   October 1999
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS MODULE:
    // To Get the IDD objects "BranchList", "Branch", "ConnectorList",
    // "Connector:Splitter", and "Connector:Mixer".  Also, to supply other modules/routines with
    // information about these objects.

    // Using/Aliasing
    using DataGlobals::DisplayExtraWarnings;
    using namespace DataLoopNode;
    using namespace DataBranchAirLoopPlant;
    using namespace NodeInputManager;
    using namespace BranchNodeConnections;

    // MODULE PARAMETER DEFINITIONS
    const char * cMIXER("Connector:Mixer");
    const char * cSPLITTER("Connector:Splitter");

    void ManageBranchInput(BranchInputManagerData &dataBranchInputManager)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Linda Lawrie
        //       DATE WRITTEN   Nov 2004
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine is called from HVACManager to make sure that branch input is
        // gathered prior to need.

        if (dataBranchInputManager.GetBranchInputFlag) {
            GetBranchInput(dataBranchInputManager);
            if (dataBranchInputManager.GetBranchListInputFlag) {
                dataBranchInputManager.GetBranchListInputFlag = false;
                GetBranchListInput(dataBranchInputManager);
            }
            AuditBranches(dataBranchInputManager, false);
            dataBranchInputManager.GetBranchInputFlag = false;
        }
    }

    //==================================================================================
    //   Routines that "get" data from internal branch management structure
    //==================================================================================

    void GetBranchList(BranchInputManagerData &dataBranchInputManager,
                       std::string const &LoopName,       // Name of Loop Branch List is on
                       std::string const &BranchListName, // Branch List Name from Input
                       int &NumBranchNames,               // Number of Branches for this Branch List
                       Array1D_string &BranchNames,       // Names of Branches on this Branch List
                       std::string const &LoopType        // Type of Loop Branch list is on
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Linda K. Lawrie
        //       DATE WRITTEN   October 1999
        //       MODIFIED       October 2001, Automatic Extensibility
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine "gets" the branch list specified in a Plant or Condenser loop and
        // returns number and names to the outside calling routine.

        // Using/Aliasing
        using General::TrimSigDigits;

        int Found;     // Points to correct Branch List/Branch
        bool ErrFound; // True when error has occurred (cannot find Branch List)

        ErrFound = false;

        if (dataBranchInputManager.GetBranchListInputFlag) {
            dataBranchInputManager.GetBranchListInputFlag = false;
            GetBranchListInput(dataBranchInputManager);
        }

        //  Find this BranchList in the master BranchList Names
        Found = UtilityRoutines::FindItemInList(BranchListName, dataBranchInputManager.BranchList);
        if (Found == 0) {
            ShowFatalError("GetBranchList: BranchList Name not found=" + BranchListName);
        }

        // Set data
        if (dataBranchInputManager.BranchList(Found).LoopName.empty()) {
            dataBranchInputManager.BranchList(Found).LoopName = LoopName;
            dataBranchInputManager.BranchList(Found).LoopType = LoopType;
        } else if (dataBranchInputManager.BranchList(Found).LoopName != LoopName) {
            ShowSevereError("GetBranchList: BranchList Loop Name already assigned");
            ShowContinueError("BranchList=" + dataBranchInputManager.BranchList(Found).Name + ", already assigned to loop=" + dataBranchInputManager.BranchList(Found).LoopName);
            ShowContinueError("Now requesting assignment to Loop=" + LoopName);
            ErrFound = true;
        }

        // Return data
        NumBranchNames = dataBranchInputManager.BranchList(Found).NumOfBranchNames;
        if (isize(BranchNames) < NumBranchNames) {
            ShowSevereError("GetBranchList: Branch Names array not big enough to hold Branch Names");
            ShowContinueError("Input BranchListName=" + BranchListName + ", in Loop=" + LoopName);
            ShowContinueError("BranchName Array size=" + TrimSigDigits(size(BranchNames)) + ", but input size=" + TrimSigDigits(NumBranchNames));
            ErrFound = true;
        } else {
            BranchNames = "";
            BranchNames({1, NumBranchNames}) = dataBranchInputManager.BranchList(Found).BranchNames({1, NumBranchNames});
        }

        if (ErrFound) {
            ShowFatalError("GetBranchList: preceding condition(s) causes program termination.");
        }
    }

    int NumBranchesInBranchList(BranchInputManagerData &dataBranchInputManager, std::string const &BranchListName)
    {

        // FUNCTION INFORMATION:
        //       AUTHOR         Linda K. Lawrie
        //       DATE WRITTEN   July 2003
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS FUNCTION:
        // This function returns the number of branches in a branch list so that the calling
        // routine can allocate arrays before calling GetBranchList.

        // Return value
        int NumBranchesInBranchList;

        // FUNCTION LOCAL VARIABLE DECLARATIONS:
        int Found;

        if (dataBranchInputManager.GetBranchListInputFlag) {
            dataBranchInputManager.GetBranchListInputFlag = false;
            GetBranchListInput(dataBranchInputManager);
        }

        //  Find this BranchList in the master BranchList Names
        Found = UtilityRoutines::FindItemInList(BranchListName, dataBranchInputManager.BranchList);
        if (Found == 0) {
            ShowFatalError("NumBranchesInBranchList: BranchList Name not found=" + BranchListName);
        }

        NumBranchesInBranchList = dataBranchInputManager.BranchList(Found).NumOfBranchNames;

        return NumBranchesInBranchList;
    }

    void GetBranchData(BranchInputManagerData &dataBranchInputManager,
                       std::string const &LoopName,         // Loop Name of this Branch
                       std::string const &BranchName,       // Requested Branch Name
                       int &PressCurveType,                 // Index of a pressure curve object
                       int &PressCurveIndex,                // Index of a pressure curve object
                       int &NumComps,                       // Number of Components on Branch
                       Array1D_string &CompType,            // Component Type for each item on Branch
                       Array1D_string &CompName,            // Component Name for each item on Branch
                       Array1D_string &CompInletNodeNames,  // Component Inlet Node IDs for each item on Branch
                       Array1D_int &CompInletNodeNums,      // Component Inlet Node Numbers for each item on Branch
                       Array1D_string &CompOutletNodeNames, // Component Outlet Node IDs for each item on Branch
                       Array1D_int &CompOutletNodeNums,     // Component Outlet Node Numbers for each item on Branch
                       bool &ErrorsFound)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Linda K. Lawrie
        //       DATE WRITTEN   October 1999
        //       MODIFIED       October 2001, Automatic Extensibility
        //                      September 2012, B. Griffith, removed component control types
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This routine gets the Branch Data (internal structure) for the requested
        // Branch Name and returns it in "list structure" to the calling routine.

        // Using/Aliasing
        using General::TrimSigDigits;

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int Count; // Loop Counter
        int MinCompsAllowed;

        // Object Data
        static Array1D<ComponentData> BComponents; // Component data to be returned

        // NumComps now defined on input

        BComponents.allocate(NumComps);

        GetInternalBranchData(dataBranchInputManager, LoopName, BranchName, PressCurveType, PressCurveIndex, NumComps, BComponents, ErrorsFound);

        MinCompsAllowed = min(
            size(CompType), size(CompName), size(CompInletNodeNames), size(CompInletNodeNums), size(CompOutletNodeNames), size(CompOutletNodeNums));
        if (MinCompsAllowed < NumComps) {
            ShowSevereError("GetBranchData: Component List arrays not big enough to hold Number of Components");
            ShowContinueError("Input BranchName=" + BranchName + ", in Loop=" + LoopName);
            ShowContinueError("Max Component Array size=" + TrimSigDigits(MinCompsAllowed) + ", but input size=" + TrimSigDigits(NumComps));
            ShowFatalError("Program terminates due to preceding conditions.");
        }

        for (Count = 1; Count <= NumComps; ++Count) {
            CompType(Count) = BComponents(Count).CType;
            CompName(Count) = BComponents(Count).Name;
            CompInletNodeNames(Count) = BComponents(Count).InletNodeName;
            CompInletNodeNums(Count) = BComponents(Count).InletNode;
            CompOutletNodeNames(Count) = BComponents(Count).OutletNodeName;
            CompOutletNodeNums(Count) = BComponents(Count).OutletNode;
        }
        BComponents.deallocate();
    }

    int NumCompsInBranch(BranchInputManagerData &dataBranchInputManager, std::string const &BranchName)
    {

        // FUNCTION INFORMATION:
        //       AUTHOR         Linda K. Lawrie
        //       DATE WRITTEN   July 2003
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS FUNCTION:
        // This function returns the number of components in a branch so that the calling
        // routine can allocate arrays before calling GetBranchData.

        // Return value
        int NumCompsInBranch;

        // FUNCTION LOCAL VARIABLE DECLARATIONS:
        int Found;

        if (dataBranchInputManager.GetBranchInputFlag) {
            dataBranchInputManager.GetBranchInputFlag = false;
            GetBranchInput(dataBranchInputManager);
        }

        Found = UtilityRoutines::FindItemInList(BranchName, dataBranchInputManager.Branch);
        if (Found == 0) {
            ShowSevereError("NumCompsInBranch:  Branch not found=" + BranchName);
            NumCompsInBranch = 0;
        } else {
            NumCompsInBranch = dataBranchInputManager.Branch(Found).NumOfComponents;
        }

        return NumCompsInBranch;
    }

    int GetAirBranchIndex(BranchInputManagerData &dataBranchInputManager, std::string const &CompType, std::string const &CompName)
    {

        // FUNCTION INFORMATION:
        //       AUTHOR         Richard Raustad, FSEC
        //       DATE WRITTEN   April 2013
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS FUNCTION:
        // This function returns the branch index so that the calling
        // routine can search for a fan on this branch or use branch flow for sizing.

        // Return value
        int GetAirBranchIndex(0);

        // FUNCTION LOCAL VARIABLE DECLARATIONS:
        int BranchNum;
        int CompNum;
        int NumBranches;

        if (dataBranchInputManager.GetBranchInputFlag) {
            dataBranchInputManager.GetBranchInputFlag = false;
            GetBranchInput(dataBranchInputManager);
        }

        NumBranches = size(dataBranchInputManager.Branch);

        if (NumBranches == 0) {
            ShowSevereError("GetAirBranchIndex:  Branch not found with component = " + CompType + " \"" + CompName + "\"");
        } else {
            for (BranchNum = 1; BranchNum <= NumBranches; ++BranchNum) {
                for (CompNum = 1; CompNum <= dataBranchInputManager.Branch(BranchNum).NumOfComponents; ++CompNum) {
                    if (UtilityRoutines::SameString(CompType, dataBranchInputManager.Branch(BranchNum).Component(CompNum).CType) &&
                        UtilityRoutines::SameString(CompName, dataBranchInputManager.Branch(BranchNum).Component(CompNum).Name)) {
                        GetAirBranchIndex = BranchNum;
                        goto BranchLoop_exit;
                    }
                }
            }
        BranchLoop_exit:;
        }

        return GetAirBranchIndex;
    }

    void GetBranchFanTypeName(BranchInputManagerData &dataBranchInputManager,
                              int const BranchNum,
                              std::string &FanType,
                              std::string &FanName,
                              bool &ErrFound // Set to true if error found, false otherwise
    )
    {

        // FUNCTION INFORMATION:
        //       AUTHOR         Richard Raustad, FSEC
        //       DATE WRITTEN   April 2013
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS FUNCTION:
        // This function returns the branch fan flow rate so that the calling
        // routine can either use this flow or use then branch flow for sizing.

        // Using/Aliasing
        using General::TrimSigDigits;

        // FUNCTION LOCAL VARIABLE DECLARATIONS:
        int CompNum;
        int NumBranches;

        if (dataBranchInputManager.GetBranchInputFlag) {
            dataBranchInputManager.GetBranchInputFlag = false;
            GetBranchInput(dataBranchInputManager);
        }

        ErrFound = false;
        NumBranches = size(dataBranchInputManager.Branch);

        FanType = std::string();
        FanName = std::string();

        if (NumBranches == 0) {
            ShowSevereError("GetBranchFanTypeName:  Branch index not found = " + TrimSigDigits(BranchNum));
            ErrFound = true;
        } else {
            if (BranchNum > 0 && BranchNum <= NumBranches) {
                for (CompNum = 1; CompNum <= dataBranchInputManager.Branch(BranchNum).NumOfComponents; ++CompNum) {
                    if (UtilityRoutines::SameString("Fan:OnOff", dataBranchInputManager.Branch(BranchNum).Component(CompNum).CType) ||
                        UtilityRoutines::SameString("Fan:ConstantVolume", dataBranchInputManager.Branch(BranchNum).Component(CompNum).CType) ||
                        UtilityRoutines::SameString("Fan:VariableVolume", dataBranchInputManager.Branch(BranchNum).Component(CompNum).CType) ||
                        UtilityRoutines::SameString("Fan:SystemModel", dataBranchInputManager.Branch(BranchNum).Component(CompNum).CType)) {
                        FanType = dataBranchInputManager.Branch(BranchNum).Component(CompNum).CType;
                        FanName = dataBranchInputManager.Branch(BranchNum).Component(CompNum).Name;
                        break;
                    }
                }
                if (FanType.empty()) ErrFound = true;
            } else {
                ShowSevereError("GetBranchFanTypeName:  Branch index not found = " + TrimSigDigits(BranchNum));
                ErrFound = true;
            }
        }
    }

    void GetInternalBranchData(BranchInputManagerData &dataBranchInputManager,
                               std::string const &LoopName,         // Loop Name for Branch
                               std::string const &BranchName,       // Requested Branch Name
                               int &PressCurveType,                 // Index of pressure curve object
                               int &PressCurveIndex,                // Index of pressure curve object
                               int &NumComps,                       // Number of Components on Branch
                               Array1D<ComponentData> &BComponents, // Component data returned
                               bool &ErrorsFound                    // True when Loop Name is already assigned and this not same loop
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Linda K. Lawrie
        //       DATE WRITTEN   October 1999
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This routine gets the Branch Data (internal structure) for the requested
        // Branch Name and returns it to the calling routine.  This is used internally
        // in the module.

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int Found; // Pointer to requested Branch Name

        if (dataBranchInputManager.GetBranchInputFlag) {
            GetBranchInput(dataBranchInputManager);
            dataBranchInputManager.GetBranchInputFlag = false;
        }

        Found = UtilityRoutines::FindItemInList(BranchName, dataBranchInputManager.Branch);
        if (Found == 0) {
            ShowSevereError("GetInternalBranchData:  Branch not found=" + BranchName);
            ErrorsFound = true;
            NumComps = 0;
        } else {
            if (dataBranchInputManager.Branch(Found).AssignedLoopName.empty()) {
                dataBranchInputManager.Branch(Found).AssignedLoopName = LoopName;
                PressCurveType = dataBranchInputManager.Branch(Found).PressureCurveType;
                PressCurveIndex = dataBranchInputManager.Branch(Found).PressureCurveIndex;
                NumComps = dataBranchInputManager.Branch(Found).NumOfComponents;
                BComponents({1, NumComps}) = dataBranchInputManager.Branch(Found).Component({1, NumComps});
            } else if (dataBranchInputManager.Branch(Found).AssignedLoopName != LoopName) {
                ShowSevereError("Attempt to assign branch to two different loops, Branch=" + BranchName);
                ShowContinueError("Branch already assigned to loop=" + dataBranchInputManager.Branch(Found).AssignedLoopName);
                ShowContinueError("New attempt to assign to loop=" + LoopName);
                ErrorsFound = true;
                NumComps = 0;
            } else {
                PressCurveType = dataBranchInputManager.Branch(Found).PressureCurveType;
                PressCurveIndex = dataBranchInputManager.Branch(Found).PressureCurveIndex;
                NumComps = dataBranchInputManager.Branch(Found).NumOfComponents;
                BComponents({1, NumComps}) = dataBranchInputManager.Branch(Found).Component({1, NumComps});
            }
        }
    }

    void GetNumSplitterMixerInConntrList(BranchInputManagerData &dataBranchInputManager,
                                         std::string const &LoopName,          // Loop Name for this Splitter (used in error message)
                                         std::string const &ConnectorListName, // Requested Connector List Name
                                         int &numSplitters,                    // Number of splitters in the loop
                                         int &numMixers,                       // Number of mixers in the loop
                                         bool &ErrorsFound                     // if no connector list
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Sankaranarayanan K P
        //       DATE WRITTEN   April 2005
        //       MODIFIED       Linda Lawrie - September 2005
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine returns the number of splitter and mixers in a connector list item
        // The data is filled from the idd object 'ConnectorList'

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int ConnNum;

        if (dataBranchInputManager.GetConnectorListInputFlag) {
            GetConnectorListInput(dataBranchInputManager);
            dataBranchInputManager.GetConnectorListInputFlag = false;
        }

        numSplitters = 0;
        numMixers = 0;
        ConnNum = UtilityRoutines::FindItemInList(ConnectorListName, dataBranchInputManager.ConnectorLists);

        if (ConnNum > 0) {
            numSplitters = dataBranchInputManager.ConnectorLists(ConnNum).NumOfSplitters;
            numMixers = dataBranchInputManager.ConnectorLists(ConnNum).NumOfMixers;
        } else {
            ShowSevereError("Ref: Loop=" + LoopName + ", Connector List not found=" + ConnectorListName);
            ErrorsFound = true;
        }
    }

    void GetConnectorList(BranchInputManagerData &dataBranchInputManager,
                          std::string const &ConnectorListName, // Requested Connector List
                          ConnectorData &Connectoid,            // Returned Connector Data
                          Optional_int_const NumInList          // Number of the current connector in the list of connectors
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Linda K. Lawrie
        //       DATE WRITTEN   October 1999
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Obtains connector data for requested connector list.  Also,
        // this subroutine gets the input for the following IDD structure:
        // ConnectorList,
        //         \memo only two connectors allowed per loop
        //         \memo if two entered, one must be Connector:Splitter and one must be Connector:Mixer
        //     A1, \field Name
        //         \required-field
        //         \reference ConnectorLists
        //     A2, \field Connector 1 Object Type
        //         \required-field
        //         \key Connector:Splitter
        //         \key Connector:Mixer
        //     A3, \field Connector 1 Name
        //         \required-field
        //     A4, \field Connector 2 Object Type
        //         \key Connector:Splitter
        //         \key Connector:Mixer
        //     A5; \field Connector 2 Name

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int Count; // Loop Counter

        if (dataBranchInputManager.GetConnectorListInputFlag) {
            GetConnectorListInput(dataBranchInputManager);
            dataBranchInputManager.GetConnectorListInputFlag = false;
        }

        if (not_blank(ConnectorListName)) {
            Count = UtilityRoutines::FindItemInList(ConnectorListName, dataBranchInputManager.ConnectorLists);
            if (Count == 0) {
                ShowFatalError("GetConnectorList: Connector List not found=" + ConnectorListName);
            }
            Connectoid = dataBranchInputManager.ConnectorLists(Count);
            if (present(NumInList)) {
                Connectoid.ConnectorType(1) = dataBranchInputManager.ConnectorLists(Count).ConnectorType(NumInList);
                Connectoid.ConnectorName(1) = dataBranchInputManager.ConnectorLists(Count).ConnectorName(NumInList);
                Connectoid.ConnectorType(2) = "";
                Connectoid.ConnectorName(2) = "";
            }
        } else {
            Connectoid.Name = "";
            Connectoid.NumOfConnectors = 0;
            Connectoid.ConnectorType(1) = "";
            Connectoid.ConnectorType(2) = "";
            Connectoid.ConnectorName(1) = "";
            Connectoid.ConnectorName(2) = "";
        }
    }

    void GetLoopMixer(BranchInputManagerData &dataBranchInputManager,
                      std::string const &LoopName,          // Loop Name for Mixer
                      std::string const &ConnectorListName, // Requested Connector List Name
                      std::string &MixerName,               // Name of Mixer
                      bool &IsMixer,                        // True when Mixer is on this connector, false otherwise
                      std::string &OutletNodeName,          // Outlet Node ID
                      int &OutletNodeNum,                   // Outlet Node Number
                      int &NumInletNodes,                   // Number of Inlet Nodes
                      Array1D_string &InletNodeNames,       // Inlet Node IDs
                      Array1D_int &InletNodeNums,           // Inlet Node Numbers
                      bool &ErrorsFound,
                      Optional_int_const ConnectorNumber, // number of the current item in connector list
                      Optional_int MixerNumber            // Mixer number for this specific splitter
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Linda K. Lawrie
        //       DATE WRITTEN   October 1999
        //       MODIFIED       October 2001, Automatic Extensibility
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This routine gets the data for the requested Connector List and returns values indicating
        // if this connector list name is a mixer or not.

        // Using/Aliasing
        using General::TrimSigDigits;

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int Count;    // Loop Counter
        int Loop;     // Loop Counter
        int NumComps; // Number of Components on this Branch
        int PressCurveType;
        int PressCurveIndex;
        bool errFlag; // Error flag from RegisterNodeConnection
        int NumParams;
        int NumAlphas;
        int NumNumbers;

        // Object Data
        ConnectorData Connectoid;           // Connector Data
        Array1D<ComponentData> BComponents; // Branch Component Data

        if (dataBranchInputManager.GetMixerInputFlag) {
            GetMixerInput(dataBranchInputManager);
            dataBranchInputManager.GetMixerInputFlag = false;
        }

        GetConnectorList(dataBranchInputManager, ConnectorListName, Connectoid, ConnectorNumber);
        if (UtilityRoutines::SameString(Connectoid.ConnectorType(1), cMIXER)) {
            Count = UtilityRoutines::FindItemInList(Connectoid.ConnectorName(1), dataBranchInputManager.Mixers);
            if (present(MixerNumber)) ++MixerNumber;
            if (Count == 0) {
                ShowFatalError("GetLoopMixer: No Mixer Found=" + Connectoid.ConnectorName(1));
            }
        } else if (UtilityRoutines::SameString(Connectoid.ConnectorType(2), cMIXER)) {
            Count = UtilityRoutines::FindItemInList(Connectoid.ConnectorName(2), dataBranchInputManager.Mixers);
            if (Count == 0) {
                ShowFatalError("GetLoopMixer: No Mixer Found=" + Connectoid.ConnectorName(2));
            }
        } else {
            Count = 0;
        }

        // Set defaults for later error potential
        IsMixer = false;
        MixerName = std::string();
        OutletNodeName = std::string();
        OutletNodeNum = 0;
        NumInletNodes = 0;
        InletNodeNames = "";
        InletNodeNums = 0;

        if (Count != 0) { // Build up Output list(s). For each component(?)

            // The inlet nodes for the mixer will be the last "outlet" node of
            // each corresponding inlet branch.  The outlet node for the mixer
            // will be the first "inlet" node of the outlet branch since that
            // would be the first node on the branch.
            MixerName = dataBranchInputManager.Mixers(Count).Name;
            IsMixer = true;
            // The number of "components" on a Mixer is the number of branches.  This is the number of alpha arguments -1.
            inputProcessor->getObjectDefMaxArgs("Branch", NumParams, NumAlphas, NumNumbers);
            BComponents.allocate(NumAlphas - 1);
            errFlag = false;
            GetInternalBranchData(dataBranchInputManager, LoopName, dataBranchInputManager.Mixers(Count).OutletBranchName, PressCurveType, PressCurveIndex, NumComps, BComponents, errFlag);
            if (errFlag) {
                ShowContinueError("..occurs for Connector:Mixer Name=" + dataBranchInputManager.Mixers(Count).Name);
                ErrorsFound = true;
            }
            if (NumComps > 0) {
                OutletNodeName = BComponents(1).InletNodeName;
                OutletNodeNum = BComponents(1).InletNode;
                NumInletNodes = dataBranchInputManager.Mixers(Count).NumInletBranches;
                // Register this node connection because the mixer gets node information indirectly from the branch
                errFlag = false;
                RegisterNodeConnection(OutletNodeNum,
                                       NodeID(OutletNodeNum),
                                       "Connector:Mixer",
                                       MixerName,
                                       ValidConnectionTypes(NodeConnectionType_Outlet),
                                       1,
                                       ObjectIsNotParent,
                                       errFlag);

                if (NumInletNodes > isize(InletNodeNames) || NumInletNodes > isize(InletNodeNums)) {
                    ShowSevereError("GetLoopMixer: Connector:Mixer=" + MixerName + " contains too many inlets for size of Inlet Array.");
                    ShowContinueError("Max array size=" + TrimSigDigits(size(InletNodeNames)) +
                                      ", Mixer statement inlets=" + TrimSigDigits(NumInletNodes));
                    ShowFatalError("Program terminates due to preceding condition.");
                }
                InletNodeNums = 0;
                InletNodeNames = "";

                for (Loop = 1; Loop <= dataBranchInputManager.Mixers(Count).NumInletBranches; ++Loop) {
                    GetInternalBranchData(dataBranchInputManager,
                        LoopName, dataBranchInputManager.Mixers(Count).InletBranchNames(Loop), PressCurveType, PressCurveIndex, NumComps, BComponents, ErrorsFound);
                    if (NumComps > 0) {
                        InletNodeNames(Loop) = BComponents(NumComps).OutletNodeName;
                        InletNodeNums(Loop) = BComponents(NumComps).OutletNode;
                        // Register this node connection because the mixer gets node information indirectly from the branch
                        errFlag = false;
                        RegisterNodeConnection(InletNodeNums(Loop),
                                               NodeID(InletNodeNums(Loop)),
                                               "Connector:Mixer",
                                               MixerName,
                                               ValidConnectionTypes(NodeConnectionType_Inlet),
                                               1,
                                               ObjectIsNotParent,
                                               errFlag);
                    }
                }
            } else {
                // Set so cascading errors don't happen?
                IsMixer = false;
            }
            BComponents.deallocate();
        }
    }

    void GetLoopSplitter(BranchInputManagerData &dataBranchInputManager,
                         std::string const &LoopName,          // Loop Name for this Splitter
                         std::string const &ConnectorListName, // Requested Connector List Name
                         std::string &SplitterName,            // Name of Splitter
                         bool &IsSplitter,                     // True if splitter on this connector list, false otherwise
                         std::string &InletNodeName,           // Inlet Node ID
                         int &InletNodeNum,                    // Inlet Node Number
                         int &NumOutletNodes,                  // Number of Outlet Nodes
                         Array1D_string &OutletNodeNames,      // Outlet Node IDs
                         Array1D_int &OutletNodeNums,          // Outlet Node Numbers
                         bool &ErrorsFound,
                         Optional_int_const ConnectorNumber, // number of the current item in connector list
                         Optional_int SplitterNumber         // splitter number for this specific splitter
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Linda K. Lawrie
        //       DATE WRITTEN   October 1999
        //       MODIFIED       October 2001, Automatic Extensibility
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This routine gets the data for the requested Connector List and returns values indicating
        // if this connector list name is a splitter or not.

        // Using/Aliasing
        using General::TrimSigDigits;

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int Count;    // Loop Counter
        int Loop;     // Loop Counter
        int NumComps; // Number of Components on this Branch
        int PressCurveType;
        int PressCurveIndex;
        bool errFlag; // Error flag from RegisterNodeConnection
        int NumParams;
        int NumAlphas;
        int NumNumbers;

        // Object Data
        ConnectorData Connectoid;           // Connector Data
        Array1D<ComponentData> BComponents; // Branch Component Data

        if (dataBranchInputManager.GetSplitterInputFlag) {
            GetSplitterInput(dataBranchInputManager);
            dataBranchInputManager.GetSplitterInputFlag = false;
        }

        if (ConnectorListName.empty()) {
            ShowSevereError("GetLoopSplitter: ConnectorListName is blank.  LoopName=" + LoopName);
            ShowFatalError("Program terminates due to previous condition.");
        }
        GetConnectorList(dataBranchInputManager, ConnectorListName, Connectoid, ConnectorNumber);
        if (UtilityRoutines::SameString(Connectoid.ConnectorType(1), cSPLITTER)) {
            Count = UtilityRoutines::FindItemInList(Connectoid.ConnectorName(1), dataBranchInputManager.Splitters);
            if (present(SplitterNumber)) ++SplitterNumber;
            if (Count == 0) {
                ShowFatalError("GetLoopSplitter: No Splitter Found=" + Connectoid.ConnectorName(1));
            }
        } else if (UtilityRoutines::SameString(Connectoid.ConnectorType(2), cSPLITTER)) {
            Count = UtilityRoutines::FindItemInList(Connectoid.ConnectorName(2), dataBranchInputManager.Splitters);
            if (Count == 0) {
                ShowFatalError("GetLoopSplitter: No Splitter Found=" + Connectoid.ConnectorName(2));
            }
        } else {
            Count = 0;
        }

        // Default for any errors
        SplitterName = std::string();
        IsSplitter = false;
        InletNodeName = std::string();
        InletNodeNum = 0;
        NumOutletNodes = 0;
        OutletNodeNames = "";
        OutletNodeNums = 0;

        if (Count != 0) { // Build up Output list(s). For each component(?)

            // The inlet node for the splitter will be the last "outlet" node of the inlet
            // branch. The outlet nodes for the splitter will be the first "inlet" node of
            // each corresponding outlet branch since that would be the first node on the branch.

            SplitterName = dataBranchInputManager.Splitters(Count).Name;
            IsSplitter = true;
            // The number of "components" on a Splitter is the number of branches.  This is the number of alpha arguments -1.
            inputProcessor->getObjectDefMaxArgs("Branch", NumParams, NumAlphas, NumNumbers);
            BComponents.allocate(NumAlphas - 1);
            errFlag = false;
            GetInternalBranchData(dataBranchInputManager, LoopName, dataBranchInputManager.Splitters(Count).InletBranchName, PressCurveType, PressCurveIndex, NumComps, BComponents, errFlag);
            if (errFlag) {
                ShowContinueError("..occurs for Splitter Name=" + dataBranchInputManager.Splitters(Count).Name);
                ErrorsFound = true;
            }
            if (NumComps > 0) {
                InletNodeName = BComponents(NumComps).OutletNodeName;
                InletNodeNum = BComponents(NumComps).OutletNode;
                NumOutletNodes = dataBranchInputManager.Splitters(Count).NumOutletBranches;
                // Register this node connection because the splitter gets node information indirectly from the branch
                errFlag = false;
                RegisterNodeConnection(InletNodeNum,
                                       NodeID(InletNodeNum),
                                       "Connector:Splitter",
                                       SplitterName,
                                       ValidConnectionTypes(NodeConnectionType_Inlet),
                                       1,
                                       ObjectIsNotParent,
                                       errFlag);

                if (NumOutletNodes > isize(OutletNodeNames) || NumOutletNodes > isize(OutletNodeNums)) {
                    ShowSevereError("GetLoopSplitter: Connector:Splitter=" + SplitterName + " contains too many outlets for size of Outlet Array.");
                    ShowContinueError("Max array size=" + TrimSigDigits(size(OutletNodeNames)) +
                                      ", Splitter statement outlets=" + TrimSigDigits(NumOutletNodes));
                    ShowFatalError("Program terminates due to preceding condition.");
                }
                OutletNodeNums = 0;
                OutletNodeNames = "";

                for (Loop = 1; Loop <= dataBranchInputManager.Splitters(Count).NumOutletBranches; ++Loop) {
                    GetInternalBranchData(dataBranchInputManager,
                        LoopName, dataBranchInputManager.Splitters(Count).OutletBranchNames(Loop), PressCurveType, PressCurveIndex, NumComps, BComponents, ErrorsFound);
                    if (NumComps > 0) {
                        OutletNodeNames(Loop) = BComponents(1).InletNodeName;
                        OutletNodeNums(Loop) = BComponents(1).InletNode;
                        // Register this node connection because the splitter gets node information indirectly from the branch
                        errFlag = false;
                        RegisterNodeConnection(OutletNodeNums(Loop),
                                               NodeID(OutletNodeNums(Loop)),
                                               "Connector:Splitter",
                                               SplitterName,
                                               ValidConnectionTypes(NodeConnectionType_Outlet),
                                               1,
                                               ObjectIsNotParent,
                                               errFlag);
                    }
                }
            } else {
                //  Set so cascading errors don't happen
                IsSplitter = false;
            }
            BComponents.deallocate();
        }
    }

    std::string GetFirstBranchInletNodeName(BranchInputManagerData &dataBranchInputManager, std::string const &BranchListName) // Branch List name to search
    {

        // FUNCTION INFORMATION:
        //       AUTHOR         Linda K. Lawrie
        //       DATE WRITTEN   November 2004
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS FUNCTION:
        // This function uses the branch structure to obtain the inlet node
        // of the first branch from referenced Branch List.

        // Return value
        std::string InletNodeName; // Inlet node name of first branch in branch list

        // FUNCTION LOCAL VARIABLE DECLARATIONS:
        int Found1; // Pointer to Branch List Name
        int Found2; // Pointer to Branch data

        if (dataBranchInputManager.GetBranchListInputFlag) {
            dataBranchInputManager.GetBranchListInputFlag = false;
            GetBranchListInput(dataBranchInputManager);
        }

        Found1 = UtilityRoutines::FindItemInList(BranchListName, dataBranchInputManager.BranchList);
        if (Found1 == 0) {
            ShowSevereError("GetFirstBranchInletNodeName: BranchList=\"" + BranchListName + "\", not a valid BranchList Name");
            InletNodeName = "Invalid Node Name";
        } else {
            Found2 = UtilityRoutines::FindItemInList(dataBranchInputManager.BranchList(Found1).BranchNames(1), dataBranchInputManager.Branch);
            if (Found2 == 0) {
                ShowSevereError("GetFirstBranchInletNodeName: BranchList=\"" + BranchListName + "\", Branch=\"" + dataBranchInputManager.BranchList(Found1).BranchNames(1) +
                                "\" not a valid Branch Name");
                InletNodeName = "Invalid Node Name";
            } else {
                InletNodeName = dataBranchInputManager.Branch(Found2).Component(1).InletNodeName;
            }
        }

        return InletNodeName;
    }

    std::string GetLastBranchOutletNodeName(BranchInputManagerData &dataBranchInputManager, std::string const &BranchListName) // Branch List name to search
    {

        // FUNCTION INFORMATION:
        //       AUTHOR         Linda K. Lawrie
        //       DATE WRITTEN   August 2003
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS FUNCTION:
        // This function uses the branch structure to obtain the outlet node
        // of the last branch from referenced Branch List.

        // Return value
        std::string OutletNodeName; // Outlet node name of last branch in branch list

        // FUNCTION LOCAL VARIABLE DECLARATIONS:
        int Found1; // Pointer to Branch List Name
        int Found2; // Pointer to Branch data

        if (dataBranchInputManager.GetBranchListInputFlag) {
            dataBranchInputManager.GetBranchListInputFlag = false;
            GetBranchListInput(dataBranchInputManager);
        }

        Found1 = UtilityRoutines::FindItemInList(BranchListName, dataBranchInputManager.BranchList);
        if (Found1 == 0) {
            ShowSevereError("GetLastBranchOutletNodeName: BranchList=\"" + BranchListName + "\", not a valid BranchList Name");
            OutletNodeName = "Invalid Node Name";
        } else {
            Found2 = UtilityRoutines::FindItemInList(dataBranchInputManager.BranchList(Found1).BranchNames(dataBranchInputManager.BranchList(Found1).NumOfBranchNames), dataBranchInputManager.Branch);
            if (Found2 == 0) {
                ShowSevereError("GetLastBranchOutletNodeName: BranchList=\"" + BranchListName + "\", Branch=\"" +
                                dataBranchInputManager.BranchList(Found1).BranchNames(dataBranchInputManager.BranchList(Found1).NumOfBranchNames) + "\" not a valid Branch Name");
                OutletNodeName = "Invalid Node Name";
            } else {
                OutletNodeName = dataBranchInputManager.Branch(Found2).Component(dataBranchInputManager.Branch(Found2).NumOfComponents).OutletNodeName;
            }
        }

        return OutletNodeName;
    }

    //==================================================================================
    //   Routines that get the input for the internal branch management structure
    //==================================================================================

    void GetBranchInput(BranchInputManagerData &dataBranchInputManager)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Linda K. Lawrie
        //       DATE WRITTEN   October 1999
        //       MODIFIED       October 2001, Automatic Extensibility
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine gets the input for the following IDD structure:
        // Branch,
        //         \extensible:4 Just duplicate last 4 fields and \ comments (changing numbering, please)
        //         \memo List components on the branch in simulation and connection order
        //         \memo Note: this should NOT include splitters or mixers which define
        //         \memo endpoints of branches
        //    A1,  \field Name
        //         \required-field
        //         \reference Branches
        //    N1, \field Maximum Flow Rate
        //         \default 0
        //         \units m3/s
        //         \minimum 0
        //         \autosizable
        //    A2, \field Pressure Curve Name
        //         \type object-list
        //         \reference AllCurves
        //    A3, \field Component 1 Object Type
        //         \required-field
        //    A4, \field Component 1 Name
        //         \required-field
        //    A5, \field Component 1 Inlet Node Name
        //         \required-field
        //    A6, \field Component 1 Outlet Node Name
        //         \required-field

        // SUBROUTINE PARAMETER DEFINITIONS:
        static std::string const RoutineName("GetBranchInput: ");

        // INTERFACE BLOCK SPECIFICATIONS
        // na

        int BCount;              // Actual Num of Branches
        bool ErrFound;           // Flag for error detection
        int NumAlphas;           // Used to retrieve names from IDF
        Array1D_string Alphas;   // Used to retrieve names from IDF
        Array1D_int NodeNums;    // Possible Array of Node Numbers (only 1 allowed)
        int NumNumbers;          // Used to retrieve numbers from IDF
        Array1D<Real64> Numbers; // Used to retrieve numbers from IDF
        Array1D_string cAlphaFields;
        Array1D_string cNumericFields;
        Array1D_bool lNumericBlanks;
        Array1D_bool lAlphaBlanks;
        int IOStat; // Could be used in the Get Routines, not currently checked
        int NumParams;

        if (dataBranchInputManager.GetBranchInputOneTimeFlag) {
            std::string CurrentModuleObject = "Branch";
            dataBranchInputManager.NumOfBranches = inputProcessor->getNumObjectsFound(CurrentModuleObject);
            if (dataBranchInputManager.NumOfBranches > 0) {
                dataBranchInputManager.Branch.allocate(dataBranchInputManager.NumOfBranches);
                for (auto &e : dataBranchInputManager.Branch)
                    e.AssignedLoopName.clear();
                ErrFound = false;
                inputProcessor->getObjectDefMaxArgs("NodeList", NumParams, NumAlphas, NumNumbers);
                NodeNums.dimension(NumParams, 0);
                inputProcessor->getObjectDefMaxArgs(CurrentModuleObject, NumParams, NumAlphas, NumNumbers);
                Alphas.allocate(NumAlphas);
                Numbers.dimension(NumNumbers, 0.0);
                cAlphaFields.allocate(NumAlphas);
                cNumericFields.allocate(NumNumbers);
                lAlphaBlanks.dimension(NumAlphas, true);
                lNumericBlanks.dimension(NumNumbers, true);
                BCount = 0;
                for (int Count = 1; Count <= dataBranchInputManager.NumOfBranches; ++Count) {
                    inputProcessor->getObjectItem(CurrentModuleObject,
                                                  Count,
                                                  Alphas,
                                                  NumAlphas,
                                                  Numbers,
                                                  NumNumbers,
                                                  IOStat,
                                                  lNumericBlanks,
                                                  lAlphaBlanks,
                                                  cAlphaFields,
                                                  cNumericFields);
                    if (UtilityRoutines::IsNameEmpty(Alphas(1), CurrentModuleObject, ErrFound)) continue;
                    ++BCount;
                    GetSingleBranchInput(dataBranchInputManager, RoutineName, BCount, Alphas, cAlphaFields, NumAlphas, NodeNums, lAlphaBlanks);
                }

                dataBranchInputManager.NumOfBranches = BCount;
                NodeNums.deallocate();
                Alphas.deallocate();
                Numbers.deallocate();
                cAlphaFields.deallocate();
                cNumericFields.deallocate();
                lAlphaBlanks.deallocate();
                lNumericBlanks.deallocate();
                if (ErrFound) {
                    ShowSevereError(RoutineName + " Invalid " + CurrentModuleObject +
                                    " Input, preceding condition(s) will likely cause termination.");
                    dataBranchInputManager.InvalidBranchDefinitions = true;
                }
                TestInletOutletNodes(ErrFound);
                dataBranchInputManager.GetBranchInputOneTimeFlag = false;
            }
        }
    }

    void GetSingleBranchInput(BranchInputManagerData &dataBranchInputManager,
                              std::string const &RoutineName,
                              int const BCount,
                              Array1D_string &Alphas,
                              Array1D_string &cAlphaFields,
                              int const NumAlphas,
                              Array1D_int &NodeNums,
                              Array1D_bool &lAlphaBlanks)
    {
        // Using
        using CurveManager::GetPressureCurveTypeAndIndex;
        using General::RoundSigDigits;

        // Locals
        int PressureCurveType;
        int PressureCurveIndex;
        bool ErrFound;      // Flag for error detection
        int Comp;           // Loop Counter
        bool IsNotOK;       // Flag to verify name
        int NumInComps;     // Number of components actually verified (no SPLITTER or MIXER allowed)
        int ConnectionType; // Used to pass variable node connection type to GetNodeNums
        int NumNodes;       // Number of Nodes from NodeInputManager

        std::string CurrentModuleObject = "Branch";

        dataBranchInputManager.Branch(BCount).Name = Alphas(1);
        GetPressureCurveTypeAndIndex(Alphas(2), PressureCurveType, PressureCurveIndex);
        if (PressureCurveType == PressureCurve_Error) {
            ShowSevereError(RoutineName + CurrentModuleObject + "=\"" + Alphas(1) + "\", invalid data.");
            ShowContinueError("..Invalid " + cAlphaFields(2) + "=\"" + Alphas(2) + "\".");
            ShowContinueError("This curve could not be found in the input deck.  Ensure that this curve has been entered");
            ShowContinueError(" as either a Curve:Functional:PressureDrop or one of Curve:{Linear,Quadratic,Cubic,Exponent}");
            ShowContinueError("This error could be caused by a misspelled curve name");
            ErrFound = true;
        }
        dataBranchInputManager.Branch(BCount).PressureCurveType = PressureCurveType;
        dataBranchInputManager.Branch(BCount).PressureCurveIndex = PressureCurveIndex;
        dataBranchInputManager.Branch(BCount).NumOfComponents = (NumAlphas - 2) / 4;
        if (dataBranchInputManager.Branch(BCount).NumOfComponents * 4 != (NumAlphas - 2)) ++dataBranchInputManager.Branch(BCount).NumOfComponents;
        NumInComps = dataBranchInputManager.Branch(BCount).NumOfComponents;
        dataBranchInputManager.Branch(BCount).Component.allocate(dataBranchInputManager.Branch(BCount).NumOfComponents);
        Comp = 1;
        for (int Loop = 3; Loop <= NumAlphas; Loop += 4) {
            if (UtilityRoutines::SameString(Alphas(Loop), cSPLITTER) || UtilityRoutines::SameString(Alphas(Loop), cMIXER)) {
                ShowSevereError(RoutineName + CurrentModuleObject + "=\"" + Alphas(1) + "\", invalid data.");
                ShowContinueError("Connector:Splitter/Connector:Mixer not allowed in object " + CurrentModuleObject);
                ErrFound = true;
                continue;
            }
            if (Comp > NumInComps) {
                ShowSevereError(RoutineName + CurrentModuleObject + "=\"" + Alphas(1) + "\", invalid data.");
                ShowContinueError("...Number of Arguments indicate [" + RoundSigDigits(NumInComps) + "], but count of fields indicates [" +
                                  RoundSigDigits(Comp) + ']');
                ShowContinueError("...examine " + CurrentModuleObject + " carefully.");
                continue;
            }
            dataBranchInputManager.Branch(BCount).Component(Comp).CType = Alphas(Loop);
            dataBranchInputManager.Branch(BCount).Component(Comp).Name = Alphas(Loop + 1);
            ValidateComponent(Alphas(Loop), Alphas(Loop + 1), IsNotOK, CurrentModuleObject);
            if (IsNotOK) {
                ShowContinueError("Occurs on " + CurrentModuleObject + '=' + Alphas(1));
                ErrFound = true;
            }
            dataBranchInputManager.Branch(BCount).Component(Comp).InletNodeName = Alphas(Loop + 2);
            // If first component on branch, then inlet node is inlet to branch, otherwise node is internal
            if (Loop == 3) {
                ConnectionType = NodeConnectionType_Inlet;
            } else {
                ConnectionType = NodeConnectionType_Internal;
            }
            if (!lAlphaBlanks(Loop + 2)) {
                GetNodeNums(dataBranchInputManager.Branch(BCount).Component(Comp).InletNodeName,
                            NumNodes,
                            NodeNums,
                            ErrFound,
                            NodeType_Unknown,
                            CurrentModuleObject,
                            dataBranchInputManager.Branch(BCount).Name,
                            ConnectionType,
                            1,
                            ObjectIsParent,
                            _,
                            cAlphaFields(Loop + 2));
                if (NumNodes > 1) {
                    ShowSevereError(RoutineName + CurrentModuleObject + "=\"" + Alphas(1) + "\", invalid data.");
                    ShowContinueError("..invalid " + cAlphaFields(Loop + 2) + "=\"" + dataBranchInputManager.Branch(BCount).Component(Comp).InletNodeName +
                                      "\" must be a single node - appears to be a list.");
                    ShowContinueError("Occurs on " + cAlphaFields(Loop) + "=\"" + Alphas(Loop) + "\", " + cAlphaFields(Loop + 1) + "=\"" +
                                      Alphas(Loop + 1) + "\".");
                    ErrFound = true;
                } else {
                    dataBranchInputManager.Branch(BCount).Component(Comp).InletNode = NodeNums(1);
                }
            } else {
                ShowSevereError(RoutineName + CurrentModuleObject + "=\"" + Alphas(1) + "\", invalid data.");
                ShowContinueError("blank required field: " + cAlphaFields(Loop + 2));
                ShowContinueError("Occurs on " + cAlphaFields(Loop) + "=\"" + Alphas(Loop) + "\", " + cAlphaFields(Loop + 1) + "=\"" +
                                  Alphas(Loop + 1) + "\".");
                ErrFound = true;
            }
            dataBranchInputManager.Branch(BCount).Component(Comp).OutletNodeName = Alphas(Loop + 3);
            // If last component on branch, then outlet node is outlet from branch, otherwise node is internal
            if (Loop == NumAlphas - 3) {
                ConnectionType = NodeConnectionType_Outlet;
            } else {
                ConnectionType = NodeConnectionType_Internal;
            }
            if (!lAlphaBlanks(Loop + 3)) {
                GetNodeNums(dataBranchInputManager.Branch(BCount).Component(Comp).OutletNodeName,
                            NumNodes,
                            NodeNums,
                            ErrFound,
                            NodeType_Unknown,
                            CurrentModuleObject,
                            dataBranchInputManager.Branch(BCount).Name,
                            ConnectionType,
                            1,
                            ObjectIsParent,
                            _,
                            cAlphaFields(Loop + 3));
                if (NumNodes > 1) {
                    ShowSevereError(RoutineName + CurrentModuleObject + "=\"" + Alphas(1) + "\", invalid data.");
                    ShowContinueError("..invalid " + cAlphaFields(Loop + 2) + "=\"" + dataBranchInputManager.Branch(BCount).Component(Comp).InletNodeName +
                                      "\" must be a single node - appears to be a list.");
                    ShowContinueError("Occurs on " + cAlphaFields(Loop) + "=\"" + Alphas(Loop) + "\", " + cAlphaFields(Loop + 1) + "=\"" +
                                      Alphas(Loop + 1) + "\".");
                    ErrFound = true;
                } else {
                    dataBranchInputManager.Branch(BCount).Component(Comp).OutletNode = NodeNums(1);
                }
            } else {
                ShowSevereError(RoutineName + CurrentModuleObject + "=\"" + Alphas(1) + "\", invalid data.");
                ShowContinueError("blank required field: " + cAlphaFields(Loop + 3));
                ShowContinueError("Occurs on " + cAlphaFields(Loop) + "=\"" + Alphas(Loop) + "\", " + cAlphaFields(Loop + 1) + "=\"" +
                                  Alphas(Loop + 1) + "\".");
                ErrFound = true;
            }

            if (!lAlphaBlanks(Loop) && !lAlphaBlanks(Loop + 1) && !lAlphaBlanks(Loop + 2) && !lAlphaBlanks(Loop + 3))
                SetUpCompSets(CurrentModuleObject,
                              dataBranchInputManager.Branch(BCount).Name,
                              Alphas(Loop),
                              Alphas(Loop + 1),
                              Alphas(Loop + 2),
                              Alphas(Loop + 3)); // no blanks in required field set

            ++Comp;
        }
        dataBranchInputManager.Branch(BCount).NumOfComponents = NumInComps;
    }

    void GetBranchListInput(BranchInputManagerData &dataBranchInputManager)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Linda K. Lawrie
        //       DATE WRITTEN   July 2003
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine gets the branch list input and fills up the structures for
        // branch lists.
        // This subroutine gets the input for the following IDD structure:
        // BRANCH LIST,
        //  \extensible:1 Just duplicate last field and \ comments (changing numbering, please)
        //  \memo Branches MUST be listed in flow order: inlet branch, then parallel branches, then outlet branch.
        //  \memo Branches are simulated in the order listed.  Branch names cannot be duplicated within a single branch list.
        //    A1, \field Branch List Name
        //        \required-field
        //        \reference BranchLists
        //    A2, \field Branch Name 1
        //        \required-field
        //        \type object-list
        //        \object-list Branches
        //    A3, \field Branch Name 2
        //        \type object-list
        //        \object-list Branches

        // Using/Aliasing
        using General::TrimSigDigits;

        // SUBROUTINE PARAMETER DEFINITIONS:
        static std::string const RoutineName("GetBranchListInput: ");

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int Count;     // Loop Counter
        int BCount;    // Actual Branch List Count
        int Loop;      // Loop Counter
        int Found;     // Points to correct Branch List/Branch
        bool ErrFound; // True when error has occurred (cannot find Branch List)
        // Following are needed because routine calls GetBranchInput
        // which would overwrite the module Alphas and NumAlphas
        int NumAlphas;         // Used to retrieve Branch list from IDF
        Array1D_string Alphas; // Used to retrieve names from IDF
        int NumNumbers;
        Array1D<Real64> Numbers; // Not used in this object
        Array1D_string cAlphaFields;
        Array1D_string cNumericFields;
        Array1D_bool lNumericBlanks;
        Array1D_bool lAlphaBlanks;
        int IOStat; // Could be used in the Get Routines, not currently checked
        int NumParams;
        std::string TestName;

        ErrFound = false;
        std::string CurrentModuleObject = "BranchList";
        dataBranchInputManager.NumOfBranchLists = inputProcessor->getNumObjectsFound(CurrentModuleObject);
        dataBranchInputManager.BranchList.allocate(dataBranchInputManager.NumOfBranchLists);
        for (auto &e : dataBranchInputManager.BranchList) {
            e.LoopName.clear();
            e.LoopType.clear();
        }
        inputProcessor->getObjectDefMaxArgs(CurrentModuleObject, NumParams, NumAlphas, NumNumbers);
        Alphas.allocate(NumAlphas);
        Numbers.dimension(NumNumbers, 0.0);
        cAlphaFields.allocate(NumAlphas);
        cNumericFields.allocate(NumNumbers);
        lAlphaBlanks.dimension(NumAlphas, true);
        lNumericBlanks.dimension(NumNumbers, true);

        if (NumNumbers > 0) {
            ShowSevereError(RoutineName + CurrentModuleObject +
                            " Object definition contains numbers, cannot be decoded by GetBranchListInput routine.");
            ErrFound = true;
        }
        BCount = 0;
        for (Count = 1; Count <= dataBranchInputManager.NumOfBranchLists; ++Count) {
            CurrentModuleObject = "BranchList";
            inputProcessor->getObjectItem(CurrentModuleObject,
                                          Count,
                                          Alphas,
                                          NumAlphas,
                                          Numbers,
                                          NumNumbers,
                                          IOStat,
                                          lNumericBlanks,
                                          lAlphaBlanks,
                                          cAlphaFields,
                                          cNumericFields);
            if (UtilityRoutines::IsNameEmpty(Alphas(1), CurrentModuleObject, ErrFound)) continue;

            ++BCount;
            dataBranchInputManager.BranchList(BCount).Name = Alphas(1);
            dataBranchInputManager.BranchList(BCount).NumOfBranchNames = NumAlphas - 1;
            dataBranchInputManager.BranchList(BCount).BranchNames.allocate(NumAlphas - 1);
            if (dataBranchInputManager.BranchList(BCount).NumOfBranchNames == 0) {
                ShowSevereError(RoutineName + CurrentModuleObject + "=\"" + dataBranchInputManager.BranchList(BCount).Name + "\", No branch names entered.");
                ErrFound = true;
            } else {
                dataBranchInputManager.BranchList(BCount).BranchNames({1, NumAlphas - 1}) = Alphas({2, NumAlphas});
                for (Loop = 1; Loop <= dataBranchInputManager.BranchList(BCount).NumOfBranchNames; ++Loop) {
                    // If NumOfBranches = 0 then Branches havent been read yet.
                    if (dataBranchInputManager.NumOfBranches == 0) {
                        GetBranchInput(dataBranchInputManager);
                    }
                    if (!dataBranchInputManager.BranchList(BCount).BranchNames(Loop).empty()) {
                        Found = UtilityRoutines::FindItemInList(dataBranchInputManager.BranchList(BCount).BranchNames(Loop), dataBranchInputManager.Branch);
                        if (Found == 0) {
                            ShowSevereError(RoutineName + CurrentModuleObject + "=\"" + dataBranchInputManager.BranchList(BCount).Name + "\", invalid data.");
                            ShowContinueError("..invalid Branch Name not found=\"" + dataBranchInputManager.BranchList(BCount).BranchNames(Loop) + "\".");
                            ErrFound = true;
                        }
                    }
                }
            }
        }

        // Check for duplicate names specified in Branch Lists
        for (Count = 1; Count <= dataBranchInputManager.NumOfBranchLists; ++Count) {
            if (dataBranchInputManager.BranchList(Count).NumOfBranchNames == 0) continue;
            TestName = dataBranchInputManager.BranchList(Count).BranchNames(1);
            for (Loop = 2; Loop <= dataBranchInputManager.BranchList(Count).NumOfBranchNames; ++Loop) {
                if (TestName != dataBranchInputManager.BranchList(Count).BranchNames(Loop)) continue;
                ShowSevereError(RoutineName + CurrentModuleObject + "=\"" + dataBranchInputManager.BranchList(BCount).Name + "\", invalid data.");
                ShowContinueError("..invalid: duplicate branch name specified in the list.");
                ShowContinueError("..Branch Name=" + TestName);
                ShowContinueError("..Branch Name #" + TrimSigDigits(Loop) + " is duplicate.");
                ErrFound = true;
            }
        }

        if (ErrFound) {
            ShowSevereError(RoutineName + " Invalid Input -- preceding condition(s) will likely cause termination.");
        }
        dataBranchInputManager.NumOfBranchLists = BCount;
        Alphas.deallocate();
        Numbers.deallocate();
        cAlphaFields.deallocate();
        cNumericFields.deallocate();
        lAlphaBlanks.deallocate();
        lNumericBlanks.deallocate();
    }

    void GetConnectorListInput(BranchInputManagerData &dataBranchInputManager)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Linda K. Lawrie
        //       DATE WRITTEN   October 1999
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Obtains connector list input from IDF.
        // ConnectorList,
        //         \memo only two connectors allowed per loop
        //         \memo if two entered, one must be Connector:Splitter and one must be Connector:Mixer
        //     A1, \field Name
        //         \required-field
        //         \reference ConnectorLists
        //     A2, \field Connector 1 Object Type
        //         \required-field
        //         \key Connector:Splitter
        //         \key Connector:Mixer
        //     A3, \field Connector 1 Name
        //         \required-field
        //     A4, \field Connector 2 Object Type
        //         \key Connector:Splitter
        //         \key Connector:Mixer
        //     A5; \field Connector 2 Name
        //  This is in the process of possibly being extended, thus the code herein.

        // METHODOLOGY EMPLOYED:
        // na

        // REFERENCES:
        // na

        // Using/Aliasing

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:
        // na

        // SUBROUTINE PARAMETER DEFINITIONS:
        // na

        // INTERFACE BLOCK SPECIFICATIONS
        // na

        // DERIVED TYPE DEFINITIONS
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int Count;               // Loop Counter
        int NumAlphas;           // Used to retrieve names from IDF
        Array1D_string Alphas;   // Used to retrieve names from IDF
        int NumNumbers;          // Used to retrieve numbers from IDF
        Array1D<Real64> Numbers; // Used to retrieve numbers from IDF
        Array1D_string cAlphaFields;
        Array1D_string cNumericFields;
        Array1D_bool lNumericBlanks;
        Array1D_bool lAlphaBlanks;
        int IOStat; // Could be used in the Get Routines, not currently checked
        int NumParams;
        int NumConnectors;
        int CCount;
        int Arg;
        int SplitNum;
        int MixerNum;
        Array1D_string BranchNames;
        int NumBranchNames;
        bool ErrorsFound;
        int Loop;
        int Loop1;
        int Loop2;
        bool CurMixer;
        bool CurSplitter;
        int TestNum;
        bool MatchFound;

        if (!dataBranchInputManager.GetConnectorListInputFlag) return;
        ErrorsFound = false;
        std::string CurrentModuleObject = "ConnectorList";
        dataBranchInputManager.NumOfConnectorLists = inputProcessor->getNumObjectsFound(CurrentModuleObject);
        dataBranchInputManager.ConnectorLists.allocate(dataBranchInputManager.NumOfConnectorLists);
        inputProcessor->getObjectDefMaxArgs(CurrentModuleObject, NumParams, NumAlphas, NumNumbers);
        if (NumAlphas != 5 || NumNumbers != 0) {
            ShowWarningError("GetConnectorList: Illegal \"extension\" to " + CurrentModuleObject +
                             " object. Internal code does not support > 2 connectors (Connector:Splitter and Connector:Mixer)");
        }
        Alphas.allocate(NumAlphas);
        Numbers.dimension(NumNumbers, 0.0);
        cAlphaFields.allocate(NumAlphas);
        cNumericFields.allocate(NumNumbers);
        lAlphaBlanks.dimension(NumAlphas, true);
        lNumericBlanks.dimension(NumNumbers, true);
        for (Count = 1; Count <= dataBranchInputManager.NumOfConnectorLists; ++Count) {
            inputProcessor->getObjectItem(CurrentModuleObject,
                                          Count,
                                          Alphas,
                                          NumAlphas,
                                          Numbers,
                                          NumNumbers,
                                          IOStat,
                                          lNumericBlanks,
                                          lAlphaBlanks,
                                          cAlphaFields,
                                          cNumericFields);
            dataBranchInputManager.ConnectorLists(Count).Name = Alphas(1);
            NumConnectors = (NumAlphas - 1) / 2; // potential problem if puts in type but not name
            if (mod(NumAlphas - 1, 2) != 0) ++NumConnectors;
            dataBranchInputManager.ConnectorLists(Count).NumOfConnectors = NumConnectors;
            dataBranchInputManager.ConnectorLists(Count).ConnectorType.allocate(NumConnectors);
            dataBranchInputManager.ConnectorLists(Count).ConnectorName.allocate(NumConnectors);
            dataBranchInputManager.ConnectorLists(Count).ConnectorMatchNo.allocate(NumConnectors);
            dataBranchInputManager.ConnectorLists(Count).ConnectorType = "UNKNOWN";
            dataBranchInputManager.ConnectorLists(Count).ConnectorName = "UNKNOWN";
            dataBranchInputManager.ConnectorLists(Count).ConnectorMatchNo = 0;
            dataBranchInputManager.ConnectorLists(Count).NumOfSplitters = 0;
            dataBranchInputManager.ConnectorLists(Count).NumOfMixers = 0;

            CCount = 0;
            for (Arg = 2; Arg <= NumAlphas; Arg += 2) {
                ++CCount;
                if (UtilityRoutines::SameString(Alphas(Arg), cSPLITTER)) {
                    dataBranchInputManager.ConnectorLists(Count).ConnectorType(CCount) = Alphas(Arg).substr(0, 30);
                    ++dataBranchInputManager.ConnectorLists(Count).NumOfSplitters;
                } else if (UtilityRoutines::SameString(Alphas(Arg), cMIXER)) {
                    dataBranchInputManager.ConnectorLists(Count).ConnectorType(CCount) = Alphas(Arg).substr(0, 30);
                    ++dataBranchInputManager.ConnectorLists(Count).NumOfMixers;
                } else {
                    ShowWarningError("GetConnectorListInput: Invalid " + cAlphaFields(Arg) + '=' + Alphas(Arg) + " in " + CurrentModuleObject + '=' +
                                     Alphas(1));
                }
                dataBranchInputManager.ConnectorLists(Count).ConnectorName(CCount) = Alphas(Arg + 1);
            }
        }
        dataBranchInputManager.GetConnectorListInputFlag = false;
        Alphas.deallocate();
        Numbers.deallocate();
        cAlphaFields.deallocate();
        cNumericFields.deallocate();
        lAlphaBlanks.deallocate();
        lNumericBlanks.deallocate();

        // Validity checks on Connector Lists
        if (dataBranchInputManager.GetSplitterInputFlag) {
            GetSplitterInput(dataBranchInputManager);
            dataBranchInputManager.GetSplitterInputFlag = false;
        }
        if (dataBranchInputManager.GetMixerInputFlag) {
            GetMixerInput(dataBranchInputManager);
            dataBranchInputManager.GetMixerInputFlag = false;
        }

        SplitNum = 0;
        MixerNum = 0;
        for (Count = 1; Count <= dataBranchInputManager.NumOfConnectorLists; ++Count) {
            if (dataBranchInputManager.ConnectorLists(Count).NumOfConnectors <= 1) continue; // Air Loop only has one.
            if (dataBranchInputManager.ConnectorLists(Count).NumOfConnectors > 2) continue;  // Rules not clear for this case
            for (Loop = 1; Loop <= dataBranchInputManager.ConnectorLists(Count).NumOfConnectors; ++Loop) {
                if (dataBranchInputManager.ConnectorLists(Count).ConnectorMatchNo(Loop) != 0) continue;
                if (UtilityRoutines::SameString(dataBranchInputManager.ConnectorLists(Count).ConnectorType(Loop), cSPLITTER)) {
                    CurSplitter = true;
                    CurMixer = false;
                    SplitNum = UtilityRoutines::FindItemInList(dataBranchInputManager.ConnectorLists(Count).ConnectorName(Loop), dataBranchInputManager.Splitters);
                    // Following code sets up branch names to be matched from Splitter/Mixer data structure
                    if (SplitNum == 0) {
                        ShowSevereError("Invalid Connector:Splitter(none)=" + dataBranchInputManager.ConnectorLists(Count).ConnectorName(Loop) + ", referenced by " +
                                        CurrentModuleObject + '=' + dataBranchInputManager.ConnectorLists(Count).Name);
                        ErrorsFound = true;
                        continue;
                    }
                    NumBranchNames = dataBranchInputManager.Splitters(SplitNum).NumOutletBranches;
                    BranchNames = dataBranchInputManager.Splitters(SplitNum).OutletBranchNames;
                } else if (UtilityRoutines::SameString(dataBranchInputManager.ConnectorLists(Count).ConnectorType(Loop), cMIXER)) {
                    CurSplitter = true;
                    CurMixer = false;
                    MixerNum = UtilityRoutines::FindItemInList(dataBranchInputManager.ConnectorLists(Count).ConnectorName(Loop), dataBranchInputManager.Mixers);
                    if (MixerNum == 0) {
                        ShowSevereError("Invalid Connector:Mixer(none)=" + dataBranchInputManager.ConnectorLists(Count).ConnectorName(Loop) + ", referenced by " +
                                        CurrentModuleObject + '=' + dataBranchInputManager.ConnectorLists(Count).Name);
                        ErrorsFound = true;
                        continue;
                    }
                    NumBranchNames = dataBranchInputManager.Mixers(MixerNum).NumInletBranches;
                    BranchNames = dataBranchInputManager.Mixers(MixerNum).InletBranchNames;
                } else {
                    continue;
                }
                // Try to match mixer to splitter
                for (Loop1 = Loop + 1; Loop1 <= dataBranchInputManager.ConnectorLists(Count).NumOfConnectors; ++Loop1) {
                    if (CurMixer && !UtilityRoutines::SameString(dataBranchInputManager.ConnectorLists(Count).ConnectorType(Loop1), cSPLITTER)) continue;
                    if (CurSplitter && !UtilityRoutines::SameString(dataBranchInputManager.ConnectorLists(Count).ConnectorType(Loop1), cMIXER)) continue;
                    if (dataBranchInputManager.ConnectorLists(Count).ConnectorMatchNo(Loop1) != 0) continue;
                    {
                        auto const SELECT_CASE_var(CurSplitter);
                        if (SELECT_CASE_var) {
                            // Current "item" is a splitter, candidate is a mixer.
                            MixerNum = UtilityRoutines::FindItemInList(dataBranchInputManager.ConnectorLists(Count).ConnectorName(Loop1), dataBranchInputManager.Mixers);
                            if (MixerNum == 0) continue;
                            if (dataBranchInputManager.Mixers(MixerNum).NumInletBranches != NumBranchNames) continue;
                            MatchFound = true;
                            for (Loop2 = 1; Loop2 <= dataBranchInputManager.Mixers(MixerNum).NumInletBranches; ++Loop2) {
                                TestNum = UtilityRoutines::FindItemInList(dataBranchInputManager.Mixers(MixerNum).InletBranchNames(Loop2), BranchNames, NumBranchNames);
                                if (TestNum == 0) {
                                    MatchFound = false;
                                    break;
                                }
                            }
                            if (MatchFound) {
                                dataBranchInputManager.ConnectorLists(Count).ConnectorMatchNo(Loop1) = MixerNum;
                                dataBranchInputManager.ConnectorLists(Count).ConnectorMatchNo(Loop) = SplitNum;
                            }
                        } else {
                            // Current "item" is a splitter, candidate is a mixer.
                            SplitNum = UtilityRoutines::FindItemInList(dataBranchInputManager.ConnectorLists(Count).ConnectorName(Loop1), dataBranchInputManager.Splitters);
                            if (SplitNum == 0) continue;
                            if (dataBranchInputManager.Splitters(SplitNum).NumOutletBranches != NumBranchNames) continue;
                            MatchFound = true;
                            for (Loop2 = 1; Loop2 <= dataBranchInputManager.Splitters(SplitNum).NumOutletBranches; ++Loop2) {
                                TestNum = UtilityRoutines::FindItemInList(dataBranchInputManager.Splitters(SplitNum).OutletBranchNames(Loop2), BranchNames, NumBranchNames);
                                if (TestNum == 0) {
                                    MatchFound = false;
                                    break;
                                }
                            }
                            if (MatchFound) {
                                dataBranchInputManager.ConnectorLists(Count).ConnectorMatchNo(Loop1) = SplitNum;
                                dataBranchInputManager.ConnectorLists(Count).ConnectorMatchNo(Loop) = MixerNum;
                            }
                        }
                    }
                }
                BranchNames.deallocate();
            }
        }

        for (Count = 1; Count <= dataBranchInputManager.NumOfConnectorLists; ++Count) {
            if (dataBranchInputManager.ConnectorLists(Count).NumOfConnectors <= 1) continue; // Air Loop only has one.
            if (dataBranchInputManager.ConnectorLists(Count).NumOfConnectors > 2) continue;  // Rules not clear
            for (Loop = 1; Loop <= dataBranchInputManager.ConnectorLists(Count).NumOfConnectors; ++Loop) {
                if (dataBranchInputManager.ConnectorLists(Count).ConnectorMatchNo(Loop) != 0) continue;
                //  = 0, not matched.
                ShowSevereError("For " + CurrentModuleObject + '=' + dataBranchInputManager.ConnectorLists(Count).Name);
                ShowContinueError("...Item=" + dataBranchInputManager.ConnectorLists(Count).ConnectorName(Loop) + ", Type=" + dataBranchInputManager.ConnectorLists(Count).ConnectorType(Loop) +
                                  " was not matched.");
                if (UtilityRoutines::SameString(dataBranchInputManager.ConnectorLists(Count).ConnectorType(Loop), "Connector:Splitter")) {
                    ShowContinueError(
                        "The BranchList for this Connector:Splitter does not match the BranchList for its corresponding Connector:Mixer.");
                } else {
                    ShowContinueError(
                        "The BranchList for this Connector:Mixer does not match the BranchList for its corresponding Connector:Splitter.");
                }
                ErrorsFound = true;
            }
        }

        if (ErrorsFound) {
            ShowFatalError("GetConnectorListInput: Program terminates for preceding conditions.");
        }
    }

    void GetSplitterInput(BranchInputManagerData &dataBranchInputManager)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Linda Lawrie
        //       DATE WRITTEN   Sept 2005 (moved from GetLoopSplitter)
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Gets the Splitter data that is used in Loops.
        // IDD structure:
        // Connector:Splitter,
        //   \min-fields 3
        //        \extensible:1 Just duplicate last field and \ comments (changing numbering, please)
        //        \memo Split one air/water stream into N outlet streams.  Branch names cannot be duplicated
        //        \memo within a single Splitter list.
        //    A1, \field Name
        //         \required-field
        //    A2, \field Inlet Branch Name
        //         \required-field
        //         \type object-list
        //         \object-list Branches
        //    A3, \field Outlet Branch 1 Name
        //         \required-field
        //         \type object-list
        //         \object-list Branches
        //    A4, \field Outlet Branch 2 Name
        //         \type object-list
        //         \object-list Branches

        // Using/Aliasing
        using General::TrimSigDigits;

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int NumAlphas;           // Used to retrieve names from IDF
        Array1D_string Alphas;   // Used to retrieve names from IDF
        int NumNumbers;          // Used to retrieve numbers from IDF
        Array1D<Real64> Numbers; // Used to retrieve numbers from IDF
        Array1D_string cAlphaFields;
        Array1D_string cNumericFields;
        Array1D_bool lNumericBlanks;
        Array1D_bool lAlphaBlanks;
        int IOStat; // Could be used in the Get Routines, not currently checked
        int NumParams;
        int Loop;
        int Loop1;
        int Count;
        int Found;
        bool ErrorsFound(false);
        std::string TestName;
        std::string BranchListName;
        std::string FoundSupplyDemandAir;
        std::string SaveSupplyDemandAir;
        std::string FoundLoop;
        std::string SaveLoop;
        bool MatchedLoop;

        if (!dataBranchInputManager.GetSplitterInputFlag) return;
        std::string CurrentModuleObject = cSPLITTER;
        dataBranchInputManager.NumSplitters = inputProcessor->getNumObjectsFound(CurrentModuleObject);
        dataBranchInputManager.Splitters.allocate(dataBranchInputManager.NumSplitters);
        inputProcessor->getObjectDefMaxArgs(CurrentModuleObject, NumParams, NumAlphas, NumNumbers);
        Alphas.allocate(NumAlphas);
        Numbers.dimension(NumNumbers, 0.0);
        cAlphaFields.allocate(NumAlphas);
        cNumericFields.allocate(NumNumbers);
        lAlphaBlanks.dimension(NumAlphas, true);
        lNumericBlanks.dimension(NumNumbers, true);
        for (Count = 1; Count <= dataBranchInputManager.NumSplitters; ++Count) {
            inputProcessor->getObjectItem(CurrentModuleObject,
                                          Count,
                                          Alphas,
                                          NumAlphas,
                                          Numbers,
                                          NumNumbers,
                                          IOStat,
                                          lNumericBlanks,
                                          lAlphaBlanks,
                                          cAlphaFields,
                                          cNumericFields);
            dataBranchInputManager.Splitters(Count).Name = Alphas(1);
            dataBranchInputManager.Splitters(Count).InletBranchName = Alphas(2);
            dataBranchInputManager.Splitters(Count).NumOutletBranches = NumAlphas - 2;
            dataBranchInputManager.Splitters(Count).OutletBranchNames.allocate(dataBranchInputManager.Splitters(Count).NumOutletBranches);
            for (Loop = 1; Loop <= dataBranchInputManager.Splitters(Count).NumOutletBranches; ++Loop) {
                dataBranchInputManager.Splitters(Count).OutletBranchNames(Loop) = Alphas(2 + Loop);
            }
        }
        dataBranchInputManager.GetSplitterInputFlag = false;
        Alphas.deallocate();
        Numbers.deallocate();
        cAlphaFields.deallocate();
        cNumericFields.deallocate();
        lAlphaBlanks.deallocate();
        lNumericBlanks.deallocate();

        // More validity -- check splitter "names" against branches.
        if (!dataBranchInputManager.GetBranchInputFlag) {
            GetBranchInput(dataBranchInputManager);
            dataBranchInputManager.GetBranchInputFlag = false;
        }
        for (Count = 1; Count <= dataBranchInputManager.NumSplitters; ++Count) {
            Found = UtilityRoutines::FindItemInList(dataBranchInputManager.Splitters(Count).InletBranchName, dataBranchInputManager.Branch);
            if (Found == 0) {
                ShowSevereError("GetSplitterInput: Invalid Branch=" + dataBranchInputManager.Splitters(Count).InletBranchName + ", referenced as Inlet Branch to " +
                                CurrentModuleObject + '=' + dataBranchInputManager.Splitters(Count).Name);
                ErrorsFound = true;
            }
            for (Loop = 1; Loop <= dataBranchInputManager.Splitters(Count).NumOutletBranches; ++Loop) {
                Found = UtilityRoutines::FindItemInList(dataBranchInputManager.Splitters(Count).OutletBranchNames(Loop), dataBranchInputManager.Branch);
                if (Found == 0) {
                    ShowSevereError("GetSplitterInput: Invalid Branch=" + dataBranchInputManager.Splitters(Count).OutletBranchNames(Loop) +
                                    ", referenced as Outlet Branch # " + TrimSigDigits(Loop) + " to " + CurrentModuleObject + '=' +
                                    dataBranchInputManager.Splitters(Count).Name);
                    ErrorsFound = true;
                }
            }
        }

        // Check for duplicate names specified in Splitters
        for (Count = 1; Count <= dataBranchInputManager.NumSplitters; ++Count) {
            TestName = dataBranchInputManager.Splitters(Count).InletBranchName;
            for (Loop = 1; Loop <= dataBranchInputManager.Splitters(Count).NumOutletBranches; ++Loop) {
                if (TestName != dataBranchInputManager.Splitters(Count).OutletBranchNames(Loop)) continue;
                ShowSevereError(CurrentModuleObject + '=' + dataBranchInputManager.Splitters(Count).Name + " specifies an outlet node name the same as the inlet node.");
                ShowContinueError("..Inlet Node=" + TestName);
                ShowContinueError("..Outlet Node #" + TrimSigDigits(Loop) + " is duplicate.");
                ErrorsFound = true;
            }
            for (Loop = 1; Loop <= dataBranchInputManager.Splitters(Count).NumOutletBranches; ++Loop) {
                for (Loop1 = Loop + 1; Loop1 <= dataBranchInputManager.Splitters(Count).NumOutletBranches; ++Loop1) {
                    if (dataBranchInputManager.Splitters(Count).OutletBranchNames(Loop) != dataBranchInputManager.Splitters(Count).OutletBranchNames(Loop1)) continue;
                    ShowSevereError(CurrentModuleObject + '=' + dataBranchInputManager.Splitters(Count).Name + " specifies duplicate outlet nodes in its outlet node list.");
                    ShowContinueError("..Outlet Node #" + TrimSigDigits(Loop) + " Name=" + dataBranchInputManager.Splitters(Count).OutletBranchNames(Loop));
                    ShowContinueError("..Outlet Node #" + TrimSigDigits(Loop) + " is duplicate.");
                    ErrorsFound = true;
                }
            }
        }

        if (ErrorsFound) {
            ShowFatalError("GetSplitterInput: Fatal Errors Found in " + CurrentModuleObject + ", program terminates.");
        }

        //  Everything supposed to be good.  Now make sure all branches in Splitter on same side of loop.
        SaveSupplyDemandAir = std::string();
        for (Count = 1; Count <= dataBranchInputManager.NumSplitters; ++Count) {
            // 2.  Find the branch name in branchlist
            TestName = dataBranchInputManager.Splitters(Count).InletBranchName;
            BranchListName = std::string();
            for (Loop1 = 1; Loop1 <= dataBranchInputManager.NumOfBranchLists; ++Loop1) {
                if (any_eq(dataBranchInputManager.BranchList(Loop1).BranchNames, TestName)) {
                    BranchListName = dataBranchInputManager.BranchList(Loop1).Name;
                    break;
                }
            }

            if (!BranchListName.empty()) {
                FoundSupplyDemandAir = std::string();
                FoundLoop = std::string();
                MatchedLoop = false;
                // 3.  Find the loop and type
                FindAirPlantCondenserLoopFromBranchList(BranchListName, FoundLoop, FoundSupplyDemandAir, MatchedLoop);
                if (MatchedLoop) {
                    SaveSupplyDemandAir = FoundSupplyDemandAir;
                    SaveLoop = FoundLoop;
                } else {
                    ShowSevereError("GetSplitterInput: Inlet Splitter Branch=\"" + TestName + "\" and BranchList=\"" + BranchListName +
                                    "\" not matched to a Air/Plant/Condenser Loop");
                    ShowContinueError("...and therefore, not a valid Loop Splitter.");
                    ShowContinueError("..." + CurrentModuleObject + '=' + dataBranchInputManager.Splitters(Count).Name);
                    ErrorsFound = true;
                }
            } else {
                ShowSevereError("GetSplitterInput: Inlet Splitter Branch=\"" + TestName + "\" not on BranchList");
                ShowContinueError("...and therefore, not a valid Loop Splitter.");
                ShowContinueError("..." + CurrentModuleObject + '=' + dataBranchInputManager.Splitters(Count).Name);
                ErrorsFound = true;
            }
            for (Loop = 1; Loop <= dataBranchInputManager.Splitters(Count).NumOutletBranches; ++Loop) {
                TestName = dataBranchInputManager.Splitters(Count).OutletBranchNames(Loop);
                BranchListName = std::string();
                for (Loop1 = 1; Loop1 <= dataBranchInputManager.NumOfBranchLists; ++Loop1) {
                    if (any_eq(dataBranchInputManager.BranchList(Loop1).BranchNames, TestName)) {
                        BranchListName = dataBranchInputManager.BranchList(Loop1).Name;
                        break;
                    }
                }

                if (!BranchListName.empty()) {
                    FoundSupplyDemandAir = std::string();
                    FoundLoop = std::string();
                    MatchedLoop = false;
                    // 3.  Find the loop and type
                    FindAirPlantCondenserLoopFromBranchList(BranchListName, FoundLoop, FoundSupplyDemandAir, MatchedLoop);
                    if (MatchedLoop) {
                        if (SaveSupplyDemandAir != FoundSupplyDemandAir || SaveLoop != FoundLoop) {
                            ShowSevereError("GetSplitterInput: Outlet Splitter Branch=\"" + TestName + "\" does not match types of Inlet Branch.");
                            ShowContinueError("...Inlet Branch is on \"" + SaveLoop + "\" on \"" + SaveSupplyDemandAir + "\" side.");
                            ShowContinueError("...Outlet Branch is on \"" + FoundLoop + "\" on \"" + FoundSupplyDemandAir + "\" side.");
                            ShowContinueError("...All branches in Loop Splitter must be on same kind of loop and supply/demand side.");
                            ShowContinueError("..." + CurrentModuleObject + '=' + dataBranchInputManager.Splitters(Count).Name);
                            ErrorsFound = true;
                        }
                    } else {
                        ShowSevereError("GetSplitterInput: Outlet Splitter Branch=\"" + TestName + "\" and BranchList=\"" + BranchListName +
                                        "\" not matched to a Air/Plant/Condenser Loop");
                        ShowContinueError("...and therefore, not a valid Loop Splitter.");
                        ShowContinueError("..." + CurrentModuleObject + '=' + dataBranchInputManager.Splitters(Count).Name);
                        ErrorsFound = true;
                    }
                } else {
                    ShowSevereError("GetSplitterInput: Outlet Splitter Branch=\"" + TestName + "\" not on BranchList");
                    ShowContinueError("...and therefore, not a valid Loop Splitter");
                    ShowContinueError("..." + CurrentModuleObject + '=' + dataBranchInputManager.Splitters(Count).Name);
                    ErrorsFound = true;
                }
            }
        }

        if (ErrorsFound) {
            ShowFatalError("GetSplitterInput: Fatal Errors Found in " + CurrentModuleObject + ", program terminates.");
        }
    }

    void GetMixerInput(BranchInputManagerData &dataBranchInputManager)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Linda Lawrie
        //       DATE WRITTEN   Sept 2005 (moved from GetLoopMixer)
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Gets the Mixer data that is used in Loops.
        // IDD Structure:
        // Connector:Mixer,
        //   \min-fields 3
        //        \extensible:1 Just duplicate last field and \ comments (changing numbering, please)
        //        \memo Mix N inlet air/water streams into one.  Branch names cannot be duplicated within
        //        \memo a single mixer list.
        //    A1 , \field Name
        //         \required-field
        //    A2 , \field Outlet Branch Name
        //         \required-field
        //         \type object-list
        //         \object-list Branches
        //    A3 , \field Inlet Branch 1 Name
        //         \required-field
        //         \type object-list
        //         \object-list Branches
        //    A4 , \field Inlet Branch 2 Name
        //         \type object-list
        //         \object-list Branches

        // Using/Aliasing
        using General::TrimSigDigits;

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int NumAlphas;           // Used to retrieve names from IDF
        Array1D_string Alphas;   // Used to retrieve names from IDF
        int NumNumbers;          // Used to retrieve numbers from IDF
        Array1D<Real64> Numbers; // Used to retrieve numbers from IDF
        Array1D_string cAlphaFields;
        Array1D_string cNumericFields;
        Array1D_bool lNumericBlanks;
        Array1D_bool lAlphaBlanks;
        int IOStat; // Could be used in the Get Routines, not currently checked
        int NumParams;
        int Loop;
        int Loop1;
        int Count;
        int Found;
        bool ErrorsFound(false);
        std::string TestName;
        std::string BranchListName;
        std::string FoundSupplyDemandAir;
        std::string SaveSupplyDemandAir;
        std::string FoundLoop;
        std::string SaveLoop;
        bool MatchedLoop;

        if (!dataBranchInputManager.GetMixerInputFlag) return;

        std::string CurrentModuleObject = cMIXER;

        dataBranchInputManager.NumMixers = inputProcessor->getNumObjectsFound(CurrentModuleObject);
        dataBranchInputManager.Mixers.allocate(dataBranchInputManager.NumMixers);
        inputProcessor->getObjectDefMaxArgs(CurrentModuleObject, NumParams, NumAlphas, NumNumbers);
        Alphas.allocate(NumAlphas);
        Numbers.dimension(NumNumbers, 0.0);
        cAlphaFields.allocate(NumAlphas);
        cNumericFields.allocate(NumNumbers);
        lAlphaBlanks.dimension(NumAlphas, true);
        lNumericBlanks.dimension(NumNumbers, true);
        for (Count = 1; Count <= dataBranchInputManager.NumMixers; ++Count) {
            inputProcessor->getObjectItem(CurrentModuleObject,
                                          Count,
                                          Alphas,
                                          NumAlphas,
                                          Numbers,
                                          NumNumbers,
                                          IOStat,
                                          lNumericBlanks,
                                          lAlphaBlanks,
                                          cAlphaFields,
                                          cNumericFields);
            dataBranchInputManager.Mixers(Count).Name = Alphas(1);
            dataBranchInputManager.Mixers(Count).OutletBranchName = Alphas(2);
            dataBranchInputManager.Mixers(Count).NumInletBranches = NumAlphas - 2;
            dataBranchInputManager.Mixers(Count).InletBranchNames.allocate(dataBranchInputManager.Mixers(Count).NumInletBranches);
            for (Loop = 1; Loop <= dataBranchInputManager.Mixers(Count).NumInletBranches; ++Loop) {
                dataBranchInputManager.Mixers(Count).InletBranchNames(Loop) = Alphas(2 + Loop);
            }
        }
        dataBranchInputManager.GetMixerInputFlag = false;
        Alphas.deallocate();
        Numbers.deallocate();
        cAlphaFields.deallocate();
        cNumericFields.deallocate();
        lAlphaBlanks.deallocate();
        lNumericBlanks.deallocate();

        // More validity -- check mixer "names" against branches.
        if (!dataBranchInputManager.GetBranchInputFlag) {
            GetBranchInput(dataBranchInputManager);
            dataBranchInputManager.GetBranchInputFlag = false;
        }
        for (Count = 1; Count <= dataBranchInputManager.NumMixers; ++Count) {
            Found = UtilityRoutines::FindItemInList(dataBranchInputManager.Mixers(Count).OutletBranchName, dataBranchInputManager.Branch);
            if (Found == 0) {
                ShowSevereError("GetMixerInput: Invalid Branch=" + dataBranchInputManager.Mixers(Count).OutletBranchName + ", referenced as Outlet Branch in " +
                                CurrentModuleObject + '=' + dataBranchInputManager.Mixers(Count).Name);
                ErrorsFound = true;
            }
            for (Loop = 1; Loop <= dataBranchInputManager.Mixers(Count).NumInletBranches; ++Loop) {
                Found = UtilityRoutines::FindItemInList(dataBranchInputManager.Mixers(Count).InletBranchNames(Loop), dataBranchInputManager.Branch);
                if (Found == 0) {
                    ShowSevereError("GetMixerInput: Invalid Branch=" + dataBranchInputManager.Mixers(Count).InletBranchNames(Loop) + ", referenced as Inlet Branch # " +
                                    TrimSigDigits(Loop) + " in " + CurrentModuleObject + '=' + dataBranchInputManager.Mixers(Count).Name);
                    ErrorsFound = true;
                }
            }
        }

        // Check for duplicate names specified in Mixer
        for (Count = 1; Count <= dataBranchInputManager.NumMixers; ++Count) {
            TestName = dataBranchInputManager.Mixers(Count).OutletBranchName;
            for (Loop = 1; Loop <= dataBranchInputManager.Mixers(Count).NumInletBranches; ++Loop) {
                if (TestName != dataBranchInputManager.Mixers(Count).InletBranchNames(Loop)) continue;
                ShowSevereError(CurrentModuleObject + '=' + dataBranchInputManager.Mixers(Count).Name + " specifies an inlet node name the same as the outlet node.");
                ShowContinueError("..Outlet Node=" + TestName);
                ShowContinueError("..Inlet Node #" + TrimSigDigits(Loop) + " is duplicate.");
                ErrorsFound = true;
            }
            for (Loop = 1; Loop <= dataBranchInputManager.Mixers(Count).NumInletBranches; ++Loop) {
                for (Loop1 = Loop + 1; Loop1 <= dataBranchInputManager.Mixers(Count).NumInletBranches; ++Loop1) {
                    if (dataBranchInputManager.Mixers(Count).InletBranchNames(Loop) != dataBranchInputManager.Mixers(Count).InletBranchNames(Loop1)) continue;
                    ShowSevereError(CurrentModuleObject + '=' + dataBranchInputManager.Mixers(Count).Name + " specifies duplicate inlet nodes in its inlet node list.");
                    ShowContinueError("..Inlet Node #" + TrimSigDigits(Loop) + " Name=" + dataBranchInputManager.Mixers(Count).InletBranchNames(Loop));
                    ShowContinueError("..Inlet Node #" + TrimSigDigits(Loop) + " is duplicate.");
                    ErrorsFound = true;
                }
            }
        }

        if (ErrorsFound) {
            ShowFatalError("GetMixerInput: Fatal Errors Found in " + CurrentModuleObject + ", program terminates.");
        }

        //  Everything supposed to be good.  Now make sure all branches in Splitter on same side of loop.
        SaveSupplyDemandAir = std::string();
        for (Count = 1; Count <= dataBranchInputManager.NumMixers; ++Count) {
            // 2.  Find the branch name in branchlist
            TestName = dataBranchInputManager.Mixers(Count).OutletBranchName;
            BranchListName = std::string();
            for (Loop1 = 1; Loop1 <= dataBranchInputManager.NumOfBranchLists; ++Loop1) {
                if (any_eq(dataBranchInputManager.BranchList(Loop1).BranchNames, TestName)) {
                    BranchListName = dataBranchInputManager.BranchList(Loop1).Name;
                    break;
                }
            }

            if (!BranchListName.empty()) {
                FoundSupplyDemandAir = std::string();
                FoundLoop = std::string();
                MatchedLoop = false;
                // 3.  Find the loop and type
                FindAirPlantCondenserLoopFromBranchList(BranchListName, FoundLoop, FoundSupplyDemandAir, MatchedLoop);
                if (MatchedLoop) {
                    SaveSupplyDemandAir = FoundSupplyDemandAir;
                    SaveLoop = FoundLoop;
                } else {
                    ShowSevereError("GetMixerInput: Outlet Mixer Branch=\"" + TestName + "\" and BranchList=\"" + BranchListName +
                                    "\" not matched to a Air/Plant/Condenser Loop");
                    ShowContinueError("...and therefore, not a valid Loop Mixer.");
                    ShowContinueError("..." + CurrentModuleObject + '=' + dataBranchInputManager.Mixers(Count).Name);
                    ErrorsFound = true;
                }
            } else {
                ShowSevereError("GetMixerInput: Outlet Mixer Branch=\"" + TestName + "\" not on BranchList");
                ShowContinueError("...and therefore, not a valid Loop Mixer.");
                ShowContinueError("..." + CurrentModuleObject + '=' + dataBranchInputManager.Mixers(Count).Name);
                ErrorsFound = true;
            }
            for (Loop = 1; Loop <= dataBranchInputManager.Mixers(Count).NumInletBranches; ++Loop) {
                TestName = dataBranchInputManager.Mixers(Count).InletBranchNames(Loop);
                BranchListName = std::string();
                for (Loop1 = 1; Loop1 <= dataBranchInputManager.NumOfBranchLists; ++Loop1) {
                    if (any_eq(dataBranchInputManager.BranchList(Loop1).BranchNames, TestName)) {
                        BranchListName = dataBranchInputManager.BranchList(Loop1).Name;
                        break;
                    }
                }

                if (!BranchListName.empty()) {
                    FoundSupplyDemandAir = std::string();
                    FoundLoop = std::string();
                    MatchedLoop = false;
                    // 3.  Find the plant loop and type
                    FindAirPlantCondenserLoopFromBranchList(BranchListName, FoundLoop, FoundSupplyDemandAir, MatchedLoop);
                    if (MatchedLoop) {
                        if (SaveSupplyDemandAir != FoundSupplyDemandAir || SaveLoop != FoundLoop) {
                            ShowSevereError("GetMixerInput: Outlet Mixer Branch=\"" + TestName + "\" does not match types of Inlet Branch.");
                            ShowContinueError("...Outlet Branch is on \"" + SaveLoop + "\" on \"" + SaveSupplyDemandAir + "\" side.");
                            ShowContinueError("...Inlet Branch is on \"" + FoundLoop + "\" on \"" + FoundSupplyDemandAir + "\" side.");
                            ShowContinueError("...All branches in Loop Mixer must be on same kind of loop and supply/demand side.");
                            ShowContinueError("..." + CurrentModuleObject + '=' + dataBranchInputManager.Mixers(Count).Name);
                            ErrorsFound = true;
                        }
                    } else {
                        ShowSevereError("GetMixerInput: Inlet Mixer Branch=\"" + TestName + "\" and BranchList=\"" + BranchListName +
                                        "\" not matched to a Air/Plant/Condenser Loop");
                        ShowContinueError("...and therefore, not a valid Loop Mixer.");
                        ShowContinueError("..." + CurrentModuleObject + '=' + dataBranchInputManager.Mixers(Count).Name);
                        ErrorsFound = true;
                    }
                } else {
                    ShowSevereError("GetMixerInput: Inlet Mixer Branch=\"" + TestName + "\" not on BranchList");
                    ShowContinueError("...and therefore, not a valid Loop Mixer");
                    ShowContinueError("..." + CurrentModuleObject + '=' + dataBranchInputManager.Mixers(Count).Name);
                    ErrorsFound = true;
                }
            }
        }

        if (ErrorsFound) {
            ShowFatalError("GetMixerInput: Fatal Errors Found in " + CurrentModuleObject + ", program terminates.");
        }
    }

    void FindPlantLoopBranchConnection(std::string const &BranchListName,
                                       std::string &FoundPlantLoopName,
                                       int &FoundPlantLoopNum,
                                       std::string &FoundSupplyDemand,
                                       Real64 &FoundVolFlowRate,
                                       bool &MatchedPlantLoop)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Linda Lawrie
        //       DATE WRITTEN   October 2007
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // An auxiliary routine locate a plant loop and type from a BranchListName

        // METHODOLOGY EMPLOYED:
        // Calls GetObject for PLANT LOOP

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int Num;
        int NumPlantLoops;
        int NumParams;
        Array1D_string Alphas;
        int NumAlphas;
        Array1D<Real64> Numbers;
        int NumNumbers;
        int IOStat;

        // Get Inputs
        std::string CurrentModuleObject = "PlantLoop";

        NumPlantLoops = inputProcessor->getNumObjectsFound(CurrentModuleObject);
        inputProcessor->getObjectDefMaxArgs(CurrentModuleObject, NumParams, NumAlphas, NumNumbers);
        Alphas.allocate(NumAlphas);
        Numbers.allocate(NumNumbers);

        for (Num = 1; Num <= NumPlantLoops; ++Num) {
            inputProcessor->getObjectItem(CurrentModuleObject, Num, Alphas, NumAlphas, Numbers, NumNumbers, IOStat);
            // Only looking for BranchList here.
            if (Alphas(8) == BranchListName) {
                FoundPlantLoopName = Alphas(1);
                FoundSupplyDemand = "Supply";
                FoundVolFlowRate = Numbers(3);
                FoundPlantLoopNum = Num;
                MatchedPlantLoop = true;
                break;
            } else if (Alphas(12) == BranchListName) {
                FoundPlantLoopName = Alphas(1);
                FoundSupplyDemand = "Demand";
                FoundVolFlowRate = Numbers(3);
                FoundPlantLoopNum = Num;
                MatchedPlantLoop = true;
                break;
            }
        }

        Alphas.deallocate();
        Numbers.deallocate();
    }

    void FindCondenserLoopBranchConnection(std::string const &BranchListName,
                                           std::string &FoundCondLoopName,
                                           int &FoundCondLoopNum,
                                           std::string &FoundSupplyDemand,
                                           Real64 &FoundVolFlowRate,
                                           bool &MatchedCondLoop)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Linda Lawrie
        //       DATE WRITTEN   February 2008
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // An auxiliary routine locate a condenser loop and type from a BranchListName

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int Num;
        int NumCondLoops;
        int NumParams;
        Array1D_string Alphas;
        int NumAlphas;
        Array1D<Real64> Numbers;
        int NumNumbers;
        int IOStat;

        // Get Inputs
        std::string CurrentModuleObject = "CondenserLoop";

        NumCondLoops = inputProcessor->getNumObjectsFound(CurrentModuleObject);
        inputProcessor->getObjectDefMaxArgs(CurrentModuleObject, NumParams, NumAlphas, NumNumbers);
        Alphas.allocate(NumAlphas);
        Numbers.allocate(NumNumbers);

        for (Num = 1; Num <= NumCondLoops; ++Num) {
            inputProcessor->getObjectItem(CurrentModuleObject, Num, Alphas, NumAlphas, Numbers, NumNumbers, IOStat);
            // Only looking for BranchList here.
            if (Alphas(8) == BranchListName) {
                FoundCondLoopName = Alphas(1);
                FoundSupplyDemand = "Supply";
                FoundVolFlowRate = Numbers(3);
                FoundCondLoopNum = Num;
                MatchedCondLoop = true;
                break;
            } else if (Alphas(12) == BranchListName) {
                FoundCondLoopName = Alphas(1);
                FoundSupplyDemand = "Demand";
                FoundVolFlowRate = Numbers(3);
                FoundCondLoopNum = Num;
                MatchedCondLoop = true;
                break;
            }
        }

        Alphas.deallocate();
        Numbers.deallocate();
    }

    void FindAirLoopBranchConnection(std::string const &BranchListName,
                                     std::string &FoundAirLoopName,
                                     int &FoundAirLoopNum,
                                     std::string &FoundAir,
                                     Real64 &FoundVolFlowRate,
                                     bool &MatchedAirLoop)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Linda Lawrie
        //       DATE WRITTEN   February 2008
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // An auxiliary routine locate a Airenser loop and type from a BranchListName

        // METHODOLOGY EMPLOYED:
        // calls GetObject for PRIMARY AIR LOOP

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int Num;
        int NumAirLoops;
        int NumParams;
        Array1D_string Alphas;
        int NumAlphas;
        Array1D<Real64> Numbers;
        int NumNumbers;
        int IOStat;

        // Get Inputs
        std::string CurrentModuleObject = "AirLoopHVAC";
        NumAirLoops = inputProcessor->getNumObjectsFound(CurrentModuleObject);
        inputProcessor->getObjectDefMaxArgs(CurrentModuleObject, NumParams, NumAlphas, NumNumbers);
        Alphas.allocate(NumAlphas);
        Numbers.allocate(NumNumbers);

        for (Num = 1; Num <= NumAirLoops; ++Num) {
            inputProcessor->getObjectItem(CurrentModuleObject, Num, Alphas, NumAlphas, Numbers, NumNumbers, IOStat);
            // Only looking for BranchList here.
            if (Alphas(4) == BranchListName) {
                FoundAirLoopName = Alphas(1);
                FoundAir = "Air";
                FoundVolFlowRate = Numbers(1);
                FoundAirLoopNum = Num;
                MatchedAirLoop = true;
                break;
            }
        }

        Alphas.deallocate();
        Numbers.deallocate();
    }

    void FindAirPlantCondenserLoopFromBranchList(std::string const &BranchListName, // Branch List Name
                                                 std::string &LoopType,             // LoopType (if found, Plant,Condenser or Air)
                                                 std::string &LoopSupplyDemandAir,  // Supply if "Supply" or Demand if "Demand" or Air if "Air"
                                                 bool &MatchedLoop                  // true if found
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Linda Lawrie
        //       DATE WRITTEN   February 2008
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Assist in validating Loop Splitter/Mixer connections.

        // METHODOLOGY EMPLOYED:
        // Call two previously written subroutines that match a Branch List Name to
        // Plant or Condenser Loop

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        std::string FoundLoopName;
        int FoundLoopNum;
        Real64 FoundLoopVolFlowRate;

        LoopSupplyDemandAir = std::string();
        FoundLoopName = std::string();
        FoundLoopNum = 0;
        FoundLoopVolFlowRate = 0.0;
        MatchedLoop = false;
        LoopType = std::string();

        // Try Plant first
        FindPlantLoopBranchConnection(BranchListName, FoundLoopName, FoundLoopNum, LoopSupplyDemandAir, FoundLoopVolFlowRate, MatchedLoop);

        if (MatchedLoop) LoopType = "Plant";
        if (!MatchedLoop) { // Try Condenser Loop
            LoopSupplyDemandAir = std::string();
            FoundLoopName = std::string();
            FoundLoopNum = 0;
            FoundLoopVolFlowRate = 0.0;
            MatchedLoop = false;

            // Try Condenser
            FindCondenserLoopBranchConnection(BranchListName, FoundLoopName, FoundLoopNum, LoopSupplyDemandAir, FoundLoopVolFlowRate, MatchedLoop);
            if (MatchedLoop) LoopType = "Condenser";
        }

        if (!MatchedLoop) { // Try Air Loop
            LoopSupplyDemandAir = std::string();
            FoundLoopName = std::string();
            FoundLoopNum = 0;
            FoundLoopVolFlowRate = 0.0;
            MatchedLoop = false;

            // Try Air
            FindAirLoopBranchConnection(BranchListName, FoundLoopName, FoundLoopNum, LoopSupplyDemandAir, FoundLoopVolFlowRate, MatchedLoop);
            if (MatchedLoop) LoopType = "Air";
        }
    }

    //==================================================================================
    //   Routines that test branch integrity
    //==================================================================================

    void AuditBranches(BranchInputManagerData &dataBranchInputManager,
                       bool const mustprint,           // true if the warning should be printed.
                       Optional_string_const CompType, // when mustprint (ScanPlantLoop)  use CompType in error message and scan
                       Optional_string_const CompName  // when mustprint (ScanPlantLoop)  use CompName in error message and scan
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Linda Lawrie
        //       DATE WRITTEN   November 2011
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This routine will point out any "dangling branches" that are not included on a BranchList.
        // Warnings are produced as the user might clutter up the input file with unused branches.

        // Using/Aliasing
        using DataErrorTracking::TotalSevereErrors;
        using General::RoundSigDigits;

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int NumDanglingCount;        // when mustprint not true, count and report
        int BlNum;                   // Branch List Counter
        int BrN;                     // Branch Counter
        int CpN;                     // Components on Branch
        int Found;                   // non-zero when found
        std::string FoundBranchName; // Branch matching compname/type
        bool NeverFound;

        NumDanglingCount = 0;
        NeverFound = true;
        for (BrN = 1; BrN <= dataBranchInputManager.NumOfBranches; ++BrN) {
            Found = 0;
            FoundBranchName = "";
            if (present(CompType) && present(CompName)) {
                for (CpN = 1; CpN <= dataBranchInputManager.Branch(BrN).NumOfComponents; ++CpN) {
                    if (!UtilityRoutines::SameString(CompType, dataBranchInputManager.Branch(BrN).Component(CpN).CType) ||
                        !UtilityRoutines::SameString(CompName, dataBranchInputManager.Branch(BrN).Component(CpN).Name))
                        continue;
                    FoundBranchName = dataBranchInputManager.Branch(BrN).Name;
                    NeverFound = false;
                }
            }
            for (BlNum = 1; BlNum <= dataBranchInputManager.NumOfBranchLists; ++BlNum) {
                Found = UtilityRoutines::FindItemInList(dataBranchInputManager.Branch(BrN).Name, dataBranchInputManager.BranchList(BlNum).BranchNames, dataBranchInputManager.BranchList(BlNum).NumOfBranchNames);
                if (Found != 0) break;
            }
            if (Found != 0) continue;
            ++NumDanglingCount;
            if (DisplayExtraWarnings || mustprint) {
                if (mustprint) {
                    ShowContinueError("AuditBranches: Branch=\"" + dataBranchInputManager.Branch(BrN).Name + "\" not found on any BranchLists.");
                    if (!FoundBranchName.empty()) {
                        ShowContinueError("Branch contains component, type=\"" + CompType + "\", name=\"" + CompName + "\"");
                    }
                } else {
                    ShowSevereMessage("AuditBranches: Branch=\"" + dataBranchInputManager.Branch(BrN).Name + "\" not found on any BranchLists.");
                    ++TotalSevereErrors;
                }
            }
        }
        if (mustprint && NeverFound) { // this may be caught during branch input, not sure
            ShowContinueError("Component, type=\"" + CompType + "\", name=\"" + CompName + "\" was not found on any Branch.");
            ShowContinueError("Look for mistyped branch or component names/types.");
        }
        if (!mustprint && NumDanglingCount > 0) {
            ShowSevereMessage("AuditBranches: There are " + RoundSigDigits(NumDanglingCount) + " branch(es) that do not appear on any BranchList.");
            TotalSevereErrors += NumDanglingCount;
            ShowContinueError("Use Output:Diagnostics,DisplayExtraWarnings; for detail of each branch not on a branch list.");
        }
    }

    void TestBranchIntegrity(BranchInputManagerData &dataBranchInputManager, OutputFiles &outputFiles, bool &ErrFound)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Linda Lawrie
        //       DATE WRITTEN   November 2001
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine tests branch integrity and displays the loop for each branch.
        // Also, input and output nodes.

        // Using/Aliasing
        using General::RoundSigDigits;

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int Loop;
        int Count;
        int MatchNode;                    // Node Number for match
        std::string MatchNodeName;        // Name for error message if not matched
        std::string BranchInletNodeName;  // Branch Inlet Node Name
        std::string BranchOutletNodeName; // Branch Outlet Node Name
        std::string BranchLoopName;       // Loop Name which Branch is part of
        std::string BranchLoopType;       // Loop Type which Branch is part of
        int NumErr;                       // Error Counter
        Array1D_bool BranchReported;
        int BCount;
        int Found;
        //  LOGICAL UniqueNodeError
        int NodeNum;
        int Loop2;
        bool IsAirBranch;
        int BranchFluidType;
        bool MixedFluidTypesOnBranchList;
        int InitialBranchFluidNode;
        Array1D_int BranchFluidNodes;
        Array1D_int FoundBranches;
        Array1D_int BranchPtrs;
        int NumNodesOnBranchList;
        int NumFluidNodes;
        std::string OriginalBranchFluidType;
        std::string cBranchFluidType;
        int Ptr;
        int EndPtr;

        struct BranchUniqueNodes
        {
            int NumNodes{0};
            Array1D_string UniqueNodeNames;
        };

        // Object Data
        Array1D<BranchUniqueNodes> BranchNodes;

        // Formats

        BranchReported.dimension(dataBranchInputManager.NumOfBranches, false);

        // Do by Branch Lists
        ShowMessage("Testing Individual Branch Integrity");
        ErrFound = false;

        BranchNodes.allocate(dataBranchInputManager.NumOfBranches);

        print(outputFiles.bnd, "{}\n", "! ===============================================================");
        static constexpr auto Format_700("! <#Branch Lists>,<Number of Branch Lists>");
        print(outputFiles.bnd, "{}\n", Format_700);
        print(outputFiles.bnd, " #Branch Lists,{}\n", dataBranchInputManager.NumOfBranchLists);
        static constexpr auto Format_702("! <Branch List>,<Branch List Count>,<Branch List Name>,<Loop Name>,<Loop Type>,<Number of Branches>");
        print(outputFiles.bnd, "{}\n", Format_702);
        static constexpr auto Format_704(
            "! <Branch>,<Branch Count>,<Branch Name>,<Loop Name>,<Loop Type>,<Branch Inlet Node Name>,<Branch Outlet Node Name>");
        print(outputFiles.bnd, "{}\n", Format_704);

        for (BCount = 1; BCount <= dataBranchInputManager.NumOfBranchLists; ++BCount) {
            print(outputFiles.bnd,
                  " Branch List,{},{},{},{},{}\n",
                  BCount,
                  dataBranchInputManager.BranchList(BCount).Name,
                  dataBranchInputManager.BranchList(BCount).LoopName,
                  dataBranchInputManager.BranchList(BCount).LoopType,
                  dataBranchInputManager.BranchList(BCount).NumOfBranchNames);

            IsAirBranch = false;
            BranchFluidType = NodeType_Unknown;
            MixedFluidTypesOnBranchList = false;
            NumNodesOnBranchList = 0;
            FoundBranches.allocate(dataBranchInputManager.BranchList(BCount).NumOfBranchNames);
            FoundBranches = 0;
            BranchPtrs.allocate(dataBranchInputManager.BranchList(BCount).NumOfBranchNames + 2);
            BranchPtrs = 0;
            for (Count = 1; Count <= dataBranchInputManager.BranchList(BCount).NumOfBranchNames; ++Count) {
                Found = UtilityRoutines::FindItemInList(dataBranchInputManager.BranchList(BCount).BranchNames(Count), dataBranchInputManager.Branch);
                if (Found > 0) {
                    NumNodesOnBranchList += dataBranchInputManager.Branch(Found).NumOfComponents * 2;
                    FoundBranches(Count) = Found;
                    BranchPtrs(Count) = NumNodesOnBranchList;
                } else {
                    ShowSevereError("Branch not found=" + dataBranchInputManager.BranchList(BCount).BranchNames(Count));
                    ErrFound = true;
                }
            }
            BranchPtrs(dataBranchInputManager.BranchList(BCount).NumOfBranchNames + 1) = BranchPtrs(dataBranchInputManager.BranchList(BCount).NumOfBranchNames) + 1;
            BranchFluidNodes.dimension(NumNodesOnBranchList, 0);
            OriginalBranchFluidType = std::string();
            NumFluidNodes = 0;
            for (Count = 1; Count <= dataBranchInputManager.BranchList(BCount).NumOfBranchNames; ++Count) {
                Found = FoundBranches(Count);
                if (Found == 0) {
                    print(outputFiles.bnd,
                          "   Branch,{},{},(not found),**Unknown**,**Unknown**,**Unknown**,**Unknown**\n",
                          Count,
                          dataBranchInputManager.BranchList(BCount).BranchNames(Count));
                    continue;
                }
                BranchReported(Found) = true;
                // Check Branch for connections

                MatchNode = 0;
                InitialBranchFluidNode = 0;
                if (dataBranchInputManager.Branch(Found).NumOfComponents > 0) {
                    MatchNode = dataBranchInputManager.Branch(Found).Component(1).InletNode;
                    MatchNodeName = dataBranchInputManager.Branch(Found).Component(1).InletNodeName;
                    BranchInletNodeName = dataBranchInputManager.Branch(Found).Component(1).InletNodeName;
                } else {
                    ShowWarningError("Branch has no components=" + dataBranchInputManager.Branch(Found).Name);
                }
                NumErr = 0;
                for (Loop = 1; Loop <= dataBranchInputManager.Branch(Found).NumOfComponents; ++Loop) {
                    if (Node(dataBranchInputManager.Branch(Found).Component(Loop).InletNode).FluidType == NodeType_Air) IsAirBranch = true;
                    if (BranchFluidType == NodeType_Unknown) {
                        ++NumFluidNodes;
                        BranchFluidNodes(NumFluidNodes) = dataBranchInputManager.Branch(Found).Component(Loop).InletNode;
                        BranchFluidType = Node(dataBranchInputManager.Branch(Found).Component(Loop).InletNode).FluidType;
                        InitialBranchFluidNode = dataBranchInputManager.Branch(Found).Component(Loop).InletNode;
                        OriginalBranchFluidType = ValidNodeFluidTypes(BranchFluidType);
                    } else if (BranchFluidType != Node(dataBranchInputManager.Branch(Found).Component(Loop).InletNode).FluidType &&
                               Node(dataBranchInputManager.Branch(Found).Component(Loop).InletNode).FluidType != NodeType_Unknown) {
                        ++NumFluidNodes;
                        BranchFluidNodes(NumFluidNodes) = dataBranchInputManager.Branch(Found).Component(Loop).InletNode;
                        MixedFluidTypesOnBranchList = true;
                    } else {
                        ++NumFluidNodes;
                        BranchFluidNodes(NumFluidNodes) = dataBranchInputManager.Branch(Found).Component(Loop).InletNode;
                    }
                    if (Node(dataBranchInputManager.Branch(Found).Component(Loop).OutletNode).FluidType == NodeType_Air) IsAirBranch = true;
                    if (BranchFluidType == NodeType_Unknown) {
                        ++NumFluidNodes;
                        BranchFluidNodes(NumFluidNodes) = dataBranchInputManager.Branch(Found).Component(Loop).InletNode;
                        BranchFluidType = Node(dataBranchInputManager.Branch(Found).Component(Loop).OutletNode).FluidType;
                        InitialBranchFluidNode = dataBranchInputManager.Branch(Found).Component(Loop).OutletNode;
                        OriginalBranchFluidType = ValidNodeFluidTypes(BranchFluidType);
                    } else if (BranchFluidType != Node(dataBranchInputManager.Branch(Found).Component(Loop).OutletNode).FluidType &&
                               Node(dataBranchInputManager.Branch(Found).Component(Loop).OutletNode).FluidType != NodeType_Unknown) {
                        ++NumFluidNodes;
                        BranchFluidNodes(NumFluidNodes) = dataBranchInputManager.Branch(Found).Component(Loop).OutletNode;
                        MixedFluidTypesOnBranchList = true;
                    } else {
                        ++NumFluidNodes;
                        BranchFluidNodes(NumFluidNodes) = dataBranchInputManager.Branch(Found).Component(Loop).OutletNode;
                    }
                    if (dataBranchInputManager.Branch(Found).Component(Loop).InletNode != MatchNode) {
                        ShowSevereError("Error Detected in BranchList=" + dataBranchInputManager.BranchList(BCount).Name);
                        ShowContinueError("Actual Error occurs in Branch=" + dataBranchInputManager.Branch(Found).Name);
                        ShowContinueError("Branch Outlet does not match Inlet, Outlet=" + MatchNodeName);
                        ShowContinueError("Inlet Name=" + dataBranchInputManager.Branch(Found).Component(Loop).InletNodeName);
                        ErrFound = true;
                        ++NumErr;
                    } else {
                        MatchNode = dataBranchInputManager.Branch(Found).Component(Loop).OutletNode;
                        MatchNodeName = dataBranchInputManager.Branch(Found).Component(Loop).OutletNodeName;
                    }
                }
                dataBranchInputManager.Branch(Found).FluidType = BranchFluidType;
                BranchOutletNodeName = MatchNodeName;
                if (dataBranchInputManager.Branch(Found).AssignedLoopName.empty()) {
                    BranchLoopName = "**Unknown**";
                    BranchLoopType = "**Unknown**";
                } else if (dataBranchInputManager.Branch(Found).AssignedLoopName == dataBranchInputManager.BranchList(BCount).LoopName) {
                    BranchLoopName = dataBranchInputManager.BranchList(BCount).LoopName;
                    BranchLoopType = dataBranchInputManager.BranchList(BCount).LoopType;
                } else {
                    BranchLoopName = dataBranchInputManager.Branch(Found).AssignedLoopName;
                    BranchLoopType = "**Unknown**";
                }
                print(outputFiles.bnd,
                      "   Branch,{},{},{},{},{},{}\n",
                      Count,
                      dataBranchInputManager.Branch(Found).Name,
                      BranchLoopName,
                      BranchLoopType,
                      BranchInletNodeName,
                      BranchOutletNodeName);
            }
            if (MixedFluidTypesOnBranchList) {
                ShowSevereError("BranchList=" + dataBranchInputManager.BranchList(BCount).Name + " has mixed fluid types in its nodes.");
                ErrFound = true;
                if (OriginalBranchFluidType.empty()) OriginalBranchFluidType = "**Unknown**";
                ShowContinueError("Initial Node=" + NodeID(InitialBranchFluidNode) + ", Fluid Type=" + OriginalBranchFluidType);
                ShowContinueError("BranchList Topology - Note nodes which do not match that fluid type:");
                Ptr = 1;
                EndPtr = BranchPtrs(1);
                for (Loop = 1; Loop <= dataBranchInputManager.BranchList(BCount).NumOfBranchNames; ++Loop) {
                    if (FoundBranches(Loop) != 0) {
                        ShowContinueError("..Branch=" + dataBranchInputManager.Branch(FoundBranches(Loop)).Name);
                    } else {
                        ShowContinueError("..Illegal Branch=" + dataBranchInputManager.BranchList(BCount).BranchNames(Loop));
                        continue;
                    }
                    for (Loop2 = Ptr; Loop2 <= EndPtr; ++Loop2) {
                        cBranchFluidType = ValidNodeFluidTypes(Node(BranchFluidNodes(Loop2)).FluidType);
                        if (cBranchFluidType.empty()) cBranchFluidType = "**Unknown**";
                        ShowContinueError("....Node=" + NodeID(BranchFluidNodes(Loop2)) + ", Fluid Type=" + cBranchFluidType);
                    }
                    Ptr = EndPtr + 1;
                    EndPtr = BranchPtrs(Loop + 1);
                }
            }
            BranchFluidNodes.deallocate();
            BranchPtrs.deallocate();
            FoundBranches.deallocate();
        }

        // Build node names in branches
        for (Count = 1; Count <= dataBranchInputManager.NumOfBranches; ++Count) {
            BranchNodes(Count).UniqueNodeNames.allocate(dataBranchInputManager.Branch(Count).NumOfComponents * 2);
            BranchNodes(Count).UniqueNodeNames = std::string();
            NodeNum = 0;
            for (Loop = 1; Loop <= dataBranchInputManager.Branch(Count).NumOfComponents; ++Loop) {
                Found = UtilityRoutines::FindItemInList(dataBranchInputManager.Branch(Count).Component(Loop).InletNodeName, BranchNodes(Count).UniqueNodeNames, NodeNum);
                if (Found == 0) {
                    ++NodeNum;
                    BranchNodes(Count).UniqueNodeNames(NodeNum) = dataBranchInputManager.Branch(Count).Component(Loop).InletNodeName;
                }
                Found = UtilityRoutines::FindItemInList(dataBranchInputManager.Branch(Count).Component(Loop).OutletNodeName, BranchNodes(Count).UniqueNodeNames, NodeNum);
                if (Found == 0) {
                    ++NodeNum;
                    BranchNodes(Count).UniqueNodeNames(NodeNum) = dataBranchInputManager.Branch(Count).Component(Loop).OutletNodeName;
                }
            }
            BranchNodes(Count).NumNodes = NodeNum;
        }
        // Check Uniqueness branch to branch
        for (Count = 1; Count <= dataBranchInputManager.NumOfBranches; ++Count) {
            for (Loop = Count + 1; Loop <= dataBranchInputManager.NumOfBranches; ++Loop) {
                for (Loop2 = 1; Loop2 <= BranchNodes(Count).NumNodes; ++Loop2) {
                    Found = UtilityRoutines::FindItemInList(
                        BranchNodes(Count).UniqueNodeNames(Loop2), BranchNodes(Loop).UniqueNodeNames, BranchNodes(Loop).NumNodes);
                    if (Found != 0) {
                        ShowSevereError("Non-unique node name found, name=" + BranchNodes(Count).UniqueNodeNames(Loop2));
                        ShowContinueError("..1st occurrence in Branch=" + dataBranchInputManager.Branch(Count).Name);
                        ShowContinueError("..duplicate occurrence in Branch=" + dataBranchInputManager.Branch(Loop).Name);
                        ErrFound = true;
                    }
                }
            }
        }
        for (Count = 1; Count <= dataBranchInputManager.NumOfBranches; ++Count) {
            BranchNodes(Count).UniqueNodeNames.deallocate();
        }
        BranchNodes.deallocate();

        BCount = 0;
        for (Count = 1; Count <= dataBranchInputManager.NumOfBranches; ++Count) {
            if (BranchReported(Count)) continue;
            ++BCount;
        }
        if (BCount > 0) {
            static constexpr auto Format_706("! <# Orphaned Branches>,<Number of Branches not on Branch Lists>");
            print(outputFiles.bnd, "{}\n", Format_706);
            print(outputFiles.bnd, " #Orphaned Branches,{}\n", BCount);
            ShowWarningError("There are orphaned Branches in input. See .bnd file for details.");

            BCount = 0;

            for (Count = 1; Count <= dataBranchInputManager.NumOfBranches; ++Count) {
                if (BranchReported(Count)) continue;
                ++BCount;
                ShowWarningError("Orphan Branch=\"" + dataBranchInputManager.Branch(Count).Name + "\".");

                if (dataBranchInputManager.Branch(Count).NumOfComponents > 0) {
                    MatchNode = dataBranchInputManager.Branch(Count).Component(1).InletNode;
                    MatchNodeName = dataBranchInputManager.Branch(Count).Component(1).InletNodeName;
                    BranchInletNodeName = dataBranchInputManager.Branch(Count).Component(1).InletNodeName;
                } else {
                    ShowWarningError("Branch has no components=" + dataBranchInputManager.Branch(Count).Name);
                }
                NumErr = 0;
                for (Loop = 1; Loop <= dataBranchInputManager.Branch(Count).NumOfComponents; ++Loop) {
                    if (dataBranchInputManager.Branch(Count).Component(Loop).InletNode != MatchNode) {
                        ShowSevereError("Error Detected in Branch=" + dataBranchInputManager.Branch(Count).Name);
                        ShowContinueError("Branch Outlet does not match Inlet, Outlet=" + MatchNodeName);
                        ShowContinueError("Inlet Name=" + dataBranchInputManager.Branch(Count).Component(Loop).InletNodeName);
                        ErrFound = true;
                        ++NumErr;
                    } else {
                        MatchNode = dataBranchInputManager.Branch(Count).Component(Loop).OutletNode;
                        MatchNodeName = dataBranchInputManager.Branch(Count).Component(Loop).OutletNodeName;
                    }
                }
                BranchOutletNodeName = MatchNodeName;
                if (dataBranchInputManager.Branch(Count).AssignedLoopName.empty()) {
                    BranchLoopName = "**Unknown**";
                    BranchLoopType = "**Unknown**";
                } else {
                    BranchLoopName = dataBranchInputManager.Branch(Count).AssignedLoopName;
                    BranchLoopType = "**Unknown**";
                }
                print(outputFiles.bnd,
                      " Branch,{},{},{},{},{},{}\n",
                      BCount,
                      dataBranchInputManager.Branch(Count).Name,
                      BranchLoopName,
                      BranchLoopType,
                      BranchInletNodeName,
                      BranchOutletNodeName);
            }
        }

        if (ErrFound) {
            ShowSevereError("Branch(es) did not pass integrity testing");
        } else {
            ShowMessage("All Branches passed integrity testing");
        }
    }

} // namespace BranchInputManager

} // namespace EnergyPlus
