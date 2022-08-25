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

#ifndef RoomAirModelManager_hh_INCLUDED
#define RoomAirModelManager_hh_INCLUDED

// EnergyPlus Headers
#include <EnergyPlus/Data/BaseData.hh>
#include <EnergyPlus/EnergyPlus.hh>

namespace EnergyPlus {

// Forward declarations
struct EnergyPlusData;

namespace RoomAirModelManager {

    void ManageAirModel(EnergyPlusData &state, int &ZoneNum);

    void GetAirModelDatas(EnergyPlusData &state);

    void GetUserDefinedPatternData(EnergyPlusData &state, bool &ErrorsFound); // True if errors found during this get input routine

    void GetAirNodeData(EnergyPlusData &state, bool &ErrorsFound); // True if errors found during this get input routine

    void GetMundtData(EnergyPlusData &state, bool &ErrorsFound); // True if errors found during this get input routine

    void GetDisplacementVentData(EnergyPlusData &state, bool &ErrorsFound); // True if errors found during this get input routine

    void GetCrossVentData(EnergyPlusData &state, bool &ErrorsFound); // True if errors found during this get input routine

    void GetUFADZoneData(EnergyPlusData &state, bool &ErrorsFound); // True if errors found during this get input routine

    void SharedDVCVUFDataInit(EnergyPlusData &state, int &ZoneNum);

    void GetRoomAirflowNetworkData(EnergyPlusData &state, bool &ErrorsFound); // True if errors found during this get input routine

    void GetRAFNNodeNum(EnergyPlusData &state,
                        std::string const &RAFNNodeName,
                        int &ZoneNum,
                        int &RAFNNodeNum,
                        bool &Errorfound); // find zone number and node number based on the node name

    bool CheckEquipName(EnergyPlusData &state,
                        std::string const &EquipType, // Equipment type
                        std::string const &EquipName, // Equipment Name
                        std::string &SupplyNodeName,  // Supply node name
                        std::string &ReturnNodeName,  // Return node name
                        int TotNumEquip,              // how many of this equipment type
                        int TypeNum);                 // equipment type number

} // namespace RoomAirModelManager

struct RoomAirModelManagerData : BaseGlobalStruct
{

    bool GetUCSDDVDataFlag = true; // UCSD
    bool GetAirModelData = true;   // Used to "get" all air model data
    bool MyOneTimeFlag = true;
    int CompNum = 0;
    int TypeNum = 0;
    int NodeNum1 = 0;
    int NodeNum2 = 0;
    int CompNumber = 0;                    // AirflowNetwork Component number
    int TypeNumber = 0;                    // Airflownetwork Type Number within a component
    int NodeNumber1 = 0;                   // The first node number in an AirflowNetwork linkage data
    int NodeNumber2 = 0;                   // The Second node number in an AirflowNetwork linkage data
    int contFloorBegin = 0;                // counter
    int contFloorLast = 0;                 // counter
    int contFloor = 0;                     // counter
    int contCeilingBegin = 0;              // counter
    int contCeilingLast = 0;               // counter
    int contCeiling = 0;                   // counter
    int contWallBegin = 0;                 // counter
    int contWallLast = 0;                  // counter
    int contWall = 0;                      // counter
    int contWindowBegin = 0;               // counter
    int contWindowLast = 0;                // counter
    int contWindow = 0;                    // counter
    int contInternalBegin = 0;             // counter
    int contInternalLast = 0;              // counter
    int contInternal = 0;                  // counter
    int contDoorBegin = 0;                 // counter
    int contDoorLast = 0;                  // counter
    int contDoor = 0;                      // counter
    int Loop = 0;                          // counter
    int Loop2 = 0;                         // counter
    int Loop3 = 0;                         // counter
    int i = 0;                             // counter
    int N = 0;                             // counter
    Real64 Z1ZoneAux = 0.0;                // Auxiliary variables
    Real64 Z2ZoneAux = 0.0;                // Auxiliary variables
    Real64 Z1Zone = 0.0;                   // Auxiliary variables
    Real64 Z2Zone = 0.0;                   // Auxiliary variables
    Real64 CeilingHeightDiffMax = 0.1;     // Maximum difference between wall height and ceiling height
    Real64 Z1ofZoneAux = 0.0;              // Auxiliary variables
    Real64 Z2ofZoneAux = 0.0;              // Auxiliary variables
    Real64 Z1ofZone = 0.0;                 // Auxiliary variables
    Real64 Z2ofZone = 0.0;                 // Auxiliary variables
    Real64 CeilingHeightDiffMaximum = 0.1; // Maximum difference between wall height and ceiling height
    Array1D_bool MyEnvrnFlag;

    void clear_state() override
    {
        *this = RoomAirModelManagerData();
    }
};

} // namespace EnergyPlus

#endif
