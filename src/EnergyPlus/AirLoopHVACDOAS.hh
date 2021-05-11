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

#ifndef ENERGYPLUS_AIRLOOPHVACDOAS_HH
#define ENERGYPLUS_AIRLOOPHVACDOAS_HH

// C++ Headers
#include <string>
#include <vector>

// EnergyPlus Headers
#include <EnergyPlus/Data/BaseData.hh>
#include <EnergyPlus/EPVector.hh>
#include <EnergyPlus/EnergyPlus.hh>

namespace EnergyPlus {

// Forward declarations
struct EnergyPlusData;

namespace AirLoopHVACDOAS {

    void CheckConvergence(EnergyPlusData &state);

    struct AirLoopMixer
    {
        std::string name;
        static AirLoopMixer *factory(EnergyPlusData &state, int object_type_of_num, std::string const &objectName);
        int numOfInletNodes;
        int m_AirLoopMixer_Num;
        int OutletNodeNum;
        std::string OutletNodeName;
        std::vector<std::string> InletNodeName;
        std::vector<int> InletNodeNum;
        Real64 OutletTemp;

        // default constructor
        AirLoopMixer() : numOfInletNodes(0), m_AirLoopMixer_Num(0), OutletNodeNum(0), OutletTemp(0.0)
        {
        }

        ~AirLoopMixer() = default; // destructor

        static void getAirLoopMixer(EnergyPlusData &state);
        void CalcAirLoopMixer(EnergyPlusData &state);
    };

    struct AirLoopSplitter
    {
        std::string name;
        static AirLoopSplitter *factory(EnergyPlusData &state, int object_type_of_num, std::string const &objectName);
        int numOfOutletNodes;
        int m_AirLoopSplitter_Num;
        std::string InletNodeName;
        std::vector<std::string> OutletNodeName;
        std::vector<int> OutletNodeNum;
        Real64 InletTemp;

        // default constructor
        AirLoopSplitter() : numOfOutletNodes(0), m_AirLoopSplitter_Num(0), InletTemp(0.0)
        {
        }

        ~AirLoopSplitter() = default; // destructor

        static void getAirLoopSplitter(EnergyPlusData &state);
        void CalcAirLoopSplitter(EnergyPlusData &state, Real64 Temp, Real64 Humrat);
    };

    struct AirLoopDOAS
    {
        // friend class AirLoopMixer and AirLoopSplitter;
        // members
        Real64 SumMassFlowRate;
        Real64 PreheatTemp;
        Real64 PrecoolTemp;
        Real64 PreheatHumRat;
        Real64 PrecoolHumRat;
        Real64 SizingMassFlow;
        Real64 SizingCoolOATemp;
        Real64 SizingCoolOAHumRat;
        Real64 HeatOutTemp;   // outdoor air temperature for heating sizing calculation
        Real64 HeatOutHumRat; // outdoor air humidity ratio for heating sizing calculation

        int m_AirLoopDOASNum;
        int m_OASystemNum;
        int m_AvailManagerSchedPtr;
        int m_AirLoopMixerIndex;
        int m_AirLoopSplitterIndex;
        int NumOfAirLoops;
        int m_InletNodeNum;
        int m_OutletNodeNum;
        int m_FanIndex;
        int m_FanInletNodeNum;
        int m_FanOutletNodeNum;
        int m_FanTypeNum;
        int m_HeatCoilNum;
        int m_CoolCoilNum;
        int ConveCount;
        int ConveIndex;

        bool m_HeatExchangerFlag;
        bool SizingOnceFlag;
        bool DXCoilFlag;
        bool FanBlowTroughFlag;

        AirLoopMixer *m_CompPointerAirLoopMixer;
        AirLoopSplitter *m_CompPointerAirLoopSplitter;

        std::string Name;
        std::string AvailManagerSchedName;
        std::string OASystemName;
        std::string AirLoopMixerName;
        std::string AirLoopSplitterName;
        std::string FanName;

        std::vector<int> m_AirLoopNum; // array of AirLoop number
        std::vector<std::string> AirLoopName;
        std::vector<int> m_OACtrlNum; // array of OA controller number

        int HWLoopNum;
        int HWLoopSide;
        int HWBranchNum;
        int HWCompNum;
        int HWCtrlNodeNum;
        int CWLoopNum;
        int CWLoopSide;
        int CWBranchNum;
        int CWCompNum;
        int CWCtrlNodeNum;
        bool MyEnvrnFlag;

        // default constructor
        AirLoopDOAS() // constructor
            : SumMassFlowRate(0.0), PreheatTemp(-999.0), PrecoolTemp(-999.0), PreheatHumRat(-999.0), PrecoolHumRat(-999.0), SizingMassFlow(0.0),
              SizingCoolOATemp(-999.0), SizingCoolOAHumRat(-999.0), HeatOutTemp(0.0), HeatOutHumRat(0.0), m_AirLoopDOASNum(0), m_OASystemNum(0),
              m_AvailManagerSchedPtr(0), m_AirLoopMixerIndex(-1), m_AirLoopSplitterIndex(-1), NumOfAirLoops(0), m_InletNodeNum(0), m_OutletNodeNum(0),
              m_FanIndex(-1), m_FanInletNodeNum(0), m_FanOutletNodeNum(0), m_FanTypeNum(0), m_HeatCoilNum(0), m_CoolCoilNum(0), ConveCount(0),
              ConveIndex(0), m_HeatExchangerFlag(false), SizingOnceFlag(true), DXCoilFlag(false), FanBlowTroughFlag(false),
              m_CompPointerAirLoopMixer(nullptr), m_CompPointerAirLoopSplitter(nullptr), HWLoopNum(0), HWLoopSide(0), HWBranchNum(0), HWCompNum(0),
              HWCtrlNodeNum(0), CWLoopNum(0), CWLoopSide(0), CWBranchNum(0), CWCompNum(0), CWCtrlNodeNum(0), MyEnvrnFlag(true)

        {
        }

        ~AirLoopDOAS() = default; // destructor

        static void getAirLoopDOASInput(EnergyPlusData &state);

        void SimAirLoopHVACDOAS(EnergyPlusData &state, bool firstHVACIteration, int &CompIndex);

        void initAirLoopDOAS(EnergyPlusData &state, bool FirstHVACIteration);

        void CalcAirLoopDOAS(EnergyPlusData &state, bool FirstHVACIteration);

        void SizingAirLoopDOAS(EnergyPlusData &state);

        void GetDesignDayConditions(EnergyPlusData &state);
    };

    int getAirLoopMixerIndex(EnergyPlusData &state, std::string const &objectName);
    int getAirLoopSplitterIndex(EnergyPlusData &state, std::string const &objectName);
    void getAirLoopHVACDOASInput(EnergyPlusData &state);

} // namespace AirLoopHVACDOAS

struct AirLoopHVACDOASData : BaseGlobalStruct
{
    bool GetInputOnceFlag = true;
    bool getAirLoopMixerInputOnceFlag = true;
    bool getAirLoopSplitterInputOnceFlag = true;
    int numAirLoopDOAS = 0;
    std::vector<AirLoopHVACDOAS::AirLoopDOAS> airloopDOAS;
    std::vector<AirLoopHVACDOAS::AirLoopMixer> airloopMixer;
    std::vector<AirLoopHVACDOAS::AirLoopSplitter> airloopSplitter;
    bool SummerDesignDayFlag = true;
    bool WinterDesignDayFlag = true;
    void clear_state() override
    {
        this->GetInputOnceFlag = true;
        this->getAirLoopMixerInputOnceFlag = true;
        this->getAirLoopSplitterInputOnceFlag = true;
        this->numAirLoopDOAS = 0;
        this->airloopDOAS.clear();
        this->airloopMixer.clear();
        this->airloopSplitter.clear();
        this->SummerDesignDayFlag = true;
        this->WinterDesignDayFlag = true;
    }
};

} // namespace EnergyPlus
#endif // ENERGYPLUS_AIRLOOPHVACDOAS_HH
