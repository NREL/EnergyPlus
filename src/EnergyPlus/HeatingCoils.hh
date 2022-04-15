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

#ifndef HeatingCoils_hh_INCLUDED
#define HeatingCoils_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>
#include <ObjexxFCL/Optional.hh>

// EnergyPlus Headers
#include <EnergyPlus/Data/BaseData.hh>
#include <EnergyPlus/DataGlobals.hh>
#include <EnergyPlus/EnergyPlus.hh>

namespace EnergyPlus {

// Forward declarations
struct EnergyPlusData;

namespace HeatingCoils {

    // MODULE PARAMETER DEFINITIONS
    Real64 constexpr MinAirMassFlow(0.001);

    enum class HeatObjTypes // reclaim heat object types
    {
        Invalid = -1,
        COMPRESSORRACK_REFRIGERATEDCASE,
        COIL_DX_COOLING, // single speed DX
        COIL_DX_MULTISPEED,
        COIL_DX_MULTIMODE,
        CONDENSER_REFRIGERATION,
        COIL_DX_VARIABLE_COOLING,
        Num
    };

    struct HeatingCoilEquipConditions
    {
        // Members
        std::string Name;             // Name of the HeatingCoil
        std::string HeatingCoilType;  // Type of HeatingCoil ie. Heating or Cooling
        std::string HeatingCoilModel; // Type of HeatingCoil ie. Simple, Detailed, etc.
        int HCoilType_Num;
        DataGlobalConstants::ResourceType FuelType_Num; // Type of fuel used, reference resource type integers
        std::string Schedule;                           // HeatingCoil Operation Schedule
        int SchedPtr;                                   // Pointer to the correct schedule
        int InsuffTemperatureWarn;                      // Used for recurring error message
        Real64 InletAirMassFlowRate;                    // MassFlow through the HeatingCoil being Simulated [kg/Sec]
        Real64 OutletAirMassFlowRate;
        Real64 InletAirTemp;
        Real64 OutletAirTemp;
        Real64 InletAirHumRat;
        Real64 OutletAirHumRat;
        Real64 InletAirEnthalpy;
        Real64 OutletAirEnthalpy;
        Real64 HeatingCoilLoad; // Total Load on the Coil [J]
        Real64 HeatingCoilRate; // Total Coil Rate on the Coil [W]
        Real64 FuelUseLoad;     // Fuel Usage of Coil [J]
        Real64 ElecUseLoad;     // Electric Usage of Coil [J]
        Real64 FuelUseRate;     // Fuel Usage of Coil [W]
        Real64 ElecUseRate;     // Electric Usage of Coil [W]
        Real64 Efficiency;      // HeatingCoil Efficiency Value
        Real64 NominalCapacity; // Nominal Capacity of Coil [W]
        Real64 DesiredOutletTemp;
        Real64 DesiredOutletHumRat;
        Real64 AvailTemperature; // Used in heat recovery test [C]
        int AirInletNodeNum;
        int AirOutletNodeNum;
        int TempSetPointNodeNum; // If applicable this is the node number that the temp setpoint exists.
        int Control;
        int PLFCurveIndex;        // Index for part-load factor curve index for gas heating coil
        Real64 ParasiticElecLoad; // parasitic electric load associated with the gas heating coil
        Real64 ParasiticFuelLoad; // parasitic fuel load associated with the gas heating coil
        // (standing pilot light) [J]
        Real64 ParasiticFuelRate; // avg. parasitic fuel consumption rate with the gas heating coil
        // (standing pilot light) [J]
        Real64 ParasiticFuelCapacity;       // capacity of parasitic fuel consumption rate, input by user [W]
        Real64 RTF;                         // Heater runtime fraction, including PLF curve impacts
        int RTFErrorIndex;                  // used in recurring error warnings
        int RTFErrorCount;                  // used in recurring error warnings
        int PLFErrorIndex;                  // used in recurring error warnings
        int PLFErrorCount;                  // used in recurring error warnings
        std::string ReclaimHeatingCoilName; // Name of reclaim heating coil
        int ReclaimHeatingSourceIndexNum;   // Index to reclaim heating source (condenser) of a specific type
        HeatObjTypes ReclaimHeatingSource;  // The source for the Reclaim Heating Coil
        //                                                            COMPRESSOR RACK:REFRIGERATED CASE    = 1
        //                                                            COIL:DX:COOLINGBYPASSFACTOREMPIRICAL = 2
        //                                                            COIL:DX:MULTISPEED:COOLINGEMPIRICAL  = 3
        //                                                            COIL:DX:MultiMode:CoolingEmpirical   = 4
        //                                                            Refrigeration:Condenser              = 5
        int NumOfStages;                     // Number of speeds
        Array1D<Real64> MSNominalCapacity;   // Nominal Capacity MS AC Furnace [W]
        Array1D<Real64> MSEfficiency;        // Efficiency for MS AC Furnace [dimensionless]
        Array1D<Real64> MSParasiticElecLoad; // Parasitic elec load MS AC Furnace (gas only) [W]
        bool DesiccantRegenerationCoil;      // true if it is a regeneration air heating coil defined in Desiccant Dehumidifier system
        int DesiccantDehumNum;               // index to desiccant dehumidifier object
        bool FaultyCoilSATFlag;              // True if the coil has SAT sensor fault
        int FaultyCoilSATIndex;              // Index of the fault object corresponding to the coil
        Real64 FaultyCoilSATOffset;          // Coil SAT sensor offset
        bool reportCoilFinalSizes;           // one time report of sizes to coil report
        int AirLoopNum;                      // Airloop number
        // Default Constructor
        HeatingCoilEquipConditions()
            : HCoilType_Num(0), FuelType_Num(DataGlobalConstants::ResourceType::None), SchedPtr(0), InsuffTemperatureWarn(0),
              InletAirMassFlowRate(0.0), OutletAirMassFlowRate(0.0), InletAirTemp(0.0), OutletAirTemp(0.0), InletAirHumRat(0.0), OutletAirHumRat(0.0),
              InletAirEnthalpy(0.0), OutletAirEnthalpy(0.0), HeatingCoilLoad(0.0), HeatingCoilRate(0.0), FuelUseLoad(0.0), ElecUseLoad(0.0),
              FuelUseRate(0.0), ElecUseRate(0.0), Efficiency(0.0), NominalCapacity(0.0), DesiredOutletTemp(0.0), DesiredOutletHumRat(0.0),
              AvailTemperature(0.0), AirInletNodeNum(0), AirOutletNodeNum(0), TempSetPointNodeNum(0), Control(0), PLFCurveIndex(0),
              ParasiticElecLoad(0.0), ParasiticFuelLoad(0.0), ParasiticFuelRate(0.0), ParasiticFuelCapacity(0.0), RTF(0.0), RTFErrorIndex(0),
              RTFErrorCount(0), PLFErrorIndex(0), PLFErrorCount(0), ReclaimHeatingSourceIndexNum(0), ReclaimHeatingSource(HeatObjTypes::Invalid),
              NumOfStages(0), DesiccantRegenerationCoil(false), DesiccantDehumNum(0), FaultyCoilSATFlag(false), FaultyCoilSATIndex(0),
              FaultyCoilSATOffset(0.0), reportCoilFinalSizes(true), AirLoopNum(0)
        {
        }
    };
    struct HeatingCoilNumericFieldData
    {
        // Members
        Array1D_string FieldNames;

        // Default Constructor
        HeatingCoilNumericFieldData()
        {
        }
    };

    void SimulateHeatingCoilComponents(EnergyPlusData &state,
                                       std::string_view CompName,
                                       bool const FirstHVACIteration,
                                       Optional<Real64 const> QCoilReq = _, // coil load to be met
                                       Optional_int CompIndex = _,
                                       Optional<Real64> QCoilActual = _,         // coil load actually delivered returned to calling component
                                       Optional_bool_const SuppHeat = _,         // True if current heating coil is a supplemental heating coil
                                       Optional_int_const FanOpMode = _,         // fan operating mode, CycFanCycCoil or ContFanCycCoil
                                       Optional<Real64 const> PartLoadRatio = _, // part-load ratio of heating coil
                                       Optional_int StageNum = _,
                                       Optional<Real64 const> SpeedRatio = _ // Speed ratio of MultiStage heating coil
    );

    // Get Input Section of the Module
    //******************************************************************************

    void GetHeatingCoilInput(EnergyPlusData &state);

    // End of Get Input subroutines for the HB Module
    //******************************************************************************

    // Beginning Initialization Section of the Module
    //******************************************************************************

    void InitHeatingCoil(EnergyPlusData &state, int const CoilNum, bool const FirstHVACIteration, Real64 const QCoilRequired);

    void SizeHeatingCoil(EnergyPlusData &state, int const CoilNum);

    // End Initialization Section of the Module
    //******************************************************************************

    // Begin Algorithm Section of the Module
    //******************************************************************************

    void CalcElectricHeatingCoil(EnergyPlusData &state,
                                 int const CoilNum, // index to heating coil
                                 Real64 &QCoilReq,
                                 Real64 &QCoilActual,       // coil load actually delivered (W)
                                 int const FanOpMode,       // fan operating mode
                                 Real64 const PartLoadRatio // part-load ratio of heating coil
    );

    void CalcMultiStageElectricHeatingCoil(EnergyPlusData &state,
                                           int const CoilNum,       // the number of the electric heating coil to be simulated
                                           Real64 const SpeedRatio, // SpeedRatio varies between 1.0 (maximum speed) and 0.0 (minimum speed)
                                           Real64 const CycRatio,   // cycling part load ratio
                                           int const StageNum,      // Stage number
                                           int const FanOpMode      // Fan operation mode
    );

    void CalcFuelHeatingCoil(EnergyPlusData &state,
                             int const CoilNum, // index to heating coil
                             Real64 const QCoilReq,
                             Real64 &QCoilActual,       // coil load actually delivered (W)
                             int const FanOpMode,       // fan operating mode
                             Real64 const PartLoadRatio // part-load ratio of heating coil
    );

    void CalcMultiStageGasHeatingCoil(EnergyPlusData &state,
                                      int const CoilNum,       // the number of the Gas heating coil to be simulated
                                      Real64 const SpeedRatio, // SpeedRatio varies between 1.0 (maximum speed) and 0.0 (minimum speed)
                                      Real64 const CycRatio,   // cycling part load ratio
                                      int const StageNum,      // Speed number
                                      int const FanOpMode      // Fan operation mode
    );

    void CalcDesuperheaterHeatingCoil(EnergyPlusData &state,
                                      int const CoilNum,     // index to desuperheater heating coil
                                      Real64 const QCoilReq, // load requested by the simulation for load based control [W]
                                      Real64 &QCoilActual    // coil load actually delivered
    );

    // End Algorithm Section of the Module
    // *****************************************************************************

    // Beginning of Update subroutines for the HeatingCoil Module
    // *****************************************************************************

    void UpdateHeatingCoil(EnergyPlusData &state, int const CoilNum);

    //        End of Update subroutines for the HeatingCoil Module
    // *****************************************************************************

    // Beginning of Reporting subroutines for the HeatingCoil Module
    // *****************************************************************************

    void ReportHeatingCoil(EnergyPlusData &state, int const CoilNum, bool const coilIsSuppHeater);

    //        End of Reporting subroutines for the HeatingCoil Module

    void GetCoilIndex(EnergyPlusData &state, std::string const &HeatingCoilName, int &HeatingCoilIndex, bool &ErrorsFound);

    void CheckHeatingCoilSchedule(EnergyPlusData &state,
                                  std::string const &CompType, // unused1208
                                  std::string_view CompName,
                                  Real64 &Value,
                                  int &CompIndex);

    Real64 GetCoilCapacity(EnergyPlusData &state,
                           std::string const &CoilType, // must match coil types in this module
                           std::string const &CoilName, // must match coil names for the coil type
                           bool &ErrorsFound            // set to true if problem
    );

    int GetCoilAvailScheduleIndex(EnergyPlusData &state,
                                  std::string const &CoilType, // must match coil types in this module
                                  std::string const &CoilName, // must match coil names for the coil type
                                  bool &ErrorsFound            // set to true if problem
    );

    int GetCoilInletNode(EnergyPlusData &state,
                         std::string_view CoilType,   // must match coil types in this module
                         std::string const &CoilName, // must match coil names for the coil type
                         bool &ErrorsFound            // set to true if problem
    );

    int GetCoilOutletNode(EnergyPlusData &state,
                          std::string_view CoilType,   // must match coil types in this module
                          std::string const &CoilName, // must match coil names for the coil type
                          bool &ErrorsFound            // set to true if problem
    );

    int GetHeatReclaimSourceIndex(EnergyPlusData &state,
                                  std::string const &CoilType, // must match coil types in this module
                                  std::string const &CoilName, // must match coil names for the coil type
                                  bool &ErrorsFound            // set to true if problem
    );

    int GetCoilControlNodeNum(EnergyPlusData &state,
                              std::string const &CoilType, // must match coil types in this module
                              std::string const &CoilName, // must match coil names for the coil type
                              bool &ErrorsFound            // set to true if problem
    );

    int GetHeatingCoilTypeNum(EnergyPlusData &state,
                              std::string const &CoilType, // must match coil types in this module
                              std::string const &CoilName, // must match coil names for the coil type
                              bool &ErrorsFound            // set to true if problem
    );

    int GetHeatingCoilIndex(EnergyPlusData &state,
                            std::string const &CoilType, // must match coil types in this module
                            std::string const &CoilName, // must match coil names for the coil type
                            bool &ErrorsFound            // set to true if problem
    );

    int GetHeatingCoilPLFCurveIndex(EnergyPlusData &state,
                                    std::string const &CoilType, // must match coil types in this module
                                    std::string const &CoilName, // must match coil names for the coil type
                                    bool &ErrorsFound            // set to true if problem
    );

    int GetHeatingCoilNumberOfStages(EnergyPlusData &state,
                                     std::string const &CoilType, // must match coil types in this module
                                     std::string const &CoilName, // must match coil names for the coil type
                                     bool &ErrorsFound            // set to true if problem
    );

    // sets data to a coil that is used as a regeneration air heating coil in
    // desiccant dehumidification system
    void SetHeatingCoilData(EnergyPlusData &state,
                            int const CoilNum,                           // Number of electric or gas heating Coil
                            bool &ErrorsFound,                           // Set to true if certain errors found
                            Optional_bool DesiccantRegenerationCoil = _, // Flag that this coil is used as regeneration air heating coil
                            Optional_int DesiccantDehumIndex = _         // Index for the desiccant dehum system where this caoil is used
    );

    void SetHeatingCoilAirLoopNumber(EnergyPlusData &state, std::string const &HeatingCoilName, int AirLoopNum, bool &ErrorsFound);

    //        End of Utility subroutines for the HeatingCoil Module

} // namespace HeatingCoils

struct HeatingCoilsData : BaseGlobalStruct
{

    int NumDesuperheaterCoil = 0; // Total number of desuperheater heating coil objects in input
    int NumElecCoil = 0;
    int NumElecCoilMultiStage = 0;
    int NumFuelCoil = 0;
    int NumGasCoilMultiStage = 0;
    int NumHeatingCoils = 0; // The Number of HeatingCoils found in the Input
    Array1D_bool MySizeFlag;
    Array1D_bool ValidSourceType;  // Used to determine if a source for a desuperheater heating coil is valid
    bool GetCoilsInputFlag = true; // Flag set to make sure you get input once
    bool CoilIsSuppHeater = false; // Flag set to indicate the heating coil is a supplemental heater
    Array1D_bool CheckEquipName;
    Array1D<HeatingCoils::HeatingCoilEquipConditions> HeatingCoil;
    Array1D<HeatingCoils::HeatingCoilNumericFieldData> HeatingCoilNumericFields;
    bool MyOneTimeFlag = true; // one time initialization flag
    bool InputErrorsFound = false;

    int MaxNums = 0;                    // Maximum number of numeric input fields
    int MaxAlphas = 0;                  // Maximum number of alpha input fields
    int TotalArgs = 0;                  // Total number of alpha and numeric arguments (max) for a certain object in the input file
    int ValidSourceTypeCounter = 0;     // Counter used to determine if desuperheater source name is valid
    bool HeatingCoilFatalError = false; // used for error checking
    Array1D_bool MySPTestFlag;          // used for error checking
    Array1D_bool ShowSingleWarning;     // Used for single warning message for desuperheater coil
    Array1D_bool MyEnvrnFlag;           // one time environment flag

    void clear_state() override
    {
        this->NumDesuperheaterCoil = 0;
        this->NumElecCoil = 0;
        this->NumElecCoilMultiStage = 0;
        this->NumFuelCoil = 0;
        this->NumGasCoilMultiStage = 0;
        this->NumHeatingCoils = 0;
        this->MySizeFlag.deallocate();
        this->ValidSourceType.deallocate();
        this->GetCoilsInputFlag = true;
        this->CoilIsSuppHeater = false;
        this->CheckEquipName.deallocate();
        this->HeatingCoil.deallocate();
        this->HeatingCoilNumericFields.deallocate();
        this->MyOneTimeFlag = true;
        this->InputErrorsFound = false;
        this->MaxNums = 0;
        this->MaxAlphas = 0;
        this->TotalArgs = 0;
        this->ValidSourceTypeCounter = 0;
        this->HeatingCoilFatalError = false;
        this->MySPTestFlag.clear();
        this->ShowSingleWarning.clear();
        this->MyEnvrnFlag.clear();
    }
};

} // namespace EnergyPlus

#endif
