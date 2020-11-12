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

#ifndef IntegratedHeatPump_hh_INCLUDED
#define IntegratedHeatPump_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.fwd.hh>
#include <ObjexxFCL/Optional.fwd.hh>

// EnergyPlus Headers
#include <EnergyPlus/EnergyPlus.hh>

namespace EnergyPlus {

// Forward declarations
struct EnergyPlusData;

namespace IntegratedHeatPump {

    // Identifier is VarSpeedCoil
    extern bool GetCoilsInputFlag; // Flag set to make sure you get input once

    // operation mode
    enum class IHPOperationMode : int
    {
        IdleMode,
        SCMode,
        SHMode,
        DWHMode,
        SCWHMatchSCMode,
        SCWHMatchWHMode,
        SCDWHMode,
        SHDWHElecHeatOffMode,
        SHDWHElecHeatOnMode
    };

    enum class IHPStorageType : int
    {
        HOTWATER,
        LIQUIDDESICCANT,
    };

    struct IntegratedHeatPumpData // variable speed coil
    {
        // Members
        std::string Name;    // Name of the  Coil
        std::string IHPtype; // type of coil

        IHPStorageType StorageType; 

        std::string SCCoilType; // Numeric Equivalent for SC Coil Type
        std::string SCCoilName;
        int SCCoilIndex; // Index to SC coil

        std::string EnDehumCoilType; // Numeric Equivalent for enhanced dehumidification Coil Type
        std::string EnDehumCoilName;
        int EnDehumCoilIndex; // Index to enhanced dehumidification coil

        std::string GridSCCoilType; // Numeric Equivalent for enhanced dehumidification Coil Type
        std::string GridSCCoilName;
        int GridSCCoilIndex; // Index to enhanced dehumidification coil

        std::string SHCoilType; // Numeric Equivalent for SH Coil Type
        std::string SHCoilName;
        int SHCoilIndex; // Index to SH coil

        std::string GridSHCoilType; // Numeric Equivalent for grid SH Coil Type
        std::string GridSHCoilName;
        int GridSHCoilIndex; // Index to grid SH coil

        std::string SCWHCoilType; // Numeric Equivalent for SCWH Coil Type
        std::string SCWHCoilName;
        int SCWHCoilIndex; // Index to SCWH coil

        std::string DWHCoilType; // Numeric Equivalent for DWH Coil Type
        std::string DWHCoilName;
        int DWHCoilIndex; // Index to DWH coil

        std::string SCDWHCoolCoilType; // Numeric Equivalent for SCDWH Coil Type, cooling part
        std::string SCDWHCoolCoilName;
        int SCDWHCoolCoilIndex; // Index to SCDWH coil, cooling part

        std::string SCDWHWHCoilType; // Numeric Equivalent for SCDWH Coil Type, water heating part
        std::string SCDWHWHCoilName;
        int SCDWHWHCoilIndex; // Index to SCDWH coil, water heating part

        std::string SHDWHHeatCoilType; // Numeric Equivalent for SHDWH Coil Type, heating part
        std::string SHDWHHeatCoilName;
        int SHDWHHeatCoilIndex; // Index to SHDWH coil, heating part

        std::string SHDWHWHCoilType; // Numeric Equivalent for SHDWH Coil Type, water heating part
        std::string SHDWHWHCoilName;
        int SHDWHWHCoilIndex; // Index to SHDWH coil, water heating part

        std::string LDDehumCoilType; // liquid desiccant dehumidification coil type
        std::string LDDehumCoilName;
        int LDDehumCoilIndex; 

        std::string LDRegenCoilType; // liquid desiccant regeneration coil type
        std::string LDRegenCoilName;
        int LDRegenCoilIndex; 

        std::string EvapCoolCoilType; // evaporative cooling coil type
        std::string EvapCoolCoilName;
        int EvapCoolCoilIndex; 

        std::string SUPHEATTYPE; // supplemental heater type

        std::string ChillerCoilType; // Numeric Equivalent for chiller Coil Type
        std::string ChillerCoilName;
        int ChillerCoilIndex; // Index to chiller coil index

        std::string ChillTankType; // Numeric Equivalent for chiller Coil Type
        std::string ChillTankName;
        int ChillTankIndex; // Index to ice storage tank

        std::string SupWaterCoilType; // Numeric Equivalent for chiller Coil Type
        std::string SupWaterCoilName;
        int SupWaterCoilIndex; // Index to chiller coil index

        int AirCoolInletNodeNum; // Node Number of the Air cooling coil Inlet
        int AirCoolOutletNodeNum; // Node Number of the Air cooling coil Inlet
        int AirHeatInletNodeNum; // Node Number of the Air cooling coil Inlet
        int AirOutletNodeNum;    // Node Number of the Air Outlet
        int WaterInletNodeNum;   // Node Number of the Water Onlet
        int WaterMiddleNodeNum; // middle node number entering liquid desiccant regeneration coil if any
        int WaterOutletNodeNum;  // Node Number of the Water Outlet
        int WaterTankoutNod;     // water node to monitor the supply water flow amount

        int ModeMatchSCWH;
        //- 0: match cooling load, 1 : match water heating load in SCWH mode
        int MinSpedSCWH;  //-minimum speed level for SCWH mode
        int MinSpedSCDWH; //- minimum speed level for SCDWH mode
        int MinSpedSHDWH;
        //- minimum speed level for SHDWH mode
        Real64 TindoorOverCoolAllow;   //- [C], indoor temperature above which indoor overcooling is allowed
        Real64 TambientOverCoolAllow;  //- [C], ambient temperature above which indoor overcooling is allowed
        Real64 TindoorWHHighPriority;  //- [C], indoor temperature above which water heating has the higher priority
        Real64 TambientWHHighPriority; // ambient temperature above which water heating has the higher priority
        Real64 TlimitSCWH;//temperature limit apply combined sc and wh mode
        Real64 TlimitSCDWH;             // temperature limit apply scdwh mode
        Real64 TlimitDWH; // temperature limit apply DWH mode
        Real64 TregenTarget;//target regeneration temperature
        Real64 Concen_Set;//salt concentration setting 
        Real64 Concen_band;//salt concentration deadband
        
        Real64 EnDehumCoilSize; //sizing factor
        Real64 GridSCCoilSize; // sizing factor
        Real64 GridSHCoilSize;    // sizing factor
        Real64 SCWHCoilSize;    // sizing factor
        Real64 SHCoilSize; // sizing factor
        Real64 DWHCoilSize; // sizing factor
        Real64 SCDWHCoolCoilSize;   // sizing factor
        Real64 SCDWHWHCoilSize; // sizing factor
        Real64 SHDWHHeatCoilSize; // sizing factor
        Real64 SHDWHWHCoilSize;     // sizing factor
        Real64 ChillerSize; //sizing factor for chiller
        Real64 SupWaterCoilWaterFRatio; //water coil air flow ratio
        Real64 SupWaterCoilAirFRatio; //water coil air flow ratio
        int ChillerRunSpeed;//chiller exit target temperature

        bool bIsEnhanchedDumLastMoment; //whether the last moment dehumidification
        bool bIsDWHSeparateunit;//whether DWH is a separate unit
        bool bIsChillerSeparateunit;        // whether chiller is a separate unit
        int IceStoreMode; //IceStorageMode ON; OFF
        Real64 ChargFracLow; //below which charging starts


        Real64 WaterVolSCDWH;
        // limit of water volume before switching from SCDWH to SCWH
        Real64 TimeLimitSHDWH; // time limit before turning from SHDWH to electric heating

        int WHtankType;
        std::string WHtankName;
        std::string WHHPType;
        int WHtankID;
        int LoopNum;
        int LoopSideNum;
        bool IsWHCallAvail;
        // whether water heating call available
        bool CheckWHCall;
        IHPOperationMode CurMode; // current working mode
        Real64 ControlledZoneTemp;
        Real64 WaterFlowAccumVol;
        // water flow accumulated volume
        Real64 SHDWHRunTime;
        Real64 CoolVolFlowScale;
        // max fan cooling volumetric flow rate
        Real64 HeatVolFlowScale;
        // max fan heating volumetric flow rate
        Real64 MaxHeatAirMassFlow;
        // maximum air mass flow rate for heating mode
        Real64 MaxHeatAirVolFlow;
        // maximum air volume flow rate for heating mode
        Real64 MaxCoolAirMassFlow;
        // maximum air mass flow rate for heating mode
        Real64 MaxCoolAirVolFlow;
        // maximum air volume flow rate for heating mode
        bool IHPCoilsSized; // whether IHP coils have been sized
        bool LDLoopChecked; //if liquid dessicant loop has been checked 

        std::string IDFanName;
        // IHP indoor fan name
        int IDFanID;
        // IHP indoor fan index
        int IDFanPlace; // indoor fan placement

        int ODAirInletNodeNum;  // oudoor coil inlet Nod
        int ODAirOutletNodeNum; // oudoor coil outlet Nod
        Real64 TankSourceWaterMassFlowRate;
        // tank source water flow rate
        Real64 AirFlowSavInWaterLoop; // air flow saving for SCWH mode
        Real64 AirFlowSavInAirLoop;   // air flow saving for SCWH mode

        // new output variables
        Real64 AirLoopFlowRate;
        // air loop mass flow rate [kg/s]
        Real64 TotalCoolingRate;
        // total cooling rate [w]
        Real64 TotalWaterHeatingRate;
        // total water heating rate [w]
        Real64 TotalSpaceHeatingRate;
        // total space heating rate [w]
        Real64 TotalPower;
        // total power consumption  [w]
        Real64 TotalLatentLoad;
        // total latent cooling rate [w]
        Real64 Qsource;
        // source energy rate, [w]
        Real64 Energy;
        // total electric energy consumption [J]
        Real64 EnergyLoadTotalCooling;
        // total cooling energy [J]
        Real64 EnergyLoadTotalHeating;
        // total heating energy [J]
        Real64 EnergyLoadTotalWaterHeating;
        // total heating energy [J]
        Real64 EnergyLatent; // total latent energy [J]
        Real64 EnergySource;
        // total source energy
        Real64 TotalCOP; // total COP
        Real64 DehumLDMassFlowRate; 
        Real64 RegenLDMassFlowRate;
        Real64 DehumLDMassFlowSize; 
        Real64 DehumAirMasslowRate;
        Real64 DehumAirMasslowSize; 
        Real64 RegenAirMasslowRate;
        Real64 RegenAirMasslowSize; 
        Real64 SupHeatRate;//supplemental heating rate
        Real64 SaltConcentration;//liquid desiccant salt concentration
        int LDHeatMode; 
        Real64 TankLDMass;//liquid dessicant mass in storgae tank
        Real64 TankSaltMass;//salt mass in the tank
        Real64 CompressorPartLoadRatio;//compressor part load ratio

        // Default Constructor
        IntegratedHeatPumpData()
            : SCCoilIndex(0), EnDehumCoilIndex(0), SHCoilIndex(0), SCWHCoilIndex(0), DWHCoilIndex(0), SCDWHCoolCoilIndex(0), SCDWHWHCoilIndex(0), SHDWHHeatCoilIndex(0), 
              GridSCCoilIndex(0), SHDWHWHCoilIndex(0), AirCoolInletNodeNum(0), AirHeatInletNodeNum(0), AirOutletNodeNum(0),
              WaterInletNodeNum(0), WaterOutletNodeNum(0),
              WaterTankoutNod(0), ModeMatchSCWH(0), MinSpedSCWH(1), MinSpedSCDWH(1), MinSpedSHDWH(1), TindoorOverCoolAllow(0.0),
              TambientOverCoolAllow(0.0), TindoorWHHighPriority(0.0), TambientWHHighPriority(0.0), WaterVolSCDWH(0.0), TimeLimitSHDWH(0.0),
              WHtankType(0), WHtankID(0), LoopNum(0), LoopSideNum(0), IsWHCallAvail(false), CheckWHCall(false), CurMode(IHPOperationMode::IdleMode),
              ControlledZoneTemp(0), WaterFlowAccumVol(0), SHDWHRunTime(0), CoolVolFlowScale(0), HeatVolFlowScale(0), MaxHeatAirMassFlow(0),
              MaxHeatAirVolFlow(0), MaxCoolAirMassFlow(0), MaxCoolAirVolFlow(0), IHPCoilsSized(false), IDFanID(0), IDFanPlace(0),
              ODAirInletNodeNum(0),                                                                                   // oudoor coil inlet Nod
              ODAirOutletNodeNum(0),                                                                                  // oudoor coil outlet Nod
              TankSourceWaterMassFlowRate(0), AirFlowSavInWaterLoop(0), AirFlowSavInAirLoop(0), AirLoopFlowRate(0.0), // air loop mass flow rate
              TotalCoolingRate(0.0),                                                                                  // total cooling rate [w]
              TotalWaterHeatingRate(0.0),                                                                             // total water heating rate [w]
              TotalSpaceHeatingRate(0.0),                                                                             // total space heating rate [w]
              TotalPower(0.0),                                                                                        // total power consumption  [w]
              TotalLatentLoad(0),                                                                                     // total latent cooling rate [w]
              Qsource(0.0),                                                                                           // source energy rate, [w]
              Energy(0.0),                      // total electric energy consumption [J]
              EnergyLoadTotalCooling(0.0),      // total cooling energy [J]
              EnergyLoadTotalHeating(0.0),      // total heating energy [J]
              EnergyLoadTotalWaterHeating(0.0), // total heating energy [J]
              EnergyLatent(0.0),                // total latent energy [J]
              EnergySource(0.0),                // total source energy
              TotalCOP(0.0),                     // total COP
              bIsEnhanchedDumLastMoment(false),   //whether the last moment dehumidification
              SHCoilSize(1.0), 
              DWHCoilSize(1.0),    
              SCWHCoilSize(1.0),
              SCDWHCoolCoilSize(1.0),
              SCDWHWHCoilSize(0.13),  
              SHDWHHeatCoilSize(0.9),
              SHDWHWHCoilSize(0.1),
              EnDehumCoilSize(0.95),
              GridSCCoilSize(1.0), 
              GridSHCoilSize(1.0), 
              ChillerSize(1.0),
              StorageType(IHPStorageType::HOTWATER), 
              LDDehumCoilIndex(0),
              LDRegenCoilIndex(0), 
              EvapCoolCoilIndex(0), 
              ChillerCoilIndex(0), 
              SupWaterCoilIndex(0),
              bIsDWHSeparateunit(false), 
              bIsChillerSeparateunit(false), 
              ChillTankIndex(0),
              TlimitSCWH(110.0), // temperature limit apply combined sc and wh mode
              TlimitSCDWH(150.0),                                 // temperature limit apply scdwh mode
              TlimitDWH(130.0),                                   // temperature limit apply DWH mode
              TregenTarget(180.0),                                // target regeneration temperature
              Concen_Set(0.45),                                  // salt concentration setting
              Concen_band(0.05),                                 // salt concentration deadband
              ChillerRunSpeed(1),     // chiller run speed
              SupWaterCoilWaterFRatio(1.0), SupWaterCoilAirFRatio(1.0),
              IceStoreMode(0), //ice storage off
              ChargFracLow(0.9),//charing fraction
              AirCoolOutletNodeNum(0), 
              DehumLDMassFlowRate(0.0), SupHeatRate(0.0), WaterMiddleNodeNum(0), 
              LDLoopChecked(false),
              SaltConcentration(0.0), LDHeatMode(0), TankLDMass(0.0), TankSaltMass(0.0), 
              DehumLDMassFlowSize(1.0), GridSHCoilIndex(0),
              DehumAirMasslowRate(0.0), DehumAirMasslowSize(1.0), RegenAirMasslowRate(0.0), 
              RegenAirMasslowSize(1.0), RegenLDMassFlowRate(0.0),
              CompressorPartLoadRatio(0.0)
        {
        }
    };

    // Object Data
    extern Array1D<IntegratedHeatPumpData> IntegratedHeatPumps;

    // Functions
    void clear_state();

    void SimIHP(EnergyPlusData &state, std::string const &CompName,   // Coil Name
                int &CompIndex,                // Index for Component name
                int const CyclingScheme,       // Continuous fan OR cycling compressor
                Real64 &MaxONOFFCyclesperHour, // Maximum cycling rate of heat pump [cycles/hr]
                Real64 &HPTimeConstant,        // Heat pump time constant [s]
                Real64 &FanDelayTime,          // Fan delay time, time delay for the HP's fan to
                int const CompOp,              // compressor on/off. 0 = off; 1= on
                Real64 const PartLoadFrac,
                int const SpeedNum,                        // compressor speed number
                Real64 const SpeedRatio,                   // compressor speed ratio
                Real64 const SensLoad,                     // Sensible demand load [W]
                Real64 const LatentLoad,                   // Latent demand load [W]
                bool const IsCallbyWH,                     // whether the call from the water heating loop or air loop, true = from water heating loop
                bool const FirstHVACIteration,             // TRUE if First iteration of simulation
                Optional<Real64 const> OnOffAirFlowRat = _, // ratio of comp on to comp off air flow rate
                bool const bEnhancedDehum = false           // whether it requires enhanced dehumidification
    );

    void SimIHPLiquidStorage(EnergyPlusData &state,
                int &CompIndex,                // Index for Component name
                int const CyclingScheme,       // Continuous fan OR cycling compressor
                Real64 &MaxONOFFCyclesperHour, // Maximum cycling rate of heat pump [cycles/hr]
                Real64 &HPTimeConstant,        // Heat pump time constant [s]
                Real64 &FanDelayTime,          // Fan delay time, time delay for the HP's fan to
                int const CompOp,              // compressor on/off. 0 = off; 1= on
                Real64 const PartLoadFrac,
                int const SpeedNum,            // compressor speed number
                Real64 const SpeedRatio,       // compressor speed ratio
                Real64 const SensLoad,         // Sensible demand load [W]
                Real64 const LatentLoad,       // Latent demand load [W]
                bool const IsCallbyWH,         // whether the call from the water heating loop or air loop, true = from water heating loop
                bool const FirstHVACIteration, // TRUE if First iteration of simulation
                Optional<Real64 const> OnOffAirFlowRat = _, // ratio of comp on to comp off air flow rate
                bool const bEnhancedDehum = false           // whether it requires enhanced dehumidification
    );

    void GetIHPInput(EnergyPlusData &state);

    void SizeIHP(EnergyPlusData &state, int const CoilNum);

    void InitializeIHP(EnergyPlusData &state, int const DXCoilNum);

    void UpdateIHP(EnergyPlusData &state, int const DXCoilNum);

    void DecideWorkMode(EnergyPlusData &state, int const DXCoilNum,
                        Real64 const SensLoad,  // Sensible demand load [W]
                        Real64 const LatentLoad // Latent demand load [W]
    );

    void UpdateIceStorage(EnergyPlusData &state, int const DXCoilNum);//update ice storage fraction

    //check liquid desiccant heating call
    bool CheckLDWHCall(EnergyPlusData &state,
                        int const DXCoilNum,
                       Real64 const SensLoad,  // Sensible demand load [W]
                       Real64 const LatentLoad // Latent demand load [W]
    );

    IHPOperationMode GetCurWorkMode(EnergyPlusData &state, int const DXCoilNum);

    int GetLowSpeedNumIHP(EnergyPlusData &state, int const DXCoilNum);

    int GetMaxSpeedNumIHP(EnergyPlusData &state, int const DXCoilNum);

    Real64 GetAirVolFlowRateIHP(EnergyPlusData &state, int const DXCoilNum,
                                int const SpeedNum,
                                Real64 const SpeedRatio,
                                bool const IsCallbyWH // whether the call from the water heating loop or air loop, true = from water heating loop
    );

    Real64 GetWaterVolFlowRateIHP(EnergyPlusData &state, int const DXCoilNum,
                                  int const SpeedNum,
                                  Real64 const SpeedRatio,
                                  bool const IsCallbyWH // whether the call from the water heating loop or air loop, true = from water heating loop
    );

    Real64 GetAirMassFlowRateIHP(EnergyPlusData &state, int const DXCoilNum,
                                 int const SpeedNum,
                                 Real64 const SpeedRatio,
                                 bool const IsCallbyWH // whether the call from the water heating loop or air loop, true = from water heating loop
    );

    bool IHPInModel(EnergyPlusData &state);

    int GetCoilIndexIHP(EnergyPlusData &state,
                        std::string const &CoilType, // must match coil types in this module
                        std::string const &CoilName, // must match coil names for the coil type
                        bool &ErrorsFound            // set to true if problem
    );

    int GetCoilInletNodeIHP(EnergyPlusData &state,
                            std::string const &CoilType, // must match coil types in this module
                            std::string const &CoilName, // must match coil names for the coil type
                            bool &ErrorsFound            // set to true if problem
    );

    int GetDWHCoilInletNodeIHP(EnergyPlusData &state,
                               std::string const &CoilType, // must match coil types in this module
                               std::string const &CoilName, // must match coil names for the coil type
                               bool &ErrorsFound            // set to true if problem
    );

    int GetDWHCoilOutletNodeIHP(EnergyPlusData &state,
                                std::string const &CoilType, // must match coil types in this module
                                std::string const &CoilName, // must match coil names for the coil type
                                bool &ErrorsFound            // set to true if problem
    );

    Real64 GetDWHCoilCapacityIHP(EnergyPlusData &state, std::string const &CoilType, // must match coil types in this module
                                 std::string const &CoilName, // must match coil names for the coil type
                                 IHPOperationMode const Mode, // mode coil type
                                 bool &ErrorsFound            // set to true if problem
    );

    int GetIHPDWHCoilPLFFPLR(EnergyPlusData &state,
                             std::string const &CoilType, // must match coil types in this module
                             std::string const &CoilName, // must match coil names for the coil type
                             IHPOperationMode const Mode, // mode coil type
                             bool &ErrorsFound            // set to true if problem
    );

    int GetGridLoadCtrlModeIHP(EnergyPlusData &state,
                                const int DXCoilNum // coil index No
    );

    void ClearCoils(EnergyPlusData &state, int const DXCoilNum // coil ID
    );

} // namespace IntegratedHeatPump

} // namespace EnergyPlus

#endif
