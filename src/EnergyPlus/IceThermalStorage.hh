// EnergyPlus, Copyright (c) 1996-2019, The Board of Trustees of the University of Illinois,
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

#ifndef IceThermalStorage_hh_INCLUDED
#define IceThermalStorage_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>

// EnergyPlus Headers
#include <EnergyPlus/DataGlobals.hh>
#include <EnergyPlus/EnergyPlus.hh>

namespace EnergyPlus {

namespace IceThermalStorage {

    // Using/Aliasing

    // Data
    // MODULE PARAMETER DEFINITIONS
    extern std::string const cIceStorageSimple;
    extern std::string const cIceStorageDetailed;

    extern int const IceStorageType_Simple;
    extern int const IceStorageType_Detailed;

    extern int const CurveVarsFracChargedLMTD;
    extern int const CurveVarsFracDischargedLMTD;
    extern int const CurveVarsLMTDMassFlow;
    extern int const CurveVarsLMTDFracCharged;
    
    extern int const DetIceInsideMelt;  // Inside melt system--charge starting with bare coil
    extern int const DetIceOutsideMelt; // Outside melt system--charge from existing ice layer on coil

    // ITS parameter
    extern Real64 const modFreezTemp;    // Water freezing Temperature, 0[C]
    extern Real64 const modFreezTempIP;  // Water freezing Temperature, 32[F]
    extern Real64 const modTimeInterval; // Time Interval (1 hr) [s]
    extern int const ITSType_IceOnCoilInternal;
    extern int const ITSType_IceOnCoilExternal;
    // Conversion parameter
    extern Real64 const modEpsLimitForX;         // 0.02  ! See Dion's code as eps1
    extern Real64 const modEpsLimitForDisCharge; // 0.20  ! See Dion's code as eps2
    extern Real64 const modEpsLimitForCharge;    // 0.20  ! See Dion's code as eps3

    // Parameter used by the Detailed Ice Storage Model
    extern Real64 const modDeltaTofMin; // Minimum allowed outlet side temperature difference [C]
    // This is (Tout - Tfreezing)
    extern Real64 const modDeltaTifMin; // Minimum allowed inlet side temperature difference [C]
    // This is (Tin - Tfreezing)

    // DERIVED TYPE DEFINITIONS
    // TYPE ITSSetCap is used for information of ITS plant in Loop, Brach, and Components.
    //  TYPE ITSSetCapData
    //    LOGICAL :: ITSFlag    = .FALSE.
    //    INTEGER :: LoopNum    =0
    //    INTEGER :: BranchNum  =0
    //    INTEGER :: CompNum    =0
    //  END TYPE ITSSetCapData

    // TYPE (ITSSetCapData), SAVE                   :: ITSSetCap=ITSSetCapData(.FALSE.,0,0,0)

    // MODULE VARIABLE DECLARATIONS:
    extern bool modResetXForITSFlag;

    // Input data
    extern Real64 modITSNomCap;  // Design nominal capacity of Ice Thermal Storage [J] (user input in GJ)
    extern int modInletNodeNum;  // Node number on the inlet side of the plant
    extern int modOutletNodeNum; // Node number on the inlet side of the plant

    // ITS numbers and FoundOrNot
    extern int IceNum;
    extern int modNumIceStorages;
    extern int modNumDetIceStorages;
    extern int modTotalIceStorages;

    // ITS UAice and HLoss
    extern Real64 modUAIceCh;    // Charging Ice Thermal Storage overall heat transfer coefficient [W/C]
    extern Real64 modUAIceDisCh; // Discharging Ice Thermal Storage overall heat transfer coefficient [W/C]
    extern Real64 modHLoss;      // ITS Heat Loss

    // ITS State
    extern Real64 modXCurIceFrac; // Current Fraction of Ice Thermal Storage remaining [fraction]
    extern Real64 modU;           // Adjusted input U after reading U Schedule [fraction]
    extern Real64 modUrate;       // Final Urate adjusted Urate based on Error protection (I) [fraction] by HOUR

    // ITS status information
    extern Real64 modITSMassFlowRate;       // ITS water mass flow rate [kg/s]
    extern Real64 modITSInletTemp;          // ITS inlet water temperature [C]
    extern Real64 modITSOutletTemp;         // ITS outlet water temperature [C]
    extern Real64 modITSOutletSetPointTemp; // ITS outlet water temperature setpoint [C]
    extern Real64 modITSCoolingRate;        // ITS Discharge(-)/Charge(+) rate [W]
    extern Real64 modITSCoolingEnergy;
    extern Real64 modChillerOutletTemp; // Chiller outlet brine temperature [C]
    extern Array1D_bool modCheckEquipName;

    struct IceStorageType {
        enum {
            Simple,
            Detailed
        };
    };
    
    struct CurveVars {
        enum {
            FracChargedLMTD,
            FracDischargedLMTD,
            LMTDMassFlow,
            LMTDFracCharged
        };
    };
    
    struct DetIce {
        enum {
            InsideMelt,  // Inside melt system--charge starting with bare coil
            OutsideMelt // Outside melt system--charge from existing ice layer on coil
        };
    };

    struct ITSType {
        enum {
            IceOnCoilInternal,
            IceOnCoilExternal
        };
    };

    struct IceStorageMapping
    {
        // Members
        // Input data
        std::string Name; // User identifier
        std::string StorageType;
        int StorageType_Num;
        int LocalEqNum;

        // Default Constructor
        IceStorageMapping() : StorageType_Num(0), LocalEqNum(0)
        {
        }
    };

    struct IceStorageSpecs
    {
        // Members
        // Input data
        std::string Name;     // User identifier
        std::string ITSType;  // Ice Thermal Storage Type
        int ITSType_Num;      // Storage Type as number (IceOnCoilInternal,IceOnCoilExternal)
        int MapNum;           // Number to Map structure
        int UratePtr;         // Charging/Discharging SchedulePtr: u value schedule
        Real64 ITSNomCap;     // Design nominal capacity of Ice Thermal Storage [J] (user input in GJ)
        int PltInletNodeNum;  // Node number on the inlet side of the plant
        int PltOutletNodeNum; // Node number on the outlet side of the plant
        // loop topology variables
        int LoopNum;
        int LoopSideNum;
        int BranchNum;
        int CompNum;
        Real64 DesignMassFlowRate;

        // Default Constructor
        IceStorageSpecs()
            : ITSType_Num(0), MapNum(0), UratePtr(0), ITSNomCap(0.0), PltInletNodeNum(0), PltOutletNodeNum(0), LoopNum(0), LoopSideNum(0),
              BranchNum(0), CompNum(0), DesignMassFlowRate(0.0)
        {
        }
    };

    struct DetailedIceStorageData
    {
        // Members
        // Input data
        std::string Name;         // User identifier
        std::string ScheduleName; // User identifier
        int ScheduleIndex;        // Plant inlet node number for ice storage unit
        Real64 NomCapacity;       // Design storage capacity of Ice Thermal Storage system [W-hr]
        // (User input for this parameter in GJ--need to convert to W-hr)
        int PlantInNodeNum;  // Plant inlet node number for ice storage unit
        int PlantOutNodeNum; // Plant outlet node number for ice storage unit
        int PlantLoopNum;
        int PlantLoopSideNum;
        int PlantBranchNum;
        int PlantCompNum;
        Real64 DesignMassFlowRate;
        int MapNum;                     // Number to Map structure
        std::string DischargeCurveName; // Curve name for discharging (used to find the curve index)
        int DischargeCurveNum;          // Curve index for discharging
        int DischargeCurveTypeNum{};      // Integer version of discharging curve independent variables type
        std::string ChargeCurveName;    // Curve name for charging (used to find the curve index)
        int ChargeCurveNum;             // Curve index for charging
        int ChargeCurveTypeNum{};         // Integer version of charging curve independent variables type
        Real64 CurveFitTimeStep;        // Time step used to generate performance data [hours]
        Real64 DischargeParaElecLoad;   // Parasitic electric load duing discharging [dimensionless]
        // (This is multiplied by the tank capacity to obtain elec consump)
        Real64 ChargeParaElecLoad; // Parasitic electric load duing charging [dimensionless]
        // (This is multiplied by the tank capacity to obtain elec consump)
        Real64 TankLossCoeff; // Fraction of total storage capacity lost per hour [1/hours]
        Real64 FreezingTemp;  // Freezing/melting temperature of ice storage unit [C]
        // Reporting data
        Real64 CompLoad;                  // load requested by plant [W]
        Real64 IceFracChange;             // Change in fraction of ice stored during the time step [fraction]
        Real64 IceFracRemaining;          // Fraction of ice remaining in storage [fraction]
        std::string ThawProcessIndicator; // User input determining whether system is inside or outside melt
        int ThawProcessIndex;             // Conversion of thaw process indicator to integer index
        Real64 IceFracOnCoil;             // Fraction of ice on the coil (affects charging) [fraction]
        Real64 DischargingRate;           // Rate at which energy is being added (thawing) to ice unit [W]
        Real64 DischargingEnergy;         // Total energy added to the ice storage unit [J]
        Real64 ChargingRate;              // Rate at which energy is removed (freezing) to ice unit [W]
        Real64 ChargingEnergy;            // Total energy removed from ice storage unit [J]
        Real64 MassFlowRate;              // Total mass flow rate to component [kg/s]
        Real64 BypassMassFlowRate;        // Mass flow rate that bypasses the ice unit locally [kg/s]
        Real64 TankMassFlowRate;          // Mass flow rate through the ice storage unit [kg/s]
        Real64 InletTemp;                 // Component inlet temperature (same as bypass temperature) [C]
        Real64 OutletTemp;                // Component outlet temperature (blended) [C]
        Real64 TankOutletTemp;            // Ice storage unit outlet temperature [C]
        Real64 ParasiticElecRate;         // Parasitic electrical energy rate consumed by ice storage [W]
        Real64 ParasiticElecEnergy;       // Total parasitic electrical energy consumed by ice storage [J]
        int DischargeIterErrors;          // Number of max iterations exceeded errors during discharging
        int DischargeErrorCount;          // Index for error counting routine
        int ChargeIterErrors;             // Number of max iterations exceeded errors during charging
        int ChargeErrorCount;             // Index for error counting routine

        // Default Constructor
        DetailedIceStorageData()
            : ScheduleIndex(0), NomCapacity(0.0), PlantInNodeNum(0), PlantOutNodeNum(0), PlantLoopNum(0), PlantLoopSideNum(0), PlantBranchNum(0),
              PlantCompNum(0), DesignMassFlowRate(0.0), MapNum(0), DischargeCurveNum(0), ChargeCurveNum(0), CurveFitTimeStep(1.0),
              DischargeParaElecLoad(0.0), ChargeParaElecLoad(0.0), TankLossCoeff(0.0), FreezingTemp(0.0), CompLoad(0.0), IceFracChange(0.0),
              IceFracRemaining(1.0), ThawProcessIndex(0), IceFracOnCoil(1.0), DischargingRate(0.0), DischargingEnergy(0.0), ChargingRate(0.0),
              ChargingEnergy(0.0), MassFlowRate(0.0), BypassMassFlowRate(0.0), TankMassFlowRate(0.0), InletTemp(0.0), OutletTemp(0.0),
              TankOutletTemp(0.0), ParasiticElecRate(0.0), ParasiticElecEnergy(0.0), DischargeIterErrors(0), DischargeErrorCount(0),
              ChargeIterErrors(0), ChargeErrorCount(0)
        {
        }
    };

    struct ReportVars
    {
        // Members
        Real64 MyLoad;            // load requested by plant [W]
        Real64 U;                 // [fraction]
        Real64 Urate;             // [fraction]
        Real64 IceFracRemain;     // Fraction of ice remaining in storage [fraction]
        Real64 ITSCoolingRate;    // [W]
        Real64 ITSCoolingEnergy;  // [J]
        Real64 ITSChargingRate;   // [W]
        Real64 ITSChargingEnergy; // [J]
        Real64 ITSmdot;           // [kg/s]
        Real64 ITSInletTemp;      // [C]
        Real64 ITSOutletTemp;     // [C]

        // Default Constructor
        ReportVars()
            : MyLoad(0.0), U(0.0), Urate(0.0), IceFracRemain(0.0), ITSCoolingRate(0.0), ITSCoolingEnergy(0.0), ITSChargingRate(0.0),
              ITSChargingEnergy(0.0), ITSmdot(0.0), ITSInletTemp(0.0), ITSOutletTemp(0.0)
        {
        }
    };

    // Object Data
    extern Array1D<IceStorageSpecs> IceStorage;        // dimension to number of machines
    extern Array1D<ReportVars> IceStorageReport;       // dimension to number of machines
    extern Array1D<DetailedIceStorageData> DetIceStor; // Derived type for detailed ice storage model
    extern Array1D<IceStorageMapping> IceStorageTypeMap;

    // Functions

    void clear_state();

    void SimIceStorage(std::string const &IceStorageType,
                       std::string const &IceStorageName,
                       int &CompIndex,
                       bool RunFlag,
                       bool FirstIteration,
                       bool InitLoopEquip,
                       Real64 &MyLoad);

    void SimDetailedIceStorage();

    //******************************************************************************

    void GetIceStorageInput();

    //******************************************************************************

    void InitDetailedIceStorage();

    void InitSimpleIceStorage();

    //******************************************************************************

    void CalcIceStorageCapacity(int IceStorageType, Real64 &MaxCap, Real64 &MinCap, Real64 &OptCap);

    //******************************************************************************

    void CalcIceStorageDormant(int IceStorageType, // BY ZG
                               int &iceNum);

    //******************************************************************************

    void CalcIceStorageCharge(int IceStorageType, // BY ZG
                              int &iceNum);

    //******************************************************************************

    void CalcQiceChargeMaxByChiller(int &iceNum, Real64 &QiceMaxByChiller);

    //******************************************************************************

    void CalcQiceChargeMaxByITS(int &IceNum,
                                Real64 chillerOutletTemp, // [degC]
                                Real64 &QiceMaxByITS            // [W]
    );

    //******************************************************************************

    void CalcIceStorageDischarge(int IceStorageType,  // by ZG
                                 int iceNum,          // ice storage number
                                 Real64 MyLoad,       // operating load
                                 bool RunFlag,        // TRUE when ice storage operating
                                 bool FirstIteration, // TRUE when first iteration of timestep
                                 Real64 MaxCap        // Max possible discharge rate (positive value)
    );

    //******************************************************************************

    void CalcQiceDischageMax(Real64 &QiceMin);

    //******************************************************************************

    void CalcUAIce(int iceNum, Real64 XCurIceFrac_loc, Real64 &UAIceCh_loc, Real64 &UAIceDisCh_loc, Real64 &HLoss_loc);

    Real64 CalcDetIceStorLMTDstar(Real64 Tin,  // ice storage unit inlet temperature
                                  Real64 Tout, // ice storage unit outlet (setpoint) temperature
                                  Real64 Tfr   // freezing temperature
    );

    Real64 CalcQstar(int CurveIndex,      // curve index
                     int CurveIndVarType, // independent variable type for ice storage
                     Real64 FracCharged,  // fraction charged for ice storage unit
                     Real64 LMTDstar,     // normalized log mean temperature difference across the ice storage unit
                     Real64 MassFlowstar  // normalized mass flow rate through the ice storage unit
    );
    
    // *****************************************************************************

    Real64 TempSItoIP(Real64 Temp);

    // *****************************************************************************

    Real64 TempIPtoSI(Real64 Temp);

    // *****************************************************************************

    void UpdateNode(Real64 MyLoad, bool RunFlag, int Num);

    // *****************************************************************************

    void RecordOutput(int iceNum, Real64 MyLoad, bool RunFlag);

    // *****************************************************************************

    void UpdateIceFractions();

    void UpdateDetailedIceStorage();

    void ReportDetailedIceStorage();

} // namespace IceThermalStorage

} // namespace EnergyPlus

#endif
