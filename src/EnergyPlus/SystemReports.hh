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

#ifndef SystemReports_hh_INCLUDED
#define SystemReports_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>

// EnergyPlus Headers
#include <EnergyPlus/Data/BaseData.hh>
#include <EnergyPlus/DataGlobalConstants.hh>
#include <EnergyPlus/EPVector.hh>
#include <EnergyPlus/EnergyPlus.hh>

namespace EnergyPlus {

// Forward declarations
struct EnergyPlusData;

namespace SystemReports {

    enum class iEndUseType
    {
        NoHeatNoCool,
        CoolingOnly,
        HeatingOnly,
        HeatAndCool,
    };

    struct Energy
    {
        // Members
        Real64 TotDemand;
        Real64 Elec;
        Real64 Gas;
        Real64 Purch;
        Real64 Other;

        // Default Constructor
        Energy() : TotDemand(0.0), Elec(0.0), Gas(0.0), Purch(0.0), Other(0.0)
        {
        }
    };

    struct CoilType
    {
        // Members
        Energy DecreasedCC; // LoadMetByVent
        Energy DecreasedHC; // LoadMetByVent
        Energy IncreasedCC; // LoadIncreasedVent
        Energy IncreasedHC; // LoadAddedByVent
        Energy ReducedByCC; // LoadAddedByVent
        Energy ReducedByHC; // LoadAddedByVent

        // Default Constructor
        CoilType() = default;
    };

    struct SummarizeLoads
    {
        // Members
        CoilType Load;             // LoadMetByVent
        CoilType NoLoad;           // LoadMetByVentNoLoad
        CoilType ExcessLoad;       // LoadAddedByVentOvercool
        CoilType PotentialSavings; // LoadAddedByVentCoolLost
        CoilType PotentialCost;    // LoadAddedByVentHeatLost

        // Default Constructor
        SummarizeLoads() = default;
    };

    struct CompTypeError
    {
        // Members
        std::string CompType;
        int CompErrIndex;

        // Default Constructor
        CompTypeError() : CompErrIndex(0)
        {
        }
    };

    struct SysPreDefRepType
    {
        Real64 SysMechVentTotal = 0.0;           // air loop mechanical vent total volume OA at standard density {m3}
        Real64 SysNatVentTotal = 0.0;            // air loop natural vent total volume OA at standard density {m3}
        Real64 SysTargetVentTotalVoz = 0.0;      // air loop target ventilation ventilation flow based on 62.1 Voz-dyn {m3}
        Real64 SysTimeBelowVozDynTotal = 0.0;    // time [hrs] that mechanical+natural ventilation is < VozTarget - 1%
        Real64 SysTimeAtVozDynTotal = 0.0;       // time [hrs] that mechanical+natural ventilation is = VozTarget within 1% and > zero
        Real64 SysTimeAboveVozDynTotal = 0.0;    // time [hrs] that mechanical+natural ventilation is > VozTarget + 1%
        Real64 SysMechVentTotalOcc = 0.0;        // air loop mechanical vent total volume OA at standard density {m3} during occupied
        Real64 SysNatVentTotalOcc = 0.0;         // air loop natural vent total volume OA at standard density {m3} during occupied
        Real64 SysTargetVentTotalVozOcc = 0.0;   // air loop target ventilation ventilation flow based on 62.1 Voz-dyn {m3} during occupied
        Real64 SysTimeBelowVozDynTotalOcc = 0.0; // time [hrs] that mechanical+natural ventilation is < VozTarget - 1% during occupied
        Real64 SysTimeAtVozDynTotalOcc = 0.0;    // time [hrs] that mechanical+natural ventilation is = VozTarget within 1% and > zero during occ
        Real64 SysTimeAboveVozDynTotalOcc = 0.0; // time [hrs] that mechanical+natural ventilation is > VozTarget + 1% during occupied
        Real64 SysTimeVentUnoccTotal = 0.0;      // time [hrs] that mechanical+natural ventilation is > zero during unoccupied
        Real64 SysTimeOccupiedTotal = 0.0;       // time [hrs] that any zone is occupied

        std::vector<Real64> SysTimeAtOALimit = {0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0};    // time [hrs] at limit [n]
        std::vector<Real64> SysTimeAtOALimitOcc = {0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0}; // time [hrs] at limit [n] during occupied
        std::vector<Real64> SysMechVentTotAtLimitOcc = {
            0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0}; // air loop mech vent total vol OA at limit [n] {m3} during occupied
    };

    // Functions

    void InitEnergyReports(EnergyPlusData &state);

    void FindFirstLastPtr(EnergyPlusData &state, int &LoopType, int &LoopNum, int &ArrayCount, int &LoopCount, bool &ConnectionFlag);

    void UpdateZoneCompPtrArray(EnergyPlusData &state,
                                int &Idx,
                                int const ListNum,
                                int const AirDistUnitNum,
                                int const PlantLoopType,
                                int const PlantLoop,
                                int const PlantBranch,
                                int const PlantComp);

    void UpdateZoneSubCompPtrArray(EnergyPlusData &state,
                                   int &Idx,
                                   int const ListNum,
                                   int const AirDistUnitNum,
                                   int const SubCompNum,
                                   int const PlantLoopType,
                                   int const PlantLoop,
                                   int const PlantBranch,
                                   int const PlantComp);

    void UpdateZoneSubSubCompPtrArray(EnergyPlusData &state,
                                      int &Idx,
                                      int const ListNum,
                                      int const AirDistUnitNum,
                                      int const SubCompNum,
                                      int const SubSubCompNum,
                                      int const PlantLoopType,
                                      int const PlantLoop,
                                      int const PlantBranch,
                                      int const PlantComp);

    void UpdateAirSysCompPtrArray(EnergyPlusData &state,
                                  int &Idx,
                                  int const AirLoopNum,
                                  int const BranchNum,
                                  int const CompNum,
                                  int const PlantLoopType,
                                  int const PlantLoop,
                                  int const PlantBranch,
                                  int const PlantComp);

    void UpdateAirSysSubCompPtrArray(EnergyPlusData &state,
                                     int &Idx,
                                     int const AirLoopNum,
                                     int const BranchNum,
                                     int const CompNum,
                                     int const SubCompNum,
                                     int const PlantLoopType,
                                     int const PlantLoop,
                                     int const PlantBranch,
                                     int const PlantComp);

    void UpdateAirSysSubSubCompPtrArray(EnergyPlusData &state,
                                        int &Idx,
                                        int const AirLoopNum,
                                        int const BranchNum,
                                        int const CompNum,
                                        int const SubCompNum,
                                        int const SubSubCompNum,
                                        int const PlantLoopType,
                                        int const PlantLoop,
                                        int const PlantBranch,
                                        int const PlantComp);

    void AllocateAndSetUpVentReports(EnergyPlusData &state);

    void CreateEnergyReportStructure(EnergyPlusData &state);

    // End Initialization Section of the Module
    //******************************************************************************

    // Beginning of Reporting subroutines for the SimAir Module
    // *****************************************************************************

    void ReportSystemEnergyUse(EnergyPlusData &state);

    void CalcSystemEnergyUse(EnergyPlusData &state,
                             bool const CompLoadFlag,
                             int const AirLoopNum,
                             std::string const &CompType,
                             DataGlobalConstants::ResourceType const EnergyType,
                             Real64 const CompLoad,
                             Real64 const CompEnergy);

    void ReportMaxVentilationLoads(EnergyPlusData &state);

    void MatchPlantSys(EnergyPlusData &state,
                       int const AirLoopNum, // counter for zone air distribution inlets
                       int const BranchNum   // counter for zone air distribution inlets
    );

    void FindDemandSideMatch(EnergyPlusData &state,
                             std::string const &CompType, // Inlet node of the component to find the match of
                             std::string_view CompName, // Outlet node of the component to find the match of
                             bool &MatchFound,            // Set to .TRUE. when a match is found
                             int &MatchLoopType,          // Loop number of the match
                             int &MatchLoop,              // Loop number of the match
                             int &MatchBranch,            // Branch number of the match
                             int &MatchComp               // Component number of the match
    );

    void ReportAirLoopConnections(EnergyPlusData &state);

    //        End of Reporting subroutines for the SimAir Module
    // *****************************************************************************

    struct IdentifyLoop
    {
        // Members
        int LoopNum;
        int LoopType;

        // Default Constructor
        IdentifyLoop() : LoopNum(0), LoopType(0)
        {
        }
    };

} // namespace SystemReports

struct SystemReportsData : BaseGlobalStruct
{

    Array1D<Real64> MaxCoolingLoadMetByVent;
    Array1D<Real64> MaxCoolingLoadAddedByVent;
    Array1D<Real64> MaxOvercoolingByVent;
    Array1D<Real64> MaxHeatingLoadMetByVent;
    Array1D<Real64> MaxHeatingLoadAddedByVent;
    Array1D<Real64> MaxOverheatingByVent;
    Array1D<Real64> MaxNoLoadHeatingByVent;
    Array1D<Real64> MaxNoLoadCoolingByVent;

    Array1D<Real64> SysTotZoneLoadHTNG;
    Array1D<Real64> SysTotZoneLoadCLNG;
    Array1D<Real64> SysOALoadHTNG;
    Array1D<Real64> SysOALoadCLNG;
    Array1D<Real64> SysTotHTNG;
    Array1D<Real64> SysTotCLNG;

    Array1D<Real64> SysTotH2OHOT;
    Array1D<Real64> SysTotH2OCOLD;
    Array1D<Real64> SysTotElec;
    Array1D<Real64> SysTotNaturalGas;
    Array1D<Real64> SysTotPropane;
    Array1D<Real64> SysTotSteam;

    Array1D<Real64> SysHumidHTNG;
    Array1D<Real64> SysHumidElec;
    Array1D<Real64> SysHumidNaturalGas;
    Array1D<Real64> SysHumidPropane;
    Array1D<Real64> SysEvapCLNG;
    Array1D<Real64> SysEvapElec;
    Array1D<Real64> SysHeatExHTNG;
    Array1D<Real64> SysHeatExCLNG;
    Array1D<Real64> DesDehumidCLNG;
    Array1D<Real64> DesDehumidElec;
    Array1D<Real64> SysSolarCollectHeating;
    Array1D<Real64> SysSolarCollectCooling;
    Array1D<Real64> SysUserDefinedTerminalHeating;
    Array1D<Real64> SysUserDefinedTerminalCooling;

    Array1D<Real64> SysFANCompHTNG;
    Array1D<Real64> SysFANCompElec;
    Array1D<Real64> SysCCCompCLNG;
    Array1D<Real64> SysCCCompH2OCOLD;
    Array1D<Real64> SysCCCompElec;
    Array1D<Real64> SysHCCompH2OHOT;
    Array1D<Real64> SysHCCompElec;
    Array1D<Real64> SysHCCompElecRes;
    Array1D<Real64> SysHCCompHTNG;
    Array1D<Real64> SysHCCompNaturalGas;
    Array1D<Real64> SysHCCompPropane;
    Array1D<Real64> SysHCCompSteam;
    Array1D<Real64> SysDomesticH2O;

    Array1D<Real64> ZoneOAMassFlow;               // zone mech vent mass flow rate {kg/s}
    Array1D<Real64> ZoneOAMass;                   // zone mech vent total mass for time {kg}
    Array1D<Real64> ZoneOAVolFlowStdRho;          // zone mech vent volume flow rate at standard density {m3/s}
    Array1D<Real64> ZoneOAVolStdRho;              // zone mech vent total volume OA at standard density {m3}
    Array1D<Real64> ZoneOAVolFlowCrntRho;         // zone mech vent volume flow rate at current density {m3/s}
    Array1D<Real64> ZoneOAVolCrntRho;             // zone mech vent total volume OA at current density {m3}
    Array1D<Real64> ZoneMechACH;                  // zone mech vent air changes per hour {ACH}
    Array1D<Real64> ZoneTargetVentilationFlowVoz; // zone target ventilation ventilation flow based on 62.1 Voz-dyn {m3/s}
    Array1D<Real64> ZoneTimeBelowVozDyn;          // time [hrs] that mechanical+natural ventilation is < VozTarget - 1%
    Array1D<Real64> ZoneTimeAtVozDyn;             // time [hrs] that mechanical+natural ventilation is = VozTarget within 1% and > zero
    Array1D<Real64> ZoneTimeAboveVozDyn;          // time [hrs] that mechanical+natural ventilation is > VozTarget + 1%
    Array1D<Real64> ZoneTimeVentUnocc;            // time [hrs] that mechanical+natural ventilation is > zero during unoccupied
    Real64 AnyZoneTimeBelowVozDyn = 0.0;          // time [hrs] that any zone mechanical+natural ventilation is < VozTarget - 1%
    Real64 AllZonesTimeAtVozDyn = 0.0;            // time [hrs] that all zones mechanical+natural ventilation is = VozTarget within 1% and > zero
    Real64 AnyZoneTimeAboveVozDyn = 0.0;          // time [hrs] that any zone mechanical+natural ventilation is > VozTarget + 1%
    Real64 AnyZoneTimeVentUnocc = 0.0;            // time [hrs] that any zone mechanical+natural ventilation is > zero during unoccupied
    Real64 AnyZoneTimeBelowVozDynOcc = 0.0;       // time [hrs] that any zone mechanical+natural ventilation is < VozTarget - 1% during occupied
    Real64 AllZonesTimeAtVozDynOcc = 0.0;         // time [hrs] that all zones mech+nat vent is = VozTarget within 1% and > zero during occupied
    Real64 AnyZoneTimeAboveVozDynOcc = 0.0;       // time [hrs] that any zone mechanical+natural ventilation is > VozTarget + 1% during occupied

    Array1D<Real64> SysMechVentFlow;             // air loop mechanical vent total volume OA at standard density {m3/s}
    Array1D<Real64> SysNatVentFlow;              // air loop natural vent total volume OA at standard density {m3/s}
    Array1D<Real64> SysTargetVentilationFlowVoz; // air loop target ventilation ventilation flow based on 62.1 Voz-dyn {m3/s}
    Array1D<Real64> SysTimeBelowVozDyn;          // time [hrs] that mechanical+natural ventilation is < VozTarget - 1%
    Array1D<Real64> SysTimeAtVozDyn;             // time [hrs] that mechanical+natural ventilation is = VozTarget within 1% and > zero
    Array1D<Real64> SysTimeAboveVozDyn;          // time [hrs] that mechanical+natural ventilation is > VozTarget + 1%
    Array1D<Real64> SysTimeVentUnocc;            // time [hrs] that mechanical+natural ventilation is > zero during unoccupied
    Array1D<bool> SysAnyZoneOccupied;            // true if any zone on system is occupied

    bool AirLoopLoadsReportEnabled = true;
    bool VentLoadsReportEnabled = true;
    bool VentEnergyReportEnabled = false;
    bool VentReportStructureCreated = false;
    int TotalLoopConnects = 0; // Total number of loop connections
    int MaxLoopArraySize = 100;
    int MaxCompArraySize = 500;

    Array1D_int SetBackCounter;
    Array1D_int HeatCoolFlag;
    Array1D_int FirstHeatCoolFlag;
    Array1D_int FirstHeatCoolHour;
    Array1D_int LastHeatCoolFlag;
    Array1D_int LastHeatCoolHour;
    Array1D_bool NoLoadFlag;
    Array1D_bool UnmetLoadFlag;

    EPVector<SystemReports::SummarizeLoads> Vent;
    EPVector<SystemReports::SysPreDefRepType> SysPreDefRep;

    bool OneTimeFlag_FindFirstLastPtr = true;
    bool OneTimeFlag_InitEnergyReports = true;
    bool OneTimeFlag_UpdateZoneCompPtrArray = true;
    int ArrayLimit_UpdateZoneCompPtrArray = 100;
    int ArrayCounter_UpdateZoneCompPtrArray = 1;
    bool OneTimeFlag_UpdateZoneSubCompPtrArray = true;
    int ArrayLimit_UpdateZoneSubCompPtrArray = 100;
    int ArrayCounter_UpdateZoneSubCompPtrArray = 1;
    bool OneTimeFlag_UpdateZoneSubSubCompPtrArray = true;
    int ArrayLimit_UpdateZoneSubSubCompPtrArray = 100;
    int ArrayCounter_UpdateZoneSubSubCompPtrArray = 1;
    bool OneTimeFlag_UpdateAirSysCompPtrArray = true;
    int ArrayLimit_UpdateAirSysCompPtrArray = 100;
    int ArrayCounter_UpdateAirSysCompPtrArray = 1;
    bool OneTimeFlag_UpdateAirSysSubCompPtrArray = true;
    int ArrayLimit_UpdateAirSysSubCompPtrArray = 100;
    int ArrayCounter_UpdateAirSysSubCompPtrArray = 1;
    bool OneTimeFlag_UpdateAirSysSubSubCompPtrArray = true;
    int ArrayLimit_UpdateAirSysSubSubCompPtrArray = 100;
    int ArrayCounter_UpdateAirSysSubSubCompPtrArray = 1;
    int NumCompTypes = 0;
    Array1D<SystemReports::CompTypeError> CompTypeErrors = Array1D<SystemReports::CompTypeError>(100);
    Array1D<SystemReports::IdentifyLoop> LoopStack;

    void clear_state() override
    {
        this->MaxCoolingLoadMetByVent.deallocate();
        this->MaxCoolingLoadAddedByVent.deallocate();
        this->MaxOvercoolingByVent.deallocate();
        this->MaxHeatingLoadMetByVent.deallocate();
        this->MaxHeatingLoadAddedByVent.deallocate();
        this->MaxOverheatingByVent.deallocate();
        this->MaxNoLoadHeatingByVent.deallocate();
        this->MaxNoLoadCoolingByVent.deallocate();
        this->SysTotZoneLoadHTNG.deallocate();
        this->SysTotZoneLoadCLNG.deallocate();
        this->SysOALoadHTNG.deallocate();
        this->SysOALoadCLNG.deallocate();
        this->SysTotHTNG.deallocate();
        this->SysTotCLNG.deallocate();
        this->SysTotH2OHOT.deallocate();
        this->SysTotH2OCOLD.deallocate();
        this->SysTotElec.deallocate();
        this->SysTotNaturalGas.deallocate();
        this->SysTotPropane.deallocate();
        this->SysTotSteam.deallocate();
        this->SysHumidHTNG.deallocate();
        this->SysHumidElec.deallocate();
        this->SysHumidNaturalGas.deallocate();
        this->SysHumidPropane.deallocate();
        this->SysEvapCLNG.deallocate();
        this->SysEvapElec.deallocate();
        this->SysHeatExHTNG.deallocate();
        this->SysHeatExCLNG.deallocate();
        this->DesDehumidCLNG.deallocate();
        this->DesDehumidElec.deallocate();
        this->SysSolarCollectHeating.deallocate();
        this->SysSolarCollectCooling.deallocate();
        this->SysUserDefinedTerminalHeating.deallocate();
        this->SysUserDefinedTerminalCooling.deallocate();
        this->SysFANCompHTNG.deallocate();
        this->SysFANCompElec.deallocate();
        this->SysCCCompCLNG.deallocate();
        this->SysCCCompH2OCOLD.deallocate();
        this->SysCCCompElec.deallocate();
        this->SysHCCompH2OHOT.deallocate();
        this->SysHCCompElec.deallocate();
        this->SysHCCompElecRes.deallocate();
        this->SysHCCompHTNG.deallocate();
        this->SysHCCompNaturalGas.deallocate();
        this->SysHCCompPropane.deallocate();
        this->SysHCCompSteam.deallocate();
        this->SysDomesticH2O.deallocate();
        this->ZoneOAMassFlow.deallocate();
        this->ZoneOAMass.deallocate();
        this->ZoneOAVolFlowStdRho.deallocate();
        this->ZoneOAVolStdRho.deallocate();
        this->ZoneOAVolFlowCrntRho.deallocate();
        this->ZoneOAVolCrntRho.deallocate();
        this->ZoneMechACH.deallocate();
        this->ZoneTargetVentilationFlowVoz.deallocate();
        this->ZoneTimeBelowVozDyn.deallocate();
        this->ZoneTimeAtVozDyn.deallocate();
        this->ZoneTimeAboveVozDyn.deallocate();
        this->ZoneTimeVentUnocc.deallocate();
        this->AnyZoneTimeBelowVozDyn = 0.0;
        this->AllZonesTimeAtVozDyn = 0.0;
        this->AnyZoneTimeAboveVozDyn = 0.0;
        this->AnyZoneTimeVentUnocc = 0.0;
        this->AnyZoneTimeBelowVozDynOcc = 0.0;
        this->AllZonesTimeAtVozDynOcc = 0.0;
        this->AnyZoneTimeAboveVozDynOcc = 0.0;
        this->SysMechVentFlow = 0.0;
        this->SysNatVentFlow = 0.0;
        this->SysTargetVentilationFlowVoz.deallocate();
        this->SysTimeBelowVozDyn.deallocate();
        this->SysTimeAtVozDyn.deallocate();
        this->SysTimeAboveVozDyn.deallocate();
        this->SysTimeVentUnocc.deallocate();
        this->SysAnyZoneOccupied.deallocate();
        this->AirLoopLoadsReportEnabled = true;
        this->VentLoadsReportEnabled = true;
        this->VentEnergyReportEnabled = false;
        this->VentReportStructureCreated = false;
        this->TotalLoopConnects = 0;
        this->MaxLoopArraySize = 100;
        this->MaxCompArraySize = 500;
        this->SetBackCounter.deallocate();
        this->HeatCoolFlag.deallocate();
        this->FirstHeatCoolFlag.deallocate();
        this->FirstHeatCoolHour.deallocate();
        this->LastHeatCoolFlag.deallocate();
        this->LastHeatCoolHour.deallocate();
        this->NoLoadFlag.deallocate();
        this->UnmetLoadFlag.deallocate();
        this->Vent.deallocate();
        this->SysPreDefRep.deallocate();
        this->OneTimeFlag_FindFirstLastPtr = true;
        this->OneTimeFlag_InitEnergyReports = true;
        this->OneTimeFlag_UpdateZoneCompPtrArray = true;
        this->ArrayLimit_UpdateZoneCompPtrArray = 100;
        this->ArrayCounter_UpdateZoneCompPtrArray = 1;
        this->OneTimeFlag_UpdateZoneSubCompPtrArray = true;
        this->ArrayLimit_UpdateZoneSubCompPtrArray = 100;
        this->ArrayCounter_UpdateZoneSubCompPtrArray = 1;
        this->OneTimeFlag_UpdateZoneSubSubCompPtrArray = true;
        this->ArrayLimit_UpdateZoneSubSubCompPtrArray = 100;
        this->ArrayCounter_UpdateZoneSubSubCompPtrArray = 1;
        this->OneTimeFlag_UpdateAirSysCompPtrArray = true;
        this->ArrayLimit_UpdateAirSysCompPtrArray = 100;
        this->ArrayCounter_UpdateAirSysCompPtrArray = 1;
        this->OneTimeFlag_UpdateAirSysSubCompPtrArray = true;
        this->ArrayLimit_UpdateAirSysSubCompPtrArray = 100;
        this->ArrayCounter_UpdateAirSysSubCompPtrArray = 1;
        this->OneTimeFlag_UpdateAirSysSubSubCompPtrArray = true;
        this->ArrayLimit_UpdateAirSysSubSubCompPtrArray = 100;
        this->ArrayCounter_UpdateAirSysSubSubCompPtrArray = 1;
        this->NumCompTypes = 0;
        this->CompTypeErrors = Array1D<SystemReports::CompTypeError>(100);
    }
};

} // namespace EnergyPlus

#endif
