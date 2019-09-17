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

#ifndef SystemReports_hh_INCLUDED
#define SystemReports_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>

namespace EnergyPlus {

namespace SystemReports {

    // Data
    // MODULE PARAMETER DEFINITIONS:
    extern int const NoHeatNoCool;
    extern int const CoolingOnly;
    extern int const HeatingOnly;
    extern int const HeatAndCool;
    extern int const MaxSetBackCount;

    // DERIVED TYPE DEFINITIONS:

    // MODULE VARIABLE DECLARATIONS:
    // Ventilation Report Variables
    extern EPVector<Real64> MaxCoolingLoadMetByVent;
    extern EPVector<Real64> MaxCoolingLoadAddedByVent;
    extern EPVector<Real64> MaxOvercoolingByVent;
    extern EPVector<Real64> MaxHeatingLoadMetByVent;
    extern EPVector<Real64> MaxHeatingLoadAddedByVent;
    extern EPVector<Real64> MaxOverheatingByVent;
    extern EPVector<Real64> MaxNoLoadHeatingByVent;
    extern EPVector<Real64> MaxNoLoadCoolingByVent;

    extern EPVector<Real64> RemMaxCoolingLoadMetByVent;
    extern EPVector<Real64> RemMaxCoolingLoadAddedByVent;
    extern EPVector<Real64> RemMaxOvercoolingByVent;
    extern EPVector<Real64> RemMaxHeatingLoadMetByVent;
    extern EPVector<Real64> RemMaxHeatingLoadAddedByVent;
    extern EPVector<Real64> RemMaxOverheatingByVent;
    extern EPVector<Real64> RemMaxNoLoadHeatingByVent;
    extern EPVector<Real64> RemMaxNoLoadCoolingByVent;

    extern EPVector<Real64> LastMaxCoolingLoadMetByVent;
    extern EPVector<Real64> LastMaxCoolingLoadAddedByVent;
    extern EPVector<Real64> LastMaxOvercoolingByVent;
    extern EPVector<Real64> LastMaxHeatingLoadMetByVent;
    extern EPVector<Real64> LastMaxHeatingLoadAddedByVent;
    extern EPVector<Real64> LastMaxOverheatingByVent;
    extern EPVector<Real64> LastMaxNoLoadHeatingByVent;
    extern EPVector<Real64> LastMaxNoLoadCoolingByVent;

    extern EPVector<Real64> SysTotZoneLoadHTNG;
    extern EPVector<Real64> SysTotZoneLoadCLNG;
    extern EPVector<Real64> SysOALoadHTNG;
    extern EPVector<Real64> SysOALoadCLNG;
    extern EPVector<Real64> SysTotHTNG;
    extern EPVector<Real64> SysTotCLNG;

    extern EPVector<Real64> SysTotH2OHOT;
    extern EPVector<Real64> SysTotH2OCOLD;
    extern EPVector<Real64> SysTotElec;
    extern EPVector<Real64> SysTotGas;
    extern EPVector<Real64> SysTotSteam;

    extern EPVector<Real64> SysHumidHTNG;
    extern EPVector<Real64> SysHumidElec;
    extern EPVector<Real64> SysHumidGas;
    extern EPVector<Real64> SysEvapCLNG;
    extern EPVector<Real64> SysEvapElec;
    extern EPVector<Real64> SysHeatExHTNG;
    extern EPVector<Real64> SysHeatExCLNG;
    extern EPVector<Real64> DesDehumidCLNG;
    extern EPVector<Real64> DesDehumidElec;
    extern EPVector<Real64> SysSolarCollectHeating;
    extern EPVector<Real64> SysSolarCollectCooling;
    extern EPVector<Real64> SysUserDefinedTerminalHeating;
    extern EPVector<Real64> SysUserDefinedTerminalCooling;

    extern EPVector<Real64> SysFANCompHTNG;
    extern EPVector<Real64> SysFANCompElec;
    extern EPVector<Real64> SysCCCompCLNG;
    extern EPVector<Real64> SysCCCompH2OCOLD;
    extern EPVector<Real64> SysCCCompElec;
    extern EPVector<Real64> SysHCCompH2OHOT;
    extern EPVector<Real64> SysHCCompElec;
    extern EPVector<Real64> SysHCCompElecRes;
    extern EPVector<Real64> SysHCCompHTNG;
    extern EPVector<Real64> SysHCCompGas;
    extern EPVector<Real64> SysHCCompSteam;
    extern EPVector<Real64> SysDomesticH20;

    extern EPVector<Real64> ZoneOAMassFlow;       // zone mech vent mass flow rate {kg/s}
    extern EPVector<Real64> ZoneOAMass;           // zone mech vent total mass for time {kg}
    extern EPVector<Real64> ZoneOAVolFlowStdRho;  // zone mech vent volume flow rate at standard density {m3/s}
    extern EPVector<Real64> ZoneOAVolStdRho;      // zone mech vent total volume OA at standard density {m3/s}
    extern EPVector<Real64> ZoneOAVolFlowCrntRho; // zone mech vent volume flow rate at current density {m3/s}
    extern EPVector<Real64> ZoneOAVolCrntRho;     // zone mech vent total volume OA at current density {m3/s}
    extern EPVector<Real64> ZoneMechACH;          // zone mech vent air changes per hour {ACH}

    extern bool AirLoopLoadsReportEnabled;
    extern bool VentLoadsReportEnabled;
    extern bool VentEnergyReportEnabled;
    extern bool VentReportStructureCreated;
    extern int TotalLoopConnects; // Total number of loop connections
    extern int MaxLoopArraySize;
    extern int MaxCompArraySize;
    extern int DBFlag;

    extern Array1D_int SetBackCounter;
    extern Array1D_int HeatCoolFlag;
    extern Array1D_int FirstHeatCoolFlag;
    extern Array1D_int FirstHeatCoolHour;
    extern Array1D_int LastHeatCoolFlag;
    extern Array1D_int LastHeatCoolHour;
    extern Array1D_bool AirLoopCalcDone;
    extern Array1D_bool NoLoadFlag;
    extern Array1D_bool UnmetLoadFlag;

    // SUBROUTINE SPECIFICATIONS FOR MODULE SystemReports

    // Reporting Initialization

    // Reporting routines for module

    // Types

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
        CoilType()
        {
        }
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
        SummarizeLoads()
        {
        }
    };

    // Object Data
    extern EPVector<SummarizeLoads> Vent;

    // Functions

    void InitEnergyReports();

    void FindFirstLastPtr(int &LoopType, int &LoopNum, int &ArrayCount, int &LoopCount, bool &ConnectionFlag);

    void UpdateZoneCompPtrArray(int &Idx,
                                int const ListNum,
                                int const AirDistUnitNum,
                                int const PlantLoopType,
                                int const PlantLoop,
                                int const PlantBranch,
                                int const PlantComp);

    void UpdateZoneSubCompPtrArray(int &Idx,
                                   int const ListNum,
                                   int const AirDistUnitNum,
                                   int const SubCompNum,
                                   int const PlantLoopType,
                                   int const PlantLoop,
                                   int const PlantBranch,
                                   int const PlantComp);

    void UpdateZoneSubSubCompPtrArray(int &Idx,
                                      int const ListNum,
                                      int const AirDistUnitNum,
                                      int const SubCompNum,
                                      int const SubSubCompNum,
                                      int const PlantLoopType,
                                      int const PlantLoop,
                                      int const PlantBranch,
                                      int const PlantComp);

    void UpdateAirSysCompPtrArray(int &Idx,
                                  int const AirLoopNum,
                                  int const BranchNum,
                                  int const CompNum,
                                  int const PlantLoopType,
                                  int const PlantLoop,
                                  int const PlantBranch,
                                  int const PlantComp);

    void UpdateAirSysSubCompPtrArray(int &Idx,
                                     int const AirLoopNum,
                                     int const BranchNum,
                                     int const CompNum,
                                     int const SubCompNum,
                                     int const PlantLoopType,
                                     int const PlantLoop,
                                     int const PlantBranch,
                                     int const PlantComp);

    void UpdateAirSysSubSubCompPtrArray(int &Idx,
                                        int const AirLoopNum,
                                        int const BranchNum,
                                        int const CompNum,
                                        int const SubCompNum,
                                        int const SubSubCompNum,
                                        int const PlantLoopType,
                                        int const PlantLoop,
                                        int const PlantBranch,
                                        int const PlantComp);

    void AllocateAndSetUpVentReports();

    void CreateEnergyReportStructure();

    // End Initialization Section of the Module
    //******************************************************************************

    // Beginning of Reporting subroutines for the SimAir Module
    // *****************************************************************************

    void ReportSystemEnergyUse();

    void CalcSystemEnergyUse(bool const CompLoadFlag,
                             int const AirLoopNum,
                             std::string const &CompType,
                             int const EnergyType,
                             Real64 const CompLoad,
                             Real64 const CompEnergy);

    void ReportMaxVentilationLoads();

    void MatchPlantSys(int const AirLoopNum, // counter for zone air distribution inlets
                       int const BranchNum   // counter for zone air distribution inlets
    );

    void FindDemandSideMatch(std::string const &CompType, // Inlet node of the component to find the match of
                             std::string const &CompName, // Outlet node of the component to find the match of
                             bool &MatchFound,            // Set to .TRUE. when a match is found
                             int &MatchLoopType,          // Loop number of the match
                             int &MatchLoop,              // Loop number of the match
                             int &MatchBranch,            // Branch number of the match
                             int &MatchComp               // Component number of the match
    );

    void ReportAirLoopConnections();

    //        End of Reporting subroutines for the SimAir Module
    // *****************************************************************************

} // namespace SystemReports

} // namespace EnergyPlus

#endif
