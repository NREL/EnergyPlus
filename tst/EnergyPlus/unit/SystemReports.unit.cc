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

// EnergyPlus::SystemReports Unit Tests

// Google Test Headers
#include <gtest/gtest.h>

// EnergyPlus Headers

#include <EnergyPlus/IOFiles.hh>
#include <EnergyPlus/SystemReports.hh>
#include <EnergyPlus/DataAirSystems.hh>
#include "Fixtures/EnergyPlusFixture.hh"
#include <EnergyPlus/OutAirNodeManager.hh>
#include <EnergyPlus/DataGlobalConstants.hh>
#include <EnergyPlus/DataLoopNode.hh>
#include <EnergyPlus/DataZoneEquipment.hh>

using namespace EnergyPlus::SystemReports;
using namespace EnergyPlus::DataGlobalConstants;
using namespace EnergyPlus::DataAirSystems;
using namespace EnergyPlus::DataGlobalConstants;

namespace EnergyPlus {

TEST_F(EnergyPlusFixture, SeparateGasOutputVariables)
{
    DataHVACGlobals::NumPrimaryAirSys = 1;
    PrimaryAirSystem.allocate(1);
    DataLoopNode::Node.allocate(2);
        
    bool CompLoadFlag(false);
    int AirLoopNum(1);
    std::string CompType1;
    std::string CompType2;
    Real64 CompLoad(150.0);
    Real64 CompEnergyUse(100.0);
    
    PrimaryAirSystem(1).NumBranches = 1;
    PrimaryAirSystem(1).Branch.allocate(1);
    PrimaryAirSystem(1).Branch(1).TotalComponents = 2;
    PrimaryAirSystem(1).Branch(1).NodeNumOut = 1;

    PrimaryAirSystem(1).Branch(1).Comp.allocate(2);
    PrimaryAirSystem(1).Branch(1).Comp(1).Name = "Main Gas Humidifier";
    PrimaryAirSystem(1).Branch(1).Comp(1).TypeOf = "HUMIDIFIER:STEAM:GAS";
    PrimaryAirSystem(1).Branch(1).Comp(1).NodeNumIn = 1;
    PrimaryAirSystem(1).Branch(1).Comp(1).NodeNumOut = 1;
    PrimaryAirSystem(1).Branch(1).Comp(1).NumMeteredVars = 1;
    PrimaryAirSystem(1).Branch(1).Comp(1).MeteredVar.allocate(1);
    PrimaryAirSystem(1).Branch(1).Comp(1).MeteredVar(1).EndUse_CompMode = 1;
    PrimaryAirSystem(1).Branch(1).Comp(1).MeteredVar(1).CurMeterReading = 100.0;
    PrimaryAirSystem(1).Branch(1).Comp(1).MeteredVar(1).ResourceType = AssignResourceTypeNum("NaturalGas");

    PrimaryAirSystem(1).Branch(1).Comp(2).Name = "Main Gas Heating Coil";
    PrimaryAirSystem(1).Branch(1).Comp(2).TypeOf = "COIL:HEATING:DESUPERHEATER";
    PrimaryAirSystem(1).Branch(1).Comp(2).NodeNumIn = 2;
    PrimaryAirSystem(1).Branch(1).Comp(2).NodeNumOut = 2;
    PrimaryAirSystem(1).Branch(1).Comp(2).NumMeteredVars = 1;
    PrimaryAirSystem(1).Branch(1).Comp(2).MeteredVar.allocate(1);
    PrimaryAirSystem(1).Branch(1).Comp(2).MeteredVar(1).EndUse_CompMode = 1;
    PrimaryAirSystem(1).Branch(1).Comp(2).MeteredVar(1).CurMeterReading = 100.0;
    PrimaryAirSystem(1).Branch(1).Comp(2).MeteredVar(1).ResourceType = AssignResourceTypeNum("NaturalGas");
    
    DataLoopNode::Node(1).MassFlowRate = 1.0;
    DataLoopNode::Node(2).MassFlowRate = 1.0;

    SysHumidNaturalGas.allocate(1);
    SysHCCompNaturalGas.allocate(1);
    SysTotNaturalGas.allocate(1);
    SysTotPropane.allocate(1);
    SysHCCompPropane.allocate(1);
    SysHumidPropane.allocate(1);

    SysHumidNaturalGas(1) = 0;
    SysHCCompNaturalGas(1) = 0;
    SysTotNaturalGas(1) = 0;

    //Calculate SysHumidNaturalGas ("Air System Humidifier NaturalGas Energy" Output Variable)
    CalcSystemEnergyUse(
        CompLoadFlag, 
        AirLoopNum, 
        PrimaryAirSystem(1).Branch(1).Comp(1).TypeOf, 
        PrimaryAirSystem(1).Branch(1).Comp(1).MeteredVar(1).ResourceType,
        CompLoad,
        CompEnergyUse);

    // Calculate SysHCCompNaturalGas ("Air System Heating Coil NaturalGas Energy" Output Variable)
    CalcSystemEnergyUse(
        CompLoadFlag, 
        AirLoopNum, 
        PrimaryAirSystem(1).Branch(1).Comp(2).TypeOf, 
        PrimaryAirSystem(1).Branch(1).Comp(2).MeteredVar(1).ResourceType,
        CompLoad,
        CompEnergyUse);

    EXPECT_EQ(SysHumidNaturalGas(1), 100);
    EXPECT_EQ(SysHCCompNaturalGas(1), 100);

    // Allocate variables to run ReportSystemEnergyUse() function for SysTotNaturalGas ("Air System NaturalGas Energy")
    SysTotHTNG.allocate(1);
    SysFANCompHTNG.allocate(1);
    SysHCCompHTNG.allocate(1);
    SysHeatExHTNG.allocate(1);
    SysHumidHTNG.allocate(1);
    SysSolarCollectHeating.allocate(1);
    SysUserDefinedTerminalHeating.allocate(1);
    SysTotCLNG.allocate(1);
    SysCCCompCLNG.allocate(1);
    SysHeatExCLNG.allocate(1);
    SysEvapCLNG.allocate(1);
    DesDehumidCLNG.allocate(1);
    SysSolarCollectCooling.allocate(1);
    SysUserDefinedTerminalCooling.allocate(1);
    SysTotElec.allocate(1);
    SysFANCompElec.allocate(1);
    SysHCCompElec.allocate(1);
    SysCCCompElec.allocate(1);
    SysHCCompElecRes.allocate(1);
    SysHumidElec.allocate(1);
    DesDehumidElec.allocate(1);
    SysEvapElec.allocate(1);
    SysTotSteam.allocate(1);
    SysHCCompSteam.allocate(1);
    SysTotH2OCOLD.allocate(1);
    SysCCCompH2OCOLD.allocate(1);
    SysTotH2OHOT.allocate(1);
    SysHCCompH2OHOT.allocate(1);

    // Calculate SysTotNaturalGas ("Air System NaturalGas Energy")
    ReportSystemEnergyUse();
    EXPECT_EQ(SysTotNaturalGas(1), 200);

    // Initialization for propane cases
    SysHumidNaturalGas(1) = 0;
    SysHCCompNaturalGas(1) = 0;
    SysTotNaturalGas(1) = 0;

    PrimaryAirSystem(1).Branch(1).Comp(1).MeteredVar(1).ResourceType = AssignResourceTypeNum("Propane");
    PrimaryAirSystem(1).Branch(1).Comp(2).MeteredVar(1).ResourceType = AssignResourceTypeNum("Propane");

    // Calculate SysHumidPropane ("Air System Humidifier Propane Energy" Output Variable)
    CalcSystemEnergyUse(CompLoadFlag,
                        AirLoopNum,
                        PrimaryAirSystem(1).Branch(1).Comp(1).TypeOf,
                        PrimaryAirSystem(1).Branch(1).Comp(1).MeteredVar(1).ResourceType,
                        CompLoad,
                        CompEnergyUse);

    // Calculate SysHCCompPropane ("Air System Heating Coil Propane Energy" Output Variable)
    CalcSystemEnergyUse(CompLoadFlag,
                        AirLoopNum,
                        PrimaryAirSystem(1).Branch(1).Comp(2).TypeOf,
                        PrimaryAirSystem(1).Branch(1).Comp(2).MeteredVar(1).ResourceType,
                        CompLoad,
                        CompEnergyUse);
   
    EXPECT_EQ(SysHumidPropane(1), 100);
    EXPECT_EQ(SysHCCompPropane(1), 100);

    // Calculate SysTotPropane ("Air System Propane Energy")
    ReportSystemEnergyUse();
    EXPECT_EQ(SysTotPropane(1), 200);
}
} // namespace EnergyPlus
