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

// EnergyPlus::Pumps Unit Tests

// Google Test Headers
#include <gtest/gtest.h>

#include "Fixtures/EnergyPlusFixture.hh"
#include <EnergyPlus/UnitVentilator.hh>
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataLoopNode.hh>
#include <EnergyPlus/DataHVACGlobals.hh>

namespace EnergyPlus {

TEST_F(EnergyPlusFixture, UnitVentilatorSetOAMassFlowRateForCoolingVariablePercentTest)
{
    Real64 MinOA;
    Real64 MassFlowRate;
    Real64 MaxOA;
    Real64 Tinlet;
    Real64 Toutdoor;
    Real64 OAMassFlowRate;
    Real64 ExpectedOAMassFlowRate;
    Real64 UnitVentNum;
    
    DataLoopNode::clear_state();
    UnitVentilator::clear_state();
    
    DataLoopNode::Node.allocate(4);
    UnitVentilator::UnitVent.allocate(1);
    
    UnitVentilator::UnitVent(1).ATMixerExists = false;
    UnitVentilator::UnitVent(1).ATMixerType = DataHVACGlobals::ATMixer_InletSide;
    UnitVentilator::UnitVent(1).FanOutletNode = 1;
    UnitVentilator::UnitVent(1).OAMixerOutNode = 2;
    UnitVentilator::UnitVent(1).ATMixerOutNode = 3;
    UnitVentilator::UnitVent(1).AirInNode = 4;
    DataLoopNode::Node(1).Enthalpy = 0.0;
    DataLoopNode::Node(2).Enthalpy = 0.0;
    DataLoopNode::Node(3).Enthalpy = 0.0;
    DataLoopNode::Node(4).Enthalpy = 0.0;

    // Test 1: Tinlet <= Toutdoor
    UnitVentNum = 1;
    Tinlet = 20.0;
    Toutdoor = 23.0;
    MinOA = 0.1;
    MaxOA = 0.9;
    MassFlowRate = 1.234;
    UnitVentilator::QZnReq = 2345.6;
    DataEnvironment::OutHumRat = 0.008;
    
    OAMassFlowRate = UnitVentilator::SetOAMassFlowRateForCoolingVariablePercent(UnitVentNum,MinOA,MassFlowRate,MaxOA,Tinlet,Toutdoor);
    ExpectedOAMassFlowRate = 0.1234;
    
    EXPECT_NEAR(ExpectedOAMassFlowRate,OAMassFlowRate, 0.0001);

    // Test 2: Toutdoor < Tinlet, OA limited by OAMin
    Tinlet = 23.0;
    Toutdoor = 1.0;
    MinOA = 0.1;
    MaxOA = 0.9;
    MassFlowRate = 1.234;
    UnitVentilator::QZnReq = 1.5678;
    DataEnvironment::OutHumRat = 0.008;
    
    OAMassFlowRate = UnitVentilator::SetOAMassFlowRateForCoolingVariablePercent(UnitVentNum,MinOA,MassFlowRate,MaxOA,Tinlet,Toutdoor);
    ExpectedOAMassFlowRate = 0.1234;
    
    EXPECT_NEAR(ExpectedOAMassFlowRate,OAMassFlowRate, 0.0001);
    
    // Test 3: Toutdoor < Tinlet, OA limited by OAMax
    Tinlet = 23.0;
    Toutdoor = 20.0;
    MinOA = 0.1;
    MaxOA = 0.9;
    MassFlowRate = 1.234;
    UnitVentilator::QZnReq = 4567.89;
    DataEnvironment::OutHumRat = 0.010;
    
    OAMassFlowRate = UnitVentilator::SetOAMassFlowRateForCoolingVariablePercent(UnitVentNum,MinOA,MassFlowRate,MaxOA,Tinlet,Toutdoor);
    ExpectedOAMassFlowRate = 1.1106;
    
    EXPECT_NEAR(ExpectedOAMassFlowRate,OAMassFlowRate, 0.0001);
    
    // Test 4: Toutdoor < Tinlet, OA between OAMin and OAMax
    Tinlet = 23.0;
    Toutdoor = 8.0;
    MinOA = 0.1;
    MaxOA = 0.9;
    MassFlowRate = 1.234;
    UnitVentilator::QZnReq = 15678.9;
    DataEnvironment::OutHumRat = 0.010;
    
    OAMassFlowRate = UnitVentilator::SetOAMassFlowRateForCoolingVariablePercent(UnitVentNum,MinOA,MassFlowRate,MaxOA,Tinlet,Toutdoor);
    ExpectedOAMassFlowRate = 1.02133;
    
    EXPECT_NEAR(ExpectedOAMassFlowRate,OAMassFlowRate, 0.0001);

    // Test 5: Toutdoor < Tinlet, OA between OAMin and OAMax, with fan heat, no AT mixer
    Tinlet = 23.0;
    Toutdoor = 8.0;
    MinOA = 0.1;
    MaxOA = 0.9;
    MassFlowRate = 1.234;
    UnitVentilator::QZnReq = 15678.9-12.34;
    DataEnvironment::OutHumRat = 0.010;
    DataLoopNode::Node(1).Enthalpy = 11.0;
    DataLoopNode::Node(2).Enthalpy = 1.0;
    DataLoopNode::Node(3).Enthalpy = 0.0;
    DataLoopNode::Node(4).Enthalpy = 0.0;
    
    OAMassFlowRate = UnitVentilator::SetOAMassFlowRateForCoolingVariablePercent(UnitVentNum,MinOA,MassFlowRate,MaxOA,Tinlet,Toutdoor);
    ExpectedOAMassFlowRate = 1.02133;
    
    EXPECT_NEAR(ExpectedOAMassFlowRate,OAMassFlowRate, 0.0001);

    // Test 6: Toutdoor < Tinlet, OA between OAMin and OAMax, with fan heat, with AT mixer inlet side
    Tinlet = 23.0;
    Toutdoor = 8.0;
    MinOA = 0.1;
    MaxOA = 0.9;
    MassFlowRate = 1.234;
    UnitVentilator::QZnReq = 15678.9-12.34;
    DataEnvironment::OutHumRat = 0.010;
    DataLoopNode::Node(1).Enthalpy = 11.0;
    DataLoopNode::Node(2).Enthalpy = 0.0;
    DataLoopNode::Node(3).Enthalpy = 1.0;
    DataLoopNode::Node(4).Enthalpy = 0.0;
    UnitVentilator::UnitVent(1).ATMixerExists = true;
    UnitVentilator::UnitVent(1).ATMixerType = DataHVACGlobals::ATMixer_InletSide;
    
    OAMassFlowRate = UnitVentilator::SetOAMassFlowRateForCoolingVariablePercent(UnitVentNum,MinOA,MassFlowRate,MaxOA,Tinlet,Toutdoor);
    ExpectedOAMassFlowRate = 1.02133;
    
    EXPECT_NEAR(ExpectedOAMassFlowRate,OAMassFlowRate, 0.0001);

    // Test 7: Toutdoor < Tinlet, OA between OAMin and OAMax, with fan heat, with AT mixer inlet side
    Tinlet = 23.0;
    Toutdoor = 8.0;
    MinOA = 0.1;
    MaxOA = 0.9;
    MassFlowRate = 1.234;
    UnitVentilator::QZnReq = 15678.9-12.34;
    DataEnvironment::OutHumRat = 0.010;
    DataLoopNode::Node(1).Enthalpy = 11.0;
    DataLoopNode::Node(2).Enthalpy = 0.0;
    DataLoopNode::Node(3).Enthalpy = 0.0;
    DataLoopNode::Node(4).Enthalpy = 1.0;
    UnitVentilator::UnitVent(1).ATMixerExists = true;
    UnitVentilator::UnitVent(1).ATMixerType = DataHVACGlobals::ATMixer_SupplySide;
    
    OAMassFlowRate = UnitVentilator::SetOAMassFlowRateForCoolingVariablePercent(UnitVentNum,MinOA,MassFlowRate,MaxOA,Tinlet,Toutdoor);
    ExpectedOAMassFlowRate = 1.02133;
    
    EXPECT_NEAR(ExpectedOAMassFlowRate,OAMassFlowRate, 0.0001);

}
    
TEST_F(EnergyPlusFixture, UnitVentilatorCalcMdotCCoilCycFanTest)
{
 
    Real64 QZnReq;
    Real64 QCoilReq;
    int UnitVentNum;
    Real64 PartLoadRatio;
    Real64 mdot;
    Real64 ExpectedResult;

    UnitVentNum = 1;
    UnitVentilator::UnitVent.allocate(UnitVentNum);
    UnitVentilator::UnitVent(UnitVentNum).FanOutletNode = 1;
    UnitVentilator::UnitVent(UnitVentNum).AirInNode = 2;
    DataLoopNode::Node.allocate(2);
    DataLoopNode::Node(2).HumRat = 0.006;
    DataLoopNode::Node(2).Temp = 23.0;
    DataLoopNode::Node(1).Temp = 23.0;
    DataLoopNode::Node(1).MassFlowRate = 1.0;

    // Test 1: QZnReq is greater than zero (heating) so mdot should be zero after the call
    UnitVentilator::UnitVent(1).MaxColdWaterFlow = 0.1234;
    mdot = -0.9999;
    QCoilReq = 5678.9;
    QZnReq = 5678.9;
    PartLoadRatio = 1.0;
    ExpectedResult = 0.0;
    UnitVentilator::CalcMdotCCoilCycFan(mdot,QCoilReq,QZnReq,UnitVentNum, PartLoadRatio);
    
    EXPECT_NEAR(ExpectedResult,mdot,0.0001);
    EXPECT_NEAR(ExpectedResult,QCoilReq,0.0001);
    
    // Test 2: QZnReq is zero (no conditioning) so mdot should be zero after the call
    UnitVentilator::UnitVent(1).MaxColdWaterFlow = 0.1234;
    mdot = -0.9999;
    QCoilReq = 0.0;
    QZnReq = 0.0;
    PartLoadRatio = 1.0;
    ExpectedResult = 0.0;
    UnitVentilator::CalcMdotCCoilCycFan(mdot,QCoilReq,QZnReq,UnitVentNum, PartLoadRatio);

    EXPECT_NEAR(ExpectedResult,mdot,0.0001);
    EXPECT_NEAR(ExpectedResult,QCoilReq,0.0001);

    // Test 3a: QZnReq is less than zero (cooling) so mdot should be non-zero, calculated based on the other variables
    UnitVentilator::UnitVent(1).MaxColdWaterFlow = 0.1234;
    mdot = -0.9999;
    QCoilReq = -5678.9;
    QZnReq = -5678.9;
    PartLoadRatio = 1.0;
    ExpectedResult = 0.1234;
    UnitVentilator::CalcMdotCCoilCycFan(mdot,QCoilReq,QZnReq,UnitVentNum, PartLoadRatio);

    EXPECT_NEAR(ExpectedResult,mdot,0.0001);
    EXPECT_NEAR(QCoilReq,QZnReq,0.1);

    // Test 3b: QZnReq is less than zero (cooling) so mdot should be non-zero, calculated based on the other variables
    UnitVentilator::UnitVent(1).MaxColdWaterFlow = 1.6;
    mdot = -0.9999;
    QCoilReq = -5678.9;
    QZnReq = -5678.9;
    PartLoadRatio = 0.5;
    ExpectedResult = 0.8;
    UnitVentilator::CalcMdotCCoilCycFan(mdot,QCoilReq,QZnReq,UnitVentNum, PartLoadRatio);

    EXPECT_NEAR(ExpectedResult,mdot,0.0001);
    EXPECT_NEAR(QCoilReq,QZnReq,0.1);

}
    
} // namespace EnergyPlus
