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

#ifndef ENERGYPLUS_DATA_COMMONINCLUDES
#define ENERGYPLUS_DATA_COMMONINCLUDES

// OK, now that we have split the EnergyPlusData class into a header and source file, and we are moving member
// variables to pointer type, we no longer need to #include those headers in the Data header file.  But there are now
// other places where we do need those includes, and multiple spots actually.  So I am putting all the required includes
// here in this one file.  Basically, when a header inclusion is removed from EnergyPlusData.hh, put it here.

// these are needed so that we don't have incomplete types when we manage the EnergyPlusData instance
#include <EnergyPlus/AirflowNetworkBalanceManager.hh>
#include <EnergyPlus/AirLoopHVACDOAS.hh>
#include <EnergyPlus/BaseboardElectric.hh>
#include <EnergyPlus/BaseboardRadiator.hh>
#include <EnergyPlus/Boilers.hh>
#include <EnergyPlus/BoilerSteam.hh>
#include <EnergyPlus/BranchInputManager.hh>
#include <EnergyPlus/ChilledCeilingPanelSimple.hh>
#include <EnergyPlus/ChillerAbsorption.hh>
#include <EnergyPlus/ChillerElectricEIR.hh>
#include <EnergyPlus/ChillerExhaustAbsorption.hh>
#include <EnergyPlus/ChillerGasAbsorption.hh>
#include <EnergyPlus/ChillerIndirectAbsorption.hh>
#include <EnergyPlus/ChillerReformulatedEIR.hh>
#include <EnergyPlus/CondenserLoopTowers.hh>
#include <EnergyPlus/Construction.hh>
#include <EnergyPlus/ConvectionCoefficients.hh>
#include <EnergyPlus/CoolTower.hh>
#include <EnergyPlus/CostEstimateManager.hh>
#include <EnergyPlus/CrossVentMgr.hh>
#include <EnergyPlus/CTElectricGenerator.hh>
#include <EnergyPlus/CurveManager.hh>
#include <EnergyPlus/DataAirLoop.hh>
#include <EnergyPlus/DataGlobals.hh>
#include <EnergyPlus/ExteriorEnergyUse.hh>
#include <EnergyPlus/Fans.hh>
#include <EnergyPlus/Pipes.hh>
#include <EnergyPlus/PlantChillers.hh>
#include <EnergyPlus/UnitHeater.hh>
#include <EnergyPlus/UnitVentilator.hh>
#include <EnergyPlus/UserDefinedComponents.hh>
#include <EnergyPlus/VariableSpeedCoils.hh>
#include <EnergyPlus/VentilatedSlab.hh>
#include <EnergyPlus/WaterCoils.hh>
#include <EnergyPlus/WaterManager.hh>
#include <EnergyPlus/WaterThermalTanks.hh>
#include <EnergyPlus/WaterToAirHeatPump.hh>
#include <EnergyPlus/WaterToAirHeatPumpSimple.hh>
#include <EnergyPlus/WaterUse.hh>
#include <EnergyPlus/WeatherManager.hh>
#include <EnergyPlus/WindowAC.hh>
#include <EnergyPlus/WindowComplexManager.hh>
#include <EnergyPlus/WindowEquivalentLayer.hh>
#include <EnergyPlus/WindowManager.hh>
#include <EnergyPlus/WindTurbine.hh>
#include <EnergyPlus/ZoneAirLoopEquipmentManager.hh>
#include <EnergyPlus/ZoneContaminantPredictorCorrector.hh>
#include <EnergyPlus/ZoneDehumidifier.hh>
#include <EnergyPlus/ZoneEquipmentManager.hh>
#include <EnergyPlus/ZonePlenum.hh>
#include <EnergyPlus/ZoneTempPredictorCorrector.hh>

#endif
