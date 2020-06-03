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

#ifndef EnergyPlusData_hh_INCLUDED
#define EnergyPlusData_hh_INCLUDED

// C++ Headers
#include <unordered_map>
#include <string>

// EnergyPlus Headers
#include <EnergyPlus/Data/BaseData.hh>
#include <EnergyPlus/Data/BaseboardData.hh>
#include <EnergyPlus/Boilers.hh>
#include <EnergyPlus/BoilerSteam.hh>
#include <EnergyPlus/ChillerAbsorption.hh>
#include <EnergyPlus/ChillerElectricEIR.hh>
#include <EnergyPlus/ChillerExhaustAbsorption.hh>
#include <EnergyPlus/ChillerGasAbsorption.hh>
#include <EnergyPlus/ChillerIndirectAbsorption.hh>
#include <EnergyPlus/ChillerReformulatedEIR.hh>
#include <EnergyPlus/EnergyPlus.hh>
#include <EnergyPlus/ExteriorEnergyUse.hh>
#include <EnergyPlus/OutputFiles.hh>
#include <EnergyPlus/PlantChillers.hh>
#include <unordered_map>
#include <string>

namespace EnergyPlus {

//struct OutputReportTabular : BaseGlobalStruct
//{
//    //int MaxHeaderLength;
//
//    void clear_state() override {
//    }
//};

    struct BoilersData : BaseGlobalStruct {
        int numBoilers = 0;
        bool getBoilerInputFlag = true;
        Array1D<Boilers::BoilerSpecs> Boiler;

        void clear_state()
        {
            numBoilers = 0;
            getBoilerInputFlag = true;
            Boiler.deallocate();
        }
    };

    struct BoilerSteamData : BaseGlobalStruct {
        int numBoilers = 0;
        bool getSteamBoilerInput = true;
        Array1D<BoilerSteam::BoilerSpecs> Boiler;

        void clear_state()
        {
            numBoilers = 0;
            getSteamBoilerInput = true;
            Boiler.deallocate();
        }
    };

    struct ChillerAbsorberData : BaseGlobalStruct {
        int numAbsorbers = 0;
        bool getInput = true;
        Array1D<ChillerAbsorption::BLASTAbsorberSpecs> absorptionChillers;

        void clear_state()
        {
            numAbsorbers = 0;
            getInput = true;
            absorptionChillers.deallocate();
        }
    };

    struct ChillerElectricEIRData : BaseGlobalStruct {
        int NumElectricEIRChillers = 0;
        bool getInputFlag = true;
        Array1D<ChillerElectricEIR::ElectricEIRChillerSpecs> ElectricEIRChiller;

        void clear_state()
        {
            NumElectricEIRChillers = 0;
            getInputFlag = true;
            ElectricEIRChiller.deallocate();
        }
    };

    struct ChillerExhaustAbsorptionData : BaseGlobalStruct {
        bool Sim_GetInput = true;
        Array1D<ChillerExhaustAbsorption::ExhaustAbsorberSpecs> ExhaustAbsorber;

        void clear_state()
        {
            Sim_GetInput = true;
            ExhaustAbsorber.deallocate();
        }
    };

    struct ChillerReformulatedEIRData : BaseGlobalStruct {
        int NumElecReformEIRChillers = 0;
        bool GetInputREIR = true;
        Array1D<ChillerReformulatedEIR::ReformulatedEIRChillerSpecs> ElecReformEIRChiller;

        void clear_state()
        {
            NumElecReformEIRChillers = 0;
            GetInputREIR = true;
            ElecReformEIRChiller.deallocate();
        }
    };

    struct ChillerGasAbsorptionData : BaseGlobalStruct {
        bool getGasAbsorberInputs = true;
        Array1D<ChillerGasAbsorption::GasAbsorberSpecs> GasAbsorber;

        void clear_state()
        {
            getGasAbsorberInputs = true;
            GasAbsorber.deallocate();
        }
    };

    struct ChillerIndirectAbsoprtionData :BaseGlobalStruct {
        int NumIndirectAbsorbers = 0;
        bool GetInput = true;
        Array1D<ChillerIndirectAbsorption::IndirectAbsorberSpecs> IndirectAbsorber;


        void clear_state()
        {
            NumIndirectAbsorbers = 0;
            GetInput = true;
            IndirectAbsorber.deallocate();
        }
    };

    struct DataGlobal : BaseGlobalStruct {
        // Data
        bool AnnualSimulation = false;

        // MODULE VARIABLE DECLARATIONS:
        std::string DayOfSimChr = "0";       // Counter for days (during the simulation) (character -- for reporting)

        // MODULE PARAMETER DEFINITIONS
        static constexpr int EndZoneSizingCalc = 4;

        void clear_state() override {
            AnnualSimulation = false;
            DayOfSimChr = "0";
        }
    };

    struct ExteriorEnergyUseData : BaseGlobalStruct {

        int NumExteriorLights = 0; // Number of Exterior Light Inputs
        int NumExteriorEqs = 0;    // Number of Exterior Equipment Inputs
        Array1D<ExteriorEnergyUse::ExteriorLightUsage> ExteriorLights;        // Structure for Exterior Light reporting
        Array1D<ExteriorEnergyUse::ExteriorEquipmentUsage> ExteriorEquipment; // Structure for Exterior Equipment Reporting
        std::unordered_map<std::string, std::string> UniqueExteriorEquipNames;
        bool GetExteriorEnergyInputFlag = true; // First time, input is "gotten"
        ExteriorEnergyUseData() : NumExteriorLights(0), NumExteriorEqs(0), GetExteriorEnergyInputFlag(true) {}

        void clear_state() {
            NumExteriorLights = 0;
            NumExteriorEqs = 0;
            ExteriorLights.deallocate();
            ExteriorEquipment.deallocate();
            UniqueExteriorEquipNames.clear();
            GetExteriorEnergyInputFlag = true;
        }
    };

    struct FansData : BaseGlobalStruct {
        // constants
        static constexpr int ExhaustFanCoupledToAvailManagers = 150;
        static constexpr int ExhaustFanDecoupledFromAvailManagers = 151;

        // members
        int NumFans;
        int NumNightVentPerf;      // number of FAN:NIGHT VENT PERFORMANCE objects found in the input
        bool GetFanInputFlag;      // Flag set to make sure you get input once
        bool LocalTurnFansOn;      // If True, overrides fan schedule and cycles ZoneHVAC component fans on
        bool LocalTurnFansOff;     // If True, overrides fan schedule and LocalTurnFansOn and cycles ZoneHVAC component fans off

        FansData() : NumFans(0), NumNightVentPerf(0), GetFanInputFlag(true), LocalTurnFansOn(false),
                     LocalTurnFansOff(false) {}

        void clear_state() override {
            NumFans = 0;
            NumNightVentPerf = 0;
            GetFanInputFlag = true;
            LocalTurnFansOn = false;
            LocalTurnFansOff = false;
        }
    };

    struct PipesData : BaseGlobalStruct {
        int NumLocalPipes;
        bool GetPipeInputFlag;

        PipesData() : NumLocalPipes(0), GetPipeInputFlag(true) {}

        void clear_state() override {
            NumLocalPipes = 0;
            GetPipeInputFlag = true;
        }
    };

    struct PlantChillersData : BaseGlobalStruct {

        int NumElectricChillers = 0;
        int NumEngineDrivenChillers = 0;
        int NumGTChillers = 0;
        int NumConstCOPChillers = 0;

        bool GetEngineDrivenInput = true;
        bool GetElectricInput = true;
        bool GetGasTurbineInput = true;
        bool GetConstCOPInput = true;

        Array1D<PlantChillers::ElectricChillerSpecs> ElectricChiller;
        Array1D<PlantChillers::EngineDrivenChillerSpecs> EngineDrivenChiller;
        Array1D<PlantChillers::GTChillerSpecs> GTChiller;
        Array1D<PlantChillers::ConstCOPChillerSpecs> ConstCOPChiller;

        void clear_state()
        {
            NumElectricChillers = 0;
            NumEngineDrivenChillers = 0;
            NumGTChillers = 0;
            NumConstCOPChillers = 0;
            GetEngineDrivenInput = true;
            GetElectricInput = true;
            GetGasTurbineInput = true;
            GetConstCOPInput = true;
            ElectricChiller.deallocate();
            EngineDrivenChiller.deallocate();
            GTChiller.deallocate();
            ConstCOPChiller.deallocate();
        }
    };

    struct EnergyPlusData : BaseGlobalStruct {
        // module globals
        BaseboardRadiatorData dataBaseboardRadiator;
        BaseboardElectricData dataBaseboardElectric;
        BoilersData dataBoilers;
        BoilerSteamData dataSteamBoilers;
        ChillerAbsorberData dataChillerAbsorbers;
        ChillerElectricEIRData dataChillerElectricEIR;
        ChillerExhaustAbsorptionData dataChillerExhaustAbsorption;
        ChillerIndirectAbsoprtionData dataChillerIndirectAbsorption;
        ChillerGasAbsorptionData dataChillerGasAbsorption;
        ChillerReformulatedEIRData dataChillerReformulatedEIR;
        DataGlobal dataGlobals;
        ExteriorEnergyUseData exteriorEnergyUse;
        FansData fans;
        PipesData pipes;

        PlantChillersData dataPlantChillers;
        //OutputReportTabular outputReportTabular;

        // todo: move this from a reference to an object value
        // after we have eliminated all calls to getSingleton
        // after we've plumbed enough of the functions to allow
        OutputFiles outputFiles;

        EnergyPlusData() {
            OutputFiles::setSingleton(&outputFiles);
        }

        // Cannot safely copy or delete this until we eradicate all remaining
        // calls to OutputFiles::getSingleton and OutputFiles::setSingleton
        EnergyPlusData(const EnergyPlusData &) = delete;
        EnergyPlusData(EnergyPlusData &&) = delete;

        // all clear states
        void clear_state() override {
            dataBaseboardElectric.clear_state();
            dataBaseboardRadiator.clear_state();
            dataBoilers.clear_state();
            dataSteamBoilers.clear_state();
            dataChillerAbsorbers.clear_state();
            dataChillerElectricEIR.clear_state();
            dataChillerExhaustAbsorption.clear_state();
            dataChillerGasAbsorption.clear_state();
            dataChillerIndirectAbsorption.clear_state();
            dataChillerReformulatedEIR.clear_state();
            dataGlobals.clear_state();
            exteriorEnergyUse.clear_state();
            fans.clear_state();
            //outputReportTabular.clear_state();
            pipes.clear_state();
            dataPlantChillers.clear_state();
        };
    };

}
#endif
