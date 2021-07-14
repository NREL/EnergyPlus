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

#ifndef Boilers_hh_INCLUDED
#define Boilers_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>

// EnergyPlus Headers
#include <EnergyPlus/Data/BaseData.hh>
#include <EnergyPlus/DataBranchAirLoopPlant.hh>
#include <EnergyPlus/DataGlobalConstants.hh>
#include <EnergyPlus/DataGlobals.hh>
#include <EnergyPlus/EnergyPlus.hh>
#include <EnergyPlus/Plant/DataPlant.hh>
#include <EnergyPlus/PlantComponent.hh>

namespace EnergyPlus {

// Forward declarations
struct EnergyPlusData;

namespace Boilers {

    // water temperature evaluation method
    enum class TempMode
    {
        NOTSET,
        ENTERINGBOILERTEMP,
        LEAVINGBOILERTEMP
    };

    struct BoilerSpecs : PlantComponent
    {
        // Members
        std::string Name;                           // user identifier
        DataGlobalConstants::ResourceType FuelType; // resource type assignment
        int TypeNum;                                // plant loop type identifier
        int LoopNum;                                // plant loop connection
        int LoopSideNum;                            // plant loop side connection
        int BranchNum;                              // plant loop branch connection
        int CompNum;                                // plant loop component connection
        bool Available;                             // TRUE if machine available in current time step
        bool ON;                                    // TRUE: simulate the machine at it's operating part load ratio
        Real64 NomCap;                              // W - design nominal capacity of Boiler
        bool NomCapWasAutoSized;                    // true if previous was set to autosize input
        Real64 NomEffic;                            // boiler efficiency at design conditions
        Real64 TempDesBoilerOut;                    // C - Boiler design outlet temperature
        DataPlant::FlowMode FlowMode;               // one of 3 modes for component flow during operation
        bool ModulatedFlowSetToLoop;                // True if the setpoint is missing at the outlet node
        bool ModulatedFlowErrDone;                  // true if setpoint warning issued
        Real64 VolFlowRate;                         // m3/s - Boiler water design volumetric flow rate
        bool VolFlowRateWasAutoSized;               // true if previous was set to autosize input
        Real64 DesMassFlowRate;                     // kg/s - Boiler water design mass flow rate
        Real64 MassFlowRate;                        // kg/s - Boiler water mass flow rate
        Real64 SizFac;                              // sizing factor
        int BoilerInletNodeNum;                     // Node number at the boiler inlet
        int BoilerOutletNodeNum;                    // Node number at the boiler outlet
        Real64 MinPartLoadRat;                      // Minimum allowed operating part load ratio
        Real64 MaxPartLoadRat;                      // Maximum allowed operating part load ratio
        Real64 OptPartLoadRat;                      // Optimal operating part load ratio
        Real64 OperPartLoadRat;                     // Actual operating part load ratio
        TempMode CurveTempMode;                     // water temp to use in curve, switch between entering and leaving
        int EfficiencyCurvePtr;                     // Index to efficiency curve
        Real64 TempUpLimitBoilerOut;                // C - Boiler outlet maximum temperature limit
        Real64 ParasiticElecLoad;                   // W - Parasitic electric power (e.g. forced draft fan)
        int EffCurveOutputError;                    // efficiency curve output <=0 recurring warning error counter
        int EffCurveOutputIndex;                    // efficiency curve output <=0 recurring warning error message index
        int CalculatedEffError;                     // calculated efficiency >1.1 recurring warning error counter
        int CalculatedEffIndex;                     // calculated efficiency >1.1 recurring warning error message index
        bool IsThisSized;                           // TRUE if sizing is done
        // Operational fault parameters
        bool FaultyBoilerFoulingFlag;     // True if the boiler has fouling fault
        int FaultyBoilerFoulingIndex;     // Index of the fault object corresponding to the boiler
        Real64 FaultyBoilerFoulingFactor; // Boiler fouling factor
        std::string EndUseSubcategory;    // identifier use for the end use subcategory
        bool MyEnvrnFlag;
        bool MyFlag;

        Real64 FuelUsed;           // W - Boiler fuel used
        Real64 ParasiticElecPower; // W - Parasitic electrical power (e.g. forced draft fan)
        Real64 BoilerLoad;         // W - Boiler Load
        Real64 BoilerMassFlowRate; // kg/s - Boiler mass flow rate
        Real64 BoilerOutletTemp;   // W - Boiler outlet temperature
        Real64 BoilerPLR;          // Boiler operating part-load ratio
        Real64 BoilerEff;          // Boiler operating efficiency

        Real64 BoilerEnergy;             // J - Boiler energy integrated over time
        Real64 FuelConsumed;             // J - Boiler Fuel consumed integrated over time
        Real64 BoilerInletTemp;          // C - Boiler inlet temperature
        Real64 ParasiticElecConsumption; // J - Parasitic Electrical Consumption (e.g. forced draft fan)

        std::string BoilerFuelTypeForOutputVariable;

        // Default Constructor
        BoilerSpecs()
            : FuelType(DataGlobalConstants::ResourceType::None), TypeNum(0), LoopNum(0), LoopSideNum(0), BranchNum(0), CompNum(0), Available(false),
              ON(false), NomCap(0.0), NomCapWasAutoSized(false), NomEffic(0.0), TempDesBoilerOut(0.0), FlowMode(DataPlant::FlowMode::Unassigned),
              ModulatedFlowSetToLoop(false), ModulatedFlowErrDone(false), VolFlowRate(0.0), VolFlowRateWasAutoSized(false), DesMassFlowRate(0.0),
              MassFlowRate(0.0), SizFac(0.0), BoilerInletNodeNum(0), BoilerOutletNodeNum(0), MinPartLoadRat(0.0), MaxPartLoadRat(0.0),
              OptPartLoadRat(0.0), OperPartLoadRat(0.0), CurveTempMode(TempMode::NOTSET), EfficiencyCurvePtr(0), TempUpLimitBoilerOut(0.0),
              ParasiticElecLoad(0.0), EffCurveOutputError(0), EffCurveOutputIndex(0), CalculatedEffError(0), CalculatedEffIndex(0),
              IsThisSized(false), FaultyBoilerFoulingFlag(false), FaultyBoilerFoulingIndex(0), FaultyBoilerFoulingFactor(1.0), MyEnvrnFlag(true),
              MyFlag(true), FuelUsed(0.0), ParasiticElecPower(0.0), BoilerLoad(0.0), BoilerMassFlowRate(0.0), BoilerOutletTemp(0.0), BoilerPLR(0.0),
              BoilerEff(0.0), BoilerEnergy(0.0), FuelConsumed(0.0), BoilerInletTemp(0.0), ParasiticElecConsumption(0.0),
              BoilerFuelTypeForOutputVariable("")
        {
        }

        void simulate([[maybe_unused]] EnergyPlusData &state,
                      const PlantLocation &calledFromLocation,
                      bool FirstHVACIteration,
                      Real64 &CurLoad,
                      bool RunFlag) override;

        void getDesignCapacities(EnergyPlusData &state,
                                 [[maybe_unused]] const PlantLocation &calledFromLocation,
                                 Real64 &MaxLoad,
                                 Real64 &MinLoad,
                                 Real64 &OptLoad) override;

        void getSizingFactor(Real64 &SizFac) override;

        void onInitLoopEquip([[maybe_unused]] EnergyPlusData &state, [[maybe_unused]] const PlantLocation &calledFromLocation) override;

        void SetupOutputVars(EnergyPlusData &state);

        void oneTimeInit(EnergyPlusData &state) override;

        void initEachEnvironment(EnergyPlusData &state);

        void InitBoiler(EnergyPlusData &state); // number of the current boiler being simulated

        void SizeBoiler(EnergyPlusData &state);

        void CalcBoilerModel(EnergyPlusData &state,
                             Real64 MyLoad,                                        // W - hot water demand to be met by boiler
                             bool RunFlag,                                         // TRUE if boiler operating
                             DataBranchAirLoopPlant::ControlTypeEnum EquipFlowCtrl // Flow control mode for the equipment
        );

        void UpdateBoilerRecords(EnergyPlusData &state,
                                 Real64 MyLoad, // boiler operating load
                                 bool RunFlag   // boiler on when TRUE
        );

        static PlantComponent *factory(EnergyPlusData &state, std::string const &objectName);
    };

    void GetBoilerInput(EnergyPlusData &state);

} // namespace Boilers

struct BoilersData : BaseGlobalStruct
{
    int numBoilers = 0;
    bool getBoilerInputFlag = true;
    Array1D<Boilers::BoilerSpecs> Boiler;

    void clear_state() override
    {
        this->numBoilers = 0;
        this->getBoilerInputFlag = true;
        this->Boiler.deallocate();
    }
};

} // namespace EnergyPlus

#endif
