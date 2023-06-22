// EnergyPlus, Copyright (c) 1996-2023, The Board of Trustees of the University of Illinois,
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

#ifndef ChillerGasAbsorption_hh_INCLUDED
#define ChillerGasAbsorption_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>

// EnergyPlus Headers
#include <EnergyPlus/Data/BaseData.hh>
#include <EnergyPlus/DataGlobalConstants.hh>
#include <EnergyPlus/DataGlobals.hh>
#include <EnergyPlus/EnergyPlus.hh>
#include <EnergyPlus/Plant/Enums.hh>
#include <EnergyPlus/Plant/PlantLocation.hh>
#include <EnergyPlus/PlantComponent.hh>
#include <EnergyPlus/UtilityRoutines.hh>

namespace EnergyPlus {

// Forward declarations
struct EnergyPlusData;

namespace ChillerGasAbsorption {

    struct GasAbsorberSpecs : PlantComponent
    {
        // Members
        // Parts of Type that do not correspond with IDD definition
        bool Available = false; // need an array of logicals--load identifiers of available equipment
        bool ON = false;        // simulate the machine at it's operating part load ratio
        bool InCoolingMode = false;
        bool InHeatingMode = false;
        // Part of Type that directly corresponds with IDD definition
        std::string Name;                         // user identifier
        Constant::eFuel FuelType;                 // Type of Fuel - DIESEL, GASOLINE, GAS
        Real64 NomCoolingCap = 0.0;               // W - design nominal capacity of Absorber
        bool NomCoolingCapWasAutoSized = false;   // true if nominal capacity was autosize on input
        Real64 NomHeatCoolRatio = 0.0;            // ratio of heating to cooling capacity
        Real64 FuelCoolRatio = 0.0;               // ratio of fuel input to cooling output
        Real64 FuelHeatRatio = 0.0;               // ratio of fuel input to heating output
        Real64 ElecCoolRatio = 0.0;               // ratio of electricity input to cooling output
        Real64 ElecHeatRatio = 0.0;               // ratio of electricity input to heating output
        int ChillReturnNodeNum = 0;               // Node number on the inlet side of the plant
        int ChillSupplyNodeNum = 0;               // Node number on the outlet side of the plant
        bool ChillSetPointErrDone = false;        // flag to report missing setpoint on CW outlet
        bool ChillSetPointSetToLoop = false;      // flag to use overall loop setpoint
        int CondReturnNodeNum = 0;                // Node number on the inlet side of the condenser
        int CondSupplyNodeNum = 0;                // Node number on the outlet side of the condenser
        int HeatReturnNodeNum = 0;                // absorber steam inlet node number, water side
        int HeatSupplyNodeNum = 0;                // absorber steam outlet node number, water side
        bool HeatSetPointErrDone = false;         // flag to report missing setpoint on HW outlet
        bool HeatSetPointSetToLoop = false;       // flag to use overall loop setpoint
        Real64 MinPartLoadRat = 0.0;              // min allowed operating frac full load
        Real64 MaxPartLoadRat = 0.0;              // max allowed operating frac full load
        Real64 OptPartLoadRat = 0.0;              // optimal operating frac full load
        Real64 TempDesCondReturn = 0.0;           // design secondary loop fluid temperature at the Absorber condenser side inlet
        Real64 TempDesCHWSupply = 0.0;            // design chilled water supply temperature
        Real64 EvapVolFlowRate = 0.0;             // m**3/s - design nominal water volumetric flow rate through the evaporator
        bool EvapVolFlowRateWasAutoSized = false; // true if evaporator flow rate was autosize on input
        Real64 CondVolFlowRate = 0.0;             // m**3/s - design nominal water volumetric flow rate through the condenser
        bool CondVolFlowRateWasAutoSized = false; // true if condenser flow rate was autosize on input
        Real64 HeatVolFlowRate = 0.0;             // m**3/s - design nominal water volumetric flow rate through the heater side
        bool HeatVolFlowRateWasAutoSized = false; // true if hot water flow rate was autosize on input
        Real64 SizFac = 0.0;                      // sizing factor
        int CoolCapFTCurve = 0;                   // cooling capacity as a function of temperature curve (chilled water temp,
        // condenser water temp)
        int FuelCoolFTCurve = 0; // Fuel-Input-to cooling output Ratio Function of Temperature Curve (chilled
        // water temp, condenser water temp)
        int FuelCoolFPLRCurve = 0; // Fuel-Input-to cooling output Ratio Function of Part Load Ratio Curve
        int ElecCoolFTCurve = 0;   // Electric-Input-to cooling output Ratio Function of Temperature Curve
        // (chilled water temp, condenser water temp)
        int ElecCoolFPLRCurve = 0;       // Electric-Input-to cooling output Ratio Function of Part Load Ratio Curve
        int HeatCapFCoolCurve = 0;       // Heating Capacity Function of Cooling Capacity Curve
        int FuelHeatFHPLRCurve = 0;      // Fuel Input to heat output ratio during heating only function
        bool isEnterCondensTemp = false; // if using entering conderser water temperature is TRUE, exiting is FALSE
        bool isWaterCooled = false;      // if water cooled it is TRUE
        Real64 CHWLowLimitTemp = 0.0;    // Chilled Water Lower Limit Temperature
        Real64 FuelHeatingValue = 0.0;
        // Calculated design values
        Real64 DesCondMassFlowRate = 0.0; // design nominal mass flow rate of water through the condenser [kg/s]
        Real64 DesHeatMassFlowRate = 0.0; // design nominal mass flow rate of water through the hot water side [kg/s]
        Real64 DesEvapMassFlowRate = 0.0; // design nominal mass flow rate of water through chilled water side [kg/s]
        // other values used during simulation
        int DeltaTempCoolErrCount = 0;        // error count for Delta Temp = 0 while cooling
        int DeltaTempHeatErrCount = 0;        // error count for Delta Temp = 0 while heating
        int CondErrCount = 0;                 // error count for poor Condenser Supply Estimate
        int lCondWaterMassFlowRate_Index = 0; // index for condenser water mass flow rate too low recurring severe warning
        bool PossibleSubcooling = false;      // Flag to determine whether plant is overcooled
        // loop topology variables
        PlantLocation CWplantLoc; // chilled water plant loop component index
        PlantLocation CDplantLoc; // condenser water plant loop component index
        PlantLocation HWplantLoc; // hot water plant loop component index
        bool envrnFlag = true;
        Real64 oldCondSupplyTemp = 0.0; // save the last iteration value of leaving condenser water temperature

        // Originally on report variable structure
        Real64 CoolingLoad = 0.0;             // cooling load on the chiller (previously called QEvap)
        Real64 CoolingEnergy = 0.0;           // variable to track total cooling load for period (was EvapEnergy)
        Real64 HeatingLoad = 0.0;             // heating load on the chiller
        Real64 HeatingEnergy = 0.0;           // heating energy
        Real64 TowerLoad = 0.0;               // load on the cooling tower/condenser (previously called QCond)
        Real64 TowerEnergy = 0.0;             // variable to track total tower load for a period (was CondEnergy)
        Real64 FuelUseRate = 0.0;             // instantaneous use of gas for period
        Real64 FuelEnergy = 0.0;              // variable to track total fuel used for a period
        Real64 CoolFuelUseRate = 0.0;         // instantaneous use of gas for period for cooling
        Real64 CoolFuelEnergy = 0.0;          // variable to track total fuel used for a period for cooling
        Real64 HeatFuelUseRate = 0.0;         // instantaneous use of gas for period for heating
        Real64 HeatFuelEnergy = 0.0;          // variable to track total fuel used for a period for heating
        Real64 ElectricPower = 0.0;           // parasitic electric power used (was PumpingPower)
        Real64 ElectricEnergy = 0.0;          // track the total electricity used for a period (was PumpingEnergy)
        Real64 CoolElectricPower = 0.0;       // parasitic electric power used  for cooling
        Real64 CoolElectricEnergy = 0.0;      // track the total electricity used for a period for cooling
        Real64 HeatElectricPower = 0.0;       // parasitic electric power used  for heating
        Real64 HeatElectricEnergy = 0.0;      // track the total electricity used for a period for heating
        Real64 ChillReturnTemp = 0.0;         // reporting: evaporator inlet temperature (was EvapInletTemp)
        Real64 ChillSupplyTemp = 0.0;         // reporting: evaporator outlet temperature (was EvapOutletTemp)
        Real64 ChillWaterFlowRate = 0.0;      // reporting: evaporator mass flow rate (was Evapmdot)
        Real64 CondReturnTemp = 0.0;          // reporting: condenser inlet temperature (was CondInletTemp)
        Real64 CondSupplyTemp = 0.0;          // reporting: condenser outlet temperature (was CondOutletTemp)
        Real64 CondWaterFlowRate = 0.0;       // reporting: condenser mass flow rate (was Condmdot)
        Real64 HotWaterReturnTemp = 0.0;      // reporting: hot water return (inlet) temperature
        Real64 HotWaterSupplyTemp = 0.0;      // reporting: hot water supply (outlet) temperature
        Real64 HotWaterFlowRate = 0.0;        // reporting: hot water mass flow rate
        Real64 CoolPartLoadRatio = 0.0;       // operating part load ratio (load/capacity for cooling)
        Real64 HeatPartLoadRatio = 0.0;       // operating part load ratio (load/capacity for heating)
        Real64 CoolingCapacity = 0.0;         // current capacity after temperature adjustment
        Real64 HeatingCapacity = 0.0;         // current heating capacity
        Real64 FractionOfPeriodRunning = 0.0; // fraction of the time period that the unit is operating
        Real64 FuelCOP = 0.0;                 // reporting: cooling output/fuel input = CoolingLoad/CoolFuelUseRate

        static GasAbsorberSpecs *factory(EnergyPlusData &state, std::string const &objectName);

        void
        simulate(EnergyPlusData &state, const PlantLocation &calledFromLocation, bool FirstHVACIteration, Real64 &CurLoad, bool RunFlag) override;

        void getDesignCapacities(
            EnergyPlusData &state, const PlantLocation &calledFromLocation, Real64 &MaxLoad, Real64 &MinLoad, Real64 &OptLoad) override;

        void getSizingFactor(Real64 &SizFac) override;

        void onInitLoopEquip(EnergyPlusData &state, const PlantLocation &calledFromLocation) override;

        void getDesignTemperatures(Real64 &TempDesCondIn, Real64 &TempDesEvapOut) override;

        void oneTimeInit(EnergyPlusData &state) override;

        void oneTimeInit_new(EnergyPlusData &state) override;

        void initialize(EnergyPlusData &state);

        void setupOutputVariables(EnergyPlusData &state);

        void size(EnergyPlusData &state);

        void calculateChiller(EnergyPlusData &state, Real64 &MyLoad);

        void calculateHeater(EnergyPlusData &state, Real64 &MyLoad, bool RunFlag);

        void updateCoolRecords(EnergyPlusData &state,
                               Real64 MyLoad, // current load
                               bool RunFlag   // TRUE if Absorber operating
        );

        void updateHeatRecords(EnergyPlusData &state,
                               Real64 MyLoad, // current load
                               bool RunFlag   // TRUE if Absorber operating
        );
    };

    void GetGasAbsorberInput(EnergyPlusData &state);

} // namespace ChillerGasAbsorption

struct ChillerGasAbsorptionData : BaseGlobalStruct
{
    bool getGasAbsorberInputs = true;
    Array1D<ChillerGasAbsorption::GasAbsorberSpecs> GasAbsorber;

    void clear_state() override
    {
        *this = ChillerGasAbsorptionData();
    }
};

} // namespace EnergyPlus

#endif
