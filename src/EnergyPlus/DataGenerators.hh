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

#ifndef DataGenerators_hh_INCLUDED
#define DataGenerators_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>

// EnergyPlus Headers
#include <EnergyPlus/Data/BaseData.hh>
#include <EnergyPlus/DataGlobals.hh>
#include <EnergyPlus/EnergyPlus.hh>

namespace EnergyPlus {

namespace DataGenerators {

    // MODULE PARAMETER DEFINITIONS:
    enum class CurveMode
    {
        Unassigned,
        Normalized, // mode where efficiency curves are modifier curves
        Direct      // mode where efficiency curves are direct
    };

    enum class SkinLoss
    {
        Unassigned,
        ConstantRate,
        UADT,             // UAdelta T mode for skin losses
        QuadraticFuelNdot // Quadratic function of fuel flow for skin losses
    };

    enum class AirSupRateMode
    {
        Unassigned,
        QuadraticFuncofNdot,  // function of fuel rate mode for air flow
        ConstantStoicsAirRat, // Constant air ratio in stoics with fuel constituents
        QuadraticFuncofPel    // function of electric power mode
    };

    enum class RecoverMode
    {
        Unassigned,
        NoRecoveryOnAirIntake,  // mode for controlling intake air heat recovery
        RecoverBurnInvertBatt,  // mode for controlling intake air heat recovery
        RecoverAuxiliaryBurner, // mode for controlling intake air heat recovery
        RecoverInverterBatt,    // mode for controlling intake air heat recovery
        RecoverInverter,        // mode for controlling intake air heat recovery
        RecoverBattery          // mode for controlling intake air heat recovery
    };

    enum class ConstituentMode
    {
        Unassigned,
        RegularAir,
        UserDefinedConstituents
    };

    enum class FuelTemperatureMode
    {
        Unassigned,
        FuelInTempFromNode,
        FuelInTempSchedule
    };

    enum class WaterTemperatureMode
    {
        Unassigned,
        WaterInReformMains,
        WaterInReformAirNode,
        WaterInReformWaterNode,
        WaterInReformSchedule
    };

    enum class InverterEfficiencyMode
    {
        Unassigned,
        Constant,
        Quadratic
    };

    enum class ExhaustGasHX
    {
        Unassigned,
        FixedEffectiveness,   // exhaust gas HX modeling mode
        LMTDempiricalUAeff,   // exhaust gas HX modeling mode
        LMTDfundementalUAeff, // exhaust gas HX modeling mode
        Condensing            // exhaust gas HX modeling mode
    };

    enum class ElectricalStorage
    {
        Unassigned,
        SimpleEffConstraints,        // electrical storage modeling mode
        LeadAcidBatterySaupe,        // electrical storage modeling mode
        LeadAcidBatterManwellMcGowan // electrical storage modeling mode
    };

    enum class LossDestination
    {
        Unassigned,
        SurroundingZone,
        AirInletForFC
    };

    enum class OperatingMode
    {
        Unassigned,
        OpModeOff,     // CHP operating mode OFF
        OpModeStandby, // CHP operating mode Stand By
        OpModeWarmUp,  // CHP operating mode Warm Up or start up
        OpModeNormal,  // CHP operating mode Normal
        OpModeCoolDown // CHP operating mode Cool down or shut down
    };

    enum class FuelMode
    {
        Unassigned,
        fuelModeGaseousConstituents,
        fuelModeGenericLiquid
    };

    Real64 constexpr MinProductGasTemp(-100.0); // Minimum bound on search for product gas temps
    Real64 constexpr MaxProductGasTemp(2000.0); // Maximum bound on search for product gas temps

    enum class ThermodynamicMode
    {
        Unassigned,
        NISTShomate,
        NASAPolynomial
    };

    Real64 constexpr RinKJperMolpK(0.0083145); // R is ideal gas constant (kJ/mol-K)
    Real64 constexpr InitHRTemp(50.0);         // Initialization temperature for heat recovery water
    Real64 constexpr ImBalanceTol(0.00001);    // used as fraction of electrical power at power module

    struct GeneratorFuelSupplyDataStruct
    {
        // Members
        // user input data
        std::string Name;                                 // name of this fuel supply module
        DataGenerators::FuelTemperatureMode FuelTempMode; // temperature of fuel node
        DataGenerators::FuelMode FuelTypeMode;            // type of fuel, gasous or liquid
        std::string NodeName;                             // node name for temperature at input
        int NodeNum;                                      // node number for temperature at input
        int SchedNum;                                     // fuel temperature at input
        int CompPowerCurveID;                             // "pointer" to compressor power cubic curve
        Real64 CompPowerLossFactor;
        int NumConstituents = 0; // number of constituents in fue supply
        Array1D_string ConstitName;
        Array1D<Real64> ConstitMolalFract;
        // calculated data (except some for generic liquid)
        Array1D_int GasLibID;        // lookup ID in Gas Phase ThermoChemistry Structure Array
        Real64 LHV;                  // lower heating value of gaseous fuel (kJ/mol)
        Real64 LHVJperkg;            // lower heating value of gaseous fuel (J/kg)
        Real64 LHVliquid;            // userdefined lhv for generic liquid (J/kg)
        Real64 HHV;                  // higher heating value of fuel (J/kg)
        Real64 MW;                   // molecular weight g/mol
        Real64 eCO2;                 // mass flow based CO2 emmissions factor for complete combustion (-)
        Real64 KmolPerSecToKgPerSec; // conversion from moles to kilograms for this fuel. (
        Real64 StoicOxygenRate;
        Real64 TfuelIntoCompress; // inlet fuel temperature
        Real64 TfuelIntoFCPM;     // compressed fuel temp
        Real64 PfuelCompEl;       // fuel compressor power
        Real64 QskinLoss;         // pumping losses for zone
        Real64 CO2ProductGasCoef; // molar multiplier for stoic products of this fuel
        Real64 H2OProductGasCoef; // molar multiplier for stoic products of this fuel

        // Default Constructor
        GeneratorFuelSupplyDataStruct()
            : FuelTempMode(DataGenerators::FuelTemperatureMode::Unassigned), FuelTypeMode(DataGenerators::FuelMode::Unassigned), NodeNum(0),
              SchedNum(0), CompPowerCurveID(0), CompPowerLossFactor(0.0), ConstitName(14), ConstitMolalFract(14, 0.0), GasLibID(14, 0), LHV(0.0),
              LHVJperkg(0.0), LHVliquid(0.0), HHV(0.0), MW(0.0), eCO2(0.0), KmolPerSecToKgPerSec(0.0), StoicOxygenRate(0.0), TfuelIntoCompress(0.0),
              TfuelIntoFCPM(0.0), PfuelCompEl(0.0), QskinLoss(0.0), CO2ProductGasCoef(0.0), H2OProductGasCoef(0.0)
        {
        }
    };

    struct GasPropertyDataStruct
    {
        // Members
        std::string ConstituentName;
        std::string ConstituentFormula;
        Real64 StdRefMolarEnthOfForm;
        DataGenerators::ThermodynamicMode ThermoMode; // method of calculation for thermodynamics
        Real64 ShomateA;
        Real64 ShomateB;
        Real64 ShomateC;
        Real64 ShomateD;
        Real64 ShomateE;
        Real64 ShomateF;
        Real64 ShomateG;
        Real64 ShomateH;
        Real64 NumCarbons;
        Real64 NumHydrogens;
        Real64 NumOxygens;
        Real64 MolecularWeight;
        Real64 NASA_A1;
        Real64 NASA_A2;
        Real64 NASA_A3;
        Real64 NASA_A4;
        Real64 NASA_A5;
        Real64 NASA_A6;
        Real64 NASA_A7;

        // Default Constructor
        GasPropertyDataStruct()
            : StdRefMolarEnthOfForm(0.0), ThermoMode(DataGenerators::ThermodynamicMode::Unassigned), ShomateA(0.0), ShomateB(0.0), ShomateC(0.0),
              ShomateD(0.0), ShomateE(0.0), ShomateF(0.0), ShomateG(0.0), ShomateH(0.0), NumCarbons(0.0), NumHydrogens(0.0), NumOxygens(0.0),
              MolecularWeight(0.0), NASA_A1(0.0), NASA_A2(0.0), NASA_A3(0.0), NASA_A4(0.0), NASA_A5(0.0), NASA_A6(0.0), NASA_A7(0.0)
        {
        }
    };

    struct GeneratorDynamicsManagerStruct
    {
        // Members
        // user input data
        std::string Name;
        Real64 PelMin;              // minimum operating point for electrical power Pel
        Real64 PelMax;              // maximum operating point for electrical power Pel
        Real64 UpTranLimit;         // power up transient limit W/s
        Real64 DownTranLimit;       // power down tran limit  W/s
        Real64 UpTranLimitFuel;     // fuel up transient limit kg/s
        Real64 DownTranLimitFuel;   // fuel down transient limit kg/s
        bool WarmUpByTimeDelay;     // Warm up mode control
        bool WarmUpByEngineTemp;    // Warm up mode control
        Real64 StartUpTimeDelay;    // time for start up [hours]
        Real64 WarmUpDelay;         // time for warm up delay [s]
        Real64 StartUpFuel;         // fuel use during start up
        Real64 StartUpElectConsum;  // electricity used during start up
        Real64 StartUpElectProd;    // electricity produced during start up
        Real64 ShutDownFuel;        // fuel consumed during shut down
        Real64 ShutDownElectConsum; // Elect consumed during shut down
        Real64 PcoolDown;           // power during cool down
        Real64 CoolDownDelay;       // time for cool down delay [hours]
        int NumCyclesInit;          // number of start stop cycles at beginning
        Real64 NumRunHoursInit;     // number of hours of operation beginning
        Real64 Pstandby;            // standby power [w]
        Real64 MCeng;               // aggregated thermal mass of engine [  ]
        Real64 MCcw;                // aggregated thermal mass of heat recovery [   ]
        Real64 kf;                  // coefficient k_f for warmup fuel flow rate
        Real64 TnomEngOp;           // nominal engine operating temperature [C]
        Real64 kp;                  // coefficient k_p for warmup power
        bool MandatoryFullCoolDown;
        bool WarmRestartOkay;
        int AvailabilitySchedID;
        // Calculated values and input from elsewhere
        DataGenerators::OperatingMode CurrentOpMode; // current operating mode, uses params like OpModeNormal
        DataGenerators::OperatingMode LastOpMode;
        Real64 FractionalDayofLastShutDown;
        Real64 FractionalDayofLastStartUp;
        bool HasBeenOn;
        bool DuringStartUp;
        bool DuringShutDown;
        Real64 FuelMdotLastTimestep;
        Real64 PelLastTimeStep;
        int NumCycles;
        Real64 PLRforSubtimestepStartUp;
        Real64 PLRforSubtimestepShutDown; // part load for not in shut down, shut down part is (1 - PLR)
        Real64 ElectEffNom;               // efficiency to use for control decisions
        Real64 ThermEffNom;               // thermal efficiency to use fo control decisions
        Real64 QdotHXMax;                 // Thermal power max
        Real64 QdotHXMin;                 // thermal power min
        Real64 QdotHXOpt;                 // thermal power nominal/optimal

        // Default Constructor
        GeneratorDynamicsManagerStruct()
            : PelMin(0.0), PelMax(0.0), UpTranLimit(0.0), DownTranLimit(0.0), UpTranLimitFuel(0.0), DownTranLimitFuel(0.0), WarmUpByTimeDelay(false),
              WarmUpByEngineTemp(true), StartUpTimeDelay(0.0), WarmUpDelay(0.0), StartUpFuel(0.0), StartUpElectConsum(0.0), StartUpElectProd(0.0),
              ShutDownFuel(0.0), ShutDownElectConsum(0.0), PcoolDown(0.0), CoolDownDelay(0.0), NumCyclesInit(0), NumRunHoursInit(0.0), Pstandby(0.0),
              MCeng(0.0), MCcw(0.0), kf(0.0), TnomEngOp(0.0), kp(0.0), MandatoryFullCoolDown(false), WarmRestartOkay(true), AvailabilitySchedID(0),
              CurrentOpMode(DataGenerators::OperatingMode::OpModeOff), LastOpMode(DataGenerators::OperatingMode::OpModeOff),
              FractionalDayofLastShutDown(0.0), FractionalDayofLastStartUp(0.0), HasBeenOn(false), DuringStartUp(false), DuringShutDown(false),
              FuelMdotLastTimestep(0.0), PelLastTimeStep(0.0), NumCycles(0), PLRforSubtimestepStartUp(0.0), PLRforSubtimestepShutDown(0.0),
              ElectEffNom(0.0), ThermEffNom(0.0), QdotHXMax(0.0), QdotHXMin(0.0), QdotHXOpt(0.0)
        {
        }
    };

} // namespace DataGenerators

struct GeneratorsData : BaseGlobalStruct
{
    int NumFuelConstit = 0;
    int NumGeneratorFuelSups = 0;
    int NumGensWDynamics = 0; // number of dynamics controls for generators
    Array1D<DataGenerators::GasPropertyDataStruct> GasPhaseThermoChemistryData;
    Array1D<DataGenerators::GeneratorFuelSupplyDataStruct> FuelSupply; // fuel supply (reused across various)
    Array1D<DataGenerators::GeneratorDynamicsManagerStruct> GeneratorDynamics;

    int InletCWnode = 0; // cooling water inlet node ID
    bool InternalFlowControl = false;
    Real64 TcwIn = 0.0;          // inlet cooling water temperature (C)
    Real64 TrialMdotcw = 0.0;    // test or estimate of what the plant flows are going to be (kg/s)
    Real64 LimitMinMdotcw = 0.0; // lower limit for cooling water flow for generatior operation (kg/s)

    void clear_state() override
    {
        NumFuelConstit = 0;
        NumGeneratorFuelSups = 0;
        NumGensWDynamics = 0;
        GasPhaseThermoChemistryData.deallocate();
        FuelSupply.deallocate();
        GeneratorDynamics.deallocate();
        this->InletCWnode = 0;
        this->InternalFlowControl = false;
        this->TcwIn = 0.0;
        this->TrialMdotcw = 0.0;
        this->LimitMinMdotcw = 0.0;
    }
};

} // namespace EnergyPlus

#endif
