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

#ifndef DataGenerators_hh_INCLUDED
#define DataGenerators_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>

// EnergyPlus Headers
#include <EnergyPlus/DataGlobals.hh>
#include <EnergyPlus/EnergyPlus.hh>

namespace EnergyPlus {

namespace DataGenerators {

    // Using/Aliasing

    // Data
    // -only module should be available to other modules and routines.
    // Thus, all variables in this module must be PUBLIC.

    // MODULE PARAMETER DEFINITIONS:
    extern int const NormalizedCurveMode; // mode where efficiency curves are modifier curves
    extern int const DirectCurveMode;     // mode where efficiency curves are direct

    extern int const ConstantRateSkinLoss;  // fixed rate mode for skin losses
    extern int const UADTSkinLoss;          // UAdelta T mode for skin losses
    extern int const QuadraticFuelNdotSkin; // Quadratic function of fuel flow for skin losses

    extern int const QuadraticFuncofNdot;  // function of fuel rate mode for air flow
    extern int const ConstantStoicsAirRat; // Constant air ratio in stoics with fuel constituents
    extern int const QuadraticFuncofPel;   // function of electric power mode

    extern int const NoRecoveryOnAirIntake;  // mode for controlling intake air heat recovery
    extern int const RecoverBurnInvertBatt;  // mode for controlling intake air heat recovery
    extern int const RecoverAuxiliaryBurner; // mode for controlling intake air heat recovery
    extern int const RecoverInverterBatt;    // mode for controlling intake air heat recovery
    extern int const RecoverInverter;        // mode for controlling intake air heat recovery
    extern int const RecoverBattery;         // mode for controlling intake air heat recovery

    extern int const RegularAir;
    extern int const UserDefinedConstituents;

    extern int const FuelInTempFromNode;
    extern int const FuelInTempSchedule;

    extern int const WaterInReformMains;
    extern int const WaterInReformAirNode;
    extern int const WaterInReformWaterNode;
    extern int const WaterInReformSchedule;

    extern int const InverterEffConstant;
    extern int const InverterEffQuadratic;

    extern int const FixedEffectiveness;   // exhaust gas HX modeling mode
    extern int const LMTDempiricalUAeff;   // exhaust gas HX modeling mode
    extern int const LMTDfundementalUAeff; // exhaust gas HX modeling mode
    extern int const Condensing;           // exhaust gas HX modeling mode

    extern int const SimpleEffConstraints;         // electrical storage modeling mode
    extern int const LeadAcidBatterySaupe;         // electrical storage modeling mode
    extern int const LeadAcidBatterManwellMcGowan; // electrical storage modeling mode

    extern int const SurroundingZone;
    extern int const AirInletForFC;

    extern int const OpModeOff;      // CHP operating mode OFF
    extern int const OpModeStandby;  // CHP operating mode Stand By
    extern int const OpModeWarmUp;   // CHP operating mode Warm Up or start up
    extern int const OpModeNormal;   // CHP operating mode Normal
    extern int const OpModeCoolDown; // CHP operating mode Cool down or shut down

    extern int const fuelModeGaseousConstituents;
    extern int const fuelModeGenericLiquid;

    extern Real64 const MinProductGasTemp; // Minimum bound on search for product gas temps
    extern Real64 const MaxProductGasTemp; // Maximum bound on search for product gas temps

    extern int const NISTShomate;
    extern int const NASAPolynomial;

    extern Real64 const RinKJperMolpK; // R is ideal gas constant (kJ/mol-K)
    extern Real64 const InitHRTemp;    // Initialization temperature for heat recovery water

    extern Real64 const ImBalanceTol; // used as fraction of electrical power at power module

    // DERIVED TYPE DEFINITIONS

    // MODULE VARIABLE DECLARATIONS:

    extern int NumFuelConstit;
    extern int NumGeneratorFuelSups;
    extern int NumMicroCHPs;
    extern int NumMicroCHPParams; // number of parameter sets for micro chp
    extern int NumGensWDynamics;  // number of dynamics controls for generators

    // Types

    struct GeneratorFuelSupplyDataStruct
    {
        // Members
        // user input data
        std::string Name;     // name of this fuel supply module
        int FuelTempMode;     // temperature of fuel node
        int FuelTypeMode;     // type of fuel, gasous or liquid
        std::string NodeName; // node name for temperature at input
        int NodeNum;          // node number for temperature at input
        int SchedNum;         // fuel temperature at input
        int CompPowerCurveID; // "pointer" to compressor power cubic curve
        Real64 CompPowerLossFactor;
        int NumConstituents; // number of constituents in fue supply
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
        Real64 H20ProductGasCoef; // molar multiplier for stoic products of this fuel

        // Default Constructor
        GeneratorFuelSupplyDataStruct()
            : FuelTempMode(0), FuelTypeMode(0), NodeNum(0), SchedNum(0), CompPowerCurveID(0), CompPowerLossFactor(0.0), ConstitName(14),
              ConstitMolalFract(14, 0.0), GasLibID(14, 0), LHV(0.0), LHVJperkg(0.0), LHVliquid(0.0), HHV(0.0), MW(0.0), eCO2(0.0),
              KmolPerSecToKgPerSec(0.0), StoicOxygenRate(0.0), TfuelIntoCompress(0.0), TfuelIntoFCPM(0.0), PfuelCompEl(0.0), QskinLoss(0.0),
              CO2ProductGasCoef(0.0), H20ProductGasCoef(0.0)
        {
        }
    };

    struct GasPropertyDataStruct
    {
        // Members
        std::string ConstituentName;
        std::string ConstituentFormula;
        Real64 StdRefMolarEnthOfForm;
        int ThermoMode; // method of calculation for thermodynamics
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
            : StdRefMolarEnthOfForm(0.0), ThermoMode(0), ShomateA(0.0), ShomateB(0.0), ShomateC(0.0), ShomateD(0.0), ShomateE(0.0), ShomateF(0.0),
              ShomateG(0.0), ShomateH(0.0), NumCarbons(0.0), NumHydrogens(0.0), NumOxygens(0.0), MolecularWeight(0.0), NASA_A1(0.0), NASA_A2(0.0),
              NASA_A3(0.0), NASA_A4(0.0), NASA_A5(0.0), NASA_A6(0.0), NASA_A7(0.0)
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
        int CurrentOpMode; // current operating mode, uses params like OpModeNormal
        int LastOpMode;
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
              CurrentOpMode(OpModeOff), LastOpMode(OpModeOff), FractionalDayofLastShutDown(0.0), FractionalDayofLastStartUp(0.0), HasBeenOn(false),
              DuringStartUp(false), DuringShutDown(false), FuelMdotLastTimestep(0.0), PelLastTimeStep(0.0), NumCycles(0),
              PLRforSubtimestepStartUp(0.0), PLRforSubtimestepShutDown(0.0), ElectEffNom(0.0), ThermEffNom(0.0), QdotHXMax(0.0), QdotHXMin(0.0),
              QdotHXOpt(0.0)
        {
        }
    };

    struct MicroCHPParamsNonNormalized
    {
        // Members
        // user parameters
        std::string Name;         // name of this PowerModule data
        Real64 MaxElecPower;      // net electric power [W]
        Real64 MinElecPower;      // net electric power [W]
        Real64 MinWaterMdot;      // minimum cooling water flow [kg/s]
        Real64 MaxWaterTemp;      // limit temp for inlet cooling water [C]
        int ElecEffCurveID;       // index for TriQuadratic for electrical efficiency
        int ThermalEffCurveID;    // index for TriQuadric for thermal efficiency
        bool InternalFlowControl; // Plant or Internal Flow rate control?
        bool PlantFlowControl;    // default is plant control
        int WaterFlowCurveID;     // index for BiQuadratic for water flow rate internal control
        int AirFlowCurveID;       // index for Quadratic for generator air flow
        Real64 DeltaPelMax;       // max rate of change in net electric power [W/s}
        Real64 DeltaFuelMdotMax;  // Maximum Rate of change in fuel flow rate [kmol/s2]
        Real64 UAhx;              // heat exchanger UA [W/K]
        Real64 UAskin;            // skin loss UA [W/K]
        Real64 RadiativeFraction; // skin loss fraction to radiant energy []
        Real64 MCeng;             // aggregated thermal mass of engine [J/K]
        Real64 MCcw;              // aggregated thermal mass of heat recovery [J/k]
        Real64 Pstandby;          // standby power [w]
        bool WarmUpByTimeDelay;   // Warm up mode control
        bool WarmUpByEngineTemp;  // Warm up mode control
        Real64 kf;                // coefficient k_f for warmup fuel flow rate
        Real64 TnomEngOp;         // nominal engine operating temperature [C]
        Real64 kp;                // coefficient k_p for warmup power
        Real64 Rfuelwarmup;       // Warm Up Fuel Flow Rate Limit Ratio
        Real64 WarmUpDelay;       // time for warm up delay [s]
        Real64 PcoolDown;         // power during cool down
        Real64 CoolDownDelay;     // time for cool down delay [s]
        bool MandatoryFullCoolDown;
        bool WarmRestartOkay;
        // calculated and from elsewhere
        Real64 TimeElapsed; // Fraction of the current hour that has elapsed (h)
        // Saved in order to identify the beginning of a new system time
        int OpMode;
        Real64 OffModeTime;      // amount of time generator spent in Off mode
        Real64 StandyByModeTime; // amount of time generator spent in standby mode
        Real64 WarmUpModeTime;   // amount of time generator spent in warm up mode
        Real64 NormalModeTime;   // amount of time generator spent in normal mode
        Real64 CoolDownModeTime; // amount of time generator spent in Cool down mode
        Real64 TengLast;         // last timestep's value for engine temperature
        Real64 TempCWOutLast;    // last timestep's value for cooling water outlet temperature
        Real64 Pnet;
        Real64 ElecEff;
        Real64 Qgross;
        Real64 ThermEff;
        Real64 Qgenss;
        Real64 NdotFuel;
        Real64 MdotFuel;
        Real64 Teng;
        Real64 TcwIn;
        Real64 TcwOut;
        Real64 MdotAir;
        Real64 QdotSkin; // rate of heat loss to zone
        Real64 QdotConvZone;
        Real64 QdotRadZone;

        // Default Constructor
        MicroCHPParamsNonNormalized()
            : MaxElecPower(0.0), MinElecPower(0.0), MinWaterMdot(0.0), MaxWaterTemp(0.0), ElecEffCurveID(0), ThermalEffCurveID(0),
              InternalFlowControl(false), PlantFlowControl(true), WaterFlowCurveID(0), AirFlowCurveID(0), DeltaPelMax(0.0), DeltaFuelMdotMax(0.0),
              UAhx(0.0), UAskin(0.0), RadiativeFraction(0.0), MCeng(0.0), MCcw(0.0), Pstandby(0.0), WarmUpByTimeDelay(false),
              WarmUpByEngineTemp(true), kf(0.0), TnomEngOp(0.0), kp(0.0), Rfuelwarmup(0.0), WarmUpDelay(0.0), PcoolDown(0.0), CoolDownDelay(0.0),
              MandatoryFullCoolDown(false), WarmRestartOkay(true), TimeElapsed(0.0), OpMode(0), OffModeTime(0.0), StandyByModeTime(0.0),
              WarmUpModeTime(0.0), NormalModeTime(0.0), CoolDownModeTime(0.0), TengLast(20.0), TempCWOutLast(20.0), Pnet(0.0), ElecEff(0.0),
              Qgross(0.0), ThermEff(0.0), Qgenss(0.0), NdotFuel(0.0), MdotFuel(0.0), Teng(20.0), TcwIn(20.0), TcwOut(20.0), MdotAir(0.0),
              QdotSkin(0.0), QdotConvZone(0.0), QdotRadZone(0.0)
        {
        }
    };

    struct MicroCHPReportDataStruct // these are all for reporting only!
    {
        // Members
        int Mode;                    // report operating mode (dev only, remove at end)
        Real64 OffModeTime;          // amount of time generator spent in Off mode
        Real64 StandyByModeTime;     // amount of time generator spent in standby mode
        Real64 WarmUpModeTime;       // amount of time generator spent in warm up mode
        Real64 NormalModeTime;       // amount of time generator spent in normal mode
        Real64 CoolDownModeTime;     // amount of time generator spent in Cool down mode
        Real64 ACPowerGen;           // reporting: power (W)
        Real64 ACEnergyGen;          // reporting: energy (J)
        Real64 QdotGross;            // reporting: interim gross power (W)
        Real64 Qgenss;               // reporting: net recovered heat rate steadystate(0)
        Real64 QdotHX;               // reporting: rate of heat exchange from engine to coolant (W)
        Real64 QdotHR;               // reporting: rate of heat recovered (W)
        Real64 Tengine;              // reporting: engine mass temperature (C)
        Real64 TotalHeatEnergyRec;   // reporting: total heat recovered (J)
        Real64 ExhaustEnergyRec;     // reporting: exhaust gas heat recovered (J)
        Real64 FuelEnergyLHV;        // reporting: Fuel Energy used in Lower Heating Value(J)
        Real64 FuelEnergyUseRateLHV; // reporting: Fuel Energy used in Lower Heating Value(W)
        Real64 FuelEnergyHHV;        // reporting: Fuel Energy used in Higher Heating Value(J)
        Real64 FuelEnergyUseRateHHV; // reporting: Fuel Energy used in Higher Heating Value(W)
        Real64 HeatRecInletTemp;     // reporting: Heat Recovery Loop Inlet Temperature (C)
        Real64 HeatRecOutletTemp;    // reporting: Heat Recovery Loop Outlet Temperature (C)
        Real64 HeatRecMdot;          // reporting: Heat Recovery Loop Mass flow rate (kg/s)
        // air supply and blower
        Real64 TairInlet; // State point 1
        Real64 MdotAir;   // air flow in kmol/sec
        // fuel supply and compressor
        Real64 TfuelInlet;           // State point 2 [C]
        Real64 NdotFuel;             // fuel flow in [kmol/sec]
        Real64 MdotFuel;             // fuel flow in [kg/s]
        Real64 FuelCompressPower;    // electrical power used by fuel supply compressor [W]
        Real64 FuelCompressEnergy;   // electrical energy used by fuel supply compressor [J]
        Real64 FuelCompressSkinLoss; // heat rate of losses.by fuel supply compressor [W]
        // heat exchanger for water to exhaust heat recovery
        //   REAL(r64)    :: qHX = 0.0d0 ! heat flow from gas stream to water [W]
        //   REAL(r64)    :: HXenergy = 0.0d0 !energy from gas stream to water [J]
        //   REAL(r64)    :: THXexh = 0.0d0 ! temperature of exhaust gases leaving heat exchanger.
        //   REAL(r64)    :: WaterVaporFractExh = 0.0d0 ! water vapor fraction in exhaust gas stream
        // relative to water vapor entering HX  (NdotH20/Ndoaux-mix)
        //    INTEGER :: SeqSubstIterations = 0 ! number of iterations in SOFC loop
        //    INTEGER :: RegulaFalsiIterations = 0 ! number of iterations in Tproduct gas solving
        Real64 SkinLossPower;   // heat loss to surrounding zone [W]
        Real64 SkinLossEnergy;  // heat loss to surround zone [J]
        Real64 SkinLossConvect; // convective heat loss to zone [W]
        Real64 SkinLossRadiat;  // radiative heat loss to zone [W}
        Real64 ElectEfficiency;
        Real64 ThermalEfficiency;
        Real64 OverallEfficiency;

        // Default Constructor
        MicroCHPReportDataStruct()
            : Mode(0), OffModeTime(0.0), StandyByModeTime(0.0), WarmUpModeTime(0.0), NormalModeTime(0.0), CoolDownModeTime(0.0), ACPowerGen(0.0),
              ACEnergyGen(0.0), QdotGross(0.0), Qgenss(0.0), QdotHX(0.0), QdotHR(0.0), Tengine(0.0), TotalHeatEnergyRec(0.0), ExhaustEnergyRec(0.0),
              FuelEnergyLHV(0.0), FuelEnergyUseRateLHV(0.0), FuelEnergyHHV(0.0), FuelEnergyUseRateHHV(0.0), HeatRecInletTemp(0.0),
              HeatRecOutletTemp(0.0), HeatRecMdot(0.0), TairInlet(0.0), MdotAir(0.0), TfuelInlet(0.0), NdotFuel(0.0), MdotFuel(0.0),
              FuelCompressPower(0.0), FuelCompressEnergy(0.0), FuelCompressSkinLoss(0.0), SkinLossPower(0.0), SkinLossEnergy(0.0),
              SkinLossConvect(0.0), SkinLossRadiat(0.0), ElectEfficiency(0.0), ThermalEfficiency(0.0), OverallEfficiency(0.0)
        {
        }
    };

    struct MicroCHPDataStruct
    {
        // Members
        // user input data
        std::string Name;                     // name of this Micro CHP Generator
        std::string ParamObjName;             // name of parameter object
        MicroCHPParamsNonNormalized A42Model; // Nested parameter data structure
        bool ModelTypeAnnex42;                // normalized =  non-normalized?
        Real64 NomEff;                        // nominal efficiency
        std::string ZoneName;
        int ZoneID;
        std::string PlantInletNodeName;
        int PlantInletNodeID;
        std::string PlantOutletNodeName;
        int PlantOutletNodeID;
        Real64 PlantMassFlowRate;              // only if internal control
        Real64 PlantMassFlowRateMax;           // hardware limit for node%massflowrateMax
        bool PlantMassFlowRateMaxWasAutoSized; // true if mass flow rate was autosized on input
        std::string AirInletNodeName;
        int AirInletNodeID;
        std::string AirOutletNodeName;
        int AirOutletNodeID;
        MicroCHPReportDataStruct Report; // structure of report variables
        int FuelSupplyID;                // index for fuel supply data structure
        int DynamicsControlID;           // index in GeneratorDynamics data where control issues are handled
        int AvailabilitySchedID;         // index for availability schedule
        int CWLoopNum;                   // cooling water plant loop index number
        int CWLoopSideNum;               // cooling water plant loop side index
        int CWBranchNum;                 // cooling water plant loop branch index
        int CWCompNum;                   // cooling water plant loop component index

        // Default Constructor
        MicroCHPDataStruct()
            : ModelTypeAnnex42(true), NomEff(0.0), ZoneID(0), PlantInletNodeID(0), PlantOutletNodeID(0), PlantMassFlowRate(0.0),
              PlantMassFlowRateMax(0.0), PlantMassFlowRateMaxWasAutoSized(false), AirInletNodeID(0), AirOutletNodeID(0), FuelSupplyID(0),
              DynamicsControlID(0), AvailabilitySchedID(0), CWLoopNum(0), CWLoopSideNum(0), CWBranchNum(0), CWCompNum(0)
        {
        }
    };

    // Object Data
    extern Array1D<GasPropertyDataStruct> GasPhaseThermoChemistryData;
    extern Array1D<GeneratorFuelSupplyDataStruct> FuelSupply; // fuel supply (reused across various)
    extern Array1D<MicroCHPDataStruct> MicroCHP;
    extern Array1D<MicroCHPParamsNonNormalized> MicroCHPParamInput; // Used during get input then put into nested
    extern Array1D<GeneratorDynamicsManagerStruct> GeneratorDynamics;

    void clear_state();

} // namespace DataGenerators

} // namespace EnergyPlus

#endif
