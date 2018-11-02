// EnergyPlus, Copyright (c) 1996-2018, The Board of Trustees of the University of Illinois,
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

// EnergyPlus Headers
#include <DataPrecisionGlobals.hh>
#include <DataSizing.hh>

namespace EnergyPlus {

namespace DataSizing {

    // MODULE INFORMATION:
    //       AUTHOR         Fred Buhl
    //       DATE WRITTEN   December 2000
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS MODULE:
    // This data-only module contains type definitions and variables
    // associated with HVAC system design flow rates, temperatures and
    // capacities. This data is available to the HVAC component modules
    // for their self sizing calculations.

    // REFERENCES:
    // na

    // OTHER NOTES:
    // na

    // Using/Aliasing
    using namespace DataPrecisionGlobals;

    // Data
    // -only module should be available to other modules and routines.
    // Thus, all variables in this module must be PUBLIC.

    // MODULE PARAMETER DEFINITIONS:

    // parameters for outside air flow method
    int const NumOAFlowMethods(9);

    int const OAFlowNone(0);
    int const OAFlowPPer(1);
    int const OAFlow(2);
    int const OAFlowPerArea(3);
    int const OAFlowACH(4);
    int const OAFlowSum(5);
    int const OAFlowMax(6);

    Array1D_string const cOAFlowMethodTypes(NumOAFlowMethods,
                                            {"Flow/Person",
                                             "Flow/Zone",
                                             "Flow/Area",
                                             "AirChanges/Hour",
                                             "Sum",
                                             "Maximum",
                                             "IndoorAirQualityProcedure",
                                             "ProportionalControlBasedonOccupancySchedule",
                                             "ProportionalControlBasedOnDesignOccupancy"});

    // parameters for outside air
    int const AllOA(1);
    int const MinOA(2);

    // parameters for loop fluid type
    int const HeatingLoop(1);
    int const CoolingLoop(2);
    int const CondenserLoop(3);
    int const SteamLoop(4);

    // paramters for sizing
    int const NonCoincident(1);
    int const Coincident(2);

    // parameters for Cooling Peak Load TYpe
    int const SensibleCoolingLoad(1);
    int const TotalCoolingLoad(2);

    // parameters for Central Cooling Capacity Control Method
    int const VAV(1);
    int const Bypass(2);
    int const VT(3);
    int const OnOff(4);

    // paramters for supply air flow rate method
    int const SupplyAirTemperature(1);
    int const TemperatureDifference(2);

    // paramters for sizing
    int const FromDDCalc(1);
    int const InpDesAirFlow(2);
    int const DesAirFlowWithLim(3);

    int const DOANeutralSup(1);
    int const DOANeutralDehumSup(2);
    int const DOACoolSup(3);

    // parameters for Type of Load to Size On
    int const Sensible(0);
    int const Latent(1);
    int const Total(2);
    int const Ventilation(3);

    // parameter for autosize
    Real64 const AutoSize(-99999.0);

    // parameter for (time-of-peak) sizing format
    gio::Fmt PeakHrMinFmt("(I2.2,':',I2.2,':00')");

    // Zone Outdoor Air Method
    int const ZOAM_FlowPerPerson(1); // set the outdoor air flow rate based on number of people in the zone
    int const ZOAM_FlowPerZone(2);   // sum the outdoor air flow rate per zone based on user input
    int const ZOAM_FlowPerArea(3);   // sum the outdoor air flow rate based on zone area
    int const ZOAM_FlowPerACH(4);    // sum the outdoor air flow rate based on number of air changes for the zone
    int const ZOAM_Sum(5);           // sum the outdoor air flow rate of the people component and the space floor area component
    int const ZOAM_Max(6);           // use the maximum of the outdoor air flow rate of the people component and the space floor area component
    int const ZOAM_IAQP(7);          // Use ASHRAE Standard 62.1-2007 IAQP to calculate the zone level outdoor air flow rates
    int const ZOAM_ProportionalControlSchOcc(8); // Use ASHRAE Standard 62.1-2004 or Trane Engineer's newsletter (volume 34-5)
                                                 // to calculate the zone level outdoor air flow rates based on scheduled occupancy
    int const ZOAM_ProportionalControlDesOcc(9); // Use ASHRAE Standard 62.1-2004 or Trane Engineer's newsletter (volume 34-5)
                                                 // to calculate the zone level outdoor air flow rates based on design occupancy

    // System Outdoor Air Method
    int const SOAM_ZoneSum(1); // Sum the outdoor air flow rates of all zones
    int const SOAM_VRP(2);     // Use ASHRAE Standard 62.1-2007 to calculate the system level outdoor air flow rates
    //  considering the zone air distribution effectiveness and the system ventilation efficiency
    int const SOAM_IAQP(3); // Use ASHRAE Standard 62.1-2007 IAQP to calculate the system level outdoor air flow rates
    // based on the CO2 setpoint
    int const SOAM_ProportionalControlSchOcc(4); // Use ASHRAE Standard 62.1-2004 or Trane Engineer's newsletter (volume 34-5)
    // to calculate the system level outdoor air flow rates based on scheduled occupancy
    int const SOAM_IAQPGC(5); // Use ASHRAE Standard 62.1-2004 IAQP to calculate the system level outdoor air flow rates
    // based on the generic contaminant setpoint
    int const SOAM_IAQPCOM(6); // Take the maximum outdoor air rate from both CO2 and generic contaminant controls
    // based on the generic contaminant setpoint
    int const SOAM_ProportionalControlDesOcc(7); // Use ASHRAE Standard 62.1-2004 or Trane Engineer's newsletter (volume 34-5)
    // to calculate the system level outdoor air flow rates based on design occupancy
    int const SOAM_ProportionalControlDesOARate(8); // Calculate the system level outdoor air flow rates based on design OA rate

    // Zone HVAC Equipment Supply Air Sizing Option
    int const None(1);
    int const SupplyAirFlowRate(2);
    int const FlowPerFloorArea(3);
    int const FractionOfAutosizedCoolingAirflow(4);
    int const FractionOfAutosizedHeatingAirflow(5);
    int const FlowPerCoolingCapacity(6);
    int const FlowPerHeatingCapacity(7);

    int const CoolingDesignCapacity(8);
    int const HeatingDesignCapacity(9);
    int const CapacityPerFloorArea(10);
    int const FractionOfAutosizedCoolingCapacity(11);
    int const FractionOfAutosizedHeatingCapacity(12);

    int const NoSizingFactorMode(101);
    int const GlobalHeatingSizingFactorMode(102);
    int const GlobalCoolingSizingFactorMode(103);
    int const LoopComponentSizingFactorMode(104);

    // DERIVED TYPE DEFINITIONS:

    // INTERFACE BLOCK SPECIFICATIONS
    // na

    // MODULE VARIABLE DECLARATIONS:

    //  days; includes effects of user multiplier
    //  and user set flows)
    //  of user input multiplier and flows
    //  all design days, calculated only)
    //  using user input system flow rates.
    //  before applying user input sys flow rates.

    int NumOARequirements(0);                       // Number of OA Requirements objects
    int NumZoneAirDistribution(0);                  // Number of zone air distribution objects
    int NumZoneSizingInput(0);                      // Number of Zone Sizing objects
    int NumSysSizInput(0);                          // Number of System Sizing objects
    int NumPltSizInput(0);                          // Number of Plant Sizing objects
    int CurSysNum(0);                               // Current Air System index (0 if not in air loop)
    int CurOASysNum(0);                             // Current outside air system index (0 if not in OA Sys)
    int CurZoneEqNum(0);                            // Current Zone Equipment index (0 if not simulating ZoneEq)
    int CurTermUnitSizingNum;                       // Current terminal unit sizing index for TermUnitSizing and TermUnitFinalZoneSizing
    int CurBranchNum(0);                            // Index of branch being simulated (or 0 if not air loop)
    int CurDuctType(0);                             // Duct type of current branch
    int CurLoopNum(0);                              // the current plant loop index
    int CurCondLoopNum(0);                          // the current condenser loop number
    int CurEnvirNumSimDay(0);                       // current environment number for day simulated
    int CurOverallSimDay(0);                        // current day of simulation
    int NumTimeStepsInAvg(0);                       // number of time steps in the averaging window for the design flow and load sequences
    int SaveNumPlantComps(0);                       // Number of components using water as an energy source or sink (e.g. water coils)
    int DataTotCapCurveIndex(0);                    // index to total capacity as a function of temperature curve
    Real64 DataTotCapCurveValue(0);                 // value of total capacity as a function of temperature curve for CoilVRF_FluidTCtrl_*
    int DataPltSizCoolNum(0);                       // index to cooling plant sizing data
    int DataPltSizHeatNum(0);                       // index to heating plant sizing data
    int DataWaterLoopNum(0);                        // index to plant water loop
    int DataCoilNum(0);                             // index to coil object
    int DataFanOpMode(0);                           // fan operating mode (ContFanCycCoil or CycFanCycCoil)
    bool DataCoilIsSuppHeater(false);               // TRUE if heating coil used as supplemental heater
    bool DataIsDXCoil(false);                       // TRUE if direct-expansion coil
    bool DataAutosizable(true);                     // TRUE if component is autosizable
    bool DataEMSOverrideON(false);                  // boolean determines if user relies on EMS to override autosizing
    bool DataScalableSizingON(false);               // boolean determines scalable flow sizing is specified
    bool DataScalableCapSizingON(false);            // boolean determines scalable capacity sizing is specified
    bool DataSysScalableFlowSizingON(false);        // boolean determines scalable system flow sizing is specified
    bool DataSysScalableCapSizingON(false);         // boolean determines scalable system capacity sizing is specified
    bool SysSizingRunDone(false);                   // True if a system sizing run is successfully completed.
    bool TermUnitSingDuct(false);                   // TRUE if a non-induction single duct terminal unit
    bool TermUnitPIU(false);                        // TRUE if a powered induction terminal unit
    bool TermUnitIU(false);                         // TRUE if an unpowered induction terminal unit
    bool ZoneEqFanCoil(false);                      // TRUE if a 4 pipe fan coil unit is being simulated
    bool ZoneEqOutdoorAirUnit(false);               // TRUE if an OutdoorAirUnit is being simulated
    bool ZoneEqUnitHeater(false);                   // TRUE if a unit heater is being simulated
    bool ZoneEqUnitVent(false);                     // TRUE if a unit ventilator unit is being simulated
    bool ZoneEqVentedSlab(false);                   // TRUE if a ventilated slab is being simulated
    bool ZoneEqDXCoil(false);                       // TRUE if a ZoneHVAC DX coil is being simulated
    bool ZoneEqUnitarySys(false);                   // TRUE if a zone UnitarySystem is being simulated
    bool ZoneCoolingOnlyFan(false);                 // TRUE if a ZoneHVAC DX cooling coil is only coil in parent
    bool ZoneHeatingOnlyFan(false);                 // TRUE if zone unit only does heating and contains a fam (such as Unit Heater)
    bool ZoneSizingRunDone(false);                  // True if a zone sizing run has been successfully completed.
    bool DataErrorsFound(false);                    // used for simulation termination when errors are found
    Real64 AutoVsHardSizingThreshold(0.1);          // criteria threshold used to determine if user hard size and autosize disagree 10%
    Real64 AutoVsHardSizingDeltaTempThreshold(1.5); // temperature criteria threshold for autosize versus hard size [C]
    Real64 DataCoilSizingAirInTemp(0.0);            // saves sizing data for use in coil object reporting
    Real64 DataCoilSizingAirInHumRat(0.0);          // saves sizing data for use in coil object reporting
    Real64 DataCoilSizingAirOutTemp(0.0);           // saves sizing data for use in coil object reporting
    Real64 DataCoilSizingAirOutHumRat(0.0);         // saves sizing data for use in coil object reporting
    Real64 DataCoilSizingFanCoolLoad(0.0);          // saves sizing data for use in coil object reporting
    Real64 DataCoilSizingCapFT(1.0);                // saves sizing data for use in coil object reporting
    Real64 DataDesAccountForFanHeat(true);          // include fan heat when true
    Real64 DataDesInletWaterTemp(0.0);              // coil inlet water temperture used for warning messages
    Real64 DataDesInletAirHumRat(0.0);              // coil inlet air humidity ratio used for warning messages
    Real64 DataDesInletAirTemp(0.0);                // coil inlet air temperature used for warning messages
    Real64 DataDesOutletAirTemp(0.0);               // coil outlet air temperature used for sizing
    Real64 DataDesOutletAirHumRat(0.0);             // coil air outlet humidity ratio used in sizing calculations [kg water / kg dry air]
    Real64 DataCoolCoilCap(0.0);                    // cooling coil capacity used for sizing with scalable inputs [W]
    Real64 DataFlowUsedForSizing(0.0);              // air flow rate used for sizing with scalable inputs [m3/s]
    Real64 DataAirFlowUsedForSizing(0.0);           // air flow rate used for sizing with scalable inputs [m3/s]
    Real64 DataWaterFlowUsedForSizing(0.0);         // water flow rate used for sizing with scalable inputs [m3/s]
    Real64 DataCapacityUsedForSizing(0.0);          // capacity used for sizing with scalable inputs [W]
    Real64 DataDesignCoilCapacity(0.0);             // calculated capacity of coil at end of UA calculation
    Real64 DataHeatSizeRatio(1.0);                  // heating coil size as a ratio of cooling coil capacity
    Real64 DataEMSOverride(0.0);                    // value of EMS variable used to override autosizing
    Real64 DataBypassFrac(0.0);                     // value of bypass fraction for Coil:Cooling:DX:TwoStageWithHumidityControlMode coils
    Real64 DataFracOfAutosizedCoolingAirflow(1.0);  // fraction of design cooling supply air flow rate
    Real64 DataFracOfAutosizedHeatingAirflow(1.0);  // fraction of design heating supply air flow rate
    Real64 DataFlowPerCoolingCapacity(0.0);         // cooling supply air flow per unit cooling capacity
    Real64 DataFlowPerHeatingCapacity(0.0);         // heating supply air flow per unit heating capacity
    Real64 DataFracOfAutosizedCoolingCapacity(1.0); // fraction of autosized cooling capacity
    Real64 DataFracOfAutosizedHeatingCapacity(1.0); // fraction of autosized heating capacit
    Real64 DataAutosizedCoolingCapacity(0.0);       // Autosized cooling capacity used for multiplying flow per capacity to get flow rate
    Real64 DataAutosizedHeatingCapacity(0.0);       // Autosized heating capacit used for multiplying flow per capacity to get flow rate
    Real64 DataConstantUsedForSizing(0.0);          // base value used for sizing inputs that are ratios of other inputs
    Real64 DataFractionUsedForSizing(0.0);          // fractional value of base value used for sizing inputs that are ratios of other inputs
    Real64 DataNonZoneNonAirloopValue(0.0);         // used when equipment is not located in a zone or airloop
    int DataZoneUsedForSizing(0);                   // pointer to control zone for air loop equipment
    int DataZoneNumber(0);                          // a pointer to a served by zoneHVAC equipment
    int NumZoneHVACSizing(0);                       // Number of design specification zone HVAC sizing objects
    int NumAirTerminalSizingSpec(0);                // Number of design specfication air terminal sizing objects
    int NumAirTerminalUnits(0);                     // Number of air terminal units (same as total number of zone inlet nodes)
    Real64 DXCoolCap(0.0);                          // The ARI cooling capacity of a DX unit.
    Real64 GlobalHeatSizingFactor(0.0);             // the global heating sizing ratio
    Real64 GlobalCoolSizingFactor(0.0);             // the global cooling sizing ratio
    Real64 SuppHeatCap(0.0);                        // the heating capacity of the supplemental heater in a unitary system
    Real64 UnitaryHeatCap(0.0);                     // the heating capacity of a unitary system
    Array1D<Real64> ZoneSizThermSetPtHi;            // highest zone thermostat setpoint during zone sizing calcs
    Array1D<Real64> ZoneSizThermSetPtLo;            // lowest zone thermostat setpoint during zone sizing calcs
    Array1D_string CoolPeakDateHrMin;               // date:hr:min of cooling peak
    Array1D_string HeatPeakDateHrMin;               // date:hr:min of heating peak
    char SizingFileColSep;                          // Character to separate columns in sizing outputs
    int DataDesicDehumNum(0);                       // index to desiccant dehumidifier
    bool DataDesicRegCoil(false);                   // TRUE if heating coil desiccant regeneration coil
    bool HRFlowSizingFlag(false);                   // True, if it is a heat recovery heat exchanger flow sizing
    Real64 DataWaterCoilSizCoolDeltaT(0.0);         // used for sizing cooling coil water design flow rate
    Real64 DataWaterCoilSizHeatDeltaT(0.0);         // used for sizing heating coil water design flow rate
    bool DataNomCapInpMeth(false);                  // True if heating coil is sized by CoilPerfInpMeth == NomCa
    int DataFanEnumType(-1);                        // Fan type used during sizing
    int DataFanIndex(-1);                           // Fan index used during sizing
    zoneFanPlacement DataFanPlacement(zoneFanPlacement::zoneFanPlaceNotSet); // identifies location of fan wrt coil

    // Object Data
    Array1D<OARequirementsData> OARequirements;
    Array1D<ZoneAirDistributionData> ZoneAirDistribution;
    Array1D<ZoneSizingInputData> ZoneSizingInput;             // Input data for zone sizing
    Array2D<ZoneSizingData> ZoneSizing;                       // Data for zone sizing (all data, all design)
    Array1D<ZoneSizingData> FinalZoneSizing;                  // Final data for zone sizing including effects
    Array2D<ZoneSizingData> CalcZoneSizing;                   // Data for zone sizing (all data)
    Array1D<ZoneSizingData> CalcFinalZoneSizing;              // Final data for zone sizing (calculated only)
    Array1D<ZoneSizingData> TermUnitFinalZoneSizing;          // Final data for sizing terminal units (indexed per terminal unit)
    Array1D<SystemSizingInputData> SysSizInput;               // Input data array for system sizing object
    Array2D<SystemSizingData> SysSizing;                      // Data array for system sizing (all data)
    Array1D<SystemSizingData> FinalSysSizing;                 // Data array for system sizing (max heat/cool)
    Array1D<SystemSizingData> CalcSysSizing;                  // Data array for system sizing (max heat/cool)
    Array1D<SysSizPeakDDNumData> SysSizPeakDDNum;             // data array for peak des day indices
    Array1D<TermUnitSizingData> TermUnitSizing;               // Data added in sizing routines (indexed per terminal unit)
    Array1D<ZoneEqSizingData> ZoneEqSizing;                   // Data added in zone eq component sizing routines
    Array1D<ZoneEqSizingData> UnitarySysEqSizing;             // Data added in unitary system sizing routines
    Array1D<ZoneEqSizingData> OASysEqSizing;                  // Data added in unitary system sizing routines
    Array1D<PlantSizingData> PlantSizData;                    // Input data array for plant sizing
    Array1D<DesDayWeathData> DesDayWeath;                     // design day weather saved at major time step
    Array1D<CompDesWaterFlowData> CompDesWaterFlow;           // array to store components' design water flow
    Array1D<ZoneHVACSizingData> ZoneHVACSizing;               // Input data for zone HVAC sizing
    Array1D<AirTerminalSizingSpecData> AirTerminalSizingSpec; // Input data for zone HVAC sizing
    // used only for Facility Load Component Summary
    Array1D<FacilitySizingData> CalcFacilitySizing; // Data for zone sizing
    FacilitySizingData CalcFinalFacilitySizing;     // Final data for zone sizing
    Array1D<Real64> VbzByZone;                      // saved value of ZoneOAUnc which is Vbz used in 62.1 tabular report
    Array1D<Real64> VdzClgByZone;    // saved value of cooling based ZoneSA which is Vdz used in 62.1 tabular report (also used for zone level Vps)
    Array1D<Real64> VdzMinClgByZone; // minimum discarge flow for cooling, Vdz includes secondary and primary flows for dual path
    Array1D<Real64> VdzHtgByZone;    // saved value of heating based ZoneSA which is Vdz used in 62.1 tabular report (also used for zone level Vps)
    Array1D<Real64> VdzMinHtgByZone; // minimum discharge flow for heating, Vdz includes secondary and primary flows for dual path
    Array1D<Real64> ZdzClgByZone;    // minimum discharge outdoor-air fraction for cooling
    Array1D<Real64> ZdzHtgByZone;    // minimum discharge outdoor-air fraction for heating
    Array1D<Real64> VpzClgByZone;    // saved value of cooling based ZonePA which is Vpz used in 62.1 tabular report
    Array1D<Real64> VpzMinClgByZone; // saved value of minimum cooling based ZonePA which is VpzClg-min used in 62.1 tabular report
    Array1D<Real64> VpzHtgByZone;    // saved value of heating based ZonePA which is Vpz used in 62.1 tabular report
    Array1D<Real64> VpzMinHtgByZone; // saved value of minimum heating based ZonePA which is VpzHtg-min used in 62.1 tabular report
    Array1D<Real64> VpzClgSumBySys;  // sum of saved value of cooling based ZonePA which is Vpz-sum used in 62.1 tabular report
    Array1D<Real64> VpzHtgSumBySys;  // sum of saved value of heating based ZonePA which is Vpz-sum used in 62.1 tabular report
    Array1D<Real64> PzSumBySys;      // sum of design people for system, Pz_sum
    Array1D<Real64> PsBySys;         // sum of peak concurrent people by system, Ps
    Array1D<Real64> DBySys;          // Population Diversity by system
    Array1D<Real64> SumRpxPzBySys;   // Sum of per person OA times number of people by system, No D yet
    Array1D<Real64> SumRaxAzBySys;   // sum of per area OA time zone area by system, does not get altered by D
    Array1D<std::string> PeakPsOccurrenceDateTimeStringBySys;    // string describing when Ps peak occurs
    Array1D<std::string> PeakPsOccurrenceEnvironmentStringBySys; // string describing Environment when Ps peak occurs
    Array1D<Real64> VouBySys;                                    // uncorrected system outdoor air requirement, for std 62.1 VRP
    Array1D<Real64> VpsClgBySys;                                 // System primary airflow Vps, for cooling for std 62.1 VRP
    Array1D<Real64> VpsHtgBySys;                                 // system primary airflow Vps, for heating for std 62.1 VRP
    Array1D<Real64> FaByZoneHeat;                                // saved value of Fa used in 62.1 tabular report
    Array1D<Real64> FbByZoneCool;                                // saved value of Fb used in 62.1 tabular report
    Array1D<Real64> FbByZoneHeat;                                // saved value of Fb used in 62.1 tabular report
    Array1D<Real64> FcByZoneCool;                                // saved value of Fc used in 62.1 tabular report
    Array1D<Real64> FcByZoneHeat;                                // saved value of Fc used in 62.1 tabular report
    Array1D<Real64> XsBySysCool;                                 // saved value of Xs used in 62.1 tabular report
    Array1D<Real64> XsBySysHeat;                                 // saved value of Xs used in 62.1 tabular report
    Array1D<Real64> EvzByZoneCool;                               // saved value of Evz (zone vent effy) used in 62.1 tabular report
    Array1D<Real64> EvzByZoneHeat;                               // saved value of Evz (zone vent effy) used in 62.1 tabular report
    Array1D<Real64> EvzByZoneCoolPrev;                           // saved value of Evz (zone vent effy) used in 62.1 tabular report
    Array1D<Real64> EvzByZoneHeatPrev;                           // saved value of Evz (zone vent effy) used in 62.1 tabular report
    Array1D<Real64> VotClgBySys;     // saved value of cooling ventilation required at primary AHU, used in 62.1 tabular report
    Array1D<Real64> VotHtgBySys;     // saved value of heating ventilation required at primary AHU, used in 62.1 tabular report
    Array1D<Real64> VozSumClgBySys;  // saved value of cooling ventilation required at clg zones
    Array1D<Real64> VozSumHtgBySys;  // saved value of cooling ventilation required at htg zones
    Array1D<Real64> TotCoolCapTemp;  // scratch variable used for calulating peak load [W]
    Array1D<Real64> EvzMinBySysHeat; // saved value of EvzMin used in 62.1 tabular report
    Array1D<Real64> EvzMinBySysCool; // saved value of EvzMin used in 62.1 tabular report
    Array1D<Real64> FaByZoneCool;    // triggers allocation in UpdateSysSizing
    Array1D<Real64> SensCoolCapTemp; // triggers allocation in UpdateSysSizing

    // Clears the global data in DataSizing.
    // Needed for unit tests, should not be normally called.
    void clear_state()
    {
        NumOARequirements = 0;
        NumZoneAirDistribution = 0;
        NumZoneSizingInput = 0;
        NumSysSizInput = 0;
        NumPltSizInput = 0;
        CurSysNum = 0;
        CurOASysNum = 0;
        CurZoneEqNum = 0;
        CurTermUnitSizingNum = 0;
        CurBranchNum = 0;
        CurDuctType = 0;
        CurLoopNum = 0;
        CurCondLoopNum = 0;
        CurEnvirNumSimDay = 0;
        CurOverallSimDay = 0;
        NumTimeStepsInAvg = 0;
        SaveNumPlantComps = 0;
        SysSizingRunDone = false;
        TermUnitSingDuct = false;
        TermUnitPIU = false;
        TermUnitIU = false;
        ZoneEqFanCoil = false;
        ZoneEqOutdoorAirUnit = false;
        ZoneEqUnitHeater = false;
        ZoneEqUnitVent = false;
        ZoneEqVentedSlab = false;
        ZoneEqDXCoil = false;
        ZoneEqUnitarySys = false;
        ZoneCoolingOnlyFan = false;
        ZoneHeatingOnlyFan = false;
        ZoneSizingRunDone = false;
        DataErrorsFound = false; // used to flag fatal errors in water coils
        AutoVsHardSizingThreshold = 0.1;
        AutoVsHardSizingDeltaTempThreshold = 1.5;

        // Data globals used for sizing
        DataTotCapCurveIndex = 0;
        DataPltSizCoolNum = 0;
        DataPltSizHeatNum = 0;
        DataWaterLoopNum = 0;
        DataCoilNum = 0;
        DataFanOpMode = 0;
        DataCoilIsSuppHeater = false;
        DataIsDXCoil = false;
        DataAutosizable = true;
        DataEMSOverrideON = false;
        DataScalableSizingON = false;
        DataScalableCapSizingON = false;
        DataSysScalableFlowSizingON = false;
        DataSysScalableCapSizingON = false;
        DataDesInletWaterTemp = 0.0;
        DataDesInletAirHumRat = 0.0;
        DataDesInletAirTemp = 0.0;
        DataDesOutletAirTemp = 0.0;
        DataDesOutletAirHumRat = 0.0;
        DataCoolCoilCap = 0.0;
        DataFlowUsedForSizing = 0.0;
        DataAirFlowUsedForSizing = 0.0;
        DataWaterFlowUsedForSizing = 0.0;
        DataCapacityUsedForSizing = 0.0;
        DataDesignCoilCapacity = 0.0;
        DataHeatSizeRatio = 1.0;
        DataEMSOverride = 0.0;
        DataBypassFrac = 0.0;
        DataFracOfAutosizedCoolingAirflow = 1.0;
        DataFracOfAutosizedHeatingAirflow = 1.0;
        DataFlowPerCoolingCapacity = 0.0;
        DataFlowPerHeatingCapacity = 0.0;
        DataFracOfAutosizedCoolingCapacity = 1.0;
        DataFracOfAutosizedHeatingCapacity = 1.0;
        DataAutosizedCoolingCapacity = 0.0;
        DataAutosizedHeatingCapacity = 0.0;
        DataConstantUsedForSizing = 0.0;
        DataFractionUsedForSizing = 0.0;
        DataNonZoneNonAirloopValue = 0.0;
        DataZoneNumber = 0;
        DataFanEnumType = -1;
        DataFanIndex = -1;
        DataFanPlacement = zoneFanPlacement::zoneFanPlaceNotSet;
        DataWaterCoilSizCoolDeltaT = 0.0;
        DataWaterCoilSizHeatDeltaT = 0.0;
        DataNomCapInpMeth = false;
        DataCoilSizingAirInTemp = 0.0;
        DataCoilSizingAirInHumRat = 0.0;
        DataCoilSizingAirOutTemp = 0.0;
        DataCoilSizingAirOutHumRat = 0.0;
        DataCoilSizingFanCoolLoad = 0.0;
        DataCoilSizingCapFT = 1.0;
        DataDesAccountForFanHeat = true;

        NumZoneHVACSizing = 0;
        NumAirTerminalSizingSpec = 0;
        NumAirTerminalUnits = 0;
        DXCoolCap = 0.0;
        GlobalHeatSizingFactor = 0.0;
        GlobalCoolSizingFactor = 0.0;
        SuppHeatCap = 0.0;
        UnitaryHeatCap = 0.0;
        ZoneSizThermSetPtHi.deallocate();
        ZoneSizThermSetPtLo.deallocate();
        CoolPeakDateHrMin.deallocate();
        HeatPeakDateHrMin.deallocate();
        SizingFileColSep = char();

        OARequirements.deallocate();
        ZoneAirDistribution.deallocate();
        ZoneSizingInput.deallocate();
        ZoneSizing.deallocate();
        FinalZoneSizing.deallocate();
        CalcZoneSizing.deallocate();
        CalcFinalZoneSizing.deallocate();
        TermUnitFinalZoneSizing.deallocate();
        SysSizInput.deallocate();
        SysSizing.deallocate();
        FinalSysSizing.deallocate();
        CalcSysSizing.deallocate();
        SysSizPeakDDNum.deallocate();
        TermUnitSizing.deallocate();
        ZoneEqSizing.deallocate();
        UnitarySysEqSizing.deallocate();
        OASysEqSizing.deallocate();
        PlantSizData.deallocate();
        DesDayWeath.deallocate();
        CompDesWaterFlow.deallocate();
        ZoneHVACSizing.deallocate();
        AirTerminalSizingSpec.deallocate();
        DataDesicDehumNum = 0;
        DataDesicRegCoil = false;

        CalcFacilitySizing.deallocate();
        CalcFinalFacilitySizing.DOASHeatAddSeq.deallocate();
        CalcFinalFacilitySizing.DOASLatAddSeq.deallocate();
        CalcFinalFacilitySizing.CoolOutHumRatSeq.deallocate();
        CalcFinalFacilitySizing.CoolOutTempSeq.deallocate();
        CalcFinalFacilitySizing.CoolZoneTempSeq.deallocate();
        CalcFinalFacilitySizing.CoolLoadSeq.deallocate();
        CalcFinalFacilitySizing.HeatOutHumRatSeq.deallocate();
        CalcFinalFacilitySizing.HeatOutTempSeq.deallocate();
        CalcFinalFacilitySizing.HeatZoneTempSeq.deallocate();
        CalcFinalFacilitySizing.HeatLoadSeq.deallocate();

        VbzByZone.deallocate();
        VdzClgByZone.deallocate();
        VdzMinClgByZone.deallocate();
        VdzHtgByZone.deallocate();
        VdzMinHtgByZone.deallocate();
        ZdzClgByZone.deallocate();
        ZdzHtgByZone.deallocate();
        VpzClgByZone.deallocate();
        VpzMinClgByZone.deallocate();
        VpzHtgByZone.deallocate();
        VpzMinHtgByZone.deallocate();
        VpzClgSumBySys.deallocate();
        VpzHtgSumBySys.deallocate();
        PzSumBySys.deallocate();
        PsBySys.deallocate();
        DBySys.deallocate();
        SumRpxPzBySys.deallocate();
        SumRaxAzBySys.deallocate();
        PeakPsOccurrenceDateTimeStringBySys.deallocate();
        PeakPsOccurrenceEnvironmentStringBySys.deallocate();
        VouBySys.deallocate();
        VpsClgBySys.deallocate();
        VpsHtgBySys.deallocate();
        FaByZoneHeat.deallocate();
        FbByZoneCool.deallocate();
        FbByZoneHeat.deallocate();
        FcByZoneCool.deallocate();
        FcByZoneHeat.deallocate();
        XsBySysCool.deallocate();
        XsBySysHeat.deallocate();
        EvzByZoneCool.deallocate();
        EvzByZoneHeat.deallocate();
        EvzByZoneCoolPrev.deallocate();
        EvzByZoneHeatPrev.deallocate();
        VotClgBySys.deallocate();
        VotHtgBySys.deallocate();
        VozSumClgBySys.deallocate();
        VozSumHtgBySys.deallocate();
        TotCoolCapTemp.deallocate();
        EvzMinBySysHeat.deallocate();
        EvzMinBySysCool.deallocate();
        FaByZoneCool.deallocate();
        SensCoolCapTemp.deallocate();
    }

    Real64 TermUnitSizingData::applyTermUnitSizingCoolFlow(Real64 const &coolFlowWithOA, // Cooling flow rate with MinOA limit applied
                                                           Real64 const &coolFlowNoOA    // Cooling flow rate without MinOA limit applied
    )
    {
        // Apply DesignSpecification:AirTerminal:Sizing to cooling flow (could be vol flow or mass flow)
        Real64 coolFlowRatio = 1.0;
        if (this->SpecDesCoolSATRatio > 0.0) {
            coolFlowRatio = this->SpecDesSensCoolingFrac / this->SpecDesCoolSATRatio;
        } else {
            coolFlowRatio = this->SpecDesSensCoolingFrac;
        }
        Real64 adjustedFlow = coolFlowNoOA * coolFlowRatio + (coolFlowWithOA - coolFlowNoOA) * this->SpecMinOAFrac;
        return adjustedFlow;
    }

    Real64 TermUnitSizingData::applyTermUnitSizingHeatFlow(Real64 const &heatFlowWithOA, // Heating flow rate with MinOA limit applied
                                                           Real64 const &heatFlowNoOA    // Heating flow rate without MinOA limit applied
    )
    {
        // Apply DesignSpecification:AirTerminal:Sizing to heating flow (could be vol flow or mass flow)
        Real64 heatFlowRatio = 1.0;
        if (this->SpecDesHeatSATRatio > 0.0) {
            heatFlowRatio = this->SpecDesSensHeatingFrac / this->SpecDesHeatSATRatio;
        } else {
            heatFlowRatio = this->SpecDesSensHeatingFrac;
        }
        Real64 adjustedFlow = heatFlowNoOA * heatFlowRatio + (heatFlowWithOA - heatFlowNoOA) * this->SpecMinOAFrac;
        return adjustedFlow;
    }

    void ZoneSizingData::scaleZoneCooling(Real64 const ratio // Scaling ratio
    )
    {
        // Apply scaling ratio to TermUnitFinalZoneSizing cooling flow and load
        this->DesCoolVolFlow = this->DesCoolVolFlow * ratio;
        this->DesCoolMassFlow = this->DesCoolMassFlow * ratio;
        this->DesCoolLoad = this->DesCoolLoad * ratio;
        this->CoolFlowSeq = this->CoolFlowSeq * ratio;
        this->CoolLoadSeq = this->CoolLoadSeq * ratio;
    }

    void ZoneSizingData::scaleZoneHeating(Real64 const ratio // Scaling ratio
    )
    {
        // Apply scaling ratio to TermUnitFinalZoneSizing heating flow and load
        this->DesHeatVolFlow = this->DesHeatVolFlow * ratio;
        this->DesHeatMassFlow = this->DesHeatMassFlow * ratio;
        this->DesHeatLoad = this->DesHeatLoad * ratio;
        this->HeatFlowSeq = this->HeatFlowSeq * ratio;
        this->HeatLoadSeq = this->HeatLoadSeq * ratio;
    }

    void resetHVACSizingGlobals(int const curZoneEqNum, int const curSysNum, bool &firstPassFlag) // called in zone equipment Report function
    {
        // reset Data globals so that prevoiusly set variables are not used in other equipment models
        DataTotCapCurveIndex = 0;
        DataPltSizCoolNum = 0;
        DataPltSizHeatNum = 0;
        DataWaterLoopNum = 0;
        DataCoilNum = 0;
        DataFanOpMode = 0;
        DataCoilIsSuppHeater = false;
        DataIsDXCoil = false;
        DataAutosizable = true;
        DataEMSOverrideON = false;
        DataScalableSizingON = false;
        DataScalableCapSizingON = false;
        DataSysScalableFlowSizingON = false;
        DataSysScalableCapSizingON = false;

        DataDesInletWaterTemp = 0.0;
        DataDesInletAirHumRat = 0.0;
        DataDesInletAirTemp = 0.0;
        DataDesOutletAirTemp = 0.0;
        DataDesOutletAirHumRat = 0.0;
        DataCoolCoilCap = 0.0;
        DataFlowUsedForSizing = 0.0;
        DataAirFlowUsedForSizing = 0.0;
        DataWaterFlowUsedForSizing = 0.0;
        DataCapacityUsedForSizing = 0.0;
        DataDesignCoilCapacity = 0.0;
        DataHeatSizeRatio = 1.0;
        DataEMSOverride = 0.0;
        DataBypassFrac = 0.0;
        DataFracOfAutosizedCoolingAirflow = 1.0;
        DataFracOfAutosizedHeatingAirflow = 1.0;
        DataFlowPerCoolingCapacity = 0.0;
        DataFlowPerHeatingCapacity = 0.0;
        DataFracOfAutosizedCoolingCapacity = 1.0;
        DataFracOfAutosizedHeatingCapacity = 1.0;
        DataAutosizedCoolingCapacity = 0.0;
        DataAutosizedHeatingCapacity = 0.0;
        DataConstantUsedForSizing = 0.0;
        DataFractionUsedForSizing = 0.0;
        DataNonZoneNonAirloopValue = 0.0;
        DataZoneNumber = 0;
        DataFanEnumType = -1;
        DataFanIndex = -1;
        DataWaterCoilSizCoolDeltaT = 0.0;
        DataWaterCoilSizHeatDeltaT = 0.0;
        DataNomCapInpMeth = false;
        DataFanPlacement = zoneFanPlacement::zoneFanPlaceNotSet;
        DataCoilSizingAirInTemp = 0.0;
        DataCoilSizingAirInHumRat = 0.0;
        DataCoilSizingAirOutTemp = 0.0;
        DataCoilSizingAirOutHumRat = 0.0;

        // These zone specific sizing variables are set in zone equipment to use for sizing.
        // Reset to avoid chance that second zone equipment will size using these variables set by first zone equipment to be sized
        if (curZoneEqNum > 0) {

            if (ZoneEqSizing.size() == 0) {
                firstPassFlag = false;
                return;
            }

            ZoneEqSizing(curZoneEqNum).AirFlow = false;
            ZoneEqSizing(curZoneEqNum).CoolingAirFlow = false;
            ZoneEqSizing(curZoneEqNum).HeatingAirFlow = false;
            ZoneEqSizing(curZoneEqNum).SystemAirFlow = false;
            ZoneEqSizing(curZoneEqNum).Capacity = false;
            ZoneEqSizing(curZoneEqNum).CoolingCapacity = false;
            ZoneEqSizing(curZoneEqNum).HeatingCapacity = false;
            ZoneEqSizing(curZoneEqNum).AirVolFlow = 0.0;
            ZoneEqSizing(curZoneEqNum).MaxHWVolFlow = 0.0;
            ZoneEqSizing(curZoneEqNum).MaxCWVolFlow = 0.0;
            ZoneEqSizing(curZoneEqNum).OAVolFlow = 0.0;
            ZoneEqSizing(curZoneEqNum).DesCoolingLoad = 0.0;
            ZoneEqSizing(curZoneEqNum).DesHeatingLoad = 0.0;
            ZoneEqSizing(curZoneEqNum).CoolingAirVolFlow = 0.0;
            ZoneEqSizing(curZoneEqNum).HeatingAirVolFlow = 0.0;
            ZoneEqSizing(curZoneEqNum).SystemAirVolFlow = 0.0;
            ZoneEqSizing(curZoneEqNum).DesignSizeFromParent = false;
        }

        if (curSysNum > 0) {

            if (UnitarySysEqSizing.size() == 0) {
                firstPassFlag = false;
                return;
            }

            UnitarySysEqSizing(curSysNum).AirFlow = false;
            UnitarySysEqSizing(curSysNum).CoolingAirFlow = false;
            UnitarySysEqSizing(curSysNum).HeatingAirFlow = false;
            UnitarySysEqSizing(curSysNum).Capacity = false;
            UnitarySysEqSizing(curSysNum).CoolingCapacity = false;
            UnitarySysEqSizing(curSysNum).HeatingCapacity = false;
        }

        firstPassFlag = false;
    }

} // namespace DataSizing

} // namespace EnergyPlus
