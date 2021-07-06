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

#ifndef DataSizing_hh_INCLUDED
#define DataSizing_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>
#include <ObjexxFCL/Array2D.hh>

// EnergyPlus Headers
#include <EnergyPlus/Data/BaseData.hh>
#include <EnergyPlus/DataGlobals.hh>
#include <EnergyPlus/EPVector.hh>
#include <EnergyPlus/EnergyPlus.hh>

namespace EnergyPlus {

// Forward declarations
struct EnergyPlusData;

namespace DataSizing {

    // parameters for outside air flow method
    constexpr int NumOAFlowMethods(9);
    constexpr int OAFlowNone(0);
    constexpr int OAFlowPPer(1);
    constexpr int OAFlow(2);
    constexpr int OAFlowPerArea(3);
    constexpr int OAFlowACH(4);
    constexpr int OAFlowSum(5);
    constexpr int OAFlowMax(6);

    extern Array1D_string const cOAFlowMethodTypes;

    // parameters for outside air
    constexpr int AllOA(1);
    constexpr int MinOA(2);

    // parameters for loop fluid type
    constexpr int HeatingLoop(1);
    constexpr int CoolingLoop(2);
    constexpr int CondenserLoop(3);
    constexpr int SteamLoop(4);

    // parameters for sizing
    constexpr int NonCoincident(1);
    constexpr int Coincident(2);

    // parameters for Cooling Peak Load TYpe
    constexpr int SensibleCoolingLoad(1);
    constexpr int TotalCoolingLoad(2);

    // parameters for Central Cooling Capacity Control Method
    constexpr int VAV(1);
    constexpr int Bypass(2);
    constexpr int VT(3);
    constexpr int OnOff(4);

    // parameters for supply air flow rate method
    constexpr int SupplyAirTemperature(1);
    constexpr int TemperatureDifference(2);

    // parameters for sizing
    constexpr int FromDDCalc(1);
    constexpr int InpDesAirFlow(2);
    constexpr int DesAirFlowWithLim(3);

    constexpr int DOANeutralSup(1);
    constexpr int DOANeutralDehumSup(2);
    constexpr int DOACoolSup(3);

    // parameters for Type of Load to Size On
    constexpr int Sensible(0);
    constexpr int Latent(1);
    constexpr int Total(2);
    constexpr int Ventilation(3);

    // parameter for autosize
    constexpr Real64 AutoSize(-99999.0);

    // parameter for (time-of-peak) sizing format
    static constexpr auto PeakHrMinFmt("{:02}:{:02}:00");

    // Zone Outdoor Air Method
    constexpr int ZOAM_FlowPerPerson(1); // set the outdoor air flow rate based on number of people in the zone
    constexpr int ZOAM_FlowPerZone(2);   // sum the outdoor air flow rate per zone based on user input
    constexpr int ZOAM_FlowPerArea(3);   // sum the outdoor air flow rate based on zone area
    constexpr int ZOAM_FlowPerACH(4);    // sum the outdoor air flow rate based on number of air changes for the zone
    constexpr int ZOAM_Sum(5);           // sum the outdoor air flow rate of the people component and the space floor area component
    constexpr int ZOAM_Max(6);           // use the maximum of the outdoor air flow rate of the people component and the space floor area component
    constexpr int ZOAM_IAQP(7);          // Use ASHRAE Standard 62.1-2007 IAQP to calculate the zone level outdoor air flow rates
    constexpr int ZOAM_ProportionalControlSchOcc(8); // Use ASHRAE Standard 62.1-2004 or Trane Engineer's newsletter (volume 34-5)
    // to calculate the zone level outdoor air flow rates based on scheduled occupancy
    constexpr int ZOAM_ProportionalControlDesOcc(9); // Use ASHRAE Standard 62.1-2004 or Trane Engineer's newsletter (volume 34-5)
    // to calculate the zone level outdoor air flow rates based on design occupancy

    // System Outdoor Air Method
    constexpr int SOAM_ZoneSum(1); // Sum the outdoor air flow rates of all zones
    constexpr int SOAM_VRP(2);     // Use ASHRAE Standard 62.1-2007 to calculate the system level outdoor air flow rates
    //  considering the zone air distribution effectiveness and the system ventilation efficiency
    constexpr int SOAM_IAQP(3); // Use ASHRAE Standard 62.1-2007 IAQP to calculate the system level outdoor air flow rates
    // based on the CO2 setpoint
    constexpr int SOAM_ProportionalControlSchOcc(4); // Use ASHRAE Standard 62.1-2004 or Trane Engineer's newsletter (volume 34-5)
    // to calculate the system level outdoor air flow rates based on scheduled occupancy
    constexpr int SOAM_IAQPGC(5); // Use ASHRAE Standard 62.1-2004 IAQP to calculate the system level outdoor air flow rates
    // based on the generic contaminant setpoint
    constexpr int SOAM_IAQPCOM(6); // Take the maximum outdoor air rate from both CO2 and generic contaminant controls
    // based on the generic contaminant setpoint
    constexpr int SOAM_ProportionalControlDesOcc(7); // Use ASHRAE Standard 62.1-2004 or Trane Engineer's newsletter (volume 34-5)
    // to calculate the system level outdoor air flow rates based on design occupancy
    constexpr int SOAM_ProportionalControlDesOARate(8); // Calculate the system level outdoor air flow rates based on design OA rate

    // Zone HVAC Equipment Supply Air Sizing Option
    constexpr int None(1);
    constexpr int SupplyAirFlowRate(2);
    constexpr int FlowPerFloorArea(3);
    constexpr int FractionOfAutosizedCoolingAirflow(4);
    constexpr int FractionOfAutosizedHeatingAirflow(5);
    constexpr int FlowPerCoolingCapacity(6);
    constexpr int FlowPerHeatingCapacity(7);

    constexpr int CoolingDesignCapacity(8);
    constexpr int HeatingDesignCapacity(9);
    constexpr int CapacityPerFloorArea(10);
    constexpr int FractionOfAutosizedCoolingCapacity(11);
    constexpr int FractionOfAutosizedHeatingCapacity(12);

    constexpr int NoSizingFactorMode(101);
    constexpr int GlobalHeatingSizingFactorMode(102);
    constexpr int GlobalCoolingSizingFactorMode(103);
    constexpr int LoopComponentSizingFactorMode(104);

    enum class zoneFanPlacement
    {
        zoneFanPlaceNotSet,
        zoneBlowThru,
        zoneDrawThru
    };

    // Types

    struct ZoneSizingInputData
    {
        // Members
        std::string ZoneName;  // name of a zone
        int ZoneNum;           // index of the zone
        int ZnCoolDgnSAMethod; // choice of how to get zone cooling design air temperature;
        //  1 = specify supply air temperature,
        //  2 = calculate from the temperature difference
        int ZnHeatDgnSAMethod; // choice of how to get zone heating design air temperature;
        //  1 = specify supply air temperature,
        //  2 = calculate from the temperature difference
        Real64 CoolDesTemp;              // zone design cooling supply air temperature [C]
        Real64 HeatDesTemp;              // zone design heating supply air temperature [C]
        Real64 CoolDesTempDiff;          // zone design cooling supply air temperature difference [deltaC]
        Real64 HeatDesTempDiff;          // zone design heating supply air temperature difference [deltaC]
        Real64 CoolDesHumRat;            // zone design cooling supply air humidity ratio [kgWater/kgDryAir]
        Real64 HeatDesHumRat;            // zone design heating supply air humidity ratio [kgWater/kgDryAir]
        std::string DesignSpecOAObjName; // name of the design specification outdoor air object
        int OADesMethod;                 // choice of how to calculate minimum outside air;
        //  1 = m3/s per person; 2 = m3/s per zone; 3 = m3/s per zone area;
        //  4 = sum of flow from 3 OA input fields;
        //  5 = max of flow from 3 OA input fields
        Real64 DesOAFlowPPer;    // design outside air flow per person in zone [m3/s]
        Real64 DesOAFlowPerArea; // design outside air flow per zone area [m3/s / m2]
        Real64 DesOAFlow;        // design outside air flow for the zone [m3/s]
        int CoolAirDesMethod;    // choice of how to get zone cooling design air flow rates;
        //  1 = calc from des day simulation; 2 = m3/s per zone, user input
        //  3 = apply limits to air flow rate from DD calc
        Real64 DesCoolAirFlow;           // design zone supply air flow rate [m3/s]
        Real64 DesCoolMinAirFlowPerArea; // design cooling minimum air flow rate per zone area [m3/s / m2]
        Real64 DesCoolMinAirFlow;        // design cooling minimum air flow rate [m3/s]
        Real64 DesCoolMinAirFlowFrac;    // design cooling minimum air flow rate fraction
        //  (of the cooling design air flow rate)
        int HeatAirDesMethod; // choice of how to get zone heating design air flow rates;
        //  1 = calc from des day simulation; 2 = m3/s per zone, user input
        //  3 = apply limits to air flow rate from DD calc
        Real64 DesHeatAirFlow;           // design zone heating supply air flow rate [m3/s]
        Real64 DesHeatMaxAirFlowPerArea; // design heating maximum air flow rate per zone area [m3/s / m2]
        Real64 DesHeatMaxAirFlow;        // design heating maximum air flow rate [m3/s]
        Real64 DesHeatMaxAirFlowFrac;    // design heating maximum air flow rate fraction
        //  (of the cooling design air flow rate)
        Real64 HeatSizingFactor; // the zone heating sizing ratio
        Real64 CoolSizingFactor; // the zone cooling sizing ratio
        Real64 ZoneADEffCooling;
        Real64 ZoneADEffHeating;
        std::string ZoneAirDistEffObjName; // name of the zone air distribution effectiveness object name
        int ZoneAirDistributionIndex;      // index to the zone air distribution object
        int ZoneDesignSpecOAIndex;         // index to the zone design spec OA object
        Real64 ZoneSecondaryRecirculation; // the zone secondary air recirculation fraction
        Real64 ZoneVentilationEff;         // zone ventilation efficiency
        bool AccountForDOAS;               // False: do nothing; True: calculate the effect of a DOA system on the zone sizing arrays
        int DOASControlStrategy;           // 1=supply neutral ventilation air; 2=supply neutral dehumidified ventilation air;
        // 3=supply cold ventilation air
        Real64 DOASLowSetpoint;  // Dedicated Outside Air Low Setpoint for Design [C]
        Real64 DOASHighSetpoint; // Dedicated Outside Air High Setpoint for Design [C]

        // Default Constructor
        ZoneSizingInputData()
            : ZoneNum(0), ZnCoolDgnSAMethod(0), ZnHeatDgnSAMethod(0), CoolDesTemp(0.0), HeatDesTemp(0.0), CoolDesTempDiff(0.0), HeatDesTempDiff(0.0),
              CoolDesHumRat(0.0), HeatDesHumRat(0.0), OADesMethod(0), DesOAFlowPPer(0.0), DesOAFlowPerArea(0.0), DesOAFlow(0.0), CoolAirDesMethod(0),
              DesCoolAirFlow(0.0), DesCoolMinAirFlowPerArea(0.0), DesCoolMinAirFlow(0.0), DesCoolMinAirFlowFrac(0.0), HeatAirDesMethod(0),
              DesHeatAirFlow(0.0), DesHeatMaxAirFlowPerArea(0.0), DesHeatMaxAirFlow(0.0), DesHeatMaxAirFlowFrac(0.0), HeatSizingFactor(0.0),
              CoolSizingFactor(0.0), ZoneADEffCooling(1.0), ZoneADEffHeating(1.0), ZoneAirDistributionIndex(0), ZoneDesignSpecOAIndex(0),
              ZoneSecondaryRecirculation(0.0), ZoneVentilationEff(0.0), AccountForDOAS(false), DOASControlStrategy(0), DOASLowSetpoint(0.0),
              DOASHighSetpoint(0.0)
        {
        }
    };

    struct ZoneSizingData
    {
        // Members
        std::string ZoneName;   // name of a zone
        std::string ADUName;    // Terminal Unit Name (air distribution unit or direct air unit) - only assigned for TermUnitFinalZoneSizing
        std::string CoolDesDay; // name of a cooling design day
        std::string HeatDesDay; // name of a heating design day
        int ZnCoolDgnSAMethod;  // choice of how to get zone cooling design air temperature;
        //  1 = specify supply air temperature,
        //  2 = calculate from the temperature difference
        int ZnHeatDgnSAMethod; // choice of how to get zone heating design air temperature;
        //  1 = specify supply air temperature,
        //  2 = calculate from the temperature difference
        Real64 CoolDesTemp;           // zone design cooling supply air temperature [C]
        Real64 HeatDesTemp;           // zone design heating supply air temperature [C]
        Real64 CoolDesTempDiff;       // zone design cooling supply air temperature difference [deltaC]
        Real64 HeatDesTempDiff;       // zone design heating supply air temperature difference [deltaC]
        Real64 CoolDesHumRat;         // zone design cooling supply air humidity ratio [kgWater/kgDryAir]
        Real64 HeatDesHumRat;         // zone design heating supply air humidity ratio [kgWater/kgDryAir]
        int ZoneAirDistributionIndex; // index to DesignSpecification:ZoneAirDistribution object
        int ZoneDesignSpecOAIndex;    // index to DesignSpecification:OutdoorAir object

        int OADesMethod; // choice of how to calculate minimum outside air;
        //  1 = m3/s per person; 2 = m3/s per zone; 3 = m3/s per zone area;
        //  4 = sum of flow from 3 OA input fields;
        //  5 = max of flow from 3 OA input fields
        Real64 DesOAFlowPPer;    // design outside air flow per person in zone [m3/s]
        Real64 DesOAFlowPerArea; // design outside air flow per zone area [m3/s / m2]
        Real64 DesOAFlow;        // design outside air flow for the zone [m3/s]
        int CoolAirDesMethod;    // choice of how to get zone cooling design air flow rates;
        //  1 = calc from des day simulation; 2 = m3/s per zone, user input
        //  3 = apply limits to air flow rate from DD calc
        Real64 InpDesCoolAirFlow;        // design zone supply air flow rate [m3/s]
        Real64 DesCoolMinAirFlowPerArea; // design cooling minimum air flow rate per zone area [m3/s / m2]
        Real64 DesCoolMinAirFlow;        // design cooling minimum air flow rate [m3/s]
        Real64 DesCoolMinAirFlowFrac;    // design cooling minimum air flow rate fraction
        //  (of the cooling design air flow rate)
        int HeatAirDesMethod; // choice of how to get zone heating design air flow rates;
        //  1 = calc from des day simulation; 2 = m3/s per zone, user input
        //  3 = apply limits to air flow rate from DD calc
        Real64 InpDesHeatAirFlow;        // design zone heating supply air flow rate [m3/s]
        Real64 DesHeatMaxAirFlowPerArea; // design heating maximum air flow rate per zone area [m3/s / m2]
        Real64 DesHeatMaxAirFlow;        // design heating maximum air flow rate [m3/s]
        Real64 DesHeatMaxAirFlowFrac;    // design heating maximum air flow rate fraction
        //  (of the cooling design air flow rate)
        Real64 HeatSizingFactor; // the zone heating sizing ratio
        Real64 CoolSizingFactor; // the zone cooling sizing ratio
        bool AccountForDOAS;     // False: do nothing; True: calculate the effect of a DOA system on the zone sizing arrays
        int DOASControlStrategy; // 1=supply neutral ventilation air; 2=supply neutral dehumidified ventilation air;
        // 3=supply cold ventilation air
        Real64 DOASLowSetpoint;         // Dedicated Outside Air Low Setpoint for Design [C]
        Real64 DOASHighSetpoint;        // Dedicated Outside Air High Setpoint for Design [C]
        int ActualZoneNum;              // index into the Zone data array (in DataHeatBalance)
        Real64 DesHeatMassFlow;         // zone design heating air mass flow rate [kg/s]
        Real64 DesHeatMassFlowNoOA;     // zone design heating air mass flow rate without applying MinOA as a limit [kg/s]
        Real64 DesHeatOAFlowFrac;       // zone design heating OA air volume fraction [-]
        bool EMSOverrideDesHeatMassOn;  // true if EMS is acting on this structure
        Real64 EMSValueDesHeatMassFlow; // Value EMS directing to use for Design Heating air mass flow [kg/s]
        Real64 DesCoolMassFlow;         // zone design cooling air mass flow rate [kg/s]
        Real64 DesCoolMassFlowNoOA;     // zone design cooling air mass flow rate without applying MinOA as a limit [kg/s]
        Real64 DesCoolOAFlowFrac;       // zone design cooling OA air volume fraction [-]
        bool EMSOverrideDesCoolMassOn;  // true if EMS is acting on this structure
        Real64 EMSValueDesCoolMassFlow; // Value EMS directing to use for Design Cooling air mass flow [kg/s]
        Real64 DesHeatLoad;             // zone design heating load including sizing factor and scaled to match airflow sizing [W]
        Real64 NonAirSysDesHeatLoad;    // base zone design heating load including sizing factor [W]
        bool EMSOverrideDesHeatLoadOn;  // true if EMS is acting on this structure
        Real64 EMSValueDesHeatLoad;     // Value EMS directing to use for zone design heating load  [W]
        Real64 DesCoolLoad;             // zone design cooling load including sizing factor and scaled to match airflow sizing [W]
        Real64 NonAirSysDesCoolLoad;    // base zone design cooling load including sizing factor [W]
        bool EMSOverrideDesCoolLoadOn;  // true if EMS is acting on this structure
        Real64 EMSValueDesCoolLoad;     // Value EMS directing to use for zone design cooling load  [W]
        Real64 DesHeatDens;             // zone design heating air density [kg/m3]
        Real64 DesCoolDens;             // zone design cooling air density [kg/m3]
        Real64 DesHeatVolFlow;          // zone design heating air volume flow rate including sizing factor and scaled to match airflow sizing [m3/s]
        Real64 DesHeatVolFlowNoOA;      // zone design heating air volume flow rate including sizing factor and scaled to match airflow sizing without
                                        // MinOA limit [m3/s]
        Real64 NonAirSysDesHeatVolFlow; // base zone design heating air volume flow rate including sizing factor [m3/s]
        bool EMSOverrideDesHeatVolOn;   // true if EMS is acting on this structure
        Real64 EMSValueDesHeatVolFlow;  // Value EMS directing to use for Design Heating air volume flow [m3/s]
        Real64 DesCoolVolFlow;          // zone design cooling air volume flow rate [m3/s]
        Real64 DesCoolVolFlowNoOA;      // zone design cooling air volume flow rate without applying MinOA as a limit [m3/s]
        Real64 NonAirSysDesCoolVolFlow; // base zone design cooling air volume flow rate including sizing factor [m3/s]
        bool EMSOverrideDesCoolVolOn;   // true if EMS is acting on this structure
        Real64 EMSValueDesCoolVolFlow;  // Value EMS directing to use for Design cooling air volume flow [m3/s]
        Real64 DesHeatVolFlowMax;       // zone design heating maximum air volume flow rate [m3/s]
        Real64 DesCoolVolFlowMin;       // zone design cooling minimum air volume flow rate [m3/s]
        Real64 DesHeatCoilInTemp;       // zone heating coil design air inlet temperature [C]
        Real64 DesCoolCoilInTemp;       // zone cooling coil design air inlet temperature [C]
        Real64 DesHeatCoilInHumRat;     // zone heating coil design air inlet humidity ratio [kg/kg]
        Real64 DesCoolCoilInHumRat;     // zone cooling coil design air inlet humidity ratio [kg/kg]
        Real64 DesHeatCoilInTempTU;     // zone heating coil design air inlet temperature (supply air)([C]
        Real64 DesCoolCoilInTempTU;     // zone cooling coil design air inlet temperature (supply air)[C]
        Real64 DesHeatCoilInHumRatTU;   // zone heating coil design air inlet humidity ratio
        //  (supply air) [kg/kg]
        Real64 DesCoolCoilInHumRatTU; // zone cooling coil design air inlet humidity ratio
        //  (supply air) [kg/kg]
        Real64 HeatMassFlow;          // current zone heating air mass flow rate (HVAC time step)
        Real64 CoolMassFlow;          // current zone cooling air mass flow rate (HVAC time step)
        Real64 HeatLoad;              // current zone heating load (HVAC time step)
        Real64 CoolLoad;              // current zone heating load (HVAC time step)
        Real64 HeatZoneTemp;          // current zone temperature (heating, time step)
        Real64 HeatOutTemp;           // current outdoor temperature (heating, time step)
        Real64 HeatZoneRetTemp;       // current zone return temperature (heating, time step)
        Real64 HeatTstatTemp;         // current zone thermostat temperature (heating, time step)
        Real64 CoolZoneTemp;          // current zone temperature (cooling, time step)
        Real64 CoolOutTemp;           // current Outdoor temperature (cooling, time step)
        Real64 CoolZoneRetTemp;       // current zone return temperature (cooling, time step)
        Real64 CoolTstatTemp;         // current zone thermostat temperature (cooling, time step)
        Real64 HeatZoneHumRat;        // current zone humidity ratio (heating, time step)
        Real64 CoolZoneHumRat;        // current zone humidity ratio (cooling, time step)
        Real64 HeatOutHumRat;         // current outdoor humidity ratio (heating, time step)
        Real64 CoolOutHumRat;         // current outdoor humidity ratio (cooling, time step)
        Real64 ZoneTempAtHeatPeak;    // zone temp at max heating [C]
        Real64 ZoneRetTempAtHeatPeak; // zone return temp at max heating [C]
        Real64 OutTempAtHeatPeak;     // outdoor temperature at max heating [C]
        Real64 ZoneTempAtCoolPeak;    // zone temp at max cooling [C]
        Real64 ZoneRetTempAtCoolPeak; // zone return temp at max cooling [C]
        Real64 OutTempAtCoolPeak;     // outdoor temperature at max cooling [C]
        Real64 ZoneHumRatAtHeatPeak;  // zone humidity ratio at max heating [kg/kg]
        Real64 ZoneHumRatAtCoolPeak;  // zone humidity ratio at max cooling [kg/kg]
        Real64 OutHumRatAtHeatPeak;   // outdoor humidity at max heating [kg/kg]
        Real64 OutHumRatAtCoolPeak;   // outdoor humidity at max cooling [kg/kg]
        int TimeStepNumAtHeatMax;     // time step number (in day) at Heating peak
        int TimeStepNumAtCoolMax;     // time step number (in day) at cooling peak
        int HeatDDNum;                // design day index of design day causing heating peak
        int CoolDDNum;                // design day index of design day causing cooling peak
        std::string cHeatDDDate;      // date of design day causing heating peak
        std::string cCoolDDDate;      // date of design day causing cooling peak
        Real64 MinOA;                 // design minimum outside air in m3/s
        Real64 DesCoolMinAirFlow2;    // design cooling minimum air flow rate [m3/s] derived from
        //  DesCoolMinAirFlowPerArea
        Real64 DesHeatMaxAirFlow2; // design heating maximum air flow rate [m3/s] derived from
        //  DesHeatMaxAirFlowPerArea
        Array1D<Real64> HeatFlowSeq;        // daily sequence of zone heating air mass flow rate (zone time step) [kg/s]
        Array1D<Real64> HeatFlowSeqNoOA;    // daily sequence of zone heating air mass flow rate (zone time step) without MinOA limit [kg/s]
        Array1D<Real64> CoolFlowSeq;        // daily sequence of zone cooling air mass flow rate (zone time step) [kg/s]
        Array1D<Real64> CoolFlowSeqNoOA;    // daily sequence of zone cooling air mass flow rate (zone time step) without MinOA limit [kg/s]
        Array1D<Real64> HeatLoadSeq;        // daily sequence of zone heating load (zone time step)
        Array1D<Real64> CoolLoadSeq;        // daily sequence of zone cooling load (zone time step)
        Array1D<Real64> HeatZoneTempSeq;    // daily sequence of zone temperatures (heating, zone time step)
        Array1D<Real64> HeatOutTempSeq;     // daily sequence of outdoor temperatures (heating, zone time step)
        Array1D<Real64> HeatZoneRetTempSeq; // daily sequence of zone return temperatures (heating,
        //  zone time step)
        Array1D<Real64> HeatTstatTempSeq;   // daily sequence of zone thermostat temperatures (heating, zone time step)
        Array1D<Real64> DesHeatSetPtSeq;    // daily sequence of indoor set point temperatures (zone time step)
        Array1D<Real64> CoolZoneTempSeq;    // daily sequence of zone temperatures (cooling, zone time step)
        Array1D<Real64> CoolOutTempSeq;     // daily sequence of outdoor temperatures (cooling, zone time step)
        Array1D<Real64> CoolZoneRetTempSeq; // daily sequence of zone return temperatures (cooling,
        //  zone time step)
        Array1D<Real64> CoolTstatTempSeq;  // daily sequence of zone thermostat temperatures (cooling, zone time step)
        Array1D<Real64> DesCoolSetPtSeq;   // daily sequence of indoor set point temperatures (zone time step)
        Array1D<Real64> HeatZoneHumRatSeq; // daily sequence of zone humidity ratios (heating, zone time step)
        Array1D<Real64> CoolZoneHumRatSeq; // daily sequence of zone humidity ratios (cooling, zone time step)
        Array1D<Real64> HeatOutHumRatSeq;  // daily sequence of outdoor humidity ratios (heating, zone time step)
        Array1D<Real64> CoolOutHumRatSeq;  // daily sequence of outdoor humidity ratios (cooling, zone time step)
        Real64 ZoneADEffCooling;           // the zone air distribution effectiveness in cooling mode
        Real64 ZoneADEffHeating;           // the zone air distribution effectiveness in heating mode
        Real64 ZoneSecondaryRecirculation; // the zone secondary air recirculation fraction
        Real64 ZoneVentilationEff;         // zone ventilation efficiency
        Real64 ZonePrimaryAirFraction;     // the zone primary air fraction for cooling based calculations
        Real64 ZonePrimaryAirFractionHtg;  // the zone primary air fraction for heating based calculations
        Real64 ZoneOAFracCooling;          // OA fraction in cooling mode
        Real64 ZoneOAFracHeating;          // OA fraction in heating mode
        Real64 TotalOAFromPeople;          // Zone OA required due to people
        Real64 TotalOAFromArea;            // Zone OA required based on floor area
        Real64 TotPeopleInZone;            // total number of people in the zone
        Real64 TotalZoneFloorArea;         // total zone floor area
        Real64 ZonePeakOccupancy;          // zone peak occupancy based on max schedule value
        Real64 SupplyAirAdjustFactor;      // supply air adjustment factor for next time step if OA is capped
        Real64 ZpzClgByZone;               // OA Std 62.1 required fraction in cooling mode ? should this be ZdzClgByZone
        Real64 ZpzHtgByZone;               // OA Std 62.1 required fraction in heating mode ? should this be ZdzHtgByZone
        Real64 VozClgByZone;    // value of required cooling vent to zone, used in 62.1 tabular report, already includes people diversity term
        Real64 VozHtgByZone;    // value of required heating vent to zone, used in 62.1 tabular report, already includes people diversity term
        Real64 DOASHeatLoad;    // current heating load from DOAS supply air [W]
        Real64 DOASCoolLoad;    // current cooling load from DOAS supply air [W]
        Real64 DOASHeatAdd;     // current heat addition rate from DOAS supply air [W]
        Real64 DOASLatAdd;      // current latent heat addition rate from DOAS supply air [W]
        Real64 DOASSupMassFlow; // current mass flow rate of DOAS supply air [kg/s]
        Real64 DOASSupTemp;     // current DOAS supply air temperature [C]
        Real64 DOASSupHumRat;   // current DOAS supply air humidity ratio [kgWater/kgDryAir]
        Real64 DOASTotCoolLoad; // current total cooling load imposed by DOAS supply air [W]
        Array1D<Real64> DOASHeatLoadSeq;    // daily sequence of zone DOAS heating load (zone time step) [W]
        Array1D<Real64> DOASCoolLoadSeq;    // daily sequence of zone DOAS cooling load (zone time step) [W]
        Array1D<Real64> DOASHeatAddSeq;     // daily sequence of zone DOAS heat addition rate (zone time step) [W]
        Array1D<Real64> DOASLatAddSeq;      // daily sequence of zone DOAS latent heat addition rate (zone time step) [W]
        Array1D<Real64> DOASSupMassFlowSeq; // daily sequence of zone DOAS supply mass flow rate (zone time step) [Kg/s]
        Array1D<Real64> DOASSupTempSeq;     // daily sequence of zone DOAS supply temperature (zone time step) [C]
        Array1D<Real64> DOASSupHumRatSeq;   // daily sequence of zone DOAS supply humidity ratio (zone time step) [kgWater/kgDryAir]
        Array1D<Real64> DOASTotCoolLoadSeq; // daily sequence of zone DOAS total cooling load (zone time step) [W]

        // Default Constructor
        ZoneSizingData()
            : ZnCoolDgnSAMethod(0), ZnHeatDgnSAMethod(0), CoolDesTemp(0.0), HeatDesTemp(0.0), CoolDesTempDiff(0.0), HeatDesTempDiff(0.0),
              CoolDesHumRat(0.0), HeatDesHumRat(0.0), ZoneAirDistributionIndex(0), ZoneDesignSpecOAIndex(0), OADesMethod(0), DesOAFlowPPer(0.0),
              DesOAFlowPerArea(0.0), DesOAFlow(0.0), CoolAirDesMethod(0), InpDesCoolAirFlow(0.0), DesCoolMinAirFlowPerArea(0.0),
              DesCoolMinAirFlow(0.0), DesCoolMinAirFlowFrac(0.0), HeatAirDesMethod(0), InpDesHeatAirFlow(0.0), DesHeatMaxAirFlowPerArea(0.0),
              DesHeatMaxAirFlow(0.0), DesHeatMaxAirFlowFrac(0.0), HeatSizingFactor(0.0), CoolSizingFactor(0.0), AccountForDOAS(false),
              DOASControlStrategy(0), DOASLowSetpoint(0.0), DOASHighSetpoint(0.0), ActualZoneNum(0), DesHeatMassFlow(0.0), DesHeatMassFlowNoOA(0.0),
              DesHeatOAFlowFrac(0.0), EMSOverrideDesHeatMassOn(false), EMSValueDesHeatMassFlow(0.0), DesCoolMassFlow(0.0), DesCoolMassFlowNoOA(0.0),
              DesCoolOAFlowFrac(0.0), EMSOverrideDesCoolMassOn(false), EMSValueDesCoolMassFlow(0.0), DesHeatLoad(0.0), NonAirSysDesHeatLoad(0.0),
              EMSOverrideDesHeatLoadOn(false), EMSValueDesHeatLoad(0.0), DesCoolLoad(0.0), NonAirSysDesCoolLoad(0.0), EMSOverrideDesCoolLoadOn(false),
              EMSValueDesCoolLoad(0.0), DesHeatDens(0.0), DesCoolDens(0.0), DesHeatVolFlow(0.0), DesHeatVolFlowNoOA(0.0),
              NonAirSysDesHeatVolFlow(0.0), EMSOverrideDesHeatVolOn(false), EMSValueDesHeatVolFlow(0.0), DesCoolVolFlow(0.0), DesCoolVolFlowNoOA(0.0),
              NonAirSysDesCoolVolFlow(0.0), EMSOverrideDesCoolVolOn(false), EMSValueDesCoolVolFlow(0.0), DesHeatVolFlowMax(0.0),
              DesCoolVolFlowMin(0.0), DesHeatCoilInTemp(0.0), DesCoolCoilInTemp(0.0), DesHeatCoilInHumRat(0.0), DesCoolCoilInHumRat(0.0),
              DesHeatCoilInTempTU(0.0), DesCoolCoilInTempTU(0.0), DesHeatCoilInHumRatTU(0.0), DesCoolCoilInHumRatTU(0.0), HeatMassFlow(0.0),
              CoolMassFlow(0.0), HeatLoad(0.0), CoolLoad(0.0), HeatZoneTemp(0.0), HeatOutTemp(0.0), HeatZoneRetTemp(0.0), HeatTstatTemp(0.0),
              CoolZoneTemp(0.0), CoolOutTemp(0.0), CoolZoneRetTemp(0.0), CoolTstatTemp(0.0), HeatZoneHumRat(0.0), CoolZoneHumRat(0.0),
              HeatOutHumRat(0.0), CoolOutHumRat(0.0), ZoneTempAtHeatPeak(0.0), ZoneRetTempAtHeatPeak(0.0), OutTempAtHeatPeak(0.0),
              ZoneTempAtCoolPeak(0.0), ZoneRetTempAtCoolPeak(0.0), OutTempAtCoolPeak(0.0), ZoneHumRatAtHeatPeak(0.0), ZoneHumRatAtCoolPeak(0.0),
              OutHumRatAtHeatPeak(0.0), OutHumRatAtCoolPeak(0.0), TimeStepNumAtHeatMax(0), TimeStepNumAtCoolMax(0), HeatDDNum(0), CoolDDNum(0),
              MinOA(0.0), DesCoolMinAirFlow2(0.0), DesHeatMaxAirFlow2(0.0), ZoneADEffCooling(1.0), ZoneADEffHeating(1.0),
              ZoneSecondaryRecirculation(0.0), ZoneVentilationEff(0.0), ZonePrimaryAirFraction(0.0), ZonePrimaryAirFractionHtg(0.0),
              ZoneOAFracCooling(0.0), ZoneOAFracHeating(0.0), TotalOAFromPeople(0.0), TotalOAFromArea(0.0), TotPeopleInZone(0.0),
              TotalZoneFloorArea(0.0), ZonePeakOccupancy(0.0), SupplyAirAdjustFactor(1.0), ZpzClgByZone(0.0), ZpzHtgByZone(0.0), VozClgByZone(0.0),
              VozHtgByZone(0.0), DOASHeatLoad(0.0), DOASCoolLoad(0.0), DOASHeatAdd(0.0), DOASLatAdd(0.0), DOASSupMassFlow(0.0), DOASSupTemp(0.0),
              DOASSupHumRat(0.0), DOASTotCoolLoad(0.0)
        {
        }

        void scaleZoneCooling(Real64 ratio // Scaling ratio
        );
        void scaleZoneHeating(Real64 ratio // Scaling ratio
        );
        void zeroMemberData();
        void allocateMemberArrays(int numOfTimeStepInDay);
    };

    struct TermUnitSizingData
    {
        // Members
        int CtrlZoneNum;               // Controlled zone number (index to FinalZoneSizing, etc.)
        std::string ADUName;           // Terminal Unit Name (air distribution unit or direct air unit)
        Real64 AirVolFlow;             // design air vol flow rate for single duct terminal unit [m3/s]
        Real64 MaxHWVolFlow;           // design Hot Water vol flow for single duct terminal unit [m3/s]
        Real64 MaxSTVolFlow;           // design Steam vol flow rate for single duct terminal unit [m3/s]
        Real64 MaxCWVolFlow;           // design Cold Water vol flow for single duct terminal unit [m3/s]
        Real64 MinFlowFrac;            // design minimum flow fraction for a terminal unit
        Real64 InducRat;               // design induction ratio for a terminal unit
        bool InducesPlenumAir;         // True if secondary air comes from the plenum
        Real64 ReheatAirFlowMult;      // multiplier for air flow in reheat coil UA calculation
        Real64 ReheatLoadMult;         // multiplier for load in reheat coil UA calculation
        Real64 DesCoolingLoad;         // design cooling load used for zone equipment [W]
        Real64 DesHeatingLoad;         // design heating load used for zone equipment [W]
        Real64 SpecDesSensCoolingFrac; // Fraction of Design Sensible Cooling Load from DesignSpecification:AirTerminal:Sizing
        Real64 SpecDesCoolSATRatio;    // Cooling Design Supply Air Temperature Difference Ratio from DesignSpecification:AirTerminal:Sizing
        Real64 SpecDesSensHeatingFrac; // Fraction of Design Sensible Heating Load from DesignSpecification:AirTerminal:Sizing
        Real64 SpecDesHeatSATRatio;    // Heating Design Supply Air Temperature Difference Ratio from DesignSpecification:AirTerminal:Sizing
        Real64 SpecMinOAFrac;          // Fraction of Minimum Outdoor Air Flow from DesignSpecification:AirTerminal:Sizing

        // Default Constructor
        TermUnitSizingData()
            : CtrlZoneNum(0), AirVolFlow(0.0), MaxHWVolFlow(0.0), MaxSTVolFlow(0.0), MaxCWVolFlow(0.0), MinFlowFrac(0.0), InducRat(0.0),
              InducesPlenumAir(false), ReheatAirFlowMult(1.0), ReheatLoadMult(1.0), DesCoolingLoad(0.0), DesHeatingLoad(0.0),
              SpecDesSensCoolingFrac(1.0), SpecDesCoolSATRatio(1.0), SpecDesSensHeatingFrac(1.0), SpecDesHeatSATRatio(1.0), SpecMinOAFrac(1.0)
        {
        }

        Real64 applyTermUnitSizingCoolFlow(Real64 const &coolFlowWithOA, // Cooling flow rate with MinOA limit applied
                                           Real64 const &coolFlowNoOA    // Cooling flow rate without MinOA limit applied
        );

        Real64 applyTermUnitSizingHeatFlow(Real64 const &heatFlowWithOA, // Heating flow rate with MinOA limit applied
                                           Real64 const &heatFlowNoOA    // Heating flow rate without MinOA limit applied
        );
    };

    struct ZoneEqSizingData // data saved from zone eq component sizing and passed to subcomponents
    {
        // Members
        Real64 AirVolFlow;            // design air vol flow rate for zone equipment unit [m3/s]
        Real64 MaxHWVolFlow;          // design Hot Water vol flow for zone equipment unit [m3/s]
        Real64 MaxCWVolFlow;          // design Cold Water vol flow for zone equipment unit [m3/s]
        Real64 OAVolFlow;             // design outside air flow for zone equipment unit [m3/s]
        Real64 ATMixerVolFlow;        // design ventilation air flow rate from air terminal mixer (central DOAS) [m3/s]
        Real64 ATMixerCoolPriDryBulb; // design ventilation drybulb temperature from air terminal mixer during cooling (central DOAS) [C]
        Real64 ATMixerCoolPriHumRat;  // design ventilation humidity ratio from air terminal mixer during cooling (central DOAS) [kgWater/kgDryAir]
        Real64 ATMixerHeatPriDryBulb; // design ventilation drybulb temperature from air terminal mixer during heating (central DOAS) [C]
        Real64 ATMixerHeatPriHumRat;  // design ventilation humidity ratio from air terminal mixer during heating (central DOAS) [kgWater/kgDryAir]
        Real64 DesCoolingLoad;        // design cooling load used for zone equipment [W]
        Real64 DesHeatingLoad;        // design heating load used for zone equipment [W]
        Real64 CoolingAirVolFlow;     // design cooling air vol flow rate for equipment[m3/s]
        Real64 HeatingAirVolFlow;     // design heating air vol flow rate for equipment[m3/s]
        Real64 SystemAirVolFlow;      // design heating air vol flow rate for equipment[m3/s]
        bool AirFlow;                 // TRUE if AirloopHVAC system air flow rate is calculated
        bool CoolingAirFlow;          // TRUE if AirloopHVAC system cooling air flow rate is calculated
        bool HeatingAirFlow;          // TRUE if AirloopHVAC system heating air flow rate is calculated
        bool SystemAirFlow;           // TRUE if AirloopHVAC system heating air flow rate is calculated
        bool Capacity;                // TRUE if AirloopHVAC system capacity is calculated
        bool CoolingCapacity;         // TRUE if AirloopHVAC system cooling capacity is calculated
        bool HeatingCapacity;         // TRUE if AirloopHVAC system heating capacity is calculated
        bool SystemCapacity;          // TRUE if AirloopHVAC system heating capacity is calculated
        bool DesignSizeFromParent;    // TRUE if design size is set by parent object - normally false, set to true for special cases e.g. ERV
        int HVACSizingIndex;          // index to DesignSpecification:ZoneHVAC:Sizing
        Array1D_int SizingMethod;    // supply air flow rate sizing method (SupplyAirFlowRate, FlowPerFloorArea, FractionOfAutosizedCoolingAirflow and
                                     // FractionOfAutosizedHeatingAirflow)
        Array1D_int CapSizingMethod; // capacity sizing methods (HeatingDesignCapacity, CoolingDesignCapacity, CapacityPerFloorArea,
                                     // FractionOfAutosizedCoolingCapacity and FractionOfAutosizedHeatingCapacity )

        // Default Constructor
        ZoneEqSizingData()
            : AirVolFlow(0.0), MaxHWVolFlow(0.0), MaxCWVolFlow(0.0), OAVolFlow(0.0),
              ATMixerVolFlow(0.0),         // design ventilation air flow rate from air terminal mixer (central DOAS) [m3/s]
              ATMixerCoolPriDryBulb(0.0),  // design air terminal mixer cooling outlet temperature [C]
              ATMixerCoolPriHumRat(0.0),   // design air terminal mixer cooling outlet humidity ratio [kgWater/kgDryAir]
              ATMixerHeatPriDryBulb(0.0),  // design air terminal mixer heating outlet temperature [C]
              ATMixerHeatPriHumRat(0.0),   // design air terminal mixer heating outlet humidity ratio [kgWater/kgDryAir]
              DesCoolingLoad(0.0),         // design cooling load used for zone equipment [W]
              DesHeatingLoad(0.0),         // design heating load used for zone equipment [W]
              CoolingAirVolFlow(0.0),      // design cooling air vol flow rate for equipment[m3/s]
              HeatingAirVolFlow(0.0),      // design heating air vol flow rate for equipment[m3/s]
              SystemAirVolFlow(0.0),       // design heating air vol flow rate for equipment[m3/s]
              AirFlow(false),              // TRUE if AirloopHVAC system air flow rate is calculated
              CoolingAirFlow(false),       // TRUE if AirloopHVAC system cooling air flow rate is calculated
              HeatingAirFlow(false),       // TRUE if AirloopHVAC system heating air flow rate is calculated
              SystemAirFlow(false),        // TRUE if AirloopHVAC system heating air flow rate is calculated
              Capacity(false),             // TRUE if AirloopHVAC system capacity is calculated
              CoolingCapacity(false),      // TRUE if AirloopHVAC system cooling capacity is calculated
              HeatingCapacity(false),      // TRUE if AirloopHVAC system heating capacity is calculated
              SystemCapacity(false),       // TRUE if AirloopHVAC system heating capacity is calculated
              DesignSizeFromParent(false), // TRUE if design size is set by parent object - normally false, set to true for special cases e.g. ERV
              HVACSizingIndex(0)           // index to DesignSpecification:ZoneHVAC:Sizing
        {
        }
    };

    // Data Structure for Zone HVAC sizing, referenced by various ZoneHVAC Equipment
    struct ZoneHVACSizingData
    {
        // Members
        std::string Name;
        int CoolingSAFMethod;           // - Method for cooling supply air flow rate sizing calculation (SupplyAirFlowRate,FlowPerFloorArea,
                                        // FractionOfAutoSizedCoolingValue, FlowPerCoolingCapacity)
        int HeatingSAFMethod;           // - Method for heating supply air flow rate sizing calculation (SupplyAirFlowRate,FlowPerFloorArea,
                                        // FractionOfAutoSizedHeatingValue, FlowPerHeatingCapacity,
        int NoCoolHeatSAFMethod;        // - Method for supply air flow sizing during no cooling and heating calculation (SupplyAirFlowRate,
                                        // FractionOfAutoSizedCoolingValue, FractionOfAutoSizedHeatingValue)
        int CoolingCapMethod;           // - Method for cooling capacity scaledsizing calculation (CoolingDesignCapacity, CapacityPerFloorArea,
                                        // FractionOfAutosizedHeatingCapacity)
        int HeatingCapMethod;           // - Method for heatiing capacity scaledsizing calculation (HeatingDesignCapacity, CapacityPerFloorArea,
                                        // FracOfAutosizedHeatingCapacity)
        Real64 MaxCoolAirVolFlow;       // - maximum cooling supply air flow rate, m3/s
        Real64 MaxHeatAirVolFlow;       // - maximum heating supply air flow rate, m3/s
        Real64 MaxNoCoolHeatAirVolFlow; // - maximum supply air flow rate when no cooling or heating, m3/s
        Real64 ScaledCoolingCapacity;   // - scaled maximum cooling capacity of zone HVAC equipment, W
        Real64 ScaledHeatingCapacity;   // - scaled maximum heating capacity of zone HVAC equipment, W
        bool RequestAutoSize;           // - true if autosizing is requested

        // Default Constructor
        ZoneHVACSizingData()
            : CoolingSAFMethod(0), HeatingSAFMethod(0), NoCoolHeatSAFMethod(0), CoolingCapMethod(0), HeatingCapMethod(0), MaxCoolAirVolFlow(0.0),
              MaxHeatAirVolFlow(0.0), MaxNoCoolHeatAirVolFlow(0.0), ScaledCoolingCapacity(0.0), ScaledHeatingCapacity(0.0), RequestAutoSize(false)
        {
        }
    };

    // Data Structure for air terminal sizing, referenced by ZoneHVAC:AirDistributionUnit
    struct AirTerminalSizingSpecData
    {
        // Members
        std::string Name;
        Real64 DesSensCoolingFrac; // Fraction of Design Sensible Cooling Load
        Real64 DesCoolSATRatio;    // Cooling Design Supply Air Temperature Difference Ratio
        Real64 DesSensHeatingFrac; // Fraction of Design Sensible Heating Load
        Real64 DesHeatSATRatio;    // Heating Design Supply Air Temperature Difference Ratio
        Real64 MinOAFrac;          // Fraction of Minimum Outdoor Air Flow

        // Default Constructor
        AirTerminalSizingSpecData() : DesSensCoolingFrac(1.0), DesCoolSATRatio(1.0), DesSensHeatingFrac(1.0), DesHeatSATRatio(1.0), MinOAFrac(1.0)
        {
        }
    };

    struct SystemSizingInputData
    {
        // Members
        std::string AirPriLoopName; // name of an AirLoopHVAC object
        int AirLoopNum;             // index number of air loop
        int LoadSizeType;           // type of load to size on;
        // 0=sensible, 1=latent, 2=total, 3=ventilation
        int SizingOption;                  // 1 = noncoincident, 2 = coincident
        int CoolOAOption;                  // 1 = use 100% outside air; 2 = use min OA; for cooling sizing
        int HeatOAOption;                  // 1 = use 100% outside air; 2 = use min OA; for heating sizing
        Real64 DesOutAirVolFlow;           // design (minimum) outside air flow rate [m3/s]
        Real64 SysAirMinFlowRat;           // minimum system air flow ratio for heating, Central Heating Maximum System Air Flow Ratio
        bool SysAirMinFlowRatWasAutoSized; // true if central heating maximum system air flow ratio was autosize on input
        Real64 PreheatTemp;                // preheat design set temperature [C]
        Real64 PrecoolTemp;                // precool design set temperature [C]
        Real64 PreheatHumRat;              // preheat design humidity ratio [kg water/kg dry air]
        Real64 PrecoolHumRat;              // precool design humidity ratio [kg water/kg dry air]
        Real64 CoolSupTemp;                // cooling design supply air temperature [C]
        Real64 HeatSupTemp;                // heating design supply air temperature [C]
        Real64 CoolSupHumRat;              // cooling design supply air humidity ratio [kg water/kg dry air]
        Real64 HeatSupHumRat;              // heating design supply air humidity ratio [kg water/kg dry air]
        int CoolAirDesMethod;              // choice of how to get system cooling design air flow rates;
        //  1 = calc from des day simulation; 2=m3/s per system, user input
        Real64 DesCoolAirFlow; // design system supply air flow rate for cooling[m3/s]
        int HeatAirDesMethod;  // choice of how to get system heating design air flow rates;
        //  1 = calc from des day simulation; 2=m3/s per zone, user input
        Real64 DesHeatAirFlow;           // design system heating supply air flow rate [m3/s]
        int ScaleCoolSAFMethod;          // choice of how to get system cooling scalable air flow rates; // (FlowPerFloorArea,
                                         // FractionOfAutosizedCoolingAirflow, FlowPerCoolingCapacity)
        int ScaleHeatSAFMethod;          // choice of how to get system heating scalable air flow rates; // (FlowPerFloorArea,
                                         // FractionOfAutosizedCoolingAirflow, FractionOfAutosizedHeatingAirflow, FlowPerHeatingCapacity)
        int SystemOAMethod;              // System Outdoor Air Method; 1 = SOAM_ZoneSum, 2 = SOAM_VRP
        Real64 MaxZoneOAFraction;        // maximum value of min OA for zones served by system
        bool OAAutoSized;                // Set to true if design OA vol flow is set to 'autosize' in Sizing:System
        int CoolingCapMethod;            // - Method for cooling capacity scaledsizing calculation (CoolingDesignCapacity, CapacityPerFloorArea,
                                         // FractionOfAutosizedCoolingCapacity)
        int HeatingCapMethod;            // - Method for heatiing capacity scaledsizing calculation (HeatingDesignCapacity, CapacityPerFloorArea,
                                         // FracOfAutosizedHeatingCapacity)
        Real64 ScaledCoolingCapacity;    // - scaled maximum cooling capacity of cooling coil in an air loop
        Real64 ScaledHeatingCapacity;    // - scaled maximum heating capacity of cooling coil in an air loop
        Real64 FloorAreaOnAirLoopCooled; // total floor of cooled zones served by an airloop
        Real64 FloorAreaOnAirLoopHeated; // total floor of heated zones served by an airloop
        Real64 FlowPerFloorAreaCooled;   // ratio of cooling supply air flow rate to total floor area of cooled zones served by an airloop
        Real64 FlowPerFloorAreaHeated;   // ratio of cooling supply air flow rate to total floor area of cooled zones served by an airloop
        Real64 FractionOfAutosizedCoolingAirflow; // fraction of of cooling supply air flow rate an airloop
        Real64 FractionOfAutosizedHeatingAirflow; // fraction of of heating supply air flow rate an airloop
        Real64 FlowPerCoolingCapacity;            // ratio of cooling supply air flow rate to cooling capacity of an airloop
        Real64 FlowPerHeatingCapacity;            // ratio of heating supply air flow rate to heating capacity of an airloop
        int CoolingPeakLoadType;                  // Type of peak to size cooling coils on   1=SensibleCoolingLoad; 2=TotalCoolingLoad
        int CoolCapControl;                       // type of control of cooling coil  1=VAV; 2=Bypass; 3=VT; 4=OnOff

        // Default Constructor
        SystemSizingInputData()
            : AirLoopNum(0), LoadSizeType(0), SizingOption(0), CoolOAOption(0), HeatOAOption(0), DesOutAirVolFlow(0.0), SysAirMinFlowRat(0.0),
              SysAirMinFlowRatWasAutoSized(false), PreheatTemp(0.0), PrecoolTemp(0.0), PreheatHumRat(0.0), PrecoolHumRat(0.0), CoolSupTemp(0.0),
              HeatSupTemp(0.0), CoolSupHumRat(0.0), HeatSupHumRat(0.0), CoolAirDesMethod(0), DesCoolAirFlow(0.0), HeatAirDesMethod(0),
              DesHeatAirFlow(0.0), ScaleCoolSAFMethod(0), ScaleHeatSAFMethod(0), SystemOAMethod(0), MaxZoneOAFraction(0.0), OAAutoSized(false),
              CoolingCapMethod(0), HeatingCapMethod(0), ScaledCoolingCapacity(0.0), ScaledHeatingCapacity(0.0), FloorAreaOnAirLoopCooled(0.0),
              FloorAreaOnAirLoopHeated(0.0), FlowPerFloorAreaCooled(0.0), FlowPerFloorAreaHeated(0.0), FractionOfAutosizedCoolingAirflow(1.0),
              FractionOfAutosizedHeatingAirflow(1.0), FlowPerCoolingCapacity(0.0), FlowPerHeatingCapacity(0.0), CoolingPeakLoadType(0), // wfb
              CoolCapControl(0)                                                                                                         // wfb
        {
        }
    };

    struct SystemSizingData // Contains data for system sizing
    {
        // Members
        std::string AirPriLoopName; // name of an AirLoopHVAC object
        std::string CoolDesDay;     // name of a cooling design day
        std::string HeatDesDay;     // name of a heating design day
        int LoadSizeType;           // type of load to size on;
        // 0=sensible, 1=latent, 2=total, 3=ventilation
        int SizingOption;                  // 1 = noncoincident, 2 = coincident.
        int CoolOAOption;                  // 1 = use 100% outside air; 2 = use min OA; for cooling sizing
        int HeatOAOption;                  // 1 = use 100% outside air; 2 = use min OA; for heating sizing
        Real64 DesOutAirVolFlow;           // design (minimum) outside air flow rate [m3/s]
        Real64 SysAirMinFlowRat;           // minimum system air flow ratio for heating, Central Heating Maximum System Air Flow Ratio
        bool SysAirMinFlowRatWasAutoSized; // true if central heating maximum system air flow ratio was autosize on input
        Real64 PreheatTemp;                // preheat design set temperature
        Real64 PrecoolTemp;                // precool design set temperature [C]
        Real64 PreheatHumRat;              // preheat design humidity ratio [kg water/kg dry air]
        Real64 PrecoolHumRat;              // precool design humidity ratio [kg water/kg dry air]
        Real64 CoolSupTemp;                // cooling design supply air temperature [C]
        Real64 HeatSupTemp;                // heating design supply air temperature[C]
        Real64 CoolSupHumRat;              // cooling design supply air humidity ratio [kg water/kg dry air]
        Real64 HeatSupHumRat;              // heating design supply air humidity ratio [kg water/kg dry air]
        int CoolAirDesMethod;              // choice of how to get system design cooling air flow rates;
        //  1 = calc from des day simulation; 2=m3/s per system, user input
        int HeatAirDesMethod; // choice of how to get system design heating air flow rates;
        //  1 = calc from des day simulation; 2=m3/s per system, user input
        Real64 InpDesCoolAirFlow;              // input design system supply air flow rate [m3/s]
        Real64 InpDesHeatAirFlow;              // input design system heating supply air flow rate [m3/s]
        Real64 CoinCoolMassFlow;               // coincident peak cooling mass flow rate [kg/s]
        bool EMSOverrideCoinCoolMassFlowOn;    // If true, EMS to change coincident peak cooling mass flow rate
        Real64 EMSValueCoinCoolMassFlow;       // Value EMS wants for coincident peak cooling mass flow rate [kg/s]
        Real64 CoinHeatMassFlow;               // coincident peak heating mass flow rate [kg/s]
        bool EMSOverrideCoinHeatMassFlowOn;    // If true, EMS to set coincident peak heating mass flow rate
        Real64 EMSValueCoinHeatMassFlow;       // Value EMS wants for coincident peak heating mass flow rate [kg/s]
        Real64 NonCoinCoolMassFlow;            // noncoincident peak cooling mass flow rate [kg/s]
        bool EMSOverrideNonCoinCoolMassFlowOn; // true, EMS to set noncoincident peak cooling mass flow rate
        Real64 EMSValueNonCoinCoolMassFlow;    // Value EMS for noncoincident peak cooling mass flow rate [kg/s]
        Real64 NonCoinHeatMassFlow;            // noncoincident peak heating mass flow rate [kg/s]
        bool EMSOverrideNonCoinHeatMassFlowOn; // true, EMS to set noncoincident peak heating mass flow rate
        Real64 EMSValueNonCoinHeatMassFlow;    // Value EMS for noncoincident peak heating mass flow rate [kg/s]
        Real64 DesMainVolFlow;                 // design main supply duct volume flow [m3/s]
        bool EMSOverrideDesMainVolFlowOn;      // If true, EMS is acting to change DesMainVolFlow
        Real64 EMSValueDesMainVolFlow;         // Value EMS providing for design main supply duct volume flow [m3/s]
        Real64 DesHeatVolFlow;                 // design heat supply duct volume flow [m3/s]
        bool EMSOverrideDesHeatVolFlowOn;      // If true, EMS is acting to change DesCoolVolFlow
        Real64 EMSValueDesHeatVolFlow;         // Value EMS providing for design cool  supply duct volume flow [m3/s]
        Real64 DesCoolVolFlow;                 // design cool  supply duct volume flow [m3/s]
        bool EMSOverrideDesCoolVolFlowOn;      // If true, EMS is acting to change DesCoolVolFlow
        Real64 EMSValueDesCoolVolFlow;         // Value EMS providing for design cool  supply duct volume flow [m3/s]
        Real64 SensCoolCap;                    // design sensible cooling capacity [W]
        Real64 TotCoolCap;                     // design total cooling capacity [W]
        Real64 HeatCap;                        // design heating capacity [W]
        Real64 PreheatCap;                     // design preheat capacity [W]
        Real64 MixTempAtCoolPeak;              // design mixed air temperature for cooling [C]
        Real64 MixHumRatAtCoolPeak;            // design mixed air hum ratio for cooling [kg water/kg dry air]
        Real64 RetTempAtCoolPeak;              // design return air temperature for cooling [C]
        Real64 RetHumRatAtCoolPeak;            // design return air hum ratio for cooling [kg water/kg dry air]
        Real64 OutTempAtCoolPeak;              // design outside air temperature for cooling [C]
        Real64 OutHumRatAtCoolPeak;            // design outside air hum ratio for cooling [kg water/kg dry air]
        Real64 MassFlowAtCoolPeak;             // air mass flow rate at the cooling peak [kg/s]
        Real64 HeatMixTemp;                    // design mixed air temperature for heating [C]
        Real64 HeatMixHumRat;                  // design mixed air hum ratio for heating [kg water/kg dry air]
        Real64 HeatRetTemp;                    // design return air temperature for heating [C]
        Real64 HeatRetHumRat;                  // design return air hum ratio for heating [kg water/kg dry air]
        Real64 HeatOutTemp;                    // design outside air temperature for heating [C]
        Real64 HeatOutHumRat;                  // design outside air hum ratio for Heating [kg water/kg dry air]
        Real64 DesCoolVolFlowMin;              // design minimum system cooling flow rate [m3/s]
        Array1D<Real64> HeatFlowSeq;           // daily sequence of system heating air mass flow rate
        //  (zone time step)
        Array1D<Real64> SumZoneHeatLoadSeq; // daily sequence of zones summed heating load [W]
        //  (zone time step)
        Array1D<Real64> CoolFlowSeq; // daily sequence of system cooling air mass flow rate
        //  (zone time step)
        Array1D<Real64> SumZoneCoolLoadSeq; // daily sequence of zones summed cooling load [W]
        //  (zone time step)
        Array1D<Real64> CoolZoneAvgTempSeq; // daily sequence of zones flow weighted average temperature [C]
        //  (zone time step)
        Array1D<Real64> HeatZoneAvgTempSeq; // daily sequence of zones flow weighted average temperature [C]
        //  (zone time step)
        Array1D<Real64> SensCoolCapSeq; // daily sequence of system sensible cooling capacity
        //  (zone time step)
        Array1D<Real64> TotCoolCapSeq; // daily sequence of system total cooling capacity
        //  (zone time step)
        Array1D<Real64> HeatCapSeq;        // daily sequence of system heating capacity [zone time step]
        Array1D<Real64> PreheatCapSeq;     // daily sequence of system preheat capacity [zone time step]
        Array1D<Real64> SysCoolRetTempSeq; // daily sequence of system cooling return temperatures [C]
        //  [zone time step]
        Array1D<Real64> SysCoolRetHumRatSeq; // daily sequence of system cooling return humidity ratios
        //  [kg water/kg dry air] [zone time step]
        Array1D<Real64> SysHeatRetTempSeq; // daily sequence of system heating return temperatures [C]
        //   [zone time step]
        Array1D<Real64> SysHeatRetHumRatSeq; // daily sequence of system heating return humidity ratios
        //  [kg water/kg dry air] [zone time step]
        Array1D<Real64> SysCoolOutTempSeq; // daily sequence of system cooling outside temperatures [C]
        //  [zone time step]
        Array1D<Real64> SysCoolOutHumRatSeq; // daily sequence of system cooling outside humidity ratios
        //  [kg water/kg dry air] [zone time step]
        Array1D<Real64> SysHeatOutTempSeq; // daily sequence of system heating outside temperatures [C]
        //  [zone time step]
        Array1D<Real64> SysHeatOutHumRatSeq; // daily sequence of system heating outside humidity ratios
        //   [kg water/kg dry air] [zone time step]
        Array1D<Real64> SysDOASHeatAddSeq; // daily sequence of heat addition rate from DOAS supply air [W]
        Array1D<Real64> SysDOASLatAddSeq;  // daily sequence of latent heat addition rate from DOAS supply air [W]
        int SystemOAMethod;                // System Outdoor Air Method; 1 = SOAM_ZoneSum, 2 = SOAM_VRP
        Real64 MaxZoneOAFraction;          // maximum value of min OA for zones served by system
        Real64 SysUncOA;                   // uncorrected system outdoor air flow based on zone people and zone area
        bool OAAutoSized;                  // Set to true if design OA vol flow is set to 'autosize'
        int ScaleCoolSAFMethod; // choice of how to get system cooling scalable air flow rates; (FlowPerFloorArea, FractionOfAutosizedCoolingAirflow,
                                // FlowPerCoolingCapacity)
        int ScaleHeatSAFMethod; // choice of how to get system heating scalable air flow rates; (FlowPerFloorArea, FractionOfAutosizedCoolingAirflow,
                                // FractionOfAutosizedHeatingAirflow, FlowPerHeatingCapacity)
        int CoolingCapMethod;   // - Method for cooling capacity scaledsizing calculation (CoolingDesignCapacity, CapacityPerFloorArea,
                                // FractionOfAutosizedCoolingCapacity)
        int HeatingCapMethod;   // - Method for heatiing capacity scaledsizing calculation (HeatingDesignCapacity, CapacityPerFloorArea,
                                // FracOfAutosizedHeatingCapacity)
        Real64 ScaledCoolingCapacity;              // - scaled maximum cooling capacity of cooling coil in an air loop
        Real64 ScaledHeatingCapacity;              // - scaled maximum heating capacity of cooling coil in an air loop
        Real64 FloorAreaOnAirLoopCooled;           // total floor of cooled zones served by an airloop
        Real64 FloorAreaOnAirLoopHeated;           // total floor of heated zones served by an airloop
        Real64 FlowPerFloorAreaCooled;             // ratio of cooling supply air flow rate to total floor area of cooled zones served by an airloop
        Real64 FlowPerFloorAreaHeated;             // ratio of cooling supply air flow rate to total floor area of cooled zones served by an airloop
        Real64 FractionOfAutosizedCoolingAirflow;  // fraction of of cooling supply air flow rate an airloop
        Real64 FractionOfAutosizedHeatingAirflow;  // fraction of of heating supply air flow rate an airloop
        Real64 FlowPerCoolingCapacity;             // ratio of cooling supply air flow rate to cooling capacity of an airloop
        Real64 FlowPerHeatingCapacity;             // ratio of heating supply air flow rate to heating capacity of an airloop
        Real64 FractionOfAutosizedCoolingCapacity; // fraction of of cooling total capacity
        Real64 FractionOfAutosizedHeatingCapacity; // fraction of of heating total capacity
        Real64 CoolingTotalCapacity;               // system total cooling capacity
        Real64 HeatingTotalCapacity;               // system total heating capacity
        int CoolingPeakLoadType;                   // Type of peak to size cooling coils on   1=SensibleCoolingLoad; 2=TotalCooligLoad
        int CoolCapControl;                        // type of control of cooling coil  1=VAV; 2=Bypass; 3=VT; 4=OnOff
        bool sysSizeHeatingDominant;
        bool sysSizeCoolingDominant;

        Real64 CoinCoolCoilMassFlow; // coincident volume flow at time of cooling coil sensible+latent peak [m3/s]
        Real64 CoinHeatCoilMassFlow; // coincident volume flow at time of heating coil sensible peak [m3/s]
        Real64 DesCoolCoilVolFlow;   // design cooling air volume flow rate at time of coil sens+latent peak [m3/s]
        Real64 DesHeatCoilVolFlow;   // design heating air volume flow rate at time of coil sens peak [m3/s]
        Real64 DesMainCoilVolFlow;   // design main supply duct volume flow at time of coil peak [m3/s]
        // These are for reporting purposes

        int SysHeatCoilTimeStepPk; // timestep in day of heating coil peak
        int SysHeatAirTimeStepPk;  // timestep in day of heating airflow peak
        int HeatDDNum;             // index of design day for heating
        int CoolDDNum;             // index of design day for cooling

        Real64 SysCoolCoinSpaceSens; // sum of zone space sensible cooling loads at coincident peak
        Real64 SysHeatCoinSpaceSens; //  sum of zone space sensible heating loads at coincident peak
        // Default Constructor
        SystemSizingData()
            : LoadSizeType(0), SizingOption(0), CoolOAOption(0), HeatOAOption(0), DesOutAirVolFlow(0.0), SysAirMinFlowRat(0.0),
              SysAirMinFlowRatWasAutoSized(false), PreheatTemp(0.0), PrecoolTemp(0.0), PreheatHumRat(0.0), PrecoolHumRat(0.0), CoolSupTemp(0.0),
              HeatSupTemp(0.0), CoolSupHumRat(0.0), HeatSupHumRat(0.0), CoolAirDesMethod(0), HeatAirDesMethod(0), InpDesCoolAirFlow(0.0),
              InpDesHeatAirFlow(0.0), CoinCoolMassFlow(0.0), EMSOverrideCoinCoolMassFlowOn(false), EMSValueCoinCoolMassFlow(0.0),
              CoinHeatMassFlow(0.0), EMSOverrideCoinHeatMassFlowOn(false), EMSValueCoinHeatMassFlow(0.0), NonCoinCoolMassFlow(0.0),
              EMSOverrideNonCoinCoolMassFlowOn(false), EMSValueNonCoinCoolMassFlow(0.0), NonCoinHeatMassFlow(0.0),
              EMSOverrideNonCoinHeatMassFlowOn(false), EMSValueNonCoinHeatMassFlow(0.0), DesMainVolFlow(0.0), EMSOverrideDesMainVolFlowOn(false),
              EMSValueDesMainVolFlow(0.0), DesHeatVolFlow(0.0), EMSOverrideDesHeatVolFlowOn(false), EMSValueDesHeatVolFlow(0.0), DesCoolVolFlow(0.0),
              EMSOverrideDesCoolVolFlowOn(false), EMSValueDesCoolVolFlow(0.0), SensCoolCap(0.0), TotCoolCap(0.0), HeatCap(0.0), PreheatCap(0.0),
              MixTempAtCoolPeak(0.0), MixHumRatAtCoolPeak(0.0), RetTempAtCoolPeak(0.0), RetHumRatAtCoolPeak(0.0), OutTempAtCoolPeak(0.0),
              OutHumRatAtCoolPeak(0.0), MassFlowAtCoolPeak(0.0), HeatMixTemp(0.0), HeatMixHumRat(0.0), HeatRetTemp(0.0), HeatRetHumRat(0.0),
              HeatOutTemp(0.0), HeatOutHumRat(0.0), DesCoolVolFlowMin(0.0), SystemOAMethod(0), MaxZoneOAFraction(0.0), SysUncOA(0.0),
              OAAutoSized(false), ScaleCoolSAFMethod(0), ScaleHeatSAFMethod(0), CoolingCapMethod(0), HeatingCapMethod(0), ScaledCoolingCapacity(0.0),
              ScaledHeatingCapacity(0.0), FloorAreaOnAirLoopCooled(0.0), FloorAreaOnAirLoopHeated(0.0), FlowPerFloorAreaCooled(0.0),
              FlowPerFloorAreaHeated(0.0), FractionOfAutosizedCoolingAirflow(1.0), FractionOfAutosizedHeatingAirflow(1.0),
              FlowPerCoolingCapacity(0.0), FlowPerHeatingCapacity(0.0), FractionOfAutosizedCoolingCapacity(1.0),
              FractionOfAutosizedHeatingCapacity(1.0), CoolingTotalCapacity(0.0), HeatingTotalCapacity(0.0), CoolingPeakLoadType(0), // wfb
              CoolCapControl(0),                                                                                                     // wfb
              sysSizeHeatingDominant(false), sysSizeCoolingDominant(false), CoinCoolCoilMassFlow(0.0), CoinHeatCoilMassFlow(0.0),
              DesCoolCoilVolFlow(0.0), DesHeatCoilVolFlow(0.0), DesMainCoilVolFlow(0.0), SysHeatCoilTimeStepPk(0), SysHeatAirTimeStepPk(0),
              HeatDDNum(0), CoolDDNum(0), SysCoolCoinSpaceSens(0.0), SysHeatCoinSpaceSens(0.0)
        {
        }
    };

    struct SysSizPeakDDNumData
    {
        // Members
        int SensCoolPeakDD;                // design day containing the sensible cooling peak
        std::string cSensCoolPeakDDDate;   // date string of design day causing sensible cooling peak
        int TotCoolPeakDD;                 // design day containing total cooling peak
        std::string cTotCoolPeakDDDate;    // date string of design day causing total cooling peak
        int CoolFlowPeakDD;                // design day containing the cooling air flow peak
        std::string cCoolFlowPeakDDDate;   // date string of design day causing cooling air flow peak
        int HeatPeakDD;                    // design day containing the heating peak
        std::string cHeatPeakDDDate;       // date string of design day causing heating peak
        Array1D<int> TimeStepAtSensCoolPk; // time step of the sensible cooling peak
        Array1D<int> TimeStepAtTotCoolPk;  // time step of the total cooling peak
        Array1D<int> TimeStepAtCoolFlowPk; // time step of the cooling air flow peak
        Array1D<int> TimeStepAtHeatPk;     // time step of the heating peak

        // Default Constructor
        SysSizPeakDDNumData() : SensCoolPeakDD(0), TotCoolPeakDD(0), CoolFlowPeakDD(0), HeatPeakDD(0)
        {
        }
    };

    struct PlantSizingData
    {
        // Members
        std::string PlantLoopName; // name of PLANT LOOP or CONDENSER LOOP object
        int LoopType;              // type of loop: 1=heating, 2=cooling, 3=condenser
        Real64 ExitTemp;           // loop design exit (supply) temperature [C]
        Real64 DeltaT;             // loop design temperature drop (or rise) [DelK]
        int ConcurrenceOption;     // sizing option for coincident or noncoincident
        int NumTimeStepsInAvg;     // number of zone timesteps in the averaging window for coincident plant flow
        int SizingFactorOption;    // option for what sizing factor to apply
        // Calculated
        Real64 DesVolFlowRate;  // loop design flow rate in m3/s
        bool VolFlowSizingDone; // flag to indicate when this loop has finished sizing flow rate
        Real64 PlantSizFac;     // hold the loop and pump sizing factor

        // Default Constructor
        PlantSizingData()
            : LoopType(0), ExitTemp(0.0), DeltaT(0.0), ConcurrenceOption(1), NumTimeStepsInAvg(0), SizingFactorOption(101), DesVolFlowRate(0.0),
              VolFlowSizingDone(false), PlantSizFac(1.0)
        {
        }
    };

    // based on ZoneSizingData but only have member variables that are related to the CheckSum/
    struct FacilitySizingData
    {
        // Members
        int CoolDDNum;                    // design day index of design day causing heating peak
        int HeatDDNum;                    // design day index of design day causing heating peak
        int TimeStepNumAtCoolMax;         // time step number (in day) at cooling peak
        Array1D<Real64> DOASHeatAddSeq;   // daily sequence of zone DOAS heat addition rate (zone time step) [W]
        Array1D<Real64> DOASLatAddSeq;    // daily sequence of zone DOAS latent heat addition rate (zone time step) [W]
        Array1D<Real64> CoolOutHumRatSeq; // daily sequence of outdoor humidity ratios (cooling, zone time step)
        Array1D<Real64> CoolOutTempSeq;   // daily sequence of outdoor temperatures (cooling, zone time step)
        Array1D<Real64> CoolZoneTempSeq;  // daily sequence of zone temperatures (cooling, zone time step)
        Array1D<Real64> CoolLoadSeq;      // daily sequence of cooling load (cooling, zone time step)
        Real64 DesCoolLoad;               // zone design cooling load [W]
        int TimeStepNumAtHeatMax;         // time step number (in day) at Heating peak
        Array1D<Real64> HeatOutHumRatSeq; // daily sequence of outdoor humidity ratios (heating, zone time step)
        Array1D<Real64> HeatOutTempSeq;   // daily sequence of outdoor temperatures (heating, zone time step)
        Array1D<Real64> HeatZoneTempSeq;  // daily sequence of zone temperatures (heating, zone time step)
        Array1D<Real64> HeatLoadSeq;      // daily sequence of heating load (cooling, zone time step)
        Real64 DesHeatLoad;               // zone design heating load [W]

        // Default Constructor
        FacilitySizingData() : CoolDDNum(0), HeatDDNum(0), TimeStepNumAtCoolMax(0), DesCoolLoad(0.0), TimeStepNumAtHeatMax(0), DesHeatLoad(0.0)
        {
        }
    };

    struct DesDayWeathData
    {
        // Members
        std::string DateString; // date of design day weather values
        Array1D<Real64> Temp;   // design day temperatures at the major time step
        Array1D<Real64> HumRat; // design day humidity ratios at the major time step
        Array1D<Real64> Press;  // design day barometric pressure at the major time step
    };

    struct CompDesWaterFlowData // design water flow rate for components that use water as an
    {
        // Members
        //  energy source or sink
        int SupNode;           // water inlet node number (condenser side for water / water)
        Real64 DesVolFlowRate; // water design flow rate [m3/s]

        // Default Constructor
        CompDesWaterFlowData() : SupNode(0), DesVolFlowRate(0.0)
        {
        }

        // Member Constructor
        CompDesWaterFlowData(int SupNode,          // water inlet node number (condenser side for water / water)
                             Real64 DesVolFlowRate // water design flow rate [m3/s]
                             )
            : SupNode(SupNode), DesVolFlowRate(DesVolFlowRate)
        {
        }
    };

    struct OARequirementsData
    {
        // Members
        std::string Name;
        int OAFlowMethod; // - Method for OA flow calculation
        //- (Flow/Person, Flow/Zone, Flow/Area, FlowACH, Sum, Maximum)
        Real64 OAFlowPerPerson;       // - OA requirement per person
        Real64 OAFlowPerArea;         // - OA requirement per zone area
        Real64 OAFlowPerZone;         // - OA requirement per zone
        Real64 OAFlowACH;             // - OA requirement per zone per hour
        int OAFlowFracSchPtr;         // - Fraction schedule applied to total OA requirement
        int OAPropCtlMinRateSchPtr;   // - Fraction schedule applied to Proportional Control Minimum Outdoor Air Flow Rate
        int CO2MaxMinLimitErrorCount; // Counter when max CO2 concentration < min CO2 concentration for SOAM_ProportionalControlSchOcc
        int CO2MaxMinLimitErrorIndex; // Index for max CO2 concentration < min CO2 concentration recurring error message for
                                      // SOAM_ProportionalControlSchOcc
        int CO2GainErrorCount;        // Counter when CO2 generation from people is zero for SOAM_ProportionalControlSchOcc
        int CO2GainErrorIndex;        // Index for recurring error message when CO2 generation from people is zero for SOAM_ProportionalControlSchOcc

        // Default Constructor
        OARequirementsData()
            : OAFlowMethod(0), OAFlowPerPerson(0.0), OAFlowPerArea(0.0), OAFlowPerZone(0.0), OAFlowACH(0.0),
              OAFlowFracSchPtr(DataGlobalConstants::ScheduleAlwaysOn), OAPropCtlMinRateSchPtr(DataGlobalConstants::ScheduleAlwaysOn),
              CO2MaxMinLimitErrorCount(0), CO2MaxMinLimitErrorIndex(0), CO2GainErrorCount(0), CO2GainErrorIndex(0)
        {
        }
    };

    struct ZoneAirDistributionData
    {
        // Members
        std::string Name;
        std::string ZoneADEffSchName;      // - Zone air distribution effectiveness schedule name
        Real64 ZoneADEffCooling;           // - Zone air distribution effectiveness in cooling mode
        Real64 ZoneADEffHeating;           // - Zone air distribution effectiveness in heating mode
        Real64 ZoneSecondaryRecirculation; // - Zone air secondary recirculation ratio
        int ZoneADEffSchPtr;               // - Zone air distribution effectiveness schedule index
        Real64 ZoneVentilationEff;         // Zone ventilation effectiveness

        // Default Constructor
        ZoneAirDistributionData()
            : ZoneADEffCooling(1.0), ZoneADEffHeating(1.0), ZoneSecondaryRecirculation(0.0), ZoneADEffSchPtr(0), ZoneVentilationEff(0.0)
        {
        }

        Real64 calculateEz(EnergyPlusData &state, int ZoneNum); // Zone index
    };

    // Resets Data globals so that previously set variables are not used in other equipment models
    void resetHVACSizingGlobals(EnergyPlusData &state,
                                int curZoneEqNum,
                                int curSysNum,
                                bool &firstPassFlag // Can be set to false during the routine
    );

    void GetCoilDesFlowT(EnergyPlusData &state,
                         int SysNum,           // central air system index
                         Real64 CpAir,         // specific heat to be used in calculations [J/kgC]
                         Real64 &DesFlow,      // returned design mass flow [kg/s]
                         Real64 &DesExitTemp,  // returned design coil exit temperature [kg/s]
                         Real64 &DesExitHumRat // returned design coil exit humidity ratio [kg/kg]
    );

} // namespace DataSizing

struct SizingData : BaseGlobalStruct
{
    int NumOARequirements = 0;                       // Number of OA Requirements objects
    int NumZoneAirDistribution = 0;                  // Number of zone air distribution objects
    int NumZoneSizingInput = 0;                      // Number of Zone Sizing objects
    int NumSysSizInput = 0;                          // Number of System Sizing objects
    int NumPltSizInput = 0;                          // Number of Plant Sizing objects
    int CurSysNum = 0;                               // Current Air System index (0 if not in air loop)
    int CurOASysNum = 0;                             // Current outside air system index (0 if not in OA Sys)
    int CurZoneEqNum = 0;                            // Current Zone Equipment index (0 if not simulating ZoneEq)
    int CurTermUnitSizingNum = 0;                    // Current terminal unit sizing index for TermUnitSizing and TermUnitFinalZoneSizing
    int CurBranchNum = 0;                            // Index of branch being simulated (or 0 if not air loop)
    int CurDuctType = 0;                             // Duct type of current branch
    int CurLoopNum = 0;                              // the current plant loop index
    int CurCondLoopNum = 0;                          // the current condenser loop number
    int CurEnvirNumSimDay = 0;                       // current environment number for day simulated
    int CurOverallSimDay = 0;                        // current day of simulation
    int NumTimeStepsInAvg = 0;                       // number of time steps in the averaging window for the design flow and load sequences
    int SaveNumPlantComps = 0;                       // Number of components using water as an energy source or sink (e.g. water coils)
    int DataTotCapCurveIndex = 0;                    // index to total capacity as a function of temperature curve
    Real64 DataTotCapCurveValue = 0;                 // value of total capacity as a function of temperature curve for CoilVRF_FluidTCtrl_*
    int DataPltSizCoolNum = 0;                       // index to cooling plant sizing data
    int DataPltSizHeatNum = 0;                       // index to heating plant sizing data
    int DataWaterLoopNum = 0;                        // index to plant water loop
    int DataCoilNum = 0;                             // index to coil object
    int DataFanOpMode = 0;                           // fan operating mode (ContFanCycCoil or CycFanCycCoil)
    bool DataCoilIsSuppHeater = false;               // TRUE if heating coil used as supplemental heater
    bool DataIsDXCoil = false;                       // TRUE if direct-expansion coil
    bool DataAutosizable = true;                     // TRUE if component is autosizable
    bool DataEMSOverrideON = false;                  // boolean determines if user relies on EMS to override autosizing
    bool DataScalableSizingON = false;               // boolean determines scalable flow sizing is specified
    bool DataScalableCapSizingON = false;            // boolean determines scalable capacity sizing is specified
    bool DataSysScalableFlowSizingON = false;        // boolean determines scalable system flow sizing is specified
    bool DataSysScalableCapSizingON = false;         // boolean determines scalable system capacity sizing is specified
    bool SysSizingRunDone = false;                   // True if a system sizing run is successfully completed.
    bool TermUnitSingDuct = false;                   // TRUE if a non-induction single duct terminal unit
    bool TermUnitPIU = false;                        // TRUE if a powered induction terminal unit
    bool TermUnitIU = false;                         // TRUE if an unpowered induction terminal unit
    bool ZoneEqFanCoil = false;                      // TRUE if a 4 pipe fan coil unit is being simulated
    bool ZoneEqOutdoorAirUnit = false;               // TRUE if an OutdoorAirUnit is being simulated
    bool ZoneEqUnitHeater = false;                   // TRUE if a unit heater is being simulated
    bool ZoneEqUnitVent = false;                     // TRUE if a unit ventilator unit is being simulated
    bool ZoneEqVentedSlab = false;                   // TRUE if a ventilated slab is being simulated
    bool ZoneEqDXCoil = false;                       // TRUE if a ZoneHVAC DX coil is being simulated
    bool ZoneEqUnitarySys = false;                   // TRUE if a zone UnitarySystem is being simulated
    bool ZoneCoolingOnlyFan = false;                 // TRUE if a ZoneHVAC DX cooling coil is only coil in parent
    bool ZoneHeatingOnlyFan = false;                 // TRUE if zone unit only does heating and contains a fam (such as Unit Heater)
    bool ZoneSizingRunDone = false;                  // True if a zone sizing run has been successfully completed.
    bool DataErrorsFound = false;                    // used for simulation termination when errors are found
    Real64 AutoVsHardSizingThreshold = 0.1;          // criteria threshold used to determine if user hard size and autosize disagree 10%
    Real64 AutoVsHardSizingDeltaTempThreshold = 1.5; // temperature criteria threshold for autosize versus hard size [C]
    Real64 DataCoilSizingAirInTemp = 0.0;            // saves sizing data for use in coil object reporting
    Real64 DataCoilSizingAirInHumRat = 0.0;          // saves sizing data for use in coil object reporting
    Real64 DataCoilSizingAirOutTemp = 0.0;           // saves sizing data for use in coil object reporting
    Real64 DataCoilSizingAirOutHumRat = 0.0;         // saves sizing data for use in coil object reporting
    Real64 DataCoilSizingFanCoolLoad = 0.0;          // saves sizing data for use in coil object reporting
    Real64 DataCoilSizingCapFT = 1.0;                // saves sizing data for use in coil object reporting
    bool DataDesAccountForFanHeat = true;            // include fan heat when true
    Real64 DataDesInletWaterTemp = 0.0;              // coil inlet water temperature used for warning messages
    Real64 DataDesInletAirHumRat = 0.0;              // coil inlet air humidity ratio used for warning messages
    Real64 DataDesInletAirTemp = 0.0;                // coil inlet air temperature used for warning messages
    Real64 DataDesOutletAirTemp = 0.0;               // coil outlet air temperature used for sizing
    Real64 DataDesOutletAirHumRat = 0.0;             // coil air outlet humidity ratio used in sizing calculations [kg water / kg dry air]
    Real64 DataCoolCoilCap = 0.0;                    // cooling coil capacity used for sizing with scalable inputs [W]
    Real64 DataFlowUsedForSizing = 0.0;              // air flow rate used for sizing with scalable inputs [m3/s]
    Real64 DataAirFlowUsedForSizing = 0.0;           // air flow rate used for sizing with scalable inputs [m3/s]
    Real64 DataWaterFlowUsedForSizing = 0.0;         // water flow rate used for sizing with scalable inputs [m3/s]
    Real64 DataCapacityUsedForSizing = 0.0;          // capacity used for sizing with scalable inputs [W]
    Real64 DataDesignCoilCapacity = 0.0;             // calculated capacity of coil at end of UA calculation
    Real64 DataHeatSizeRatio = 1.0;                  // heating coil size as a ratio of cooling coil capacity
    Real64 DataEMSOverride = 0.0;                    // value of EMS variable used to override autosizing
    Real64 DataBypassFrac = 0.0;                     // value of bypass fraction for Coil:Cooling:DX:TwoStageWithHumidityControlMode coils
    Real64 DataFracOfAutosizedCoolingAirflow = 1.0;  // fraction of design cooling supply air flow rate
    Real64 DataFracOfAutosizedHeatingAirflow = 1.0;  // fraction of design heating supply air flow rate
    Real64 DataFlowPerCoolingCapacity = 0.0;         // cooling supply air flow per unit cooling capacity
    Real64 DataFlowPerHeatingCapacity = 0.0;         // heating supply air flow per unit heating capacity
    Real64 DataFracOfAutosizedCoolingCapacity = 1.0; // fraction of autosized cooling capacity
    Real64 DataFracOfAutosizedHeatingCapacity = 1.0; // fraction of autosized heating capacit
    Real64 DataAutosizedCoolingCapacity = 0.0;       // Autosized cooling capacity used for multiplying flow per capacity to get flow rate
    Real64 DataAutosizedHeatingCapacity = 0.0;       // Autosized heating capacit used for multiplying flow per capacity to get flow rate
    Real64 DataConstantUsedForSizing = 0.0;          // base value used for sizing inputs that are ratios of other inputs
    Real64 DataFractionUsedForSizing = 0.0;          // fractional value of base value used for sizing inputs that are ratios of other inputs
    Real64 DataNonZoneNonAirloopValue = 0.0;         // used when equipment is not located in a zone or airloop
    Real64 DataSizingFraction = 1.0;                 // used when ratios of sizing is required
    int DataZoneUsedForSizing = 0;                   // pointer to control zone for air loop equipment
    int DataZoneNumber = 0;                          // a pointer to a served by zoneHVAC equipment
    int NumZoneHVACSizing = 0;                       // Number of design specification zone HVAC sizing objects
    int NumAirTerminalSizingSpec = 0;                // Number of design specfication air terminal sizing objects
    int NumAirTerminalUnits = 0;                     // Number of air terminal units (same as total number of zone inlet nodes)
    Real64 DXCoolCap = 0.0;                          // The ARI cooling capacity of a DX unit.
    Real64 GlobalHeatSizingFactor = 0.0;             // the global heating sizing ratio
    Real64 GlobalCoolSizingFactor = 0.0;             // the global cooling sizing ratio
    Real64 SuppHeatCap = 0.0;                        // the heating capacity of the supplemental heater in a unitary system
    Real64 UnitaryHeatCap = 0.0;                     // the heating capacity of a unitary system
    Array1D<Real64> ZoneSizThermSetPtHi;             // highest zone thermostat setpoint during zone sizing calcs
    Array1D<Real64> ZoneSizThermSetPtLo;             // lowest zone thermostat setpoint during zone sizing calcs
    Array1D_string CoolPeakDateHrMin;                // date:hr:min of cooling peak
    Array1D_string HeatPeakDateHrMin;                // date:hr:min of heating peak
    char SizingFileColSep;                           // Character to separate columns in sizing outputs
    int DataDesicDehumNum = 0;                       // index to desiccant dehumidifier
    bool DataDesicRegCoil = false;                   // TRUE if heating coil desiccant regeneration coil
    bool HRFlowSizingFlag = false;                   // True, if it is a heat recovery heat exchanger flow sizing
    Real64 DataWaterCoilSizCoolDeltaT = 0.0;         // used for sizing cooling coil water design flow rate
    Real64 DataWaterCoilSizHeatDeltaT = 0.0;         // used for sizing heating coil water design flow rate
    bool DataNomCapInpMeth = false;                  // True if heating coil is sized by CoilPerfInpMeth == NomCa
    int DataFanEnumType = -1;                        // Fan type used during sizing
    int DataFanIndex = -1;                           // Fan index used during sizing
    DataSizing::zoneFanPlacement DataFanPlacement = DataSizing::zoneFanPlacement::zoneFanPlaceNotSet; // identifies location of fan wrt coil
    int DataDXSpeedNum = 0;
    EPVector<DataSizing::OARequirementsData> OARequirements;
    EPVector<DataSizing::ZoneAirDistributionData> ZoneAirDistribution;
    EPVector<DataSizing::ZoneSizingInputData> ZoneSizingInput;    // Input data for zone sizing
    Array2D<DataSizing::ZoneSizingData> ZoneSizing;               // Data for zone sizing (all data, all design)
    EPVector<DataSizing::ZoneSizingData> FinalZoneSizing;         // Final data for zone sizing including effects
    Array2D<DataSizing::ZoneSizingData> CalcZoneSizing;           // Data for zone sizing (all data)
    EPVector<DataSizing::ZoneSizingData> CalcFinalZoneSizing;     // Final data for zone sizing (calculated only)
    EPVector<DataSizing::ZoneSizingData> TermUnitFinalZoneSizing; // Final data for sizing terminal units (indexed per terminal unit)
    EPVector<DataSizing::SystemSizingInputData> SysSizInput;      // Input data array for system sizing object
    Array2D<DataSizing::SystemSizingData> SysSizing;              // Data array for system sizing (all data)
    EPVector<DataSizing::SystemSizingData> FinalSysSizing;        // Data array for system sizing (max heat/cool)
    EPVector<DataSizing::SystemSizingData> CalcSysSizing;         // Data array for system sizing (max heat/cool)
    EPVector<DataSizing::SysSizPeakDDNumData> SysSizPeakDDNum;    // data array for peak des day indices
    EPVector<DataSizing::TermUnitSizingData> TermUnitSizing;      // Data added in sizing routines (indexed per terminal unit)
    EPVector<DataSizing::ZoneEqSizingData> ZoneEqSizing;          // Data added in zone eq component sizing routines
    EPVector<DataSizing::ZoneEqSizingData> UnitarySysEqSizing;    // Data added in unitary system sizing routines
    EPVector<DataSizing::ZoneEqSizingData> OASysEqSizing;         // Data added in unitary system sizing routines
    EPVector<DataSizing::PlantSizingData> PlantSizData;           // Input data array for plant sizing
    EPVector<DataSizing::DesDayWeathData> DesDayWeath;            // design day weather saved at major time step
    EPVector<DataSizing::CompDesWaterFlowData> CompDesWaterFlow;  // array to store components' design water flow
    EPVector<DataSizing::ZoneHVACSizingData> ZoneHVACSizing;      // Input data for zone HVAC sizing
    EPVector<DataSizing::AirTerminalSizingSpecData>
        AirTerminalSizingSpec;                                   // Input data for zone HVAC sizing used only for Facility Load Component Summary
    EPVector<DataSizing::FacilitySizingData> CalcFacilitySizing; // Data for zone sizing
    DataSizing::FacilitySizingData CalcFinalFacilitySizing;      // Final data for zone sizing
    Array1D<Real64> VbzByZone;                                   // saved value of ZoneOAUnc which is Vbz used in 62.1 tabular report
    Array1D<Real64> VdzClgByZone;    // saved value of cooling based ZoneSA which is Vdz used in 62.1 tabular report (also used for zone level Vps)
    Array1D<Real64> VdzMinClgByZone; // minimum discharge flow for cooling, Vdz includes secondary and primary flows for dual path
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
    Array1D<Real64> TotCoolCapTemp;  // scratch variable used for calculating peak load [W]
    Array1D<Real64> EvzMinBySysHeat; // saved value of EvzMin used in 62.1 tabular report
    Array1D<Real64> EvzMinBySysCool; // saved value of EvzMin used in 62.1 tabular report
    Array1D<Real64> FaByZoneCool;    // triggers allocation in UpdateSysSizing
    Array1D<Real64> SensCoolCapTemp; // triggers allocation in UpdateSysSizing

    void clear_state() override
    {
        this->NumOARequirements = 0;
        this->NumZoneAirDistribution = 0;
        this->NumZoneSizingInput = 0;
        this->NumSysSizInput = 0;
        this->NumPltSizInput = 0;
        this->CurSysNum = 0;
        this->CurOASysNum = 0;
        this->CurZoneEqNum = 0;
        this->CurTermUnitSizingNum = 0;
        this->CurBranchNum = 0;
        this->CurDuctType = 0;
        this->CurLoopNum = 0;
        this->CurCondLoopNum = 0;
        this->CurEnvirNumSimDay = 0;
        this->CurOverallSimDay = 0;
        this->NumTimeStepsInAvg = 0;
        this->SaveNumPlantComps = 0;
        this->DataTotCapCurveIndex = 0;
        this->DataTotCapCurveValue = 0;
        this->DataPltSizCoolNum = 0;
        this->DataPltSizHeatNum = 0;
        this->DataWaterLoopNum = 0;
        this->DataCoilNum = 0;
        this->DataFanOpMode = 0;
        this->DataCoilIsSuppHeater = false;
        this->DataIsDXCoil = false;
        this->DataAutosizable = true;
        this->DataEMSOverrideON = false;
        this->DataScalableSizingON = false;
        this->DataScalableCapSizingON = false;
        this->DataSysScalableFlowSizingON = false;
        this->DataSysScalableCapSizingON = false;
        this->SysSizingRunDone = false;
        this->TermUnitSingDuct = false;
        this->TermUnitPIU = false;
        this->TermUnitIU = false;
        this->ZoneEqFanCoil = false;
        this->ZoneEqOutdoorAirUnit = false;
        this->ZoneEqUnitHeater = false;
        this->ZoneEqUnitVent = false;
        this->ZoneEqVentedSlab = false;
        this->ZoneEqDXCoil = false;
        this->ZoneEqUnitarySys = false;
        this->ZoneCoolingOnlyFan = false;
        this->ZoneHeatingOnlyFan = false;
        this->ZoneSizingRunDone = false;
        this->DataErrorsFound = false;
        this->AutoVsHardSizingThreshold = 0.1;
        this->AutoVsHardSizingDeltaTempThreshold = 1.5;
        this->DataCoilSizingAirInTemp = 0.0;
        this->DataCoilSizingAirInHumRat = 0.0;
        this->DataCoilSizingAirOutTemp = 0.0;
        this->DataCoilSizingAirOutHumRat = 0.0;
        this->DataCoilSizingFanCoolLoad = 0.0;
        this->DataCoilSizingCapFT = 1.0;
        this->DataDesAccountForFanHeat = true;
        this->DataDesInletWaterTemp = 0.0;
        this->DataDesInletAirHumRat = 0.0;
        this->DataDesInletAirTemp = 0.0;
        this->DataDesOutletAirTemp = 0.0;
        this->DataDesOutletAirHumRat = 0.0;
        this->DataCoolCoilCap = 0.0;
        this->DataFlowUsedForSizing = 0.0;
        this->DataAirFlowUsedForSizing = 0.0;
        this->DataWaterFlowUsedForSizing = 0.0;
        this->DataCapacityUsedForSizing = 0.0;
        this->DataDesignCoilCapacity = 0.0;
        this->DataHeatSizeRatio = 1.0;
        this->DataEMSOverride = 0.0;
        this->DataBypassFrac = 0.0;
        this->DataFracOfAutosizedCoolingAirflow = 1.0;
        this->DataFracOfAutosizedHeatingAirflow = 1.0;
        this->DataFlowPerCoolingCapacity = 0.0;
        this->DataFlowPerHeatingCapacity = 0.0;
        this->DataFracOfAutosizedCoolingCapacity = 1.0;
        this->DataFracOfAutosizedHeatingCapacity = 1.0;
        this->DataAutosizedCoolingCapacity = 0.0;
        this->DataAutosizedHeatingCapacity = 0.0;
        this->DataConstantUsedForSizing = 0.0;
        this->DataFractionUsedForSizing = 0.0;
        this->DataNonZoneNonAirloopValue = 0.0;
        this->DataSizingFraction = 1.0;
        this->DataZoneUsedForSizing = 0;
        this->DataZoneNumber = 0;
        this->NumZoneHVACSizing = 0;
        this->NumAirTerminalSizingSpec = 0;
        this->NumAirTerminalUnits = 0;
        this->DXCoolCap = 0.0;
        this->GlobalHeatSizingFactor = 0.0;
        this->GlobalCoolSizingFactor = 0.0;
        this->SuppHeatCap = 0.0;
        this->UnitaryHeatCap = 0.0;
        this->ZoneSizThermSetPtHi.deallocate();
        this->ZoneSizThermSetPtLo.deallocate();
        this->CoolPeakDateHrMin.deallocate();
        this->HeatPeakDateHrMin.deallocate();
        this->SizingFileColSep = char();
        this->DataDesicDehumNum = 0;
        this->DataDesicRegCoil = false;
        this->HRFlowSizingFlag = false;
        this->DataWaterCoilSizCoolDeltaT = 0.0;
        this->DataWaterCoilSizHeatDeltaT = 0.0;
        this->DataNomCapInpMeth = false;
        this->DataFanEnumType = -1;
        this->DataFanIndex = -1;
        this->DataFanPlacement = DataSizing::zoneFanPlacement::zoneFanPlaceNotSet;
        this->DataDXSpeedNum = 0;
        this->OARequirements.deallocate();
        this->ZoneAirDistribution.deallocate();
        this->ZoneSizingInput.deallocate();
        this->ZoneSizing.deallocate();
        this->FinalZoneSizing.deallocate();
        this->CalcZoneSizing.deallocate();
        this->CalcFinalZoneSizing.deallocate();
        this->TermUnitFinalZoneSizing.deallocate();
        this->SysSizInput.deallocate();
        this->SysSizing.deallocate();
        this->FinalSysSizing.deallocate();
        this->CalcSysSizing.deallocate();
        this->SysSizPeakDDNum.deallocate();
        this->TermUnitSizing.deallocate();
        this->ZoneEqSizing.deallocate();
        this->UnitarySysEqSizing.deallocate();
        this->OASysEqSizing.deallocate();
        this->PlantSizData.deallocate();
        this->DesDayWeath.deallocate();
        this->CompDesWaterFlow.deallocate();
        this->ZoneHVACSizing.deallocate();
        this->AirTerminalSizingSpec.deallocate();
        this->CalcFacilitySizing.deallocate();
        this->CalcFinalFacilitySizing = DataSizing::FacilitySizingData();
        this->VbzByZone.deallocate();
        this->VdzClgByZone.deallocate();
        this->VdzMinClgByZone.deallocate();
        this->VdzHtgByZone.deallocate();
        this->VdzMinHtgByZone.deallocate();
        this->ZdzClgByZone.deallocate();
        this->ZdzHtgByZone.deallocate();
        this->VpzClgByZone.deallocate();
        this->VpzMinClgByZone.deallocate();
        this->VpzHtgByZone.deallocate();
        this->VpzMinHtgByZone.deallocate();
        this->VpzClgSumBySys.deallocate();
        this->VpzHtgSumBySys.deallocate();
        this->PzSumBySys.deallocate();
        this->PsBySys.deallocate();
        this->DBySys.deallocate();
        this->SumRpxPzBySys.deallocate();
        this->SumRaxAzBySys.deallocate();
        this->PeakPsOccurrenceDateTimeStringBySys.deallocate();
        this->PeakPsOccurrenceEnvironmentStringBySys.deallocate();
        this->VouBySys.deallocate();
        this->VpsClgBySys.deallocate();
        this->VpsHtgBySys.deallocate();
        this->FaByZoneHeat.deallocate();
        this->FbByZoneCool.deallocate();
        this->FbByZoneHeat.deallocate();
        this->FcByZoneCool.deallocate();
        this->FcByZoneHeat.deallocate();
        this->XsBySysCool.deallocate();
        this->XsBySysHeat.deallocate();
        this->EvzByZoneCool.deallocate();
        this->EvzByZoneHeat.deallocate();
        this->EvzByZoneCoolPrev.deallocate();
        this->EvzByZoneHeatPrev.deallocate();
        this->VotClgBySys.deallocate();
        this->VotHtgBySys.deallocate();
        this->VozSumClgBySys.deallocate();
        this->VozSumHtgBySys.deallocate();
        this->TotCoolCapTemp.deallocate();
        this->EvzMinBySysHeat.deallocate();
        this->EvzMinBySysCool.deallocate();
        this->FaByZoneCool.deallocate();
        this->SensCoolCapTemp.deallocate();
    }
};

} // namespace EnergyPlus

#endif
